/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Given a procedure, we can create a temp table that has the exact shape as the proc
// We can then insert and select from the temp table to fake a result set
// This file performs codegen for those procedures

#include "cg_test_helpers.h"
#include <stdint.h>

#include "ast.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "symtab.h"
#include "crc64xz.h"
#include "encoders.h"

#define DUMMY_TABLE           1 // dummy_table attribute flag
#define DUMMY_INSERT          2 // dummy_insert attribute flag
#define DUMMY_SELECT          4 // dummy_select attribute flag
#define DUMMY_RESULT_SET      8 // dummy_result_set attribute flag
#define DUMMY_TEST         0x10 // dummy_test attribute flag

#define DUMMY_TEST_INSERT_ROWS  2 // minimum number of rows inserted in table for dummy_test attribution

static charbuf *cg_th_output;
static charbuf *cg_th_decls;
static charbuf* cg_th_procs;

// dummy_test utility variable used to emit statements.
static charbuf *gen_create_triggers;
static charbuf *gen_drop_triggers;

// All triggers per tables. This is used as part of dummy_test to help look up
// all the triggers to emit
static symtab *all_tables_with_triggers;

// All indexes per tables. This is used as part of dummy_test to help look up
// all the indexes to emit
static symtab *all_tables_with_indexes;

// Record the autotest attribute processed. This is used to figure out if there
// will be code gen to write to the output file
static int32_t helper_flags = 0;

// Indicate whether we should write to the output file
static int32_t write_to_file = 0;

// hold all the table name, column name and column values provided by dummy_test node
static symtab *dummy_test_infos = NULL;

typedef struct dummy_test_info {
  list_item *sorted_tables_ast;
  CSTR table_current;
  symtab *table_added;
  symtab *table_triggers_visited;
} dummy_test_info;

static void find_all_table_node(dummy_test_info *info, ast_node *node);

static void cg_dummy_test_populate(charbuf *gen_insert_tables, ast_node *table_ast, int32_t *dummy_value_seed);

// The dummy_table, dummy_insert, dummy_select and dummy_result_set attributions
// will reference the original procedure by name in a LIKE clause.  In order to get its
// result type, we need to emit a declaration for the proc because its body will not be
// in the test helper file.  This function tells us if we need to emit that declaration.
static bool is_declare_proc_needed() {
  int32_t needed = DUMMY_TABLE | DUMMY_INSERT | DUMMY_SELECT | DUMMY_RESULT_SET;
  return !!(helper_flags & needed);
}

// Emit a declaration for the proc so that the signature is known by
// the generated dummy procs.  See above.
static void cg_test_helpers_declare_proc(ast_node *ast) {
  bprintf(cg_th_decls, "\n");
  gen_set_output_buffer(cg_th_decls);
  gen_declare_proc_from_create_proc(ast);
  bprintf(cg_th_decls, ";\n");
}

static bool_t cg_test_helpers_force_if_not_exists(
  ast_node *_Nonnull ast,
  void *_Nullable context,
  charbuf *_Nonnull output)
{
  bprintf(output, "IF NOT EXISTS ");
  return true;
}

// Emit an open proc which creates a temp table in the form of the original proc
// Emit a close proc which drops the temp table
static void cg_test_helpers_dummy_table(CSTR name) {
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC open_%s()\n", name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  CREATE TEMP TABLE test_%s(LIKE %s);\n", name, name);
  bprintf(cg_th_procs, "END;\n");

  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC close_%s()\n", name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  DROP TABLE test_%s;\n", name);
  bprintf(cg_th_procs, "END;\n");
}

// Emit a dummy insert to the temp table using FROM ARGUMENTS
static void cg_test_helpers_dummy_insert(CSTR name) {
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC insert_%s(LIKE %s)\n", name, name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  INSERT INTO test_%s FROM ARGUMENTS;\n", name);
  bprintf(cg_th_procs, "END;\n");
}

// Emit a dummy select from the temp table which will have a result set
// that matches that of the original proc
static void cg_test_helpers_dummy_select(CSTR name) {
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC select_%s()\n", name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  SELECT * FROM test_%s;\n", name);
  bprintf(cg_th_procs, "END;\n");
}

// Emit a procedure that takes in arguments by the shape of the procedure
// and produces a result set
static void cg_test_helpers_dummy_result_set(CSTR name) {
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC generate_%s_row(LIKE %s)\n", name, name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  DECLARE curs CURSOR LIKE %s;\n", name);
  bprintf(cg_th_procs, "  FETCH curs FROM ARGUMENTS;\n");
  bprintf(cg_th_procs, "  OUT curs;\n");
  bprintf(cg_th_procs, "END;\n");
}

// Find all tables in the child nodes.
static void find_all_table_node_left_right(dummy_test_info *info, ast_node *node) {
  // Check the left and right nodes.
  if (ast_has_left(node)) {
    find_all_table_node(info, node->left);
  }
  if (ast_has_right(node)) {
    find_all_table_node(info, node->right);
  }
}

// Find all triggers on the table "table_or_view_name" then find all the tables those
// triggers actions depend on. The tables in those triggers should be parf of dummy_test
// codegen otherwise trigger creation stmt will fail in dummy_test.
static void find_all_triggers_node(dummy_test_info *info, CSTR table_or_view_name) {
  symtab_entry *triggers_entry = symtab_find(all_tables_with_triggers, table_or_view_name);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.if_not_exists_callback = cg_test_helpers_force_if_not_exists;

  // We want to make sure we only run this function once for tables with triggers to
  // avoid infinite loop
  if (triggers_entry &&
      symtab_add(info->table_triggers_visited, table_or_view_name, NULL)) {
    // We collect this table as having triggers. Later we'll use this datastructure to emit
    // those triggers.
    bytebuf *buf = (bytebuf *)triggers_entry->val;
    ast_node **items = (ast_node **)buf->ptr;
    int32_t count = buf->used / sizeof(*items);
    for (int32_t i = 0; i < count; i++) {
      EXTRACT_ANY_NOTNULL(create_trigger_stmt, items[i]);

      // emit create trigger stmt
      gen_set_output_buffer(gen_create_triggers);
      gen_statement_with_callbacks(create_trigger_stmt, &callbacks);
      bprintf(gen_create_triggers, ";\n");

      // emit drop trigger stmt
      gen_set_output_buffer(gen_drop_triggers);
      EXTRACT_NOTNULL(trigger_body_vers, create_trigger_stmt->right);
      EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
      EXTRACT_ANY_NOTNULL(trigger_name_ast, trigger_def->left);
      EXTRACT_STRING(trigger_name, trigger_name_ast);
      bprintf(gen_drop_triggers, "DROP TRIGGER IF EXISTS %s;\n", trigger_name);

      // Now we need to find all the tables referenced in the triggers, because those tables
      // should also to be part of tables emit by dummy_test. Otherwise the triggers statement
      // will be referencing non existent table in dummy_test.
      find_all_table_node_left_right(info, create_trigger_stmt);
    }
  }
}

// Recursively finds all view, triggers and table nodes but also all table to tables relationship
//    and all table to triggers relationship. During the walk through we mark a table visited if
//    all its dependences (foreign key table, tables references in the a view) are marked visited.
//
// sorted_tables_ast: When a table is marked visited then we add that table on top of the list
//    sorted_tables_ast. By the end of the recursion sorted_tables_ast the list will have child
//    table on top and the parent they depends as next in the list.
//    This is used later to emit in the correct order the drop tables statements. e.g: drop first
//    the parent table then the child can follow. If you do this in reverse Sqlite will complain
// table_asts: The ast node of all tables referenced directly or indirectly in a subtree. The
//    initial subtree is a stored proc. This datastructure will be used later to find all column's
//    name and type for tables.
// table_triggers_visited: This is a trigger cache to avoid searching the triggers of the same table twice
// node: The ast node to walkthrough. The node is a create_proc_stmt when this function is first
//    called.
//
// This function is equivalent to void find_table_refs(ast_node *_Nonnull node,
//                                                     find_ast_node_callback _Nonnull callback,
//                                                     void *_Nullable callback_context)
// in cg_common.h except that it does more:
//  - look up views
//  - look up all tables relationship instead of just tables reference in a proc
//  - look up drop table statement
//  - look up triggers and tables referenced in those triggers
// ...
// find_table_refs(...) is also a sensitive code that control which tables changes trigger UI notification.
// Because of all of these I decided to implement a separate function to fit the need related to dummy_test.
static void find_all_table_node(dummy_test_info *info, ast_node *node) {
  ast_node *table_or_view_name_ast = NULL;

  if (is_ast_table_or_subquery(node)) {
    EXTRACT_ANY_NOTNULL(any_table_or_query, node->left);
    if (is_ast_str(any_table_or_query)) {
      table_or_view_name_ast = any_table_or_query;
    }
  }
  else if (is_ast_delete_stmt(node) || is_ast_update_stmt(node) || is_ast_fk_target(node)) {
    EXTRACT_ANY_NOTNULL(name_ast, node->left);
    table_or_view_name_ast = name_ast;
  }
  else if (is_ast_drop_view_stmt(node) || is_ast_drop_table_stmt(node)) {
    EXTRACT_ANY_NOTNULL(name_ast, node->right);
    table_or_view_name_ast = name_ast;
  }
  else if (is_ast_insert_stmt(node)) {
    EXTRACT(name_columns_values, node->right);
    EXTRACT_ANY_NOTNULL(name_ast, name_columns_values->left);
    table_or_view_name_ast = name_ast;
  }
  else if (is_ast_trigger_target_action(node)) {
    EXTRACT_ANY_NOTNULL(name_ast, node->left);
    table_or_view_name_ast = name_ast;
  }

  if (table_or_view_name_ast) {
    EXTRACT_STRING(table_or_view_name, table_or_view_name_ast);
    ast_node *table_or_view = find_table_or_view_even_hidden(table_or_view_name);

    if (table_or_view) {
      // This part prevents us from any cycles, the only possible cycle is T references T in an FK
      bool skip = is_ast_fk_target(node) && info->table_current && !Strcasecmp(table_or_view_name, info->table_current);

      if (!skip) {
        CSTR table_saved = info->table_current;
        info->table_current = table_or_view_name;

        // Now let walkthrough the new found table (table_or_view_name) to find all the tables it
        // depends on.
        find_all_table_node_left_right(info, table_or_view);

        // Find all triggers on the table "table_or_view_name" then find all of the tables and triggers
        // referenced by them.
        find_all_triggers_node(info, table_or_view_name);

        info->table_current = table_saved;
      }

      // We do a seperate check for this because we want to add it "deepest" level, so that it is
      // as early as possible in the list.  For sure if B depends on A then A is earlier in the list
      bool_t needs_add = symtab_add(info->table_added, table_or_view_name, NULL);
      if (needs_add) {
        // Add table_or_view_name at the top of the list. This means all the parent tables to
        // table_or_view_name are already the list. Later we'll walk this sorted list to emit
        // [DROP TABLE ...] stmt for each individual tables in the list.
        add_item_to_list(&info->sorted_tables_ast, table_or_view);
      }
    }
  }

  // Check the left and right nodes.
  find_all_table_node_left_right(info, node);
}

// Format the value in node accordingly to the node type. The semantic analysis
// has already made sure the ast node type matches the column type in the table
static void cg_dummy_test_column_value(charbuf *output, ast_node *value) {
  if (is_ast_uminus(value)) {
    Contract(is_ast_num(value->left));
    bprintf(output, "%s", "-");
    value = value->left;
  }

  if (is_ast_str(value)) {
    bprintf(output, "%s", ((str_ast_node *)value)->value);
  }
  else if (is_ast_num(value)) {
    EXTRACT_NUM_VALUE(lit, value);
    bprintf(output, "%s", lit);
  }
  else if (is_ast_null(value)) {
    bprintf(output, "NULL");
  }
}

// Find the parent column referenced in the foreign key statement by child table
// "table_name" and column "column_name". We use this function to find parent
// column to do some validation to avoid foreign key violations in insert statement
// we emit.
static void find_parent_column(
  ast_node *_Nullable *_Nonnull fk_table_out,
  CSTR _Nullable *_Nonnull fk_column_name_out,
  CSTR table_name,
  CSTR column_name)
{
  ast_node *table_ast = find_table_or_view_even_hidden(table_name);
  Contract(is_ast_create_table_stmt(table_ast));
  EXTRACT_NOTNULL(col_key_list, table_ast->right);
  *fk_table_out = NULL;
  *fk_column_name_out = NULL;

  for (ast_node *col_keys = col_key_list; col_keys; col_keys = col_keys->right) {
    if (is_ast_col_def(col_keys->left)) {
      // the column might be marked as an FK by the form col REFERENCES ref_table(ref_col)
      // to verify this we need to know that:
      // 1. col matches the required name
      // 2. there is an fk column attribute
      // if so we can get the referenced name and column from that attribute

      EXTRACT_NOTNULL(col_def, col_keys->left);
      EXTRACT_NOTNULL(col_def_type_attrs, col_def->left);
      EXTRACT_ANY(attrs, col_def_type_attrs->right);
      EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
      EXTRACT_STRING(name, col_def_name_type->left);

      if (!Strcasecmp(name, column_name)) {
        for (ast_node *attr = attrs; attr; attr = attr->right) {
          if (is_ast_col_attrs_fk(attr)) {
            EXTRACT_NOTNULL(fk_target_options, attr->left);
            EXTRACT_NOTNULL(fk_target, fk_target_options->left);
            EXTRACT_STRING(ref_table_name, fk_target->left);
            EXTRACT_NAMED_NOTNULL(ref_list, name_list, fk_target->right);
            EXTRACT_STRING(ref_col_name, ref_list->left);
            Contract(!ref_list->right); // it must be a list of one because its attribute form

            *fk_table_out = find_table_or_view_even_hidden(ref_table_name);
            *fk_column_name_out = ref_col_name;
            return;
          }
        }
      }
    }
    else if (is_ast_fk_def(col_keys->left)) {
      // In the general case we're looking for an FK constraint that has this column
      // if we find such a constraint (ast_fk_def) then we look at the name list
      // for the required column, if present we extract the referenced table
      // and the corresponding referenced column.

      EXTRACT_NOTNULL(fk_def, col_keys->left);
      EXTRACT_NOTNULL(fk_info, fk_def->right);
      EXTRACT_NOTNULL(name_list, fk_info->left);
      int32_t column_index = 0;
      bool_t found = 0;
      for (ast_node *list = name_list; list; list = list->right) {
        EXTRACT_STRING(name, list->left);
        if (!Strcasecmp(name, column_name)) {
          found = 1;
          break;
        }
        column_index++;
      }

      if (found) {
        // All we need to do now is find the referenced name list
        // and skip to the column_index entry to get the corresponding
        // referenced name.  The table name is sitting there for us
        // on a silver platter.
        EXTRACT_NOTNULL(fk_target_options, fk_info->right);
        EXTRACT_NOTNULL(fk_target, fk_target_options->left);
        EXTRACT_STRING(fk_table, fk_target->left);
        EXTRACT_ANY_NOTNULL(fk_name_list, fk_target->right);
        int32_t index = 0;
        while (index < column_index) {
          fk_name_list = fk_name_list->right;
          index++;
        }
        Invariant(fk_name_list);
        EXTRACT_STRING(fk_col_name, fk_name_list->left);
        *fk_table_out = find_table_or_view_even_hidden(fk_table);
        *fk_column_name_out = fk_col_name;
        return;
      }
    }
  }
}

// make sure a value is within 1 and DUMMY_TEST_INSERT_ROWS
static int32_t cg_validate_value_range(int32_t value) {
  if (1 <= value && value <= DUMMY_TEST_INSERT_ROWS) {
    return value;
  } else {
    return (value % DUMMY_TEST_INSERT_ROWS) + 1;
  }
}

// Emit a value of a parent column referenced by child column "column_name".
// It allows the insert statement of a child table to include the column value
// from the parent table.
// This is useful to make sure a value provided by the user in dummy_test info
// is actually include.
// the parent column might have multiple values available. We use "index" to specified
// the index of the one we want to emit.
// e.g: Foo table has a foreign key column 'A' referincing column 'B' on the table Baa.
// If a value for column 'B' of table Baa was specified in dummy_test info then that
// value will be populated to column 'B' of table Foo
static void cg_parent_column_value(charbuf *output, CSTR table_name, CSTR column_name, int32_t index) {
  ast_node *fk_table;
  CSTR fk_column_name;
  find_parent_column(&fk_table, &fk_column_name, table_name, column_name);
  if (fk_table) {
    CSTR fk_table_name = fk_table->sem->sptr->struct_name;
    symtab_entry *fk_table_entry = symtab_find(dummy_test_infos, fk_table_name);
    if (fk_table_entry) {
      symtab *fk_column_names = (symtab *)fk_table_entry->val;
      symtab_entry *fk_column_values_entry = symtab_find(fk_column_names, fk_column_name);
      if (fk_column_values_entry) {
        bytebuf *fk_column_values = (bytebuf *)fk_column_values_entry->val;
        ast_node **list = (ast_node **)fk_column_values->ptr;
        int32_t size = fk_column_values->used / sizeof(void *);
        cg_dummy_test_column_value(output, list[index % size]);
        return;
      }
    }
  }
}

// Emit INSERT statement for a table by using @dummy_seed to generated dummy data
// but also info in dummy_test attribute. If column's values are provided in
// dummy_test info for the table, it'll be used otherwise @dummy_seed is used to
// populated seed value into table.
static void cg_dummy_test_populate (charbuf *gen_insert_tables, ast_node *table_ast, int32_t *dummy_value_seed) {
  Contract(is_ast_create_table_stmt(table_ast));

  sem_struct *sptr = table_ast->sem->sptr;
  CSTR table_name = sptr->struct_name;
  bool_t add_row;
  int32_t row_index = -1;
  symtab_entry *table_entry = symtab_find(dummy_test_infos, table_name);
  do {
    row_index++;
    add_row = 0;
    CHARBUF_OPEN(names);
    CHARBUF_OPEN(values);
    CSTR comma = "";
    symtab *column_name_cache = symtab_new();

    // extract column values for insert statement from dummy_test info and emit
    // the insert statement
    if (table_entry) {
      symtab *table = (symtab *)table_entry->val;
      for (int32_t j = 0; j < table->capacity; j++) {
        symtab_entry column_entry = table->payload[j];

        if (column_entry.sym) {
          CSTR column_name = column_entry.sym;
          bytebuf *column_values_entry = (bytebuf *)column_entry.val;
          ast_node **column_values = (ast_node **)column_values_entry->ptr;
          int32_t size = column_values_entry->used/sizeof(void **);
          if (row_index < size) {
            CHARBUF_OPEN(str_val);
            cg_dummy_test_column_value(&str_val, column_values[row_index]);

            bprintf(&values, "%s%s", comma, str_val.ptr);
            bprintf(&names, "%s%s", comma, column_name);
            comma = ", ";

            symtab_add(column_name_cache, column_name, NULL);
            add_row = 1;
            CHARBUF_CLOSE(str_val);
          }
        }
      }
    }

    // we make sure that we add at least DUMMY_TEST_INSERT_ROWS rows per table
    if (row_index < DUMMY_TEST_INSERT_ROWS) {
      add_row = 1;
    }

    if (add_row) {
      // provide specific values for primary and foreign column to avoid foreign key violation.
      for (int32_t i = 0; i < sptr->count; i++) {
        sem_t col_type = sptr->semtypes[i];
        CSTR column_name = sptr->names[i];

        // We find primary and foreign key column that are missing values in the
        // insert statement and add those values to avoid sql foreign key violation eror.
        if (!symtab_find(column_name_cache, column_name)) {
          if (is_referenceable_by_foreign_key(table_ast, column_name) || is_foreign_key(col_type)) {
            CHARBUF_OPEN(str_val);
            // we do +1 because index value start at zero and we don't want to insert zero as primary key
            int32_t index_value = row_index + 1;
            if (is_foreign_key(col_type)) {
              cg_parent_column_value(&str_val, table_name, column_name, row_index);
              if (str_val.used <= 1) {
                // The parent table does not have explicit dummy info on this column.
                // In this case the parent table key referenced here was created with default value
                // between 1 and DUMMY_TEST_INSERT_ROWS. We just need to select one of these default
                // value.
                bprintf(&str_val, is_numeric(col_type) ? "%d" : "\'%d\'", cg_validate_value_range(index_value));
              }
            }
            bprintf(&names, "%s%s", comma, column_name);
            bprintf(&values, "%s", comma);
            comma = ", ";

            if (str_val.used > 1) {
              bprintf(&values, "%s", str_val.ptr);
            } else {
              bprintf(&values, is_numeric(col_type) ? "%d" : "\'%d\'", index_value);
            }
            CHARBUF_CLOSE(str_val);
          }
        }
      }

      bprintf(gen_insert_tables,
              "INSERT OR IGNORE INTO %s(%s) VALUES(%s) @dummy_seed(%d)%s;\n",
              table_name,
              names.ptr,
              values.ptr,
              (*dummy_value_seed)++,
              row_index % 2 == 0 ? "" : " @dummy_nullables @dummy_defaults");
    }

    CHARBUF_CLOSE(values);
    CHARBUF_CLOSE(names);
    symtab_delete(column_name_cache);
  } while (add_row);
}

// Walk through all triggers and create a dictionnary of triggers per tables.
static void init_all_trigger_per_table() {
  Contract(all_tables_with_triggers == NULL);
  all_tables_with_triggers = symtab_new();

  for(list_item *item = all_triggers_list; item; item = item->next) {
    EXTRACT_NOTNULL(create_trigger_stmt, item->ast);
    EXTRACT_NOTNULL(trigger_body_vers, create_trigger_stmt->right);
    EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
    EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
    EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
    EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
    EXTRACT_ANY_NOTNULL(table_name_ast, trigger_target_action->left);
    EXTRACT_STRING(table_name, table_name_ast);

    symtab_append_bytes(all_tables_with_triggers, table_name, &create_trigger_stmt, sizeof(create_trigger_stmt));
  }
}

static void init_all_indexes_per_table() {
  Contract(all_tables_with_indexes == NULL);
  all_tables_with_indexes = symtab_new();

  for (list_item *item = all_indices_list; item; item = item->next) {
    EXTRACT_NOTNULL(create_index_stmt, item->ast);
    EXTRACT_NOTNULL(create_index_on_list, create_index_stmt->left);
    EXTRACT_ANY_NOTNULL(table_name_ast, create_index_on_list->right);
    EXTRACT_STRING(table_name, table_name_ast);

    symtab_append_bytes(all_tables_with_indexes, table_name, &create_index_stmt, sizeof(create_index_stmt));
  }
}

// Emit create and drop index statement for all indexes on a table.
static void cg_emit_index_stmt(
  CSTR table_name,
  charbuf *gen_create_indexes,
  charbuf *gen_drop_indexes,
  gen_sql_callbacks *callback)
{
  symtab_entry *indexes_entry = symtab_find(all_tables_with_indexes, table_name);
  bytebuf *buf = indexes_entry ? (bytebuf *)indexes_entry->val : NULL;
  ast_node **indexes_ast = buf ? (ast_node **)buf->ptr : NULL;
  int32_t count = buf ? buf->used / sizeof(*indexes_ast) : 0;
  gen_set_output_buffer(gen_create_indexes);

  for (int32_t i = 0; i < count; i++) {
    ast_node *index_ast = indexes_ast[i];
    gen_statement_with_callbacks(index_ast, callback);
    bprintf(gen_create_indexes, ";\n");

    Contract(is_ast_create_index_stmt(index_ast));
    EXTRACT_NOTNULL(create_index_on_list, index_ast->left);
    EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
    EXTRACT_STRING(index_name, index_name_ast);
    bprintf(gen_drop_indexes, "DROP INDEX IF EXISTS %s;\n", index_name);
  }
}

static CSTR get_table_or_view_name(ast_node *table_or_view) {
  CSTR table_name = NULL;
  if (is_ast_create_table_stmt(table_or_view)) {
    EXTRACT(create_table_name_flags, table_or_view->left);
    EXTRACT(table_flags_attrs, create_table_name_flags->left);
    EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
    EXTRACT_STRING(name, name_ast);
    table_name = name;
  }
  else {
    Contract(is_ast_create_view_stmt(table_or_view));
    EXTRACT(view_and_attrs, table_or_view->right);
    EXTRACT(name_and_select, view_and_attrs->left);
    EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
    EXTRACT_STRING(name, name_ast);
    table_name = name;
  }
  return table_name;
}

// Emit procedure for dummy_test attribution. This is the entry point that emit
// generated code for dummy_test attribution.
// This function will generated stored procedures to :
//  - Create all the tables referenced in the create proc statement.
//  - Populate data into all the tables referenced in the create proc statement.
//  - Drop all the tables referenced in the create proc statement.
//  - Read tables reference in the create proc statement.
// The tables are created, populated and drop in an specific order to avoid foreign key violation or table not existing errors.
// But also the data populated in the foreign key columns of these tables do not violate the foreign key constraint.
static void cg_test_helpers_dummy_test(ast_node *stmt) {
  Contract(is_ast_create_proc_stmt(stmt));
  EXTRACT_STRING(proc_name, stmt->left);

  CHARBUF_OPEN(create_triggers);
  CHARBUF_OPEN(drop_triggers);
  gen_create_triggers = &create_triggers;
  gen_drop_triggers = &drop_triggers;

  dummy_test_info info = {
    .table_current = NULL,
    .table_added = symtab_new(),
    .table_triggers_visited = symtab_new(),
    .sorted_tables_ast = NULL
  };

  find_all_table_node(&info, stmt);

  // The create proc statement does not reference any table, therefore there is nothing to emit
  if (info.sorted_tables_ast == NULL) {
    CHARBUF_CLOSE(drop_triggers);
    CHARBUF_CLOSE(create_triggers);
    symtab_delete(info.table_added);
    symtab_delete(info.table_triggers_visited);
    return;
  }

  int32_t value_seed = 123;

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.if_not_exists_callback = cg_test_helpers_force_if_not_exists;

  CHARBUF_OPEN(gen_create_tables);
  CHARBUF_OPEN(gen_drop_tables);
  CHARBUF_OPEN(gen_populate_tables);
  CHARBUF_OPEN(gen_read_tables);
  CHARBUF_OPEN(gen_declare_funcs);
  CHARBUF_OPEN(gen_drop_indexes);

  helper_flags |= DUMMY_TEST;

  for (list_item *item = info.sorted_tables_ast; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(table_or_view, item->ast);
    Invariant(is_ast_create_table_stmt(table_or_view) || is_ast_create_view_stmt(table_or_view));
    CSTR table_name = get_table_or_view_name(table_or_view);
    bprintf(&gen_drop_tables, "DROP %s IF EXISTS %s;\n", is_ast_create_table_stmt(table_or_view) ? "TABLE" : "VIEW", table_name);
  }

  // reverse the list to get the tables back into the declared order like that we can loop over
  // to emit table creation of parent tables before child tables.
  reverse_list(&info.sorted_tables_ast);

  for (list_item *item = info.sorted_tables_ast; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(table_or_view, item->ast);

    gen_set_output_buffer(&gen_create_tables);
    gen_statement_with_callbacks(table_or_view, &callbacks);
    bprintf(&gen_create_tables, ";\n");

    CSTR table_name = get_table_or_view_name(table_or_view);

    cg_emit_index_stmt(table_name, &gen_create_tables, &gen_drop_indexes, &callbacks);

    if (is_ast_create_table_stmt(table_or_view)) {
      cg_dummy_test_populate(&gen_populate_tables, table_or_view, &value_seed);
    }

    bprintf(&gen_read_tables, "\n");
    bprintf(&gen_read_tables, "CREATE PROC test_%s_read_%s()\n", proc_name, table_name);
    bprintf(&gen_read_tables, "BEGIN\n");
    bprintf(&gen_read_tables, " SELECT * FROM %s;\n", table_name);
    bprintf(&gen_read_tables, "END;\n");
  }

  // Emit declare functions because it may be needed for schema and query validation
  gen_set_output_buffer(&gen_declare_funcs);
  bprintf(&gen_declare_funcs, "\n");
  for (list_item *item = all_functions_list; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(any_func, item->ast);
    Contract(is_ast_declare_func_stmt(any_func) || is_ast_declare_select_func_stmt(any_func));
    if (is_ast_declare_select_func_stmt(any_func)) {
      gen_one_stmt(any_func);
      bprintf(&gen_declare_funcs, ";\n");
    }
  }

  // declare functions
  bprintf(cg_th_procs, "%s", gen_declare_funcs.ptr);

  // create tables proc
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_create_tables()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, &gen_create_tables, 2);
  bprintf(cg_th_procs, "END;\n");

  // Create the triggers proc.
  //
  // We emit the trigger creation code in its own proc for two reasons:
  //
  // 1. If the code is part of the create table proc it might have unwanted
  //    effects on the dummy data populated later. Some dummy data in the table
  //    will likely be altered because of the triggers and the DB will end up in
  //    an unexpected state. Generally the dummy data is considered authoritative
  //    of the desire end state, it isn't transactions to be applied.
  //
  // 2. We want to give the engineer control of if/when the triggers are applied.
  if (gen_create_triggers->used > 1) {
    bprintf(cg_th_procs, "\n");
    bprintf(cg_th_procs, "CREATE PROC test_%s_create_triggers()\n", proc_name);
    bprintf(cg_th_procs, "BEGIN\n");
    bindent(cg_th_procs, gen_create_triggers, 2);
    bprintf(cg_th_procs, "END;\n");
  }

  // populate tables proc
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_populate_tables()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, &gen_populate_tables, 2);
  bprintf(cg_th_procs, "END;\n");

  // drop tables proc
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_drop_tables()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, &gen_drop_tables, 2);
  bprintf(cg_th_procs, "END;\n");

  // drop trigger proc
  if (gen_drop_triggers->used > 1) {
    bprintf(cg_th_procs, "\n");
    bprintf(cg_th_procs, "CREATE PROC test_%s_drop_triggers()\n", proc_name);
    bprintf(cg_th_procs, "BEGIN\n");
    bindent(cg_th_procs, gen_drop_triggers, 2);
    bprintf(cg_th_procs, "END;\n");
  }

  // read tables proc
  bprintf(cg_th_procs, "%s", gen_read_tables.ptr);

  // drop indexes proc
  if (gen_drop_indexes.used > 1) {
    bprintf(cg_th_procs, "\n");
    bprintf(cg_th_procs, "CREATE PROC test_%s_drop_indexes()\n", proc_name);
    bprintf(cg_th_procs, "BEGIN\n");
    bindent(cg_th_procs, &gen_drop_indexes, 2);
    bprintf(cg_th_procs, "END;\n");
  }

  CHARBUF_CLOSE(gen_drop_indexes);
  CHARBUF_CLOSE(gen_declare_funcs);
  CHARBUF_CLOSE(gen_read_tables);
  CHARBUF_CLOSE(gen_populate_tables);
  CHARBUF_CLOSE(gen_drop_tables);
  CHARBUF_CLOSE(gen_create_tables);
  CHARBUF_CLOSE(drop_triggers);
  CHARBUF_CLOSE(create_triggers);
  symtab_delete(info.table_added);
  symtab_delete(info.table_triggers_visited);
}

// check whether "value" already exist in "column_values". This is used to avoid
// having the same value repeated in a column. It can only happens if the value
// explicitely added to dummy_test info match values from @dummy_seed.
static bool_t is_column_value_exist(bytebuf *column_values, sem_t column_type, ast_node *value) {
  bool_t exist = 0;
  ast_node **list = (ast_node **)column_values->ptr;
  int32_t size = column_values->used / sizeof(ast_node *);
  for (int32_t i = 0; i < size; i++) {
    ast_node *l = list[i];
    sem_t col_type = core_type_of(column_type);
    ast_node *r = value;


    if (col_type == SEM_TYPE_LONG_INTEGER ||
        col_type == SEM_TYPE_INTEGER ||
        col_type == SEM_TYPE_REAL ||
        col_type == SEM_TYPE_BOOL) {

      bool_t minus_l = is_ast_uminus(l);
      if (minus_l) {
        Contract(is_ast_num(l->left));
        l = l->left;
      }

      bool_t minus_r = is_ast_uminus(r);
      if (minus_r) {
        Contract(is_ast_num(r->left));
        r = r->left;
      }

      EXTRACT_NUM_VALUE(lv, l);
      EXTRACT_NUM_VALUE(rv, r);
      exist = minus_l == minus_r && !Strcasecmp(lv, rv);
    }
    else if (col_type == SEM_TYPE_TEXT) {
      EXTRACT_STRING(lv, l);
      EXTRACT_STRING(rv, r);
      exist = !Strcasecmp(lv, rv);
    }

    if (exist) {
      return true;
    }
  }
  return false;
}

// Insert the column value from a child table to the parent table to make sure
// the row in child table referenced an existing row in parent table when we emit
// insert statement for both table. This function is only called from foreign key columns
// e.g: Let's say you have table Foo with colum id which is a foreign key on
//      table Baa. If the user has manually added a value for the column id of
//      the table Foo in dummy_test info then this method will add the same value
//      to column id of the table Baa into dummy_test info
static void add_value_to_fk_table(
  CSTR table_name,
  CSTR column_name,
  sem_t column_type,
  ast_node *column_value)
{
  if (is_ast_null(column_value)) {
    return;
  }

  ast_node *fk_table_ast;
  CSTR fk_column_name;
  find_parent_column(&fk_table_ast, &fk_column_name, table_name, column_name);
  CSTR fk_table_name = fk_table_ast->sem->sptr->struct_name;

  symtab *fk_column_list = symtab_ensure_symtab(dummy_test_infos, fk_table_name);
  bytebuf *fk_column_values = symtab_ensure_bytebuf(fk_column_list, fk_column_name);

  // We want to avoid adding the same value to multiple rows in the same table.
  if (!is_column_value_exist(fk_column_values, column_type, column_value)) {
    bytebuf_append_var(fk_column_values, column_value);
  }
}

// Walkthrough the dummy_test tree and collect all the dummy_test info provided
// in the AST. These later are used in insert statement proc for dummy_test attribute
static void collect_dummy_test_info(
  ast_node *_Nullable misc_attr_value_list,
  void *_Nullable context)
{
  EXTRACT_STRING(autotest_attr_name, misc_attr_value_list->left);

  if (is_autotest_dummy_test(autotest_attr_name)) {
    // walkthrough dummy_test tree and retreive the table name then the column name
    // of the table name and then the column values of the column names. We repeat
    // it for the next table info.
    for (ast_node *dummy_test_list = misc_attr_value_list->right; dummy_test_list; dummy_test_list = dummy_test_list->right) {
      bytebuf column_values_cache;
      bytebuf column_types_cache;
      bytebuf column_names_cache;

      bytebuf_open(&column_values_cache);
      bytebuf_open(&column_types_cache);
      bytebuf_open(&column_names_cache);

      // collect table name from dummy_test info
      ast_node *table_list = dummy_test_list->left;
      EXTRACT_STRING(table_name, table_list->left);
      symtab *column_list = symtab_ensure_symtab(dummy_test_infos, table_name);

      // collect column names from dummy_test info
      ast_node *column_name_list = table_list->right;
      for (ast_node *list = column_name_list->left; list; list = list->right) {
        EXTRACT_STRING(column_name, list->left);
        sem_t col_type = find_column_type(table_name, column_name);

        bytebuf *column_values = symtab_ensure_bytebuf(column_list, column_name);
        // cache the info
        bytebuf_append_var(&column_values_cache, column_values);
        bytebuf_append_var(&column_types_cache, col_type);
        bytebuf_append_var(&column_names_cache, column_name);
      }

      // collect column value from dummy_test info. We can have multiple rows of column value
      for (ast_node *column_values_list = column_name_list->right; column_values_list; column_values_list = column_values_list->right) {
        int32_t column_value_index = 0;
        // collect one row of column value
        for (ast_node *list = column_values_list->left; list; list = list->right) {
          ast_node *misc_attr_value = list->left;
          Contract(column_values_cache.used);
          bytebuf *column_values = ((bytebuf **) column_values_cache.ptr)[column_value_index];
          sem_t column_type = ((sem_t *) column_types_cache.ptr)[column_value_index];
          CSTR column_name = ((CSTR *) column_names_cache.ptr)[column_value_index];

          bytebuf_append_var(column_values, misc_attr_value);
          column_value_index++;

          // Let's handle a case where a column value is added to dummy_test info for a
          // foreign key column. We need to make sure that column value is also added to
          // the foreign key table in dummy_test info.
          // e.g: table A has a foreign key on table B. If a value is specified for that
          //      foreign key of table A in dummy_test info then that value needs to be
          //      added to table B in dummy_test info.
          if (is_foreign_key(column_type)) {
            add_value_to_fk_table(table_name, column_name, column_type, misc_attr_value);
          }
        }
      }

      bytebuf_close(&column_values_cache);
      bytebuf_close(&column_types_cache);
      bytebuf_close(&column_names_cache);
    }
  }
}

static void test_helpers_find_ast_misc_attr_callback(
  CSTR _Nullable misc_attr_prefix,
  CSTR _Nonnull misc_attr_name,
  ast_node *_Nullable ast_misc_attr_value_list,
  void *_Nullable context)
{
  ast_node *stmt = (ast_node *)context;
  Contract(is_ast_create_proc_stmt(stmt));

  if (misc_attr_prefix &&
      misc_attr_name &&
      !Strcasecmp(misc_attr_prefix, "cql") &&
      !Strcasecmp(misc_attr_name, "autotest")) {

    CHARBUF_OPEN(decls_temp);
    CHARBUF_OPEN(procs_temp);

    charbuf *decls_saved = cg_th_decls;
    charbuf *procs_saved = cg_th_procs;

    cg_th_decls = &decls_temp;
    cg_th_procs = &procs_temp;

    EXTRACT_STRING(proc_name, stmt->left);

    for (ast_node *list = ast_misc_attr_value_list; list; list = list->right) {
      ast_node *misc_attr_value = list->left;
      // we found a nested list which should be nested dummy_test with info
      // @attribute(cql:autotest=(..., (dummy_test, ...), ...))
      if (is_ast_misc_attr_value_list(misc_attr_value)) {
        collect_dummy_test_info(misc_attr_value, context);
        cg_test_helpers_dummy_test(stmt);
      }
      // we found autotest attribution
      // @attribute(cql:autotest=(dummy_table, dummy_test, dummy_insert, dummy_select, dummy_result_set))
      else {
        // In principle, any option can be combined with any other but some only make sense for procs with
        // a result.

        EXTRACT_STRING(autotest_attr_name, misc_attr_value);
        if (is_autotest_dummy_test(autotest_attr_name)) {
          cg_test_helpers_dummy_test(stmt);
        }

        // these options  are only for procs that return a result set
        if (has_result_set(stmt) || has_out_stmt_result(stmt) || has_out_union_stmt_result(stmt)) {
          if (is_autotest_dummy_table(autotest_attr_name)) {
            helper_flags |= DUMMY_TABLE;
            cg_test_helpers_dummy_table(proc_name);
          }
          else if (is_autotest_dummy_insert(autotest_attr_name)) {
            helper_flags |= DUMMY_INSERT;
            cg_test_helpers_dummy_insert(proc_name);
          }
          else if (is_autotest_dummy_select(autotest_attr_name)) {
            helper_flags |= DUMMY_SELECT;
            cg_test_helpers_dummy_select(proc_name);
          }
          else if (is_autotest_dummy_result_set(autotest_attr_name)) {
            helper_flags |= DUMMY_RESULT_SET;
            cg_test_helpers_dummy_result_set(proc_name);
          }
        }
      }
    }

    if (is_declare_proc_needed()) {
      // We only need a declare statement if there are procedures to write, except for dummy_test
      cg_test_helpers_declare_proc(stmt);
    }

    cg_th_decls = decls_saved;
    cg_th_procs = procs_saved;

    // generate test delimiters only if needed

    if (decls_temp.used > 1) {
      if (options.test) {
        bprintf(cg_th_decls, "\n-- The statement ending at line %d", stmt->lineno);
      }
      bprintf(cg_th_decls, "%s", decls_temp.ptr);
    }

    // We always generate a marker in the procs section, because there are cases
    // where we need to verify that we generated nothing.
    if (options.test) {
      bprintf(cg_th_procs, "\n-- The statement ending at line %d", stmt->lineno);
      if (procs_temp.used == 1) {
        // this gives us a nice clear message in the output
        bprintf(cg_th_procs, "\n-- no output generated --\n");
      }
    }

    bprintf(cg_th_procs, "%s", procs_temp.ptr);

    CHARBUF_CLOSE(procs_temp);
    CHARBUF_CLOSE(decls_temp);
  }
}

static void cg_test_helpers_create_proc_stmt(ast_node *stmt, ast_node *misc_attrs) {
  Contract(is_ast_create_proc_stmt(stmt));

  if (misc_attrs) {
    helper_flags = 0;
    dummy_test_infos = symtab_new();

    find_misc_attrs(misc_attrs, test_helpers_find_ast_misc_attr_callback, stmt);

    write_to_file |= helper_flags;
    symtab_delete(dummy_test_infos);
    dummy_test_infos = NULL;
  }
}

// Iterate through statement list
static void cg_test_helpers_stmt_list(ast_node *head) {
  Contract(is_ast_stmt_list(head));
  init_all_trigger_per_table();
  init_all_indexes_per_table();
  CHARBUF_OPEN(procs_buf);
  CHARBUF_OPEN(decls_buf);
  cg_th_procs = &procs_buf;
  cg_th_decls = &decls_buf;

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);

    if (is_ast_create_proc_stmt(stmt)) {
      EXTRACT_STRING(proc_name, stmt->left);
      cg_test_helpers_create_proc_stmt(stmt, misc_attrs);
    }
  }

  bprintf(cg_th_output, "%s", decls_buf.ptr);
  bprintf(cg_th_output, "\n");
  bprintf(cg_th_output, "%s", procs_buf.ptr);

  CHARBUF_CLOSE(decls_buf);
  CHARBUF_CLOSE(procs_buf);
  symtab_delete(all_tables_with_triggers);
  all_tables_with_triggers = NULL;
  symtab_delete(all_tables_with_indexes);
  all_tables_with_indexes = NULL;
}

// Main entry point for test_helpers
cql_noexport void cg_test_helpers_main(ast_node *head) {
  Contract(options.file_names_count == 1);
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  CHARBUF_OPEN(output_buf);

  cg_th_output = &output_buf;
  write_to_file = 0;
  helper_flags = 0;

  bprintf(cg_th_output, "%s", rt->source_prefix);
  cg_test_helpers_stmt_list(head);
  cql_write_file(options.file_names[0], cg_th_output->ptr);

  CHARBUF_CLOSE(output_buf);

  // Force the globals to null state so that they do not look like roots to LeakSanitizer
  // all of these should have been freed already.  This is the final safety net to prevent
  // non-reporting of leaks.

  gen_create_triggers = NULL;
  gen_drop_triggers = NULL;
  all_tables_with_triggers = NULL;
  all_tables_with_indexes = NULL;
  dummy_test_infos = NULL;
  cg_th_output = NULL;
  cg_th_decls = NULL;
  cg_th_procs = NULL;
  helper_flags = 0;
  write_to_file = 0;
}

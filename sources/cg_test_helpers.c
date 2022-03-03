/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_TEST_HELPERS)

// stubs to avoid link errors
cql_noexport void cg_test_helpers_main(ast_node *head) {}

#else

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

// hold all the table name, column name and column values provided by dummy_test node
static symtab *dummy_test_infos = NULL;

typedef struct dummy_test_info {
  list_item *found_tables;
  list_item *found_views;
  CSTR table_current;
  struct table_callbacks *callbacks;
} dummy_test_info;

static void find_all_table_nodes(dummy_test_info *info, ast_node *node);

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

// Find all triggers on the table "table_or_view_name" then find all the tables those
// triggers actions depend on. The tables in those triggers should be parf of dummy_test
// codegen otherwise trigger creation stmt will fail in dummy_test.
static void find_all_triggers_node(dummy_test_info *info, CSTR table_or_view_name) {
  symtab_entry *triggers_entry = symtab_find(all_tables_with_triggers, table_or_view_name);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.if_not_exists_callback = cg_test_helpers_force_if_not_exists;

  // We can safely visit all the triggers because we know we visit any given table only once

  if (triggers_entry) {
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
      continue_find_table_node(info->callbacks, create_trigger_stmt);
    }
  }
}

//  - looks up all table relationships instead of just tables reference in a proc (follows the FKs)
//  - looks up drop table statements
//  - looks up triggers, and then the tables referenced in those triggers
static void found_table_or_view(CSTR _Nonnull table_or_view_name, ast_node *_Nonnull table_or_view, void *_Nullable context) {
  Contract(table_or_view);

  dummy_test_info *info = (dummy_test_info *)context;

  bool deleted = table_or_view->sem->delete_version > 0;

  // tables/views that are deleted have no business appearing in the dummy test output
  if (!deleted) {

    // Now let's walk through the new found table (table_or_view_name) to find all the tables it
    // depends on.  This is to find the FKs inside it.  Note that we don't have to check for
    // cycles because the walker driving all of this already does that, we just go.
    continue_find_table_node(info->callbacks, table_or_view);

    // Items will naturally be inserted at the front of the list because add_item_to_list always adds
    // at the head.  We don't want duplicates so we need to check.  We do want newly found items to
    // go to the head because as visit things we always want it to be the case that dependencies we
    // visit later end up at the front.  So if A depends on B then B will be first in the list.

    // Note tables do not directly depend on views so what's going to happen here is that
    // we will follow the FK chain and the deepest table will emitted first, hence be at the tail of the list.
    // Now the thing is one of those tables might have a trigger...the trigger itself could have
    // additional dependencies such as views.  This is ok, this is sort of an indirect table to view
    // dependency but the thing is in this case the view must be created AFTER the tables not before
    // to manage this we keep a view list and a table list which we will stitch together at the end
    // so that all the views are after all the tables

    // This callback is invoked exactly once per table/view by the walker so we already know we
    // have to add the item to the list, we don't need to keep our own state.

    // note by now we've already visited and added things inside us so our dependencies are already in the list
    if (is_ast_create_view_stmt(table_or_view)) {
      add_item_to_list(&info->found_views, table_or_view);
    }
    else {
      add_item_to_list(&info->found_tables, table_or_view);
    }

    // Find all triggers on the table "table_or_view_name" then find all of the tables and triggers
    // referenced by them.  These must come after the table itself has been analyzed
    find_all_triggers_node(info, table_or_view_name);
  }
}

static void find_all_table_nodes(dummy_test_info *info, ast_node *node) {
  table_callbacks callbacks = {
    .callback_any_table = found_table_or_view,
    .callback_any_view = found_table_or_view,
    .callback_context = info,
    .notify_table_or_view_drops = true,
    .notify_fk = true,
    .notify_triggers = true,
  };

  info->callbacks  = &callbacks;
  find_table_refs(&callbacks, node);

  // stitch the views to the tables to make one list, views first
  for (list_item *item = info->found_views; item; item = item->next) {
     if (!item->next) {
       item->next = info->found_tables;
       info->found_tables = info->found_views;
       break;
     }
  }
  // this shouldn't be used after it's been linked in
  info->found_views = NULL;
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
    EXTRACT_STRING(lit, value);
    bprintf(output, "%s", lit);
  }
  else if (is_ast_num(value)) {
    EXTRACT_NUM_VALUE(lit, value);
    bprintf(output, "%s", lit);
  }
  else if (is_ast_null(value)) {
    bprintf(output, "NULL");
  }
  else {
    Contract(is_ast_blob(value));
    EXTRACT_BLOBTEXT(lit, value);
    bprintf(output, "%s", lit);
  }
}

// Find the parent column referenced in the foreign key statement by child table
// "table_name" and column "column_name". We use this function to find parent
// column to do some validation to avoid foreign key violations in insert statement
// we emit.
static void find_parent_column(
  ast_node *_Nullable *_Nonnull referenced_table_ast,
  CSTR _Nullable *_Nonnull referenced_column,
  CSTR table_name,
  CSTR column_name)
{
  ast_node *table_ast = find_table_or_view_even_deleted(table_name);
  Contract(is_ast_create_table_stmt(table_ast));
  EXTRACT_NOTNULL(col_key_list, table_ast->right);
  *referenced_table_ast = NULL;
  *referenced_column = NULL;

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

            *referenced_table_ast = find_table_or_view_even_deleted(ref_table_name);
            *referenced_column = ref_col_name;
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
        EXTRACT_STRING(referenced_table, fk_target->left);
        EXTRACT_ANY_NOTNULL(fk_name_list, fk_target->right);
        int32_t index = 0;
        while (index < column_index) {
          fk_name_list = fk_name_list->right;
          index++;
        }
        Invariant(fk_name_list);
        EXTRACT_STRING(fk_col_name, fk_name_list->left);
        *referenced_table_ast = find_table_or_view_even_deleted(referenced_table);
        *referenced_column = fk_col_name;
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
// is actually included.
// the parent column might have multiple values available. We use "index" to specify
// the index of the one we want to emit.
// e.g: Foo table has a foreign key column 'A' referencing column 'B' on the table Bar.
// If a value for column 'B' of table Bar was specified in dummy_test info then that
// value will be populated to column 'B' of table Foo
static void cg_parent_column_value(charbuf *output, CSTR table_name, CSTR column_name, int32_t index) {
  ast_node *referenced_table_ast;
  CSTR referenced_column;
  find_parent_column(&referenced_table_ast, &referenced_column, table_name, column_name);

  if (referenced_table_ast) {
    CSTR referenced_table_name = referenced_table_ast->sem->sptr->struct_name;
    symtab_entry *referenced_table_entry = symtab_find(dummy_test_infos, referenced_table_name);

    if (referenced_table_entry) {
      symtab *fk_col_name_buf = (symtab *)referenced_table_entry->val;
      symtab_entry *fk_column_values_entry = symtab_find(fk_col_name_buf, referenced_column);

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

// Emit a literal using an integer value base on the sem type.  e.g. quote it, cast it to blog, etc.
static void cg_dummy_test_emit_integer_value(charbuf *output, sem_t col_type, int32_t value) {
  if (is_numeric(col_type)) {
    bprintf(output, "%d", value);
  } else if (is_blob(col_type)) {
    bprintf(output, "CAST(\'%d\' as blob)", value);
  } else {
    bprintf(output, "\'%d\'", value);
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
    symtab *col_syms = symtab_new();

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

            symtab_add(col_syms, column_name, NULL);
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
        if (!symtab_find(col_syms, column_name)) {
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
                cg_dummy_test_emit_integer_value(&str_val, col_type, cg_validate_value_range(index_value));
              }
            }
            bprintf(&names, "%s%s", comma, column_name);
            bprintf(&values, "%s", comma);
            comma = ", ";

            if (str_val.used > 1) {
              bprintf(&values, "%s", str_val.ptr);
            } else {
              cg_dummy_test_emit_integer_value(&values, col_type, index_value);
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
    symtab_delete(col_syms);
  } while (add_row);
}

// Walk through all triggers and create a dictionnary of triggers per tables.
static void init_all_trigger_per_table() {
  Contract(all_tables_with_triggers == NULL);
  all_tables_with_triggers = symtab_new();

  for (list_item *item = all_triggers_list; item; item = item->next) {
    EXTRACT_NOTNULL(create_trigger_stmt, item->ast);
    EXTRACT_NOTNULL(trigger_body_vers, create_trigger_stmt->right);
    EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
    EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
    EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
    EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
    EXTRACT_ANY_NOTNULL(table_name_ast, trigger_target_action->left);
    EXTRACT_STRING(table_name, table_name_ast);

    if (create_trigger_stmt->sem->delete_version > 0) {
      // dummy_test should not emit deleted trigger
      continue;
    }

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

    if (create_index_stmt->sem->delete_version > 0) {
      // dummy_test should not emit deleted indexes
      continue;
    }

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
    EXTRACT_NOTNULL(create_index_stmt, index_ast);
    EXTRACT_NOTNULL(create_index_on_list, create_index_stmt->left);
    EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
    EXTRACT_STRING(index_name, index_name_ast);

    gen_statement_with_callbacks(index_ast, callback);
    bprintf(gen_create_indexes, ";\n");
    bprintf(gen_drop_indexes, "DROP INDEX IF EXISTS %s;\n", index_name);
  }
}

static CSTR get_table_or_view_name(ast_node *table_or_view) {
  CSTR table_name = NULL;
  if (is_ast_create_table_stmt(table_or_view)) {
    EXTRACT_NOTNULL(create_table_name_flags, table_or_view->left);
    EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
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
    .found_tables = NULL,
    .found_views = NULL
  };

  // First thing we have to do is gather all the tables that are used transitively by the procedure
  // that needs dummy_test helpers.
  find_all_table_nodes(&info, stmt);

  // If the create proc statement does not reference any tables, there is nothing to emit
  if (info.found_tables == NULL) {
    CHARBUF_CLOSE(drop_triggers);
    CHARBUF_CLOSE(create_triggers);
    return;
  }

  // There are some tables, so we've work to do, there are several types of functions emitted
  // by this helper type, we're going to need them all.

  int32_t value_seed = 123;

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.if_not_exists_callback = cg_test_helpers_force_if_not_exists;
  callbacks.mode = gen_mode_no_annotations;

  CHARBUF_OPEN(gen_create_tables);
  CHARBUF_OPEN(gen_drop_tables);
  CHARBUF_OPEN(gen_populate_tables);
  CHARBUF_OPEN(gen_read_tables);
  CHARBUF_OPEN(gen_declare_funcs);
  CHARBUF_OPEN(gen_drop_indexes);

  // Here we record that we actually emitted some dummy test stuff for this proc, this helps us
  // decide if we need the test markers in test mode.
  helper_flags |= DUMMY_TEST;

  // The found tables list begins in an order that is correct for dropping (i.e. the "leaf" tables/views are first)
  // do that now...

  for (list_item *item = info.found_tables; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(table_or_view, item->ast);
    Invariant(is_ast_create_table_stmt(table_or_view) || is_ast_create_view_stmt(table_or_view));
    CSTR table_name = get_table_or_view_name(table_or_view);
    bprintf(&gen_drop_tables, "DROP %s IF EXISTS %s;\n", is_ast_create_table_stmt(table_or_view) ? "TABLE" : "VIEW", table_name);
  }

  // Reverse the list to get the tables back into a safe-to-declare order that we can loop over
  // to emit table creation of parent tables before child tables.
  reverse_list(&info.found_tables);

  // For each found table we're going to do some table specific things

  for (list_item *item = info.found_tables; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(table_or_view, item->ast);

    ast_node *ast_to_emit = table_or_view;

    // the virtual table ast in the symbol table points to the table decl part of the virtual table create
    // we want the whole statement so we have to back up one notch up the tree.
    bool_t is_virtual_table = table_or_view->parent && is_ast_create_virtual_table_stmt(table_or_view->parent);
    if (is_virtual_table) {
      ast_to_emit = table_or_view->parent;
    }

    // First thing we need is the CREATE DDL for the item in question, make that now
    gen_set_output_buffer(&gen_create_tables);
    gen_statement_with_callbacks(ast_to_emit, &callbacks);
    bprintf(&gen_create_tables, ";\n");

    // Next we need the DDL for any indices that may be on the table, we'll generate
    // the CREATE for those indices and a DROP for the indices.  The CREATE goes with
    // the table creates.  The indices may be dropped seperately so the DROP goes
    // in its own buffer
    CSTR table_name = get_table_or_view_name(table_or_view);
    cg_emit_index_stmt(table_name, &gen_create_tables, &gen_drop_indexes, &callbacks);

    // Next we generate a fragment to populate data for this table using the current seed value
    // We don't do this for views or virtual tables
    if (is_ast_create_table_stmt(table_or_view) && !is_virtual_table) {
      cg_dummy_test_populate(&gen_populate_tables, table_or_view, &value_seed);
    }

    // Finally, there is a helper procedure for each table or view that just reads all that
    // data out of it.  Most tests don't use all of them but it's only test code so size doesn't
    // matter so much and it's super easy to have them all handy so we aren't picky.

    bprintf(&gen_read_tables, "\n");
    bprintf(&gen_read_tables, "CREATE PROC test_%s_read_%s()\n", proc_name, table_name);
    bprintf(&gen_read_tables, "BEGIN\n");
    bprintf(&gen_read_tables, "  SELECT * FROM %s;\n", table_name);
    bprintf(&gen_read_tables, "END;\n");
  }

  // At this point we're done with all the tables, we're ready to generate the main methods
  // plus do the rest of the housekeeping

  // Emit declare functions because they may be needed for schema and query validation
  // We don't try to guess which functions were used, we just emit the correct declarations for them all.
  // We could in principle do this one time for the entire translation unit but duplicates don't hurt anyway.
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
  //
  // We create the create/drop triggers helpers even for procs that don't use any
  // tables with triggers. Otherwise callsites might have to change when triggers
  // are added/removed from the schema.
  if (gen_drop_triggers->used <= 1) {
    // Similarly, to avoid the procs signature changing based on triggers being
    // added/removed we use the below snippet to force the procedure to use the
    // db-using signature, even if no triggers are actually created.
    bprintf(gen_create_triggers, "IF @rc THEN END IF;\n");
  }
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_create_triggers()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, gen_create_triggers, 2);
  bprintf(cg_th_procs, "END;\n");

  // populate tables proc
  if (gen_populate_tables.used > 1) {
    bprintf(cg_th_procs, "\n");
    bprintf(cg_th_procs, "CREATE PROC test_%s_populate_tables()\n", proc_name);
    bprintf(cg_th_procs, "BEGIN\n");
    bindent(cg_th_procs, &gen_populate_tables, 2);
    bprintf(cg_th_procs, "END;\n");
  }

  // drop tables proc
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_drop_tables()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, &gen_drop_tables, 2);
  bprintf(cg_th_procs, "END;\n");

  // drop trigger proc
  if (gen_drop_triggers->used <= 1) {
    bprintf(gen_drop_triggers, "IF @rc THEN END IF;\n");
  }
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC test_%s_drop_triggers()\n", proc_name);
  bprintf(cg_th_procs, "BEGIN\n");
  bindent(cg_th_procs, gen_drop_triggers, 2);
  bprintf(cg_th_procs, "END;\n");

  // read tables procedures
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
}

// check whether "value" already exist in "column_values". This is used to avoid
// having the same value repeated in a column. It can only happens if the value
// explicitely added to dummy_test info match values from @dummy_seed.
static bool_t is_column_value_present(bytebuf *column_values, sem_t column_type, ast_node *value) {
  bool_t exist = 0;
  ast_node **list = (ast_node **)column_values->ptr;
  int32_t size = column_values->used / sizeof(ast_node *);
  for (int32_t i = 0; i < size; i++) {
    ast_node *l = list[i];
    sem_t col_type = core_type_of(column_type);
    ast_node *r = value;

    // The numbers get some special treatment because unary minus might be in the node
    // if it's present we peel it off and compare what's left.  Remember all numerics
    // are represented as positive numbers with possibly a negation operator if needed.
    // It has to be this way so that 1-5 doesn't parse as 1 and -5 with no operator.
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
// the row in child table references a row in then parent table when we emit
// insert statement for both tables. This function is only called from foreign key columns.
// e.g: Suppose you have table "Foo" with column "id" which is a foreign key reference to
//      "id" in table "Bar". If the user has manually added a value for the column "id" in
//      the table "Foo" in dummy_test info then this method will add the same value
//      to column "id" of the table "Bar" into its dummy_test info.
static void add_value_to_referenced_table(
  CSTR table_name,
  CSTR column_name,
  sem_t column_type,
  ast_node *column_value)
{
  // if the data column is "NULL" then it doesn't actually have to go into the parent at all
  if (is_ast_null(column_value)) {
    return;
  }

  ast_node *referenced_table_ast;
  CSTR referenced_column;
  find_parent_column(&referenced_table_ast, &referenced_column, table_name, column_name);
  CSTR referenced_table_name = referenced_table_ast->sem->sptr->struct_name;

  symtab *fk_col_syms = symtab_ensure_symtab(dummy_test_infos, referenced_table_name);
  bytebuf *fk_column_values = symtab_ensure_bytebuf(fk_col_syms, referenced_column);

  // We want to avoid adding the same value to multiple rows in the same table.

  // Note this is imperfect:  if the FK relationship is multi-columnar then we're going
  // to have a bug here.  e.g.  if the FK columns are (a,b) and we have already added
  // (1,2) to the fk table we could get into trouble when try to add (1,3) because
  // then "1" will look like it's already there.  We live with this limitation
  // because this is only a test helper...  it's entirely optional anyway and if you
  // really want full control you can always write your own data inserter.

  if (!is_column_value_present(fk_column_values, column_type, column_value)) {
    bytebuf_append_var(fk_column_values, column_value);
  }
}

// Walk through the dummy_test attributes collecting this information.  This
// is a set of columns and values which will later be used in the generated
// data insertion procedure.  This is entirely optional but if you want specific
// data to be inserted you can put it in the attribute.
static void collect_dummy_test_info(
  ast_node *_Nullable misc_attr_value_list,
  void *_Nullable context)
{
  EXTRACT_STRING(autotest_attr_name, misc_attr_value_list->left);

  if (is_autotest_dummy_test(autotest_attr_name)) {
    // walkthrough dummy_test tree and retreive the table name then the column name
    // of the table name and then the column values of the column names. We repeat
    // it for the next table info.
    for (ast_node *dummy_attr = misc_attr_value_list->right; dummy_attr; dummy_attr = dummy_attr->right) {
      bytebuf col_data_buf;
      bytebuf col_type_buf;
      bytebuf col_name_buf;

      bytebuf_open(&col_data_buf);
      bytebuf_open(&col_type_buf);
      bytebuf_open(&col_name_buf);

      // the data attribute looks kind of like this:
      // @attribute(cql:autotest = (
      //   .. other auto test attributes
      //   (dummy_test,
      //     (table_name1, (col1, col2), (col1_val1, col2_val1), (col1_val2, col2_val2) ),
      //     (table_name2, (col1, col2), (col1_val1, col2_val1), (col1_val2, col2_val2) ),
      //     ...
      //   )
      //   .. other auto test attributes
      // ))
      //
      // we're concerned with the dummy_test entries here, they have a very specific format
      // i.e. first the table then the column names, and then a list of matching columns and values

      // note that sem.c has already verified the correct shape, see error CQL0277

      // collect table name from dummy_test info
      ast_node *table_list = dummy_attr->left;
      EXTRACT_STRING(table_name, table_list->left);
      symtab *col_syms = symtab_ensure_symtab(dummy_test_infos, table_name);

      // collect column names from dummy_test info
      ast_node *column_name_list = table_list->right;
      for (ast_node *list = column_name_list->left; list; list = list->right) {
        EXTRACT_STRING(column_name, list->left);
        sem_t col_type = find_column_type(table_name, column_name);

        bytebuf *column_values = symtab_ensure_bytebuf(col_syms, column_name);

        // store the column meta data, create space to hold values in databuf
        bytebuf_append_var(&col_data_buf, column_values);
        bytebuf_append_var(&col_type_buf, col_type);
        bytebuf_append_var(&col_name_buf, column_name);
      }

      // collect column value from dummy_test info. We can have multiple rows of column value
      for (ast_node *values_ast = column_name_list->right; values_ast; values_ast = values_ast->right) {

        int32_t column_index = 0;

        // collect one row of column value
        for (ast_node *list = values_ast->left; list; list = list->right) {
          ast_node *misc_attr_value = list->left;
          Contract(col_data_buf.used);
          bytebuf *column_values = ((bytebuf **) col_data_buf.ptr)[column_index];
          sem_t column_type = ((sem_t *) col_type_buf.ptr)[column_index];
          CSTR column_name = ((CSTR *) col_name_buf.ptr)[column_index];

          bytebuf_append_var(column_values, misc_attr_value);
          column_index++;

          // If a column value is added to dummy_test info for a foreign key column then
          // we need to make sure that same column value is also added as a value in the
          // the referenced table's dummy_test info.
          // e.g.
          //   create table A(id integer primary key);
          //   create table B(id integer primary key references A(id));
          //
          // If there is sample data provided for B.id then we must also ensure that
          // the value provided for B.id is also add as a sample row in A with the same
          // value for id.
          if (is_foreign_key(column_type)) {
            add_value_to_referenced_table(table_name, column_name, column_type, misc_attr_value);
          }
        }
      }

      bytebuf_close(&col_data_buf);
      bytebuf_close(&col_type_buf);
      bytebuf_close(&col_name_buf);
    }
  }
}

// This is invoked for every misc attribute on every create proc statement
// in this translation unit.  We're looking for attributes of the form cql:autotest=(...)
// and we ignore anything else.
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

    // We're actually using intermediate buffers here only so that
    // we can test if they were used (non-empty) at the end so that
    // we can emit the test delimeters if and only if they are needed
    // these are otherwise going to pass through to gh_th_decls and _procs
    // as they came in.
    CHARBUF_OPEN(decls_temp);
    CHARBUF_OPEN(procs_temp);

    charbuf *decls_saved = cg_th_decls;
    charbuf *procs_saved = cg_th_procs;

    cg_th_decls = &decls_temp;
    cg_th_procs = &procs_temp;

    EXTRACT_STRING(proc_name, stmt->left);

    for (ast_node *list = ast_misc_attr_value_list; list; list = list->right) {
      ast_node *misc_attr_value = list->left;
      // We found a nested list which should be nested dummy_test with info
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

        // these options are only for procs that return a result set
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
      // if we emitted one of the helpers above that sets helper_flags it tells us that we
      // need to emit a declaration for the procedure that had the attribute (i.e. the thing
      // we are trying to mock).  The generated code uses the name of that procedure in a LIKE
      // clause and it won't otherwise be in our output so we emit a declaration for it here.
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

// Having found a create proc statement, we set up to get the attributes on it.
// The find_misc_attrs callback will be invoked for every attribute on the procedure.
// test_helpers_find_ast_misc_attr_callback() will look for the relevant ones.
static void cg_test_helpers_create_proc_stmt(ast_node *stmt, ast_node *misc_attrs) {
  Contract(is_ast_create_proc_stmt(stmt));

  if (misc_attrs) {
    helper_flags = 0;
    dummy_test_infos = symtab_new();

    find_misc_attrs(misc_attrs, test_helpers_find_ast_misc_attr_callback, stmt);

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

// Force the globals to null state so that they do not look like roots to LeakSanitizer
// all of these should have been freed already.  This is the final safety net to prevent
// non-reporting of leaks.
static void cg_test_helpers_reset_globals() {
  gen_create_triggers = NULL;
  gen_drop_triggers = NULL;
  all_tables_with_triggers = NULL;
  all_tables_with_indexes = NULL;
  dummy_test_infos = NULL;
  cg_th_output = NULL;
  cg_th_decls = NULL;
  cg_th_procs = NULL;
  helper_flags = 0;
}

// Main entry point for test_helpers
cql_noexport void cg_test_helpers_main(ast_node *head) {
  Contract(options.file_names_count == 1);
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();
  cg_test_helpers_reset_globals();

  CHARBUF_OPEN(output_buf);

  cg_th_output = &output_buf;

  bprintf(cg_th_output, "%s", rt->source_prefix);
  cg_test_helpers_stmt_list(head);
  cql_write_file(options.file_names[0], cg_th_output->ptr);

  CHARBUF_CLOSE(output_buf);
  cg_test_helpers_reset_globals();
}

#endif

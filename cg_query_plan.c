/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include "cg_query_plan.h"
#include "charbuf.h"
#include "cql.h"
#include "encoders.h"
#include "gen_sql.h"
#include "cg_common.h"
#include "sem.h"

static void cg_qp_one_stmt(ast_node *stmt);

static charbuf *schema_stmts;
static charbuf *query_plans;
static CSTR current_procedure_name;
static charbuf *current_ok_table_scan;

// Count sql statement found in ast
static uint32_t sql_stmt_count = 0;

static gen_sql_callbacks *cg_qp_callbacks = NULL;

// When generating the query plan report there will be no actual
// variable values to use in the query.  To get around this
// we replace all variable references with a type-correct version
// of the constant "1".  This gives us a pretty good query plan
// even if there are parameters in the real query.
// Note: if the SQLite query processor were much more fancy this
// wouldn't work at all. Constants matter.
static bool_t variables_callback(
  struct ast_node *_Nonnull ast,
  void *_Nullable context,
  charbuf *_Nonnull output)
{
  sem_t sem_type = ast->sem->sem_type;
  if (is_numeric(sem_type) || is_object(sem_type)) {
    bprintf(output, "1");
  }
  else if (is_text(sem_type)) {
    bprintf(output, "'1'");
  }
  else {
    Contract(is_blob(sem_type));
    bprintf(output, "cast('1' as blob)");
  }

  return true;
}

static void cg_qp_explain_query_stmt(ast_node *stmt) {
  sql_stmt_count++;
  CHARBUF_OPEN(proc);
  CHARBUF_OPEN(body);
  CHARBUF_OPEN(sql);
  CHARBUF_OPEN(cstr_sql);
  CHARBUF_OPEN(cstr_sql2);

  gen_set_output_buffer(&sql);
  gen_statement_with_callbacks(stmt, cg_qp_callbacks);
  cg_encode_c_string_literal(sql.ptr, &cstr_sql);
  // Encode the encoded special character to be able retain the special characteres in the
  // json string output
  bprintf(&cstr_sql2, "\"");
  for (int32_t i = 1; i < cstr_sql.used - 2; i++) {
    cg_encode_char_as_c_string_literal(cstr_sql.ptr[i], &cstr_sql2);
  }
  bprintf(&cstr_sql2, "\"");
  bprintf(&body, "DECLARE stmt TEXT NOT NULL;\n");
  // Storing the string statement in a variable prevents codegen from stripping the '\n'
  // We strip '\n' when formatting the one line string sql statement to a multiline c
  // string sql statement. It gave a better look to the code gen but that does not work
  // for query plan use case because we want to keep '\n' in the string statement because
  // we re-use that statement later to print it in Diff or the terminal.
  bprintf(&body, "SET stmt := %s;\n", cstr_sql2.ptr);
  bprintf(&body, "INSERT INTO sql_temp(id, sql) VALUES(%d, stmt);\n", sql_stmt_count);
  if (current_procedure_name && current_ok_table_scan && current_ok_table_scan->used > 1) {
    bprintf(
        &body,
        "INSERT INTO ok_table_scan(sql_id, proc_name, table_names) VALUES(%d, \"%s\", \"%s\");\n",
        sql_stmt_count,
        current_procedure_name,
        current_ok_table_scan->ptr);
  }
  bprintf(&body, "DECLARE C CURSOR FOR EXPLAIN QUERY PLAN\n");
  bprintf(&body, "%s;\n", sql.ptr);
  bprintf(&body, "LOOP FETCH C\n");
  bprintf(&body, "BEGIN\n");
  bprintf(&body, "  INSERT INTO plan_temp(sql_id, iselectid, iorder, ifrom, zdetail) VALUES(%d, C.iselectid, C.iorder, C.ifrom, C.zdetail);\n", sql_stmt_count);
  bprintf(&body, "END;\n");

  bprintf(&proc, "CREATE PROC populate_query_plan_%d()\n", sql_stmt_count);
  bprintf(&proc, "BEGIN\n");
  bindent(&proc, &body, 2);
  bprintf(&proc, "END;\n\n");

  bprintf(query_plans, "%s", proc.ptr);

  CHARBUF_CLOSE(cstr_sql2);
  CHARBUF_CLOSE(cstr_sql);
  CHARBUF_CLOSE(sql);
  CHARBUF_CLOSE(body);
  CHARBUF_CLOSE(proc);
}

static void cg_qp_sql_stmt(ast_node *ast) {
  gen_set_output_buffer(schema_stmts);
  gen_statement_with_callbacks(ast, cg_qp_callbacks);
  bprintf(schema_stmts, ";\n");
}

static void cg_qp_ok_table_scan_callback(
    CSTR _Nonnull name,
    ast_node* _Nonnull misc_attr_value,
    void* _Nullable context) {
  Contract(context && is_ast_str(misc_attr_value));

  charbuf *ok_table_scan_buf = (charbuf *)context;
  EXTRACT_STRING(table_name, misc_attr_value);
  if (ok_table_scan_buf->used > 1) {
    bprintf(ok_table_scan_buf, ",");
  }
  // the "#" around the table_name are used as delimiter of the
  // table_name's word to later find tables that are ok to scan.
  bprintf(ok_table_scan_buf, "#%s#", table_name);
}

// There are now extract work to be done in create_proc_stmt substree.
// We need to associate the proc name and ok_table_scan's tables to all
// sql statement found in the proc.
// It's used later to establish the relationship between a statement and
// a proc name (but all "ok_table_scan" attribution). But also to detect
// whether or not scan table alert should be silent because of "ok_table_scan".
//
// The code in this function collect all the tables of "ok_table_scan" attr
// and associate them with the proc name and the statement ids in the proc.
//
// e.g: With the info below now avaialable we can now figure out wheter or
// an alert of scan table can be made on a particular statement id.
// stmt(id) <-> proc_name <-> table_name (ok_table_scan)
static void cg_qp_create_proc_stmt(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  Contract(current_procedure_name == NULL);
  Contract(current_ok_table_scan == NULL);

  CHARBUF_OPEN(ok_table_scan_buf);
  current_ok_table_scan = &ok_table_scan_buf;

  // The statement has attributions therefore we should collect the values
  // of "ok_table_scan" attribution if applicable. Otherwise we have nothing
  // record on this proc related to "ok_table_scan".
  if (is_ast_stmt_and_attr(ast->parent)) {
    EXTRACT_NOTNULL(stmt_and_attr, ast->parent);
    EXTRACT_NOTNULL(misc_attrs, stmt_and_attr->left);
    EXTRACT_STRING(table_name, ast->left);

    current_procedure_name = table_name;
    find_ok_table_scan(misc_attrs, cg_qp_ok_table_scan_callback, (void *) &ok_table_scan_buf);
  }

  cg_qp_one_stmt(ast->left);
  cg_qp_one_stmt(ast->right);

  current_procedure_name = NULL;
  current_ok_table_scan = NULL;
  CHARBUF_CLOSE(ok_table_scan_buf);
}

static void cg_qp_one_stmt(ast_node *stmt) {
  if (!stmt || is_ast_primitive(stmt)) {
    return;
  }

  symtab_entry *entry = symtab_find(cg_stmts, stmt->type);
  if (entry) {
    ((void (*)(ast_node*))entry->val)(stmt);
  } else {
    cg_qp_one_stmt(stmt->left);
    cg_qp_one_stmt(stmt->right);
  }
}

static void cg_qp_stmt_list(ast_node *head) {
  Contract(is_ast_stmt_list(head));
  for (ast_node *stmt = head; stmt; stmt = stmt->right) {
    cg_qp_one_stmt(stmt->left);
  }
}

static void emit_populate_no_table_scan_proc(charbuf *output) {
  CHARBUF_OPEN(no_scan_tables_buf);

  for(list_item *item = all_tables_list; item; item = item->next) {
    if (is_ast_create_table_stmt(item->ast)) {
      EXTRACT_MISC_ATTRS(item->ast, misc_attrs);
      if (misc_attrs != NULL) {
        EXTRACT(create_table_name_flags, item->ast->left);
        EXTRACT(table_flags_attrs, create_table_name_flags->left);
        EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
        EXTRACT_STRING(name, name_ast);
        if (exists_attribute_str(misc_attrs, "no_table_scan")) {
          if (no_scan_tables_buf.used > 1) {
            bprintf(&no_scan_tables_buf, "\n  UNION  ");
          }
          bprintf(&no_scan_tables_buf, "SELECT \"%s\"", name);
        }
      }
    }
  }

  bprintf(output, "CREATE PROC populate_no_table_scan()\n"
                  "BEGIN\n");
  if (no_scan_tables_buf.used > 1) {
    bprintf(output, "  INSERT OR IGNORE INTO no_table_scan(table_name) %s;\n", no_scan_tables_buf.ptr);
  }
  bprintf(output, "END;\n");

  CHARBUF_CLOSE(no_scan_tables_buf);
}

static void cg_qp_emit_declare_func(charbuf *output) {
  // Emit declare functions because it may be needed for schema and query validation
  gen_set_output_buffer(output);
  for (list_item *item = all_functions_list; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(any_func, item->ast);
    Contract(is_ast_declare_func_stmt(any_func) || is_ast_declare_select_func_stmt(any_func));
    if (is_ast_declare_select_func_stmt(any_func)) {
      gen_statement_with_callbacks(any_func, cg_qp_callbacks);
      bprintf(output, ";\n");
    }
  }
}

static void cg_qp_emit_create_schema_proc(charbuf *output) {
  bprintf(output, "CREATE PROC create_schema()\n"
                  "BEGIN\n");
  bindent(output,    schema_stmts, 2);
  bprintf(output, "  CREATE TABLE sql_temp(\n"
                  "    id INT NOT NULL PRIMARY KEY,\n"
                  "    sql TEXT NOT NULL\n"
                  "  ) WITHOUT ROWID;\n"
                  "  CREATE TABLE plan_temp(\n"
                  "    iselectid INT NOT NULL,\n"
                  "    iorder INT NOT NULL,\n"
                  "    ifrom INT NOT NULL,\n"
                  "    zdetail TEXT NOT NULL,\n"
                  "    sql_id INT NOT NULL,\n"
                  "    FOREIGN KEY (sql_id) REFERENCES sql_temp(id)\n"
                  "  );\n"
                  "  CREATE TABLE no_table_scan(\n"
                  "    table_name TEXT NOT NULL PRIMARY KEY\n"
                  "  );\n"
                  "  CREATE TABLE alert(\n"
                  "    table_name TEXT NOT NULL PRIMARY KEY\n"
                  "  );\n"
                  "  CREATE TABLE ok_table_scan(\n"
                  "    sql_id INT NOT NULL PRIMARY KEY,\n"
                  "    proc_name TEXT NOT NULL,\n"
                  "    table_names TEXT NOT NULL\n"
                  "  ) WITHOUT ROWID;\n"
                  "END;\n");
}

static void emit_populate_tables_proc(charbuf *output) {
  bprintf(output, "%s", query_plans->ptr);
}

static void emit_print_sql_statement_proc(charbuf *output) {
  bprintf(output,
          "%s",
          "CREATE PROC print_sql_statement(sql_id integer not null)\n"
          "BEGIN\n"
          "  DECLARE C CURSOR FOR SELECT * FROM sql_temp WHERE id = sql_id LIMIT 1;\n"
          "  FETCH C;\n"
          "  CALL printf(\"  \\\"%s\\\",\\n\", C.sql);\n"
          "END;\n"
  );
}

static void emit_populate_alert_table_proc(charbuf *output) {
  bprintf(output, "CREATE PROC populate_alert_table(table_ text not null)\n");
  bprintf(output, "BEGIN\n");
  bprintf(output, "  INSERT OR IGNORE INTO alert SELECT upper(table_) as table_name FROM plan_temp\n");
  bprintf(output, "    WHERE ( zdetail GLOB ('*[Ss][Cc][Aa][Nn]* ' || table_) OR \n");
  bprintf(output, "            zdetail GLOB ('*[Ss][Cc][Aa][Nn]* ' || table_ || ' *')\n");
  bprintf(output, "          )\n");
  bprintf(output, "    AND sql_id NOT IN (\n");
  bprintf(output, "      SELECT sql_id from ok_table_scan\n");
  bprintf(output, "        WHERE table_names GLOB ('*#' || table_ || '#*')\n");
  bprintf(output, "    );\n");
  bprintf(output, "END;\n");
}

static void emit_print_table_scan_violation_proc(charbuf *output) {
  bprintf(output,
          "%s",
          "CREATE PROC print_table_scan_violation()\n"
          "BEGIN\n"
          "  DECLARE C CURSOR FOR SELECT table_name FROM no_table_scan;\n"
          "  LOOP FETCH C\n"
          "  BEGIN\n"
          "    CALL populate_alert_table(C.table_name);\n"
          "  END;\n\n"
          "  DECLARE count INTEGER NOT NULL;\n"
          "  SET count := (SELECT COUNT(*) FROM alert);\n"
          "  IF count > 0 THEN \n"
          "    CALL printf(\"[\\\"Alert\\\"],\\n\");\n"
          "    CALL printf(\"[{\\\"value\\\": \\\"TABLE SCAN VIOLATION:  \");\n"
          "    DECLARE C2 CURSOR FOR SELECT group_concat(table_name, ', ') AS table_list FROM alert;\n"
          "    FETCH C2;\n"
          "    CALL printf(\"%s\", C2.table_list);\n"
          "    CALL printf(\"\\\", \\\"style\\\": {\\\"fontSize\\\": 14, \\\"color\\\": \\\"red\\\", \\\"fontWeight\\\": \\\"bold\\\"}}],\\n\");\n"
          "  ELSE\n"
          "    CALL printf(\"[],\\n\");\n"
          "  END IF;"
          "END;\n"
  );
}

static void emit_print_query_plan_stat_proc(charbuf *output) {
  bprintf(output,
          "%s",
          "CREATE PROC print_query_plan_stat(id_ integer not null)\n"
          "BEGIN\n"
          "  CALL printf(\"  [\\n\");\n"
          "  DECLARE Ca CURSOR FOR\n"
          "  WITH\n"
          "    scan(name, count, priority) AS (\n"
          "      SELECT 'SCAN', COUNT(*), 0 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%scan%' AND sql_id = id_\n"
          "    ),\n"
          "    search(name, count, priority) AS (\n"
          "      SELECT 'SEARCH', COUNT(*), 4 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%search%' AND iselectid NOT IN (\n"
          "          SELECT iselectid \n"
          "          FROM plan_temp \n"
          "          WHERE zdetail LIKE '%search%using%covering%'\n"
          "        ) AND sql_id = id_\n"
          "    ),\n"
          "    search_fast(name, count, priority) AS (\n"
          "      SELECT 'SEARCH USING COVERING', COUNT(*), 5 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%search%using%covering%' AND sql_id = id_\n"
          "    ),\n"
          "    bree(name, count, priority) AS (\n"
          "      SELECT 'TEMP B-TREE', COUNT(*), 1 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%temp b-tree%' AND sql_id = id_\n"
          "    ),\n"
          "    compound_subqueries(name, count, priority) AS (\n"
          "      SELECT 'COMPOUND SUBQUERIES', COUNT(*), 2 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%compound subqueries%' AND sql_id = id_\n"
          "    ),\n"
          "    execute_scalar(name, count, priority) AS (\n"
          "      SELECT 'EXECUTE SCALAR', COUNT(*), 3 \n"
          "        FROM plan_temp \n"
          "        WHERE zdetail LIKE '%execute scalar%' AND sql_id = id_\n"
          "    )\n"
          "  SELECT \n"
          "   CASE name\n"
          "     WHEN 'SCAN' THEN '{\"value\": \"' || name || '\", \"style\": {\"fontSize\": 14, \"color\": \"red\", \"fontWeight\": \"bold\"}}'\n"
          "     WHEN 'TEMP B-TREE' THEN '{\"value\": \"' || name || '\", \"style\": {\"fontSize\": 14, \"color\": \"red\", \"fontWeight\": \"bold\"}}'\n"
          "     ELSE '\"' || name || '\"'\n"
          "   END name,\n"
          "   CASE name\n"
          "     WHEN 'SCAN' THEN '{\"value\": ' || count || ', \"style\": {\"fontSize\": 14, \"color\": \"red\", \"fontWeight\": \"bold\"}}'\n"
          "     WHEN 'TEMP B-TREE' THEN '{\"value\": ' || count || ', \"style\": {\"fontSize\": 14, \"color\": \"red\", \"fontWeight\": \"bold\"}}'\n"
          "     ELSE '' || count\n"
          "   END value\n"
          "   FROM (\n"
          "   SELECT * FROM scan\n"
          "   UNION ALL\n"
          "   SELECT * FROM search\n"
          "   UNION ALL\n"
          "   SELECT * FROM search_fast\n"
          "   UNION ALL\n"
          "   SELECT * FROM bree\n"
          "   UNION ALL\n"
          "   SELECT * FROM compound_subqueries\n"
          "   UNION ALL\n"
          "   SELECT * FROM execute_scalar\n"
          "  )\n"
          "  WHERE count > 0 ORDER BY priority ASC, count DESC;\n"
          "  CALL printf(\"    [],\\n\");\n"
          "  LOOP FETCH Ca\n"
          "  BEGIN\n"
          "    CALL printf(\"    [%s, %s],\\n\", Ca.name, Ca.value);\n"
          "  END;\n"
          "  CALL printf(\"    []\\n\");\n"
          "  CALL printf(\"  ],\\n\");\n"
          "END;\n"
  );
}

static void emit_print_query_plan_graph_proc(charbuf *output) {
  bprintf(output,
          "%s",
          "CREATE PROC print_query_plan_graph(id_ integer not null)\n"
          "BEGIN\n"
          "  CALL printf(\"  \\\"\");\n"
          "  DECLARE C CURSOR FOR\n"
          "  WITH RECURSIVE\n"
          "    plan_chain(iselectid,  zdetail, level) AS (\n"
          "     SELECT 0 as  iselectid, '?' as  zdetail, 0 as level\n"
          "     UNION ALL\n"
          "     SELECT plan_temp.iselectid, plan_temp.zdetail, plan_chain.level+1 as level\n"
          "      FROM plan_temp JOIN plan_chain ON plan_temp.iorder=plan_chain.iselectid WHERE plan_temp.sql_id = id_\n"
          "     ORDER BY 3 DESC\n"
          "    )\n"
          "    SELECT\n"
          "     substr('                              ', 1, max(level - 1, 0)*4) ||\n"
          "     substr('|.............................', 1, min(level, 1)*4) ||\n"
          "     zdetail as graph_line FROM plan_chain;\n"
          "\n"
          "  LOOP FETCH C\n"
          "  BEGIN\n"
          "    CALL printf(\"\\\\n%s\", C.graph_line);\n"
          "  END;\n"
          "  CALL printf(\"\\\"\\n\");\n"
          "END;\n"
  );
}

static void emit_print_query_plan(charbuf *output) {
  bprintf(output,
          "CREATE PROC print_query_plan(sql_id integer not null)\n"
          "BEGIN\n"
          "  CALL printf(\"[\\n\");\n"
          "  CALL print_sql_statement(sql_id);\n"
          "  CALL print_query_plan_stat(sql_id);\n"
          "  CALL print_query_plan_graph(sql_id);\n"
          "  CALL printf(\"],\\n\");\n"
          "END;\n"
  );
}

#undef STMT_INIT
#define STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_qp_ ## x)

#undef STMT_INIT_EXPL
#define STMT_INIT_EXPL(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_qp_explain_query_stmt)

#undef STMT_INIT_DDL
#define STMT_INIT_DDL(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_qp_sql_stmt)

cql_noexport void cg_query_plan_main(ast_node *head) {
  sql_stmt_count = 0; // reset statics

  Contract(options.file_names_count == 1);
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  cg_stmts = symtab_new();

  STMT_INIT(create_proc_stmt);

  // schema
  //  * note probably need to add declare select function to this list
  //    that can be needed to correctly parse the body of triggers or views (which might use the function)
  STMT_INIT_DDL(create_table_stmt);
  STMT_INIT_DDL(create_index_stmt);
  STMT_INIT_DDL(create_view_stmt);
  STMT_INIT_DDL(create_trigger_stmt);

  // dml
  STMT_INIT_EXPL(select_stmt);
  STMT_INIT_EXPL(with_select_stmt);
  STMT_INIT_EXPL(with_insert_stmt);
  STMT_INIT_EXPL(update_stmt);
  STMT_INIT_EXPL(delete_stmt);
  STMT_INIT_EXPL(with_delete_stmt);
  STMT_INIT_EXPL(insert_stmt);
  STMT_INIT_EXPL(upsert_stmt);
  STMT_INIT_EXPL(drop_table_stmt);
  STMT_INIT_EXPL(drop_view_stmt);
  STMT_INIT_EXPL(drop_index_stmt);
  STMT_INIT_EXPL(begin_trans_stmt);
  STMT_INIT_EXPL(commit_trans_stmt);

  CHARBUF_OPEN(query_plans_buf);
  query_plans = &query_plans_buf;
  CHARBUF_OPEN(schema_stmts_buf);
  schema_stmts = &schema_stmts_buf;
  CHARBUF_OPEN(output_buf);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.variables_callback = &variables_callback;
  cg_qp_callbacks = &callbacks;

  cg_qp_stmt_list(head);

  if (sql_stmt_count) {
    bprintf(&output_buf, rt->source_prefix);
    if (options.test) {
      while (head->right) {
        head = head->right;
      }
      bprintf(&output_buf, "-- The statement ending at line %d\n\n", head->left->lineno);
    }
    cg_qp_emit_declare_func(&output_buf);
    bprintf(&output_buf, "\n");
    cg_qp_emit_create_schema_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_populate_no_table_scan_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_populate_tables_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_populate_alert_table_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_print_table_scan_violation_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_print_sql_statement_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_print_query_plan_stat_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_print_query_plan_graph_proc(&output_buf);
    bprintf(&output_buf, "\n");
    emit_print_query_plan(&output_buf);
    bprintf(&output_buf, "\n");
  }

  // create an empty query_plan method even if there are no statements
  // (the helpers above won't be needed)
  bprintf(&output_buf, "CREATE PROC query_plan()\n");
  bprintf(&output_buf, "BEGIN\n");
  if (sql_stmt_count) {
    bprintf(&output_buf, "  CALL create_schema();\n");
    bprintf(&output_buf, "  BEGIN TRY\n");
    bprintf(&output_buf, "    CALL populate_no_table_scan();\n");
    bprintf(&output_buf, "  END TRY;\n");
    bprintf(&output_buf, "  BEGIN CATCH\n");
    bprintf(&output_buf, "    CALL printf(\"failed populating no_table_scan table\\n\");\n");
    bprintf(&output_buf, "    THROW;\n");
    bprintf(&output_buf, "  END CATCH;\n");
  }
  for (int32_t i = 1; i <= sql_stmt_count; i++) {
    bprintf(&output_buf, "  BEGIN TRY\n");
    bprintf(&output_buf, "    CALL populate_query_plan_%d();\n", i);
    bprintf(&output_buf, "  END TRY;\n");
    bprintf(&output_buf, "  BEGIN CATCH\n");
    bprintf(&output_buf, "    CALL printf(\"failed populating query %d\\n\");\n", i);
    bprintf(&output_buf, "    THROW;\n");
    bprintf(&output_buf, "  END CATCH;\n");
  }

  bprintf(&output_buf, "  CALL printf(\"[\\n\");\n");

  if (sql_stmt_count) {
    bprintf(&output_buf, "  CALL print_table_scan_violation();\n");
  }

  bprintf(&output_buf, "  CALL printf(\"[\\n\");\n");
  bprintf(&output_buf, "  CALL printf(\"[\\n\");\n");
  bprintf(&output_buf, "  CALL printf(\"[\\\"Query\\\", \\\"Stat\\\", \\\"Graph\\\"],\\n\");\n");
  for (int32_t i = 1; i <= sql_stmt_count; i++) {
    bprintf(&output_buf, "  CALL print_query_plan(%d);\n", i);
  }
  bprintf(&output_buf, "  CALL printf(\"[]\\n\");\n");
  bprintf(&output_buf, "  CALL printf(\"]\\n\");\n");
  bprintf(&output_buf, "  CALL printf(\"],\\n\");\n");

  bprintf(&output_buf, "  CALL printf(\"[]\\n\");\n");
  bprintf(&output_buf, "  CALL printf(\"]\");\n");

  bprintf(&output_buf, "END;\n");

  cql_write_file(options.file_names[0], output_buf.ptr);

  CHARBUF_CLOSE(output_buf);
  CHARBUF_CLOSE(schema_stmts_buf);
  CHARBUF_CLOSE(query_plans_buf);

  // Force the globals to null state so that they do not look like roots to LeakSanitizer
  // all of these should have been freed already.  This is the final safety net to prevent
  // non-reporting of leaks.

  schema_stmts = NULL;
  query_plans = NULL;
  cg_qp_callbacks = NULL;
}

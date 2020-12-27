/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// In case there is any doubt, 'cql.y' is included in the license as well as
// the code bision generates from it.

// cql - pronounced "see-cue-el" is a basic tool for enabling stored
//      procedures for SQLite. The tool does this by parsing specialized
//      .sql files:
//
//      - loose DDL (not in a proc) in the .sql is used to declare tables and views
//        has no other effect
//      - SQL DML and DDL logic is converted to the equivalent sqlite calls to do the work
//      - loose DML and loose control flow is consolidated into a global proc you can name
//        with the --global_proc command line switch
//      - control flow is converted to C control flow
//      - stored procs map into C functions directly, stored procs with a result set
//        become a series of procs for creating, accessing, and destroying the result set
//      - all sqlite code gen has full error checking and participates in SQL try/catch
//        and throw patterns
//      - strings and result sets can be mapped into assorted native objects by
//        defining the items in cqlrt.h
//      - everything is strongly typed, and type checked, using the primitive types:
//        bool, int, long int, real, and text
//
// Design principles:
//
//  1. Keep each pass in one file (simple, focused, and easy refactor)
//  2. Use simple printable AST parse nodes (no separate #define per AST node type)
//  3. 100% unit test coverage on all passes including output validation

%{

#include <inttypes.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include "cql.h"
#include "charbuf.h"

#include "ast.h"
#include "cg_common.h"
#include "cg_c.h"
#include "cg_java.h"
#include "cg_schema.h"
#include "cg_json_schema.h"
#include "cg_test_helpers.h"
#include "cg_query_plan.h"
#include "cg_udf.h"
#include "cg_objc.h"
#include "gen_sql.h"
#include "sem.h"
#include "encoders.h"
#include "unit_tests.h"
#include "rt.h"

// The stack needed is modest (32k) and this prevents leaks in error cases because
// it's just a stack alloc.
#define YYSTACK_USE_ALLOCA 1

static void parse_cmd(int argc, char **argv);
static void print_dot(struct ast_node* node);
static ast_node *file_literal(ast_node *);

int yylex();
void yyerror(const char *s, ...);
void yyset_in(FILE *);
void yyset_lineno(int);

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wimplicit-int-conversion"
#pragma clang diagnostic ignored "-Wconversion"

// In two places in the grammar we have to include an optional name list
// even though the name list isn't actually allowed.  We do this to avoid
// a shift reduce conflict.  We can't avoid the conflict case without a lot
// of very ugly grammar duplication. So this is the lesser of two evils
// and definitely more maintainable.

#define YY_ERROR_ON_COLUMNS(x) \
  if (x) yyerror("\nCursor columns not allowed in this form.\n")

#ifdef CQL_AMALGAM
static void cql_reset_globals(void);
#endif

#define AST_STR(node) (((str_ast_node *)node)->value)

%}

%define parse.error verbose

%union {
  struct ast_node *aval;
  int ival;
  char *sval;
}

%token <sval> ID
%token <sval> STRLIT CSTRLIT BLOBLIT
%token <sval> INTLIT
%token <ival> BOOL_
%token <ival> AT_DUMMY_NULLABLES
%token <ival> AT_DUMMY_DEFAULTS
%token <sval> LONGLIT
%token <sval> REALLIT

/*
 SQLite understands the following binary operators, in order from LOWEST to HIGHEST precedence:

 OR
 AND
 =    ==   !=   <>   IS   IS NOT   IN   NOT IN   LIKE   NOT LIKE   GLOB   MATCH   REGEXP
 <    <=   >    >=
 <<   >>   &    |
 +    -
 *    /    %
 ||
*/


// NOTE the precedence declared here in the grammar MUST agree with the precedence
// declared in ast.h EXPR_PRI_XXX or else badness ensues.

%left UNION_ALL UNION INTERSECT EXCEPT
%right ASSIGN
%left OR
%left AND
%left BETWEEN
%left NOT
%left NE NE_ '=' EQEQ LIKE NOT_LIKE GLOB MATCH REGEXP IN IS_NOT IS
%left '<' '>' GE LE
%left LS RS '&' '|'
%left '+' '-'
%left '*' '/' '%'
%nonassoc UMINUS '~' COLLATE
%left CONCAT

%token EXCLUDE_GROUP EXCLUDE_CURRENT_ROW EXCLUDE_TIES EXCLUDE_NO_OTHERS CURRENT_ROW UNBOUNDED PRECEDING FOLLOWING
%token CREATE DROP TABLE WITHOUT ROWID PRIMARY KEY NULL_ DEFAULT CHECK AT_DUMMY_SEED VIRTUAL AT_EMIT_ENUMS
%token OBJECT TEXT BLOB LONG_ INT_ INTEGER LONG_INTEGER REAL ON UPDATE CASCADE ON_CONFLICT DO NOTHING
%token DELETE INDEX FOREIGN REFERENCES CONSTRAINT UPSERT STATEMENT CONST
%token INSERT INTO VALUES VIEW SELECT QUERY_PLAN EXPLAIN OVER WINDOW FILTER PARTITION RANGE ROWS GROUPS
%token AS CASE WHEN FROM THEN ELSE END LEFT
%token OUTER JOIN WHERE GROUP BY ORDER ASC
%token DESC INNER FCOUNT AUTOINCREMENT DISTINCT
%token LIMIT OFFSET TEMP TRIGGER IF ALL CROSS USING RIGHT
%token UNIQUE HAVING SET TO DISTINCTROW ENUM
%token FUNC FUNCTION PROC PROCEDURE BEGIN_ OUT INOUT CURSOR DECLARE FETCH LOOP LEAVE CONTINUE FOR
%token OPEN CLOSE ELSE_IF WHILE CALL TRY CATCH THROW RETURN
%token SAVEPOINT ROLLBACK COMMIT TRANSACTION RELEASE ARGUMENTS
%token CAST WITH RECURSIVE REPLACE IGNORE ADD COLUMN RENAME ALTER
%token AT_ECHO AT_CREATE AT_RECREATE AT_DELETE AT_SCHEMA_UPGRADE_VERSION AT_PREVIOUS_SCHEMA AT_SCHEMA_UPGRADE_SCRIPT
%token AT_PROC AT_FILE AT_ATTRIBUTE AT_SENSITIVE DEFERRED NOT_DEFERRABLE DEFERRABLE IMMEDIATE RESTRICT ACTION INITIALLY NO
%token BEFORE AFTER INSTEAD OF FOR_EACH_ROW EXISTS RAISE FAIL ABORT AT_ENFORCE_STRICT AT_ENFORCE_NORMAL
%token AT_BEGIN_SCHEMA_REGION AT_END_SCHEMA_REGION
%token AT_DECLARE_SCHEMA_REGION AT_DECLARE_DEPLOYABLE_REGION AT_SCHEMA_AD_HOC_MIGRATION PRIVATE

/* ddl stuff */
%type <ival> opt_temp opt_if_not_exists opt_unique opt_no_rowid dummy_modifier compound_operator opt_query_plan
%type <ival> opt_fk_options fk_options fk_on_options fk_action fk_initial_state fk_deferred_options
%type <ival> frame_type frame_exclude join_type

%type <aval> col_key_list col_key_def col_def col_name
%type <aval> version_attrs opt_version_attrs version_attrs_opt_recreate opt_delete_version_attr
%type <aval> misc_attr_key misc_attr misc_attrs misc_attr_value misc_attr_value_list
%type <aval> col_attrs str_literal num_literal any_literal const_expr
%type <aval> pk_def fk_def unq_def check_def fk_target_options opt_module_args

%type <aval> alter_table_add_column_stmt
%type <aval> create_index_stmt create_table_stmt create_view_stmt create_virtual_table_stmt
%type <aval> indexed_column indexed_columns
%type <aval> drop_index_stmt drop_table_stmt drop_view_stmt drop_trigger_stmt

%type <aval> trigger_update_stmt trigger_delete_stmt trigger_insert_stmt trigger_select_stmt
%type <aval> trigger_stmt trigger_stmts opt_when_expr trigger_action opt_of
%type <aval> trigger_def trigger_operation create_trigger_stmt raise_expr
%type <ival> trigger_condition opt_foreachrow

/* dml stuff */
%type <aval> with_delete_stmt delete_stmt
%type <aval> insert_stmt with_insert_stmt insert_list insert_stmt_type opt_column_spec opt_insert_dummy_spec expr_names expr_name
%type <aval> with_prefix with_select_stmt cte_table cte_tables
%type <aval> select_expr select_expr_list select_opts select_stmt select_core values explain_stmt explain_target
%type <aval> select_stmt_no_with select_core_list
%type <aval> window_func_inv opt_filter_clause window_name_or_defn window_defn opt_select_window
%type <aval> opt_partition_by opt_frame_spec frame_boundary_opts frame_boundary_start frame_boundary_end frame_boundary
%type <aval> opt_where opt_groupby opt_having opt_orderby opt_limit opt_offset opt_as_alias as_alias window_clause
%type <aval> groupby_item groupby_list opt_asc_desc window_name_defn window_name_defn_list
%type <aval> table_or_subquery table_or_subquery_list query_parts table_function opt_from_query_parts
%type <aval> opt_join_cond join_cond join_clause join_target join_target_list
%type <aval> basic_update_stmt with_update_stmt update_stmt update_cursor_stmt update_entry update_list upsert_stmt conflict_target
%type <aval> declare_schema_region_stmt declare_deployable_region_stmt call with_upsert_stmt
%type <aval> begin_schema_region_stmt end_schema_region_stmt schema_ad_hoc_migration_stmt region_list region_spec

/* expressions and types */
%type <aval> expr basic_expr math_expr expr_list typed_name typed_names case_list call_expr_list call_expr shape_arguments
%type <aval> name name_list opt_name_list opt_name
%type <aval> data_type data_type_numeric data_type_opt_notnull creation_type object_type

/* proc stuff */
%type <aval> create_proc_stmt declare_func_stmt declare_proc_stmt
%type <aval> arg_expr arg_list inout param params

/* statements */
%type <aval> stmt
%type <aval> stmt_list opt_stmt_list
%type <aval> any_stmt
%type <aval> begin_trans_stmt
%type <aval> call_stmt
%type <aval> close_stmt
%type <aval> commit_trans_stmt commit_return_stmt
%type <aval> continue_stmt
%type <aval> declare_stmt
%type <aval> declare_enum_stmt enum_values enum_value emit_enums_stmt
%type <aval> echo_stmt
%type <aval> fetch_stmt fetch_values_stmt fetch_call_stmt from_shape
%type <aval> if_stmt elseif_item elseif_list opt_else opt_elseif_list proc_savepoint_stmt
%type <aval> leave_stmt return_stmt
%type <aval> loop_stmt
%type <aval> open_stmt
%type <aval> out_stmt out_union_stmt
%type <aval> previous_schema_stmt
%type <aval> release_savepoint_stmt
%type <aval> rollback_trans_stmt rollback_return_stmt
%type <aval> savepoint_stmt
%type <aval> schema_upgrade_script_stmt
%type <aval> schema_upgrade_version_stmt
%type <aval> set_stmt
%type <aval> throw_stmt
%type <aval> trycatch_stmt
%type <aval> version_annotation
%type <aval> while_stmt
%type <aval> enforce_strict_stmt enforce_normal_stmt enforcement_options shape_def

%start program

%%

program:
  opt_stmt_list  {
    gen_init();
    if (options.semantic) {
      sem_main($opt_stmt_list);
    }
    if (options.codegen) {
      rt->code_generator($opt_stmt_list);
    }
    else if (options.print_ast) {
      print_ast($opt_stmt_list, NULL, 0, 0);
      cql_output("\n");
    } else if (options.print_dot) {
      cql_output("\ndigraph parse {");
      print_dot($opt_stmt_list);
      cql_output("\n}\n");
    }
    else {
      gen_stmt_list_to_stdout($opt_stmt_list);
    }
    if (options.semantic) {
      cql_exit_on_semantic_errors($opt_stmt_list);
    }
  }
  ;

opt_stmt_list:
  /*nil*/  { $opt_stmt_list = NULL; }
  | stmt_list  { $opt_stmt_list = $stmt_list; }
  ;

stmt_list[result]:
  stmt ';'  { $result = new_ast_stmt_list($stmt, NULL); $result->lineno = $stmt->lineno;}
  | stmt ';' stmt_list[slist]  { $result = new_ast_stmt_list($stmt, $slist); $result->lineno = $stmt->lineno; }
  ;

stmt:
  misc_attrs any_stmt  { $stmt = $misc_attrs ? new_ast_stmt_and_attr($misc_attrs, $any_stmt) : $any_stmt; }
  ;

any_stmt: select_stmt
  | explain_stmt
  | create_trigger_stmt
  | create_table_stmt
  | create_virtual_table_stmt
  | create_index_stmt
  | create_view_stmt
  | alter_table_add_column_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | with_delete_stmt
  | delete_stmt
  | call_stmt
  | with_insert_stmt
  | insert_stmt
  | with_update_stmt
  | update_stmt
  | update_cursor_stmt
  | upsert_stmt
  | with_upsert_stmt
  | set_stmt
  | create_proc_stmt
  | declare_proc_stmt
  | declare_func_stmt
  | declare_stmt
  | declare_enum_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_call_stmt
  | while_stmt
  | loop_stmt
  | leave_stmt
  | return_stmt
  | rollback_return_stmt
  | commit_return_stmt
  | continue_stmt
  | if_stmt
  | open_stmt
  | close_stmt
  | out_stmt
  | out_union_stmt
  | throw_stmt
  | trycatch_stmt
  | begin_trans_stmt
  | rollback_trans_stmt
  | commit_trans_stmt
  | proc_savepoint_stmt
  | savepoint_stmt
  | release_savepoint_stmt
  | echo_stmt
  | schema_upgrade_version_stmt
  | schema_upgrade_script_stmt
  | previous_schema_stmt
  | enforce_strict_stmt
  | enforce_normal_stmt
  | declare_schema_region_stmt
  | declare_deployable_region_stmt
  | begin_schema_region_stmt
  | end_schema_region_stmt
  | schema_ad_hoc_migration_stmt
  | emit_enums_stmt
  ;

explain_stmt:
  EXPLAIN opt_query_plan explain_target  { $explain_stmt = new_ast_explain_stmt(new_ast_opt($opt_query_plan), $explain_target); }
  ;

opt_query_plan:
  /* nil */  { $opt_query_plan = EXPLAIN_NONE; }
  | QUERY_PLAN  { $opt_query_plan = EXPLAIN_QUERY_PLAN; }
  ;

explain_target: select_stmt
  | update_stmt
  | delete_stmt
  | with_delete_stmt
  | with_insert_stmt
  | insert_stmt
  | upsert_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | begin_trans_stmt
  | commit_trans_stmt
  ;

previous_schema_stmt:
  AT_PREVIOUS_SCHEMA  { $previous_schema_stmt = new_ast_previous_schema_stmt(); }
  ;

schema_upgrade_script_stmt:
  AT_SCHEMA_UPGRADE_SCRIPT  { $schema_upgrade_script_stmt = new_ast_schema_upgrade_script_stmt(); }
  ;

schema_upgrade_version_stmt:
  AT_SCHEMA_UPGRADE_VERSION '(' INTLIT ')'  {
    $schema_upgrade_version_stmt = new_ast_schema_upgrade_version_stmt(new_ast_opt(atoi($INTLIT))); }
  ;

set_stmt:
  SET name ASSIGN expr  { $set_stmt = new_ast_assign($name, $expr); }
  | SET name[id] FROM CURSOR name[cursor] { $set_stmt = new_ast_set_from_cursor($id, $cursor); }
  ;

version_attrs_opt_recreate:
  /* nil */  { $version_attrs_opt_recreate = NULL; }
  | AT_RECREATE  { $version_attrs_opt_recreate = new_ast_recreate_attr(NULL); }
  | AT_RECREATE '(' name ')'  { $version_attrs_opt_recreate = new_ast_recreate_attr($name); }
  | version_attrs  { $version_attrs_opt_recreate = $version_attrs; }
  ;

opt_version_attrs:
  /* nil */  { $opt_version_attrs = NULL; }
  | version_attrs  { $opt_version_attrs = $version_attrs; }
  ;

version_attrs:
  AT_CREATE version_annotation opt_version_attrs  { $version_attrs = new_ast_create_attr($version_annotation, $opt_version_attrs); }
  | AT_DELETE version_annotation opt_version_attrs  { $version_attrs = new_ast_delete_attr($version_annotation, $opt_version_attrs); }
  ;

opt_delete_version_attr:
  /* nil */  {$opt_delete_version_attr = NULL; }
  | AT_DELETE version_annotation  { $opt_delete_version_attr = new_ast_delete_attr($version_annotation, NULL); }
  ;

drop_table_stmt:
  DROP TABLE IF EXISTS name  { $drop_table_stmt = new_ast_drop_table_stmt(new_ast_opt(1), $name);  }
  | DROP TABLE name  { $drop_table_stmt = new_ast_drop_table_stmt(NULL, $name);  }
  ;

drop_view_stmt:
  DROP VIEW IF EXISTS name  { $drop_view_stmt = new_ast_drop_view_stmt(new_ast_opt(1), $name);  }
  | DROP VIEW name  { $drop_view_stmt = new_ast_drop_view_stmt(NULL, $name);  }
  ;

drop_index_stmt:
  DROP INDEX IF EXISTS name  { $drop_index_stmt = new_ast_drop_index_stmt(new_ast_opt(1), $name);  }
  | DROP INDEX name  { $drop_index_stmt = new_ast_drop_index_stmt(NULL, $name);  }
  ;

drop_trigger_stmt:
  DROP TRIGGER IF EXISTS name  { $drop_trigger_stmt = new_ast_drop_trigger_stmt(new_ast_opt(1), $name);  }
  | DROP TRIGGER name  { $drop_trigger_stmt = new_ast_drop_trigger_stmt(NULL, $name);  }
  ;

create_virtual_table_stmt: CREATE VIRTUAL TABLE opt_if_not_exists name[table_name]
                           USING name[module_name] opt_module_args
                           AS '(' col_key_list ')' opt_delete_version_attr {
    int flags = $opt_if_not_exists;
    struct ast_node *flags_node = new_ast_opt(flags);
    struct ast_node *name = $table_name;
    struct ast_node *col_key_list = $col_key_list;
    struct ast_node *version_info = $opt_delete_version_attr ? $opt_delete_version_attr : new_ast_recreate_attr(NULL);
    struct ast_node *table_flags_attrs = new_ast_table_flags_attrs(flags_node, version_info);
    struct ast_node *table_name_flags = new_ast_create_table_name_flags(table_flags_attrs, name);
    struct ast_node *create_table_stmt =  new_ast_create_table_stmt(table_name_flags, col_key_list);
    struct ast_node *module_info = new_ast_module_info($module_name, $opt_module_args);
    $create_virtual_table_stmt = new_ast_create_virtual_table_stmt(module_info, create_table_stmt);
  };

opt_module_args: /* nil */ { $opt_module_args = NULL; }
  | '(' misc_attr_value_list ')' { $opt_module_args = $misc_attr_value_list; }
  | '(' ARGUMENTS FOLLOWING ')' { $opt_module_args = new_ast_following(); }
  ;

create_table_stmt:
  CREATE opt_temp TABLE opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate  {
    int flags = $opt_temp | $opt_if_not_exists | $opt_no_rowid;
    struct ast_node *flags_node = new_ast_opt(flags);
    struct ast_node *name = $name;
    struct ast_node *col_key_list = $col_key_list;
    struct ast_node *table_flags_attrs = new_ast_table_flags_attrs(flags_node, $version_attrs_opt_recreate);
    struct ast_node *table_name_flags = new_ast_create_table_name_flags(table_flags_attrs, name);
    $create_table_stmt =  new_ast_create_table_stmt(table_name_flags, col_key_list);
  }
  ;

opt_temp:
  /* nil */  { $opt_temp = 0; }
  | TEMP  { $opt_temp = GENERIC_IS_TEMP; }
  ;

opt_if_not_exists:
  /* nil */  { $opt_if_not_exists = 0;  }
  | IF NOT EXISTS  { $opt_if_not_exists = GENERIC_IF_NOT_EXISTS; }
  ;

opt_no_rowid:
  /* nil */  { $opt_no_rowid = 0; }
  | WITHOUT ROWID  { $opt_no_rowid = TABLE_IS_NO_ROWID; }
  ;

col_key_list[result]:
  col_key_def  { $result = new_ast_col_key_list($col_key_def, NULL); }
  | col_key_def ',' col_key_list[ckl]  { $result = new_ast_col_key_list($col_key_def, $ckl); }
  ;

col_key_def:
  col_def
  | pk_def
  | fk_def
  | unq_def
  | check_def
  | shape_def
  ;

check_def:
  CONSTRAINT name CHECK '(' expr ')' { $check_def = new_ast_check_def($name, $expr); }
  | CHECK '(' expr ')'  { $check_def = new_ast_check_def(NULL, $expr); }
  ;

shape_def:
    LIKE name { $shape_def = new_ast_like($name, NULL); }
  | LIKE name ARGUMENTS { $shape_def = new_ast_like($name, $name); }
  ;

col_name:
  name  { $col_name = $name; }
  ;

misc_attr_key:
  name  { $misc_attr_key = $name; }
  | name[lhs] ':' name[rhs]  { $misc_attr_key = new_ast_dot($lhs, $rhs); }
  ;

misc_attr_value_list[result]:
  misc_attr_value  { $result = new_ast_misc_attr_value_list($misc_attr_value, NULL); }
  | misc_attr_value ',' misc_attr_value_list[mav]  { $result = new_ast_misc_attr_value_list($misc_attr_value, $mav); }
  ;

misc_attr_value:
  name  { $misc_attr_value = $name; }
  | any_literal  { $misc_attr_value = $any_literal; }
  | const_expr  { $misc_attr_value = $const_expr; }
  | '(' misc_attr_value_list ')'  { $misc_attr_value = $misc_attr_value_list; }
  | '-' num_literal  { $misc_attr_value = new_ast_uminus($num_literal);}
  ;

misc_attr:
  AT_ATTRIBUTE '(' misc_attr_key ')'  { $misc_attr = new_ast_misc_attr($misc_attr_key, NULL); }
  | AT_ATTRIBUTE '(' misc_attr_key '=' misc_attr_value ')'  { $misc_attr = new_ast_misc_attr($misc_attr_key, $misc_attr_value); }
  ;

misc_attrs[result]:
  /* nil */  { $result = NULL; }
  | misc_attr misc_attrs[ma]  { $result = new_ast_misc_attrs($misc_attr, $ma); }
  ;

col_def:
  misc_attrs col_name data_type col_attrs  {
  struct ast_node *misc_attrs = $misc_attrs;
  struct ast_node *col_name = $col_name;
  struct ast_node *data_type = $data_type;
  struct ast_node *col_attrs= $col_attrs;
  struct ast_node *name_type = new_ast_col_def_name_type(col_name, data_type);
  struct ast_node *col_def_type_attrs = new_ast_col_def_type_attrs(name_type, col_attrs);
  $col_def = new_ast_col_def(col_def_type_attrs, misc_attrs);
  }
  ;

pk_def:
  PRIMARY KEY '(' name_list ')'  { $pk_def = new_ast_pk_def($name_list);}
  ;

opt_fk_options:
  /* nil */  { $opt_fk_options = 0; }
  | fk_options  { $opt_fk_options = $fk_options; }
  ;

fk_options:
  fk_on_options  { $fk_options = $fk_on_options; }
  | fk_deferred_options  { $fk_options = $fk_deferred_options; }
  | fk_on_options fk_deferred_options  { $fk_options = $fk_on_options | $fk_deferred_options; }
  ;

fk_on_options:
  ON DELETE fk_action  { $fk_on_options = $fk_action; }
  | ON UPDATE fk_action  { $fk_on_options = ($fk_action << 4); }
  | ON UPDATE fk_action[lhs] ON DELETE fk_action[rhs]  { $fk_on_options = ($lhs << 4) | $rhs; }
  | ON DELETE fk_action[lhs] ON UPDATE fk_action[rhs]  { $fk_on_options = ($rhs << 4) | $lhs; }
  ;

fk_action:
  SET NULL_  { $fk_action = FK_SET_NULL; }
  | SET DEFAULT  { $fk_action = FK_SET_DEFAULT; }
  | CASCADE  { $fk_action = FK_CASCADE; }
  | RESTRICT  { $fk_action = FK_RESTRICT; }
  | NO ACTION  { $fk_action = FK_NO_ACTION; }
  ;

fk_deferred_options:
  DEFERRABLE fk_initial_state  { $fk_deferred_options = FK_DEFERRABLE | $fk_initial_state; }
  | NOT_DEFERRABLE fk_initial_state  { $fk_deferred_options = FK_NOT_DEFERRABLE | $fk_initial_state; }
  ;

fk_initial_state:
  /* nil */  { $fk_initial_state = 0; }
  | INITIALLY DEFERRED  { $fk_initial_state = FK_INITIALLY_DEFERRED; }
  | INITIALLY IMMEDIATE  { $fk_initial_state = FK_INITIALLY_IMMEDIATE; }
  ;

fk_def:
  FOREIGN KEY '(' name_list ')' fk_target_options  { $fk_def = new_ast_fk_def($name_list, $fk_target_options); }
  ;

fk_target_options:
  REFERENCES name '(' name_list ')' opt_fk_options  {
    $fk_target_options = new_ast_fk_target_options(new_ast_fk_target($name, $name_list), new_ast_opt($opt_fk_options)); }
  ;

unq_def:
  CONSTRAINT name UNIQUE '(' name_list ')'  { $unq_def = new_ast_unq_def($name, $name_list); }
  | UNIQUE '(' name_list ')'  { $unq_def = new_ast_unq_def(NULL, $name_list); }
  ;

opt_unique:
  /* nil */  { $opt_unique = 0; }
  | UNIQUE  { $opt_unique = 1; }
  ;

indexed_column:
  name opt_asc_desc  { $indexed_column = new_ast_indexed_column($name, $opt_asc_desc); }
  ;

indexed_columns[result]:
  indexed_column  { $result = new_ast_indexed_columns($indexed_column, NULL); }
  | indexed_column ',' indexed_columns[ic]  { $result = new_ast_indexed_columns($indexed_column, $ic); }
  ;

create_index_stmt:
  CREATE opt_unique INDEX opt_if_not_exists name[tbl_name] ON name[idx_name] '(' indexed_columns ')' opt_delete_version_attr  {
    int flags = 0;
    if ($opt_unique) flags |= INDEX_UNIQUE;
    if ($opt_if_not_exists) flags |= INDEX_IFNE;

    ast_node *create_index_on_list = new_ast_create_index_on_list($tbl_name, $idx_name);
    ast_node *index_names_and_attrs = new_ast_index_names_and_attrs($indexed_columns, $opt_delete_version_attr);
    ast_node *flags_names_attrs = new_ast_flags_names_attrs(new_ast_opt(flags), index_names_and_attrs);
    $create_index_stmt = new_ast_create_index_stmt(create_index_on_list, flags_names_attrs);
  }
  ;

name:
  ID  { $name = new_ast_str($ID); }
  | TEXT  { $name = new_ast_str("text"); }
  | TRIGGER  { $name = new_ast_str("trigger"); }
  | ROWID  { $name = new_ast_str("rowid"); }
  | KEY  { $name = new_ast_str("key"); }
  | VIRTUAL  { $name = new_ast_str("virtual"); }
  ;

opt_name:
  /* nil */  { $opt_name = NULL; }
  | name  { $opt_name = $name; }
  ;

name_list[result]:
  name  { $result = new_ast_name_list($name, NULL); }
  |  name ',' name_list[nl]  { $result = new_ast_name_list($name, $nl); }
  ;

opt_name_list:
  /* nil */  { $opt_name_list = NULL; }
  | name_list  {$opt_name_list = $name_list; }
  ;

col_attrs[result]:
  /* nil */  { $result = NULL; }
  | NOT NULL_ col_attrs[ca]  { $result = new_ast_col_attrs_not_null(NULL, $ca); }
  | PRIMARY KEY col_attrs[ca]  { $result = new_ast_col_attrs_pk(NULL, $ca); }
  | PRIMARY KEY AUTOINCREMENT col_attrs[ca]  { $result = new_ast_col_attrs_pk(new_ast_col_attrs_autoinc(), $ca);}
  | DEFAULT '-' num_literal col_attrs[ca]  { $result = new_ast_col_attrs_default(new_ast_uminus($num_literal), $ca);}
  | DEFAULT num_literal col_attrs[ca]  { $result = new_ast_col_attrs_default($num_literal, $ca);}
  | DEFAULT const_expr col_attrs[ca]  { $result = new_ast_col_attrs_default($const_expr, $ca);}
  | DEFAULT str_literal col_attrs[ca]  { $result = new_ast_col_attrs_default($str_literal, $ca);}
  | COLLATE name col_attrs[ca]  { $result = new_ast_col_attrs_collate($name, $ca);}
  | CHECK '(' expr ')' col_attrs[ca]  { $result = new_ast_col_attrs_check($expr, $ca);}
  | UNIQUE col_attrs[ca]  { $result = new_ast_col_attrs_unique(NULL, $ca);}
  | AT_SENSITIVE col_attrs[ca]  { $result = new_ast_sensitive_attr(NULL, $ca); }
  | AT_CREATE version_annotation col_attrs[ca]  { $result = new_ast_create_attr($version_annotation, $ca);}
  | AT_DELETE version_annotation col_attrs[ca]  { $result = new_ast_delete_attr($version_annotation, $ca);}
  | fk_target_options col_attrs[ca]  { $result = new_ast_col_attrs_fk($fk_target_options, $ca); }
  ;

version_annotation:
  '(' INTLIT ',' name ')'  {
    $version_annotation = new_ast_version_annotation(new_ast_opt(atoi($INTLIT)), $name); }
  | '(' INTLIT ')'  {
    $version_annotation = new_ast_version_annotation(new_ast_opt(atoi($INTLIT)), NULL); }
  ;

object_type:
  OBJECT  { $object_type = new_ast_type_object(NULL); }
  | OBJECT '<' name '>'  { $object_type = new_ast_type_object($name); }
  | OBJECT '<' name CURSOR '>' {
    CSTR type = dup_printf("%s CURSOR", AST_STR($name));
    $object_type = new_ast_type_object(new_ast_str(type)); }
  ;

data_type_numeric:
  INT_  { $data_type_numeric = new_ast_type_int(); }
  | INTEGER  { $data_type_numeric = new_ast_type_int(); }
  | REAL  { $data_type_numeric = new_ast_type_real(); }
  | LONG_  { $data_type_numeric = new_ast_type_long(); }
  | BOOL_  { $data_type_numeric = new_ast_type_bool(); }
  | LONG_ INTEGER  { $data_type_numeric = new_ast_type_long(); }
  | LONG_ INT_  { $data_type_numeric = new_ast_type_long(); }
  | LONG_INTEGER  { $data_type_numeric = new_ast_type_long(); }

data_type:
  data_type_numeric { $data_type = $data_type_numeric; }
  | TEXT  { $data_type = new_ast_type_text();  }
  | BLOB  { $data_type = new_ast_type_blob(); }
  | object_type  { $data_type = $object_type; }
  ;

data_type_opt_notnull:
  data_type  { $data_type_opt_notnull = $data_type; }
  | data_type NOT NULL_  { $data_type_opt_notnull = new_ast_notnull($data_type); }
  | data_type AT_SENSITIVE  { $data_type_opt_notnull = new_ast_sensitive_attr($data_type, NULL); }
  | data_type AT_SENSITIVE NOT NULL_  { $data_type_opt_notnull = new_ast_sensitive_attr(new_ast_notnull($data_type), NULL); }
  | data_type NOT NULL_ AT_SENSITIVE  { $data_type_opt_notnull = new_ast_sensitive_attr(new_ast_notnull($data_type), NULL); }
  ;

str_literal:
  STRLIT  { $str_literal = new_ast_str($STRLIT);}
  | CSTRLIT  { $str_literal = new_ast_cstr($CSTRLIT); }
  ;

num_literal:
  INTLIT  { $num_literal = new_ast_num(NUM_INT, $INTLIT); }
  | LONGLIT  { $num_literal = new_ast_num(NUM_LONG, $LONGLIT); }
  | REALLIT  { $num_literal = new_ast_num(NUM_REAL, $REALLIT); }
  ;

const_expr:
  CONST '(' expr ')' { $const_expr = new_ast_const($expr); }
  ;

any_literal:
  str_literal  { $any_literal = $str_literal; }
  | num_literal  { $any_literal = $num_literal; }
  | NULL_  { $any_literal = new_ast_null(); }
  | AT_FILE '(' str_literal ')'  { $any_literal = file_literal($str_literal); }
  | AT_PROC  { $any_literal = new_ast_str("@PROC"); }
  | BLOBLIT  { $any_literal = new_astb($BLOBLIT); }
  ;

raise_expr:
  RAISE '(' IGNORE ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_IGNORE), NULL); }
  | RAISE '(' ROLLBACK ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_ROLLBACK), $expr); }
  | RAISE '(' ABORT ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_ABORT), $expr); }
  | RAISE '(' FAIL ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_FAIL), $expr); }
  ;

call:
  name '(' arg_list ')' opt_filter_clause  {
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, $opt_filter_clause);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $arg_list);
      $call = new_ast_call($name, call_arg_list);
  }
  | name '(' DISTINCT arg_list ')' opt_filter_clause  {
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(new_ast_distinct(), $opt_filter_clause);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $arg_list);
      $call = new_ast_call($name, call_arg_list);
  }
  ;

basic_expr:
  name  { $basic_expr = $name; }
  | name[lhs] '.' name[rhs]  { $basic_expr = new_ast_dot($lhs, $rhs); }
  | any_literal  { $basic_expr = $any_literal; }
  | const_expr { $basic_expr = $const_expr; }
  | '(' expr ')'  { $basic_expr = $expr; }
  | call  { $basic_expr = $call; }
  | window_func_inv  { $basic_expr = $window_func_inv; }
  | raise_expr  { $basic_expr = $raise_expr; }
  | '(' select_stmt ')'  { $basic_expr = $select_stmt; }
  | EXISTS '(' select_stmt ')'  { $basic_expr = new_ast_exists_expr($select_stmt); }
  ;

math_expr[result]:
  basic_expr  { $result = $basic_expr; }
  | math_expr[lhs] '&' math_expr[rhs]  { $result = new_ast_bin_and($lhs, $rhs); }
  | math_expr[lhs] '|' math_expr[rhs]  { $result = new_ast_bin_or($lhs, $rhs); }
  | math_expr[lhs] LS math_expr[rhs]  { $result = new_ast_lshift($lhs, $rhs); }
  | math_expr[lhs] RS  math_expr[rhs]  { $result = new_ast_rshift($lhs, $rhs); }
  | math_expr[lhs] '+' math_expr[rhs]  { $result = new_ast_add($lhs, $rhs); }
  | math_expr[lhs] '-' math_expr[rhs]  { $result = new_ast_sub($lhs, $rhs); }
  | math_expr[lhs] '*' math_expr[rhs]  { $result = new_ast_mul($lhs, $rhs); }
  | math_expr[lhs] '/' math_expr[rhs]  { $result = new_ast_div($lhs, $rhs); }
  | math_expr[lhs] '%' math_expr[rhs]  { $result = new_ast_mod($lhs, $rhs); }
  | '-' math_expr[rhs] %prec UMINUS  { $result = new_ast_uminus($rhs); }
  | math_expr[lhs] CONCAT math_expr[rhs]  { $result = new_ast_concat($lhs, $rhs); }
  ;

expr[result]:
  basic_expr  { $result = $basic_expr; }
  | expr[lhs] '&' expr[rhs]  { $result = new_ast_bin_and($lhs, $rhs); }
  | expr[lhs] '|' expr[rhs]  { $result = new_ast_bin_or($lhs, $rhs); }
  | expr[lhs] LS expr[rhs]  { $result = new_ast_lshift($lhs, $rhs); }
  | expr[lhs] RS expr[rhs]  { $result = new_ast_rshift($lhs, $rhs); }
  | expr[lhs] '+' expr[rhs]  { $result = new_ast_add($lhs, $rhs); }
  | expr[lhs] '-' expr[rhs]  { $result = new_ast_sub($lhs, $rhs); }
  | expr[lhs] '*' expr[rhs]  { $result = new_ast_mul($lhs, $rhs); }
  | expr[lhs] '/' expr[rhs]  { $result = new_ast_div($lhs, $rhs); }
  | expr[lhs] '%' expr[rhs]  { $result = new_ast_mod($lhs, $rhs); }
  | '-' expr[rhs] %prec UMINUS  { $result = new_ast_uminus($rhs); }
  | NOT expr[rhs]  { $result = new_ast_not($rhs); }
  | '~' expr[rhs]  { $result = new_ast_tilde($rhs); }
  | expr[lhs] COLLATE name  { $result = new_ast_collate($lhs, $name); }
  | expr[lhs] AND expr[rhs]  { $result = new_ast_and($lhs, $rhs); }
  | expr[lhs] OR expr[rhs]  { $result = new_ast_or($lhs, $rhs); }
  | expr[lhs] '=' expr[rhs]  { $result = new_ast_eq($lhs, $rhs); }
  | expr[lhs] EQEQ expr[rhs]  { $result = new_ast_eq($lhs, $rhs); }
  | expr[lhs] '<' expr[rhs]  { $result = new_ast_lt($lhs, $rhs); }
  | expr[lhs] '>' expr[rhs]  { $result = new_ast_gt($lhs, $rhs); }
  | expr[lhs] NE expr[rhs]  { $result = new_ast_ne($lhs, $rhs); }
  | expr[lhs] NE_ expr[rhs]  { $result = new_ast_ne($lhs, $rhs); }
  | expr[lhs] GE expr[rhs]  { $result = new_ast_ge($lhs, $rhs); }
  | expr[lhs] LE expr[rhs]  { $result = new_ast_le($lhs, $rhs); }
  | expr[lhs] NOT IN '(' expr_list ')'  { $result = new_ast_not_in($lhs, $expr_list); }
  | expr[lhs] NOT IN '(' select_stmt ')'  { $result = new_ast_not_in($lhs, $select_stmt); }
  | expr[lhs] IN '(' expr_list ')'  { $result = new_ast_in_pred($lhs, $expr_list); }
  | expr[lhs] IN '(' select_stmt ')'  { $result = new_ast_in_pred($lhs, $select_stmt); }
  | expr[lhs] LIKE expr[rhs]  { $result = new_ast_like($lhs, $rhs); }
  | expr[lhs] NOT_LIKE expr[rhs]  { $result = new_ast_not_like($lhs, $rhs); }
  | expr[lhs] MATCH expr[rhs]  { $result = new_ast_match($lhs, $rhs); }
  | expr[lhs] REGEXP expr[rhs]  { $result = new_ast_regexp($lhs, $rhs); }
  | expr[lhs] GLOB expr[rhs]  { $result = new_ast_glob($lhs, $rhs); }
  | expr[lhs] NOT BETWEEN math_expr[me1] AND math_expr[me2]  { $result = new_ast_not_between($lhs, new_ast_range($me1,$me2)); }
  | expr[lhs] BETWEEN math_expr[me1] AND math_expr[me2]  { $result = new_ast_between($lhs, new_ast_range($me1,$me2)); }
  | expr[lhs] IS_NOT expr[rhs]  { $result = new_ast_is_not($lhs, $rhs); }
  | expr[lhs] IS expr[rhs]  { $result = new_ast_is($lhs, $rhs); }
  | expr[lhs] CONCAT expr[rhs]  { $result = new_ast_concat($lhs, $rhs); }
  | CASE expr[cond] case_list END  { $result = new_ast_case_expr($cond, new_ast_connector($case_list, NULL)); }
  | CASE expr[cond1] case_list ELSE expr[cond2] END  { $result = new_ast_case_expr($cond1, new_ast_connector($case_list, $cond2));}
  | CASE case_list END  { $result = new_ast_case_expr(NULL, new_ast_connector($case_list, NULL));}
  | CASE case_list ELSE expr[cond] END  { $result = new_ast_case_expr(NULL, new_ast_connector($case_list, $cond));}
  | CAST '(' expr[sexp] AS data_type ')'  { $result = new_ast_cast_expr($sexp, $data_type); }
  ;

case_list[result]:
  WHEN expr[e1] THEN expr[e2]  { $result = new_ast_case_list(new_ast_when($e1, $e2), NULL); }
  | WHEN expr[e1] THEN expr[e2] case_list[cl]  { $result = new_ast_case_list(new_ast_when($e1, $e2), $cl);}
  ;

arg_expr: '*' { $arg_expr = new_ast_star(); }
  | expr { $arg_expr = $expr; }
  | shape_arguments { $arg_expr = $shape_arguments; }
  ;

arg_list[result]:
  /* nil */  { $result = NULL; }
  | arg_expr  { $result = new_ast_arg_list($arg_expr, NULL); }
  | arg_expr ',' arg_list[al]  { $result = new_ast_arg_list($arg_expr, $al); }
  ;

expr_list[result]:
  expr  { $result = new_ast_expr_list($expr, NULL); }
  | expr ',' expr_list[el]  { $result = new_ast_expr_list($expr, $el); }
  ;

shape_arguments:
  FROM name  { $shape_arguments = new_ast_from_shape($name, NULL); }
  | FROM name shape_def  { $shape_arguments = new_ast_from_shape($name, $shape_def); }
  | FROM ARGUMENTS  { $shape_arguments = new_ast_from_shape(new_ast_str("ARGUMENTS"), NULL); }
  | FROM ARGUMENTS shape_def  { $shape_arguments = new_ast_from_shape(new_ast_str("ARGUMENTS"), $shape_def); }
  ;

call_expr:
  expr  { $call_expr = $expr; }
  | shape_arguments  { $call_expr = $shape_arguments; }
  ;

call_expr_list[result]:
  call_expr  { $result = new_ast_expr_list($call_expr, NULL); }
  | call_expr ',' call_expr_list[cel]  { $result = new_ast_expr_list($call_expr, $cel); }
  ;

cte_tables[result]:
  cte_table  { $result = new_ast_cte_tables($cte_table, NULL); }
  | cte_table ',' cte_tables[ct]  { $result = new_ast_cte_tables($cte_table, $ct); }
  ;

cte_table:
    name '(' name_list ')' AS '(' select_stmt_no_with ')'  { 
      ast_node *cte_decl = new_ast_cte_decl($name, $name_list);
      $cte_table = new_ast_cte_table(cte_decl, $select_stmt_no_with); }
  | name '(' '*' ')' AS '(' select_stmt_no_with ')' {
      ast_node *cte_decl = new_ast_cte_decl($name, new_ast_star());
      $cte_table = new_ast_cte_table(cte_decl, $select_stmt_no_with); }
  ;

with_prefix:
  WITH cte_tables  { $with_prefix = new_ast_with($cte_tables); }
  | WITH RECURSIVE cte_tables  { $with_prefix = new_ast_with_recursive($cte_tables); }
  ;

with_select_stmt:
  with_prefix select_stmt_no_with  { $with_select_stmt = new_ast_with_select_stmt($with_prefix, $select_stmt_no_with); }
  ;

select_stmt:
  with_select_stmt  { $select_stmt = $with_select_stmt; }
  | select_stmt_no_with  { $select_stmt = $select_stmt_no_with; }
  ;

select_stmt_no_with:
  select_core_list opt_orderby opt_limit opt_offset  {
      struct ast_node *select_offset = new_ast_select_offset($opt_offset, NULL);
      struct ast_node *select_limit = new_ast_select_limit($opt_limit, select_offset);
      struct ast_node *select_orderby = new_ast_select_orderby($opt_orderby, select_limit);
       $select_stmt_no_with = new_ast_select_stmt($select_core_list, select_orderby);
  }
  ;

select_core_list[result]:
  select_core { $result = new_ast_select_core_list($select_core, NULL); }
  | select_core compound_operator select_core_list[list] {
     ast_node *select_core_compound = new_ast_select_core_compound(new_ast_opt($compound_operator), $list);
     $result = new_ast_select_core_list($select_core, select_core_compound); 
  }
  ;

values[result]:
  '(' insert_list ')'  {
    $result = new_ast_values($insert_list, NULL);
  }
  | '(' insert_list ')' ',' values[ov]  {
    $result = new_ast_values($insert_list, $ov);
  }
  ;

select_core:
  SELECT select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window  {
    struct ast_node *select_having = new_ast_select_having($opt_having, $opt_select_window);
    struct ast_node *select_groupby = new_ast_select_groupby($opt_groupby, select_having);
    struct ast_node *select_where = new_ast_select_where($opt_where, select_groupby);
    struct ast_node *select_from_etc = new_ast_select_from_etc($opt_from_query_parts, select_where);
    struct ast_node *select_expr_list_con = new_ast_select_expr_list_con($select_expr_list, select_from_etc);
     $select_core = new_ast_select_core($select_opts, select_expr_list_con);
  }
  | VALUES values  {
    $select_core = new_ast_select_core(new_ast_select_values(), $values);
  }
  ;

compound_operator:
  UNION  { $compound_operator = COMPOUND_OP_UNION; }
  | UNION_ALL  { $compound_operator = COMPOUND_OP_UNION_ALL; }
  | INTERSECT  { $compound_operator = COMPOUND_OP_INTERSECT; }
  | EXCEPT  { $compound_operator = COMPOUND_OP_EXCEPT; }
  ;

window_func_inv:
  name '(' arg_list ')' opt_filter_clause OVER window_name_or_defn  {
    struct ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, $opt_filter_clause);
    struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $arg_list);
    struct ast_node *win_func_call = new_ast_call($name, call_arg_list);
    $window_func_inv = new_ast_window_func_inv(win_func_call, $window_name_or_defn);
  }
  ;

opt_filter_clause:
  /* nil */  { $opt_filter_clause = NULL; }
  | FILTER '(' opt_where ')'  { $opt_filter_clause = new_ast_opt_filter_clause($opt_where); }
  ;

window_name_or_defn: window_defn
  | name
  ;

window_defn:
  '(' opt_partition_by opt_orderby opt_frame_spec ')'  {
    ast_node *window_defn_orderby = new_ast_window_defn_orderby($opt_orderby, $opt_frame_spec);
    $window_defn = new_ast_window_defn($opt_partition_by, window_defn_orderby);
  }
  ;

opt_frame_spec:
  /* nil */  { $opt_frame_spec = NULL; }
  | frame_type frame_boundary_opts frame_exclude  {
    int32_t frame_boundary_opts_flags = (int32_t)((int_ast_node *)($frame_boundary_opts)->left)->value;
    int32_t flags = $frame_type | frame_boundary_opts_flags | $frame_exclude;
    ast_node *expr_list = $frame_boundary_opts->right;
    $opt_frame_spec = new_ast_opt_frame_spec(new_ast_opt(flags), expr_list);
  }
  ;

frame_type:
  RANGE  { $frame_type = FRAME_TYPE_RANGE; }
  | ROWS  { $frame_type = FRAME_TYPE_ROWS; }
  | GROUPS  { $frame_type = FRAME_TYPE_GROUPS; }
  ;

frame_exclude:
  /* nil */  { $frame_exclude = FRAME_EXCLUDE_NONE; }
  | EXCLUDE_NO_OTHERS  { $frame_exclude = FRAME_EXCLUDE_NO_OTHERS; }
  | EXCLUDE_CURRENT_ROW  { $frame_exclude = FRAME_EXCLUDE_CURRENT_ROW; }
  | EXCLUDE_GROUP  { $frame_exclude = FRAME_EXCLUDE_GROUP; }
  | EXCLUDE_TIES  { $frame_exclude = FRAME_EXCLUDE_TIES; }
  ;

frame_boundary_opts:
  frame_boundary  {
    ast_node *ast_flags = $frame_boundary->left;
    ast_node *expr_list = new_ast_expr_list($frame_boundary->right, NULL);
    $frame_boundary_opts = new_ast_frame_boundary_opts(ast_flags, expr_list);
  }
  | BETWEEN frame_boundary_start AND frame_boundary_end  {
    int32_t flags = (int32_t)(((int_ast_node *)$frame_boundary_start->left)->value | ((int_ast_node *)$frame_boundary_end->left)->value);
    ast_node *expr_list = new_ast_expr_list($frame_boundary_start->right, $frame_boundary_end->right);
    $frame_boundary_opts = new_ast_frame_boundary_opts(new_ast_opt(flags), expr_list);
  }
  ;

frame_boundary_start:
  UNBOUNDED PRECEDING  { $frame_boundary_start = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_UNBOUNDED), NULL); }
  | expr PRECEDING  { $frame_boundary_start = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_PRECEDING), $expr); }
  | CURRENT_ROW  { $frame_boundary_start = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_CURRENT_ROW), NULL); }
  | expr FOLLOWING  { $frame_boundary_start = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_FOLLOWING), $expr); }
  ;

frame_boundary_end:
  expr PRECEDING  { $frame_boundary_end = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_PRECEDING), $expr); }
  | CURRENT_ROW  { $frame_boundary_end = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_CURRENT_ROW), NULL); }
  | expr FOLLOWING  { $frame_boundary_end = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_FOLLOWING), $expr); }
  | UNBOUNDED FOLLOWING  { $frame_boundary_end = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_UNBOUNDED), NULL); }
  ;

frame_boundary:
  UNBOUNDED PRECEDING  { $frame_boundary = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_UNBOUNDED), NULL); }
  | expr PRECEDING  { $frame_boundary = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_PRECEDING), $expr); }
  | CURRENT_ROW  { $frame_boundary = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_CURRENT_ROW), NULL); }
  ;

opt_partition_by:
  /* nil */  { $opt_partition_by = NULL; }
  | PARTITION BY expr_list  { $opt_partition_by = new_ast_opt_partition_by($expr_list); }
  ;

opt_select_window:
  /* nil */  { $opt_select_window = NULL; }
  | window_clause  { $opt_select_window = new_ast_opt_select_window($window_clause); }
  ;

window_clause:
  WINDOW window_name_defn_list  { $window_clause = new_ast_window_clause($window_name_defn_list); }
  ;

window_name_defn_list[result]:
  window_name_defn  { $result = new_ast_window_name_defn_list($window_name_defn, NULL); }
  | window_name_defn ',' window_name_defn_list[wndl]  { $result = new_ast_window_name_defn_list($window_name_defn, $wndl); }
  ;

window_name_defn:
  name AS window_defn  { $window_name_defn = new_ast_window_name_defn($name, $window_defn); }
  ;

region_spec:
    name  { $region_spec = new_ast_region_spec($name, new_ast_opt(PUBLIC_REGION)); }
  | name PRIVATE  { $region_spec = new_ast_region_spec($name, new_ast_opt(PRIVATE_REGION)); }
  ;

region_list[result]:
  region_spec ',' region_list[rl]  { $result = new_ast_region_list($region_spec, $rl); }
  | region_spec  { $result = new_ast_region_list($region_spec, NULL); }
  ;

declare_schema_region_stmt:
  AT_DECLARE_SCHEMA_REGION name  { $declare_schema_region_stmt = new_ast_declare_schema_region_stmt($name, NULL); }
  | AT_DECLARE_SCHEMA_REGION name USING region_list  { $declare_schema_region_stmt = new_ast_declare_schema_region_stmt($name, $region_list); }
  ;

declare_deployable_region_stmt:
  AT_DECLARE_DEPLOYABLE_REGION  name  { $declare_deployable_region_stmt = new_ast_declare_deployable_region_stmt($name, NULL); }
  | AT_DECLARE_DEPLOYABLE_REGION name USING region_list  { $declare_deployable_region_stmt = new_ast_declare_deployable_region_stmt($name, $region_list); }
  ;

begin_schema_region_stmt:
  AT_BEGIN_SCHEMA_REGION name  {$begin_schema_region_stmt = new_ast_begin_schema_region_stmt($name); }
  ;

end_schema_region_stmt:
  AT_END_SCHEMA_REGION  {$end_schema_region_stmt = new_ast_end_schema_region_stmt(); }
  ;

schema_ad_hoc_migration_stmt:
  AT_SCHEMA_AD_HOC_MIGRATION version_annotation  { $schema_ad_hoc_migration_stmt = new_ast_schema_ad_hoc_migration_stmt($version_annotation); }
  ;

emit_enums_stmt:
  AT_EMIT_ENUMS opt_name_list { $emit_enums_stmt = new_ast_emit_enums_stmt($opt_name_list); }
  ;

opt_from_query_parts:
  /* nil */  { $opt_from_query_parts = NULL; }
  | FROM query_parts  { $opt_from_query_parts = $query_parts; }
  ;

opt_where:
  /* nil */  { $opt_where = NULL; }
  | WHERE expr  { $opt_where = new_ast_opt_where($expr); }
  ;

opt_groupby:
  /* nil */  { $opt_groupby = NULL; }
  | GROUP BY groupby_list  { $opt_groupby = new_ast_opt_groupby($groupby_list); }
  ;

groupby_list[result]:
  groupby_item  { $result = new_ast_groupby_list($groupby_item, NULL); }
  | groupby_item ',' groupby_list[gl]  { $result = new_ast_groupby_list($groupby_item, $gl); }
  ;

groupby_item:
  expr opt_asc_desc  { $groupby_item = new_ast_groupby_item($expr, $opt_asc_desc); }
  ;

opt_asc_desc:
  /* nil */  { $opt_asc_desc = NULL; }
  | ASC  { $opt_asc_desc = new_ast_asc(); }
  | DESC  { $opt_asc_desc = new_ast_desc(); }
  ;

opt_having:
  /* nil */  { $opt_having = NULL; }
  | HAVING expr  { $opt_having = new_ast_opt_having($expr); }
  ;

opt_orderby:
  /* nil */  { $opt_orderby = NULL; }
  | ORDER BY groupby_list  { $opt_orderby = new_ast_opt_orderby($groupby_list); }
  ;

opt_limit:
  /* nil */  { $opt_limit = NULL; }
  | LIMIT expr  { $opt_limit = new_ast_opt_limit($expr); }
  ;

opt_offset:
  /* nil */  { $opt_offset = NULL; }
  | OFFSET expr  { $opt_offset = new_ast_opt_offset($expr); }
  ;

select_opts:
  /* nil */  { $select_opts = NULL; }
  | ALL  { $select_opts = new_ast_select_opts(new_ast_all()); }
  | DISTINCT  { $select_opts = new_ast_select_opts(new_ast_distinct()); }
  | DISTINCTROW  { $select_opts = new_ast_select_opts(new_ast_distinctrow()); }
  ;

select_expr_list[result]:
  select_expr  { $result = new_ast_select_expr_list($select_expr, NULL); }
  | select_expr ',' select_expr_list[sel]  { $result = new_ast_select_expr_list($select_expr, $sel); }
  | '*'  { $result = new_ast_select_expr_list(new_ast_star(), NULL); }
  ;

select_expr:
  expr opt_as_alias  { $select_expr = new_ast_select_expr($expr, $opt_as_alias); }
  |  name '.' '*'  { $select_expr = new_ast_table_star($name); }
  ;

opt_as_alias:
  /* nil */  { $opt_as_alias = NULL;  }
  | as_alias
  ;

as_alias:
  AS name  { $as_alias = new_ast_opt_as_alias($name); }
  | name  { $as_alias = new_ast_opt_as_alias($name); }
  ;

query_parts:
  table_or_subquery_list  { $query_parts = $table_or_subquery_list; }
  | join_clause  { $query_parts = $join_clause; }
  ;

table_or_subquery_list[result]:
  table_or_subquery  { $result = new_ast_table_or_subquery_list($table_or_subquery, NULL); }
  | table_or_subquery ',' table_or_subquery_list[tsl]  { $result = new_ast_table_or_subquery_list($table_or_subquery, $tsl); }
  ;

join_clause:
  table_or_subquery join_target_list  { $join_clause = new_ast_join_clause($table_or_subquery, $join_target_list); }
  ;

join_target_list[result]:
  join_target  { $result = new_ast_join_target_list($join_target, NULL); }
  | join_target join_target_list[jtl]  { $result = new_ast_join_target_list($join_target, $jtl); }
  ;

table_or_subquery:
  name opt_as_alias  { $table_or_subquery = new_ast_table_or_subquery($name, $opt_as_alias); }
  | '(' select_stmt ')' opt_as_alias  { $table_or_subquery = new_ast_table_or_subquery($select_stmt, $opt_as_alias); }
  | table_function opt_as_alias  { $table_or_subquery = new_ast_table_or_subquery($table_function, $opt_as_alias); }
  | '(' query_parts ')'  { $table_or_subquery = new_ast_table_or_subquery($query_parts, NULL); }
  ;

join_type:  
  /*nil */       { $join_type = JOIN_INNER; }
  | LEFT         { $join_type = JOIN_LEFT; }
  | RIGHT        { $join_type = JOIN_RIGHT; }
  | LEFT OUTER   { $join_type = JOIN_LEFT_OUTER; }
  | RIGHT OUTER  { $join_type = JOIN_RIGHT_OUTER; }
  | INNER        { $join_type = JOIN_INNER; }
  | CROSS        { $join_type = JOIN_CROSS; }
  ;

join_target: join_type JOIN table_or_subquery opt_join_cond  {
      struct ast_node *asti_join_type = new_ast_opt($join_type);
      struct ast_node *table_join = new_ast_table_join($table_or_subquery, $opt_join_cond);
      $join_target = new_ast_join_target(asti_join_type, table_join); }
  ;

opt_join_cond:
  /* nil */  { $opt_join_cond = NULL; }
  | join_cond
  ;

join_cond:
  ON expr  { $join_cond = new_ast_join_cond(new_ast_on(), $expr); }
  | USING '(' name_list ')'  { $join_cond = new_ast_join_cond(new_ast_using(), $name_list); }
  ;

table_function:
  name '(' arg_list ')'  { $table_function = new_ast_table_function($name, $arg_list); }
  ;

create_view_stmt:
  CREATE opt_temp VIEW opt_if_not_exists name AS select_stmt opt_delete_version_attr  {
  struct ast_node *flags = new_ast_opt($opt_temp | $opt_if_not_exists);
  struct ast_node *name_and_select = new_ast_name_and_select($name, $select_stmt);
  struct ast_node *view_and_attrs = new_ast_view_and_attrs(name_and_select, $opt_delete_version_attr);
  $create_view_stmt = new_ast_create_view_stmt(flags, view_and_attrs); }
  ;

with_delete_stmt:
  with_prefix delete_stmt  { $with_delete_stmt = new_ast_with_delete_stmt($with_prefix, $delete_stmt); }
  ;

delete_stmt:
  DELETE FROM name opt_where  {
   $delete_stmt = new_ast_delete_stmt($name, $opt_where); }
  ;

opt_insert_dummy_spec:
  /*nil*/  { $opt_insert_dummy_spec = NULL; }
  | AT_DUMMY_SEED '(' expr ')' dummy_modifier  {
    $opt_insert_dummy_spec = new_ast_insert_dummy_spec($expr, new_ast_opt($dummy_modifier)); }
  ;

dummy_modifier:
  /* nil */  { $dummy_modifier = 0; }
  | AT_DUMMY_NULLABLES  { $dummy_modifier = INSERT_DUMMY_NULLABLES; }
  | AT_DUMMY_DEFAULTS  { $dummy_modifier = INSERT_DUMMY_DEFAULTS; }
  | AT_DUMMY_NULLABLES AT_DUMMY_DEFAULTS  { $dummy_modifier = INSERT_DUMMY_NULLABLES | INSERT_DUMMY_DEFAULTS; }
  | AT_DUMMY_DEFAULTS AT_DUMMY_NULLABLES  { $dummy_modifier = INSERT_DUMMY_NULLABLES | INSERT_DUMMY_DEFAULTS; }
  ;

insert_stmt_type:
  INSERT INTO  { $insert_stmt_type = new_ast_insert_normal();  }
  | INSERT OR REPLACE INTO  { $insert_stmt_type = new_ast_insert_or_replace(); }
  | INSERT OR IGNORE INTO  { $insert_stmt_type = new_ast_insert_or_ignore(); }
  | INSERT OR ROLLBACK INTO  { $insert_stmt_type = new_ast_insert_or_rollback(); }
  | INSERT OR ABORT INTO  { $insert_stmt_type = new_ast_insert_or_abort(); }
  | INSERT OR FAIL INTO  { $insert_stmt_type = new_ast_insert_or_fail(); }
  | REPLACE INTO  { $insert_stmt_type = new_ast_insert_replace(); }
  ;

with_insert_stmt:
  with_prefix insert_stmt  { $with_insert_stmt = new_ast_with_insert_stmt($with_prefix, $insert_stmt); }
  ;

opt_column_spec:
  /* nil */  { $opt_column_spec = NULL; }
  | '(' opt_name_list ')'  { $opt_column_spec = new_ast_column_spec($opt_name_list); }
  | '(' shape_def ')'  { $opt_column_spec = new_ast_column_spec($shape_def); }
  ;

from_shape:
  FROM CURSOR name opt_column_spec  { $from_shape = new_ast_from_shape($opt_column_spec, $name); }
  | FROM name opt_column_spec  { $from_shape = new_ast_from_shape($opt_column_spec, $name); }
  | FROM ARGUMENTS opt_column_spec  { $from_shape = new_ast_from_shape($opt_column_spec, new_ast_str("ARGUMENTS")); }
  ;

insert_stmt:
  insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $select_stmt);
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, columns_values);
    $insert_stmt_type->left = $opt_insert_dummy_spec;
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values);  }
  | insert_stmt_type name opt_column_spec from_shape opt_insert_dummy_spec  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $from_shape);
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, columns_values);
    $insert_stmt_type->left = $opt_insert_dummy_spec;
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values);  }
  | insert_stmt_type name DEFAULT VALUES  {
    struct ast_node *default_columns_values = new_ast_default_columns_values();
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, default_columns_values);
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values);
  }
  | insert_stmt_type name USING expr_names opt_insert_dummy_spec {
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, $expr_names);
    $insert_stmt_type->left = $opt_insert_dummy_spec;
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values); }
  ;

insert_list[result]:
  /* nil */  { $result = NULL; }
  | expr  { $result = new_ast_insert_list($expr, NULL); }
  | expr ',' insert_list[il]  { $result = new_ast_insert_list($expr, $il); }
  ;

basic_update_stmt:
  UPDATE opt_name SET update_list opt_where  {
    struct ast_node *orderby = new_ast_update_orderby(NULL, NULL);
    struct ast_node *where = new_ast_update_where($opt_where, orderby);
    struct ast_node *list = new_ast_update_set($update_list, where);
    $basic_update_stmt = new_ast_update_stmt($opt_name, list); }
  ;

with_update_stmt:
  with_prefix update_stmt  { $with_update_stmt = new_ast_with_update_stmt($with_prefix, $update_stmt); }
  ;

update_stmt:
  UPDATE name SET update_list opt_where opt_orderby opt_limit  {
    struct ast_node *limit = $opt_limit;
    struct ast_node *orderby = new_ast_update_orderby($opt_orderby, limit);
    struct ast_node *where = new_ast_update_where($opt_where, orderby);
    struct ast_node *list = new_ast_update_set($update_list, where);
    $update_stmt = new_ast_update_stmt($name, list); }
  ;

update_entry:
  name '=' expr  { $update_entry = new_ast_update_entry($name, $expr); }
  ;

update_list[result]:
  update_entry  { $result = new_ast_update_list($update_entry, NULL); }
  | update_entry ',' update_list[ul]  { $result = new_ast_update_list($update_entry, $ul); }
  ;

with_upsert_stmt:
  with_prefix upsert_stmt  { $with_upsert_stmt = new_ast_with_upsert_stmt($with_prefix, $upsert_stmt); }
  ;

upsert_stmt:
  insert_stmt ON_CONFLICT conflict_target DO NOTHING  {
    struct ast_node *upsert_update = new_ast_upsert_update($conflict_target, NULL);
    $upsert_stmt = new_ast_upsert_stmt($insert_stmt, upsert_update); }
  | insert_stmt ON_CONFLICT conflict_target DO basic_update_stmt  {
    struct ast_node *upsert_update = new_ast_upsert_update($conflict_target, $basic_update_stmt);
    $upsert_stmt = new_ast_upsert_stmt($insert_stmt, upsert_update); }
  ;

update_cursor_stmt:
  UPDATE CURSOR name opt_column_spec FROM VALUES '(' insert_list ')'  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $insert_list);
    $update_cursor_stmt = new_ast_update_cursor_stmt($name, columns_values); }
  | UPDATE CURSOR name opt_column_spec from_shape  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $from_shape);
    $update_cursor_stmt = new_ast_update_cursor_stmt($name, columns_values); }
  | UPDATE CURSOR name USING expr_names {
    $update_cursor_stmt = new_ast_update_cursor_stmt($name, $expr_names); }
  ;

conflict_target:
  /* nil */  { $conflict_target = new_ast_conflict_target(NULL, NULL); }
  | '(' indexed_columns ')' opt_where  {
    $conflict_target = new_ast_conflict_target($indexed_columns, $opt_where);
  }
  ;

creation_type:
  object_type  { $creation_type = $object_type; }
  | object_type NOT NULL_  { $creation_type = new_ast_notnull($object_type); }
  | TEXT  { $creation_type = new_ast_type_text(); }
  | TEXT NOT NULL_  { $creation_type = new_ast_notnull(new_ast_type_text()); }
  | BLOB  { $creation_type = new_ast_type_blob(); }
  | BLOB NOT NULL_  { $creation_type = new_ast_notnull(new_ast_type_blob()); }
  ;

function: FUNC | FUNCTION
  ;

declare_enum_stmt:
  DECLARE ENUM name data_type_numeric '(' enum_values ')' {
     ast_node *typed_name = new_ast_typed_name($name, $data_type_numeric);
     $declare_enum_stmt = new_ast_declare_enum_stmt(typed_name, $enum_values); }
  ;

enum_values[result]:
    enum_value { $result = new_ast_enum_values($enum_value, NULL); }
  | enum_value ',' enum_values[next] { $result = new_ast_enum_values($enum_value, $next); }
  ;

enum_value:
    name { $enum_value = new_ast_enum_value($name, NULL); }
  | name '=' expr { $enum_value = new_ast_enum_value($name, $expr); }
  ;

declare_func_stmt:
  DECLARE function name '(' params ')' data_type_opt_notnull  {
      $declare_func_stmt = new_ast_declare_func_stmt($name, new_ast_func_params_return($params, $data_type_opt_notnull)); }
  | DECLARE SELECT function name '(' params ')' data_type_opt_notnull  {
      $declare_func_stmt = new_ast_declare_select_func_stmt($name, new_ast_func_params_return($params, $data_type_opt_notnull)); }
  | DECLARE function name '(' params ')' CREATE creation_type  {
      ast_node *type = new_ast_create($creation_type);
      $declare_func_stmt = new_ast_declare_func_stmt($name, new_ast_func_params_return($params, type)); }
  | DECLARE SELECT function name '(' params ')' '(' typed_names ')'  {
      $declare_func_stmt = new_ast_declare_select_func_stmt($name, new_ast_func_params_return($params, $typed_names)); }
  ;

procedure: PROC | PROCEDURE
  ;

declare_proc_stmt:
  DECLARE procedure name '(' params ')'  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_BASIC));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, NULL)); }
  | DECLARE procedure name '(' params ')' '(' typed_names ')'  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_DML));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, $typed_names)); }
  | DECLARE procedure name '(' params ')' USING TRANSACTION  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_USES_DML));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, NULL)); }
  | DECLARE procedure name '(' params ')' OUT '(' typed_names ')'  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, $typed_names)); }
  | DECLARE procedure name '(' params ')' OUT '(' typed_names ')' USING TRANSACTION  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT | PROC_FLAG_USES_DML));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, $typed_names)); }
  | DECLARE procedure name '(' params ')' OUT UNION '(' typed_names ')'  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT_UNION));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, $typed_names)); }
  | DECLARE procedure name '(' params ')' OUT UNION '(' typed_names ')' USING TRANSACTION  {
      ast_node *proc_name_flags = new_ast_proc_name_type($name, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT_UNION | PROC_FLAG_USES_DML));
      $declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($params, $typed_names)); }
  ;

create_proc_stmt:
  CREATE procedure name '(' params ')' BEGIN_ opt_stmt_list END  {
    $create_proc_stmt = new_ast_create_proc_stmt($name, new_ast_proc_params_stmts($params, $opt_stmt_list)); }
  ;

inout:
  IN  { $inout = new_ast_in(); }
  | OUT  { $inout = new_ast_out(); }
  | INOUT  { $inout = new_ast_inout(); }
  ;

typed_name:
  name data_type_opt_notnull  { $typed_name = new_ast_typed_name($name, $data_type_opt_notnull); }
  | shape_def  { $typed_name = new_ast_typed_name(NULL, $shape_def); }
  | name shape_def  { $typed_name = new_ast_typed_name($name, $shape_def); }
  ;

typed_names[result]:
  typed_name  { $result = new_ast_typed_names($typed_name, NULL); }
  | typed_name ',' typed_names[tn]  { $result = new_ast_typed_names($typed_name, $tn);}
  ;

param:
  name data_type_opt_notnull  { $param = new_ast_param(NULL, new_ast_param_detail($name, $data_type_opt_notnull)); }
  | inout name data_type_opt_notnull  { $param = new_ast_param($inout, new_ast_param_detail($name, $data_type_opt_notnull)); }
  | shape_def  { $param = new_ast_param(NULL, new_ast_param_detail(NULL, $shape_def)); }
  | name shape_def  { $param = new_ast_param(NULL, new_ast_param_detail($name, $shape_def)); }
  ;

params[result]:
  /* nil */  { $result = NULL; }
  | param  { $result = new_ast_params($param, NULL); }
  |  param ',' params[par]  { $result = new_ast_params($param, $par); }
  ;

declare_stmt:
  DECLARE name_list data_type_opt_notnull  { $declare_stmt = new_ast_declare_vars_type($name_list, $data_type_opt_notnull); }
  | DECLARE name CURSOR FOR select_stmt  { $declare_stmt = new_ast_declare_cursor($name, $select_stmt); }
  | DECLARE name CURSOR FOR explain_stmt  { $declare_stmt = new_ast_declare_cursor($name, $explain_stmt); }
  | DECLARE name CURSOR FOR call_stmt  { $declare_stmt = new_ast_declare_cursor($name, $call_stmt); }
  | DECLARE name CURSOR FETCH FROM call_stmt  { $declare_stmt = new_ast_declare_value_cursor($name, $call_stmt); }
  | DECLARE name CURSOR shape_def  { $declare_stmt = new_ast_declare_cursor_like_name($name, $shape_def); }
  | DECLARE name CURSOR LIKE select_stmt  { $declare_stmt = new_ast_declare_cursor_like_select($name, $select_stmt); }
  | DECLARE name[id] CURSOR FOR name[obj] { $declare_stmt = new_ast_declare_cursor($id, $obj); }
  ;

call_stmt:
  CALL name '(' ')'  { $call_stmt = new_ast_call_stmt($name, NULL); }
  | CALL name '(' call_expr_list ')'  { $call_stmt = new_ast_call_stmt($name, $call_expr_list); }
  ;

while_stmt:
  WHILE expr BEGIN_ opt_stmt_list END  { $while_stmt = new_ast_while_stmt($expr, $opt_stmt_list); }
  ;

loop_stmt:
  LOOP fetch_stmt BEGIN_ opt_stmt_list END  { $loop_stmt = new_ast_loop_stmt($fetch_stmt, $opt_stmt_list); }
  ;

leave_stmt:
  LEAVE  { $leave_stmt = new_ast_leave_stmt(); }
  ;

return_stmt:
  RETURN  { $return_stmt = new_ast_return_stmt(); }
  ;

rollback_return_stmt:
  ROLLBACK RETURN { $rollback_return_stmt = new_ast_rollback_return_stmt(); }
  ;

commit_return_stmt:
  COMMIT RETURN  { $commit_return_stmt = new_ast_commit_return_stmt(); }
  ;

throw_stmt:
  THROW  { $throw_stmt = new_ast_throw_stmt(); }
  ;

trycatch_stmt:
  BEGIN_ TRY opt_stmt_list[osl1] END TRY ';' BEGIN_ CATCH opt_stmt_list[osl2] END CATCH  { $trycatch_stmt = new_ast_trycatch_stmt($osl1, $osl2); }
  ;

continue_stmt:
  CONTINUE  { $continue_stmt = new_ast_continue_stmt(); }
  ;

fetch_stmt:
  FETCH name INTO name_list  { $fetch_stmt = new_ast_fetch_stmt($name, $name_list); }
  | FETCH name  { $fetch_stmt = new_ast_fetch_stmt($name, NULL); }
  ;

fetch_values_stmt:
  FETCH name opt_column_spec FROM VALUES '(' insert_list ')' opt_insert_dummy_spec  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $insert_list);
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, columns_values);
    $fetch_values_stmt = new_ast_fetch_values_stmt($opt_insert_dummy_spec, name_columns_values); }
  | FETCH name opt_column_spec from_shape opt_insert_dummy_spec  {
    struct ast_node *columns_values = new_ast_columns_values($opt_column_spec, $from_shape);
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, columns_values);
    $fetch_values_stmt = new_ast_fetch_values_stmt($opt_insert_dummy_spec, name_columns_values); }
  | FETCH name USING expr_names opt_insert_dummy_spec {
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, $expr_names);
    $fetch_values_stmt = new_ast_fetch_values_stmt($opt_insert_dummy_spec, name_columns_values); }
  ;

expr_names[result]:
  expr_name  { $result = new_ast_expr_names($expr_name, NULL); }
  |  expr_name ',' expr_names[sel]  { $result = new_ast_expr_names($expr_name, $sel); }
  ;

expr_name: expr as_alias { $expr_name = new_ast_expr_name($expr, $as_alias); }
  ;

fetch_call_stmt:
  FETCH name opt_column_spec FROM call_stmt  {
    YY_ERROR_ON_COLUMNS($opt_column_spec);  // not really allowed, see macro for details.
    $fetch_call_stmt = new_ast_fetch_call_stmt($name, $call_stmt); }
  ;

open_stmt:
  OPEN name  { $open_stmt = new_ast_open_stmt($name); }
  ;

close_stmt:
  CLOSE name  { $close_stmt = new_ast_close_stmt($name); }
  ;

out_stmt:
  OUT name  { $out_stmt = new_ast_out_stmt($name); }
  ;

out_union_stmt:
  OUT UNION name  { $out_union_stmt = new_ast_out_union_stmt($name); }
  ;

if_stmt:
  IF expr THEN opt_stmt_list opt_elseif_list opt_else END IF  {
    struct ast_node *if_alt = new_ast_if_alt($opt_elseif_list, $opt_else);
    struct ast_node *cond_action = new_ast_cond_action($expr, $opt_stmt_list);
    $if_stmt = new_ast_if_stmt(cond_action, if_alt); }
  ;

opt_else:
  /* nil */  { $opt_else = NULL; }
  | ELSE opt_stmt_list  { $opt_else = new_ast_else($opt_stmt_list); }
  ;

elseif_item:
  ELSE_IF expr THEN opt_stmt_list  {
    struct ast_node *cond_action = new_ast_cond_action($expr, $opt_stmt_list);
    $elseif_item = new_ast_elseif(cond_action, NULL); }
  ;

elseif_list[result]:
  elseif_item  { $result = $elseif_item; }
  | elseif_item elseif_list[el2]  { $elseif_item->right = $el2; $result = $elseif_item; }
  ;

opt_elseif_list:
  /* nil */  { $opt_elseif_list = NULL; }
  | elseif_list  { $opt_elseif_list = $elseif_list; }
  ;

begin_trans_stmt:
  BEGIN_ TRANSACTION  { $begin_trans_stmt = new_ast_begin_trans_stmt(); }
  ;

rollback_trans_stmt:
  ROLLBACK TRANSACTION  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt(NULL); }
  | ROLLBACK TRANSACTION TO SAVEPOINT name  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt($name); }
  | ROLLBACK TRANSACTION TO SAVEPOINT AT_PROC  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt(new_ast_str("@PROC")); }
  ;

commit_trans_stmt:
  COMMIT TRANSACTION  { $commit_trans_stmt = new_ast_commit_trans_stmt(); }
  ;

proc_savepoint_stmt:  procedure SAVEPOINT BEGIN_ opt_stmt_list END {
    $proc_savepoint_stmt = new_ast_proc_savepoint_stmt($opt_stmt_list);
  }
  ;

savepoint_stmt:
  SAVEPOINT name  {
    $savepoint_stmt = new_ast_savepoint_stmt($name); }
  | SAVEPOINT AT_PROC {
    $savepoint_stmt = new_ast_savepoint_stmt(new_ast_str("@PROC")); }
  ;

release_savepoint_stmt:
  RELEASE SAVEPOINT name  {
    $release_savepoint_stmt = new_ast_release_savepoint_stmt($name); }
  | RELEASE SAVEPOINT AT_PROC {
    $release_savepoint_stmt = new_ast_release_savepoint_stmt(new_ast_str("@PROC")); }
  ;

echo_stmt:
  AT_ECHO name ',' str_literal  { $echo_stmt = new_ast_echo_stmt($name, $str_literal); }
  ;

alter_table_add_column_stmt:
  ALTER TABLE name ADD COLUMN col_def  {
    $alter_table_add_column_stmt = new_ast_alter_table_add_column_stmt($name, $col_def); }
  ;

create_trigger_stmt:
  CREATE opt_temp TRIGGER opt_if_not_exists trigger_def opt_delete_version_attr  {
    int flags = $opt_temp | $opt_if_not_exists;
    $create_trigger_stmt = new_ast_create_trigger_stmt(
        new_ast_opt(flags),
        new_ast_trigger_body_vers($trigger_def, $opt_delete_version_attr)); }
  ;

trigger_def:
  name[n1] trigger_condition trigger_operation ON name[n2] trigger_action  {
  $trigger_def = new_ast_trigger_def(
        $n1,
        new_ast_trigger_condition(
          new_ast_opt($trigger_condition),
          new_ast_trigger_op_target(
            $trigger_operation,
            new_ast_trigger_target_action(
              $n2,
              $trigger_action)))); }
  ;

trigger_condition:
  /* nil */  { $trigger_condition = TRIGGER_BEFORE; /* before is the default per https://sqlite.org/lang_createtrigger.html */ }
  | BEFORE  { $trigger_condition = TRIGGER_BEFORE; }
  | AFTER  { $trigger_condition = TRIGGER_AFTER; }
  | INSTEAD OF  { $trigger_condition = TRIGGER_INSTEAD_OF; }
 ;

trigger_operation:
  DELETE  { $trigger_operation = new_ast_trigger_operation(new_ast_opt(TRIGGER_DELETE), NULL); }
  | INSERT  { $trigger_operation = new_ast_trigger_operation(new_ast_opt(TRIGGER_INSERT), NULL); }
  | UPDATE opt_of  { $trigger_operation = new_ast_trigger_operation(new_ast_opt(TRIGGER_UPDATE), $opt_of); }
  ;

opt_of:
  /* nil */  { $opt_of = NULL; }
  | OF name_list  { $opt_of = $name_list; }
  ;

trigger_action:
  opt_foreachrow opt_when_expr BEGIN_ trigger_stmts END  {
  $trigger_action = new_ast_trigger_action(
        new_ast_opt($opt_foreachrow),
        new_ast_trigger_when_stmts($opt_when_expr, $trigger_stmts)); }
  ;

opt_foreachrow:
  /* nil */  { $opt_foreachrow = 0; }
  | FOR_EACH_ROW  { $opt_foreachrow = TRIGGER_FOR_EACH_ROW; }
  ;

opt_when_expr:
  /* nil */  { $opt_when_expr = NULL; }
  | WHEN expr  { $opt_when_expr = $expr; }
  ;

trigger_stmts[result]:
  trigger_stmt  { $result = new_ast_stmt_list($trigger_stmt, NULL); }
  | trigger_stmt  trigger_stmts[ts]  { $result = new_ast_stmt_list($trigger_stmt, $ts); }
  ;

trigger_stmt:
  trigger_update_stmt ';'  { $trigger_stmt = $trigger_update_stmt; }
  | trigger_insert_stmt ';'  { $trigger_stmt = $trigger_insert_stmt; }
  | trigger_delete_stmt ';'  { $trigger_stmt = $trigger_delete_stmt; }
  | trigger_select_stmt ';'  { $trigger_stmt = $trigger_select_stmt; }
  ;

trigger_select_stmt:
  select_stmt_no_with  { $trigger_select_stmt = $select_stmt_no_with; }
  ;

trigger_insert_stmt:
  insert_stmt  { $trigger_insert_stmt = $insert_stmt; }
  ;

trigger_delete_stmt:
  delete_stmt  { $trigger_delete_stmt = $delete_stmt; }
  ;

trigger_update_stmt:
  basic_update_stmt  { $trigger_update_stmt = $basic_update_stmt; }
  ;

enforcement_options:
  FOREIGN KEY ON UPDATE  { $enforcement_options = new_ast_opt(ENFORCE_FK_ON_UPDATE); }
  | FOREIGN KEY ON DELETE  { $enforcement_options = new_ast_opt(ENFORCE_FK_ON_DELETE); }
  | JOIN  { $enforcement_options = new_ast_opt(ENFORCE_STRICT_JOIN); }
  | UPSERT STATEMENT  { $enforcement_options = new_ast_opt(ENFORCE_UPSERT_STMT); }
  | WINDOW function  { $enforcement_options = new_ast_opt(ENFORCE_WINDOW_FUNC); }
  | procedure  { $enforcement_options = new_ast_opt(ENFORCE_PROCEDURE); }
  | WITHOUT ROWID  { $enforcement_options = new_ast_opt(ENFORCE_WITHOUT_ROWID); }
  ;

enforce_strict_stmt:
  AT_ENFORCE_STRICT enforcement_options  { $enforce_strict_stmt = new_ast_enforce_strict_stmt($enforcement_options); }
  ;

enforce_normal_stmt:
  AT_ENFORCE_NORMAL enforcement_options  { $enforce_normal_stmt = new_ast_enforce_normal_stmt($enforcement_options); }
  ;

%%

#pragma clang diagnostic pop

void yyerror(const char *format, ...) {
  extern int yylineno;
  va_list args;
  va_start(args, format);

  CHARBUF_OPEN(err);
  bprintf(&err, "Error at: %s:%d ", current_file, yylineno);
  vbprintf(&err, format, args);
  cql_emit_error(err.ptr);
  CHARBUF_CLOSE(err);
  va_end(args);
}

static unsigned long next_id = 0;

static void print_dot(struct ast_node *node) {
  assert(node);
  unsigned long id = next_id++;

  bool_t primitive = true;

  if (is_ast_num(node)) {
    cql_output("\n    %s%lx [label = \"%s\" shape=plaintext]", node->type, id, ((struct num_ast_node*)node)->value);
  } else if (is_ast_str(node)) {
    cql_output("\n    %s%lx [label = \"%s\" shape=plaintext]", node->type, id, ((struct str_ast_node*)node)->value);
  } else {
    cql_output("\n    %s%lx [label = \"%s\" shape=plaintext]", node->type, id, node->type);
    primitive = false;
  }

  if (primitive) {
    return;
  }

  if (!ast_has_left(node) && !ast_has_right(node)) {
    return;
  }

  if (ast_has_left(node)) {
    cql_output("\n    %s%lx -> %s%lx;", node->type, id, node->left->type, next_id);
    print_dot(node->left);
  } else {
    cql_output("\n    _%lx [label = \"⏚\" shape=plaintext]", id);
    cql_output("\n    %s%lx -> _%lx;", node->type, id, id);
  }

  if (ast_has_right(node)) {
    cql_output("\n %s%lx -> %s%lx;", node->type, id, node->right->type, next_id);
    print_dot(node->right);
  } else {
    cql_output("\n    _%lx [label = \"⏚\" shape=plaintext]", id);
    cql_output("\n    %s%lx -> _%lx;", node->type, id, id);
  }
}

cql_data_defn( cmd_options options );

cql_data_defn( const char *global_proc_name );

cql_data_defn( rtdata *rt );

static int32_t gather_arg_params(int32_t a, int32_t argc, char **argv, int32_t *out_count, char ***out_args);

static void parse_cmd(int argc, char **argv) {

  // default result type
  options.rt = "c";
  rt = find_rtdata(options.rt);
  Invariant(rt);

  current_file = "<stdin>";

  for (int32_t a = 1; a < argc; a++) {
    char *arg = argv[a];
    if (strcmp(arg, "--print") == 0 || strcmp(arg, "-p") == 0) {
      options.print_ast = 1;
    } else if (strcmp(arg, "--nolines") == 0) {
      options.nolines = 1;
    } else if (strcmp(arg, "--dot") == 0) {
      options.print_dot = 1;
    } else if (strcmp(arg, "--sem") == 0) {
      options.semantic = 1;
    } else if (strcmp(arg, "--compress") == 0) {
        options.compress = 1;
    } else if (strcmp(arg, "--run_unit_tests") == 0) {
        options.run_unit_tests = 1;
    } else if (strcmp(arg, "--generate_exports") == 0) {
        options.generate_exports = 1;
    } else if (strcmp(arg, "--generate_type_getters") == 0) {
        options.generate_type_getters = 1;
    } else if (strcmp(arg, "--cg") == 0) {
      a = gather_arg_params(a, argc, argv, &options.file_names_count, &options.file_names);
      options.codegen = 1;
      options.semantic = 1;
    } else if (strcmp(arg, "--include_regions") == 0) {
      a = gather_arg_params(a, argc, argv, &options.include_regions_count, &options.include_regions);
    } else if (strcmp(arg, "--exclude_regions") == 0) {
      a = gather_arg_params(a, argc, argv, &options.exclude_regions_count, &options.exclude_regions);
    } else if (strcmp(arg, "--generate_copy") == 0) {
      options.generate_copy = 1;
    } else if (strcmp(arg, "--cqlrt") == 0) {
      if (a + 1 < argc) {
        a++;
        options.cqlrt = argv[a];
      } else {
        cql_error("--cqlrt requires an additional param for the name of the runtime header\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--rt") == 0) {
      if (a + 1 < argc) {
        a++;
        options.rt = argv[a];
        rt = find_rtdata(options.rt);
        if (!rt) {
          cql_error("unknown cg runtime '%s'\n", options.rt);
          cql_cleanup_and_exit(1);
        }
      } else {
        fprintf(
          stderr,
          "--rt requires an additional runtime param (e.g. c, objc, java, json_schema)\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--test") == 0) {
      options.test = 1;
    } else if (strcmp(arg, "--dev") == 0) {
      options.dev = 1;
    } else if (strcmp(arg, "--in") == 0) {
      if (a + 1 < argc) {
        a++;
        FILE *f = fopen(argv[a], "r");
        if (!f) {
          cql_error("unable to open '%s' for read\n", argv[a]);
          cql_cleanup_and_exit(1);
        }
        yyset_in(f);

        current_file = argv[a];
      } else {
        cql_error("--in requires an additional file name param\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--global_proc") == 0) {
      if (a + 1 < argc) {
        a++;
        global_proc_name = argv[a];
      } else {
        cql_error("--global_proc requires an additional proc name param\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--objc_c_include_path") == 0) {
      if (a + 1 < argc) {
        a++;
        options.objc_c_include_path = argv[a];
      } else {
        cql_error("--objc_c_include_path requires an additional include path for a C header\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--objc_assembly_query_namespace") == 0) {
      if (a + 1 < argc) {
        a++;
        options.objc_assembly_query_namespace = argv[a];
      } else {
        cql_error("--objc_assembly_query_namespace requires an additional namespace for the assembly query Objective-C header\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--c_include_namespace") == 0) {
      if (a + 1 < argc) {
        a++;
        options.c_include_namespace = argv[a];
      } else {
        cql_error("--c_include_namespace requires an additional C include namespace\n");
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--java_package_name") == 0) {
      if (a + 1 < argc) {
        a++;
        options.java_package_name = argv[a];
      } else {
        cql_error("--java_package_name requires an additional Java package name\n");
        cql_cleanup_and_exit(1);
      }
    }
    else if (strcmp(arg, "--java_assembly_query_classname") == 0) {
      if (a + 1 < argc) {
        a++;
        options.java_assembly_query_classname = argv[a];
      } else {
        cql_error("--java_assembly_query_classname requires an additional assembly query classname for extension fragment codegen\n");
        cql_cleanup_and_exit(1);
      }
    } else {
      cql_error("unknown arg '%s'\n", argv[a]);
      cql_cleanup_and_exit(1);
    }
  }

  if (options.codegen && options.rt && (rt->required_file_names_count != options.file_names_count && rt->required_file_names_count != -1)) {
    fprintf(stderr,
            "--rt %s requires %" PRId32 " files for --cg, but received %" PRId32 "\n",
            options.rt,
            rt->required_file_names_count,
            options.file_names_count);
    cql_cleanup_and_exit(1);
  }

  if (options.cqlrt) {
    rt->cqlrt = options.cqlrt;
  }
}

#ifndef CQL_IS_NOT_MAIN
  // Normally CQL is the main entry point.  If you are using CQL in an embedded fashion
  // then you want to invoke its main at some other time. If you define CQL_IS_NOT_MAIN
  // then cql_main is not renamed to main.  You call cql_main when you want.
  #define cql_main main
#endif

// In order for leak sanitizer to run, main must exit normally
// and yet there are cases we need to bail out like we would in exit
// to do that we use cql_cleanup_and_exit below which triggers this longjmp
// here in main.  CQL aspires to be a library in the future and so
// it cannot exit in those cases either it has to clean up, clean.

static jmp_buf for_exit;
static int32_t exit_code;
static int32_t manual_syntax_errors;

int cql_main(int argc, char **argv) {
  manual_syntax_errors = 0;
  exit_code = 0;
  yylineno = 1;

  if (!setjmp(for_exit)) {
    parse_cmd(argc, argv);
    ast_init();

    if (options.run_unit_tests) {
      run_unit_tests();
    } else if (yyparse() || manual_syntax_errors) {
      cql_error("\nParse errors found, no further passes will run.\n");
      cql_cleanup_and_exit(2);
    }
  }

  cg_c_cleanup();
  sem_cleanup();
  ast_cleanup();
  gen_cleanup();
  rt_cleanup();

#ifdef CQL_AMALGAM
  // the variables need to be set back to zero so we can
  // be called again as though we were just loaded
  cql_reset_globals();
#endif

  return exit_code;
}

#undef cql_main

// Use the longjmp buffer with the indicated code, see the comments above
// for why this has to be this way.  Note we do this in one line so that
// we don't get bogus code coverage errors for not covering the trialing brace
void cql_cleanup_and_exit(int32_t code) { exit_code = code; longjmp(for_exit, 1); }

static int32_t gather_arg_params(int32_t a, int32_t argc, char **argv, int *out_count, char ***out_args) {
  if (a + 1 < argc) {
    a++;
    *out_args = &argv[a];
    *out_count = 1;
    while ((a + 1 < argc) && (strncmp("--", argv[a + 1], 2) != 0)) {
      a++;
      (*out_count)++;
    }
  } else {
    cql_error("%s requires additional arguments.\n", argv[a]);
    exit(1);
  }

  return a;
}

extern int yylineno;

void line_directive(const char *directive) {
  Invariant(directive[0] == '#');
  int line = atoi(directive + 1);
  yyset_lineno(line -1);  // we are about to process the linefeed

  char *q1 = strchr(directive +1, '"');
  if (!q1) return;
  char *q2 = strchr(q1+1, '"');
  if (!q2) return;

  CHARBUF_OPEN(temp);
  cg_decode_c_string_literal(q1, &temp);
  current_file = Strdup(temp.ptr);
  CHARBUF_CLOSE(temp);

  // we don't free the current file because it is used in the trees alongside lineno
  // free(current_file);  Don't do this.
}

// Make a string literal node based on the current file
// the node includes a search term, the literal begins at the pattern
// that is present.  So if the current dir is /var/foo/bar/baz/YourProjectRoot
// you can start at YourProjectRoot easily.
static ast_node *file_literal(ast_node *ast) {
  CHARBUF_OPEN(filter);
  EXTRACT_STRING(str, ast);
  cg_decode_string_literal(str, &filter);

  const char *p = strstr(current_file, filter.ptr);
  if (!p) {
    p = current_file;
  }

  CHARBUF_OPEN(literal);
  cg_encode_string_literal(p, &literal);
  ast_node *ret = new_ast_str(Strdup(literal.ptr));
  CHARBUF_CLOSE(literal);

  CHARBUF_CLOSE(filter);
  return ret;
}

#ifndef cql_emit_error

// CQL "stderr" outputs are emitted with this API
// You can refine it to be a method of your choice with
// "#define cql_emit_error your_method" and then your method will get
// the data instead. This will be whatever output the
// compiler would have emitted to to stderr.  This includes compiler
// errors or invalid argument combinations.  Note that CQL never
// emits error fragments with this API, you always get all the text of
// one error.  This is important if you are filtering or looking for
// particular errors in a test harness or some such.
// You must copy the memory if you intend to keep it. "data" will be freed.

// Note: you may use cql_cleanup_and_exit to force a failure from within
// this API but doing so might result in unexpected cleanup paths that have
// not been tested.

void cql_emit_error(const char *err) {
  fprintf(stderr, "%s", err);
  if (error_capture) {
    bprintf(error_capture, "%s", err);
  }
}

#endif

#ifndef cql_emit_output

// CQL "stdout" outputs are emitted (in arbitrarily small pieces) with this API
// You can refine it to be a method of your choice with
// "#define cql_emit_output your_method" and then your method will get
// the data instead. This will be whatever output the
// compiler would have emitted to to stdout.  This usually
// reformated CQL or semantic trees and such -- not the normal compiler output.
// You must copy the memory if you intend to keep it. "data" will be freed.

// Note: you may use cql_cleanup_and_exit to force a failure from within
// this API but doing so might result in unexpected cleanup paths that have
// not been tested.

void cql_emit_output(const char *msg) {
  printf("%s", msg);
}

#endif

// Perform the formatting and then call cql_emit_error (which may be replaced)
// The point of all this is so that cql can have a printf-like error API but
// if you are trying to integrate with CQL you only have to handle the much
// simpler cql_emit_error API.

void cql_error(const char *format, ...)  {
  va_list args;
  va_start(args, format);
  CHARBUF_OPEN(err);
  vbprintf(&err, format, args);
  cql_emit_error(err.ptr);
  CHARBUF_CLOSE(err);
  va_end(args);
}

// Perform the formatting and the call cql_emit_output (which may be replaced)
// The point of all this is so that cql can have a printf-like output API but
// if you are trying to integrate with CQL you only have to handle the much
// simple cql_emit_output API.

void cql_output(const char *format, ...)  {
  va_list args;
  va_start(args, format);
  CHARBUF_OPEN(err);
  vbprintf(&err, format, args);
  cql_emit_output(err.ptr);
  CHARBUF_CLOSE(err);
  va_end(args);
}

#ifndef cql_open_file_for_write

// Not a normal integration point, the normal thing to do is replace cql_write_file
// but if all you need to do is adjust the path or something like that you could replace
// this method instead.  This presumes that a FILE * is still ok for your scenario.

FILE *_Nonnull cql_open_file_for_write(CSTR _Nonnull file_name) {
  FILE *file;
  if (!(file = fopen(file_name, "w"))) {
    cql_error("unable to open %s for write\n", file_name);
    cql_cleanup_and_exit(1);
  }
  return file;
}

#endif

#ifndef cql_write_file

// CQL code generation outputs are emitted in one "gulp" with this API
// You can refine it to be a method of your choice with
// "#define cql_write_file your_method" and then your method will get
// the filename and the data. This will be whatever output the
// compiler would have emitted to one of it's --cg arguments. You can
// then write it somewhere or duplicate the memory or whatever.  You
// must copy the memory if you intend to keep it. "data" will be freed.

// Note: you *may* use cql_cleanup_and_exit to force a failure from within
// this API.  That's a normal failure mode that is well-tested.

void cql_write_file(CSTR _Nonnull file_name, CSTR _Nonnull data) {
  FILE *file = cql_open_file_for_write(file_name);
  fprintf(file, "%s", data);
  fclose(file);
}

#endif

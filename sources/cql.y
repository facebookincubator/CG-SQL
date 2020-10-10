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

%}

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
%token CREATE DROP TABLE WITHOUT ROWID PRIMARY KEY NULL_ DEFAULT AT_DUMMY_SEED
%token OBJECT TEXT BLOB LONG_ INT_ INTEGER LONG_INTEGER REAL ON UPDATE CASCADE ON_CONFLICT DO NOTHING
%token DELETE INDEX FOREIGN REFERENCES CONSTRAINT UPSERT STATEMENT
%token INSERT INTO VALUES VIEW SELECT QUERY_PLAN EXPLAIN OVER WINDOW FILTER PARTITION RANGE ROWS GROUPS
%token AS CASE WHEN FROM THEN ELSE END LEFT
%token OUTER JOIN WHERE GROUP BY ORDER ASC
%token DESC INNER FCOUNT AUTOINCREMENT DISTINCT
%token LIMIT OFFSET TEMP TRIGGER IF ALL CROSS USING RIGHT
%token UNIQUE HAVING SET TO DISTINCTROW
%token FUNC FUNCTION PROC PROCEDURE BEGIN_ OUT INOUT CURSOR CURSOR_FOR DECLARE FETCH LOOP LEAVE CONTINUE
%token OPEN CLOSE ELSE_IF WHILE CALL TRY CATCH THROW RETURN
%token SAVEPOINT ROLLBACK COMMIT TRANSACTION RELEASE ARGUMENTS
%token CAST WITH RECURSIVE REPLACE IGNORE ADD COLUMN RENAME ALTER
%token AT_ECHO AT_CREATE AT_RECREATE AT_DELETE AT_SCHEMA_UPGRADE_VERSION AT_PREVIOUS_SCHEMA AT_SCHEMA_UPGRADE_SCRIPT
%token AT_FILE AT_ATTRIBUTE AT_SENSITIVE DEFERRED NOT_DEFERRABLE DEFERRABLE IMMEDIATE RESTRICT ACTION INITIALLY NO
%token BEFORE AFTER INSTEAD OF FOR_EACH_ROW EXISTS RAISE FAIL ABORT AT_ENFORCE_STRICT AT_ENFORCE_NORMAL
%token AT_BEGIN_SCHEMA_REGION AT_END_SCHEMA_REGION
%token AT_DECLARE_SCHEMA_REGION AT_DECLARE_DEPLOYABLE_REGION AT_SCHEMA_AD_HOC_MIGRATION PRIVATE

/* ddl stuff */
%type <ival> opt_temp opt_if_not_exists opt_unique opt_no_rowid dummy_modifier compound_operator opt_query_plan
%type <ival> opt_fk_options fk_options fk_on_options fk_action fk_initial_state fk_deferred_options
%type <ival> frame_type frame_exclude

%type <aval> col_key_list col_key_def col_def col_name
%type <aval> version_attrs opt_version_attrs version_attrs_opt_recreate opt_delete_version_attr
%type <aval> misc_attr_key misc_attr misc_attrs misc_attr_value misc_attr_value_list
%type <aval> col_attrs str_literal num_literal any_literal
%type <aval> pk_def fk_def unq_def fk_target_options

%type <aval> alter_table_add_column_stmt
%type <aval> create_index_stmt create_table_stmt create_view_stmt
%type <aval> indexed_column indexed_columns
%type <aval> drop_index_stmt drop_table_stmt drop_view_stmt drop_trigger_stmt

%type <aval> trigger_update_stmt trigger_delete_stmt trigger_insert_stmt trigger_select_stmt
%type <aval> trigger_stmt trigger_stmts opt_when_expr trigger_action opt_of
%type <aval> trigger_def trigger_operation create_trigger_stmt raise_expr
%type <ival> trigger_condition opt_foreachrow

/* dml stuff */
%type <aval> with_delete_stmt delete_stmt
%type <aval> insert_stmt with_insert_stmt insert_list insert_stmt_type opt_column_spec opt_insert_dummy_spec
%type <aval> with_prefix with_select_stmt cte_decl cte_table cte_tables
%type <aval> select_expr select_expr_list select_opts select_stmt select_core values explain_stmt explain_target
%type <aval> select_stmt_no_with select_core_compound select_core_list
%type <aval> window_func_inv opt_filter_clause window_name_or_defn window_defn opt_select_window
%type <aval> opt_partition_by opt_frame_spec frame_boundary_opts frame_boundary_start frame_boundary_end frame_boundary
%type <aval> opt_where opt_groupby opt_having opt_orderby opt_limit opt_offset opt_as_alias window_clause
%type <aval> groupby_item groupby_list opt_asc_desc window_name_defn window_name_defn_list
%type <aval> table_or_subquery table_or_subquery_list query_parts table_function opt_from_query_parts
%type <aval> opt_join_cond opt_outer join_cond join_clause join_target join_target_list opt_inner_cross left_or_right
%type <aval> basic_update_stmt with_update_stmt update_stmt update_cursor_stmt update_entry update_list upsert_stmt conflict_target
%type <aval> declare_schema_region_stmt declare_deployable_region_stmt call with_upsert_stmt
%type <aval> begin_schema_region_stmt end_schema_region_stmt schema_ad_hoc_migration_stmt region_list region_spec from_arguments

/* expressions and types */
%type <aval> expr basic_expr math_expr expr_list typed_name typed_names case_list call_expr_list call_expr cursor_arguments
%type <aval> name name_list opt_name_list opt_name
%type <aval> data_type data_type_opt_notnull creation_type object_type

/* proc stuff */
%type <aval> create_proc_stmt declare_func_stmt declare_proc_stmt
%type <aval> arg_expr arg_list opt_inout param params

/* statements */
%type <aval> stmt
%type <aval> stmt_list opt_stmt_list
%type <aval> any_stmt
%type <aval> begin_trans_stmt
%type <aval> call_stmt
%type <aval> close_stmt
%type <aval> commit_trans_stmt
%type <aval> continue_stmt
%type <aval> declare_stmt
%type <aval> echo_stmt
%type <aval> fetch_stmt fetch_values_stmt fetch_call_stmt fetch_cursor_stmt from_cursor
%type <aval> if_stmt elseif_item elseif_list opt_else opt_elseif_list
%type <aval> leave_stmt return_stmt
%type <aval> loop_stmt
%type <aval> open_stmt
%type <aval> out_stmt out_union_stmt
%type <aval> previous_schema_stmt
%type <aval> release_savepoint_stmt
%type <aval> rollback_trans_stmt
%type <aval> savepoint_stmt
%type <aval> schema_upgrade_script_stmt
%type <aval> schema_upgrade_version_stmt
%type <aval> set_stmt
%type <aval> throw_stmt
%type <aval> trycatch_stmt
%type <aval> version_annotation
%type <aval> while_stmt
%type <aval> enforce_strict_stmt enforce_normal_stmt enforcement_options

%start program

%%

program: opt_stmt_list {
    gen_init();
    if (options.semantic) {
      sem_main($1);
    }
    if (options.codegen) {
      rt->code_generator($1);
    }
    else if (options.print_ast) {
      print_ast($1, NULL, 0, 0);
      cql_output("\n");
    } else if (options.print_dot) {
      cql_output("\ndigraph parse {");
      print_dot($1);
      cql_output("\n}\n");
    }
    else {
      gen_stmt_list_to_stdout($1);
    }
    if (options.semantic) {
      cql_exit_on_semantic_errors($1);
    }
  }
  ;

opt_stmt_list: /*nil*/ { $$ = NULL; }
  | stmt_list { $$ = $1; }

stmt_list: stmt ';' { $$ = new_ast_stmt_list($1, NULL); $$->lineno = $1->lineno;}
  | stmt ';' stmt_list { $$ = new_ast_stmt_list($1, $3); $$->lineno = $1->lineno; }
  ;

stmt: misc_attrs any_stmt { $$ = $1 ? new_ast_stmt_and_attr($1, $2) : $2; }

any_stmt: select_stmt
  | explain_stmt
  | create_trigger_stmt
  | create_table_stmt
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
  | fetch_stmt
  | fetch_values_stmt
  | fetch_call_stmt
  | fetch_cursor_stmt
  | while_stmt
  | loop_stmt
  | leave_stmt
  | return_stmt
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
  ;

explain_stmt: EXPLAIN opt_query_plan explain_target { $$ = new_ast_explain_stmt(new_ast_opt($2), $3); }
  ;

opt_query_plan: /* nil */ { $$ = EXPLAIN_NONE; }
  | QUERY_PLAN { $$ = EXPLAIN_QUERY_PLAN; }
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

previous_schema_stmt: AT_PREVIOUS_SCHEMA { $$ = new_ast_previous_schema_stmt(); }
  ;

schema_upgrade_script_stmt: AT_SCHEMA_UPGRADE_SCRIPT { $$ = new_ast_schema_upgrade_script_stmt(); }
  ;

schema_upgrade_version_stmt: AT_SCHEMA_UPGRADE_VERSION '(' INTLIT ')' {
    $$ = new_ast_schema_upgrade_version_stmt(new_ast_opt(atoi($3))); }
  ;

set_stmt: SET name ASSIGN expr   { $$ = new_ast_assign($2, $4); }
  ;

version_attrs_opt_recreate: /* nil */ { $$ = NULL; }
  | AT_RECREATE { $$ = new_ast_recreate_attr(NULL); }
  | AT_RECREATE '(' name ')'  { $$ = new_ast_recreate_attr($3); }
  | version_attrs { $$ = $1; }
  ;

opt_version_attrs: /* nil */ { $$ = NULL; }
  | version_attrs { $$ = $1; }
  ;

version_attrs: AT_CREATE version_annotation opt_version_attrs { $$ = new_ast_create_attr($2, $3); }
  | AT_DELETE version_annotation opt_version_attrs { $$ = new_ast_delete_attr($2, $3); }
  ;

opt_delete_version_attr: /* nil */ {$$ = NULL; }
  | AT_DELETE version_annotation { $$ = new_ast_delete_attr($2, NULL); }
  ;

drop_table_stmt: DROP TABLE IF EXISTS name { $$ = new_ast_drop_table_stmt(new_ast_opt(1), $5);  }
  | DROP TABLE name { $$ = new_ast_drop_table_stmt(NULL, $3);  }
  ;

drop_view_stmt: DROP VIEW IF EXISTS name { $$ = new_ast_drop_view_stmt(new_ast_opt(1), $5);  }
  | DROP VIEW name { $$ = new_ast_drop_view_stmt(NULL, $3);  }
  ;

drop_index_stmt: DROP INDEX IF EXISTS name { $$ = new_ast_drop_index_stmt(new_ast_opt(1), $5);  }
  | DROP INDEX name { $$ = new_ast_drop_index_stmt(NULL, $3);  }
  ;

drop_trigger_stmt: DROP TRIGGER IF EXISTS name { $$ = new_ast_drop_trigger_stmt(new_ast_opt(1), $5);  }
  | DROP TRIGGER name { $$ = new_ast_drop_trigger_stmt(NULL, $3);  }
  ;

create_table_stmt: CREATE opt_temp TABLE opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate {
    int flags = $2 | $4 | $9;
    struct ast_node *flags_node = new_ast_opt(flags);
    struct ast_node *name = $5;
    struct ast_node *col_key_list = $7;
    struct ast_node *table_flags_attrs = new_ast_table_flags_attrs(flags_node, $10);
    struct ast_node *table_name_flags = new_ast_create_table_name_flags(table_flags_attrs, name);
    $$ =  new_ast_create_table_stmt(table_name_flags, col_key_list);
  }
  ;

opt_temp: /* nil */ { $$ = 0; }
  | TEMP { $$ = GENERIC_IS_TEMP; }
  ;

opt_if_not_exists: /* nil */ { $$ = 0;  }
  | IF NOT EXISTS { $$ = GENERIC_IF_NOT_EXISTS; }
  ;

opt_no_rowid: /* nil */ { $$ = 0; }
  | WITHOUT ROWID { $$ = TABLE_IS_NO_ROWID; }
  ;

col_key_list: col_key_def { $$ = new_ast_col_key_list($1, NULL); }
  | col_key_def ',' col_key_list  { $$ = new_ast_col_key_list($1, $3); }
  ;

col_key_def: col_def
  | pk_def
  | fk_def
  | unq_def
  | LIKE name { $$ = new_ast_like($2, NULL); }
  ;

col_name: name { $$ = $1; }
  ;

misc_attr_key: name { $$ = $1; }
  | name ':' name { $$ = new_ast_dot($1, $3); }
  ;

misc_attr_value_list: misc_attr_value { $$ = new_ast_misc_attr_value_list($1, NULL); }
  | misc_attr_value ',' misc_attr_value_list { $$ = new_ast_misc_attr_value_list($1, $3); }
  ;

misc_attr_value: name { $$ = $1; }
  | any_literal { $$ = $1; }
  | '(' misc_attr_value_list ')' { $$ = $2; }
  | '-' num_literal { $$ = new_ast_uminus($2);}
  ;

misc_attr:  AT_ATTRIBUTE '(' misc_attr_key ')' { $$ = new_ast_misc_attr($3, NULL); }
  | AT_ATTRIBUTE '(' misc_attr_key '=' misc_attr_value ')' { $$ = new_ast_misc_attr($3, $5); }
  ;

misc_attrs: /* nil */ { $$ = NULL; }
  | misc_attr misc_attrs { $$ = new_ast_misc_attrs($1, $2); }
  ;

col_def: misc_attrs col_name data_type col_attrs {
  struct ast_node *misc_attrs = $1;
  struct ast_node *col_name = $2;
  struct ast_node *data_type = $3;
  struct ast_node *col_attrs= $4;
  struct ast_node *name_type = new_ast_col_def_name_type(col_name, data_type);
  struct ast_node *col_def_type_attrs = new_ast_col_def_type_attrs(name_type, col_attrs);
  $$ = new_ast_col_def(col_def_type_attrs, misc_attrs);
  }
  ;

pk_def: PRIMARY KEY '(' name_list ')'  { $$ = new_ast_pk_def($4);}
  ;

opt_fk_options: /* nil */ { $$ = 0; }
  | fk_options { $$ = $1; }
  ;

fk_options: fk_on_options { $$ = $1; }
  | fk_deferred_options { $$ = $1; }
  | fk_on_options fk_deferred_options  { $$ = $1 | $2; }
  ;

fk_on_options:
    ON DELETE fk_action { $$ = $3; }
  | ON UPDATE fk_action { $$ = ($3 << 4); }
  | ON UPDATE fk_action ON DELETE fk_action { $$ = ($3 << 4) | $6; }
  | ON DELETE fk_action ON UPDATE fk_action { $$ = ($6 << 4) | $3; }
  ;

fk_action:
    SET NULL_ { $$ = FK_SET_NULL; }
  | SET DEFAULT { $$ = FK_SET_DEFAULT; }
  | CASCADE { $$ = FK_CASCADE; }
  | RESTRICT { $$ = FK_RESTRICT; }
  | NO ACTION { $$ = FK_NO_ACTION; }
  ;

fk_deferred_options:
    DEFERRABLE fk_initial_state { $$ = FK_DEFERRABLE | $2; }
  | NOT_DEFERRABLE fk_initial_state { $$ = FK_NOT_DEFERRABLE | $2; }
  ;

fk_initial_state: /* nil */ { $$ = 0; }
  | INITIALLY DEFERRED { $$ = FK_INITIALLY_DEFERRED; }
  | INITIALLY IMMEDIATE { $$ = FK_INITIALLY_IMMEDIATE; }
  ;

fk_def: FOREIGN KEY '(' name_list ')' fk_target_options { $$ = new_ast_fk_def($4, $6); }
  ;

fk_target_options : REFERENCES name '(' name_list ')' opt_fk_options {
    $$ = new_ast_fk_target_options(new_ast_fk_target($2, $4), new_ast_opt($6)); }
  ;

unq_def: CONSTRAINT name UNIQUE '(' name_list ')' { $$ = new_ast_unq_def($2, $5); }
  | UNIQUE '(' name_list ')' { $$ = new_ast_unq_def(NULL, $3); }
  ;

opt_unique: /* nil */ { $$ = 0; }
  | UNIQUE { $$ = 1; }
  ;

indexed_column: name opt_asc_desc { $$ = new_ast_indexed_column($1, $2); }
  ;

indexed_columns: indexed_column { $$ = new_ast_indexed_columns($1, NULL); }
  | indexed_column ',' indexed_columns { $$ = new_ast_indexed_columns($1, $3); }
  ;

create_index_stmt: CREATE opt_unique INDEX opt_if_not_exists name ON name '(' indexed_columns ')' opt_delete_version_attr {
    int flags = 0;
    if ($2) flags |= INDEX_UNIQUE;
    if ($4) flags |= INDEX_IFNE;

    ast_node *create_index_on_list = new_ast_create_index_on_list($5, $7);
    ast_node *index_names_and_attrs = new_ast_index_names_and_attrs($9, $11);
    ast_node *flags_names_attrs = new_ast_flags_names_attrs(new_ast_opt(flags), index_names_and_attrs);
    $$ = new_ast_create_index_stmt(create_index_on_list, flags_names_attrs);
  }
  ;

name: ID { $$ = new_ast_str($1); }
  | TEXT { $$ = new_ast_str("text"); }
  | TRIGGER { $$ = new_ast_str("trigger"); }
  | ROWID { $$ = new_ast_str("rowid"); }
  ;

opt_name: /* nil */ { $$ = NULL; }
  | name { $$ = $1; }
  ;

name_list: name { $$ = new_ast_name_list($1, NULL); }
  |  name ',' name_list  { $$ = new_ast_name_list($1, $3); }
  ;

opt_name_list: /* nil */ { $$ = NULL; }
  | name_list {$$ = $1; }
  ;

col_attrs: /* nil */ { $$ = NULL; }
  | NOT NULL_ col_attrs { $$ = new_ast_col_attrs_not_null(NULL, $3); }
  | PRIMARY KEY col_attrs { $$ = new_ast_col_attrs_pk(NULL, $3); }
  | PRIMARY KEY AUTOINCREMENT col_attrs { $$ = new_ast_col_attrs_pk(new_ast_col_attrs_autoinc(), $4);}
  | DEFAULT '-' num_literal col_attrs { $$ = new_ast_col_attrs_default(new_ast_uminus($3), $4);}
  | DEFAULT num_literal col_attrs { $$ = new_ast_col_attrs_default($2, $3);}
  | DEFAULT str_literal col_attrs { $$ = new_ast_col_attrs_default($2, $3);}
  | UNIQUE col_attrs { $$ = new_ast_col_attrs_unique(NULL, $2);}
  | AT_SENSITIVE col_attrs { $$ = new_ast_sensitive_attr(NULL, $2); }
  | AT_CREATE version_annotation col_attrs { $$ = new_ast_create_attr($2, $3);}
  | AT_DELETE version_annotation col_attrs { $$ = new_ast_delete_attr($2, $3);}
  | fk_target_options col_attrs { $$ = new_ast_col_attrs_fk($1, $2); }
  ;

version_annotation: '(' INTLIT ',' name ')' {
    $$ = new_ast_version_annotation(new_ast_opt(atoi($2)), $4); }
  | '(' INTLIT ')' {
    $$ = new_ast_version_annotation(new_ast_opt(atoi($2)), NULL); }
  ;

object_type:
    OBJECT { $$ = new_ast_type_object(NULL); }
  | OBJECT '<' name '>' { $$ = new_ast_type_object($3); }
  ;

data_type:
    INT_ { $$ = new_ast_type_int(); }
  | INTEGER { $$ = new_ast_type_int(); }
  | TEXT { $$ = new_ast_type_text();  }
  | REAL { $$ = new_ast_type_real(); }
  | LONG_ { $$ = new_ast_type_long(); }
  | BOOL_ { $$ = new_ast_type_bool(); }
  | LONG_ INTEGER { $$ = new_ast_type_long(); }
  | LONG_ INT_ { $$ = new_ast_type_long(); }
  | LONG_INTEGER { $$ = new_ast_type_long(); }
  | BLOB { $$ = new_ast_type_blob(); }
  | object_type { $$ = $1; }
  ;

data_type_opt_notnull: data_type { $$ = $1; }
  | data_type NOT NULL_ { $$ = new_ast_notnull($1); }
  | data_type AT_SENSITIVE { $$ = new_ast_sensitive_attr($1, NULL); }
  | data_type AT_SENSITIVE NOT NULL_ { $$ = new_ast_sensitive_attr(new_ast_notnull($1), NULL); }
  | data_type NOT NULL_ AT_SENSITIVE  { $$ = new_ast_sensitive_attr(new_ast_notnull($1), NULL); }
  ;

str_literal: STRLIT { $$ = new_ast_str($1);}
  | CSTRLIT { $$ = new_ast_cstr($1); }
  ;

num_literal:  INTLIT { $$ = new_ast_num(NUM_INT, $1); }
  | LONGLIT { $$ = new_ast_num(NUM_LONG, $1); }
  | REALLIT { $$ = new_ast_num(NUM_REAL, $1); }
  ;

any_literal: str_literal { $$ = $1; }
  | num_literal { $$ = $1; }
  | NULL_ { $$ = new_ast_null(); }
  | AT_FILE '(' str_literal ')' { $$ = file_literal($3); }
  | BLOBLIT { $$ = new_astb($1); }
  ;

raise_expr:
    RAISE '(' IGNORE ')'  { $$ = new_ast_raise(new_ast_opt(RAISE_IGNORE), NULL); }
  | RAISE '(' ROLLBACK ','  expr ')' { $$ = new_ast_raise(new_ast_opt(RAISE_ROLLBACK), $5); }
  | RAISE '(' ABORT ','  expr ')' { $$ = new_ast_raise(new_ast_opt(RAISE_ABORT), $5); }
  | RAISE '(' FAIL ','  expr ')' { $$ = new_ast_raise(new_ast_opt(RAISE_FAIL), $5); }
  ;

call:
    name '(' arg_list ')' opt_filter_clause {
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, $5);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $3);
      $$ = new_ast_call($1, call_arg_list);
  }
  | name '(' DISTINCT arg_list ')' opt_filter_clause {
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(new_ast_distinct(), $6);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $4);
      $$ = new_ast_call($1, call_arg_list);
  }
  ;

basic_expr: name { $$ = $1; }
  | name '.' name { $$ = new_ast_dot($1, $3); }
  | any_literal { $$ = $1; }
  | '(' expr ')' { $$ = $2; }
  | call { $$ = $1; }
  | window_func_inv { $$ = $1; }
  | raise_expr { $$ = $1; }
  | '(' select_stmt ')' { $$ = $2; }
  | EXISTS '(' select_stmt ')' { $$ = new_ast_exists_expr($3); }
  ;

math_expr: basic_expr { $$ = $1; }
  | math_expr '&' math_expr { $$ = new_ast_bin_and($1, $3); }
  | math_expr '|' math_expr { $$ = new_ast_bin_or($1, $3); }
  | math_expr LS math_expr { $$ = new_ast_lshift($1, $3); }
  | math_expr RS  math_expr { $$ = new_ast_rshift($1, $3); }
  | math_expr '+' math_expr { $$ = new_ast_add($1, $3); }
  | math_expr '-' math_expr { $$ = new_ast_sub($1, $3); }
  | math_expr '*' math_expr { $$ = new_ast_mul($1, $3); }
  | math_expr '/' math_expr { $$ = new_ast_div($1, $3); }
  | math_expr '%' math_expr { $$ = new_ast_mod($1, $3); }
  | '-' math_expr %prec UMINUS { $$ = new_ast_uminus($2); }
  | math_expr CONCAT math_expr { $$ = new_ast_concat($1, $3); }
  ;

expr: basic_expr { $$ = $1; }
  | expr '&' expr { $$ = new_ast_bin_and($1, $3); }
  | expr '|' expr { $$ = new_ast_bin_or($1, $3); }
  | expr LS expr { $$ = new_ast_lshift($1, $3); }
  | expr RS expr { $$ = new_ast_rshift($1, $3); }
  | expr '+' expr { $$ = new_ast_add($1, $3); }
  | expr '-' expr { $$ = new_ast_sub($1, $3); }
  | expr '*' expr { $$ = new_ast_mul($1, $3); }
  | expr '/' expr { $$ = new_ast_div($1, $3); }
  | expr '%' expr { $$ = new_ast_mod($1, $3); }
  | '-' expr %prec UMINUS { $$ = new_ast_uminus($2); }
  | NOT expr { $$ = new_ast_not($2); }
  | '~' expr { $$ = new_ast_tilde($2); }
  | expr COLLATE name { $$ = new_ast_collate($1, $3); }
  | expr AND expr { $$ = new_ast_and($1, $3); }
  | expr OR expr { $$ = new_ast_or($1, $3); }
  | expr '=' expr { $$ = new_ast_eq($1, $3); }
  | expr EQEQ expr { $$ = new_ast_eq($1, $3); }
  | expr '<' expr { $$ = new_ast_lt($1, $3); }
  | expr '>' expr { $$ = new_ast_gt($1, $3); }
  | expr NE expr { $$ = new_ast_ne($1, $3); }
  | expr NE_ expr { $$ = new_ast_ne($1, $3); }
  | expr GE expr { $$ = new_ast_ge($1, $3); }
  | expr LE expr { $$ = new_ast_le($1, $3); }
  | expr NOT IN '(' expr_list ')' { $$ = new_ast_not_in($1, $5); }
  | expr NOT IN '(' select_stmt ')' { $$ = new_ast_not_in($1, $5); }
  | expr IN '(' expr_list ')' { $$ = new_ast_in_pred($1, $4); }
  | expr IN '(' select_stmt ')' { $$ = new_ast_in_pred($1, $4); }
  | expr LIKE expr { $$ = new_ast_like($1, $3); }
  | expr NOT_LIKE expr { $$ = new_ast_not_like($1, $3); }
  | expr MATCH expr { $$ = new_ast_match($1, $3); }
  | expr REGEXP expr { $$ = new_ast_regexp($1, $3); }
  | expr GLOB expr { $$ = new_ast_glob($1, $3); }
  | expr NOT BETWEEN math_expr AND math_expr  { $$ = new_ast_not_between($1, new_ast_range($4,$6)); }
  | expr BETWEEN math_expr AND math_expr  { $$ = new_ast_between($1, new_ast_range($3,$5)); }
  | expr IS_NOT expr { $$ = new_ast_is_not($1, $3); }
  | expr IS expr { $$ = new_ast_is($1, $3); }
  | expr CONCAT expr { $$ = new_ast_concat($1, $3); }
  | CASE expr case_list END { $$ = new_ast_case_expr($2, new_ast_connector($3, NULL)); }
  | CASE expr case_list ELSE expr END { $$ = new_ast_case_expr($2, new_ast_connector($3, $5));}
  | CASE case_list END { $$ = new_ast_case_expr(NULL, new_ast_connector($2, NULL));}
  | CASE case_list ELSE expr END { $$ = new_ast_case_expr(NULL, new_ast_connector($2, $4));}
  | CAST '(' expr AS data_type ')' { $$ = new_ast_cast_expr($3, $5); }
  ;

case_list: WHEN expr THEN expr { $$ = new_ast_case_list(new_ast_when($2, $4), NULL); }
  | WHEN expr THEN expr case_list { $$ = new_ast_case_list(new_ast_when($2, $4), $5);}
  ;

arg_expr: '*' { $$ = new_ast_star(); }
  | expr { $$ = $1; }
  | cursor_arguments { $$ = $1; }
  | from_arguments { $$ = $1; }
  ;

arg_list: /* nil */ { $$ = NULL; }
  | arg_expr { $$ = new_ast_arg_list($1, NULL); }
  | arg_expr ',' arg_list { $$ = new_ast_arg_list($1, $3); }
  ;

expr_list: expr { $$ = new_ast_expr_list($1, NULL); }
  | expr ',' expr_list { $$ = new_ast_expr_list($1, $3); }
  ;

cursor_arguments : FROM name { $$ = new_ast_from_cursor($2, NULL); }
  | FROM name LIKE name { $$ = new_ast_from_cursor($2, $4); }
  ;

call_expr: expr { $$ = $1; }
  | cursor_arguments { $$ = $1; }
  | from_arguments { $$ = $1; }
  ;

call_expr_list: call_expr { $$ = new_ast_expr_list($1, NULL); }
  | call_expr ',' call_expr_list { $$ = new_ast_expr_list($1, $3); }
  ;

cte_tables:  cte_table { $$ = new_ast_cte_tables($1, NULL); }
  | cte_table ',' cte_tables { $$ = new_ast_cte_tables($1, $3); }
  ;

cte_table: cte_decl AS '(' select_stmt_no_with ')' { $$ = new_ast_cte_table($1, $4); }
  ;

cte_decl: name '(' name_list ')' { $$ = new_ast_cte_decl($1, $3); }
  | name '(' '*' ')' { $$ = new_ast_cte_decl($1, new_ast_star()); }
  ;

with_prefix: WITH cte_tables { $$ = new_ast_with($2); }
  | WITH RECURSIVE cte_tables { $$ = new_ast_with_recursive($3); }
  ;

with_select_stmt: with_prefix select_stmt_no_with { $$ = new_ast_with_select_stmt($1, $2); }
  ;

select_stmt: with_select_stmt { $$ = $1; }
  | select_stmt_no_with { $$ = $1; }
  ;

select_stmt_no_with: select_core_list opt_orderby opt_limit opt_offset {
      struct ast_node *select_offset = new_ast_select_offset($4, NULL);
      struct ast_node *select_limit = new_ast_select_limit($3, select_offset);
      struct ast_node *select_orderby = new_ast_select_orderby($2, select_limit);
       $$ = new_ast_select_stmt($1, select_orderby);
  }
  ;

select_core_list: select_core { $$ = new_ast_select_core_list($1, NULL); }
  | select_core select_core_compound { $$ = new_ast_select_core_list($1, $2); }
  ;

select_core_compound: compound_operator select_core_list {
    $$ = new_ast_select_core_compound(new_ast_opt($1), $2);
  }
  ;


values: '(' insert_list ')' {
    $$ = new_ast_values($2, NULL);
  }
  | '(' insert_list ')' ',' values {
    $$ = new_ast_values($2, $5);
  }
  ;

select_core: SELECT select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window {
    struct ast_node *select_having = new_ast_select_having($7, $8);
    struct ast_node *select_groupby = new_ast_select_groupby($6, select_having);
    struct ast_node *select_where = new_ast_select_where($5, select_groupby);
    struct ast_node *select_from_etc = new_ast_select_from_etc($4, select_where);
    struct ast_node *select_expr_list_con = new_ast_select_expr_list_con($3, select_from_etc);
     $$ = new_ast_select_core($2, select_expr_list_con);
  }
  | VALUES values {
    $$ = new_ast_select_core(new_ast_select_values(), $2);
  }
  ;

compound_operator:
    UNION { $$ = COMPOUND_OP_UNION; }
  | UNION_ALL { $$ = COMPOUND_OP_UNION_ALL; }
  | INTERSECT { $$ = COMPOUND_OP_INTERSECT; }
  | EXCEPT { $$ = COMPOUND_OP_EXCEPT; }
  ;

window_func_inv: name '(' arg_list ')' opt_filter_clause OVER window_name_or_defn {
    struct ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, $5);
    struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $3);
    struct ast_node *win_func_call = new_ast_call($1, call_arg_list);
    $$ = new_ast_window_func_inv(win_func_call, $7);
  }
  ;

opt_filter_clause: /* nil */ { $$ = NULL; }
  | FILTER '(' opt_where ')' { $$ = new_ast_opt_filter_clause($3); }
  ;

window_name_or_defn: window_defn
  | name
  ;

window_defn: '(' opt_partition_by opt_orderby opt_frame_spec ')' {
    ast_node *window_defn_orderby = new_ast_window_defn_orderby($3, $4);
    $$ = new_ast_window_defn($2, window_defn_orderby);
  }
  ;

opt_frame_spec: /* nil */ { $$ = NULL; }
  | frame_type frame_boundary_opts frame_exclude {
    int32_t frame_boundary_opts_flags = (int32_t)((int_ast_node *)($2)->left)->value;
    int32_t flags = $1 | frame_boundary_opts_flags | $3;
    ast_node *expr_list = $2->right;
    $$ = new_ast_opt_frame_spec(new_ast_opt(flags), expr_list);
  }
  ;

frame_type: RANGE { $$ = FRAME_TYPE_RANGE; }
  | ROWS { $$ = FRAME_TYPE_ROWS; }
  | GROUPS { $$ = FRAME_TYPE_GROUPS; }
  ;

frame_exclude: /* nil */ { $$ = FRAME_EXCLUDE_NONE; }
  | EXCLUDE_NO_OTHERS { $$ = FRAME_EXCLUDE_NO_OTHERS; }
  | EXCLUDE_CURRENT_ROW { $$ = FRAME_EXCLUDE_CURRENT_ROW; }
  | EXCLUDE_GROUP { $$ = FRAME_EXCLUDE_GROUP; }
  | EXCLUDE_TIES { $$ = FRAME_EXCLUDE_TIES; }
  ;

frame_boundary_opts: frame_boundary {
    ast_node *ast_flags = $1->left;
    ast_node *expr_list = new_ast_expr_list($1->right, NULL);
    $$ = new_ast_frame_boundary_opts(ast_flags, expr_list);
  }
  | BETWEEN frame_boundary_start AND frame_boundary_end {
    int32_t flags = (int32_t)(((int_ast_node *)$2->left)->value | ((int_ast_node *)$4->left)->value);
    ast_node *expr_list = new_ast_expr_list($2->right, $4->right);
    $$ = new_ast_frame_boundary_opts(new_ast_opt(flags), expr_list);
  }
  ;

frame_boundary_start: UNBOUNDED PRECEDING { $$ = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_UNBOUNDED), NULL); }
  | expr PRECEDING { $$ = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_PRECEDING), $1); }
  | CURRENT_ROW { $$ = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_CURRENT_ROW), NULL); }
  | expr FOLLOWING { $$ = new_ast_frame_boundary_start(new_ast_opt(FRAME_BOUNDARY_START_CURRENT_ROW), $1); }
  ;

frame_boundary_end: expr PRECEDING { $$ = new_ast_frame_boundary_end($1, new_ast_opt(FRAME_BOUNDARY_END_PRECEDING)); }
  | CURRENT_ROW { $$ = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_CURRENT_ROW), NULL); }
  | expr FOLLOWING { $$ = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_FOLLOWING), $1); }
  | UNBOUNDED FOLLOWING { $$ = new_ast_frame_boundary_end(new_ast_opt(FRAME_BOUNDARY_END_UNBOUNDED), NULL); }
  ;

frame_boundary: UNBOUNDED PRECEDING { $$ = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_UNBOUNDED), NULL); }
  | expr PRECEDING { $$ = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_PRECEDING), $1); }
  | CURRENT_ROW { $$ = new_ast_frame_boundary(new_ast_opt(FRAME_BOUNDARY_CURRENT_ROW), NULL); }
  ;

opt_partition_by: /* nil */ { $$ = NULL; }
  | PARTITION BY expr_list { $$ = new_ast_opt_partition_by($3); }
  ;

opt_select_window: /* nil */ { $$ = NULL; }
  | window_clause { $$ = new_ast_opt_select_window($1); }
  ;

window_clause: WINDOW window_name_defn_list { $$ = new_ast_window_clause($2); }
  ;

window_name_defn_list: window_name_defn { $$ = new_ast_window_name_defn_list($1, NULL); }
  | window_name_defn ',' window_name_defn_list { $$ = new_ast_window_name_defn_list($1, $3); }
  ;

window_name_defn: name AS window_defn { $$ = new_ast_window_name_defn($1, $3); }

region_spec:
    name  { $$ = new_ast_region_spec($1, new_ast_opt(PUBLIC_REGION)); }
  | name PRIVATE { $$ = new_ast_region_spec($1, new_ast_opt(PRIVATE_REGION)); }
  ;

region_list :
    region_spec ',' region_list { $$ = new_ast_region_list($1, $3); }
  | region_spec { $$ = new_ast_region_list($1, NULL); }
  ;

declare_schema_region_stmt:
  AT_DECLARE_SCHEMA_REGION name { $$ = new_ast_declare_schema_region_stmt($2, NULL); }
  | AT_DECLARE_SCHEMA_REGION name USING region_list { $$ = new_ast_declare_schema_region_stmt($2, $4); }
  ;

declare_deployable_region_stmt:
  AT_DECLARE_DEPLOYABLE_REGION  name { $$ = new_ast_declare_deployable_region_stmt($2, NULL); }
  | AT_DECLARE_DEPLOYABLE_REGION name USING region_list { $$ = new_ast_declare_deployable_region_stmt($2, $4); }
  ;

begin_schema_region_stmt: AT_BEGIN_SCHEMA_REGION name {$$ = new_ast_begin_schema_region_stmt($2); }
  ;

end_schema_region_stmt: AT_END_SCHEMA_REGION {$$ = new_ast_end_schema_region_stmt(); }
  ;

schema_ad_hoc_migration_stmt: AT_SCHEMA_AD_HOC_MIGRATION version_annotation { $$ = new_ast_schema_ad_hoc_migration_stmt($2); }
  ;

opt_from_query_parts: /* nil */ { $$ = NULL; }
  | FROM query_parts { $$ = $2; }
  ;

opt_where: /* nil */ { $$ = NULL; }
  | WHERE expr { $$ = new_ast_opt_where($2); }
  ;

opt_groupby: /* nil */ { $$ = NULL; }
  | GROUP BY groupby_list { $$ = new_ast_opt_groupby($3); }
  ;

groupby_list: groupby_item { $$ = new_ast_groupby_list($1, NULL); }
  | groupby_item ',' groupby_list { $$ = new_ast_groupby_list($1, $3); }
  ;

groupby_item: expr opt_asc_desc { $$ = new_ast_groupby_item($1, $2); }
  ;

opt_asc_desc: /* nil */ { $$ = NULL; }
  | ASC { $$ = new_ast_asc(); }
  | DESC { $$ = new_ast_desc(); }
  ;

opt_having: /* nil */ { $$ = NULL; }
  | HAVING expr { $$ = new_ast_opt_having($2); }
  ;

opt_orderby: /* nil */ { $$ = NULL; }
  | ORDER BY groupby_list { $$ = new_ast_opt_orderby($3); }
  ;

opt_limit: /* nil */ { $$ = NULL; }
  | LIMIT expr { $$ = new_ast_opt_limit($2); }
  ;

opt_offset: /* nil */ { $$ = NULL; }
  | OFFSET expr { $$ = new_ast_opt_offset($2); }
  ;

select_opts: /* nil */ { $$ = NULL; }
  | ALL  { $$ = new_ast_select_opts(new_ast_all()); }
  | DISTINCT { $$ = new_ast_select_opts(new_ast_distinct()); }
  | DISTINCTROW { $$ = new_ast_select_opts(new_ast_distinctrow()); }
  ;

select_expr_list: select_expr { $$ = new_ast_select_expr_list($1, NULL); }
  | select_expr ',' select_expr_list { $$ = new_ast_select_expr_list($1, $3); }
  | '*' { $$ = new_ast_select_expr_list(new_ast_star(), NULL); }
  ;

select_expr: expr opt_as_alias { $$ = new_ast_select_expr($1, $2); }
  |  name '.' '*' { $$ = new_ast_table_star($1); }
  ;

opt_as_alias: /* nil */ { $$ = NULL;  }
  | AS name { $$ = new_ast_opt_as_alias($2); }
  | name { $$ = new_ast_opt_as_alias($1); }
  ;

query_parts: table_or_subquery_list { $$ = $1; }
  | join_clause { $$ = $1; }
  ;

table_or_subquery_list: table_or_subquery { $$ = new_ast_table_or_subquery_list($1, NULL); }
  | table_or_subquery ',' table_or_subquery_list { $$ = new_ast_table_or_subquery_list($1, $3); }
  ;

join_clause: table_or_subquery join_target_list { $$ = new_ast_join_clause($1, $2); }
  ;

join_target_list: join_target { $$ = new_ast_join_target_list($1, NULL); }
  | join_target join_target_list { $$ = new_ast_join_target_list($1, $2); }
  ;

table_or_subquery: name opt_as_alias { $$ = new_ast_table_or_subquery($1, $2); }
  | '(' select_stmt ')' opt_as_alias { $$ = new_ast_table_or_subquery($2, $4); }
  | table_function opt_as_alias { $$ = new_ast_table_or_subquery($1, $2); }
  | '(' query_parts ')' { $$ = new_ast_table_or_subquery($2, NULL); }
  ;

join_target: opt_inner_cross JOIN table_or_subquery opt_join_cond {
      int join_type = JOIN_INNER;
      if (is_ast_cross($1)) {
        join_type = JOIN_CROSS;
      }

      struct ast_node *asti_join_type = new_ast_opt(join_type);
      struct ast_node *table_join = new_ast_table_join($3, $4);
      $$ = new_ast_join_target(asti_join_type, table_join);
    }
  | left_or_right opt_outer JOIN table_or_subquery opt_join_cond {
      int join_type = 0;
      if ($2) {
        if (is_ast_left($1)) {
          join_type = JOIN_LEFT_OUTER;
        }
        else {
          join_type = JOIN_RIGHT_OUTER;
        }
      }
      else {
        if (is_ast_left($1)) {
          join_type = JOIN_LEFT;
        }
        else {
          join_type = JOIN_RIGHT;
        }
      }

      struct ast_node *asti_join_type = new_ast_opt(join_type);
      struct ast_node *table_join = new_ast_table_join($4, $5);
      $$ = new_ast_join_target(asti_join_type, table_join);
    }
  ;

opt_inner_cross: /* nil */ { $$ = NULL; }
  | INNER { $$ = new_ast_inner(); }
  | CROSS { $$ = new_ast_cross(); }
  ;

opt_outer: /* nil */ { $$ = NULL; }
  | OUTER { $$ = new_ast_outer(); }
  ;

left_or_right: LEFT { $$ = new_ast_left(); }
  | RIGHT { $$ = new_ast_right(); }
  ;

opt_join_cond: /* nil */ { $$ = NULL; }
  | join_cond
  ;

join_cond: ON expr { $$ = new_ast_join_cond(new_ast_on(), $2); }
  | USING '(' name_list ')' { $$ = new_ast_join_cond(new_ast_using(), $3); }
  ;

table_function: name '(' arg_list ')' { $$ = new_ast_table_function($1, $3); }
  ;

create_view_stmt: CREATE opt_temp VIEW opt_if_not_exists name AS select_stmt opt_delete_version_attr {
  struct ast_node *flags = new_ast_opt($2 | $4);
  struct ast_node *name_and_select = new_ast_name_and_select($5, $7);
  struct ast_node *view_and_attrs = new_ast_view_and_attrs(name_and_select, $8);
  $$ = new_ast_create_view_stmt(flags, view_and_attrs); }
  ;

with_delete_stmt: with_prefix delete_stmt { $$ = new_ast_with_delete_stmt($1, $2); }
  ;

delete_stmt: DELETE FROM name opt_where {
   $$ = new_ast_delete_stmt($3, $4); }
  ;

opt_insert_dummy_spec : /*nil*/  { $$ = NULL; }
  | AT_DUMMY_SEED '(' expr ')' dummy_modifier {
    $$ = new_ast_insert_dummy_spec($3, new_ast_opt($5)); }
  ;

dummy_modifier: /* nil */ { $$ = 0; }
  | AT_DUMMY_NULLABLES { $$ = INSERT_DUMMY_NULLABLES; }
  | AT_DUMMY_DEFAULTS  { $$ = INSERT_DUMMY_DEFAULTS; }
  | AT_DUMMY_NULLABLES AT_DUMMY_DEFAULTS  { $$ = INSERT_DUMMY_NULLABLES | INSERT_DUMMY_DEFAULTS; }
  | AT_DUMMY_DEFAULTS AT_DUMMY_NULLABLES  { $$ = INSERT_DUMMY_NULLABLES | INSERT_DUMMY_DEFAULTS; }
  ;

insert_stmt_type : INSERT INTO { $$ = new_ast_insert_normal();  }
  | INSERT OR REPLACE INTO { $$ = new_ast_insert_or_replace(); }
  | INSERT OR IGNORE INTO { $$ = new_ast_insert_or_ignore(); }
  | INSERT OR ROLLBACK INTO { $$ = new_ast_insert_or_rollback(); }
  | INSERT OR ABORT INTO { $$ = new_ast_insert_or_abort(); }
  | INSERT OR FAIL INTO { $$ = new_ast_insert_or_fail(); }
  | REPLACE INTO { $$ = new_ast_insert_replace(); }
  ;

with_insert_stmt: with_prefix insert_stmt { $$ = new_ast_with_insert_stmt($1, $2); }
  ;

opt_column_spec: /* nil */ { $$ = NULL; }
  | '(' opt_name_list ')' { $$ = new_ast_column_spec($2); }
  | '(' LIKE name ')' { $$ = new_ast_column_spec(new_ast_like($3, NULL)); }
  ;

from_cursor:  FROM CURSOR name opt_column_spec { $$ = new_ast_from_cursor($4, $3); }
  ;

from_arguments: FROM ARGUMENTS { $$ = new_ast_from_arguments(NULL); }
  | FROM ARGUMENTS LIKE name { $$ = new_ast_from_arguments($4); }
  ;

insert_stmt: insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $4);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $1->left = $5;
    $$ = new_ast_insert_stmt($1, name_columns_values);  }
  | insert_stmt_type name opt_column_spec from_arguments opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $4);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $1->left = $5;
    $$ = new_ast_insert_stmt($1, name_columns_values);  }
  | insert_stmt_type name opt_column_spec from_cursor opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $4);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $1->left = $5;
    $$ = new_ast_insert_stmt($1, name_columns_values);  }
  | insert_stmt_type name DEFAULT VALUES {
    struct ast_node *default_columns_values = new_ast_default_columns_values();
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, default_columns_values);
    $$ = new_ast_insert_stmt($1, name_columns_values);
  }
  ;

insert_list: { $$ = NULL; }
  | expr { $$ = new_ast_insert_list($1, NULL); }
  | expr ',' insert_list { $$ = new_ast_insert_list($1, $3); }
  ;

basic_update_stmt: UPDATE opt_name SET update_list opt_where {
    struct ast_node *orderby = new_ast_update_orderby(NULL, NULL);
    struct ast_node *where = new_ast_update_where($5, orderby);
    struct ast_node *list = new_ast_update_set($4, where);
    $$ = new_ast_update_stmt($2, list); }
  ;

with_update_stmt: with_prefix update_stmt { $$ = new_ast_with_update_stmt($1, $2); }
  ;

update_stmt: UPDATE name SET update_list opt_where opt_orderby opt_limit {
    struct ast_node *limit = $7;
    struct ast_node *orderby = new_ast_update_orderby($6, limit);
    struct ast_node *where = new_ast_update_where($5, orderby);
    struct ast_node *list = new_ast_update_set($4, where);
    $$ = new_ast_update_stmt($2, list); }
  ;

update_entry: name '=' expr { $$ = new_ast_update_entry($1, $3); }
  ;

update_list: update_entry { $$ = new_ast_update_list($1, NULL); }
  | update_entry ',' update_list { $$ = new_ast_update_list($1, $3); }
  ;

with_upsert_stmt: with_prefix upsert_stmt { $$ = new_ast_with_upsert_stmt($1, $2); }
  ;

upsert_stmt: insert_stmt ON_CONFLICT conflict_target DO NOTHING {
    struct ast_node *upsert_update = new_ast_upsert_update($3, NULL);
    $$ = new_ast_upsert_stmt($1, upsert_update); }
  | insert_stmt ON_CONFLICT conflict_target DO basic_update_stmt {
    struct ast_node *upsert_update = new_ast_upsert_update($3, $5);
    $$ = new_ast_upsert_stmt($1, upsert_update); }
  ;

update_cursor_stmt:
    UPDATE CURSOR name opt_column_spec FROM VALUES '(' insert_list ')'  {
    struct ast_node *columns_values = new_ast_columns_values($4, $8);
    $$ = new_ast_update_cursor_stmt($3, columns_values); }
  | UPDATE CURSOR name opt_column_spec from_cursor {
    struct ast_node *columns_values = new_ast_columns_values($4, $5);
    $$ = new_ast_update_cursor_stmt($3, columns_values); }
  ;

conflict_target:  /* nil */ { $$ = new_ast_conflict_target(NULL, NULL); }
  | '(' indexed_columns ')' opt_where {
    $$ = new_ast_conflict_target($2, $4);
  }
  ;

creation_type: object_type { $$ = $1; }
  | object_type NOT NULL_ { $$ = new_ast_notnull($1); }
  | TEXT { $$ = new_ast_type_text(); }
  | TEXT NOT NULL_ { $$ = new_ast_notnull(new_ast_type_text()); }
  | BLOB { $$ = new_ast_type_blob(); }
  | BLOB NOT NULL_ { $$ = new_ast_notnull(new_ast_type_blob()); }
  ;

function: FUNC | FUNCTION
  ;

declare_func_stmt: DECLARE function name '(' params ')' data_type_opt_notnull {
      $$ = new_ast_declare_func_stmt($3, new_ast_func_params_return($5, $7)); }
  | DECLARE SELECT function name '(' params ')' data_type_opt_notnull {
      $$ = new_ast_declare_select_func_stmt($4, new_ast_func_params_return($6, $8)); }
  | DECLARE function name '(' params ')' CREATE creation_type {
      ast_node *type = new_ast_create($8);
      $$ = new_ast_declare_func_stmt($3, new_ast_func_params_return($5, type)); }
  | DECLARE SELECT function name '(' params ')' '(' typed_names ')' {
      $$ = new_ast_declare_select_func_stmt($4, new_ast_func_params_return($6, $9)); }
  ;

procedure: PROC | PROCEDURE
  ;

declare_proc_stmt: DECLARE procedure name '(' params ')' {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_BASIC));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, NULL)); }
  | DECLARE procedure name '(' params ')' '(' typed_names ')' {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_DML));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, $8)); }
  | DECLARE procedure name '(' params ')' USING TRANSACTION {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_USES_DML));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, NULL)); }
  | DECLARE procedure name '(' params ')' OUT '(' typed_names ')' {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, $9)); }
  | DECLARE procedure name '(' params ')' OUT '(' typed_names ')' USING TRANSACTION {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT | PROC_FLAG_USES_DML));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, $9)); }
  | DECLARE procedure name '(' params ')' OUT UNION '(' typed_names ')' {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT_UNION));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, $10)); }
  | DECLARE procedure name '(' params ')' OUT UNION '(' typed_names ')' USING TRANSACTION {
      ast_node *proc_name_flags = new_ast_proc_name_type($3, new_ast_opt(PROC_FLAG_STRUCT_TYPE | PROC_FLAG_USES_OUT_UNION | PROC_FLAG_USES_DML));
      $$ = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts($5, $10)); }
  ;

create_proc_stmt: CREATE procedure name '(' params ')' BEGIN_ opt_stmt_list END {
    $$ = new_ast_create_proc_stmt($3, new_ast_proc_params_stmts($5, $8)); }
  ;

opt_inout: /* nil */ { $$ = NULL ; }
  | IN { $$ = new_ast_in(); }
  | OUT { $$ = new_ast_out(); }
  | INOUT { $$ = new_ast_inout(); }
  ;

typed_name: name data_type_opt_notnull { $$ = new_ast_typed_name($1, $2); }
  | LIKE name { $$ = new_ast_like($2, NULL); }
  ;

typed_names: typed_name  { $$ = new_ast_typed_names($1, NULL); }
  | typed_name ',' typed_names { $$ = new_ast_typed_names($1, $3);}
  ;

param: opt_inout name data_type_opt_notnull { $$ = new_ast_param($1, new_ast_param_detail($2, $3)); }
  | LIKE name { $$ = new_ast_param(new_ast_like($2, NULL), NULL); }
  ;

params: /* nil */ { $$ = NULL; }
  | param { $$ = new_ast_params($1, NULL); }
  |  param ',' params  { $$ = new_ast_params($1, $3); }
  ;

declare_stmt: DECLARE name_list data_type_opt_notnull { $$ = new_ast_declare_vars_type($2, $3); }
  | DECLARE name CURSOR_FOR select_stmt { $$ = new_ast_declare_cursor($2, $4); }
  | DECLARE name CURSOR_FOR explain_stmt { $$ = new_ast_declare_cursor($2, $4); }
  | DECLARE name CURSOR_FOR call_stmt  { $$ = new_ast_declare_cursor($2, $4); }
  | DECLARE name CURSOR FETCH FROM call_stmt { $$ = new_ast_declare_value_cursor($2, $6); }
  | DECLARE name CURSOR LIKE name { $$ = new_ast_declare_cursor_like_name($2, new_ast_like($5, NULL)); }
  | DECLARE name CURSOR LIKE select_stmt { $$ = new_ast_declare_cursor_like_select($2, $5); }
  ;

call_stmt: CALL name '(' ')' { $$ = new_ast_call_stmt($2, NULL); }
  | CALL name '(' call_expr_list ')' { $$ = new_ast_call_stmt($2, $4); }
  ;

while_stmt: WHILE expr BEGIN_ opt_stmt_list END { $$ = new_ast_while_stmt($2, $4); }
  ;

loop_stmt: LOOP fetch_stmt BEGIN_ opt_stmt_list END { $$ = new_ast_loop_stmt($2, $4); }
  ;

leave_stmt: LEAVE { $$ = new_ast_leave_stmt(); }
  ;

return_stmt: RETURN { $$ = new_ast_return_stmt(); }
  ;

throw_stmt: THROW { $$ = new_ast_throw_stmt(); }
  ;

trycatch_stmt: BEGIN_ TRY opt_stmt_list END TRY ';' BEGIN_ CATCH opt_stmt_list END CATCH { $$ = new_ast_trycatch_stmt($3, $9); }
  ;

continue_stmt: CONTINUE { $$ = new_ast_continue_stmt(); }
  ;

fetch_stmt: FETCH name INTO name_list { $$ = new_ast_fetch_stmt($2, $4); }
  | FETCH name { $$ = new_ast_fetch_stmt($2, NULL); }
  ;

fetch_values_stmt:
    FETCH name opt_column_spec FROM VALUES '(' insert_list ')' opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $7);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $$ = new_ast_fetch_values_stmt($9, name_columns_values); }
  | FETCH name opt_column_spec from_arguments opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $4);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $$ = new_ast_fetch_values_stmt($5, name_columns_values); }
  | FETCH name opt_column_spec from_cursor opt_insert_dummy_spec {
    struct ast_node *columns_values = new_ast_columns_values($3, $4);
    struct ast_node *name_columns_values = new_ast_name_columns_values($2, columns_values);
    $$ = new_ast_fetch_values_stmt($5, name_columns_values); }
  ;

fetch_call_stmt: FETCH name opt_column_spec FROM call_stmt {
    YY_ERROR_ON_COLUMNS($3);  // not really allowed, see macro for details.
    $$ = new_ast_fetch_call_stmt($2, $5); }
  ;

fetch_cursor_stmt: FETCH name opt_column_spec FROM name {
    YY_ERROR_ON_COLUMNS($3);  // not really allowed, see macro for details.
    $$ = new_ast_fetch_cursor_stmt($2, $5); }
  ;

open_stmt: OPEN name { $$ = new_ast_open_stmt($2); }
  ;

close_stmt: CLOSE name  { $$ = new_ast_close_stmt($2); }
  ;

out_stmt: OUT name  { $$ = new_ast_out_stmt($2); }
  ;

out_union_stmt: OUT UNION name  { $$ = new_ast_out_union_stmt($3); }
  ;

if_stmt: IF expr THEN opt_stmt_list opt_elseif_list opt_else END IF {
    struct ast_node *if_alt = new_ast_if_alt($5, $6);
    struct ast_node *cond_action = new_ast_cond_action($2, $4);
    $$ = new_ast_if_stmt(cond_action, if_alt); }
  ;

opt_else: /* nil */ { $$ = NULL; }
  | ELSE opt_stmt_list { $$ = new_ast_else($2); }
  ;

elseif_item: ELSE_IF expr THEN opt_stmt_list {
    struct ast_node *cond_action = new_ast_cond_action($2, $4);
    $$ = new_ast_elseif(cond_action, NULL); }
  ;

elseif_list: elseif_item { $$ = $1; }
  | elseif_item elseif_list { $1->right = $2; $$ = $1; }
  ;

opt_elseif_list: /* nil */ { $$ = NULL; }
  | elseif_list { $$ = $1; }
  ;

begin_trans_stmt: BEGIN_ TRANSACTION { $$ = new_ast_begin_trans_stmt(); }
  ;

rollback_trans_stmt: ROLLBACK TRANSACTION { $$ = new_ast_rollback_trans_stmt(NULL); }
  | ROLLBACK TRANSACTION TO SAVEPOINT name { $$ = new_ast_rollback_trans_stmt($5); }
  ;

commit_trans_stmt: COMMIT TRANSACTION { $$ = new_ast_commit_trans_stmt(); }
  ;

savepoint_stmt: SAVEPOINT name { $$ = new_ast_savepoint_stmt($2); }
  ;

release_savepoint_stmt: RELEASE SAVEPOINT name { $$ = new_ast_release_savepoint_stmt($3); }
  ;

echo_stmt: AT_ECHO name ',' str_literal { $$ = new_ast_echo_stmt($2, $4); }
  ;

alter_table_add_column_stmt: ALTER TABLE name ADD COLUMN col_def {
    $$ = new_ast_alter_table_add_column_stmt($3, $6); }
  ;

create_trigger_stmt:  CREATE opt_temp TRIGGER opt_if_not_exists trigger_def opt_delete_version_attr {
    int flags = $2 | $4;
    $$ = new_ast_create_trigger_stmt(
        new_ast_opt(flags),
        new_ast_trigger_body_vers($5, $6)); }
  ;

trigger_def: name trigger_condition trigger_operation ON name trigger_action {
  $$ = new_ast_trigger_def(
        $1,
        new_ast_trigger_condition(
          new_ast_opt($2),
          new_ast_trigger_op_target(
            $3,
            new_ast_trigger_target_action(
              $5,
              $6)))); }
  ;

trigger_condition:
   /* nil */  { $$ = 0; }
 | BEFORE     { $$ = TRIGGER_BEFORE; }
 | AFTER      { $$ = TRIGGER_AFTER; }
 | INSTEAD OF { $$ = TRIGGER_INSTEAD_OF; }
 ;

trigger_operation:
    DELETE  { $$ = new_ast_trigger_operation(new_ast_opt(TRIGGER_DELETE), NULL); }
  | INSERT  { $$ = new_ast_trigger_operation(new_ast_opt(TRIGGER_INSERT), NULL); }
  | UPDATE opt_of { $$ = new_ast_trigger_operation(new_ast_opt(TRIGGER_UPDATE), $2); }
  ;

opt_of:
    /* nil */ { $$ = NULL; }
  | OF name_list { $$ = $2; }
  ;

trigger_action:  opt_foreachrow opt_when_expr BEGIN_ trigger_stmts END {
  $$ = new_ast_trigger_action(
        new_ast_opt($1),
        new_ast_trigger_when_stmts($2, $4)); }
  ;

opt_foreachrow:
    /* nil */ { $$ = 0; }
  | FOR_EACH_ROW  { $$ = TRIGGER_FOR_EACH_ROW; }
  ;

opt_when_expr:
    /* nil */ { $$ = NULL; }
  | WHEN expr { $$ = $2; }
  ;

trigger_stmts:
    trigger_stmt  { $$ = new_ast_stmt_list($1, NULL); }
  | trigger_stmt  trigger_stmts { $$ = new_ast_stmt_list($1, $2); }
  ;

/* These forms are slightly different than the normal statements, not all variations are allowed.
 * This section clearly states the mapping.  It could be done more tersely but this costs us nothing.
 */

trigger_stmt:
    trigger_update_stmt ';' { $$ = $1; }
  | trigger_insert_stmt ';' { $$ = $1; }
  | trigger_delete_stmt ';' { $$ = $1; }
  | trigger_select_stmt ';' { $$ = $1; }
  ;

trigger_select_stmt : select_stmt_no_with { $$ = $1; }
  ;

trigger_insert_stmt : insert_stmt { $$ = $1; }
  ;

trigger_delete_stmt : delete_stmt { $$ = $1; }
  ;

trigger_update_stmt : basic_update_stmt { $$ = $1; }
  ;

enforcement_options:
    FOREIGN KEY ON UPDATE { $$ = new_ast_opt(ENFORCE_FK_ON_UPDATE); }
  | FOREIGN KEY ON DELETE { $$ = new_ast_opt(ENFORCE_FK_ON_DELETE); }
  | JOIN { $$ = new_ast_opt(ENFORCE_STRICT_JOIN); }
  | UPSERT STATEMENT { $$ = new_ast_opt(ENFORCE_UPSERT_STMT); }
  | WINDOW function { $$ = new_ast_opt(ENFORCE_WINDOW_FUNC); }
  | procedure { $$ = new_ast_opt(ENFORCE_PROCEDURE); }
  | WITHOUT ROWID { $$ = new_ast_opt(ENFORCE_WITHOUT_ROWID); }
  ;

enforce_strict_stmt: AT_ENFORCE_STRICT enforcement_options { $$ = new_ast_enforce_strict_stmt($2); }
  ;

enforce_normal_stmt: AT_ENFORCE_NORMAL enforcement_options { $$ = new_ast_enforce_normal_stmt($2); }
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
  // then you want to invoke it's main at some other time.  If you define CQL_IS_NOT_MAIN
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

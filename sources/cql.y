/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// In case there is any doubt, 'cql.y' is included in the license as well as
// the code bison generates from it.

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
//  1. Keep each pass in one file (simple, focused, and easy refactor).
//  2. Use simple printable AST parse nodes (no separate #define per AST node type).
//  3. 100% unit test coverage on all passes including output validation.

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

// Bison defines this only if __GNUC__ is defined, but Clang defines _MSC_VER
// and not __GNUC__ on Windows.
#ifdef __clang__
  #define YY_ATTRIBUTE_UNUSED __attribute__((unused))
#endif

static void parse_cmd(int argc, char **argv);
static void print_dot(struct ast_node* node);
static ast_node *file_literal(ast_node *);
static void cql_exit_on_parse_errors();
static void parse_cleanup();
static void cql_usage();
static ast_node *make_statement_node(ast_node *misc_attrs, ast_node *any_stmt);
static ast_node *make_coldef_node(ast_node *col_def_tye_attrs, ast_node *misc_attrs);
static ast_node *reduce_str_chain(ast_node *str_chain);

// Set to true upon a call to `yyerror`.
static bool_t parse_error_occurred;
static CSTR table_comment_saved;

int yylex();
void yyerror(const char *s, ...);
void yyset_in(FILE *);
void yyset_lineno(int);
void yyrestart(FILE *);

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
  if (x) yyerror("Cursor columns not allowed in this form.")

// We insert calls to `cql_inferred_notnull` as part of a rewrite so we expect
// to see it during semantic analysis, but it cannot be allowed to appear in a
// program. It would be unsafe if it could: It coerces a value from a nullable
// type to a nonnull type without any runtime check.
#define YY_ERROR_ON_CQL_INFERRED_NOTNULL(x) \
  EXTRACT_STRING(proc_name, x); \
  if (!strcmp(proc_name, "cql_inferred_notnull")) { \
    yyerror("Call to internal function is not allowed 'cql_inferred_notnull'"); \
  }

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

%token <sval> ID TRUE_ "TRUE" FALSE_ "FALSE"
%token <sval> STRLIT CSTRLIT BLOBLIT
%token <sval> INTLIT
%token <ival> BOOL_ "BOOL"
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
// declared in ast.h EXPR_PRI_XXX or else badness ensues.  It must also agree
// with the SQLite precedence shown below or badness ensues...

// Don't try to remove the NOT_IN, IS_NOT, NOT_BETWEEN, or NOT_LIKE tokens
// you can match the language with those but the precedence of NOT is wrong
// so order of operations will be subtlely off.  There are now tests for this.

%left UNION_ALL UNION INTERSECT EXCEPT
%right ASSIGN
%left OR
%left AND
%left NOT
%left BETWEEN NOT_BETWEEN NE NE_ '=' EQEQ LIKE NOT_LIKE GLOB NOT_GLOB MATCH NOT_MATCH REGEXP NOT_REGEXP IN NOT_IN IS_NOT IS IS_TRUE IS_FALSE IS_NOT_TRUE IS_NOT_FALSE
%right ISNULL NOTNULL
%left '<' '>' GE LE
%left LS RS '&' '|'
%left '+' '-'
%left '*' '/' '%'
%left CONCAT
%left COLLATE
%right UMINUS '~'

/* from the SQLite grammar  for comparison

 left OR.
 left AND.
 right NOT.
 left IS MATCH LIKE_KW BETWEEN IN ISNULL NOTNULL NE EQ.
 left GT LE LT GE.
 right ESCAPE.    (NYI in CQL)
 left BITAND BITOR LSHIFT RSHIFT.
 left PLUS MINUS.
 left STAR SLASH REM.
 left CONCAT.
 left COLLATE.
 right BITNOT.
*/

// String representations for operators mentioned above. (These cannot be given
// in the precedence declarations themselves.)
%token ASSIGN ":="
%token CONCAT "||"
%token EQEQ "=="
%token GE ">="
%token LE "<="
%token LS "<<"
%token NE "<>"
%token NE_ "!="
%token RS ">>"

%token EXCLUDE_GROUP EXCLUDE_CURRENT_ROW EXCLUDE_TIES EXCLUDE_NO_OTHERS CURRENT_ROW UNBOUNDED PRECEDING FOLLOWING
%token AT_BLOB_GET_KEY_TYPE AT_BLOB_GET_VAL_TYPE AT_BLOB_GET_KEY AT_BLOB_GET_VAL AT_BLOB_CREATE_KEY AT_BLOB_CREATE_VAL AT_BLOB_UPDATE_KEY AT_BLOB_UPDATE_VAL
%token CREATE DROP TABLE WITHOUT ROWID PRIMARY KEY NULL_ "NULL" DEFAULT CHECK AT_DUMMY_SEED VIRTUAL AT_EMIT_GROUP AT_EMIT_ENUMS AT_EMIT_CONSTANTS
%token OBJECT TEXT BLOB LONG_ "LONG" INT_ "INT" INTEGER LONG_INT LONG_INTEGER REAL ON UPDATE CASCADE ON_CONFLICT DO NOTHING
%token DELETE INDEX FOREIGN REFERENCES CONSTRAINT UPSERT STATEMENT CONST
%token INSERT INTO VALUES VIEW SELECT QUERY_PLAN EXPLAIN OVER WINDOW FILTER PARTITION RANGE ROWS GROUPS
%token AS CASE WHEN FROM FROM_BLOB THEN ELSE END LEFT SWITCH
%token OUTER JOIN WHERE GROUP BY ORDER ASC NULLS FIRST LAST
%token DESC INNER AUTOINCREMENT DISTINCT
%token LIMIT OFFSET TEMP TRIGGER IF ALL CROSS USING RIGHT AT_EPONYMOUS
%token HIDDEN UNIQUE HAVING SET LET TO DISTINCTROW ENUM
%token FUNC FUNCTION PROC PROCEDURE INTERFACE BEGIN_ "BEGIN" OUT INOUT CURSOR DECLARE VAR TYPE FETCH LOOP LEAVE CONTINUE FOR ENCODE CONTEXT_COLUMN CONTEXT_TYPE
%token OPEN CLOSE ELSE_IF WHILE CALL TRY CATCH THROW RETURN
%token SAVEPOINT ROLLBACK COMMIT TRANSACTION RELEASE ARGUMENTS
%token CAST WITH RECURSIVE REPLACE IGNORE ADD COLUMN COLUMNS RENAME ALTER
%token AT_ECHO AT_CREATE AT_RECREATE AT_DELETE AT_SCHEMA_UPGRADE_VERSION AT_PREVIOUS_SCHEMA AT_SCHEMA_UPGRADE_SCRIPT
%token AT_RC AT_PROC AT_FILE AT_ATTRIBUTE AT_SENSITIVE DEFERRED NOT_DEFERRABLE DEFERRABLE IMMEDIATE EXCLUSIVE RESTRICT ACTION INITIALLY NO
%token BEFORE AFTER INSTEAD OF FOR_EACH_ROW EXISTS RAISE FAIL ABORT AT_ENFORCE_STRICT AT_ENFORCE_NORMAL AT_ENFORCE_RESET AT_ENFORCE_PUSH AT_ENFORCE_POP
%token AT_BEGIN_SCHEMA_REGION AT_END_SCHEMA_REGION
%token AT_DECLARE_SCHEMA_REGION AT_DECLARE_DEPLOYABLE_REGION AT_SCHEMA_AD_HOC_MIGRATION PRIVATE
%token SIGN_FUNCTION CURSOR_HAS_ROW AT_UNSUB AT_RESUB

/* ddl stuff */
%type <ival> opt_temp opt_if_not_exists opt_unique opt_no_rowid dummy_modifier compound_operator opt_query_plan
%type <ival> opt_fk_options fk_options fk_on_options fk_action fk_initial_state fk_deferred_options transaction_mode conflict_clause
%type <ival> frame_type frame_exclude join_type
%type <ival> opt_vtab_flags

%type <aval> col_key_list col_key_def col_def col_name
%type <aval> version_attrs opt_version_attrs version_attrs_opt_recreate opt_delete_version_attr opt_delete_plain_attr
%type <aval> misc_attr_key misc_attr misc_attrs misc_attr_value misc_attr_value_list
%type <aval> col_attrs str_literal num_literal any_literal const_expr str_chain str_leaf
%type <aval> pk_def fk_def unq_def check_def fk_target_options opt_module_args opt_conflict_clause
%type <aval> col_calc col_calcs column_calculation

%type <aval> alter_table_add_column_stmt
%type <aval> create_index_stmt create_table_stmt create_view_stmt create_virtual_table_stmt
%type <aval> indexed_column indexed_columns
%type <aval> drop_index_stmt drop_table_stmt drop_view_stmt drop_trigger_stmt
%type <ival> create_table_prefix_opt_temp

%type <aval> trigger_update_stmt trigger_delete_stmt trigger_insert_stmt trigger_select_stmt select_nothing_stmt
%type <aval> trigger_stmt trigger_stmts opt_when_expr trigger_action opt_of
%type <aval> trigger_def trigger_operation create_trigger_stmt raise_expr
%type <ival> trigger_condition opt_foreachrow

/* dml stuff */
%type <aval> with_delete_stmt delete_stmt
%type <aval> insert_stmt with_insert_stmt insert_list_item insert_list insert_stmt_type opt_column_spec opt_insert_dummy_spec expr_names expr_name
%type <aval> with_prefix with_select_stmt cte_table cte_tables cte_binding_list cte_binding cte_decl shared_cte
%type <aval> select_expr select_expr_list select_opts select_stmt select_core values explain_stmt explain_target
%type <aval> select_stmt_no_with select_core_list
%type <aval> window_func_inv opt_filter_clause window_name_or_defn window_defn opt_select_window
%type <aval> opt_partition_by opt_frame_spec frame_boundary_opts frame_boundary_start frame_boundary_end frame_boundary
%type <aval> opt_where opt_groupby opt_having opt_orderby opt_limit opt_offset opt_as_alias as_alias window_clause
%type <aval> groupby_item groupby_list orderby_item orderby_list opt_asc_desc opt_nullsfirst_nullslast window_name_defn window_name_defn_list
%type <aval> table_or_subquery table_or_subquery_list query_parts table_function opt_from_query_parts
%type <aval> opt_join_cond join_cond join_clause join_target join_target_list
%type <aval> basic_update_stmt with_update_stmt update_stmt update_cursor_stmt update_entry update_list upsert_stmt conflict_target
%type <aval> declare_schema_region_stmt declare_deployable_region_stmt call with_upsert_stmt
%type <aval> begin_schema_region_stmt end_schema_region_stmt schema_ad_hoc_migration_stmt region_list region_spec
%type <aval> schema_unsub_stmt schema_resub_stmt

/* expressions and types */
%type <aval> expr basic_expr math_expr expr_list typed_name typed_names case_list call_expr_list call_expr shape_arguments
%type <aval> name name_list opt_name_list opt_name
%type <aval> data_type_any data_type_numeric data_type_with_options opt_kind

/* proc stuff */
%type <aval> create_proc_stmt declare_func_stmt declare_select_func_no_check_stmt declare_proc_stmt declare_interface_stmt declare_proc_no_check_stmt declare_out_call_stmt
%type <aval> arg_expr arg_list inout param params func_params func_param

/* statements */
%type <aval> stmt
%type <aval> stmt_list opt_stmt_list
%type <aval> any_stmt
%type <aval> begin_trans_stmt
%type <aval> call_stmt
%type <aval> close_stmt
%type <aval> commit_trans_stmt commit_return_stmt
%type <aval> continue_stmt
%type <aval> control_stmt
%type <aval> declare_stmt declare_simple_var_stmt
%type <aval> declare_enum_stmt enum_values enum_value emit_enums_stmt emit_group_stmt
%type <aval> declare_const_stmt const_values const_value emit_constants_stmt declare_group_stmt simple_variable_decls
%type <aval> echo_stmt
%type <aval> fetch_stmt fetch_values_stmt fetch_call_stmt from_shape fetch_cursor_from_blob_stmt
%type <aval> guard_stmt
%type <aval> if_stmt elseif_item elseif_list opt_else opt_elseif_list proc_savepoint_stmt
%type <aval> leave_stmt return_stmt
%type <aval> loop_stmt
%type <aval> out_stmt out_union_stmt out_union_parent_child_stmt child_results child_result
%type <aval> previous_schema_stmt
%type <aval> release_savepoint_stmt
%type <aval> rollback_trans_stmt rollback_return_stmt savepoint_name
%type <aval> savepoint_stmt
%type <aval> schema_upgrade_script_stmt
%type <aval> schema_upgrade_version_stmt
%type <aval> set_stmt let_stmt
%type <aval> switch_stmt switch_cases switch_case
%type <aval> throw_stmt
%type <aval> trycatch_stmt
%type <aval> version_annotation
%type <aval> while_stmt
%type <aval> enforce_strict_stmt enforce_normal_stmt enforce_reset_stmt enforce_push_stmt enforce_pop_stmt
%type <aval> enforcement_options shape_def shape_def_base shape_expr shape_exprs
%type <aval> blob_get_key_type_stmt blob_get_val_type_stmt blob_get_key_stmt blob_get_val_stmt
%type <aval> blob_create_key_stmt blob_create_val_stmt blob_update_key_stmt blob_update_val_stmt
%type <aval> opt_use_offset

%start program

%%

program:
  opt_stmt_list  {
    if (parse_error_occurred) {
      cql_exit_on_parse_errors();
    }
    gen_init();
    if (options.semantic) {
      sem_main($opt_stmt_list);
    }
    if (options.codegen) {
      rt->code_generator($opt_stmt_list);
    }
    else if (options.print_ast) {
      print_root_ast($opt_stmt_list);
      cql_output("\n");
    } else if (options.print_dot) {
      cql_output("\ndigraph parse {");
      print_dot($opt_stmt_list);
      cql_output("\n}\n");
    }
    else if (options.echo_input) {
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
  stmt ';'  {
     // We're going to do this cheesy thing with the stmt_list structures so that we can
     // code the stmt_list rules using left recursion.  We're doing this because it's
     // possible that there could be a LOT of statements and this minimizes the use
     // of the bison stack because reductions happen sooner with this pattern.  It does
     // mean we have to do some weird stuff because we need to build the list so that the
     // tail is on the right.  To accomplish this we take advantage of the fact that the
     // parent pointer of the statement list is meaningless while it is unrooted.  It
     // would always be null.  We store the tail of the statement list there so we know
     // where to add new nodes on the right.  When the statement list is put into the tree
     // the parent node is set as usual so nobody will know we did this and we don't
     // have to add anything to the node for this one case.

     // With this done we can handle several thousand statements without using much stack space.

     $result = new_ast_stmt_list($stmt, NULL);
     $result->lineno = $stmt->lineno;

     // set up the tail pointer invariant to use later
     $result->parent = $result;
     }
  | stmt_list[slist] stmt ';'  {
     ast_node *new_stmt = new_ast_stmt_list($stmt, NULL);
     new_stmt->lineno = $stmt->lineno;

     // use our tail pointer invariant so we can add at the tail without searching
     ast_node *tail = $slist->parent;
     ast_set_right(tail, new_stmt);

     // re-establish the tail invariant per the above
     $slist->parent = new_stmt;
     $result = $slist;
     }
  ;

stmt:
  misc_attrs any_stmt  { $stmt = make_statement_node($misc_attrs, $any_stmt); }
  ;

any_stmt:
    alter_table_add_column_stmt
  | begin_schema_region_stmt
  | begin_trans_stmt
  | blob_get_key_type_stmt
  | blob_get_val_type_stmt
  | blob_get_key_stmt
  | blob_get_val_stmt
  | blob_create_key_stmt
  | blob_create_val_stmt
  | blob_update_key_stmt
  | blob_update_val_stmt
  | call_stmt
  | close_stmt
  | commit_return_stmt
  | commit_trans_stmt
  | continue_stmt
  | create_index_stmt
  | create_proc_stmt
  | create_table_stmt
  | create_trigger_stmt
  | create_view_stmt
  | create_virtual_table_stmt
  | declare_deployable_region_stmt
  | declare_enum_stmt
  | declare_const_stmt
  | declare_group_stmt
  | declare_select_func_no_check_stmt
  | declare_func_stmt
  | declare_out_call_stmt
  | declare_proc_no_check_stmt
  | declare_proc_stmt
  | declare_interface_stmt
  | declare_schema_region_stmt
  | declare_stmt
  | delete_stmt
  | drop_index_stmt
  | drop_table_stmt
  | drop_trigger_stmt
  | drop_view_stmt
  | echo_stmt
  | emit_enums_stmt
  | emit_group_stmt
  | emit_constants_stmt
  | end_schema_region_stmt
  | enforce_normal_stmt
  | enforce_pop_stmt
  | enforce_push_stmt
  | enforce_reset_stmt
  | enforce_strict_stmt
  | explain_stmt
  | select_nothing_stmt
  | fetch_call_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_cursor_from_blob_stmt
  | guard_stmt
  | if_stmt
  | insert_stmt
  | leave_stmt
  | let_stmt
  | loop_stmt
  | out_stmt
  | out_union_stmt
  | out_union_parent_child_stmt
  | previous_schema_stmt
  | proc_savepoint_stmt
  | release_savepoint_stmt
  | return_stmt
  | rollback_return_stmt
  | rollback_trans_stmt
  | savepoint_stmt
  | select_stmt
  | schema_ad_hoc_migration_stmt
  | schema_resub_stmt
  | schema_unsub_stmt
  | schema_upgrade_script_stmt
  | schema_upgrade_version_stmt
  | set_stmt
  | switch_stmt
  | throw_stmt
  | trycatch_stmt
  | update_cursor_stmt
  | update_stmt
  | upsert_stmt
  | while_stmt
  | with_delete_stmt
  | with_insert_stmt
  | with_update_stmt
  | with_upsert_stmt
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

let_stmt:
  LET name ASSIGN expr  { $let_stmt = new_ast_let_stmt($name, $expr); }
  ;

version_attrs_opt_recreate:
  /* nil */  { $version_attrs_opt_recreate = NULL; }
  | AT_RECREATE  opt_delete_plain_attr { $version_attrs_opt_recreate = new_ast_recreate_attr(NULL, $opt_delete_plain_attr); }
  | AT_RECREATE '(' name ')'  opt_delete_plain_attr { $version_attrs_opt_recreate = new_ast_recreate_attr($name, $opt_delete_plain_attr); }
  | version_attrs  { $version_attrs_opt_recreate = $version_attrs; }
  ;

opt_delete_plain_attr:
  /* nil */  {$opt_delete_plain_attr = NULL; }
  | AT_DELETE { $opt_delete_plain_attr = new_ast_delete_attr(NULL, NULL); }
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

create_virtual_table_stmt: CREATE VIRTUAL TABLE opt_vtab_flags name[table_name]
                           USING name[module_name] opt_module_args
                           AS '(' col_key_list ')' opt_delete_version_attr {
    int flags = $opt_vtab_flags;
    struct ast_node *flags_node = new_ast_opt(flags);
    struct ast_node *name = $table_name;
    struct ast_node *col_key_list = $col_key_list;
    struct ast_node *version_info = $opt_delete_version_attr ? $opt_delete_version_attr : new_ast_recreate_attr(NULL, NULL);
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

create_table_prefix_opt_temp:
  CREATE opt_temp TABLE {
    /* This node only exists so that we can get an early reduce in the table flow to grab the doc comment */
   $create_table_prefix_opt_temp = $opt_temp; table_comment_saved = get_last_doc_comment();
  };

create_table_stmt:
  create_table_prefix_opt_temp opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate  {
    int flags = $create_table_prefix_opt_temp | $opt_if_not_exists | $opt_no_rowid;
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

opt_vtab_flags:
  /* nil */ { $opt_vtab_flags = 0; }
  | IF NOT EXISTS  { $opt_vtab_flags = GENERIC_IF_NOT_EXISTS; }
  | AT_EPONYMOUS { $opt_vtab_flags = VTAB_IS_EPONYMOUS; }
  | AT_EPONYMOUS IF NOT EXISTS  { $opt_vtab_flags = VTAB_IS_EPONYMOUS | GENERIC_IF_NOT_EXISTS; }
  | IF NOT EXISTS AT_EPONYMOUS { $opt_vtab_flags = VTAB_IS_EPONYMOUS | GENERIC_IF_NOT_EXISTS; }
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

shape_exprs[result] :
  shape_expr ',' shape_exprs[next] { $result = new_ast_shape_exprs($shape_expr, $next); }
  | shape_expr { $result = new_ast_shape_exprs($shape_expr, NULL); }
  ;

shape_expr:
  name  { $shape_expr = new_ast_shape_expr($name, $name); }
  | '-' name  { $shape_expr = new_ast_shape_expr($name, NULL); }
  ;

shape_def:
    shape_def_base  { $shape_def = new_ast_shape_def($shape_def_base, NULL); }
  | shape_def_base '(' shape_exprs ')' { $shape_def = new_ast_shape_def($shape_def_base, $shape_exprs); }
  ;

shape_def_base:
    LIKE name { $shape_def_base = new_ast_like($name, NULL); }
  | LIKE name ARGUMENTS { $shape_def_base = new_ast_like($name, $name); }
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
  | '+' num_literal  { $misc_attr_value = $num_literal;}
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
  misc_attrs col_name data_type_any col_attrs  {
  struct ast_node *name_type = new_ast_col_def_name_type($col_name, $data_type_any);
  struct ast_node *col_def_type_attrs = new_ast_col_def_type_attrs(name_type, $col_attrs);
  $col_def = make_coldef_node(col_def_type_attrs, $misc_attrs);
  }
  ;

pk_def:
  CONSTRAINT name PRIMARY KEY '(' indexed_columns ')' opt_conflict_clause {
    ast_node *indexed_columns_conflict_clause = new_ast_indexed_columns_conflict_clause($indexed_columns, $opt_conflict_clause);
    $pk_def = new_ast_pk_def($name, indexed_columns_conflict_clause);
  }
  | PRIMARY KEY '(' indexed_columns ')' opt_conflict_clause {
    ast_node *indexed_columns_conflict_clause = new_ast_indexed_columns_conflict_clause($indexed_columns, $opt_conflict_clause);
    $pk_def = new_ast_pk_def(NULL, indexed_columns_conflict_clause);
  }
  ;

opt_conflict_clause:
  /* nil */ { $opt_conflict_clause = NULL; }
  | conflict_clause { $opt_conflict_clause = new_ast_opt($conflict_clause); }
  ;

conflict_clause:
  ON_CONFLICT ROLLBACK { $conflict_clause = ON_CONFLICT_ROLLBACK; }
  | ON_CONFLICT ABORT { $conflict_clause = ON_CONFLICT_ABORT; }
  | ON_CONFLICT FAIL { $conflict_clause = ON_CONFLICT_FAIL; }
  | ON_CONFLICT IGNORE { $conflict_clause = ON_CONFLICT_IGNORE; }
  | ON_CONFLICT REPLACE { $conflict_clause = ON_CONFLICT_REPLACE; }
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
  CONSTRAINT name FOREIGN KEY '(' name_list ')' fk_target_options  {
    ast_node *fk_info = new_ast_fk_info($name_list, $fk_target_options);
    $fk_def = new_ast_fk_def($name, fk_info); }
  | FOREIGN KEY '(' name_list ')' fk_target_options  {
    ast_node *fk_info = new_ast_fk_info($name_list, $fk_target_options);
    $fk_def = new_ast_fk_def(NULL, fk_info); }
  ;

fk_target_options:
  REFERENCES name '(' name_list ')' opt_fk_options  {
    $fk_target_options = new_ast_fk_target_options(new_ast_fk_target($name, $name_list), new_ast_opt($opt_fk_options)); }
  ;

unq_def:
  CONSTRAINT name UNIQUE '(' indexed_columns ')' opt_conflict_clause {
    ast_node *indexed_columns_conflict_clause = new_ast_indexed_columns_conflict_clause($indexed_columns, $opt_conflict_clause);
    $unq_def = new_ast_unq_def($name, indexed_columns_conflict_clause);
  }
  | UNIQUE '(' indexed_columns ')' opt_conflict_clause {
    ast_node *indexed_columns_conflict_clause = new_ast_indexed_columns_conflict_clause($indexed_columns, $opt_conflict_clause);
    $unq_def = new_ast_unq_def(NULL, indexed_columns_conflict_clause);
  }
  ;

opt_unique:
  /* nil */  { $opt_unique = 0; }
  | UNIQUE  { $opt_unique = 1; }
  ;

indexed_column:
  expr opt_asc_desc  {
    $indexed_column = new_ast_indexed_column($expr, $opt_asc_desc); }
  ;

indexed_columns[result]:
  indexed_column  { $result = new_ast_indexed_columns($indexed_column, NULL); }
  | indexed_column ',' indexed_columns[ic]  { $result = new_ast_indexed_columns($indexed_column, $ic); }
  ;

create_index_stmt:
  CREATE opt_unique INDEX opt_if_not_exists name[tbl_name] ON name[idx_name] '(' indexed_columns ')' opt_where opt_delete_version_attr  {
    int flags = 0;
    if ($opt_unique) flags |= INDEX_UNIQUE;
    if ($opt_if_not_exists) flags |= INDEX_IFNE;

    ast_node *create_index_on_list = new_ast_create_index_on_list($tbl_name, $idx_name);
    ast_node *index_names_and_attrs = new_ast_index_names_and_attrs($indexed_columns, $opt_where);
    ast_node *connector = new_ast_connector(index_names_and_attrs, $opt_delete_version_attr);
    ast_node *flags_names_attrs = new_ast_flags_names_attrs(new_ast_opt(flags), connector);
    $create_index_stmt = new_ast_create_index_stmt(create_index_on_list, flags_names_attrs);
  }
  ;

name:
  ID  { $name = new_ast_str($ID); }
  | TEXT  { $name = new_ast_str("text"); }
  | TRIGGER  { $name = new_ast_str("trigger"); }
  | ROWID  { $name = new_ast_str("rowid"); }
  | REPLACE  { $name = new_ast_str("replace"); }
  | KEY  { $name = new_ast_str("key"); }
  | VIRTUAL  { $name = new_ast_str("virtual"); }
  | TYPE { $name = new_ast_str("type"); }
  | HIDDEN { $name = new_ast_str("hidden"); }
  | PRIVATE { $name = new_ast_str("private"); }
  | FIRST { $name = new_ast_str("first"); }
  | LAST { $name = new_ast_str("last"); }
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

cte_binding_list[result]:
  cte_binding { $result = new_ast_cte_binding_list($cte_binding, NULL); }
  | cte_binding ',' cte_binding_list[nl]  { $result = new_ast_cte_binding_list($cte_binding, $nl); }
  ;

cte_binding: name[formal] name[actual] { $cte_binding = new_ast_cte_binding($formal, $actual); }
  | name[formal] AS name[actual] { $cte_binding = new_ast_cte_binding($formal, $actual); }
  ;

col_attrs[result]:
  /* nil */  { $result = NULL; }
  | NOT NULL_ opt_conflict_clause col_attrs[ca]  { $result = new_ast_col_attrs_not_null($opt_conflict_clause, $ca); }
  | PRIMARY KEY opt_conflict_clause col_attrs[ca]  {
    ast_node *autoinc_and_conflict_clause = new_ast_autoinc_and_conflict_clause(NULL, $opt_conflict_clause);
    $result = new_ast_col_attrs_pk(autoinc_and_conflict_clause, $ca);
  }
  | PRIMARY KEY opt_conflict_clause AUTOINCREMENT col_attrs[ca]  {
    ast_node *autoinc_and_conflict_clause = new_ast_autoinc_and_conflict_clause(new_ast_col_attrs_autoinc(), $opt_conflict_clause);
    $result = new_ast_col_attrs_pk(autoinc_and_conflict_clause, $ca);
  }
  | DEFAULT '-' num_literal col_attrs[ca]  { $result = new_ast_col_attrs_default(new_ast_uminus($num_literal), $ca);}
  | DEFAULT '+' num_literal col_attrs[ca]  { $result = new_ast_col_attrs_default($num_literal, $ca);}
  | DEFAULT num_literal col_attrs[ca]  { $result = new_ast_col_attrs_default($num_literal, $ca);}
  | DEFAULT const_expr col_attrs[ca]  { $result = new_ast_col_attrs_default($const_expr, $ca);}
  | DEFAULT str_literal col_attrs[ca]  { $result = new_ast_col_attrs_default($str_literal, $ca);}
  | COLLATE name col_attrs[ca]  { $result = new_ast_col_attrs_collate($name, $ca);}
  | CHECK '(' expr ')' col_attrs[ca]  { $result = new_ast_col_attrs_check($expr, $ca);}
  | UNIQUE opt_conflict_clause col_attrs[ca]  { $result = new_ast_col_attrs_unique($opt_conflict_clause, $ca);}
  | HIDDEN col_attrs[ca]  { $result = new_ast_col_attrs_hidden(NULL, $ca);}
  | AT_SENSITIVE col_attrs[ca]  { $result = new_ast_sensitive_attr(NULL, $ca); }
  | AT_CREATE version_annotation col_attrs[ca]  { $result = new_ast_create_attr($version_annotation, $ca);}
  | AT_DELETE version_annotation col_attrs[ca]  { $result = new_ast_delete_attr($version_annotation, $ca);}
  | fk_target_options col_attrs[ca]  { $result = new_ast_col_attrs_fk($fk_target_options, $ca); }
  ;

version_annotation:
  '(' INTLIT ',' name ')'  {
    $version_annotation = new_ast_version_annotation(new_ast_opt(atoi($INTLIT)), $name); }
  | '(' INTLIT ',' name[lhs] ':' name[rhs] ')'  {
    ast_node *dot = new_ast_dot($lhs, $rhs);
    $version_annotation = new_ast_version_annotation(new_ast_opt(atoi($INTLIT)), dot); }
  | '(' INTLIT ')'  {
    $version_annotation = new_ast_version_annotation(new_ast_opt(atoi($INTLIT)), NULL); }
  ;

opt_kind:
  /* nil */ { $opt_kind = NULL; }
  | '<' name '>' {$opt_kind = $name; }
  ;

data_type_numeric:
  INT_ opt_kind { $data_type_numeric = new_ast_type_int($opt_kind); }
  | INTEGER opt_kind { $data_type_numeric = new_ast_type_int($opt_kind); }
  | REAL opt_kind { $data_type_numeric = new_ast_type_real($opt_kind); }
  | LONG_ opt_kind { $data_type_numeric = new_ast_type_long($opt_kind); }
  | BOOL_ opt_kind { $data_type_numeric = new_ast_type_bool($opt_kind); }
  | LONG_ INTEGER opt_kind { $data_type_numeric = new_ast_type_long($opt_kind); }
  | LONG_ INT_ opt_kind { $data_type_numeric = new_ast_type_long($opt_kind); }
  | LONG_INT opt_kind { $data_type_numeric = new_ast_type_long($opt_kind); }
  | LONG_INTEGER opt_kind { $data_type_numeric = new_ast_type_long($opt_kind); }
  ;

data_type_any:
  data_type_numeric { $data_type_any = $data_type_numeric; }
  | TEXT  opt_kind { $data_type_any = new_ast_type_text($opt_kind);  }
  | BLOB  opt_kind { $data_type_any = new_ast_type_blob($opt_kind); }
  | OBJECT opt_kind { $data_type_any = new_ast_type_object($opt_kind); }
  | OBJECT '<' name CURSOR '>' { /* special case for boxed cursor */
    CSTR type = dup_printf("%s CURSOR", AST_STR($name));
    $data_type_any = new_ast_type_object(new_ast_str(type)); }
  | OBJECT '<' name SET '>' { /* special case for result sets */
    CSTR type = dup_printf("%s SET", AST_STR($name));
    $data_type_any = new_ast_type_object(new_ast_str(type)); }
  | ID { $data_type_any = new_ast_str($ID); }
  ;

data_type_with_options:
  data_type_any { $data_type_with_options = $data_type_any; }
  | data_type_any NOT NULL_ { $data_type_with_options = new_ast_notnull($data_type_any); }
  | data_type_any AT_SENSITIVE { $data_type_with_options = new_ast_sensitive_attr($data_type_any, NULL); }
  | data_type_any AT_SENSITIVE NOT NULL_ { $data_type_with_options = new_ast_sensitive_attr(new_ast_notnull($data_type_any), NULL); }
  | data_type_any NOT NULL_ AT_SENSITIVE { $data_type_with_options = new_ast_sensitive_attr(new_ast_notnull($data_type_any), NULL); }
  ;

str_literal:
  str_chain { $str_literal = reduce_str_chain($str_chain); }
  ;

str_chain[result]:
  str_leaf { $result = new_ast_str_chain($str_leaf, NULL); }
  | str_leaf str_chain[next] { $result = new_ast_str_chain($str_leaf, $next); }
  ;

str_leaf:
  STRLIT  { $str_leaf = new_ast_str($STRLIT);}
  | CSTRLIT  { $str_leaf = new_ast_cstr($CSTRLIT); }
  ;

num_literal:
  INTLIT  { $num_literal = new_ast_num(NUM_INT, $INTLIT); }
  | LONGLIT  { $num_literal = new_ast_num(NUM_LONG, $LONGLIT); }
  | REALLIT  { $num_literal = new_ast_num(NUM_REAL, $REALLIT); }
  | TRUE_ { $num_literal = new_ast_num(NUM_BOOL, "1"); }
  | FALSE_ { $num_literal = new_ast_num(NUM_BOOL, "0"); }
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
  | BLOBLIT  { $any_literal = new_ast_blob($BLOBLIT); }
  ;

raise_expr:
  RAISE '(' IGNORE ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_IGNORE), NULL); }
  | RAISE '(' ROLLBACK ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_ROLLBACK), $expr); }
  | RAISE '(' ABORT ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_ABORT), $expr); }
  | RAISE '(' FAIL ','  expr ')'  { $raise_expr = new_ast_raise(new_ast_opt(RAISE_FAIL), $expr); }
  ;

call:
  name '(' arg_list ')' opt_filter_clause  {
      YY_ERROR_ON_CQL_INFERRED_NOTNULL($name);
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, $opt_filter_clause);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $arg_list);
      $call = new_ast_call($name, call_arg_list);
  }
  | name '(' DISTINCT arg_list ')' opt_filter_clause  {
      YY_ERROR_ON_CQL_INFERRED_NOTNULL($name);
      struct ast_node *call_filter_clause = new_ast_call_filter_clause(new_ast_distinct(), $opt_filter_clause);
      struct ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, $arg_list);
      $call = new_ast_call($name, call_arg_list);
  }
  | basic_expr ':' name '(' arg_list ')' {
      $call = new_ast_reverse_apply(
        $basic_expr,
        new_ast_call(
          $name,
          new_ast_call_arg_list(
            new_ast_call_filter_clause(NULL, NULL),
            $arg_list)));
  }
  ;

basic_expr:
  name  { $basic_expr = $name; }
  | AT_RC { $basic_expr = new_ast_str("@RC"); }
  | name[lhs] '.' name[rhs]  { $basic_expr = new_ast_dot($lhs, $rhs); }
  | any_literal  { $basic_expr = $any_literal; }
  | const_expr { $basic_expr = $const_expr; }
  | '(' expr ')'  { $basic_expr = $expr; }
  | call  { $basic_expr = $call; }
  | window_func_inv  { $basic_expr = $window_func_inv; }
  | raise_expr  { $basic_expr = $raise_expr; }
  | '(' select_stmt ')'  { $basic_expr = $select_stmt; }
  | '(' select_stmt IF NOTHING expr ')'  { $basic_expr = new_ast_select_if_nothing_expr($select_stmt, $expr); }
  | '(' select_stmt IF NOTHING OR NULL_ expr ')'  { $basic_expr = new_ast_select_if_nothing_or_null_expr($select_stmt, $expr); }
  | '(' select_stmt IF NOTHING THROW')'  { $basic_expr = new_ast_select_if_nothing_throw_expr($select_stmt); }
  | EXISTS '(' select_stmt ')'  { $basic_expr = new_ast_exists_expr($select_stmt); }
  | CASE expr[cond] case_list END  { $basic_expr = new_ast_case_expr($cond, new_ast_connector($case_list, NULL)); }
  | CASE expr[cond1] case_list ELSE expr[cond2] END  { $basic_expr = new_ast_case_expr($cond1, new_ast_connector($case_list, $cond2));}
  | CASE case_list END  { $basic_expr = new_ast_case_expr(NULL, new_ast_connector($case_list, NULL));}
  | CASE case_list ELSE expr[cond] END  { $basic_expr = new_ast_case_expr(NULL, new_ast_connector($case_list, $cond));}
  | CAST '(' expr[sexp] AS data_type_any ')'  { $basic_expr = new_ast_cast_expr($sexp, $data_type_any); }
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
  | math_expr[lhs] IS_NOT_TRUE  { $result = new_ast_is_not_true($lhs); }
  | math_expr[lhs] IS_NOT_FALSE  { $result = new_ast_is_not_false($lhs); }
  | math_expr[lhs] ISNULL  { $result = new_ast_is($lhs, new_ast_null()); }
  | math_expr[lhs] NOTNULL  { $result = new_ast_is_not($lhs, new_ast_null()); }
  | math_expr[lhs] IS_TRUE  { $result = new_ast_is_true($lhs); }
  | math_expr[lhs] IS_FALSE  { $result = new_ast_is_false($lhs); }
  | '-' math_expr[rhs] %prec UMINUS  { $result = new_ast_uminus($rhs); }
  | '+' math_expr[rhs] %prec UMINUS  { $result = $rhs; }
  | '~' math_expr[rhs]  { $result = new_ast_tilde($rhs); }
  | NOT math_expr[rhs]  { $result = new_ast_not($rhs); }
  | math_expr[lhs] '=' math_expr[rhs]  { $result = new_ast_eq($lhs, $rhs); }
  | math_expr[lhs] EQEQ math_expr[rhs]  { $result = new_ast_eq($lhs, $rhs); }
  | math_expr[lhs] '<' math_expr[rhs]  { $result = new_ast_lt($lhs, $rhs); }
  | math_expr[lhs] '>' math_expr[rhs]  { $result = new_ast_gt($lhs, $rhs); }
  | math_expr[lhs] NE math_expr[rhs]  { $result = new_ast_ne($lhs, $rhs); }
  | math_expr[lhs] NE_ math_expr[rhs]  { $result = new_ast_ne($lhs, $rhs); }
  | math_expr[lhs] GE math_expr[rhs]  { $result = new_ast_ge($lhs, $rhs); }
  | math_expr[lhs] LE math_expr[rhs]  { $result = new_ast_le($lhs, $rhs); }
  | math_expr[lhs] NOT_IN '(' expr_list ')'  { $result = new_ast_not_in($lhs, $expr_list); }
  | math_expr[lhs] NOT_IN '(' select_stmt ')'  { $result = new_ast_not_in($lhs, $select_stmt); }
  | math_expr[lhs] IN '(' expr_list ')'  { $result = new_ast_in_pred($lhs, $expr_list); }
  | math_expr[lhs] IN '(' select_stmt ')'  { $result = new_ast_in_pred($lhs, $select_stmt); }
  | math_expr[lhs] LIKE math_expr[rhs]  { $result = new_ast_like($lhs, $rhs); }
  | math_expr[lhs] NOT_LIKE math_expr[rhs] { $result = new_ast_not_like($lhs, $rhs); }
  | math_expr[lhs] MATCH math_expr[rhs]  { $result = new_ast_match($lhs, $rhs); }
  | math_expr[lhs] NOT_MATCH math_expr[rhs] { $result = new_ast_not_match($lhs, $rhs); }
  | math_expr[lhs] REGEXP math_expr[rhs]  { $result = new_ast_regexp($lhs, $rhs); }
  | math_expr[lhs] NOT_REGEXP math_expr[rhs] { $result = new_ast_not_regexp($lhs, $rhs); }
  | math_expr[lhs] GLOB math_expr[rhs]  { $result = new_ast_glob($lhs, $rhs); }
  | math_expr[lhs] NOT_GLOB math_expr[rhs] { $result = new_ast_not_glob($lhs, $rhs); }
  | math_expr[lhs] BETWEEN math_expr[me1] %prec BETWEEN AND math_expr[me2]  { $result = new_ast_between($lhs, new_ast_range($me1,$me2)); }
  | math_expr[lhs] NOT_BETWEEN math_expr[me1] %prec BETWEEN AND math_expr[me2]  { $result = new_ast_not_between($lhs, new_ast_range($me1,$me2)); }
  | math_expr[lhs] IS_NOT math_expr[rhs]  { $result = new_ast_is_not($lhs, $rhs); }
  | math_expr[lhs] IS math_expr[rhs]  { $result = new_ast_is($lhs, $rhs); }
  | math_expr[lhs] CONCAT math_expr[rhs]  { $result = new_ast_concat($lhs, $rhs); }
  | math_expr[lhs] COLLATE name { $result = new_ast_collate($lhs, $name); }
  ;

expr[result]:
  math_expr { $result = $math_expr; }
  | expr[lhs] AND expr[rhs]  { $result = new_ast_and($lhs, $rhs); }
  | expr[lhs] OR expr[rhs]  { $result = new_ast_or($lhs, $rhs); }
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

column_calculation:
  COLUMNS '(' col_calcs ')' {
    $column_calculation = new_ast_column_calculation($col_calcs, NULL); }
  | COLUMNS '(' DISTINCT col_calcs ')' {
    $column_calculation = new_ast_column_calculation($col_calcs, new_ast_distinct()); }
  ;

col_calcs[result]:
  col_calc  { $result = new_ast_col_calcs($col_calc, NULL); }
  | col_calc ',' col_calcs[list] { $result = new_ast_col_calcs($col_calc, $list); }
  ;

col_calc:
  name { $col_calc = new_ast_col_calc($name, NULL); }
  | shape_def { $col_calc = new_ast_col_calc(NULL, $shape_def); }
  | name shape_def { $col_calc = new_ast_col_calc($name, $shape_def); }
  | name[n1] '.' name[n2] { $col_calc = new_ast_col_calc(new_ast_dot($n1, $n2), NULL); }
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

cte_decl:
  name '(' name_list ')'  { $cte_decl = new_ast_cte_decl($name, $name_list); }
  | name '(' '*' ')'  { $cte_decl = new_ast_cte_decl($name, new_ast_star()); }
  ;

shared_cte:
  call_stmt { $shared_cte = new_ast_shared_cte($call_stmt, NULL); }
  | call_stmt USING cte_binding_list { $shared_cte = new_ast_shared_cte($call_stmt, $cte_binding_list); }
  ;

cte_table:
  cte_decl AS '(' select_stmt ')'  { $cte_table = new_ast_cte_table($cte_decl, $select_stmt); }
  | cte_decl AS '(' shared_cte')' { $cte_table = new_ast_cte_table($cte_decl, $shared_cte); }
  | '(' call_stmt ')' {
      ast_node *name = $call_stmt->left;
      ast_node *cte_decl =  new_ast_cte_decl(name, new_ast_star());
      ast_node *shared_cte = new_ast_shared_cte($call_stmt, NULL);
      $cte_table = new_ast_cte_table(cte_decl, shared_cte); }
  | '(' call_stmt USING cte_binding_list ')' {
      ast_node *name = $call_stmt->left;
      ast_node *cte_decl =  new_ast_cte_decl(name, new_ast_star());
      ast_node *shared_cte = new_ast_shared_cte($call_stmt, $cte_binding_list);
      $cte_table = new_ast_cte_table(cte_decl, shared_cte); }
  | cte_decl LIKE '(' select_stmt ')'  {
      $cte_table = new_ast_cte_table($cte_decl, new_ast_like($select_stmt, NULL)); }
  | cte_decl LIKE name  {
      $cte_table = new_ast_cte_table($cte_decl, new_ast_like($name, NULL)); }
  ;

with_prefix:
  WITH cte_tables  { $with_prefix = new_ast_with($cte_tables); }
  | WITH RECURSIVE cte_tables  { $with_prefix = new_ast_with_recursive($cte_tables); }
  ;

with_select_stmt:
  with_prefix select_stmt_no_with  { $with_select_stmt = new_ast_with_select_stmt($with_prefix, $select_stmt_no_with); }
  ;

select_nothing_stmt:
  SELECT NOTHING { $select_nothing_stmt = new_ast_select_nothing_stmt(); }
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

schema_unsub_stmt:
  AT_UNSUB version_annotation { $schema_unsub_stmt = new_ast_schema_unsub_stmt($version_annotation); }
  ;

schema_resub_stmt:
  AT_RESUB version_annotation { $schema_resub_stmt = new_ast_schema_resub_stmt($version_annotation); }
  ;

schema_ad_hoc_migration_stmt:
  AT_SCHEMA_AD_HOC_MIGRATION version_annotation
    { $schema_ad_hoc_migration_stmt = new_ast_schema_ad_hoc_migration_stmt($version_annotation, NULL); }
  | AT_SCHEMA_AD_HOC_MIGRATION FOR AT_RECREATE '(' name[group] ',' name[proc] ')'
    { $schema_ad_hoc_migration_stmt = new_ast_schema_ad_hoc_migration_stmt($group, $proc); }
  ;

emit_enums_stmt:
  AT_EMIT_ENUMS opt_name_list { $emit_enums_stmt = new_ast_emit_enums_stmt($opt_name_list); }
  ;

emit_group_stmt:
  AT_EMIT_GROUP opt_name_list { $emit_group_stmt = new_ast_emit_group_stmt($opt_name_list); }
  ;

emit_constants_stmt:
  AT_EMIT_CONSTANTS name_list { $emit_constants_stmt = new_ast_emit_constants_stmt($name_list); }
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
  expr  { $groupby_item = new_ast_groupby_item($expr); }
  ;

opt_asc_desc:
  /* nil */  { $opt_asc_desc = NULL; }
  | ASC  opt_nullsfirst_nullslast { $opt_asc_desc = new_ast_asc($opt_nullsfirst_nullslast); }
  | DESC  opt_nullsfirst_nullslast { $opt_asc_desc = new_ast_desc($opt_nullsfirst_nullslast); }
  ;

opt_nullsfirst_nullslast:
  /* nil */  { $opt_nullsfirst_nullslast = NULL; }
  | NULLS FIRST  { $opt_nullsfirst_nullslast = new_ast_nullsfirst(); }
  | NULLS LAST  { $opt_nullsfirst_nullslast = new_ast_nullslast(); }
  ;

opt_having:
  /* nil */  { $opt_having = NULL; }
  | HAVING expr  { $opt_having = new_ast_opt_having($expr); }
  ;

opt_orderby:
  /* nil */  { $opt_orderby = NULL; }
  | ORDER BY orderby_list  { $opt_orderby = new_ast_opt_orderby($orderby_list); }
  ;

orderby_list[result]:
  orderby_item  { $result = new_ast_orderby_list($orderby_item, NULL); }
  | orderby_item ',' orderby_list[gl]  { $result = new_ast_orderby_list($orderby_item, $gl); }
  ;

orderby_item:
  expr opt_asc_desc  { $orderby_item = new_ast_orderby_item($expr, $opt_asc_desc); }
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
  | name '.' '*'  { $select_expr = new_ast_table_star($name); }
  | column_calculation  { $select_expr = $column_calculation; }
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
  | '(' shared_cte ')' opt_as_alias  { $table_or_subquery = new_ast_table_or_subquery($shared_cte, $opt_as_alias); }
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
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values); }
  | insert_stmt_type name USING select_stmt {
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, $select_stmt);
    $insert_stmt_type->left = NULL; // dummy spec not allowed in this form
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values);
  }
  | insert_stmt_type name USING expr_names opt_insert_dummy_spec {
    struct ast_node *name_columns_values = new_ast_name_columns_values($name, $expr_names);
    $insert_stmt_type->left = $opt_insert_dummy_spec;
    $insert_stmt = new_ast_insert_stmt($insert_stmt_type, name_columns_values); }
  ;

insert_list_item:
  expr { $insert_list_item = $expr; }
  | shape_arguments  {$insert_list_item = $shape_arguments; }
  ;

insert_list[result]:
  /* nil */  { $result = NULL; }
  | insert_list_item { $result = new_ast_insert_list($insert_list_item, NULL); }
  | insert_list_item ',' insert_list[il]  { $result = new_ast_insert_list($insert_list_item, $il); }
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

function: FUNC | FUNCTION
  ;

declare_out_call_stmt:
  DECLARE OUT call_stmt { $declare_out_call_stmt = new_ast_declare_out_call_stmt($call_stmt); }
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

declare_const_stmt:
  DECLARE CONST GROUP name '(' const_values ')' {
    $declare_const_stmt = new_ast_declare_const_stmt($name, $const_values); }
  ;

declare_group_stmt:
  DECLARE GROUP name BEGIN_ simple_variable_decls END {
    $declare_group_stmt = new_ast_declare_group_stmt($name, $simple_variable_decls); }
  ;

simple_variable_decls[result]:
  declare_simple_var_stmt[cur] ';' { $result = new_ast_stmt_list($cur, NULL); }
  | declare_simple_var_stmt[cur] ';' simple_variable_decls[next] { $result = new_ast_stmt_list($cur, $next); }
  ;

const_values[result]:
   const_value { $result = new_ast_const_values($const_value, NULL);  }
  | const_value ',' const_values[next] { $result = new_ast_const_values($const_value, $next); }
  ;

const_value:  name '=' expr { $const_value = new_ast_const_value($name, $expr); }
  ;

declare_select_func_no_check_stmt:
  DECLARE SELECT function name NO CHECK data_type_with_options {
    $declare_select_func_no_check_stmt = new_ast_declare_select_func_no_check_stmt($name, new_ast_func_params_return(NULL, $data_type_with_options)); }
  | DECLARE SELECT function name NO CHECK '(' typed_names ')' {
    $declare_select_func_no_check_stmt = new_ast_declare_select_func_no_check_stmt($name, new_ast_func_params_return(NULL, $typed_names)); }
  ;

declare_func_stmt:
  DECLARE function name '(' func_params ')' data_type_with_options  {
      $declare_func_stmt = new_ast_declare_func_stmt($name, new_ast_func_params_return($func_params, $data_type_with_options)); }
  | DECLARE SELECT function name '(' params ')' data_type_with_options  {
      $declare_func_stmt = new_ast_declare_select_func_stmt($name, new_ast_func_params_return($params, $data_type_with_options)); }
  | DECLARE function name '(' func_params ')' CREATE data_type_with_options  {
      ast_node *create_data_type = new_ast_create_data_type($data_type_with_options);
      $declare_func_stmt = new_ast_declare_func_stmt($name, new_ast_func_params_return($func_params, create_data_type)); }
  | DECLARE SELECT function name '(' params ')' '(' typed_names ')'  {
      $declare_func_stmt = new_ast_declare_select_func_stmt($name, new_ast_func_params_return($params, $typed_names)); }
  ;

procedure: PROC | PROCEDURE
  ;

declare_proc_no_check_stmt:
  DECLARE procedure name NO CHECK { $declare_proc_no_check_stmt = new_ast_declare_proc_no_check_stmt($name); }
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

declare_interface_stmt:
  DECLARE INTERFACE name '(' typed_names ')'  {
      $declare_interface_stmt = new_ast_declare_interface_stmt($name, new_ast_proc_params_stmts(NULL, $typed_names)); }

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
  name data_type_with_options  { $typed_name = new_ast_typed_name($name, $data_type_with_options); }
  | shape_def  { $typed_name = new_ast_typed_name(NULL, $shape_def); }
  | name shape_def  { $typed_name = new_ast_typed_name($name, $shape_def); }
  ;

typed_names[result]:
  typed_name  { $result = new_ast_typed_names($typed_name, NULL); }
  | typed_name ',' typed_names[tn]  { $result = new_ast_typed_names($typed_name, $tn);}
  ;

func_param:
  param { $func_param = $param; }
  | name CURSOR { $func_param = new_ast_param(NULL, new_ast_param_detail($name, new_ast_type_cursor())); }
  ;

func_params[result]:
  /* nil */  { $result = NULL; }
  | func_param  { $result = new_ast_params($func_param, NULL); }
  |  func_param ',' func_params[par]  { $result = new_ast_params($func_param, $par); }
  ;

param:
  name data_type_with_options  { $param = new_ast_param(NULL, new_ast_param_detail($name, $data_type_with_options)); }
  | inout name data_type_with_options  { $param = new_ast_param($inout, new_ast_param_detail($name, $data_type_with_options)); }
  | shape_def  { $param = new_ast_param(NULL, new_ast_param_detail(NULL, $shape_def)); }
  | name shape_def  { $param = new_ast_param(NULL, new_ast_param_detail($name, $shape_def)); }
  ;

params[result]:
  /* nil */  { $result = NULL; }
  | param  { $result = new_ast_params($param, NULL); }
  |  param ',' params[par]  { $result = new_ast_params($param, $par); }
  ;

/* these forms are just storage */
declare_simple_var_stmt:
  DECLARE name_list data_type_with_options  { $declare_simple_var_stmt = new_ast_declare_vars_type($name_list, $data_type_with_options); }
  | VAR name_list data_type_with_options  { $declare_simple_var_stmt = new_ast_declare_vars_type($name_list, $data_type_with_options); }
  | DECLARE name CURSOR shape_def  { $declare_simple_var_stmt = new_ast_declare_cursor_like_name($name, $shape_def); }
  | DECLARE name CURSOR LIKE select_stmt  { $declare_simple_var_stmt = new_ast_declare_cursor_like_select($name, $select_stmt); }
  | DECLARE name CURSOR LIKE '(' typed_names ')' { $declare_simple_var_stmt = new_ast_declare_cursor_like_typed_names($name, $typed_names); }
  ;

/* the additional forms are just about storage */
declare_stmt:
  declare_simple_var_stmt { $declare_stmt = $declare_simple_var_stmt; }
  | DECLARE name CURSOR FOR select_stmt  { $declare_stmt = new_ast_declare_cursor($name, $select_stmt); }
  | DECLARE name CURSOR FOR explain_stmt  { $declare_stmt = new_ast_declare_cursor($name, $explain_stmt); }
  | DECLARE name CURSOR FOR call_stmt  { $declare_stmt = new_ast_declare_cursor($name, $call_stmt); }
  | DECLARE name CURSOR FETCH FROM call_stmt  { $declare_stmt = new_ast_declare_value_cursor($name, $call_stmt); }
  | DECLARE name[id] CURSOR FOR expr { $declare_stmt = new_ast_declare_cursor($id, $expr); }
  | DECLARE name TYPE data_type_with_options { $declare_stmt = new_ast_declare_named_type($name, $data_type_with_options); }
  ;

call_stmt:
  CALL name '(' ')'  {
      YY_ERROR_ON_CQL_INFERRED_NOTNULL($name);
      $call_stmt = new_ast_call_stmt($name, NULL); }
  | CALL name '(' call_expr_list ')'  {
      YY_ERROR_ON_CQL_INFERRED_NOTNULL($name);
      $call_stmt = new_ast_call_stmt($name, $call_expr_list); }
  | CALL name '(' '*' ')'  {
      YY_ERROR_ON_CQL_INFERRED_NOTNULL($name);
      // sugar form -- this is the same as
      // CALL name ( FROM LOCALS LIKE name ARGUMENTS) -- i.e. all arg names that match
      ast_node *like = new_ast_like($name, $name);
      ast_node *shape_def = new_ast_shape_def(like, NULL);
      ast_node *call_expr = new_ast_from_shape(new_ast_str("LOCALS"), shape_def);
      ast_node *call_expr_list = new_ast_expr_list(call_expr, NULL);
      $call_stmt = new_ast_call_stmt($name, call_expr_list); }
  ;

while_stmt:
  WHILE expr BEGIN_ opt_stmt_list END  { $while_stmt = new_ast_while_stmt($expr, $opt_stmt_list); }
  ;

switch_stmt:
  SWITCH expr switch_case switch_cases {
    ast_node *cases = new_ast_switch_case($switch_case, $switch_cases);
    ast_node *switch_body = new_ast_switch_body($expr, cases);
    $switch_stmt = new_ast_switch_stmt(new_ast_opt(0), switch_body);  }
  | SWITCH expr ALL VALUES switch_case switch_cases {
    ast_node *cases = new_ast_switch_case($switch_case, $switch_cases);
    ast_node *switch_body = new_ast_switch_body($expr, cases);
    $switch_stmt = new_ast_switch_stmt(new_ast_opt(1), switch_body);  }
  ;

switch_case:
  WHEN expr_list THEN stmt_list { $switch_case = new_ast_connector($expr_list, $stmt_list); }
  | WHEN expr_list THEN NOTHING { $switch_case = new_ast_connector($expr_list, NULL); }
  ;

switch_cases[result]:
  switch_case switch_cases[next] {
    $result = new_ast_switch_case($switch_case, $next); }
  | ELSE stmt_list END {
    ast_node *conn = new_ast_connector(NULL, $stmt_list);
    $result = new_ast_switch_case(conn, NULL); }
  | END { $result = NULL; }
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

fetch_cursor_from_blob_stmt:
  FETCH name FROM_BLOB expr { $fetch_cursor_from_blob_stmt = new_ast_fetch_cursor_from_blob_stmt($name, $expr); }
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

close_stmt:
  CLOSE name  { $close_stmt = new_ast_close_stmt($name); }
  ;

out_stmt:
  OUT name  { $out_stmt = new_ast_out_stmt($name); }
  ;

out_union_stmt:
  OUT UNION name  { $out_union_stmt = new_ast_out_union_stmt($name); }
  ;

out_union_parent_child_stmt:
  OUT UNION call_stmt JOIN child_results { $out_union_parent_child_stmt = new_ast_out_union_parent_child_stmt($call_stmt, $child_results); }
  ;

child_results[result]:
   child_result { $result = new_ast_child_results($child_result, NULL); }
   | child_result AND child_results[next] { $result = new_ast_child_results($child_result, $next); }
   ;

child_result:
  call_stmt USING '(' name_list ')' { $child_result = new_ast_child_result($call_stmt, new_ast_named_result(NULL, $name_list)); }
  | call_stmt USING '(' name_list ')' AS name { $child_result = new_ast_child_result($call_stmt, new_ast_named_result($name, $name_list)); }
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

control_stmt:
  commit_return_stmt  { $control_stmt = $commit_return_stmt; }
  | continue_stmt  { $control_stmt = $continue_stmt; }
  | leave_stmt  { $control_stmt = $leave_stmt; }
  | return_stmt  { $control_stmt = $return_stmt; }
  | rollback_return_stmt  { $control_stmt = $rollback_return_stmt; }
  | throw_stmt  { $control_stmt = $throw_stmt; }

guard_stmt:
  IF expr control_stmt  { $guard_stmt = new_ast_guard_stmt($expr, $control_stmt); }
  ;

transaction_mode:
  /* nil */ { $transaction_mode = TRANS_DEFERRED; }
  | DEFERRED { $transaction_mode = TRANS_DEFERRED; }
  | IMMEDIATE { $transaction_mode = TRANS_IMMEDIATE; }
  | EXCLUSIVE { $transaction_mode = TRANS_EXCLUSIVE; }
  ;

begin_trans_stmt:
  BEGIN_ transaction_mode TRANSACTION  { $begin_trans_stmt = new_ast_begin_trans_stmt(new_ast_opt($transaction_mode)); }
  | BEGIN_ transaction_mode { $begin_trans_stmt = new_ast_begin_trans_stmt(new_ast_opt($transaction_mode)); }
  ;

rollback_trans_stmt:
  ROLLBACK  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt(NULL); }
  | ROLLBACK TRANSACTION  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt(NULL); }
  | ROLLBACK TO savepoint_name  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt($savepoint_name); }
  | ROLLBACK TRANSACTION TO savepoint_name  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt($savepoint_name); }
  | ROLLBACK TO SAVEPOINT savepoint_name  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt($savepoint_name); }
  | ROLLBACK TRANSACTION TO SAVEPOINT savepoint_name  {
      $rollback_trans_stmt = new_ast_rollback_trans_stmt($savepoint_name); }
  ;

commit_trans_stmt:
  COMMIT TRANSACTION  { $commit_trans_stmt = new_ast_commit_trans_stmt(); }
  | COMMIT { $commit_trans_stmt = new_ast_commit_trans_stmt(); }
  ;

proc_savepoint_stmt:  procedure SAVEPOINT BEGIN_ opt_stmt_list END {
    $proc_savepoint_stmt = new_ast_proc_savepoint_stmt($opt_stmt_list);
  }
  ;

savepoint_name:
  AT_PROC { $savepoint_name = new_ast_str("@PROC"); }
  | name { $savepoint_name = $name; }
  ;

savepoint_stmt:
  SAVEPOINT savepoint_name  {
    $savepoint_stmt = new_ast_savepoint_stmt($savepoint_name); }
  ;

release_savepoint_stmt:
  RELEASE savepoint_name  {
    $release_savepoint_stmt = new_ast_release_savepoint_stmt($savepoint_name); }
  | RELEASE SAVEPOINT savepoint_name  {
    $release_savepoint_stmt = new_ast_release_savepoint_stmt($savepoint_name); }
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
  | WITHOUT ROWID  { $enforcement_options = new_ast_opt(ENFORCE_WITHOUT_ROWID); }
  | TRANSACTION { $enforcement_options = new_ast_opt(ENFORCE_TRANSACTION); }
  | SELECT IF NOTHING { $enforcement_options = new_ast_opt(ENFORCE_SELECT_IF_NOTHING); }
  | INSERT SELECT { $enforcement_options = new_ast_opt(ENFORCE_INSERT_SELECT); }
  | TABLE FUNCTION { $enforcement_options = new_ast_opt(ENFORCE_TABLE_FUNCTION); }
  | ENCODE CONTEXT_COLUMN { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_COLUMN); }
  | ENCODE CONTEXT_TYPE INTEGER { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_INTEGER); }
  | ENCODE CONTEXT_TYPE LONG_INTEGER { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_LONG_INTEGER); }
  | ENCODE CONTEXT_TYPE REAL { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_REAL); }
  | ENCODE CONTEXT_TYPE BOOL_ { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_BOOL); }
  | ENCODE CONTEXT_TYPE TEXT { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_TEXT); }
  | ENCODE CONTEXT_TYPE BLOB { $enforcement_options = new_ast_opt(ENFORCE_ENCODE_CONTEXT_TYPE_BLOB); }
  | IS_TRUE { $enforcement_options = new_ast_opt(ENFORCE_IS_TRUE); }
  | CAST { $enforcement_options = new_ast_opt(ENFORCE_CAST); }
  | SIGN_FUNCTION { $enforcement_options = new_ast_opt(ENFORCE_SIGN_FUNCTION); }
  | CURSOR_HAS_ROW { $enforcement_options = new_ast_opt(ENFORCE_CURSOR_HAS_ROW); }
  ;

enforce_strict_stmt:
  AT_ENFORCE_STRICT enforcement_options  { $enforce_strict_stmt = new_ast_enforce_strict_stmt($enforcement_options); }
  ;

enforce_normal_stmt:
  AT_ENFORCE_NORMAL enforcement_options  { $enforce_normal_stmt = new_ast_enforce_normal_stmt($enforcement_options); }
  ;

enforce_reset_stmt:
  AT_ENFORCE_RESET { $enforce_reset_stmt = new_ast_enforce_reset_stmt(); }
  ;

enforce_push_stmt:
  AT_ENFORCE_PUSH { $enforce_push_stmt = new_ast_enforce_push_stmt(); }
  ;

enforce_pop_stmt:
  AT_ENFORCE_POP { $enforce_pop_stmt = new_ast_enforce_pop_stmt(); }
  ;

opt_use_offset:
  /* nil */ { $opt_use_offset = new_ast_opt(0); }
  | OFFSET  { $opt_use_offset = new_ast_opt(1); }
  ;

blob_get_key_type_stmt:
  AT_BLOB_GET_KEY_TYPE name { $blob_get_key_type_stmt = new_ast_blob_get_key_type_stmt($name); }
  ;

blob_get_val_type_stmt:
  AT_BLOB_GET_VAL_TYPE name { $blob_get_val_type_stmt = new_ast_blob_get_val_type_stmt($name); }
  ;

blob_get_key_stmt:
  AT_BLOB_GET_KEY name opt_use_offset { $blob_get_key_stmt = new_ast_blob_get_key_stmt($name, $opt_use_offset); }
  ;

blob_get_val_stmt:
  AT_BLOB_GET_VAL name opt_use_offset { $blob_get_val_stmt = new_ast_blob_get_val_stmt($name, $opt_use_offset); }
  ;

blob_create_key_stmt:
  AT_BLOB_CREATE_KEY name opt_use_offset { $blob_create_key_stmt = new_ast_blob_create_key_stmt($name, $opt_use_offset); }
  ;

blob_create_val_stmt:
  AT_BLOB_CREATE_VAL name opt_use_offset { $blob_create_val_stmt = new_ast_blob_create_val_stmt($name, $opt_use_offset); }
  ;

blob_update_key_stmt:
  AT_BLOB_UPDATE_KEY name opt_use_offset  { $blob_update_key_stmt = new_ast_blob_update_key_stmt($name, $opt_use_offset); }
  ;

blob_update_val_stmt:
  AT_BLOB_UPDATE_VAL name opt_use_offset { $blob_update_val_stmt = new_ast_blob_update_val_stmt($name, $opt_use_offset); }
  ;

%%

#pragma clang diagnostic pop

void yyerror(const char *format, ...) {
  extern int yylineno;
  va_list args;
  va_start(args, format);

  CHARBUF_OPEN(err);
  bprintf(&err, "%s:%d:1: error: ", current_file, yylineno);
  vbprintf(&err, format, args);
  bputc(&err, '\n');
  cql_emit_error(err.ptr);
  CHARBUF_CLOSE(err);
  va_end(args);

  parse_error_occurred = true;
}

static uint64_t next_id = 0;

static void print_dot(struct ast_node *node) {
  assert(node);
  uint64_t id = next_id++;

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

static int32_t gather_arg_param(int32_t a, int32_t argc, char **argv, char **out_arg, const char *errmsg);

static void parse_cmd(int argc, char **argv) {

  if (argc == 1) {
    cql_usage();
    cql_cleanup_and_exit(0);
  }

  // default result type
  options.rt = "c";
  rt = find_rtdata(options.rt);
  Invariant(rt);

  current_file = "<stdin>";

  // This code is generally not something you want on but it can be useful
  // if you are trying to diagnose a complex failure in a larger build and
  // you need to see what the executions were.  It can also be helpful if
  // you are using CQL in its amalgam form. Though, in that case, the
  // fprintf probably needs to be modified.

  // #define CQL_EXEC_TRACING 1
  #ifdef CQL_EXEC_TRACING

  CHARBUF_OPEN(args);
  bprintf(&args, "cql ");
  for (int32_t i = 1; i < argc; i++) {
    bprintf(&args, "%s%s", argv[i], i == argc - 1 ? "\n" : " ");
  }
  FILE *tr = fopen("/tmp/cqltrace.log", "a+");
  fprintf(tr, "%s", args.ptr);
  fclose(tr);
  CHARBUF_CLOSE(args);

  #endif

  for (int32_t a = 1; a < argc; a++) {
    char *arg = argv[a];
    if (strcmp(arg, "--echo") == 0) {
      options.echo_input = 1;
    } else if (strcmp(arg, "--ast") == 0) {
      options.print_ast = 1;
    } else if (strcmp(arg, "--nolines") == 0) {
      options.nolines = 1;
    } else if (strcmp(arg, "--schema_exclusive") == 0) {
      options.schema_exclusive = 1;
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
    } else if (strcmp(arg, "--cqlrt") == 0) {
      a = gather_arg_param(a, argc, argv, &options.cqlrt, "for the name of the runtime header");
    } else if (strcmp(arg, "--rt") == 0) {
      a = gather_arg_param(a, argc, argv, &options.rt, "(e.g., c, objc, java, json_schema)");
      rt = find_rtdata(options.rt);
      if (!rt) {
        cql_error("unknown cg runtime '%s'\n", options.rt);
        cql_cleanup_and_exit(1);
      }
    } else if (strcmp(arg, "--test") == 0) {
      options.test = 1;
    } else if (strcmp(arg, "--dev") == 0) {
      options.dev = 1;
    } else if (strcmp(arg, "--in") == 0) {
      a = gather_arg_param(a, argc, argv, NULL, "for the file name");
      FILE *f = fopen(argv[a], "r");
      if (!f) {
        cql_error("unable to open '%s' for read\n", argv[a]);
        cql_cleanup_and_exit(1);
      }
      yyset_in(f);
      // reset the scanner to point to the newly input file (yyset_in(f)). Otherwise the scanner
      // might continue to point to the input file from the previous run in case there are still
      // a stream to read.
      // Usually when the parser encouter a syntax error, it stops reading the input file.
      // On the next run the scanner will want to continue and finish from where it stops
      // before moving to the file of the current run.
      // Therefore it's important to always do this because we're in a new run and should ignore
      // previous run because a result were already produced for that prevous run.
      yyrestart(f);

      current_file = argv[a];
    } else if (strcmp(arg, "--min_schema_version") == 0) {
      a = gather_arg_param(a, argc, argv, NULL, "for the minimum schema version");
      options.min_schema_version = atoi(argv[a]);
    } else if (strcmp(arg, "--global_proc") == 0) {
      a = gather_arg_param(a, argc, argv, NULL,  "for the global proc name");
      global_proc_name = argv[a];
    } else if (strcmp(arg, "--c_include_path") == 0) {
      a = gather_arg_param(a, argc, argv, &options.c_include_path, "for the include path of a C header");
    } else if (strcmp(arg, "--objc_c_include_path") == 0) {
      a = gather_arg_param(a, argc, argv, &options.objc_c_include_path, "for the include path of a C header");
    } else if (strcmp(arg, "--c_include_namespace") == 0) {
      a = gather_arg_param(a, argc, argv, &options.c_include_namespace, "for the C include namespace");
    } else if (strcmp(arg, "--java_package_name") == 0) {
      a = gather_arg_param(a, argc, argv, &options.java_package_name, "for the Java package name");
    } else if (strcmp(arg, "--java_fragment_interface_mode") == 0) {
      options.java_fragment_interface_mode = true;
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

cql_noexport CSTR cql_builtin_text() {
  return
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_partition_create () CREATE OBJECT<partitioning> NOT NULL;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_partition_cursor (p OBJECT<partitioning> NOT NULL, key CURSOR, value CURSOR) BOOL NOT NULL;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_extract_partition (p OBJECT<partitioning> NOT NULL, key CURSOR) CREATE OBJECT NOT NULL;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_string_dictionary_create() CREATE OBJECT<string_dictionary> NOT NULL;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_string_dictionary_add(dict OBJECT<string_dictionary> NOT NULL, key TEXT NOT NULL, value TEXT NOT NULL) BOOL NOT NULL;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_string_dictionary_find(dict OBJECT<string_dictionary> NOT NULL, key TEXT) TEXT;"
    "@attribute(cql:builtin)"
    "DECLARE FUNC cql_cursor_format(C CURSOR) CREATE TEXT NOT NULL;";
}

int cql_main(int argc, char **argv) {
  exit_code = 0;
  yylineno = 1;

  if (!setjmp(for_exit)) {
    parse_cmd(argc, argv);
    ast_init();

    // add the builtin declares before we process the real input
    cql_setup_for_builtins();

    if (options.run_unit_tests) {
      run_unit_tests();
    } else if (yyparse()) {
      cql_exit_on_parse_errors();
    }
  }

  cg_c_cleanup();
  sem_cleanup();
  ast_cleanup();
  parse_cleanup();
  gen_cleanup();
  rt_cleanup();
  cg_java_cleanup();

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

static void cql_exit_on_parse_errors() {
  cql_error("Parse errors found, no further passes will run.\n");
  cql_cleanup_and_exit(2);
}

static void parse_cleanup() {
  parse_error_occurred = false;
}

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
    cql_cleanup_and_exit(1);
  }

  return a;
}

static int32_t gather_arg_param(int32_t a, int32_t argc, char **argv, char **out_arg, const char *errmsg) {
  if (a + 1 < argc) {
    a++;
    if (out_arg) {
      *out_arg = argv[a];
    }
  } else {
    cql_error("%s requires an additional param%s%s.\n", argv[a], errmsg ? " " : "", errmsg);
    cql_cleanup_and_exit(1);
  }

  return a;
}

extern int yylineno;

void line_directive(const char *directive) {
  char *directive_start = strchr(directive, '#');
  Invariant(directive_start != NULL);
  char *line_start = strchr(directive_start + 1, ' ');
  Invariant(line_start != NULL);
  int line = atoi(line_start + 1);
  yyset_lineno(line -1);  // we are about to process the linefeed

  char *q1 = strchr(directive_start +1, '"');
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
// You can define it to be a method of your choice with
// "#define cql_emit_error your_method" and then your method will get
// the data instead. This will be whatever output the
// compiler would have emitted to to stderr.  This includes semantic
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
// You can define it to be a method of your choice with
// "#define cql_emit_output your_method" and then your method will get
// the data instead. This will be whatever output the
// compiler would have emitted to to stdout.  This is usually
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

FILE *_Nonnull cql_open_file_for_write(const char *_Nonnull file_name) {
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
// then write it to a location of your choice.
// You must copy the memory if you intend to keep it. "data" will be freed.

// Note: you *may* use cql_cleanup_and_exit to force a failure from within
// this API.  That's a normal failure mode that is well-tested.

void cql_write_file(const char *_Nonnull file_name, const char *_Nonnull data) {
  FILE *file = cql_open_file_for_write(file_name);
  fprintf(file, "%s", data);
  fclose(file);
}

#endif

static void cql_usage() {
  cql_emit_output(
    "Usage:\n"
    "--in file\n"
    "  reads the given file for the input instead of stdin\n"
    "--sem\n"
    "  performs semantic analysis on the input file ONLY\n"
    "--ast\n"
    "  prints the internal AST to stdout\n"
    "--echo\n"
    "  echoes the input in normalized form from the AST\n"
    "--dot\n"
    "  prints the internal AST to stdout in DOT format for graph visualization\n"
    "--cg output1 output2 ...\n"
    "  codegen into the named outputs\n"
    "  any number of output files may be needed for a particular result type, two is common\n"
    "--nolines\n"
    "  suppress the #line directives for lines; useful if you need to debug the C code\n"
    "--global_proc name\n"
    "  any loose SQL statements not in a stored proc are gathered and put into a procedure of the given name\n"
    "--compress\n"
    "  compresses SQL text into fragements that can be assembled into queries to save space\n"
    "--test\n"
    "  some of the output types can include extra diagnostics if --test is included\n"
    "--dev\n"
    "  some codegen features only make sense during development, this enables dev mode\n"
    "  example: explain query plans\n"
    "\n"
    "Result Types (--rt *) These are the various outputs the compiler can produce.\n"
    "\n"
    "--rt c\n"
    "  this is the standard C compilation of the sql file\n"
    "  requires two output files (foo.h and foo.c)\n"
    "--rt objc\n"
    "  Objective-C wrappers for result sets produced by the stored procedures in the input\n"
    "  requires one output file (foo.h)\n"
    "--rt java\n"
    "  java wrappers for result sets produced by the stored procedures in the input\n"
    "  requires one output file (foo.java)\n"
    "--rt json_schema\n"
    "  produces JSON output suitable for consumption by downstream codegen tools\n"
    "  requires one output file (foo.json)\n"
    "--rt schema\n"
    "  produces the canonical schema for the given input files; stored procedures etc. are removed\n"
    "  requires one output file\n"
    "--rt schema_upgrade\n"
    "  produces a CQL schema upgrade script which can then be compiled with CQL itself\n"
    "  requires one output file (foo.sql)\n"
    "--rt query_plan\n"
    "  produces a set of helper procedures that create a query plan for every DML statement in the input\n"
    "  requires one output file (foo_queryplans.sql)\n"
    "--rt stats\n"
    "  produces a simple .csv file with node count information for AST nodes per procedure in the input\n"
    "  requires one output file (foo.csv)\n"
    "--rt udf\n"
    "  generates stub implementations of all UDFs needed in the input, for use with --rt query_plan\n"
    "  requires two output files (udfs.h, udfs.c)\n"
    "\n"
    "--include_regions a b c\n"
    "  the indicated regions will be declared;\n"
    "  used with --rt schema_upgrade or --rt schema\n"
    "--exclude_regions x y z\n"
    "  the indicated regions will still be declared but the upgrade code will be suppressed\n"
    "  used with --rt schema_upgrade\n"
    "--min_schema_version n\n"
    "  the schema upgrade script will not include upgrade steps for schema older than the version specified\n"
    "  used with --rt schema_upgrade\n"
    "--schema_exclusive\n"
    "  the schema upgrade script assumes it owns all the schema in the database, it aggressively removes other things\n"
    "  used with --rt schema_upgrade\n"
    "--java_package_name name\n"
    "  specifies the name of package a generated java class will be a part of\n"
    "--java_fragment_interface_mode\n"
    "  sets the Java codegen mode to generate interfaces for base and extension fragments\n"
    "--c_include_namespace\n"
    "  for the C codegen runtimes, headers will be referenced as #include <namespace/file.h>\n"
    "--c_include_path\n"
    "  for C codegen runtimes this will be used to create the #include directive at the start of the C\n"
    "--objc_c_include_path\n"
    "  for ObjC codegen runtimes this represents the header of the C generated code for the same CQL source file\n"
    "--cqlrt foo.h\n"
    "  emits foo.h into the C output instead of cqlrt.h\n"
    "--generate_exports\n"
    "  requires another output file to --cg; it contains the procedure declarations for the input\n"
    "  used with --rt c\n"
    "--generate_type_getters\n"
    "  emits rowset accessors using shared type getters instead of individual functions\n"
    "  this makes them more interoperable if they share columns\n"
    "  used with --rt c\n"
    );
}

static ast_node *make_statement_node(ast_node *misc_attrs, ast_node *any_stmt)
{
  // This is the equivalent of:
  //
  // misc_attrs any_stmt  { $stmt = $misc_attrs ? new_ast_stmt_and_attr($misc_attrs, $any_stmt) : $any_stmt; }

  // Add the most recent doc comment (if any) to any table/view/proc
  if (is_ast_create_proc_stmt(any_stmt) ||
      is_ast_create_view_stmt(any_stmt) ||
      is_ast_create_table_stmt(any_stmt)) {
    CSTR comment = table_comment_saved ? table_comment_saved : get_last_doc_comment();
    if (comment) {
       ast_node *misc_attr_key = new_ast_dot(new_ast_str("cql"), new_ast_str("doc_comment"));
       ast_node *misc_attr = new_ast_misc_attr(misc_attr_key, new_ast_cstr(comment));
       misc_attrs = new_ast_misc_attrs(misc_attr, misc_attrs);
    }
  }

  // in any case, we get one chance to use this
  table_comment_saved = NULL;

  if (misc_attrs) {
     return new_ast_stmt_and_attr(misc_attrs, any_stmt);
  }
  else {
     return any_stmt;
  }
}

// creates a column definition node with a doc comment if needed
static ast_node *make_coldef_node(ast_node *col_def_type_attrs, ast_node *misc_attrs)
{
  // This is the equivalent of:
  //
  // $col_def = new_ast_col_def(col_def_type_attrs, $misc_attrs);
  //
  // (with optional comment node)

  CSTR comment = get_last_doc_comment();
  if (comment) {
     ast_node *misc_attr_key = new_ast_dot(new_ast_str("cql"), new_ast_str("doc_comment"));
     ast_node *misc_attr = new_ast_misc_attr(misc_attr_key, new_ast_cstr(comment));
     misc_attrs = new_ast_misc_attrs(misc_attr, misc_attrs);
  }

  return new_ast_col_def(col_def_type_attrs, misc_attrs);
}

// When a chain of strings appears like this "xxx" "yyy" we reduce it to a single
// string literal by concatenating all the pieces.
static ast_node *reduce_str_chain(ast_node *str_chain) {
  Contract(is_ast_str_chain(str_chain));

  // trivial case, length one chain
  if (!str_chain->right) {
    return str_chain->left;
  }

  CHARBUF_OPEN(tmp);
  CHARBUF_OPEN(result);

  for (ast_node *item = str_chain; item; item = item->right) {
    Invariant(is_ast_str_chain(item));
    Invariant(is_ast_str(item->left));

    str_ast_node *str_node = (str_ast_node *)item->left;
    cg_decode_string_literal(str_node->value, &tmp);
  }

  cg_encode_string_literal(tmp.ptr, &result);
  ast_node *lit = new_ast_str(Strdup(result.ptr));

  // this just forces the literal to be echoed as a C literal
  // so that it is prettier in the echoed output, otherwise no difference
  // all literals are stored in SQL format.
  ((str_ast_node *)lit)->cstr_literal = true;

  CHARBUF_CLOSE(result);
  CHARBUF_CLOSE(tmp);

  return lit;
}

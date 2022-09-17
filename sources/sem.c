/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SEM)

// stubs to avoid link errors,

cql_noexport void sem_main(ast_node *head) {}
cql_noexport void sem_cleanup() {}
cql_noexport void print_sem_type(struct sem_node *sem) {}

#else

// Perform semantic analysis of the various nodes and validate type correctness
// the semantic nodes contain enough information that code can be generated
// include, importantly, data about the shape of any given select statement
// and the type of any expression.

#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include "cg_common.h"
#include "compat.h"
#include "cql.h"
#include "ast.h"
#include "cql.y.h"
#include "sem.h"
#include "charbuf.h"
#include "bytebuf.h"
#include "list.h"
#include "gen_sql.h"
#include "symtab.h"
#include "eval.h"
#include "rewrite.h"
#include "encoders.h"
#include "printf.h"
#include "flow.h"

#define NORMAL_CALL  0  // a normal procedure or function call
#define PROC_AS_FUNC 1  // treating a proc like a function with the out-arg trick

#define IS_NOT_COUNT 0  // analyzing the arguments of a normal function
#define IS_COUNT     1  // analyzing the arguments of the count function

#define IS_CASE 0  // analyzing the arguments of a case expression
#define IS_IIF  1  // analyzing the arguments of an iif expression

#define CQL_FROM_RECREATE "cql:from_recreate"
#define CQL_MODULE_WARN "cql:module_must_not_be_deleted_see_docs_for_CQL0392"

// These are the symbol tables with the ast dispatch when we get to an ast node
// we look it up here and call the appropriate function whose name matches the ast
// node type.

static symtab *non_sql_stmts;
static symtab *sql_stmts;

// Note: initialized statics are moot because in amalgam mode the code
// will not be reloaded... you have to re-initialize all statics in the cleanup function

// We have to do extra checks against tables that transitioned from the @recreate plan
// to the strongly managed plan
static list_item *all_prev_recreate_tables;

// When validating against the previous schema all newly @create columns must
// have a schema version >= the max in the previous schema.
static list_item *created_columns;
static int32_t max_previous_schema_version;

// Some facts to keep in mind when thinking about pending region validations:
// * when doing previous schhema validation the previous schema come after @previous_schema;
//   this means it comes after the main schema so even though it's "previous" it comes second in the file
//   this can be confusing.  Previous here always means the item in the previous schema which is the second item
//   of that name found.
// * when the item is encountered it's possible that it is in a region but that region has not
//   yet been used in a depoyable region. In order to be sure that deployable regions don't change
//   we have to wait until we've seen all the regions to decide which entites are in which deployable regions.
// * The canonical form of the previous schema has the region declarations first but we can't assume we're
//   getting these in the canonical order. So we have to enqueue.
// * The ast nodes here are durable as always as is the name which comes from the symbol table or the ast node
//   both of which are durable
// * The current region (current_region) won't be set when we come back to this later to validate so we
//   have to store that too.  Confusingly current_region is the current region we are processing and so
//   when this is enqueued it will be the current region of an item in the previous schema.  So it's the
//   current previous region...  We don't want to count on the semantic node of the previous schema item
//   because it might have errors for other reasons and they are lightly processed so we harvest everything
//   we need for later checking.
typedef struct deployable_validation {
  ast_node *prev;       // the node in the previous schema
  ast_node *cur;        // and in the "current" schema (which came before)
  CSTR prev_region;     // the logical region of the previous schema item
  CSTR cur_region;      // and the logical region of the current schema
  CSTR name;            // the name of the entity (whatever type it may be)
} deployable_validation;

// Define the signature of the callback registered in MISC_ATTRS_INT(...) to validate attributes
// on any statement.
typedef void (*sem_misc_attribute_callback)(
    CSTR misc_attr_prefix,
    CSTR misc_attr_name,
    ast_node *ast_misc_attr_values,
    ast_node *misc_attrs,
    ast_node *any_stmt);

static bytebuf *deployable_validations;

static bytebuf *unitary_locals;

// A list node holding the `sem_t *` for a nullability improvement of a global
// variable. These are used for un-setting all improvements of globals at every
// procedure call.
typedef struct global_notnull_improvement_item {
  sem_t *type;
  struct global_notnull_improvement_item *next;
} global_notnull_improvement_item;

// The analysis of loops like LOOP and WHILE is done in two passes. First, we
// analyze the loop to conservatively figure out every improvement that the loop
// could possibly unset. After that, then we reanalyze it with said improvements
// unset to ensure that everything is safe. See `sem_stmt_list_within_loop` for
// more information on why this is necessary.
typedef enum {
  LOOP_ANALYSIS_STATE_NONE,
  LOOP_ANALYSIS_STATE_ANALYZE,
  LOOP_ANALYSIS_STATE_REANALYZE
} loop_analysis_state;

// If a function has been registered via `FUNC_INIT`, its associated analysis
// function must conform to the type `sem_func`. When called, its argument list
// will have already been analyzed and verified to be free of errors.
typedef void sem_func(ast_node *ast, uint32_t arg_count);

// If a function has been registered via `SPECIAL_FUNC_INIT`, its associated
// analysis function must conform to the type `sem_special_func`. When called,
// it must do analysis of its own arguments as appropriate. It must also set
// `*is_aggregate` to true if it should be treated as an aggregate function by
// `sem_expr_call`; it will not be considered an aggregate function otherwise.
typedef void sem_special_func(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate);

// forward references for mutual recursion cases
static void sem_stmt_list(ast_node *ast);
static void sem_stmt_list_in_current_flow_context(ast_node *ast);
static void sem_stmt_list_within_loop(ast_node *stmt_list, ast_node *true_expr);
static void sem_select(ast_node *node);
static void sem_select_core_list(ast_node *ast);
static void sem_query_parts(ast_node *node);
static void sem_table_function(ast_node *node);
static void sem_as_alias(ast_node *node, CSTR *alias_target);
static void sem_fetch_stmt(ast_node *ast);
static void sem_fetch_values_stmt(ast_node *ast);
static void sem_call_stmt_opt_cursor(ast_node *ast, CSTR cursor_name);
static void sem_resolve_cursor_field(ast_node *expr, ast_node *cursor, CSTR field, sem_t **type_ptr);
static bool_t sem_validate_context(ast_node *ast, CSTR name, uint32_t valid_contexts);
static void sem_expr_select(ast_node *ast, CSTR cstr);
static void sem_with_select_stmt(ast_node *ast);
static void sem_upsert_stmt(ast_node *ast);
static void sem_with_upsert_stmt(ast_node *ast);
static void sem_with_select(ast_node *ast);
static void sem_explain(ast_node *ast);
static void sem_validate_args(ast_node *ast, ast_node *arg_list);
static void sem_validate_args_vs_formals(ast_node *ast, CSTR name, ast_node *arg_list, ast_node *params, bool_t proc_as_func);
static void sem_validate_old_object_or_marked_create(ast_node *root, ast_node *ast, CSTR err_msg, CSTR name);
static void sem_validate_marked_create_or_delete(ast_node *root, ast_node *ast, CSTR err_msg, CSTR name);
static bool_t sem_validate_compatible_table_cols_vals(ast_node *table_ast, ast_node *name_list, ast_node *insert_list);
static bool_t sem_validate_compatible_table_cols_select(ast_node *table_ast, ast_node *name_list, ast_node *select_stmt);
static bool_t sem_validate_compatible_cols_vals(ast_node *name_list, ast_node *values);
static void enqueue_pending_region_validation(ast_node *prev, ast_node *cur, CSTR name);
static void sem_validate_previous_deployable_region(ast_node *root, deployable_validation *v);
static void sem_opt_where(ast_node *ast);
static void sem_opt_orderby(ast_node *ast);
static void sem_opt_filter_clause(ast_node *ast);
static bool_t sem_validate_identical_text(ast_node *prev_def, ast_node *def, gen_func fn, gen_sql_callbacks *callbacks);
static bool_t sem_validate_identical_ddl(ast_node *cur, ast_node *prev);
static void sem_setup_region_filters(void);
static void sem_inside_create_proc_stmt(ast_node *ast);
static void sem_declare_cursor_for_expr(ast_node *ast);
static sem_join * new_sem_join(uint32_t count);
static void sem_validate_check_expr_for_table(ast_node *table, ast_node *expr, CSTR context);
static void sem_validate_index_expr_for_jptr(sem_join *jptr, ast_node *expr);
static void sem_numeric_expr(ast_node *expr, ast_node *context, CSTR subject, uint32_t expr_context);
static void sem_misc_attrs_basic(ast_node *ast);
static void sem_data_type_var(ast_node *ast);
static CSTR sem_combine_kinds_general(ast_node *ast, CSTR kleft, CSTR kright);
static CSTR sem_combine_kinds(ast_node *ast, CSTR current_kind);
static bool_t sem_select_stmt_is_mixed_results(ast_node *ast);
static bool_t sem_verify_legal_variable_name(ast_node *variable, CSTR name);
static void sem_verify_no_anon_columns(ast_node *ast);
static bool_t sem_verify_no_duplicate_names(ast_node *name_list);
static sem_t *find_mutable_type(CSTR name, CSTR scope);
static void sem_set_notnull_improved(CSTR name, CSTR scope);
static void sem_unset_notnull_improved(CSTR name, CSTR scope);
static void sem_unset_global_notnull_improvements();
static void sem_set_has_row_improved(CSTR cursor_name);
static void sem_unset_has_row_improved(CSTR cursor_name);
static void sem_set_improvements_for_true_condition(ast_node *ast);
static void sem_set_improvements_for_false_condition(ast_node *ast);
static bool_t variable_should_require_initialization(sem_t sem_type);
static void reset_enforcements(void);
static uint32_t sem_with_depth(void);
static ast_node *sem_find_table(CSTR name, ast_node *ast_error);
static void sem_shared_cte(ast_node *cte_body);
static void sem_declare_proc_stmt(ast_node *ast);
static bool sem_create_migration_proc_prototype(ast_node *origin, CSTR name);
static bool_t sem_has_extra_clauses(ast_node *select_from_etc, ast_node *select_orderby);
static void sem_non_blob_storage_table(ast_node *ast_error, ast_node *ast_table);
static ast_node *sem_synthesize_current_locals();

static void lazy_free_symtab(void *syms) {
  symtab_delete(syms);
}

static void add_pending_symtab_free(symtab *syms) {
  lazy_free *p = _new(lazy_free);
  p->context = syms;
  p->teardown = lazy_free_symtab;
  add_lazy_free(p);
}

struct enforcement_options {
  bool_t strict_fk_update;            // indicates there must be some "ON UPDATE" action in every FK
  bool_t strict_fk_delete;            // indicates there must be some "ON DELETE" action in every FK
  bool_t strict_join;                 // only ANSI style joins may be used, "from A,B" is rejected
  bool_t strict_upsert_stmt;          // no upsert statement may be used
  bool_t strict_window_func;          // no window functions may be used
  bool_t strict_without_rowid;        // no WITHOUT ROWID may be used.
  bool_t strict_transaction;          // no transactions may be started, commited, aborted etc.
  bool_t strict_if_nothing;           // (select ..) expressions must include the if nothing form
  bool_t strict_insert_select;        // insert with select may not include joins
  bool_t strict_table_function;       // table valued functions cannot be used on left/right joins (avoiding SQLite bug)
  bool_t strict_encode_context;       // encode context must be specified in @vault_sensitive
  bool_t strict_encode_context_type;  // the specified vault context column must be the specified data type
  bool_t strict_is_true;              // IS TRUE, IS FALSE, etc. may not be used because of downlevel issues
  bool_t strict_cast;                 // NO-OP casts result in errors
  bool_t strict_sign_function;        // the SQLite sign function may not be used (as it is absent in <3.35.0)
  bool_t strict_cursor_has_row;       // auto cursors require a has-row check before certain fields are accessed
};

static struct enforcement_options enforcement;

static sem_t encode_context_type;

typedef struct enforcement_stack_record {
  struct enforcement_stack_record *next;
  struct enforcement_options options;
} enforcement_stack_record;

static struct enforcement_stack_record *enforcement_stack;

typedef struct dummy_info {
  CSTR name;                    // the column name
  sem_t sem_type_col;           // its type
  ast_node *name_list_head;     // name list head and tail
  ast_node *name_list_tail;
  ast_node *insert_list_head;   // insert list head and tail
  ast_node *insert_list_tail;
  sem_join *jptr;               // the scope of the name
  bool_t use_null;              // use null for the dummy value rather than the seed
} dummy_info;
static void sem_synthesize_dummy_value(dummy_info *info);

// When processing version attributes there's a lot going on and loose arguments are insane.  So hence this struct.
typedef struct version_attrs_info {
  // inputs
  CSTR name;                          // the name of the thing we're studying (for errors)
  ast_node *target_ast;               // the thing whose version attributes are being studied
  ast_node *attrs_ast;                // the start of the version attributes on the thing

  // result
  int32_t create_version;             // the create version or -1
  ast_node *create_version_ast;       // the @create version ast
  CSTR create_proc;                   // the create migration proc if any
  int32_t delete_version;             // the delete version or -1
  ast_node *delete_version_ast;       // the @delete version ast
  CSTR delete_proc;                   // the delete migration proc if any
  sem_t flags;                        // SEM_FLAGS_DELETED or  0
  uint32_t create_code;               // @create annotation code (computed from ast type)
  uint32_t delete_code;               // @delete annotation code (computed from ast type)
  bool_t recreate;                    // true if table is on the @recreate plan
  ast_node *recreate_version_ast;     // the @recreate node
  CSTR recreate_group_name;           // the @recreate group name if there is one
  bool_t is_virtual_table;            // versioning rules for virtual tables apply
  bool_t is_temp;                     // versioning rules for temp objects apply
} version_attrs_info;

// extracts the useful information out of @create and @delete versions
static bool_t sem_validate_version_attrs(version_attrs_info *vers_info);

// validates previous and current attributes for valid progression
static bool_t sem_validate_attrs_prev_cur(version_attrs_info *prev, version_attrs_info *cur, ast_node *name_ast);

// ensures DDL inside of a proc has no attributes
static bool_t sem_validate_vers_ok_in_context(version_attrs_info *vers);

// records an annotation from the version info
static void sem_record_annotation_from_vers_info(version_attrs_info *vers_info);

// Validate whether or not an object is usable with a schema region. The object
// can only be a table, view, trigger or index.
static bool_t sem_validate_object_ast_in_current_region(
    CSTR name,
    ast_node *table_ast,
    ast_node *err_target,
    CSTR msg);

// The current join can have a parent if it is a nested select so we have to capture a stack
// of joins as the current joinscope.

typedef struct sem_joinscope {
  sem_join *jptr;
  struct sem_joinscope *parent;
} sem_joinscope;

// This defines the join scope that will be searched when resolving names
static sem_joinscope *current_joinscope;

// we're watching any symbols that come from this scope
static sem_join *monitor_jptr;

// if we see a symbol in the monitored scope, put it in this table.
static symtab *monitor_symtab;

// nested select level
static int32_t select_level;

// nested statement level
static int32_t sem_stmt_level;

// for making unique names of between temporaries
static int32_t between_count;

// If we are nested in a loop/while.
static int32_t loop_depth;

// If the current proc has used DML/DDL.
static bool_t has_dml;

// If the current proc is a shared fragment
static bool_t in_shared_fragment;

// If we are current processing the use of a shared fragment
static bool_t in_shared_fragment_call;

// If the current context is a trigger statement list
static bool_t in_trigger;

// If the current context is inside of a switch statement
static bool_t in_switch;

// If we are within a proc savepoint block, then true
static bool_t in_proc_savepoint;

// The schema version can be overridden to look at previous versions for upgrade scripts
// -1 indicates that the lastest schema should be used
static int32_t schema_upgrade_version;

// In a schema upgrade script we don't hide tables and we don't use create statements
// in procs as declarations.
static bool_t schema_upgrade_script;

// The current schema region if any, these do not nest
static CSTR current_region;

// These are all the names of all the antecedents of the current region (transitively)
static symtab *current_region_image;

// The current explain statement being process
static ast_node *current_explain_stmt;

// The current expression context (i.e. what part of the statement are we parsing).
static uint32_t current_expr_context;

// If we have started validating previous schema this will be true
static bool_t validating_previous_schema;

// The current annonation target in create proc statement
static CSTR annotation_target;

// @unsub and @resub must happen in version order to make sense
// here we track the most recent number; these must be non-decreasing
static int32_t last_sub_version;

// When we're doing previous schema validation we will march through the unsub/resub
// directives in order, this tells us the next one to consider.
static list_item *next_subscription;

// Once we've found an error during previous subscription validation, we don't report
// any more to avoid crazy spam.  This is the mercy flag.
static bool_t found_subscription_error;

// True if we are analyzing a call to `cql_inferred_notnull`. This can happen
// for three reasons:
//
// * We just did a rewrite that produced a `cql_inferred_notnull` call and now
//   we're computing its type.
// * We're analyzing an expression that was already analyzed (e.g., in a CTE).
// * We're analyzing the output of a previous CQL run within which calls to
//   `cql_inferrred_notnull` may occur.
//
// Regardless of the cause, if `is_analyzing_notnull_rewrite` is true, we do not
// want to rewrite again.
static bool_t is_analyzing_notnull_rewrite;

// Keeps track of all global variables that may currently be improved to be NOT
// NULL. We need this because we must un-improve all such variables after every
// procedure call (because we don't do interprocedural analysis and cannot know
// which globals may have been set to NULL).
static global_notnull_improvement_item *global_notnull_improvements;

// Keeps tracks of the current loop analysis state. If this is equal to
// `LOOP_ANALYSIS_STATE_ANALYZE`, we are analyzing with a non-final set of
// improvements. This is useful for two reasons:
//
// 1. Procedures that perform rewrites based on improvements (e.g.,
//    `sem_resolve_id_expr`) can use this to verify whether a rewrite is safe to
//    perform (`LOOP_ANALYSIS_STATE_NONE` or `LOOP_ANALYSIS_STATE_REANALYZE`) or
//    whether they should wait because they do not yet have definitive
//    information (`LOOP_ANALYSIS_STATE_ANALYZE`).
//
// 2. Analyses that would otherwise fail if called during reanalysis (e.g.,
//    `sem_verify_legal_variable_name`) can use this to check whether the
//    current state is `LOOP_ANALYSIS_STATE_REANALYZE` and adjust their
//    behaviors accordingly.
static loop_analysis_state current_loop_analysis_state = LOOP_ANALYSIS_STATE_NONE;

// True if the procedure currently being analyzed contains a TRY block that has
// been annotated with @attribute(cql:try_is_proc_body). Such an annotation
// implies that, conceptually, the main logic of the procedure exists entirely
// within the TRY block; any surrounding code typically exists only for the
// purpose of atypical error reporting or logging.
//
// See `sem_find_ast_misc_attr_trycatch_is_proc_body_callback` for context.
static bool_t current_proc_contains_try_is_proc_body;

// Push a context that stops us from searching further up.
#define PUSH_JOIN_BLOCK() \
  sem_joinscope blocker;\
  blocker.parent = current_joinscope; \
  blocker.jptr = NULL; \
  current_joinscope = &blocker;

// Push the current join onto the joinscope, this is for nested selects for instance.
#define PUSH_JOIN(name, x)  \
  sem_joinscope name;  \
  name.parent = current_joinscope; \
  name.jptr = x; \
  current_joinscope = &name;

#define POP_JOIN() \
  current_joinscope = current_joinscope->parent;

// Save the current expression context and create a new one, needed for instance if there is a nested select expression.
// The context must have EXACTLY one bit set (enforced by contract below)
#define PUSH_EXPR_CONTEXT(x) \
  uint32_t saved_expr_context = current_expr_context; \
  current_expr_context = (x); \
  Contract(0 == (current_expr_context & (current_expr_context - 1)))

#define POP_EXPR_CONTEXT() \
  current_expr_context = saved_expr_context

// We push a new monitor but only if there isn't already one present;
// the signal is that the symtab is not null if there is one present
// if there is one present, we just disable it by setting the jptr to null
// we only do the monitoring and therefore alias minification on the top
// level select statements;  internal names can be used all over for
// nested nested selects and correlated subqueries.  That's not where
// the space savings is anyway.
#define PUSH_MONITOR_SYMTAB() \
  symtab *monitor_symbtab_saved = monitor_symtab; \
  sem_join *monitor_jptr_saved = monitor_jptr; \
  if (select_level != 1) { \
    monitor_jptr = NULL; \
    used_symbols = NULL; \
    monitor_symtab = NULL; \
  } \
  else { \
    monitor_jptr = current_joinscope->jptr; \
    used_symbols = monitor_symtab = symtab_new(); \
    add_pending_symtab_free(used_symbols); \
  }

// put it all back unconditionally.
#define POP_MONITOR_SYMTAB() \
  monitor_jptr = monitor_jptr_saved; \
  monitor_symtab = monitor_symbtab_saved;

// These are the various symbol tables we need, they are stored super dumbly.
static symtab *interfaces;
static symtab *procs;
static symtab *unchecked_procs;
static symtab *proc_arg_info;
static symtab *triggers;
static symtab *upgrade_procs;
static symtab *ad_hoc_migrates;
static symtab *builtin_funcs;
static symtab *builtin_special_funcs;
static symtab *funcs;
static symtab *unchecked_funcs;
static symtab *exprs;
static symtab *tables;
static symtab *indices;
static symtab *globals;
static symtab *locals;
static symtab *enums;
static symtab *constant_groups;
static symtab *variable_groups;
static symtab *constants;
static symtab *current_variables;
static symtab *savepoints;
static symtab *table_items;  // assorted things that go into a table
static symtab *base_fragments;
static symtab *extension_fragments;
static symtab *assembly_fragments;
static symtab *extensions_by_basename;
static symtab *ref_sources_for_target_table;
static symtab *ref_targets_for_source_table;
static symtab *builtin_aggregated_funcs;
static symtab *arg_bundles;
static symtab *global_types;
static symtab *local_types;
static symtab *misc_attributes;

static ast_node *current_table_ast;
static CSTR current_table_name;

// during previous schema validations when we hit the previous section we have to
// save these, they the new schema for later comparison
static symtab *new_regions;
static symtab *new_enums;

// for dispatching expression types
typedef struct sem_expr_dispatch {
  void (*func)(ast_node *ast, CSTR str);
  CSTR str;
} sem_expr_dispatch;

// the current chain of common table expressions (for WITH clauses)
typedef struct cte_state {
  struct cte_state *prev;
  symtab *ctes;
} cte_state;

// top of the cte chain
static cte_state *cte_cur;

// If the current context is a upsert statement
static bool_t in_upsert;

// hold the table ast query in the current upsert statement.
static ast_node *current_upsert_table_ast;

// If we encounter an FK that refers to the table it is in then we have to defer processing
// of that FK until the table's columns and types are all known.  This just gives us an
// easy way to hold the data we need to validate until later.
typedef struct pending_table_validation {
  struct pending_table_validation *next;
  ast_node *ref_table_ast;
  ast_node *table_ast;
  ast_node *def;
  ast_node *fk;  // for fk attributes
  ast_node *check; // for check expressions
} pending_table_validation;

// The list of pending FK validations
static pending_table_validation *pending_table_validations_head;

static void sem_validate_fk_attr(pending_table_validation *pending);

// for verifying that a particular shared fragment call does not cause name conflicts inside the fragment
typedef struct binding_info {
  CSTR actual;
  CSTR formal;
  CSTR proc;
  CSTR proc_calling;
  charbuf *err;
} binding_info;

// If a foreign key in a table is self-referencing (i.e. T references T)
// then we have to defer the validation until we're done with the table and
// have compute all the types of all the columns.  So store the data so we can
// run it later
static void enqueue_pending_table_validation(pending_table_validation *pending) {
  pending_table_validation *v = _ast_pool_new(pending_table_validation);
  *v = *pending;
  v->next = pending_table_validations_head;
  pending_table_validations_head = v;
}

// Once we're done with the table, if any validations are pending we can dispatch them
// There are two types:  the attribute type e.g. "ref_id references T(id)" and
// the check expression type e.g. check(length(name) <32).  Both cases can be
// resolved after the table is fully processed because at that point we know all the
// column names in the table.  FK references seem like they could be resolved immediately
// until you consider that a table may FK to its own columns so we have to know
// all the names to be sure to give correct errors in that case, too.
static void run_pending_table_validations() {
  pending_table_validation *v = pending_table_validations_head;

  for (; v; v = v->next) {
    if (v->fk) {
      sem_validate_fk_attr(v);
      if (is_error(v->fk)) {
        goto error;
      }
    }
    else {
      Invariant(v->check);
      sem_validate_check_expr_for_table(v->table_ast, v->check, "CHECK");
      if (is_error(v->check)) {
        goto error;
      }
    }
  }

  pending_table_validations_head = NULL;  // the minipool will free these
  return;

error:
  record_error(v->table_ast);
  record_error(v->def);
  pending_table_validations_head = NULL;  // the minipool will free these
}

// This is validation for a check expression: deferred
// The tables columns are now known that there is a computed sptr for
// the tables type. We can put all it's columns into scope by creating
// a pending join expression the one struct in it. There is nothing
// else in scope here.  Note variables are allowed, but weird.
// You can in principle bind variables in place of constants in DDL
// but this is basically never done. Still, for symmettry we allow
// this (not new here but expressions are few in DDL)
//   * create a join context with a table that is impossible to name
//   * expressions like T.id are always invalid hence we use $$$ for the name
//     because that can match no syntactically correct "T".
//   * do smeantic analysis as usual on the expression, any numeric is
//     ok for a bool
static void sem_validate_check_expr_for_table(ast_node *table, ast_node *expr, CSTR context) {
  Contract(is_ast_create_table_stmt(table));
  Contract(expr);

  if (!is_error(table)) {
    sem_join *jptr;
    sem_struct *sptr = table->sem->sptr;

    jptr = new_sem_join(1);
    jptr->names[0] = "$$"; // there is no scope name for this make something that is an invalidate identifier
    jptr->tables[0] = sptr;

    // jptr is freed by the mini allocator later

    // save current symbol tables
    symtab *saved_locals = locals;
    symtab *saved_globals = globals;

    // hide variables for this expression
    locals = globals = NULL;

    PUSH_JOIN(expr_scope, jptr);
    sem_numeric_expr(expr, NULL, context, SEM_EXPR_CONTEXT_CONSTRAINT);
    POP_JOIN();

    locals = saved_locals;
    globals = saved_globals;

    // expr is already marked with an error by the above, no further record_error needed
  }
}

static void sem_validate_index_expr_for_jptr(sem_join *jptr, ast_node *expr) {
  Contract(jptr);
  Contract(expr);

  // save current symbol tables
  symtab *saved_locals = locals;
  symtab *saved_globals = globals;

  // hide variables for this expression
  locals = globals = NULL;

  PUSH_JOIN(expr_scope, jptr);
  sem_root_expr(expr, SEM_EXPR_CONTEXT_CONSTRAINT);
  POP_JOIN();

  locals = saved_locals;
  globals = saved_globals;

  // expr is already marked with an error by the above, no further record_error needed
}


// data needed for processing a column definition
typedef struct coldef_info {
  version_attrs_info *table_info;    // the various table version info items from the containing table
  sem_t col_sem_type;                // the semantic type of the created_columns
  CSTR col_name;                     // the column we are currently processing
  int32_t autoinc_columns;           // total number of autoinc columns in this table (at most 1)
  int32_t primary_keys;              // total number of primary key columns in this table (at most 1)
  int32_t previous_create_version;   // version number of the previous column (ordinal n-1)
  int32_t create_version;            // create version of this column or -1
  int32_t delete_version;            // delete version of this column or -1
  int32_t column_ordinal;            // column ordinal number (0 based, for this table)
  CSTR create_proc;                  // the name of the create migration proc if any
  CSTR delete_proc;                  // the name of the delete migration proc if any
  ast_node *default_value;           // the default value expression if there is one
} coldef_info;

// We collect these as we process the column definitions.
// tracking this information helps us to report on duplicates
// (like you can't say primary key 2 times)
// and otherwise get the results on a silver platter when processing is done.
// This also gives us access to the pending table info which can be used
// for validation and error messages.
static void init_coldef_info(coldef_info *info, version_attrs_info *table_info) {
  info->table_info = table_info;
  info->col_sem_type = SEM_TYPE_PENDING;
  info->col_name = NULL;
  info->autoinc_columns = 0;
  info->primary_keys = 0;
  info->create_version = -1;
  info->delete_version = -1;
  info->previous_create_version = -1;
  info->column_ordinal = -1;
  info->create_proc = NULL;
  info->delete_proc = NULL;
  info->default_value = NULL;
}

typedef struct name_check {
  // inputs
  ast_node *name_list;       // the name list
  sem_join *jptr;            // the scope in which to look for these names

  symtab *names;             // the names we found (no duplicates)
  ast_node *name_list_tail;  // the tail of the name list
  uint32_t count;            // the count of names
} name_check;

static bool_t sem_name_check(name_check *check);

// This is the setup for looking for a list of names in a particular join scope.  This is useful for
// making sure names are from (e.g.) the names in the select list, or the names of the
// columns of a table (one sptr in the jptr will do that job) or other such contexts.
static void init_name_check(name_check *check, ast_node *name_list, sem_join *jptr) {
  check->names = symtab_new();
  check->name_list_tail = NULL;
  check->count = 0;
  check->jptr = jptr;
  check->name_list = name_list;
}

// Releases the temp info.
static void destroy_name_check(name_check *check) {
  symtab_delete(check->names);
  check->name_list_tail = NULL;
  check->count = 0;
}

// create a durable copy of the text of a simple expression
static CSTR dup_expr_text_buffer(charbuf *tmp, ast_node *expr) {
  CSTR result = NULL;

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_echo; // we want all the text, unexpanded, so NOT for sqlite output (this is raw echo)
  gen_set_output_buffer(tmp);
  gen_with_callbacks(expr, gen_root_expr, &callbacks);
  result = Strdup(tmp->ptr);

  return result;
}

// create a durable copy of the text of a simple expression
CSTR dup_expr_text(ast_node *expr) {
  CHARBUF_OPEN(tmp);
  CSTR result = dup_expr_text_buffer(&tmp, expr);
  CHARBUF_CLOSE(tmp);
  return result;
}

// get the text for the expression, avoid the memory alloc for the easy case
static CSTR expr_as_text(ast_node *expr) {
  if (is_ast_str(expr)) {
   // easy case, super common, we have the name handy
   EXTRACT_STRING(name, expr);
   return name;
  }

  // it's an expression so we have to compute the text
  return dup_expr_text(expr);
}

// The name list item could be either indexed columns or a vanilla name list
// indexed columns have extra shape and might hold an expression. If an expression
// then we need to use the text of the expression as the value.
CSTR string_from_name_list_item(ast_node *node) {
  Contract(is_ast_indexed_columns(node) || is_ast_name_list(node));

  if (is_ast_indexed_columns(node)) {
    EXTRACT_NOTNULL(indexed_column, node->left);
    EXTRACT_ANY_NOTNULL(expr, indexed_column->left);

    return expr_as_text(expr);
  }

  EXTRACT_STRING(name, node->left);
  return name;
}

// Check if two name list nodes have the same members (in any order)
static bool_t is_name_list_equal(ast_node *name_list1, ast_node *name_list2) {
  symtab *cache = symtab_new();

  int32_t count1 = 0;

  for (ast_node *name_list = name_list1; name_list; name_list = name_list->right) {
    CSTR name = string_from_name_list_item(name_list);
    symtab_add(cache, name, NULL);
    count1++;
  }

  int32_t count2 = 0;

  for (ast_node *name_list = name_list2; name_list; name_list = name_list->right) {
    CSTR name = string_from_name_list_item(name_list);
    if (!symtab_find(cache, name)) {
      symtab_delete(cache);
      return false;
    }
    count2++;
  }

  symtab_delete(cache);
  return count1 == count2;
}

// Check if one of the indexed_columns is a subset of the other
// e.g: if indexed_columns2 is (a, b) then
//
// indexed_columns1 = (a, b, c) returns true
// indexed_columns1 = (b, a) returns true
// indexed_columns1 = (c, d, b, a) returns true
// indexed_columns1 = (a) returns true
//
// AND
//
// indexed_columns1 = (a, c) return false
// indexed_columns1 = (d) return false
// indexed_columns1 = (b, d) return false
static bool_t is_either_list_a_subset(ast_node *indexed_columns1, ast_node *indexed_columns2) {
  ast_node *a1 = indexed_columns1;
  ast_node *a2 = indexed_columns2;
  while (a1 && a2) {
    a1 = a1->right;
    a2 = a2->right;
  }

  // First make sure the small list is indexed_columns1 and bigger list is indexed_columns2
  // so we can just check if small list is in bigger list.
  if (!a2 && a1) {
    // exchange if is a2 is smaller
    ast_node *temp = indexed_columns1;
    indexed_columns1 = indexed_columns2;
    indexed_columns2 = temp;
  }

  bool_t included = true;
  symtab *cache = symtab_new();
  for (ast_node *names = indexed_columns2; names; names = names->right) {
    EXTRACT(indexed_column, names->left);
    EXTRACT_ANY_NOTNULL(expr, indexed_column->left);
    symtab_add(cache, expr_as_text(expr), NULL);
  }

  for (ast_node *names = indexed_columns1; names; names = names->right) {
    EXTRACT(indexed_column, names->left);
    EXTRACT_ANY_NOTNULL(expr, indexed_column->left);
    if (!symtab_find(cache, expr_as_text(expr))) {
      included = false;
      break;
    }
  }

  symtab_delete(cache);
  return included;
}

// Find the first unique key node in table_ast
static ast_node *find_first_unique_key(ast_node *table_ast) {
  Contract(is_ast_create_table_stmt(table_ast) &&
           is_ast_col_key_list(table_ast->right));
  ast_node *result = NULL;
  for (ast_node *col_key_list = table_ast->right; col_key_list; col_key_list = col_key_list->right) {
    ast_node *col_def = col_key_list->left;
    if (is_ast_unq_def(col_def)) {
      result = col_def;
      break;
    }
  }
  return result;
}

// Find the next unique key node to uk node in table_ast
static ast_node *find_next_unique_key(ast_node *unq_def) {
  Contract(is_ast_unq_def(unq_def));
  EXTRACT_NOTNULL(col_key_list, unq_def->parent);
  ast_node *result = NULL;
  while ((col_key_list = col_key_list->right)) {
    ast_node *col_def = col_key_list->left;
    if (is_ast_unq_def(col_def)) {
      result = col_def;
      break;
    }
  }
  return result;
}

// Check if a unique key ('uk') is valid. We only look at at all the unique keys
// preceding 'uk' because they have passed all validation already.
// e.g.
// create table simple_ak_table_4 (
//  a integer not null,
//  b text,
//  c real,
//  d long int
// );
//
// case 1:
// uk a,b  OK so far!
// uk a,b,c  INVALID:  (a,b) already unique
// uk b,a  INVALID: same as (a,b)
// uk c, d, b, a  INVALID: contains (a,b) which is already unique
// uk a  INVALID: because then (b,a) would be bogus because it contains (a)
//
// case 2:
// uk a, b  OK!
// uk a, c  OK! Because it has c, not found in (a, b)
// uk d  OK! Because d not in any of the above
// uk b, d  INVALID: because d already unique by itself above
// uk b, c  OK! because no one key already has (b, c) in it
//
// As you can see from the examples the general rule here is that if
// the columns of the new key are a superset of any previous key then
// it's kind of a goofy unique key because something smaller is already
// unique.  And likewise if this key is completely contained in any
// previous key then that previous key is goofy because this smaller
// key is already unique.
static bool_t is_unique_key_valid(ast_node *table_ast, ast_node *uk) {
  Contract(is_ast_create_table_stmt(table_ast) && is_ast_unq_def(uk));
  EXTRACT_NOTNULL(indexed_columns_conflict_clause, uk->right);
  EXTRACT_NAMED_NOTNULL(indexed_columns1, indexed_columns, indexed_columns_conflict_clause->left);
  for (ast_node *unq_def = find_first_unique_key(table_ast); unq_def; unq_def = find_next_unique_key(unq_def)) {
    if (uk == unq_def) {
      break;
    }

    EXTRACT_NAMED_NOTNULL(indexed_columns_conflict_clause2, indexed_columns_conflict_clause, unq_def->right);
    EXTRACT_NAMED_NOTNULL(indexed_columns2, indexed_columns, indexed_columns_conflict_clause2->left);
    if (is_either_list_a_subset(indexed_columns1, indexed_columns2)) {
      return false;
    }
  }
  return true;
}

// Make sure the given number is an integer, and is in range.
// We can only check the range if the integer evalautes to a constant which is
// common enough that we try to do this here.  If it's a not something we recognize
// is a constant then all bets are off.
static bool_t is_num_int_in_range(ast_node *ast, int64_t lower, int64_t upper) {
  // any non-integer type is out;  can't be "real" or "null" in particular which are
  // usually considered numeric compat but not here.  This is an index.
  if (!is_integer(ast->sem->sem_type)) {
    return false;
  }

  eval_node result = EVAL_NIL;
  eval(ast, &result);

  if (result.sem_type == SEM_TYPE_ERROR) {
    // not a constant, can't verify it now, this will have to be a run time error
    return true;
  }

  // put the result in the int64 fields
  eval_cast_to(&result, SEM_TYPE_LONG_INTEGER);

  return result.int64_value >= lower && result.int64_value <= upper;
}

// Wrappers for the func table.
static bool_t add_func(ast_node *ast, CSTR name) {
  return symtab_add(funcs, name, ast);
}

ast_node *find_func(CSTR name) {
  symtab_entry *entry = symtab_find(funcs, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static bool_t add_unchecked_func(ast_node *ast, CSTR name) {
  return symtab_add(unchecked_funcs, name, ast);
}

cql_noexport ast_node *find_unchecked_func(CSTR name) {
  symtab_entry *entry = symtab_find(unchecked_funcs, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

ast_node *find_recreate_migrator(CSTR name) {
  symtab_entry *entry = symtab_find(ad_hoc_recreate_actions, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Wrappers for the index list (note we only use these for validation).
static void add_index(ast_node *ast, CSTR name) {
  symtab_add(indices, name, ast);
}

static ast_node *find_index(CSTR name) {
  symtab_entry *entry = symtab_find(indices, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// returns the node only if it exists and is not restricted by the schema region.
static ast_node *find_usable_index(CSTR name, ast_node *err_target, CSTR msg) {
  ast_node *index_ast = find_index(name);
  if (!index_ast) {
    report_error(err_target, msg, name);
    return NULL;
  }

  if (!sem_validate_object_ast_in_current_region(name, index_ast, err_target, msg)) {
    return NULL;
  }

  return index_ast;
}

// Standard helpers for checking for a semantic error in the AST.
cql_noexport bool_t is_sem_error(sem_node *sem) {
  return !sem || core_type_of(sem->sem_type) == SEM_TYPE_ERROR;
}

cql_noexport bool_t is_error(ast_node *ast) {
  return !ast || is_sem_error(ast->sem);
}

// Note: this returns the flag not a bool.
static sem_t not_nullable_flag(sem_t sem_type) {
  return sem_type & SEM_TYPE_NOTNULL;
}

cql_noexport bool_t is_not_nullable(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_NOTNULL);
}

static bool_t has_default(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_HAS_DEFAULT);
}

static bool_t has_autoincrement(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_AUTOINCREMENT);
}

// Note this returns the flag not a bool.
static sem_t both_notnull_flag(sem_t sem_type_1, sem_t sem_type_2) {
  return sem_type_1 & sem_type_2 & SEM_TYPE_NOTNULL;
}

// Note this returns the flag not a bool.
cql_noexport sem_t sensitive_flag(sem_t sem_type) {
  return sem_type & SEM_TYPE_SENSITIVE;
}

// If any of the fields of the struct type are SENSITIVE then the result is SENSITIVE
// We need this to handle EXISTS(select * from ...)
static sem_t any_sensitive(sem_struct *sptr) {
  sem_t sem_sensitive = 0;
  for (int32_t i = 0; sem_sensitive == 0 && i < sptr->count; i++) {
    sem_sensitive |= sensitive_flag(sptr->semtypes[i]);
  }
  return sem_sensitive;
}

// The normal combination for semantic flags, just the flags:
// * if either is nullable the result is nullable
// * if either is sensitive the result is sensitive
// nullable is weird because the flag is "NOTNULL" so everything is inverted
static sem_t combine_flags(sem_t sem_type_1, sem_t sem_type_2) {
  return both_notnull_flag(sem_type_1, sem_type_2) |
         sensitive_flag(sem_type_1) |
         sensitive_flag(sem_type_2);
}

// This is needed as often as the other case.
cql_noexport bool_t is_nullable(sem_t sem_type) {
  return !(sem_type & SEM_TYPE_NOTNULL);
}

static bool_t is_unique_key(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_UK);
}

cql_noexport bool_t is_virtual_ast(ast_node *ast) {
  return ast->sem && (ast->sem->sem_type & SEM_TYPE_VIRTUAL);
}

cql_noexport bool_t is_primary_key(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_PK);
}

cql_noexport bool_t is_foreign_key(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_FK);
}

// Returns true if exactly one flag bit is set, else false.
cql_noexport bool_t is_single_flag(sem_t sem_type) {
  return sem_type & SEM_TYPE_FLAGS && !(sem_type & (sem_type - 1));
}

// Strips out all the flag bits and gives you the base/core type.
cql_noexport sem_t core_type_of(sem_t sem_type) {
  return sem_type & SEM_TYPE_CORE;
}

// Several helpers for identifying various node types.
cql_noexport bool_t is_bool(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_BOOL;
}

cql_noexport bool_t is_real(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_REAL;
}

cql_noexport bool_t is_variable(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_VARIABLE);
}

cql_noexport bool_t is_in_parameter(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_IN_PARAMETER);
}

cql_noexport bool_t is_out_parameter(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_OUT_PARAMETER);
}

cql_noexport bool_t is_cursor_formal(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_CURSOR_FORMAL;
}

cql_noexport bool_t was_set_variable(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_WAS_SET);
}

/*
cql_noexport bool_t is_backing(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_BACKING);
}
*/

cql_noexport bool_t is_backed(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_BACKED);
}

cql_noexport bool_t is_inout_parameter(sem_t sem_type) {
  return is_in_parameter(sem_type) && is_out_parameter(sem_type);
}

cql_noexport bool_t has_out_stmt_result(ast_node *ast) {
  sem_t sem_type = ast->sem->sem_type;
  return !!(sem_type & SEM_TYPE_USES_OUT);
}

cql_noexport bool_t has_out_union_stmt_result(ast_node *ast) {
  sem_t sem_type = ast->sem->sem_type;
  return !!(sem_type & SEM_TYPE_USES_OUT_UNION);
}

cql_noexport bool_t has_out_union_call(ast_node *ast) {
  sem_t sem_type = ast->sem->sem_type;
  return !!(sem_type & SEM_TYPE_CALLS_OUT_UNION);
}

// The proc has a normal result set if it has a struct type and it isn't using either out or out union
cql_noexport bool_t has_result_set(ast_node *ast) {
  sem_t sem_type = ast->sem->sem_type;
  sem_t any_out = sem_type & (SEM_TYPE_USES_OUT | SEM_TYPE_USES_OUT_UNION); // non-zero if either
  return !any_out && is_struct(ast->sem->sem_type);
}

cql_noexport bool_t is_create_func(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_CREATE_FUNC);
}

cql_noexport bool_t is_deleted(ast_node *ast) {
  Contract(ast->sem);
  sem_node *sem = ast->sem;

  // if unsubscribed it's logically deleted
  if (sem->unsub_version > sem->resub_version) {
    // note only tables ever set this so we just don't go here at all for columns
    return true;
  }

  sem_t sem_type = sem->sem_type;
  return !!(sem_type & SEM_TYPE_DELETED);
}

static bool_t is_validated(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_VALIDATED);
}

cql_noexport bool_t is_dml_proc(sem_t sem_type) {
  return !!(sem_type & SEM_TYPE_DML_PROC);
}

cql_noexport bool_t is_text(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_TEXT;
}

cql_noexport bool_t is_integer(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_INTEGER || core_type_of(sem_type) == SEM_TYPE_LONG_INTEGER;
}

cql_noexport bool_t is_object(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_OBJECT;
}

cql_noexport bool_t is_blob(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_BLOB;
}

cql_noexport bool_t is_ref_type(sem_t sem_type) {
  return is_text(sem_type) || is_object(sem_type) || is_blob(sem_type);
}

cql_noexport bool_t is_null_type(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_NULL;
}

cql_noexport bool_t is_string_compat(sem_t sem_type) {
  return is_text(sem_type) || is_null_type(sem_type);
}

cql_noexport bool_t is_object_compat(sem_t sem_type) {
  return is_object(sem_type) || is_null_type(sem_type);
}

cql_noexport bool_t is_blob_compat(sem_t sem_type) {
  return is_blob(sem_type) || is_null_type(sem_type);
}

cql_noexport bool_t is_numeric(sem_t sem_type) {
  sem_type = core_type_of(sem_type);
  return sem_type >= SEM_TYPE_BOOL && sem_type <= SEM_TYPE_REAL;
}

cql_noexport bool_t is_numeric_compat(sem_t sem_type) {
  sem_type = core_type_of(sem_type);
  return sem_type >= SEM_TYPE_NULL && sem_type <= SEM_TYPE_REAL;
}

// The non-compound types (i.e. not struct or join)
cql_noexport bool_t is_unitary(sem_t sem_type) {
  return core_type_of(sem_type) < SEM_TYPE_MAX_UNITARY;
}

cql_noexport bool_t is_cursor(sem_t sem_type) {
  return is_struct(sem_type) && is_variable(sem_type);
}

cql_noexport bool_t is_auto_cursor(sem_t sem_type) {
  return is_cursor(sem_type) && (sem_type & SEM_TYPE_HAS_SHAPE_STORAGE);
}

cql_noexport bool_t is_struct(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_STRUCT;
}

static bool_t is_join(sem_t sem_type) {
  return core_type_of(sem_type) == SEM_TYPE_JOIN;
}

cql_noexport bool_t is_numeric_expr(ast_node *expr) {
  return is_numeric_compat(expr->sem->sem_type);
}

cql_noexport bool_t is_autotest_dummy_table(CSTR name) {
  return !Strcasecmp(name, "dummy_table");
}

cql_noexport bool_t is_autotest_dummy_insert(CSTR name) {
  return !Strcasecmp(name, "dummy_insert");
}

cql_noexport bool_t is_autotest_dummy_select(CSTR name) {
  return !Strcasecmp(name, "dummy_select");
}

cql_noexport bool_t is_autotest_dummy_result_set(CSTR name) {
  return !Strcasecmp(name, "dummy_result_set");
}

cql_noexport bool_t is_autotest_dummy_test(CSTR name) {
  return !Strcasecmp(name, "dummy_test");
}

cql_noexport bool_t sem_is_str_name(ast_node *ast) {
  if (is_ast_str(ast)) {
    EXTRACT_STRING(name, ast);
    return name[0] != '\'' && name[0] != '\"';
  }
  return false;
}

// Detect if column name is sensitive in result set
bool_t is_sensitive_column_in_result_set(CSTR name) {
  sem_struct *sptr = current_proc->sem->sptr;
  uint32_t count = sptr->count;
  for (int32_t i = 0; i < count; i++) {
    CSTR col = sptr->names[i];
    if (!strcmp(name, col)) {
      if (sptr->semtypes[i] & SEM_TYPE_SENSITIVE) {
        return true;
      }
    }
  }
  return false;
}

// This is used to ensure column name (param 'name') is part of the proc return struct
static void sem_column_name_exist_in_result_set(CSTR name, ast_node *misc_attr_value, void *context) {
  EXTRACT_NOTNULL(misc_attrs, (ast_node *)context);
  Contract(current_proc);
  sem_struct *sptr = current_proc->sem->sptr;

  // if there is no return struct that's a different error
  if (sptr) {
    int32_t icol = sem_column_index(sptr, name);
    if (icol < 0) {
      CHARBUF_OPEN(msg);
      bprintf(&msg, "CQL0239: %s column does not exist in result set", annotation_target);
      report_error(misc_attrs, msg.ptr, name);
      record_error(misc_attrs);
      CHARBUF_CLOSE(msg);
      return;
    }
  }

  record_ok(misc_attr_value);
}

// This is used to ensure column type matches the specified encode context column type
static void sem_column_name_match_encode_context_column_type(CSTR name, ast_node *misc_attrs)
{
  if (!enforcement.strict_encode_context_type) {
    return;
  }
  Contract(misc_attrs);
  Contract(current_proc);
  sem_struct *sptr = current_proc->sem->sptr;
  uint32_t count = sptr->count;
  for (int32_t i = 0; i < count; i++) {
    CSTR col = sptr->names[i];
    if (!strcmp(name, col)) {
      if (core_type_of(sptr->semtypes[i]) != encode_context_type) {
        report_error(misc_attrs, "CQL0402: vault context column in vault_senstive attribute must match the specified type in strict mode", "vault_sensitive");
        record_error(misc_attrs);
      }
    }
  }
}

// The helper checks whether or not a column should be encoded. A column
// is encoded if and only if it's marked and is also sensitive.
// The eligibility infos are extract from vault_sensitive attribution
// on create_proc_stmt node and stored in encode_columns symtab.
// Eligible columns are encoded if they are sensitive as soon as they're
// fetched from db (see cql_multifetch(...)).
bool_t should_encode_col(CSTR col, sem_t sem_type, bool_t use_encode_arg, symtab *encode_columns_arg) {
  // objects can't be encoded
  if (core_type_of(sem_type) == SEM_TYPE_OBJECT) {
    return false;
  }

  bool_t is_col_eligible = false;
  if (encode_columns_arg && encode_columns_arg->count > 0 ) {
    is_col_eligible = symtab_find(encode_columns_arg, col) != NULL;
  } else {
    // otherwise all column are always eligible if use_encode is TRUE.
    is_col_eligible = use_encode_arg;
  }
  return is_col_eligible && sensitive_flag(sem_type);
}

// encode context column in vault_sensitive attribute must not be sensitive
static void enforce_non_sensitive_context_column(CSTR name, ast_node *misc_attrs)
{
  if (is_sensitive_column_in_result_set(name)) {
    report_error(misc_attrs, "CQL0400: encode context column can't be sensitive", name);
    record_error(misc_attrs);
  } else {
    record_ok(misc_attrs);
  }
}

// report error if encode context column mode is on and no encode context column is provided
static void enforce_encode_context_column_with_strict_mode(ast_node *misc_attrs)
{
  if (enforcement.strict_encode_context) {
    report_error(misc_attrs, "CQL0401: context column must be specified if strict encode context column mode is enabled", "vault_sensitive");
    record_error(misc_attrs);
  }
}

// enforce the specified column name must be valid string
// return false if column is not valid string
static bool_t vault_sensitive_encode_column_valid_string(ast_node *misc_attr_value, ast_node *misc_attrs)
{
  if (!sem_is_str_name(misc_attr_value)) {
    report_error(misc_attr_value, "CQL0363: all arguments must be names", "vault_sensitive");
    record_error(misc_attr_value);
    if (misc_attrs) {
      record_error(misc_attrs);
    }
    return false;
  }
  return true;
}

// This is the place we add all the to-be-encoded columns to a list.
// The passed in ast_misc_attrs may contain non string values (like list),
// which are considered to be error cases and skipped here.
static void vault_sensitive_encode_columns(ast_node *ast_misc_attrs, symtab *vault_column_list, ast_node *misc_attrs) {
  for (ast_node *list = ast_misc_attrs; list; list = list->right) {
    // any non-string values are error cases
    if (is_ast_str(list->left)) {
      EXTRACT_STRING(name, list->left);
      if (misc_attrs && current_proc) {
        sem_column_name_exist_in_result_set(name, list->left, misc_attrs);
      }
      if (vault_column_list) {
        symtab_add(vault_column_list, name, NULL);
      }
    } else {
      report_error(list->left, "CQL0363: all arguments must be names", "vault_sensitive");
      record_error(list->left);
      if (misc_attrs) {
        record_error(misc_attrs);
      }
    }
  }
}

// Information about the vault_sensitive parsing result
// encode_context_column is the column used as encode context
// encode_columns is a list of columns to be encoded.
// misc_attrs is used to report parsing errors.
// This is used in the find_misc_attrs callback to save parsing result.
typedef struct encode_info {
  CSTR *encode_context_column;
  symtab *encode_columns;
  ast_node *misc_attrs;
} encode_info;

// This is the core part of the parsing logic for vault_sensitive
// It does two things:
// (1) extract encode_info struct from the net info.
// (2) report error case if invalid format is detected.
// There are two types of formats for vault_sensitive
// (a) @attribute(cql:vault_sensitive=(<col1>, <col2>, ...))
// (b) @attribute(cql:vault_sensitive=(<context_col>, (<col1>, <col2>, ...)))
// format (b) has extra encode context column specified.
// both encode context column and encode columns need to be present in resultSet
static void sem_find_ast_misc_attr_vault_sensitive_callback(
  CSTR misc_attr_prefix,
  CSTR misc_attr_name,
  ast_node *ast_misc_attr_value_list,
  void *context)
{
  Contract(context);

  // If missing any part of the attribute or the attribute isn't what is
  // expected then it's not our attribute.
  if (!misc_attr_prefix ||
      !misc_attr_name ||
      Strcasecmp(misc_attr_prefix, "cql") ||
      Strcasecmp(misc_attr_name, "vault_sensitive")) {
    return;
  }

  encode_info *info = (encode_info *)context;
  ast_node *misc_attrs = info->misc_attrs;

  // case @attribute(cql:vault_sensitive)
  // all sensitive columns in the result set will be encoded without context column.
  if (ast_misc_attr_value_list == NULL) {
    // report error if strict mode is on for encode context column
    enforce_encode_context_column_with_strict_mode(misc_attrs);
    return;
  }

  // case @attribute(cql:vault_sensitive=col)
  // only col is encoded without context column
  if (!is_ast_misc_attr_value_list(ast_misc_attr_value_list)) {
    // detect invalid string column
    if (!vault_sensitive_encode_column_valid_string(ast_misc_attr_value_list, misc_attrs)) {
      return;
    }
    // ensure column exists in result set
    EXTRACT_STRING(name, ast_misc_attr_value_list);
    if (misc_attrs && current_proc) {
      sem_column_name_exist_in_result_set(name, ast_misc_attr_value_list, misc_attrs);
    }
    // extract the single to-be-encoded column
    if (info->encode_columns) {
      symtab_add(info->encode_columns, name, NULL);
    }
    // report error if strict mode is on for encode context column
    enforce_encode_context_column_with_strict_mode(misc_attrs);
    return;
  }

  // find out if we are dealing with format (a) or format (b)
  // (a) @attribute(cql:vault_sensitive=(<col1>, <col2>, ...))
  // (b) @attribute(cql:vault_sensitive=(<context_col>, (<col1>, <col2>, ...)))
  // if we see nested column list, it's format (b) with context column, otherwise format (a).
  bool_t has_nested_columns = false;
  for (ast_node *list = ast_misc_attr_value_list; list; list = list->right) {
    ast_node *misc_attr_value = list->left;
    if (is_ast_misc_attr_value_list(misc_attr_value)) {
      has_nested_columns = true;
    }
  }

  // format (a) without context column
  // case @attribute(cql:vault_sensitive=(col1, col2, ...))
  if (!has_nested_columns) {
    vault_sensitive_encode_columns(ast_misc_attr_value_list, info->encode_columns, misc_attrs);
    // report error if strict mode is on for encode context column
enforce_encode_context_column_with_strict_mode(misc_attrs);
    return;
  }

  // format (b) with context column
  // case @attribute(cql:vault_sensitive=(context_col, (col1, col2, ...)))
  uint32_t context_col_count = 0;
  for (ast_node *list = ast_misc_attr_value_list; list; list = list->right) {
    ast_node *misc_attr_value = list->left;
    // we found the column list to be encoded
    if (is_ast_misc_attr_value_list(misc_attr_value)) {
      vault_sensitive_encode_columns(misc_attr_value, info->encode_columns, misc_attrs);
    }
    else {
      // we found encode context column and ensure it is valid string
      if (!vault_sensitive_encode_column_valid_string(misc_attr_value, misc_attrs)) {
        return;
      }
      EXTRACT_STRING(context_col, misc_attr_value);
      if (misc_attrs && current_proc) {
        enforce_non_sensitive_context_column(context_col, misc_attrs);
        sem_column_name_exist_in_result_set(context_col, misc_attr_value, misc_attrs);
        sem_column_name_match_encode_context_column_type(context_col, misc_attrs);
      }
      // extrac context column
      if (info->encode_context_column) {
        *(info->encode_context_column) = context_col;
      }
      // enforce that we only specify context column once
      context_col_count ++;
      if (context_col_count > 1 && misc_attrs) {
        report_error(misc_attrs, "CQL0408: encode context column can be only specified once", context_col);
        record_error(misc_attrs);
      }
    }
  }
}

cql_noexport void init_encode_info(ast_node *misc_attrs, bool_t *use_encode_arg, CSTR *encode_context_column_arg, symtab *encode_columns_arg) {
  *use_encode_arg = misc_attrs && exists_attribute_str(misc_attrs, "vault_sensitive");
  if (*use_encode_arg) {
    encode_info info;
    info.encode_context_column = encode_context_column_arg;
    info.encode_columns = encode_columns_arg;
    info.misc_attrs = NULL;
    find_misc_attrs(misc_attrs, sem_find_ast_misc_attr_vault_sensitive_callback, &info);
    record_ok(misc_attrs);
  }
}

// subscription directives have the same kind of payload and need the same basic info
typedef struct subs_info {
  ast_node *target_ast;
  CSTR name;
  int32_t vers;
} subs_info;

// This extracts the basic info from a sub/unsub directive and does minimal sanity check.
static void sem_subs_extract(ast_node *ast, subs_info *info);

// This tells us if we're actually going to try to add the entity we are working on
// to our tables and so forth.  The idea here is that a view/table/whatever might
// not "count" in terms of checking for uniqueness and adding to the all_whatevers
// tables if we are processing in certain contexts.  Specifically if we are
// validating previous schema then the previous version "doesn't count" and
// if we are doing a schema upgrade then any DDL we find also doesn't count because
// those versions might be different than the "final" version after all the upgrades
// are applied.  So, for instance, checking that they are the same as the declared
// version would be counter-productive.
static bool_t will_add_current_entity() {
  return !(validating_previous_schema) && !(schema_upgrade_script && current_proc);
}

CSTR coretype_string(sem_t sem_type) {
  CSTR result = NULL;
  switch (core_type_of(sem_type)) {
    case SEM_TYPE_INTEGER: result = "INTEGER"; break;
    case SEM_TYPE_TEXT: result = "TEXT"; break;
    case SEM_TYPE_LONG_INTEGER: result = "LONG_INT"; break;
    case SEM_TYPE_REAL: result = "REAL"; break;
    case SEM_TYPE_BOOL: result = "BOOL"; break;
    case SEM_TYPE_BLOB: result = "BLOB"; break;
    case SEM_TYPE_OBJECT: result = "OBJECT"; break;
  }
  Invariant(result);
  return result;
}

// Construct the version_attrs_info struct.
static void init_version_attrs_info(version_attrs_info *vers_info, CSTR name, ast_node *ast, ast_node *attrs) {
  vers_info->name = name;
  vers_info->target_ast = ast;
  vers_info->attrs_ast = attrs;
  vers_info->create_version_ast = NULL;
  vers_info->create_proc = NULL;
  vers_info->create_version = -1;
  vers_info->delete_version = -1;
  vers_info->delete_version_ast = NULL;
  vers_info->delete_proc = NULL;
  vers_info->flags = 0;
  vers_info->recreate = false;
  vers_info->recreate_version_ast = NULL;
  vers_info->recreate_group_name = NULL;
  vers_info->is_virtual_table = false;
  vers_info->is_temp = false;

  if (is_ast_create_table_stmt(ast)) {
    vers_info->create_code = SCHEMA_ANNOTATION_CREATE_TABLE;
    vers_info->delete_code = SCHEMA_ANNOTATION_DELETE_TABLE;
    vers_info->is_virtual_table =  ast->parent && is_ast_create_virtual_table_stmt(ast->parent);
    EXTRACT_NOTNULL(create_table_name_flags, ast->left);
    EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
    EXTRACT_OPTION(flags, table_flags_attrs->left);
    vers_info->is_temp = !!(flags & TABLE_IS_TEMP);
  }
  else if (is_ast_create_index_stmt(ast)) {
    vers_info->create_code = SCHEMA_ANNOTATION_INVALID;
    vers_info->delete_code = SCHEMA_ANNOTATION_DELETE_INDEX;
  }
  else if (is_ast_create_trigger_stmt(ast)) {
    vers_info->create_code = SCHEMA_ANNOTATION_INVALID;
    vers_info->delete_code = SCHEMA_ANNOTATION_DELETE_TRIGGER;
    EXTRACT_OPTION(flags, ast->left);
    vers_info->is_temp = !! (flags & TRIGGER_IS_TEMP);
  }
  else {
    // this is all that's left
    Contract(is_ast_create_view_stmt(ast));
    vers_info->create_code = SCHEMA_ANNOTATION_INVALID;
    vers_info->delete_code = SCHEMA_ANNOTATION_DELETE_VIEW;

    EXTRACT_OPTION(flags, ast->left);
    vers_info->is_temp = !! (flags & VIEW_IS_TEMP);
  }
}

// Simple wrappers for the tables list.
static void add_table_or_view(ast_node *ast) {
  symtab_add(tables, ast->sem->sptr->struct_name, ast);
}

// Validates whether or not an object is usable within the current schema
// region. The object can only be a table, view, trigger or index. If not valid,
// reports an error using `err_target` and `msg`, if present.
static bool_t sem_validate_object_ast_in_current_region(CSTR name,
                                             ast_node *table_ast,
                                             ast_node *err_target,
                                             CSTR msg) {
  Contract(name);
  Contract(table_ast);
  Contract(err_target && msg || !err_target && !msg);

  // We're in a non-region therefore no validation needed because non-region stmt
  // can reference schema in any region.
  if (!current_region) {
    return true;
  }

  if (table_ast->sem && table_ast->sem->region) {
    // if we have a current region then the image is always computed!
    Invariant(current_region_image);
    if (!symtab_find(current_region_image, table_ast->sem->region)) {
      // The target region is not accessible from this region
      if (err_target) {
        CHARBUF_OPEN(err_msg);
        bprintf(&err_msg, "%s (object is in schema region '%s' not accessible from region '%s')",
          msg,
          table_ast->sem->region,
          current_region);
        report_error(err_target, err_msg.ptr, name);
        CHARBUF_CLOSE(err_msg);
      }
      return false;
    }
  }
  else if (err_target) {
    CHARBUF_OPEN(err_msg);
    bprintf(&err_msg, "%s (while in schema region '%s', accessing an object that isn't in a region is invalid)",
      msg,
      current_region);
    report_error(err_target, err_msg.ptr, name);
    CHARBUF_CLOSE(err_msg);
    return false;
  }

  return true;
}

// Only clients that want to ensure no conflict of names (whether deleted or
// not) use this version.
ast_node *find_table_or_view_even_deleted(CSTR name) {
  Contract(name);

  symtab_entry *entry = symtab_find(tables, name);

  return entry ? (ast_node*)(entry->val) : NULL;
}

// Returns the node only if it exists and is not restricted by the schema
// region. If not found, reports an error using `err_target` and `msg`, if
// present.
static ast_node *find_usable_table_or_view_even_deleted(CSTR name, ast_node *err_target, CSTR msg) {
  Contract(name);
  Contract(err_target && msg || !err_target && !msg);

  ast_node *table_ast = find_table_or_view_even_deleted(name);
  if (!table_ast) {
    if (err_target) {
      report_error(err_target, msg, name);
    }
    return NULL;
  }

  if (!sem_validate_object_ast_in_current_region(name, table_ast, err_target, msg)) {
    return NULL;
  }

  return table_ast;
}

// Returns the node only if the table is not deleted; most clients use this. If
// not found, reports an error using `err_target` and `msg`, if present.
cql_noexport ast_node *find_usable_and_not_deleted_table_or_view(CSTR name, ast_node *err_target, CSTR msg) {
  Contract(name);
  Contract(err_target && msg || !err_target && !msg);

  ast_node *table_ast = find_usable_table_or_view_even_deleted(name, err_target, msg);
  if (!table_ast) {
    return NULL;
  }

  // Check for views first. If this is a migration script, the view will be a
  // stub so we don't want to look at it it too deeply: It's just there so we
  // can produce this error.
  if (schema_upgrade_version > 0 && !is_ast_create_table_stmt(table_ast)) {
    // views may not be accessed in a migration script
    Invariant(is_ast_create_view_stmt(table_ast));
    if (err_target) {
      CHARBUF_OPEN(err_msg);
      bprintf(&err_msg, "%s (view hidden in migration script)", msg);
      report_error(err_target, err_msg.ptr, name);
      CHARBUF_CLOSE(err_msg);
    }
    return NULL;
  }

  if (is_deleted(table_ast)) {
    if (err_target) {
      CHARBUF_OPEN(err_msg);
      if (schema_upgrade_version > 0) {
        bprintf(&err_msg, "%s (not visible in schema version %d)", msg, schema_upgrade_version);
      }
      else {
        if (table_ast->sem->delete_version > table_ast->sem->unsub_version)  {
          bprintf(&err_msg, "%s (hidden by @delete)", msg);
        }
        else {
          bprintf(&err_msg, "%s (hidden by @unsub)", msg);
        }
      }
      report_error(err_target, err_msg.ptr, name);
      CHARBUF_CLOSE(err_msg);
    }
    return NULL;
  }

  return table_ast;
}

// Like `find_usable_and_not_deleted_table_or_view`, but merely returns true if
// there is such a table, else false: No errors are reported. This can be used
// to check whether or not a to-be-introduced name will shadow something that is
// already in scope and usable.
static bool_t is_usable_and_not_deleted_table_or_view(CSTR name) {
  Contract(name);

  return !!find_usable_and_not_deleted_table_or_view(name, NULL, NULL);
}

static void add_cte(ast_node *ast) {
  Contract(cte_cur);
  Contract(cte_cur->ctes);
  symtab_add(cte_cur->ctes, ast->sem->sptr->struct_name, ast);
}

static ast_node *find_cte(CSTR name) {
  cte_state *state = cte_cur;
  while (state) {
    symtab_entry *entry = symtab_find(state->ctes, name);
    if (entry) {
        return (ast_node*)entry->val;
    }
    state = state->prev;
  }

  return NULL;
}

// Wrappers for the trigger table.
static bool_t add_trigger(ast_node *ast, CSTR name) {
  return symtab_add(triggers, name, ast);
}

static ast_node *find_trigger(CSTR name) {
  symtab_entry *entry = symtab_find(triggers, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// wrapper for variable groups
ast_node *find_variable_group(CSTR name) {
  symtab_entry *entry = symtab_find(variable_groups, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// wrapper for constant groups
ast_node *find_constant_group(CSTR name) {
  symtab_entry *entry = symtab_find(constant_groups, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// wrapper for constants
ast_node *find_constant(CSTR name) {
  symtab_entry *entry = symtab_find(constants, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// returns the node only if it exists and is not restricted by the schema region.
static ast_node *find_usable_trigger(CSTR name, ast_node *err_target, CSTR msg) {
  ast_node *trigger_ast = find_trigger(name);

  if (!trigger_ast) {
    report_error(err_target, msg, name);
    return NULL;
  }

  if (!sem_validate_object_ast_in_current_region(name, trigger_ast, err_target, msg)) {
    return NULL;
  }

  return trigger_ast;
}

// Wrappers for the enum table.
static bool_t add_enum(ast_node *ast, CSTR name) {
  return symtab_add(enums, name, ast);
}

ast_node *find_enum(CSTR name) {
  symtab_entry *entry = symtab_find(enums, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Wrappers for the arg bundles table
cql_noexport bool_t add_arg_bundle(ast_node *ast, CSTR name) {
  return symtab_add(arg_bundles, name, ast);
}

cql_noexport ast_node *find_arg_bundle(CSTR name) {
  if (!Strcasecmp(name,  "LOCALS")) {
    return sem_synthesize_current_locals();
  }
  symtab_entry *entry = symtab_find(arg_bundles, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Wrappers for the proc table.
static bool_t add_proc(ast_node *ast, CSTR name) {
  return symtab_add(procs, name, ast);
}

cql_noexport ast_node *find_proc(CSTR name) {
  symtab_entry *entry = symtab_find(procs, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static bool_t add_unchecked_proc(ast_node *ast, CSTR name) {
  return symtab_add(unchecked_procs, name, ast);
}

cql_noexport ast_node *find_unchecked_proc(CSTR name) {
  symtab_entry *entry = symtab_find(unchecked_procs, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

cql_noexport bytebuf *find_proc_arg_info(CSTR name) {
  symtab_entry *entry = symtab_find(proc_arg_info, name);
  return entry ? (bytebuf *)(entry->val) : NULL;
}

static ast_node *find_upgrade_proc(CSTR name) {
  symtab_entry *entry = symtab_find(upgrade_procs, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static ast_node *find_ad_hoc_migrate(CSTR name) {
  symtab_entry *entry = symtab_find(ad_hoc_migrates, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Wrappers for shape table
static bool_t add_interface_type(ast_node *ast, CSTR name) {
  return symtab_add(interfaces, name, ast);
}

ast_node *find_interface_type(CSTR name) {
  symtab_entry *entry = symtab_find(interfaces, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Wrappers for the region table
cql_noexport ast_node *find_region(CSTR name) {
  symtab_entry *entry = symtab_find(schema_regions, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// Helper to store the named type in a symbol table. As with local variables,
// types declared in a procedure are local to that procedure and stored in their
// own symbol table. The local symbol table is always searched first to find named type
// otherwise the global storage is used.
static bool_t add_named_type(CSTR name, ast_node *ast) {
  symtab *tab;
  if (current_proc) {
    tab = local_types;
  } else {
    tab = global_types;
  }

  if (!symtab_add(tab, name, ast)) {
    report_error(ast, "CQL0359: duplicate type declaration", name);
    record_error(ast);
    return false;
  }
  return true;
}

// Look up the named type node from the relevant symbol tables.
// Recall that local named types are in their own table.
cql_noexport ast_node *find_named_type(CSTR name) {
  symtab_entry *entry = NULL;
  // We first try to find it in local storage because it has
  // higher priority otherwise we look into the global storage
  if (local_types) {
    entry = symtab_find(local_types, name);
  }
  if (!entry) {
    entry = symtab_find(global_types, name);
  }

  return entry ? (ast_node*)(entry->val) : NULL;
}

static ast_node *find_cur_region(CSTR name) {
  // during previous schema validation the current regions is the previous ones
  symtab_entry *entry = symtab_find(new_regions, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static bool_t add_region(ast_node *ast, CSTR name) {
  return symtab_add(schema_regions, name, ast);
}

// Wrappers for fragment symbol tables
static bool_t add_base_fragment(ast_node *ast, CSTR name) {
  return symtab_add(base_fragments, name, ast);
}

static bool_t add_extension_fragment(ast_node *ast, CSTR name) {
  return symtab_add(extension_fragments, name, ast);
}

static bool_t add_assembly_fragment(ast_node *ast, CSTR name) {
  return symtab_add(assembly_fragments, name, ast);
}

// This symbol table will accumulate the extension fragments
static void add_extension_to_base(ast_node *create_proc_stmt, CSTR name) {
  symtab_append_bytes(extensions_by_basename, name, &create_proc_stmt, sizeof(create_proc_stmt));
}

cql_noexport ast_node *find_base_fragment(CSTR name) {
  symtab_entry *entry = symtab_find(base_fragments, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static ast_node *find_extension_fragment(CSTR name) {
  symtab_entry *entry = symtab_find(extension_fragments, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

static ast_node *find_assembly_fragment(CSTR name) {
  symtab_entry *entry = symtab_find(assembly_fragments, name);
  return entry ? (ast_node*)(entry->val) : NULL;
}

// For debug/test output, pretty print a sem_type_core
static void get_sem_core(sem_t sem_type, charbuf *out) {
  switch (core_type_of(sem_type)) {
    case SEM_TYPE_NULL: bprintf(out, "null"); break;
    case SEM_TYPE_INTEGER: bprintf(out, "integer"); break;
    case SEM_TYPE_TEXT: bprintf(out, "text"); break;
    case SEM_TYPE_OBJECT: bprintf(out, "object"); break;
    case SEM_TYPE_BLOB: bprintf(out, "blob"); break;
    case SEM_TYPE_LONG_INTEGER: bprintf(out, "longint"); break;
    case SEM_TYPE_REAL: bprintf(out, "real"); break;
    case SEM_TYPE_BOOL: bprintf(out, "bool"); break;
    case SEM_TYPE_ERROR: bprintf(out, "err"); break;
    case SEM_TYPE_OK: bprintf(out, "ok"); break;
    case SEM_TYPE_REGION: bprintf(out, "region"); break;
    case SEM_TYPE_CURSOR_FORMAL: bprintf(out, "cursor"); break;
  }
}

// For debug/test output, prettyprint the flags
static void get_sem_flags(sem_t sem_type, charbuf *out) {
  // This is never present in the AST itself.
  Contract(!(sem_type & SEM_TYPE_ALIAS));

  // This is never present in the AST after a top-level statement has been
  // analyzed: All initialization improvements on variables and parameters are
  // unset by then, and, unlike `SEM_TYPE_INFERRED_NOTNULL`, there is no
  // equivalent of cql_inferred_notnull that would benefit from leaving it on
  // expressions.
  Contract(!(sem_type & SEM_TYPE_INIT_COMPLETE));

  if (sem_type & SEM_TYPE_INFERRED_NOTNULL) {
    bprintf(out, " inferred_notnull");
  }
  if (sem_type & SEM_TYPE_NOTNULL) {
    bprintf(out, " notnull");
  }
  if (sem_type & SEM_TYPE_VARIABLE) {
    bprintf(out, " variable");
  }
  if (sem_type & SEM_TYPE_INIT_REQUIRED) {
    bprintf(out, " init_required");
  }
  if (sem_type & SEM_TYPE_HAS_DEFAULT) {
    bprintf(out, " has_default");
  }
  if (sem_type & SEM_TYPE_HAS_CHECK) {
    bprintf(out, " has_check");
  }
  if (sem_type & SEM_TYPE_HAS_COLLATE) {
    bprintf(out, " has_collate");
  }
  if (sem_type & SEM_TYPE_IN_PARAMETER) {
    bprintf(out, " in");
  }
  if (sem_type & SEM_TYPE_OUT_PARAMETER) {
    bprintf(out, " out");
  }
  if (sem_type & SEM_TYPE_DML_PROC) {
    bprintf(out, " dml_proc");
  }
  if (sem_type & SEM_TYPE_HAS_SHAPE_STORAGE) {
    bprintf(out, " shape_storage");
  }
  if (sem_type & SEM_TYPE_CREATE_FUNC) {
    bprintf(out, " create_func");
  }
  if (sem_type & SEM_TYPE_SELECT_FUNC) {
    bprintf(out, " select_func");
  }
  if (sem_type & SEM_TYPE_DELETED) {
    bprintf(out, " deleted");
  }
  if (sem_type & SEM_TYPE_HIDDEN_COL) {
    bprintf(out, " hidden_col");
  }
  if (sem_type & SEM_TYPE_TVF) {
    bprintf(out, " table_valued_function");
  }
  if (sem_type & SEM_TYPE_VALIDATED) {
    bprintf(out, " validated");
  }
  if (sem_type & SEM_TYPE_PK) {
    bprintf(out, " primary_key");
  }
  if (sem_type & SEM_TYPE_FK) {
    bprintf(out, " foreign_key");
  }
  if (sem_type & SEM_TYPE_AUTOINCREMENT) {
    bprintf(out, " autoinc");
  }
  if (sem_type & SEM_TYPE_UK) {
    bprintf(out, " unique_key");
  }
  if (sem_type & SEM_TYPE_USES_OUT) {
    bprintf(out, " uses_out");
  }
  if (sem_type & SEM_TYPE_USES_OUT_UNION) {
    bprintf(out, " uses_out_union");
  }
  if (sem_type & SEM_TYPE_CALLS_OUT_UNION) {
    bprintf(out, " calls_out_union");
  }
  if (sem_type & SEM_TYPE_VALUE_CURSOR) {
    bprintf(out, " value_cursor");
  }
  if (sem_type & SEM_TYPE_SENSITIVE) {
    bprintf(out, " sensitive");
  }
  if (sem_type & SEM_TYPE_IMPLICIT) {
    bprintf(out, " implicit");
  }
  if (sem_type & SEM_TYPE_DEPLOYABLE) {
    bprintf(out, " deployable");
  }
  if (sem_type & SEM_TYPE_BOXED) {
    bprintf(out, " boxed");
  }
  if (sem_type & SEM_TYPE_VIRTUAL) {
    bprintf(out, " virtual");
  }
  if (sem_type & SEM_TYPE_INLINE_CALL) {
    bprintf(out, " inline_call");
  }
  if (sem_type & SEM_TYPE_SERIALIZE) {
    bprintf(out, " serialize");
  }
  if (sem_type & SEM_TYPE_FETCH_INTO) {
    bprintf(out, " fetch_into");
  }
  if (sem_type & SEM_TYPE_WAS_SET) {
    bprintf(out, " was_set");
  }
  if (sem_type & SEM_TYPE_BACKING) {
    bprintf(out, " backing");
  }
  if (sem_type & SEM_TYPE_BACKED) {
    bprintf(out, " backed");
  }
}

// For debug/test output, prettyprint a structure type
static void print_sem_struct(sem_struct *sptr) {
  cql_output("%s: { ", sptr->struct_name);
  for (int32_t i = 0; i < sptr->count; i++) {
    if (i != 0) {
      cql_output(", ");
    }
    CHARBUF_OPEN(temp);
    get_sem_core(sptr->semtypes[i], &temp);
    if (sptr->kinds[i]) {
     bprintf(&temp, "<%s>", sptr->kinds[i]);
    }
    get_sem_flags(sptr->semtypes[i], &temp);
    cql_output("%s: %s", sptr->names[i], temp.ptr);
    CHARBUF_CLOSE(temp);
  }
  cql_output(" }");
}

// For debug/test output, prettyprint a join type
static void print_sem_join(sem_join *jptr) {
  Contract(jptr);
  Contract(jptr->count);

  if (jptr->count == 1) {
    // If it's one join I just call it a because I feel bad calling it
    // a join until there's more than one.
    cql_output("TABLE { ");
  }
  else {
    cql_output("JOIN { ");
  }
  for (int32_t i = 0; i < jptr->count; i++) {
    if (i != 0) {
      cql_output(", ");
    }
    cql_output("%s: %s", jptr->names[i], jptr->tables[i]->struct_name);
  }
  cql_output(" }");
}

// This dispatches the other helpers to prettyprint the net type
cql_noexport void print_sem_type(sem_node *sem) {

  // in the event of error marking, disregard everything else
  if (is_sem_error(sem)) {
    CHARBUF_OPEN(temp);
    get_sem_core(SEM_TYPE_ERROR, &temp);
    cql_output("%s", temp.ptr);
    CHARBUF_CLOSE(temp);
    return;
  }

  if (sem->name) {
    cql_output("%s: ", sem->name);
  }

  sem_t sem_type = sem->sem_type;

  if (is_struct(sem_type)) {
     print_sem_struct(sem->sptr);
  }
  else if (is_join(sem_type)) {
    print_sem_join(sem->jptr);
  }
  else {
    CHARBUF_OPEN(temp);
    get_sem_core(sem_type, &temp);
    cql_output("%s", temp.ptr);
    CHARBUF_CLOSE(temp);
  }

  if (sem->kind) {
     cql_output("<%s>", sem->kind);
  }

  if (sem->value) {
    CHARBUF_OPEN(temp);
    eval_format_number(sem->value, EVAL_FORMAT_NORMAL, &temp);
    cql_output(" = %s", temp.ptr);
    CHARBUF_CLOSE(temp);
  }

  CHARBUF_OPEN(temp);
  get_sem_flags(sem_type, &temp);
  cql_output("%s", temp.ptr);
  CHARBUF_CLOSE(temp);

  if (sem->create_version > 0) {
    cql_output(" @create(%d)", sem->create_version);
  }
  if (sem->delete_version > 0) {
    cql_output(" @delete(%d)", sem->delete_version);
  }

  if (sem->recreate) {
    cql_output(" @recreate");
  }

  if (sem->recreate_group_name) {
    cql_output("(%s)", sem->recreate_group_name);
  }
}

// The standard error reporter, the ast node is used to get the line number
// the message is logged and the subject is cited if present.  The type
// of node is also included but it's frequently useless...
cql_noexport void report_error(ast_node *ast, CSTR msg, CSTR subject) {
  CSTR subj1 = "";
  CSTR subj2 = "";
  CSTR subj3 = "";

  if (subject) {
    subj1 = " '";
    subj2 = subject;
    subj3 = "'";
  }

  cql_error("%s:%d:1: error: in %s : %s%s%s%s\n",
      ast->filename,
      ast->lineno,
      ast->type,
      msg,
      subj1, subj2, subj3);
}

static void cql_attach_captured_errors(ast_node *stmt) {
   if (is_error(stmt)) {
      Invariant(stmt->sem);
      Invariant(stmt->sem->sem_type == SEM_TYPE_ERROR);
      stmt->sem->error = Strdup(error_capture->ptr);
   }
}

// Outside of normal statement list processing you have to do your own
// error capture logic; this helper capture a single error message.
// the general case is more felixble but typically not needed.  Since these
// errors don't flow up we have to mark the root directly so that the system
// knows there were semantic errors.  This flow is only for deferred errors.
static void report_and_capture_error(ast_node *root, ast_node *ast, CSTR err_msg, CSTR name) {
  CHARBUF_OPEN(errbuf);

  if (options.print_ast) {
    error_capture = &errbuf;
  }

  report_error(ast, err_msg, name);
  record_error(ast);
  record_error(root);

  if (error_capture) {
    cql_attach_captured_errors(ast);
    error_capture = NULL;
  }

  CHARBUF_CLOSE(errbuf);
}

// error reporter for appending extra info on mismatched sem types where
// exact type is expected
static void report_sem_type_mismatch(
    sem_t sem_expected_type,
    sem_t sem_actual_type,
    ast_node *node,
    CSTR prepend_error_message,
    CSTR sem_name) {
  CHARBUF_OPEN(temp);

  bprintf(&temp, "%s (expected ", prepend_error_message);
  get_sem_core(sem_expected_type, &temp);
  sem_expected_type &= (SEM_TYPE_NOTNULL | SEM_TYPE_SENSITIVE);
  get_sem_flags(sem_expected_type, &temp);

  bprintf(&temp, "; found ");
  get_sem_core(sem_actual_type, &temp);
  sem_actual_type &= (SEM_TYPE_NOTNULL | SEM_TYPE_SENSITIVE);
  get_sem_flags(sem_actual_type, &temp);
  bprintf(&temp, ")");

  report_error(node, temp.ptr, sem_name);
  CHARBUF_CLOSE(temp);
}

// This is the basic constructor for the semantic info node.
cql_noexport sem_node * new_sem(sem_t sem_type) {
  sem_node *sem = _ast_pool_new(sem_node);
  sem->sem_type = sem_type;
  sem->name = NULL;
  sem->error = NULL;
  sem->kind = NULL;
  sem->sptr = NULL;
  sem->jptr = NULL;
  sem->create_version = -1;
  sem->delete_version = -1;
  sem->unsub_version = 0;
  sem->resub_version = 0;
  sem->recreate = false;
  sem->recreate_group_name = NULL;
  sem->used_symbols = NULL;
  sem->index_list = NULL;
  sem->region = NULL;
  sem->value = NULL;
  return sem;
}

// Sets additional flags for `ast->sem->sem_type` without mutating other
// copies of `ast->sem`.
cql_noexport void sem_add_flags(ast_node *ast, sem_t flags) {
  sem_node *sem = _ast_pool_new(sem_node);
  memcpy(sem, ast->sem, sizeof(sem_node));
  sem->sem_type |= flags;
  ast->sem = sem;
}

// Removes specified flags for `ast->sem->sem_type` without mutating other
// copies of `ast->sem`.
cql_noexport void sem_remove_flags(ast_node *ast, sem_t flags) {
  sem_node *sem = _ast_pool_new(sem_node);
  memcpy(sem, ast->sem, sizeof(sem_node));
  sem->sem_type &= sem_not(flags);
  ast->sem = sem;
}

// Like `add_sem_flags`, but completely replaces the flags instead of adding
// additional flags.
static void sem_replace_flags(ast_node *ast, sem_t flags) {
  sem_node *sem = _ast_pool_new(sem_node);
  memcpy(sem, ast->sem, sizeof(sem_node));
  sem->sem_type = flags;
  ast->sem = sem;
}

// For cases where we just want to record that there was no error
// we use the canonical ok node.  It must never be modified because
// it is shared.
static sem_node *sem_ok;

static sem_node *ok_sentinel(void) {
  if (sem_ok) {
    return sem_ok;
  }
  else {
    return sem_ok = new_sem(SEM_TYPE_OK);
  }
}

// Get the index of a column by name from the struct
// We need this so that we can validate the presence of columns
// in a particular struct.
int32_t sem_column_index(sem_struct *sptr, CSTR name) {
  uint32_t count = sptr->count;
  for (int32_t i = 0; i < count; i++) {
    CSTR col = sptr->names[i];
    if (!strcmp(name, col)) {
      return i;
    }
  }
  return -1;
}

// Stow the ok marker somewhere.
cql_noexport void record_ok(ast_node *ast) {
  ast->sem = ok_sentinel();
}

// Errors may be annotated with information so we make a unique error
// node for every place we're placing a new error.
cql_noexport void record_error(ast_node *ast) {
  ast->sem = new_sem(SEM_TYPE_ERROR);
}

// Some math operators like << >> & | % only make sense on integers
// This function does the extra checking to ensure they do not get real values
// as arguments.  It's a post-pass after the normal math checks.
static void sem_reject_real(ast_node *ast, CSTR op) {
  if (!is_error(ast)) {
    sem_t core_type = core_type_of(ast->sem->sem_type);
    if (core_type == SEM_TYPE_REAL) {
      report_error(ast, "CQL0001: operands must be an integer type, not real", op);
      record_error(ast);
    }
  }
}

// sem_struct records the information for one "table" it's an array
// of names and primitive types.
static sem_struct * new_sem_struct(CSTR name, uint32_t count) {
  sem_struct *sptr = _ast_pool_new(sem_struct);
  sptr->struct_name = name;
  sptr->count = count;
  sptr->names = _ast_pool_new_array(CSTR, count);
  sptr->kinds = _ast_pool_new_array(CSTR, count);
  sptr->semtypes = _ast_pool_new_array(sem_t, count);

  for (int32_t i = 0; i < count; i++) {
    sptr->names[i] = NULL;
    sptr->semtypes[i] = SEM_TYPE_ERROR;
    sptr->kinds[i] = NULL;
  }

  return sptr;
}

// sem_join records the concatenation of 1 or more sem_structs
// note that a single table can be a "join" if it's all there is
// the current result of the FROM clause as it accumulates is
// one of these.
static sem_join * new_sem_join(uint32_t count) {
  sem_join *jptr = _ast_pool_new(sem_join);
  jptr->count = count;
  jptr->names = _ast_pool_new_array(CSTR, count);
  jptr->tables = _ast_pool_new_array(sem_struct *, count);

  for (int32_t i = 0; i < count; i++) {
    jptr->names[i] = NULL;
    jptr->tables[i] = NULL;
  }

  return jptr;
}

// When we're joining with join types other than INNER it's
// possible to lose the notnull flag
// e.g. in "X left outer join Y" even if Y has only not-null columns
// the result of the join will have Y with all nullable columns
// because it's a left outer join.  This method produces a sem_struct
// with the indicated removals.
static sem_struct *sem_clone_struct_strip_flags(sem_struct *sptr, sem_t strip) {
  sem_struct *result = new_sem_struct(sptr->struct_name, sptr->count);
  for (int32_t i = 0; i < sptr->count; i++) {
    result->names[i] = sptr->names[i];
    result->kinds[i] = sptr->kinds[i];
    result->semtypes[i] = sptr->semtypes[i] & sem_not(strip);
  }
  return result;
}

// When making the initial join scope for a table we want
// to get rid of other table-ish flags like HAS_DEFAULT and AUTOINCREMENT
// they don't contribute to anything and they make the tree ugly.
static sem_struct *new_sem_struct_strip_table_flags(sem_struct *sptr) {
  sem_t allowed_flags = SEM_TYPE_CORE | SEM_TYPE_NOTNULL | SEM_TYPE_SENSITIVE | SEM_TYPE_HIDDEN_COL | SEM_TYPE_ALIAS;

  return sem_clone_struct_strip_flags(sptr, sem_not(allowed_flags));
}

// Create a base join type from a single struct.
static sem_join *sem_join_from_sem_struct(sem_struct *sptr) {
  sem_join *jptr = new_sem_join(1);
  jptr->names[0] = sptr->struct_name;
  jptr->tables[0] = new_sem_struct_strip_table_flags(sptr);

  return jptr;
}

// If either of the types is an object then produce an error on the ast.
static bool_t error_any_object(ast_node *ast, sem_t core_type_left, sem_t core_type_right, CSTR op) {
  if (is_object(core_type_left)) {
    report_error(ast->left, "CQL0002: left operand cannot be an object in", op);
    record_error(ast);
    return true;
  }

  if (is_object(core_type_right)) {
    report_error(ast->right, "CQL0003: right operand cannot be an object in", op);
    record_error(ast);
    return true;
  }

  return false;
}

// If either of the types is a blob then produce an error on the ast.
static bool_t error_any_blob_types(ast_node *ast, sem_t core_type_left, sem_t core_type_right, CSTR op) {
  if (is_blob(core_type_left)) {
    report_error(ast->left, "CQL0004: left operand cannot be a blob in", op);
    record_error(ast);
    return true;
  }

  if (is_blob(core_type_right)) {
    report_error(ast->right, "CQL0005: right operand cannot be a blob in", op);
    record_error(ast);
    return true;
  }

  return false;
}

// If either of the types is text then produce an error on the ast.
static bool_t error_any_text_types(ast_node *ast, sem_t core_type_left, sem_t core_type_right, CSTR op) {
  if (is_text(core_type_left)) {
    report_error(ast->left, "CQL0007: left operand cannot be a string in", op);
    record_error(ast);
    return true;
  }

  if (is_text(core_type_right)) {
    report_error(ast->right, "CQL0008: right operand cannot be a string in", op);
    record_error(ast);
    return true;
  }

  return false;
}

// This is the work horse of semantic analysis, it checks if sem_type_needed
// is compatible with core_type_found and generates an error if it is not.
static bool_t sem_verify_compat(ast_node *ast, sem_t sem_type_needed, sem_t sem_type_found, CSTR subject) {
  // normalize even if we weren't given core types
  sem_t core_type_needed = core_type_of(sem_type_needed);
  sem_t core_type_found = core_type_of(sem_type_found);

  // Note that SEM_TYPE_CURSOR_FORMAL never gets here... by the time we've
  // found a parameter slot that needs a cursor we already used sem_cursor
  // and it's all been verified.  So even though the param list can have a non-unitary
  // type we don't ever test non-unitary here.

  Invariant(is_unitary(core_type_needed));
  Invariant(is_unitary(core_type_found));

  switch (core_type_needed) {
    case SEM_TYPE_TEXT:
      if (!is_string_compat(core_type_found)) {
        report_error(ast, "CQL0009: incompatible types in expression", subject);
        record_error(ast);
        return false;
      }
      break;

    case SEM_TYPE_OBJECT:
      if (!is_object_compat(core_type_found)) {
        report_error(ast, "CQL0010: incompatible types in expression", subject);
        record_error(ast);
        return false;
      }
      break;

    case SEM_TYPE_BLOB:
      if (!is_blob_compat(core_type_found)) {
        report_error(ast, "CQL0011: incompatible types in expression", subject);
        record_error(ast);
        return false;
      }
      break;

    case SEM_TYPE_BOOL:
    case SEM_TYPE_INTEGER:
    case SEM_TYPE_LONG_INTEGER:
    case SEM_TYPE_REAL:
      if (!is_numeric_compat(core_type_found)) {
        report_error(ast, "CQL0012: incompatible types in expression", subject);
        record_error(ast);
        return false;
      }
      break;

    case SEM_TYPE_NULL:
      // null is compatible with everything
      break;
  }

  return true;
}

// When performing assignment either explicitly ( set X := Y ) or implicit (binding args to a proc call)
// there are additional type compat checks to be done beyond the normal is compat.  The above helps you
// with symmetric operations like X == Y where either side can be promoted.  In an assignment the left
// side cannot be promoted so the store can be lossy.  This checks for the lossy cases that are otherwise
// compatible.  That is, we assume that the above has already been called.
static bool_t sem_verify_safeassign(ast_node *ast, sem_t sem_type_needed, sem_t sem_type_found, CSTR subject) {
  // normalize even if we weren't given core types
  sem_t core_type_needed = core_type_of(sem_type_needed);
  sem_t core_type_found = core_type_of(sem_type_found);
  CSTR err_type = NULL;

  Invariant(is_unitary(core_type_needed));
  Invariant(is_unitary(core_type_found));

  // the target of an assignment cannot be of type null
  Invariant(core_type_needed != SEM_TYPE_NULL);

  switch (core_type_needed) {
    case SEM_TYPE_TEXT:
    case SEM_TYPE_OBJECT:
    case SEM_TYPE_BLOB:
    case SEM_TYPE_BOOL:
    case SEM_TYPE_REAL:
      // this is called only after we've already verified basic compatibility (see above)
      // so these are always safe
      //  * assign to real gives you a free floating conversion
      //  * assign to bool converts to truthiness
      //  * blob, object, text require exact match for compat
      return true;

    // these are the possible lossy cases

    case SEM_TYPE_INTEGER:
    case SEM_TYPE_LONG_INTEGER:
      if (core_type_found == SEM_TYPE_REAL) {
         err_type = "REAL";
         goto error;
      }

      if (core_type_found == SEM_TYPE_LONG_INTEGER && core_type_needed == SEM_TYPE_INTEGER) {
         err_type = "LONG_INT";
         goto error;
      }
      break;
  }

  Invariant(!err_type);
  return true;

error:
  Invariant(err_type);
  CHARBUF_OPEN(tmp);
  bprintf(&tmp, "CQL0242: lossy conversion from type '%s' in ", err_type);

  // append the text of the offensive expression
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_echo; // we want all the text, unexpanded, so NOT for sqlite output (this is raw echo)
  gen_set_output_buffer(&tmp);
  gen_with_callbacks(ast, gen_root_expr, &callbacks);
  report_error(ast, tmp.ptr, NULL);
  CHARBUF_CLOSE(tmp);

  return false;
}

// This verifies that the types are compatible and that it's ok to assign
// the expression to the variable.  In practice that means:
// * the variable type core type and kind must be compatible with the expression core type and kind
// * the variable must be nullable if the expression is nullable
// * the variable must be sensitive if the assignment is sensitive
// * the variable type must be bigger than the expression type
// Here ast is used only to give a place to put any errors.
cql_noexport bool_t sem_verify_assignment(ast_node *ast, sem_t sem_type_needed, sem_t sem_type_found, CSTR var_name) {
  if (!sem_verify_compat(ast, sem_type_needed, sem_type_found, var_name)) {
    return false;
  }

  if (!sem_verify_safeassign(ast, sem_type_needed, sem_type_found, var_name)) {
    return false;
  }

  if (is_nullable(sem_type_found) && is_not_nullable(sem_type_needed)) {
    report_error(ast, "CQL0013: cannot assign/copy possibly null expression to not null target", var_name);
    return false;
  }

  if (sensitive_flag(sem_type_found) && !sensitive_flag(sem_type_needed)) {
    report_error(ast, "CQL0014: cannot assign/copy sensitive expression to non-sensitive target", var_name);
    return false;
  }

  return true;
}

// The second workhorse of semantic analysis, given two types that
// are previously known to be compatible, it returns the smallest type
// that holds both.  If either is nullable the result is nullable.
// Note: in the few cases where that isn't true the normal algorithm for
// nullablity result must be overridden (see coalesce for instance).
static sem_t sem_combine_types(sem_t sem_type_1, sem_t sem_type_2) {
  sem_t combined_flags = combine_flags(sem_type_1, sem_type_2);
  sem_t core_type_1 = core_type_of(sem_type_1);
  sem_t core_type_2 = core_type_of(sem_type_2);
  sem_t ret = 0;

  // early out for the easy case.
  if (core_type_1 == core_type_2) {
    return core_type_1 | combined_flags;
  }

  // We always validate that it's ok to combine types before we combine.
  switch (core_type_1) {
    case SEM_TYPE_TEXT:
      // if you combine a text with anything (text or NULL) you get TEXT
      Invariant(is_string_compat(core_type_2));
      ret = SEM_TYPE_TEXT;
      break;

    case SEM_TYPE_BLOB:
      // if you combine an object with anything (object or NULL) you get OBJECT
      Invariant(is_blob_compat(core_type_2));
      ret = SEM_TYPE_BLOB;
      break;

    case SEM_TYPE_OBJECT:
      // if you combine an object with anything (object or NULL) you get OBJECT
      Invariant(is_object_compat(core_type_2));
      ret = SEM_TYPE_OBJECT;
      break;

    case SEM_TYPE_REAL:
      // If you combine a real with any other numeric you get a real.
      Invariant(is_numeric_compat(core_type_2));
      ret = SEM_TYPE_REAL;
      break;

    case SEM_TYPE_LONG_INTEGER:
      // If you combine a long integer with a real you get real.
      // If you combine it with any other numeric type you get a long integer.
      Invariant(is_numeric_compat(core_type_2));
      if (core_type_2 == SEM_TYPE_REAL) {
        ret = SEM_TYPE_REAL;
      }
      else {
        ret = SEM_TYPE_LONG_INTEGER;
      }
      break;

    case SEM_TYPE_INTEGER:
      // If you combine an integer with a real you get real.
      // If you combine an integer with a long integer you get long integer.
      // If you combine it with any other numeric type you get an integer.
      Invariant(is_numeric_compat(core_type_2));
      if (core_type_2 == SEM_TYPE_REAL || core_type_2 == SEM_TYPE_LONG_INTEGER) {
        ret = core_type_2;
      }
      else {
        ret = SEM_TYPE_INTEGER;
      }
      break;

    case SEM_TYPE_BOOL:
      // If you combine bool with any numeric type it upgrades to that type.
      // If you combine it with null, you get a nullable bool.
      Invariant(is_numeric_compat(core_type_2));
      if (core_type_2 == SEM_TYPE_NULL) {
        ret = SEM_TYPE_BOOL;
      }
      else {
        ret = core_type_2;
      }
      break;

    case SEM_TYPE_NULL:
      // If you combine null with anything you get nullable that thing.
      Invariant(is_nullable(combined_flags));
      ret = core_type_2;
      break;
  }

  return ret | combined_flags;
}

// Recursive analysis: complain if the result is not numeric.
// this is the first method that shows the error propagation rules
// generally, errors bubble up but once one has been reported in a subtree
// we do not keep reporting more (saving insane amounts of output).
// the context here is used to create a better error location;
// the caller often has a good idea what line number would be a better choice
// than the expression itself.
static void sem_numeric_expr(ast_node *expr, ast_node *context, CSTR subject, uint32_t expr_context) {
  Contract(expr);
  sem_root_expr(expr, expr_context);

  if (!is_error(expr) && !is_numeric_expr(expr)) {
    ast_node *best = context ? context : expr;
    report_error(best, "CQL0015: expected numeric expression", subject);
    record_error(expr);
  }

  if (context) {
    context->sem = expr->sem;
  }
}

// Given two table nodes attempt to produce the join of them according to the join type.
static void join_tables(ast_node *t1, ast_node *t2, ast_node *result, int32_t join_type) {
  if (is_error(t1) || is_error(t2)) {
    record_error(result);
    return;
  }

  if (enforcement.strict_table_function) {
     bool_t t1_tvf = !!(t1->sem->sem_type & SEM_TYPE_TVF);
     bool_t t2_tvf = !!(t2->sem->sem_type & SEM_TYPE_TVF);
     bool_t error = false;
     switch (join_type) {
       case JOIN_CROSS:
         // cross is the same as INNER in SQLITE (only optimization differences to suppress optimization)
         // there is no full outer join
         break;
       case JOIN_LEFT_OUTER:
       case JOIN_LEFT:
         error = t2_tvf;
         break;
       case JOIN_RIGHT_OUTER:
       case JOIN_RIGHT:
         // sqlite doesn't actually have right join; attempting to use it will net you syntax errors
         // CQL is forward looking in this regard...
         error = t1_tvf;
         break;
     }

     if (error) {
       report_error(result, "CQL0371: table valued function used in a left/right/cross context; this would hit a SQLite bug.  Wrap it in a CTE instead.", NULL);
       record_error(result);
       return;
     }
  }

  sem_join *j1 = t1->sem->jptr;
  sem_join *j2 = t2->sem->jptr;
  Invariant(j1);
  Invariant(j2);

  // First make sure the resulting type could be reasonably used, no duplicate table names
  // note that the semantic names include any aliasing.
  for (int32_t i = 0; i < j1->count; i++) {
    for (int32_t j = 0; j < j2->count; j++) {
      CSTR n1 = j1->names[i];
      CSTR n2 = j2->names[j];

      if (!Strcasecmp(n1, n2)) {
        report_error(t2, "CQL0016: duplicate table name in join", n1);
        record_error(result);
        return;
      }
    }
  }

  // Now create the resulting data type, at this point we're good to go.
  sem_join *jptr = new_sem_join(j1->count + j2->count);

   // the join type will tell us which side(s) need not null removed
  sem_t strip_left = 0;
  sem_t strip_right = 0;

  switch (join_type) {
    case JOIN_INNER:
    case JOIN_CROSS:
      // cross join is the same as inner join in SQLite
      // the only difference is the optimizer declines to reorder cross joins as a hint
      // there is no full outer join
      strip_left = strip_right = 0;
      break;
    case JOIN_LEFT_OUTER:
    case JOIN_LEFT:
      // Note: left outer join can result in not nulls even if there is no join condition
      // because the table on the right might be empty
      strip_left = 0;
      strip_right = SEM_TYPE_NOTNULL;
      break;
    case JOIN_RIGHT_OUTER:
    case JOIN_RIGHT:
      // Note: SQLite doesn' have right join yet so this is forward looking
      strip_left = SEM_TYPE_NOTNULL;
      strip_right = 0;
      break;
  }

  // Now just copy over the names and the tables.
  int32_t j = 0;
  for (int32_t i = 0; i < j1->count; i++, j++) {
    jptr->names[j] = j1->names[i];
    jptr->tables[j] = sem_clone_struct_strip_flags(j1->tables[i], strip_left);
  }
  for (int32_t i = 0; i < j2->count; i++, j++) {
    jptr->names[j] = j2->names[i];
    jptr->tables[j] = sem_clone_struct_strip_flags(j2->tables[i], strip_right);
  }

  result->sem = new_sem(SEM_TYPE_JOIN);
  result->sem->jptr = jptr;
}

// In cases where the result of the join is now senstive we need to
// add a flag bit to the join columns.  Once you constrain on a senstive
// column all the columns become sensitive.
static void sem_add_flags_to_join(sem_join *jptr, sem_t flags) {
  for (int32_t i = 0; i <jptr->count; i++) {
    sem_struct *sptr = jptr->tables[i];
    for (int32_t j = 0; j < sptr->count; j++) {
      sptr->semtypes[j] |= flags;
    }
  }
}

// Given a column ast type convert it to the appropriate sem_type.
static void sem_data_type_column(ast_node *ast) {
  // The data_type could be a declare named type, therefore
  // we should rewrite the node to the real type
  rewrite_data_type_if_needed(ast);
  if (is_error(ast)) {
    record_error(ast);
    return;
  }

  if (is_ast_type_int(ast)) {
    ast->sem = new_sem(SEM_TYPE_INTEGER);
  } else if (is_ast_type_text(ast)) {
    ast->sem = new_sem(SEM_TYPE_TEXT);
  } else if (is_ast_type_blob(ast)) {
    ast->sem = new_sem(SEM_TYPE_BLOB);
  } else if (is_ast_type_object(ast)) {
    ast->sem = new_sem(SEM_TYPE_OBJECT);
  } else if (is_ast_type_long(ast)) {
    ast->sem = new_sem(SEM_TYPE_LONG_INTEGER);
  } else if (is_ast_type_real(ast)) {
    ast->sem = new_sem(SEM_TYPE_REAL);
  } else if (is_ast_type_cursor(ast)) {
    ast->sem = new_sem(SEM_TYPE_CURSOR_FORMAL);
  } else {
    Contract(is_ast_type_bool(ast));
    ast->sem = new_sem(SEM_TYPE_BOOL);
  }

  if (ast->left) {
    EXTRACT_STRING(kind, ast->left);
    ast->sem->kind = kind;

    bool_t is_set = !!ends_in_set(kind);
    bool_t is_cursor = !!ends_in_cursor(kind);

    if (is_set || is_cursor) {
      // <T SET> and <T CURSOR> get additional checks

      // now we extract just the type name
      CHARBUF_OPEN(tmp);
      for (int32_t i = 0; kind[i] && kind[i] != ' '; i++) {
        bputc(&tmp, kind[i]);
      }

      // We make a like node for the object type (which is itself not in AST here)
      // so that we can use the standard likeable helpers for error checking
      AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
      ast_node *type_node = new_ast_str(tmp.ptr);
      ast_node *like_node = new_ast_like(type_node, NULL);
      AST_REWRITE_INFO_RESET();

      CHARBUF_CLOSE(tmp);

      // the indicated type must be a valid shape name (one we could use in LIKE T)
      ast_node *like_target = sem_find_shape_def_base(like_node, LIKEABLE_FOR_VALUES);
      if (!like_target) {
        record_error(ast);
        return;
      }

      // it's a result set so it must also be a proc type
      if (is_set && !is_ast_create_proc_stmt(like_target) && !is_ast_declare_proc_stmt(like_target)) {
        report_error(ast, "CQL0090: object<T SET> has a T that is not a procedure with a result set", kind);
        record_error(ast);
        return;
      }
    }
  }
}

// Create the semantic type, it might be wrapped
// in a not_null node, extract that.
static void sem_data_type_var(ast_node *ast) {
  // The data_type could be a declare named type, therefore
  // we should rewrite the node to the real type
  rewrite_data_type_if_needed(ast);
  if (is_error(ast)) {
    record_error(ast);
    return;
  }

  if (is_ast_create_data_type(ast)) {
    ast_node *data_type = ast->left;

    sem_data_type_var(data_type);
    if (is_error(data_type)) {
      record_error(ast);
      return;
    }

    sem_t core_type = core_type_of(data_type->sem->sem_type);

    // The create data type is restricted to text, blob, object only.
    if (core_type != SEM_TYPE_TEXT && core_type != SEM_TYPE_BLOB && core_type != SEM_TYPE_OBJECT) {
      report_error(ast, "CQL0361: return data type in a create function declaration can only be text, blob or object", NULL);
      record_error(ast);
      return;
    }

    // Create a node for me using my child's type but adding func create.
    ast->sem = new_sem(SEM_TYPE_CREATE_FUNC | data_type->sem->sem_type);
    // copy object type to the sem if applicable. It's used to rewrite
    // named type ast.
    ast->sem->kind = data_type->sem->kind;
  }
  else if (is_ast_notnull(ast)) {
    EXTRACT_ANY_NOTNULL(data_type, ast->left);
    sem_data_type_var(data_type);

    if (data_type->sem->sem_type & SEM_TYPE_NOTNULL) {
      report_error(ast, "CQL0367: an attribute was specified twice", "not null");
      record_error(ast);
      return;
    }

    // Create a node for me using my child's type but adding not null.
    ast->sem = new_sem(SEM_TYPE_NOTNULL | data_type->sem->sem_type);
    // copy object type to the sem if applicable. It's used to rewrite
    // named type ast.
    ast->sem->kind = data_type->sem->kind;
  }
  else if (is_ast_sensitive_attr(ast)) {
    EXTRACT_ANY_NOTNULL(data_type, ast->left);
    sem_data_type_var(data_type);
    if (is_error(data_type)) {
      record_error(ast);
      return;
    }

    if (data_type->sem->sem_type & SEM_TYPE_SENSITIVE) {
      report_error(ast, "CQL0367: an attribute was specified twice", "@sensitive");
      record_error(ast);
      return;
    }

    // Create a node for me using my child's type but adding not null.
    ast->sem = new_sem(SEM_TYPE_SENSITIVE | data_type->sem->sem_type);
    // copy object type to the sem if applicable. It's used to rewrite
    // named type ast.
    ast->sem->kind = data_type->sem->kind;
  }
  else {
    sem_data_type_column(ast);
  }
}

// Use the standard name checker to check for valid names in this scope
// Items must be in scope and no duplicate names are allowed.
static bool_t sem_validate_name_list(ast_node *name_list, sem_join *jptr) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_name_list(name_list) || is_ast_indexed_columns(name_list));

  name_check check;
  init_name_check(&check, name_list, jptr);
  bool_t valid = sem_name_check(&check);
  destroy_name_check(&check);
  return valid;
}

// Here we check the index found in the "previous" schema against the current schema.
// There are several validations we have to do here:
//  * the index should be present (but maybe marked with @delete)
//  * the index has to have a compatible create version
//  * the index has to have a compatible delete version
static void sem_validate_previous_index(ast_node *prev_index) {
  Contract(!current_joinscope);

  Contract(is_ast_create_index_stmt(prev_index));
  EXTRACT_NOTNULL(create_index_on_list, prev_index->left);
  EXTRACT_NOTNULL(flags_names_attrs, prev_index->right);
  EXTRACT_NOTNULL(connector, flags_names_attrs->right);
  EXTRACT_NOTNULL(index_names_and_attrs, connector->left);
  EXTRACT_NOTNULL(indexed_columns, index_names_and_attrs->left);
  EXTRACT(opt_where, index_names_and_attrs->right);
  EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
  EXTRACT_STRING(index_name, index_name_ast);
  EXTRACT_ANY_NOTNULL(table_name_ast, create_index_on_list->right);
  EXTRACT_STRING(table_name, table_name_ast);

  ast_node *ast = find_index(index_name);

  if (!ast) {
    if (options.schema_exclusive) {
      // In exclusive schema mode, unknown indices are bulk deleted
      // therefore you do not need a tombstone
      return;
    }

    // If the table the index was on is going away then we don't need
    // to verify that the index has a tombstone.  In fact it is not
    // possible to declare the tombstone now because the table name is not
    // valid.  There's no need for the tombstone anyway because when the
    // table is deleted all its indices will also be deleted.
    ast_node *table_ast = find_table_or_view_even_deleted(table_name);

    // the table must exist and be affirmatively deleted to avoid the error!
    if (table_ast && table_ast->sem->delete_version > 0) {
      return;
    }

    report_error(prev_index, "CQL0017: index was present but now it does not exist (use @delete instead)", index_name);
    record_error(prev_index);
    return;
  }

  enqueue_pending_region_validation(prev_index, ast, index_name);
}

// We often need to find the index of a particular column
cql_noexport int32_t find_col_in_sptr(sem_struct *sptr, CSTR name) {
  for (int32_t i = 0; i < sptr->count; i++) {
    if (!Strcasecmp(sptr->names[i], name)) {
      return i;
    }
  }
  return -1;
}

// Helper function to update the column type in a table node.
static void sem_update_column_type(ast_node *table_ast, ast_node *columns, sem_t type) {
  Contract(is_ast_name_list(columns) || is_ast_indexed_columns(columns));

  sem_struct *sptr = table_ast->sem->sptr;
  sem_join *jptr = table_ast->sem->jptr;
  for (ast_node *item = columns; item; item = item->right) {
    ast_node *name_ast = item->left;
    if (is_ast_indexed_column(name_ast)) {
      name_ast = name_ast->left;
    }

    if (is_ast_str(name_ast)) {
      EXTRACT_STRING(name, name_ast);

      // always a valid column name, it MUST match
      int32_t i = find_col_in_sptr(sptr, name);
      Invariant(i >= 0);
      sptr->semtypes[i] |= type;
      jptr->tables[0]->semtypes[i] |= type;
    }
  }
}

// This is only for indices and triggers, they have no @create annotation ever
// as they are always @recreate objects, but they can be deleted.  All we need to do
// is verify that they have no delete migration proc; it's not safe for them to have such
// a proc because indices and triggers must be removed entirely if their table is ever deleted
// at which point the migration proc would vanish.  To avoid this problem we don't support
// migration procs on these objects.
static bool_t sem_validate_no_delete_migration(version_attrs_info *vers_info, ast_node *ast, CSTR obj_name) {
  Contract(vers_info);
  Contract(vers_info->create_version < 0);
  Contract(!vers_info->create_proc);

  if (vers_info->delete_proc) {
    report_error(ast, "CQL0321: migration proc not allowed on object", obj_name);
    record_error(ast);
    return false;
  }

  return true;
}

// Top level index creation, we don't really do anything with indices
// in CQL but we do validate that they make sense (so we lookup all the names)
// using the helper above.
static void sem_create_index_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  Contract(is_ast_create_index_stmt(ast));
  EXTRACT_NOTNULL(create_index_on_list, ast->left);
  EXTRACT_NOTNULL(flags_names_attrs, ast->right);
  EXTRACT_NOTNULL(connector, flags_names_attrs->right);
  EXTRACT_NOTNULL(index_names_and_attrs, connector->left);
  EXTRACT_NOTNULL(indexed_columns, index_names_and_attrs->left);
  EXTRACT(opt_where, index_names_and_attrs->right);
  EXTRACT_ANY(attrs, connector->right);
  EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
  EXTRACT_STRING(index_name, index_name_ast);
  EXTRACT_ANY_NOTNULL(table_name_ast, create_index_on_list->right);
  EXTRACT_STRING(table_name, table_name_ast);

  // Index declarations (i.e. outside of any proc) are totally ignored
  // in the context of a schema migration script.  This prevents us from
  // getting errors because the index refers to tables or columns that are not yet
  // in existence in the version we are migrating.  If you need an index
  // in your migration script you have to create it and use it yourself
  // since you can't rely on the presence of that index during migration anyway.
  if (schema_upgrade_version > 0 && !current_proc) {
    record_ok(ast);
    return;
  }

  if (validating_previous_schema) {
    record_ok(ast);
    sem_validate_previous_index(ast);
    return;
  }

  bool_t adding_current_entity = will_add_current_entity();

  // if there is an existing index, save it here so we can check for duplicates later.
  ast_node *existing_defn = adding_current_entity ? find_index(index_name) : NULL;

  version_attrs_info vers_info;
  init_version_attrs_info(&vers_info, index_name, ast, attrs);
  bool_t valid_version_info = sem_validate_version_attrs(&vers_info);
  Invariant(valid_version_info);  // nothing can go wrong with index version info

  if (!sem_validate_vers_ok_in_context(&vers_info)) {
    record_error(ast);
    return;
  }

  if (!sem_validate_no_delete_migration(&vers_info, ast, index_name)) {
    return;
  }

  ast_node *table_ast = NULL;
  bool_t deleting = vers_info.delete_version > 0;

  if (deleting) {
    table_ast = find_usable_table_or_view_even_deleted(
      table_name,
      table_name_ast,
      "CQL0019: create index table name not found");

    if (is_deleted(table_ast)) {
      report_error(ast, "CQL0397: object is an orphan because its table is deleted. Remove rather than @delete", index_name);
      record_error(ast);
      return;
    }
  }
  else {
    table_ast = find_usable_and_not_deleted_table_or_view(
      table_name,
      table_name_ast,
      "CQL0019: create index table name not found");
  }

  if (!table_ast) {
    record_error(ast);
    return;
  }

  sem_non_blob_storage_table(ast, table_ast);
  if (is_error(ast)) {
    return;
  }

  if (is_virtual_ast(table_ast)) {
    report_error(table_name_ast, "CQL0159: cannot add an index to a virtual table", table_name);
    record_error(ast);
    return;
  }

  // It's only interesting to check for this error in the main schema declarations, not in previous schema
  // and not in schema upgrade scripts (which are driven by correct regions).  "adding_current_entity"
  // is for exactly those cases.
  if (table_ast->sem->recreate && adding_current_entity) {
    CSTR table_region = table_ast->sem->region;
    if (table_region != current_region) {
      // The only valid cases are both null or both the current not-null region string.
      // NOTE: the region string is canonical (normalized in begin region) so you don't even have to check the text
      report_error(ast, "CQL0066: if a table is marked @recreate, its indices must be in its schema region", index_name);
      record_error(ast);
      return;
    }
  }

  // CREATE INDEX [index_name] ON [table-name] ( [name_list] )
  // don't check the index names if we're deleting the index, they are useless anyway
  if (!deleting && !sem_validate_name_list(indexed_columns, table_ast->sem->jptr)) {
    record_error(ast);
    return;
  }

  if (opt_where) {
    EXTRACT_ANY_NOTNULL(expr, opt_where->left);
    sem_validate_check_expr_for_table(table_ast, expr, "WHERE");
    opt_where->sem = expr->sem;
    if (is_error(expr))  {
      record_error(ast);
      return;
    }
  }

  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->delete_version = vers_info.delete_version;
  ast->sem->region = current_region;

  if (existing_defn) {
    if (!sem_validate_identical_ddl(existing_defn, ast)) {
      report_error(index_name_ast, "CQL0018: duplicate index name", index_name);
      record_error(index_name_ast);
      record_error(ast);
    }
    return;
  }

  if (adding_current_entity) {
    // deleted or no it goes in the main list
    add_item_to_list(&all_indices_list, ast);

    // and consume the name
    add_index(ast, index_name);

    // and record the annotation
    sem_record_annotation_from_vers_info(&vers_info);

    // add the index to the table it is on
    add_item_to_list(&table_ast->sem->index_list, ast);
  }
}

// Similar to other constraints, we don't actually do anything with this
// other than offer some validation.  Again we use the usual helpers
// for name lookup within the context of this one PK/AK
static void sem_unq_def(ast_node *table_ast, ast_node *def) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_table_stmt(table_ast));
  Contract(is_ast_unq_def(def));
  EXTRACT_NOTNULL(indexed_columns_conflict_clause, def->right);
  EXTRACT_NOTNULL(indexed_columns, indexed_columns_conflict_clause->left);

  if (def->left) {
    EXTRACT_STRING(name, def->left);
    if (symtab_find(table_items, name)) {
      report_error(def, "CQL0020: duplicate constraint name in table", name);
      record_error(table_ast);
      return;
    }
    symtab_add(table_items, name, def);
  }

  // check if def node is unique key compare to others in table_ast
  if (!is_unique_key_valid(table_ast, def)) {
    report_error(def, "CQL0269: at least part of this unique key is redundant with previous unique keys", NULL);
    record_error(table_ast);
    return;
  }

  // CONSTRAINT name UNIQUE [indexed_columns]
  // or UNIQUE [indexed_columns]
  if (!sem_validate_name_list(indexed_columns, table_ast->sem->jptr)) {
    record_error(table_ast);
    return;
  }
}

// If we are in strict mode, then we validate that each FK has some choice
// for on update and/or on delete.  The choice might still be "do nothing"
// but you can't just leave it blank.
static bool_t sem_validate_fk_flags(ast_node *def, int32_t flags) {
  if (enforcement.strict_fk_update) {
    if (0 == (flags & FK_ON_UPDATE)) {
      report_error(def, "CQL0237: strict FK validation requires that some ON UPDATE option be selected for every foreign key", NULL);
      record_error(def);
      return false;
    }
  }

  if (enforcement.strict_fk_delete) {
    if (0 == (flags & FK_ON_DELETE)) {
      report_error(def, "CQL0238: strict FK validation requires that some ON DELETE option be selected for every foreign key", NULL);
      record_error(def);
      return false;
    }
  }

  return true;
}

// Here we're going to find the "referenced" table in a foreign key reference from the current context.  That table
// has to exist and be appropriately visible.  However, it also has extra constraints because it is being used in the
// context of a foreign key.  Notably, if the referenced table is marked @recreate then it must be in the same @recreate
// group as the current table or it's an error because the referenced table might be recreated away leaving all the
// foreign key references in this table as orphans.
//
// So we check the following:
// If the referenced table is @recreate and any of the following:
//   * the referenced table is in no group, OR
//   * the containing table is not @recreate at all (non-recreate table can't reference @recreate tables at all), OR
//   * the containing table is in no @recreate group (it's @recreate but not in any group so they might not rev together), OR
//   * the recreate groups of the two tables are different (it's in an @recreate group but not same one so they my not rev together)
// Then the reference is not valid.
// Additionally:
//   * the referenced table must be created in an version that came before the referencing table
static ast_node *find_and_validate_referenced_table(CSTR table_name, ast_node *err_target, version_attrs_info *table_info) {

  // The previous schema might have different regions, @recreate groups and other things than the now current schema;
  // it was validated for self consistency when it was created so we don't need to re-check it now and we already validated
  // the now current schema for @recreate violations.   Validating the old against the new just causes spurious errors, so don't.

  // This code doesn't get invoked in those cases.
  Contract(!validating_previous_schema);

  // The table is referring to itself, the other checks are moot and the name is not yet registered as the table
  // is currently under construction.  It can't be the case that it is referring to a different recreate group
  // or a future version, because it is referring to itself.
  if (!Strcasecmp(table_name, current_table_name)) {
    Invariant(current_table_ast);
    return current_table_ast;
  }

  ast_node *ref_table_ast;

  if (table_info->delete_version > 0 || current_proc) {
    // Create table statements inside a proc are exempt from the extra checks. Those statements aren't just schema
    // declarations they are the ones creating the table, maybe to make things right in the context of schema upgrade
    // itself. These extra check just doesn't make sense there.

    // Deleted tables likewise, do not need to have FK's that make sense in the current schema

    ref_table_ast = find_table_or_view_even_deleted(table_name);

    if (!ref_table_ast) {
      report_error(err_target, "CQL0021: foreign key refers to non-existent table", table_name);
      record_error(err_target);
    }

    // this table is going away, so the fk checks are moot
    return ref_table_ast;
  }

  ref_table_ast = find_usable_and_not_deleted_table_or_view(
    table_name,
    err_target,
    "CQL0021: foreign key refers to non-existent table");

  if (!ref_table_ast) {
    return NULL;
  }

  // We have to make sure we aren't referencing the future.
  //   * Recreate tables can see any version they like, if the name is in scope that's good enough
  //     which has already been verified.
  //   * Other tables may only "see" the same version or an earlier
  //     version.
  // Normal processing can't actually get into this state because if you tried to create the referencing
  // table with the smaller version number first you would get errors because the name of the referenced
  // table doesn't yet exist.  But if you created them at the same time and you made a typo in the version
  // number of the referenced table such that it was accidentally bigger you'd create a weirdness.
  // So we check for that situation here and reject it to prevent that sort of typo.
  if (!table_info->recreate) {
    int32_t ref_create_verison = ref_table_ast->sem->create_version;
    int32_t cur_create_version = table_info->create_version;

    if (ref_create_verison > 0 && ref_create_verison > cur_create_version) {
      report_error(err_target, "CQL0324: referenced table was created in a later version so it cannot be used in a foreign key", table_name);
      record_error(err_target);
      return NULL;
    }
  }

  // If the referenced table is @recreate then only @recreate tables in the same group can use it as an FK.
  // this is important because the @recreate table might change arbitrarily and anything not in its @recreate
  // group won't be updated at the same time.  This means @create tables can never reference @recreate tables
  // because those tables aren't as "stable".
  if (ref_table_ast->sem->recreate) {
    if (!ref_table_ast->sem->recreate_group_name ||
        !table_info->recreate ||
        !table_info->recreate_group_name ||
        Strcasecmp(table_info->recreate_group_name, ref_table_ast->sem->recreate_group_name)) {
    report_error(err_target, "CQL0060: referenced table can be independently recreated so it cannot be used in a foreign key", table_name);
    record_error(err_target);
    return NULL;
    }
  }

  return ref_table_ast;
}

// find_referenceable_columns's callback. It return true if name_list includes
// a specific column name.
// This is used in autotest(dummy_test) to figure out if a column should have
// an explicit value to avoid sql foreign key violation
static bool_t validate_referenceable_column_callback(ast_node *indexed_columns, void *context) {
  Contract(is_ast_indexed_columns(indexed_columns));
  CSTR column_name = (CSTR)context;

  for (; indexed_columns; indexed_columns = indexed_columns->right) {
    Invariant(is_ast_indexed_columns(indexed_columns));

    EXTRACT_NOTNULL(indexed_column, indexed_columns->left);
    ast_node *name_ast = indexed_column->left;

    // if this is an expression that is other than a simple name, it can't match any identifier
    // auto test will have no way of meeting this constraint automatically
    if (is_ast_str(name_ast)) {
      EXTRACT_STRING(name, name_ast);
      if (!Strcasecmp(column_name, name)) {
        return true;
      }
    }
  }
  return false;
}

// Check if a column is a primary or unique key
static bool_t is_column_unique_key(ast_node *ref_table_ast, CSTR column_name) {
  sem_struct *sptr = ref_table_ast->sem->sptr;
  for (int32_t i = 0; i < sptr->count; i++) {
    if (!Strcasecmp(column_name, sptr->names[i]) &&
        (is_primary_key(sptr->semtypes[i]) || is_unique_key(sptr->semtypes[i]))) {
      return true;
    }
  }
  return false;
}

// find_referenceable_colunns's callback
typedef bool_t (*validate_referenceable_columns_callback)(ast_node *name_list, void *context);

// Walkthrough create table node for table "table_name" and/or all the create
// index node to find :
//   - CONSTRAINT UNIQUE ([name_list]) statement
//   - CREATE INDEX name ON name([name_list]) statement
// The found nodes are passed to the callback to do validation. As soon as the
// callback return true the walkthrough stop otherwise it continues.
static bool_t find_referenceable_columns(
  ast_node *ref_table_ast,
  validate_referenceable_columns_callback callback,
  void *context
) {
  Contract(is_ast_create_table_stmt(ref_table_ast));

  EXTRACT_NOTNULL(create_table_name_flags, ref_table_ast->left);
  EXTRACT_STRING(ref_table_name, create_table_name_flags->right);

  EXTRACT_NOTNULL(col_key_list, ref_table_ast->right);
  for (; col_key_list; col_key_list = col_key_list->right) {
    EXTRACT_ANY_NOTNULL(col_def, col_key_list->left);
    // check if all column are in PRIMARY KEY ([name_list]) statement
    if (is_ast_pk_def(col_def)) {
      EXTRACT_NOTNULL(indexed_columns_conflict_clause, col_def->right);
      EXTRACT_NAMED_NOTNULL(indexed_columns2, indexed_columns, indexed_columns_conflict_clause->left);
      if (callback(indexed_columns2, context)) {
        return true;
      }
    }
    // check if all column are in CONSTRAINT UNIQUE ([name_list]) statement
    else if (is_ast_unq_def(col_def)) {
      EXTRACT_NOTNULL(indexed_columns_conflict_clause, col_def->right);
      EXTRACT_NAMED_NOTNULL(indexed_columns2, indexed_columns, indexed_columns_conflict_clause->left);
      if (callback(indexed_columns2, context)) {
        return true;
      }
    }
  }

  // check if all column are in CREATE UNIQUE INDEX statement
  for (int32_t i = 0; i < indices->capacity; i++) {
    symtab_entry entry = indices->payload[i];
    if (entry.sym) {
      ast_node *index_ast = (ast_node *)entry.val;

      Contract(is_ast_create_index_stmt(index_ast));
      EXTRACT_NOTNULL(create_index_on_list, index_ast->left);
      EXTRACT_NOTNULL(flags_names_attrs, index_ast->right);
      EXTRACT_NOTNULL(connector, flags_names_attrs->right);
      EXTRACT_NOTNULL(index_names_and_attrs, connector->left);
      EXTRACT_OPTION(flags, flags_names_attrs->left);
      EXTRACT_NOTNULL(indexed_columns, index_names_and_attrs->left);
      EXTRACT(opt_where, index_names_and_attrs->right);
      EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
      EXTRACT_STRING(index_name, index_name_ast);
      EXTRACT_ANY_NOTNULL(table_name_ast, create_index_on_list->right);
      EXTRACT_STRING(table_name, table_name_ast);

      if (!(flags & INDEX_UNIQUE)) {
        continue;
      }

      if (Strcasecmp(ref_table_name, table_name)) {
        continue;
      }

      if (callback(indexed_columns, context)) {
        return true;
      }
    }
  }

  return false;
}

// Check whether or not a column in a table is referenceable by other table in
// foreign key statement.
// This is used in autotest(dummy_test) to figure out which columns needs to have
// explicite value in INSERT statement to avoid sql foreign key violations.
//
// A column is considered referenceable if column is :
//  - a primary e.g: create table t (a text primary key)
//  - unique key e.g: create table t (a text unique)
//  - a group of primary key e.g: create table t (a text, b text, primary key (a, b))
//  - listed in CONSTRAINT UNIQUE statement e.g: create table t (a text, constraint unique (a))
//  - listed in a CREATE UNIQUE INDEX statement e.g: create index unique on t(a)
cql_noexport bool_t is_referenceable_by_foreign_key(ast_node *ref_table_ast, CSTR column_name)
{
  return is_column_unique_key(ref_table_ast, column_name)
    || find_referenceable_columns(
      ref_table_ast,
      validate_referenceable_column_callback,
      (void *)column_name);
}

// find_referenceable_columns's callback. It returns true if both name lists
// have the same items (in any order). This is used to figure out a list of columns
// in a foreign key clause are referenceable.
static bool_t validate_referenceable_fk_def_callback(ast_node *name_list, void *context) {
  Contract(is_ast_name_list(context) || is_ast_indexed_columns(context));
  return is_name_list_equal(name_list, (ast_node *)context);
}

// Validate whether or not the columns referenced in the foreign key statement
// are referenceable.
// A set of columns are considered referenceable if they are :
//  - a primary e.g: create table t (a text primary key)
//  - unique key e.g: create table t (a text unique)
//  - a group of primary key e.g: create table t (a text, b text, primary key (a, b))
//  - listed in CONSTRAINT UNIQUE statement e.g: create table t (a text, constraint unique (a))
//  - listed in a CREATE UNIQUE INDEX statement e.g: create index unique on t(a)
static sem_t sem_validate_referenceable_fk_def(ast_node *ref_table_ast, ast_node *name_list) {
  Contract(is_ast_name_list(name_list));

  EXTRACT_NOTNULL(create_table_name_flags, ref_table_ast->left);
  EXTRACT_STRING(ref_table_name, create_table_name_flags->right);

  // If we only have only one column listed in name_list then we just
  // check if that column is a single primary or unique key in table
  if (!name_list->right) {
    EXTRACT_STRING(column_name, name_list->left);
    if (is_column_unique_key(ref_table_ast, column_name)) {
      return true;
    }
  }

  // otherwise we are going to check if all the column in name_list are
  // - a unique key (UNIQUE (...) OR UNIQUE CONSTRAINT (...))
  // - unique index (CREATE UNIQUE INDEX ...)
  // - a group of primary key (PRIMARY KEY (a,b,...)).
  bool_t valid = find_referenceable_columns(
    ref_table_ast,
    validate_referenceable_fk_def_callback,
    name_list);

  if (!valid) {
    EXTRACT_STRING(name, name_list->left);
    report_error(name_list, "CQL0272: columns referenced in the foreign key statement should match exactly a unique key in the parent table", ref_table_name);
  }
  return valid;
}

// Get the name string of the object, for a variety of objects (tables and views for now)
static CSTR sem_get_name(ast_node *ast) {
  CSTR name = NULL;

  if (is_ast_create_view_stmt(ast)) {
    EXTRACT_NOTNULL(view_and_attrs, ast->right);
    EXTRACT_NOTNULL(name_and_select, view_and_attrs->left);
    EXTRACT_STRING(view_name, name_and_select->left);
    name = view_name;
  }
  else if (is_ast_create_table_stmt(ast)) {
    EXTRACT_NOTNULL(create_table_name_flags, ast->left);
    EXTRACT_STRING(table_name, create_table_name_flags->right);
    name = table_name;
  }
  else if (is_ast_create_trigger_stmt(ast)) {
    EXTRACT_NOTNULL(trigger_body_vers, ast->right);
    EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
    EXTRACT_ANY_NOTNULL(trigger_name_ast, trigger_def->left);
    EXTRACT_STRING(trigger_name, trigger_name_ast);
    name = trigger_name;
  }

  Contract(name); // failure means an invalid type was provided
  return name;
}

// These symbol tables track ast dependencies by name
// This tells use which tables refer to which other tables by FK (both directions)
// And which views refer to which tables by name (both directions)
static void record_table_dependencies(ast_node *src_ast, ast_node *target_ast) {
  Contract(is_ast_create_table_stmt(target_ast) || is_ast_create_view_stmt(target_ast));

  // note this will verify that it is one of the known dependency types also
  CSTR src_name = sem_get_name(src_ast);
  CSTR target_name = sem_get_name(target_ast);

  symtab_append_bytes(ref_sources_for_target_table, target_name, &src_ast, sizeof(src_ast));
  symtab_append_bytes(ref_targets_for_source_table, src_name, &target_ast, sizeof(target_ast));
}

// Similar to other constraints, we don't actually do anything with this
// other than offer some validation.  Again we use the usual helpers
// for name lookup within the context of this one FK.  Note that
// the FK has to be queried against two tables to fully validate it.
static void sem_fk_def(ast_node *table_ast, ast_node *def, version_attrs_info *table_info) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_table_stmt(table_ast));
  Contract(is_ast_fk_def(def));
  EXTRACT_NOTNULL(fk_info, def->right);
  EXTRACT_NAMED_NOTNULL(src_list, name_list, fk_info->left);
  EXTRACT_NOTNULL(fk_target_options, fk_info->right);
  EXTRACT_NOTNULL(fk_target, fk_target_options->left);
  EXTRACT_OPTION(flags, fk_target_options->right);
  EXTRACT_STRING(ref_table_name, fk_target->left);
  EXTRACT_NAMED_NOTNULL(ref_list, name_list, fk_target->right);

  // FK's inside of a table declaration (i.e. outside of any proc) are totally ignored
  // in the context of a schema migration script.  This prevents us from
  // getting errors because the latest version of the table refers to tables or
  // columns that are not yet in existence in the version we are migrating.
  // FKs in tables created by your migration script are honored.
  // When schema_upgrade_version > 0 it means we are in a migration script looking
  // and we are looking at logical schema version from the past.
  if (schema_upgrade_version > 0 && !current_proc) {
    record_ok(def);
    return;
  }

  // If we're doing previous schema validation  we don't have to validate the columns at all.
  // The previous schema may have different regions and/or @recreate groups and this will
  // just lead to spurious errors.  The current schema was already checked for consistency
  // all we have to do is validate that the text of the columns didn't change and that
  // happens later.  Visibiliity rules are moot.
  if (validating_previous_schema) {
    record_ok(def);
    return;
  }

  // FOREIGN KEY ( [src_list] ) REFERENCES [table_name] ([ref_list])

  if (def->left) {
    EXTRACT_STRING(constraint_name, def->left);
    if (symtab_find(table_items, constraint_name)) {
      report_error(def, "CQL0020: duplicate constraint name in table", constraint_name);
      record_error(table_ast);
      return;
    }
    symtab_add(table_items, constraint_name, def);
  }

  if (!sem_validate_name_list(src_list, table_ast->sem->jptr)) {
    record_error(table_ast);
    return;
  }

  // Here we make sure that the target table is visible here, that it is
  // in a compatible recreate group, and it was created before the current
  // table, if appropriate.
  ast_node *ref_table_ast = find_and_validate_referenced_table(
    ref_table_name,
    def,
    table_info);
  if (!ref_table_ast) {
    record_error(table_ast);
    return;
  }

  if (!sem_validate_name_list(ref_list, ref_table_ast->sem->jptr)) {
    record_error(table_ast);
    return;
  }

  // Here, we check to make sure that the target of this FK is, in fact, a unique key
  // in the target table.
  if (!sem_validate_referenceable_fk_def(ref_table_ast, ref_list)) {
    record_error(table_ast);
    record_error(def);
    return;
  }

  sem_update_column_type(table_ast, src_list, SEM_TYPE_FK);

  for ( ; src_list && ref_list; src_list = src_list->right, ref_list = ref_list->right) {
    ast_node *key = src_list->left;
    ast_node *ref = ref_list->left;
    if (core_type_of(key->sem->sem_type) != core_type_of(ref->sem->sem_type)) {
      CSTR error_message = "CQL0022: exact type of both sides of a foreign key must match";
      report_sem_type_mismatch(
          key->sem->sem_type,
          ref->sem->sem_type,
          key,
          error_message,
          key->sem->name);
      record_error(table_ast);
      record_error(def);
      return;
    }
  }

  if (src_list || ref_list) {
    report_error(def, "CQL0023: number of columns on both sides of a foreign key must match", NULL);
    record_error(table_ast);
    record_error(def);
    return;
  }

  // flags are only checked if we are in the appropriate strict mode
  if (!sem_validate_fk_flags(def, flags)) {
    record_error(table_ast);
    return;
  }

  record_table_dependencies(table_ast, ref_table_ast);

  record_ok(def);
}

// Similar to other constraints, we don't actually do anything with this
// other than offer some validation.  Again we use the usual helpers
// for name lookup within the context of this one PK.
static void sem_pk_def(ast_node *table_ast, ast_node *def) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_table_stmt(table_ast));
  Contract(is_ast_pk_def(def));
  EXTRACT_NOTNULL(indexed_columns_conflict_clause, def->right);
  EXTRACT(indexed_columns, indexed_columns_conflict_clause->left);

  // PRIMARY KEY [indexed_columns]

  if (def->left) {
    EXTRACT_STRING(name, def->left);
    if (symtab_find(table_items, name)) {
      report_error(def, "CQL0020: duplicate constraint name in table", name);
      record_error(table_ast);
      return;
    }
    symtab_add(table_items, name, def);
  }

  if (!sem_validate_name_list(indexed_columns, table_ast->sem->jptr)) {
    record_error(table_ast);
    return;
  }

  // pk columns are all not null. These mutations are not visible elsewhere
  // because `sptr` and `jptr` are uniquely referenced at this point:
  // `sem_pk_def` is only called via `sem_constraints` which in turn is only
  // called from `sem_create_table_stmt` which allocates new values.
  sem_update_column_type(table_ast, indexed_columns, SEM_TYPE_NOTNULL);
}

// Currently the only known builtin migration proc are
//  * cql:from_recreate
//  * cql:module_must_not_be_deleted_see_docs_for_CQLmmmm
//
// If this is ever generalized something fancier might be needed here
// like a name table or something.  For now, keeping it simple.
static void sem_validate_builtin_migration_proc(ast_node *ast, uint32_t code, CSTR name) {
  bool_t is_from_recreate = !Strcasecmp(CQL_FROM_RECREATE, name);
  bool_t is_module_warn = !Strcasecmp(CQL_MODULE_WARN, name);

  if (!is_from_recreate && !is_module_warn) {
    report_error(ast, "CQL0379: unknown built-in migration procedure", name);
    record_error(ast);
    return;
  }

  if (is_from_recreate && code != SCHEMA_ANNOTATION_CREATE_TABLE) {
    report_error(ast, "CQL0378: built-in migration procedure not valid in this context", name);
    record_error(ast);
    return;
  }

  if (is_module_warn && code != SCHEMA_ANNOTATION_DELETE_TABLE) {
    report_error(ast, "CQL0378: built-in migration procedure not valid in this context", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
  return;
}

static bool_t sem_validate_version(uint32_t code, ast_node *ast, int32_t *version, CSTR *out_proc) {
  Contract(version);
  EXTRACT(version_annotation, ast->left);
  EXTRACT_OPTION(vers, version_annotation->left);

  *out_proc = NULL;

  if (vers < 1) {
    report_error(ast, "CQL0025: version number in annotation must be positive", NULL);
    record_error(ast);
    return false;
  }

  if (*version > 0) {
    report_error(ast, "CQL0026: duplicate version annotation", NULL);
    record_error(ast);
    return false;
  }

  if (version_annotation->right) {
    CSTR proc_name = NULL;

    if (is_ast_dot(version_annotation->right)) {
      EXTRACT_NOTNULL(dot, version_annotation->right);
      EXTRACT_STRING(lhs, dot->left);
      EXTRACT_STRING(rhs, dot->right);
      proc_name = dup_printf("%s:%s", lhs, rhs);
      sem_validate_builtin_migration_proc(dot, code, proc_name);
      if (is_error(dot)) {
        record_error(ast);
        return false;
      }
    }
    else {
      EXTRACT_STRING(name, version_annotation->right);
      proc_name = name;

      size_t len = strlen(name);
      if (len >= 4) {
        size_t offset = len - 4;
        if (!Strcasecmp(name + offset, "_crc")) {
          report_error(ast, "CQL0338: the name of a migration procedure may not end in '_crc'", name);
          record_error(ast);
          return false;
        }
      }

      if (!sem_create_migration_proc_prototype(ast, proc_name)) {
        record_error(ast);
        return false;
      }
    }

    *out_proc = proc_name;
  }

  if (validating_previous_schema) {
    // During previous schema validation we track the biggest schema version we've seen
    if (vers > max_previous_schema_version) {
      bool_t excluded = current_region && excluded_regions && symtab_find(excluded_regions, current_region);

      if (!excluded) {
        max_previous_schema_version = vers;
      }
    }
  }
  else {
    // In normal operation we just look for duplicate procs, note duplicate procs
    // are not a problem when validating against previous schema.
    if (version_annotation->right && !is_ast_dot(version_annotation->right)) {
      EXTRACT_STRING(name, version_annotation->right);
      if (!symtab_add(upgrade_procs, name, ast)) {
        report_error(version_annotation->right, "CQL0027: a procedure can appear in only one annotation", name);
        record_error(ast);
        return false;
      }
    }
  }

  *version = vers;

  return true;
}

// When we find @create, @delete or @recreate we have to record that we found such an annotation.
// Later, if/when we generate schema we will be able to walk through these in a suitable sort order
// and then emit the appropriate migrations.
static void record_schema_annotation(int32_t vers, ast_node *target_ast, CSTR target_name, uint32_t type, ast_node *def, ast_node *ast, int32_t ordinal) {
  Contract(target_ast);
  Contract(target_name);
  switch (type) {
    case SCHEMA_ANNOTATION_DELETE_INDEX:
    case SCHEMA_ANNOTATION_DELETE_VIEW:
    case SCHEMA_ANNOTATION_DELETE_TRIGGER:
      // For these items, there is no schema action needed so only record the annotation
      // if there is a migration proc.  Downstream code will Contract on this.
      if (!ast->right) {
        return;
      }
  }

  schema_annotation *note = bytebuf_alloc(schema_annotations, sizeof(*note));

  note->version = vers;
  note->annotation_type = type;
  note->column_ordinal = ordinal;
  note->target_name = target_name;
  note->target_ast = target_ast;
  note->column_ast = def;
  note->annotation_ast = ast;
}

static int32_t recreates;

// Recreate annotations get stored in a different stream, they are processed in order as well but
// they don't merge in with the others.  So we're building up two buffers.
static void record_recreate_annotation(ast_node *target_ast, CSTR target_name, CSTR group_name, ast_node *annotation) {
  recreate_annotation *note = bytebuf_alloc(recreate_annotations, sizeof(*note));

  note->target_name = target_name;
  note->target_ast = target_ast;
  note->annotation_ast = annotation;
  note->group_name = group_name;
  note->ordinal = recreates++;
}

// This applies the validation for a FK in the context of a column, so that
// single column is the FK to the outside reference.
static void sem_col_attrs_fk(ast_node *fk, ast_node *def, coldef_info *info) {
  Contract(is_ast_col_attrs_fk(fk));
  Contract(is_ast_col_def(def));
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  EXTRACT_NOTNULL(fk_target_options, fk->left);
  EXTRACT_NOTNULL(fk_target, fk_target_options->left);
  EXTRACT_STRING(table_name, fk_target->left);
  EXTRACT_NAMED_NOTNULL(ref_list, name_list, fk_target->right);

  // REFERENCES [table_name] ([ref_list]) options

  // FK's inside of a table declaration (i.e. outside of any proc) are totally ignored
  // in the context of a schema migration script.  This prevents us from
  // getting errors because the latest version of the table refers to tables or
  // columns that are not yet in existence in the version we are migrating.
  // FKs in tables created by your migration script are honored.
  // NOTE: schema migration script here means a migration proc is being defined here.
  // This is not the normal schema upgrader.  But migration procs by definition work
  // on past versions of the schema.  Sometimes the "--rt schema_upgrade" thing is
  // called the schema migration script but this is not that.  This is where
  // @SCHEMA_UPGRADE_VERSION has been specified so that we should pretend to be
  // at an older schema version because we are upgrading that version.
  if (schema_upgrade_version > 0 && !current_proc) {
    record_ok(fk);
    return;
  }

  // If we're doing previous schema validation  we don't have to validate the columns at all.
  // The previous schema may have different regions and/or @recreate groups and this will
  // just lead to spurious errors.  The current schema was already checked for consistency
  // all we have to do is validate that the text of the columns didn't change and that
  // happens later.  Visibiliity of the referenced table in the previous schema is moot.
  if (validating_previous_schema) {
    record_ok(fk);
    return;
  }

  ast_node *ref_table_ast = find_and_validate_referenced_table(
    table_name,
    def,
    info->table_info);
  if (!ref_table_ast) {
    record_error(fk);
    return;
  }

  pending_table_validation pending = {
    .ref_table_ast = ref_table_ast,
    .table_ast = info->table_info->target_ast,
    .def = def,
    .fk = fk
  };

  // If this is an FK from a table to itself then we have to defer this work because
  // the names and types of the columns are not yet computed. For simplicity we just
  // defer the work always.
  enqueue_pending_table_validation(&pending);

  // ok for now
  record_ok(fk);
}

// Now resume validation of the foreign key; Note that we never try to look up
// the name of the referenced table because the referenced table might be the
// same as the table that contains the foreign key, such as:
//    create table T(id primary key, id2 references T(id))
// In that case T is not yet in the symbol table, as validation is incomplete.
// That's ok, we know the node for the current table without having to look it up.
// Note: these validations run in the context of the table being validated before
// that table is accepted, not later.  We're still "in" the table, if you will.
void sem_validate_fk_attr(pending_table_validation *pending) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  ast_node *fk = pending->fk;
  ast_node *def = pending->def;
  ast_node *ref_table_ast = pending->ref_table_ast;
  ast_node *src_table_ast = pending->table_ast;

  Contract(is_ast_create_table_stmt(ref_table_ast));
  Contract(is_ast_create_table_stmt(src_table_ast));
  Contract(is_ast_col_attrs_fk(fk));
  Contract(is_ast_col_def(def));

  EXTRACT_NOTNULL(fk_target_options, fk->left);
  EXTRACT_NOTNULL(fk_target, fk_target_options->left);
  EXTRACT_OPTION(flags, fk_target_options->right);
  EXTRACT_NAMED_NOTNULL(ref_list, name_list, fk_target->right);

  if (!sem_validate_name_list(ref_list, ref_table_ast->sem->jptr)) {
    record_error(fk);
    return;
  }

  ast_node *ref = ref_list->left;
  if (ref_list->right || core_type_of(def->sem->sem_type) != core_type_of(ref->sem->sem_type)) {
    report_error(def, "CQL0028: FK reference must be exactly one column with the correct type", def->sem->name);
    record_error(fk);
    return;
  }

  // flags are only checked if we are in the appropriate strict mode
  if (!sem_validate_fk_flags(fk, flags)) {
    return;
  }

  // make sure this is a valid key in the target table, safe to do this now since we are deferred
  if (!sem_validate_referenceable_fk_def(ref_table_ast, ref_list)) {
    record_error(fk);
    return;
  }

  record_table_dependencies(src_table_ast, ref_table_ast);

  record_ok(fk);
}

// Parse out the column information for this column and add the necessary flags
// to the semantic type.  Note that we don't care about all of these flags.
static sem_t sem_col_attrs(ast_node *def, ast_node *head, coldef_info *info) {
  Contract(head);
  Contract(info);

  bool_t adding_current_entity = will_add_current_entity();

  sem_t flags = 0;
  // For semantic analysis we only care about a subset of the attributes
  for (ast_node *ast = head; ast; ast = ast->right) {
    sem_t new_flags = 0;
    if (is_ast_create_attr(ast)) {
      if (!sem_validate_version(SCHEMA_ANNOTATION_CREATE_COLUMN, ast, &info->create_version, &info->create_proc)) {
        record_error(head);
        return false;
      }
      if (adding_current_entity) {
        record_schema_annotation(info->create_version, info->table_info->target_ast, info->table_info->name,
                                 SCHEMA_ANNOTATION_CREATE_COLUMN, def, ast->left, info->column_ordinal);
      }
    }
    else if (is_ast_delete_attr(ast)) {
      if (!sem_validate_version(SCHEMA_ANNOTATION_DELETE_COLUMN, ast, &info->delete_version, &info->delete_proc)) {
        record_error(head);
        return false;
      }
      if (adding_current_entity) {
        record_schema_annotation(info->delete_version, info->table_info->target_ast, info->table_info->name,
                                 SCHEMA_ANNOTATION_DELETE_COLUMN, def, ast->left, info->column_ordinal);
      }
    }
    else if (is_ast_col_attrs_not_null(ast)) {
      // We need this so that we can avoid generating null checks.
      new_flags = SEM_TYPE_NOTNULL; // prevent two of the same
    }
    else if (is_ast_sensitive_attr(ast)) {
      new_flags = SEM_TYPE_SENSITIVE; // prevent two of the same
    }
    else if (is_ast_col_attrs_default(ast)) {
      // We need this flag so that we can validate INSERT statements with missing columns
      sem_expr(ast->left);
      ast_node *expr = ast->left; // expr might have been rewritten so we fetch it now
      if (is_error(expr)) {
        record_error(head);
        return false;
      }
      info->default_value = expr;

      new_flags = SEM_TYPE_HAS_DEFAULT;  // prevent two of the same
    }
    else if (is_ast_col_attrs_check(ast)) {
      // we can't check the expression until the table is defined and we know all the columns so wait...
      EXTRACT_ANY_NOTNULL(expr, ast->left)
      pending_table_validation pending = {
        .table_ast = info->table_info->target_ast,
        .def = ast,
        .check = expr,
      };

      enqueue_pending_table_validation(&pending);
      new_flags = SEM_TYPE_HAS_CHECK;   // prevent two of the same
    }
    else if (is_ast_col_attrs_collate(ast)) {
      // Nothing much can go wrong here, the grammar only allows an id and it can be any id
      // In principle only some ids are valid but we have no way of knowing which at compile time.
      // We could make you declare them all but that's for another time, if ever.
      // All we're left with is make sure the column is text.  You could try to collate blobs but that
      // seems like a really bad idea so we're taking a stand on that.  This could be relaxed later if
      // it proves to be a mistake.

      sem_t core_type = core_type_of(info->col_sem_type);
      if (core_type != SEM_TYPE_TEXT) {
        report_error(ast->left, "CQL0348: collate applied to a non-text column", info->col_name);
        record_error(head);
        return false;
      }

      new_flags = SEM_TYPE_HAS_COLLATE;   // prevent two of the same
    }
    else if (is_ast_col_attrs_pk(ast)) {
      // sqlite defines all pk columns to be not null
      new_flags = SEM_TYPE_PK;
      info->primary_keys++;
      EXTRACT_NOTNULL(autoinc_and_conflict_clause, ast->left);
      EXTRACT(col_attrs_autoinc, autoinc_and_conflict_clause->left);
      if (col_attrs_autoinc) {
        // We need this so that we can validate INSERT statements
        // this column must be absent in an INSERT.
        new_flags |= SEM_TYPE_AUTOINCREMENT;
        info->autoinc_columns++;

        sem_t core_type = core_type_of(info->col_sem_type);

        if (core_type != SEM_TYPE_INTEGER && core_type != SEM_TYPE_LONG_INTEGER) {
          report_error(ast->left, "CQL0029: autoincrement column must be [LONG_]INTEGER PRIMARY KEY", info->col_name);
          record_error(head);
          return false;
        }
      }
    }
    else if (is_ast_col_attrs_fk(ast)) {
      sem_col_attrs_fk(ast, def, info);
      if (is_error(ast)) {
        record_error(head);
        return false;
      }
      new_flags = SEM_TYPE_FK; // prevent two of the same
    }
    else if (is_ast_col_attrs_hidden(ast)) {
      // NOTE: SEM_TYPE_VIRTUAL is not yet computed so we can't use that here, later this is easier

      ast_node *table_ast = info->table_info->target_ast;
      bool_t is_virtual_table = table_ast->parent && is_ast_create_virtual_table_stmt(table_ast->parent);

      // ignored for non-virtual tables SQLite does the same e.g:
      // > create table foo(x integer hidden, y integer); insert into foo(x,y) values(1,2); select * from foo;
      // 1|2

      if (flags) {
        report_error(ast, "CQL0362: HIDDEN column attribute must be the first attribute if present", NULL);
        record_error(head);
        return false;
      }

      if (is_virtual_table) {
        new_flags = SEM_TYPE_HIDDEN_COL;
      }
    }
    else {
      // this is all that's left
      Contract(is_ast_col_attrs_unique(ast));
      // while it's not normal, it is possible for exactly one row to be NULL
      // so this attribute doesn't affect nullability
      new_flags = SEM_TYPE_UK; // prevent two of the same
    }

    if (flags & new_flags) {
      report_error(ast, "CQL0030: a column attribute was specified twice on the same column", info->col_name);
      record_error(head);
      return false;
    }

    flags |= new_flags;
  }

  // these flags imply not null, add that after duplicate checking is done
  if (flags & (SEM_TYPE_PK | SEM_TYPE_AUTOINCREMENT)) {
    flags |= SEM_TYPE_NOTNULL;
  }

  Invariant(schema_upgrade_version != 0);  // -1 or positive

  if (schema_upgrade_script) {
    // no deleted columns processing, keep it all..
  }
  else if (schema_upgrade_version < 0) {
    if (info->delete_version > 0) {
      flags |= SEM_TYPE_DELETED;
    }
  }
  else {
    // The delete version is the version that the column was deleted in.
    // If we are migrating beyond that, the column is already deleted.
    // if were on that version (in a migration context) then you're allowed
    // to look at that column so that you can zero it or some such.
    if (info->delete_version > 0 && schema_upgrade_version > info->delete_version) {
      flags |= SEM_TYPE_DELETED;
    }

    // The create version ist he version that the column was created in.
    // If we are migrating to a schema before the column was created then we
    // cannot see it yet.
    if (info->create_version > 0 && schema_upgrade_version < info->create_version) {
      flags |= SEM_TYPE_DELETED;
    }
  }

  record_ok(head);
  return flags;
}

// Parse out a column definition, creating the necessary semantic type
// note that we need to carry some state here to do the validation.  We
// track the number of auto-increment columns we've seen so far and
// complain if we see more than one.
static void sem_col_def(ast_node *def, coldef_info *info) {
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);

  // We rewrite col_def_type_attrs node before reading the subtree
  // to make sure we read a rewrite subtree.
  rewrite_right_col_def_type_attrs_if_needed(col_def_type_attrs);
  if (is_error(col_def_type_attrs)) {
    record_error(def);
    return;
  }

  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(data_type, col_def_name_type->right);

  info->default_value = NULL;
  info->create_version = -1;
  info->delete_version = -1;
  info->column_ordinal++;

  // column name:  [name]
  sem_data_type_column(data_type);

  def->sem = new_sem(data_type->sem->sem_type);
  def->sem->name = name;
  def->sem->kind = data_type->sem->kind;

  info->col_sem_type = def->sem->sem_type;
  info->col_name = name;

  if (attrs) {
    sem_add_flags(def, sem_col_attrs(def, attrs, info));
    if (is_error(attrs)) {
      record_error(def);
      return;
    }

    // check type compat of the default value if there is one now that flags are all processed

    ast_node *expr = info->default_value;
    if (expr) {
      sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);

     // there is a common pattern TEXT DEFAULT 0 which is ok because the 0 converts to text
     // so we'll allow any literal to be used for text
     if (!is_text(def->sem->sem_type)) {
       // otherwise normal assignment rules
       if (!sem_verify_assignment(expr, def->sem->sem_type, expr->sem->sem_type, "default value")) {
         record_error(def);
         return;
       }
     }
    }
  }

  if (is_primary_key(def->sem->sem_type) && is_unique_key(def->sem->sem_type)) {
    report_error(def, "CQL0031: column can't be primary key and also unique key", name);
    record_error(def);
    return;
  }

  // all the columns with a create annotation have to be at the end and in order
  if (info->create_version < info->previous_create_version) {
    report_error(def, "CQL0032: created columns must be at the end and must be in version order", name);
    record_error(def);
    return;
  }

  info->previous_create_version = info->create_version;

  if (info->delete_version > 0 || info->create_version > 0) {
    if (info->table_info->recreate) {
      report_error(def, "CQL0033: columns in a table marked @recreate cannot have @create or @delete", name);
      record_error(def);
      return;
    }

    if (!is_nullable(def->sem->sem_type) && !has_default((def->sem->sem_type))) {
      report_error(def, "CQL0034: create/delete version numbers can only be applied to "
                        "columns that are nullable or have a default value", name);
      record_error(def);
      return;
    }
  }

  // you can't delete a column before it's been created
  if (info->delete_version > 0 && info->delete_version <= info->create_version) {
    report_error(def, "CQL0035: column delete version can't be <= column create version", name);
    record_error(def);
    return;
  }

  // sanity check the column delete version against the table versions if they are present
  // note version -1 indicates version annotation not present.

  if (info->delete_version > 0) {
    if (info->table_info->create_version > 0 && info->delete_version <= info->table_info->create_version) {
      report_error(def, "CQL0036: column delete version can't be <= the table create version", name);
      record_error(def);
      return;
    }

    if (info->table_info->delete_version > 0 && info->delete_version >= info->table_info->delete_version) {
      report_error(def, "CQL0037: column delete version can't be >= the table delete version", name);
      record_error(def);
    }
  }

  // sanity check the column create version against the table versions if they are present
  // note version -1 indicates version annotation not present.

  if (info->create_version > 0) {
    if (info->table_info->create_version > 0 && info->create_version <= info->table_info->create_version) {
      report_error(def, "CQL0038: column create version can't be <= the table create version", name);
      record_error(def);
      return;
    }

    if (info->table_info->delete_version > 0 && info->create_version >= info->table_info->delete_version) {
      report_error(def, "CQL0039: column create version can't be >= the table delete version", name);
      record_error(def);
      return;
    }
  }

  if (info->autoinc_columns > 1) {
    report_error(name_ast, "CQL0040: table can only have one autoinc column", name);
    record_error(def);
    return;
  }

  if (is_object(def->sem->sem_type)) {
    report_error(name_ast, "CQL0041: tables cannot have object columns", name);
    record_error(def);
    return;
  }

  // record the version info in the semantic type
  def->sem->create_version = info->create_version;
  def->sem->delete_version = info->delete_version;
}

// Queue a pending check valiation, this is just like the columns case
// we could do this right away because constraints come after columns
// but we may as well just do the checks all the same.
static void sem_check_def(ast_node *table_ast, ast_node *def) {
  EXTRACT_ANY_NOTNULL(expr, def->right)
  pending_table_validation pending = {
      .table_ast = table_ast,
      .def = def,
      .check = expr,
   };

  enqueue_pending_table_validation(&pending);
  record_ok(def);
}

// Dispatch the correct constraint type.  Release the saved table items
// (used to find duplicates) when done.  This is always clean on entry
// because this can't nest.
static void sem_constraints(ast_node *table_ast, ast_node *col_key_list, coldef_info *info) {
  Contract(is_ast_col_key_list(col_key_list));
  Invariant(!current_joinscope && !table_items);
  table_items = symtab_new();

  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    // stop if we have already found an error, the semantic info will be clobbered
    // by the error node so it's not safe to proceed
    if (is_error(table_ast)) {
      break;
    }

    if (is_ast_col_def(def)) {
      // column defs already processed
    } else if (is_ast_pk_def(def)) {
      sem_pk_def(table_ast, def);
      info->primary_keys++;
    } else if (is_ast_fk_def(def)) {
      sem_fk_def(table_ast, def, info->table_info);
    } else if (is_ast_check_def(def)) {
      sem_check_def(table_ast, def);
    } else {
      Contract(is_ast_unq_def(def));
      sem_unq_def(table_ast, def);
    }
  }

  symtab_delete(table_items);
  table_items = NULL;
}

// All the binary ops do the same preparation, they evaluate the left and the
// right expression, then they check those for errors.  Then they need
// the types of those expressions and the combined_flags of the result.  This
// does exactly that for its various callers.  Returns true if all is well.
static bool_t sem_binary_prep(ast_node *ast, sem_t *core_type_left, sem_t *core_type_right, sem_t *combined_flags) {
  EXTRACT_ANY_NOTNULL(left, ast->left);
  EXTRACT_ANY_NOTNULL(right, ast->right);

  // left op right
  sem_expr(left);
  sem_expr(right);

  if (is_error(left) || is_error(right)) {
    record_error(ast);
    *core_type_left = SEM_TYPE_ERROR;
    *core_type_right = SEM_TYPE_ERROR;
    *combined_flags = 0;
    return false;
  }

  *core_type_left = core_type_of(left->sem->sem_type);
  *core_type_right = core_type_of(right->sem->sem_type);
  *combined_flags = combine_flags(left->sem->sem_type, right->sem->sem_type);

  Invariant(is_unitary(*core_type_left));
  Invariant(is_unitary(*core_type_right));

  return true;
}

// Validates string compatible left and right and computes the result type.
// Works for like and not like, and helper for match, glob, and regexp.
static void sem_binary_like(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (!is_string_compat(core_type_left)) {
    report_error(ast->left, "CQL0042: left operand must be a string in", op);
    record_error(ast);
    return;
  }

  if (!is_string_compat(core_type_right)) {
    report_error(ast->right, "CQL0043: right operand must be a string in", op);
    record_error(ast);
    return;
  }

  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
}

// validates the left arg of collate, the right arg can be any id
static void sem_collate(ast_node *ast, CSTR op) {
  Contract(is_ast_collate(ast));
  Contract(is_ast_str(ast->right));
  EXTRACT_ANY_NOTNULL(left, ast->left);

  // [left] COLLATE name
  sem_expr(left);

  if (is_error(left)) {
    record_error(ast);
    return;
  }

  if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
    report_error(ast, "CQL0241: COLLATE may only appear in the context of a SQL statement", NULL);
    record_error(ast);
    return;
  }

  ast->sem =left->sem;
}

// Validates string/number compatible left and right and the result type should always be string
static void sem_concat(ast_node *ast, CSTR op) {
  Contract(is_ast_concat(ast));
  EXTRACT_ANY_NOTNULL(left, ast->left);
  EXTRACT_ANY_NOTNULL(right, ast->right);

  // [ast->left] || [ast->right];
  sem_expr(left);
  sem_expr(right);

  if (is_error(left) || is_error(right)) {
    record_error(ast);
    return;
  }

  if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
    report_error(ast, "CQL0241: CONCAT may only appear in the context of a SQL statement", NULL);
    record_error(ast);
    return;
  }

  sem_t sem_type_left = left->sem->sem_type;
  sem_t sem_type_right = right->sem->sem_type;
  sem_t combined_flags = combine_flags(left->sem->sem_type, right->sem->sem_type);

  if (is_blob(sem_type_left) || is_blob(sem_type_right)) {
    ast_node *operand = is_blob(sem_type_left) ? left : right;
    report_error(operand, "CQL0243: blob operand must be converted to string first in", op);
    record_error(ast);
    return;
  }

  // There's nothing left but these types, all else is excluded because of SQL expression context
  // and explicit blob reject.
  Invariant(is_string_compat(sem_type_left) || is_numeric_compat(sem_type_left));
  Invariant(is_string_compat(sem_type_right) || is_numeric_compat(sem_type_right));

  ast->sem = new_sem(SEM_TYPE_TEXT | combined_flags);
}

// match/glob/regexp are just like 'like' but it can only appear inside of SQL
static void sem_binary_match(ast_node *ast, CSTR op) {
  if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
    report_error(ast, "CQL0044: operator may only appear in the context of a SQL statement", op);
    record_error(ast);
    return;
  }
  sem_binary_like(ast, op);
}

// For all math operations, we combine the types and yield the type that
// holds both using the helper.  If any text, that's an error.
static void sem_binary_math(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (error_any_object(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (error_any_blob_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (error_any_text_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  sem_t core_type = sem_combine_types(core_type_left, core_type_right);

  // all math operations combine to at least integers (e.g. bool + bool = integer)
  if (core_type == SEM_TYPE_BOOL) {
    core_type = SEM_TYPE_INTEGER;
  }

  CSTR kind = sem_combine_kinds(ast->right, ast->left->sem->kind);
  if (is_error(ast->right)) {
    record_error(ast);
    return;
  }

  ast->sem = new_sem(core_type | combined_flags);
  ast->sem->kind = kind;
}

// For all math operations, we combine the types and yield the type that
// holds both using the helper.  If any text, that's an error.
static void sem_binary_integer_math(ast_node *ast, CSTR op) {
  sem_binary_math(ast, op);
  sem_reject_real(ast, op);
}

// For all the logical operands, the result is always a boolean.  Again
// text type inputs result in an error.
static void sem_binary_logical(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (error_any_text_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
}

static void sem_binary_eq_or_ne(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;

  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (is_ast_null(ast->left) || is_ast_null(ast->right)) {
    report_error(ast, "CQL0373: comparing against NULL always yields NULL; use IS and IS NOT instead", NULL);
    record_error(ast);
    return;
  }

  if (!sem_verify_compat(ast, core_type_left, core_type_right, op)) {
    return;
  }

  sem_combine_kinds(ast->right, ast->left->sem->kind);
  if (is_error(ast->right)) {
    record_error(ast);
    return;
  }

  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
  // the result is a normal bool, not a bool of any particular kind
}

// The comparison types always return a boolean and can accept anything
// that is compatible on the left or the right.
static void sem_binary_compare(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (error_any_object(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (error_any_blob_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (!sem_verify_compat(ast, core_type_left, core_type_right, op)) {
    return;
  }

  sem_combine_kinds(ast->right, ast->left->sem->kind);
  if (is_error(ast->right)) {
    record_error(ast);
    return;
  }

  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
  // the result is a normal bool, not a bool of any particular kind
}

// Any const node is evaluated at compile time.  The kinds of sub-expressions
// that are allowed are limited.  See the "eval" function for more on this.
static void sem_expr_const(ast_node *ast, CSTR op) {
  Contract(is_ast_const(ast));

  sem_root_expr(ast->left, SEM_EXPR_CONTEXT_NONE);
  if (is_error(ast->left)) {
    record_error(ast);
    return;
  }

  eval_node result = EVAL_NIL;
  eval(ast->left, &result);

  if (result.sem_type == SEM_TYPE_ERROR) {
    report_error(ast, "CQL0353: evaluation of constant failed", NULL);
    record_error(ast);
    return;
  }

  ast_node *ast_new = eval_set(ast, &result);
  sem_root_expr(ast_new, SEM_EXPR_CONTEXT_NONE);
  ast->sem = ast_new->sem;
}

// The unary operators all have a similar prep to the binary.  We need
// to visit the left side (it's always the left node even if the operator goes on the right)
// if that's ok then we need the combined_flags and core type.  There is only
// the one.  Returns true if everything is ok.
static bool_t sem_unary_prep(ast_node *ast, sem_t *core_type, sem_t *combined_flags) {
  // op left | left op
  sem_expr(ast->left);

  if (is_error(ast->left)) {
    *core_type = SEM_TYPE_ERROR;
    *combined_flags = 0;
    record_error(ast);
    return false;
  }

  sem_node *sem = ast->left->sem;
  sem_t sem_type = sem->sem_type;

  *core_type = core_type_of(sem_type);
  *combined_flags = not_nullable_flag(sem_type) | sensitive_flag(sem_type);

  Invariant(is_unitary(*core_type));
  return true;
}

static bool_t sem_validate_numeric(ast_node *ast, sem_t core_type, CSTR op) {
  if (is_blob(core_type)) {
    report_error(ast->left, "CQL0045: blob operand not allowed in", op);
    record_error(ast);
    return false;
  }

  if (is_object(core_type)) {
    report_error(ast->left, "CQL0046: object operand not allowed in", op);
    record_error(ast);
    return false;
  }

  if (is_text(core_type)) {
    report_error(ast->left, "CQL0047: string operand not allowed in", op);
    record_error(ast);
    return false;
  }

  return true;
}

// The only unary math operators are '-' and '~'
// Reference types are not allowed
static void sem_unary_math(ast_node *ast, CSTR op) {
  sem_t core_type, combined_flags;
  if (!sem_unary_prep(ast, &core_type, &combined_flags)) {
    return;
  }

  if (!sem_validate_numeric(ast, core_type, op)) {
    return;
  }

  // The result of unary math promotes to integer.  Basically this converts
  // bool to integer.  Long integer and Real stay as they are.  Text is
  // already ruled out.
  sem_t sem_type_result = sem_combine_types(
      (SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL),
      (core_type | combined_flags));

  ast->sem = new_sem(sem_type_result);
  ast->sem->kind = ast->left->sem->kind;

  // note ast->sem->name is NOT propagated because SQLite doesn't let you refer to
  // the column 'x' in 'select -x' -- the column name is actually '-x' which is useless
  // so we have no name once you apply unary math (unless you use 'as')
  // hence ast->sem->name = ast->left->sem->name is WRONG here and it is not missing on accident
}

static void sem_unary_integer_math(ast_node *ast, CSTR op) {
  sem_unary_math(ast, op);
  sem_reject_real(ast, op);
}

// The only logical unary operator is 'NOT' but there might be others some day.
// Text is not allowed.
static void sem_unary_logical(ast_node *ast, CSTR op) {
  sem_t core_type, combined_flags;
  if (!sem_unary_prep(ast, &core_type, &combined_flags)) {
    return;
  }

  if (!sem_validate_numeric(ast, core_type, op)) {
    return;
  }

  // For logical always returns a bool or null
  // the canonical example is NOT.
  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
}

// This is used for IS TRUE and IS FALSE
static void sem_unary_is_true_or_false(ast_node *ast, CSTR op) {
  if (enforcement.strict_is_true) {
    report_error(ast, "CQL0403: operator may not be used because it is not supported on old versions of SQLite", op);
    record_error(ast);
    return;
  }

  sem_t core_type, combined_flags;
  if (!sem_unary_prep(ast, &core_type, &combined_flags)) {
    return;
  }

  if (!sem_validate_numeric(ast, core_type, op)) {
    return;
  }

  // IS forms always return BOOL NOT NULL
  ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL | combined_flags);
}

// IS and IS NOT are special in that they return a not null boolean.
static void sem_binary_is_or_is_not(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;

  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (!sem_verify_compat(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (is_ast_null(ast->right) && is_not_nullable(ast->left->sem->sem_type)) {
    report_error(
      ast,
      "CQL0409: cannot use IS NULL or IS NOT NULL on a value of a NOT NULL type",
      expr_as_text(ast->left));
    record_error(ast);
    return;
  }

  // the result of is or is not is always a bool and never null
  ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL | sensitive_flag(combined_flags));
}

// Do analysis on an argument, notably * can only appear in count(*)
// so this is where that validation happens.  Otherwise recurse.
static void sem_arg_expr(ast_node *ast, bool_t is_count) {
  if (is_ast_star(ast)) {
    if (is_count) {
      ast->sem = new_sem(SEM_TYPE_INTEGER);
    }
    else {
      report_error(ast, "CQL0051: argument can only be used in count(*)", "*");
      record_error(ast);
    }
  }
  else {
    sem_expr(ast);
  }
}

// Walk an entire argument list and do the type inference on each argument.
// Not that this happens in the context of a function call and depending
// on what the function is, there may be rules for compatability of the
// arguments with the function and each other.  That doesn't happen here.
// This just gets the type of each arg and makes sure independently they are
// not bogus.
static void sem_arg_list(ast_node *head, bool_t is_count) {
  Contract(!head || is_ast_arg_list(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(arg, ast->left)
    sem_arg_expr(arg, is_count);
    if (is_error(arg)) {
      record_error(head);
      return;
    }
  }

  if (head) {
    record_ok(head);
  }
}

// Helper to get the first arg out of an arg list because we do that a lot.
cql_noexport ast_node *first_arg(ast_node *arg_list) {
  Contract(is_ast_arg_list(arg_list));
  EXTRACT_ANY_NOTNULL(arg, arg_list->left);

  return arg;
}

// Helper to get the second arg out of an arg list
cql_noexport ast_node *second_arg(ast_node *arg_list) {
  Contract(is_ast_arg_list(arg_list));
  EXTRACT_ANY_NOTNULL(arg, arg_list->right->left);

  return arg;
}

// Helper to get the third arg out of an arg list
cql_noexport ast_node *third_arg(ast_node *arg_list) {
  Contract(is_ast_arg_list(arg_list));
  EXTRACT_ANY_NOTNULL(arg, arg_list->right->right->left);

  return arg;
}

// Given `type`, return a new `sem_t` where `SEM_TYPE_NOTNULL_INFERRED` has been
// replaced with `SEM_TYPE_NOTNULL` if the former was present.
static sem_t type_with_finalized_nullability_improvement(sem_t type) {
  if (type & SEM_TYPE_INFERRED_NOTNULL) {
    // Upgrade the inferred nonnull type so it has a proper NOT NULL type.
    type |= SEM_TYPE_NOTNULL;
    // Prevent this from propagating needlessly to keep --ast clean.
    type &= u64_not(SEM_TYPE_INFERRED_NOTNULL);
  }

  return type;
}

// Select * is special in that it creates its own struct type by assembling
// all the columns of all the tables in the selects join result.  This does
// the work of assembling that struct.  Note the result of a select is a struct type.
// This means that join types only appear in the FROM part of DML
static void sem_select_star(ast_node *ast) {
  if (!current_joinscope || !current_joinscope->jptr) {
    report_error(ast, "CQL0052: select * cannot be used with no FROM clause", NULL);
    record_error(ast);
    return;
  }

  sem_join *jptr = current_joinscope->jptr;

  // First figure out how many fields there will be by visiting
  // every table in the join and summing the counts.
  uint32_t count = 0;
  for (int32_t i = 0; i < jptr->count; i++) {
    sem_struct *table = jptr->tables[i];
    count += table->count;
  }

  Invariant(count > 0);

  // Now collapse all the fields in all the tables into one table.
  sem_struct *sptr = new_sem_struct("select", count);
  int32_t field = 0;

  for (int32_t i = 0; i < jptr->count; i++) {
    sem_struct *table = jptr->tables[i];
    for (int32_t j = 0; j < table->count; j++, field++) {
      sptr->names[field] = table->names[j];
      // If we inferred a column in `table` to be nonnull, make it a proper
      // nonnull type in the result.
      sptr->semtypes[field] = type_with_finalized_nullability_improvement(table->semtypes[j]);
      sptr->kinds[field] = table->kinds[j];
    }
  }

  Invariant(field == count);

  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;

  // If the result has any un-named columns we can't use it.
  sem_verify_no_anon_columns(ast);
}

// When expanding select T.* we need to do two passes.  Our ultimate goal is to make
// a struct type for the select list of the select statement that has the T.*  but
// there could be any number of these expansions in it.  So in pass one we go through
// each item and count the room we will need for the expansions and the normal columns
// this will give us the total space required.  In pass two we will those in
// (see sem_select_table_star_add).  In pass 1 we do all the error checking so that
// by the time we're running pass 2 nothing can go wrong.
static uint32_t sem_select_table_star_count(ast_node *ast) {
  Contract(is_ast_table_star(ast));
  EXTRACT_STRING(name, ast->left);

  if (!current_joinscope || !current_joinscope->jptr) {
    report_error(ast, "CQL0053: select [table].* cannot be used with no FROM clause", NULL);
    record_error(ast);
    return false;
  }

  sem_join *jptr = current_joinscope->jptr;

  for (int32_t i = 0; i < jptr->count; i++) {
    if (!Strcasecmp(jptr->names[i], name)) {
      ast->sem = new_sem(SEM_TYPE_STRUCT);
      ast->sem->name = jptr->names[i];
      ast->sem->sptr = jptr->tables[i];

      // If the result has any un-named columns we can't use it.
      // We need a valid name for each column to expand it correctly.
      sem_verify_no_anon_columns(ast);
      if (is_error(ast)) {
        return 0;
      }

      return jptr->tables[i]->count;
    }
  }

  report_error(ast, "CQL0054: table not found", name);
  record_error(ast);
  return 0;
}

// Using the T in T.* from the ast, find the table in the current join that matches T
// then fill in the types and names from that table into the indicated result struct
// the result struct has already been allocated with enough room those the table at
// the indicated index.  We do this operation in two passes so we know how much to
// allocate and this is pass 2 where we fill in the values
static int32_t sem_select_table_star_add(ast_node *ast, sem_struct *sptr, int32_t index) {
  Contract(is_ast_table_star(ast));
  EXTRACT_STRING(name, ast->left);
  Invariant(current_joinscope);
  Invariant(current_joinscope->jptr);

  sem_join *jptr = current_joinscope->jptr;

  int32_t i = 0;
  while (Strcasecmp(jptr->names[i], name)) {
    i++;
  }

  // we found it once when we got the count, it's still there.
  Invariant(i < jptr->count);

  // we know there's room here, fill it in...
  sem_struct *table = jptr->tables[i];
  for (int32_t j = 0; j < table->count; j++) {
    sptr->names[index] = table->names[j];
    // If we inferred a column in `table` to be nonnull, make it a proper
    // nonnull type in the result.
    sptr->semtypes[index] = type_with_finalized_nullability_improvement(table->semtypes[j]);
    sptr->kinds[index] = table->kinds[j];
    index++;
  }

  return index;
}

// This verifies all the columns have a name.
// This check is important for a variety of cases, but the main ones are:
//  - select * or select T.*  -> we can't expand the start if there aren't names
//  - select results -> we can't make the getters if the columns don't have names
static void sem_verify_no_anon_columns(ast_node *ast) {
   // Sanity check our arguments, it is for sure a struct type.
  Invariant(is_struct(ast->sem->sem_type));
  sem_struct *sptr = ast->sem->sptr;
  uint32_t count = ast->sem->sptr->count;

  for (int32_t i = 0; i < count; i++) {
    const char *col = sptr->names[i];
    if (!strcmp(col, "_anon")) {
      report_error(ast, "CQL0055: all columns in the select must have a name", NULL);
      record_error(ast);
      return;
    }
  }
}

// ensure none of the columns have null type
static void sem_verify_no_null_columns(ast_node *ast) {
   // Sanity check our arguments, it is for sure a struct type.
  Invariant(is_struct(ast->sem->sem_type));
  sem_struct *sptr = ast->sem->sptr;
  uint32_t count = ast->sem->sptr->count;

  for (int32_t i = 0; i < count; i++) {
    if (is_null_type(sptr->semtypes[i])) {
      report_error(ast, "CQL0056: NULL expression has no type to imply the type of the select result", sptr->names[i]);
      record_error(ast);
    }
  }
}

// In various contexts we have to verify that the result of a select statement
// is well formed for re-use.  That means every column must have a name and a type
// This is so that we know, for instance, the name and type of every column in
// a result set from the select statement.
cql_noexport void sem_verify_no_anon_no_null_columns(ast_node *ast) {
  sem_verify_no_anon_columns(ast);
  if (!is_error(ast)) {
    sem_verify_no_null_columns(ast);
  }
  // if there is an error it will be on the ast on exit as is normal
}

static void sem_emit_one_sptr_type(charbuf *output, sem_struct *sptr, int32_t i) {
  bprintf(output, "%s ", sptr->names[i]);
  sem_t sem_type = sptr->semtypes[i];
  get_sem_core(sem_type, output);

  // the bits that matter
  sem_type &= SEM_TYPE_NOTNULL;
  get_sem_flags(sem_type, output);
}

// given that we found sptr differences in one of the places that the types must match
// we use this helper to tell the user what's different in detail
static void sem_emit_column_diff_diagnostics(ast_node *left, ast_node *right) {
  Invariant(is_struct(left->sem->sem_type));
  sem_struct *sptr_left = left->sem->sptr;
  Invariant(is_struct(right->sem->sem_type));
  sem_struct *sptr_right = right->sem->sptr;

  CHARBUF_OPEN(report);
  CHARBUF_OPEN(tmp);

  bprintf(&report, "additional difference diagnostic info:\n\n");
  bprintf(&report, "%s:%d:1: error: likely end location of the 1st item\n", left->filename, left->lineno);
  bprintf(&report, "  this item has %d columns\n", sptr_left->count);
  bprintf(&report, "%s:%d:1: error: likely end location of the 2nd item\n", right->filename, right->lineno);
  bprintf(&report, "  this item has %d columns\n", sptr_right->count);
  bprintf(&report, "\n");

  symtab *left_symbols = symtab_new();
  symtab *right_symbols = symtab_new();

  int32_t i;
  for (i = 0; i < sptr_left->count; i++) {
    bclear(&tmp);
    sem_emit_one_sptr_type(&tmp, sptr_left, i);
    if (!symtab_add(left_symbols, Strdup(tmp.ptr), NULL)) {
      bprintf(&report, "duplicate column in 1st: %s\n", tmp.ptr);
    }
  }

  for (i = 0; i < sptr_right->count; i++) {
    bclear(&tmp);
    sem_emit_one_sptr_type(&tmp, sptr_right, i);
    if (!symtab_add(right_symbols, Strdup(tmp.ptr), NULL)) {
      bprintf(&report, "duplicate column in 2nd: %s\n", tmp.ptr);
    }
  }

  for (i = 0; i < sptr_left->count; i++) {
    bclear(&tmp);
    sem_emit_one_sptr_type(&tmp, sptr_left, i);
    if (!symtab_find(right_symbols, tmp.ptr)) {
      bprintf(&report, "only in 1st: %s\n", tmp.ptr);
    }
  }

  for (i = 0; i < sptr_right->count; i++) {
    bclear(&tmp);
    sem_emit_one_sptr_type(&tmp, sptr_right, i);
    if (!symtab_find(left_symbols, tmp.ptr)) {
      bprintf(&report, "only in 2nd: %s\n", tmp.ptr);
    }
  }

  symtab_delete(left_symbols);
  symtab_delete(right_symbols);
  report_error(left, report.ptr, NULL);
  CHARBUF_CLOSE(tmp);
  CHARBUF_CLOSE(report);
}

static sem_struct *sem_unify_compatible_columns(ast_node *left, ast_node *right) {
  Invariant(is_struct(left->sem->sem_type));
  sem_struct *sptr_left = left->sem->sptr;
  Invariant(is_struct(right->sem->sem_type));
  sem_struct *sptr_right = right->sem->sptr;

  // Count, and names of columns must be an *exact* match.

  if (sptr_left->count != sptr_right->count) {
    report_error(left, "CQL0057: if multiple selects, all must have the same column count", NULL);
    sem_emit_column_diff_diagnostics(left, right);
    record_error(left);
    record_error(right);
    return NULL;
  }

  for (int32_t i = 0; i < sptr_left->count; i++) {
    const char *col1 = sptr_left->names[i];
    const char *col2 = sptr_right->names[i];

    Invariant(col1 && col2);
    if (strcmp(col1, col2)) {
      CSTR err_msg = dup_printf(
           "CQL0058: if multiple selects,"
           " all column names must be identical so they have unambiguous names; error"
           " in column %d: '%s' vs. '%s'", i+1, col1, col2);
      report_error(left, err_msg, NULL);
      sem_emit_column_diff_diagnostics(left, right);
      record_error(left);
      record_error(right);
      return NULL;
    }
  }

  // Column types must be compatible
  sem_struct *sptr = new_sem_struct("union", sptr_left->count);

  for (int32_t i = 0; i < sptr_left->count; i++) {
    sem_t sem_type_1 = sptr_left->semtypes[i];
    sem_t sem_type_2 = sptr_right->semtypes[i];
    const char *col = sptr_left->names[i];

    if (!sem_verify_compat(left, sem_type_1, sem_type_2, col)) {
      record_error(left);
      record_error(right);
      return NULL;
    }

    CSTR kind_1 = sptr_left->kinds[i];
    CSTR kind_2 = sptr_right->kinds[i];

    CSTR kind = sem_combine_kinds_general(left, kind_1, kind_2);
    if (is_error(left)) {
      record_error(left);
      record_error(right);
      return NULL;
    }

    sptr->semtypes[i] = sem_combine_types(sem_type_1, sem_type_2);
    sptr->names[i] = sptr_left->names[i];
    sptr->kinds[i] = kind;
  }

  return sptr;
}

cql_noexport void sem_verify_identical_columns(ast_node *expected, ast_node *actual, CSTR target) {
  Contract(actual);
  Contract(expected);
  sem_struct *sptr_expected = expected->sem->sptr;
  sem_struct *sptr_actual = actual->sem->sptr;
  Contract(sptr_expected);
  Contract(sptr_actual);

  // Count, type, and names of columns must be an *exact* match.

  if (sptr_expected->count != sptr_actual->count) {
    CSTR errmsg = dup_printf("CQL0057: %s, all must have the same column count", target);
    report_error(actual, errmsg, NULL);
    sem_emit_column_diff_diagnostics(expected, actual);
    record_error(actual);
    return;
  }

  for (int32_t i = 0; i < sptr_expected->count; i++) {
    sem_t sem_type_1 = sptr_expected->semtypes[i];
    sem_t sem_type_2 = sptr_actual->semtypes[i];
    const char *col1 = sptr_expected->names[i];
    const char *col2 = sptr_actual->names[i];

    if (strcmp(col1, col2)) {
      CSTR errmsg = dup_printf(
        "CQL0058: %s,"
        " all column names must be identical so they have unambiguous names; error"
        " in column %d: '%s' vs. '%s'", target, i+1, col1, col2);
      report_error(actual, errmsg, NULL);
      sem_emit_column_diff_diagnostics(expected, actual);
      record_error(actual);
      return;
    }

    if (core_type_of(sem_type_1) != core_type_of(sem_type_2)) {
      CSTR error_message = dup_printf("CQL0061: %s, all columns must be an exact type match", target);
      report_sem_type_mismatch(sem_type_1, sem_type_2, actual, error_message, col2);
      record_error(actual);
      return;
    }

    if (is_nullable(sem_type_1) != is_nullable(sem_type_2)) {
      CSTR error_message =
        dup_printf("CQL0062: %s, all columns must be "
        "an exact type match (including nullability)", target);
      report_sem_type_mismatch(
          sem_type_1, sem_type_2, actual, error_message, col2);
      record_error(actual);
      return;
    }
  }
}


// If a procedure is returning a select statement then we need to attach that
// type to the procedures semantic info.  We have to do some extra validation
// at this point, especially if the proc already has some other select return.
// This is where we make sure all the kinds of selects that might be returned
// are 100% compatible.
static void sem_update_proc_type_for_select(ast_node *ast) {
  bool_t is_out = is_ast_out_stmt(ast);
  bool_t is_out_union = is_ast_out_union_stmt(ast);
  bool_t is_select = is_select_stmt(ast);
  bool_t is_calling_out_union = false;

  if (is_ast_call_stmt(ast)) {
     // still nothing
      Invariant(!(is_out || is_out_union || is_select));

     // the type of result is based on the call type
     sem_t sem_call = ast->sem->sem_type;

     is_out = !!(sem_call & SEM_TYPE_USES_OUT);
     is_calling_out_union = !!(sem_call & SEM_TYPE_USES_OUT_UNION);
     is_select = !(is_calling_out_union || is_out);
  }

  Contract(is_out || is_out_union || is_select || is_calling_out_union);

  // Ignore 'select'/'call'/'out'/'out union' statement nodes inside explain
  // statement subtree. This method should be called once for explain statement
  // at the root node
  if (current_explain_stmt && !is_ast_explain_stmt(ast)) {
    // In this code only select stmt will be used inside explain stmt, let's make
    // sure it stays the same
    Contract(is_select_stmt(ast));
    return;
  }

  // We might get called after any select, if it's a loose select (no proc) or
  // if the current proc already has errors, or we're in a trigger, we can stop here...
  if (!current_proc || is_error(current_proc) || is_error(ast) || in_trigger) {
    return;
  }

  // Sanity check our arguments, it is for sure a select.
  Invariant(is_struct(ast->sem->sem_type));

  // Sanity check the state, the current proc is a proc.
  Invariant(is_proc(current_proc));
  EXTRACT_STRING(name, current_proc->left);

  // It's at least got an OK record
  Invariant(current_proc->sem);

  // If the select we were given has any un-named columns we can't use it.
  sem_verify_no_anon_no_null_columns(ast);
  if (is_error(ast)) {
    return;
  }

  // If we haven't seen any other result type, then we're good to go, use this one.
  if (core_type_of(current_proc->sem->sem_type) == SEM_TYPE_OK) {

    // start with the source of the data for the shape
    current_proc->sem = ast->sem;

    // strip the out/out union flag from the source of the select
    // instead use the correct flag for the current proc
    sem_t sem_type = current_proc->sem->sem_type;
    sem_type &= sem_not(SEM_TYPE_USES_OUT | SEM_TYPE_USES_OUT_UNION);

    // add back what we need
    if (is_out_union) {
      sem_type |= SEM_TYPE_USES_OUT_UNION;
    }

    if (is_calling_out_union) {
      sem_type |= SEM_TYPE_CALLS_OUT_UNION | SEM_TYPE_USES_OUT_UNION;
    }

    if (is_out) {
      sem_type |= SEM_TYPE_USES_OUT;
    }

    // this clones the sem entirely, replacing the flags
    sem_replace_flags(current_proc, sem_type);

    // what follows is a no-op but it double checks important invariants so just let it go
  }

  // This code is the only code that sets the sem type so it must be struct
  Invariant(is_struct(current_proc->sem->sem_type));

  // Now we do the hard work of verifying that this select is compatible with
  // the previous select.  We check pretty much everything.

  // Note: we are not ever going to change the type, either it matches or it's an error
  // the only time we change the type to one of these is in the above case where
  // the type was not yet set.

  bool_t did_out = has_out_stmt_result(current_proc);
  bool_t did_call_out_union = has_out_union_call(current_proc);
  bool_t did_out_union = has_out_union_stmt_result(current_proc) && !did_call_out_union;
  bool_t did_select = has_result_set(current_proc) && !did_call_out_union;

  Invariant(did_out + did_out_union + did_select + did_call_out_union == 1);

  if (is_out != did_out || is_out_union != did_out_union || is_select != did_select || is_calling_out_union != did_call_out_union) {
    report_error(ast, "CQL0063: can't mix and match out, out union, or select/call for return values", name);
    record_error(ast);
    return;
  }

  sem_verify_identical_columns(current_proc, ast, "in multiple select/out statements");
}

static ast_node *get_named_param(ast_node *params, CSTR name) {
  for (; params; params = params->right) {
    EXTRACT_NOTNULL(param, params->left);

    // args already evaluated and no errors
    Invariant(param->sem);

    if (!Strcasecmp(name, param->sem->name)) {
      return param;
    }
  }

  return NULL;
}

// Like `report_error`, but does nothing if `ast` is NULL. Used to make it
// easier to handle a NULL AST in the `sem_try_resolve_*` family of functions.
static void report_resolve_error(ast_node *ast, CSTR msg, CSTR subject) {
  if (ast) {
    report_error(ast, msg, subject);
  }
}

// Like `record_error`, but does nothing if `ast` is NULL. Used to make it
// easier to handle a NULL AST in the `sem_try_resolve_*` family of functions.
static void record_resolve_error(ast_node *ast) {
  if (ast) {
    record_error(ast);
  }
}

// All `sem_try_resolve_*` functions return either `SEM_RESOLVE_CONTINUE` to
// indicate that another resolver should be tried, or `SEM_RESOLVE_STOP` to
// indicate that the correct resolver was found. Continuing implies that no
// failure has (yet) occurred, but stopping implies neither success nor failure.
typedef enum {
  SEM_RESOLVE_CONTINUE = 0,
  SEM_RESOLVE_STOP = 1
} sem_resolve;

static sem_resolve sem_try_resolve_locals_bundle(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (!scope || Strcasecmp(scope, "LOCALS")) {
    return SEM_RESOLVE_CONTINUE;
  }

  symtab_entry *entry = symtab_find(locals, name);
  if (!entry) {
    CHARBUF_OPEN(tmp);
      bprintf(&tmp, "%s_", name);
      entry = symtab_find(locals, tmp.ptr);
    CHARBUF_CLOSE(tmp);

    if (!entry) {
      report_resolve_error(ast, "CQL0201: expanding FROM LOCALS, there is no local matching", name);
      record_resolve_error(ast);
      return SEM_RESOLVE_STOP;
    }
  }

  ast_node *var = (ast_node *)entry->val;

  if (ast) {
    ast->sem = var->sem;
  }

  *type_ptr = &var->sem->sem_type;

  return SEM_RESOLVE_STOP;
}

static sem_resolve sem_try_resolve_arguments_bundle(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (!scope || Strcasecmp(scope, "ARGUMENTS")) {
    return SEM_RESOLVE_CONTINUE;
  }

  Invariant(current_proc);

  ast_node *params = get_proc_params(current_proc);
  Invariant(params);

  // these are always synthetically generated so they are 100% sure to match
  ast_node *param = get_named_param(params, name);
  if (!param) {
    CHARBUF_OPEN(tmp);
      bprintf(&tmp, "%s_", name);
      param = get_named_param(params, tmp.ptr);
    CHARBUF_CLOSE(tmp);
  }

  if (param) {
    if (ast) {
      ast->sem = param->sem;
    }

    *type_ptr = &param->sem->sem_type;

    return SEM_RESOLVE_STOP;
  }

  report_resolve_error(ast, "CQL0201: expanding FROM ARGUMENTS, there is no argument matching", name);
  record_resolve_error(ast);
  return SEM_RESOLVE_STOP;
}

// This helper adds new variables global or local, it also tracks the unitary locals
// so that we can do the LOCALS shape
static void add_variable(CSTR name, ast_node *variable) {
  // We could be working on locals and not unitary_locals inside of proc declarations
  // so we have to check both things.
  if (current_variables == locals && unitary_locals) {
    if (is_unitary(variable->sem->sem_type)) {
      bytebuf_append(unitary_locals, &variable, sizeof(variable));
    }
  }

  symtab_add(current_variables, name, variable);
}

// Look for the given name as a local or global variable.  First local.
cql_noexport ast_node *find_local_or_global_variable(CSTR name) {
  // look in the two variable tables, in order, first match wins
  symtab_entry *entry = symtab_find(locals, name);

  if (!entry) {
    entry = symtab_find(globals, name);
  }

  return entry ? entry->val : NULL;
}

static sem_resolve sem_try_resolve_variable(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (scope) {
    return SEM_RESOLVE_CONTINUE;
  }

  ast_node *variable = find_local_or_global_variable(name);
  if (!variable) {
    return SEM_RESOLVE_CONTINUE;
  }

  sem_t sem_type = variable->sem->sem_type;

  if (is_object(sem_type) &&
      CURRENT_EXPR_CONTEXT_IS_NOT(SEM_EXPR_CONTEXT_NONE | SEM_EXPR_CONTEXT_TABLE_FUNC)) {
    report_resolve_error(ast, "CQL0064: object variables may not appear in the context"
                              " of a SQL statement (except table-valued functions)", name);
    record_resolve_error(ast);
    return SEM_RESOLVE_STOP;
  }

  if (ast) {
    ast->sem = new_sem(sem_type);
    ast->sem->name = variable->sem->name;
    ast->sem->kind = variable->sem->kind;
    // Needed for cursors.
    ast->sem->sptr = variable->sem->sptr;
  }

  *type_ptr = &variable->sem->sem_type;

  return SEM_RESOLVE_STOP;
}

// Returns true if okay, else false.
static bool_t sem_resolve_column_does_not_conflict_with_variable(ast_node *ast, CSTR column_name, CSTR table_name) {
  Contract(column_name);

  // It is an error if a column hides a local/global. This is only a problem if
  // the table is not scoped. The form T1.x is always the table column so it's
  // always safe. However T1.x == x will give an error if there is a local 'x'
  // because the 'x' could be either.
  if (!table_name && find_local_or_global_variable(column_name)) {
    report_resolve_error(ast, "CQL0059: a variable name might be ambiguous "
                              "with a column name, this is an anti-pattern", column_name);
    record_resolve_error(ast);
    return false;
  }

  return true;
}

// Given a column name and optional scope (table name), try to find it as one of
// the columns in the current join scope or else in one of the parent
// joinscopes. If the name is ambiguous at any given level then it is an error,
// but inner scopes are allowed to hide the names of outer scopes.
static sem_resolve sem_try_resolve_column(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  sem_t sem_type = 0;
  CSTR col = NULL;
  CSTR kind = NULL;
  sem_join *found_jptr = NULL;
  sem_t *type = NULL;

  // We walk the chain of scopes until we find a stop frame or else we run out
  // this allows nested joins to see their parent scopes.
  for (sem_joinscope *jscp = current_joinscope; jscp && jscp->jptr; jscp = jscp->parent) {
    sem_join *jptr = jscp->jptr;
    bool_t found_in_this_joinscope = false;
    for (int32_t i = 0; i < jptr->count; i++) {
      if (scope == NULL || !Strcasecmp(scope, jptr->names[i])) {
        sem_struct *table = jptr->tables[i];
        for (int32_t j = 0; j < table->count; j++) {
          if (!Strcasecmp(name, table->names[j])) {
            if (found_in_this_joinscope) {
              // Since we found two candidates in the same joinscope, we have an
              // ambiguity. It doesn't matter if we check for this before or
              // after we check for aliases as all aliases are in their own
              // joinscopes anyway.
              report_resolve_error(ast, "CQL0065: identifier is ambiguous", name);
              record_resolve_error(ast);
              return SEM_RESOLVE_STOP;
            }
            if (table->semtypes[j] & SEM_TYPE_ALIAS) {
              if (col) {
                // We already found a column, and that column was found in a
                // child joinscope. It must be the case that the found column
                // shadows an alias.
                report_resolve_error(
                  ast,
                  "CQL0435: must use qualified form to avoid ambiguity with alias",
                  name);
              } else {
                // An aliases is being referred to directly. Since we can only
                // have `SEM_TYPE_ALIAS` when analyzing one of the
                // below-mentioned clauses (for which referencing an alias is
                // not allowed), we have an error.
                report_resolve_error(
                  ast,
                  "CQL0436: alias referenced from WHERE, GROUP BY, HAVING, or WINDOW clause",
                  name);
              }
              record_resolve_error(ast);
              return SEM_RESOLVE_STOP;
            }
            if (col) {
              // We already have our result column, but we continue to search to
              // check for shadowed aliases.
              continue;
            }
            found_in_this_joinscope = true;
            sem_type = table->semtypes[j];
            col = table->names[j];
            kind = table->kinds[j];
            found_jptr = jptr;
            // Store this for setting type_ptr later, if successful.
            type = &table->semtypes[j];
          }
        }
      }
    }
  }

  if (!col) {
    return SEM_RESOLVE_CONTINUE;
  }

  if (!sem_resolve_column_does_not_conflict_with_variable(ast, name, scope)) {
    return SEM_RESOLVE_STOP;
  }

  if (ast) {
    ast->sem = new_sem(sem_type);
    ast->sem->name = col; // be sure to use the canonical name
    ast->sem->kind = kind; // use the kind if there is one
    if (found_jptr && found_jptr == monitor_jptr) {
      symtab_add(monitor_symtab, col, NULL);
    }
  }

  *type_ptr = type;

  return SEM_RESOLVE_STOP;
}

static sem_resolve sem_try_resolve_rowid(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  sem_t sem_type = 0;
  CSTR col = NULL;
  CSTR kind = NULL;

  if (!(current_joinscope && current_joinscope->jptr)) {
    return SEM_RESOLVE_CONTINUE;
  }

  // there are 3 valid names for the rowid, any will do.
  if (Strcasecmp(name, "_rowid_") && Strcasecmp(name, "rowid") && Strcasecmp(name, "oid")) {
    return SEM_RESOLVE_CONTINUE;
  }

  sem_join *jptr = current_joinscope->jptr;
  if (scope == NULL && jptr->count == 1) {
    // if only one table then that's the rowid
    col = name;
    sem_type = SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL;
  }
  else if (scope != NULL) {
    // more than one table but the name is scoped, still have a chance
    for (int32_t i = 0; i < jptr->count; i++) {
      if (!Strcasecmp(scope, jptr->names[i])) {
        col = name;
        kind = NULL;
        sem_type = SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL;
        break;
      }
    }
  }
  else {
    report_resolve_error(ast, "CQL0066: identifier is ambiguous", name);
    record_resolve_error(ast);
    return SEM_RESOLVE_STOP;
  }

  if (!col) {
    return SEM_RESOLVE_CONTINUE;
  }

  if (!sem_resolve_column_does_not_conflict_with_variable(ast, name, scope)) {
    return SEM_RESOLVE_STOP;
  }

  if (ast) {
    ast->sem = new_sem(sem_type);
    ast->sem->name = col;
    ast->sem->kind = kind;
  }

  return SEM_RESOLVE_STOP;
}

// Similar to `sem_try_resolve_column`, but used outside of the
// `sem_try_resolve_*` family of functions a way of looking up a column given
// only a column name. Intentionally allows situations in which the name of a
// column shadows a variable. Returns `true` if the column was found, else
// `false`. Returning `true` does not imply that semantic analysis was
// successful.
static bool_t sem_find_column_for_name(ast_node *ast, CSTR name) {
  Contract(ast);
  Contract(name);

  sem_t *type = NULL;

  // We want to bypass checking for conflicts with locals and globals.
  symtab *saved_locals = locals;
  symtab *saved_globals = globals;
  locals = NULL;
  globals = NULL;

  bool_t found = sem_try_resolve_column(ast, name, NULL, &type) == SEM_RESOLVE_STOP;
  if (!found) {
    found = sem_try_resolve_rowid(ast, name, NULL, &type) == SEM_RESOLVE_STOP;
  }

  locals = saved_locals;
  globals = saved_globals;

  return found;
}

static void sem_resolve_cursor_field(ast_node *ast, ast_node *cursor, CSTR field, sem_t **type_ptr) {
  Contract(cursor);
  Contract(field);
  Contract(type_ptr);

  sem_t sem_type = cursor->sem->sem_type;
  CSTR scope = cursor->sem->name;

  // We don't do this if the cursor was not used with the auto syntax
  if (!is_auto_cursor(sem_type)) {
    report_resolve_error(ast, "CQL0067: cursor was not used with 'fetch [cursor]'", scope);
    record_resolve_error(ast);
    return;
  }

  // Find the name if it exists;  emit the canonical field name, which
  // has the exact case from the declaration.  The user might have used
  // something like C.VaLuE when the field is "value". The local has to match.

  sem_struct *sptr = cursor->sem->sptr;
  Invariant(sptr->count > 0);

  for (int32_t i = 0; i < sptr->count; i++) {
    if (!Strcasecmp(sptr->names[i], field)) {
      if (ast) {
        ast->sem = new_sem(sptr->semtypes[i] | SEM_TYPE_VARIABLE);
        ast->sem->name = dup_printf("%s.%s", scope, sptr->names[i]);
        ast->sem->kind = sptr->kinds[i];
      }
      *type_ptr = &sptr->semtypes[i];
      return;
    }
  }

  report_resolve_error(ast, "CQL0068: field not found in cursor", field);
  record_resolve_error(ast);
}

static sem_resolve sem_try_resolve_cursor_field(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (!scope) {
    return SEM_RESOLVE_CONTINUE;
  }

  ast_node *variable = find_local_or_global_variable(scope);
  if (!variable || !is_cursor(variable->sem->sem_type)) {
    return SEM_RESOLVE_CONTINUE;
  }

  sem_resolve_cursor_field(ast, variable, name, type_ptr);
  return SEM_RESOLVE_STOP;
}

static sem_resolve sem_try_resolve_enum(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (!scope) {
    return SEM_RESOLVE_CONTINUE;
  }

  ast_node *enum_stmt = find_enum(scope);
  if (!enum_stmt) {
    return SEM_RESOLVE_CONTINUE;
  }

  Invariant(is_ast_declare_enum_stmt(enum_stmt));

  // Find the name if it exists;  if it does then this becomes a rewrite

  EXTRACT_NOTNULL(enum_values, enum_stmt->right);

  while (enum_values) {
    EXTRACT_NOTNULL(enum_value, enum_values->left);
    EXTRACT_STRING(enum_member, enum_value->left);

    if (!Strcasecmp(enum_member, name)) {
      if (ast) {
        ast_node *ast_new = eval_set(ast, enum_value->left->sem->value);
        sem_root_expr(ast_new, SEM_EXPR_CONTEXT_NONE);
        ast->sem = ast_new->sem;
        ast->sem->kind = enum_stmt->sem->kind;
      }
      return SEM_RESOLVE_STOP;
    }
    enum_values = enum_values->right;
  }

  report_resolve_error(ast, "CQL0357: enum does not contain", name);
  record_resolve_error(ast);

  return SEM_RESOLVE_STOP;
}

static sem_resolve sem_try_resolve_global_constant(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  if (scope) {
    return SEM_RESOLVE_CONTINUE;
  }

  ast_node *const_value = find_constant(name);
  if (!const_value) {
    return SEM_RESOLVE_CONTINUE;
  }

  if (ast) {
    if (is_numeric(const_value->sem->sem_type)) {
      ast_node *ast_new = eval_set(ast, const_value->left->sem->value);
      sem_root_expr(ast_new, SEM_EXPR_CONTEXT_NONE);
      ast->sem = ast_new->sem;
    }
    else {
      *ast = *const_value->right;
    }
  }

  return SEM_RESOLVE_STOP;
}

static sem_resolve sem_try_resolve_arg_bundle(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

   if (!scope) {
     return SEM_RESOLVE_CONTINUE;
   }

   ast_node *shape = find_arg_bundle(scope);
   if (!shape) {
     return SEM_RESOLVE_CONTINUE;
   }

  // Find the name if it exists;  emit the canonical field name, which
  // has the exact case from the declaration.  The user might have used
  // something like C.VaLuE when the field is "value". The local has to match.

  sem_struct *sptr = shape->sem->sptr;
  Invariant(sptr->count > 0);

  for (int32_t i = 0; i < sptr->count; i++) {
    if (!Strcasecmp(sptr->names[i], name)) {
      // We found the dot form of the name (e.g., 'bundle.foo') in the argument
      // bundle. The underscore version of the name (e.g., 'bundle_foo')
      // therefore must exist in `locals`: We always create both versions of the
      // name, and the local is not allowed to be redefined.
      //
      // It's important that we set the same `sem_t` pointer for both the dot
      // form and the underscore-separated form of references to parameters
      // within argument bundles. If we didn't, `find_mutable_type` would be
      // less useful: Getting up the type pointer for the dot form of the name
      // and setting an improvement on it wouldn't affect the underscore form,
      // and vice versa.
      //
      // The easiest way to set the same type pointer for both is to simply
      // resolve the underscore form.
      CHARBUF_OPEN(underscore_name);
      bprintf(&underscore_name, "%s_%s", scope, name);
      sem_resolve result = sem_try_resolve_variable(ast, underscore_name.ptr, NULL, type_ptr);
      CHARBUF_CLOSE(underscore_name);

      Invariant(result == SEM_RESOLVE_STOP);

      return SEM_RESOLVE_STOP;
    }
  }

  report_resolve_error(ast, "CQL0068: field not found in shape", name);
  record_resolve_error(ast);

  return SEM_RESOLVE_STOP;
}

// This function is responsible for resolving both unqualified identifiers (ids)
// and qualified identifiers (dots). It performs the following two roles:
//
// - If an optional `ast` is provided, it works the same way most semantic
//   analysis functions work: semantic information will be written into into the
//   ast, errors will be reported to the user, and errors will be recorded in
//   the AST.
//
// - `*typr_ptr` will be set to mutable type (`sem_t *`) in the current
//   environment if the identifier successfully resolves to a type. (There are,
//   unfortunately, a few exceptions in which a type will be successfully
//   resolved and yet `*typr_ptr` will not be set. These include when the
//   expression is `rowid` (or similar) and when the id resolves to an enum
//   case. The reason no mutable type is returned in these cases is that a new
//   type is allocated as part of semantic analysis, and there exists no single,
//   stable type in the environment to which a pointer could be returned. This
//   is a limitation of this function, albeit one that's currently not
//   problematic.)
//
//  Resolution is attempted in the order that the `sem_try_resolve_*` functions
//  appear in the `resolver` array. Each takes the same arguments: An (optional)
//  AST, a mandatory name, an optional scope, and mandatory type pointer. If the
//  identifier provided to one of these resolvers is resolved successfully, *or*
//  if the correct resolver was found but there was an error in the program,
//  `SEM_RESOLVE_STOP` is returned and resolution is complete, successful or
//  not. If a resolver is tried and it determines that it is not the correct
//  resolver for the identifier in question, `SEM_RESOLVE_CONTINUE` is returned
//  and the next resolver is tried.
//
// This function should not be called directly. If one is interested in
// performing semantic analysis, call `sem_resolve_id` (or, if within an
// expression, `sem_resolve_id_expr`). Alternatively, if one wants to get a
// mutable type from the environment, call `find_mutable_type`.
static void sem_resolve_id_with_type(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  *type_ptr = NULL;

  sem_resolve (*resolver[])(ast_node *ast, CSTR, CSTR, sem_t **) = {
    sem_try_resolve_arguments_bundle,
    sem_try_resolve_locals_bundle,
    sem_try_resolve_column,
    sem_try_resolve_rowid,
    sem_try_resolve_variable,
    sem_try_resolve_enum,
    sem_try_resolve_global_constant,
    sem_try_resolve_cursor_field,
    sem_try_resolve_arg_bundle,
  };

  for (uint32_t i = 0; i < sizeof(resolver) / sizeof(void *); i++) {
    if (resolver[i](ast, name, scope, type_ptr) == SEM_RESOLVE_STOP) {
      return;
    }
  }

  if (scope) {
    name = dup_printf("%s.%s", scope, name);
  }

  report_resolve_error(ast, "CQL0069: name not found", name);
  record_resolve_error(ast);
}

// Resolves a (potentially qualified) identifier, writing semantic information
// into `ast` if successful, or reporting and recording an error for `ast` if
// not.
cql_noexport void sem_resolve_id(ast_node *ast, CSTR name, CSTR scope) {
  Contract(is_id_or_dot(ast));
  Contract(name);

  // We have no use for `type` and simply throw it away.
  sem_t *type = NULL;
  sem_resolve_id_with_type(ast, name, scope, &type);
}

// Checks that the variable provided has been initialized, if required. As this
// is to be used only for *references* to a previously declared variable, the
// combination of flags indicating a completed initialization, if present, will
// be removed as they serve no purpose outside of declarations.
static void sem_validate_variable_referenced_is_initialized_if_required(ast_node *ast) {
  Contract(is_id_or_dot(ast));
  Contract(ast->sem);
  Contract(is_variable(ast->sem->sem_type));

  sem_t sem_type = ast->sem->sem_type;

  if (sem_type & SEM_TYPE_INIT_REQUIRED && !(sem_type & SEM_TYPE_INIT_COMPLETE)) {
    report_error(ast, "CQL0438: variable possibly used before initialization", ast->sem->name);
    record_error(ast);
    return;
  }

  // `SEM_TYPE_INIT_COMPLETE` and `SEM_TYPE_INIT_REQUIRED` are only useful on
  // declarations. We can remove these to avoid noise in the --ast output.
  ast->sem->sem_type &= sem_not(SEM_TYPE_INIT_REQUIRED | SEM_TYPE_INIT_COMPLETE);
}

// Returns true if the type of a cursor field requires checking that the cursor
// itself has a row before the field is accessed, else false.
static bool_t auto_cursor_field_requires_has_row_check(sem_t sem_type) {
  // Since cursor fields are technically variables and we want to use the same
  // policy for has-row checks as we use for initialization (because, after all,
  // fetching a cursor is just another form of initialization), we delegate
  // accordingly.
  return variable_should_require_initialization(sem_type);
}

// Given the AST of an auto cursor field access and the type of the auto cursor
// itself, returns true if a has-row check has been performed appropriately (if
// required), else false.
static void sem_validate_auto_cursor_field_accessed_has_row_check_if_required(ast_node *ast, sem_t cursor_type) {
  Contract(is_ast_dot(ast));
  Contract(ast->sem);
  Contract(is_variable(ast->sem->sem_type));
  Contract(!is_cursor(ast->sem->sem_type));
  Contract(is_auto_cursor(cursor_type));

  if (cursor_type & SEM_TYPE_HAS_ROW) {
    // The cursor has a row, so we're good.
    return;
  }

  if (!auto_cursor_field_requires_has_row_check(ast->sem->sem_type)) {
    // The cursor may not have a row, but having a row is not required to access
    // this particular field. All is well.
    return;
  }

  // The cursor has *not* been verified to have a row and the field being
  // accessed is of a type which requires it to have one. This is unsafe: If the
  // cursor does not have a row at runtime, the field will be NULL despite the
  // nonnull type. We issue an error accordingly.
  report_error(
    ast,
    "CQL0460: field of a nonnull reference type accessed before verifying that the cursor has a row",
    ast->sem->name
  );
  record_error(ast);
}

// Given an AST that may represent a cursor field reference, validate that the
// requirements for has-row checks have been met.
static void sem_validate_has_row_check_requirements_if_applicable(ast_node *ast) {
  Contract(ast);
  Contract(ast->sem);

  if (enforcement.strict_cursor_has_row) {
    if (is_ast_dot(ast)) {
      // Since this is a dot, `ast` might refer to a cursor field. To check if
      // it does, we have to look up the type of `scope`.
      EXTRACT_STRING(scope, ast->left);
      sem_t *type = find_mutable_type(scope, NULL);
      if (type && is_cursor(*type)) {
        // `scope` corresponds to a cursor (and thus `ast` refers to a cursor
        // field). It must, therefore, be the case that the cursor is an auto
        // cursor.
        Invariant(is_auto_cursor(*type));
        sem_validate_auto_cursor_field_accessed_has_row_check_if_required(ast, *type);
      }
    }
  }
}

// Given an AST that may represent a variable reference, validate that the
// requirements for initialization have been met.
static void sem_validate_initialization_requirements_if_applicable(ast_node *ast) {
  Contract(ast);
  Contract(ast->sem);

  sem_t sem_type = ast->sem->sem_type;

  if (is_variable(sem_type)) {
    // The name/scope pair refers to a variable. Ensure that it has been
    // appropriately initialized before use.
    sem_validate_variable_referenced_is_initialized_if_required(ast);
  }
}

// Analyze a cursor used as an expression; this is the has-row boolean case.
static void sem_cursor_as_expression(ast_node *ast) {
  Contract(ast);
  Contract(ast->sem);
  Contract(is_cursor(ast->sem->sem_type));

  // When the name of a cursor appears in an expression context, it doesn't
  // refer to the cursor itself. Instead, it refers to a boolean indicating
  // whether or not the cursor has a row. We adjust things here accordingly.
  CSTR vname = NULL;
  if (is_auto_cursor(ast->sem->sem_type)) {
    vname = dup_printf("%s._has_row_", ast->sem->name);
  } else {
    if (!(ast->sem->sem_type & SEM_TYPE_FETCH_INTO)) {
      report_error(ast, "CQL0067: cursor was not used with 'fetch [cursor]'", ast->sem->name);
      record_error(ast);
      return;
    }
    vname = dup_printf("_%s_has_row_", ast->sem->name);
  }
  ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_VARIABLE | SEM_TYPE_NOTNULL);
  ast->sem->name = vname;
}

// Analyze an expression subject to a nonnull improvement and rewrite it, if
// appropriate.
static void sem_notnull_improved_expr(ast_node *ast) {
  Contract(is_id_or_dot(ast));
  Contract(ast->sem);
  Contract(ast->sem->sem_type & SEM_TYPE_INFERRED_NOTNULL);

  if (is_analyzing_notnull_rewrite) {
    // If we're analyzing the product of a rewrite and we're already inside of a
    // call to `cql_inferred_notnull`, do not expand again.
    // forever.
    return;
  }

  if (current_loop_analysis_state == LOOP_ANALYSIS_STATE_ANALYZE) {
    // We're inside of a loop that we're going going to analyze again. If we
    // were to rewrite now, we could rewrite something to be nonnull that we'll
    // later find out is actually nullable.
    //
    // To avoid ending up with a bogus call to `cql_inferred_notnull`, we skip
    // the rewrite and optimistically make it nonnull directly. This poses no
    // problems for codegen if we turn out to be right because we'll perform the
    // rewrite as usual during the next stage of loop analysis, and it poses no
    // problems for semantic analysis if we're wrong because we'll catch the
    // error on the next phase.
    ast->sem->sem_type |= SEM_TYPE_NOTNULL;
    // Prevent this from propagating needlessly to keep --ast clean.
    ast->sem->sem_type &= u64_not(SEM_TYPE_INFERRED_NOTNULL);
    return;
  }

  // If we've made it here, it's safe and appropriate to do the rewrite.
  rewrite_nullable_to_notnull(ast);
}

// Like `sem_resolve_id`, but specific to expression contexts.
static void sem_resolve_id_expr(ast_node *ast, CSTR name, CSTR scope) {
  Contract(is_id_or_dot(ast));
  Contract(name);

  // Perform resolution, as for ids and dots outside of expressions.
  sem_resolve_id(ast, name, scope);
  if (is_error(ast)) {
    return;
  }

  sem_validate_has_row_check_requirements_if_applicable(ast);
  if (is_error(ast)) {
    return;
  }

  sem_validate_initialization_requirements_if_applicable(ast);
  if (is_error(ast)) {
    return;
  }

  sem_t sem_type = ast->sem->sem_type;

  if (is_cursor(sem_type)) {
    // Cursors themselves cannot have improved nullability.
    Invariant(!(sem_type & SEM_TYPE_INFERRED_NOTNULL));
    sem_cursor_as_expression(ast);
    return;
  }

  if (sem_type & SEM_TYPE_INFERRED_NOTNULL) {
    // Things with improved nullability must not be cursors.
    Invariant(!is_cursor(sem_type));
    sem_notnull_improved_expr(ast);
  }
}

// Returns the *mutable* type (`sem_t *`) for a given (potentially qualified)
// identifier if one exists in the environment. The type pointer returned is for
// the original binding and thus corresponds to the type set via
// `sem_resolve_id`, *not* the type set via `sem_resolve_id_expr`. See the
// documentation for `sem_resolve_id_with_type` for limitations.
static sem_t *find_mutable_type(CSTR name, CSTR scope) {
  Contract(name);

  sem_t *type = NULL;
  sem_resolve_id_with_type(NULL, name, scope, &type);

  return type;
}

// Like `find_mutable_type`, but sets `is_global` to true if the name/scope pair
// refers to either a global variable or the field of a global auto cursor
// (i.e., anything that is both global and mutable).
static sem_t *find_mutable_type_and_global_status(CSTR name, CSTR scope, bool_t *is_global) {
  Contract(name);
  Contract(is_global);

  *is_global = false;

  // First, we perform resolution as we normally would.
  sem_t *type = NULL;
  sem_resolve_id_with_type(NULL, name, scope, &type);

  // This function is presently only used for setting nullability improvements.
  // Given that nullability improvements are only set after an expression or
  // statement has been analyzed successfully, and given that there exists no
  // nullable type for which `sem_resolve_id_with_type` will ever fail to set a
  // type pointer -- the things for which it can fail, e.g., enum cases, all
  // have nonnull types -- we must have `type`. Should this ever change, it
  // would be appropriate to return here when `type` is NULL: We only do not do
  // that now for the sake of maintaining code coverage.
  Invariant(type);

  // Resolving was successful. Now, we need to check whether or not it resolved
  // to either a global variable or a field of a global auto cursor.
  if (scope) {
    // We have a name and a scope: The name/scope pair might refer to a global
    // auto cursor field.
    symtab_entry *entry = symtab_find(globals, scope);
    if (!entry) {
      // `scope` does not appear in globals, so we know this cannot be the
      // global case.
      return type;
    }

    ast_node *ast = entry->val;

    if (!is_cursor(ast->sem->sem_type)) {
      // We found something in `globals` that supports dot syntax, but it's not
      // a cursor. It must be the case that `scope` refers to something that
      // both shadows the name in `globals` and also supports dot syntax (e.g.,
      // a local cursor or an argument bundle).
      return type;
    }

    // There is global cursor named `scope`. We resolve `name` as though it were
    // one of its fields, then compare that resolution to the one above to
    // determine whether the name/scope pair refers to a field of the global
    // auto cursor (the equal case) or whether it refers to something that
    // shadows it (the non-equal case).
    sem_t *global_type = NULL;
    sem_resolve_cursor_field(NULL, ast, name, &global_type);

    *is_global = type == global_type;
  }
  else {
    // We only have a name: The name might refer to a global variable.
    symtab_entry *entry = symtab_find(globals, name);
    if (!entry) {
      // `name` does not appear in globals, so we know this cannot be the global
      // case.
      return type;
    }

    ast_node *ast = entry->val;

    // There is a global variable named `name`. Now, we just need to compare the
    // address of its `sem_t` to what we resolved above to determine whether
    // `name` refers to the global variable (the equal case) or whether it
    // refers to something that shadows it (the non-equal case).
    *is_global = type == &ast->sem->sem_type;
  }

  return type;
}

// Here we check that type<Foo> only combines with type<Foo> or type.
// If there is a current object type, then the next item must match
// If there is no such type, then an object type that arrives becomes the required type
// if they ever don't match record an error
static CSTR sem_combine_kinds_general(ast_node *ast, CSTR kleft, CSTR kright) {
  if (kright) {
    if (kleft) {
      if (Strcasecmp(kleft, kright)) {
        CSTR errmsg = dup_printf("CQL0070: expressions of different kinds can't be mixed: '%s' vs. '%s'", kright, kleft);
        report_error(ast, errmsg, NULL);
        record_error(ast);
      }
    }
    return kright;
  }

  return kleft;
}

// helper to crack the ast nodes first and then call the normal comparisons
static CSTR sem_combine_kinds(ast_node *ast, CSTR kright) {
  CSTR kleft = ast->sem->kind;
  return sem_combine_kinds_general(ast, kleft, kright);
}

// Validate the contents of the case list of a case expression.
//
// There are two parts to each element the list: the "when" expression and the
// "then" expression. We compute the aggregate type of the when expressions as
// we go, promoting it up to a larger type if needed (e.g., if one when is an
// int and the other is a real, then the result is a real). Likewise,
// nullability is computed as the aggregate. Note that if nothing matches, the
// result is null; we always get a nullable result unless there is an "else"
// expression.
//
// If we started with "case expr" (indicated by `has_expression_to_match`), then
// each when expression must be comparable to the case expression. If we started
// with "case when xx then yy", then each case expression must be numeric
// (typically boolean).
//
// As with IF, CASE can improve nullability. This is only possible when
// `has_expression_to_match` is false though. This is because something like
// "CASE 0 WHEN x IS NOT NULL THEN x ELSE y" will only ever take the first
// branch when "x" IS null despite the "IS NOT NULL" check.
static void sem_case_list(
  ast_node *head,
  bool_t has_expression_to_match,
  sem_t sem_type_required_for_when,
  CSTR kind_required_for_when,
  bool_t is_iif)
{
  Contract(is_ast_case_list(head));
  Contract(has_expression_to_match || sem_type_required_for_when == SEM_TYPE_BOOL);
  sem_t sem_type_result = SEM_TYPE_PENDING;
  CSTR then_kind = NULL;

  sem_t sem_sensitive  = 0;

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_NOTNULL(when, ast->left);
    // WHEN [case_expr] THEN [then_expr]
    EXTRACT_ANY_NOTNULL(case_expr, when->left);
    EXTRACT_ANY_NOTNULL(then_expr, when->right);

    sem_expr(case_expr);
    if (is_error(case_expr)) {
      record_error(ast);
      record_error(head);
      return;
    }

    if (!sem_verify_compat(case_expr, sem_type_required_for_when, case_expr->sem->sem_type, is_iif ? "iif" : "when")) {
      record_error(ast);
      record_error(head);
      return;
    }

    sem_combine_kinds(case_expr, kind_required_for_when);
    if (is_error(case_expr)) {
      record_error(ast);
      record_error(head);
      return;
    }

    FLOW_PUSH_CONTEXT_BRANCH();
    if (!has_expression_to_match) {
      sem_set_improvements_for_true_condition(case_expr);
    }
    sem_expr(then_expr);
    FLOW_POP_CONTEXT_BRANCH();

    if (is_error(then_expr)) {
      record_error(ast);
      record_error(head);
      return;
    }

    sem_set_improvements_for_false_condition(case_expr);

    sem_sensitive |= sensitive_flag(case_expr->sem->sem_type);
    sem_sensitive |= sensitive_flag(then_expr->sem->sem_type);

    if (sem_type_result == SEM_TYPE_PENDING) {
      sem_type_result = then_expr->sem->sem_type;
      then_kind = then_expr->sem->kind;
    }
    else {
      sem_t sem_type_current = then_expr->sem->sem_type;

      if (!sem_verify_compat(then_expr, sem_type_result, sem_type_current, "then")) {
        record_error(ast);
        record_error(head);
        return;
      }

      then_kind = sem_combine_kinds(then_expr, then_kind);
      if (is_error(then_expr)) {
        record_error(ast);
        record_error(head);
        return;
      }

      // upgrade the result type to a bigger type if needed
      sem_type_result = sem_combine_types(sem_type_result, sem_type_current);
    }

    when->sem = then_expr->sem;
    ast->sem = when->sem;
  }

  head->sem = new_sem(sem_type_result | sem_sensitive);
  head->sem->kind = then_kind;
}

// Performs analysis of case expressions, including those rewritten from an IIF.
// The case list is handled in the above function; in this part, we find the
// type of the expr in "case [expr]" if there is one, then we do the else
// handling. Note that the absence of an else forces the case to have a possibly
// null result.
cql_noexport void sem_case(ast_node *ast, bool_t is_iif) {
  Contract(is_ast_case_expr(ast));
  EXTRACT_ANY(expr, ast->left);
  EXTRACT_NOTNULL(connector, ast->right);
  EXTRACT_NOTNULL(case_list, connector->left);
  EXTRACT_ANY(else_expr, connector->right);

  // CASE [expr]? [case_list] ELSE [else_expr] END

  sem_t sem_type_required_for_when = SEM_TYPE_BOOL;
  sem_t sem_sensitive = 0;
  CSTR kind_required_for_when = NULL;

  if (expr) {
    // case can have expression or just when clauses
    sem_expr(expr);
    if (is_error(expr)) {
      record_error(ast);
      return;
    }
    kind_required_for_when = expr->sem->kind;
    sem_type_required_for_when = core_type_of(expr->sem->sem_type);
    sem_sensitive |= sensitive_flag(expr->sem->sem_type);
  }

  // Each WHEN expression may improve its corresponding THEN expression when we
  // are not using the matching form of CASE (i.e, when we're not using the
  // "CASE expr case_list ..." form). We create a new context here to contain
  // all of the contingent contexts created within `sem_case_list`.
  //
  // The reason we create this context here instead of within `sem_case_list` is
  // that we do not want to pop it until after we've analyzed `else_expr`. Doing
  // so would allow un-improvements within the case list to negatively affect
  // `else_expr`; that would be incorrect, because if `else_expr` ends up being
  // evaluated, none of the THEN expressions within `case_list` could have been
  // evaluated.
  FLOW_PUSH_CONTEXT_BRANCH_GROUP();

  sem_case_list(case_list, !!expr, sem_type_required_for_when, kind_required_for_when, is_iif);
  if (is_error(case_list)) {
    goto error;
  }

  ast->sem = case_list->sem;
  sem_sensitive |= sensitive_flag(case_list->sem->sem_type);

  if (else_expr) {
    flow_set_context_branch_group_covers_all_cases(true);
    FLOW_PUSH_CONTEXT_BRANCH();
    sem_expr(else_expr);
    FLOW_POP_CONTEXT_BRANCH();
    if (is_error(else_expr)) {
      goto error;
    }

    sem_t sem_type_else = else_expr->sem->sem_type;
    sem_t sem_type_result = ast->sem->sem_type;

    sem_sensitive |= sensitive_flag(sem_type_else);

    if (!sem_verify_compat(else_expr, sem_type_result, sem_type_else, is_iif ? "iif" : "else")) {
      goto error;
    }

    CSTR else_kind = sem_combine_kinds(else_expr, ast->sem->kind);
    if (is_error(else_expr)) {
      goto error;
    }

    sem_type_result = sem_combine_types(sem_type_result, sem_type_else);
    ast->sem = new_sem(sem_type_result | sem_sensitive);
    ast->sem->kind = else_kind;
  }
  else {
    // If there is no else clause then you get null if you miss all the cases
    // so it has to be nullable return type.
    sem_t new_flags = ast->sem->sem_type;
    new_flags &= sem_not(SEM_TYPE_NOTNULL);
    new_flags |= sem_sensitive;
    sem_replace_flags(ast, new_flags);
  }

  connector->sem = ast->sem;

cleanup:
  FLOW_POP_CONTEXT_BRANCH_GROUP();
  return;

error:
  record_error(ast);
  goto cleanup;
}

static void sem_expr_case(ast_node *ast, CSTR cstr) {
  Contract(is_ast_case_expr(ast));

  sem_case(ast, IS_CASE);
}

// if we get here we are re-evaluating a subtree, this rewritten bit
// is necessarily processed so we don't have to do it again
static void sem_expr_between_rewrite(ast_node *ast, CSTR cstr) {
  Contract(is_ast_between_rewrite(ast));
  return;
}

// Between requires type compatability between all three of its arguments.
// Nullability follows the usual rules, if any might be null then the result
// type might be null.  In any case the result's core type is BOOL.
static void sem_expr_between_or_not_between(ast_node *ast, CSTR cstr) {
  Contract(is_ast_between(ast) || is_ast_not_between(ast));
  EXTRACT_NOTNULL(range, ast->right);

  bool_t between = is_ast_between(ast);

  // [ast->left] [NOT] BETWEEN [range->left] AND [range->right]");
  sem_expr(ast->left);
  sem_expr(range->left);
  sem_expr(range->right);

  if (is_error(ast->left) || is_error(range->left) || is_error(range->right)) {
    record_error(ast);
    return;
  }

  sem_t sem_type_item = ast->left->sem->sem_type;
  sem_t sem_type_min = range->left->sem->sem_type;
  sem_t sem_type_max = range->right->sem->sem_type;

  CSTR operation = between ? "BETWEEN" : "NOT BETWEEN";
  CSTR operation_or_and = between ? "BETWEEN/AND" : "NOT BETWEEN/AND";
  if (is_blob(sem_type_item)) {
    report_error(ast->left, "CQL0071: first operand cannot be a blob in", operation);
    record_error(ast);
    return;
  }

  if (is_object(sem_type_item)) {
    report_error(ast->left, "CQL0072: first operand cannot be an object in", operation);
    record_error(ast);
    return;
  }

  // the min can't be compared with the item
  if (!sem_verify_compat(ast, sem_type_item, sem_type_min, operation)) {
    return;
  }

  // the max can't be compared with the item
  if (!sem_verify_compat(ast, sem_type_item, sem_type_max, operation)) {
    return;
  }

  // the right and left aren't compatible with each other even though they are both compatible with the main operand
  // e.g. null between 1 and 'x'
  if (!sem_verify_compat(ast, sem_type_min, sem_type_max, operation_or_and)) {
    return;
  }

  // check for compatible kinds between item and the min
  sem_combine_kinds(range->left, ast->left->sem->kind);
  if (is_error(range->left)) {
    record_error(ast);
    return;
  }

  // check for compatible kinds between item and the max
  sem_combine_kinds(range->right, ast->left->sem->kind);
  if (is_error(range->right)) {
    record_error(ast);
    return;
  }

  // check for compatible kinds between min and max
  // this can fail if the item is generic and the left and right have kind
  // e.g.   12 between min_dollars and max_euros
  sem_combine_kinds(range->right, range->left->sem->kind);
  if (is_error(range->right)) {
    record_error(ast);
    return;
  }

  // If we're going to be doing this not to SQL then we rewrite the between operation
  // as follows:
  //  * x between y and z ==> temp = x, temp >= y AND temp <= z
  //  * x not between y and z ==>  temp = x, temp < y OR temp > z
  // We do this to get the right short circuit behavior for between without having
  // to duplicate the highly complex codge for shortcut AND/OR
  if (current_expr_context == SEM_EXPR_CONTEXT_NONE) {
    ast_node *expr = ast->left;

    AST_REWRITE_INFO_SET(expr->lineno, expr->filename);

    symtab *scope = locals ? locals : globals;
    Invariant(scope);
    CSTR name = dup_printf("_between_%d_", between_count++);

    // implictly declare the local variable we need
    ast_node *asts = new_ast_str(name);
    asts->sem = new_sem(expr->sem->sem_type | SEM_TYPE_VARIABLE);
    asts->sem->name = name;
    symtab_add(scope, name, asts);

    ast_node *left_item = new_ast_str(name);
    ast_node *right_item = new_ast_str(name);
    ast_node *left_cmp;
    ast_node *right_cmp;
    ast_node *combine;

    if (between) {
      left_cmp = new_ast_ge(left_item, range->left);
      right_cmp = new_ast_le(right_item, range->right);
      combine = new_ast_and(left_cmp, right_cmp);
    }
    else {
      left_cmp = new_ast_lt(left_item, range->left);
      right_cmp = new_ast_gt(right_item, range->right);
      combine = new_ast_or(left_cmp, right_cmp);
    }

    ast_set_right(range, combine);
    ast_set_left(range, new_ast_str(name));

    sem_expr(range->left);
    sem_expr(range->right);

    ast->type = k_ast_between_rewrite;

    AST_REWRITE_INFO_RESET();
  }

  sem_t combined_flags = not_nullable_flag(sem_type_item) & both_notnull_flag(sem_type_min, sem_type_max);
  combined_flags |= sensitive_flag(sem_type_item) | sensitive_flag(sem_type_min) | sensitive_flag(sem_type_max);
  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
}

// For cast expressions we use the type provided for the semantic type
// the only trick is that we preserve the combined_flags of the input argument.
static void sem_expr_cast(ast_node *ast, CSTR cstr) {
  Contract(is_ast_cast_expr(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, ast->right);

  // CAST ( expr, data_type )

  sem_expr(expr);
  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  sem_data_type_column(data_type);
  if (is_error(data_type)) {
    record_error(ast);
    return;
  }

  // We allow conversion between numeric types without going to SQLite, the text conversions
  // are crazy complex and basically impossible to clone so you have to do (select cast(...))
  // for those.

  if (!is_numeric(data_type->sem->sem_type) || !is_numeric(expr->sem->sem_type)) {
    if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
      report_error(ast, "CQL0073: CAST may only appear in the context of SQL statement", NULL);
      record_error(ast);
      return;
    }
  }

  if (enforcement.strict_cast) {
    if (core_type_of(data_type->sem->sem_type) == core_type_of(expr->sem->sem_type)) {
      // if the core type is the same and the kind is the same then the cast did nothing
      CSTR k1 = data_type->sem->kind;
      CSTR k2 = expr->sem->kind;

      // either both are null, or both are not null and they match
      bool_t same = (!k1 && !k2) || (k1 && k2 && !Strcasecmp(k1, k2));

      if (same) {
        CSTR err_msg = dup_expr_text(ast);
        report_error(expr, "CQL0170: cast is redundant, remove to reduce code size", err_msg);
        record_error(ast);
        return;
      }
    }
  }

  sem_t combined_flags = not_nullable_flag(expr->sem->sem_type) | sensitive_flag(expr->sem->sem_type);

  ast->sem = new_sem(data_type->sem->sem_type | combined_flags);
  ast->sem->kind = data_type->sem->kind;
}

// Coalesce requires type compatability between all of its arguments.  The result
// is a not null type if we find a not null item in the list.  There should be
// nothing after that item.  Note that ifnull and coalesce are really the same thing
// except ifnull must have exactly two arguments.
static void sem_coalesce(ast_node *call_ast, bool_t is_ifnull) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // ifnull ( [arg_list] )
  // coalesce ( [arg_list] )

  sem_t sem_sensitive = 0;

  uint32_t arg_count = 0;
  for (ast_node *ast = arg_list; ast; ast = ast->right) arg_count++;

  if (arg_count < 2) {
    report_error(name_ast, "CQL0074: too few arguments provided", name);
    record_error(call_ast);
    return;
  }

  if (is_ifnull && arg_count != 2) {
    report_error(name_ast, "CQL0075: incorrect number of arguments", name);
    record_error(call_ast);
    return;
  }

  sem_t sem_type_result = SEM_TYPE_PENDING;
  CSTR kind_result = NULL;

  for (ast_node *ast = arg_list; ast; ast = ast->right) {
    ast_node *expr = ast->left;

    if (is_ast_null(expr)) {
      report_error(expr, "CQL0076: NULL literal is useless in function", name);
      record_error(expr);
      record_error(call_ast);
      return;
    }

    // arg list already already analyzed for us by sem_expr_call
    // sem_expr(expr);
    Invariant(expr->sem);

    sem_sensitive |= sensitive_flag(expr->sem->sem_type);

    if (sem_type_result == SEM_TYPE_PENDING) {
      sem_type_result = expr->sem->sem_type;
    }
    else {
      sem_t sem_type_current = expr->sem->sem_type;
      if (!sem_verify_compat(expr, sem_type_result, sem_type_current, name)) {
        record_error(expr);
        record_error(call_ast);
        return;
      }

      // Note that if we are still here the result is not yet nullable
      // the combined result will therefore not be nullable, this is ok
      // because the next thing we do is improve the type.
      Invariant(!is_not_nullable(sem_type_result));
      sem_type_result = sem_combine_types(sem_type_result, sem_type_current);

      // This is the magic right here: upgrade the result type to not null
      // and stop at this point.  There should be nothing after the first item
      // that is known to be not null.
      if (is_not_nullable(sem_type_current)) {
        sem_type_result |= SEM_TYPE_NOTNULL;
      }
    }


    // Even the first arg might be not-nullable so we check every time through
    if (is_not_nullable(sem_type_result) && ast->right) {
      CSTR err_msg = dup_expr_text(expr);
      report_error(expr, "CQL0077: encountered arg known to be not null"
                          " before the end of the list, rendering the rest useless.", err_msg);
      record_error(call_ast);
      return;
    }

    kind_result = sem_combine_kinds(expr, kind_result);
    if (is_error(expr)) {
      record_error(call_ast);
      return;
    }
  }

  call_ast->sem = new_sem(sem_type_result | sem_sensitive);
  call_ast->sem->kind = kind_result;
}

// The in predicate is like many of the other multi-argument operators.  All the
// items must be type compatible.  Note that in this case the nullablity of
// the items does not matter, only the nullability of the item being tested.
// Note that null in (null) is null, not true.
static void sem_expr_in_pred_or_not_in(ast_node *ast, CSTR cstr) {
  Contract(is_ast_in_pred(ast) || is_ast_not_in(ast));
  EXTRACT_ANY_NOTNULL(needle, ast->left);

  // [needle] [NOT] IN ( [expr_list | select_stmt] )

  sem_expr(needle);
  if (is_error(needle)) {
    record_error(ast);
    return;
  }

  sem_t sem_type_needed = needle->sem->sem_type;
  CSTR kind_needed = needle->sem->kind;
  sem_t combined_flags = not_nullable_flag(sem_type_needed) | sensitive_flag(sem_type_needed);

  if (is_ast_expr_list(ast->right)) {
    EXTRACT_NOTNULL(expr_list, ast->right);

    // make sure the items are all of some comparable type
    for (ast_node *item = expr_list; item; item = item->right) {
      ast_node *expr = item->left;

      sem_expr(expr);
      item->sem = expr->sem;

      if (is_error(expr)) {
        record_error(ast);
        return;
      }

      sem_t sem_type_current = expr->sem->sem_type;
      if (!sem_verify_compat(ast, sem_type_needed, sem_type_current, is_ast_in_pred(ast) ? "IN" : "NOT IN")) {
        return;
      }

      sem_combine_kinds(expr, kind_needed);
      if (is_error(expr)) {
        record_error(ast);
        return;
      }

      combined_flags |= sensitive_flag(sem_type_current);
      sem_type_needed = sem_combine_types(sem_type_needed, sem_type_current);
    }
  }
  else {
    uint32_t valid = SEM_EXPR_CONTEXT_SELECT_LIST
                    |SEM_EXPR_CONTEXT_WHERE
                    |SEM_EXPR_CONTEXT_ON
                    |SEM_EXPR_CONTEXT_HAVING
                    |SEM_EXPR_CONTEXT_TABLE_FUNC;

    if (CURRENT_EXPR_CONTEXT_IS_NOT(valid)) {
      report_error( ast, "CQL0078: [not] in (select ...) is only allowed inside "
                         "of select lists, where, on, and having clauses", NULL);
      record_error(ast);
      return;
    }

    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

    sem_expr_select((ast_node *)select_stmt, "SELECT");
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    // make sure the select statement is of some comparable type
    if (!sem_verify_compat(ast, sem_type_needed, select_stmt->sem->sem_type, is_ast_in_pred(ast) ? "IN" : "NOT IN")) {
      return;
    }

    sem_combine_kinds(select_stmt, kind_needed);
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    combined_flags |= sensitive_flag(select_stmt->sem->sem_type);
  }

  ast->sem = new_sem(SEM_TYPE_BOOL | combined_flags);
}

// This is a helper method that logs an error if the indicated counts
// do not match.  It doesn't actually walk the list.  We always have to
// do that for other reasons anyway so there is no additional walk here.
static bool_t sem_validate_arg_count(ast_node *ast, uint32_t count, uint32_t expected) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  if (count != expected) {
    report_error(name_ast, "CQL0079: function got incorrect number of arguments", name);
    record_error(name_ast);
    record_error(ast);
    return false;
  }

  return true;
}

// This helper method checks the given name against the current context
static bool_t sem_validate_context(ast_node *ast, CSTR name, uint32_t valid_contexts) {
  if (CURRENT_EXPR_CONTEXT_IS(valid_contexts)) {
    return true;
  }

  report_error(ast, "CQL0080: function may not appear in this context", name);
  record_error(ast);
  return false;
}

// This helper method checks the function against the mask of its valid contexts.
static bool_t sem_validate_function_context(ast_node *ast, uint32_t valid_contexts) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  return sem_validate_context(ast, name, valid_contexts);
}

// This is a helper method that tells us if the EXISTS function can be used
// in the current expression context.  Exists cannot be used in GROUP BY
// for instance.  Nor does it make sense in a loose expression outside of
// a select.
static bool_t sem_validate_exists_context(ast_node *ast) {
  Contract(is_ast_exists_expr(ast));

  return sem_validate_context(ast, "exists",
            SEM_EXPR_CONTEXT_SELECT_LIST |
            SEM_EXPR_CONTEXT_HAVING |
            SEM_EXPR_CONTEXT_WHERE |
            SEM_EXPR_CONTEXT_ON |
            SEM_EXPR_CONTEXT_TABLE_FUNC);
}

// Compute the type of an exists subexpression.  The context must be valid
// the nested select must be ok.  The result will be a not null boolean.
static void sem_expr_exists(ast_node *ast, CSTR cstr) {
  Contract(is_ast_exists_expr(ast));
  EXTRACT_ANY_NOTNULL(select_stmt, ast->left);

  if (!sem_validate_exists_context(ast)) {
    return;
  }

  sem_select(select_stmt);
  if (is_error(select_stmt)) {
    record_error(ast);
    return;
  }

  sem_t sem_sensitive = any_sensitive(select_stmt->sem->sptr);
  ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL | sem_sensitive);
}

static bool_t sem_validate_window_context(ast_node *ast) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  // check the context first, better error message
  if (!sem_validate_function_context(ast, SEM_EXPR_CONTEXT_WINDOW)) {
    return false;
  }

  return true;
}

// Aggregate functions can only be used in certain places.  For instance
// they may not appear in a WHERE clause.  Validate the current context.
static bool_t sem_validate_aggregate_context(ast_node *ast) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  // check the context first, better error message
  if (!sem_validate_function_context(ast,
            SEM_EXPR_CONTEXT_SELECT_LIST |
            SEM_EXPR_CONTEXT_HAVING |
            SEM_EXPR_CONTEXT_ORDER_BY |
            SEM_EXPR_CONTEXT_WINDOW |
            SEM_EXPR_CONTEXT_WINDOW_FILTER)) {
    return false;
  }

  if (!current_joinscope || !current_joinscope->jptr) {
    report_error(ast, "CQL0081: aggregates only make sense if there is a FROM clause", name);
    record_error(ast);
    return false;
  }

  return true;
}

// validate the node appear inside SQL statement
static bool_t sem_validate_appear_inside_sql_stmt(ast_node *ast) {
  return sem_validate_function_context(ast, u32_not(SEM_EXPR_CONTEXT_NONE));
}

// validate the node appear inside SQL statement
static bool_t sem_validate_sql_not_constraint(ast_node *ast) {
  return sem_validate_function_context(ast, u32_not(SEM_EXPR_CONTEXT_NONE | SEM_EXPR_CONTEXT_CONSTRAINT));
}


// cql_blob_get(blob, table.column) -- this will ultimately expand into
// user_defined_blob_get(blob, hash_code) but we need the table form so that we know the
// result type accurately.  The rewrite happens when we use gen_sql with for_sqlite true.
// In all other stages, it stays as is.  As a consequence, we also get a dependency on the
// backed table.  When we visit this kind of node we also want to create a dependency on
// the backing table.
static void sem_special_func_cql_blob_get(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  *is_aggregate = false;

  // round can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 2)) {
    return;
  }

  ast_node *blob_expr = first_arg(arg_list);

  sem_expr(blob_expr);
  if (is_error(blob_expr)) {
    record_error(ast);
    return;
  }

  if (!sem_verify_compat(blob_expr, blob_expr->sem->sem_type, SEM_TYPE_BLOB, "cql_blob_get")) {
    record_error(ast);
    return;
  }

  ast_node *table_expr = second_arg(arg_list);

  if (!is_ast_dot(table_expr) || !is_ast_str(table_expr->left) || !is_ast_str(table_expr->right)) {
    report_error(table_expr, "CQL0257: argument must be table.column where table is a backed table", NULL);
    record_error(ast);
    return;
  }

  EXTRACT_STRING(t_name, table_expr->left);
  EXTRACT_STRING(c_name, table_expr->right);

  // give a better error if the table is not found
  ast_node *table_ast = find_usable_and_not_deleted_table_or_view(
      t_name,
      table_expr->left,
      "CQL0095: table/view not defined");
  if (!table_ast) {
    record_error(ast);
    return;
  }

  if (!is_backed(table_ast->sem->sem_type)) {
    report_error(table_expr, "CQL0488: the indicated table is not declared for backed storage", t_name);
    record_error(ast);
    return;
  }

  sem_t sem_type = find_column_type(t_name, c_name);
  if (!sem_type) {
    CSTR err_data = dup_printf("%s.%s", t_name, c_name);
    report_error(table_expr, "CQL0489: the indicated column is not present in the named backed storage", err_data);
    record_error(ast);
    return;
  }

  table_expr->sem = name_ast->sem = ast->sem = new_sem(sem_type);

  // ast->sem->name is not set here because e.g. cql_blob_get(x, foo.bar) is not named "x"
}

// You can count anything, you always get an integer
static void sem_special_func_count(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  *is_aggregate = true;

  sem_arg_list(arg_list, IS_COUNT);
  if (arg_list && is_error(arg_list)) {
    record_error(ast);
    return;
  }

  if (!sem_validate_aggregate_context(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  sem_node *sem = first_arg(arg_list)->sem;
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL | sensitive_flag(sem->sem_type));

  // ast->sem->name is not set here because e.g. sum(x) is not named "x"
}

// You can min/max numerics and strings, you get what you started with.
static void sem_aggr_func_min_or_max(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // if no args then fail the arg count test...
  if (arg_count == 0) {
    sem_validate_arg_count(ast, arg_count, 1);
    return;
  }

  sem_node *sem = first_arg(arg_list)->sem;
  sem_t core_type = core_type_of(sem->sem_type);

  if (!is_numeric(core_type) && !is_text(core_type)) {
    report_error(ast, "CQL0257: argument must be a string or numeric in", name);
    record_error(ast);
    return;
  }

  // If the one arg version then only in an aggregate context
  if (arg_count == 1) {
    if (!sem_validate_aggregate_context(ast)) {
      return;
    }

    // sensitivity is preserved but nullability is not because (e.g.)
    // select max(1) from sqlite_master where 0;  ->  NULL not zero rows
    name_ast->sem = ast->sem = new_sem(core_type | sensitive_flag(sem->sem_type));

    // ast->sem->name is not set here because e.g. min(x) is not named "x"

    return;
  }

  // min/max can only appear inside of SQL, the multi-column version can be anywhere
  // because it's not an aggregation
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  sem_t sem_type_needed = sem->sem_type;
  sem_t combined_flags = not_nullable_flag(sem_type_needed) | sensitive_flag(sem_type_needed);

  // we know there are at least two args
  ast_node *ast_args = arg_list->right;
  Invariant(ast_args);

  for (; ast_args; ast_args = ast_args->right) {
    EXTRACT_ANY_NOTNULL(arg, ast_args->left);
    sem_t sem_type_current = arg->sem->sem_type;

    if (!is_numeric(sem_type_current) && !is_text(sem_type_current)) {
      report_error(ast, "CQL0257: argument must be a string or numeric in", name);
      record_error(ast);
      return;
    }

    if (!sem_verify_compat(ast, sem_type_needed, sem_type_current, name)) {
      return;
    }

    combined_flags |= sensitive_flag(sem_type_current);
    sem_type_needed = sem_combine_types(sem_type_needed, sem_type_current);
  }

  name_ast->sem = ast->sem = new_sem(sem_type_needed | combined_flags);
  // the new node is does not keep any of the names of the members, just the net type
}

// You can round real numbers, you may specify a precision
static void sem_func_round(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // round can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  // if no args then fail the arg count test...
  if (arg_count == 0) {
    sem_validate_arg_count(ast, arg_count, 1);
    return;
  }

  // if too many args
  if (arg_count > 2) {
    sem_validate_arg_count(ast, arg_count, 2);
    return;
  }

  sem_node *sem = first_arg(arg_list)->sem;
  sem_t sem_type = sem->sem_type;
  sem_t core_type = core_type_of(sem_type);

  if (core_type != SEM_TYPE_REAL) {
    report_error(ast, "CQL0087: first argument must be of type real", name);
    record_error(ast);
    return;
  }

  sem_t combined_flags = not_nullable_flag(sem_type) | sensitive_flag(sem_type);

  if (arg_count == 2) {
    ast_node *arg2 = second_arg(arg_list);
    if (!is_numeric_expr(arg2)) {
      report_error(name_ast, "CQL0082: second argument must be numeric", name);
      record_error(ast);
      return;
    }
    sem_reject_real(arg2, "ROUND argument 2");
    if (is_error(arg2)) {
      record_error(ast);
      return;
    }
    combined_flags = combine_flags(sem_type, arg2->sem->sem_type);
    name_ast->sem = ast->sem = new_sem(SEM_TYPE_REAL | combined_flags);
  }
  else {
    name_ast->sem = ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | combined_flags);
  }
}

// Min and Max are the same validation
static void sem_aggr_func_max(ast_node *ast, uint32_t arg_count) {
  sem_aggr_func_min_or_max(ast, arg_count);
}

// Min and Max are the same validation
static void sem_aggr_func_min(ast_node *ast, uint32_t arg_count) {
  sem_aggr_func_min_or_max(ast, arg_count);
}

// Avg validation -> any numeric is ok, but you get a real back.
static void sem_aggr_func_avg(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_aggregate_context(ast)) {
    return;
  }

  // Note: avg does not have a multi-arg form like min/max, only
  // the single arg form is legal in Sqlite
  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  if (!is_numeric_expr(arg) || is_ast_null(arg)) {
    report_error(name_ast, "CQL0082: argument must be numeric", name);
    record_error(ast);
    return;
  }

  // avg will be real, sensitivity preserved, nullability NOT preserved
  // because all aggregates can return NULL if there are zero rows
  // e.g. select avg(1) from sqlite_master where 0;   -> NULL
  sem_t combined_flags = sensitive_flag(arg->sem->sem_type);
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_REAL | combined_flags);

  // ast->sem->name is not set here because e.g. avg(x) is not named "x"
}

static void sem_func_ifnull(ast_node *ast, uint32_t arg_count) {
  sem_coalesce(ast, 1);  // set "ifnull"
}

// This is a wrapper function that tells the code generator to compress
// the string literal into fragments like we do for statements.  This
// will do nothing unless --compress has been selected
static void sem_func_cql_compressed(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_context(ast, name, SEM_EXPR_CONTEXT_NONE)) {
    return;
  }

  // only one argument
  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  if (!is_strlit(arg)) {
    report_error(ast, "CQL0421: first argument must be a string literal", name);
    record_error(ast);
    return;
  }

  // the literal type flows through
  name_ast->sem = ast->sem = arg->sem;
}

// The usual blob verification stuff.  Note that cql_get_blob_size
// returns not null (blob size of nil is zero).
// We can't declare this function with the normal syntax because it
// has to prop sensitivity.  We should probably have a syntax for
// nullable args get nullable results and sensitive args get sensitive
// results but we don't have one at this time.
static void sem_func_cql_get_blob_size(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_context(ast, name, SEM_EXPR_CONTEXT_NONE)) {
    return;
  }

  // only one argument
  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);
  if (!is_blob(arg->sem->sem_type)) {
    report_error(ast, "CQL0345: argument must be of type blob", name);
    record_error(ast);
    return;
  }

  // integer type
  sem_t sem_type = SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL;

  // add sensitivity if argument is sensitive
  sem_type |= sensitive_flag(arg->sem->sem_type);

  name_ast->sem = ast->sem = new_sem(sem_type);
}

static void sem_func_length(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // length can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  // one or two args
  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);
  if (!is_text(arg->sem->sem_type)) {
    report_error(ast, "CQL0085: all arguments must be strings", name);
    record_error(ast);
    return;
  }

  // integer type
  sem_t sem_type = SEM_TYPE_INTEGER;
  // add sensitivity if argument is sensitive
  sem_type |= sensitive_flag(arg->sem->sem_type);
  // add nullability if argument is nullable
  sem_type |=  not_nullable_flag(arg->sem->sem_type);

  name_ast->sem = ast->sem = new_sem(sem_type);
}

static void sem_func_trim(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // trim can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  // one or two args
  if (arg_count != 1 && !sem_validate_arg_count(ast, arg_count, 2)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = arg_count == 1 ? NULL : second_arg(arg_list);

  if (!is_text(arg1->sem->sem_type) || (arg2 && !is_text(arg2->sem->sem_type))) {
    report_error(ast, "CQL0085: all arguments must be strings", name);
    record_error(ast);
    return;
  }

  // type text, not null if arg1 is not null
  sem_t sem_type = SEM_TYPE_TEXT | (arg1->sem->sem_type & SEM_TYPE_NOTNULL);

  // add sensitivity if either is sensitive
  sem_type |= (arg1->sem->sem_type & SEM_TYPE_SENSITIVE);
  sem_type |= arg2 ? (arg2->sem->sem_type & SEM_TYPE_SENSITIVE) : 0;

  name_ast->sem = ast->sem = new_sem(sem_type);

  // preserve the string kind of the main arg, otherwise no kind checks needed for trim
  ast->sem->kind = arg1->sem->kind;
}

// ltrim has the same semantics as trim
static void sem_func_ltrim(ast_node *ast, uint32_t arg_count) {
  sem_func_trim(ast, arg_count);
}

// rtrim has the same semantics as trim
static void sem_func_rtrim(ast_node *ast, uint32_t arg_count) {
  sem_func_trim(ast, arg_count);
}

static void sem_func_nullif(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // nullif can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 2)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);

  if (!sem_verify_compat(arg1, arg1->sem->sem_type, arg2->sem->sem_type, "NULLIF")) {
    return;
  }

  sem_combine_kinds(arg2, arg1->sem->kind);
  if (is_error(arg2)) {
    record_error(ast);
    return;
  }

  // nullif will be the same type as arg1, sensitivity preserved; nullability
  // added because nullif() can (obviously) return NULL
  name_ast->sem = ast->sem = new_sem(arg1->sem->sem_type & sem_not(SEM_TYPE_NOTNULL));
  ast->sem->kind = arg1->sem->kind;

  // ast->sem->name is not set here because e.g. nullif(x) is not named "x"
}

static void sem_func_instr(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // instr can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 2)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);

  if (!is_text(arg1->sem->sem_type) || !is_text(arg2->sem->sem_type)) {
    report_error(ast, "CQL0085: all arguments must be strings", name);
    record_error(ast);
    return;
  }

  // instr() is integer type, sensitivity, nullability preserved;
  sem_t combine = combine_flags(arg1->sem->sem_type, arg2->sem->sem_type);
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_INTEGER | combine);

  // ast->sem->name is not set here because e.g. instr(x) is not named "x"
  // the kind of instr is generic, the integer returned has no kind even if the strings do
}

static void sem_func_sign(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  if (!is_numeric_expr(arg)) {
    report_error(name_ast, "CQL0082: argument must be numeric", name);
    record_error(ast);
    return;
  }

  if (enforcement.strict_sign_function) {
    if (CURRENT_EXPR_CONTEXT_IS_NOT(SEM_EXPR_CONTEXT_NONE)) {
      report_error(
        ast,
        "CQL0452: function may not be used in SQL because it is not supported on old versions of SQLite",
        name
      );
      record_error(ast);
      return;
    }
  }

  sem_t sem_type = arg->sem->sem_type;

  sem_t combined_flags = not_nullable_flag(sem_type) | sensitive_flag(sem_type);

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_INTEGER | combined_flags);
}

static void sem_func_abs(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  if (!is_numeric_expr(arg)) {
    report_error(name_ast, "CQL0082: argument must be numeric", name);
    record_error(ast);
    return;
  }

  // abs() will be the same type as arg, sensitivity, nullability preserved;
  name_ast->sem = ast->sem = arg->sem;

  // ast->sem->name is not set here because e.g. abs(x) is not named "x"

  // preserve the kind of the arg
  ast->sem->kind = arg->sem->kind;
}

static void sem_func_char(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // char can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  if (arg_count == 0) {
    sem_validate_arg_count(ast, arg_count, 1);
    return;
  }

  sem_t sensitive = SEM_TYPE_NULL;
  ast_node *arg;
  do {
    arg = first_arg(arg_list);
    sem_t sem_type = arg->sem->sem_type;

    if (!is_integer(sem_type)) {
      report_error(ast, "CQL0317: char function arguments must be integer", name);
      record_error(ast);
      return;
    }
    sensitive |= sensitive_flag(sem_type);
  } while ((arg_list = arg_list->right));

  // char() will always return a string, sensitivity param is preserved.
  // char return null if params doesn't have a character representation
  // of the unicode code point values of integers table
  // e.g: select char(1) -> NULL; select char(67); -> "C"
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT | sensitive);

  // ast->sem->name is not set here because e.g. char(x) is not named "x"
  // the result has no 'kind'
}

// Validate the variable argument is a auto cursor. This is called to validate
// cql_cursor_diff_xxx(X,Y) arguments.
static bool_t sem_validate_cursor_from_variable(ast_node *ast, CSTR target) {
  if (is_variable(ast->sem->sem_type)) {
    sem_cursor(ast);
    return !is_error(ast);
  }

  report_error(ast, "CQL0341: argument must be a variable in function", target);
  record_error(ast);
  return false;
}

// The attest notnull family are CQL builtin functions that return a value of a
// nonnull type when given a nullable value, either after some runtime check is
// performed (in the case of ifnull_throw and ifnull_crash, which are used
// directly by the programmer), or not (in the case of cql_inferred_notnull, which
// will only show up as the product of rewrite rules).
static void sem_func_attest_notnull(ast_node *ast, uint32_t arg_count, uint32_t valid_contexts) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_context(ast, name, valid_contexts)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  sem_t sem_type = arg1->sem->sem_type;

  if (is_null_type(sem_type) || is_not_nullable(sem_type)) {
    report_error(arg1, "CQL0344: argument must be a nullable type (but not constant NULL) in", name);
    record_error(ast);
    return;
  }

  ast->sem = arg1->sem;
  sem_add_flags(ast, SEM_TYPE_NOTNULL); // note this makes a copy
  name_ast->sem = ast->sem;
}

// uses attest notnull semantic helper
static void sem_func_ifnull_throw(ast_node *ast, uint32_t arg_count) {
  sem_func_attest_notnull(ast, arg_count, SEM_EXPR_CONTEXT_NONE);

  // "throw" implies that we have a return code which implies all of the proc
  // things as surely as if we had used the database.  We need to be a proc
  // with a result code.
  has_dml = 1;
}

// uses attest notnull semantic helper
static void sem_func_ifnull_crash(ast_node *ast, uint32_t arg_count) {
  sem_func_attest_notnull(ast, arg_count, SEM_EXPR_CONTEXT_NONE);
}

// Special function that tells us the expression as been already verified to be not null
// due to control flow or other context.
static void sem_special_func_cql_inferred_notnull(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate) {
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT_NOTNULL(arg_list, call_arg_list->right);

  // Since we're checking a call to `cql_inferred_notnull`, its arguments have
  // already been rewritten and we don't want to do it again. Setting
  // `is_analyzing_notnull_rewrite` prevents that.
  is_analyzing_notnull_rewrite = true;
  sem_arg_list(arg_list, IS_NOT_COUNT);
  is_analyzing_notnull_rewrite = false;

  // Our argument is just a reference to something already analyzed previously,
  // so we could not have possibly failed.
  Invariant(!is_error(arg_list));

  // This compiles to nothing for SQLite so we can allow all contexts.
  sem_func_attest_notnull(ast, arg_count, SEM_EXPR_CONTEXT_FLAGS);
  Invariant(ast->sem->sem_type & SEM_TYPE_INFERRED_NOTNULL);
  // Prevent this from propagating needlessly to keep --ast clean.
  ast->sem->sem_type &= u64_not(SEM_TYPE_INFERRED_NOTNULL);
}

// validate expression with cql_cursor_diff_xxx func is semantically correct.
// cql_cursor_diff_xxx is a CQL builtin function that compare the values of a
// row between two cursors.
// Note cql_cursor_diff_xxx is also rewritten to a case_expr node
static bool_t validate_cql_cursor_diff(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 2)) {
    return false;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);

  if (!sem_validate_cursor_from_variable(arg1, name) ||
      !sem_validate_cursor_from_variable(arg2, name)) {
    record_error(ast);
    return false;
  }

  // We've already validated that the two argument are variables for auto cursor. We just
  // need to validate their shapes are identical
  sem_struct *sptr1 = arg1->sem->sptr;
  sem_struct *sptr2 = arg2->sem->sptr;

  if (sptr1->count != sptr2->count) {
    report_error(ast, "CQL0342: cursor arguments must have identical column count", name);
    record_error(ast);
    return false;
  }

  if (!is_auto_cursor(arg1->sem->sem_type) || !is_auto_cursor(arg2->sem->sem_type)) {
    EXTRACT_STRING(arg1_name, arg1);
    EXTRACT_STRING(arg2_name, arg2);
    CSTR cursor_name = !is_auto_cursor(arg1->sem->sem_type) ? arg1_name : arg2_name;
    report_error(arg1, "CQL0067: cursor was not used with 'fetch [cursor]'", cursor_name);
    record_error(arg1);
    record_error(ast);
    return false;
  }

  // we're making sure the two argument cursors have the same shape, because we can
  // only do diffing with cursors with the same shape.
  CSTR target = dup_printf("in %s", name);
  sem_verify_identical_columns(arg1, arg2, target);
  if (is_error(arg2)) {
    record_error(ast);
    return false;
  }

  // the function always return a string which is the name of the first column in the
  // cursors that are different otherwise null.
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT);

  return true;
}

static void sem_func_cql_cursor_diff_col(ast_node *ast, uint32_t arg_count) {
  if (!validate_cql_cursor_diff(ast, arg_count)) {
    return;
  }

  // We have a cql_cursor_diff_col function call, we rewrite the node to
  // a case_expr node.
  rewrite_cql_cursor_diff(ast, true);
}

static void sem_func_cql_cursor_diff_val(ast_node *ast, uint32_t arg_count) {
  if (!validate_cql_cursor_diff(ast, arg_count)) {
    return;
  }

  // We have a cql_cursor_diff_val function call, we rewrite the node to
  // a case_expr node.
  rewrite_cql_cursor_diff(ast, false);
}

// This is a special function because we do not want to analyze the arguments
// until after the rewrite to a CASE expression.
static void sem_special_func_iif(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate) {
  Contract(is_ast_call(ast));

  if (!sem_validate_arg_count(ast, arg_count, 3)) {
    return;
  }

  // We have the right number of arguments, so we proceed to rewrite the AST to
  // a (possibly semantically invalid) case_expr node, then analyze it.
  rewrite_iif(ast);
  sem_case(ast, IS_IIF);
}

static void sem_func_upper(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // upper can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);
  if (!is_text(arg->sem->sem_type)) {
    report_error(name_ast, "CQL0086: first argument must be a string in function", name);
    record_error(ast);
    return;
  }

  // upper() will be the same type as arg, sensitivity, nullability, and kind preserved;
  ast->sem = arg->sem;
  sem_add_flags(ast, 0);    // no flags added this is a clone
  name_ast->sem = ast->sem;

  ast->sem->name = NULL;    // applying upper/lower loses the name, SQlite doesn't recognize lower(foo) as foo
}

// lower has the same rules as upper
static void sem_func_lower(ast_node *ast, uint32_t arg_count) {
  return sem_func_upper(ast, arg_count);
}

static void sem_func_coalesce(ast_node *ast, uint32_t arg_count) {
  return sem_coalesce(ast, 0);  // do not set "ifnull"
}

// This is the common part of sum and total, we just verify that
// we're dealing with numerics, we are in the right context,
// and have one arg.  Note sum with more than one arg is not supported
// in SQLite, so there's no "non-aggregate" case like min/max.
static void sem_validate_sum_or_total(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_aggregate_context(ast)) {
    return;
  }

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  // you can sum any numeric
  if (!is_numeric_expr(arg) || is_ast_null(arg)) {
    report_error(name_ast, "CQL0083: argument must be numeric", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Sum validation -> any numeric is ok
static void sem_aggr_func_sum(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  sem_validate_sum_or_total(ast, arg_count);
  if (is_error(ast)) {
    return;
  }

  // Sum has slightly different validation than total because of the weird
  // rules about sum over zero rows. e.g. even "select sum(1) where 0"
  // gives null.

  ast_node *arg = first_arg(arg_list);

  // This will give us a sensitive result if the argument is sensitive
  // the output will always be nullable (because SEM_TYPE_INTEGER is nullable)
  // the result will be at least integer sized, but bigger if the argument
  // is long integer or real.
  sem_t result = sem_combine_types(SEM_TYPE_INTEGER, arg->sem->sem_type);

  // set the result type accordingly
  name_ast->sem = ast->sem = new_sem(result);

  // ast->sem->name is not set here because e.g. sum(x) is not named "x"
}

// Total validation -> any numeric is ok
static void sem_aggr_func_total(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  sem_validate_sum_or_total(ast, arg_count);
  if (is_error(ast)) {
    return;
  }

  // Total has slightly different validation than sum because of the weird
  // rules about sum over zero rows. e.g. even "select sum(1) where 0"
  // gives null but "select total(1) where 0" gives 0.0.  total() always gives
  // a number.

  ast_node *arg = first_arg(arg_list);

  // set the result type accordingly
  name_ast->sem = ast->sem = new_sem(SEM_TYPE_REAL | SEM_TYPE_NOTNULL | sensitive_flag(arg->sem->sem_type));

  // ast->sem->name is not set here because e.g. total(x) is not named "x"
}

// Substr validation -> 2 or 3 args, first arg is a string, the others are integers
static void sem_func_substr(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // two or three args allowed, otherwise fail the test
  if (arg_count < 2 || arg_count > 3) {
    sem_validate_arg_count(ast, arg_count, 2);
    return;
  }

  // substr can only appear inside of SQL
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);
  ast_node *arg3 = (arg_count == 3) ? third_arg(arg_list) : NULL;

  if (!is_text(arg1->sem->sem_type)) {
    report_error(ast, "CQL0086: first argument must be a string in function", name);
    record_error(ast);
    return;
  }

  // the first index can be any numeric
  if (!is_numeric_expr(arg2) || is_ast_null(arg2)) {
    report_error(name_ast, "CQL0083: argument must be numeric", name);
    record_error(ast);
    return;
  }

  // We try to evaluate arg 2 as a constant, if we can do so and if we get zero
  // then the user has made a mistake.  The indices are 1 based.
  eval_node result = EVAL_NIL;
  eval(arg2, &result);
  if (result.sem_type != SEM_TYPE_ERROR && result.sem_type != SEM_TYPE_NULL) {
    eval_cast_to(&result, SEM_TYPE_LONG_INTEGER);
    if (result.int64_value == 0) {
      report_error(arg2, "CQL0406: substr uses 1 based indices, the 2nd argument of substr may not be zero", NULL);
      record_error(arg2);
      record_error(ast);
      return;
    }
  }

  // the second index can be any numeric (if it exists)
  if (arg3) {
    if (!is_numeric_expr(arg3) || is_ast_null(arg3)) {
      report_error(name_ast, "CQL0083: argument must be numeric", name);
      record_error(ast);
      return;
    }
  }

  // The result is nonnull if all arguments are nonnull, and sensitive if any
  // arguments are sensitive.
  sem_t flags = combine_flags(arg1->sem->sem_type, arg2->sem->sem_type);
  if (arg3) {
    flags = combine_flags(flags, arg3->sem->sem_type);
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT | flags);
  // applying substr loses the name, SQLite doesn't recognize substr(foo, ..) as foo

  // preserve the string 'kind' even though it's a substring (that seems the most sensible)
  ast->sem->kind = arg1->sem->kind;
}

// Validates SQLite's replace(input, find, replace_with) function.
static void sem_func_replace(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // All three arguments must be provided.
  if (!sem_validate_arg_count(ast, arg_count, 3)) {
    return;
  }

  // The replace function can only appear in SQL.
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  ast_node *input = first_arg(arg_list);
  sem_t input_type = input->sem->sem_type;

  ast_node *find = second_arg(arg_list);
  sem_t find_type = find->sem->sem_type;

  ast_node *replace_with = third_arg(arg_list);
  sem_t replace_with_type = replace_with->sem->sem_type;

  // All arguments must be strings.
  if (!is_text(input_type) || !is_text(find_type) || !is_text(replace_with_type)) {
    report_error(ast, "CQL0085: all arguments must be strings", name);
    record_error(ast);
    return;
  }

  // The result is nonnull if all arguments are nonnull, and sensitive if any
  // arguments are sensitive.
  sem_t flags = combine_flags(input_type, combine_flags(find_type, replace_with_type));

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT | flags);

  // The result has the same kind as the input argument.
  ast->sem->kind = input->sem->kind;
}

// generic function to do basic validation for builtin window functions.
static void sem_validate_window_func(
  ast_node *ast,
  uint32_t arg_count_actual,
  uint32_t arg_count_needed_min,
  uint32_t arg_count_needed_max,
  sem_t sem_func_return) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)

  if (!sem_validate_window_context(ast)) {
    return;
  }

  if (arg_count_actual < arg_count_needed_min || arg_count_actual > arg_count_needed_max) {
    // this will fail, but it generates the error for us nicely
    sem_validate_arg_count(ast, arg_count_needed_max, arg_count_actual);
    return;
  }

  name_ast->sem = ast->sem = new_sem(sem_func_return);
}

// Validation of the builtin window function row_number(...). It takes 0 arguments
static void sem_func_row_number(ast_node *ast, uint32_t arg_count) {
  sem_validate_window_func(ast, arg_count, 0, 0, SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
}

// Validation of the builtin window function rank(...). It takes 0 arguments
static void sem_func_rank(ast_node *ast, uint32_t arg_count) {
  sem_validate_window_func(ast, arg_count, 0, 0, SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
}

// Validation of the builtin window function dense_rank(...). It takes 0 arguments
static void sem_func_dense_rank(ast_node *ast, uint32_t arg_count) {
  sem_validate_window_func(ast, arg_count, 0, 0, SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
}

// Validation of the builtin window function percent_rank(...). It takes 0 arguments
static void sem_func_percent_rank(ast_node *ast, uint32_t arg_count) {
  sem_validate_window_func(ast, arg_count, 0, 0, SEM_TYPE_REAL | SEM_TYPE_NOTNULL);
}

// Validation of the builtin window function cume_dist(...). It takes 0 arguments
static void sem_func_cume_dist(ast_node *ast, uint32_t arg_count) {
  sem_validate_window_func(ast, arg_count, 0, 0, SEM_TYPE_REAL | SEM_TYPE_NOTNULL);
}

// Validation of the builtin window function ntile(...). It takes one integer argument
static void sem_func_ntile(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  sem_validate_window_func(ast, arg_count, 1, 1, SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
  if (is_error(ast)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);
  if (!is_num_int_in_range(arg, 1, INT_MAX)) {
    report_error(ast, "CQL0300: argument must be an integer (between 1 and max integer) in function", name);
    record_error(ast);
    record_error(arg_list);
    return;
  }

  ast->sem->sem_type |= sensitive_flag(arg->sem->sem_type);

  // ast->sem->name is not set here because e.g. ntile(x) is not named "x"
}

// Validation of the builtin window function lag(...). It takes three parameters
// with two of them optional.
static void sem_func_lag(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  sem_t combined_flags = 0;

  sem_validate_window_func(ast, arg_count, 1, 3, SEM_TYPE_OK);
  if (is_error(ast)) {
    return;
  }

  // all args have already had their own semantic check done in sem_expr_call, we don't do it again.
  // we're only going to check how the args relate to each other and compute the final semantic type.
  ast_node *arg1 = first_arg(arg_list);

  if (arg_count > 1) {
    ast_node *arg2 = second_arg(arg_list);
    if (!is_num_int_in_range(arg2, 0, INT_MAX)) {
      report_error(ast, "CQL0301: second argument must be an integer (between 0 and max integer) in function", name);
      record_error(ast);
      record_error(arg_list);
      return;
    }
  }

  if (arg_count > 2) {
    ast_node *arg3 = third_arg(arg_list);

    // Note arg3 is a default value for arg1, to be used if the offset in arg2 results in us going off the
    // end of the partition, so arg1 can't be evaluated.  This means we aren't truly doing an assignment
    // assignment here.  But we are trying to keep the save result type though because if arg3 caused the
    // result type to get bigger (e.g. arg3 is real and arg1 is an integer) that's probably just wrong.
    // But, we are allowing the nullability and sensitivity bits to mix.  So if arg3 is sensitive the
    // entire result becomes sensitive.  And if arg3 is nullable the result becomes nullable.  Note that
    // because arg2 might be out of the buffer the only way to get not nullable is if arg2 is not nullable
    // AND arg3 is not nullable.  This is actually just the normal not null combination logic.  And sensitivity
    // will combine in the normal way too.
    // So what we're about to do here is do the normal assignment checks but suppress the error if arg3 is
    // sensitive and arg1 isn't by pretending arg1 is sensitive. And suppress the error if arg3 is nullable
    // and arg1 isn't by pretending arg3 is not nullable. This leaves us with the numeric compatibility checks
    // and lossy conversions and such.  Note that if arg1 is a real, then arg3 can be an integer, that's fine!
    // Exact type match isn't required and that's what this logic is all about.

    sem_t arg1_effective = arg1->sem->sem_type | SEM_TYPE_SENSITIVE;
    sem_t arg3_effective = arg3->sem->sem_type | SEM_TYPE_NOTNULL;

    bool_t ok = sem_verify_assignment(arg3, arg1_effective, arg3_effective, "arg3 used as default value for arg1");

    // now check the type<kind>

    if (ok) {
      sem_combine_kinds(arg3, arg1->sem->kind);
      ok = !is_error(arg3);
    }

    if (!ok) {
      report_error(ast, "CQL0302: first and third arguments must be compatible in function", name);
      record_error(ast);
      record_error(arg_list);
      return;
    }

    // sensitivity and nullability combine as usual.  Note that if arg1 is nullable and arg3 is not nullable
    // even though it is the default value for arg1 it is NOT USED usless arg1 and offset is outside the window
    // so normal nulls in the window can stay.  As a result this is no coalesce or anything like that. This is
    // just a normal nullability and sensitivity combo.
    combined_flags = combine_flags(arg1->sem->sem_type, arg3->sem->sem_type);
  }
  else {
    // with no default value, we might get nulls if we are out of the window, so notnull is stripped!
    combined_flags = sensitive_flag(arg1->sem->sem_type);
  }

  // we only copy core type to strip extra flag like not nullable. e.g: even though arg1 may
  // not be nullable, lag() should still be nullable unless the third argument is not.
  sem_t type = core_type_of(arg1->sem->sem_type);

  ast->sem = arg1->sem;
  sem_replace_flags(ast, type | combined_flags);
  name_ast->sem = ast->sem;
}

// Validation of the builtin window function lead(...). It takes three parameters
// with two of them optional.
static void sem_func_lead(ast_node *ast, uint32_t arg_count) {
  // semantically lead() is the same as lag() therefore we can use the same code.
  sem_func_lag(ast, arg_count);
}

// Validation of the builtin window function first_value(...). It takes one expression parameter
static void sem_func_first_value(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  sem_validate_window_func(ast, arg_count, 1, 1, SEM_TYPE_OK);
  if (is_error(ast)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);
  ast->sem->sem_type = arg->sem->sem_type;

  ast->sem->kind = arg->sem->kind;
  // ast->sem->name is not set here because e.g. first_value(x) is not named "x"
}

// Validation of the builtin window function last_value(...). It takes one expression parameter
static void sem_func_last_value(ast_node *ast, uint32_t arg_count) {
  sem_func_first_value(ast, arg_count);
}

// Validation of the builtin window function nth_value(...). It takes two parameters
static void sem_func_nth_value(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  sem_validate_window_func(ast, arg_count, 2, 2, SEM_TYPE_OK);
  if (is_error(ast)) {
    return;
  }

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);
  if (!is_num_int_in_range(arg2, 1, INT_MAX)) {
    report_error(ast, "CQL0303: second argument must be an integer between 1 and max integer in function", name);
    record_error(ast);
    record_error(arg_list);
    return;
  }

  sem_t sem_type = arg1->sem->sem_type;
  // we only copy core type to strip extra flag like not null. e.g: even though arg1 may
  // not be nullable, nth_value() should still be nullable.
  ast->sem->sem_type = core_type_of(sem_type) | sensitive_flag(sem_type);

  name_ast->sem->kind = arg1->sem->kind;
  // ast->sem->name is not set here because e.g. nth_value(x) is not named "x"
}

// The group_concat function has an optional seperator, otherwise
// it accepts anything and it results in a string.
static void sem_aggr_func_group_concat(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_aggregate_context(ast)) {
    return;
  }

  if (arg_count == 0 || arg_count > 2) {
    sem_validate_arg_count(ast, arg_count, 2);
    return;
  }

  // Note that group concat should preserve sensitivity but it cannot
  // preserve nullability, because even an non-null text field
  // might group_concat into null.  Example:
  //   select group_concat('not_null') from sqlite_master where 0;
  //   -> NULL
  // i.e. if there are no rows to concat you get NULL not an empty string
  // so we have to assume group_concat might return null.
  // All SQL systems I tested agree on this, it's not even unique to SQLite

  ast_node *arg1 = first_arg(arg_list);
  sem_t sem_type = arg1->sem->sem_type;
  sem_t combined_flags = sensitive_flag(sem_type);

  if (arg_count == 2) {
    sem_t sem_type_2 = second_arg(arg_list)->sem->sem_type;
    if (!is_text(sem_type_2)) {
      report_error(ast, "CQL0084: second argument must be a string in function", name);
      record_error(ast);
      return;
    }
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT | combined_flags);

  // ast->sem->name is not set here because e.g. group_concat(x) is not named "x"
  // group_concat has no kind, leave that null too
}

// All of the date formats are strings until converted to something else.
// Validate the format args, nothing else is really needed.
static void sem_strftime(ast_node *ast, uint32_t arg_count, bool_t has_format, sem_t sem_type) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // strftime can appear reasonably in most places, notably not as a LIMIT or
  // OFFSET; also not supported without a context
  if (!sem_validate_function_context(ast,
                                     SEM_EXPR_CONTEXT_SELECT_LIST |
                                     SEM_EXPR_CONTEXT_ON |
                                     SEM_EXPR_CONTEXT_HAVING |
                                     SEM_EXPR_CONTEXT_WHERE |
                                     SEM_EXPR_CONTEXT_GROUP_BY |
                                     SEM_EXPR_CONTEXT_ORDER_BY |
                                     SEM_EXPR_CONTEXT_CONSTRAINT |
                                     SEM_EXPR_CONTEXT_TABLE_FUNC)) {
    return;
  }

  if (arg_count < 1 + has_format) {
    sem_validate_arg_count(ast, arg_count, 1 + has_format);
    return;
  }

  // All arguments must be strings
  for (ast_node *node = arg_list; node; node = node->right) {
    if (!is_string_compat(first_arg(node)->sem->sem_type)) {
      report_error(ast, "CQL0085: all arguments must be strings", name);
      record_error(ast);
      return;
    }
  }

  // Handling the very special case of strftime('now') as returning not null
  // this is super common.  This is just a format, and the known safe format.
  if (arg_count == 2 && has_format) {
    ast_node *first = first_arg(arg_list);
    ast_node *second = second_arg(arg_list);
    if (is_ast_str(first) && is_ast_str(second)) {
       EXTRACT_STRING(arg1, first);
       EXTRACT_STRING(arg2, second);
       if (!strcmp(arg1, "'%s'") && !strcmp(arg2, "'now'")) {
         // 'now' can't be used in a contraint, not deterministic
         if (!sem_validate_sql_not_constraint(ast)) {
           return;
         }

         sem_type |= SEM_TYPE_NOTNULL;
      }
    }
  }

  // the common special case of just a timestring and it's the 'now' literal
  if (has_format == 0 && arg_count == 1) {
    ast_node *first = first_arg(arg_list);
    if (is_ast_str(first)) {
      EXTRACT_STRING(arg1, first);
      if (!strcmp(arg1, "'now'")) {
        // 'now' can't be used in a contraint, not deterministic
        if (!sem_validate_sql_not_constraint(ast)) {
          return;
        }

        sem_type |= SEM_TYPE_NOTNULL;
      }
    }
  }

  // Without validating a lot of logic for strftime, we must assume that the
  // result is nullable, as any modifier param may render the result NULL.
  name_ast->sem = ast->sem = new_sem(sem_type);
}

static void sem_func_strftime(ast_node *ast, uint32_t arg_count) {
  sem_strftime(ast, arg_count, 1, SEM_TYPE_TEXT);
}

static void sem_func_date(ast_node *ast, uint32_t arg_count) {
  sem_strftime(ast, arg_count, 0, SEM_TYPE_TEXT);
}

static void sem_func_time(ast_node *ast, uint32_t arg_count) {
  sem_strftime(ast, arg_count, 0, SEM_TYPE_TEXT);
}

static void sem_func_datetime(ast_node *ast, uint32_t arg_count) {
  sem_strftime(ast, arg_count, 0, SEM_TYPE_TEXT);
}

static void sem_func_julianday(ast_node *ast, uint32_t arg_count) {
  sem_strftime(ast, arg_count, 0, SEM_TYPE_REAL);
}

// The "ptr" function is used to get the memory address of an object at runtime
// as a LONG INT. This is useful when calling SQLite functions as they are
// unable to deal with objects directly.
static void sem_special_func_ptr(ast_node *ast, uint32_t arg_count, bool_t *is_aggregate) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  PUSH_EXPR_CONTEXT(SEM_EXPR_CONTEXT_TABLE_FUNC);
  sem_arg_list(arg_list, IS_NOT_COUNT);
  if (arg_list && is_error(arg_list)) {
    record_error(ast);
    return;
  }
  POP_EXPR_CONTEXT();

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  // this method is really only interesting for passing pointers around sql stuff
  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL);
}

// The "sensitive" function is used to take something that is
// not sensitive  and have it be treated as sensitive.  This is really
// only needed to get argument types to match in compound select
// statements or other similar situations.
static void sem_func_sensitive(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  // add sensitive
  ast->sem = arg->sem;
  sem_add_flags(ast, SEM_TYPE_SENSITIVE); // note this makes a copy
  name_ast->sem = ast->sem;
}

// The "nullable" function is used to take something that is
// not nullable and have it be treated as nullable.  This is really
// only needed to get argument types to match in compound select
// statements or other similar situations.
static void sem_func_nullable(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  // strip away not null if present, keep other flags (like sensitive)
  ast->sem = arg->sem;
  sem_remove_flags(ast, SEM_TYPE_NOTNULL); // note this makes a copy
  name_ast->sem = ast->sem;
}

// This is a helper method that performs validation for builtin functions that have no arguments
// and only require a database connection. They are fair game in most places and since they take
// no args, not a lot can go wrong.
static bool sem_validate_db_func_with_no_args(ast_node *ast, uint32_t arg_count) {
  has_dml = 1;

  if (!sem_validate_arg_count(ast, arg_count, 0)) {
    return 1;
  }

  // DB functions can appear reasonably in most places, but not for grouping or limiting
  // and not in constraints (not deterministic) (this is for 'changes' and 'last_insert_rowid'
  if (!sem_validate_function_context(ast,
          SEM_EXPR_CONTEXT_SELECT_LIST |
          SEM_EXPR_CONTEXT_ON |
          SEM_EXPR_CONTEXT_WHERE |
          SEM_EXPR_CONTEXT_HAVING |
          SEM_EXPR_CONTEXT_TABLE_FUNC |
          SEM_EXPR_CONTEXT_NONE)) {
            return 1;
  }

  return 0;
}

// The random function gives you a random long_int
static void sem_func_random(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  if (!sem_validate_arg_count(ast, arg_count, 0)) {
    return;
  }

  if (!sem_validate_sql_not_constraint(ast)) {
    return;
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL);
}

// The likely function is a no-op function used for query optimizations.
// It tells the query planner that the given (one) argument is probably a boolean value of `true`.
static void sem_func_likely(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (!sem_validate_arg_count(ast, arg_count, 1)) {
    return;
  }

  if (!sem_validate_appear_inside_sql_stmt(ast)) {
    return;
  }

  ast_node *arg = first_arg(arg_list);

  // the function return type matches the argument type
  ast->sem = arg->sem;
  name_ast->sem = ast->sem;
}

// The changes function is used to get the integer number of rows changed
// by the most recent update/insert/delete.
static void sem_func_changes(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  if (sem_validate_db_func_with_no_args(ast, arg_count)) {
    return;
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
}

// The last_insert_rowid function is used to get the rowid of the most recently inserted row with a rowid.
static void sem_func_last_insert_rowid(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  if (sem_validate_db_func_with_no_args(ast, arg_count)) {
    return;
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL);
}

// Given an already analyzed argument list, an AST to use for reporting errors
// in the format string, the decoded format string itself, and the name of the
// procedure being called, verify that the format string is valid and that the
// arguments provided match up appropriately. Returns `true` if successful, else
// `false`.
static bool_t sem_validate_args_for_format(
  ast_node *arg_list,
  ast_node *format_strlit,
  CSTR format_string,
  CSTR proc_name)
{
  Contract(!arg_list || is_ast_arg_list(arg_list));
  Contract(is_strlit(format_strlit));

  // Allocate space for a `printf_iterator`, then initialize it with
  // `format_strlit` (so any errors in the format string itself will report the
  // location of the string literal) and the decoded format string.
  printf_iterator *iterator = minipool_alloc(ast_pool, (uint32_t)sizeof_printf_iterator);
  printf_iterator_init(iterator, format_strlit, format_string);

  // Iterate over the arguments, checking them against the format string (and
  // validating the format string itself) as we go.
  for (ast_node *arg_item = arg_list; arg_item; arg_item = arg_item->right) {
    sem_t sem_type = printf_iterator_next(iterator);
    if (sem_type == SEM_TYPE_ERROR) {
      return false;
    }
    if (sem_type == SEM_TYPE_OK) {
      report_error(arg_list, "CQL0422: more arguments provided than expected by format string", proc_name);
      return false;
    }
    ast_node *arg = arg_item->left;
    if (!sem_verify_assignment(arg, sem_type, core_type_of(arg->sem->sem_type), proc_name)) {
      return false;
    }
  }

  // We're out of arguments. Verify that we're out of substitutions too and that
  // no errors are lurking later in the format string.
  sem_t sem_type = printf_iterator_next(iterator);
  if (sem_type == SEM_TYPE_ERROR) {
    return false;
  }
  if (sem_type != SEM_TYPE_OK) {
    report_error(arg_list, "CQL0423: fewer arguments provided than expected by format string", proc_name);
    return false;
  }

  return true;
}

// Returns the decoded format string (i.e., the string literal absent the
// surrounding quotes) for initializing a `printf_iterator`.
static CSTR format_string_from_format_strlit(ast_node *format_strlit) {
  Contract(is_ast_str(format_strlit));

  CSTR result;

  EXTRACT_STRING(encoded_format_string, format_strlit);
  CHARBUF_OPEN(format_string_buf);
  cg_decode_string_literal(encoded_format_string, &format_string_buf);
  result = Strdup(format_string_buf.ptr);
  CHARBUF_CLOSE(format_string_buf);

  return result;
}

// The printf function converts its arguments to a string. It must be called
// with a string literal containing the format string as its first argument.
static void sem_func_printf(ast_node *ast, uint32_t arg_count) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (arg_count == 0) {
    // use the standard helper to report the error
    sem_validate_arg_count(ast, arg_count, 1);
    return;
  }

  // printf can appear reasonably in most places, notably not as a LIMIT or OFFSET
  if (!sem_validate_function_context(ast,
        SEM_EXPR_CONTEXT_SELECT_LIST |
        SEM_EXPR_CONTEXT_ON |
        SEM_EXPR_CONTEXT_HAVING |
        SEM_EXPR_CONTEXT_WHERE |
        SEM_EXPR_CONTEXT_GROUP_BY |
        SEM_EXPR_CONTEXT_ORDER_BY |
        SEM_EXPR_CONTEXT_CONSTRAINT |
        SEM_EXPR_CONTEXT_TABLE_FUNC |
        SEM_EXPR_CONTEXT_NONE))
  {
    return;
  }

  // Verify that the first argument is a string literal.
  ast_node *format_strlit = first_arg(arg_list);
  if (!is_strlit(format_strlit)) {
    report_error(ast, "CQL0421: first argument must be a string literal", name);
    record_error(ast);
    return;
  }

  CSTR format_string = format_string_from_format_strlit(format_strlit);

  // Verify that the arguments are appropriate for the format string provided.
  ast_node *args_for_format = arg_list->right;
  if (!sem_validate_args_for_format(args_for_format, format_strlit, format_string, name)) {
    record_error(ast);
    return;
  }

  name_ast->sem = ast->sem = new_sem(SEM_TYPE_TEXT | SEM_TYPE_NOTNULL);

  // We do not require that the types implied by the format string match the
  // types of the arguments exactly -- the former is allowed to be larger than
  // the latter -- but they must match in the generated code when we're using
  // printf outside SQL. Calling `rewrite_printf_inserting_casts_as_needed` will
  // insert casts appropriately such that the types match up exactly.
  if (current_expr_context == SEM_EXPR_CONTEXT_NONE) {
    // We use a static variable to keep track of whether or not we're currently
    // in the process of a rewrite: `rewrite_printf_inserting_casts_as_needed`
    // will call `sem_expr` to validate the rewrite and we don't want to loop
    // forever.
    static bool_t is_rewriting = false;
    if (!is_rewriting) {
      is_rewriting = true;
      rewrite_printf_inserting_casts_as_needed(ast, format_string);
      is_rewriting = false;
    }
  }
}

// Compute the semantic type of each argument, this is minimally necessary
// If any arguments are not internally consistent report that error.
static void sem_validate_args(ast_node *ast, ast_node *arg_list) {
  for (ast_node *item = arg_list; item; item = item->right) {
    EXTRACT_ANY(expr, item->left);
    sem_expr(expr);
    if (is_error(expr)) {
      record_error(ast);
      record_error(arg_list);
      return;
    }
  }
  record_ok(ast);
}

// User defined function, this is an external function
// There are a few things to check:
//  * If this is declared without the select keyword then
//     * we can't use these in SQL, so this has to be a loose expression
//  * If this is declared with the select keyword then
//     * we can ONLY use these in SQL, not in a loose expression
//  * args have to be checked and compatible with formals
static void sem_user_func(ast_node *ast, ast_node *user_func) {
  Contract(is_ast_call(ast));
  Contract(
    is_ast_declare_func_stmt(user_func) ||
    is_ast_declare_select_func_stmt(user_func) ||
    is_ast_declare_select_func_no_check_stmt(user_func));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_NOTNULL(func_params_return, user_func->right);
  EXTRACT(params, func_params_return->left);
  EXTRACT_ANY_NOTNULL(ret_data_type, func_params_return->right);

  if (is_ast_declare_func_stmt(user_func)) {
    if (CURRENT_EXPR_CONTEXT_IS_NOT(SEM_EXPR_CONTEXT_NONE)) {
      report_error(ast, "CQL0088: user function may not appear in the context of a SQL statement", name);
      record_error(ast);
      return;
    }
  }
  else {
    // Must be a select func (is_ast_declare_select_func or is_ast_declare_select_func_no_check) case (verified above)
    if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
      report_error(ast, "CQL0089: user function may only appear in the context of a SQL statement", name);
      record_error(ast);
      return;
    }

    // We don't know if UDF is deterministic or not (we need notation for that at some point)
    // for now forbid UDF in a constraint
    if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_CONSTRAINT)) {
      report_error(ast, "CQL0393: user function cannot appear in a constraint expression ", name);
      record_error(ast);
      return;
    }
  }

  if (is_struct(user_func->sem->sem_type)) {
    report_error(ast, "CQL0395: table valued functions may not be used in an expression context", name);
    record_error(ast);
    return;
  }

  // Skip argument type checking for select_func_no_check
  if (is_ast_declare_select_func_no_check_stmt(user_func)) {
    record_ok(ast);
  } else {
    sem_validate_args_vs_formals(ast, name, arg_list, params, NORMAL_CALL);
  }

  if (is_error(ast)) {
    return;
  }

  ast->sem = ret_data_type->sem;
}

// We're looking for this shape, simple select with just the skeleton
// We already have a helper for most of this...
//  | {select_stmt}: select: { sum: integer }
//    | {select_core_list}: select: { sum: integer }
//    | | {select_core}: select: { sum: integer }
//    |   | {select_expr_list_con}: select: { sum: integer }
//    |     | {select_expr_list}: select: { sum: integer }
//    |       | ....
//    |     | {select_from_etc}:
//    | {select_orderby}
//      | {select_limit}
//        | {select_offset}
bool_t is_no_clause_simple_select(ast_node *select_stmt) {
  // accept only if simple select statement (no WITH variants etc.)
  if (is_ast_select_stmt(select_stmt)) {
    EXTRACT_NOTNULL(select_core_list, select_stmt->left);

    // accept only if not compound select
    if (!select_core_list->right) {
      EXTRACT_NOTNULL(select_core, select_core_list->left);
      EXTRACT_NOTNULL(select_expr_list_con, select_core->right);
      EXTRACT_NOTNULL(select_from_etc, select_expr_list_con->right);
      EXTRACT_NOTNULL(select_orderby, select_stmt->right);

      // no from clause and none of the extras either (WHERE, HAVING, ORDER BY etc.)
      return !select_from_etc->left && !sem_has_extra_clauses(select_from_etc, select_orderby);
    }
  }

  return false;
}

// validate shared fragment call:
//  * target has no errors
//  * target is a shared fragment
//    * target therefore a single select statement
//    * target therefore has no out-arguments
//  * target has no select clauses like FROM etc.
//  * target has one column, it's just a SQL expression
static void sem_validate_expression_fragment(ast_node *ast, ast_node *proc) {
  Contract(is_ast_call(ast));
  Contract(is_proc(proc));

  EXTRACT_STRING(proc_name, get_proc_name(proc));
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_NOTNULL(proc_params_stmts, proc->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);

  // check if we are calling a shared fragment
  uint32_t frag_type = find_proc_frag_type(proc);
  if (frag_type != FRAG_TYPE_SHARED) {
    report_error(ast,
      "CQL0224: a function call to a procedure inside SQL may call "
      "only a shared fragment i.e. @attribute(cql:shared_fragment)", proc_name);
    record_error(ast);
    return;
  }

  Invariant(proc->sem->sptr);
  sem_struct *sptr = proc->sem->sptr;
  if (sptr->count > 1) {
      report_error(ast, "CQL0232: nested select expression must return exactly one column", proc_name);
      record_error(ast);
      return;
  }

  EXTRACT_ANY_NOTNULL(select_stmt, stmt_list->left);

  if (!is_no_clause_simple_select(select_stmt)) {
    report_error(ast, "CQL0450: a shared fragment used like a function must be a simple SELECT with no FROM clause", proc_name);
    record_error(ast);
    return;
  }

  sem_validate_args_vs_formals(ast, proc_name, arg_list, params, NORMAL_CALL);
  if (is_error(ast)) {
    return;
  }

  Invariant(ast->sem->sem_type == SEM_TYPE_OK);

  Invariant(sptr->count == 1);
  ast->sem = new_sem(sptr->semtypes[0]);
  ast->sem->kind = sptr->kinds[0];

  // we don't want the inline-ness to run up the tree so
  // we just put that flag bit on the proc name

  ast->left->sem = new_sem(ast->sem->sem_type);
  *ast->left->sem = *ast->sem;
  ast->left->sem->sem_type |= SEM_TYPE_INLINE_CALL;
}

// Calling a stored procedure as a function
// There are a few things to check:
//  * it has to be a loose expression or else a shared fragment
//    * in this case regular arg matching happens on all arguments
//  * it returns a result set (via out union, out, or select)
//    * all args must match in this case also
//  * scalar proc as func case
//    * the last formal must be an OUT arg and it must be a scalar type
//    * that out arg will be treated as the return value of the "function"
//    * in code-gen we will create a temporary for it, semantic analysis doesn't care
static void sem_proc_as_func(ast_node *ast, ast_node *proc) {
  Contract(is_ast_call(ast));
  Contract(is_proc(proc));

  EXTRACT_STRING(proc_name, get_proc_name(proc));

  // no calling procs that had errors...
  if (is_error(proc)) {
    report_error(ast, "CQL0213: procedure had errors, can't call", proc_name);
    record_error(ast);
    return;
  }

  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // Ensure we have none of these forms.  There is general checking for this
  // later but by checking here we can give a more specific error message
  EXTRACT_NOTNULL(call_filter_clause, call_arg_list->left);
  EXTRACT(distinct, call_filter_clause->left);
  EXTRACT(opt_filter_clause, call_filter_clause->right);

  if (distinct || opt_filter_clause) {
    report_error(ast, "CQL0451: procedure as function call is not compatible with DISTINCT or filter clauses", proc_name);
    record_error(ast);
    return;
  }

  EXTRACT_NOTNULL(proc_params_stmts, proc->right);
  EXTRACT(params, proc_params_stmts->left);

  if (CURRENT_EXPR_CONTEXT_IS_NOT(SEM_EXPR_CONTEXT_NONE)) {
    // evaluation proceeds as an expression fragment, it fails or not
    // either way we're done with it.
    sem_validate_expression_fragment(ast, proc);
    return;
  }

  bool_t result_set_return = has_out_stmt_result(proc) || has_result_set(proc) || has_out_union_stmt_result(proc);

  bool_t validation_type = result_set_return ?  NORMAL_CALL : PROC_AS_FUNC;

  sem_validate_args_vs_formals(ast, proc_name, arg_list, params, validation_type);
  Invariant(ast->sem);  // either an error or a result

  if (result_set_return && !is_error(ast)) {
    // this call will return the result set object
    ast->sem = new_sem(SEM_TYPE_OBJECT | SEM_TYPE_NOTNULL);
    ast->sem->kind = dup_printf("%s SET", proc_name);
  }

  // The call may have mutated any or all of the currently improved globals, so
  // we simply invalidate all of them.
  sem_unset_global_notnull_improvements();

  has_dml |= is_dml_proc(proc->sem->sem_type);
}

// This validates that RAISE is being used in the context of a trigger and that
// it has the correct args.
static void sem_expr_raise(ast_node *ast, CSTR cstr) {
  Contract(is_ast_raise(ast));
  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_ANY(expr, ast->right);

  Contract(flags >= RAISE_IGNORE && flags <= RAISE_FAIL);

  if (!in_trigger) {
    report_error(ast, "CQL0092: RAISE may only be used in a trigger statement", NULL);
    record_error(ast);
    return;
  }

  if (expr) {
    sem_expr(expr);
    if (is_error(expr)) {
      record_error(ast);
      return;
    }

    if (!is_text(expr->sem->sem_type)) {
      report_error(expr, "CQL0093: RAISE 2nd argument must be a string", NULL);
      record_error(ast);
      return;
    }
  }

  ast->sem = new_sem(SEM_TYPE_NULL);
}

// This validates that the call is to one of the functions that we know and
// then delegates to the appropriate shared helper function for that type
// of call for additional validation.
static void sem_expr_call(ast_node *ast, CSTR cstr) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT_NOTNULL(call_filter_clause, call_arg_list->left);
  EXTRACT(distinct, call_filter_clause->left);
  EXTRACT(opt_filter_clause, call_filter_clause->right);
  EXTRACT(arg_list, call_arg_list->right);
  bool_t call_aggr_or_user_def_func = 0;

  // Only aggregated/user defined function can use DISTINCT keyword
  // and filter clause.
  //
  // name( [arg_list] ) filter (where expr)

  if (opt_filter_clause) {
    sem_opt_filter_clause(opt_filter_clause);
    if (is_error(opt_filter_clause)) {
      record_error(ast);
      return;
    }
  }

  // expand any FROM forms in the arg list
  if (!rewrite_shape_forms_in_list_if_needed(arg_list)) {
    record_error(ast);
    return;
  }

  uint32_t arg_count = 0;
  for (ast_node *item = arg_list; item; item = item->right) arg_count++;

  // In any aggregate function that takes a single argument, that argument can be preceded by the keyword DISTINCT
  if (distinct && (arg_count != 1 || is_ast_star(first_arg(arg_list)))) {
    report_error(ast, "CQL0304: DISTINCT may only be used with one explicit argument in an aggregate function", name);
    record_error(ast);
    return;
  }

  // check for functions
  symtab_entry *entry = symtab_find(builtin_funcs, name);
  if (!entry) {
    // check for aggregate functions
    entry = symtab_find(builtin_aggregated_funcs, name);
    if (entry) {
      call_aggr_or_user_def_func = 1;
    }
  }
  if (entry) {
    sem_arg_list(arg_list, IS_NOT_COUNT);
    if (arg_list && is_error(arg_list)) {
      record_error(ast);
      return;
    }
    ((sem_func *)entry->val)(ast, arg_count);
    goto additional_checks;
  }

  // check for special functions which do their own analysis of their arguments
  entry = symtab_find(builtin_special_funcs, name);
  if (entry) {
    ((sem_special_func *)entry->val)(ast, arg_count, &call_aggr_or_user_def_func);
    goto additional_checks;
  }

  // check for user defined functions
  ast_node *user_func = find_func(name);
  if (user_func) {
    sem_user_func(ast, user_func);
    call_aggr_or_user_def_func = 1;
    goto additional_checks;
  }

  ast_node *unchecked_user_func = find_unchecked_func(name);
  if (unchecked_user_func) {
    sem_arg_list(arg_list, IS_NOT_COUNT);
    if (arg_list && is_error(arg_list)) {
      record_error(ast);
      return;
    }

    sem_user_func(ast, unchecked_user_func);
    call_aggr_or_user_def_func = 1;
    goto additional_checks;
  }

  // check for a proc that can be called as a function
  ast_node *proc = find_proc(name);
  if (proc) {
    sem_proc_as_func(ast, proc);
    if (is_error(ast)) {
      return;
    }
    goto additional_checks;
  }

  // check for an attempt to use an unchecked proc in an expression context
  ast_node *unchecked_proc = find_unchecked_proc(name);
  if (unchecked_proc) {
    report_error(name_ast, "CQL0405: procedure of an unknown type used in an expression", name);
    record_error(ast);
    return;
  }

  report_error(name_ast, "CQL0094: function not yet implemented", name);
  record_error(ast);
  return;

additional_checks:
  if (!call_aggr_or_user_def_func) {
    if (distinct) {
      // Only aggregated functions and user defined functions that take one parameter
      // can use DISTINCT keyword e.g: SELECT COUNT(DISTINCT X)
      report_error(ast, "CQL0305: DISTINCT may only be used in function that are aggregated or user defined", name);
      record_error(ast);
      return;
    }

    if (opt_filter_clause) {
      // FILTER clause may only be used in aggregated function, user defined function and window function
      report_error(ast, "CQL0306: FILTER clause may only be used in function that are aggregated or user defined", name);
      record_error(ast);
      return;
    }
  }
}

static void sem_opt_filter_clause(ast_node *ast) {
  Contract(is_ast_opt_filter_clause(ast));
  EXTRACT_NOTNULL(opt_where, ast->left);

  // FILTER ([opt_where])
  sem_opt_where(opt_where);

  ast->sem = opt_where->sem;
}

static void sem_opt_partition_by(ast_node *ast) {
  Contract(is_ast_opt_partition_by(ast));
  EXTRACT_NOTNULL(expr_list, ast->left);

  PUSH_EXPR_CONTEXT(SEM_EXPR_CONTEXT_WHERE);

  // compute semantic type of each expr, reporting errors
  sem_validate_args(ast, expr_list);

  POP_EXPR_CONTEXT();
}

static void sem_opt_frame_spec(ast_node *ast) {
  Contract(is_ast_opt_frame_spec(ast));
  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_NOTNULL(expr_list, ast->right);
  EXTRACT_ANY(left_expr, expr_list->left);
  EXTRACT_ANY(right_expr, expr_list->right);

  int32_t frame_type_flags = flags & FRAME_TYPE_FLAGS;
  int32_t frame_boundary_flags = flags & FRAME_BOUNDARY_FLAGS;
  int32_t frame_boundary_start_flags = flags & FRAME_BOUNDARY_START_FLAGS;
  int32_t frame_boundary_end_flags = flags & FRAME_BOUNDARY_END_FLAGS;
  int32_t frame_exclude_flags = flags & FRAME_EXCLUDE_FLAGS;
  bool_t error = false;

  Contract(frame_type_flags && frame_exclude_flags);
  if (frame_boundary_flags) {
    Contract(!frame_boundary_start_flags && !frame_boundary_end_flags);
    if (left_expr) {
      sem_root_expr(left_expr, SEM_EXPR_CONTEXT_WHERE);
      error = is_error(left_expr);
    }
  }
  else {
    Contract(frame_boundary_start_flags && frame_boundary_end_flags);
    if (left_expr) {
      sem_root_expr(left_expr, SEM_EXPR_CONTEXT_WHERE);
      error |= is_error(left_expr);
    }
    if (right_expr) {
      sem_root_expr(right_expr, SEM_EXPR_CONTEXT_WHERE);
      error |= is_error(right_expr);
    }
  }

  if (error) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

static void sem_window_defn(ast_node *ast) {
  Contract(is_ast_window_defn(ast));
  EXTRACT(opt_partition_by, ast->left);
  EXTRACT_NOTNULL(window_defn_orderby, ast->right);
  EXTRACT(opt_orderby, window_defn_orderby->left);
  EXTRACT(opt_frame_spec, window_defn_orderby->right);
  bool_t error = false;

  if (opt_partition_by) {
    sem_opt_partition_by(opt_partition_by);
    error = is_error(opt_partition_by);
  }
  if (opt_orderby) {
    sem_opt_orderby(opt_orderby);
    error |= is_error(opt_orderby);
  }
  if (opt_frame_spec) {
    sem_opt_frame_spec(opt_frame_spec);
    error |= is_error(opt_frame_spec);
  }

  if (error) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Check whether a window name definition was referenced
static void sem_window_reference(ast_node *ast) {
  Contract(is_ast_window_name_defn(ast));
  EXTRACT_STRING(window_name, ast->left);

  ast_node *select_expr_list_con = ast->parent;
  while (!is_ast_select_expr_list_con(select_expr_list_con)) {
    select_expr_list_con = select_expr_list_con->parent;
  }

  EXTRACT_NOTNULL(select_expr_list, select_expr_list_con->left);
  for (ast_node *item = select_expr_list; item; item = item->right) {
    EXTRACT_NOTNULL(select_expr, item->left);
    EXTRACT_ANY_NOTNULL(any_expr, select_expr->left);
    if (is_ast_window_func_inv(any_expr)) {
      EXTRACT_ANY_NOTNULL(window_name_or_defn, any_expr->right);
      if (is_ast_str(window_name_or_defn)) {
        EXTRACT_STRING(name, window_name_or_defn);
        if (!Strcasecmp(window_name, name)) {
          record_ok(ast);
          return;
        }
      }
    }
  }

  report_error(ast->left, "CQL0296: window name definition is not used", window_name);
  record_error(ast->left);
  record_error(ast);
}

static void sem_window_name_defn(ast_node *ast) {
  Contract(is_ast_window_name_defn(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(window_defn, ast->right);

  // name AS [window_defn]
  sem_window_reference(ast);
  if (is_error(ast)) {
    return;
  }

  sem_window_defn(window_defn);
  if (is_error(window_defn)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

static bool_t sem_window_name_defn_list(ast_node *ast) {
  Contract(is_ast_window_name_defn_list(ast));
  EXTRACT_NOTNULL(window_name_defn, ast->left);
  EXTRACT(window_name_defn_list, ast->right);
  bool_t error = false;

  if (window_name_defn_list) {
    sem_window_name_defn_list(window_name_defn_list);
    error = is_error(window_name_defn_list->left);
  }

  sem_window_name_defn(window_name_defn);
  error |= is_error(window_name_defn);

  return error;
}

static void sem_window_clause(ast_node *ast) {
  Contract(is_ast_window_clause(ast));
  EXTRACT_NOTNULL(window_name_defn_list, ast->left);
  bool_t error =false;

  // WINDOW [window_name_defn_list]
  error = sem_window_name_defn_list(window_name_defn_list);

  if (error) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Check whether a window name was defined.
static void sem_window_name(ast_node *ast) {
  Contract(is_ast_str(ast));
  EXTRACT_STRING(window_name, ast);
  EXTRACT_NOTNULL(window_func_inv, ast->parent);

  ast_node *select_expr_list_con = window_func_inv->parent;
  while (!is_ast_select_expr_list_con(select_expr_list_con)) {
    select_expr_list_con = select_expr_list_con->parent;
  }
  EXTRACT_NOTNULL(select_from_etc, select_expr_list_con->right);
  EXTRACT_NOTNULL(select_where, select_from_etc->right);
  EXTRACT_NOTNULL(select_groupby, select_where->right);
  EXTRACT_NOTNULL(select_having, select_groupby->right);
  EXTRACT(opt_select_window, select_having->right);
  bool_t valid = false;
  if (opt_select_window) {
    EXTRACT_NOTNULL(window_clause, opt_select_window->left);
    EXTRACT_NOTNULL(window_name_defn_list, window_clause->left);
    for (ast_node *item = window_name_defn_list; item; item = item->right) {
      EXTRACT_NOTNULL(window_name_defn, item->left);
      EXTRACT_STRING(name, window_name_defn->left);
      if (!Strcasecmp(window_name, name)) {
        valid = true;
        break;
      }
    }
  }

  if (!valid) {
    report_error(ast, "CQL0295: window name is not defined", window_name);
    record_error(ast);
    return;
  }
  record_ok(ast);
}

static void sem_window_name_or_defn(ast_node *ast) {
  bool_t error = false;

  if (is_ast_str(ast)) {
    sem_window_name(ast);
    error = is_error(ast);
  }
  else {
    Contract(is_ast_window_defn(ast));
    sem_window_defn(ast);
    error = is_error(ast);
  }

  if (error) {
    record_error(ast);
    return;
  }
  record_ok(ast);
}

// This validates that the window function call is to one of the window functions
// that we know and then delegates to the appropriate shared helper function for
// that type of window function call for additional validation. We compute the
// semantic type of all the arguments before we validate the particular function.
static void sem_expr_window_func_inv(ast_node *ast, CSTR cstr) {
  if (enforcement.strict_window_func) {
    report_error(ast, "CQL0312: window function invocation are forbidden if strict window function mode is enabled", NULL);
    record_error(ast);
    return;
  }

  Contract(is_ast_window_func_inv(ast));
  EXTRACT_NOTNULL(call, ast->left);
  EXTRACT_NOTNULL(call_arg_list, call->right);
  EXTRACT_NOTNULL(call_filter_clause, call_arg_list->left);
  EXTRACT(opt_filter_clause, call_filter_clause->right);
  EXTRACT_ANY_NOTNULL(window_name_or_defn, ast->right);

  if (CURRENT_EXPR_CONTEXT_IS_NOT(SEM_EXPR_CONTEXT_SELECT_LIST)) {
    report_error(ast, "CQL0294: window function invocations can only appear in the select list of a select statement", NULL);
    record_error(ast);
    return;
  }

  bool_t error = false;
  // We're making deferentiation between "window" context and "window filter" context because
  // FILTER clause in may only be used with aggregate window functions.
  uint32_t expr_context = opt_filter_clause ? SEM_EXPR_CONTEXT_WINDOW_FILTER : SEM_EXPR_CONTEXT_WINDOW;

  // Set expression context because some function calls may only be valid when called in a window context
  PUSH_EXPR_CONTEXT(expr_context);
  sem_expr_call(call, cstr);
  error = is_error(call);
  POP_EXPR_CONTEXT();

  sem_window_name_or_defn(window_name_or_defn);
  error |= is_error(window_name_or_defn);

  if (error) {
    record_error(ast);
    return;
  }

  ast->sem = call->sem;
}

// A top level expression defines the context for this evaluation.
// There are cases where nesting can happen that changes the context,
// e.g. you can put a nested select in a where clause and that nested select
// could legally have aggregates.  This keeps the stack of contexts.
cql_noexport void sem_root_expr(ast_node *ast, uint32_t expr_context) {
  PUSH_EXPR_CONTEXT(expr_context);
  sem_expr(ast);
  POP_EXPR_CONTEXT();
}

// This is the primary dispatch for all expression types.  We find the
// type of expression and then dispatch to the appropriate helper.  This
//  is also where the leaf types are handled (e.g. literals)
cql_noexport void sem_expr(ast_node *ast) {

  // These are all the expressions there are, we have to find it in this table
  // or else someone added a new expression type and it isn't supported yet.
  symtab_entry *entry = symtab_find(exprs, ast->type);
  Invariant(entry);
  sem_expr_dispatch *disp = (sem_expr_dispatch*)entry->val;
  disp->func(ast, disp->str);
}

// Naming of the an expression can happen in a number of places.  The way
// this is done is that whoever should get the alias remembers themselves
// as the alias target and that person then is renamed.  This is almost always
// someone's sem->name field.
static void sem_as_alias(ast_node *ast, CSTR *alias_target) {
  EXTRACT_STRING(name, ast->left);
  // AS [name]
  if (alias_target) {
    *alias_target = name;
  }
}

// This is a possibly aliased element in the select list.  A "select expression"
// The current joinscope is already set appropriately for this evaluation by
// the caller.  There may be none (if there is no from clause).
static void sem_select_expr(ast_node *ast) {
  Contract(is_ast_select_expr(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(opt_as_alias, ast->right);

  // expr [AS alias]
  sem_root_expr(expr, SEM_EXPR_CONTEXT_SELECT_LIST);

  if (is_error(expr)) {
    ast->sem = expr->sem;
    return;
  }

  Invariant(is_unitary(expr->sem->sem_type));
  ast->sem = new_sem(expr->sem->sem_type);
  ast->sem->name = expr->sem->name;
  ast->sem->kind = expr->sem->kind;

  if (opt_as_alias) {
    sem_as_alias(opt_as_alias, &ast->sem->name);
  }
}

// This validates the select list, getting the type of each element.
// If the select list is the special "*" select list, it must be the only
// element and that is handled with a special helper.
// Otherwise, get each item and validate.  At this point we compute the
// net result type of the select from the select list.
static void sem_select_expr_list(ast_node *ast) {
  Contract(ast);

  if (is_ast_star(ast->left)) {
    // select * from [etc]
    Contract(ast->right == NULL);
    sem_select_star(ast->left);
    ast->sem = ast->left->sem;
    return;
  }

  uint32_t count = 0;
  ast_node *node = ast;
  for (; node; node = node->right) {
    if (is_ast_table_star(node->left)) {
      EXTRACT_NOTNULL(table_star, node->left);
      count += sem_select_table_star_count(table_star);

      if (is_error(table_star)) {
        record_error(ast);
        return;
      }
    }
    else {
      EXTRACT_NOTNULL(select_expr, node->left);
      sem_select_expr(select_expr);

      if (is_error(select_expr)) {
        record_error(ast);
        return;
      }

      count++;
    }
  }

  // Here we make the struct type for this select, by enumerating
  // the types of all the columns and using the aliased name if any.
  sem_struct *sptr = new_sem_struct("select", count);
  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;

  int32_t i = 0;
  for (ast_node *snode = ast; snode; snode = snode->right) {
    if (is_ast_table_star(snode->left)) {
      EXTRACT_NOTNULL(table_star, snode->left);
      i = sem_select_table_star_add(table_star, sptr, i);
    }
    else {
      EXTRACT_NOTNULL(select_expr, snode->left);

      if (select_expr->sem->name) {
        sptr->names[i] = select_expr->sem->name;
      }
      else {
        // If you do "select 1" it has no name.
        sptr->names[i] = "_anon";
      }

      // get the kind if there is one (null is ok) as that means the type has no kind
      sptr->kinds[i] = select_expr->sem->kind;

      sptr->semtypes[i] = select_expr->sem->sem_type;
      i++;
    }
  }

  Invariant(count == i);
}

// Helper function for looking up a table in a table factor context
// This is the normal context where tables are found inside of select
// statements.  We have to search the cte space as well as the normal
// table names.  Note this is not a table alias, but an actual table
// or cte.  So we don't use this for instance to resolve "T1.x" that's
// done by normal name resolution rules.
static ast_node *sem_find_table(CSTR name, ast_node *ast_error) {
  ast_node *table_ast = find_cte(name);
  if (!table_ast) {
    table_ast = find_usable_and_not_deleted_table_or_view(
      name,
      ast_error,
      "CQL0095: table/view not defined");
    if (!table_ast) {
      record_error(ast_error);
    }
    else {
      sem_non_blob_storage_table(ast_error, table_ast);
      if (is_error(ast_error)) {
        return NULL;
      }
    }
  }
  return table_ast;
}

// A table factor is one of three things:
// * a table name (a string)  select * from X
// * a select subquery (select X,Y from..) as T2
// * a list of table references select * from (X, Y, Z)
// Here we dispatch to the appropriate helper for each case.
static void sem_table_or_subquery(ast_node *ast) {
  Contract(is_ast_table_or_subquery(ast));

  EXTRACT_ANY_NOTNULL(factor, ast->left);

  CSTR *alias_target = NULL;

  if (is_ast_str(factor)) {
    // [name]
    EXTRACT_STRING(name, factor);
    ast_node *table_ast = sem_find_table(name, ast);
    if (!table_ast) {
      return;
    }

    sem_node *sem = new_sem(SEM_TYPE_JOIN);
    sem->jptr = sem_join_from_sem_struct(table_ast->sem->sptr);
    ast->sem = factor->sem = sem;
    alias_target = &ast->sem->jptr->names[0];
  }
  else if (is_select_stmt(factor)) {
    // [SELECT ...]
    sem_select(factor);

    if (is_error(factor)) {
      record_error(ast);
      return;
    }

    ast->sem = new_sem(SEM_TYPE_JOIN);
    ast->sem->jptr = sem_join_from_sem_struct(factor->sem->sptr);
    alias_target = &ast->sem->jptr->names[0];
  }
  else if (is_ast_shared_cte(factor)) {
    // [CALL shared_fragment ...]
    sem_shared_cte(factor);

    if (is_error(factor)) {
      record_error(ast);
      return;
    }

    ast->sem = new_sem(SEM_TYPE_JOIN);
    ast->sem->jptr = sem_join_from_sem_struct(factor->sem->sptr);
    alias_target = &ast->sem->jptr->names[0];
  }
  else if (is_ast_table_function(factor)) {
    sem_table_function(factor);

    if (is_error(factor)) {
      record_error(ast);
      return;
    }

    ast->sem = factor->sem;
    alias_target = &ast->sem->jptr->names[0];
  }
  else {
    // this is all that's left...
    sem_query_parts(factor);
    ast->sem = factor->sem;
  }

  EXTRACT(opt_as_alias, ast->right);
  if (opt_as_alias) {
    sem_as_alias(opt_as_alias, alias_target);
  }
}

// When specifying joins, one of the alternatives is to give the shared
// columns in the join e.g. select * from X inner join Y using (a,b).
// This method validates that all the columns are present on both sides of the
// join, that they are unique, and they are comparable.
// The return code tells us if any columns had SENSITIVE data.
static sem_t sem_join_using_columns(ast_node *join, ast_node *join_cond, sem_join *left, sem_join *right) {
  EXTRACT_ANY_NOTNULL(cond_type, join_cond->left);
  EXTRACT_NOTNULL(name_list, join_cond->right);
  Contract(is_ast_using(cond_type));

  sem_t sem_sensitive = 0;

  if (!sem_verify_no_duplicate_names(name_list)) {
    record_error(join_cond);
    record_error(join);
    return false;
  }

  for (ast_node *ast = name_list; ast; ast = ast->right) {
    Contract(is_ast_name_list(ast));
    EXTRACT_STRING(name, ast->left);
    // [name]

    sem_t sem_type_left;
    sem_t sem_type_right;

    // check left (and only the left!)
    {
      PUSH_JOIN_BLOCK();
      PUSH_JOIN(left_join, left);
      if (!sem_find_column_for_name(ast, name)) {
        report_error(ast, "CQL0096: join using column not found on the left side of the join", name);
        record_error(join_cond);
        record_error(join);
      }
      POP_JOIN();
      POP_JOIN();

      if (is_error(join)) {
        return false;
      }

      sem_type_left = ast->sem->sem_type;
      sem_sensitive |= sensitive_flag(sem_type_left);
    }

    // and then right (and only the right!)
    {
      PUSH_JOIN_BLOCK();
      PUSH_JOIN(right_join, right);
      if (!sem_find_column_for_name(ast, name)) {
        report_error(ast, "CQL0097: join using column not found on the right side of the join", name);
        record_error(join_cond);
        record_error(join);
      }
      POP_JOIN();
      POP_JOIN();

      if (is_error(join)) {
        return false;
      }

      sem_type_right = ast->sem->sem_type;
      sem_sensitive |= sensitive_flag(sem_type_right);
    }

    if (core_type_of(sem_type_left) != core_type_of(sem_type_right)) {
      report_error(ast, "CQL0098: left/right column types in join USING(...) do not match exactly", name);
      record_error(join);
      return false;
    }
  }

  join_cond->sem = join->sem;
  return sem_sensitive;
}

// The most explicit join condition is a full expression in an ON clause
// this is like select a,b from X inner join Y on X.id = Y.id;
// The on expression should be something that can be used as a bool
// so any numeric will do.
// The return code tells us if the ON condition used SENSITIVE data.
static sem_t sem_join_cond_on(ast_node *join, ast_node *join_cond) {
  EXTRACT_ANY_NOTNULL(cond_type, join_cond->left);
  EXTRACT_ANY_NOTNULL(expr, join_cond->right);
  Contract(is_ast_on(cond_type));

  PUSH_JOIN(j, join->sem->jptr);
  sem_numeric_expr(expr, cond_type, "ON", SEM_EXPR_CONTEXT_ON);
  POP_JOIN();

  if (is_error(expr)) {
    record_error(join_cond);
    record_error(join);
    return false;
  }

  join_cond->sem = join->sem;
  return expr->sem->sem_type;
}

// A join_clause is an ast notion, it includes logically
// a table_or_subquery on the left
// the join target list on the right, which is a list of join target.
// the join target comprises
//  * the type of join
//  * the join condition (using or ON)
//  * the table factor that is to be joined (see above for legal table factors)
// We have to assemble these things and validate that the parts are all legal.
// This is where the actual join type can be computed.
static void sem_join_target(ast_node *ast) {
  Contract(is_ast_join_target(ast));

  // find the left table reference of the join
  EXTRACT_NOTNULL(join_target_list, ast->parent);
  EXTRACT_ANY_NOTNULL(parent, join_target_list->parent);
  Contract(is_ast_join_clause(parent) || is_ast_join_target_list(parent));
  EXTRACT_ANY_NOTNULL(table_ref, parent->left);
  // if this is the first join_target node under the join_clause node then the
  // left table is a table_or_subquery node otherwise is a join_target node
  Contract(is_ast_table_or_subquery(table_ref) || is_ast_join_target(table_ref));

  EXTRACT_OPTION(join_type, ast->left);
  EXTRACT_NOTNULL(table_join, ast->right);
  EXTRACT_NOTNULL(table_or_subquery, table_join->left);
  sem_table_or_subquery(table_or_subquery);

  if (is_error(table_ref)) {
    return;
  }

  // We will need the result of the join to evaluate the ON clause
  // so compute it aggressively.  We use the join type to modify
  // nullability of the results.
  join_tables(table_ref, table_or_subquery, ast, join_type);

  if (is_error(ast)) {
    return;
  }

  // Now do validation on the join condition, we get different results
  // depending on the factor.
  EXTRACT(join_cond, table_join->right);
  if (join_cond) {
    EXTRACT_ANY_NOTNULL(cond_type, join_cond->left);

    sem_t sem_type = 0;

    if (is_ast_on(cond_type)) {
      sem_type = sem_join_cond_on(ast, join_cond);
    }
    else {
      Contract(is_ast_using(cond_type)); // only other type
      sem_type = sem_join_using_columns(ast, join_cond, table_ref->sem->jptr, table_or_subquery->sem->jptr);
    }

    // We have to mark the entire join result as SENSITIVE if the join condition used SENSITIVE
    if (sensitive_flag(sem_type)) {
      sem_add_flags_to_join(ast->sem->jptr, SEM_TYPE_SENSITIVE);
    }
  }
}

static void sem_table_or_subquery_list(ast_node *ast) {
  Contract(is_ast_table_or_subquery_list(ast));
  EXTRACT_NOTNULL(table_or_subquery, ast->left);
  EXTRACT_ANY(table_or_subquery_list, ast->right);

  sem_table_or_subquery(table_or_subquery);

  if (!table_or_subquery_list) {
    ast->sem = table_or_subquery->sem;
  }
  else {
    if (enforcement.strict_join) {
      report_error(ast, "CQL0263: non-ANSI joins are forbidden if strict join mode is enabled", NULL);
      record_error(ast);
      return;
    }

    sem_table_or_subquery_list(table_or_subquery_list);

    // not really an inner join but that gives the correct nullability
    join_tables(table_or_subquery, table_or_subquery_list, ast, JOIN_INNER);
  }
}

static void sem_join_target_list(ast_node *ast) {
  Contract(is_ast_join_target_list(ast));
  do {
    EXTRACT_NOTNULL(join_target, ast->left);
    EXTRACT_ANY(join_target_list, ast->right);
    sem_join_target(join_target);
    ast = join_target_list;
  } while (ast);
}

// join clause subtree comprises all the join statement of the select statement.
// hold a table_or_subquery node on the left and join target list on the right
static void sem_join_clause(ast_node *ast) {
  Contract(is_ast_join_clause(ast));
  EXTRACT_NOTNULL(table_or_subquery, ast->left);
  EXTRACT_NOTNULL(join_target_list, ast->right);

  sem_table_or_subquery(table_or_subquery);
  sem_join_target_list(join_target_list);

  if (is_error(table_or_subquery) || is_error(join_target_list->left)) {
    record_error(ast);
    return;
  }

  // | |     | | {join_clause}:
  // | |     |   | {table_or_subquery}    #0
  // | |     |   | | ...
  // | |     |   | {join_target_list}     #1
  // | |     |     | {join_target}        #2
  // | |     |     | | ...
  // | |     |     | {join_target_list}   #3
  // | |     |       | {join_target}      #4
  // | |     |       | | ...
  // - join_target_list node #1 compute the join of #2 join_target node and the
  // precedent #0 table_or_subquery node.
  // - join_target_list node #3 compute the join of #4 join_target node and the
  // precedent #1 join_target_list node.
  // - Therefore the last join_target_list node under join_clause subtree compute
  // the join on all the table and the ast->sem value from that node is transfered
  // to join_clause node.
  ast_node * previous_join_target_list = join_target_list;
  while (previous_join_target_list->right) {
    previous_join_target_list = previous_join_target_list->right;
  }
  ast->sem = previous_join_target_list->left->sem;
}

// Whenever you see (X, Y, Z) in the from clause that is an unconstrained join of
// those tables.  Since no join condition is specified there presumably
// there will be something in the WHERE clause later.  This is non-ansi
// legacy join syntax.  You don't get extra nulls so it's like an inner join in
// that regard, it's not an OUTER join as there is no column correlation at all
// it's just the cross product.  INNER join is used here to get the right
// nullabilty result but actually it's not really an inner join in any
// other respect.
static void sem_query_parts(ast_node *ast) {
  Contract(is_ast_table_or_subquery_list(ast) || is_ast_join_clause(ast));
  if (is_ast_table_or_subquery_list(ast)) {
    sem_table_or_subquery_list(ast);
  }
  else {
    Contract(is_ast_join_clause(ast)); // this is all that's left
    sem_join_clause(ast);
  }
}


// Table valued functions can appear anywhere a table is allowed.
// The validation rules are:
// * must be a valid function
// * must return a struct type (i.e. a table-valued-function)
// * must have valid arg expressions
// * arg expressions must match formal parameters
// The name of the resulting table is the name of the function
//  * but it can be aliased later with "AS"
static void sem_table_function(ast_node *ast) {
  Contract(is_ast_table_function(ast));

  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT(arg_list, ast->right);

  // name( [arg_list] )

  // check for user defined functions
  ast_node *user_func = find_func(name);
  ast_node *unchecked_user_func = find_unchecked_func(name);
  user_func = user_func ? user_func : unchecked_user_func;
  if (!user_func) {
    report_error(name_ast, "CQL0250: table-valued function not declared", name);
    record_error(ast);
    return;
  }

  if (!is_struct(user_func->sem->sem_type)) {
    report_error(name_ast, "CQL0249: function is not a table-valued-function", name);
    record_error(ast);
    return;
  }

  Contract(is_ast_declare_select_func_stmt(user_func) || is_ast_declare_select_func_no_check_stmt(user_func));
  EXTRACT_NOTNULL(func_params_return, user_func->right);
  EXTRACT(params, func_params_return->left);

  if (is_ast_declare_select_func_no_check_stmt(user_func)) {
    sem_arg_list(arg_list, IS_NOT_COUNT);
    if (arg_list && is_error(arg_list)) {
      record_error(ast);
      return;
    }
    record_ok(ast);
  } else {
    // SQL Func context is basically the same the ON context but allows for Object types
    PUSH_EXPR_CONTEXT(SEM_EXPR_CONTEXT_TABLE_FUNC);
    sem_validate_args_vs_formals(ast, name, arg_list, params, NORMAL_CALL);
    POP_EXPR_CONTEXT();
  }

  if (is_error(ast)) {
    return;
  }

  sem_node *sem = new_sem(SEM_TYPE_JOIN|SEM_TYPE_TVF);
  sem->jptr = sem_join_from_sem_struct(user_func->sem->sptr);
  sem->jptr->names[0] = name;
  ast->sem = name_ast->sem = sem;
}

// A group-by list is a list of [expression].  These each
// need to be validated.  Note this is a place where the expression context
// changes.
static void sem_groupby_list(ast_node *head) {
  Contract(is_ast_groupby_list(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_groupby_list(ast));
    EXTRACT_NOTNULL(groupby_item, ast->left);
    EXTRACT_ANY_NOTNULL(expr, groupby_item->left);

    sem_root_expr(expr, SEM_EXPR_CONTEXT_GROUP_BY);
    if (is_error(expr)) {
      record_error(head);
      return;
    }
  }

  record_ok(head);
}


// A order-by list is a list of [expression, ASC/DESC].  These each
// need to be validated.  Note this is a place where the expression context
// changes.
static void sem_orderby_list(ast_node *head) {
  Contract(is_ast_orderby_list(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_orderby_list(ast));
    EXTRACT_NOTNULL(orderby_item, ast->left);
    EXTRACT_ANY_NOTNULL(expr, orderby_item->left);

    sem_root_expr(expr, SEM_EXPR_CONTEXT_ORDER_BY);
    if (is_error(expr)) {
      record_error(head);
      return;
    }
  }

  record_ok(head);
}

// Simple numeric expression will do for where;  set a new context.
static void sem_opt_where(ast_node *ast) {
  Contract(is_ast_opt_where(ast));

  // WHERE [ast->left]
  sem_numeric_expr(ast->left, ast, "WHERE", SEM_EXPR_CONTEXT_WHERE);
}

// Simple numeric expression will do for having;  set a new context.
static void sem_opt_having(ast_node *ast) {
  Contract(is_ast_opt_having(ast));

  // HAVING [ast->left]
  sem_numeric_expr(ast->left, ast, "HAVING", SEM_EXPR_CONTEXT_HAVING);
}

// Window clause in the select stmt
static void sem_opt_select_window(ast_node *ast) {
  Contract(is_ast_opt_select_window(ast));
  EXTRACT_NOTNULL(window_clause, ast->left);

  // WINDOW [ast->left]
  sem_window_clause(window_clause);

  ast->sem = window_clause->sem;
}

// The group-by node, if present, simply delegates to the groupby_list helper.
static void sem_opt_groupby(ast_node *ast) {
  Contract(is_ast_opt_groupby(ast));
  EXTRACT_NOTNULL(groupby_list, ast->left);

  // GROUP BY [groupby_list]
  sem_groupby_list(groupby_list);
  if (is_error(groupby_list)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// The order-by node, if present, simply delegates to the orderby_list helper.
static void sem_opt_orderby(ast_node *ast) {
  Contract(is_ast_opt_orderby(ast));
  EXTRACT_NOTNULL(orderby_list, ast->left);

  // ORDER BY [group_list] [opt_asc_desc]
  sem_orderby_list(orderby_list);
  if (is_error(orderby_list)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Limit can be any numeric.  It may not refer to any columns, only variables.
static void sem_opt_limit(ast_node *ast) {
  Contract(is_ast_opt_limit(ast));

  PUSH_JOIN_BLOCK();

  // LIMIT [ast->left]
  sem_numeric_expr(ast->left, ast, "LIMIT", SEM_EXPR_CONTEXT_LIMIT);

  POP_JOIN();
}

// Offset can be any numeric.  It may not refer to any columns, only variables.
static void sem_opt_offset(ast_node *ast) {
  Contract(is_ast_opt_offset(ast));

  PUSH_JOIN_BLOCK();

  // LIMIT [ast->left]
  sem_numeric_expr(ast->left, ast, "OFFSET", SEM_EXPR_CONTEXT_OFFSET);

  POP_JOIN();
}

// The select_from_etc node is the meat of the select statement.  Basically
// all the stuff that starts with FROM.  You can do select 1,2 without going
// here but after that you get all the goodness.  Here we extract each of
// the fragments and pass them along to the appropriate helper.
static void sem_select_from(ast_node *ast) {
  Contract(is_ast_select_from_etc(ast));
  EXTRACT_ANY(query_parts, ast->left);

  // FROM [query_parts]
 if (query_parts) {
    sem_query_parts(query_parts);
    ast->sem = query_parts->sem;
    return;
  }

  // It's ok to have not any query_parts. If none, then it's just "okß
  // e.g. select 1 where 0;
  record_ok(ast);
}

// Do the semantic analysis of ORDER BY ... LIMIT ... OFFSET nodes.
// It also expect that the join table infos are already pushed into the join stack by the callsite.
static bool_t sem_select_orderby(ast_node *ast) {
  Contract(is_ast_select_orderby(ast));
  EXTRACT(opt_orderby, ast->left);
  EXTRACT_NOTNULL(select_limit, ast->right);
  EXTRACT(opt_limit, select_limit->left);
  EXTRACT_NOTNULL(select_offset, select_limit->right);
  EXTRACT(opt_offset, select_offset->left);
  bool_t error = false;

  sem_t sem_sensitive = 0;

  if (opt_orderby) {
    sem_opt_orderby(opt_orderby);
    error |= is_error(opt_orderby);
  }

  // These parts may not refer to columns, they will block the join.
  // Blocking is necessary because this select could be nested in a larger
  // select and we don't want to refer to any of THOSE columns either.

  if (opt_limit) {
    sem_opt_limit(opt_limit);
    error |= is_error(opt_limit);
    sem_sensitive |= sensitive_flag(opt_limit->sem->sem_type);
  }

  if (opt_offset) {
    if (!opt_limit) {
      report_error(opt_offset, "CQL0271: OFFSET clause may only be used if LIMIT is also present", NULL);
      record_error(opt_offset);
      error = true;
    }
    else {
      sem_opt_offset(opt_offset);
      error |= is_error(opt_offset);
      sem_sensitive |= sensitive_flag(opt_offset->sem->sem_type);
    }
  }

  if (sem_sensitive) {
    // Not really a fan of reaching up the tree to set the core list from here
    // but flowing this computation would be pretty complicated and the AST
    // is a fixed shape here (hence the strict extracts) so we'll do this the
    // easy way for now.  If this gets more complicated then this case should
    // be fused with the WHERE and HAVING case and happen at a higher level with
    // some flow.
    EXTRACT_NOTNULL(select_stmt, ast->parent);
    EXTRACT_NOTNULL(select_core_list, select_stmt->left);

    sem_struct *sptr = select_core_list->sem->sptr;
    for (int32_t i = 0; i < sptr->count; i++) {
      sptr->semtypes[i] |= sem_sensitive;
    }
  }

  if (error) {
    record_error(ast);
  }
  return error;
}

static sem_t sem_select_where_etc(ast_node *ast) {
  Contract(is_ast_select_from_etc(ast));

  EXTRACT_NOTNULL(select_where, ast->right);
  EXTRACT(opt_where, select_where->left);
  EXTRACT_NOTNULL(select_groupby, select_where->right);
  EXTRACT(opt_groupby, select_groupby->left);
  EXTRACT_NOTNULL(select_having, select_groupby->right);
  EXTRACT(opt_having, select_having->left);
  EXTRACT(opt_select_window, select_having->right);

  bool_t error = false;
  sem_t sem_sensitive = 0;

  if (opt_where) {
    sem_opt_where(opt_where);
    error |= is_error(opt_where);
  }

  if (opt_groupby) {
    sem_opt_groupby(opt_groupby);
    error |= is_error(opt_groupby);
  }

  if (opt_having && !opt_groupby) {
    error = true;
    report_error(opt_having, "CQL0099: HAVING clause requires GROUP BY clause", NULL);
    record_error(opt_having);
  }

  if (opt_having) {
    // HAVING
    sem_opt_having(opt_having);
    error |= is_error(opt_having);
  }

  if (opt_select_window) {
    // WINDOW
    sem_opt_select_window(opt_select_window);
    error |= is_error(opt_select_window);
  }

  if (error) {
    record_error(ast);
  }
  else {
    if (opt_where) {
      sem_sensitive |= sensitive_flag(opt_where->sem->sem_type);
    }
    if (opt_having) {
      sem_sensitive |= sensitive_flag(opt_having->sem->sem_type);
    }
  }
  return sem_sensitive;
}

// Given a select expression list, return a `sem_struct` containing only the
// aliases present in the expression list with all types set to
// `SEM_TYPE_ALIAS`. This is used by `sem_select_expr_list_con` to catch any
// references to an alias, or references to a column that shadows an alias, from
// a WHERE, GROUP BY, HAVING, or WINDOW clause.
static sem_struct *select_expr_list_alias_sptr(ast_node *select_expr_list) {
  Contract(is_ast_select_expr_list(select_expr_list));

  list_item *aliases = NULL;
  uint32_t alias_count = 0;
  for (ast_node *item = select_expr_list; item; item = item->right) {
    Invariant(is_ast_select_expr_list(item));
    if (is_ast_star(item->left) || is_ast_table_star(item->left)) {
      continue;
    }
    EXTRACT_NOTNULL(select_expr, item->left);
    EXTRACT(opt_as_alias, select_expr->right);
    if (opt_as_alias) {
      add_item_to_list(&aliases, opt_as_alias);
      alias_count++;
    }
  }

  sem_struct *sptr = new_sem_struct("aliases", alias_count);

  for (uint32_t i = 0; aliases; aliases = aliases->next, i++) {
    EXTRACT_NOTNULL(opt_as_alias, aliases->ast);
    EXTRACT_STRING(name, opt_as_alias->left);
    sptr->names[i] = name;
    sptr->semtypes[i] = SEM_TYPE_ALIAS;
  }

  return sptr;
}

// The select ast below the statement starts with this construction node.
// It has the select list and the query_parts.  The query_parts being the
// tail of the select (after the FROM).  Here we simply dispatch the appropriate
// helpers for both of these.  Note that if there is a FROM clause we push
// that joinscope so that evaluations of the select list can use the results of
// the join.  Otherwise you get your parent's chain, or nothing.
static void sem_select_expr_list_con(ast_node *ast) {
  Contract(is_ast_select_expr_list_con(ast));
  EXTRACT_NOTNULL(select_expr_list, ast->left);
  EXTRACT_NOTNULL(select_from_etc, ast->right);
  EXTRACT_ANY(query_parts, select_from_etc->left);
  EXTRACT_NOTNULL(select_where, select_from_etc->right);
  EXTRACT(opt_where, select_where->left);

  bool_t error = false;
  sem_t sem_sensitive = 0;
  sem_join *from_jptr = NULL;

  // Analyze the FROM portion (if it exists).
  sem_select_from(select_from_etc);
  error = is_error(select_from_etc);

  if (!error) {
    from_jptr = select_from_etc->sem->jptr;
    Invariant(from_jptr && query_parts || !from_jptr && !query_parts);

    rewrite_select_expr_list(ast, from_jptr);
    error = is_error(ast);
  }

  // Push a flow context to contain improvements made via the WHERE clause that
  // will be in effect for the SELECT expression list.
  FLOW_PUSH_CONTEXT_NORMAL();

  if (!error) {

    // Analyze the WHERE clause. We first push on the result of
    // `select_expr_list_alias_sptr(select_expr_list)` which is simply all
    // aliases present in `select_expr_list` with a sem_type of
    // `SEM_TYPE_ALIAS`. If a reference to a column in the WHERE, GROUP BY,
    // HAVING, or WINDOW portion of the SELECT resolves to something in
    // `alias_scope`, or resolves to something that shadows something in
    // `alias_scope`, we'll issue an error in `sem_try_resolve_column`.
    //
    // The background here is that SQLite allows the above-mentioned clauses to
    // reference aliases of the SELECT expression list by replacing all such
    // references with the expression to which they refer. It only does this,
    // however, if the alias is not shadowed by something in the FROM. For
    // example, in 'SELECT a + b AS x FROM t WHERE x IS NOT NULL', it's
    // impossible to know if 'WHERE x IS NOT NULL' refers to the alias 'x' or to
    // a column in 't' without first knowing whether 't' has a column 'x':
    //
    // * If there is such a column 't.x' and the programmer meant to refer to
    //   it, the programmer should have instead written 'WHERE t.x IS NOT NULL'
    //   instead for the sake of clarity and we'll issue an error indicating
    //   exactly that.
    //
    // * If there is no such column 't.x' and the programmer meant to refer to
    //   the alias, the programmer should have instead written 'WHERE a + b IS
    //   NOT NULL' (which is what SQLite would normally do for them) and we'll
    //   issue an error about the WHERE clause refering to an alias.
    //
    // It's easy to see why we issue the first error indicating the need for
    // 't.x', but why do we also issue the second error that disallows referring
    // to aliases in WHERE clauses entirely? The reason is that, if we did not,
    // we could not analyze the WHERE clause before we analyzed the SELECT
    // expression list, and we'd therefore have no easy way to set the
    // appropriate nonnull improvements in time. Previous versions of CQL worked
    // around this fact, but they did so at the cost of a fair amount of
    // complexity. Given that most other databases (e.g. PostgreSQL, SQL Server,
    // and MySQL) disallow references to the SELECT expression list from a WHERE
    // clause, disallowing it in CQL seemed a better solution than continuing to
    // accommodate SQLite's above-mentioned quirk. See the user-facing
    // documentation for errors CQL0427 and CQL0428 for further justification.
    PUSH_JOIN(alias_scope, sem_join_from_sem_struct(select_expr_list_alias_sptr(select_expr_list)));
    {
      if (query_parts) {
        PUSH_JOIN(from_scope, from_jptr);
        sem_sensitive = sem_select_where_etc(select_from_etc);
        error = is_error(select_from_etc);
        if (!error && opt_where) {
          EXTRACT_ANY_NOTNULL(where_expr, opt_where->left);
          sem_set_improvements_for_true_condition(where_expr);
        }
        POP_JOIN();
      } else {
        sem_sensitive = sem_select_where_etc(select_from_etc);
        error = is_error(select_from_etc);
        if (!error && opt_where) {
          EXTRACT_ANY_NOTNULL(where_expr, opt_where->left);
          sem_set_improvements_for_true_condition(where_expr);
        }
      }
    }
    POP_JOIN();
  }

  if (!error) {
    // Analyze the SELECT expression list with any applicable improvements from
    // the WHERE clause.
    if (query_parts) {
      PUSH_JOIN(from_scope, from_jptr);
      sem_select_expr_list(select_expr_list);
      POP_JOIN();
    } else {
      sem_select_expr_list(select_expr_list);
    }
    error = is_error(select_expr_list);
  }

  FLOW_POP_CONTEXT_NORMAL();

  if (!error) {
    ast->sem = select_expr_list->sem;

    if (select_level == 1) {
      // This is a top-level select which means it's eligable for alias
      // minification. Leaving `ast->sem->used_symbols` as NULL would
      // incorrectly indicate otherwise, so we set it to an empty symbol table
      // here. See `gen_select_expr` to understand why this is needed.
      symtab *used_symbols = symtab_new();
      add_pending_symtab_free(used_symbols);
      ast->sem->used_symbols = used_symbols;
    }

    if (sem_sensitive) {
      // Propagate sensitivity.
      sem_struct *sptr = ast->sem->sptr;
      for (uint32_t i = 0; i < sptr->count; i++) {
        sptr->semtypes[i] |= sem_sensitive;
      }
    }
  }

  if (error) {
    record_error(ast);
    record_error(select_expr_list);
  }
}

// Semantic analysis of the select_values node.
// * aliases are not allowed.
// * all expressions in the same column should be of compatible types
// * if any exression is senstive its entire column becomes senstive

static void sem_values(ast_node *ast) {
  Contract(is_ast_values(ast));
  EXTRACT(insert_list, ast->left);

  // if there are any FROM C(like shape) thing in the values list, expand them
  // we do the first row of values if there is one... we need this to get the count
  // so that we know that they all match
  if (!rewrite_shape_forms_in_list_if_needed(insert_list)) {
    record_error(ast);
    return;
  }

  uint32_t total_count = 0;
  ast_node *items = insert_list;
  while (items) {
    total_count++;
    items = items->right;
  }
  sem_struct *sptr = new_sem_struct("values", total_count);

  // We're walking through each value list to:
  // 1- validate the value expressions
  // 2- validate/combine the type with the struct type of values clause
  for (ast_node *values = ast; values; values = values->right) {
    EXTRACT_NAMED(values_insert_list, insert_list, values->left);

    if (values_insert_list == NULL) {
      report_error(ast, "CQL0336: select statement with VALUES clause requires a non empty list of values", NULL);
      record_error(ast);
      return;
    }

    // if there are any FROM C(like shape) thing in the values list, expand them
    if (!rewrite_shape_forms_in_list_if_needed(values_insert_list)) {
      record_error(ast);
      return;
    }

    // To compute the type of each column in the VALUES clause we have to
    // visit each node.  As we go along we're going to accumulate the type
    // that best fits what we have seen so far or else produce an error
    // if there is no type that can hold all the values in a column.
    // Once this is done the values clause can be made to look just like a select
    // result including the synthetic column names.

    int32_t values_count = 0;
    ast_node *last_expr = NULL;
    for (ast_node *node = values_insert_list; node; node = node->right) {
      EXTRACT_ANY_NOTNULL(expr, node->left);
      last_expr = expr;

      if (values_count >= total_count) {
        report_error(expr, "CQL0337: the number of columns values for each row should be identical in VALUES clause", NULL);
        record_error(expr);
        record_error(ast);
        return;
      }

      // expr
      sem_root_expr(expr, SEM_EXPR_CONTEXT_SELECT_LIST);
      if (is_error(expr)) {
        record_error(ast);
        return;
      }

      Invariant(is_unitary(expr->sem->sem_type));

      sem_t sem_type = sptr->semtypes[values_count];
      CSTR sem_name = sptr->names[values_count];

      sem_name = dup_printf("column%d", values_count + 1);

      if (sem_type == SEM_TYPE_ERROR) {
        sem_type = expr->sem->sem_type;
      }
      else {
        if (!sem_verify_compat(expr, sem_type, expr->sem->sem_type, "VALUES clause")) {
          record_error(ast);
          return;
        }
        // In a values clause the sem type of column is the combination of compatible sem type of
        // all the values for that column. We've verified the compatibility of the values clause struct type
        // with the column value. Now we need combine both sem type and flags.
        // eg: VALUES (1), (2.2); the values statement has one column with an integer at the first
        // row and real at the second raw. The final sem type of this column should be real.
        sem_type = sem_combine_types(sem_type, expr->sem->sem_type);
      }

      sptr->semtypes[values_count] = sem_type;
      sptr->names[values_count] = sem_name;
      sptr->kinds[values_count] = NULL;
      values_count++;
    }

    Invariant(last_expr);
    if (total_count != values_count) {
      report_error(last_expr, "CQL0337: the number of columns values for each row should be identical in VALUES clause", NULL);
      record_error(last_expr);
      record_error(ast);
      return;
    }
  }

  // Here we make the struct type for this select, by enumerating
  // the types of all the columns and using the aliased name if any.
  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->used_symbols = NULL;
}

// select_core is the core component of the select statement diagram. It comprises
// [SELECT ... FROM ... WHERE ... GROUP BY ...] or [VALUES (...), ...]. It does not
// include [WITH ...], [ORDERBY ... LIMIT OFFSET ...]. Note that most of the clauses
// in a select statement are optional. This function execute the semantic
// anlysis of the select-core component.
static void sem_select_core(ast_node *ast) {
  Contract(is_ast_select_core(ast));

  EXTRACT_ANY(any_select_opts, ast->left);
  EXTRACT_ANY_NOTNULL(select_core_right, ast->right);

  if (is_ast_select_values(any_select_opts)) {
    // VALUES [values]
    Contract(is_ast_values(select_core_right));
    sem_values(select_core_right);
  } else {
    // SELECT [select_opts] [select_expr_list_con]
    // select options not needed for semantic analysis
    Contract(is_ast_select_expr_list_con(select_core_right));
    sem_select_expr_list_con(select_core_right);
  }

  ast->sem = select_core_right->sem;
  has_dml = 1;
}

// Merge two used symbols list into one.
// This is called to merge the symbols used in [select_orderby] to the symbols used
// in [select_core]. If we dont do that then the list of used symbols in a
// select statement will be incomplete and minify_aliases feature (CG_MINIFY_ALIASES)
// won't work correctly
static void sem_add_used_symbols(symtab **used_symbols, symtab *add_symbols) {
  if (*used_symbols == NULL) {
    *used_symbols = add_symbols;
  }
  else if (add_symbols) {
    for (int32_t i = 0; i < add_symbols->capacity; i++) {
      if (add_symbols->payload[i].sym) {
        symtab_add(*used_symbols, add_symbols->payload[i].sym, NULL);
      }
    }
  }
}

// Like sem_select_orderby, but with the restriction that ordering can only be
// specified via indices (e.g., 2) and simple name expressions (e.g, x), not
// arbitrary expressions (e.g., x + y).
static bool_t sem_select_orderby_with_simple_ordering_only(ast_node *ast) {
  Contract(is_ast_select_orderby(ast));

  if (sem_select_orderby(ast)) {
    return 1;
  }

  EXTRACT(opt_orderby, ast->left);
  if (!opt_orderby) {
    return 0;
  }

  EXTRACT_NOTNULL(orderby_list, opt_orderby->left);
  for (ast_node *list = orderby_list; list; list = list->right) {
    Contract(is_ast_orderby_list(list));
    EXTRACT_NOTNULL(orderby_item, list->left);
    EXTRACT_ANY_NOTNULL(expr, orderby_item->left);
    if (is_ast_num(expr)) {
      continue;
    }
    if (is_id(expr)) {
      continue;
    }
    report_error(expr, "CQL0398: compound select cannot be ordered by the result of an expression", NULL);
    record_error(ast);
    return 1;
  }

  return 0;
}

// A select statement in any context, it has the options (which we don't care
// about for semantic analysis) plus the statement itself.
static void sem_select_no_with(ast_node *ast) {
  Contract(is_ast_select_stmt(ast));
  EXTRACT_NOTNULL(select_core_list, ast->left);
  EXTRACT_NOTNULL(select_orderby, ast->right);
  EXTRACT_NOTNULL(select_core, select_core_list->left);
  EXTRACT(select_core_compound, select_core_list->right);

  bool_t error = false;
  symtab *used_symbols = NULL;

  sem_select_core_list(select_core_list);
  if (is_error(select_core_list)) {
    record_error(ast);
    return;
  }

  // The select_core node can be "SELECT" or "VALUES" clause statement.
  Contract(is_ast_select_expr_list_con(select_core->right) || is_ast_values(select_core->right));
  EXTRACT_ANY_NOTNULL(select_core_right, select_core->right);

  Contract(select_core_right->sem);
  Contract(select_core_right->sem->sptr);
  PUSH_JOIN(list_scope, sem_join_from_sem_struct(select_core_right->sem->sptr));
  {
    PUSH_MONITOR_SYMTAB();
    if (select_core_compound || is_ast_values(select_core->right)) {
      // [SELECT ... UNION SELECT ...]
      // For compounded select statement, the [select_orderby] can only reference the columns
      // listed in [select_expr_list] therefore we should not push into the JOIN stack
      // the columns from the table ([select_from_etc]).
      // e.g: SELECT col1, col2 from t1 UNION SELECT col1, col2 FROM t2 ORDER BY t1.col3;
      // You can not reference in ORDER BY statement a column from t1 table that is not
      // listed in the [select_expr_list]. Below is a comand line execution to explain
      // the above
      // ------------------------------------------------------------------------
      // sqlite> create table t1(id int, name text);
      // sqlite> create table t2(id int, name text);
      // sqlite> SELECT id FROM t1 UNION SELECT id from t2 ORDER BY t1.name;
      // Error: 1st ORDER BY term does not match any column in the result set
      // sqlite> SELECT id FROM t1 UNION SELECT id from t2 ORDER BY t1.id;
      // sqlite>
      // ------------------------------------------------------------------------
      // sqlite produce an error on ORDER BY t1.name because column t1.name is not
      // part of the result set of each compounded SELECT statement.
      // This is the reason why we don't push the table(select_from_etc) sem_struct
      // into the stack before semantic analysis of select_orderby ast.
      //
      // [VALUES (...), (...), ...]
      // For select values statement you can not reference the columns listed in [select_insert_list]
      // because they are anonimous. Therefore we should not push into the JOIN stack the columns from
      // [select_insert_list].
      error = sem_select_orderby_with_simple_ordering_only(select_orderby);
    }
    else {
      // [SELECT ...]
      // For non compounded select statement we need both columns in [select_expr_list]
      // and columns in the table [select_from_etc] to accurately validate the [select_orderby]
      // ast because columns from [select_expr_list] and columns in the table [select_from_etc]
      // can be referenced in the [select_orderby] statement
      Contract(is_ast_select_expr_list_con(select_core_right));
      EXTRACT_NOTNULL(select_from_etc, select_core_right->right);

      PUSH_JOIN(from_scope, select_from_etc->sem->jptr);
      error = sem_select_orderby(select_orderby);
      POP_JOIN();
    }
    POP_MONITOR_SYMTAB();
  }
  POP_JOIN();

  if (error) {
    record_error(ast);
    return;
  }
  // merge used_symbols from [select_orderby] to the [select_core] node.
  sem_add_used_symbols(&select_core_list->sem->used_symbols, used_symbols);

  ast->sem = select_core_list->sem;
}

// Compound select statements must have compatible columns and exact name
// match of the columns in the select list.  We enforce that here.
static void sem_select_core_list(ast_node *ast) {
  Contract(is_ast_select_core_list(ast));
  EXTRACT_NOTNULL(select_core, ast->left);
  EXTRACT(select_core_compound, ast->right);

  sem_select_core(select_core);
  if (is_error(select_core)) {
    record_error(ast);
    return;
  }

  // This means the select statement only have one select_core node. which mean
  // select_core_list node is in a non compound select statement
  // e.g: SELECT * FROM table
  if (select_core_compound == NULL) {
    ast->sem = select_core->sem;
    return;
  }

  // This means we have more than one select_core node. Which means select_core_list node
  // is in a compounded select statement
  // e.g: SELECT ... UNION SELECT ...
  EXTRACT_NOTNULL(select_core_list, select_core_compound->right);
  sem_select_core_list(select_core_list);
  if (is_error(select_core_list)) {
    record_error(ast);
    return;
  }

  sem_struct *sptr = sem_unify_compatible_columns(select_core, select_core_list);
  if (!sptr) {
    record_error(ast);
    return;
  }

  sem_add_used_symbols(&select_core->sem->used_symbols, select_core_list->sem->used_symbols);

  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  EXTRACT_OPTION(compound_operator, select_core_compound->left);
  ast->sem->sptr->struct_name = get_compound_operator_name(compound_operator);

  // We have no used symbols yet, but it's still important to set
  // `ast->sem->used_symbols` to `select_core->sem->used_symbols` here otherwise
  // any reference to an aliased column in an ORDER BY would not properly
  // prevent removal of the referenced alias during minification.
  Invariant(!select_core->sem->used_symbols || select_core->sem->used_symbols->count == 0);
  Invariant(!select_core_list->sem->used_symbols || select_core_list->sem->used_symbols->count == 0);
  ast->sem->used_symbols = select_core->sem->used_symbols;
}

// Any select in any context (used when a select appears within another statement)
static void sem_select(ast_node *ast) {
  select_level++;
  if (is_ast_with_select_stmt(ast)) {
    sem_with_select(ast);
  }
  else if (is_ast_explain_stmt(ast)) {
    sem_explain(ast);
  }
  else {
    Contract(is_ast_select_stmt(ast));
    sem_select_no_with(ast);
  }
  select_level--;
}

// Top level statement list processing for select, not that a select statement
// can't appear in other places (such as a nested expression).  This is only for
// select in the context of a statement list.  Others use just 'sem_select'
static void sem_select_stmt(ast_node *stmt) {
   sem_select(stmt);
   sem_update_proc_type_for_select(stmt);
}

// Any explain in any context (used when a explain appears within another statement)
// e.g: declare c cursor for explain query plan ...
static void sem_explain(ast_node *stmt) {
  Contract(is_ast_explain_stmt(stmt) && current_explain_stmt == NULL);
  EXTRACT_OPTION(query_plan, stmt->left);
  EXTRACT_ANY_NOTNULL(sql_stmt, stmt->right);

  current_explain_stmt = stmt;

  // EXPLAIN [explain_op] [explain_target] is only available in dev mode
  if (!options.dev) {
    report_error(stmt, "CQL0292: explain statement is only available in dev mode because its result set may vary between SQLite versions", NULL);
    record_error(stmt);
    goto cleanup;
  }

  if (query_plan != EXPLAIN_QUERY_PLAN) {
    report_error(stmt, "CQL0293: only [EXPLAIN QUERY PLAN ...] statement is supported", NULL);
    record_error(stmt);
    goto cleanup;
  }

  symtab_entry *entry = symtab_find(sql_stmts, sql_stmt->type);
  Contract(entry);
  ((void (*)(ast_node*))entry->val)(sql_stmt);

  if (is_error(sql_stmt)) {
    record_error(stmt);
    goto cleanup;
  }

  // Warning: The data returned by the EXPLAIN QUERY PLAN command is intended for
  // interactive debugging only. The output format may change between SQLite releases.
  // Applications should not depend on the output format of the EXPLAIN QUERY
  // PLAN command.
  // An EXPLAIN QUERY PLAN command returns zero or more rows of four columns each.
  // The column names are "selectid", "order", "from", "detail". The first three columns
  // contain an integer value. The final column, "detail", contains a text value which
  // carries most of the useful information.
  // EXPLAIN QUERY PLAN is most useful on a SELECT statement, but may also be appear with
  // other statements that read data from database tables (e.g. UPDATE, DELETE, INSERT INTO ... SELECT)
  //
  // Because of the above explain statement will only be available in dev mode in CQL.
  // Explain statement statement behave like a statement but does not list explicitely
  // the column result there we have to manually build the sem_struct and sem_join that
  // reflex the exact output of EXPLAIN QUERY PLAN [stmt] statement
  sem_struct *sptr = new_sem_struct("explain_query", 4);
  sptr->semtypes[0] = SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL;
  sptr->names[0] = "iselectid";
  sptr->semtypes[1] = SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL;
  sptr->names[1] = "iorder";
  sptr->semtypes[2] = SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL;
  sptr->names[2] = "ifrom";
  sptr->semtypes[3] = SEM_TYPE_TEXT | SEM_TYPE_NOTNULL;
  sptr->names[3] = "zdetail";

  stmt->sem = new_sem(SEM_TYPE_STRUCT);
  stmt->sem->sptr = sptr;

cleanup:
  Contract(current_explain_stmt != NULL);
  current_explain_stmt = NULL;
}

// Top level statement list processing for explain stmt, note that an explain
// statement can't appear in other places (such as a cursor stmt).  This is only
// for explain in the context of a statement list.  Others use just 'sem_explain'
static void sem_explain_stmt(ast_node *stmt) {
  sem_explain(stmt);
  sem_update_proc_type_for_select(stmt);
}

// This adds a common table expression to the current CTE context.
// select statement must already have been analyzed.  The validations
// are :
//  * CTE name unique
//  * no duplicate columns
//  * number of CTE columns matches number of select columns
//
// The type of the CTE is inferred from the column types of the select.
static void sem_cte_decl(ast_node *ast, ast_node *select_core)  {
  Contract(is_ast_cte_decl(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(name_list, ast->right)

  if (find_cte(name)) {
    report_error(ast, "CQL0100: duplicate common table name", name);
    record_error(ast);
    return;
  }

  if (is_usable_and_not_deleted_table_or_view(name)) {
    report_error(ast, "CQL0437: common table name shadows previously declared table or view", name);
    record_error(ast);
    return;
  }

  if (is_ast_star(name_list)) {
    rewrite_cte_name_list_from_columns(ast, select_core);
    if (is_error(ast)) {
      return;
    }
    name_list = ast->right;
  }

  if (!sem_verify_no_duplicate_names(name_list)) {
    record_error(ast);
    return;
  }

  Invariant(is_struct(select_core->sem->sem_type));
  sem_struct *sptr = new_sem_struct_strip_table_flags(select_core->sem->sptr);

  ast_node *item = name_list;

  for (int32_t i = 0; i < sptr->count; i++) {
    if (!item) {
      report_error(ast, "CQL0101: too few column names specified in common table expression", name);
      record_error(ast);
      return;
    }

    // use the names from the CTE decl rather than the select
    EXTRACT_STRING(col_name, item->left);
    sptr->names[i] = col_name;

    item = item->right;
  }

  if (item) {
    report_error(ast, "CQL0102: too many column names specified in common table expression", name);
    record_error(ast);
    return;
  }

  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->sptr->struct_name = name;

  add_cte(ast);
}

// When accumulating cte info this structure holds the various discoveries and
// provides the callback for any per CTE actions
typedef struct shared_cte_info {
  void *context;
  void (*callback)(void *context, CSTR cte_name, ast_node *cte_decl);

  // information for errors detected during walk
  ast_node *missing_else;
  ast_node *bad_statement_form;
  ast_node *non_select_stmt;
} shared_cte_info;

// Now we get the top level CTE tables out of the target procedure.
// We need to scan the CTEs for entries that use LIKE, those are the table arguments
// we will check the details those later, for now we just need the names
// and the AST.  Note that we previously checked that any duplicate parameter
// names were identically typed. e.g. in the below "source" must be identical
// in both cases.
//
//   if bb == 1 then
//     with source(*) like (select 1 x)
//     select * from source;
//   else
//     with source(*) like (select 1 x)
//     select * from source where x = bb;
//   end if;
static void sem_accumulate_cte_info(ast_node *stmt, shared_cte_info *info)
{
  Contract(is_ast_with_select_stmt(stmt));
  EXTRACT_ANY_NOTNULL(with_prefix, stmt->left)
  EXTRACT(cte_tables, with_prefix->left);

  for (ast_node *ast = cte_tables; ast; ast = ast->right) {
    EXTRACT_NOTNULL(cte_table, ast->left);
    EXTRACT_NOTNULL(cte_decl, cte_table->left);
    EXTRACT_ANY_NOTNULL(cte_body, cte_table->right);

    if (is_ast_like(cte_body)) {
      EXTRACT_STRING(cte_name, cte_decl->left);
      if (info->callback) {
        info->callback(info->context, cte_name, cte_decl);
      }
    }
  }
}

// Walk a statement list inside of a shared fragment
// in all such cases there can only be one statement in the list
// anything else is an error and is dutifully recorded.
static void sem_accumulate_stmt_list(ast_node *ast, shared_cte_info *info) {
  Contract(is_ast_stmt_list(ast));

  // all the statement lists must have exactly one statement
  if (ast->right) {
    info->bad_statement_form = ast->right;
    return;
  }

  // note the representation of statement lists is such that they always have
  // at least one statement, an empty statement list is represented by null
  // statement lists not null statements.
  EXTRACT_ANY_NOTNULL(stmt, ast->left);
  if (is_ast_with_select_stmt(stmt)) {
    sem_accumulate_cte_info(stmt, info);
  }
  else if (!is_select_stmt(stmt)) {
    info->non_select_stmt = stmt;
    return;
  }
}

// The cond_action node is the predicate of an IF/ELSEIF and its statement list
// the statement list must be non-empty.  The expression doesn't contribute
// to the CTEs and is therefore ignored (it's checked elsewhere)
static void sem_accumulate_cond_action(ast_node *ast, shared_cte_info *info) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);
  if (!stmt_list) {
    // empty statement list is not allowed
    info->bad_statement_form = ast;
    return;
  }
  sem_accumulate_stmt_list(stmt_list, info);
}

// Here we simply walk the elseif chain processing each statement list
static void sem_accumulate_elseif_list(ast_node *ast, shared_cte_info *info) {
  Contract(is_ast_elseif(ast));

  while (ast) {
    Contract(is_ast_elseif(ast));
    EXTRACT(cond_action, ast->left);
    sem_accumulate_cond_action(cond_action, info);
    ast = ast->right;
  }
}

// The if statement form has the main cond_action then an optional
// elseif chain and then an optional else node.  The else node is not
// actually optional for shared fragments so we will give an error
// if it is absent.  Otherwise the helpers above descend into the pieces.
// In each case we record the ast_node that should get the error if there is one.
static void sem_accumulate_if_stmt(ast_node *ast, shared_cte_info *info) {
  Contract(is_ast_if_stmt(ast));
  EXTRACT_NOTNULL(cond_action, ast->left);
  EXTRACT_NOTNULL(if_alt, ast->right);
  EXTRACT(elseif, if_alt->left);
  EXTRACT_NAMED(elsenode, else, if_alt->right);

  sem_accumulate_cond_action(cond_action, info);

  if (elseif) {
    sem_accumulate_elseif_list(elseif, info);
  }

  if (elsenode) {
    EXTRACT(stmt_list, elsenode->left);
    if (!stmt_list) {
      info->bad_statement_form = ast;
      return;
    }
    sem_accumulate_stmt_list(stmt_list, info);
  }
  else {
    info->missing_else = ast;
  }
}

// The presence of a migration proc is an indirect declaration of its signature
// make a declare proc for that migration out of thin air and add it to the
// known proc decls as usual so that we can report errors later if the migration
// proc we find does not match.
static bool sem_create_migration_proc_prototype(ast_node *origin, CSTR name)
{
 /*
  {declare_proc_stmt}
  | {proc_name_type}
  | | {name foo}
  | | {int PROC_FLAG_USES_DML}
  | {proc_params_stmts}
 */

  AST_REWRITE_INFO_SET(origin->lineno, origin->filename);

  ast_node *ast_name = new_ast_str(name);
  ast_node *proc_name_flags = new_ast_proc_name_type(ast_name, new_ast_opt(PROC_FLAG_USES_DML));
  ast_node *declare_proc_stmt = new_ast_declare_proc_stmt(proc_name_flags, new_ast_proc_params_stmts(NULL, NULL));

  AST_REWRITE_INFO_RESET();

  sem_declare_proc_stmt(declare_proc_stmt);
  return !is_error(declare_proc_stmt);
}

// The procedure is already known to be of the correct shape
// that is, either one select, or else an if statement with one select
// in each branch. We figure out which case we're in and then accumulate the
// pieces using the callback to tell our caller what we found.
static void sem_accumulate_proc_cte_info(ast_node *create_proc_stmt, shared_cte_info *info) {
  Contract(is_ast_create_proc_stmt(create_proc_stmt));

  EXTRACT_NOTNULL(proc_params_stmts, create_proc_stmt->right);
  EXTRACT_NOTNULL(stmt_list, proc_params_stmts->right);
  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  if (is_ast_with_select_stmt(stmt)) {
    sem_accumulate_cte_info(stmt, info);
  }
  else if (is_ast_if_stmt(stmt)) {
    sem_accumulate_if_stmt(stmt, info);
  }
  // note, it might be a normal select, in which case there is nothing to do.
  // a normal select has no CTE LIKE forms because it has no CTEs.
  // We know it's one of the legal forms by the time we are here.
}

// Save the name of the first table parameter that we find, this is
// used in a context were we just want to know that there are none
// so if we find one that's the error.
static void found_any_table_params_callback(void *context, CSTR name, ast_node *cte_decl) {
  // save the first name we find
  if (!*(CSTR *)context) {
    *(CSTR*)context = name;
  }
}

// Here we ensure that the called shared fragment does not need any table bindings
// because none were provided!
static void sem_shared_fragment_ensure_no_table_binding(
  ast_node *call_stmt,
  ast_node *create_proc_stmt)
{
  Contract(is_ast_create_proc_stmt(create_proc_stmt));

  // the procedure exists, and it is not in an error state (already checked)
  Contract(!is_error(create_proc_stmt));

  // and furthermore it's got a result type, again this is already checked.
  Contract(is_struct(create_proc_stmt->sem->sem_type));

  CSTR cte_name = NULL;
  shared_cte_info info;
  memset(&info, 0, sizeof(info));
  info.callback = found_any_table_params_callback;
  info.context = &cte_name;

  sem_accumulate_proc_cte_info(create_proc_stmt, &info);

  if (cte_name) {
    report_error(call_stmt, "CQL0430: no actual table was provided for the table parameter", cte_name);
    record_error(call_stmt);
  }
}

// Add the cte_decl to the list provided in context but de-duplicate
// we're doing this because we will want this list to know if all of the required table parameters
// are covered by the USING clause of the call.  We will have previously checked that
// any duplicated table parameter names have exactly the same type.
static void make_distinct_table_params_list_callback(void *context, CSTR cte_name, ast_node *cte_decl) {
  list_item **head = (list_item**)context;

  // check for duplicates, ignore any, we only need one copy
  list_item *item = *head;
  while (item) {
    EXTRACT_NAMED_NOTNULL(decl, cte_decl, item->ast);
    EXTRACT_STRING(existing_name, decl->left);
    if (!Strcasecmp(existing_name, cte_name)) {
      break;
    }
    item = item->next;
  }

  // duplicate not found
  if (!item) {
     add_item_to_list(head, cte_decl);
  }
}


// This is a recursive check for any embedded CTEs that have names that will conflict with a given binding
// this can get quite complicated.  Here's an example:
//
// @attribute(cql:shared_fragment)
// create proc too()
// begin
//  with
//    source(*) like (select 1 x),
//    foo(*) as (select * from source)
//    select * from foo;
// end;
//
// @attribute(cql:shared_fragment)
// create proc goo()
// begin
//  with
//    source(*) like (select 1 x),
//    (call too() using source as source)
//    select * from too;
// end;
//
// with foo(*) as (select 1 x)
//   select * from (call goo() using foo as source);
//
// here the call to "goo" must fail because it tries to bind "foo" as source
// and there is a "foo" inside of "too". This is a problem because "goo" calls "too"
// and forwards its "source" formal to too.
//
// To find these we have to recursively walk procedure bindings to get to the deepest
// shared fragment.  Note that we don't have to walk where there is no binding
// nor do we have to walk if the binding does not forward an argument that is provided
// externally.
cql_noexport void sem_check_bound_cte_name_conflict(ast_node *node, binding_info *info) {
  if (is_ast_cte_table(node)) {
    EXTRACT_NOTNULL(cte_decl, node->left);
    EXTRACT_ANY_NOTNULL(cte_body, node->right);

    // this is a proxy node, it isn't a source of conflicts
    // this name will be replaced, it's even ok if the arg name matches the formal name
    if (is_ast_like(cte_body)) {
      return;
    }

    EXTRACT_STRING(cte_name, cte_decl->left);

    if (!Strcasecmp(cte_name, info->actual)) {
      bprintf(info->err, "Procedure '%s' has a different CTE that is also named '%s'\n", info->proc, info->actual);
      bprintf(info->err, "The above originated from CALL %s USING %s AS %s\n", info->proc, info->actual, info->formal);
      return;
    }
  }

  if (is_ast_cte_binding(node)) {
    EXTRACT_STRING(actual, node->left);
    EXTRACT_STRING(formal, node->right);

    // if we are forwarding the table parameter then we have to analyze what's under us.
    // for this analysis we don't use the formal name since that name is itself replaced
    if (!Strcasecmp(info->formal, actual)) {
      binding_info new;
      new.err = info->err;
      new.proc = info->proc_calling;
      new.proc_calling = NULL;
      new.actual = info->actual;
      new.formal = formal;

      ast_node *proc_ast = find_proc(new.proc);
      Invariant(proc_ast);

      sem_check_bound_cte_name_conflict(proc_ast, &new);

      if (info->err->used > 1) {
         bprintf(info->err, "The above originated from CALL %s USING %s AS %s\n", info->proc, info->actual, info->formal);
      }
    }

    // nothing underneath a cte_binding anyway, so either it's an error or we're done, either way.
    return;
  }

  // declare the new info in case we need it
  // this has to be outside of the test below so it survives that block
  binding_info new_info;

  // if we're on a shared CTE usage, then we recurse into the CALL
  if (is_ast_shared_cte(node)) {
    EXTRACT_NOTNULL(call_stmt, node->left);
    EXTRACT(cte_binding_list, node->right);

    EXTRACT_ANY_NOTNULL(name_ast, call_stmt->left);
    EXTRACT_STRING(name, name_ast);

    new_info = *info;
    new_info.proc_calling = name;

    // we're going to recurse with the new info, the calling target is populated now
    info = &new_info;
  }

  // Recurse left and right if there are nodes and no errors already

  if (info->err->used == 1 && ast_has_left(node)) {
    sem_check_bound_cte_name_conflict(node->left, info);
  }

  if (info->err->used == 1 && ast_has_right(node)) {
    sem_check_bound_cte_name_conflict(node->right, info);
  }
}

// Here we ensure that the table binding for any given shared CTE is correct
// This means that the number of table args has to match and the provided names
// have to exist and be compatible with the table parameters. There can be no
// extras and no conflicts.
static void sem_shared_fragment_table_binding(
  ast_node *call_stmt,
  ast_node *create_proc_stmt,
  ast_node *cte_binding_list)
{
  Contract(is_ast_call_stmt(call_stmt));
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  Contract(is_ast_cte_binding_list(cte_binding_list));

  CHARBUF_OPEN(tmp);

  // the procedure exists, and it is not in an error state (already checked)
  Contract(!is_error(create_proc_stmt));

  // and furthermore it's got a result type, again this is already checked.
  Contract(is_struct(create_proc_stmt->sem->sem_type));

  EXTRACT_STRING(proc_name, call_stmt->left);

  symtab *bindings = symtab_new();
  symtab *formals = symtab_new();
  symtab *cols = NULL;
  ast_node *item = NULL;

  for (item = cte_binding_list ; item; item = item->right) {
    EXTRACT_NOTNULL(cte_binding, item->left);
    EXTRACT_STRING(actual, cte_binding->left);
    EXTRACT_STRING(formal, cte_binding->right);

    bool_t added = symtab_add(bindings, formal, cte_binding);
    if (!added) {
      report_error(cte_binding->right, "CQL0428: duplicate binding of table in CALL/USING clause", formal);
      record_error(call_stmt);
      goto cleanup;
    }
  }

  EXTRACT_NOTNULL(proc_params_stmts, create_proc_stmt->right);

  // setup to get the list of unique table parameters required for this call
  list_item *parms_head = NULL;
  shared_cte_info info;
  memset(&info, 0, sizeof(info));
  info.callback = make_distinct_table_params_list_callback;
  info.context = &parms_head;

  sem_accumulate_proc_cte_info(create_proc_stmt, &info);

  if (!parms_head) {
    report_error(cte_binding_list, "CQL0429: called procedure has no table arguments but a USING clause is present", proc_name);
    record_error(call_stmt);
    goto cleanup;
  }

  // We need to scan the table arguments
  // for each one of those we then need to ensure that the of the actual table is compatible
  // with the type of the formal table and that the total number of columns is a match.
  // Note that there cannot be extra columns because if there were that might create ambiguities
  // in the result.

  for (list_item *it = parms_head; it; it = it->next) {
    EXTRACT_NOTNULL(cte_decl, it->ast);

    EXTRACT_STRING(cte_name, cte_decl->left);
    symtab_entry *entry = symtab_find(bindings, cte_name);

    if (!entry) {
      report_error(cte_binding_list, "CQL0430: no actual table was provided for the table parameter", cte_name);
      record_error(call_stmt);
      goto cleanup;
    }

    EXTRACT_NOTNULL(cte_binding, entry->val);
    ast_node *ast_formal = cte_binding->right;
    ast_node *ast_actual = cte_binding->left;
    EXTRACT_STRING(actual, ast_actual);
    EXTRACT_STRING(formal, ast_formal);

    // sanity check, the name matches, we just looked it up...
    Invariant(!strcmp(formal, cte_name));

    bool_t added = symtab_add(formals, cte_name, cte_decl);
    Contract(added); // known to be unique due to previous checks

    // We have to bind the name of the actual table in the current context
    // sem_find_table does exactly this -- this is the code used for a table_ref
    // inside of a from clause
    ast_node *table = sem_find_table(actual, ast_actual);
    if (!table) {
      // errors already reported
      record_error(call_stmt);
      goto cleanup;
    }

    sem_struct *sptr_formals = cte_decl->sem->sptr;
    sem_struct *sptr_actuals = table->sem->sptr;
    ast_formal->sem = cte_decl->sem;
    ast_actual->sem = table->sem;
    sem_add_flags(ast_formal, 0); // forces the sem info to be copied but change nothing
    sem_add_flags(ast_actual, 0); // forces the sem info to be copied but change nothing

    // Both are known to be struct types
    Invariant(sptr_formals);
    Invariant(sptr_actuals);

    if (sptr_formals->count != sptr_actuals->count) {
      report_error(ast_actual, "CQL0432: table provided must have the same number of columns as the table parameter", actual);
      record_error(call_stmt);
      goto cleanup;
    }

    cols = symtab_new();

    for (uint32_t i = 0; i < sptr_actuals->count; i++) {
      // we're adding the address of the name as a surrogate for the index 'i'
      // we can easily undo this to get 'i' back when we look it up
      symtab_add(cols, sptr_actuals->names[i], &sptr_actuals->names[i]);
    }

    for (uint32_t i = 0; i < sptr_formals->count; i++) {
      CSTR name = sptr_formals->names[i];
      symtab_entry *col_entry = symtab_find(cols, name);
      if (!col_entry) {
        CSTR msg = dup_printf(
          "CQL0433: table argument '%s' requires column '%s' but it is missing in provided table",
          formal, name);
        report_error(ast_actual, msg, actual);
        record_error(call_stmt);
        goto cleanup;
      }

      CSTR *pname = col_entry->val;

      uint32_t j = (uint32_t)(pname - &sptr_actuals->names[0]);

      sem_t sem_type_formal = sptr_formals->semtypes[i];
      sem_t sem_type_actual = sptr_actuals->semtypes[j];
      CSTR kformal = sptr_formals->kinds[i];
      CSTR kactual = sptr_actuals->kinds[j];

      if (!sem_verify_assignment(ast_actual, sem_type_formal, sem_type_actual, name)) {
        record_error(call_stmt);
        goto cleanup;
      }

      sem_combine_kinds_general(ast_actual, kformal, kactual);
      if (is_error(ast_actual)) {
        record_error(call_stmt);
        goto cleanup;
      }
    }

    symtab_delete(cols);
    cols = NULL;
  }

  for (item = cte_binding_list ; item; item = item->right) {
    EXTRACT_NOTNULL(cte_binding, item->left);
    EXTRACT_ANY_NOTNULL(ast_formal, cte_binding->right);
    EXTRACT_ANY_NOTNULL(ast_actual, cte_binding->left);

    EXTRACT_STRING(actual, ast_actual);
    EXTRACT_STRING(formal, ast_formal);

    if (!symtab_find(formals, formal)) {
      report_error(ast_formal, "CQL0431: an actual table was provided for a table parameter that does not exist", formal);
      record_error(call_stmt);
      goto cleanup;
    }
  }

  for (item = cte_binding_list ; item; item = item->right) {
    EXTRACT_NOTNULL(cte_binding, item->left);
    EXTRACT_STRING(actual, cte_binding->left);
    EXTRACT_STRING(formal, cte_binding->right);

    binding_info bind_info;
    bind_info.err = &tmp;
    bind_info.proc = proc_name;
    bind_info.proc_calling = NULL;
    bind_info.actual = actual;
    bind_info.formal = formal;

    ast_node *cte_decl = find_cte(actual);

    // if the actual name is not a cte (i.e. it's a global) then it can't conflict
    // because any CTE in our subtree is not allowed to conflict with any global name
    if (!cte_decl) {
      continue;
    }

    EXTRACT_NOTNULL(cte_table, cte_decl->parent);
    EXTRACT_ANY_NOTNULL(cte_body, cte_table->right);

    // If this CTE declares a table parameter then this binding  will be checked when we invoke this
    // shared fragment and an actual value is provided.  It would be meaningless to check if the name of the
    // formal causes a conflict, that name won't be used unless the actual happens to match
    // the formal.  In any case it is the actual parameter that matters.  We have to be in a shared fragment
    // or the like form would be illegal in the first place and we wouldn't be here.
    if (is_ast_like(cte_body)) {
      continue;
    }

    sem_check_bound_cte_name_conflict(create_proc_stmt, &bind_info);

    if (tmp.used > 1) {
      CSTR err_msg =
        dup_printf("CQL0444: this use of the named shared fragment is not legal because of a name conflict '%s'\n%s",
          proc_name,
          tmp.ptr);
      report_error(cte_binding, err_msg, NULL);
      record_error(call_stmt);
      goto cleanup;
    }
  }


cleanup:
  CHARBUF_CLOSE(tmp);

  if (cols) {
    symtab_delete(cols);
  }
  symtab_delete(bindings);
  symtab_delete(formals);
}

// We've found a shared fragment call site, process the fragment
static void sem_shared_cte(ast_node *cte_body) {
  EXTRACT_NOTNULL(call_stmt, cte_body->left);
  EXTRACT(cte_binding_list, cte_body->right);

  bool_t in_shared_fragment_call_saved = in_shared_fragment_call;
  in_shared_fragment_call = true;

  // The semantic info for this kind of call looks just like any other
  // we use the helper directly because this is not a loose call statement
  // but there is no cursor.  We don't want the procedure we are in (if any)
  // to become a result-set procedure.
  sem_call_stmt_opt_cursor(call_stmt, NULL);
  if (is_error(call_stmt)) {
    record_error(cte_body);
    goto cleanup;
  }

  // check if we are calling a shared fragment
  EXTRACT_ANY_NOTNULL(proc_name_ast, call_stmt->left);
  EXTRACT_STRING(proc_name, proc_name_ast);
  ast_node *proc_stmt = find_proc(proc_name);
  uint32_t frag_type = find_proc_frag_type(proc_stmt);
  if (frag_type != FRAG_TYPE_SHARED) {
    report_error(proc_name_ast, "CQL0224: a CALL statement inside SQL may call only a shared fragment i.e. @attribute(cql:shared_fragment)", proc_name);
    record_error(cte_body);
    goto cleanup;
  }

  if (!is_ast_create_proc_stmt(proc_stmt)) {
    report_error(proc_name_ast, "CQL0468: @attribute(cql:shared_fragment) may only be placed on a CREATE PROC statement", proc_name);
    record_error(cte_body);
    goto cleanup;
  }

  if (cte_binding_list) {
    // if there is a binding list we have to ensure the number and type of bindings are correct
    sem_shared_fragment_table_binding(call_stmt, proc_stmt, cte_binding_list);
    if (is_error(call_stmt)) {
      record_error(cte_body);
      goto cleanup;
    }
  }
  else {
    // if there is no binding list we still have to ensure that there are no bindings required
    sem_shared_fragment_ensure_no_table_binding(call_stmt, proc_stmt);
    if (is_error(call_stmt)) {
      record_error(cte_body);
      goto cleanup;
    }
  }

  cte_body->sem = call_stmt->sem;

cleanup:
  in_shared_fragment_call = in_shared_fragment_call_saved;
}

// Here we process the CTE and the select it is associated with:
//  * analyze the select
//    * if it is compound analyze only the first part of the union;
//      that part is not allowed to have recursive mention to the CTE
//  * bind the CTE to the analyzed select
//  * analyze the rest of the select if we only analyzed the first part
static void sem_cte_table(ast_node *ast)  {
  Contract(is_ast_cte_table(ast));
  EXTRACT(cte_decl, ast->left);
  EXTRACT_ANY_NOTNULL(cte_body, ast->right);

  if (is_ast_like(cte_body)) {
    EXTRACT_NAMED_NOTNULL(like_ast, like, cte_body);

    if (!in_shared_fragment || sem_with_depth() > 1) {
      CSTR name = NULL;
      if (current_proc) {
        EXTRACT_STRING(proc_name, current_proc->left);
        name = proc_name;
      }

      report_error(cte_body,
          "CQL0427: LIKE CTE form may only be used inside a shared fragment at the top level"
          " i.e. @attribute(cql:shared_fragment)", name);

      record_error(ast);
      return;
    }

    if (is_id(like_ast->left)) {
      // name alias

      // must be a valid shape
      ast_node *found_shape = sem_find_shape_def_base(like_ast, LIKEABLE_FOR_VALUES);
      if (!found_shape) {
        record_error(ast);
        return;
      }

      // now process the declaration using the types from the select
      sem_cte_decl(cte_decl, found_shape);
      if (is_error(cte_decl)) {
        record_error(ast);
        return;
      }

      ast->sem = cte_decl->sem;
      return;
    }
    else {
      // this is the LIKE (select ..) case, we just evaluate the select as usual
      cte_body = cte_body->left;
    }
  }

  // the simple select form is allowed to be recursive

  if (is_ast_select_stmt(cte_body)) {
    ast_node *select_stmt = cte_body;
    // To handle possible recursive references we check if the cte is being
    // defined by a union or union all.  If it, we create the type information
    // for the CTE from just the top half of the union.  Which must not have
    // recursive references.  Otherwise we use the whole thing.
    EXTRACT_NOTNULL(select_core, select_stmt->left->left);

    // analyze just the top half of the union
    sem_select_core(select_core);
    if (is_error(select_core)) {
      record_error(ast);
      return;
    }

    // now process the declaration using the types from the base select
    sem_cte_decl(cte_decl, select_core);
    if (is_error(cte_decl)) {
      record_error(ast);
      return;
    }

    // at this point the cte is defined, we can analyze the entire select
    // for the CTE.  This allows recursive references other parts of the select.
    // However the type defined for the CTE is provisional, we haven't yet
    // considered the effect of the union on nullability.  But what we have
    // is what we will use for the recurrence.

    sem_select_no_with(select_stmt);
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    // Once this is done we have to revise the semantic type to account for
    // possible nulls in the other branches of the union.  This is the type
    // we will expose to the world.

    // replace the types but not the names!
    cte_decl->sem->sptr->semtypes = select_stmt->sem->sptr->semtypes;
  }
  else if (is_ast_shared_cte(cte_body)) {
    sem_shared_cte(cte_body);
    if (is_error(cte_body)) {
      record_error(ast);
      return;
    }

    // now process the declaration using the types from call
    sem_cte_decl(cte_decl, cte_body);
    if (is_error(cte_decl)) {
      record_error(ast);
      return;
    }
  }
  else {
    // all the other forms are treated directly like a "local view"
    // which is basically what a CTE is.  No special processing of the top half etc.

    ast_node *select_stmt = cte_body;

    sem_select(select_stmt);
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    // now process the declaration using the types from the select
    sem_cte_decl(cte_decl, select_stmt);
    if (is_error(cte_decl)) {
      record_error(ast);
      return;
    }
  }

  ast->sem = cte_decl->sem;
}

// Walk the list of CTE tables in the WITH clause and set up each one.
static void sem_cte_tables(ast_node *head)  {
  Contract(is_ast_cte_tables(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    sem_cte_table(ast->left);
    if (is_error(ast->left)) {
      record_error(ast);
      record_error(head);
      return;
    }
  }

  record_ok(head);
}

// This tells us how deeply nested we are in CTE expressions at the moment
static uint32_t sem_with_depth() {
  uint32_t depth = 0;
  cte_state *head = cte_cur;
  while (head) {
    depth++;
    head = head->prev;
  }
  return depth;
}

// Add a new set of tables to the stack
// Needed because WITH statements can be nested due to nested selects
// So there can be multiple scopes within one select statement.
static void sem_push_cte_state() {
  cte_state *new_state = _ast_pool_new(cte_state);

  new_state->prev = cte_cur;
  new_state->ctes = symtab_new();
  cte_cur = new_state;
}

// Remove this CTE from the stack
static void sem_pop_cte_state() {
  Contract(cte_cur);
  Contract(cte_cur->ctes);

  symtab_delete(cte_cur->ctes);
  cte_cur = cte_cur->prev;

  // the CTE state is pool allocated so we don't have to free it
}

// Set up a new CTE context, chaining to the previous one (in case of
// nested selects) and then do semantic analysis of the select that
// was scoped by the WITH.
static void sem_with_select(ast_node *ast) {
  Contract(is_ast_with_select_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT(cte_tables, with_prefix->left);
  EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

  sem_push_cte_state();

  sem_cte_tables(cte_tables);
  if (is_error(cte_tables)) {
    record_error(ast);
    goto cleanup;
  }

  sem_select(select_stmt);
  if (is_error(select_stmt)) {
    record_error(ast);
    goto cleanup;
  }

  ast->sem = select_stmt->sem;

cleanup:
  sem_pop_cte_state();
}

// top level with stmt
static void sem_with_select_stmt(ast_node *stmt) {
  Contract(is_ast_with_select_stmt(stmt));
  Invariant(cte_cur == NULL);
  sem_select(stmt);
  sem_update_proc_type_for_select(stmt);
  Invariant(cte_cur == NULL);
}

// Here we check the view found in the "previous" schema against the current schema.
// There are several validations we have to do here:
//  * the view should be present (but maybe marked with @delete)
//  * the view must not be a table now
//  * the view has to have a compatible create version
//  * the view has to have a compatible delete version
//  * the view create flags (like TEMP, or IF NOT EXISTS) must be the same
static void sem_validate_previous_view(ast_node *prev_view) {
  Contract(!current_joinscope);

  Contract(is_ast_create_view_stmt(prev_view));
  EXTRACT_OPTION(prev_flags, prev_view->left);
  EXTRACT_NAMED(prev_view_and_attrs, view_and_attrs, prev_view->right);
  EXTRACT_NAMED(prev_name_and_select, name_and_select, prev_view_and_attrs->left);
  EXTRACT_ANY_NOTNULL(prev_name_ast, prev_name_and_select->left);
  EXTRACT_STRING(name, prev_name_ast);

  bool_t is_temp = !! (prev_flags & VIEW_IS_TEMP);

  ast_node *ast = find_table_or_view_even_deleted(name);

  if (!ast) {
    if (is_temp || options.schema_exclusive) {
      // temp view totally deleted -> that's ok
      // In exclusive mode views  are bulk deleted, so no tombstones are needed
      return;
    }
  }

  if (!ast) {
    report_error(prev_view, "CQL0104: view was present but now it does not exist (use @delete instead)", name);
    record_error(prev_view);
    return;
  }

  if (is_ast_create_table_stmt(ast)) {
    report_error(ast, "CQL0105: object was a view but is now a table", name);
    record_error(prev_view);
    record_error(ast);
    return;
  }

  enqueue_pending_region_validation(prev_view, ast, name);
}

// Here we check the trigger found in the "previous" schema against the current schema.
// There are several validations we have to do here:
//  * the trigger should be present (but maybe marked with @delete)
//  * the trigger has to have a compatible delete version
//  * the create flags (like TEMP, or IF NOT EXISTS) must be the same
static void sem_validate_previous_trigger(ast_node *prev_trigger) {
  Contract(!current_joinscope);
  Contract(is_ast_create_trigger_stmt(prev_trigger));

  EXTRACT_OPTION(prev_flags, prev_trigger->left);
  EXTRACT_NAMED_NOTNULL(prev_trigger_body_vers, trigger_body_vers, prev_trigger->right);
  EXTRACT_NAMED_NOTNULL(prev_trigger_def, trigger_def, prev_trigger_body_vers->left);
  EXTRACT_ANY_NOTNULL(prev_trigger_name_ast, prev_trigger_def->left);
  EXTRACT_STRING(name, prev_trigger_name_ast);

  bool_t is_temp = !! (prev_flags & TRIGGER_IS_TEMP);

  ast_node *ast = find_trigger(name);
  if (!ast) {
    if (is_temp || options.schema_exclusive) {
      // Temp totally deleted -> that's ok, they always go away
      // In exclusive mode triggers are bulk deleted, so no tombstones are needed
      return;
    }

    // If the table the trigger was on is going away then we don't need
    // to verify that the trigger has a tombstone.  In fact is it not
    // possible to declare the tombstone now because the table name is not
    // valid.  There's no need for the tombstone anyway because when the
    // table is deleted all its triggers will also be deleted.

    EXTRACT_NAMED_NOTNULL(prev_trigger_condition, trigger_condition, prev_trigger_def->right);
    EXTRACT_NAMED_NOTNULL(prev_trigger_op_target, trigger_op_target, prev_trigger_condition->right);
    EXTRACT_NAMED_NOTNULL(prev_trigger_target_action, trigger_target_action, prev_trigger_op_target->right);
    EXTRACT_STRING(prev_table_name, prev_trigger_target_action->left);
    ast_node *ast_table = find_table_or_view_even_deleted(prev_table_name);

    // the table must exist and be affirmatively deleted to avoid the error!
    if (ast_table && ast_table->sem->delete_version > 0) {
      return;
    }

    report_error(prev_trigger, "CQL0106: trigger was present but now it does not exist (use @delete instead)", name);
    record_error(prev_trigger);
    return;
  }

  enqueue_pending_region_validation(prev_trigger, ast, name);
}

// When we locate a table used by a view we simply add that info to the dependency map in both directions
static void sem_found_dep_in_view(CSTR _Nonnull name, ast_node *_Nonnull target_ast, void *_Nullable context) {
  Contract(is_ast_create_table_stmt(target_ast) || is_ast_create_view_stmt(target_ast));
  EXTRACT_NOTNULL(create_view_stmt, context);

  record_table_dependencies(create_view_stmt, target_ast);
}

// Here we peek into the view body and find the tables that it uses.
// We're going to record those so that if a table is unsubscribed we can make sure
// there are no lingering views still using it. We don't have to worry about nested views
// because if the main view uses a nested view and that nested view uses the table then the nested view itself
// will cause an error to be reported.
static void sem_record_view_dependencies(ast_node *ast) {
  Contract(is_ast_create_view_stmt(ast));

  table_callbacks callbacks = {
    .callback_any_table = sem_found_dep_in_view,
    .callback_any_view = sem_found_dep_in_view,
    .callback_context = ast,
    .do_not_recurse_views = true,
  };

  find_table_refs(&callbacks, ast);
}

// Create view analysis is very simple because select does the heavy lifting.  All we
// have to do is validate that the view is unique then validate the select statement.
// The view will be added to the table/view list.
// Views must not be allowed to have any NULL type columns, all nulls must be converted to
// some type with a CAST.
static void sem_create_view_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_view_stmt(ast));
  EXTRACT(view_and_attrs, ast->right);
  EXTRACT(name_and_select, view_and_attrs->left);
  EXTRACT_ANY(attrs, view_and_attrs->right);
  EXTRACT_ANY_NOTNULL(select_stmt, name_and_select->right);
  EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
  EXTRACT_STRING(name, name_ast);

  // if we're validating a previous view we don't want to parse the contents, we only want
  // to verify that this view has resonable name and version markings and so forth.
  // We can't try to analyze the previous version because, like with migration below
  // the view may refer to previous view might refer to now deleted columns and so forth.
  if (validating_previous_schema) {
    // begin in the ok state, validate (which may add errors) and then we're done here
    record_ok(ast);
    sem_validate_previous_view(ast);
    return;
  }

  bool_t adding_current_entity = will_add_current_entity();

  // if there is an existing view, save it here so we can check for duplicates later.
  ast_node *existing_defn = adding_current_entity ? find_table_or_view_even_deleted(name) : NULL;

  // View declarations (i.e. outside of any proc) are totally ignored
  // in the context of a schema migration script.  This prevents us from
  // getting errors because the view refers to tables or columns that are not yet
  // in existence in the version we are migrating.  If you need a view
  // in your migration script you have to create it and use it yourself
  // since you can't rely on the presence of that view during migration anyway.
  if (schema_upgrade_version > 0 && !current_proc) {
    // burn the name, creating a bogus view, views are not allowed to be used in migration scripts anyway
    // we add this stub so that we can produce a superior error if you try to refer to this view
    symtab_add(tables, name, ast);
    // no other processing of semantics
    record_ok(ast);
    return;
  }

  // CREATE [opt_temp] VIEW [name] AS [select_stmt]
  sem_select(select_stmt);
  if (is_error(select_stmt)) {
    record_error(ast);
    return;
  }

  sem_verify_no_anon_no_null_columns(select_stmt);
  if (is_error(select_stmt)) {
    record_error(ast);
    return;
  }

  version_attrs_info vers_info;
  init_version_attrs_info(&vers_info, name, ast, attrs);

  bool_t valid_version_info = sem_validate_version_attrs(&vers_info);
  Invariant(valid_version_info);  // nothing can go wrong with view version info

  if (!sem_validate_vers_ok_in_context(&vers_info)) {
    record_error(ast);
    return;
  }

  Invariant(is_struct(select_stmt->sem->sem_type));
  Invariant(select_stmt->sem->sptr);

  select_stmt->sem->sptr->struct_name = name;
  select_stmt->sem->jptr = sem_join_from_sem_struct(select_stmt->sem->sptr);

  ast->sem = new_sem(select_stmt->sem->sem_type);
  ast->sem->sptr = select_stmt->sem->sptr;
  ast->sem->jptr = select_stmt->sem->jptr;
  ast->sem->sem_type |= vers_info.flags;
  ast->sem->delete_version = vers_info.delete_version;
  ast->sem->region = current_region;

  if (ast->sem->delete_version > 0) {
    ast->sem->sem_type |= SEM_TYPE_DELETED;
  }

  if (existing_defn) {
    if (!sem_validate_identical_ddl(existing_defn, ast)) {
      report_error(name_ast, "CQL0103: duplicate table/view name", name);
      record_error(name_ast);
      record_error(ast);
    }
    return;
  }

  if (adding_current_entity) {
    // deleted or no it goes in the main list
    add_item_to_list(&all_views_list, ast);

    // The name is consumed, some clients will use find_usable_and_not_deleted_table_or_view
    // to not see deleted views (e.g. select) others don't (e.g. drop)
    add_table_or_view(ast);

    // and record the annotation
    sem_record_annotation_from_vers_info(&vers_info);

    // record the tables used by this view (and cross link)
    sem_record_view_dependencies(ast);
  }
}

// Parse out the version attributes for this target for use in the semantic type
// Returns true if all is well, false if there was an error.
static bool_t sem_validate_version_attrs(version_attrs_info *vers_info) {
  Contract(vers_info);
  Contract(vers_info->target_ast);

  for (ast_node *ast = vers_info->attrs_ast; ast; ast = ast->right) {
    if (is_ast_recreate_attr(ast)) {
      // recreate attributes come in exactly this one order; enforced by syntax
      // we get the recreate node and an optional delete, nothing else
      Contract(ast == vers_info->attrs_ast);
      vers_info->recreate = true;
      vers_info->recreate_version_ast = ast;

      if (ast->left) {
        EXTRACT_STRING(group_name, ast->left);
        vers_info->recreate_group_name = group_name;
      }

      // optional delete node is present, it has shape, enforced by parser
      // cons up a fake v1 delete annotation for the vers_info
      if (ast->right) {
        AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
        ast_node *version_annotation = new_ast_version_annotation(new_ast_opt(1), NULL);
        AST_REWRITE_INFO_RESET();

        vers_info->delete_version_ast = version_annotation;
        vers_info->delete_version = 1;
        vers_info->flags |= SEM_TYPE_DELETED;
      }

      // either way, we're done now
      return true;
    }
    if (is_ast_create_attr(ast)) {
      if (!sem_validate_version(vers_info->create_code, ast, &vers_info->create_version, &vers_info->create_proc)) {
        record_error(vers_info->target_ast);
        return false;
      }
      EXTRACT(version_annotation, ast->left);
      vers_info->create_version_ast = version_annotation;
    } else {
      Contract (is_ast_delete_attr(ast));
      if (!sem_validate_version(vers_info->delete_code, ast, &vers_info->delete_version, &vers_info->delete_proc)) {
        record_error(vers_info->target_ast);
        return false;
      }
      EXTRACT(version_annotation, ast->left);
      vers_info->delete_version_ast = version_annotation;
    }
  }

  if (vers_info->delete_version > 0 && vers_info->delete_version <= vers_info->create_version) {
    report_error(vers_info->target_ast, "CQL0107: delete version can't be <= create version", vers_info->name);
    return false;
  }

  if (schema_upgrade_version == -1) {
    if (vers_info->delete_version > 0) {
      vers_info->flags |= SEM_TYPE_DELETED;
    }
  }
  else {
    // The delete version is the version that the column was deleted in.
    // If we are migrating beyond that, the column is already deleted.
    // if were on that version (in a migration context) then you're allowed
    // to look at that column so that you can zero it or some such.
    if (vers_info->delete_version != -1 && schema_upgrade_version > vers_info->delete_version) {
      vers_info->flags |= SEM_TYPE_DELETED;
    }

    // The create version is the version that the column was created in.
    // If we are migrating to a schema before the column was created then we
    // cannot see it yet.
    if (vers_info->create_version != -1 && schema_upgrade_version < vers_info->create_version) {
      vers_info->flags |= SEM_TYPE_DELETED;
    }
  }

  return true;
}

// Ensure that the table parameter is not blob storage, if it is
// then mark an error at the indicated location
static void sem_non_blob_storage_table(ast_node *ast_error, ast_node *ast_table) {
  Contract(ast_error);
  Contract(ast_table);

  if (is_ast_create_table_stmt(ast_table) && is_table_blob_storage(ast_table)) {
    EXTRACT_NOTNULL(create_table_name_flags, ast_table->left);
    EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
    EXTRACT_STRING(name, name_ast);

    report_error(ast_error, "CQL0458: the indicated table may only be used for blob storage", name);
    record_error(ast_error);
    return;
  }

  record_ok(ast_error);
}

// This is the basic checking for the drop table statement
// * the table must exist in some version
// * it has to be a table and not a view
static void sem_drop_table_stmt(ast_node *ast) {
  Contract(is_ast_drop_table_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->right);
  EXTRACT_STRING(name, name_ast);

  // we might be making the dropped table a reality so it's ok to try to drop @deleted tables
  ast_node *table_ast = find_usable_table_or_view_even_deleted(name, name_ast, "CQL0108: table in drop statement does not exist");
  if (!table_ast) {
    record_error(ast);
    return;
  }

  sem_non_blob_storage_table(ast, table_ast);
  if (is_error(ast)) {
    return;
  }

  name_ast->sem = table_ast->sem;

  if (!is_ast_create_table_stmt(table_ast)) {
    report_error(name_ast, "CQL0109: cannot drop a view with drop table", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// This is the basic checking for the drop view statement
// * the view must exist in some version
// * it has to be a view and not a table
static void sem_drop_view_stmt(ast_node *ast) {
  Contract(is_ast_drop_view_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->right);
  EXTRACT_STRING(name, name_ast);

  // we might be making the dropped view a reality so it's ok to try to drop @deleted views
  ast_node *view_ast = find_usable_table_or_view_even_deleted(name, name_ast, "CQL0110: view in drop statement does not exist");
  if (!view_ast) {
    record_error(ast);
    return;
  }

  name_ast->sem = view_ast->sem;

  if (!is_ast_create_view_stmt(view_ast)) {
    report_error(name_ast, "CQL0111: cannot drop a table with drop view", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// This is the basic checking for the drop index statement
// * the index must exist (have been declared) in some version
// * it could be deleted now, that's ok, but the name has to be valid
static void sem_drop_index_stmt(ast_node *ast) {
  Contract(is_ast_drop_index_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->right);
  EXTRACT_STRING(name, name_ast);

  ast_node *index_ast = find_usable_index(name, name_ast, "CQL0112: index in drop statement was not declared");
  if (!index_ast) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// This is the basic checking for the drop trigger statement
// * the trigger must exist (have been declared) in some version
// * it could be deleted now, that's ok, but the name has to be valid
static void sem_drop_trigger_stmt(ast_node *ast) {
  Contract(is_ast_drop_trigger_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->right);
  EXTRACT_STRING(name, name_ast);

  ast_node *trigger_ast = find_usable_trigger(name,
                                              name_ast,
                                              "CQL0113: trigger in drop statement was not declared");
  if (!trigger_ast) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Validate that a delete version is compatible with the previous delete version
static bool_t sem_validate_delete_prev_cur(int32_t prev, int32_t cur) {
  // version numbers (-1, 1 and 2) gives all combos of = > <
  //
  // previous   -1  -1  -1  |   1   1   1  |  2    2   2
  // current    -1   1   2  |  -1   1   2  | -1    1   2
  // result     ok  ok  ok  | err  ok  err | err  err ok
  //
  // so -1 (no versioning) can go to anything (becomes versioned)
  // and any other version must match exactly (can't change version after defined)

  return  prev < 0 || prev == cur;
}

// Validate that a create version is compatible with the previous create version
static bool_t sem_validate_create_prev_cur(int32_t prev, int32_t cur) {
  // version numbers (-1, 1 and 2) gives all combos of = > <
  //
  // previous   -1  -1  -1   |   1   1   1  |  2    2   2
  // current    -1   1   2   |  -1   1   2  | -1    1   2
  // result     ok  err err  | err  ok  err | err  err ok
  //
  // if the table was in the original it has to stay in the original it can't be created later
  // if it was created later it has to stay created later in the same version

  return  prev == cur;
}


// Both null is ok, or exact match of contents is ok.  Only one null or different is not ok.
static bool_t sem_match_optional_string(CSTR prev, CSTR cur) {
  // identical (or both null is ok)
  if (cur == prev) {
    return true;
  }

  // either null is a failure
  if (!cur || !prev) {
    return false;
  }

  return !strcmp(prev, cur);
}

// Looking at two version attribute lists for two different versions of the same
// entity, we validate that they are compatible:
//  * create versions compatible (using sem_validate_create_prev_cur)
//  * delete versions compatible (using sem_validate_delete_prev_cur)
//  * create migration proc identical (both have none or both the same)
//  * delete migration proc identical (both have none or both the same)
//
// Errors are attached here.
static bool_t sem_validate_attrs_prev_cur(version_attrs_info *prev, version_attrs_info *cur, ast_node *name_ast) {
  Contract(name_ast);
  EXTRACT_STRING(name, name_ast);

  bool_t valid = sem_validate_version_attrs(cur);
  Invariant(valid);  // already verified

  valid = sem_validate_version_attrs(prev);
  Invariant(valid);  // already verified

  // Note that it is ok to go from "no plan" to "recreate" so -1 for both is ok
  if (prev->create_version > 0 || prev->delete_version > 0) {
    if (cur->recreate && !prev->recreate) {
      report_error(name_ast, "CQL0114: current schema can't go back to @recreate semantics for", name);
      return false;
    }
  }

  if (prev->recreate) {
    // if we used to be on the @recreate plan then we don't have to check the current create version
    // but we do have to make sure the recreate transition special action is being used
    if (cur->create_version > 0 && cur->create_code == SCHEMA_ANNOTATION_CREATE_TABLE) {
       if (!cur->create_proc || Strcasecmp(CQL_FROM_RECREATE, cur->create_proc)) {
         report_error(name_ast, "CQL0377: table transitioning from @recreate to @create must use @create(nn,cql:from_recreate)", name);
         return false;
       }
    }
  }
  else {
    // otherwise this is not a recreate to create transition so normal version checks
    if (!sem_validate_create_prev_cur(prev->create_version, cur->create_version)) {
      report_error(name_ast, "CQL0115: current create version not equal to previous create version for", name);
      return false;
    }

    // not previously on the recreate plan the proc name must match
    if (!sem_match_optional_string(prev->create_proc, cur->create_proc)) {
      report_error(name_ast, "CQL0118: @create procedure changed in object", name);
      return false;
    }
  }

  if (!sem_validate_delete_prev_cur(prev->delete_version, cur->delete_version)) {
    report_error(name_ast, "CQL0116: current delete version not equal to previous delete version for", name);
    return false;
  }

  // adding a migrate proc when moving to the delete plan is ok
  // if we were already on the delete plan then the migrate proc must match
  if (prev->delete_version > 0) {
    if (!sem_match_optional_string(prev->delete_proc, cur->delete_proc)) {
      report_error(name_ast, "CQL0117: @delete procedure changed in object", name);
      return false;
    }
  }

  return true;
}

// Return the default value from the attribute list
// This must be called when there is a default value by contract.
// This has the side-effect of validating the HAS_DEFAULT flag
cql_noexport ast_node *sem_get_col_default_value(ast_node *attrs) {
  Contract(attrs);

  ast_node *ast = attrs;

  while (!is_ast_col_attrs_default(ast)) {
     ast = ast->right;
  }

  Contract(ast);
  return ast->left;
}

// Validate the the previous and current verision of the schema for a given
// column are compatible.  Note columns can't be re-ordered.  If they are the
// names will appear to not match, that's ok.
// For any given column and its previous version:
//  * the name must match
//  * the type must match (including combined_flags and autoincrement)
//  * @create cannot ever change
//  * @delete can "arrive" but never change
//  * if the column has a default value it has to change
// Any failures produces an error on the column
static void sem_validate_col_def_prev_cur(ast_node *def, ast_node *prev_def, version_attrs_info *cur_info, version_attrs_info *prev_info) {
  // pull out the current column info
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);
  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
  EXTRACT_STRING(name, name_ast);

  // pull out the previous column info
  Contract(is_ast_col_def(prev_def));
  EXTRACT_NAMED_NOTNULL(prev_col_def_type_attrs, col_def_type_attrs, prev_def->left);
  EXTRACT_ANY(prev_attrs, prev_col_def_type_attrs->right);
  EXTRACT_NAMED_NOTNULL(prev_col_def_name_type, col_def_name_type, prev_col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(prev_name_ast, prev_col_def_name_type->left);
  EXTRACT_STRING(prev_name, prev_name_ast);

  if (strcmp(name, prev_name)) {
    report_error(name_ast, "CQL0119: column name is different between previous and current schema", name);
    record_error(prev_def);
    return;
  }

  // It's ok for the column types to differ in sensitivity; this results in no represenation differences.
  sem_t cur_type = def->sem->sem_type & sem_not(SEM_TYPE_SENSITIVE | SEM_TYPE_DELETED);
  sem_t prev_type = prev_def->sem->sem_type & sem_not(SEM_TYPE_SENSITIVE | SEM_TYPE_DELETED);

  if (cur_type != prev_type) {
    report_error(name_ast, "CQL0120: column type is different between previous and current schema", name);
    record_error(prev_def);
    return;
  }

  coldef_info cur_cd_info;
  init_coldef_info(&cur_cd_info, cur_info);
  sem_col_def(def, &cur_cd_info);

  coldef_info prev_cd_info;
  init_coldef_info(&prev_cd_info, prev_info);
  sem_col_def(prev_def, &prev_cd_info);

  if (!sem_validate_create_prev_cur(prev_cd_info.create_version, cur_cd_info.create_version)) {
    report_error(name_ast, "CQL0121: column current create version not equal to previous create version", name);
    record_error(prev_def);
    return;
  }

  if (!sem_validate_delete_prev_cur(prev_cd_info.delete_version, cur_cd_info.delete_version)) {
    report_error(name_ast, "CQL0122: column current delete version not equal to previous delete version", name);
    record_error(prev_def);
    return;
  }

  // if the column was already deleted then the procedure name must match
  if (prev_cd_info.delete_version != -1) {
    if (!sem_match_optional_string(prev_cd_info.delete_proc, cur_cd_info.delete_proc)) {
      report_error(name_ast, "CQL0123: column @delete procedure changed", name);
      record_error(prev_def);
      return;
    }
  }

  // The create case is a little easier (no -1 check) because if the column was just created then
  // it isn't in the prevous schema at all and hence we wouldn't even be here.  This loop only
  // covers columns that exist in previous by definition.
  if (!sem_match_optional_string(prev_cd_info.create_proc, cur_cd_info.create_proc)) {
    report_error(name_ast, "CQL0124: column @create procedure changed", name);
    record_error(prev_def);
    return;
  }

  // if default value changed that's an error
  if (has_default(def->sem->sem_type)) {
    // previously verified that these match
    Invariant(has_default(prev_def->sem->sem_type));
    ast_node *cur_default = sem_get_col_default_value(attrs);
    ast_node *prev_default = sem_get_col_default_value(prev_attrs);

    bool_t identical = sem_validate_identical_text(prev_default, cur_default, gen_root_expr, NULL);
    if (!identical) {
      report_error(name_ast, "CQL0125: column current default value not equal to previous default value", name);
      record_error(prev_def);
      return;
    }
  }
}

// In addition to the normal checking, we look at the canonical string
// for the facet, if it's changed at all then there is an error.
// Note that @create/@delete are not validated here.  Those are just
// for columns and they are tested above.  Unstructured attributes like
// @attribute are disregarded entirely because they are assumed to not
// affect the schema and could be essential for codegen correctness.
// NOTE: when validating table definition pieces we are generous with @sensitive
// we already independently check if @sensitive was added and that does not cause
// a schema failure (removing it does). Here we do not generate the text of @senstive
// by providing callbacks so it looks like we're generating for SQLite.  This is
// deliberate and the tests verify this.
static bool_t sem_validate_identical_coldef(ast_node *def, ast_node *prev_def) {
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  return sem_validate_identical_text(def, prev_def, gen_col_or_key, &callbacks);
}

// This is the callback method handed to the gen_ method to force a
// no IF NOT EXISTS qualifier on create table or view statements.
static bool_t force_no_if_not_exists(ast_node *ast, void *context, charbuf *output) {
  // no output
  return true; // handled
}

// Full text comparison (including all options) but excluding "IF NOT EXISTS"
static bool_t sem_validate_identical_ddl(ast_node *cur, ast_node *prev) {
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_echo; // we want all the options to count, so NOT for sqlite output
  callbacks.if_not_exists_callback = force_no_if_not_exists; // we'll strip IF NOT EXISTS if it's there
  return sem_validate_identical_text(cur, prev, gen_one_stmt, &callbacks);
}

// When comparing two declarations, it will sometimes be the case that one of
// the declarations will have not yet been analyzed. This callback allows for
// named types to be resolved in such unanalyzed declarations so that the
// comparison can be made properly.
static bool_t sem_named_type_gen_sql_callback(ast_node *ast, void *context, charbuf *buf) {
  Contract(is_ast_str(ast));
  EXTRACT_STRING(name, ast);

  ast_node *named_type = find_named_type(name);

  if (named_type) {
    AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
    gen_data_type(rewrite_gen_data_type(named_type->sem->sem_type, named_type->sem->kind));
    AST_REWRITE_INFO_RESET();
    return true;
  }

  return false;
}

// Several places require identical definitions if names are duplicated
// This method does the job for a variety of objects, it generates the canoncial text
// for the AST and verifies that it is identical.  This works for all kinds of objects.
static bool_t sem_validate_identical_text(ast_node *prev, ast_node *cur, gen_func fn, gen_sql_callbacks *callbacks) {
  CHARBUF_OPEN(prev_sql);
  CHARBUF_OPEN(cur_sql);

  // We set `named_type_callback` so named types are always resolved before
  // being compared -- even when `prev` or `cur` have not been analyzed.
  gen_sql_callbacks callbacks_with_named_type_callback;
  if (callbacks) {
     // The input `callbacks` is copied to avoid mutating anything in the
     // caller.
     callbacks_with_named_type_callback = *callbacks;
  } else {
    init_gen_sql_callbacks(&callbacks_with_named_type_callback);
    // We set `gen_mode_echo` as it's equivalent to passing NULL to
    // `gen_with_callbacks`.
    callbacks_with_named_type_callback.mode = gen_mode_echo;
  }
  callbacks_with_named_type_callback.named_type_callback = sem_named_type_gen_sql_callback;

  gen_set_output_buffer(&prev_sql);
  gen_with_callbacks(prev, fn, &callbacks_with_named_type_callback);

  gen_set_output_buffer(&cur_sql);
  gen_with_callbacks(cur, fn, &callbacks_with_named_type_callback);

  bool_t identical = !strcmp(prev_sql.ptr, cur_sql.ptr);

  if (!identical) {
    cql_error("Incompatible declarations found\n");
    report_error(prev, prev_sql.ptr, NULL);
    report_error(cur, cur_sql.ptr, NULL);
    cql_error("The above must be identical.\n");
  }

  CHARBUF_CLOSE(cur_sql);
  CHARBUF_CLOSE(prev_sql);

  return identical;
}

// Here we're going to validate that two function declarations are identical.
// We're going to do this by comparing the canonical sql for both.
// We could compare the AST directly but to do so we would have to basically
// recapitulate all of the same walking that the text generator does.
// That is a maintenance problem but also doing it this way is economical
// and it ensures that the string decoding is bug-free.
static bool_t sem_validate_identical_funcs(ast_node *prev_func, ast_node *cur_func) {
  CHARBUF_OPEN(prev_sql);
  CHARBUF_OPEN(cur_sql);

  gen_set_output_buffer(&prev_sql);
  gen_statement_with_callbacks(prev_func, NULL);

  gen_set_output_buffer(&cur_sql);
  gen_statement_with_callbacks(cur_func, NULL);

  bool_t identical = !strcmp(prev_sql.ptr, cur_sql.ptr);

  CHARBUF_CLOSE(cur_sql);
  CHARBUF_CLOSE(prev_sql);

  return identical;
}

// Here we're going to validate that two proc declarations are identical.
// We're going to do this by comparing the canonical sql for both.
// We could compare the AST directly but to do so we would have to basically
// recapitulate all of the same walking that the text generator does.
// That is a maintenance problem but also doing it this way is economical
// and it ensures that the string decoding is bug-free.
static bool_t sem_validate_identical_procs(ast_node *prev_proc, ast_node *cur_proc) {
  return sem_validate_identical_text(prev_proc, cur_proc, gen_declare_proc_from_create_or_decl, NULL);
}

// Here we check the table found in the "previous" schema against the current schema.
// There are several validations we have to do here:
//  * the table should be present (but maybe marked with @delete)
//  * the table must not be a view now
//  * the table has to have a compatible create version
//  * the table has to have a compatible delete version
//  * any matching column validations (see related method) must be ok
//  * the new version of the table can't have fewer columns (you @delete instead)
//  * if the new verision has more columns they have to be all at the end and be marked create
//  * new columns can't be marked with @create and @delete
//  * the table create flags (like TEMP, or IF NOT EXISTS) must be the same
static void sem_validate_previous_table(ast_node *prev_table) {
  Contract(!current_joinscope);
  Contract(is_ast_create_table_stmt(prev_table));
  EXTRACT_NAMED_NOTNULL(prev_create_table_name_flags, create_table_name_flags, prev_table->left);
  EXTRACT_NAMED_NOTNULL(prev_table_flags_attrs, table_flags_attrs, prev_create_table_name_flags->left);
  EXTRACT_OPTION(prev_flags, prev_table_flags_attrs->left);
  EXTRACT_ANY(prev_table_attrs, prev_table_flags_attrs->right);
  EXTRACT_ANY_NOTNULL(prev_name_ast, prev_create_table_name_flags->right);
  EXTRACT_STRING(name, prev_name_ast);
  EXTRACT_ANY_NOTNULL(prev_col_key_list, prev_table->right);

  bool_t is_temp = !!(prev_flags & TABLE_IS_TEMP);

  // validation of @deleted tables is a thing, so we need deleted tables, too
  ast_node *ast = find_table_or_view_even_deleted(name);

  if (!ast && is_temp) {
    // temp table totally gone, that's ok
    return;
  }

  if (!ast) {
    report_error(prev_table, "CQL0126: table was present but now it does not exist (use @delete instead)", name);
    record_error(prev_table);
    return;
  }

  if (!is_ast_create_table_stmt(ast)) {
    report_error(ast, "CQL0127: object was a table but is now a view", name);
    record_error(prev_table);
    record_error(ast);
    return;
  }

  EXTRACT_NOTNULL(create_table_name_flags, ast->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
  EXTRACT_ANY_NOTNULL(col_key_list, ast->right);

  // Tables that are missing from the previous schema have to be validated as well
  // but using their own rules.  That happens in sem_validate_all_tables_not_in_previous.
  // Once this flag is set sem_validate_all_tables_not_in_previous won't consider this table.
  sem_add_flags(ast, SEM_TYPE_VALIDATED);

  version_attrs_info prev_info;
  init_version_attrs_info(&prev_info, name, prev_table, prev_table_attrs);
  version_attrs_info cur_info;
  init_version_attrs_info(&cur_info, name, ast, table_attrs);

  if (!sem_validate_attrs_prev_cur(&prev_info, &cur_info, name_ast)) {
    record_error(prev_table);
    record_error(ast);
    return;
  }

  // the max schema number, we can't do that until later so save it.
  if (prev_info.recreate && !cur_info.recreate) {
    // the table was deleted and didn't stay on the recreate plan, that's an error
    if (cur_info.delete_version > 0 && !cur_info.is_virtual_table) {
      report_error(ast, "CQL0448: table was marked @delete but it needs to be marked @recreate @delete", name);
      record_error(prev_table);
      record_error(ast);
      return;
    }

    // check create verisions
    if (ast->sem->create_version > 0) {
      add_item_to_list(&all_prev_recreate_tables, ast);
    }
  }

  if (prev_info.recreate && cur_info.recreate) {
     bool_t error = false;
     if (prev_info.recreate_group_name == NULL || cur_info.recreate_group_name == NULL) {
        // error only if we lost the group name
        error =  prev_info.recreate_group_name && !cur_info.recreate_group_name;
     }
     else {
        // error if the name changed
        error = !!Strcasecmp(prev_info.recreate_group_name, cur_info.recreate_group_name);
     }

     if (error) {
       report_error(ast, "CQL0449: recreate group annotation changed in table", name);
       record_error(prev_table);
       record_error(ast);
       return;
     }
  }

  // If we're on the @recreate plan then we can make any changes we like to the table
  // We don't need to check the rest... drop/create works on everything.
  if (cur_info.recreate || prev_info.recreate) {
    return;
  }

  // Begin table facet validations

  ast_node *prev_item = prev_col_key_list;
  ast_node *item = col_key_list;
  ast_node *def = NULL;
  ast_node *prev_def = NULL;

  // First validate columns
  for (;;) {
    while (item && !is_ast_col_def(def = item->left)) {
      Contract(is_ast_col_key_list(item));
      item = item->right;
    }

    while (prev_item && !is_ast_col_def(prev_def = prev_item->left)) {
      Contract(is_ast_col_key_list(prev_item));
      prev_item = prev_item->right;
    }

    if (!item || !prev_item) {
      break;
    }

    // this gives superior diagnostics for most typical differences
    sem_validate_col_def_prev_cur(def, prev_def, &cur_info, &prev_info);
    if (is_error(prev_def)) {
      record_error(prev_table);
      record_error(ast);
      return;
    }

    // any other differences are found by comparing the canonical def text
    if (!sem_validate_identical_coldef(def, prev_def)) {
      report_error(def, "CQL0128: table has a column that is different in the previous and current schema", def->sem->name);
      record_error(prev_table);
      record_error(ast);
      return;
    }

    item = item->right;
    prev_item = prev_item->right;
  }

  // the loop doesn't end until we hit the end of one of the lists
  Invariant(!item || !prev_item);

  // If def is null and prev_def is not null then that means stuff was removed
  // in the current schema, it should have been marked delete, not removed.

  if (prev_item) {
    report_error(prev_item, "CQL0129: a column was removed from the table rather than marked with @delete", prev_item->left->sem->name);
    record_error(prev_table);
    record_error(ast);
    return;
  }

  // If there are any columns left then they should be only created columns
  // These are new created columns (that's fine and their created version must
  // be >= the biggest schema version in the previous schema.
  // It's ok to add more created columns to the current schema.

  for ( ;item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(cdef, item->left);

    // looking only at columns
    if (!is_ast_col_def(cdef)) {
      continue;
    }

    if (cdef->sem->create_version < 0) {
      report_error(cdef, "CQL0130: table has columns added without marking them @create", name);
      record_error(prev_table);
      record_error(ast);
      return;
    }

    if (cdef->sem->delete_version > 0) {
      report_error(cdef, "CQL0131: table has newly added columns that are marked both @create and @delete", name);
      record_error(prev_table);
      record_error(ast);
      return;
    }

    // The create version will have to be validated against the max version in the previous schema.
    // We can't do that until the end when we know the max version.
    add_item_to_list(&created_columns, cdef);
  }

  // Reset and validate non-columns

  prev_item = prev_col_key_list;
  item = col_key_list;

  for (;;) {
    while (item && is_ast_col_def(def = item->left)) {
      Contract(is_ast_col_key_list(item));
      item = item->right;
    }

    while (prev_item && is_ast_col_def(prev_def = prev_item->left)) {
      Contract(is_ast_col_key_list(prev_item));
      prev_item = prev_item->right;
    }

    if (!item || !prev_item) {
      break;
    }

    // any other differences are found by comparing the canonical def text
    if (!sem_validate_identical_coldef(def, prev_def)) {
      report_error(def, "CQL0132: table has a facet that is different in the previous and current schema", name);
      record_error(prev_table);
      record_error(ast);
      return;
    }

    item = item->right;
    prev_item = prev_item->right;
  }

  // the loop doesn't end until we hit the end of one of the lists
  Invariant(!item || !prev_item);

  // If def is null and prev_def is not null then that means stuff was removed
  // in the current schema, it should have been marked delete, not removed.

  if (prev_item) {
    report_error(prev_item, "CQL0133: non-column facets have been removed from the table", name);
    record_error(prev_table);
    return;
  }

  // some new non-column was added...
  if (item) {
    report_error(item, "CQL0134: table has a new non-column facet in the current schema", name);
    record_error(prev_table);
    record_error(ast);
    return;
  }

  // If both def and prev_def are null then perfect match no further checking.

  // the table flags have to match
  if (flags != prev_flags) {
    report_error(ast, "CQL0135: table create statement attributes different than previous version", name);
    record_error(prev_table);
    record_error(ast);
    return;
  }

  enqueue_pending_region_validation(prev_table, ast, name);
}

// Verison info can be gathered from tables, views, or indices (columns are done seperately)
// Here we emit a record the annotation with the correct code into the pending annotations buffer
// this will be later sorted and used to drive schema migration if schema codegen happens.
static void sem_record_annotation_from_vers_info(version_attrs_info *vers_info) {
  ast_node *target_ast = vers_info->target_ast;

  if (vers_info->recreate) {
    ast_node *recreate_ast = vers_info->recreate_version_ast;
    CSTR group_name = vers_info->recreate_group_name ? vers_info->recreate_group_name : "";
    record_recreate_annotation(target_ast, vers_info->name, group_name, recreate_ast);
    // no need for @create or @delete annotation if recreate, the recreate will also handle delete if reqd
    return;
  }

  if (vers_info->create_version > 0) {
    EXTRACT(version_annotation, vers_info->create_version_ast);
    uint32_t code = vers_info->create_code;
    record_schema_annotation(vers_info->create_version, target_ast, vers_info->name, code, NULL, version_annotation, 0);
  }

  if (vers_info->delete_version > 0) {
    EXTRACT(version_annotation, vers_info->delete_version_ast);
    uint32_t code = vers_info->delete_code;
    record_schema_annotation(vers_info->delete_version, target_ast, vers_info->name, code, NULL, version_annotation, 0);
  }

}

typedef struct trigger_dep_context {
   ast_node *trigger_ast;
   CSTR trigger_on_table_name;
} trigger_dep_context;

// When we locate a table used by a view we simply add that info to the dependency map in both directions
static void sem_found_dep_in_trigger(CSTR _Nonnull target_name, ast_node *_Nonnull target_ast, void *_Nullable context) {
  Contract(is_ast_create_table_stmt(target_ast) || is_ast_create_view_stmt(target_ast));

  trigger_dep_context *info  = context;

  if (Strcasecmp(info->trigger_on_table_name, target_name)) {
    // we don't have to record that the trigger depends on the table that it is on
    // if that table goes away the trigger is implicitly deleted anyway, it would
    // just give us a bunch of false positives.  It's the other tables that need searching
    record_table_dependencies(info->trigger_ast, target_ast);
  }
}

// Here we peek into the trigger body and find the tables that it uses.
// We're going to record those so that if a table is unsubscribed we can make sure
// there are no lingering triggers still using it.  We don't have to worry about views inside
// the body because if the trigger uses a view and the view uses a table then that view itself
// will cause an error to be reported if you attempt to unsubscribe the table.
static void sem_record_trigger_dependencies(ast_node *ast) {
  Contract(is_ast_create_trigger_stmt(ast));

  EXTRACT_NOTNULL(trigger_body_vers, ast->right);
  EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
  EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
  EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
  EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
  EXTRACT_ANY_NOTNULL(table_name_ast, trigger_target_action->left);
  EXTRACT_STRING(table_name, table_name_ast);

  trigger_dep_context context = {
    .trigger_ast = ast,
    .trigger_on_table_name = table_name
  };

  table_callbacks callbacks = {
    .callback_any_table = sem_found_dep_in_trigger,
    .callback_any_view = sem_found_dep_in_trigger,
    .callback_context = &context,
    .do_not_recurse_views = true,
  };

  find_table_refs(&callbacks, ast);
}

// The create trigger statement is quite a beast, validations include:
//  * the trigger name must be unique
//  * For insert the "new.*" table is available in expressions/statement
//  * For delete the "old.*" table is avallable in expressions/statements
//  * For update both are available
//    * If optional columns present in update, they must be unique/valid
//  * The When expression must evaluate to a numeric
//  * The statement list must be error free with the usual rules plus new/old
//  * The RAISE function may be used inside a trigger (NYI)
//  * The table_name must be a table (not a view) UNLESS the trigger type is TRIGGER_INSTEAD_OF
//  * select statements inside the statement block do not count as returns for the proc
static void sem_create_trigger_stmt(ast_node *ast) {
  Contract(is_ast_create_trigger_stmt(ast));

  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_NOTNULL(trigger_body_vers, ast->right);
  EXTRACT_ANY(trigger_attrs, trigger_body_vers->right);
  EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
  EXTRACT_ANY_NOTNULL(trigger_name_ast, trigger_def->left);
  EXTRACT_STRING(trigger_name, trigger_name_ast);
  EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
  EXTRACT_OPTION(cond_flags, trigger_condition->left);
  flags |= cond_flags;
  EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
  EXTRACT_NOTNULL(trigger_operation, trigger_op_target->left);
  EXTRACT_OPTION(op_flags, trigger_operation->left);
  EXTRACT(name_list, trigger_operation->right);
  flags |= op_flags;
  EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
  EXTRACT_ANY_NOTNULL(table_name_ast, trigger_target_action->left);
  EXTRACT_STRING(table_name, table_name_ast);
  EXTRACT_NOTNULL(trigger_action, trigger_target_action->right);
  EXTRACT_OPTION(action_flags, trigger_action->left);
  flags |= action_flags;
  EXTRACT_NOTNULL(trigger_when_stmts, trigger_action->right);
  EXTRACT_ANY(when_expr, trigger_when_stmts->left);
  EXTRACT_NOTNULL(stmt_list, trigger_when_stmts->right);

  // as with many other constructs, if we're validating previous schema it isn't safe to look inside the trigger
  // body because it likely refers to things that don't exist in the current schema.  This being the case
  // we just do the previous validation and move on;  Views do the same.
  if (validating_previous_schema) {
    record_ok(ast);
    sem_validate_previous_trigger(ast);
    return;
  }

  bool_t adding_current_entity = will_add_current_entity();

  // if there is an existing trigger, save it here so we can check for duplicates later.
  ast_node *existing_defn = adding_current_entity ? find_trigger(trigger_name) : NULL;

  // Trigger declarations (i.e. outside of any proc) are totally ignored
  // in the context of a schema migration script.  This prevents us from
  // getting errors because the trigger refers to tables or columns that are not yet
  // in existence in the version we are migrating.
  if (schema_upgrade_version > 0) {
    record_ok(ast);
    return;
  }

  version_attrs_info vers_info;
  init_version_attrs_info(&vers_info, trigger_name, ast, trigger_attrs);

  bool_t valid_version_info = sem_validate_version_attrs(&vers_info);
  Invariant(valid_version_info);   // nothing can go wrong with trigger versions

  if (!sem_validate_vers_ok_in_context(&vers_info)) {
    record_error(ast);
    return;
  }

  if (!sem_validate_no_delete_migration(&vers_info, ast, trigger_name)) {
    return;
  }

  ast_node *target = NULL;
  bool_t deleting = vers_info.delete_version > 0;

  if (deleting) {
    target = find_usable_table_or_view_even_deleted(
      table_name,
      table_name_ast,
      "CQL0137: table/view not found");

    if (is_deleted(target)) {
      report_error(ast, "CQL0397: object is an orphan because its table is deleted. Remove rather than @delete", trigger_name);
      record_error(ast);
      return;
    }
  }
  else {
    target = find_usable_and_not_deleted_table_or_view(
      table_name,
      table_name_ast,
      "CQL0137: table/view not found");
  }

  if (!target) {
    record_error(ast);
    return;
  }

  sem_non_blob_storage_table(ast, target);
  if (is_error(ast)) {
    return;
  }

  if (is_virtual_ast(target)) {
    report_error(table_name_ast, "CQL0162: cannot add a trigger to a virtual table", table_name);
    record_error(ast);
    return;
  }

  table_name_ast->sem = target->sem;

  if (!is_ast_create_table_stmt(target) && !(flags & TRIGGER_INSTEAD_OF)) {
    report_error(table_name_ast, "CQL0138: a trigger on a view must be the INSTEAD OF form", table_name);
    record_error(ast);
    return;
  }

  sem_join *jptr;
  sem_struct *sptr = target->sem->sptr;

  if (flags & TRIGGER_INSERT) {
    jptr = new_sem_join(1);
    jptr->names[0] = "new";
    jptr->tables[0] = sptr;
  }
  else if (flags & TRIGGER_DELETE) {
    jptr = new_sem_join(1);
    jptr->names[0] = "old";
    jptr->tables[0] = sptr;
  }
  else {
    Contract(flags & TRIGGER_UPDATE);
    jptr = new_sem_join(2);
    jptr->names[0] = "old";
    jptr->tables[0] = sptr;
    jptr->names[1] = "new";
    jptr->tables[1] = sptr;

    // temporarily change the count 1 to avoid name ambiguity resolving the columns
    jptr->count = 1;

    // validate columns names if present
    if (name_list && !sem_validate_name_list(name_list, jptr)) {
      record_error(ast);
      return;
    }

    // and put it back if we're gonna proceed
    jptr->count = 2;
  }

  if (when_expr) {
    PUSH_JOIN(when_scope, jptr);
    sem_numeric_expr(when_expr, NULL, "WHEN", SEM_EXPR_CONTEXT_WHERE);
    POP_JOIN();

    if (is_error(when_expr)) {
      record_error(ast);
      return;
    }
  }

  PUSH_JOIN(trigger_scope, jptr);
  Invariant(!in_trigger);
  in_trigger = 1;

  sem_stmt_list(stmt_list);

  Invariant(in_trigger);
  in_trigger = 0;
  POP_JOIN();

  if (is_error(stmt_list)) {
    record_error(ast);
    return;
  }

  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->delete_version = vers_info.delete_version;
  ast->sem->region = current_region;

  if (ast->sem->delete_version > 0) {
    ast->sem->sem_type |= SEM_TYPE_DELETED;
  }

  if (existing_defn) {
    if (!sem_validate_identical_ddl(existing_defn, ast)) {
      report_error(trigger_name_ast, "CQL0136: trigger already exists", trigger_name);
      record_error(trigger_name_ast);
      record_error(ast);
    }
    return;
  }

  if (adding_current_entity) {
    add_trigger(ast, trigger_name);
    add_item_to_list(&all_triggers_list, ast);

    // and record the annotation
    sem_record_annotation_from_vers_info(&vers_info);

    // record the tables used by this trigger (and cross link)
    sem_record_trigger_dependencies(ast);
  }
}

static bool_t sem_validate_virtual_table_vers(version_attrs_info *table_vers_info) {
  Contract(table_vers_info);
  EXTRACT_NOTNULL(create_table_stmt, table_vers_info->target_ast);

  // if deleting virtual table... you must add the reminder
  if (table_vers_info->is_virtual_table && table_vers_info->delete_version_ast) {
     if (!table_vers_info->delete_proc || Strcasecmp(CQL_MODULE_WARN, table_vers_info->delete_proc )) {
        report_error(table_vers_info->delete_version_ast, "CQL0392: when deleting a virtual table you must specify @delete(nn, "
            CQL_MODULE_WARN ") as a reminder not to delete the module for this virtual table", table_vers_info->name);
        record_error(create_table_stmt);
        return false;
     }
  }
  return true;
}

// If you are putting DDL inside of a procedure then it is going to run regardless; these
// entires do not get versioning attributes, those are reserved for schema declarations outside
// of any procedure.
static bool_t sem_validate_vers_ok_in_context(version_attrs_info *vers) {
  bool_t is_versioned = vers->create_version > 0 || vers->delete_version > 0;

  // virtual tables are always recreate, this is hard coded, so disregard that as a versioning error
  is_versioned |= !vers->is_virtual_table && vers->recreate;

  if (current_proc && is_versioned) {
     report_error(vers->target_ast, "CQL0396: versioning attributes may not be used on DDL inside a procedure", vers->name);
     return false;
  }

  if (vers->is_temp && is_versioned) {
    report_error(vers->target_ast, "CQL0139: temp objects may not have versioning annotations", vers->name);
    return false;
  }

  return true;
}

static void report_invalid_blob_storage_column(ast_node *ast, CSTR reason, CSTR column, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0459: table is not suitable for use as blob storage: column '%s' %s in", column, reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

// validate that the indicated col_def is ok for blob storage
// this basically means it has to be ultra simple
// no autoinc, no fk, no pk, no default value
static void sem_blob_storage_col_def(ast_node *table_ast, ast_node *def, CSTR table_name) {
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);

  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
  EXTRACT_STRING(col_name, name_ast);

  // if we find anything weird, it's an error
  for (ast_node *ast = attrs; ast; ast = ast->right) {
    if (is_ast_create_attr(ast) || is_ast_col_attrs_not_null(ast) || is_ast_sensitive_attr(ast)) {
    }
    else if (is_ast_delete_attr(ast)) {
      // In principle we could support this, but we don't for now.
      // To do this you'd have to ensure that the blob storage included the field but didn't decode it
      // if it was present...  the problem is that old versions of the blob might exist with the field
      // there, so you can't just get rid of it.  Really this is likely to be super confusing
      // and we may never do this for that reason.
      report_invalid_blob_storage_column(table_ast, "has been deleted", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_default(ast)) {
      // In principle we could support this, but we don't for now.
      report_invalid_blob_storage_column(table_ast, "has a default value", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_check(ast)) {
      report_invalid_blob_storage_column(table_ast, "has a check expression", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_collate(ast)) {
      report_invalid_blob_storage_column(table_ast, "specifies collation order", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_pk(ast)) {
      // note that autoinc must be on a pk and is in its node so this also detects autoinc
      report_invalid_blob_storage_column(table_ast, "has a primary key", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_fk(ast)) {
      report_invalid_blob_storage_column(table_ast, "has a foreign key", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_hidden(ast)) {
      report_invalid_blob_storage_column(table_ast, "is a hidden column", col_name, table_name);
      return;
    }
    else {
      // this is all that's left
      Contract(is_ast_col_attrs_unique(ast));
      report_invalid_blob_storage_column(table_ast, "has a unique key", col_name, table_name);
      return;
    }
  }
}

static void report_invalid_blob_storage(ast_node *ast, CSTR reason, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0459: table is not suitable for use as blob storage: %s", reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

static void sem_validate_table_for_blob_storage(ast_node *ast) {
  Contract(is_ast_create_table_stmt(ast));
  EXTRACT_NOTNULL(create_table_name_flags, ast->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(col_key_list, ast->right);

  Contract(!is_error(ast));

  int32_t temp = flags & TABLE_IS_TEMP;
  int32_t no_rowid = flags & TABLE_IS_NO_ROWID;

  if (temp) {
    report_invalid_blob_storage(ast, "it is redundantly marked TEMP", name);
    return;
  }

  if (no_rowid) {
    report_invalid_blob_storage(ast, "it is redundantly marked WITHOUT ROWID", name);
    return;
  }

  // check the column defs, error out if we find any constraints
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    if (!is_ast_col_def(def)) {
      report_invalid_blob_storage(ast, "it has at least one constraint", name);
      return;
    }

    sem_blob_storage_col_def(ast, def, name);
    if (is_error(ast)) {
      return;
    }
  }

  while (table_attrs) {
    if (is_ast_recreate_attr(table_attrs)) {
      report_invalid_blob_storage(ast, "it is declared using @recreate", name);
      return;
    }
    table_attrs = table_attrs->right;
  }
}

static void report_invalid_backing_column(ast_node *ast, CSTR reason, CSTR column, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0483: table is not suitable for use as backing storage: column '%s' %s in", column, reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

// validate that the indicated col_def is ok for blob storage
// this basically means it has to be ultra simple
// no autoinc, no fk, no pk, no default value
static void sem_backing_col_def(ast_node *table_ast, ast_node *def, CSTR table_name) {
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);

  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
  EXTRACT_STRING(col_name, name_ast);

  // if we find anything weird, it's an error
  for (ast_node *ast = attrs; ast; ast = ast->right) {
    if (is_ast_col_attrs_not_null(ast) || is_ast_sensitive_attr(ast)) {
        // these basic column attributes are allowed
    }
    else if (is_ast_col_attrs_pk(ast)) {
      EXTRACT_NOTNULL(autoinc_and_conflict_clause, ast->left);
      EXTRACT(col_attrs_autoinc, autoinc_and_conflict_clause->left);

      // conflict clause is ok, we can ignore it, autoinc is not supported until we do "stage 2"
      // of this feature, initially the pk is just a blob and all the playload is in another blob

      if (col_attrs_autoinc) {
        report_invalid_backing_column(table_ast, "specifies auto increment", col_name, table_name);
        return;
      }
    }
    else if (is_ast_create_attr(ast)) {
      // backing tables do not support schema changes at this time
      report_invalid_backing_column(table_ast, "has create attribute", col_name, table_name);
      return;
    }
    else if (is_ast_delete_attr(ast)) {
      // backing tables do not support schema changes at this time
      report_invalid_backing_column(table_ast, "has delete attribute", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_default(ast)) {
      // In principle we could support this, but we don't for now.
      report_invalid_backing_column(table_ast, "has a default value", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_check(ast)) {
      report_invalid_backing_column(table_ast, "has a check expression", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_collate(ast)) {
      report_invalid_backing_column(table_ast, "specifies collation order", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_fk(ast)) {
      report_invalid_backing_column(table_ast, "has a foreign key", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_hidden(ast)) {
      report_invalid_backing_column(table_ast, "is a hidden column", col_name, table_name);
      return;
    }
    else {
      // this is all that's left
      Contract(is_ast_col_attrs_unique(ast));
      report_invalid_backing_column(table_ast, "has a unique key", col_name, table_name);
      return;
    }
  }
}

static void report_invalid_backing(ast_node *ast, CSTR reason, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0483: table is not suitable for use as backing storage: %s", reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

static void sem_validate_table_for_backing(ast_node *ast) {
  Contract(is_ast_create_table_stmt(ast));
  EXTRACT_NOTNULL(create_table_name_flags, ast->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(col_key_list, ast->right);

  Contract(!is_error(ast));

  int32_t temp = flags & TABLE_IS_TEMP;
  int32_t no_rowid = flags & TABLE_IS_NO_ROWID;

  if (temp) {
    report_invalid_backing(ast, "it is redundantly marked TEMP", name);
    return;
  }

  if (no_rowid) {
    report_invalid_backing(ast, "it is redundantly marked WITHOUT ROWID", name);
    return;
  }

  // check the column defs, error out if we find any constraints
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    // PK is allowed
    if (is_ast_pk_def(def)) {
      continue;
    }

    if (!is_ast_col_def(def)) {
      report_invalid_backing(ast, "it has at least one invalid constraint", name);
      return;
    }

    sem_backing_col_def(ast, def, name);
    if (is_error(ast)) {
      return;
    }
  }

  while (table_attrs) {
    if (is_ast_recreate_attr(table_attrs)) {
      report_invalid_backing(ast, "it is declared using @recreate", name);
      return;
    }
    table_attrs = table_attrs->right;
  }
}


static void report_invalid_backed_column(ast_node *ast, CSTR reason, CSTR column, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0487: table is not suitable for use as backed storage: column '%s' %s in", column, reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

// validate that the indicated col_def is ok for blob storage
// this basically means it has to be ultra simple
// no autoinc, no fk, no default value, no unique constraints etc.
static void sem_backed_col_def(ast_node *table_ast, ast_node *def, CSTR table_name) {
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);

  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
  EXTRACT_STRING(col_name, name_ast);

  // if we find anything weird, it's an error
  for (ast_node *ast = attrs; ast; ast = ast->right) {
    if (is_ast_col_attrs_not_null(ast) || is_ast_sensitive_attr(ast)) {
        // these basic column attributes are allowed
    }
    else if (is_ast_col_attrs_pk(ast)) {
      EXTRACT_NOTNULL(autoinc_and_conflict_clause, ast->left);
      EXTRACT(col_attrs_autoinc, autoinc_and_conflict_clause->left);

      // conflict clause is ok, we can ignore it, autoinc is not supported until we do "stage 2"
      // of this feature, initially the pk is just a blob and all the playload is in another blob

      if (col_attrs_autoinc) {
        report_invalid_backed_column(table_ast, "specifies auto increment", col_name, table_name);
        return;
      }
    }
    else if (is_ast_create_attr(ast)) {
      // backed tables do not use schema annotations, they can change at whim
      report_invalid_backed_column(table_ast, "has create attribute", col_name, table_name);
      return;
    }
    else if (is_ast_delete_attr(ast)) {
      // backed tables do not use schema annotations, they can change at whim
      report_invalid_backed_column(table_ast, "has delete attribute", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_default(ast)) {
      // In principle we could support this, but we don't for now.
      report_invalid_backed_column(table_ast, "has a default value", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_check(ast)) {
      report_invalid_backed_column(table_ast, "has a check expression", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_collate(ast)) {
      report_invalid_backed_column(table_ast, "specifies collation order", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_fk(ast)) {
      // these could be supported when the backing table is allowed to have an explicit FK column or columns to match
      report_invalid_backed_column(table_ast, "has a foreign key", col_name, table_name);
      return;
    }
    else if (is_ast_col_attrs_hidden(ast)) {
      report_invalid_backed_column(table_ast, "is a hidden column", col_name, table_name);
      return;
    }
    else {
      // this is all that's left
      Contract(is_ast_col_attrs_unique(ast));
      report_invalid_backed_column(table_ast, "has a unique key", col_name, table_name);
      return;
    }
  }
}

static void report_invalid_backed(ast_node *ast, CSTR reason, CSTR table) {
  Contract(ast);
  Contract(reason);
  Contract(table);

  CSTR err_msg = dup_printf("CQL0487: table is not suitable for use as backed storage: %s", reason);
  report_error(ast, err_msg, table);
  record_error(ast);
}

static void sem_validate_table_for_backed(ast_node *ast) {
  Contract(is_ast_create_table_stmt(ast));
  EXTRACT_NOTNULL(create_table_name_flags, ast->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(col_key_list, ast->right);

  Contract(!is_error(ast));

  int32_t temp = flags & TABLE_IS_TEMP;
  int32_t no_rowid = flags & TABLE_IS_NO_ROWID;

  if (temp) {
    report_invalid_backed(ast, "it is redundantly marked TEMP", name);
    return;
  }

  if (no_rowid) {
    report_invalid_backed(ast, "it is redundantly marked WITHOUT ROWID", name);
    return;
  }

  // check the column defs, error out if we find any constraints
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    // PK is allowed
    if (is_ast_pk_def(def)) {
      continue;
    }

    if (!is_ast_col_def(def)) {
      report_invalid_backed(ast, "it has at least one invalid constraint", name);
      return;
    }

    sem_backed_col_def(ast, def, name);
    if (is_error(ast)) {
      return;
    }
  }

  if (table_attrs) {
    report_invalid_backed(ast, "it is declared using schema directives (@recreate, @create etc.)", name);
    return;
  }
}

// Unlike the other parts of DDL we actually deeply care about the tables.
// We have to grab all the columns and column types out of it and create
// the appropriate sem_struct, as well as the sem_join with just one table.
// Along the way we validate a bunch of stuff like:
// * unique table name
// * no duplicate column names
// * recursive correctness of constraints (see above)
// The table will be added to the table/view list.
static void sem_create_table_stmt(ast_node *ast) {
  Contract(!current_joinscope);
  Contract(is_ast_create_table_stmt(ast));
  EXTRACT_NOTNULL(create_table_name_flags, ast->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(col_key_list, ast->right);

  // These never nest
  Invariant(!current_table_name);
  Invariant(!current_table_ast);

  // Save these so that we can quickly detect if we are trying to refer to the table that we are building
  // and get its ast.  This is needed because that name won't be in the symbol table until it succeeds. So
  // the self-referencing table case needs to use these to detect the self-reference.
  current_table_name = name;
  current_table_ast = ast;

  int32_t temp = flags & TABLE_IS_TEMP;
  int32_t no_rowid = flags & TABLE_IS_NO_ROWID;

  // CREATE [TEMP]TABLE [name] [if_not_exist] [col_key_list] [without rowid] [table_attrs]

  version_attrs_info table_vers_info;
  init_version_attrs_info(&table_vers_info, name, ast, table_attrs);

  if (!sem_validate_version_attrs(&table_vers_info)) {
    record_error(ast);
    goto cleanup;
  }

  if (!sem_validate_virtual_table_vers(&table_vers_info)) {
    record_error(ast);
    goto cleanup;
  }

  if (!sem_validate_vers_ok_in_context(&table_vers_info)) {
    record_error(ast);
    goto cleanup;
  }

  bool_t adding_current_entity = will_add_current_entity();

  // if there is an existing table, save it here so we can check for duplicates later.
  ast_node *existing_defn = adding_current_entity ? find_table_or_view_even_deleted(name) : NULL;

  coldef_info col_info;
  init_coldef_info(&col_info, &table_vers_info);

  bool_t rewrite_col = rewrite_col_key_list(col_key_list);

  if (!rewrite_col) {
    record_error(ast);
    goto cleanup;
  }

  // first count up the columns (and only the columns)
  uint32_t cols = 0;
  bool_t found_constraint = false;
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    if (is_ast_col_def(def)) {
      if (found_constraint) {
        report_error(def, "CQL0349: column definitions may not come after constraints", col_info.col_name);
        record_error(ast);
        goto cleanup;;
      }

      sem_col_def(def, &col_info);
      if (is_error(def)) {
        record_error(ast);
        goto cleanup;
      }

      if (temp && (col_info.create_version > 0 || col_info.delete_version > 0)) {
        report_error(def, "CQL0140: columns in a temp table may not have versioning attributes", col_info.col_name);
        record_error(ast);
        goto cleanup;;
      }

      if (is_deleted(def)) {
        continue;
      }

      cols++;
    }
    else {
      found_constraint = true;
    }
  }

  Invariant(col_info.autoinc_columns <= 1);

  if (col_info.autoinc_columns && no_rowid) {
    report_error(ast, "CQL0141: table has an AUTOINCREMENT column; it cannot also be WITHOUT ROWID", name);
    record_error(ast);
    goto cleanup;
  }

  if (enforcement.strict_without_rowid && no_rowid) {
    report_error(ast, "CQL0339: WITHOUT ROWID tables are forbidden if strict without rowid mode is enabled", name);
    record_error(ast);
    goto cleanup;
  }

  // now create a struct type with the correct number of columns
  // the types have already been computed so all we have to do is
  // check for duplicates
  sem_struct *sptr = new_sem_struct(name, cols);

  symtab *columns = symtab_new();

  int32_t col = 0;
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    if (is_ast_col_def(def)) {
      Invariant(def->sem->name);
      Invariant(col <= cols);  // it's possible that the rest are deleted and we're at the end.

      // columns must be unique, including deleted columns
      if (!symtab_add(columns, def->sem->name, NULL)) {
        EXTRACT_NOTNULL(col_def_type_attrs, def->left);
        EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
        EXTRACT_ANY_NOTNULL(col_def_ast, col_def_name_type->left);

        report_error(col_def_ast, "CQL0142: duplicate column name", def->sem->name);
        record_error(ast);
        symtab_delete(columns);
        goto cleanup;;
      }

      if (is_deleted(def)) {
        continue;
      }

      Invariant(col < cols);

      sptr->names[col] = def->sem->name;
      sptr->semtypes[col] = def->sem->sem_type;
      sptr->kinds[col] = def->sem->kind;
      col++;
    }
  }

  symtab_delete(columns);

  Invariant(col == cols);

  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->jptr = sem_join_from_sem_struct(sptr);
  ast->sem->region = current_region;

  sem_constraints(ast, col_key_list, &col_info);

  if (is_error(ast)) {
    // important to early out here so that sem_type is not altered with table flags
    // that will break invariants (SEM_TYPE_ERROR should stay "pure")
    goto cleanup;;
  }

  if (col_info.primary_keys > 1) {
    report_error(name_ast, "CQL0143: more than one primary key in table", name);
    record_error(ast);
    goto cleanup;;
  }

  // Constraints may have computed non-nullability changes
  // if there are any such changes we need to apply them to the def
  // node so that the types are consistent.

  col = 0;
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    if (is_ast_col_def(def)) {
      if (is_deleted(def)) {
        continue;
      }

      Invariant(col < cols);

      if (not_nullable_flag(sptr->semtypes[col]) && !not_nullable_flag(def->sem->sem_type)) {
        sem_add_flags(def, SEM_TYPE_NOTNULL);
      }
      col++;
    }
  }

  Invariant(col == cols);

  ast->sem->sem_type           |= table_vers_info.flags;
  ast->sem->create_version      = table_vers_info.create_version;
  ast->sem->delete_version      = table_vers_info.delete_version;
  ast->sem->recreate            = table_vers_info.recreate;
  ast->sem->recreate_group_name = table_vers_info.recreate_group_name;

  run_pending_table_validations();

  if (!is_error(ast)) {
    if (existing_defn) {

      // Use the virtual table definition for comparison if there is one -- it's the parent node.
      // If only one of the tables is virtual then the text can't possibly match so we don't
      // need any special case logic for mix and match of virtual/non-virtual. And the error
      // message will include the text of both so it should be obvious what has happened.
      ast_node *existing_cmp = is_virtual_ast(existing_defn) ? existing_defn->parent : existing_defn;
      ast_node *current_cmp = is_virtual_ast(ast) ? ast->parent : ast;

      if (!sem_validate_identical_ddl(existing_cmp, current_cmp)) {
        report_error(name_ast, "CQL0103: duplicate table/view name", name);
        record_error(name_ast);
        record_error(ast);
      }
      goto cleanup;;
    }

    if (is_table_blob_storage(ast)) {
      sem_validate_table_for_blob_storage(ast);
      if (is_error(ast)) {
        goto cleanup;
      }
    }

    if (is_table_backing(ast)) {
      sem_validate_table_for_backing(ast);
      if (is_error(ast)) {
        goto cleanup;
      }
      ast->sem->sem_type |= SEM_TYPE_BACKING;
    }

    if (is_table_backed(ast)) {
      sem_validate_table_for_backed(ast);
      if (is_error(ast)) {
        goto cleanup;
      }
      ast->sem->sem_type |= SEM_TYPE_BACKED;
    }

    if (validating_previous_schema) {
      sem_validate_previous_table(ast);
    }
    else if (adding_current_entity) {
      // deleted or no it goes in the main list
      add_item_to_list(&all_tables_list, ast);

      // The name is consumed, some clients will use find_usable_and_not_deleted_table_or_view
      // to not see deleted views (e.g. select) others don't (e.g. drop)
      add_table_or_view(ast);

      sem_record_annotation_from_vers_info(&table_vers_info);
    }
  }

cleanup:
  current_table_name = NULL;
  current_table_ast = NULL;
}

// Semantic analysis for virtual tables is odd. The "virtual" part of the
// create virtual table is competely uninteresting to CQL. It is a module
// invocation to a module that CQL has no visibility into.  The arguments
// can be anything; in the SQLite language they can be literally a letter
// to gramma -- the only requirement is that the parens match.  CQL limits
// the args to the forms allowed in a misc attribute list. This is general
// enough to represent an arbitrary LISP program but not totally arbitrary,
// but it requires no validation beyond syntax!  So we're left with the
// part that tells us the table shape.
void sem_create_virtual_table_stmt(ast_node *ast) {
  Contract(is_ast_create_virtual_table_stmt(ast));

  EXTRACT_NOTNULL(module_info, ast->left);
  EXTRACT_NOTNULL(create_table_stmt, ast->right);
  EXTRACT_NOTNULL(create_table_name_flags, create_table_stmt->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_STRING(name, create_table_name_flags->right);
  EXTRACT_STRING(module_name, module_info->left);

  if (is_table_blob_storage(ast)) {
    report_invalid_blob_storage(ast, "it is a virtual table", name);
    return;
  }

  if (is_table_backing(ast)) {
    report_invalid_backing(ast, "it is a virtual table", name);
    return;
  }

  if (is_table_backed(ast)) {
    report_invalid_backed(ast, "it is a virtual table", name);
    return;
  }

  bool_t is_eponymous = !!(flags & VTAB_IS_EPONYMOUS);

  if (is_eponymous && Strcasecmp(name, module_name)) {
    CSTR err_msg = dup_printf(
         "CQL0447: virtual table '%s' claims to be eponymous but its module name '%s' differs from its table name",
         name, module_name);
    report_error(ast, err_msg, NULL);
    record_error(ast);
    return;
  }

  sem_create_table_stmt(create_table_stmt);
  if (is_error(create_table_stmt)) {
    record_error(ast);
    return;
  }

  // nothing else can go wrong, any module name is legal and any args are legal
  // the args are not checked against anything as they are only meaningful to
  // the module code that interprets them. In a very real sense CQL only
  // cares about the 'AS' part of the create table statement

  create_table_stmt->sem->sem_type |= SEM_TYPE_VIRTUAL;
  ast->sem = create_table_stmt->sem;
}

// Validate alter table add column
// * table must exist and not be a view (in any version)
// * column definition must be self-consistent
// * no auto increment columns may be added
// * no not nullable columns may be added
//
// Note: Alter statements are typically used in the context of migration so it's
// possible the table is gone in the latest version.  We still have to run
// the intervening upgrade steps so basically DDL gets to ignore the current
// state.
static void sem_alter_table_add_column_stmt(ast_node *ast) {
  Contract(is_ast_alter_table_add_column_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT(col_def, ast->right);

  // ALTER statements can be operating in the past, so we might be working on a table that is now deleted
  ast_node *table_ast = find_usable_table_or_view_even_deleted(name, name_ast, "CQL0144: table in alter statement does not exist");
  if (!table_ast) {
    record_error(ast);
    return;
  }

  name_ast->sem = table_ast->sem;

  if (!is_ast_create_table_stmt(table_ast)) {
    report_error(name_ast, "CQL0144: cannot alter a view", name);
    record_error(ast);
    return;
  }

  if (is_virtual_ast(table_ast)) {
    report_error(name_ast, "CQL0164: cannot use ALTER TABLE on a virtual table", name);
    record_error(ast);
    return;
  }

  version_attrs_info table_vers_info;
  init_version_attrs_info(&table_vers_info, name, table_ast, NULL);
  table_vers_info.create_version      = table_ast->sem->create_version;
  table_vers_info.delete_version      = table_ast->sem->delete_version;
  table_vers_info.recreate            = table_ast->sem->recreate;
  table_vers_info.recreate_group_name = table_ast->sem->recreate_group_name;

  coldef_info col_info;
  init_coldef_info(&col_info, &table_vers_info);

  sem_col_def(col_def, &col_info);
  if (is_error(col_def)) {
    record_error(ast);
    return;
  }

  if (col_info.create_version > 0 || col_info.delete_version > 0) {
    report_error(col_def, "CQL0145: version annotations not valid in alter statement", col_def->sem->name);
    record_error(ast);
    return;
  }

  if (col_info.autoinc_columns) {
    report_error(col_def, "CQL0146: adding an auto increment column is not allowed", col_def->sem->name);
    record_error(ast);
    return;
  }

  if (is_not_nullable(col_def->sem->sem_type) && !has_default(col_def->sem->sem_type)) {
    report_error(col_def, "CQL0147: adding a not nullable column with no default value is not allowed", col_def->sem->name);
    record_error(ast);
    return;
  }

  // CQL's world view is that the schema as declared is authoritative, so the alter should
  // already be reflected.  The only purpose of executing this statement is to bring the physical schema
  // up to date.

  EXTRACT_ANY_NOTNULL(col_key_list, table_ast->right);

  ast_node *def_found = NULL;

  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    // only columns
    if (!is_ast_col_def(def)) {
      continue;
    }

    // if the column is not marked for @create it doesn't count!
    if (def->sem->create_version < 0) {
      continue;
    }

    // if the column is logically deleted, it doesn't count
    if (is_deleted(def)) {
      continue;
    }

    // if the name matches, we found it!
    if (!strcmp(def->sem->name, col_def->sem->name)) {
      def_found = def;
      break;
    }
  }

  if (!def_found) {
    report_error(col_def, "CQL0148: added column must already be reflected in declared schema, with @create, exact name match required", col_def->sem->name);
    record_error(ast);
    return;
  }

  sem_t sem_type_added = col_def->sem->sem_type;
  sem_t sem_type_required = def_found->sem->sem_type;

  if (sem_type_added != sem_type_required) {
    report_error(col_def, "CQL0149: added column must be an exact match for the column type declared in the table", col_def->sem->name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Enables a nonnull improvement, if possible.
static void sem_set_notnull_improved(CSTR name, CSTR scope) {
  Contract(name);

  bool_t is_global = false;

  sem_t *type = find_mutable_type_and_global_status(name, scope, &is_global);
  Contract(type);

  // There's no need to proceed if this is a NOT NULL type or if it is a
  // nullable type that has already been improved.
  if (*type & (SEM_TYPE_NOTNULL | SEM_TYPE_INFERRED_NOTNULL)) {
    return;
  }

  // We keep track of globals as we need to un-improve all of them after every
  // procedure call. We need to do this due to the assumption that the procedure
  // could have set any number of globals to null.
  if (is_global) {
    // Since this is a global, record it as such.
    global_notnull_improvement_item *global_item = _ast_pool_new(global_notnull_improvement_item);
    global_item->type = type;
    global_item->next = global_notnull_improvements;
    global_notnull_improvements = global_item;
  }

  flow_set_flag_for_type(SEM_TYPE_INFERRED_NOTNULL, type);
}

// This needs to be called for everything that is no longer safe to consider NOT
// NULL due to a mutation. It is fine to call this for something not currently
// subject to improvement, but it must only be called with a name/scope pair
// referring to something has a mutable type (e.g., it must not be an unbound
// variable, a cursor used an expression, an enum case, et cetera).
static void sem_unset_notnull_improved(CSTR name, CSTR scope) {
  Contract(name);

  sem_t *type = find_mutable_type(name, scope);
  // There is no case in which we should ever attempt to unset something that
  // doesn't have a mutable type: Such a name/scope pair would've never been
  // improved to begin with (e.g., because our input program had an error in
  // which the name/scope pair was used without being defined), and no hazard
  // (e.g., SET or OUT args) allows something for which `find_mutable_type` will
  // return NULL (e.g., an enum case, 'rowid', or a cursor in an expression
  // position).
  Contract(type);

  // As in `sem_unset_notnull_improvements_in_context`, it is critical that we
  // do not unset an improvement if it is not currently set; see the comments
  // within `sem_unset_notnull_improvements_in_context` for details.
  if (*type & SEM_TYPE_INFERRED_NOTNULL) {
    flow_unset_flag_for_type(SEM_TYPE_INFERRED_NOTNULL, type);
  }
}

// Unsets notnull improvements for all currently improved globals.
static void sem_unset_global_notnull_improvements() {
  for (global_notnull_improvement_item *head = global_notnull_improvements; head; head = head->next) {
    if (*head->type & SEM_TYPE_INFERRED_NOTNULL) {
      flow_unset_flag_for_type(SEM_TYPE_INFERRED_NOTNULL, head->type);
    }
  }
}

// Given a conditional expression `ast` possibly containing AND-linked
// subexpressions, set all of the applicable nullability and has-row
// improvements within the current flow context. Generally speaking, calls to
// this function should be bounded by a new flow context corresponding to the
// portion of the program for which the condition `ast` must be be true.
static void sem_set_improvements_for_true_condition(ast_node* ast)
{
  Contract(ast);

  if (is_ast_and(ast)) {
    // We include all improvements along the outermost spine of AND expressions.
    Invariant(ast->left);
    Invariant(ast->right);
    sem_set_improvements_for_true_condition(ast->left);
    sem_set_improvements_for_true_condition(ast->right);
    return;
  }

  if (is_id(ast)) {
    // This is the "id" case. We can possibly make a has-row improvement.
    EXTRACT_STRING(name, ast);
    sem_t *type = find_mutable_type(name, NULL);
    if (type && is_auto_cursor(*type)) {
      // `ast` refers to a cursor an auto cursor. We can set a has-row
      // improvement accordingly.
      sem_set_has_row_improved(name);
      return;
    }
  }

  if (is_ast_is_not(ast) && is_ast_null(ast->right) && is_id_or_dot(ast->left)) {
    // This is the "id_or_dot IS NOT NULL" case. We can improve nullability
    // here.
    //
    // NOTE: Since calling a procedure as a function is only allowed if it
    // has exactly one trailing OUT parameter (which becomes the return value),
    // it cannot be the case that any call within `ast` could unset any
    // improvements we just made (because no variables might be passed as OUT
    // arguments therein). Were the aforementioned proc-as-func restriction not
    // in place, we'd need to deep-traverse `ast` looking for any calls with OUT
    // arguments as any improvements we just made could possibly be invalidated
    // by such calls. This was, indeed, the case in earlier versions of CQL.
    EXTRACT_NAME_AND_SCOPE(ast->left);
    sem_set_notnull_improved(name, scope);
    return;
  }
}

// Improvements for known-false conditions are dual to improvements for
// known-true conditions.
//
// For nullability, known-false conditions improve ids and dots verified to be
// NULL via `IS NULL` along the outermost spine of `OR` expressions, whereas
// known-true conditions improve ids and dots verified to be nonnull via `IS NOT
// NULL` along the outermost spine of `AND` expressions. For example, the
// following two statements introduce the same improvements:
//
//   IF a IS NOT NULL AND b IS NOT NULL THEN
//     -- `a` and `b` are improved here because we know the condition is true
//   END IF;
//
//   IF a IS NULL OR b IS NULL RETURN;
//   -- `a` and `b` are improved here because we know the condition is false
//   -- since we must not have returned if we got this far
//
// Likewise, for cursors, known-false conditions improve ids verified to not
// have a row along the outermost spine of `OR` expresions, whereas known-true
// conditions improve cursors verified to have a row along the outermost spine
// of `AND` expressions. Again, the following two statements introduce the same
// improvements:
//
//   IF c THEN
//     -- `c` is known to have a row here
//   END IF;
//
//   IF not c THEN RETURN;
//   -- `c` is known to have a row here
static void sem_set_improvements_for_false_condition(ast_node *ast) {
  Contract(ast);

  if (is_ast_or(ast)) {
    // As in `sem_set_improvements_for_true_condition`, we make sure to
    // recurse through the left side first in keeping with the order of
    // evaluation at runtime.
    sem_set_improvements_for_false_condition(ast->left);
    sem_set_improvements_for_false_condition(ast->right);
    return;
  }

  if (is_ast_not(ast) && is_id(ast->left)) {
    // This is the "NOT id" case. We can possibly make a has-row improvement.
    EXTRACT_STRING(name, ast->left);
    sem_t *type = find_mutable_type(name, NULL);
    if (type && is_auto_cursor(*type)) {
      // `ast->left` refers to an auto cursor. We can set a has-row improvement
      // accordingly.
      sem_set_has_row_improved(name);
      return;
    }
  }

  if (is_ast_is(ast) && is_ast_null(ast->right) && is_id_or_dot(ast->left)) {
    // This is the "id_or_dot IS NULL" case. We can improve nullability here.
    EXTRACT_ANY_NOTNULL(id_or_dot, ast->left);
    EXTRACT_NAME_AND_SCOPE(id_or_dot);
    sem_set_notnull_improved(name, scope);
    return;
  }
}

// This is the [expression] then [statements] part of an IF or ELSE IF
// Which is what we mean by a conditional action.  We have to validate
// that the condition is numeric and the statements have no errors.
// There's helper for all that.
static void sem_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  // [expr] THEN stmt_list
  sem_expr(expr);
  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  if (!is_numeric_expr(expr)) {
    report_error(expr, "CQL0150: expected numeric expression in IF predicate", NULL);
    record_error(ast);
    return;
  }

  if (stmt_list) {
    FLOW_PUSH_CONTEXT_BRANCH();
    // Add improvements for `stmt_list` where `expr` must be true.
    sem_set_improvements_for_true_condition(expr);
    sem_stmt_list_in_current_flow_context(stmt_list);
    FLOW_POP_CONTEXT_BRANCH();
    if (is_error(stmt_list)) {
      record_error(ast);
      return;
    }
  } else {
    flow_context_branch_group_add_empty_branch();
  }

  // If a later branch will be taken, `expr` must be false. Add its negative
  // improvements to the context created in `sem_if_stmt` so that all later
  // branches will be improved by the OR-linked spine of IS NULL checks in
  // `expr`.
  sem_set_improvements_for_false_condition(expr);

  ast->sem = expr->sem;
}

// Enables an initialization improvement for a variable if the improvement does
// not already exist.
static void sem_set_initialization_improved(CSTR name, CSTR scope) {
  Contract(name);

  sem_t *type = find_mutable_type(name, scope);
  Contract(type);
  Contract(is_variable(SEM_TYPE_VARIABLE));

  if (!(*type & SEM_TYPE_INIT_REQUIRED) || *type & SEM_TYPE_INIT_COMPLETE) {
    return;
  }

  flow_set_flag_for_type(SEM_TYPE_INIT_COMPLETE, type);
}

// Enables a has-row improvement for an auto cursor if the improvement does not
// already exist.
static void sem_set_has_row_improved(CSTR cursor_name) {
  Contract(cursor_name);

  sem_t *type = find_mutable_type(cursor_name, NULL);
  Contract(type);
  Contract(is_auto_cursor(*type));

  if (*type & SEM_TYPE_HAS_ROW) {
    return;
  }

  flow_set_flag_for_type(SEM_TYPE_HAS_ROW, type);
}

// Disables a has-row improvement for an auto cursor if the improvement exists.
// This must be called after every FETCH that is not guaranteed to result in a
// row.
static void sem_unset_has_row_improved(CSTR cursor_name) {
  Contract(cursor_name);

  sem_t *type = find_mutable_type(cursor_name, NULL);
  Contract(type);
  Contract(is_auto_cursor(*type));

  if (!(*type & SEM_TYPE_HAS_ROW)) {
    return;
  }

  flow_unset_flag_for_type(SEM_TYPE_HAS_ROW, type);
}

// This is the list of else-ifs, which is to say a linked list of
// conditional actions (see above).  We just walk the list and
// decorate each piece accordingly, if anything goes wrong mark the
// head with an error.
static void sem_elseif_list(ast_node *head) {
  Contract(is_ast_elseif(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_elseif(ast));
    EXTRACT_NOTNULL(cond_action, ast->left);

    // ELSE IF [cond_action]
    sem_cond_action(cond_action);
    if (is_error(cond_action)) {
      record_error(ast);
      record_error(head);
      return;
    }
    ast->sem = cond_action->sem;
  }
}

// Returns the first statement in a list of statements while taking attributes
// into account.
static ast_node *first_stmt_in_stmt_list(ast_node *ast) {
  Contract(is_ast_stmt_list(ast));

  EXTRACT_ANY_NOTNULL(stmt_or_stmt_and_attr, ast->left);

  if (is_ast_stmt_and_attr(stmt_or_stmt_and_attr)) {
    EXTRACT_ANY_NOTNULL(stmt, stmt_or_stmt_and_attr->right);
    return stmt;
  }

  return stmt_or_stmt_and_attr;
}

// Returns true if `ast` is a type of control statement, else false.
static bool_t is_control_stmt(ast_node *ast) {
  return is_ast_commit_return_stmt(ast)
    || is_ast_continue_stmt(ast)
    || is_ast_leave_stmt(ast)
    || is_ast_return_stmt(ast)
    || is_ast_rollback_return_stmt(ast)
    || is_ast_throw_stmt(ast);
}

// Given a `stmt_list`, perform a shallow search and return true if the
// statement list directly contains a control statement, else return false.
static bool_t stmt_list_contains_control_stmt(ast_node *ast) {
  Contract(is_ast_stmt_list(ast));

  for (ast_node *head = ast; head; head = head->right) {
    ast_node *stmt = first_stmt_in_stmt_list(head);
    if (is_control_stmt(stmt)) {
      return true;
    }
  }

  return false;
}

// The top level if node links the initial cond_action with a possible
// series of else_if nodes and then the else node.  All that happens
// at this point is decoding of the if pieces and calling out to the helpers.
// The else clause is the only thing that isn't a cond_action.  This is
// basically just calling out and marking errors up the stack as needed.
static void sem_if_stmt(ast_node *ast) {
  Contract(is_ast_if_stmt(ast));
  EXTRACT_NOTNULL(cond_action, ast->left);
  EXTRACT_NOTNULL(if_alt, ast->right);

  // Each branch gets its own flow context in `sem_cond_action` where its
  // condition is known to be true. We also create one more context for the
  // entire set of branches. In addition to grouping the branches together, this
  // outer context holds all of the negative improvements that result from the
  // knowledge that, if a given branch's statements are being evaluated, all
  // previous branches' conditions must have been false.
  FLOW_PUSH_CONTEXT_BRANCH_GROUP();

  // IF [cond_action]
  EXTRACT(elseif, if_alt->left);
  EXTRACT_NAMED(elsenode, else, if_alt->right);

  sem_cond_action(cond_action);
  if (is_error(cond_action)) {
    goto error;
  }

  if (elseif) {
    sem_elseif_list(elseif);
    if (is_error(elseif)) {
      record_error(if_alt);
      goto error;
    }
  }

  if (elsenode) {
    // ELSE [stmt_list]
    flow_set_context_branch_group_covers_all_cases(true);
    EXTRACT(stmt_list, elsenode->left);
    if (stmt_list) {
      FLOW_PUSH_CONTEXT_BRANCH();
      sem_stmt_list_in_current_flow_context(stmt_list);
      FLOW_POP_CONTEXT_BRANCH();
      if (is_error(stmt_list)) {
        record_error(elsenode);
        record_error(if_alt);
        goto error;
      }
    } else {
      flow_context_branch_group_add_empty_branch();
    }
    record_ok(elsenode);
  }

  record_ok(if_alt);

  ast->sem = cond_action->sem;
  // END IF

cleanup:
  FLOW_POP_CONTEXT_BRANCH_GROUP();

  if (is_error(ast)) {
    return;
  }

  // Check for use of the guard pattern, i.e., an IF with only a THEN block that
  // concludes with a control statement. If this IF follows the guard pattern,
  // then we can add improvements for the statements that follow due to the fact
  // that the condition must have been false.
  //
  // NOTE: The reason that no `elseif` or `elsenode` can be present for this to
  // be safe is that, if the THEN condition were false, those branches would
  // run, and they could do something that would invalidate the improvements
  // we're about to make (e.g., by setting something to NULL).
  if (!elseif && !elsenode) {
    EXTRACT(stmt_list, cond_action->right);
    if (stmt_list && stmt_list_contains_control_stmt(stmt_list)) {
      EXTRACT_ANY_NOTNULL(cond_expr, cond_action->left);
      sem_set_improvements_for_false_condition(cond_expr);
    }
  }

  return;

error:
  record_error(ast);
  goto cleanup;
}

// Guard statements are a restricted form of IF statement where the current
// block or procedure is exited when the guard condition is true. The valid
// forms are as follows:
//
//   IF expr COMMIT RETURN;
//   IF expr CONTINUE;
//   IF expr LEAVE;
//   IF expr RETURN;
//   IF expr ROLLBACK RETURN;
//   IF expr THROW;
//
//  As with IF statements, nullability improvements are possible.
static void sem_guard_stmt(ast_node *ast) {
  Contract(is_ast_guard_stmt(ast));
  EXTRACT_ANY_NOTNULL(control_stmt, ast->right);
  Invariant(is_control_stmt(control_stmt));

  rewrite_guard_stmt_to_if_stmt(ast);
}

// This is the delete analyzer, it sets up a joinscope for the table being
// deleted and the validates the WHERE if present against that joinscope.
// Additionally we verify that the table actually was defined and is not a view.
static void sem_delete_stmt(ast_node *ast) {
  Contract(is_ast_delete_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT(opt_where, ast->right);

  // DELETE FROM [name]

  ast_node *table_ast = find_usable_and_not_deleted_table_or_view(
    name,
    name_ast,
    "CQL0151: table in delete statement does not exist");
  if (!table_ast) {
    record_error(ast);
    return;
  }

  sem_non_blob_storage_table(ast, table_ast);
  if (is_error(ast)) {
    return;
  }

  name_ast->sem = table_ast->sem;

  if (!is_ast_create_table_stmt(table_ast)) {
    report_error(name_ast, "CQL0152: cannot delete from a view", name);
    record_error(ast);
    return;
  }

  PUSH_JOIN(where_scope, table_ast->sem->jptr);

  if (opt_where) {
    // WHERE
    sem_opt_where(opt_where);
    if (is_error(opt_where)) {
      record_error(ast);
      POP_JOIN();
      return;
    }
  }

  POP_JOIN();

  record_ok(ast);
}

// Top level WITH-DELETE form -- create the CTE context and then process
// the delete statement.
static void sem_with_delete_stmt(ast_node *stmt) {
  Contract(is_ast_with_delete_stmt(stmt));
  EXTRACT_ANY_NOTNULL(with_prefix, stmt->left)
  EXTRACT(cte_tables, with_prefix->left);
  EXTRACT_NOTNULL(delete_stmt, stmt->right);

  Invariant(cte_cur == NULL);

  sem_push_cte_state();

  sem_cte_tables(cte_tables);
  if (is_error(cte_tables)) {
    record_error(stmt);
    goto cleanup;
  }

  sem_delete_stmt(delete_stmt);

  if (is_error(delete_stmt)) {
    record_error(stmt);
    goto cleanup;
  }

  stmt->sem = delete_stmt->sem;

cleanup:
  sem_pop_cte_state();
}


// This is is the helper that computes the types in an update where
// you might go update foo set x = y.  This is the "set x = y" portion.
// This will be one update in a list of such updates.  The only trick here
// is that when setting, the left side must not be a variable, it has to be a column.
// To do this we temporarily hide the variables head.  We verify that the types
// are compatible and we also handle the special case of trying to set a
// not-nullable type to null.
static void sem_update_entry(ast_node *ast, symtab *update_columns) {
  Contract(is_ast_update_entry(ast));
  Contract(current_joinscope);

  // name = expr  or name1.name2 = expr

  EXTRACT_ANY_NOTNULL(left, ast->left);
  EXTRACT_ANY_NOTNULL(right, ast->right);

  {
    symtab *saved_locals = locals;
    symtab *saved_globals = globals;

    // hide variables for this expression, no variables on left of :=
    locals = globals = NULL;

    sem_expr(left);

    locals = saved_locals;
    globals = saved_globals;
  }

  if (is_error(left)) {
    record_error(ast);
    return;
  }

  if (!symtab_add(update_columns, left->sem->name, NULL)) {
    report_error(left, "CQL0153: duplicate target column name in update statement", left->sem->name);
    record_error(ast);
    record_error(left);
    return;
  }

  sem_root_expr(right, SEM_EXPR_CONTEXT_SELECT_LIST);
  if (is_error(right)) {
    record_error(ast);
    return;
  }

  sem_t sem_type_left = left->sem->sem_type;
  sem_t sem_type_right = right->sem->sem_type;

  Invariant(is_unitary(sem_type_left));
  Invariant(is_unitary(sem_type_right));
  Invariant(!is_object(sem_type_left));
  Invariant(!is_object(sem_type_right));

  if (!sem_verify_assignment(right, sem_type_left, sem_type_right, left->sem->name)) {
    record_error(ast);
    return;
  }

  sem_combine_kinds(right, left->sem->kind);
  if (is_error(right)) {
    record_error(ast);
    return;
  }

  ast->sem = left->sem;
}

// This is the list of updates we need to perform, we walk the list here and handle
// each one, reporting errors as we go.
static void sem_update_list(ast_node *head) {
  Contract(is_ast_update_list(head));

  symtab *update_columns = symtab_new();

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_update_list(ast));
    EXTRACT_NOTNULL(update_entry, ast->left);

    sem_update_entry(update_entry, update_columns);
    if (is_error(update_entry)) {
      record_error(head);
      symtab_delete(update_columns);
      return;
    }
  }

  symtab_delete(update_columns);

  record_ok(head);
}

// This is the top level update statement, it calls the update list helpers
// and sets up the joinscope for the table(s) being updated.  If there are
// optional clauses they are evaluated just like in a select statement
// with those same helper methods.
static void sem_update_stmt(ast_node *ast) {
  Contract(is_ast_update_stmt(ast));
  EXTRACT_ANY(name_ast, ast->left);
  EXTRACT_NOTNULL(update_set, ast->right);
  EXTRACT_NOTNULL(update_list, update_set->left);
  EXTRACT_NOTNULL(update_where, update_set->right);
  EXTRACT(opt_where, update_where->left);
  EXTRACT_NOTNULL(update_orderby, update_where->right);
  EXTRACT(opt_orderby, update_orderby->left);
  EXTRACT(opt_limit, update_orderby->right);
  ast_node *table_ast = NULL;

  // update [table] SET [update_list]

  if (name_ast) {
    EXTRACT_STRING(name, name_ast);

    table_ast = find_usable_and_not_deleted_table_or_view(
      name,
      name_ast,
      "CQL0154: table in update statement does not exist");
    if (!table_ast) {
      record_error(ast);
      return;
    }

    name_ast->sem = table_ast->sem;

    if (!is_ast_create_table_stmt(table_ast)) {
      report_error(name_ast, "CQL0155: cannot update a view", name);
      record_error(ast);
      return;
    }

    // This means we're in upsert statement subtree therefore the table name
    // should not be included in the update statement
    if (in_upsert) {
      report_error(name_ast, "CQL0281: upsert statement does not include table name in the update statement", name);
      record_error(ast);
      return;
    }
  } else {
    // This means we're in an upsert statement therefore the table name should not
    // be provided in the update statement of upsert otherwise it's a symantical error
    if (!in_upsert) {
      report_error(ast, "CQL0282: update statement require table name", NULL);
      record_error(ast);
      return;
    }
    Contract(current_upsert_table_ast);
    table_ast = current_upsert_table_ast;
  }

  ast->sem = table_ast->sem;

  // Any early out at this point is an error, cleanup is needed to get the POP_JOIN
  bool_t error = true;

  PUSH_JOIN(update_scope, table_ast->sem->jptr);

  sem_update_list(update_list);
  if (is_error(update_list)) {
    goto cleanup;
  }

  if (opt_where) {
    // WHERE
    sem_opt_where(opt_where);
    if (is_error(opt_where)) {
      goto cleanup;
    }
  }

  if (opt_orderby) {
    // ORDER BY
    sem_opt_orderby(opt_orderby);
    if (is_error(opt_orderby)) {
      goto cleanup;
    }
  }

  if (opt_limit) {
    // LIMIT
    sem_opt_limit(opt_limit);
    if (is_error(opt_limit)) {
      goto cleanup;
    }
  }

  error = false;

cleanup:

  if (error) {
    record_error(ast);
  }

  POP_JOIN();
}

// The column list specifies the columns we will provide, they must exist and be unique.
// The insert list specifies the values that are to be updated.
// The type of each value must match the type of the column.
// If there are too many or too few columns, that is also an error.
static void sem_update_cursor_stmt(ast_node *ast) {
  Contract(is_ast_update_cursor_stmt(ast));
  EXTRACT_ANY(cursor, ast->left);
  EXTRACT_STRING(name, cursor);
  EXTRACT_ANY_NOTNULL(columns_values, ast->right);

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  // expr_names node is a sugar syntax we need to rewrite [USING ...] part to [FROM VALUES(...)]
  if (is_ast_expr_names(columns_values)) {
    rewrite_expr_names_to_columns_values(columns_values);
    Contract(is_ast_columns_values(columns_values));
  }

  rewrite_empty_column_list(columns_values, cursor->sem->sptr);

  rewrite_like_column_spec_if_needed(columns_values);
  if (is_error(columns_values)) {
    record_error(ast);
    return;
  }

  rewrite_from_shape_if_needed(ast, columns_values);
  if (is_error(ast)) {
    return;
  }

  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT_ANY_NOTNULL(name_list, column_spec->left);
  EXTRACT_ANY_NOTNULL(insert_list, columns_values->right);

  // if there are any FROM C(like shape) thing in the values list, expand them
  if (!rewrite_shape_forms_in_list_if_needed(insert_list)) {
    record_error(ast);
    return;
  }

  sem_t sem_type = cursor->sem->sem_type;

  // We can't do this if the cursor was not used with the auto syntax
  if (!is_auto_cursor(sem_type)) {
    report_error(cursor, "CQL0067: cursor was not used with 'fetch [cursor]'", name);
    record_error(cursor);
    record_error(ast);
    return;
  }

  // count values
  uint32_t cols = 0;

  for (ast_node *item = insert_list; item; item = item->right) {
    cols++;
  }

  sem_join *jptr = sem_join_from_sem_struct(cursor->sem->sptr);

  // check the column names for uniqueness, build a symbol table of them
  name_check check;
  init_name_check(&check, name_list, jptr);
  bool_t valid = sem_name_check(&check);

  // Ensure that the number of values matches the number of columns.
  if (valid && check.count != cols) {
    report_error(ast, "CQL0157: count of columns differs from count of values", NULL);
    valid = 0;
  }

  if (valid) {
    valid = sem_validate_compatible_cols_vals(name_list, insert_list);
  }

  destroy_name_check(&check);

  if (valid) {
    record_ok(ast);
  }
  else {
    record_error(ast);
  }
}

// Top level WITH-UPDATE form -- create the CTE context and then process
// the update statement.
static void sem_with_update_stmt(ast_node *stmt) {
  Contract(is_ast_with_update_stmt(stmt));
  EXTRACT_ANY_NOTNULL(with_prefix, stmt->left)
  EXTRACT(cte_tables, with_prefix->left);
  EXTRACT_NOTNULL(update_stmt, stmt->right);

  Invariant(cte_cur == NULL);

  sem_push_cte_state();

  sem_cte_tables(cte_tables);
  if (is_error(cte_tables)) {
    record_error(stmt);
    goto cleanup;
  }

  sem_update_stmt(update_stmt);

  if (is_error(update_stmt)) {
    record_error(stmt);
    goto cleanup;
  }

  stmt->sem = update_stmt->sem;

cleanup:
  sem_pop_cte_state();
}

static int32_t sem_insert_dummy_spec(ast_node *ast) {
  EXTRACT_ANY_NOTNULL(seed_expr, ast->left);
  EXTRACT_OPTION(flags, ast->right);

  sem_root_expr(seed_expr, SEM_EXPR_CONTEXT_NONE);
  if (is_error(seed_expr)) {
    record_error(ast);
    return false;
  }

  ast->sem = seed_expr->sem;
  sem_t sem_type = seed_expr->sem->sem_type;

  if (is_nullable(sem_type) || core_type_of(sem_type) != SEM_TYPE_INTEGER) {
    report_error(seed_expr, "CQL0156: seed expression must be a non-nullable integer", NULL);
    record_error(ast);
    return false;
  }

  return flags;
}

// If we're here then we need to verify that the insert we're dealing with is
// the values form with exactly one value row.  If it is then we hoist out that
// row.  We do this so that we can still do dummy defaults on the simple
// insert form even though it looks like a select statement.  It can be just
// a value list.
static bool_t sem_validate_one_values_row(ast_node *stmt, ast_node **insert_list) {
  Contract(insert_list);
  *insert_list = NULL;

  // not a simple select (could be WITH...SELECT, or EXPLAIN or some such)
  if (!is_ast_select_stmt(stmt)) {
    goto error;
  }

  EXTRACT_NAMED_NOTNULL(any_select_stmt, select_stmt, stmt);
  EXTRACT_NOTNULL(select_core_list, any_select_stmt->left);
  EXTRACT_NOTNULL(select_core, select_core_list->left);
  EXTRACT(select_core_compound, select_core_list->right);
  EXTRACT_ANY(select_values, select_core->left);

  // not VALUES at all, can't rewrite this.
  if (!is_ast_select_values(select_values)) {
    goto error;
  }

  // INSERT [conflict resolution] INTO name [( name_list )] VALUES [(...) ...] [insert_dummy_spec]
  // dummy_insert_spec feature support VALUES with a single row of values
  // this is because there is only one seed expression and you likely want
  // each row to have some unique seed value. This this is only intended for
  // test code creating dummy data this restriction simplifies things for
  // everyone.  Make your dummy rows in a loop rather than with a value set.
  // One row at a time is all we need.
  if (select_core_compound != NULL) {
    goto error;
  }

  EXTRACT_NOTNULL(values, select_core->right);

  // INSERT [conflict resolution] INTO name [( name_list )] [VALUES (...), (...), ...]
  // We have multiple values in select_values ast to be handled. This is not allowed
  if (values->right != NULL) {
    goto error;
  }

  // At this point we know we have validated: INSERT [conflict resolution] INTO name [( name_list )] VALUES(...) [insert_dummy_spec]
  // We need to rewrite the columns_values->right node by replacing select_stmt node value with insert_list node.
  EXTRACT_NAMED(values_insert_list, insert_list, values->left);
  *insert_list = values_insert_list;
  return true;

error:
  report_error(stmt, "CQL0334: @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row", NULL);
  record_error(stmt);
  return false;
}

// The column list specifies the columns we will provide, they must exist and be unique.
// The columns specified must suffice to insert a row (all not nulls and not default present)
// The insert list specifies the values that are to be inserted.
// The type of each value must match the type of the column.
// Autoinc columns may be specified as NULL.
// If there are too many or too few columns, that is also an error.
// By the time we get here, column_spec is populated with a synthetic (possibly empty)
// list and we have normalized on the insert columns form.
static void sem_column_spec_and_values(ast_node *ast, ast_node *table_ast) {
  Contract(is_ast_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(insert_type, ast->left);
  EXTRACT_NOTNULL(name_columns_values, ast->right);
  EXTRACT_NOTNULL(columns_values, name_columns_values->right);
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT(name_list, column_spec->left);
  EXTRACT(insert_dummy_spec, insert_type->left);

  ast_node *select_stmt = NULL;
  ast_node *insert_list = NULL;

  EXTRACT_ANY_NOTNULL(value_source, columns_values->right);

  // We need an insert list for dummy specs... if we don't have one then
  // we check to see if we need to do the easy rewrite.
  if (is_ast_insert_list(value_source)) {
    // We might already columns/values due to rewriting say "FROM ARGUMENTS"
    // this is always good to go.
    insert_list = value_source;
  }
  else if (insert_dummy_spec) {
    // We need the values form if there is a dummy spec... so let's see if we can convert it.
    if (!sem_validate_one_values_row(value_source, &insert_list)) {
      // it doesn't have a simple insert list, it's an error then.
      record_error(ast);
      record_error(columns_values);
      return;
    }
    // we're back on track with the simple form known to be good
    // note, it still might be an empty insert_list, that's allowed.
    ast_set_right(columns_values, insert_list);
  }
  else {
    // Since the insert statement does not have insert_dummy_spec, then we
    // don't need to do any rewrite.  Any select form is allowed then.
    Contract(is_select_stmt(value_source));
    select_stmt = value_source;
  }

  Contract(table_ast->sem);
  Contract(table_ast->sem->sptr);
  Contract(table_ast->sem->jptr);

  int32_t dummy_flags = 0;

  if (insert_dummy_spec) {
    dummy_flags = sem_insert_dummy_spec(insert_dummy_spec);
    if (is_error(insert_dummy_spec)) {
      record_error(ast);
      return;
    }
  }

  if (select_stmt) {
    sem_select(select_stmt);
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    if (enforcement.strict_insert_select && sem_select_stmt_is_mixed_results(select_stmt)) {
      report_error(select_stmt,
        "CQL0370: due to a memory leak bug in old SQLite versions, "
        "the select part of an insert must not have a top level join or compound operator. "
        "Use WITH and a CTE, or a nested select to work around this.", NULL);
      record_error(select_stmt);
      record_error(ast);
      return;
    }
  }

  bool_t valid = 1;

  // count values, find end of the value list
  ast_node *insert_list_tail = NULL;
  uint32_t cols = 0;

  if (select_stmt) {
    cols = select_stmt->sem->sptr->count;
  }
  else {
    for (ast_node *item = insert_list; item; item = item->right) {
      insert_list_tail = item;
      cols++;
    }
  }

  // check the column names for uniqueness, build a symbol table of them
  name_check check;
  init_name_check(&check, name_list, table_ast->sem->jptr);
  valid = sem_name_check(&check);

  symtab *column_names = check.names;
  ast_node *name_list_tail = check.name_list_tail;

  // Ensure that the number of values matches the number of columns.
  if (valid && check.count != cols) {
    report_error(ast, "CQL0157: count of columns differs from count of values", NULL);
    valid = 0;
  }

  if (valid) {
    // ensure that all the necessary columns are present in some order

    sem_struct *sptr = table_ast->sem->sptr;
    for (int32_t icol = 0; icol < sptr->count; icol++) {
      sem_t sem_type_col = sptr->semtypes[icol];
      CSTR name = sptr->names[icol];

      if (symtab_find(column_names, name)) {
        continue;
      }

      if (has_default(sem_type_col) && !(dummy_flags & INSERT_DUMMY_DEFAULTS)) {
        continue;
      }

      if (is_nullable(sem_type_col) && !(dummy_flags & INSERT_DUMMY_NULLABLES)) {
        continue;
      }

      if (sem_type_col & SEM_TYPE_HIDDEN_COL) {
        continue;
      }

      if (!insert_dummy_spec) {
        report_error(ast, "CQL0158: required column missing in INSERT statement", sptr->names[icol]);
        valid = 0;
        break;
      }

      // if we get this far then we're going to re-write the AST for the missing columns

      // the select statement alternative has no dummy values
      Invariant(!select_stmt);

      // insert the dummy value into the two lists, there's a lot of state here.

      Invariant(column_spec);  // still set up

      dummy_info info;
      info.name = name;
      info.sem_type_col = sem_type_col;
      info.jptr = table_ast->sem->jptr;
      info.name_list_tail = name_list_tail;
      info.name_list_head = column_spec->left;
      info.insert_list_tail = insert_list_tail;
      info.insert_list_head = columns_values->right;
      info.use_null = 0;

      AST_REWRITE_INFO_SET(column_spec->lineno, column_spec->filename);

      sem_synthesize_dummy_value(&info);

      AST_REWRITE_INFO_RESET();

      name_list = info.name_list_head;
      name_list_tail = info.name_list_tail;
      insert_list = info.insert_list_head;
      insert_list_tail = info.insert_list_tail;

      ast_set_left(column_spec, name_list);
      ast_set_right(columns_values, insert_list);
    }
  }

  if (valid) {
    if (select_stmt) {
      valid = sem_validate_compatible_table_cols_select(table_ast, name_list, select_stmt);
    }
    else {
      valid = sem_validate_compatible_table_cols_vals(table_ast, name_list, insert_list);
    }
  }

  destroy_name_check(&check);

  if (valid) {
    record_ok(ast);
  }
  else {
    record_error(ast);
  }
}

// This is the top level insert statement
// We check that the table exists and then we walk the columns and the value list
// using the  helper above to make sure they are valid for the table.
// Also we cannot insert into a view.
static void sem_insert_stmt(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(insert_type, ast->left);
  EXTRACT_NOTNULL(name_columns_values, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, name_columns_values->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);
  EXTRACT(insert_dummy_spec, insert_type->left);

  // INSERT [conflict resolution] INTO name [( name_list )] VALUES (insert_list)
  // INSERT [conflict resolution] INTO name [( name_list )] SELECT ...
  // INSERT [conflict resolution] INTO name [( name_list )] FROM ARGUMENTS

  ast_node *table_ast = find_usable_and_not_deleted_table_or_view(
    name,
    name_ast,
    "CQL0160: table in insert statement does not exist");
  if (!table_ast) {
    record_error(ast);
    return;
  }

  name_ast->sem = table_ast->sem;

  if (!is_ast_create_table_stmt(table_ast)) {
    report_error(name_ast, "CQL0161: cannot insert into a view", name);
    record_error(ast);
    return;
  }

  // expr_names node is a sugar syntax we need to rewrite it to a SQL syntax
  if (is_ast_expr_names(columns_values)) {
    rewrite_expr_names_to_columns_values(columns_values);
    Contract(is_ast_columns_values(columns_values));
  }

  // here we look for the sugar form INSERT foo USING select ... and rewrite it
  // we just need to make sure the select is semantically ok and has names we can use
  // the rewrite itself will just create a name list, easy sugar.
  if (is_select_stmt(columns_values)) {
    sem_select_stmt(columns_values);
    if (is_error(columns_values)) {
      record_error(ast);
      return;
    }

    sem_verify_no_anon_columns(columns_values);
    if (is_error(columns_values)) {
      record_error(ast);
      return;
    }

    rewrite_select_stmt_to_columns_values(columns_values);
    Contract(is_ast_columns_values(columns_values));
  }

  // This means we're in upsert tree. We want to record table_ast to be used
  // later for semantic analysis in other part of the upsert tree. But also do
  // semantic analysis on this insert_stmt node.
  if (in_upsert) {
    if (!is_ast_insert_normal(insert_type)) {
      report_error(insert_type, "CQL0283: upsert syntax only supports INSERT INTO", name);
      record_error(ast);
      return;
    }
    else if (is_ast_default_columns_values(columns_values)) {
      // INSERT [conflict resolution] INTO name DEFAULT VALUES
      // insert statement with default values can not be used in an upsert statement
      report_error(insert_type, "CQL0316: upsert-clause is not compatible with DEFAULT VALUES", name);
      record_error(ast);
      return;
    }

    Contract(!current_upsert_table_ast);
    current_upsert_table_ast = table_ast;
  }

  if (is_ast_columns_values(columns_values)) {
    rewrite_empty_column_list(columns_values, table_ast->sem->sptr);

    rewrite_like_column_spec_if_needed(columns_values);
    if (is_error(columns_values)) {
      record_error(ast);
      return;
    }

    rewrite_from_shape_if_needed(ast, columns_values);
    if (is_error(ast)) {
      return;
    }

    sem_column_spec_and_values(ast, table_ast);
  }
  else {
    // INSERT [conflict resolution] INTO name DEFAULT VALUES
    Contract(is_ast_default_columns_values(columns_values));

    // We need to do semantic analysis to validate all columns type in the table
    // are compatible. A columns is compatible if:
    // - has default value, including autoincrement column
    // - or is nullable. The default value for a nullable column is NULL.
    sem_struct *sptr = table_ast->sem->sptr;
    for (int32_t i = 0; i < sptr->count; i++) {
      sem_t type = sptr->semtypes[i];
      if (!is_nullable(type) && !has_default(type) && !has_autoincrement(type)) {
        report_error(
          name_ast,
          "CQL0315: mandatory column with no default value in INSERT INTO name DEFAULT VALUES statement",
          sptr->names[i]);
        record_error(ast);
        return;
      }
    }
    record_ok(ast);
  }
}

// Recursively goes through all the node to find the root select_stmt with SELECT token and
// check whether or not it has WHERE clause.
static bool_t is_root_select_stmt_has_opt_where_node (ast_node *ast, int32_t *select_count) {
  if (!ast || is_primitive(ast)) {
    return false;
  }

  // we're only checking the root select stmt. The nested select stmt are skipped
  if (is_select_stmt(ast)) {
    EXTRACT_NOTNULL(select_core_list, ast->left);
    EXTRACT_NOTNULL(select_core, select_core_list->left);
    if (is_ast_select_values(select_core->left)) {
      // The select_stmt with SELECT clause is the one we're tracking as part of the upsert pattern
      // in this function. However, the [VALUES (...), (...)] is also a select_stmt. In the upsert
      // form we require that the select statement has a WHERE clause. The [VALUES(...), (...)] form
      // can't have a WHERE clause, so we skip this subtree.
      return false;
    }
    if (*select_count == 0) {
      return false;
    } else {
      (*select_count) -= 1;
    }
  }

  if (is_ast_opt_where(ast)) {
    return true;
  }

  return is_root_select_stmt_has_opt_where_node(ast->left, select_count) ||
         is_root_select_stmt_has_opt_where_node(ast->right, select_count);
}

// Top level WITH-UPSERT form -- create the CTE context and then process
// the upsert statement.
static void sem_with_upsert_stmt(ast_node *stmt) {
  Contract(is_ast_with_upsert_stmt(stmt));
  EXTRACT_ANY_NOTNULL(with_prefix, stmt->left)
  EXTRACT(cte_tables, with_prefix->left);
  EXTRACT_NOTNULL(upsert_stmt, stmt->right);

  Invariant(cte_cur == NULL);

  sem_push_cte_state();

  sem_cte_tables(cte_tables);
  if (is_error(cte_tables)) {
    record_error(stmt);
    goto cleanup;
  }

  sem_upsert_stmt(upsert_stmt);

  if (is_error(upsert_stmt)) {
    record_error(stmt);
    goto cleanup;
  }

  stmt->sem = upsert_stmt->sem;

cleanup:
  sem_pop_cte_state();
}

// The semantic analysis of upsert_stmt consist in runing insert_stmt, update_stmt
// analysis and validate node (conflict_target) which belong only to upsert tree
static void sem_upsert_stmt(ast_node *stmt) {
  Contract(is_ast_upsert_stmt(stmt) && !in_upsert && !current_upsert_table_ast);
  if (enforcement.strict_upsert_stmt) {
    report_error(stmt, "CQL0289: upsert statement are forbidden if strict upsert statement mode is enabled", NULL);
    record_error(stmt);
    return;
  }

  EXTRACT_NOTNULL(insert_stmt, stmt->left);
  EXTRACT_NOTNULL(upsert_update, stmt->right);
  EXTRACT(conflict_target, upsert_update->left);
  EXTRACT(update_stmt, upsert_update->right);
  EXTRACT(indexed_columns, conflict_target->left);
  EXTRACT(opt_where, conflict_target->right);
  in_upsert = 1;

  // insert_stmt ON CONFLICT ([indexed_columns]) [WHERE ...] DO [UPDATE ...]

  sem_insert_stmt(insert_stmt);
  if (is_error(insert_stmt)) {
    goto error;
  }

  // Make sure this attribute was populated by sem_insert_stmt(...)
  Contract(is_ast_create_table_stmt(current_upsert_table_ast));

  // grab the columns portion from the insert statement ast
  EXTRACT_NOTNULL(name_columns_values, insert_stmt->right);
  EXTRACT_NOTNULL(columns_values, name_columns_values->right);
  EXTRACT(column_spec, columns_values->left);
  EXTRACT(name_list, column_spec->left);

  // The columns we attempted to insert will form the columns of the "excluded" table which
  // we put into scope by pushing a join onto the joinscope stack, we'll do that when
  // we process the update (but not the insert or the where on the insert)
  // for now we just get the type ready.
  uint32_t names_count = 0;
  ast_node *ast = name_list;
  for ( ;ast; ast = ast->right) names_count++;

  sem_struct *sptr = new_sem_struct("excluded", names_count);
  ast = name_list;
  for (int32_t i = 0; i < names_count; i++, ast = ast->right) {
    sptr->semtypes[i] = ast->left->sem->sem_type;
    sptr->names[i] = ast->left->sem->name;
    sptr->kinds[i] = ast->left->sem->kind;
  }

  // We'll store the resultant type in the AST as well on the conflict target
  // which gives us the ability to see it in the test output.
  conflict_target->sem = new_sem(SEM_TYPE_STRUCT);
  conflict_target->sem->sptr = sptr;

  // and here is our join target
  sem_join *jptr_excluded = sem_join_from_sem_struct(sptr);

  int32_t select_count = 1;
  bool_t found_where_stmt = is_root_select_stmt_has_opt_where_node(insert_stmt, &select_count);
  if (select_count == 0 && !found_where_stmt) {
    report_error(insert_stmt, "CQL0280: upsert statement requires a where clause if the insert clause uses select", NULL);
    record_error(insert_stmt);
    goto error;
  }

  if (indexed_columns) {
    if (!sem_validate_name_list(indexed_columns, current_upsert_table_ast->sem->jptr)) {
      record_error(upsert_update);
      record_error(conflict_target);
      goto error;
    }

    bool_t is_single_unique_key = 0;
    // if we only have one column listed in the indexed_columns node then we
    // check if that column is a single primary or unique key in the table
    if (!indexed_columns->right) {
      EXTRACT_NOTNULL(indexed_column, indexed_columns->left);
      EXTRACT_STRING(column_name, indexed_column->left);
      is_single_unique_key = is_column_unique_key(current_upsert_table_ast, column_name);
    }

    // we are going to check if all the columns in indexed_columns node are
    // - a unique key (UNIQUE (...) OR UNIQUE CONSTRAINT (...))
    // - unique index (CREATE UNIQUE INDEX ...)
    // - a group of primary key (PRIMARY KEY (a,b,...)).
    if (!is_single_unique_key) {
      bool_t valid = find_referenceable_columns(current_upsert_table_ast,
                                                validate_referenceable_fk_def_callback,
                                                indexed_columns);
      if (!valid) {
        report_error(indexed_columns, "CQL0279: columns referenced in an UPSERT conflict target must exactly match a unique key the target table", NULL);
        record_error(upsert_update);
        record_error(conflict_target);
        goto error;
      }
    }
  }

  if (opt_where) {
    // The opt_where node is in the upsert context therefore we need to make sure
    // we register a join context for search
    PUSH_JOIN_BLOCK()
    PUSH_JOIN(upsert_scope, current_upsert_table_ast->sem->jptr)
    sem_opt_where(opt_where);
    POP_JOIN()
    POP_JOIN()
    if (is_error(opt_where)) {
      record_error(upsert_update);
      record_error(conflict_target);
      goto error;
    }
  }

  if (update_stmt) {
    PUSH_JOIN(update_scope, jptr_excluded);
    sem_update_stmt(update_stmt);
    POP_JOIN();
    if (is_error(update_stmt)) {
      record_error(upsert_update);
      goto error;
    }
  }

  stmt->sem = insert_stmt->sem;
  record_ok(upsert_update);

cleanup:
  in_upsert = 0;
  current_upsert_table_ast = NULL;

  return;

error:
  record_error(stmt);
  goto cleanup;
}

// Top level WITH-INSERT form -- create the CTE context and then process
// the insert statement.
static void sem_with_insert_stmt(ast_node *stmt) {
  Contract(is_ast_with_insert_stmt(stmt));
  EXTRACT_ANY_NOTNULL(with_prefix, stmt->left)
  EXTRACT(cte_tables, with_prefix->left);
  EXTRACT_NOTNULL(insert_stmt, stmt->right);

  Invariant(cte_cur == NULL);

  sem_push_cte_state();

  sem_cte_tables(cte_tables);
  if (is_error(cte_tables)) {
    record_error(stmt);
    goto cleanup;
  }

  sem_insert_stmt(insert_stmt);

  if (is_error(insert_stmt)) {
    record_error(stmt);
    goto cleanup;
  }

  stmt->sem = insert_stmt->sem;

cleanup:
  sem_pop_cte_state();
}

// This code works for any of the cursor to/from blob forms as the checks are the same
// the blob might be the source or the destination, the last two args tell us which way it is
// which mostly doesn't matter.  We ensure that the blob is a blob and the cursor is a cursor
// and the blob kind tells us a struct that has the same columns as the cursor.  We
// only need to know the destination so that when we check if the columns are the same
// we can complaint accurately that the source doesn't match the target rather than
// the reverse.
cql_noexport void sem_validate_cursor_blob_compat(
  ast_node *ast_error,
  ast_node *cursor,
  ast_node *blob,
  ast_node *dest,
  ast_node *src)
{
  Contract(blob);
  Contract(cursor);
  Contract(dest);
  Contract(src);

  Contract(blob != cursor);
  Contract(src != dest);
  Contract(src == blob || src == cursor);
  Contract(dest == blob || dest == cursor);

  sem_expr(blob);
  if (is_error(blob)) {
    record_error(ast_error);
    return;
  }

  // the blob must be a blob
  if (!is_blob(blob->sem->sem_type)) {
    report_error(blob, "CQL0461: fetch from blob operand is not a blob", NULL);
    record_error(ast_error);
    return;
  }

  // and the cursor must be a cursor
  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast_error);
    return;
  }

  if (!is_auto_cursor(cursor->sem->sem_type)) {
    report_error(cursor, "CQL0454: cursor was not declared for storage", cursor->sem->name);
    record_error(ast_error);
    return;
  }

  // Note that the blob might have been rewritten due to notnull improvement
  // but that's ok, we only need the name and it's in the sem node for us now.
  // If the expression is such that it doesn't have a name that's ok too.
  CSTR kind = blob->sem->kind;
  CSTR blob_name = blob->sem->name;

  if (!kind) {
    report_error(blob, "CQL0455: blob variable must have a type-kind for type safety", blob_name);
    record_error(ast_error);
    return;
  }

  ast_node *table_ast = find_usable_and_not_deleted_table_or_view(
      kind,
      blob,
      "CQL0453: blob type is not a valid table");
  if (!table_ast) {
    record_error(ast_error);
    return;
  }

  if (!is_ast_create_table_stmt(table_ast)) {
    report_error(blob, "CQL0456: blob type is a view, not a table", kind);
    record_error(ast_error);
    return;
  }

  if (!is_table_blob_storage(table_ast)) {
    report_error(blob, "CQL0457: the indicated table is not marked with @attribute(cql:blob_storage)", kind);
    record_error(ast_error);
    return;
  }

  blob->sem->sptr = table_ast->sem->sptr;

  sem_verify_identical_columns(dest, src, "in the cursor and the blob type");
  if (is_error(src)) {
    record_error(ast_error);
    return;
  }

  // Now we need to mark the cursor as requiring the serializer helpers
  // so that we know to code-gen them later.  Since we want to do this
  // during the cursor declare we need to put this on the declaration.
  // Therefore, we're mutating the flags in place so as to change the
  // various linked places this type is used in particular we want to mutate
  // the semantic info in the declare cursor node.  This is why we
  // don't make a new semantic node.

  ast_node *var = find_local_or_global_variable(cursor->sem->name);
  Invariant(var); // we know the cursor exists and is unique already
  var->sem->sem_type |= SEM_TYPE_SERIALIZE;

  record_ok(ast_error);
  return;
}

// Check for matching cursor and blob
static void sem_fetch_cursor_from_blob_stmt(ast_node *ast) {
  Contract(is_ast_fetch_cursor_from_blob_stmt(ast));
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_ANY_NOTNULL(blob, ast->right);

  // the final error is set by the helper, 'ast' will have the code regardless
  sem_validate_cursor_blob_compat(ast, cursor, blob, cursor, blob);
}

// Check for matching cursor and blob
static void sem_set_blob_from_cursor_stmt(ast_node *ast) {
  Contract(is_ast_set_blob_from_cursor_stmt(ast));
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  EXTRACT_ANY_NOTNULL(blob, ast->left);
  EXTRACT_ANY_NOTNULL(cursor, ast->right);

  // the final error is set by the helper, 'ast' will have the code regardless
  sem_validate_cursor_blob_compat(ast, cursor, blob, blob, cursor);
}

// This is the statement used for loading a value cursor from ... values
// There are a number of forms, but importantly all of these apply to value
// cursors, not statement cursors.  So we're never dealing with a sqlite statement
// here, just columns.  They could be being loaded from anywhere.
// The general forms:
//   fetch C(cols) from values (values) [insert_dummy_spec]
//   fetch C from shape
// The "from shape" case is sugar; it is immediately rewritten into
// the normal fetch from values form where the values are the proc arguments.
// In addition if the shape is the name of a single blob variable
// with no other qualifiers then this is the form to convert blobs to cursors
// or likewise if the target is a blob then this converts cursors to blobs.
// The statement type is rewritten in sem_try_rewrite_blob_fetch_forms
// So this leaves us with the first form.
//   * if the name list is empty that's the same as listing every columnm,
//     so that is rewritten as well (more sugar)
//   * we have to check all the names against all the values for type compatibility
//   * we have to ensure that all the non-null values of the cursor were specified
//   * we use all the columns that are present, if there are any non-null columns missing
//     then we give an error unless dummy data is specified.  If dummy data is specified
//     then we rewrite the value list and the column list to add the needed columns and
//     use a dummy value.
// Note: fetch values doesn't go through SQlite and so we can't use the (CAST printf() as blob) trick
//     to make dummy blob data.  Well, we could but it would have to be even more complex.
//     (select CAST(printf(...) as blob))  which seems excessive so we just punt.
static void sem_fetch_values_stmt(ast_node *ast) {
  Contract(is_ast_fetch_values_stmt(ast));
  Contract(!current_joinscope);  // I don't belong inside a select(!)

  EXTRACT(insert_dummy_spec, ast->left);
  EXTRACT_NOTNULL(name_columns_values, ast->right);
  EXTRACT_ANY_NOTNULL(cursor, name_columns_values->left)
  EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);

  // FETCH name [( name_list )] FROM VALUES (insert_list) [insert_dummy_spec]
  // FETCH name FROM ARGUMENTS;  (rewritten into the first form)
  // FETCH name [(name_list )] FROM ARGUMENTS; (rewritten into the first form)
  // FETCH name USING expr_names;

  if (try_rewrite_blob_fetch_forms(ast)) {
    // true means affirmative success or failure and errors logged already
    has_dml = 1;  // this implies return code and all that comes with it
    if (!is_error(ast)) {
      // This type of fetch can fail with an exception. If it does not, we
      // definitely have a row. If it does, we'll be bumped out of the nearest
      // enclosing jump context and this improvement will be unset
      // appropriately.
      sem_set_has_row_improved(cursor->sem->name);
    }
    return;
  }

  // not the blob form, we proceed as usual

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  if (!(cursor->sem->sem_type & SEM_TYPE_VALUE_CURSOR)) {
    report_error(cursor, "CQL0165: fetch values is only for value cursors, not for sqlite cursors", cursor->sem->name);
    record_error(ast);
    return;
  }

  if (is_ast_expr_names(columns_values)) {
    rewrite_expr_names_to_columns_values(columns_values);
  }
  Invariant(is_ast_columns_values(columns_values));

  rewrite_empty_column_list(columns_values, cursor->sem->sptr);

  rewrite_like_column_spec_if_needed(columns_values);
  if (is_error(columns_values)) {
    record_error(ast);
    return;
  }

  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT(name_list, column_spec->left);

  rewrite_from_shape_if_needed(ast, columns_values);
  if (is_error(ast)) {
    return;
  }

  // this may have be rewritten by the above
  EXTRACT(insert_list, columns_values->right);

  // if there are any FROM C(like shape) thing in the values list, expand them
  if (!rewrite_shape_forms_in_list_if_needed(insert_list)) {
    record_error(ast);
    return;
  }

  int32_t dummy_flags = 0;

  if (insert_dummy_spec) {
    dummy_flags = sem_insert_dummy_spec(insert_dummy_spec);
    if (is_error(insert_dummy_spec)) {
      record_error(ast);
      return;
    }
  }

  bool_t valid = 1;

  // count values, find end of the value list
  ast_node *insert_list_tail = NULL;
  int32_t cols = 0;
  for (ast_node *item = insert_list; item; item = item->right) {
    insert_list_tail = item;
    cols++;
  }

  cursor->sem->jptr = sem_join_from_sem_struct(cursor->sem->sptr);

  // check the column names for uniqueness, build a symbol table of them
  name_check check;
  init_name_check(&check, name_list, cursor->sem->jptr);
  valid = sem_name_check(&check);

  symtab *insert_column_names = check.names;
  ast_node *name_list_tail = check.name_list_tail;

  // Ensure that the number of values matches the number of columns.
  if (valid && check.count != cols) {
    report_error(ast, "CQL0166: count of columns differs from count of values", NULL);
    valid = 0;
  }

  if (valid) {
    // Ensure that all the necessary columns are present in some order.

    sem_struct *sptr = cursor->sem->sptr;
    for (int32_t icol = 0; icol < sptr->count; icol++) {
      sem_t sem_type_col = sptr->semtypes[icol];
      CSTR name = sptr->names[icol];

      if (symtab_find(insert_column_names, name)) {
        continue;
      }

      bool_t will_use_null = is_nullable(sem_type_col) && !(dummy_flags & INSERT_DUMMY_NULLABLES);

      // Note if the column is nullable we will automatically insert a null even if no dummy data.
      // Previously we would generate an error if any columns were missing with no dummy data but
      // all null, or mostly null rows are super useful.

      if (!will_use_null && !insert_dummy_spec) {
        report_error(ast, "CQL0167: required column missing in FETCH statement", sptr->names[icol]);
        valid = 0;
        break;
      }

      // if we get this far then we're going to re-write the AST for the missing columns

      // insert the dummy value into the two lists, there's a lot of state here.

      dummy_info info;
      info.name = name;
      info.sem_type_col = sem_type_col;
      info.jptr = cursor->sem->jptr;
      info.name_list_tail = name_list_tail;
      info.name_list_head = column_spec->left;
      info.insert_list_tail = insert_list_tail;
      info.insert_list_head = columns_values->right;
      info.use_null = will_use_null;

      if (is_blob(sem_type_col) && !info.use_null) {
        report_error(ast, "CQL0168: CQL has no good way to generate dummy blobs; not supported for now", NULL);
        valid = 0;
        break;
      }

      AST_REWRITE_INFO_SET(columns_values->lineno, columns_values->filename);

      sem_synthesize_dummy_value(&info);

      AST_REWRITE_INFO_RESET();

      name_list = info.name_list_head;
      name_list_tail = info.name_list_tail;
      insert_list = info.insert_list_head;
      insert_list_tail = info.insert_list_tail;

      ast_set_right(columns_values, insert_list);
      ast_set_left(column_spec, name_list);
    }
  }

  if (valid) {
    valid = sem_validate_compatible_cols_vals(name_list, insert_list);
  }

  destroy_name_check(&check);

  if (valid) {
    // TODO: It's possible to do better here by un-improving those being set to
    // a value of a nullable type and actually improving those being set to a
    // notnull type. For now though, we just unimprove all of them.
    sem_struct *sptr = cursor->sem->sptr;
    for (uint32_t i = 0; i < sptr->count; i++) {
      sem_unset_notnull_improved(sptr->names[i], cursor->sem->name);
    }

    // This type of fetch cannot fail so no fetch check should be required.
    sem_set_has_row_improved(cursor->sem->name);

    record_ok(ast);
  }
  else {
    record_error(ast);
  }
}

// Here we just make sure that we can look up every name in this name list
// in the indicated joinscope.  This is helpful if you want to ensure that
// there are names present in a certain level of the tree.
// This helper is intended to (e.g.) look up column names in the context
// of the table they belong to.  Or look up column names in both sides
// of an FK relationship.  This is only used to evaluate at the top level
// if it's happening in the context of a join that's wrong.
static bool_t sem_name_check(name_check *check) {
  bool_t valid = true;
  PUSH_JOIN_BLOCK()
  PUSH_JOIN(name_check, check->jptr);

  // Check for invalid column names and duplicate column names.

  check->name_list_tail = NULL;

  for (ast_node *item = check->name_list; item; item = item->right) {
    Contract(is_ast_name_list(item) || is_ast_indexed_columns(item));
    check->name_list_tail = item;

    CSTR item_name = NULL;
    ast_node *target = NULL;

    if (is_ast_name_list(item)) {
      ast_node *name_ast = target = item->left;

      // Resolve name with no qualifier in the current scope.
      EXTRACT_STRING(name, name_ast);

      if (!sem_find_column_for_name(name_ast, name) || is_error(name_ast)) {
        report_error(name_ast, "CQL0171: name not found", name);
        record_error(name_ast);
        record_error(check->name_list);
        valid = false;
        break;
      }

      item_name = name_ast->sem->name;
    }
    else {
      EXTRACT_NOTNULL(indexed_column, item->left);
      EXTRACT_ANY_NOTNULL(expr, indexed_column->left);

      sem_validate_index_expr_for_jptr(check->jptr, expr);
      if (is_error(expr)) {
        record_error(check->name_list);
        valid = false;
        break;
      }

      item_name = expr_as_text(expr);

      target = expr;
    }

    Invariant(item_name);

    if (!symtab_add(check->names, item_name, NULL)) {
      report_error(target, "CQL0172: name list has duplicate name", item_name);
      record_error(target);
      record_error(check->name_list);
      valid = 0;
      break;
    }

    check->count++;
  }

  POP_JOIN();
  POP_JOIN();
  return valid;
}

// This helper produces a "printf" call suitable for constructing the text "col_%d"
// which is the format of string and blob columns given a dummy seed value for the %d.
// The local variable "seed" is used for the column.  These AST nodes will be used
// in the rewrite of the dummy columns for string and blobs.
static ast_node *printf_col_for_dummy(CSTR col, CSTR seed_name) {
  ast_node *ast_printf = new_ast_str("printf");
  CSTR fmt = dup_printf("'%s_%%d'", col);  // this turns into 'col_%d'
  ast_node *ast_string = new_ast_str(fmt);
  ast_node *ast_seed = new_ast_str(seed_name);

  ast_node *args = new_ast_arg_list(ast_string, new_ast_arg_list(ast_seed, NULL));
  ast_node *call_filter_clause = new_ast_call_filter_clause(NULL, NULL);
  ast_node *call_arg_list = new_ast_call_arg_list(call_filter_clause, args);
  return new_ast_call(ast_printf, call_arg_list);
}

// If we're doing either a FETCH from values or an INSERT from values
// we might need a dummy value.  To accomplish this we add the missing
// value to the column list and to the values list, changing the AST.
// The later code will then process it as though it was present in the first place.
// The code gen for dummy values will use the hidden variable _seed_ which
// is initialized from the dummy value expression.  That will be part of
// codegen later.  For now we only need insert the columns.  They will be
// validated later.  This validation can't actually fail.
static void sem_synthesize_dummy_value(dummy_info *info) {
  CSTR seed_name = "_seed_";
  symtab *scope = locals ? locals : globals;
  Invariant(scope);

  // implictly declare _seed_ as a local/global variable if needed
  if (!symtab_find(scope, seed_name)) {
    ast_node *asts = new_ast_str(seed_name);
    asts->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL | SEM_TYPE_VARIABLE);
    asts->sem->name = seed_name;
    symtab_add(scope, seed_name, asts);
  }

  // Look up the name in the current scope, and only that scope.  No locals
  // No nothing.  Just the columns in the indicated type.
  ast_node *ast_col = new_ast_str(info->name);
  PUSH_JOIN_BLOCK()
  PUSH_JOIN(info_scope, info->jptr);
  bool_t found = sem_find_column_for_name(ast_col, info->name);
  Invariant(found);
  Invariant(!is_error(ast_col));  // name is known to be good!
  POP_JOIN();
  POP_JOIN();

  // add name to the name list
  ast_node *new_tail = new_ast_name_list(ast_col, NULL);
  if (info->name_list_tail) {
    ast_set_right(info->name_list_tail, new_tail);
  }
  else {
    info->name_list_head = new_tail;
  }
  info->name_list_tail = new_tail;

  ast_node *expr = NULL;

  // make a dummy insert node for the column based on its type
  if (info->use_null) {
    Contract(is_nullable(info->sem_type_col));
    expr = new_ast_null();
  }
  else if (is_numeric(info->sem_type_col)) {
    // numbers dummy value is seed
    expr = new_ast_str(seed_name);
  }
  else if (is_blob(info->sem_type_col)) {
    ast_node *inner = printf_col_for_dummy(info->name, seed_name);
    expr = new_ast_cast_expr(inner, new_ast_type_blob(NULL));
  }
  else {
    // strings are column_name_{seed} -- using printf
    expr = printf_col_for_dummy(info->name, seed_name);
  }

  new_tail = new_ast_insert_list(expr, NULL);
  if (info->insert_list_tail) {
    ast_set_right(info->insert_list_tail, new_tail);
  }
  else {
    info->insert_list_head = new_tail;

  }
  info->insert_list_tail = new_tail;
}

// Ensure that the values are valid and the types of the values are compatible
// with the types of the columns.
static bool_t sem_validate_compatible_table_cols_vals(ast_node *table_ast, ast_node *name_list, ast_node *insert_list) {
  Contract(is_ast_create_table_stmt(table_ast));

  ast_node *value = insert_list;
  sem_struct *sptr = table_ast->sem->sptr;

  for (ast_node *item = name_list ; item; item = item->right, value = value->right) {
    EXTRACT_ANY_NOTNULL(expr, value->left);
    EXTRACT_ANY_NOTNULL(col, item->left);
    sem_root_expr(expr, SEM_EXPR_CONTEXT_WHERE); // non-aggregate context
    if (is_error(expr)) {
      return false;
    }

    // we have to find the specific column now so that we can look at table flags
    int32_t icol = 0;
    while (strcmp(sptr->names[icol], col->sem->name)) icol++;

    // autoinc column may be specified as null even though it's not-nullable
    if (is_ast_null(expr) && (sptr->semtypes[icol] & SEM_TYPE_AUTOINCREMENT)) {
      continue;
    }

    // if the column and expression kinds are not compatible we have to bail (e.g. <dollars> used where <euros> expected)
    sem_combine_kinds(expr, col->sem->kind);
    if (is_error(expr)) {
      return false;
    }

    // otherwise the columns have to be assignment compatible
    if (!sem_verify_assignment(expr, col->sem->sem_type, expr->sem->sem_type, col->sem->name)) {
      return false;
    }
  }

  return true;
}

// Ensure that the columns of the select are compatible with the columns of the table in the order specified
static bool_t sem_validate_compatible_table_cols_select(ast_node *table_ast, ast_node *name_list, ast_node *select_stmt) {
  Contract(is_ast_create_table_stmt(table_ast));

  sem_struct *sptr_select = select_stmt->sem->sptr;
  sem_struct *sptr = table_ast->sem->sptr;

  if (is_ast_with_select_stmt(select_stmt)) {
    select_stmt = select_stmt->right;
  }

  // no additional checks are possible if it isn't a standard select statement presumably containing values
  // we just move on and report the best error we can (we still need the types to match)
  if (is_ast_select_stmt(select_stmt)) {
    // The select statement might contain a VALUES clause.
    // We need to walk through all the select_core nodes and do type checking of
    // expressions in VALUES clauses against the name_list. In case of errors found,
    // we tag the error into the expression node in the values clause instead of
    // the name_list. This provides a better error location for the user.
    // e.g: insert into foo select 1 union all values ('x') union all values (3) ...
    // The 'x' is incorrect and the error should refer to that rather than some
    // generic error about the select statement being badly formed.
    EXTRACT_NOTNULL(select_core_list, select_stmt->left);
    for (ast_node *item = select_core_list; item; item = item->right) {
      Contract(is_ast_select_core_list(item));
      EXTRACT_NOTNULL(select_core, item->left);
      EXTRACT_ANY_NOTNULL(select_core_right, select_core->right);

      if (is_ast_values(select_core_right)) {
        EXTRACT_NOTNULL(values, select_core_right);

        for (ast_node *node = values; node; node = node->right) {
          EXTRACT_NOTNULL(insert_list, node->left);

          ast_node *value = insert_list;
          for (ast_node *list = name_list ; list && value; list = list->right, value = value->right) {
            EXTRACT_ANY_NOTNULL(expr, value->left);
            EXTRACT_ANY_NOTNULL(col, list->left);

            // we have to find the specific column now so that we can look at table flags
            int32_t icol = 0;
            while (strcmp(sptr->names[icol], col->sem->name)) icol++;

            // autoinc column may be specified as null even though it's not-nullable
            if (is_null_type(expr->sem->sem_type) && (sptr->semtypes[icol] & SEM_TYPE_AUTOINCREMENT)) {
              continue;
            }

            // NOTE: kind mismatch can be an issue here but only if the values clause has some expressions in it,
            // which is atypical but it can happen.  The normal thing is that values are all constants.
            sem_combine_kinds(expr, col->sem->kind);
            if (is_error(expr)) {
              return false;
            }

            // in case of semantic error the expr is tagged to the expr node in values clause.
            if (!sem_verify_assignment(expr, col->sem->sem_type, expr->sem->sem_type, col->sem->name)) {
              return false;
            }
          }
        }
      }

      // We need to make item->right is always a select_core_list ast.
      if (item->right) {
        Contract(is_ast_select_core_compound(item->right));
        Contract(is_ast_select_core_list(item->right->right));
        item = item->right;
      }
    }
  }

  // Here we just validate that the column types in struct type of the select
  // statement are compatible with the column names types that receive the values
  // in the insert statement.
  int32_t icol_select = 0;
  for (ast_node *item = name_list ; item; item = item->right, icol_select++) {
    Invariant(icol_select < sptr_select->count);

    EXTRACT_ANY_NOTNULL(col, item->left);

    // we have to find the specific column now so that we can look at table flags
    int32_t icol = 0;
    while (strcmp(sptr->names[icol], col->sem->name)) icol++;

    // autoinc column may be specified as null even though it's not-nullable
    if (is_null_type(sptr_select->semtypes[icol_select]) && (sptr->semtypes[icol] & SEM_TYPE_AUTOINCREMENT)) {
      continue;
    }

    sem_combine_kinds(col, sptr_select->kinds[icol_select]);
    if (is_error(col)) {
      return false;
    }

    // otherwise the columns have to be assignment compatible
    if (!sem_verify_assignment(col, col->sem->sem_type, sptr_select->semtypes[icol_select], col->sem->name)) {
      return false;
    }
  }

  return true;
}

// Check that the indicated columns are compatible with the corresponding expressions
// Note the count has already been verified.
static bool_t sem_validate_compatible_cols_vals(ast_node *name_list, ast_node *values) {
  ast_node *value = values;

  for (ast_node *item = name_list ; item; item = item->right, value = value->right) {
    EXTRACT_ANY_NOTNULL(expr, value->left);
    EXTRACT_ANY_NOTNULL(col, item->left);
    sem_expr(expr);
    if (is_error(expr)) {
      return false;
    }

    sem_combine_kinds(col, expr->sem->kind);
    if (is_error(col)) {
      return false;
    }

    if (!sem_verify_assignment(expr, col->sem->sem_type, expr->sem->sem_type, col->sem->name)) {
      return false;
    }
  }

  // count matches
  Invariant(!value);

  return true;
}

// The set statement is for local assignment.  We just validate
// that the target exists and is compatible with the source.
// There are special cases for cursor variables, which cannot be set.
static void sem_assign(ast_node *ast) {
  Contract(is_ast_assign(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  ast_node *variable = find_local_or_global_variable(name);

  if (!variable) {
    report_error(ast, "CQL0173: variable not found", name);
    record_error(ast);
    return;
  }

  ast->sem = name_ast->sem = variable->sem;

  sem_t sem_type_var = variable->sem->sem_type;
  Invariant(is_variable(sem_type_var));

  if (is_struct(sem_type_var)) {
    report_error(ast, "CQL0174: cannot set a cursor", name);
    record_error(ast);
    return;
  }

  Invariant(!current_joinscope);
  sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);

  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  if (!sem_verify_assignment(expr, sem_type_var, expr->sem->sem_type, name)) {
    record_error(name_ast);
    record_error(ast);
  }

  sem_combine_kinds(expr, variable->sem->kind);
  if (is_error(expr)) {
    record_error(ast);
  }

  // We can infer that the left side of `:=` is not null if the right side is
  // not null. Otherwise, we remove any existing improvement as it is no longer
  // valid.
  if (is_not_nullable(expr->sem->sem_type)) {
    sem_set_notnull_improved(name, NULL);
  } else {
    sem_unset_notnull_improved(name, NULL);
  }

  variable->sem->sem_type |= SEM_TYPE_WAS_SET;

  sem_set_initialization_improved(name, NULL);
}

static void sem_let_stmt(ast_node *ast) {
  Contract(is_ast_let_stmt(ast));

  EXTRACT_ANY_NOTNULL(variable, ast->left)
  EXTRACT_STRING(name, variable);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  // LET [name] := [expr]
  if (!sem_verify_legal_variable_name(variable, name)) {
    record_error(ast);
    return;
  }

  Invariant(!current_joinscope);
  sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);

  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  if (is_null_type(expr->sem->sem_type)) {
    report_error(ast, "CQL0056: NULL expression has no type to imply the declaration of variable", name);
    record_error(ast);
    return;
  }

  // the variable is now the exact type of the expression

  sem_t sem_type_var = expr->sem->sem_type;
  sem_type_var &= (SEM_TYPE_NOTNULL | SEM_TYPE_SENSITIVE | SEM_TYPE_CORE);
  sem_type_var |= SEM_TYPE_VARIABLE;

  variable->sem = ast->sem = new_sem(sem_type_var);
  variable->sem->name = name;
  variable->sem->kind = expr->sem->kind;
  add_variable(name, variable);
}

// In/out processing for a procedure just decodes the AST into the sem_type
// it returns the bits that should be set.
static sem_t sem_opt_inout(ast_node *ast) {
  if (is_ast_in(ast)) {
    // IN
    return SEM_TYPE_IN_PARAMETER;
  }
  else if (is_ast_out(ast)) {
    // OUT
    return SEM_TYPE_OUT_PARAMETER;
  }
  else if (is_ast_inout(ast)) {
    // INOUT
    return SEM_TYPE_IN_PARAMETER | SEM_TYPE_OUT_PARAMETER;
  }
  else {
    Contract(!ast);
    return SEM_TYPE_IN_PARAMETER;
  }
}

// Returns true if the `sem_t` of a variable indicates that initialization
// should be required before the value is used (except when passed as an OUT
// argument), else false.
static bool_t variable_should_require_initialization(sem_t sem_type) {
  Contract(is_variable(sem_type));

  // For now, we only require initialization in the case of nonnull reference
  // types as they presently have no sensible default and will be NULL absent
  // initialization.
  return is_not_nullable(sem_type) && is_ref_type(sem_type);
}

// Returns true if the given `sem_t` of a parameter indicates that
// initialization should be required before the procedure returns, else false.
static bool_t param_should_require_initialization(sem_t sem_type) {
  if (is_in_parameter(sem_type)) {
    // Arguments passed for IN and INOUT parameters must have already been
    // initialized by the caller.
    return false;
  }

  return variable_should_require_initialization(sem_type);
}

// A single a proc parameter, it gets its semantic type by the helper
// for the type of a variable.  The main thing that needs to be done here
// is to ensure the name doesn't conflict, and record it as a new local.
static void sem_param(ast_node *ast) {
  Contract(is_ast_param(ast));
  EXTRACT_ANY(opt_inout, ast->left);
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, param_detail->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(data_type, param_detail->right);

  if (symtab_find(locals, name)) {
    report_error(name_ast, "CQL0175: duplicate parameter name", name);
    record_error(ast);
    return;
  }

  sem_data_type_var(data_type);
  if (is_error(data_type)) {
    record_error(ast);
    return;
  }

  sem_t sem_type = data_type->sem->sem_type;
  sem_type |= SEM_TYPE_VARIABLE;
  sem_type |= sem_opt_inout(opt_inout);

  // We set this even if `current_proc` is NULL (i.e., we set it for the DECLARE
  // PROC and DECLARE FUNCTION forms, not just CREATE PROC). The reason for
  // doing this is so that any later parameter declarations that refer to the
  // current procedure using LIKE...ARGUMENTS will have this flag set
  // appropriately.
  if (param_should_require_initialization(sem_type)) {
    sem_type |= SEM_TYPE_INIT_REQUIRED;
  }

  ast->sem = param_detail->sem = name_ast->sem = new_sem(sem_type);

  // [name]
  ast->sem->name = name;
  ast->sem->kind = data_type->sem->kind;

  Invariant(current_variables == locals);
  add_variable(name, ast);
}

// This handles the case where you are using the LIKE proc ARGUMENTS form
// There are quite a few rules here that need to be enforced:
//   * the proc must exist
//   * it must have some args
//   * none of the args may be of type 'object'
//     * because declaring cursors of this form is not yet supported (easy)
//     * because then you have to deal with object<T> in struct_type (not that easy)
//     * because then you have to disallow OUT [cursor] on such cursors (easy)
//     * for now punt on that, as the non-object cases are very valuable
//  With all that done we just make a fake ast node that has the type we need in it
//  and return that.  The type is the usual struct_type
static ast_node *sem_find_likeable_proc_args(ast_node *like_ast, int32_t likeable_for) {
  Contract(is_ast_like(like_ast));

  EXTRACT_ANY_NOTNULL(name_ast, like_ast->left);
  EXTRACT_STRING(like_name, name_ast);

  ast_node *proc= find_proc(like_name);
  if (!proc) {
    report_error(like_ast, "CQL0069: name not found", like_name);
    goto error;
  }

  if (is_error(proc)) {
    report_error(like_ast, "CQL0069: name not found (proc had errors, cannot be used)", like_name);
    goto error;
  }

  EXTRACT_STRING(proc_name, get_proc_name(proc));

  ast_node *result;

  // we're goign to make a synthetic type node for the procedures arguments
  if (yylineno == -1) {
    // set up a rewrite context if one is needed
    // if yylineno is not -1 then something else is already rewriting
    AST_REWRITE_INFO_SET(like_ast->lineno, like_ast->filename);
    result = new_ast_str(like_name);
    AST_REWRITE_INFO_RESET();
  }
  else {
    result = new_ast_str(like_name);
  }

  uint32_t count =0 ;

  ast_node *params = get_proc_params(proc);

  if (!params) {
    report_error(like_ast, "CQL0262: LIKE ... ARGUMENTS used on a procedure with no arguments", like_name);
    goto error;
  }

  for (; params; params = params->right, count++) ;

  CSTR shape_name = dup_printf("%s[arguments]", proc_name);

  sem_struct *sptr = new_sem_struct(shape_name, count);

  params = get_proc_params(proc);

  uint32_t i = 0;
  for (; params; params = params->right, i++) {
    EXTRACT_NOTNULL(param, params->left);

    Invariant(param->sem);
    sem_t sem_type_param = param->sem->sem_type;

    if (likeable_for == LIKEABLE_FOR_VALUES) {
      // strip VARIABLE and OUT, add IN
      // the cursor field will not be the out arg pointer version but the data version
      // and it's no longer a standalone variable
      sem_type_param &= sem_not(SEM_TYPE_OUT_PARAMETER | SEM_TYPE_WAS_SET);
      sem_type_param |= SEM_TYPE_IN_PARAMETER;
    }

    sem_type_param &= sem_not(SEM_TYPE_VARIABLE);

    sptr->semtypes[i] = sem_type_param;
    sptr->names[i] = param->sem->name;
    sptr->kinds[i] = param->sem->kind;
  }

  result->sem = new_sem(SEM_TYPE_STRUCT);
  result->sem->sptr = sptr;
  return result;

error:
    record_error(like_ast);
    record_error(name_ast);
    return NULL;
}

// This is the general helper for handling the "LIKE [name]" form Basically we
// are going to replace the LIKE sequence with a list of names.  We just need to
// find an named object that has a structure type, then return a fresh copy of
// it with a fresh sptr (to prevent aliasing).
// It can be:
//   * a cursor
//   * a proc that returns a result set (or any proc if using ARGUMENTS form)
//   * an interface
//   * a table
//   * a view
//   * an arg bundle (even "ARGUMENTS")
// The source doesn't matter, we just need its shape.  In most cases
// we only need the names, not even the types.  But we might need either.
// (e.g. declare cursor X like Y needs the type info)
//
cql_noexport ast_node *sem_find_shape_def_base(ast_node *like_ast, int32_t likeable_for) {
  Contract(is_ast_like(like_ast));

  if (like_ast->right) {
    // from arguments form, only proc names allowed
    // `sem_find_likeable_proc_args` gives us a fresh ast with a fresh sem node
    // and sptr, so we can just return it here.
    return sem_find_likeable_proc_args(like_ast, likeable_for);
  }

  EXTRACT_ANY_NOTNULL(name_ast, like_ast->left);
  EXTRACT_STRING(like_name, name_ast);

  ast_node *found_shape = find_local_or_global_variable(like_name);
  if (found_shape) {
    if (!is_cursor(found_shape->sem->sem_type)) {
      report_error(like_ast, "CQL0200: variable is not a cursor", like_name);
      goto error;
    }
  }

  if (!found_shape) {
    // look for an arg bundle match
    found_shape = find_arg_bundle(like_name);
  }

  if (!found_shape) {
    // note: it's ok to use the LIKE construct on deleted tables too, hence even_deleted
    found_shape = find_table_or_view_even_deleted(like_name);
  }

  if (!found_shape) {
    found_shape = find_proc(like_name);
    if (found_shape) {
      if (!found_shape->sem->sptr) {
        report_error(like_ast, "CQL0178: proc has no result", like_name);
        goto error;
      }
    }
  }

  if (!found_shape) {
    found_shape = find_interface_type(like_name);
  }

  if (!found_shape || is_error(found_shape)) {
    report_error(like_ast, "CQL0202: must be a cursor, proc, table, or view", like_name);
    goto error;
  }

  record_ok(like_ast);

  // To prevent aliasing of the sptr (which would make it impossible to use
  // functions like `find_mutable_type` safely), we need to shallow copy the ast
  // and its sem node before cloning the sptr in the next step.
  ast_node *new_shape = _ast_pool_new(ast_node);
  *new_shape = *found_shape;
  new_shape->sem = _ast_pool_new(sem_node);
  *new_shape->sem = *found_shape->sem;

  // We never want `SEM_TYPE_INFERRED_NOTNULL` to propagate via LIKE as it would
  // falsely imply a NOT NULL status, e.g., in `declare C0 cursor like C1`.
  new_shape->sem->sptr = sem_clone_struct_strip_flags(new_shape->sem->sptr, SEM_TYPE_INFERRED_NOTNULL);

  return new_shape;

error:
  record_error(like_ast);
  record_error(name_ast);
  return NULL;
}

cql_noexport ast_node *sem_find_shape_def(ast_node *shape_def, int32_t likeable_for) {
  Contract(is_ast_shape_def(shape_def));
  EXTRACT_NOTNULL(like, shape_def->left);
  EXTRACT(name_list, shape_def->right);

  ast_node *base_shape = sem_find_shape_def_base(like, likeable_for);
  if (!base_shape) {
    goto error;
  }

  if (name_list) {
    if (!sem_verify_no_duplicate_names(name_list)) {
      goto error;
    }

    sem_struct *sptr_old = base_shape->sem->sptr;
    Invariant(sptr_old);


    ast_node *iter = name_list;
    uint32_t count = 0;

    while (iter) {
      count++;
      iter = iter->right;
    }

    Invariant(count >= 1);
    sem_struct *sptr_new = new_sem_struct("select", count);

    int32_t inew = 0;
    iter = name_list;

    while (iter) {
      EXTRACT_STRING(name, iter->left);

      int32_t iold = find_col_in_sptr(sptr_old, name);
      if (iold < 0) {
        report_error(iter->left, "CQL0069: name not found", name);
        goto error;
      }

      sptr_new->names[inew] = sptr_old->names[iold];
      sptr_new->semtypes[inew] = sptr_old->semtypes[iold];
      sptr_new->kinds[inew] = sptr_old->kinds[iold];
      iter = iter->right;
      inew++;
    }

    AST_REWRITE_INFO_SET(base_shape->lineno, base_shape->filename);
    ast_node *result = new_ast_shape_def(NULL, NULL);
    result->sem = new_sem(SEM_TYPE_STRUCT);
    result->sem->sptr = sptr_new;
    AST_REWRITE_INFO_RESET();
    return result;
  }

  return base_shape;

error:
  record_error(shape_def);
  return NULL;
}

// All we have to do here is walk the parameter list and use the helper above
// for each parameter.
static void sem_params(ast_node *head, bytebuf *args_info) {
  Contract(is_ast_params(head));

  rewrite_params(head, args_info);
  if (is_error(head)) {
    return;
  }

  // we're only going to record the proc argument shape for
  // create proc statements, we need this stuff for the JSON
  // output so we can emit where the arguments came from.
  // Since we have to do this anyway we're also going to make
  // a fake arg bundle for all the arguments.  By doing this
  // the "from arguments" forms all look exactly the same as
  // any other "from shape" kind of thing so we don't need
  // special code to walk the arguments.  It just looks like a shape.

  CSTR *arg_names = NULL;
  sem_struct *sptr = NULL;

  if (args_info) {
    uint32_t count = args_info->used / sizeof(CSTR) / 3;
    if (count) {
      AST_REWRITE_INFO_SET(head->lineno, head->filename);
      CSTR args = "ARGUMENTS";

      ast_node *ast_args = new_ast_str(args);
      ast_args->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_HAS_SHAPE_STORAGE);
      ast_args->sem->name = args;
      sptr = new_sem_struct(args, count);
      ast_args->sem->sptr = sptr;
      add_arg_bundle(ast_args, args);
      arg_names = (CSTR *)args_info->ptr;

      AST_REWRITE_INFO_RESET();
    }
  }

  uint32_t i = 0;
  for (ast_node *ast = head; ast; ast = ast->right, i++) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    sem_param(param);
    if (is_error(param)) {
      record_error(head);
      return;
    }

    if (sptr) {
      Invariant(i < sptr->count);
      sptr->names[i] = arg_names[i*3];
      sptr->semtypes[i] = param->sem->sem_type;
      sptr->kinds[i] = param->sem->kind;
    }
  }

  record_ok(head);
}

// If we need to implicitly generate locals or a return struct for a select statement
// the all of the columns have to have unique names.  This really can only happen if
// you do something like select * from foo T1 join foo T2 on T1.id = T2.parent_id
// There will be two "id" columns in that result.  Basically you can't use select *
// then because of the code-gen would give errors in naming those columns (and it's
// confusing as hell).
static void sem_validate_unique_names_struct_type(ast_node *ast) {
  Contract(ast);
  Contract(ast->sem);
  Contract(is_struct(ast->sem->sem_type));
  Contract(!is_error(ast));  // already "ok" at least

  sem_struct *sptr = ast->sem->sptr;

  symtab *fields = symtab_new();

  for (int32_t i = 0; i < sptr->count; i++) {
    if (!symtab_add(fields, sptr->names[i], NULL)) {
      report_error(ast, "CQL0180: duplicate column name in result not allowed", sptr->names[i]);
      record_error(ast);
      break;
    }
  }

  symtab_delete(fields);
  return;
}

// Find the column type of a column in a table. Return 0 if not found
sem_t find_column_type(CSTR table_name, CSTR column_name) {
  ast_node *table_ast = find_table_or_view_even_deleted(table_name);
  if (table_ast) {
    for (int32_t i = 0; i < table_ast->sem->sptr->count; i++) {
      if (!Strcasecmp(column_name, table_ast->sem->sptr->names[i])) {
        return table_ast->sem->sptr->semtypes[i];
      }
    }
  }
  return false;
}

static void report_dummy_test_error(ast_node *target, CSTR message, CSTR subject, int32_t *error) {
  report_error(target, message, subject);
  record_error(target);
  *error = true;
}

// semantic analysis of dummy_test info. Return true if the node is processed otherwise false
static bool_t sem_autotest_dummy_test(
  ast_node *misc_attr_value_list,
  void *context)
{
  Contract(is_ast_misc_attr_value_list(misc_attr_value_list));

  if (!is_ast_str(misc_attr_value_list->left)) {
    return false;
  }

  EXTRACT_STRING(autotest_attr_name, misc_attr_value_list->left);

  if (!is_autotest_dummy_test(autotest_attr_name)) {
    return false;
  }

  bytebuf column_types = {NULL, 0, 0};
  bytebuf column_names = {NULL, 0, 0};

  int32_t *error = (int32_t *)context;

  // walkthrough dummy_test tree and retreive the table name then the column name
  // of the table name and then the column values of the column names. We repeat
  // it for the next table info.
  for (ast_node *dummy_test_list = misc_attr_value_list->right; dummy_test_list; dummy_test_list = dummy_test_list->right) {
    bytebuf_open(&column_types);
    bytebuf_open(&column_names);

    int32_t column_count = 0;

    // find table name
    if (!is_ast_misc_attr_value_list(dummy_test_list->left) || !is_ast_str(dummy_test_list->left->left)) {
      report_dummy_test_error(
      dummy_test_list->left,
        "CQL0273: autotest attribute has incorrect format (table name should be nested) in",
        "dummy_test",
        error);
      goto cleanup;
    }

    ast_node *table_list = dummy_test_list->left;
    EXTRACT_STRING(table_name, table_list->left);
    ast_node *table = find_table_or_view_even_deleted(table_name);
    if (!table) {
      report_dummy_test_error(
        table_list->left,
        "CQL0274: autotest attribute 'dummy_test' has non existent table",
        table_name,
        error);
      goto cleanup;
    }

    record_ok(table_list->left);

    // find column names
    ast_node *column_name_list = table_list->right;
    if (!is_ast_misc_attr_value_list(column_name_list->left)) {
      report_dummy_test_error(
        table_list->left,
        "CQL0273: autotest attribute has incorrect format (column name should be nested) in",
        "dummy_test",
        error);
      goto cleanup;
    }

    for (ast_node *list = column_name_list->left; list; list = list->right) {
      if (!is_ast_str(list->left)) {
        report_dummy_test_error(
          table_list->left,
          "CQL0273: autotest attribute has incorrect format (column name should be nested) in",
          "dummy_test",
          error);
        goto cleanup;
      }
      ast_node *misc_attr_value = list->left;
      EXTRACT_STRING(column_name, misc_attr_value);
      sem_t col_type = find_column_type(table_name, column_name);
      if (!col_type) {
        report_dummy_test_error(
          misc_attr_value,
          "CQL0275: autotest attribute 'dummy_test' has non existent column",
          column_name,
          error);
        goto cleanup;
      }
      record_ok(misc_attr_value);
      sem_t *col_type_ptr = bytebuf_alloc(&column_types, sizeof(sem_t));
      *col_type_ptr = col_type;
      CSTR *colum_name_ptr = bytebuf_alloc(&column_names, sizeof(CSTR));
      *colum_name_ptr = column_name;
      column_count++;
    }

    // find column values
    if (!is_ast_misc_attr_value_list(column_name_list->right)) {
      report_dummy_test_error(
        table_list->left,
        "CQL0273: autotest attribute has incorrect format (column value should be nested) in",
        "dummy_test",
        error);
      goto cleanup;
    }

    for (ast_node *column_values_list = column_name_list->right; column_values_list; column_values_list = column_values_list->right) {
      if (!is_ast_misc_attr_value_list(column_values_list->left)) {
        report_dummy_test_error(
          table_list->left,
          "CQL0273: autotest attribute has incorrect format (column value should be nested) in",
          "dummy_test",
          error);
        goto cleanup;
      }

      int32_t column_value_count = 0;
      for (ast_node *list = column_values_list->left; list; list = list->right) {

        if (column_value_count >= column_count) {
          report_dummy_test_error(
            table_list->left,
            "CQL0273: autotest attribute has incorrect format (too many column values) in",
            "dummy_test",
            error);
          goto cleanup;
        }

        ast_node *misc_attr_value = list->left;
        sem_t col_type = ((sem_t *)column_types.ptr)[column_value_count];
        sem_t core_type = core_type_of(col_type);

        if (is_ast_uminus(misc_attr_value)) {
          Contract(is_ast_num(misc_attr_value->left));
          misc_attr_value = misc_attr_value->left;
        }

        bool_t ok = false;

        if (is_ast_num(misc_attr_value)) {
           // an integer literal is good for any numeric type
           EXTRACT_NUM_TYPE(num_type, misc_attr_value);

           if (num_type == NUM_INT || num_type == NUM_BOOL) {
             // an integer or bool literal is good for any numeric type
             ok = is_numeric(core_type);
           }
           else if (num_type == NUM_LONG) {
             // NUM_LONG might not fit in REAL, compatible only with itself
             ok = core_type == SEM_TYPE_LONG_INTEGER;
           }
           else {
             Contract(num_type == NUM_REAL);
             // a real literal is only good for a real column
             ok = core_type == SEM_TYPE_REAL;
           }
        }
        else if (is_strlit(misc_attr_value)) {
           // a string literal is ok for any text column
           ok = core_type == SEM_TYPE_TEXT;
        }
        else if (is_ast_null(misc_attr_value)) {
           // the null token is ok for any nullable column
           ok = is_nullable(col_type);
        }
        else if (is_ast_blob(misc_attr_value)) {
           // a blob literal is ok for blob column
           ok = core_type == SEM_TYPE_BLOB;
        }

        if (!ok) {
          report_dummy_test_error(
            misc_attr_value,
            "CQL0276: autotest attribute 'dummy_test' has invalid value type in",
            ((CSTR *) column_names.ptr)[column_value_count],
            error);
          goto cleanup;
        }
        record_ok(misc_attr_value);
        column_value_count++;
      }

      if (column_count != column_value_count) {
        report_dummy_test_error(
          table_list->left,
          "CQL0273: autotest attribute has incorrect format (mismatch number of column and values) in",
          "dummy_test",
          error);
        goto cleanup;
      }
    }

    bytebuf_close(&column_types);
    bytebuf_close(&column_names);
  }

  record_ok(misc_attr_value_list->left);

cleanup:
  if (column_types.ptr) {
     bytebuf_close(&column_types);
  }

  if (column_names.ptr) {
     bytebuf_close(&column_names);
  }
  return true;
}

// Searching for valid cql:autotest=(x,y,z...) attributes
// If we find an invalid form for the attribute or we find a listed item that
// is unknown then we use the context to flag and error (which we report).
// All of these will do complex code gen if an autotest codegen pass is selected
// but for here we just verify that the attribute is of the correct form and hence
// could be used.
static void sem_find_ast_misc_attr_callback(
  CSTR misc_attr_prefix,
  CSTR misc_attr_name,
  ast_node *ast_misc_attr_value_list,
  void *context)
{
  if (misc_attr_prefix &&
      misc_attr_name &&
      !(Strcasecmp(misc_attr_prefix, "cql")) &&
      !(Strcasecmp(misc_attr_name, "autotest"))) {
    int32_t *error = (int32_t *)context;

    if (!is_ast_misc_attr_value_list(ast_misc_attr_value_list)) {
      report_dummy_test_error(
        ast_misc_attr_value_list,
        "CQL0277: autotest has incorrect format",
        "no test types specified",
        error);
      return;
    }

    for (ast_node *list = ast_misc_attr_value_list; list; list = list->right) {
      ast_node *misc_attr_value = list->left;
      // We found a nested dummy_test with info
      // @attribute(cql:autotest=(..., (dummy_test, ...))))
      if (is_ast_misc_attr_value_list(misc_attr_value)) {
        if (sem_autotest_dummy_test(misc_attr_value, context)) {
          if (*error) {
            record_error(misc_attr_value->left);
          }
        } else {
          report_dummy_test_error(
            misc_attr_value->left,
            "CQL0277: autotest has incorrect format",
            "found nested attributes that don't start with dummy_test",
            error);
        }
      }
      else { // autotest attribute
        EXTRACT_STRING(autotest_attr_name, misc_attr_value);
        if (!is_autotest_dummy_table(autotest_attr_name) &&
            !is_autotest_dummy_insert(autotest_attr_name) &&
            !is_autotest_dummy_select(autotest_attr_name) &&
            !is_autotest_dummy_result_set(autotest_attr_name) &&
            !is_autotest_dummy_test(autotest_attr_name)) {
          report_dummy_test_error(
            misc_attr_value,
            "CQL0278: autotest attribute name is not valid",
            autotest_attr_name,
            error);
        } else {
          record_ok(misc_attr_value);
        }
      }
    }
  }
}

// semantic analysis of autotest attributes. The autotest attribution should
// look like this:
// @attribute(cql:autotest=(<name>,
//                          <name>,
//                          ...
//                         )
//           )
// <name> can be "dummy_test" or "dummy_table" or "dummy_insert" or "dummy_select" or
// "dummy_result_set"
//
// If informations are added to "dummy_test" attribute then format should look like this:
// @attribute(cql:autotest=(...,
//                          (dummy_test,
//                                    (<table1>,
//                                              (<column_name1>, ...),
//                                              (<column_value1>, ...),
//                                              (<column_value1>, ...)
//                                    ),
//                                    (<table2>,
//                                              ...
//                                    )
//                          ),
//                          ...
//                         )
//           )
//  "..." can be any autotest attribution name except "dummy_test"
static void sem_autotests(ast_node *misc_attrs) {
  Contract(is_ast_misc_attrs(misc_attrs));
  int32_t error = false;

  find_misc_attrs(misc_attrs, sem_find_ast_misc_attr_callback, &error);
  if (error) {
    record_error(misc_attrs);
    return;
  }
  record_ok(misc_attrs);
}

// Check wheter or not the values of the attribution are valid names of columns in the current proc's result_set
static uint32_t sem_column_name_annotation(ast_node *misc_attrs, find_annotation_values find, CSTR target) {
  Contract(is_ast_misc_attrs(misc_attrs));
  Contract(annotation_target == NULL);
  record_ok(misc_attrs);

  annotation_target = target;
  uint32_t result = find(misc_attrs, sem_column_name_exist_in_result_set, misc_attrs);
  annotation_target = NULL;
  return result;
}

// Check the autodrop to make sure it is conformant, it has to be a valid temp table.
static void sem_one_autodrop(CSTR name, ast_node *misc_attr_value, void *context) {
  EXTRACT_NOTNULL(misc_attrs, (ast_node *)context);

  // temp tables are never @deleted, look only for not_deleted tables
  ast_node *temp_table = find_usable_and_not_deleted_table_or_view(
    name,
    misc_attr_value->parent,
    "CQL0181: autodrop temp table does not exist");
  if (!temp_table) {
    record_error(misc_attrs);
    return;
  }

  if (!is_ast_create_table_stmt(temp_table)) {
    report_error(misc_attr_value->parent, "CQL0182: autodrop target is not a table", name);
    record_error(misc_attrs);
    return;
  }

  EXTRACT_NOTNULL(create_table_name_flags, temp_table->left);
  EXTRACT_NOTNULL(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);

  int32_t temp = flags & TABLE_IS_TEMP;

  if (!temp) {
    report_error(misc_attr_value->parent, "CQL0183: autodrop target must be a temporary table", name);
    record_error(misc_attrs);
    return;
  }

  record_ok(misc_attr_value);
}

// If a stored proc is marked with the autodrop annotation then we automatically drop the indicated
// tables when the proc is finished running.  The attributes should look like this:
// @attribute(cql:autodrop=(table1, table2, ,...))
static uint32_t sem_autodrops(ast_node *misc_attrs) {
  Contract(is_ast_misc_attrs(misc_attrs));
  record_ok(misc_attrs);
  return find_autodrops(misc_attrs, sem_one_autodrop, misc_attrs);
}

// In a query fragment, the first CTE name has to match the core fragment name.
// This helper can be used to validate any CTE name.
static bool_t sem_fragment_CTE_name_check(
  ast_node *cte_decl,
  CSTR name,
  CSTR error_msg)
{
  EXTRACT_ANY_NOTNULL(name_cte_decl, cte_decl->left);
  EXTRACT_STRING(with_cte_name, name_cte_decl);
  if (strcmp(with_cte_name, name)) {
    report_error(name_cte_decl, error_msg, name);
    record_error(name_cte_decl);
    return false;
  }
  return true;
}

// Both the base and the extension fragments have to end with "select * from [their_cte]"
// Rather than try to check for that by walking the tree and getting all the ast junk right
// it's WAY easier to just generate the text from the tree (which will be trivial in normal cases)
// and then Strcmp it.  This code used to be full of all manner of special cases for which we
// still even have tests but all that goes away with a simple string check.
static void sem_fragment_select_everything_check(ast_node* select_stmt, CSTR name)
{
  Contract(select_stmt);
  Contract(name);
  CHARBUF_OPEN(reqd_sql);
  CHARBUF_OPEN(cur_sql);

  // the expected multi-line formatted version of the required select
  bprintf(&reqd_sql, "SELECT *\n  FROM %s", name);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_echo; // we want all the text, unexpanded, so NOT for sqlite output (this is raw echo)
  gen_set_output_buffer(&cur_sql);
  gen_with_callbacks(select_stmt, gen_one_stmt, &callbacks);

  if (Strcasecmp(cur_sql.ptr, reqd_sql.ptr)) {
    CSTR msg = dup_printf("CQL0251: fragment must end with exactly 'SELECT * FROM %s'", name);
    report_error(select_stmt, msg, NULL);
    record_error(select_stmt);
  }

  CHARBUF_CLOSE(cur_sql);
  CHARBUF_CLOSE(reqd_sql);
}

// Extension fragments that add rows (this form)
// @attribute(cql:extension_fragment=b)
// create proc ext1()
// begin
//   with b(id) as (select * from foo),
//     ext1(*) as (
//       select * from b
//       union all
//       select 7 id
//   )
//   select * from ext1;
// end;
//
// the fragment must first select everything from the base query "select * from b" above
// so that no rows are lost
//
// (This code is nearly identical to the above, it's duplicated for clarity and so that the error
// message can be different without adding lots of conditionals everywhere. If it was any bigger
// it could be refactored but it has hardly any body to it)
static void sem_fragment_union_shape(ast_node *select_core, CSTR name)
{
  Contract(name);
  Contract(is_ast_select_core(select_core));
  CHARBUF_OPEN(reqd_sql);
  CHARBUF_OPEN(cur_sql);

  // the expected multi-line formatted version of the required select
  bprintf(&reqd_sql, "SELECT *\n  FROM %s", name);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_echo; // we want all the text, unexpanded, so NOT for sqlite output (this is raw echo)
  gen_set_output_buffer(&cur_sql);
  gen_with_callbacks(select_core, gen_select_core, &callbacks);

  if (Strcasecmp(cur_sql.ptr, reqd_sql.ptr)) {
    CSTR msg = dup_printf("CQL0330: fragment must start with exactly 'SELECT * FROM %s'", name);
    report_error(select_core, msg, NULL);
    record_error(select_core);
  }

  CHARBUF_CLOSE(cur_sql);
  CHARBUF_CLOSE(reqd_sql);
}

// The query fragment must use the WITH for to define a chain of CTE's
static void sem_fragment_has_with_select_stmt(ast_node *stmt_list) {
  Contract(is_ast_stmt_list(stmt_list));

  if (stmt_list->right) {
    goto error;
  }

  EXTRACT_ANY(with_select_stmt, stmt_list->left);

  if (!is_ast_with_select_stmt(with_select_stmt)) {
    goto error;
  }

 return;

error:
  report_error(stmt_list,
    "CQL0290: fragments can only have one statement in the statement list and it must be a WITH...SELECT", NULL);
  record_error(stmt_list);
}

// Check the number of params under two name lists are the same.
static bool_t fragment_base_cte_name_list_check(ast_node *base_name_list, ast_node *name_list) {
  if (!base_name_list && !name_list) {
    return true;
  } else
  if (!base_name_list || !name_list) {
    return false;
  }
  return fragment_base_cte_name_list_check(base_name_list->right, name_list->right);
}

// Check for consistent sem types and nullability for base CTE in current fragment and its base fragment
static bool_t fragment_base_cte_sem_types_check(ast_node *base_select_expr_list, ast_node *select_expr_list) {
  Contract(is_struct(base_select_expr_list->sem->sem_type));
  sem_struct *base_sptr = base_select_expr_list->sem->sptr;
  Contract(is_struct(select_expr_list->sem->sem_type));
  sem_struct *sptr = select_expr_list->sem->sptr;
  Invariant(base_sptr && sptr);
  Invariant(base_sptr->count == sptr->count);

  for (int32_t i = 0; i < base_sptr->count; i++) {
    sem_t base_sem_type = base_sptr->semtypes[i];
    sem_t sem_type = sptr->semtypes[i];
    const char *col_name = base_sptr->names[i];

    if (core_type_of(base_sem_type) != core_type_of(sem_type)) {
      CSTR error_message =
        "CQL0287: extension/assembly fragment must add stub "
        "for base CTE with same types from base fragment";
      report_sem_type_mismatch(base_sem_type, sem_type, base_select_expr_list, error_message, col_name);
      return false;
    }

    if (is_nullable(base_sem_type) != is_nullable(sem_type)) {
      CSTR error_message =
        "CQL0288: extension/assembly fragment stub for base CTE column must be "
        "exact type match (including nullability)";
      report_sem_type_mismatch(base_sem_type, sem_type, base_select_expr_list, error_message, col_name);
      return false;
    }
  }
  return true;
}

typedef struct attr_value_record {
  CSTR value;
  ast_node *ast;
} attr_value_record;

static void reset_attr_value_record(attr_value_record *data) {
  data->value = NULL;
  data->ast = NULL;
}

// records the at most one instance of found attribute
static void record_attr_value(CSTR value, ast_node *misc_attr_value, void *context) {
  Contract(context);
  attr_value_record *record = (attr_value_record *)context;

  // this callback is always used after the target is known to have exactly one matching attribute
  Invariant(!record->value);
  Invariant(!record->ast);
  record->value = value;
  record->ast = misc_attr_value;
}

// helper to find the at most one instance of the named misc attribute
static void find_named_cql_attribute(ast_node *misc_attr_list, CSTR attr_name, attr_value_record *data) {
  reset_attr_value_record(data);
  uint32_t count = find_attribute_str(misc_attr_list, record_attr_value, data, attr_name);

  // this search is always used after the target is known to have exactly one matching attribute
  Invariant(count == 1);
  Invariant(data->ast);
}

// when we discover a table parameter we'll see if we can find it
// in the table of names we've seen before.  If we find it, the new
// parameter must have the exact same type as what we already have.
typedef struct bind_equivalence_info {
  symtab *names;
  ast_node *bind_mismatch_error;
} bind_equivalence_info;

// Here we must verify that if we found two table parameters of the same name that they are of the
// same exact type.  Since they have the same name there will be one table binding for the both
// of them and so if their type is not identical then no one binding could satisfy both
static void verify_identical_table_params_callback(void *context, CSTR name, ast_node *cte_decl) {
  bind_equivalence_info *info = (bind_equivalence_info *)context;

  symtab_entry *entry = symtab_find(info->names, name);

  if (!entry) {
    // new name, nothing to check
    symtab_add(info->names, name, cte_decl);
    return;
  }

  // existing name must be identical
  sem_verify_identical_columns((ast_node*)entry->val, cte_decl, name);
  if (is_error(cte_decl)) {
    info->bind_mismatch_error = cte_decl;
  }
}

// If a stored proc is marked with the shared_fragment attribute, we check for the simple
// shared form of one select statement, with no OUT or IN/OUT args
// The attribute should look like this:
// @attribute(cql:shared_fragment)
static void sem_shared_fragment(ast_node *misc_attrs, ast_node *create_proc_stmt) {
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  Contract(is_ast_misc_attrs(misc_attrs));

  Contract(!current_joinscope);  // I don't belong inside a select(!)
  EXTRACT_NOTNULL(proc_params_stmts, create_proc_stmt->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_STRING(proc_name, current_proc->left);

  if (stmt_list->right) {
    report_error(stmt_list, "CQL0179: shared fragments must consist of exactly one top level statement", proc_name);
    record_error(misc_attrs);
    record_error(stmt_list);
    record_error(create_proc_stmt);
    return;
  }

  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  if (!is_select_stmt(stmt) && !is_ast_if_stmt(stmt)) {
    report_error(stmt, "CQL0441: shared fragments may only have IF, SELECT, or WITH...SELECT at the top level", proc_name);
    record_error(misc_attrs);
    record_error(stmt_list);
    record_error(create_proc_stmt);
    return;
  }

  if (is_ast_if_stmt(stmt)) {
    shared_cte_info info;
    bind_equivalence_info bind_info;
    memset(&info, 0, sizeof(info));
    memset(&bind_info, 0, sizeof(bind_info));

    info.context = &bind_info;
    info.callback = verify_identical_table_params_callback;
    bind_info.names = symtab_new();

    sem_accumulate_proc_cte_info(create_proc_stmt, &info);

    symtab_delete(bind_info.names);

    if (info.missing_else) {
      report_error(info.missing_else, "CQL0442: shared fragments with conditionals must include an else clause", proc_name);
      record_error(misc_attrs);
      record_error(stmt_list);
      record_error(create_proc_stmt);
      return;
    }

    if (info.bad_statement_form) {
      report_error(info.bad_statement_form, "CQL0443: shared fragments with conditionals must have exactly one SELECT, or WITH...SELECT in each statement list", proc_name);
      record_error(misc_attrs);
      record_error(stmt_list);
      record_error(create_proc_stmt);
      return;
    }

    if (info.non_select_stmt) {
      report_error(info.non_select_stmt, "CQL0443: shared fragments with conditionals must have exactly SELECT, or WITH...SELECT in each statement list", proc_name);
      record_error(misc_attrs);
      record_error(stmt_list);
      record_error(create_proc_stmt);
      return;
    }

    // This means specifically that we found a case where two table parameters were specified in the
    // fragment that have the same name but are of different types
    // error already reported in this case, we just record the failure and move on
    if (bind_info.bind_mismatch_error) {
      record_error(misc_attrs);
      record_error(stmt_list);
      record_error(create_proc_stmt);
      return;
    }
  }

  for (ast_node *ast = params; ast; ast = ast->right) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    if (is_out_parameter(param->sem->sem_type)) {
      report_error(stmt_list,  "CQL0208: shared fragments cannot have any out or in/out parameters", param->sem->name);
      record_error(misc_attrs);
      record_error(stmt_list);
      record_error(create_proc_stmt);
      return;
    }
  }
}

// If a stored proc is marked with the base_fragment attribute, we syntax check that it specifies base fragment table inside
// The attributes should look like this:
// @attribute(cql:base_fragment=core_table)
static void sem_base_fragment(ast_node *misc_attrs, ast_node *stmt_list, ast_node *create_proc_stmt) {
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  Contract(is_ast_misc_attrs(misc_attrs));

  attr_value_record data;
  find_named_cql_attribute(misc_attrs, "base_fragment", &data);
  CSTR assembly_frag_name = data.value;

  sem_fragment_has_with_select_stmt(stmt_list);
  if (is_error(stmt_list)) {
    goto error;
  }

  // safe to do this now that we have done the verification above
  EXTRACT_NOTNULL(with_select_stmt, stmt_list->left);

  // check for the single named CTE
  EXTRACT_NOTNULL(cte_tables, with_select_stmt->left->left);
  if (cte_tables->right) {
    report_error(cte_tables, "CQL0253: base fragment must have only a single CTE named the same as the fragment", assembly_frag_name);
    goto error;
  }

  EXTRACT_NOTNULL(cte_decl, cte_tables->left->left);
  if (!sem_fragment_CTE_name_check(cte_decl, assembly_frag_name,
    "CQL0253: base fragment must have only a single CTE named the same as the fragment")) {
    goto error;
  }

  // check for select everything from the named CTE
  EXTRACT_NOTNULL(select_stmt, with_select_stmt->right);

  sem_fragment_select_everything_check(select_stmt, assembly_frag_name);
  if (is_error(select_stmt)) {
    goto error;
  }

  if (find_base_fragment(assembly_frag_name)) {
    report_error(misc_attrs, "CQL0256: fragment name conflicts with existing base fragment", assembly_frag_name);
    goto error;
  }
  else {
    add_base_fragment(create_proc_stmt, assembly_frag_name);
  }

  record_ok(misc_attrs);
  return;

error:
  record_error(misc_attrs);
  record_error(stmt_list);
  record_error(create_proc_stmt);
}

// Make sure extension fragment select all the columns of its base CTE
// Looking for base_fragment.* here
// Example:
// some_extension_cte(x,y,z,foo) as (
// select base.* ..... from .....)
// In this example, base.* is what we want
static bool_t extension_fragment_select_check(ast_node *select_expr_list_con,
  ast_node *my_cte_tables, CSTR name) {
  EXTRACT_NOTNULL(select_expr_list, select_expr_list_con->left);

  // the first entry has to be table.*
  EXTRACT_ANY_NOTNULL(table_star, select_expr_list->left);
  if (!is_ast_table_star(table_star)) {
    goto error;
  }

  // it has to be the correct table
  EXTRACT_STRING(table_star_name, table_star->left);
  if (Strcasecmp(table_star_name, name)) {
    goto error;
  }
  return true;

error:
  report_error(my_cte_tables, "CQL0259: extension fragment CTE must select T.* from base CTE", name);
  record_error(my_cte_tables);
  record_error(select_expr_list_con);
  return false;
}

// Only work for fragments! Pass in a stmt_list ast node (must be the one under proc_params_stmts)
// Return the nearest cte_tables ast node from all its children.
static ast_node *get_cte_tables_by_stmt_list(ast_node *stmt_list) {
  Contract(is_ast_stmt_list(stmt_list));
  EXTRACT_NOTNULL(with_select_stmt, stmt_list->left);
  EXTRACT_NOTNULL(cte_tables, with_select_stmt->left->left);
  return cte_tables;
}

// Only work for fragment. Pass in a create_proc_stmt of the fragment
// Return the nearest cte_tables ast node from all its children.
static ast_node *get_cte_tables_by_create_proc_stmt(ast_node *create_proc_stmt) {
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  EXTRACT_NOTNULL(proc_params_stmts, create_proc_stmt->right);
  return get_cte_tables_by_stmt_list(proc_params_stmts->right);
}

// When processing fragments all the fragments must have the same signature as the base
// fragment. If this were not the case then you might miss errors due to type mismatches
// in the final assembly.  This way you can't add args that won't be present in the end
// nor can any of the types of those args be wrong in any of the semantic checks along the way.
// Additionally, if any of the arguments appear in the selected result (they can) then
// the types of those is going to match in all the getters.
// We do this check by converting the params to the canoncial string representation and
// complaining if there is not exact (case insensitive) match.
static bool_t fragment_params_check(CSTR assembly_frag_name, ast_node *create_proc_stmt) {
  // the fragment name has already been checked for existence using fragment_base_columns_check
  ast_node *base_fragment = find_base_fragment(assembly_frag_name);
  Invariant(base_fragment);

  ast_node *paramsReqd = get_proc_params(base_fragment);
  ast_node *paramsActual = get_proc_params(create_proc_stmt);
  ast_node *name_ast = get_proc_name(create_proc_stmt);
  EXTRACT_STRING(name, name_ast);

  bool_t result = true;

  CHARBUF_OPEN(reqd);
  CHARBUF_OPEN(actual);

  if (paramsReqd) {
    gen_set_output_buffer(&reqd);
    gen_with_callbacks(paramsReqd, gen_params, NULL);
  }

  if (paramsActual) {
    gen_set_output_buffer(&actual);
    gen_with_callbacks(paramsActual, gen_params, NULL);
  }

  if (Strcasecmp(actual.ptr, reqd.ptr)) {
    const char *msg = dup_printf( "CQL0322: fragment parameters must be exactly '(%s)'", reqd.ptr, name);
    report_error(name_ast, msg, NULL);
    record_error(name_ast);
    record_error(create_proc_stmt);
    result = false;
  }

  CHARBUF_CLOSE(actual);
  CHARBUF_CLOSE(reqd);
  return result;
}

// Check for fragment name in extension/assembly fragment from existing base fragment
// Also check for column consistency (name list & column sem types) in fragment base CTEs.
static bool_t fragment_base_columns_check(
  CSTR name,
  ast_node * str_ast,
  ast_node * stmt_list)
{
  ast_node *base_fragment = find_base_fragment(name);
  if (!base_fragment) {
    EXTRACT_NOTNULL(misc_attr, str_ast->parent);
    EXTRACT_NOTNULL(misc_attrs, misc_attr->parent);
    report_error(str_ast, "CQL0255: fragment name is not a previously declared base fragment", name);
    record_error(str_ast);
    return false;
  }

  sem_fragment_has_with_select_stmt(stmt_list);
  if (is_error(stmt_list)) {
    return false;
  }

  EXTRACT_NOTNULL(with_select_stmt, stmt_list->left);

  // Check base CTE name should be consistent with the one declared in base fragment
  EXTRACT_NOTNULL(cte_tables, with_select_stmt->left->left);
  EXTRACT_NOTNULL(cte_decl, cte_tables->left->left);
  if (!sem_fragment_CTE_name_check(cte_decl, name,
    "CQL0268: extension/assembly fragment must have the CTE named same as the base fragment")) {
    return false;
  }

  // Check the base CTE in extension/assembly fragment and the one declared in base fragment
  // are consistent:
  // 1) base CTE column name list (e.g. WITH core(x,y,z) ...) should be the same
  EXTRACT_NAMED_NOTNULL(base_cte_tables, cte_tables, get_cte_tables_by_create_proc_stmt(base_fragment));
  EXTRACT_NAMED_NOTNULL(base_cte_table, cte_table, base_cte_tables->left);
  EXTRACT_NAMED_NOTNULL(base_cte_decl, cte_decl, base_cte_table->left);
  if (!(fragment_base_cte_name_list_check(base_cte_decl->right, cte_decl->right))) {
      report_error(cte_decl,
        "CQL0268: extension/assembly fragment must use base CTE column list same as from the base fragment",
        name);
    record_error(cte_decl);
    return false;
  }

  // 2) sem types for base CTE stub in extension/assembly fragment
  // (e.g. the stub for core(x,y,z): as (select 1,"a",nullable(3L)) ...)
  // should be consistent with sem types for columns xyz
  EXTRACT_NAMED_NOTNULL(base_select_stmt, select_stmt, base_cte_table->right);
  EXTRACT_NAMED_NOTNULL(base_select_core_list, select_core_list, base_select_stmt->left);
  EXTRACT_NAMED_NOTNULL(base_select_core, select_core, base_select_core_list->left);
  EXTRACT_NAMED_NOTNULL(base_select_expr_list_con, select_expr_list_con, base_select_core->right);

  EXTRACT_NAMED_NOTNULL(cte_table, cte_table, cte_tables->left);
  EXTRACT_NOTNULL(select_stmt, cte_table->right);
  EXTRACT_NOTNULL(select_core_list, select_stmt->left);
  EXTRACT_NOTNULL(select_core, select_core_list->left);
  EXTRACT_NAMED_NOTNULL(select_expr_list_con, select_expr_list_con, select_core->right);

  if (!(fragment_base_cte_sem_types_check(base_select_expr_list_con->left, select_expr_list_con->left))) {
    record_error(select_expr_list_con);
    return false;
  }
  return true;
}

static bool_t sem_extension_left_outer_join(CSTR assembly_frag_name, ast_node *my_select_expr_list_con, ast_node *my_cte_tables)
{
  EXTRACT_NAMED_NOTNULL(my_select_expr_list, select_expr_list, my_select_expr_list_con->left);

  // check for left outer join
  EXTRACT_NOTNULL(select_from_etc, my_select_expr_list_con->right);
  EXTRACT_ANY_NOTNULL(join_clause, select_from_etc->left);

  // it has to start with a simple join
  if (!is_ast_join_clause(join_clause)) {
    goto error;
  }

  EXTRACT_NOTNULL(table_or_subquery, join_clause->left);

  // we have to be joining from a table, not anything more complex
  EXTRACT_ANY_NOTNULL(table_name_ast, table_or_subquery->left);
  if (!is_ast_str(table_name_ast)) {
    goto error;
  }

  // it has to be the correct table
  EXTRACT_STRING(table_name, table_name_ast);
  if (Strcasecmp(assembly_frag_name, table_name)) {
    goto error;
  }

  EXTRACT_NOTNULL(join_target_list, join_clause->right);
  EXTRACT_NOTNULL(join_target, join_target_list->left);
  EXTRACT_OPTION(join_type, join_target->left);

  // it has to be a left join
  if (join_type != JOIN_LEFT_OUTER) {
    goto error;
  }

  return true;

error:
  report_error(my_cte_tables, "CQL0260: extension fragment CTE must be a simple left outer join from",
    assembly_frag_name);

  return false;
}

// Returns true if any of the "etc" parts of from_etc are present.
// This let's us easily check for the presence of any extra clauses that are
// not allowed in an extension fragment.
static bool_t sem_has_extra_clauses(ast_node *select_from_etc, ast_node *select_orderby) {
  Contract(is_ast_select_orderby(select_orderby));

  bool_t has_extras = false;

  if (select_from_etc) {
    Contract(is_ast_select_from_etc(select_from_etc));

    EXTRACT_NOTNULL(select_where, select_from_etc->right);
    EXTRACT(opt_where, select_where->left);
    EXTRACT_NOTNULL(select_groupby, select_where->right);
    EXTRACT(opt_groupby, select_groupby->left);
    EXTRACT_NOTNULL(select_having, select_groupby->right);
    EXTRACT(opt_having, select_having->left);
    EXTRACT(opt_select_window, select_having->right);

    has_extras |= !!opt_where;
    has_extras |= !!opt_groupby;
    has_extras |= !!opt_having;
    has_extras |= !!opt_select_window;
  }

  EXTRACT(opt_orderby, select_orderby->left);
  EXTRACT_NOTNULL(select_limit, select_orderby->right);
  EXTRACT(opt_limit, select_limit->left);
  EXTRACT_NOTNULL(select_offset, select_limit->right);
  EXTRACT(opt_offset, select_offset->left);

  has_extras |= !!opt_orderby;
  has_extras |= !!opt_limit;
  has_extras |= !!opt_offset;

  return has_extras;
}

// If a stored proc is marked with extension_fragment attribute, we syntax check that it extends the base fragment
// specified previously and follows consistency rules
// The attributes should look like this:
// @attribute(cql:extension_fragment=core)
static void sem_extension_fragment(ast_node *misc_attrs, ast_node *stmt_list, ast_node *create_proc_stmt) {
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  Contract(is_ast_misc_attrs(misc_attrs));

  attr_value_record data;
  find_named_cql_attribute(misc_attrs, "extension_fragment", &data);
  CSTR assembly_frag_name = data.value;

  // Check the extension fragment adding column(s) for basic syntax requirements and correct reference to base fragment
  // * attribute name match existing base fragment name
  // * refer to CTE stub set up in base fragment, with same name, columns and stub select values
  // * followed by a single extension CTE with columns including all base fragment columns (select T.* from base CTE)
  // * left outer join with base CTE so it cannot remove from base
  // * the final select must be select * from extension CTE
  if (!fragment_base_columns_check(assembly_frag_name, data.ast, stmt_list)) {
    goto error;
  }

  // ensure that the fragment parameters exactly match the base fragment
  if (!fragment_params_check(assembly_frag_name, create_proc_stmt)) {
    goto error;
  }

  // The above has already checked for these conditions -- one statement and it's a with..select
  Invariant(!stmt_list->right);
  EXTRACT_NOTNULL(with_select_stmt, stmt_list->left);

  ast_node *cte_tables = get_cte_tables_by_stmt_list(stmt_list);
  EXTRACT_NAMED_NOTNULL(my_cte_tables, cte_tables, cte_tables->right);

  // get extension CTE name
  EXTRACT_NAMED_NOTNULL(my_cte_table, cte_table, my_cte_tables->left);
  EXTRACT_ANY_NOTNULL(name_cte_decl, my_cte_table->left->left);
  EXTRACT_STRING(my_cte_name, name_cte_decl);

  // check for the single extension CTE
  if (my_cte_tables->right != NULL) {
    EXTRACT_NAMED_NOTNULL(extra_cte_table, cte_table, my_cte_tables->right->left);
    EXTRACT_ANY_NOTNULL(extra_name_cte_decl, extra_cte_table->left->left);
    EXTRACT_STRING(extra_name, extra_name_cte_decl);

    report_error(my_cte_tables, "CQL0258: extension fragment must add exactly one CTE; found extra named", extra_name);
    record_error(cte_tables);
    record_error(with_select_stmt);
    goto error;
  }

  // Here we check for one of the two extension forms:
  // 1) select ... from base_fragment left outer join ...
  // 2) select ... from base_fragment union all select ... from ...
  // For the first form, we require it to use left outer join only.
  // For the second form, we require two select to have the same columns. (Checked in other places with error CQL0057)
  // For both forms, we require them to contain all columns from their base fragments in select expression.
  // In this if expression we would check the first form and we would check the second form in else expression.

  EXTRACT_ANY_NOTNULL(my_select_stmt, my_cte_table->right);
  EXTRACT_NAMED_NOTNULL(my_any_select_core_list, select_core_list, my_select_stmt->left);
  EXTRACT_NAMED_NOTNULL(my_select_core, select_core, my_any_select_core_list->left);

  if (my_any_select_core_list->right == NULL) {
    // select statement without compounded select_core nodes
    // this is the case where we are adding columns.
    // The prescribed shape is very strictly determined it has to be

    // @attribute(cql:extension_fragment=core)
    // create proc good_pattern(...args...)
    // begin
    // with
    //  core(x,y,z,a) as (select 1, nullable("a"), nullable(3L), 4),
    //  ext(x,y,z,a,flag) as (select core.*, [other columns] FROM core left outer join [any join target])
    //  select * from ext
    // end;
    //
    // Here we're checking this part:
    //  (select core.*, [other columns] FROM core left outer join [any join target])
    //
    // * it has to be a LEFT OUTER JOIN from the base fragment table "core" in the example above
    // * you have to include core.* as the first thing in your select list
    // * you can't have any other clauses like WHERE, HAVING, LIMIT, because that would potentially
    //   remove or re-order rows
    EXTRACT_NAMED_NOTNULL(my_select_expr_list_con, select_expr_list_con, my_select_core->right);
    if (!extension_fragment_select_check(my_select_expr_list_con, my_cte_tables, assembly_frag_name)) {
      goto error;
    }

    if (!sem_extension_left_outer_join(assembly_frag_name, my_select_expr_list_con, my_cte_tables)) {
      goto error;
    }

    EXTRACT_NOTNULL(select_from_etc, my_select_expr_list_con->right);
    EXTRACT_NOTNULL(select_orderby, my_select_stmt->right);

    if (sem_has_extra_clauses(select_from_etc, select_orderby)) {
      report_error(select_from_etc,
         "CQL0320: extension fragment CTE must have a FROM clause and no other top level clauses", my_cte_name);
      goto error;
    }
  }
  else {
    // In the case where we're using the form at adds rows (that's a fragment with union all)
    // (see sem_fragment_union_shape for more details) we have to make sure we preserve all
    // of the base rows and only add new rows.  The first part is to make sure the top half
    // of the compound query is "select * from base"

    EXTRACT(select_core, my_any_select_core_list->left);
    sem_fragment_union_shape(select_core, assembly_frag_name);
    if (is_error(select_core)) {
      record_error(with_select_stmt);
      goto error;
    }

    // We still have to make sure this clause does not remove or reorder any rows
    // so the overall compound query cannot have any ORDER-BY LIMIT or OFFSET
    // it has to be a plain from clause with nothing that can affect the top part
    // which we just confirmed to be "select * from base"

    EXTRACT_NOTNULL(select_orderby, my_select_stmt->right);

    if (sem_has_extra_clauses(NULL, select_orderby)) {
      report_error(select_core,
         "CQL0331: extension fragment CTE must have not have ORDER BY or LIMIT clauses", my_cte_name);
      record_error(with_select_stmt);
      goto error;
    }

    // get all the things associated with this fragment name
    bytebuf *buf = symtab_ensure_bytebuf(extensions_by_basename, assembly_frag_name);
    size_t frag_count = buf->used / sizeof(ast_node *);
    ast_node **frags = (ast_node **)buf->ptr;

    // the previous fragment (if there is one) must be of the UNION ALL flavor
    if (frag_count) {
      ast_node *extension = frags[frag_count-1];
      EXTRACT_NAMED_NOTNULL(extension_cte_tables, cte_tables, get_cte_tables_by_create_proc_stmt(extension));
      EXTRACT_NAMED_NOTNULL(extension_cte_table, cte_table, extension_cte_tables->right->left);

      EXTRACT_ANY_NOTNULL(prev_select_stmt, extension_cte_table->right);
      EXTRACT_NAMED_NOTNULL(prev_any_select_core_list, select_core_list, prev_select_stmt->left);

      if (prev_any_select_core_list->right == NULL) {
        // not a union all CTE -- error
        report_error(select_core,
           "CQL0332: all extension fragments that use UNION ALL must come before those that use LEFT OUTER JOIN", my_cte_name);
        record_error(with_select_stmt);
        goto error;
      }
    }

    // Now we have to make sure it's only UNION ALL for the whole CTE at the top level
    ast_node *select_core_list = my_any_select_core_list;
    while (select_core_list) {

      Contract(is_ast_select_core_list(select_core_list));
      // EXTRACT_NOTNULL(select_core, select_core_list->left);

      EXTRACT(select_core_compound, select_core_list->right);
      if (!select_core_compound) {
        break;
      }
      EXTRACT_OPTION(compound_operator, select_core_compound->left);
      select_core_list = select_core_compound->right;

      if (compound_operator != COMPOUND_OP_UNION_ALL) {
        report_error(select_core_compound,
           "CQL0333: all the compound operators in this CTE must be UNION ALL", my_cte_name);
        record_error(with_select_stmt);
        goto error;
      }
    }

    ast_node *base_proc = find_base_fragment(assembly_frag_name);
    Invariant(base_proc);

    sem_verify_identical_columns(base_proc, create_proc_stmt, "in extension fragment");
    if (is_error(create_proc_stmt)) {
      goto error;
    }
  }

  // check for select * from {extension CTE name}
  EXTRACT_NOTNULL(select_stmt, with_select_stmt->right);
  sem_fragment_select_everything_check(select_stmt, my_cte_name);
  if (is_error(select_stmt)) {
    goto error;
  }

  Invariant(!stmt_list->right);
  Invariant(is_ast_with_select_stmt(stmt_list->left));

  if (find_extension_fragment(my_cte_name)) {
    report_error(my_cte_table, "CQL0266: extension fragment name conflicts with existing fragment", my_cte_name);
    record_error(my_cte_table);
    record_error(cte_tables);
    goto error;
  }

  add_extension_fragment(create_proc_stmt, my_cte_name);
  add_extension_to_base(create_proc_stmt, assembly_frag_name);
  return;

error:
  record_error(misc_attrs);
  record_error(stmt_list);
  record_error(create_proc_stmt);
}

// In assembly_fragment, we clone the ast trees of extension fragments. For the base tables occur in the CTE tables of
// extension fragments, we need to rename their names to the name of previous extenstion table.
// This procedure change their names recursively by passing in the select_compund_stmt ast node, base table name and the
// previous extension table name.
static void replace_fragment_name(ast_node *node, CSTR base_name, CSTR new_name) {
  if (node == NULL) {
    return;
  }
  if (is_ast_str(node)) {
    EXTRACT_STRING(name, node);
    if (!strcmp(name, base_name)) {
      str_ast_node * extension_node = (str_ast_node *)node;
      extension_node->value = new_name;
    }
  }
  if (is_primitive(node)) {
    return;
  }

  if (!is_ast_null(node->left)) {
    replace_fragment_name(node->left, base_name, new_name);
  }
  if (!is_ast_null(node->right)) {
    replace_fragment_name(node->right, base_name, new_name);
  }
}

// Pass in the old_extension_cte_tables which is used to get the base cte.
// the cte_tables, which is the tail cte_tables of the assembly fragment,
// and the newly cloned extension_cte_tables (it is from the right child of the old_extension_cte_tables).
//
// In assembly_fragment, we clone the ast trees of extension fragments. The cte tables of each extension would be
// spliced to the assembly cte_tables at its tail.
// Since we replace the name of base cte table to the previous extension cte table, the declaration of newly spliced
// extension need to be expanded to include all parameters in the declaration of previous extension table.
static bool_t assembly_fragment_expand_cte_tables(
  ast_node *old_extension_cte_tables,
  ast_node *cte_tables,
  ast_node *extension_cte_tables)
{
  Contract(!ast_has_right(extension_cte_tables));
  EXTRACT_NOTNULL(cte_decl, cte_tables->left->left);
  EXTRACT_NAMED_NOTNULL(base_cte_decl, cte_decl, old_extension_cte_tables->left->left);
  EXTRACT_NAMED_NOTNULL(extension_cte_table, cte_table, extension_cte_tables->left);
  EXTRACT_NAMED_NOTNULL(extension_cte_decl, cte_decl, extension_cte_table->left);
  EXTRACT_NOTNULL(name_list, cte_decl->right);
  EXTRACT_NAMED_NOTNULL(base_name_list, name_list, base_cte_decl->right);
  EXTRACT_NAMED_NOTNULL(extension_name_list, name_list, extension_cte_decl->right);
  ast_node *last_extension_name_list = extension_name_list;

  while (base_name_list) {
    base_name_list = base_name_list->right;
    last_extension_name_list = extension_name_list;
    extension_name_list = extension_name_list->right;
    name_list = name_list->right;
  }
  cte_tables->right = extension_cte_tables;
  extension_cte_tables->parent = cte_tables;
  EXTRACT_NAMED_NOTNULL(previous_cte_table, cte_table, cte_tables->left);
  if (!name_list) {
    return true;
  }
  ast_node *assembly_name_list = name_list;
  ast_node *extension_name_list_head = extension_name_list;
  while (extension_name_list) {
    name_list = assembly_name_list;
    while (name_list) {
      EXTRACT_STRING(assembly_parameter, name_list->left);
      EXTRACT_STRING(extension_parameter, extension_name_list->left);
      if (!strcmp(assembly_parameter, extension_parameter)) {
        report_error(extension_name_list,
          "CQL0267: extension fragments of same base fragment share the same cte column", extension_parameter);
        record_error(extension_name_list);
        return false;
      }
      name_list = name_list->right;
    }
    extension_name_list = extension_name_list->right;
  }

  // Copy the name list tree of previous extension fragment and insert it in the current new extension fragment.
  // For example, previous one is (x,y,a) current is (x,y,b), it would be (x,y,a,b) and the struct will be
  //{name_list}
  // {name x}
  // {name_list}
  // | {name y}
  // | {name_list}
  //   | {name a}
  //   | {name_list}
  //     | {name b}
  name_list = assembly_name_list;
  ast_node *cloned_name_list = copy_ast_tree(name_list);
  ast_set_right(last_extension_name_list, cloned_name_list);
  while (ast_has_right(cloned_name_list)) {
    cloned_name_list = cloned_name_list->right;
  }
  ast_set_right(cloned_name_list, extension_name_list_head);

  return true;
}

// Here were going to verify that the procedure in question implements the indicated interface
// in order to do so it has to have the correct columns with compatible types in any order
// which is to say the procedures result type has to be a superset of the interface columns
//   * note there can be more than one interface
//   * we will eventually allow columns that are compatable with trivial conversion (e.g. not null -> nullable)
static void sem_validate_one_interface(CSTR _Nonnull interface_name, ast_node *_Nonnull attr, void *_Nullable context) {
  ast_node *create_proc_stmt = (ast_node *)context;

  Contract(is_ast_create_proc_stmt(create_proc_stmt));

  symtab *names = symtab_new();
  ast_node *name_ast = get_proc_name(create_proc_stmt);
  EXTRACT_STRING(proc_name, name_ast);

  ast_node *interface = find_interface_type(interface_name);
  if (!interface) {
    report_error(attr, "CQL0482: interface not found", interface_name);
    record_error(attr);
    goto error;
  }

  sem_struct *interface_sptr = interface->sem->sptr;
  sem_struct *proc_sptr = create_proc_stmt->sem->sptr;

  // stash the indices of all the column names in the procedure so that we can find them quickly
  for (uint32_t i = 0; i < proc_sptr->count; i++) {
    symtab_add(names, proc_sptr->names[i], (void *)(uint64_t)i);
  }

  for (uint32_t i = 0; i < interface_sptr->count; ++i) {

    CSTR interface_column_name = interface_sptr->names[i];

    symtab_entry *entry = symtab_find(names, interface_sptr->names[i]);
    if (!entry) {
      CSTR msg = dup_printf("CQL0484: procedure '%s' is missing column '%s' of interface '%s'",
        proc_name, interface_column_name, interface_name);
      report_error(create_proc_stmt, msg, NULL);
      goto error;
    }

    // the column index in the procedure result type can be different, we saved it above
    uint32_t j = (uint32_t)(uint64_t)entry->val;

    sem_t actual_type = proc_sptr->semtypes[j];
    sem_t expected_type = interface_sptr->semtypes[i];

    if (
      core_type_of(actual_type) != core_type_of(expected_type) ||
      is_nullable(actual_type) != is_nullable(expected_type) ||
      sensitive_flag(actual_type) != sensitive_flag(expected_type)
    ) {
      CSTR error = "CQL0485: column types returned by proc need to be the same as defined on the interface";
      report_sem_type_mismatch(expected_type, actual_type, create_proc_stmt, error, interface_column_name);
      goto error;
    }
  }

  goto cleanup;

error:
  record_error(create_proc_stmt);

cleanup:
  symtab_delete(names);
}

// The given procedure was marked with @attribute(cql:assembly_fragment=core)
// validate that it is a well-formed query fragment
static void sem_assembly_fragment(ast_node *misc_attrs, ast_node *stmt_list, ast_node *create_proc_stmt) {
  Contract(is_ast_misc_attrs(misc_attrs));
  Contract(is_ast_create_proc_stmt(create_proc_stmt));
  Contract(!stmt_list || is_ast_stmt_list(stmt_list)); // might be null (which will produce an error)

  attr_value_record data;
  find_named_cql_attribute(misc_attrs, "assembly_fragment", &data);

  CSTR assembly_frag_name = data.value;
  ast_node *str_ast = data.ast;

  ast_node *name_ast = get_proc_name(create_proc_stmt);
  EXTRACT_STRING(proc_name, name_ast);

  // The assembly fragment must conform to a strict pattern
  // * it must be the only assembly fragment with this base name
  // * there must be a base fragment of the provided name
  // * this fragment must consist of a single with..select statement
  // * it must have exactly one CTE
  // * the name of that one CTE must be the same as the base fragment name
  // * the CTE column must be exactly the same as in the base fragment (name and type)
  // * the procedure name must be the same as the CTE base fragment name
  //
  // As a result of these rules the assembly fragment will end up looking something like this:
  //
  // @attribute(cql:assembly_fragment=assembly_frag_name)
  // create proc assembly_frag_name(id_ integer not null)
  // begin
  //   with
  //   assembly_frag_name(x,y,z) as (select 1 x, 'b' y, 3 z)
  //   select * from assembly_frag_name;
  // end;
  //
  // Notes:
  //  * the "(select 1 x, 'b' y, 3 z)" can be anything that results in the right type
  //    that part of the CTE will be replaced with the actual contents of the base fragment
  //    so it is just a surrogate for whatever that query is.  This saves you from having
  //    to duplicate the base query all over.
  //
  //  * the form assembly_frag_name(*) as (select ...) is a good option as it saves you
  //    from duplicating the column names and is rewritten to the above
  //
  //  * the "select * from assembly_frag_name" portion can be any query you like that uses
  //    "assembly_frag_name".  All appearances of assembly_frag_name will be replaced with the
  //    CTE for the final assembled query (see below).  So you could select some or all
  //    of the columns in any order.  In practice you really want to include "*" for
  //    assembly_frag_name.* in the select list so that any columns that were added will
  //    appear even though you might not know what they are going to be.  But other
  //    columns/tables can be added and can contribute to say sort order or limit or
  //    anything like that.

  if (find_assembly_fragment(assembly_frag_name)) {
    report_error(str_ast, "CQL0264: duplicate assembly fragments of base fragment", assembly_frag_name);
    goto error;
  }

  if (!fragment_base_columns_check(assembly_frag_name, str_ast, stmt_list)) {
    goto error;
  }

  // ensure that the fragment parameters exactly match the base fragment
  if (!fragment_params_check(assembly_frag_name, create_proc_stmt)) {
    goto error;
  }

  ast_node *cte_tables = get_cte_tables_by_stmt_list(stmt_list);
  if (cte_tables->right != NULL) {
   report_error(cte_tables, "CQL0265: assembly fragment can only have one CTE", assembly_frag_name);
   record_error(cte_tables);
   goto error;
  }

  if (Strcasecmp(assembly_frag_name, proc_name)) {
    report_error(create_proc_stmt,
      "CQL0319: name of the assembly procedure must match the name of the base fragment", proc_name);
    goto error;
  }

  // Now we modify the AST to assemble all the fragments:
  // * Replace the stub core into real core
  // * Assemble each plugins CTE by
  //   * Changing the CTE, referencing from base fragment to previous fragment
  //   * Add the previous fragment's addtional columns to the output
  //
  // * The final select is changed to refer to the overall CTE and the grouping and ordering are preserved
  // It will clone the existing ast tree to build the assembly fragment ast tree by:
  // 1. Replace the base cte stub with the a cloned real one.
  // 2. Clone each extension fragment and attach it to assembly fragment.
  //
  // Note:
  // The sem pointers of all nodes of cloned tree begin as null.  We use a second
  // call to sem_inside_create_proc_stmt to get the true semantic info.
  // Tthis happens after the cloning process is complete.
  //
  // Struct of stmt_list before:
  // {stmt_list}
  // | {with_select_stmt}
  //   | {with}
  //   | | {cte_tables}
  //   | | | {cte_table}  <- stub base cte
  //   .....
  // Struct of stmt_list after:
  // {stmt_list}
  // | {with_select_stmt}
  //   | {with}
  //   | | {cte_tables}
  //   | | | {cte_table} <- cloned real base cte
  //   | | | ...
  //   | | | {cte_tables}
  //   | |   | {cte_table} <- cloned first extension cte
  //   | |   | ...
  //   | |   | {cte_tables}
  //   | |     | {cte_table} <- cloned second extension cte
  //   ... ...

  EXTRACT_NAMED_NOTNULL(base_cte_tables, cte_tables,
    get_cte_tables_by_create_proc_stmt(find_base_fragment(assembly_frag_name)));

  cte_tables->left = base_cte_tables->left;
  base_cte_tables->left->parent = cte_tables->left;

  // get all the things associated with this fragment name
  bytebuf *buf = symtab_ensure_bytebuf(extensions_by_basename, assembly_frag_name);
  size_t frag_count = buf->used / sizeof(ast_node *);
  ast_node **frags = (ast_node **)buf->ptr;

  CSTR previous_cte_name;
  bool_t is_first = true;

  for (uint32_t i = 0; i < frag_count; i++) {
    ast_node *extension = frags[i];
    EXTRACT_NAMED_NOTNULL(extension_cte_tables, cte_tables, get_cte_tables_by_create_proc_stmt(extension));
    EXTRACT_NAMED_NOTNULL(extension_cte_table, cte_table, extension_cte_tables->right->left);

    ast_node *new_extension_cte_tables = copy_ast_tree(extension_cte_tables->right);
    if (!is_first) {
      // If it is not the first extension fragment, we need to change the base cte name to previous extension cte name.
      replace_fragment_name(new_extension_cte_tables->left->right, assembly_frag_name, previous_cte_name);
    }
    is_first = false;
    EXTRACT_STRING(this_cte_name, extension_cte_table->left->left);
    previous_cte_name = this_cte_name;
    if (!assembly_fragment_expand_cte_tables(extension_cte_tables, cte_tables, new_extension_cte_tables)) {
      goto error;
    }
    cte_tables = cte_tables->right;
  }

  if (!is_first) {
    replace_fragment_name(stmt_list->left->right, assembly_frag_name, previous_cte_name);
  }

  add_assembly_fragment(create_proc_stmt, assembly_frag_name);

  // For assembly fragment, we only copied the ast node but didn't update the semantic information.
  // Now walk the procedure again.  Note, this might fail if something goes wrong with the rewrite
  // (which it shouldn't) but if it does it will produce errors in the normal way.
  sem_inside_create_proc_stmt(create_proc_stmt);
  return;

error:
  record_error(misc_attrs);
  record_error(stmt_list);
  record_error(create_proc_stmt);
}

// Returns true if all parameters of the current procedure that require
// initialization have been initialized, else false. Any errors that occur will
// be reported as occurring at the location of `error_ast`.
static bool_t sem_validate_current_proc_params_are_initialized(ast_node *error_ast) {
  Contract(current_proc);
  Contract(error_ast);

  // Check the parameters to ensure all have been initialized.
  ast_node *params = get_proc_params(current_proc);
  for (ast_node *param_item = params; param_item; param_item = param_item->right) {
    EXTRACT_NOTNULL(param, param_item->left);
    sem_t sem_type = param->sem->sem_type;
    if (sem_type & SEM_TYPE_INIT_REQUIRED && !(sem_type & SEM_TYPE_INIT_COMPLETE)) {
      EXTRACT_NOTNULL(param_detail, param->right);
      EXTRACT_STRING(name, param_detail->left);
      report_error(error_ast, "CQL0439: nonnull reference OUT parameter possibly not always initialized", name);
      return false;
    }
  }

  return true;
}

// Used for sem_create_proc_stmt() only.
// Here we run the sem analysis for params and stmt_list of the create_proc_stmt ast node.
// Since assembly fragment processing will change the ast tree struct, this procedure
// will be executed one more time after changing the tree struct.
//
// This returns a bool because it has to handle the null stmt_list case so the usual contract
// of putting the error on the ast doesn't work.  Returns true on error.
static void sem_inside_create_proc_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_STRING(proc_name, current_proc->left);

  if (params) {
    Contract(is_ast_params(params));
  }

  int32_t saved_between_count = between_count;
  has_dml = 0;
  current_variables = locals = symtab_new();
  arg_bundles = symtab_new();
  between_count = 0;
  in_proc_savepoint = false;

  // We push a normal context for all of the statements within the procedure. We
  // do this here rather than simply using `sem_stmt_list` below so that we can
  // verify all of the procedure's parameters have been appropriately
  // initialized before the initialization improvements are unset.
  FLOW_PUSH_CONTEXT_NORMAL();

  // we process the parameter list even if there are no statements
  if (params) {
    bytebuf *args_info = symtab_ensure_bytebuf(proc_arg_info, proc_name);
    sem_params(params, args_info);
    if (is_error(params)) {
      goto error;
    }
  }

  // We have to mark the thing as ok here because it could be called
  // recursively and we want to know if there are any errors so far.
  // the semantic info might be mutated later so don't use the shared ok record
  ast->sem = new_sem(SEM_TYPE_OK);

  if (!stmt_list) {
    if (find_proc_frag_type(ast) != FRAG_TYPE_NONE) {
      report_error(ast, "CQL0440: fragments may not have an empty body", proc_name);
      goto error;
    }
  } else {
    // BEGIN [stmt_list] END
    //
    // We avoid pushing a new context so that any initialization improvements
    // are still in effect when we later validate that all parameters have been
    // appropriately initialized.
    sem_stmt_list_in_current_flow_context(stmt_list);
    if (is_error(stmt_list)) {
      goto error;
    }

    if (ast->sem && ast->sem->sptr) {
      ast->sem->sptr = new_sem_struct_strip_table_flags(ast->sem->sptr);
      ast->sem->sptr->struct_name = proc_name;
    }

    if (has_dml) {
      Invariant(ast->sem);
      ast->sem->sem_type |= SEM_TYPE_DML_PROC;
    }
  }

  if (current_proc_contains_try_is_proc_body) {
    // This procedure contains a TRY block that should be treated as the main
    // body of the procedure. Parameter initialization has already been checked
    // accordingly within `sem_trycatch_stmt`.
    //
    // Checking it again here, which we do not do, could very well fail. In
    // fact, not enforcing initialization here is half of the utility of the
    // "cql:try_is_proc_body" attribute at present. (The other half, of course,
    // is that `sem_trycatch_stmt` *does* enforce it.)
  } else {
    // Verify that all parameters have been appropriately initialized using the
    // end of the procedure for error reporting. This is the common case.
    if (!sem_validate_current_proc_params_are_initialized(ast)) {
      goto error;
    }
  }

cleanup:
  FLOW_POP_CONTEXT_NORMAL();
  symtab_delete(locals);
  symtab_delete(arg_bundles);
  locals = NULL;
  arg_bundles = NULL;
  current_variables = globals;
  between_count = saved_between_count;
  current_proc_contains_try_is_proc_body = false;

  return;

error:
  record_error(ast);
  goto cleanup;
}

// Helper function to validate that ok_table_scan attribution is semantically correctly.
// ok_table_scan value can only be a table name and should be used in a create proc statement
static void sem_validate_ok_table_scan_value(ast_node *misc_attrs, ast_node *ast_misc_attr_value) {
  if (!is_ast_str(ast_misc_attr_value)) {
    report_error(ast_misc_attr_value, "CQL0325: ok_table_scan attribute must be a name", NULL);
    record_error(ast_misc_attr_value);
    return;
  }

  EXTRACT_STRING(table_name, ast_misc_attr_value);
  if (!find_usable_table_or_view_even_deleted(table_name, misc_attrs, "CQL0326: the table name in ok_table_scan does not exist")) {
    record_error(ast_misc_attr_value);
    return;
  }

  record_ok(ast_misc_attr_value);
}

// This function validates the semantics of the ok_table_scan attribute.
// It can only be assigned to a create proc statement and takes table
// names as value.
// It's used by the test helpers runtime to know on which tables it's are ok to
// have allow table scan in a stored proc.
static void sem_misc_attrs_ok_table_scan(
    CSTR misc_attr_prefix,
    CSTR misc_attr_name,
    ast_node *ast_misc_attr_values,
    ast_node *misc_attrs,
    ast_node *any_stmt) {
  Contract(misc_attr_name);
  Contract(any_stmt);
  Contract(misc_attrs);

  if (!Strcasecmp(misc_attr_prefix, "cql") &&
      !Strcasecmp(misc_attr_name, "ok_table_scan")) {
    if (!is_ast_create_proc_stmt(any_stmt)) {
        report_error(misc_attrs, "CQL0329: ok_table_scan attribute can only be used in a create procedure statement", NULL);
        record_error(misc_attrs);
        return;
    } else if (is_ast_misc_attr_value_list(ast_misc_attr_values)) {
      // the value in ok_table_scan attributions is a list of value. we have to go
      // through the list and validate each of them.
      for (ast_node *list = ast_misc_attr_values; list; list = list->right) {
        ast_node *ast_misc_attr_value = list->left;
        sem_validate_ok_table_scan_value(misc_attrs, ast_misc_attr_value);
        if (is_error(ast_misc_attr_value)) {
          record_error(misc_attrs);
          return;
        }
      }
    }
    else {
      // The value in ok_table_scan attributions should be str node otherwise it's an error.
      sem_validate_ok_table_scan_value(misc_attrs, ast_misc_attr_values);
      if (is_error(ast_misc_attr_values)) {
        record_error(misc_attrs);
        return;
      }
    }
  }
}

// This function validates the semantics of the no_table_scan attribute.
// The attribute does not take a value and can only be use on create table
// statement.
// It's used by the test helpers runtime to know on which tables it's forbidden
// to have table scan.
static void sem_misc_attrs_no_table_scan(
    CSTR misc_attr_prefix,
    CSTR misc_attr_name,
    ast_node *ast_misc_attr_values,
    ast_node *misc_attrs,
    ast_node *any_stmt) {
  Contract(misc_attr_name);
  Contract(any_stmt);
  Contract(misc_attrs);

  if (ast_misc_attr_values != NULL) {
    report_error(ast_misc_attr_values, "CQL0327: a value should not be assigned to no_table_scan attribute", NULL);
    record_error(ast_misc_attr_values);
    record_error(misc_attrs);
    return;
  }

  if (is_ast_stmt_and_attr(misc_attrs->parent)) {
    ast_node *stmt = misc_attrs->parent->right;
    if (!is_ast_create_table_stmt(stmt)) {
      report_error(misc_attrs, "CQL0328: no_table_scan attribute may only be added to a create table statement", NULL);
      record_error(misc_attrs);
      return;
    }
  }
}

// This function validate the semantic of vault_sensitive attribute. The attribute does not take a value
// and can only be used in create proc statement.
// The vault_sensitive attribution should look like this:
// @attribute(cql:vault_sensitive=(<column_name1>, <column_name2>, ...)) or
// @attribute(cql:vault_sensitive=(<context_column_name>, (<column_name1>, <column_name2>, ...)))
// <context_column_name> can be any column name in the resultset, and will be passed in as encoding context param
// <column_name1>, <column_name2>, ... are the column names to be encoded if eligible.
static void sem_misc_attrs_vault_sensitive(
    CSTR misc_attr_prefix,
    CSTR misc_attr_name,
    ast_node *ast_misc_attr_values,
    ast_node *misc_attrs,
    ast_node *any_stmt) {
   Contract(misc_attr_name);
  Contract(any_stmt);
  Contract(misc_attrs);

  if (is_ast_stmt_and_attr(misc_attrs->parent)) {
    ast_node *stmt = misc_attrs->parent->right;
    if (!is_ast_create_proc_stmt(stmt)) {
      report_error(misc_attrs, "CQL0328: vault_sensitive attribute may only be added to a create procedure statement", NULL);
      record_error(misc_attrs);
      return;
    }
  }

  if (exists_attribute_str(misc_attrs, "vault_sensitive")) {
    encode_info info;
    info.encode_columns = NULL;
    info.encode_context_column = NULL;
    info.misc_attrs = misc_attrs;
    find_misc_attrs(misc_attrs, sem_find_ast_misc_attr_vault_sensitive_callback, &info);
  }
}

// Semantic anlysis of ok_table_scan and no_table_scan attribution.
// ok_table_scan: can only be assigned to a create proc statement and
// the value can only be table names.
// no_table_scan: can only be assigned to a create table statement and
// has not value.
static void sem_misc_attrs_callback(
  CSTR misc_attr_prefix,
  CSTR misc_attr_name,
  ast_node *ast_misc_attr_values,
  void *context) {
  Contract(misc_attr_name);
  Contract(context);
  EXTRACT_NOTNULL(misc_attrs, context);

  if (!misc_attr_prefix) {
    return;
  }

  // We can stop as soon as any misc_attr has an error.
  if (is_error(misc_attrs)) {
    return;
  }

  EXTRACT_NOTNULL(stmt_and_attr, misc_attrs->parent);
  EXTRACT_ANY_NOTNULL(any_stmt, stmt_and_attr->right);

  symtab_entry *entry = symtab_find(misc_attributes, misc_attr_name);
  if (entry) {
    ((sem_misc_attribute_callback)entry->val)(
        misc_attr_prefix,
        misc_attr_name,
        ast_misc_attr_values,
        misc_attrs,
        any_stmt);
  }
}

// Semantic analysis of any attributions that can appear in any statement
static void sem_misc_attrs(ast_node *ast) {
  Contract(is_ast_misc_attrs(ast));

  // Assume the node is ok;  only some nodes get semantic analysis.
  record_ok(ast);

  find_misc_attrs(ast, sem_misc_attrs_callback, ast);
}

// Semantic analysis of stored procedures is fairly easy at the core:
//  * check for duplicate names
//  * validate the paramaters are well formed
//  * set the current proc in flight (this not allowed to nest)
//  * recurse on the statement list and prop errors
//  * record the name of the procedure for callers
// In addition, while processing the statement:
//  * the HAS_DML flag is added if any DML/DDL statements are encountered
//    * this will change the emitted signature of the proc to include a sqlite3 *db
//      input argument and it will return a sqlite error code (e.g. SQLITE_OK)
//  * select statements that are loose in the proc represent the "return" of that
//    select;  this changes the signature to include a sqlite3_stmt **pstmt parameter
//
static void sem_create_proc_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  // any early exit is an error
  bool_t error = true;

  if (current_proc) {
    report_error(name_ast, "CQL0184: stored procedures cannot be nested", name);
    goto cleanup;
  }

  uint32_t frag_type = find_proc_frag_type(ast);
  in_shared_fragment = frag_type == FRAG_TYPE_SHARED;

  Invariant(!locals);
  Invariant(!local_types);

  // create local storage for named type defined in the proc
  local_types = symtab_new();

  unitary_locals = _ast_pool_new(bytebuf);
  bytebuf_open(unitary_locals);

  // CREATE PROC [name] ( [params] )

  if (find_func(name)) {
    report_error(name_ast, "CQL0185: proc name conflicts with func name", name);
    goto cleanup;
  }

  if (find_interface_type(name)) {
    report_error(name_ast, "CQL0481: proc name conflicts with interface name", name);
    goto cleanup;
  }

  ast_node *existing_proc = find_proc(name);

  if (is_ast_create_proc_stmt(existing_proc)) {
    report_error(name_ast, "CQL0186: duplicate stored proc name", name);
    goto cleanup;
  }

  if (!existing_proc) {
    // proc doesn't exist, ok to add it
    bool_t added = add_proc(ast, name);
    Invariant(added);
  }
  else {
    // replace the declaration with this definition
    // the declare is now upgraded to a (pending) create
    symtab_entry *entry = symtab_find(procs, name);
    entry->val = ast;
  }

  ast_node *schema_attr = find_upgrade_proc(name);
  if (schema_attr) {
    Invariant(is_ast_create_attr(schema_attr) ||
              is_ast_delete_attr(schema_attr) ||
              is_ast_schema_ad_hoc_migration_stmt(schema_attr));

    EXTRACT(version_annotation, schema_attr->left);
    EXTRACT_OPTION(vers, version_annotation->left);

    if (vers != schema_upgrade_version) {
      CSTR msg = dup_printf("CQL0187: @schema_upgrade_version not declared or doesn't match upgrade version %d for proc", vers);
      report_error(name_ast, msg, name);
      goto cleanup;
    }

    if (params) {
      report_error(params, "CQL0233: procedure previously declared as schema upgrade proc, it can have no args", name);
      goto cleanup;
    }
  }

  current_proc = ast;

  sem_inside_create_proc_stmt(ast);
  if (is_error(ast)) {
    goto cleanup;
  }

  if (schema_attr && !has_dml) {
    report_error(name_ast, "CQL0188: procedure is supposed to do schema migration but it doesn't have any DML", name);
    goto cleanup;
  }

  if (is_struct(ast->sem->sem_type)) {
    sem_validate_unique_names_struct_type(ast);
    if (is_error(ast)) {
      goto cleanup;
    }
  }

  if (existing_proc) {
    bool_t matching = sem_validate_identical_procs(existing_proc, ast);
    if (!matching) {
      report_error(ast, "CQL0189: procedure declarations/definitions do not match", name);
      goto cleanup;
    }
  }

  // Check for valid autodrops, identity column, vault_sensitive or fragment annotations
  // Note: these attribute are ignored on empty procs because they are meaningless.
  if (misc_attrs && stmt_list) {
    bool_t result_set_proc = has_result_set(ast);
    bool_t out_stmt_proc = has_out_stmt_result(ast);
    bool_t out_union_proc = has_out_union_stmt_result(current_proc);

    // If a stored proc is marked with the vault_sensitive annotation then we validate
    // both the encode context and encode columns.  The attributes should look like this:
    // @attribute(cql:vault_sensitve=(col1, col2, ,...)) or
    // @attribute(cql:vault_sensitve=(context_col, (col1, col2, ,...)))
    annotation_target = "vault_sensitive";
    if (exists_attribute_str(misc_attrs, annotation_target)) {
      encode_info info;
      info.encode_columns = NULL;
      info.encode_context_column = NULL;
      info.misc_attrs = misc_attrs;
      find_misc_attrs(misc_attrs, sem_find_ast_misc_attr_vault_sensitive_callback, &info);
    }
    if (is_error(misc_attrs)) {
      goto cleanup;
    }
    annotation_target = NULL;

    // Vault required db pointer to encode/decode column values. Because of that the proc with vault
    // attribution should have access to the db pointer. Only dml proc has a db pointer, therefore
    // vault annotation can only be available to dml proc.
    if (exists_attribute_str(misc_attrs, "vault_sensitive") && !has_dml) {
      report_error(misc_attrs, "CQL0364: vault_sensitive annotation can only go on a procedure that uses the database", NULL);
      record_error(misc_attrs);
      goto cleanup;
    }

    // If a stored proc is marked with the identity annotation then we generate the
    // "sameness" helper method that checks those columns.  The attributes should look like this:
    // @attribute(cql:identity=(col1, col2, ,...))
    uint32_t identity_count = sem_column_name_annotation(misc_attrs, find_identity_columns, "procedure identity");
    if (is_error(misc_attrs)) {
      goto cleanup;
    }

    if (identity_count && !result_set_proc && !out_stmt_proc && !out_union_proc) {
      report_error(misc_attrs, "CQL0240: identity annotation can only go on a procedure that returns a result set", name);
      record_error(misc_attrs);
      goto cleanup;
    }

    if (frag_type == FRAG_TYPE_MIXED) {
      report_error(misc_attrs, "CQL0318: more than one fragment annotation on procedure", name);
      goto cleanup;
    }
    else if (frag_type == FRAG_TYPE_SHARED) {
      sem_shared_fragment(misc_attrs, ast);
      if (is_error(ast)) {
        goto cleanup;
      }
    }
    else if (frag_type == FRAG_TYPE_BASE) {
      sem_base_fragment(misc_attrs, stmt_list, ast);
      if (is_error(ast)) {
        goto cleanup;
      }
    }
    else if (frag_type == FRAG_TYPE_EXTENSION) {
      sem_extension_fragment(misc_attrs, stmt_list, ast);
      if (is_error(ast)) {
        goto cleanup;
      }
    }
    else if (frag_type == FRAG_TYPE_ASSEMBLY) {
      sem_assembly_fragment(misc_attrs, stmt_list, ast);
      if (is_error(ast)) {
        goto cleanup;
      }
    }

    sem_autotests(misc_attrs);
    if (is_error(misc_attrs)) {
      goto cleanup;
    }

    find_attribute_str(misc_attrs, sem_validate_one_interface, ast, "implements");
    if (is_error(ast)) {
      record_error(misc_attrs);
      goto cleanup;
    }

    uint32_t autodrop_count = sem_autodrops(misc_attrs);
    if (is_error(misc_attrs)) {
      goto cleanup;
    }

    if (autodrop_count) {
      if (!result_set_proc && !out_stmt_proc) {
        // note: out union doesn't need autodrop, it has no auto-fetcher, so autodrop isn't even valid
        report_error(misc_attrs, "CQL0234: autodrop annotation can only go on a procedure that returns a result set", name);
        record_error(misc_attrs);
        goto cleanup;
      }

      if (!has_dml) {
        report_error(misc_attrs, "CQL0236: autodrop annotation can only go on a procedure that uses the database", name);
        record_error(misc_attrs);
        goto cleanup;
      }
    }
  }

  // success!
  error = false;

cleanup:

  if (error) {
    record_error(ast);
  }

  // this has already been set (either the error case or the result)
  Invariant(ast->sem);

  ast->sem->region = current_region;
  name_ast->sem = ast->sem;
  current_proc = NULL;
  SYMTAB_CLEANUP(local_types);
  in_shared_fragment = false;
  BYTEBUF_CLEANUP(unitary_locals);
}

// Validate the name is unique in the given name list and attach the type
// to the name in the semantic type.  The only thing that can go wrong here
// is if the name is not unique.  The type ast has no error cases.
static void sem_typed_name(ast_node *typed_name, symtab *names) {
  Contract(is_ast_typed_name(typed_name));
  EXTRACT_ANY_NOTNULL(name_ast, typed_name->left);
  EXTRACT_STRING(name, name_ast);

  if (!symtab_add(names, name, typed_name)) {
    report_error(name_ast, "CQL0190: duplicate column name", name);
    record_error(typed_name);
    return;
  }

  sem_data_type_var(typed_name->right);
  typed_name->sem = typed_name->right->sem;
  typed_name->sem->name = name;
}

// Here we create a structure type from the list of typed names
// First each name is evaluated and checked for duplicates.
// One the types are determined, we create the struct type with
// the correct number of fields and simply copy in the type of
// each name into the sptr.
static void sem_typed_names(ast_node *head) {
  Contract(is_ast_typed_names(head));

  rewrite_typed_names(head);
  if (is_error(head)) {
    return;
  }

  symtab *names = symtab_new();
  uint32_t count = 0;

  for (ast_node *ast = head; ast; ast = ast->right, count++) {
    Contract(is_ast_typed_names(ast));
    EXTRACT(typed_name, ast->left);
    sem_typed_name(typed_name, names);

    if (is_error(typed_name)) {
      record_error(head);
      symtab_delete(names);
      return;
    }
  }

  symtab_delete(names);

  head->sem = new_sem(SEM_TYPE_STRUCT);
  sem_struct *sptr = new_sem_struct("select", count);
  head->sem->sptr = sptr;

  int32_t i = 0;
  for (ast_node *ast = head; ast; ast = ast->right, i++) {
    Contract(is_ast_typed_names(ast));
    EXTRACT(typed_name, ast->left);
    sptr->names[i] = typed_name->sem->name;
    sptr->semtypes[i] = typed_name->sem->sem_type;
    sptr->kinds[i] = typed_name->sem->kind;
  }

  Invariant(i == count);
}

// Function declarations are simpler than proc; there is
// no possibility of a result set return, there must be a return type
// (use proc if there is none).  Optional args as usual. Also args can be unchecked.
static void sem_declare_func_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(
    is_ast_declare_func_stmt(ast) ||
    is_ast_declare_select_func_stmt(ast) ||
    is_ast_declare_select_func_no_check_stmt(ast)
  );

  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(func_params_return, ast->right);
  EXTRACT(params, func_params_return->left);
  EXTRACT_ANY_NOTNULL(ret_data_type, func_params_return->right);

  if (current_proc) {
    report_error(name_ast, "CQL0191: declared functions must be top level", name);
    record_error(ast);
    return;
  }

  Invariant(!locals);

  // CREATE FUNC [name] ( [params] ) [ret_data_type]

  if (find_proc(name)) {
    report_error(name_ast, "CQL0192: func name conflicts with proc name", name);
    record_error(ast);
    return;
  }

  ast_node *existing_func = find_func(name);
  ast_node *existing_unchecked_func = find_unchecked_func(name);

  // Prevent redeclaration of normal function to be unchecked and vice versa
  if (
    (is_ast_declare_select_func_no_check_stmt(ast) && existing_func) ||
    (!is_ast_declare_select_func_no_check_stmt(ast) && existing_unchecked_func)
  ) {
      report_error(ast, "CQL0486: function cannot be both a normal function and an unchecked function", name);
      record_error(ast);
      return;
  }

  // Check if it's ok to add a checked/unchecked func to their repsective symtabs
  if (is_ast_declare_select_func_no_check_stmt(ast)) {
    if (!existing_unchecked_func) {
      bool_t added = add_unchecked_func(ast, name);
      Invariant(added);
    }
  } else {
    if (!existing_func) {
      bool_t added = add_func(ast, name);
      Invariant(added);
    }
  }

  if (params) {
    current_variables = locals = symtab_new();
    arg_bundles = symtab_new();

    sem_params(params, NULL);

    symtab_delete(locals);
    locals = NULL;
    symtab_delete(arg_bundles);
    arg_bundles = NULL;
    current_variables = globals;

    if (is_error(params)) {
      record_error(ast);
      return;
    }
  }

  if (is_ast_typed_names(ret_data_type)) {
    sem_typed_names(ret_data_type);
  }
  else {
    sem_data_type_var(ret_data_type);
    if (is_error(ret_data_type)) {
      record_error(ast);
      return;
    }
  }

  // this also promotes errors up from the return type
  name_ast->sem = ast->sem = ret_data_type->sem;

  ast_node *matching_func = is_ast_declare_select_func_no_check_stmt(ast) ? existing_unchecked_func : existing_func;
  if (matching_func) {
    bool_t matching = sem_validate_identical_funcs(matching_func, ast);
    if (!matching) {
      report_error(name_ast, "CQL0193: duplicate function name", name);
      record_error(ast);
    }
  }

  if (will_add_current_entity()) {
    add_item_to_list(&all_functions_list, ast);
  }
}

// This is a helper function for handling select function declarations
static void sem_declare_select_func_stmt_common(ast_node *ast) {
  Contract(is_ast_declare_select_func_stmt(ast) || is_ast_declare_select_func_no_check_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(func_params_return, ast->right);
  EXTRACT_ANY_NOTNULL(ret_data_type, func_params_return->right);

  sem_declare_func_stmt(ast);
  if (is_error(ast)) {
    return;
  }

  if (symtab_find(builtin_funcs, name)) {
    report_error(name_ast, "CQL0314: select function does not require a declaration, it is a CQL built-in", name);
    record_error(ast);
    return;
  }

  if (core_type_of(ret_data_type->sem->sem_type) == SEM_TYPE_OBJECT) {
    report_error(name_ast, "CQL0347: select function may not return type OBJECT", name);
    record_error(ast);
    return;
  }

  sem_add_flags(ast, SEM_TYPE_SELECT_FUNC);

  bool_t adding_current_entity = will_add_current_entity();

  if (adding_current_entity) {
    add_item_to_list(&all_select_functions_list, ast);
  }
}

// This declares a UDF that is known to SQLite.
// Note that we cannot verify that SQLite actually knows this UDF
// You have to take steps yourself to register the UDF or there will
// be run time errors.
static void sem_declare_select_func_stmt(ast_node *ast) {
  Contract(is_ast_declare_select_func_stmt(ast));
  sem_declare_select_func_stmt_common(ast);
}

// This is similar to sem_declare_select_func_stmt, except
// parameters are marked to be unchecked. So calls to this function
// won't have their arguments type checked.
static void sem_declare_select_func_no_check_stmt(ast_node *ast) {
  Contract(is_ast_declare_select_func_no_check_stmt(ast));
  sem_declare_select_func_stmt_common(ast);
}

// If we are processing an enumeration you are allowed to use the previous
// values of the enum in later values, so for instance you could do this
//   declare enum foo (
//     big = 100,
//     medium = big/2,
//     small = medium/2
//  );
//
// This code recursively walks enum tree and replaces names it can
// with names from the enum that is currently being declared.
// This is the only place unqualified enum names can appear.
//
// Note that qualified names are untouched and the current enum is not
// yet in scope.
static void sem_replace_seen_enum_values(ast_node *ast, symtab *names) {
  Contract(ast);

  // we're lookign only for unqualified names
  if (is_ast_dot(ast) || is_strlit(ast)) {
     return;
  }

  if (!is_ast_str(ast)) {
    if (ast_has_left(ast)) {
       sem_replace_seen_enum_values(ast->left, names);
    }
    if (ast_has_right(ast)) {
       sem_replace_seen_enum_values(ast->right, names);
    }
    return;
  }

  // this name might be one of the enums for the enum in flight
  EXTRACT_STRING(name, ast);

  symtab_entry *entry = symtab_find(names, name);
  ast_node *enum_value = entry ? (ast_node*)(entry->val) : NULL;

  // this is an evaluated enum, previously seen
  if (enum_value) {
    // it *must* have been evaluated
    Invariant(enum_value->left);
    Invariant(enum_value->left->sem);
    Invariant(enum_value->left->sem->value);

    ast_node *ast_new = eval_set(ast, enum_value->left->sem->value);
    sem_root_expr(ast_new, SEM_EXPR_CONTEXT_NONE);
    ast->sem = ast_new->sem;
  }
}

// Enums are a way of declaring scoped numeric constants, the name
// reference of the enum will be rewritten wherever it appears so that
// neither the C compiler nor SQLite will ever see a enum name.  Which
// is good because neither would know its meaning.
// Declaration follows the usual rules.
//   * the name must be unique or else the declaration must be identical
//     to any we've seen before.
//   * the enum member names must be unique
//   * the values must be valid expressions that can be resolved, all
//     the values will be cast to the type of the enum
//   * if there is no value specified then the value is one greater than
//     the last value seen, or 1 if it is the first value
//   * the value expressions can include other enums (because those will
//     become constants) and they can include names that were previously
//     defined in this enum, those are replaced with constants in a pre-step.
static void sem_declare_enum_stmt(ast_node *ast) {
  Contract(is_ast_declare_enum_stmt(ast));
  EXTRACT_NOTNULL(typed_name, ast->left);
  EXTRACT_NOTNULL(enum_values, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, typed_name->left);
  EXTRACT_STRING(name, name_ast);
  sem_data_type_column(typed_name->right);
  typed_name->sem = typed_name->right->sem;
  typed_name->sem->sem_type |= SEM_TYPE_NOTNULL;
  typed_name->sem->name = name;
  typed_name->sem->kind = name;

  if (current_proc) {
    report_error(name_ast, "CQL0358: declared enums must be top level", name);
    record_error(ast);
    return;
  }

  ast_node *existing_enum = find_enum(name);

  ast->sem = typed_name->sem;
  symtab *names = symtab_new();
  sem_t sem_type_enum = typed_name->sem->sem_type;

  eval_node result = EVAL_NIL;
  result.int32_value = 0;
  result.sem_type = SEM_TYPE_INTEGER;

  while (enum_values) {
     EXTRACT_NOTNULL(enum_value, enum_values->left);
     EXTRACT_ANY_NOTNULL(enum_name_ast, enum_value->left);
     EXTRACT_STRING(enum_name, enum_name_ast);
     EXTRACT_ANY(expr, enum_value->right);

     if (!symtab_add(names, enum_name, enum_value)) {
       report_error(enum_value, "CQL0354: duplicate enum member", enum_name);
       record_error(ast);
       goto cleanup;
     }

     if (expr) {
       sem_replace_seen_enum_values(expr, names);
       sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);
       eval(expr, &result);

       if (result.sem_type == SEM_TYPE_ERROR || result.sem_type == SEM_TYPE_NULL) {
         report_error(enum_value, "CQL0355: evaluation failed", enum_name);
         record_error(ast);
         goto cleanup;
       }
     }
     else {
       eval_add_one(&result);
     }

     eval_cast_to(&result, ast->sem->sem_type);
     enum_name_ast->sem = new_sem(sem_type_enum);
     enum_name_ast->sem->value = _ast_pool_new(eval_node);
     *enum_name_ast->sem->value = result;

     enum_values = enum_values->right;
  }

  if (existing_enum) {
    bool_t matching = sem_validate_identical_ddl(ast, existing_enum);
    if (!matching) {
      report_error(ast, "CQL0356: enum definitions do not match", name);
      record_error(ast);
      goto cleanup;
    }
  }
  else {
    // note that enums  get a slightly different treatment when in previous schema
    // validation mode.  Most entites are not added to the name tables at all
    // we check it as we visit it and then move on;   We can't do that with enums
    // because they are used by later things (e.g. default values) and the "new" enums
    // (before the @previous_schema  marker) might be very different. We need the "old"
    // enums to calculate the default values or whatever and make sure they haven't changed.
    // So we can't just check them and move on like we do with other stuff.
    // At the end we'll have two symbol tables, the second of which we'll end up discarding.

    bool_t adding_current_entity = will_add_current_entity();

    // this enum is now visible, we still do this (even if previous schema mode)
    bool_t added = add_enum(ast, name);
    Invariant(added);

    // when processing previous schema we don't add the enum to the all enums list
    // so that it won't show up in JSON etc.
    if (adding_current_entity) {
      add_item_to_list(&all_enums_list, ast);

      // Add this enum to the list of global types like that enums and declare name types
      // can be search from a single storage.
      if (!add_named_type(name, ast)) {
        goto cleanup;
      }
    }
  }

cleanup:
   symtab_delete(names);
}

// Variables groups give us a convenient way of declare a bunch of unscoped
// variables that can be emitted as a unit.  This is a mirror to the enum
// and constant patterns.  The problem we're solving here is that if
// you declare a global variable then there is no way to just "extern" said
// variable.  The CQL declaration *is* the definition.  The group mechanism
// only gives you the extern declarations when you mention the group.  Though
// semantically this means nothing, they are still declared as usual.  When
// we go to codegen we emit (e.g.) "extern int foo".  To get the definition,
// you do "@emit_variable_group foo" then we will emit "int foo".  This
// mirrors enums and constants which have the same problem and solve it the
// same way.
static void sem_declare_group_stmt(ast_node *ast) {
  Contract(is_ast_declare_group_stmt(ast));

  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(stmt_list, ast->right);

  if (current_proc) {
    report_error(name_ast, "CQL0462: group declared variables must be top level", name);
    record_error(ast);
    return;
  }

  // We check for an existing variable group with the same name before analyzing
  // `stmt_list` so that we can bail out early if `ast` is an identical
  // redeclaration. If we didn't return early for the identical case, we'd run
  // into duplicate variable name errors when calling `sem_one_stmt` further
  // down.

  ast_node *existing_variable_group = find_variable_group(name);

  if (existing_variable_group) {
    bool_t matching = sem_validate_identical_ddl(ast, existing_variable_group);
    if (!matching) {
      report_error(ast, "CQL0463: variable definitions do not match in group", name);
      record_error(ast);
      return;
    }

    record_ok(ast);
    return;
  }

  symtab_add(variable_groups, name, ast);

  while (stmt_list) {
     EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

     sem_one_stmt(stmt);
     if (is_error(stmt)) {
        record_error(ast);
        return;
     }

     stmt_list = stmt_list->right;
  }

  record_ok(ast);
}

// Here we will eventually emit the the actual group variable definitions into
// the output stream. In semantic analysis we only have to verify that the group
// name is valid. See `sem_declare_group_stmt` above for more details.
static void sem_emit_group_stmt(ast_node *ast) {
  Contract(is_ast_emit_group_stmt(ast));
  EXTRACT(name_list, ast->left);

  while (name_list) {
    EXTRACT_ANY_NOTNULL(name_ast, name_list->left);
    EXTRACT_STRING(name, name_ast);

    if (!find_variable_group(name)) {
      report_error(name_ast, "CQL0464: group not found", name);
      record_error(ast);
      return;
    }

    name_list = name_list->right;
  }

  record_ok(ast);
}

// Constant groups are a way of declaring arbitrary unscoped constants, the name
// reference of the constant will be rewritten wherever it appears so that
// neither the C compiler nor SQLite will ever see a constant name.  Which
// is good because neither would know its meaning.
// Declaration follows the usual rules.
//   * the group name must be unique or else the declaration must be identical
//     to any we've seen before.
//   * the constant names must be unique
//   * the values must be valid expressions that can be resolved
//   * the value expressions can include other constants
//   * numeric constants expressions are evaluated at compile time
//   * string constants must be a string literal
static void sem_declare_const_stmt(ast_node *ast) {
  Contract(is_ast_declare_const_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(const_values, ast->right);

  if (current_proc) {
    report_error(name_ast, "CQL0358: declared constants must be top level", name);
    record_error(ast);
    return;
  }

  ast_node *existing_constant_group = find_constant_group(name);

  if (!existing_constant_group) {
    symtab_add(constant_groups, name, ast);
  }

  eval_node result = EVAL_NIL;

  while (const_values) {
     EXTRACT_NOTNULL(const_value, const_values->left);
     EXTRACT_ANY_NOTNULL(const_name_ast, const_value->left);
     EXTRACT_STRING(const_name, const_name_ast);
     EXTRACT_ANY(expr, const_value->right);

     if (!existing_constant_group) {
       if (!symtab_add(constants, const_name, const_value)) {
         report_error(const_value, "CQL0354: duplicate constant name", const_name);
         record_error(ast);
         return;
       }
     }

     sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);
     if (is_error(expr)) {
        record_error(ast);
        return;
     }

     // refetch, this could be rewritten
     expr = const_value->right;

     if (is_numeric(expr->sem->sem_type)) {
       eval(expr, &result);

       if (result.sem_type != SEM_TYPE_ERROR && result.sem_type != SEM_TYPE_NULL) {
         const_value->sem = expr->sem;
         const_name_ast->sem = expr->sem;
         const_name_ast->sem->value = _ast_pool_new(eval_node);
         *const_name_ast->sem->value = result;
         const_values = const_values->right;
         continue;
       }
     }
     else if (is_strlit(expr)) {
       const_name_ast->sem = expr->sem;
       const_value->sem = expr->sem;
       const_values = const_values->right;
       continue;
     }

     CHARBUF_OPEN(tmp);
     bprintf(&tmp, "%s = ", const_name);
     CSTR expr_text = dup_expr_text_buffer(&tmp, expr);

     report_error(expr, "CQL0177: global constants must be either constant numeric expressions or string literals", expr_text);
     record_error(expr);
     record_error(ast);
     CHARBUF_CLOSE(tmp);
     return;

  }

  if (existing_constant_group) {
    bool_t matching = sem_validate_identical_ddl(ast, existing_constant_group);
    if (!matching) {
      report_error(ast, "CQL0356: const definitions do not match", name);
      record_error(ast);
      return;
    }
  }
  else {
    // note that consts get a slightly different treatment when in previous schema
    // validation mode.  Most entites are not added to the name tables at all
    // we check it as we visit it and then move on;   We can't do that with consts
    // because they are used by later things (e.g. default values) and the "new" constants
    // (before the @previous_schema  marker) might be very different. We need the "old"
    // consts to calculate the default values or whatever and make sure they haven't changed.
    // So we can't just check them and move on like we do with other stuff.
    // At the end we'll have two symbol tables, the second of which we'll end up discarding.

    bool_t adding_current_entity = will_add_current_entity();

    // when processing previous schema we don't add the const to the all consts list
    // so that it won't show up in JSON etc.
    if (adding_current_entity) {
      add_item_to_list(&all_constant_groups_list, ast);
    }
  }

  record_ok(ast);
}

// Declares an external procedure that can be called with any combination of C args
// this is intended for procedures like `printf` that cannot be readily described with
// CQL strict types.
static void sem_declare_proc_no_check_stmt(ast_node *ast) {
  Contract(is_ast_declare_proc_no_check_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  if (find_proc(name)) {
    report_error(ast, "CQL0404: procedure cannot be both a normal procedure and an unchecked procedure", name);
    record_error(ast);
    return;
  }

  if (find_interface_type(name)) {
    report_error(ast, "CQL0481: proc name conflicts with interface name", name);
    record_error(ast);
    return;
  }

  // it can be added more than once, no need to check the return code
  add_unchecked_proc(ast, name);
  record_ok(ast);
}

// There are three forms of this declaration:
// 1. a regular proc with no DML
//    declare proc X(id integer);
// 2. a regular proc that uses DML (needs a db paramter and returns rc)
//    declare proc X(id integer) using transaction;
// 3. a proc that returns a result set, you provide the result columns
//    declare proc X(id integer) : (A bool not null, B text);
static void sem_declare_proc_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_declare_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_name_type, ast->left);
  EXTRACT_ANY_NOTNULL(name_ast, proc_name_type->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_OPTION(type, proc_name_type->right);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);

  if (current_proc) {
    report_error(name_ast, "CQL0194: declared procedures must be top level", name);
    record_error(ast);
    return;
  }

  Invariant(!locals);

  // CREATE PROC [name] ( [params] )

  if (find_unchecked_proc(name)) {
    report_error(ast, "CQL0404: procedure cannot be both a normal procedure and an unchecked procedure", name);
    record_error(ast);
    return;
  }

  if (find_func(name)) {
    report_error(name_ast, "CQL0195: proc name conflicts with func name", name);
    record_error(ast);
    return;
  }

  if (find_interface_type(name)) {
    report_error(ast, "CQL0481: proc name conflicts with interface name", name);
    record_error(ast);
    return;
  }

  ast_node *existing_proc = find_proc(name);
  if (!existing_proc) {
    // proc doesn't exist, ok to add it
    bool_t added = add_proc(ast, name);
    Invariant(added);
  }

  if (params) {
    current_variables = locals = symtab_new();
    arg_bundles = symtab_new();

    sem_params(params, NULL);

    symtab_delete(locals);
    locals = NULL;
    symtab_delete(arg_bundles);
    arg_bundles = NULL;
    current_variables = globals;

    if (is_error(params)) {
      record_error(ast);
      return;
    }
  }

  if (type & PROC_FLAG_STRUCT_TYPE) {
    EXTRACT_NOTNULL(typed_names, proc_params_stmts->right);
    sem_typed_names(typed_names);

    if (is_error(typed_names)) {
      record_error(ast);
      return;
    }

    ast->sem = typed_names->sem;
    ast->sem->sptr->struct_name = name;
  }
  else {
    ast->sem = new_sem(SEM_TYPE_OK);
  }

  if (type & PROC_FLAG_USES_OUT) {
    sem_add_flags(ast, SEM_TYPE_USES_OUT);
  }

  if (type & PROC_FLAG_USES_DML) {
    sem_add_flags(ast, SEM_TYPE_DML_PROC);
  }

  if (type & PROC_FLAG_USES_OUT_UNION) {
    sem_add_flags(ast, SEM_TYPE_USES_OUT_UNION);
  }

  name_ast->sem = ast->sem;

  if (existing_proc) {
    bool_t matching = sem_validate_identical_procs(existing_proc, ast);
    if (!matching) {
      report_error(ast, "CQL0196: procedure declarations/definitions do not match", name);
      record_error(ast);
    }
  }
}

static void sem_declare_interface_stmt(ast_node *ast) {
  Contract(!current_joinscope);  // I don't belong inside a select(!)
  Contract(is_ast_declare_interface_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT_NOTNULL(typed_names, proc_params_stmts->right);

  if (current_proc) {
    report_error(name_ast, "CQL0480: declared interface must be top level", name);
    record_error(ast);
    return;
  }

  Invariant(!locals);

  if (find_unchecked_proc(name) || find_proc(name)) {
    report_error(ast, "CQL0478: interface name conflicts with procedure name", name);
    record_error(ast);
    return;
  }

  if (find_func(name)) {
    report_error(name_ast, "CQL0477: interface name conflicts with func name", name);
    record_error(ast);
    return;
  }

  ast_node *existing_interface = find_interface_type(name);
  if (!existing_interface) {
    bool_t added = add_interface_type(ast, name);
    Invariant(added);
  }

  sem_typed_names(typed_names);

  if (is_error(typed_names)) {
    record_error(ast);
    return;
  }

  ast->sem = typed_names->sem;
  ast->sem->sptr->struct_name = name;

  if (existing_interface) {
      bool_t matching = sem_validate_identical_text(existing_interface, ast, gen_declare_interface_stmt, NULL);

      if (!matching) {
        report_error(ast, "CQL0479: interface declarations do not match", name);
        record_error(ast);
      }
    }

}

// This helper verifies that the name of a variable is ok in the current scope
//  * globals cannot conflict with globals or table/view names
//  * locals cannot conflict with each other
//  * scopes do not nest in CQL so any local is the same anywhere no matter
//    where it appears, it can be used any point later.  This could be changed.
static bool_t sem_verify_legal_variable_name(ast_node *variable, CSTR name) {
  // Do not erroneously warn about duplicate variables if we're reanalyzing
  // a statement list within a loop.
  if (current_loop_analysis_state != LOOP_ANALYSIS_STATE_REANALYZE) {
    if (symtab_find(current_variables, name)) {
      report_error(variable, "CQL0197: duplicate variable name in the same scope", name);
      return false;
    }
  }

  // global variables can't conflict with table names, not even deleted table names
  if (current_variables == globals && find_table_or_view_even_deleted(name)) {
    report_error(variable, "CQL0198: global variable hides table/view name", name);
    return false;
  }

  return true;
}

// This declares a new local or global variable that is not a cursor.
// The type is computed with the same helper that is used for analyzing
// column definitions.  Once we have the type we walk the list of variable
// names, check them for duplicates and such (see above) and assign their type.
// Variables gain the SEM_TYPE_VARIABLE info in their semantic node and
// their sem->name field is set.  Later if any case-insensitive match
// hits the variable, the sem->name field can be used to get the canonical name.
static void sem_declare_vars_type(ast_node *declare_vars_type) {
  Contract(is_ast_declare_vars_type(declare_vars_type));
  EXTRACT_NOTNULL(name_list, declare_vars_type->left);
  EXTRACT_ANY_NOTNULL(data_type, declare_vars_type->right);

  // DECLARE [name_list] [data_type]
  sem_data_type_var(data_type);
  if (is_error(data_type)) {
    record_error(declare_vars_type);
    return;
  }
  sem_t sem_type = data_type->sem->sem_type;
  Invariant(is_unitary(sem_type));

  bool_t error = false;

  for (ast_node *ast = name_list; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(variable, ast->left);
    EXTRACT_STRING(name, variable);

    if (!sem_verify_legal_variable_name(variable, name)) {
      record_error(variable);
      record_error(ast);
      error = true;
      continue;
    }

    variable->sem = ast->sem = new_sem(sem_type | SEM_TYPE_VARIABLE);
    if (variable_should_require_initialization(variable->sem->sem_type)) {
      variable->sem->sem_type |= SEM_TYPE_INIT_REQUIRED;
    }
    variable->sem->name = name;
    variable->sem->kind = data_type->sem->kind;
    add_variable(name, variable);
  }

  if (error) {
    record_error(declare_vars_type);
  }
  else {
    declare_vars_type->sem = new_sem(sem_type);
    declare_vars_type->sem->kind = data_type->sem->kind;
  }
}

// This declare a new local or global name for a type. It validate
// the data type and store the name declared. The name can be use
// in any places in the cql syntax where data type are.
static void sem_declare_named_type(ast_node *ast) {
  Contract(is_ast_declare_named_type(ast));
  EXTRACT_ANY(name_ast, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, ast->right);
  EXTRACT_STRING(name, name_ast);

  // DECLARE TYPE [name] [data_type]
  sem_data_type_var(data_type);
  if (is_error(data_type)) {
    record_error(ast);
    return;
  }

  // this also promotes errors up from the data type
  name_ast->sem = ast->sem = data_type->sem;

  if (!is_error(ast)) {
    if (!add_named_type(name, ast)) {
      return;
    }
  }
}

// There are two forms of the declare cursor, both of which allow this
// code to infer the type of the cursor.
//   * declare foo cursor for select etc.
//     * the type of the cursor is the net struct type of the select list
//   * declare foo cursor for call proc();
//     * proc must be statement that produces a result set via select (see above)
//     * the type of the cursor is the struct of the select returned by the proc
//     * note if there is more than one loose select in the proc they must match exactly
//   * cursor names have the same rules duplicates as other variables
// With this in mind, both cases simply recurse on either the select or the call
// and then pull out the struct type and use it for the cursor.
static void sem_declare_cursor(ast_node *ast) {
  Contract(is_ast_declare_cursor(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_STRING(name, cursor);

  sem_t out_union_and_dml = 0;

  if (is_select_stmt(ast->right)) {
    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

    // DECLARE [name] CURSOR FOR [select_stmt]
    // or
    // DECLARE [name] CURSOR FOR [explain_stmt]
    sem_select(select_stmt);
    if (is_error(select_stmt)) {
      record_error(ast);
      return;
    }

    // We need to know whether or not the cursor source of data is a DML.
    // A DML source require a not null db pointer. This info is used to
    // decided whether we can do encoding/decoding of result_set's fields.
    out_union_and_dml = SEM_TYPE_DML_PROC;
  }
  else if (is_ast_call_stmt(ast->right)) {
    EXTRACT_NOTNULL(call_stmt, ast->right);

    // DECLARE [name] CURSOR FOR [call_stmt]]
    sem_call_stmt_opt_cursor(call_stmt, name);
    if (is_error(call_stmt)) {
      record_error(ast);
      return;
    }

    if (!is_struct(call_stmt->sem->sem_type)) {
      report_error(call_stmt, "CQL0199: cursor requires a procedure that returns a result set via select", name);
      record_error(ast);
      return;
    }

    if (has_out_stmt_result(call_stmt)) {
      report_error(ast, "CQL0270: use FETCH FROM for procedures that returns a cursor with OUT", name);
      record_error(ast);
      return;
    }

    out_union_and_dml = has_out_union_stmt_result(call_stmt) ? SEM_TYPE_USES_OUT_UNION : 0;
    // We need to know whether or not the cursor source of data is a DML.
    // A DML source require a not null db pointer. This info is used to
    // decided whether we can do encoding/decoding of result_set's fields.
    out_union_and_dml |= call_stmt->sem->sem_type & SEM_TYPE_DML_PROC;
  }
  else {
    sem_declare_cursor_for_expr(ast);
    return;
  }

  if (!sem_verify_legal_variable_name(ast, name)) {
    record_error(ast->left);
    record_error(ast);
    return;
  }

  // SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE <=> it's a cursor
  cursor->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | out_union_and_dml);
  cursor->sem->sptr = ast->right->sem->sptr;
  cursor->sem->name = name;
  ast->sem = cursor->sem;

  add_variable(name, cursor);
}

// This is the "unboxing" primitive for cursors.  The idea here is that
// you have an object variable with a statement in it and you want to
// make a cursor over that statement.
static void sem_declare_cursor_for_expr(ast_node *ast) {
  Contract(is_ast_declare_cursor);
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);
  EXTRACT_STRING(name, cursor);

  // DECLARE cursor_name CURSOR FOR expr

  if (!sem_verify_legal_variable_name(ast, name)) {
    record_error(ast->left);
    record_error(ast);
    return;
  }

  sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);
  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  // the indicated type must be a valid shape name (one we could use in LIKE T)
  ast_node *like_target = sem_find_likeable_from_expr_type(expr);
  if (!like_target) {
    record_error(ast);
    return;
  }

  sem_t cursor_flags = SEM_TYPE_BOXED;
  if (ends_in_set(expr->sem->kind)) {
    cursor_flags = SEM_TYPE_USES_OUT_UNION;
  }

  // the cursor is marked as BOXED because there is a boxed object controlling its lifetime

  // SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE <=> it's a cursor
  cursor->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | cursor_flags);
  cursor->sem->sptr = like_target->sem->sptr;
  cursor->sem->name = name;
  ast->sem = cursor->sem;

  add_variable(name, cursor);
}

// Verify that the indicated variable has a valid cursor type
// and return the type associated with it.  The rules are:
//  * the variable must be of type object<T CURSOR> or object<T SET> for some T
//  * the T part must be a "likeable" expression (i.e. a shape)
// there is some string massaging to check for and remove the
// " CURSOR" or " SET" and a temporary node is created so we can re-use
// the usual likeable name check.
cql_noexport ast_node *sem_find_likeable_from_expr_type(ast_node *expr) {
  // it has to be a typed object expression
  if (!is_object(expr->sem->sem_type) || !expr->sem->kind) {
    CSTR expr_text = dup_expr_text(expr);
    report_error(expr, "CQL0346: expression must be of type object<T cursor> where T is a valid shape name", expr_text);
    return NULL;
  }

  CSTR kind = expr->sem->kind;

  size_t len = strlen(kind);
  size_t len_tail = ends_in_cursor(kind);
  if (!len_tail) {
    len_tail = ends_in_set(kind);
    if (!len_tail) {
      CSTR expr_text = dup_expr_text(expr);
      report_error(expr, "CQL0343: variable must be of type object<T CURSOR> or object<T SET> where T is a valid shape name", expr_text);
      return NULL;
    }
  }

  // now we extract just the type name having ignored the " CURSOR" part.
  CHARBUF_OPEN(tmp);
  for (int32_t i = 0; i < len - len_tail; i++) {
    bputc(&tmp, kind[i]);
  }

  // We make a like node for the object type (which is itself not in AST here)
  // so that we can use the standard likeable helpers for error checking
  AST_REWRITE_INFO_SET(expr->lineno, expr->filename);
  ast_node *type_node = new_ast_str(tmp.ptr);
  ast_node *like_node = new_ast_like(type_node, NULL);
  AST_REWRITE_INFO_RESET();

  CHARBUF_CLOSE(tmp);

  // the indicated type must be a valid shape name (one we could use in LIKE T)
  ast_node *like_target = sem_find_shape_def_base(like_node, LIKEABLE_FOR_VALUES);

  // we have already checked the name when the object was declared, or else we made the name ourselves
  // in all these cases it MUST be valid already or else something is seriously wrong.
  Invariant(like_target);

  return like_target;
}

// This is the boxing primitive for cursors.  We will take the statement cursor
// and construct an object variable with a type name that corresponds to the
// shape of the cursor.  This variable can then be passed around as usual
// and at a later time you can extract the underlying statement with the
// unboxing primitive above.  There are a number of things that can go wrong
// here.  There must be a suitable cursor, a suitable shape, and the shape
// must exactly match the cursor shape for starters.
static void sem_set_from_cursor(ast_node *ast) {
  Contract(is_ast_set_from_cursor(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->right);
  EXTRACT_STRING(cursor_name, cursor);
  EXTRACT_STRING(var_name, ast->left);

  if (try_rewrite_blob_fetch_forms(ast)) {
    // true means affirmative success or failure and errors logged already
    has_dml = 1;  // this implies return code and all that comes with it
    return;
  }

  // must be a valid cursor
  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  // SET [name] FROM CURSOR [cursor_name]
  ast_node *var = find_local_or_global_variable(var_name);

  if (!var) {
    report_error(ast, "CQL0173: variable not found", var_name);
    record_error(ast);
    return;
  }

  ast->left->sem = var->sem;

  // the indicated type must be a valid shape name (one we could use in LIKE T)
  ast_node *like_target = sem_find_likeable_from_expr_type(var);

  // we have already checked the name when the object was declared, or else we made the name ourselves
  // in all these cases it MUST be valid already or else something is seriously wrong.
  Invariant(like_target);

  // the cursor has to be a statement cursor
  if (cursor->sem->sem_type & SEM_TYPE_VALUE_CURSOR) {
    report_error(cursor,
       "CQL0261: cursor did not originate from a SQLite statement, it only has values", cursor->sem->name);
    record_error(ast);
    return;
  }

  sem_verify_identical_columns(like_target, cursor, "in the cursor and the variable type");
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  // Tag the cursor *variable* (i.e. the AST from the original definition site
  // of the cursor) as a boxed cursor. This is necessary because we need to
  // have this information available when we process the declaration in codegen
  // before we see that it was boxed.
  ast_node *cursor_var = find_local_or_global_variable(cursor->sem->name);
  Invariant(cursor_var);
  Invariant(is_cursor(cursor_var->sem->sem_type));
  sem_add_flags(cursor_var, SEM_TYPE_BOXED);

  ast->sem = cursor_var->sem;
}

// Here we're going to make a new value cursor using the indicated name for the shape.
// The name has to be "likeable" meaning it refers to some named thing with a shape
// such as a table, a view, another cursor, or a procedure that returns a result set.
// These are the so called "value cursors" in that they have no underlying statement
// that they move through.  You can just load them up with a row and pass them around.
static void sem_declare_cursor_like_name(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_name(ast));
  EXTRACT_ANY_NOTNULL(new_cursor_ast, ast->left);
  EXTRACT_STRING(new_cursor_name, new_cursor_ast);
  EXTRACT_ANY_NOTNULL(shape_def, ast->right);

  // no duplicates allowed
  if (!sem_verify_legal_variable_name(ast, new_cursor_name)) {
    record_error(new_cursor_ast);
    record_error(ast);
    return;
  }

  // must be a valid shape
  ast_node *found_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
  if (!found_shape) {
    record_error(ast);
    return;
  }

  // good to go, make our cursor, with storage.
  shape_def->sem = found_shape->sem;
  new_cursor_ast->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | SEM_TYPE_VALUE_CURSOR | SEM_TYPE_HAS_SHAPE_STORAGE);
  new_cursor_ast->sem->sptr = found_shape->sem->sptr;
  new_cursor_ast->sem->name = new_cursor_name;
  ast->sem = new_cursor_ast->sem;

  add_variable(new_cursor_name, new_cursor_ast);
}

// Here we make a value cursor from the template of a select statement.
// The select statement only provides type shape, it's the most flexible way to
// create a structure type.  Note:  we want to add other ways like a table name
// or the name of another cursor.  But for now we have the most powerful one
// if also the most verbose.
// * The select must be itself valid.
// * The cursor name must be unique
static void sem_declare_cursor_like_select(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_select(ast));
  Contract(is_select_stmt(ast->right));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_STRING(name, cursor);

  EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

  // DECLARE [name] CURSOR LIKE [select_stmt]
  {
    // the select statement doesn't count as DML because we won't be running it
    bool_t has_dml_saved = has_dml;
    sem_select(select_stmt);
    has_dml = has_dml_saved;
  }

  if (is_error(select_stmt)) {
    record_error(ast);
    return;
  }

  sem_verify_no_anon_no_null_columns(select_stmt);
  if (is_error(select_stmt)) {
    record_error(ast);
    return;
  }

  if (!sem_verify_legal_variable_name(ast, name)) {
    record_error(ast->left);
    record_error(ast);
    return;
  }

  // SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE <=> it's a cursor
  cursor->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | SEM_TYPE_VALUE_CURSOR | SEM_TYPE_HAS_SHAPE_STORAGE);
  cursor->sem->sptr = select_stmt->sem->sptr;
  cursor->sem->name = name;
  ast->sem = cursor->sem;

  add_variable(name, cursor);
}


// Here we make a value cursor from a list of typed names
// * the typed names must be valid types/names with no duplicate column names etc.
//   * they make include rewrites using LIKE internally
// * The cursor name must be unique
static void sem_declare_cursor_like_typed_names(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_typed_names(ast));
  EXTRACT_NOTNULL(typed_names, ast->right);
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_STRING(name, cursor);

  // DECLARE [name] CURSOR LIKE ( [select_stmt] )

  sem_typed_names(typed_names);
  if (is_error(typed_names)) {
    record_error(ast);
    return;
  }

  if (!sem_verify_legal_variable_name(ast, name)) {
    record_error(ast->left);
    record_error(ast);
    return;
  }

  // SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE <=> it's a cursor
  cursor->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | SEM_TYPE_VALUE_CURSOR | SEM_TYPE_HAS_SHAPE_STORAGE);
  cursor->sem->sptr = typed_names->sem->sptr;
  cursor->sem->name = name;
  ast->sem = cursor->sem;

  add_variable(name, cursor);
}


// Here we're just checking that the proc mentioned in the call statement uses the OUT cursor form
static void sem_call_stmt_has_out_stmt_result_for_cursor(ast_node *call_stmt, CSTR name) {
  Contract(is_ast_call_stmt(call_stmt));

  if (!has_out_stmt_result(call_stmt)) {
    report_error(call_stmt, "CQL0203: cursor requires a procedure that returns a cursor with OUT", name);
    record_error(call_stmt);
    return;
  }
}

// This statement declares a cursor that will be based on the return type of a procedure
// when using this form the cursor is also fetched, hence the name.  The fetch result of
// there stored proc will be used for the value.  At this point we use its type only.
// * the call must be semantically valid
// * the procedure must return an OUT parameter (not a result set)
// * the cursor name must be unique
static void sem_declare_value_cursor(ast_node *ast) {
  Contract(is_ast_declare_value_cursor(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT_STRING(name, cursor);

  EXTRACT_NOTNULL(call_stmt, ast->right);

  // DECLARE [name] CURSOR FETCH FROM [call_stmt]]
  sem_call_stmt_opt_cursor(call_stmt, name);
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  sem_call_stmt_has_out_stmt_result_for_cursor(call_stmt, name);
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  if (!sem_verify_legal_variable_name(ast, name)) {
    record_error(ast->left);
    record_error(ast);
    return;
  }

  // SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE <=> it's a cursor
  cursor->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | SEM_TYPE_VALUE_CURSOR | SEM_TYPE_HAS_SHAPE_STORAGE);
  cursor->sem->sptr = ast->right->sem->sptr;
  cursor->sem->name = name;
  cursor->sem->sem_type |= call_stmt->sem->sem_type & SEM_TYPE_DML_PROC;
  ast->sem = cursor->sem;

  add_variable(name, cursor);
}

static ast_node *sem_synthesize_current_locals() {
  if (!current_proc) {
    return NULL;
  }

  AST_REWRITE_INFO_SET(current_proc->lineno, current_proc->filename);
  CSTR locals_name = "LOCALS";

  uint32_t usable_locals = unitary_locals->used / sizeof(ast_node *);
  ast_node **l_syms = (ast_node **)unitary_locals->ptr;

  sem_struct *sptr = new_sem_struct(locals_name, usable_locals);

  ast_node *ast_locals = new_ast_str(locals_name);
  ast_locals->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_HAS_SHAPE_STORAGE);
  ast_locals->sem->name = locals_name;
  ast_locals->sem->sptr = sptr;

  for (uint32_t i = 0; i < usable_locals; i++) {
    ast_node *var = l_syms[i];
    Invariant(is_unitary(var->sem->sem_type));

    sptr->names[i] = var->sem->name;
    sptr->kinds[i] = var->sem->kind;
    sptr->semtypes[i] = var->sem->sem_type;
  }

  AST_REWRITE_INFO_RESET();

  return ast_locals;
}


// Try to analyze the name first as an arg bundle, and that fails, then try as a cursor
// these are the two shapes that hold data.
cql_noexport void sem_any_shape(ast_node *ast) {
  EXTRACT_STRING(name, ast);
  ast_node *shape = find_arg_bundle(name);

  if (shape) {
    ast->sem = shape->sem;
    return;
  }

  // give better error messages if the shape is "ARGUMENTS"
  if (!strcmp("ARGUMENTS", name)) {
    if (current_proc) {
      EXTRACT_STRING(proc_name, get_proc_name(current_proc));
      report_error(ast, "CQL0340: FROM ARGUMENTS used in a procedure with no arguments", proc_name);
    }
    else {
      report_error(ast, "CQL0163: FROM ARGUMENTS construct is only valid inside a procedure", NULL);
    }
    record_error(ast);
    return;
  }

  // try it as a cursor (whatever error happens here will be the final answer)
  sem_cursor(ast);
}

// Cursors appear in only a few places legally as an actual cursor:
//
//  * fetch cursor [one of the fetch flavors]
//  * close cursor
//  * on the left side of X.field where X is a cursor that was autofetched
//  * on the right side of a `declare cursor C like ...` statement.
//
//  In those cases, we specifically verify that is, in fact, a cursor.  In other
//  cases, the name of a cursor refers to a boolean that indicates whether the
//  cursor presently has a value.
cql_noexport void sem_cursor(ast_node *ast) {
  if (!is_id(ast)) {
    CSTR expr_text = dup_expr_text(ast);
    report_error(ast, "CQL0205: not a cursor", expr_text);
    record_error(ast);
    return;
  }

  EXTRACT_STRING(name, ast);

  sem_resolve_id(ast, name, NULL);
  if (is_error(ast)) {
    return;
  }

  if (!is_cursor(ast->sem->sem_type)) {
    report_error(ast, "CQL0205: not a cursor", name);
    record_error(ast);
    return;
  }
}

// Information about switch cases, and the origin of the constants
// this will be used to ensure enums are covered and there are no duplicates in the case list.
typedef struct case_val {
  int64_t value;
  ast_node *source;
} case_val;

// Switch cases semantic analysis:
// * the case expressions must be constant expressions
// * the case expressions must promote to the type of the expression with no loss
// * if all_values was specified you can't use else or it's a joke
static void sem_switch_expr_list(ast_node *ast, sem_t core_type, bytebuf *case_data) {
  Contract(is_ast_expr_list(ast));
  ast_node *head = ast;

  while (ast) {
    Contract(is_ast_expr_list(ast));
    EXTRACT_ANY_NOTNULL(expr, ast->left);
    sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);
    if (is_error(expr)) {
      record_error(head);
      return;
    }
    // we're going to do an immediate eval and it might have been replaced
    // by enum rewrite.. so we have to fetch the node again.
    expr = ast->left;
    Invariant(expr);

    sem_t core_type_expr = core_type_of(expr->sem->sem_type);
    if (core_type_expr > core_type) {
      report_error(expr, "CQL0382: type of a WHEN expression is bigger than the type of the SWITCH expression", NULL);
      record_error(head);
      return;
    }

    eval_node result = EVAL_NIL;
    eval(expr, &result);

    if (result.sem_type == SEM_TYPE_ERROR) {
      report_error(expr, "CQL0380: WHEN expression cannot be evaluated to a constant", NULL);
      record_error(head);
      return;
    }

    eval_cast_to(&result, SEM_TYPE_LONG_INTEGER);

    case_val val = {
      .value = result.int64_value,
      .source = expr
    };

    bytebuf_append(case_data, &val, sizeof(val));
    ast = ast->right;
  }

  record_ok(head);
}

static int case_val_comparator(const void *v1, const void *v2) {
  case_val *c1 = (case_val *)v1;
  case_val *c2 = (case_val *)v2;

  if (c1->value < c2->value) return -1;
  if (c1->value > c2->value) return 1;
  return 0;
}

// Here we have a few things to do:
//  * first we verify that the switch expression is indeed an enum type
//  * we already know that the type of the expression is integral so we don't have to check that again
//  * at that point we need all the enum values, we get all the ones that do not start with "_"
//     * this allows you to have psuedo-values like "_max" in your enum, simple convention
//  * the enum may have aliases so we have to dedupe the values, this gives us the final count of items
//     * we can de-dupe in place
//  * then we sort the enum values, the case values are already sorted from the duplicates check
//  * then we merge the case values and the enum values
//    * we only need one index since we are going to error out at the first divergence of the merge
//    * we report extra values on either side as "missing" or "invalid"
//  * if the merge ends prematurely, whichever side has more values yields an error for missing or extra values
// After that clean up the memory and we're done...
static void sem_check_all_values_condition(ast_node *expr, bytebuf *case_buffer) {
  bytebuf *enum_buffer = _ast_pool_new(bytebuf);
  bytebuf_open(enum_buffer);

  int32_t case_count = case_buffer->used / sizeof(case_val);
  case_val *case_vals = (case_val *)case_buffer->ptr;

  CSTR kind = expr->sem->kind;
  ast_node *enum_stmt = NULL;

  if (!kind || !(enum_stmt = find_enum(kind))) {
    report_error(expr, "CQL0386: SWITCH ... ALL VALUES is used but the switch expression is not an enum type", NULL);
    record_error(expr);
    goto cleanup;
  }

  Invariant(is_ast_declare_enum_stmt(enum_stmt));

  // enumerate the list of enums and get their values, convert them all to LONG and then add them to the list
  EXTRACT_NOTNULL(enum_values, enum_stmt->right);

  while (enum_values) {
     EXTRACT_NOTNULL(enum_value, enum_values->left);
     EXTRACT_STRING(enum_member, enum_value->left);

     if (enum_member[0] != '_') {
       eval_node result = *enum_value->left->sem->value;
       eval_cast_to(&result, SEM_TYPE_LONG_INTEGER);

       case_val val = {
         .value = result.int64_value,
         .source = enum_value
       };

       bytebuf_append(enum_buffer, &val, sizeof(val));
     }

     enum_values = enum_values->right;
  }

  size_t enum_count = enum_buffer->used / sizeof(case_val);
  case_val *enum_vals = (case_val *)enum_buffer->ptr;
  qsort(enum_vals, enum_count, sizeof(case_val), case_val_comparator);

  // dedupe the enumeration cases, there are sometimes aliases
  // e.g. declare enum integer ( x = 1, another_name_for_x = 1);

  uint32_t i = 0;
  uint32_t j = 0;

  for (i = 0; i < enum_count - 1; i++) {
    Invariant(j <= i);
    if (enum_vals[i].value == enum_vals[i+1].value) {
      continue;
    }
    enum_vals[j++] = enum_vals[i];
  }

  // Now do a merge to find the differences
  // NOTE: we only need one index since we stop at the first difference

  Invariant(j < enum_count);
  enum_vals[j++] = enum_vals[i];
  enum_count = j;

  i = 0;
  while (i < case_count && i < enum_count) {
    if (case_vals[i].value < enum_vals[i].value) {
      CSTR errant = dup_printf("%lld", (llint_t)case_vals[i].value);
      report_error(case_vals[i].source, "CQL0388: a value exists in the switch that is not present in the enum", errant);
      record_error(expr);
      goto cleanup;
    }

    if (case_vals[i].value > enum_vals[i].value) {
      EXTRACT_STRING(enum_member, enum_vals[i].source->left);
      report_error(expr, "CQL0387: a value exists in the enum that is not present in the switch", enum_member);
      record_error(expr);
      goto cleanup;
    }
    i++;
  }

  // if either side has left over members that's an error

  if (i < case_count) {
    Invariant(i == enum_count);
    CSTR errant = dup_printf("%lld", (llint_t)case_vals[i].value);
    report_error(case_vals[i].source, "CQL0388: a value exists in the switch that is not present in the enum", errant);
    record_error(expr);
    goto cleanup;
  }

  if (i < enum_count) {
    Invariant(i == case_count);
    EXTRACT_STRING(enum_member, enum_vals[i].source->left);
    report_error(expr, "CQL0387: a value exists in the enum that is not present in the switch", enum_member);
    record_error(expr);
    goto cleanup;
  }

cleanup:
  BYTEBUF_CLEANUP(enum_buffer);
}

// Switch cases semantic analysis:
// * the case expressions must be constant expressions
// * the case expressions must promote to the type of the expression with no loss
// * the statement list must have no errors
// * the expressions can't be just "else..."
// * if all_values was specified you can't use else or it's a joke
static void sem_switch_cases(ast_node *ast, ast_node *expr, bool_t all_values) {
  Contract(is_ast_switch_case(ast));

  sem_t core_type = core_type_of(expr->sem->sem_type);
  bytebuf *case_buffer = _ast_pool_new(bytebuf);
  bytebuf_open(case_buffer);

  ast_node *head = ast;
  int32_t stmt_lists = 0;
  bool_t has_else = false;

  // We push a new branch group for two reasons:
  //
  // 1. It allows each branch of the SWITCH to be analyzed independently with
  //    respect to improvements.
  //
  // 2. If every branch makes the same improvement and an ELSE branch or ALL
  //    VALUES is present, we can retain the improvement after the SWITCH.
  FLOW_PUSH_CONTEXT_BRANCH_GROUP();

  while (ast) {
    EXTRACT_NOTNULL(connector, ast->left);
    EXTRACT(stmt_list, connector->right);

    bool_t branch_error = false;

    FLOW_PUSH_CONTEXT_BRANCH();

    // first check for expression list, this is a WHEN x,y,z THEN clause
    if (connector->left) {
      EXTRACT_NOTNULL(expr_list, connector->left);

      sem_switch_expr_list(expr_list, core_type, case_buffer);
      if (is_error(expr_list)) {
        record_error(head);
        branch_error = true;
      }
    }
    else {
      // no expr list corresponds to the else case
      Invariant(ast != head);  // 'else' is never first!
      Invariant(!ast->right);  // 'else' is always last!
      Invariant(stmt_list);    // 'else' always has a statement list

      has_else = true;

      if (all_values) {
        report_error(ast, "CQL0383: switch ... ALL VALUES is useless with an ELSE clause", NULL);
        record_error(head);
        branch_error = true;
      }
    }

    // no stmt list corresponds to WHEN ... THEN NOTHING
    if (!branch_error && stmt_list) {
      stmt_lists++;
      sem_stmt_list_in_current_flow_context(stmt_list);
      if (is_error(stmt_list)) {
        record_error(head);
        branch_error = true;
      }
    }

    FLOW_POP_CONTEXT_BRANCH();

    if (branch_error) {
      goto cleanup;
    }

    ast = ast->right;
  }

  if (stmt_lists == 0) {
    report_error(head, "CQL0384: switch statement did not have any actual statements in it", NULL);
    record_error(head);
    goto cleanup;
  }

  // check for duplicate cases in the case list

  size_t case_count = case_buffer->used / sizeof(case_val);
  case_val *case_vals = (case_val *)case_buffer->ptr;
  qsort(case_vals, case_count, sizeof(case_val), case_val_comparator);
  Invariant(case_count > 0);  // enforced by grammar

  for (int32_t i = 0; i < case_count - 1; i++) {
    if (case_vals[i].value == case_vals[i+1].value) {
      CSTR duplicate = dup_printf("%lld", (long long)case_vals[i].value);
      report_error(case_vals[i].source, "CQL0385: WHEN clauses contain duplicate values", duplicate);
      record_error(head);
      goto cleanup;
    }
  }

  if (all_values) {
    sem_check_all_values_condition(expr, case_buffer);
    if (is_error(expr)) {
      record_error(head);
      goto cleanup;
    }
    flow_set_context_branch_group_covers_all_cases(true);
  } else {
    flow_set_context_branch_group_covers_all_cases(has_else);
  }

  record_ok(head);

cleanup:
  FLOW_POP_CONTEXT_BRANCH_GROUP();
  BYTEBUF_CLEANUP(case_buffer);
}

// Switch statement semantic analysis:
// * the type of the switch expression must be integral (i.e. bool, integer, long_int)
// * the type must be not null
// * the case expressions must be constant expressions
// * the case expressions must promote to the type of the expression with no loss
// * the expressions can't be just "else..."
// NYI: If ALL VALUES is specified then:
//  * the type of switch expression must be an enum
//  * all the values in the enum must be covered by the switch
//  * if all_values was specified you can't use else or it's a joke
static void sem_switch_stmt(ast_node *ast) {
  Contract(is_ast_switch_stmt(ast));
  EXTRACT_OPTION(all_values, ast->left);
  EXTRACT_NOTNULL(switch_body, ast->right);
  EXTRACT_ANY_NOTNULL(expr, switch_body->left);
  EXTRACT_NOTNULL(switch_case, switch_body->right);

  // SWITCH [expr] [switch_body] END
  // SWITCH [expr] ALL VALUES [switch_body] END

  bool_t in_switch_saved = in_switch;
  in_switch = true;

  sem_root_expr(expr, SEM_EXPR_CONTEXT_NONE);
  if (is_error(expr)) {
    record_error(ast);
    goto cleanup;
  }

  sem_t core_type = core_type_of(expr->sem->sem_type);
  if (!is_integer(core_type) || is_nullable(expr->sem->sem_type)) {
    report_error(expr, "CQL0381: case expression must be a not-null integral type", NULL);
    record_error(ast);
    goto cleanup;
  }

  sem_switch_cases(switch_case, expr, !!all_values);
  if (is_error(switch_case)) {
    record_error(ast);
    goto cleanup;
  }

  record_ok(ast);

cleanup:
  in_switch = in_switch_saved;
}

// While semantic analysis is super simple.
//  * the condition must be numeric
//  * the statement list must be error-free
//  * loop_depth is increased allowing the use of interior leave/continue
static void sem_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);

  // WHILE [expr] BEGIN [stmt_list] END

  sem_numeric_expr(expr, ast, "WHILE", SEM_EXPR_CONTEXT_NONE);

  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  if (stmt_list) {
    loop_depth++;

    sem_stmt_list_within_loop(stmt_list, expr);

    loop_depth--;

    if (is_error(stmt_list)) {
      record_error(ast);
      return;
    }
  }

  record_ok(ast);
}

// Loop analysis is just as simple as "while" -- because the loop_stmt
// literally has an embedded fetch, you simply use the fetch helper to
// validate that the fetch is good and then visit the statement list.
// Loop depth is increased as with while.
static void sem_loop_stmt(ast_node *ast) {
  Contract(is_ast_loop_stmt(ast));
  EXTRACT_NOTNULL(fetch_stmt, ast->left);
  EXTRACT(stmt_list, ast->right);

  // LOOP [fetch_stmt] BEGIN [stmt_list] END

  sem_fetch_stmt(fetch_stmt);
  if (is_error(fetch_stmt)) {
    record_error(ast);
    return;
  }

  if (stmt_list) {
    loop_depth++;

    EXTRACT_ANY_NOTNULL(condition, fetch_stmt->left);
    sem_stmt_list_within_loop(stmt_list, condition);

    loop_depth--;

    if (is_error(stmt_list)) {
      record_error(ast);
      return;
    }
  }

  record_ok(ast);
}

static CSTR name_from_name_list_node(ast_node *ast) {
  Contract(is_ast_name_list(ast));
  EXTRACT_STRING(name, ast->left);
  return name;
}

static CSTR name_from_region_list_node(ast_node *ast) {
  Contract(is_ast_region_list(ast));
  EXTRACT_NOTNULL(region_spec, ast->left);
  EXTRACT_STRING(name, region_spec->left);
  return name;
}

typedef CSTR (*name_func)(ast_node *ast);

static bool_t sem_verify_no_duplicate_names_func(ast_node *list, name_func func) {
  Contract(list);
  Contract(func);

  // Walk starting from the current node looking for any duplicate name
  // later in the list.
  for (ast_node *a1 =list; a1; a1 = a1->right) {
    CSTR n1 = func(a1);
    Contract(n1);

    for (ast_node *a2 = a1->right; a2; a2 = a2->right) {
      CSTR n2 = func(a2);
      Contract(n2);

      if (!Strcasecmp(n1, n2)) {
        report_error(a2->left, "CQL0206: duplicate name in list", n2);
        return false;
      }
    }
  }
  return true;
}

// There are many cases where a list of names must have no duplicates;
// This helper walks the list and reports an error if there are two
// names that are case-insensitively the same.
static bool_t sem_verify_no_duplicate_names(ast_node *name_list) {
  Contract(is_ast_name_list(name_list));
  return sem_verify_no_duplicate_names_func(name_list, name_from_name_list_node);
}

// Just like the above except it's a region list (so there is an extra node in the AST)
static bool_t sem_verify_no_duplicate_regions(ast_node *region_list) {
  Contract(is_ast_region_list(region_list));
  return sem_verify_no_duplicate_names_func(region_list, name_from_region_list_node);
}

// Verifies that an expression passed as an OUT or INOUT argument for a call is
// the name of a valid variable. If it is not, reports an error indicating for
// which parameter such a name was expected.
static void sem_validate_arg_is_name_of_existing_variable(ast_node *arg, CSTR param_name) {
  if (!is_id_or_dot(arg)) {
    goto error;
  }

  EXTRACT_NAME_AND_SCOPE(arg);
  sem_t *type = NULL;
  sem_resolve_id_with_type(arg, name, scope, &type);
  if (is_error(arg)) {
    return;
  }

  if (!is_variable(arg->sem->sem_type)) {
    goto error;
  }

  // Check that our variable is not a cursor used as a boolean expression. We
  // must check the original binding `*type`, not `arg->sem->sem_type`, because
  // `SEM_TYPE_STRUCT` is only set on the original binding.
  if (!type || is_struct(*type)) {
    goto error;
  }

  return;

error:
  report_error(arg, "CQL0207: expected a variable name for OUT or INOUT argument", param_name);
  record_error(arg);
}


// Analyzes an argument passed for an OUT or INOUT parameter by verifying that
// it is a valid variable and that it has been initialized (if necessary), and
// then sets appropriate improvements.
static void sem_arg_for_out_param(ast_node *arg, ast_node *param) {
  Contract(arg);
  Contract(is_ast_param(param));

  sem_validate_arg_is_name_of_existing_variable(arg, param->sem->name);
  if (is_error(arg)) {
    return;
  }

  // Check to see if `arg` is being passed as an INOUT argument.
  if (is_in_parameter(param->sem->sem_type)) {
    // The caller is responsible for the initialization of arguments passed for
    // INOUT parameters. We already checked that we have a variable above, so
    // now we just need to verify that it has been appropriately initialized.
    sem_validate_variable_referenced_is_initialized_if_required(arg);
    if (is_error(arg)) {
      return;
    }
  }

  EXTRACT_NAME_AND_SCOPE(arg);

  // If `arg` is being passed for an OUT parameter of a nullable type, it could
  // be set to NULL; if it is being passed for an OUT parameter of a nonnull
  // type, then the name/scope pair must refer to a declared-nonnull type (which
  // can be neither improved nor unimproved), and so there is no harm in calling
  // this.
  sem_unset_notnull_improved(name, scope);

  // The callee will initialize the variable during its execution (unless the
  // callee throws, in which case all execution will either effectively stop
  // or a TRY block in the caller or further up the stack will safely bound
  // the initialization improvement).
  sem_set_initialization_improved(name, scope);

  ast_node *variable = find_local_or_global_variable(arg->sem->name);
  if (variable) {
    variable->sem->sem_type |= SEM_TYPE_WAS_SET;
  }
}

// Given an argument that (typically) has not yet been checked and a formal
// parameter that has been, check the argument and verify that it is allowed to
// be passed for that particular parameter.
static bool_t sem_validate_arg_vs_formal(ast_node *arg, ast_node *param) {
  Contract(arg);
  Contract(param);
  Contract(param->sem);

  sem_t sem_type_param = param->sem->sem_type;

  // As a first step, we check the argument itself.
  if (is_cursor_formal(sem_type_param)) {
    // a cursor arg demands a cursor expression, any such cursor will do
    sem_cursor(arg);
    if (is_error(arg)) {
      return false;
    }

    if (!is_auto_cursor(arg->sem->sem_type)) {
      report_error(arg, "CQL0067: cursor was not used with 'fetch [cursor]'", arg->sem->name);
      record_error(arg);
      return false;
    }

    ast_node *var = find_local_or_global_variable(arg->sem->name);
    Invariant(var); // we know the cursor exists and is unique already
    var->sem->sem_type |= SEM_TYPE_SERIALIZE;
    return true;
  } else if (is_out_parameter(sem_type_param)) {
    sem_arg_for_out_param(arg, param);
    if (is_error(arg)) {
      return false;
    }
  } else {
    // In the case of an IN-only parameter, we allow an expression.
    sem_arg_expr(arg, false);
    if (is_error(arg)) {
      return false;
    }
  }

  sem_t sem_type_arg = arg->sem->sem_type;

  // Now, we can check it against what was expected. Note that it's possible to
  // be both in and out in which case both validations have to happen.

  if (is_in_parameter(sem_type_param)) {
    // you have to be able to "assign" the arg to the param
    if (!sem_verify_assignment(arg, sem_type_param, sem_type_arg, param->sem->name)) {
      return false;
    }
  }

  // the formal and the argument must match object types as well (if present)
  sem_combine_kinds(arg, param->sem->kind);
  if (is_error(arg)) {
    return false;
  }

  if (is_out_parameter(sem_type_param)) {
    // We already checked this above when checking the arg itself.
    Invariant(is_variable(sem_type_arg));

    // you have to be able to "assign" the param to the arg (reverse of in)
    if (!sem_verify_assignment(arg, sem_type_arg, sem_type_param, arg->sem->name)) {
      return false;
    }

    if (core_type_of(sem_type_param) != core_type_of(sem_type_arg)) {
      CSTR error_message = "CQL0209: proc out parameter: arg must be an exact type match";
      report_sem_type_mismatch(sem_type_param, sem_type_arg, arg, error_message, arg->sem->name);
      return false;
    }

    if (is_nullable(sem_type_param) != is_nullable(sem_type_arg)) {
      CSTR error_message = "CQL0210: proc out parameter: arg must be an exact type match (even nullability)";
      report_sem_type_mismatch(sem_type_param, sem_type_arg, arg, error_message, arg->sem->name);
      return false;
    }
  }

  return true;
}

// Pointer tag indicating an id that is passed as an OUT or INOUT argument.
static const uintptr_t id_out_tag_bit = 0x1;

// Given a (possibly) tagged id, return an untagged, deferencable pointer.
static ast_node *id_from_out_tagged_id(ast_node *tagged_id) {
  return (ast_node *)((uintptr_t)tagged_id & ~id_out_tag_bit);
}

// Given an id, return the tagged version.
static ast_node *out_tagged_id_from_id(ast_node *id) {
 return (ast_node *)((uintptr_t)id | id_out_tag_bit);
}

// Returns true if `id` is tagged, else false.
static bool_t is_id_out_tagged(ast_node *id) {
  return !!((uintptr_t)id & id_out_tag_bit);
}

// Compares the names of two (possibly) tagged ids in a manner such that
// `out_tagged_id_comparator` can be used as a comparator for `qsort`. If the
// names match, we then compare by line number so that, if we later report an
// error, we can easily point to the first use.
static int out_tagged_id_comparator(const void *a, const void *b) {
  ast_node *id_a = id_from_out_tagged_id(*(ast_node **)a);
  ast_node *id_b = id_from_out_tagged_id(*(ast_node **)b);
  EXTRACT_STRING(a_name, id_a);
  EXTRACT_STRING(b_name, id_b);

  int result = strcmp(a_name, b_name);
  if (!result) {
    return id_a->lineno > id_b->lineno ? 1 : id_a->lineno < id_b->lineno ? -1 : 0;
  }

  return result;
}

// Returns true if all OUT and INOUT arguments in `arg_list` are unique with
// respect to all other arguments (including IN arguments), else false. If OUT
// and INOUT arguments were allowed to alias, setting a variable passed in via
// an OUT or INOUT parameter could cause the values of other parameters to be
// unexpectedly mutated.
static bool_t sem_validate_out_args_are_unique(ast_node *arg_list, ast_node *params) {
  Contract(!arg_list || is_ast_arg_list(arg_list) || is_ast_expr_list(arg_list));
  Contract(!params || is_ast_params(params));

  // Count up the ids passed as arguments so that we can allocate an array of
  // the correct size to hold all of them. As a minor optimization, we also
  // check whether we have at least one OUT or INOUT argument so we can bail out
  // early if we don't.
  //
  // NOTE: Technically, we only count ids with an associated parameter. If this
  // procedure is called after checking that the correct number of arguments
  // were provided, we'll always have enough parameters to count all of the ids
  // and will check all of the arguments for aliasing. If not, we'll check just
  // the arguments that have parameters and the caller can then fail due to
  // excess arguments later on. The caller decides which behavior it prefers.
  uint32_t ids_count = 0;
  bool_t has_out_argument = false;
  ast_node *arg_item = arg_list;
  ast_node *param_item = params;
  for (; arg_item && param_item; arg_item = arg_item->right, param_item = param_item->right) {
    EXTRACT_ANY_NOTNULL(arg, arg_item->left);
    if (!is_id(arg)) {
      continue;
    }
    ids_count++;
    EXTRACT_NOTNULL(param, param_item->left);
    if (is_out_parameter(param->sem->sem_type)) {
      has_out_argument = true;
    }
  }

  // If there isn't at least one OUT or INOUT argument, or if we don't have at
  // least two ids, there can be no aliasing.
  if (!has_out_argument || ids_count < 2) {
    return true;
  }

  // Put all of the ids into an array, tagging the ones that were used for OUT
  // or INOUT arguments. We do this because the arguments themselves do not
  // contain whether or not they were used as OUT or INOUT arguments in their
  // sem nodes: That information is only present in the associated parameter.
  // Tagging ids gives us an easy way to track OUT/INOUT usage without having to
  // later unset anything on the ids themselves.
  ast_node **ids = _ast_pool_new_array(ast_node *, ids_count);
  ast_node **ids_ptr = ids;
  arg_item = arg_list;
  param_item = params;
  for (; arg_item && param_item; arg_item = arg_item->right, param_item = param_item->right) {
    EXTRACT_ANY_NOTNULL(arg, arg_item->left);
    if (!is_id(arg)) {
      continue;
    }
    EXTRACT_NOTNULL(param, param_item->left);
    *ids_ptr++ = is_out_parameter(param->sem->sem_type) ? out_tagged_id_from_id(arg) : arg;
  }

  // Sort them by name, then by line number.
  qsort(ids, ids_count, sizeof(ast_node *), out_tagged_id_comparator);

  // Look for duplicates involving an OUT or INOUT usage.
  for (uint32_t i = 1; i < ids_count; i++) {
    // If either the current id or previous id is tagged, and if their names
    // match, it must be the case that an OUT or INOUT argument is aliased.
    if (is_id_out_tagged(ids[i - 1]) || is_id_out_tagged(ids[i])) {
      ast_node *previous_id = id_from_out_tagged_id(ids[i - 1]);
      ast_node *id = id_from_out_tagged_id(ids[i]);
      EXTRACT_STRING(previous_id_name, previous_id);
      EXTRACT_STRING(id_name, id);
      if (!strcmp(previous_id_name, id_name)) {
        // We sorted by name and then by line number, so `previous_id` is
        // guaranteed to contain the earliest line number we could report.
        CSTR msg = "CQL0426: OUT or INOUT argument cannot be used again in same call";
        report_error(previous_id, msg, id_name);
        return false;
      }
    }
  }

  return true;
}

// This is the core helper method for procedure calls and function calls.
// It validates that the type and number of arguments are compatible for the
// call in question. When we get here, we typically have a list of unchecked
// arguments in arg_list and the formals to verify against in params. (In the
// case of recursive expressions, arg_list may have already been checked.)
// Errors will be recorded on the given ast.  Since the shape of the tree
// varies slightly between function and procedure calls, this helper expects to
// have the items harvested and ready to go.
//
// Semantic rules:
//  * for all cases each argument must be error-free (no internal type
//    conflicts)
//  * for known procs
//    * the call has to have the correct number of arguments
//    * if the formal is an out parameter the argument must be a variable
//      * the type of the variable must be an exact type match for the formal
//    * non-out parameters must be type-compatible, but exact match is not
//      required
static void sem_validate_args_vs_formals(ast_node *ast, CSTR name, ast_node *arg_list, ast_node *params, bool_t proc_as_func) {
  ast_node *arg_item = arg_list;
  ast_node *param_item = params;

  // First, we check the arguments themselves.
  for (; arg_item && param_item; arg_item = arg_item->right, param_item = param_item->right) {
    EXTRACT_ANY_NOTNULL(arg, arg_item->left);
    EXTRACT_NOTNULL(param, param_item->left);

    if (!sem_validate_arg_vs_formal(arg, param)) {
      record_error(ast);
      return;
    }

    // The proc-as-func case is only allowed for procedures with zero or more
    // IN parameters followed by exactly one OUT (but not INOUT) parameter.
    if (proc_as_func && is_out_parameter(param->sem->sem_type)) {
      if (is_in_parameter(param->sem->sem_type)) {
        report_error(ast, "CQL0424: procedure with INOUT parameter used as function", name);
        record_error(ast);
        return;
      }
      if (param_item->right) {
        report_error(ast, "CQL0425: procedure with non-trailing OUT parameter used as function", name);
        record_error(ast);
        return;
      }
    }
  }

  // If we used up all the args and it's a proc as func case then we have one
  // last chance to be correct, there has to be exactly one out argument left
  // we'll treat that as the virtual return.
  if (proc_as_func && !arg_item && param_item && !param_item->right) {
    EXTRACT_NOTNULL(param, param_item->left);

    Invariant(param->sem);
    Invariant(is_unitary(param->sem->sem_type)); // params can't be structs or cursors

    sem_t sem_type_param = param->sem->sem_type;

    if (!is_out_parameter(sem_type_param) || is_in_parameter(sem_type_param)) {
      report_error(param, "CQL0211: procedure without trailing OUT parameter used as function", name);
      record_error(ast);
      return;
    }

    ast->sem = new_sem(core_type_of(sem_type_param) | not_nullable_flag(sem_type_param) | sensitive_flag(sem_type_param));
    return;
  }

  if (param_item) {
    report_error(ast, "CQL0212: too few arguments provided to procedure", name);
    record_error(ast);
    return;
  }

  // if any args are left that's an error
  // if items matches and it's proc as func then the last arg was provided, that's also an error
  if (arg_item || proc_as_func) {
    report_error(ast, "CQL0235: too many arguments provided to procedure", name);
    record_error(ast);
    return;
  }

  // Finally, we check whether or not any OUT or INOUT arguments are aliased.
  if (!sem_validate_out_args_are_unique(arg_list, params)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// This is the sematic analysis for a call statement.  There are three ways
// that a call can happen:
//   * signatures of procedures that we know in full:
//     * call foo();
//     * declare cursor for call foo();
//   * some external call to some outside function we don't know
//     * e.g. call printf('hello, world\n');
//
// The cursor form can be used if and only if the procedure has a loose select
// or a call to a procedure with a loose select. In that case the procedure will
// have a structure type, rather than just "ok" (the normal signature for a proc).
// If the user is attempting to do the second case, cursor_name will be set and
// the appropriate verification happens here.
//
// Note:  Recursively calling fetch cursor is not really doable in general
// because at the point of the call we might not yet know that the method
// does in fact return a select.  You could make it work if you put the select
// before the recursive call.
//
// Semantic rules:
//  * for all cases each argument must be error-free (no internal type conflicts)
//  * for known procs
//    * the call has to have the correct number of arguments
//    * if the formal is an out parameter the argument must be a variable
//      * the type of the variable must be an exact type match for the formal
//    * non-out parameters must be type-compatible, but exact match is not required
static void sem_call_stmt_opt_cursor(ast_node *ast, CSTR cursor_name) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT(expr_list, ast->right);
  EXTRACT_STRING(name, name_ast);

  ast_node *proc_stmt = find_proc(name);

  if (!proc_stmt && !find_unchecked_proc(name)) {
    report_error(ast, "CQL0323: calls to undeclared procedures are forbidden; declaration missing or typo", name);
    record_error(ast);
    return;
  }

  // Not found is not an error in this case (is_error will return on null)
  if (proc_stmt && is_error(proc_stmt)) {
    report_error(ast, "CQL0213: procedure had errors, can't call", name);
    record_error(ast);
    return;
  }

  if (proc_stmt &&
    is_ast_create_proc_stmt(proc_stmt) &&
    find_proc_frag_type(proc_stmt) == FRAG_TYPE_SHARED &&
    !is_ast_shared_cte(ast->parent)) {
      report_error(ast, "CQL0434: shared fragments may not be called outside of a SQL statement", name);
      record_error(ast);
      return;
  }

  if (proc_stmt && is_struct(proc_stmt->sem->sem_type)) {
    if (!cursor_name && !current_proc && !in_shared_fragment_call) {
      report_error(ast, "CQL0214: procedures with results can only be called using a cursor in global context", name);
      record_error(ast);
      return;
    }
  }

  // expand any FROM forms in the arg list
  if (!rewrite_shape_forms_in_list_if_needed(expr_list)) {
    record_error(ast);
    return;
  }

  record_ok(name_ast);

  // If known proc, do additional validation
  if (proc_stmt) {
    Contract(is_proc(proc_stmt));
    EXTRACT_NOTNULL(proc_params_stmts, proc_stmt->right);
    EXTRACT(params, proc_params_stmts->left);

    name_ast->sem = proc_stmt->sem;

    has_dml |= is_dml_proc(proc_stmt->sem->sem_type);

    sem_validate_args_vs_formals(ast, name, expr_list, params, NORMAL_CALL);

    // The call may have mutated any or all of the currently improved globals,
    // so we simply invalidate all of them. We do this before returning in the
    // case of an error so that subsequently checked statements have a slightly
    // more accurate view of what can no longer be considered improved.
    sem_unset_global_notnull_improvements();

    if (is_error(ast)) {
      return;
    }
  } else {
    // compute semantic type of each arg, reporting errors
    sem_validate_args(ast, expr_list);
    if (is_error(ast)) {
      return;
    }
  }

  ast->sem = name_ast->sem;
}

// The fetch statement has two forms:
//   * fetch C into var1, var2, var3 etc.
//   * fetch C;
// The second form is called the auto_cursor.
// In the first form the variables of the cursor must be assignment compatible
// with declared structure type of the cursor and the count must be correct.
// In the second form, the codegen will implicitly create local variables that
// are exactly the correct type, but that's later.  Since no semantic error is
// possible in that case we simply record that this is an auto_cursor and then
// later we will allow the use of C.field during analysis.
// Of course "C" must be a valid cursor.
static void sem_fetch_stmt(ast_node *ast) {
  Contract(is_ast_fetch_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);
  EXTRACT(name_list, ast->right);

  // FETCH [name] [ INTO [name_list] ]

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  if (cursor->sem->sem_type & SEM_TYPE_VALUE_CURSOR) {
    report_error(cursor, "CQL0215: value cursors are not used with FETCH C, or FETCH C INTO", cursor->sem->name);
    record_error(ast);
    return;
  }

  if (!name_list) {
    sem_validate_unique_names_struct_type(cursor);
    if (is_error(cursor)) {
      record_error(ast);
      return;
    }

    sem_verify_no_anon_no_null_columns(cursor);
    if (is_error(cursor)) {
      record_error(ast);
      return;
    }

    // Tag the cursor *variable* (i.e. the AST from the original definition site
    // of the cursor) as an auto cursor. This is necessary because we need to
    // have this information available during codegen before we see that the
    // cursor was used in a fetch.  So we leave this breadcrumb.
    ast_node *cursor_var = find_local_or_global_variable(cursor->sem->name);
    Invariant(cursor_var);
    Invariant(is_cursor(cursor_var->sem->sem_type));
    sem_add_flags(cursor_var, SEM_TYPE_HAS_SHAPE_STORAGE);

    // We also tag the cursor in `ast`, both for clarity (i.e. so we can see
    // that the cursor has the auto_cursor flag set in tests) and because
    // codegen will look for the flag on `ast` itself (which gets it from the
    // following assignment).
    sem_add_flags(cursor, SEM_TYPE_HAS_SHAPE_STORAGE);
    ast->sem = cursor->sem;

    // Remove nullability improvements from all of the fields.
    sem_struct *sptr = cursor_var->sem->sptr;
    for (uint32_t i = 0; i < sptr->count; i++) {
      sem_unset_notnull_improved(sptr->names[i], cursor->sem->name);
    }

    // The "FETCH c" form is not guaranteed to give us a row.
    sem_unset_has_row_improved(cursor->sem->name);

    return;
  }

  if (!sem_verify_no_duplicate_names(name_list)) {
    record_error(ast);
    return;
  }

  // ensure the types match

  uint32_t icol = 0;
  uint32_t cols = cursor->sem->sptr->count;
  ast_node *item = name_list;
  for (item = name_list; item && icol < cols; item = item->right, icol++) {
    EXTRACT_ANY_NOTNULL(var_name_ast, item->left);
    EXTRACT_STRING(name, var_name_ast);

    ast_node *variable = find_local_or_global_variable(name);
    if (!variable) {
      report_error(var_name_ast, "CQL0216: FETCH variable not found", name);
      record_error(ast);
      return;
    }

    var_name_ast->sem = variable->sem;

    sem_t sem_type_cursor = cursor->sem->sptr->semtypes[icol];
    sem_t sem_type_variable = variable->sem->sem_type;

    // there are no object columns and therefore no object cursors, any such creature was long ago eliminated
    Invariant(!is_object(sem_type_cursor));

    if (!sem_verify_assignment(var_name_ast, sem_type_variable, sem_type_cursor, name)) {
      record_error(ast);
      return;
    }

    // For the time being, unlike SET, we do not allow FETCH INTO to both unset
    // and set nullability improvements because we do not yet have a way of
    // enforcing that the programmer verified that the fetch itself was
    // successful. If it did set nullability improvements, code like the
    // following would erroneously report a redundant IS NOT NULL check which
    // might then encourage the programmer to remove the IS NOT NULL check
    // rather than add the appropriate has-row check:
    //
    //   DECLARE x TEXT;
    //   FETCH cursor_with_text_notnull_column INTO x;
    //   -- an error would be issued here due to IS NOT NULL, but x CAN be null!
    //   IF x IS NOT NULL THEN
    //     ...
    //   END IF;
    //
    // This will be revisited once CQL has added a notion of has-row
    // improvements for cursors.
    sem_unset_notnull_improved(name, NULL);

    // Even though the fetch may have failed, we optimistically consider the
    // variable to have been initialized. In essence, this is no different from
    // fetching into an auto cursor and then using SET to initialize a variable
    // by setting it to the value of one of the cursor's fields (even though the
    // fetch could have failed), which we also allow. Again, this will be
    // revisited in the future once has-row improvements have been added.
    sem_set_initialization_improved(name, NULL);

    variable->sem->sem_type |= SEM_TYPE_WAS_SET;
  }

  if (icol != cols || item) {
    report_error(ast, "CQL0217: number of variables did not match count of columns in cursor", cursor->sem->name);
    record_error(ast);
    return;
  }

  // Tag the cursor *variable* (i.e. the AST from the original definition site
  // of the cursor) with FETCH_INTO. This is necessary because we need to
  // have this information available during codegen before we see that the
  // cursor was used in a fetch.  So we leave this breadcrumb.

  ast_node *cursor_var = find_local_or_global_variable(cursor->sem->name);
  Invariant(cursor_var);
  Invariant(is_cursor(cursor_var->sem->sem_type));
  sem_add_flags(cursor_var, SEM_TYPE_FETCH_INTO);

  ast->sem = cursor_var->sem;
}

// In this form we're working on a cursor that is going to be loaded by making a call.  This call statement
// must be using the OUT statement and its OUT value must exactly match the shape of the target cursor.
static void sem_fetch_call_stmt(ast_node *ast) {
  Contract(is_ast_fetch_call_stmt(ast));
  Contract(is_ast_call_stmt(ast->right));
  EXTRACT_ANY_NOTNULL(cursor, ast->left)
  EXTRACT_STRING(cursor_name, cursor);
  EXTRACT_ANY_NOTNULL(call_stmt, ast->right);

  // FETCH [cursor] FROM CALL [call_stnmt]

  sem_call_stmt_opt_cursor(call_stmt, cursor_name);
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  sem_call_stmt_has_out_stmt_result_for_cursor(call_stmt, cursor_name);
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  sem_verify_identical_columns(cursor, call_stmt, "receiving cursor from call");
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  // The "FROM CALL" form is not guaranteed to give us a row.
  sem_unset_has_row_improved(cursor_name);

  record_ok(ast);
}

// Fetch the next statement assuming we're on a statement
// this is only tricky because the parent node might not be a statement
// list due to the way attributes on statements work.
static ast_node *get_next_stmt(ast_node *ast) {
  ast_node *parent = ast->parent;

  if (is_ast_stmt_list(parent)) {
    return parent->right;
  }

  EXTRACT_NOTNULL(stmt_and_attr, parent);
  EXTRACT_NOTNULL(stmt_list, stmt_and_attr->parent);
  return stmt_list->right;
}

// Some of the control flow statements like LEAVE, CONTINUE, and RETURN should have nothing
// after them.  This handles those cases in a uniform way.
static void sem_last_statement_in_block(ast_node *ast) {
  if (get_next_stmt(ast)) {
    report_error(ast, "CQL0308: statement should be the last thing in a statement list", NULL);
    record_error(ast);
  }
  else {
    record_ok(ast);
  }
}

// We just need to ensure that continue is inside a loop.
static void sem_continue_stmt(ast_node *ast) {
  Contract(is_ast_continue_stmt(ast));

  // CONTINUE
  if (loop_depth == 0) {
    report_error(ast, "CQL0218: continue must be inside of a 'loop' or 'while' statement", NULL);
    record_error(ast);
    return;
  }

  sem_last_statement_in_block(ast);
}

// We just need to ensure that leave is inside a loop.
static void sem_leave_stmt(ast_node *ast) {
  Contract(is_ast_leave_stmt(ast));

  // LEAVE
  if (loop_depth == 0 && !in_switch) {
    report_error(ast, "CQL0219: leave must be inside of a 'loop', 'while', or 'switch' statement", NULL);
    record_error(ast);
    return;
  }

  sem_last_statement_in_block(ast);
}

// Return should not appear at the top level, it's redundant.  It also should be
// the last thing in a statement block.
static void sem_return_common(ast_node *ast) {
  Contract(is_ast_return_stmt(ast) || is_ast_rollback_return_stmt(ast) || is_ast_commit_return_stmt(ast));

  // RETURN
  if (sem_stmt_level <= 1) {
    report_error(ast, "CQL0307: return statement should be in a procedure and not at the top level", NULL);
    record_error(ast);
    return;
  }

  // for sure in a statement now due to the above
  Invariant(current_proc);

  // Since this is a potential exit point of the current procedure, all
  // parameters requiring initialization should have been initialized by now.
  if (!sem_validate_current_proc_params_are_initialized(ast)) {
    record_error(ast);
    return;
  }

  sem_last_statement_in_block(ast);
}

// The usual return rules plus a return statement may not appear inside of a proc savepoint
// you have to use either rollback or commit return.
static void sem_return_stmt(ast_node *ast) {
  if (in_proc_savepoint) {
    report_error(ast, "CQL0352: use COMMIT RETURN or ROLLBACK RETURN in within a proc savepoint block", NULL);
    record_error(ast);
    return;
  }
  sem_return_common(ast);
}

// Must be inside of a proc savepoint plus the usual return rules
static void sem_commit_return_stmt(ast_node *ast) {
  Contract(is_ast_commit_return_stmt(ast));

  if (!in_proc_savepoint) {
    report_error(ast, "CQL0350: statement must appear inside of a PROC SAVEPOINT block", NULL);
    record_error(ast);
    return;
  }

  // and the usual return rules
  sem_return_common(ast);
}

// Must be inside of a proc savepoint plus the usual return rules
static void sem_rollback_return_stmt(ast_node *ast) {
  Contract(is_ast_rollback_return_stmt(ast));

  if (!in_proc_savepoint) {
    report_error(ast, "CQL0350: statement must appear inside of a PROC SAVEPOINT block", NULL);
    record_error(ast);
    return;
  }

  // and the usual return rules
  sem_return_common(ast);
}

// The rules here:
//  * it must be in a procedure
//  * it must be at the top level
static void sem_proc_savepoint_stmt(ast_node *ast)
{
  Contract(is_ast_proc_savepoint_stmt(ast));
  EXTRACT(stmt_list, ast->left);

  if (!current_proc || sem_stmt_level != 1 ) {
    report_error(ast, "CQL0351: statement should be in a procedure and at the top level", NULL);
    record_error(ast);
    return;
  }

  Invariant(!in_proc_savepoint);

  if (stmt_list) {
   in_proc_savepoint = true;
   // We avoid making a new context here so that any improvements made within
   // the statement list will persist after the end of the PROC SAVEPOINT
   // statement. This is safe because we enter it unconditionally and because
   // the only way to jump out early is to jump out of the entire procedure.
   sem_stmt_list_in_current_flow_context(stmt_list);
   in_proc_savepoint = false;

   if (is_error(stmt_list)) {
     record_error(ast);
     return;
    }
  }

  record_ok(ast);
}

// If @attribute(cql:try_is_proc_body) is present, performs additional analysis
// using the try/catch AST provided as `context` such that the statement list of
// the TRY is treated as though it were the main body of the procedure. In
// particular, it ensures that all parameters of the current procedure have been
// initialized by the end of the TRY and prevents `sem_inside_create_proc_stmt`
// from later doing the same at the end of the procedure.
//
// The reason why @attribute(cql:try_is_proc_body) is needed is that users, for
// various reasons, sometimes need to wrap certain stored procedures in a
// try/catch such that custom error handling or logging can be implemented. In
// doing so, however, they break our assumptions about things like
// initialization of OUT parameters: We normally enforce that parameters must be
// initialized by the end of a procedure, but, if the procedure is wrapped in a
// try/catch so that the CATCH can help perform some custom error reporting
// (e.g., log the error and then rethrow the exception), any initialization
// improvements made in the TRY will be unset at the end of the procedure.
//
// A somewhat contrived example use case for this is as follows:
//
//   #define LOGGING_PROC_BEGIN \
//     BEGIN \
//       LET error_in_try := FALSE; \
//       @attribute(cql:try_is_proc_body) \
//       BEGIN TRY
//
//   #define LOGGING_PROC_END \
//       END TRY; \
//       BEGIN CATCH \
//         SET error_in_try := TRUE; \
//       END CATCH; \
//       IF error_in_try THEN \
//         CALL some_proc_that_logs_and_rethrows(__FILE__, __LINE__); \
//       END IF; \
//     END
//
//   CREATE PROC some_proc(OUT x TEXT NOT NULL)
//   LOGGING_PROC_BEGIN
//     IF some_condition THEN
//       SET x := some_value;
//     ELSE
//       SET x := get_another_value_or_throw()
//     END IF;
//   LOGGING_PROC_END;
//
// As can be seen above, the main part of the procedure does, in fact, always
// initialize x unless an exception occurs -- and, if it does, the handling
// within LOGGING_PROC_END will take care of it. Our normal analyses cannot
// understand that though. By giving programmers a way to explicitly indicate
// that this pattern is in effect, we can know to ensure that x is initialized
// within what is, conceptually, the main body of the procedure (i.e., the TRY)
// and then not worry about it later on.
//
// NOTE: It is very possible to misuse @attribute(cql:try_is_proc_body) such
// that parameter initialization checking becomes useless. There is nothing we
// can do about that here: We must simply assume the programmer has used it
// appropriately.
void sem_find_ast_misc_attr_trycatch_is_proc_body_callback(
  CSTR _Nullable misc_attr_prefix,
  CSTR _Nonnull misc_attr_name,
  ast_node *_Nullable ast_misc_attr_value_list,
  void *_Nullable context)
{
  Contract(misc_attr_name);
  Contract(is_ast_trycatch_stmt(context));

  if (!misc_attr_prefix || Strcasecmp(misc_attr_prefix, "cql") || Strcasecmp(misc_attr_name, "try_is_proc_body")) {
    return;
  }

  ast_node *ast = context;

  if (ast_misc_attr_value_list) {
    report_error(ast_misc_attr_value_list, "CQL0445: @attribute(cql:try_is_proc_body) accepts no values", NULL);
    record_error(ast);
    return;
  }

  if (current_proc_contains_try_is_proc_body) {
    report_error(
      context,
      "CQL0446: @attribute(cql:try_is_proc_body) cannot be used more than once per procedure",
      NULL
    );
    record_error(ast);
    return;
  }

  // Set this so `sem_inside_create_proc_stmt` knows not to perform the
  // initialization check later on.
  current_proc_contains_try_is_proc_body = true;

  // Use the end of the TRY block for error reporting if it has any statements,
  // else just use the whole block since we have nothing better.
  EXTRACT_NAMED(try_list, stmt_list, ast->left);
  if (!sem_validate_current_proc_params_are_initialized(try_list ? try_list : ast)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// No analysis needed here other than that the two statement lists are ok.
static void sem_trycatch_stmt(ast_node *ast) {
  Contract(is_ast_trycatch_stmt(ast));
  EXTRACT_NAMED(try_list, stmt_list, ast->left);
  EXTRACT_NAMED(catch_list, stmt_list, ast->right);

  bool_t error = false;

  // We assume any statement within the TRY can throw. Using a jump context
  // keeps things safe in the presence of code like the following:
  //
  //   DECLARE x INT;
  //   SET x := 42;
  //   BEGIN TRY
  //     IF some_condition THEN
  //       SET x := NULL;
  //       IF another_condition THEN
  //         THROW;
  //       END IF;
  //       SET x := 100; -- may never happen
  //     ELSE
  //       -- do nothing; neutral for x
  //     END IF;
  //     -- x is still nonnull here as the outer IF was neutral for x
  //   END TRY;
  //   BEGIN CATCH
  //     -- x must be nullable here as the final SET may have not occurred
  //   END CATCH;
  //   -- x must also be nullable here
  //
  // If we did not use a jump context, x would be nonnull after the TRY because
  // the set to NULL was neutralized by the subsequent set to 100 in the same
  // branch.
  FLOW_PUSH_CONTEXT_JUMP();

  if (try_list) {
    sem_stmt_list_in_current_flow_context(try_list);
    error = is_error(try_list);
  }

  if (!error) {
    EXTRACT_MISC_ATTRS(ast, misc_attrs);
    if (misc_attrs) {
      // If the "cql:try_is_proc_body" attribute is set, we need to check it as
      // though it were the true body of the procedure.
      find_misc_attrs(misc_attrs, sem_find_ast_misc_attr_trycatch_is_proc_body_callback, ast);
      error = is_error(ast);
    }
  }

  FLOW_POP_CONTEXT_JUMP();

  if (!error && catch_list) {
    sem_stmt_list(catch_list);
    error = is_error(catch_list);
  }

  if (error) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Throw can literally go anywhere, so it's ok.
static void sem_throw_stmt(ast_node *ast) {
  Contract(is_ast_throw_stmt(ast));

  // "throw" implies that we have a return code which implies all of the proc
  // things as surely as if we had used the database.  We need to be a proc
  // with a result code.
  has_dml = 1;

  // ok to throw at the end of any block
  sem_last_statement_in_block(ast);
}

static void sem_verify_transaction_ok(ast_node *ast) {
  if (enforcement.strict_transaction) {
    report_error(ast, "CQL0366: transaction operations disallowed while STRICT TRANSACTION enforcement is on.", NULL);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// Begin trans can go anywhere, it's ok.
static void sem_begin_trans_stmt(ast_node *ast) {
  Contract(is_ast_begin_trans_stmt(ast));
  sem_verify_transaction_ok(ast);
}

// Commit trans can go anywhere, it's ok.
static void sem_commit_trans_stmt(ast_node *ast) {
  Contract(is_ast_commit_trans_stmt(ast));
  sem_verify_transaction_ok(ast);
}

// Rollback trans can go anywhere but if you're using the format
// where you rollback to a particular save point then we must have
// seen that name in a savepoint statement or it's an error.
static void sem_rollback_trans_stmt(ast_node *ast) {
  Contract(is_ast_rollback_trans_stmt(ast));

  if (!ast->left) {
    sem_verify_transaction_ok(ast);
    return;
  }

  rewrite_proclit(ast->left);
  if (is_error(ast->left)) {
    record_error(ast);
    return;
  }

  EXTRACT_STRING(name, ast->left);
  if (!symtab_find(savepoints, name)) {
    report_error(ast, "CQL0220: savepoint has not been mentioned yet, probably wrong", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// The savepoint statement can go anywhere but we do record this savepoint name
// as having been seen so we can verify it in rollback.
static void sem_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_savepoint_stmt(ast));
  rewrite_proclit(ast->left);
  if (is_error(ast->left)) {
    record_error(ast);
    return;
  }
  EXTRACT_STRING(name, ast->left);

  // these don't have lexical semantics but at least we can verify that
  // you don't try to release or rollback to a savepoint we've never seen before
  symtab_add(savepoints, name, ast);  // if already exits, no problem.

  record_ok(ast);
}

// Release savepoint can go anywhere but we must have
// seen that name in a savepoint statement or it's an error.
static void sem_release_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_release_savepoint_stmt(ast));
  rewrite_proclit(ast->left);
  if (is_error(ast->left)) {
    record_error(ast);
    return;
  }
  EXTRACT_STRING(name, ast->left);

  if (!symtab_find(savepoints, name)) {
    report_error(ast, "CQL0221: savepoint has not been mentioned yet, probably wrong", name);
    record_error(ast);
    return;
  }

  record_ok(ast);
}

// For close [cursor], we just validate that the name is in fact a cursor.
static void sem_close_stmt(ast_node *ast) {
  Contract(is_ast_close_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);

  // CLOSE [name]

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  if (cursor->sem->sem_type & SEM_TYPE_BOXED) {
    EXTRACT_STRING(name, cursor);
    report_error(ast, "CQL0391: CLOSE cannot be used on a boxed cursor", name);
    record_error(ast);
    return;
  }

  ast->sem = cursor->sem;
}

// For out [cursor], we first validate that the name is a cursor
// then we set the output type of the procedure we're in accordingly
static void sem_out_any(ast_node *ast) {
  Contract(is_ast_out_stmt(ast) || is_ast_out_union_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor, ast->left);

  if (!current_proc) {
    report_error(ast, "CQL0222: out cursor statement only makes sense inside of a procedure", NULL);
    record_error(ast);
    return;
  }

  // OUT [name]

  sem_cursor(cursor);
  if (is_error(cursor)) {
    record_error(ast);
    return;
  }

  if (!is_auto_cursor(cursor->sem->sem_type)) {
    report_error(ast, "CQL0223: cursor was not fetched with the auto-fetch syntax 'fetch [cursor]'", cursor->sem->name);
    record_error(ast);
    return;
  }

  ast->sem = cursor->sem;
  sem_update_proc_type_for_select(ast);
}

static void sem_out_stmt(ast_node *ast) {
  Contract(is_ast_out_stmt(ast));
  sem_out_any(ast);
}

static void sem_out_union_stmt(ast_node *ast) {
  Contract(is_ast_out_union_stmt(ast));
  sem_out_any(ast);
}

static void sem_out_union_parent_child_stmt(ast_node *ast) {
  Contract(is_ast_out_union_parent_child_stmt(ast));
  rewrite_out_union_parent_child_stmt(ast);

  // analyze the first statement of the rewrite
  // the rest of the rewrite will proceed normally as we march through the statement list
  sem_one_stmt(ast);
}

// echo is valid in any context
static void sem_echo_stmt(ast_node *ast) {
  Contract(is_ast_echo_stmt(ast));
  EXTRACT_STRING(str, ast->right);

  record_ok(ast);
}

static void sem_previous_schema_stmt(ast_node *ast) {
  Contract(is_ast_previous_schema_stmt(ast));

  sem_setup_region_filters();

  if (current_proc) {
    report_error(ast, "CQL0225: switching to previous schema validation mode must be outside of any proc", NULL);
    record_error(ast);
    return;
  }

  if (schema_upgrade_version > 0) {
    report_error(ast, "CQL0254: switching to previous schema validation mode not allowed if @schema_upgrade_version was used", NULL);
    record_error(ast);
    return;
  }

  validating_previous_schema = true;
  reset_enforcements();
  enforcement.strict_cast = false;  // this is normally on by default, we want no strict in previous schema

  // we're entering the previous schema section, the regions will be redeclared.
  // later we'll want to validate against these;  we have to save the current regions
  // and begin fresh or there will be bogus duplicate region declaration warnings.
  // see the processing in sem_declare_schema_region_stmt which shows how regions
  // are different than other entities.  This "duplicate" business is handled differently
  // for regions.
  new_regions = schema_regions;
  new_enums = enums;

  // this is all it takes to start fresh...
  schema_regions = symtab_new();
  enums = symtab_new();

  deployable_validations = _ast_pool_new(bytebuf);
  bytebuf_open(deployable_validations);

  reverse_list(&all_subscriptions_list);
  next_subscription = all_subscriptions_list;
  found_subscription_error = false;

  record_ok(ast);
}

// When upgrading the DDL it's necessary to emit create table statements
// for the original version of the schema.  These create statements conflict
// with the current version of the schema.  This attribute tells CQL to
// 1) ignore DDL in stored procs for declaration purposes; only DDL outside of a proc counts
// 2) do not make any columns "deleted" thereby allowing all annotations to be present
//    so they can be used to validate other aspects of the migration script.
static void sem_schema_upgrade_script_stmt(ast_node *ast) {
  Contract(is_ast_schema_upgrade_script_stmt(ast));

  if (current_proc) {
    report_error(ast, "CQL0226: schema upgrade declaration must be outside of any proc", NULL);
    record_error(ast);
    return;
  }

  if (tables->count) {
    report_error(ast, "CQL0227: schema upgrade declaration must come before any tables are declared", NULL);
    record_error(ast);
    return;
  }

  schema_upgrade_script = 1;
  record_ok(ast);
}

// For sql stored procs that are supposed to update previous schema versions
// you can use this attribute to put CQL into that mindset.  This will make
// the columns deleted for the version in question rather than the current version.
// This is important because older schema migration procs might still refer to
// old columns.  Those columns truly exist at that schema version.
static void sem_schema_upgrade_version_stmt(ast_node *ast) {
  Contract(is_ast_schema_upgrade_version_stmt(ast));
  EXTRACT_OPTION(vers, ast->left);

  if (vers <= 0) {
    report_error(ast, "CQL0228: schema upgrade version must be a positive integer", NULL);
    record_error(ast);
    return;
  }

  if (schema_upgrade_version > 0) {
    report_error(ast, "CQL0229: schema upgrade version declaration may only appear once", NULL);
    record_error(ast);
    return;
  }

  if (current_proc) {
    report_error(ast, "CQL0230: schema upgrade version declaration must be outside of any proc", NULL);
    record_error(ast);
    return;
  }

  if (tables->count) {
    report_error(ast, "CQL0231: schema upgrade version declaration must come before any tables are declared", NULL);
    record_error(ast);
    return;
  }

  schema_upgrade_version = vers;

  record_ok(ast);
}

// forward to the more general version with no cursor
static void sem_call_stmt(ast_node *ast) {
  sem_call_stmt_opt_cursor(ast, NULL);
  if (is_struct(ast->sem->sem_type)) {
    sem_update_proc_type_for_select(ast);
  }
}

static void sem_declare_out_call_stmt(ast_node *ast) {
  Contract(is_ast_declare_out_call_stmt(ast));
  EXTRACT_NOTNULL(call_stmt, ast->left);

  EXTRACT_ANY_NOTNULL(name_ast, call_stmt->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT(expr_list, call_stmt->right);

  ast_node *proc_stmt = find_proc(name);

  if (!proc_stmt) {
    report_error(ast, "CQL0389: DECLARE OUT requires that the procedure be already declared", name);
    record_error(ast);
    return;
  }

  // The semantic info for the proc is useless if it had errors, can't use it
  if (is_error(proc_stmt)) {
    report_error(ast, "CQL0213: procedure had errors, can't call", name);
    record_error(ast);
    return;
  }

  ast_node *params = get_proc_params(proc_stmt);

  int32_t out_args = 0;

  for (; params && expr_list; params = params->right, expr_list = expr_list->right) {
    EXTRACT_NOTNULL(param, params->left);

    Invariant(param->sem);
    sem_t sem_type_param = param->sem->sem_type;

    if (is_in_parameter(sem_type_param)) {
      // in or in/out we skip
      continue;
    }

    Invariant(is_out_parameter(sem_type_param));  // that's all that's left
    out_args++;

    EXTRACT_ANY_NOTNULL(arg, expr_list->left);

    if (!is_id(arg)) {
      report_error(arg, "CQL0207: expected a variable name for OUT or INOUT argument", param->sem->name);
      record_error(ast);
      return;
    }

    EXTRACT_STRING(var_name, arg);

    if (arg->sem && arg->sem->sem_type & SEM_TYPE_IMPLICIT) {
      // If we're here, we must be reanalyzing a statement list as the implicit
      // flag is already set on `arg`.
      Invariant(current_loop_analysis_state == LOOP_ANALYSIS_STATE_REANALYZE);
      // We also must have already made a variable for this argument.
      symtab_entry *entry = symtab_find(current_variables, var_name);
      Invariant(entry);
      // That variable doesn't have the implicit flag set because we pulled it
      // off later in this function during the first loop analysis pass.
      ast_node *variable = entry->val;
      Invariant(!(variable->sem->sem_type & SEM_TYPE_IMPLICIT));
      // However, if it doesn't have it, we'll run into an issue when we call
      // `sem_call_stmt` below. When the type of `arg` would eventually looked
      // up during that call in `sem_resolve_id_expr`, the variable would
      // already be in scope (*without* the implicit flag set), its type would
      // be written into `arg`, and the implicit flag would be effectively
      // removed from `arg`. We'd then fail to emit the required variable
      // declaration during codegen due to the lack of the flag.
      //
      // The fix is to simply put the implicit flag back onto the variable
      // itself. Doing this allows the remainder of this function to work as it
      // did during the first loop analysis pass: It'll be on the variable for
      // `sem_call_stmt`, then we'll pull it back off at the end.
      variable->sem->sem_type |= SEM_TYPE_IMPLICIT;
      continue;
    }

    if (!symtab_find(current_variables, var_name)) {
      sem_t sem_type_var = param->sem->sem_type;
      sem_type_var &= (SEM_TYPE_NOTNULL | SEM_TYPE_SENSITIVE | SEM_TYPE_CORE);
      sem_type_var |= SEM_TYPE_VARIABLE | SEM_TYPE_IMPLICIT;

      AST_REWRITE_INFO_SET(name_ast->lineno, name_ast->filename);
      ast_node *variable = new_ast_str(var_name);
      variable->sem = ast->sem = new_sem(sem_type_var);
      variable->sem->name = var_name;
      variable->sem->kind = param->sem->kind;
      add_variable(var_name, variable);
      AST_REWRITE_INFO_RESET();
    }
  }

  if (out_args == 0) {
    report_error(name_ast, "CQL0390: DECLARE OUT CALL used on a procedure with no missing OUT arguments", name);
    record_error(ast);
    return;
  }

  sem_call_stmt(call_stmt);
  if (is_error(call_stmt)) {
    record_error(ast);
    return;
  }

  // Now we have to do a final swizzle, we want the call to have the IMPLICIT flag
  // on the variable usages just as we set up above, but we only want *this* call
  // to have them.  The flag must now be removed from the actual variables.  So we
  // do the walk the code generator is going to do but sort of in reverse... we're
  // wanting variables to undecorate.  The IMPLICIT bits are the bread crumbs we need.

  expr_list = call_stmt->right;

  for (; expr_list; expr_list = expr_list->right) {
    EXTRACT_ANY_NOTNULL(arg, expr_list->left);
    if (arg->sem->sem_type & SEM_TYPE_IMPLICIT) {
      EXTRACT_STRING(var_name, arg);
      symtab_entry *entry = symtab_find(current_variables, var_name);
      Invariant(entry);  // we just added it!
      ast_node *var = (ast_node*)(entry->val);
      // This must be the case as the same variable may only appear once if used
      // as an OUT or INOUT argument.
      Invariant(var->sem->sem_type & SEM_TYPE_IMPLICIT);
      // take it off the variable (so later uses will not get the mark)
      var->sem->sem_type &= sem_not(SEM_TYPE_IMPLICIT);
      // the implicit bit stays on the expression
    }
  }

  record_ok(ast);
}

// This is the main entry point for any kind of statement.  When we don't know
// what the statement is yet (such as we're walking a statement list) this will
// dispatch to the correct method.  Also, the top level statement captures
// any errors.
cql_noexport void sem_one_stmt(ast_node *stmt) {
  CHARBUF_OPEN(errbuf);
  bool_t capture_now = options.print_ast && error_capture == NULL;

  if (capture_now) {
    error_capture = &errbuf;
  }

  ast_node *stmt_and_attr = NULL;
  bool_t error = false;
  // We need to validate attributions of a statement, such as cql:ok_table_scan
  // or cql:no_table_scan which can only appear on a specific type of stmt.
  // We also need to do basic validation of the attributes, in case of const expressions.
  if (is_ast_stmt_and_attr(stmt->parent)) {
    stmt_and_attr = stmt->parent;
    EXTRACT_NOTNULL(misc_attrs, stmt_and_attr->left);

    // first check for expression failures with no regard to the particular attribute
    sem_misc_attrs_basic(misc_attrs);
    if (is_error(misc_attrs)) {
      record_error(stmt_and_attr);
      record_error(stmt);
      error = true;
    }
    else {
      sem_misc_attrs(misc_attrs);
      if (is_error(misc_attrs)) {
        record_error(stmt_and_attr);
        record_error(stmt);
        error = true;
      }
    }
  }

  if (!error) {
    symtab_entry *entry = symtab_find(non_sql_stmts, stmt->type);
    if (entry) {
      ((void (*)(ast_node*))entry->val)(stmt);
    }
    else {
      // If you use any of the following then you are a DML proc.
      has_dml = 1;
      entry = symtab_find(sql_stmts, stmt->type);

      // These are all the statements there are, we have to find it in this table
      // or else someone added a new statement and it isn't supported yet.
      Invariant(entry);
      ((void (*)(ast_node*))entry->val)(stmt);
    }
  }

  error |= is_error(stmt);
  // if stmt_and_attr exist then we should report the error to it since it's the root node
  // of a cql statement.
  if (stmt_and_attr) {
    error ? record_error(stmt_and_attr) : record_ok(stmt_and_attr);
  }

  if (capture_now) {
    cql_attach_captured_errors(stmt);
    error_capture = NULL;
  }

  if (is_control_stmt(stmt)) {
    flow_set_context_always_jumps(true);
  }

  CHARBUF_CLOSE(errbuf);
}

// We're just going to walk the tree of attribute values here
// looking for any CONST expressions.  If we find one, we evaluate
// that.  Anything that's not a CONST express is known to be a literal
// or just a name.
static void sem_misc_attr_value(ast_node *ast) {
  // nested attributes, we just recurse on those
  if (is_ast_misc_attr_value_list(ast)) {
    for (ast_node *item = ast; item; item = item->right) {
      sem_misc_attr_value(item->left);
      if (is_error(item->left)) {
        record_error(item);
        record_error(ast);
        return;
      }
    }
  }
  else if (is_ast_const(ast)) {
    // if the ast is bad the error will prop, this evaluates the const
    sem_root_expr(ast, SEM_EXPR_CONTEXT_NONE);
    if (is_error(ast)) {
      // ast already marked with is_error
      return;
    }
  }
  record_ok(ast);
}

// This is the basic checking of misc attributes we always do.
// The point of this is to find any constant expressions and replace
// them with actual literals and reveal any errors in those expressions.
// Most attributes don't need any processing because they are arbitrary names
// or regular literals.
static void sem_misc_attrs_basic(ast_node *ast) {
  Contract(is_ast_misc_attrs(ast));

  ast_node *head = ast;

  while (ast) {
    EXTRACT(misc_attr, ast->left);
    EXTRACT_ANY(misc_attr_value, misc_attr->right);

    if (misc_attr_value) {
      sem_misc_attr_value(misc_attr_value);
      if (is_error(misc_attr_value)) {
        record_error(head);
        return;
      }
    }
    ast = ast->right;
  }
  record_ok(head);
}

// Like `sem_stmt_list`, but does not create a new flow context. This is useful
// for cases where we want to analyze a statement list within a particular type
// of flow context (e.g., a branch context) or within a particular instance of
// an existing context.
static void sem_stmt_list_in_current_flow_context(ast_node *head) {
  Contract(head);

  sem_stmt_level++;

  bool_t error = false;
  for (ast_node *ast = head; ast; ast = ast->right) {
    ast_node *stmt = first_stmt_in_stmt_list(ast);
    sem_one_stmt(stmt);
    if (is_error(stmt)) {
      error = true;
    }
  }

  // if anything went wrong, then report the error on the statement list
  if (error) {
    record_error(head);
  }
  else {
    record_ok(head);
  }

  sem_stmt_level--;
}

// This helper just walks the list and processes each statement.  If anything
// goes wrong the first node in the list is marked as "error" so that callers
// can see that the net statement list is in error without walking each node.
static void sem_stmt_list(ast_node *head) {
  // For any list of statements, any improvements made within cannot be assumed
  // to be valid afterwards. We therefore need to create a new context.
  FLOW_PUSH_CONTEXT_NORMAL();

  sem_stmt_list_in_current_flow_context(head);

  FLOW_POP_CONTEXT_NORMAL();
}

// Like `sem_stmt_list`, but specifically for lists of statements within loops
// (e.g., WHILE and LOOP). The optional `true_expr` argument is used to set
// positive improvements via the knowledge that `true_expr` must have been true
// if the body of the loop is presently executing. (Due to the fact that one can
// jump out of a loop, no assumptions can be made about `true_expr` after the
// loop exits, and so no negative improvements are set.)
static void sem_stmt_list_within_loop(ast_node *stmt_list, ast_node *true_expr) {
  Contract(stmt_list);

  loop_analysis_state saved_loop_analysis_state = current_loop_analysis_state;
  bool_t is_top_level_loop = false;

recurse:
  switch (current_loop_analysis_state) {
    case LOOP_ANALYSIS_STATE_NONE:
      is_top_level_loop = true;
      current_loop_analysis_state = LOOP_ANALYSIS_STATE_ANALYZE;
      // Save a stack frame and avoid implicit fallthrough.
      goto recurse;
    case LOOP_ANALYSIS_STATE_ANALYZE: {
      // Analyze the statement list within a jump context. The jump context
      // ensures that any improvements in effect before the loop which are unset
      // within the loop, then re-set later in the loop, are re-unset after the
      // loop. See `_flow_pop_context_jump` for an example of why this is
      // necessary.
      FLOW_PUSH_CONTEXT_JUMP();
      if (true_expr) {
        sem_set_improvements_for_true_condition(true_expr);
      }
      sem_stmt_list_in_current_flow_context(stmt_list);
      FLOW_POP_CONTEXT_JUMP();
      if (is_error(stmt_list)) {
        goto cleanup;
      }
      // We only want to perform reanalysis if this is a top-level loop. Doing
      // it for every loop would not only result in a lot of unnecessary work,
      // it would also cause problems for other parts of the code that need to
      // have the final set of improvements to do their job properly (e.g.,
      // `sem_resolve_id_expr`) -- we cannot have the final set of improvements
      // for a particular loop until all the preceding portions of all enclosing
      // loops also have their final sets.
      if (is_top_level_loop) {
        current_loop_analysis_state = LOOP_ANALYSIS_STATE_REANALYZE;
        goto recurse;
      }
      break;
    }
    case LOOP_ANALYSIS_STATE_REANALYZE: {
      // Analyze the statement list again. This is necessary so that any
      // un-improvements via statements later in the loop can appropriately
      // negatively affect statements earlier in the loop should evaluation of
      // the loop repeat. If we didn't do this, code such as the following would
      // not result in an error:
      //
      //   DECLARE x INT;
      //   SET x := 1;
      //   WHILE some_condition
      //   BEGIN
      //     CALL requires_int_notnull(x);
      //     SET x := NULL;
      //   END;
      //
      // NOTE: We create another jump context here, but a normal context would
      // work just as well because any improvements in effect before the loop that
      // needed to be unset to ensure safety were already unset above.
      FLOW_PUSH_CONTEXT_JUMP();
      if (true_expr) {
        sem_set_improvements_for_true_condition(true_expr);
      }
      sem_stmt_list_in_current_flow_context(stmt_list);
      FLOW_POP_CONTEXT_JUMP();
      if (is_error(stmt_list)) {
        goto cleanup;
      }
      break;
    }
  }

cleanup:
  current_loop_analysis_state = saved_loop_analysis_state;
}

// Expression type for current proc literal
static void sem_expr_proclit(ast_node *ast) {
  Contract(is_ast_str(ast));

  // name already known to match or we wouldn't be here
  CSTR name = process_proclit(ast, "@proc");
  if (!name) {
    return;
  }

  // replace with a standard string literal
  CSTR strlit = dup_printf("'%s'", name);
  ((str_ast_node*)ast)->value = strlit;

  ast->sem = new_sem(SEM_TYPE_TEXT | SEM_TYPE_NOTNULL);
}

// @rc is like a builtin variable, it refers to the _rc_ state
// note, use of @rc forces you to become a dml proc which isn't
// very onerous because rc makes no sense if it isn't a dml proc.
// We do it this way because it's possible that you're using @rc
// in a loop or some such and you haven't run any DML yet so we don't
// yet know that you are a DML proc.  Generating an error would be annoying.
// This also has the useful property that you can force a proc to be dml
// with "if @rc then endif;" which is useful when you are trying to create mocks.
static void sem_expr_at_rc(ast_node *ast) {
  Contract(is_ast_str(ast));
  ast->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL| SEM_TYPE_VARIABLE);
  ast->sem->name = "@rc";
  has_dml = 1; // use of result code implies DML proc
}

// Expression type for numeric primitives
static void sem_expr_num(ast_node *ast, CSTR cstr) {
  Contract(is_ast_num(ast));
  EXTRACT_NUM_TYPE(num_type, ast);
  switch (num_type) {
  case NUM_BOOL:
    ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL);
    break;

  case NUM_INT:
    ast->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
    break;

  case NUM_LONG:
    ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL);
    break;

  default:
    // this is all that's left
    Contract(num_type == NUM_REAL);
    ast->sem = new_sem(SEM_TYPE_REAL | SEM_TYPE_NOTNULL);
    break;
  }
}

// Expression type for blob literals, valid only in a SQL context
static void sem_expr_blob(ast_node *ast, CSTR cstr) {
  Contract(is_ast_blob(ast));

  if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
    report_error(ast, "CQL0313: blob literals may only appear in the context of a SQL statement", NULL);
    record_error(ast);
    return;
  }

  // blob literal
  ast->sem = new_sem(SEM_TYPE_BLOB | SEM_TYPE_NOTNULL);
}

// Expression type for string or identifier primitives
static void sem_expr_str(ast_node *ast, CSTR cstr) {
  Contract(is_ast_str(ast));
  EXTRACT_STRING(str, ast);
  if (is_strlit(ast)) {
    // note str is the lexeme, so it is still quoted and escaped
    ast->sem = new_sem(SEM_TYPE_TEXT | SEM_TYPE_NOTNULL);
  }
  else if (is_proclit(ast)) {
    sem_expr_proclit(ast);
  }
  else if (is_at_rc(ast)) {
    sem_expr_at_rc(ast);
  }
  else {
    sem_resolve_id_expr(ast, str, NULL);
  }
}

// Expression type for constant NULL
static void sem_expr_null(ast_node *ast, CSTR cstr) {
  Contract(is_ast_null(ast));
  // null literal
  ast->sem = new_sem(SEM_TYPE_NULL);
}

// Expression type for scoped name.
static void sem_expr_dot(ast_node *ast, CSTR cstr) {
  Contract(is_ast_dot(ast));
  EXTRACT_NAME_AND_SCOPE(ast);
  sem_resolve_id_expr(ast, name, scope);
}

// This function is used to detect the pattern that leaks memory on SQLite.
// The context is an INSERT statement that is using a SELECT for its data
// the patterns detected that might leak are:
//   insert X
//     select * from X join Y
//
//   insert X
//     select * from X
//     union all
//     select * from Y
//
// The error tells you to replace these with something like
//
//   with cte(*) as (select * from X join Y)
//     insert X select * from cte;
//
// The idea is that the select that forms the insert cannot have a top level operand
// like join or union.  But you can nest one without problem.
//
// Also ok:
//
//   insert X
//     select * from (select * from X join Y)
//
// So here we look at the various select forms:
//   * explain form -> can't happen
//   * with form : look at the inner select and verify that
//   * select form:
//     * if compound report error
//     * if VALUES -> ok
//     * if not compound, report error if top node is a join
static bool_t sem_select_stmt_is_mixed_results(ast_node *ast) {
  Contract(is_select_stmt(ast));
  Contract(!is_ast_explain_stmt(ast));  // disallowed by grammar

  ast_node *select_stmt;

  // first extract the select stmt out of the WITH form if needed
  if (is_ast_select_stmt(ast)) {
    select_stmt = ast;
  }
  else {
    Contract(is_ast_with_select_stmt(ast));
    EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
    EXTRACT(cte_tables, with_prefix->left);
    // extract the main select out of the with, this is where we will look for the
    // top level join
    select_stmt = ast->right;
  }

  // Get the select_core, there must be one
  Invariant(is_ast_select_stmt(select_stmt));
  EXTRACT_NOTNULL(select_core_list, select_stmt->left);
  EXTRACT_NOTNULL(select_core, select_core_list->left);

  // if compound, this is the error case
  if (select_core_list->right) {
    return true;
  }

  EXTRACT_ANY(any_select_opts, select_core->left);
  EXTRACT_ANY_NOTNULL(select_core_right, select_core->right);

  // the core might be VALUES -> that's ok
  if (is_ast_select_values(any_select_opts)) {
    // VALUES [values]
    Contract(is_ast_values(select_core_right));
    return false;
  }

  // ok to traverse to query parts now
  Contract(is_ast_select_expr_list_con(select_core_right));
  EXTRACT_NOTNULL(select_expr_list_con, select_core->right);
  EXTRACT_NOTNULL(select_from_etc, select_expr_list_con->right);
  EXTRACT_ANY(query_parts, select_from_etc->left);

  // if top query part is a join -> this is the bad case
  return is_ast_join_clause(query_parts);
}

// helper function to check if a select expression with a built-in aggregate function will always return a row
static bool_t sem_check_aggregate_select_expr_must_return_a_row(ast_node *ast, ast_node *select_where) {
  Contract(is_ast_select_stmt(ast));
  Contract(is_ast_select_where(select_where));

  EXTRACT_NOTNULL(select_groupby, select_where->right);
  EXTRACT(opt_groupby, select_groupby->left);

  EXTRACT_NOTNULL(select_orderby, ast->right);
  EXTRACT_NOTNULL(select_limit, select_orderby->right);
  EXTRACT(opt_limit, select_limit->left);
  EXTRACT_NOTNULL(select_offset, select_limit->right);
  EXTRACT(opt_offset, select_offset->left);

  // Assume any OFFSET or GROUP BY clause may lead to aggregation not return a row. OFFSETs with constant of 0 or less
  // are no-ops, but won't be considered
  if (is_ast_opt_offset(opt_offset) || is_ast_opt_groupby(opt_groupby)) {
    return false;
  }

  // When LIMIT is used, if it cannot evaluate to a positive constant, then it might not return a row
  if (!opt_limit) {
    // Short circuit and allow error handling to kick in elsewhere
    return true;
  } else {
    eval_node result = EVAL_NIL;
    eval(opt_limit->left, &result);

    if (result.sem_type != SEM_TYPE_ERROR && result.sem_type != SEM_TYPE_NULL) {
      eval_cast_to(&result, SEM_TYPE_LONG_INTEGER);
      return result.int64_value >= 1;
    } else {
      return false;
    }
  }

  return true;
}

// Only for use in (select expr ...) so there is known to be exactly one item in the
// select list.  This tells us if there is some way we can know that there will be
// a row for sure in such an expression.  There are assorted special cases that are
// helpful to handle such as:
//   there is no FROM and no WHERE  e.g. (select 1)
//   the select list is only COUNT or TOTAL e.g. (select count(*) from somewhere)
// anything that looks complicated -> we assume it might not return rows
static bool_t sem_select_expr_must_return_a_row(ast_node *ast) {
  // we only handle simple select forms, WITH etc. are assumed to be complex
  // and might return null or whatever...

  // a with_select or query plan or some such... not simple
  if (!is_ast_select_stmt(ast)) {
    return false;
  }

  // In a simple select statement (not compound or otherwise weird, like (select 1)
  // the lack of a from clause means we can't be in the zero row case so no need
  // to remove nullability there either.
  EXTRACT_NOTNULL(select_core_list, ast->left);
  EXTRACT_NOTNULL(select_core, select_core_list->left);
  EXTRACT_NOTNULL(select_expr_list_con, select_core->right);
  EXTRACT_NOTNULL(select_from_etc, select_expr_list_con->right);
  EXTRACT_ANY(query_parts, select_from_etc->left);
  EXTRACT_NOTNULL(select_where, select_from_etc->right);
  EXTRACT(opt_where, select_where->left);

  // compound query is not simple (it could have INTERSECT or some such)
  if (select_core_list->right) {
    return false;
  }

  // No query_parts and opt_where means there is no FROM/WHERE clause, so the
  // result can't be nullable due to zero rows.  It might be nullable for other
  // reasons already computed so the flag bit just stays

  // note (SELECT EXISTS(whatever)) will fall into this form because there is
  // no top level from or where clause.  Also exists isn't a proc call so it's not the next case

  if (!query_parts && !opt_where) {
    return true;
  }

  // One last chance, a simple select list with just COUNT or EXISTS is also for sure
  // going to return a row, we can handle that.

  EXTRACT_ANY_NOTNULL(select_expr_list, select_expr_list_con->left);

  // might be select * or T.* or some such, has to be a simple expression.
  if (!is_ast_select_expr(select_expr_list->left)) {
    return false;
  }

  // ok it's not *, so we have a shot at this, it could be one of the safe ones
  EXTRACT_NOTNULL(select_expr, select_expr_list->left);

  // remember only 1 arg cases are allowed in this func, this is the (select expr..) node
  Contract(select_expr_list->right == NULL);
  EXTRACT_ANY_NOTNULL(expr, select_expr->left);

  // the special cases are calls, if it's not a call we're done
  if (!is_ast_call(expr)) {
    return false;
  }

  // built-in aggregate functions clause might always return at least one row
  EXTRACT_STRING(name, expr->left);
  if (
    !Strcasecmp("avg", name) ||
    !Strcasecmp("count", name) ||
    !Strcasecmp("group_concat", name) ||
    !Strcasecmp("sum", name) ||
    !Strcasecmp("total", name)
  ) {
    return sem_check_aggregate_select_expr_must_return_a_row(ast, select_where);;
  }

  // min and max are aggregate functions if they have exactly one argument, they are scalar functions if they have two
  // or more arguments
  if (
    !Strcasecmp("max", name) ||
    !Strcasecmp("min", name)
  ) {
      EXTRACT_ANY_NOTNULL(call_arg_list, expr->right);
      EXTRACT_ANY_NOTNULL(arg_list, call_arg_list->right);

      uint32_t arg_count = 0;
      for (ast_node *item = arg_list; item; item = item->right) arg_count++;

      if (arg_count == 1) {
        return sem_check_aggregate_select_expr_must_return_a_row(ast, select_where);
      }
  }

  return false;
}

// Expression type for nested select expression
static void sem_expr_select(ast_node *ast, CSTR cstr) {
  Contract(is_select_stmt(ast));
  EXTRACT_ANY_NOTNULL(parent, ast->parent);

  // this tells us if we might be the left side of a select if nothing
  bool_t in_select_if_nothing =
     is_ast_select_if_nothing_throw_expr(parent) ||
     is_ast_select_if_nothing_expr(parent) ||
     is_ast_select_if_nothing_or_null_expr(parent);

  if (in_select_if_nothing && current_expr_context != SEM_EXPR_CONTEXT_NONE) {
    report_error(parent, "CQL0369: (SELECT ... IF NOTHING) construct is for use in top level expressions, not inside of other DML", NULL);
    record_error(ast);
    return;
  }

  if (current_expr_context == SEM_EXPR_CONTEXT_CONSTRAINT) {
    report_error(ast, "CQL0394: nested select expressions may not appear inside of a constraint expression", NULL);
    record_error(ast);
    return;
  }

  // (select ...)
  sem_select(ast);
  if (is_error(ast)) {
    return;
  }

  // For purposes of testing "strict if nothing", a select on the left side of the if nothing
  // operator is in an if nothing context  but the right side is not in an if nothing context.
  //  e.g.
  // in (select foo from bar if nothing (select baz)) the (select baz) is not in an
  // if nothing context and hence would generate an error if "strict if nothing" is on.
  // Inside of SQL is ok in all cases
  // Trivial selects (e.g. (select <expr>)) are always ok

  bool_t invalid_select  =
    enforcement.strict_if_nothing &&
    current_expr_context == SEM_EXPR_CONTEXT_NONE &&
    !(in_select_if_nothing && parent->left == ast) &&
    !sem_select_expr_must_return_a_row(ast);

  if (invalid_select) {
    report_error(ast, "CQL0368: strict select if nothing requires that all (select ...) expressions include 'if nothing'", NULL);
    record_error(ast);
    return;
  }

  Invariant(is_struct(ast->sem->sem_type));
  sem_struct *sptr = ast->sem->sptr;
  Invariant(sptr);
  if (sptr->count != 1) {
    report_error(ast, "CQL0232: nested select expression must return exactly one column", NULL);
    record_error(ast);
    return;
  }

  // select expressions might return zero rows and become null like that,
  // so we usually have to remove the notnull bit from the type.
  bool_t remove_notnull = 1;

  if (current_expr_context == SEM_EXPR_CONTEXT_NONE) {
    // In a non-sql context (e.g. set x := (select 1); )
    // a row is expected or there is an exception.
    // So no need to remove nullability there.
    remove_notnull = 0;
  }
  else if (sem_select_expr_must_return_a_row(ast)) {
    // any of the forms that are known to return a row such as
    //  * no where clause
    //  * no from clause
    //  * select list uses only exists or count
    // in those cases zero rows isn't an option so we don't have to concern ourselves removing nullability
    remove_notnull = 0;
  }

  sem_t sem_type = sptr->semtypes[0];

  if (sem_type == SEM_TYPE_NULL) {
     report_error(ast, "CQL0374: SELECT expression is equivalent to NULL", NULL);
     record_error(ast);
     return;
  }

  // and boom remove the bit if we're supposed to remove it (most times except the above exceptions)
  if (remove_notnull) {
    sem_type &= sem_not(SEM_TYPE_NOTNULL);
  }

  ast->sem = new_sem(sem_type);
  ast->sem->name = sptr->names[0];
  ast->sem->kind = sptr->kinds[0];
}

// If nothing throw is exactly the same as a normal select expr
// the only difference is that it is legal inside of strict select if nothing
// because the user has made the throw explicit so they're saying they
// know it's gonna throw and that's ok.
static void sem_expr_select_if_nothing_throw(ast_node *ast, CSTR op) {
  Contract(is_ast_select_if_nothing_throw_expr(ast));
  EXTRACT_ANY_NOTNULL(select_expr, ast->left);
  sem_expr_select(select_expr, op);
  ast->sem = select_expr->sem;
}

// Despite the unusual nature of SELECT .. IF NOTHING ... the net semantic rules
// are basically exactly the same as any normal binary operator.
//   * types must be compatible
//   * the net type is the promoted type of left and right
//   * nullable or sensitive if either is nullable or sensitive
//   * type kind must be compatible
//   * special case SELECT ... IF NOTHING OR NULL ... is not null if the right are is not null
static void sem_expr_select_if_nothing(ast_node *ast, CSTR op) {
  // same rules for both forms
  Contract(is_ast_select_if_nothing_expr(ast) || is_ast_select_if_nothing_or_null_expr(ast));

  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (error_any_object(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (!sem_verify_compat(ast, ast->left->sem->sem_type, ast->right->sem->sem_type, op)) {
    return;
  }

  sem_t core_type = sem_combine_types(core_type_left, core_type_right);

  CSTR kind = sem_combine_kinds(ast->right, ast->left->sem->kind);
  if (is_error(ast->right)) {
    record_error(ast);
    return;
  }

  if (is_ast_select_if_nothing_or_null_expr(ast)) {
    if (is_nullable(ast->right->sem->sem_type)) {
      if (is_ast_null(ast->right)) {
        report_error(ast, "CQL0372: SELECT ... IF NOTHING OR NULL NULL is redundant; use SELECT ... IF NOTHING NULL instead", NULL);
        record_error(ast);
        return;
      }
    } else {
      // if the right arg is not null then the expression is not null because it's like a builtin ifnull
      combined_flags |= SEM_TYPE_NOTNULL;
    }
  }

  ast->sem = new_sem(core_type | combined_flags);
  ast->sem->kind = kind;
}

// At this point all processing of input is complete.  So now we walk all the tables
// that we ever saw and visit any that have not already been validated.  This is
// the set of tables not present in the previous schema.  All of these must be
// marked with @create.
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_all_tables_not_in_previous(ast_node *root) {
  CHARBUF_OPEN(err_msg);
  bprintf(&err_msg, "CQL0309: new table must be added with @create(%d) or later", max_previous_schema_version);

  for (list_item *item = all_tables_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_table_stmt(ast));

    // no need to report on tables that are already in error state
    if (!is_error(ast)) {
      EXTRACT_NOTNULL(create_table_name_flags, ast->left);
      EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
      EXTRACT_STRING(name, name_ast);

      sem_validate_old_object_or_marked_create(root, ast, err_msg.ptr, name);
    }
  }

  CHARBUF_CLOSE(err_msg);
}

// Verify that either:
// * the object either previously existed (and hence previously validated)
// * or, the object has been marked as new in the current schema version.
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_old_object_or_marked_create(ast_node *root, ast_node *ast, CSTR err_msg, CSTR name) {
  Contract(root);
  Contract(ast);
  Contract(err_msg);
  Contract(name);

  // if the object has other errors we don't need to check its version info right now, that's just spurious
  if (is_error(ast)) {
    return;
  }

  // if the object was already checked by previous schema, we don't have to do anything
  if (is_validated(ast->sem->sem_type)) {
    return;
  }

  // If the object is marked as created at or after the previous schema version
  // then it's good.
  if (ast->sem->create_version >= max_previous_schema_version) {
    return;
  }

  // Direct to @recreate is also ok
  if (ast->sem->recreate) {
    return;
  }

  report_and_capture_error(root, ast, err_msg, name);
}

// At this point all processing of input is complete.  So now we walk all the tables
// that left the @recreate plan and make sure they entered the strong plan at the right
// version number.
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_all_prev_recreate_tables(ast_node *root) {
  CHARBUF_OPEN(err_msg);
  bprintf(&err_msg, "CQL0399: table must leave @recreate management with @create(%d) or later", max_previous_schema_version);

  for (list_item *item = all_prev_recreate_tables; item; item = item->next) {
    ast_node *ast = item->ast;

    EXTRACT_NOTNULL(create_table_name_flags, ast->left);
    EXTRACT_ANY_NOTNULL(name_ast, create_table_name_flags->right);
    EXTRACT_STRING(name, name_ast);

    sem_validate_marked_create_or_delete(root, ast, err_msg.ptr, name);
  }

  CHARBUF_CLOSE(err_msg);
}

// Verify that either:
// * the object was created at a late enough version
// * or, the object was deleted at a late enough version
//
// Note: if it's both we will have previously validated that the versions are compatible with each other
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_marked_create_or_delete(ast_node *root, ast_node *ast, CSTR err_msg, CSTR name) {
  Contract(root);
  Contract(ast);
  Contract(err_msg);
  Contract(name);

  Invariant(is_ast_create_table_stmt(ast));

  // If the object is marked as created at or after the previous schema version
  // then it's good.
  if (ast->sem->create_version >= max_previous_schema_version) {
    return;
  }

  report_and_capture_error(root, ast, err_msg, name);
}

// At this point all processing of input is complete.  So now we walk all the
// created columns.
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_all_columns_not_in_previous(ast_node *root) {
  CHARBUF_OPEN(err_msg);
  bprintf(&err_msg, "CQL0310: new column must be added with @create(%d) or later", max_previous_schema_version);

  for (list_item *item = created_columns; item; item = item->next) {
    ast_node *def = item->ast;
    Invariant(is_ast_col_def(def));

    EXTRACT_NOTNULL(col_def_type_attrs, def->left);
    EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
    EXTRACT_ANY_NOTNULL(name_ast, col_def_name_type->left);
    EXTRACT_STRING(name, name_ast);

    sem_validate_old_object_or_marked_create(root, def, err_msg.ptr, name);
  }
  CHARBUF_CLOSE(err_msg);
}

static void sem_enforcement_options(ast_node *ast, bool_t strict) {
  EXTRACT_OPTION(option, ast);

  switch (option) {
    case ENFORCE_STRICT_JOIN:
      enforcement.strict_join = strict;
      break;

    case ENFORCE_FK_ON_UPDATE:
      enforcement.strict_fk_update = strict;
      break;

    case ENFORCE_UPSERT_STMT:
      enforcement.strict_upsert_stmt = strict;
      break;

    case ENFORCE_WINDOW_FUNC:
      enforcement.strict_window_func = strict;
      break;

    case ENFORCE_CAST:
      enforcement.strict_cast = strict;
      break;

    case ENFORCE_WITHOUT_ROWID:
      enforcement.strict_without_rowid = strict;
      break;

    case ENFORCE_TRANSACTION:
      enforcement.strict_transaction = strict;
      break;

    case ENFORCE_SELECT_IF_NOTHING:
      enforcement.strict_if_nothing = strict;
      break;

    case ENFORCE_INSERT_SELECT:
      enforcement.strict_insert_select = strict;
      break;

    case ENFORCE_TABLE_FUNCTION:
      enforcement.strict_table_function = strict;
      break;

    case ENFORCE_ENCODE_CONTEXT_COLUMN:
      enforcement.strict_encode_context = strict;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_INTEGER:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_INTEGER;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_LONG_INTEGER:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_LONG_INTEGER;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_REAL:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_REAL;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_BOOL:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_BOOL;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_TEXT:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_TEXT;
      break;

    case ENFORCE_ENCODE_CONTEXT_TYPE_BLOB:
      enforcement.strict_encode_context_type = strict;
      encode_context_type = SEM_TYPE_BLOB;
      break;

    case ENFORCE_IS_TRUE:
      enforcement.strict_is_true = strict;
      break;

    case ENFORCE_SIGN_FUNCTION:
      enforcement.strict_sign_function = strict;
      break;

    case ENFORCE_CURSOR_HAS_ROW:
      enforcement.strict_cursor_has_row = strict;
      break;

    default:
      // this is all that's left
      Contract(option == ENFORCE_FK_ON_DELETE);
      enforcement.strict_fk_delete = strict;
      break;
  }
}

// At this point all processing of input is complete.  So now we walk all the ad hoc rules
// that we ever saw and visit any that have not already been validated.  This is
// the set of rules not present in the previous schema.  All of these must be
// marked at the most recent version.
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_all_ad_hoc_not_in_previous(ast_node *root) {
  CHARBUF_OPEN(err_msg);
  bprintf(&err_msg, "new ad hoc rule must be added at version %d or later", max_previous_schema_version);

  for (list_item *item = all_ad_hoc_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_schema_ad_hoc_migration_stmt(ast));

    EXTRACT(version_annotation, ast->left);
    EXTRACT_STRING(name, version_annotation->right);

    sem_validate_old_object_or_marked_create(root, ast, err_msg.ptr, name);
  }

  CHARBUF_CLOSE(err_msg);
}

// At this point all processing of input is complete.  So now we walk all the ad hoc rules
// that we ever saw and visit any that have not already been validated.  This is
// the set of rules not present in the previous schema.  All of these must be
// marked at the most recent version.
//
// Note: this processing does not happen in the context of a statement
// so we have to do our own error capture logic.
static void sem_validate_all_subscriptions_not_in_previous(ast_node *root) {
  Contract(root);

  // since we validate in order, we can start where we left off with validations
  // anything else was already checked (we don't need a VALIDATED bit for these guys)
  // also, these are already known to be in ascending order, so we only need to check
  // the first one.
  if (next_subscription && !found_subscription_error) {
    ast_node *ast = next_subscription->ast;

    subs_info info;
    sem_subs_extract(ast, &info);

    // * if the annotation has other errors we don't need to check its version info right now, that's just spurious
    // * if the annotation is at or after the max previous schema version it's good

    if (!is_error(ast) && info.vers < max_previous_schema_version) {
      CSTR err_msg = dup_printf(
        "new @unsub/@resub must be added at version %d or later",
        max_previous_schema_version);

      report_and_capture_error(root, ast, err_msg, NULL);
    }
  }
}

// switch to strict mode
static void sem_enforce_strict_stmt(ast_node * ast) {
  Contract(is_ast_enforce_strict_stmt(ast));
  sem_enforcement_options(ast->left, 1);
  record_ok(ast);
}

// switch to normal mode
static void sem_enforce_normal_stmt(ast_node * ast) {
  Contract(is_ast_enforce_normal_stmt(ast));
  sem_enforcement_options(ast->left, 0);
  record_ok(ast);
}

static void reset_enforcements() {
  memset(&enforcement, 0, sizeof(enforcement));
  enforcement.strict_cast = true;
}

// reset all to normal mode
static void sem_enforce_reset_stmt(ast_node * ast) {
  Contract(is_ast_enforce_reset_stmt(ast));
  reset_enforcements();
  record_ok(ast);
}

// save current enforcement options
static void sem_enforce_push_stmt(ast_node *ast) {
  Contract(is_ast_enforce_push_stmt(ast));
  // this item will be freed with the pool
  enforcement_stack_record *item = _ast_pool_new(enforcement_stack_record);
  item->options = enforcement;
  item->next = enforcement_stack;
  enforcement_stack = item;
  record_ok(ast);
}

// restore previous options
static void sem_enforce_pop_stmt(ast_node *ast) {
  Contract(is_ast_enforce_pop_stmt(ast));
  enforcement_stack_record *item = enforcement_stack;

  if (!item) {
    report_error(ast, "CQL0365: @enforce_pop used but there is nothing to pop", NULL);
    record_error(ast);
    return;
  }

  enforcement = item->options;
  enforcement_stack = item->next;
  record_ok(ast);
}

// Ensure that the schema directives are not inside of a procedure
static bool_t verify_schema_region_out_of_proc(ast_node *ast) {
  if (current_proc) {
    report_error(ast, "CQL0248: schema region directives may not appear inside of a procedure", NULL);
    return false;
  }

  return true;
}


// Checks to see if a given region has any links that peek into the middle of an owned
// Section; these are illegal
static void sem_validate_region_links(ast_node *ast) {
  Contract(is_region(ast));
  EXTRACT_STRING(name, ast->left);

  EXTRACT(region_list, ast->right);
  for (ast_node *item = region_list; item; item = item->right) {
    Contract(is_ast_region_list(item));
    EXTRACT_NOTNULL(region_spec, item->left);
    EXTRACT_STRING(item_name, region_spec->left);
    ast_node *region = find_region(item_name);

    // if it's linking to something unclaimed that's ok
    // if it's linking to something that is a deployable region root that's ok
    // if it's claimed and not the root that's an error
    // if the target already has errors, don't spam more errors

    if (!is_error(region) && region->sem->region && !(region->sem->sem_type & SEM_TYPE_DEPLOYABLE)) {
       CHARBUF_OPEN(msg);
       bprintf(&msg, "CQL0291: region links into the middle of a deployable region;"
                     " you must point to the root of '%s' not into the middle:",
                     region->sem->region);
       report_error(ast, msg.ptr, name);
       ast->sem->region = "(error)";
       record_error(ast);
       CHARBUF_CLOSE(msg);
       return;
    }
  }
}

// A schema region is an partitioning of the schema such that it
// only uses objects in the same partition or one of its declared
// dependencies.  One schema region may be upgraded independently
// from any others (assuming they happen such that dependents are done first).
// Here we validate:
//  * the region name is unique
//  * the dependencies (if any) are unique and exist
static void sem_declare_schema_region_stmt(ast_node *ast) {
  Contract(is_region(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT(region_list, ast->right);

  // @DECLARE_SCHEMA_REGION name [USING region_list]

  if (!verify_schema_region_out_of_proc(ast)) {
    record_error(ast);
    return;
  }

  // Check the name list first, before we have defined the new region
  // this prevents any chance of cycles in the DAG.

  if (region_list) {
    if (!sem_verify_no_duplicate_regions(region_list)) {
      record_error(ast);
      return;
    }

    for (ast_node *item = region_list; item; item = item->right) {
      Contract(is_ast_region_list(item));
      EXTRACT_NOTNULL(region_spec, item->left);
      EXTRACT_STRING(item_name, region_spec->left);
      if (!find_region(item_name)) {
        report_error(item, "CQL0244: unknown schema region", item_name);
        record_error(ast);
        return;
      }
    }
  }

  sem_t sem_type = SEM_TYPE_REGION;

  if (is_ast_declare_deployable_region_stmt(ast)) {
    sem_type |= SEM_TYPE_DEPLOYABLE;
  }

  ast->sem = new_sem(sem_type);
  ast->sem->name = name;

  // note that regions get a slightly different treatment when in previous schema
  // validation mode.  Most entites are not added to the name tables at all
  // we check it as we visit it and then move on;   We can't do that with regions
  // because they are used by later things and the "new" regions (before the @previous_schema
  // marker) might be very different. We need the "old" regions to calculate the
  // old deployment regions and make sure they haven't changed.  So we can't just
  // check them and move on like we do with other stuff.  At the end we'll have
  // two symbol tables

  bool_t adding_current_entity = will_add_current_entity();

  // So, per the above we still do this (even if previous schema mode)

  if (!add_region(ast, name)) {
    report_error(ast, "CQL0245: schema region already defined", name);
    record_error(ast);
    return;
  }

  // But we don't do this:  So when emitting the schema we won't emit
  // the previous regions.  Other entites do neither the above add
  // or the below add. This is the difference.

  if (adding_current_entity) {
    add_item_to_list(&all_regions_list, ast);
  }

  sem_validate_region_links(ast);
}

// Recursively marks all the contained regions that are not already deployment regions
// as being deployed in this region.
static void sem_mark_deployment_subgraph(CSTR current, CSTR owner) {
  // Every name we encounter has already been validated!
  ast_node *region = find_region(current);
  Invariant(region);

  // the region field corresponds to the region this entity is in, for a region
  // that means the deployment region its in.  Note that the regions form a DAG
  // so it's possible to get to the same node two different ways.  That's not a problem.
  // By construction this will paint exactly the nodes that are under this owning
  // deployable region and we know it's safe to visit these all because if there
  // were any possible conflicts they would have been spotted when the node was added
  // or when a previous deployment region was declared.  See the relevant logic
  // in sem_declare_deployable_region where unmarked nodes are re-checked.
  if (region->sem->region) {
    // if it's already set, we're good to go
    return;
  }

  region->sem->region = owner;

  EXTRACT(region_list, region->right);
  for (ast_node *item =region_list; item; item = item->right) {
    Contract(is_ast_region_list(item));
    EXTRACT_NOTNULL(region_spec, item->left);
    EXTRACT_STRING(item_name, region_spec->left);
    sem_mark_deployment_subgraph(item_name, owner);
  }
}

// A deployable region is a regular region plus additional rules
// We first declare the region and then check for the containment rules
static void sem_declare_deployable_region_stmt(ast_node *ast) {
  Contract(is_ast_declare_deployable_region_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  sem_declare_schema_region_stmt(ast);
  if (is_error(ast)) {
    return;
  }

  // I am the new owner of the entire subgraph
  sem_mark_deployment_subgraph(ast->sem->name, ast->sem->name);

  for (list_item *item = all_regions_list; item; item = item->next) {
    ast_node *region_ast = item->ast;
    EXTRACT_STRING(target_name, region_ast->left);

    // this region is not yet part of any deployment, it might be reaching into
    // the deployment region we just made, so we have to check its linkage again

    if  (is_error(region_ast)) {
      continue;
    }

    if (!region_ast->sem->region) {

      Invariant(target_name);

      sem_validate_region_links(region_ast);
      if (is_error(region_ast)) {
        // This region is now in error also, its root isn't well defined
        record_error(ast);
        return;
      }
    }
  }

  // I am the new owner of the entire subgraph
  sem_mark_deployment_subgraph(ast->sem->name, ast->sem->name);

  // error checking will go here
}

typedef struct region_walk {
  CSTR root_name;
  symtab *regions;
  bool_t honor_private;
} region_walk;

// Recursively builds the set of all regions that are antecedents of the given region
// This will let us quickly test if any given object should be visible in the current context.
static void sem_walk_regions(region_walk *acc, CSTR name) {
  // First try to add, and see if we've already visited this region.
  // The region shape is a DAG so it's possible to get to the same place
  // two different ways.
  if (!symtab_add(acc->regions, name, NULL)) {
    return;
  }

  // Every name we encounter has already been validated!
  ast_node *region = find_region(name);
  Invariant(region);

  EXTRACT(region_list, region->right);
  for (ast_node *item = region_list; item; item = item->right) {
    Contract(is_ast_region_list(item));
    EXTRACT_NOTNULL(region_spec, item->left);
    EXTRACT_STRING(item_name, region_spec->left);
    EXTRACT_OPTION(type, region_spec->right);
    bool_t is_private = (type == PRIVATE_REGION);

    // Notes here:
    //  * Private things are not private to the one that introduced them
    //    so if we're at the root we visit privates
    //  * If we're doing schema gen or something like that visibility
    //    isn't a consideration, so private shouldn't be honored.
    //  * If you can reach a region two ways one of which isn't private, then
    //    it will be accumulated.

    if (name == acc->root_name || !acc->honor_private || !is_private) {
      sem_walk_regions(acc, item_name);
    }
  }
}

// Just use the helper above to do all regions
cql_noexport void sem_accumulate_full_region_image(symtab *r, CSTR name) {
  region_walk acc = {
    .regions = r,
    .root_name = name,
    .honor_private = 0,
  };

  sem_walk_regions(&acc, name);
}

// Just use the helper above to do only public regions
cql_noexport void sem_accumulate_public_region_image(symtab *r, CSTR name) {
  region_walk acc = {
    .regions = r,
    .root_name = name,
    .honor_private = 1,
  };

  sem_walk_regions(&acc, name);
}

// For each region list (include and exclude) we will first validate that we have
// a valid region then compute its transitive closure.  We're doing this here because
// as we encounter DDL elements we want to be able to make a quick in/out decision.
// Each element has the region string in its AST so by flatting the set here we can
// do a quick in/out test on the include an exclude list and get the right output.
// It also means that anything that can go wrong will go wrong right here;   After this
// all region names are known to be good.
static symtab *sem_accumulate_regions(int32_t count, char **regions) {
  symtab *result = symtab_new();

  for (int32_t i = 0; i < count; i++) {
    CSTR region = regions[i];
    if (!find_region(region)) {
      cql_error("invalid region specified '%s'\n", region);
      symtab_delete(result);
      cql_cleanup_and_exit(1);
    }

    sem_accumulate_full_region_image(result, region);
  }

  return result;
}

// To make the region filters, we have to compute the transitive closure of all the regions
// that were specified on the command line.  There are two such region lists and we process
// those lists here.
static void sem_setup_region_filters() {
  // reset these guys if they are already loaded
  SYMTAB_CLEANUP(included_regions);
  SYMTAB_CLEANUP(excluded_regions);

  if (options.include_regions_count) {
    included_regions = sem_accumulate_regions(
                          options.include_regions_count,
                          options.include_regions);
  }

  if (options.exclude_regions_count) {
    excluded_regions = sem_accumulate_regions(
                          options.exclude_regions_count,
                          options.exclude_regions);
  }
}

// Entering a schema region makes all the objects that follow part of that
// region.  It also means that all the contained objects must refer to
// only pieces of schema that are in the same region or a dependent region.
// Here we validate that region we are entering is in fact a valid region
// and that there isn't already a schema region.
static void sem_begin_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_begin_schema_region_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  // @BEGIN_SCHEMA_REGION name

  if (!verify_schema_region_out_of_proc(ast)) {
    record_error(ast);
    return;
  }

  if (current_region) {
    report_error(ast, "CQL0246: schema regions do not nest; end the current region before starting a new one", NULL);
    record_error(ast);
    return;
  }

  ast_node *region = find_region(name);
  if (!region) {
    report_error(ast->left, "CQL0244: unknown schema region", name);
    record_error(ast);
    return;
  }

  // Get the canonical name of the region (case adjusted)
  Contract(is_region(region));
  EXTRACT_STRING(region_name, region->left);

  // we already know we are not in a region
  Invariant(!current_region_image);
  current_region_image = symtab_new();
  sem_accumulate_public_region_image(current_region_image, region_name);

  // this is the one and only text pointer value for this region
  current_region = region_name;
  record_ok(ast);
}

// Leaving a schema region puts you back in the default region.
// Here we check that we are in a schema region.
static void sem_end_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_end_schema_region_stmt(ast));

  // @END_SCHEMA_REGION

  if (!verify_schema_region_out_of_proc(ast)) {
    record_error(ast);
    return;
  }

  if (!current_region) {
    report_error(ast, "CQL0247: you must begin a schema region before you can end one", NULL);
    record_error(ast);
    return;
  }

  // We are in a region, so there is for sure a region image
  Invariant(current_region_image);
  symtab_delete(current_region_image);
  current_region_image = NULL;
  current_region = NULL;
  record_ok(ast);
}

static void sem_validate_previous_ad_hoc(ast_node *prev, CSTR name, int32_t version) {
  Contract(is_ast_schema_ad_hoc_migration_stmt(prev));

  // The ad hoc migrates are not mixed with any other kind of migrate scripts in their symbol table, so:
  // 1. some other kind of migrate script can't be used to verify that this one exists
  // 2. for sure the type of the object stored in the symbol table is schema_ad_hoc_migration_stmt
  // Hence the extracts below are absolutely safe.

  ast_node *ast = find_ad_hoc_migrate(name);
  if (!ast) {
    report_error(prev, "CQL0286: ad hoc schema migration directive was removed; this is not allowed", name);
    record_error(prev);
    return;
  }

  EXTRACT_NOTNULL(schema_ad_hoc_migration_stmt, ast);
  EXTRACT_NAMED_NOTNULL(vers_annotation, version_annotation, schema_ad_hoc_migration_stmt->left);
  EXTRACT_OPTION(vers, vers_annotation->left);

  if (vers != version) {
    report_error(ast, "CQL0285: ad hoc schema migration directive version number changed", name);
    record_error(ast);
    record_error(prev);
    return;
  }

  enqueue_pending_region_validation(prev, ast, name);

  // we've checked this migration, it isn't new and it matches
  sem_add_flags(schema_ad_hoc_migration_stmt, SEM_TYPE_VALIDATED);
}

// This extracts the basic info from a sub/unsub directive and does minimal sanity check.
static void sem_subs_extract(ast_node *ast, subs_info *info) {
  Contract(is_ast_schema_unsub_stmt(ast) || is_ast_schema_resub_stmt(ast));
  CSTR directive = is_ast_schema_unsub_stmt(ast) ? "@unsub" : "@resub";

  EXTRACT_NOTNULL(version_annotation, ast->left);
  EXTRACT_OPTION(vers, version_annotation->left);

  if (vers < 1) {
    report_error(ast, "CQL0025: version number in annotation must be positive", NULL);
    record_error(ast);
    return;
  }

  if (!version_annotation->right) {
    CSTR msg = dup_printf("CQL0465: %s directive must provide a table or view name", directive);
    report_error(ast, msg, NULL);
    record_error(ast);
    return;
  }

  EXTRACT_STRING(name, version_annotation->right);

  info->vers = vers;
  info->name = name;
  info->target_ast = NULL;  // not part of basic extraction

  // don't clobber existing semantic info
  if (!ast->sem) {
    record_ok(ast);
  }
}

// These validations are common to both @unsub and @resub and they
// mainly check generic things like the version numbers are reasonable,
// the indicated table is a table, and stuff like that.  They need
// follow-up to ensure that the particular operation makes sense.
static void sem_subs_validate_common(ast_node *ast, subs_info *info) {
  Contract(is_ast_schema_unsub_stmt(ast) || is_ast_schema_resub_stmt(ast));

  sem_subs_extract(ast, info);
  if (is_error(ast)) {
    return;
  }

  ast_node *target = find_usable_table_or_view_even_deleted(
    info->name, ast, "CQL0466: the table/view named in an @unsub/@resub directive does not exist");

  if (!target) {
    record_error(ast);
    return;
  }

  if (info->vers < last_sub_version) {
    report_error(ast, "CQL0467: @unsub/@resub versions must be in non-decreasing order", NULL);
    record_error(ast);
    return;
  }

  if (target->sem->delete_version > 0 && target->sem->delete_version <= info->vers) {
    report_error(ast, "CQL0469: table/view is already deleted", info->name);
    record_error(ast);
    return;
  }

  if (!target->sem->recreate && target->sem->create_version >= info->vers) {
    report_error(ast, "CQL0470: table/view not yet created at indicated version", info->name);
    record_error(ast);
    return;
  }

  if (target->sem->unsub_version == info->vers || target->sem->resub_version == info->vers) {
    report_error(ast, "CQL0471: table/view has another @unsub/@resub at this version number", info->name);
    record_error(ast);
    return;
  }

  info->target_ast = target;

  record_ok(ast);
}

// find the next unsub/resub annotation that we recorded, it has to match what we just saw
static void validate_previous_schema_subscription_annotation(ast_node *previous_ast) {
  Contract(is_ast_schema_unsub_stmt(previous_ast) || is_ast_schema_resub_stmt(previous_ast));
  subs_info previous_info;

  sem_subs_extract(previous_ast, &previous_info);
  if (is_error(previous_ast)) {
    return;
  }

  // we accumulate the biggest previous schema number we've ever see during prevous schema validation
  if (previous_info.vers > max_previous_schema_version) {
    max_previous_schema_version = previous_info.vers;
  }

  // once we've found one delta against the subscription history we stop reporting errors
  if (found_subscription_error) {
    return;
  }

  if (!next_subscription) {
    found_subscription_error = true;
    report_error(previous_ast, "CQL0476: previous schema had more unsub/resub directives than the current schema", NULL);
    record_error(previous_ast);
    return;
  }

  // we advance in any case
  ast_node *found_ast = next_subscription->ast;
  next_subscription = next_subscription->next;

  Invariant(is_ast_schema_unsub_stmt(found_ast) || is_ast_schema_resub_stmt(found_ast));

  subs_info found_info;

  sem_subs_extract(found_ast, &found_info);
  Invariant(!is_error(found_ast)); // already checked

  // normal case, it is all matching
  if (!Strcasecmp(previous_info.name, found_info.name) &&
      previous_info.vers == found_info.vers &&
      previous_ast->type == found_ast->type) {
    // the previous item doesn't need any marking, it's all good already
    Invariant(!is_error(previous_ast));
    return;
  }

  // no more error checks regardless
  found_subscription_error = true;

  CSTR prev_directive = is_ast_schema_unsub_stmt(previous_ast) ? "@unsub" : "@resub";
  CSTR found_directive = is_ast_schema_unsub_stmt(found_ast) ? "@unsub" : "@resub";

  CSTR msg = dup_printf(
    "CQL0475: @unsub/@resub directives did not match between current and previous schema\n"
    "previous schema %s(%d, %s)\n"
    "current schema %s(%d, %s)",
      prev_directive, previous_info.vers, previous_info.name,
      found_directive, found_info.vers, found_info.name);

  report_error(previous_ast, msg, NULL);
  record_error(previous_ast);
  return;
}

static void sem_schema_unsub_stmt(ast_node *ast) {
  Contract(is_ast_schema_unsub_stmt(ast));

  if (validating_previous_schema) {
    validate_previous_schema_subscription_annotation(ast);
    return;
  }

  subs_info info;
  sem_subs_validate_common(ast, &info);
  if (is_error(ast)) {
    return;
  }

  int32_t vers = info.vers;
  ast_node *target = info.target_ast;
  CSTR name = info.name;
  Contract(target);

  if (target->sem->unsub_version > target->sem->resub_version) {
    report_error(ast, "CQL0472: table/view is already unsubscribed", name);
    record_error(ast);
    return;
  }

  bytebuf *buf = symtab_ensure_bytebuf(ref_sources_for_target_table, name);
  size_t ref_count = buf->used / sizeof(ast_node *);
  ast_node **sources = (ast_node **)buf->ptr;

  for (uint32_t i = 0; i < ref_count; i++) {
    ast_node *src_ast = sources[i];

    // note this checks for both @deleted and @unsub
    if (!is_deleted(src_ast)) {
      // we're not going to return; we keep generating as many errors as needed
      CSTR src_name = sem_get_name(src_ast);

      // carve out an exception for tables that refer to themselves, that FK won't break anything
      if (Strcasecmp(name, src_name)) {
        report_error(ast, "CQL0473: @unsub is invalid because the table/view is still used by", src_name);
        record_error(ast);
      }
    }
  }

  if (is_error(ast)) {
    return;
  }

  target->sem->unsub_version = vers;
  last_sub_version = vers;
  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->region = current_region;

  add_item_to_list(&all_subscriptions_list, ast);

  bool_t is_table = is_ast_create_table_stmt(target);
  bool_t is_recreate = target->sem->recreate;

  // recreate tables need no actions for unsubscription/resubscription
  // views likewise need no actions (they are always recreate)
  if (is_table && !is_recreate) {
    record_schema_annotation(vers, target, name, SCHEMA_ANNOTATION_UNSUB, NULL, ast->left, 0);
  }
}

static void sem_schema_resub_stmt(ast_node *ast) {
  Contract(is_ast_schema_resub_stmt(ast));

  if (validating_previous_schema) {
    validate_previous_schema_subscription_annotation(ast);
    return;
  }

  subs_info info;
  sem_subs_validate_common(ast, &info);
  if (is_error(ast)) {
    return;
  }

  int32_t vers = info.vers;
  ast_node *target = info.target_ast;
  CSTR name = info.name;
  Contract(target);

  if (target->sem->resub_version > target->sem->unsub_version) {
    report_error(ast, "CQL0472: table/view is already resubscribed", name);
    record_error(ast);
    return;
  }

  // This bit is tricky so it's worth a little explaination. The @resub is out of band with the
  // the table -- we'll see it after the create table regardless of version history.  That means
  // we could see such a directive on a table that's been marked with @delete.  Now the thing is
  // this could be ok.  You might create a table in v10, unsubscribe in v20, resubscribe in v30
  // and then finally delete the table in v40.  There were many versions where the resub made sense
  // and that resub comes after the create table statement in the input  for sure.  Now, when validating
  // resub we consider this table to be a "child" and ask the question "do all its parents exist?"
  // so that if you resub a table you must first resub all its parents.  The exception to this is
  // if the table becomes deleted where the parents are moot.  At one time the parents needed to exist
  // but now they could be unsubscribed or deleted and it doesn't matter because this child is
  // deleted.  So we only check the parent tables on undeleted children.  It's strange because
  // we're resubscribing to a deleted table here but remember in this example we would still have to
  // generate the create table at v30 so that the history is consistent even if the resub is effectively
  // cancelled later by a delete.  The weird thing is we saw the v40 delete before we saw the
  // v30 unsub because the unsubs are necessarily out of band with the table.  You could imagine
  // putting @unsub/@resub on the table but if you did that it would not be possible for schema
  // subscribers to @unsub without modifying the schema and the whole point is that *some*
  // subscribers might want to unsubscribe so the annotation can't go on the table.  Which means
  // we have to live with this out-of-order business.

  if (target->sem->delete_version < 0) {
    bytebuf *buf = symtab_ensure_bytebuf(ref_targets_for_source_table, name);
    size_t ref_count = buf->used / sizeof(ast_node *);
    ast_node **targets = (ast_node **)buf->ptr;

    for (uint32_t i = 0; i < ref_count; i++) {
      ast_node *ref_ast = targets[i];

      // note this checks for both @deleted and @unsub
      if (is_deleted(ref_ast)) {
        CSTR ref_name = sem_get_name(ref_ast);

        // carve out an exception for tables that refer to themselves, that FK won't break anything
        if (Strcasecmp(name, ref_name)) {
          // we're not going to return; we keep generating as many errors as needed
          report_error(ast, "CQL0474: @resub is invalid because the table/view references", ref_name);
          record_error(ast);
        }
      }
    }
  }

  if (is_error(ast)) {
    return;
  }

  target->sem->resub_version = vers;
  last_sub_version = vers;
  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->region = current_region;

  bool_t is_table = is_ast_create_table_stmt(target);
  bool_t is_recreate = target->sem->recreate;

  add_item_to_list(&all_subscriptions_list, ast);

  // recreate tables need no actions for unsubscription/resubscription
  // views are always recreate so they also need no actions.
  if (is_table && !is_recreate)  {
    record_schema_annotation(vers, target, name, SCHEMA_ANNOTATION_RESUB, NULL, ast->left, 0);
  }
}

static void sem_schema_ad_hoc_migration_stmt_for_version(ast_node *ast) {
  Contract(is_ast_schema_ad_hoc_migration_stmt(ast));
  EXTRACT_NOTNULL(version_annotation, ast->left);

  CSTR name = NULL;
  int32_t version = -1; // sentinel indicating it's not yet set

  if (!sem_validate_version(SCHEMA_ANNOTATION_AD_HOC, ast, &version, &name)) {
    record_error(ast);
    return;
  }

  if (!name) {
    report_error(ast, "CQL0284: ad hoc schema migration directive must provide a procedure to run", NULL);
    record_error(ast);
    return;
  }

  bool_t adding_current_entity = will_add_current_entity();

  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->region = current_region;
  ast->sem->create_version = version;

  if (validating_previous_schema) {
    sem_validate_previous_ad_hoc(ast, name, version);
  }
  else if (adding_current_entity) {
    add_item_to_list(&all_ad_hoc_list, ast);
    symtab_add(ad_hoc_migrates, name, ast);
    record_schema_annotation(version, ast, name, SCHEMA_ANNOTATION_AD_HOC, NULL, version_annotation, 0);
  }
}

// this is where you specify a procedure that should be run if you need to recreate a table or a table group
static void sem_schema_ad_hoc_migration_stmt_for_recreate(ast_node *ast) {
  Contract(is_ast_schema_ad_hoc_migration_stmt(ast));
  EXTRACT_STRING(group, ast->left);
  EXTRACT_STRING(proc, ast->right);

  bool_t adding_current_entity = will_add_current_entity();

  ast->sem = new_sem(SEM_TYPE_OK);
  ast->sem->region = current_region;

  // there is no previous schema validation for these guys, you can add and remove them as you please

  if (adding_current_entity) {
    if (!symtab_add(ad_hoc_recreate_actions, group, ast)) {
       report_error(ast, "CQL0176: indicated procedure or group already has a recreate action", group);
       record_error(ast);
       return;
    }
  }

  if (!sem_create_migration_proc_prototype(ast, proc)) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}

static void sem_schema_ad_hoc_migration_stmt(ast_node *ast) {
  Contract(is_ast_schema_ad_hoc_migration_stmt(ast));
  if (ast->right) {
    sem_schema_ad_hoc_migration_stmt_for_recreate(ast);
  }
  else {
    sem_schema_ad_hoc_migration_stmt_for_version(ast);
  }
}

static void enqueue_pending_region_validation(ast_node *prev, ast_node *cur, CSTR name) {
  // we're processing the previous item when we enqueue, if that item has no region
  // then it is allowed to make any change it wants to.
  if (!current_region) {
    // no need to enqueue a check
    return;
  }

  deployable_validation *v = bytebuf_new(deployable_validations, deployable_validation);

  v->prev = prev;
  v->cur = cur;
  v->prev_region = current_region;  // recall we are processing previous items (see above)
  v->cur_region = cur->sem->region ? cur->sem->region : "(none)";
  v->name = name;
}

// Given some schema object (any kind of ast) look to see if it has a region
// if does have a deployment region then make sure it didn't change.
static void sem_validate_previous_deployable_region(ast_node *root, deployable_validation *v) {
  // if there was no previous deployment region you can acquire one
  CSTR prev_region = v->prev_region;
  ast_node *cur = v->cur;
  ast_node *prev = v->prev;
  CSTR  name = v->name;

  // sem error will be missing useful state, no need to look at this and no need to pile
  // on more errors, whatever it is already has errors reported against it
  Contract(!is_error(cur));
  Contract(!is_error(prev));

  // null previous region is not enqueued, there's nothing to validate
  Invariant(prev_region);
  ast_node *prev_reg = find_region(prev_region);

  // Importantly, the entire reason we have to enqueue these is because this
  // check right here cannot be done on the previous region at the time we find the
  // item we are checking.  The deployment region (DR) that contains this region (R) may not
  // have been declared before things were put into (R).  So the check has to happen at the end.

  if (prev_reg->sem->region) {
    // recall that regions themselves have a region tag which is the deployable region they are in
    CSTR prev_deployment_region = prev_reg->sem->region;

    // now get the current deployment region or "(none)" which can match nothing (invalid name)
    CSTR cur_deployment_region = "(none)";
    if (cur->sem->region) {
      ast_node *cur_reg = find_cur_region(cur->sem->region);
      if (cur_reg->sem->region) {
        cur_deployment_region = cur_reg->sem->region;
      }
    }

    if (strcmp(cur_deployment_region, prev_deployment_region)) {
      CHARBUF_OPEN(msg);
      bprintf(&msg,
              "CQL0311: object's deployment region changed from '%s' to '%s'",
              prev_deployment_region,
              cur_deployment_region);

      report_and_capture_error(root, cur, msg.ptr, name);
      record_error(prev);
      CHARBUF_CLOSE(msg);
    }
  }
}

// At this point everything is all queued up and ready to go, just run through the pending
// validations and do them.
static void sem_validate_all_deployable_regions(ast_node *root) {
  uint32_t count = deployable_validations->used / sizeof(deployable_validation);

  deployable_validation *validations = (deployable_validation *)deployable_validations->ptr;

  for (int32_t i = 0; i < count; i++) {
    deployable_validation *v = &validations[i];

    // don't pile on more errors...
    if (!is_error(v->prev) && !is_error(v->cur)) {
      sem_validate_previous_deployable_region(root, v);
    }
  }
}

static void sem_emit_enums_stmt(ast_node *ast) {
  Contract(is_ast_emit_enums_stmt(ast));
  EXTRACT(name_list, ast->left);

  while (name_list) {
    EXTRACT_ANY_NOTNULL(name_ast, name_list->left);
    EXTRACT_STRING(name, name_ast);

    if (!find_enum(name)) {
      report_error(name_ast, "CQL0169: enum not found", name);
      record_error(ast);
      return;
    }

    name_list = name_list->right;
  }

  record_ok(ast);
}

static void sem_emit_constants_stmt(ast_node *ast) {
  Contract(is_ast_emit_constants_stmt(ast));
  EXTRACT_NOTNULL(name_list, ast->left);

  while (name_list) {
    EXTRACT_ANY_NOTNULL(name_ast, name_list->left);
    EXTRACT_STRING(name, name_ast);

    if (!find_constant_group(name)) {
      report_error(name_ast, "CQL0169: constant group not found", name);
      record_error(ast);
      return;
    }

    name_list = name_list->right;
  }

  record_ok(ast);
}

// The blob state functions affect codegen only, there is nothing that can go wrong with them
// during semantic analysis so they all just mark the result ok.  During codegen we will keep
// track of the currently mapped items and make them available to sql echo so that the
// appropriate replacements can be made

// always good to go, see above
static void sem_blob_get_key_type_stmt(ast_node *ast) {
  Contract(is_ast_blob_get_key_type_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_get_val_type_stmt(ast_node *ast) {
  Contract(is_ast_blob_get_val_type_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_get_key_stmt(ast_node *ast) {
  Contract(is_ast_blob_get_key_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_get_val_stmt(ast_node *ast) {
  Contract(is_ast_blob_get_val_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_create_key_stmt(ast_node *ast) {
  Contract(is_ast_blob_create_key_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_create_val_stmt(ast_node *ast) {
  Contract(is_ast_blob_create_val_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_update_key_stmt(ast_node *ast) {
  Contract(is_ast_blob_update_key_stmt(ast));
  record_ok(ast);
}

// always good to go, see above
static void sem_blob_update_val_stmt(ast_node *ast) {
  Contract(is_ast_blob_update_val_stmt(ast));
  record_ok(ast);
}

// Most codegen types are not compatible with previous schema generation because it adds stuff to the AST
// and that stuff isn't even fully type evaluated.  So the best thing to do is punt on codegen if we
// did that sort of validation.
cql_noexport void exit_on_validating_schema() {
  if (validating_previous_schema) {
    cql_error("This code generation mode is not compatible with @previous_schema validation mode.\n");
    cql_cleanup_and_exit(1);
  }
}

#undef STMT_INIT
#define STMT_INIT(x) symtab_add(syms, k_ast_ ## x, (void *)sem_ ## x)

#undef FUNC_INIT
#define FUNC_INIT(x) symtab_add(builtin_funcs, #x, (void *)sem_func_ ## x)

// A special function is one whose arguments require special treatment during
// semantic analysis, for whatever reason. The procedure that does the analysis
// (of type `sem_special_func`) must therefore be sure to analyze its argument
// list appropriately: It will not be done beforehand as it is with functions
// registered via `FUNC_INIT`. It must also indicate if it is an aggregate
// function so that `sem_expr_call` knows how to handle it.
#undef SPECIAL_FUNC_INIT
#define SPECIAL_FUNC_INIT(x) symtab_add(builtin_special_funcs, #x, (void *)sem_special_func_ ## x)

#undef AGGR_FUNC_INIT
#define AGGR_FUNC_INIT(x) symtab_add(builtin_aggregated_funcs, #x, (void *)sem_aggr_func_ ## x)

#undef EXPR_INIT
#define EXPR_INIT(x, func, str) \
  static sem_expr_dispatch expr_disp_ ## x = { func, str }; \
  symtab_add(exprs, k_ast_ ## x, (void *)&expr_disp_ ## x);

#undef MISC_ATTR_INIT
#define MISC_ATTR_INIT(x) symtab_add(misc_attributes, #x, (void *)sem_misc_attrs_ ## x)

// This method loads up the global symbol tables in either empty state or
// with the appropriate tokens ready to go.  Using our own symbol tables for
// dispatch saves us a lot of if/else string comparison verbosity.
cql_noexport void sem_main(ast_node *ast) {
  // restore all globals and statics we own
  sem_cleanup();
  eval_init();

  AST_REWRITE_INFO_START();

  exprs = symtab_new();
  builtin_funcs = symtab_new();
  builtin_special_funcs = symtab_new();
  funcs = symtab_new();
  interfaces = symtab_new();
  unchecked_funcs = symtab_new();
  procs = symtab_new();
  unchecked_procs = symtab_new();
  proc_arg_info = symtab_new();
  enums = symtab_new();
  triggers = symtab_new();
  upgrade_procs = symtab_new();
  ad_hoc_migrates = symtab_new();
  tables = symtab_new();
  indices = symtab_new();
  globals = symtab_new();
  constant_groups = symtab_new();
  variable_groups = symtab_new();
  constants = symtab_new();
  current_variables = globals;
  savepoints = symtab_new();
  schema_regions = symtab_new();
  non_sql_stmts = symtab_new();
  sql_stmts = symtab_new();
  base_fragments = symtab_new();
  extension_fragments = symtab_new();
  assembly_fragments = symtab_new();
  extensions_by_basename = symtab_new();
  ref_sources_for_target_table = symtab_new();
  ref_targets_for_source_table = symtab_new();
  builtin_aggregated_funcs = symtab_new();
  global_types = symtab_new();
  misc_attributes = symtab_new();
  ad_hoc_recreate_actions = symtab_new();

  schema_annotations = _ast_pool_new(bytebuf);
  recreate_annotations = _ast_pool_new(bytebuf);
  bytebuf_open(schema_annotations);
  bytebuf_open(recreate_annotations);

  Invariant(cte_cur == NULL);

  symtab *syms = non_sql_stmts;

  STMT_INIT(if_stmt);
  STMT_INIT(guard_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(switch_stmt);
  STMT_INIT(leave_stmt);
  STMT_INIT(continue_stmt);
  STMT_INIT(return_stmt);
  STMT_INIT(rollback_return_stmt);
  STMT_INIT(commit_return_stmt);
  STMT_INIT(call_stmt);
  STMT_INIT(declare_out_call_stmt);
  STMT_INIT(declare_vars_type);
  STMT_INIT(let_stmt);
  STMT_INIT(assign);
  STMT_INIT(set_from_cursor);
  STMT_INIT(misc_attrs);
  STMT_INIT(create_proc_stmt);
  STMT_INIT(declare_enum_stmt);
  STMT_INIT(declare_const_stmt);
  STMT_INIT(declare_group_stmt);
  STMT_INIT(declare_interface_stmt);
  STMT_INIT(declare_proc_stmt);
  STMT_INIT(declare_proc_no_check_stmt);
  STMT_INIT(declare_func_stmt);
  STMT_INIT(declare_select_func_stmt);
  STMT_INIT(declare_select_func_no_check_stmt);
  STMT_INIT(echo_stmt);
  STMT_INIT(schema_upgrade_version_stmt);
  STMT_INIT(schema_upgrade_script_stmt);
  STMT_INIT(previous_schema_stmt);
  STMT_INIT(fetch_values_stmt);
  STMT_INIT(declare_cursor_like_name);
  STMT_INIT(declare_cursor_like_select);
  STMT_INIT(declare_cursor_like_typed_names);
  STMT_INIT(declare_value_cursor);
  STMT_INIT(declare_cursor);
  STMT_INIT(declare_named_type);
  STMT_INIT(out_stmt);
  STMT_INIT(out_union_stmt);
  STMT_INIT(out_union_parent_child_stmt);

  syms = sql_stmts;

  STMT_INIT(trycatch_stmt);
  STMT_INIT(throw_stmt);
  STMT_INIT(create_table_stmt);
  STMT_INIT(create_virtual_table_stmt);
  STMT_INIT(create_trigger_stmt);
  STMT_INIT(drop_table_stmt);
  STMT_INIT(drop_view_stmt);
  STMT_INIT(drop_index_stmt);
  STMT_INIT(drop_trigger_stmt);
  STMT_INIT(fetch_cursor_from_blob_stmt);
  STMT_INIT(set_blob_from_cursor_stmt);
  STMT_INIT(alter_table_add_column_stmt);
  STMT_INIT(create_index_stmt);
  STMT_INIT(create_view_stmt);
  STMT_INIT(explain_stmt);
  STMT_INIT(select_stmt);
  STMT_INIT(with_select_stmt);
  STMT_INIT(delete_stmt);
  STMT_INIT(with_delete_stmt);
  STMT_INIT(update_stmt);
  STMT_INIT(update_cursor_stmt);
  STMT_INIT(with_update_stmt);
  STMT_INIT(insert_stmt);
  STMT_INIT(with_insert_stmt);
  STMT_INIT(upsert_stmt);
  STMT_INIT(with_upsert_stmt);
  STMT_INIT(loop_stmt);
  STMT_INIT(fetch_stmt);
  STMT_INIT(fetch_call_stmt);
  STMT_INIT(begin_trans_stmt);
  STMT_INIT(commit_trans_stmt);
  STMT_INIT(rollback_trans_stmt);
  STMT_INIT(savepoint_stmt);
  STMT_INIT(release_savepoint_stmt);
  STMT_INIT(close_stmt);
  STMT_INIT(enforce_normal_stmt);
  STMT_INIT(enforce_strict_stmt);
  STMT_INIT(enforce_reset_stmt);
  STMT_INIT(enforce_push_stmt);
  STMT_INIT(enforce_pop_stmt);
  STMT_INIT(declare_schema_region_stmt);
  STMT_INIT(declare_deployable_region_stmt);
  STMT_INIT(begin_schema_region_stmt);
  STMT_INIT(end_schema_region_stmt);
  STMT_INIT(schema_ad_hoc_migration_stmt);
  STMT_INIT(schema_unsub_stmt);
  STMT_INIT(schema_resub_stmt);
  STMT_INIT(proc_savepoint_stmt);
  STMT_INIT(emit_enums_stmt);
  STMT_INIT(emit_group_stmt);
  STMT_INIT(emit_constants_stmt);

  STMT_INIT(blob_get_key_type_stmt);
  STMT_INIT(blob_get_val_type_stmt);
  STMT_INIT(blob_get_key_stmt);
  STMT_INIT(blob_get_val_stmt);
  STMT_INIT(blob_create_key_stmt);
  STMT_INIT(blob_create_val_stmt);
  STMT_INIT(blob_update_key_stmt);
  STMT_INIT(blob_update_val_stmt);

  AGGR_FUNC_INIT(max);
  AGGR_FUNC_INIT(min);
  AGGR_FUNC_INIT(sum);
  AGGR_FUNC_INIT(total);
  AGGR_FUNC_INIT(avg);
  AGGR_FUNC_INIT(group_concat);

  FUNC_INIT(ifnull);
  FUNC_INIT(nullif);
  FUNC_INIT(upper);
  FUNC_INIT(lower);
  FUNC_INIT(cql_cursor_diff_col);
  FUNC_INIT(cql_cursor_diff_val);
  FUNC_INIT(char);
  FUNC_INIT(sign);
  FUNC_INIT(abs);
  FUNC_INIT(round);
  FUNC_INIT(instr);
  FUNC_INIT(coalesce);
  FUNC_INIT(last_insert_rowid);
  FUNC_INIT(changes);
  FUNC_INIT(printf);
  FUNC_INIT(strftime);
  FUNC_INIT(date);
  FUNC_INIT(time);
  FUNC_INIT(datetime);
  FUNC_INIT(julianday);
  FUNC_INIT(ifnull_crash);
  FUNC_INIT(ifnull_throw);
  FUNC_INIT(nullable);
  FUNC_INIT(sensitive);
  FUNC_INIT(substr);
  FUNC_INIT(replace);
  FUNC_INIT(row_number);
  FUNC_INIT(rank);
  FUNC_INIT(dense_rank);
  FUNC_INIT(percent_rank);
  FUNC_INIT(cume_dist);
  FUNC_INIT(ntile);
  FUNC_INIT(lag);
  FUNC_INIT(lead);
  FUNC_INIT(first_value);
  FUNC_INIT(last_value);
  FUNC_INIT(nth_value);
  FUNC_INIT(random);
  FUNC_INIT(likely);
  FUNC_INIT(cql_compressed);

  FUNC_INIT(trim);
  FUNC_INIT(ltrim);
  FUNC_INIT(rtrim);
  FUNC_INIT(length);

  FUNC_INIT(cql_get_blob_size);

  SPECIAL_FUNC_INIT(count);
  SPECIAL_FUNC_INIT(iif);
  SPECIAL_FUNC_INIT(ptr);
  SPECIAL_FUNC_INIT(cql_inferred_notnull);
  SPECIAL_FUNC_INIT(cql_blob_get);

  EXPR_INIT(num, sem_expr_num, "NUM");
  EXPR_INIT(str, sem_expr_str, "STR");
  EXPR_INIT(blob, sem_expr_blob, "BLB");
  EXPR_INIT(null, sem_expr_null, "NULL");
  EXPR_INIT(dot, sem_expr_dot, "DOT");
  EXPR_INIT(const, sem_expr_const, "CONST");
  EXPR_INIT(lshift, sem_binary_integer_math, "<<");
  EXPR_INIT(rshift, sem_binary_integer_math, "<<");
  EXPR_INIT(bin_and, sem_binary_integer_math, "&");
  EXPR_INIT(bin_or, sem_binary_integer_math, "|");
  EXPR_INIT(collate, sem_collate, "COLLATE");
  EXPR_INIT(mul, sem_binary_math, "*");
  EXPR_INIT(div, sem_binary_math, "/");
  EXPR_INIT(mod, sem_binary_integer_math, "%");
  EXPR_INIT(add, sem_binary_math, "+");
  EXPR_INIT(sub, sem_binary_math, "-");
  EXPR_INIT(not, sem_unary_logical, "NOT");
  EXPR_INIT(is_not_true, sem_unary_is_true_or_false, "IS NOT TRUE");
  EXPR_INIT(is_not_false, sem_unary_is_true_or_false, "IS NOT FALSE");
  EXPR_INIT(is_true, sem_unary_is_true_or_false, "IS TRUE");
  EXPR_INIT(is_false, sem_unary_is_true_or_false, "IS FALSE");
  EXPR_INIT(tilde, sem_unary_integer_math, "~");
  EXPR_INIT(uminus, sem_unary_math, "-");
  EXPR_INIT(eq, sem_binary_eq_or_ne, "=");
  EXPR_INIT(lt, sem_binary_compare, "<");
  EXPR_INIT(gt, sem_binary_compare, ">");
  EXPR_INIT(ne, sem_binary_eq_or_ne, "<>");
  EXPR_INIT(ge, sem_binary_compare, ">=");
  EXPR_INIT(le, sem_binary_compare, "<=");
  EXPR_INIT(call, sem_expr_call, "CALL");
  EXPR_INIT(window_func_inv, sem_expr_window_func_inv, "WINDOW-FUNC-INV");
  EXPR_INIT(raise, sem_expr_raise, "RAISE");
  EXPR_INIT(exists_expr, sem_expr_exists, "EXISTS");
  EXPR_INIT(between, sem_expr_between_or_not_between, "BETWEEN");
  EXPR_INIT(not_between, sem_expr_between_or_not_between, "NOT BETWEEN");
  EXPR_INIT(between_rewrite, sem_expr_between_rewrite, "BETWEEN REWRITE");
  EXPR_INIT(and, sem_binary_logical, "AND");
  EXPR_INIT(or, sem_binary_logical, "OR");
  EXPR_INIT(select_stmt, sem_expr_select, "SELECT");
  EXPR_INIT(select_if_nothing_throw_expr, sem_expr_select_if_nothing_throw, "IF NOTHING THROW");
  EXPR_INIT(select_if_nothing_expr, sem_expr_select_if_nothing, "IF NOTHING");
  EXPR_INIT(select_if_nothing_or_null_expr, sem_expr_select_if_nothing, "IF NOTHING OR NULL");
  EXPR_INIT(with_select_stmt, sem_expr_select, "WITH...SELECT");
  EXPR_INIT(is, sem_binary_is_or_is_not, "IS");
  EXPR_INIT(is_not, sem_binary_is_or_is_not, "IS NOT");
  EXPR_INIT(like, sem_binary_like, "LIKE");
  EXPR_INIT(not_like, sem_binary_like, "NOT LIKE");
  EXPR_INIT(match, sem_binary_match, "MATCH");
  EXPR_INIT(not_match, sem_binary_match, "NOT MATCH");
  EXPR_INIT(regexp, sem_binary_match, "REGEXP");
  EXPR_INIT(not_regexp, sem_binary_match, "NOT REGEXP");
  EXPR_INIT(glob, sem_binary_match, "GLOB");
  EXPR_INIT(not_glob, sem_binary_match, "NOT GLOB");
  EXPR_INIT(in_pred, sem_expr_in_pred_or_not_in, "IN");
  EXPR_INIT(not_in, sem_expr_in_pred_or_not_in, "NOT IN");
  EXPR_INIT(cast_expr, sem_expr_cast, "CAST");
  EXPR_INIT(case_expr, sem_expr_case, "CASE");
  EXPR_INIT(concat, sem_concat, "||");

  MISC_ATTR_INIT(ok_table_scan);
  MISC_ATTR_INIT(no_table_scan);
  MISC_ATTR_INIT(vault_sensitive);

  if (ast) {
    sem_stmt_list(ast);
  }

  Invariant(cte_cur == NULL);

  // put tables/views/etc into the natural order (the order declared)
  reverse_list(&all_tables_list);
  reverse_list(&all_functions_list);
  reverse_list(&all_views_list);
  reverse_list(&all_indices_list);
  reverse_list(&all_triggers_list);
  reverse_list(&all_regions_list);
  reverse_list(&all_ad_hoc_list);
  reverse_list(&all_enums_list);
  reverse_list(&all_select_functions_list);

  // We want the list in order when validating previous schema
  // to help us to do the validations, so we don't need to reverse it again
  // but if we never hit @previous_schmea then reverse it now.
  if (!validating_previous_schema) {
    reverse_list(&all_subscriptions_list);
  }

  // the index list in any given table needs to be reversed to get the natural order
  for (list_item *item = all_tables_list; item; item = item->next) {
    ast_node *table = item->ast;
    Invariant(ast);

    if (table->sem && table->sem->index_list) {
      reverse_list(&table->sem->index_list);
    }
  }

  if (validating_previous_schema) {
    reverse_list(&all_prev_recreate_tables);
    sem_validate_all_tables_not_in_previous(ast);
    sem_validate_all_columns_not_in_previous(ast);
    sem_validate_all_prev_recreate_tables(ast);
    sem_validate_all_ad_hoc_not_in_previous(ast);
    sem_validate_all_deployable_regions(ast);
    sem_validate_all_subscriptions_not_in_previous(ast);
  }

  if (validating_previous_schema) {
    // In case there is any futher processing, the region symbol table
    // used during previous schema procesing is now useless, put it back
    // to the normal  one.
    symtab_delete(schema_regions);  // the regions during previous schema processing
    schema_regions = new_regions;   // the original regions
    new_regions = NULL;             // nobody should be looking here anymore

    symtab_delete(enums);  // the enums during previous schema processing
    enums = new_enums;     // the original enums
    new_enums = NULL;      // nobody should be looking here anymore
  }

  AST_REWRITE_INFO_END();

  // in case later passes need the regions resolved
  sem_setup_region_filters();
}

// This method frees all the global state of the semantic analyzer
cql_noexport void sem_cleanup() {
  eval_cleanup();

  BYTEBUF_CLEANUP(deployable_validations);
  BYTEBUF_CLEANUP(recreate_annotations);
  BYTEBUF_CLEANUP(schema_annotations);
  BYTEBUF_CLEANUP(unitary_locals);

  SYMTAB_CLEANUP(ad_hoc_migrates);
  SYMTAB_CLEANUP(extensions_by_basename);
  SYMTAB_CLEANUP(ref_sources_for_target_table);
  SYMTAB_CLEANUP(ref_targets_for_source_table);
  SYMTAB_CLEANUP(assembly_fragments);
  SYMTAB_CLEANUP(base_fragments);
  SYMTAB_CLEANUP(builtin_funcs);
  SYMTAB_CLEANUP(builtin_special_funcs)
  SYMTAB_CLEANUP(current_region_image);
  SYMTAB_CLEANUP(exprs);
  SYMTAB_CLEANUP(extension_fragments);
  SYMTAB_CLEANUP(funcs);
  SYMTAB_CLEANUP(unchecked_funcs);
  SYMTAB_CLEANUP(globals);
  SYMTAB_CLEANUP(indices);
  SYMTAB_CLEANUP(locals);
  SYMTAB_CLEANUP(constant_groups);
  SYMTAB_CLEANUP(variable_groups);
  SYMTAB_CLEANUP(constants);
  SYMTAB_CLEANUP(monitor_symtab );
  SYMTAB_CLEANUP(new_regions);
  SYMTAB_CLEANUP(new_enums);
  SYMTAB_CLEANUP(non_sql_stmts);
  SYMTAB_CLEANUP(procs);
  SYMTAB_CLEANUP(unchecked_procs);
  SYMTAB_CLEANUP(interfaces);
  SYMTAB_CLEANUP(proc_arg_info);
  SYMTAB_CLEANUP(enums);
  SYMTAB_CLEANUP(schema_regions);
  SYMTAB_CLEANUP(savepoints);
  SYMTAB_CLEANUP(sql_stmts);
  SYMTAB_CLEANUP(table_items);
  SYMTAB_CLEANUP(tables);
  SYMTAB_CLEANUP(triggers);
  SYMTAB_CLEANUP(upgrade_procs);
  SYMTAB_CLEANUP(builtin_aggregated_funcs);
  SYMTAB_CLEANUP(included_regions);
  SYMTAB_CLEANUP(excluded_regions);
  SYMTAB_CLEANUP(global_types);
  SYMTAB_CLEANUP(misc_attributes);
  SYMTAB_CLEANUP(ad_hoc_recreate_actions);

  // these are getting zeroed so that leaksanitizer will not count those objects as reachable from a global root.

  all_ad_hoc_list = NULL;
  all_functions_list = NULL;
  all_indices_list = NULL;
  all_prev_recreate_tables = NULL;
  all_regions_list = NULL;
  all_select_functions_list = NULL;
  all_tables_list = NULL;
  all_subscriptions_list = NULL;
  next_subscription = NULL;
  found_subscription_error = false;
  all_triggers_list = NULL;
  all_views_list = NULL;
  created_columns = NULL;
  cte_cur = NULL;
  current_explain_stmt = NULL;
  current_expr_context = SEM_EXPR_CONTEXT_NONE;
  current_joinscope = NULL;
  current_region = NULL;
  current_upsert_table_ast = NULL;
  current_variables = NULL;  // this is either locals or globals, freed above
  has_dml = false;
  in_shared_fragment_call = false;
  in_trigger = false;
  in_switch = false;
  in_upsert = false;
  loop_depth = 0;
  last_sub_version = 1;
  in_proc_savepoint = false;
  max_previous_schema_version = -1;
  reset_enforcements();
  enforcement_stack = NULL; // all the stack entries are in the ast pool, nothing to free
  monitor_jptr = NULL;
  recreates = 0;
  schema_upgrade_script = false;
  schema_upgrade_version = -1;
  select_level = 0;
  sem_stmt_level = -1;
  sem_ok = NULL;
  validating_previous_schema = false;
  between_count = 0;
  pending_table_validations_head = NULL;
  current_table_name = NULL;
  current_table_ast = NULL;
  local_types = NULL;
  is_analyzing_notnull_rewrite = false;
  global_notnull_improvements = NULL;
  current_loop_analysis_state = LOOP_ANALYSIS_STATE_NONE;
  current_proc_contains_try_is_proc_body = false;
}

#endif

// When validating against the previous schema we need to make sure all these items
// have been validated.  Any that are found in the old schema must match appropriately
// and any that are not found must have suitable @create markers.
cql_data_defn( list_item *all_tables_list );
cql_data_defn( list_item *all_subscriptions_list );
cql_data_defn( list_item *all_functions_list );
cql_data_defn( list_item *all_views_list );
cql_data_defn( list_item *all_indices_list );
cql_data_defn( list_item *all_triggers_list );
cql_data_defn( list_item *all_regions_list );
cql_data_defn( list_item *all_ad_hoc_list );
cql_data_defn( list_item *all_select_functions_list );
cql_data_defn( list_item *all_enums_list );
cql_data_defn( list_item *all_constant_groups_list );
cql_data_defn( bool_t use_encode );
cql_data_defn( CSTR _Nullable encode_context_column );
cql_data_defn( symtab *encode_columns );

// If creating debug/test output, we will hold errors for a given statement in this buffer.
cql_data_defn( charbuf *error_capture );

// The flags for the global proc (which has no ast node) this is what captures loose statements
cql_data_defn( sem_t global_proc_flags );

// If there is a current proc, it's root ast.
cql_data_defn(ast_node *current_proc);

cql_data_defn( symtab *schema_regions );

// These are the symbol tables with the accumulated included/excluded regions
cql_data_defn( symtab *included_regions );
cql_data_defn( symtab *excluded_regions );

// all the schema annotations
cql_data_defn( bytebuf *schema_annotations );
cql_data_defn( bytebuf *recreate_annotations );

// any table or group can have an action
cql_data_defn( symtab *ad_hoc_recreate_actions );

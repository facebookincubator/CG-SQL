/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "ast.h"
#include "charbuf.h"
#include "cql.h"
#include "sem.h"
#include "symtab.h"
#include "crc64xz.h"

// When emitting code for a sql statement you might need to prepare it expecting
// to use the sql statement yourself, or you may just want to run the SQL.
// CG_PREPARE indicates a result is expected.
#define CG_PREPARE 0            // so we can be explicit about not 1
#define CG_EXEC 1
#define CG_MINIFY_ALIASES 2
#define CG_NO_MINIFY_ALIASES 0  // so we can be explicit about not 2

// currently used only in DASM but of interest to all the code generators in the future
#define CG_EMIT_ONLY_BINDING 4  // assume the statement is ready to be bound, emit only the bindings
#define CG_EMIT_ONLY_PREPARE 8  // emit only the prepare, omitting the bindings which will be done later using the above

// To understand the PUSH/POP eval macros, and generally all the expression state
// macros you have to understand the overall theory of operation of the expression
// code generation.  In a traditional evaluation you could walk the tree and assemble
// the net expression just like we do when we produce SQL in the gen_* walk.  However,
// this doesn't work for SQL->C transpilation because of nullable types and compound
// expressions.
//   * Nullable types
//     The problem here is that the nullable has an is_null and an value field
//     in order to represent the possibily null state of something like a bool.
//     There is no one expression type that can hold all of that and we can't do
//     things like multiply a struct or otherwise accumulate expression state.
//     To solve this problem we keep a stack of local variables with intermediate
//     nullable results.  The result of evaluation is then TWO strings, one of which
//     is a string to get the value of the current expression and one of which is
//     a similar string to find out if that expression is null.  When a temporary is used
//     code generation will emit the necessary code to load the temporary variable
//     and then produce "variable.is_null" and "variable.value" as the pieces.
//   * Compound expressions
//     Many expressions cannot be evaluated without control flow logic.  The simplest
//     example is logical AND with short-circuit.  Knowing that the AND, and even its
//     fragements might need logic emitted you have to take the same approach --
//     you allocate a scratch variable to hold the answer, then compute it.  The same
//     approach lets you resole CASE/WHEN, IN, and BETWEEN, among others.  All of
//     these might require temporary variables and control flow.
// With the above general approach in mind, it becomes possible to keep building up
// subexpressions as long as nothing forces you to spill to locals.  So you can do
// 1+2*3 and so forth all you like and keep getting a nice simple string.  This gives
// you the best combination of simple, readable output with correct SQL semantics.
//
// The upshot of all this is that everything in sight gets an is_null and value
// buffer to write into.  Those pieces are then used to assemble any necessary evaluation.
//
// The pri argument tells the callee the context you intend to use the result.
// For instance if the result of this expression evaluation is going to be used
// as the left size of == you would pass in EXPR_PRI_COMP.  This tells the evaluator
// if the left side has binding strength weaker then == it must add parens because
// it will be used in the context of == by its caller.

#define CG_PUSH_MAIN_INDENT(tag, indent) \
CHARBUF_OPEN(tag##_buf); \
charbuf *tag##_main_saved = cg_main_output; \
int32_t tag##_indent = indent; \
cg_main_output = &tag##_buf; \

#define CG_POP_MAIN_INDENT(tag) \
cg_main_output = tag##_main_saved; \
bindent(cg_main_output, &tag##_buf, tag##_indent); \
CHARBUF_CLOSE(tag##_buf);

// Make a temporary buffer for the evaluation results using the canonical
// naming convention.  This might exit having burned some stack slots
// for its result variables, that's normal.
#define CG_PUSH_EVAL(expr, pri) \
CHARBUF_OPEN(expr##_is_null); \
CHARBUF_OPEN(expr##_value); \
cg_expr(expr, &expr##_is_null, &expr##_value, pri);

// Close the buffers used for the above.
// The scratch stack is not restored so that any temporaries used in
// the evaluation of expr will not be re-used prematurely.  They
// can't be used again until either the expression is finished,
// or they have been captured in a less-nested result variable.
#define CG_POP_EVAL(expr) \
CHARBUF_CLOSE(expr##_value); \
CHARBUF_CLOSE(expr##_is_null);

// Create buffers for a temporary variable.  Use cg_scratch_var to fill in the buffers
// with the text needed to refer to the variable.  cg_scratch_var picks the name
// based on stack level-and type.
#define CG_PUSH_TEMP(name, sem_type) \
CHARBUF_OPEN(name); \
CHARBUF_OPEN(name##_is_null); \
CHARBUF_OPEN(name##_value); \
cg_scratch_var(NULL, sem_type, &name, &name##_is_null, &name##_value); \
stack_level++;

// Release the buffers for the temporary, restore the stack level.
#define CG_POP_TEMP(name) \
CHARBUF_CLOSE(name##_value); \
CHARBUF_CLOSE(name##_is_null); \
CHARBUF_CLOSE(name); \
stack_level--;

// Make a scratch variable to hold the final result of an evaluation.
// It may or may not be used.  It should be the first thing you put
// so that it is on the top of your stack.  This only saves the slot.
// If you use this variable you can reclaim other temporaries that come
// from deeper in the tree since they will no longer be needed.
#define CG_RESERVE_RESULT_VAR(ast, sem_type) \
int32_t stack_level_reserved = stack_level; \
sem_t sem_type_reserved = sem_type; \
ast_node *ast_reserved = ast; \
CHARBUF_OPEN(result_var); \
CHARBUF_OPEN(result_var_is_null); \
CHARBUF_OPEN(result_var_value); \
stack_level++;

// If the result variable is going to be used, this writes its name
// and .value and .is_null into the is_null and value fields.
#define CG_USE_RESULT_VAR() \
int32_t stack_level_now = stack_level; \
stack_level = stack_level_reserved; \
cg_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_is_null, &result_var_value); \
stack_level = stack_level_now; \
Invariant(result_var.used > 1); \
bprintf(is_null, "%s", result_var_is_null.ptr); \
bprintf(value, "%s", result_var_value.ptr)

// Release the buffer holding the name of the variable.
// If the result variable was used, we can re-use any temporaries
// with a bigger number.  They're no longer needed since they
// are captured in this result.  We know it was used if it
// has .used > 1 (there is always a trailing null so empty is 1).
#define CG_CLEANUP_RESULT_VAR() \
if (result_var.used > 1) stack_level = stack_level_reserved + 1; \
CHARBUF_CLOSE(result_var_value); \
CHARBUF_CLOSE(result_var_is_null); \
CHARBUF_CLOSE(result_var);

// This does reserve and use in one step
#define CG_SETUP_RESULT_VAR(ast, sem_type) \
CG_RESERVE_RESULT_VAR(ast, sem_type); \
CG_USE_RESULT_VAR();

#define CG_BEGIN_ADJUST_FOR_OUTARG(var, sem_type_var) \
CHARBUF_OPEN(adjusted_target); \
/* for out parameters we need to do *name */ \
if (is_out_parameter(sem_type_var)) { \
  bprintf(&adjusted_target, "*%s", var); \
  var = adjusted_target.ptr; \
}

#define CG_END_ADJUST_FOR_OUTARG() \
CHARBUF_CLOSE(adjusted_target);

#define CG_CHARBUF_OPEN_SYM_WITH_PREFIX(name, symbol_prefix, ...) \
CHARBUF_OPEN(name); \
cg_sym_name(rt->symbol_case, &name, symbol_prefix, ##__VA_ARGS__, NULL)

#define CG_CHARBUF_OPEN_SYM(name, ...) \
CG_CHARBUF_OPEN_SYM_WITH_PREFIX(name, rt->symbol_prefix, ##__VA_ARGS__)

// This is the symbol table for all the tokens.
// This saves us from having a giant switch for the AST types
// and for the builtin functions.
//
// Note: semantic analysis knows about more function than code-gen does
// that's because many functions are only legal in the context of SQL
// so we have no codegen for them.  But we do need to verify correctness.
#define STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_ ## x)
#define NO_OP_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_no_op)
#define DDL_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_any_ddl_stmt)
#define STD_DML_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_std_dml_exec_stmt)
#define FUNC_INIT(x) symtab_add(cg_funcs, # x, (void *)cg_func_ ## x)
#define EXPR_INIT(x, func, str, pri_new) \
  static cg_expr_dispatch expr_disp_ ## x = { func, str, pri_new }; \
  symtab_add(cg_exprs, k_ast_ ## x, (void *)&expr_disp_ ## x);

typedef void (*cg_expr_dispatch_func)(ast_node *_Nonnull ast,
                                      CSTR _Nonnull op,
                                      charbuf *_Nonnull is_null,
                                      charbuf *_Nonnull value,
                                      int32_t pri,
                                      int32_t pri_new);

// for dispatching expression types
typedef struct cg_expr_dispatch {
  cg_expr_dispatch_func _Nonnull func;
  CSTR _Nonnull str;
  int32_t pri_new;
} cg_expr_dispatch;

// These are pre-loaded with pointers to functions for handling the
// root statements and functions.
cql_data_decl( symtab *_Nullable cg_stmts );
cql_data_decl( symtab *_Nullable cg_funcs );
cql_data_decl( symtab *_Nullable cg_exprs );

// Several code generators track the nesting level of their blocks for
// various purposes, mostly indenting and diagnostic output.
cql_data_decl( int32_t stmt_nesting_level );

// This is the first of two major outputs, this one holds the .h file output
// it will get the prototypes of all the functions we generate.
cql_data_decl( charbuf *_Nullable cg_header_output );

// This is current place where statements should be going.  It begins
// as a buffer that holds the original.c file but it is normal for this
// to get temporarily redirected into other places, such as the body of
// a stored proc.
cql_data_decl( charbuf *_Nullable cg_main_output );

// This will spill into the main buffer at the end.  String literals go here.
cql_data_decl( charbuf *_Nullable cg_constants_output );

// This will spill into the main buffer at the end.  Extern declarations go here
cql_data_decl( charbuf *_Nullable cg_fwd_ref_output );

// All local variable declarations are hoisted to the front of the resulting C.
// This prevents C lexical scoping from affecting SQL scoping rules.
cql_data_decl( charbuf *_Nullable cg_declarations_output );

// Scratch variables go into their own section and will go out adjacent to
// local variable declarations.
cql_data_decl( charbuf *_Nullable cg_scratch_vars_output );

// Any on-exit cleanup goes here. This is going to be the code to finalize
// any sql statements that were generated and also to release any strings
// we were holding on to.
cql_data_decl( charbuf *_Nullable cg_cleanup_output );

// The definitions of all of the statement fragments go into this section
cql_data_decl( charbuf *_Nullable cg_fragments_output );

// Prints a symbol name, along with any configured prefix, to the specified buffer.
// Multiple CSTRs may be supplied to build the name, which will be concatenated
// together.  The configured symbol case will be applied to the full symbol name.
// The prefix will be included as specified.
//
// All input names are assumed to be in snake case already.
cql_noexport void cg_sym_name(cg_symbol_case symbol_case, charbuf *_Nonnull output, CSTR _Nonnull symbol_prefix, CSTR _Nonnull name, ...);

// Initializes all of the common buffers and sym tables.
cql_noexport void cg_common_init(void);

// cleanup the global state
cql_noexport void cg_common_cleanup(void);

// Extract the last portion of a Unix path, without its extension
cql_noexport void extract_base_path_without_extension(charbuf *_Nonnull output, CSTR _Nonnull file_name);

// Exit if any semantic errors
cql_noexport void cql_exit_on_semantic_errors(ast_node *_Nullable head);

// Exit if no global proc name specified
cql_noexport void exit_on_no_global_proc(void);

// For the common case of "semantic-only" nodes
cql_noexport void cg_no_op(ast_node *_Nonnull ast);

// For expanding select *
cql_noexport bool_t cg_expand_star(ast_node *_Nonnull ast, void *_Nullable context, charbuf *_Nonnull buffer);

cql_noexport crc_t crc_charbuf(charbuf *_Nonnull input);

typedef struct table_callbacks {
  symtab *_Nullable visited_any_table;
  symtab *_Nullable visited_insert;
  symtab *_Nullable visited_update;
  symtab *_Nullable visited_delete;
  symtab *_Nullable visited_from;
  symtab *_Nullable visited_proc;
  find_ast_str_node_callback _Nullable callback_any_table;
  find_ast_str_node_callback _Nullable callback_any_view;
  find_ast_str_node_callback _Nullable callback_inserts;
  find_ast_str_node_callback _Nullable callback_updates;
  find_ast_str_node_callback _Nullable callback_deletes;
  find_ast_str_node_callback _Nullable callback_from;
  find_ast_str_node_callback _Nullable callback_proc;
  void *_Nullable callback_context;
} table_callbacks;

cql_noexport void find_table_refs(table_callbacks *_Nonnull data, ast_node *_Nonnull node);

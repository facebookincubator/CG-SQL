/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "cql.h"

cql_noexport void cg_lua_main(struct ast_node *_Nonnull root);
cql_noexport void cg_lua_cleanup(void);

typedef void (*cg_lua_expr_dispatch_func)(ast_node *_Nonnull ast,
                                      CSTR _Nonnull op,
                                      charbuf *_Nonnull value,
                                      int32_t pri,
                                      int32_t pri_new);

// for dispatching expression types
typedef struct cg_lua_expr_dispatch {
  cg_lua_expr_dispatch_func _Nonnull func;
  CSTR _Nonnull str;
  int32_t pri_new;
} cg_lua_expr_dispatch;

#define LUA_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_lua_ ## x)
#define LUA_NO_OP_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_lua_no_op)
#define LUA_DDL_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_lua_any_ddl_stmt)
#define LUA_STD_DML_STMT_INIT(x) symtab_add(cg_stmts, k_ast_ ## x, (void *)cg_lua_std_dml_exec_stmt)
#define LUA_FUNC_INIT(x) symtab_add(cg_funcs, # x, (void *)cg_lua_func_ ## x)
#define LUA_EXPR_INIT(x, func, str, pri_new) \
  static cg_lua_expr_dispatch expr_disp_ ## x = { func, str, pri_new }; \
  symtab_add(cg_exprs, k_ast_ ## x, (void *)&expr_disp_ ## x);

// Make a temporary buffer for the evaluation results using the canonical
// naming convention.  This might exit having burned some stack slots
// for its result variables, that's normal.
#define CG_LUA_PUSH_EVAL(expr, pri) \
CHARBUF_OPEN(expr##_value); \
cg_lua_expr(expr, &expr##_value, pri);

// Close the buffers used for the above.
// The scratch stack is not restored so that any temporaries used in
// the evaluation of expr will not be re-used prematurely.  They
// can't be used again until either the expression is finished,
// or they have been captured in a less-nested result variable.
#define CG_LUA_POP_EVAL(expr) \
CHARBUF_CLOSE(expr##_value);

// Create buffers for a temporary variable.  Use cg_scratch_var to fill in the buffers
// with the text needed to refer to the variable.  cg_scratch_var picks the name
// based on stack level-and type.
#define CG_LUA_PUSH_TEMP(name, sem_type) \
CHARBUF_OPEN(name); \
CHARBUF_OPEN(name##_value); \
cg_lua_scratch_var(NULL, sem_type, &name, &name##_value); \
lua_stack_level++;

// Release the buffers for the temporary, restore the stack level.
#define CG_LUA_POP_TEMP(name) \
CHARBUF_CLOSE(name##_value); \
CHARBUF_CLOSE(name); \
lua_stack_level--;


// Make a scratch variable to hold the final result of an evaluation.
// It may or may not be used.  It should be the first thing you put
// so that it is on the top of your stack.  This only saves the slot.
// If you use this variable you can reclaim other temporaries that come
// from deeper in the tree since they will no longer be needed.
#define CG_LUA_RESERVE_RESULT_VAR(ast, sem_type) \
int32_t lua_stack_level_reserved = lua_stack_level; \
sem_t sem_type_reserved = sem_type; \
ast_node *ast_reserved = ast; \
CHARBUF_OPEN(result_var); \
CHARBUF_OPEN(result_var_value); \
lua_stack_level++;

// If the result variable is going to be used, this writes its name and .value and into the outputs
#define CG_LUA_USE_RESULT_VAR() \
int32_t lua_stack_level_now = lua_stack_level; \
lua_stack_level = lua_stack_level_reserved; \
cg_lua_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_value); \
lua_stack_level = lua_stack_level_now; \
Invariant(result_var.used > 1); \
bprintf(value, "%s", result_var_value.ptr)

// Release the buffer holding the name of the variable.
// If the result variable was used, we can re-use any temporaries
// with a bigger number.  They're no longer needed since they
// are captured in this result.  We know it was used if it
// has .used > 1 (there is always a trailing null so empty is 1).
#define CG_LUA_CLEANUP_RESULT_VAR() \
if (result_var.used > 1) lua_stack_level = lua_stack_level_reserved + 1; \
CHARBUF_CLOSE(result_var_value); \
CHARBUF_CLOSE(result_var);

// This does reserve and use in one step
#define CG_LUA_SETUP_RESULT_VAR(ast, sem_type) \
CG_LUA_RESERVE_RESULT_VAR(ast, sem_type); \
CG_LUA_USE_RESULT_VAR();

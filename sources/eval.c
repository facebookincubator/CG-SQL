/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
#include "symtab.h"
#include "eval.h"

// These are the symbol tables with the ast dispatch when we get to an ast node
// we look it up here and call the appropriate function whose name matches the ast
// node type.

static symtab *evals;
static symtab *eval_funcs;

typedef void (*eval_dispatch)(ast_node *expr, eval_node *result);

static void eval_null(ast_node *expr, eval_node *result) {
  result->sem_type = SEM_TYPE_NULL;
}

static void eval_num(ast_node *expr, eval_node *result) {
  sem_t core_type = core_type_of(expr->sem->sem_type);
  result->sem_type = core_type;

  EXTRACT_NUM_TYPE(num_type, expr);
  EXTRACT_NUM_VALUE(lit, expr);

  bool known_type = false;

  switch (core_type) {
  case SEM_TYPE_INTEGER:
    result->_int32 = atoi(lit);
    known_type = true;
    break;

  case SEM_TYPE_LONG_INTEGER:
    result->_int64 = (int64_t)atoll(lit);
    known_type = true;
    break;

  case SEM_TYPE_REAL:
    result->_real = atof(lit);
    known_type = true;
    break;
  }

  Invariant(known_type);
}

static void eval_cast_to(eval_node *result, sem_t sem_type) {
  sem_t core_type_source = core_type_of(result->sem_type);
  sem_t core_type_target = core_type_of(sem_type);
  if (core_type_source == core_type_target) {
    return;
  }

  switch (core_type_target) {
    case SEM_TYPE_REAL:
      switch (core_type_source) {
        case SEM_TYPE_INTEGER:
          result->_real = (double)result->_int32;
          break;
        case SEM_TYPE_BOOL:
          result->_real = (double)!!result->_bool;
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->_real = (double)result->_int64;
          break;
      }
      break;
    case SEM_TYPE_LONG_INTEGER:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
          result->_int64 = (int64_t)result->_real;
          break;
        case SEM_TYPE_BOOL:
          result->_int64 = (int64_t)!!result->_bool;
          break;
        case SEM_TYPE_INTEGER:
          result->_int64 = (int64_t)result->_int32;
          break;
      }
      break;
    case SEM_TYPE_INTEGER:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
          result->_int32 = (int32_t)result->_real;
          break;
        case SEM_TYPE_BOOL:
          result->_int32 = (int32_t)!!result->_bool;
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->_int32 = (int32_t)result->_int64;
          break;
      }
      break;
    case SEM_TYPE_BOOL:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
          result->_bool = (bool_t)!!result->_real;
          break;
        case SEM_TYPE_INTEGER:
          result->_bool = (bool_t)!!result->_int32;
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->_bool = (bool_t)!!result->_int64;
          break;
      }
      break;
  }
  result->sem_type = core_type_target;
}

cql_noexport ast_node *eval_set(ast_node *expr, eval_node *result) {
  Contract(result);
  sem_t core_type = core_type_of(result->sem_type);
  Contract(core_type != SEM_TYPE_ERROR);

  AST_REWRITE_INFO_SET(expr->lineno, expr->filename);

  ast_node *new_num = NULL;

  switch (core_type) {
  case SEM_TYPE_INTEGER:
    new_num = new_ast_num(NUM_INT, dup_printf("%d", result->_int32));
    break;

  case SEM_TYPE_LONG_INTEGER:
    new_num = new_ast_num(NUM_LONG, dup_printf("%lld", (llint_t)result->_int64));
    break;

  case SEM_TYPE_BOOL:
    new_num = new_ast_num(NUM_INT, dup_printf("%d", !!result->_bool));
    break;

  case SEM_TYPE_REAL:
    new_num = new_ast_num(NUM_REAL, dup_printf("%g", result->_real));
    break;

  case SEM_TYPE_NULL:
    new_num = new_ast_null();
    break;
  }

  Invariant(new_num);
  ast_node *parent = expr->parent;

  if (parent->left == expr) {
    ast_set_left(parent, new_num);
  }
  else {
    ast_set_right(parent, new_num);
  }

  AST_REWRITE_INFO_RESET();
  return new_num;
}

static sem_t eval_combined_type(eval_node *left, eval_node *right) {
  sem_t core_type_left = core_type_of(left->sem_type);
  sem_t core_type_right = core_type_of(right->sem_type);

  if (core_type_left == SEM_TYPE_REAL || core_type_right == SEM_TYPE_REAL) {
    return SEM_TYPE_REAL;
  }

  if (core_type_left == SEM_TYPE_LONG_INTEGER || core_type_right == SEM_TYPE_LONG_INTEGER) {
    return SEM_TYPE_LONG_INTEGER;
  }

  if (core_type_left == SEM_TYPE_INTEGER || core_type_right == SEM_TYPE_INTEGER) {
    return SEM_TYPE_INTEGER;
  }

  if (core_type_left == SEM_TYPE_BOOL || core_type_right == SEM_TYPE_BOOL) {
    return SEM_TYPE_BOOL;
  }

  return SEM_TYPE_NULL;
}

#define BINARY_OP(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
  \
  sem_t core_type = eval_combined_type(&left, &right); \
  \
  if (left.sem_type == SEM_TYPE_ERROR || right.sem_type == SEM_TYPE_ERROR) { \
    result->sem_type = SEM_TYPE_ERROR; \
    return; \
  } \
  \
  if (left.sem_type == SEM_TYPE_NULL || right.sem_type == SEM_TYPE_NULL) { \
    result->sem_type = SEM_TYPE_NULL; \
    return; \
  } \
  \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->_int32 = (left._int32 op right._int32); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->_int64 = (left._int64 op right._int64); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->_bool = !!(left._bool op right._bool); \
    break; \
  \
  case SEM_TYPE_REAL: \
    result->_real = (left._real op right._real); \
    break; \
  } \
  \
  result->sem_type = core_type;

#define BINARY_OP_NO_REAL(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
  \
  sem_t core_type = eval_combined_type(&left, &right); \
  \
  if (left.sem_type == SEM_TYPE_ERROR || right.sem_type == SEM_TYPE_ERROR) { \
    result->sem_type = SEM_TYPE_ERROR; \
    return; \
  } \
  \
  if (left.sem_type == SEM_TYPE_NULL || right.sem_type == SEM_TYPE_NULL) { \
    result->sem_type = SEM_TYPE_NULL; \
    return; \
  } \
  \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->_int32 = (left._int32 op right._int32); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->_int64 = (left._int64 op right._int64); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->_bool = !!(left._bool op right._bool); \
    break; \
  \
  } \
  \
  result->sem_type = core_type;

#define COMPARE_BINARY_OP(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
  \
  sem_t core_type = eval_combined_type(&left, &right); \
  \
  if (left.sem_type == SEM_TYPE_ERROR || right.sem_type == SEM_TYPE_ERROR) { \
    result->sem_type = SEM_TYPE_ERROR; \
    return; \
  } \
 \
  if (left.sem_type == SEM_TYPE_NULL || right.sem_type == SEM_TYPE_NULL) { \
    result->sem_type = SEM_TYPE_NULL; \
    return; \
  } \
  \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->_bool = (left._int32 op right._int32); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->_bool = (left._int64 op right._int64); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->_bool = !!(left._bool op right._bool); \
    break; \
  \
  case SEM_TYPE_REAL: \
    result->_bool = (left._real op right._real); \
    break; \
  } \
  \
  result->sem_type = SEM_TYPE_BOOL;

static void eval_add(ast_node *expr, eval_node *result) {
  BINARY_OP(+);
}

static void eval_sub(ast_node *expr, eval_node *result) {
  BINARY_OP(-);
}

static void eval_mul(ast_node *expr, eval_node *result) {
  BINARY_OP(*);
}

static void eval_div(ast_node *expr, eval_node *result) {
  BINARY_OP(/);
}

static void eval_mod(ast_node *expr, eval_node *result) {
  BINARY_OP_NO_REAL(%);
}

static void eval_eq(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(==);
}

static void eval_ne(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(!=);
}

static void eval_le(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(<=);
}

static void eval_ge(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(>=);
}

static void eval_lt(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(<);
}

static void eval_gt(ast_node *expr, eval_node *result) {
  COMPARE_BINARY_OP(>);
}

static void eval_lshift(ast_node *expr, eval_node *result) {
  BINARY_OP_NO_REAL(<<);
}

static void eval_rshift(ast_node *expr, eval_node *result) {
  BINARY_OP_NO_REAL(>>);
}

static void eval_bin_and(ast_node *expr, eval_node *result) {
  BINARY_OP_NO_REAL(&);
}

static void eval_bin_or(ast_node *expr, eval_node *result) {
  BINARY_OP_NO_REAL(|);
}

static void eval_is(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval_node right = {};
  eval(expr->left, &left);
  eval(expr->right, &right);
  
  sem_t core_type = eval_combined_type(&left, &right);

  if (left.sem_type == SEM_TYPE_ERROR || right.sem_type == SEM_TYPE_ERROR) { \
    result->sem_type = SEM_TYPE_ERROR; \
    return;
  }

  result->sem_type = SEM_TYPE_BOOL;
 
  if (left.sem_type == SEM_TYPE_NULL || right.sem_type == SEM_TYPE_NULL) {
    result->_bool = (left.sem_type == SEM_TYPE_NULL && right.sem_type == SEM_TYPE_NULL);
    return;
  }

  eval_cast_to(&left, core_type);
  eval_cast_to(&right, core_type);

  switch (core_type) {
    case SEM_TYPE_INTEGER:
      result->_bool = (left._int32 == right._int32);
      break;

    case SEM_TYPE_LONG_INTEGER:
      result->_bool = (left._int64 == right._int64);
      break;
  
    case SEM_TYPE_BOOL:
      result->_bool = !!(left._bool == right._bool);
      break;
   
    case SEM_TYPE_REAL:
      result->_bool = (left._real == right._real);
      break;
  }
}

static void eval_is_not(ast_node *expr, eval_node *result) {
  eval_is(expr, result);
  if (result->sem_type == SEM_TYPE_BOOL) {
     result->_bool = !result->_bool;
  }
}

static void eval_and(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval(expr->left, &left);

  if (left.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (left.sem_type != SEM_TYPE_NULL) {
    eval_cast_to(&left, SEM_TYPE_BOOL);
    if (!left._bool) {
      result->sem_type = SEM_TYPE_BOOL;
      result->_bool = 0;
      return;
    }
  }

  eval_node right = {};
  eval(expr->right, &right);
  if (right.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (right.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  eval_cast_to(&right, SEM_TYPE_BOOL);
  if (!right._bool) {
    result->sem_type = SEM_TYPE_BOOL;
    result->_bool = 0;
    return;
  } 

  if (left.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  result->sem_type = SEM_TYPE_BOOL;
  result->_bool = 1;
}

static void eval_or(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval(expr->left, &left);

  if (left.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (left.sem_type != SEM_TYPE_NULL) {
    eval_cast_to(&left, SEM_TYPE_BOOL);
    if (left._bool) {
      result->sem_type = SEM_TYPE_BOOL;
      result->_bool = 1;
      return;
    }
  }

  eval_node right = {};
  eval(expr->right, &right);
  if (right.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (right.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  eval_cast_to(&right, SEM_TYPE_BOOL);
  if (right._bool) {
    result->sem_type = SEM_TYPE_BOOL;
    result->_bool = 1;
    return;
  } 

  if (left.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  result->sem_type = SEM_TYPE_BOOL;
  result->_bool = 0;
}

cql_noexport void eval_cast_expr(ast_node *expr, eval_node *result) {
  eval(expr->left, result);

  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  if (result->sem_type == SEM_TYPE_NULL) {
    return;
  }

  eval_cast_to(result, core_type_of(expr->sem->sem_type));
}


cql_noexport void eval(ast_node *expr, eval_node *result) {
  // These are all the expressions there are, we have to find it in this table
  // or else someone added a new expression type and it isn't supported yet.
  symtab_entry *entry = symtab_find(evals, expr->type);
  if (!entry) {
    eval_node err_result = { .sem_type = SEM_TYPE_ERROR };
    *result = err_result;
    return;
  }

  eval_dispatch disp = (eval_dispatch)entry->val;
  disp(expr, result);
}

#undef FUNC_INIT
#define FUNC_INIT(x) symtab_add(eval_funcs, #x, (void *)eval_func_ ## x)

#undef EXPR_INIT
#define EXPR_INIT(x) symtab_add(evals, #x, (void *)eval_ ## x)

// This method loads up the global symbol tables in either empty state or
// with the appropriate tokens ready to go.  Using our own symbol tables for
// dispatch saves us a lot of if/else string comparison verbosity.
cql_noexport void eval_init() {
// restore all globals and statics we own
eval_cleanup();

evals = symtab_new();
eval_funcs = symtab_new();

/*
FUNC_INIT(ifnull);
FUNC_INIT(nullif);
FUNC_INIT(abs);
FUNC_INIT(coalesce);
*/

EXPR_INIT(null);
EXPR_INIT(num);
EXPR_INIT(add);
EXPR_INIT(mul);
EXPR_INIT(div);
EXPR_INIT(mod);
EXPR_INIT(sub);
EXPR_INIT(lshift);
EXPR_INIT(rshift);
EXPR_INIT(bin_and);
EXPR_INIT(bin_or);
EXPR_INIT(eq);
EXPR_INIT(lt);
EXPR_INIT(gt);
EXPR_INIT(ne);
EXPR_INIT(ge);
EXPR_INIT(le);
EXPR_INIT(and);
EXPR_INIT(or);
EXPR_INIT(is);
EXPR_INIT(is_not);
EXPR_INIT(cast_expr);

/*
EXPR_INIT(str);
EXPR_INIT(blob);
EXPR_INIT(dot);
EXPR_INIT(collate);
EXPR_INIT(not);
EXPR_INIT(tilde);
EXPR_INIT(uminus);
EXPR_INIT(call);
EXPR_INIT(window_func_inv);
EXPR_INIT(raise);
EXPR_INIT(exists_expr);
EXPR_INIT(between);
EXPR_INIT(not_between);
EXPR_INIT(select_stmt);
EXPR_INIT(with_select_stmt);
EXPR_INIT(like);
EXPR_INIT(not_like);
EXPR_INIT(match);
EXPR_INIT(regexp);
EXPR_INIT(glob);
EXPR_INIT(in_pred);
EXPR_INIT(not_in);
EXPR_INIT(case_expr);
EXPR_INIT(concat);
*/
}

cql_noexport void eval_cleanup() {
SYMTAB_CLEANUP(evals);
SYMTAB_CLEANUP(eval_funcs);
}

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
#include <float.h>
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

// This is the symbol table with the ast dispatch when we get to an ast node
// we look it up here and call the appropriate function whose name matches the ast
// node type.
static symtab *evals;

// The signature of the various evaluation functions
typedef void (*eval_dispatch)(ast_node *expr, eval_node *result);

// Null literal
static void eval_null(ast_node *expr, eval_node *result) {
  result->sem_type = SEM_TYPE_NULL;
}

// A number; it could be any of the numeric types with or without a hex prefix
static void eval_num(ast_node *expr, eval_node *result) {
  EXTRACT_NUM_TYPE(num_type, expr);
  EXTRACT_NUM_VALUE(lit, expr);

  result->sem_type = SEM_TYPE_ERROR;

  switch (num_type) {
  case NUM_BOOL:
    result->bool_value = (bool_t)!!strtol(lit, NULL, 10);
    result->sem_type = SEM_TYPE_BOOL;
    break;

  case NUM_INT:
    result->int32_value = (int32_t)strtol(lit, NULL, has_hex_prefix(lit) ? 16 : 10);
    result->sem_type = SEM_TYPE_INTEGER;
    break;

  case NUM_LONG:
    result->int64_value = strtol(lit, NULL, has_hex_prefix(lit) ? 16 : 10);
    result->sem_type = SEM_TYPE_LONG_INTEGER;
    break;

  case NUM_REAL:
    result->real_value = atof(lit);
    result->sem_type = SEM_TYPE_REAL;
    break;
  }

  Invariant(result->sem_type != SEM_TYPE_ERROR);
}

// Used for explicit casts but also for numeric conversions when
// a free promotion is allowed.  This will convert between
// any of the numeric types.  There are twelve possible conversions
// since a type never converts to itself.  This only works on
// numeric types.  Any errors and null stuff must be pre-checked.
cql_noexport void eval_cast_to(eval_node *result, sem_t sem_type) {
  sem_t core_type_source = core_type_of(result->sem_type);
  sem_t core_type_target = core_type_of(sem_type);

  Contract(core_type_source != SEM_TYPE_NULL);
  Contract(core_type_target != SEM_TYPE_NULL);
  Contract(core_type_source != SEM_TYPE_ERROR);
  Contract(core_type_target != SEM_TYPE_ERROR);

  if (core_type_source == core_type_target) {
    return;
  }

  switch (core_type_target) {
    case SEM_TYPE_REAL:
      switch (core_type_source) {
        case SEM_TYPE_INTEGER:
          result->real_value = (double)result->int32_value;
          break;
        case SEM_TYPE_BOOL:
          result->real_value = (double)!!result->bool_value;
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->real_value = (double)result->int64_value;
          break;
      }
      break;
    case SEM_TYPE_LONG_INTEGER:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
          result->int64_value = (int64_t)result->real_value;
          break;
        case SEM_TYPE_BOOL:
          result->int64_value = (int64_t)!!result->bool_value;
          break;
        case SEM_TYPE_INTEGER:
          result->int64_value = (int64_t)result->int32_value;
          break;
      }
      break;
    case SEM_TYPE_INTEGER:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
          result->int32_value = (int32_t)result->real_value;
          break;
        case SEM_TYPE_BOOL:
          result->int32_value = (int32_t)!!result->bool_value;
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->int32_value = (int32_t)result->int64_value;
          break;
      }
      break;
    case SEM_TYPE_BOOL:
      switch (core_type_source) {
        case SEM_TYPE_REAL:
           result->bool_value = (result->real_value != 0);
          break;
        case SEM_TYPE_INTEGER:
          result->bool_value = (result->int32_value != 0);
          break;
        case SEM_TYPE_LONG_INTEGER:
          result->bool_value = (result->int64_value != 0);
          break;
      }
      break;
  }
  result->sem_type = core_type_target;
}

static void eval_format_real(double real, charbuf *output) {
  CHARBUF_OPEN(tmp);
  bprintf(&tmp, "%*e", DECIMAL_DIG, real);
  CSTR p = tmp.ptr;
  while (*p == ' ') p++;
  bprintf(output, "%s", p);
  CHARBUF_CLOSE(tmp);
}

// In order to use the eval logic to replace expression trees we
// need to be able to make a new ast node that represents the result
// of a calculation.  This function takes such a result and creates
// a node.  The incoming expression is harvested for file and
// line info and then replaced in the tree.  It's value is otherwise irrelevant
// because the computation has already been done.
// The incoming evaluation result must not be an error node.
cql_noexport ast_node *eval_set(ast_node *expr, eval_node *result) {
  Contract(result);
  sem_t core_type = core_type_of(result->sem_type);
  Contract(core_type != SEM_TYPE_ERROR);

  AST_REWRITE_INFO_SET(expr->lineno, expr->filename);

  ast_node *new_num = NULL;

  switch (core_type) {
  case SEM_TYPE_INTEGER:
    new_num = new_ast_num(NUM_INT, dup_printf("%d", result->int32_value));
    break;

  case SEM_TYPE_LONG_INTEGER:
    new_num = new_ast_num(NUM_LONG, dup_printf("%lld", (llint_t)result->int64_value));
    break;

  case SEM_TYPE_BOOL:
    new_num = new_ast_num(NUM_BOOL, dup_printf("%d", !!result->bool_value));
    break;

  case SEM_TYPE_REAL:
    {
    CHARBUF_OPEN(tmp);
    eval_format_real(result->real_value, &tmp);
    new_num = new_ast_num(NUM_REAL, dup_printf("%s", tmp.ptr));
    CHARBUF_CLOSE(tmp);
    break;
    }

  case SEM_TYPE_NULL:
    new_num = new_ast_null();
    break;
  }

  // now replace the incoming expression in its tree

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

// This finds the best type for a numeric operation.
// We use the "biggest" numeric type that can hold the result.
// Errors and nulls must have already been handled, they have
// no business showing up here.
static sem_t eval_combined_type(eval_node *left, eval_node *right) {
  sem_t core_type_left = core_type_of(left->sem_type);
  sem_t core_type_right = core_type_of(right->sem_type);

  Contract(core_type_left != SEM_TYPE_ERROR);
  Contract(core_type_right != SEM_TYPE_ERROR);
  Contract(core_type_left != SEM_TYPE_NULL);
  Contract(core_type_right != SEM_TYPE_NULL);

  sem_t result = SEM_TYPE_ERROR;

  if (core_type_left == SEM_TYPE_REAL || core_type_right == SEM_TYPE_REAL) {
    result = SEM_TYPE_REAL;
  }
  else if (core_type_left == SEM_TYPE_LONG_INTEGER || core_type_right == SEM_TYPE_LONG_INTEGER) {
    result =  SEM_TYPE_LONG_INTEGER;
  }
  else if (core_type_left == SEM_TYPE_INTEGER || core_type_right == SEM_TYPE_INTEGER) {
    result = SEM_TYPE_INTEGER;
  }
  else if (core_type_left == SEM_TYPE_BOOL || core_type_right == SEM_TYPE_BOOL) {
    result = SEM_TYPE_BOOL;
  }

  Invariant(result != SEM_TYPE_ERROR);
  return result;
}

// All the normal binary operators are handled the same way, only the operator actually varies.
// The thing is the operator has to be lexically substituted in so that we get the correct
// math type so much as this much macro is a code smell, the alternative is open coding this
// for every binary operator which is worse.  The steps are:
//   * any error in the operands results in an error
//   * any null operand results in a null result
//   * find the smallest numeric type that will hold the answer
//   * convert to that type if needed
//   * apply the operator on that type
//
// NOTE: logical AND/OR cannot be on this plan because of their short circuit behavior.
//       for bitwise operators, see the _NO_REAL version of this macro
//       for comparisons likewise see below for a slightly different version.
#define BINARY_OP(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
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
  if ((#op)[0] == '/' && result_is_false(&right)) { \
    /* special case to prevent divide by zero */ \
    result->sem_type = SEM_TYPE_ERROR; \
    return; \
  } \
  \
  sem_t core_type = eval_combined_type(&left, &right); \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  result->sem_type = SEM_TYPE_ERROR; \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->sem_type = SEM_TYPE_INTEGER; \
    result->int32_value = (left.int32_value op right.int32_value); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->sem_type = SEM_TYPE_LONG_INTEGER; \
    result->int64_value = (left.int64_value op right.int64_value); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = 0 != (left.bool_value op right.bool_value); \
    break; \
  \
  case SEM_TYPE_REAL: \
    result->sem_type = SEM_TYPE_REAL; \
    result->real_value = (left.real_value op right.real_value); \
    break; \
  } \
  \
  Invariant(result->sem_type == core_type)

// This is exactly like the standard binary operator macro except it is for
// the operators that are not allowed to apply to real numbers.  e.g.
// bitwise and/or and left/right shift.
#define BINARY_OP_NO_REAL(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
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
  if ((#op)[0] == '%' && result_is_false(&right)) { \
    /* special case to prevent divide by zero */ \
    result->sem_type = SEM_TYPE_ERROR; \
    return; \
  } \
  sem_t core_type = eval_combined_type(&left, &right); \
  \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  result->sem_type = SEM_TYPE_ERROR; \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->sem_type = SEM_TYPE_INTEGER; \
    result->int32_value = (left.int32_value op right.int32_value); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->sem_type = SEM_TYPE_LONG_INTEGER; \
    result->int64_value = (left.int64_value op right.int64_value); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = 0 != (left.bool_value op right.bool_value); \
    break; \
  } \
  \
  Invariant(result->sem_type == core_type)

// The final large class of operators are the comparisons. These
// have similar rules to the normal operators but the return type
// is bool/null/error.  The flow is pretty similar though
//   * any error in the operands results in an error
//   * any null operand results in a null result
//   * find the smallest numeric type that will hold the answer
//   * convert to that type if needed
//   * apply the operator on that type
//   * return the resulting bool
//
// NOTE:  is and is_not cannot be on this plan because of their
//        null semantics, they are similar, see below.
#define COMPARE_BINARY_OP(op) \
  eval_node left = {}; \
  eval_node right = {}; \
  eval(expr->left, &left); \
  eval(expr->right, &right); \
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
  sem_t core_type = eval_combined_type(&left, &right); \
  eval_cast_to(&left, core_type); \
  eval_cast_to(&right, core_type); \
  result->sem_type = SEM_TYPE_ERROR; \
  \
  switch (core_type) { \
  case SEM_TYPE_INTEGER: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = (left.int32_value op right.int32_value); \
    break; \
  \
  case SEM_TYPE_LONG_INTEGER: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = (left.int64_value op right.int64_value); \
    break; \
  \
  case SEM_TYPE_BOOL: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = 0 != (left.bool_value op right.bool_value); \
    break; \
  \
  case SEM_TYPE_REAL: \
    result->sem_type = SEM_TYPE_BOOL; \
    result->bool_value = (left.real_value op right.real_value); \
    break; \
  } \
  \
  Invariant(result->sem_type == SEM_TYPE_BOOL);

// True if the node is a zero; not err, not NULL, an actual zero
static bool_t result_is_false(eval_node *result) {
  // null is not false
  if (result->sem_type == SEM_TYPE_NULL || result->sem_type == SEM_TYPE_ERROR) {
    return false;
  }

  eval_node temp = *result;
  eval_cast_to(&temp, SEM_TYPE_BOOL);
  return !temp.bool_value;
}

// True if the node is not zero; not err, not NULL, an actual non-zero
static bool_t result_is_true(eval_node *result) {
  // null/error is not true
  if (result->sem_type == SEM_TYPE_NULL || result->sem_type == SEM_TYPE_ERROR) {
    return false;
  }

  eval_node temp = *result;
  eval_cast_to(&temp, SEM_TYPE_BOOL);
  return temp.bool_value;
}

// Having defined the helper macros all of the normal operators
// are now just one of the standard expansions

static void eval_add(ast_node *expr, eval_node *result) {
  BINARY_OP(+);
}

static void eval_sub(ast_node *expr, eval_node *result) {
  BINARY_OP(-);
}

static void eval_mul(ast_node *expr, eval_node *result) {
  BINARY_OP(*);
}

// note: BINARY_OP has divide by zero logic
static void eval_div(ast_node *expr, eval_node *result) {
  BINARY_OP(/);
}

// note: BINARY_OP_NO_REAL has divide by zero logic
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

// The 'is' form is very similar to the others but the null handling is different
// so there's an early out for that;  The logic is:
// * any error in left or right yields an error
// * if either left or right is null, the result is true if and only if both are null
// * at this point all the error/null cases are handled so it's just like the
//   back end of the BINARY_OP case;  the args have arleady been evaluated so:
//   * compute the smallest type that holds both values
//   * convert to that type
//   * return true if and only if the values are equal as that type
static void eval_is(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval_node right = {};
  eval(expr->left, &left);
  eval(expr->right, &right);

  if (left.sem_type == SEM_TYPE_ERROR || right.sem_type == SEM_TYPE_ERROR) { \
    result->sem_type = SEM_TYPE_ERROR; \
    return;
  }


  if (left.sem_type == SEM_TYPE_NULL || right.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_BOOL;
    result->bool_value = (left.sem_type == SEM_TYPE_NULL && right.sem_type == SEM_TYPE_NULL);
    return;
  }

  sem_t core_type = eval_combined_type(&left, &right);
  eval_cast_to(&left, core_type);
  eval_cast_to(&right, core_type);

  result->sem_type = SEM_TYPE_ERROR;

  switch (core_type) {
    case SEM_TYPE_INTEGER:
      result->sem_type = SEM_TYPE_BOOL;
      result->bool_value = (left.int32_value == right.int32_value);
      break;

    case SEM_TYPE_LONG_INTEGER:
      result->sem_type = SEM_TYPE_BOOL;
      result->bool_value = (left.int64_value == right.int64_value);
      break;

    case SEM_TYPE_BOOL:
      result->sem_type = SEM_TYPE_BOOL;
      result->bool_value = 0 != (left.bool_value == right.bool_value);
      break;

    case SEM_TYPE_REAL:
      result->sem_type = SEM_TYPE_BOOL;
      result->bool_value = (left.real_value == right.real_value);
      break;
  }

  Invariant(result->sem_type == SEM_TYPE_BOOL);
}

// We can use the 'is' logic to do this, we simply run "is", if the result
// was a bool (i.e. not an error) then we simply invert the bool.
static void eval_is_not(ast_node *expr, eval_node *result) {
  eval_is(expr, result);
  if (result->sem_type == SEM_TYPE_BOOL) {
     result->bool_value = !result->bool_value;
  }
}

// Not has simple rules;  If the operand is an error or null it is unchanged
// any other operator is converted to a bool and then inverted.
static void eval_not(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR || result->sem_type == SEM_TYPE_NULL) {
    return;
  }

  eval_cast_to(result, SEM_TYPE_BOOL);
  Invariant(result->sem_type == SEM_TYPE_BOOL);  // set by the above
  result->bool_value = !result->bool_value;
}

// Is false has simple rules;  If the operand is an error it is unchanged
// null becomes FALSE any other is converted to a bool and inverted
static void eval_is_false(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  result->bool_value = result_is_false(result);
  result->sem_type = SEM_TYPE_BOOL;
}

// Is true has simple rules;  If the operand is an error it is unchanged
// null becomes FALSE any other is converted to a bool.
static void eval_is_true(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  result->bool_value = result_is_true(result);
  result->sem_type = SEM_TYPE_BOOL;
}

// Is not true has simple rules;  If the operand is an error it is unchanged
// null becomes TRUE any other is converted to a bool and inverted
static void eval_is_not_true(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  if (result->sem_type == SEM_TYPE_NULL) {
    result->bool_value = 1;
  }
  else {
    result->bool_value = !result_is_true(result);
  }

  result->sem_type = SEM_TYPE_BOOL;
}

// Is not false has simple rules;  If the operand is an error it is unchanged
// null becomes TRUE any other is converted to a bool
static void eval_is_not_false(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  if (result->sem_type == SEM_TYPE_NULL) {
    result->bool_value = 1;
  }
  else {
    result->bool_value = !result_is_false(result);
  }

  result->sem_type = SEM_TYPE_BOOL;
}

// The bitwise not operator is rather like the normal not with a few twists:
//  * an error or null argument is returned unchanged
//  * integers and long_integers are bitwise inverted with ~
//  * bool is promoted to integer so the only possible result values are:
//    -1 for ~0, and -2 for ~1;  This is also what SQLite does.
static void eval_tilde(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR || result->sem_type == SEM_TYPE_NULL) {
    return;
  }

  // only the numeric cases left
  bool matched = false;

  switch (result->sem_type) {
    case SEM_TYPE_INTEGER:
      result->int32_value = ~result->int32_value;
      matched = true;
      break;

    case SEM_TYPE_LONG_INTEGER:
      result->int64_value = ~result->int64_value;
      matched = true;
      break;

    case SEM_TYPE_BOOL:
      result->sem_type = SEM_TYPE_INTEGER;
      // use ternary in case bool has a true value other than 1
      // this is clearer than writing ~!!result->bool_value I think...
      result->int32_value = result->bool_value ? ~1 : ~0;
      matched = true;
      break;
  }
  Invariant(matched);
}

// Unary minus (negation) is very much like bitwise not:
// * error or null are unchanged
// * all others are negated
// * bool promotes to an integer and is negated so the valid results are 0 and -1
static void eval_uminus(ast_node *expr, eval_node *result) {
  eval(expr->left, result);
  if (result->sem_type == SEM_TYPE_ERROR || result->sem_type == SEM_TYPE_NULL) {
    return;
  }

  // only the numeric cases left
  bool matched = false;

  switch (result->sem_type) {
    case SEM_TYPE_INTEGER:
      matched = true;
      result->int32_value = -result->int32_value;
      break;

    case SEM_TYPE_LONG_INTEGER:
      matched = true;
      result->int64_value = -result->int64_value;
      break;

    case SEM_TYPE_REAL:
      matched = true;
      result->real_value = -result->real_value;
      break;

    case SEM_TYPE_BOOL:
      matched = true;
      result->sem_type = SEM_TYPE_INTEGER;
      // use ternary in case bool has a true value other than 1
      // this is clearer than writing -!!result->bool_value I think...
      result->int32_value = result->bool_value ? -1 : 0;
      break;
  }
  Invariant(matched);
}

// logical AND is tricky because it has to follow the truth table and also
// short circuit.
//   * if the left arg is an error the result is an error
//   * if the left result is false the answer is false and the right are is
//     not evaluated.
//   * if the right arg is an error the result is an error
//   * if the right arg is false the answer is false
//   * if either are null the answer is null
//   * otherwise the answer is true.
static void eval_and(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval(expr->left, &left);

  if (left.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (result_is_false(&left)) {
    result->sem_type = SEM_TYPE_BOOL;
    result->bool_value = 0;
    return;
  }

  eval_node right = {};
  eval(expr->right, &right);
  if (right.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (result_is_false(&right)) {
    result->sem_type = SEM_TYPE_BOOL;
    result->bool_value = 0;
    return;
  }

  if (right.sem_type == SEM_TYPE_NULL || left.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  result->sem_type = SEM_TYPE_BOOL;
  result->bool_value = 1;
}

// logical OR is tricky because it has to follow the truth table and also
// short circuit.
//   * if the left arg is an error the result is an error
//   * if the left result is true the answer is true and the right are is
//     not evaluated.
//   * if the right arg is an error the result is an error
//   * if the right arg is true the answer is true
//   * if either are null the answer is null
//   * otherwise the answer is false.
static void eval_or(ast_node *expr, eval_node *result) {
  eval_node left = {};
  eval(expr->left, &left);

  if (left.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (result_is_true(&left)) {
    result->sem_type = SEM_TYPE_BOOL;
    result->bool_value = 1;
    return;
  }

  eval_node right = {};
  eval(expr->right, &right);
  if (right.sem_type == SEM_TYPE_ERROR) {
    result->sem_type = SEM_TYPE_ERROR;
    return;
  }

  if (result_is_true(&right)) {
    result->sem_type = SEM_TYPE_BOOL;
    result->bool_value = 1;
    return;
  }

  if (right.sem_type == SEM_TYPE_NULL || left.sem_type == SEM_TYPE_NULL) {
    result->sem_type = SEM_TYPE_NULL;
    return;
  }

  result->sem_type = SEM_TYPE_BOOL;
  result->bool_value = 0;
}

// Cast is super easy because we arleady have the eval_cast_to helper
// we just do the evaluation it and early out on error or null
static void eval_cast_expr(ast_node *expr, eval_node *result) {
  eval(expr->left, result);

  if (result->sem_type == SEM_TYPE_ERROR) {
    return;
  }

  if (result->sem_type == SEM_TYPE_NULL) {
    return;
  }

  // known to be numeric, eval_cast_to can do the job now.
  eval_cast_to(result, core_type_of(expr->sem->sem_type));
}

// This helper is used in the evaluation of case expressions
// it's very much like the "==" operator.  Errors have already
// been handled in the caller.
//  * null is not equal to anything
//  * convert to the smallest type that can hold left or right
//  * compare using that type
static bool eval_are_equal(eval_node *_left, eval_node *_right) {
  eval_node left = *_left;
  eval_node right = *_right;

  Contract(right.sem_type != SEM_TYPE_ERROR);
  Contract(left.sem_type != SEM_TYPE_ERROR);

  // null isn't equal to anything
  if (right.sem_type == SEM_TYPE_NULL || left.sem_type == SEM_TYPE_NULL) {
    return false;
  }

  sem_t core_type = eval_combined_type(&left, &right);
  eval_cast_to(&left, core_type);
  eval_cast_to(&right, core_type);

  // there must be a conversion or semantic would have stopped us and both
  // are now promoted... we just compare the values
  Invariant(left.sem_type == right.sem_type);

  int32_t result = -1;

  switch (left.sem_type) {
    case SEM_TYPE_INTEGER:
      result = left.int32_value == right.int32_value;
      break;

    case SEM_TYPE_LONG_INTEGER:
      result = left.int64_value == right.int64_value;
      break;

    case SEM_TYPE_REAL:
      result = left.real_value == right.real_value;
      break;

    case SEM_TYPE_BOOL:
      result = left.bool_value == right.bool_value;
      break;
  }

  Invariant(result == 0 || result == 1);
  return result;
}

// Case expression evaluation is fairly complex with the two cases outlined below.
// In short:
//   * evaluate the test expression if there is one
//   * visit the WHEN expressions and use equality or truth
//     to pick the correct THEN expression
//   * use the else expression if there is one, else use NULL
//   * any errors encountered yield an error
//   * the usual null equality rules apply
static void eval_case_expr(ast_node *ast, eval_node *result) {
  EXTRACT_ANY(expr, ast->left);
  EXTRACT_NOTNULL(connector, ast->right);
  EXTRACT_NOTNULL(case_list, connector->left);
  EXTRACT_ANY(else_expr, connector->right);

  // Case can have expression or just when clauses
  if (expr) {
    // This branch has a test expression, save its value
    eval_node test_result = {};
    eval(expr, &test_result);
    if (test_result.sem_type == SEM_TYPE_ERROR) {
      result->sem_type = SEM_TYPE_ERROR;
      return;
    }

    // now walk through the case list, and choose the matching THEN
    ast = case_list;
    while (ast) {
      EXTRACT_NOTNULL(when, ast->left);
      EXTRACT_ANY_NOTNULL(case_expr, when->left);
      EXTRACT_ANY_NOTNULL(then_expr, when->right);

      eval_node case_result = {};
      eval(case_expr, &case_result);
      if (case_result.sem_type == SEM_TYPE_ERROR) {
        result->sem_type = SEM_TYPE_ERROR;
        return;
      }

      if (eval_are_equal(&test_result, &case_result)) {
        eval(then_expr, result);
        return;
      }
      ast = ast->right;
    }
  }
  else {
    // This is the case where we're looking for the first true expressions
    // walk through the case list, and choose the matching THEN
    ast = case_list;
    while (ast) {
      EXTRACT_NOTNULL(when, ast->left);
      EXTRACT_ANY_NOTNULL(case_expr, when->left);
      EXTRACT_ANY_NOTNULL(then_expr, when->right);

      eval_node case_result = {};
      eval(case_expr, &case_result);
      if (case_result.sem_type == SEM_TYPE_ERROR) {
        result->sem_type = SEM_TYPE_ERROR;
        return;
      }

      if (result_is_true(&case_result)) {
        eval(then_expr, result);
        return;
      }

      ast = ast->right;
    }
  }

  // if we get this far, there's no match, use the else clause if there is one
  if (else_expr) {
    eval(else_expr, result);
  }
  else {
    result->sem_type = SEM_TYPE_NULL;
  }
}

cql_noexport void eval_add_one(eval_node *result) {
  Contract(result->sem_type != SEM_TYPE_ERROR);
  Contract(result->sem_type != SEM_TYPE_NULL);

  switch (result->sem_type) {
    case SEM_TYPE_INTEGER:
      result->int32_value++;
      break;

    case SEM_TYPE_LONG_INTEGER:
      result->int64_value++;
      break;

    case SEM_TYPE_REAL:
      result->real_value++;
      break;

    case SEM_TYPE_BOOL:
      result->bool_value = !result->bool_value;
      break;
  }
}

cql_noexport void eval_format_number(eval_node *result, charbuf *output) {
  Contract(result->sem_type != SEM_TYPE_ERROR);
  Contract(result->sem_type != SEM_TYPE_NULL);

  uint32_t used = output->used;

  switch (result->sem_type) {
    case SEM_TYPE_INTEGER:
      bprintf(output, "%d", result->int32_value);
      break;

    case SEM_TYPE_LONG_INTEGER:
      bprintf(output, "%lld", (llint_t)result->int64_value);
      break;

    case SEM_TYPE_REAL:
      eval_format_real(result->real_value, output);
      break;

    case SEM_TYPE_BOOL:
      bprintf(output, "%d", !!result->bool_value);
      break;
  }

  // verify we wrote something
  Invariant(output->used > used);
}

static eval_node err_result = { .sem_type = SEM_TYPE_ERROR };

// Dispatch to the worker function using the token string in the ast and the symbol table
// any unknown symbols are evaluation errors due to unsupported const expression form.
cql_noexport void eval(ast_node *expr, eval_node *result) {
  // this saves us a whole lot of string compares...
  symtab_entry *entry = symtab_find(evals, expr->type);
  if (!entry) {
    *result = err_result;
    return;
  }

  eval_dispatch disp = (eval_dispatch)entry->val;
  disp(expr, result);
  if (result->sem_type == SEM_TYPE_ERROR) {
    *result = err_result;  // blast any state that may be in there leaving just the error
  }
}

#undef EXPR_INIT
#define EXPR_INIT(x) symtab_add(evals, #x, (void *)eval_ ## x)

// This method loads up the global symbol table and cleans any pending state we had
cql_noexport void eval_init() {
  // restore all globals and statics we own
  eval_cleanup();

  evals = symtab_new();

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
  EXPR_INIT(is_true);
  EXPR_INIT(is_false);
  EXPR_INIT(is_not_true);
  EXPR_INIT(is_not_false);
  EXPR_INIT(cast_expr);
  EXPR_INIT(not);
  EXPR_INIT(tilde);
  EXPR_INIT(uminus);
  EXPR_INIT(case_expr);
}

// the only global state we have is the symbol table, clean that up
cql_noexport void eval_cleanup() {
  SYMTAB_CLEANUP(evals);
}

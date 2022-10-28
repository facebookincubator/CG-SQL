/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Perform codegen of the various nodes to "LUA".

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_CG_LUA)

// stubs to avoid link errors.
cql_noexport void cg_lua_main(ast_node *head) {}
cql_noexport void cg_lua_init(void) {}
cql_noexport void cg_lua_cleanup() {}

#else

#include "ast.h"
#include "bytebuf.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "eval.h"
#include "symtab.h"
#include "encoders.h"
#include "cg_lua.h"

// relevant LUA binding order
#define LUA_EXPR_PRI_ROOT -999
#define LUA_EXPR_PRI_ASSIGN 0
#define LUA_EXPR_PRI_LOR 1
#define LUA_EXPR_PRI_LAND 2
#define LUA_EXPR_PRI_EQ_NE 3
#define LUA_EXPR_PRI_LT_GT 3
#define LUA_EXPR_PRI_BOR  4
#define LUA_EXPR_PRI_BXOR  5 // not used
#define LUA_EXPR_PRI_BAND 6
#define LUA_EXPR_PRI_SHIFT 7
#define LUA_EXPR_PRI_ADD 8
#define LUA_EXPR_PRI_MUL 9
#define LUA_EXPR_PRI_UNARY 10
#define LUA_EXPR_PRI_HIGHEST 999

static void cg_lua_expr(ast_node *node, charbuf *value, int32_t pri);
static void cg_lua_stmt_list(ast_node *node);
static void cg_lua_get_column(sem_t sem_type, CSTR cursor, int32_t index, CSTR var, charbuf *output);
static void cg_lua_binary(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new);
static void cg_lua_is_or_is_not(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new);
static void cg_lua_store_same_type(charbuf *output, CSTR var, sem_t sem_type, CSTR value);
static void cg_lua_store(charbuf *output, CSTR var, sem_t sem_type_var, sem_t sem_type_expr, CSTR value);
static void cg_lua_call_stmt_with_cursor(ast_node *ast, CSTR cursor_name);
static void cg_lua_proc_result_set(ast_node *ast);
static void cg_lua_var_decl(charbuf *output, sem_t sem_type, CSTR base_name);
static void cg_lua_emit_external_arglist(ast_node *expr_list, charbuf *prep, charbuf *invocation, charbuf *cleanup);
static void cg_lua_call_named_external(CSTR name, ast_node *expr_list);
static void cg_lua_user_func(ast_node *ast, charbuf *value);
static void cg_lua_copy(charbuf *output, CSTR var, sem_t sem_type_var, CSTR value);
static void cg_lua_insert_dummy_spec(ast_node *ast);
static void cg_lua_declare_simple_var(sem_t sem_type, CSTR name);
static void cg_lua_put_typecode(charbuf *output, sem_t sem_type);
cql_noexport void cg_lua_init(void);


// lots of AST nodes require no action -- this guy is very good at that.
static void cg_lua_no_op(ast_node * ast) {
}

// Emits a sql statement with bound args.
static void cg_lua_bound_sql_statement(CSTR stmt_name, ast_node *stmt, int32_t cg_lua_exec);

// These globals represent the major state of the code-generator

// True if we are presently emitting a stored proc
static bool_t lua_in_proc = 0;

// True if we are in a loop (hence the statement might run again)
static bool_t lua_in_loop = 0;

// exports file if we are outputing exports
static charbuf *lua_exports_output = NULL;

// The stack level, which facilitates safe re-use of scratch variables.
static int32_t lua_stack_level = 0;

// Case statements might need to generate a unique label for their "else" code
// We count the statements to make an easy label
static int32_t lua_case_statement_count = 0;

// We need a local to hold the (const char *) conversion of a string reference
// when calling out to external code. This gives each such temporary a unique name.
static int32_t lua_temp_cstr_count = 0;

// This tells us if we needed a temporary statement to do an exec or prepare
// with no visible statement result.  If we emitted the temporary we have to
// clean it up.  Examples of this set x := (select 1);   or  DELETE from foo;
static bool_t lua_temp_statement_emitted = 0;

// This tells us if we have already emitted the declaration for the dummy data
// seed variable holder _seed_ in the current context.
static bool_t lua_seed_declared;

// Each catch block needs a unique pair of lables, they are numbered.
static int32_t lua_catch_block_count = 0;

#define CQL_CLEANUP_DEFAULT_LABEL "cql_cleanup"

// In the event of a failure of a sql block or a throw we need to emit
// a goto to the current cleanup target. This is it.  Try/catch manipulate this.
static CSTR lua_error_target = CQL_CLEANUP_DEFAULT_LABEL;

#define CQL_LUA_RCTHROWN_DEFAULT "CQL_OK"  // no variable at the root level, it's just "ok"
// When we need the most recent caught error code we have to use the variable that is
// holding the right value.  Each catch scope has its own corresponding to the error
// that it caught.

static CSTR lua_rcthrown_current = CQL_LUA_RCTHROWN_DEFAULT;
static int32_t lua_rcthrown_index = 0;
static bool_t lua_rcthrown_used = false;

// We set this to true when we have used the error target in the current context
// The current context is either the current procedure or the current try/catch block
// If this is true we need to emit the cleanup label.
static bool_t lua_error_target_used = false;

// We set this to true if a "return" statement happened in a proc.  This also
// forces the top level "cql_cleanup" to be emitted.  We need a different flag for this
// because no matter how deeply nested we are "return" goes to the outermost error target.
// If this is set we will emit that top level target even if there were no other uses.
static bool_t lua_return_used = false;

// We use this table to track named scratch variables that we might need
// this is used in cases where the name has to be computed and there may be several of them
static symtab *lua_named_temporaries;

// The current shared fragment number in the current procdure
static int32_t proc_cte_index;

// This is the mapping between the original parameter name and the aliased name
// for a particular parameter of a particular shared CTE fragment
static symtab *proc_arg_aliases;

// This is the mapping between the original CTE and the aliased name
// for a particular parameter of a particular shared CTE fragment
static symtab *proc_cte_aliases;

// Shared fragment management state
// These are the important fragment classifications, we can use simpler codegen if
// some of these are false.
static bool_t lua_has_conditional_fragments;
static bool_t lua_has_shared_fragments;
static bool_t lua_has_variables;

// Each bound statement in a proc gets a unique index
static int32_t lua_cur_bound_statement;

// this holds the text of the generated SQL broken at fragment boundaries
static bytebuf lua_shared_fragment_strings = {NULL, 0, 0};

// these track the current and max predicate number, these
// correspond 1:1 with a fragment string in the shared_fragment_strings buffer
static int32_t lua_max_fragment_predicate = 0;
static int32_t lua_cur_fragment_predicate = 0;

// these track the current variable count, we snapshot the previous count
// before generating each fragment string so we know how many variables were in there
// we use these to emit the appropriate booleans for each bound variable
static int32_t lua_prev_variable_count;
static int32_t lua_cur_variable_count;
static bool_t lua_continue_label_needed;
static int32_t lua_continue_label_number;
static int32_t lua_continue_label_next;

static bool_t lua_in_inline_function_fragment;

// the current proc name or null
static CSTR lua_current_proc_name() {
  CSTR result = NULL;
  if (current_proc) {
    ast_node *proc_name_ast = get_proc_name(current_proc);
    EXTRACT_STRING(proc_name, proc_name_ast);
    result = proc_name;
  }

  return result;
}

// generate an error if the given expression is true (note this drives tracing)
static void cg_lua_error_on_expr(CSTR expr) {
  bprintf(cg_main_output, "if %s then cql_error_trace(_rc_, _db_); goto %s; end\n", expr, lua_error_target);
  lua_error_target_used = true;
}

// generate an error if the return code is not the required value (helper for common case)
static void cg_lua_error_on_rc_notequal(CSTR required) {
  CHARBUF_OPEN(tmp);
  bprintf(&tmp, "_rc_ ~= %s", required);
  cg_lua_error_on_expr(tmp.ptr);
  CHARBUF_CLOSE(tmp);
}

// generate an error if the return code is not CQL_OK (helper for common case)
static void cg_lua_error_on_not_sqlite_ok() {
  cg_lua_error_on_expr("_rc_ ~= CQL_OK");
}

// This tells us if a subtree should be wrapped in ()
// Basically we know the binding strength of the context (pri) and the current element (pri_new)
// Weaker contexts get parens.  Equal contexts get parens on the right side because all ops
// are left to right associtive in SQL. Stronger child contexts never need parens because
// the operator already binds tighter than its parent in the tree.
static bool_t lua_needs_paren(ast_node *ast, int32_t pri_new, int32_t pri) {
  // if the priorities are different then parens are needed
  // if and only if the new priority (this node) is weaker than the
  // containing priority (the parent node)

  if (pri_new != pri) {
    return pri_new < pri;
  }

  // If equal binding strength, put parens on the right of the expression
  // because our entire world is left associative.
  //
  //  so e.g.  *(a, /(b,c)) becomes a*(b/c);

  return ast->parent->right == ast;
}

// emits a cql_to_num call including a few special cases
// e.g. cql_to_num(true) and cql_to_num(false) are very common
static void cg_lua_emit_to_num(charbuf *output, CSTR input) {
  if (!strcmp("true", input)) {
    bprintf(output, "1");
    return;
  }
  if (!strcmp("false", input)) {
    bprintf(output, "0");
    return;
  }
  if (!strcmp("nil", input)) {
    bprintf(output, "nil");
    return;
  }
  bprintf(output, "cql_to_num(%s)", input);
}

// converts a boolean into a number if necessary
// this is important because stuff like "true + 1 == 2" must be true
static void cg_lua_to_num(sem_t sem_type, charbuf *value) {
  if (is_bool(sem_type)) {
     CHARBUF_OPEN(temp);
     bprintf(&temp, "%s", value->ptr);
     bclear(value);
     cg_lua_emit_to_num(value, temp.ptr);
     CHARBUF_CLOSE(temp);
  }
}

// emits a cql_to_float call
static void cg_lua_emit_to_float(charbuf *output, CSTR input) {
  if (!strcmp("nil", input)) {
    bprintf(output, "nil");
    return;
  }
  bprintf(output, "cql_to_float(%s)", input);
}

// converts a boolean into a number if necessary
// this is important because stuff like "true + 1 == 2" must be true
static void cg_lua_to_float(sem_t sem_type, charbuf *value) {
  if (!is_real(sem_type)) {
     CHARBUF_OPEN(temp);
     bprintf(&temp, "%s", value->ptr);
     bclear(value);
     cg_lua_emit_to_float(value, temp.ptr);
     CHARBUF_CLOSE(temp);
  }
}

// Emits cql_to_bool include special cases for the most common conversions
// 0, 1, and nil all get hard coded treatment, otherwise use the helper.
static void cg_lua_emit_to_bool(charbuf *output, CSTR input) {
  if (!strcmp("1", input)) {
    bprintf(output, "true");
    return;
  }
  if (!strcmp("0", input)) {
    bprintf(output, "false");
    return;
  }
  if (!strcmp("nil", input)) {
    bprintf(output, "nil");
    return;
  }
  bprintf(output, "cql_to_bool(%s)", input);
}

// converts a numeric type into a boolean, this is important because
// in lua 0 is not falsey.  So we must always generate stuff like
// if cql_to_bool(int_expression) if we have a numeric exprecession and
// need a boolean expression.
static void cg_lua_to_bool(sem_t sem_type, charbuf *value) {
  if (!is_bool(sem_type)) {
     CHARBUF_OPEN(temp);
     bprintf(&temp, "%s", value->ptr);
     bclear(value);
     cg_lua_emit_to_bool(value, temp.ptr);
     CHARBUF_CLOSE(temp);
  }
}

// We have a series of masks to remember if we have emitted any given scratch variable.
// We might need several temporaries at the same level if different types appear
// at the same level but in practice we tend not to run into such things.  Mostly
// this works very well at arranging for the same scratch nullable int (or whatever)
// to be re-used in every statement.  The stack depth is limited to bundles of 64bits
//  with thisrepresentation. One bit for each stack level tracks if the temp has been
// generated.  This could be extended if needed...
typedef struct cg_lua_type_masks {
  uint64_t reals[CQL_MAX_STACK/64];
  uint64_t bools[CQL_MAX_STACK/64];
  uint64_t ints[CQL_MAX_STACK/64];
  uint64_t longs[CQL_MAX_STACK/64];
  uint64_t strings[CQL_MAX_STACK/64];
  uint64_t objects[CQL_MAX_STACK/64];
  uint64_t blobs[CQL_MAX_STACK/64];
} cg_lua_type_masks;

// There is one set of masks for nullables and another for not-nullables.
// Lua has very little need of this stuff, switch & case are good examples
typedef struct cg_lua_scratch_masks {
  cg_lua_type_masks nullables;
  cg_lua_type_masks notnullables;
} cg_lua_scratch_masks;

// Any new name context might need new temporaries, this points to the current
// context.  In practice it is set when we start processing a proc and it
// is cleared when we exit that proc.
static cg_lua_scratch_masks *_Nullable cg_lua_current_masks;

// just like it sounds
static void cg_lua_zero_masks(cg_lua_scratch_masks *_Nonnull masks) {
  memset(masks, 0, sizeof(*masks));
}

// Reference types and non-null locals begin at a zero value.  References are especially
// crucial because if they started at something other than null then we would try to
// release that pointer on exit which would be bad.  Note that this means that even
// a non-null text variable (for instance) begins at null when it is initialized.  This is
// much like the _Nonnull clang option which can't prevent a global variable from starting
// at null.  It's a bit weird but there isn't really a viable alternative short of some
// non-null BS value which seems worse.
static void cg_lua_emit_local_init(charbuf *output, sem_t sem_type)
{
  if (is_nullable(sem_type)) {
    // no init needed
    bprintf(output, "\n");
    return;
  }

  sem_t core_type = core_type_of(sem_type);
  switch (core_type) {
    case SEM_TYPE_INTEGER:
    case SEM_TYPE_LONG_INTEGER:
      bprintf(output, " = 0\n");
      break;

    case SEM_TYPE_TEXT:
    case SEM_TYPE_BLOB:
    case SEM_TYPE_OBJECT:
      // no init needed
      bprintf(output, "\n");
      break;

    case SEM_TYPE_REAL:
      bprintf(output, " = 0.0\n");
      break;

    case SEM_TYPE_BOOL:
      bprintf(output, " = false\n");
      break;
   }
}

// Emit a declaration for a local whose name is base_name and whose type
// is given by sem_type.   Is_local really only decides if we add "\n" to
// the end of the output.  This lets us use the same helper for list of
// arg-prototypes as a list of declarations.
// The real "trick" here is:
//  * flags might say it's an output parameter in which case we declare a pointer
//  * flags might indicate nullable, in which case we need the struct version
//  * text is always a reference, nullable or no.  But if you make a text local
//    then we also gotta clean it up.
static void cg_lua_var_decl(charbuf *output, sem_t sem_type, CSTR name) {
  Contract(is_unitary(sem_type));
  Contract(!is_null_type(sem_type));
  Contract(cg_main_output);

  bprintf(output, "local %s", name);
  cg_lua_emit_local_init(output, sem_type);
}

// Sometimes when we need a scratch variable to store an intermediate result
// we can avoid the scratch variable entirely and use the target of the assignment
// in flight for the storage.  For instance:
//   declare x, y integer;
//   set y := 1;
//   set x := case when y == 1 then 3 else 2 end;
//
// A scratch variable is not used to hold the result of the RHS of the set because
// the target of the assignment is known and compatible.
// The target must match the exact type including nullability.  Note bogus
// sensitive assignments or incompatible assignments were already ruled out
// in semantic analysis.
static bool_t lua_is_assignment_target_reusable(ast_node *ast, sem_t sem_type) {
  if (ast && ast->parent && (is_ast_assign(ast->parent) || is_ast_let_stmt(ast->parent))) {
    EXTRACT_ANY_NOTNULL(name_ast, ast->parent->left);
    sem_t sem_type_target = name_ast->sem->sem_type;
    sem_type_target &= (SEM_TYPE_CORE | SEM_TYPE_NOTNULL);
    return sem_type_target == sem_type;
  }
  return false;
}

// The scratch variable helper uses the given sem_type and the current
// stack level to create a temporary variable name for that type at that level.
// If the variable does not already have a declaration (as determined by the masks)
// then a declaration is added to the scratch_vars section.
static void cg_lua_scratch_var(ast_node *ast, sem_t sem_type, charbuf *var, charbuf *value) {
  Contract(is_unitary(sem_type));
  Contract(!is_null_type(sem_type));

  sem_t core_type = core_type_of(sem_type);
  sem_type &= (SEM_TYPE_CORE | SEM_TYPE_NOTNULL);

  Contract(lua_stack_level < CQL_MAX_STACK);

  // try to avoid creating a scratch variable if we can use the target of an assignment in flight.
  if (lua_is_assignment_target_reusable(ast, sem_type)) {
    Invariant(ast && ast->parent && ast->parent->left);
    EXTRACT_ANY_NOTNULL(name_ast, ast->parent->left);
    EXTRACT_STRING(name, name_ast);
    bprintf(var, "%s", name);
  }
  else {
    // Generate a scratch variable name of the correct type.  We don't generate
    // the declaration of any given scratch variable more than once.  We use the
    // current stack level to make the name.  This means that have to burn a stack level
    // if you want more than one scratch.  Stacklevel is normally increased by
    // the CG_LUA_PUSH_EVAL macro which does the recursion but it can also be manually
    // increased if temporaries are needed for some other reason.  Any level of
    // recursion is expected to fix all that.

    CSTR prefix;

    cg_lua_type_masks *pmask;
    if (is_nullable(sem_type)) {
      pmask = &cg_lua_current_masks->nullables;
      prefix = "_tmp_n";
    }
    else {
      pmask = &cg_lua_current_masks->notnullables;
      prefix = "_tmp";
    }

    uint64_t *usedmask = NULL;

    switch (core_type) {
      case SEM_TYPE_INTEGER:
        bprintf(var, "%s_int_%d", prefix, lua_stack_level);
        usedmask = pmask->ints;
        break;
      case SEM_TYPE_BLOB:
        bprintf(var, "%s_blob_%d", prefix, lua_stack_level);
        usedmask = pmask->blobs;
        break;
      case SEM_TYPE_OBJECT:
        bprintf(var, "%s_object_%d", prefix, lua_stack_level);
        usedmask = pmask->objects;
        break;
      case SEM_TYPE_TEXT:
        bprintf(var, "%s_text_%d", prefix, lua_stack_level);
        usedmask = pmask->strings;
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(var, "%s_int64_%d", prefix, lua_stack_level);
        usedmask = pmask->longs;
        break;
      case SEM_TYPE_REAL:
        bprintf(var, "%s_double_%d", prefix, lua_stack_level);
        usedmask = pmask->reals;
        break;
      case SEM_TYPE_BOOL:
        bprintf(var, "%s_bool_%d", prefix, lua_stack_level);
        usedmask = pmask->bools;
        break;
    }

    int32_t index = lua_stack_level/64;
    uint64_t mask = ((uint64_t)1) << (lua_stack_level % 64);

    // Emit scratch if needed.
    if (!(usedmask[index] & mask)) {
      cg_lua_var_decl(cg_scratch_vars_output, sem_type, var->ptr);
      usedmask[index] |= mask;
    }
  }

  // If the value expression is desired, generate them here.
  if (value) {
    bprintf(value, "%s", var->ptr);
  }
}

// Set nullable output type to null.
static void cg_lua_set_null(charbuf *output, CSTR name, sem_t sem_type) {
  bprintf(output, "%s = nil\n", name);
}

// Once we've done any type conversions for the basic types we can do pretty simple assignments
// The nullable non-reference types typically need of the helper macros unless it's an exact-type copy
// operation.  This function is used by cg_lua_store near the finish line.
static void cg_lua_copy(charbuf *output, CSTR var, sem_t sem_type_var, CSTR value) {
  bprintf(output, "%s = %s\n", var, value);
}

// This is most general store function.  Given the type of the destination and the type of the source
// plus the value of the source it generates the correct operation to set it.
// * if storing to a boolean from a non-boolean normalize the result to true/false
// * if storing to a non-boolean from a boolean normalize the result to 0/1
static void cg_lua_store(charbuf *output, CSTR var, sem_t sem_type_var, sem_t sem_type_expr, CSTR value) {
  // dead store -- source = target
  if (!strcmp(var, value)) {
    // dead store -- source = target
    return;
  }

  CHARBUF_OPEN(result);
  bprintf(&result, "%s", value);

  // Normalize floats and bools for storage
  if (is_real(sem_type_var) && !is_real(sem_type_expr)) {
    cg_lua_to_float(sem_type_expr, &result);
  }
  else if (is_bool(sem_type_var) && !is_bool(sem_type_expr)) {
    cg_lua_to_bool(sem_type_expr, &result);
  }
  else if (!is_bool(sem_type_var) && is_bool(sem_type_expr)) {
    cg_lua_to_num(sem_type_expr, &result);
  }

  cg_lua_copy(output, var, sem_type_var, result.ptr);
  CHARBUF_CLOSE(result);
}

// This is a simple helper for store where we know that the type of the thing being stored
// is exactly the same as the type of the thing we are storing.  This is used when we
// just made a temporary of exactly the correct type to hold an expression.  cg_lua_store
// handles this all but this helper lets you specify only one type.
static void cg_lua_store_same_type(charbuf *output, CSTR var, sem_t sem_type, CSTR value) {
  cg_lua_store(output, var, sem_type, sem_type, value);
}

// All the normal (no short-circuit) binary operators
// can be handled the same way.
//   * op is the operator text
//   * value is  the usual outputs
//   * pri is the strength of the caller
//   * pri_new is the strength of "op"
// The helper lua_needs_paren() tells us if we should wrap this subtree in parens (see above)
// If the inputs are not nullable then we can make the easy case of returning the
// result in the value string (and 0 for is null).  Otherwise, cg_lua_combine_nullables
// does the job.
static void cg_lua_binary(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  // left op right
  bool_t force_call = false;

  ast_node *l = ast->left;
  ast_node *r = ast->right;

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  CSTR op_name = ast->type;

  // Integer division in Lua has different truncation policy for negative
  // numbers than C, we have to emulate the C/SQLite behavior
  if (!strcmp(op, "/")) {
    if (core_type_of(sem_type_result) != SEM_TYPE_REAL) {
       // lua integer division operator
       op_name = "idiv";
       force_call = true;
    }
  }

  // Integer mod in lua results in different signs for negative
  // numbers than C. We have to emulate the C/SQLite behavior
  // Mod is only allowed to operate on integer so we don't have to check
  if (!strcmp(op, "%")) {
    force_call = true;
  }

  if (!strcmp(op, "~=")) {
    if (core_type_of(sem_type_right) == SEM_TYPE_BLOB) {
      force_call = true;
      op_name = "blob_ne";
    }
  }

  if (!strcmp(op, "==")) {
    if (core_type_of(sem_type_right) == SEM_TYPE_BLOB) {
      force_call = true;
      op_name = "blob_eq";
    }
  }

  if (!strcmp(op, "like") || !strcmp(op, "not_like")) {
    force_call = true;
  }

  if (sem_type_result == SEM_TYPE_NULL) {
    bprintf(value, "nil");
    return;
  }

  // this hold the formula for the answer
  CG_LUA_PUSH_EVAL(l, pri_new);
  CG_LUA_PUSH_EVAL(r, pri_new);

  cg_lua_to_num(sem_type_left, &l_value);
  cg_lua_to_num(sem_type_right, &r_value);

  if (!strcmp(l_value.ptr, "nil") || !strcmp(r_value.ptr, "nil")) {
    bprintf(value, "nil");
  }
  else if (force_call || is_nullable(sem_type_left) || is_nullable(sem_type_right)) {
    bprintf(value, "cql_%s(%s, %s)", op_name, l_value.ptr, r_value.ptr);
  }
  else {
    if (lua_needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(%s %s %s)", l_value.ptr, op, r_value.ptr);
    }
    else {
      bprintf(value, "%s %s %s", l_value.ptr, op, r_value.ptr);
    }
  }

  CG_LUA_POP_EVAL(r);
  CG_LUA_POP_EVAL(l);
}

// All the "is" operators are the same as binary ops but they do not handle null
// specially.  e.g. null is null evaluates with = with no extra null logic needed
// so they can be handled even more simply than regular binary ops
// can be handled the same way.
//   * op is the operator text
//   * value is  the usual outputs
//   * pri is the strength of the caller
//   * pri_new is the strength of "op"
// The helper lua_needs_paren() tells us if we should wrap this subtree in parens (see above)
// If the inputs are not nullable then we can make the easy case of returning the
// result in the value string (and 0 for is null).
static void cg_lua_is_or_is_not(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  // left op right

  ast_node *l = ast->left;
  ast_node *r = ast->right;

  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  // this hold the formula for the answer
  CG_LUA_PUSH_EVAL(l, pri_new);
  CG_LUA_PUSH_EVAL(r, pri_new);

  cg_lua_to_num(sem_type_left, &l_value);
  cg_lua_to_num(sem_type_right, &r_value);

  if (!strcmp(op, "~=") && core_type_of(sem_type_right) == SEM_TYPE_BLOB) {
    bprintf(value, "cql_blob_is_ne(%s, %s)", l_value.ptr, r_value.ptr);
  }
  else if (!strcmp(op, "==") && core_type_of(sem_type_right) == SEM_TYPE_BLOB) {
    bprintf(value, "cql_blob_is_eq(%s, %s)", l_value.ptr, r_value.ptr);
  }
  else if (lua_needs_paren(ast, pri_new, pri)) {
    bprintf(value, "(%s %s %s)", l_value.ptr, op, r_value.ptr);
  }
  else {
    bprintf(value, "%s %s %s", l_value.ptr, op, r_value.ptr);
  }

  CG_LUA_POP_EVAL(r);
  CG_LUA_POP_EVAL(l);
}

// code gen for expr IS FALSE
// operands already known to be of the correct type so all we have to do is
// check for nullable or not nullable and generate the appropriate code using
// either the helper or just looking at the value
// this must never return nil
static void cg_lua_expr_is_false(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is_false(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  // expr IS FALSE
  sem_t sem_type_is_expr = expr->sem->sem_type;

  // we always put parens because ! is the highest binding, so we can use ROOT, the callee never needs parens
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  if (is_nullable(sem_type_is_expr)) {
    bprintf(value, "cql_is_false(%s)", expr_value.ptr);
  }
  else if (is_bool(sem_type_is_expr)) {
    bprintf(value, "not(%s)", expr_value.ptr);
  }
  else {
    bprintf(value, "(%s == 0)", expr_value.ptr);
  }

  CG_LUA_POP_EVAL(expr);
}

// code gen for expr IS NOT FALSE
// operands already known to be of the correct type so all we have to do is
// check for nullable or not nullable and generate the appropriate code using
// either the helper or just looking at the value
// this must never return nil
static void cg_lua_expr_is_not_false(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is_not_false(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  sem_t sem_type_is_expr = expr->sem->sem_type;

  // expr IS NOT FALSE

  // we always put parens because ! is the highest binding, so we can use ROOT, the callee never needs parens
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  if (is_nullable(sem_type_is_expr)) {
    bprintf(value, "cql_is_not_false(%s)", expr_value.ptr);
  }
  else if (is_bool(sem_type_is_expr)) {
    bprintf(value, "%s", expr_value.ptr);
  }
  else {
    bprintf(value, "(%s ~= 0)", expr_value.ptr);
  }

  CG_LUA_POP_EVAL(expr);
}

// code gen for expr IS TRUE
// operands already known to be of the correct type so all we have to do is
// check for nullable or not nullable and generate the appropriate code using
// either the helper or just looking at the value
// this must never return nil
static void cg_lua_expr_is_true(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is_true(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  sem_t sem_type_is_expr = expr->sem->sem_type;

  // we always put parens because ! is the highest binding, so we can use ROOT, the callee never needs parens
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  if (is_nullable(sem_type_is_expr)) {
    bprintf(value, "cql_is_true(%s)", expr_value.ptr);
  }
  else if (is_bool(sem_type_is_expr)) {
    bprintf(value, "%s", expr_value.ptr);
  }
  else {
    bprintf(value, "(%s ~= 0)", expr_value.ptr);
  }

  CG_LUA_POP_EVAL(expr);
}

// code gen for expr IS NOT TRUE
// operands already known to be of the correct type so all we have to do is
// check for nullable or not nullable and generate the appropriate code using
// either the helper or just looking at the value
// this must never return nil
static void cg_lua_expr_is_not_true(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is_not_true(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  sem_t sem_type_is_expr = expr->sem->sem_type;

  // expr IS NOT TRUE

  // we always put parens because ! is the highest binding, so we can use ROOT, the callee never needs parens
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  if (is_nullable(sem_type_is_expr)) {
    bprintf(value, "cql_is_not_true(%s)", expr_value.ptr);
  }
  else if (is_bool(sem_type_is_expr)) {
    bprintf(value, "not(%s)", expr_value.ptr);
  }
  else {
    bprintf(value, "(%s == 0)", expr_value.ptr);
  }

  CG_LUA_POP_EVAL(expr);
}

// The logical operations are fairly tricky, the code generators for
// each of them are very similar.  In simple cases the direct operator can be
// used, otherwise we wrap the right operand in an anonymous function to defer
// its evaluation and then do the logical op with short circuiting via a helper
static void cg_lua_expr_and_or(ast_node *ast, CSTR str, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_or(ast) || is_ast_and(ast));
  Contract(pri_new == LUA_EXPR_PRI_LOR || pri_new == LUA_EXPR_PRI_LAND);

  EXTRACT_ANY_NOTNULL(l, ast->left);
  EXTRACT_ANY_NOTNULL(r, ast->right);

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  if (is_ast_null(l) && is_ast_null(r)) {
    bprintf(value, "nil");
    return;
  }

  CG_LUA_PUSH_EVAL(l, pri_new);
  CHARBUF_OPEN(right_eval);
  charbuf *saved = cg_main_output;
  cg_main_output = &right_eval;

  CG_LUA_PUSH_EVAL(r, pri_new);
  cg_main_output = saved;

  if (!is_nullable(sem_type_result) && right_eval.used == 1) {
    if (lua_needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(");
    }
    cg_lua_to_bool(sem_type_left, &l_value);
    cg_lua_to_bool(sem_type_right, &r_value);

    bprintf(value, "%s %s %s", l_value.ptr, str, r_value.ptr);

    if (lua_needs_paren(ast, pri_new, pri)) {
      bprintf(value, ")");
    }
  }
  else {
    if (right_eval.used > 1) {
      // multi-line version
      bprintf(value, "cql_shortcircuit_%s(%s,\nfunction()\n%s\nreturn %s\nend\n)",
        str, l_value.ptr, right_eval.ptr, r_value.ptr);
    }
    else {
      // one line version
      bprintf(value, "cql_shortcircuit_%s(%s, function() return %s end)",
        str, l_value.ptr, r_value.ptr);
    }
  }

  CG_LUA_POP_EVAL(r);
  CHARBUF_CLOSE(right_eval);
  CG_LUA_POP_EVAL(l);
}

// The unary operators are handled just like the binary operators.  All of the
// LUA outputs have the form (op arg).  We just have to decide if we need parens.
// We use the same rules for parens here as in other places.  "pri" tells us
// the context of the caller, if it is stronger than our operator then we need parens.
static void cg_lua_unary(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  // op [left]

  EXTRACT_ANY_NOTNULL(expr, ast->left);
  sem_t sem_type_expr = expr->sem->sem_type;

  if (!strcmp(op, "-") && is_ast_num(expr)) {
    // we have to do special code gen for -9223372036854775808
    // to avoid compiler warnings...  This is how the literal
    // gets handled in limits.h as well...
    EXTRACT_NUM_TYPE(num_type, expr);
    EXTRACT_NUM_VALUE(lit, expr);

    if (num_type == NUM_LONG && !strcmp("9223372036854775808", lit)) {
      // emit MIN_LONG in a way that the LUA compiler can accept
      bprintf(value, "(-9223372036854775807 - 1)");
      return;
    }
  }

  CHARBUF_OPEN(result);
  CG_LUA_PUSH_EVAL(expr, pri_new)

  // the NOT operator requires that we normalize to bool
  if (!strcmp(op, "not")) {
    cg_lua_to_bool(sem_type_expr, &expr_value);
  }
  else {
    cg_lua_to_num(sem_type_expr, &expr_value);
  }

  if (lua_needs_paren(ast, pri_new, pri)) {
    bprintf(&result, "(%s%s)", op, expr_value.ptr);
  }
  else {
    // We always add a space to avoid creating "--" or "++"
    // expr_value might be -1 or -x or some such.  This way we're
    // always safe at the cost of a space.
    bprintf(&result, "%s %s", op, expr_value.ptr);
  }

  if (is_not_nullable(sem_type_expr)) {
    bprintf(value, "%s", result.ptr);
  }
  else {
    bprintf(value, "cql_unary_%s(%s)", ast->type, expr_value.ptr);
  }

  CG_LUA_POP_EVAL(expr);
  CHARBUF_CLOSE(result);
}

// sign has a standard helper
static void cg_lua_func_sign(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  // sign ( expr )

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
  bprintf(value, "cql_unary_sign(%s)", expr_value.ptr);
  CG_LUA_POP_EVAL(expr);
}

// abs has a standard helper
static void cg_lua_func_abs(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_ANY_NOTNULL(expr, arg_list->left); // first arg

  // abs ( expr )

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
  bprintf(value, "cql_unary_abs(%s)", expr_value.ptr);
  CG_LUA_POP_EVAL(expr);
}

// This helper generates the tests for each entry in the IN list.
// we generate the appropriate equality test.  We use a helper function
// for blob comparison for flexibility. Note expr is already known
// to be not null here.  There was previous codegen for that case.  The result
// is either bool or nullable bool.
static void cg_lua_in_or_not_in_expr_list(ast_node *head, CSTR expr, CSTR result, sem_t sem_type_result, bool_t is_not_in) {
  Contract(is_bool(sem_type_result));
  CSTR found_value = is_not_in ? "false" : "true";
  CSTR not_found_value = is_not_in ? "true" : "false";

  cg_lua_store_same_type(cg_main_output, result, sem_type_result, found_value);

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(in_expr, ast->left)

    // null can't ever match anything, waste of time.
    if (is_ast_null(in_expr)) {
      continue;
    }

    int32_t lua_stack_level_saved = lua_stack_level;
    CG_LUA_PUSH_EVAL(in_expr, LUA_EXPR_PRI_EQ_NE);

    sem_t sem_type_expr = in_expr->sem->sem_type;

    cg_lua_to_num(sem_type_expr, &in_expr_value);

    if (core_type_of(sem_type_expr) == SEM_TYPE_BLOB) {
      bprintf(cg_main_output, "if cql_blob_eq(%s, %s) then break end\n", expr, in_expr_value.ptr);
    }
    else {
      bprintf(cg_main_output, "if %s == %s then break end\n", expr, in_expr_value.ptr);
    }

    CG_LUA_POP_EVAL(in_expr);

    // This comparison clause fully used any temporaries associated with expr
    // this is kind of like the result variable case, except we didn't store the result
    // we used it in the "if" test, but we're done with it.
    lua_stack_level = lua_stack_level_saved;
  }

  cg_lua_store_same_type(cg_main_output, result, sem_type_result, not_found_value);
}

// The [NOT] IN structure is the simplest of the multi-test forms.
// It's actually a special case of case/when if you like.
// Each item in the [NOT] IN needs to be evaluated because there is no rule
// that says they are constants.
// NOT IN is just a similar reversed check compare IN starting with opposite result value.
// The general pattern for  X IN (U, V) looks like this
//
//  int result;
//  repeat
//    prep statements for X;
//    temp = X;
//    if temp is null then result = null break; end [only needed if X is nullable]
//
//    result = true  /* cg_lua_in_or_not_in_expr_list generates the alternatives */
//    (result = false if NOT IN case)
//
//    prep statements for U;
//    compute U;
//    if (temp == U) break;
//
//    prep statements for V
//    compute V
//    if temp == V then break end
//
//    result = false
//    (result = true if NOT IN case)
//   until true
//
// The result ends up in the is_null and value fields as usual.
static void cg_lua_expr_in_pred_or_not_in(
  ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_in_pred(ast) || is_ast_not_in(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left)
  EXTRACT_NOTNULL(expr_list, ast->right);

  // [expr] [NOT] IN ( [expr_list] )

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;

  if (is_null_type(sem_type_expr)) {
    bprintf(value, "nil");
    return;
  }

  // The answer will be stored in this scratch variable.
  // note: we do not allow the assignment variable to be used because it might be
  // in the candidate list. Since we write to it before we're done the early
  // "result = 1" would kill something like  r := x in (r, b);
  CG_LUA_SETUP_RESULT_VAR(NULL, sem_type_result);

  bprintf(cg_main_output, "repeat\n");

  CG_PUSH_MAIN_INDENT(do, 2);

  // Evaluate the expression and stow it in a temporary.
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
  CG_LUA_PUSH_TEMP(temp, sem_type_expr);

  cg_lua_to_num(sem_type_expr, &expr_value);

  // Copy the expression, we can't evaluate it more than once, so stow it.
  cg_lua_store_same_type(cg_main_output, temp.ptr, sem_type_expr, expr_value.ptr);

  // If the expression is null the result is null
  if (is_nullable(sem_type_expr)) {
    bprintf(cg_main_output, "if %s == nil then\n", temp_value.ptr);
    bprintf(cg_main_output, "  ");
    cg_lua_set_null(cg_main_output, result_var.ptr, sem_type_result);
    bprintf(cg_main_output, "  break\n");
    bprintf(cg_main_output, "end\n");
  }

  // Now generate the list
  cg_lua_in_or_not_in_expr_list(expr_list, temp_value.ptr, result_var.ptr, sem_type_result, is_ast_not_in(ast));

  CG_LUA_POP_TEMP(temp);
  CG_LUA_POP_EVAL(expr);
  CG_POP_MAIN_INDENT(do);
  CG_LUA_CLEANUP_RESULT_VAR();

  bprintf(cg_main_output, "until true\n");
}

// This helper method emits the alternatives for the case.  If there was an
// expression the temporary holding the expression is in expr.  Expr has
// already been tested for null if that was a possibility so we only need its
// value at this point.
static void cg_lua_case_list(ast_node *head, CSTR expr, CSTR result, sem_t sem_type_result) {
  Contract(is_ast_case_list(head));

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_NOTNULL(when, ast->left);
    EXTRACT_ANY_NOTNULL(case_expr, when->left);
    EXTRACT_ANY_NOTNULL(then_expr, when->right);

    // null can't ever match anything, waste of time.
    if (is_ast_null(case_expr)) {
      continue;
    }

    // WHEN [case_expr] THEN [then_expr]

    sem_t sem_type_then_expr = then_expr->sem->sem_type;
    sem_t sem_type_case_expr = case_expr->sem->sem_type;

    int32_t lua_stack_level_saved = lua_stack_level;
    CG_LUA_PUSH_EVAL(case_expr, LUA_EXPR_PRI_EQ_NE);

    if (expr) {
      bprintf(cg_main_output, "if %s == %s then\n", expr, case_expr_value.ptr);
    }
    else {
      cg_lua_to_bool(sem_type_case_expr, &case_expr_value);
      bprintf(cg_main_output, "if %s then\n", case_expr_value.ptr);
    }
    CG_LUA_POP_EVAL(case_expr);

    // The comparison above clause fully used any temporaries associated with expr
    lua_stack_level = lua_stack_level_saved;

    CG_PUSH_MAIN_INDENT(then, 2);
    CG_LUA_PUSH_EVAL(then_expr, LUA_EXPR_PRI_ROOT);

    cg_lua_store(cg_main_output, result, sem_type_result, sem_type_then_expr, then_expr_value.ptr);
    bprintf(cg_main_output, "break\n");

    CG_LUA_POP_EVAL(then_expr);
    CG_POP_MAIN_INDENT(then);
    bprintf(cg_main_output, "end\n");

    // This 'then' clause stored its result, temporaries no longer needed
    // This is just like the result variable case
    lua_stack_level = lua_stack_level_saved;
  }
}

// Case looks a lot like IN except the net result is computed at each step
// and the test is different at each step.  It's a straight generalization.
//
// Case X when U then R1 when V then R2 else R3 end;
//
//   declare result (whatever type holds R1, R2, and R3)
//
//   repeat
//     statements to evaluate X
//     temp = X
//     [ if temp is null then goto case_else end ] optional if temp is nullable
//
//     statements to evaluate U
//     if temp == U then
//       statements to evaluate R1
//       result = R1
//       break
//     end
//
//     statements to evaluate V
//     if temp == V then
//       statements to evaluate R2
//       result = R2
//       break
//     end
//
//   ::case_else::
//     statements to evaluate R3;
//     result = R3
//   until true
//
// If the X is omitted then U and V are normal boolean expressions and
// the code becomes if (U) etc  if (V) etc. with no temp.
static void cg_lua_expr_case(ast_node *case_expr, CSTR str, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_case_expr(case_expr));
  EXTRACT_ANY(expr, case_expr->left);
  EXTRACT_NOTNULL(connector, case_expr->right);
  EXTRACT_NOTNULL(case_list, connector->left);
  EXTRACT_ANY(else_expr, connector->right);

  // if we need an else label, this will hold the value.
  int32_t else_label_number = -1;

  sem_t sem_type_result = case_expr->sem->sem_type;

  // CASE [expr]? [case_list] ELSE [else_expr] END

  // The answer will be stored in this scratch variable, any type is possible
  CG_LUA_SETUP_RESULT_VAR(case_expr, sem_type_result);

  bprintf(cg_main_output, "repeat\n");

  CG_PUSH_MAIN_INDENT(do, 2);

  // if the form is case expr when ... then save the expr in a temporary
  if (expr) {
    sem_t sem_type_expr = expr->sem->sem_type;
    CG_LUA_PUSH_TEMP(temp, sem_type_expr);

    int32_t lua_stack_level_saved = lua_stack_level;

    // Compute the value of the expression.
    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_EQ_NE);

    // Store it in the temporary we just made, which has the exact correct type (we just made it)
    cg_lua_store_same_type(cg_main_output, temp.ptr, sem_type_expr, expr_value.ptr);

    // here "temp" is like a mini-result variable... anything from expr can be released
    // we only need temp now, so restore to that level.
    lua_stack_level = lua_stack_level_saved;

    // If the expression is null, then we go to the else logic.  Note: there is always else logic
    // either the user provides it or we do (to use null as the default).
    if (is_nullable(sem_type_expr)) {
      else_label_number = ++lua_case_statement_count;
      bprintf(cg_main_output, "if %s == nil then ", temp_value.ptr);
      bprintf(cg_main_output, "goto case_else_%d end\n", else_label_number);
    }

    cg_lua_case_list(case_list, temp_value.ptr, result_var.ptr, sem_type_result);

    CG_LUA_POP_EVAL(expr);
    CG_LUA_POP_TEMP(temp);
  }
  else {
    // Otherwise do the case list with no expression...
    cg_lua_case_list(case_list, NULL, result_var.ptr, sem_type_result);
  }

  if (else_label_number >= 0) {
    bprintf(cg_main_output, "::case_else_%d::\n", else_label_number);
  }

  // If there is an else clause, spit out the result for that now.
  // Note that lack of an else is by-construction a nullable outcome because
  // the semantics of case say that if you miss all the cases you get null.
  if (else_expr) {
    sem_t sem_type_else = else_expr->sem->sem_type;

    CG_LUA_PUSH_EVAL(else_expr, LUA_EXPR_PRI_ROOT);

    cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_else, else_expr_value.ptr);

    CG_LUA_POP_EVAL(else_expr);
  }
  else {
    // No else, result must be nullable. (enforced by cg_lua_set_null)
    cg_lua_set_null(cg_main_output, result_var.ptr, sem_type_result);
  }

  CG_POP_MAIN_INDENT(do);
  CG_LUA_CLEANUP_RESULT_VAR();

  bprintf(cg_main_output, "until true\n");
}

// we have built-in support for numeric casts only, the SQL string cast operations are highly
// complex with interesting parsing rules and so forth.  We don't try to do those at all
// but there's no reason we can't do the simple numeric conversions in the non-SQL path
static void cg_lua_expr_cast(ast_node *cast_expr, CSTR str, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_cast_expr(cast_expr));

  ast_node *expr = cast_expr->left;

  sem_t sem_type_result = cast_expr->sem->sem_type;
  sem_t core_type_result = core_type_of(sem_type_result);
  sem_t sem_type_expr = expr->sem->sem_type;
  sem_t core_type_expr = core_type_of(sem_type_expr);

  CG_LUA_PUSH_EVAL(expr, pri_new);

  if (core_type_expr == core_type_result) {
    // no-op cast, just pass through -- we have to add parens because they were
    // implicit in the call syntax of cast -- so this is the safe/easy thing to do
    bprintf(value, "(%s)", expr_value.ptr);
  }
  else switch (core_type_result) {
    case SEM_TYPE_INTEGER:
    case SEM_TYPE_LONG_INTEGER:
      bprintf(value, "cql_to_integer(%s)", expr_value.ptr);
      break;

    case SEM_TYPE_REAL:
      bprintf(value, "cql_to_float(%s)", expr_value.ptr);
      break;

    case SEM_TYPE_BOOL:
      bprintf(value, "cql_to_bool(%s)", expr_value.ptr);
      break;
  }

  CG_LUA_POP_EVAL(expr);
}

// This converts from SQL string literal format to C literal format.
//  * the single quotes around the string become double quotes
//  * escaped single quote becomes just single quote
//  * backslash escapes are preserved
static void cg_lua_requote_literal(CSTR str, charbuf *output) {
  CHARBUF_OPEN(plaintext);
  cg_decode_string_literal(str, &plaintext);
  cg_encode_c_string_literal(plaintext.ptr, output);
  CHARBUF_CLOSE(plaintext);
}

// Here we use the helper above to create a variable name for the literal
// then we declare that variable and emit the initializer.  The macro
// cql_string_literal does the job for us while allowing the different
// string implementations.  These go into the constants section.
static void cg_lua_string_literal(CSTR str, charbuf *output) {
  Contract(str);
  Contract(str[0] == '\'');

  // no fancy tricks, just emit the string
  cg_lua_requote_literal(str, output);
}

// The rewritten between expression is designed to be super easy to code gen.
// The semantic analyzer has already turned the between or not beween into a normal
// combination of and/or so all we have to do is load up the temporary with the test
// value and then evaluate the test expression. Between and not between look the same
// to the codgen (they will have different expressions).  This lets us get all that
// weird short circuit behavior super easy.  It's literally the AND/OR code running.
static void cg_lua_expr_between_rewrite(
  ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_between_rewrite(ast));
  EXTRACT_NOTNULL(range, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT_STRING(var, range->left);
  EXTRACT_ANY_NOTNULL(test, range->right);

  // BETWEEN REWRITE [var := expr] CHECK [test]

  sem_t sem_type_var = expr->sem->sem_type;

  if (is_ast_null(expr)) {
    bprintf(value, "nil");
    return;
  }

  cg_lua_var_decl(cg_declarations_output, sem_type_var, var);

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
  cg_lua_store_same_type(cg_main_output, var, sem_type_var, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);

  cg_lua_expr(test, value, pri);
}

// This is the first of the key primitives in codegen -- it generates the
// output buffers for an identifier.  There are a few interesting cases.
//
//   * LUA identifiers are very simple, we don't need structs or temps so
//     we can simply emit the name with no changes
//   * we have special case code for the @RC identifier for the most recent result code
//   * we have to undo the cursor transform _C_has_row_ into C._has_row_ because
//     cursors are uniform in LUA, this is goofy but works for now
//   * when processing shared fragments we might need to alias local variables
//     to their computed value
//
// Note: It's important to use the semantic name sem->name rather than the text
// of the ast because the user might refer case insensitively to the variable FoO
// and we need to emit the canonical name (e.g. foo, or Foo, or whatever it was).
static void cg_lua_id(ast_node *expr, charbuf *value) {
  sem_t sem_type = expr->sem->sem_type;
  Invariant(is_variable(sem_type));

  // Crucial, we want the canonical version of the name, not any MixED case version
  // the user might have typed.
  CSTR name = expr->sem->name;

  // map the logical @rc variable to the correct saved version
  if (!strcmp(name, "@rc")) {
    bprintf(value, "%s", lua_rcthrown_current);
    lua_rcthrown_used = true;
    return;
  }

  // The semantic pass changed something like 'if C' into if '_C_has_row_' which
  // is a bit lame but I can't fix it just now. The cg pass should be doing this
  // transform with a dot operator but in any case we can undo it with a kludge.
  if (name[0] == '_') {
     int32_t len = (int32_t)strlen(name);
     int32_t plen = sizeof("_has_row_") - 1;
     int32_t index = len - plen;
     if (len > plen && strcmp(name + index, "_has_row_") == 0) {
       for (int32_t i = 1; i < index; i++) {
         bputc(value, name[i]);
       }
       bprintf(value, "._has_row_");
       return;
     }
  }

  // while generating expressions for the CTE assignments we might have to
  // rename the proc args to the name in the outermost context
  if (proc_arg_aliases) {
    symtab_entry *entry = symtab_find(proc_arg_aliases, name);
    if (entry) {
      EXTRACT_ANY_NOTNULL(var, entry->val);
      name = var->sem->name;
    }
  }

  bprintf(value, "%s", name);
}

// Recall that coalesce returns the first non-null arg from the list of arguments.
// The arguments must be type compatible, this was previously verified.  To do
// the codgen for coalesce(X,Y) we use a pattern like this:
//   declare result of the appropriate type;
//   repeat
//     evaluate X
//     if x is not null then
//       result = X  -- we can use the form where  X is known to be not null
//       break       -- we're done...
//     end
//     ... other cases just like the above...
//     ... the final case has no test, use it even if null
//     evaluate Y
//     result = Y
//   until true
static void cg_lua_func_coalesce(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // ifnull ( [arg_list] )
  // coalesce ( [arg_list] )

  sem_t sem_type_result = call_ast->sem->sem_type;

  // the answer will be stored in this scratch variable
  CG_LUA_SETUP_RESULT_VAR(call_ast, sem_type_result);

  bprintf(cg_main_output, "repeat\n");
  CG_PUSH_MAIN_INDENT(do, 2);
  for (ast_node *ast = arg_list; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(expr, ast->left);

    sem_t sem_type_expr = expr->sem->sem_type;

    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

    // Generate the test for all but the last choice.
    if (ast->right) {
      bprintf(cg_main_output, "if %s ~= nil then\n  ", expr_value.ptr);
    }

    cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_expr, expr_value.ptr);

    if (ast->right) {
      bprintf(cg_main_output, "  break\n");
      bprintf(cg_main_output, "end\n");
    }

    CG_LUA_POP_EVAL(expr);
  }
  CG_POP_MAIN_INDENT(do);
  bprintf(cg_main_output, "until true\n");
  CG_LUA_CLEANUP_RESULT_VAR();
}

// Ifnull is an alias for coalesce, with only two args.
static void cg_lua_func_ifnull(ast_node *call_ast, charbuf *value) {
  cg_lua_func_coalesce(call_ast, value);
}

// no-op function, we just force parents to not screw up the order of ops
static void cg_lua_func_sensitive(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // sensitive ( any expression ) -- at run time this function is a no-op
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  // we just evaluate the inner expression
  // we have to fake a high binding strength so that it will for sure emit parens
  // as the nullable() construct looks like has parens and we don't know our context
  // oh well, extra parens is better than the temporaries of doing this with PUSH_EVAL etc.
  cg_lua_expr(expr, value, LUA_EXPR_PRI_HIGHEST);
}

// no-op function, we just force parents to not screw up the order of ops
static void cg_lua_func_nullable(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // nullable ( any expression ) -- at run time this function is a no-op
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  // we just evaluate the inner expression
  // we have to fake a high binding strength so that it will for sure emit parens
  // as the nullable() construct looks like has parens and we don't know our context
  // oh well, extra parens is better than the temporaries of doing this with PUSH_EVAL etc.
  cg_lua_expr(expr, value, LUA_EXPR_PRI_HIGHEST);
}

typedef enum {
  LUA_ATTEST_NOTNULL_VARIANT_CRASH,
  LUA_ATTEST_NOTNULL_VARIANT_INFERRED,
  LUA_ATTEST_NOTNULL_VARIANT_THROW,
} lua_attest_notnull_variant;

// Generates code for all functions of the attest_notnull family.
static void cg_lua_func_attest_notnull(ast_node *call_ast, charbuf *value, lua_attest_notnull_variant variant) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // notnull ( a_nullable_expression )

  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  // result known to be not null so easy codegen

  sem_t sem_type_expr = expr->sem->sem_type;
  Invariant(is_nullable(sem_type_expr));  // expression must already be in a temp

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  switch (variant) {
    case LUA_ATTEST_NOTNULL_VARIANT_CRASH:
      bprintf(cg_main_output, "cql_invariant(%s ~= nil)\n", expr_value.ptr);
      break;

    case LUA_ATTEST_NOTNULL_VARIANT_INFERRED:
      // Semantic analysis has guaranteed that the input is not going to be
      // NULL so we don't need to check anything here.
      break;

    case LUA_ATTEST_NOTNULL_VARIANT_THROW:
      bprintf(cg_main_output, "if %s == nil then\n", expr_value.ptr);
      bprintf(cg_main_output, "  _rc_ = CQL_ERROR\n");
      bprintf(cg_main_output, "  cql_error_trace(_rc_, _db_)\n");
      bprintf(cg_main_output, "  goto %s\n", lua_error_target);
      bprintf(cg_main_output, "end\n");
      lua_error_target_used = true;
      break;
  }

  bprintf(value, "%s", expr_value.ptr);

  CG_LUA_POP_EVAL(expr);
}

static void cg_lua_func_ifnull_throw(ast_node *call_ast, charbuf *value) {
  cg_lua_func_attest_notnull(call_ast, value, LUA_ATTEST_NOTNULL_VARIANT_THROW);
}

static void cg_lua_func_ifnull_crash(ast_node *call_ast, charbuf *value) {
  cg_lua_func_attest_notnull(call_ast, value, LUA_ATTEST_NOTNULL_VARIANT_CRASH);
}

// The `cql_inferred_notnull` function is not used by the programmer directly,
// but rather inserted via a rewrite during semantic analysis to coerce a value
// of a nullable type to be nonnull. The reason for this approach, as opposed to
// just changing the type directly, is that there are also representational
// differences between values of nullable and nonnull types; some conversion is
// required.
static void cg_lua_func_cql_inferred_notnull(ast_node *call_ast, charbuf *value) {
  cg_lua_func_attest_notnull(call_ast, value, LUA_ATTEST_NOTNULL_VARIANT_INFERRED);
}

// This is a no-op for now, that is no compression.
// i.e. lua codegen doesn't have compressed string forms yet so we just emit a normal literal
static void cg_lua_func_cql_compressed(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);
  EXTRACT_STRING(str, expr);

  cg_lua_string_literal(str, value);
}

// There's a helper for this method, just call it.  Super easy.
static void cg_lua_func_changes(ast_node *ast, charbuf *value) {
  bprintf(value, "cql_changes(_db_)");
}

// There's a helper for this method, just call it.  Super easy.
static void cg_lua_func_last_insert_rowid(ast_node *ast, charbuf *value) {
  bprintf(value, "cql_last_insert_rowid(_db_)");
}

// Printf also has a helper, we just call it.  There are other helpers to emit
// a call to an external (not stored proc) function.  Use that.
static void cg_lua_func_printf(ast_node *call_ast, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  CG_LUA_SETUP_RESULT_VAR(call_ast, SEM_TYPE_TEXT | SEM_TYPE_NOTNULL);
  CHARBUF_OPEN(inv);
  bprintf(&inv, "%s = cql_printf", result_var.ptr);
  cg_lua_call_named_external(inv.ptr, arg_list);
  CHARBUF_CLOSE(inv);
  CG_LUA_CLEANUP_RESULT_VAR();
}

// wrapper function for the builtin cql_get_blob_size
// this is super simple in LUA because the nullable case is the same as the not nullable case
static void cg_lua_func_cql_get_blob_size(ast_node *ast, charbuf *value) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  // store cql_get_blob_size call in temp. e.g: cql_get_blob_size(expr_value)
  bprintf(value, "cql_get_blob_size(%s)", expr_value.ptr);

  CG_LUA_POP_EVAL(expr);
}

// This is some kind of function call in an expression context.  Look up the method
// and call one of the cg_lua_func_* workers above.  All arg combos are known to be good
// because semantic analysis verified them already.
static void cg_lua_expr_call(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  // name( [arg_list] )

  if (find_func(name) || find_proc(name)) {
    cg_lua_user_func(ast, value);
  }
  else {
    symtab_entry *entry = symtab_find(cg_funcs, name);
    Invariant(entry);  // names have already been verified!
    ((void (*)(ast_node *, charbuf *))entry->val)(ast, value);
  }
}

// Numeric literal, spit it out.
static void cg_lua_expr_num(ast_node *expr, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_num(expr));
  EXTRACT_NUM_TYPE(num_type, expr);
  EXTRACT_NUM_VALUE(lit, expr);

  // a numeric literal
  if (num_type == NUM_BOOL) {
    // in LUA, the codegen for bools has to be "true" "false" because 0 is not falsey
    bprintf(value, "%s", strcmp(lit, "0") ? "true" : "false");
  }
  else {
    bprintf(value, "%s", lit);
  }
}

// string nodes are simple in LUA as we can generate simple ids or string literals consistently
static void cg_lua_expr_str(ast_node *expr, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  // String could be an id, or a literal -- literals start with single quote.
  Contract(is_ast_str(expr));
  EXTRACT_STRING(str, expr);
  if (is_strlit(expr)) {
    // Note str is the lexeme, so it is still quoted and escaped.
    cg_lua_string_literal(str, value);
  }
  else {
    cg_lua_id(expr, value);
  }
}

// the "dot" operator (e.g. C.x) is handled on the ID path
static void cg_lua_expr_dot(ast_node *expr, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  // X.Y has a net local name computed by semantic analysis.  Use it like any other id.
  Contract(is_ast_dot(expr));
  cg_lua_id(expr, value);
}

// the null constant
static void cg_lua_expr_null(ast_node *expr, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_null(expr));
  // null literal
  bprintf(value, "nil");
}

// This is the main entry point for codegen of an expression.  It dispatches
// to one of the above workers for all the complex types and handles a few primitives
// in place.
static void cg_lua_expr(ast_node *expr, charbuf *value, int32_t pri) {
  Contract(value);
  Contract(value->used == 1);  // just the null (i.e. empty buffer)

  // These are all the expressions there are, we have to find it in this table
  // or else someone added a new expression type and it isn't supported yet.
  symtab_entry *entry = symtab_find(cg_exprs, expr->type);
  Invariant(entry);
  cg_lua_expr_dispatch *disp = (cg_lua_expr_dispatch *)entry->val;
  disp->func(expr, disp->str, value, pri, disp->pri_new);
}

// This is a nested select expression.  To evaluate we will
//  * prepare a temporary to hold the result
//  * generate the bound SQL statement
//  * extract the exactly one argument into the result variable
//    which is of exactly the right type
//  * use that variable as the result.
// The helper methods take care of sqlite error management.
static void cg_lua_expr_select(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_select_stmt(ast));

  // SELECT [select_opts] [select_expr_list_con]

  sem_t sem_type_result = ast->sem->sem_type;

  CG_LUA_SETUP_RESULT_VAR(ast, sem_type_result);

  cg_lua_bound_sql_statement(NULL, ast, CG_PREPARE | CG_MINIFY_ALIASES);

  // exactly one column is allowed, already checked in semantic analysis, fetch it
  bprintf(cg_main_output, "_rc_ = cql_step(_temp_stmt)\n");
  cg_lua_error_on_rc_notequal("CQL_ROW");
  cg_lua_get_column(sem_type_result, "_temp_stmt", 0, result_var.ptr, cg_main_output);
  bprintf(cg_main_output, "cql_finalize_stmt(_temp_stmt)\n");
  bprintf(cg_main_output, "_temp_stmt = nil\n");

  CG_LUA_CLEANUP_RESULT_VAR();
}

// select if nothing is exactly the same codegen as regular select
// the throwing which is done by default was make explcit.  The normal
// codegen already does the "throw" (i.e. goto the current error target).
static void cg_lua_expr_select_if_nothing_throw(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_select_if_nothing_throw_expr(ast));
  EXTRACT_ANY_NOTNULL(select_expr, ast->left);
  cg_lua_expr_select(select_expr, op, value, pri, pri_new);
}

// This helper does the evaluation of the select statement portion of the
// (SELECT ... IF NOTHING ...) forms.  Importantly the result type of the
// select might not exactly match the result type of expression because
// the default value could be of a different type and it might cause the
// overall expression to be not null.  So here we have to fetch just the
// select statement part into its own result variable of the exact correct type
// later we will safely assign that result to the final type if it held a value
static void cg_lua_expr_select_frag(ast_node *ast, charbuf *value) {
  sem_t sem_type_result = ast->sem->sem_type;

  CG_LUA_SETUP_RESULT_VAR(ast, sem_type_result);

  cg_lua_bound_sql_statement(NULL, ast, CG_PREPARE | CG_MINIFY_ALIASES);

  // exactly one column is allowed, already checked in semantic analysis, fetch it
  bprintf(cg_main_output, "_rc_ = cql_step(_temp_stmt)\n");
  cg_lua_error_on_expr("_rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE");
  bprintf(cg_main_output, "if _rc_ == CQL_ROW then\n");
  cg_lua_get_column(sem_type_result, "_temp_stmt", 0, result_var.ptr, cg_main_output);

  CG_LUA_CLEANUP_RESULT_VAR();

  // note that callers are expected to check the remaining error codes and clean up
  // the temp statement.
}

// This is a nested select expression.  To evaluate we will
//  * prepare a temporary to hold the result
//  * generate the bound SQL statement
//  * extract the exactly one argument into the result variable
//    which is of exactly the right type
//  * use that variable as the result.
//  * if there is no row, we use the default expression
// The helper methods takes care of sqlite error management.
static void cg_lua_expr_select_if_nothing(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_select_if_nothing_expr(ast));

  EXTRACT_ANY_NOTNULL(select_stmt, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  // SELECT [select_opts] [select_expr_list_con] IF NOTHING expr

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;
  sem_t sem_type_select = select_stmt->sem->sem_type;

  // this is the overall result
  CG_LUA_SETUP_RESULT_VAR(ast, sem_type_result);

  CHARBUF_OPEN(select_value);

  // the select statement might have a different result type than overall
  // e.g. (select an_int from somewhere if nothing 2.5), the overall result is real
  cg_lua_expr_select_frag(select_stmt, &select_value);

  // we're inside of the "if __rc__ == CQL_ROW then" case
  // we need to store the result of the select in our output variable
  // note that these are known to be compatible (already verified) but they might not
  // be the exact same type, hence the copy.  In this case we're definitely using the value.
  bprintf(cg_main_output, "  ");
  cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_select, select_value.ptr);

  bprintf(cg_main_output, "else\n  ");

  // if no row found, then evaluate and use the default
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
  cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_expr, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);

  bprintf(cg_main_output, "end\n");
  bprintf(cg_main_output, "cql_finalize_stmt(_temp_stmt)\n");
  bprintf(cg_main_output, "_temp_stmt = nil\n");

  CHARBUF_CLOSE(select_value);

  CG_LUA_CLEANUP_RESULT_VAR();
}

// This is a nested select expression.  To evaluate we will
//  * prepare a temporary to hold the result
//  * generate the bound SQL statement
//  * extract the exactly one argument into the result variable
//    which is of exactly the right type
//  * use that variable as the result.
//  * if there is no row, or the returned value is null we use the default expression
// The helper methods take care of sqlite error management.
static void cg_lua_expr_select_if_nothing_or_null(ast_node *ast, CSTR op, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_select_if_nothing_or_null_expr(ast));

  EXTRACT_ANY_NOTNULL(select_stmt, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  // SELECT [select_opts] [select_expr_list_con] IF NOTHING expr

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;
  sem_t sem_type_select = select_stmt->sem->sem_type;

  CG_LUA_SETUP_RESULT_VAR(ast, sem_type_result);

  CHARBUF_OPEN(select_value);

  // the select statement might have a different result type than overall
  // e.g. (select an_int from somewhere if nothing 2.5), the overall result is real
  cg_lua_expr_select_frag(select_stmt, &select_value);

  // we're inside of the "if _rc_ == CQL_ROW then" case
  // in this variation we have to first see if the result is null before we use it
  bprintf(cg_main_output, "end\n");
  bprintf(cg_main_output, "if _rc_ == CQL_DONE or %s == nil then\n  ", select_value.ptr);

  // now row or null result, evaluate the default
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
  cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_expr, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);

  bprintf(cg_main_output, "else\n  ");
  // ok to use the value we fetched, go ahead an copy it to its final destination
  // note this may change the type but only in a compatible way
  cg_lua_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_select, select_value.ptr);
  bprintf(cg_main_output, "end\n");
  bprintf(cg_main_output, "_rc_ = CQL_OK\n");
  bprintf(cg_main_output, "cql_finalize_stmt(_temp_stmt)\n");
  bprintf(cg_main_output, "_temp_stmt = nil\n");

  CHARBUF_CLOSE(select_value);

  CG_LUA_CLEANUP_RESULT_VAR();
}

// This is the elementary piece of the if-then construct, it's one condition
// and one statement list.  It can happen in the context of the top level
// if or any else-if.  In lua 0 is not falsey so we have to be sure to
// convert numerics to bools but otherwise things are very easy/normal.
// Nil is falsey so no issues there.
//
// > if nil then print("truthy") end;
// > if 0 then print("truthy") end;
// truthy
// > if 1 then print("truthy") end;
// truthy
// if false then print("truthy") end;
//
static void cg_lua_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  // [expr ast->left] THEN stmt_list

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  sem_t sem_type_expr = expr->sem->sem_type;

  cg_lua_to_bool(sem_type_expr, &expr_value);

  bprintf(cg_main_output, "if %s then\n", expr_value.ptr);

  CG_LUA_POP_EVAL(expr);

  if (stmt_list) {
    cg_lua_stmt_list(stmt_list);
  }
}

// Recursively emits the else-if chain.  These have to nest to allow for
// expressions to generate statements.
static void cg_lua_elseif_list(ast_node *ast, ast_node *elsenode) {
  if (ast) {
    Contract(is_ast_elseif(ast));
    EXTRACT(cond_action, ast->left);

    // ELSE IF [cond_action]
    bprintf(cg_main_output, "else\n");
      CG_PUSH_MAIN_INDENT(else, 2);
      cg_lua_cond_action(cond_action);
      cg_lua_elseif_list(ast->right, elsenode);
      CG_POP_MAIN_INDENT(else);
  }
  else if (elsenode) {
    Contract(is_ast_else(elsenode));
    // ELSE [stmt_list]
    EXTRACT(stmt_list, elsenode->left);
    bprintf(cg_main_output, "else\n");
    cg_lua_stmt_list(stmt_list);
  }

  bprintf(cg_main_output, "end\n");
}

// As with the other cases the fact that expressions might require statements
// complicates the codegen. If there is an else-if (expression) that expression
// might itself require statements to compute the expression.  Even a logical AND
// might require statements if there is nullability involved.
// That means the overall pattern has to look like this, with nesting.
//
//   prep statements;
//   result = final expression
//   if result then
//     statements
//   else
//     prep statements;
//     result = final expression
//     if result then
//       statements
//     else
//      statements
//     end
//   end
static void cg_lua_if_stmt(ast_node *ast) {
  Contract(is_ast_if_stmt(ast));

  EXTRACT_NOTNULL(cond_action, ast->left);
  EXTRACT_NOTNULL(if_alt, ast->right);

  // IF [cond_action] [if_alt]
  cg_lua_cond_action(cond_action);

  EXTRACT(elseif, if_alt->left);
  EXTRACT_NAMED(elsenode, else, if_alt->right);
  cg_lua_elseif_list(elseif, elsenode);

  // END IF
}

// This code uses the same cg_lua_store helper method to do an assignment as
// is used all over the place for assigning to scratch variables.  All
// we have to do here is pull the name and types out of the ast.
static void cg_lua_assign(ast_node *ast) {
  Contract(is_ast_assign(ast) || is_ast_let_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  CSTR name = name_ast->sem->name;  // crucial: use the canonical name not the specified name

  Contract(lua_stack_level == 0);

  // SET [name] := [expr]

  sem_t sem_type_var = name_ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
  cg_lua_store(cg_main_output, name, sem_type_var, sem_type_expr, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);
}

// In the LET statement, we declare the variable based on type, emit that
// then do the usual SET codegen.
static void cg_lua_let_stmt(ast_node *ast) {
  Contract(is_ast_let_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);

  cg_lua_declare_simple_var(name_ast->sem->sem_type, name);
  cg_lua_assign(ast);
}

// Walk all the params of a stored proc and emit each one with a comma where needed.
static void cg_lua_params(ast_node *ast, charbuf *decls, charbuf *returns) {
  Contract(is_ast_params(ast));

  while (ast) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    sem_t sem_type  = param->sem->sem_type;
    if (is_in_parameter(sem_type)) {
      if (decls->used > 1) {
        bprintf(decls, ", ");
      }
      bprintf(decls, "%s", param->sem->name);
    }

    if (is_out_parameter(sem_type)) {
      if (returns->used > 1) {
        bprintf(returns, ", ");
      }
      bprintf(returns, "%s", param->sem->name);
    }

    ast = ast->right;
  }
}

// Emit any initialization code needed for the parameters
// in particular out parameters assume that there is garbage
// in the out location, so they hammer a NULL or 0 into that slot.
static void cg_lua_param_init(ast_node *ast, charbuf *body) {
  Contract(is_ast_param(ast));
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, param_detail->left)
  EXTRACT_STRING(name, name_ast);

  // [in out] name [datatype]

  sem_t sem_type = name_ast->sem->sem_type;

  // In a proc decl the out arg initialized to null, this avoids attempting
  // to release any incoming garbage value and ensures some sanity in the event
  // the the return code is ignored...  Nobody ignores return codes, right?
  if (is_out_parameter(sem_type) && !is_in_parameter(sem_type)) {
    cg_lua_var_decl(body, sem_type, name);
  }
}

// Walk all the params of a stored proc, if any of them require initialization code
// in the body, emit that here.
static void cg_lua_params_init(ast_node *ast, charbuf *body) {
  Contract(is_ast_params(ast));

  while (ast) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    cg_lua_param_init(param, body);

    ast = ast->right;
  }
}

// Emit the return code variables for the procedure
// if the procedure uses throw then it needs the saved RC as well so we can re-throw it
static void cg_lua_emit_rc_vars(charbuf *output) {
  bprintf(output, "  local _rc_ = CQL_OK\n");
}

// For LUA the contract rules are simple:
// * in not null args need a contract
// * out args are not really args, they are return values so nothing to check
// * inout args are in as an arg and out as as a return value, so check if not null
//
static void cg_lua_emit_contracts(ast_node *ast, charbuf *b) {
  Contract(is_ast_params(ast));
  Contract(b);

  bool_t did_emit_contract = false;

  int32_t position = 1;
  for (ast_node *params = ast; params; params = params->right, position++) {
    Contract(is_ast_params(params));
    EXTRACT_NOTNULL(param, params->left);
    EXTRACT_NOTNULL(param_detail, param->right);
    EXTRACT_ANY_NOTNULL(name_ast, param_detail->left);
    EXTRACT_STRING(name, name_ast);
    sem_t sem_type = name_ast->sem->sem_type;

    if (is_out_parameter(sem_type) && !is_in_parameter(sem_type)) {
      // in LUA these are return value nothing to check
      continue;
    }

    bool_t notnull = is_not_nullable(sem_type);

    if (notnull) {
      bprintf(b, "  cql_contract_argument_notnull(%s, %d)\n", name, position);
      did_emit_contract = true;
    }
  }

  if (did_emit_contract) {
    bprintf(b, "\n");
  }
}

// emit the fetch results function defintion (not the body) into the indicated buffer
static void cg_lua_emit_fetch_results_prototype(
  bool_t dml_proc,
  ast_node *params,
  CSTR proc_name,
  CSTR result_set_name,
  charbuf *decl)
{
  CG_CHARBUF_OPEN_SYM(fetch_results_sym, proc_name, "_fetch_results");
  CHARBUF_OPEN(args);
  CHARBUF_OPEN(returns);

  // optional db reference
  if (dml_proc) {
    bprintf(&args, "_db_");
  }

  // args to forward
  if (params) {
    cg_lua_params(params, &args, &returns);
  }

  bprintf(decl, "function %s(%s)\n", fetch_results_sym.ptr, args.ptr);

  CHARBUF_CLOSE(returns);
  CHARBUF_CLOSE(args);
  CHARBUF_CLOSE(fetch_results_sym);
}

// The prototype for the given procedure goes into the given buffer.  This
// is a naked prototype, so additional arguments could be added -- it will be
// missing the trailing ")" and it will not have EXPORT or anything like that
// on it.
static void cg_lua_emit_proc_prototype(ast_node *ast, charbuf *proc_decl) {
  Contract(is_ast_create_proc_stmt(ast) || is_ast_declare_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  CSTR name = NULL;

  // in lua declare generates nothing so it's always this case
  Invariant(is_ast_create_proc_stmt(ast));

  if (is_ast_create_proc_stmt(ast)) {
    EXTRACT_STRING(n, ast->left);
    name = n;
  }
  /* snipping in case we need something in the future for the declare case
  else {
    // LUA has no exterrn prototype form and we're not doing headers so nothing to do
    EXTRACT_NOTNULL(proc_name_type, ast->left);
    EXTRACT_STRING(n, proc_name_type->left);
    name = n;
  } */

  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  // if you're doing out_union then the row fetcher is all there is
  CSTR suffix = out_union_proc ? "_fetch_results" : "";

  CG_CHARBUF_OPEN_SYM(proc_name_base, name);
  CG_CHARBUF_OPEN_SYM(proc_sym, name, suffix);
  CHARBUF_OPEN(args);
  CHARBUF_OPEN(returns);

  // CREATE PROC [name] ( [params] )
  if (params) {
    cg_lua_params(params, &args, &returns);
  }

  if (dml_proc) {
    bprintf(proc_decl, "%s(_db_", proc_sym.ptr);
    if (args.used > 1) {
      bprintf(proc_decl, ", ");
      bprintf(proc_decl, "%s", args.ptr);
    }
  }
  else {
    bprintf(proc_decl, "%s(%s", proc_sym.ptr, args.ptr);
  }

  CHARBUF_CLOSE(returns);
  CHARBUF_CLOSE(args);
  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(proc_name_base);
}

// Emitting a stored proc is mostly setup.  We have a bunch of housekeeping to do:
//  * create new scratch buffers for the body and the locals and the cleanup section
//  * save the current output globals
//  * set the globals to point to those buffers
//  * save the old scratch masks and create new ones
//  * emit the prototype of the LUA function for this proc
//  * recursively spit out the statements
//  * when this is all done assemble the pieces into the original output streams
//  * procedures that use SQL will get a hidden _db_ argument
//  * procedures that return a result set will get a statement result value
//    * and the additional procedures for creating the result set and accessing it are emitted
//  * cursor OUT forms get rows output or a single row output
static void cg_lua_create_proc_stmt(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  bool_t private_proc = misc_attrs && exists_attribute_str(misc_attrs, "private");
  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);
  bool_t calls_out_union = has_out_union_call(ast);
  proc_cte_index = 0;
  lua_cur_bound_statement = 0;

  // sets base_fragment_name as well for the current fragment
  uint32_t frag_type = find_fragment_attr_type(misc_attrs, &base_fragment_name);

  // these have already been ruled out
  Invariant(frag_type != FRAG_TYPE_SHARED);
  Invariant(frag_type != FRAG_TYPE_BASE);
  Invariant(frag_type != FRAG_TYPE_EXTENSION);

  CHARBUF_OPEN(proc_fwd_ref);
  CHARBUF_OPEN(proc_contracts);
  CHARBUF_OPEN(proc_body);
  CHARBUF_OPEN(proc_locals);
  CHARBUF_OPEN(proc_cleanup);

  bool_t saved_lua_error_target_used = lua_error_target_used;
  lua_error_target_used = false;
  lua_return_used = false;

  int32_t saved_lua_rcthrown_index = lua_rcthrown_index;
  lua_rcthrown_index = 0;

  bool_t saved_lua_rcthrown_used = lua_rcthrown_used;
  lua_rcthrown_used = 0;

  bool_t saved_temp_emitted = lua_temp_statement_emitted;
  bool_t saved_lua_seed_declared = lua_seed_declared;
  charbuf *saved_main = cg_main_output;
  charbuf *saved_decls = cg_declarations_output;
  charbuf *saved_scratch = cg_scratch_vars_output;
  charbuf *saved_cleanup = cg_cleanup_output;
  charbuf *saved_fwd_ref = cg_fwd_ref_output;
  cg_lua_scratch_masks *saved_masks = cg_lua_current_masks;

  Invariant(!use_encode);
  Invariant(!encode_context_column);
  Invariant(!encode_columns);
  encode_columns = symtab_new();
  Invariant(lua_named_temporaries == NULL);
  lua_named_temporaries = symtab_new();

  cg_lua_scratch_masks masks;
  cg_lua_current_masks = &masks;
  cg_lua_zero_masks(cg_lua_current_masks);
  lua_temp_statement_emitted = false;
  lua_in_proc = true;
  current_proc = ast;
  lua_seed_declared = false;

  init_encode_info(misc_attrs, &use_encode, &encode_context_column, encode_columns);

  bprintf(cg_declarations_output, "\n");

  // if you're doing out_union then the row fetcher is all there is
  CSTR suffix = out_union_proc ? "_fetch_results" : "";

  CG_CHARBUF_OPEN_SYM(proc_name_base, name);
  CG_CHARBUF_OPEN_SYM(proc_sym, name, suffix);

  CHARBUF_OPEN(proc_decl);
  cg_lua_emit_proc_prototype(ast, &proc_decl);

  // CREATE PROC [name] ( [params] )
  if (params) {
    cg_lua_params_init(params, &proc_locals);
    if (!private_proc) {
      cg_lua_emit_contracts(params, &proc_contracts);
    }
  }

  cg_fwd_ref_output = &proc_fwd_ref;
  cg_main_output = &proc_body;
  cg_declarations_output = &proc_locals;
  cg_scratch_vars_output = &proc_locals;
  cg_cleanup_output = &proc_cleanup;

  // BEGIN [stmt_list] END
  cg_lua_stmt_list(stmt_list);

  cg_fwd_ref_output = saved_fwd_ref;
  cg_main_output = saved_main;
  cg_declarations_output = saved_decls;
  cg_scratch_vars_output = saved_scratch;
  cg_cleanup_output = saved_cleanup;
  cg_lua_current_masks = saved_masks;
  lua_temp_statement_emitted = saved_temp_emitted;
  lua_seed_declared = saved_lua_seed_declared;

  bprintf(cg_declarations_output, proc_fwd_ref.ptr);
  bprintf(cg_declarations_output, "function %s)\n", proc_decl.ptr);
  bprintf(cg_declarations_output, proc_contracts.ptr);

  if (dml_proc) {
    cg_lua_emit_rc_vars(cg_declarations_output);
    if (result_set_proc) {
      bprintf(cg_declarations_output, "  local _result_stmt = nil\n");
    }
  }

  if (out_stmt_proc) {
    bprintf(cg_declarations_output, "  local _result_ = nil\n");
  }

  if (calls_out_union) {
    bprintf(cg_declarations_output, "  local _result_set_ = {}\n");
  }
  else if (out_union_proc) {
    bprintf(cg_declarations_output, "  local _rows_ = {}\n");
  }

  bindent(cg_declarations_output, &proc_locals, 2);
  if (proc_locals.used > 1) {
    bprintf(cg_declarations_output, "\n");
  }

  bprintf(cg_declarations_output, "%s", proc_body.ptr);

  if (dml_proc) {
    bprintf(cg_declarations_output, "  _rc_ = CQL_OK\n");
  }

  if (lua_error_target_used || lua_return_used) {
    bprintf(cg_declarations_output, "\n::%s::", lua_error_target);
  }

  bprintf(cg_declarations_output, "\n");

  if (proc_cleanup.used > 1) {
    bprintf(cg_declarations_output, proc_cleanup.ptr);
  }

  if (result_set_proc) {
    // Because of control flow it's possible that we never actually ran a select statement
    // even if there were no errors.  Or maybe we caught the error.  In any case if we
    // are not producing an error then we have to produce an empty result set to go with it.
    bprintf(cg_declarations_output, "  if _rc_ == CQL_OK and _result_stmt == nil then _rc_, _result_stmt = cql_no_rows_stmt(_db_) end\n");
  }

  CHARBUF_OPEN(returns);

  if (dml_proc) {
    bprintf(&returns, "_rc_");
  }

  if (result_set_proc) {
    bprintf(&returns, ", _result_stmt");
  }

  if (out_stmt_proc) {
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "_result_");
  }

  if (calls_out_union) {
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "_result_set_");
  }
  else if (out_union_proc) {
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "_rows_");
  }

  ast_node *item = params;
  while (item) {
    EXTRACT_ANY_NOTNULL(param, item->left);
    if (is_out_parameter(param->sem->sem_type)) {
      if (returns.used > 1)  {
        bprintf(&returns, ", ");
      }
      bprintf(&returns, "%s", param->sem->name);
    }
    item = item->right;
  }

  if (returns.used > 1)  {
    bprintf(cg_declarations_output, "  return %s\n", returns.ptr);
  }

  bprintf(cg_declarations_output, "end\n");

  CHARBUF_CLOSE(returns);
  CHARBUF_CLOSE(proc_decl);
  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(proc_name_base);
  CHARBUF_CLOSE(proc_cleanup);
  CHARBUF_CLOSE(proc_locals);
  CHARBUF_CLOSE(proc_body);
  CHARBUF_CLOSE(proc_contracts);
  CHARBUF_CLOSE(proc_fwd_ref);

  if (out_stmt_proc || out_union_proc || result_set_proc) {
    cg_lua_proc_result_set(ast);
  }

  lua_in_proc = false;
  use_encode = false;
  current_proc = NULL;
  base_fragment_name = NULL;

  symtab_delete(encode_columns);
  symtab_delete(lua_named_temporaries);
  encode_context_column = NULL;
  encode_columns = NULL;
  lua_named_temporaries = NULL;
  lua_error_target_used = saved_lua_error_target_used;
  lua_rcthrown_index = saved_lua_rcthrown_index;
  lua_rcthrown_used = saved_lua_rcthrown_used;
  Invariant(!strcmp(lua_error_target, CQL_CLEANUP_DEFAULT_LABEL));
  Invariant(!strcmp(lua_rcthrown_current, CQL_LUA_RCTHROWN_DEFAULT));
}

static void cg_lua_declare_simple_var(sem_t sem_type, CSTR name) {
  cg_lua_var_decl(cg_declarations_output, sem_type, name);
}

// Emit a bunch of variable declarations for normal variables.
// cg_lua_var_decl does exactly this job for us.  Add any global variables to
// the header file output.
static void cg_lua_declare_vars_type(ast_node *declare_vars_type) {
  Contract(is_ast_declare_vars_type(declare_vars_type));
  EXTRACT_NOTNULL(name_list, declare_vars_type->left);

  // DECLARE [name_list] [data_type]

  for (ast_node *ast = name_list; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(name_ast, ast->left);
    EXTRACT_STRING(name, name_ast);

    cg_lua_declare_simple_var(name_ast->sem->sem_type, name);
  }
}

// This is a callback method handed to the gen_ method that creates SQL for us
// it will call us every time it finds a variable that needs to be bound.  That
// variable is replaced by ? in the SQL output.  We end up with a list of variables
// to bind on a silver platter (but in reverse order).
static bool_t cg_lua_capture_variables(ast_node *ast, void *context, charbuf *buffer) {
  // all variables have a name
  Contract(ast->sem->name);

  // If the current context is inline function expansion then arg variables
  // are emitted as is -- we rewrite these so that they come from an inline table
  // e.g.
  //   'select x + y'
  // becomes
  //   '(select x + y from (select arg1 x, arg2 y))'
  //
  // as a result x, y are not bound variables
  if (lua_in_inline_function_fragment) {
    return false;
  }

  lua_cur_variable_count++;

  symtab_entry *entry = symtab_find(proc_arg_aliases, ast->sem->name);
  if (entry) {
    // this variable has been rewritten to a new name, use the alias
    ast = entry->val;
  }

  list_item **head = (list_item**)context;
  add_item_to_list(head, ast);

  bprintf(buffer, "?");
  return true;
}

// This is a callback method handed to the gen_ method that creates SQL for us
// it will call us every time it finds a cte table that needs to be generated.
// If this is one of the tables that is supposed to be an "argument" then
// we will remove the stub definition of the CTE.  References to this name
// will be changed to required table in another callback
static bool_t cg_lua_suppress_cte(ast_node *ast, void *context, charbuf *buffer) {
  Contract(is_ast_cte_table(ast));
  EXTRACT(cte_decl, ast->left);
  EXTRACT_STRING(name, cte_decl->left);

  // if we have an alias we suppress the name
  symtab_entry *entry = symtab_find(proc_cte_aliases, name);
  return !!entry;
}

// This a callback method handed to the gen_ method that creates SQL for us
// it will call us every time it finds a table reference that needs to be generated.
// If this is one of the tables that is supposed to be an "argument" then
// we will emit the desired value instead of the stub name.   Note that
// this is always the name of a CTE and CTE of the old name was suppressed
// using the callback above cg_lua_suppress_cte
static bool_t cg_lua_table_rename(ast_node *ast, void *context, charbuf *buffer) {
  // this is a simple table factor, so an actual name...
  EXTRACT_STRING(name, ast);
  bool_t handled = false;

  // if we have an alias we suppress the name
  symtab_entry *entry = symtab_find(proc_cte_aliases, name);
  if (entry) {
    EXTRACT(cte_binding, entry->val);
    EXTRACT_STRING(actual, cte_binding->left);
    bprintf(buffer, "%s", actual);
    handled = true;
  }

  return handled;
}

// This helper method fetches a single column from a select statement.  The result
// is to be stored in the local variable "var"
static void cg_lua_get_column(sem_t sem_type, CSTR cursor, int32_t index, CSTR var, charbuf *output) {
  if (core_type_of(sem_type) == SEM_TYPE_BOOL) {
    bprintf(output, "  %s = cql_to_bool(cql_get_value(%s, %d))\n", var, cursor, index);
  } else {
    bprintf(output, "  %s = cql_get_value(%s, %d)\n", var, cursor, index);
  }
}

// Emit a declaration for the temporary statement _temp_stmt_ if we haven't
// already done so.  Also emit the cleanup once.
static void lua_ensure_temp_statement() {
  if (!lua_temp_statement_emitted) {
    bprintf(cg_declarations_output, "local _temp_stmt = nil\n");
    bprintf(cg_cleanup_output, "  cql_finalize_stmt(_temp_stmt)\n");
    bprintf(cg_cleanup_output, "  _temp_stmt = nil\n");
    lua_temp_statement_emitted = true;
  }
}

// This tells us how many fragments we emitted using some size math
static int32_t cg_lua_fragment_count() {
  return (int32_t)(lua_shared_fragment_strings.used / sizeof(CSTR));
}

// when we complete a chunk of fragment text we have to emit the predicates
// for the variables that were in that chunk.  We do this in the same
// context as the conditional for that string.
static void cg_lua_flush_variable_predicates() {
  if (!lua_has_conditional_fragments) {
    return;
  }

  while (lua_prev_variable_count < lua_cur_variable_count) {
    if (lua_cur_fragment_predicate == 0 || lua_cur_fragment_predicate + 1 == lua_max_fragment_predicate) {
      bprintf(cg_main_output, "_vpreds_%d[%d] = true -- pred %d known to be true\n",
      lua_cur_bound_statement,
      lua_prev_variable_count++,
      lua_cur_fragment_predicate);
    }
    else {
      // If we're back in previous context we can always just use the predicate value
      // for that context which was set in an earlier block.
      // TODO: I think we can prove that it's always true in the code block we are in
      // so this could be = 1 and hence is the same as the above.
      bprintf(cg_main_output, "_vpreds_%d[%d] = _preds_%d[%d]\n",
        lua_cur_bound_statement,
        lua_prev_variable_count++,
        lua_cur_bound_statement,
        lua_cur_fragment_predicate);
    }
  }
}

// If we have set up the predicate for this chunk of text we can just use it
// we see that by looking at how many predicates we set up and if we
// are past that point. If we need a predicate for the current line
// we use the predicate value for the "current" predicate scope,
// which nests.  Whatever the current predicate is we use that
// and make an entry in the array.  So that way there is always
// one computed predicate for each chunk of text we plan to emit.
static void cg_lua_fragment_copy_pred() {
  if (!lua_has_conditional_fragments) {
    return;
  }

  int32_t count = cg_lua_fragment_count();
  if (count + 1 == lua_max_fragment_predicate) {
    return;
  }

  if (lua_cur_fragment_predicate == 0) {
    bprintf(cg_main_output, "_preds_%d[%d] = true\n",
      lua_cur_bound_statement,
      lua_max_fragment_predicate++);
  }
  else {
    // TODO: I think we can prove that it's always true in the code block we are in
    // so this could be = true and hence is the same as the above.
    bprintf(cg_main_output, "_preds_%d[%d] = _preds_%d[%d]\n",
      lua_cur_bound_statement,
      lua_max_fragment_predicate++,
      lua_cur_bound_statement,
      lua_cur_fragment_predicate);
  }

  cg_lua_flush_variable_predicates();
}

// First we make sure we have a predicate row and then we emit the line
// assuming there is anything to emit...
static void cg_lua_emit_one_frag(charbuf *buffer) {
  // TODO: can we make this an invariant?
  if (buffer->used > 1) {
    cg_lua_fragment_copy_pred();
    CSTR str = Strdup(buffer->ptr);
    bytebuf_append_var(&lua_shared_fragment_strings, str);
    bclear(buffer);
  }
}

// Emit a fragment from a statement, note that this can nest
static void cg_lua_fragment_stmt(ast_node *stmt, charbuf *buffer) {
  gen_one_stmt(stmt);
  cg_lua_emit_one_frag(buffer);
  cg_lua_flush_variable_predicates();
}

// a new block in a conditional, this is the "it's true" case for it
// assign it a number and move on.  Note the code is always inside of
// if (the_expression_was_true) {...}
static void cg_lua_fragment_setpred() {
  lua_cur_fragment_predicate = lua_max_fragment_predicate;
  if (lua_has_conditional_fragments) {
    bprintf(cg_main_output, "_preds_%d[%d] = true\n",
      lua_cur_bound_statement,
      lua_max_fragment_predicate++);
  }
}

// Emit the if condition for the conditional fragment and then generate the
// predicate setting as well as the SQL for that part of the fragment.
static void cg_lua_fragment_cond_action(ast_node *ast, charbuf *buffer) {
  Contract(is_ast_cond_action(ast));
  EXTRACT_NOTNULL(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  // [expr ast->left] THEN stmt_list

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  bprintf(cg_main_output, "if %s then\n", expr_value.ptr);

  CG_LUA_POP_EVAL(expr);

  int32_t cur_fragment_predicate_saved = lua_cur_fragment_predicate;

  CG_PUSH_MAIN_INDENT(ifbody, 2);
  cg_lua_fragment_setpred();

  // and we emit the next statement string fragment
  cg_lua_fragment_stmt(stmt_list->left, buffer);

  lua_cur_fragment_predicate = cur_fragment_predicate_saved;

  CG_POP_MAIN_INDENT(ifbody);
}

// Here we're just walking the elseif list, as with normal codegen when we get
// to the end we deal with the elsenode.  We can't do the else node in the caller
// because we need to emit it inside the deepest matching parens.  So we just
// push the elsenode down the recursion until its needed.
static void cg_lua_fragment_elseif_list(ast_node *ast, ast_node *elsenode, charbuf *buffer) {
  if (ast) {
    Contract(is_ast_elseif(ast));
    EXTRACT(cond_action, ast->left);

    // ELSE IF [cond_action]
    bprintf(cg_main_output, "else\n");
      CG_PUSH_MAIN_INDENT(else, 2);
      cg_lua_fragment_cond_action(cond_action, buffer);
      cg_lua_fragment_elseif_list(ast->right, elsenode, buffer);
      CG_POP_MAIN_INDENT(else);
    bprintf(cg_main_output, "end\n");
  }
  else if (elsenode) {
    Contract(is_ast_else(elsenode));
    // ELSE [stmt_list]
    EXTRACT(stmt_list, elsenode->left);

    bprintf(cg_main_output, "else\n");
      CG_PUSH_MAIN_INDENT(else, 2);

      int32_t cur_fragment_predicate_saved = lua_cur_fragment_predicate;
      cg_lua_fragment_setpred();

      // this is the next string fragment
      cg_lua_fragment_stmt(stmt_list->left, buffer);

      lua_cur_fragment_predicate = cur_fragment_predicate_saved;
      CG_POP_MAIN_INDENT(else);
    bprintf(cg_main_output, "end\n");
  }
}

// This handles the expression fragment case, this is rewritten so that
// the arguments of the expression fragment become columns of one row of table
// e.g.
// @attribute(cql:shared_fragment)
// create proc ex_frag(x integer)
// begin
//    select x + 2 * x as result;
// end
//
// this becomes
//
// "SELECT x + 2 * x from (select ? as x)"
//
// The expression fragment is not allowed to have its own from clause which means
// we can use the from clause for our own purposes (local binding).  The is very
// helpful if the fragment happens often or if the argument would otherwise have
// to be evaluated many times.  But it comes at the cost of a one-row query.
static bool_t cg_lua_inline_func(ast_node *call_ast, void *context, charbuf *buffer) {
  Contract(is_ast_call(call_ast));
  EXTRACT_STRING(proc_name, call_ast->left);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // flush what we have so far
  cg_lua_emit_one_frag(buffer);

  ast_node *ast = find_proc(proc_name);

  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  bool_t saved_in_inline_function_fragment = lua_in_inline_function_fragment;
  symtab *saved_proc_arg_aliases = proc_arg_aliases;
  symtab *saved_proc_cte_aliases = proc_cte_aliases;
  lua_in_inline_function_fragment = true;

  proc_arg_aliases = NULL;
  proc_cte_aliases = NULL;

  bprintf(buffer, "(");

  // Emit a fragment from a statement, note that this can nest
  cg_lua_fragment_stmt(stmt, buffer);

  proc_arg_aliases = saved_proc_arg_aliases;
  proc_cte_aliases = saved_proc_cte_aliases;
  lua_in_inline_function_fragment = saved_in_inline_function_fragment;

  if (params) {
    // If there are any args we create a nested select expression
    // to bind them to the variable names.  Note that this means
    // args are evaluated once which could be important if there
    // are SQL functions with side-effects being used (highly rare)
    // or expensive functions.
    bprintf(buffer, " FROM (SELECT ");

    while (params) {
      Invariant(is_ast_params(params));
      Invariant(arg_list); // expressions match the args

      EXTRACT_NOTNULL(param, params->left);
      EXTRACT_ANY_NOTNULL(expr, arg_list->left);

      EXTRACT_NOTNULL(param_detail, param->right);
      EXTRACT_ANY_NOTNULL(param_name_ast, param_detail->left)
      EXTRACT_STRING(param_name, param_name_ast);

      gen_root_expr(expr);
      bprintf(buffer, " %s", param_name);
      if (params->right) {
        bprintf(buffer, ", ");
      }

      // guaranteed to stay in lock step
      params = params->right;
      arg_list = arg_list->right;
    }
    bprintf(buffer, ")");
  }

  bprintf(buffer, ")");
  cg_lua_emit_one_frag(buffer);

  return true;
}

// Here we've found a call expression where a CTE should be so like
// with
//  X(*) as (call foo(1,2,3))
// select * from X;
//
// or
//
// with
//  X(*) as (call foo(1,2,3) USING foo as source1, bar = source2)
// select * from X;
//
// What we're going to do is replace the call with the body of the procedure that is being called.
// We have to do a few things to make this work:
//  * the args to the procedure have to be evaluated and put into locals
//  * any use of those arguments has to be redirected to said locals (so rename the locals)
//  * naturally any of those arguments can't be database things (wrong context) so we can evaluate them
//    all in advance
//  * if the call has the "USING" form then we have to alias all instances of the mentioned
//    tables in the target procedure to be the values that were provided
//  * any such args/aliases have been pre-validated during semantic analysis
//  * code gen is designed to keep as many string literals identical as possible so that they can be folded
static bool_t cg_lua_call_in_cte(ast_node *cte_body, void *context, charbuf *buffer) {
  EXTRACT_NOTNULL(call_stmt, cte_body->left);
  EXTRACT(cte_binding_list, cte_body->right);

  EXTRACT_STRING(name, call_stmt->left);
  EXTRACT_ANY(expr_list, call_stmt->right);

  ast_node *ast = find_proc(name);

  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);

  bool_t saved_in_inline_function_fragment = lua_in_inline_function_fragment;
  symtab *saved_proc_arg_aliases = proc_arg_aliases;
  symtab *saved_proc_cte_aliases = proc_cte_aliases;
  lua_in_inline_function_fragment = false;

  symtab *new_arg_aliases = symtab_new();
  proc_cte_aliases = symtab_new();

  while (cte_binding_list) {
    EXTRACT_NOTNULL(cte_binding, cte_binding_list->left);
    EXTRACT_STRING(formal, cte_binding->right);
    EXTRACT_STRING(actual, cte_binding->left);

    // The "actual" might itself be an alias from the outer scope
    // be sure to push that down if that's the case.  One level
    // is always enough because each level does its own push if
    // needed.

    bool_t handled = false;

    if (saved_proc_cte_aliases) {
      symtab_entry *entry = symtab_find(saved_proc_cte_aliases, actual);
      if (entry) {
        symtab_add(proc_cte_aliases, formal, entry->val);
        handled = true;
      }
    }

    if (!handled) {
      // normal case, the first time a name is aliased
      symtab_add(proc_cte_aliases, formal, cte_binding);
    }

    cte_binding_list = cte_binding_list->right;
  }

  if (params) {
    // move to the next index if we need to alias anything
    proc_cte_index++;
  }

  while (params) {
    Invariant(is_ast_params(params));
    Invariant(expr_list); // expressions match the args

    EXTRACT_NOTNULL(param, params->left);
    EXTRACT_ANY_NOTNULL(expr, expr_list->left);

    EXTRACT_NOTNULL(param_detail, param->right);
    EXTRACT_ANY_NOTNULL(param_name_ast, param_detail->left)
    EXTRACT_STRING(param_name, param_name_ast);

    sem_t sem_type_var = param_name_ast->sem->sem_type;

    CSTR alias_name = dup_printf("_p%d_%s_", proc_cte_index, param_name);

    AST_REWRITE_INFO_SET(param->lineno, param->filename);

    ast_node *alias  = new_ast_str(alias_name);
    symtab_add(new_arg_aliases, param_name, alias);
    alias->sem = new_sem(sem_type_var);
    alias->sem->name = alias_name;
    alias->sem->kind = param_name_ast->sem->kind;

    AST_REWRITE_INFO_RESET();

    // emit the declaration
    cg_lua_var_decl(cg_declarations_output, sem_type_var, alias_name);

    sem_t sem_type_expr = expr->sem->sem_type;

    // evaluate the expression and assign
    // note that any arg aliases here are in the context of the caller not the callee
    // we're setting up the aliases for the callee right now and they aren't ready yet even
    // but that's ok because the expressions are in the context of the caller.

    // todo: if the evaluation has a nested select statement then we will have to re-enter
    // all of this.  We can either ban that (which isn't insane really) or else we can
    // save the codegen state like callbacks and such so that it can re-enter.  That's
    // the desired path.

    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
    cg_lua_store(cg_main_output, alias_name, sem_type_var, sem_type_expr, expr_value.ptr);
    CG_LUA_POP_EVAL(expr);

    // guaranteed to stay in lock step
    params = params->right;
    expr_list = expr_list->right;
  }

  // exactly one statment
  Invariant(!stmt_list->right);

  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  // now replace the aliases for just this one bit
  proc_arg_aliases = new_arg_aliases;

  cg_lua_emit_one_frag(buffer);

  // we need the column names for our select
  // we'll accomplish this by generating a CTE wrapper
  // the column names are were already in the original text but
  // we want to minify those out, we could turn off alias minification here
  // but if we did that then we couldn't share the text of the fragment
  // so instead we make a wrapper that has exatly the column names we need

  bool_t is_nested_select = is_ast_table_or_subquery(cte_body->parent);

  CHARBUF_OPEN(wrapper);
  if (is_nested_select) {
    // this ensures that all the columns of the select are correctly named
    bprintf(&wrapper, "WITH _ns_(");

    sem_struct *sptr = cte_body->sem->sptr;

    for (int32_t i = 0; i < sptr->count; i++) {
      bprintf(&wrapper, "%s%s", i == 0 ? "": ", ", sptr->names[i]);
    }

    bprintf(&wrapper, ") AS (");
    cg_lua_emit_one_frag(&wrapper);
  }

  if (is_ast_if_stmt(stmt)) {
    EXTRACT_NOTNULL(cond_action, stmt->left);
    EXTRACT_NOTNULL(if_alt, stmt->right);
    EXTRACT(elseif, if_alt->left);
    EXTRACT_NAMED_NOTNULL(elsenode, else, if_alt->right);

    cg_lua_fragment_cond_action(cond_action, buffer);
    cg_lua_fragment_elseif_list(elseif, elsenode, buffer);
  }
  else {
    cg_lua_fragment_stmt(stmt, buffer);
  }

  if (is_nested_select) {
    bprintf(&wrapper, ") SELECT * FROM _ns_");
    cg_lua_emit_one_frag(&wrapper);
  }

  CHARBUF_CLOSE(wrapper);

  symtab_delete(proc_arg_aliases);
  symtab_delete(proc_cte_aliases);
  proc_arg_aliases = saved_proc_arg_aliases;
  proc_cte_aliases = saved_proc_cte_aliases;
  lua_in_inline_function_fragment = saved_in_inline_function_fragment;

  return true;
}

// We're looking for the presence of any shared fragments and in particular
// the presence of conditionals within them.  We don't have to do much for
// this check but we do have to recurse the search as the normal walk doesn't
// go into the body of shared fragments and the conditionals might be deeper
// in the tree.
static bool_t cg_lua_search_conditionals_call_in_cte(ast_node *cte_body, void *context, charbuf *buffer) {
  EXTRACT_NOTNULL(call_stmt, cte_body->left);
  EXTRACT_STRING(name, call_stmt->left);

  ast_node *ast = find_proc(name);

  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  lua_has_conditional_fragments |= is_ast_if_stmt(stmt);
  lua_has_shared_fragments = true;

  // recurse the fragment contents, we might find more stuff, like variables
  // and such deeper in the tree
  gen_one_stmt(stmt);

  return false;
}

// We simply record that we found some variables, any variables
static bool_t cg_lua_note_variable_exists(ast_node *cte_body, void *context, charbuf *buffer) {
  lua_has_variables = true;
  return false;
}

// The inline function counts as a shared fragment and we recurse to find any
// internal shared fragments or conditional fragments inside of the inline function.
// Note that even though it has no FROM clause the inline function could have
// a nested select inside of its select list and therefore all fragment types
// can appear inside of an inline function fragment.
static bool_t cg_lua_note_inline_func(ast_node *call_ast, void *context, charbuf *buffer) {
  Contract(is_ast_call(call_ast));
  EXTRACT_STRING(proc_name, call_ast->left);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);

  ast_node *ast = find_proc(proc_name);

  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);
  EXTRACT_ANY_NOTNULL(stmt, stmt_list->left);

  // recurse the fragment contents, we might find more stuff, like variables
  // and such deeper in the tree
  gen_one_stmt(stmt);

  lua_has_shared_fragments = true;
  return false;
}


// We set up a walk of the tree using the echo functions but
// we are going to note what kinds of things we spotted while doing
// the walk.  We need to know in advance what style of codegen we'll
// be doing.
static void cg_lua_classify_fragments(ast_node *stmt) {
  lua_has_shared_fragments = false;
  lua_has_conditional_fragments = false;
  lua_has_variables = false;

  CHARBUF_OPEN(sql);
  gen_set_output_buffer(&sql);
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.cte_proc_callback = cg_lua_search_conditionals_call_in_cte;
  callbacks.variables_callback = cg_lua_note_variable_exists;
  callbacks.inline_func_callback = cg_lua_note_inline_func;
  gen_statement_with_callbacks(stmt, &callbacks);
  CHARBUF_CLOSE(sql);
}

// This is the most important function for sqlite access;  it does the heavy
// lifting of generating the C code to prepare and bind a SQL statement.
// If cg_lua_exec is true (CG_EXEC) then the statement is executed immediately
// and finalized.  No results are expected.  To accomplish this we do the following:
//   * figure out the name of the statement, either it's given to us
//     or we're using the temp statement
//   * call get_statement_with_callback to get the text of the SQL from the AST
//     * the callback will give us all the variables to bind
//     * count the variables so we know what column numbers to use (the list is backwards!)
//   * if CG_EXEC and no variables we can use the simpler sqlite3_exec form
//   * bind any variables
//   * if there are variables CG_EXEC will step and finalize
static void cg_lua_bound_sql_statement(CSTR stmt_name, ast_node *stmt, int32_t cg_lua_flags) {
  list_item *vars = NULL;

  lua_cur_bound_statement++;
  lua_cur_fragment_predicate = 0;
  lua_max_fragment_predicate = 0;
  lua_prev_variable_count = 0;
  lua_cur_variable_count = 0;

  bytebuf_open(&lua_shared_fragment_strings);

  cg_lua_classify_fragments(stmt);

  if (lua_has_conditional_fragments) {
    bprintf(cg_main_output, "_preds_%d = {}\n", lua_cur_bound_statement);
    if (lua_has_variables) {
      bprintf(cg_main_output, "_vpreds_%d = {}\n", lua_cur_bound_statement);
    }
  }

  bool_t minify_aliases = !!(cg_lua_flags & CG_MINIFY_ALIASES);
  bool_t exec_only = !!(cg_lua_flags & CG_EXEC);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.variables_callback = cg_lua_capture_variables;
  callbacks.variables_context = &vars;
  callbacks.star_callback = cg_expand_star;
  callbacks.minify_casts = true;
  callbacks.minify_aliases = minify_aliases;
  callbacks.long_to_int_conv = true;
  callbacks.cte_proc_callback = cg_lua_call_in_cte;
  callbacks.cte_suppress_callback = cg_lua_suppress_cte;
  callbacks.table_rename_callback = cg_lua_table_rename;
  callbacks.inline_func_callback = cg_lua_inline_func;

  CHARBUF_OPEN(temp);
  gen_set_output_buffer(&temp);
  gen_statement_with_callbacks(stmt, &callbacks);

  // whether or not there is a prepare statement
  bool_t has_prepare_stmt = !exec_only || vars;

  uint32_t count = 0;
  for (list_item *item = vars; item; item = item->next, count++) ;

  if (stmt_name == NULL && has_prepare_stmt) {
    lua_ensure_temp_statement();
    stmt_name = "_temp";
  }

  // take care of what's left in the buffer after the other fragments have been emitted
  if (lua_has_shared_fragments) {
    cg_lua_emit_one_frag(&temp);
  }

  {
    CSTR suffix = lua_has_shared_fragments ? "_var" : "";

    if (!has_prepare_stmt) {
      bprintf(cg_main_output, "_rc_ = cql_exec%s(_db_,\n  ", suffix);
    }
    else {
      bprintf(cg_main_output, "_rc_, %s_stmt = cql_prepare%s(_db_, \n  ", stmt_name, suffix);
    }

    if (!lua_has_shared_fragments) {
      cg_pretty_quote_plaintext(temp.ptr, cg_main_output, PRETTY_QUOTE_C);
    }
    else {
      int32_t scount = cg_lua_fragment_count();

      // declare the predicate variables if needed
      if (lua_has_conditional_fragments) {
        bprintf(cg_main_output, "%d, _preds_%d,\n", scount, lua_cur_bound_statement);
        bprintf(cg_declarations_output, "local _preds_%d  -- %d possible fragments\n", lua_cur_bound_statement, scount);
        if (lua_has_variables) {
          bprintf(cg_declarations_output, "local _vpreds_%d -- %d possible bindings\n", lua_cur_bound_statement, lua_cur_variable_count);
        }
      }
      else {
        bprintf(cg_main_output, "%d, nil,\n", scount);
      }

      bprintf(cg_main_output, "  {\n");
      CSTR *strs = (CSTR *)(lua_shared_fragment_strings.ptr);
      for (size_t i = 0; i < scount; i++) {
        bprintf(cg_main_output, "  ");
        cg_pretty_quote_plaintext(strs[i], cg_main_output, PRETTY_QUOTE_C);
        if (i + 1 < scount) {
          bprintf(cg_main_output, ",\n");
        }
        else {
          bprintf(cg_main_output, "\n");
        }
      }
      bprintf(cg_main_output, "  }\n");
    }
    bprintf(cg_main_output, ")\n");
  }
  cg_lua_error_on_not_sqlite_ok();

  CHARBUF_CLOSE(temp);

  reverse_list(&vars);

  if (count) {
    CHARBUF_OPEN(typestring);
    bputc(&typestring, '"');

    // Now emit the binding args for each variable
    for (list_item *item = vars; item; item = item->next)  {
      sem_t sem_type = item->ast->sem->sem_type;
      cg_lua_put_typecode(&typestring, sem_type);
    }

    bputc(&typestring, '"');

    if (lua_has_conditional_fragments) {
      bprintf(cg_main_output, "_rc_ = cql_multibind_var(_db_, %s_stmt, %d, _vpreds_%d, %s, {",
        stmt_name, count, lua_cur_bound_statement, typestring.ptr);
    }
    else {
      bprintf(cg_main_output, "_rc_ = cql_multibind(_db_, %s_stmt, %s, {",
        stmt_name, typestring.ptr);
    }

    CHARBUF_CLOSE(typestring);


    // Now emit the binding args for each variable
    for (list_item *item = vars; item; item = item->next)  {
      Contract(item->ast->sem->name);
      if (item != vars) {
        bprintf(cg_main_output, ", ");
      }
      bprintf(cg_main_output, "%s", item->ast->sem->name);
    }

    bprintf(cg_main_output, "})\n");
    cg_lua_error_on_not_sqlite_ok();
  }

  if (exec_only && vars) {
    bprintf(cg_main_output, "_rc_ = cql_step(%s_stmt)\n", stmt_name);
    cg_lua_error_on_rc_notequal("CQL_DONE");
    bprintf(cg_main_output, "cql_finalize_stmt(%s_stmt)\n", stmt_name);
    bprintf(cg_main_output, "%s_stmt = nil\n", stmt_name);
  }

  // vars is pool allocated, so we don't need to free it
  bytebuf_close(&lua_shared_fragment_strings);
}

static void cg_lua_emit_field_names(charbuf *output, sem_struct *sptr) {
  Contract(sptr);

  bprintf(output, "{ ");
    for (int32_t i = 0; i < sptr->count; i++) {
    if (i > 0) {
      bprintf(output, ", ");
    }
    if (strcmp(sptr->names[i], "_anon")) {
      bprintf(output, "\"%s\"", sptr->names[i]);
    }
    else {
      bprintf(output, "\"_anon%d\"", i);
    }
  }
  bprintf(output, " }");
}

// copied here for easy reference
//
// #define SEM_TYPE_BOOL 1         // the subtree is a bool
// #define SEM_TYPE_INTEGER 2      // the subtree is an integer
// #define SEM_TYPE_LONG_INTEGER 3 // the subtree is a long_integer
// #define SEM_TYPE_REAL 4         // the subtree is a real
// #define SEM_TYPE_TEXT 5         // the subtree is a text type
// #define SEM_TYPE_BLOB 6         // the subtree is a blob type
// #define SEM_TYPE_OBJECT 7       // the subtree is any object type
//
// code meanings
//
// f = flag = bool  (b is for blob)
// i = integer
// l = long_int
// d = double (the real type)
// s = string (the text type)
// b = blob
// o = object
//
// these are the same codes used by the blob encoder
//
// Note if the sem type codes were ever re-ordered a zillion tests
// would break until these lines were fixed so there isn't really
// a maintenance issue here.  There are actually more subtle
// order dependencies for range checks so this doesn't really
// add anything new.
//
static char code_nullable[] = "@fildsbo";
static char code_not_nullable[] = "@FILDSBO";

static void cg_lua_put_typecode(charbuf *output, sem_t sem_type) {
  sem_t core_type = core_type_of(sem_type);
  bool_t nullable = is_nullable(sem_type);
  Invariant(core_type >= SEM_TYPE_NULL && core_type <= SEM_TYPE_OBJECT);
  bputc(output, nullable ? code_nullable[core_type] : code_not_nullable[core_type]);
}

static void cg_lua_emit_field_types(charbuf *output, sem_struct *sptr) {
  bputc(output, '"');

  for (int32_t i = 0; i < sptr->count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    cg_lua_put_typecode(output, sem_type);
  }

  bputc(output, '"');
}

// This emits the declaration for an "auto cursor" -- that is a cursor
// that includes storage for all the fields it can fetch.  In LUA all
// cursors have storage.  When you do FETCH INTO first the cursor is loaded
// and then the variables are assigned, so there is only the one path.
static void cg_lua_declare_auto_cursor(CSTR cursor_name, sem_struct *sptr) {
  Contract(cursor_name);
  Contract(sptr);

  // this should really zero the cursor
  bprintf(cg_declarations_output, "local %s = { _has_row_ = false }\n", cursor_name);
  bprintf(cg_declarations_output, "local %s_fields_ = ", cursor_name);
  cg_lua_emit_field_names(cg_declarations_output, sptr);
  bprintf(cg_declarations_output, "\n");
  bprintf(cg_declarations_output, "local %s_types_ = ", cursor_name);
  cg_lua_emit_field_types(cg_declarations_output, sptr);
  bprintf(cg_declarations_output, "\n");
}

// Declaring a cursor causes us to do the following:
//  * emit a local variable for the cursor in the declarations section
//  * emit cleanup logic for that local in the cleanup section
//  * execute the select or call statement that is associated with the cursor
//    * store the resulting statement for use later in fetch
//  * declare a hidden has_row local for the cursor so that the cursor name
//    can be used in expressions to see if a row was fetched.
static void cg_lua_declare_cursor(ast_node *ast) {
  Contract(is_ast_declare_cursor(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(cursor_name, name_ast);

  // TODO, finalize cursor before fetching if in loop cg_c does this

  bool_t is_for_select = false;
  bool_t is_for_call = false;
  bool_t is_for_expr = false;
  bool_t out_union_processing = false;
  bool_t is_boxed = !!(name_ast->sem->sem_type & SEM_TYPE_BOXED);
  bool_t is_unboxing = true;

  if (is_ast_call_stmt(ast->right)) {
    out_union_processing = has_out_union_stmt_result(ast);
    is_for_call = true;
    is_unboxing = false;
    EXTRACT_STRING(name, ast->right->left);
  }
  else if (is_select_stmt(ast->right)) {
    is_for_select = true;
    is_unboxing = false;
  }
  else {
    is_for_expr = true;
    if (ends_in_set(ast->right->sem->kind)) {
      out_union_processing = true;
      is_unboxing = false;
    }
  }

  // only one of these (is boxed makes no sense with out union)
  Invariant(!out_union_processing || !is_boxed);

  // can't be both of these either
  Invariant(!out_union_processing || !is_unboxing);

  // unboxing implies is_boxed   a->b <==> (!a | b)
  Invariant(!is_unboxing || is_boxed);

  if (out_union_processing) {
    bprintf(cg_declarations_output, "local %s_result_set_ = nil\n", cursor_name);
    bprintf(cg_declarations_output, "local %s_row_num_ = 0\n", cursor_name);
    bprintf(cg_declarations_output, "local %s_row_count_ = 0\n", cursor_name);

    if (is_for_expr) {
      EXTRACT_ANY_NOTNULL(expr, ast->right);
      CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

      bprintf(cg_main_output, "%s_result_set_ = %s\n", cursor_name, expr_value.ptr);
      bprintf(cg_main_output, "%s_row_num_ = 0\n", cursor_name);
      bprintf(cg_main_output, "%s_row_count_ = #(%s_result_set_)\n", cursor_name, cursor_name);

      CG_LUA_POP_EVAL(expr);
    }
  }
  else {
    bprintf(cg_declarations_output, "local %s_stmt = nil\n", cursor_name);

    if (!is_boxed) {
      // easy case, no boxing, just finalize on exit.
      bprintf(cg_cleanup_output, "  cql_finalize_stmt(%s_stmt)\n", cursor_name);
      bprintf(cg_cleanup_output, "  %s_stmt = nil\n", cursor_name);

      if (lua_in_loop) {
        // tricky case, the call might iterate so we have to clean up the cursor before we do the call
        bprintf(cg_main_output, "cql_finalize_stmt(%s_stmt)\n", cursor_name);
      }
    }
  }

  if (is_for_select) {
    // DECLARE [name] CURSOR FOR [select_stmt]
    // or
    // DECLARE [name] CURSOR FOR [explain_stmt]
    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

    if (is_boxed) {
      // The next prepare will finalize the statement, we don't want to do that
      // if the cursor is being handled by boxes. The box downcount will take care of it
      bprintf(cg_main_output, "%s_stmt = nil\n", cursor_name);
    }
    cg_lua_bound_sql_statement(cursor_name, select_stmt, CG_PREPARE|CG_MINIFY_ALIASES);
  }
  else if (is_unboxing) {
    Invariant(is_for_expr);

    // DECLARE [name] CURSOR FOR [box_object_expr]
    EXTRACT_ANY_NOTNULL(expr, ast->right);
    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

    bprintf(cg_main_output, "%s_stmt = %s\n", cursor_name, expr_value.ptr);

    CG_LUA_POP_EVAL(expr);
  }
  else if (is_for_expr) {
  }
  else {
    Invariant(is_for_call);
    // DECLARE [name] CURSOR FOR [call_stmt]]
    if (is_boxed) {
      // The next prepare will finalize the statement, we don't want to do that
      // if the cursor is being handled by boxes. The box downcount will take care of it
      bprintf(cg_main_output, "%s_stmt = nil\n", cursor_name);
    }

    EXTRACT_NOTNULL(call_stmt, ast->right);
    cg_lua_call_stmt_with_cursor(call_stmt, cursor_name);
  }

  // in lua we always use "auto cursor" form we don't do cursor without storage
  // we just copy from the cursor if we need to when doing fetch into.  This makes
  // things a lot more symmetric.
  cg_lua_declare_auto_cursor(cursor_name, name_ast->sem->sptr);

  // in C you have to put something in the .h file if you want to have a global cursor
  // we have none of that in LUA so... nothing here... for global cursor stuff.  see cg_c.c
  // if you want to see the sadness that is C.
}

// This is the cursor boxing primitive, we'll make an object variable for this cursor here
// Note since the cursor is boxed its lifetime is already controlled by an object associated
// with the cursor.  This happens as soon as the cursor is created, however it is created.
// The codegen system knows that the cursor may be boxed at some point using the SEM_TYPE_BOXED flag
static void cg_lua_set_from_cursor(ast_node *ast) {
  Contract(is_ast_set_from_cursor(ast));
  EXTRACT_ANY_NOTNULL(variable, ast->left);
  EXTRACT_ANY_NOTNULL(cursor, ast->right);
  EXTRACT_STRING(cursor_name, cursor);
  EXTRACT_STRING(var_name, variable);

  // in LUA the statement is already an object, we just store it
  bprintf(cg_main_output, "%s = %s_stmt\n", var_name, cursor_name);
}

static void cg_lua_declare_cursor_like(ast_node *name_ast) {
  EXTRACT_STRING(cursor_name, name_ast);

  Contract(name_ast->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE);
  cg_lua_declare_auto_cursor(cursor_name, name_ast->sem->sptr);
}

static void cg_lua_declare_cursor_like_name(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_name(ast));
  Contract(ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  cg_lua_declare_cursor_like(name_ast);
}

static void cg_lua_declare_cursor_like_select(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_select(ast));
  Contract(is_select_stmt(ast->right));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  cg_lua_declare_cursor_like(name_ast);
}

static void cg_lua_declare_cursor_like_typed_names(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_typed_names(ast));
  Contract(is_ast_typed_names(ast->right));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  cg_lua_declare_cursor_like(name_ast);
}

// The value cursor form for sure will be fetched.   We emit the necessary locals
// for the cursor here.
static void cg_lua_declare_value_cursor(ast_node *ast) {
  Contract(is_ast_declare_value_cursor(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(cursor_name, name_ast);
  EXTRACT_NOTNULL(call_stmt, ast->right);

  // DECLARE [name] CURSOR FETCH FROM [call_stmt]]
  cg_lua_declare_auto_cursor(cursor_name, name_ast->sem->sptr);
  cg_lua_call_stmt_with_cursor(call_stmt, cursor_name);
}

// Fetch values has been checked for the presence of all columns and seed values
// have already been added if needed.
static void cg_lua_fetch_values_stmt(ast_node *ast) {
  Contract(is_ast_fetch_values_stmt(ast));

  EXTRACT(insert_dummy_spec, ast->left);
  EXTRACT(name_columns_values, ast->right);
  EXTRACT_ANY_NOTNULL(cursor, name_columns_values->left)
  EXTRACT(columns_values, name_columns_values->right);
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT(insert_list, columns_values->right);
  EXTRACT(name_list, column_spec->left);

  if (insert_dummy_spec) {
    cg_lua_insert_dummy_spec(insert_dummy_spec);
  }

  // get the canonical name of the cursor (the string might be case-sensitively different)
  CSTR cursor_name = cursor->sem->name;

  // FETCH name [( name_list )] FROM VALUES (insert_list) [insert_dummy_spec]

  ast_node *value = insert_list;

  bprintf(cg_main_output, "%s._has_row_ = true\n", cursor_name);

  for (ast_node *item = name_list ; item; item = item->right, value = value->right) {
    EXTRACT_ANY_NOTNULL(expr, value->left);
    EXTRACT_ANY_NOTNULL(col, item->left);
    EXTRACT_STRING(var, col);

    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
    CHARBUF_OPEN(temp);
    bprintf(&temp, "%s.%s", cursor_name, var);
    cg_lua_store(cg_main_output, temp.ptr, col->sem->sem_type, expr->sem->sem_type, expr_value.ptr);
    CHARBUF_CLOSE(temp);
    CG_LUA_POP_EVAL(expr);
  }
}

// native blob storage support, these are just cursor calls
static void cg_lua_fetch_cursor_from_blob_stmt(ast_node *ast) {
  Contract(is_ast_fetch_cursor_from_blob_stmt(ast));
  CSTR cursor_name = ast->left->sem->name;

  EXTRACT_ANY_NOTNULL(blob, ast->right);
  Invariant(is_blob(blob->sem->sem_type));

  CG_LUA_PUSH_EVAL(blob, C_EXPR_PRI_ROOT);

  bprintf(cg_main_output,
    "_rc_, %s = cql_deserialize_from_blob(%s)\n", cursor_name, blob_value.ptr);
  cg_lua_error_on_not_sqlite_ok();

  CG_LUA_POP_EVAL(blob);
}

// native blob storage support, these are just cursor calls
static void cg_lua_set_blob_from_cursor_stmt(ast_node *ast) {
  Contract(is_ast_set_blob_from_cursor_stmt(ast));

  CSTR blob_name  = ast->left->sem->name;
  CSTR cursor_name = ast->right->sem->name;

  bprintf(cg_main_output,
    "_rc_, %s = cql_serialize_to_blob(%s);\n", blob_name, cursor_name);
  cg_lua_error_on_not_sqlite_ok();
}

// Fetch has already been rigorously checked so we don't have to worry about
// argument counts or type mismatches in the codegen.  We have two cases:
//  * Fetch into variables
//    * loop over the variables which must match with the columns (!) and
//      use the cg_lua_get_column helpers to emit the code for a store
//  * Fetch into auto variables
//    * loop over the field names of the sem_struct that corresponds to the cursor
//    * set each local according to the automatically generated name as above
// Note: cg_lua_get_column does the error processing
static void cg_lua_fetch_stmt(ast_node *ast) {
  Contract(is_ast_fetch_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor_ast, ast->left);
  EXTRACT(name_list, ast->right);

  // use the canonical name, not the AST name (case could be different)
  CSTR cursor_name = cursor_ast->sem->name;

  // FETCH [name] [INTO [name_list]]

  bool_t uses_out_union = !!(ast->sem->sem_type & SEM_TYPE_USES_OUT_UNION);

  if (uses_out_union) {
    bprintf(cg_main_output, "%s_row_num_ = %s_row_num_ + 1\n", cursor_name, cursor_name);
    bprintf(cg_main_output, "if %s_row_num_ <= %s_row_count_ then\n", cursor_name, cursor_name);
    bprintf(cg_main_output, "  %s = %s_result_set_[%s_row_num_]\n", cursor_name, cursor_name, cursor_name);
    bprintf(cg_main_output, "else\n");
    // this should really zero the cursor
    bprintf(cg_main_output, "  %s = { _has_row_ = false }\n", cursor_name);
    bprintf(cg_main_output, "end\n");
  }

  // if there is a row, then we need to read the row into the variables
  // there are two alternatives: reading into locals/args or reading into
  // auto-generated cursor variables.  Either way we get each column.

  sem_struct *sptr = ast->left->sem->sptr;
  if (uses_out_union) {
  }
  else {
    bprintf(cg_main_output, "-- step and fetch\n");
    bprintf(cg_main_output, "_rc_ = cql_multifetch(%s_stmt, %s, %s_types_, %s_fields_",
      cursor_name, cursor_name, cursor_name, cursor_name);
    bprintf(cg_main_output, ")\n");
    cg_lua_error_on_expr("_rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE");
  }

  // the fetch INTO case reads out the fields from cursor which was fetched as usual
  if (name_list) {
    int32_t i = 0; // column get is zero based

    for (ast_node *item = name_list; item; item = item->right, i++) {
      EXTRACT_ANY_NOTNULL(name_ast, item->left);
      EXTRACT_STRING(var, name_ast);
      bprintf(cg_main_output, "%s = %s.", var, cursor_name);
      if (strcmp(sptr->names[i], "_anon")) {
        bprintf(cg_main_output, "%s", sptr->names[i]);
      }
      else {
        bprintf(cg_main_output, "_anon%d", i);
      }
      bprintf(cg_main_output, "\n");
    }
  }
}

static void cg_lua_fetch_call_stmt(ast_node *ast) {
  Contract(is_ast_fetch_call_stmt(ast));
  EXTRACT_STRING(cursor_name, ast->left);
  EXTRACT_ANY_NOTNULL(call_stmt, ast->right);

  cg_lua_call_stmt_with_cursor(call_stmt, cursor_name);
}

// The update cursor statement differs from the more general fetch form in that
// it is only to be used to tweak fields in an already loaded cursor.  The sematics
// are that if you try to "update" a cursor with no row the update is ignored.
// The purpose of this is to let you edit one or two fields of a row as you fetch them
// before using OUT or OUT UNION or INSERT ... FROM CURSOR.  You want to do this
// without having to restate all the columns, which besides being verbose makes it hard
// for people to see what things you are changing and what you are not.
static void cg_lua_update_cursor_stmt(ast_node *ast) {
  Contract(is_ast_update_cursor_stmt(ast));
  EXTRACT_ANY(cursor, ast->left);
  EXTRACT_STRING(name, cursor);
  EXTRACT_NOTNULL(columns_values, ast->right);
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT_ANY_NOTNULL(name_list, column_spec->left);
  EXTRACT_ANY_NOTNULL(insert_list, columns_values->right);

  bprintf(cg_main_output, "if %s._has_row_ then\n", name);

  CG_PUSH_MAIN_INDENT(stores, 2);

  ast_node *col = name_list;
  ast_node *val = insert_list;

  for ( ; col && val; col = col->right, val = val->right) {
    ast_node *expr = val->left;
    ast_node *name_ast = col->left;

    CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
    CHARBUF_OPEN(temp);
    bprintf(&temp, "%s.%s", name, name_ast->sem->name);
    cg_lua_store(cg_main_output, temp.ptr, name_ast->sem->sem_type, expr->sem->sem_type, expr_value.ptr);
    CHARBUF_CLOSE(temp);
    CG_LUA_POP_EVAL(expr);
  }

  CG_POP_MAIN_INDENT(stores);

  bprintf(cg_main_output, "end\n");
}

// Here we just emit the various values for an IF expression that is part of
// a SWITCH/WHEN clause
//  * the correct indent level is already set up
//  * we know evaluation will work because the semantic pass already checked it
//  * formatting numbers never fails, we use LUA number format
static void cg_lua_switch_expr_list(ast_node *ast, sem_t sem_type_switch_expr, CSTR val) {
  Contract(is_ast_expr_list(ast));

  bprintf(cg_main_output, "if ");

  while (ast) {
    Contract(is_ast_expr_list(ast));
    EXTRACT_ANY_NOTNULL(expr, ast->left);

    eval_node result = EVAL_NIL;
    eval(expr, &result);
    Invariant(result.sem_type != SEM_TYPE_ERROR); // already checked

    bprintf(cg_main_output, "%s == ", val);

    eval_format_number(&result, EVAL_FORMAT_FOR_LUA, cg_main_output);

    if (ast->right) {
      bprintf(cg_main_output, " or ");
    }

    ast = ast->right;
  }
  bprintf(cg_main_output, " then\n");
}

// Switch actually generates pretty easily because of the constraints that were
// placed on the various expressions.  We know that the case lables are all
// integers and we know that the expression type of the switch expression is
// a not null integer type so we can easily generate the switch form.  Anything
// that could go wrong has already been checked.  In LUA there is no switch
// statement so we just generate a series of IF statements and an ELSE case.
// We put all that into a repeat .. until true loop so that we can use "break"
// to get out of the loop.
static void cg_lua_switch_stmt(ast_node *ast) {
  Contract(is_ast_switch_stmt(ast));
  EXTRACT_NOTNULL(switch_body, ast->right);
  EXTRACT_ANY_NOTNULL(expr, switch_body->left);
  EXTRACT_NOTNULL(switch_case, switch_body->right);

  // SWITCH [expr] [switch_body] END
  // SWITCH [expr] ALL VALUES [switch_body] END

  sem_t sem_type_expr = expr->sem->sem_type;

  CG_LUA_PUSH_TEMP(val, sem_type_expr);
  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);
  cg_lua_copy(cg_main_output, val.ptr, sem_type_expr, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);

  bprintf(cg_main_output, "repeat\n");

  CG_PUSH_MAIN_INDENT(cases, 2);

  bool_t first_case = true;

  bool_t has_default = false;
  for (ast_node *temp = switch_case; temp; temp = temp->right) {
    EXTRACT_NOTNULL(connector, temp->left);
    if (!connector->left) {
      has_default = true;
    }
  }

  while (switch_case) {
    EXTRACT_NOTNULL(connector, switch_case->left);
    EXTRACT(stmt_list, connector->right);

    // no stmt list corresponds to WHEN ... THEN NOTHING
    // we can skip the entire case set unless there is a default
    // in which case we have to emit it with just break...
    if (stmt_list || has_default) {
      if (!first_case) {
        bprintf(cg_main_output, "\n");  // break between statement lists
      }
      first_case = false;

      // no expr list corresponds to the else case
      if (connector->left) {
        EXTRACT_NOTNULL(expr_list, connector->left);
        cg_lua_switch_expr_list(expr_list, expr->sem->sem_type, val.ptr);
      }
      else {
        bprintf(cg_main_output, "-- default\n");
      }

      if (stmt_list) {
        cg_lua_stmt_list(stmt_list);
      }

      if (connector->left) {
        bprintf(cg_main_output, "  break\n");
        bprintf(cg_main_output, "end\n");
      }
    }
    switch_case = switch_case->right;
  }

  CG_POP_MAIN_INDENT(cases);
  bprintf(cg_main_output, "until true\n");

  CG_LUA_POP_TEMP(val);
}

// "While" suffers from the same problem as IF and as a consequence
// generating while (expression) would not generalize.
// The overall pattern for while has to look like this:
//
//  while true
//  do
//    prep statements;
//    condition = final expression;
//    if  not(condition) then break end
//
//    statements;
//    ::continue_label%d::
//  end
//
// Note that while can have leave and continue substatements which have to map
// to break and goto ::continue::.   That means other top level statements that aren't loops
// must not create a C loop construct or break/continue would have the wrong target.
static void cg_lua_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);
  sem_t sem_type = expr->sem->sem_type;

  bool_t lua_continue_label_needed_saved = lua_continue_label_needed;
  int32_t lua_continue_label_number_saved = lua_continue_label_number;
  lua_continue_label_needed = false;
  lua_continue_label_next++;
  lua_continue_label_number = lua_continue_label_next;

  // WHILE [expr] BEGIN [stmt_list] END

  bprintf(cg_main_output, "while true\n");
  bprintf(cg_main_output, "do\n");

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ROOT);

  // note that not(nil) is true in lua because nil is falsey
  // so we correctly break out of the while if the expression's value is nil
  cg_lua_to_bool(sem_type, &expr_value);
  bprintf(cg_main_output, "if not(%s) then break end\n", expr_value.ptr);

  bool_t loop_saved = lua_in_loop;
  lua_in_loop = true;

  CG_LUA_POP_EVAL(expr);

  cg_lua_stmt_list(stmt_list);

  if (lua_continue_label_needed) {
    bprintf(cg_main_output, "::continue%d::\n", lua_continue_label_number);
  }

  bprintf(cg_main_output, "end\n");

  lua_in_loop = loop_saved;
  lua_continue_label_needed = lua_continue_label_needed_saved;
  lua_continue_label_number = lua_continue_label_number_saved;
}

// The general pattern for this is very simple:
//   while true
//   do
//     do the fetch
//     if no rows then break end
//     do your loop
//   end
// It has to be this because the fetch might require many statements.
// There are helpers for all of this so it's super simple.
static void cg_lua_loop_stmt(ast_node *ast) {
  Contract(is_ast_loop_stmt(ast));
  EXTRACT_NOTNULL(fetch_stmt, ast->left);
  EXTRACT(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(cursor_ast, fetch_stmt->left);

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = cursor_ast->sem->name;

  // LOOP [fetch_stmt] BEGIN [stmt_list] END

  bprintf(cg_main_output, "while true\ndo\n");
  CG_PUSH_MAIN_INDENT(loop, 2);

  cg_lua_fetch_stmt(fetch_stmt);

  bprintf(cg_main_output, "if not %s._has_row_ then break end\n", cursor_name);

  bool_t loop_saved = lua_in_loop;
  lua_in_loop = true;

  bool_t lua_continue_label_needed_saved = lua_continue_label_needed;
  int32_t lua_continue_label_number_saved = lua_continue_label_number;
  lua_continue_label_needed = false;
  lua_continue_label_next++;
  lua_continue_label_number = lua_continue_label_next;

  CG_POP_MAIN_INDENT(loop);

  cg_lua_stmt_list(stmt_list);

  if (lua_continue_label_needed) {
    bprintf(cg_main_output, "::continue%d::\n", lua_continue_label_number);
  }

  bprintf(cg_main_output, "end\n");

  lua_in_loop = loop_saved;
  lua_continue_label_needed = lua_continue_label_needed_saved;
  lua_continue_label_number = lua_continue_label_number_saved;
}

// Only SQL loops are allowed to use C loops, so "continue" is perfect
static void cg_lua_continue_stmt(ast_node *ast) {
  Contract(is_ast_continue_stmt(ast));

  // CONTINUE
  bprintf(cg_main_output, "goto continue%d\n", lua_continue_label_number);
  lua_continue_label_needed = true;
}

// Only SQL loops are allowed to use C loops, so "break" is perfect
static void cg_lua_leave_stmt(ast_node *ast) {
  Contract(is_ast_leave_stmt(ast));

  // LEAVE
  bprintf(cg_main_output, "break\n");
}

// We go to the main cleanup label and exit the current procedure
static void cg_lua_return_stmt(ast_node *ast) {
  Contract(is_ast_return_stmt(ast) || is_ast_rollback_return_stmt(ast) || is_ast_commit_return_stmt(ast));

  // RETURN
  bool_t dml_proc = is_dml_proc(current_proc->sem->sem_type);
  if (dml_proc) {
    bprintf(cg_main_output, "_rc_ = CQL_OK -- clean up any CQL_ROW value or other non-error\n");
  }
  bprintf(cg_main_output, "goto %s -- return\n", CQL_CLEANUP_DEFAULT_LABEL);
  lua_return_used = true;
}

// Rollback the current procedure savepoint, then perform a return.
// Note that to rollback a savepoint you have to do the rollback AND the release
// and then you're unwound to the savepoint state.  The transaction in flight is
// still in flight if there is one.
static void cg_lua_rollback_return_stmt(ast_node *ast) {
  Contract(is_ast_rollback_return_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
  ast_node *rollback = new_ast_rollback_trans_stmt(new_ast_str(lua_current_proc_name()));
  ast_node *release = new_ast_release_savepoint_stmt(new_ast_str(lua_current_proc_name()));
  AST_REWRITE_INFO_RESET();

  cg_lua_bound_sql_statement(NULL, rollback, CG_EXEC);
  cg_lua_bound_sql_statement(NULL, release, CG_EXEC);
  cg_lua_return_stmt(ast);
}

// Commits the current procedure savepoint, then perform a return.
// Note savepoint semantics are just "release" is sort of like commit
// in that it doesn't rollback and becomes part of the current transaction
// which may or may not commit but that's what we mean by commit.
static void cg_lua_commit_return_stmt(ast_node *ast) {
  Contract(is_ast_commit_return_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
  ast_node *commit = new_ast_release_savepoint_stmt(new_ast_str(lua_current_proc_name()));
  AST_REWRITE_INFO_RESET();

  cg_lua_bound_sql_statement(NULL, commit, CG_EXEC);
  cg_lua_return_stmt(ast);
}

// Finalize the statement object associated with the cursor.
// Note this sets the cursor to null, so you can do it again.  Cleanup
// might also do this. That's fine.
static void cg_lua_close_stmt(ast_node *ast) {
  Contract(is_ast_close_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor_ast, ast->left);
  EXTRACT_STRING(name, cursor_ast);

  // CLOSE [name]

  sem_t sem_type = cursor_ast->sem->sem_type;

  if (!(sem_type & SEM_TYPE_VALUE_CURSOR)) {
    bprintf(cg_main_output, "cql_finalize_stmt(%s_stmt)\n", name);
    bprintf(cg_main_output, "%s_stmt = nil\n", name);
  }
  // this should really zero the cursor
  bprintf(cg_main_output, "%s = { _has_row_ = false }\n", name);
}

// The OUT statement copies the current value of a cursor into an implicit
// OUT structure variable (_result_).  The type of the variable is inferred
// from the cursor you return.  All OUT statements in any given proc must
// agree on the exact type (this has already been verified).  At this point
// all we have to do is copy the fields.
static void cg_lua_out_stmt(ast_node *ast) {
  Contract(is_ast_out_stmt(ast));

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = ast->left->sem->name;

  // OUT [cursor_name]

  bprintf(cg_main_output, "_result_ = cql_clone_row(%s)\n", cursor_name);
}

static void cg_lua_out_union_stmt(ast_node *ast) {
  Contract(is_ast_out_union_stmt(ast));

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = ast->left->sem->name;

  // OUT UNION [cursor_name]

  bprintf(cg_main_output, "if %s._has_row_ then\n", cursor_name);
  bprintf(cg_main_output, "  table.insert(_rows_, cql_clone_row(%s))\n", cursor_name);
  bprintf(cg_main_output, "end\n");
}

// emit the string literal into the otuput if the current runtime matches
static void cg_lua_echo_stmt(ast_node *ast) {
  Contract(is_ast_echo_stmt(ast));
  EXTRACT_STRING(rt_name, ast->left);
  EXTRACT_STRING(str, ast->right);

  // @ECHO [rt], [str]

  if (!Strcasecmp(rt_name, options.rt)) {
    if (current_proc) {
      cg_decode_string_literal(str, cg_main_output);
    } else {
      cg_decode_string_literal(str, cg_declarations_output);
    }
  }
}

// This is the helper method to dispatch a call to an external function like "printf"
// given a name in the AST.  This is for when the user coded the call.
static void cg_lua_call_external(ast_node *ast) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY(arg_list, ast->right);

  return cg_lua_call_named_external(name, arg_list);
}

// This is performs an external function call, normalizing strings and passing
// the current value of nullables.  It's all straight up value-calls.  This form
// is used when the name might not be in the AST, such as we need a call to
// a sqlite helper method with user provided args.  All we do here is emit
// the  name and then use the arg list helper.
// The arg list helper gives us prep/invocation/cleanup buffers which we must emit.
static void cg_lua_call_named_external(CSTR name, ast_node *arg_list) {
  CHARBUF_OPEN(invocation);
  CHARBUF_OPEN(prep);
  CHARBUF_OPEN(cleanup);

  // Note this function is called in an expression context such as
  // for the builtin "printf" SQL function it can also be called in the call
  // statement context such as "call printf();"  In the second case it's
  // top level and the stack doesn't matter as it will be reset but in the first
  // case we need to restore the temp stack after we are done with the args.
  int32_t lua_stack_level_saved = lua_stack_level;

  bprintf(&invocation, "%s(", name);
  cg_lua_emit_external_arglist(arg_list, &prep, &invocation, &cleanup);
  bprintf(&invocation, ")\n");

  bprintf(cg_main_output, "%s%s%s", prep.ptr, invocation.ptr, cleanup.ptr);

  lua_stack_level = lua_stack_level_saved;  // put the scratch stack back

  CHARBUF_CLOSE(cleanup);
  CHARBUF_CLOSE(prep);
  CHARBUF_CLOSE(invocation);
}

// This is the hard work of doing the call actually happens.  We have to:
//   * evaluate each argument in the arg list
//   * emit a standard call for the lot
//   * there are no out args, so any reference to an out arg means the local copy
//   * there is no return value (that's what native functions are for)
static void cg_lua_emit_external_arglist(ast_node *arg_list, charbuf *prep, charbuf *invocation, charbuf *cleanup) {
  for (ast_node *item = arg_list; item; item = item->right) {
    EXTRACT_ANY(arg, item->left);

    CG_LUA_PUSH_EVAL(arg, LUA_EXPR_PRI_ROOT);
    bprintf(invocation, "%s", arg_value.ptr);
    CG_LUA_POP_EVAL(arg);

    if (item->right) {
      bprintf(invocation, ", ");
    }
  }
}

// When performing a call there are several things we might need to do to the arguments
// in order to get the correct calling convention.
//  * strings are already references, they go as is.
//  * not-nullables can go as is, unless
//  * if the paramater is not nullable and the argument is compatible but not an exact match,
//    then we box the argument into a temporary not nullable and pass that through
//  * finally, both the paramater and the argument was not nullable then we have to recover
//    the variable name from the evaluated value.
static void cg_lua_emit_one_arg(ast_node *arg, sem_t sem_type_param, sem_t sem_type_arg, charbuf *invocation, charbuf *returns) {
  CG_LUA_PUSH_EVAL(arg, LUA_EXPR_PRI_ROOT);

  if (is_out_parameter(sem_type_param)) {
    if (returns->used > 1) {
      bprintf(returns, ", ");
    }
    bprintf(returns, "%s", arg->sem->name);
  }

  if (is_in_parameter(sem_type_param)) {
    // either way arg_value is now correct
    if (invocation->used > 1) {
      bprintf(invocation, ", ");
    }

    if (is_cursor_formal(sem_type_param)) {
      // cursor formal expands to three actual arguments
      bprintf(invocation, "%s, %s_types_, %s_fields_", arg->sem->name, arg->sem->name, arg->sem->name);
    }
    else if (is_bool(sem_type_param) && !is_bool(sem_type_arg)) {
       cg_lua_emit_to_bool(invocation, arg_value.ptr);
    }
    else if (!is_bool(sem_type_param) && is_bool(sem_type_arg)) {
       cg_lua_emit_to_num(invocation, arg_value.ptr);
    }
    else {
      bprintf(invocation, "%s", arg_value.ptr);
    }
  }

  CG_LUA_POP_EVAL(arg);
}

// This generates the invocation for a user defined external function.
// Basically we do a simple invoke with the matching argument types which are known exactly
// we do the usual argument conversions using cg_lua_emit_one_arg just like when calling procedures
// however we capture the return type in a temporary variable created exactly for this purpose.
// This code is also used in the proc as func path hence the dml stuff
static void cg_lua_user_func(ast_node *ast, charbuf *value) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  ast_node *params = NULL;
  ast_node *func_stmt = find_func(name);
  CSTR func_name = NULL;

  bool_t proc_as_func = false;
  bool_t dml_proc = false;
  bool_t result_set_return = false;

  if (func_stmt) {
    EXTRACT_STRING(fname, func_stmt->left);
    params = get_func_params(func_stmt);
    func_name = fname;
  }
  else {
    // has to be one of these two, already validated
    ast_node *proc_stmt = find_proc(name);
    Invariant(proc_stmt);
    params = get_proc_params(proc_stmt);
    ast_node *proc_name_ast = get_proc_name(proc_stmt);
    EXTRACT_STRING(pname, proc_name_ast);
    func_name = pname;
    proc_as_func = true;
    dml_proc = is_dml_proc(proc_stmt->sem->sem_type);

    result_set_return = has_out_stmt_result(proc_stmt) || has_result_set(proc_stmt) || has_out_union_stmt_result(proc_stmt);
  }

  sem_t sem_type_result = ast->sem->sem_type;

  // The answer will be stored in this scratch variable, any type is possible
  CG_LUA_SETUP_RESULT_VAR(ast, sem_type_result);
  CHARBUF_OPEN(args);
  CHARBUF_OPEN(returns);
  CG_CHARBUF_OPEN_SYM(func_sym, func_name, result_set_return ? "_fetch_results" : "");

  if (dml_proc) {
    // at least one arg for the out arg so add _db_ with comma
    bprintf(&args, "_db_");
    bprintf(&returns, "_rc_");
  }

  if (returns.used > 1) {
    bprintf(&returns, ", ");
  }
  bprintf(&returns, "%s", result_var.ptr);

  ast_node *item;
  for (item = arg_list; item; item = item->right, params = params->right) {
    EXTRACT_ANY(arg, item->left);
    sem_t sem_type_arg = arg->sem->sem_type;

    EXTRACT_NOTNULL(param, params->left);
    sem_t sem_type_param = param->sem->sem_type;

    cg_lua_emit_one_arg(arg, sem_type_param, sem_type_arg, &args, &returns);
  }

  // Now store the result of the call.
  // the only trick here is we have to make sure we honor create semantics
  // otherwise we can just copy the data since the variable is for sure
  // an exact match for the call return by construction.

  bprintf(cg_main_output, "%s = %s(%s)\n", returns.ptr, func_sym.ptr, args.ptr);

  if (proc_as_func && dml_proc) {
    // cascade the failure
    cg_lua_error_on_not_sqlite_ok();
  }

  CHARBUF_CLOSE(func_sym);
  CHARBUF_CLOSE(returns);
  CHARBUF_CLOSE(args);
  CG_LUA_CLEANUP_RESULT_VAR();  // this will restore the scratch stack for us
}

// Forward the call processing to the general helper (with cursor arg)
static void cg_lua_call_stmt(ast_node *ast) {
  // If the call has a result set it is stored in our result parameter
  // just like a loose select statement would be.  Note this can be
  // overridden by a later result which is totally ok.  Same as for select
  // statements.
  return cg_lua_call_stmt_with_cursor(ast, NULL);
}


// emit the declarations for anything implicitly declared then do a normal call
static void cg_lua_declare_out_call_stmt(ast_node *ast) {
  Contract(is_ast_declare_out_call_stmt(ast));
  EXTRACT_NOTNULL(call_stmt, ast->left);
  EXTRACT(expr_list, call_stmt->right);

  for (; expr_list; expr_list = expr_list->right) {
    EXTRACT_ANY_NOTNULL(arg, expr_list->left);
    if (arg->sem->sem_type & SEM_TYPE_IMPLICIT) {
      EXTRACT_STRING(var_name, arg);
      cg_lua_declare_simple_var(arg->sem->sem_type, var_name);
    }
  }

  cg_lua_call_stmt(call_stmt);
}


// This helper method walks all the args and all the formal paramaters at the same time
// it gets the appropriate type info for each and then generates the expression
// for the evaluation of that argument.
static void cg_lua_emit_proc_params(charbuf *output, charbuf *results, ast_node *params, ast_node *args) {
  for (ast_node *item = args; item; item = item->right, params = params->right) {
    EXTRACT_ANY_NOTNULL(arg, item->left);
    sem_t sem_type_arg = arg->sem->sem_type;

    EXTRACT_NOTNULL(param, params->left);
    sem_t sem_type_param = param->sem->sem_type;

    // note this might require type conversion, handled here.
    cg_lua_emit_one_arg(arg, sem_type_param, sem_type_arg, output, results);
  }
}

// A call statement has several varieties:
//  * an external call to an unknown proc
//    * use the external call helper
//  * if the target is a dml proc
//    * add the _db_ argument, for sure we have it because if we call a DML proc
//      we are a DML proc so we, too, had such an arg.  Pass it along.
//    * capture the _rc_ return code and do the error processing.
//  * if the proc returns a relational result (see below) we use the given
//    cursor to capture it, or else we use the functions result argument
//    as indicated below
//
// There are a variety of call forms (we'll see the symmetric version of this
// in cg_lua_create_proc_stmt).  The first thing to consider is, does the procedure
// produce some kind of relational result, there are four ways it can do this:
//
//   1. It returns a statement (it used a loose SELECT)
//   2. It returns a single row (it used OUT)
//   3. It returns a result set (it used OUT UNION)
//   4. It returns no relational result, just out args maybe.
//
//  Now we have to consider this particular call, and the chief question is
//  are we capturing the relational result in a cursor? If we are then referring
//  to the above:
//
//   1a. The cursor will be a statement cursor, holding the SQLite statement
//   2a. The cursor will hold the row, it is a value cursor (you can't step it)
//   3a. The cursor will hold a pointer to the result set which can be indexed
//   4a. A cursor cannot be used if there is no relational result.
//
//  Note that the error case above has already been detected in semantic analysis
//  so we would not be here if it happened.  This is true of the other error cases
//  as well.  If we're doing code-gen we know we're good.
//
//  If the result is not captured in a cursor then we have the following outcomes
//
//  1b. The current procedure returns statement as a relational result
//      (just as though it had done the select)
//  2b. This is not allowed, the row must be captured by a cursor (error).
//  3b. The current procedure returns the result set (just as though it had done
//      the OUT UNION)
//  4b. This is a "normal" function call with just normal arguments
//
// Compounding the above, the procedure might use the database or not.  If it uses
// the database (dml_proc) we have to add that argument and we expect a success code.
// If it doesn't use the database it can still return a relational result with
// OUT or OUT UNION.  It can't have done a SELECT (no database) or could it have
// called a procedure that did a SELECT (again, no database).  So the statement
// cursor case is eliminated. This creates a fairly complex matrix but most of the
// logic is highly similar.
//
// In call cases we can use the arg helper method to emit each arg.  There are
// several rules for each kind of arg, described above in cg_lua_emit_one_arg.
static void cg_lua_call_stmt_with_cursor(ast_node *ast, CSTR cursor_name) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY(expr_list, ast->right);

  // check for call to unknown proc, use canonical calling convention for those
  ast_node *proc_stmt = find_proc(name);
  if (!proc_stmt) {
    cg_lua_call_external(ast);
    return;
  }

  ast_node *proc_name_ast = get_proc_name(proc_stmt);
  EXTRACT_STRING(proc_name, proc_name_ast);

  ast_node *params = get_proc_params(proc_stmt);
  bool_t dml_proc = is_dml_proc(proc_stmt->sem->sem_type);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  CSTR fetch_results = out_union_proc ? "_fetch_results" : "";

  CG_CHARBUF_OPEN_SYM(proc_sym, proc_name, fetch_results);
  CG_CHARBUF_OPEN_SYM(result_type, proc_name, "_row");
  CG_CHARBUF_OPEN_SYM(result_sym, proc_name, "_row", "_data");
  CG_CHARBUF_OPEN_SYM(result_set_ref, name, "_result_set_ref");
  CHARBUF_OPEN(args);
  CHARBUF_OPEN(returns);

  if (dml_proc) {
    bprintf(&returns, "_rc_");
    bprintf(&args, "_db_");
  }

  if (out_union_proc && !cursor_name) {
    // This is case 3b above.  The tricky bit here is that there might
    // be more than one such call.  The callee is not going to release
    // the out arg as it might be junk from the callee's perspective so
    // we have to release it in case this call is in a loop or if this
    // call is repeated in some other way
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "_result_set_");
  }
  else if (out_union_proc) {
    // this is case 3a above.
    Invariant(cursor_name); // either specified or the default _result_ variable
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "%s_result_set_", cursor_name);
  }
  else if (result_set_proc && !cursor_name) {
    // This is case 1b above, prop the result as our output.  As with case
    // 3b above we have to pre-release _result_stmt_ because of repetition.
    bprintf(cg_main_output, "cql_finalize_stmt(_result_stmt)\n");
    bprintf(cg_main_output, "_result_stmt = nil\n");
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "_result_stmt");
  }
  else if (result_set_proc) {
    // this is case 1a above
    Invariant(cursor_name); // either specified or the default _result_ variable
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "%s_stmt", cursor_name);
  }
  else if (out_stmt_proc) {
    Invariant(cursor_name);
    if (returns.used > 1) {
      bprintf(&returns, ", ");
    }
    bprintf(&returns, "%s", cursor_name);
  }

  // we don't need to manage the stack, we're always called at the top level
  // we're wiping it when we exit this function anyway
  Invariant(lua_stack_level == 0);

  // emit provided args, the param specs are needed for possible type conversions
  cg_lua_emit_proc_params(&args, &returns, params, expr_list);

  // For a fetch results proc we have to add the out argument here.

  if (returns.used > 1) {
    bprintf(cg_main_output, "%s = ", returns.ptr);
  }
  bprintf(cg_main_output, "%s(%s)\n", proc_sym.ptr, args.ptr);

  if (dml_proc) {
    // if there is an error code, check it, and cascade the failure
    cg_lua_error_on_not_sqlite_ok();
  }

  if (out_union_proc && cursor_name) {
    // note lua indexes are 1 based
    // case 3a, capturing the cursor, we set the row index to 0 (it will be pre-incremented)
    bprintf(cg_main_output, "%s_row_num_ = 0\n", cursor_name);
    bprintf(cg_main_output, "%s_row_count_ = #(%s_result_set_)\n", cursor_name, cursor_name);
  }

  CHARBUF_CLOSE(returns);
  CHARBUF_CLOSE(args);
  CHARBUF_CLOSE(result_set_ref);
  CHARBUF_CLOSE(result_sym);
  CHARBUF_CLOSE(result_type);
  CHARBUF_CLOSE(proc_sym);
}

// Straight up DDL invocation.  The ast has the statement, execute it!
// We don't minify the aliases because DDL can have views and the view column names
// can be referred to in users of the view.  Loose select statements can have
// no external references to column aliases.
static void cg_lua_any_ddl_stmt(ast_node *ast) {
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC|CG_NO_MINIFY_ALIASES);
}

// Straight up DML invocation.  The ast has the statement, execute it!
static void cg_lua_std_dml_exec_stmt(ast_node *ast) {
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC|CG_MINIFY_ALIASES);
}

// DML with PREPARE.  The ast has the statement.
// Note: _result_ is the output variable for the sqlite3_stmt we generate
//       this was previously added when the stored proc params were generated.
static void cg_lua_select_stmt(ast_node *ast) {
  Contract(is_select_stmt(ast));
  cg_lua_bound_sql_statement("_result", ast, CG_PREPARE|CG_MINIFY_ALIASES);
}

// DML with PREPARE.  The ast has the statement.
static void cg_lua_with_select_stmt(ast_node *ast) {
  Contract(is_ast_with_select_stmt(ast));
  cg_lua_select_stmt(ast);
}

static void cg_lua_explain_stmt(ast_node *ast) {
  Contract(is_ast_explain_stmt(ast));
  cg_lua_bound_sql_statement("_result", ast, CG_PREPARE|CG_MINIFY_ALIASES);
}

static void cg_lua_insert_dummy_spec(ast_node *ast) {
  EXTRACT_ANY_NOTNULL(expr, ast->left); // the seed expr

  CSTR name = "_seed_";

  sem_t sem_type_var = SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL;
  sem_t sem_type_expr = expr->sem->sem_type;

  if (!lua_seed_declared) {
    cg_lua_var_decl(cg_declarations_output, sem_type_var, name);
    lua_seed_declared = true;
  }

  CG_LUA_PUSH_EVAL(expr, LUA_EXPR_PRI_ASSIGN);
  cg_lua_store(cg_main_output, name, sem_type_var, sem_type_expr, expr_value.ptr);
  CG_LUA_POP_EVAL(expr);
}

static void cg_lua_opt_seed_process(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(insert_type, ast->left);
  EXTRACT_ANY(insert_dummy_spec, insert_type->left);

  if (insert_dummy_spec) {
    cg_lua_insert_dummy_spec(insert_dummy_spec);
  }
}

// DML invocation but first set the seed variable if present
static void cg_lua_insert_stmt(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));

  cg_lua_opt_seed_process(ast);
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_lua_with_insert_stmt(ast_node *ast) {
  Contract(is_ast_with_insert_stmt(ast));
  EXTRACT_NOTNULL(insert_stmt, ast->right);
  cg_lua_opt_seed_process(insert_stmt);
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_lua_with_upsert_stmt(ast_node *ast) {
  Contract(is_ast_with_upsert_stmt(ast));
  EXTRACT_NOTNULL(upsert_stmt, ast->right);
  EXTRACT_NOTNULL(insert_stmt, upsert_stmt->left);
  cg_lua_opt_seed_process(insert_stmt);
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_lua_upsert_stmt(ast_node *ast) {
  Contract(is_ast_upsert_stmt(ast));
  EXTRACT_NOTNULL(insert_stmt, ast->left);

  cg_lua_opt_seed_process(insert_stmt);
  cg_lua_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// Very little magic is needed to do try/catch in our context.  The error
// handlers for all the sqlite calls check _rc_ and if it's an error they
// "goto" the current error target.  That target is usually CQL_CLEANUP_DEFAULT_LABEL.
// Inside the try block, the cleanup handler is changed to the catch block.
// The catch block puts it back.  Otherwise, generate nested statements as usual.
static void cg_lua_trycatch_helper(ast_node *try_list, ast_node *try_extras, ast_node *catch_list) {
  CHARBUF_OPEN(catch_start);
  CHARBUF_OPEN(catch_end);

  // We need unique labels for this block
  ++lua_catch_block_count;
  bprintf(&catch_start, "catch_start_%d", lua_catch_block_count);
  bprintf(&catch_end, "catch_end_%d", lua_catch_block_count);

  // Divert the error target.
  CSTR saved_lua_error_target = lua_error_target;
  bool_t saved_lua_error_target_used = lua_error_target_used;
  lua_error_target = catch_start.ptr;
  lua_error_target_used = false;

  // Emit the try code.
  bprintf(cg_main_output, "-- try\n\n");

  cg_lua_stmt_list(try_list);

  if (try_extras) {
    cg_lua_stmt_list(try_extras);
  }

  // If we get to the end, skip the catch block.
  bprintf(cg_main_output, "  goto %s\n\n", catch_end.ptr);

  // Emit the catch code, with labels at the start and the end.
  if (lua_error_target_used) {
    bprintf(cg_main_output, "::%s:: ", catch_start.ptr);
  }

  // Restore the error target, the catch block runs with the old error target
  lua_error_target = saved_lua_error_target;
  lua_error_target_used = saved_lua_error_target_used;
  CSTR lua_rcthrown_saved = lua_rcthrown_current;

  bprintf(cg_main_output, "\n");
  bprintf(cg_main_output, "do\n");

  CHARBUF_OPEN(rcthrown);

  bprintf(&rcthrown, "_rc_thrown_%d", ++lua_rcthrown_index);
  lua_rcthrown_current = rcthrown.ptr;
  bool_t lua_rcthrown_used_saved = lua_rcthrown_used;
  lua_rcthrown_used = false;

  CHARBUF_OPEN(catch_block);
    charbuf *main_saved = cg_main_output;
    cg_main_output = &catch_block;

    cg_lua_stmt_list(catch_list);

    cg_main_output = main_saved;

    if (lua_rcthrown_used) {
      bprintf(cg_main_output, "  local %s = _rc_\n", rcthrown.ptr);
    }

    bprintf(cg_main_output, "%s", catch_block.ptr);

  CHARBUF_CLOSE(catch_block);

  lua_rcthrown_current = lua_rcthrown_saved;
  lua_rcthrown_used = lua_rcthrown_used_saved;
  bprintf(cg_main_output, "end\n");

  bprintf(cg_main_output, "\n::%s::\n", catch_end.ptr);

  CHARBUF_CLOSE(rcthrown);
  CHARBUF_CLOSE(catch_end);
  CHARBUF_CLOSE(catch_start);
}

// the helper does all the work, see those notes
static void cg_lua_trycatch_stmt(ast_node *ast) {
  Contract(is_ast_trycatch_stmt(ast));
  EXTRACT_NAMED(try_list, stmt_list, ast->left);
  EXTRACT_NAMED(catch_list, stmt_list, ast->right);

  cg_lua_trycatch_helper(try_list, NULL, catch_list);
}

// this is just a special try/catch
static void cg_lua_proc_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_proc_savepoint_stmt(ast));
  EXTRACT(stmt_list, ast->left);

  if (stmt_list) {
    AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
    ast_node *savepoint = new_ast_savepoint_stmt(new_ast_str(lua_current_proc_name()));
    ast_node *release1  = new_ast_release_savepoint_stmt(new_ast_str(lua_current_proc_name()));
    ast_node *release2  = new_ast_release_savepoint_stmt(new_ast_str(lua_current_proc_name()));
    ast_node *rollback  = new_ast_rollback_trans_stmt(new_ast_str(lua_current_proc_name()));
    ast_node *try_extra_stmts = new_ast_stmt_list(release1, NULL);
    ast_node *throw_stmt = new_ast_throw_stmt();
    ast_node *catch_stmts =
		new_ast_stmt_list(rollback,
                new_ast_stmt_list(release2,
                new_ast_stmt_list(throw_stmt, NULL)));
    AST_REWRITE_INFO_RESET();
    cg_lua_bound_sql_statement(NULL, savepoint, CG_EXEC);
    cg_lua_trycatch_helper(stmt_list, try_extra_stmts, catch_stmts);
  }
}

// Convert _rc_ into an error code.  If it already is one keep it.
// Then go to the current error target.
static void cg_lua_throw_stmt(ast_node *ast) {
  Contract(is_ast_throw_stmt(ast));

  bprintf(cg_main_output, "_rc_ = cql_best_error(%s)\n", lua_rcthrown_current);
  bprintf(cg_main_output, "goto %s\n", lua_error_target);
  lua_error_target_used = true;
  lua_rcthrown_used = true;
}

// Dispatch to one of the statement helpers using the symbol table.
// There are special rules for the DDL methods. If they appear in a
// global context (outside of any stored proc) they do not run, they
// are considered declarations only.
static void cg_lua_one_stmt(ast_node *stmt, ast_node *misc_attrs) {
  // we're going to compute the fragment name if needed but we always start clean
  base_fragment_name = NULL;

  // reset the temp stack
  lua_stack_level = 0;

  // There are special rules for some procedures, we avoid emiting them here
  // so that we don't generate the comments or anything for them.  Testing later
  // is more of a mess.

  if (misc_attrs && is_ast_create_proc_stmt(stmt)) {
    // only assembly fragments get any output

    // sets base_fragment_name as well for the current fragment
    uint32_t frag_type = find_fragment_attr_type(misc_attrs, &base_fragment_name);

    if (frag_type == FRAG_TYPE_EXTENSION || frag_type == FRAG_TYPE_BASE || frag_type == FRAG_TYPE_SHARED) {
      return;
    }
  }

  symtab_entry *entry = symtab_find(cg_stmts, stmt->type);
  Contract(entry);

  if (!lua_in_proc) {
    // DDL operations not in a procedure are ignored
    // but they can declare schema during the semantic pass
    if (entry->val == cg_lua_any_ddl_stmt) {
       return;
    }

    // loose select statements also have no codegen, the global proc has no result type
    if (is_select_stmt(stmt)) {
       return;
    }
  }

  CHARBUF_OPEN(tmp_header);
  CHARBUF_OPEN(tmp_declarations);
  CHARBUF_OPEN(tmp_main);
  CHARBUF_OPEN(tmp_scratch);

  charbuf *header_saved = cg_header_output;
  charbuf *declarations_saved = cg_declarations_output;
  charbuf *main_saved = cg_main_output;
  charbuf *scratch_saved = cg_scratch_vars_output;

  // Redirect all output to the temporary buffers so we can see how big it is
  // The comments need to go before this, so we save the output then check it
  // then emit the generated code.

  cg_main_output = &tmp_main;
  cg_declarations_output = &tmp_declarations;
  cg_header_output = &tmp_header;
  cg_scratch_vars_output = &tmp_scratch;

  // These are all the statements there are, we have to find it in this table
  // or else someone added a new statement and it isn't supported yet.
  Invariant(entry);
  ((void (*)(ast_node*))entry->val)(stmt);

  // safe to put it back now
  cg_main_output = main_saved;
  cg_header_output = header_saved;
  cg_declarations_output = declarations_saved;
  cg_scratch_vars_output = scratch_saved;

  // Emit a helpful comment for top level statements.
  if (stmt_nesting_level == 1) {
    charbuf *out = cg_main_output;
    if (is_ast_declare_vars_type(stmt) || is_proc(stmt) || is_ast_echo_stmt(stmt)) {
      out = cg_declarations_output;
    }

    bool_t skip_comment = false;

    // don't contaminate echo output with comments except in test, where we need it for verification
    skip_comment |= (!options.test && is_ast_echo_stmt(stmt));

    // If no code gen in the main buffer, don't add a comment, that will force a global proc
    // We used to have all kinds of special cases to detect the statements that don't generate code
    // and that was a bug farm.  So now instead we just look to see if it made code.  If it didn't make
    // code we will not force the global proc to exist because of the stupid comment...
    skip_comment |= (out == cg_main_output && tmp_main.used == 1);

    // put a line marker in the header file in case we want a test suite that verifies that
    if (options.test) {
      bprintf(cg_header_output, "\n-- The statement ending at line %d\n", stmt->lineno);
    }

    // emit comments for most statements: we do not want to require the global proc block
    // just because there was a comment so this is suppressed for "no code" things
    if (!skip_comment) {
      if (options.test) {
        if (!options.compress) {
          bprintf(out, "\n-- The statement ending at line %d\n", stmt->lineno);
        }
      } else {
        if (!options.compress) {
          bprintf(cg_declarations_output, "\n-- Generated from %s:%d\n", stmt->filename, stmt->lineno);
        }
      }
      if (!options.compress) {
        // emit source comment
        bprintf(out, "\n--[[\n");
        gen_stmt_level = 1;
        gen_set_output_buffer(out);
        if (misc_attrs) {
          gen_misc_attrs(misc_attrs);
        }
        gen_one_stmt(stmt);
        bprintf(out, ";\n--]]\n");
      }
    }
  }

  // and finally write what we saved
  bprintf(cg_main_output, "%s", tmp_main.ptr);
  bprintf(cg_header_output, "%s", tmp_header.ptr);
  bprintf(cg_scratch_vars_output, "%s", tmp_scratch.ptr);
  bprintf(cg_declarations_output, "%s", tmp_declarations.ptr);

  CHARBUF_CLOSE(tmp_scratch);
  CHARBUF_CLOSE(tmp_main);
  CHARBUF_CLOSE(tmp_declarations);
  CHARBUF_CLOSE(tmp_header);
}

// Emit the nested statements with one more level of indenting.
static void cg_lua_stmt_list(ast_node *head) {
  if (!head) {
    return;
  }

  stmt_nesting_level++;

  charbuf *saved_main = cg_main_output;
  CHARBUF_OPEN(temp);
  cg_main_output = &temp;

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    cg_lua_one_stmt(stmt, misc_attrs);
  }

  cg_main_output = saved_main;
  bindent(cg_main_output, &temp, 2);
  CHARBUF_CLOSE(temp);

  stmt_nesting_level--;
}

// All the data you need to make a getter or setter...
// there's a lot of it and most of it is the same for all cases
typedef struct lua_function_info {
  CSTR name;
  CSTR col;
  int32_t col_index;
  charbuf *defs;
  charbuf *headers;
  bool_t uses_out;
  sem_t ret_type;
  sem_t name_type;
  CSTR result_set_ref_type;
  CSTR row_struct_type;
  CSTR sym_suffix;
  CSTR value_suffix;
  uint32_t frag_type;
  bool_t is_private;
} lua_function_info;


// If a stored procedure generates a result set then we need to do some extra work
// to create the C friendly rowset creating and accessing helpers.  If stored
// proc "foo" creates a row set then we need to:
//  * emit a struct "foo_row" that has the shape of each row
//    * this isn't used by the client code but we use it in our code-gen
//  * emit a function "foo_fetch_results" that will call "foo" and read the rows
//    from the statement created by "foo".
//    * this method will construct a result set object via cql_result_create and store the data
//    * the remaining functions use cql_result_set_get_data and _get_count to get the data back out
//  * for each named column emit a function "foo_get_[column-name]" which
//    gets that column out of the rowset for the indicated row number.
//  * prototypes for the above go into the main output header file
static void cg_lua_proc_result_set(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  Contract(is_struct(ast->sem->sem_type));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT_STRING(name, ast->left);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  bool_t suppress_result_set = misc_attrs && exists_attribute_str(misc_attrs, "suppress_result_set");
  bool_t is_private = misc_attrs && exists_attribute_str(misc_attrs, "private");

  bool_t uses_out_union = has_out_union_stmt_result(ast);

  if (!uses_out_union && (suppress_result_set || is_private)) {
    return;
  }

  bool_t uses_out = has_out_stmt_result(ast);
  bool_t result_set_proc = has_result_set(ast);

  // exactly one of these
  Invariant(uses_out + uses_out_union + result_set_proc == 1);

  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);

  // sets base_fragment_name as well for the current fragment
  uint32_t frag_type = find_fragment_attr_type(misc_attrs, &base_fragment_name);

  // register the proc name if there is a callback, the particular result type will do whatever it wants
  rt->register_proc_name && rt->register_proc_name(name);

  charbuf *d = cg_declarations_output;
  charbuf *main_saved = cg_main_output;
  cg_main_output = d;

  // name replacement such that extension fragment should always reference to
  // base result set type instead of setting up their own
  CSTR result_set_name = (frag_type == FRAG_TYPE_EXTENSION) ? base_fragment_name : name;

  CHARBUF_OPEN(data_types);
  CHARBUF_OPEN(result_set_create);
  CHARBUF_OPEN(temp);
  CG_CHARBUF_OPEN_SYM(getter_prefix, name);
  CG_CHARBUF_OPEN_SYM(proc_sym, name);
  CG_CHARBUF_OPEN_SYM(row_sym, name, "_row");
  CG_CHARBUF_OPEN_SYM(result_count_sym, name, "_result_count");
  CG_CHARBUF_OPEN_SYM(fetch_results_sym, name, "_fetch_results");


  // Emit foo_result_count, which is really just a proxy to cql_result_set_get_count,
  // but it is hiding the cql_result_set implementation detail from the API of the generated
  // code by providing a proc-scoped function for it with the typedef for the result set.

  // Skip generating fetch result function for extension and fragments since they always get
  // results fetched through the assembly query
  if (frag_type != FRAG_TYPE_EXTENSION && frag_type != FRAG_TYPE_BASE) {
    if (uses_out) {
      // Emit foo_fetch_results, it has the same signature as foo only with a result set
      // instead of a statement.

      bprintf(d, "\n");
      cg_lua_emit_fetch_results_prototype(dml_proc, params, name, result_set_name, d);

      bprintf(d, "  local result_set = nil\n");

      CHARBUF_OPEN(args);
      CHARBUF_OPEN(returns);

      // optional db arg and return code
      if (dml_proc) {
        bprintf(d, "  local _rc_\n");
        bprintf(&args, "_db_");
        bprintf(&returns, "_rc_, _result_");
      }
      else {
        bprintf(&returns, "_result_");
      }

      if (params) {
        cg_lua_params(params, &args, &returns);
      }

      bprintf(d, "  %s = %s(%s)\n", returns.ptr, proc_sym.ptr, args.ptr);
      bprintf(d, "  ");
      if (dml_proc) {
        cg_lua_error_on_not_sqlite_ok();
      }

      bprintf(d, "result_set = { _result_ }\n");

      bclear(&args);
      bclear(&returns);

      if (dml_proc) {
        bprintf(&returns, "_rc_, result_set");
      }
      else {
        bprintf(&returns, "result_set");
      }

      if (params) {
        cg_lua_params(params, &args, &returns);
      }

      bprintf(d, "\n::cql_cleanup::\n");
      bprintf(d, "  return %s\n", returns.ptr);
      bprintf(d, "end\n\n");

      CHARBUF_CLOSE(returns);
      CHARBUF_CLOSE(args);
    }
    else if (result_set_proc) {
      // Emit foo_fetch_results, it has the same signature as foo only with a result set
      // instead of a statement.
      Invariant(dml_proc);

      bprintf(d, "\n");
      cg_lua_emit_fetch_results_prototype(dml_proc, params, name, result_set_name, d);

      bprintf(d, "  local result_set = nil\n");
      bprintf(d, "  local _rc_\n");

      CHARBUF_OPEN(args);
      CHARBUF_OPEN(returns);

      // fixed db arg and return code
      bprintf(&args, "_db_");
      bprintf(&returns, "_rc_, stmt");

      if (params) {
        cg_lua_params(params, &args, &returns);
      }

      bprintf(d, "  %s = %s(%s)\n", returns.ptr, proc_sym.ptr, args.ptr);
      bprintf(d, "  ");
      cg_lua_error_on_not_sqlite_ok();

      bprintf(d, "  _rc_, result_set = cql_fetch_all_rows(stmt, ");
      cg_lua_emit_field_types(d, ast->sem->sptr);
      bprintf(d, ", ");
      cg_lua_emit_field_names(d, ast->sem->sptr);
      bprintf(d, ")\n");

      bclear(&args);
      bclear(&returns);

      bprintf(&returns, "_rc_, result_set");
      if (params) {
        cg_lua_params(params, &args, &returns);
      }

      bprintf(d, "\n::cql_cleanup::\n");
      bprintf(d, "  cql_finalize_stmt(stmt)\n");
      bprintf(d, "  stmt = nil\n");
      bprintf(d, "  return %s\n", returns.ptr);
      bprintf(d, "end\n\n");

      CHARBUF_CLOSE(returns);
      CHARBUF_CLOSE(args);
    }
  }

  CHARBUF_CLOSE(fetch_results_sym);
  CHARBUF_CLOSE(result_count_sym);
  CHARBUF_CLOSE(row_sym);
  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(getter_prefix);
  CHARBUF_CLOSE(temp);
  CHARBUF_CLOSE(result_set_create);
  CHARBUF_CLOSE(data_types);

  cg_main_output = main_saved;
}

// Main entry point for code-gen.  This will set up the buffers for the global
// variables and any loose calls or DML.  Any code that needs to run in the
// global scope will be added to the global_proc.  This is the only codegen
// error that is possible.  If you need global code and you don't have a global
// proc then you can't proceed.  Semantic analysis doƒesn't want to know that stuff.
// Otherwise all we do is set up the most general buffers for the global case and
// spit out a function with the correct name.
cql_noexport void cg_lua_main(ast_node *head) {
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  CSTR body_file_name = options.file_names[0];

  cg_lua_init();

  cg_lua_scratch_masks global_scratch_masks;
  cg_lua_current_masks = &global_scratch_masks;
  cg_lua_zero_masks(cg_lua_current_masks);

  CHARBUF_OPEN(body_file);
  CHARBUF_OPEN(indent);

  bprintf(&body_file, "%s", rt->source_prefix);
  bprintf(&body_file, "%s", rt->source_wrapper_begin);
  bprintf(&body_file, rt->cqlrt_template, rt->cqlrt);

  cg_lua_stmt_list(head);

  bprintf(&body_file, "%s", cg_fwd_ref_output->ptr);
  bprintf(&body_file, "%s", cg_constants_output->ptr);
  bprintf(&body_file, "%s", cg_declarations_output->ptr);

  // main function after constants and decls (if needed)

  bool_t global_proc_needed = cg_main_output->used > 1 || cg_scratch_vars_output->used > 1;

  if (global_proc_needed) {
    exit_on_no_global_proc();

    bindent(&indent, cg_scratch_vars_output, 2);
    bprintf(&body_file, "\nfunction %s(_db_)\n", global_proc_name);
    cg_lua_emit_rc_vars(&body_file);

    bprintf(&body_file, "%s", indent.ptr);
    bprintf(&body_file, "%s", cg_main_output->ptr);
    bprintf(&body_file, "\n");
    if (lua_error_target_used) {
      bprintf(&body_file, "::%s::\n", lua_error_target);
    }
    bprintf(&body_file, "%s", cg_cleanup_output->ptr);
    bprintf(&body_file, "  return _rc_\n");
    bprintf(&body_file, "end\n");
  }

  bprintf(&body_file, "%s", rt->source_wrapper_end);


  CHARBUF_CLOSE(indent);


  cql_write_file(body_file_name, body_file.ptr);

  CHARBUF_CLOSE(body_file);

  cg_lua_cleanup();
}

cql_noexport void cg_lua_init(void) {
  cg_lua_cleanup(); // reset globals/statics
  cg_common_init();

  Contract(!lua_error_target_used);

  LUA_DDL_STMT_INIT(drop_table_stmt);
  LUA_DDL_STMT_INIT(drop_view_stmt);
  LUA_DDL_STMT_INIT(drop_index_stmt);
  LUA_DDL_STMT_INIT(drop_trigger_stmt);
  LUA_DDL_STMT_INIT(create_table_stmt);
  LUA_DDL_STMT_INIT(create_virtual_table_stmt);
  LUA_DDL_STMT_INIT(create_trigger_stmt);
  LUA_DDL_STMT_INIT(create_index_stmt);
  LUA_DDL_STMT_INIT(create_view_stmt);
  LUA_DDL_STMT_INIT(alter_table_add_column_stmt);

  LUA_NO_OP_STMT_INIT(enforce_reset_stmt);
  LUA_NO_OP_STMT_INIT(enforce_normal_stmt);
  LUA_NO_OP_STMT_INIT(enforce_strict_stmt);
  LUA_NO_OP_STMT_INIT(enforce_push_stmt);
  LUA_NO_OP_STMT_INIT(enforce_pop_stmt);
  LUA_NO_OP_STMT_INIT(declare_schema_region_stmt);
  LUA_NO_OP_STMT_INIT(declare_deployable_region_stmt);
  LUA_NO_OP_STMT_INIT(begin_schema_region_stmt);
  LUA_NO_OP_STMT_INIT(end_schema_region_stmt);
  LUA_NO_OP_STMT_INIT(schema_upgrade_version_stmt);
  LUA_NO_OP_STMT_INIT(schema_upgrade_script_stmt);
  LUA_NO_OP_STMT_INIT(schema_ad_hoc_migration_stmt);
  LUA_NO_OP_STMT_INIT(declare_enum_stmt);
  LUA_NO_OP_STMT_INIT(declare_const_stmt);
  LUA_NO_OP_STMT_INIT(declare_named_type);
  LUA_NO_OP_STMT_INIT(declare_proc_no_check_stmt);
  LUA_NO_OP_STMT_INIT(schema_unsub_stmt);
  LUA_NO_OP_STMT_INIT(schema_resub_stmt);
  LUA_NO_OP_STMT_INIT(declare_group_stmt);
  LUA_NO_OP_STMT_INIT(declare_interface_stmt);
  LUA_NO_OP_STMT_INIT(emit_group_stmt);
  LUA_NO_OP_STMT_INIT(emit_enums_stmt);
  LUA_NO_OP_STMT_INIT(emit_constants_stmt);
  LUA_NO_OP_STMT_INIT(declare_select_func_no_check_stmt);
  LUA_NO_OP_STMT_INIT(declare_select_func_stmt);
  LUA_NO_OP_STMT_INIT(declare_func_stmt);
  LUA_NO_OP_STMT_INIT(declare_proc_stmt);

  LUA_STD_DML_STMT_INIT(begin_trans_stmt);
  LUA_STD_DML_STMT_INIT(commit_trans_stmt);
  LUA_STD_DML_STMT_INIT(rollback_trans_stmt);
  LUA_STD_DML_STMT_INIT(savepoint_stmt);
  LUA_STD_DML_STMT_INIT(release_savepoint_stmt);
  LUA_STD_DML_STMT_INIT(delete_stmt);
  LUA_STD_DML_STMT_INIT(with_delete_stmt);
  LUA_STD_DML_STMT_INIT(update_stmt);
  LUA_STD_DML_STMT_INIT(with_update_stmt);

  COMMON_STMT_INIT(blob_get_key_type_stmt);
  COMMON_STMT_INIT(blob_get_val_type_stmt);
  COMMON_STMT_INIT(blob_get_key_stmt);
  COMMON_STMT_INIT(blob_get_val_stmt);
  COMMON_STMT_INIT(blob_create_key_stmt);
  COMMON_STMT_INIT(blob_create_val_stmt);
  COMMON_STMT_INIT(blob_update_key_stmt);
  COMMON_STMT_INIT(blob_update_val_stmt);

  // insert forms have some special processing for the 'seed' case
  LUA_STMT_INIT(insert_stmt);
  LUA_STMT_INIT(with_insert_stmt);
  LUA_STMT_INIT(upsert_stmt);
  LUA_STMT_INIT(with_upsert_stmt);

  // these DML methods need to use prepare and have other processing other than just EXEC
  LUA_STMT_INIT(explain_stmt);
  LUA_STMT_INIT(select_stmt);
  LUA_STMT_INIT(with_select_stmt);

  LUA_STMT_INIT(if_stmt);
  LUA_STMT_INIT(switch_stmt);
  LUA_STMT_INIT(while_stmt);
  LUA_STMT_INIT(leave_stmt);
  LUA_STMT_INIT(continue_stmt);
  LUA_STMT_INIT(return_stmt);
  LUA_STMT_INIT(rollback_return_stmt);
  LUA_STMT_INIT(commit_return_stmt);
  LUA_STMT_INIT(call_stmt);
  LUA_STMT_INIT(declare_out_call_stmt);
  LUA_STMT_INIT(declare_vars_type);
  LUA_STMT_INIT(assign);
  LUA_STMT_INIT(let_stmt);
  LUA_STMT_INIT(set_from_cursor);
  LUA_STMT_INIT(create_proc_stmt);
  LUA_STMT_INIT(trycatch_stmt);
  LUA_STMT_INIT(proc_savepoint_stmt);
  LUA_STMT_INIT(throw_stmt);

  LUA_STMT_INIT(declare_cursor);
  LUA_STMT_INIT(declare_cursor_like_name);
  LUA_STMT_INIT(declare_cursor_like_select);
  LUA_STMT_INIT(declare_value_cursor);
  LUA_STMT_INIT(declare_cursor_like_typed_names);

  LUA_STMT_INIT(loop_stmt);
  LUA_STMT_INIT(fetch_stmt);
  LUA_STMT_INIT(fetch_values_stmt);
  LUA_STMT_INIT(set_blob_from_cursor_stmt);
  LUA_STMT_INIT(fetch_cursor_from_blob_stmt);
  LUA_STMT_INIT(update_cursor_stmt);
  LUA_STMT_INIT(fetch_call_stmt);

  LUA_STMT_INIT(close_stmt);
  LUA_STMT_INIT(out_stmt);
  LUA_STMT_INIT(out_union_stmt);
  LUA_STMT_INIT(echo_stmt);

  LUA_FUNC_INIT(sign);
  LUA_FUNC_INIT(abs);
  LUA_FUNC_INIT(sensitive);
  LUA_FUNC_INIT(nullable);
  LUA_FUNC_INIT(ifnull_throw);
  LUA_FUNC_INIT(ifnull_crash);
  LUA_FUNC_INIT(ifnull);
  LUA_FUNC_INIT(coalesce);
  LUA_FUNC_INIT(last_insert_rowid);
  LUA_FUNC_INIT(changes);
  LUA_FUNC_INIT(printf);
  LUA_FUNC_INIT(cql_get_blob_size);
  LUA_FUNC_INIT(cql_inferred_notnull);
  LUA_FUNC_INIT(cql_compressed);

  LUA_EXPR_INIT(num, cg_lua_expr_num, "num", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(str, cg_lua_expr_str, "STR", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(null, cg_lua_expr_null, "NULL", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(dot, cg_lua_expr_dot, "DOT", LUA_EXPR_PRI_ROOT);

  LUA_EXPR_INIT(lshift, cg_lua_binary, "<<", LUA_EXPR_PRI_SHIFT);
  LUA_EXPR_INIT(rshift, cg_lua_binary, ">>", LUA_EXPR_PRI_SHIFT);
  LUA_EXPR_INIT(bin_and, cg_lua_binary, "&", LUA_EXPR_PRI_BAND);
  LUA_EXPR_INIT(bin_or, cg_lua_binary, "|", LUA_EXPR_PRI_BOR);

  LUA_EXPR_INIT(mul, cg_lua_binary, "*", LUA_EXPR_PRI_MUL);
  LUA_EXPR_INIT(div, cg_lua_binary, "/", LUA_EXPR_PRI_MUL);
  LUA_EXPR_INIT(mod, cg_lua_binary, "%", LUA_EXPR_PRI_MUL);
  LUA_EXPR_INIT(add, cg_lua_binary, "+", LUA_EXPR_PRI_ADD);
  LUA_EXPR_INIT(sub, cg_lua_binary, "-", LUA_EXPR_PRI_ADD);
  LUA_EXPR_INIT(not, cg_lua_unary, "not", LUA_EXPR_PRI_UNARY);
  LUA_EXPR_INIT(tilde, cg_lua_unary, "~", LUA_EXPR_PRI_UNARY);
  LUA_EXPR_INIT(uminus, cg_lua_unary, "-", LUA_EXPR_PRI_UNARY);
  LUA_EXPR_INIT(eq, cg_lua_binary, "==", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(ne, cg_lua_binary, "~=", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(lt, cg_lua_binary, "<", LUA_EXPR_PRI_LT_GT);
  LUA_EXPR_INIT(gt, cg_lua_binary, ">", LUA_EXPR_PRI_LT_GT);
  LUA_EXPR_INIT(ge, cg_lua_binary, ">=", LUA_EXPR_PRI_LT_GT);
  LUA_EXPR_INIT(le, cg_lua_binary, "<=", LUA_EXPR_PRI_LT_GT);
  LUA_EXPR_INIT(call, cg_lua_expr_call, "CALL", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(between_rewrite, cg_lua_expr_between_rewrite, "BETWEEN", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(and, cg_lua_expr_and_or, "and", LUA_EXPR_PRI_LAND);
  LUA_EXPR_INIT(or, cg_lua_expr_and_or, "or", LUA_EXPR_PRI_LOR);
  LUA_EXPR_INIT(select_stmt, cg_lua_expr_select, "SELECT", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(select_if_nothing_expr, cg_lua_expr_select_if_nothing, "SELECT", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(select_if_nothing_throw_expr, cg_lua_expr_select_if_nothing_throw, "SELECT", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(select_if_nothing_or_null_expr, cg_lua_expr_select_if_nothing_or_null, "SELECT", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(with_select_stmt, cg_lua_expr_select, "WITH...SELECT", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(is, cg_lua_is_or_is_not, "==", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(is_not, cg_lua_is_or_is_not, "~=", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(is_not_true, cg_lua_expr_is_not_true, "IS NOT TRUE", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(is_not_false, cg_lua_expr_is_not_false, "IS NOT FALSE", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(is_true, cg_lua_expr_is_true, "IS TRUE", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(is_false, cg_lua_expr_is_false, "IS FALSE", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(like, cg_lua_binary, "like", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(not_like, cg_lua_binary, "not_like", LUA_EXPR_PRI_EQ_NE);
  LUA_EXPR_INIT(in_pred, cg_lua_expr_in_pred_or_not_in, "IN", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(not_in, cg_lua_expr_in_pred_or_not_in, "NOT IN", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(case_expr, cg_lua_expr_case, "CASE", LUA_EXPR_PRI_ROOT);
  LUA_EXPR_INIT(cast_expr, cg_lua_expr_cast, "CAST", LUA_EXPR_PRI_ROOT);
}

// To make sure we start at a zero state.  This is really necessary stuff
// because of the amalgam.  In the context of the amalgam the compiler
// might be run more than once without the process exiting. Hence we have
// to reset the globals and empty the symbol tables.
cql_noexport void cg_lua_cleanup() {
  cg_common_cleanup();

  SYMTAB_CLEANUP(lua_named_temporaries);

  base_fragment_name = NULL;
  lua_exports_output = NULL;
  lua_error_target = NULL;
  cg_lua_current_masks = NULL;

  lua_in_loop = false;
  lua_case_statement_count = 0;
  lua_catch_block_count = 0;
  lua_error_target = CQL_CLEANUP_DEFAULT_LABEL;
  lua_error_target_used = false;
  lua_rcthrown_current = CQL_LUA_RCTHROWN_DEFAULT;
  lua_rcthrown_used = false;
  lua_rcthrown_index = 0;
  lua_return_used = false;
  lua_seed_declared = false;
  lua_stack_level = 0;
  lua_temp_cstr_count = 0;
  lua_temp_statement_emitted = false;
  lua_continue_label_needed = false;
  lua_continue_label_number = 0;
  lua_continue_label_next = 0;
}

#endif

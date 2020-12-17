/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Perform codegen of the various nodes to "C".

#include "cg_c.h"

#include "ast.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "eval.h"
#include "symtab.h"
#include "encoders.h"
#include <stdio.h>

static void cg_expr(ast_node *node, charbuf *is_null, charbuf *value, int32_t pri);
static void cg_stmt_list(ast_node *node);
static void cg_get_column(sem_t sem_type, CSTR cursor, int32_t index, CSTR var, charbuf *output);
static void cg_binary(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new);
static void cg_store_same_type(charbuf *output, CSTR var, sem_t sem_type, CSTR is_null, CSTR value);
static void cg_store(charbuf *output, CSTR var, sem_t sem_type_var, sem_t sem_type_expr, CSTR is_null, CSTR value);
static void cg_call_stmt_with_cursor(ast_node *ast, CSTR cursor_name);
static void cg_proc_result_set(ast_node *ast);
static void cg_var_decl(charbuf *output, sem_t sem_type, CSTR base_name, bool_t is_local);
static void cg_emit_external_arglist(ast_node *expr_list, charbuf *prep, charbuf *invocation, charbuf *cleanup);
static void cg_call_named_external(CSTR name, ast_node *expr_list);
static void cg_user_func(ast_node *ast, charbuf *is_null, charbuf *value);
static void cg_c_init(void);
static void cg_copy(charbuf *output, CSTR var, sem_t sem_type_var, CSTR value);
static void cg_insert_dummy_spec(ast_node *ast);
static void cg_release_out_arg_before_call(sem_t sem_type_arg, sem_t sem_type_param, CSTR name);
static void cg_refs_offset(charbuf *output, sem_struct *sptr, CSTR offset_sym_name, CSTR struct_name);
static void cg_col_offsets(charbuf *output, sem_struct *sptr, CSTR sym_name, CSTR struct_name);

// Emits a sql statement with bound args.
static void cg_bound_sql_statement(CSTR stmt_name, ast_node *stmt, int32_t cg_exec);

// These globals represent the major state of the code-generator

// True if we are presently emitting a stored proc
static bool_t in_proc = 0;

// Keep record of the assembly query fragment for result set type reference if
// we are presently emitting an extension fragment stored proc
static CSTR parent_fragment_name;

// exports file if we are outputing exports
static charbuf *exports_output = NULL;

// The stack level, which faciliates safe re-use of scratch variables.
static int32_t stack_level = 0;

// Every string literal in a compiland gets a unique number.  This is it.
static int32_t string_literals_count = 0;

// Every output string fragment gets unique id
static int32_t fragment_count = 0;

// Case statements might need to generate a unique label for their "else" code
// We count the statements to make an easy label
static int32_t case_statement_count = 0;

// We need a local to hold the (const char *) conversion of a string reference
// when calling out to external code. This gives each such temporary a unique name.
static int32_t temp_cstr_count = 0;

// This tells us if we needed a temporary statement to do an exec or prepare
// with no visible statement result.  If we emitted the temporary we have to
// clean it up.  Examples of this set x := (select 1);   or  DELETE from foo;
static bool_t temp_statement_emitted = 0;

// This tells us if we have already emitted the declaration for the dummy data
// seed variable holder _seed_ in the current context.
static bool_t seed_declared;

// Each catch block needs a unique pair of lables, they are numbered.
static int32_t catch_block_count = 0;

// Used to give us a clue about when it might be smart to emit diagnostic
// output but otherwise uninteresting.  We increment this on ever nested block.
cql_data_defn( int32_t stmt_nesting_level );

#define CQL_CLEANUP_DEFAULT_LABEL "cql_cleanup"

// In the event of a failure of a sql block or a throw we need to emit
// a goto to the current cleanup target. This is it.  Try/catch manipulate this.
static CSTR error_target = CQL_CLEANUP_DEFAULT_LABEL;

// We set this to true when we have used the error target in the current context
// The current context is either the current procedure or the current try/catch block
// If this is true we need to emit the cleanup label.
static bool_t error_target_used = false;

// We set this to true if a "return" statement happened in a proc.  This also
// forces the top level "cql_cleanup" to be emitted.  We need a different flag for this
// because no matter how deeply nested we are "return" goes to the outermost error target.
// If this is set we will emit that top level target even if there were no other uses.
static bool_t return_used = false;

// We use this table to track named scratch variables that we might need
// this is used in cases where the name has to be computed and there may be several of them
static symtab *named_temporaries;

// String literals are frequently duplicated, we want a unique constant for each piece of text
static symtab *string_literals;

// Statement text fragments are frequently duplicated, we want a unique constant for each chunk of DML/DDL
static symtab *text_fragments;

// If the proc we are generating uses throw, we need to save the _rc_ in every catch block
static bool_t proc_uses_throw = false;

// See cg_find_best_line for more details on why this is what it is.
// All that's going on here is we recursively visit the tree and find the smallest
// line number that matches the given file in that branch.
static int32_t cg_find_first_line_recursive(ast_node *ast, CSTR filename) {
  int32_t line = INT32_MAX;
  int32_t lleft = INT32_MAX;
  int32_t lright = INT32_MAX;

  // file name is usually the same actual string but not always
  if (ast->filename == filename || !strcmp(filename, ast->filename)) {
   line = ast->lineno;
  }

  if (ast_has_left(ast)) {
   lleft = cg_find_first_line_recursive(ast->left, filename);
   if (lleft < line) line = lleft;
  }

  if (ast_has_right(ast)) {
   lright = cg_find_first_line_recursive(ast->right, filename);
   if (lright < line) line = lright;
  }

  return line;
}

// What's going on here is that the AST is generated on REDUCE operations.
// that means the line number at the time any AST node was generated is
// the largest line number anywhere in that AST.  But if we're looking for
// the line number for a statement we want the line number where it started.
// The way to get that is to recurse through the tree and choose the smallest
// line number anywhere in the tree.  But, we must only use line numbers
// from the same file as the one we ended on.  If (e.g.) a procedure spans files
// this will cause jumping around but that's not really avoidable.
static int32_t cg_find_first_line(ast_node *ast) {
  return cg_find_first_line_recursive(ast, ast->filename);
}

// emit the line directive, escape the file name using the C convention
static void cg_line_directive(CSTR filename, int32_t lineno, charbuf *output) {
  if (options.test || options.nolines) {
    return;
  }

  CHARBUF_OPEN(tmp);
  cg_encode_c_string_literal(filename, &tmp);
  bprintf(output, "# %d %s\n", lineno, tmp.ptr);
  CHARBUF_CLOSE(tmp);
}

// use the recursive search to emit the smallest line number in this subtree
static void cg_line_directive_min(ast_node *ast, charbuf *output) {
  int32_t lineno = cg_find_first_line(ast);
  cg_line_directive(ast->filename, lineno, output);
}

// The line number in the node is the last line number in the subtree because
// that is when the REDUCE operation happened when building the AST.
static void cg_line_directive_max(ast_node *ast, charbuf *output) {
  cg_line_directive(ast->filename, ast->lineno, output);
}

// The situation in CQL is that most statements, even single line statements,
// end up generating many lines of C.  So the normal situation is that you need
// to emit additional # directives to stay on the same line you were already on.
// In fact basically the situation is that you want to stay on your current line
// until the next time you see an explicit switch.  So what we do here is this:
//
// * when we see a # line directive  (e.g. # 32 "foo") we remember that line
// * if we are in a proc, emit the last such directive (just the line number) before each line
// * when we find an actual #line directive, we don't also emit the last known directive too
// * no additional outputs outside of procedures
// * we use the #define _PROC_ and #undef _PROC_ markers to know if we are in a proc
// 
// Typical output:
//
// # 1 "foo.sql"
// 
// /*
// CREATE PROC foo ()
// BEGIN
//   IF 1 THEN
//     CALL printf("one");
//   ELSE
//     CALL printf("two");
//   END IF;
// END;
// */
//
// #define _PROC_ "foo"
// # 1
// void foo(void) {
// # 3 "x"
//   if (1) {
// # 4 "x"
//     printf("one");
// # 4
//   }
// # 4
//   else {
// # 6 "x"
//     printf("two");
// # 6
//   }
// # 6
// 
// # 6
// }
// #undef _PROC_
//
static void cg_insert_line_directives(CSTR input, charbuf *output)
{
   CHARBUF_OPEN(last_line_directive);
   CHARBUF_OPEN(line);

   CSTR start_proc = "#define _PROC_ "; // this marks the start of a proc
   size_t start_proc_len = strlen(start_proc);

   CSTR end_proc = "#undef _PROC_";  // this marks the end of a proc
   size_t end_proc_len = strlen(end_proc);

   // true between the markers above (i.e. in the text of a procedure)
   bool_t now_in_proc = false;

   // true immediately after we see something like # 32 "foo.sql" in the input
   bool_t suppress_because_new_directive = false;  

   while (breadline(&line, &input)) {
      CSTR trim = line.ptr;
      while (*trim == ' ') trim++;

      if (!strncmp(trim, start_proc, start_proc_len)) {
        // entering a procedure, we will start to emit additional line directives to stay on the same line
        bprintf(output, "%s\n", line.ptr);
        now_in_proc = true;
        continue;
      }
      else if (!strncmp(trim, end_proc, end_proc_len)) {
        // leaving a procedure, we will no longer emit additional line directives to stay on the same line
        bprintf(output, "%s\n", line.ptr);
        now_in_proc = false;
        continue;
      }
      else  if (trim[0] == '#' && trim[1] == ' ') {
        bclear(&last_line_directive);
        bprintf(&last_line_directive, "%s", trim);
        bprintf(output, "%s\n", last_line_directive.ptr);
        char *next_space = strchr(last_line_directive.ptr + 2, ' ');
        if (next_space) *next_space = '\0';

        // this prevents us from emitting the sequence
        // # 32 "foo.sql"
        // # 32
        // the second # 32 would be a waste...
        suppress_because_new_directive = true;
        continue;
      }

      if (last_line_directive.ptr[0] && !suppress_because_new_directive && now_in_proc) {
        // this forces us to stay on the current line until we explicitly switch lines
        // every line becomes
        // # 32
        // [whatever]
        bprintf(output, "%s\n", last_line_directive.ptr);
      }

      suppress_because_new_directive = false;
      bprintf(output, "%s\n", line.ptr);
   }

  CHARBUF_CLOSE(line);
  CHARBUF_CLOSE(last_line_directive);
}

// return the symbol name for the string literal if there is one
static CSTR find_literal(CSTR str) {
  symtab_entry *entry = symtab_find(string_literals, str);
  return entry ? entry->val : NULL;
}

// the current proc name or null
static CSTR current_proc_name() {
  if (current_proc) {
    ast_node *proc_name_ast = get_proc_name(current_proc);
    EXTRACT_STRING(proc_name, proc_name_ast);
    return proc_name;
  }

  return NULL;
}

// generate an error if the given expression is true (note this drives tracing)
static void cg_error_on_expr(CSTR expr) {
  bprintf(cg_main_output, "if (%s) { cql_error_trace(); goto %s; }\n", expr, error_target);
  error_target_used = true;
}

// generate an error if the return code is not the required value (helper for common case)
static void cg_error_on_rc_notequal(CSTR required) {
  CHARBUF_OPEN(tmp);
  bprintf(&tmp, "_rc_ != %s", required);
  cg_error_on_expr(tmp.ptr);
  CHARBUF_CLOSE(tmp);
}

// generate an error if the return code is not SQLITE_OK (helper for common case)
static void cg_error_on_not_sqlite_ok() {
  cg_error_on_expr("_rc_ != SQLITE_OK");
}

// This tells us if a subtree should be wrapped in ()
// Basically we know the binding strength of the context (pri) and the current element (pri_new)
// Weaker contexts get parens.  Equal contexts get parens on the right side because all ops
// are left to right associtive in SQL. Stronger child contexts never need parens because
// the operator already binds tighter than its parent in the tree.
static bool_t needs_paren(ast_node *ast, int32_t pri_new, int32_t pri) {
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

// We have a series of masks to remember if we have emitted any given scratch variable.
// We might need several temporaries at the same level if different types appear
// at the same level but in practice we tend not to run into such things.  Mostly
// this works very well at arranging for the same scratch nullable int (or whatever)
// to be re-used in every statement.  The stack depth is limited to bundles of 64bits
//  with thisrepresentation. One bit for each stack level tracks if the temp has been
// generated.  This could be extended if needed...
typedef struct cg_type_masks {
  uint64_t reals[CQL_MAX_STACK/64];
  uint64_t bools[CQL_MAX_STACK/64];
  uint64_t ints[CQL_MAX_STACK/64];
  uint64_t longs[CQL_MAX_STACK/64];
  uint64_t strings[CQL_MAX_STACK/64];
  uint64_t objects[CQL_MAX_STACK/64];
  uint64_t blobs[CQL_MAX_STACK/64];
} cg_type_masks;

// There is one set of masks for nullables and another for not-nullables.
// The later doesn't get used very frequently...
typedef struct cg_scratch_masks {
  cg_type_masks nullables;
  cg_type_masks notnullables;
} cg_scratch_masks;

// Any new name context might need new temporaries, this points to the current
// context.  In practice it is set when we start processing a proc and it
// is cleared when we exit that proc.
static cg_scratch_masks *_Nullable cg_current_masks;

// just like it sounds
static void cg_zero_masks(cg_scratch_masks *_Nonnull masks) {
  memset(masks, 0, sizeof(*masks));
}

// When emitting a variable reference, you might want the full local variable
// helper function to record the assembly query fragment for result set type
// reference if we are presently emitting an extension fragment stored proc
static void cg_set_parent_fragment_name(CSTR _Nonnull name, ast_node *_Nonnull _misc_attr, void *_Nullable _context) {
  parent_fragment_name = name;
}

// emit the type decl for the reference and the typeid if needed (msys only)
static void cg_result_set_type_decl(charbuf *output, CSTR sym, CSTR ref) {
  bprintf(output, "cql_result_set_type_decl(%s, %s);\n", sym, ref);

  // if the result type needs extra type info, let it do so.
  rt->result_set_type_decl_extra && rt->result_set_type_decl_extra(output, sym, ref);
}

// When emitting a variable reference, you might want the full local variable
// defintion or just the declaration parts as they would appear in a prototype.
#define CG_VAR_DECL_PROTO 0
#define CG_VAR_DECL_LOCAL 1

static void cg_emit_local_init(charbuf *output, sem_t sem_type)
{
  sem_t core_type = core_type_of(sem_type);
  bool_t notnull = is_not_nullable(sem_type);
  switch (core_type) {
    case SEM_TYPE_INTEGER:
      if (notnull) {
        bprintf(output, " = 0");
      }
      break;

    case SEM_TYPE_TEXT:
      bprintf(output, " = NULL");
      break;

    case SEM_TYPE_BLOB:
      bprintf(output, " = NULL");
      break;

    case SEM_TYPE_OBJECT:
      bprintf(output, " = NULL");
      break;

    case SEM_TYPE_LONG_INTEGER:
      if (notnull) {
        bprintf(output, " = 0");
      }
      break;

    case SEM_TYPE_REAL:
      if (notnull) {
        bprintf(output, " = 0");
      }
      break;

    case SEM_TYPE_BOOL:
      if (notnull) {
        bprintf(output, " = 0");
      }
      break;
   }
}

static void cg_emit_local_nullable_init(charbuf *output, CSTR name, sem_t sem_type) {
  sem_t core_type = core_type_of(sem_type);
  switch (core_type) {
    case SEM_TYPE_INTEGER:
    case SEM_TYPE_LONG_INTEGER:
    case SEM_TYPE_REAL:
    case SEM_TYPE_BOOL:
      if (in_proc) {
        bprintf(output, "cql_set_null(%s);\n", name);
      }
      break;

    case SEM_TYPE_TEXT:
    case SEM_TYPE_OBJECT:
    case SEM_TYPE_BLOB:
      break;
   }
}

// Emit a declaration for a local whose name is base_name and whose type
// is given by sem_type.   Is_local really only decides if we add ";\n" to
// the end of the output.  This lets us use the same helper for list of
// arg-prototypes as a list of declarations.
// The real "trick" here is:
//  * flags might say it's an output parameter in which case we declare a pointer
//  * flags might indicate nullable, in which case we need the struct version
//  * text is always a reference, nullable or no.  But if you make a text local
//    then we also gotta clean it up.
static void cg_var_decl(charbuf *output, sem_t sem_type, CSTR base_name, bool_t is_local) {
  Contract(is_unitary(sem_type));
  Contract(!is_null_type(sem_type));
  Contract(cg_main_output);

  sem_t core_type = core_type_of(sem_type);
  bool_t notnull = is_not_nullable(sem_type);

  CHARBUF_OPEN(name);
  if (is_out_parameter(sem_type)) {
    bprintf(&name, "*_Nonnull ");
  }
  bprintf(&name, "%s", base_name);

  switch (core_type) {
    case SEM_TYPE_INTEGER:
      if (notnull) {
        bprintf(output, "%s %s", rt->cql_int32, name.ptr);
      }
      else {
        bprintf(output, "cql_nullable_int32 %s", name.ptr);
      }
      break;

    case SEM_TYPE_TEXT:
      bprintf(output, "%s ", rt->cql_string_ref);
      if (!is_local) {
        if (notnull) {
          bprintf(output, "_Nonnull ");
        } else {
          bprintf(output, "_Nullable ");
        }
      }
      bprintf(output, "%s", name.ptr);
      if (is_local) {
        bprintf(cg_cleanup_output, "  %s(%s);\n", rt->cql_string_release, name.ptr);
      }
      break;

    case SEM_TYPE_BLOB:
      bprintf(output, "%s ", rt->cql_blob_ref);
      if (!is_local) {
        if (notnull) {
          bprintf(output, "_Nonnull ");
        } else {
          bprintf(output, "_Nullable ");
        }
      }
      bprintf(output, "%s", name.ptr);
      if (is_local) {
        bprintf(cg_cleanup_output, "  %s(%s);\n", rt->cql_blob_release, name.ptr);
      }
      break;

  case SEM_TYPE_OBJECT:
      bprintf(output, "%s ", rt->cql_object_ref);
      if (!is_local) {
        if (notnull) {
          bprintf(output, "_Nonnull ");
        } else {
          bprintf(output, "_Nullable ");
        }
      }
      bprintf(output, "%s", name.ptr);
      if (is_local) {
        bprintf(cg_cleanup_output, "  %s(%s);\n", rt->cql_object_release, name.ptr);
      }
      break;

    case SEM_TYPE_LONG_INTEGER:
      if (notnull) {
        bprintf(output, "%s %s", rt->cql_int64, name.ptr);
      }
      else {
        bprintf(output, "cql_nullable_int64 %s", name.ptr);
      }
      break;

    case SEM_TYPE_REAL:
      if (notnull) {
        bprintf(output, "%s %s", rt->cql_double, name.ptr);
      }
      else {
        bprintf(output, "cql_nullable_double %s", name.ptr);
      }
      break;

    case SEM_TYPE_BOOL:
      if (notnull) {
        bprintf(output, "%s %s", rt->cql_bool, name.ptr);
      }
      else {
        bprintf(output, "cql_nullable_bool %s", name.ptr);
      }
      break;
  }

  if (is_local) {
    cg_emit_local_init(output, sem_type);
    bprintf(output, ";\n");
    if (!notnull) {
      cg_emit_local_nullable_init(output, name.ptr, sem_type);
    }
  }
  CHARBUF_CLOSE(name);
}

static bool_t is_assignment_target_reusable(ast_node *ast, sem_t sem_type) {
  if (ast && ast->parent && is_ast_assign(ast->parent)) {
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
// then a declaration is added to the scratch_vars section.  This is one of the root
// ways of getting an .is_null and .value back.  Note that not null variables always
// have a .is_null of "0" which becomes important when deciding how to assign
// one result to another.  Everything stays uniform.
static void cg_scratch_var(ast_node *ast, sem_t sem_type, charbuf *var, charbuf *is_null, charbuf *value) {
  Contract(is_unitary(sem_type));
  Contract(!is_null_type(sem_type));

  sem_t core_type = core_type_of(sem_type);
  sem_type &= (SEM_TYPE_CORE | SEM_TYPE_NOTNULL);

  Contract(stack_level < CQL_MAX_STACK);

  if (is_assignment_target_reusable(ast, sem_type)) {
    Invariant(ast && ast->parent && ast->parent->left);
    EXTRACT_ANY_NOTNULL(name_ast, ast->parent->left);
    EXTRACT_STRING(name, name_ast);
    if (is_out_parameter(name_ast->sem->sem_type)) {
      bprintf(var, "*%s", name);
    }
    else {
      bprintf(var, "%s", name);
    }
  }
  else {
    CSTR prefix;

    cg_type_masks *pmask;
    if (is_nullable(sem_type)) {
      pmask = &cg_current_masks->nullables;
      prefix = "_tmp_n";
    }
    else {
      pmask = &cg_current_masks->notnullables;
      prefix = "_tmp";
    }

    uint64_t *usedmask;

    switch (core_type) {
      case SEM_TYPE_INTEGER:
        bprintf(var, "%s_int_%d", prefix, stack_level);
        usedmask = pmask->ints;
        break;
      case SEM_TYPE_BLOB:
        bprintf(var, "%s_blob_%d", prefix, stack_level);
        usedmask = pmask->blobs;
        break;
      case SEM_TYPE_OBJECT:
        bprintf(var, "%s_object_%d", prefix, stack_level);
        usedmask = pmask->objects;
        break;
      case SEM_TYPE_TEXT:
        bprintf(var, "%s_text_%d", prefix, stack_level);
        usedmask = pmask->strings;
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(var, "%s_int64_%d", prefix, stack_level);
        usedmask = pmask->longs;
        break;
      case SEM_TYPE_REAL:
        bprintf(var, "%s_double_%d", prefix, stack_level);
        usedmask = pmask->reals;
        break;
      case SEM_TYPE_BOOL:
        bprintf(var, "%s_bool_%d", prefix, stack_level);
        usedmask = pmask->bools;
        break;
    }

    int32_t index = stack_level/64;
    unsigned long long mask = 1LL << (stack_level % 64);

    // Emit scratch if needed.
    if (!(usedmask[index] & mask)) {
      cg_var_decl(cg_scratch_vars_output, sem_type, var->ptr, CG_VAR_DECL_LOCAL);
      usedmask[index] |= mask;
    }
  }

  if (is_null && value) {
    if (is_ref_type(sem_type)) {
      bprintf(is_null, "!%s", var->ptr);
      bprintf(value, "%s", var->ptr);
    }
    else if (is_not_nullable(sem_type)) {
      bprintf(is_null, "0");
      bprintf(value, "%s", var->ptr);
    }
    else {
      bprintf(is_null, "%s.is_null", var->ptr);
      bprintf(value, "%s.value", var->ptr);
    }
  }
}

// This helper deals with one of the most common situations we have a nullable
// result that has to go into the result variable "var" and it's coming from one
// or two nullable sources.  The standard combine rules are that if eiter is null
// the result must be null.  However, some might be known to be not-null at compile
// time.  This function generates a call to the best runtime helper.  By doing it here
// all of the callers do not have to know all of the cases.  Here "val" is some
// arithmetic or logical combination of the values.
static void cg_combine_nullables(charbuf *out, CSTR var, CSTR l_is_null, CSTR r_is_null, CSTR val) {
  // generate the optimal set expression for the target nullable

  if (!strcmp(l_is_null, "1") || !strcmp(r_is_null, "1")) {
    // either known to be null, result is null
    bprintf(out, "cql_set_null(%s);\n", var);
  }
  else if (!strcmp(l_is_null, "0") && !strcmp(r_is_null, "0")) {
    // both known to be not null
    bprintf(out, "cql_set_notnull(%s, %s);\n", var, val);
  }
  else if (!strcmp(l_is_null, "0")) {
    // left known to be not null, null if right is null
    // Note: the target of the assignment is only compatible with the source, not identical
    // So the macro here generates the assignment field by field which gives free conversions.
    bprintf(out, "cql_set_nullable(%s, %s, %s);\n", var, r_is_null, val);
  }
  else if (!strcmp(r_is_null, "0")) {
    // right known to be not null, null if left is null
    // Note: the target of the assignment is only compatible with the source, not identical
    // So the macro here generates the assignment field by field which gives free conversions.
    bprintf(out, "cql_set_nullable(%s, %s, %s);\n", var, l_is_null, val);
  }
  else {
    // either could be null
    bprintf(out, "cql_combine_nullables(%s, %s, %s, %s);\n", var, l_is_null, r_is_null, val);
  }
}

// We pass this on to the general combine handler, letting it think the second operand was not null.
// There is of course no second operand.  We do this because one of the args might be a null literal
// or known not null for some other reason in which case we can make vastly simpler code.  That logic
// is already done in the above in the general case.  This call probably ends up in
// cg_set_nullable_nontrivial after the nullchecks are done.
static void cg_set_nullable(charbuf *out, CSTR var, CSTR is_null, CSTR value) {
  cg_combine_nullables(out, var, is_null, "0", value);
}

// Set nullable output type to null.  The only trick here is that reference types
// need the ref counting stuff.
// NOTE: there are lots of cases where even not-nullable ref types have to be set to null.
// These correspond to cases where the value is known to be uninitialized and we need
// to get something sane in there so that there isn't junk.  Or likewise where the value
// must become uninitialized because it's logically not readable anymore (e.g. in a cursor with no data).
// We have to free the data... so we have to set it to null... This is a bit weird but the alternative
// is some not-null sentinel constant string like the empty string which seems worse...
static void cg_set_null(charbuf *output, CSTR name, sem_t sem_type) {
  if (is_blob(sem_type)) {
    bprintf(output, "cql_set_blob_ref(&%s, NULL);\n", name);
  }
  else if (is_object(sem_type)) {
    bprintf(output, "cql_set_object_ref(&%s, NULL);\n", name);
  }
  else if (is_text(sem_type)) {
    bprintf(output, "cql_set_string_ref(&%s, NULL);\n", name);
  }
  else if (is_nullable(sem_type)) {
    bprintf(output, "cql_set_null(%s);\n", name);
  }
}

// Once we've don't any type conversions for the basic types we can do pretty simple assignments
// The nullable non-reference types typically need of the helper macros unless it's an exact-type copy
// operation.  This function is used by cg_store near the finish line.
static void cg_copy(charbuf *output, CSTR var, sem_t sem_type_var, CSTR value) {
  if (is_text(sem_type_var)) {
    bprintf(output, "cql_set_string_ref(&%s, %s);\n", var, value);
  }
  else if (is_blob(sem_type_var)) {
    bprintf(output, "cql_set_blob_ref(&%s, %s);\n", var, value);
  }
  else if (is_object(sem_type_var)) {
    if (var[0] == '*') {
      // this is just to avoid weird looking &*foo in the output which happens
      // when the target is an output variable
      bprintf(output, "cql_set_object_ref(%s, %s);\n", var+1, value);
    }
    else {
      bprintf(output, "cql_set_object_ref(&%s, %s);\n", var, value);
    }
  }
  else {
    bprintf(output, "%s = %s;\n", var, value);
  }
}

// Functions are a little special in that they can return reference types that come
// with a +1 reference.  To handle those you do not want to upcount the target.
// We release whatever we're holding and then hammer it with the new value
// with no upcount using the +1 we were given.
static void cg_copy_for_create(charbuf *output, CSTR var, sem_t sem_type_var, CSTR value) {
  if (is_text(sem_type_var)) {
    bprintf(cg_main_output, "%s(%s);\n", rt->cql_string_release, var);
  }
  else if (is_blob(sem_type_var)) {
    bprintf(output, "%s(%s);\n", rt->cql_blob_release, var);
  }
  else if (is_object(sem_type_var)) {
    bprintf(output, "%s(%s);\n", rt->cql_object_release, var);
  }
  bprintf(output, "%s = %s;\n", var, value);
}

// This is most general store function.  Given the type of the destination and the type of the source
// plus the is_null and value of the source it generates the correct operation to set it.
// * if storing to a boolean from non-boolean first normalize the result to a 0 or 1
// * for text, emit cql_set_string_ref to do the job
// * for nullables use cg_set_nullable (see above) to do the job
// * for not-nullables x = y is all you need.
static void cg_store(charbuf *output, CSTR var, sem_t sem_type_var, sem_t sem_type_expr, CSTR is_null, CSTR value) {
  CHARBUF_OPEN(adjusted_value);
  CG_BEGIN_ADJUST_FOR_OUTARG(var, sem_type_var);

  // Most types convert correctly with no help, the C compiler will convert.  This is not true for bool.
  if (is_bool(sem_type_var) && !is_bool(sem_type_expr)) {
    // exclude some things that are already normalized
    if (strcmp("0", value) && strcmp("1", value) && value[0] != '!') {
      bprintf(&adjusted_value, "!!(%s)", value);
      value = adjusted_value.ptr;
    }
  }

  bool_t handled = 0;

  if (!is_nullable(sem_type_var) || is_ref_type(sem_type_var)) {
    // dead store -- source = target
    handled = !strcmp(var, value);
  }
  else {
    CHARBUF_OPEN(val);
    CHARBUF_OPEN(nul);
    bprintf(&val, "%s.value", var);
    bprintf(&nul, "%s.is_null", var);
    handled = !strcmp(val.ptr, value) && !strcmp(nul.ptr, is_null);
    CHARBUF_CLOSE(nul);
    CHARBUF_CLOSE(val);
  }

  if (handled) {
    // nothing left to do
  }
  else if (is_ref_type(sem_type_var) || !is_nullable(sem_type_var)) {
    cg_copy(output, var, sem_type_var, value);
  }
  else {
    cg_set_nullable(output, var, is_null, value);
  }

  CG_END_ADJUST_FOR_OUTARG();
  CHARBUF_CLOSE(adjusted_value);
}

// This is a simple helper for store where we know that the type of the thing being stored
// is exactly the same as the type of the thing we are storing.  This is used when we
// just made a temporary of exactly the correct type to hold an expression.  cg_store
// handles this all but this helper lets you specify only one type.
static void cg_store_same_type(charbuf *output, CSTR var, sem_t sem_type, CSTR is_null, CSTR value) {
  cg_store(output, var, sem_type, sem_type, is_null, value);
}

// This is the general helper for some kind of comparison.  In fact comparison for
// the numeric types is exactly like all the other binary operators so really this is
// here only to special case the code gen for text comparison.  The passed in "op" is
// the operator between the left and right, so "<=", "<", etc.  String comparisons
// use the cql_string_compare helper to do the compare and the comparison is changed to be
// relative to zero.  So x <= y turns into cql_string_compare(x, y) <= 0.  If the arguments
// are not nullable, the comparison expression goes directly into value.  If the
// the result is nullable, it goes into a scratch var using the "combine" rules, see above.
static void cg_binary_compare(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_left = ast->left->sem->sem_type;
  sem_t sem_type_right = ast->right->sem->sem_type;

  if (!is_text(sem_type_left) && !is_text(sem_type_right)) {
   // for numeric, the usual binary processing works
   cg_binary(ast, op, is_null, value, pri, pri_new);
   return;
  }

  // Both sides are text or null; already verified compatible in semantic phase.

  CHARBUF_OPEN(comparison);

  if (needs_paren(ast, pri_new, pri)) {
    bprintf(&comparison, "(");
  }

  ast_node *l = ast->left;
  ast_node *r = ast->right;

  CG_RESERVE_RESULT_VAR(ast, sem_type_result);
  CG_PUSH_EVAL(l, pri_new);
  CG_PUSH_EVAL(r, pri_new);

  if (is_ast_like(ast)) {
    bprintf(&comparison, "%s(%s, %s) == 0", rt->cql_string_like, l_value.ptr, r_value.ptr);
  }
  else if (is_ast_not_like(ast)) {
    bprintf(&comparison, "%s(%s, %s) != 0", rt->cql_string_like, l_value.ptr, r_value.ptr);
  }
  else {
    bprintf(&comparison, "%s(%s, %s) %s 0", rt->cql_string_compare, l_value.ptr, r_value.ptr, op);
  }

  if (needs_paren(ast, pri_new, pri)) {
    bprintf(&comparison, ")");
  }

  if (is_not_nullable(sem_type_left) && is_not_nullable(sem_type_right)) {
    bprintf(value, "%s", comparison.ptr);
    bprintf(is_null, "0");
  }
  else {
    CG_USE_RESULT_VAR();
    cg_combine_nullables(cg_main_output, result_var.ptr, l_is_null.ptr, r_is_null.ptr, comparison.ptr);
  }

  CG_POP_EVAL(r);
  CG_POP_EVAL(l);
  CG_CLEANUP_RESULT_VAR();

  CHARBUF_CLOSE(comparison);
}

// Other than string comparison, all the normal (no short-circuit) binary operators
// can be handled the same way.
//   * op is the operator text
//   * is_null and value are the usual outputs
//   * pri is the strength of the caller
//   * pri_new is the strength of "op"
// The helper needs_paren() tells us if we should wrap this subtree in parens (see above)
// If the inputs are not nullable then we can make the easy case of returning the
// result in the value string (and 0 for is null).  Otherwise, cg_combine_nullables
// does the job.
static void cg_binary(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  // left op right

  ast_node *l = ast->left;
  ast_node *r = ast->right;

  sem_t sem_type_result = ast->sem->sem_type;

  if (sem_type_result == SEM_TYPE_NULL) {
    bprintf(value, "0");
    bprintf(is_null, "1");
    return;
  }

  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  // this hold the formula for the answer
  CHARBUF_OPEN(result);

  CG_RESERVE_RESULT_VAR(ast, sem_type_result);
  CG_PUSH_EVAL(l, pri_new);
  CG_PUSH_EVAL(r, pri_new);

  bprintf(&result, "%s %s %s", l_value.ptr, op, r_value.ptr);

  if (is_not_nullable(sem_type_left) && is_not_nullable(sem_type_right)) {
    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(%s)", result.ptr);
    }
    else {
      bprintf(value, "%s", result.ptr);
    }
    bprintf(is_null, "0");
  }
  else {
    // put result into result_var
    CG_USE_RESULT_VAR();
    cg_combine_nullables(cg_main_output, result_var.ptr, l_is_null.ptr, r_is_null.ptr, result.ptr);
  }

  CG_POP_EVAL(r);
  CG_POP_EVAL(l);
  CG_CLEANUP_RESULT_VAR();

  CHARBUF_CLOSE(result);
}

// The code-gen for is_null is one of the easiest.  The recursive call
// produces is_null as one of the outputs.  Use that.  Our is_null result
// is always zero because IS NULL is never, itself, null.
static void cg_expr_is_null(ast_node *expr, charbuf *is_null, charbuf *value) {
  sem_t sem_type_expr = expr->sem->sem_type;

  // expr IS NULL
  bprintf(is_null, "0"); // the result of is null is never null

  // The fact that this is not constant not null for not null reference types reflects
  // the weird state of affairs with uninitualized reference variables which
  // must be null even if they are typed not null.

  if (is_not_nullable(sem_type_expr) && !is_ref_type(sem_type_expr)) {
    // Note, sql has no side-effects so we can fold this away.
    bprintf(value, "0");
  }
  else {
    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    bprintf(value, "%s", expr_is_null.ptr);
    CG_POP_EVAL(expr);
  }
}

// The code-gen for is_not_null is one of the easiest.  The recursive call
// produces is_null as one of the outputs.  Invert that.  Our is_null result
// is always zero because IS NOT NULL is never, itself, null.
static void cg_expr_is_not_null(ast_node *expr, charbuf *is_null, charbuf *value) {
  sem_t sem_type_expr = expr->sem->sem_type;

  // expr IS NOT NULL
  bprintf(is_null, "0"); // the result of is not null is never null

  // The fact that this is not constant not null for not null reference types reflects
  // the weird state of affairs with uninitialized reference variables which
  // must be null even if they are typed not null.

  if (is_not_nullable(sem_type_expr) && !is_ref_type(sem_type_expr)) {
    // Note, sql has no side-effects so we can fold this away.
    bprintf(value, "1");
  }
  else {
    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    bprintf(value, "!%s", expr_is_null.ptr);
    CG_POP_EVAL(expr);
  }
}

// This is the general IS pattern, there are several case:
//  * if the right hand side is NULL then use the special helper for IS NULL
//    * that helper makes better code for this special case (the 99% case)
//  * if the args are text, use the text comparor runtime helper
//  * if the args are non-nullable or reference types (not text) equality works
//  * otherwise use the messy formula
//    * both are either null or both not null AND
//    * either they are null or their values are equal
static void cg_expr_is(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is(ast));
  EXTRACT_ANY_NOTNULL(l, ast->left);
  EXTRACT_ANY_NOTNULL(r, ast->right);

  // left IS right

  if (is_ast_null(l)) {
    cg_expr_is_null(r, is_null, value);
    return;
  } else if (is_ast_null(r)) {
    cg_expr_is_null(l, is_null, value);
    return;
  }

  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  // the resut of IS, will not be null, no cases.
  bprintf(is_null, "0");

  if (is_text(sem_type_left) || is_text(sem_type_right)) {
    // Both sides are text already verified compatible in semantic phase.

    CG_PUSH_EVAL(l, pri_new);
    CG_PUSH_EVAL(r, pri_new);

    bprintf(value, "%s(%s, %s)", rt->cql_string_equal, l_value.ptr, r_value.ptr, op);

    CG_POP_EVAL(r);
    CG_POP_EVAL(l);
    return;
  }

  CG_PUSH_EVAL(l, pri_new);
  CG_PUSH_EVAL(r, pri_new);

  bool_t refs = is_ref_type(sem_type_left) || is_ref_type(sem_type_right);
  bool_t notnull = is_not_nullable(sem_type_left) && is_not_nullable(sem_type_right);

  if (notnull || refs) {
    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(%s == %s)", l_value.ptr, r_value.ptr);
    }
    else {
      bprintf(value, "%s == %s", l_value.ptr, r_value.ptr);
    }
  }
  else {
    bprintf(value, "((%s == %s) && (%s || %s == %s))",
      l_is_null.ptr, r_is_null.ptr, r_is_null.ptr,
      l_value.ptr, r_value.ptr);
  }

  CG_POP_EVAL(r);
  CG_POP_EVAL(l);
}
// This is the general IS NOT pattern, there are several case:
//  * if the right hand side is NULL then use the special helper for IS NOT NULL
//    * that helper makes better code for this special case (the 99% case)
//  * if the args are text, use the text comparor runtime helper (with !)
//  * if the args are non-nullable or reference types (not text) inequality works
//  * otherwise use the messy IS formula (qv), but invert the result
static void cg_expr_is_not(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_is_not(ast));
  EXTRACT_ANY_NOTNULL(l, ast->left);
  EXTRACT_ANY_NOTNULL(r, ast->right);

  // left IS NOT right

  if (is_ast_null(r)) {
    cg_expr_is_not_null(l, is_null, value);
    return;
  } else if (is_ast_null(l)) {
    cg_expr_is_not_null(r, is_null, value);
    return;
  }

  sem_t sem_type_left = l->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  // the resut of IS NOT, will not be null, no cases.
  bprintf(is_null, "0");

  if (is_text(sem_type_left) || is_text(sem_type_right)) {
    // Both sides are text already verified compatible in semantic phase.

    CG_PUSH_EVAL(l, pri_new);
    CG_PUSH_EVAL(r, pri_new);

    bprintf(value, "!%s(%s, %s)", rt->cql_string_equal, l_value.ptr, r_value.ptr, op);

    CG_POP_EVAL(r);
    CG_POP_EVAL(l);
    return;
  }

  CG_PUSH_EVAL(l, pri_new);
  CG_PUSH_EVAL(r, pri_new);

  bprintf(is_null, "0");

  bool_t refs = is_ref_type(sem_type_left) || is_ref_type(sem_type_right);
  bool_t notnull = is_not_nullable(sem_type_left) && is_not_nullable(sem_type_right);

  if (notnull || refs) {
    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(%s != %s)", l_value.ptr, r_value.ptr);
    }
    else {
      bprintf(value, "%s != %s", l_value.ptr, r_value.ptr);
    }
  }
  else {
    bprintf(value, "!((%s == %s) && (%s || %s == %s))",
      l_is_null.ptr, r_is_null.ptr, r_is_null.ptr,
      l_value.ptr, r_value.ptr);
  }

  CG_POP_EVAL(r);
  CG_POP_EVAL(l);
}

// Helper to emit an "if false" condition
// There are lots of cases where the object is not nullable and we'd like nicer code
// for those cases, hence this helper.
static void cg_if_false(charbuf *output, CSTR is_null, CSTR value) {
  if (!strcmp(is_null, "0")) {
    bprintf(output, "if (!(%s)) {\n", value);
  }
  else if (!strcmp(is_null, "1")) {
    // null is not false
    bprintf(output, "if (0) {\n", value);
  }
  else {
   bprintf(output, "if (cql_is_nullable_false(%s, %s)) {\n", is_null, value);
  }
}

// Helper to emit an "if true" condition
// There are lots of cases where the object is not nullable and we'd like nicer code
// for those cases, hence this helper.
static void cg_if_true(charbuf *output, CSTR is_null, CSTR value) {
  if (!strcmp(is_null, "0")) {
    bprintf(output, "if (%s) {\n", value);
  }
  else if (!strcmp(is_null, "1")) {
    // null is not true
    bprintf(output, "if (0) {\n", value);
  }
  else {
   bprintf(output, "if (cql_is_nullable_true(%s, %s)) {\n", is_null, value);
  }
}

// The logical operations are fairly tricky, the code generators for
// each of them are very similar.  Basically x OR y has to be this:
//   * if x is true the answer is true and don't evaluate y (null is not true)
//   * if x is not true and y is true the answer is true (and both were evaluated)
//   * if neither is true and either is null the answer is null
//   * if both are false (only) the answer is false.  See the truth table.
// To get this code generation we need some if statements...  This is another
// of the cases where an expression-looking thing actually has statements in the C.
// There is easy case code if both are known to to be nullable, where the result
// is directly computed with ||.
static void cg_expr_or(ast_node *ast, CSTR str, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_or(ast));
  Contract(pri_new == C_EXPR_PRI_LOR);

  EXTRACT_ANY_NOTNULL(l, ast->left);
  EXTRACT_ANY_NOTNULL(r, ast->right);

  // [left] OR [right]

  // Logical OR truth table and short circuit rules:
  //
  // left right   result evalaute
  //----- -----   ------ --------
  // null null    null   both
  // null 0       null   both
  // null 1       1      both
  // 0    null    null   both
  // 0    0       0      both
  // 0    1       1      both
  // 1    null    1      left only
  // 1    0       1      left only
  // 1    1       1      left only

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  CG_RESERVE_RESULT_VAR(ast, sem_type_result);

  CHARBUF_OPEN(right_eval);

  charbuf *saved_main = cg_main_output;
  cg_main_output = &right_eval;
  CG_PUSH_EVAL(r, pri_new);
  cg_main_output = saved_main;

  // Easiest case of all, we can use the logical || operator
  // we can only do this if everything is non-null and no-statement generation is needed for the right expression
  // it's ok if statement generation is needed for the left because that never needs to short circuit (left
  // is always evaluated).
  if (!is_nullable(sem_type_result) && right_eval.used == 1) {
    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(");
    }

    CG_PUSH_EVAL(l, pri_new);

    bprintf(is_null, "0");
    bprintf(value, "%s || %s", l_value.ptr, r_value.ptr);

    CG_POP_EVAL(l);

    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, ")");
    }
  }
  else {
    // Possibly nullable result...
    CG_USE_RESULT_VAR();

    // More special cases, both null is just null.
    if (is_ast_null(l) && is_ast_null(r)) {
      bprintf(cg_main_output, "cql_set_null(%s);\n", result_var.ptr);
    }
    else {
      CG_PUSH_EVAL(l, C_EXPR_PRI_ROOT);
      cg_if_true(cg_main_output, l_is_null.ptr, l_value.ptr); // if (left...) {
      // if left is true the result is true and don't evaluate the right
      bprintf(cg_main_output, "  ");
      cg_store_same_type(cg_main_output, result_var.ptr, sem_type_result, "0", "1");
      bprintf(cg_main_output, "}\n");
      bprintf(cg_main_output, "else {\n");
      // Left is not true, it's null or false.  We need the right.
      // We already stored the statements right needs (if any).  Spit those out now.
      CG_PUSH_MAIN_INDENT(r, 2);
      bprintf(cg_main_output, "%s", right_eval.ptr);

      if (!is_nullable(sem_type_result)) {
        // If the result is not null then neither of the inputs are null
        // In this branch the left was not true, so it must have been false.
        // Therefore the result is whatever is on the right.  And it's not null.
        cg_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_right, "0", r_value.ptr);
      }
      else {
        // One was nullable so we have to do the nullable logic
        cg_if_true(cg_main_output, r_is_null.ptr, r_value.ptr); // if (right..) {
        bprintf(cg_main_output, "  ");
        // The right was true, the result is therefore true.
        cg_store_same_type(cg_main_output, result_var.ptr, sem_type_result, "0", "1");
        bprintf(cg_main_output, "}\n");
        bprintf(cg_main_output, "else {\n  ");
        // Neither was true, so the result is null or false.  Null if either are null.
        cg_combine_nullables(cg_main_output, result_var.ptr, l_is_null.ptr, r_is_null.ptr, "0");
        bprintf(cg_main_output, "}\n");
      }

      CG_POP_MAIN_INDENT(r);
      CG_POP_EVAL(l);

      bprintf(cg_main_output, "}\n");
    }
  }

  CG_POP_EVAL(r);
  CHARBUF_CLOSE(right_eval);
  CG_CLEANUP_RESULT_VAR();
}

// The logical operations are fairly tricky, the code generators for
// each of them are very similar.  Basically x AND y has to be this:
//   * if x is false the answer is false and don't evaluate y (null is not false)
//   * if x is not false and y is false the answer is false (and both were evaluated)
//   * if neither is false and either is null the answer is null
//   * if both are true (only) the answer is true.  See the truth table.
// To get this code generation we need some if statements...  This is another
// of the cases where an expression-looking thing actually has statements in the C.
// There is easy case code if both are known to to be nullable, where the result
// is directly computed with &&.
static void cg_expr_and(ast_node *ast, CSTR str, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_and(ast));
  Contract(pri_new == C_EXPR_PRI_LAND);

  EXTRACT_ANY_NOTNULL(l, ast->left);
  EXTRACT_ANY_NOTNULL(r, ast->right);

  // [left] AND [right]

  // Logical AND truth table and short circuit rules:
  //
  // left right   result evalaute
  //----- -----   ------ --------
  // null null    null   both
  // null 0       0      both
  // null 1       null   both
  // 0    null    0      left only
  // 0    0       0      left only
  // 0    1       0      left only
  // 1    null    null   both
  // 1    0       0      both
  // 1    1       1      both

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_right = r->sem->sem_type;

  CG_RESERVE_RESULT_VAR(ast, sem_type_result);

  CHARBUF_OPEN(right_eval);

  charbuf *saved_main = cg_main_output;
  cg_main_output = &right_eval;
  CG_PUSH_EVAL(r, pri_new);
  cg_main_output = saved_main;

  // Easiest case of all, we can use the logical && operator
  // we can only do this if everything is non-null and no-statement generation is needed for the right expression
  // it's ok if statement generation is needed for the left because that never needs to short circuit (left
  // is always evaluated).
  if (!is_nullable(sem_type_result) && right_eval.used == 1) {
    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, "(");
    }

    CG_PUSH_EVAL(l, pri_new);

    bprintf(is_null, "0");
    bprintf(value, "%s && %s", l_value.ptr, r_value.ptr);

    CG_POP_EVAL(l);

    if (needs_paren(ast, pri_new, pri)) {
      bprintf(value, ")");
    }
  }
  else {
    // We're doing the longish form, so there is a result variable for the answer.
    CG_USE_RESULT_VAR();

    // More special cases, both null is just null.
    if (is_ast_null(l) && is_ast_null(r)) {
      bprintf(cg_main_output, "cql_set_null(%s);\n", result_var.ptr);
    }
    else {
      CG_PUSH_EVAL(l, C_EXPR_PRI_ROOT);
      cg_if_false(cg_main_output, l_is_null.ptr, l_value.ptr); // if (!left...) {
      bprintf(cg_main_output, "  ");
      cg_store_same_type(cg_main_output, result_var.ptr, sem_type_result, "0", "0");
      bprintf(cg_main_output, "}\n");
      bprintf(cg_main_output, "else {\n");
      // Left is not false, it's null or true.  We need the right.
      // We already stored the statements right needs (if any).  Spit those out now.
      CG_PUSH_MAIN_INDENT(r, 2);
      bprintf(cg_main_output, "%s", right_eval.ptr);

      if (!is_nullable(sem_type_result)) {
        // If the result is not null then neither of the inputs are null
        // In this branch the left was not false, so it must have been true.
        // Therefore the result is whatever is on the right.  And it's not null.
        cg_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_right, "0", r_value.ptr);
      }
      else {
        // One was nullable so we have to do the nullable logic
        cg_if_false(cg_main_output, r_is_null.ptr, r_value.ptr); // if (!right..) {
        bprintf(cg_main_output, "  ");
        // The right is false, the result is therefore false.
        cg_store_same_type(cg_main_output, result_var.ptr, sem_type_result, "0", "0");
        bprintf(cg_main_output, "}\n");
        bprintf(cg_main_output, "else {\n  ");
        // Neither was false, so the result is null or true.  Null if either are null.
        cg_combine_nullables(cg_main_output, result_var.ptr, l_is_null.ptr, r_is_null.ptr, "1");
        bprintf(cg_main_output, "}\n");
      }

      CG_POP_MAIN_INDENT(r);
      CG_POP_EVAL(l);

      bprintf(cg_main_output, "}\n");
    }
  }

  CG_POP_EVAL(r);
  CHARBUF_CLOSE(right_eval);
  CG_CLEANUP_RESULT_VAR();
}

// The unary operators are handled just like the binary operators.  All of the
// C outputs have the form (op arg).  We just have to decide if we need parens.
// We use the same rules for parens here as in other places.  "pri" tells us
// the context of the caller, if it is stronger than our operator then we need parens.
// As usual, there is the easy case for not-nullables and the "use a temporary" case
// for nullables.
static void cg_unary(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  // op [left]

  EXTRACT_ANY_NOTNULL(expr, ast->left);
  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;

  CHARBUF_OPEN(result);
  CG_RESERVE_RESULT_VAR(ast, sem_type_result);
  CG_PUSH_EVAL(expr, pri_new)

  // UNARY is the highest... so we never need parens
  Invariant(pri_new >= pri);

  // We always add a space to avoid creating "--" or "++"
  // expr_value might be -1 or -x or some such.  This way we're
  // always safe at the cost of a space.
  bprintf(&result, "%s %s", op, expr_value.ptr);

  if (is_not_nullable(sem_type_expr)) {
    bprintf(is_null, "0");
    bprintf(value, "%s", result.ptr);
  }
  else {
    CG_USE_RESULT_VAR();
    cg_set_nullable(cg_main_output, result_var.ptr, expr_is_null.ptr, result.ptr);
  }

  CG_POP_EVAL(expr);
  CG_CLEANUP_RESULT_VAR();
  CHARBUF_CLOSE(result);
}

// This helper generates the tests for each entry in the IN list.
// we generate the appropriate equality test -- one for strings
// one for nullables and one for not nullables.  Note expr is already known
// to be not null here.  There was previous codegen for that case.  The result
// is either bool or nullable bool.
static void cg_in_or_not_in_expr_list(ast_node *head, CSTR expr, CSTR result, sem_t sem_type_result, bool_t is_not_in) {
  Contract(is_bool(sem_type_result));
  CSTR found_value = is_not_in ? "0" : "1";
  CSTR not_found_value = is_not_in ? "1" : "0";

  cg_store_same_type(cg_main_output, result, sem_type_result, "0", found_value);

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(in_expr, ast->left)

    // null can't ever match anything, waste of time.
    if (is_ast_null(in_expr)) {
      continue;
    }

    cg_line_directive_min(in_expr, cg_main_output);

    CG_PUSH_EVAL(in_expr, C_EXPR_PRI_EQ_NE);
    sem_t sem_type_in_expr = in_expr->sem->sem_type;

    if (is_text(sem_type_in_expr)) {
      bprintf(cg_main_output, "if (%s(%s, %s) == 0)", rt->cql_string_compare, expr, in_expr_value.ptr);
    }
    else if (is_nullable(sem_type_in_expr)) {
      bprintf(cg_main_output,
              "if (cql_is_nullable_true(%s, %s == %s))",
              in_expr_is_null.ptr,
              expr,
              in_expr_value.ptr);
    }
    else {
      bprintf(cg_main_output, "if (%s == %s)", expr, in_expr_value.ptr);
    }

    bprintf(cg_main_output, " break;\n");
    CG_POP_EVAL(in_expr);
  }

  cg_store_same_type(cg_main_output, result, sem_type_result, "0", not_found_value);
}

static void cg_null_result(ast_node *ast, charbuf *is_null, charbuf *value) {
  sem_t sem_type_result = ast->sem->sem_type;
  CG_SETUP_RESULT_VAR(ast, sem_type_result);
  cg_set_null(cg_main_output, result_var.ptr, sem_type_result);
  CG_CLEANUP_RESULT_VAR();
}

// The [NOT] IN structure is the simplest of the multi-test forms.
// It's actually a special case of case/when if you like.
// Each item in the [NOT] IN needs to be evaluated because there is no rule
// that says they are constants.
// NOT IN is just a similar reversed check compare IN starting with opposite result value.
// The general pattern for  X IN (U, V) looks like this
//
//  int result;
//  do {
//    prep statements for X;
//    temp = X;
//    if (temp is null) { result = null; break; } [only needed if X is nullable]
//
//    result = 1;  /* cg_in_or_not_in_expr_list generates the alternatives */
//    (result = 0; if NOT IN case)
//
//    prep statements for U;
//    compute U;
//    if (temp == U) break;
//
//    prep statements for V;
//    compute V;
//    if (temp == V) break;
//
//    result = 0;
//    (result = 1; if NOT IN case)
//   } while (0);
//
// The result ends up in the is_null and value fields as usual.
static void cg_expr_in_pred_or_not_in(
  ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_in_pred(ast) || is_ast_not_in(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left)
  EXTRACT_NOTNULL(expr_list, ast->right);

  // [expr] [NOT] IN ( [expr_list] )

  sem_t sem_type_result = ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;

  if (is_null_type(sem_type_expr)) {
    cg_null_result(ast, is_null, value);
    return;
  }

  // The answer will be stored in this scratch variable.
  // note: we do not allow the assignment variable to be used because it might be
  // in the candidate list. Since we write to it before we're done the early
  // "result = 1" would kill something like  r := x in (r, b);
  CG_SETUP_RESULT_VAR(NULL, sem_type_result);

  bprintf(cg_main_output, "do {\n");

  CG_PUSH_MAIN_INDENT(do, 2);

  cg_line_directive_min(expr, cg_main_output);

  // Evaluate the expression and stow it in a temporary.
  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
  CG_PUSH_TEMP(temp, sem_type_expr);

  // Copy the expression, we can't evaluate it more than once, so stow it.
  cg_store_same_type(cg_main_output, temp.ptr, sem_type_expr, expr_is_null.ptr, expr_value.ptr);

  // If the expression is null the result is null
  if (is_nullable(sem_type_expr)) {
    bprintf(cg_main_output, "if (%s) {    \n", temp_is_null.ptr);
    bprintf(cg_main_output, "  ");
    cg_set_null(cg_main_output, result_var.ptr, sem_type_result);
    bprintf(cg_main_output, "  break;\n");
    bprintf(cg_main_output, "}\n");
  }

  // Now generate the list
  cg_in_or_not_in_expr_list(expr_list, temp_value.ptr, result_var.ptr, sem_type_result, is_ast_not_in(ast));

  CG_POP_TEMP(temp);
  CG_POP_EVAL(expr);
  CG_POP_MAIN_INDENT(do);
  CG_CLEANUP_RESULT_VAR();

  cg_line_directive_max(ast, cg_main_output);
  bprintf(cg_main_output, "} while (0);\n");
}

// This helper method emits the alternatives for the case.  If there was an
// expression the temporary holding the expression is in expr.  Expr has
// already been tested for null if that was a possibility so we only need its
// value at this point.
static void cg_case_list(ast_node *head, CSTR expr, CSTR result, sem_t sem_type_result) {
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

    sem_t sem_type_case_expr = case_expr->sem->sem_type;
    sem_t sem_type_then_expr = then_expr->sem->sem_type;

    cg_line_directive_min(case_expr, cg_main_output);

    CG_PUSH_EVAL(case_expr, C_EXPR_PRI_EQ_NE);

    if (expr) {
      // Generate a comparison for the appropriate data type (expr known to be not null)
      if (is_text(sem_type_case_expr)) {
        bprintf(cg_main_output, "if (%s(%s, %s) == 0) {\n",
                rt->cql_string_compare,
                expr,
                case_expr_value.ptr);
      }
      else if (is_nullable(sem_type_case_expr)) {
        bprintf(cg_main_output, "if (cql_is_nullable_true(%s, %s == %s)) {\n",
                case_expr_is_null.ptr,
                expr,
                case_expr_value.ptr);
      }
      else {
        bprintf(cg_main_output, "if (%s == %s) {\n", expr, case_expr_value.ptr);
      }
    }
    else {
      // No temporary, generate a test for a boolean expression (which may or may not be null)
      if (is_nullable(sem_type_case_expr)) {
        bprintf(cg_main_output, "if (cql_is_nullable_true(%s, %s)) {\n",
                case_expr_is_null.ptr,
                case_expr_value.ptr);
      }
      else {
        bprintf(cg_main_output, "if (%s) {\n", case_expr_value.ptr);
      }
    }
    cg_line_directive_min(then_expr, cg_main_output);

    CG_PUSH_MAIN_INDENT(then, 2);
    CG_PUSH_EVAL(then_expr, C_EXPR_PRI_ROOT);

    cg_store(cg_main_output, result, sem_type_result, sem_type_then_expr, then_expr_is_null.ptr, then_expr_value.ptr);
    bprintf(cg_main_output, "break;\n");

    CG_POP_EVAL(then_expr);
    CG_POP_MAIN_INDENT(then);
    CG_POP_EVAL(case_expr);
    bprintf(cg_main_output, "}\n");
  }
}

// Case looks a lot like IN except the net result is computed at each step
// and the test is different at each step.  It's a straight generalization.
//
// Case X when U then R1 when V then R2 else R3 end;
//
//   declare result (whatever type holds R1, R2, and R3)
//
//   do {
//     statements to evaluate X;
//     temp = X;
//     [ if temp is null goto case_else; ] optional if temp is nullable
//
//     statements to evaluate U
//     if (temp == U) {
//       statements to evaluate R1;
//       result = R1;
//       break;
//     }
//
//     statements to evaluate V
//     if (temp == V) {
//       statements to evaluate R2;
//       result = R2;
//       break;
//     }
//   case_else:
//     statements to evaluate R3;
//     result = R3;
//   } while (0);
//
// If the X is omitted then U and V are normal boolean expressions and
// the code becomes if (U) etc  if (V) etc. with no temp.
static void cg_expr_case(ast_node *case_expr, CSTR str, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
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
  CG_SETUP_RESULT_VAR(case_expr, sem_type_result);

  cg_line_directive_min(case_expr, cg_main_output);
  bprintf(cg_main_output, "do {\n");

  CG_PUSH_MAIN_INDENT(do, 2);

  // if the form is case expr when ... then save the expr in a temporary
  if (expr) {
    cg_line_directive_min(expr, cg_main_output);

    sem_t sem_type_expr = expr->sem->sem_type;
    CG_PUSH_TEMP(temp, sem_type_expr);

    // Compute the value of the expression.
    CG_PUSH_EVAL(expr, C_EXPR_PRI_EQ_NE);

    // Store it in the temporary we just made, which has the exact correct type (we just made it)
    bprintf(cg_main_output, "  ");
    cg_store_same_type(cg_main_output, temp.ptr, sem_type_expr, expr_is_null.ptr, expr_value.ptr);

    // If the expression is null, then we go to the else logic.  Note: there is always else logic
    // either the user provides it or we do (to use null as the default).
    if (is_nullable(sem_type_expr)) {
      else_label_number = ++case_statement_count;
      bprintf(cg_main_output, "  if (%s) ", temp_is_null.ptr);
      bprintf(cg_main_output, "goto case_else_%d;\n", else_label_number);
    }

    cg_case_list(case_list, temp_value.ptr, result_var.ptr, sem_type_result);

    CG_POP_EVAL(expr);
    CG_POP_TEMP(temp);
  }
  else {
    // Otherwise do the case list with no expression...
    cg_case_list(case_list, NULL, result_var.ptr, sem_type_result);
  }

  if (else_label_number >= 0) {
    bprintf(cg_main_output, "case_else_%d:\n", else_label_number);
  }

  // If there is an else clause, spit out the result for that now.
  // Note that lack of an else is by-construction a nullable outcome because
  // the semantics of case say that if you miss all the cases you get null.
  if (else_expr) {
    cg_line_directive_min(else_expr, cg_main_output);

    sem_t sem_type_else = else_expr->sem->sem_type;

    CG_PUSH_EVAL(else_expr, C_EXPR_PRI_ROOT);

    cg_line_directive_max(case_expr, cg_main_output);
    cg_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_else, else_expr_is_null.ptr, else_expr_value.ptr);

    CG_POP_EVAL(else_expr);
  }
  else {
    // No else, result must be nullable. (enforced by cg_set_null)
    cg_line_directive_max(case_expr, cg_main_output);
    cg_set_null(cg_main_output, result_var.ptr, sem_type_result);
  }

  CG_POP_MAIN_INDENT(do);
  CG_CLEANUP_RESULT_VAR();

  bprintf(cg_main_output, "} while (0);\n");
}

static void cg_expr_cast(ast_node *cast_expr, CSTR str, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_cast_expr(cast_expr));

  sem_t sem_type_result = cast_expr->sem->sem_type;
  sem_t core_type_result = core_type_of(sem_type_result);

  ast_node *expr = cast_expr->left;
  sem_t core_type_expr = core_type_of(expr->sem->sem_type);

  CSTR type_text = NULL;
  CSTR bool_norm = "";

  if (core_type_expr == SEM_TYPE_BOOL) {
    // convert bool to 0/1
    bool_norm = "!!";
  }

  switch (core_type_result) {
    case SEM_TYPE_INTEGER:
      type_text = rt->cql_int32;
      break;

    case SEM_TYPE_LONG_INTEGER:
      type_text = rt->cql_int64;
      break;

    case SEM_TYPE_REAL:
      type_text = rt->cql_double;
      break;

    case SEM_TYPE_BOOL:
      // convert to 0/1 as part of conversion
      bool_norm = "!!";
      type_text = rt->cql_bool;
      break;
  }

  Invariant(type_text);  // all other types forbidden by semantic analysis

  CG_RESERVE_RESULT_VAR(cast_expr, sem_type_result);
  CG_PUSH_EVAL(expr, pri_new);
  CHARBUF_OPEN(result);

  bprintf(&result, "((%s)%s(%s))", type_text, bool_norm, expr_value.ptr);

  if (core_type_expr == core_type_result) {
    // no-op cast, just pass through
    bprintf(is_null, "%s", expr_is_null.ptr);
    bprintf(value, "%s", expr_value.ptr);
  }
  else if (is_not_nullable(sem_type_result)) {
    // simple cast, use the result with no temporary
    bprintf(value, "%s", result.ptr);
    bprintf(is_null, "0");
  }
  else {
    // nullable form, make a result variable and store
    CG_USE_RESULT_VAR();
    cg_set_nullable(cg_main_output, result_var.ptr, expr_is_null.ptr, result.ptr);
  }

  CHARBUF_CLOSE(result);
  CG_POP_EVAL(expr);
  CG_CLEANUP_RESULT_VAR();
}

// A CQL string literal needs to be stored somewhere so it looks like a string_ref.
// Here is a helper method for creating the name of the literal.  We use
// some letters from the text of the literal in the variable name to make it
// easier to find and recognize.
static bool_t cg_make_nice_literal_name(CSTR str, charbuf *output) {
  // empty buffer (just the null terminator)
  Contract(output->used == 1);

  CSTR existing_name = find_literal(str);
  if (existing_name) {
    bprintf(output, "%s", existing_name);
    return false;
  }

  bprintf(output, "_literal_%d", ++string_literals_count);
  bool_t underscore = 0;

  for (int32_t i = 0; str[i] && i < CQL_NICE_LITERAL_NAME_LIMIT; i++) {
    char ch = str[i];
    if (isalpha(ch)) {
      bputc(output, ch);
      underscore = 0;
    }
    else if (ch == '\\') {
     if (str[i+1]) i++;  // don't fall off the end
    }
    else if (!underscore) {
      bputc(output, '_');
      underscore = 1;
    }
  }

  if (current_proc) {
    EXTRACT_STRING(name, current_proc->left);
    bprintf(output, "%s", name);
  }

  symtab_add(string_literals, str, Strdup(output->ptr));
  return true;
}

// This converts from SQL string literal format to C literal format.
//  * the single quotes around the string become double quotes
//  * escaped single quote becomes just single quote
//  * backslash escapes are preserved
static void cg_requote_literal(CSTR str, charbuf *output) {
  CHARBUF_OPEN(plaintext);
  cg_decode_string_literal(str, &plaintext);
  cg_encode_c_string_literal(plaintext.ptr, output);
  CHARBUF_CLOSE(plaintext);
}

// Here we use the helper above to create a variable name for the literal
// then we declare that variable and emit the initializer.  The macro
// cql_string_literal does the job for us while allowing the different
// string implementations.  These go into the constants section.
static void cg_string_literal(CSTR str, charbuf *output) {
  Contract(str);
  Contract(str[0] == '\'');

  CHARBUF_OPEN(name);
  bool_t is_new = cg_make_nice_literal_name(str, &name);

  // Emit reference to a new shared string.
  bprintf(output, name.ptr);

  if (is_new) {
    // The shared string itself must live forever so it goes in global constants.
    bprintf(cg_constants_output, "%s(%s, ", rt->cql_string_literal, name.ptr);
    cg_requote_literal(str, cg_constants_output);
    bprintf(cg_constants_output, ");\n");
  }

  CHARBUF_CLOSE(name);
}

// The rewritten between expression is designed to be super easy to code gen.
// The semantic analyzer has already turned the between or not beween into a normal
// combination of and/or so all we have to do is load up the temporary with the test
// value and then evaluate the test expression. Between and not between look the same
// to the codgen (they will have different expressions).  This lets us get all that
// weird short circuit behavior super easy.  It's literally the AND/OR code running.
static void cg_expr_between_rewrite(
  ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_between_rewrite(ast));
  EXTRACT_NOTNULL(range, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT_STRING(var, range->left);
  EXTRACT_ANY_NOTNULL(test, range->right);

  // BETWEEN REWRITE [var := expr] CHECK [test]

  sem_t sem_type_var = expr->sem->sem_type;

  if (is_ast_null(expr)) {
    bprintf(is_null, "1");
    bprintf(value, "0");
    return;
  }

  cg_var_decl(cg_declarations_output, sem_type_var, var, CG_VAR_DECL_LOCAL);

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ASSIGN);
  cg_store_same_type(cg_main_output, var, sem_type_var, expr_is_null.ptr, expr_value.ptr);
  CG_POP_EVAL(expr);

  cg_expr(test, is_null, value, pri);
}

// This is the first of the key primitives in codegen -- it generates the
// output buffers for an identifier.  There are a few interesting cases.
//   * if it's an out variable we refer to it as *foo
//   * nullable strings use "id" for .value and "!id" for .is_null
//   * nullable other use the variables .is_null and .value
//   * non-nullables use the variable for the value and "0" for is_null
//
// Note: It's important to use the semantic name sem->name rather than the text
// of the ast because the user might refer case insensitively to the variable FoO
// and we need to emit the canonical name (e.g. foo, or Foo, or whatever it was).
static void cg_id(ast_node *expr, charbuf *is_null, charbuf *value) {
  sem_t sem_type = expr->sem->sem_type;
  Invariant(is_variable(sem_type));

  CHARBUF_OPEN(name_buff);
  CSTR name = expr->sem->name; // crucial

  if (is_out_parameter(sem_type)) {
    bprintf(&name_buff, "(*%s)", name);
    name = name_buff.ptr;
  }

  if (is_ref_type(sem_type)) {
    // Note that reference type identifiers can't be assumed to be not null
    // even if declared so, because they begin uninitialized.  Yes this is weird.
    // C has the same problem...
    bprintf(value, "%s", name);
    bprintf(is_null, "!%s", name);
  }
  else {
    if (is_nullable(sem_type)) {
      bprintf(value, "%s.value", name);
      bprintf(is_null, "%s.is_null", name);
    }
    else {
      bprintf(value, "%s", name);
      bprintf(is_null, "0", name);
    }
  }

  CHARBUF_CLOSE(name_buff);
}

// Recall that coalesce returns the first non-null arg from the list of arguments.
// The arguments must be type compatible, this was previously verified.  To do
// the codgen for coalesce(X,Y) we use a pattern like this:
//   declare result of the appropriate type;
//   do {
//     evaluate X;
//     if (x is not null) {
//       result = X;  // we can use the form where  X is known to be not null
//       break;       // we're done...
//     }
//     ... other cases just like the above...
//     ... the final case has no test, use it even if null
//     evaluate Y;
//     result = Y;
//   } while (0);
static void cg_func_coalesce(ast_node *call_ast, charbuf *is_null, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  // ifnull ( [arg_list] )
  // coalesce ( [arg_list] )

  sem_t sem_type_result = call_ast->sem->sem_type;

  // the answer will be stored in this scratch variable
  CG_SETUP_RESULT_VAR(call_ast, sem_type_result);

  cg_line_directive_min(call_ast, cg_main_output);
  bprintf(cg_main_output, "do {\n");
  CG_PUSH_MAIN_INDENT(do, 2);
  for (ast_node *ast = arg_list; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(expr, ast->left);

    sem_t sem_type_expr = expr->sem->sem_type;

    cg_line_directive_max(expr, cg_main_output);
    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);

    // Generate the test for all but the last choice.
    if (ast->right) {
      bprintf(cg_main_output, "if (!%s) {\n  ", expr_is_null.ptr);

      // We can generate the store for a known not null value
      // because we just tested for not null, cg_store will pick the best
      // assignment macro for that case based on type.
      bclear(&expr_is_null);
      bputc(&expr_is_null, '0');
      sem_type_expr |= SEM_TYPE_NOTNULL;
    }

    cg_store(cg_main_output, result_var.ptr, sem_type_result, sem_type_expr, expr_is_null.ptr, expr_value.ptr);

    if (ast->right) {
      bprintf(cg_main_output, "  break;\n");
      bprintf(cg_main_output, "}\n");
    }

    CG_POP_EVAL(expr);
  }
  CG_POP_MAIN_INDENT(do);
  bprintf(cg_main_output, "} while (0);\n");
  CG_CLEANUP_RESULT_VAR();
}

// Ifnull is an alias for coalesce, with only two args.
static void cg_func_ifnull(ast_node *call_ast, charbuf *is_null, charbuf *value) {
  cg_func_coalesce(call_ast, is_null, value);
}

static void cg_func_attest_notnull(ast_node *call_ast, charbuf *is_null, charbuf *value) {
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

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    bprintf(cg_main_output, "cql_invariant(!%s);\n", expr_is_null.ptr);
    bprintf(is_null, "0");
    bprintf(value, "%s", expr_value.ptr);
  CG_POP_EVAL(expr);
}

// There's a helper for this method, just call it.  Super easy.
static void cg_func_changes(ast_node *ast, charbuf *is_null, charbuf *value) {
  bprintf(is_null, "0");
  bprintf(value, "sqlite3_changes(_db_)");
}

// There's a helper for this method, just call it.  Super easy.
static void cg_func_last_insert_rowid(ast_node *ast, charbuf *is_null, charbuf *value) {
  bprintf(is_null, "0");
  bprintf(value, "sqlite3_last_insert_rowid(_db_)");
}

// Printf also has a helper, we just call it.  There are other helpers to emit
// a call to an external (not stored proc) function.  Use that.
static void cg_func_printf(ast_node *call_ast, charbuf *is_null, charbuf *value) {
  Contract(is_ast_call(call_ast));
  EXTRACT_ANY_NOTNULL(name_ast, call_ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, call_ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  CG_SETUP_RESULT_VAR(call_ast, SEM_TYPE_TEXT | SEM_TYPE_NOTNULL);
  bprintf(cg_main_output, "{\n");
  cg_call_named_external("  char *_printf_result = sqlite3_mprintf", arg_list);
  bprintf(cg_main_output, "  %s(%s);\n", rt->cql_string_release, result_var.ptr);
  bprintf(cg_main_output, "  %s = %s(_printf_result);\n", result_var.ptr, rt->cql_string_ref_new);
  bprintf(cg_main_output, "  sqlite3_free(_printf_result);\n");
  bprintf(cg_main_output, "}\n");
  CG_CLEANUP_RESULT_VAR();
}

static void cg_func_cql_get_blob_size(ast_node *ast, charbuf*is_null, charbuf *value) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  EXTRACT_ANY_NOTNULL(expr, arg_list->left);

  sem_t sem_type_var = name_ast->sem->sem_type;

  CG_RESERVE_RESULT_VAR(ast, sem_type_var);
  // Evaluate the expression and stow it in a temporary.
  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
  CHARBUF_OPEN(temp);

  // store cql_get_blob_size call in temp. e.g: cql_get_blob_size(expr_value)
  bprintf(&temp, "%s(%s)", rt->cql_get_blob_size, expr_value.ptr);

  if (is_not_nullable(sem_type_var)) {
    // The result is known to be not nullable therefore we can store directly the value to the result buff
    bprintf(is_null, "0");
    bprintf(value, "%s", temp.ptr);
  } else {
    CG_USE_RESULT_VAR();
    cg_store(cg_main_output, result_var.ptr, sem_type_var, sem_type_var, expr_is_null.ptr, temp.ptr);
  }

  CHARBUF_CLOSE(temp);
  CG_POP_EVAL(expr);
  CG_CLEANUP_RESULT_VAR();
}

// This is some kind of function call in an expression context.  Look up the method
// and call one of the cg_func_* workers above.  All arg combos are known to be good
// because semantic analysis verified them already.
static void cg_expr_call(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left)
  EXTRACT_STRING(name, name_ast);

  // name( [arg_list] )

  if (find_func(name) || find_proc(name)) {
    cg_user_func(ast, is_null, value);
  }
  else {
    symtab_entry *entry = symtab_find(cg_funcs, name);
    Invariant(entry);  // names have already been verified!
    ((void (*)(ast_node *, charbuf *, charbuf *))entry->val)(ast, is_null, value);
  }
}

// Numeric literal, spit it out.
static void cg_expr_num(ast_node *expr, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_num(expr));
  EXTRACT_NUM_TYPE(num_type, expr);
  EXTRACT_NUM_VALUE(lit, expr);
  // a numeric literal
  bprintf(is_null, "0");

  if (num_type == NUM_LONG) {
    // add long suffix if needed
    bprintf(value, "_64(%s)", lit);
  }
  else {
    bprintf(value, "%s", lit);
  }
}

static void cg_expr_str(ast_node *expr, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  // String could be an id, or a literal -- literals start with single quote.
  Contract(is_ast_str(expr));
  EXTRACT_STRING(str, expr);
  if (is_ast_strlit(expr)) {
    // Note str is the lexeme, so it is still quoted and escaped.
    cg_string_literal(str, value);
    bprintf(is_null, "0");
  }
  else {
    cg_id(expr, is_null, value);
  }
}

static void cg_expr_dot(ast_node *expr, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  // X.Y has a net local name computed by semantic analysis.  Use it like any other id.
  Contract(is_ast_dot(expr));
  cg_id(expr, is_null, value);
}

static void cg_expr_null(ast_node *expr, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_null(expr));
  // null literal
  bprintf(value, "NULL");
  bprintf(is_null, "1");
}

// This is the main entry point for codegen of an expression.  It dispatches
// to one of the above workers for all the complex types and handles a few primitives
// in place. See the introductory notes to understand is_null and value.
static void cg_expr(ast_node *expr, charbuf *is_null, charbuf *value, int32_t pri) {
  Contract(is_null);
  Contract(value);
  Contract(value->used == 1);  // just the null (i.e. empty buffer)
  Contract(is_null->used == 1); // just the null (i.e. empty buffer)

  int32_t stack_level_saved = stack_level;

  // These are all the expressions there are, we have to find it in this table
  // or else someone added a new expression type and it isn't supported yet.
  symtab_entry *entry = symtab_find(cg_exprs, expr->type);
  Invariant(entry);
  cg_expr_dispatch *disp = (cg_expr_dispatch*)entry->val;
  disp->func(expr, disp->str, is_null, value, pri, disp->pri_new);

  Invariant(stack_level == stack_level_saved);
}

// This is a nested select expression.  To evaluate we will
//  * prepare a temporary to hold the result
//  * generate the bound SQL statement
//  * extract the exactly one argument into the result variable
//    which is of exactly the right type
//  * use that variable as the result.
// The helper methods take care of sqlite error management.
static void cg_expr_select(ast_node *ast, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_select_stmt(ast));
  int32_t stack_level_saved = stack_level;

  // SELECT [select_opts] [select_expr_list_con]

  sem_t sem_type_result = ast->sem->sem_type;

  CG_SETUP_RESULT_VAR(ast, sem_type_result);

  cg_bound_sql_statement(NULL, ast, CG_PREPARE | CG_MINIFY_ALIASES);

  // exactly one column is allowed, already checked in semantic analysis, fetch it
  bprintf(cg_main_output, "_rc_ = sqlite3_step(_temp_stmt);\n");
  cg_error_on_rc_notequal("SQLITE_ROW");
  cg_get_column(sem_type_result, "_temp_stmt", 0, result_var.ptr, cg_main_output);
  bprintf(cg_main_output, "cql_finalize_stmt(&_temp_stmt);\n");

  CG_CLEANUP_RESULT_VAR();

  Invariant(stack_level == stack_level_saved);
}

// This is the elementary piece of the if-then construct, it's one condition
// and one statement list.  It can happen in the context of the top level
// if or any else-if.  The conditional generated requires either simple true
// for not nulls or nullable true (i.e. null is false, false is false).
static void cg_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(expr, ast->left);

  // [expr ast->left] THEN stmt_list

  sem_t sem_type_expr = expr->sem->sem_type;

  cg_line_directive_max(expr, cg_main_output);

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);

  if (is_ast_null(expr) || is_not_nullable(sem_type_expr)) {
    bprintf(cg_main_output, "if (%s) {\n", expr_value.ptr);
  }
  else {
    bprintf(cg_main_output, "if (cql_is_nullable_true(%s, %s)) {\n", expr_is_null.ptr, expr_value.ptr);
  }

  CG_POP_EVAL(expr);

  if (stmt_list) {
    cg_stmt_list(stmt_list);
    cg_line_directive_max(stmt_list, cg_main_output);
  }

  bprintf(cg_main_output, "}\n");
}

// Recursively emits the else-if chain.  These have to nest to allow for
// expressions to generate statements.
static void cg_elseif_list(ast_node *ast, ast_node *elsenode) {
  if (ast) {
    Contract(is_ast_elseif(ast));
    EXTRACT(cond_action, ast->left);

    // ELSE IF [cond_action]
    bprintf(cg_main_output, "else {\n");
      CG_PUSH_MAIN_INDENT(else, 2);
      cg_cond_action(cond_action);
      cg_elseif_list(ast->right, elsenode);
      CG_POP_MAIN_INDENT(else);
    bprintf(cg_main_output, "}\n");
  }
  else if (elsenode) {
    Contract(is_ast_else(elsenode));
    // ELSE [stmt_list]
    cg_line_directive_min(elsenode, cg_main_output);
    EXTRACT(stmt_list, elsenode->left);
    bprintf(cg_main_output, "else {\n");
    cg_stmt_list(stmt_list);
    bprintf(cg_main_output, "}\n");
  }
}

// As with the other cases the fact that expressions might require statements
// complicates the codegen. If there is an else-if (expression) that expression
// might itself require statements to compute the expression.  Even a logical AND
// might require statements if there is nullability involved.
// That means the overall pattern has to look like this, with nesting.
//
// prep statements;
//   result = final expression;
//   if (result) {
//     statements;
//   }
//   else {
//     prep statements;
//     result = final expression;
//     if (result) {
//       statements;
//     }
//     else {
//      statements;
//     }
//   }
static void cg_if_stmt(ast_node *ast) {
  Contract(is_ast_if_stmt(ast));

  EXTRACT(cond_action, ast->left);
  EXTRACT_NOTNULL(if_alt, ast->right);

  // IF [cond_action] [if_alt]
  cg_cond_action(cond_action);

  EXTRACT(elseif, if_alt->left);
  EXTRACT_NAMED(elsenode, else, if_alt->right);
  cg_elseif_list(elseif, elsenode);

  // END IF
}

// This code uses the same cg_store helper method to do an assignment as
// is used all over the place for assigning to scratch variables.  All
// we have to do here is pull the name and types out of the ast.
static void cg_assign(ast_node *ast) {
  Contract(is_ast_assign(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  CSTR name = name_ast->sem->name;  // crucial: use the canonical name not the specified name

  Contract(stack_level == 0);

  // SET [name] := [expr]

  sem_t sem_type_var = name_ast->sem->sem_type;
  sem_t sem_type_expr = expr->sem->sem_type;

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ASSIGN);
  cg_store(cg_main_output, name, sem_type_var, sem_type_expr, expr_is_null.ptr, expr_value.ptr);
  CG_POP_EVAL(expr);
}

// This is the processing for a single parameter in a stored proc declaration.
// All we have to do is emit the type signature of that parameter.  This is
// precisely what cg_var_decl with CG_VAR_DECL_PROTO is for...
static void cg_param(ast_node *ast, charbuf *decls, charbuf *body) {
  Contract(is_ast_param(ast));
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, param_detail->left)
  EXTRACT_STRING(name, name_ast);

  // [in out] name [datatype]

  sem_t sem_type = name_ast->sem->sem_type;

  cg_var_decl(decls, sem_type, name, CG_VAR_DECL_PROTO);

  // In a proc decl the out arg initialized to null, this avoids attempting
  // to release any incoming garbage value and ensures some sanity in the event
  // the the return code is ignored...  Nobody ignores return codes, right?
  if (body && is_out_parameter(sem_type) && !is_in_parameter(sem_type)) {
    if (is_ref_type(sem_type)) {
      bprintf(body, "  *(void **)%s = NULL; // set out arg to non-garbage\n", name);
    }
  }
}

// Walk all the params of a stored proc and emit each one with a comma where needed.
// cg_param does all the hard work.
static void cg_params(ast_node *ast, charbuf *decls, charbuf *body) {
  Contract(is_ast_params(ast));

  while (ast) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    cg_param(param, decls, body);

    if (ast->right) {
      bprintf(decls, ", ");
    }

    ast = ast->right;
  }
}

// This is used when we are making a call to a known stored proc.  This is usable
// only in limited cases where we have previously set up local variables whose
// names exactly match the formal names of the function we are attempting to call.
// In particular the function that generates a rowset knows how to call the corresponding
// function that generates a statement because their signatures exactly match.
static void cg_param_name(ast_node *ast, charbuf *output) {
  Contract(is_ast_param(ast));
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, param_detail->left)
  EXTRACT_STRING(name, name_ast);

  bprintf(output, "%s", name);
}

// This loops through the parameters and emits each one as part of a call
// where we have a local for each parameter.  See above.
static void cg_param_names(ast_node *ast, charbuf *output) {
  Contract(is_ast_params(ast));

  while (ast) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left);

    cg_param_name(param, output);

    if (ast->right) {
      bprintf(output, ", ");
    }

    ast = ast->right;
  }
}

// row types are emitted in a canonical order to make comparison, hashing
// and other operations easier.  For now that order is simply primitive
// types and then reference types.  This will become more strict over time
// as other areas take advantage of ordering to generate more compact code.
static void cg_fields_in_canonical_order(charbuf *output, sem_struct *sptr) {
  uint32_t count = sptr->count;

  // first primitive types
  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    if (is_ref_type(sem_type)) {
      continue;
    }
    CSTR col = sptr->names[i];
    bprintf(output, "  ");
    cg_var_decl(output, sem_type, col, CG_VAR_DECL_PROTO);
    bprintf(output, ";\n");
  }

  // then reference types
  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    if (!is_ref_type(sem_type)) {
      continue;
    }

    CSTR col = sptr->names[i];
    bprintf(output, "  ");
    cg_var_decl(output, sem_type, col, CG_VAR_DECL_PROTO);
    bprintf(output, ";\n");
  }
}

// This function generates a struct definition into the indicated output.
// The struct will be named for the current proc name and the given name
// at least one of which must be non-null.  This is used for the result
// type of procs with the OUT keyword and for automatic cursors.
// The struct includes the _has_row_ boolean plus the fields of the
// sem_struct provided.
static void cg_c_struct_for_sptr(charbuf *output, sem_struct *sptr, CSTR name) {
  Invariant(sptr);

  CSTR scope = current_proc_name();
  Contract(scope || name);  // no scope and no name makes no sense
  CSTR suffix = (name && scope) ? "_" : "";
  scope = scope ? scope : "";
  name = name ? name : "";

  CG_CHARBUF_OPEN_SYM(row_type, scope, suffix, name, "_row");
  bprintf(output, "\ntypedef struct %s {\n", row_type.ptr);

  // emit the two standard fields, _has_row_, _refs_count_, and _refs_offset_
  bprintf(output, "  cql_bool _has_row_;\n");
  bprintf(output, "  cql_uint16 _refs_count_;\n");
  bprintf(output, "  cql_uint16 _refs_offset_;\n");

  cg_fields_in_canonical_order(output, sptr);

  bprintf(output, "} %s;\n", row_type.ptr);

  CHARBUF_CLOSE(row_type);
}

static int32_t refs_count_sptr(sem_struct *sptr) {
  int32_t refs_count = 0;

  for (int32_t i = 0; i < sptr->count; i++) {
    if (is_ref_type(sptr->semtypes[i])) {
      refs_count++;
    }
  }

  return refs_count;
}

// Emit the offsets for the given struct type into the output with the given name
static void cg_col_offsets(charbuf *output, sem_struct *sptr, CSTR sym_name, CSTR struct_name) {
  uint32_t count = sptr->count;

  bprintf(output, "\nstatic cql_uint16 %s[] = { %d", sym_name, count);

  for (int32_t i = 0; i < sptr->count; i++) {
    bprintf(output, ",\n");
    bprintf(output, "  cql_offsetof(%s, %s)", struct_name,  sptr->names[i]);
  }

  bprintf(output, "\n};\n");
}

// Emit the offsets for the reference types in the given struct type into the output
static void cg_refs_offset(charbuf *output, sem_struct *sptr, CSTR offset_sym, CSTR struct_name) {
  int32_t refs_count = refs_count_sptr(sptr);

  bprintf(output, "\n#define %s ", offset_sym);

  for (int32_t i = 0; i < sptr->count; i++) {
    if (is_ref_type(sptr->semtypes[i])) {
      bprintf(output, "cql_offsetof(%s, %s) // count = %d\n", struct_name,  sptr->names[i], refs_count);
      break;
    }
  }
}

static void find_identity_columns_callback(CSTR _Nonnull name, ast_node *_Nonnull found_ast, void *_Nullable context) {
  Invariant(context);
  charbuf *output = (charbuf*)context;

  Invariant(current_proc);
  Invariant(current_proc->sem);
  Invariant(current_proc->sem->sptr);

  sem_struct *sptr = current_proc->sem->sptr;

  // already checked in semantic analysis
  int32_t col_index = sem_column_index(sptr, name);
  Invariant(col_index >= 0);

  bprintf(output, "  %d, // %s\n", col_index, name);
}

// Emit the array of identity columns (used by cql_rows_same to determine which columns identify the "same" record)
// Return 1 if any identity columns were found; otherwise 0
static bool_t cg_identity_columns(charbuf *output,
                                  CSTR proc_name,
                                  ast_node *_Nullable misc_attrs,
                                  CSTR identity_columns_sym) {
  if (!misc_attrs) {
    return false;
  }

  CHARBUF_OPEN(cols);
  uint32_t count = find_identity_columns(misc_attrs, &find_identity_columns_callback, &cols);
  if (count > 0) {
    bprintf(output, "\nstatic cql_uint16 %s[] = { %d,\n%s};\n", identity_columns_sym, count, cols.ptr);
  }
  CHARBUF_CLOSE(cols);
  return count > 0;
}

// Emit the teardown information for use in in a rowset row or cursor row
// this is just the count of references and the offset of the first reference
// in the row structure. Since the references are stored together and are
// at the end of the row you can alway clean up a row using just the offset
// and the count.
static void cg_struct_teardown_info(charbuf *output, sem_struct *sptr, CSTR name) {
  Invariant(sptr);

  int32_t refs_count = refs_count_sptr(sptr);
  if (!refs_count) {
    return;
  }

  CSTR scope = current_proc_name();
  Contract(scope || name);  // no scope and no name makes no sense
  CSTR suffix = (name && scope) ? "_" : "";
  scope = scope ? scope : "";
  name = name ? name : "";

  CG_CHARBUF_OPEN_SYM(count_sym, scope, suffix, name, "_refs_count");
  CG_CHARBUF_OPEN_SYM(offset_sym, scope, suffix, name, "_refs_offset");
  CG_CHARBUF_OPEN_SYM(row_type, scope, suffix, name, "_row");

  cg_refs_offset(output, sptr, offset_sym.ptr, row_type.ptr);

  CHARBUF_CLOSE(row_type);
  CHARBUF_CLOSE(offset_sym);
  CHARBUF_CLOSE(count_sym);
}

// Emit the return code variables for the procedure
// if the procedure uses throw then it needs the saved RC as well so we can re-throw it
void cg_emit_rc_vars(charbuf *output) {
  bprintf(output, "  %s _rc_ = SQLITE_OK;\n", rt->cql_code);
  if (proc_uses_throw) {
    bprintf(output, "  %s _rc_thrown_ = SQLITE_OK;\n", rt->cql_code);
  }
}

// Emitting a stored proc is mostly setup.  We have a bunch of housekeeping to do:
//  * create new scratch buffers for the body and the locals and the cleanup section
//  * save the current output globals
//  * set the globals to point to those buffers
//  * save the old scratch masks and create new ones
//  * emit the prototype of the C function for this proc into two streams:
//      * the .h file (in prototype form)
//      * the current main_output (in function definition form)
//      * use the helper method above to emit the parameters
//  * recursively spit out the statements
//  * when this is all done assemble the pieces into the original output streams
//  * procedures that use SQL will get a hidden _db_ argument
//  * procedures that return a result set will get a statement output
//    * and the additional procedures for creating the result set and accessing it are emitted
static void cg_create_proc_stmt(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);

  // Skip generating actual functions for fetching result set if and only if
  // we are emitting an extension query fragment that would reference parent result set
  EXTRACT_MISC_ATTRS(ast, misc_attrs);
  if (misc_attrs && find_extension_fragment_attr(misc_attrs, NULL, NULL)) {
    Contract(has_result_set(ast));
    cg_proc_result_set(ast);
    return;
  }

  // set up proc_uses_throw from the state bits
  proc_uses_throw = !!(ast->sem->sem_type & SEM_TYPE_USES_THROW);

  CHARBUF_OPEN(proc_fwd_ref);
  CHARBUF_OPEN(proc_body);
  CHARBUF_OPEN(proc_locals);
  CHARBUF_OPEN(proc_cleanup);

  bool_t saved_error_target_used = error_target_used;
  error_target_used = 0;

  bool_t saved_temp_emitted = temp_statement_emitted;
  bool_t saved_seed_declared = seed_declared;
  charbuf *saved_main = cg_main_output;
  charbuf *saved_decls = cg_declarations_output;
  charbuf *saved_scratch = cg_scratch_vars_output;
  charbuf *saved_cleanup = cg_cleanup_output;
  charbuf *saved_fwd_ref = cg_fwd_ref_output;
  cg_scratch_masks *saved_masks = cg_current_masks;

  Invariant(named_temporaries == NULL);
  named_temporaries = symtab_new();

  cg_scratch_masks masks;
  cg_current_masks = &masks;
  cg_zero_masks(cg_current_masks);
  temp_statement_emitted = 0;
  in_proc = 1;
  current_proc = ast;
  seed_declared = 0;

  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  bprintf(cg_declarations_output, "\n");

  // if you're doing out_union then the row fetcher is all there is
  CSTR suffix = out_union_proc ? "_fetch_results" : "";

  CHARBUF_OPEN(proc_decl);
  CG_CHARBUF_OPEN_SYM(proc_name_base, name);
  CG_CHARBUF_OPEN_SYM(proc_sym, name, suffix);

  bprintf(cg_declarations_output, "#define _PROC_ \"%s\"\n", proc_sym.ptr);

  if (out_stmt_proc || out_union_proc) {
    cg_c_struct_for_sptr(cg_fwd_ref_output, ast->sem->sptr, NULL);
    cg_struct_teardown_info(cg_declarations_output, ast->sem->sptr, NULL);
  }

  if (out_stmt_proc || out_union_proc || result_set_proc) {
    cg_proc_result_set(ast);
  }

  bool_t need_comma = 0;
  if (dml_proc) {
    bprintf(&proc_decl, "CQL_WARN_UNUSED %s %s(sqlite3 *_Nonnull _db_", rt->cql_code, proc_sym.ptr);
    if (result_set_proc) {
      bprintf(&proc_decl, ", sqlite3_stmt *_Nullable *_Nonnull _result_stmt");
    }
    need_comma = 1;
  }
  else {
    bprintf(&proc_decl, "void %s(", proc_sym.ptr);
  }

  if (out_union_proc) {
    CG_CHARBUF_OPEN_SYM(result_set_ref, name, "_result_set_ref");

    if (need_comma) {
      bprintf(&proc_decl, ", ");
    }

    // result set type
    bprintf(&proc_decl, "%s _Nullable *_Nonnull _result_set_", result_set_ref.ptr);

    CHARBUF_CLOSE(result_set_ref);
    need_comma = 1;

    CSTR rc = dml_proc ? "_rc_" : "SQLITE_OK";
    bprintf(&proc_cleanup, "  cql_results_from_data(%s, &_rows_, &%s_info, (cql_result_set_ref *)_result_set_);\n",
      rc,
      proc_name_base.ptr);

    CG_CHARBUF_OPEN_SYM(perf_index, name, "_perf_index");
      // emit profiling start signal
      bprintf(&proc_body, "  cql_profile_start(CRC_%s, &%s);\n", proc_name_base.ptr, perf_index.ptr);
    CHARBUF_CLOSE(perf_index);
  }

  // CREATE PROC [name] ( [params] )
  if (params) {
    if (need_comma) {
      bprintf(&proc_decl, ", ");
    }
    cg_params(params, &proc_decl, &proc_body);
  }

  if (out_stmt_proc) {
    if (dml_proc || params) {
      bprintf(&proc_decl, ", ");
    }

    CG_CHARBUF_OPEN_SYM(result_type, name, "_row");
    bprintf(&proc_decl, "%s *_Nonnull _result_", result_type.ptr);
    CHARBUF_CLOSE(result_type);

    bprintf(&proc_locals, "memset(_result_, 0, sizeof(*_result_));\n");
  }

  if (!params && !out_stmt_proc && !out_union_proc && !dml_proc) {
    bprintf(&proc_decl, "void");  // make foo(void) rather than foo()
  }

  // If the proc has a result, don't expose a function with the proc name.
  // Consumers should use the _fetch_results function to execute the proc.
  // We don't make it "static" so that CQL-based tests can access it.  However
  // you would have to import it yourself to get access to the symbol (--generate_exports)
  charbuf *decl = (result_set_proc || out_stmt_proc) ? cg_fwd_ref_output : cg_header_output;
  bprintf(decl, "%s%s);\n", rt->symbol_visibility, proc_decl.ptr);

  if (options.generate_exports) {
    gen_set_output_buffer(exports_output);
    gen_declare_proc_from_create_proc(ast);
    bprintf(exports_output, ";\n");
  }

  cg_fwd_ref_output = &proc_fwd_ref;
  cg_main_output = &proc_body;
  cg_declarations_output = &proc_locals;
  cg_scratch_vars_output = &proc_locals;
  cg_cleanup_output = &proc_cleanup;

  // BEGIN [stmt_list] END
  cg_stmt_list(stmt_list);

  cg_fwd_ref_output = saved_fwd_ref;
  cg_main_output = saved_main;
  cg_declarations_output = saved_decls;
  cg_scratch_vars_output = saved_scratch;
  cg_cleanup_output = saved_cleanup;
  cg_current_masks = saved_masks;
  temp_statement_emitted = saved_temp_emitted;
  seed_declared = saved_seed_declared;

  bprintf(cg_declarations_output, proc_fwd_ref.ptr);
  bprintf(cg_declarations_output, "%s) {\n", proc_decl.ptr);

  if (dml_proc) {
    cg_emit_rc_vars(cg_declarations_output);
    if (result_set_proc) {
      bprintf(cg_declarations_output, "  *_result_stmt = NULL;\n");
    }
  }

  if (out_union_proc) {
    bprintf(cg_declarations_output, "  cql_bytebuf _rows_;\n");
    bprintf(cg_declarations_output, "  cql_bytebuf_open(&_rows_);\n");
  }

  bindent(cg_declarations_output, &proc_locals, 2);
  if (proc_locals.used > 1) {
    bprintf(cg_declarations_output, "\n");
  }

  bprintf(cg_declarations_output, "%s", proc_body.ptr);

  cg_line_directive_max(ast, cg_declarations_output);

  if (dml_proc) {
    bprintf(cg_declarations_output, "  _rc_ = SQLITE_OK;\n");
  }

  bool_t empty_statement_needed = false;

  if (error_target_used || return_used) {
    bprintf(cg_declarations_output, "\n%s:", error_target);
    empty_statement_needed = true;
  }

  bprintf(cg_declarations_output, "\n");

  if (proc_cleanup.used > 1) {
    bprintf(cg_declarations_output, proc_cleanup.ptr);
    empty_statement_needed = false;
  }

  if (result_set_proc) {
    // Because of control flow it's possible that we never actually ran a select statement
    // even if there were no errors.  Or maybe we caught the error.  In any case we must
    // not return a success code if there is no output result. Downgrade success to error.
    bprintf(cg_declarations_output, "  if (_rc_ == SQLITE_OK && !*_result_stmt) _rc_ = SQLITE_ERROR;\n");
    empty_statement_needed = false;
  }

  if (dml_proc) {
    bprintf(cg_declarations_output, "  return _rc_;\n");
    empty_statement_needed = false;
  }

  if (empty_statement_needed) {
    bprintf(cg_declarations_output, "  ; // label requires some statement\n");
  }

  bprintf(cg_declarations_output, "}\n");
  bprintf(cg_declarations_output, "#undef _PROC_\n");

  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(proc_name_base);
  CHARBUF_CLOSE(proc_decl);
  CHARBUF_CLOSE(proc_cleanup);
  CHARBUF_CLOSE(proc_locals);
  CHARBUF_CLOSE(proc_body);
  CHARBUF_CLOSE(proc_fwd_ref);

  in_proc = 0;
  current_proc = NULL;
  parent_fragment_name = NULL;

  symtab_delete(named_temporaries);
  named_temporaries = NULL;
  error_target_used = saved_error_target_used;
  Invariant(!strcmp(error_target, CQL_CLEANUP_DEFAULT_LABEL));
}

// Here we have to emit the prototype for the declared function as a C prototype
// this is just like our stored procs but we also have a return type.  See cg_declare_proc_stmt
// for a comparison.
static void cg_declare_func_stmt(ast_node *ast) {
  Contract(is_ast_declare_func_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(func_params_return, ast->right);
  EXTRACT(params, func_params_return->left);

  sem_t sem_type_return = func_params_return->right->sem->sem_type;

  CHARBUF_OPEN(func_decl);
  CG_CHARBUF_OPEN_SYM(func_sym, name);

  cg_var_decl(&func_decl, sem_type_return, func_sym.ptr, 0);
  bprintf(&func_decl, "(");

  // DECLARE FUNC [name] ( [params] ) returntype
  if (params) {
    cg_params(params, &func_decl, NULL);
  }
  else {
    bprintf(&func_decl, "void");
  }

  bprintf(cg_header_output, "%s%s);\n", rt->symbol_visibility, func_decl.ptr);

  CHARBUF_CLOSE(func_sym);
  CHARBUF_CLOSE(func_decl);
}

static void cg_declare_select_func_stmt(ast_node *ast) {
  Contract(is_ast_declare_select_func_stmt(ast));

  // We do not emit the declaration of the sql UDF into the header file
  // since it is not callable from C (unlike regular declared functions)

  // NO-OP
}

// Emit the prototype for the declared method, but no body.
static void cg_declare_proc_stmt(ast_node *ast) {
  Contract(is_ast_declare_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_name_type, ast->left);
  EXTRACT_ANY_NOTNULL(name_ast, proc_name_type->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);

  Contract(!current_proc);

  current_proc = ast;

  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  CSTR fetch_results = out_union_proc ? "_fetch_results" : "";

  CHARBUF_OPEN(proc_decl);
  CG_CHARBUF_OPEN_SYM(proc_sym, name, fetch_results);

  if (dml_proc) {
    bprintf(&proc_decl, "CQL_WARN_UNUSED %s %s(sqlite3 *_Nonnull _db_", rt->cql_code, proc_sym.ptr);
    if (out_union_proc) {
      bprintf(&proc_decl, ", ");
      CG_CHARBUF_OPEN_SYM(result_set_ref, name, "_result_set_ref");
      CG_CHARBUF_OPEN_SYM(result_set, name, "_result_set");
        cg_result_set_type_decl(cg_fwd_ref_output, result_set.ptr, result_set_ref.ptr);
        bprintf(&proc_decl, "%s _Nullable *_Nonnull _result_set_", result_set_ref.ptr);
      CHARBUF_CLOSE(result_set);
      CHARBUF_CLOSE(result_set_ref);
    }
    else if (result_set_proc) {
      bprintf(&proc_decl, ", sqlite3_stmt *_Nullable *_Nonnull _result_stmt");
    }
    if (params) {
      bprintf(&proc_decl, ", ");
    }
  }
  else {
    bprintf(&proc_decl, "void %s(", proc_sym.ptr);
    if (out_union_proc) {
      CG_CHARBUF_OPEN_SYM(result_set_ref, name, "_result_set_ref");
      CG_CHARBUF_OPEN_SYM(result_set, name, "_result_set");
        cg_result_set_type_decl(cg_fwd_ref_output, result_set.ptr, result_set_ref.ptr);
        bprintf(&proc_decl, "%s _Nullable *_Nonnull _result_set_", result_set_ref.ptr);
      CHARBUF_CLOSE(result_set);
      CHARBUF_CLOSE(result_set_ref);

      if (params) {
        bprintf(&proc_decl, ", ");
      }
    }
  }

  // CREATE PROC [name] ( [params] )
  if (params) {
    cg_params(params, &proc_decl, NULL);
  }

  if (out_stmt_proc) {
    if (dml_proc || params) {
      bprintf(&proc_decl, ", ");
    }
    CG_CHARBUF_OPEN_SYM(result_type, name, "_row");
    bprintf(&proc_decl, "%s *_Nonnull _result_", result_type.ptr);
    CHARBUF_CLOSE(result_type);
    cg_c_struct_for_sptr(cg_fwd_ref_output, ast->sem->sptr, NULL);
  }

  if (!params && !out_stmt_proc && !dml_proc && !out_union_proc) {
    bprintf(&proc_decl, "void");  // make foo(void) rather than foo()
  }

  bprintf(cg_fwd_ref_output, "%s%s);\n\n", rt->symbol_visibility, proc_decl.ptr);

  current_proc = NULL;

  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(proc_decl);
}

// Emit a bunch of variable declarations for normal variables.
// cg_var_decl does exactly this job for us.  Add any global variables to
// the header file output.
static void cg_declare_vars_type(ast_node *declare_vars_type) {
  Contract(is_ast_declare_vars_type(declare_vars_type));
  EXTRACT_NOTNULL(name_list, declare_vars_type->left);

  // DECLARE [name_list] [data_type]

  for(ast_node *ast = name_list; ast; ast = ast->right) {
    EXTRACT_ANY_NOTNULL(name_ast, ast->left);
    EXTRACT_STRING(name, name_ast);

    cg_var_decl(cg_declarations_output, name_ast->sem->sem_type, name, CG_VAR_DECL_LOCAL);
    if (!in_proc) {
      bprintf(cg_header_output, "%s", rt->symbol_visibility);
      cg_var_decl(cg_header_output, name_ast->sem->sem_type, name, CG_VAR_DECL_PROTO);
      bprintf(cg_header_output, ";\n");
    }
  }
}

// This is the callback method handed to the gen_ method that creates SQL for us
// it will call us every time it finds a variable that needs to be bound.  That
// variable is replaced by ? in the SQL output.  We end up with a list of variables
// to bind on a silver platter (but in reverse order).
static bool_t cg_capture_variables(ast_node *ast, void *context, charbuf *buffer) {
  list_item **head = (list_item**)context;
  add_item_to_list(head, ast);

  bprintf(buffer, "?");
  return true;
}

// This helper method fetchs a single column from a select statement.  The result
// is to be stored in the local variable "var" which will be in the correct state
// including nullability.  There are helpers for most cases, otherwise
// we can use normal sqlite accessors.  Strings of course create a cql_string_ref
// and blobs create a cql_blog_ref.
static void cg_get_column(sem_t sem_type, CSTR cursor, int32_t index, CSTR var, charbuf *output) {
  sem_t core_type = core_type_of(sem_type);

  bprintf(output, "  ");

  if (is_nullable(sem_type)) {
    switch (core_type) {
      case SEM_TYPE_BOOL:
        bprintf(output, "cql_column_nullable_bool(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_INTEGER:
        bprintf(output, "cql_column_nullable_int32(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_REAL:
        bprintf(output, "cql_column_nullable_double(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(output, "cql_column_nullable_int64(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_TEXT:
        bprintf(output, "cql_column_nullable_string_ref(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_BLOB:
        bprintf(output, "cql_column_nullable_blob_ref(%s, %d, &%s);\n", cursor, index, var);
        break;

    }
  }
  else {
    switch (core_type) {
      case SEM_TYPE_BOOL:
        bprintf(output, "%s = sqlite3_column_int(%s, %d) != 0;\n", var, cursor, index);
        break;
      case SEM_TYPE_INTEGER:
        bprintf(output, "%s = sqlite3_column_int(%s, %d);\n", var, cursor, index);
        break;
      case SEM_TYPE_REAL:
        bprintf(output, "%s = sqlite3_column_double(%s, %d);\n", var, cursor, index);
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(output, "%s = sqlite3_column_int64(%s, %d);\n", var, cursor, index);
        break;
      case SEM_TYPE_TEXT:
        bprintf(output, "cql_column_string_ref(%s, %d, &%s);\n", cursor, index, var);
        break;
      case SEM_TYPE_BLOB:
        bprintf(output, "cql_column_blob_ref(%s, %d, &%s);\n", cursor, index, var);
        break;
    }
  }
}

static void cg_cql_datatype(sem_t sem_type, charbuf *output) {
  if (!is_nullable(sem_type)) {
    bprintf(output, "CQL_DATA_TYPE_NOT_NULL | ");
  }

  switch (core_type_of(sem_type)) {
    case SEM_TYPE_BOOL:
        bprintf(output, "CQL_DATA_TYPE_BOOL");
        break;
      case SEM_TYPE_INTEGER:
        bprintf(output, "CQL_DATA_TYPE_INT32");
        break;
      case SEM_TYPE_REAL:
      bprintf(output, "CQL_DATA_TYPE_DOUBLE");
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(output, "CQL_DATA_TYPE_INT64");
        break;
      case SEM_TYPE_TEXT:
        bprintf(output, "CQL_DATA_TYPE_STRING");
        break;
      case SEM_TYPE_OBJECT:
        bprintf(output, "CQL_DATA_TYPE_OBJECT");
        break;
      default:
        // nothing else left
        Contract(is_blob(sem_type));
        bprintf(output, "CQL_DATA_TYPE_BLOB");
        break;
    }
}

// CQL uses the helper method cql_multifetch to get all the columns from a statement
// This helper generates the correct CQL_DATA_TYPE_* data info and emits the
// correct argument.
static void cg_fetch_column(sem_t sem_type, CSTR var) {
  cg_cql_datatype(sem_type, cg_main_output);

  bprintf(cg_main_output, ", ");

  if (!is_out_parameter(sem_type)) {
    bprintf(cg_main_output, "&");
  }

  bprintf(cg_main_output, "%s", var);
}

// CQL uses the helper method cql_multibind to bind all the columns to a statement
// This helper generates the correct CQL_DATA_TYPE_* data info and emits the
// arg in the expected format (pointers for nullable primitives) the value
// for all ref types plus all non nullables.
static void cg_bind_column(sem_t sem_type, CSTR var) {
  cg_cql_datatype(sem_type, cg_main_output);

  bprintf(cg_main_output, ", ");

  bool_t needs_address = is_nullable(sem_type) && !is_ref_type(sem_type);

  CSTR prefix = "";

  if (needs_address) {
    // out parameter is already an address
    if (!is_out_parameter(sem_type)) {
      prefix = "&";
    }
  }
  else {
    // don't want address use * prefix on out args
    if (is_out_parameter(sem_type)) {
      prefix = "*";
    }
  }

  bprintf(cg_main_output, "%s%s", prefix, var);
}

// Emit a declaration for the temporary statement _temp_stmt_ if we haven't
// already done so.  Also emit the cleanup once.
static void ensure_temp_statement() {
  if (!temp_statement_emitted) {
    bprintf(cg_declarations_output, "sqlite3_stmt *_temp_stmt = NULL;\n");
    bprintf(cg_cleanup_output, "  cql_finalize_stmt(&_temp_stmt);\n");
    temp_statement_emitted = 1;
  }
}

// Now we either find the fragment already and get its number or else
// we can make a new fragment.  This is all about creating the shared
// identifiers.  Note that we use character offsets in the main string
// as the identifiers so that we can easily offset from the base.  This
// saves us from having yet another array.  Note also that we might want to
// encode these ids in a variable length encoding so that we can have more
// than 64k of them...
static int32_t cg_intern_fragment(CSTR str, int32_t len) {
  symtab_entry *entry = symtab_find(text_fragments, str);
  if (entry) {
    return (int32_t)(int64_t)(entry->val);
  }

  int32_t result = fragment_count;
  symtab_add(text_fragments, Strdup(str), fragment_count + (char *)NULL);
  fragment_count += len + 1;  // include space for the nil
  bprintf(cg_fragments_output, "  \"%s\\0\"\n", str);
  return result;
}

// Variable length integer encoding: any byte that starts with the high bit set
// indicates that there are more bytes.  The last byte does not have the high bit set.
// So the one-byte encoding is just the simple integer as one byte.
static void cg_varinteger(int32_t val, charbuf *output) {
  do {
    uint32_t byte = val & 0x7f;
    if (val > 0x7f) {
      byte |= 0x80;
    }
    val >>= 7;
    val &= 0x01ffffff;
    bprintf(output, "\\x%02x", byte);
  } while (val);
}

// We found a shareable fragment, encode it for emission into the literal.
// Importantly these ids are 32 bits but we store them in an array of uint16_t
// so we really want to use a smarter encoding before that becomes a problem.
// We don't just want to use 32 bits everywhere because that eats the savings
static void cg_flush_fragment(bool_t first, CSTR start, CSTR cur, charbuf *output) {
  CHARBUF_OPEN(temp);
  int32_t len = (int32_t)(cur - start);

  while (start < cur) {
    cg_encode_char_as_c_string_literal(*start, &temp);
    start++;
  }

  int32_t offset = cg_intern_fragment(temp.ptr, len);
  CHARBUF_CLOSE(temp);

  cg_varinteger(offset + 1, output);

}

// Break the input string into pieces that are likely to be shared, assign each
// a number and then emit the array of those numbers instead of the original string.
// We're doing this because there is a lot of redundancy in typically generated
// SQL (e.g. the words SELECT, DROP, EXISTS appear a lot) and we can encode this
// much more economically.  Note also column names like system_function_name are
// broken because the system_ part is often shared.
static uint32_t cg_statement_fragments(CSTR in, charbuf *output) {
  Contract(in);
  int32_t len = (int32_t)strlen(in);
  Contract(len);
  uint32_t count = 0;

  CSTR start = in;
  CSTR cur = in;
  bool_t first = 1;

  int32_t prev_state = 0;
  int32_t cur_state = 0;

  bputc(output, '"');
  cg_varinteger(len + 1, output);

  for (; *cur ; cur++, prev_state = cur_state) {
    char ch = *cur;
    if (ch == ' ' || ch == '\n') {
      cur_state = 0;
    }
    else if ((ch >= 'a' && ch <= 'z') || (ch >= '@' && ch <= 'Z') || (ch >= '0' && ch <= '9')) {
      cur_state = 1;
    }
    else {
      cur_state = 2;
    }

    if (prev_state == cur_state) {
      continue;
    }

    if (cur - start <= 4 && cur_state == 0) {
      continue;
    }

    if (ch == ' ' || ch == '_') {
      cur++;
      ch = *cur;
      if ((ch >= 'a' && ch <= 'z') || (ch >= '@' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_') {
        cur_state = 1;
      }
    }

    if (start < cur) {
      cg_flush_fragment(first, start, cur, output);
      first = 0;
      start = cur;
      count++;
    }
  }

  if (start < cur) {
    cg_flush_fragment(first, start, cur, output);
    count++;
  }

  bputc(output, '"');

  return count;
}

// This is the most important function for sqlite access;  it does the heavy
// lifting of generating the C code to prepare and bind a SQL statement.
// If cg_exec is true (CG_EXEC) then the statement is executed immediately
// and finalized.  No results are expected.  To accomplish this we do the following:
//   * figure out the name of the statement, either it's given to us
//     or we're using the temp statement
//   * call get_statement_with_callback to get the text of the SQL from the AST
//     * the callback will give us all the variables to bind
//     * count the variables so we know what column numbers to use (the list is backwards!)
//   * if CG_EXEC and no variables we can use the simpler sqlite3_exec form
//   * bind any variables
//   * if there are variables CG_EXEC will step and finalize
static void cg_bound_sql_statement(CSTR stmt_name, ast_node *stmt, int32_t cg_flags) {
  list_item *vars = NULL;
  CSTR amp = "&";

  if (stmt_name && !strcmp("_result", stmt_name)) {
    // predefined out argument
    amp = "";
  }

  bool_t minify_aliases = !!(cg_flags & CG_MINIFY_ALIASES);
  bool_t exec_only = !!(cg_flags & CG_EXEC);

  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.variables_callback = cg_capture_variables;
  callbacks.variables_context = &vars;
  callbacks.star_callback = cg_expand_star;
  callbacks.minify_casts = 1;
  callbacks.minify_aliases = minify_aliases;
  callbacks.long_to_int_conv = true;

  CHARBUF_OPEN(temp);
  gen_set_output_buffer(&temp);
  gen_statement_with_callbacks(stmt, &callbacks);

  // whether or not there is a prepare statement
  bool_t has_prepare_stmt = !exec_only || vars;

  uint32_t count = 0;
  for (list_item *item = vars; item; item = item->next, count++) ;

  if (stmt_name == NULL && has_prepare_stmt) {
    ensure_temp_statement();
    stmt_name = "_temp";
  }

  if (options.compress) {
    bprintf(cg_main_output, "/*  ");
    cg_pretty_quote_plaintext(temp.ptr, cg_main_output, PRETTY_QUOTE_C | PRETTY_QUOTE_MULTI_LINE);
    bprintf(cg_main_output, " */\n");

    if (!has_prepare_stmt) {
      bprintf(cg_main_output, "_rc_ = cql_exec_frags(_db_,\n");
    }
    else {
      bprintf(cg_main_output, "_rc_ = cql_prepare_frags(_db_, %s%s_stmt,\n  ", amp, stmt_name);
    }

    bprintf(cg_main_output, "_fragments_, ");
    cg_statement_fragments(temp.ptr, cg_main_output);
    bprintf(cg_main_output, ");\n");
  }
  else {
    if (!has_prepare_stmt) {
      bprintf(cg_main_output, "_rc_ = cql_exec(_db_,\n  ");
    }
    else {
      bprintf(cg_main_output, "_rc_ = cql_prepare(_db_, %s%s_stmt,\n  ", amp, stmt_name);
    }
    cg_pretty_quote_plaintext(temp.ptr, cg_main_output, PRETTY_QUOTE_C | PRETTY_QUOTE_MULTI_LINE);
    bprintf(cg_main_output, ");\n");
  }

  CHARBUF_CLOSE(temp);

  reverse_list(&vars);

  if (count) {
    bprintf(cg_main_output, "cql_multibind(&_rc_, _db_, %s%s_stmt, %d", amp, stmt_name, count);

    // Now emit the binding args for each variable
    for (list_item *item = vars; item; item = item->next)  {
      Contract(item->ast->sem->name);
      bprintf(cg_main_output, ",\n              ");
      cg_bind_column(item->ast->sem->sem_type, item->ast->sem->name);
    }

    bprintf(cg_main_output, ");\n");
  }

  cg_error_on_not_sqlite_ok();

  if (exec_only && vars) {
    bprintf(cg_main_output, "_rc_ = sqlite3_step(%s_stmt);\n", stmt_name);
    cg_error_on_rc_notequal("SQLITE_DONE");

    bprintf(cg_main_output, "cql_finalize_stmt(&%s_stmt);\n", stmt_name);
  }

  // vars is pool allocated, so we don't need to free it
}

// This emits the declaration for an "auto cursor" -- that is a cursor
// that includes storage for all the fields it can fetch.  It uses the
// struct helper to make a suitable struct and the creates the local and
// initializes its teardown function.  Code also has to go into the cleanup
// section for suitable teardown.
static void cg_declare_auto_cursor(CSTR cursor_name, sem_struct *sptr) {
  Contract(cursor_name);
  Contract(sptr);

  int32_t refs_count = refs_count_sptr(sptr);

  cg_c_struct_for_sptr(cg_fwd_ref_output, sptr, cursor_name);

  CSTR scope = current_proc_name();
  CSTR suffix = scope ? "_" : "";
  scope = scope ? scope : "";

  CG_CHARBUF_OPEN_SYM(row_type, scope, suffix, cursor_name, "_row");

  if (refs_count) {
    CG_CHARBUF_OPEN_SYM(refs_offset, scope, suffix, cursor_name, "_refs_offset");
    bprintf(cg_declarations_output, "%s %s = { ._refs_count_ = %d, ._refs_offset_ = %s };\n", row_type.ptr, cursor_name, refs_count, refs_offset.ptr);
    bprintf(cg_cleanup_output, "  cql_teardown_row(%s);\n", cursor_name);
    cg_struct_teardown_info(cg_fwd_ref_output, sptr, cursor_name);
    CHARBUF_CLOSE(refs_offset);
  }
  else {
    bprintf(cg_declarations_output, "%s %s = { 0 };\n", row_type.ptr, cursor_name);
  }

  CHARBUF_CLOSE(row_type);
}

// This causes enum-ish declarations to go into the header file.
// Those enum things are not even used in our codegen because the CQL codegen
// simply resolves to constants.  However this will make it possible to use
// the enum in callers.  The enum is "public" in this sense.  This is a lot
// like the gen_sql code except it will be in C format.  Note C has no floating
// point enums so we have to do those with macros.

static void cg_emit_one_enum(ast_node *ast) {
  Contract(is_ast_declare_enum_stmt(ast));
  EXTRACT_NOTNULL(typed_name, ast->left);
  EXTRACT_NOTNULL(enum_values, ast->right);
  EXTRACT_ANY(name_ast, typed_name->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY_NOTNULL(type, typed_name->right);

  bprintf(cg_header_output, "#ifndef enum_%s_defined\n", name);
  bprintf(cg_header_output, "#define enum_%s_defined\n\n", name);

  if (type->sem->sem_type != SEM_TYPE_REAL) {
    bprintf(cg_header_output, "enum %s {", name);

    while (enum_values) {
       EXTRACT_NOTNULL(enum_value, enum_values->left);
       EXTRACT_ANY_NOTNULL(enum_name_ast, enum_value->left);
       EXTRACT_STRING(enum_name, enum_name_ast);

       bool_t is_long = type->sem->sem_type == SEM_TYPE_LONG_INTEGER;

       bprintf(cg_header_output, "\n  %s__%s = ", name, enum_name);

       if (is_long) {
         bprintf(cg_header_output, "_64(");
       }
       eval_format_number(enum_name_ast->sem->value, cg_header_output);

       if (is_long) {
         bprintf(cg_header_output, ")");
       }

       if (enum_values->right) {
         bprintf(cg_header_output, ",");
       }

       enum_values = enum_values->right;
    }
    bprintf(cg_header_output, "\n};\n");
  }
  else {
    bprintf(cg_header_output, "\n// enum %s (floating point values)\n", name);
    while (enum_values) {
       EXTRACT_NOTNULL(enum_value, enum_values->left);
       EXTRACT_ANY_NOTNULL(enum_name_ast, enum_value->left);
       EXTRACT_STRING(enum_name, enum_name_ast);

       bprintf(cg_header_output, "#define %s__%s ", name, enum_name);
       eval_format_number(enum_name_ast->sem->value, cg_header_output);
       bprintf(cg_header_output, "\n");

       enum_values = enum_values->right;
    }
  }
  bprintf(cg_header_output, "\n#endif\n", name);
}

static void cg_emit_enums_stmt(ast_node *ast) {
  Contract(is_ast_emit_enums_stmt(ast));
  EXTRACT(name_list, ast->left);

  if (name_list) {
    // names specified: emit those
    while (name_list) {
      // names previously checked, we assert they are good here
      EXTRACT_STRING(name, name_list->left);
      EXTRACT_NOTNULL(declare_enum_stmt, find_enum(name));
      cg_emit_one_enum(declare_enum_stmt);
      name_list = name_list->right;
    }
  }
  else {
    // none specified: emit all
    for (list_item *item = all_enums_list; item; item = item->next) {
      EXTRACT_NOTNULL(declare_enum_stmt, item->ast);
      cg_emit_one_enum(declare_enum_stmt);
    }
  }
}

// Declaring a cursor causes us to do the following:
//  * emit a local variable for the cursor in the declarations section
//  * emit cleanup logic for that local in the cleanup section
//  * execute the select or call statement that is associated with the cursor
//    * store the resulting statement for use later in fetch
//  * declare a hidden has_row local for the cursor so that the cursor name
//    can be used in expressions to see if a row was fetched.
static void cg_declare_cursor(ast_node *ast) {
  Contract(is_ast_declare_cursor(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(cursor_name, name_ast);

  bool_t out_union_proc = false;
  bool_t is_boxed = !!(name_ast->sem->sem_type & SEM_TYPE_BOXED);
  bool_t is_unboxing = is_ast_str(ast->right);

  if (is_ast_call_stmt(ast->right)) {
    out_union_proc = has_out_union_stmt_result(ast);
  }

  // only one of these (is boxed makes no sense with out union)
  Invariant(!out_union_proc || !is_boxed);

  // can't be both of these either
  Invariant(!out_union_proc || !is_unboxing);

  // unboxing implies is_boxed   a->b <==> (!a | b)
  Invariant(!is_unboxing || is_boxed);

  if (out_union_proc) {
    EXTRACT_STRING(name, ast->right->left);

    CG_CHARBUF_OPEN_SYM(result_ref, name, "_result_set_ref");

    bprintf(cg_declarations_output, "%s %s_result_set_ = NULL;\n", result_ref.ptr, cursor_name);
    bprintf(cg_declarations_output, "%s %s_row_num_ = 0;\n", rt->cql_int32, cursor_name);
    bprintf(cg_declarations_output, "%s %s_row_count_ = 0;\n", rt->cql_int32, cursor_name);
    bprintf(cg_cleanup_output, "  cql_object_release(%s_result_set_);\n", cursor_name);

    CHARBUF_CLOSE(result_ref);
  }
  else {
    bprintf(cg_declarations_output, "sqlite3_stmt *%s_stmt = NULL;\n", cursor_name);

    if (!is_boxed) {
      // easy case, no boxing, just finalize on exit.
      bprintf(cg_cleanup_output, "  cql_finalize_stmt(&%s_stmt);\n", cursor_name);
    }
  }

  if (is_select_stmt(ast->right)) {
    // DECLARE [name] CURSOR FOR [select_stmt]
    // or
    // DECLARE [name] CURSOR FOR [explain_stmt]
    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

    if (is_boxed) {
      // The next prepare will finalize the statement, we don't want to do that
      // if the cursor is being handled by boxes. The box downcount will take care of it
      bprintf(cg_main_output, "%s_stmt = NULL;\n", cursor_name);
    }
    cg_bound_sql_statement(cursor_name, select_stmt, CG_PREPARE|CG_MINIFY_ALIASES);
  }
  else if (is_unboxing) {
    // DECLARE [name] CURSOR FOR [named_box_object]

    CHARBUF_OPEN(box_name);
    bprintf(cg_main_output, "%s_stmt = cql_unbox_stmt(%s);\n", cursor_name, ast->right->sem->name);
    bprintf(&box_name, "%s_object_", cursor_name);
    cg_copy(cg_main_output, box_name.ptr, SEM_TYPE_OBJECT, ast->right->sem->name);
    CHARBUF_CLOSE(box_name);
  }
  else {
    // DECLARE [name] CURSOR FOR [call_stmt]]
    if (is_boxed) {
      // The next prepare will finalize the statement, we don't want to do that
      // if the cursor is being handled by boxes. The box downcount will take care of it
      bprintf(cg_main_output, "%s_stmt = NULL;\n", cursor_name);
    }
    EXTRACT_NOTNULL(call_stmt, ast->right);
    cg_call_stmt_with_cursor(call_stmt, cursor_name);
  }

  if (is_boxed) {
    // An object will control the lifetime of the cursor.  If the cursor is boxed
    // this is the object reference that will be used.  This way the exit path is
    // uniform regardless of whether or not the object was in fact boxed in the
    // control flow.  This is saying that it might be boxed later so we use this
    // general mechanism for lifetime. The cg_var_decl helper handles cleanup too.

    CHARBUF_OPEN(box_name);
    bprintf(&box_name, "%s_object_", cursor_name);

    cg_var_decl(cg_declarations_output, SEM_TYPE_OBJECT, box_name.ptr, CG_VAR_DECL_LOCAL);

    // the unbox case gets the object from the unbox operation above, so skip if unboxing

    if (!is_unboxing) {
      // Note we have to clear the stashed box object and then accept the new box without
      // increasing the retain count on the new box because it starts with a +1 as usual.
      // This is a job for cg_copy_for_create!

      CHARBUF_OPEN(box_value);
      bprintf(&box_value, "cql_box_stmt(%s_stmt)", cursor_name);
      cg_copy_for_create(cg_main_output, box_name.ptr, SEM_TYPE_OBJECT, box_value.ptr);
      CHARBUF_CLOSE(box_value);
    }

    CHARBUF_CLOSE(box_name);
  }

  if (name_ast->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE) {
    cg_declare_auto_cursor(cursor_name, name_ast->sem->sptr);
  }
  else {
    // make the cursor_has_row hidden variable
    CHARBUF_OPEN(temp);
    bprintf(&temp, "_%s_has_row_", cursor_name);
    cg_var_decl(cg_declarations_output, SEM_TYPE_BOOL | SEM_TYPE_NOTNULL, temp.ptr, CG_VAR_DECL_LOCAL);
    CHARBUF_CLOSE(temp);
  }
}

// This is the cursor boxing primitive, we'll make an object variable for this cursor here
// Note since the cursor is boxed its lifetime is already controlled by an object associated
// with the cursor.  This happens as soon as the cursor is created, however it is created.
// The codegen system knows that the cursor may be boxed at some point using the SEM_TYPE_BOXED flag
static void cg_set_from_cursor(ast_node *ast) {
  Contract(is_ast_set_from_cursor(ast));
  EXTRACT_ANY_NOTNULL(variable, ast->left);
  EXTRACT_ANY_NOTNULL(cursor, ast->right);
  EXTRACT_STRING(cursor_name, cursor);
  EXTRACT_STRING(var_name, variable);

  CHARBUF_OPEN(value);
  bprintf(&value, "%s_object_", cursor_name);
  cg_copy(cg_main_output, var_name, SEM_TYPE_OBJECT, value.ptr);
  CHARBUF_CLOSE(value);
}

static void cg_declare_cursor_like(ast_node *name_ast) {
  EXTRACT_STRING(cursor_name, name_ast);

  Contract(name_ast->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE);
  cg_declare_auto_cursor(cursor_name, name_ast->sem->sptr);
}

static void cg_declare_cursor_like_name(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_name(ast));
  Contract(ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  cg_declare_cursor_like(name_ast);
}

static void cg_declare_cursor_like_select(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_select(ast));
  Contract(is_select_stmt(ast->right));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);

  cg_declare_cursor_like(name_ast);
}

// The value cursor form for sure will be fetched.   We emit the necessary locals
// for the cursor here.  Those are one for "_has_row_" field and another for each
// element of the structure the cursor holds.
static void cg_declare_value_cursor(ast_node *ast) {
  Contract(is_ast_declare_value_cursor(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(cursor_name, name_ast);
  EXTRACT_NOTNULL(call_stmt, ast->right);

  // DECLARE [name] CURSOR FETCH FROM [call_stmt]]
  cg_declare_auto_cursor(cursor_name, name_ast->sem->sptr);
  cg_call_stmt_with_cursor(call_stmt, cursor_name);
}

// Fetch values has been checked for the presence of all columns and seed values
// have already been added if needed.  All we have to generate evaluation of each value
// and then a store.  There is no "fetch values into name_list" form.
static void cg_fetch_values_stmt(ast_node *ast) {
  Contract(is_ast_fetch_values_stmt(ast));

  EXTRACT(insert_dummy_spec, ast->left);
  EXTRACT(name_columns_values, ast->right);
  EXTRACT_ANY_NOTNULL(cursor, name_columns_values->left)
  EXTRACT(columns_values, name_columns_values->right);
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT(insert_list, columns_values->right);
  EXTRACT(name_list, column_spec->left);

  if (insert_dummy_spec) {
    cg_insert_dummy_spec(insert_dummy_spec);
  }

  // get the canonical name of the cursor (the string might be case-sensitively different)
  CSTR cursor_name = cursor->sem->name;

  // FETCH name [( name_list )] FROM VALUES (insert_list) [insert_dummy_spec]

  ast_node *value = insert_list;

  bprintf(cg_main_output, "%s._has_row_ = 1;\n", cursor_name);

  for (ast_node *item = name_list ; item; item = item->right, value = value->right) {
    EXTRACT_ANY_NOTNULL(expr, value->left);
    EXTRACT_ANY_NOTNULL(col, item->left);
    EXTRACT_STRING(var, col);

    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    CHARBUF_OPEN(temp);
    bprintf(&temp, "%s.%s", cursor_name, var);
    cg_store(cg_main_output, temp.ptr, col->sem->sem_type, expr->sem->sem_type, expr_is_null.ptr, expr_value.ptr);
    CHARBUF_CLOSE(temp);
    CG_POP_EVAL(expr);
  }
}

// Fetch has already been rigorously checked so we don't have to worry about
// argument counts or type mismatches in the codegen.  We have two cases:
//  * Fetch into variables
//    * loop over the variables which must match with the columns (!) and
//      use the cg_get_column helpers to emit the code for a store
//  * Fetch into auto variables
//    * loop over the field names of the sem_struct that corresponds to the cursor
//    * set each local according to the automatically generated name as above
// Note: cg_get_column does the error processing
static void cg_fetch_stmt(ast_node *ast) {
  Contract(is_ast_fetch_stmt(ast));
  EXTRACT_ANY_NOTNULL(cursor_ast, ast->left);
  EXTRACT(name_list, ast->right);

  // use the canonical name, not the AST name (case could be different)
  CSTR cursor_name = cursor_ast->sem->name;

  // FETCH [name] [INTO [name_list]]

  bool_t uses_out_union = !!(ast->sem->sem_type & SEM_TYPE_USES_OUT_UNION);

  CHARBUF_OPEN(row_test);

  if (uses_out_union) {
    bprintf(cg_main_output, "%s_row_num_++;\n", cursor_name);
    bprintf(&row_test, "%s_row_num_ < %s_row_count_", cursor_name, cursor_name);
  }
  else {
    bprintf(cg_main_output, "_rc_ = sqlite3_step(%s_stmt);\n", cursor_name);
    bprintf(&row_test, "_rc_ == SQLITE_ROW");
  }

  if (ast->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE) {
    bprintf(cg_main_output, "%s._has_row_ = %s;\n", cursor_name, row_test.ptr);
  }
  else {
    bprintf(cg_main_output, "_%s_has_row_ = %s;\n", cursor_name, row_test.ptr);
  }

  CHARBUF_CLOSE(row_test);

  // if there is a row, then we need to read the row into the variables
  // there are two alternatives: reading into locals/args or reading into
  // auto-generated cursor variables.  Either way we get each column.

  sem_struct *sptr = ast->left->sem->sptr;

  if (uses_out_union) {
    bprintf(cg_main_output, "cql_copyoutrow((cql_result_set_ref)%s_result_set_, %s_row_num_, %d",
      cursor_name, cursor_name, sptr->count);
  }
  else {
    bprintf(cg_main_output, "cql_multifetch(_rc_, %s_stmt, %d", cursor_name, sptr->count);
  }


  CSTR newline = ",\n               ";

  if (name_list) {
    int32_t i = 0; // column get is zero based

    for (ast_node *item = name_list; item; item = item->right, i++) {
      EXTRACT_ANY_NOTNULL(name_ast, item->left);
      EXTRACT_STRING(var, name_ast);
      sem_t sem_type_var = name_ast->sem->sem_type;
      bprintf(cg_main_output, "%s", newline);
      cg_fetch_column(sem_type_var, var);
    }
  }
  else {
    for (int32_t i = 0; i < sptr->count; i++) {
      CHARBUF_OPEN(temp);
      bprintf(&temp, "%s.%s", cursor_name, sptr->names[i]);
      bprintf(cg_main_output, "%s", newline);
      cg_fetch_column(sptr->semtypes[i], temp.ptr);
      CHARBUF_CLOSE(temp);
    }
  }
  bprintf(cg_main_output, ");\n");
  if (!uses_out_union) {
    cg_error_on_expr("_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE");
  }
}

static void cg_fetch_call_stmt(ast_node *ast) {
  Contract(is_ast_fetch_call_stmt(ast));
  EXTRACT_STRING(cursor_name, ast->left);
  EXTRACT_ANY_NOTNULL(call_stmt, ast->right);

  cg_call_stmt_with_cursor(call_stmt, cursor_name);
}

// The update cursor statement differs from the more general fetch form in that
// it is only to be used to tweak fields in an already loaded cursor.  The sematics
// are that if you try to "update" a cursor with no row the update is ignored.
// The purpose of this is to let you edit one or two fields of a row as you fetch them
// before using OUT or OUT UNION or INSERT ... FROM CURSOR.  You want to do this
// without having to restate all the columns, which besides being verbose makes it hard
// for people to see what things you are changing and what you are not.
static void cg_update_cursor_stmt(ast_node *ast) {
  Contract(is_ast_update_cursor_stmt(ast));
  EXTRACT_ANY(cursor, ast->left);
  EXTRACT_STRING(name, cursor);
  EXTRACT_NOTNULL(columns_values, ast->right);
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT_ANY_NOTNULL(name_list, column_spec->left);
  EXTRACT_ANY_NOTNULL(insert_list, columns_values->right);

  bprintf(cg_main_output, "if (%s._has_row_) {\n", name);

  CG_PUSH_MAIN_INDENT(stores, 2);

  ast_node *col = name_list;
  ast_node *val = insert_list;

  for ( ; col && val; col = col->right, val = val->right) {
    ast_node *expr = val->left;
    ast_node *name_ast = col->left;

    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    CHARBUF_OPEN(temp);
    bprintf(&temp, "%s.%s", name, name_ast->sem->name);
    cg_store(cg_main_output, temp.ptr, name_ast->sem->sem_type, expr->sem->sem_type, expr_is_null.ptr, expr_value.ptr);
    CHARBUF_CLOSE(temp);
    CG_POP_EVAL(expr);
  }

  CG_POP_MAIN_INDENT(stores);

  bprintf(cg_main_output, "}\n");
}

// "While" suffers from the same problem as IF and as a consequence
// generating while (expression) would not generalize.
// The overall pattern for while has to look like this:
//
//  for (;;) {
//    prep statements;
//    condition = final expression;
//    if (!condition) break;
//
//    statements;
//  }
//
// Note that while can have leave and continue substatements which have to map
// to break and continue.   That means other top level statements that aren't loops
// must not create a C loop construct or break/continue would have the wrong target.
static void cg_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);
  sem_t sem_type = expr->sem->sem_type;

  // WHILE [expr] BEGIN [stmt_list] END

  bprintf(cg_main_output, "for (;;) {\n");

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);

  if (is_nullable(sem_type)) {
    bprintf(cg_main_output, "if (!cql_is_nullable_true(%s, %s)) break;\n", expr_is_null.ptr, expr_value.ptr);
  }
  else {
    bprintf(cg_main_output, "if (!(%s)) break;\n", expr_value.ptr);
  }

  CG_POP_EVAL(expr);

  cg_stmt_list(stmt_list);

  bprintf(cg_main_output, "}\n");
}

// The general pattern for this is very simple:
//   for (;;) {
//     do the fetch;
//     if (no rows) break;
//     do your loop;
//  }
// It has to be this because the fetch might require many statements.
// There are helpers for all of this so it's super simple.
static void cg_loop_stmt(ast_node *ast) {
  Contract(is_ast_loop_stmt(ast));
  EXTRACT_NOTNULL(fetch_stmt, ast->left);
  EXTRACT(stmt_list, ast->right);
  EXTRACT_ANY_NOTNULL(cursor_ast, fetch_stmt->left);

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = cursor_ast->sem->name;

  // LOOP [fetch_stmt] BEGIN [stmt_list] END

  bprintf(cg_main_output, "for (;;) {\n");
  CG_PUSH_MAIN_INDENT(loop, 2);

  cg_fetch_stmt(fetch_stmt);

  if (fetch_stmt->left->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE) {
    bprintf(cg_main_output, "if (!%s._has_row_) break;\n", cursor_name);
  }
  else {
    bprintf(cg_main_output, "if (!_%s_has_row_) break;\n", cursor_name);
  }

  CG_POP_MAIN_INDENT(loop);

  cg_stmt_list(stmt_list);

  bprintf(cg_main_output, "}\n");
}

// Only SQL loops are allowed to use C loops, so "continue" is perfect
static void cg_continue_stmt(ast_node *ast) {
  Contract(is_ast_continue_stmt(ast));

  // CONTINUE
  bprintf(cg_main_output, "continue;\n");
}

// Only SQL loops are allowed to use C loops, so "break" is perfect
static void cg_leave_stmt(ast_node *ast) {
  Contract(is_ast_leave_stmt(ast));

  // LEAVE
  bprintf(cg_main_output, "break;\n");
}

// Only SQL loops are allowed to use C loops, so "break" is perfect
static void cg_return_stmt(ast_node *ast) {
  Contract(is_ast_return_stmt(ast) || is_ast_rollback_return_stmt(ast) || is_ast_commit_return_stmt(ast));

  // RETURN
  bool_t dml_proc = is_dml_proc(current_proc->sem->sem_type);
  if (dml_proc) {
    bprintf(cg_main_output, "_rc_ = SQLITE_OK; // clean up any SQLITE_ROW value or other non-error\n");
  }
  bprintf(cg_main_output, "goto %s; // return\n", CQL_CLEANUP_DEFAULT_LABEL);
  return_used = true;
}

static void cg_rollback_return_stmt(ast_node *ast) {
  Contract(is_ast_rollback_return_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
  ast_node *rollback = new_ast_rollback_trans_stmt(new_ast_str(current_proc_name()));
  ast_node *release = new_ast_release_savepoint_stmt(new_ast_str(current_proc_name()));
  AST_REWRITE_INFO_RESET();

  cg_bound_sql_statement(NULL, rollback, CG_EXEC);
  cg_bound_sql_statement(NULL, release, CG_EXEC);
  cg_return_stmt(ast);
}

static void cg_commit_return_stmt(ast_node *ast) {
  Contract(is_ast_commit_return_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
  ast_node *commit = new_ast_release_savepoint_stmt(new_ast_str(current_proc_name()));
  AST_REWRITE_INFO_RESET();

  cg_bound_sql_statement(NULL, commit, CG_EXEC);
  cg_return_stmt(ast);
}


// This is a no-op in sqlite.
static void cg_open_stmt(ast_node *ast) {
  Contract(is_ast_open_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  // OPEN [name]
  // there is no sqlite semantic for open, this method left empty
}

// Finalize the statement object associated with the cursor.
// Note this sets the cursor to null, so you can do it again.  Cleanup
// might also do this. That's fine.
static void cg_close_stmt(ast_node *ast) {
  Contract(is_ast_close_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  // CLOSE [name]
  bprintf(cg_main_output, "cql_finalize_stmt(&%s_stmt);\n", name);
}

// The OUT statement copies the current value of a cursor into an implicit
// OUT structure variable (_result_).  The type of the variable is inferred
// from the cursor you return.  All OUT statements in any given proc must
// agree on the exact type (this has already been verified).  At this point
// all we have to do is copy the fields.
static void cg_out_stmt(ast_node *ast) {
  Contract(is_ast_out_stmt(ast));

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = ast->left->sem->name;

  // OUT [cursor_name]

  // if there is a row, then we need to the values into the result
  CHARBUF_OPEN(var);
  CHARBUF_OPEN(value);

  sem_t sem_type_has_row = SEM_TYPE_BOOL | SEM_TYPE_NOTNULL;

  ast_node *proc_name_ast = get_proc_name(current_proc);
  EXTRACT_STRING(proc_name, proc_name_ast);

  // We can just blindly copy out the values because FETCH puts something
  // intelligent there if there is no row available (e.g. null strings)
  bprintf(&var, "_result_->%s", "_has_row_");
  bprintf(&value, "%s._has_row_", cursor_name);
  cg_copy(cg_main_output, var.ptr, sem_type_has_row, value.ptr);

  CG_CHARBUF_OPEN_SYM(sym, proc_name, "_refs_offset");

  sem_struct *sptr = ast->left->sem->sptr;
  int32_t refs_count = refs_count_sptr(sptr);

  if (refs_count) {
    // if no ref count it starts null and stays null
    bprintf(cg_main_output, "_result_->_refs_count_ = %d;\n", refs_count);
    bprintf(cg_main_output, "_result_->_refs_offset_ = %s;\n", sym.ptr);
  }

  for (int32_t i = 0; i < sptr->count; i++) {
    bclear(&var);
    bclear(&value);
    bprintf(&var, "_result_->%s", sptr->names[i]);
    bprintf(&value, "%s.%s", cursor_name, sptr->names[i]);
    cg_copy(cg_main_output, var.ptr, sptr->semtypes[i], value.ptr);
  }

  CHARBUF_CLOSE(sym);
  CHARBUF_CLOSE(value);
  CHARBUF_CLOSE(var);
}

static void cg_out_union_stmt(ast_node *ast) {
  Contract(is_ast_out_union_stmt(ast));

  // get the canonical name of the cursor (the name in the tree might be case-sensitively different)
  CSTR cursor_name = ast->left->sem->name;

  // OUT UNION [cursor_name]

  bprintf(cg_main_output, "cql_retain_row(%s);\n", cursor_name);
  bprintf(cg_main_output, "if (%s._has_row_) ", cursor_name);
  bprintf(cg_main_output, "cql_bytebuf_append(&_rows_, (const void *)&%s, sizeof(%s));\n", cursor_name, cursor_name);
}

// emit the string literal into the otuput if the current runtime matches
static void cg_echo_stmt(ast_node *ast) {
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
static void cg_call_external(ast_node *ast) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY(arg_list, ast->right);

  return cg_call_named_external(name, arg_list);
}

// This is performs an external function call, normalizing strings and passing
// the current value of nullables.  It's all straight up value-calls.  This form
// is used when the name might not be in the AST, such as we need a call to
// a sqlite helper method with user provided args.  All we do here is emit
// the  name and then use the arg list helper.
// The arg list helper gives us prep/invocation/cleanup buffers which we must emit.
static void cg_call_named_external(CSTR name, ast_node *arg_list) {
  CHARBUF_OPEN(invocation);
  CHARBUF_OPEN(prep);
  CHARBUF_OPEN(cleanup);

  int32_t stack_level_saved = stack_level;

  bprintf(&invocation, "%s(", name);
  cg_emit_external_arglist(arg_list, &prep, &invocation, &cleanup);
  bprintf(&invocation, ");\n");

  bprintf(cg_main_output, "%s%s%s", prep.ptr, invocation.ptr, cleanup.ptr);

  stack_level = stack_level_saved;  // put the scratch stack back

  CHARBUF_CLOSE(cleanup);
  CHARBUF_CLOSE(prep);
  CHARBUF_CLOSE(invocation);
}

// This is the hard work of doing the call actually happens.  We have to:
//   * evaluate each argument in the arg list
//     * for string literals, don't make a string_ref, just emit the literal as a quoted string
//       it's going to a C style call anyway...
//     * for strings
//       * add a prep statement to convert the string to a const char * in a temporary
//       * include the temporary in the arg list
//       * add a cleanup statement to release the temporary
//     * for others, use the expression value
//   * burn the top of the stack, in case the result is stored in a temporary,
//     each arg gets a fresh top of stack so they don't clobber each others results.
static void cg_emit_external_arglist(ast_node *arg_list, charbuf *prep, charbuf *invocation, charbuf *cleanup) {
  for (ast_node *item = arg_list; item; item = item->right) {
    EXTRACT_ANY(arg, item->left);
    sem_t sem_type_arg = arg->sem->sem_type;

    if (is_ast_strlit(arg)) {
      // special case, don't make a string object for string literals that are going to
      // external methods like printf
      CHARBUF_OPEN(quoted);

      EXTRACT_STRING(literal, arg);
      cg_requote_literal(literal, &quoted);
      bprintf(invocation, "%s", quoted.ptr);

      CHARBUF_CLOSE(quoted);
    }
    else {
      CG_PUSH_EVAL(arg, C_EXPR_PRI_ROOT);

      if (is_text(sem_type_arg)) {
        // external/unknown proc, convert to cstr first
        temp_cstr_count++;
        bprintf(prep, "cql_alloc_cstr(_cstr_%d, %s);\n", temp_cstr_count, arg_value.ptr);
        bprintf(invocation, "_cstr_%d", temp_cstr_count);
        bprintf(cleanup, "cql_free_cstr(_cstr_%d, %s);\n", temp_cstr_count, arg_value.ptr);
      }
      else {
        bprintf(invocation, "%s", arg_value.ptr);
      }

      CG_POP_EVAL(arg);
      stack_level++; // burn a stack slot so arg will not be re-used
    }

    if (item->right) {
      bprintf(invocation, ", ");
    }
  }
}

// We have to release any valid object we have before we call an out function
// or we will leak a reference.
static void cg_release_out_arg_before_call(sem_t sem_type_arg, sem_t sem_type_param, CSTR name) {
  if (is_ref_type(sem_type_arg)) {
    if (!is_in_parameter(sem_type_param)) {
      cg_store(cg_main_output, name, sem_type_arg, sem_type_arg, "1", "NULL");
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
static void cg_emit_one_arg(ast_node *arg, sem_t sem_type_param, sem_t sem_type_arg, charbuf *invocation) {
  CG_PUSH_EVAL(arg, C_EXPR_PRI_ROOT);

  do {
    // check for special case of out parameters
    if (is_out_parameter(sem_type_param)) {
      Contract(is_variable(sem_type_arg));  // previously checked (semantic pass)
      if (is_out_parameter(sem_type_arg)) {
        bprintf(invocation, "%s", arg->sem->name);
      }
      else {
        bprintf(invocation, "&%s", arg->sem->name);
      }

      cg_release_out_arg_before_call(sem_type_arg, sem_type_param, arg->sem->name);
      break;
    }

    if (is_ref_type(sem_type_arg)) {
      // normal case, pass the reference
      bprintf(invocation, "%s", arg_value.ptr);
      break;
    }

    bool_t must_box_arg = 0;
    if (is_nullable(sem_type_param)) {
       must_box_arg |= is_ast_null(arg);
       must_box_arg |= is_not_nullable(sem_type_arg);
       must_box_arg |= core_type_of(sem_type_arg) != core_type_of(sem_type_param);
    }

    if (must_box_arg) {
      // we have to pass a nullable of the exact type, box to that.
      CG_PUSH_TEMP(box_var, sem_type_param);
      cg_store(cg_main_output, box_var.ptr, sem_type_param, sem_type_arg, arg_is_null.ptr, arg_value.ptr);
      bprintf(invocation, "%s", box_var.ptr);
      CG_POP_TEMP(box_var);
      // burn the stack slot for the temporary, it can't be re-used during the call
      stack_level++;
      break;
    }

    if (is_nullable(sem_type_param)) {
      // This is the unfortunate case where we have split out temporary or variable that
      // the "not nullable" that we need into two parts and we need them back together
      // for the call... so we just strip off the .value from the value we were given.
      const int32_t dot_value_length = 6;  //  .value is 6 characters.

      // if it was not null we'd be boxing, above.
      Invariant(is_nullable(sem_type_arg));
      // if it was a null literal, we'd be boxing
      Invariant(!is_null_type(sem_type_arg));
      // it's bigger than .value
      Invariant(arg_value.used > dot_value_length);
      // it ends in .value
      Invariant(!strcmp(".value", arg_value.ptr + arg_value.used - dot_value_length - 1));

      arg_value.used -= dot_value_length;
      arg_value.ptr[arg_value.used - 1] = 0;
    }

    // either way arg_value is now correct
    bprintf(invocation, "%s", arg_value.ptr);
  }  while (0);

  CG_POP_EVAL(arg);
  // burn the stack slot for the temporary for the expression can't be re-used during the call
  stack_level++;
}

// This generates the invocation for a user defined external function.
// Basically we do a simple C invoke with the matching argument types which are known exactly
// we do the usual argument conversions using cg_emit_one_arg just like when calling procedures
// however we capture the return type in a temporary variable created exactly for this purpose.
// Note we do this without using the usual macros because those would invoke the function twice:
// one time for is_null and one time for _value which is a no-no.  So here we emit a direct
// assignment much like we would in cg_store.  Except we don't have to do all the cg_store things
// because we know the target is a perfectly matching local variable we just created.
static void cg_user_func(ast_node *ast, charbuf *is_null, charbuf *value) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  ast_node *params = NULL;
  ast_node *func_stmt = find_func(name);
  CSTR func_name = NULL;

  bool_t proc_as_func = 0;
  bool_t dml_proc = 0;

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
    proc_as_func = 1;
    dml_proc = is_dml_proc(proc_stmt->sem->sem_type);
  }

  sem_t sem_type_result = ast->sem->sem_type;

  // The answer will be stored in this scratch variable, any type is possible
  CG_SETUP_RESULT_VAR(ast, sem_type_result);
  CHARBUF_OPEN(invocation);
  CG_CHARBUF_OPEN_SYM(func_sym, func_name);

  if (dml_proc) {
    // at least one arg for the out arg so add _db_ with comma
    bprintf(&invocation, "_rc_ = %s(_db_, ", func_sym.ptr);
  }
  else {
    bprintf(&invocation, "%s(", func_sym.ptr);
  }

  int32_t stack_level_saved = stack_level;

  ast_node *item;
  for (item = arg_list; item; item = item->right, params = params->right) {
    EXTRACT_ANY(arg, item->left);
    sem_t sem_type_arg = arg->sem->sem_type;

    EXTRACT_NOTNULL(param, params->left);
    sem_t sem_type_param = param->sem->sem_type;

    cg_emit_one_arg(arg, sem_type_param, sem_type_arg, &invocation);

    // if any more items in the list or the out arg still pending, we need comma
    if (item->right || proc_as_func) {
      bprintf(&invocation, ", ");
    }
  }

  if (!item && params) {
    // The only way this happens is when calling a stored proc like a function
    // using the last arg as the return type.
    Invariant(proc_as_func);
    Invariant(!params->right);
    EXTRACT_NOTNULL(param, params->left);
    sem_t param_type = param->sem->sem_type;
    Invariant(is_out_parameter(param_type));
    Invariant(!is_in_parameter(param_type));

    // the result variable is not an in/out arg, it's just a regular local
    // it's otherwise the same as the paramater by consruction
    sem_t arg_type = param_type & sem_not(SEM_TYPE_OUT_PARAMETER|SEM_TYPE_IN_PARAMETER);

    cg_release_out_arg_before_call(arg_type, param_type, result_var.ptr);
    bprintf(&invocation, "&%s", result_var.ptr);
  }

  bprintf(&invocation, ")");
  stack_level = stack_level_saved;  // put the scratch stack back

  // Now store the result of the call.
  // the only trick here is we have to make sure we honor create semantics
  // otherwise we can just copy the data since the variable is for sure
  // an exact match for the call return by construction.

  if (proc_as_func) {
    // just do the function call, the result variable assignment happens as part of the call
    bprintf(cg_main_output, "%s;\n", invocation.ptr);
    if (dml_proc) {
      // cascade the failure
      cg_error_on_not_sqlite_ok();
    }
  }
  else if (is_create_func(func_stmt->sem->sem_type)) {
    cg_copy_for_create(cg_main_output, result_var.ptr, func_stmt->sem->sem_type, invocation.ptr);
  }
  else {
    cg_copy(cg_main_output, result_var.ptr, func_stmt->sem->sem_type, invocation.ptr);
  }

  CHARBUF_CLOSE(func_sym);
  CHARBUF_CLOSE(invocation);
  CG_CLEANUP_RESULT_VAR();
}

// Forward the call processing to the general helper (with cursor arg)
static void cg_call_stmt(ast_node *ast) {
  // If the call has a result set it is stored in our result parameter
  // just like a loose select statement would be.  Note this can be
  // overrided by a later result which is totally ok.  Same as for select
  // statements.
  return cg_call_stmt_with_cursor(ast, "*_result");
}

// This helper method walks all the args and all the formal paramaters at the same time
// it gets the appropriate type info for each and then generates the expression
// for the evaluation of that argument.
static void cg_emit_proc_params(charbuf *output, ast_node *params, ast_node *args) {
  for (ast_node *item = args; item; item = item->right, params = params->right) {
    EXTRACT_ANY_NOTNULL(arg, item->left);
    sem_t sem_type_arg = arg->sem->sem_type;

    EXTRACT_NOTNULL(param, params->left);
    sem_t sem_type_param = param->sem->sem_type;

    // note this might require type conversion, handled here.
    cg_emit_one_arg(arg, sem_type_param, sem_type_arg, output);

    if (item->right) {
      bprintf(output, ", ");
    }
  }
}

// A call statement has several varieties:
//  * an external call to an unknown proc
//    * use the external call helper
//  * if the target is a dml proc
//    * add the _db_ argument, for sure we have it because if we call a DML proc
//      we are a DML proc so we, too, had such an arg.  Pass it along.
//    * capture the _rc_ return code and do the error processing.
//  * if the proc results a result set, the this call is happening in the contexts
//    of "declare cursor for call func()" -- we have the cursor name.  Use it
//    to generate the output arg for the statement.
// Once that's done we can use the arg helper method to emit each arg.  There are
// several rules for each kind of arg, described above in cg_emit_one_arg
static void cg_call_stmt_with_cursor(ast_node *ast, CSTR cursor_name) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_ANY(expr_list, ast->right);

  // check for call to unknown proc, use canonical calling convention for those
  ast_node *proc_stmt = find_proc(name);
  if (!proc_stmt) {
    cg_call_external(ast);
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

  CHARBUF_OPEN(invocation);
  CG_CHARBUF_OPEN_SYM(proc_sym, proc_name, fetch_results);
  CG_CHARBUF_OPEN_SYM(result_type, proc_name, "_row");
  CG_CHARBUF_OPEN_SYM(result_sym, proc_name, "_row", "_data");

  if (dml_proc) {
    bprintf(&invocation, "_rc_ = %s(_db_", proc_sym.ptr);
    if (out_union_proc) {
      Invariant(cursor_name); // either specified or the default _result_ variable
      bprintf(&invocation, ", &%s_result_set_", cursor_name);
    }
    else if (result_set_proc) {
      Invariant(cursor_name); // either specified or the default _result_ variable
      bprintf(&invocation, ", &%s_stmt", cursor_name);
    }
    if (expr_list) {
      bprintf(&invocation, ", ");
    }
  }
  else {
    bprintf(&invocation, "%s(", proc_sym.ptr);
    if (out_union_proc) {
      Invariant(cursor_name); // either specified or the default _result_ variable
      bprintf(&invocation, "&%s_result_set_", cursor_name);
      if (expr_list) {
        bprintf(&invocation, ", ");
      }
    }
  }

  int32_t stack_level_saved = stack_level;

  // emit provided args, the param specs are needed for possible type conversions
  cg_emit_proc_params(&invocation, params, expr_list);

  // For a fetch results proc we have to add the out argument here.
  // Declare that variable if needed.
  if (out_stmt_proc) {
    if (dml_proc || params) {
      bprintf(&invocation, ", ");
    }
    bprintf(cg_main_output, "cql_teardown_row(%s);\n", cursor_name);
    bprintf(&invocation, "(%s *)&%s", result_type.ptr, cursor_name);
    bprintf(&invocation, "); // %s identical to cursor type\n", result_type.ptr);
  }
  else {
    bprintf(&invocation, ");\n");
  }
  stack_level = stack_level_saved;  // put the scratch stack back

  bprintf(cg_main_output, "%s", invocation.ptr);

  if (out_union_proc) {
    bprintf(cg_main_output, "%s_row_num_ = %s_row_count_ = -1;\n", cursor_name, cursor_name);
  }

  if (dml_proc) {
    // cascade the failure
    cg_error_on_not_sqlite_ok();
  }

  if (out_union_proc) {
    bprintf(cg_main_output, "%s_row_count_ = cql_result_set_get_count((cql_result_set_ref)%s_result_set_);\n",
      cursor_name, cursor_name);
  }

  CHARBUF_CLOSE(result_sym);
  CHARBUF_CLOSE(result_type);
  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(invocation);
}

// Straight up DDL invocation.  The ast has the statement, execute it!
static void cg_any_ddl_stmt(ast_node *ast) {
  cg_bound_sql_statement(NULL, ast, CG_EXEC|CG_NO_MINIFY_ALIASES);
}

// Straight up DML invocation.  The ast has the statement, execute it!
static void cg_std_dml_exec_stmt(ast_node *ast) {
  cg_bound_sql_statement(NULL, ast, CG_EXEC|CG_MINIFY_ALIASES);
}

// DML with PREPARE.  The ast has the statement.
// Note: _result_ is the output variable for the sqlite3_stmt we generate
//       this was previously added when the stored proc params were generated.
static void cg_select_stmt(ast_node *ast) {
  Contract(is_select_stmt(ast));
  cg_bound_sql_statement("_result", ast, CG_PREPARE|CG_MINIFY_ALIASES);
}

// DML with PREPARE.  The ast has the statement.
static void cg_with_select_stmt(ast_node *ast) {
  Contract(is_ast_with_select_stmt(ast));
  cg_select_stmt(ast);
}

static void cg_insert_dummy_spec(ast_node *ast) {
  EXTRACT_ANY_NOTNULL(expr, ast->left); // the seed expr

  CSTR name = "_seed_";

  sem_t sem_type_var = SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL;
  sem_t sem_type_expr = expr->sem->sem_type;

  if (!seed_declared) {
    cg_var_decl(cg_declarations_output, sem_type_var, name, CG_VAR_DECL_LOCAL);
    seed_declared = 1;
  }

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ASSIGN);
  cg_store(cg_main_output, name, sem_type_var, sem_type_expr, expr_is_null.ptr, expr_value.ptr);
  CG_POP_EVAL(expr);
}

static void cg_opt_seed_process(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(insert_type, ast->left);
  EXTRACT(insert_dummy_spec, insert_type->left);

  if (insert_dummy_spec) {
    cg_insert_dummy_spec(insert_dummy_spec);
  }
}

// DML invocation but first set the seed variable if present
static void cg_insert_stmt(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));

  cg_opt_seed_process(ast);
  cg_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_with_insert_stmt(ast_node *ast) {
  Contract(is_ast_with_insert_stmt(ast));
  EXTRACT_NOTNULL(insert_stmt, ast->right);
  cg_opt_seed_process(insert_stmt);
  cg_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_with_upsert_stmt(ast_node *ast) {
  Contract(is_ast_with_upsert_stmt(ast));
  EXTRACT_NOTNULL(upsert_stmt, ast->right);
  EXTRACT_NOTNULL(insert_stmt, upsert_stmt->left);
  cg_opt_seed_process(insert_stmt);
  cg_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// DML invocation but first set the seed variable if present
static void cg_upsert_stmt(ast_node *ast) {
  Contract(is_ast_upsert_stmt(ast));
  EXTRACT_NOTNULL(insert_stmt, ast->left);

  cg_opt_seed_process(insert_stmt);
  cg_bound_sql_statement(NULL, ast, CG_EXEC | CG_NO_MINIFY_ALIASES);
}

// Very little magic is needed to do try/catch in our context.  The error
// handlers for all the sqlite calls check _rc_ and if it's an error they
// "goto" the current error target.  That target is usually CQL_CLEANUP_DEFAULT_LABEL.
// Inside the try block, the cleanup handler is changed to the catch block.
// The catch block puts it back.  Otherwise, generate nested statements as usual.
static void cg_trycatch_helper(ast_node *try_list, ast_node *try_extras, ast_node *catch_list) {
  CHARBUF_OPEN(catch_start);
  CHARBUF_OPEN(catch_end);

  // We need unique labels for this block
  ++catch_block_count;
  bprintf(&catch_start, "catch_start_%d", catch_block_count);
  bprintf(&catch_end, "catch_end_%d", catch_block_count);

  // Divert the error target.
  CSTR saved_error_target = error_target;
  bool_t saved_error_target_used = error_target_used;
  error_target = catch_start.ptr;
  error_target_used = 0;

  // Emit the try code.
  bprintf(cg_main_output, "// try\n{\n");

  cg_stmt_list(try_list);

  if (try_extras) {
    cg_stmt_list(try_extras);
  }

  // If we get to the end, skip the catch block.
  bprintf(cg_main_output, "  goto %s;\n}\n", catch_end.ptr);

  // Emit the catch code, with labels at the start and the end.
  if (error_target_used) {
    bprintf(cg_main_output, "%s: ", catch_start.ptr);
  }

  // Restore the error target, the catch block runs with the old error target
  error_target = saved_error_target;
  error_target_used = saved_error_target_used;

  bprintf(cg_main_output, "{\n");

  if (proc_uses_throw) {
    bprintf(cg_main_output, "  _rc_thrown_ = _rc_;\n");
  }

  cg_stmt_list(catch_list);

  bprintf(cg_main_output, "}\n%s:;\n", catch_end.ptr);

  CHARBUF_CLOSE(catch_end);
  CHARBUF_CLOSE(catch_start);
}

// the helper does all the work, see those notes
static void cg_trycatch_stmt(ast_node *ast) {
  Contract(is_ast_trycatch_stmt(ast));
  EXTRACT_NAMED(try_list, stmt_list, ast->left);
  EXTRACT_NAMED(catch_list, stmt_list, ast->right);

  cg_trycatch_helper(try_list, NULL, catch_list);
}

// this is just a special try/catch
static void cg_proc_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_proc_savepoint_stmt(ast));
  EXTRACT(stmt_list, ast->left);

  if (stmt_list) {
    AST_REWRITE_INFO_SET(ast->lineno, ast->filename);
    ast_node *savepoint = new_ast_savepoint_stmt(new_ast_str(current_proc_name()));
    ast_node *release1  = new_ast_release_savepoint_stmt(new_ast_str(current_proc_name()));
    ast_node *release2  = new_ast_release_savepoint_stmt(new_ast_str(current_proc_name()));
    ast_node *rollback  = new_ast_rollback_trans_stmt(new_ast_str(current_proc_name()));
    ast_node *try_extra_stmts = new_ast_stmt_list(release1, NULL);
    ast_node *throw_stmt = new_ast_throw_stmt();
    ast_node *catch_stmts =
		new_ast_stmt_list(rollback,
                new_ast_stmt_list(release2,
                new_ast_stmt_list(throw_stmt, NULL)));
    AST_REWRITE_INFO_RESET();
    cg_bound_sql_statement(NULL, savepoint, CG_EXEC);
    cg_trycatch_helper(stmt_list, try_extra_stmts, catch_stmts);
  }
}

// Convert _rc_ into an error code.  If it already is one keep it.
// Then go to the current error target.
static void cg_throw_stmt(ast_node *ast) {
  Contract(is_ast_throw_stmt(ast));

  bprintf(cg_main_output, "_rc_ = cql_best_error(_rc_thrown_);\n");
  bprintf(cg_main_output, "goto %s;\n", error_target);
  error_target_used = 1;
}

// Dispatch to one of the statement helpers using the symbol table.
// There are special rules for the DDL methods. If they appear in a
// global context (outside of any stored proc) they do not run, they
// are considered declarations only.
static void cg_one_stmt(ast_node *stmt, ast_node *misc_attrs) {
  // we're going to compute the fragment name if needed but we always start clean
  parent_fragment_name = NULL;

  // There are special rules for some procedures, we avoid emiting them here
  // so that we don't generate the comments or anything for them.  Testing later
  // is more of a mess.
  if (misc_attrs && is_ast_create_proc_stmt(stmt)) {
    // The base fragment never creates any code, it only defines types and so forth.
    if (find_base_fragment_attr(misc_attrs, NULL, NULL)) {
      // If we find the base attribute, we're done.  We just need the testing tag.
      if (options.test) {
        bprintf(cg_header_output, "\n// The statement ending at line %d\n", stmt->lineno);
        bprintf(cg_declarations_output, "\n// The statement ending at line %d\n", stmt->lineno);
      }

      return;
    }

    // Extension fragments should be emitted when compiled by themselves, if the assembly
    // fragment is present the extension fragments are suppressed and only the assembly is emitted.
    // Normally extensions are compiled by themselves (with just the base) then AGAIN with the assembly.
    // The assembly expects that we have seen all the extensions before it is compiled.
    if (find_extension_fragment_attr(misc_attrs, cg_set_parent_fragment_name, NULL)) {
      // We know the name of the assembly fragment it must be the same as the name in the attribute.
      // If we find that procedure, everything about the extension is suppressed for this compilation.
      if (find_proc(parent_fragment_name)) {
        return;
      }
    }

    // also look for the assembly fragment, if present use that to set the parent fragment name
    find_assembly_query_attr(misc_attrs, cg_set_parent_fragment_name, NULL);
  }

  symtab_entry *entry = symtab_find(cg_stmts, stmt->type);

  // DDL operations not in a procedure are ignored
  // but they can declare schema during the semantic pass
  if (entry->val == cg_any_ddl_stmt && !in_proc) {
     return;
  }

  // don't emit a # line directive for the echo statement because it will messed up
  // if the echo doesn't end in a linefeed and that's legal.  And there is normally
  // no visible code for these things anyway.
  if (!is_ast_echo_stmt(stmt)) {
    charbuf *line_out = (stmt_nesting_level == 1) ? cg_declarations_output : cg_main_output;
    cg_line_directive_min(stmt, line_out);
  }

  // Emit a helpful comment for top level statements.
  if (stmt_nesting_level == 1) {
    charbuf *out = cg_main_output;
    if (is_ast_declare_vars_type(stmt) || is_ast_proc(stmt) || is_ast_echo_stmt(stmt)) {
      out = cg_declarations_output;
    }

    bool_t skip_comment = false;
    skip_comment |= is_ast_declare_func_stmt(stmt);
    skip_comment |= is_ast_declare_select_func_stmt(stmt);
    skip_comment |= is_ast_emit_enums_stmt(stmt);
    skip_comment |= (!options.test && is_ast_echo_stmt(stmt));
    skip_comment |= entry->val == cg_no_op;

    // put a line marker in the header file in case we want a test suite that verifies that
    if (options.test) {
      bprintf(cg_header_output, "\n// The statement ending at line %d\n", stmt->lineno);
    }

    // emit comments for most statements: we do not want to require the global proc block
    // just because there was a comment so this is suppressed for "no code" things
    if (!skip_comment) {
      if (options.test) {
        bprintf(out, "\n// The statement ending at line %d\n", stmt->lineno);
      } else {
        bprintf(cg_header_output, "\n// Generated from %s:%d\n", stmt->filename, stmt->lineno);
        bprintf(cg_declarations_output, "\n// Generated from %s:%d\n", stmt->filename, stmt->lineno);
      }
      // emit source comment
      bprintf(out, "\n/*\n");
      gen_stmt_level = 1;
      gen_set_output_buffer(out);
      gen_one_stmt(stmt);
      bprintf(out, ";\n*/\n");
    }
  }

  // These are all the statements there are, we have to find it in this table
  // or else someone added a new statement and it isn't supported yet.
  Invariant(entry);
  ((void (*)(ast_node*))entry->val)(stmt);
}

// Emit the nested statements with one more level of indenting.
static void cg_stmt_list(ast_node *head) {
  if (!head) {
    return;
  }

  stmt_nesting_level++;

  charbuf *saved_main = cg_main_output;
  CHARBUF_OPEN(temp);
  cg_main_output = &temp;

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    cg_one_stmt(stmt, misc_attrs);
  }

  cg_main_output = saved_main;
  bindent(cg_main_output, &temp, 2);
  CHARBUF_CLOSE(temp);

  stmt_nesting_level--;
}

// Here we generate the type constant for the column
// The type must be one of the types that we can fetch
// from SQLite so that means not a struct, not the null type
// (the null type is reserved for the literal NULL, it's
// not a storage class) and certainly nothing that is
// a struct or sentinel type.  Likewise objects cannot
// be the result of a select, so that's out too.
static void cg_data_type(charbuf *buffer, sem_t sem_type) {
  Contract(is_unitary(sem_type));
  Contract(!is_null_type(sem_type));
  Contract(!is_object(sem_type));

  sem_t core_type = core_type_of(sem_type);

  switch (core_type) {
    case SEM_TYPE_INTEGER:
      bprintf(buffer, "CQL_DATA_TYPE_INT32");
      break;
    case SEM_TYPE_LONG_INTEGER:
      bprintf(buffer, "CQL_DATA_TYPE_INT64");
      break;
    case SEM_TYPE_REAL:
      bprintf(buffer, "CQL_DATA_TYPE_DOUBLE");
      break;
    case SEM_TYPE_BOOL:
      bprintf(buffer, "CQL_DATA_TYPE_BOOL");
      break;
    case SEM_TYPE_TEXT:
      bprintf(buffer, "CQL_DATA_TYPE_STRING");
      break;
    case SEM_TYPE_BLOB:
      bprintf(buffer, "CQL_DATA_TYPE_BLOB");
      break;
  }
  if (is_not_nullable(sem_type)) {
    bprintf(buffer, " | CQL_DATA_TYPE_NOT_NULL");
  }
}

// All the data you need to make a getter...
// there's a lot of it and most of it is the same for all cases
typedef struct getter_info {
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
  bool_t is_ext_fragment;
  bool_t is_private;
} getter_info;

// Using the information above we emit a column getter.  The essence of this
// is to reach into the data field of the result set and index the requested row
// then fetch the column.  There's two parts to this:
// * First we need to compute the name of the getter, it's fairly simple coming
//   from the name of the procedure that had the select and the field name that we
//   are getting.
// * Second we emit the body of the getter there's a few cases here
//   * for fragments, we don't do our own getting, the master assembled query knows
//     all the pieces so we delegate to it;  but we need to emit a declaration for
//     the getter in the master query
//   * for normal rowsets it's foo->data[i].column
//   * for single row result sets it's just foo->data->column; there is only the one row
static void cg_proc_result_set_getter(getter_info *info) {
  charbuf *h = info->headers;
  charbuf *d = info->defs;
  sem_t sem_type = info->ret_type;

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(symbol_prefix, info->is_private ? "__PRIVATE__" : "", rt->symbol_prefix);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    col_getter_sym,
    // only prefix with __PRIVATE__ the private assembly methods
    info->is_ext_fragment ? rt->symbol_prefix : symbol_prefix.ptr,
    info->name,
    "_get_",
    info->col,
    info->sym_suffix);

  CHARBUF_OPEN(func_decl);
  cg_var_decl(&func_decl, sem_type, col_getter_sym.ptr, CG_VAR_DECL_PROTO);
  bprintf(&func_decl, "(%s _Nonnull result_set", info->result_set_ref_type);

  // a procedure that uses OUT gives exactly one row, so no index in the API
  if (!info->uses_out) {
    bprintf(&func_decl, ", %s row", rt->cql_int32);
  }

  bprintf(&func_decl, ")");

  bprintf(h, "%s%s;\n", rt->symbol_visibility, func_decl.ptr);
  bprintf(d, "\n%s {\n", func_decl.ptr);

  // Note that the special handling of assembly fragments is not needed for the non-inline-getters case
  // because in that case all the getters are emitted into the defs section anyway.

  if (info->is_ext_fragment) {
    // extension fragment can be compiled independently so we include forward declared parent row getter in its header
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
      parent_col_getter_sym,
      symbol_prefix.ptr,
      parent_fragment_name,
      "_get_",
      info->col,
      info->sym_suffix);
    CHARBUF_OPEN(parent_func_fwd_decl);
    cg_var_decl(&parent_func_fwd_decl, sem_type, parent_col_getter_sym.ptr, CG_VAR_DECL_PROTO);
    bprintf(&parent_func_fwd_decl, "(%s _Nonnull result_set", info->result_set_ref_type);
    bprintf(&parent_func_fwd_decl, ", %s row", rt->cql_int32);
    bprintf(&parent_func_fwd_decl, ")");
    bprintf(h, "%s%s;\n\n", rt->symbol_visibility, parent_func_fwd_decl.ptr);
    CHARBUF_CLOSE(parent_func_fwd_decl);

    // extension fragment row getter should always delegate to corresponding getter from its parent fragment
    bprintf(d, "  return %s%s_get_%s%s(result_set, row);\n", symbol_prefix.ptr, parent_fragment_name,
      info->col, info->sym_suffix ?: "");
    CHARBUF_CLOSE(parent_col_getter_sym);
  }
  else {
    bprintf(d,
            "  %s *data = (%s *)%s((cql_result_set_ref)result_set);\n",
            info->row_struct_type,
            info->row_struct_type,
            rt->cql_result_set_get_data);

    // Single row result set is always data[0]
    // And data->field looks nicer than data[0].field
    if (info->uses_out) {
      bprintf(d, "  return data->%s%s;\n", info->col, info->value_suffix ?: "");
    }
    else {
      bprintf(d, "  return data[row].%s%s;\n", info->col, info->value_suffix ?: "");
    }
  }
  bprintf(d, "}\n");

  CHARBUF_CLOSE(func_decl);
  CHARBUF_CLOSE(col_getter_sym);
  CHARBUF_CLOSE(symbol_prefix);
}

// The inlineable version of the getter can be generated instead of the opened coded version as above
// This type inlines well because it uses a small number of standard helpers to do the fetching.
// The situation is not so different from the original open coded case above.
// This code mirrors cg_proc_result_set_getter
// * First we need to compute the name of the getter, it's fairly simple coming
//   from the name of the procedure that had the select and the field name that we
//   are getting.
// * Second we emit the body of the getter there's a few cases here
//   * for fragments, we don't do our own getting, the master assembled query knows
//     all the pieces so we delegate to it;  but we need to emit a declaration for
//     the getter in the master query
//   * for normal rowsets it's something like cql_result_set_get_bool(result_set, row, column)
//   * for single row result sets it's just cql_result_set_get_bool(result_set, 0, column)
static void cg_proc_result_set_type_based_getter(getter_info *_Nonnull info)
{
  charbuf *h = info->headers;
  charbuf *d = info->defs;
  charbuf *out = NULL;

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(symbol_prefix, info->is_private ? "__PRIVATE__" : "", rt->symbol_prefix);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    col_getter_sym,
    // only prefix with __PRIVATE__ the private assembly methods
    info->is_ext_fragment ? rt->symbol_prefix : symbol_prefix.ptr,
    info->name,
    "_get_",
    info->col,
    info->sym_suffix);

  CHARBUF_OPEN(func_decl);
  cg_var_decl(&func_decl, info->ret_type, col_getter_sym.ptr, CG_VAR_DECL_PROTO);
  bprintf(&func_decl, "(%s _Nonnull result_set", info->result_set_ref_type);

  // a procedure that uses OUT gives exactly one row, so no index in the API
  if (!info->uses_out) {
    bprintf(&func_decl, ", %s row", rt->cql_int32);
  }
  bprintf(&func_decl, ")");

  CSTR row = info->uses_out ? "0" : "row";

  // not set yet
  Invariant(!out);

  if (is_assembly_query) {
    // In the assembly case we emit only the prototype into the header file the body goes into the main section
    // This is necessary because the extension fragments could be in other linkage units and they will try to
    // call these getters.  So the linkage has to allow for this even if they are say a different dylib/dll.
    // The body of the function will be emitted into the defs section (d).
    bprintf(h, "\n%s%s;", rt->symbol_visibility, func_decl.ptr);
    bprintf(d, "\n%s%s {\n", rt->symbol_visibility, func_decl.ptr);
    out = d;
  }
  else {
    // The inline body will all go into the header file in the normal case.
    bprintf(h, "\nstatic inline %s {\n", func_decl.ptr);
    out = h;
  }

  // definitely set now
  Invariant(out);

  if (info->is_ext_fragment) {
    // extension fragment can be complied independently so we include forward declared parent row getter in its header
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
      parent_col_getter_sym,
      symbol_prefix.ptr,
      parent_fragment_name,
      "_get_",
      info->col,
      info->sym_suffix);
    CHARBUF_OPEN(parent_func_decl);
    cg_var_decl(&parent_func_decl, info->ret_type, parent_col_getter_sym.ptr, CG_VAR_DECL_PROTO);
    bprintf(&parent_func_decl, "(%s _Nonnull result_set, int32_t row)", info->result_set_ref_type);
    bprintf(out, "  extern %s;\n", parent_func_decl.ptr);
    CHARBUF_CLOSE(parent_func_decl);
    CHARBUF_CLOSE(parent_col_getter_sym);

    // extension fragment row getter should always delegate to corresponding getter from its parent fragment
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
      extension_getter_sym,
      symbol_prefix.ptr,
      parent_fragment_name,
      "_get_",
      info->col,
      info->sym_suffix);
    bprintf(out, "  return %s(result_set, row);\n", extension_getter_sym.ptr);
    CHARBUF_CLOSE(extension_getter_sym);
  }
  else {
    bprintf(out, "  return ");
    if (is_ref_type(info->name_type) && is_nullable(info->ret_type)) {
      bprintf(out,
              "%s((cql_result_set_ref)result_set, %s, %d) ? NULL : ",
              rt->cql_result_set_get_is_null,
              row,
              info->col_index);
    }
    // You can't return object types in result sets, so they cannot be used here.
    Contract(info->name_type != SEM_TYPE_OBJECT);
    switch (info->name_type) {
      case SEM_TYPE_NULL:
        bprintf(out, "%s", rt->cql_result_set_get_is_null);
        break;
      case SEM_TYPE_BOOL:
        bprintf(out, "%s", rt->cql_result_set_get_bool);
        break;
      case SEM_TYPE_REAL:
        bprintf(out, "%s", rt->cql_result_set_get_double);
        break;
      case SEM_TYPE_INTEGER:
        bprintf(out, "%s", rt->cql_result_set_get_int32);
        break;
      case SEM_TYPE_LONG_INTEGER:
        bprintf(out, "%s", rt->cql_result_set_get_int64);
        break;
      case SEM_TYPE_TEXT:
        bprintf(out, "%s", rt->cql_result_set_get_string);
        break;
      case SEM_TYPE_BLOB:
        bprintf(out, "%s", rt->cql_result_set_get_blob);
        break;
    }
    bprintf(out, "((cql_result_set_ref)result_set, %s, %d);\n", row, info->col_index);
  }
  bprintf(out, "}\n");

  CHARBUF_CLOSE(func_decl);
  CHARBUF_CLOSE(col_getter_sym);
  CHARBUF_CLOSE(symbol_prefix);
}

// Write out the autodrop info into the stream
static void cg_one_autodrop(CSTR _Nonnull name, ast_node *_Nonnull misc_attr_value, void *_Nullable context) {
  Invariant(context);
  charbuf *output = (charbuf *)context;
  bprintf(output, "%s\\0", name);
}

// If a stored proc is marked with the autodrop annotation when we automatically drop the indicated
// tables when the proc is finished running.  The attributes should look like this:
// @attribute(cql:autodrop=(table1, table2, ,...))
static void cg_autodrops(ast_node *misc_attrs, charbuf *output) {
  if (!misc_attrs) {
     return;
  }
  CHARBUF_OPEN(temp);
    find_autodrops(misc_attrs, cg_one_autodrop, &temp);
    if (temp.used > 1) {
      bprintf(output, "    .autodrop_tables = \"%s\",\n", temp.ptr);
    }
  CHARBUF_CLOSE(temp);
}

typedef struct fetch_result_info {
  CSTR data_types_sym;
  CSTR col_offsets_sym;
  CSTR refs_offset_sym;
  CSTR identity_columns_sym;
  CSTR row_sym;
  CSTR proc_sym;
  CSTR perf_index;
  ast_node *misc_attrs;
  int32_t refs_count;
  bool_t has_identity_columns;
  bool_t dml_proc;
  bool_t use_stmt;
  int32_t indent;
  CSTR prefix;
} fetch_result_info;

// This generates the cql_fetch_info structure for the various output flavors
// there is some variability here and so it's useful to consolidate the logic.
// The factors are:
//   * if this is not a DML proc then there's no DB and no return code
//   * if there is no statement it means we're returning from a value cursor (which implies the above, too)
//   * we may or may not have references in the data type, so we include those if needed
//   * likewise identity columns
//   * the autodrops helper itself tests for the presence of the attribute in the correct form
//
// The above represents the runtime cql_fetch_info struct that will be used to either fetch all
// rows or else fetch a single row from a given buffer.  Either way, the metadata is assembled
// here for use at runtime.
//
// Other fields are not conditional.
static void cg_fetch_info(fetch_result_info *info, charbuf *output)
{
  CHARBUF_OPEN(tmp);
    if (info->prefix) {
      bprintf(&tmp, "cql_fetch_info %s_info = {\n", info->prefix);
    }
    else {
      bprintf(&tmp, "cql_fetch_info info = {\n");
    }
    if (info->dml_proc) {
      bprintf(&tmp, "  .rc = rc,\n");
      bprintf(&tmp, "  .db = _db_,\n");
    }
    else {
      bprintf(&tmp, "  .rc = SQLITE_OK,\n"); // this case can't fail, there are no db ops
    }
    if (info->use_stmt) {
      bprintf(&tmp, "  .stmt = stmt,\n");
    }
    bprintf(&tmp, "  .data_types = %s,\n", info->data_types_sym);
    bprintf(&tmp, "  .col_offsets = %s,\n", info->col_offsets_sym);
    if (info->refs_count) {
      bprintf(&tmp, "  .refs_count = %d,\n", info->refs_count);
      bprintf(&tmp, "  .refs_offset = %s,\n", info->refs_offset_sym);
    }
    if (info->has_identity_columns) {
      bprintf(&tmp, "  .identity_columns = %s,\n", info->identity_columns_sym);
    }
    bprintf(&tmp, "  .rowsize = sizeof(%s),\n", info->row_sym);
    bprintf(&tmp, "  .crc = CRC_%s,\n", info->proc_sym);
    bprintf(&tmp, "  .perf_index = &%s,\n", info->perf_index);

    cg_autodrops(info->misc_attrs, &tmp);

    bprintf(&tmp, "};\n");
  bindent(output, &tmp,  info->indent);
  CHARBUF_CLOSE(tmp);
}

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
static void cg_proc_result_set(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  Contract(is_struct(ast->sem->sem_type));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT_STRING(name, ast->left);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  bool_t suppress_result_set = misc_attrs && exists_attribute_str(misc_attrs, "suppress_result_set");
  if (suppress_result_set) {
    return;
  }

  bool_t uses_out = has_out_stmt_result(ast);
  bool_t uses_out_union = has_out_union_stmt_result(ast);
  bool_t result_set_proc = has_result_set(ast);

  // exactly one of these
  Invariant(uses_out + uses_out_union + result_set_proc == 1);

  bool_t dml_proc = is_dml_proc(ast->sem->sem_type);
  bool_t is_ext_fragment = misc_attrs &&
      find_extension_fragment_attr(misc_attrs, cg_set_parent_fragment_name, NULL);

  bool_t suppress_getters = misc_attrs && exists_attribute_str(misc_attrs, "suppress_getters");

  is_assembly_query = misc_attrs && find_assembly_query_attr(misc_attrs, NULL, NULL);
  charbuf *h = cg_header_output;
  charbuf *d = cg_declarations_output;

  // register the proc name if there is a callback, the particular result type will do whatever it wants
  rt->register_proc_name && rt->register_proc_name(name);

  // name replacement such that extension fragment should always reference to
  // parent result set type instead of setting up their own
  CSTR result_set_name = is_ext_fragment ? parent_fragment_name : name;

  CHARBUF_OPEN(data_types);
  CHARBUF_OPEN(result_set_create);
  CHARBUF_OPEN(temp);
  CG_CHARBUF_OPEN_SYM(getter_prefix, name);
  CG_CHARBUF_OPEN_SYM(stored_proc_name_sym, name, "_stored_procedure_name");
  CG_CHARBUF_OPEN_SYM(result_set_sym, result_set_name, "_result_set");
  CG_CHARBUF_OPEN_SYM(result_set_ref, result_set_name, "_result_set_ref");
  CG_CHARBUF_OPEN_SYM(proc_sym, name);
  CG_CHARBUF_OPEN_SYM(row_sym, name, "_row");
  CG_CHARBUF_OPEN_SYM(data_types_sym, name, "_data_types");
  CG_CHARBUF_OPEN_SYM(data_types_count_sym, name, "_data_types_count");
  CG_CHARBUF_OPEN_SYM(col_offsets_sym, name, "_col_offsets");
  CG_CHARBUF_OPEN_SYM(refs_offset_sym, name, "_refs_offset");
  CG_CHARBUF_OPEN_SYM(identity_columns_sym, name, "_identity_columns");
  CG_CHARBUF_OPEN_SYM(result_count_sym, name, "_result_count");
  CG_CHARBUF_OPEN_SYM(fetch_results_sym, name, "_fetch_results");
  CG_CHARBUF_OPEN_SYM(copy_sym, name, "_copy");
  CG_CHARBUF_OPEN_SYM(perf_index, name, "_perf_index");

  sem_struct *sptr = ast->sem->sptr;
  uint32_t count = sptr->count;

  bprintf(h, "#define CRC_%s %lldL\n", proc_sym.ptr, (llint_t)crc_charbuf(&proc_sym));
  // setting up perf index unless we are currently emitting an extension fragment
  // which does not fetch result independently (it depends on parent result set instead)
  if (!is_ext_fragment) {
    bprintf(d, "static int32_t %s;\n", perf_index.ptr);

    bprintf(h,
            "\n%s%s _Nonnull %s;\n",
            rt->symbol_visibility,
            rt->cql_string_ref,
            stored_proc_name_sym.ptr);
    bprintf(d, "\n%s(%s, \"%s\");\n", rt->cql_string_literal, stored_proc_name_sym.ptr, name);

    if (result_set_proc) {
      // First build the struct we need
      // As we walk the fields, construct the teardown operation needed
      // to clean up that field and save it.
      bprintf(d, "\ntypedef struct %s {\n", row_sym.ptr);
      cg_fields_in_canonical_order(d, sptr);
      bprintf(d, "} %s;\n", row_sym.ptr);
    }

    bprintf(h, "\n#define %s %d\n", data_types_count_sym.ptr, count);
    bprintf(&data_types,
            "\nuint8_t %s[%s] = {\n",
            data_types_sym.ptr,
            data_types_count_sym.ptr);
  }

  // If we are generating the typed getters, setup the function tables.
  if (options.generate_type_getters && !is_ext_fragment) {
    bprintf(h,
            "\n%suint8_t %s[%s];\n",
            rt->symbol_visibility,
            data_types_sym.ptr,
            data_types_count_sym.ptr);
  }

  bprintf(h, "\n");
  cg_result_set_type_decl(h, result_set_sym.ptr, result_set_ref.ptr);

  // For each field emit the _get_field method
  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR col = sptr->names[i];

    // Skip generating row-level data type for extension since parent fragment
    // contains same info for all properly indexed rows
    if (!is_ext_fragment) {
      bprintf(&data_types, "  ");
      cg_data_type(&data_types, sem_type);
      bprintf(&data_types, ", // %s\n", col);
    }

    if (suppress_getters) {
      continue;
    }

    sem_t core_type = core_type_of(sem_type);
    bool_t col_is_nullable = is_nullable(sem_type);

    getter_info info = {
      .name = name,
      .col = col,
      .col_index = i,
      .headers = h,
      .defs = d,
      .uses_out = uses_out,
      .result_set_ref_type = result_set_ref.ptr,
      .row_struct_type = row_sym.ptr,
      .is_ext_fragment = is_ext_fragment,
      .is_private = false,
    };

    // if the current row is equal or greater than the base query count
    // is considered a private accesor since it belongs to the extension accessors
    if (is_ext_fragment || is_assembly_query) {
      // we already know the base compiled with no errors
      ast_node *base_proc = find_base_fragment(parent_fragment_name);
      Invariant(base_proc);
      Invariant(base_proc->sem);
      Invariant(base_proc->sem->sptr);

      uint32_t col_count_for_base = base_proc->sem->sptr->count;
      Invariant(col_count_for_base > 0);

      info.is_private = i >= col_count_for_base;
    }

    if (options.generate_type_getters) {
      if (col_is_nullable && !is_ref_type(sem_type)) {
        info.ret_type = SEM_TYPE_BOOL | SEM_TYPE_NOTNULL;
        info.name_type = SEM_TYPE_NULL;
        info.sym_suffix = "_is_null";
        cg_proc_result_set_type_based_getter(&info);

        info.ret_type = core_type | SEM_TYPE_NOTNULL;
        info.name_type = core_type;
        info.sym_suffix = "_value";
        cg_proc_result_set_type_based_getter(&info);
      }
      else {
        info.ret_type = sem_type;
        info.name_type = core_type;
        info.sym_suffix = NULL;
        cg_proc_result_set_type_based_getter(&info);
      }
    }
    else if (col_is_nullable && is_numeric(sem_type)) {
      info.ret_type = SEM_TYPE_BOOL | SEM_TYPE_NOTNULL;
      info.sym_suffix = "_is_null";
      info.value_suffix = ".is_null";
      cg_proc_result_set_getter(&info);

      info.ret_type = core_type_of(sem_type) | SEM_TYPE_NOTNULL,
      info.sym_suffix = "_value";
      info.value_suffix = ".value";
      cg_proc_result_set_getter(&info);
    }
    else {
      info.ret_type = sem_type;
      info.sym_suffix = NULL;
      info.value_suffix = NULL;
      cg_proc_result_set_getter(&info);
    }
  }

  if (options.generate_type_getters) {
    bprintf(h, "\n");
  }

  CHARBUF_OPEN(is_null_getter);

  // Check whether we need to generate a copy function.
  bool_t generate_copy = (options.generate_copy ||
                         (rt->proc_should_generate_copy && rt->proc_should_generate_copy(name)));

  int32_t refs_count = refs_count_sptr(sptr);
  // Skip generating reference and column offsets for extension fragment since it always
  // delegate to parent fragment for retrieving results with proper index
  if (!is_ext_fragment) {
    bprintf(&data_types, "};\n");
    bprintf(d, data_types.ptr);

    if (refs_count && !uses_out) {
      // note: fetch procs have already emitted this.
      cg_refs_offset(d, sptr, refs_offset_sym.ptr, row_sym.ptr);
    }

    cg_col_offsets(d, sptr, col_offsets_sym.ptr, row_sym.ptr);
  }

  bool_t has_identity_columns = cg_identity_columns(d, name, misc_attrs, identity_columns_sym.ptr);

  bprintf(&result_set_create,
          "(%s)%s(%s, count, %d, %s, meta)",
          result_set_ref.ptr,
          rt->cql_result_set_ref_new,
          uses_out ? "row" : "b.ptr",
          count,
          data_types_sym.ptr);

  CHARBUF_CLOSE(is_null_getter);

  // Emit foo_result_count, which is really just a proxy to cql_result_set_get_count,
  // but it is hiding the cql_result_set implementation detail from the API of the generated
  // code by providing a proc-scoped function for it with the typedef for the result set.
  bclear(&temp);
  bprintf(&temp, "%s %s(%s _Nonnull result_set)", rt->cql_int32, result_count_sym.ptr, result_set_ref.ptr);
  bprintf(h, "%s%s;\n", rt->symbol_visibility, temp.ptr);
  bprintf(d, "\n%s {\n", temp.ptr);
  bprintf(d, "  return %s((cql_result_set_ref)result_set);\n", rt->cql_result_set_get_count);
  bprintf(d, "}\n");

  // Skip generating fetch result function for extension fragment since it always gets
  // results fetched through parent fragment
  if (!is_ext_fragment) {
    if (uses_out) {
      // Emit foo_fetch_results, it has the same signature as foo only with a result set
      // instead of a statement.
      bclear(&temp);

      // either return code or void
      if (dml_proc) {
        bprintf(&temp, "CQL_WARN_UNUSED %s ", rt->cql_code);
      }
      else {
        bprintf(&temp, "void ");
      }

      // proc name
      bprintf(&temp, "%s(", fetch_results_sym.ptr);

      // optional db reference
      if (dml_proc) {
        bprintf(&temp, "sqlite3 *_Nonnull _db_,");
      }

      // result set type
      bprintf(&temp, " %s _Nullable *_Nonnull result_set", result_set_ref.ptr);

      // args to forward
      if (params) {
        bprintf(&temp, ", ");
        cg_params(params, &temp, NULL);
      }

      // ready for prototype and function begin now
      bprintf(h, "%s%s);\n", rt->symbol_visibility, temp.ptr);
      bprintf(d, "\n%s) {\n", temp.ptr);

      // emit profiling start signal
      bprintf(d, "  cql_profile_start(CRC_%s, &%s);\n", proc_sym.ptr, perf_index.ptr);

      // one row result set from out parameter

      bprintf(d, "  *result_set = NULL;\n");
      bprintf(d, "  %s *row = (%s *)calloc(1, sizeof(%s));\n", row_sym.ptr, row_sym.ptr, row_sym.ptr);
      bprintf(d, "  ");

      // optional db arg and return code
      if (dml_proc) {
        bprintf(d, "cql_code rc = %s(_db_, ", proc_sym.ptr);
      }
      else {
        bprintf(d, "%s(", proc_sym.ptr);
      }

      if (params) {
        cg_param_names(params, d);
        bprintf(d, ", ");
      }
      bprintf(d, "row);\n");

      fetch_result_info info = {
          .dml_proc = dml_proc,
          .use_stmt = 0,
          .data_types_sym = data_types_sym.ptr,
          .col_offsets_sym = col_offsets_sym.ptr,
          .refs_count = refs_count,
          .refs_offset_sym = refs_offset_sym.ptr,
          .has_identity_columns = has_identity_columns,
          .identity_columns_sym = identity_columns_sym.ptr,
          .row_sym = row_sym.ptr,
          .proc_sym = proc_sym.ptr,
          .perf_index = perf_index.ptr,
          .misc_attrs = misc_attrs,
          .indent = 2,
      };

      cg_fetch_info(&info, d);

      if (dml_proc) {
        bprintf(d, "  return ");
      }
      else {
        bprintf(d, "  ");
      }
      bprintf(d, "cql_one_row_result(&info, (char *)row, row->_has_row_, (cql_result_set_ref *)result_set);\n");
      bprintf(d, "}\n\n");
    }
    else if (result_set_proc) {
      // Emit foo_fetch_results, it has the same signature as foo only with a result set
      // instead of a statement.
      bclear(&temp);
      bprintf(&temp,
          "CQL_WARN_UNUSED %s %s(sqlite3 *_Nonnull _db_, %s _Nullable *_Nonnull result_set",
          rt->cql_code,
          fetch_results_sym.ptr,
          result_set_ref.ptr);

      if (params) {
        bprintf(&temp, ", ");
        cg_params(params, &temp, NULL);
      }

      // To create the rowset we make a byte buffer object.  That object lets us
      // append row data to an in-memory stream.  Each row is fetched by binding
      // to a row object.  We use cg_get_column to read the columns.  The row
      // object of course has exactly the right type for each column.
      bprintf(h, "%s%s);\n", rt->symbol_visibility, temp.ptr);
      bprintf(d, "\n%s) {\n", temp.ptr);
      bprintf(d, "  sqlite3_stmt *stmt = NULL;\n");

      // emit profiling start signal
      bprintf(d, "  cql_profile_start(CRC_%s, &%s);\n", proc_sym.ptr, perf_index.ptr);

      // Invoke the base proc to get the statement
      bprintf(d, "  cql_code rc = %s(_db_, &stmt", proc_sym.ptr);
      if (params) {
        bprintf(d, ", ");
        cg_param_names(params, d);
      }
      bprintf(d, ");\n");

      // Now read in in all the rows using this fetch information
      fetch_result_info info = {
          .dml_proc = 1,
          .use_stmt = 1,
          .data_types_sym = data_types_sym.ptr,
          .col_offsets_sym = col_offsets_sym.ptr,
          .refs_count = refs_count,
          .refs_offset_sym = refs_offset_sym.ptr,
          .has_identity_columns = has_identity_columns,
          .identity_columns_sym = identity_columns_sym.ptr,
          .row_sym = row_sym.ptr,
          .proc_sym = proc_sym.ptr,
          .perf_index = perf_index.ptr,
          .misc_attrs = misc_attrs,
          .indent = 2,
      };

      cg_fetch_info(&info, d);
      bprintf(d, "  return cql_fetch_all_results(&info, (cql_result_set_ref *)result_set);\n");
      bprintf(d, "}\n\n");
    }
    else {
      // this is the only case left
      Invariant(uses_out_union);

      fetch_result_info info = {
          .dml_proc = 0,
          .use_stmt = 0,
          .data_types_sym = data_types_sym.ptr,
          .col_offsets_sym = col_offsets_sym.ptr,
          .refs_count = refs_count,
          .refs_offset_sym = refs_offset_sym.ptr,
          .has_identity_columns = has_identity_columns,
          .identity_columns_sym = identity_columns_sym.ptr,
          .row_sym = row_sym.ptr,
          .proc_sym = proc_sym.ptr,
          .perf_index = perf_index.ptr,
          .misc_attrs = misc_attrs,
          .indent = 0,
          .prefix = proc_sym.ptr,
      };

      cg_fetch_info(&info, d);
    }
  }

  if (generate_copy) {
    bprintf(h,
            "#define %s(result_set, result_set_to%s) \\\n"
            "%s((cql_result_set_ref)(result_set))->copy( \\\n"
            "  (cql_result_set_ref)(result_set), \\\n"
            "  (cql_result_set_ref *)(result_set_to), \\\n"
            "  %s, \\\n"
            "  %s)\n",
            copy_sym.ptr,
            uses_out ? "": ", from, count",
            rt->cql_result_set_get_meta,
            uses_out ? "0" : "from",
            uses_out ? "1" : "count");
  }

  if (rt->generate_equality_macros & !is_ext_fragment) {
    bclear(&temp);

    CG_CHARBUF_OPEN_SYM(hash_sym, name, uses_out ? "_hash" : "_row_hash");
    bprintf(h,
            "#define %s(result_set%s) "
            "%s((cql_result_set_ref)(result_set))->rowHash((cql_result_set_ref)(result_set), %s)\n",
            hash_sym.ptr,
            uses_out ? "" : ", row",
            rt->cql_result_set_get_meta,
            uses_out ? "0" : "row");
    CHARBUF_CLOSE(hash_sym);

    CG_CHARBUF_OPEN_SYM(equal_sym, name, uses_out ? "_equal" : "_row_equal");
    bprintf(h,
            "#define %s(rs1%s, rs2%s) \\\n"
            "%s((cql_result_set_ref)(rs1))->rowsEqual( \\\n"
            "  (cql_result_set_ref)(rs1), \\\n"
            "  %s, \\\n"
            "  (cql_result_set_ref)(rs2), \\\n"
            "  %s)\n",
            equal_sym.ptr,
            uses_out ? "" : ", row1",
            uses_out ? "" : ", row2",
            rt->cql_result_set_get_meta,
            uses_out ? "0" : "row1",
            uses_out ? "0" : "row2");
    CHARBUF_CLOSE(equal_sym);

    if (has_identity_columns) {
      CG_CHARBUF_OPEN_SYM(same_sym, name, uses_out ? "_same" : "_row_same");
      bprintf(h, "#define %s(rs1%s, rs2%s) \\\n"
              "%s((cql_result_set_ref)(rs1))->rowsSame( \\\n"
              "  (cql_result_set_ref)(rs1), \\\n"
              "  %s, \\\n"
              "  (cql_result_set_ref)(rs2), \\\n"
              "  %s)\n",
              same_sym.ptr,
              uses_out ? "" : ", row1",
              uses_out ? "" : ", row2",
              rt->cql_result_set_get_meta,
              uses_out ? "0" : "row1",
              uses_out ? "0" : "row2");
      CHARBUF_CLOSE(same_sym);
    }
  }

  CHARBUF_CLOSE(perf_index);
  CHARBUF_CLOSE(copy_sym);
  CHARBUF_CLOSE(fetch_results_sym);
  CHARBUF_CLOSE(result_count_sym);
  CHARBUF_CLOSE(identity_columns_sym);
  CHARBUF_CLOSE(refs_offset_sym);
  CHARBUF_CLOSE(col_offsets_sym);
  CHARBUF_CLOSE(data_types_count_sym);
  CHARBUF_CLOSE(data_types_sym);
  CHARBUF_CLOSE(row_sym);
  CHARBUF_CLOSE(proc_sym);
  CHARBUF_CLOSE(result_set_ref);
  CHARBUF_CLOSE(result_set_sym);
  CHARBUF_CLOSE(stored_proc_name_sym);
  CHARBUF_CLOSE(getter_prefix);
  CHARBUF_CLOSE(temp);
  CHARBUF_CLOSE(result_set_create);
  CHARBUF_CLOSE(data_types);
}

// Main entry point for code-gen.  This will set up the buffers for the global
// variables and any loose calls or DML.  Any code that needs to run in the
// global scope will be added to the global_proc.  This is the only codegen
// error that is possible.  If you need global code and you don't have a global
// proc then you can't proceed.  Semantic analysis doƒesn't want to know that stuff.
// Otherwise all we do is set up the most general buffers for the global case and
// spit out a function with the correct name.
cql_noexport void cg_c_main(ast_node *head) {
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  CSTR header_file_name = options.file_names[0];
  CSTR body_file_name = options.file_names[1];
  CSTR exports_file_name = NULL;

  int32_t export_file_index = 2;

  if (options.generate_exports) {
    if (options.file_names_count <= export_file_index) {
      cql_error("--cg had the wrong number of arguments, argument %d was needed\n", export_file_index + 1);
      cql_cleanup_and_exit(1);
    }

    exports_file_name = options.file_names[export_file_index];
  }

  cg_c_init();

  cg_scratch_masks global_scratch_masks;
  cg_current_masks = &global_scratch_masks;
  cg_zero_masks(cg_current_masks);

  if (options.compress) {
    // seed the fragments with popular ones based on statistics
    CHARBUF_OPEN(ignored);
    cg_statement_fragments(
      ", " "AS " "." ",\n  " "NOT " "(" ") " "TEXT" "id " "thread_"
      "NULL," "id," "LONG_" "timestamp_" "CAST," "TABLE " "media_"
      "NULL " "EXISTS " "IF " "CREATE " "DROP " "C" "url_" "profile_" "VIEW " "= ",
      &ignored);
    CHARBUF_CLOSE(ignored);
  }

  CHARBUF_OPEN(exports_file);
  CHARBUF_OPEN(header_file);
  CHARBUF_OPEN(body_file);
  CHARBUF_OPEN(indent);

  if (exports_file_name) {
    exports_output = &exports_file;

    if (rt->exports_prefix) {
      bprintf(exports_output, "%s", rt->exports_prefix);
    }
  }

  cg_stmt_list(head);

  bprintf(&body_file, "%s", rt->source_prefix);

  if (options.c_include_namespace) {
    bprintf(&body_file, "#include \"%s/%s\"\n\n", options.c_include_namespace, options.file_names[0]);
  } else {
    bprintf(&body_file, "#include \"%s\"\n\n", options.file_names[0]);
  }

  bprintf(&body_file, "%s", rt->source_wrapper_begin);
  bprintf(&body_file, "#pragma clang diagnostic ignored \"-Wbitwise-op-parentheses\"\n");
  bprintf(&body_file, "#pragma clang diagnostic ignored \"-Wshift-op-parentheses\"\n");
  bprintf(&body_file, "#pragma clang diagnostic ignored \"-Wlogical-not-parentheses\"\n");
  bprintf(&body_file, "#pragma clang diagnostic ignored \"-Wliteral-conversion\"\n");

  bprintf(&body_file, "%s", cg_fwd_ref_output->ptr);
  bprintf(&body_file, "%s", cg_constants_output->ptr);

  if (cg_fragments_output->used > 1) {
    bprintf(&body_file, "static const char _fragments_[] = \n%s;\n", cg_fragments_output->ptr);
  }
  bprintf(&body_file, "%s", cg_declarations_output->ptr);

  // main function after constants and decls (if needed)

  bool_t global_proc_needed = cg_main_output->used > 1 || cg_scratch_vars_output->used > 1;

  if (global_proc_needed) {
    exit_on_no_global_proc();

    proc_uses_throw = !!(global_proc_flags & SEM_TYPE_USES_THROW);
    bprintf(&body_file, "#define _PROC_ %s\n", global_proc_name);

    bindent(&indent, cg_scratch_vars_output, 2);
    bprintf(&body_file, "\ncql_code %s(sqlite3 *_Nonnull _db_) {\n", global_proc_name);
    cg_emit_rc_vars(&body_file);

    bprintf(&body_file, "%s", indent.ptr);
    bprintf(&body_file, "%s", cg_main_output->ptr);
    bprintf(&body_file, "\n");
    if (error_target_used) {
      bprintf(&body_file, "%s:\n", error_target);
    }
    bprintf(&body_file, "%s", cg_cleanup_output->ptr);
    bprintf(&body_file, "  return _rc_;\n");
    bprintf(&body_file, "}\n");
    bprintf(&body_file, "\n#undef _PROC_\n");
  }

  bprintf(&body_file, "%s", rt->source_wrapper_end);

  bprintf(&header_file, "%s", rt->header_prefix);
  bprintf(&header_file, rt->cqlrt_template, rt->cqlrt);
  bprintf(&header_file, "%s", rt->header_wrapper_begin);
  bprintf(&header_file, "%s", cg_header_output->ptr);
  bprintf(&header_file, "%s", rt->header_wrapper_end);

  CHARBUF_CLOSE(indent);

  cql_write_file(header_file_name, header_file.ptr);

  if (options.nolines || options.test) {
    cql_write_file(body_file_name, body_file.ptr);
  }
  else {
    CHARBUF_OPEN(body_with_line_directives);

    cg_insert_line_directives(body_file.ptr, &body_with_line_directives);
    cql_write_file(body_file_name, body_with_line_directives.ptr);

    CHARBUF_CLOSE(body_with_line_directives);
  }

  if (exports_file_name) {
    cql_write_file(exports_file_name, exports_file.ptr);
  }

  CHARBUF_CLOSE(body_file);
  CHARBUF_CLOSE(header_file);
  CHARBUF_CLOSE(exports_file);

  cg_c_cleanup();
}

static void cg_c_init(void) {
  cg_c_cleanup(); // reset globals/statics
  cg_common_init();

  Contract(!error_target_used);

  // one table for the whole translation unit
  Contract(!string_literals);
  string_literals = symtab_new_case_sens();

  Contract(!text_fragments);
  text_fragments = symtab_new_case_sens();

  DDL_STMT_INIT(drop_table_stmt);
  DDL_STMT_INIT(drop_view_stmt);
  DDL_STMT_INIT(drop_index_stmt);
  DDL_STMT_INIT(drop_trigger_stmt);
  DDL_STMT_INIT(create_table_stmt);
  DDL_STMT_INIT(create_virtual_table_stmt);
  DDL_STMT_INIT(create_trigger_stmt);
  DDL_STMT_INIT(create_index_stmt);
  DDL_STMT_INIT(create_view_stmt);
  DDL_STMT_INIT(alter_table_add_column_stmt);

  NO_OP_STMT_INIT(enforce_normal_stmt);
  NO_OP_STMT_INIT(enforce_strict_stmt);
  NO_OP_STMT_INIT(declare_schema_region_stmt);
  NO_OP_STMT_INIT(declare_deployable_region_stmt);
  NO_OP_STMT_INIT(begin_schema_region_stmt);
  NO_OP_STMT_INIT(end_schema_region_stmt);
  NO_OP_STMT_INIT(schema_upgrade_version_stmt);
  NO_OP_STMT_INIT(schema_upgrade_script_stmt);
  NO_OP_STMT_INIT(schema_ad_hoc_migration_stmt);
  NO_OP_STMT_INIT(declare_enum_stmt);

  STD_DML_STMT_INIT(begin_trans_stmt);
  STD_DML_STMT_INIT(commit_trans_stmt);
  STD_DML_STMT_INIT(rollback_trans_stmt);
  STD_DML_STMT_INIT(savepoint_stmt);
  STD_DML_STMT_INIT(release_savepoint_stmt);
  STD_DML_STMT_INIT(delete_stmt);
  STD_DML_STMT_INIT(with_delete_stmt);
  STD_DML_STMT_INIT(update_stmt);
  STD_DML_STMT_INIT(with_update_stmt);

  // insert forms have some special processing for the 'seed' case
  STMT_INIT(insert_stmt);
  STMT_INIT(with_insert_stmt);
  STMT_INIT(upsert_stmt);
  STMT_INIT(with_upsert_stmt);

  // these DML methods need to use prepare and have other processing other than just EXEC
  STMT_INIT(select_stmt);
  STMT_INIT(with_select_stmt);

  STMT_INIT(if_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(leave_stmt);
  STMT_INIT(continue_stmt);
  STMT_INIT(return_stmt);
  STMT_INIT(rollback_return_stmt);
  STMT_INIT(commit_return_stmt);
  STMT_INIT(call_stmt);
  STMT_INIT(declare_vars_type);
  STMT_INIT(assign);
  STMT_INIT(set_from_cursor);
  STMT_INIT(create_proc_stmt);
  STMT_INIT(emit_enums_stmt);
  STMT_INIT(declare_proc_stmt);
  STMT_INIT(declare_func_stmt);
  STMT_INIT(declare_select_func_stmt);
  STMT_INIT(trycatch_stmt);
  STMT_INIT(proc_savepoint_stmt);
  STMT_INIT(throw_stmt);

  STMT_INIT(declare_cursor);
  STMT_INIT(declare_cursor_like_name);
  STMT_INIT(declare_cursor_like_select);
  STMT_INIT(declare_value_cursor);

  STMT_INIT(loop_stmt);
  STMT_INIT(fetch_stmt);
  STMT_INIT(fetch_values_stmt);
  STMT_INIT(update_cursor_stmt);
  STMT_INIT(fetch_call_stmt);
  STMT_INIT(open_stmt);

  STMT_INIT(close_stmt);
  STMT_INIT(out_stmt);
  STMT_INIT(out_union_stmt);
  STMT_INIT(echo_stmt);

  FUNC_INIT(attest_notnull);
  FUNC_INIT(ifnull);
  FUNC_INIT(coalesce);
  FUNC_INIT(last_insert_rowid);
  FUNC_INIT(changes);
  FUNC_INIT(printf);
  FUNC_INIT(cql_get_blob_size);

  EXPR_INIT(num, cg_expr_num, "num", C_EXPR_PRI_ROOT);
  EXPR_INIT(str, cg_expr_str, "STR", C_EXPR_PRI_ROOT);
  EXPR_INIT(null, cg_expr_null, "NULL", C_EXPR_PRI_ROOT);
  EXPR_INIT(dot, cg_expr_dot, "DOT", C_EXPR_PRI_ROOT);

  EXPR_INIT(lshift, cg_binary, "<<", C_EXPR_PRI_SHIFT);
  EXPR_INIT(rshift, cg_binary, ">>", C_EXPR_PRI_SHIFT);
  EXPR_INIT(bin_and, cg_binary, "&", C_EXPR_PRI_BAND);
  EXPR_INIT(bin_or, cg_binary, "|", C_EXPR_PRI_BOR);

  EXPR_INIT(mul, cg_binary, "*", C_EXPR_PRI_MUL);
  EXPR_INIT(div, cg_binary, "/", C_EXPR_PRI_MUL);
  EXPR_INIT(mod, cg_binary, "%", C_EXPR_PRI_MUL);
  EXPR_INIT(add, cg_binary, "+", C_EXPR_PRI_ADD);
  EXPR_INIT(sub, cg_binary, "-", C_EXPR_PRI_ADD);
  EXPR_INIT(not, cg_unary, "!", C_EXPR_PRI_UNARY);
  EXPR_INIT(tilde, cg_unary, "~", C_EXPR_PRI_UNARY);
  EXPR_INIT(uminus, cg_unary, "-", C_EXPR_PRI_UNARY);
  EXPR_INIT(eq, cg_binary_compare, "==", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(ne, cg_binary_compare, "!=", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(lt, cg_binary_compare, "<", C_EXPR_PRI_LT_GT);
  EXPR_INIT(gt, cg_binary_compare, ">", C_EXPR_PRI_LT_GT);
  EXPR_INIT(ge, cg_binary_compare, ">=", C_EXPR_PRI_LT_GT);
  EXPR_INIT(le, cg_binary_compare, "<=", C_EXPR_PRI_LT_GT);
  EXPR_INIT(call, cg_expr_call, "CALL", C_EXPR_PRI_ROOT);
  EXPR_INIT(between_rewrite, cg_expr_between_rewrite, "BETWEEN", C_EXPR_PRI_ROOT);
  EXPR_INIT(and, cg_expr_and, "AND", C_EXPR_PRI_LAND);
  EXPR_INIT(or, cg_expr_or, "OR", C_EXPR_PRI_LOR);
  EXPR_INIT(select_stmt, cg_expr_select, "SELECT", C_EXPR_PRI_ROOT);
  EXPR_INIT(with_select_stmt, cg_expr_select, "WITH...SELECT", C_EXPR_PRI_ROOT);
  EXPR_INIT(is, cg_expr_is, "IS NULL", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(is_not, cg_expr_is_not, "IS NOT NULL", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(like, cg_binary_compare, "LIKE", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(not_like, cg_binary_compare, "LIKE", C_EXPR_PRI_EQ_NE);
  EXPR_INIT(in_pred, cg_expr_in_pred_or_not_in, "IN", C_EXPR_PRI_ROOT);
  EXPR_INIT(not_in, cg_expr_in_pred_or_not_in, "NOT IN", C_EXPR_PRI_ROOT);
  EXPR_INIT(case_expr, cg_expr_case, "CASE", C_EXPR_PRI_ROOT);
  EXPR_INIT(cast_expr, cg_expr_cast, "CAST", C_EXPR_PRI_ROOT);
}

cql_noexport void cg_c_cleanup() {
  cg_common_cleanup();

  SYMTAB_CLEANUP(named_temporaries);
  SYMTAB_CLEANUP(string_literals);
  SYMTAB_CLEANUP(text_fragments);

  parent_fragment_name = NULL;
  exports_output = NULL;
  error_target = NULL;
  cg_current_masks = NULL;

  case_statement_count = 0;
  catch_block_count = 0;
  error_target = CQL_CLEANUP_DEFAULT_LABEL;
  error_target_used = false;
  return_used = false;
  fragment_count = 0;
  seed_declared = false;
  stack_level = 0;
  string_literals_count = 0;
  temp_cstr_count = 0;
  temp_statement_emitted = false;
}

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cg_common.h"
#include "ast.h"
#include "sem.h"
#include "symtab.h"

// Storage declarations
cql_data_defn( symtab *_Nullable cg_stmts );
cql_data_defn( symtab *_Nullable cg_funcs );
cql_data_defn( symtab *_Nullable cg_exprs );
cql_data_defn( charbuf *_Nullable cg_header_output );
cql_data_defn( charbuf *_Nullable cg_main_output );
cql_data_defn( charbuf *_Nullable cg_fwd_ref_output );
cql_data_defn( charbuf *_Nullable cg_constants_output );
cql_data_defn( charbuf *_Nullable cg_declarations_output );
cql_data_defn( charbuf *_Nullable cg_scratch_vars_output );
cql_data_defn( charbuf *_Nullable cg_cleanup_output );
cql_data_defn( charbuf *_Nullable cg_fragments_output );

// Prints a symbol name, along with any configured prefix, to the specified buffer.
// Multiple CSTRs may be supplied to build the name, which will be concatenated
// together.  The configured symbol case will be applied to the full symbol name.
// The prefix will be included as specified.
//
// All input names are assumed to be in snake case already.
cql_noexport void cg_sym_name(charbuf *_Nonnull output, CSTR _Nonnull symbol_prefix, CSTR _Nonnull name, ...)
{
  // Print the prefix first
  bprintf(output, symbol_prefix);

  // Setup the arg list
  va_list args;
  va_start(args, name);

  CSTR name_component = name;

  // Check the case configuration
  switch (rt->symbol_case) {
    case cg_symbol_case_snake:{
      // No need to modify it, everything in here is already snake case.
      do {
        bprintf(output, "%s", name_component);
      } while ((name_component = va_arg(args, CSTR)));
      break;
    }
    case cg_symbol_case_camel:
    case cg_symbol_case_pascal:{
      // Remove all underscores and uppercase each next character, along with the first if pascal case.
      bool should_upper = (rt->symbol_case != cg_symbol_case_camel);
      do {
        const size_t len = strlen(name_component);
        for (size_t i = 0; i != len; ++i) {
          if (name_component[i] == '_') {
            should_upper = true;
          } else if (should_upper) {
            bputc(output, (char)toupper(name_component[i]));
            should_upper = false;
          } else {
            bputc(output, name_component[i]);
          }
        }
      } while ((name_component = va_arg(args, CSTR)));
      break;
    }
  }
  va_end(args);
}

#define ALLOC_AND_OPEN_CHARBUF_REF(x) \
  (x) = (charbuf *)calloc(1, sizeof(charbuf)); \
  bopen(x);

#define CLEANUP_CHARBUF_REF(x) if (x) { bclose(x); free(x);  x = NULL; }

cql_noexport void cg_common_init(void)
{
  // All of these will leak, but we don't care.  The tool will shut down after running cg, so it is pointless to clean
  // up after ourselves here.
  cg_stmts = symtab_new();
  cg_funcs = symtab_new();
  cg_exprs = symtab_new();

  ALLOC_AND_OPEN_CHARBUF_REF(cg_header_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_main_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_fwd_ref_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_constants_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_declarations_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_scratch_vars_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_cleanup_output);
  ALLOC_AND_OPEN_CHARBUF_REF(cg_fragments_output)
}

// lots of AST nodes require no action -- this guy is very good at that.
cql_noexport void cg_no_op(ast_node * ast) {
}

cql_noexport void extract_base_path_without_extension(charbuf *_Nonnull output,
    CSTR _Nonnull file_name) {
  size_t file_name_length = strlen(file_name);

  Contract(file_name_length > 0);

  char* last_slash_ptr = strrchr(file_name, '/');

  // Slash cannot be the last character of the string
  if (last_slash_ptr == &file_name[file_name_length - 1] ) {
    cql_error("badly formed file name (trailing '/') '%s'\n", file_name);
    cql_cleanup_and_exit(1);
  }

  size_t begin_index = last_slash_ptr ? (size_t)(last_slash_ptr - file_name) + 1 : 0;

  char* last_dot_ptr = strrchr(file_name, '.');

  // Dot cannot be the last character of the string
  if (last_dot_ptr == &file_name[file_name_length - 1] ) {
    cql_error("badly formed file name (trailing '.') '%s'\n", file_name);
    cql_cleanup_and_exit(1);
  }

  size_t end_index = file_name_length;
  if (last_dot_ptr) {
    size_t last_dot_index = (size_t)(last_dot_ptr - file_name);

    if (last_dot_index >= begin_index) {
      end_index = last_dot_index;
    }
  }

  if (end_index == begin_index) {
    cql_error("badly formed file name (empty base name) '%s'\n", file_name);
    cql_cleanup_and_exit(1);
  }

  for (size_t i = begin_index; i < end_index; i++) {
    bputc(output, file_name[i]);
  }
}

cql_noexport void cql_exit_on_semantic_errors(ast_node *head) {
  if (head && is_error(head)) {
    cql_error("semantic errors present; no code gen.\n");
    cql_cleanup_and_exit(1);
  }
}

cql_noexport void exit_on_no_global_proc() {
  if (!global_proc_name) {
    cql_error("There are global statements but no global proc name was specified (use --global_proc)\n");
    cql_cleanup_and_exit(1);
  }
}

// This callback causes select * to be expanded into the appropriate literal
// column names.  We don't do this expansion for exists or not exists because
// the columns don't matter.
//
// The tree shape is:
//
// |       | | {exists_expr}: bool notnull
// |       |   | {select_stmt}: select: { id: integer notnull }
// |       |     | {select_core}: select: { id: integer notnull }
// |       |     | | {select_expr_list_con}: select: { id: integer notnull }
// |       |     |   | {select_expr_list}: select: { id: integer notnull }
// --
// or
// --
// {sem_select_stmt}: union: { A: integer }
// | {select_core_list}: union: { A: integer }
// | | {select_core}: select: { A: integer variable }
// | | | {select_expr_list_con}: select: { A: integer variable }
// | |   | {select_expr_list}: select: { A: integer variable }
// | |   | | ...
// | | {select_core_compound}
// |   | {int 2}
// |   | {select_core_list}: union: { A: integer }
// |     | {select_core}: select: { A: integer variable }
// |     | | {select_expr_list_con}: select: { A: integer variable }
// |     |   | {select_expr_list}: select: { A: integer variable }
// |     |   | | ...
// |     | {select_core_compound}
// |       | {int 2}
// |       | {select_core}: select: { A: integer variable }
// |         | {select_expr_list_con}: select: { A: integer variable }
// |           | {select_expr_list}: select: { A: integer variable }
// |           | | ...
bool_t cg_expand_star(ast_node *_Nonnull ast, void *_Nullable context, charbuf *_Nonnull buffer) {
  Contract(is_ast_star(ast) || is_ast_table_star(ast));
  Contract(ast->sem);
  Contract(!is_error(ast));  // already "ok" at least
  Contract(is_struct(ast->sem->sem_type));

  if (is_ast_star(ast)) {
    EXTRACT(select_expr_list, ast->parent);
    EXTRACT(select_expr_list_con, select_expr_list->parent);
    EXTRACT(select_core, select_expr_list_con->parent);
    EXTRACT_ANY(any_select_core, select_core->parent);

    while (!is_ast_select_stmt(any_select_core)) {
      any_select_core = any_select_core->parent;
    }
    EXTRACT_ANY_NOTNULL(select_context, any_select_core->parent);

    if (is_ast_exists_expr(select_context)) {
      return false;
    }

    sem_struct *sptr = ast->sem->sptr;
    uint32_t count = sptr->count;

    bool_t first = true;
    for (uint32_t i = 0; i < count; i++) {
      if (!first) {
        bprintf(buffer, ", ");
      }
      if (!(sptr->semtypes[i] & SEM_TYPE_HIDDEN_COL)) {
        first = false;
        bprintf(buffer, "%s", sptr->names[i]);
      }
    }
  }
  else {
    Invariant(is_ast_table_star(ast));
    sem_struct *sptr = ast->sem->sptr;
    uint32_t count = sptr->count;

    bool_t first = true;
    for (uint32_t i = 0; i < count; i++) {
      if (!first) {
        bprintf(buffer, ", ");
      }
      if (!(sptr->semtypes[i] & SEM_TYPE_HIDDEN_COL)) {
        first = false;
        bprintf(buffer, "%s.%s", ast->sem->name, sptr->names[i]);
      }
    }
  }

  return true;
}

// Recursively finds table nodes, executing the callback for each that is found.  The
// callback will not be executed more than once for the same table name.
static void find_table_node(table_callbacks *callbacks, ast_node *node) {
  // Check the type of node so that we can find the direct references to tables. We
  // can't know the difference between a table or view in the ast, so we will need to
  // later find the definition to see if it points to a create_table_stmt to distinguish
  // from views.

  find_ast_str_node_callback alt_callback = NULL;
  symtab *alt_visited = NULL;
  ast_node *table_or_view_name_ast = NULL;

  if (is_ast_declare_cursor_like_select(node)) {
    // There is a select in this declaration but it doesn't really run, it's just type info
    // so that doesn't count.  So we don't recurse here.
    return;
  }
  else if (is_ast_table_or_subquery(node)) {
    EXTRACT_ANY_NOTNULL(factor, node->left);
    if (is_ast_str(factor)) {
      // the other table factor cases (there are several) do not have a string payload
      table_or_view_name_ast = factor;
      alt_callback = callbacks->callback_from;
      alt_visited = callbacks->visited_from;
    }
  }
  else if (is_ast_delete_stmt(node)) {
    EXTRACT_ANY_NOTNULL(name_ast, node->left);
    table_or_view_name_ast = name_ast;
    alt_callback = callbacks->callback_deletes;
    alt_visited = callbacks->visited_delete;
  }
  else if (is_ast_insert_stmt(node)) {
    EXTRACT(name_columns_values, node->right);
    EXTRACT_ANY_NOTNULL(name_ast, name_columns_values->left);
    table_or_view_name_ast = name_ast;
    alt_callback = callbacks->callback_inserts;
    alt_visited = callbacks->visited_insert;
  }
  else if (is_ast_update_stmt(node)) {
    EXTRACT_ANY(name_ast, node->left);
    // name_ast node is NULL if update statement is part of an upsert statement
    if (name_ast) {
      table_or_view_name_ast = name_ast;
      alt_callback = callbacks->callback_updates;
      alt_visited = callbacks->visited_update;
    }
  }
  else if (callbacks->callback_proc) {
    if (is_ast_call_stmt(node) | is_ast_call(node)) {
      // Both cases have the name in the node left so we can consolidate
      // the check to see if it's a proc is redundant in the call_stmt case
      // but it lets us share code so we just go with it.  The other case
      // is a possible proc_as_func call so we must check if the target is a proc.

      EXTRACT_ANY_NOTNULL(name_ast, node->left);
      EXTRACT_STRING(name, name_ast);
      ast_node *proc = find_proc(name);
      if (proc) {
        EXTRACT_STRING(canon_name, get_proc_name(proc));
        if (symtab_add(callbacks->visited_proc, canon_name, name_ast)) {
          callbacks->callback_proc(canon_name, name_ast, callbacks->callback_context);
        }
      }
    }
  }

  if (table_or_view_name_ast) {
    // Find the definition and see if we have a create_table_stmt.
    EXTRACT_STRING(table_or_view_name, table_or_view_name_ast);
    ast_node *table_or_view = find_table_or_view_even_deleted(table_or_view_name);

    // Make sure we don't process a table or view that we've already processed.
    if (table_or_view) {
      if (is_ast_create_table_stmt(table_or_view)) {
        EXTRACT(create_table_name_flags, table_or_view->left);
        EXTRACT_STRING(canonical_name, create_table_name_flags->right);

        // Found a table, execute the callback.
        if (symtab_add(callbacks->visited_any_table, canonical_name, table_or_view)) {
          callbacks->callback_any_table(canonical_name, table_or_view, callbacks->callback_context);
        }

        // Emit the second callback if any.
        if (alt_callback && symtab_add(alt_visited, canonical_name, table_or_view)) {
          alt_callback(canonical_name, table_or_view, callbacks->callback_context);
        }
      } else {
        Contract(is_ast_create_view_stmt(table_or_view));
        EXTRACT_NOTNULL(view_and_attrs, table_or_view->right);
        EXTRACT_NOTNULL(name_and_select, view_and_attrs->left);
        EXTRACT_STRING(canonical_name, name_and_select->left);

        if (symtab_add(callbacks->visited_any_table, canonical_name, table_or_view)) {
          // Report the view itself
          if (callbacks->callback_any_view) {
            callbacks->callback_any_view(canonical_name, table_or_view, callbacks->callback_context);
          }

          // Look through the view definition for tables. Just call through recursively.
          find_table_node(callbacks, table_or_view);
        }
      }
    }
  }

  // Check the left and right nodes.
  if (ast_has_left(node)) {
    find_table_node(callbacks, node->left);
  }

  if (ast_has_right(node)) {
    find_table_node(callbacks, node->right);
  }
}

// Find references in a proc and invoke the corresponding callback on them
// this is useful for dependency analysis.
cql_noexport void find_table_refs(table_callbacks *callbacks, ast_node *node) {
  // Each kind of callback needs its own symbol table because, for instance,
  // you might see a table as an insert and also as an update. If we use
  // a single visited table like we used to then the second kind of usage would
  // not get recorded.

  // Note: we don't need a seperate table for visiting views and visiting tables
  // any given name can only be a view or a table, never both.
  callbacks->visited_any_table = symtab_new();
  callbacks->visited_insert = symtab_new();
  callbacks->visited_update = symtab_new();
  callbacks->visited_delete = symtab_new();
  callbacks->visited_from = symtab_new();
  callbacks->visited_proc = symtab_new();

  find_table_node(callbacks, node);

  SYMTAB_CLEANUP(callbacks->visited_any_table);
  SYMTAB_CLEANUP(callbacks->visited_insert);
  SYMTAB_CLEANUP(callbacks->visited_update);
  SYMTAB_CLEANUP(callbacks->visited_delete);
  SYMTAB_CLEANUP(callbacks->visited_from);
  SYMTAB_CLEANUP(callbacks->visited_proc);
}


// Produce a crc of a given charbuf using the CRC helpers.
cql_noexport crc_t crc_charbuf(charbuf *input) {
  crc_t crc = crc_init();
  crc = crc_update(crc, (const unsigned char *)input->ptr, input->used);
  return crc_finalize(crc);
}

cql_noexport void cg_common_cleanup() {
  SYMTAB_CLEANUP(cg_stmts);
  SYMTAB_CLEANUP(cg_funcs);
  SYMTAB_CLEANUP(cg_exprs);

  CLEANUP_CHARBUF_REF(cg_header_output);
  CLEANUP_CHARBUF_REF(cg_main_output);
  CLEANUP_CHARBUF_REF(cg_fwd_ref_output);
  CLEANUP_CHARBUF_REF(cg_constants_output);
  CLEANUP_CHARBUF_REF(cg_declarations_output);
  CLEANUP_CHARBUF_REF(cg_scratch_vars_output);
  CLEANUP_CHARBUF_REF(cg_cleanup_output);
  CLEANUP_CHARBUF_REF(cg_fragments_output)
}

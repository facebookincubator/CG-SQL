/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Assorted definitions for the CQL abstract syntax tree

#define AST_EMIT_DEFS 1

#include "cql.h"
#include "minipool.h"
#include "ast.h"
#include "sem.h"
#include "gen_sql.h"
#include "cg_common.h"
#include "encoders.h"

cql_data_defn( minipool *ast_pool );
cql_data_defn( minipool *str_pool );
cql_data_defn( char *_Nullable current_file );

// Helper object to just hold info in find_attribute_str(...)
typedef struct misc_attrs_type {
  CSTR attribute_name;
  void * context;
  find_ast_str_node_callback str_node_callback;
  uint32_t count;
} misc_attrs_type;

cql_noexport void ast_init() {
  minipool_open(&ast_pool);
  minipool_open(&str_pool);
}

cql_noexport void ast_cleanup() {
  minipool_close(&ast_pool);
  minipool_close(&str_pool);
  run_lazy_frees();
}

cql_noexport void ast_set_rewrite_info(int32_t lineno, CSTR filename) {
  yylineno = lineno;
  current_file = (char *)filename;
}

cql_noexport void ast_reset_rewrite_info() {
  yylineno = -1;
  current_file = NULL;
}

cql_noexport bool_t is_ast_num(ast_node *node) {
  return node && (node->type == k_ast_num);
}

cql_noexport bool_t is_ast_int(ast_node *node) {
  return node && (node->type == k_ast_int);
}

cql_noexport bool_t is_ast_str(ast_node *node) {
  return node && (node->type == k_ast_str);
}

cql_noexport bool_t is_ast_blob(ast_node *node) {
  return node && (node->type == k_ast_blob);
}
  
cql_noexport bool_t is_at_rc(ast_node *node) {
  return is_ast_str(node) && !Strcasecmp("@RC", ((str_ast_node*)node)->value);
}

cql_noexport bool_t is_proclit(ast_node *node) {
  return is_ast_str(node) && !Strcasecmp("@proc", ((str_ast_node*)node)->value);
}

cql_noexport bool_t is_strlit(ast_node *node) {
  return is_ast_str(node) && ((str_ast_node *)node)->value[0] == '\'';
}

cql_noexport bool_t is_id(ast_node *node) {
  return is_ast_str(node) && ((str_ast_node *)node)->value[0] != '\'';
}

cql_noexport bool_t is_id_or_dot(ast_node *node) {
  return is_id(node) || is_ast_dot(node);
}

cql_noexport bool_t is_primitive(ast_node *node) {
  return is_ast_num(node) || is_ast_str(node) || is_ast_blob(node) || is_ast_int(node);
}

cql_noexport bool_t is_proc(ast_node *node) {
  return is_ast_create_proc_stmt(node) || is_ast_declare_proc_stmt(node);
}

cql_noexport bool_t is_region(ast_node *ast) {
  return is_ast_declare_schema_region_stmt(ast) || is_ast_declare_deployable_region_stmt(ast);
}

cql_noexport bool_t is_select_stmt(ast_node *ast) {
  return is_ast_select_stmt(ast) ||
         is_ast_explain_stmt(ast) ||
         is_ast_with_select_stmt(ast);
}

cql_noexport bool_t is_delete_stmt(ast_node *ast) {
  return is_ast_delete_stmt(ast) ||
         is_ast_with_delete_stmt(ast);
}

cql_noexport bool_t is_update_stmt(ast_node *ast) {
  return is_ast_update_stmt(ast) ||
         is_ast_with_update_stmt(ast);
}

cql_noexport bool_t is_insert_stmt(ast_node *ast) {
  return is_ast_insert_stmt(ast) ||
         is_ast_with_insert_stmt(ast) ||
         is_ast_upsert_stmt(ast) ||
         is_ast_with_upsert_stmt(ast);
}

cql_noexport bool_t ast_has_left(ast_node *node) {
  if (is_primitive(node)) {
    return false;
  }
  return (node->left != NULL);
}

cql_noexport bool_t ast_has_right(ast_node *node) {
  if (is_primitive(node)) {
    return false;
  }
  return (node->right != NULL);
}

cql_noexport void ast_set_right(ast_node *parent, ast_node *right)  {
  parent->right = right;
  if (right) {
    right->parent = parent;
  }
}

cql_noexport void ast_set_left(ast_node *parent, ast_node *left) {
  parent->left = left;
  if (left) {
    left->parent = parent;
  }
}


cql_noexport ast_node *new_ast(const char *type, ast_node *left, ast_node *right) {
  Contract(current_file && yylineno > 0);
  ast_node *ast = _ast_pool_new(ast_node);
  ast->type = type;
  ast->left = left;
  ast->right = right;
  ast->lineno = yylineno;
  ast->filename = current_file;
  ast->sem = NULL;

  if (left) left->parent = ast;
  if (right) right->parent = ast;

  return ast;
}

cql_noexport ast_node *new_ast_opt(int32_t value) {
  Contract(current_file && yylineno > 0);
  int_ast_node *iast = _ast_pool_new(int_ast_node);
  iast->type = k_ast_int;
  iast->value = value;
  iast->lineno = yylineno;
  iast->filename = current_file;
  iast->sem = NULL;
  return (ast_node *)iast;
}

cql_noexport ast_node *new_ast_str(CSTR value) {
  Contract(current_file && yylineno > 0);
  str_ast_node *sast = _ast_pool_new(str_ast_node);
  sast->type = k_ast_str;
  sast->value = value;
  sast->lineno = yylineno;
  sast->filename = current_file;
  sast->sem = NULL;
  sast->cstr_literal = false;
  return (ast_node *)sast;
}

cql_noexport ast_node *new_ast_num(int32_t num_type, CSTR value) {
  Contract(current_file && yylineno > 0);
  Contract(value);
  num_ast_node *nast = _ast_pool_new(num_ast_node);
  nast->type = k_ast_num;
  nast->value = value;
  nast->lineno = yylineno;
  nast->filename = current_file;
  nast->sem = NULL;
  nast->num_type = num_type;
  Contract(nast->value);
  return (ast_node *)nast;
}

cql_noexport ast_node *new_ast_blob(CSTR value) {
  Contract(current_file && yylineno > 0);
  str_ast_node *sast = _ast_pool_new(str_ast_node);
  sast->type = k_ast_blob;
  sast->value = value;
  sast->lineno = yylineno;
  sast->filename = current_file;
  sast->sem = NULL;
  sast->cstr_literal = false;
  return (ast_node *)sast;
}

// Get the compound operator name. crash if compound operation integer is
// invalid
cql_noexport CSTR get_compound_operator_name(int32_t compound_operator) {
  CSTR result = NULL;

  switch (compound_operator) {
    case COMPOUND_OP_EXCEPT:
      result = "EXCEPT";
      break;
    case COMPOUND_OP_INTERSECT:
      result = "INTERSECT";
      break;
    case COMPOUND_OP_UNION:
      result = "UNION";
      break;
    case COMPOUND_OP_UNION_ALL:
      result = "UNION ALL";
      break;
  }

  Invariant(result);
  return result;
}

// This converts C string literal syntax into SQL string literal syntax
// the test of the program expects the SQL style literals.  We support
// C style literals largely because they pass through the C pre-processor better.
// Even stuff like the empty string '' causes annoying warnings.  However
// the escaping is lightly different.  Also C string literals have useful escape sequences
cql_noexport CSTR convert_cstrlit(CSTR cstr) {
  CHARBUF_OPEN(decoded);
  CHARBUF_OPEN(encoded);
  cg_decode_c_string_literal(cstr, &decoded);
  cg_encode_string_literal(decoded.ptr, &encoded);

  CSTR result = Strdup(encoded.ptr);
  CHARBUF_CLOSE(encoded);
  CHARBUF_CLOSE(decoded);
  return result;
}

cql_noexport ast_node *new_ast_cstr(CSTR value) {
  value = convert_cstrlit(value);
  str_ast_node *sast = (str_ast_node *)new_ast_str(value);
  sast->cstr_literal = true;
  return (ast_node *)sast;
}

static char padbuffer[4096];

cql_noexport bool_t print_ast_value(struct ast_node *node) {
  bool_t ret = false;

  if (is_ast_str(node)) {
    cql_output("%s", padbuffer);
    if (is_strlit(node)) {
      cql_output("{strlit %s}", ((struct str_ast_node *)node)->value);
    }
    else {
      cql_output("{name %s}", ((struct str_ast_node *)node)->value);
    }
    ret = true;
  }

  if (is_ast_num(node)) {
    cql_output("%s", padbuffer);

    EXTRACT_NUM_TYPE(num_type, node);
    EXTRACT_NUM_VALUE(val, node);

    if (num_type == NUM_INT) {
      cql_output("{int %s}", val);
    }
    else if (num_type == NUM_LONG) {
      cql_output("{longint %s}", val);
    }
    else if (num_type == NUM_REAL) {
      cql_output("{dbl %s}", val);
    }
    ret = true;
  }

  if (is_ast_blob(node)) {
    cql_output("%s", padbuffer);
    cql_output("{blob %s}", ((struct str_ast_node *)node)->value);
    ret = true;
  }

  if (is_ast_int(node)) {
    cql_output("%s", padbuffer);
    cql_output("{int %lld}", (llint_t)((struct int_ast_node *)node)->value);
    ret = true;
  }

  if (ret && node->sem) {
    cql_output(": ");
    print_sem_type(node->sem);
  }

  if (ret) {
    cql_output("\n");
  }

  return ret;
}

cql_noexport void print_ast_type(ast_node *node) {
  cql_output("%s", padbuffer);
  cql_output("{%s}", node->type);
  if (node->sem) {
    cql_output(": ");
    print_sem_type(node->sem);
  }
  cql_output("\n");
}

// Helper function to get the parameters node out of the ast for a proc.
cql_noexport ast_node *get_proc_params(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast) || is_ast_declare_proc_stmt(ast));
  // works for both
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  return params;
}

// Helper function to get the proc name from a declare_proc_stmt or create_proc_stmt
cql_noexport ast_node *get_proc_name(ast_node *ast) {
  if (is_ast_create_proc_stmt(ast)) {
    return ast->left;
  }

  Contract(is_ast_declare_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_name_type, ast->left);
  return proc_name_type->left;
}

// Helper function to get the parameters node out of the ast for a func.
cql_noexport ast_node *get_func_params(ast_node *func_stmt) {
  Contract(is_ast_declare_func_stmt(func_stmt) || is_ast_declare_select_func_stmt(func_stmt));
  EXTRACT_NOTNULL(func_params_return, func_stmt->right);
  EXTRACT(params, func_params_return->left);
  return params;
}

// Helper function to extract the list of attribute.
// Walk through a misc_attrs node and call the callbacks :
//  - find_ast_misc_attr_callback if misc_attr node is found
// Let's take the example below and see what values will be passed to callbacks
// e.g:
//  @attribute(cql:foo=(baa, (name, 'nelly')))
//  @attribute(cql:base=raoul)
//  create procedure sample()
//  begin
//    select * from baa;
//  end;
//
//  1- find_ast_misc_attr_callback ("cql", "foo", <(baa, (name, 'nelly'))>, <context>)
//  2- find_ast_misc_attr_callback("cql", "foo", <raoul>, <context>)
//  3- End
cql_noexport void find_misc_attrs(
  ast_node *_Nullable ast_misc_attrs,
  find_ast_misc_attr_callback _Nonnull misc_attr_callback,
  void *_Nullable context)
{
  Contract(is_ast_misc_attrs(ast_misc_attrs));

  for (ast_node *misc_attrs = ast_misc_attrs; misc_attrs; misc_attrs = misc_attrs->right) {
    Invariant(is_ast_misc_attrs(misc_attrs));
    ast_node *misc_attr = misc_attrs->left;
    ast_node *misc_attr_key = misc_attr->left;
    ast_node *values = misc_attr->right;
    CSTR misc_attr_prefix = NULL;
    CSTR misc_attr_name = NULL;
    if (is_ast_dot(misc_attr_key)) {
      misc_attr_prefix = ((str_ast_node *)misc_attr_key->left)->value;
      misc_attr_name = ((str_ast_node *)misc_attr_key->right)->value;
    } else {
      misc_attr_name = ((str_ast_node *)misc_attr_key)->value;
    }
    misc_attr_callback(misc_attr_prefix, misc_attr_name, values, context);
  }
}

// This callback helper dispatches matching string or list of string values
// for the indicated cql:attribute_name.  Non-string values are ignored in
// this path.  Note that the attribute might be badly formed hence there are
// few Contract enforcement here.  We can't crash if the value is unexpected
// we just don't recognize it as properly attributed for whatever (e.g. it just
// isn't a base fragment decl if it has an integer value for the fragment name)
static void ast_find_ast_misc_attr_callback(
  CSTR misc_attr_prefix,
  CSTR misc_attr_name,
  ast_node *ast_misc_attr_values,
  void *_Nullable context)
{
  misc_attrs_type* misc = (misc_attrs_type*) context;

  // First make sure that there is a prefix and name and that they match
  if (misc_attr_prefix &&
      misc_attr_name &&
      !Strcasecmp(misc_attr_prefix, "cql") &&
      !Strcasecmp(misc_attr_name, misc->attribute_name)) {

    // The attribute value might be a string or a list of strings.
    // Non-string, non-list attributes are ignored for this callback type
    if (is_ast_str(ast_misc_attr_values)) {
      EXTRACT_STRING(name, ast_misc_attr_values);
      if (misc->str_node_callback) {
        misc->str_node_callback(name, ast_misc_attr_values, misc->context);
      }
      misc->count++;
    }
    else if (is_ast_misc_attr_value_list(ast_misc_attr_values)) {
      for (ast_node *list = ast_misc_attr_values; list; list = list->right) {
        // any non-string values are ignored, loop over the rest calling on each string
        if (is_ast_str(list->left)) {
          EXTRACT_STRING(name, list->left);
          if (misc->str_node_callback) {
            misc->str_node_callback(name, list->left, misc->context);
          }
          misc->count++;
        }
      }
    }
  }
}

// Helper function to extract the specified string type attribute (if any) from the misc attributes
// provided, and invoke the callback function
cql_noexport uint32_t find_attribute_str(
  ast_node *_Nullable misc_attr_list,
  find_ast_str_node_callback _Nullable callback,
  void *_Nullable context,
  const char *attribute_name)
{
  Contract(is_ast_misc_attrs(misc_attr_list));

  misc_attrs_type misc = {
    .str_node_callback = callback,
    .context = context,
    .attribute_name = attribute_name,
    .count = 0,
  };

  find_misc_attrs(misc_attr_list, ast_find_ast_misc_attr_callback, &misc);
  return misc.count;
}

// This callback helper tests only if the attribute matches the search condition
// the value is irrelevant for this type of attribute
static void ast_exists_ast_misc_attr_callback(
  CSTR misc_attr_prefix,
  CSTR misc_attr_name,
  ast_node *ast_misc_attr_values,
  void *_Nullable context)
{
  misc_attrs_type* misc = (misc_attrs_type*) context;

  // First make sure that there is a prefix and name and that they match
  if (misc_attr_prefix &&
      misc_attr_name &&
      !Strcasecmp(misc_attr_prefix, "cql") &&
      !Strcasecmp(misc_attr_name, misc->attribute_name)) {
          misc->count++;
  }
}

// Helper function to extract the specified string type attribute (if any) from the misc attributes
// provided, and invoke the callback function
cql_noexport uint32_t exists_attribute_str(ast_node *_Nullable misc_attr_list, const char *attribute_name)
{
  if (!misc_attr_list) {
    return 0;
  }

  Contract(is_ast_misc_attrs(misc_attr_list));

  misc_attrs_type misc = {
    .str_node_callback = NULL,
    .context = NULL,
    .attribute_name = attribute_name,
    .count = 0,
  };

  find_misc_attrs(misc_attr_list, ast_exists_ast_misc_attr_callback, &misc);
  return misc.count;
}

// Helper function to extract the ok_table_scan nodes (if any) from the misc attributes
// provided, and invoke the callback function.
cql_noexport uint32_t find_ok_table_scan(
  ast_node *_Nonnull list,
  find_ast_str_node_callback _Nonnull callback,
  void *_Nullable context)
{
  return find_attribute_str(list, callback, context, "ok_table_scan");
}

// Helper function to extract the auto-drop nodes (if any) from the misc attributes
// provided, and invoke the callback function.
cql_noexport uint32_t find_autodrops(
  ast_node *_Nonnull list,
  find_ast_str_node_callback _Nonnull callback,
  void *_Nullable context)
{
  return find_attribute_str(list, callback, context, "autodrop");
}

// Helper function to extract the identity columns (if any) from the misc attributes
// provided, and invoke the callback function
cql_noexport uint32_t find_identity_columns(
  ast_node *_Nullable misc_attr_list,
  find_ast_str_node_callback _Nonnull callback,
  void *_Nullable context)
{
  return find_attribute_str(misc_attr_list, callback, context, "identity");
}

// Helper function to extract the base fragment node (if any) from the misc attributes
// provided, and invoke the callback function.
cql_noexport uint32_t find_base_fragment_attr(
  ast_node *_Nullable misc_attr_list,
  find_ast_str_node_callback _Nullable callback,
  void *_Nullable context)
{
  return find_attribute_str(misc_attr_list, callback, context, "base_fragment");
}

// Helper function to extract the extension fragment node (if any) from the misc attributes
// provided, and invoke the callback function.
cql_noexport uint32_t find_extension_fragment_attr(
  ast_node *_Nullable misc_attr_list,
  find_ast_str_node_callback _Nullable callback,
  void *_Nullable context)
{
  return find_attribute_str(misc_attr_list, callback, context, "extension_fragment");
}

// Helper function to extract the assembled fragment node (if any) from the misc attributes
// provided, and invoke the callback function.
cql_noexport uint32_t find_assembly_query_attr(
  ast_node *_Nullable misc_attr_list,
  find_ast_str_node_callback _Nullable callback,
  void *_Nullable context)
{
  return find_attribute_str(misc_attr_list, callback, context, "assembly_fragment");
}

// Keep record of the assembly query fragment for result set type reference if
// we are presently emitting an extension fragment stored proc
cql_data_defn ( CSTR base_fragment_name );

// The name of the base fragment is used as part of the field getters names
// we get it from the attribute associated with whichever fragment we're looking at
// semantic rules ensure these are consistent.  The base fragment name basically
// let's us tie together extensions so we know which ones are part of which query
// and then we put those together.  This function is the callback used to harvest
// the base fragment name from wherever we found it.  Each fragment has it.
static void cg_set_base_fragment_name(CSTR _Nonnull name, ast_node *_Nonnull _misc_attr, void *_Nullable _context) {
  base_fragment_name = name;
}

// Look for the assembly fragment annotations, return the type or MIXED if
// there is no unique type. This is used to find cases where the same attribute
// is present more than once or different/incompatible attributes are present
cql_noexport uint32_t find_fragment_attr_type(ast_node *_Nullable misc_attr_list) {
  if (!misc_attr_list) {
    base_fragment_name = NULL;
    return FRAG_TYPE_NONE;
  }

  uint32_t base = find_base_fragment_attr(misc_attr_list, cg_set_base_fragment_name, NULL);
  uint32_t extension = find_extension_fragment_attr(misc_attr_list, cg_set_base_fragment_name, NULL);
  uint32_t assembly = find_assembly_query_attr(misc_attr_list, cg_set_base_fragment_name, NULL);

  if (base + extension + assembly > 1) {
    return FRAG_TYPE_MIXED;
  }
  if (base + extension + assembly == 0) {
    return FRAG_TYPE_NONE;
  }
  if (base) {
    return FRAG_TYPE_BASE;
  }
  if (extension) {
    return FRAG_TYPE_EXTENSION;
  }
  Invariant(assembly);
  return FRAG_TYPE_ASSEMBLY;
}

cql_noexport void print_ast(ast_node *node, ast_node *parent, int32_t pad, bool_t flip) {
  if (pad == 0) {
    padbuffer[0] = '\0';
  }

  if (!node) {
    return;
  }

  if (print_ast_value(node)) {
    return;
  }

  if (is_ast_stmt_list(parent) && is_ast_stmt_list(node)) {
    print_ast(node->left, node, pad, !node->right);
    print_ast(node->right, node, pad, 1);
  }
  else {
    if (pad == 2) {
      cql_output("\n");

      if (parent && is_ast_stmt_list(parent)) {
        cql_output("The statement ending at line %d\n\n", node->lineno);
        gen_stmt_level = 1;

        EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, parent);

        if (misc_attrs) {
          gen_misc_attrs_to_stdout(misc_attrs);
        }

        gen_one_stmt_to_stdout(stmt);
        cql_output("\n");

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SEM)
        // sem off, nothing to print here
#else
        // print any error text
        if (stmt->sem && stmt->sem->sem_type == SEM_TYPE_ERROR && stmt->sem->error) {
          cql_output("%s\n", stmt->sem->error);
        }
#endif
      }
    }
    print_ast_type(node);
    if (flip && pad >= 2) {
      padbuffer[pad-2] = ' ';
    }
    if (pad == 0) {
      padbuffer[pad] = ' ';
    }
    else {
      padbuffer[pad] = '|';
    }
    padbuffer[pad+1] = ' ';
    padbuffer[pad+2] = '\0';
    print_ast(node->left, node, pad+2, !node->right);
    print_ast(node->right, node, pad+2, 1);
    padbuffer[pad] = '\0';
  }
}

// Clone a ast tree by the given root ast_node.
// The sems of all nodes are NULL in the new tree.
cql_noexport ast_node *copy_ast_tree(ast_node *_Nonnull node) {
  Contract(node);
  ast_node *new_node = NULL;

  // Note: ast_has_left includes the is_primitive check so it's safe to do on leaf nodes too
  // even though they have no left/right at all.

  ast_node *left_node = NULL;
  ast_node *right_node = NULL;
  if (ast_has_left(node)) {
    left_node = copy_ast_tree(node->left);
  }
  if (ast_has_right(node)) {
    right_node = copy_ast_tree(node->right);
  }

  AST_REWRITE_INFO_SET(node->lineno, node->filename);
  if (is_ast_num(node)) {
    EXTRACT_NUM_TYPE(num_type, node);
    EXTRACT_NUM_VALUE(val, node);
    new_node = new_ast_num(num_type, val);
  } else if (is_ast_int(node)) {
    EXTRACT_OPTION(value, node);
    new_node = new_ast_opt(value);
  } else if (is_ast_blob(node)) {
    EXTRACT_BLOBTEXT(value, node);
    new_node = new_ast_blob(value);
  } else if (is_ast_str(node)) {
    EXTRACT_STRING(value, node);
    new_node = new_ast_str(value);
  } else {
    new_node = new_ast(node->type, left_node, right_node);
  }
  AST_REWRITE_INFO_RESET();

  Invariant(new_node);
  return new_node;
}



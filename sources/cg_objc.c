/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Perform codegen of the various nodes to "Obj-C".

#include "cg_objc.h"

#include "ast.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "symtab.h"

// Set up the buffer for extension header outputs if we are emitting for an extension
// or assembly query since we will make decision on including it into codegen output
// if we are emitting for extension only that needs different reference calls to assembly
// query for referencing the aggregated resultset instead of regular calls from itself.
// Otherwise this additional output will be abandoned at program exit anyway.
static charbuf *objc_extension_header = NULL;

static uint32_t objc_frag_type;

static void cg_objc_proc_result_set_c_getter(
  bool_t fetch_proc,
  charbuf *buffer,
  CSTR name,
  CSTR col_name,
  CSTR sym_suffix,
  bool_t is_private)
{
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    impl_symbol_prefix,
    is_private ? "__PRIVATE__" : "",
    rt->impl_symbol_prefix);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    col_getter_sym,
    impl_symbol_prefix.ptr,
    name,
    "_get_",
    col_name,
    sym_suffix);

  bprintf(buffer, "%s(cResultSet%s)", col_getter_sym.ptr, fetch_proc ? "" : ", row");
  CHARBUF_CLOSE(col_getter_sym);
  CHARBUF_CLOSE(impl_symbol_prefix);
}

static void cg_objc_proc_result_set_getter(
  bool_t fetch_proc,
  CSTR name,
  CSTR col_name,
  CSTR objc_name,
  CSTR c_result_set_ref,
  CSTR c_convert,
  uint32_t col,
  sem_t sem_type,
  charbuf *output)
{
  Contract(is_unitary(sem_type));
  sem_t core_type = core_type_of(sem_type);
  Contract(core_type != SEM_TYPE_NULL);
  Contract(cg_main_output);

  bool_t nullable = is_nullable(sem_type);
  CSTR return_type;
  CSTR return_type_separator = "";

  CSTR value_convert_begin = "";
  CSTR value_convert_end = "";
  CSTR c_getter_suffix = "";

  uint32_t col_count_for_base = 0;

  bool_t is_ext = objc_frag_type == FRAG_TYPE_EXTENSION;
  bool_t is_asm_frag = objc_frag_type == FRAG_TYPE_ASSEMBLY;

  if (objc_frag_type != FRAG_TYPE_NONE) {
    // we already know the base compiled with no errors
    ast_node *base_proc = find_base_fragment(base_fragment_name);
    Invariant(base_proc);
    Invariant(base_proc->sem);
    Invariant(base_proc->sem->sptr);

    col_count_for_base = base_proc->sem->sptr->count;
  }

  bool_t is_private = col >= col_count_for_base && col_count_for_base > 0
    && is_asm_frag;

  CHARBUF_OPEN(value);

  if (nullable) {
    switch (core_type) {
      case SEM_TYPE_INTEGER:
      case SEM_TYPE_LONG_INTEGER:
      case SEM_TYPE_REAL:
      case SEM_TYPE_BOOL:
        return_type = "NSNumber *_Nullable";
        return_type_separator = " ";
        value_convert_begin = "@(";
        value_convert_end = ")";
        c_getter_suffix = "_value";
        cg_objc_proc_result_set_c_getter(fetch_proc, &value, name, col_name, "_is_null", is_private);
        bprintf(&value, " ? nil : ");
        break;
      case SEM_TYPE_BLOB:
        return_type = "NSData *_Nullable";
        return_type_separator = " ";
        value_convert_begin = "(__bridge NSData *)";
        break;
      case SEM_TYPE_TEXT:
        return_type = "NSString *_Nullable";
        return_type_separator = " ";
        value_convert_begin = "(__bridge NSString *)";
        break;
      case SEM_TYPE_OBJECT:
        return_type = "NSObject *_Nullable";
        return_type_separator = " ";
        value_convert_begin = "(__bridge NSObject *)";
        break;
    }
  } else {
    switch (core_type) {
      case SEM_TYPE_INTEGER:
        return_type_separator = " ";
        return_type = rt->cql_int32;
        break;
      case SEM_TYPE_LONG_INTEGER:
        return_type_separator = " ";
        return_type = rt->cql_int64;
        break;
      case SEM_TYPE_REAL:
        return_type_separator = " ";
        return_type = rt->cql_double;
        break;
      case SEM_TYPE_BOOL:
        return_type_separator = " ";
        return_type = rt->cql_bool;
        value_convert_end = " ? YES : NO";
        break;
      case SEM_TYPE_TEXT:
        return_type = rt->cql_string_ref;
        value_convert_begin = "(__bridge NSString *)";
        break;
      case SEM_TYPE_BLOB:
        return_type = rt->cql_blob_ref;
        value_convert_begin = "(__bridge NSData *)";
        break;
      case SEM_TYPE_OBJECT:
        return_type = rt->cql_object_ref;
        return_type_separator = " ";
        value_convert_begin = "(__bridge NSObject *)";
        break;
    }
  }

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    impl_symbol_prefix,
    is_private ? "__PRIVATE__" : "",
    rt->impl_symbol_prefix);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    symbol_prefix,
    is_private ? "__PRIVATE__" : "",
    rt->symbol_prefix);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    objc_getter,
    // only prefix with __PRIVATE__ the private assembly methods
    is_ext ? rt->symbol_prefix : symbol_prefix.ptr,
    name,
    "_get_",
    col_name);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_getter,
                                  impl_symbol_prefix.ptr,
                                  name,
                                  "_get_",
                                  col_name,
                                  c_getter_suffix);

  if (fetch_proc) {
    bprintf(&value, "%s%s(cResultSet)%s",
            value_convert_begin,
            c_getter.ptr,
            value_convert_end);
  }
  else {
    bprintf(&value, "%s%s(cResultSet, row)%s",
            value_convert_begin,
            c_getter.ptr,
            value_convert_end);
  }

  if (is_ext) {
    // Since the parent assembly query has already fetched the aggregated resultSet, we just emit
    // a call to the parent for extension fragment rather than doing a regular getter call.
    CG_CHARBUF_OPEN_SYM(objc_parent_name, base_fragment_name);
    bprintf(output,
            "\nstatic inline %s%s%s(%s *resultSet, %s row)\n",
            return_type,
            return_type_separator,
            objc_getter.ptr,
            objc_parent_name.ptr,
            rt->cql_int32);
    CHARBUF_CLOSE(objc_parent_name);
    bprintf(output, "{\n");
  }
  else {
    if (fetch_proc) {
      bprintf(output,
              "\nstatic inline %s%s%s(%s *resultSet)\n",
              return_type,
              return_type_separator,
              objc_getter.ptr,
              objc_name);
    }
    else {
      bprintf(output,
              "\nstatic inline %s%s%s(%s *resultSet, %s row)\n",
              return_type,
              return_type_separator,
              objc_getter.ptr,
              objc_name,
              rt->cql_int32);
    }
    bprintf(output, "{\n");
  }
  bprintf(output, "  %s cResultSet = %s(resultSet);\n", c_result_set_ref, c_convert);
  bprintf(output, "  return %s;\n", value.ptr);
  bprintf(output, "}\n");

  CHARBUF_CLOSE(c_getter);
  CHARBUF_CLOSE(objc_getter);
  CHARBUF_CLOSE(symbol_prefix);
  CHARBUF_CLOSE(impl_symbol_prefix);
  CHARBUF_CLOSE(value);
}

static void cg_objc_proc_result_set(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  Contract(is_struct(ast->sem->sem_type));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT_STRING(name, ast->left);
  EXTRACT_MISC_ATTRS(ast, misc_attrs);

  // if getters are suppressed the entire class is moot
  // if result set is suppressed the entire class is moot
  // private implies result set suppressed so also moot
  bool_t suppressed =
    exists_attribute_str(misc_attrs, "suppress_getters") ||
    exists_attribute_str(misc_attrs, "suppress_result_set") ||
    exists_attribute_str(misc_attrs, "private");

  if (suppressed) {
    return;
  }

  Invariant(!use_vault);
  Invariant(!vault_columns);
  vault_columns = symtab_new();
  init_vault_info(misc_attrs, &use_vault, vault_columns);

  bool_t is_ext = objc_frag_type == FRAG_TYPE_EXTENSION;
  if (is_ext) {
    Contract(base_fragment_name);
  }

  CSTR c_result_set_name = is_ext ? base_fragment_name : name;
  charbuf *h = is_ext ? objc_extension_header : cg_header_output;

  CG_CHARBUF_OPEN_SYM(objc_name, name);
  CG_CHARBUF_OPEN_SYM(objc_result_set_name, c_result_set_name);
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_name, rt->impl_symbol_prefix, name);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_result_set, rt->impl_symbol_prefix, c_result_set_name, "_result_set");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    c_result_set_ref, rt->impl_symbol_prefix, c_result_set_name, "_result_set_ref");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_convert, "", c_name.ptr, "_from_", objc_name.ptr);

  CG_CHARBUF_OPEN_SYM(objc_class_name, c_result_set_name);
  // if extension we emit the base class name
  bprintf(h, "\n@class %s;\n", is_ext ? objc_class_name.ptr : objc_name.ptr);

  // Since the parent assembly query has already fetched the aggregated resultSet, we call to use that directly and
  // skip setting up objc and c bridging for extension fragment specific result unless otherwise
  if (!is_ext) {
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(objc_convert, "", objc_name.ptr, "_from_", c_name.ptr);

    bprintf(h, "\nstatic inline %s *%s(%s resultSet)\n", objc_name.ptr, objc_convert.ptr, c_result_set_ref.ptr);
    bprintf(h, "{\n");
    bprintf(h, "  return (__bridge %s *)resultSet;\n", objc_name.ptr);
    bprintf(h, "}\n");

    CHARBUF_CLOSE(objc_convert);
  }

  bprintf(
    h,
    "\nstatic inline %s %s(%s *resultSet)\n",
    c_result_set_ref.ptr,
    c_convert.ptr,
    is_ext ? objc_class_name.ptr : objc_name.ptr);
  bprintf(h, "{\n");
  bprintf(h, "  return (__bridge %s)resultSet;\n", c_result_set_ref.ptr);
  bprintf(h, "}\n");

  bool_t out_stmt_proc = has_out_stmt_result(ast);
  // extension fragments use SELECT and are incompatible with the single row result set form using OUT
  if (is_ext) {
    Invariant(!out_stmt_proc);
  }

  sem_struct *sptr = ast->sem->sptr;
  uint32_t count = sptr->count;
  for (uint32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR col = sptr->names[i];
    cg_objc_proc_result_set_getter(
      out_stmt_proc,
      name,
      col,
      objc_name.ptr,
      c_result_set_ref.ptr,
      c_convert.ptr,
      i,
      sem_type,
      h);
  }

  if (use_vault) {
    for (uint32_t i = 0; i < count; i++) {
      CSTR col = sptr->names[i];
      sem_t sem_type = sptr->semtypes[i];
      bool_t encode = should_vault_col(col, sem_type, use_vault, vault_columns);
      if (encode) {
        CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
            objc_getter, objc_name.ptr, "_get_", col, "_is_encoded");
        CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
            c_getter, c_name.ptr, "_get_", col, "_is_encoded");

        bprintf(h,
            "\nstatic inline %s %s(%s *resultSet)\n",
            rt->cql_bool,
            objc_getter.ptr,
            objc_result_set_name.ptr);
        bprintf(h, "{\n");
        if (is_ext) {
          CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
            objc_result_set_name_getter, objc_result_set_name.ptr, "_get_", col, "_is_encoded");
          bprintf(
              h, "  return %s(resultSet);\n", objc_result_set_name_getter.ptr);
          CHARBUF_CLOSE(objc_result_set_name_getter);
        }
        else {
          bprintf(h, "  return %s(%s(resultSet));\n", c_getter.ptr, c_convert.ptr);
        }
        bprintf(h, "}\n");

        CHARBUF_CLOSE(c_getter);
        CHARBUF_CLOSE(objc_getter);
      }
    }

    // Add a helper function that overrides CQL_DATA_TYPE_ENCODED bit of a resultset.
    // It's a debugging function that allow you to turn ON/OFF encoding/decoding when
    // your app is running.
    bprintf(h,
            "\nstatic inline void %sSetEncoding(%s col, %s encode)\n",
            objc_name.ptr,
            rt->cql_int32,
            rt->cql_bool);
    bprintf(h, "{\n");
    bprintf(h, "  return %sSetEncoding(col, encode);\n", c_name.ptr);
    bprintf(h, "}\n");
  }

  bprintf(h,
          "\nstatic inline %s %sResultCount(%s *resultSet)\n",
          rt->cql_int32,
          objc_name.ptr,
          objc_result_set_name.ptr);
  bprintf(h, "{\n");
  bprintf(h, "  return %sResultCount(%s(resultSet));\n", c_name.ptr, c_convert.ptr);
  bprintf(h, "}\n");

  bprintf(h,
          "\nstatic inline %s *%sCopy(%s *resultSet",
          objc_result_set_name.ptr,
          objc_name.ptr,
          objc_result_set_name.ptr);
  if (!out_stmt_proc) {
    bprintf(h,
            ", %s from, %s count",
            rt->cql_int32,
            rt->cql_int32);
  }
  bprintf(h, ")\n");
  bprintf(h, "{\n");
  bprintf(h, "  %s copy;\n", c_result_set_ref.ptr);
  bprintf(h,
          "  %sCopy(%s(resultSet), &copy%s);\n",
          c_name.ptr,
          c_convert.ptr,
          out_stmt_proc ? "" : ", from, count");
  bprintf(h, "  return (__bridge_transfer %s *)copy;\n", is_ext ? objc_class_name.ptr : objc_name.ptr);
  bprintf(h, "}\n");

  if (!is_ext) {
    bprintf(h,
            "\nstatic inline NSUInteger %s%sHash(%s *resultSet",
            objc_name.ptr,
            out_stmt_proc ? "" : "Row",
            objc_name.ptr);
    if (!out_stmt_proc) {
      bprintf(h, ", %s row", rt->cql_int32);
    }
    bprintf(h, ")\n");
    bprintf(h, "{\n");
    bprintf(h,
            "  return %s%sHash(%s(resultSet)%s);\n",
            c_name.ptr,
            out_stmt_proc ? "" : "Row",
            c_convert.ptr,
            out_stmt_proc ? "" : ", row");
    bprintf(h, "}\n");

    bprintf(h,
            "\nstatic inline BOOL %s%sEqual(%s *resultSet1",
            objc_name.ptr,
            out_stmt_proc ? "" : "Row",
            objc_name.ptr);
    if (!out_stmt_proc) {
      bprintf(h, ", %s row1", rt->cql_int32);
    }
    bprintf(h, ", %s *resultSet2", objc_name.ptr);
    if (!out_stmt_proc) {
      bprintf(h, ", %s row2", rt->cql_int32);
    }
    bprintf(h, ")\n");
    bprintf(h, "{\n");
    bprintf(h,
            "  return %s%sEqual(%s(resultSet1)%s, %s(resultSet2)%s);\n",
            c_name.ptr,
            out_stmt_proc ? "" : "Row",
            c_convert.ptr,
            out_stmt_proc ? "" : ", row1",
            c_convert.ptr,
            out_stmt_proc ? "" : ", row2");
    bprintf(h, "}\n");
  }

  CHARBUF_CLOSE(objc_class_name);
  CHARBUF_CLOSE(c_convert);
  CHARBUF_CLOSE(c_result_set_ref);
  CHARBUF_CLOSE(c_result_set);
  CHARBUF_CLOSE(c_name);
  CHARBUF_CLOSE(objc_result_set_name);
  CHARBUF_CLOSE(objc_name);

  use_vault = 0;
  symtab_delete(vault_columns);
  vault_columns = NULL;
}

static void cg_objc_create_proc_stmt(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  if (result_set_proc || out_stmt_proc || out_union_proc) {
    cg_objc_proc_result_set(ast);
  }
}

static void cg_objc_one_stmt(ast_node *stmt) {
  // DDL operations not in a procedure are ignored
  // but they can declare schema during the semantic pass
  if (is_ast_create_proc_stmt(stmt)) {
    cg_objc_create_proc_stmt(stmt);
  }
}

static void cg_objc_stmt_list(ast_node *head) {
  bool_t containsAssembly = false;
  symtab *assembly_query_names = symtab_new();
  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    // skiping the base fragment getters since generating in each extension
    // will cause colisions including two fragments headers
    objc_frag_type = find_fragment_attr_type(misc_attrs);
    if (objc_frag_type == FRAG_TYPE_BASE) {
      continue;
    }
    if (!containsAssembly) {
      // record if an assembly is present
      containsAssembly = objc_frag_type == FRAG_TYPE_ASSEMBLY;
    }
    if (objc_frag_type == FRAG_TYPE_EXTENSION) {
      // record the extension base name to build the imports
      symtab_add(assembly_query_names, base_fragment_name, NULL);
    }
    cg_objc_one_stmt(stmt);
  }

  // Include header outputs in extension buffer into final generation if this is for
  // extension only and disgard if this is for parent assembly query
  if (!containsAssembly) {
    bprintf(cg_header_output, "%s", objc_extension_header->ptr);
  }

  SYMTAB_CLEANUP(assembly_query_names);
}


static void cg_objc_init(void) {
  cg_common_init();
}

// Main entry point for code-gen.
cql_noexport void cg_objc_main(ast_node *head) {
  Invariant(options.file_names_count == 1);
  Invariant(options.objc_c_include_path != NULL);
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();
  cg_objc_init();

  CHARBUF_OPEN(extension_header);
  CHARBUF_OPEN(header_file);
  CHARBUF_OPEN(imports);

  objc_extension_header = &extension_header;

  bprintf(&header_file, "%s", rt->header_prefix);
  bprintf(&header_file, "\n#import <%s>\n", options.objc_c_include_path);

  // gen objc code ....
  cg_objc_stmt_list(head);

  bprintf(&header_file, "%s", rt->header_wrapper_begin);

  bprintf(&header_file, "%s", cg_header_output->ptr);
  bprintf(&header_file, "%s", rt->header_wrapper_end);

  CSTR header_file_name = options.file_names[0];
  cql_write_file(header_file_name, header_file.ptr);

  CHARBUF_CLOSE(imports);
  CHARBUF_CLOSE(header_file);
  CHARBUF_CLOSE(extension_header);

  // reset globals so they don't interfere with leaksan
  objc_extension_header = NULL;
}

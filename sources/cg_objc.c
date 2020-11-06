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

// Keep current and all records of the assembly query names for importing result set type
// if we are presently emitting for an extension fragment stored proc
static CSTR current_parent_query_name;

static symtab *assembly_query_names;

// Set up the buffer for extension header outputs if we are emitting for an extension
// or assembly query since we will make decision on including it into codegen output
// if we are emitting for extension only that needs different reference calls to assembly
// query for referencing the aggregated resultset instead of regular calls from itself.
// Otherwise this additional output will be abandoned at program exit anyway.
static charbuf *objc_extension_header = NULL;

// keep track of how many rows belong to the base query
static uint32_t col_count_for_base_objc = 0;

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
  bool_t is_private = col >= col_count_for_base_objc && col_count_for_base_objc > 0
    && (is_extension_fragment || is_assembly_query);

  CHARBUF_OPEN(value);

  if (nullable) {
    switch (core_type) {
      case SEM_TYPE_INTEGER:
      case SEM_TYPE_LONG_INTEGER:
      case SEM_TYPE_REAL:
      case SEM_TYPE_BOOL:
        return_type = "NSNumber *_Nullable";
        return_type_separator = " ";
        // Since the conversion is already done by the parent assembly query, we can just return the parent value
        // directly and no need to convert again here for fragment
        if (!is_extension_fragment) {
          value_convert_begin = "@(";
          value_convert_end = ")";
          c_getter_suffix = "_value";
          cg_objc_proc_result_set_c_getter(fetch_proc, &value, name, col_name, "_is_null", is_private);
          bprintf(&value, " ? nil : ");
        }
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
    is_extension_fragment ? rt->symbol_prefix : symbol_prefix.ptr,
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
  else if (is_extension_fragment) {
    // extension fragment always calls parent fragment 's getter for getting column from resultSet
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
      objc_parent_getter,
      symbol_prefix.ptr,
      current_parent_query_name ?: "",
      "_get_",
      col_name);
    bprintf(&value, "%s(resultSet, row)",
            objc_parent_getter.ptr);
    CHARBUF_CLOSE(objc_parent_getter);
  }
  else {
    bprintf(&value, "%s%s(cResultSet, row)%s",
            value_convert_begin,
            c_getter.ptr,
            value_convert_end);
  }

  if (is_extension_fragment) {
    // Since the parent assembly query has already fetched the aggregated resultSet, we just emit
    // a call to the parent for extension fragment rather than doing a regular getter call.
    CG_CHARBUF_OPEN_SYM(objc_parent_name, current_parent_query_name);
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
    bprintf(output, "  %s cResultSet = %s(resultSet);\n", c_result_set_ref, c_convert);
  }
  bprintf(output, "  return %s;\n", value.ptr);
  bprintf(output, "}\n");

  CHARBUF_CLOSE(c_getter);
  CHARBUF_CLOSE(objc_getter);
  CHARBUF_CLOSE(symbol_prefix);
  CHARBUF_CLOSE(impl_symbol_prefix);
  CHARBUF_CLOSE(value);
}

static void cg_objc_set_parent_fragment_name(CSTR _Nonnull name, ast_node *_Nonnull _misc_attr, void *_Nullable _context) {
  current_parent_query_name = name;
  symtab_add(assembly_query_names, name, NULL);
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
  bool_t suppressed = 
    exists_attribute_str(misc_attrs, "suppress_getters") ||
    exists_attribute_str(misc_attrs, "suppress_result_set");

  if (suppressed) {
    return;
  }

  is_extension_fragment = misc_attrs &&
      find_extension_fragment_attr(misc_attrs, cg_objc_set_parent_fragment_name, NULL);
  is_assembly_query = misc_attrs && find_assembly_query_attr(misc_attrs, NULL, NULL);
  if (is_extension_fragment) {
    Contract(current_parent_query_name);
  }

  CSTR c_result_set_name = is_extension_fragment ? current_parent_query_name : name;
  charbuf *h = is_extension_fragment ? objc_extension_header : cg_header_output;

  CG_CHARBUF_OPEN_SYM(objc_name, name);
  CG_CHARBUF_OPEN_SYM(objc_result_set_name, c_result_set_name);
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_name, rt->impl_symbol_prefix, name);

  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_result_set, rt->impl_symbol_prefix, c_result_set_name, "_result_set");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(
    c_result_set_ref, rt->impl_symbol_prefix, c_result_set_name, "_result_set_ref");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(c_convert, "", c_name.ptr, "_from_", objc_name.ptr);

  // Since the parent assembly query has already fetched the aggregated resultSet, we call to use that directly and
  // skip setting up objc and c bridging for extension fragment specific result unless otherwise
  if (!is_extension_fragment) {
    CG_CHARBUF_OPEN_SYM_WITH_PREFIX(objc_convert, "", objc_name.ptr, "_from_", c_name.ptr);

    bprintf(h, "\n@class %s;\n", objc_name.ptr);
    bprintf(h, "\nstatic inline %s *%s(%s resultSet)\n", objc_name.ptr, objc_convert.ptr, c_result_set_ref.ptr);
    bprintf(h, "{\n");
    bprintf(h, "  return (__bridge %s *)resultSet;\n", objc_name.ptr);
    bprintf(h, "}\n");

    bprintf(h, "\nstatic inline %s %s(%s *resultSet)\n", c_result_set_ref.ptr, c_convert.ptr, objc_name.ptr);
    bprintf(h, "{\n");
    bprintf(h, "  return (__bridge %s)resultSet;\n", c_result_set_ref.ptr);
    bprintf(h, "}\n");

    CHARBUF_CLOSE(objc_convert);
  }

  bool_t out_stmt_proc = has_out_stmt_result(ast);
  // extension fragments use SELECT and are incompatible with the single row result set form using OUT
  if (is_extension_fragment) {
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

  bprintf(h,
          "\nstatic inline %s %sResultCount(%s *resultSet)\n",
          rt->cql_int32,
          objc_name.ptr,
          objc_result_set_name.ptr);
  bprintf(h, "{\n");
  if (is_extension_fragment) {
    bprintf(h,
            "  return %sResultCount(resultSet);\n",
            objc_result_set_name.ptr);
  }
  else {
    bprintf(h, "  return %sResultCount(%s(resultSet));\n", c_name.ptr, c_convert.ptr);
  }
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
  if (is_extension_fragment) {
    bprintf(h, "  return %sCopy(resultSet, from, count);\n", objc_result_set_name.ptr);
  }
  else {
    bprintf(h, "  %s copy;\n", c_result_set_ref.ptr);
    bprintf(h,
            "  %sCopy(%s(resultSet), &copy%s);\n",
            c_name.ptr,
            c_convert.ptr,
            out_stmt_proc ? "" : ", from, count");
    bprintf(h, "  return (__bridge_transfer %s *)copy;\n", objc_name.ptr);
  }
  bprintf(h, "}\n");

  if (!is_extension_fragment) {
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

  CHARBUF_CLOSE(c_convert);
  CHARBUF_CLOSE(c_result_set_ref);
  CHARBUF_CLOSE(c_result_set);
  CHARBUF_CLOSE(c_name);
  CHARBUF_CLOSE(objc_result_set_name);
  CHARBUF_CLOSE(objc_name);
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
  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    // skip codegen here for attribute-annotated base query fragment since codegen only for included assembled fragment
    if (misc_attrs && find_base_fragment_attr(misc_attrs, NULL, NULL)) {
      sem_struct *sptr = stmt->sem->sptr;
      // record number of base columns to track base rows vs extension rows
      col_count_for_base_objc = sptr->count;
      continue;
    }
    cg_objc_one_stmt(stmt);
  }
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

  int32_t error = 0;
  CHARBUF_OPEN(extension_header);
  CHARBUF_OPEN(header_file);

  objc_extension_header = &extension_header;
  assembly_query_names = symtab_new();
  current_parent_query_name = NULL;
  col_count_for_base_objc = 0;

  // gen objc code ....
  cg_objc_stmt_list(head);

  // The procedure has no result set
  if (cg_header_output->used == 1 && objc_extension_header->used == 1) {
    goto cleanup;
  }

  bprintf(&header_file, "%s", rt->header_prefix);
  // if it is an extension fragment we don't need to include the C header,
  // extension will rely only on assembly header to access data
  if (!is_extension_fragment) {
    bprintf(&header_file, "\n#import <%s>\n", options.objc_c_include_path);
  }
  if (is_extension_fragment && !is_assembly_query) {
    CSTR assembly_query_namespace_val = options.objc_assembly_query_namespace;
    // if we are currently emitting for extension fragment, it has to import and use row getters from parent
    // assembly query generated header file, with parent namespace supplied through codegen
    if (!assembly_query_namespace_val) {
      cql_error("assembly query namespace not provided for extension fragment; no code gen.\n");
      error = 1;
      goto cleanup;
    }

    symtab_entry *parent_query_names = symtab_copy_sorted_payload(assembly_query_names, default_symtab_comparator);
    for (uint32_t i = 0; i < assembly_query_names->count; i++) {
      CG_CHARBUF_OPEN_SYM(objc_parent_name, parent_query_names[i].sym);
      CHARBUF_OPEN(assembly_query_namespace);
      if (strcmp(".", assembly_query_namespace_val) == 0) {
        bprintf(&assembly_query_namespace, "");
      }
      else {
        bprintf(&assembly_query_namespace, "%s/", assembly_query_namespace_val);
      }
      bprintf(&header_file,
              "\n#import <%s%s.h>\n",
              assembly_query_namespace.ptr,
              objc_parent_name.ptr);
      CHARBUF_CLOSE(assembly_query_namespace);
      CHARBUF_CLOSE(objc_parent_name);
    }
    free(parent_query_names);
  }

  bprintf(&header_file, "%s", rt->header_wrapper_begin);

  // Include header outputs in extension buffer into final generation if this is for
  // extension only and disgard if this is for parent assembly query
  if (!is_assembly_query) {
    bprintf(&header_file, "%s", extension_header.ptr);
  }

  bprintf(&header_file, "%s", cg_header_output->ptr);
  bprintf(&header_file, "%s", rt->header_wrapper_end);

  CSTR header_file_name = options.file_names[0];
  cql_write_file(header_file_name, header_file.ptr);

cleanup:
  CHARBUF_CLOSE(header_file);
  CHARBUF_CLOSE(extension_header);
  SYMTAB_CLEANUP(assembly_query_names);

  // reset globals so they don't interfere with leaksan
  objc_extension_header = NULL;
  current_parent_query_name = NULL;
  col_count_for_base_objc = 0;

  if (error) {
    cql_cleanup_and_exit(error);
  }
}

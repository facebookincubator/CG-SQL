/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_OBJC)

// stubs to avoid link errors
cql_noexport void cg_objc_main(ast_node *head) {}

#else

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

// Whether a text column in the result set of a proc is encoded
static bool_t is_string_column_encoded = 0;

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
  charbuf *output,
  bool_t encode,
  bool_t custom_type_for_encoded_column)
{
  Contract(is_unitary(sem_type));
  sem_t core_type = core_type_of(sem_type);
  Contract(core_type != SEM_TYPE_NULL);
  Contract(cg_main_output);

  bool_t nullable = is_nullable(sem_type);
  CHARBUF_OPEN(return_type);
  CSTR return_type_separator = "";

  CHARBUF_OPEN(value_convert_begin);
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
        bprintf(&return_type, "%s", "NSNumber *_Nullable");
        return_type_separator = " ";
         bprintf(&value_convert_begin, "%s", "@(");
        value_convert_end = ")";
        c_getter_suffix = "_value";
        cg_objc_proc_result_set_c_getter(fetch_proc, &value, name, col_name, "_is_null", is_private);
        bprintf(&value, " ? nil : ");
        break;
      case SEM_TYPE_BLOB:
        bprintf(&return_type, "%s_Nullable", rt->cql_blob_ref);
        return_type_separator = " ";
        bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_blob_ref);
        break;
      case SEM_TYPE_TEXT:
        if (encode && custom_type_for_encoded_column) {
          is_string_column_encoded = 1;
          bprintf(&return_type, "%s *_Nullable", rt->cql_string_ref_encode);
          bprintf(&value_convert_begin, "(__bridge %s *)", rt->cql_string_ref_encode);
        } else {
          bprintf(&return_type, "%s_Nullable", rt->cql_string_ref);
          bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_string_ref);
        }
        return_type_separator = " ";
        break;
      case SEM_TYPE_OBJECT:
        bprintf(&return_type, "%s_Nullable", rt->cql_object_ref);
        return_type_separator = " ";
        bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_object_ref);
        break;
    }
  } else {
    switch (core_type) {
      case SEM_TYPE_INTEGER:
        return_type_separator = " ";
        bprintf(&return_type, "%s", rt->cql_int32);
        break;
      case SEM_TYPE_LONG_INTEGER:
        return_type_separator = " ";
        bprintf(&return_type, "%s", rt->cql_int64);
        break;
      case SEM_TYPE_REAL:
        return_type_separator = " ";
        bprintf(&return_type, "%s", rt->cql_double);
        break;
      case SEM_TYPE_BOOL:
        return_type_separator = " ";
        bprintf(&return_type, "%s", rt->cql_bool);
        value_convert_end = " ? YES : NO";
        break;
      case SEM_TYPE_TEXT:
        if (encode && custom_type_for_encoded_column) {
          is_string_column_encoded = 1;
          bprintf(&return_type, "%s", rt->cql_string_ref_encode);
          bprintf(&value_convert_begin, "(__bridge %s *)", rt->cql_string_ref_encode);
        } else {
          bprintf(&return_type, "%s", rt->cql_string_ref);
          bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_string_ref);
        }
        break;
      case SEM_TYPE_BLOB:
        bprintf(&return_type, "%s", rt->cql_blob_ref);
        bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_blob_ref);
        break;
      case SEM_TYPE_OBJECT:
        bprintf(&return_type, "%s", rt->cql_object_ref);
        return_type_separator = " ";
        bprintf(&value_convert_begin, "(__bridge %s)", rt->cql_object_ref);
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
            value_convert_begin.ptr,
            c_getter.ptr,
            value_convert_end);
  }
  else {
    bprintf(&value, "%s%s(cResultSet, row)%s",
            value_convert_begin.ptr,
            c_getter.ptr,
            value_convert_end);
  }

  if (is_ext) {
    // Since the parent assembly query has already fetched the aggregated resultSet, we just emit
    // a call to the parent for extension fragment rather than doing a regular getter call.
    CG_CHARBUF_OPEN_SYM(objc_parent_name, base_fragment_name);
    bprintf(output,
            "\nstatic inline %s%s%s(%s *resultSet, %s row)\n",
            return_type.ptr,
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
              return_type.ptr,
              return_type_separator,
              objc_getter.ptr,
              objc_name);
    }
    else {
      bprintf(output,
              "\nstatic inline %s%s%s(%s *resultSet, %s row)\n",
              return_type.ptr,
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
  CHARBUF_CLOSE(value_convert_begin);
  CHARBUF_CLOSE(return_type);
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

  Invariant(!use_encode);
  Invariant(!encode_context_column);
  Invariant(!encode_columns);
  encode_columns = symtab_new();
  init_encode_info(misc_attrs, &use_encode, &encode_context_column, encode_columns);

  bool_t is_ext = objc_frag_type == FRAG_TYPE_EXTENSION;
  if (is_ext) {
    Contract(base_fragment_name);
  }

  bool_t custom_type_for_encoded_column = !!exists_attribute_str(misc_attrs, "custom_type_for_encoded_column");
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
      h,
      should_encode_col(col, sem_type, use_encode, encode_columns),
      custom_type_for_encoded_column);
  }

  if (use_encode) {
    for (uint32_t i = 0; i < count; i++) {
      CSTR col = sptr->names[i];
      sem_t sem_type = sptr->semtypes[i];
      bool_t encode = should_encode_col(col, sem_type, use_encode, encode_columns);
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

  CG_CHARBUF_OPEN_SYM(cgs_result_count, name, "_result_count");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(result_count, rt->impl_symbol_prefix, name, "_result_count");

  bprintf(h,
          "\nstatic inline %s %s(%s *resultSet)\n",
          rt->cql_int32,
          cgs_result_count.ptr,
          objc_result_set_name.ptr);


  bprintf(h, "{\n");
  bprintf(h, "  return %s(%s(resultSet));\n", result_count.ptr, c_convert.ptr);
  bprintf(h, "}\n");

  CHARBUF_CLOSE(result_count);
  CHARBUF_CLOSE(cgs_result_count);

  CG_CHARBUF_OPEN_SYM(cgs_copy_func_name, name, "_copy");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(copy_func_name, rt->impl_symbol_prefix, name, "_copy");

  bool_t generate_copy = misc_attrs && exists_attribute_str(misc_attrs, "generate_copy");
  if (generate_copy) {
    bprintf(h,
            "\nstatic inline %s *%s(%s *resultSet",
            objc_result_set_name.ptr,
            cgs_copy_func_name.ptr,
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
            "  %s(%s(resultSet), &copy%s);\n",
            copy_func_name.ptr,
            c_convert.ptr,
            out_stmt_proc ? "" : ", from, count");
    bprintf(h, "  %s(copy);\n", rt->cql_result_set_note_ownership_transferred);
    bprintf(h, "  return (__bridge_transfer %s *)copy;\n", is_ext ? objc_class_name.ptr : objc_name.ptr);
    bprintf(h, "}\n");
  }

  CHARBUF_CLOSE(copy_func_name);
  CHARBUF_CLOSE(cgs_copy_func_name);

  CSTR opt_row = out_stmt_proc ? "" : "_row";
  CG_CHARBUF_OPEN_SYM(cgs_hash_func_name, name, opt_row, "_hash");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(hash_func_name, rt->impl_symbol_prefix, name, opt_row, "_hash");
  CG_CHARBUF_OPEN_SYM(cgs_eq_func_name, name, opt_row, "_equal");
  CG_CHARBUF_OPEN_SYM_WITH_PREFIX(eq_func_name, rt->impl_symbol_prefix, name, opt_row, "_equal");

  if (!is_ext) {


    bprintf(h,
            "\nstatic inline NSUInteger %s(%s *resultSet",
            cgs_hash_func_name.ptr,
            objc_name.ptr);
    if (!out_stmt_proc) {
      bprintf(h, ", %s row", rt->cql_int32);
    }
    bprintf(h, ")\n");
    bprintf(h, "{\n");
    bprintf(h,
            "  return %s(%s(resultSet)%s);\n",
            hash_func_name.ptr,
            c_convert.ptr,
            out_stmt_proc ? "" : ", row");
    bprintf(h, "}\n");

    bprintf(h,
            "\nstatic inline BOOL %s(%s *resultSet1",
            cgs_eq_func_name.ptr,
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
            "  return %s(%s(resultSet1)%s, %s(resultSet2)%s);\n",
            eq_func_name.ptr,
            c_convert.ptr,
            out_stmt_proc ? "" : ", row1",
            c_convert.ptr,
            out_stmt_proc ? "" : ", row2");
    bprintf(h, "}\n");
  }

  CHARBUF_CLOSE(eq_func_name);
  CHARBUF_CLOSE(cgs_eq_func_name);
  CHARBUF_CLOSE(hash_func_name);
  CHARBUF_CLOSE(cgs_hash_func_name);


  CHARBUF_CLOSE(objc_class_name);
  CHARBUF_CLOSE(c_convert);
  CHARBUF_CLOSE(c_result_set_ref);
  CHARBUF_CLOSE(c_result_set);
  CHARBUF_CLOSE(c_name);
  CHARBUF_CLOSE(objc_result_set_name);
  CHARBUF_CLOSE(objc_name);

  use_encode = 0;
  symtab_delete(encode_columns);
  encode_columns = NULL;
  encode_context_column = NULL;
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
  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    objc_frag_type = find_fragment_attr_type(misc_attrs, &base_fragment_name);

    if (objc_frag_type == FRAG_TYPE_SHARED) {
      // shared fragments never create any code
      continue;
    }

    if (objc_frag_type == FRAG_TYPE_BASE) {
      // skipping the base fragment getters since generating in each extension
      // will cause collisions including two fragments headers
      continue;
    }

    if (!containsAssembly) {
      // record if an assembly is present
      containsAssembly = objc_frag_type == FRAG_TYPE_ASSEMBLY;
    }

    cg_objc_one_stmt(stmt);
  }

  // Include header outputs in extension buffer into final generation if this is for
  // extension only and disgard if this is for parent assembly query
  if (!containsAssembly) {
    bprintf(cg_header_output, "%s", objc_extension_header->ptr);
  }
}


static void cg_objc_init(void) {
  cg_common_init();
}

// Main entry point for code-gen.
cql_noexport void cg_objc_main(ast_node *head) {
  Invariant(options.file_names_count == 1);
  Invariant(is_string_column_encoded == 0);
  if (!options.objc_c_include_path) {
    cql_error("The C header path must be provided as argument (use --objc_c_include_path)\n");
    cql_cleanup_and_exit(1);
  }
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  // OBJC can be configured in a variety of ways, when running the suites and pattern matching
  // we hard code it to these normalized options.  This also means we don't test every combo
  // but really this only affects the symbol string building and that is heavily tested in
  // other places.  Previously we used sed to normalize.
  
  if (options.test) {
    // make a new rt in the pool so we can safely mutate it without affecting
    // subsequent runs in the amalgam case.

    rtdata *rtnew = _ast_pool_new(rtdata);
    *rtnew = *rt;
    rt = rtnew;

    // set some canonical options

    rt->header_prefix = "#pragma once\n\n#import <Foundation/Foundation.h>\n\n";
    rt->symbol_prefix = "CGB";
    rt->impl_symbol_prefix = "CGC";
    rt->symbol_case = cg_symbol_case_pascal;
    rt->cql_string_ref_encode = "CGOEncodedString";
  }

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

  if (is_string_column_encoded) {
    bprintf(&header_file, "\n@class %s;\n", rt->cql_string_ref_encode);
  }

  bprintf(&header_file, "%s", cg_header_output->ptr);
  bprintf(&header_file, "%s", rt->header_wrapper_end);

  CSTR header_file_name = options.file_names[0];
  cql_write_file(header_file_name, header_file.ptr);

  CHARBUF_CLOSE(imports);
  CHARBUF_CLOSE(header_file);
  CHARBUF_CLOSE(extension_header);

  // reset globals so they don't interfere with leaksan
  objc_extension_header = NULL;
  is_string_column_encoded = 0;
}

#endif

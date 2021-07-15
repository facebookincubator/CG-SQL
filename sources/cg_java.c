/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Perform codegen of the various nodes to "Java".

#include "cg_java.h"

#include "ast.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "symtab.h"

typedef struct cg_java_context {
  // Buffer to hold the accumulated offsets for fragment core columns to be used by extensions.
  charbuf *frag_col_offsets_for_core;

  // Increment on each extension fragment SP to supply total count for assembly query to process
  // column offsets
  uint32_t frag_col_offset_count;

  // In the Java codegen pipeline, we support only one SP per codegen run. This is to accomodate
  // the fact that in Java we can only generate one top level public class per file
  uint32_t generated_proc_count;
} cg_java_context;

// Helper for assembly query to codegen static offset getter function for its extensions to call
// - first extension has a column offset of 0 since all of its columns start right after core
// - for following extension, its offset is the total number of fragment specific columns for all previous fragments
static void cg_java_ext_col_offsets_in_asm(charbuf *body, uint32_t col_count_for_base, cg_java_context *java_context) {
  bprintf(body,
          "private static final Map<Long, Integer> fragmentColOffsetsForCore = new HashMap<>();\n"
          "static {\n");
  bprintf(body, java_context->frag_col_offsets_for_core->ptr);
  bprintf(body,
          "}\n"
          "\n"
          "public static int getExtensionColOffset(Long fragmentCRC) {\n"
          "  Integer offset = fragmentColOffsetsForCore.get(fragmentCRC);\n"
          "\n"
          "  if (offset == null) {\n"
          "    throw new RuntimeException(\"Invalid CQL fragment CRC \" + fragmentCRC);\n"
          "  }\n"
          "\n"
          "  return offset;\n"
          "}\n"
          "\n");
}

static void cg_java_proc_result_set_getter(bool_t fetch_proc,
                                           CSTR name,
                                           CSTR col_name,
                                           int32_t col,
                                           charbuf *java,
                                           sem_t sem_type,
                                           bool_t encode,
                                           bool is_extension_fragment,
                                           uint32_t col_count_for_base) {
  Contract(is_unitary(sem_type));
  sem_t core_type = core_type_of(sem_type);
  Contract(core_type != SEM_TYPE_NULL);

  bool_t notnull = is_not_nullable(sem_type);

  CSTR tmp_func;
  CSTR return_type;
  CSTR field_type;
  CSTR prefix = "get_";
  CSTR nullable_prefix = notnull ? "" : "_nullable_";

  if (notnull) {
    tmp_func = rt->cql_result_set_get_data;
  } else {
    tmp_func = rt->cql_java_tmp_getter_nullable;
  }

  switch (core_type) {
    case SEM_TYPE_INTEGER:
      if (notnull) {
        return_type = rt->cql_int32;
      } else {
        return_type = rt->cql_int32_nullable;
      }
      field_type = rt->cql_int32_nullable;
      break;

    case SEM_TYPE_TEXT:
      nullable_prefix = "";
      return_type = rt->cql_string_ref;
      field_type = rt->cql_string_ref;
      break;

    case SEM_TYPE_LONG_INTEGER:
      if (notnull) {
        return_type = rt->cql_int64;
      } else {
        return_type = rt->cql_int64_nullable;
      }
      field_type = rt->cql_int64_nullable;
      break;

    case SEM_TYPE_REAL:
      if (notnull) {
        return_type = rt->cql_double;
      } else {
        return_type = rt->cql_double_nullable;
      }
      field_type = rt->cql_double_nullable;
      break;

    case SEM_TYPE_BOOL:
      if (notnull) {
        prefix = "";
        return_type = rt->cql_bool;
      } else {
        return_type = rt->cql_bool_nullable;
      }
      field_type = rt->cql_bool_nullable;
      break;

    case SEM_TYPE_BLOB:
      nullable_prefix = "";
      return_type = rt->cql_blob_ref;
      field_type = "Blob";
      break;
  }

  CG_CHARBUF_OPEN_SYM(col_name_camel, prefix, col_name);
  CG_CHARBUF_OPEN_SYM(method_name, nullable_prefix, field_type);

  CHARBUF_OPEN(col_index);
  if (is_extension_fragment && col >= col_count_for_base) {
    // extension fragment getter's column index is calculated using its col_index
    // and offset provided by the assembly query
    bprintf(&col_index,
            "%d + colOffset",
            col);
  }
  else {
    bprintf(&col_index, "%d", col);
  }
  bprintf(java,
          tmp_func,
          return_type,
          col_name_camel.ptr,
          // patternlint-disable-next-line prefer-sized-ints-in-msys
          fetch_proc ? "" : "int row",
          method_name.ptr,
          fetch_proc ? "0" : "row",
          col_index.ptr);

  if (encode) {
    bprintf(java, "public %s %sIsEncoded() {\n", rt->cql_bool, col_name_camel.ptr);
    bprintf(java, "  return mResultSet.getIsEncoded(%s);\n", col_index.ptr);
    bprintf(java, "}\n\n");
  }

  CHARBUF_CLOSE(col_index);
  CHARBUF_CLOSE(method_name);
  CHARBUF_CLOSE(col_name_camel);
}

static void no_op(CSTR _Nonnull name, ast_node *_Nonnull attr, void *_Nullable context) {
  return;
}

static void cg_java_proc_result_set(ast_node *ast, cg_java_context *java_context) {
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

  uint32_t frag_type = find_fragment_attr_type(misc_attrs);
  bool_t is_extension_fragment = frag_type == FRAG_TYPE_EXTENSION;
  bool_t is_assembly_fragment = frag_type == FRAG_TYPE_ASSEMBLY;

  if (java_context->generated_proc_count == 1 && !is_assembly_fragment && !is_extension_fragment) {
    // We've already generated a Java SP. More SPs are not allowed unless
    // this is for either assembly query or fragments supplied for it
    cql_error(
            "The Java code generator only supports one stored procedure per file. "
            "Multiple procedures were found while generating %s\n",
            options.file_names[0]);
    cql_cleanup_and_exit(1);
  }
  Contract(is_ast_create_proc_stmt(ast));
  Contract(is_struct(ast->sem->sem_type));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT_STRING(name, ast->left);
  sem_struct *sptr = ast->sem->sptr;

  uint32_t col_count_for_base = 0;
  if (frag_type != FRAG_TYPE_NONE) {
    // we already know the base compiled with no errors
    ast_node *base_proc = find_base_fragment(base_fragment_name);
    Invariant(base_proc);
    Invariant(base_proc->sem);
    Invariant(base_proc->sem->sptr);
    col_count_for_base = base_proc->sem->sptr->count;
  }

  // extension column getters are managed in extensions only so skip generating for assembly
  uint32_t count = is_assembly_fragment ? col_count_for_base : sptr->count;

  // We store number of columns for all extension fragments and use that to derive column offset for each of them
  if (is_extension_fragment) {
    CHARBUF_OPEN(fragment_name);
    bprintf(&fragment_name, "%s", name);
    bprintf(java_context->frag_col_offsets_for_core,
            "  fragmentColOffsetsForCore.put(%lldL, %d);\n",
            crc_charbuf(&fragment_name),
            java_context->frag_col_offset_count);
    CHARBUF_CLOSE(fragment_name);
    java_context->frag_col_offset_count += count - col_count_for_base;
  }

  bool_t include_identity_columns = misc_attrs != NULL ? find_identity_columns(misc_attrs, no_op, NULL) != 0 : 0;

  CHARBUF_OPEN(cg_java_output);
  CHARBUF_OPEN(class_def);
  CHARBUF_OPEN(body);
  CHARBUF_OPEN(class_name);
  extract_base_path_without_extension(&class_name, options.file_names[0]);

  CHARBUF_OPEN(close_bracket);

  bprintf(&cg_java_output, "%s", rt->source_prefix);
  bprintf(&cg_java_output, rt->source_wrapper_begin, options.java_package_name);
  if (is_assembly_fragment) {
    bprintf(&cg_java_output,
            "import java.util.HashMap;\n"
            "import java.util.Map;\n\n");
  } else if (is_extension_fragment) {
    // in order to access method from the assembly class, we need to import
    // Method to access them via reflection
    bprintf(&cg_java_output,
            "import java.lang.reflect.InvocationTargetException;\n"
            "import java.lang.reflect.Method;\n\n");
  }

  bool_t out_stmt_proc = has_out_stmt_result(ast);

  bprintf(&class_def,
          rt->cql_java_tmp_class_def,
          class_name.ptr);

  // Stored procedure name constant does not make sense for extension fragments since the application layer
  // that coordinates access to them will be dealing with assembly
  if (!is_extension_fragment) {
    bprintf(&body, "public static final String STORED_PROCEDURE_NAME = \"%s\";\n\n", name);
  }

  if (is_assembly_fragment) {
    cg_java_ext_col_offsets_in_asm(&body, col_count_for_base, java_context);
    // if is an assembly query we need to expose the resultset to instantiate the fragments from it.
    bprintf(&body, "public CQLResultSet toFragment() {\n");
    bprintf(&body, "    return mResultSet;\n");
    bprintf(&body, "}\n\n");
  } else if (is_extension_fragment && options.java_assembly_query_classname) {
    // If we are emitting for extension fragment, its CRC is used to look up its column offset from assembly query
    CHARBUF_OPEN(fragment_name);
    bprintf(&fragment_name, "%s", name);
    bprintf(&body, "private static final Long extensionCRC = %lldL;\n", crc_charbuf(&fragment_name));
    CHARBUF_CLOSE(fragment_name);
    // patternlint-disable-next-line prefer-sized-ints-in-msys
    bprintf(&body, "private static int colOffset = -1;\n\n");
    bprintf(&body,
            "static {\n"
            "    try {\n"
            "        Class<?> c = Class.forName(\"%s\");\n"
            "        Method getExtensionColOffset = c.getMethod(\"getExtensionColOffset\", Long.class);\n"
            "        colOffset = (Integer)getExtensionColOffset.invoke(null, extensionCRC);\n"
            "    } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException | "
            "ClassNotFoundException e) {\n"
            "       throw new RuntimeException(e);\n"
            "    }\n"
            "}\n\n",
            options.java_assembly_query_classname);
  }

  bprintf(&body,
          rt->cql_java_tmp_class_constructor,
          class_name.ptr);

  // For each field emit the _get_field method
  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR col = sptr->names[i];
    bool_t encode = should_encode_col(col, sem_type, use_encode, encode_columns);
    cg_java_proc_result_set_getter(out_stmt_proc, name, col, i, &body, sem_type, encode, is_extension_fragment, col_count_for_base);
  }

  bprintf(&body, "%s", rt->cql_result_set_get_count);

  bool_t generate_copy = misc_attrs && exists_attribute_str(misc_attrs, "generate_copy");
  if (generate_copy) {
    bprintf(&body, rt->cql_result_set_copy, class_name.ptr, class_name.ptr);
  }

  bprintf(&body, rt->cql_result_set_has_identity_columns, include_identity_columns ? "true" : "false");
  bindent(&cg_java_output, &class_def, 0);
  bindent(&cg_java_output, &body, 2);
  bprintf(&cg_java_output, "}\n");

  cql_write_file(options.file_names[0], cg_java_output.ptr);

  CHARBUF_CLOSE(close_bracket);
  CHARBUF_CLOSE(class_name);
  CHARBUF_CLOSE(body);
  CHARBUF_CLOSE(class_def);
  CHARBUF_CLOSE(cg_java_output);

  java_context->generated_proc_count++;

  use_encode = 0;
  symtab_delete(encode_columns);
  encode_columns = NULL;
  encode_context_column = NULL;
}

static void cg_java_create_proc_stmt(ast_node *ast, cg_java_context *java_context) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  bool_t result_set_proc = has_result_set(ast);
  bool_t out_stmt_proc = has_out_stmt_result(ast);
  bool_t out_union_proc = has_out_union_stmt_result(ast);

  if (result_set_proc || out_stmt_proc || out_union_proc) {
    sem_struct *sptr = ast->sem->sptr;
    uint32_t count = sptr->count;
    for (int32_t i = 0; i < count; i++) {
      sem_t sem_type = sptr->semtypes[i];

      // resultsets with objects in java are not supported
      if (core_type_of(sem_type) == SEM_TYPE_OBJECT) {
        cql_error("out cursors with object columns are not yet supported for java\n");
        cql_cleanup_and_exit(1);
      }
    }

    cg_java_proc_result_set(ast, java_context);
  }
}

// java codegen only deals with the create proc statement so use an easy dispatch
static void cg_java_one_stmt(ast_node *stmt, cg_java_context *java_context) {
  if (is_ast_create_proc_stmt(stmt)) {
    cg_java_create_proc_stmt(stmt, java_context);
  }
}

static void cg_java_stmt_list(ast_node *head, cg_java_context *java_context) {
  uint32_t frag_type = FRAG_TYPE_NONE;
  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);
    // skiping the base fragment getters since generating in each extension
    // will cause collisions including two fragments headers
    frag_type = find_fragment_attr_type(misc_attrs);
    if (frag_type == FRAG_TYPE_BASE) {
      continue;
    }
    cg_java_one_stmt(stmt, java_context);
  }

  // Final check to make sure valid parent assembly query classname if we are emitting for extension fragment
  if (frag_type == FRAG_TYPE_EXTENSION && !options.java_assembly_query_classname) {
    cql_error("assembly query classname not provided for extension fragment; no code gen.\n");
    cql_cleanup_and_exit(1);
  }
}

static void cg_java_init(void) {
  cg_common_init();
}

// Main entry point for code-gen.
cql_noexport void cg_java_main(ast_node *head) {
  // this is verified by the generic code
  Invariant(options.file_names_count == 1);

  if (!options.java_package_name) {
    cql_error("A java package name must be specified (use --java_package_name)\n");
    cql_cleanup_and_exit(1);
  }

  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();

  cg_java_init();

  // gen java code ....
  CHARBUF_OPEN(frag_col_offsets_for_core);
  cg_java_context java_context = {
    .frag_col_offsets_for_core = &frag_col_offsets_for_core,
  };
  cg_java_stmt_list(head, &java_context);
  CHARBUF_CLOSE(frag_col_offsets_for_core);
}

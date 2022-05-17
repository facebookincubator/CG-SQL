/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Perform codegen of the various nodes to "Java".

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_JAVA)

// stubs to avoid link errors
cql_noexport void cg_java_main(ast_node *head) {}
cql_noexport void cg_java_cleanup() {}

#else

#include "cg_java.h"
#include <stdint.h>

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

  // Table that maps a fragment name to the interfaces that the assembly will implement.  These
  // are collected from the in-scope base and extension fragments that have been parsed before
  // the assembly.
  symtab *frag_assembly_interfaces;

  // In the Java codegen pipeline, we support only one SP per codegen run. This is to acccomodate
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
  bindent(body, java_context->frag_col_offsets_for_core, 2);
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

static bool_t cg_java_frag_type_query_proc(uint32_t frag_type) {
  Contract(frag_type != FRAG_TYPE_EXTENSION);
  return frag_type != FRAG_TYPE_BASE;
}

static void cg_java_getter_sig(charbuf *buf, CSTR return_type, CSTR name, CSTR params) {
  bprintf(buf, "public %s %s(%s)", return_type, name, params);
}

static void cg_java_proc_result_set_getter(
  bool_t fetch_proc,
  CSTR name,
  CSTR col_name,
  int32_t col,
  charbuf *java,
  sem_t sem_type,
  bool_t encode,
  bool_t custom_type_for_encoded_column,
  uint32_t frag_type,
  uint32_t col_count_for_base)
{
  Contract(is_unitary(sem_type));
  Contract(core_type_of(sem_type) != SEM_TYPE_NULL);
  Contract(frag_type != FRAG_TYPE_SHARED);
  Contract(frag_type != FRAG_TYPE_EXTENSION);

  bool_t notnull = is_not_nullable(sem_type);
  sem_t core_type = core_type_of(sem_type);

  CSTR return_type;
  CSTR field_type;
  CSTR prefix = "get_";
  CSTR nullable_prefix = notnull ? "" : "_nullable_";
  CSTR nullable_attr = notnull ? "" : "@Nullable\n";

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
      if (encode && custom_type_for_encoded_column) {
        return_type = rt->cql_string_ref_encode;
        field_type = rt->cql_string_ref_encode;
      } else {
        return_type = rt->cql_string_ref;
        field_type = rt->cql_string_ref;
      }
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
  bprintf(&col_index, "%d", col);

  bprintf(java, nullable_attr);
  CHARBUF_OPEN(getter_sig);
  // patternlint-disable-next-line prefer-sized-ints-in-msys
  cg_java_getter_sig(&getter_sig, return_type, col_name_camel.ptr, fetch_proc ? "" : "int row");
  if (!options.java_fragment_interface_mode || cg_java_frag_type_query_proc(frag_type)) {
    bprintf(java,
            rt->cql_result_set_get_data,
            getter_sig.ptr,
            method_name.ptr,
            fetch_proc ? "0" : "row",
            col_index.ptr);
  } else {
    bprintf(java, "%s;\n\n", getter_sig.ptr);
  }

  if (encode) {
    bprintf(java, "public %s %sIsEncoded() {\n", rt->cql_bool, col_name_camel.ptr);
    bprintf(java, "  return mResultSet.getIsEncoded(%s);\n", col_index.ptr);
    bprintf(java, "}\n\n");
  }

  CHARBUF_CLOSE(getter_sig);
  CHARBUF_CLOSE(col_index);
  CHARBUF_CLOSE(method_name);
  CHARBUF_CLOSE(col_name_camel);
}

static void no_op(CSTR _Nonnull name, ast_node *_Nonnull attr, void *_Nullable context) {
  return;
}

static void cg_java_validate_proc_count(cg_java_context *java_context, uint32_t frag_type) {
  if (java_context->generated_proc_count == 1 && frag_type == FRAG_TYPE_NONE) {
    // We've already generated a Java SP. More SPs are not allowed unless
    // this is for either assembly query or fragments supplied for it
    cql_error(
        "The Java code generator only supports one stored procedure per file. "
        "Multiple procedures were found while generating %s\n",
        options.file_names[0]
    );
    cql_cleanup_and_exit(1);
  }
}

static void cg_java_fragment_columns(
  CSTR name,
  cg_java_context *java_context,
  uint32_t frag_type,
  uint32_t *count,
  uint32_t *col_count_for_base)
{
  Contract(frag_type != FRAG_TYPE_SHARED);
  Contract(frag_type != FRAG_TYPE_EXTENSION);

  if (frag_type != FRAG_TYPE_NONE) {
    // we already know the base compiled with no errors
    ast_node *base_proc = find_base_fragment(base_fragment_name);
    Invariant(base_proc);
    Invariant(base_proc->sem);
    Invariant(base_proc->sem->sptr);
    *col_count_for_base = base_proc->sem->sptr->count;
  }

  *count = frag_type == FRAG_TYPE_ASSEMBLY ? *col_count_for_base : *count;
}

static void cg_java_write_implements_interface(
  charbuf *buf,
  CSTR name,
  cg_java_context *java_context,
  uint32_t frag_type)
{
  Contract(frag_type != FRAG_TYPE_NONE);
  Contract(frag_type != FRAG_TYPE_SHARED);
  Contract(frag_type != FRAG_TYPE_EXTENSION);

  CHARBUF_OPEN(interfaces);

  if (options.java_fragment_interface_mode) {
    Invariant(base_fragment_name);

    // Get the interface buffers
    charbuf *frag_assembly_interface;
    frag_assembly_interface = symtab_ensure_charbuf(java_context->frag_assembly_interfaces, base_fragment_name);

    if (frag_type == FRAG_TYPE_BASE) {
      // The current class name should be registered to both the extension and assembly interfaces.  There should
      // be nothing in the interface buffers, as the base extension is the first declared semantically.
      Invariant(frag_assembly_interface->used == 1);
      cg_sym_name(cg_symbol_case_pascal, frag_assembly_interface, "", name, NULL);
    }
    else if (frag_type == FRAG_TYPE_ASSEMBLY) {
      // This interface extends the base interface.
      bprintf(&interfaces, "%s", frag_assembly_interface->ptr);
    }
  }

  if (interfaces.used > 1) {
    bprintf(buf, " implements %s", interfaces.ptr);
  }
  CHARBUF_CLOSE(interfaces);
}

static void cg_java_write_imports(charbuf *buf, uint32_t frag_type) {
  uint32_t used = buf->used;
  if (!options.java_fragment_interface_mode) {
    if (frag_type == FRAG_TYPE_ASSEMBLY) {
      bprintf(buf,
              "import java.util.HashMap;\n"
              "import java.util.Map;\n");
    }
  }
  if (buf->used != used) {
    bprintf(buf, "\n");
  }
}

static void cg_java_write_fragment_class_accessors(
  charbuf *buf,
  CSTR name,
  uint32_t frag_type,
  cg_java_context *java_context,
  uint32_t col_count_for_base)
{
  if (frag_type == FRAG_TYPE_ASSEMBLY) {
    cg_java_ext_col_offsets_in_asm(buf, col_count_for_base, java_context);
    // if is an assembly query we need to expose the resultset to instantiate the fragments from it.
    bprintf(buf, "public CQLResultSet toFragment() {\n");
    bprintf(buf, "    return mResultSet;\n");
    bprintf(buf, "}\n\n");
  }
}

static void cg_java_write_class_or_interface(
  charbuf *buf,
  uint32_t frag_type,
  charbuf *name,
  charbuf *implements_interface,
  charbuf *body)
{
  if (!options.java_fragment_interface_mode || cg_java_frag_type_query_proc(frag_type)) {
    bprintf(buf,
            "public final class %s extends CQLViewModel%s {\n\n",
            name->ptr,
            implements_interface->ptr);
  } else {
    bprintf(buf,
            "public interface %s%s {\n\n",
            name->ptr,
            implements_interface->ptr);
  }
  bindent(buf, body, 2);
  bprintf(buf, "}\n");
}

static void cg_java_proc_result_set(ast_node *ast, cg_java_context *java_context) {
  EXTRACT_MISC_ATTRS(ast, misc_attrs);
  EXTRACT_STRING(name, ast->left);

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

  bool_t custom_type_for_encoded_column = !!exists_attribute_str(misc_attrs, "custom_type_for_encoded_column");
  uint32_t frag_type = find_fragment_attr_type(misc_attrs, &base_fragment_name);
  bool_t is_query_proc = cg_java_frag_type_query_proc(frag_type);
  cg_java_validate_proc_count(java_context, frag_type);

  Contract(is_ast_create_proc_stmt(ast));
  Contract(is_struct(ast->sem->sem_type));
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  sem_struct *sptr = ast->sem->sptr;

  uint32_t count = sptr->count;
  uint32_t col_count_for_base = 0;
  if (!options.java_fragment_interface_mode) {
    cg_java_fragment_columns(name, java_context, frag_type, &count, &col_count_for_base);
  }

  CHARBUF_OPEN(body);
  CHARBUF_OPEN(class_name);
  extract_base_path_without_extension(&class_name, options.file_names[0]);

  CHARBUF_OPEN(implements_interface);
  if (options.java_fragment_interface_mode && frag_type != FRAG_TYPE_NONE) {
    cg_java_write_implements_interface(&implements_interface, name, java_context, frag_type);
  }

  bool_t out_stmt_proc = has_out_stmt_result(ast);

  // Stored procedure name constant does not make sense for extension fragments since the application layer
  // that coordinates access to them will be dealing with assembly
  if (is_query_proc) {
    bprintf(&body, "public static final String STORED_PROCEDURE_NAME = \"%s\";\n\n", name);
  }

  if (!options.java_fragment_interface_mode) {
    cg_java_write_fragment_class_accessors(&body, name, frag_type, java_context, col_count_for_base);
  }

  if (!options.java_fragment_interface_mode || is_query_proc) {
    bprintf(&body,
            "public %s(CQLResultSet resultSet) {\n"
            "  super(resultSet);\n"
            "}\n\n",
            class_name.ptr);
  }

  bool_t is_string_column_encoded = false;
  // For each field emit the _get_field method
  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR col = sptr->names[i];
    bool_t encode = should_encode_col(col, sem_type, use_encode, encode_columns);
    is_string_column_encoded += custom_type_for_encoded_column && encode && core_type_of(sem_type) == SEM_TYPE_TEXT;
    cg_java_proc_result_set_getter(
        out_stmt_proc,
        name,
        col,
        i,
        &body,
        sem_type,
        encode,
        custom_type_for_encoded_column,
        frag_type,
        col_count_for_base);
  }

  CG_CHARBUF_OPEN_SYM(get_count, "", "get_count");
  CHARBUF_OPEN(get_count_sig);
  cg_java_getter_sig(&get_count_sig, rt->cql_int32, get_count.ptr, "");
  if (!options.java_fragment_interface_mode || is_query_proc) {
    bprintf(&body, rt->cql_result_set_get_count, get_count_sig.ptr);
  } else {
    bprintf(&body, "%s;\n\n", get_count_sig.ptr);
  }

  bool_t generate_copy = misc_attrs && exists_attribute_str(misc_attrs, "generate_copy");
  if (generate_copy) {
    bprintf(&body, rt->cql_result_set_copy, class_name.ptr, class_name.ptr);
  }

  if (!options.java_fragment_interface_mode || is_query_proc) {
    bool_t include_identity_columns = misc_attrs != NULL ? find_identity_columns(misc_attrs, no_op, NULL) != 0 : 0;
    bprintf(&body, rt->cql_result_set_has_identity_columns, include_identity_columns ? "true" : "false");
  }

  CHARBUF_OPEN(cg_java_output);
  CSTR custom_class_import = is_string_column_encoded ? rt->cql_string_ref_encode_include : "";
  bprintf(&cg_java_output, "%s", rt->source_prefix);
  bprintf(&cg_java_output, rt->source_wrapper_begin, options.java_package_name, custom_class_import);
  cg_java_write_imports(&cg_java_output, frag_type);
  cg_java_write_class_or_interface(
    &cg_java_output,
    frag_type,
    &class_name,
    &implements_interface,
    &body);

  cql_write_file(options.file_names[0], cg_java_output.ptr);

  CHARBUF_CLOSE(cg_java_output);
  CHARBUF_CLOSE(get_count_sig);
  CHARBUF_CLOSE(get_count);
  CHARBUF_CLOSE(implements_interface);
  CHARBUF_CLOSE(class_name);
  CHARBUF_CLOSE(body);

  java_context->generated_proc_count++;

  use_encode = 0;
  symtab_delete(encode_columns);
  encode_columns = NULL;
  encode_context_column = NULL;
}

static void cg_java_create_proc_stmt(ast_node *ast, cg_java_context *java_context) {
  Contract(is_ast_create_proc_stmt(ast));
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
    frag_type = find_fragment_attr_type(misc_attrs, NULL);

    if (frag_type == FRAG_TYPE_SHARED) {
      // shared fragment types generate no code ever
      continue;
    }

    if (frag_type == FRAG_TYPE_EXTENSION) {
      // shared fragment types generate no code ever
      continue;
    }

    if (!options.java_fragment_interface_mode) {
      // skiping the base fragment getters since generating in each extension
      // will cause collisions including two fragments headers
      if (frag_type == FRAG_TYPE_BASE) {
        continue;
      }
    }

    cg_java_one_stmt(stmt, java_context);
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
  symtab *frag_assembly_interfaces = symtab_new();
  CHARBUF_OPEN(frag_col_offsets_for_core);
  cg_java_context java_context = {
    .frag_col_offsets_for_core = &frag_col_offsets_for_core,
    .frag_assembly_interfaces = frag_assembly_interfaces,
  };
  cg_java_stmt_list(head, &java_context);
  CHARBUF_CLOSE(frag_col_offsets_for_core);
  symtab_delete(frag_assembly_interfaces);
}

cql_noexport void cg_java_cleanup() {
  SYMTAB_CLEANUP( encode_columns );
}

#endif

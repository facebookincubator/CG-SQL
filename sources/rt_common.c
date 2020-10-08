/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "cg_common.h"
#include "cg_c.h"
#include "cg_java.h"
#include "cg_schema.h"
#include "cg_json_schema.h"
#include "cg_test_helpers.h"
#include "cg_query_plan.h"
#include "cg_udf.h"
#include "cg_objc.h"
#include "rt.h"

// These are the various result types we can produce
// they include useful string fragments for the code generator

// patternlint-disable prefer-sized-ints-in-msys
// patternlint-disable windows-comptability-prefer-sized-longs-in-msys
// disabled due to many instances of "int" and "long" in a string or a comment

static rtdata rt_c = {
  .name = "c",
  .code_generator = &cg_c_main,
  .required_file_names_count = -1,
  .header_prefix =
    RT_IP_NOTICE("//")
    RT_AUTOGEN("//")
    "#pragma once\n\n",
  .cqlrt_template = "#include \"%s\"\n\n",
  .cqlrt = "cqlrt.h",
  .header_wrapper_begin = "",
  .header_wrapper_end = "",
  .source_prefix =
    RT_IP_NOTICE("//")
    RT_AUTOGEN("//") "\n",
  .source_wrapper_begin = "",
  .source_wrapper_end = "",
  .exports_prefix =
    RT_IP_NOTICE("--")
    RT_AUTOGEN("--") "\n",
  .symbol_case = cg_symbol_case_snake,
  .generate_type_getters = 0,
  .generate_equality_macros = 1,
  .symbol_prefix = "",
  .symbol_visibility = "extern ",
  .cql_contract = "cql_contract",
  .cql_log_database_error = "cql_log_database_error",
  .cql_hash_code = "cql_hash_code",
  .cql_bool = "cql_bool",
  .cql_int32 = "cql_int32",
  .cql_int64 = "cql_int64",
  .cql_double = "cql_double",
  .cql_code = "cql_code",
  .cql_blob_ref = "cql_blob_ref",
  .cql_blob_retain = "cql_blob_retain",
  .cql_blob_release = "cql_blob_release",
  .cql_object_ref = "cql_object_ref",
  .cql_object_retain = "cql_object_retain",
  .cql_object_release = "cql_object_release",
  .cql_string_ref = "cql_string_ref",
  .cql_string_ref_new = "cql_string_ref_new",
  .cql_string_literal = "cql_string_literal",
  .cql_string_retain = "cql_string_retain",
  .cql_string_release = "cql_string_release",
  .cql_string_hash = "cql_string_hash",
  .cql_blob_hash = "cql_blob_hash",
  .cql_string_compare = "cql_string_compare",
  .cql_string_equal = "cql_string_equal",
  .cql_string_like = "cql_string_like",
  .cql_alloc_cstr = "cql_alloc_cstr",
  .cql_free_cstr = "cql_free_cstr",
  .cql_result_set_ref = "cql_result_set_ref",
  .cql_result_set_ref_new = "cql_result_set_create",
  .cql_result_set_meta_struct = "cql_result_set_meta",
  .cql_result_set_get_meta = "cql_result_set_get_meta",
  .cql_result_set_retain = "cql_result_set_retain",
  .cql_result_set_release = "cql_result_set_release",
  .cql_result_set_get_count = "cql_result_set_get_count",
  .cql_result_set_get_data = "cql_result_set_get_data",
  .cql_result_set_get_bool = "cql_result_set_get_bool_col",
  .cql_result_set_get_double = "cql_result_set_get_double_col",
  .cql_result_set_get_int32 = "cql_result_set_get_int32_col",
  .cql_result_set_get_int64 = "cql_result_set_get_int64_col",
  .cql_result_set_get_string = "cql_result_set_get_string_col",
  .cql_result_set_get_blob = "cql_result_set_get_blob_col",
  .cql_result_set_get_is_null = "cql_result_set_get_is_null_col",
};

static rtdata rt_objc = {
  .name = "objc",
  .code_generator = &cg_objc_main,
  .required_file_names_count = 1,
  // note the @ is split from the generated so that tools don't think this is a generated file
  .header_prefix =
    RT_IP_NOTICE("//")
    RT_SIGNSRC("//")
    "#pragma once\n\n"
    "#import <Foundation/Foundation.h>\n",
  .header_wrapper_begin = "\nNS_ASSUME_NONNULL_BEGIN\n",
  .header_wrapper_end = "\nNS_ASSUME_NONNULL_END\n",
  .symbol_case = cg_symbol_case_pascal,
  .generate_type_getters = 1,
  .generate_equality_macros = 1,
  .symbol_prefix = RT_SYM_PREFIX,
  .impl_symbol_prefix = RT_IMPL_SYMBOL_PREFIX,
  .cql_hash_code = "NSUInteger",
  .cql_bool = "BOOL",
  .cql_int32 = "int32_t",
  .cql_int64 = "int64_t",
  .cql_double = "double",
  .cql_code = "int",
  .cql_blob_ref = "NSData *",
  .cql_object_ref = "NSObject *",
  .cql_string_ref = "NSString *",
};

static rtdata rt_java = {
  .name = "java",
  .code_generator = &cg_java_main,
  .required_file_names_count = 1,
  .source_prefix =
    RT_IP_NOTICE("//")
    RT_SIGNSRC("//") "\n",
  .source_wrapper_begin =
    "package %s; \n\n"
    "import " RT_JAVA_RT_PACKAGE ".CQLResultSet;\n"
    "import " RT_JAVA_RT_PACKAGE ".CQLViewModel;\n"
    "import javax.annotation.Nullable;\n\n",
  .symbol_case = cg_symbol_case_camel,
  .generate_type_getters = 1,
  .generate_equality_macros = 1,
  .register_proc_name = RT_REGISTER_PROC_NAME,
  .proc_should_generate_copy = RT_SHOULD_GENERATE_COPY,
  .symbol_prefix = "",
  .impl_symbol_prefix = "",
  .cql_hash_code = "long",
  .cql_bool = "boolean",
  .cql_int32 = "int",
  .cql_int64 = "long",
  .cql_double = "double",
  .cql_code = "int",
  .cql_blob_ref = "byte[]",
  .cql_object_ref = "Object",
  .cql_string_ref = "String",
  .cql_result_set_get_count =
    "public int getCount() {\n"
    "  return mResultSet.getCount();\n"
    "}\n\n",
  .cql_result_set_get_data =
    "public %s %s(%s) {\n"
    "  return mResultSet.get%s(%s, %s);\n"
    "}\n\n",
  .cql_bool_nullable = "Boolean",
  .cql_int32_nullable = "Integer",
  .cql_int64_nullable = "Long",
  .cql_double_nullable = "Double",
  .cql_java_tmp_class_def = "public final class %s extends CQLViewModel {\n\n",
  .cql_java_tmp_class_constructor =
    "public %s(CQLResultSet resultSet) {\n"
    "  super(resultSet);\n"
    "}\n\n",
  .cql_java_tmp_getter_nullable =
    "@Nullable\n"
    "public %s %s(%s) {\n"
    "  return mResultSet.get%s(%s, %s);\n"
    "}\n\n",
  .cql_result_set_has_identity_columns =
    "@Override \n"
    "protected boolean hasIdentityColumns() {\n"
    "  return %s;\n"
    "}\n\n",
  .cql_result_set_copy =
    "@Nullable\n"
    "public %s copy(int row, int count) {\n"
    "  CQLResultSet resultSet = mResultSet.copy(row, count); \n"
    "  if (resultSet == null) {\n"
    "    return null;\n"
    "  }\n"
    "  return new %s(resultSet);\n"
    "}\n\n",
};

static rtdata rt_schema_upgrade = {
  .name = "schema_upgrade",
  .code_generator = &cg_schema_upgrade_main,
  .required_file_names_count = 1,
  .source_prefix =
    RT_IP_NOTICE("--")
    RT_SIGNSRC("--") "\n",
  .symbol_case = cg_symbol_case_camel,
};

static rtdata rt_schema = {
  .name = "schema",
  .code_generator = &cg_schema_main,
  .required_file_names_count = 1,
  .source_prefix =
    RT_IP_NOTICE("--")
    RT_SIGNSRC("--") "\n",
  .symbol_case = cg_symbol_case_camel,
};

static rtdata rt_json_schema = {
  .name = "json_schema",
  .code_generator = &cg_json_schema_main,
  .required_file_names_count = 1,
  .source_prefix = "",
  .symbol_case = cg_symbol_case_camel,
};

static rtdata rt_test_helpers = {
  .name = "test_helpers",
  .code_generator = &cg_test_helpers_main,
  .required_file_names_count = 1,
  .source_prefix =
    RT_IP_NOTICE("--")
    RT_SIGNSRC("--") "\n",
};

static rtdata rt_query_plan = {
  .name = "query_plan",
  .code_generator = &cg_query_plan_main,
  .required_file_names_count = 1,
  .source_prefix =
    RT_IP_NOTICE("--")
    RT_SIGNSRC("--") "\n",
};

static rtdata rt_udf = {
  .name = "udf",
  .code_generator = &cg_udf_main,
  .required_file_names_count = 2,
  .header_prefix =
    RT_IP_NOTICE("//")
    RT_SIGNSRC("//")
    "#pragma once\n\n",
  .source_prefix =
    RT_IP_NOTICE("//")
    RT_SIGNSRC("//") "\n",
  .cqlrt_template = "#include \"%s\"\n\n",
  .cqlrt = "cqlrt.h",
};

static rtdata *(rt_all[]) = {
  &rt_c,
  &rt_objc,
  &rt_java,
  &rt_schema_upgrade,
  &rt_schema,
  &rt_json_schema,
  &rt_test_helpers,
  &rt_query_plan,
  &rt_udf,
  RT_EXTRAS
  NULL,
};

cql_noexport rtdata *find_rtdata(CSTR name) {
  rt_cleanup();

  int32_t i = 0;
  rtdata *rt_ = NULL;
  while ((rt_ = rt_all[i])) {
    if (!strcmp(rt_->name, name)) {
       break;
    }
    i++;
  }

  // the result type can override this, we don't want to check both places so normalize to the option.
  if (rt_) {
    options.generate_type_getters |= rt_->generate_type_getters;
  }

  return rt_;
}

cql_noexport void rt_cleanup() {
  RT_EXTRA_CLEANUP
}

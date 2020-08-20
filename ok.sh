#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CQL_DIR=.
OUT_DIR="out"
TEST_DIR="test"
cd ${CQL_DIR}


normalize_lines() {
  sed -e "s/The statement ending at line .*/The statement ending at line XXXX/" \
      -e "s/\.sql:[0-9]* :/.sql:XXXX :/" <"$@" >${OUT_DIR}/__temp
  cp ${OUT_DIR}/__temp "$@"
}

cp ${OUT_DIR}/test.out ${TEST_DIR}/test.ref 2>/dev/null
cp ${OUT_DIR}/schema.out ${TEST_DIR}/schema.ref 2>/dev/null
cp ${OUT_DIR}/dottest.out ${TEST_DIR}/dottest.ref 2>/dev/null
cp ${OUT_DIR}/sem_test.out ${TEST_DIR}/sem_test.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_err.out ${TEST_DIR}/sem_test_err.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_dev.out ${TEST_DIR}/sem_test_dev.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_dev_err.out ${TEST_DIR}/sem_test_dev_err.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_migrate.out ${TEST_DIR}/sem_test_migrate.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_migrate_err.out ${TEST_DIR}/sem_test_migrate_err.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_prev.out ${TEST_DIR}/sem_test_prev.ref 2>/dev/null
cp ${OUT_DIR}/sem_test_prev_err.out ${TEST_DIR}/sem_test_prev_err.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c.out.c ${TEST_DIR}/cg_test_c.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c.out.h ${TEST_DIR}/cg_test_c_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_exports.out ${TEST_DIR}/cg_test_exports.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c_with_namespace.out.c ${TEST_DIR}/cg_test_c_with_namespace.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c_with_namespace.out.h ${TEST_DIR}/cg_test_c_header_with_namespace.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c_with_type_getters.out.c ${TEST_DIR}/cg_test_c_with_type_getters.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_c_with_type_getters.out.h ${TEST_DIR}/cg_test_c_header_with_type_getters.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_c.out.c ${TEST_DIR}/cg_extension_fragment_test_c.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_c.out.h ${TEST_DIR}/cg_extension_fragment_test_c_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_c.out.c ${TEST_DIR}/cg_assembly_query_test_c.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_c.out.h ${TEST_DIR}/cg_assembly_query_test_c_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_msys.out.c ${TEST_DIR}/cg_extension_fragment_test_msys.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_msys.out.h ${TEST_DIR}/cg_extension_fragment_test_msys_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_msys.out.c ${TEST_DIR}/cg_assembly_query_test_msys.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_msys.out.h ${TEST_DIR}/cg_assembly_query_test_msys_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_msys_schema_utils.out.c ${TEST_DIR}/cg_extension_fragment_test_msys_schema_utils.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_msys_schema_utils.out.h ${TEST_DIR}/cg_extension_fragment_test_msys_schema_utils_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_msys_schema_utils.out.c ${TEST_DIR}/cg_assembly_query_test_msys_schema_utils.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_msys_schema_utils.out.h ${TEST_DIR}/cg_assembly_query_test_msys_schema_utils_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_json_schema.out ${TEST_DIR}/cg_test_json_schema.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_test_helpers.out ${TEST_DIR}/cg_test_test_helpers.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_schema_upgrade.out ${TEST_DIR}/cg_test_schema_upgrade.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_schema_partial_upgrade.out ${TEST_DIR}/cg_test_schema_partial_upgrade.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_schema_prev.out ${TEST_DIR}/cg_test_schema_prev.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_java_not_nullable_proc.out ${TEST_DIR}/cg_test_java_not_nullable_proc.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_java_nullable_proc.out ${TEST_DIR}/cg_test_java_nullable_proc.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_java_out_union.out ${TEST_DIR}/cg_test_java_out_union.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_java.out ${TEST_DIR}/cg_extension_fragment_test_java.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_java.out ${TEST_DIR}/cg_assembly_query_test_java.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_objc.out ${TEST_DIR}/cg_test_objc_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_test_objc.out ${TEST_DIR}/cg_extension_fragment_test_objc_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_extension_fragment_with_core_base_test_objc.out ${TEST_DIR}/cg_extension_fragment_with_core_base_test_objc_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_assembly_query_test_objc.out ${TEST_DIR}/cg_assembly_query_test_objc_header.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_msys_schema_utils_with_namespace.out.c ${TEST_DIR}/cg_test_msys_schema_utils_with_namespace.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_msys_schema_utils_with_namespace.out.h ${TEST_DIR}/cg_test_msys_schema_utils_header_with_namespace.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_query_plan.out ${TEST_DIR}/cg_test_query_plan.ref 2>/dev/null
cp ${OUT_DIR}/cg_test_query_plan_js.out ${TEST_DIR}/cg_test_query_plan_view.ref 2>/dev/null
cp ${OUT_DIR}/udf.c ${TEST_DIR}/cg_test_udf_c.ref 2>/dev/null
cp ${OUT_DIR}/udf.h ${TEST_DIR}/cg_test_udf_header.ref 2>/dev/null
cp ${OUT_DIR}/badpath_err.out ${TEST_DIR}/badpath_err.ref 2>/dev/null
cp ${OUT_DIR}/unwriteable_err.out ${TEST_DIR}/unwriteable_err.ref 2>/dev/null
cp ${OUT_DIR}/cg_1_2_err.out ${TEST_DIR}/cg_1_2_err.ref 2>/dev/null
cp ${OUT_DIR}/sem_abort_err.out ${TEST_DIR}/sem_abort_err.ref 2>/dev/null
cp ${OUT_DIR}/invalid_arg_err.out ${TEST_DIR}/invalid_arg_err.ref 2>/dev/null
cp ${OUT_DIR}/cg_requires_file_err.out ${TEST_DIR}/cg_requires_file_err.ref 2>/dev/null
cp ${OUT_DIR}/generate_file_type_err.out ${TEST_DIR}/generate_file_type_err.ref 2>/dev/null
cp ${OUT_DIR}/generate_file_file_err.out ${TEST_DIR}/generate_file_file_err.ref 2>/dev/null
cp ${OUT_DIR}/rt_arg_missing_err.out ${TEST_DIR}/rt_arg_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/rt_arg_bogus_err.out ${TEST_DIR}/rt_arg_bogus_err.ref 2>/dev/null
cp ${OUT_DIR}/cqlrt_arg_missing_err.out ${TEST_DIR}/cqlrt_arg_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/global_proc_missing_err.out ${TEST_DIR}/global_proc_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/objc_include_missing_err.out ${TEST_DIR}/objc_include_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/in_arg_missing_err.out ${TEST_DIR}/in_arg_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/c_include_namespace_missing_err.out ${TEST_DIR}/c_include_namespace_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_many_procs_err.out ${TEST_DIR}/java_rt_many_procs_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_no_package_err.out ${TEST_DIR}/java_rt_no_package_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_no_package_flag_err.out ${TEST_DIR}/java_rt_no_package_flag_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_filename_end_slash_err.out ${TEST_DIR}/java_rt_filename_end_slash_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_filename_end_dot_err.out ${TEST_DIR}/java_rt_filename_end_dot_err.ref 2>/dev/null
cp ${OUT_DIR}/java_rt_filename_no_base_err.out ${TEST_DIR}/java_rt_filename_no_base_err.ref 2>/dev/null
cp ${OUT_DIR}/simple_error_err.out ${TEST_DIR}/simple_error_err.ref 2>/dev/null
cp ${OUT_DIR}/prev_and_codegen_incompat_err.out ${TEST_DIR}/prev_and_codegen_incompat_err.ref 2>/dev/null
cp ${OUT_DIR}/gen_exports_args_err.out ${TEST_DIR}/gen_exports_args_err.ref 2>/dev/null
cp ${OUT_DIR}/inc_invalid_regions_err.out ${TEST_DIR}/inc_invalid_regions_err.ref 2>/dev/null
cp ${OUT_DIR}/excl_invalid_regions_err.out ${TEST_DIR}/excl_invalid_regions_err.ref 2>/dev/null
cp ${OUT_DIR}/global_proc_needed_err.out ${TEST_DIR}/global_proc_needed_err.ref 2>/dev/null
cp ${OUT_DIR}/asm_query_ns_needed_err.out ${TEST_DIR}/asm_query_ns_needed_err.ref 2>/dev/null
cp ${OUT_DIR}/asm_query_ns_nonempty_err.out ${TEST_DIR}/asm_query_ns_nonempty_err.ref 2>/dev/null
cp ${OUT_DIR}/write_fail_err.out ${TEST_DIR}/write_fail_err.ref 2>/dev/null
cp ${OUT_DIR}/java_classname_missing_err.out ${TEST_DIR}/java_classname_missing_err.ref 2>/dev/null
cp ${OUT_DIR}/java_classname_noargs_err.out ${TEST_DIR}/java_classname_noargs_err.ref 2>/dev/null
cp ${OUT_DIR}/vscode_test.out ${TEST_DIR}/vscode_test.ref 2>/dev/null

echo normalizing line numbers in all .ref files
for f in ${TEST_DIR}/*.ref
do
  normalize_lines "$f"
done

printf "\n"
echo having run ok.sh, you certify that the current output is correct, or at least ready to be reviewed
printf "\n"

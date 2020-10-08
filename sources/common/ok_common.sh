#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC1091
source common/test_helpers.sh || exit 1

copy_ref() {
  normalize_lines "${OUT_DIR}/$1" 2>/dev/null
  cp "${OUT_DIR}/$1" "${TEST_DIR}/$1.ref" 2>/dev/null
}

copy_ref alt_cqlrt.err
copy_ref asm_query_ns_needed.err
copy_ref asm_query_ns_nonempty.err
copy_ref badpath.err
copy_ref bigquote.err
copy_ref c_include_namespace_missing.err
copy_ref cg_1_2.err
copy_ref cg_extension_fragment_with_core_base_test_objc.out
copy_ref cg_requires_file.err
copy_ref cg_test_assembly_query_c.c
copy_ref cg_test_assembly_query_c.h
copy_ref cg_test_assembly_query_java.out
copy_ref cg_test_assembly_query_objc.out
copy_ref cg_test_c.c
copy_ref cg_test_c.h
copy_ref cg_test_c.err
copy_ref cg_test_c_with_namespace.c
copy_ref cg_test_c_with_namespace.h
copy_ref cg_test_c_with_type_getters.c
copy_ref cg_test_c_with_type_getters.h
copy_ref cg_test_exports.out
copy_ref cg_test_extension_fragment_c.c
copy_ref cg_test_extension_fragment_c.h
copy_ref cg_test_extension_fragment_java.out
copy_ref cg_test_extension_fragment_objc.out
copy_ref cg_test_java.err
copy_ref cg_test_java_not_nullable_proc.out
copy_ref cg_test_java_nullable_proc.out
copy_ref cg_test_java_out_union.out
copy_ref cg_test_json_schema.out
copy_ref cg_test_objc.out
copy_ref cg_test_objc.err
copy_ref cg_test_query_plan.out
copy_ref cg_test_query_plan_js.out
copy_ref cg_test_schema_partial_upgrade.out
copy_ref cg_test_schema_partial_upgrade.err
copy_ref cg_test_schema_prev.out
copy_ref cg_test_schema_prev.err
copy_ref cg_test_schema_upgrade.out
copy_ref cg_test_schema_upgrade.err
copy_ref cg_test_test_helpers.out
copy_ref cqlrt_arg_missing.err
copy_ref dasm_test.out
copy_ref dottest.out
copy_ref excl_invalid_regions.err
copy_ref gen_exports_args.err
copy_ref generate_file_file.err
copy_ref generate_file_type.err
copy_ref global_proc_missing.err
copy_ref global_proc_needed.err
copy_ref in_arg_missing.err
copy_ref inc_invalid_regions.err
copy_ref invalid_arg.err
copy_ref java_classname_missing.err
copy_ref java_classname_noargs.err
copy_ref java_rt_filename_end_dot.err
copy_ref java_rt_filename_end_slash.err
copy_ref java_rt_filename_no_base.err
copy_ref java_rt_many_procs.err
copy_ref java_rt_no_package.err
copy_ref java_rt_no_package_flag.err
copy_ref no_dasm_globals.err
copy_ref objc_include_missing.err
copy_ref objc_no_results.err
copy_ref prev_and_codegen_incompat.err
copy_ref rt_arg_bogus.err
copy_ref rt_arg_missing.err
copy_ref schema.out
copy_ref sem_abort.err
copy_ref sem_test.out
copy_ref sem_test_dev.out
copy_ref sem_test_dev.err
copy_ref sem_test.err
copy_ref sem_test_migrate.out
copy_ref sem_test_migrate.err
copy_ref sem_test_prev.out
copy_ref sem_test_prev.err
copy_ref simple_error.err
copy_ref test.out
copy_ref udf.c
copy_ref udf.h
copy_ref unsupported_in_dasm.err
copy_ref unwriteable.err
copy_ref write_fail.err

copy_extras

printf "\n"
echo having run ok.sh, you certify that the current output is correct, or at least ready to be reviewed
printf "\n"

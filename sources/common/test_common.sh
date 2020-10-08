#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

MAKE_ARGS="${MAKE_COVERAGE_ARGS}"

failed() {
  echo '--------------------------------- FAILED'
  make_clean_msg
  exit 1
}

do_make() {
  if [ "${MAKE_ARGS}" == "" ]
  then
    # we don't want to send empty strings "" to make, so avoid that
    make "$@"
  else
    make "$@" "${MAKE_ARGS}"
  fi
}

sem_check() {
  ${CQL} "$@"
  if [ "$?" -ne "1" ]
  then
     echo 'All semantic analysis checks have errors in the test'
     echo 'the normal return code is "1" -- any other return code is bad news'
     echo 'A return code of zero indicates we reported success in the face of errors'
     echo 'A return code other than 1 indicates an unexpected fatal error of some type'
     return 1
  fi
}


building() {
  echo '--------------------------------- STAGE 1 -- make clean, then make'
  echo building new cql
  do_make clean >/dev/null 2>/dev/null
  if ! do_make all >"${OUT_DIR}/build.out" 2>"${OUT_DIR}/build.err"
  then
    echo build cql failed:
    cat "${OUT_DIR}/build.err"
    failed
  fi

  if grep "^State.*conflicts:" "${OUT_DIR}/cql.y.output" >"${OUT_DIR}/build.err"
  then
    echo "conflicts found in grammar, these must be fixed" >>"${OUT_DIR}/build.err"
    echo "look at the conflicting states in" "${OUT_DIR}/cql.y.output" "to debug" >>"${OUT_DIR}/build.err"
    cat "${OUT_DIR}/build.err"
    failed
  fi

  echo building cql amalgam
  if ! do_make amalgam >"${OUT_DIR}/build.out" 2>"${OUT_DIR}/build.err"
  then
    echo build cql amalgam failed:
    cat "${OUT_DIR}/build.err"
    failed
  fi

  echo building cql-verify
  if ! (do_make cql-verify) 2>"${OUT_DIR}/build.err"
  then
    echo build cql-verify failed:
    cat "${OUT_DIR}/build.err"
    failed
  fi

  echo building json-test
  if ! (do_make json-test) 2>"${OUT_DIR}/build.err"
  then
    echo build json-test failed:
    cat "${OUT_DIR}/build.err"
    failed
  fi
}

create_unwritable_file() {
  rm -f "$1"
  echo x >"$1"
  chmod -rw "$1"
}

basic_test() {
  echo '--------------------------------- STAGE 2 -- BASIC PARSING TEST'
  echo running "${TEST_DIR}/test.sql"
  if ! ${CQL} --dev --in "${TEST_DIR}/test.sql" >"${OUT_DIR}/test.out"
  then
   echo basic parsing test failed
   failed
  fi
  echo "  computing diffs (empty if none)"
  on_diff_exit test.out
}

dot_test() {
  echo '--------------------------------- STAGE 3 -- .DOT OUTPUT TEST'
  echo running "${TEST_DIR}/dottest.sql"
  if ! ${CQL} --dot --in "${TEST_DIR}/dottest.sql" >"${OUT_DIR}/dottest.out"
  then
    echo DOT syntax test failed
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit dottest.out
}

semantic_test() {
  echo '--------------------------------- STAGE 4 -- SEMANTIC ANALYSIS TEST'
  echo running semantic analysis test
  if ! sem_check --sem --print --dev --in "${TEST_DIR}/sem_test.sql" >"${OUT_DIR}/sem_test.out" 2>"${OUT_DIR}/sem_test.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/sem_test.err"
     failed
  fi

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/sem_test.sql" "${OUT_DIR}/sem_test.out"
  then
    echo failed verification
    failed
  fi

  echo running dev semantic analysis test
  if ! sem_check --sem --print --in "${TEST_DIR}/sem_test_dev.sql" >"${OUT_DIR}/sem_test_dev.out" 2>"${OUT_DIR}/sem_test_dev.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/sem_test_dev.err"
     failed
  fi

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/sem_test_dev.sql" "${OUT_DIR}/sem_test_dev.out"
  then
    echo failed verification
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit sem_test.out
  on_diff_exit sem_test.err
  on_diff_exit sem_test_dev.out
  on_diff_exit sem_test_dev.err
}

code_gen_c_test() {
  echo '--------------------------------- STAGE 5 -- C CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_c.h" "${OUT_DIR}/cg_test_c.c" "${OUT_DIR}/cg_test_exports.out" --in "${TEST_DIR}/cg_test.sql" --global_proc cql_startup --generate_copy --generate_exports 2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test.sql" "${OUT_DIR}/cg_test_c.c"
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test with type getters enabled
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_c_with_type_getters.h" "${OUT_DIR}/cg_test_c_with_type_getters.c" --in "${TEST_DIR}/cg_test_c_type_getters.sql" --global_proc cql_startup --generate_copy --generate_type_getters  2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_c_type_getters.sql" "${OUT_DIR}/cg_test_c_with_type_getters.h"
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test with namespace enabled
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_c_with_namespace.h" "${OUT_DIR}/cg_test_c_with_namespace.c" "${OUT_DIR}/cg_test_imports_with_namespace.ref" --in "${TEST_DIR}/cg_test.sq"l --global_proc cql_startup --generate_copy --c_include_namespace test_namespace --generate_exports 2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test.sql" "${OUT_DIR}/cg_test_c_with_namespace.c"
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test for extension query fragment
  if ! ${CQL} --generate_type_getters --test --cg "${OUT_DIR}/cg_test_extension_fragment_c.h" "${OUT_DIR}/cg_test_extension_fragment_c.c" "${OUT_DIR}/cg_test_extension_fragment_imports.ref" --in "${TEST_DIR}/cg_test_extension_fragment.sql" --global_proc cql_startup --generate_copy --generate_exports 2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_extension_fragment.sql" "${OUT_DIR}/cg_test_extension_fragment_c.c"
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test for assembly query
  if ! ${CQL} --generate_type_getters --test --cg "${OUT_DIR}/cg_test_assembly_query_c.h" "${OUT_DIR}/cg_test_assembly_query_c.c" "${OUT_DIR}/cg_test_assembly_query_imports.ref" --in "${TEST_DIR}/cg_test_assembly_query.sql" --global_proc cql_startup --generate_copy --generate_exports 2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_assembly_query.sql" "${OUT_DIR}/cg_test_assembly_query_c.c"
  then
    echo "ERROR: failed c query fragment verification"
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_c.c
  on_diff_exit cg_test_c.h
  on_diff_exit cg_test_c_with_namespace.c
  on_diff_exit cg_test_c_with_namespace.h
  on_diff_exit cg_test_c_with_type_getters.c
  on_diff_exit cg_test_c_with_type_getters.h
  on_diff_exit cg_test_exports.out
  on_diff_exit cg_test_extension_fragment_c.c
  on_diff_exit cg_test_extension_fragment_c.h
  on_diff_exit cg_test_assembly_query_c.c
  on_diff_exit cg_test_assembly_query_c.h
  on_diff_exit cg_test_c.err

  echo "  compiling code"

  if ! do_make cg_test
  then
    echo CQL generated invalid C code
    failed
  fi

  echo "  compiling query fragment code"
  if ! do_make cg_test_extension_fragment
  then
    echo CQL generated invalid C code for extension query fragment
    failed
  fi

  if ! do_make cg_test_assembly_query
  then
    echo CQL generated invalid C code for assembly query
    failed
  fi
}

code_gen_java_test() {
  echo '--------------------------------- STAGE 6 -- JAVA CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_java.out" --in "${TEST_DIR}/cg_test_no_result_set.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo validating empty codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_java.err

  echo running java with suppressed getters
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_java.out" --in "${TEST_DIR}/cg_test_suppressed.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo validating empty codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_java.err

  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_java_not_nullable_proc.out" --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_java_nullable_proc.out" --in "${TEST_DIR}/cg_test_single_proc_nullable.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo running java codegen test for extension query fragment
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_extension_fragment_java.out" --in "${TEST_DIR}/cg_test_extension_java_fragment.sql" --rt java --java_package_name com.facebook.cqlviewmodels --java_assembly_query_classname com.facebook.cqlviewmodels.cg_test_assembly_query_java 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo running java codegen test for assembly query fragment
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_assembly_query_java.out" --in "${TEST_DIR}/cg_test_assembly_query.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo running java codegen test for out union
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_java_out_union.out" --in "${TEST_DIR}/cg_test_out_union.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/cg_test_java.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_java.err"
    failed
  fi

  echo validating codegen
  echo "  computing diffs (empty if none)"

  on_diff_exit cg_test_java_not_nullable_proc.out
  on_diff_exit cg_test_java_nullable_proc.out
  on_diff_exit cg_test_extension_fragment_java.out
  on_diff_exit cg_test_assembly_query_java.out
  on_diff_exit cg_test_java_out_union.out
}

code_gen_objc_test() {
  echo '--------------------------------- STAGE 7 -- OBJ-C CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test.sql" --rt objc --objc_assembly_query_namespace . 2>"${OUT_DIR}/cg_test_objc.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_objc.err"
    failed
  fi

  echo validating codegen
  echo "  check that the objc_c_include_path argument was used"
  if ! grep "<Test/TestFile.h>" "${OUT_DIR}/cg_test_objc.out"
  then
    echo error etc.
    failed
  fi

  echo running objc codegen test for extension query fragment
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_extension_fragment_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test_extension_fragment.sql" --rt objc --objc_assembly_query_namespace . 2>"${OUT_DIR}/cg_test_objc.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_objc.err"
    failed
  fi

  echo running objc codegen test for extension query fragment with core base
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_extension_fragment_with_core_base_test_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test_extension_fragment.sql" --rt objc --objc_assembly_query_namespace MessengerBridgeQueries 2>"${OUT_DIR}/cg_test_objc.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_objc.err"
    failed
  fi

  echo running objc codegen test for assembly query fragment
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_assembly_query_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test_assembly_query.sql" --rt objc 2>"${OUT_DIR}/cg_test_objc.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_objc.err"
    failed
  fi

  echo validating codegen
  echo "  computing diffs (empty if none)"

  on_diff_exit cg_test_objc.out
  on_diff_exit cg_test_extension_fragment_objc.out
  on_diff_exit cg_extension_fragment_with_core_base_test_objc.out
  on_diff_exit cg_test_assembly_query_objc.out
  on_diff_exit cg_test_objc.err
}

assorted_errors_test() {
  echo '--------------------------------- STAGE 8 -- FAST FAIL CASES'
  echo running various failure cases that cause no output

# the output path doesn't exist, should cause an error

  if ${CQL} --in /xx/yy/zz 2>"${OUT_DIR}/badpath.err"
  then
    echo "reading from non-existant file should have failed, but didn't"
    failed
  fi

  on_diff_exit badpath.err

# the output file is not writeable, should cause an error

  if ${CQL} --cg /xx/yy/zz /xx/yy/zzz --in "${TEST_DIR}/cg_test.sql" --global_proc xx 2>"${OUT_DIR}/unwriteable.err"
  then
    echo "failed writing to unwriteable file should have failed, but didn't"
    failed
  fi

  on_diff_exit unwriteable.err

# wrong number of args specified in --cg (for objc)

  if ${CQL} --cg __temp __temp2 --in "${TEST_DIR}/cg_test.sql" --rt objc 2>"${OUT_DIR}/cg_1_2.err"
  then
    echo "objc rt should require 1 files for the cg param but two were passed, should have failed"
    failed
  fi

# semantic errors should abort output (we'll not try to write)

  on_diff_exit cg_1_2.err

  if ${CQL} --cg __temp /xx/yy/zz --in "${TEST_DIR}/semantic_error.sql" 2>"${OUT_DIR}/sem_abort.err"
  then
    echo "simple semantic error to abort output -- failed"
    failed
  fi

  on_diff_exit sem_abort.err

# no result sets in the input for objc should result in empty output, not errors

  if ! ${CQL} --cg __temp --in "${TEST_DIR}/noresult.sql" --objc_c_include_path dummy --rt objc 2>"${OUT_DIR}/objc_no_results.err"
  then
    echo "no result sets in output objc case, should not fail"
    failed
  fi

  on_diff_exit objc_no_results.err

# bogus arg should report error

  if ${CQL} --garbonzo!! 2>"${OUT_DIR}/invalid_arg.err"
  then
    echo "invalid arg should report error -- failed"
    failed
  fi

  on_diff_exit invalid_arg.err

# --cg did not have any following args, should force an error

  if ${CQL} --cg 2>"${OUT_DIR}/cg_requires_file.err"
  then
    echo "failed to require a file name with --cg"
    failed
  fi

  on_diff_exit cg_requires_file.err

# --generate_file_type did not specify a file type

  if ${CQL} --generate_file_type 2>"${OUT_DIR}/generate_file_type.err"
  then
    echo "failed to require a file type with --generate_file_type"
    failed
  fi

  on_diff_exit generate_file_type.err

# --generate_file_type specified invalid file type (should cause an error)

  if ${CQL} --generate_file_type foo 2>"${OUT_DIR}/generate_file_file.err"
  then
    echo "failed to require a valid file type with --generate_file_type"
    failed
  fi

  on_diff_exit generate_file_file.err

# --rt specified with no arg following it

  if ${CQL} --rt 2>"${OUT_DIR}/rt_arg_missing.err"
  then
    echo "failed to require a runtime with --rt"
    failed
  fi

  on_diff_exit rt_arg_missing.err

# invalid result type specified with --rt, should force an error

  if ${CQL} --rt foo 2>"${OUT_DIR}/rt_arg_bogus.err"
  then
    echo "failed to require a valid result type with --rt"
    failed
  fi

  on_diff_exit rt_arg_bogus.err

# --cqlrt specified but no file name present, should force an error

  if ${CQL} --cqlrt 2>"${OUT_DIR}/cqlrt_arg_missing.err"
  then
    echo "failed to require a file arg  with --cqlrt"
    failed
  fi

  on_diff_exit cqlrt_arg_missing.err

# --global_proc has no proc name

  if ${CQL} --global_proc 2>"${OUT_DIR}/global_proc_missing.err"
  then
    echo "failed to require a procedure name with --global_proc"
    failed
  fi

  on_diff_exit global_proc_missing.err

# objc_c_include_path had no path

  if ${CQL} --objc_c_include_path 2>"${OUT_DIR}/objc_include_missing.err"
  then
    echo "failed to require an include path with --objc_c_include_path"
    failed
  fi

  on_diff_exit objc_include_missing.err

# --in arg missing

  if ${CQL} --in 2>"${OUT_DIR}/in_arg_missing.err"
  then
    echo "failed to require a file name with --in"
    failed
  fi

  on_diff_exit in_arg_missing.err

# no c_include_namespace arg

  if ${CQL} --c_include_namespace 2>"${OUT_DIR}/c_include_namespace_missing.err"
  then
    echo "failed to require a C namespace with --c_include_namespace"
    failed
  fi

  on_diff_exit c_include_namespace_missing.err

# more than one proc in java file

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test.sql" --cg "${OUT_DIR}/dummy.out" --java_package_name dummy 2>"${OUT_DIR}/java_rt_many_procs.err"
  then
    echo "failed aborting a java rt codegen with more than one proc"
    failed
  fi

  on_diff_exit java_rt_many_procs.err

# package flag has no arg

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --cg "${OUT_DIR}/dummy.out" --java_package_name 2>"${OUT_DIR}/java_rt_no_package.err"
  then
    echo "failed to require a java package with --java_package_name"
    failed
  fi

  on_diff_exit java_rt_no_package.err

# java package flag missing

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --cg "${OUT_DIR}/dummy.out" 2>"${OUT_DIR}/java_rt_no_package_flag.err"
  then
    echo "failed to require --java_package_name"
    failed
  fi

  on_diff_exit java_rt_no_package_flag.err

# java output filename ends with /

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --cg "${OUT_DIR}/dummy/" --java_package_name package 2>"${OUT_DIR}/java_rt_filename_end_slash.err"
  then
    echo "failed to require output file name that doesn't end with /"
    failed
  fi

  on_diff_exit java_rt_filename_end_slash.err

# java output filename ends with .

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --cg "${OUT_DIR}/dummy." --java_package_name package 2>"${OUT_DIR}/java_rt_filename_end_dot.err"
  then
    echo "failed to require output file doesn't end with ."
    failed
  fi

  on_diff_exit java_rt_filename_end_dot.err

# java output filename has no basename (e.g. foo/bar/.java)

  if ${CQL} --rt java --in "${TEST_DIR}/cg_test_single_proc_not_nullable.sql" --cg "${OUT_DIR}/.java" --java_package_name package 2>"${OUT_DIR}/java_rt_filename_no_base.err"
  then
    echo "failed to require output file with a base name"
    failed
  fi

  on_diff_exit java_rt_filename_no_base.err
}

schema_migration_test() {
  echo '--------------------------------- STAGE 9 -- SCHEMA MIGRATION TESTS'
  echo running semantic analysis for migration test
  if ! sem_check --sem --print --in "${TEST_DIR}/sem_test_migrate.sql" >"${OUT_DIR}/sem_test_migrate.out" 2>"${OUT_DIR}/sem_test_migrate.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/sem_test_migrate.err"
     failed
  fi

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/sem_test_migrate.sql" "${OUT_DIR}/sem_test_migrate.out"
  then
    echo failed verification
    failed
  fi
  echo "  computing diffs (empty if none)"
  on_diff_exit sem_test_migrate.out
  on_diff_exit sem_test_migrate.err

  echo '---------------------------------'
  echo running a schema migrate proc test
  if ! sem_check --sem --in "${TEST_DIR}/schema_version_error.sql" --print >"${OUT_DIR}/schema_version_error.out" 2>"${OUT_DIR}/schema_version_error.err.out"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/schema_version_error.err.out"
     failed
  fi;

  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/schema_version_error.sql" "${OUT_DIR}/schema_version_error.out"
  then
    echo failed verification
    failed
  fi

  echo '---------------------------------'
  echo running semantic analysis for previous schema error checks test
  if ! sem_check --sem --print --exclude_regions high_numbered_thing --in "${TEST_DIR}/sem_test_prev.sql" >"${OUT_DIR}/sem_test_prev.out" 2>"${OUT_DIR}/sem_test_prev.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/sem_test_prev.err"
     failed
  fi;

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/sem_test_prev.sql" "${OUT_DIR}/sem_test_prev.out"
  then
    echo failed verification
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit sem_test_prev.out
  on_diff_exit sem_test_prev.err

  echo '---------------------------------'
  echo running code gen for migration test

  if ! ${CQL} --cg "${OUT_DIR}/cg_test_schema_upgrade.out" --in "${TEST_DIR}/cg_test_schema_upgrade.sql" --global_proc test --rt schema_upgrade 2>"${OUT_DIR}/cg_test_schema_upgrade.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_schema_upgrade.err"
    failed
  fi

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_schema_upgrade.sql" "${OUT_DIR}/cg_test_schema_upgrade.out"
  then
    echo failed verification
    failed
  fi

  echo "  compiling the upgrade script with CQL"
  if ! ${CQL} --cg "${OUT_DIR}/cg_test_schema_upgrade.h" "${OUT_DIR}/cg_test_schema_upgrade.c" --in "${OUT_DIR}/cg_test_schema_upgrade.out"
  then
    echo CQL compilation failed
    failed;
  fi

  echo "  compiling the upgrade script with C"
  if ! do_make cg_test_schema_upgrade
  then
    echo CQL migration script compilation failed.
    failed;
  fi

  echo "  computing diffs (empty if none)"

  on_diff_exit cg_test_schema_upgrade.out
  on_diff_exit cg_test_schema_upgrade.err

  echo '---------------------------------'
  echo running code gen to produce previous schema

  if ! ${CQL} --cg "${OUT_DIR}/cg_test_schema_prev.out" --in "${TEST_DIR}/cg_test_schema_upgrade.sql" --rt schema 2>"${OUT_DIR}/cg_test_schema_prev.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_schema_prev.err"
    failed
  fi


  echo combining geneated previous schema with itself to ensure it self validates

  cat "${OUT_DIR}/cg_test_schema_prev.out" > "${OUT_DIR}/prev_loop.out"
  echo "@previous_schema;" >> "${OUT_DIR}/prev_loop.out"
  cat "${OUT_DIR}/cg_test_schema_prev.out" >> "${OUT_DIR}/prev_loop.out"

  if ! ${CQL} --cg "${OUT_DIR}/prev_twice.out" --in "${OUT_DIR}/prev_loop.out" --rt schema 2>"${OUT_DIR}/cg_test_schema_prev_twice.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_schema_prev_twice.err"
    failed
  fi

  echo comparing the generated previous schema from that combination and it should be identical to the original

  if ! ${CQL} --cg "${OUT_DIR}/prev_thrice.out" --in "${OUT_DIR}/prev_twice.out" --rt schema 2>"${OUT_DIR}/cg_test_schema_prev_thrice.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_schema_prev_thrice.err"
    failed
  fi

  echo "  computing diffs after several applications (empty if none)"
  __on_diff_exit "${OUT_DIR}/cg_test_schema_prev.out" "${OUT_DIR}/prev_twice.out"
  __on_diff_exit "${OUT_DIR}/prev_twice.out" "${OUT_DIR}/prev_thrice.out"

  echo "  computing previous schema diffs from reference (empty if none)"
  on_diff_exit cg_test_schema_prev.out
  on_diff_exit cg_test_schema_prev.err

  echo "  running schema migration with include/exclude args"
  if ! ${CQL} --cg "${OUT_DIR}/cg_test_schema_partial_upgrade.out" --in "${TEST_DIR}/cg_test_schema_upgrade.sql" --global_proc test --rt schema_upgrade --include_regions extra --exclude_regions shared 2>"${OUT_DIR}/cg_test_schema_partial_upgrade.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_schema_partial_upgrade.err"
    failed
  fi

  echo "  compiling the upgrade script with CQL"
  if ! ${CQL} --cg "${OUT_DIR}/cg_test_schema_partial_upgrade.h" "${OUT_DIR}/cg_test_schema_partial_upgrade.c" --in "${OUT_DIR}/cg_test_schema_partial_upgrade.out"
  then
    echo CQL compilation failed
    failed;
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_schema_partial_upgrade.out
  on_diff_exit cg_test_schema_partial_upgrade.err
}

misc_cases() {
  echo '--------------------------------- STAGE 10 -- MISC CASES'
  echo running simple error test
  if ${CQL} --in "${TEST_DIR}/error.sql" >"${OUT_DIR}/error.out" 2>"${OUT_DIR}/simple_error.err"
  then
    echo simple error test failed
    failed
  fi

  on_diff_exit simple_error.err

  echo running previous schema and codegen incompatible test
  if ${CQL} --cg "${OUT_DIR}/__temp.h" "${OUT_DIR}/__temp.c" --in "${TEST_DIR}/cg_test_prev_invalid.sql" 2>"${OUT_DIR}/prev_and_codegen_incompat.err"
  then
    echo previous schema and codegen are supposed to be incompatible
    failed
  fi

  on_diff_exit prev_and_codegen_incompat.err

  echo running big quote test
  if ! ${CQL} --cg "${OUT_DIR}/__temp.h" "${OUT_DIR}/__temp.c" --in "${TEST_DIR}/bigquote.sql" --global_proc x >/dev/null 2>"${OUT_DIR}/bigquote.err"
  then
    echo big quote test failed
    failed
  fi

  on_diff_exit bigquote.err

  echo running alternate cqlrt.h test
  if ! ${CQL} --cg "${OUT_DIR}/__temp.h" "${OUT_DIR}/__temp.c" --in "${TEST_DIR}/cg_test.sql" --global_proc x --cqlrt alternate_cqlrt.h 2>"${OUT_DIR}/alt_cqlrt.err"
  then
    echo alternate cqlrt test failed
    failed
  fi

  if ! grep alternate_cqlrt.h "${OUT_DIR}/__temp.h" >/dev/null
  then
    echo alternate cqlrt did not appear in the output header
    failed
  fi

  on_diff_exit alt_cqlrt.err

  echo running too few -cg arguments with --generate_exports test
  if ${CQL} --cg "${OUT_DIR}/__temp.c" "${OUT_DIR}/__temp.h" --in "${TEST_DIR}/cg_test.sql" --global_proc x --generate_exports 2>"${OUT_DIR}/gen_exports_args.err"
  then
    echo too few --cg args test failed
    failed
  fi

  on_diff_exit gen_exports_args.err

  echo running invalid include regions test
  if ${CQL} --cg "${OUT_DIR}/cg_test_schema_partial_upgrade.out" --in "${TEST_DIR}/cg_test_schema_upgrade.sql" --global_proc test --rt schema_upgrade --include_regions bogus --exclude_regions shared 2>"${OUT_DIR}/inc_invalid_regions.err"
  then
    echo invalid include region test failed
    failed
  fi

  on_diff_exit inc_invalid_regions.err

  echo running invalid exclude regions test
  if ${CQL} --cg "${OUT_DIR}/cg_test_schema_partial_upgrade.out" --in "${TEST_DIR}/cg_test_schema_upgrade.sql" --global_proc test --rt schema_upgrade --include_regions extra --exclude_regions bogus 2>"${OUT_DIR}/excl_invalid_regions.err"
  then
    echo invalid exclude region test failed
    failed
  fi

  on_diff_exit excl_invalid_regions.err

  echo running global proc is needed but not present test
  if ${CQL} --cg "${OUT_DIR}/__temp.c" "${OUT_DIR}/__temp.h" --in "${TEST_DIR}/bigquote.sql" 2>"${OUT_DIR}/global_proc_needed.err"
  then
    echo global proc needed but absent failed
    failed
  fi

  on_diff_exit global_proc_needed.err

  echo running assembly query namespace not provided for extension fragment test
  if ${CQL} --test --cg "${OUT_DIR}/cg_test_extension_fragment_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test_extension_fragment.sql" --rt objc 2>"${OUT_DIR}/asm_query_ns_needed.err"
  then
    echo assembly query namespace is required for extension fragment
    failed
  fi

  on_diff_exit asm_query_ns_needed.err

  echo running assembly query namespace provided empty for extension fragment test
  if ${CQL} --test --cg "${OUT_DIR}/cg_test_extension_fragment_objc.out" --objc_c_include_path Test/TestFile.h --in "${TEST_DIR}/cg_test_extension_fragment.sql" --rt objc --objc_assembly_query_namespace 2>"${OUT_DIR}/asm_query_ns_nonempty.err"
  then
    echo assembly query namespace is required to be non-empty for extension fragment
    failed
  fi

  on_diff_exit asm_query_ns_nonempty.err

  echo running test where output file cannot be written
  create_unwritable_file "${OUT_DIR}/unwritable.h.out"
  create_unwritable_file "${OUT_DIR}/unwritable.c.out"
  if ${CQL} --cg "${OUT_DIR}/unwritable.h".out "${OUT_DIR}/unwritable.c".out --in "${TEST_DIR}/cg_test.sql" --rt c --global_proc cql_startup 2>"${OUT_DIR}/write_fail.err"
  then
    echo writing should have failed
    failed
  fi

  on_diff_exit write_fail.err

  echo 'running missing --java_assembly_query_classname flag for extension fragment codegen test'
  if ${CQL} --test --cg "${OUT_DIR}/__temp.out" --in "${TEST_DIR}/cg_test_extension_java_fragment.sql" --rt java --java_package_name com.facebook.cqlviewmodels 2>"${OUT_DIR}/java_classname_missing.err"
  then
    echo '--java_assembly_query_classname was missing, this should have caused an error'
    failed
  fi

  on_diff_exit java_classname_missing.err

  echo 'running missing --java_assembly_query_classname args for extension fragment codegen test'
  if ${CQL} --test --cg "${OUT_DIR}/__temp.out" --in "${TEST_DIR}/cg_test_extension_java_fragment.sql" --rt java --java_package_name com.facebook.cqlviewmodels --java_assembly_query_classname 2>"${OUT_DIR}/java_classname_noargs.err"
  then
    echo '--java_assembly_query_classname had no arguments, this should have caused an error'
    failed
  fi

  on_diff_exit java_classname_noargs.err
}

json_schema_test() {
  echo '--------------------------------- STAGE 11 -- JSON SCHEMA TEST'
  echo running json schema test
  cc -DCQL_TEST -E -x c "${TEST_DIR}/cg_test_json_schema.sql" >"${OUT_DIR}/__temp"
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_json_schema.out" --in "${OUT_DIR}/__temp" --rt json_schema 2>"${OUT_DIR}/cg_test_json_schema.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_json_schema.err"
    failed
  fi

  echo validating json output
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_json_schema.sql" "${OUT_DIR}/cg_test_json_schema.out"
  then
    echo failed verification
    failed
  fi

  echo "checking for valid JSON formatting (test mode disabled)"
  cc -E -x c "${TEST_DIR}/cg_test_json_schema.sql" >"${OUT_DIR}/__temp"
  if  ! ${CQL} --cg "${OUT_DIR}/__temp.out" --in "${OUT_DIR}/__temp" --rt json_schema 2>"${OUT_DIR}/cg_test_json_schema.err"
  then
    cat cg_test_json_schema.err
    echo non-test JSON output failed
    failed
  fi

  echo "checking for well formed JSON using python"
  if ! common/json_check.py <"${OUT_DIR}/__temp.out" >/dev/null
  then
    echo json is badly formed -- see "${OUT_DIR}/__temp.out"
    failed
  fi

  echo "checking for CQL JSON grammar conformance"
  if ! out/json_test <"${OUT_DIR}/__temp.out" >"${OUT_DIR}/json_errors.txt"
  then
    echo json did not pass grammar check
    cat "${OUT_DIR}/json_errors.txt"
    failed
  fi

  echo validating json codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_json_schema.out
}

test_helpers_test() {
  echo '--------------------------------- STAGE 12 -- TEST HELPERS TEST'
  echo running test builders test
  cc -DCQL_TEST -E -x c "${TEST_DIR}/cg_test_test_helpers.sql" >"${OUT_DIR}/__temp"
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_test_helpers.out" --in "${OUT_DIR}/__temp" --rt test_helpers 2>"${OUT_DIR}/cg_test_test_helpers.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_test_helpers.err"
    failed
  fi

  echo validating test helpers output
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_test_helpers.sql" "${OUT_DIR}/cg_test_test_helpers.out"
  then
    echo failed verification
    failed
  fi

  echo validating test helpers codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_test_helpers.out

  echo running semantic analysis on test helpers output
  if ! sem_check --sem --print --in "${TEST_DIR}/cg_test_test_helpers.ref" >/dev/null 2>"${OUT_DIR}/cg_test_test_helpers.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/cg_test_test_helpers.err"
     failed
  fi
}

run_test() {
  echo '--------------------------------- STAGE 13 -- RUN CODE TEST'
  echo running codegen test with execution
  if ! cc -E -x c -w "${TEST_DIR}/run_test.sql" >"${OUT_DIR}/run_test_cpp.out"
  then
    echo preprocessing failed.
    failed
  elif ! ${CQL} --cg "${OUT_DIR}/run_test.h" "${OUT_DIR}/run_test.c" --in "${OUT_DIR}/run_test_cpp.out" --global_proc cql_startup --rt c --generate_copy
  then
    echo codegen failed.
    failed
  elif ! (echo "  compiling code"; do_make run_test )
  then
    echo build failed
    failed
  elif ! (echo "  executing tests"; "./${OUT_DIR}/a.out")
  then
    echo tests failed
    failed
  elif ! ${CQL} --compress --cg "${OUT_DIR}/run_test.h" "${OUT_DIR}/run_test.c" --in "${OUT_DIR}/run_test_cpp.out" --global_proc cql_startup --rt c --generate_copy
  then
    echo compressed codegen failed.
    failed
  elif ! (echo "  compiling code (compressed version)"; do_make run_test )
  then
    echo build failed
    failed
  elif ! (echo "  executing tests (compressed version)"; "./${OUT_DIR}/a.out")
  then
    echo tests failed
    failed
  elif ! (echo "  compiling compat code"; do_make run_test_compat )
  then
    echo compile compat failed
    failed
  elif ! (echo "  executing compat tests"; "./${OUT_DIR}/a.out")
  then
    echo compat tests failed
    failed
  fi
}

upgrade_test() {
  echo '--------------------------------- STAGE 14 -- SCHEMA UPGRADE TEST'
  if ! upgrade/upgrade_test.sh "${TEST_COVERAGE_ARGS}"
  then
    failed
  fi
}

query_plan_test() {
  echo '--------------------------------- STAGE 15 -- TEST QUERY PLAN'

  echo C preprocessing
  cc -DCQL_TEST -E -x c "${TEST_DIR}/cg_test_query_plan.sql" >"${OUT_DIR}/cg_test_query_plan2.sql"

  echo semantic analysis
  if ! ${CQL} --sem --print --dev --in "${OUT_DIR}/cg_test_query_plan2.sql" >"${OUT_DIR}/__temp" 2>"${OUT_DIR}/cg_test_query_plan.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/cg_test_query_plan.err"
     failed
  fi

  echo codegen query plan
  if ! ${CQL} --test --dev --cg "${OUT_DIR}/cg_test_query_plan.out" --in "${OUT_DIR}/cg_test_query_plan2.sql" --rt query_plan 2>"${OUT_DIR}/cg_test_query_plan.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_query_plan.err"
    failed
  fi

  echo semantic analysis
  if ! ${CQL} --sem --print --dev --test --in "${OUT_DIR}/cg_test_query_plan.out" >"${OUT_DIR}/__temp" 2>"${OUT_DIR}/cg_test_query_plan.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/cg_test_query_plan.err"
     failed
  fi

  echo validating test
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test_query_plan.sql" "${OUT_DIR}/cg_test_query_plan.out"
  then
    echo failed verification
    failed
  fi

  echo validating query plan codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_query_plan.out

  echo validate udf test
  if ! ${CQL} --test --dev --cg "${OUT_DIR}/udf.h" "${OUT_DIR}/udf.c" --in "${OUT_DIR}/cg_test_query_plan.out" --rt udf 2>"${OUT_DIR}/udf.err"
  then
    echo "ERROR:"
    cat $"{OUT_DIR}/udf.err"
    failed
  fi

  echo validating udf codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit udf.c
  on_diff_exit udf.h

  echo build query plan c code
  if ! ${CQL} --test --dev --cg "${OUT_DIR}/query_plan.h" "${OUT_DIR}/query_plan.c" --in "${OUT_DIR}/cg_test_query_plan.out" 2>"${OUT_DIR}/query_plan_print.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/query_plan_print.err"
    failed
  fi

  echo compile query plan code
  if ! do_make query_plan_test
  then
    echo build failed
    failed
  fi

  # linux build of sqlite doesn't have explain query plan
  echo run query plan in c
  if ! "./${OUT_DIR}/query_plan_test" >"${OUT_DIR}/cg_test_query_plan_view.out" 2>"${OUT_DIR}/cg_test_query_plan_view.err"
  then
    echo "${OUT_DIR}/query_plan_test returned a failure code"
    cat "${OUT_DIR}/cg_test_query_plan_view.out"
    cat "${OUT_DIR}/cg_test_query_plan_view.err"
    failed
  fi

  echo validate json format of query plan report
  if ! common/json_check.py <"${OUT_DIR}/cg_test_query_plan_view.out" >"${OUT_DIR}/cg_test_query_plan_js.out" 2>"${OUT_DIR}/cg_test_query_plan_js.err"
  then
    echo "${OUT_DIR}/cg_test_query_plan_view.out has invalid json format"
    cat "${OUT_DIR}/cg_test_query_plan_js.err"
    failed
  fi

  echo validating query plan view
  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_query_plan_js.out
}

# add other stages before this one

unit_tests() {
  if ! (${CQL} --run_unit_tests)
  then
    echo CQL unit tests failed
    failed
  fi
}

GENERATED_TAG=generated
AT_GENERATED_TAG="@$GENERATED_TAG"

signatures_test() {
  echo checking for signatures in reference files
  # shellcheck disable=SC2086
  if grep "$AT_GENERATED_TAG SignedSource" ${TEST_DIR}/*.ref
  then
    echo "signatures found in reference files, this is never valid."
    echo "change the test logic so that it validates the presence of the signature which then strips it."
    echo "it's likely that one of those validations is missing which caused ok.sh to put a signature into a .ref file."
    failed
  fi
}

make_clean_msg() {
  echo "To clean artifacts: make clean"
}

if ! building
then
  cat "${OUT_DIR}/build.out" "${OUT_DIR}/build.err"
  echo "build failed."
fi

# each of these will exit if anything goes wrong
basic_test
unit_tests
dot_test
semantic_test
code_gen_c_test
code_gen_java_test
code_gen_objc_test
assorted_errors_test
schema_migration_test
misc_cases
json_schema_test
test_helpers_test
run_test
upgrade_test
query_plan_test
signatures_test
extra_tests

make_clean_msg
echo '--------------------------------- DONE SUCCESS'
exit 0

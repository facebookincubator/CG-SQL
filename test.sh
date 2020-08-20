#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CQL_DIR=.
OUT_DIR="out"
TEST_DIR="test"
CQL="./${OUT_DIR}/cql"
cd "${CQL_DIR}" || exit 1
# shellcheck disable=SC1091
source test_helpers.sh || exit 1

GENERATED_TAG=generated
AT_GENERATED_TAG="@$GENERATED_TAG"

set_linux() {
  MAKE_LINUX_ARGS="LINUX=1"
  IS_LINUX=1
}

# This is a quick and dirty test for a devserver, if it matches you don't need to also add --linux
if [[ ${HOSTNAME} == *".facebook.com"* ]]; then
  set_linux
fi

while [ "$1" != "" ]
do
  if [ "$1" == "--linux" ]
  then
     set_linux
     shift 1
  elif [ "$1" == "--coverage" ]
  then
     MAKE_COVERAGE_ARGS="COVERAGE=1"
     TEST_COVERAGE_ARGS="--coverage"
     shift 1
  elif [ "$1" == "--use_amalgam" ]
  then
     CQL=${OUT_DIR}/cql_amalgam
     shift 1
     USE_AMALGAM=1
  else
     echo "Usage: test.sh [--linux] [--coverage] [--use_amalgam]"
     exit 1
  fi
done

MAKE_ARGS="${MAKE_COVERAGE_ARGS} ${MAKE_LINUX_ARGS}"

failed() {
  echo '--------------------------------- FAILED'
  make_clean_msg
  exit 1
}

do_make() {
  if [ "${MAKE_ARGS}" == " " ]
  then
    # we don't want to send empty strings " " to make, so avoid that
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
  if ! do_make all >${OUT_DIR}/build.out 2>${OUT_DIR}/build.err
  then
    echo build cql failed:
    cat ${OUT_DIR}/build.err
    failed
  fi

  if grep "^State.*conflicts:" ${OUT_DIR}/cql.y.output >${OUT_DIR}/build.err
  then
    echo "conflicts found in grammar, these must be fixed" >>${OUT_DIR}/build.err
    echo "look at the conflicting states in" ${OUT_DIR}/cql.y.output "to debug" >>${OUT_DIR}/build.err
    cat ${OUT_DIR}/build.err
    failed
  fi

  echo building cql amalgam
  if ! do_make amalgam >${OUT_DIR}/build.out 2>${OUT_DIR}/build.err
  then
    echo build cql amalgam failed:
    cat ${OUT_DIR}/build.err
    failed
  fi

  echo building cql tester
  if ! (cd tester && do_make all) 2>${OUT_DIR}/build.err
  then
    echo build cqltester failed:
    cat ${OUT_DIR}/build.err
    failed
  fi
}

create_unwritable_file() {
  file_name=$1
  rm -f "$file_name"
  echo x >"$file_name"
  chmod -rw "$file_name"
}

strip_signature() {
   grep -v "$AT_GENERATED_TAG SignedSource" $1 >"${OUT_DIR}/__nosig"
   cp "${OUT_DIR}/__nosig" $1
}

on_invalid_sig_exit() {
  file_name=$1
  if ! grep -q "$AT_GENERATED_TAG SignedSource" "$file_name"
  then
    echo "  ERROR: $file_name not signed"
    failed
  fi

  strip_signature "$file_name"
}

basic_test() {
  echo '--------------------------------- STAGE 2 -- ${CQL} <${TEST_DIR}/test.sql -- BASIC PARSING TEST'
  echo running ${TEST_DIR}/test.sql
  if ! ${CQL} --dev --in ${TEST_DIR}/test.sql >${OUT_DIR}/test.out
  then
   echo basic parsing test failed
   failed
  fi
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/test.ref ${OUT_DIR}/test.out
}

dot_test() {
  echo '--------------------------------- STAGE 3 -- .DOT OUTPUT TEST'
  echo running ${TEST_DIR}/dottest.sql
  if ! ${CQL} --dot --in ${TEST_DIR}/dottest.sql >${OUT_DIR}/dottest.out
  then
    echo DOT syntax test failed
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/dottest.ref ${OUT_DIR}/dottest.out
}

semantic_test() {
  echo '--------------------------------- STAGE 4 -- SEMANTIC ANALYSIS TEST'
  echo running semantic analysis test
  if ! sem_check --sem --print --dev --in ${TEST_DIR}/sem_test.sql >${OUT_DIR}/sem_test.out 2>${OUT_DIR}/sem_test_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/sem_test_err.out
     failed
  fi

  echo validating output trees
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/sem_test.sql ${OUT_DIR}/sem_test.out
  then
    echo failed verification
    failed
  fi

  echo running dev semantic analysis test
  if ! sem_check --sem --print --in ${TEST_DIR}/sem_test_dev.sql >${OUT_DIR}/sem_test_dev.out 2>${OUT_DIR}/sem_test_dev_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/sem_test_dev_err.out
     failed
  fi

  echo validating output trees
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/sem_test_dev.sql ${OUT_DIR}/sem_test_dev.out
  then
    echo failed verification
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/sem_test.ref ${OUT_DIR}/sem_test.out
  on_diff_exit ${TEST_DIR}/sem_test_err.ref ${OUT_DIR}/sem_test_err.out
  on_diff_exit ${TEST_DIR}/sem_test_dev.ref ${OUT_DIR}/sem_test_dev.out
  on_diff_exit ${TEST_DIR}/sem_test_dev_err.ref ${OUT_DIR}/sem_test_dev_err.out
}

code_gen_c_test() {
  echo '--------------------------------- STAGE 5 -- C CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_c.out.h ${OUT_DIR}/cg_test_c.out.c ${OUT_DIR}/cg_test_exports.out --in ${TEST_DIR}/cg_test.sql --global_proc cql_startup --generate_copy --generate_exports 2>${OUT_DIR}/cg_test_c_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_c_err.out
    failed
  fi

  on_invalid_sig_exit ${OUT_DIR}/cg_test_c.out.h
  on_invalid_sig_exit ${OUT_DIR}/cg_test_c.out.c

  echo validating codegen
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test.sql ${OUT_DIR}/cg_test_c.out.c
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test with type getters enabled
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_c_with_type_getters.out.h ${OUT_DIR}/cg_test_c_with_type_getters.out.c --in ${TEST_DIR}/cg_test_c_type_getters.sql --global_proc cql_startup --generate_copy --generate_type_getters  2>${OUT_DIR}/cg_test_c_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_c_err.out
    failed
  fi

  on_invalid_sig_exit ${OUT_DIR}/cg_test_c_with_type_getters.out.h
  on_invalid_sig_exit ${OUT_DIR}/cg_test_c_with_type_getters.out.c

  echo validating codegen
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test_c_type_getters.sql ${OUT_DIR}/cg_test_c_with_type_getters.out.h
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test with namespace enabled
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_c_with_namespace.out.h ${OUT_DIR}/cg_test_c_with_namespace.out.c ${OUT_DIR}/cg_test_imports_with_namespace.ref --in ${TEST_DIR}/cg_test.sql --global_proc cql_startup --generate_copy --c_include_namespace test_namespace --generate_exports 2>${OUT_DIR}/cg_test_c_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_c_err.out
    failed
  fi

  on_invalid_sig_exit ${OUT_DIR}/cg_test_c_with_namespace.out.h
  on_invalid_sig_exit ${OUT_DIR}/cg_test_c_with_namespace.out.c

  echo validating codegen
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test.sql ${OUT_DIR}/cg_test_c_with_namespace.out.c
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test for extension query fragment
  if ! ${CQL} --test --generate_type_getters --cg ${OUT_DIR}/cg_extension_fragment_test_c.out.h ${OUT_DIR}/cg_extension_fragment_test_c.out.c ${OUT_DIR}/cg_extension_fragment_test_imports.ref --in ${TEST_DIR}/cg_extension_fragment_test.sql --global_proc cql_startup --generate_copy --generate_exports 2>${OUT_DIR}/cg_test_c_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_c_err.out
    failed
  fi

  echo validating codegen
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_extension_fragment_test.sql ${OUT_DIR}/cg_extension_fragment_test_c.out.c
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo running codegen test for assembly query
  if ! ${CQL} --test --generate_type_getters --cg ${OUT_DIR}/cg_assembly_query_test_c.out.h ${OUT_DIR}/cg_assembly_query_test_c.out.c ${OUT_DIR}/cg_assembly_query_test_imports.ref --in ${TEST_DIR}/cg_assembly_query_test.sql --global_proc cql_startup --generate_copy --generate_exports 2>${OUT_DIR}/cg_test_c_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_c_err.out
    failed
  fi

  echo validating codegen
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_assembly_query_test.sql ${OUT_DIR}/cg_assembly_query_test_c.out.c
  then
    echo "ERROR: failed c query fragment verification"
    failed
  fi

  on_invalid_sig_exit ${OUT_DIR}/cg_extension_fragment_test_c.out.c
  on_invalid_sig_exit ${OUT_DIR}/cg_extension_fragment_test_c.out.h
  on_invalid_sig_exit ${OUT_DIR}/cg_assembly_query_test_c.out.c
  on_invalid_sig_exit ${OUT_DIR}/cg_assembly_query_test_c.out.h
  on_invalid_sig_exit ${OUT_DIR}/cg_test_exports.out

  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/cg_test_c.ref ${OUT_DIR}/cg_test_c.out.c
  on_diff_exit ${TEST_DIR}/cg_test_c_header.ref ${OUT_DIR}/cg_test_c.out.h
  on_diff_exit ${TEST_DIR}/cg_test_c_with_namespace.ref ${OUT_DIR}/cg_test_c_with_namespace.out.c
  on_diff_exit ${TEST_DIR}/cg_test_c_header_with_namespace.ref ${OUT_DIR}/cg_test_c_with_namespace.out.h
  on_diff_exit ${TEST_DIR}/cg_test_c_with_type_getters.ref ${OUT_DIR}/cg_test_c_with_type_getters.out.c
  on_diff_exit ${TEST_DIR}/cg_test_c_header_with_type_getters.ref ${OUT_DIR}/cg_test_c_with_type_getters.out.h
  on_diff_exit ${TEST_DIR}/cg_test_exports.ref ${OUT_DIR}/cg_test_exports.out
  on_diff_exit ${TEST_DIR}/cg_extension_fragment_test_c.ref ${OUT_DIR}/cg_extension_fragment_test_c.out.c
  on_diff_exit ${TEST_DIR}/cg_extension_fragment_test_c_header.ref ${OUT_DIR}/cg_extension_fragment_test_c.out.h
  on_diff_exit ${TEST_DIR}/cg_assembly_query_test_c.ref ${OUT_DIR}/cg_assembly_query_test_c.out.c
  on_diff_exit ${TEST_DIR}/cg_assembly_query_test_c_header.ref ${OUT_DIR}/cg_assembly_query_test_c.out.h
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_c_err.out

  echo "  compiling code"
  cp ${OUT_DIR}/cg_test_c.out.c ${OUT_DIR}/cg_test.c
  cp ${OUT_DIR}/cg_extension_fragment_test_c.out.c ${OUT_DIR}/cg_extension_fragment_test.c
  cp ${OUT_DIR}/cg_assembly_query_test_c.out.c ${OUT_DIR}/cg_assembly_query_test.c

  if ! do_make cg_test
  then
    echo CQL generated invalid C code
    failed
  fi

  echo "  compiling query fragment code"
  if ! do_make cg_extension_fragment_test
  then
    echo CQL generated invalid C code for extension query fragment
    failed
  fi

  if ! do_make cg_assembly_query_test
  then
    echo CQL generated invalid C code for assembly query
    failed
  fi
}

code_gen_java_test() {
  echo '--------------------------------- STAGE 8 -- JAVA CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_java.out --in ${TEST_DIR}/cg_test_no_result_set.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo validating empty codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_java_err.out

  echo running java with suppressed getters
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_java.out --in ${TEST_DIR}/cg_test_suppressed.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo validating empty codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_java_err.out

  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_java_not_nullable_proc.out --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_java_nullable_proc.out --in ${TEST_DIR}/cg_test_single_proc_nullable.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo running java codegen test for extension query fragment
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_extension_fragment_test_java.out --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt java --java_package_name com.facebook.cqlviewmodels --java_assembly_query_classname com.facebook.cqlviewmodels.cg_assembly_query_test_java 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo running java codegen test for assembly query fragment
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_assembly_query_test_java.out --in ${TEST_DIR}/cg_assembly_query_test.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo running java codegen test for out union
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_java_out_union.out --in ${TEST_DIR}/cg_test_out_union.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/cg_test_java_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_java_err.out
    failed
  fi

  echo validating codegen
  echo "  computing diffs (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/cg_test_java_not_nullable_proc.out
  on_invalid_sig_exit ${OUT_DIR}/cg_test_java_nullable_proc.out
  on_invalid_sig_exit ${OUT_DIR}/cg_extension_fragment_test_java.out
  on_invalid_sig_exit ${OUT_DIR}/cg_assembly_query_test_java.out
  on_invalid_sig_exit ${OUT_DIR}/cg_test_java_out_union.out

  on_diff_exit ${TEST_DIR}/cg_test_java_not_nullable_proc.ref ${OUT_DIR}/cg_test_java_not_nullable_proc.out
  on_diff_exit ${TEST_DIR}/cg_test_java_nullable_proc.ref ${OUT_DIR}/cg_test_java_nullable_proc.out
  on_diff_exit ${TEST_DIR}/cg_extension_fragment_test_java.ref ${OUT_DIR}/cg_extension_fragment_test_java.out
  on_diff_exit ${TEST_DIR}/cg_assembly_query_test_java.ref ${OUT_DIR}/cg_assembly_query_test_java.out
  on_diff_exit ${TEST_DIR}/cg_test_java_out_union.ref ${OUT_DIR}/cg_test_java_out_union.out
}

code_gen_objc_test() {
  echo '--------------------------------- STAGE 9 -- OBJ-C CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_test.sql --rt objc --objc_assembly_query_namespace . 2>${OUT_DIR}/cg_test_objc_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_objc_err.out
    failed
  fi

  echo validating codegen
  echo "  check that the objc_c_include_path argument was used"
  if ! grep "<Test/TestFile.h>" ${OUT_DIR}/cg_test_objc.out
  then
    echo error etc.
    failed
  fi

  echo running objc codegen test for extension query fragment
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_extension_fragment_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt objc --objc_assembly_query_namespace . 2>${OUT_DIR}/cg_test_objc_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_objc_err.out
    failed
  fi

  echo running objc codegen test for extension query fragment with core base
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_extension_fragment_with_core_base_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt objc --objc_assembly_query_namespace MessengerBridgeQueries 2>${OUT_DIR}/cg_test_objc_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_objc_err.out
    failed
  fi

  echo running objc codegen test for assembly query fragment
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_assembly_query_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_assembly_query_test.sql --rt objc 2>${OUT_DIR}/cg_test_objc_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_objc_err.out
    failed
  fi

  echo validating codegen
  echo "  computing diffs (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/cg_test_objc.out
  on_invalid_sig_exit ${OUT_DIR}/cg_extension_fragment_test_objc.out
  on_invalid_sig_exit ${OUT_DIR}/cg_extension_fragment_with_core_base_test_objc.out
  on_invalid_sig_exit ${OUT_DIR}/cg_assembly_query_test_objc.out

  on_diff_exit ${TEST_DIR}/cg_test_objc_header.ref ${OUT_DIR}/cg_test_objc.out
  on_diff_exit ${TEST_DIR}/cg_extension_fragment_test_objc_header.ref ${OUT_DIR}/cg_extension_fragment_test_objc.out
  on_diff_exit ${TEST_DIR}/cg_extension_fragment_with_core_base_test_objc_header.ref ${OUT_DIR}/cg_extension_fragment_with_core_base_test_objc.out
  on_diff_exit ${TEST_DIR}/cg_assembly_query_test_objc_header.ref ${OUT_DIR}/cg_assembly_query_test_objc.out
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_objc_err.out
}

assorted_errors_test() {
  echo '--------------------------------- STAGE 10 -- FAST FAIL CASES'
  echo running various failure cases that cause no output

# the output path doesn't exist, should cause an error

  if ${CQL} --in /xx/yy/zz 2>${OUT_DIR}/badpath_err.out
  then
    echo "reading from non-existant file should have failed, but didn't"
    failed
  fi

  on_diff_exit ${TEST_DIR}/badpath_err.ref ${OUT_DIR}/badpath_err.out

# the output file is not writeable, should cause an error

  if ${CQL} --cg /xx/yy/zz /xx/yy/zzz --in ${TEST_DIR}/cg_test.sql 2>${OUT_DIR}/unwriteable_err.out
  then
    echo "failed writing to unwriteable file should have failed, but didn't"
    failed
  fi

  on_diff_exit ${TEST_DIR}/unwriteable_err.ref ${OUT_DIR}/unwriteable_err.out

# wrong number of args specified in --cg (for objc)

  if ${CQL} --cg __temp __temp2 --in ${TEST_DIR}/cg_test.sql --rt objc 2>${OUT_DIR}/cg_1_2_err.out
  then
    echo "objc rt should require 1 files for the cg param but two were passed, should have failed"
    failed
  fi

# semantic errors should abort output (we'll not try to write)

  on_diff_exit ${TEST_DIR}/cg_1_2_err.ref ${OUT_DIR}/cg_1_2_err.out

  if ${CQL} --cg __temp /xx/yy/zz --in ${TEST_DIR}/semantic_error.sql 2>${OUT_DIR}/sem_abort_err.out
  then
    echo "simple semantic error to abort output -- failed"
    failed
  fi

  on_diff_exit ${TEST_DIR}/sem_abort_err.ref ${OUT_DIR}/sem_abort_err.out

# no result sets in the input for objc should result in empty output, not errors

  if ! ${CQL} --cg __temp --in ${TEST_DIR}/noresult.sql --objc_c_include_path dummy --rt objc 2>${OUT_DIR}/objc_no_results_err.out
  then
    echo "no result sets in output objc case, should not fail"
    failed
  fi

  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/objc_no_results_err.out

# bogus arg should report error

  if ${CQL} --garbonzo!! 2>${OUT_DIR}/invalid_arg_err.out
  then
    echo "invalid arg should report error -- failed"
    failed
  fi

  on_diff_exit ${TEST_DIR}/invalid_arg_err.ref ${OUT_DIR}/invalid_arg_err.out

# --cg did not have any following args, should force an error

  if ${CQL} --cg 2>${OUT_DIR}/cg_requires_file_err.out
  then
    echo "failed to require a file name with --cg"
    failed
  fi

  on_diff_exit ${TEST_DIR}/cg_requires_file_err.ref ${OUT_DIR}/cg_requires_file_err.out

# --generate_file_type did not specify a file type

  if ${CQL} --generate_file_type 2>${OUT_DIR}/generate_file_type_err.out
  then
    echo "failed to require a file type with --generate_file_type"
    failed
  fi

  on_diff_exit ${TEST_DIR}/generate_file_type_err.ref ${OUT_DIR}/generate_file_type_err.out

# --generate_file_type specified invalid file type (should cause an error)

  if ${CQL} --generate_file_type foo 2>${OUT_DIR}/generate_file_file_err.out
  then
    echo "failed to require a valid file type with --generate_file_type"
    failed
  fi

  on_diff_exit ${TEST_DIR}/generate_file_file_err.ref ${OUT_DIR}/generate_file_file_err.out

# --rt specified with no arg following it

  if ${CQL} --rt 2>${OUT_DIR}/rt_arg_missing_err.out
  then
    echo "failed to require a runtime with --rt"
    failed
  fi

  on_diff_exit ${TEST_DIR}/rt_arg_missing_err.ref ${OUT_DIR}/rt_arg_missing_err.out

# invalid result type specified with --rt, should force an error

  if ${CQL} --rt foo 2>${OUT_DIR}/rt_arg_bogus_err.out
  then
    echo "failed to require a valid result type with --rt"
    failed
  fi

  on_diff_exit ${TEST_DIR}/rt_arg_bogus_err.ref ${OUT_DIR}/rt_arg_bogus_err.out

# --cqlrt specified but no file name present, should force an error

  if ${CQL} --cqlrt 2>${OUT_DIR}/cqlrt_arg_missing_err.out
  then
    echo "failed to require a file arg  with --cqlrt"
    failed
  fi

  on_diff_exit ${TEST_DIR}/cqlrt_arg_missing_err.ref ${OUT_DIR}/cqlrt_arg_missing_err.out

# --global_proc has no proc name

  if ${CQL} --global_proc 2>${OUT_DIR}/global_proc_missing_err.out
  then
    echo "failed to require a procedure name with --global_proc"
    failed
  fi

  on_diff_exit ${TEST_DIR}/global_proc_missing_err.ref ${OUT_DIR}/global_proc_missing_err.out

# objc_c_include_path had no path

  if ${CQL} --objc_c_include_path 2>${OUT_DIR}/objc_include_missing_err.out
  then
    echo "failed to require an include path with --objc_c_include_path"
    failed
  fi

  on_diff_exit ${TEST_DIR}/objc_include_missing_err.ref ${OUT_DIR}/objc_include_missing_err.out

# --in arg missing

  if ${CQL} --in 2>${OUT_DIR}/in_arg_missing_err.out
  then
    echo "failed to require a file name with --in"
    failed
  fi

  on_diff_exit ${TEST_DIR}/in_arg_missing_err.ref ${OUT_DIR}/in_arg_missing_err.out

# no c_include_namespace arg

  if ${CQL} --c_include_namespace 2>${OUT_DIR}/c_include_namespace_missing_err.out
  then
    echo "failed to require a C namespace with --c_include_namespace"
    failed
  fi

  on_diff_exit ${TEST_DIR}/c_include_namespace_missing_err.ref ${OUT_DIR}/c_include_namespace_missing_err.out

# more than one proc in java file

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test.sql --cg ${OUT_DIR}/dummy.out --java_package_name dummy 2>${OUT_DIR}/java_rt_many_procs_err.out
  then
    echo "failed aborting a java rt codegen with more than one proc"
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_many_procs_err.ref ${OUT_DIR}/java_rt_many_procs_err.out

# package flag has no arg

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --cg ${OUT_DIR}/dummy.out --java_package_name 2>${OUT_DIR}/java_rt_no_package_err.out
  then
    echo "failed to require a java package with --java_package_name"
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_no_package_err.ref ${OUT_DIR}/java_rt_no_package_err.out

# java package flag missing

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --cg ${OUT_DIR}/dummy.out 2>${OUT_DIR}/java_rt_no_package_flag_err.out
  then
    echo "failed to require --java_package_name"
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_no_package_flag_err.ref ${OUT_DIR}/java_rt_no_package_flag_err.out

# java output filename ends with /

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --cg ${OUT_DIR}/dummy/ --java_package_name package 2>${OUT_DIR}/java_rt_filename_end_slash_err.out
  then
    echo "failed to require output file name that doesn't end with /"
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_filename_end_slash_err.ref ${OUT_DIR}/java_rt_filename_end_slash_err.out

# java output filename ends with .

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --cg ${OUT_DIR}/dummy. --java_package_name package 2>${OUT_DIR}/java_rt_filename_end_dot_err.out
  then
    echo "failed to require output file doesn't end with ."
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_filename_end_dot_err.ref ${OUT_DIR}/java_rt_filename_end_dot_err.out

# java output filename has no basename (e.g. foo/bar/.java)

  if ${CQL} --rt java --in ${TEST_DIR}/cg_test_single_proc_not_nullable.sql --cg ${OUT_DIR}/.java --java_package_name package 2>${OUT_DIR}/java_rt_filename_no_base_err.out
  then
    echo "failed to require output file with a base name"
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_rt_filename_no_base_err.ref ${OUT_DIR}/java_rt_filename_no_base_err.out
}

schema_migration_test() {
  echo '--------------------------------- STAGE 11 -- SCHEMA MIGRATION TESTS'
  echo running semantic analysis for migration test
  if ! sem_check --sem --print --in ${TEST_DIR}/sem_test_migrate.sql >${OUT_DIR}/sem_test_migrate.out 2>${OUT_DIR}/sem_test_migrate_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/sem_test_migrate_err.out
     failed
  fi

  echo validating output trees
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/sem_test_migrate.sql ${OUT_DIR}/sem_test_migrate.out
  then
    echo failed verification
    failed
  fi
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/sem_test_migrate.ref ${OUT_DIR}/sem_test_migrate.out
  on_diff_exit ${TEST_DIR}/sem_test_migrate_err.ref ${OUT_DIR}/sem_test_migrate_err.out

  echo '---------------------------------'
  echo running a schema migrate proc test
  if ! sem_check --sem --in ${TEST_DIR}/schema_version_error.sql --print >${OUT_DIR}/schema_version_error.out 2>${OUT_DIR}/schema_version_error.err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/schema_version_error.err.out
     failed
  fi;

  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/schema_version_error.sql ${OUT_DIR}/schema_version_error.out
  then
    echo failed verification
    failed
  fi

  echo '---------------------------------'
  echo running semantic analysis for previous schema error checks test
  if ! sem_check --sem --print --exclude_regions high_numbered_thing --in ${TEST_DIR}/sem_test_prev.sql >${OUT_DIR}/sem_test_prev.out 2>${OUT_DIR}/sem_test_prev_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/sem_test_prev_err.out
     failed
  fi;

  echo validating output trees
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/sem_test_prev.sql ${OUT_DIR}/sem_test_prev.out
  then
    echo failed verification
    failed
  fi

  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/sem_test_prev.ref ${OUT_DIR}/sem_test_prev.out
  on_diff_exit ${TEST_DIR}/sem_test_prev_err.ref ${OUT_DIR}/sem_test_prev_err.out

  echo '---------------------------------'
  echo running code gen for migration test

  if ! ${CQL} --cg ${OUT_DIR}/cg_test_schema_upgrade.out --in ${TEST_DIR}/cg_test_schema_upgrade.sql --global_proc test --rt schema_upgrade 2>${OUT_DIR}/cg_test_schema_upgrade_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_schema_upgrade_err.out
    failed
  fi

  echo validating output trees
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test_schema_upgrade.sql ${OUT_DIR}/cg_test_schema_upgrade.out
  then
    echo failed verification
    failed
  fi

  echo "  compiling the upgrade script with CQL"
  if ! ${CQL} --cg ${OUT_DIR}/cg_test_schema_upgrade.out.h ${OUT_DIR}/cg_test_schema_upgrade.out.c --in ${OUT_DIR}/cg_test_schema_upgrade.out
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
  on_invalid_sig_exit ${OUT_DIR}/cg_test_schema_upgrade.out

  on_diff_exit ${TEST_DIR}/cg_test_schema_upgrade.ref ${OUT_DIR}/cg_test_schema_upgrade.out
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_schema_upgrade_err.out

  echo '---------------------------------'
  echo running code gen to produce previous schema

  if ! ${CQL} --cg ${OUT_DIR}/cg_test_schema_prev.out --in ${TEST_DIR}/cg_test_schema_upgrade.sql --rt schema 2>${OUT_DIR}/cg_test_schema_prev_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_schema_prev_err.out
    failed
  fi


  echo combining geneated previous schema with itself to ensure it self validates

  cat ${OUT_DIR}/cg_test_schema_prev.out > ${OUT_DIR}/prev_loop.out
  echo "@previous_schema;" >> ${OUT_DIR}/prev_loop.out
  cat ${OUT_DIR}/cg_test_schema_prev.out >> ${OUT_DIR}/prev_loop.out

  if ! ${CQL} --cg ${OUT_DIR}/prev_twice.out --in ${OUT_DIR}/prev_loop.out --rt schema 2>${OUT_DIR}/cg_test_schema_prev_twice_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_schema_prev_twice_err.out
    failed
  fi

  echo comparing the generated previous schema from that combination and it should be identical to the original

  if ! ${CQL} --cg ${OUT_DIR}/prev_thrice.out --in ${OUT_DIR}/prev_twice.out --rt schema 2>${OUT_DIR}/cg_test_schema_prev_thrice_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_schema_prev_thrice_err.out
    failed
  fi

  echo "  computing diffs after several applications (empty if none)"
  on_diff_exit ${OUT_DIR}/cg_test_schema_prev.out ${OUT_DIR}/prev_twice.out
  on_diff_exit ${OUT_DIR}/prev_twice.out ${OUT_DIR}/prev_thrice.out

  echo "  computing previous schema diffs from reference (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/cg_test_schema_prev.out
  on_diff_exit ${TEST_DIR}/cg_test_schema_prev.ref ${OUT_DIR}/cg_test_schema_prev.out
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_schema_prev_err.out

  echo "  running schema migration with include/exclude args"
  if ! ${CQL} --cg ${OUT_DIR}/cg_test_schema_partial_upgrade.out --in ${TEST_DIR}/cg_test_schema_upgrade.sql --global_proc test --rt schema_upgrade --include_regions extra --exclude_regions shared 2>${OUT_DIR}/cg_test_schema_partial_upgrade_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_schema_partial_upgrade_err.out
    failed
  fi

  echo "  compiling the upgrade script with CQL"
  if ! ${CQL} --cg ${OUT_DIR}/cg_test_schema_partial_upgrade.out.h ${OUT_DIR}/cg_test_schema_partial_upgrade.out.c --in ${OUT_DIR}/cg_test_schema_partial_upgrade.out
  then
    echo CQL compilation failed
    failed;
  fi

  echo "  computing diffs (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/cg_test_schema_partial_upgrade.out
  on_diff_exit ${TEST_DIR}/cg_test_schema_partial_upgrade.ref ${OUT_DIR}/cg_test_schema_partial_upgrade.out
  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/cg_test_schema_partial_upgrade_err.out
}

misc_cases() {
  echo '--------------------------------- STAGE 12 -- MISC CASES'
  echo running simple error test
  if ${CQL} --in ${TEST_DIR}/error.sql >${OUT_DIR}/error.out 2>${OUT_DIR}/simple_error_err.out
  then
    echo simple error test failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/simple_error_err.ref ${OUT_DIR}/simple_error_err.out

  echo running previous schema and codegen incompatible test
  if ${CQL} --cg ${OUT_DIR}/__temp.h ${OUT_DIR}/__temp.c --in ${TEST_DIR}/cg_test_prev_invalid.sql 2>${OUT_DIR}/prev_and_codegen_incompat_err.out
  then
    echo previous schema and codegen are supposed to be incompatible
    failed
  fi

  on_diff_exit ${TEST_DIR}/prev_and_codegen_incompat_err.ref ${OUT_DIR}/prev_and_codegen_incompat_err.out

  echo running big quote test
  if ! ${CQL} --cg ${OUT_DIR}/__temp.h ${OUT_DIR}/__temp.c --in ${TEST_DIR}/bigquote.sql --global_proc x >/dev/null 2>${OUT_DIR}/bigquote_err.out
  then
    echo big quote test failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/bigquote_err.out

  echo running alternate cqlrt.h test
  if ! ${CQL} --cg ${OUT_DIR}/__temp.h ${OUT_DIR}/__temp.c --in ${TEST_DIR}/cg_test.sql --global_proc x --cqlrt alternate_cqlrt.h 2>${OUT_DIR}/alt_cqlrt_err.out
  then
    echo alternate cqlrt test failed
    failed
  fi

  if ! grep alternate_cqlrt.h ${OUT_DIR}/__temp.h >/dev/null
  then
    echo alternate cqlrt did not appear in the output header
    failed
  fi

  on_diff_exit ${TEST_DIR}/no_errors_allowed.ref ${OUT_DIR}/alt_cqlrt_err.out

  echo running too few -cg arguments with --generate_exports test
  if ${CQL} --cg ${OUT_DIR}/__temp.c ${OUT_DIR}/__temp.h --in ${TEST_DIR}/cg_test.sql --global_proc x --generate_exports 2>${OUT_DIR}/gen_exports_args_err.out
  then
    echo too few --cg args test failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/gen_exports_args_err.ref ${OUT_DIR}/gen_exports_args_err.out

  echo running invalid include regions test
  if ${CQL} --cg ${OUT_DIR}/cg_test_schema_partial_upgrade.out --in ${TEST_DIR}/cg_test_schema_upgrade.sql --global_proc test --rt schema_upgrade --include_regions bogus --exclude_regions shared 2>${OUT_DIR}/inc_invalid_regions_err.out
  then
    echo invalid include region test failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/inc_invalid_regions_err.ref ${OUT_DIR}/inc_invalid_regions_err.out

  echo running invalid exclude regions test
  if ${CQL} --cg ${OUT_DIR}/cg_test_schema_partial_upgrade.out --in ${TEST_DIR}/cg_test_schema_upgrade.sql --global_proc test --rt schema_upgrade --include_regions extra --exclude_regions bogus 2>${OUT_DIR}/excl_invalid_regions_err.out
  then
    echo invalid exclude region test failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/excl_invalid_regions_err.ref ${OUT_DIR}/excl_invalid_regions_err.out

  echo running global proc is needed but not present test
  if ${CQL} --cg ${OUT_DIR}/__temp.c ${OUT_DIR}/__temp.h --in ${TEST_DIR}/bigquote.sql 2>${OUT_DIR}/global_proc_needed_err.out
  then
    echo global proc needed but absent failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/global_proc_needed_err.ref ${OUT_DIR}/global_proc_needed_err.out

  echo running assembly query namespace not provided for extension fragment test
  if ${CQL} --test --cg ${OUT_DIR}/cg_extension_fragment_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt objc 2>${OUT_DIR}/asm_query_ns_needed_err.out
  then
    echo assembly query namespace is required for extension fragment
    failed
  fi

  on_diff_exit ${TEST_DIR}/asm_query_ns_needed_err.ref ${OUT_DIR}/asm_query_ns_needed_err.out

  echo running assembly query namespace provided empty for extension fragment test
  if ${CQL} --test --cg ${OUT_DIR}/cg_extension_fragment_test_objc.out --objc_c_include_path Test/TestFile.h --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt objc --objc_assembly_query_namespace 2>${OUT_DIR}/asm_query_ns_nonempty_err.out
  then
    echo assembly query namespace is required to be non-empty for extension fragment
    failed
  fi

  on_diff_exit ${TEST_DIR}/asm_query_ns_nonempty_err.ref ${OUT_DIR}/asm_query_ns_nonempty_err.out

  echo running test where output file cannot be written
  create_unwritable_file ${OUT_DIR}/unwritable.h.out
  create_unwritable_file ${OUT_DIR}/unwritable.c.out
  if ${CQL} --cg ${OUT_DIR}/unwritable.h.out ${OUT_DIR}/unwritable.c.out --in ${TEST_DIR}/cg_test.sql --rt c --global_proc cql_startup 2>${OUT_DIR}/write_fail_err.out
  then
    echo writing should have failed
    failed
  fi

  on_diff_exit ${TEST_DIR}/write_fail_err.ref ${OUT_DIR}/write_fail_err.out

  echo 'running missing --java_assembly_query_classname flag for extension fragment codegen test'
  if ${CQL} --test --cg ${OUT_DIR}/__temp.out --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt java --java_package_name com.facebook.cqlviewmodels 2>${OUT_DIR}/java_classname_missing_err.out
  then
    echo '--java_assembly_query_classname was missing, this should have caused an error'
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_classname_missing_err.ref ${OUT_DIR}/java_classname_missing_err.out

  echo 'running missing --java_assembly_query_classname args for extension fragment codegen test'
  if ${CQL} --test --cg ${OUT_DIR}/__temp.out --in ${TEST_DIR}/cg_extension_fragment_test.sql --rt java --java_package_name com.facebook.cqlviewmodels --java_assembly_query_classname 2>${OUT_DIR}/java_classname_noargs_err.out
  then
    echo '--java_assembly_query_classname had no arguments, this should have caused an error'
    failed
  fi

  on_diff_exit ${TEST_DIR}/java_classname_noargs_err.ref ${OUT_DIR}/java_classname_noargs_err.out
}

json_schema_test() {
  echo '--------------------------------- STAGE 13 -- JSON SCHEMA TEST'
  echo running json schema test
  cc -DCQL_TEST -E -x c ${TEST_DIR}/cg_test_json_schema.sql >${OUT_DIR}/__temp
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_json_schema.out --in ${OUT_DIR}/__temp --rt json_schema 2>${OUT_DIR}/cg_test_json_schema_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_json_schema_err.out
    failed
  fi

  echo validating json output
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test_json_schema.sql ${OUT_DIR}/cg_test_json_schema.out
  then
    echo failed verification
    failed
  fi

  echo "checking for valid JSON formatting (test mode disabled)"
  cc -E -x c ${TEST_DIR}/cg_test_json_schema.sql >${OUT_DIR}/__temp
  if  ! ${CQL} --cg ${OUT_DIR}/__temp.out --in ${OUT_DIR}/__temp --rt json_schema 2>${OUT_DIR}/cg_test_json_schema_err.out
  then
    cat cg_test_json_schema_err.out
    echo non-test JSON output failed
    failed
  fi

  if ! ./json_check.py <${OUT_DIR}/__temp.out >/dev/null
  then
    echo json is badly formed -- see ${OUT_DIR}/__temp.out
    failed
  fi

  echo validating json codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/cg_test_json_schema.ref ${OUT_DIR}/cg_test_json_schema.out
}

test_helpers_test() {
  echo '--------------------------------- STAGE 14 -- TEST HELPERS TEST'
  echo running test builders test
  cc -DCQL_TEST -E -x c ${TEST_DIR}/cg_test_test_helpers.sql >${OUT_DIR}/__temp
  if ! ${CQL} --test --cg ${OUT_DIR}/cg_test_test_helpers.out --in ${OUT_DIR}/__temp --rt test_helpers 2>${OUT_DIR}/cg_test_test_helpers_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_test_helpers_err.out
    failed
  fi

  echo validating test helpers output
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test_test_helpers.sql ${OUT_DIR}/cg_test_test_helpers.out
  then
    echo failed verification
    failed
  fi

  on_invalid_sig_exit ${OUT_DIR}/cg_test_test_helpers.out

  echo validating test helpers codegen
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/cg_test_test_helpers.ref ${OUT_DIR}/cg_test_test_helpers.out

  echo running semantic analysis on test helpers output
  if ! sem_check --sem --print --in ${TEST_DIR}/cg_test_test_helpers.ref >/dev/null 2>${OUT_DIR}/cg_test_test_helpers_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/cg_test_test_helpers_err.out
     failed
  fi
}

run_test() {
  echo '--------------------------------- STAGE 15 -- RUN CODE TEST'
  echo running codegen test with execution
  if ! cc -E -x c -w ${TEST_DIR}/run_test.sql >${OUT_DIR}/run_test_cpp.out
  then
    echo preprocessing failed.
    failed
  elif ! ${CQL} --cg ${OUT_DIR}/run_test.h ${OUT_DIR}/run_test.c --in ${OUT_DIR}/run_test_cpp.out --global_proc cql_startup --rt c --generate_copy
  then
    echo codegen failed.
    failed
  elif ! (echo "  compiling code"; do_make run_test )
  then
    echo build failed
    failed
  elif ! (echo "  executing tests"; ./${OUT_DIR}/a.out)
  then
    echo tests failed
    failed
  elif ! ${CQL} --compress --cg ${OUT_DIR}/run_test.h ${OUT_DIR}/run_test.c --in ${OUT_DIR}/run_test_cpp.out --global_proc cql_startup --rt c --generate_copy
  then
    echo compressed codegen failed.
    failed
  elif ! (echo "  compiling code (compressed version)"; do_make run_test )
  then
    echo build failed
    failed
  elif ! (echo "  executing tests (compressed version)"; ./${OUT_DIR}/a.out)
  then
    echo tests failed
    failed
  elif ! (echo "  compiling compat code"; do_make run_test_compat )
  then
    echo compile compat failed
    failed
  elif ! (echo "  executing compat tests"; ./${OUT_DIR}/a.out)
  then
    echo compat tests failed
    failed
  fi
}

upgrade_test() {
  echo '--------------------------------- STAGE 18 -- SCHEMA UPGRADE TEST'
  if ! upgrade/upgrade_test.sh ${TEST_COVERAGE_ARGS}
  then
    failed
  fi
}

query_plan_test() {
  echo '--------------------------------- STAGE 20 -- TEST QUERY PLAN'

  echo C preprocessing
  cc -DCQL_TEST -E -x c ${TEST_DIR}/cg_test_query_plan.sql >${OUT_DIR}/cg_test_query_plan2.sql

  echo semantic analysis
  if ! ${CQL} --sem --print --dev --in ${OUT_DIR}/cg_test_query_plan2.sql >${OUT_DIR}/__temp 2>${OUT_DIR}/cg_test_query_plan_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/cg_test_query_plan_err.out
     failed
  fi

  echo codegen query plan
  if ! ${CQL} --test --dev --cg ${OUT_DIR}/cg_test_query_plan.out --in ${OUT_DIR}/cg_test_query_plan2.sql --rt query_plan 2>${OUT_DIR}/cg_test_query_plan_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/cg_test_query_plan_err.out
    failed
  fi

  echo semantic analysis
  if ! ${CQL} --sem --print --dev --test --in ${OUT_DIR}/cg_test_query_plan.out >${OUT_DIR}/__temp 2>${OUT_DIR}/cg_test_query_plan_err.out
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat ${OUT_DIR}/cg_test_query_plan_err.out
     failed
  fi

  echo validating test
  if ! ${OUT_DIR}/cql-verify ${TEST_DIR}/cg_test_query_plan.sql ${OUT_DIR}/cg_test_query_plan.out
  then
    echo failed verification
    failed
  fi

  echo validating query plan codegen
  echo "  computing diffs (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/cg_test_query_plan.out
  on_diff_exit ${TEST_DIR}/cg_test_query_plan.ref ${OUT_DIR}/cg_test_query_plan.out

  echo validate udf test
  if ! ${CQL} --test --dev --cg ${OUT_DIR}/udf.h ${OUT_DIR}/udf.c --in ${OUT_DIR}/cg_test_query_plan.out --rt udf 2>${OUT_DIR}/udf_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/udf_err.out
    failed
  fi

  echo validating udf codegen
  echo "  computing diffs (empty if none)"
  on_invalid_sig_exit ${OUT_DIR}/udf.h
  on_invalid_sig_exit ${OUT_DIR}/udf.c
  on_diff_exit ${TEST_DIR}/cg_test_udf_c.ref ${OUT_DIR}/udf.c
  on_diff_exit ${TEST_DIR}/cg_test_udf_header.ref ${OUT_DIR}/udf.h

  echo build query plan c code
  if ! ${CQL} --test --dev --cg ${OUT_DIR}/query_plan.h ${OUT_DIR}/query_plan.c --in ${OUT_DIR}/cg_test_query_plan.out 2>${OUT_DIR}/query_plan_print_err.out
  then
    echo "ERROR:"
    cat ${OUT_DIR}/query_plan_print_err.out
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
  if ! ./${OUT_DIR}/query_plan_test >${OUT_DIR}/cg_test_query_plan_view.out 2>${OUT_DIR}/cg_test_query_plan_view_err.out
  then
    echo ${OUT_DIR}/query_plan_test returned a failure code
    cat ${OUT_DIR}/cg_test_query_plan_view.out
    cat ${OUT_DIR}/cg_test_query_plan_view_err.out
    failed
  fi

  echo validate json format of query plan report
  if ! ./json_check.py <${OUT_DIR}/cg_test_query_plan_view.out  >${OUT_DIR}/cg_test_query_plan_js.out 2>${OUT_DIR}/cg_test_query_plan_js_err.out
  then
    echo ${OUT_DIR}/cg_test_query_plan_view.out has invalid json format
    cat ${OUT_DIR}/cg_test_query_plan_js_err.out
    failed
  fi

  echo validating query plan view
  echo "  computing diffs (empty if none)"
  on_diff_exit ${TEST_DIR}/cg_test_query_plan_view.ref ${OUT_DIR}/cg_test_query_plan_js.out
}

signatures_test() {
  echo checking for signatures in reference files
  if grep "$AT_GENERATED_TAG SignedSource" ${TEST_DIR}/*.ref
  then
    echo "signatures found in reference files, this is never valid."
    echo "change the test logic so that it validates the presence of the signature which then strips it."
    echo "it's likely that one of those validations is missing which caused ok.sh to put a signature into a .ref file."
    failed
  fi
}

unit_tests() {
  if ! (${CQL} --run_unit_tests)
  then
    echo CQL unit tests failed
    failed
  fi
}

make_clean_msg() {
  echo "To clean artifacts: make clean"
}

if ! building
then
  cat ${OUT_DIR}/build.out ${OUT_DIR}/build.err
  echo "build failed."
fi

# each of these will exit if anything goes wrong
basic_test
schema_test
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

make_clean_msg
echo '--------------------------------- DONE SUCCESS'
exit 0

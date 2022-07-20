#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

FBSOURCE_DIR=$(realpath "$(hg root)")
CQL_DIR="${FBSOURCE_DIR}/xplat/msys/tools/cql"
OUT_DIR="out"
# We're using CQL binary from BUCK like that users don't have to deal with
# setup requirements e.g: CQL compile with bison version >= 3.x
CQL=$(buck build //xplat/msys/tools/cql:cql_impl --show-full-output | sed 's/[^ ]* //')
cd "${CQL_DIR}" || exit 1

#echo C preprocessing
cc -DCQL_TEST -E -x c "$1" >"${OUT_DIR}/cg_test_query_plan2.sql"

# echo semantic analysis
if ! ${CQL} --sem --ast --dev --in "${OUT_DIR}/cg_test_query_plan2.sql" >"${OUT_DIR}/__temp" 2>"${OUT_DIR}/cg_test_query_plan.err"
then
    echo "CQL semantic analysis returned error"
    cat "${OUT_DIR}/cg_test_query_plan.err"
    failed
fi

# echo codegen query plan
if ! ${CQL} --test --dev --cg "${OUT_DIR}/cg_test_query_plan.out" --in "${OUT_DIR}/cg_test_query_plan2.sql" --rt query_plan 2>"${OUT_DIR}/cg_test_query_plan.err"
then
    echo "CQL codegen query plan error"
    cat "${OUT_DIR}/cg_test_query_plan.err"
    failed
fi

# echo semantic analysis
if ! ${CQL} --sem --ast --dev --test --in "${OUT_DIR}/cg_test_query_plan.out" >"${OUT_DIR}/__temp" 2>"${OUT_DIR}/cg_test_query_plan.err"
then
    echo "CQL query plan semantic analysis return error"
    cat "${OUT_DIR}/cg_test_query_plan.err"
    failed
fi

# echo validate udf test
if ! ${CQL} --test --dev --cg "${OUT_DIR}/udf.h" "${OUT_DIR}/udf.c" --in "${OUT_DIR}/cg_test_query_plan.out" --rt udf 2>"${OUT_DIR}/udf.err"
then
    echo "CQL codegen udf return error"
    cat "${OUT_DIR}/udf.err"
    failed
fi

# echo build query plan c code
if ! ${CQL} --test --dev --cg "${OUT_DIR}/query_plan.h" "${OUT_DIR}/query_plan.c" --in "${OUT_DIR}/cg_test_query_plan.out" 2>"${OUT_DIR}/query_plan_print.err"
then
    echo "CQL codegen query plan return error"
    cat "${OUT_DIR}/query_plan_print.err"
    failed
fi

# echo compile query plan code
if ! make query_plan_test >/dev/null 2>"${OUT_DIR}/query_plan_print.err"
then
    echo "CQL query plan build fialed"
    cat "${OUT_DIR}/query_plan_print.err"
    failed
fi

# echo run query plan in c
"./${OUT_DIR}/query_plan_test"

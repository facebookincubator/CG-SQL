#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

DIR="$( dirname -- "$0"; )"

cp "$1" "${DIR}/out/qp_in"
cd "${DIR}" || exit

if ! make > "out/make.out"
then
   echo "CQL build failed"
   cat "out/make.out"
   failed
fi

CQL="out/cql"

#echo C preprocessing
cc -DCQL_TEST -E -x c "out/qp_in" >"out/cg_test_query_plan2.sql"

# echo semantic analysis
if ! ${CQL} --sem --ast --dev --in "out/cg_test_query_plan2.sql" >"out/__temp" 2>"out/cg_test_query_plan.err"
then
    echo "CQL semantic analysis returned error"
    cat "out/cg_test_query_plan.err"
    failed
fi

# echo codegen query plan
if ! ${CQL} --test --dev --cg "out/cg_test_query_plan.out" --in "out/cg_test_query_plan2.sql" --rt query_plan 2>"out/cg_test_query_plan.err"
then
    echo "CQL codegen query plan error"
    cat "out/cg_test_query_plan.err"
    failed
fi

# echo semantic analysis of generated code (pre check)
if ! ${CQL} --sem --ast --dev --test --in "out/cg_test_query_plan.out" >"out/__temp" 2>"out/cg_test_query_plan.err"
then
    echo "CQL query plan semantic analysis returned error"
    cat "out/cg_test_query_plan.err"
    failed
fi

# build any udfs needed
if ! ${CQL} --test --dev --cg "out/udf.h" "out/udf.c" --in "out/cg_test_query_plan.out" --rt udf 2>"out/udf.err"
then
    echo "CQL codegen udf return error"
    cat "out/udf.err"
    failed
fi

# build query plan c code
if ! ${CQL} --test --dev --cg "out/query_plan.h" "out/query_plan.c" --in "out/cg_test_query_plan.out" 2>"out/query_plan_print.err"
then
    echo "CQL codegen query plan return error"
    cat "out/query_plan_print.err"
    failed
fi

# compile query plan code with Makefile
if ! make query_plan_test >"out/make.out" 2>"out/make.err"
then
    echo "CQL query plan build failed"
    echo "stdout"
    cat "out/make.out"
    echo "stderr"
    cat "out/make.err"
    failed
fi

# echo run query plan in c
"./out/query_plan_test"
echo
echo "done"

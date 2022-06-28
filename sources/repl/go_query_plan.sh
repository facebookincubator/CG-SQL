#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

CQL_FILE=go.sql
CQL_ROOT_DIR=..
CQL=$CQL_ROOT_DIR/out/cql

if [ ! -f "${CQL}" ]; then
    echo "${CQL} not found"
    echo "cd .."
    echo "make clean"
    echo "make"
    exit 1
fi

./clean.sh
./clean_query_plan.sh

# Generate Query Plan Script
cc -E -x c $CQL_FILE >go.sql.pre
$CQL --in go.sql.pre --rt query_plan --cg go-qp.sql

# Generate UDF stubs
$CQL --in $CQL_FILE --rt udf --cg go-qp-udf.h go-qp-udf.c

# Compile and link CQL artifacts, with a main C file query_plan_test.c
$CQL --in go-qp.sql --cg go-qp.h go-qp.c --dev
cc -I$CQL_ROOT_DIR -I. -c $CQL_ROOT_DIR/query_plan_test.c go-qp.c go-qp-udf.c
cc -I$CQL_ROOT_DIR -I. -O -o go_query_plan go-qp.o go-qp-udf.o query_plan_test.o $CQL_ROOT_DIR/cqlrt.c -lsqlite3

# Run and generate query plans
./go_query_plan

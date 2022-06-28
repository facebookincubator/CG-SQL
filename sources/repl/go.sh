#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

set -e

CQL=../out/cql
if [ ! -f "${CQL}" ]; then
    echo "${CQL} not found"
    echo "cd .."
    echo "make clean"
    echo "make"
    exit 1
fi

echo "${CQL} ready"

./clean.sh
cc -E -x c go.sql >go.sql.pre
$CQL --in go.sql.pre --cg go.h go.c
cc -g -I.. -I. -o go go.c ../cqlrt.c main.c -lsqlite3
./go

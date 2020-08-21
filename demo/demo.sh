#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

O="../out"
CQL="${O}/cql"

${CQL} --in demo.sql --cg ${O}/demo.h ${O}/demo.c --generate_copy
cc -g -I.. -I${O} -o ${O}/demo ${O}/demo.c ../cqlrt.c demo_client.c -lsqlite3
${O}/demo
echo "SUCCESS"

#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

O="../out"
CQL="${O}/cql"

(cd ..; make demo-binary)
${O}/demo
$CQL --in demo.sql --rt json_schema --cg demo.json
echo "SUCCESS"

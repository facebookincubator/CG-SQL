#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

set -e

O="../out"

(cd ..; make demo-binary)
${O}/demo
echo "SUCCESS"

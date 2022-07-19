#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

make
rm -f out/x.l
out/cql --in lua_demo/run_test_prep.sql --cg out/x.l --rt lua --global_proc go
echo "go(sqlite3.open_memory())" >>out/x.l
lua out/x.l

#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

make
rm -f out/x.l out/run_test.lua
cc -DLUA_RUN_TEST -E -x c test/run_test.sql >out/lua_run_test.sql 2>out/lua_preprocess.errs
out/cql --in out/lua_run_test.sql --cg out/run_test_core.lua --rt lua --global_proc go

cat lua_demo/test_helpers.lua out/run_test_core.lua >out/run_test.lua
echo "go(sqlite3.open_memory())" >>out/run_test.lua

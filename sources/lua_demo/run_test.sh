#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

lua_demo/prepare_run_test.sh
lua out/run_test.lua

echo "schema upgrade test"
lua out/lua_schema_upgrade4.lua >out/lua_upgrade.txt
diff lua_demo/lua_upgrade.ref out/lua_upgrade.txt
echo no diffs means success

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


prep_upgrader() {
  V="$1"

  if [ "$V" -eq 4 ]; then
    ex="--schema_exclusive"
  else
    ex=""
  fi

  cc -DLUA_RUN_TEST -E -x c "upgrade/SchemaPersistentV$V.sql" >"out/lua_upgrade$V.sql" 2>out/lua_preprocess.errs
  # shellcheck disable=SC2086
  out/cql --in "out/lua_upgrade$V.sql" --cg "out/lua_schema_upgrade$V.sql" --rt schema_upgrade --global_proc lua_upgrade  ${ex}
  cat <lua_demo/upgrade_harness.cql >>"out/lua_schema_upgrade$V.sql"
  out/cql --in "out/lua_schema_upgrade$V.sql" --cg "out/lua_schema_upgrade$V.lua" --rt lua
}

for i in {0..4}
do
  prep_upgrader "$i"
done

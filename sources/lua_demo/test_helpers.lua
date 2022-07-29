--[[
Copyright (c) Meta Platforms, Inc. and affiliates.

This source code is licensed under the MIT license found in the
LICENSE file in the root directory of this source tree.
--]]

-- TESTING METHODS --

function cql_init_extensions(db)
  db:create_function(
    "rscount",
    1,
    function(ctx,rs)
      local ud = context:user_data()
      rs = ud[rs]
      print(#rs)
      ctx:result_number(#rs)
    end,
    cql_user_data
  )
   db:create_function(
    "rscol",
    3,
    function(ctx,rs, row, col)
      print(#rs)
      ctx:result_number(rs[row][col])
    end
  )
  return sqlite3.OK
end;

function get_outstanding_refs()
  return 0
end

function string_from_blob(str)
  return str
end

function blob_from_string(blob)
  return blob
end

function run_test_math(int1)
   return int1 * 7, int1 * 5
end

function string_create()
  return "Hello, world."
end

function set_create()
  return {}
end

function set_contains(s, k)
  return s[k] ~= nil
end

function set_add(s, k)
  if s[k] ~= nil then
    return false
  end

  s[k] = 1
  return true
end

function cql_invariant(x)
 if x == false or x == 0 then
    print("invariant failed")
    force_error_exit()
 end
end

function some_integers_fetch(a,b,c)
  return some_integers_fetch_results(a,b,c)
end

function exit(code)
  print("exit code", code)
end

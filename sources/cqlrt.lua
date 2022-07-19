--[[
Copyright (c) Meta Platforms, Inc. and affiliates.

This source code is licensed under the MIT license found in the
LICENSE file in the root directory of this source tree.
--]]

-- the built in version of sqlite is compiled without this enable_load_extenion (via -DOMIT...)
-- so we will get this error
-- dlopen(/usr/local/lib/lua/5.4/lsqlite3.so, 0x0006): symbol not found in flat namespace '_sqlite3_enable_load_extension'
-- use the brew version instead

package.loadlib("/usr/local/opt/sqlite/lib/libsqlite3.dylib","*")
sqlite3 = require('lsqlite3')


-- we know about these codes, everything else is some error
CQL_OK = sqlite3.OK
CQL_ERROR = sqlite3.ERROR
CQL_DONE = sqlite3.DONE
CQL_ROW = sqlite3.ROW

function cql_is(x,y)
   if x == nil and y == nil then
     return true;
   end

   if x == y then
     return true
   end

   -- normalize bools to 0/1 for comparison
   if x == true then x = 1 end;
   if x == false then x = 0 end;
   if y == true then y = 1 end;
   if y == false then y = 0 end;

   return x == y
end

function cql_is_not(x,y)
  return not cql_is(x,y)
end

function cql_eq(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x == y
  end
end

function cql_blob_eq(x,y)
  -- identity comparison for now
  return cql_eq(x,y)
end

function cql_blob_is_eq(x,y)
  -- identity comparison for now
  return x == y
end

function cql_ne(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x ~= y
  end
end

function cql_blob_ne(x,y)
  -- identity comparison for now
  return cql_ne(x,y)
end

function cql_blob_is_ne(x,y)
  -- identity comparison for now
  return x ~= y
end

function cql_ge(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x >= y
  end
end

function cql_gt(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x > y
  end
end

function cql_le(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x <= y
  end
end

function cql_lt(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x < y
  end
end

function cql_logical_and(x,y)
  if cql_is_false(x) or cql_is_false(y) then
    return false;
  elseif x == nil or y == nil then
    return nil
  else
    return true
  end
end

function cql_logical_or(x,y)
  if cql_is_true(x) or cql_is_true(y) then
    return true
  elseif x == nil or y == nil then
    return nil
  else
    return false
  end
end

function cql_shortcircuit_and(x,y)
  if cql_is_false(x) then
    return false
  else
    return cql_logical_and(x, y())
  end
end

function cql_shortcircuit_or(x,y)
  if cql_is_true(x) then
    return true
  else
    return cql_logical_or(x, y())
  end
end

function cql_add(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x + y;
  end
end

function cql_sub(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x - y;
  end
end

function cql_mul(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x * y;
  end
end

function cql_div(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x / y;
  end
end

function cql_idiv(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x // y;
  end
end

function cql_mod(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x % y;
  end
end

function cql_lshift(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x << y;
  end
end

function cql_rshift(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x >> y;
  end
end

function cql_bin_and(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x & y;
  end
end

function cql_bin_or(x,y)
  if x == nil or y == nil then
    return nil
  else
    return x | y;
  end
end

function cql_is_true(x)
  return x ~= nil and x ~= 0 and x ~= false;
end

function cql_is_not_true(x)
  return x == nil or x == 0 or x == false;
end

function cql_is_false(x)
  return x == 0 or x == false
end

function cql_is_not_false(x)
  return x ~= 0 and x ~= false
end

function cql_like(x, y)
  if x == nil or y == nil then
    return nil
  end

  local db = sqlite3.open_memory()
  local stmt = db:prepare("SELECT ? LIKE ?")
  stmt:bind(1, x);
  stmt:bind(2, y);
  stmt:step()
  local result = stmt:get_value(0)
  stmt:finalize()
  db:close()
  return cql_to_bool(result)
end;

function cql_unary_sign(x)
  if x == nil then return nil end
  if x == true then return 1 end
  if x == false then return 0 end
  if x < 0 then return -1 end
  if x > 0 then return 1 end
  return 0
end

function cql_unary_not(x)
  if x == nil then return nil end
  return not x
end

function cql_unary_uminus(x)
  if x == nil then return nil end
  return -x
end

function cql_unary_abs(x)
  if x == nil then return nil end
  if x == true then return true end
  if x == false then return false end
  if x < 0 then return -x end
  return x
end

function cql_to_num(b)
  if b == nil then return nil; end
  if b then return 1 end;
  return 0
end

function cql_to_bool(n)
  if n == nil then return nil; end
  if n == false then return false end
  return n ~= 0;
end

function cql_clone_row(row)
  local result = {}
  local k
  local v

  for k, v in pairs(row) do
    result[k] = v
  end
  return result
end

function cql_get_blob_size(blob)
  return #blob
end

-- this needs better error handling
-- the normal SQLite printf is not exposed to lua so we emulate it with a select statement
function cql_printf(fmt, ...)
  fmt = string.gsub(fmt, "'", "''")
  args = {...}
  cmd = "select printf('" .. fmt .."'"
  for i= 1, #args
  do
    cmd = cmd .. ",?"
  end;
  cmd = cmd .. ");"

  -- dummy database
  local db = sqlite3.open_memory()
  local stmt = db:prepare(cmd)
  for i= 1, #args
  do
    stmt:bind(i, args[i])
  end;
  stmt:step()
  local result = stmt:get_value(0)
  stmt:finalize()
  db:close()
  return result
end

function cql_finalize_stmt(stmt)
  if stmt ~= nil then
    stmt:finalize()
  end
end

function cql_prepare(db, sql)
  local stmt = db:prepare(sql)
  return db:errcode(), stmt
end

function cql_get_value(stmt, col)
  return stmt:get_value(col)
end

function cql_no_rows_stmt(db, sql)
  return cql_prepare(db, "select 1 where 0")
end

function cql_step(stmt)
  return stmt:step()
end

function cql_exec(db, sql)
  return db:exec(sql)
end

function cql_multibind(db, stmt, args)
  local rc = sqlite3.OK
  for i = 1, #args
  do
    local rc = stmt:bind(i, args[i])
    if rc ~= sqlite3.OK then break end
  end;
  return rc
end

function cql_prepare_var(db, frag_count, frag_preds, frags)
  sql = ""
  for i = 1, frag_count
  do
     if frag_preds == nil or frag_preds[i-1] then
       sql = sql .. frags[i]
     end
  end
  local stmt = db:prepare(sql)
  return db:errcode(), stmt
end

function cql_exec_var(db, frag_count, frag_preds, frags)
  sql = ""
  for i = 1, frag_count
  do
     if frag_preds == nil or frag_preds[i-1] then
       sql = sql .. frags[i]
     end
  end
  return db:exec(sql)
end

function cql_multibind_var(db, stmt, bind_count, bind_preds, args)
  local bind_index = 1
  local rc = sqlite3.OK
  for i = 1, #args
  do
    if bind_preds[i-1] then
      rc = stmt:bind(bind_index, args[i])
      if rc ~= sqlite3.OK then break end
      bind_index = bind_index + 1
    end
  end;
  return rc
end

function cql_error_trace(rc, db)
  if db:errcode() ~= 0 then
    print("err: ", rc, "db info:", db:errcode(), db:errmsg())
  else
    print("err: ", rc, "thrown exception")
  end
end

function cql_multifetch(stmt, result, args)
  result._has_row_ = false
  rc = stmt:step()
  if rc ~= sqlite3.ROW then return rc end;
  for i = 1, stmt:columns()
  do
    result[args[i]] = stmt:get_value(i-1)
  end
  result._has_row_ = true

  return rc
end


function cql_fetch_all_rows(stmt, args)
  local rc
  local result_set = {}

  repeat
    rc = stmt:step()
    if rc ~= sqlite3.ROW then break end;

    local result = {}
    for i = 1, stmt:columns()
    do
      result[args[i]] = stmt:get_value(i-1)
    end
    result._has_row_ = true
    table.insert(result_set, result)
  until false

  if rc ~= sqlite3.DONE then
     result_set = nil
  else
     rc = sqlite3.OK
  end
  return rc, result_set
end

function cql_to_integer(num)
  if num == true then return 1 end
  if num == false then return 0 end
  if num == nil then return nil end
  return math.floor(num);
end;

function cql_to_float(num)
  if num == true then return 1.0 end
  if num == false then return 0.0 end
  if num == nil then return nil end
  return 0.0 + num;
end;

function cql_to_bool(num)
  if num == false then return false end
  if num == nil then return nil end
  return num ~= 0
end;

function cql_contract_argument_notnull(arg, index)
  if arg == nil then
    print("arg is null -- index", index)
    exit_on_error();
  end
end

function cql_partition_create()
  return {};
end;

function cql_make_str_key(key_table)
  local key = ""
  for k,v in pairs(key_table)
  do
     if k ~= "_has_row_" then
       key = key .. tostring(v)
     end
  end
  return key
end

function cql_hash_string(str)
  local hash = 0;
  local len = #str
  for i=1, len
  do
    byte = string.byte(str, i);
    hash = ((hash << 5) | (hash >> 59)) ~ byte;
  end
  return hash
end

function cql_cursor_hash(key)
  if key == nil or not key._has_row_ then
     return 0
  end

  return cql_hash_string(cql_make_str_key(key))
end

function cql_cursors_equal(k1, k2)
  if k1 == nil and k2 == nil then return true end
  if k1 == nil or k2 == nil then return false end
  if (not k1._has_row_) and not k2._has_row_ then return true end
  if (not k1._has_row_) or not k2._has_row_ then return false end
  if #k1 ~= #k2 then return false end

  for k,v in pairs(k1)
  do
     if v ~= k2[k] then return false end
  end

  return true
end

function cql_partition_cursor(partition, key, cursor)
  if not cursor._has_row_ then return false end
  key = cql_make_str_key(key)
  cursor = cql_clone_row(cursor)
  if partition[key] ~= nil then
     table.insert(partition[key], cursor)
  else
     partition[key] = {cursor}
  end
  return true
end;

function cql_extract_partition(partition, key)
  key = cql_make_str_key(key)
  if partition[key] ~= nil then
     return partition[key]
  else
     if partition.__empty__ == nil then
       partition.__empty__ = {}
     end
     return partition.__empty__
  end
end

function cql_facets_create()
  return {}
end

function cql_facet_find(facets, facet)
   local result = facets[facet]
   if result ~= nil then
     return result
   else
     return -1
   end
end

function cql_facet_upsert(facets, facet, value)
   facets[facet] = value
   return true
end

function cql_facet_add(facets, facet, value)
   if facets[facet] ~= nil then return false end
   facets[facet] = value
   return true
end

function cql_string_dictionary_create()
  return {}
end

function cql_string_dictionary_add(dict, key, val)
  if dict[key] ~= nil then return false end
  dict[key] = val
  return true
end

function cql_string_dictionary_find(dict, key)
  return dict[key]
end

function cql_string_list_create()
  return {}
end

function cql_string_list_get_count(list)
  return #list
end

function cql_string_list_add_string(list, str)
  table.insert(list, str)
end

function cql_string_list_get_string(list, i)
  -- one based index
  return list[i+1]
end

function cql_exec_internal(db, str)
  return db:exec(str)
end

function _cql_contains_column_def(haystack, needle)
  if haystack == nil or needle == nil then return false end
  local i
  local j
  i, j = string.find(haystack, needle)
  if i == nil or i < 2 then return false end
  local ch = string.sub(haystack, i-1, i-1)
  return ch == "(" or ch == " "
end

function cql_best_error(err)
  if err == sqlite3.OK then
    return sqlite3.ERROR
  else
    return err
  end
end

function cql_changes(db)
  return db:changes()
end

function cql_last_insert_rowid(db)
  return db:last_insert_rowid()
end

function cql_box_stmt(stmt)
  return stmt
end

function cql_unbox_stmt(box)
  return box
end

cql_user_data = {}

function cql_set_user_data(k, v)
   cql_user_data[k] = v
end

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

-- TESTING METHODS --
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

-- ---

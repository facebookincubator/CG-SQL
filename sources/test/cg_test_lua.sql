/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare proc printf no check;
declare proc puts no check;

-- basic test table with an auto inc field
create table foo(
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
);

-- second test table with combination of fields
@attribute(bar_is_good=1)
create table bar(
  id INTEGER NOT NULL PRIMARY KEY,
  @attribute(collossal_cave='xyzzy')
  name TEXT,
  rate LONG INT,
  type INTEGER,
  size REAL @create(2)
);

-- test view that reads from the test tables
create view baz as select id, name, type from bar;

-- declare variables of the basic types
declare i0_nullable int;
declare i1_nullable int;
declare r0_nullable real;
declare l0_nullable long integer;
declare l1_nullable long integer;
declare b0_nullable bool;
declare t0_nullable text;

-- same types but not null variant
declare i2 int not null;
declare r2 real not null;
declare l2 long integer not null;
declare b2 bool not null;
declare t2 text not null;

-- initialize for later use
set t2 := "text";

-- TEST: assign eveything not null
-- Note: semantic analysis verifies no chance of
--       assigning nullable to not nullable
-- + i2 = 1
set i2 := 1;

-- TEST: assign rhs not null
-- + i1_nullable = 88
set i1_nullable := 88;

-- remove the nullability improvement
-- + i1_nullable = nil
set i1_nullable := null;

-- TEST: assign everything nullable
-- + i0_nullable = i1_nullable
set i0_nullable := i1_nullable;

-- TEST: assign NULL to nullable string
-- + t0_nullable = nil
set t0_nullable := null;

-- + t0_nullable = t2
set t0_nullable := t2;

-- remove the nullability improvement
-- + t0_nullable = nil
set t0_nullable := null;

-- TEST: simple unary operators
-- + SET i2 := - -1;
-- + i2 = - - 1
set i2 := - -1;

-- + i0_nullable = - i2
set i0_nullable := -i2;

-- + i0_nullable = cql_unary_uminus(nil)
set i0_nullable := -null;

-- + i1_nullable = cql_unary_uminus(i0_nullable)
set i1_nullable := -i0_nullable;

-- + r0_nullable = 2.2
set r0_nullable := 2.2;

-- remove the nullability improvement
-- + r0_nullable = nil
set r0_nullable := null;

-- + r2 = 3.5
set r2 := 3.5;

-- + i1_nullable = cql_to_num(cql_unary_not(cql_unary_not(cql_to_bool(i0_nullable))))
set i1_nullable := NOT NOT i0_nullable;

-- + i2 = cql_to_num(not not b2)
set i2 := NOT NOT b2;

-- TEST: not null arithmetic
-- + i2 = 1 * 3 + 5
set i2 := 1 * 3 + 5;

-- TEST: everything in sight is nullable
-- + r0_nullable = cql_mul(r0_nullable, i1_nullable)
set r0_nullable := r0_nullable * i1_nullable;

-- TEST: right operand is not null
-- + r0_nullable = cql_mul(r0_nullable, i2)
set r0_nullable := r0_nullable * i2;

-- TEST: left operand is not null
-- + i0_nullable = cql_mul(12, i1_nullable)
set i0_nullable := 12 * i1_nullable;

-- TEST: an operaand is actually null
-- + i0_nullable = nil
set i0_nullable := null * i1_nullable;

-- TEST: make sure the stacking is working correctly
-- +  r0_nullable = cql_add(cql_mul(r0_nullable, i1_nullable), cql_mul(r0_nullable, i1_nullable))
set r0_nullable := r0_nullable * i1_nullable + r0_nullable * i1_nullable;

-- TEST: a more complex stacking example
-- + r0_nullable = cql_add(cql_add(cql_mul(r0_nullable, i1_nullable), cql_mul(r0_nullable, i0_nullable)), r0_nullable)
set r0_nullable := (r0_nullable * i1_nullable + r0_nullable * i0_nullable) + r0_nullable;

-- TEST: string assignment -- nasty string
-- + t2 = "This is a \" \\ test '' \n \" "
set t2 := "This is a \" \\ test '' \n \" ";

-- TEST: call an external procedure (type not known)
-- + printf("Hello, world\n")
call printf("Hello, world\n");

-- TEST: logical AND with short circuit
-- + i2 = cql_to_num(cql_to_bool(r2) and cql_to_bool(l2))
set i2 := r2 and l2;

-- helper methods for the next test
declare function side_effect1() integer;
declare function side_effect2() integer;

-- TEST: the operands have side effects, the short circuit must not
-- do the evaluation of the side effect for the second arg if the first
-- returns false.  This is the trickiest case because ti looks like it's
-- safe  to use the (x && y) form because the operands are non-null.
-- it isn't though because there was expression work to get to the non-null
-- state.  The Coalesce is important to this test for that reason.
-- + repeat
-- +   _tmp_n_int_1 = side_effect1()
-- +   if _tmp_n_int_1 ~= nil then
-- +     _tmp_int_0 = _tmp_n_int_1
-- +     break
-- +   end
-- +   _tmp_int_0 = 7
-- + until true
-- the first arg was fully evaluated into a temp
-- + i2 = cql_to_num(cql_shortcircuit_and(_tmp_int_0,
-- + function()
-- second arg is deferred
-- + repeat
-- +   _tmp_n_int_2 = side_effect2()
-- +   if _tmp_n_int_2 ~= nil then
-- +     _tmp_int_1 = _tmp_n_int_2
-- +     break
-- +   end
-- +   _tmp_int_1 = 5
-- +   until true
-- +   return _tmp_int_1
-- + end
-- + ))
set i2 := coalesce(side_effect1(), 7) and coalesce(side_effect2(), 5);

-- TEST: trival NULL on AND
-- i0_nullable = cql_to_num(nil)
set i0_nullable := NULL and NULL;

-- TEST: logical AND with nullables
-- + i0_nullable = cql_to_num(cql_shortcircuit_and(i0_nullable, function() return i1_nullable end))
set i0_nullable := i0_nullable and i1_nullable;

-- TEST: logical AND with constant nulls
-- + i0_nullable = cql_to_num(cql_shortcircuit_and(nil, function() return i1_nullable end))
set i0_nullable := NULL and i1_nullable;

-- TEST: logical AND with constant nulls
-- + i0_nullable = cql_to_num(cql_shortcircuit_and(i0_nullable, function() return nil end))
set i0_nullable := i0_nullable and NULL;

-- TEST: logical OR with short circuit
-- + i2 = cql_to_num(cql_to_bool(r2) or cql_to_bool(l2))
set i2 := r2 or l2;

-- TEST: complex side effect, looks safe (no nullables) but it isn't because of codegen on the right
-- + repeat
-- +   _tmp_n_int_1 = side_effect1()
-- +   if _tmp_n_int_1 ~= nil then
-- +     _tmp_int_0 = _tmp_n_int_1
-- +     break
-- +   end
-- +   _tmp_int_0 = 7
-- + until true
-- + i2 = cql_to_num(cql_shortcircuit_or(_tmp_int_0,
-- + function()
-- + repeat
-- +   _tmp_n_int_2 = side_effect2()
-- +   if _tmp_n_int_2 ~= nil then
-- +     _tmp_int_1 = _tmp_n_int_2
-- +     break
-- +   end
-- +   _tmp_int_1 = 5
-- + until true
-- + return _tmp_int_1
-- + end
-- + ))
set i2 := coalesce(side_effect1(), 7) or coalesce(side_effect2(), 5);

-- TEST: trival NULL on OR
-- + i0_nullable = nil
set i0_nullable := NULL or NULL;

-- TEST: logical OR with nullables
-- +  i0_nullable = cql_to_num(cql_shortcircuit_or(i0_nullable, function() return i1_nullable end))
set i0_nullable := i0_nullable or i1_nullable;

-- TEST: logical OR with constant nulls
-- + i0_nullable = cql_to_num(cql_shortcircuit_or(nil, function() return i1_nullable end))
set i0_nullable := NULL or i1_nullable;

-- TEST: logical OR with constant nulls
-- + i0_nullable = cql_to_num(cql_shortcircuit_or(i0_nullable, function() return nil end))
set i0_nullable := i0_nullable or NULL;

-- TEST: is null basic test
-- + i2 = cql_to_num(nil == nil)
set i2 := null is null;

-- TEST: is null test general case
-- +i2 = cql_to_num(cql_add(i0_nullable, i1_nullable) == nil))
set i2 := (i0_nullable + i1_nullable) is null;

-- TEST: is not null basic test
-- + i2 = cql_to_num(nil ~= nil)
set i2 := null is not null;

-- TEST: is not null test general case
-- + i2 = cql_to_num(cql_add(i0_nullable, i1_nullable) ~= nil)
set i2 := (i0_nullable + i1_nullable) is not null;

-- TEST: complex if/else pattern
-- + if true then
-- +   i2 = 1
-- + else
-- +   if cql_eq(i0_nullable, i1_nullable) then
-- +     i2 = 2
-- +   else
-- +     i2 = 3
-- +   end
-- + end
if 1 then
 set i2 := 1;
else if i0_nullable == i1_nullable then
 set i2 := 2;
else
 set i2 := 3;
end if;

-- TEST: complex if/else pattern, embedded logical operation
-- + if true then
-- +   i2 = 1
-- + else
-- +   if cql_shortcircuit_or(i0_nullable, function() return i1_nullable end) then
-- +     i2 = 2
-- +   else
-- +     i2 = 3
-- +   end
-- + end
if 1 then
 set i2 := 1;
else if i0_nullable or i1_nullable then
 set i2 := 2;
else
 set i2 := 3;
end if;


-- TEST: simple procedure with external call
-- note that in lua even an integer argument could come in as nil because
-- all args are fully flexiblie so that means the contract is not optional
-- do not remove
-- + function test(i)
-- ------------------------------------------
-- do not remove this contract it's necessary
-- + cql_contract_argument_notnull(i, 1)
-- ------------------------------------------
-- +   if cql_to_bool(i) then
-- +     puts("true")
-- +   end
-- + end

create procedure test(i integer not null)
begin
  if i then
    call puts('true');
  end if;
end;

-- TEST: guard statements are simply rewritten to if statements
-- + if a ~= nil then
-- +    goto cql_cleanup -- return
-- + ::cql_cleanup::
create proc proc_with_return_guard(a int)
begin
  if a is not null return;
  let x := a;
end;

-- TEST: simple between
-- + SET b2 := BETWEEN REWRITE _between_0_ := 1 CHECK (_between_0_ >= 0 AND _between_0_ <= 3);
-- + _between_0_ = 1
-- + b2 = _between_0_ >= 0 and _between_0_ <= 3
set b2 := 1 between 0 and 3;

-- TEST: between with some nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_1_ := i1_nullable CHECK (_between_1_ >= i0_nullable AND _between_1_ <= r2);
-- + _between_1_ = i1_nullable
-- + i0_nullable = cql_to_num(cql_shortcircuit_and(cql_ge(_between_1_, i0_nullable), function() return cql_le(_between_1_, r2) end))
set i0_nullable := i1_nullable between i0_nullable and r2;

-- TEST: between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_2_ := i1_nullable CHECK (_between_2_ >= r2 AND _between_2_ <= i0_nullable);
-- + _between_2_ = i1_nullable
-- + i0_nullable = cql_to_num(cql_shortcircuit_and(cql_ge(_between_2_, r2), function() return cql_le(_between_2_, i0_nullable) end))
set i0_nullable := i1_nullable between r2 and i0_nullable;

-- TEST: simple not between
-- + SET b2 := BETWEEN REWRITE _between_3_ := 1 CHECK (_between_3_ < 0 OR _between_3_ > 3);
-- + _between_3_ = 1
-- + b2 = _between_3_ < 0 or _between_3_ > 3
set b2 := 1 not between 0 and 3;

-- TEST: not between with some nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_4_ := i1_nullable CHECK (_between_4_ < i0_nullable OR _between_4_ > r2);
-- + _between_4_ = i1_nullable
-- + i0_nullable = cql_to_num(cql_shortcircuit_or(cql_lt(_between_4_, i0_nullable), function() return cql_gt(_between_4_, r2) end))
set i0_nullable := i1_nullable not between i0_nullable and r2;

-- TEST: not between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_5_ := i1_nullable CHECK (_between_5_ < r2 OR _between_5_ > i0_nullable);
-- + _between_5_ = i1_nullable
-- + i0_nullable = cql_to_num(cql_shortcircuit_or(cql_lt(_between_5_, r2), function() return cql_gt(_between_5_, i0_nullable) end))
set i0_nullable := i1_nullable not between r2 and i0_nullable;

-- TEST: out parameter test
-- + function out_test()
-- +   local i = 0
-- +   local ii
-- +   i = i2
-- +   ii = i0_nullable
-- +   return i, ii
-- + end
create procedure out_test(out i integer not null, out ii integer)
begin
  set i := i2;
  set ii := i0_nullable;
end;

-- TEST: long storage (it's all the same in LUA)
-- + local longint_var
declare longint_var long integer;

-- TEST long mul
-- + longint_var = cql_mul(cql_add(l0_nullable, l1_nullable), 5)
set longint_var := (l0_nullable + l1_nullable) * 5;

-- TEST: make a cursor
-- +  _rc_, foo_cursor_stmt = cql_prepare(_db_,
-- +    "SELECT id, ? FROM foo WHERE id = ?")
-- +2 if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  _rc_ = cql_multibind(_db_, foo_cursor_stmt, "Ii", {i2, i0_nullable})
declare foo_cursor cursor for select id, i2 from foo where id = i0_nullable;

-- TEST: fetch a cursor
-- + _rc_ = cql_multifetch(foo_cursor_stmt, foo_cursor, foo_cursor_types_, foo_cursor_fields_)
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + i0_nullable = foo_cursor.id
-- + i2 = foo_cursor.i2
fetch foo_cursor into i0_nullable, i2;

-- TEST: test elementary cursor on select with no tables, still round trips through sqlite
declare col1 integer;
declare col2 real not null;

-- +  _rc_, basic_cursor_stmt = cql_prepare(_db_,
-- +    "SELECT 1, 2.5")
-- +  if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
declare basic_cursor cursor for select 1, 2.5;

-- +  _rc_ = cql_multifetch(basic_cursor_stmt, basic_cursor, basic_cursor_types_, basic_cursor_fields_)
-- +  if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  col1 = basic_cursor._anon0
-- +  col2 = basic_cursor._anon1
fetch basic_cursor into col1, col2;

-- + cql_finalize_stmt(basic_cursor_stmt)
-- + basic_cursor_stmt = nil
close basic_cursor;

-- TEST: the most expensive way to swap two variables ever :)
declare arg1 integer not null;
declare arg2 integer not null;
set arg1 := 7;
set arg2 := 11;

-- +   _rc_, exchange_cursor_stmt = cql_prepare(_db_,
-- +     "SELECT ?, ?")
-- +2  if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  _rc_ = cql_multibind(_db_, exchange_cursor_stmt, "II", {arg2, arg1})
declare exchange_cursor cursor for select arg2, arg1;

-- +  _rc_ = cql_multifetch(exchange_cursor_stmt, exchange_cursor, exchange_cursor_types_, exchange_cursor_fields_)
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + arg1 = exchange_cursor.arg2
-- + arg2 = exchange_cursor.arg1
fetch exchange_cursor into arg1, arg2;

-- + cql_finalize_stmt(exchange_cursor_stmt)
-- + exchange_cursor_stmt = nil
close exchange_cursor;

-- TEST: simple nested select
-- +  _rc_, _temp_stmt = cql_prepare(_db_,
-- +    "SELECT ? + 1")
-- +  _rc_ = cql_multibind(_db_, _temp_stmt, "I", {i2})
-- +  cql_finalize_stmt(_temp_stmt)
set i2 := (select i2+1);

-- TEST: nested select with nullable
-- in LUA these bind the same
-- +  _rc_, _temp_stmt = cql_prepare(_db_,
-- +    "SELECT ? + 1")
-- +  _rc_ = cql_multibind(_db_, _temp_stmt, "i", {i0_nullable})
-- +  cql_finalize_stmt(_temp_stmt)
set i0_nullable := (select i0_nullable+1);

-- TEST: tricky quoted text
-- this validates that the C escaping works right when making SQL
-- + "DELETE FROM bar WHERE name LIKE '\\\\ \" \\n'"
delete from bar where name like '\\ " \n';

-- TEST: binding an out parameter
-- + function outparm_test(_db_)
-- +  local foo
-- +  foo = 1
-- +  "DELETE FROM bar WHERE id = ?")
-- +  _rc_ = cql_multibind(_db_, _temp_stmt, "I", {foo})
-- +  return _rc_, foo
create procedure outparm_test(out foo integer not null)
begin
 set foo := 1;
 delete from bar where id = foo;
end;

-- TEST: a simple stored proc that throws
-- + function throwing(_db_)
-- +   local _rc_ = CQL_OK
-- +     _rc_ = cql_exec(_db_,
-- +       "DELETE FROM bar")
-- +     if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto catch_start_1; end
-- +     goto catch_end_1
-- +   ::catch_start_1::
-- +   do
-- +     local _rc_thrown_1 = _rc_
-- +     printf("error\n")
-- +     _rc_ = cql_best_error(_rc_thrown_1)
-- +     goto cql_cleanup
-- +   end
-- + ::catch_end_1::
-- +   _rc_ = CQL_OK
-- +
-- + ::cql_cleanup::
-- +   return _rc_
-- + end
create procedure throwing()
begin
  begin try
   delete from bar;
  end try;
  begin catch
   call printf("error\n");
   throw;
  end catch;
end;

-- TEST: a simple case expression
-- + repeat
-- +   if true then
-- +     i2 = 100
-- +     break
-- +   end
-- +   if cql_to_bool(2) then
-- +     i2 = 200
-- +     break
-- +   end
-- +   i2 = 300
-- + until true
set i2 := case when 1 then 100 when 2 then 200 when null then 500 else 300 end;

-- TEST: a simple in expression
-- +  repeat
-- +    _tmp_int_1 = 3
-- +    _tmp_bool_0 = false
-- +    if _tmp_int_1 == 1 then break end
-- +    if _tmp_int_1 == 2 then break end
-- +    if _tmp_int_1 == 4 then break end
-- +    _tmp_bool_0 = true
-- + until true
-- + i2 = cql_to_num(_tmp_bool_0)
set i2 := 3 in (1, 2, null, 4);

-- TEST: in with nullables
-- + repeat
-- +   _tmp_n_int_1 = i1_nullable
-- +   if _tmp_n_int_1 == nil then
-- +     _tmp_n_bool_0 = nil
-- +     break
-- +   end
-- +   _tmp_n_bool_0 = true
-- +   if _tmp_n_int_1 == 1 then break end
-- +   if _tmp_n_int_1 == 2 then break end
-- +   if _tmp_n_int_1 == cql_to_num(b0_nullable) then break end
-- +   _tmp_n_bool_0 = false
-- + until true
-- + i0_nullable = cql_to_num(_tmp_n_bool_0)
set i0_nullable := i1_nullable in (1, 2, null, b0_nullable);

-- TEST: a simple not in expression
-- + repeat
-- +   _tmp_int_1 = 3
-- +   _tmp_bool_0 = false
-- +   if _tmp_int_1 == 1 then break end
-- +   if _tmp_int_1 == 2 then break end
-- +   if _tmp_int_1 == 4 then break end
-- +   _tmp_bool_0 = true
-- + until true
-- + i2 = cql_to_num(_tmp_bool_0)
set i2 := 3 not in (1, 2, null, 4);

-- TEST: not in with nullables
-- + repeat
-- +   _tmp_n_int_1 = i1_nullable
-- +   if _tmp_n_int_1 == nil then
-- +     _tmp_n_bool_0 = nil
-- +     break
-- +   end
-- +   _tmp_n_bool_0 = false
-- +   if _tmp_n_int_1 == 1 then break end
-- +   if _tmp_n_int_1 == 2 then break end
-- +   if _tmp_n_int_1 == cql_to_num(b0_nullable) then break end
-- +   _tmp_n_bool_0 = true
-- + until true
-- + i0_nullable = cql_to_num(_tmp_n_bool_0)
set i0_nullable := i1_nullable not in (1, 2, null, b0_nullable);

-- TEST: between with strings
-- + SET b2 := BETWEEN REWRITE _between_6_ := 'b' CHECK (_between_6_ >= 'a' AND _between_6_ <= 'c');
-- + _between_6_ = "b"
-- + b2 = _between_6_ >= "a" and _between_6_ <= "c"
set b2 := 'b' between 'a' and 'c';

-- TEST: between with nullable strings right
-- + _between_7_ = "b"
-- + b0_nullable = cql_shortcircuit_and(_between_7_ >= "a", function() return cql_le(_between_7_, t0_nullable) end)
set b0_nullable := 'b' between 'a' and t0_nullable;

-- TEST: between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_8_ := 'b' CHECK (_between_8_ >= t0_nullable AND _between_8_ <= 'c');
-- + _between_8_ = "b"
-- + b0_nullable = cql_shortcircuit_and(cql_ge(_between_8_, t0_nullable), function() return _between_8_ <= "c" end)
set b0_nullable := 'b' between t0_nullable and 'c';

-- TEST: between with nullable strings null operand
-- + SET b0_nullable := BETWEEN REWRITE _between_9_ := 'b' CHECK (_between_9_ >= NULL AND _between_9_ <= 'c');
-- + _between_9_ = "b"
-- + b0_nullable = cql_shortcircuit_and(nil, function() return _between_9_ <= "c" end)
set b0_nullable := 'b' between null and 'c';

-- TEST: not between with strings
-- + _between_10_ = "b"
-- + b2 = _between_10_ < "a" or _between_10_ > "c"
set b2 := 'b' not between 'a' and 'c';

-- TEST: not between with nullable strings right
-- + SET b0_nullable := BETWEEN REWRITE _between_11_ := 'b' CHECK (_between_11_ < 'a' OR _between_11_ > t0_nullable);
-- + _between_11_ = "b"
-- + b0_nullable = cql_shortcircuit_or(_between_11_ < "a", function() return cql_gt(_between_11_, t0_nullable) end)
set b0_nullable := 'b' not between 'a' and t0_nullable;

-- TEST: not between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_12_ := 'b' CHECK (_between_12_ < t0_nullable OR _between_12_ > 'c');
-- + _between_12_ = "b"
-- + b0_nullable = cql_shortcircuit_or(cql_lt(_between_12_, t0_nullable), function() return _between_12_ > "c" end)
set b0_nullable := 'b' not between t0_nullable and 'c';

-- TEST: not between with nullable strings null operand
-- + SET b0_nullable := BETWEEN REWRITE _between_13_ := 'b' CHECK (_between_13_ < NULL OR _between_13_ > 'c');
-- + _between_13_ = "b"
-- + b0_nullable = cql_shortcircuit_or(nil, function() return _between_13_ > "c" end)
set b0_nullable := 'b' not between null and 'c';

-- TEST: this procedure will have a structured semantic type
-- + function with_result_set(_db_)
-- + _rc_, _result_stmt = cql_prepare(_db_,
-- + "SELECT id, name, rate, type, size FROM bar")
-- + if _rc_ == CQL_OK and _result_stmt == nil then _rc_, _result_stmt = cql_no_rows_stmt(_db_) end
-- + function with_result_set_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Islid", { "id", "name", "rate", "type", "size" })
create procedure with_result_set()
begin
  select * from bar;
end;

-- TEST: grabs values from a view that is backed by a table
-- + function select_from_view(_db_)
-- + "SELECT id, type FROM baz")
-- + if _rc_ == CQL_OK and _result_stmt == nil then _rc_, _result_stmt = cql_no_rows_stmt(_db_) end
-- + return _rc_, _result_stmt
-- + function select_from_view_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Ii", { "id", "type" })
-- + return _rc_, result_set
create proc select_from_view()
begin
  select id, type from baz;
end;

-- TEST: create dml for a view
-- + "CREATE VIEW MyView AS SELECT 1 AS f1, 2 AS f2, 3 AS f3")
create procedure make_view()
begin
   create view MyView as select 1 as f1, 2 as f2, 3 as f3;
end;

-- TEST: code gen a simple create index statement
-- + "CREATE INDEX index_1 ON bar (id)"
create procedure make_index()
begin
  create index index_1 on bar(id);
end;

-- TEST: create a proc with reader logic with more than one arg
-- + function get_data(_db_, name_, id_)
-- + function get_data_fetch_results(_db_, name_, id_)
-- + _rc_, stmt = get_data(_db_, name_, id_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Islid", { "id", "name", "rate", "type", "size" })
create procedure get_data(name_ text not null, id_ integer not null)
begin
  select * from bar where id = id_ and name = name_;
end;

-- TEST: create a proc that uses the new cursor fetch strategy
--       then bind values from those implicit variables in a CQL statement
--       and also bind the _has_rows auto local as well
-- validate auto variable management
-- + function easy_fetch(_db_)
-- +   local C_stmt = nil
-- +   local C = { _has_row_ = false }
-- +   local C_fields_ = { "id", "name", "rate", "type", "size" }
-- +   local C_types_ = "Islid"
-- +   local C2 = { _has_row_ = false }
-- +   local C2_fields_ = { "id", "name", "rate", "type", "size" }
-- +   local C2_types_ = "Islid"
-- +   _rc_, C_stmt = cql_prepare(_db_,
-- +   "SELECT id, name, rate, type, size FROM bar")
-- +   _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- +   printf("%d %s\n", C.id, C.name)
-- +   _rc_, C2_stmt = cql_prepare(_db_,
-- +     "SELECT id, name, rate, type, size FROM bar WHERE ? AND id = ?")
-- +   _rc_ = cql_multibind(_db_, C2_stmt, "FI", {C._has_row_, C.id})
-- +   cql_finalize_stmt(C_stmt)
-- +   cql_finalize_stmt(C2_stmt)
-- +   return _rc_
@attribute(cql:vault_sensitive)
create proc easy_fetch()
begin
  declare C cursor for select * from bar;
  fetch C;
  call printf("%d %s\n", C.id, C.name);
  declare C2 cursor for select * from bar where C and id = C.id;
end;

-- TEST: safe not nullable assignment
-- + repeat
-- +   if i0_nullable ~= nil then
-- +     i2 = i0_nullable
-- +     break
-- +   end
-- +   i2 = 3
-- + until true
set i2 := ifnull(i0_nullable, 3);

-- TEST: this works too, but the result might be nullable
-- + repeat
-- +   if i0_nullable ~= nil then
-- +     break
-- +   end
-- +   i0_nullable = i1_nullable
-- + until true
set i0_nullable := ifnull(i0_nullable, i1_nullable);

-- TEST: create a proc that passes in to out, the out arg must be created as a local
-- + function copy_int(a)
-- +  local b
-- +  b = a
-- +  return b
create proc copy_int(a int, out b int)
begin
  set b := a;
end;

-- TEST: simple in/out function call
-- + i1_nullable = copy_int(i0_nullable)
call copy_int(i0_nullable, i1_nullable);

-- TEST: try out last_insert_rowid()
-- + local row
-- + row = cql_last_insert_rowid(_db_)
-- - cleanup
create proc insert_rowid_reader()
begin
  declare row long integer;
  set row := last_insert_rowid();
end;

-- TEST: try out changes()
-- + local ct
-- + ct = cql_changes(_db_)
-- - cleanup
create proc changes_reader()
begin
  declare ct integer;
  set ct := changes();
end;

-- + local s
declare s text not null;

-- TEST: try out printf expression
-- + s = cql_printf("%d and %d", 1, 2)
set s := printf('%d and %d', 1, 2);

-- TEST: simple string management for printf
-- + s = cql_printf("%d and %d", 3, 4)
set s := printf('%d and %d', 3, 4);

-- TEST: printf inserts casts for numeric types (but only as needed)
-- + s = cql_printf("%lld %lld %lld %llu %d %d %llu %d %f %f %s %f", cql_to_integer(4), cql_to_integer(5), cql_to_integer(true), 0, cql_to_integer(false), 0, 6, 7, 0.0, 0.0, nil, cql_to_float(8))
set s := printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 4, nullable(5), true, null, false, null, 6L, 7, 0.0, null, null, 8);

-- TEST: printf doesn't insert casts when used in SQL
-- + SELECT printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 5, 5, 1, NULL, 0, NULL, 6, 7, 0.0, NULL, NULL, 8)
set s := (select printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 5, nullable(5), true, null, false, null, 6L, 7, 0.0, null, null, 8));

-- TEST: make sure that we use the canonical name for 's' in codegen not 'S'.  Even though S is legal.
-- + s = "x"
set S := 'x';

-- TEST: declare proc and call it
-- + DECLARE PROC xyzzy (id INTEGER) (A INTEGER NOT NULL);
declare proc xyzzy(id integer) ( A integer not null );

-- TEST: call declared proc, capture statement in a cursor
-- + local xyzzy_cursor = { _has_row_ = false }
-- + _rc_, xyzzy_cursor_stmt = xyzzy(_db_, 1)
-- + cql_finalize_stmt(xyzzy_cursor_stmt)
create proc xyzzy_test()
begin
  declare xyzzy_cursor cursor for call xyzzy(1);
end;

-- TEST: declare a simple proc with no dml
-- + DECLARE PROC plugh (id INTEGER);
declare proc plugh(id integer);

-- TEST: create a proc that returns a mix of possible types in a select
-- + "SELECT 1, 2, CAST(3 AS LONG_INT), 3.0, 'xyz', NULL")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "FILDSf", { "_bool", "_integer", "_longint", "_real", "_text", "_nullable_bool" })
create proc complex_return()
begin
  select TRUE as _bool,
   2 as _integer,
   cast(3 as long integer) as _longint,
   3.0 as _real,
   'xyz' as _text,
   cast(null as bool) as _nullable_bool;
end;

-- TEST: create a proc with a nested select within an in statement for hierarchical queries
create proc hierarchical_query(rate_ long integer not null, limit_ integer not null, offset_ integer not null)
-- + "SELECT id FROM foo WHERE id IN (SELECT id FROM bar WHERE rate = ? ORDER BY name LIMIT ? OFFSET ?) ORDER BY id")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "I", { "id" })
begin
  select *
  from foo
  where id in (
    select id
    from bar
    where rate = rate_
    order by name
    limit limit_
    offset offset_
  )
  order by id;
end;

-- TEST: create a proc with a nested select within a not in statement for hierarchical queries
-- + "SELECT id FROM foo WHERE id NOT IN (SELECT id FROM bar WHERE rate = ? ORDER BY name LIMIT ? OFFSET ?) ORDER BY id")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "I", { "id" })
create proc hierarchical_unmatched_query(rate_ long integer not null, limit_ integer not null, offset_ integer not null)
begin
  select *
  from foo
  where id not in (
    select id
    from bar
    where rate = rate_
    order by name
    limit limit_
    offset offset_
  )
  order by id;
end;

-- TEST: create a proc with a compound select union form
-- +  "SELECT 1 UNION SELECT 2"
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "I", { "A" })
create proc union_select()
begin
 select 1 as A union select 2 as A;
end;

-- TEST: create a proc with a compound select union all form
-- + "SELECT 1 UNION ALL SELECT 2")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "I", { "A" })
create proc union_all_select()
begin
 select 1 as A union all select 2 as A;
end;

-- TEST: create a valid union using not null columns and nullable matching
-- + "SELECT 'foo' UNION ALL SELECT name FROM bar")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "s", { "name" })
create proc union_all_with_nullable()
begin
  select nullable('foo') as name
  union all
  select name from bar;
end;

-- TEST: create a simple with statement
-- + local C_fields_ = { "a", "b", "c" }
-- + local C_types_ = "III"
-- + "WITH X (a, b, c) AS (SELECT 1, 2, 3) SELECT a, b, c FROM X")
-- +  _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- - fetch_results
create proc with_stmt_using_cursor()
begin
  declare C cursor for
    with X(a,b,c) as (select 1,2,3)
    select * from X;
  fetch C;
end;

-- TEST: with statement top level
-- + "WITH X (a, b, c) AS (SELECT 1, 2, 3) SELECT a, b, c FROM X")
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "III", { "a", "b", "c" })
create proc with_stmt()
begin
  with X(a,b,c) as (select 1,2,3) select * from X;
end;

-- TEST: with recursive statement top level
-- + "WITH RECURSIVE X (a, b, c) AS (SELECT 1, 2, 3 UNION ALL SELECT 4, 5, 6) SELECT a, b, c FROM X"
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "III", { "a", "b", "c" })
create proc with_recursive_stmt()
begin
  with recursive X(a,b,c) as (select 1,2,3 union all select 4,5,6) select * from X;
end;

-- TEST: parent procedure
-- + "SELECT 1, 2, 3"
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "III", { "one", "two", "three" })
create proc parent_proc()
begin
  select 1 as one, 2 as two, 3 as three;
end;

-- TEST: child procedure
-- +  "SELECT 4, 5, 6"
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "III", { "four", "five", "six" })
create proc parent_proc_child()
begin
  select 4 as four, 5 as five, 6 as six;
end;


-- TEST: fetch nullable output parameter
-- + local C = { _has_row_ = false }
-- + local C_fields_ = { "_anon0" }
-- + local C_types_ = "I"
-- + "SELECT 1")
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + output = C._anon0
-- + result = C._has_row_
-- + return _rc_, output, result
create proc outint_nullable(out output integer, out result bool not null)
begin
  declare C cursor for select 1;
  fetch C into output;
  set result := C;
END;

-- TEST: fetch not null output parameter
-- + local C_fields_ = { "_anon0" }
-- + local C_types_ = "I"
-- + "SELECT 1")
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + output = C._anon0
-- + return _rc_, output, result
create proc outint_notnull(out output integer not null, out result bool not null)
begin
  declare C cursor for select 1;
  fetch C into output;
  set result := C;
END;

declare function simple_func(int1 integer) integer;

-- TEST: call external function
-- + result = simple_func(2)
let result := simple_func(2);

-- TEST: call external function
-- + _tmp_n_int_% = simple_func(1)
-- + result = simple_func(_tmp_n_int_%)
set result := simple_func(simple_func(1));

declare function text_func(int1 integer, int2 integer not null) text not null;
declare text_result text;

-- TEST: call external text function
-- + _tmp_text_0 = text_func(123, 456)
-- + text_result = _tmp_text_0
set text_result := text_func(123, 456);

-- TEST: create object variable
-- + local obj_var
declare obj_var object;

-- TEST: assign null to object variable
-- + obj_var = nil
set obj_var := null;

-- TEST: declare not null object
-- + local obj_var2
declare obj_var2 object not null;

declare function obj_notnull_func() object not null;

-- initialize for later use
set obj_var2 := obj_notnull_func();

-- TEST: assign var to object variable
-- + obj_var = obj_var2
set obj_var := obj_var2;

-- remove the nullability improvement
-- + obj_var = nil
set obj_var := null;

-- TEST: object comparison
-- +  b0_nullable = cql_eq(obj_var, obj_var)
set b0_nullable := obj_var == obj_var;

-- TEST: object variable in IN clause
-- + _tmp_n_object_% = obj_var
-- + if _tmp_n_object_% == obj_var then break end
-- + b0_nullable = _tmp_n_bool_%
set b0_nullable := obj_var in (obj_var, obj_var);

-- TEST: object variable in IN clause
-- +_tmp_object_% = obj_var2
-- + if _tmp_object_% == obj_var2 then break end
set b2 := obj_var2 in (obj_var2, obj_var2);

-- TEST: object variable in NOT IN clause
-- +  _tmp_n_object_% = obj_var
-- +  if _tmp_n_object_% == nil then
-- +  if _tmp_n_object_% == obj_var then break end
set b0_nullable := obj_var not in (obj_var, obj_var);

-- TEST: object variable in NOT IN clause
-- +  _tmp_object_% = obj_var2
-- +  if _tmp_object_% == obj_var2 then break end
set b2 := obj_var2 not in (obj_var2, obj_var2);

-- TEST: proc with object args (boring in LUA)
-- + function obj_proc()
-- +   local an_object
-- +   an_object = nil
-- +   return an_object
-- + end
create proc obj_proc(out an_object object)
begin
  set an_object := null;
end;

-- TEST: cursor with object in it
-- + function cursor_with_object(object_)
-- + C._has_row_ = true
-- + C.object_ = object_
-- + _result_ = cql_clone_row(C)
-- + function cursor_with_object_fetch_results(object_)
-- +  _result_ = cursor_with_object(object_)
-- +  result_set = { _result_ }
create proc cursor_with_object(object_ object)
begin
  declare C cursor like cursor_with_object arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: case statement with objects
-- + _tmp_n_object_1 = obj_var
-- + if _tmp_n_object_1 == nil then goto case_else_1 end
-- + if _tmp_n_object_1 == obj_var then
set i2 := case obj_var when obj_var then 1 else 2 end;

-- TEST: case statement with returning objects
-- + obj_var = obj_var2
-- + obj_var = nil
set obj_var := case 1 when 1 then obj_var2 else null end;

declare function obj_func() object;

-- TEST: function invocation with object function
-- + obj_var = obj_func()
set obj_var := obj_func();

declare function obj_func_create() create object;

-- TEST: function invocation with creater object function
-- + obj_var = obj_func_create()
set obj_var := obj_func_create();

declare function text_func_create() create text;

-- TEST: function invocation with creater text function
-- + text_result = text_func_create()
set text_result := text_func_create();

-- TEST: assign nullable to object with helper or crash
-- +  _tmp_n_object_0 = obj_func()
-- +  cql_invariant(_tmp_n_object_0 ~= nil)
-- +  obj_var2 = _tmp_n_object_0
set obj_var2 := ifnull_crash(obj_func());

-- TEST: assign nullable to object with helper or throw
-- + _tmp_n_object_% = obj_func()
-- + if _tmp_n_object_% == nil then
-- +   _rc_ = CQL_ERROR
-- +   goto cql_cleanup
-- + end
-- + obj_var2 = _tmp_n_object_%
set obj_var2 := ifnull_throw(obj_func());

-- TEST: assign nullable to object with helper or crash
-- + _tmp_n_object_% = obj_func_create()
-- + cql_invariant(_tmp_n_object_% ~= nil)
-- + obj_var2 = _tmp_n_object_%
set obj_var2 := ifnull_crash(obj_func_create());

-- TEST: assign nullable int to an integer
-- + cql_invariant(i0_nullable ~= nil)
-- + i2 = i0_nullable
set i2 := ifnull_crash(i0_nullable);

-- TEST: assign nullable int to an integer or throw
-- + if i0_nullable == nil then
-- +   _rc_ = CQL_ERROR
-- +   goto cql_cleanup
-- + end
-- + i2 = i0_nullable
set i2 := ifnull_throw(i0_nullable);

-- TEST: unused temp in unary not emitted
-- - _tmp
-- + o = i
-- + o = - 1
create proc unused_temp(i integer, out o integer not null)
begin
  set o := coalesce(i, -1);
end;

-- TEST: echo something to the output
-- + Garbonzo
-- + chick pea
@echo lua, "local Garbonzo -- a chick pea\n";

-- TEST: echo all the escape characters that are supported
-- + --
-- + '%'
@echo lua, "--\/'\a\b\f\t\v'\r\n";

-- TEST: echo inside a procedure
-- + function echo_test()
-- + s = "before echo"
-- + s = "omg echo"
-- + s = "before echo"
create proc echo_test()
begin
  declare s text;
  SET s := "before echo";
  @echo lua, "s = \"omg echo\"\n";
  SET s := "after echo";
end;

-- TEST: insert or replace form
-- +  "INSERT OR REPLACE INTO bar(id, type) VALUES(1, 5)"
insert or replace into bar(id, type) values (1,5);

-- TEST: insert default from
-- +  "INSERT INTO foo DEFAULT VALUES"
insert into foo default values;

-- TEST: insert from stored procedure
-- + function insert_values(_db_, id_, type_)
-- +   "INSERT INTO bar(id, type) VALUES(?, ?)"
-- + ::cql_cleanup::
-- +   cql_finalize_stmt(_temp_stmt)
-- +   return _rc_
-- + end
create proc insert_values(id_ integer not null, type_ integer)
begin
  insert into bar(id, type) values (id_, type_);
end;

-- TEST: alter table add column
-- +   _rc_ = cql_exec(_db_,
-- + "ALTER TABLE bar ADD COLUMN size REAL"
create proc alter_table_test()
begin
  alter table bar add column size real;
end;

-- TEST: drop table
-- + _rc_ = cql_exec(_db_,
-- + "DROP TABLE IF EXISTS bar"
create proc drop_table_test()
begin
  drop table if exists bar;
end;

-- TEST: use a procedure to get a result set
-- + function uses_proc_for_result(_db_)
-- + _rc_, _result_stmt = with_result_set(_db_)
-- + return _rc_, _result_stmt
-- + function uses_proc_for_result_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Islid", { "id", "name", "rate", "type", "size" })
-- + return _rc_, result_set
create procedure uses_proc_for_result()
begin
  call with_result_set();
end;

-- TEST: declare a void func
-- no output
--- - voidfunc
declare function voidfunc() integer;

-- TEST: use a select exists clause
-- + "SELECT EXISTS (SELECT * FROM bar)"
set b2 := (select exists(select * from bar));

-- TEST: for expand of select * columns from whole result
-- + _rc_, expanded_select_stmt = cql_prepare(_db_,
-- + "SELECT id, name, rate, type, size FROM bar"
declare expanded_select cursor for select * from bar;

-- TEST: for expand of select * columns from table
-- + "SELECT bar.id, bar.name, bar.rate, bar.type, bar.size FROM bar"
declare table_expanded_select cursor for select bar.* from bar;

-- TEST: use a long literal
-- + l2 = 3147483647
set l2 := 3147483647L;

-- TEST: use a long literal
-- + l2 = 3147483647
set l2 := 3147483647;

-- TEST: use drop index in a proc
-- + "DROP INDEX index_1"
create proc index_dropper()
begin
  drop index index_1;
end;

-- TEST: simple DML statements for json_schema cg
-- +2 "INSERT INTO foo(id) VALUES(NULL)"
-- + "UPDATE bar SET name = 'bar' WHERE name = 'baz'"
-- + "DELETE FROM foo WHERE id = 1"
create proc misc_dml_proc()
begin
  insert into foo values(NULL);
  insert into foo(id) values(NULL);
  update bar set name = 'bar' where name = 'baz';
  delete from foo where id = 1;
end;

-- TEST: use dummy data
-- + INSERT INTO bar(id, name, rate, type, size) VALUES(_seed_, printf('name_%d', _seed_), _seed_, _seed_, _seed_)
-- + @DUMMY_SEED(123) @DUMMY_DEFAULTS @DUMMY_NULLABLES;
-- + _seed_ = 123
-- + "INSERT INTO bar(id, name, rate, type, size) VALUES(?, printf('name_%d', ?), ?, ?, ?)"
-- + _rc_ = cql_multibind(_db_, _temp_stmt, "IIIII", {_seed_, _seed_, _seed_, _seed_, _seed_})
create proc dummy_user()
begin
  insert into bar () values () @dummy_seed(123) @dummy_nullables @dummy_defaults;
end;

-- TEST: simple out arg proc
-- + foo = "x"
-- +  return foo
create proc proc_with_out_arg(out foo text)
begin
  set foo := 'x';
end;

-- TEST: this stuff is easy in lua because out args are return values
-- + foo = "x"
-- + foo = proc_with_out_arg()
-- + bar = proc_with_out_arg()
-- + return foo
create proc calls_out_proc(out foo text)
begin
  set foo := 'x';
  declare bar text;
  call proc_with_out_arg(foo);
  call proc_with_out_arg(bar);
end;

-- TEST: create blob variable
-- + local blob_var
declare blob_var blob;

-- TEST: create blob variable2
-- + local blob_var2
declare blob_var2 blob not null;

declare function blob_notnull_func() blob not null;

-- initialize for later use
-- + blob_var2 = blob_notnull_func()
set blob_var2 := blob_notnull_func();

-- TEST: assign null to blob variable
-- + blob_var = nil
set blob_var := null;

-- TEST: assign var to blob variable
-- + blob_var = blob_var2
set blob_var := blob_var2;

-- remove the nullability improvement
-- + blob_var = nil
set blob_var := null;

-- TEST: blob comparison "=="
-- + b0_nullable = cql_blob_eq(blob_var, blob_var)
set b0_nullable := blob_var == blob_var;

-- TEST: blob comparison "IS" NULL
-- + b0_nullable = blob_var == nil
set b0_nullable := blob_var IS null;

-- TEST: blob comparison "!="
-- + b0_nullable = cql_blob_ne(blob_var, blob_var)
set b0_nullable := blob_var != blob_var;

-- TEST: blob comparison "<>"
-- + b0_nullable = cql_blob_ne(blob_var, blob_var)
set b0_nullable := blob_var <> blob_var;

-- TEST: blob comparison "IS"
-- + b0_nullable = cql_blob_is_eq(blob_var, blob_var)
set b0_nullable := blob_var IS blob_var;

-- TEST: blob comparison "IS NOT"
-- + b0_nullable = cql_blob_is_ne(blob_var, blob_var)
set b0_nullable := blob_var IS NOT blob_var;

-- TEST: blob variable in IN clause
-- + _tmp_n_blob_1 = blob_var
-- + if cql_blob_eq(_tmp_n_blob_1, blob_var) then break end
-- + if cql_blob_eq(_tmp_n_blob_1, blob_var) then break end
set b0_nullable := blob_var in (blob_var, blob_var);

-- TEST: blob variable in IN clause
-- + _tmp_blob_% = blob_var2
-- + if cql_blob_eq(_tmp_blob_%, blob_var) then break end
-- + if cql_blob_eq(_tmp_blob_%, blob_var2) then break end
set b2 := blob_var2 in (blob_var, blob_var2);

-- TEST: blob variable in NOT IN clause
-- + _tmp_n_blob_% = blob_var
-- + if cql_blob_eq(_tmp_n_blob_%, blob_var) then break end
-- + if cql_blob_eq(_tmp_n_blob_%, blob_var2) then break end
set b0_nullable := blob_var not in (blob_var, blob_var2);

-- TEST: blob variable in NOT IN clause
-- + _tmp_blob_% = blob_var2
-- + if cql_blob_eq(_tmp_blob_%, blob_var) then break end
-- + if cql_blob_eq(_tmp_blob_%, blob_var2) then break end
set b2 := blob_var2 not in (blob_var, blob_var2);

-- TEST: proc with blob args
-- + function blob_proc()
-- + return a_blob
create proc blob_proc(out a_blob blob)
begin
  set a_blob := null;
end;

-- TEST: case statement with blobs
-- +  _tmp_n_blob_1 = blob_var
-- + if _tmp_n_blob_1 == nil then goto case_else_2 end
-- + if _tmp_n_blob_1 == blob_var then
set i2 := case blob_var when blob_var then 1 else 2 end;

-- TEST: case statement with returning blobs
-- + blob_var = blob_var2
-- + blob_var = nil
set blob_var := case 1 when 1 then blob_var2 else null end;

declare function blob_func() blob;

-- TEST: function invocation with blob function
-- + blob_var := blob_func()
set blob_var := blob_func();

declare function blob_func_create() create blob;

-- TEST: function invocation with creater blob function
-- + blob_var = blob_func_create()
set blob_var := blob_func_create();

-- make a table with blobs in it
create table blob_table (
  blob_id integer not null,
  b_notnull blob not null,
  b_nullable blob
);

-- TEST: fetch a nullable blob
-- + "SELECT b_nullable FROM blob_table WHERE blob_id = 1")
-- + blob_var = cql_get_value(_temp_stmt, 0)
set blob_var := (select b_nullable from blob_table where blob_id = 1);

-- TEST: fetch a not null blob
-- + "SELECT b_notnull FROM blob_table WHERE blob_id = 1")
-- + _tmp_blob_% = cql_get_value(_temp_stmt, 0)
-- + blob_var = _tmp_blob_%
set blob_var := (select b_notnull from blob_table where blob_id = 1);

-- some not null blob object we can use
declare blob_var_notnull blob not null;

-- initialize for later use
-- + blob_var_notnull = blob_notnull_func()
set blob_var_notnull := blob_notnull_func();

-- TEST: bind a nullable blob and a not null blob
-- + INSERT INTO blob_table(blob_id, b_nullable, b_notnull) VALUES(0, blob_var, blob_var_notnull);
-- + "INSERT INTO blob_table(blob_id, b_nullable, b_notnull) VALUES(0, ?, ?)"
-- + _rc_ = cql_multibind(_db_, _temp_stmt, "bB", {blob_var, blob_var_notnull})
insert into blob_table(blob_id, b_nullable, b_notnull) values(0, blob_var, blob_var_notnull);

-- TEST: a result set that includes blobs
-- +  "SELECT blob_id, b_notnull, b_nullable FROM blob_table
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "IBb", { "blob_id", "b_notnull", "b_nullable" })
create proc blob_returner()
begin
  select * from blob_table;
end;

-- TEST: forcing null set of object temporary by having no else case
-- +  repeat
-- +    if true then
-- this copy is optimized away!
-- -      obj_var = obj_var
-- +      break
-- +    end
-- +    obj_var = nil
-- +  until true
set obj_var := case when 1 then obj_var end;

-- TEST: force a proc with no arg list
-- + function voidproc()
-- - return
create proc voidproc()
begin
  declare unused int;
end;

-- TEST: create an output struct proc
-- main proc does the select and emits a row
-- + local C_fields_ = { "id", "name", "rate", "type", "size", "extra1", "extra2" }
-- + local C_types_ = "IslidSS"
-- +  "SELECT bar.id, bar.name, bar.rate, bar.type, bar.size, 'xyzzy', 'plugh' FROM bar")
-- +   _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
--
-- OUT C :  remember C could be mutated, so we have to shallow copy it
-- + _result_ = cql_clone_row(C)
-- + return _rc_, _result_
--
-- wrap the one row in a table so it looks like an normal result set
-- + function out_cursor_proc_fetch_results(_db_)
-- + _rc_, _result_ = out_cursor_proc(_db_)
-- + result_set = { _result_ }
-- + return _rc_, result_set
create proc out_cursor_proc()
begin
  declare C cursor for select bar.*, 'xyzzy' extra1, 'plugh' extra2 from bar;
  fetch C;
  out C;
end;

-- TEST: fetch from an output struct proc
-- + _rc_, C = out_cursor_proc(_db_)
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
create proc read_cursor_proc()
begin
  declare C cursor fetch from call out_cursor_proc();
end;

-- TEST: declare a cursor and do a fetch as separate actions
-- + _rc_, C = out_cursor_proc(_db_)
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
create proc declare_cursor_then_fetch_from_proc()
begin
  declare C cursor like out_cursor_proc;
  fetch C from call out_cursor_proc();
end;

-- TEST: proc decl with out args
-- + DECLARE PROC fetcher_proc () OUT (a INTEGER, b TEXT);
declare proc fetcher_proc() out (a integer, b text);

-- TEST: All void all day
-- + DECLARE PROC totally_void_proc ();
-- no output
-- - function totally_void_proc
declare proc totally_void_proc();

-- TEST: call out proc like a function
-- + _rc_, i2 = outparm_test(_db_)
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
set i2 := outparm_test();

declare proc compute(in a_ integer not null, out b_ integer not null);

-- TEST: call out proc like a function, this one has args
-- + _tmp_int_1 = compute(1)
-- + i2 = compute(_tmp_int_1)
set i2 := compute(compute(1));

-- a dml method
declare proc dml_compute(in a_ integer not null, out b_ integer not null) USING TRANSACTION;

-- TEST: call out proc like a function, this one has args and uses the db
-- + _rc_, _tmp_int_1 = dml_compute(_db_, 1)
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + _rc_, i2 = dml_compute(_db_, _tmp_int_1)
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
set i2 := dml_compute(dml_compute(1));

-- TEST: write the result of a proc-as-func call to an out variable
-- + _rc_, a_ = dml_compute(_db_, 1)
create proc dml_user(out a_ integer not null)
begin
  set a_ := dml_compute(1);
end;

-- a test table for the following case
create table threads (
 thread_key long int not null
);

-- TEST: nested subquery in a proc
-- + function thread_theme_info_list(_db_, thread_key_)
-- + "SELECT thread_key FROM (SELECT thread_key FROM threads) AS T")
-- +  if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  return _rc_, _result_stmt
-- + function thread_theme_info_list_fetch_results(_db_, thread_key_)
-- + _rc_, stmt = thread_theme_info_list(_db_, thread_key_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "L", { "thread_key" })
create procedure thread_theme_info_list(thread_key_ LONG INT NOT NULL)
begin
  select * from (select thread_key from threads) T;
end;

-- TEST: value cursor fetch
-- + function fetch_values_dummy()
-- + _seed_ = 123
-- + C._has_row_ = true
-- + C.id = _seed_
-- + _tmp_text_0 = cql_printf("name_%d", _seed_)
-- + C.name = _tmp_text_0
-- + C.rate = _seed_
-- + C.type = _seed_
-- + C.size = cql_to_float(_seed_)
-- - _rc_
-- - cql_cleanup
-- - return
create proc fetch_values_dummy()
begin
  declare C cursor like select * from bar;
  fetch C() from values() @dummy_seed(123) @dummy_nullables;
end;

-- TEST: value cursor fetch, using type syntax
-- this cursor has the fields of bar plus xx and yy
-- + function fetch_values_extended()
-- + _seed_ = 123
-- + C._has_row_ = true
-- + C.id = _seed_
-- + _tmp_text_0 = cql_printf("name_%d", _seed_)
-- + C.name = _tmp_text_0
-- + C.rate = _seed_
-- + C.type = _seed_
-- + C.size = cql_to_float(_seed_)
-- + C.xx = cql_to_float(_seed_)
-- + _tmp_text_1 = cql_printf("yy_%d", _seed_)
-- + C.yy = _tmp_text_1
-- - return
create proc fetch_values_extended()
begin
  declare C cursor like (like bar, xx real, yy text);
  fetch C() from values() @dummy_seed(123) @dummy_nullables;
end;

-- TEST: c style literal
-- + SET x := "\"Testing\" \\''";
create proc c_literal(out x text)
begin
  set x := "\"Testing\" \\''";
end;

-- TEST: no cleanup label needed proc
-- + function no_cleanup_label_needed_proc(_db_)
-- - cql_cleanup
-- + if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto catch_start_2; end
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto catch_start_2; end
-- + goto catch_end_2
-- + ::catch_end_2::
-- + catch_start%:
create proc no_cleanup_label_needed_proc()
begin
  begin try
    declare C cursor for select 1 as N;
    fetch C;
  end try;
  begin catch
    declare x integer;
  end catch;
end;

-- TEST: no code after the last label
-- begin try and begin catch imply dml proc
-- + function no_code_after_catch(_db_)
-- - cql_cleanup
-- + return _rc_
create proc no_code_after_catch()
begin
  begin try
    @attribute(foo) -- just messing with the tree
    declare x integer;
  end try;
  begin catch
    @attribute(bar) -- just messing with the tree
    declare y integer;
  end catch;
end;

-- TEST: void cursor fetcher
-- + function out_no_db_fetch_results()
-- + local C = { _has_row_ = false }
-- + C._has_row_ = true
-- + C.A = 3
-- + C.B = cql_to_float(12)
-- + _result_ = cql_clone_row(C)
-- + return _result_
create proc out_no_db()
begin
  declare C cursor like select 1 A, 2.5 B;
  fetch C(A,B) from values(3,12);
  out C;
end;

-- TEST: declare cursor like cursor
-- + local C0 = { _has_row_ = false }
-- + local C1 = { _has_row_ = false }
-- + C1.A = 3
-- + C1.B = cql_to_float(12)
-- + _result_ = cql_clone_row(C1)
-- + return _result_
create proc declare_cursor_like_cursor()
begin
  declare C0 cursor like select 1 A, 2.5 B;
  declare C1 cursor like C0;
  fetch C1(A,B) from values(3,12);
  out C1;
end;

-- TEST: declare cursor like proc
-- + local C = { _has_row_ = false }
-- + _result_ = cql_clone_row(C)
-- + return _result_
create proc declare_cursor_like_proc()
begin
  declare C cursor like fetcher_proc;
  out C;
end;

-- TEST: declare a cursor like a table
-- + local C = { _has_row_ = false }
-- + _result_ = cql_clone_row(C)
-- + return _result_
create proc declare_cursor_like_table()
begin
  declare C cursor like bar;
  out C;
end;

-- TEST: declare a cursor like a view
-- + local C = { _has_row_ = false }
-- + _result_ = cql_clone_row(C)
-- + return _result_
create proc declare_cursor_like_view()
begin
  declare C cursor like MyView;
  out C;
end;

-- TEST: stress case for quote management
-- the backslash must be preserved in a regular sql string and then escaped
-- the newlines in the c string are turned into newline characters in the SQL string
-- but they have to be escaped due to being embedded in a c string
-- the ones with a leading space are the echoed sql, the strings are not C escaped there
-- so this checks both paths
-- +  DELETE FROM bar WHERE name LIKE "\n\n";
-- + "DELETE FROM bar WHERE name LIKE '\n\n'"
-- +  DELETE FROM bar WHERE name = ' '' \n '' \';
-- + "DELETE FROM bar WHERE name = ' '' \\n '' \\'"
-- +  DELETE FROM bar WHERE name <> "'";
-- + "DELETE FROM bar WHERE name <> ''''"
-- +  DELETE FROM bar WHERE name >= '\';
-- + "DELETE FROM bar WHERE name >= '\\'"
create proc weird_quoting()
begin
  delete from bar where name like "\n\n";
  -- the newline looking thing is NOT an escape sequence it's a pain in the ass...
  delete from bar where name = ' '' \n '' \';
  -- lots of transforms required to get this right
  delete from bar where name != "\'";
  -- another tricky case
  delete from bar where name >= '\';
end;

-- TEST: create a table with a long integer autoinc column
-- this requires the workaround of downgradeing the long to int
-- note: sqlite integers can hold 64 bits so they are already "long"
-- + id LONG_INT PRIMARY KEY AUTOINCREMENT,
-- + "CREATE TABLE long_int_autoinc( id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT )"
create proc long_auto_table_maker()
begin
  create table long_int_autoinc (
    id long primary key autoincrement,
    name text
  );
end;

declare proc blob_out(out x blob);

-- TEST: force blob out var to be null
-- + b = blob_out()
create proc blob_call1()
begin
 declare b blob;
 call blob_out(b);
end;

-- TEST: blob return from func
-- + b = blob_out()
create proc blob_call2()
begin
 declare b blob;
 set b := blob_out(); -- use function call syntax should be the same
end;

-- TEST: forces us to set a blob to null via else.  This is not the store code path
-- + b = b1
-- + b = nil
create proc blob_no_else()
begin
  declare b blob;
  declare b1 blob;
  set b := case b when b then b1 end;
end;

-- TEST: use with-insert form
-- +  _rc_ = cql_exec(_db_,
-- + "WITH x (a) AS (SELECT 111) INSERT INTO foo(id) VALUES(ifnull(( SELECT a FROM x ), 0))"
with x(a) as (select 111)
insert into foo values ( ifnull((select a from x), 0));

-- TEST: use insert from select (put this in a proc to force the schema utils to walk it)
-- + "WITH x (a) AS (SELECT 111) INSERT INTO foo(id) SELECT a FROM x")
create proc with_inserter()
begin
  with x(a) as (select 111)
    insert into foo select * from x;
end;

declare select func SqlUserFunc(id integer) real not null;

-- TEST: invoke a declared user function
-- +  "SELECT SqlUserFunc(123)"
set r2 := (select SqlUserFunc(123));

-- TEST: multiple rewrites complex arg filter
--
-- Note: we're doing something that's legal but not really very useful here just to force the codegen.
-- the out_arg should still be present in the args (and we check it here plus the code is required to compile)
-- and we have to be able to correctly code gen two different like args cases in different locations.
-- It's hard to think of a real use case for this but I want to make sure the rewriter doesn't screw it up.
--
-- + function multi_rewrite(_db_, blob_id_, b_notnull_, b_nullable_, id_, name_, rate_, type_, size_)
-- + cql_contract_argument_notnull(blob_id_, 1)
-- + cql_contract_argument_notnull(b_notnull_, 2)
-- + cql_contract_argument_notnull(id_, 4)
-- + "INSERT INTO blob_table(blob_id, b_notnull, b_nullable) VALUES(?, ?, ?)")
-- + _rc_ = cql_multibind(_db_, _temp_stmt, "IBb", {blob_id_, b_notnull_, b_nullable_})
-- + out_arg = 1
-- + return _rc_, out_arg
create proc multi_rewrite(like blob_table, like bar, out out_arg integer not null)
begin
  insert into blob_table from arguments;
  set out_arg := 1;
end;

-- TEST: fetch to a cursor from another cursor
-- + C0._has_row_ = true
-- + C0.A = 2
-- + C0.B = "bar"
-- + C1._has_row_ = true
-- + C1.A = C0.A
-- + C1.B = C0.B
-- + _result_ = cql_clone_row(C1)
create proc fetch_to_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, "foo" B;
  declare C1 cursor like C0;
  fetch C0 from values (2, "bar");
  fetch C1 from C0;
  out C1;
end;

-- TEST loop statement cursor with autofetch
-- + local C_fields_ = { "A" }
-- + local C_types_ = "I"
-- + _rc_, C_stmt = cql_prepare(_db_,
-- + "SELECT 1")
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + if not C._has_row_ then break end
-- + printf("%d\n", C.A)
create proc loop_statement_cursor()
begin
  declare C cursor for select 1 A;
  loop fetch C
  begin
    call printf("%d\n", C.A);
  end;
end;

-- TEST loop statement cursor with autofetch
-- + local C_fields_ = { "A" }
-- + local C_types_ = "I"
-- + _rc_, C_stmt = cql_prepare(_db_,
-- + "SELECT 1"
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + A_ = C.A
-- + if not C._has_row_ then break end
-- + printf("%d\n", A_)
create proc loop_statement_not_auto_cursor()
begin
  declare C cursor for select 1 A;
  declare A_ integer not null;
  loop fetch C into A_
  begin
    call printf("%d\n", A_);
  end;
end;


@attribute(cql:suppress_result_set)
create proc simple_select()
begin
  select 1 x;
end;

-- TEST: call for cursor in loop
-- one release in cleanup; one in the loop
-- + local C_fields_ = { "x" }
-- + local C_types_ = "I"
-- +2 cql_finalize_stmt(C_stmt)
-- + _rc_, C_stmt = simple_select(_db_)
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
create proc call_in_loop()
begin
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call simple_select();
     fetch C;
  end;
end;

-- TEST: same, but with a nullable condition
-- one release in cleanup; one in the loop
-- +2 cql_finalize_stmt(C_stmt)
-- + local C_fields_ = { "x" }
-- + local C_types_ = "I"
-- + _rc_, C_stmt = simple_select(_db_)
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
create proc call_in_loop_with_nullable_condition()
begin
  declare i int;
  set i := nullable(0);
  while i < 5
  begin
    set i := i + 1;
    declare C cursor for call simple_select();
    fetch C;
  end;
end;

-- TEST: call in loop with boxing
-- + local box
-- + local D_stmt = nil
-- + local D = { _has_row_ = false }
-- + local D_fields_ = { "x" }
-- + local D_types_ = "I"
-- + _rc_, C_stmt = simple_select(_db_)
-- + box = C_stmt
-- due to the loops we do not finalize D as we go on each iteration
-- - cql_finalize_stmt(D_stmt)
-- + D_stmt = box
-- + _rc_ = cql_multifetch(D_stmt, D, D_types_, D_fields_)
create proc call_in_loop_boxed()
begin
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call simple_select();
     declare box object<C cursor>;
     set box from cursor C;
     declare D cursor for box;
     fetch D;
  end;
end;

-- TEST: verify the decl, this is only for later tests
-- + function out_union_helper_fetch_results()
-- - function out_union_helper()
-- + C._has_row_ = true
-- + C.x = 1
-- + table.insert(_rows_, cql_clone_row(C))
-- + return _rows_
create proc out_union_helper()
begin
  declare C cursor like select 1 x;
  fetch C using 1 x;
  out union C;
end;

-- TEST: verify the decl, this is only for later tests
-- + function out_union_dml_helper_fetch_results(_db_)
-- - function out_union_dml_helper(_db_)
-- + local C_fields_ = { "x" }
-- + local C_types_ = "I"
-- + "SELECT 1"
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + table.insert(_rows_, cql_clone_row(C))
-- + return _rc_, _rows_
create proc out_union_dml_helper()
begin
  declare C cursor for select 1 x;
  fetch C;
  out union C;
end;

-- TEST: call out union in a loop
-- + C_result_set_ = out_union_helper_fetch_results()
-- + C_row_num_ = 0
-- + C_row_count_ = #(C_result_set_)
-- + C_row_num_ = C_row_num_ + 1
-- + if C_row_num_ <= C_row_count_ then
-- +   C = C_result_set_[C_row_num_]
-- + else
-- +   C = { _has_row_ = false }
-- + end
create proc call_out_union_in_loop()
begin
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call out_union_helper();
     fetch C;
  end;
end;

-- TEST: here we create a proc that is going to forward the result of out union as its own result
-- - USING TRANSACTION
-- + function forward_out_union_fetch_results()
-- - function forward_out_union()
-- + local _result_set_ = {}
-- + _result_set_ = out_union_helper_fetch_results()
-- + return _result_set_
create proc forward_out_union()
begin
  call out_union_helper();
end;

-- TEST: make an out union helper without defining it
-- - function
declare proc extern_out_union_helper () OUT UNION (x INTEGER NOT NULL);

-- TEST: this should still compile even though the body of the proc isn't here
-- + function forward_out_union_extern_fetch_results()
-- - function forward_out_union_extern()
-- + _result_set_ = extern_out_union_helper_fetch_results()
-- + return _result_set_
create proc forward_out_union_extern()
begin
  call extern_out_union_helper();
end;

-- TEST: forward out union result, with dml proc
-- + function forward_out_union_dml_fetch_results(_db_)
-- - function forward_out_union_dml(_db_)
-- + _rc_, _result_set_ = out_union_dml_helper_fetch_results(_db_)
-- + return _rc_, _result_set_
create proc forward_out_union_dml()
begin
  call out_union_dml_helper();
end;

-- TEST: ensure cursors work outside of a proc
-- + _rc_, global_cursor_stmt = cql_prepare(_db_,
-- + "SELECT 1, 2")
declare global_cursor cursor for select 1 a, 2 b;

-- TEST: fetch from global cursor
-- _rc_ = cql_multifetch(global_cursor_stmt, global_cursor, global_cursor_types_, global_cursor_fields_)
fetch global_cursor;

-- TEST: use like in an expression
-- +  i2 = cql_to_num(cql_like("x", "y"))
set i2 := 'x' LIKE 'y';

-- TEST: use not like in an expression
-- +  i2 = cql_to_num(cql_not_like("x", "y"))
set i2 := 'x' NOT LIKE 'y';

-- TEST: use like in a SQL statement
-- +  "SELECT 'x' LIKE 'y'"
set i2 := (select 'x' LIKE 'y');

-- TEST: use not like in a SQL statement
-- +  "SELECT 'x' NOT LIKE 'y'"
set i2 := (select 'x' NOT LIKE 'y');

-- TEST: use match in a SQL statement
-- +  "SELECT 'x' MATCH 'y'"
set i2 := (select 'x' MATCH 'y');

-- TEST: use glob in a SQL statement
-- +  "SELECT 'x' GLOB 'y'"
set i2 := (select 'x' GLOB 'y');

-- TEST: use lot of bitwise operators
-- NOTE the SQL order of ops is different...
-- no parens used here
-- +  SET i2 := 1 << 2 | 1 << 4 & 1 >> 8;
-- in Sqlite binary math operators all bind equal and left to right so the above is the same as
--    SET i2 :=  (((((1 << 2) | 1) << 4) & 1) >> 8);
-- in LUA that changes because << and >> are stronger than | and &
-- + i2 = ((1 << 2 | 1) << 4 & 1) >> 8
set i2 := 1 << 2 | 1 << 4 & 1 >> 8;

-- TEST: now maybe what you expected to see.  Force the issue with parens
-- + SET i2 := 1 << 2 | (1 << 4) & (1 >> 8);
-- Still not what you expected... remember | and & are EQUAL in sql
-- so the above was parsed left to right...
-- + i2 = (1 << 2 | 1 << 4) & 1 >> 8
set i2 := (1 << 2) | (1 << 4) & (1 >> 8);

-- TEST: this is really the normal thing
-- some parens were redunant, removed...
-- + SET i2 := 1 << 2 | (1 << 4 & (1 >> 8));
-- now this is the usual C order of ops and no parens are in the C
-- + i2 = 1 << 2 | 1 << 4 & 1 >> 8
set i2 := (1 << 2) | ((1 << 4) & (1 >> 8));

-- TEST: force a high binding ~
-- nothing weird here, ~ binds very strong in both languages
-- + i2 = 1 | ~ i2
set i2 := 1 | ~i2;

-- TEST: create a trigger, force the dml
-- + "CREATE TEMP TRIGGER IF NOT EXISTS trigger1 BEFORE DELETE ON bar FOR EACH ROW WHEN old.id > 7 BEGIN SELECT old.id; END"
create proc make_trigger()
begin
  create temp trigger if not exists trigger1
    before delete on bar
    for each row
    when old.id > 7
  begin
    select old.id;
  end;
end;

-- TEST: IS patterns
-- + b = 1 == 1
-- + b = "x" == "x"
-- + b = "x" == "y"
-- + b = cql_to_bool(1 + cql_to_num((3 == 4)))
-- + i = 1
-- + j = 2
-- + b = i == j
create proc is_test()
begin
  declare b bool not null;
  set b := 1 is 1;
  set b := 'x' is 'x';
  set b := 'x' is 'y';
  set b := 1 + (3 is 4);

  let i := nullable(1);
  let j := nullable(2);

  set b := i is j;
end;

-- TEST: blob comparaison
-- + b = cql_blob_is_eq(bl1, bl2)
-- + b = cql_blob_is_ne(bl1, bl2)
create proc is_blob()
begin
  declare bl1 blob;
  declare bl2 blob;
  declare b bool not null;
  set b := bl1 is bl2;
  set b := bl1 is not bl2;
end;

-- TEST: IS NOT patterns
-- + b = 1 ~= 1
-- + b = "x" ~= "x"
-- + b = "x" ~= "y"
-- + b = cql_to_bool(1 + cql_to_num((3 ~= 4)))
-- + i = 1
-- + j = 2
-- + b = i ~= j
create proc is_not_test()
begin
  declare b bool not null;
  set b := 1 is not 1;
  set b := 'x' is not 'x';
  set b := 'x' is not 'y';
  set b := 1 + (3 is not 4);

  let i := nullable(1);
  let j := nullable(2);

  set b := i is not j;
end;

-- TEST: null on lhs of IN
-- + b = nil
-- + return b
create proc in_test(x integer, out b bool)
begin
  set b := NULL IN (1);
end;

-- TEST: null on lhs of NOT IN
-- + b = nil
-- + return b
create proc not_in_test(x integer, out b bool)
begin
  set b := NULL NOT IN (1);
end;

-- TEST: drop a trigger (both flavors)
-- +1 "DROP TRIGGER IF EXISTS trigger1"
-- +1 "DROP TRIGGER trigger1"
create proc drop_trigger_test()
begin
  drop trigger if exists trigger1;
  drop trigger trigger1;
end;

-- TEST: create proc with a single-column identity attribute
-- + function simple_identity(_db_)
-- + _rc_, _result_stmt = cql_prepare(_db_,
-- +    "SELECT 1, 2")
-- + function simple_identity_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "II", { "id", "data" })
-- TODO, idendity was only interesting for partial compare on the rowset
-- this really doesn't make much sense in the LUA world but some thinking
-- could be needed here.
@attribute(cql:identity=(id))
create proc simple_identity()
begin
  select 1 as id, 2 as data;
end;

-- TEST: create proc with a multi-column identity attribute
-- + function complex_identity(_db_)
-- + "SELECT 1, 2, 3")
-- + function complex_identity_fetch_results(_db_)
-- +  _rc_, stmt = complex_identity(_db_)
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "III", { "col1", "col2", "data" })
@attribute(cql:identity=(col1, col2))
create proc complex_identity()
begin
  select 1 as col1, 2 as col2, 3 as data;
end;

-- TEST: create proc with a out cursor and identity column
-- + function out_cursor_identity(_db_)
-- + local C_fields_ = { "id", "data" }
-- + local C_types_ = "II"
-- + "SELECT 1, 2")
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- + function out_cursor_identity_fetch_results(_db_)
-- + result_set = { _result_ }
@attribute(cql:identity=(id))
create proc out_cursor_identity()
begin
  declare C cursor for select 1 as id, 2 as data;
  fetch C;
  out C;
end;

create table radioactive(
  id integer not null,
  data text @sensitive
);

-- TEST: create a proc that reducts some sensitive data
-- + function radioactive_proc(_db_)
-- + "SELECT id, data FROM radioactive")
-- + function radioactive_proc_fetch_results(_db_)
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "Is", { "id", "data" })
-- TODO vault_sensitive is not yet supported, we'll have to figure out some kind of plan for what this means
@attribute(cql:vault_sensitive)
create proc radioactive_proc()
begin
  select * from radioactive;
end;

-- TEST: with delete form
-- +  "WITH x (a) AS (SELECT 111) DELETE FROM foo WHERE id IN (SELECT a FROM x)")
create proc with_deleter()
begin
  with x(a) as (select 111)
    delete from foo where id in (select * from x);
end;

-- TEST: with update form
-- +  "WITH x (a) AS (SELECT 111) UPDATE bar SET name = 'xyzzy' WHERE id IN (SELECT a FROM x)"
create proc with_updater()
begin
  with x(a) as (select 111)
    update bar set name = 'xyzzy' where id in (select * from x);
end;

create temp table table1( id integer);
create temp table table2( id integer);

-- TEST: autodrop attribute
-- + function autodropper(_db_)
-- TODO -- this attribute is not yet supported but it's also really unpopular...
@attribute(cql:autodrop=(table1, table2))
create proc autodropper()
begin
   select 1 a, 2 b;
end;

-- TEST: base fragment attribute
-- there should be no proc codegen
-- - function base_fragment
@attribute(cql:base_fragment=core)
create proc base_fragment(id_ integer not null)
begin
with
  core(x,y,z) as (select id,name,rate from bar where id = id_)
select * from core;
end;

-- TEST: make sure that the name of the cursor is canonicalized
-- There should be no references to the version with the wrong case
-- + function simple_cursor_proc()
-- + local A_CURSOR = { _has_row_ = false }
-- + A_CURSOR.id = 1
-- + _result_ = cql_clone_row(A_CURSOR)
-- + return _result_
-- + function simple_cursor_proc_fetch_results()
-- +  _result_ = simple_cursor_proc()
-- + result_set = { _result_ }
create procedure simple_cursor_proc()
begin
  declare A_CURSOR cursor like select 1 id;
  fetch a_cursor (id) from values(1);
  out a_cursor;
end;

-- TEST: force codegen to include (and ignore) the enforcement directives
-- these have no output so there's nothing to verify really
-- we just verify that we did not echo the comment for these
-- thereby causing the need for the global proc for no reason
-- - @enforce
@enforce_strict foreign key on update;
@enforce_normal foreign key on delete;

-- TEST: force codegen to include (and ignore) the schema region directives
-- these have no output so there's nothing to verify really
-- we just verify that we did not echo the comment for these
-- thereby causing the need for the global proc for no reason
-- - @declare
-- - @begin
-- - @end
-- - schema
-- - region
@declare_schema_region root_region;
@begin_schema_region root_region;
@end_schema_region;

-- this section has trivial casts, we still need to test codegen for this
-- because normal mode is still legal
@enforce_normal cast;

-- TEST: select with redundant cast and alias
-- + "SELECT (5), T.xyzzy FROM (SELECT 1 AS xyzzy) AS T"
create proc redundant_cast()
begin
  select CAST(5 as integer) plugh, T.xyzzy five from (select 1 xyzzy) as T;
end;

-- TEST: select with alias in view
-- + "CREATE VIEW alias_preserved AS SELECT (5) AS plugh, T.xyzzy AS five FROM (SELECT 1 AS xyzzy) AS T"
create proc view_creator()
begin
  create view alias_preserved as
    select CAST(5 as integer) plugh, T.xyzzy five from (select 1 xyzzy) as T;
end;

@enforce_strict cast;

create table switch_account_badges(badge_count integer);
create table unread_pending_threads(unread_pending_thread_count integer);

-- TEST: nested select table should not have column aliases removed
-- + "SELECT SUM(A.unread_pending_thread_count), SUM(A.switch_account_badge_count)
-- + FROM (SELECT P.unread_pending_thread_count AS unread_pending_thread_count, 0 AS switch_account_badge_count
-- + FROM unread_pending_threads AS P
-- + UNION ALL
-- + SELECT 0 AS unread_pending_thread_count, S.badge_count AS switch_account_badge_count
-- + FROM switch_account_badges AS S) AS A"
CREATE PROC settings_info ()
BEGIN
  declare C cursor for
    SELECT SUM(A.unread_pending_thread_count) AS unread_pending_thread_count,
         SUM(A.switch_account_badge_count) AS switch_account_badge_count
    FROM (SELECT P.unread_pending_thread_count AS unread_pending_thread_count, 0 AS switch_account_badge_count
    FROM unread_pending_threads AS P
  UNION ALL
  SELECT 0 AS unread_pending_thread_count, S.badge_count AS switch_account_badge_count
    FROM switch_account_badges AS S) AS A;
END;

-- TEST: aliases in top-level selects can be removed if not referenced
-- + "SELECT 1, 2 UNION ALL SELECT foo.id, 2 FROM foo"
CREATE PROC top_level_select_alias_unused()
BEGIN
  SELECT 1 AS id, 2 as x
  UNION ALL
  SELECT foo.id, 2 as x
  FROM foo;
END;

-- TEST: aliases in top-level selects must not be removed if referenced from an
-- order by clause
-- + "SELECT 1 AS id, 2 UNION ALL SELECT foo.id, 2 FROM foo ORDER BY id"
CREATE PROC top_level_select_alias_used_in_orderby()
BEGIN
  SELECT 1 AS id, 2 as x
  UNION ALL
  SELECT foo.id, 2 as x
  FROM foo
  ORDER BY id;
END;

-- TEST: try to use a WITH_SELECT form in a select expression
-- + "WITH threads2 (count) AS (SELECT 1) SELECT COUNT(*) FROM threads2"
-- + _tmp_int_0 = cql_get_value(_temp_stmt, 0)
-- + x = _tmp_int_0
create proc use_with_select()
begin
   declare x integer;
   SET x := (WITH threads2 (count) AS (SELECT 1 foo) SELECT COUNT(*) FROM threads2);
end;

-- declare a simple table-valued function
declare select function ReadFromRowset(rowset Object<rowset>) (id integer);

-- TEST: use a table valued function that consumes an object
-- + function rowset_object_reader(_db_, rowset)
-- + "SELECT id FROM ReadFromRowset(?)")
-- + _rc_ = cql_multibind(_db_, C_stmt, "o", {rowset})
create proc rowset_object_reader(rowset Object<rowset>)
begin
  declare C cursor for select * from ReadFromRowset(rowset);
end;

-- TEST: codegen upsert statement with update statement
-- + function upsert_do_something(_db_)
-- + "INSERT INTO foo(id) SELECT id FROM bar WHERE 1
-- + ON CONFLICT (id) DO UPDATE SET id = 10 WHERE id <> 10"
create proc upsert_do_something()
BEGIN
 insert into foo select id from bar where 1 on conflict(id) do update set id=10 where id != 10;
END;

-- TEST: codegen with upsert statement form
-- + function with_upsert_form(_db_)
-- + "WITH names (id) AS (VALUES(1), (5), (3), (12)) INSERT INTO foo(id)
-- + SELECT id FROM names WHERE 1 ON CONFLICT (id) DO UPDATE SET id = 10 WHERE id <> 10"
create proc with_upsert_form()
BEGIN
 with names(id) as (values (1), (5), (3), (12))
 insert into foo select id from names where 1 on conflict(id) do update set id = 10 where id != 10;
END;

-- TEST: codegen upsert statement with do nothing
-- + function upsert_do_nothing(_db_, id_)
-- + "INSERT INTO foo(id) VALUES(?) ON CONFLICT DO NOTHING"
create proc upsert_do_nothing(id_ integer not null)
BEGIN
 insert into foo(id) values(id_) on conflict do nothing;
END;

-- TEST: codegen with-insert with a seed
-- + _seed_ = 1337
-- + "WITH some_cte (id) AS (SELECT 1 AS id) INSERT INTO bar(id) VALUES(ifnull(( SELECT id FROM some_cte ), 0))"
with some_cte(id) as (select 1 id)
insert into bar(id)
values (ifnull((select id from some_cte), 0))
@dummy_seed(1337);

-- TEST: codegen upsert with a seed
-- + _seed_ = 1338
-- + "INSERT INTO bar(id) VALUES(1) ON CONFLICT (id) DO UPDATE SET id = 10"
insert into bar(id) values(1) @dummy_seed(1338)
on conflict(id) do
update set id=10;

-- TEST: set up a couple of out cursor procs (body not needed)
declare procedure p1() out (id integer not null, t text);
declare procedure p2() out (id integer not null, t text) using transaction;

-- TEST: this test forces several out cursors to go into the symbol table
-- the idea is that it reveals any cases where a temporary pointer is
-- stored into the symbol table as was the case with the temporary
-- row data for each cursor.  The test is this:  is c2 properly emitted?
-- +1 local c1 = { _has_row_ = false }
-- +1 local c2 = { _has_row_ = false }
-- +1 c1 = p1()
-- +1 _rc_, c2 = p2(_db_)
create procedure use_many_out_cursors()
begin
  declare c1 cursor fetch from call p1();
  declare c2 cursor fetch from call p2();
end;

-- TEST: each fetch forces the declaration of the cursor storage if it has
-- not already been declared.  In this case the third branch of the if
-- must find that p1 and p2 row data are already declared and not duplicate
-- the declarations.
-- + function fetch_many_times(_db_, arg)
-- +1 local C = { _has_row_ = false }
-- +2 C = p1()
-- +2 _rc_, C = p2(_db_)
create procedure fetch_many_times(arg bool not null)
begin
  declare C cursor like p1;
  if arg  == 1 then
    fetch C from call p1();
  else if arg == 2 then
    fetch C from call p2();
  else
    fetch C from call p1();
    fetch C from call p2();
  end if;
end;

-- TEST: create a result set from rows values
-- + function out_union_two_fetch_results()
-- + local _rows_ = {}
-- + C._has_row_ = true
-- + C.x = 1
-- + C.y = "y"
-- +2 table.insert(_rows_, cql_clone_row(C))
-- + return _rows_
create proc out_union_two()
begin
 declare C cursor like select 1 x, '2' y;
 fetch C from values(1, "y");
 out union C;
 out union C;
end;

-- TEST: read back the two rows from the above
-- + function out_union_reader(_db_)
-- + c_result_set_ = out_union_two_fetch_results()
-- + c_row_num_ = 0
-- + c_row_count_ = #(c_result_set_)
-- + c = c_result_set_[c_row_num_]
-- + c = { _has_row_ = false }
create proc out_union_reader()
begin
  declare c cursor for call out_union_two();
  loop fetch C
  begin
    call printf("%d %s\n", C.x, C.y);
  end;
end;

-- TEST: create a result set from selected rows
-- + function out_union_from_select_fetch_results(_db_)
-- + local _rows_ = {}
-- + local C_fields_ = { "x", "y" }
-- + local C_types_ = "IS"
-- + "SELECT 1, '2'"
-- +1 _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- +2 table.insert(_rows_, cql_clone_row(C))
-- + return _rc_, _rows_
create proc out_union_from_select()
begin
 declare C cursor for select 1 x, '2' y;
 fetch C;
 out union C;
 out union C;
end;

-- TEST: reading from out union again, this time a DML proc (uses select)
-- slightly different call path
-- + c_row_num_ = 0
-- + c_row_count_ = #(c_result_set_)
-- + _rc_, c_result_set_ = out_union_from_select_fetch_results(_db_)
create proc out_union_dml_reader()
begin
  declare c cursor for call out_union_from_select();
  loop fetch C
  begin
    call printf("%d %s\n", C.x, C.y);
  end;
end;

-- This just sets up a call to a procedure that takes two integers
create proc out_union_values(a integer not null, b integer not null)
begin
  declare x cursor like select 1 x, 2 y;
  fetch x from values(a,b);
  out union x;
end;

-- TEST:  we need to be able to call the above proc
-- + function read_out_union_values(_db_, a, b)
-- + cql_contract_argument_notnull(a, 1)
-- + cql_contract_argument_notnull(b, 2)
-- + C_result_set_ = out_union_values_fetch_results(a, b)
-- + C_row_num_ = 0
-- + C_row_count_ = #(C_result_set_)
-- + C_row_num_ = C_row_num_ + 1
-- + if C_row_num_ <= C_row_count_ then
-- +  C = C_result_set_[C_row_num_]
-- + else
-- +  C = { _has_row_ = false }
create proc read_out_union_values(a integer not null, b integer not null)
begin
  declare C cursor for call out_union_values(a,b);
  fetch C;
end;

-- This just sets up a call to a procedure that proceduce a dml out union result set
-- + function out_union_dml_fetch_results(_db_)
-- + local _rows_ = {}
-- + local x_fields_ = { "id", "data" }
-- + local x_types_ = "Is"
-- + "SELECT id, data FROM radioactive")
-- + _rc_ = cql_multifetch(x_stmt, x, x_types_, x_fields_)
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + table.insert(_rows_, cql_clone_row(x))
-- + return _rc_, _rows_
-- TODO whatever vault sensitive means here needs to be explored
@attribute(cql:vault_sensitive)
create proc out_union_dml()
begin
  declare x cursor for select * from radioactive;
  fetch x;
  out union x;
end;

-- TEST: we need to make sure t
-- + _rc_, C_result_set_ = out_union_dml_fetch_results(_db_)
-- + C_row_count_ = #(C_result_set_)
-- + C = C_result_set_[C_row_num_]
-- TODO whatever vault sensitive means here needs to be explored
@attribute(cql:vault_sensitive)
create proc out_union_dml_for_call()
begin
  declare C cursor for call out_union_dml();
  fetch C;
end;

-- TEST: generate a compound select statement in an expression (this is a legal form)
-- + "SELECT 1 WHERE 0 UNION SELECT 2 LIMIT 1"
create proc compound_select_expr()
begin
  declare x integer;
  set x := (select 1 where 0 union select 2 limit 1);
end;

-- TEST: generate window function invocation
-- + "SELECT id,  row_number() OVER () FROM foo"
create proc window_function_invocation()
begin
  select id, row_number() over () as row_num from foo;
end;

-- TEST: update some of the cursor columns
-- + if C._has_row_ then
-- +   C.x = 2
-- + end
create proc update_cursor()
begin
  declare C cursor like select 1 x, 2 y;
  update cursor C(x) from values (2);
end;

-- TEST: make sure decl output is correct for DML out union
-- + DECLARE PROC out_union_with_dml (id INTEGER) OUT UNION (id INTEGER NOT NULL) USING TRANSACTION;
declare proc out_union_with_dml(id integer) out union (id integer not null) using transaction;

-- TEST: make sure decl output is correct for non-DML out union
-- + DECLARE PROC out_union_no_dml (id INTEGER) OUT UNION (id INTEGER NOT NULL);
declare proc out_union_no_dml(id integer) out union (id integer not null);

-- TEST: emit goto cql_cleanup in case of return
-- + goto cql_cleanup
-- + ::cql_cleanup::
create proc use_return()
begin
  begin try
    select 1 x;
  end try;
  begin catch
    return;
  end catch;
end;

-- TEST: emit goto cql_cleanup in case of return, force the label even if not
-- used for any other error processing
-- + goto cql_cleanup
-- + ::cql_cleanup::
create proc use_return_no_error_flow()
begin
  if 1 then
    return;
  end if;
end;

-- TEST: empty proc body
-- + CREATE PROC empty_proc ()
-- + BEGIN
-- + END;
-- + function empty_proc()
-- - ::cql_cleanup::
create proc empty_proc()
begin
end;

-- TEST: empty body parts, all statement list types
-- nothing really to validate here; if any of the empty cases
-- are not handled it will crash.  If the blocks are badly shaped
-- it won't compile. Can't think of anything that isn't redundant here
-- + CREATE PROC empty_blocks ()
create proc empty_blocks()
begin
  if 1 then
  end if;

  if 2 then
  else
  end if;

  if 3 then
  else if 4
  then
  else
  end if;

  while 1
  begin
  end;

  declare c cursor for select 1 x;
  loop fetch c
  begin
  end;

  begin try
  end try;
  begin catch
  end catch;
end;

-- This proc illustrates empty catch blocks correctly emitting labels
-- with no intervening statements.  C insists at least one statement
-- after a label but LUA doesn't care.  The real test here is to
-- ensure the generated code compiles, lua will puke if we make it
-- badly formed and the test forces a compile.
-- + function tail_catch(_db_)
-- +2 -- try
-- +2 goto catch_end_%
-- +2 ::catch_end_%::
create proc tail_catch()
begin
   begin try
   end try;
   begin catch
     begin try
     end try;
     begin catch
     end catch;
   end catch;
end;

-- TEST: the SQL output will include an escaped quote ''
-- this used to fool us into thinking we had left quoted mode
-- and so later characters would not be correctly escaped in the output
-- in particular the newline would get messed up because we thought
-- it was a line break in non-quoted SQL which can be replaced with a space
-- note the newline is escaped and present
-- + "INSERT INTO bar(id, name) VALUES(1, 'it''s high noon\r\n\f\b\t\v')"
create proc pretty_print_with_quote()
begin
  insert into bar(id, name) values(1, "it's high noon\r\n\f\b\t\v");
end;

-- TEST: string literal with hex forms
-- + "INSERT INTO bar(id, name) VALUES(1, '\x01\x02\xa1\x1bg')"
create proc hex_quote()
begin
  insert into bar(id, name) values(1, "\x01\x02\xA1\x1b\x00\xg");
end;

-- TEST: no getters generated for this function
-- lua has no getters but we can verify that there are no errors for using the form
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Islid", { "id", "name", "rate", "type", "size" })
@attribute(cql:suppress_getters)
create proc lotsa_columns_no_getters()
begin
  select * from bar;
end;

-- TEST: a copy function will be generated
-- LUA does not have or need a copy function, verify it is ignored
-- + function sproc_with_copy(_db_)
-- + "SELECT id, name, rate, type, size FROM bar"
-- + function sproc_with_copy_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Islid", { "id", "name", "rate", "type", "size" })
@attribute(cql:generate_copy)
create proc sproc_with_copy()
begin
  select * from bar;
end;

-- TEST: emit an object result set with setters with not null values
-- LUA has no setters, so we just verify that the attribute is ignored
------
-- LUA, show us your setters!  WE HAVE NO SETTERS!
------
-- + function emit_object_with_setters(o, x, i, l, b, d, t, bl)
-- + cql_contract_argument_notnull(o, 1)
-- + cql_contract_argument_notnull(x, 2)
-- + cql_contract_argument_notnull(i, 3)
-- + cql_contract_argument_notnull(l, 4)
-- + cql_contract_argument_notnull(b, 5)
-- + cql_contract_argument_notnull(d, 6)
-- + cql_contract_argument_notnull(t, 7)
-- + cql_contract_argument_notnull(bl, 8)
-- + C.o = o
-- + C.x = x
-- + C.i = i
-- + C.l = l
-- + C.b = b
-- + C.d = d
-- + C.t = t
-- + C.bl = bl
-- + _result_ = cql_clone_row(C)
-- +function emit_object_with_setters_fetch_results(o, x, i, l, b, d, t, bl)
-- +  _result_ = emit_object_with_setters(o, x, i, l, b, d, t, bl)
-- +  result_set = { _result_ }
@attribute(cql:emit_setters)
create proc emit_object_with_setters(
  o object not null,
  x object not null,
  i integer not null,
  l long not null,
  b bool not null,
  d real not null,
  t text not null,
  bl blob not null)
begin
  declare C cursor like emit_object_with_setters arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: emit an object result set with setters with nullable values
-- LUA has no setters, so we just verify that the attribute is ignored
-- + function emit_setters_with_nullables(o, x, i, l, b, d, t, bl)
-- +  C.o = o
-- +  C.x = x
-- +  C.i = i
-- +  C.l = l
-- +  C.b = b
-- +  C.d = d
-- +  C.t = t
-- +  C.bl = bl
-- +  _result_ = cql_clone_row(C)
-- + function emit_setters_with_nullables_fetch_results(o, x, i, l, b, d, t, bl)
-- + _result_ = emit_setters_with_nullables(o, x, i, l, b, d, t, bl)
-- + result_set = { _result_ }
@attribute(cql:emit_setters)
create proc emit_setters_with_nullables(
  o object,
  x object,
  i integer,
  l long,
  b bool,
  d real,
  t text,
  bl blob)
begin
  declare C cursor like emit_setters_with_nullables arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: emit an object result set not out and setters
-- lua has no getters or setters, not needed, we just verify that this is ignored
-- + function no_out_with_setters(_db_)
-- + "SELECT id, name, rate, type, size FROM bar"
-- + return _rc_, _result_stmt
-- + function no_out_with_setters_fetch_results(_db_)
-- + _rc_, stmt = no_out_with_setters(_db_)
@attribute(cql:emit_setters)
create proc no_out_with_setters()
begin
  select * from bar;
end;

-- TEST: no result set items should be generated at all
-- + function lotsa_columns_no_result_set(_db_)
-- + "SELECT id, name, rate, type, size FROM bar")
-- + return _rc_, _result_stmt
-- - function lotsa_columns_no_result_set_fetch_results(_db_)
@attribute(cql:suppress_result_set)
create proc lotsa_columns_no_result_set()
begin
  select * from bar;
end;

-- TEST: make sure that _rc_ is set to SQLITE_OK when we return
-- note that the match strings includes comments that are in the output
-- the comments in the output help to find a goto that is a return
-- + function early_out_rc_cleared(_db_)
-- + _rc_ = CQL_OK -- clean up any CQL_ROW value or other non-error
-- + goto cql_cleanup -- return
create proc early_out_rc_cleared(out x integer)
begin
  declare C cursor for select 1 x;
  fetch C;
  if C then
    return;
  end if;
end;

-- TEST: helper table
create table vault_mixed_sensitive(
  id int not null primary key,
  name text @sensitive,
  title text,
  type long @sensitive
);

create table vault_mixed_not_nullable_sensitive(
  id int not null primary key,
  name text not null @sensitive,
  title text not null,
  type long not null @sensitive
);

-- TEST: helper table
create table vault_non_sensitive(
  id int not null primary key,
  name text,
  title text,
  type long
);

-- TEST: vault_sensitive attribute includes sensitive column (name) and non sensitive column (id)
-- TODO figure out what vaulting means to lua if anything
-- + function vault_sensitive_with_values_proc(_db_)
-- +    "SELECT id, name, title, type FROM vault_mixed_sensitive"
-- +  return _rc_, _result_stmt
-- + function vault_sensitive_with_values_proc_fetch_results(_db_)
-- +  _rc_, stmt = vault_sensitive_with_values_proc(_db_)
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
-- +  return _rc_, result_set
@attribute(cql:vault_sensitive=(id, name))
@attribute(cql:custom_type_for_encoded_column)
create proc vault_sensitive_with_values_proc()
begin
  select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes sensitive column (name) and non sensitive column (id)
-- TODO figure out what vaulting means to lua if anything
-- +function vault_not_nullable_sensitive_with_values_proc(_db_)
-- +    "SELECT id, name, title, type FROM vault_mixed_not_nullable_sensitive")
-- +  return _rc_, _result_stmt
-- +function vault_not_nullable_sensitive_with_values_proc_fetch_results(_db_)
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
-- +  return _rc_, result_set
@attribute(cql:vault_sensitive=(id, name))
@attribute(cql:custom_type_for_encoded_column)
create proc vault_not_nullable_sensitive_with_values_proc()
begin
  select * from vault_mixed_not_nullable_sensitive;
end;

-- TEST: vault_sensitive attribute includes sensitive column (data) and non sensitive column (id)
-- TODO figure out what vaulting means to lua if anything
-- +function vault_sensitive_mixed_proc(_db_)
-- +    "SELECT id, name, title, type FROM vault_mixed_sensitive")
-- +  return _rc_, _result_stmt
-- + function vault_sensitive_mixed_proc_fetch_results(_db_)
-- +  _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
-- +  return _rc_, result_set
@attribute(cql:vault_sensitive)
create proc vault_sensitive_mixed_proc()
begin
  select * from vault_mixed_sensitive;
end;

-- TEST: vault union all a sensitive and non sensitive table
-- TODO figure out what vaulting means to lua if anything
-- + function vault_union_all_table_proc(_db_)
-- + "SELECT id, name, title, type FROM vault_mixed_sensitive UNION ALL SELECT id, name, title, type FROM vault_non_sensitive")
-- + return _rc_, _result_stmt
-- + function vault_union_all_table_proc_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
-- + return _rc_, result_set
@attribute(cql:vault_sensitive)
create proc vault_union_all_table_proc()
begin
  select * from vault_mixed_sensitive
  union all
  select * from vault_non_sensitive;
end;

-- TEST: vault on alias column name
-- TODO figure out what vaulting means to lua if anything
-- + function vault_alias_column_proc(_db_)
-- + "SELECT name FROM vault_mixed_sensitive"
-- + return _rc_, _result_stmt
-- + function vault_alias_column_proc_fetch_results(_db_)
-- + _rc_, stmt = vault_alias_column_proc(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "s", { "alias_name" })
-- + return _rc_, result_set
@attribute(cql:vault_sensitive=alias_name)
create proc vault_alias_column_proc()
begin
  select name as alias_name from vault_mixed_sensitive;
end;

-- TEST: vault on alias column name
-- TODO figure out what vaulting means to lua if anything
-- + function vault_alias_column_name_proc(_db_)
-- + "SELECT name FROM vault_mixed_sensitive")
-- + function vault_alias_column_name_proc_fetch_results(_db_)
-- + _rc_, stmt = vault_alias_column_name_proc(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "s", { "alias_name" })
-- + return _rc_, result_set
@attribute(cql:vault_sensitive=alias_name)
create proc vault_alias_column_name_proc()
begin
  select name as alias_name from vault_mixed_sensitive;
end;

-- TEST: vault a column in cursor result
-- TODO figure out what vaulting means to lua if anything
-- + function vault_cursor_proc(_db_)
-- + local C_fields_ = { "name" }
-- + local C_types_ = "s"
-- + "SELECT name FROM vault_mixed_sensitive"
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- - fetch_results
@attribute(cql:vault_sensitive)
create proc vault_cursor_proc()
begin
  declare C cursor for select name from vault_mixed_sensitive;
  fetch c;
end;

-- TEST: vault_sensitive attribute includes encode context column (title) and sensitive column (id, name)
-- TODO figure out what vaulting means to lua if anything
-- + function vault_sensitive_with_context_and_sensitive_columns_proc(_db_)
-- + "SELECT id, name, title, type FROM vault_mixed_sensitive")
-- + function vault_sensitive_with_context_and_sensitive_columns_proc_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
@attribute(cql:vault_sensitive=(title, (id, name)))
create proc vault_sensitive_with_context_and_sensitive_columns_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes no encode context column and sensitive column (id, name)
-- TODO figure out what vaulting means to lua if anything
-- + function vault_sensitive_with_no_context_and_sensitive_columns_proc(_db_)
-- + "SELECT id, name, title, type FROM vault_mixed_sensitive"
-- + function vault_sensitive_with_no_context_and_sensitive_columns_proc_fetch_results(_db_)
-- + _rc_, stmt = vault_sensitive_with_no_context_and_sensitive_columns_proc(_db_)
@attribute(cql:vault_sensitive=((id, name)))
create proc vault_sensitive_with_no_context_and_sensitive_columns_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes encode context column (title) and no sensitive column
-- TODO figure out what vaulting means to lua if anything
-- + function vault_sensitive_with_context_and_no_sensitive_columns_proc_fetch_results(_db_)
-- + _rc_, result_set = cql_fetch_all_rows(stmt, "Issl", { "id", "name", "title", "type" })
@attribute(cql:vault_sensitive=(title, (id, name)))
create proc vault_sensitive_with_context_and_no_sensitive_columns_proc()
begin
 select * from vault_non_sensitive;
end;

create table ext_test_table (
  f1 integer not null,
  f2 integer not null @sensitive,
  f3 integer not null
);

-- TEST: define a base fragment, no output for this
-- - function baseline
@attribute(cql:base_fragment=frag_test)
create proc baseline()
begin
  with
    frag_test(*) as (select 1 id)
  select * from frag_test;
end;


-- TEST: extension creates no getters
-- - ext_get_id
-- - get_f2_is_null
-- - get_f2_value
-- - result_count
@attribute(cql:extension_fragment=frag_test)
@attribute(cql:vault_sensitive)
create proc ext()
begin
  with
    frag_test(*) as (select 1 id),
    ext(*) as (select frag_test.*, f2 from frag_test left outer join ext_test_table on f1 = id)
  select * from ext;
end;

-- TEST: another extension, this one should not include anything about f2, it doesn't "know" about that column
-- - f2
@attribute(cql:extension_fragment=frag_test)
create proc ext2()
begin
  with
    frag_test(*) as (select 1 id),
    ext2(*) as (select frag_test.*, f3 from frag_test left outer join ext_test_table on f1 = id)
  select * from ext2;
end;

-- TEST: simple box operation
-- + function try_boxing(_db_)
-- + _rc_, C_stmt = cql_prepare(_db_,
-- + "SELECT id, name, rate, type, size FROM bar"
-- + result = C_stmt
-- + return _rc_, result
-- boxed object controlsl lifetime now
-- - finalize
create proc try_boxing(out result object<bar cursor>)
begin
  declare C cursor for select * from bar;
  set result from cursor C;
end;

-- TEST: simple unbox
-- + function try_unboxing(_db_, boxed_cursor)
-- + local C_fields_ = { "id", "name", "rate", "type", "size" }
-- + local C_types_ = "Islid"
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
-- boxing controls lifetime
-- - finalize
create proc try_unboxing(boxed_cursor object<bar cursor>)
begin
  declare C cursor for boxed_cursor;
  fetch C;
end;

-- TEST: numeric cast operation int32
-- + x = cql_to_integer(3.2)
create proc local_cast_int_notnull()
begin
  declare x integer not null;
  set x := cast(3.2 as integer);
end;

-- TEST: numeric cast operation int32 nullable
-- + r = 3.2
-- + x = cql_to_integer(r)
create proc local_cast_int()
begin
  declare x integer;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as integer);
end;

-- TEST: numeric cast operation int64 nullable
-- + x = cql_to_integer(3.2)
create proc local_cast_long_notnull()
begin
  declare x long not null;
  set x := cast(3.2 as long);
end;

-- TEST: numeric cast operation int64 nullable
-- + r = 3.2
-- + x = cql_to_integer(r)
create proc local_cast_long()
begin
  declare x long;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as long);
end;

-- TEST: numeric cast operation real
-- + x = cql_to_float(3)
create proc local_cast_real_notnull()
begin
  declare x real not null;
  set x := cast(3 as real);
end;

-- TEST: numeric cast operation real nullable
-- + r = 3
-- + x = cql_to_float(r)
create proc local_cast_real()
begin
  declare x real;
  declare r int;
  set r := nullable(3);
  set x := cast(r as real);
end;

-- TEST: numeric cast operation bool (and normalize)
-- + x = cql_to_bool(3.2)
create proc local_cast_bool_notnull()
begin
  declare x bool not null;
  set x := cast(3.2 as bool);
end;

-- TEST: numeric cast operation bool nullable (and normalize)
-- + r = 3.2
-- + x = cql_to_bool(r)
create proc local_cast_bool()
begin
  declare x bool;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as bool);
end;

-- TEST: numeric cast operation from bool (normalize b)
-- + b = true
-- + x = cql_to_float(b)
create proc local_cast_from_bool_notnull()
begin
  declare b bool not null;
  set b := 1;
  declare x real not null;
  set x := cast(b as real);
end;

-- TEST: numeric cast operation from bool nullable (normalize b)
-- + b = true
-- + x = cql_to_float(b)
create proc local_cast_from_bool()
begin
  declare b bool;
  set b := nullable(1);
  declare x real;
  set x := cast(b as real);
end;

-- this section has trivial casts, we still need to test codegen for this
-- because normal mode is still legal
@enforce_normal cast;

-- TEST: numeric cast operation from bool not nullable (no-op version)
-- + b = false
-- + x = (b)
create proc local_cast_from_bool_no_op_notnull()
begin
  declare x bool not null;
  declare b bool not null;
  set b := 0;
  set x := cast(b as bool);
end;

-- TEST: numeric cast operation from bool nullable (no-op version)
-- + b = true
-- + x = (b)
create proc local_cast_from_bool_no_op()
begin
  declare b bool;
  set b := nullable(1);
  declare x bool;
  set x := cast(b as bool);
end;

@enforce_strict cast;

-- TEST: test cql_get_blob_size codegen
-- + "SELECT ?"
-- + rc_ = cql_multibind(_db_, _temp_stmt, "b", {blob_var})
-- + l0_nullable = cql_get_blob_size(_tmp_n_blob_%)
set l0_nullable := cql_get_blob_size((select blob_var));

-- TEST: test cql_get_blob_size codegen with not null blob
-- + l2 = cql_get_blob_size(blob_var2)
set l2 := cql_get_blob_size(blob_var2);

-- TEST: test basic proc savepoint structure
-- + "SAVEPOINT base_proc_savepoint"
-- + -- try
-- + "RELEASE base_proc_savepoint"
-- + ::catch_start%::
-- + "ROLLBACK TO base_proc_savepoint"
-- + _rc_ = cql_best_error(_rc_thrown_1)
-- + ::catch_end%::
create proc base_proc_savepoint()
begin
  proc savepoint
  begin
    declare X integer;
  end;
end;

-- TEST: commit returns will have two commit  paths
-- +1 "SAVEPOINT base_proc_savepoint_commit_return"
-- +3 "RELEASE base_proc_savepoint_commit_return"
-- +1 "ROLLBACK TO base_proc_savepoint_commit_return"
create proc base_proc_savepoint_commit_return()
begin
  proc savepoint
  begin
    if 1 then
      commit return;
    end if;
  end;
end;

-- TEST: rollback returns will have two rollback paths
-- +1 "SAVEPOINT base_proc_savepoint_rollback_return"
-- +2 "ROLLBACK TO base_proc_savepoint_rollback_return"
-- +3 "RELEASE base_proc_savepoint_rollback_return"
create proc base_proc_savepoint_rollback_return()
begin
  proc savepoint
  begin
    if 1 then
      rollback return;
    end if;
  end;
end;

DECLARE x INTEGER NOT NULL;

-- TEST: a series of paren checks on left association
-- avoid hard coded divide by zero (hence 4/3 not e.g 1/3)
-- + x = 1 * (4 // 3)
SET x := 1 * (4 / 3);

-- + x = 1 * 2 // 3
SET x := 1 * 2 / 3;

-- + x = 1 + 2 // 3
SET x := 1 + 2 / 3;

-- + x = 1 + (2 - 3)
SET x := 1 + (2 - 3);

-- + x = 1 + 2 * 3
SET x := 1 + 2 * 3;

-- + x = 1 * (2 + 3)
SET x := 1 * (2 + 3);

-- + x = 1 - (2 + 3)
SET x := 1 - (2 + 3);

-- + x = 1 - (2 - 3)
SET x := 1 - (2 - 3);

-- + x = 1 - 2 - (2 - 3)
SET x := 1 - 2 - (2 - 3);

-- the first parens do not change eval order from left to right at all
-- + x = 1 - 2 - (2 - 3)
SET x := (1 - 2) - (2 - 3);

-- + x = 1 // 2 // 3
SET x := 1 / 2 / 3;

-- avoid hard coded divide by zero
-- + x = 1 // (4 // 3)
SET x := 1 / (4 / 3);

-- + x = 1 // 2
SET x := 1 / 2;

-- + x = 1 * 2 * (3 * 4)
SET x := 1 * 2 * (3 * 4);

-- the first parens don't change anything
-- the second parens could matter if it was floating point
-- + x = 1 * 2 * (3 * 4)
SET x := (1 * 2) * (3 * 4);

-- note that in C & binds tighter than | so parens are required in C
-- note that in SQL | and & are equal so this expression left associates
-- + x = (1 | 2) & 3
SET x := 1 | 2 & 3;

-- + x = 1 | 2 & 3
SET x := 1 | (2 & 3);

-- + x = 1 | 2 | 3
SET x := 1 | 2 | 3;

-- sub optimal but we're trying to preserve written order due to floating point
-- + x = 1 | (2 | 3)
SET x := 1 | (2 | 3);

-- + x = 1 | (3 + 4 | 5)
SET x := 1 | (3 + 4 | 5);

-- + x = 1 | 3 + (4 | 5)
SET x := 1 | 3 + (4 | 5);

-- +  x = (1 | 3) + (4 | 5)
SET x := (1 | 3) + (4 | 5);

-- + x = (1 + 2) * 5
set x := (1 + 2) * 5;

-- + x = 1 + 2 - 1
set x := (1 + 2) - 1;

-- + x = 1 << 2 | 3
set x := 1 << 2 | 3;

-- + x = 1 << (2 | 3)
set x := 1 << (2 | 3);

-- + x = 1 | 2 << 3
set x := 1 | (2 << 3);

-- + x = 1 << (2 << 3)
set x := 1 << (2 << 3);

-- + cql_to_num(1 < cql_to_num((2 > 3)))
set x := 1 < (2 > 3);

-- + x = 1 << (2 >> 3)
set x := 1 << (2 >> 3);

-- + x = 1 | (2 | 3)
set x := 1 | (2 | 3);

-- + x = 1 | 2 | 3
set x := (1 | 2) | 3;

-- + cql_to_num(1 == cql_to_num((2 ~= 3)))
set x := 1 == (2 != 3);

create table SalesInfo(
  month integer,
  amount real
);

-- TEST: ORDERBY BETWEEN PRECEEDING AND FOLLOWING NO FILTER NO EXCLUDE
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS SalesMovingAverage
create proc window1()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: simple OVER and ORDER BY
-- + SUM(amount) OVER (ORDER BY month) AS RunningTotal
create proc window2()
begin
  SELECT month, amount, SUM(amount) OVER
    (ORDER BY month) RunningTotal
  FROM SalesInfo;
end;

-- TEST: ROWS expr preceeding and expr following, exclude no others
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS) AS SalesMovingAverage
create proc window3()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: ROWS expr preceeding and expr following, exclude no others with FILTER
-- + AVG(amount) FILTER (WHERE month = 1) OVER (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS) AS SalesMovingAverage
create proc window4()
begin
  SELECT month, amount, AVG(amount) FILTER(WHERE month = 1) OVER
    (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: ROWS expr preceeding and expr following, exclude current row
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 3 PRECEDING AND 4 FOLLOWING EXCLUDE CURRENT ROW) AS SalesMovingAverage
create proc window5()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month ROWS BETWEEN 3 PRECEDING AND 4 FOLLOWING EXCLUDE CURRENT ROW)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: ROWS expr preceeding and expr following, exclude group
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 4 PRECEDING AND 5 FOLLOWING EXCLUDE GROUP) AS SalesMovingAverage
create proc window6()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month ROWS BETWEEN 4 PRECEDING AND 5 FOLLOWING EXCLUDE GROUP)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: ROWS expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 6 PRECEDING AND 7 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
create proc window7()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month ROWS BETWEEN 6 PRECEDING AND 7 FOLLOWING EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: RANGE expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month RANGE BETWEEN 8 PRECEDING AND 9 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
create proc window8()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month RANGE BETWEEN 8 PRECEDING AND 9 FOLLOWING EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN 10 PRECEDING AND 11 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
create proc window9()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN 10 PRECEDING AND 11 FOLLOWING EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS unbounded proceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND 12 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
create proc window10()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND 12 FOLLOWING EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS expr following and expr preceeding
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN 13 FOLLOWING AND 14 PRECEDING) AS SalesMovingAverage
create proc window11()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN 13 FOLLOWING AND 14 PRECEDING)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS between current row and unbounded following
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS SalesMovingAverage
create proc window12()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS between unbounded preceding and current row with no exclude
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS SalesMovingAverage
create proc window13()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: GROUPS between unbounded preceding and current row with exclude ties
-- +  AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES) AS SalesMovingAverage
create proc window14()
begin
  SELECT month, amount, AVG(amount) OVER
    (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: correct parse and re-emit of CURRENT_ROW
-- + AVG(amount) OVER (PARTITION BY month ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES) AS SalesMovingAverage
create proc window15()
begin
  SELECT month, amount, AVG(amount) OVER
    (PARTITION BY month ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: correct parse and re-emit of CURRENT_ROW
-- + AVG(amount) OVER (GROUPS CURRENT ROW) AS SalesMovingAverage
create proc window16()
begin
  SELECT month, amount, AVG(amount) OVER
    (GROUPS CURRENT ROW)
  SalesMovingAverage FROM SalesInfo;
end;

-- TEST: use result code in a procedure
-- + function emit_rc(_db_)
-- + result_code = CQL_OK
-- + return _rc_, result_code
create proc emit_rc(out result_code integer not null)
begin
  set result_code := @rc;
end;

-- TEST: ensure that we use the right result code for thrown and storage
-- this code samples the @rc value at various places, the different names
-- allow us to be sure that we're using the right code in each scope.
-- + e0 = CQL_OK
-- + err = CQL_OK
-- + local _rc_thrown_1 = _rc_
-- + err = _rc_thrown_1
-- + e1 = _rc_thrown_1
-- + e2 = _rc_thrown_1
-- + local _rc_thrown_2 = _rc_
-- + e3 = _rc_thrown_2
-- + err = _rc_thrown_2
-- + _rc_ = cql_best_error(_rc_thrown_2)
-- + e4 = _rc_thrown_1
-- + local _rc_thrown_3 = _rc_
-- + e5 = _rc_thrown_3
-- + printf("Error %d\n", err)
-- + e6 = CQL_OK
create proc rc_test()
begin
  LET err := @rc;
  let e0 := @rc;
  begin try
  begin try
    create table whatever_anything(id integer);
  end try;
  begin catch
    set err := @rc;
    let e1 := @rc;
    begin try
       let e2 := @rc;
       create table whatever_anything(id integer);
    end try;
    begin catch
       let e3 := @rc;
       set err := @rc;
       throw;
    end catch;
    let e4 := @rc;
  end catch;
  end try;
  begin catch
    let e5 := @rc;
    call printf("Error %d\n", err);
  end catch;
  let e6 := @rc;
end;

-- TEST: lazy decl of rcthrown variables (via throw)
-- - local _rc_thrown_1 = _rc_
-- + local _rc_thrown_2 = _rc_
-- + _rc_ = cql_best_error(_rc_thrown_2)
create proc rc_test_lazy1()
begin
  begin try
    create table whatever_anything(id integer);
  end try;
  begin catch
    begin try
       create table whatever_anything(id integer);
    end try;
    begin catch
       throw;
    end catch;
  end catch;
end;

-- TEST: lazy decl of rcthrown variables (via @rc)
-- - local _rc_thrown_1 = _rc_
-- + local _rc_thrown_2 = _rc_
-- + err = _rc_thrown_2
create proc rc_test_lazy2()
begin
  begin try
    create table whatever_anything(id integer);
  end try;
  begin catch
    begin try
       create table whatever_anything(id integer);
    end try;
    begin catch
       let err := @rc;
    end catch;
  end catch;
end;

-- TEST: make an integer enum
declare enum some_ints integer (
  foo = 12,
  bar = 3
);

-- TEST: make a float enum
declare enum some_reals real (
  foo = 12,
  bar = 3
);

-- TEST: make a long enum
declare enum some_longs long (
  foo = 87363537363847643647937,
  bar = 3
);

-- TEST: emit enums doesn't do anything in lua because there is no .h file
-- we could emit them as globals or something but we don't do anything right now
@emit_enums some_ints;
@emit_enums;

-- TEST: if we ever emit stuff we have to be ok with emitting the same thing twice
@emit_enums some_longs;

-- TEST: resolve a virtual table, note that the arguments become the declaration
-- + "CREATE VIRTUAL TABLE virt_table USING virt_module ( id INTEGER, t TEXT)"
create proc virtual_table_creator()
begin
  -- this will be rewritten
  create virtual table virt_table using virt_module (arguments following) as (
    id integer,
    t text
  );
end;

-- TEST: the cursor here should not have the out arg form of y
-- + local y
-- + y = 0
-- + C.x = 1
-- + C.y = 1
-- + C.y = out_arg_cursor(C.x)
-- + return y
create proc out_arg_cursor(x integer not null, out y integer not null)
begin
  declare C cursor like out_arg_cursor arguments;
  fetch C from values(1,1);
  call out_arg_cursor(from C);
end;

-- TEST: create virtual table
-- +    "CREATE VIRTUAL TABLE v1 USING m1"
-- +    "CREATE VIRTUAL TABLE v2 USING m2 (x)"
-- +    "CREATE VIRTUAL TABLE v3 USING m2 ( id INTEGER)"
create proc make_virt_table()
begin
  create virtual table v1 using m1 as (id integer);
  create virtual table v2 using m2(x) as (id integer);
  create virtual table v3 using m2(arguments following) as (id integer);
end;

-- TEST: declaration of a named type
declare my_name_type type text not null;

-- make a virtual table with a hidden column for use in the next tests
create virtual table virtual_with_hidden using module_name as (
  vx integer hidden not null,
  vy integer
);

-- TEST: hidden applied on virtual tables
-- +  "SELECT vy FROM virtual_with_hidden"
create proc virtual1()
begin
  select * from virtual_with_hidden;
end;

-- TEST: hidden columns may be used by name
-- + "SELECT vx, vy FROM virtual_with_hidden WHERE vx = 2"
create proc virtual2()
begin
  select vx, vy from virtual_with_hidden where vx = 2;
end;

-- TEST: insert into the table, verify autoexpand is correct there, too
-- only "y" should be inserted here
-- + _rc_ = cql_exec(_db_,
-- + "INSERT INTO virtual_with_hidden(vy) VALUES(1)"
insert into virtual_with_hidden values(1);

-- TEST: you can use the hidden column if you do it by name
-- + _rc_ = cql_exec(_db_,
-- + "INSERT INTO virtual_with_hidden(vx, vy) VALUES(1, 2)"
insert into virtual_with_hidden(vx, vy) values(1,2);

-- TEST: get row from the bar table or else -1
-- +  _rc_ = cql_step(_temp_stmt)
-- +  if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  if _rc_ == CQL_ROW then
-- +    _tmp_n_int_1 = cql_get_value(_temp_stmt, 0)
-- +    i0_nullable = _tmp_n_int_1
-- +  else
-- +    i0_nullable = - 1
-- +  end
set i0_nullable := (select type from bar if nothing -1);

-- TEST: normal code gen for if nothing throw
-- +  if _rc_ ~= CQL_OK then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  _rc_ = cql_step(_temp_stmt)
-- +  if _rc_ ~= CQL_ROW then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- +  _tmp_n_int_0 = cql_get_value(_temp_stmt, 0)
-- +  i0_nullable = _tmp_n_int_0
set i0_nullable := (select type from bar if nothing throw);

-- TEST: get row from bar if no row or null -1
-- + if _rc_ == CQL_ROW then
-- +   _tmp_n_int_1 = cql_get_value(_temp_stmt, 0)
-- + end
-- + if _rc_ == CQL_DONE or _tmp_n_int_1 == nil then
-- +   i2 = - 1
-- + else
-- +   i2 = _tmp_n_int_1
-- + end
set i2 := (select type from bar if nothing or null -1);

-- TEST: get row from the bar table or else ""
-- + if _rc_ == CQL_ROW then
-- +   _tmp_n_text_1 = cql_get_value(_temp_stmt, 0)
-- +   t0_nullable = _tmp_n_text_1
-- + else
-- +   t0_nullable = ""
-- + end
set t0_nullable := (select name from bar if nothing "");

-- TEST: get row from the bar table or else "garbonzo"
-- + _rc_ = cql_step(_temp_stmt)
-- + if _rc_ ~= CQL_ROW and _rc_ ~= CQL_DONE then cql_error_trace(_rc_, _db_); goto cql_cleanup; end
-- + if _rc_ == CQL_ROW then
-- +   _tmp_n_text_1 = cql_get_value(_temp_stmt, 0)
-- + end
-- + if _rc_ == CQL_DONE or _tmp_n_text_1 == nil then
-- +   t2 = "garbonzo"
-- + else
-- +   t2 = _tmp_n_text_1
-- + end
set t2 := (select name from bar if nothing or null "garbonzo");

-- TEST: verify private exports and binding
-- private doesn't mean anything in lua
-- + function private_proc()
@attribute(cql:private)
create proc private_proc(out x integer)
begin
  set x := 1;
end;

-- TEST: verify that getters are not present on private out union but the fetcher is
-- LUA never has getters so that part is moot
-- + function private_out_union_fetch_results()
-- + table.insert(_rows_, cql_clone_row(C))
-- + return _rows_
@attribute(cql:private)
create proc private_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in LUA correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + C_result_set_ = private_out_union_fetch_results()
create proc use_private_out_union()
begin
  declare C cursor for call private_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify that getters are not present on no getters out union but the fetcher is
-- LUA never has getters so this is moot
-- + function no_getters_out_union_fetch_results()
-- + local _rows_ = {}
-- + C._has_row_ = true
-- + C.a_field = 1
-- + table.insert(_rows_, cql_clone_row(C))
-- + return _rows_
@attribute(cql:suppress_getters)
create proc no_getters_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in LUA correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + C_result_set_ = no_getters_out_union_fetch_results()
create proc use_no_getters_out_union()
begin
  declare C cursor for call no_getters_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify that getters are not present on suppress results out union but the fetcher is
-- lua never has getters so private is kind of moot
-- + function suppress_results_out_union_fetch_results()
-- + table.insert(_rows_, cql_clone_row(C))
@attribute(cql:suppress_result_set)
create proc suppress_results_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in LUA correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + function use_suppress_results_out_union(_db_)
-- + C_result_set_ = suppress_results_out_union_fetch_results()
create proc use_suppress_results_out_union()
begin
  declare C cursor for call suppress_results_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify private exports and binding for result set case
-- private procs have result set suppressed
-- + function private_result(_db_)
-- + "SELECT 1"
-- + return _rc_, _result_stmt, x
-- - private_result_fetch_results
@attribute(cql:private)
create proc private_result(out x integer)
begin
  select 1 x;
end;

-- TEST: private proc forward ref results in static prototype
-- this doesn't mean anything in LUA, no result set
-- + @ATTRIBUTE(cql:private)
@attribute(cql:private)
declare proc private_fwd_ref(x integer not null);

-- TEST: ensure out args set to null for ref types
-- nothing to do in LUA
-- + local x
-- + return x
create proc set_out_arg_ref_test(out x text)
begin
end;

-- TEST: ensure out args set to null for nullable types
-- nothing to do in LUA
-- + local x
-- + return x
create proc set_out_arg_null_test(out x integer)
begin
end;

-- TEST: ensure out args set to null for non-null types
-- + x = 0
-- + return x
create proc set_out_arg_notnull_test(out x integer not null)
begin
end;

declare global_cursor2 cursor like select "x" x;

-- TEST: closing a cursor should finalize its statement if it has one and values if it has them
-- + function early_close_cursor(_db_)
-- + cql_finalize_stmt(global_cursor_stmt)
-- + global_cursor_stmt = nil
-- + global_cursor = { _has_row_ = false }
-- + global_cursor2 = { _has_row_ = false }
create proc early_close_cursor()
begin
  close global_cursor;
  close global_cursor2;
end;

-- TEST: construct a lot of variables of various types
-- the decls are all the same in LUA
-- + r = 1.0
-- + i = 1
-- + l = 1
-- + t = "T"
-- + nl = (~2)
-- + ni = (2 + 2)
-- + nr = 2.0
-- + nt = "NT"
-- + sl = (~3)
-- + si = (3 + 3)
-- + sr = 3.0
-- + st = "ST"
create proc various_lets()
begin
  let r := 1.0;
  let i := 1;
  let l := 1L;
  let t := "T";
  let nl := nullable(~2L);
  let ni := nullable(2+2);
  let nr := nullable(2.0);
  let nt := nullable("NT");
  let sl := sensitive(~3L);
  let si := sensitive(3+3);
  let sr := sensitive(3.0);
  let st := sensitive("ST");
end;

-- TEST: check that rc is set correctly in try/catch blocks
-- +1 local _rc_ = CQL_OK
-- one setting the code on exit plus one for the init as above
-- rc is not clobbered elsewhere!
-- +2 _rc_ = CQL_OK
-- + return _rc_
create proc try_catch_rc()
begin
  declare C cursor for select 'foo' extra2 from bar;
  begin try
    fetch C;
  end try;
  begin catch
  end catch;
end;

-- TEST: basic code gen for the switch
-- + _tmp_int_0 = i2
-- + repeat
-- +   if _tmp_int_0 == 1 or _tmp_int_0 == 3 then
-- +     i2 = 30
-- +     break
-- +   end
-- +   if _tmp_int_0 == 4 then
-- +     i2 = 40
-- +     break
-- +   end
-- +   if _tmp_int_0 == 5 then
-- +     break
-- +   end
-- +   -- default
-- +     i2 = 50
-- + until true
-- case 5 must be present because there is a default, so it needs the case label and break;
switch i2
  when 1, 3 then
    set i2 := 30;
  when 4 then
    set i2 := 40;
  when 5 then nothing
  else
    set i2 := 50;
end;

-- TEST: basic code gen for the switch (no default)
-- + _tmp_int_0 = i2
-- + repeat
-- +   if _tmp_int_0 == 1 or _tmp_int_0 == 3 then
-- +     i2 = 30
-- +     break
-- +   end
-- +   if _tmp_int_0 == 4 then
-- +     i2 = 40
-- +     break
-- +   end
-- + until true
-- the 5 case can be removed as there is no default label
-- so we save some code
-- - if _tmp_int_0 == 5 then
switch i2
  when 1, 3 then
    set i2 := 30;
  when 4 then
    set i2 := 40;
  when 5 then nothing
end;

-- TEST: basic code gen for the switch (no default, int64)
-- + _tmp_int64_0 = l2
-- + repeat
-- +   if _tmp_int64_0 == 1 or _tmp_int64_0 == 3 then
-- +     i2 = 30
-- +     break
-- +   end
-- +   if _tmp_int64_0 == 4 then
-- +     i2 = 40
-- +     break
-- +   end
-- + until true
switch l2
  when 1, 3 then
    set i2 := 30;
  when 4L then
    set i2 := 40;
  when 5 then nothing
end;

-- TEST: special case: just excluding 1, 2, 3... no statements but the ELSE
-- + _tmp_int_0 = i2
-- + repeat
-- +  if _tmp_int_0 == 1 or _tmp_int_0 == 2 or _tmp_int_0 == 3 then
-- +    break
-- +  end
-- +  -- default
-- +     i2 = 123
-- + until true
switch i2
  when 1, 2, 3 then nothing
  else
    set i2 := 123;
end;

-- TEST: use of LEAVE within a switch
-- + _tmp_int_0 = i2
-- + repeat
-- +   if _tmp_int_0 == 1 then
-- +     if cql_to_bool(i2) then
-- +       break
-- +     end
-- +     i2 = 999
-- +     break
-- +   end
-- +   -- default
-- +     i2 = 1
-- + until true
switch i2
  when 1 then
    if i2 then leave; end if;
    set i2 := 999;
  else
    set i2 := 1;
end;

-- used in the next suite of tests
declare proc out2_proc(x integer, out y integer not null, out z integer not null);

-- TEST: implicit declare including re-use
-- + function out_decl_test(x)
-- +2 u, v = out2_proc(x)
create proc out_decl_test(x integer)
begin
  declare out call out2_proc(x, u, v);
  declare out call out2_proc(x, u, v);
end;

-- TEST: implicit declare within a loop; this is a different case because
-- sem_declare_out_call_stmt has to take care to retain the SEM_TYPE_IMPLICIT
-- flags appropriately during loop reanalysis
-- + function out_decl_loop_test(x)
-- +2 u, v = out2_proc(x)
create proc out_decl_loop_test(x integer)
begin
  while 1
  begin
    declare out call out2_proc(x, u, v);
    declare out call out2_proc(x, u, v);
  end;
end;

-- TEST: most binary operations involving a null-typed argument result in null
-- one for the initializer and one for the assignment
-- +1 add0 = nil
-- +1 add1 = nil
-- +1 bin_and0 = nil
-- +1 bin_and1 = nil
-- +1 bin_or0 = nil
-- +1 bin_or1 = nil
-- +1 div0 = nil
-- +1 div1 = nil
-- +1 ge0 = nil
-- +1 ge1 = nil
-- +1 gt0 = nil
-- +1 gt1 = nil
-- +1 le0 = nil
-- +1 le1 = nil
-- these also match not_like
-- +2 like0 = nil
-- +2 like1 = nil
-- +1 lshift0 = nil
-- +1 lshift1 = nil
-- +1 lt0 = nil
-- +1 lt1 = nil
-- +1 mod0 = nil
-- +1 mod1 = nil
-- +1 mul0 = nil
-- +1 mul1 = nil
-- +1 not_like0 = nil
-- +1 not_like1 = nil
-- +1 rshift0 = nil
-- +1 rshift1 = nil
-- +1 sub0 = nil
-- +1 sub1 = nil
-- - Error
create proc binary_ops_with_null()
begin
  let add0 := NULL + 42;
  let add1 := 42 + NULL;
  let bin_and0 := NULL & 42;
  let bin_and1 := 42 & NULL;
  let bin_or0 := NULL | 42;
  let bin_or1 := 42 | NULL;
  let div0 := NULL / 42;
  let div1 := 42 / NULL;
  let ge0 := NULL >= 42;
  let ge1 := 42 >= NULL;
  let gt0 := NULL > 42;
  let gt1 := 42 > NULL;
  let le0 := NULL <= 42;
  let le1 := 42 <= NULL;
  let like0 := NULL LIKE "foo";
  let like1 := "foo" LIKE NULL;
  let lshift0 := NULL << 42;
  let lshift1 := 42 << NULL;
  let lt0 := NULL < 42;
  let lt1 := 42 < NULL;
  let mod0 := NULL % 42;
  let mod1 := 42 % NULL;
  let mul0 := NULL * 42;
  let mul1 := 42 * NULL;
  let not_like0 := NULL NOT LIKE "foo";
  let not_like1 := "foo" NOT LIKE NULL;
  let rshift0 := NULL >> 42;
  let rshift1 := 42 >> NULL;
  let sub0 := NULL - 42;
  let sub1 := 42 - NULL;
end;

-- Verify that this is a DML proc even though it does nothing but use throw
-- + function uses_throw(_db_)
-- + _rc_ = cql_best_error(CQL_OK)
create proc uses_throw()
begin
  throw;
end;

-- TEST: verify that this is a DML proc even though it does nothing but ifnull_throw
-- + function uses_ifnull_throw(_db_, x)
-- + _rc_ = CQL_ERROR
create proc uses_ifnull_throw(x int)
begin
   let y := ifnull_throw(x);
end;

-- TEST: force out cursor with object case
-- + function out_object(o)
-- + cql_contract_argument_notnull(o, 1)
-- + C._has_row_ = true
-- + C.o = o
-- + _result_ = cql_clone_row(C)
-- + function out_object_fetch_results(o)
-- + _result_ = out_object(o)
-- + result_set = { _result_ }
create proc out_object(o object not null)
begin
  declare C cursor like out_object arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: Verify that contracts are inserted where appropriate (and not inserted
-- where not appropriate)
-- +  cql_contract_argument_notnull(b, 2)
-- +  cql_contract_argument_notnull(d, 4)
-- +  cql_contract_argument_notnull(f, 6)
-- +  cql_contract_argument_notnull(h, 8)
-- +  cql_contract_argument_notnull(n, 14)
-- +  cql_contract_argument_notnull(p, 16)
create proc exercise_contracts(
  a int,
  b int not null,
  c text,
  d text not null,
  e blob,
  f blob not null,
  g object,
  h object not null,
  out i int,
  out j int not null,
  out k text,
  out l text not null,
  inout m int,
  inout n int not null,
  inout o text,
  inout p text not null,
)
begin
  set l := "text";
end;

-- TEST: Contracts should be emitted for public procs
-- +1 cql_contract_argument_notnull(t, 1)
create proc public_proc_with_a_contract(t text not null)
begin
end;

-- TEST: Contracts should not be emitted for private procs
-- - cql_contract_argument_notnull
@attribute(cql:private)
create proc private_proc_without_a_contract(t text not null)
begin
end;

-- TEST: Contracts should be emitted only once, not also in fetch results
-- +1 cql_contract_argument_notnull(t, 1)
create proc result_set_proc_with_contract_in_fetch_results(t text not null)
begin
  select * from bar;
end;

-- TEST: Contracts should be emitted only once, not also in fetch results
-- +1 cql_contract_argument_notnull(t, 1)
create proc out_proc_with_contract_in_fetch_results(t text not null)
begin
  declare C cursor like bar;
  out C;
end;

-- TEST: The improving of nullable variables compiles to nothing in SQL.
-- + "SELECT ? + 1"
create proc nullability_improvements_are_erased_for_sql()
begin
  declare a int;
  if a is not null then
    select (a + 1) as b;
  end if;
end;

-- TEST: The improving of nullable variables to be nonnull respects the
-- underlying nullable representation.
-- + function nullability_improvements_do_not_change_access()
-- + if a ~= nil then
-- + b = a
-- + a = 0
create proc nullability_improvements_do_not_change_access()
begin
  declare a int;
  if a is not null then
    let b := a;
    set a := 0;
  end if;
end;

-- TEST: a loose select statement generates no code (and will produce no errors)
-- the errors are checked when this code is compiled in LUA.  If the code
-- were generated there would be errors because the global proc
-- doesn't have the statement out arg.  We also verify that
-- no call to cql_prepare happens hence no select
-- - cql_prepare
select 1 x;

-- TEST: numeric is true case
-- + true_test = (1 ~= 0)
let true_test := 1 is true;

-- TEST: numberic is false case
-- + false_test = (0 == 0)
let false_test := 0 is false;

-- TEST: the helper handles all the weird cases...
-- + true_test = cql_is_true(i0_nullable)
set true_test := i0_nullable is true;

-- TEST: the helper handles all the weird cases...
-- + false_test = cql_is_false(i0_nullable)
set false_test := i0_nullable is false;

-- TEST: number is not true test
-- + true_test = (1 == 0)
set true_test := 1 is not true;

-- TEST: numeric is not false test
-- + false_test = (0 ~= 0)
set false_test := 0 is not false;

-- TEST: the helper handles all the weird cases...
-- + true_test = cql_is_not_true(i0_nullable)
set true_test := i0_nullable is not true;

-- TEST: the helper handles all the weird cases...
-- +  false_test = cql_is_not_false(i0_nullable)
set false_test := i0_nullable is not false;

CREATE TABLE big_data(
  f1 LONG_INT NOT NULL,
  f2 INTEGER NOT NULL,
  f3 TEXT,
  f4 TEXT NOT NULL,
  f5 TEXT,
  f6 TEXT,
  f7 LONG_INT,
  f8 LONG_INT NOT NULL,
  f9 LONG_INT NOT NULL,
  f10 LONG_INT NOT NULL,
  f11 LONG_INT NOT NULL,
  f12 TEXT @SENSITIVE,
  f13 BOOL NOT NULL,
  f14 LONG_INT,
  f15 BOOL,
  f16 INTEGER NOT NULL,
  f17 INTEGER NOT NULL,
  f18 TEXT,
  f19 INTEGER,
  f20 TEXT,
  f21 INTEGER,
  f22 TEXT,
  f23 INTEGER,
  f24 LONG_INT NOT NULL,
  f25 TEXT,
  f26 BOOL NOT NULL,
  f27 BOOL NOT NULL,
  f28 BOOL NOT NULL,
  f29 TEXT,
  f30 TEXT,
  f31 TEXT,
  f32 INTEGER,
  f33 LONG_INT,
  f34 INTEGER,
  f35 TEXT,
  f36 TEXT,
  f38 LONG_INT NOT NULL,
  f39 LONG_INT UNIQUE,
  f40 BOOL,
  f41 BOOL NOT NULL,
  f42 TEXT,
  f43 TEXT,
  f44 LONG_INT,
  f45 BOOL NOT NULL,
  f46 LONG_INT,
  f47 INTEGER NOT NULL,
  f48 TEXT,
  f49 LONG_INT,
  f50 TEXT,
  f51 TEXT,
  f52 LONG_INT,
  f53 INTEGER NOT NULL,
  f54 TEXT,
  f55 LONG_INT NOT NULL,
  f56 LONG_INT NOT NULL,
  f57 TEXT,
  f58 TEXT,
  f59 INTEGER,
  f60 TEXT,
  f61 INTEGER,
  f62 LONG_INT,
  f63 LONG_INT,
  f64 INTEGER,
  f65 LONG_INT NOT NULL,
  f66 INTEGER NOT NULL,
  f67 INTEGER NOT NULL,
  f68 INTEGER,
  f69 TEXT,
  f70 REAL,
  f71 LONG_INT,
  f72 INTEGER,
  f73 INTEGER,
  f74 LONG_INT,
  f75 INTEGER
);

-- TEST: lots of string temporaries create no errors
-- this is really a stress test for the stack and temporary management
-- the codegen is very simple. The compiler shouldn't crash on this stuff
-- it has in the past.
-- + "SELECT f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29, f30, f31, f32, f33, f34, f35, f36, f38, f39, f40, f41, f42, f43, f44, f45, f46, f47, f48, f49, f50, f51, f52, f53, f54, f55, f56, f57, f58, f59, f60, f61, f62, f63, f64, f65, f66, f67, f68, f69, f70, f71, f72, f73, f74, f75 FROM big_data"
-- + _rc_ = cql_multifetch(C_stmt, C, C_types_, C_fields_)
CREATE PROC BigFormat ()
BEGIN
  DECLARE C CURSOR FOR SELECT * FROM big_data;
  LOOP FETCH C
  BEGIN
    LET s := cql_cursor_format(C);
  END;
END;

-- TEST: codegen for sign
-- + sign_val_int = cql_unary_sign(- 2)
LET sign_val_int := sign(-2);

-- TEST: codegen for sign: nullable arg
-- + sign_val_nullable = cql_unary_sign((-2))
LET sign_val_nullable := sign(nullable(-2));

-- TEST: codegen for absolute value
-- + abs_val_int = cql_unary_abs(- 2)
LET abs_val_int := abs(-2);

-- TEST: codegen for absolute value: nullable arg
-- + abs_val_nullable = cql_unary_abs((-2))
LET abs_val_nullable := abs(nullable(-2));

-- TEST: codegen for absolute value long
-- + abs_val_long = cql_unary_abs(- 2)
LET abs_val_long := abs(-2L);

-- TEST: codegen for absolute value real
-- + abs_val_real = cql_unary_abs(- 2.0)
LET abs_val_real := abs(-2.0);

-- TEST: codegen for absolute value bool
-- + abs_val_bool = cql_unary_abs(true)
LET abs_val_bool := abs(true);

-- TEST: codegen for absolute value of null
-- + abs_val_nullable = cql_unary_abs(nil)
SET abs_val_nullable := abs(null);

-- Used in the following test.
create proc ltor_proc_int_not_null(a int not null, b int not null, out c int not null) begin end;
create proc ltor_proc_int(a int, b int, out c int) begin end;
create proc ltor_proc_text_not_null(a text not null, b text not null, out c text not null) begin set c := "text"; end;
create proc ltor_proc_text(a text, b text, out c text) begin end;
declare function ltor_func_int_not_null(a int not null, b int not null) int not null;
declare function ltor_func_int(a int, b int) int;
declare function ltor_func_text_not_null(a text not null, b text not null) text not null;
declare function ltor_func_text(a text, b text) text;

-- TEST: Arguments are always evaluated left-to-right (which is ensured by generating temps).
-- + _tmp_int_1 = ltor_proc_int_not_null(1, 2)
-- + _tmp_int_2 = ltor_proc_int_not_null(3, 4)
-- + a = ltor_proc_int_not_null(_tmp_int_1, _tmp_int_2)
-- + _tmp_n_int_1 = ltor_proc_int(1, 2)
-- + _tmp_n_int_2 = ltor_proc_int(3, 4)
-- + b = ltor_proc_int(_tmp_n_int_1, _tmp_n_int_2)
-- + _tmp_text_1 = ltor_proc_text_not_null("1", "2")
-- + _tmp_text_2 = ltor_proc_text_not_null("3", "4")
-- + c = ltor_proc_text_not_null(_tmp_text_1, _tmp_text_2)
-- + _tmp_n_text_1 = ltor_proc_text("1", "2")
-- + _tmp_n_text_2 = ltor_proc_text("3", "4")
-- + d = ltor_proc_text(_tmp_n_text_1, _tmp_n_text_2)
-- + _tmp_int_1 = ltor_func_int_not_null(1, 2)
-- + _tmp_int_2 = ltor_func_int_not_null(3, 4)
-- + e = ltor_func_int_not_null(_tmp_int_1, _tmp_int_2)
-- + _tmp_n_int_1 = ltor_func_int(1, 2)
-- + _tmp_n_int_2 = ltor_func_int(3, 4)
-- + f = ltor_func_int(_tmp_n_int_1, _tmp_n_int_2)
-- + _tmp_text_1 = ltor_func_text_not_null("1", "2")
-- + _tmp_text_2 = ltor_func_text_not_null("3", "4")
-- + g = ltor_func_text_not_null(_tmp_text_1, _tmp_text_2)
-- + _tmp_n_text_1 = ltor_func_text("1", "2")
-- + _tmp_n_text_2 = ltor_func_text("3", "4")
-- + h = ltor_func_text(_tmp_n_text_1, _tmp_n_text_2)
create proc arguments_are_evaluated_left_to_right()
begin
  let a := ltor_proc_int_not_null(ltor_proc_int_not_null(1, 2), ltor_proc_int_not_null(3, 4));
  let b := ltor_proc_int(ltor_proc_int(1, 2), ltor_proc_int(3, 4));
  let c := ltor_proc_text_not_null(ltor_proc_text_not_null("1", "2"), ltor_proc_text_not_null("3", "4"));
  let d := ltor_proc_text(ltor_proc_text("1", "2"), ltor_proc_text("3", "4"));
  let e := ltor_func_int_not_null(ltor_func_int_not_null(1, 2), ltor_func_int_not_null(3, 4));
  let f := ltor_func_int(ltor_func_int(1, 2), ltor_func_int(3, 4));
  let g := ltor_func_text_not_null(ltor_func_text_not_null("1", "2"), ltor_func_text_not_null("3", "4"));
  let h := ltor_func_text(ltor_func_text("1", "2"), ltor_func_text("3", "4"));
end;

create proc f1(out x integer not null)
begin
  set x := 5;
end;

create proc f2(out x integer )
begin
  set x := 5;
end;

create proc f3(y integer, out x integer )
begin
  set x := y;
end;

-- TEST: ensure that the temporary from calling f1 is not reused in the 3rd call
-- everything is easier in LUA it's a standard pattern...
-- proc as func not null case (same in LUA)
-- + _tmp_int_0 = f1()
-- + _tmp_int_1 = f1()
-- + _tmp_int_2 = f1()
-- + q = _tmp_int_0 + _tmp_int_1 + _tmp_int_2
-- proc as func case (no args)
-- + _tmp_n_int_0 = f2()
-- + _tmp_n_int_1 = f2()
-- + _tmp_n_int_2 = f2()
-- + r = cql_add(cql_add(_tmp_n_int_0, _tmp_n_int_1), _tmp_n_int_2)
-- proc as func case (args)
-- + _tmp_n_int_0 = f3(0)
-- + _tmp_n_int_1 = f3(1)
-- + tmp_n_int_2 = f3(2)
-- + s = cql_add(cql_add(_tmp_n_int_0, _tmp_n_int_1), _tmp_n_int_2)
create proc multi_call_temp_reuse()
begin
  let q := f1() + f1() + f1();
  let r := f2() + f2() + f2();
  let s := f3(0) + f3(1) + f3(2);
end;

-- TEST: The `sensitive` function is a no-op and never appears in the LUA output.
-- + x = "hello"
-- + "SELECT 'hello'")
create proc sensitive_function_is_a_no_op()
begin
  let x := sensitive("hello");
  select sensitive("hello") as y;
end;

-- TEST: the AND operator has unusual short circuit evaluation
-- this monster gave us fits with temporary generation in the C code but
-- lua uses lazy eval functions to do short circuit.  It doesn't need any temporaries ever
-- so this ends up being stupid easy.  It's included for historical purposes.
-- breaking this into fragments for readability
--
-- + if cql_shortcircuit_and(cql_gt(a, b),
-- + function() return cql_shortcircuit_or(cql_lt(a, c),
-- + function() return c == nil end) end) then
-- + c = a
-- + end
create proc and_preserves_temps(a long, b long, c long)
begin
  if a > b and (a < c or c is null) then
     set c := a;
  end if;
end;

-- TEST: the OR operator has unusual short circuit evaluation
-- this monster gave us fits with temporary generation in the C code but
-- lua uses lazy eval functions to do short circuit.  It doesn't need any temporaries ever
-- so this ends up being stupid easy.  It's included for historical purposes.
-- breaking this into fragments for readability
--
-- + if cql_shortcircuit_or(cql_lt(c, 0),
-- + function() return cql_shortcircuit_and(cql_gt(a, c),
-- + function() return cql_gt(b, c) end) end) then
-- + c = a
-- + end
create proc or_preserves_temps(a long, b long, c long)
begin
  if c < 0 or (a > c and b > c) then
     set c := a;
  end if;
end;

-- TEST: make sure we don't emit this into the output
-- - function shared_frag
@attribute(cql:shared_fragment)
create proc shared_frag()
begin
 select 1234 shared_something; -- hence no cql_code return type
end;

-- TEST use the above
-- note that the generated string has the query parts above
-- Fragment sandwich:
-- ---- first we see the prepare_var variant
-- + _rc_, _result_stmt = cql_prepare_var(_db_,
-- --- three parts in this sandwich
-- + 3, nil
-- + "WITH shared_frag (shared_something) AS (",
-- ---- then we see the shared fragment-- note the name can be elided and it is!
-- + "SELECT 1234",
-- ---- then we see what came after the shared fragment
-- + ") SELECT shared_something FROM shared_frag"
create proc foo()
begin
  with
    (call shared_frag())
  select * from shared_frag;
end;


-- used in the following test
@attribute(cql:shared_fragment)
create proc shared_conditional(x integer not null)
begin
  if x == 1 then
    select x as x;
  else if x == 2 then
    select x + x as x;
  else
    select x + x + x as x;
  end if;
end;

-- TEST: use a conditional shared fragment in various ways
-- this proc forces a variable sandwich, there are
-- variables before the conditionals inside the conditonals
-- (see above) and after the conditionals
-- 5 text fragments
-- 8 variable usages
--
-- + local _preds_1  -- 5 possible fragments
-- + local _vpreds_1 -- 8 possible bindings
-- + _preds_1 = {}
-- + _vpreds_1 = {}
--
-- control flow to figure out which predicates to enable
-- + _p1_x_ = 1
-- + _preds_1[0] = true
-- + _vpreds_1[0] = true -- pred 0 known to be true
-- + if _p1_x_ == 1 then
-- +   _preds_1[1] = true
-- +   _vpreds_1[1] = true -- pred 1 known to be true
-- + else
-- +   if _p1_x_ == 2 then
-- +     _preds_1[2] = true
-- +     _vpreds_1[2] = true -- pred 2 known to be true
-- +     _vpreds_1[3] = true -- pred 2 known to be true
-- +   else
-- +     _preds_1[3] = true
-- +     _vpreds_1[4] = true -- pred 3 known to be true
-- +     _vpreds_1[5] = true -- pred 3 known to be true
-- +     _vpreds_1[6] = true -- pred 3 known to be true
-- +   end
-- + end
-- + _preds_1[4] = true
-- + _vpreds_1[7] = true -- pred 0 known to be true
-- + _rc_, _result_stmt = cql_prepare_var(_db_,
-- + 5, _preds_1,
--
-- root fragment 0 always present
-- + "WITH some_cte (id) AS (SELECT ?), shared_conditional (x) AS (",
--
-- option 1 fragment 1
-- + "SELECT ?",
--
-- option 2 fragment 2
-- + "SELECT ? + ?",
--
-- option 3 fragment 3
-- + "SELECT ? + ? + ?",
--
-- pop to root, fragment 4 condition same as fragment 0
-- + ") SELECT bar.id, bar.name, bar.rate, bar.type, bar.size FROM bar INNER JOIN some_cte ON ? = 5"
--
-- 8 variable sites, only some of which are used
-- + _rc_ = cql_multibind_var(_db_, _result_stmt, 8, _vpreds_1, "IIIIIIII", {x, _p1_x_, _p1_x_, _p1_x_, _p1_x_, _p1_x_, _p1_x_, x})
create proc shared_conditional_user(x integer not null)
begin
  with
  some_cte(id) as (select x),
  (call shared_conditional(1))
  select bar.* from bar join some_cte on x = 5;
end;


-- used in the following test, this is silly fragment
-- but it forces complex push and pop of variable state
@attribute(cql:shared_fragment)
create proc nested_shared_proc(x_ integer not null)
begin
  if x_ <= 5 then
    with
    (call shared_conditional(1))
    select * from shared_conditional where x_ == 5;
  else
    select x_ as x;
  end if;
end;

-- TEST: variable arg management in a nested context
-- + local _preds_1  -- 8 possible fragments
-- + local _vpreds_1 -- 8 possible bindings
-- + _preds_1 = {}
-- + _vpreds_1 = {}
-- + _p1_x__ = 1
-- + _preds_1[0] = true
-- + if _p1_x__ <= 5 then
-- +   _preds_1[1] = true
-- +   _p2_x_ = 1
-- +   if _p2_x_ == 1 then
-- +     _preds_1[2] = true
-- +     _vpreds_1[0] = true -- pred 2 known to be true
-- +   else
-- +     if _p2_x_ == 2 then
-- +       _preds_1[3] = true
-- +       _vpreds_1[1] = true -- pred 3 known to be true
-- +       _vpreds_1[2] = true -- pred 3 known to be true
-- +     else
-- +       _preds_1[4] = true
-- +       _vpreds_1[3] = true -- pred 4 known to be true
-- +       _vpreds_1[4] = true -- pred 4 known to be true
-- +       _vpreds_1[5] = true -- pred 4 known to be true
-- +     end
-- +   end
-- this is what's unique about this test, we popped back to the context of predicate 1
-- +   _preds_1[5] = _preds_1[1]
-- +   _vpreds_1[6] = _preds_1[1]
-- + else
-- +   _preds_1[6] = true
-- +   _vpreds_1[7] = true -- pred 6 known to be true
-- + end
create proc nested_shared_stuff()
begin
  with
  (call nested_shared_proc(1))
  select * from nested_shared_proc;
end;


-- TEST: nested select syntax with complex fragment
--
-- 10 fragments and 8 variables as expected
-- control flow corresponds to the nested selects (manually verified)
-- see discussion per fragment
-- +  local _preds_1  -- 10 possible fragments
-- +  local _vpreds_1 -- 8 possible bindings
-- +  _preds_1 = {}
-- +  _vpreds_1 = {}
-- +  _p1_x__ = 1
-- +  _preds_1[0] = true
-- +  _preds_1[1] = true
-- +  if _p1_x__ <= 5 then
-- +    _preds_1[2] = true
-- +    _p2_x_ = 1
-- +    if _p2_x_ == 1 then
-- +      _preds_1[3] = true
-- +      _vpreds_1[0] = true -- pred 3 known to be true
-- +    else
-- +      if _p2_x_ == 2 then
-- +        _preds_1[4] = true
-- +        _vpreds_1[1] = true -- pred 4 known to be true
-- +        _vpreds_1[2] = true -- pred 4 known to be true
-- +      else
-- +        _preds_1[5] = true
-- +        _vpreds_1[3] = true -- pred 5 known to be true
-- +        _vpreds_1[4] = true -- pred 5 known to be true
-- +        _vpreds_1[5] = true -- pred 5 known to be true
-- +      end
-- +    end
-- +    _preds_1[6] = _preds_1[2]
-- +    _vpreds_1[6] = _preds_1[2]
-- +  else
-- +    _preds_1[7] = true
-- +    _vpreds_1[7] = true -- pred 7 known to be true
-- +  end
-- +  _preds_1[8] = true
-- +  _preds_1[9] = true
-- +  _rc_, _result_stmt = cql_prepare_var(_db_,
-- +    10, _preds_1,
--
-- fragment 0 always present
-- +  "SELECT x FROM (",
--
-- fragment 1, the nested wrapper -- always present
-- +  "WITH _ns_(x) AS (",
--
-- fragment 2 present if x <= 5
-- +  "WITH shared_conditional (x) AS (",
--
-- fragment 3 present if x == 1
-- first variable binding v[0] = pred[3]
-- +  "SELECT ?",
--
-- fragment 4 present if x == 2
-- second variable binding v[1], v[2] = pred[4]
-- +  "SELECT ? + ?",
--
-- fragment 5 present if x == 3
-- third variable binding v[3], v[4], v[5] = pred[5]
-- +  "SELECT ? + ? + ?",
--
-- fragment 6 the tail of fragment 2, present if x <= 5
-- fourth variable binding v[6] = pred[6] = pred[2]
-- +  ") SELECT x FROM shared_conditional WHERE ? = 5",
--
-- fragment 7 present if x > 5
-- fifth variable binding v[7] = pred[7] = !pred[2]
-- +  "SELECT ?",
--
-- fragment 8 present always
-- +  ") SELECT * FROM _ns_",
--
-- fragment 9 present always
-- +  ")"
create proc use_nested_select_shared_frag_form()
begin
  select * from (call nested_shared_proc(1));
end;

-- TEST: in the nested select case we have to wrap the fragment text with a CTE
-- the column names are needed and the CTE does not provide them.  The _ns_ wrapper
-- accomplishes this.  We do it this way so that the text of the fragment is the same
-- if we are using nested select or not.
-- + "SELECT shared_something FROM (",
-- + "WITH _ns_(shared_something) AS (",
-- + "SELECT 1234",
-- + ") SELECT * FROM _ns_",
-- + ")"
@attribute(cql:private)
create proc simple_shared_frag()
begin
  select * from (call shared_frag());
end;

declare const group some_constants (
  const_u = false,
  const_w = 3.5,
  const_x = 1L,
  const_y = 2+3,
  const_z = "hello, world\n"
);

-- TEST: slash star and star slash safety
-- when we generate the comment for this proc we have to
-- nix the slash star and star slash or otherwise things will fail
-- in the generated C code:
--
--   - For star slash, not doing so will result in the comment
--     block ending prematurely, resulting in invalid C code that
--     won't compile.
--   - For slash star, there is a high chance that the compiler
--     will reject the generated code under certain configurations
--     (-Werror and -Wcomment flags).
create proc slash_star_and_star_slash()
begin
  let x := "/*  */";
end;

@emit_constants some_constants;

@attribute(cql:blob_storage)
create table structured_storage(
  id integer not null,
  name text not null
);

-- TEST: basic blob serialization case
-- + _rc_, B = cql_serialize_to_blob(C);
-- + _rc_, D = cql_deserialize_from_blob(B)
create proc blob_serialization_test()
begin
  declare C cursor for select 1 id, 'foo' name;
  fetch C;

  declare B blob<structured_storage>;

  set B from cursor C;

  declare D cursor like C;
  fetch D from B;
end;

declare function make_blob() create blob<structured_storage>;

-- TEST: get a blob from somewhere other than a local
-- checks general expression evaluation in the fetch path
-- func call is a good standing for general eval
-- + _tmp_n_blob_0 = make_blob()
-- + _rc_, C = cql_deserialize_from_blob(_tmp_n_blob_0)
create proc deserialize_func()
begin
  declare C cursor like structured_storage;
  fetch C from blob make_blob();
end;

-- TEST: ensure that the max constants are getting handled correctly
-- including the special cases to avoid compiler warnings.  Note that
-- this code has to compile correctly in C to pass the test also.  Run
-- time checks for this are in run_test.sql because this is subtle
--
-- + big1 = 0x7fffffffffffffff
-- + big2 = 0x8000000000000000
-- + big3 = (-9223372036854775807 - 1)
-- + big4 = (-9223372036854775807 - 1)
-- + big5 = 9223372036854775807
-- + big6 = 9223372036854775807
create proc bigstuff()
begin
  let big1 := 0x7fffffffffffffffL;
  let big2 := 0x8000000000000000L;
  let big3 := -9223372036854775808L;
  let big4 := -9223372036854775808;
  let big5 := 9223372036854775807L;
  let big6 := 9223372036854775807;
end;

declare const group big_constants(
  big_long_constants_max = 9223372036854775807,
  big_long_constants_min = -9223372036854775808,
  big_long_constants_almost_min = -9223372036854775807
);

@emit_constants big_constants;

-- TEST: variable group creates declarations only
-- group produces nothing in the main stream!
-- - row
-- - define
-- - extern
declare group var_group
begin
  declare gr_cursor cursor like select 1 x, "2" y;
  declare gr_integer integer;
  declare gr_blob_cursor cursor like structured_storage;
end;

-- TEST: emit group is a LUA no-op
@emit_group var_group;

-- TEST: use the global cursor for serialization
-- This sets the SERIALIZATION bit on the cursor causing it to emit more stuff
-- even though it's out of order the codegen will be affected
-- the test cases above verify this
-- _rc_, b = cql_serialize_to_blob(gr_blob_cursor);
create proc use_gr_cursor_for_serialization(out b blob<structured_storage>)
begin
  set b from cursor gr_blob_cursor;
end;

-- TEST: In C if we mutate a reference arg then we have to track its lifetime
-- we cannot just borrow the reference, the parameter is not released
-- nor can it be...  So convert this to a normal local pattern.
-- In LUA this is all trivial, because there are no borrowed refs.
-- + x = "hi"
create proc mutated_in_param_ref(x text)
begin
  set x := 'hi';
end;

-- TEST: likely() is correctly emitted
-- + "SELECT likely(1)"
set b2 := ( select likely(1) );

-- TEST: Declare an OUT proc that will be redeclared and then created to ensure
-- that struct redeclarations are appropriately guarded against (and, thus, the
-- result can be compiled successfully).
declare proc some_redeclared_out_proc() out (x int) using transaction;
declare proc some_redeclared_out_proc() out (x int) using transaction;
create proc some_redeclared_out_proc()
begin
  declare c cursor for select nullable(1) x;
  fetch c;
  out c;
end;

-- TEST: Declare an OUT UNION proc that will be redeclared and then created to
-- ensure that struct redeclarations are appropriately guarded against (and,
-- thus, the result can be compiled successfully).
declare proc some_redeclared_out_union_proc() out union (x int) using transaction;
declare proc some_redeclared_out_union_proc() out union (x int) using transaction;

create proc some_redeclared_out_union_proc()
begin
  declare c cursor for select nullable(1) x;
  fetch c;
  out union c;
end;

declare function external_cursor_func(x cursor) integer;

-- TEST call a function that takes a generic cursor
-- + result = external_cursor_func(shape_storage, shape_storage_types_, shape_storage_fields_)
create proc external_cursor_caller ()
begin
  declare shape_storage cursor like select 1 as x;
  let result := external_cursor_func(shape_storage);
end;

-- helper method that clobbers x (in out)
create proc clobber1(inout x text)
begin
  set x := "xyzzy";
end;

-- helper method that clobbers x (out arg)
create proc clobber2(out x text)
begin
  set x := "xyzzy";
end;

-- TEST: use of in arg at in/out position requires copy (in C)
-- in lua it means nothing... because in ref args are not borrowed
-- + x = clobber1(x)
create proc mutated_in_arg1(x text)
begin
  call clobber1(x);
end;

-- TEST: use of in arg at out position requires copy (in C)
-- in lua it means nothing... because in ref args are not borrowed
-- + x = clobber2()
create proc mutated_in_arg2(x text)
begin
  call clobber2(x);
end;

-- TEST: use of in arg for fetch into requires copy
-- we never have problems with in ref args gettng mutated in LUA
-- everything is gc'd.  In C output if ref args are borrowed
-- so if you change them you have to make a copy so you can release
-- whatever you put there. In lua you do nothing...
-- + function mutated_in_arg3(_db_, x)
-- + x = C.x
create proc mutated_in_arg3(x text)
begin
  declare C cursor for select "x" x;
  fetch C into x;
end;

-- TEST: make sure the not null contract works for mutated out variables
-- this is not even a thing in lua codegen because out variables are always local
-- + cql_contract_argument_notnull(x, 1)
-- + x = "xyzzy"
-- + return x
create proc mutated_not_null(inout x text not null)
begin
  set x := 'xyzzy';
end;

-- TEST: declaration of an unchecked select function
-- this is a no-op
declare select function no_check_select_fun no check text;

-- TEST: declaration of an unchecked table-valued select function
-- this is a no-op
declare select function no_check_select_table_valued_fun no check (t text);

-- for the next case
create proc simple_child_proc()
begin
  select 1 x, 2 y;
end;

-- TEST: emit getters and setters for a simple result set set type
-- note that LUA has no setters or getters nor does it need them
-- so we're basically verifying the child result set type only here
-- + function simple_container_proc_fetch_results(_db_)
-- + C._has_row_ = true
-- + C.a = 1
-- + C.b = 2
-- + _rc_, _tmp_object_0 = simple_child_proc_fetch_results(_db_)
-- + C.c = _tmp_object_0
-- + table.insert(_rows_, cql_clone_row(C))
@attribute(cql:emit_setters)
create proc simple_container_proc()
begin
  declare C cursor like (a integer, b integer not null, c object<simple_child_proc set>);
  fetch C using
     1 a,
     2 b,
     simple_child_proc() c;

  out union C;
end;

declare proc num_arg(x integer not null);

-- TEST: invoke num proc with a bool
-- + num_arg(1)
create proc call_num_with_bool()
begin
   call num_arg(true);
end;

-- TEST: switch label with max long constant
-- the constant must be composed of simple valid literals
-- + WHEN -9223372036854775808L THEN
-- + WHEN 9223372036854775807L THEN
-- + if _tmp_int64_0 == (-9223372036854775807 - 1) then
-- + if _tmp_int64_0 == 9223372036854775807 then
create proc big_switch_label(x long integer not null)
begin
  switch x
  when -9223372036854775808L then let y := 0;
  when 9223372036854775807 then let z := 1;
  end;
end;

-- we just have to successfully ignore these
-- there is no codegen, this is a semantic analysis thing
declare interface should_not_frag(id integer);

--------------------------------------------------------------------
-------------------- add new tests before this point ---------------
--------------------------------------------------------------------
let this_is_the_end := 0xf00d;

create proc end_proc() begin end;

-- TEST: end marker -- this is the last test
-- + local end_marker
declare end_marker integer;
--------------------------------------------------------------------

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
-- + i2 = 1;
set i2 := 1;

-- TEST: assign rhs not null
-- + cql_set_notnull(i1_nullable, 88);
set i1_nullable := 88;

-- remove the nullability improvement
-- + cql_set_null(i1_nullable);
set i1_nullable := null;

-- TEST: assign everything nullable
-- + cql_set_nullable(i0_nullable, i1_nullable.is_null, i1_nullable.value);
set i0_nullable := i1_nullable;

-- TEST: assign NULL to nullable string
-- + cql_set_string_ref(&t0_nullable, NULL);
set t0_nullable := null;

-- + cql_set_string_ref(&t0_nullable, t2);
set t0_nullable := t2;

-- remove the nullability improvement
-- + cql_set_string_ref(&t0_nullable, NULL);
set t0_nullable := null;

-- TEST: simple unary operators
-- + SET i2 := - -1;
-- + i2 = - - 1;
set i2 := - -1;

-- + cql_set_notnull(i0_nullable, - i2);
set i0_nullable := -i2;

-- + cql_set_null(i0_nullable);
set i0_nullable := -null;

-- + cql_set_nullable(i1_nullable, i0_nullable.is_null, - i0_nullable.value);
set i1_nullable := -i0_nullable;

-- + cql_set_notnull(r0_nullable, 2.2%);
set r0_nullable := 2.2;

-- remove the nullability improvement
-- + cql_set_null(r0_nullable);
set r0_nullable := null;

-- + r2 = 3.5%;
set r2 := 3.5;

-- + cql_set_nullable(_tmp_n_bool_1, i0_nullable.is_null, ! i0_nullable.value);
-- + cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, ! _tmp_n_bool_1.value);
-- + cql_set_nullable(i1_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i1_nullable := NOT NOT i0_nullable;

-- + i2 = ! ! b2;
set i2 := NOT NOT b2;

-- TEST: not null arithmetic
-- + i2 = 1 * 3 + 5;
set i2 := 1 * 3 + 5;

-- TEST: everything in sight is nullable
-- + cql_combine_nullables(r0_nullable, r0_nullable.is_null, i1_nullable.is_null, r0_nullable.value * i1_nullable.value);
set r0_nullable := r0_nullable * i1_nullable;

-- TEST: right operand is not null
-- + cql_set_nullable(r0_nullable, r0_nullable.is_null, r0_nullable.value * i2);
set r0_nullable := r0_nullable * i2;

-- TEST: left operand is not null
-- + cql_set_nullable(i0_nullable, i1_nullable.is_null, 12 * i1_nullable.value);
set i0_nullable := 12 * i1_nullable;

-- TEST: an operaand is actually null
-- + cql_set_null(i0_nullable);
set i0_nullable := null * i1_nullable;

-- TEST: make sure the stacking is working correctly
-- + cql_combine_nullables(_tmp_n_double_1, r0_nullable.is_null, i1_nullable.is_null, r0_nullable.value * i1_nullable.value);
-- + cql_combine_nullables(_tmp_n_double_2, r0_nullable.is_null, i1_nullable.is_null, r0_nullable.value * i1_nullable.value);
-- + cql_combine_nullables(r0_nullable, _tmp_n_double_1.is_null, _tmp_n_double_2.is_null, _tmp_n_double_1.value + _tmp_n_double_2.value);
set r0_nullable := r0_nullable * i1_nullable + r0_nullable * i1_nullable;

-- TEST: a more complex stacking example
-- + cql_combine_nullables(_tmp_n_double_2, r0_nullable.is_null, i1_nullable.is_null, r0_nullable.value * i1_nullable.value);
-- + cql_combine_nullables(_tmp_n_double_3, r0_nullable.is_null, i0_nullable.is_null, r0_nullable.value * i0_nullable.value);
-- + cql_combine_nullables(_tmp_n_double_1, _tmp_n_double_2.is_null, _tmp_n_double_3.is_null, _tmp_n_double_2.value + _tmp_n_double_3.value);
-- + cql_combine_nullables(r0_nullable, _tmp_n_double_1.is_null, r0_nullable.is_null, _tmp_n_double_1.value + r0_nullable.value);
set r0_nullable := (r0_nullable * i1_nullable + r0_nullable * i0_nullable) + r0_nullable;

-- TEST: string assignment -- nasty string
-- + cql_set_string_ref(&t2, _literal%This_is_a_test_);
set t2 := "This is a \" \\ test '' \n \" ";

-- TEST: call an external procedure (type not known)
-- + printf("Hello, world\n");
call printf("Hello, world\n");

-- TEST: logical AND with short circuit
-- + i2 = r2 && l2;
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
-- +  do {
-- +    _tmp_n_int_2 = side_effect1();
-- +    if (!_tmp_n_int_2.is_null) {
-- +      _tmp_int_1 = _tmp_n_int_2.value;
-- +      break;
-- +    }
-- +    _tmp_int_1 = 7;
-- +  } while (0);
-- +  if (!(_tmp_int_1)) {
-- +    _tmp_bool_0 = 0;
-- +  }
-- +  else {
-- +      _tmp_n_int_3 = side_effect2();
-- +      if (!_tmp_n_int_3.is_null) {
-- +        _tmp_int_2 = _tmp_n_int_3.value;
-- +        break;
-- +      }
-- +      _tmp_int_2 = 5;
-- +    } while (0);
-- +    _tmp_bool_0 = !!(_tmp_int_2);
-- +  }
-- +  i2 = _tmp_bool_0;
set i2 := coalesce(side_effect1(), 7) and coalesce(side_effect2(), 5);

-- TEST: trival NULL on AND
-- + cql_set_null(_tmp_n_bool_0);
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := NULL and NULL;

-- TEST: logical AND with nullables
-- + if (cql_is_nullable_false(i0_nullable.is_null, i0_nullable.value))
-- + if (cql_is_nullable_false(i1_nullable.is_null, i1_nullable.value))
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i0_nullable and i1_nullable;

-- TEST: logical AND with constant nulls
-- + if (cql_is_nullable_false(i1_nullable.is_null, i1_nullable.value))
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := NULL and i1_nullable;

-- TEST: logical AND with constant nulls
-- + if (cql_is_nullable_false(i0_nullable.is_null, i0_nullable.value))
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i0_nullable and NULL;

-- TEST: logical OR with short circuit
-- + i2 = r2 || l2;
set i2 := r2 or l2;

-- TEST: complex side effect, looks safe but it isn't because of codegen
-- +  do {
-- +    _tmp_n_int_2 = side_effect1();
-- +    if (!_tmp_n_int_2.is_null) {
-- +      _tmp_int_1 = _tmp_n_int_2.value;
-- +      break;
-- +    }
-- +    _tmp_int_1 = 7;
-- +  } while (0);
-- +  if (_tmp_int_1) {
-- +    _tmp_bool_0 = 1;
-- +  }
-- +  else {
-- +    do {
-- +      _tmp_n_int_3 = side_effect2();
-- +      if (!_tmp_n_int_3.is_null) {
-- +        _tmp_int_2 = _tmp_n_int_3.value;
-- +        break;
-- +      }
-- +      _tmp_int_2 = 5;
-- +    } while (0);
-- +    _tmp_bool_0 = !!(_tmp_int_2);
-- +  }
-- +  i2 = _tmp_bool_0;
set i2 := coalesce(side_effect1(), 7) or coalesce(side_effect2(), 5);

-- TEST: trival NULL on OR
-- + cql_set_null(_tmp_n_bool_0);
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := NULL or NULL;

-- TEST: logical OR with nullables
-- + cql_is_nullable_true(i0_nullable.is_null, i0_nullable.value)
-- + cql_is_nullable_true(i1_nullable.is_null, i1_nullable.value)
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i0_nullable or i1_nullable;

-- TEST: logical OR with constant nulls
-- + cql_is_nullable_true(i1_nullable.is_null, i1_nullable.value)
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := NULL or i1_nullable;

-- TEST: logical OR with constant nulls
-- + cql_is_nullable_true(i0_nullable.is_null, i0_nullable.value)
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i0_nullable or NULL;

-- TEST: is null basic test
-- + i2 = 1;
set i2 := null is null;

-- TEST: is null test general case
-- + cql_combine_nullables(_tmp_n_int_0, i0_nullable.is_null, i1_nullable.is_null, i0_nullable.value + i1_nullable.value);
-- + i2 = _tmp_n_int_0.is_null;
set i2 := (i0_nullable + i1_nullable) is null;

-- TEST: is not null basic test
-- + i2 = !1;
set i2 := null is not null;

-- TEST: is not null test general case
-- + cql_combine_nullables(_tmp_n_int_0, i0_nullable.is_null, i1_nullable.is_null, i0_nullable.value + i1_nullable.value);
-- + i2 = !_tmp_n_int_0.is_null;
set i2 := (i0_nullable + i1_nullable) is not null;

-- TEST: complex if/else pattern
-- Note: of interest because the embedded nullable
--       comparison requires statements to compute
-- + if (1) {
-- + i2 = 1;
-- + }
-- + else {
-- + cql_combine_nullables(_tmp_n_bool_0, i0_nullable.is_null, i1_nullable.is_null, i0_nullable.value == i1_nullable.value);
-- + if (cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) {
-- + i2 = 2;
-- + }
-- + else {
-- +  i2 = 3;
-- + }
-- + }
-- +4 {
-- +4 }
-- +2 else {
if 1 then
 set i2 := 1;
else if i0_nullable == i1_nullable then
 set i2 := 2;
else
 set i2 := 3;
end if;

-- TEST: complex if/else pattern, embedded logical operation
-- Note: of interest because the embedded logical requires
--       statements to compute
-- validating the bits are are unique to this construct
-- + if (cql_is_nullable_true(i0_nullable.is_null, i0_nullable.value)) {
-- + if (cql_is_nullable_true(i1_nullable.is_null, i1_nullable.value)) {
-- + if (cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) {
if 1 then
 set i2 := 1;
else if i0_nullable or i1_nullable then
 set i2 := 2;
else
 set i2 := 3;
end if;

-- TEST: simple procedure with external call
-- + void test(cql_int32 i) {
-- + if (i) {
-- + puts("true");
create procedure test(i integer not null)
begin
  if i then
    call puts('true');
  end if;
end;

-- TEST: guard statements are simply rewritten to if statements
-- + if (!a.is_null) {
-- + goto cql_cleanup; // return
-- + cql_set_nullable(x, a.is_null, a.value);
create proc proc_with_return_guard(a int)
begin
  if a is not null return;
  let x := a;
end;

-- TEST: simple between
-- + SET b2 := BETWEEN REWRITE _between_0_ := 1 CHECK (_between_0_ >= 0 AND _between_0_ <= 3);
-- + _between_0_ = 1;
-- + b2 = _between_0_ >= 0 && _between_0_ <= 3;
set b2 := 1 between 0 and 3;

-- TEST: between with some nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_1_ := i1_nullable CHECK (_between_1_ >= i0_nullable AND _between_1_ <= r2);
-- + cql_set_nullable(_between_1_, i1_nullable.is_null, i1_nullable.value);
-- + cql_combine_nullables(_tmp_n_bool_1, _between_1_.is_null, i0_nullable.is_null, _between_1_.value >= i0_nullable.value);
-- + if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_set_nullable(_tmp_n_bool_2, _between_1_.is_null, _between_1_.value <= r2);
-- +   if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable between i0_nullable and r2;

-- TEST: between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_2_ := i1_nullable CHECK (_between_2_ >= r2 AND _between_2_ <= i0_nullable);
-- + cql_set_nullable(_between_2_, i1_nullable.is_null, i1_nullable.value);
-- + cql_set_nullable(_tmp_n_bool_1, _between_2_.is_null, _between_2_.value >= r2);
-- + if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_2, _between_2_.is_null, i0_nullable.is_null, _between_2_.value <= i0_nullable.value);
-- +   if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable between r2 and i0_nullable;

-- TEST: simple not between
-- + SET b2 := BETWEEN REWRITE _between_3_ := 1 CHECK (_between_3_ < 0 OR _between_3_ > 3);
-- + _between_3_ = 1;
-- + b2 = _between_3_ < 0 || _between_3_ > 3;
set b2 := 1 not between 0 and 3;

-- TEST: not between with some nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_4_ := i1_nullable CHECK (_between_4_ < i0_nullable OR _between_4_ > r2);
-- + cql_set_nullable(_between_4_, i1_nullable.is_null, i1_nullable.value);
-- + cql_combine_nullables(_tmp_n_bool_1, _between_4_.is_null, i0_nullable.is_null, _between_4_.value < i0_nullable.value);
-- + if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_set_nullable(_tmp_n_bool_2, _between_4_.is_null, _between_4_.value > r2);
-- +   if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable not between i0_nullable and r2;

-- TEST: not between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_5_ := i1_nullable CHECK (_between_5_ < r2 OR _between_5_ > i0_nullable);
-- + cql_set_nullable(_between_5_, i1_nullable.is_null, i1_nullable.value);
-- + cql_set_nullable(_tmp_n_bool_1, _between_5_.is_null, _between_5_.value < r2);
-- + if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_2, _between_5_.is_null, i0_nullable.is_null, _between_5_.value > i0_nullable.value);
-- +   if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable not between r2 and i0_nullable;

-- TEST: out parameter test
-- + void out_test(cql_int32 *_Nonnull i, cql_nullable_int32 *_Nonnull ii) {
-- + *i = i2;
-- + cql_set_nullable(*ii, i0_nullable.is_null, i0_nullable.value);
-- + }
create procedure out_test(out i integer not null, out ii integer)
begin
  set i := i2;
  set ii := i0_nullable;
end;

-- TEST: force a cql_int64 variable to be pushed on the scratch stack
-- + cql_nullable_int64 longint_var = { .is_null = 1 };
declare longint_var long integer;

-- + cql_combine_nullables(_tmp_n_int64_1, l0_nullable.is_null, l1_nullable.is_null, l0_nullable.value + l1_nullable.value);
-- + cql_set_nullable(longint_var, _tmp_n_int64_1.is_null, _tmp_n_int64_1.value * 5);
set longint_var := (l0_nullable + l1_nullable) * 5;

-- TEST: make a cursor
-- + _rc_ = cql_prepare(_db_, &foo_cursor_stmt,
-- + "SELECT id, ? "
-- + "FROM foo "
-- + "WHERE id = ?"
-- + cql_multibind(&_rc_, _db_, &foo_cursor_stmt, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, i2,
-- +               CQL_DATA_TYPE_INT32, &i0_nullable);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
declare foo_cursor cursor for select id, i2 from foo where id = i0_nullable;

-- TEST: fetch a cursor
-- + _rc_ = sqlite3_step(foo_cursor_stmt);
-- + _rc_ = sqlite3_step(foo_cursor_stmt);
-- + _foo_cursor_has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, foo_cursor_stmt, 2,
-- +                CQL_DATA_TYPE_INT32, &i0_nullable,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &i2);
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
fetch foo_cursor into i0_nullable, i2;

-- TEST: test elementary cursor on select with no tables, still round trips through sqlite
declare col1 integer;
declare col2 real not null;
-- + _rc_ = cql_prepare(_db_, &basic_cursor_stmt,
declare basic_cursor cursor for select 1, 2.5;
-- + cql_multifetch(_rc_, basic_cursor_stmt, 2,
-- +                CQL_DATA_TYPE_INT32, &col1,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_DOUBLE, &col2);
fetch basic_cursor into col1, col2;
-- + cql_finalize_stmt(&basic_cursor_stmt);
close basic_cursor;

-- TEST: the most expensive way to swap two variables ever :)
declare arg1 integer not null;
declare arg2 integer not null;
set arg1 := 7;
set arg2 := 11;
-- + _rc_ = cql_prepare(_db_, &exchange_cursor_stmt,
-- + cql_multibind(&_rc_, _db_, &exchange_cursor_stmt, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, arg2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, arg1);
declare exchange_cursor cursor for select arg2, arg1;
-- + cql_multifetch(_rc_, exchange_cursor_stmt, 2,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &arg1,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &arg2);
fetch exchange_cursor into arg1, arg2;
-- + cql_finalize_stmt(&exchange_cursor_stmt);
close exchange_cursor;

-- TEST: simple nested select
-- + _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT ? + 1"
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 1,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, i2);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
-- + _rc_ = sqlite3_step(_temp_stmt);
-- + if (_rc_ != SQLITE_ROW) { cql_error_trace(); goto cql_cleanup; }
-- + i2 = sqlite3_column_int(_temp_stmt, 0);
-- + cql_finalize_stmt(&_temp_stmt);
set i2 := (select i2+1);

-- TEST: nested select with nullable
-- validate just the different bit
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 1,
-- +               CQL_DATA_TYPE_INT32, &i0_nullable);
set i0_nullable := (select i0_nullable+1);

-- TEST: tricky quoted text
-- this validates that the C escaping works right when making SQL
-- + "DELETE FROM bar WHERE name LIKE '\\\\ \" \\n'"
delete from bar where name like '\\ " \n';

-- TEST: binding an out parameter
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 1,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, *foo);
create procedure outparm_test(out foo integer not null)
begin
 set foo := 1;
 delete from bar where id = foo;
end;

-- TEST: a simple stored proc that throws
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto catch_start_1; }
-- + goto catch_end_1;
-- + catch_start_1: {
-- + printf("error\n");
-- + _rc_ = cql_best_error(_rc_thrown_1);
-- + goto cql_cleanup;
-- + catch_end_1:
-- + _rc_ = SQLITE_OK;
-- + cql_cleanup:
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
-- + do {
-- +  if (1) {
-- +   i2 = 100;
-- +   break;
-- +  }
-- +  if (2) {
-- +   i2 = 200;
-- +   break;
-- +  }
-- +  i2 = 300;
-- + } while (0);
set i2 := case when 1 then 100 when 2 then 200 when null then 500 else 300 end;

-- TEST: a simple in expression
-- + do {
-- +  _tmp_int_% = 3;
-- +  _tmp_bool_0 = 1;
-- +  if (_tmp_int_% == 1) break;
-- +  if (_tmp_int_% == 2) break;
-- +  if (_tmp_int_% == 4) break;
-- +  _tmp_bool_0 = 0;
-- + } while (0);
-- + i2 = _tmp_bool_0;
set i2 := 3 in (1, 2, null, 4);

-- TEST: in with nullables
-- + do {
-- +  cql_set_nullable(_tmp_n_int_%, i1_nullable.is_null, i1_nullable.value);
-- +  if (_tmp_n_int_%.is_null) {
-- +    cql_set_null(_tmp_n_bool_0);
-- +    break;
-- +  }
-- +  cql_set_notnull(_tmp_n_bool_0, 1);
-- +  if (_tmp_n_int_%.value == 1) break;
-- +  if (_tmp_n_int_%.value == 2) break;
-- +  if (cql_is_nullable_true(b0_nullable.is_null, _tmp_n_int_%.value == b0_nullable.value)) break;
-- +  cql_set_notnull(_tmp_n_bool_0, 0);
-- + } while (0);
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable in (1, 2, null, b0_nullable);

-- TEST: a simple not in expression
-- + do {
-- +  _tmp_int_% = 3;
-- +  _tmp_bool_0 = 0;
-- +  if (_tmp_int_% == 1) break;
-- +  if (_tmp_int_% == 2) break;
-- +  if (_tmp_int_% == 4) break;
-- +  _tmp_bool_0 = 1;
-- + } while (0);
-- + i2 = _tmp_bool_0;
set i2 := 3 not in (1, 2, null, 4);

-- TEST: not in with nullables
-- + do {
-- +  cql_set_nullable(_tmp_n_int_%, i1_nullable.is_null, i1_nullable.value);
-- +  if (_tmp_n_int_%.is_null) {
-- +    cql_set_null(_tmp_n_bool_0);
-- +    break;
-- +  }
-- +  cql_set_notnull(_tmp_n_bool_0, 0);
-- +  if (_tmp_n_int_%.value == 1) break;
-- +  if (_tmp_n_int_%.value == 2) break;
-- +  if (cql_is_nullable_true(b0_nullable.is_null, _tmp_n_int_%.value == b0_nullable.value)) break;
-- +  cql_set_notnull(_tmp_n_bool_0, 1);
-- + } while (0);
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable not in (1, 2, null, b0_nullable);

-- TEST: between with strings
-- + SET b2 := BETWEEN REWRITE _between_6_ := 'b' CHECK (_between_6_ >= 'a' AND _between_6_ <= 'c');
-- + cql_set_string_ref(&_between_6_, _literal_%_b_);
-- + b2 = cql_string_compare(_between_6_, _literal_%_a_) >= 0 && cql_string_compare(_between_6_, _literal_%_c_) <= 0;
set b2 := 'b' between 'a' and 'c';

-- TEST: between with nullable strings right
-- + SET b0_nullable := BETWEEN REWRITE _between_7_ := 'b' CHECK (_between_7_ >= 'a' AND _between_7_ <= t0_nullable);
-- + cql_set_string_ref(&_between_7_, _literal_%_b_);
-- + if (!(cql_string_compare(_between_7_, _literal_%_a_) >= 0)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_2, !_between_7_, !t0_nullable, cql_string_compare(_between_7_, t0_nullable) <= 0);
-- +   if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between 'a' and t0_nullable;

-- TEST: between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_8_ := 'b' CHECK (_between_8_ >= t0_nullable AND _between_8_ <= 'c');
-- + cql_set_string_ref(&_between_8_, _literal_%_b_);
-- + cql_combine_nullables(_tmp_n_bool_1, !_between_8_, !t0_nullable, cql_string_compare(_between_8_, t0_nullable) >= 0);
-- + if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   if (!(cql_string_compare(_between_8_, _literal_%_c_) <= 0)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between t0_nullable and 'c';

-- TEST: between with nullable strings null operand
-- + SET b0_nullable := BETWEEN REWRITE _between_9_ := 'b' CHECK (_between_9_ >= NULL AND _between_9_ <= 'c');
-- + cql_set_string_ref(&_between_9_, _literal_%_b_);
-- + cql_set_null(_tmp_n_bool_1);
-- + if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   if (!(cql_string_compare(_between_9_, _literal_%_c_) <= 0)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between null and 'c';

-- TEST: not between with strings
-- + SET b2 := BETWEEN REWRITE _between_10_ := 'b' CHECK (_between_10_ < 'a' OR _between_10_ > 'c');
-- + cql_set_string_ref(&_between_10_, _literal_%_b_);
-- + b2 = cql_string_compare(_between_10_, _literal_%_a_) < 0 || cql_string_compare(_between_10_, _literal_%_c_) > 0;
set b2 := 'b' not between 'a' and 'c';

-- TEST: not between with nullable strings right
-- + SET b0_nullable := BETWEEN REWRITE _between_11_ := 'b' CHECK (_between_11_ < 'a' OR _between_11_ > t0_nullable);
-- + cql_set_string_ref(&_between_11_, _literal_%_b_);
-- + if (cql_string_compare(_between_11_, _literal_%_a_) < 0) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_2, !_between_11_, !t0_nullable, cql_string_compare(_between_11_, t0_nullable) > 0);
-- +   if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +      cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between 'a' and t0_nullable;

-- TEST: not between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_12_ := 'b' CHECK (_between_12_ < t0_nullable OR _between_12_ > 'c');
-- + cql_set_string_ref(&_between_12_, _literal_%_b_);
-- + cql_combine_nullables(_tmp_n_bool_1, !_between_12_, !t0_nullable, cql_string_compare(_between_12_, t0_nullable) < 0);
-- + if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   if (cql_string_compare(_between_12_, _literal_%_c_) > 0) {
-- +      cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between t0_nullable and 'c';

-- TEST: not between with nullable strings null operand
-- verify rewrite
-- + SET b0_nullable := BETWEEN REWRITE _between_13_ := 'b' CHECK (_between_13_ < NULL OR _between_13_ > 'c');
-- + cql_set_string_ref(&_between_%, _literal_%_b_);
-- + cql_set_null(_tmp_n_bool_1);
-- + if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   if (cql_string_compare(_between_%_, _literal_%_c_) > 0) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between null and 'c';

-- TEST: this procedure will have a structured semantic type
-- + cql_string_proc_name(with_result_set_stored_procedure_name, "with_result_set");
-- + cql_code with_result_set(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt) {
-- + #define with_result_set_refs_offset cql_offsetof(with_result_set_row, name) // count = 1
-- + cql_int32 with_result_set_get_id(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_string_ref _Nullable with_result_set_get_name(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_bool with_result_set_get_rate_is_null(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int64 with_result_set_get_rate_value(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 with_result_set_get_type_value(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 with_result_set_result_count(with_result_set_result_set_ref _Nonnull result_set) {
-- + CQL_WARN_UNUSED cql_code with_result_set_fetch_results(sqlite3 *_Nonnull _db_, with_result_set_result_set_ref _Nullable *_Nonnull result_set) {
-- + if (_rc_ == SQLITE_OK && !*_result_stmt) _rc_ = cql_no_rows_stmt(_db_, _result_stmt);
create procedure with_result_set()
begin
  select * from bar;
end;

-- TEST: grabs values from a view that is backed by a table
-- + cql_code select_from_view(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt) {
-- - .refs_count = 0,
-- - .refs_offset = 0,
-- + cql_int32 select_from_view_get_id(select_from_view_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_bool select_from_view_get_type_is_null(select_from_view_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 select_from_view_get_type_value(select_from_view_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 select_from_view_result_count(select_from_view_result_set_ref _Nonnull result_set) {
-- + CQL_WARN_UNUSED cql_code select_from_view_fetch_results(sqlite3 *_Nonnull _db_, select_from_view_result_set_ref _Nullable *_Nonnull result_set) {
create proc select_from_view()
begin
  select id, type from baz;
end;

-- TEST: create dml for a view
-- +  "CREATE VIEW MyView AS "
-- +  "SELECT 1 AS f1, 2 AS f2, 3 AS f3"
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
-- + cql_code get_data(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt, cql_string_ref _Nonnull name_, cql_int32 id_) {
create procedure get_data(name_ text not null, id_ integer not null)
begin
  select * from bar where id = id_ and name = name_;
end;

-- TEST: create a proc that uses the new cursor fetch strategy
--       then bind values from those implicit variables in a CQL statement
--       and also bind the _has_rows auto local as well
-- validate auto variable management
-- + #define easy_fetch_C_refs_offset cql_offsetof(easy_fetch_C_row, name) // count = 1
-- + easy_fetch_C_row C = { ._refs_count_ = 1, ._refs_offset_ = easy_fetch_C_refs_offset };
-- + sqlite3_stmt *C2_stmt = NULL;
-- note that C2 is never fetched and therefore has no has_row, we don't want to generate
-- this variable because with good warnings it will create an unused variable error
-- - _C2_has_row_
-- + C._has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, C_stmt, 5,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.id,
-- +                CQL_DATA_TYPE_STRING, &C.name,
-- +                CQL_DATA_TYPE_INT64, &C.rate,
-- +                CQL_DATA_TYPE_INT32, &C.type,
-- +                CQL_DATA_TYPE_DOUBLE, &C.size);
-- + cql_multibind(&_rc_, _db_, &C2_stmt, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_BOOL, C._has_row_,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, C.id);
-- + cql_finalize_stmt(&C_stmt);
-- + cql_teardown_row(C);
-- + cql_finalize_stmt(&C2_stmt);
@attribute(cql:vault_sensitive)
create proc easy_fetch()
begin
  declare C cursor for select * from bar;
  fetch C;
  call printf("%d %s\n", C.id, C.name);
  declare C2 cursor for select * from bar where C and id = C.id;
end;

-- TEST: safe not nullable assignment
-- + i2 = i0_nullable.value;
-- + i2 = 3;
set i2 := ifnull(i0_nullable, 3);

-- TEST: this works too, but the result might be nullable
-- + do {
-- +  if (!i0_nullable.is_null) {
-- +    cql_set_notnull(i0_nullable, i0_nullable.value);
-- +    break;
-- +  }
-- +  cql_set_nullable(i0_nullable, i1_nullable.is_null, i1_nullable.value);
-- + } while (0);
set i0_nullable := ifnull(i0_nullable, i1_nullable);

-- TEST: create a proc that takes a nullable int and pass it a nullable int
--       this forces the case where the variable for the int has to be
--       reconstituted from the .value field
create proc copy_int(a int, out b int)
begin
  set b := a;
end;

-- + copy_int(i0_nullable, &i1_nullable);
call copy_int(i0_nullable, i1_nullable);

-- TEST: try out last_insert_rowid()
-- + cql_set_notnull(row, sqlite3_last_insert_rowid(_db_));
-- - cql_cleanup
create proc insert_rowid_reader()
begin
  declare row long integer;
  set row := last_insert_rowid();
end;

-- TEST: try out changes()
-- + cql_set_notnull(ct, sqlite3_changes(_db_));
-- - cql_cleanup
create proc changes_reader()
begin
  declare ct integer;
  set ct := changes();
end;

-- TEST: try out printf expression
declare s text not null;
-- + _printf_result = sqlite3_mprintf("%d and %d", 1, 2);
-- + cql_string_release(s);
-- + s = cql_string_ref_new(_printf_result);
set s := printf('%d and %d', 1, 2);

-- + _printf_result = sqlite3_mprintf("%d and %d", 3, 4)
-- + cql_string_release(s);
-- + s = cql_string_ref_new(_printf_result);
set s := printf('%d and %d', 3, 4);

-- TEST: printf inserts casts for numeric types (but only as needed)
-- + sqlite3_mprintf("%lld %lld %lld %llu %d %d %llu %d %f %f %s %f", ((cql_int64)(4)), _tmp_n_int64_%.value,
-- + ((cql_int64)!!(1)), _64(0), ((cql_int32)!!(0)), 0, _64(6), 7, 0.0, 0.0, NULL, ((cql_double)(8)));
set s := printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 4, nullable(5), true, null, false, null, 6L, 7, 0.0, null, null, 8);

-- TEST: printf doesn't insert casts when used in SQL
-- + SELECT printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 5, 5, 1, NULL, 0, NULL, 6, 7, 0.0, NULL, NULL, 8)
set s := (select printf('%lld %lld %lld %llu %d %d %llu %d %f %f %s %f', 5, nullable(5), true, null, false, null, 6L, 7, 0.0, null, null, 8));

-- TEST: make sure that we use the canonical name for 's' in codegen not 'S'.  Even though S is legal.
-- + cql_set_string_ref(&s, _literal%x_);
set S := 'x';

-- TEST: declare proc and call it
-- + /*
-- + DECLARE PROC xyzzy (id INTEGER) (A INTEGER NOT NULL);
-- + */
declare proc xyzzy(id integer) ( A integer not null );

-- + _rc_ = xyzzy(_db_, &xyzzy_cursor_stmt, _tmp_n_int_%);
-- +  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
create proc xyzzy_test()
begin
  declare xyzzy_cursor cursor for call xyzzy(1);
end;

-- TEST: declare a simple proc with no dml
-- + /*
-- + DECLARE PROC plugh (id INTEGER);
-- + */
declare proc plugh(id integer);

-- TEST: create a proc that returns a mix of possible types
--       in a select
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
create proc union_select()
begin
 select 1 as A union select 2 as A;
end;

-- TEST: create a proc with a compound select union all form
create proc union_all_select()
begin
 select 1 as A union all select 2 as A;
end;

-- TEST: create a valid union using not null columns and nullable matching
create proc union_all_with_nullable()
begin
  select nullable('foo') as name
  union all
  select name from bar;
end;

-- TEST: create a simple with statement
create proc with_stmt_using_cursor()
begin
  declare C cursor for
    with X(a,b,c) as (select 1,2,3)
    select * from X;
  fetch C;
end;

-- TEST: with statement top level
create proc with_stmt()
begin
  with X(a,b,c) as (select 1,2,3) select * from X;
end;

-- TEST: with recursive statement top level
create proc with_recursive_stmt()
begin
  with recursive X(a,b,c) as (select 1,2,3 union all select 4,5,6) select * from X;
end;

-- TEST: parent procedure
create proc parent_proc()
begin
  select 1 as one, 2 as two, 3 as three;
end;

-- TEST: child procedure
create proc parent_proc_child()
begin
  select 4 as four, 5 as five, 6 as six;
end;

-- TEST: fetch nullable output parameter
-- + _C_has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, C_stmt, 1,
-- +                CQL_DATA_TYPE_INT32, output);
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + *result = _C_has_row_;
create proc outint_nullable(out output integer, out result bool not null)
begin
  declare C cursor for select 1;
  fetch C into output;
  set result := C;
END;

-- TEST: fetch not null output parameter
-- + cql_bool _C_has_row_ = 0;
-- + _C_has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, C_stmt, 1,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, output);
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + *result = _C_has_row_;
create proc outint_notnull(out output integer not null, out result bool not null)
begin
  declare C cursor for select 1;
  fetch C into output;
  set result := C;
END;

declare function simple_func(int1 integer) integer;

-- TEST: call external function
-- + cql_set_notnull(_tmp_n_int_%, 2);
-- + result = simple_func(_tmp_n_int_%);
let result := simple_func(2);

-- TEST: call external function
-- + cql_set_notnull(_tmp_n_int_2, 1);
-- + _tmp_n_int_1 = simple_func(_tmp_n_int_2);
-- + result = simple_func(_tmp_n_int_1);
set result := simple_func(simple_func(1));

declare function text_func(int1 integer, int2 integer not null) text not null;
declare text_result text;

-- TEST: call external text function
-- + cql_set_notnull(_tmp_n_int_%, 123);
-- + cql_set_string_ref(&_tmp_text_0, text_func(_tmp_n_int_%, 456));
-- + cql_set_string_ref(&text_result, _tmp_text_0);
set text_result := text_func(123, 456);

-- TEST: create object variable
-- + cql_object_ref obj_var = NULL;
declare obj_var object;

-- TEST: assign null to object variable
-- + cql_set_object_ref(&obj_var, NULL);
set obj_var := null;

-- TEST: declare not null object
-- + cql_object_ref obj_var2 = NULL;
declare obj_var2 object not null;

declare function obj_notnull_func() object not null;

-- initialize for later use
set obj_var2 := obj_notnull_func();

-- TEST: assign var to object variable
-- + cql_set_object_ref(&obj_var, obj_var2);
set obj_var := obj_var2;

-- remove the nullability improvement
-- + cql_set_object_ref(&obj_var, NULL);
set obj_var := null;

-- TEST: object comparison
-- +  cql_combine_nullables(b0_nullable, !obj_var, !obj_var, obj_var == obj_var);
set b0_nullable := obj_var == obj_var;

-- TEST: object variable in IN clause
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
set b0_nullable := obj_var in (obj_var, obj_var);

-- TEST: object variable in IN clause
-- + if (_tmp_object_% == obj_var2) break;
set b2 := obj_var2 in (obj_var2, obj_var2);

-- TEST: object variable in NOT IN clause
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
set b0_nullable := obj_var not in (obj_var, obj_var);

-- TEST: object variable in NOT IN clause
-- + if (_tmp_object_% == obj_var2) break;
set b2 := obj_var2 not in (obj_var2, obj_var2);

-- TEST: proc with object args
-- + void obj_proc(cql_object_ref _Nullable *_Nonnull an_object)
create proc obj_proc(out an_object object)
begin
  set an_object := null;
end;

-- TEST: cursor with object in it
-- + cursor_with_object(object_, row);
-- + void cursor_with_object(cql_object_ref _Nullable object_, cursor_with_object_row *_Nonnull _result_)
create proc cursor_with_object(object_ Object)
begin
  declare C cursor like cursor_with_object arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: case statement with objects
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_1 == obj_var))
set i2 := case obj_var when obj_var then 1 else 2 end;

-- TEST: case statement with returning objects
-- + cql_set_object_ref(&obj_var, obj_var2);
-- + cql_set_object_ref(&obj_var, NULL);
set obj_var := case 1 when 1 then obj_var2 else null end;

declare function obj_func() object;

-- TEST: function invocation with object function
-- + cql_set_object_ref(&obj_var, obj_func());
set obj_var := obj_func();

declare function obj_func_create() create object;

-- TEST: function invocation with creater object function
-- +  cql_object_release(obj_var);
-- +  obj_var = obj_func_create();
set obj_var := obj_func_create();

declare function text_func_create() create text;

-- TEST: function invocation with creater text function
-- cql_string_release(_tmp_n_text_0);
-- _tmp_n_text_0 = text_func_create();
set text_result := text_func_create();

-- TEST: assign nullable to object with helper or crash
-- + cql_set_object_ref(&_tmp_n_object_0, obj_func());
-- + cql_invariant(!!_tmp_n_object_0);
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := ifnull_crash(obj_func());

-- TEST: assign nullable to object with helper or crash (ifnull_crash synonym)
-- + cql_set_object_ref(&_tmp_n_object_0, obj_func());
-- + cql_invariant(!!_tmp_n_object_0);
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := ifnull_crash(obj_func());

-- TEST: assign nullable to object with helper or throw
-- + cql_set_object_ref(&_tmp_n_object_0, obj_func());
-- + if (!_tmp_n_object_0) {
-- +   _rc_ = SQLITE_ERROR;
-- +   goto cql_cleanup;
-- + }
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := ifnull_throw(obj_func());

-- TEST: assign nullable to object with helper or crash
-- + cql_object_release(_tmp_n_object_0);
-- + _tmp_n_object_0 = obj_func_create();
-- + cql_invariant(!!_tmp_n_object_0);
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := ifnull_crash(obj_func_create());

-- TEST: assign nullable int to an integer
-- + cql_invariant(!i0_nullable.is_null);
-- + i2 = i0_nullable.value
set i2 := ifnull_crash(i0_nullable);

-- TEST: assign nullable int to an integer or throw
-- + if (i0_nullable.is_null) {
-- +   _rc_ = SQLITE_ERROR;
-- +   goto cql_cleanup;
-- + }
-- + i2 = i0_nullable.value;
set i2 := ifnull_throw(i0_nullable);

-- TEST: unused temp in unary not emitted
-- - cql_int32 _tmp_int_0 = 0;
-- - cql_int32 _tmp_int_1 = 0;
-- + o = i.value;
-- + o = - 1;
create proc unused_temp(i integer, out o integer not null)
begin
  set o := coalesce(i, -1);
end;

-- TEST: echo something to the output
-- + Garbonzo
-- + chick pea
@echo c, "int Garbonzo; // a chick pea\n";

-- TEST: echo all the escape characters that are supported
-- + //
-- + '%'
@echo c, "//\/'\a\b\f\t\v'\r\n";

-- TEST: echo inside a procedure
-- + void echo_test(void) {
-- + cql_set_string_ref(&s, _literal%before_echo_%);
-- + #define ECHO_TEST 1
-- + cql_set_string_ref(&s, _literal%after_echo_%);
create proc echo_test()
begin
  declare s text;
  SET s := "before echo";
  @echo c, "#define ECHO_TEST 1\n";
  SET s := "after echo";
end;

-- TEST: insert or replace form
-- +  "INSERT OR REPLACE INTO bar(id, type) VALUES(1, 5)"
insert or replace into bar(id, type) values (1,5);

-- TEST: insert default from
-- +  "INSERT INTO foo DEFAULT VALUES"
insert into foo default values;

-- TEST: insert from stored procedure
-- + "INSERT INTO bar(id, type) VALUES(?, ?)"
-- + cql_code insert_values(sqlite3 *_Nonnull _db_, cql_int32 id_, cql_nullable_int32 type_) {
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
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
-- + _rc_ = SQLITE_OK;
create proc drop_table_test()
begin
  drop table if exists bar;
end;

-- TEST: use a procedure to get a result set
-- + cql_code uses_proc_for_result(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt)
-- + *_result_stmt = NULL;
-- + _rc_ = with_result_set(_db_, _result_stmt);
-- +1 cql_finalize_stmt(_result_stmt);
create procedure uses_proc_for_result()
begin
  call with_result_set();
end;

-- TEST: declare a void func
declare function voidfunc() integer;

-- TEST: use a select exists clause
-- + "SELECT EXISTS (SELECT * "
-- + "FROM bar)"
set b2 := (select exists(select * from bar));

-- TEST: for expand of select * columns from whole result
-- + _rc_ = cql_prepare(_db_, &expanded_select_stmt,
-- + "SELECT id, name, rate, type, size "
-- + "FROM bar"
declare expanded_select cursor for select * from bar;

-- TEST: for expand of select * columns from table
-- + "SELECT bar.id, bar.name, bar.rate, bar.type, bar.size "
-- + "FROM bar"
declare table_expanded_select cursor for select bar.* from bar;

-- TEST: use a long literal
-- + l2 = _64(3147483647);
set l2 := 3147483647L;

-- TEST: use a long literal
-- + l2 = _64(3147483647);
set l2 := 3147483647;

-- TEST: use drop index in a proc
-- + "DROP INDEX index_1"
create proc index_dropper()
begin
  drop index index_1;
end;

-- TEST: simple DML statements for json_schema cg
-- +2 "INSERT INTO foo(id) VALUES(NULL)"
-- + "UPDATE bar "
-- + "SET name = 'bar' "
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
-- + _seed_ = 123;
-- + "INSERT INTO bar(id, name, rate, type, size) VALUES(?, printf('name_%d', ?), ?, ?, ?)"
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 5,
-- +4              CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, _seed_,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, _seed_);
create proc dummy_user()
begin
  insert into bar () values () @dummy_seed(123) @dummy_nullables @dummy_defaults;
end;

create proc proc_with_out_arg(out foo text)
begin
  set foo := 'x';
end;

-- TEST: the incoming arg must be nulled
--  when we call proc_with_out_arg we have to release the out arg before we call it or leak
-- (note: run tests verify this too)
-- bar is local and it is forced to null to start
-- + cql_string_ref bar = NULL;
-- foo is the out arg, we clobber it to a safe value
-- + *(void **)foo = NULL;
-- foo is set to something useful
-- + cql_set_string_ref(&*foo, _literal_%x_%);
-- we have to release the something useful before we make the call
-- + cql_set_string_ref(&*foo, NULL);
-- + proc_with_out_arg(foo);
-- we have to release bar before we make the call
-- + cql_set_string_ref(&bar, NULL);
-- + proc_with_out_arg(&bar);
create proc calls_out_proc(out foo text)
begin
  set foo := 'x';
  declare bar text;
  call proc_with_out_arg(foo);
  call proc_with_out_arg(bar);
end;

-- TEST: create blob variable
-- + cql_blob_ref blob_var = NULL;
declare blob_var blob;

-- TEST: create blob variable2
-- + cql_blob_ref blob_var2 = NULL;
declare blob_var2 blob not null;

declare function blob_notnull_func() blob not null;

-- initialize for later use
set blob_var2 := blob_notnull_func();

-- TEST: assign null to blob variable
-- + cql_set_blob_ref(&blob_var, NULL);
set blob_var := null;

-- TEST: assign var to blob variable
-- + cql_set_blob_ref(&blob_var, blob_var2);
set blob_var := blob_var2;

-- remove the nullability improvement
-- + cql_set_blob_ref(&blob_var, NULL);
set blob_var := null;

-- TEST: blob comparison "=="
-- + cql_combine_nullables(b0_nullable, !blob_var, !blob_var, cql_blob_equal(blob_var, blob_var));
set b0_nullable := blob_var == blob_var;

-- TEST: blob comparison "IS" NULL
-- + cql_set_notnull(b0_nullable, !blob_var);
set b0_nullable := blob_var IS null;

-- TEST: blob comparison "!="
-- + cql_combine_nullables(b0_nullable, !blob_var, !blob_var, !cql_blob_equal(blob_var, blob_var));
set b0_nullable := blob_var != blob_var;

-- TEST: blob comparison "<>"
-- + cql_combine_nullables(b0_nullable, !blob_var, !blob_var, !cql_blob_equal(blob_var, blob_var));
set b0_nullable := blob_var <> blob_var;

-- TEST: blob comparison "IS"
-- + cql_set_notnull(b0_nullable, cql_blob_equal(blob_var, blob_var));
set b0_nullable := blob_var IS blob_var;

-- TEST: blob comparison "IS NOT"
-- + cql_set_notnull(b0_nullable, !cql_blob_equal(blob_var, blob_var));
set b0_nullable := blob_var IS NOT blob_var;

-- TEST: blob variable in IN clause
-- + cql_set_notnull(_tmp_n_bool_0, 1);
-- + if (cql_blob_equal(_tmp_n_blob_%, blob_var)) break;
-- + if (cql_blob_equal(_tmp_n_blob_%, blob_var)) break;
-- + cql_set_notnull(_tmp_n_bool_0, 0);
set b0_nullable := blob_var in (blob_var, blob_var);

-- TEST: blob variable in IN clause
-- + _tmp_bool_0 = 1;
-- + if (cql_blob_equal(_tmp_blob_%, blob_var)) break;
-- + if (cql_blob_equal(_tmp_blob_%, blob_var2)) break;
-- + _tmp_bool_0 = 0;
set b2 := blob_var2 in (blob_var, blob_var2);

-- TEST: blob variable in NOT IN clause
-- + cql_set_notnull(_tmp_n_bool_0, 0);
-- + if (cql_blob_equal(_tmp_n_blob_%, blob_var)) break;
-- + if (cql_blob_equal(_tmp_n_blob_%, blob_var)) break;
-- + cql_set_notnull(_tmp_n_bool_0, 1);
set b0_nullable := blob_var not in (blob_var, blob_var);

-- TEST: blob variable in NOT IN clause
-- + if (cql_blob_equal(_tmp_blob_%, blob_var)) break;
-- + if (cql_blob_equal(_tmp_blob_%, blob_var2)) break;
-- + b2 = _tmp_bool_0;
set b2 := blob_var2 not in (blob_var, blob_var2);

-- TEST: proc with blob args
-- + void blob_proc(cql_blob_ref _Nullable *_Nonnull a_blob)
create proc blob_proc(out a_blob blob)
begin
  set a_blob := null;
end;

-- TEST: case statement with blobs
-- + if (cql_is_nullable_true(!blob_var, _tmp_n_blob_1 == blob_var))
set i2 := case blob_var when blob_var then 1 else 2 end;

-- TEST: case statement with returning blobs
-- + cql_set_blob_ref(&blob_var, blob_var2);
-- + cql_set_blob_ref(&blob_var, NULL);
set blob_var := case 1 when 1 then blob_var2 else null end;

declare function blob_func() blob;

-- TEST: function invocation with blob function
-- + cql_set_blob_ref(&blob_var, blob_func());
set blob_var := blob_func();

declare function blob_func_create() create blob;

-- TEST: function invocation with creater blob function
-- +  cql_blob_release(blob_var);
-- +  blob_var = blob_func_create();
set blob_var := blob_func_create();

-- make a table with blobs in it
create table blob_table (
  blob_id integer not null,
  b_notnull blob not null,
  b_nullable blob
);

-- TEST: fetch a nullable blob
-- + cql_column_nullable_blob_ref(_temp_stmt, 0, &blob_var);
set blob_var := (select b_nullable from blob_table where blob_id = 1);

-- TEST: fetch a not null blob
-- + cql_column_blob_ref(_temp_stmt, 0, &_tmp_blob_0);
set blob_var := (select b_notnull from blob_table where blob_id = 1);

-- some not null blob object we can use
declare blob_var_notnull blob not null;

-- initialize for later use
set blob_var_notnull := blob_notnull_func();

-- TEST: bind a nullable blob and a not null blob
-- + INSERT INTO blob_table(blob_id, b_nullable, b_notnull) VALUES(0, blob_var, blob_var_notnull);
-- + "INSERT INTO blob_table(blob_id, b_nullable, b_notnull) VALUES(0, ?, ?)"
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 2,
-- +               CQL_DATA_TYPE_BLOB, blob_var,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_BLOB, blob_var_notnull);
insert into blob_table(blob_id, b_nullable, b_notnull) values(0, blob_var, blob_var_notnull);

-- TEST: a result set that includes blobs
create proc blob_returner()
begin
  select * from blob_table;
end;

-- TEST: forcing null set of object temporary by having no else case
-- + cql_set_object_ref(&obj_var, NULL);
set obj_var := case when 1 then obj_var end;

-- TEST: force a proc with no arg list
-- + void voidproc(void) {
create proc voidproc()
begin
 declare unused int;
end;

-- TEST: create an output struct proc
-- + #define out_cursor_proc_C_refs_offset cql_offsetof(out_cursor_proc_C_row, name) // count = 3
-- + memset(_result_, 0, sizeof(*_result_));
-- + out_cursor_proc_C_row C = { ._refs_count_ = 3, ._refs_offset_ = out_cursor_proc_C_refs_offset };
-- + _result_->_has_row_ = C._has_row_;
-- + _result_->id = C.id;
-- + cql_set_string_ref(&_result_->name, C.name);
-- + _result_->rate = C.rate;
-- + _result_->type = C.type;
-- + _result_->size = C.size;
-- + cql_set_string_ref(&_result_->extra1, C.extra1);
-- + cql_set_string_ref(&_result_->extra2, C.extra2);
-- + DECLARE PROC out_cursor_proc () OUT (id INTEGER NOT NULL, name TEXT, rate LONG_INT, type INTEGER, size REAL, extra1 TEXT NOT NULL, extra2 TEXT NOT NULL) USING TRANSACTION;
create proc out_cursor_proc()
begin
  declare C cursor for select bar.*, 'xyzzy' extra1, 'plugh' extra2 from bar;
  fetch C;
  out C;
end;

-- TEST: fetch from an output struct proc
-- + #define read_cursor_proc_C_refs_offset cql_offsetof(read_cursor_proc_C_row, name) // count = 3
-- + read_cursor_proc_C_row C = { ._refs_count_ = 3, ._refs_offset_ = read_cursor_proc_C_refs_offset };
-- +2 cql_teardown_row(C);
-- +1 _rc_ = out_cursor_proc(_db_, (out_cursor_proc_row *)&C);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
create proc read_cursor_proc()
begin
  declare C cursor fetch from call out_cursor_proc();
end;

-- TEST: declare a cursor and do a fetch as separate actions
-- +1  declare_cursor_then_fetch_from_proc_C_row C = { ._refs_count_ = 3, ._refs_offset_ = declare_cursor_then_fetch_from_proc_C_refs_offset };
-- +2  cql_teardown_row(C);
-- +1  _rc_ = out_cursor_proc(_db_, (out_cursor_proc_row *)&C);
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
declare proc totally_void_proc();

-- TEST: call out proc like a function
-- + SET i2 := outparm_test();
-- + _rc_ = outparm_test(_db_, &i2);
-- +  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
set i2 := outparm_test();

declare proc compute(in a_ integer not null, out b_ integer not null);

-- TEST: call out proc like a function, this one has args
-- + compute(1, &_tmp_int_1);
-- + compute(_tmp_int_1, &i2);
set i2 := compute(compute(1));

-- a dml method
declare proc dml_compute(in a_ integer not null, out b_ integer not null) USING TRANSACTION;

-- TEST: call out proc like a function, this one has args and uses the db
-- + _rc_ = dml_compute(_db_, 1, &_tmp_int_1);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
-- + _rc_ = dml_compute(_db_, _tmp_int_1, &i2);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
set i2 := dml_compute(dml_compute(1));

-- TEST: write the result of a proc-as-func call to an out variable
-- + _rc_ = dml_compute(_db_, 1, &*a_);
create proc dml_user(out a_ integer not null)
begin
  set a_ := dml_compute(1);
end;

-- a test table for the following case
create table threads (
 thread_key long int not null
);

-- TEST: nested subquery in a proc
-- this forces the json_schema runtime to run over an atypical table_factor
-- + _rc_ = cql_prepare(_db_, _result_stmt,
-- + "SELECT thread_key "
-- + "FROM (SELECT thread_key "
-- + "FROM threads) AS T"
create procedure thread_theme_info_list(thread_key_ LONG INT NOT NULL)
begin
  select *
  from (select thread_key from threads) T;
end;

-- TEST: value cursor fetch
-- + _seed_ = 123;
-- + C._has_row_ = 1;
-- + C.id = _seed_;
-- + char *_printf_result = sqlite3_mprintf("name_%d", _seed_);
-- + cql_string_release(_tmp_text_0);
-- + _tmp_text_0 = cql_string_ref_new(_printf_result);
-- + sqlite3_free(_printf_result);
-- + cql_set_string_ref(&C.name, _tmp_text_0);
-- + cql_set_notnull(C.rate, _seed_);
-- + cql_set_notnull(C.type, _seed_);
-- + cql_set_notnull(C.size, _seed_);
-- This should not be a dml proc, it doesn't actually use the db
-- + fetch_values_dummy(void)
-- - _rc_
-- - cql_cleanup
create proc fetch_values_dummy()
begin
  declare C cursor like select * from bar;
  fetch C() from values() @dummy_seed(123) @dummy_nullables;
end;

-- TEST: value cursor fetch, using type syntax
-- this cursor has the fields of bar plus xx and yy
-- + cql_int32 id;
-- + cql_nullable_int64 rate;
-- + cql_nullable_int32 type;
-- + cql_nullable_double size;
-- + cql_nullable_double xx;
-- + cql_string_ref _Nullable name;
-- + cql_string_ref _Nullable yy;
-- + fetch_values_extended_C_row C = { ._refs_count_ = 2, ._refs_offset_ = fetch_values_extended_C_refs_offset };
-- + cql_set_string_ref(&C.name, _tmp_text_0);
-- + cql_set_notnull(C.rate, _seed_);
-- + cql_set_notnull(C.type, _seed_);
-- + cql_set_notnull(C.size, _seed_);
-- + cql_set_notnull(C.xx, _seed_);
-- + fetch_values_extended(void)
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
-- - cql_cleanup
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto catch_start%; }
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
-- begin try and begin catch implyl dml proc
-- + cql_code no_code_after_catch(sqlite3 *_Nonnull _db_)
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
-- + void out_no_db(out_no_db_row *_Nonnull _result_) {
-- + memset(_result_, 0, sizeof(*_result_));
-- + out_no_db_C_row C = { 0 };
-- + C._has_row_ = 1;
-- + C.A = 3;
-- + C.B = 12;
-- + _result_->_has_row_ = C._has_row_;
-- + _result_->A = C.A;
-- + _result_->B = C.B;
-- + DECLARE PROC out_no_db () OUT (A INTEGER NOT NULL, B REAL NOT NULL);
create proc out_no_db()
begin
  declare C cursor like select 1 A, 2.5 B;
  fetch C(A,B) from values(3,12);
  out C;
end;

-- TEST: declare cursor like cursor
-- + declare_cursor_like_cursor_C0_row C0 = { 0 };
-- + declare_cursor_like_cursor_C1_row C1 = { 0 };
-- + memset(_result_, 0, sizeof(*_result_));
-- + C1._has_row_ = 1;
-- + C1.A = 3;
-- + C1.B = 12;
-- + _result_->_has_row_ = C1._has_row_;
-- + _result_->A = C1.A;
-- + _result_->B = C1.B;
create proc declare_cursor_like_cursor()
begin
  declare C0 cursor like select 1 A, 2.5 B;
  declare C1 cursor like C0;
  fetch C1(A,B) from values(3,12);
  out C1;
end;

-- TEST: declare cursor like proc
-- + void declare_cursor_like_proc(declare_cursor_like_proc_row *_Nonnull _result_) {
-- + memset(_result_, 0, sizeof(*_result_));
-- + _result_->_has_row_ = C._has_row_;
-- + _result_->_refs_count_ = 1;
-- + _result_->_refs_offset_ = declare_cursor_like_proc_refs_offset;
-- + _result_->a = C.a;
-- + cql_set_string_ref(&_result_->b, C.b);
-- + cql_teardown_row(C);
-- - Error
create proc declare_cursor_like_proc()
begin
  declare C cursor like fetcher_proc;
  out C;
end;

-- TEST: declare a cursor like a table
-- + void declare_cursor_like_table(declare_cursor_like_table_row *_Nonnull _result_) {
-- + declare_cursor_like_table_C_row C = { ._refs_count_ = 1, ._refs_offset_ = declare_cursor_like_table_C_refs_offset };
-- + memset(_result_, 0, sizeof(*_result_));
-- + _result_->_has_row_ = C._has_row_;
-- + _result_->_refs_offset_ = declare_cursor_like_table_refs_offset;
-- + _result_->id = C.id;
-- + cql_set_string_ref(&_result_->name, C.name);
-- + _result_->rate = C.rate;
-- + _result_->type = C.type;
-- + _result_->size = C.size;
-- + cql_teardown_row(C);
-- - Error
create proc declare_cursor_like_table()
begin
  declare C cursor like bar;
  out C;
end;

-- TEST: declare a cursor like a view
-- + void declare_cursor_like_view_fetch_results( declare_cursor_like_view_result_set_ref _Nullable *_Nonnull result_set) {
-- + declare_cursor_like_view_C_row C = { 0 };
-- + _result_->_has_row_ = C._has_row_;
-- + _result_->f1 = C.f1;
-- + _result_->f2 = C.f2;
-- + _result_->f3 = C.f3;
-- - Error
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
-- + "id INTEGER PRIMARY KEY AUTOINCREMENT, "
create proc long_auto_table_maker()
begin
  create table long_int_autoinc (
    id long primary key autoincrement,
    name text
  );
end;

declare proc blob_out(out x blob);

-- TEST: force a blob variable to be cleared to null before a proc cll
-- call on out parameter.  This is important because the blob
-- might be holding a value and the out arg is assume to be junk
-- +   cql_blob_ref b = NULL;
-- +   cql_set_blob_ref(&b, NULL);
-- +   blob_out(&b);
-- +   cql_blob_release(b);
create proc blob_call1()
begin
 declare b blob;
 call blob_out(b);
end;

-- TEST: force a blob variable to be cleared to null before a function
-- call on out parameter.  This is important because the blob
-- might be holding a value and the out arg is assume to be junk
-- +   cql_blob_ref b = NULL;
-- +   cql_set_blob_ref(&b, NULL);
-- +   blob_out(&b);
-- +   cql_blob_release(b);
create proc blob_call2()
begin
 declare b blob;
 set b := blob_out(); -- use function call syntax should be the same
end;

-- TEST: forces us to set a blob to null via else.  This is not the store code path
-- + cql_set_blob_ref(&b, b1);
-- + cql_set_blob_ref(&b, NULL);
create proc blob_no_else()
begin
  declare b blob;
  declare b1 blob;
  set b := case b when b then b1 end;
end;

-- TEST: use with-insert form
-- +  _rc_ = cql_exec(_db_,
-- + "WITH "
-- + "x (a) AS (SELECT 111) "
-- + "INSERT INTO foo(id) VALUES(ifnull(( SELECT a "
-- + "FROM x ), 0))");
with x(a) as (select 111)
insert into foo values ( ifnull((select a from x), 0));

-- TEST: use insert from select (put this in a proc to force the schema utils to walk it)
-- + "WITH "
-- + "x (a) AS (SELECT 111) "
-- + "INSERT INTO foo(id) SELECT a "
-- + "FROM x"
create proc with_inserter()
begin
  with x(a) as (select 111)
    insert into foo select * from x;
end;

declare select func SqlUserFunc(id integer) real not null;

-- TEST: invoke a declared user function
-- + _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT SqlUserFunc(123)"
set r2 := (select SqlUserFunc(123));

-- TEST: multiple rewrites complex arg filter
--
-- Note: we're doing something that's legal but not really very useful here just to force the codegen.
-- the out_arg should still be present in the args (and we check it here plus the code is required to compile)
-- and we have to be able to correctly code gen two different like args cases in different locations.
-- It's hard to think of a real use case for this but I want to make sure the rewriter doesn't screw it up.
--
-- + cql_code multi_rewrite(sqlite3 *_Nonnull _db_,
-- + cql_int32 blob_id_, cql_blob_ref _Nonnull b_notnull_, cql_blob_ref _Nullable b_nullable_,
-- + cql_int32 id_, cql_string_ref _Nullable name_, cql_nullable_int64 rate_, cql_nullable_int32 type_,
-- + cql_nullable_double size_, cql_int32 *_Nonnull out_arg)
-- + "INSERT INTO blob_table(blob_id, b_notnull, b_nullable) VALUES(?, ?, ?)"
-- + cql_multibind(&_rc_, _db_, &_temp_stmt, 3,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, blob_id_,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_BLOB, b_notnull_,
-- +               CQL_DATA_TYPE_BLOB, b_nullable_);
create proc multi_rewrite(like blob_table, like bar, out out_arg integer not null)
begin
  insert into blob_table from arguments;
  set out_arg := 1;
end;

-- TEST: fetch to a cursor from another cursor
-- + C1._has_row_ = 1;
-- + C1.A = C0.A;
-- + cql_set_string_ref(&C1.B, C0.B);
create proc fetch_to_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, "foo" B;
  declare C1 cursor like C0;
  fetch C0 from values (2, "bar");
  fetch C1 from C0;
  out C1;
end;

-- TEST loop statement cursor with autofetch
-- + sqlite3_stmt *C_stmt = NULL;
-- + loop_statement_cursor_C_row C = { 0 };
-- + "SELECT 1"
-- + C._has_row_ = _rc_ == SQLITE_ROW;
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (!C._has_row_) break
create proc loop_statement_cursor()
begin
  declare C cursor for select 1 A;
  loop fetch C
  begin
    call printf("%d\n", C.A);
  end;
end;

-- TEST loop statement cursor with autofetch
-- + sqlite3_stmt *C_stmt = NULL;
-- + cql_bool _C_has_row_ = 0;
-- + cql_int32 A_ = 0;
-- + "SELECT 1"
-- + _C_has_row_ = _rc_ == SQLITE_ROW;
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (!_C_has_row_) break
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
-- + if (!(i.value < 5)) break;
-- +2 cql_finalize_stmt(&C_stmt);
-- + _rc_ = simple_select(_db_, &C_stmt);
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
-- + if (!cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) break;
-- +2 cql_finalize_stmt(&C_stmt);
-- + _rc_ = simple_select(_db_, &C_stmt);
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
-- one release in cleanup, one before the fetch
-- +2 cql_object_release(C_object_);
-- one release in cleanup
-- +1 cql_object_release(D_object_);
-- +1 cql_object_release(box);
-- + C_object_ = cql_box_stmt(C_stmt);
-- + cql_set_object_ref(&box, C_object_);
-- + D_stmt = cql_unbox_stmt(D_object_);
-- + C_object_ = cql_box_stmt(C_stmt);
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
-- + DECLARE PROC out_union_helper () OUT UNION (x INTEGER NOT NULL);
create proc out_union_helper()
begin
  declare C cursor like select 1 x;
  fetch C using 1 x;
  out union C;
end;

-- TEST: verify the decl, this is only for later tests
-- + DECLARE PROC out_union_dml_helper () OUT UNION (x INTEGER NOT NULL) USING TRANSACTION;
create proc out_union_dml_helper()
begin
  declare C cursor for select 1 x;
  fetch C;
  out union C;
end;

-- TEST: call out union in a loop
-- two instances, one for the call and one at cleanup
-- +2 cql_object_release(C_result_set_);
-- + out_union_helper_fetch_results(&C_result_set_);
-- + C_row_num_ = C_row_count_ = -1;
-- + C_row_count_ = cql_result_set_get_count((cql_result_set_ref)C_result_set_);
-- + C_row_num_++;
-- + C._has_row_ = C_row_num_ < C_row_count_;
-- + cql_copyoutrow(NULL, (cql_result_set_ref)C_result_set_, C_row_num_, 1,
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
-- + DECLARE PROC forward_out_union () OUT UNION (x INTEGER NOT NULL)
-- - USING TRANSACTION
-- + *_result_set_ = NULL;
-- + out_union_helper_fetch_results((out_union_helper_result_set_ref *)_result_set_);
-- + if (!*_result_set_) *_result_set_ = (forward_out_union_result_set_ref)cql_no_rows_result_set();
-- +1 cql_object_release(*_result_set_);
create proc forward_out_union()
begin
  call out_union_helper();
end;

-- declare one, this ensures we have the necessary types after the decl (the row type esp.)
declare proc extern_out_union_helper () OUT UNION (x INTEGER NOT NULL);

-- TEST: this should still compile even though the body of the proc isn't here
-- + extern_out_union_helper_fetch_results((extern_out_union_helper_result_set_ref *)_result_set_);
-- +1 cql_object_release(*_result_set_);
create proc forward_out_union_extern()
begin
  call extern_out_union_helper();
end;

-- TEST: forward out union result, with dml proc
-- + DECLARE PROC forward_out_union_dml () OUT UNION (x INTEGER NOT NULL) USING TRANSACTION;
-- + *_result_set_ = NULL;
-- +  _rc_ = out_union_dml_helper_fetch_results(_db_, (out_union_dml_helper_result_set_ref *)_result_set_);
-- +1 cql_object_release(*_result_set_);
create proc forward_out_union_dml()
begin
  call out_union_dml_helper();
end;

-- TEST: ensure cursors work outside of a proc
--  _rc_ = cql_prepare(_db_, &global_cursor_stmt,
--    "SELECT 1 AS a, 2 AS b"
declare global_cursor cursor for select 1 a, 2 b;

-- TEST: fetch from global cursor
-- + _rc_ = sqlite3_step(global_cursor_stmt);
-- + global_cursor._has_row_ = _rc_ == SQLITE_ROW;
fetch global_cursor;

-- TEST: use like in an expression
-- +  i2 = cql_string_like(_literal_%_x_, _literal_%_y_) == 0;
set i2 := 'x' LIKE 'y';

-- TEST: use not like in an expression
-- +  i2 = cql_string_like(_literal_%_x_, _literal_%_y_) != 0;
set i2 := 'x' NOT LIKE 'y';

-- TEST: use like in a SQL statement
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT 'x' LIKE 'y'"
set i2 := (select 'x' LIKE 'y');

-- TEST: use not like in a SQL statement
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT 'x' NOT LIKE 'y'"
set i2 := (select 'x' NOT LIKE 'y');

-- TEST: use match in a SQL statement
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT 'x' MATCH 'y'"
set i2 := (select 'x' MATCH 'y');

-- TEST: use glob in a SQL statement
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +  "SELECT 'x' GLOB 'y'"
set i2 := (select 'x' GLOB 'y');

-- TEST: use lot of bitwise operators
-- NOTE the SQL order of ops is different...
-- no parens used here
-- +  SET i2 := 1 << 2 | 1 << 4 & 1 >> 8;
-- in Sqlite binary math operators all bind equal and left to right so the above is the same as
--    SET i2 :=  (((((1 << 2) | 1) << 4) & 1) >> 8);
-- in C that becomes
-- because i C  << and >> are stronger than | and &
-- + i2 = ((1 << 2 | 1) << 4 & 1) >> 8;
set i2 := 1 << 2 | 1 << 4 & 1 >> 8;

-- TEST: now maybe what you expected to see.  Force the issue with parens
-- + SET i2 := 1 << 2 | (1 << 4) & (1 >> 8);
-- Still not what you expected... remember | and & are EQUAL in sql
-- so the above was parsed left to right...
-- + i2 = (1 << 2 | 1 << 4) & 1 >> 8;
set i2 := (1 << 2) | (1 << 4) & (1 >> 8);

-- TEST: this is really the normal thing
-- some parens were redunant, removed...
-- + SET i2 := 1 << 2 | (1 << 4 & (1 >> 8));
-- now this is the usual C order of ops and no parens are in the C
-- + i2 = 1 << 2 | 1 << 4 & 1 >> 8;
set i2 := (1 << 2) | ((1 << 4) & (1 >> 8));

-- TEST: force a high binding ~
-- nothing weird here, ~ binds very strong in both languages
-- + i2 = 1 | ~ i2;
set i2 := 1 | ~i2;

-- TEST: create a trigger, force the dml
-- + _rc_ = cql_exec(_db_,
-- +   "CREATE TEMP TRIGGER IF NOT EXISTS trigger1 "
-- +     "BEFORE DELETE ON bar "
-- +     "FOR EACH ROW "
-- +     "WHEN old.id > 7 "
-- +   "BEGIN "
-- +     "SELECT old.id; "
-- +   "END"
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
-- + b = 1 == 1;
-- + b = cql_string_equal(_literal_%_x_, _literal_%_x_);
-- + b = cql_string_equal(_literal_%_x_, _literal_%_y_);
-- + b = !!(1 + (3 == 4));
-- + cql_set_notnull(i, 1);
-- + cql_set_notnull(j, 2);
-- + b = ((i.is_null == j.is_null) && (j.is_null || i.value == j.value))
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
-- + b = cql_blob_equal(bl1, bl2);
-- + b = !cql_blob_equal(bl1, bl2);
create proc is_blob()
begin
  declare bl1 blob;
  declare bl2 blob;
  declare b bool not null;
  set b := bl1 is bl2;
  set b := bl1 is not bl2;
end;

-- TEST: IS NOT patterns
-- + b = 1 != 1;
-- + b = !cql_string_equal(_literal_%_x_, _literal_%_x_);
-- + b = !cql_string_equal(_literal_%_x_, _literal_%_y_);
-- + b = !!(1 + (3 != 4));
-- + cql_set_notnull(i, 1);
-- + cql_set_notnull(j, 2);
-- + b = !((i.is_null == j.is_null) && (j.is_null || i.value == j.value))
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
-- + cql_set_null(*b);
create proc in_test(x integer, out b bool)
begin
  set b := NULL IN (1);
end;

-- TEST: null on lhs of NOT IN
-- + cql_set_null(*b);
-- + DECLARE PROC not_in_test (x INTEGER, OUT b BOOL);
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
-- + cql_uint16 simple_identity_identity_columns[] = { 1,
-- + DECLARE PROC simple_identity () (id INTEGER NOT NULL, data INTEGER NOT NULL);
@attribute(cql:identity=(id))
create proc simple_identity()
begin
  select 1 as id, 2 as data;
end;

-- TEST: create proc with a multi-column identity attribute
-- + cql_uint16 complex_identity_identity_columns[] = { 2,
@attribute(cql:identity=(col1, col2))
create proc complex_identity()
begin
  select 1 as col1, 2 as col2, 3 as data;
end;

-- TEST: create proc with a out cursor and identity column
-- + cql_uint16 out_cursor_identity_identity_columns[] = { 1,
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
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // data
-- + void radioactive_proc_set_encoding(cql_int32 col, cql_bool encode) {
@attribute(cql:vault_sensitive)
create proc radioactive_proc()
begin
 select * from radioactive;
end;

-- TEST: with delete form
-- + _rc_ = cql_exec(_db_,
-- +   "WITH "
-- +   "x (a) AS (SELECT 111) "
-- +   "DELETE FROM foo WHERE id IN (SELECT a "
-- +     "FROM x)");
create proc with_deleter()
begin
  with x(a) as (select 111)
    delete from foo where id in (select * from x);
end;

-- TEST: with update form
-- + _rc_ = cql_exec(_db_,
-- +  "WITH "
-- +  "x (a) AS (SELECT 111) "
-- +  "UPDATE bar "
-- +  "SET name = 'xyzzy' "
-- +    "WHERE id IN (SELECT a "
-- +    "FROM x)");
create proc with_updater()
begin
  with x(a) as (select 111)
    update bar set name = 'xyzzy' where id in (select * from x);
end;

create temp table table1( id integer);
create temp table table2( id integer);

-- TEST: autodrop attribute
-- + .autodrop_tables = "table1\0table2\0",
@attribute(cql:autodrop=(table1, table2))
create proc autodropper()
begin
   select 1 a, 2 b;
end;

-- TEST: base fragment attribute
-- there should be no proc codegen
-- - cql_code % base_fragment
-- - cleanup
@attribute(cql:base_fragment=core)
create proc base_fragment(id_ integer not null)
begin
with
  core(x,y,z) as (select id,name,rate from bar where id = id_)
select * from core;
end;

-- TEST: make sure that the name of the cursor is canonicalized
-- There should be no references to the version with the wrong case
-- + simple_cursor_proc_A_CURSOR_row A_CURSOR = { 0 };
-- + A_CURSOR._has_row_ = 1;
-- + A_CURSOR.id = 1;
-- + _result_->_has_row_ = A_CURSOR._has_row_;
-- + _result_->id = A_CURSOR.id;
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
-- + "SELECT (5), T.xyzzy "
-- + "FROM (SELECT 1 AS xyzzy) AS T");
create proc redundant_cast()
begin
  select CAST(5 as integer) plugh, T.xyzzy five from (select 1 xyzzy) as T;
end;

-- TEST: select with alias in view
-- + "CREATE VIEW alias_preserved AS "
-- + "SELECT (5) AS plugh, T.xyzzy AS five "
-- + "FROM (SELECT 1 AS xyzzy) AS T");
create proc view_creator()
begin
  create view alias_preserved as
    select CAST(5 as integer) plugh, T.xyzzy five from (select 1 xyzzy) as T;
end;

@enforce_strict cast;

create table switch_account_badges(badge_count integer);
create table unread_pending_threads(unread_pending_thread_count integer);

-- TEST: nested select table should not have column aliases removed
-- +  "SELECT SUM(A.unread_pending_thread_count), SUM(A.switch_account_badge_count) "
-- +    "FROM (SELECT P.unread_pending_thread_count AS unread_pending_thread_count, 0 AS switch_account_badge_count "
-- +    "FROM unread_pending_threads AS P "
-- +  "UNION ALL "
-- +  "SELECT 0 AS unread_pending_thread_count, S.badge_count AS switch_account_badge_count "
-- +    "FROM switch_account_badges AS S) AS A");
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
-- + "SELECT 1, 2 "
-- + "UNION ALL "
-- + "SELECT foo.id, 2 "
-- +   "FROM foo");
CREATE PROC top_level_select_alias_unused()
BEGIN
  SELECT 1 AS id, 2 as x
  UNION ALL
  SELECT foo.id, 2 as x
  FROM foo;
END;

-- TEST: aliases in top-level selects must not be removed if referenced from an
-- order by clause
-- + "SELECT 1 AS id, 2 "
-- + "UNION ALL "
-- + "SELECT foo.id, 2 "
-- +   "FROM foo "
-- + "ORDER BY id");
CREATE PROC top_level_select_alias_used_in_orderby()
BEGIN
  SELECT 1 AS id, 2 as x
  UNION ALL
  SELECT foo.id, 2 as x
  FROM foo
  ORDER BY id;
END;

-- TEST: try to use a WITH_SELECT form in a select expression
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +   "WITH "
-- +   "threads2 (count) AS (SELECT 1) "
-- +   "SELECT COUNT(*) "
-- +     "FROM threads2");
create proc use_with_select()
begin
   declare x integer;
   SET x := (WITH threads2 (count) AS (SELECT 1 foo) SELECT COUNT(*) FROM threads2);
end;

-- declare a simple table-valued function
declare select function ReadFromRowset(rowset Object<rowset>) (id integer);

-- TEST: use a table valued function that consumes an object
-- + cql_multibind(&_rc_, _db_, &C_stmt, 1,
-- + CQL_DATA_TYPE_OBJECT, rowset);
create proc rowset_object_reader(rowset Object<rowset>)
begin
  declare C cursor for select * from ReadFromRowset(rowset);
end;

-- TEST: codegen upsert statement with update statement
-- + "INSERT INTO foo(id) SELECT id "
-- + "FROM bar "
-- + "WHERE 1 "
-- + "ON CONFLICT (id) DO UPDATE "
-- + "SET id = 10 "
-- + "WHERE id <> 10"
-- + cql_code upsert_do_something(sqlite3 *_Nonnull _db_) {
create proc upsert_do_something()
BEGIN
 insert into foo select id from bar where 1 on conflict(id) do update set id=10 where id != 10;
END;

-- TEST: codegen with upsert statement form
-- + cql_code with_upsert_form(sqlite3 *_Nonnull _db_) {
-- + "WITH "
-- + "names (id) AS (VALUES(1), (5), (3), (12)) "
-- + "INSERT INTO foo(id) SELECT id "
-- +   "FROM names "
-- +   "WHERE 1 "
-- + "ON CONFLICT (id) DO UPDATE "
-- + "SET id = 10 "
-- +   "WHERE id <> 10");
create proc with_upsert_form()
BEGIN
 with names(id) as (values (1), (5), (3), (12))
 insert into foo select id from names where 1 on conflict(id) do update set id = 10 where id != 10;
END;

-- TEST: codegen upsert statement with do nothing
-- + "INSERT INTO foo(id) VALUES(?) "
-- + "ON CONFLICT DO NOTHING"
-- + cql_code upsert_do_nothing(sqlite3 *_Nonnull _db_, cql_int32 id_) {
create proc upsert_do_nothing(id_ integer not null)
BEGIN
 insert into foo(id) values(id_) on conflict do nothing;
END;

-- TEST: codegen with-insert with a seed
-- + _seed_ = 1337;
-- + _rc_ = cql_exec(_db_,
-- + "WITH "
-- + "some_cte (id) AS (SELECT 1 AS id) "
-- + "INSERT INTO bar(id) VALUES(ifnull(( SELECT id "
-- + "FROM some_cte ), 0))");
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
with some_cte(id) as (select 1 id)
insert into bar(id)
values (ifnull((select id from some_cte), 0))
@dummy_seed(1337);

-- TEST: codegen upsert with a seed
-- + _seed_ = 1338;
-- + _rc_ = cql_exec(_db_,
-- + "INSERT INTO bar(id) VALUES(1) "
-- + "ON CONFLICT (id) DO UPDATE "
-- + "SET id = 10");
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
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
-- + use_many_out_cursors_c1_row c1 = { ._refs_count_ = 1, ._refs_offset_ = use_many_out_cursors_c1_refs_offset };
-- + use_many_out_cursors_c2_row c2 = { ._refs_count_ = 1, ._refs_offset_ = use_many_out_cursors_c2_refs_offset };
-- +1 p1((p1_row *)&c1);
-- +1 _rc_ = p2(_db_, (p2_row *)&c2);
-- +2 cql_teardown_row(c1);
-- +2 cql_teardown_row(c2);
create procedure use_many_out_cursors()
begin
  declare c1 cursor fetch from call p1();
  declare c2 cursor fetch from call p2();
end;

-- TEST: each fetch forces the declaration of the cursor storage if it has
-- not already been declared.  In this case the third branch of the if
-- must find that p1 and p2 row data are already declare and not duplicate
-- the declarations.
-- +1 fetch_many_times_C_row C = { ._refs_count_ = 1, ._refs_offset_ = fetch_many_times_C_refs_offset };
-- +2 p1((p1_row *)&C);
-- +2 _rc_ = p2(_db_, (p2_row *)&C);
-- +5 cql_teardown_row(C);
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
-- + DECLARE PROC out_union_two () OUT UNION (x INTEGER NOT NULL, y TEXT NOT NULL);
-- + void out_union_two_fetch_results(out_union_two_result_set_ref _Nullable *_Nonnull _result_set_) {
-- + cql_profile_start(CRC_out_union_two, &out_union_two_perf_index);
-- + cql_bytebuf _rows_;
-- + cql_bytebuf_open(&_rows_);
-- +2 cql_retain_row(C);
-- +2 if (C._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C, sizeof(C));
-- + cql_results_from_data(SQLITE_OK, &_rows_, &out_union_two_info, (cql_result_set_ref *)_result_set_);
-- + cql_teardown_row(C);
create proc out_union_two()
begin
 declare C cursor like select 1 x, '2' y;
 fetch C from values(1, "y");
 out union C;
 out union C;
end;

-- TEST: read back the two rows from the above
-- + CQL_WARN_UNUSED cql_code out_union_reader(sqlite3 *_Nonnull _db_) {
-- + out_union_two_result_set_ref c_result_set_ = NULL
-- + cql_int32 c_row_num_ = 0;
-- + cql_int32 c_row_count_ = 0;
-- + out_union_reader_c_row c = { ._refs_count_ = 1, ._refs_offset_ = out_union_reader_c_refs_offset };
-- + out_union_two_fetch_results(&c_result_set_);
-- + c_row_num_ = c_row_count_ = -1;
-- + c_row_count_ = cql_result_set_get_count((cql_result_set_ref)c_result_set_);
-- + for (;;) {
-- +   c_row_num_++;
-- +   c._has_row_ = c_row_num_ < c_row_count_;
-- +   cql_copyoutrow(NULL, (cql_result_set_ref)C_result_set_, c_row_num_, 2,
-- +                  CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &c.x,
-- +                  CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &c.y);
-- NOT PRESENT !!
-- -   if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- +   if (!c._has_row_) break;
-- + }
-- + cql_object_release(c_result_set_);
-- + cql_teardown_row(c);
-- + return _rc_;
create proc out_union_reader()
begin
  declare c cursor for call out_union_two();
  loop fetch C
  begin
    call printf("%d %s\n", C.x, C.y);
  end;
end;

-- TEST: create a result set from selected rows
-- + DECLARE PROC out_union_from_select () OUT UNION (x INTEGER NOT NULL, y TEXT NOT NULL) USING TRANSACTION;
-- + cql_bytebuf _rows_;
-- + cql_bytebuf_open(&_rows_);
-- +2 cql_retain_row(C);
-- +2 if (C._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C, sizeof(C));
-- + out_union_from_select_info.db = _db_;
-- + cql_results_from_data(_rc_, &_rows_, &out_union_from_select_info, (cql_result_set_ref *)_result_set_);
-- + cql_teardown_row(C);
create proc out_union_from_select()
begin
 declare C cursor for select 1 x, '2' y;
 fetch C;
 out union C;
 out union C;
end;

-- TEST: reading from out union again, this time a DML proc (uses select)
-- slightly different call path
-- + _rc_ = out_union_from_select_fetch_results(_db_, &c_result_set_);
-- + c_row_num_ = c_row_count_ = -1;
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
-- + c_row_count_ = cql_result_set_get_count((cql_result_set_ref)c_result_set_);
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

-- TEST:  we need to be able to call the above proc, this requires
-- the args be emitted correctly, with a comma (!)
-- + out_union_values_fetch_results(&C_result_set_, a, b);
-- + C_row_num_ = C_row_count_ = -1;
-- + C_row_count_ = cql_result_set_get_count((cql_result_set_ref)C_result_set_);
-- + C_row_num_++;
-- + C._has_row_ = C_row_num_ < C_row_count_;
-- + cql_copyoutrow(NULL, (cql_result_set_ref)C_result_set_, C_row_num_, 2,
-- +   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.x,
-- +   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
create proc read_out_union_values(a integer not null, b integer not null)
begin
  declare C cursor for call out_union_values(a,b);
  fetch C;
end;

-- This just sets up a call to a procedure that proceduce a dml out union result set
-- + out_union_dml_info.db = _db_;
-- + cql_results_from_data(_rc_, &_rows_, &out_union_dml_info, (cql_result_set_ref *)_result_set_);
@attribute(cql:vault_sensitive)
create proc out_union_dml()
begin
  declare x cursor for select * from radioactive;
  fetch x;
  out union x;
end;

-- TEST: we need to make sure the a notnull db pointer is pass to cql_copyoutrow(...)
-- + cql_copyoutrow(_db_, (cql_result_set_ref)C_result_set_, C_row_num_, 2,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.id,
-- +                CQL_DATA_TYPE_STRING, &C.data);
@attribute(cql:vault_sensitive)
create proc out_union_dml_for_call()
begin
  declare C cursor for call out_union_dml();
  fetch C;
end;

-- TEST: generate a compound select statement in an expression (this is a legal form)
-- + _rc_ = cql_prepare(_db_, &_temp_stmt,
-- + "SELECT 1 "
-- + "WHERE 0 "
-- + "UNION "
-- + "SELECT 2 "
-- + "LIMIT 1");
create proc compound_select_expr()
begin
  declare x integer;
  set x := (select 1 where 0 union select 2 limit 1);
end;

-- TEST: generate window function invocation
-- + "SELECT id,  "
-- + "row_number() OVER () "
-- + "FROM foo");
create proc window_function_invocation()
begin
  select id, row_number() over () as row_num from foo;
end;

-- TEST: update some of the cursor columns
-- + if (C._has_row_) {
-- +   C.x = 2;
-- + }
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
-- + goto cql_cleanup; // return
-- + cql_cleanup:
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
-- + goto cql_cleanup; // return
-- + cql_cleanup:
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
-- + void empty_proc(void) {
-- - cql_cleanup:
-- + }
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

-- This proc illustrates a case where we need to put the ;
-- after the catch label so that there is a statement.  Note
-- the problem is that two catches end in a row.  The fact that
-- the bodies of the functions are empty changes nothing. It only
-- matters that we had end catch and then end some other block
-- so empty statement lists are not required to make this issue happen
-- + CQL_WARN_UNUSED cql_code tail_catch(sqlite3 *_Nonnull _db_) {
-- +2 // try
-- + goto catch_end_6;
-- + goto catch_end_7;
-- mandatory ; after this label
-- + catch_end_7:;
-- + catch_end_6:;
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
-- + "INSERT INTO bar(id, name) VALUES(1, 'it''s high noon\r\n\f\b\t\v')");
create proc pretty_print_with_quote()
begin
  insert into bar(id, name) values(1, "it's high noon\r\n\f\b\t\v");
end;

-- TEST: string literal with hex forms
-- + "INSERT INTO bar(id, name) VALUES(1, '\x01\x02\xa1\x1bg')");
create proc hex_quote()
begin
  insert into bar(id, name) values(1, "\x01\x02\xA1\x1b\x00\xg");
end;

-- TEST: no getters generated for this function
-- getters go into the .h stream and there's no test hook for that
-- but this can be verified by checking the .ref for the header file manually
-- We verify that we are still generating the data types (only the getters are suppressed)
-- +  CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- +  CQL_DATA_TYPE_STRING, // name
-- +  CQL_DATA_TYPE_INT64, // rate
-- +  CQL_DATA_TYPE_INT32, // type
-- +  CQL_DATA_TYPE_DOUBLE, // size
@attribute(cql:suppress_getters)
create proc lotsa_columns_no_getters()
begin
  select * from bar;
end;


-- TEST: a copy function will be generated
-- + cql_code sproc_with_copy(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt)
@attribute(cql:generate_copy)
create proc sproc_with_copy()
begin
  select * from bar;
end;

-- TEST: emit an object result set with setters with not null values
-- + extern void emit_object_with_setters_set_o(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_object_ref _Nonnull new_value) {
-- +   cql_contract_argument_notnull((void *)new_value, 2);
-- +   cql_result_set_set_object_col((cql_result_set_ref)result_set, 0, 0, new_value);
-- + extern void emit_object_with_setters_set_x(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_object_ref _Nonnull new_value) {
-- +   cql_contract_argument_notnull((void *)new_value, 2);
-- +   cql_result_set_set_object_col((cql_result_set_ref)result_set, 0, 1, new_value);
-- + extern void emit_object_with_setters_set_i(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_int32 new_value) {
-- +   cql_result_set_set_int32_col((cql_result_set_ref)result_set, 0, 2, new_value);
-- + extern void emit_object_with_setters_set_l(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_int64 new_value) {
-- +   cql_result_set_set_int64_col((cql_result_set_ref)result_set, 0, 3, new_value);
-- + extern void emit_object_with_setters_set_b(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_bool new_value) {
-- +   cql_result_set_set_bool_col((cql_result_set_ref)result_set, 0, 4, new_value);
-- + extern void emit_object_with_setters_set_d(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_double new_value) {
-- +   cql_result_set_set_double_col((cql_result_set_ref)result_set, 0, 5, new_value);
-- + extern void emit_object_with_setters_set_t(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_string_ref _Nonnull new_value) {
-- +   cql_contract_argument_notnull((void *)new_value, 2);
-- +   cql_result_set_set_string_col((cql_result_set_ref)result_set, 0, 6, new_value);
-- + extern void emit_object_with_setters_set_bl(emit_object_with_setters_result_set_ref _Nonnull result_set, cql_blob_ref _Nonnull new_value) {
-- +   cql_contract_argument_notnull((void *)new_value, 2);
-- +   cql_result_set_set_blob_col((cql_result_set_ref)result_set, 0, 7, new_value);
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
-- + extern void emit_setters_with_nullables_set_o(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_object_ref _Nullable new_value) {
-- +   cql_result_set_set_object_col((cql_result_set_ref)result_set, 0, 0, new_value);
-- + extern void emit_setters_with_nullables_set_x(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_object_ref _Nullable new_value) {
-- +   cql_result_set_set_object_col((cql_result_set_ref)result_set, 0, 1, new_value);
-- + extern void emit_setters_with_nullables_set_i_value(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_int32 new_value) {
-- +   cql_result_set_set_int32_col((cql_result_set_ref)result_set, 0, 2, new_value);
-- + extern void emit_setters_with_nullables_set_i_to_null(emit_setters_with_nullables_result_set_ref _Nonnull result_set) {
-- +   cql_result_set_set_int32_col((cql_result_set_ref)result_set, 0, 2, new_value);
-- + extern void emit_setters_with_nullables_set_l_value(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_int64 new_value) {
-- +   cql_result_set_set_int64_col((cql_result_set_ref)result_set, 0, 3, new_value);
-- + extern void emit_setters_with_nullables_set_l_to_null(emit_setters_with_nullables_result_set_ref _Nonnull result_set) {
-- +   cql_result_set_set_int64_col((cql_result_set_ref)result_set, 0, 3, new_value);
-- + extern void emit_setters_with_nullables_set_b_value(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_bool new_value) {
-- +   cql_result_set_set_bool_col((cql_result_set_ref)result_set, 0, 4, new_value);
-- + extern void emit_setters_with_nullables_set_b_to_null(emit_setters_with_nullables_result_set_ref _Nonnull result_set) {
-- +   cql_result_set_set_bool_col((cql_result_set_ref)result_set, 0, 4, new_value);
-- + extern void emit_setters_with_nullables_set_d_value(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_double new_value) {
-- +   cql_result_set_set_double_col((cql_result_set_ref)result_set, 0, 5, new_value);
-- + extern void emit_setters_with_nullables_set_d_to_null(emit_setters_with_nullables_result_set_ref _Nonnull result_set) {
-- +   cql_result_set_set_double_col((cql_result_set_ref)result_set, 0, 5, new_value);
-- + extern void emit_setters_with_nullables_set_t(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_string_ref _Nullable new_value) {
-- +   cql_result_set_set_string_col((cql_result_set_ref)result_set, 0, 6, new_value);
-- + extern void emit_setters_with_nullables_set_bl(emit_setters_with_nullables_result_set_ref _Nonnull result_set, cql_blob_ref _Nullable new_value) {
-- +  cql_result_set_set_blob_col((cql_result_set_ref)result_set, 0, 7, new_value);
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
-- + extern void no_out_with_setters_set_id(no_out_with_setters_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 new_value)
-- + extern void no_out_with_setters_set_name(no_out_with_setters_result_set_ref _Nonnull result_set, cql_int32 row, cql_string_ref _Nullable new_value)
-- + extern void no_out_with_setters_set_rate_value(no_out_with_setters_result_set_ref _Nonnull result_set, cql_int32 row, cql_int64 new_value)
-- + extern void no_out_with_setters_set_type_value(no_out_with_setters_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 new_value)
-- + extern void no_out_with_setters_set_size_value(no_out_with_setters_result_set_ref _Nonnull result_set, cql_int32 row, cql_double new_value)
@attribute(cql:emit_setters)
create proc no_out_with_setters()
begin
  select * from bar;
end;


-- TEST: no result set items should be generated at all
-- - CQL_DATA_TYPE
-- - lotsa_columns_no_result_set_fetch_results
-- - lotsa_columns_no_result_set_get_
-- - lotsa_columns_no_result_set_data_types
-- - lotsa_columns_no_result_set_refs_offset
-- - lotsa_columns_no_result_set_col_offsets
-- - lotsa_columns_no_result_set_result_count
@attribute(cql:suppress_result_set)
create proc lotsa_columns_no_result_set()
begin
  select * from bar;
end;

-- TEST: make sure that _rc_ is set to SQLITE_OK when we return
-- + _rc_ = SQLITE_OK; // clean up any SQLITE_ROW value or other non-error
-- + goto cql_cleanup; // return
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
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64, // type
@attribute(cql:vault_sensitive=(id, name))
@attribute(cql:custom_type_for_encoded_column)
create proc vault_sensitive_with_values_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes sensitive column (name) and non sensitive column (id)
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_NOT_NULL, // title
-- + CQL_DATA_TYPE_INT64 | CQL_DATA_TYPE_NOT_NULL, // type
@attribute(cql:vault_sensitive=(id, name))
@attribute(cql:custom_type_for_encoded_column)
create proc vault_not_nullable_sensitive_with_values_proc()
begin
 select * from vault_mixed_not_nullable_sensitive;
end;

-- TEST: vault_sensitive attribute includes sensitive column (data) and non sensitive column (id)
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64 | CQL_DATA_TYPE_ENCODED, // type
@attribute(cql:vault_sensitive)
create proc vault_sensitive_with_no_values_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault union all a sensitive and non sensitive table
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64 | CQL_DATA_TYPE_ENCODED, // type
@attribute(cql:vault_sensitive)
create proc vault_union_all_table_proc()
begin
 select * from vault_mixed_sensitive
 union all
 select * from vault_non_sensitive;
end;

-- TEST: vault on alias column name
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // alias_name
@attribute(cql:vault_sensitive=alias_name)
create proc vault_alias_column_proc()
begin
 select name as alias_name from vault_mixed_sensitive;
end;

-- TEST: vault on alias column name
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // alias_name
@attribute(cql:vault_sensitive=alias_name)
create proc vault_alias_column_name_proc()
begin
 select name as alias_name from vault_mixed_sensitive;
end;

-- TEST: vault a column in cursor result
-- + cql_multifetch(_rc_, C_stmt, 1,
-- +                CQL_DATA_TYPE_STRING, &C.name);
@attribute(cql:vault_sensitive)
create proc vault_cursor_proc()
begin
  declare C cursor for select name from vault_mixed_sensitive;
  fetch c;
end;

-- TEST: vault_sensitive attribute includes encode context column (title) and sensitive column (id, name)
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64, // type
@attribute(cql:vault_sensitive=(title, (id, name)))
create proc vault_sensitive_with_context_and_sensitive_columns_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes no encode context column and sensitive column (id, name)
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_ENCODED, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64, // type
@attribute(cql:vault_sensitive=((id, name)))
create proc vault_sensitive_with_no_context_and_sensitive_columns_proc()
begin
 select * from vault_mixed_sensitive;
end;

-- TEST: vault_sensitive attribute includes encode context column (title) and no sensitive column
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
-- + CQL_DATA_TYPE_STRING, // name
-- + CQL_DATA_TYPE_STRING, // title
-- + CQL_DATA_TYPE_INT64, // type
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
-- - get
-- - set
-- - void
-- - return
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
-- + sqlite3_stmt *C_stmt = NULL;
-- + cql_object_ref C_object_ = NULL;
-- 1 for the boxing and 1 for the cleanup
-- +2 cql_object_release(C_object_);
-- + C_object_ = cql_box_stmt(C_stmt);
-- + cql_set_object_ref(result, C_object_);
-- - cql_finalize_stmt(&C);
create proc try_boxing(out result object<bar cursor>)
begin
  declare C cursor for select * from bar;
  set result from cursor C;
end;

-- TEST: simple unbox
-- + C_stmt = cql_unbox_stmt(C_object_);
-- + cql_set_object_ref(&C_object_, boxed_cursor);
-- + _rc_ = sqlite3_step(C_stmt);
-- + cql_object_release(C_object_);
-- - cql_finalize_stmt(&C);
create proc try_unboxing(boxed_cursor object<bar cursor>)
begin
  declare C cursor for boxed_cursor;
  fetch C;
end;

-- TEST: numeric cast operation int32
-- + x = ((cql_int32)(3.2));
create proc local_cast_int_notnull()
begin
  declare x integer not null;
  set x := cast(3.2 as integer);
end;

-- TEST: numeric cast operation int32 nullable
-- + cql_set_nullable(x, r.is_null, ((cql_int32)(r.value)));
create proc local_cast_int()
begin
  declare x integer;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as integer);
end;

-- TEST: numeric cast operation int64 nullable
-- + x = ((cql_int64)(3.2));
create proc local_cast_long_notnull()
begin
  declare x long not null;
  set x := cast(3.2 as long);
end;

-- TEST: numeric cast operation int64 nullable
-- + cql_set_nullable(x, r.is_null, ((cql_int64)(r.value)));
create proc local_cast_long()
begin
  declare x long;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as long);
end;

-- TEST: numeric cast operation real
-- + x = ((cql_double)(3));
create proc local_cast_real_notnull()
begin
  declare x real not null;
  set x := cast(3 as real);
end;

-- TEST: numeric cast operation real nullable
-- + cql_set_nullable(x, r.is_null, ((cql_double)(r.value)));
create proc local_cast_real()
begin
  declare x real;
  declare r int;
  set r := nullable(3);
  set x := cast(r as real);
end;

-- TEST: numeric cast operation bool (and normalize)
-- + x = ((cql_bool)!!(3.2));
create proc local_cast_bool_notnull()
begin
  declare x bool not null;
  set x := cast(3.2 as bool);
end;

-- TEST: numeric cast operation bool nullable (and normalize)
-- + cql_set_nullable(x, r.is_null, ((cql_bool)!!(r.value)));
create proc local_cast_bool()
begin
  declare x bool;
  declare r real;
  set r := nullable(3.2);
  set x := cast(r as bool);
end;

-- TEST: numeric cast operation from bool (normalize b)
-- + x = ((cql_double)!!(b));
create proc local_cast_from_bool_notnull()
begin
  declare b bool not null;
  set b := 1;
  declare x real not null;
  set x := cast(b as real);
end;

-- TEST: numeric cast operation from bool nullable (normalize b)
-- + cql_set_nullable(x, b.is_null, ((cql_double)!!(b.value)));
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
-- + x = b;
create proc local_cast_from_bool_no_op_notnull()
begin
  declare x bool not null;
  declare b bool not null;
  set b := 1;
  set x := cast(b as bool);
end;

-- TEST: numeric cast operation from bool nullable (no-op version)
-- + cql_set_nullable(x, b.is_null, b.value);
create proc local_cast_from_bool_no_op()
begin
  declare b bool;
  set b := nullable(1);
  declare x bool;
  set x := cast(b as bool);
end;

@enforce_strict cast;

-- TEST: test cql_get_blob_size codegen
-- + cql_set_notnull(l0_nullable, cql_get_blob_size(_tmp_n_blob_0));
set l0_nullable := cql_get_blob_size((select blob_var));

-- TEST: test cql_get_blob_size codegen with not null blob
-- + l2 = cql_get_blob_size(blob_var2);
set l2 := cql_get_blob_size(blob_var2);

-- TEST: test basic proc savepoint structure
-- + "SAVEPOINT base_proc_savepoint");
-- + // try
-- + "RELEASE base_proc_savepoint");
-- + catch_start% {
-- + "ROLLBACK TO base_proc_savepoint");
-- + _rc_ = cql_best_error(_rc_thrown_1);
-- + catch_end%:;
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
-- avoid hard coded divide by zero
-- + x = 1 * (4 / 3);
SET x := 1 * (4 / 3);

-- + x = 1 * 2 / 3;
SET x := 1 * 2 / 3;

-- + x = 1 + 2 / 3;
SET x := 1 + 2 / 3;

-- + x = 1 + (2 - 3);
SET x := 1 + (2 - 3);

-- + x = 1 + 2 * 3;
SET x := 1 + 2 * 3;

-- + x = 1 * (2 + 3);
SET x := 1 * (2 + 3);

-- + x = 1 - (2 + 3);
SET x := 1 - (2 + 3);

-- + x = 1 - (2 - 3);
SET x := 1 - (2 - 3);

-- + x = 1 - 2 - (2 - 3);
SET x := 1 - 2 - (2 - 3);

-- the first parens do not change eval order from left to right at all
-- + x = 1 - 2 - (2 - 3);
SET x := (1 - 2) - (2 - 3);

-- + x = 1 / 2 / 3;
SET x := 1 / 2 / 3;

-- avoid hard coded divide by zero
-- + x = 1 / (4 / 3);
SET x := 1 / (4 / 3);

-- + x = 1 / 2;
SET x := 1 / 2;

-- + x = 1 * 2 * (3 * 4)
SET x := 1 * 2 * (3 * 4);

-- the first parens don't change anything
-- the second parens could matter if it was floating point
-- + x = 1 * 2 * (3 * 4)
SET x := (1 * 2) * (3 * 4);

-- note that in C & binds tighter than | so parens are required in C
-- note that in SQL | and & are equal so this expression left associates
-- + x = (1 | 2) & 3;
SET x := 1 | 2 & 3;

-- + x = 1 | 2 & 3;
SET x := 1 | (2 & 3);

-- + x = 1 | 2 | 3
SET x := 1 | 2 | 3;

-- sub optimal but we're trying to preserve written order due to floating point
-- + x = 1 | (2 | 3)
SET x := 1 | (2 | 3);

-- + x = 1 | (3 + 4 | 5);
SET x := 1 | (3 + 4 | 5);

-- + x = 1 | 3 + (4 | 5);
SET x := 1 | 3 + (4 | 5);

-- +  x = (1 | 3) + (4 | 5);
SET x := (1 | 3) + (4 | 5);

-- + x = (1 + 2) * 5;
set x := (1 + 2) * 5;

-- + x = 1 + 2 - 1;
set x := (1 + 2) - 1;

-- + x = 1 << 2 | 3;
set x := 1 << 2 | 3;

-- + x = 1 << (2 | 3);
set x := 1 << (2 | 3);

-- + x = 1 | 2 << 3
set x := 1 | (2 << 3);

-- + x = 1 << (2 << 3);
set x := 1 << (2 << 3);

-- + x = 1 < (2 > 3);
set x := 1 < (2 > 3);

-- + x = 1 << (2 >> 3);
set x := 1 << (2 >> 3);

-- + x = 1 | (2 | 3);
set x := 1 | (2 | 3);

-- + x = 1 | 2 | 3;
set x := (1 | 2) | 3;

-- + x = 1 == (2 != 3);
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
-- + DECLARE PROC emit_rc (OUT result_code INTEGER NOT NULL) USING TRANSACTION;
-- + CQL_WARN_UNUSED cql_code emit_rc(sqlite3 *_Nonnull _db_, cql_int32 *_Nonnull result_code)
-- + cql_code _rc_ = SQLITE_OK;
-- + *result_code = SQLITE_OK;
create proc emit_rc(out result_code integer not null)
begin
  set result_code := @rc;
end;

-- TEST: ensure that we use the right result code for thrown and storage
-- this code samples the @rc value at various places, the different names
-- allow us to be sure that we're using the right code in each scope.
-- + cql_code _rc_ = SQLITE_OK;
-- + cql_int32 err = 0;
-- + cql_int32 e0 = 0;
-- + cql_int32 e1 = 0;
-- + cql_int32 e2 = 0;
-- + cql_int32 e3 = 0;
-- + cql_int32 e4 = 0;
-- + cql_int32 e5 = 0;
-- + cql_int32 e6 = 0;
-- + e0 = SQLITE_OK;
-- + err = SQLITE_OK;
-- + int32_t _rc_thrown_1 = _rc_;
-- + err = _rc_thrown_1;
-- + e1 = _rc_thrown_1;
-- + e2 = _rc_thrown_1;
-- + int32_t _rc_thrown_2 = _rc_;
-- + e3 = _rc_thrown_2;
-- + err = _rc_thrown_2;
-- + _rc_ = cql_best_error(_rc_thrown_2);
-- + e4 = _rc_thrown_1;
-- + int32_t _rc_thrown_3 = _rc_;
-- + e5 = _rc_thrown_3;
-- + printf("Error %d\n", err);
-- + e6 = SQLITE_OK;
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
-- - int32_t _rc_thrown_1 = _rc_;
-- + int32_t _rc_thrown_2 = _rc_;
-- +  _rc_ = cql_best_error(_rc_thrown_2);
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
-- - int32_t _rc_thrown_1 = _rc_;
-- + int32_t _rc_thrown_2 = _rc_;
-- +  err = _rc_thrown_2;
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

-- TEST: force these into the .h file, there will be two copies of some_ints
@emit_enums some_ints;
@emit_enums;

-- TEST: force these into the .h file, there will be two copies of some_longs
@emit_enums some_longs;

-- TEST: resolve a virtual table, note that the arguments become the declaration
-- + "CREATE VIRTUAL TABLE virt_table USING virt_module ( "
-- +   "id INTEGER, "
-- +   "t TEXT)");
create proc virtual_table_creator()
begin
  -- this will be rewritten
  create virtual table virt_table using virt_module (arguments following) as (
    id integer,
    t text
  );
end;

-- TEST: the cursor here should not have the out arg form of y
-- + C.x = 1;
-- + C.y = 1;
-- + out_arg_cursor(C.x, &C.y);
create proc out_arg_cursor(x integer not null, out y integer not null)
begin
  declare C cursor like out_arg_cursor arguments;
  fetch C from values(1,1);
  call out_arg_cursor(from C);
end;

-- TEST: create virtual table
-- + "CREATE VIRTUAL TABLE v1 USING m1");
-- + "CREATE VIRTUAL TABLE v2 USING m2 (x)");
-- + "CREATE VIRTUAL TABLE v3 USING m2 ( "
-- +   "id INTEGER)");
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
-- + "SELECT vy "
-- + "FROM virtual_with_hidden");
create proc virtual1()
begin
  select * from virtual_with_hidden;
end;

-- TEST: hidden columns may be used by name
-- +  _rc_ = cql_prepare(_db_, _result_stmt,
-- + "SELECT vx, vy "
-- + "FROM virtual_with_hidden "
-- + "WHERE vx = 2");
create proc virtual2()
begin
  select vx, vy from virtual_with_hidden where vx = 2;
end;

-- TEST: insert into the table, verify autoexpand is correct there, too
-- only "y" should be inserted here
-- + _rc_ = cql_exec(_db_,
-- + "INSERT INTO virtual_with_hidden(vy) VALUES(1)");
insert into virtual_with_hidden values(1);

-- TEST: you can use the hidden column if you do it by name
-- + _rc_ = cql_exec(_db_,
-- + "INSERT INTO virtual_with_hidden(vx, vy) VALUES(1, 2)");
insert into virtual_with_hidden(vx, vy) values(1,2);

-- TEST: get row from the bar table or else -1
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (_rc_ == SQLITE_ROW) {
-- +   cql_column_nullable_int32(_temp_stmt, 0, &_tmp_n_int_1);
-- +   cql_set_nullable(i0_nullable, _tmp_n_int_1.is_null, _tmp_n_int_1.value);
-- + }
-- + else {
-- +   cql_set_notnull(i0_nullable, - 1);
-- + }
set i0_nullable := (select type from bar if nothing -1);

-- TEST: normal code gen for if nothing throw
-- + SET i0_nullable := ( SELECT type
-- + FROM bar IF NOTHING THROW );
-- + _rc_ = cql_prepare(_db_, &_temp_stmt,
-- +   "SELECT type "
-- +     "FROM bar");
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
-- + _rc_ = sqlite3_step(_temp_stmt);
-- + if (_rc_ != SQLITE_ROW) { cql_error_trace(); goto cql_cleanup; }
set i0_nullable := (select type from bar if nothing throw);

-- TEST: get row from bar if no row or null -1
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (_rc_ == SQLITE_ROW) {
-- +   cql_column_nullable_int32(_temp_stmt, 0, &_tmp_n_int_1);
-- + }
-- + if (_rc_ == SQLITE_DONE || _tmp_n_int_1.is_null) {
-- +   i2 = - 1;
-- + } else {
-- +   i2 = _tmp_n_int_1.value;
-- + }
set i2 := (select type from bar if nothing or null -1);

-- TEST: get row from the bar table or else ""
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (_rc_ == SQLITE_ROW) {
-- +   cql_column_nullable_string_ref(_temp_stmt, 0, &_tmp_n_text_1);
-- +   cql_set_string_ref(&t0_nullable, _tmp_n_text_1);
-- + }
-- + else {
-- +   cql_set_string_ref(&t0_nullable, _literal_%_);
-- + }
set t0_nullable := (select name from bar if nothing "");

-- TEST: get row from the bar table or else "garbonzo"
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (_rc_ == SQLITE_ROW) {
-- +   cql_column_nullable_string_ref(_temp_stmt, 0, &_tmp_n_text_1);
-- + }
-- + if (_rc_ == SQLITE_DONE || !_tmp_n_text_1) {
-- +   cql_set_string_ref(&t2, _literal_%_garbonzo_);
-- + } else {
-- +   cql_set_string_ref(&t2, _tmp_n_text_1);
-- + }
set t2 := (select name from bar if nothing or null "garbonzo");


-- TEST: verify private exports and binding
-- + DECLARE PROC private_proc (OUT x INTEGER);
-- + static void private_proc(cql_nullable_int32 *_Nonnull x)
@attribute(cql:private)
create proc private_proc(out x integer)
begin
  set x := 1;
end;

-- TEST: verify that getters are not present on private out union but the fetcher is
-- + .crc = CRC_private_out_union,
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // a_field
-- + DECLARE PROC private_out_union () OUT UNION (a_field INTEGER NOT NULL);
-- + static void private_out_union_fetch_results(private_out_union_result_set_ref _Nullable *_Nonnull _result_set_) {
-- -- no getter
-- - private_out_union_get_a_field
@attribute(cql:private)
create proc private_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: verify that when alt_prefix is set, alt_prefix is the prefix of emitted function name.
-- + void c_proc_with_alt_prefix(cql_nullable_int32 *_Nonnull x)
-- - void proc_with_alt_prefix(cql_nullable_int32 *_Nonnull x)
@attribute(cql:alt_prefix=c_)
create proc proc_with_alt_prefix(out x integer)
begin
  set x := 1;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in C correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + private_out_union_fetch_results(&C_result_set_);
create proc use_private_out_union()
begin
  declare C cursor for call private_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify that getters are not present on no getters out union but the fetcher is
-- + .crc = CRC_no_getters_out_union,
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // a_field
-- + DECLARE PROC no_getters_out_union () OUT UNION (a_field INTEGER NOT NULL);
-- - static void
-- + void no_getters_out_union_fetch_results(no_getters_out_union_result_set_ref _Nullable *_Nonnull _result_set_) {
-- -- no getter
-- - no_getters_out_union_get_a_field
@attribute(cql:suppress_getters)
create proc no_getters_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in C correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + no_getters_out_union_fetch_results(&C_result_set_);
create proc use_no_getters_out_union()
begin
  declare C cursor for call no_getters_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify that getters are not present on suppress results out union but the fetcher is
-- + .crc = CRC_suppress_results_out_union,
-- + CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // a_field
-- + DECLARE PROC suppress_results_out_union () OUT UNION (a_field INTEGER NOT NULL);
-- - static void
-- + void suppress_results_out_union_fetch_results(suppress_results_out_union_result_set_ref _Nullable *_Nonnull _result_set_) {
-- -- no getter
-- - suppress_results_out_union_get_a_field
@attribute(cql:suppress_result_set)
create proc suppress_results_out_union()
begin
  declare C cursor like select 1 a_field;

  fetch C from values(1);
  out union C;
end;

-- TEST: use the private out union function in the same translation unit, it should have everything we need to call it
-- note that compiling this code in C correctly is part of the test which verifies lots of linkage in addition
-- to just these strings.
-- + suppress_results_out_union_fetch_results(&C_result_set_);
create proc use_suppress_results_out_union()
begin
  declare C cursor for call suppress_results_out_union();
  loop fetch C
  begin
    call printf("%d\n", C.a_field);
  end;
end;

-- TEST: verify private exports and binding for result set case
-- + DECLARE PROC private_result (OUT x INTEGER) (x INTEGER NOT NULL);
-- + static CQL_WARN_UNUSED cql_code private_result(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt, cql_nullable_int32 *_Nonnull x) {
-- -- cql_code private_result_fetch_results
@attribute(cql:private)
create proc private_result(out x integer)
begin
  select 1 x;
end;

-- TEST: private proc forward ref results in static prototype
-- + static void private_fwd_ref(cql_int32 x);
@attribute(cql:private)
declare proc private_fwd_ref(x integer not null);

-- TEST: ensure out args set to null for ref types
-- + void set_out_arg_ref_test(cql_string_ref _Nullable *_Nonnull x) {
-- + *(void **)x = NULL; // set out arg to non-garbage
create proc set_out_arg_ref_test(out x text)
begin
end;

-- TEST: ensure out args set to null for nullable types
-- + void set_out_arg_null_test(cql_nullable_int32 *_Nonnull x) {
-- + cql_set_null(*x); // set out arg to non-garbage
create proc set_out_arg_null_test(out x integer)
begin
end;

-- TEST: ensure out args set to null for non-null types
-- + void set_out_arg_notnull_test(cql_int32 *_Nonnull x) {
-- + *x = 0; // set out arg to non-garbage
create proc set_out_arg_notnull_test(out x integer not null)
begin
end;

declare global_cursor2 cursor like select "x" x;

-- TEST: closing a cursor should finalize its statement if it has one and values if it has them
-- + CQL_WARN_UNUSED cql_code early_close_cursor(sqlite3 *_Nonnull _db_) {
-- + cql_finalize_stmt(&global_cursor_stmt);
-- + cql_teardown_row(global_cursor2);
create proc early_close_cursor()
begin
  close global_cursor;
  close global_cursor2;
end;

-- TEST: construct a lot of variables of various types
-- + cql_double r = 0;
-- + cql_int32 i = 0;
-- + cql_int64 l = 0;
-- + cql_string_ref t = NULL;
-- + cql_nullable_int64 nl = { .is_null = 1 };
-- + cql_nullable_int32 ni = { .is_null = 1 };
-- + cql_nullable_double nr = { .is_null = 1 };
-- + cql_string_ref nt = NULL;
-- + r = 1.0;
-- + i = 1;
-- + l = _64(1);
-- + cql_set_string_ref(&t, _literal_%_T_various_lets);
-- + cql_set_notnull(nl, (~_64(2)));
-- + cql_set_notnull(ni, (2 + 2));
-- + cql_set_notnull(nr, 2.0);
-- + cql_set_string_ref(&nt, _literal_%_NT_various_lets);
-- + sl = (~_64(3));
-- + si = (3 + 3);
-- + sr = 3.0;
-- + cql_set_string_ref(&st, _literal_%_ST_various_lets);
-- - Error
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
-- +1 cql_code _rc_ = SQLITE_OK;
-- two for setting the code plus one for the init as above
-- +2 _rc_ = SQLITE_OK;
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
-- + switch (i2) {
-- + case 1:
-- + case 3:
-- + i2 = 30;
-- four code blocks one each for (1,3) and (4), (5), and default
-- +4 break;
-- + case 4:
-- + i2 = 40;
-- + default:
-- + i2 = 50;
-- case 5 must be present because there is a default, so it needs the case label and break;
-- + case 5:
-- + }
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
-- + switch (i2) {
-- + case 1:
-- + case 3:
-- + i2 = 30;
-- only two code blocks for (1,3) and (4); 5 is omitted, no default
-- +2 break;
-- + case 4:
-- + i2 = 40;
-- - default:
-- case 5 is no longer present because there is no default so we can just omit the label and save code
-- - case 5:
-- + }
switch i2
  when 1, 3 then
    set i2 := 30;
  when 4 then
    set i2 := 40;
  when 5 then nothing
end;

-- TEST: basic code gen for the switch (no default, int64)
-- + switch (l2) {
-- + case 1:
-- + case 3:
-- + i2 = 30;
-- +2 break;
-- + case _64(4):
-- + i2 = 40;
-- - default:
-- - case _64(5):
-- + }
switch l2
  when 1, 3 then
    set i2 := 30;
  when 4L then
    set i2 := 40;
  when 5 then nothing
end;

-- TEST: special case: just excluding 1, 2, 3... no statements but the ELSE
-- + switch (i2) {
-- + case 1:
-- + case 2:
-- + case 3:
-- two net cases (1,2,3) and default
-- +2 break;
-- + default:
-- + i2 = 123;
switch i2
  when 1, 2, 3 then nothing
  else
    set i2 := 123;
end;

-- TEST: use of LEAVE within a switch
-- +  switch (i2) {
-- +    case 1:
-- +      if (i2) {
-- +        break;
-- +      }
-- +      i2 = 999;
-- +3      break;
-- +    default:
-- +      i2 = 1;
-- +      break;
-- +  }
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
-- + void out_decl_test(cql_nullable_int32 x) {
-- + cql_int32 u = 0;
-- + cql_int32 v = 0;
-- +2 out2_proc(x, &u, &v);
create proc out_decl_test(x integer)
begin
  declare out call out2_proc(x, u, v);
  declare out call out2_proc(x, u, v);
end;

-- TEST: implicit declare within a loop; this is a different case because
-- sem_declare_out_call_stmt has to take care to retain the SEM_TYPE_IMPLICIT
-- flags appropriately during loop reanalysis
-- + void out_decl_loop_test(cql_nullable_int32 x) {
-- + cql_int32 u = 0;
-- + cql_int32 v = 0;
-- +2 out2_proc(x, &u, &v);
create proc out_decl_loop_test(x integer)
begin
  while 1
  begin
    declare out call out2_proc(x, u, v);
    declare out call out2_proc(x, u, v);
  end;
end;

-- TEST: most binary operations involving a null-typed argument result in null
-- + cql_set_null(add0);
-- + cql_set_null(add1);
-- + cql_set_null(bin_and0);
-- + cql_set_null(bin_and1);
-- + cql_set_null(bin_or0);
-- + cql_set_null(bin_or1);
-- + cql_set_null(div0);
-- + cql_set_null(div1);
-- + cql_set_null(ge0);
-- + cql_set_null(ge1);
-- + cql_set_null(gt0);
-- + cql_set_null(gt1);
-- + cql_set_null(le0);
-- + cql_set_null(le1);
-- + cql_set_null(like0);
-- + cql_set_null(like1);
-- + cql_set_null(lshift0);
-- + cql_set_null(lshift1);
-- + cql_set_null(lt0);
-- + cql_set_null(lt1);
-- + cql_set_null(mod0);
-- + cql_set_null(mod1);
-- + cql_set_null(mul0);
-- + cql_set_null(mul1);
-- + cql_set_null(not_like0);
-- + cql_set_null(not_like1);
-- + cql_set_null(rshift0);
-- + cql_set_null(rshift1);
-- + cql_set_null(sub0);
-- + cql_set_null(sub1);
-- - cql_set_notnull
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
-- + DECLARE PROC uses_throw () USING TRANSACTION;
-- + CQL_WARN_UNUSED cql_code uses_throw(sqlite3 *_Nonnull _db_) {
-- + _rc_ = cql_best_error(SQLITE_OK);
create proc uses_throw()
begin
  throw;
end;

-- TEST: verify that this is a DML proc even though it does nothing but ifnull_throw
-- + DECLARE PROC uses_ifnull_throw (x INTEGER) USING TRANSACTION;
-- + CQL_WARN_UNUSED cql_code uses_ifnull_throw(sqlite3 *_Nonnull _db_, cql_nullable_int32 x) {
-- + _rc_ = SQLITE_ERROR;
create proc uses_ifnull_throw(x int)
begin
   let y := ifnull_throw(x);
end;

-- +  CQL_DATA_TYPE_OBJECT | CQL_DATA_TYPE_NOT_NULL, // o
-- + cql_offsetof(out_object_row, o)
create proc out_object(o object not null)
begin
  declare C cursor like out_object arguments;
  fetch C from arguments;
  out C;
end;

-- TEST: Verify that contracts are inserted where appropriate (and not inserted
-- where not appropriate)
-- + cql_contract_argument_notnull((void *)d, 4);
-- + cql_contract_argument_notnull((void *)f, 6);
-- + cql_contract_argument_notnull((void *)h, 8);
-- + cql_contract_argument_notnull((void *)i, 9);
-- + cql_contract_argument_notnull((void *)j, 10);
-- + cql_contract_argument_notnull((void *)k, 11);
-- + cql_contract_argument_notnull((void *)l, 12);
-- + cql_contract_argument_notnull((void *)m, 13);
-- + cql_contract_argument_notnull((void *)n, 14);
-- + cql_contract_argument_notnull((void *)o, 15);
-- + cql_contract_argument_notnull_when_dereferenced((void *)p, 16);
-- +11 cql_contract_argument_notnull
-- +1 cql_contract_argument_notnull_when_dereferenced
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
-- + cql_contract_argument_notnull((void *)t, 1);
create proc public_proc_with_a_contract(t text not null)
begin
end;

-- TEST: Contracts should not be emitted for private procs
-- - cql_contract_argument_notnull((void *)t, 1);
@attribute(cql:private)
create proc private_proc_without_a_contract(t text not null)
begin
end;

-- TEST: Contracts should be emitted only in _fetch_results for result set procs
-- +1 cql_contract_argument_notnull((void *)t, 1);
create proc result_set_proc_with_contract_in_fetch_results(t text not null)
begin
  select * from bar;
end;

-- TEST: Contracts should be emitted only in _fetch_results for out procs
-- +1 cql_contract_argument_notnull((void *)t, 1);
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
-- + cql_nullable_int32 a = { .is_null = 1 };
-- + cql_int32 b = 0;
-- + b = a.value;
-- + cql_set_notnull(a, 0);
create proc nullability_improvements_do_not_change_access()
begin
  declare a int;
  if a is not null then
    let b := a;
    set a := 0;
  end if;
end;

-- TEST: a loose select statement generates no code (and will produce no errors)
-- the errors are checked when this code is compiled in C.  If the code
-- were generated there would be errors because the global proc
-- doesn't have the statement out arg.  We also verify that
-- no call to cql_prepare happens hence no select
-- - cql_prepare
select 1 x;

-- TEST: we should infer a bool not null variable and compute is true correctly
-- + true_test = !!(1);
let true_test := 1 is true;

-- TEST: we should infer a bool not null variable and compute is false correctly
-- + false_test = !(0);
let false_test := 0 is false;

-- TEST: we should infer a bool type and use the nullable version of the test
-- + true_test = cql_is_nullable_true(i0_nullable.is_null, i0_nullable.value);
set true_test := i0_nullable is true;

-- TEST: we should infer a bool type and use the nullable version of the test
-- + false_test = cql_is_nullable_false(i0_nullable.is_null, i0_nullable.value);
set false_test := i0_nullable is false;

-- TEST: we should infer a bool not null variable and compute is true correctly
-- + true_test = !(1);
set true_test := 1 is not true;

-- TEST: we should infer a bool not null variable and compute is false correctly
-- + false_test = !!(0);
set false_test := 0 is not false;

-- TEST: we should infer a bool type and use the nullable version of the test
-- + true_test = !cql_is_nullable_true(i0_nullable.is_null, i0_nullable.value);
set true_test := i0_nullable is not true;

-- TEST: we should infer a bool type and use the nullable version of the test
-- + false_test = !cql_is_nullable_false(i0_nullable.is_null, i0_nullable.value);
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

-- TEST: big test needs not string temporaries just one helper call
-- we do not want to see the "get" pattern
-- - cql_set_string_ref(&s, cql_cursor_format(&C_dyn));
-- we want to see the "create" pattern (i.e. we start with a +1 ref)
-- + cql_string_release(s);
-- + s = cql_cursor_format(&C_dyn);
-- - cql_string_release(_tmp_text
CREATE PROC BigFormat ()
BEGIN
  DECLARE C CURSOR FOR SELECT * FROM big_data;
  LOOP FETCH C
  BEGIN
    LET s := cql_cursor_format(C);
  END;
END;

-- TEST: codegen for sign
-- + _tmp_int_2 = - 2;
-- + sign_val_int = ((_tmp_int_2 > 0) - (_tmp_int_2 < 0));
LET sign_val_int := sign(-2);

-- TEST: codegen for sign: nullable arg
-- + cql_set_notnull(_tmp_n_int_2, (-2));
-- + cql_set_nullable(sign_val_nullable, _tmp_n_int_2.is_null, ((_tmp_n_int_2.value > 0) - (_tmp_n_int_2.value < 0)));
LET sign_val_nullable := sign(nullable(-2));

-- TEST: codegen for absolute value
-- + _tmp_int_2 = - 2;
-- + abs_val_int = abs(_tmp_int_2);
LET abs_val_int := abs(-2);

-- TEST: codegen for absolute value: nullable arg
-- + cql_set_notnull(_tmp_n_int_2, (-2));
-- + cql_set_nullable(abs_val_nullable, _tmp_n_int_2.is_null, abs(_tmp_n_int_2.value));
LET abs_val_nullable := abs(nullable(-2));

-- TEST: codegen for absolute value long
-- +  _tmp_int64_2 = - _64(2);
-- +  abs_val_long = labs(_tmp_int64_2);
LET abs_val_long := abs(-2L);

-- TEST: codegen for absolute value real
-- + _tmp_double_2 = - 2.0;
-- + abs_val_real = fabs(_tmp_double_2);
LET abs_val_real := abs(-2.0);

-- TEST: codegen for absolute value bool
-- + _tmp_bool_% = 1;
-- + abs_val_bool = !!_tmp_bool_%;
LET abs_val_bool := abs(true);

-- TEST: codegen for absolute value of null
-- + cql_set_null(abs_val_nullable);
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

-- TEST: Arguments are always evaluated left-to-right (which is ensured by
-- generating temps).
-- + ltor_proc_int_not_null(1, 2, &_tmp_int_%);
-- + ltor_proc_int_not_null(3, 4, &_tmp_int_%);
-- + ltor_proc_int_not_null(_tmp_int_%, _tmp_int_%, &a);
-- + ltor_proc_int(_tmp_n_int_%, _tmp_n_int_%, &_tmp_n_int_%);
-- + ltor_proc_int(_tmp_n_int_%, _tmp_n_int_%, &_tmp_n_int_%);
-- + ltor_proc_int(_tmp_n_int_%, _tmp_n_int_%, &b);
-- + ltor_proc_text_not_null(_literal_%_arg%, _literal_%_arg%, &_tmp_text_%);
-- + ltor_proc_text_not_null(_literal_%_arg%, _literal_%_arg%, &_tmp_text_%);
-- + ltor_proc_text_not_null(_tmp_text_%, _tmp_text_%, &c);
-- + ltor_proc_text(_literal_%_arg%, _literal_%_arg%, &_tmp_n_text_%);
-- + ltor_proc_text(_literal_%_arg%, _literal_%_arg%, &_tmp_n_text_%);
-- + ltor_proc_text(_tmp_n_text_%, _tmp_n_text_%, &d);
-- + _tmp_int_% = ltor_func_int_not_null(1, 2);
-- + _tmp_int_% = ltor_func_int_not_null(3, 4);
-- + e = ltor_func_int_not_null(_tmp_int_%, _tmp_int_%);
-- + _tmp_n_int_% = ltor_func_int(_tmp_n_int_%, _tmp_n_int_%);
-- + _tmp_n_int_% = ltor_func_int(_tmp_n_int_%, _tmp_n_int_%);
-- + f = ltor_func_int(_tmp_n_int_%, _tmp_n_int_%);
-- + cql_set_string_ref(&_tmp_text_%, ltor_func_text_not_null(_literal_%_arg%, _literal_%_arg%));
-- + cql_set_string_ref(&_tmp_text_%, ltor_func_text_not_null(_literal_%_arg%, _literal_%_arg%));
-- + cql_set_string_ref(&g, ltor_func_text_not_null(_tmp_text_%, _tmp_text_%));
-- + cql_set_string_ref(&_tmp_n_text_%, ltor_func_text(_literal_%_arg%, _literal_%_arg%));
-- + cql_set_string_ref(&_tmp_n_text_%, ltor_func_text(_literal_%_arg%, _literal_%_arg%));
-- + cql_set_string_ref(&h, ltor_func_text(_tmp_n_text_%, _tmp_n_text_%));
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
-- this was previously problematic because in the case of f1 we have a not null
-- result so no result variable is used, the expression for the first f1() + f1()
-- becomes something like tmp1 + tmp2 but then that part of the AST returns
-- and we reused tmp1 again for the next call resulting in tmp1 + tmp2 + tmp1
-- which is very bad indeed
--- NOT NULL CASE: NO TEMPS CAN BE REUSED!
-- +  f1(&_tmp_int_2);
-- +  f1(&_tmp_int_3);
-- +  f1(&_tmp_int_4);
-- +  q = _tmp_int_2 + _tmp_int_3 + _tmp_int_4;
--- NULLABLE CASE: TEMPS CAN BE REUSED tmp1 combines tmp2 and tmp3, tmp2 can be reused!
-- +  f2(&_tmp_n_int_2);
-- +  f2(&_tmp_n_int_3);
-- +  cql_combine_nullables(_tmp_n_int_1, _tmp_n_int_2.is_null, _tmp_n_int_3.is_null, _tmp_n_int_2.value + _tmp_n_int_3.value);
-- +  f2(&_tmp_n_int_2);
-- +  cql_combine_nullables(r, _tmp_n_int_1.is_null, _tmp_n_int_2.is_null, _tmp_n_int_1.value + _tmp_n_int_2.value);
--- NULLABLE CASE WITH BOXING: TEMPS CAN BE REUSED tmp1 combines tmp2 and tmp3, both can be reused
-- +  cql_set_notnull(_tmp_n_int_3, 0);
-- +  f3(_tmp_n_int_3, &_tmp_n_int_2);
-- +  cql_set_notnull(_tmp_n_int_4, 1);
-- +  f3(_tmp_n_int_4, &_tmp_n_int_3);
-- +  cql_combine_nullables(_tmp_n_int_1, _tmp_n_int_2.is_null, _tmp_n_int_3.is_null, _tmp_n_int_2.value + _tmp_n_int_3.value);
-- +  cql_set_notnull(_tmp_n_int_3, 2);
-- +  f3(_tmp_n_int_3, &_tmp_n_int_2);
-- + cql_combine_nullables(s, _tmp_n_int_1.is_null, _tmp_n_int_2.is_null, _tmp_n_int_1.value + _tmp_n_int_2.value);
create proc multi_call_temp_reuse()
begin
  let q := f1() + f1() + f1();
  let r := f2() + f2() + f2();
  let s := f3(0) + f3(1) + f3(2);
end;

-- TEST: The `sensitive` function is a no-op and never appears in the C output.
-- + cql_string_ref x = NULL;
-- + cql_set_string_ref(&x, _literal_%_hello_sensitive_function_is_a_no_op);
-- + _rc_ = cql_prepare(_db_, _result_stmt,
-- + "SELECT 'hello'");
create proc sensitive_function_is_a_no_op()
begin
  let x := sensitive("hello");
  select sensitive("hello") as y;
end;

-- TEST: the AND operator has unusual short circuit evaluation
-- we had a bug where the right arg was evaluated first but
-- emitted second, this caused it to clobber temps from the left
-- this test verifies that the temps inside the or are correct
-- tmp_bool_1, _2, and _3 must be distinct.
-- if you evaluate in the in the wrong order you end up with overlap
-- this is a big snip but it's necessary here
-- +  cql_combine_nullables(_tmp_n_bool_1, a.is_null, b.is_null, a.value > b.value);
-- +  if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +    cql_set_notnull(_tmp_n_bool_0, 0);
-- +  }
-- +  else {
-- +    cql_combine_nullables(_tmp_n_bool_3, a.is_null, c.is_null, a.value < c.value);
-- +    if (cql_is_nullable_true(_tmp_n_bool_3.is_null, _tmp_n_bool_3.value)) {
-- +      cql_set_notnull(_tmp_n_bool_2, 1);
-- +    }
-- +    else {
-- +      if (c.is_null) {
-- +        cql_set_notnull(_tmp_n_bool_2, 1);
-- +      }
-- +      else {
-- +        cql_set_nullable(_tmp_n_bool_2, _tmp_n_bool_3.is_null, 0);
-- +      }
-- +    }
-- +    if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +      cql_set_notnull(_tmp_n_bool_0, 0);
-- +    }
-- +    else {
-- +      cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 1);
-- +    }
-- +  }
-- +  if (cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) {
-- +    cql_set_nullable(c, a.is_null, a.value);
-- +  }
create proc and_preserves_temps(a long, b long, c long)
begin
  if a > b and (a < c or c is null) then
     set c := a;
  end if;
end;

-- TEST: the OR operator has unusual short circuit evaluation
-- we had a bug where the right arg was evaluated first but
-- emitted second, this caused it to clobber temps from the left
-- this test verifies that the temps inside the or are correct.
-- tmp_bool_1, _2, and _3 must be distinct.
-- this is a big snip but it's necessary here
-- if you evaluate in the in the wrong order you end up with overlap
-- +  cql_set_nullable(_tmp_n_bool_1, c.is_null, c.value < 0);
-- +  if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +    cql_set_notnull(_tmp_n_bool_0, 1);
-- +  }
-- +  else {
-- +    cql_combine_nullables(_tmp_n_bool_3, a.is_null, c.is_null, a.value > c.value);
-- +    if (cql_is_nullable_false(_tmp_n_bool_3.is_null, _tmp_n_bool_3.value)) {
-- +      cql_set_notnull(_tmp_n_bool_2, 0);
-- +    }
-- +    else {
-- +      cql_combine_nullables(_tmp_n_bool_4, b.is_null, c.is_null, b.value > c.value);
-- +      if (cql_is_nullable_false(_tmp_n_bool_4.is_null, _tmp_n_bool_4.value)) {
-- +        cql_set_notnull(_tmp_n_bool_2, 0);
-- +      }
-- +      else {
-- +        cql_combine_nullables(_tmp_n_bool_2, _tmp_n_bool_3.is_null, _tmp_n_bool_4.is_null, 1);
-- +      }
-- +    }
-- +    if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +      cql_set_notnull(_tmp_n_bool_0, 1);
-- +    }
-- +    else {
-- +      cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_1.is_null, _tmp_n_bool_2.is_null, 0);
-- +    }
-- +  }
-- +  if (cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) {
-- +    cql_set_nullable(c, a.is_null, a.value);
-- +  }
create proc or_preserves_temps(a long, b long, c long)
begin
  if c < 0 or (a > c and b > c) then
     set c := a;
  end if;
end;

-- TEST: make sure we don't emit this into the output
-- - cql_code
@attribute(cql:shared_fragment)
create proc shared_frag()
begin
 select 1234 shared_something; -- hence no cql_code return type
end;

-- TEST use the above
-- note that the generated string has the query parts above
-- Fragment sandwich:
-- ---- first we see the prepare_var variant
-- +  _rc_ = cql_prepare_var(_db_, _result_stmt,
-- --- three parts in this sandwich
-- +    3,
-- +  "WITH "
-- +    "shared_frag (shared_something) AS (",
-- ---- then we see the shared fragment-- note the name can be elided and it is!
-- +  "SELECT 1234",
-- ---- then we see what came after the shared fragment
-- +  ") "
-- +    "SELECT shared_something "
-- +      "FROM shared_frag"
-- +  );
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
-- + char _preds_1[5];
-- 8 variable usages
-- + char _vpreds_1[8];
-- + memset(&_preds_1[0], 0, sizeof(_preds_1));
-- + memset(&_vpreds_1[0], 0, sizeof(_vpreds_1));
-- control flow to figure out which predicates to enable
-- + _p1_x_ = 1;
-- + _preds_1[0] = 1;
-- + _vpreds_1[0] = 1; // pred 0 known to be 1
-- + if (_p1_x_ == 1) {
-- +   _preds_1[1] = 1;
-- +   _vpreds_1[1] = 1; // pred 1 known to be 1
-- + }
-- + else {
-- +   if (_p1_x_ == 2) {
-- +     _preds_1[2] = 1;
-- +     _vpreds_1[2] = 1; // pred 2 known to be 1
-- +     _vpreds_1[3] = 1; // pred 2 known to be 1
-- +   }
-- +   else {
-- +     _preds_1[3] = 1;
-- +     _vpreds_1[4] = 1; // pred 3 known to be 1
-- +     _vpreds_1[5] = 1; // pred 3 known to be 1
-- +     _vpreds_1[6] = 1; // pred 3 known to be 1
-- +   }
-- + }
-- + _preds_1[4] = 1;
-- + _vpreds_1[7] = 1; // pred 0 known to be 1
-- + _rc_ = cql_prepare_var(_db_, _result_stmt,
-- + 5, _preds_1,
--
-- root fragment 0 always present
-- + "WITH "
-- +   "some_cte (id) AS (SELECT ?), "
-- +   "shared_conditional (x) AS (",
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
-- + ") "
-- +   "SELECT bar.id, bar.name, bar.rate, bar.type, bar.size "
-- +     "FROM bar "
-- +     "INNER JOIN some_cte ON ? = 5"
--
-- 8 variable sites, only some of which are used
-- + cql_multibind_var(&_rc_, _db_, _result_stmt, 8, _vpreds_1,
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
-- + memset(&_preds_1[0], 0, sizeof(_preds_1));
-- + memset(&_vpreds_1[0], 0, sizeof(_vpreds_1));
-- + _p1_x__ = 1;
-- + _preds_1[0] = 1;
-- + if (_p1_x__ <= 5) {
-- +   _preds_1[1] = 1;
-- +   _p2_x_ = 1;
-- +   if (_p2_x_ == 1) {
-- +     _preds_1[2] = 1;
-- +     _vpreds_1[0] = 1; // pred 2 known to be 1
-- +   }
-- +   else {
-- +     if (_p2_x_ == 2) {
-- +       _preds_1[3] = 1;
-- +       _vpreds_1[1] = 1; // pred 3 known to be 1
-- +       _vpreds_1[2] = 1; // pred 3 known to be 1
-- +     }
-- +     else {
-- +       _preds_1[4] = 1;
-- +       _vpreds_1[3] = 1; // pred 4 known to be 1
-- +       _vpreds_1[4] = 1; // pred 4 known to be 1
-- +       _vpreds_1[5] = 1; // pred 4 known to be 1
-- +     }
-- +   }
-- this is what's unique about this test, we popped back to the context of predicate 1
-- +   _preds_1[5] = _preds_1[1];
-- +   _vpreds_1[6] = _preds_1[1];
-- + }
-- + else {
-- +   _preds_1[6] = 1;
-- +   _vpreds_1[7] = 1; // pred 6 known to be 1
-- + }
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
-- +  char _preds_1[10];
-- +  char _vpreds_1[8];
-- +  memset(&_preds_1[0], 0, sizeof(_preds_1));
-- +  memset(&_vpreds_1[0], 0, sizeof(_vpreds_1));
-- +  _p1_x__ = 1;
-- +  _preds_1[0] = 1;
-- +  _preds_1[1] = 1;
-- +  if (_p1_x__ <= 5) {
-- +    _preds_1[2] = 1;
-- +    _p2_x_ = 1;
-- +    if (_p2_x_ == 1) {
-- +      _preds_1[3] = 1;
-- +      _vpreds_1[0] = 1; // pred 3 known to be 1
-- +    }
-- +    else {
-- +      if (_p2_x_ == 2) {
-- +        _preds_1[4] = 1;
-- +        _vpreds_1[1] = 1; // pred 4 known to be 1
-- +        _vpreds_1[2] = 1; // pred 4 known to be 1
-- +      }
-- +      else {
-- +        _preds_1[5] = 1;
-- +        _vpreds_1[3] = 1; // pred 5 known to be 1
-- +        _vpreds_1[4] = 1; // pred 5 known to be 1
-- +        _vpreds_1[5] = 1; // pred 5 known to be 1
-- +      }
-- +    }
-- +    _preds_1[6] = _preds_1[2];
-- +    _vpreds_1[6] = _preds_1[2];
-- +  }
-- +  else {
-- +    _preds_1[7] = 1;
-- +    _vpreds_1[7] = 1; // pred 7 known to be 1
-- +  }
-- +  _preds_1[8] = 1;
-- +  _preds_1[9] = 1;
-- +  _rc_ = cql_prepare_var(_db_, _result_stmt,
-- +    10, _preds_1,
--
-- fragment 0 always present
-- +  "SELECT x "
-- +      "FROM (",
--
-- fragment 1, the nested wrapper -- always present
-- +  "WITH _ns_(x) AS (",
--
-- fragment 2 present if x <= 5
-- +  "WITH "
-- +    "shared_conditional (x) AS (",
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
-- +  ") "
-- +    "SELECT x "
-- +      "FROM shared_conditional "
-- +      "WHERE ? = 5",
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
-- + "SELECT shared_something "
-- + "FROM (",
-- + "WITH _ns_(shared_something) AS (",
-- + "SELECT 1234",
-- + ") SELECT * FROM _ns_",
-- + ")"
@attribute(cql:private)
create proc simple_shared_frag()
begin
  select * from (call shared_frag());
end;


-- used in the next test
@attribute(cql:shared_fragment)
create proc shared_frag_else_nothing(id_ integer)
begin
  if id_ > 0 then
    select id_ as id1, 'x' as text1;
  else
    select nothing;
  end if;
end;

-- TEST: select nothing expands into the right number of columns
-- + "SELECT 0,0 WHERE 0",
create proc shared_frag_else_nothing_test()
begin
  with (call shared_frag_else_nothing(5))
  select * from foo;
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
-- + _rc_ = cql_serialize_to_blob(&B, &C_dyn);
-- + _rc_ = cql_deserialize_from_blob(B, &D_dyn);
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
-- +  cql_blob_release(_tmp_n_blob_0);
-- + _tmp_n_blob_0 = make_blob();
-- + _rc_ = cql_deserialize_from_blob(_tmp_n_blob_0, &C_dyn);
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
-- +  big1 = _64(0x7fffffffffffffff);
-- +  big2 = _64(0x8000000000000000);
-- +  big3 = (_64(-9223372036854775807) - 1);
-- +  big4 = (_64(-9223372036854775807) - 1);
-- +  big5 = _64(9223372036854775807);
-- +  big6 = _64(9223372036854775807);
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
-- - struct
-- - row
-- - define
-- - extern
-- - error:
declare group var_group
begin
  declare gr_cursor cursor like select 1 x, "2" y;
  declare gr_integer integer;
  declare gr_blob_cursor cursor like structured_storage;
end;

-- TEST: emits the definitions only
-- + gr_cursor_row gr_cursor = { ._refs_count_ = 1, ._refs_offset_ = gr_cursor_refs_offset };
-- + cql_nullable_int32 gr_integer = { .is_null = 1 };
--
-- additional stuff for a cursor that needs to be serialized
-- note that the arrays are not static for a global cursor
-- + gr_blob_cursor_row gr_blob_cursor = { ._refs_count_ = 1, ._refs_offset_ = gr_blob_cursor_refs_offset };
-- + cql_uint16 gr_blob_cursor_cols[] = { 2,
-- +   cql_offsetof(gr_blob_cursor_row, id),
-- +   cql_offsetof(gr_blob_cursor_row, name)
-- + };
-- + uint8_t gr_blob_cursor_data_types[] = {
-- +   CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL,
-- +   CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_NOT_NULL
-- + };
@emit_group var_group;

-- TEST: use the global cursor for serialization
-- This sets the SERIALIZATION bit on the cursor causing it to emit more stuff
-- even though it's out of order the codegen will be affected
-- the test cases above verify this
-- + _rc_ = cql_serialize_to_blob(b, &gr_blob_cursor_dyn);
create proc use_gr_cursor_for_serialization(out b blob<structured_storage>)
begin
  set b from cursor gr_blob_cursor;
end;

-- TEST: if we mutate a reference arg then we have to track its lifetime
-- we cannot just borrow the reference, the parameter is not released
-- nor can it be...  So convert this to a normal local pattern.
-- + DECLARE PROC mutated_in_param_ref (x TEXT);
-- + void mutated_in_param_ref(cql_string_ref _Nullable _in__x) {
-- + cql_string_ref x = NULL;
-- + cql_set_string_ref(&x, _in__x);
-- + cql_set_string_ref(&x, _literal_%_hi_mutated_in_param_ref);
-- + cql_string_release(x);
create proc mutated_in_param_ref(x text)
begin
  set x := 'hi';
end;

-- TEST: likely() is correctly emitted
-- +  _rc_ = cql_prepare(_db_, &_temp_stmt,
-- + "SELECT likely(1)");
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
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
-- + cql_dynamic_cursor shape_storage_dyn = {
-- + .cursor_data = (void *)&shape_storage,
-- + .cursor_has_row = (void *)&shape_storage._has_row_,
-- + .cursor_data_types = shape_storage_data_types,
-- + .cursor_col_offsets = shape_storage_cols,
-- + result = external_cursor_func(&shape_storage_dyn);
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

-- TEST: use of in arg at in/out position requires copy
-- + void mutated_in_arg1(cql_string_ref _Nullable _in__x) {
-- + cql_string_ref x = NULL;
-- + cql_set_string_ref(&x, _in__x);
create proc mutated_in_arg1(x text)
begin
  call clobber1(x);
end;

-- TEST: use of in arg at out position requires copy
-- + void mutated_in_arg2(cql_string_ref _Nullable _in__x) {
-- + cql_string_ref x = NULL;
-- + cql_set_string_ref(&x, _in__x);
create proc mutated_in_arg2(x text)
begin
  call clobber2(x);
end;

-- TEST: use of in arg for fetch into requires copy
-- + CQL_WARN_UNUSED cql_code mutated_in_arg3(sqlite3 *_Nonnull _db_, cql_string_ref _Nullable _in__x) {
-- + cql_string_ref x = NULL;
-- + cql_set_string_ref(&x, _in__x);
create proc mutated_in_arg3(x text)
begin
  declare C cursor for select "x" x;
  fetch C into x;
end;

-- TEST: make sure the not null contract is renamed
-- + void mutated_not_null(cql_string_ref _Nonnull _in__x) {
-- + cql_contract_argument_notnull((void *)_in__x, 1);
create proc mutated_not_null(x text not null)
begin
  set x := 'xyzzy';
end;

-- TEST: declaration of an unchecked select function
declare select function no_check_select_fun no check text;

-- TEST: declaration of an unchecked table-valued select function
declare select function no_check_select_table_valued_fun no check (t text);

-- a proc that returns a value, we will use its shape below
declare proc a_proc_we_need() (id integer, t text);

-- TEST make sure we export everything we need including the recursive dependency
-- +2 DECLARE PROC a_proc_we_need () (id INTEGER, t TEXT);
-- DECLARE PROC a_proc_that_needs_dependents () OUT UNION (a_foo OBJECT<a_proc_we_need SET>, another_foo OBJECT<a_proc_we_need SET>) USING TRANSACTION;
create proc a_proc_that_needs_dependents()
begin
  declare C cursor like (a_foo object<a_proc_we_need set>, another_foo object<a_proc_we_need set>);
  fetch C using a_proc_we_need() a_foo, a_proc_we_need() another_foo;
  out union C;
end;

-- TEST: check for needed types in the args
-- + DECLARE PROC a_proc_we_need () (id INTEGER, t TEXT);
-- + DECLARE PROC another_proc_that_needs_dependents (a_foo OBJECT<a_proc_we_need SET>);
create proc another_proc_that_needs_dependents(a_foo object<a_proc_we_need set>)
begin
end;


create proc simple_child_proc()
begin
  select 1 x, 2 y;
end;

-- TEST: emit getters and setters for a simple result set set type
-- + cql_bool simple_container_proc_get_a_is_null(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + return data[row].a.is_null;
-- + cql_int32 simple_container_proc_get_a_value(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + return data[row].a.value;
-- + extern void simple_container_proc_set_a_value(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 new_value) {
-- + cql_result_set_set_int32_col((cql_result_set_ref)result_set, row, 0, new_value);
-- + extern void simple_container_proc_set_a_to_null(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_result_set_set_to_null_col((cql_result_set_ref)result_set, row, 0);
-- + cql_int32 simple_container_proc_get_b(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + return data[row].b;
-- + void simple_container_proc_set_b(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 new_value) {
-- + cql_result_set_set_int32_col((cql_result_set_ref)result_set, row, 1, new_value);
-- + simple_child_proc_result_set_ref _Nullable simple_container_proc_get_c(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + return (simple_child_proc_result_set_ref _Nullable )data[row].c;
-- + extern void simple_container_proc_set_c(simple_container_proc_result_set_ref _Nonnull result_set, cql_int32 row, simple_child_proc_result_set_ref _Nullable new_value) {
-- + cql_result_set_set_object_col((cql_result_set_ref)result_set, row, 2, (cql_object_ref)new_value);
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

-- these are largely no-op directives until we generate SQL for them
-- at this point we just make sure we can generate these without crashing

@blob_get_key_type bgetkey_type;
@blob_get_val_type bgetval_type;
@blob_get_key bgetkey offset;
@blob_get_val bgetval;
@blob_create_key bcreatekey offset;
@blob_create_val bcreateval;
@blob_update_key bupdatekey offset;
@blob_update_val bupdateval;

@attribute(cql:backing_table)
create table backing(
  k blob primary key,
  v blob
);

@attribute(cql:backed_by=backing)
create table backed(
  pk int primary key,
  flag bool not null,
  id long,
  name text,
  age real,
  storage blob
);

@attribute(cql:backed_by=backing)
create table backed2(
  pk1 int,
  pk2 int,
  flag bool not null,
  id long,
  name text,
  extra int,
  primary key(pk1, pk2)
);

-- TEST: cql_blob_get should expand to the correct calls and hash codes
-- + "SELECT bgetkey(k, 0), bgetval(v, 1055660242183705531), bgetval(v, -7635294210585028660), bgetval(v, -9155171551243524439), bgetval(v, -6946718245010482247), bgetval(v, -3683705396192132539) "
create proc use_cql_blob_get_backed()
begin
  declare C cursor for select
    cql_blob_get(k, backed.pk),
    cql_blob_get(v, backed.flag),
    cql_blob_get(v, backed.storage),
    cql_blob_get(v, backed.id),
    cql_blob_get(v, backed.name),
    cql_blob_get(v, backed.age) from backing;
end;

-- TEST: cql_blob_get should expand to the correct calls and hash codes
-- + "SELECT bgetkey(k, 0), bgetkey(k, 1), bgetval(v, -9155171551243524439), bgetval(v, 4605090824299507084), bgetval(v, -6946718245010482247) "
create proc use_cql_blob_get_backed2()
begin
  declare C cursor for select
    cql_blob_get(k, backed2.pk1),
    cql_blob_get(k, backed2.pk2),
    cql_blob_get(v, backed2.id),
    cql_blob_get(v, backed2.extra),
    cql_blob_get(v, backed2.name) from backing;
end;

-- TEST: we should have created a shared fragment called _backed
-- + _backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM _backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
create proc use_generated_fragment()
begin
  with (call _backed())
  select * from _backed;
end;

-- TEST: we swap in the shared fragment and get the columns from it
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
create proc use_backed_table_directly()
begin
  select * from backed;
end;

-- TEST: we swap in the shared fragment and get the columns from it
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
-- verify this is a NOT result set proc
-- - sqlite3_stmt *_Nullable *_Nonnull _result_stmt
create proc use_backed_table_with_cursor()
begin
  declare C cursor for select * from backed;
end;

-- TEST: we swap in the shared fragment and get the columns from it
-- + one (x) AS (SELECT 1),
-- + two (x) AS (SELECT 2)
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
-- verify this is a result set proc
-- + sqlite3_stmt *_Nullable *_Nonnull _result_stmt
create proc use_backed_table_directly_in_with_select()
begin
  with one(*) as (select 1 x), two(*) as (select 2 x)
  select * from backed;
end;

-- TEST: we swap in the shared fragment and get the columns from it
-- + one (x) AS (SELECT 1),
-- + two (x) AS (SELECT 2)
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
-- verify this is NOT a result set proc
-- - sqlite3_stmt *_Nullable *_Nonnull _result_stmt
create proc use_backed_table_with_select_and_cursor()
begin
  declare C cursor for
  with one(*) as (select 1 x), two(*) as (select 2 x)
  select * from backed;
end;

-- TEST: select expression with backed table
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT flag
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
-- verify this is NOT a result set proc
-- - sqlite3_stmt *_Nullable *_Nonnull _result_stmt
create proc use_backed_table_select_expr(out x bool not null)
begin
  set x := (select flag from backed);
end;

-- TEST: explain query plan with replacement
-- + EXPLAIN QUERY PLAN
-- + backed (rowid, pk, flag, id, name, age, storage) AS (CALL _backed())
-- + SELECT rowid, bgetkey(T.k, 0),
-- + bgetval(T.v, 1055660242183705531),
-- + bgetval(T.v, -9155171551243524439),
-- + bgetval(T.v, -6946718245010482247),
-- + bgetval(T.v, -3683705396192132539),
-- + bgetval(T.v, -7635294210585028660
-- + FROM backing AS T
-- + SELECT rowid, pk, flag, id, name, age, storage
-- + FROM backed
-- + WHERE bgetkey_type(T.k) = -5417664364642960231
-- verify this is a result set proc
-- + sqlite3_stmt *_Nullable *_Nonnull _result_stmt
@attribute(cql:private)
create proc explain_query_plan_backed(out x bool not null)
begin
  explain query plan select * from backed;
end;

-- try the path where we use offsets in the value blob
@blob_get_val bgetval offset;

-- TEST: we should get value indexes 0, 1, 2, 3, 4 not hashes
-- + SELECT rowid, bgetkey(T.k, 0), bgetval(T.v, 0), bgetval(T.v, 1), bgetval(T.v, 2), bgetval(T.v, 3), bgetval(T.v, 4)
create proc use_backed_table_select_expr_value_offsets(out x bool not null)
begin
  set x := (select flag from backed);
end;

-- go back to the other way
@blob_get_val bgetval;

@attribute(cql:backed_by=backing)
create table small_backed(
  pk int primary key,
  x text,
  y real
);

-- TEST: simple insert with values
-- + _vals (pk, x, y) AS (VALUES(1, '2', 3.14), (4, '5', 6), (7, '8', 9.7))
-- + INSERT INTO backing(k, v) SELECT
-- + bcreatekey(-4190907309554122430, V.pk, 1),
-- + bcreateval(-4190907309554122430, 7953209610392031882, V.x, 4, 3032304244189539277, V.y, 3)
-- + FROM _vals AS V
create proc insert_backed_values()
begin
  insert into small_backed values(1, "2", 3.14),  (4, "5", 6),  (7, "8", 9.7);
end;

-- TEST: simple with-insert using values
-- + U (x, y, z) AS (VALUES(1, '2', 3.14))
-- + V (x, y, z) AS (VALUES(1, '2', 3.14))
-- + _vals (pk, x, y) AS (SELECT x, y, z
-- + FROM V)
-- + INSERT INTO backing(k, v) SELECT
-- + bcreatekey(-4190907309554122430, V.pk, 1)
-- + bcreateval(-4190907309554122430, 7953209610392031882, V.x, 4, 3032304244189539277, V.y, 3) "
-- + FROM _vals AS V
create proc insert_backed_values_using_with()
begin
  with
    U(x,y,z) as (values (1, "2", 3.14)), -- just here to verify that we can keep many CTES
    V(x,y,z) as (values (1, "2", 3.14))
  insert into small_backed select * from V;
end;

-- TEST: simple insert using form
-- + _vals (pk, x, y) AS (VALUES(1, '2', 3.14))
-- + INSERT INTO backing(k, v) SELECT
-- + bcreatekey(-4190907309554122430, V.pk, 1)
-- + bcreateval(-4190907309554122430, 7953209610392031882, V.x, 4, 3032304244189539277, V.y, 3)
-- + FROM _vals AS V
create proc insert_backed_values_using_form()
begin
  insert into small_backed using 1 pk, "2" x, 3.14 y;
end;

-- TEST: insert from a select
-- + small_backed (rowid, pk, x, y) AS (
-- + SELECT rowid, bgetkey(T.k, 0) AS pk, bgetval(T.v, 7953209610392031882) AS x, bgetval(T.v, 3032304244189539277) AS y
-- + FROM backing AS T
-- + WHERE bgetkey_type(T.k) = -4190907309554122430
-- + _vals (pk, x, y) AS (SELECT pk + 1000, B.x || 'x', B.y + 50
-- + FROM small_backed AS B)
-- + INSERT INTO backing(k, v) SELECT bcreatekey(-4190907309554122430, V.pk, 1), bcreateval(-4190907309554122430, 7953209610392031882, V.x, 4, 3032304244189539277, V.y, 3)
-- + bcreatekey(-4190907309554122430, V.pk, 1)
-- + bcreateval(-4190907309554122430, 7953209610392031882, V.x, 4, 3032304244189539277, V.y, 3) "
-- + FROM _vals AS V
create proc inserted_backed_from_select()
begin
  insert into small_backed select pk+1000, B.x||'x', B.y+50 from small_backed B;
end;

-- TEST: delete from backed
-- + small_backed (rowid, pk, x, y)
-- + DELETE FROM backing WHERE rowid IN (SELECT rowid
-- + FROM small_backed
-- + WHERE pk = 12345)
create proc delete_from_backed()
begin
  delete from small_backed where pk = 12345;
end;

-- TEST: delete from backed with no where clause
-- + small_backed (rowid, pk, x, y)
-- + DELETE FROM backing WHERE rowid IN (SELECT rowid
-- + FROM small_backed)
-- + v (x) AS (VALUES(1)
create proc delete_from_backed_no_where_clause()
begin
  with v(x) as (values(1)) -- force the with select form
  delete from small_backed;
end;

-- TEST: use cql_blob_update and validate hash codes etc.
-- + SELECT bupdatekey(?, 0, 1)
-- + SELECT bupdateval(?, -3683705396192132539, 21, 3, -6946718245010482247, 'dave', 4)
create proc test_blob_update_expand()
begin
  declare b blob;
  let x := (select cql_blob_update(b, 1, backed.pk));
  let z := (select cql_blob_update(b, 21, backed.age, "dave", backed.name));
end;


-- TEST: simple update into backed table value only
-- + UPDATE backing
-- + SET v = bupdateval(v, -6946718245010482247, 'foo', 4) 
-- + WHERE rowid IN (SELECT rowid
-- + FROM backed
-- + WHERE name = 'one')
create proc update_backed_set_value()
begin
  update backed set name = 'foo' where name = 'one';
end;

-- TEST: simple update into backed table value only, using with clause
-- + V (x) AS (VALUES(1))
-- + UPDATE backing
-- + SET v = bupdateval(v, -6946718245010482247, 'goo', 4)
-- + WHERE rowid IN (SELECT rowid
-- + FROM backed
-- + WHERE name = 'with_update')
create proc update_backed_with_clause()
begin
  with V(x) as (values(1)) -- force a with clause
  update backed set name = 'goo' where name = 'with_update';
end;

-- TEST: simple update into backed table key only
-- + UPDATE backing
-- + SET k = bupdatekey(k, 0, 100)
-- + WHERE rowid IN (SELECT rowid
-- + FROM backed
-- + WHERE name = 'two')
create proc update_backed_set_key()
begin
  update backed set pk = 100  where name = 'two';
end;

-- TEST: update key and value, add other clauses
-- + UPDATE backing
-- + SET k = bupdatekey(k, 0, 100)
-- + v = bupdateval(v, -3683705396192132539, 77, 3)
-- + WHERE rowid IN (SELECT rowid
-- + FROM backed
-- + WHERE name = 'three'
-- + ORDER BY age
-- + LIMIT 7)
create proc update_backed_set_both()
begin
  update backed set pk = 100, age = 77 where name = 'three' order by age limit 7;
end;

--------------------------------------------------------------------
-------------------- add new tests before this point ---------------
--------------------------------------------------------------------
let this_is_the_end := 0xf00d;

create proc end_proc() begin end;

-- TEST: end marker -- this is the last test
-- + cql_nullable_int32 end_marker = { .is_null = 1 };
-- + cql_code cql_startup(sqlite3 *_Nonnull _db_)
declare end_marker integer;
--------------------------------------------------------------------

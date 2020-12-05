/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

-- TEST: assign eveything not null
-- Note: semantic analysis verifies no chance of
--       assigning nullable to not nullable
-- + i2 = 1;
set i2 := 1;

-- TEST: assign rhs not null
-- + cql_set_notnull(i1_nullable, 88);
set i1_nullable := 88;

-- TEST: assign everything nullable
-- + cql_set_nullable(i0_nullable, i1_nullable.is_null, i1_nullable.value);
set i0_nullable := i1_nullable;

-- TEST: assign NULL to nullable string
-- + cql_set_string_ref(&t0_nullable, NULL);
set t0_nullable := null;

-- + cql_set_string_ref(&t0_nullable, t2);
set t0_nullable := t2;

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
-- +    _tmp_n_int_3 = side_effect1();
-- +    if (!_tmp_n_int_3.is_null) {
-- +      _tmp_int_2 = _tmp_n_int_3.value;
-- +      break;
-- +    }
-- +    _tmp_int_2 = 7;
-- +  } while (0);
-- +  if (!(_tmp_int_2)) {
-- +    _tmp_bool_0 = 0;
-- +  }
-- +  else {
-- +      _tmp_n_int_2 = side_effect2();
-- +      if (!_tmp_n_int_2.is_null) {
-- +        _tmp_int_1 = _tmp_n_int_2.value;
-- +        break;
-- +      }
-- +      _tmp_int_1 = 5;
-- +    } while (0);
-- +    _tmp_bool_0 = !!(_tmp_int_1);
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
-- +    _tmp_n_int_3 = side_effect1();
-- +    if (!_tmp_n_int_3.is_null) {
-- +      _tmp_int_2 = _tmp_n_int_3.value;
-- +      break;
-- +    }
-- +    _tmp_int_2 = 7;
-- +  } while (0);
-- +  if (_tmp_int_2) {
-- +    _tmp_bool_0 = 1;
-- +  }
-- +  else {
-- +    do {
-- +      _tmp_n_int_2 = side_effect2();
-- +      if (!_tmp_n_int_2.is_null) {
-- +        _tmp_int_1 = _tmp_n_int_2.value;
-- +        break;
-- +      }
-- +      _tmp_int_1 = 5;
-- +    } while (0);
-- +    _tmp_bool_0 = !!(_tmp_int_1);
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

-- TEST: is null test non-null subexpression
-- + i2 = 0;
set i2 := (1+2*3) is null;

-- TEST: is null test general case
-- + cql_combine_nullables(_tmp_n_int_0, i0_nullable.is_null, i1_nullable.is_null, i0_nullable.value + i1_nullable.value);
-- + i2 = _tmp_n_int_0.is_null;
set i2 := (i0_nullable + i1_nullable) is null;

-- TEST: is not null basic test
-- + i2 = !1;
set i2 := null is not null;

-- TEST: is not null test non-null subexpression
-- + i2 = 1;
set i2 := (1+2*3) is not null;

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

-- TEST: simple between
-- + SET b2 := BETWEEN REWRITE _between_0_ := 1 CHECK (_between_0_ >= 0 AND _between_0_ <= 3);
-- + _between_0_ = 1;
-- + b2 = _between_0_ >= 0 && _between_0_ <= 3;
set b2 := 1 between 0 and 3;

-- TEST: between with some nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_1_ := i1_nullable CHECK (_between_1_ >= i0_nullable AND _between_1_ <= r2);
-- + cql_set_nullable(_between_1_, i1_nullable.is_null, i1_nullable.value);
-- + cql_combine_nullables(_tmp_n_bool_2, _between_1_.is_null, i0_nullable.is_null, _between_1_.value >= i0_nullable.value);
-- + if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_set_nullable(_tmp_n_bool_1, _between_1_.is_null, _between_1_.value <= r2);
-- +   if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_2.is_null, _tmp_n_bool_1.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable between i0_nullable and r2;

-- TEST: between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_2_ := i1_nullable CHECK (_between_2_ >= r2 AND _between_2_ <= i0_nullable);
-- + cql_set_nullable(_between_2_, i1_nullable.is_null, i1_nullable.value);
-- + cql_set_nullable(_tmp_n_bool_2, _between_2_.is_null, _between_2_.value >= r2);
-- + if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_1, _between_2_.is_null, i0_nullable.is_null, _between_2_.value <= i0_nullable.value);
-- +   if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_2.is_null, _tmp_n_bool_1.is_null, 1);
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
-- + cql_combine_nullables(_tmp_n_bool_2, _between_4_.is_null, i0_nullable.is_null, _between_4_.value < i0_nullable.value);
-- + if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_set_nullable(_tmp_n_bool_1, _between_4_.is_null, _between_4_.value > r2);
-- +   if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_2.is_null, _tmp_n_bool_1.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(i0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set i0_nullable := i1_nullable not between i0_nullable and r2;

-- TEST: not between with different nullables
-- + SET i0_nullable := BETWEEN REWRITE _between_5_ := i1_nullable CHECK (_between_5_ < r2 OR _between_5_ > i0_nullable);
-- + cql_set_nullable(_between_5_, i1_nullable.is_null, i1_nullable.value);
-- + cql_set_nullable(_tmp_n_bool_2, _between_5_.is_null, _between_5_.value < r2);
-- + if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_1, _between_5_.is_null, i0_nullable.is_null, _between_5_.value > i0_nullable.value);
-- +   if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_combine_nullables(_tmp_n_bool_0, _tmp_n_bool_2.is_null, _tmp_n_bool_1.is_null, 0);
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
-- + cql_nullable_int64 longint_var;
declare longint_var long integer;

-- + cql_combine_nullables(_tmp_n_int64_1, l0_nullable.is_null, l1_nullable.is_null, l0_nullable.value + l1_nullable.value);
-- + cql_set_nullable(longint_var, _tmp_n_int64_1.is_null, _tmp_n_int64_1.value * 5);
set longint_var := (l0_nullable + l1_nullable) * 5;

-- TEST: make a cursor
-- + _rc_ = cql_prepare(_db_, &foo_cursor,
-- + "SELECT id, ? "
-- + "FROM foo "
-- + "WHERE id = ?"
-- + cql_multibind(&_rc_, _db_, &foo_cursor, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, i2,
-- +               CQL_DATA_TYPE_INT32, &i0_nullable);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
declare foo_cursor cursor for select id, i2 from foo where id = i0_nullable;

-- TEST: fetch a cursor
-- + _rc_ = sqlite3_step(foo_cursor);
-- + _rc_ = sqlite3_step(foo_cursor);
-- + _foo_cursor_has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, foo_cursor, 2,
-- +                CQL_DATA_TYPE_INT32, &i0_nullable,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &i2);
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
fetch foo_cursor into i0_nullable, i2;

-- TEST: test elementary cursor on select with no tables, still round trips through sqlite
declare col1 integer;
declare col2 real not null;
-- + _rc_ = cql_prepare(_db_, &basic_cursor,
declare basic_cursor cursor for select 1, 2.5;
open basic_cursor;
-- + cql_multifetch(_rc_, basic_cursor, 2,
-- +                CQL_DATA_TYPE_INT32, &col1,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_DOUBLE, &col2);
fetch basic_cursor into col1, col2;
-- + cql_finalize_stmt(&basic_cursor);
close basic_cursor;

-- TEST: the most expensive way to swap two variables ever :)
declare arg1 integer not null;
declare arg2 integer not null;
set arg1 := 7;
set arg2 := 11;
-- + _rc_ = cql_prepare(_db_, &exchange_cursor,
-- + cql_multibind(&_rc_, _db_, &exchange_cursor, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, arg2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, arg1);
declare exchange_cursor cursor for select arg2, arg1;
open exchange_cursor;
-- + cql_multifetch(_rc_, exchange_cursor, 2,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &arg1,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &arg2);
fetch exchange_cursor into arg1, arg2;
-- + cql_finalize_stmt(&exchange_cursor);
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
-- + _rc_ = cql_best_error(_rc_thrown_);
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
-- + cql_set_string_ref(&_between_6_, _literal_2_b_);
-- + b2 = cql_string_compare(_between_6_, _literal_4_a_) >= 0 && cql_string_compare(_between_6_, _literal_3_c_) <= 0;
set b2 := 'b' between 'a' and 'c';

-- TEST: between with nullable strings right
-- + SET b0_nullable := BETWEEN REWRITE _between_7_ := 'b' CHECK (_between_7_ >= 'a' AND _between_7_ <= t0_nullable);
-- + cql_set_string_ref(&_between_7_, _literal_2_b_);
-- + if (!(cql_string_compare(_between_7_, _literal_4_a_) >= 0)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_1, !_between_7_, !t0_nullable, cql_string_compare(_between_7_, t0_nullable) <= 0);
-- +   if (cql_is_nullable_false(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between 'a' and t0_nullable;

-- TEST: between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_8_ := 'b' CHECK (_between_8_ >= t0_nullable AND _between_8_ <= 'c');
-- + cql_set_string_ref(&_between_8_, _literal_2_b_);
-- + cql_combine_nullables(_tmp_n_bool_2, !_between_8_, !t0_nullable, cql_string_compare(_between_8_, t0_nullable) >= 0);
-- + if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   if (!(cql_string_compare(_between_8_, _literal_3_c_) <= 0)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between t0_nullable and 'c';

-- TEST: between with nullable strings null operand
-- + SET b0_nullable := BETWEEN REWRITE _between_9_ := 'b' CHECK (_between_9_ >= NULL AND _between_9_ <= 'c');
-- + cql_set_string_ref(&_between_9_, _literal_2_b_);
-- + cql_set_null(_tmp_n_bool_2);
-- + if (cql_is_nullable_false(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 0);
-- + }
-- + else {
-- +   if (!(cql_string_compare(_between_9_, _literal_3_c_) <= 0)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 0);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 1);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' between null and 'c';

-- TEST: not between with strings
-- + SET b2 := BETWEEN REWRITE _between_10_ := 'b' CHECK (_between_10_ < 'a' OR _between_10_ > 'c');
-- + cql_set_string_ref(&_between_10_, _literal_2_b_);
-- + b2 = cql_string_compare(_between_10_, _literal_4_a_) < 0 || cql_string_compare(_between_10_, _literal_3_c_) > 0;
set b2 := 'b' not between 'a' and 'c';

-- TEST: not between with nullable strings right
-- + SET b0_nullable := BETWEEN REWRITE _between_11_ := 'b' CHECK (_between_11_ < 'a' OR _between_11_ > t0_nullable);
-- + cql_set_string_ref(&_between_11_, _literal_2_b_);
-- + if (cql_string_compare(_between_11_, _literal_4_a_) < 0) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   cql_combine_nullables(_tmp_n_bool_1, !_between_11_, !t0_nullable, cql_string_compare(_between_11_, t0_nullable) > 0);
-- +   if (cql_is_nullable_true(_tmp_n_bool_1.is_null, _tmp_n_bool_1.value)) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +      cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_1.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between 'a' and t0_nullable;

-- TEST: not between with nullable strings left
-- + SET b0_nullable := BETWEEN REWRITE _between_12_ := 'b' CHECK (_between_12_ < t0_nullable OR _between_12_ > 'c');
-- + cql_set_string_ref(&_between_12_, _literal_2_b_);
-- + cql_combine_nullables(_tmp_n_bool_2, !_between_12_, !t0_nullable, cql_string_compare(_between_12_, t0_nullable) < 0);
-- + if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   if (cql_string_compare(_between_12_, _literal_3_c_) > 0) {
-- +      cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between t0_nullable and 'c';

-- TEST: not between with nullable strings null operand
-- verify rewrite
-- + SET b0_nullable := BETWEEN REWRITE _between_13_ := 'b' CHECK (_between_13_ < NULL OR _between_13_ > 'c');
-- + cql_set_string_ref(&_between_%, _literal_%_b_);
-- + cql_set_null(_tmp_n_bool_2);
-- + if (cql_is_nullable_true(_tmp_n_bool_2.is_null, _tmp_n_bool_2.value)) {
-- +   cql_set_notnull(_tmp_n_bool_0, 1);
-- + }
-- + else {
-- +   if (cql_string_compare(_between_%_, _literal_3_c_) > 0) {
-- +     cql_set_notnull(_tmp_n_bool_0, 1);
-- +   }
-- +   else {
-- +     cql_set_nullable(_tmp_n_bool_0, _tmp_n_bool_2.is_null, 0);
-- +   }
-- + }
-- + cql_set_nullable(b0_nullable, _tmp_n_bool_0.is_null, _tmp_n_bool_0.value);
set b0_nullable := 'b' not between null and 'c';

-- TEST: this procedure will have a structured semantic type
-- + cql_string_literal(with_result_set_stored_procedure_name, "with_result_set");
-- + cql_code with_result_set(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_) {
-- + #define with_result_set_refs_offset cql_offsetof(with_result_set_row, name) // count = 1
-- + cql_int32 with_result_set_get_id(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_string_ref _Nullable with_result_set_get_name(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_bool with_result_set_get_rate_is_null(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int64 with_result_set_get_rate_value(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 with_result_set_get_type_value(with_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- + cql_int32 with_result_set_result_count(with_result_set_result_set_ref _Nonnull result_set) {
-- + CQL_WARN_UNUSED cql_code with_result_set_fetch_results(sqlite3 *_Nonnull _db_, with_result_set_result_set_ref _Nullable *_Nonnull result_set) {
-- + if (_rc_ == SQLITE_OK && !*_result_) _rc_ = SQLITE_ERROR;
create procedure with_result_set()
begin
  select * from bar;
end;

-- TEST: grabs values from a view that is backed by a table
-- + cql_code select_from_view(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_) {
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
-- + cql_code get_data(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_, cql_string_ref _Nonnull name_, cql_int32 id_) {
create procedure get_data(name_ text not null, id_ integer not null)
begin
  select * from bar where id = id_ and name = name_;
end;

-- TEST: create a proc that uses the new cursor fetch strategy
--       then bind values from those implicit variables in a CQL statement
--       and also bind the _has_rows auto local as well
-- validate auto variable management
-- + #define easy_fetch_C_refs_offset cql_offsetof(easy_fetch_C_row, name) // count = 1
-- + easy_fetch_C_row C_ = { ._refs_count_ = 1, ._refs_offset_ = easy_fetch_C_refs_offset };
-- + sqlite3_stmt *C2 = NULL;
-- + cql_bool _C2_has_row_ = 0;
-- + C_._has_row_ = _rc_ == SQLITE_ROW;
-- + cql_multifetch(_rc_, C, 5,
-- +                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C_.id,
-- +                CQL_DATA_TYPE_STRING, &C_.name,
-- +                CQL_DATA_TYPE_INT64, &C_.rate,
-- +                CQL_DATA_TYPE_INT32, &C_.type,
-- +                CQL_DATA_TYPE_DOUBLE, &C_.size);
-- + cql_multibind(&_rc_, _db_, &C2, 2,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_BOOL, C_._has_row_,
-- +               CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, C_.id);
-- + cql_finalize_stmt(&C);
-- + cql_teardown_row(C_);
-- + cql_finalize_stmt(&C2);
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

-- TEST: make sure that we use the canonical name for 's' in codegen not 'S'.  Even though S is legal.
-- + cql_set_string_ref(&s, _literal%x_);
set S := 'x';

-- TEST: declare proc and call it
-- + /*
-- + DECLARE PROC xyzzy (id INTEGER) (A INTEGER NOT NULL);
-- + */
declare proc xyzzy(id integer) ( A integer not null );

-- + _rc_ = xyzzy(_db_, &xyzzy_cursor, _tmp_n_int_%);
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
  select cast(1 as bool) as _bool,
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
-- + cql_multifetch(_rc_, C, 1,
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
-- + cql_multifetch(_rc_, C, 1,
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
declare result integer;

-- TEST: call external function
-- + cql_set_notnull(_tmp_n_int_2, 2);
-- + result = simple_func(_tmp_n_int_2);
set result := simple_func(2);

-- TEST: call external function
-- + cql_set_notnull(_tmp_n_int_3, 1);
-- + _tmp_n_int_1 = simple_func(_tmp_n_int_3);
-- + result = simple_func(_tmp_n_int_1);
set result := simple_func(simple_func(1));

declare function text_func(int1 integer, int2 integer not null) text not null;
declare text_result text;

-- TEST: call external text function
-- + cql_set_notnull(_tmp_n_int_2, 123);
-- + cql_set_string_ref(&_tmp_text_0, text_func(_tmp_n_int_2, 456));
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

-- TEST: assign var to object variable
-- + cql_set_object_ref(&obj_var, obj_var2);
set obj_var := obj_var2;

-- TEST: object comparison
-- +  cql_combine_nullables(b0_nullable, !obj_var, !obj_var, obj_var == obj_var);
set b0_nullable := obj_var == obj_var;

-- TEST: object variable in IN clause
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
set b0_nullable := obj_var in (obj_var, obj_var);

-- TEST: object variable in IN clause
-- + if (_tmp_object_2 == obj_var2) break;
set b2 := obj_var2 in (obj_var2, obj_var2);

-- TEST: object variable in NOT IN clause
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
-- + if (cql_is_nullable_true(!obj_var, _tmp_n_object_% == obj_var)) break;
set b0_nullable := obj_var not in (obj_var, obj_var);

-- TEST: object variable in NOT IN clause
-- + if (_tmp_object_2 == obj_var2) break;
set b2 := obj_var2 not in (obj_var2, obj_var2);

-- TEST: proc with object args
-- + void obj_proc(cql_object_ref _Nullable *_Nonnull an_object)
create proc obj_proc(out an_object object)
begin
  set an_object := null;
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

-- TEST: assign nullable to object with helper (not create)
-- + cql_set_object_ref(&_tmp_n_object_0, obj_func());
-- + cql_invariant(!!_tmp_n_object_0);
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := attest_notnull(obj_func());

-- TEST: assign nullable to object with helper (with create)
-- + cql_object_release(_tmp_n_object_0);
-- + _tmp_n_object_0 = obj_func_create();
-- + cql_invariant(!!_tmp_n_object_0);
-- + cql_set_object_ref(&obj_var2, _tmp_n_object_0);
set obj_var2 := attest_notnull(obj_func_create());

-- TEST: assign nullable int to an integer
-- + cql_invariant(!i0_nullable.is_null);
-- + i2 = i0_nullable.value
set i2 := attest_notnull(i0_nullable);

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
-- + cql_code uses_proc_for_result(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_)
-- + *_result_ = NULL;
-- + _rc_ = with_result_set(_db_, &*_result_);
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
-- + _rc_ = cql_prepare(_db_, &expanded_select,
-- + "SELECT id, name, rate, type, size "
-- + "FROM bar"
declare expanded_select cursor for select * from bar;

-- TEST: for expand of select * columns from table
-- + "SELECT bar.id, bar.name, bar.rate, bar.type, bar.size "
-- + "FROM bar"
declare table_expanded_select cursor for select bar.* from bar;

-- TEST: use a long literal
-- + l2 = 3147483647L;
set l2 := 3147483647L;

-- TEST: use a long literal
-- + l2 = 3147483647L;
set l2 := 3147483647;

-- TEST: use drop index in a proc
-- + "DROP INDEX index_1"
create proc index_dropper()
begin
  drop index index_1;
end;

-- TEST: simple DML statements for schema_util cg
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

-- TEST: assign null to blob variable
-- + cql_set_blob_ref(&blob_var, NULL);
set blob_var := null;

-- TEST: assign var to blob variable
-- + cql_set_blob_ref(&blob_var, blob_var2);
set blob_var := blob_var2;

-- TEST: blob comparison
-- + cql_combine_nullables(b0_nullable, !blob_var, !blob_var, blob_var == blob_var);
set b0_nullable := blob_var == blob_var;

-- TEST: blob variable in IN clause
-- + if (cql_is_nullable_true(!blob_var, _tmp_n_blob_% == blob_var)) break;
-- + if (cql_is_nullable_true(!blob_var, _tmp_n_blob_% == blob_var)) break;
set b0_nullable := blob_var in (blob_var, blob_var);

-- TEST: blob variable in IN clause
-- + if (_tmp_blob_2 == blob_var2) break;
set b2 := blob_var2 in (blob_var, blob_var2);

-- TEST: blob variable in NOT IN clause
-- + if (cql_is_nullable_true(!blob_var, _tmp_n_blob_% == blob_var)) break;
-- + if (cql_is_nullable_true(!blob_var, _tmp_n_blob_% == blob_var)) break;
set b0_nullable := blob_var not in (blob_var, blob_var);

-- TEST: blob variable in NOT IN clause
-- + if (_tmp_blob_2 == blob_var2) break;
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
-- + out_cursor_proc_C_row C_ = { ._refs_count_ = 3, ._refs_offset_ = out_cursor_proc_C_refs_offset };
-- + _result_->_has_row_ = C_._has_row_;
-- + _result_->id = C_.id;
-- + cql_set_string_ref(&_result_->name, C_.name);
-- + _result_->rate = C_.rate;
-- + _result_->type = C_.type;
-- + _result_->size = C_.size;
-- + cql_set_string_ref(&_result_->extra1, C_.extra1);
-- + cql_set_string_ref(&_result_->extra2, C_.extra2);
create proc out_cursor_proc()
begin
  declare C cursor for select bar.*, 'xyzzy' extra1, 'plugh' extra2 from bar;
  fetch C;
  out C;
end;

-- TEST: fetch from an output struct proc
-- + #define read_cursor_proc_C_refs_offset cql_offsetof(read_cursor_proc_C_row, name) // count = 3
-- + read_cursor_proc_C_row C_ = { ._refs_count_ = 3, ._refs_offset_ = read_cursor_proc_C_refs_offset };
-- +2 cql_teardown_row(C_);
-- +1 _rc_ = out_cursor_proc(_db_, (out_cursor_proc_row *)&C_);
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
create proc read_cursor_proc()
begin
  declare C cursor fetch from call out_cursor_proc();
end;

-- TEST: declare a cursor and do a fetch as separate actions
-- +1  declare_cursor_then_fetch_from_proc_C_row C_ = { ._refs_count_ = 3, ._refs_offset_ = declare_cursor_then_fetch_from_proc_C_refs_offset };
-- +2  cql_teardown_row(C_);
-- +1  _rc_ = out_cursor_proc(_db_, (out_cursor_proc_row *)&C_);
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
-- this forces the msys_schema_utils to run over an atypical table_factor
-- + _rc_ = cql_prepare(_db_, _result_,
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
-- + C_._has_row_ = 1;
-- + C_.id = _seed_;
-- + char *_printf_result = sqlite3_mprintf("name_%d", _seed_);
-- + cql_string_release(_tmp_text_0);
-- + _tmp_text_0 = cql_string_ref_new(_printf_result);
-- + sqlite3_free(_printf_result);
-- + cql_set_string_ref(&C_.name, _tmp_text_0);
-- + cql_set_notnull(C_.rate, _seed_);
-- + cql_set_notnull(C_.type, _seed_);
-- + cql_set_notnull(C_.size, _seed_);
-- This should not be a dml proc, it doesn't actually use the db
-- + fetch_values_dummy(void)
-- - _rc_
-- - cql_cleanup
create proc fetch_values_dummy()
begin
  declare C cursor like select * from bar;
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
-- + out_no_db_C_row C_ = { 0 };
-- + C_._has_row_ = 1;
-- + C_.A = 3;
-- + C_.B = 12;
-- + _result_->_has_row_ = C_._has_row_;
-- + _result_->A = C_.A;
-- + _result_->B = C_.B;
create proc out_no_db()
begin
  declare C cursor like select 1 A, 2.5 B;
  fetch C(A,B) from values(3,12);
  out C;
end;

-- TEST: declare cursor like cursor
-- + declare_cursor_like_cursor_C0_row C0_ = { 0 };
-- + declare_cursor_like_cursor_C1_row C1_ = { 0 };
-- + memset(_result_, 0, sizeof(*_result_));
-- + _C1_._has_row_ = 1;
-- + C1_.A = 3;
-- + C1_.B = 12;
-- + _result_->_has_row_ = C1_._has_row_;
-- + _result_->A = C1_.A;
-- + _result_->B = C1_.B;
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
-- + _result_->_has_row_ = C_._has_row_;
-- + _result_->_refs_count_ = 1;
-- + _result_->_refs_offset_ = declare_cursor_like_proc_refs_offset;
-- + _result_->a = C_.a;
-- + cql_set_string_ref(&_result_->b, C_.b);
-- + cql_teardown_row(C_);
-- - Error
create proc declare_cursor_like_proc()
begin
  declare C cursor like fetcher_proc;
  out C;
end;

-- TEST: declare a cursor like a table
-- + void declare_cursor_like_table(declare_cursor_like_table_row *_Nonnull _result_) {
-- + declare_cursor_like_table_C_row C_ = { ._refs_count_ = 1, ._refs_offset_ = declare_cursor_like_table_C_refs_offset };
-- + memset(_result_, 0, sizeof(*_result_));
-- + _result_->_has_row_ = C_._has_row_;
-- + _result_->_refs_offset_ = declare_cursor_like_table_refs_offset;
-- + _result_->id = C_.id;
-- + cql_set_string_ref(&_result_->name, C_.name);
-- + _result_->rate = C_.rate;
-- + _result_->type = C_.type;
-- + _result_->size = C_.size;
-- + cql_teardown_row(C_);
-- - Error
create proc declare_cursor_like_table()
begin
  declare C cursor like bar;
  out C;
end;

-- TEST: declare a cursor like a view
-- + void declare_cursor_like_view_fetch_results( declare_cursor_like_view_result_set_ref _Nullable *_Nonnull result_set) {
-- + declare_cursor_like_view_C_row C_ = { 0 };
-- + _result_->_has_row_ = C_._has_row_;
-- + _result_->f1 = C_.f1;
-- + _result_->f2 = C_.f2;
-- + _result_->f3 = C_.f3;
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
-- + C1_._has_row_ = C0_._has_row_;
-- + C1_.A = C0_.A;
-- + cql_set_string_ref(&C1_.B, C0_.B);
create proc fetch_to_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, "foo" B;
  declare C1 cursor like C0;
  fetch C0 from values (2, "bar");
  fetch C1 from C0;
  out C1;
end;

-- TEST loop statement cursor with autofetch
-- + sqlite3_stmt *C = NULL;
-- + loop_statement_cursor_C_row C_ = { 0 };
-- + "SELECT 1"
-- + C_._has_row_ = _rc_ == SQLITE_ROW;
-- + if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- + if (!C_._has_row_) break
create proc loop_statement_cursor()
begin
  declare C cursor for select 1 A;
  loop fetch C
  begin
    call printf("%d\n", C.A);
  end;
end;

-- TEST loop statement cursor with autofetch
-- + sqlite3_stmt *C = NULL;
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

-- TEST: ensure cursors work outside of a proc
--  _rc_ = cql_prepare(_db_, &global_cursor,
--    "SELECT 1 AS a, 2 AS b"
declare global_cursor cursor for select 1 a, 2 b;

-- TEST: fetch from global cursor
-- + _rc_ = sqlite3_step(global_cursor);
-- + global_cursor_._has_row_ = _rc_ == SQLITE_ROW;
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

  declare i integer;
  declare j integer;
  set i := 1;
  set j := 2;

  set b := i is j;
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

  declare i integer;
  declare j integer;
  set i := 1;
  set j := 2;

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
-- + static cql_uint16 simple_identity_identity_columns[] = { 1,
@attribute(cql:identity=(id))
create proc simple_identity()
begin
  select 1 as id, 2 as data;
end;

-- TEST: create proc with a multi-column identity attribute
-- + static cql_uint16 complex_identity_identity_columns[] = { 2,
@attribute(cql:identity=(col1, col2))
create proc complex_identity()
begin
  select 1 as col1, 2 as col2, 3 as data;
end;

-- TEST: create proc with a out cursor and identity column
-- + static cql_uint16 out_cursor_identity_identity_columns[] = { 1,
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
-- + CREATE PROC radioactive_proc ()
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
-- - base_fragment
@attribute(cql:base_fragment=core)
create proc base_fragment(id_ integer not null)
begin
with
  core(x,y,z) as (select id,name,rate from bar where id = id_)
select * from core;
end;

-- TEST: make sure that the name of the cursor is canonicalized
-- There should be no references to the version with the wrong case
-- + simple_cursor_proc_A_CURSOR_row A_CURSOR_ = { 0 };
-- + A_CURSOR_._has_row_ = 1;
-- + A_CURSOR_.id = 1;
-- + _result_->_has_row_ = A_CURSOR_._has_row_;
-- + _result_->id = A_CURSOR_.id;
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
-- + cql_multibind(&_rc_, _db_, &C, 1,
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
-- + "foo (id) AS (SELECT 1 AS id) "
-- + "INSERT INTO bar(id) VALUES(ifnull(( SELECT id "
-- + "FROM foo ), 0))");
-- + if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
with foo(id) as (select 1 id)
insert into bar(id)
values (ifnull((select id from foo), 0))
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
-- + use_many_out_cursors_c1_row c1_ = { ._refs_count_ = 1, ._refs_offset_ = use_many_out_cursors_c1_refs_offset };
-- + use_many_out_cursors_c2_row c2_ = { ._refs_count_ = 1, ._refs_offset_ = use_many_out_cursors_c2_refs_offset };
-- +1 p1((p1_row *)&c1_);
-- +1 _rc_ = p2(_db_, (p2_row *)&c2_);
-- +2 cql_teardown_row(c1_);
-- +2 cql_teardown_row(c2_);
create procedure use_many_out_cursors()
begin
  declare c1 cursor fetch from call p1();
  declare c2 cursor fetch from call p2();
end;

-- TEST: each fetch forces the declaration of the cursor storage if it has
-- not already been declared.  In this case the third branch of the if
-- must find that p1 and p2 row data are already declare and not duplicate
-- the declarations.
-- +1 fetch_many_times_C_row C_ = { ._refs_count_ = 1, ._refs_offset_ = fetch_many_times_C_refs_offset };
-- +2 p1((p1_row *)&C_);
-- +2 _rc_ = p2(_db_, (p2_row *)&C_);
-- +5 cql_teardown_row(C_);
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
-- + void out_union_two_fetch_results(out_union_two_result_set_ref _Nullable *_Nonnull _result_set_) {
-- + cql_profile_start(CRC_out_union_two, &out_union_two_perf_index);
-- + cql_bytebuf _rows_;
-- + cql_bytebuf_open(&_rows_);
-- +2 cql_retain_row(C_);
-- +2 if (C_._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C_, sizeof(C_));
-- + cql_results_from_data(SQLITE_OK, &_rows_, &out_union_two_info, (cql_result_set_ref *)_result_set_);
-- + cql_teardown_row(C_);
create proc out_union_two()
begin
 declare C cursor like select 1 x, '2' y;
 fetch C from values(1, "y");
 out union C;
 out union C;
end;

-- TEST: read back the two rows from the above
-- + CQL_WARN_UNUSED cql_code out_union_reader(sqlite3 *_Nonnull _db_) {
-- + out_union_two_result_set_ref c_result_set_ = NULL;
-- + cql_int32 c_row_num_ = 0;
-- + cql_int32 c_row_count_ = 0;
-- + out_union_reader_c_row c_ = { ._refs_count_ = 1, ._refs_offset_ = out_union_reader_c_refs_offset };
-- + out_union_two_fetch_results(&c_result_set_);
-- + c_row_num_ = c_row_count_ = -1;
-- + c_row_count_ = cql_result_set_get_count((cql_result_set_ref)c_result_set_);
-- + for (;;) {
-- +   C_row_num_++;
-- +   C_._has_row_ = C_row_num_ < C_row_count_;
-- +   cql_copyoutrow((cql_result_set_ref)C_result_set_, C_row_num_, 2,
-- +                  CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C_.x,
-- +                  CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &C_.y);
-- NOT PRESENT !!
-- -   if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
-- +   if (!C_._has_row_) break;
-- + }
-- + cql_object_release(c_result_set_);
-- + cql_teardown_row(c_);
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
-- + cql_bytebuf _rows_;
-- + cql_bytebuf_open(&_rows_);
-- +2 cql_retain_row(C_);
-- +2 if (C_._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C_, sizeof(C_));
-- + cql_results_from_data(_rc_, &_rows_, &out_union_from_select_info, (cql_result_set_ref *)_result_set_);
-- + cql_teardown_row(C_);
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
-- + C_._has_row_ = C_row_num_ < C_row_count_;
-- + cql_copyoutrow((cql_result_set_ref)C_result_set_, C_row_num_, 2,
-- +   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C_.x,
-- +   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C_.y);
create proc read_out_union_values(a integer not null, b integer not null)
begin
  declare C cursor for call out_union_values(a,b);
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
-- + if (C_._has_row_) {
-- +   C_.x = 2;
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
-- + cql_cleanup:
-- +   ; // label requires some statement
-- + }
create proc empty_proc()
begin
end;

-- TEST: empty body parts, all statment list types
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

create table ext_test_table (
  f1 integer not null,
  f2 integer not null,
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

-- TEST: extension creates getters for the base columns and the new columns it added
-- + cql_int32 ext_get_id(frag_test_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   return frag_test_get_id(result_set, row);
-- + cql_bool ext_get_f2_is_null(frag_test_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   return __PRIVATE__frag_test_get_f2_is_null(result_set, row);
-- + cql_int32 ext_get_f2_value(frag_test_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   return __PRIVATE__frag_test_get_f2_value(result_set, row);
-- + cql_int32 ext_result_count(frag_test_result_set_ref _Nonnull result_set) {
-- +   return cql_result_set_get_count((cql_result_set_ref)result_set);
@attribute(cql:extension_fragment=frag_test)
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
-- + sqlite3_stmt *C = NULL;
-- + cql_object_ref C_object_ = NULL;
-- 1 for the boxing and 1 for the cleanup
-- +2 cql_object_release(C_object_);
-- + C_object_ = cql_box_stmt(C);
-- - cql_finalize_stmt(&C);
create proc try_boxing(out result object<bar cursor>)
begin
  declare C cursor for select * from bar;
  set result from cursor C;
end;

-- TEST: simple unbox
-- + C = cql_unbox_stmt(boxed_cursor);
-- + cql_set_object_ref(&C_object_, boxed_cursor);
-- + _rc_ = sqlite3_step(C);
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
  set r := 3.2;
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
  set r := 3.2;
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
  set r := 3;
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
  set r := 3.2;
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
  set b := 1;
  declare x real;
  set x := cast(b as real);
end;

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
  set b := 1;
  declare x bool;
  set x := cast(b as bool);
end;

-- TEST: test cql_get_blob_size codegen
-- + cql_set_nullable(l0_nullable, !_tmp_n_blob_1, cql_get_blob_size(_tmp_n_blob_1));
set l0_nullable := cql_get_blob_size((select blob_var));

-- TEST: test cql_get_blob_size codegen with not null blob
-- + l2 = cql_get_blob_size(blob_var2);
set l2 := cql_get_blob_size(blob_var2);

-- TEST: test basic proc savepoint structure
-- + "SAVEPOINT base_proc_savepoint");
-- + // try
-- + "RELEASE SAVEPOINT base_proc_savepoint");
-- + catch_start% {
-- + "ROLLBACK TRANSACTION TO SAVEPOINT base_proc_savepoint");
-- + _rc_ = cql_best_error(_rc_thrown_);
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
-- +3 "RELEASE SAVEPOINT base_proc_savepoint_commit_return"
-- +1 "ROLLBACK TRANSACTION TO SAVEPOINT base_proc_savepoint_commit_return"
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
-- +2 "ROLLBACK TRANSACTION TO SAVEPOINT base_proc_savepoint_rollback_return"
-- +3 "RELEASE SAVEPOINT base_proc_savepoint_rollback_return"
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

-- + x = 1 * (2 / 3);
SET x := 1 * (2 / 3);

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

-- + x = 1 / (2 / 3);
SET x := 1 / (2 / 3);

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
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
SalesMovingAverage FROM SalesInfo;

-- TEST: simple OVER and ORDER BY
-- + SUM(amount) OVER (ORDER BY month) AS RunningTotal
SELECT month, amount, SUM(amount) OVER
  (ORDER BY month) RunningTotal
FROM SalesInfo;

-- TEST: ROWS expr preceeding and expr following, exclude no others
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
SalesMovingAverage FROM SalesInfo;

-- TEST: ROWS expr preceeding and expr following, exclude no others with FILTER
-- + AVG(amount) FILTER (WHERE month = 1) OVER (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS) AS SalesMovingAverage
SELECT month, amount, AVG(amount) FILTER(WHERE month = 1) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
SalesMovingAverage FROM SalesInfo;

-- TEST: ROWS expr preceeding and expr following, exclude current row
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 3 PRECEDING AND 4 FOLLOWING EXCLUDE CURRENT ROW) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 3 PRECEDING AND 4 FOLLOWING EXCLUDE CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

-- TEST: ROWS expr preceeding and expr following, exclude group
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 4 PRECEDING AND 5 FOLLOWING EXCLUDE GROUP) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 4 PRECEDING AND 5 FOLLOWING EXCLUDE GROUP)
SalesMovingAverage FROM SalesInfo;

-- TEST: ROWS expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month ROWS BETWEEN 6 PRECEDING AND 7 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 6 PRECEDING AND 7 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: RANGE expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month RANGE BETWEEN 8 PRECEDING AND 9 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month RANGE BETWEEN 8 PRECEDING AND 9 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS expr preceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN 10 PRECEDING AND 11 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN 10 PRECEDING AND 11 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS unbounded proceeding and expr following, exclude ties
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND 12 FOLLOWING EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND 12 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS expr following and expr preceeding 
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN 13 FOLLOWING AND 14 PRECEDING) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN 13 FOLLOWING AND 14 PRECEDING)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS between current row and unbounded following
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS between unbounded preceding and current row with no exclude
-- + AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

-- TEST: GROUPS between unbounded preceding and current row with exclude ties
-- +  AVG(amount) OVER (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: correct parse and re-emit of CURRENT_ROW
-- + AVG(amount) OVER (PARTITION BY month ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (PARTITION BY month ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

-- TEST: correct parse and re-emit of CURRENT_ROW
-- + AVG(amount) OVER (GROUPS CURRENT ROW) AS SalesMovingAverage
SELECT month, amount, AVG(amount) OVER
  (GROUPS CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

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

--------------------------------------------------------------------
-------------------- add new tests before this point ---------------
--------------------------------------------------------------------
create proc end_proc() begin declare x integer; end;
-- TEST: end marker -- this is the last test
-- + cql_nullable_int32 end_marker;
-- + cql_code cql_startup(sqlite3 *_Nonnull _db_)
declare end_marker integer;
--------------------------------------------------------------------

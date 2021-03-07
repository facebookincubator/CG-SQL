/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqltest.h"

BEGIN_SUITE()

declare function blob_from_string(str text @sensitive) create blob not null;
declare function string_from_blob(b blob @sensitive) create text not null;
declare procedure cql_init_extensions() using transaction;

declare enum floats real (
   one = 1.0,
   two = 2.0
);

declare enum longs long_int (
   one = 1,
   big = 0x100000000,
   neg = -1
);

call cql_init_extensions();

BEGIN_TEST(arithmetic)
  EXPECT((1 + 2) * 3 == 9);
  EXPECT(1 + 2 * 3 == 7);
  EXPECT(6 / 3 == 2);
  EXPECT(7 - 5 == 2);
  EXPECT(6 % 5 == 1);
  EXPECT(5 / 2.5 == 2);
  EXPECT(-(1+3) == -4);
  EXPECT(-1+3 == 2);
  EXPECT(1+-3 == -2);
  EXPECT(longs.neg == -1);
  EXPECT(-longs.neg == 1);
  EXPECT(- -longs.neg == -1);
END_TEST(arithmetic)

declare side_effect_0_count integer not null;
declare side_effect_1_count integer not null;
declare side_effect_null_count integer not null;

create proc side_effect_0(out result integer)
begin
   set result := 0;
   set side_effect_0_count := side_effect_0_count + 1;
end;

create proc side_effect_1(out result integer)
begin
   set result := 1;
   set side_effect_1_count := side_effect_1_count + 1;
end;

create proc side_effect_null(out result integer)
begin
   set result := null;
   set side_effect_null_count := side_effect_null_count + 1;
end;

create proc reset_counts()
begin
   set side_effect_0_count := 0;
   set side_effect_1_count := 0;
   set side_effect_null_count := 0;
end;

BEGIN_TEST(logical_operations)
  EXPECT((null and 0) == 0);
  EXPECT((null and 0) = 0);
  EXPECT((0 and null) == 0);
  EXPECT((1 and null) is null);
  EXPECT((null and 1) is null);
  EXPECT((null or 1) == 1);
  EXPECT((1 or null) == 1);
  EXPECT((0 or null) is null);
  EXPECT((null or 0) is null);
  EXPECT((0 or 1) and (1 or 0));
  EXPECT(NOT (1+2) == 0);
  EXPECT((NOT 1)+2 == 2);

  EXPECT((side_effect_0() and side_effect_0()) == 0);
  EXPECT(side_effect_0_count == 1);
  call reset_counts();

  EXPECT((side_effect_0() and side_effect_1()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((side_effect_0() and side_effect_null()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_null_count == 0);
  call reset_counts();

  EXPECT((side_effect_1() and side_effect_0()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_1() and side_effect_1()) == 1);
  EXPECT(side_effect_1_count == 2);
  call reset_counts();

  EXPECT((side_effect_1() and side_effect_null()) is null);
  EXPECT(side_effect_1_count == 1);
  EXPECT(side_effect_null_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() and side_effect_0()) == 0);
  EXPECT(side_effect_null_count == 1);
  EXPECT(side_effect_0_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() and side_effect_1()) is null);
  EXPECT(side_effect_null_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() and side_effect_null()) is null);
  EXPECT(side_effect_null_count == 2);
  call reset_counts();

  EXPECT((side_effect_0() or side_effect_0()) == 0);
  EXPECT(side_effect_0_count == 2);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((side_effect_0() or side_effect_1()) == 1);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_0() or side_effect_null()) is null);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_null_count == 1);
  call reset_counts();

  EXPECT((side_effect_1() or side_effect_0()) == 1);
  EXPECT(side_effect_0_count == 0);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_1() or side_effect_1()) == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_1() or side_effect_null()) == 1);
  EXPECT(side_effect_null_count == 0);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() or side_effect_0()) is null);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_null_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() or side_effect_1()) == 1);
  EXPECT(side_effect_null_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((side_effect_null() or side_effect_null()) is null);
  EXPECT(side_effect_null_count == 2);
  call reset_counts();

  -- even though this looks like all non nulls we do not eval side_effect_1
  -- we can't use the simple && form because there is statement output
  -- requred to evaluate the coalesce.

  EXPECT((0 and coalesce(side_effect_1(), 1)) == 0);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((1 and coalesce(side_effect_1(), 1)) == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((1 or coalesce(side_effect_1(), 1)) == 1);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((0 or coalesce(side_effect_1(), 1)) == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  -- this is the same as NOT (0 < 0) rather than (NOT 0) < 0
  -- do not move NOT around or you will break stuff
  -- I have broken this many times now do not change this expectation
  -- it will save your life.
  EXPECT(NOT 0 < 0);
END_TEST(logical_operations)

declare zero integer not null;
set zero := 0;

declare one integer not null;
set one := 1;

 -- logical and short-circuit verify 1/0 not evaluated
BEGIN_TEST(local_operations_early_out)
  EXPECT(not (0 and 1/zero));
  EXPECT(1 or 1/zero);
END_TEST(local_operations_early_out)

-- assorted between combinations
BEGIN_TEST(between_operations)
  EXPECT(1 between 0 and 2);
  EXPECT(not 3 between 0 and 2);
  EXPECT(not (3 between 0 and 2));
  EXPECT((null between 0 and 2) is null);
  EXPECT((1 between null and 2) is null);
  EXPECT((1 between 0 and null) is null);

  EXPECT((-1 between side_effect_0() and side_effect_1()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((0 between side_effect_0() and side_effect_1()) == 1);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((2 between side_effect_0() and side_effect_1()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((-1 not between side_effect_0() and side_effect_1()) == 1);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 0);
  call reset_counts();

  EXPECT((0 not between side_effect_0() and side_effect_1()) == 0);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();

  EXPECT((2 not between side_effect_0() and side_effect_1()) == 1);
  EXPECT(side_effect_0_count == 1);
  EXPECT(side_effect_1_count == 1);
  call reset_counts();
END_TEST(between_operations)

-- assorted not between combinations
BEGIN_TEST(not_between_operations)
  EXPECT(3 not between 0 and 2);
  EXPECT((not 1 not between 0 and 2) == 0);
  EXPECT(not (1 not between 0 and 2));
  EXPECT(1 not between 2 and 0);
  EXPECT(not 3 not between 2 and 0);
  EXPECT((null not between 0 and 2) is null);
  EXPECT((1 not between null and 2) is null);
  EXPECT((1 not between 0 and null) is null);
END_TEST(not_between_operations)

-- assorted comparisons
BEGIN_TEST(numeric_comparisons)
  EXPECT(0 == zero);
  EXPECT(not (one == zero));
  EXPECT(one <> zero);
  EXPECT(not (one <> 1));
  EXPECT(one > zero);
  EXPECT(zero < one);
  EXPECT(one >= zero);
  EXPECT(zero <= one);
  EXPECT(one >= 1);
  EXPECT(one <= 1);
END_TEST(numeric_comparisons)

-- verify that out parameter is set in proc call
create procedure echo ( in arg1 integer not null, out arg2 integer not null)
begin
  set arg2 := arg1;
end;

BEGIN_TEST(out_arguments)
  declare scratch integer not null;
  call echo(12, scratch);
  EXPECT(scratch == 12);
END_TEST(out_arguments)

-- test simple recursive function
create procedure fib (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    declare t integer not null;
    call fib(arg - 1,  result);
    call fib(arg - 2,  t);
    set result := t + result;
  end if;
end;

BEGIN_TEST(simple_recursion)
  declare scratch integer not null;
  call fib(1, scratch);
  EXPECT(scratch == 1);
  call fib(2, scratch);
  EXPECT(scratch == 1);
  call fib(3, scratch);
  EXPECT(scratch == 2);
  call fib(4, scratch);
  EXPECT(scratch == 3);
  call fib(5, scratch);
  EXPECT(scratch == 5);
  call fib(6, scratch);
  EXPECT(scratch == 8);
END_TEST(simple_recursion)

-- test elementary cursor on select with no tables, still round trips through sqlite
BEGIN_TEST(cursor_basics)
  declare col1 integer;
  declare col2 real not null;
  declare basic_cursor cursor for select 1, 2.5;
  open basic_cursor;
  fetch basic_cursor into col1, col2;
  EXPECT(basic_cursor);
  EXPECT(col1 == 1);
  EXPECT(col2 == 2.5);
  fetch basic_cursor into col1, col2;
  EXPECT(not basic_cursor);
  close basic_cursor;
END_TEST(cursor_basics)

-- the most expensive way to swap two variables ever :)
BEGIN_TEST(exchange_with_cursor)
  declare arg1 integer not null;
  declare arg2 integer not null;
  set arg1 := 7;
  set arg2 := 11;
  declare exchange_cursor cursor for select arg2, arg1;
  open exchange_cursor;
  fetch exchange_cursor into arg1, arg2;
  EXPECT(exchange_cursor);
  EXPECT(arg1 == 11);
  EXPECT(arg2 == 7);
  close exchange_cursor;
END_TEST(exchange_with_cursor)

create procedure make_mixed()
begin
  create table mixed(
    id integer not null,
    name text,
    code long int,   -- these are nullable to make the cg harder
    flag bool,
    rate real,
    bl blob
  );
end;

create procedure drop_mixed()
begin
  drop table if exists mixed;
end;

call make_mixed();

create procedure load_mixed()
begin
  delete from mixed;
  insert into mixed values (1, "a name", 12, 1, 5.0, cast("blob1" as blob));
  insert into mixed values (2, "another name", 14, 3, 7.0, cast("blob2" as blob));
end;

create procedure load_mixed_dupes()
begin
  delete from mixed;
  insert into mixed values (1, "a name", 12, 1, 5.0, NULL);
  insert into mixed values (1, "a name", 12, 1, 5.0, NULL);
  insert into mixed values (2, "another name", 14, 3, 7.0, cast("blob2" as blob));
  insert into mixed values (2, "another name", 14, 3, 7.0, cast("blob2" as blob));
  insert into mixed values (1, "a name", 12, 1, 5.0, cast("blob1" as blob));
  insert into mixed values (1, NULL, 12, 1, 5.0, NULL);
end;

create procedure load_mixed_dupe_identities()
begin
  delete from mixed;
  insert into mixed values (1, "a name", 12, 1, 5.0, NULL);
  insert into mixed values (1, "another name", 12, 1, 5.0, NULL);
  insert into mixed values (1, "another name", 12, 0, 5.0, NULL);
  insert into mixed values (1, "another name", 12, 0, 5.0, cast("blob1" as blob));
  insert into mixed values (1, "another name", 14, 0, 7.0, cast("blob1" as blob));
end;

create procedure load_mixed_with_nulls()
begin
  call load_mixed();
  insert into mixed values (3, NULL, NULL, NULL, NULL, NULL);
  insert into mixed values (4, "last name", 16, 0, 9.0, cast("blob3" as blob));
end;

create procedure update_mixed(id_ integer not null, name_ text, code_ long int, bl_ blob)
begin
  update mixed set code = code_, bl = bl_ where id = id_;
end;

-- test readback of two rows
BEGIN_TEST(read_mixed)
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_ bool;
  declare rate_ real;
  declare bl_ blob;

  call load_mixed();

  declare read_cursor cursor for select * from mixed;
  open read_cursor;

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  EXPECT(read_cursor);
  EXPECT(id_ == 1);
  EXPECT(name_ == "a name");
  EXPECT(code_ == 12);
  EXPECT(flag_ == 1);
  EXPECT(rate_ == 5);
  EXPECT(string_from_blob(bl_) == "blob1");

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  EXPECT(read_cursor);
  EXPECT(id_ == 2);
  EXPECT(name_ == "another name");
  EXPECT(code_ == 14);
  EXPECT(flag_ == 1);
  EXPECT(rate_ == 7);
  EXPECT(string_from_blob(bl_) == "blob2");

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  EXPECT(not read_cursor);
  close read_cursor;
END_TEST(read_mixed)

-- now attempt a mutation
BEGIN_TEST(mutate_mixed)
  declare new_code long integer;
  declare code_ long integer;
  set new_code := 88;
  declare id_ integer;
  set id_ := 2;  -- either works

  call load_mixed();

  update mixed set code = new_code where id = id_;
  declare updated_cursor cursor for select code from mixed where id = id_;
  open updated_cursor;
  fetch updated_cursor into code_;
  close updated_cursor;
  EXPECT(code_ == new_code);
END_TEST(mutate_mixed)

BEGIN_TEST(nested_select_expressions)
  -- use nested expression select
  declare temp_1 integer not null;
  set temp_1 := (select zero*5 + one*11);
  EXPECT(temp_1 == 11);

  call load_mixed();

  set temp_1 := (select id from mixed where id > 1 order by id limit 1);
  EXPECT(temp_1 == 2);

  set temp_1 := (select count(*) from mixed);
  EXPECT(temp_1 == 2);

  declare temp_2 real;
  set temp_2 := (select avg(id) from mixed);
  EXPECT(temp_2 == 1.5);

  EXPECT((select longs.neg) == -1);
  EXPECT((select -longs.neg) == 1);
  EXPECT((select - -longs.neg) == -1);
END_TEST(nested_select_expressions)

-- complex delete pattern

create proc delete_one_from_mixed(out _id integer not null)
begin
  set _id := (select id from mixed order by id limit 1);
  delete from mixed where id = _id;
end;

BEGIN_TEST(delete_several)
  call load_mixed();
  EXPECT(2 == (select count(*) from mixed));

  declare id_ integer not null;
  call delete_one_from_mixed(id_);
  EXPECT(1 == id_);
  EXPECT(0 == (select count(*) from mixed where id = id_));
  EXPECT(1 == (select count(*) from mixed where id != id_));

  call delete_one_from_mixed(id_);
  EXPECT(2 == id_);
  EXPECT(0 == (select count(*) from mixed));
END_TEST(delete_several)

-- some basic string stuff using sqlite for string helpers
create proc string_copy(in input text not null, out output text not null)
begin
  -- extra shuffling for refcount testing
  declare t text not null;
  set t := input;
  set output := t;
end;

-- some basic string stuff using sqlite for string helpers
create proc string_equal(in t1 text not null, in t2 text not null, out result bool not null)
begin
  set result := (select t1 == t2);
end;

-- try out some string lifetime functions
BEGIN_TEST(string_ref_test)
  declare a_string text not null;
  call string_copy("Hello", a_string);
  declare result bool not null;
  call string_equal(a_string, "Hello", result);
  EXPECT(result);
END_TEST(string_ref_test)

-- try out some string comparisons
BEGIN_TEST(string_comparisons)
  declare t1 text;
  declare t2 text;
  declare t3 text;

  set t1 := "a";
  set t2 := "b";
  set t3 := "a";

  EXPECT("a" == "a");
  EXPECT("a" IS "a");
  EXPECT("a" != "b");
  EXPECT("a" IS NOT "b");
  EXPECT(t1 < t2);
  EXPECT(t2 > t1);
  EXPECT(t1 <= t2);
  EXPECT(t2 >= t1);
  EXPECT(t1 <= t3);
  EXPECT(t3 >= t1);
  EXPECT(t1 == t3);
  EXPECT(t1 != t2);
END_TEST(string_comparisons)

-- string comnparison nullability checks
BEGIN_TEST(string_comparisons_nullability)
  declare null_ text;
  declare x text not null;
  set x := "x";
  EXPECT((x < x) is not null);
  EXPECT((x > "x") is not null);
  EXPECT((null_ > x) is null);
  EXPECT((x > null_) is null);
  EXPECT((null_ > null_) is null);
  EXPECT((NULL == null_) is null);
  EXPECT((null_ == NULL) is null);
END_TEST(string_comparisons_nullability)

-- string is null and is not null tests
BEGIN_TEST(string_is_null_or_not)
  declare null_ text;
  declare x text not null;
  set x := "x";
  declare y text;
  set y := "y";

  EXPECT(null_ is null);
  EXPECT(x is not null);
  EXPECT(y is not null);
  EXPECT(not (null_ is not null));
  EXPECT(not (x is null));
  EXPECT(not (y is null));

END_TEST(string_is_null_or_not)

-- binding tests for not null types
BEGIN_TEST(bind_not_nullables)
  declare b bool not null;
  declare i integer not null;
  declare l long integer not null;
  declare r real not null;
  declare t text not null;

  set b := 1;
  set i := 2;
  set l := 3;
  set r := 4.5;
  set t := "foo";

  EXPECT(b == (select b)); -- binding not null bool
  EXPECT(i == (select i)); -- binding not null int
  EXPECT(l == (select l)); -- binding not null long
  EXPECT(r == (select r)); -- binding not null real
  EXPECT(t == (select t)); -- binding not null text

  EXPECT(b != (select not b)); -- binding not null bool
  EXPECT(i != (select 1 + i)); -- binding not null int
  EXPECT(l != (select 1 + l)); -- binding not null long
  EXPECT(r != (select 1 + r)); -- binding not null real
END_TEST(bind_not_nullables)

-- binding tests for nullable types
BEGIN_TEST(bind_nullables_not_null)
  declare b bool;
  declare i integer;
  declare l long integer;
  declare r real;
  declare t text;

  set b := 1;
  set i := 2;
  set l := 3;
  set r := 4.5;
  set t := "foo";

  EXPECT(b == (select b)); -- binding nullable not null bool
  EXPECT(i == (select i)); -- binding nullable not null int
  EXPECT(l == (select l)); -- binding nullable not null long
  EXPECT(r == (select r)); -- binding nullable not null real
  EXPECT(t == (select t)); -- binding nullable not null text

  EXPECT(b != (select not b)); -- binding nullable not null bool
  EXPECT(i != (select 1 + i)); -- binding nullable not null int
  EXPECT(l != (select 1 + l)); -- binding nullable not null long
  EXPECT(r != (select 1 + r)); -- binding nullable not null real
END_TEST(bind_nullables_not_null)

-- binding tests for nullable types values null
BEGIN_TEST(bind_nullables_null)
  declare b bool;
  declare i integer;
  declare l long integer;
  declare r real;
  declare t text;

  set b := null;
  set i := null;
  set l := null;
  set r := null;
  set t := null;

  EXPECT((select b) is null); -- binding null bool
  EXPECT((select i) is null); -- binding null int
  EXPECT((select l) is null); -- binding null long
  EXPECT((select r) is null); -- binding null real
  EXPECT((select t) is null); -- binding null text

END_TEST(bind_nullables_null)

BEGIN_TEST(loop_fetch)
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_  bool;
  declare rate_ real;
  declare bl_ blob;
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for select * from mixed;
  open read_cursor;

  set count := 0;
  set sum := 0;
  loop fetch read_cursor into id_, name_, code_, flag_, rate_, bl_
  begin
    set count := count + 1;
    set sum := sum + id_;
  end;

  EXPECT(count == 2);  -- there should be two rows
  EXPECT(sum  == 3);   -- some math along the way
END_TEST(loop_fetch)

create procedure load_more_mixed()
begin
  delete from mixed;
  insert into mixed values (1, "a name", 12, 1, 5.0, NULL);
  insert into mixed values (2, "some name", 14, 3, 7.0, NULL);
  insert into mixed values (3, "yet another name", 15, 3, 17.4, NULL);
  insert into mixed values (4, "some name", 19, 4, 9.1, NULL);
  insert into mixed values (5, "what name", 21, 8, 12.3, NULL);
end;

BEGIN_TEST(loop_control_flow)
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_  bool;
  declare rate_ real;
  declare bl_ blob;
  declare count integer not null;

  call load_more_mixed();

  declare read_cursor cursor for select * from mixed;
  open read_cursor;

  set count := 0;
  loop fetch read_cursor into id_, name_, code_, flag_, rate_, bl_
  begin
    -- skip number two
    if id_ == 2 then
      continue;
    end if;
    set count := count + 1;
    -- should break on number 4
    if name_ == "some name" then
      leave;
    end if;
  end;

  EXPECT(count == 3); -- there should be three rows tested
  EXPECT(id_  == 4);  -- the match goes with id #4
END_TEST(loop_control_flow)

-- basic test of while loop plus leave and continue
BEGIN_TEST(while_control_flow)
  declare i, sum integer not null;

  set i := 0;
  set sum := 0;
  while i < 5
  begin
    set i := i + 1;
    set sum := sum + i;
  end;

  EXPECT(i == 5);  -- loop ended on time
  EXPECT(sum == 15); -- correct sum computed: 1+2+3+4+5

  set i := 0;
  set sum := 0;
  while i < 5
  begin
    set i := i + 1;
    if i == 2 then
      continue;
    end if;

    if i == 4 then
      leave;
    end if;

    set sum := sum + i;
  end;

  EXPECT(i == 4);  -- loop ended on time
  EXPECT(sum == 4);  -- correct sum computed: 1+3
END_TEST(while_control_flow)

-- same test but the control variable is nullable making the expression nullable
BEGIN_TEST(while_control_flow_with_nullables)
  declare i, sum integer;

  set i := 0;
  set sum := 0;
  while i < 5
  begin
    set i := i + 1;
    set sum := sum + i;
  end;

  EXPECT(i == 5); -- loop ended on time
  EXPECT(sum == 15);  -- correct sum computed: 1+2+3+4+5
END_TEST(while_control_flow_with_nullables)

-- like predicate test
BEGIN_TEST(like_predicate)
  EXPECT("this is a test" like "%is a%");
  EXPECT(not ("this is a test" like "is a"));

  declare txt text;
  EXPECT(("" like txt) is null);
  EXPECT((txt like "%") is null);
  EXPECT((txt like txt) is null);
END_TEST(like_predicate)

-- error handling with try catch throw
create procedure throws(out did_throw bool not null)
begin
  declare x integer not null;
  set did_throw := 0;
  begin try
    -- this fails
    set x := (select id from mixed where id = 999);
  end try;
  begin catch
    set did_throw := 1;
    -- and rethrow!
    throw;
  end catch;
  set did_throw := 0; -- test fails if this runs, it should not
end;

BEGIN_TEST(throw_and_catch)
  declare did_throw bool not null;
  declare did_continue bool not null;
  set did_continue := 0;
  begin try
    call throws(did_throw);
    set did_throw := one / zero;  -- this does not run
  end try;
  begin catch
    set did_continue := 1;
  end catch;
  EXPECT(did_throw == 1);  -- exception was caught
  EXPECT(did_continue == 1);  -- execution continued
END_TEST(throw_and_catch)

-- the catch block should not run if no errors
BEGIN_TEST(throw_and_not_catch)
  declare did_catch integer not null;
  begin try
    set did_catch := 0;
  end try;
  begin catch
    set did_catch := 1;
  end catch;
  EXPECT(did_catch == 0); -- catch did not run
END_TEST(throw_and_not_catch)

create procedure case_tester1(value integer not null, out result integer)
begin
  set result := case value
                     when 1 then 100
                     when 2 then 200
                     when 3 then 300
                     else 400 end;
end;

create procedure case_tester2(value integer not null, out result integer)
begin
  set result := case value
                     when 1 then 100
                     when 2 then 200
                     when 3 then 300
                     end;
end;

BEGIN_TEST(simple_case_test)
  declare result integer;
  call case_tester1(1, result);
  EXPECT(result == 100);
  call case_tester1(2, result);
  EXPECT(result == 200);
  call case_tester1(3, result);
  EXPECT(result == 300);
  call case_tester1(5, result);
  EXPECT(result == 400);

  call case_tester2(1, result);
  EXPECT(result == 100);
  call case_tester2(2, result);
  EXPECT(result == 200);
  call case_tester2(3, result);
  EXPECT(result == 300);
  call case_tester2(5, result);
  EXPECT(result is null);
END_TEST(simple_case_test)

create procedure string_case_tester1(value text, out result text)
begin
  set result := case value
                     when "1" then "100"
                     when "2" then "200"
                     when "3" then "300"
                     end;
end;

BEGIN_TEST(string_case_test)
  declare result text;
  call string_case_tester1("1", result);
  EXPECT(result == "100");
  call string_case_tester1("2", result);
  EXPECT(result == "200");
  call string_case_tester1("3", result);
  EXPECT(result == "300");
  call string_case_tester1("5", result);
  EXPECT(result is null);
END_TEST(string_case_test)


create procedure in_tester1(value integer not null, out result bool not null)
begin
  set result := value in (1, 2, 3);
end;

BEGIN_TEST(in_test_not_null)
  declare result bool not null;
  call in_tester1(1, result);
  EXPECT(result);
  call in_tester1(2, result);
  EXPECT(result);
  call in_tester1(3, result);
  EXPECT(result);
  call in_tester1(4, result);
  EXPECT(not result);
END_TEST(in_test_not_null)

create procedure in_tester2(value integer, out result bool)
begin
  declare two integer;
  set two := 2;
  set result := value in (1, two, 3);
end;

BEGIN_TEST(in_test_nullables)
  declare result bool;
  call in_tester2(1, result);
  EXPECT(result);
  call in_tester2(2, result);
  EXPECT(result);
  call in_tester2(3, result);
  EXPECT(result);
  call in_tester2(4, result);
  EXPECT(not result);
  call in_tester2(null, result);
  EXPECT(result is null);
END_TEST(in_test_nullables)

create procedure nullables_case_tester(value integer, out result integer not null)
begin
  -- this is a very weird way to get a bool
  set result := case 1 when value then 1 else 0 end;
end;

BEGIN_TEST(nullable_when_test)
  declare result integer not null;
  call nullables_case_tester(1, result);
  EXPECT(result == 1);
  call nullables_case_tester(0, result);
  EXPECT(result == 0);
END_TEST(nullable_when_test)

create procedure nullables_case_tester2(value integer, out result integer not null)
begin
  -- this is a very weird way to get a bool
  set result := case when value then 1 else 0 end;
end;

BEGIN_TEST(nullable_when_pred_test)
  declare result integer not null;
  call nullables_case_tester(1, result);
  EXPECT(result == 1);
  call nullables_case_tester(0, result);
  EXPECT(result == 0);
  call nullables_case_tester(null, result);
  EXPECT(result == 0);
END_TEST(nullable_when_pred_test)

create procedure in_string_tester(value text, out result bool)
begin
  set result := value in ("this", "that");
end;

BEGIN_TEST(string_in_test)
  declare result bool;
  call in_string_tester("this", result);
  EXPECT(result);
  call in_string_tester("that", result);
  EXPECT(result);
  call in_string_tester("at", result);
  EXPECT(not result);
  call in_string_tester(null, result);
  EXPECT(result is null);
END_TEST(string_in_test)

BEGIN_TEST(string_between_test)
  declare n1, n2, n3 text;
  declare s1, s2, s3 text not null;

  set n1 := "1";
  set n2 := "2";
  set n3 := "3";
  set s1 := "1";
  set s2 := "2";
  set s3 := "3";

  EXPECT(s2 between s1 and s3);
  EXPECT(not (s2 between s3 and s1));
  EXPECT(1 + (s2 between s1 and s3) == 2);

  EXPECT(n2 between n1 and n3);
  EXPECT(not (n2 between n3 and n1));

  set n2 := null;
  EXPECT((n2 between n1 and n3) is null);
  set n2 := "2";

  set n1 := null;
  EXPECT((n2 between n1 and n3) is null);
  set n1 := "1";

  set n3 := null;
  EXPECT((n2 between n1 and n3) is null);
  set n3 := "3";
END_TEST(string_between_test)

BEGIN_TEST(string_not_between_test)
  declare n1, n2, n3 text;
  declare s1, s2, s3 text not null;

  set n1 := "1";
  set n2 := "2";
  set n3 := "3";
  set s1 := "1";
  set s2 := "2";
  set s3 := "3";

  EXPECT(not (s2 not between s1 and s3));
  EXPECT(s2 not between s3 and s1);
  EXPECT(1 + (s2 not between s1 and s3) == 1);

  EXPECT(not (n2 not between n1 and n3));
  EXPECT(n2 not between n3 and n1);

  set n2 := null;
  EXPECT((n2 not between n1 and n3) is null);
  set n2 := "2";

  set n1 := null;
  EXPECT((n2 not between n1 and n3) is null);
  set n1 := "1";

  set n3 := null;
  EXPECT((n2 not between n1 and n3) is null);
  set n3 := "3";
END_TEST(string_not_between_test)

create proc maybe_commit(do_commit bool not null)
begin
  call load_mixed();
  begin transaction;
  delete from mixed where id = 1;
  EXPECT(1 == (select count(*) from mixed)); -- delete successful
  if do_commit then
    commit transaction;
  else
    rollback transaction;
  end if;
end;

BEGIN_TEST(transaction_mechanics)
  call maybe_commit(1);
  EXPECT(1 == (select count(*) from mixed)); -- commit successful
  call maybe_commit(0);
  EXPECT(2 == (select count(*) from mixed)); -- rollback successful
END_TEST(transaction_mechanics)

@attribute(cql:identity=(id, code, bl))
create procedure get_mixed(lim integer not null)
begin
  select * from mixed limit lim;
end;

create procedure get_one_from_mixed(id_ integer not null)
begin
  declare C cursor for select * from mixed where id = id_;
  fetch C;
  out C;
end;

BEGIN_TEST(proc_loop_fetch)
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_  bool;
  declare rate_ real;
  declare bl_ blob;
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for call get_mixed(200);
  open read_cursor;

  set count := 0;
  loop fetch read_cursor into id_, name_, code_, flag_, rate_, bl_
  begin
    set count := count + 1;
  end;

  EXPECT(count == 2); -- there should be two rows
END_TEST(proc_loop_fetch)

create proc savepoint_maybe_commit(do_commit bool not null)
begin
  call load_mixed();
  savepoint foo;
  delete from mixed where id = 1;
  EXPECT(1 == (select count(*) from mixed));  -- delete successful
  if do_commit then
    release savepoint foo;
  else
    rollback transaction to savepoint foo;
  end if;
end;

BEGIN_TEST(savepoint_mechanics)
  call savepoint_maybe_commit(1);
  EXPECT(1 == (select count(*) from mixed));  -- savepoint commit successful
  call savepoint_maybe_commit(0);
  EXPECT(2 == (select count(*) from mixed));  -- savepoint rollback successful
END_TEST(savepoint_mechanics)

BEGIN_TEST(exists_test)
  call load_mixed();
  EXPECT((select EXISTS(select * from mixed)));  -- exists found rows
  delete from mixed;
  EXPECT((select NOT EXISTS(select * from mixed)));  -- not exists found no rows
END_TEST(exists_test)

create proc bulk_load_mixed(rows_ integer not null)
begin
  delete from mixed;
  declare i integer not null;
  set i := 0;
  while i < rows_
  begin
    insert into mixed values (i, "a name", 12, 1, 5.0, cast(i as blob));
    set i := i + 1;
  end;
end;

BEGIN_TEST(complex_nested_selects)
  create table data(id int, val int);
  create table codes(id int, code int);

  insert into data values(1, 100);
  insert into data values(2, 200);
  insert into data values(3, 300);

  insert into codes values(1, 1000);
  insert into codes values(1, 1001);
  insert into codes values(1, 1002);
  insert into codes values(2, 2000);
  insert into codes values(2, 2001);
  insert into codes values(3, 3000);

  declare c1 cursor for select id from data as T1 where exists (select * from codes as T2 where T1.id == T2.id and T2.code % 1000 == 1);

  declare id_ integer;
  declare count_ integer;
  loop fetch c1 into id_
  begin
    EXPECT(case id_ when 1 then 1 when 2 then 1 else 0 end);
  end;

  declare c2 cursor for
    select id, (select count(*) from codes T2 where T2.id = T1.id) as code_count
    from data T1
    where val >= 7;
  loop fetch c2 into id_, count_
  begin
    EXPECT(count_ == case id_ when 1 then 3 when 2 then 2 when 3 then 1 else 0 end);
  end;
END_TEST(complex_nested_selects)

BEGIN_TEST(proc_loop_auto_fetch)
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for call get_mixed(200);
  open read_cursor;

  set count := 0;
  set sum := 0;
  loop fetch read_cursor
  begin
    set count := count + 1;
    set sum := sum + read_cursor.id;
  end;

  EXPECT(count == 2);  -- there should be two rows
  EXPECT(sum  == 3);  -- id checksum
END_TEST(proc_loop_auto_fetch)

BEGIN_TEST(coalesce)
  declare i integer;
  set i := null;
  EXPECT(coalesce(i, i, 2) == 2); -- grab the not null last value
  EXPECT(ifnull(i, 2) == 2); -- grab the not null last value
  EXPECT((select coalesce(i, i, 2)) == 2); -- grab the not null last value

  set i:= 3;
  EXPECT(coalesce(i, i, 2) == 3); -- grab the not null first value
  EXPECT(ifnull(i, 2) == 3); -- grab the not null first value
  EXPECT((select coalesce(i, i, 2)) == 3); -- grab the not null first value
END_TEST(coalesce)

BEGIN_TEST(printf_expression)
EXPECT(printf("%d and %d", 12, 7) == "12 and 7"); -- loose printf ok
EXPECT((select printf("%d and %d", 12, 7)) == "12 and 7"); -- sql printf ok
END_TEST(printf_expression)

BEGIN_TEST(case_with_null)
  declare x integer;
  set x := null;
  set x := case x when 0 then 1 else 2 end;
  EXPECT(x == 2); --null only matches the else
END_TEST(case_with_null)

BEGIN_TEST(group_concat)
  create table conc_test(id int, name text);
  insert into conc_test values (1,"x");
  insert into conc_test values (1,"y");
  insert into conc_test values (2,"z");
  declare C cursor for select id, group_concat(name) as vals from conc_test group by id;
  fetch C;
  EXPECT(C.id = 1);
  EXPECT(C.vals = "x,y");
  fetch C;
  EXPECT(C.id = 2);
  EXPECT(C.vals = "z");
END_TEST(group_concat)

BEGIN_TEST(strftime)
EXPECT((select strftime("%s", "1970-01-01T00:00:03")) == "3"); -- sql strftime ok
EXPECT((select strftime(null, "1970-01-01T00:00:03")) is null); -- strftime null format ok
EXPECT((select strftime("%s", null)) is null); -- strftime null timestring ok
EXPECT((select strftime("%s", "1970-01-01T00:00:03", "+1 day")) == "86403"); -- strftime null timestring ok
EXPECT((select strftime("%W", "now", "+1 month", "start of month", "-3 minutes", "weekday 4")) is not null); -- strftime with multiple modifiers on now ok

END_TEST(strftime)

BEGIN_TEST(cast_expr)
  EXPECT((select cast(1.3 as int)) == 1); -- cast expression
END_TEST(cast_expr)

BEGIN_TEST(union_all_test)
 declare C cursor for
    select 1 as A, 2 as B
    union all
    select 3 as A, 4 as B;
  fetch C;
  EXPECT(C.A = 1);
  EXPECT(C.B = 2);
  fetch C;
  EXPECT(C.A = 3);
  EXPECT(C.B = 4);
END_TEST(union_all_test)

BEGIN_TEST(union_test)
 declare C cursor for
    select 1 as A, 2 as B
    union
    select 1 as A, 2 as B;
  fetch C;
  EXPECT(C.A = 1);
  EXPECT(C.B = 2);
  fetch C;
  EXPECT(NOT C); -- no more rows
END_TEST(union_test)

BEGIN_TEST(union_test_with_nullable)
 declare C cursor for
    select nullable(121) as A, 212 as B
    union
    select nullable(121) as A, 212 as B;
  fetch C;
  EXPECT(C.A = 121);
  EXPECT(C.B = 212);
  fetch C;
  EXPECT(NOT C);
END_TEST(union_test_with_nullable)

BEGIN_TEST(with_test)
 declare C cursor for
  with X(A,B) as ( select 1,2)
  select * from X;

  fetch C;
  EXPECT(C.A = 1);
  EXPECT(C.B = 2);
  fetch C;
  EXPECT(NOT C);
END_TEST(with_test)

BEGIN_TEST(with_recursive_test)
declare C cursor for
  with recursive
    c1(current) as (
     select 1
     union all
     select current+1 from c1
     limit 5
    ),
    c2(current) as (
     select 6
     union all
     select current+1 from c2
     limit 5
    )
  select current as X from c1
  union all
  select current as X from c2;

  declare i integer not null;
  set i := 1;

  loop fetch C
  begin
    EXPECT(C.X == i); -- iterating over the recursive result
    set i := i + 1;
  end;
  EXPECT(i == 11); -- 10 results matched, 11th did not match
END_TEST(with_recursive_test)


create proc outint(out int1 integer, out int2 integer not null)
begin
  declare C1 cursor for select 1;
  fetch C1 into int1;
  declare C2 cursor for select 2;
  fetch C2 into int2;
END;

BEGIN_TEST(fetch_output_param)
  declare int1 integer;
  declare int2 integer not null;
  call outint(int1, int2);
  EXPECT(int1 == 1); -- bind output nullable
  EXPECT(int2 == 2); -- bind output not nullable
END_TEST(fetch_output_param)

declare function run_test_math(int1 integer not null, out int2 integer) integer not null;
declare function string_create() create text;
declare function string_ref_count(str text) integer not null;

BEGIN_TEST(external_functions)
  declare int_out integer;
  declare int_result integer not null;

  set int_result := run_test_math(100, int_out);
  EXPECT(int_out == 500);
  EXPECT(int_result == 700);

  declare text_result text;
  set text_result := string_create();

  EXPECT(text_result like "%Hello%");
END_TEST(external_functions)

declare function set_create() create object not null;
declare function set_add(_set object not null, _key text not null) bool not null;
declare function set_contains(_set object not null, _key text not null) bool not null;

BEGIN_TEST(external_set)
 -- stress the create and copy semantics
 declare _set object not null;
 set _set := set_create();
 declare _set2 object not null;
 set _set2 := set_create();
 set _set := _set2; -- this is a copy

 EXPECT(_set is not null);  -- successful create
 EXPECT(not set_contains(_set, "garbonzo")); -- initially empty
 EXPECT(set_add(_set, "garbonzo")); -- successful addition
 EXPECT(set_contains(_set, "garbonzo")); -- key added
 EXPECT(not set_add(_set, "garbonzo")); -- duplicate addition
END_TEST(external_set)

BEGIN_TEST(object_notnull)
 declare _setNN object not null;
 declare _set object;
 set _set := set_create();
 set _setNN := attest_notnull(_set);
 EXPECT(_set == _setNN); // should be the same pointer
END_TEST(object_notnull)

BEGIN_TEST(dummy_values)
  delete from mixed;
  declare i integer not null;
  set i := 0;
  while (i < 20)
  begin
    insert into mixed (bl) values (cast(i as blob)) @dummy_seed(i) @dummy_nullables @dummy_defaults;
    set i := i + 1;
  end;

  declare C cursor for select * from mixed;
  set i := 0;
  while (i < 20)
  begin
    fetch C;
    EXPECT(C.id == i);
    EXPECT(C.name == printf("name_%d", i));
    EXPECT(C.code == i);
    EXPECT(not C.flag == not i);
    EXPECT(C.rate == i);
    set i := i + 1;
  end;
END_TEST(dummy_values)

BEGIN_TEST(blob_basics)
  declare s text not null;
  set s := "a string";
  declare b blob not null;
  set b := blob_from_string(s);
  declare s2 text not null;
  set s2 := string_from_blob(b);
  EXPECT(s == s2); -- blob conversion failed
  EXPECT(b == blob_from_string("a string"));
  EXPECT(b IS blob_from_string("a string"));
  EXPECT(b <> blob_from_string("a strings"));
  EXPECT(b IS NOT blob_from_string("a strings"));

  declare b_null blob;
  set b_null := null;
  declare s_null text;
  set s_null := null;
  EXPECT(b_null IS b_null);
  EXPECT(s_null IS s_null);
  EXPECT(b_null IS NOT b);
  EXPECT(s_null IS NOT s);
  EXPECT(b_null IS NULL);
  EXPECT(s_null IS NULL);
END_TEST(blob_basics)

create proc blob_table_maker()
begin
  create table if not exists blob_table(
    id integer not null,
    b1 blob,
    b2 blob not null
  );
  delete from blob_table;
end;

create proc load_blobs()
begin
  call blob_table_maker();

  declare i, count integer not null;
  set i := 0;
  set count := 20;

  declare s text not null;
  declare b1 blob;
  declare b2 blob not null;

  while (i < count)
  begin
    set s := printf("nullable blob %d", i);
    set b1 := blob_from_string(s);
    set s := printf("not nullable blob %d", i);
    set b2 := blob_from_string(s);
    insert into blob_table(id, b1, b2) values (i, b1, b2);
     set i := i + 1;
  end;
end;

BEGIN_TEST(blob_data_manip)
  call load_blobs();
  declare i, count integer not null;

  declare C cursor for select * from blob_table order by id;
  set i := 0;
  set count := 20;

  loop fetch C
  begin
     declare s1, s2 text;
     EXPECT(i == C.id);

     set s1 := string_from_blob(c.b1);
     EXPECT(s1 == printf("nullable blob %d", i)); -- nullable blob failed to round trip

     set s2 := string_from_blob(c.b2);
     EXPECT(s2 == printf("not nullable blob %d", i)); -- not nullable blob failed to round trip

     set i := i + 1;
  end;

  EXPECT(i == count); -- wrong number of rows
END_TEST(blob_data_manip)

create procedure get_blob_table()
begin
  select * from blob_table;
end;

create procedure load_sparse_blobs()
begin
  call blob_table_maker();

  declare s text not null;
  declare b1 blob;
  declare b2 blob not null;

  declare i, count integer not null;
  set i := 0;
  set count := 20;

  while (i < count)
  begin
    set s := printf("nullable blob %d", i);
    set b1 := case when i % 2 == 0 then blob_from_string(s) else null end;
    set s := printf("not nullable blob %d", i);
    set b2 := blob_from_string(s);
    insert into blob_table(id, b1, b2) values (i, b1, b2);
     set i := i + 1;
  end;
end;

BEGIN_TEST(blob_data_manip_nullables)
  declare i, count integer not null;
  declare C cursor for select * from blob_table order by id;
  set i := 0;
  set count := 20;

  call load_sparse_blobs();

  loop fetch C
  begin
     declare s1, s2 text;
     set s1 := string_from_blob(C.b1);
     EXPECT(i == C.id);
     if i % 2 == 0 then
       set s1 := string_from_blob(C.b1);
       EXPECT(s1 == printf("nullable blob %d", i)); -- nullable blob failed to round trip
     else
       EXPECT(C.b1 is null);
     end if;
     set s2 := string_from_blob(C.b2);
     EXPECT(s2 == printf("not nullable blob %d", i)); -- not nullable blob failed to round trip
     set i := i + 1;
  end;

  EXPECT(i == count); -- wrong number of rows
END_TEST(blob_data_manip_nullables)

create proc row_getter(x integer not null, y real not null, z text)
begin
  declare C cursor for select x X, y Y, z Z;
  fetch C;
  out C;
end;

BEGIN_TEST(data_reader)
  declare C cursor fetch from call row_getter(1, 2.5, "xyzzy");
  EXPECT(C.X == 1);
  EXPECT(C.Y == 2.5);
  EXPECT(C.Z == "xyzzy");
END_TEST(data_reader)

-- test simple recursive function -- using func syntax!
create procedure fib2 (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    set result := fib2(arg-1) + fib2(arg-2);
  end if;
end;

BEGIN_TEST(recurse_with_proc)
  EXPECT(fib2(1) == 1);
  EXPECT(fib2(2) == 1);
  EXPECT(fib2(3) == 2);
  EXPECT(fib2(4) == 3);
  EXPECT(fib2(5) == 5);
  EXPECT(fib2(6) == 8);
END_TEST(recurse_with_proc)

-- test simple recursive function -- using func syntax!
create procedure fib3 (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := (select 1); -- for this to be a dml proc
  else
    set result := fib3(arg-1) + fib3(arg-2);
  end if;
end;

BEGIN_TEST(recurse_with_dml_proc)
  -- we force all the error handling code to run with this flavor
  EXPECT(fib3(1) == 1);
  EXPECT(fib3(2) == 1);
  EXPECT(fib3(3) == 2);
  EXPECT(fib3(4) == 3);
  EXPECT(fib3(5) == 5);
  EXPECT(fib3(6) == 8);
END_TEST(recurse_with_dml_proc)

BEGIN_TEST(row_id_test)
  call load_mixed();
  declare C cursor for select rowid from mixed;
  declare r integer not null;
  set r := 1;

  loop fetch C
  begin
    EXPECT(C.rowid == r);
    set r := r + 1;
  end;
END_TEST(row_id_test)


BEGIN_TEST(bind_and_fetch_all_types)
  declare i integer not null;
  declare l long not null;
  declare r real not null;
  declare b bool not null;
  declare s text not null;
  declare bl blob not null;

  set i := 10;
  set l := 1234567890156789L;
  set r := 1234.45;
  set b := 1;
  set s := "string";
  set bl := blob_from_string("blob text");

  EXPECT(13*i == (select 13*i));
  EXPECT(13*l == (select 13*l));
  EXPECT(13*r == (select 13*r));
  EXPECT(not b == (select not b));
  EXPECT(printf("foo %s", s) == (select printf("foo %s", s)));
  EXPECT("blob text" == string_from_blob((select bl)));
END_TEST(bind_and_fetch_all_types)

BEGIN_TEST(bind_and_fetch_all_types_nullable)
  declare i integer;
  declare l long;
  declare r real;
  declare b bool;
  declare s text;
  declare bl blob;

  set i := 10;
  set l := 1234567890156789L;
  set r := 1234.45;
  set b := 1;
  set s := "string";
  set bl := blob_from_string("blob text");

  EXPECT(13*i == (select 13*i));
  EXPECT(13*l == (select 13*l));
  EXPECT(13*r == (select 13*r));
  EXPECT(not b == (select not b));
  EXPECT(printf("foo %s", s) == (select printf("foo %s", s)));
  EXPECT("blob text" == string_from_blob((select bl)));
END_TEST(bind_and_fetch_all_types_nullable)

BEGIN_TEST(fetch_all_types_cursor)
  declare i integer not null;
  declare l long not null;
  declare r real not null;
  declare b bool not null;
  declare s text not null;
  declare bl blob not null;

  set i := 10;
  set l := 1234567890156789L;
  set r := 1234.45;
  set b := 1;
  set s := "string";
  set bl := blob_from_string("blob text");

  declare C cursor for select i*13 i, l*13 l, r*13 r, not b b, printf("foo %s",s) s, bl bl;
  fetch C;
  EXPECT(13*i == C.i);
  EXPECT(13*l == C.l);
  EXPECT(13*r == C.r);
  EXPECT(not b == C.b);
  EXPECT(printf("foo %s", s) == C.s);
  EXPECT("blob text" == string_from_blob(C.bl));

  fetch C;
  EXPECT(not C);
  EXPECT(C.i ==  0);
  EXPECT(C.l ==  0);
  EXPECT(C.r ==  0);
  EXPECT(C.b ==  0);
  EXPECT(C.s is null); -- even though s is not null, it is null... sigh
  EXPECT(c.bl is null); -- even though bl is not null, it is null... sigh
END_TEST(fetch_all_types_cursor)

BEGIN_TEST(fetch_all_types_cursor_nullable)
  declare i integer;
  declare l long;
  declare r real;
  declare b bool;
  declare s text;
  declare bl blob;

  set i := 10;
  set l := 1234567890156789L;
  set r := 1234.45;
  set b := 1;
  set s := "string";
  set bl := blob_from_string("blob text");

  declare C cursor for select i*13 i, l*13 l, r*13 r, not b b, printf("foo %s",s) s, bl bl;
  fetch C;
  EXPECT(C);
  EXPECT(13*i == C.i);
  EXPECT(13*l == C.l);
  EXPECT(13*r == C.r);
  EXPECT(not b == C.b);
  EXPECT(printf("foo %s", s) == C.s);
  EXPECT("blob text" == string_from_blob(C.bl));

  fetch C;
  EXPECT(not C);
  EXPECT(C.i is null);
  EXPECT(C.l is null);
  EXPECT(C.r is null);
  EXPECT(C.b is null);
  EXPECT(C.s is null);
  EXPECT(c.bl is null);
END_TEST(fetch_all_types_cursor_nullable)

-- || is not yet implemented
BEGIN_TEST(concat_pri)
END_TEST(concat_pri)

-- Test precedence of multiply with (* / %) with add (+ -)
BEGIN_TEST(multiply_pri)
  EXPECT(1+2*3 == 7);
  EXPECT(1+2*3+4*5 == 27);
  EXPECT(1+2/2 == 2);
  EXPECT(1+2/2*4 == 5);
  EXPECT(1+2/2*4 == 5);
  EXPECT(1*2+3 == 5);
  EXPECT(1*2+6/3 == 4);
  EXPECT(1*2+6/3 == 4);
  EXPECT(2*3*4+3/3 == 25);
  EXPECT(-5*5 == -25);
  EXPECT(5-5*5 == -20);
  EXPECT(4+5*5 == 29);
  EXPECT(4*5+5 == 25);
  EXPECT(4*4-1 == 15);
  EXPECT(10-4*2 == 2);
  EXPECT(25%3/2 == 0);
  EXPECT(25/5%2 == 1);
  EXPECT(25*5%2 == 1);
  EXPECT(25*5%4%2 == 1);
  EXPECT(25-5%2 == 24);
  EXPECT(15%3-2 == -2);
  EXPECT(15-30%4 == 13);
  EXPECT(15-30/2 == 0);
  EXPECT(15/5-3 == 0);
  EXPECT(15*5-3 == 72);
  EXPECT(5*5-3 == 22);
  EXPECT(25+5%2 == 26);
  EXPECT(15%3+2 == 2);
  EXPECT(15+30%4 == 17);
  EXPECT(15+30/2 == 30);
  EXPECT(15/5+3 == 6);
  EXPECT(15*5+3 == 78);
  EXPECT(5*5+3 == 28);
  EXPECT(5*12/3 == 20);
  EXPECT(5*12/3%7 == 6);
  EXPECT(9%12/3*7 == 21);
END_TEST(multiply_pri)

-- Test precedence of binary (<< >> & |) with add (+ -)
BEGIN_TEST(shift_pri)
  EXPECT(10<<1+1 == 40);
  EXPECT(1+10<<1 == 22);
  EXPECT(10<<1-1 == 10);
  EXPECT(10<<4-1 == 80);
  EXPECT(10-1<<1 == 18);

  EXPECT(10>>3-1 == 2);
  EXPECT(11-1>>1 == 5);
  EXPECT(10>>1+1 == 2);
  EXPECT(1+10>>1 == 5);

  EXPECT(10&1+1 == 2);
  EXPECT(1+10&1 == 1);
  EXPECT(1+10&7 == 3);
  EXPECT(10-1&7 == 1);
  EXPECT(10-4&7 == 6);

  EXPECT(10|1+1 == 10);
  EXPECT(10|4 == 14);
  EXPECT(1+10|4 == 15);
  EXPECT(10-1|7 == 15);
  EXPECT(10-3|7 == 7);

  EXPECT(6&4 == 4);
  EXPECT(6&4|12 == 12);
  EXPECT(6&4|12|2 == 14);
  EXPECT(6&4|12|2|2 == 14);
  EXPECT(6&4|12|2|2<<3 == 112);
  EXPECT(6&4|12|2|2<<3>>3<<2 == 56);
END_TEST(shift_pri)

-- Test precedence of inequality (< <= > >=) with binary (<< >> & |)
BEGIN_TEST(inequality_pri)
  EXPECT(10 < 10<<1);
  EXPECT(10 <= 10<<1);
  EXPECT(10 > 10>>1);
  EXPECT(10 >= 10>>1);
  EXPECT(0 >= 0>>1);
  EXPECT(0 <= 0<<1);
  EXPECT(5 >= 0<<31);
  EXPECT(5 > 0<<31);
  EXPECT(16>>1 >= 4<<1);
  EXPECT(4<<1 <= 16>>1);
  EXPECT(16>>1 > 3<<1);
  EXPECT(16>>1 >= 3<<1);
  EXPECT(16>>1 <= 4<<1);

  EXPECT(16&8 <= 4|8);
  EXPECT(16&8 < 15);
  EXPECT(16&8 <= 15);
  EXPECT(16&17 > 4);
  EXPECT(16&17 >= 4);
  EXPECT(6 > 4&5);
  EXPECT(6 >= 4&5);
  EXPECT(6 > 4|5);
  EXPECT(6 >= 4|5);

  EXPECT(3|8 >= 4&5);
  EXPECT(3|8 > 4&5);
  EXPECT(3|4 >= 4&5);
  EXPECT(3|4 > 4&5);
  EXPECT(4&5 <= 3|8);
  EXPECT(4&5 < 3|8);
  EXPECT(4&5 <= 3|4);
  EXPECT(4&5 < 3|4);
  EXPECT(4|3 <= 3|4);
  EXPECT(4&5 <= 5&4);
  EXPECT(4&5 >= 5&4);

  EXPECT(4&5 >= 5&4 > 0);
  EXPECT(4&5 >= 5&4 <= 1);
  EXPECT(4&5 >= 5&4 >= 1);
  EXPECT(3&10 <= 100 <= 3&2);
  EXPECT((3&10 <= 100) <= 3&2 == 3&10 <= 100 <= 3&2);
  EXPECT(5 > 3 > -1 > 0);
END_TEST(inequality_pri)

create proc true_if_null(b bool, out result bool not null)
begin
  set result := b is null;
end;

-- Test precedence of equality (= == != <> LIKE GLOB MATCH IN NOT IN IS_NOT_NULL IS_NULL) with binary (< <= > >=)
BEGIN_TEST(equality_pri)
  EXPECT(5 == 5);
  EXPECT(5 < 6 == 6 > 5);
  EXPECT(5 <= 6 == 6 >= 5);
  EXPECT(5 < 6 == 6 >= 5);
  EXPECT(5 <= 6 == 6 > 5);
  EXPECT(5 <= 6 == 1);
  EXPECT(1 == 5 < 6);
  EXPECT(1 == 5 <= 6);
  EXPECT(1 == 0 + 1);
  EXPECT(1 == 1 + 0 * 1);
  EXPECT(1 == 0 * 1 + 1);
  EXPECT(1 == 0 * -1 + 1);
  EXPECT(1 + 1 == 3 - 1 == 1);
  EXPECT(1 + 1 == 3 - 1 != 0);
  EXPECT(1 + 1 == 3 - 1 != 30);

  EXPECT(5 = 5);
  EXPECT(5 < 6 = 6 > 5);
  EXPECT(5 <= 6 = 6 >= 5);
  EXPECT(5 < 6 = 6 >= 5);
  EXPECT(5 <= 6 = 6 > 5);
  EXPECT(5 <= 6 = 1);
  EXPECT(1 = 5 < 6);
  EXPECT(1 = 5 <= 6);
  EXPECT(1 = 0 + 1);
  EXPECT(1 = 1 + 0 * 1);
  EXPECT(1 = 0 * 1 + 1);
  EXPECT(1 = 0 * -1 + 1);
  EXPECT(1 + 1 = 3 - 1 = 1);
  EXPECT(1 + 1 = 3 - 1 <> 0);
  EXPECT(1 + 1 == 3 - 1 <> 0);
  EXPECT(1 + 1 = 3 - 1 <> 30);
  EXPECT(1 + 1 == 3 - 1 <> 30);

  EXPECT(1 == 1 <> 0 == 1 = 1 != 0 = 1 == 1);

  -- CQL requires both operands of binary_like to be text, so there is no way to test
  -- order of operations with <, <=, etc. When concat (||) is implemented, it is
  -- possible to write a test case.

  -- CQL requires both operands of binary_like to be text, so there is no way to test
  -- order of operations with <, <=, etc. When concat (||) is implemented, it is
  -- possible to write a test case.

  -- GLOB must be inside a select statement so it also cannot be tested
  -- MATCH can only be in a select statement, no test necessary

  -- Test IS_NOT and IS
  EXPECT(1 + 1 IS NULL == 0);
  EXPECT(1 + 1 IS NOT NULL == 1);
  EXPECT(1 + 1 IS NULL + 1 == 0); -- Evaluated as: (1 + 1) IS (NULL + 1) == 0;
  EXPECT(1 + 1 IS NOT NULL);
  EXPECT((1 + 1 IS NOT NULL) + 1 == 2);
  EXPECT(1 + 1 IS NOT NULL + 1 == 1);
  EXPECT(1 + NULL IS NULL);
  EXPECT(NULL + 1 IS NULL);
  EXPECT(NULL * 1 IS NULL);
  EXPECT(NULL * 0 IS NULL);
  EXPECT(0 * NULL * 0 IS NULL);
  EXPECT(NULL > 0 IS NULL);
  EXPECT(NULL >= 1 IS NULL);
  EXPECT(NULL < 2 IS NULL);
  EXPECT(NULL <= 3 IS NULL);
  EXPECT(1 + NULL == 3 IS NULL);
  EXPECT(1 + NULL != 3 IS NULL);
  EXPECT(1 + NULL <> 3 IS NULL);
  EXPECT(1 = NULL * 1 + 1 IS NULL);
  EXPECT(1 = NULL * -1 + 1 IS NULL);
  EXPECT(1 + NULL = 3 - 1 = 1 IS NULL);
  EXPECT(1 + NULL = 3 - 1 <> 0 IS NULL);
  EXPECT(1 + NULL == 3 - 1 <> 0 IS NULL);
  EXPECT(1 + NULL = 3 - 1 <> 30 IS NULL);
  EXPECT(1 + NULL == 3 - 1 <> 30 IS NULL);
  EXPECT((NULL IS NOT NULL) == 0);
  EXPECT(1 + 1 IS NOT NULL);
  EXPECT(NULL == 3 IS NULL);
  EXPECT(((NULL == 3) IS NULL) == 1);
  EXPECT((NULL == 3 IS NULL) == 1);
  EXPECT((NULL == 3 IS NULL) == 1);
  EXPECT((NULL == 3 IS NULL) IS NOT NULL);
  EXPECT((1 + NULL == 3 IS NOT NULL) == 0);
  EXPECT((1 + NULL = 3 - 1 <> 0 IS NOT NULL) == 0);
  EXPECT((1 + NULL == 3 - 1 <> 0 IS NOT NULL) == 0);
  EXPECT((1 + NULL = 3 - 1 <> 30 IS NOT NULL) == 0);

  -- Basic IS tests, all non null
  EXPECT(2 * 3 IS 4 + 2);
  EXPECT(2 * 3 IS 4 + 2);
  EXPECT(10-4*2 IS 2);
  EXPECT(25%3/2 IS 0);
  EXPECT(25/5%2 IS 1);
  EXPECT(25*5%2 IS 1);
  EXPECT(25*5%4%2 IS 1);
  EXPECT(25-5%2 IS 24);
  EXPECT(15%3-2 IS -2);
  EXPECT(15-30%4 IS 13);
  EXPECT(15-30/2 IS 0);
  EXPECT(15/5-3 IS 0);
  EXPECT(15*5-3 IS 72);
  EXPECT(5*5-3 IS 22);
  EXPECT(25+5%2 IS 26);
  EXPECT(15%3+2 IS 2);
  EXPECT(15+30%4 IS 17);
  EXPECT(15+30/2 IS 30);
  EXPECT(15/5+3 IS 6);
  EXPECT(15*5+3 IS 78);
  EXPECT(5*5+3 IS 28);
  EXPECT(5*12/3 IS 20);
  EXPECT(5*12/3%7 IS 6);
  EXPECT(9%12/3*7 IS 21);

  -- IS tests with null
  EXPECT(1 IS 1 == 1 IS 1 == 1);
  EXPECT(5 > 6 IS 2 < 1);
  EXPECT(5 <= 6 IS 2 > 1);
  EXPECT(5 == 5 IS 2 > 1);
  EXPECT("1" IS "2" == 0);
  EXPECT("1" IS NULL == 0);
  EXPECT(NULL IS "1" == 0);
  EXPECT(NULL IS NULL);
  EXPECT(NULL == 0 IS NULL);
  EXPECT(NULL IS NULL == 1 != 0);
  EXPECT(NULL IS NULL = 1 <> 0);
  EXPECT(NULL == NULL IS NULL);
  EXPECT(NULL IS (NULL == 0));
  EXPECT(NULL IS NOT NULL == 0);
  EXPECT((NULL IS NOT NULL) == 0);
  EXPECT(5 > 2 IS NOT NULL);
  EXPECT(NULL IS NOT 2 < 3);
  EXPECT(NULL IS NULL + 1);
  EXPECT(NULL IS 1 + NULL);
  EXPECT(NULL IS 1 << NULL);

  -- Test IN
  EXPECT(3 IN (1, 2) == 0);
  EXPECT(3 + 2 IN (1, 5));
  EXPECT(3 / 3 IN (1, 2));
  EXPECT(3 / 3 IN (1, 2) IN (1));
  EXPECT(1 IN (NULL, 1));
  EXPECT(NOT (1 IN (NULL, 5)));
  EXPECT(true_if_null(NULL IN (1)));

  -- Test NOT IN
  EXPECT(3 NOT IN (1, 2) == 1);
  EXPECT(1 NOT IN (1, 2) == 0);
  EXPECT(3 + 1 NOT IN (1, 5));
  EXPECT(3 / 1 NOT IN (1, 2));
  EXPECT(3 / 1 NOT IN (1, 2) NOT IN (0));
  EXPECT(NOT (1 NOT IN (NULL, 1)));
  EXPECT(1 NOT IN (NULL, 5));
  EXPECT(true_if_null(NULL NOT IN (1)));

  declare x text;
  set x := NULL;
  EXPECT((x IN ("foo", "goo")) IS NULL);
  EXPECT((x NOT IN ("foo", "goo")) IS NULL);
END_TEST(equality_pri)

-- AND tests with = == != <> IS IS_NOT IN NOT IN
BEGIN_TEST(and_pri)
  EXPECT(3 + 3 AND 5);
  EXPECT((3 + 3 AND 0) == 0);
  EXPECT((NULL AND 1) IS NULL);
  EXPECT((NULL AND 1 = NULL) IS NULL);
  EXPECT(NOT (NULL AND 1 IS NULL));
  EXPECT((NULL AND 0) == 0);
  EXPECT(NOT (NULL AND 0));
  EXPECT(1 AND 0 == 0);
  EXPECT(1 AND 0 = 0);
  EXPECT(1 AND 1 != 0);
  EXPECT(1 AND 1 <> 0);
  EXPECT(5 IS 5 AND 2 IS 2);
  EXPECT(5 IS NOT NULL AND 2 IS 2);
  EXPECT(5 IS NOT NULL AND 2 IS 2);
  EXPECT(5 AND 0 + 1);
  EXPECT(5 AND 0 * 1 + 1);
  EXPECT(5 AND 0 >> 4 >= -1);
  EXPECT(5 AND 0 | 4 & 12);
  EXPECT(5 AND 6 / 3);
  EXPECT((5 AND 25 % 5) == 0);
  EXPECT(5 AND 0 IN (0));
  EXPECT(5 AND 1 NOT IN (0));
  EXPECT(NOT(5 AND 0 NOT IN (0)));
END_TEST(and_pri)

-- Test AND with OR
BEGIN_TEST(or_pri)
  -- The following tests show that if AND and OR were evaluated from
  -- left to right, then the output would be different
  EXPECT((0 OR 1 OR 1 AND 0 OR 0) != ((((0 OR 1) OR 1) AND 0) OR 0));
  EXPECT((1 OR 1 AND 0 AND 1 AND 0) != ((((1 OR 1) AND 0) AND 1) AND 0));
  EXPECT((0 OR 1 OR 1 AND 0 AND 1) != ((((0 OR 1) OR 1) AND 0) AND 1));
  EXPECT((1 OR 1 OR 1 AND 0 AND 0) != ((((1 OR 1) OR 1) AND 0) AND 0));
  EXPECT((1 OR 1 OR 1 AND 0 OR 0) != ((((1 OR 1) OR 1) AND 0) OR 0));
  EXPECT((1 AND 1 AND 1 OR 1 AND 0) != ((((1 AND 1) AND 1) OR 1) AND 0));
  EXPECT((1 OR 0 AND 0 AND 1 OR 0) != ((((1 OR 0) AND 0) AND 1) OR 0));
  EXPECT((1 AND 1 OR 0 AND 0 AND 1) != ((((1 AND 1) OR 0) AND 0) AND 1));
  EXPECT((1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0));
  EXPECT((1 OR 0 AND 0 OR 1 AND 0) != ((((1 OR 0) AND 0) OR 1) AND 0));
  EXPECT((1 OR 1 AND 1 AND 1 AND 0) != ((((1 OR 1) AND 1) AND 1) AND 0));
  EXPECT((0 AND 0 OR 1 OR 0 AND 0) != ((((0 AND 0) OR 1) OR 0) AND 0));
  EXPECT((0 OR 1 OR 1 AND 0 AND 0) != ((((0 OR 1) OR 1) AND 0) AND 0));
  EXPECT((1 AND 1 AND 1 OR 0 AND 0) != ((((1 AND 1) AND 1) OR 0) AND 0));
  EXPECT((1 OR 1 OR 1 AND 0 AND 1) != ((((1 OR 1) OR 1) AND 0) AND 1));
  EXPECT((1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0));
END_TEST(or_pri)

-- Take some priority tests and replace constants with nullable variables
BEGIN_TEST(nullable_test)
  declare x0 integer;
  declare x1 integer;
  declare x2 integer;
  declare x3 integer;
  declare x4 integer;
  declare x5 integer;
  declare x6 integer;
  declare x7 integer;
  declare x8 integer;
  declare x9 integer;
  declare temp0 integer;
  declare temp1 integer;
  declare temp2 integer;
  declare temp3 integer;
  declare temp4 integer;

  set x0 := 0;
  set x1 := 1;
  set x2 := 2;
  set x3 := 3;
  set x4 := 4;
  set x5 := 5;
  set x6 := 6;
  set x7 := 7;
  set x8 := 8;
  set x9 := 9;

  set temp0 := 27;
  EXPECT(x1+x2*x3+x4*x5 == temp0);
  EXPECT(x1+x2/x2 == x2);
  EXPECT(x1+x2/x2*x4 == x5);
  EXPECT(x1+x2/x2*x4 == x5);
  EXPECT(x1*x2+x3 == x5);
  EXPECT(x1*x2+x6/x3 == x4);
  EXPECT(x1*x2+x6/x3 == x4);
  set temp0 := 25;
  EXPECT(x2*x3*x4+x3/x3 == temp0);
  set temp0 := -25;
  EXPECT(-x5*x5 == temp0);
  set temp0 := -20;
  EXPECT(x5-x5*x5 == temp0);
  set temp0 := 29;
  EXPECT(x4+x5*x5 == temp0);
  set temp0 := 25;
  EXPECT(x4*x5+x5 == temp0);
  set temp0 := 15;
  EXPECT(x4*x4-x1 == temp0);
  set temp0 := 10;
  EXPECT(10-x4*x2 == x2);

  set temp0 := 10;

  set temp1 := 40;
  EXPECT(temp0<<x1+x1 == temp1);
  set temp1 := 22;
  EXPECT(x1+temp0<<x1 == temp1);
  EXPECT(temp0<<x1-x1 == temp0);
  set temp1 := 80;
  EXPECT(temp0<<x4-x1 == temp1);
  set temp1 := 18;
  EXPECT(temp0-x1<<x1 == temp1);

  EXPECT(temp0>>x3-x1 == x2);
  set temp1 := 11;
  EXPECT(temp1-x1>>x1 == x5);
  EXPECT(temp0>>x1+x1 == x2);
  EXPECT(x1+temp0>>x1 == x5);

  EXPECT(temp0&x1+x1 == x2);
  EXPECT(x1+temp0&x1 == x1);
  EXPECT(x1+temp0&x7 == x3);
  EXPECT(temp0-x1&x7 == x1);
  EXPECT(temp0-x4&x7 == x6);

  EXPECT(temp0|x1+x1 == temp0);
  set temp1 := 14;
  EXPECT(temp0|x4 == temp1);
  set temp1 := 15;
  EXPECT(x1+temp0|x4 == temp1);
  EXPECT(temp0-x1|x7 == temp1);
  EXPECT(temp0-x3|x7 == x7);

  set temp1 := 12;

  EXPECT(x6&x4 == x4);
  EXPECT(x6&x4|temp1 == temp1);
  set temp2 := 14;
  EXPECT(x6&x4|temp1|x2 == temp2);
  EXPECT(x6&x4|temp1|x2|x2 == temp2);
  set temp2 := 112;
  EXPECT(x6&x4|temp1|x2|x2<<x3 == temp2);
  set temp2 := 56;
  EXPECT(x6&x4|temp1|x2|x2<<x3>>x3<<x2 == temp2);

  EXPECT(temp0 < temp0<<x1);
  EXPECT(temp0 <= temp0<<x1);
  set temp1 := 31;
  EXPECT(x5 >= x0<<temp1);
  EXPECT(x5 > x0<<temp1);
  set temp1 := 16;
  EXPECT(temp1>>x1 >= x4<<x1);
  EXPECT(x4<<x1 <= temp1>>x1);
  EXPECT(temp1>>x1 > x3<<x1);
  EXPECT(temp1>>x1 >= x3<<x1);
  EXPECT(temp1>>x1 <= x4<<x1);

  EXPECT(temp1&x8 <= x4|x8);
  set temp2 := 15;
  EXPECT(temp1&8 < temp2);
  EXPECT(x6 > x4|x5);
  EXPECT(x6 >= x4|x5);

  EXPECT(x4&x5 <= x3|x4);
  EXPECT(x4&x5 < x3|x4);
  EXPECT(x4|x3 <= x3|x4);
  EXPECT(x4&x5 <= x5&x4);
  EXPECT(x4&x5 >= x5&x4);

  EXPECT(x4&x5 >= x5&x4 > x0);
  EXPECT(x4&x5 >= x5&x4 <= x1);
  EXPECT(x4&x5 >= x5&x4 >= x1);
  set temp1 := 100;
  EXPECT(x3&temp0 <= temp1 <= x3&x2);
  EXPECT((x3&temp0 <= temp1) <= x3&x2 == x3&temp0 <= temp1 <= x3&x2);
  EXPECT(x5 > x3 > -x1 > x0);

  set temp1 := 30;
  EXPECT(x5 == x5);
  EXPECT(x5 < x6 == x6 > x5);
  EXPECT(x5 <= x6 == x6 >= x5);
  EXPECT(x5 < x6 == x6 >= x5);
  EXPECT(x5 <= x6 == x6 > x5);
  EXPECT(x5 <= x6 == x1);
  EXPECT(x1 == x5 < x6);
  EXPECT(x1 == x5 <= x6);
  EXPECT(x1 == x0 + x1);
  EXPECT(x1 == x1 + x0 * x1);
  EXPECT(x1 == x0 * x1 + x1);
  EXPECT(x1 == x0 * -x1 + x1);
  EXPECT(x1 + x1 == x3 - x1 == x1);
  EXPECT(x1 + x1 == x3 - x1 != x0);
  EXPECT(x1 + x1 == x3 - x1 != temp1);

  EXPECT(x5 = x5);
  EXPECT(x5 < x6 = x6 > x5);
  EXPECT(x5 <= x6 = x6 >= x5);
  EXPECT(x5 < x6 = x6 >= x5);
  EXPECT(x5 <= x6 = x6 > x5);
  EXPECT(x5 <= x6 = x1);
  EXPECT(x1 = x5 < x6);
  EXPECT(x1 = x5 <= x6);
  EXPECT(x1 = x0 + x1);
  EXPECT(x1 = x1 + x0 * x1);
  EXPECT(x1 = x0 * x1 + x1);
  EXPECT(x1 = x0 * -x1 + x1);
  EXPECT(x1 + x1 = x3 - x1 = x1);
  EXPECT(x1 + x1 = x3 - x1 <> x0);
  EXPECT(x1 + x1 == x3 - x1 <> x0);
  EXPECT(x1 + x1 = x3 - x1 <> temp1);
  EXPECT(x1 + x1 == x3 - x1 <> temp1);

  set temp1 := 30;
  declare temp_null integer;
  set temp_null := NULL;

  EXPECT(x1 + x1 IS NULL == x0);
  EXPECT(x1 + x1 IS NOT NULL == x1);
  EXPECT(x1 + x1 IS NULL + x1 == x0);
  EXPECT(x1 + x1 IS NOT NULL);
  EXPECT((x1 + x1 IS NOT NULL) + x1 == x2);
  EXPECT(x1 + x1 IS NOT NULL + x1 == x1);
  EXPECT(x1 + NULL IS NULL);
  EXPECT(NULL + x1 IS NULL);
  EXPECT(NULL * x1 IS NULL);
  EXPECT(NULL * x0 IS NULL);
  EXPECT(x0 * NULL * x0 IS NULL);
  EXPECT(NULL > x0 IS NULL);
  EXPECT(NULL >= x1 IS NULL);
  EXPECT(NULL < x2 IS NULL);
  EXPECT(NULL <= x3 IS NULL);
  EXPECT(x1 + NULL == x3 IS NULL);
  EXPECT(x1 + NULL != x3 IS NULL);
  EXPECT(x1 + NULL <> x3 IS NULL);
  EXPECT(x1 = temp_null * x1 + x1 IS temp_null);
  EXPECT(x1 = temp_null * -x1 + x1 IS temp_null);
  EXPECT(x1 + temp_null = x3 - x1 = x1 IS temp_null);
  EXPECT(x1 + temp_null = x3 - x1 <> x0 IS temp_null);
  EXPECT(x1 + temp_null == x3 - x1 <> x0 IS temp_null);
  EXPECT(x1 + temp_null = x3 - x1 <> temp1 IS temp_null);
  EXPECT(x1 + temp_null == x3 - x1 <> temp1 IS temp_null);
  EXPECT((temp_null IS NOT temp_null) == x0);
  EXPECT(x1 + x1 IS NOT temp_null);
  EXPECT(temp_null == x3 IS temp_null);
  EXPECT(((temp_null == x3) IS temp_null) == x1);
  EXPECT((temp_null == x3 IS temp_null) == x1);
  EXPECT((temp_null == x3 IS temp_null) == x1);
  EXPECT((temp_null == x3 IS temp_null) IS NOT temp_null);
  EXPECT((x1 + temp_null == x3 IS NOT temp_null) == x0);
  EXPECT((x1 + temp_null = x3 - x1 <> x0 IS NOT temp_null) == x0);
  EXPECT((x1 + temp_null == x3 - x1 <> x0 IS NOT temp_null) == x0);
  EXPECT((x1 + temp_null = x3 - x1 <> temp1 IS NOT temp_null) == x0);

  set temp0 := 25;

  EXPECT(x2 * x3 IS x4 + x2);
  EXPECT(x2 * x3 IS x4 + x2);
  set temp1 := 10;
  EXPECT(temp1-x4*x2 IS x2);
  EXPECT(temp0%x3/x2 IS x0);
  EXPECT(temp0/x5%x2 IS x1);
  EXPECT(temp0*x5%x2 IS x1);
  EXPECT(temp0*x5%x4%x2 IS x1);
  set temp1 := 24;
  EXPECT(temp0-x5%x2 IS temp1);
  set temp1 := 15;
  EXPECT(temp1%x3-x2 IS -x2);
  set temp2 := 30;
  set temp3 := 13;
  EXPECT(temp1-temp2%x4 IS temp3);
  EXPECT(temp1-temp2/x2 IS x0);
  EXPECT(temp1/x5-x3 IS x0);
  set temp3 := 72;
  EXPECT(temp1*x5-x3 IS temp3);
  set temp3 := 22;
  EXPECT(x5*x5-x3 IS temp3);
  set temp3 := 26;
  EXPECT(temp0+x5%x2 IS temp3);
  EXPECT(temp1%x3+x2 IS x2);
  set temp1 := 17;
  set temp2 := 30;
  set temp3 := 15;
  EXPECT(temp3+temp2%x4 IS temp1);
  set temp1 := 30;
  EXPECT(temp3+temp1/x2 IS temp1);
  EXPECT(temp3/x5+x3 IS x6);
  set temp1 := 78;
  EXPECT(temp3*x5+x3 IS temp1);
  set temp1 := 28;
  EXPECT(x5*x5+x3 IS temp1);
  set temp1 := 20;
  set temp2 := 12;
  EXPECT(x5*temp2/x3 IS temp1);
  EXPECT(x5*temp2/x3%x7 IS x6);
  set temp1 := 21;
  set temp2 := 12;
  EXPECT(x9%temp2/x3*x7 IS temp1);

  EXPECT(x1 IS x1 == x1 IS x1 == x1);
  EXPECT(x5 > x6 IS x2 < x1);
  EXPECT(x5 <= x6 IS x2 > x1);
  EXPECT(x5 == x5 IS x2 > x1);
  EXPECT(NULL IS NULL);
  EXPECT(NULL == x0 IS NULL);
  EXPECT(NULL IS NULL == x1 != x0);
  EXPECT(NULL IS NULL = x1 <> x0);
  EXPECT(NULL == NULL IS NULL);
  EXPECT(NULL IS (NULL == x0));
  EXPECT(NULL IS NOT NULL == x0);
  EXPECT((NULL IS NOT NULL) == x0);
  EXPECT(x5 > x2 IS NOT NULL);
  EXPECT(NULL IS NOT x2 < x3);
  EXPECT(NULL IS NULL + x1);
  EXPECT(NULL IS x1 + NULL);
  EXPECT(NULL IS x1 << NULL);

  declare one text;
  declare two text;
  set one := "1";
  set two := "2";
  EXPECT(one IS two == x0);
  EXPECT(one IS NULL == x0);
  EXPECT(NULL IS one == x0);

  -- Test IN
  EXPECT(x3 IN (x1, x2) == x0);
  EXPECT(x3 + x2 IN (x1, x5));
  EXPECT(x3 / x3 IN (x1, x2));
  EXPECT(x3 / x3 IN (x1, x2) IN (x1));
  EXPECT(x1 IN (NULL, x1));
  EXPECT(NOT (x1 IN (NULL, x5)));
  EXPECT(true_if_null(NULL IN (x1)));

  -- Test NOT IN
  EXPECT(x1 NOT IN (x1, x2) == x0);
  EXPECT(x3 NOT IN (x1, x2) == x1);
  EXPECT(x3 + x2 NOT IN (x1, x2));
  EXPECT(x3 / x1 NOT IN (x1, x2));
  EXPECT(x3 / x1 NOT IN (x1, x2) IN (x1));
  EXPECT(NOT (x1 NOT IN (NULL, x1)));
  EXPECT(x1 NOT IN (NULL, x5));
  EXPECT(true_if_null(NULL NOT IN (x1)));

  declare x text;
  set x := NULL;
  EXPECT((x IN ("foo", "goo")) IS NULL);
  EXPECT((x NOT IN ("foo", "goo")) IS NULL);

  EXPECT(x3 + x3 AND x5);
  EXPECT((x3 + x3 AND x0) == x0);
  EXPECT((NULL AND x1) IS NULL);
  EXPECT((NULL AND x1 = NULL) IS NULL);
  EXPECT(NOT (NULL AND x1 IS NULL));
  EXPECT((NULL AND x0) == x0);
  EXPECT(NOT (NULL AND x0));
  EXPECT(x1 AND x0 == x0);
  EXPECT(x1 AND x0 = x0);
  EXPECT(x1 AND x1 != x0);
  EXPECT(x1 AND x1 <> x0);
  EXPECT(x5 IS x5 AND x2 IS x2);
  EXPECT(x5 IS NOT NULL AND x2 IS x2);
  EXPECT(x5 IS NOT NULL AND x2 IS x2);
  EXPECT(x5 AND x0 + x1);
  EXPECT(x5 AND x0 * x1 + x1);
  EXPECT(x5 AND x0 >> x4 >= -x1);
  set temp1 := 12;
  EXPECT(x5 AND x0 | x4 & temp1);
  EXPECT(x5 AND x6 / x3);
  set temp1 := 25;
  EXPECT((x5 AND temp1 % x5) == x0);
  EXPECT(x5 AND x0 IN (x0));

  EXPECT((x0 OR x1 OR x1 AND x0 OR x0) != ((((x0 OR x1) OR x1) AND x0) OR x0));
  EXPECT((x1 OR x1 AND x0 AND x1 AND x0) != ((((x1 OR x1) AND x0) AND x1) AND x0));
  EXPECT((x0 OR x1 OR x1 AND x0 AND x1) != ((((x0 OR x1) OR x1) AND x0) AND x1));
  EXPECT((x1 OR x1 OR x1 AND x0 AND x0) != ((((x1 OR x1) OR x1) AND x0) AND x0));
  EXPECT((x1 OR x1 OR x1 AND x0 OR x0) != ((((x1 OR x1) OR x1) AND x0) OR x0));
  EXPECT((x1 AND x1 AND x1 OR x1 AND x0) != ((((x1 AND x1) AND x1) OR x1) AND x0));
  EXPECT((x1 OR x0 AND x0 AND x1 OR x0) != ((((x1 OR x0) AND x0) AND x1) OR x0));
  EXPECT((x1 AND x1 OR x0 AND x0 AND x1) != ((((x1 AND x1) OR x0) AND x0) AND x1));
  EXPECT((x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0));
  EXPECT((x1 OR x0 AND x0 OR x1 AND x0) != ((((x1 OR x0) AND x0) OR x1) AND x0));
  EXPECT((x1 OR x1 AND x1 AND x1 AND x0) != ((((x1 OR x1) AND x1) AND x1) AND x0));
  EXPECT((x0 AND x0 OR x1 OR x0 AND x0) != ((((x0 AND x0) OR x1) OR x0) AND x0));
  EXPECT((x0 OR x1 OR x1 AND x0 AND x0) != ((((x0 OR x1) OR x1) AND x0) AND x0));
  EXPECT((x1 AND x1 AND x1 OR x0 AND x0) != ((((x1 AND x1) AND x1) OR x0) AND x0));
  EXPECT((x1 OR x1 OR x1 AND x0 AND x1) != ((((x1 OR x1) OR x1) AND x0) AND x1));
  EXPECT((x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0));

END_TEST(nullable_test)

@attribute(cql:vault_sensitive)
create procedure load_encoded_table()
begin
  create table all_types_encoded_table(
    b0 bool @sensitive,
    i0 integer @sensitive,
    l0 long @sensitive,
    d0 real @sensitive,
    s0 text @sensitive,
    bl0 blob @sensitive,

    b1 bool not null @sensitive,
    i1 integer not null @sensitive,
    l1 long not null @sensitive,
    d1 real not null @sensitive,
    s1 text not null @sensitive,
    bl1 blob not null @sensitive
  );

  insert into all_types_encoded_table values (
    cast(0 as bool), 0, 0, 0.0, "0", cast("0" as blob),
    cast(1 as bool), 1, 1, 1.1, "1", cast("1" as blob)
  );

  select * from all_types_encoded_table;
end;

@attribute(cql:vault_sensitive)
create procedure load_encoded_cursor()
begin
  declare C cursor for select * from all_types_encoded_table;
  fetch C;
  out C;
end;

@attribute(cql:vault_sensitive)
create proc out_union_dml()
begin
  declare x cursor for select * from all_types_encoded_table;
  fetch x;
  out union x;
end;

@attribute(cql:vault_sensitive)
create proc out_union_not_dml()
begin
  declare bogus cursor for select 1; -- just to make the proc dml to test a non dml cursor x with vault.

  declare x cursor like all_types_encoded_table;
  fetch x using
    0 b0,
    0 i0,
    0 l0,
    0.0 d0,
    "0" s0,
    blob_from_string("0") bl0,
    1 b1,
    1 i1,
    1 l1,
    1.1 d1,
    "1" s1,
    blob_from_string("1") bl1;

  out union x;
end;

@attribute(cql:vault_sensitive)
create proc load_decoded_out_union()
begin
  declare C cursor for call out_union_dml();
  fetch C;
  out C;
end;

@attribute(cql:vault_sensitive)
create proc load_decoded_multi_out_union()
begin
  declare C cursor for call out_union_dml();
  fetch C;
  out union C;

  declare C1 cursor for call out_union_not_dml();
  fetch C1;
  out union C1;
end;

BEGIN_TEST(encoded_values)
  declare C cursor for call load_encoded_table();
  fetch C;
  EXPECT(C.b0 IS 0);
  EXPECT(C.i0 IS 0);
  EXPECT(C.l0 IS 0);
  EXPECT(C.d0 IS 0.0);
  EXPECT(C.s0 IS "0");
  EXPECT(string_from_blob(C.bl0) IS "0");
  EXPECT(C.b1 IS 1);
  EXPECT(C.i1 IS 1);
  EXPECT(C.l1 IS 1);
  EXPECT(C.d1 IS 1.1);
  EXPECT(C.s1 IS "1");
  EXPECT(string_from_blob(C.bl1) IS "1");

  declare C1 cursor for call out_union_dml();
  fetch C1;
  EXPECT(cql_cursor_diff_val(C, C1) IS NULL);

  declare C2 cursor for call out_union_not_dml();
  fetch C2;
  EXPECT(cql_cursor_diff_val(C, C2) IS NULL);

  declare C3 cursor fetch from call load_decoded_out_union();
  EXPECT(cql_cursor_diff_val(C, C3) IS NULL);
END_TEST(encoded_values)

BEGIN_TEST(encoded_null_values)
  create table encode_null_table(
      b0 bool @sensitive,
      i0 integer @sensitive,
      l0 long @sensitive,
      d0 real @sensitive,
      s0 text @sensitive,
      bl0 blob @sensitive
  );
  insert into encode_null_table using
    null b0,
    null i0,
    null l0,
    null d0,
    null s0,
    null bl0;

  declare C cursor for select * from encode_null_table;
  fetch C;

  EXPECT(C.b0 IS null);
  EXPECT(C.i0 IS null);
  EXPECT(C.l0 IS null);
  EXPECT(C.d0 IS null);
  EXPECT(C.s0 IS null);
  EXPECT(C.bl0 IS null);
END_TEST(encoded_null_values)

@attribute(cql:vault_sensitive=(y))
create procedure load_some_encoded_field()
begin
  create table some_encoded_field_table(x integer, y text @sensitive);
  insert into some_encoded_field_table using 66 x, 'bogus' y;

  declare C cursor for select * from some_encoded_field_table;
  fetch C;
  out C;
end;

BEGIN_TEST(read_partially_vault_cursor)
 declare C cursor fetch from call load_some_encoded_field();

 EXPECT(C.x IS 66);
 EXPECT(C.y IS 'bogus');
END_TEST(read_partially_vault_cursor)

create procedure load_all_types_table()
begin
  create table all_types_table(
    b0 bool @sensitive,
    i0 integer @sensitive,
    l0 long @sensitive,
    d0 real @sensitive,
    s0 text @sensitive,
    bl0 blob @sensitive,

    b1 bool not null,
    i1 integer not null,
    l1 long not null,
    d1 real not null,
    s1 text not null,
    bl1 blob not null
  );

  -- all nullables null
  insert into all_types_table(bl1) values(cast("bl1_0" as blob)) @dummy_seed(0);

  -- all nullables not null
  insert into all_types_table(bl0, bl1) values(cast("bl0_1" as blob),  cast("bl1_1" as blob)) @dummy_seed(1) @dummy_nullables;
  select * from all_types_table;
end;

-- this proc will make the tables and also this serves as the table declarations
create procedure init_temp_tables()
begin
  create temp table temp_table_one(id integer not null @sensitive);
  create temp table temp_table_two(id integer not null);
  create temp table temp_table_three(id integer not null);

  insert into temp_table_one values(1);
  insert into temp_table_two values(2);
  insert into temp_table_three values(3);
end;

-- The run test client verifies that we can call this proc twice
-- having read the rowset out of it and it still succeeds because
-- the tables are dropped. Note simply calling the proc from CQL
-- will not do the job -- you have to use the result set helper
-- to get the auto-cleanup.  If you are using the statement
-- as with a direct CQL call, you are out of luck
@attribute(cql:autodrop=(temp_table_one, temp_table_two, temp_table_three))
create procedure read_three_tables_and_autodrop()
begin
  call init_temp_tables();

  select * from temp_table_one
  union all
  select * from temp_table_two
  union all
  select * from temp_table_three;
end;

-- This helper proc will be called by the client producing its one-row result
-- it has no DB pointer and that exercises and important case in the autodrop logic
-- where info.db is NULL.  There can be no autodrop tables here.
create procedure simple_cursor_proc()
begin
  declare C cursor like temp_table_one;
  fetch C (id) from values(1);
  out c;
end;

-- This is a simple proc we will use to create a result set that is a series of integers.
-- Below we will read and verify these results.

-- this table will never exist
create table dummy_table(id integer);

create proc some_integers(start integer not null, stop integer not null)
begin
  declare C cursor like select 1 v, 2 vsq, "xx" junk;
  declare i integer not null;
  set i := start;
  while (i < stop)
  begin
    fetch C(v, vsq, junk) from values (i, i*i, printf("%d", i));
    out union C;
    set i := i + 1;
  end;

  -- if the start was -1 then force an error, this ensures full cleanup
  -- do this after we have produced rows to make it hard
  if start == -1 then
    drop table dummy_table;
  end if;
end;

-- we need this helper to get a rowset out with type "object", all it does is call the above proc
-- we just need the cast that it does really, but there's no way to code that cast in CQL.

declare proc some_integers_fetch(out rs object not null, start integer not null, stop integer not null) using transaction;

-- these are the helper functions we will be using to read the rowset, they are defined and registered elsewhere
-- See the "call cql_init_extensions();" above for registration.

declare select function rscount(rs long) long;
declare select function rscol(rs long, row integer not null, col integer not null) long;

-- This test is is going to create a rowset using a stored proc, then
-- using the helper proc some_integers_fetch() get access to the result set pointer
-- rather than the sqlite statement.  Then it iterates over the result set as though
-- that result set were a virtual table.  The point of all of this is to test
-- the virtual-table-like construct that we have created and in so doing
-- test the runtime binding facilities needed by ptr(x)

BEGIN_TEST(rowset_reading)
  declare start, stop, cur integer not null;
  set start := 10;
  set stop := 20;
  declare rs object not null;
  call some_integers_fetch(rs, start, stop);

  -- use a nullable version too to exercise both kinds of binding
  declare rs1 object;
  set rs1 := rs;

  declare C cursor for
    with recursive
    C(i) as (select 0 i union all select i+1 i from C limit rscount(ptr(rs))),
    V(v,vsq) as (select rscol(ptr(rs), C.i, 0), rscol(ptr(rs1), C.i, 1) from C)
    select * from V;

  set cur := start;
  loop fetch C
  begin
    EXPECT(C.v == cur);
    EXPECT(C.v * C.v == C.vsq);
    set cur := cur + 1;
  end;

END_TEST(rowset_reading)

BEGIN_TEST(rowset_reading_language_support)
  declare cur integer not null;
  set cur := 7;
  declare C cursor for call some_integers(7, 12);
  loop fetch C
  begin
    EXPECT(C.v == cur);
    EXPECT(c.vsq == cur * cur);
    set cur := cur + 1;
  end;
END_TEST(rowset_reading_language_support)

create procedure all_types_union()
begin
  declare C cursor like all_types_table;

  -- all nullables null
  fetch C(bl1) from values(blob_from_string("bl1_0")) @dummy_seed(0);
  out union C;

  -- all nullables not null
  fetch C(bl0, bl1) from values(blob_from_string("bl0_1"), blob_from_string("bl1_1")) @dummy_seed(1) @dummy_nullables;
  out union C;
end;

BEGIN_TEST(read_all_types_rowset)
  declare C cursor for call all_types_union();
  fetch C;
  EXPECT(C);

  EXPECT(C.b0 IS NULL);
  EXPECT(C.i0 IS NULL);
  EXPECT(C.l0 IS NULL);
  EXPECT(C.d0 IS NULL);
  EXPECT(C.s0 IS NULL);
  EXPECT(C.bl0 IS NULL);
  EXPECT(C.b1 IS 0);
  EXPECT(C.i1 IS 0);
  EXPECT(C.l1 IS 0);
  EXPECT(C.d1 IS 0);
  EXPECT(C.s1 == "s1_0");
  EXPECT(C.bl1 == blob_from_string("bl1_0"));

  fetch C;
  EXPECT(C);

  EXPECT(C.b0 IS 1);
  EXPECT(C.i0 IS 1);
  EXPECT(C.l0 IS 1);
  EXPECT(C.d0 IS 1);
  EXPECT(C.s0 IS "s0_1");
  EXPECT(C.bl0 IS blob_from_string("bl0_1"));
  EXPECT(C.b1 IS 1);
  EXPECT(C.i1 IS 1);
  EXPECT(C.l1 IS 1);
  EXPECT(C.d1 IS 1);
  EXPECT(C.s1 == "s1_1");
  EXPECT(C.bl1 IS blob_from_string("bl1_1"));

  fetch C;
  EXPECT(not C);
END_TEST(read_all_types_rowset)

-- make something to fake out CQL so we can call the real fetcher
DECLARE PROC load_all_types_table_shim() out union
   (b0 BOOL, i0 INTEGER, l0 LONG_INT, d0 REAL, s0 TEXT, bl0 BLOB,
    b1 BOOL NOT NULL, i1 INTEGER NOT NULL, l1 LONG_INT NOT NULL, d1 REAL NOT NULL, s1 TEXT NOT NULL, bl1 BLOB NOT NULL)
    using transaction;

-- we're going to redirect this declared out union function to be the
-- automaticlaly generated fetcher above.  That's a normal result set
-- generated in the normal way.  And boom just like that we're reading a
-- result set rather than the statement

@echo c,"#define load_all_types_table_shim_fetch_results load_all_types_table_fetch_results\n";
@echo c,"#define load_all_types_table_shim_result_set_ref load_all_types_table_result_set_ref\n";

BEGIN_TEST(read_all_types_auto_fetcher)
  declare C cursor for call load_all_types_table_shim();
  fetch C;
  EXPECT(C);

  EXPECT(C.b0 IS NULL);
  EXPECT(C.i0 IS NULL);
  EXPECT(C.l0 IS NULL);
  EXPECT(C.d0 IS NULL);
  EXPECT(C.s0 IS NULL);
  EXPECT(C.bl0 IS NULL);
  EXPECT(C.b1 IS 0);
  EXPECT(C.i1 IS 0);
  EXPECT(C.l1 IS 0);
  EXPECT(C.d1 IS 0);
  EXPECT(C.s1 == "s1_0");
  EXPECT(string_from_blob(C.bl1) == "bl1_0");

  fetch C;
  EXPECT(C);

  EXPECT(C.b0 IS 1);
  EXPECT(C.i0 IS 1);
  EXPECT(C.l0 IS 1);
  EXPECT(C.d0 IS 1);
  EXPECT(C.s0 IS "s0_1");
  EXPECT(string_from_blob(C.bl0) == "bl0_1");
  EXPECT(C.b1 IS 1);
  EXPECT(C.i1 IS 1);
  EXPECT(C.l1 IS 1);
  EXPECT(C.d1 IS 1);
  EXPECT(C.s1 == "s1_1");
  EXPECT(string_from_blob(C.bl1) == "bl1_1");
  EXPECT(cql_get_blob_size(C.bl1) == 5);

  fetch C;
  EXPECT(not C);
END_TEST(read_all_types_auto_fetcher)

BEGIN_TEST(rowset_via_union_failed)
  declare ok_after_all bool not null;
  declare start, stop, cur integer not null;

  set start := -1;
  set stop := 1;
  declare rs object not null;
  begin try
    call some_integers_fetch(rs, start, stop);
  end try;
  begin catch
    set ok_after_all := 1;
  end catch;

  -- throw happened and we're not gonna leak
  EXPECT(ok_after_all);

END_TEST(rowset_via_union_failed)

BEGIN_TEST(boxing_cursors)
  declare i integer not null;

  set i := 0;
  while i < 5
  begin
    declare C cursor for
      with data(x,y) as (values (1,2), (3,4), (5,6))
      select * from data;

    declare box object<C cursor>;
    set box from cursor C;
    declare D cursor for box;

    fetch C;
    EXPECT(C.x == 1);
    EXPECT(C.y == 2);

    fetch D;
    -- C did not change
    EXPECT(C.x == 1);
    EXPECT(C.y == 2);
    EXPECT(D.x == 3);
    EXPECT(D.y == 4);

    fetch C;
    -- C advanced D values held
    EXPECT(C.x == 5);
    EXPECT(C.y == 6);
    EXPECT(D.x == 3);
    EXPECT(D.y == 4);

    set i := i + 1;
  end;
END_TEST(boxing_cursors)

create proc a_few_rows()
begin
  with data(x,y) as (values (1,2), (3,4), (5,6))
  select * from data;
end;

BEGIN_TEST(boxing_from_call)
  declare i integer not null;

  set i := 0;
  while i < 5
  begin
    declare C cursor for call a_few_rows();

    declare box object<C cursor>;
    set box from cursor C;
    declare D cursor for box;

    fetch C;
    EXPECT(C.x == 1);
    EXPECT(C.y == 2);

    fetch D;
    -- C did not change
    EXPECT(C.x == 1);
    EXPECT(C.y == 2);
    EXPECT(D.x == 3);
    EXPECT(D.y == 4);

    fetch C;
    -- C advanced D values held
    EXPECT(C.x == 5);
    EXPECT(C.y == 6);
    EXPECT(D.x == 3);
    EXPECT(D.y == 4);

    set i := i + 1;
  end;
END_TEST(boxing_from_call)

BEGIN_TEST(numeric_casts)
  declare b bool not null;
  declare i int not null;
  declare l long not null;
  declare r real not null;
  declare b0 bool;
  declare i0 int;
  declare l0 long;
  declare r0 real;

  -- force conversion (not null)
  set b := cast(7.5 as bool);
  EXPECT(b == 1);
  set i := cast(1.9 as integer);
  EXPECT(i == 1);
  set l := cast(12.9 as long);
  EXPECT(l == 12);
  set r := cast(12 as real);
  EXPECT(r == 12.0);

  -- null cases
  EXPECT(cast(b0 as bool) is null);
  EXPECT(cast(b0 as int) is null);
  EXPECT(cast(b0 as long) is null);
  EXPECT(cast(b0 as real) is null);

  -- force conversion (nullable)
  declare x real;
  set x := 7.5;
  set b0 := cast(x as bool);
  EXPECT(b0 == 1);
  set x := 1.9;
  set i0 := cast(x as integer);
  EXPECT(i0 == 1);
  set x := 12.9;
  set l0 := cast(x as long);
  EXPECT(l0 == 12);
  set x := 12.0;
  set r0 := cast(x as real);
  EXPECT(r0 == 12.0);
  set l := 12;
  set r0 := cast(l as real);
  EXPECT(r0 == 12.0);

END_TEST(numeric_casts)

create proc dummy(seed integer not null, i integer not null, r real not null, b bool not null)
begin
   EXPECT(seed == i);
   EXPECT(seed == r);
   EXPECT(not seed == not b);
end;

BEGIN_TEST(cursor_args)
  declare args cursor like dummy arguments;
  fetch args() from values() @dummy_seed(12);
  call dummy(from args);
END_TEST(cursor_args)

DECLARE PROCEDURE cql_exec_internal(sql TEXT NOT NULL) USING TRANSACTION;
create table xyzzy(id integer, name text, data blob);

BEGIN_TEST(exec_internal)
  call cql_exec_internal("create table xyzzy(id integer, name text, data blob);");
  declare bl1 blob;
  set bl1 := blob_from_string('z');
  declare bl2 blob;
  set bl2 := blob_from_string('w');
  insert into xyzzy using 1 id, 'x' name, bl1 data;
  insert into xyzzy using 2 id, 'y' name, bl2 data;
  declare C cursor for select * from xyzzy;
  declare D cursor like C;
  fetch C;
  fetch D using 1 id, 'x' name, bl1 data;
  EXPECT(cql_cursor_diff_val(C,D) is null);
  fetch C;
  fetch D using 2 id, 'y' name, bl2 data;
  EXPECT(cql_cursor_diff_val(C,D) is null);
END_TEST(exec_internal)

BEGIN_TEST(const_folding)
  EXPECT(const(1 + 1) == 2);
  EXPECT(const(1.0 + 1) == 2.0);
  EXPECT(const(1 + 1L) == 2L);
  EXPECT(const(1 + (1==1) ) == 2);
  EXPECT(const(1.0 + 1L) == 2.0);
  EXPECT(const(1.0 + (1 == 1)) == 2.0);
  EXPECT(const((1==1) + 1L) == 2L);

  EXPECT(2 == const(1 + 1));
  EXPECT(2.0 == const(1.0 + 1));
  EXPECT(2L == const(1 + 1L));
  EXPECT(2 == const(1 + (1==1) ));

  EXPECT(const(1 - 1) == 0);
  EXPECT(const(1.0 - 1) == 0.0);
  EXPECT(const(1 - 1L) == 0L);
  EXPECT(const(1 - (1==1) ) == 0);

  EXPECT(const(3 * 2) == 6);
  EXPECT(const(3.0 * 2) == 6.0);
  EXPECT(const(3 * 2L) == 6L);
  EXPECT(const(3 * (1==1) ) == 3);

  EXPECT(const(3 / 1) == 3);
  EXPECT(const(3.0 / 1) == 3.0);
  EXPECT(const(3 / 1L) == 3L);
  EXPECT(const(3 / (1==1) ) == 3);

  EXPECT(const(3 % 1) == 0);
  EXPECT(const(3 % 1L) == 0L);
  EXPECT(const(3 % (1==1) ) == 0);

  EXPECT(const(8 | 1) == 9);
  EXPECT(const(8 | 1L) == 9L);
  EXPECT(const(8 | (1==1) ) == 9);

  EXPECT(const(7 & 4) == 4);
  EXPECT(const(7 & 4L) == 4L);
  EXPECT(const(7 & (1==1) ) == 1);

  EXPECT(const(16 << 1) == 32);
  EXPECT(const(16 << 1L) == 32L);
  EXPECT(const(16 << (1==1) ) == 32);

  EXPECT(const(16 >> 1) == 8);
  EXPECT(const(16 >> 1L) == 8L);
  EXPECT(const(16 >> (1==1) ) == 8);

  EXPECT(const(NULL) is null);

  EXPECT(const( 1 or 1/0) == 1);
  EXPECT(const( 0 or null) is null);
  EXPECT(const( 0 or 0) == 0);
  EXPECT(const( 0 or 1) == 1);
  EXPECT(const( null or null) is null);
  EXPECT(const( null or 0) is null);
  EXPECT(const( null or 1) is 1);

  EXPECT(const( 0 and 1/0) == 0);
  EXPECT(const( 1 and null) is null);
  EXPECT(const( 1 and 0) == 0);
  EXPECT(const( 1 and 1) == 1);
  EXPECT(const( null and null) is null);
  EXPECT(const( null and 0) == 0);
  EXPECT(const( null and 1) is null);

  EXPECT(const(3 == 3));
  EXPECT(const(3 == 3.0));
  EXPECT(const(3 == 3L));
  EXPECT(const((0 == 0) == (1 == 1)));

  EXPECT(const(4 != 3));
  EXPECT(const(4 != 3.0));
  EXPECT(const(4 != 3L));
  EXPECT(const((1 == 0) != (1 == 1)));

  EXPECT(const(4 >= 3));
  EXPECT(const(4 >= 3.0));
  EXPECT(const(4 >= 3L));
  EXPECT(const((1 == 1) >= (1 == 0)));

  EXPECT(const(3 >= 3));
  EXPECT(const(3 >= 3.0));
  EXPECT(const(3 >= 3L));
  EXPECT(const((1 == 1) >= (1 == 1)));

  EXPECT(const(4 > 3));
  EXPECT(const(4 > 3.0));
  EXPECT(const(4 > 3L));
  EXPECT(const((1 == 1) > (1 == 0)));

  EXPECT(const(2 <= 3));
  EXPECT(const(2 <= 3.0));
  EXPECT(const(2 <= 3L));
  EXPECT(const((1 == 0) <= (1 == 1)));

  EXPECT(const(3 <= 3));
  EXPECT(const(3 <= 3.0));
  EXPECT(const(3 <= 3L));
  EXPECT(const((1 == 1) <= (1 == 1)));

  EXPECT(const(2 < 3));
  EXPECT(const(2 < 3.0));
  EXPECT(const(2 < 3L));
  EXPECT(const((1 == 0) < (1 == 1)));

  EXPECT((NULL == NULL) is NULL);
  EXPECT((NULL + NULL) is NULL);
  EXPECT((NULL - NULL) is NULL);
  EXPECT((NULL * NULL) is NULL);
  EXPECT((NULL / NULL) is NULL);
  EXPECT((NULL % NULL) is NULL);
  EXPECT((NULL | NULL) is NULL);
  EXPECT((NULL & NULL) is NULL);
  EXPECT((NULL << NULL) is NULL);
  EXPECT((NULL >> NULL) is NULL);

  EXPECT(const(NULL == NULL) is NULL);
  EXPECT(const(NULL + NULL) is NULL);
  EXPECT(const(NULL - NULL) is NULL);
  EXPECT(const(NULL * NULL) is NULL);
  EXPECT(const(NULL / NULL) is NULL);
  EXPECT(const(NULL % NULL) is NULL);
  EXPECT(const(NULL | NULL) is NULL);
  EXPECT(const(NULL & NULL) is NULL);
  EXPECT(const(NULL << NULL) is NULL);
  EXPECT(const(NULL >> NULL) is NULL);

  EXPECT(const((NULL == NULL) is NULL));
  EXPECT(const((NULL + NULL) is NULL));
  EXPECT(const((NULL - NULL) is NULL));
  EXPECT(const((NULL * NULL) is NULL));
  EXPECT(const((NULL / NULL) is NULL));
  EXPECT(const((NULL % NULL) is NULL));
  EXPECT(const((NULL | NULL) is NULL));
  EXPECT(const((NULL & NULL) is NULL));
  EXPECT(const((NULL << NULL) is NULL));
  EXPECT(const((NULL >> NULL) is NULL));

  EXPECT(const(NULL IS NOT NULL) == 0);
  EXPECT(const(NULL IS NOT 1));
  EXPECT(const(1 IS NOT NULL));

  EXPECT(const(1 IS 1));
  EXPECT(const(1L IS 1L));
  EXPECT(const(1.0 IS 1.0));
  EXPECT(const((1==1) is (2==2)));

  EXPECT(const(cast(3.2 as integer) == 3));
  EXPECT(const(cast(3.2 as long_int) == 3L));
  EXPECT(const(cast(3.2 as bool) == 1));
  EXPECT(const(cast(0.0 as bool) == 0));
  EXPECT(const(cast(null+0 as bool) is null));
  EXPECT(const(cast(3L as real) == 3.0));
  EXPECT(const(cast(3L as integer) == 3));
  EXPECT(const(cast(3L as bool) == 1));
  EXPECT(const(cast(0L as bool) == 0));

  EXPECT(const(not 0) == 1);
  EXPECT(const(not 1) == 0);
  EXPECT(const(not 2) == 0);
  EXPECT(const(not 0L) == 1);
  EXPECT(const(not 1L) == 0);
  EXPECT(const(not 2L) == 0);
  EXPECT(const(not 2.0) == 0);
  EXPECT(const(not 0.0) == 1);
  EXPECT(const(not not 2) == 1);
  EXPECT(const(not NULL) is NULL);

  EXPECT(const(~0) == -1);
  EXPECT(const(~0L) == -1L);
  EXPECT(const(~ ~0L) == 0L);
  EXPECT(const(~NULL) is NULL);
  EXPECT(const(~(0==0)) == -2);
  EXPECT(const(~(0==1)) == -1);

  EXPECT(const(-1) == -1);
  EXPECT(const(-2) == -2);
  EXPECT(const(-1.0) == -1.0);
  EXPECT(const(-2.0) == -2.0);
  EXPECT(const((0 + -2)) == -2);
  EXPECT(const(-(1 + 1)) == -2);
  EXPECT(const(-1L) == -1L);
  EXPECT(const(- -1L) == 1L);
  EXPECT(const(-NULL) is NULL);
  EXPECT(const(-(0==0)) == -1);
  EXPECT(const(-(0==1)) == 0);

  -- IIF gets rewritten to case/when so we use that here for convenience
  EXPECT(const(iif(1, 3, 5)) == 3);
  EXPECT(const(iif(0, 3, 5)) == 5);
  EXPECT(const(iif(1L, 3, 5)) == 3);
  EXPECT(const(iif(0L, 3, 5)) == 5);
  EXPECT(const(iif(1.0, 3, 5)) == 3);
  EXPECT(const(iif(0.0, 3, 5)) == 5);
  EXPECT(const(iif((1==1), 3, 5)) == 3);
  EXPECT(const(iif((1==0), 3, 5)) == 5);

  EXPECT(const(case 1 when 2 then 20 else 10 end) == 10);
  EXPECT(const(case 2 when 2 then 20 else 10 end) == 20);
  EXPECT(const(case 2 when 1 then 10 when 2 then 20 else 40 end) == 20);
  EXPECT(const(case 1 when 1 then 10 when 2 then 20 else 40 end) == 10);
  EXPECT(const(case 5 when 1 then 10 when 2 then 20 else 40 end) == 40);
  EXPECT(const(case null when 1 then 10 when 2 then 20 else 40 end) == 40);

  EXPECT(const(case 1.0 when 2 then 20 else 10 end) == 10);
  EXPECT(const(case 2.0 when 2 then 20 else 10 end) == 20);
  EXPECT(const(case 2.0 when 1 then 10 when 2 then 20 else 40 end) == 20);
  EXPECT(const(case 1.0 when 1 then 10 when 2 then 20 else 40 end) == 10);
  EXPECT(const(case 5.0 when 1 then 10 when 2 then 20 else 40 end) == 40);

  EXPECT(const(case 1L when 2 then 20 else 10 end) == 10);
  EXPECT(const(case 2L when 2 then 20 else 10 end) == 20);
  EXPECT(const(case 2L when 1 then 10 when 2 then 20 else 40 end) == 20);
  EXPECT(const(case 1L when 1 then 10 when 2 then 20 else 40 end) == 10);
  EXPECT(const(case 5L when 1 then 10 when 2 then 20 else 40 end) == 40);

  EXPECT(const(case (1==1) when (1==1) then 10 else 20 end) == 10);
  EXPECT(const(case (1==0) when (1==1) then 10 else 20 end) == 20);
  EXPECT(const(case (1==1) when (0==1) then 10 else 20 end) == 20);
  EXPECT(const(case (1==0) when (0==1) then 10 else 20 end) == 10);
  EXPECT(const(case (1==0) when null then 10 else 20 end) == 20);
  EXPECT(const(case (1==0) when null then 10 end ) is null);

  EXPECT(const(case 5L when 1 then 10 when 2 then 20 end) is NULL);
  EXPECT(const(case when NULL then 1 else 2 end) == 2);

  EXPECT(const(0x10) == 16);
  EXPECT(const(0x10 + 0xf) == 31);
  EXPECT(const(0x100100100) == 0x100100100);
  EXPECT(const(0x100100100L) == 0x100100100);
  EXPECT(const(0x100100100) == 0x100100100L);
  EXPECT(const(0x100100100L) == 0x100100100L);

END_TEST(const_folding)

BEGIN_TEST(long_literals)
  declare x long not null;
  declare z long;

  set x := 1L;
  EXPECT(x == 1);

  set x := 10000000000;
  EXPECT(x = 10000000000);
  EXPECT(x != const(cast(10000000000L as integer)));
  EXPECT(x > 0x7fffffff);

  set x := 10000000000L;
  EXPECT(x = 10000000000L);
  EXPECT(x != const(cast(10000000000L as integer)));
  EXPECT(x > 0x7fffffff);

  set x := 0x1000000000L;
  EXPECT(x = 0x1000000000L);
  EXPECT(x != const(cast(0x10000000000L as integer)));
  EXPECT(x > 0x7fffffff);

  set x := 0x1000000000;
  EXPECT(x = 0x1000000000L);
  EXPECT(x != const(cast(0x10000000000L as integer)));
  EXPECT(x > 0x7fffffff);

  set x := const(0x1000000000);
  EXPECT(x = 0x1000000000L);
  EXPECT(x != const(cast(0x1000000000L as integer)));
  EXPECT(x > 0x7fffffff);

  set x := 1000L * 1000 * 1000 * 1000;
  EXPECT(x = 1000000000000);
  EXPECT(x != const(cast(1000000000000 as integer)));
  set x := const(1000L * 1000 * 1000 * 1000);

  set z := 1L;
  EXPECT(z == 1);

  set z := 10000000000;
  EXPECT(z = 10000000000);
  EXPECT(z != const(cast(10000000000L as integer)));
  EXPECT(z > 0x7fffffff);

  set z := 10000000000L;
  EXPECT(z = 10000000000L);
  EXPECT(z != const(cast(10000000000L as integer)));
  EXPECT(z > 0x7fffffff);

  set z := 0x1000000000L;
  EXPECT(z = 0x1000000000L);
  EXPECT(z != const(cast(0x1000000000L as integer)));
  EXPECT(z > 0x7fffffff);

  set z := 0x1000000000;
  EXPECT(z = 0x1000000000L);
  EXPECT(z != const(cast(0x1000000000L as integer)));
  EXPECT(z > 0x7fffffff);

  set z := const(0x1000000000);
  EXPECT(z = 0x1000000000L);
  EXPECT(z != const(cast(0x1000000000L as integer)));
  EXPECT(z > 0x7fffffff);

  set z := 1000L * 1000 * 1000 * 1000;
  EXPECT(z = 1000000000000);
  EXPECT(z != const(cast(1000000000000 as integer)));
  set z := const(1000L * 1000 * 1000 * 1000);

END_TEST(long_literals)

create proc no_statement_really(x integer)
begin
  if x then
    select 1 x;
  end if;
end;

BEGIN_TEST(null_statement)
  declare C cursor for call no_statement_really(0);
  declare x integer;
  set x := 0;
  loop fetch C
  begin
     set x := x + 1;
  end;
  EXPECT(x == 0);
END_TEST(null_statement)

BEGIN_TEST(if_nothing_forms)
  create table tdata (
    id integer,
    v integer,
    t text);

  declare t1 text;
  set t1 := (select t from tdata if nothing "nothing");
  EXPECT(t1 == "nothing");

  declare v1 integer;
  set v1 := (select v from tdata if nothing -1);
  EXPECT(v1 == -1);

  insert into tdata values(1, 2, null);
  set t1 := (select t from tdata if nothing "nothing");
  EXPECT(t1 is null);

  set v1 := (select v from tdata if nothing -1);
  EXPECT(v1 == 2);

  set t1 := (select t from tdata if nothing or null "still nothing");
  EXPECT(t1 == "still nothing");

  insert into tdata values(2, null, "x");
  set v1 := (select v from tdata where id == 2 if nothing or null -1);
  EXPECT(v1 == -1);

END_TEST(if_nothing_forms)

create proc simple_select()
begin
  select 1 x;
end;

BEGIN_TEST(call_in_loop)
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call simple_select();
     fetch C;
     EXPECT(C.x == 1);
  end;
END_TEST(call_in_loop)

BEGIN_TEST(call_in_loop_boxed)
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
     EXPECT(D.x == 1);
  end;
END_TEST(call_in_loop_boxed)

create proc out_union_helper()
begin
  declare C cursor like select 1 x;
  fetch C using 1 x;
  out union C;
end;

BEGIN_TEST(call_out_union_in_loop)
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call out_union_helper();
     fetch C;
     EXPECT(C.x == 1);
  end;
END_TEST(call_out_union_in_loop)

create table simple_rc_table(id integer, foo text);
create proc simple_insert()
begin
  insert into simple_rc_table(id, foo) values(1, "foo");
end;

create proc select_if_nothing(id_ integer not null)
begin
  declare bar text;
  set bar := (select foo from simple_rc_table where id == id_ if nothing "bar");
end;

create proc select_if_nothing_throw(id_ integer not null)
begin
  declare bar text;
  set bar := (select foo from simple_rc_table where id == id_ if nothing throw);
end;

BEGIN_TEST(rc_simple_select)
  declare C cursor for call simple_select();
  EXPECT(@rc == 0);
END_TEST(rc_simple_select)

BEGIN_TEST(rc_simple_insert_and_select)
  create table simple_rc_table(id integer, foo text);

  call simple_insert();
  EXPECT(@rc == 0);

  call select_if_nothing(1);
  EXPECT(@rc == 0);

  call select_if_nothing(2);
  EXPECT(@rc == 0);

  begin try
    call select_if_nothing_throw(2);
  end try;
  begin catch
    EXPECT(@rc != 0);
  end catch;
END_TEST(rc_simple_insert_and_select)

END_SUITE()

-- manually force tracing on by redefining the macros
@echo c,"#undef cql_error_trace\n";
@echo c,"#define cql_error_trace() run_test_trace_callback(_PROC_, __FILE__, __LINE__)\n";
@echo c,"void run_test_trace_callback(const char *proc, const char *file, int32_t line);\n";

-- this table will never actually be created, only declared
-- hence it is a good source of db errors
create table does_not_exist(id integer);

create proc fails_because_bogus_table()
begin
   begin try
     declare D cursor for select * from does_not_exist;
   end try;
   begin catch
     -- Without tracing this failure code can be seen, the cursor D
     -- will be finalized as part of cleanup and THAT success will be
     -- the sqlite3_errmsg() result.  Tracing lets you see the error as it happens.
     drop table if exists does_not_exist;
     -- now we save the code
     throw;
   end catch;
end;

@echo c,"#undef cql_error_trace\n";
@echo c,"#define cql_error_trace()\n";

@emit_enums;

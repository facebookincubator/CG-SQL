/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare tests integer not null;
declare tests_passed integer not null;
declare fails integer not null;
declare expectations integer not null;
declare function get_outstanding_refs() integer not null;
declare start_refs integer not null;
declare end_refs integer not null;
declare proc print no check;
declare proc exit no check;
create procedure errcheck(passed bool @sensitive, message text, line integer not null)
begin
  set expectations := expectations + 1;
  if not coalesce(passed, 0) then
    call print(printf("test: %s: FAIL on line %d\n", message, line));
    set fails := fails + 1;
  end if;
end;

create procedure end_suite()
begin
  call print(printf("%d tests executed. %d passed, %d failed.  %d expectations failed of %d.\n",
    tests, tests_passed, tests - tests_passed, fails, expectations));
  call exit(fails);
end;

-- use this for both normal eval and SQLite eval
declare function get_blob_byte(b blob not null, i integer not null) integer not null;
declare function get_blob_size(b blob not null) integer not null;
declare function create_truncated_blob(b blob not null, truncated_size integer not null) create blob not null;



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

create procedure test_arithmetic() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck((1 + 2) * 3 == 9, "(1 + 2) * 3 == 9", 44); call errcheck((select (1 + 2) * 3 == 9), "(select (1 + 2) * 3 == 9)", 44);
  call errcheck(1 + 2 * 3 == 7, "1 + 2 * 3 == 7", 45); call errcheck((select 1 + 2 * 3 == 7), "(select 1 + 2 * 3 == 7)", 45);
  call errcheck(6 / 3 == 2, "6 / 3 == 2", 46); call errcheck((select 6 / 3 == 2), "(select 6 / 3 == 2)", 46);
  call errcheck(7 - 5 == 2, "7 - 5 == 2", 47); call errcheck((select 7 - 5 == 2), "(select 7 - 5 == 2)", 47);
  call errcheck(6 % 5 == 1, "6 % 5 == 1", 48); call errcheck((select 6 % 5 == 1), "(select 6 % 5 == 1)", 48);
  call errcheck(5 / 2.5 == 2, "5 / 2.5 == 2", 49); call errcheck((select 5 / 2.5 == 2), "(select 5 / 2.5 == 2)", 49);
  call errcheck(-(1+3) == -4, "-(1+3) == -4", 50); call errcheck((select -(1+3) == -4), "(select -(1+3) == -4)", 50);
  call errcheck(-1+3 == 2, "-1+3 == 2", 51); call errcheck((select -1+3 == 2), "(select -1+3 == 2)", 51);
  call errcheck(1+-3 == -2, "1+-3 == -2", 52); call errcheck((select 1+-3 == -2), "(select 1+-3 == -2)", 52);
  call errcheck(longs.neg == -1, "longs.neg == -1", 53); call errcheck((select longs.neg == -1), "(select longs.neg == -1)", 53);
  call errcheck(-longs.neg == 1, "-longs.neg == 1", 54); call errcheck((select -longs.neg == 1), "(select -longs.neg == 1)", 54);
  call errcheck(- -longs.neg == -1, "- -longs.neg == -1", 55); call errcheck((select - -longs.neg == -1), "(select - -longs.neg == -1)", 55);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "arithmetic")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "arithmetic")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_arithmetic(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "arithmetic", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_logical_operations() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck((null and 0) == 0, "(null and 0) == 0", 88); call errcheck((select (null and 0) == 0), "(select (null and 0) == 0)", 88);
  call errcheck((null and 0) = 0, "(null and 0) = 0", 89); call errcheck((select (null and 0) = 0), "(select (null and 0) = 0)", 89);
  call errcheck((0 and null) == 0, "(0 and null) == 0", 90); call errcheck((select (0 and null) == 0), "(select (0 and null) == 0)", 90);
  call errcheck((1 and null) is null, "(1 and null) is null", 91); call errcheck((select (1 and null) is null), "(select (1 and null) is null)", 91);
  call errcheck((null and 1) is null, "(null and 1) is null", 92); call errcheck((select (null and 1) is null), "(select (null and 1) is null)", 92);
  call errcheck((null or 1) == 1, "(null or 1) == 1", 93); call errcheck((select (null or 1) == 1), "(select (null or 1) == 1)", 93);
  call errcheck((1 or null) == 1, "(1 or null) == 1", 94); call errcheck((select (1 or null) == 1), "(select (1 or null) == 1)", 94);
  call errcheck((0 or null) is null, "(0 or null) is null", 95); call errcheck((select (0 or null) is null), "(select (0 or null) is null)", 95);
  call errcheck((null or 0) is null, "(null or 0) is null", 96); call errcheck((select (null or 0) is null), "(select (null or 0) is null)", 96);
  call errcheck((0 or 1) and (1 or 0), "(0 or 1) and (1 or 0)", 97); call errcheck((select (0 or 1) and (1 or 0)), "(select (0 or 1) and (1 or 0))", 97);
  call errcheck(NOT (1+2) == 0, "NOT (1+2) == 0", 98); call errcheck((select NOT (1+2) == 0), "(select NOT (1+2) == 0)", 98);
  call errcheck((NOT 1)+2 == 2, "(NOT 1)+2 == 2", 99); call errcheck((select (NOT 1)+2 == 2), "(select (NOT 1)+2 == 2)", 99);

  call errcheck((side_effect_0() and side_effect_0()) == 0, "(side_effect_0() and side_effect_0()) == 0", 101);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 102);
  call reset_counts();

  call errcheck((side_effect_0() and side_effect_1()) == 0, "(side_effect_0() and side_effect_1()) == 0", 105);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 106);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 107);
  call reset_counts();

  call errcheck((side_effect_0() and side_effect_null()) == 0, "(side_effect_0() and side_effect_null()) == 0", 110);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 111);
  call errcheck(side_effect_null_count == 0, "side_effect_null_count == 0", 112);
  call reset_counts();

  call errcheck((side_effect_1() and side_effect_0()) == 0, "(side_effect_1() and side_effect_0()) == 0", 115);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 116);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 117);
  call reset_counts();

  call errcheck((side_effect_1() and side_effect_1()) == 1, "(side_effect_1() and side_effect_1()) == 1", 120);
  call errcheck(side_effect_1_count == 2, "side_effect_1_count == 2", 121);
  call reset_counts();

  call errcheck((side_effect_1() and side_effect_null()) is null, "(side_effect_1() and side_effect_null()) is null", 124);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 125);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 126);
  call reset_counts();

  call errcheck((side_effect_null() and side_effect_0()) == 0, "(side_effect_null() and side_effect_0()) == 0", 129);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 130);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 131);
  call reset_counts();

  call errcheck((side_effect_null() and side_effect_1()) is null, "(side_effect_null() and side_effect_1()) is null", 134);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 135);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 136);
  call reset_counts();

  call errcheck((side_effect_null() and side_effect_null()) is null, "(side_effect_null() and side_effect_null()) is null", 139);
  call errcheck(side_effect_null_count == 2, "side_effect_null_count == 2", 140);
  call reset_counts();

  call errcheck((side_effect_0() or side_effect_0()) == 0, "(side_effect_0() or side_effect_0()) == 0", 143);
  call errcheck(side_effect_0_count == 2, "side_effect_0_count == 2", 144);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 145);
  call reset_counts();

  call errcheck((side_effect_0() or side_effect_1()) == 1, "(side_effect_0() or side_effect_1()) == 1", 148);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 149);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 150);
  call reset_counts();

  call errcheck((side_effect_0() or side_effect_null()) is null, "(side_effect_0() or side_effect_null()) is null", 153);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 154);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 155);
  call reset_counts();

  call errcheck((side_effect_1() or side_effect_0()) == 1, "(side_effect_1() or side_effect_0()) == 1", 158);
  call errcheck(side_effect_0_count == 0, "side_effect_0_count == 0", 159);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 160);
  call reset_counts();

  call errcheck((side_effect_1() or side_effect_1()) == 1, "(side_effect_1() or side_effect_1()) == 1", 163);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 164);
  call reset_counts();

  call errcheck((side_effect_1() or side_effect_null()) == 1, "(side_effect_1() or side_effect_null()) == 1", 167);
  call errcheck(side_effect_null_count == 0, "side_effect_null_count == 0", 168);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 169);
  call reset_counts();

  call errcheck((side_effect_null() or side_effect_0()) is null, "(side_effect_null() or side_effect_0()) is null", 172);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 173);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 174);
  call reset_counts();

  call errcheck((side_effect_null() or side_effect_1()) == 1, "(side_effect_null() or side_effect_1()) == 1", 177);
  call errcheck(side_effect_null_count == 1, "side_effect_null_count == 1", 178);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 179);
  call reset_counts();

  call errcheck((side_effect_null() or side_effect_null()) is null, "(side_effect_null() or side_effect_null()) is null", 182);
  call errcheck(side_effect_null_count == 2, "side_effect_null_count == 2", 183);
  call reset_counts();

  -- even though this looks like all non nulls we do not eval side_effect_1
  -- we can't use the simple && form because there is statement output
  -- requred to evaluate the coalesce.

  call errcheck((0 and coalesce(side_effect_1(), 1)) == 0, "(0 and coalesce(side_effect_1(), 1)) == 0", 190);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 191);
  call reset_counts();

  call errcheck((1 and coalesce(side_effect_1(), 1)) == 1, "(1 and coalesce(side_effect_1(), 1)) == 1", 194);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 195);
  call reset_counts();

  call errcheck((1 or coalesce(side_effect_1(), 1)) == 1, "(1 or coalesce(side_effect_1(), 1)) == 1", 198);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 199);
  call reset_counts();

  call errcheck((0 or coalesce(side_effect_1(), 1)) == 1, "(0 or coalesce(side_effect_1(), 1)) == 1", 202);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 203);
  call reset_counts();

  -- this is the same as NOT (0 < 0) rather than (NOT 0) < 0
  -- do not move NOT around or you will break stuff
  -- I have broken this many times now do not change this expectation
  -- it will save your life.
  call errcheck(NOT 0 < 0, "NOT 0 < 0", 210); call errcheck((select NOT 0 < 0), "(select NOT 0 < 0)", 210);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "logical_operations")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "logical_operations")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_logical_operations(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "logical_operations", start_refs, end_refs)); set fails := fails + 1; end if;

declare zero integer not null;
set zero := 0;

declare one integer not null;
set one := 1;

 -- logical and short-circuit verify 1/0 not evaluated
create procedure test_local_operations_early_out() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(not (0 and 1/zero), "not (0 and 1/zero)", 221); call errcheck((select not (0 and 1/zero)), "(select not (0 and 1/zero))", 221);
  call errcheck(1 or 1/zero, "1 or 1/zero", 222); call errcheck((select 1 or 1/zero), "(select 1 or 1/zero)", 222);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "local_operations_early_out")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "local_operations_early_out")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_local_operations_early_out(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "local_operations_early_out", start_refs, end_refs)); set fails := fails + 1; end if;

-- assorted between combinations
create procedure test_between_operations() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(1 between 0 and 2, "1 between 0 and 2", 227); call errcheck((select 1 between 0 and 2), "(select 1 between 0 and 2)", 227);
  call errcheck(not 3 between 0 and 2, "not 3 between 0 and 2", 228); call errcheck((select not 3 between 0 and 2), "(select not 3 between 0 and 2)", 228);
  call errcheck(not (3 between 0 and 2), "not (3 between 0 and 2)", 229); call errcheck((select not (3 between 0 and 2)), "(select not (3 between 0 and 2))", 229);
  call errcheck((null between 0 and 2) is null, "(null between 0 and 2) is null", 230); call errcheck((select (null between 0 and 2) is null), "(select (null between 0 and 2) is null)", 230);
  call errcheck((1 between null and 2) is null, "(1 between null and 2) is null", 231); call errcheck((select (1 between null and 2) is null), "(select (1 between null and 2) is null)", 231);
  call errcheck((1 between 0 and null) is null, "(1 between 0 and null) is null", 232); call errcheck((select (1 between 0 and null) is null), "(select (1 between 0 and null) is null)", 232);

  call errcheck((-1 between side_effect_0() and side_effect_1()) == 0, "(-1 between side_effect_0() and side_effect_1()) == 0", 234);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 235);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 236);
  call reset_counts();

  call errcheck((0 between side_effect_0() and side_effect_1()) == 1, "(0 between side_effect_0() and side_effect_1()) == 1", 239);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 240);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 241);
  call reset_counts();

  call errcheck((2 between side_effect_0() and side_effect_1()) == 0, "(2 between side_effect_0() and side_effect_1()) == 0", 244);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 245);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 246);
  call reset_counts();

  call errcheck((-1 not between side_effect_0() and side_effect_1()) == 1, "(-1 not between side_effect_0() and side_effect_1()) == 1", 249);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 250);
  call errcheck(side_effect_1_count == 0, "side_effect_1_count == 0", 251);
  call reset_counts();

  call errcheck((0 not between side_effect_0() and side_effect_1()) == 0, "(0 not between side_effect_0() and side_effect_1()) == 0", 254);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 255);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 256);
  call reset_counts();

  call errcheck((2 not between side_effect_0() and side_effect_1()) == 1, "(2 not between side_effect_0() and side_effect_1()) == 1", 259);
  call errcheck(side_effect_0_count == 1, "side_effect_0_count == 1", 260);
  call errcheck(side_effect_1_count == 1, "side_effect_1_count == 1", 261);
  call reset_counts();
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "between_operations")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "between_operations")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_between_operations(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "between_operations", start_refs, end_refs)); set fails := fails + 1; end if;

-- assorted not between combinations
create procedure test_not_between_operations() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(3 not between 0 and 2, "3 not between 0 and 2", 267); call errcheck((select 3 not between 0 and 2), "(select 3 not between 0 and 2)", 267);
  call errcheck(not 1 not between 0 and 2, "not 1 not between 0 and 2", 268); call errcheck((select not 1 not between 0 and 2), "(select not 1 not between 0 and 2)", 268);
  call errcheck(not (1 not between 0 and 2), "not (1 not between 0 and 2)", 269); call errcheck((select not (1 not between 0 and 2)), "(select not (1 not between 0 and 2))", 269);
  call errcheck((not 1) not between 0 and 2 == 0, "(not 1) not between 0 and 2 == 0", 270); call errcheck((select (not 1) not between 0 and 2 == 0), "(select (not 1) not between 0 and 2 == 0)", 270);
  call errcheck(1 not between 2 and 0, "1 not between 2 and 0", 271); call errcheck((select 1 not between 2 and 0), "(select 1 not between 2 and 0)", 271);
  call errcheck(0 == not 7 not between 5 and 6, "0 == not 7 not between 5 and 6", 272); call errcheck((select 0 == not 7 not between 5 and 6), "(select 0 == not 7 not between 5 and 6)", 272);
  call errcheck(1 == (not 7) not between 5 and 6, "1 == (not 7) not between 5 and 6", 273); call errcheck((select 1 == (not 7) not between 5 and 6), "(select 1 == (not 7) not between 5 and 6)", 273);
  call errcheck((null not between 0 and 2) is null, "(null not between 0 and 2) is null", 274); call errcheck((select (null not between 0 and 2) is null), "(select (null not between 0 and 2) is null)", 274);
  call errcheck((1 not between null and 2) is null, "(1 not between null and 2) is null", 275); call errcheck((select (1 not between null and 2) is null), "(select (1 not between null and 2) is null)", 275);
  call errcheck((1 not between 0 and null) is null, "(1 not between 0 and null) is null", 276); call errcheck((select (1 not between 0 and null) is null), "(select (1 not between 0 and null) is null)", 276);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "not_between_operations")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "not_between_operations")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_not_between_operations(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "not_between_operations", start_refs, end_refs)); set fails := fails + 1; end if;

-- assorted comparisons
create procedure test_numeric_comparisons() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(0 == zero, "0 == zero", 281); call errcheck((select 0 == zero), "(select 0 == zero)", 281);
  call errcheck(not (one == zero), "not (one == zero)", 282); call errcheck((select not (one == zero)), "(select not (one == zero))", 282);
  call errcheck(one <> zero, "one <> zero", 283); call errcheck((select one <> zero), "(select one <> zero)", 283);
  call errcheck(not (one <> 1), "not (one <> 1)", 284); call errcheck((select not (one <> 1)), "(select not (one <> 1))", 284);
  call errcheck(one > zero, "one > zero", 285); call errcheck((select one > zero), "(select one > zero)", 285);
  call errcheck(zero < one, "zero < one", 286); call errcheck((select zero < one), "(select zero < one)", 286);
  call errcheck(one >= zero, "one >= zero", 287); call errcheck((select one >= zero), "(select one >= zero)", 287);
  call errcheck(zero <= one, "zero <= one", 288); call errcheck((select zero <= one), "(select zero <= one)", 288);
  call errcheck(one >= 1, "one >= 1", 289); call errcheck((select one >= 1), "(select one >= 1)", 289);
  call errcheck(one <= 1, "one <= 1", 290); call errcheck((select one <= 1), "(select one <= 1)", 290);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "numeric_comparisons")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "numeric_comparisons")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_numeric_comparisons(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "numeric_comparisons", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_simple_funcs() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(abs(-2) == 2, "abs(-2) == 2", 294); call errcheck((select abs(-2) == 2), "(select abs(-2) == 2)", 294);
  call errcheck(abs(2) == 2, "abs(2) == 2", 295); call errcheck((select abs(2) == 2), "(select abs(2) == 2)", 295);
  call errcheck(abs(-2.0) == 2, "abs(-2.0) == 2", 296); call errcheck((select abs(-2.0) == 2), "(select abs(-2.0) == 2)", 296);
  call errcheck(abs(2.0) == 2, "abs(2.0) == 2", 297); call errcheck((select abs(2.0) == 2), "(select abs(2.0) == 2)", 297);
  LET t := 3L;
  call errcheck(abs(t) == t, "abs(t) == t", 299); call errcheck((select abs(t) == t), "(select abs(t) == t)", 299);
  call errcheck(abs(-t) == t, "abs(-t) == t", 300); call errcheck((select abs(-t) == t), "(select abs(-t) == t)", 300);
  SET t := -4;
  call errcheck(abs(t) == -t, "abs(t) == -t", 302); call errcheck((select abs(t) == -t), "(select abs(t) == -t)", 302);
  call errcheck(abs(-t) == -t, "abs(-t) == -t", 303); call errcheck((select abs(-t) == -t), "(select abs(-t) == -t)", 303);

  call errcheck(abs(true) == true, "abs(true) == true", 305); call errcheck((select abs(true) == true), "(select abs(true) == true)", 305);
  call errcheck(abs(false) == false, "abs(false) == false", 306); call errcheck((select abs(false) == false), "(select abs(false) == false)", 306);
  call errcheck(abs(null) is null, "abs(null) is null", 307); call errcheck((select abs(null) is null), "(select abs(null) is null)", 307);

  call errcheck(sign(5) == 1, "sign(5) == 1", 309);
  call errcheck(sign(0.1) == 1, "sign(0.1) == 1", 310);
  call errcheck(sign(7L) == 1, "sign(7L) == 1", 311);
  call errcheck(sign(true) == 1, "sign(true) == 1", 312);
  call errcheck(sign(-5) == -1, "sign(-5) == -1", 313);
  call errcheck(sign(-0.1) == -1, "sign(-0.1) == -1", 314);
  call errcheck(sign(-7L) == -1, "sign(-7L) == -1", 315);
  call errcheck(sign(0) == 0, "sign(0) == 0", 316);
  call errcheck(sign(0.0) == 0, "sign(0.0) == 0", 317);
  call errcheck(sign(0L) == 0, "sign(0L) == 0", 318);
  call errcheck(sign(false) == 0, "sign(false) == 0", 319);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "simple_funcs")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "simple_funcs")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_simple_funcs(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "simple_funcs", start_refs, end_refs)); set fails := fails + 1; end if;

-- verify that out parameter is set in proc call
create procedure echo ( in arg1 integer not null, out arg2 integer not null)
begin
  set arg2 := arg1;
end;

create procedure test_out_arguments() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare scratch integer not null;
  call echo(12, scratch);
  call errcheck(scratch == 12, "scratch == 12", 331); call errcheck((select scratch == 12), "(select scratch == 12)", 331);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "out_arguments")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "out_arguments")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_out_arguments(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "out_arguments", start_refs, end_refs)); set fails := fails + 1; end if;

-- test simple recursive function
create procedure fib (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    declare t integer not null;
    call fib(arg - 1, result);
    call fib(arg - 2, t);
    set result := t + result;
  end if;
end;

create procedure test_simple_recursion() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare scratch integer not null;
  call fib(1, scratch);
  call errcheck(scratch == 1, "scratch == 1", 350);
  call fib(2, scratch);
  call errcheck(scratch == 1, "scratch == 1", 352);
  call fib(3, scratch);
  call errcheck(scratch == 2, "scratch == 2", 354);
  call fib(4, scratch);
  call errcheck(scratch == 3, "scratch == 3", 356);
  call fib(5, scratch);
  call errcheck(scratch == 5, "scratch == 5", 358);
  call fib(6, scratch);
  call errcheck(scratch == 8, "scratch == 8", 360);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "simple_recursion")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "simple_recursion")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_simple_recursion(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "simple_recursion", start_refs, end_refs)); set fails := fails + 1; end if;

-- test elementary cursor on select with no tables, still round trips through sqlite
create procedure test_cursor_basics() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare col1 integer;
  declare col2 real not null;
  declare basic_cursor cursor for select 1, 2.5;
  fetch basic_cursor into col1, col2;
  call errcheck(basic_cursor, "basic_cursor", 369);
  call errcheck(col1 == 1, "col1 == 1", 370);
  call errcheck(col2 == 2.5, "col2 == 2.5", 371);
  fetch basic_cursor into col1, col2;
  call errcheck(not basic_cursor, "not basic_cursor", 373);
  close basic_cursor;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cursor_basics")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cursor_basics")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cursor_basics(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cursor_basics", start_refs, end_refs)); set fails := fails + 1; end if;

-- the most expensive way to swap two variables ever :)
create procedure test_exchange_with_cursor() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare arg1 integer not null;
  declare arg2 integer not null;
  set arg1 := 7;
  set arg2 := 11;
  declare exchange_cursor cursor for select arg2, arg1;
  fetch exchange_cursor into arg1, arg2;
  call errcheck(exchange_cursor, "exchange_cursor", 385);
  call errcheck(arg1 == 11, "arg1 == 11", 386);
  call errcheck(arg2 == 7, "arg2 == 7", 387);
  close exchange_cursor;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "exchange_with_cursor")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "exchange_with_cursor")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_exchange_with_cursor(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "exchange_with_cursor", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure make_mixed()
begin
  create table mixed(
    id integer not null,
    name text,
    code long int, -- these are nullable to make the cg harder
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
create procedure test_read_mixed() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_ bool;
  declare rate_ real;
  declare bl_ blob;

  call load_mixed();

  declare read_cursor cursor for select * from mixed;

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  call errcheck(read_cursor, "read_cursor", 464);
  call errcheck(id_ == 1, "id_ == 1", 465);
  call errcheck(name_ == "a name", "name_ == \"a name\"", 466);
  call errcheck(code_ == 12, "code_ == 12", 467);
  call errcheck(flag_ == 1, "flag_ == 1", 468);
  call errcheck(rate_ == 5, "rate_ == 5", 469);
  call errcheck(string_from_blob(bl_) == "blob1", "string_from_blob(bl_) == \"blob1\"", 470);

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  call errcheck(read_cursor, "read_cursor", 473);
  call errcheck(id_ == 2, "id_ == 2", 474);
  call errcheck(name_ == "another name", "name_ == \"another name\"", 475);
  call errcheck(code_ == 14, "code_ == 14", 476);
  call errcheck(flag_ == 1, "flag_ == 1", 477);
  call errcheck(rate_ == 7, "rate_ == 7", 478);
  call errcheck(string_from_blob(bl_) == "blob2", "string_from_blob(bl_) == \"blob2\"", 479);

  fetch read_cursor into id_, name_, code_, flag_, rate_, bl_;
  call errcheck(not read_cursor, "not read_cursor", 482);
  close read_cursor;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "read_mixed")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "read_mixed")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_read_mixed(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "read_mixed", start_refs, end_refs)); set fails := fails + 1; end if;

-- now attempt a mutation
create procedure test_mutate_mixed() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare new_code long integer;
  declare code_ long integer;
  set new_code := 88;
  declare id_ integer;
  set id_ := 2; -- either works

  call load_mixed();

  update mixed set code = new_code where id = id_;
  declare updated_cursor cursor for select code from mixed where id = id_;
  fetch updated_cursor into code_;
  close updated_cursor;
  call errcheck(code_ == new_code, "code_ == new_code", 500);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "mutate_mixed")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "mutate_mixed")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_mutate_mixed(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "mutate_mixed", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_nested_select_expressions() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- use nested expression select
  declare temp_1 integer not null;
  set temp_1 := (select zero*5 + one*11);
  call errcheck(temp_1 == 11, "temp_1 == 11", 507);

  call load_mixed();

  set temp_1 := (select id from mixed where id > 1 order by id limit 1);
  call errcheck(temp_1 == 2, "temp_1 == 2", 512);

  set temp_1 := (select count(*) from mixed);
  call errcheck(temp_1 == 2, "temp_1 == 2", 515);

  declare temp_2 real;
  set temp_2 := (select avg(id) from mixed);
  call errcheck(temp_2 == 1.5, "temp_2 == 1.5", 519);

  call errcheck((select longs.neg) == -1, "(select longs.neg) == -1", 521);
  call errcheck((select -longs.neg) == 1, "(select -longs.neg) == 1", 522);
  call errcheck((select - -longs.neg) == -1, "(select - -longs.neg) == -1", 523);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "nested_select_expressions")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "nested_select_expressions")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_nested_select_expressions(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "nested_select_expressions", start_refs, end_refs)); set fails := fails + 1; end if;

-- complex delete pattern

create proc delete_one_from_mixed(out _id integer not null)
begin
  set _id := (select id from mixed order by id limit 1);
  delete from mixed where id = _id;
end;

create procedure test_delete_several() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call load_mixed();
  call errcheck(2 == (select count(*) from mixed), "2 == (select count(*) from mixed)", 536);

  declare id_ integer not null;
  call delete_one_from_mixed(id_);
  call errcheck(1 == id_, "1 == id_", 540);
  call errcheck(0 == (select count(*) from mixed where id = id_), "0 == (select count(*) from mixed where id = id_)", 541);
  call errcheck(1 == (select count(*) from mixed where id != id_), "1 == (select count(*) from mixed where id != id_)", 542);

  call delete_one_from_mixed(id_);
  call errcheck(2 == id_, "2 == id_", 545);
  call errcheck(0 == (select count(*) from mixed), "0 == (select count(*) from mixed)", 546);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "delete_several")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "delete_several")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_delete_several(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "delete_several", start_refs, end_refs)); set fails := fails + 1; end if;

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
create procedure test_string_ref_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare a_string text not null;
  call string_copy("Hello", a_string);
  declare result bool not null;
  call string_equal(a_string, "Hello", result);
  call errcheck(result, "result", 570);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_ref_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_ref_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_ref_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_ref_test", start_refs, end_refs)); set fails := fails + 1; end if;

-- try out some string comparisons
create procedure test_string_comparisons() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare t1 text;
  declare t2 text;
  declare t3 text;

  set t1 := "a";
  set t2 := "b";
  set t3 := "a";

  call errcheck("a" == "a", "\"a\" == \"a\"", 583); call errcheck((select "a" == "a"), "(select \"a\" == \"a\")", 583);
  call errcheck("a" IS "a", "\"a\" IS \"a\"", 584); call errcheck((select "a" IS "a"), "(select \"a\" IS \"a\")", 584);
  call errcheck("a" != "b", "\"a\" != \"b\"", 585); call errcheck((select "a" != "b"), "(select \"a\" != \"b\")", 585);
  call errcheck("a" IS NOT "b", "\"a\" IS NOT \"b\"", 586); call errcheck((select "a" IS NOT "b"), "(select \"a\" IS NOT \"b\")", 586);
  call errcheck(t1 < t2, "t1 < t2", 587); call errcheck((select t1 < t2), "(select t1 < t2)", 587);
  call errcheck(t2 > t1, "t2 > t1", 588); call errcheck((select t2 > t1), "(select t2 > t1)", 588);
  call errcheck(t1 <= t2, "t1 <= t2", 589); call errcheck((select t1 <= t2), "(select t1 <= t2)", 589);
  call errcheck(t2 >= t1, "t2 >= t1", 590); call errcheck((select t2 >= t1), "(select t2 >= t1)", 590);
  call errcheck(t1 <= t3, "t1 <= t3", 591); call errcheck((select t1 <= t3), "(select t1 <= t3)", 591);
  call errcheck(t3 >= t1, "t3 >= t1", 592); call errcheck((select t3 >= t1), "(select t3 >= t1)", 592);
  call errcheck(t1 == t3, "t1 == t3", 593); call errcheck((select t1 == t3), "(select t1 == t3)", 593);
  call errcheck(t1 != t2, "t1 != t2", 594); call errcheck((select t1 != t2), "(select t1 != t2)", 594);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_comparisons")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_comparisons")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_comparisons(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_comparisons", start_refs, end_refs)); set fails := fails + 1; end if;

-- string comparison nullability checks
create procedure test_string_comparisons_nullability() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare null_ text;
  declare x text not null;
  set x := "x";
  call errcheck((nullable(x) < nullable(x)) is not null, "(nullable(x) < nullable(x)) is not null", 602); call errcheck((select (nullable(x) < nullable(x)) is not null), "(select (nullable(x) < nullable(x)) is not null)", 602);
  call errcheck((nullable(x) > nullable("x")) is not null, "(nullable(x) > nullable(\"x\")) is not null", 603); call errcheck((select (nullable(x) > nullable("x")) is not null), "(select (nullable(x) > nullable(\"x\")) is not null)", 603);
  call errcheck((null_ > x) is null, "(null_ > x) is null", 604); call errcheck((select (null_ > x) is null), "(select (null_ > x) is null)", 604);
  call errcheck((x > null_) is null, "(x > null_) is null", 605); call errcheck((select (x > null_) is null), "(select (x > null_) is null)", 605);
  call errcheck((null_ > null_) is null, "(null_ > null_) is null", 606); call errcheck((select (null_ > null_) is null), "(select (null_ > null_) is null)", 606);
  call errcheck((null_ == null_) is null, "(null_ == null_) is null", 607); call errcheck((select (null_ == null_) is null), "(select (null_ == null_) is null)", 607);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_comparisons_nullability")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_comparisons_nullability")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_comparisons_nullability(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_comparisons_nullability", start_refs, end_refs)); set fails := fails + 1; end if;

-- string is null and is not null tests
create procedure test_string_is_null_or_not() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare null_ text;
  declare x text not null;
  set x := "x";
  declare y text;
  set y := nullable("y");

  call errcheck(null_ is null, "null_ is null", 618); call errcheck((select null_ is null), "(select null_ is null)", 618);
  call errcheck(nullable(x) is not null, "nullable(x) is not null", 619); call errcheck((select nullable(x) is not null), "(select nullable(x) is not null)", 619);
  call errcheck(y is not null, "y is not null", 620); call errcheck((select y is not null), "(select y is not null)", 620);
  call errcheck(not (null_ is not null), "not (null_ is not null)", 621); call errcheck((select not (null_ is not null)), "(select not (null_ is not null))", 621);
  call errcheck(not (nullable(x) is null), "not (nullable(x) is null)", 622); call errcheck((select not (nullable(x) is null)), "(select not (nullable(x) is null))", 622);
  call errcheck(not (y is null), "not (y is null)", 623); call errcheck((select not (y is null)), "(select not (y is null))", 623);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_is_null_or_not")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_is_null_or_not")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_is_null_or_not(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_is_null_or_not", start_refs, end_refs)); set fails := fails + 1; end if;

-- binding tests for not null types
create procedure test_bind_not_nullables() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck(b == (select b), "b == (select b)", 641); -- binding not null bool
  call errcheck(i == (select i), "i == (select i)", 642); -- binding not null int
  call errcheck(l == (select l), "l == (select l)", 643); -- binding not null long
  call errcheck(r == (select r), "r == (select r)", 644); -- binding not null real
  call errcheck(t == (select t), "t == (select t)", 645); -- binding not null text

  call errcheck(b != (select not b), "b != (select not b)", 647); -- binding not null bool
  call errcheck(i != (select 1 + i), "i != (select 1 + i)", 648); -- binding not null int
  call errcheck(l != (select 1 + l), "l != (select 1 + l)", 649); -- binding not null long
  call errcheck(r != (select 1 + r), "r != (select 1 + r)", 650); -- binding not null real
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bind_not_nullables")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bind_not_nullables")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bind_not_nullables(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bind_not_nullables", start_refs, end_refs)); set fails := fails + 1; end if;

-- binding tests for nullable types
create procedure test_bind_nullables_not_null() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck(b == (select b), "b == (select b)", 667); -- binding nullable not null bool
  call errcheck(i == (select i), "i == (select i)", 668); -- binding nullable not null int
  call errcheck(l == (select l), "l == (select l)", 669); -- binding nullable not null long
  call errcheck(r == (select r), "r == (select r)", 670); -- binding nullable not null real
  call errcheck(t == (select t), "t == (select t)", 671); -- binding nullable not null text

  call errcheck(b != (select not b), "b != (select not b)", 673); -- binding nullable not null bool
  call errcheck(i != (select 1 + i), "i != (select 1 + i)", 674); -- binding nullable not null int
  call errcheck(l != (select 1 + l), "l != (select 1 + l)", 675); -- binding nullable not null long
  call errcheck(r != (select 1 + r), "r != (select 1 + r)", 676); -- binding nullable not null real
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bind_nullables_not_null")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bind_nullables_not_null")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bind_nullables_not_null(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bind_nullables_not_null", start_refs, end_refs)); set fails := fails + 1; end if;

-- binding tests for nullable types values null
create procedure test_bind_nullables_null() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck((select b) is null, "(select b) is null", 693); -- binding null bool
  call errcheck((select i) is null, "(select i) is null", 694); -- binding null int
  call errcheck((select l) is null, "(select l) is null", 695); -- binding null long
  call errcheck((select r) is null, "(select r) is null", 696); -- binding null real
  call errcheck((select t) is null, "(select t) is null", 697); -- binding null text

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bind_nullables_null")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bind_nullables_null")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bind_nullables_null(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bind_nullables_null", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_loop_fetch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_ bool;
  declare rate_ real;
  declare bl_ blob;
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for select * from mixed;

  set count := 0;
  set sum := 0;
  loop fetch read_cursor into id_, name_, code_, flag_, rate_, bl_
  begin
    set count := count + 1;
    set sum := sum + id_;
  end;

  call errcheck(count == 2, "count == 2", 722); -- there should be two rows
  call errcheck(sum == 3, "sum == 3", 723); -- some math along the way
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "loop_fetch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "loop_fetch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_loop_fetch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "loop_fetch", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure load_more_mixed()
begin
  delete from mixed;
  insert into mixed values (1, "a name", 12, 1, 5.0, NULL);
  insert into mixed values (2, "some name", 14, 3, 7.0, NULL);
  insert into mixed values (3, "yet another name", 15, 3, 17.4, NULL);
  insert into mixed values (4, "some name", 19, 4, 9.1, NULL);
  insert into mixed values (5, "what name", 21, 8, 12.3, NULL);
end;

create procedure test_loop_control_flow() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_ bool;
  declare rate_ real;
  declare bl_ blob;
  declare count integer not null;

  call load_more_mixed();

  declare read_cursor cursor for select * from mixed;

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

  call errcheck(count == 3, "count == 3", 763); -- there should be three rows tested
  call errcheck(id_ == 4, "id_ == 4", 764); -- the match goes with id #4
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "loop_control_flow")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "loop_control_flow")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_loop_control_flow(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "loop_control_flow", start_refs, end_refs)); set fails := fails + 1; end if;

-- basic test of while loop plus leave and continue
create procedure test_while_control_flow() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i, sum integer not null;

  set i := 0;
  set sum := 0;
  while i < 5
  begin
    set i := i + 1;
    set sum := sum + i;
  end;

  call errcheck(i == 5, "i == 5", 779); -- loop ended on time
  call errcheck(sum == 15, "sum == 15", 780); -- correct sum computed: 1+2+3+4+5

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

  call errcheck(i == 4, "i == 4", 798); -- loop ended on time
  call errcheck(sum == 4, "sum == 4", 799); -- correct sum computed: 1+3
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "while_control_flow")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "while_control_flow")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_while_control_flow(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "while_control_flow", start_refs, end_refs)); set fails := fails + 1; end if;

-- same test but the control variable is nullable making the expression nullable
create procedure test_while_control_flow_with_nullables() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i, sum integer;

  set i := 0;
  set sum := 0;
  while i < 5
  begin
    set i := i + 1;
    set sum := sum + i;
  end;

  call errcheck(i == 5, "i == 5", 814); -- loop ended on time
  call errcheck(sum == 15, "sum == 15", 815); -- correct sum computed: 1+2+3+4+5
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "while_control_flow_with_nullables")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "while_control_flow_with_nullables")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_while_control_flow_with_nullables(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "while_control_flow_with_nullables", start_refs, end_refs)); set fails := fails + 1; end if;

-- like predicate test
create procedure test_like_predicate() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck("this is a test" like "%is a%", "\"this is a test\" like \"%is a%\"", 820); call errcheck((select "this is a test" like "%is a%"), "(select \"this is a test\" like \"%is a%\")", 820);
  call errcheck(not ("this is a test" like "is a"), "not (\"this is a test\" like \"is a\")", 821); call errcheck((select not ("this is a test" like "is a")), "(select not (\"this is a test\" like \"is a\"))", 821);

  declare txt text;
  call errcheck(("" like txt) is null, "(\"\" like txt) is null", 824); call errcheck((select ("" like txt) is null), "(select (\"\" like txt) is null)", 824);
  call errcheck((txt like "%") is null, "(txt like \"%\") is null", 825); call errcheck((select (txt like "%") is null), "(select (txt like \"%\") is null)", 825);
  call errcheck((txt like txt) is null, "(txt like txt) is null", 826); call errcheck((select (txt like txt) is null), "(select (txt like txt) is null)", 826);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "like_predicate")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "like_predicate")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_like_predicate(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "like_predicate", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_throw_and_catch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare did_throw bool not null;
  declare did_continue bool not null;
  set did_continue := 0;
  begin try
    call throws(did_throw);
    set did_throw := one / zero; -- this does not run
  end try;
  begin catch
    set did_continue := 1;
  end catch;
  call errcheck(did_throw == 1, "did_throw == 1", 857); -- exception was caught
  call errcheck(did_continue == 1, "did_continue == 1", 858); -- execution continued
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "throw_and_catch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "throw_and_catch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_throw_and_catch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "throw_and_catch", start_refs, end_refs)); set fails := fails + 1; end if;

-- the catch block should not run if no errors
create procedure test_throw_and_not_catch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare did_catch integer not null;
  begin try
    set did_catch := 0;
  end try;
  begin catch
    set did_catch := 1;
  end catch;
  call errcheck(did_catch == 0, "did_catch == 0", 870); -- catch did not run
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "throw_and_not_catch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "throw_and_not_catch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_throw_and_not_catch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "throw_and_not_catch", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_simple_case_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result integer;
  call case_tester1(1, result);
  call errcheck(result == 100, "result == 100", 894);
  call case_tester1(2, result);
  call errcheck(result == 200, "result == 200", 896);
  call case_tester1(3, result);
  call errcheck(result == 300, "result == 300", 898);
  call case_tester1(5, result);
  call errcheck(result == 400, "result == 400", 900);

  call case_tester2(1, result);
  call errcheck(result == 100, "result == 100", 903);
  call case_tester2(2, result);
  call errcheck(result == 200, "result == 200", 905);
  call case_tester2(3, result);
  call errcheck(result == 300, "result == 300", 907);
  call case_tester2(5, result);
  call errcheck(result is null, "result is null", 909);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "simple_case_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "simple_case_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_simple_case_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "simple_case_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure string_case_tester1(value text, out result text)
begin
  set result := case value
                     when "1" then "100"
                     when "2" then "200"
                     when "3" then "300"
                     end;
end;

create procedure test_string_case_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result text;
  call string_case_tester1("1", result);
  call errcheck(result == "100", "result == \"100\"", 924);
  call string_case_tester1("2", result);
  call errcheck(result == "200", "result == \"200\"", 926);
  call string_case_tester1("3", result);
  call errcheck(result == "300", "result == \"300\"", 928);
  call string_case_tester1("5", result);
  call errcheck(result is null, "result is null", 930);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_case_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_case_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_case_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_case_test", start_refs, end_refs)); set fails := fails + 1; end if;


create procedure in_tester1(value integer not null, out result bool not null)
begin
  set result := value in (1, 2, 3);
end;

create procedure test_in_test_not_null() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result bool not null;
  call in_tester1(1, result);
  call errcheck(result, "result", 942);
  call in_tester1(2, result);
  call errcheck(result, "result", 944);
  call in_tester1(3, result);
  call errcheck(result, "result", 946);
  call in_tester1(4, result);
  call errcheck(not result, "not result", 948);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "in_test_not_null")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "in_test_not_null")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_in_test_not_null(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "in_test_not_null", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure in_tester2(value integer, out result bool)
begin
  declare two integer;
  set two := 2;
  set result := value in (1, two, 3);
end;

create procedure test_in_test_nullables() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result bool;
  call in_tester2(1, result);
  call errcheck(result, "result", 961);
  call in_tester2(2, result);
  call errcheck(result, "result", 963);
  call in_tester2(3, result);
  call errcheck(result, "result", 965);
  call in_tester2(4, result);
  call errcheck(not result, "not result", 967);
  call in_tester2(null, result);
  call errcheck(result is null, "result is null", 969);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "in_test_nullables")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "in_test_nullables")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_in_test_nullables(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "in_test_nullables", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure nullables_case_tester(value integer, out result integer not null)
begin
  -- this is a very weird way to get a bool
  set result := case 1 when value then 1 else 0 end;
end;

create procedure test_nullable_when_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result integer not null;
  call nullables_case_tester(1, result);
  call errcheck(result == 1, "result == 1", 981);
  call nullables_case_tester(0, result);
  call errcheck(result == 0, "result == 0", 983);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "nullable_when_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "nullable_when_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_nullable_when_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "nullable_when_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure nullables_case_tester2(value integer, out result integer not null)
begin
  -- this is a very weird way to get a bool
  set result := case when value then 1 else 0 end;
end;

create procedure test_nullable_when_pred_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result integer not null;
  call nullables_case_tester(1, result);
  call errcheck(result == 1, "result == 1", 995);
  call nullables_case_tester(0, result);
  call errcheck(result == 0, "result == 0", 997);
  call nullables_case_tester(null, result);
  call errcheck(result == 0, "result == 0", 999);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "nullable_when_pred_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "nullable_when_pred_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_nullable_when_pred_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "nullable_when_pred_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure in_string_tester(value text, out result bool)
begin
  set result := value in ("this", "that");
end;

create procedure test_string_in_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare result bool;
  call in_string_tester("this", result);
  call errcheck(result, "result", 1010);
  call in_string_tester("that", result);
  call errcheck(result, "result", 1012);
  call in_string_tester("at", result);
  call errcheck(not result, "not result", 1014);
  call in_string_tester(null, result);
  call errcheck(result is null, "result is null", 1016);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_in_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_in_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_in_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_in_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_string_between_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare n1, n2, n3 text;
  declare s1, s2, s3 text not null;

  set n1 := "1";
  set n2 := "2";
  set n3 := "3";
  set s1 := "1";
  set s2 := "2";
  set s3 := "3";

  call errcheck(s2 between s1 and s3, "s2 between s1 and s3", 1030); call errcheck((select s2 between s1 and s3), "(select s2 between s1 and s3)", 1030);
  call errcheck(not (s2 between s3 and s1), "not (s2 between s3 and s1)", 1031); call errcheck((select not (s2 between s3 and s1)), "(select not (s2 between s3 and s1))", 1031);
  call errcheck(1 + (s2 between s1 and s3) == 2, "1 + (s2 between s1 and s3) == 2", 1032); call errcheck((select 1 + (s2 between s1 and s3) == 2), "(select 1 + (s2 between s1 and s3) == 2)", 1032);

  call errcheck(n2 between n1 and n3, "n2 between n1 and n3", 1034); call errcheck((select n2 between n1 and n3), "(select n2 between n1 and n3)", 1034);
  call errcheck(not (n2 between n3 and n1), "not (n2 between n3 and n1)", 1035); call errcheck((select not (n2 between n3 and n1)), "(select not (n2 between n3 and n1))", 1035);

  set n2 := null;
  call errcheck((n2 between n1 and n3) is null, "(n2 between n1 and n3) is null", 1038); call errcheck((select (n2 between n1 and n3) is null), "(select (n2 between n1 and n3) is null)", 1038);
  set n2 := "2";

  set n1 := null;
  call errcheck((n2 between n1 and n3) is null, "(n2 between n1 and n3) is null", 1042); call errcheck((select (n2 between n1 and n3) is null), "(select (n2 between n1 and n3) is null)", 1042);
  set n1 := "1";

  set n3 := null;
  call errcheck((n2 between n1 and n3) is null, "(n2 between n1 and n3) is null", 1046); call errcheck((select (n2 between n1 and n3) is null), "(select (n2 between n1 and n3) is null)", 1046);
  set n3 := "3";
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_between_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_between_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_between_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_between_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_string_not_between_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare n1, n2, n3 text;
  declare s1, s2, s3 text not null;

  set n1 := "1";
  set n2 := "2";
  set n3 := "3";
  set s1 := "1";
  set s2 := "2";
  set s3 := "3";

  call errcheck(not (s2 not between s1 and s3), "not (s2 not between s1 and s3)", 1061); call errcheck((select not (s2 not between s1 and s3)), "(select not (s2 not between s1 and s3))", 1061);
  call errcheck(s2 not between s3 and s1, "s2 not between s3 and s1", 1062); call errcheck((select s2 not between s3 and s1), "(select s2 not between s3 and s1)", 1062);
  call errcheck(1 + (s2 not between s1 and s3) == 1, "1 + (s2 not between s1 and s3) == 1", 1063); call errcheck((select 1 + (s2 not between s1 and s3) == 1), "(select 1 + (s2 not between s1 and s3) == 1)", 1063);

  call errcheck(not (n2 not between n1 and n3), "not (n2 not between n1 and n3)", 1065); call errcheck((select not (n2 not between n1 and n3)), "(select not (n2 not between n1 and n3))", 1065);
  call errcheck(n2 not between n3 and n1, "n2 not between n3 and n1", 1066); call errcheck((select n2 not between n3 and n1), "(select n2 not between n3 and n1)", 1066);

  set n2 := null;
  call errcheck((n2 not between n1 and n3) is null, "(n2 not between n1 and n3) is null", 1069); call errcheck((select (n2 not between n1 and n3) is null), "(select (n2 not between n1 and n3) is null)", 1069);
  set n2 := "2";

  set n1 := null;
  call errcheck((n2 not between n1 and n3) is null, "(n2 not between n1 and n3) is null", 1073); call errcheck((select (n2 not between n1 and n3) is null), "(select (n2 not between n1 and n3) is null)", 1073);
  set n1 := "1";

  set n3 := null;
  call errcheck((n2 not between n1 and n3) is null, "(n2 not between n1 and n3) is null", 1077); call errcheck((select (n2 not between n1 and n3) is null), "(select (n2 not between n1 and n3) is null)", 1077);
  set n3 := "3";
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_not_between_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_not_between_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_not_between_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_not_between_test", start_refs, end_refs)); set fails := fails + 1; end if;

create proc maybe_commit(do_commit bool not null)
begin
  call load_mixed();
  begin transaction;
  delete from mixed where id = 1;
  call errcheck(1 == (select count(*) from mixed), "1 == (select count(*) from mixed)", 1086); -- delete successful
  if do_commit then
    commit transaction;
  else
    rollback transaction;
  end if;
end;

create procedure test_transaction_mechanics() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call maybe_commit(1);
  call errcheck(1 == (select count(*) from mixed), "1 == (select count(*) from mixed)", 1096); -- commit successful
  call maybe_commit(0);
  call errcheck(2 == (select count(*) from mixed), "2 == (select count(*) from mixed)", 1098); -- rollback successful
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "transaction_mechanics")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "transaction_mechanics")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_transaction_mechanics(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "transaction_mechanics", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:identity=(id, code, bl))
@attribute(cql:generate_copy)
create procedure get_mixed(lim integer not null)
begin
  select * from mixed limit lim;
end;

@attribute(cql:generate_copy)
create procedure get_one_from_mixed(id_ integer not null)
begin
  declare C cursor for select * from mixed where id = id_;
  fetch C;
  out C;
end;

create procedure test_proc_loop_fetch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare id_ integer not null;
  declare name_ text;
  declare code_ long int;
  declare flag_ bool;
  declare rate_ real;
  declare bl_ blob;
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for call get_mixed(200);

  set count := 0;
  loop fetch read_cursor into id_, name_, code_, flag_, rate_, bl_
  begin
    set count := count + 1;
  end;

  call errcheck(count == 2, "count == 2", 1135); -- there should be two rows
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "proc_loop_fetch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "proc_loop_fetch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_proc_loop_fetch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "proc_loop_fetch", start_refs, end_refs)); set fails := fails + 1; end if;

create proc savepoint_maybe_commit(do_commit bool not null)
begin
  call load_mixed();
  savepoint foo;
  delete from mixed where id = 1;
  call errcheck(1 == (select count(*) from mixed), "1 == (select count(*) from mixed)", 1143); -- delete successful
  if do_commit then
    release savepoint foo;
  else
    rollback transaction to savepoint foo;
  end if;
end;

create procedure test_savepoint_mechanics() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call savepoint_maybe_commit(1);
  call errcheck(1 == (select count(*) from mixed), "1 == (select count(*) from mixed)", 1153); -- savepoint commit successful
  call savepoint_maybe_commit(0);
  call errcheck(2 == (select count(*) from mixed), "2 == (select count(*) from mixed)", 1155); -- savepoint rollback successful
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "savepoint_mechanics")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "savepoint_mechanics")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_savepoint_mechanics(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "savepoint_mechanics", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_exists_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call load_mixed();
  call errcheck((select EXISTS(select * from mixed)), "(select EXISTS(select * from mixed))", 1160); -- exists found rows
  delete from mixed;
  call errcheck((select NOT EXISTS(select * from mixed)), "(select NOT EXISTS(select * from mixed))", 1162); -- not exists found no rows
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "exists_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "exists_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_exists_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "exists_test", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_complex_nested_selects() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  create table vals(id int, val int);
  create table codes(id int, code int);

  insert into vals values(1, 100);
  insert into vals values(2, 200);
  insert into vals values(3, 300);

  insert into codes values(1, 1000);
  insert into codes values(1, 1001);
  insert into codes values(1, 1002);
  insert into codes values(2, 2000);
  insert into codes values(2, 2001);
  insert into codes values(3, 3000);

  declare c1 cursor for select id from vals as T1 where exists (select * from codes as T2 where T1.id == T2.id and T2.code % 1000 == 1);

  declare id_ integer;
  declare count_ integer;
  loop fetch c1 into id_
  begin
    call errcheck(case id_ when 1 then 1 when 2 then 1 else 0 end, "case id_ when 1 then 1 when 2 then 1 else 0 end", 1198);
  end;

  declare c2 cursor for
    select id, (select count(*) from codes T2 where T2.id = T1.id) as code_count
    from vals T1
    where val >= 7;
  loop fetch c2 into id_, count_
  begin
    call errcheck(count_ == case id_ when 1 then 3 when 2 then 2 when 3 then 1 else 0 end, "count_ == case id_ when 1 then 3 when 2 then 2 when 3 then 1 else 0 end", 1207);
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "complex_nested_selects")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "complex_nested_selects")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_complex_nested_selects(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "complex_nested_selects", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_proc_loop_auto_fetch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare count, sum integer not null;

  call load_mixed();

  declare read_cursor cursor for call get_mixed(200);

  set count := 0;
  set sum := 0;
  loop fetch read_cursor
  begin
    set count := count + 1;
    set sum := sum + read_cursor.id;
  end;

  call errcheck(count == 2, "count == 2", 1226); -- there should be two rows
  call errcheck(sum == 3, "sum == 3", 1227); -- id checksum
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "proc_loop_auto_fetch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "proc_loop_auto_fetch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_proc_loop_auto_fetch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "proc_loop_auto_fetch", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_coalesce() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i integer;
  set i := null;
  call errcheck(coalesce(i, i, 2) == 2, "coalesce(i, i, 2) == 2", 1233); call errcheck((select coalesce(i, i, 2) == 2), "(select coalesce(i, i, 2) == 2)", 1233); -- grab the not null last value
  call errcheck(ifnull(i, 2) == 2, "ifnull(i, 2) == 2", 1234); call errcheck((select ifnull(i, 2) == 2), "(select ifnull(i, 2) == 2)", 1234); -- grab the not null last value

  set i := nullable(3);
  call errcheck(coalesce(i, i, 2) == 3, "coalesce(i, i, 2) == 3", 1237); call errcheck((select coalesce(i, i, 2) == 3), "(select coalesce(i, i, 2) == 3)", 1237); -- grab the not null first value
  call errcheck(ifnull(i, 2) == 3, "ifnull(i, 2) == 3", 1238); call errcheck((select ifnull(i, 2) == 3), "(select ifnull(i, 2) == 3)", 1238); -- grab the not null first value
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "coalesce")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "coalesce")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_coalesce(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "coalesce", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_printf_expression() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(printf("%d and %d", 12, 7) == "12 and 7", "printf(\"%d and %d\", 12, 7) == \"12 and 7\"", 1242); -- loose printf ok
  call errcheck((select printf("%d and %d", 12, 7)) == "12 and 7", "(select printf(\"%d and %d\", 12, 7)) == \"12 and 7\"", 1243); -- sql printf ok
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "printf_expression")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "printf_expression")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_printf_expression(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "printf_expression", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_case_with_null() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare x integer;
  set x := null;
  set x := case x when 0 then 1 else 2 end;
  call errcheck(x == 2, "x == 2", 1250); --null only matches the else
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "case_with_null")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "case_with_null")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_case_with_null(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "case_with_null", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_group_concat() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  create table conc_test(id int, name text);
  insert into conc_test values (1,"x");
  insert into conc_test values (1,"y");
  insert into conc_test values (2,"z");
  declare C cursor for select id, group_concat(name) as vals from conc_test group by id;
  fetch C;
  call errcheck(C.id = 1, "C.id = 1", 1260);
  call errcheck(C.vals = "x,y", "C.vals = \"x,y\"", 1261);
  fetch C;
  call errcheck(C.id = 2, "C.id = 2", 1263);
  call errcheck(C.vals = "z", "C.vals = \"z\"", 1264);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "group_concat")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "group_concat")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_group_concat(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "group_concat", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_strftime() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck((select strftime("%s", "1970-01-01T00:00:03")) == "3", "(select strftime(\"%s\", \"1970-01-01T00:00:03\")) == \"3\"", 1268); -- sql strftime ok
  call errcheck((select strftime(null, "1970-01-01T00:00:03")) is null, "(select strftime(null, \"1970-01-01T00:00:03\")) is null", 1269); -- strftime null format ok
  call errcheck((select strftime("%s", null)) is null, "(select strftime(\"%s\", null)) is null", 1270); -- strftime null timestring ok
  call errcheck((select strftime("%s", "1970-01-01T00:00:03", "+1 day")) == "86403", "(select strftime(\"%s\", \"1970-01-01T00:00:03\", \"+1 day\")) == \"86403\"", 1271); -- strftime null timestring ok
  call errcheck((select strftime("%W", "now", "+1 month", "start of month", "-3 minutes", "weekday 4")) is not null, "(select strftime(\"%W\", \"now\", \"+1 month\", \"start of month\", \"-3 minutes\", \"weekday 4\")) is not null", 1272); -- strftime with multiple modifiers on now ok
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "strftime")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "strftime")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_strftime(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "strftime", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_cast_expr() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck((select cast(1.3 as int)) == 1, "(select cast(1.3 as int)) == 1", 1276); -- cast expression
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cast_expr")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cast_expr")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cast_expr(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cast_expr", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_union_all_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor for
    select 1 as A, 2 as B
    union all
    select 3 as A, 4 as B;
  fetch C;
  call errcheck(C.A = 1, "C.A = 1", 1285);
  call errcheck(C.B = 2, "C.B = 2", 1286);
  fetch C;
  call errcheck(C.A = 3, "C.A = 3", 1288);
  call errcheck(C.B = 4, "C.B = 4", 1289);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "union_all_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "union_all_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_union_all_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "union_all_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_union_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor for
    select 1 as A, 2 as B
    union
    select 1 as A, 2 as B;
  fetch C;
  call errcheck(C.A = 1, "C.A = 1", 1298);
  call errcheck(C.B = 2, "C.B = 2", 1299);
  fetch C;
  call errcheck(NOT C, "NOT C", 1301); -- no more rows
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "union_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "union_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_union_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "union_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_union_test_with_nullable() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor for
    select nullable(121) as A, 212 as B
    union
    select nullable(121) as A, 212 as B;
  fetch C;
  call errcheck(C.A = 121, "C.A = 121", 1310);
  call errcheck(C.B = 212, "C.B = 212", 1311);
  fetch C;
  call errcheck(NOT C, "NOT C", 1313);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "union_test_with_nullable")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "union_test_with_nullable")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_union_test_with_nullable(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "union_test_with_nullable", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_with_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor for
  with X(A,B) as ( select 1,2)
  select * from X;

  fetch C;
  call errcheck(C.A = 1, "C.A = 1", 1322);
  call errcheck(C.B = 2, "C.B = 2", 1323);
  fetch C;
  call errcheck(NOT C, "NOT C", 1325);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "with_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "with_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_with_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "with_test", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_with_recursive_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
    call errcheck(C.X == i, "C.X == i", 1352); -- iterating over the recursive result
    set i := i + 1;
  end;
  call errcheck(i == 11, "i == 11", 1355); -- 10 results matched, 11th did not match
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "with_recursive_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "with_recursive_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_with_recursive_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "with_recursive_test", start_refs, end_refs)); set fails := fails + 1; end if;


create proc outint(out int1 integer, out int2 integer not null)
begin
  declare C1 cursor for select 1;
  fetch C1 into int1;
  declare C2 cursor for select 2;
  fetch C2 into int2;
END;

create procedure test_fetch_output_param() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare int1 integer;
  declare int2 integer not null;
  call outint(int1, int2);
  call errcheck(int1 == 1, "int1 == 1", 1371); -- bind output nullable
  call errcheck(int2 == 2, "int2 == 2", 1372); -- bind output not nullable
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "fetch_output_param")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "fetch_output_param")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_fetch_output_param(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "fetch_output_param", start_refs, end_refs)); set fails := fails + 1; end if;

declare function run_test_math(int1 integer not null, out int2 integer) integer not null;
declare function string_create() create text;
declare function string_ref_count(str text) integer not null;

create procedure test_external_functions() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare int_out integer;
  declare int_result integer not null;

  set int_result := run_test_math(100, int_out);
  call errcheck(int_out == 500, "int_out == 500", 1384);
  call errcheck(int_result == 700, "int_result == 700", 1385);

  declare text_result text;
  set text_result := string_create();

  call errcheck(text_result like "%Hello%", "text_result like \"%Hello%\"", 1390);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "external_functions")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "external_functions")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_external_functions(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "external_functions", start_refs, end_refs)); set fails := fails + 1; end if;

declare function set_create() create object not null;
declare function set_add(_set object not null, _key text not null) bool not null;
declare function set_contains(_set object not null, _key text not null) bool not null;

create procedure test_external_set() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- stress the create and copy semantics
  declare _set object not null;
  set _set := set_create();
  declare _set2 object not null;
  set _set2 := set_create();
  set _set := _set2; -- this is a copy

  call errcheck(nullable(_set) is not null, "nullable(_set) is not null", 1405); -- successful create
  call errcheck(not set_contains(_set, "garbonzo"), "not set_contains(_set, \"garbonzo\")", 1406); -- initially empty
  call errcheck(set_add(_set, "garbonzo"), "set_add(_set, \"garbonzo\")", 1407); -- successful addition
  call errcheck(set_contains(_set, "garbonzo"), "set_contains(_set, \"garbonzo\")", 1408); -- key added
  call errcheck(not set_add(_set, "garbonzo"), "not set_add(_set, \"garbonzo\")", 1409); -- duplicate addition
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "external_set")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "external_set")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_external_set(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "external_set", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_object_notnull() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare _setNN object not null;
  declare _set object;
  set _set := nullable(set_create());
  set _setNN := ifnull_crash(_set);
  call errcheck(_set == _setNN, "_set == _setNN", 1417); -- should be the same pointer
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "object_notnull")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "object_notnull")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_object_notnull(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "object_notnull", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_dummy_values() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
    call errcheck(C.id == i, "C.id == i", 1435);
    call errcheck(C.name == printf("name_%d", i), "C.name == printf(\"name_%d\", i)", 1436);
    call errcheck(C.code == i, "C.code == i", 1437);
    call errcheck(not C.flag == not i, "not C.flag == not i", 1438);
    call errcheck(C.rate == i, "C.rate == i", 1439);
    set i := i + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "dummy_values")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "dummy_values")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_dummy_values(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "dummy_values", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_blob_basics() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare s text not null;
  set s := "a string";
  declare b blob not null;
  set b := blob_from_string(s);
  declare s2 text not null;
  set s2 := string_from_blob(b);
  call errcheck(s == s2, "s == s2", 1451); -- blob conversion failed
  call errcheck(b == blob_from_string("a string"), "b == blob_from_string(\"a string\")", 1452);
  call errcheck(b IS blob_from_string("a string"), "b IS blob_from_string(\"a string\")", 1453);
  call errcheck(b <> blob_from_string("a strings"), "b <> blob_from_string(\"a strings\")", 1454);
  call errcheck(b IS NOT blob_from_string("a strings"), "b IS NOT blob_from_string(\"a strings\")", 1455);

  declare b_null blob;
  set b_null := null;
  declare s_null text;
  set s_null := null;
  call errcheck(b_null IS b_null, "b_null IS b_null", 1461);
  call errcheck(s_null IS s_null, "s_null IS s_null", 1462);
  call errcheck(b_null IS NOT b, "b_null IS NOT b", 1463);
  call errcheck(s_null IS NOT s, "s_null IS NOT s", 1464);
  call errcheck(b_null IS NULL, "b_null IS NULL", 1465);
  call errcheck(s_null IS NULL, "s_null IS NULL", 1466);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "blob_basics")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "blob_basics")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_blob_basics(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "blob_basics", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_blob_data_manip() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call load_blobs();
  declare i, count integer not null;

  declare C cursor for select * from blob_table order by id;
  set i := 0;
  set count := 20;

  loop fetch C
  begin
     declare s1, s2 text;
     call errcheck(i == C.id, "i == C.id", 1513);

     set s1 := string_from_blob(c.b1);
     call errcheck(s1 == printf("nullable blob %d", i), "s1 == printf(\"nullable blob %d\", i)", 1516); -- nullable blob failed to round trip

     set s2 := string_from_blob(c.b2);
     call errcheck(s2 == printf("not nullable blob %d", i), "s2 == printf(\"not nullable blob %d\", i)", 1519); -- not nullable blob failed to round trip

     set i := i + 1;
  end;

  call errcheck(i == count, "i == count", 1524); -- wrong number of rows
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "blob_data_manip")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "blob_data_manip")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_blob_data_manip(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "blob_data_manip", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_blob_data_manip_nullables() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i, count integer not null;
  declare C cursor for select * from blob_table order by id;
  set i := 0;
  set count := 20;

  call load_sparse_blobs();

  loop fetch C
  begin
     declare s1, s2 text;
     set s1 := string_from_blob(C.b1);
     call errcheck(i == C.id, "i == C.id", 1567);
     if i % 2 == 0 then
       set s1 := string_from_blob(C.b1);
       call errcheck(s1 == printf("nullable blob %d", i), "s1 == printf(\"nullable blob %d\", i)", 1570); -- nullable blob failed to round trip
     else
       call errcheck(C.b1 is null, "C.b1 is null", 1572);
     end if;
     set s2 := string_from_blob(C.b2);
     call errcheck(s2 == printf("not nullable blob %d", i), "s2 == printf(\"not nullable blob %d\", i)", 1575); -- not nullable blob failed to round trip
     set i := i + 1;
  end;

  call errcheck(i == count, "i == count", 1579); -- wrong number of rows
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "blob_data_manip_nullables")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "blob_data_manip_nullables")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_blob_data_manip_nullables(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "blob_data_manip_nullables", start_refs, end_refs)); set fails := fails + 1; end if;

create proc row_getter(x integer not null, y real not null, z text)
begin
  declare C cursor for select x X, y Y, z Z;
  fetch C;
  out C;
end;

create procedure test_data_reader() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor fetch from call row_getter(1, 2.5, "xyzzy");
  call errcheck(C.X == 1, "C.X == 1", 1591);
  call errcheck(C.Y == 2.5, "C.Y == 2.5", 1592);
  call errcheck(C.Z == "xyzzy", "C.Z == \"xyzzy\"", 1593);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "data_reader")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "data_reader")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_data_reader(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "data_reader", start_refs, end_refs)); set fails := fails + 1; end if;

-- test simple recursive function -- using func syntax!
create procedure fib2 (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    set result := fib2(arg-1) + fib2(arg-2);
  end if;
end;

create procedure test_recurse_with_proc() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(fib2(1) == 1, "fib2(1) == 1", 1607);
  call errcheck(fib2(2) == 1, "fib2(2) == 1", 1608);
  call errcheck(fib2(3) == 2, "fib2(3) == 2", 1609);
  call errcheck(fib2(4) == 3, "fib2(4) == 3", 1610);
  call errcheck(fib2(5) == 5, "fib2(5) == 5", 1611);
  call errcheck(fib2(6) == 8, "fib2(6) == 8", 1612);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "recurse_with_proc")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "recurse_with_proc")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_recurse_with_proc(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "recurse_with_proc", start_refs, end_refs)); set fails := fails + 1; end if;

-- test simple recursive function -- using func syntax!
create procedure fib3 (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := (select 1); -- for this to be a dml proc
  else
    set result := fib3(arg-1) + fib3(arg-2);
  end if;
end;

create procedure test_recurse_with_dml_proc() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- we force all the error handling code to run with this flavor
  call errcheck(fib3(1) == 1, "fib3(1) == 1", 1627);
  call errcheck(fib3(2) == 1, "fib3(2) == 1", 1628);
  call errcheck(fib3(3) == 2, "fib3(3) == 2", 1629);
  call errcheck(fib3(4) == 3, "fib3(4) == 3", 1630);
  call errcheck(fib3(5) == 5, "fib3(5) == 5", 1631);
  call errcheck(fib3(6) == 8, "fib3(6) == 8", 1632);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "recurse_with_dml_proc")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "recurse_with_dml_proc")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_recurse_with_dml_proc(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "recurse_with_dml_proc", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_row_id_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call load_mixed();
  declare C cursor for select rowid from mixed;
  declare r integer not null;
  set r := 1;

  loop fetch C
  begin
    call errcheck(C.rowid == r, "C.rowid == r", 1643);
    set r := r + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "row_id_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "row_id_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_row_id_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "row_id_test", start_refs, end_refs)); set fails := fails + 1; end if;


create procedure test_bind_and_fetch_all_types() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck(13*i == (select 13*i), "13*i == (select 13*i)", 1664);
  call errcheck(13*l == (select 13*l), "13*l == (select 13*l)", 1665);
  call errcheck(13*r == (select 13*r), "13*r == (select 13*r)", 1666);
  call errcheck(not b == (select not b), "not b == (select not b)", 1667);
  call errcheck(printf("foo %s", s) == (select printf("foo %s", s)), "printf(\"foo %s\", s) == (select printf(\"foo %s\", s))", 1668);
  call errcheck("blob text" == string_from_blob((select bl)), "\"blob text\" == string_from_blob((select bl))", 1669);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bind_and_fetch_all_types")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bind_and_fetch_all_types")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bind_and_fetch_all_types(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bind_and_fetch_all_types", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_bind_and_fetch_all_types_nullable() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck(13*i == (select 13*i), "13*i == (select 13*i)", 1687);
  call errcheck(13*l == (select 13*l), "13*l == (select 13*l)", 1688);
  call errcheck(13*r == (select 13*r), "13*r == (select 13*r)", 1689);
  call errcheck(not b == (select not b), "not b == (select not b)", 1690);
  call errcheck(printf("foo %s", s) == (select printf("foo %s", s)), "printf(\"foo %s\", s) == (select printf(\"foo %s\", s))", 1691);
  call errcheck("blob text" == string_from_blob((select bl)), "\"blob text\" == string_from_blob((select bl))", 1692);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bind_and_fetch_all_types_nullable")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bind_and_fetch_all_types_nullable")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bind_and_fetch_all_types_nullable(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bind_and_fetch_all_types_nullable", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_fetch_all_types_cursor() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
  call errcheck(13*i == C.i, "13*i == C.i", 1712);
  call errcheck(13*l == C.l, "13*l == C.l", 1713);
  call errcheck(13*r == C.r, "13*r == C.r", 1714);
  call errcheck(not b == C.b, "not b == C.b", 1715);
  call errcheck(printf("foo %s", s) == C.s, "printf(\"foo %s\", s) == C.s", 1716);
  call errcheck("blob text" == string_from_blob(C.bl), "\"blob text\" == string_from_blob(C.bl)", 1717);

  /*
  fetch C;
  call errcheck(not C, "not C", 1720);
  call errcheck(C.i == 0, "C.i == 0", 1721);
  call errcheck(C.l == 0, "C.l == 0", 1722);
  call errcheck(C.r == 0, "C.r == 0", 1723);
  call errcheck(C.b == 0, "C.b == 0", 1724);
  call errcheck(nullable(C.s) is null, "nullable(C.s) is null", 1725); -- even though s is not null, it is null... sigh
  call errcheck(nullable(c.bl) is null, "nullable(c.bl) is null", 1726); -- even though bl is not null, it is null... sigh
  */
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "fetch_all_types_cursor")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "fetch_all_types_cursor")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_fetch_all_types_cursor(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "fetch_all_types_cursor", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_fetch_all_types_cursor_nullable() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i integer;
  declare l long;
  declare r real;
  declare b bool;
  declare s text;
  declare bl blob;

  set i := nullable(10);
  set l := nullable(1234567890156789L);
  set r := nullable(1234.45);
  set b := nullable(1);
  set s := nullable("string");
  set bl := nullable(blob_from_string("blob text"));

  declare C cursor for select i*13 i, l*13 l, r*13 r, not b b, printf("foo %s",s) s, bl bl;
  fetch C;
  call errcheck(C, "C", 1746);
  call errcheck(13*i == C.i, "13*i == C.i", 1747);
  call errcheck(13*l == C.l, "13*l == C.l", 1748);
  call errcheck(13*r == C.r, "13*r == C.r", 1749);
  call errcheck(not b == C.b, "not b == C.b", 1750);
  call errcheck(printf("foo %s", s) == C.s, "printf(\"foo %s\", s) == C.s", 1751);
  call errcheck("blob text" == string_from_blob(C.bl), "\"blob text\" == string_from_blob(C.bl)", 1752);

  /*
  fetch C;
  call errcheck(not C, "not C", 1755);
  call errcheck(C.i is null, "C.i is null", 1756);
  call errcheck(C.l is null, "C.l is null", 1757);
  call errcheck(C.r is null, "C.r is null", 1758);
  call errcheck(C.b is null, "C.b is null", 1759);
  call errcheck(nullable(C.s) is null, "nullable(C.s) is null", 1760);
  call errcheck(nullable(c.bl) is null, "nullable(c.bl) is null", 1761);
  */
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "fetch_all_types_cursor_nullable")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "fetch_all_types_cursor_nullable")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_fetch_all_types_cursor_nullable(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "fetch_all_types_cursor_nullable", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_concat_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- concat is weaker than ~
  call errcheck('-22' == (SELECT ~1||2), "'-22' == (SELECT ~1||2)", 1766);
  call errcheck('-22' == (SELECT (~1)||2), "'-22' == (SELECT (~1)||2)", 1767);

  -- if the order was otherwise we'd get a different result...
  -- a semantic error actually
  call errcheck(-13 == (SELECT ~CAST(1||2 as INTEGER)), "-13 == (SELECT ~CAST(1||2 as INTEGER))", 1771);

  --- negation is stronger than CONCAT
  call errcheck('01' == (select -0||1), "'01' == (select -0||1)", 1774);
  call errcheck('01' == (select (-0)||1), "'01' == (select (-0)||1)", 1775);

  -- if the order was otherwise we'd get a different result...
  -- a semantic error actually
  call errcheck(-1 == (select -CAST(0||1 as INTEGER)), "-1 == (select -CAST(0||1 as INTEGER))", 1779);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "concat_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "concat_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_concat_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "concat_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Test precedence of multiply with (* / %) with add (+ -)
create procedure test_multiply_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(1+2*3 == 7, "1+2*3 == 7", 1785); call errcheck((select 1+2*3 == 7), "(select 1+2*3 == 7)", 1785);
  call errcheck(1+2*3+4*5 == 27, "1+2*3+4*5 == 27", 1786); call errcheck((select 1+2*3+4*5 == 27), "(select 1+2*3+4*5 == 27)", 1786);
  call errcheck(1+2/2 == 2, "1+2/2 == 2", 1787); call errcheck((select 1+2/2 == 2), "(select 1+2/2 == 2)", 1787);
  call errcheck(1+2/2*4 == 5, "1+2/2*4 == 5", 1788); call errcheck((select 1+2/2*4 == 5), "(select 1+2/2*4 == 5)", 1788);
  call errcheck(1+2/2*4 == 5, "1+2/2*4 == 5", 1789); call errcheck((select 1+2/2*4 == 5), "(select 1+2/2*4 == 5)", 1789);
  call errcheck(1*2+3 == 5, "1*2+3 == 5", 1790); call errcheck((select 1*2+3 == 5), "(select 1*2+3 == 5)", 1790);
  call errcheck(1*2+6/3 == 4, "1*2+6/3 == 4", 1791); call errcheck((select 1*2+6/3 == 4), "(select 1*2+6/3 == 4)", 1791);
  call errcheck(1*2+6/3 == 4, "1*2+6/3 == 4", 1792); call errcheck((select 1*2+6/3 == 4), "(select 1*2+6/3 == 4)", 1792);
  call errcheck(2*3*4+3/3 == 25, "2*3*4+3/3 == 25", 1793); call errcheck((select 2*3*4+3/3 == 25), "(select 2*3*4+3/3 == 25)", 1793);
  call errcheck(-5*5 == -25, "-5*5 == -25", 1794); call errcheck((select -5*5 == -25), "(select -5*5 == -25)", 1794);
  call errcheck(5-5*5 == -20, "5-5*5 == -20", 1795); call errcheck((select 5-5*5 == -20), "(select 5-5*5 == -20)", 1795);
  call errcheck(4+5*5 == 29, "4+5*5 == 29", 1796); call errcheck((select 4+5*5 == 29), "(select 4+5*5 == 29)", 1796);
  call errcheck(4*5+5 == 25, "4*5+5 == 25", 1797); call errcheck((select 4*5+5 == 25), "(select 4*5+5 == 25)", 1797);
  call errcheck(4*4-1 == 15, "4*4-1 == 15", 1798); call errcheck((select 4*4-1 == 15), "(select 4*4-1 == 15)", 1798);
  call errcheck(10-4*2 == 2, "10-4*2 == 2", 1799); call errcheck((select 10-4*2 == 2), "(select 10-4*2 == 2)", 1799);
  call errcheck(25%3/2 == 0, "25%3/2 == 0", 1800); call errcheck((select 25%3/2 == 0), "(select 25%3/2 == 0)", 1800);
  call errcheck(25/5%2 == 1, "25/5%2 == 1", 1801); call errcheck((select 25/5%2 == 1), "(select 25/5%2 == 1)", 1801);
  call errcheck(25*5%2 == 1, "25*5%2 == 1", 1802); call errcheck((select 25*5%2 == 1), "(select 25*5%2 == 1)", 1802);
  call errcheck(25*5%4%2 == 1, "25*5%4%2 == 1", 1803); call errcheck((select 25*5%4%2 == 1), "(select 25*5%4%2 == 1)", 1803);
  call errcheck(25-5%2 == 24, "25-5%2 == 24", 1804); call errcheck((select 25-5%2 == 24), "(select 25-5%2 == 24)", 1804);
  call errcheck(15%3-2 == -2, "15%3-2 == -2", 1805); call errcheck((select 15%3-2 == -2), "(select 15%3-2 == -2)", 1805);
  call errcheck(15-30%4 == 13, "15-30%4 == 13", 1806); call errcheck((select 15-30%4 == 13), "(select 15-30%4 == 13)", 1806);
  call errcheck(15-30/2 == 0, "15-30/2 == 0", 1807); call errcheck((select 15-30/2 == 0), "(select 15-30/2 == 0)", 1807);
  call errcheck(15/5-3 == 0, "15/5-3 == 0", 1808); call errcheck((select 15/5-3 == 0), "(select 15/5-3 == 0)", 1808);
  call errcheck(15*5-3 == 72, "15*5-3 == 72", 1809); call errcheck((select 15*5-3 == 72), "(select 15*5-3 == 72)", 1809);
  call errcheck(5*5-3 == 22, "5*5-3 == 22", 1810); call errcheck((select 5*5-3 == 22), "(select 5*5-3 == 22)", 1810);
  call errcheck(25+5%2 == 26, "25+5%2 == 26", 1811); call errcheck((select 25+5%2 == 26), "(select 25+5%2 == 26)", 1811);
  call errcheck(15%3+2 == 2, "15%3+2 == 2", 1812); call errcheck((select 15%3+2 == 2), "(select 15%3+2 == 2)", 1812);
  call errcheck(15+30%4 == 17, "15+30%4 == 17", 1813); call errcheck((select 15+30%4 == 17), "(select 15+30%4 == 17)", 1813);
  call errcheck(15+30/2 == 30, "15+30/2 == 30", 1814); call errcheck((select 15+30/2 == 30), "(select 15+30/2 == 30)", 1814);
  call errcheck(15/5+3 == 6, "15/5+3 == 6", 1815); call errcheck((select 15/5+3 == 6), "(select 15/5+3 == 6)", 1815);
  call errcheck(15*5+3 == 78, "15*5+3 == 78", 1816); call errcheck((select 15*5+3 == 78), "(select 15*5+3 == 78)", 1816);
  call errcheck(5*5+3 == 28, "5*5+3 == 28", 1817); call errcheck((select 5*5+3 == 28), "(select 5*5+3 == 28)", 1817);
  call errcheck(5*12/3 == 20, "5*12/3 == 20", 1818); call errcheck((select 5*12/3 == 20), "(select 5*12/3 == 20)", 1818);
  call errcheck(5*12/3%7 == 6, "5*12/3%7 == 6", 1819); call errcheck((select 5*12/3%7 == 6), "(select 5*12/3%7 == 6)", 1819);
  call errcheck(9%12/3*7 == 21, "9%12/3*7 == 21", 1820); call errcheck((select 9%12/3*7 == 21), "(select 9%12/3*7 == 21)", 1820);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "multiply_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "multiply_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_multiply_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "multiply_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Test precedence of binary (<< >> & |) with add (+ -)
create procedure test_shift_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(10<<1+1 == 40, "10<<1+1 == 40", 1825); call errcheck((select 10<<1+1 == 40), "(select 10<<1+1 == 40)", 1825);
  call errcheck(1+10<<1 == 22, "1+10<<1 == 22", 1826); call errcheck((select 1+10<<1 == 22), "(select 1+10<<1 == 22)", 1826);
  call errcheck(10<<1-1 == 10, "10<<1-1 == 10", 1827); call errcheck((select 10<<1-1 == 10), "(select 10<<1-1 == 10)", 1827);
  call errcheck(10<<4-1 == 80, "10<<4-1 == 80", 1828); call errcheck((select 10<<4-1 == 80), "(select 10<<4-1 == 80)", 1828);
  call errcheck(10-1<<1 == 18, "10-1<<1 == 18", 1829); call errcheck((select 10-1<<1 == 18), "(select 10-1<<1 == 18)", 1829);

  call errcheck(10>>3-1 == 2, "10>>3-1 == 2", 1831); call errcheck((select 10>>3-1 == 2), "(select 10>>3-1 == 2)", 1831);
  call errcheck(11-1>>1 == 5, "11-1>>1 == 5", 1832); call errcheck((select 11-1>>1 == 5), "(select 11-1>>1 == 5)", 1832);
  call errcheck(10>>1+1 == 2, "10>>1+1 == 2", 1833); call errcheck((select 10>>1+1 == 2), "(select 10>>1+1 == 2)", 1833);
  call errcheck(1+10>>1 == 5, "1+10>>1 == 5", 1834); call errcheck((select 1+10>>1 == 5), "(select 1+10>>1 == 5)", 1834);

  call errcheck(10&1+1 == 2, "10&1+1 == 2", 1836); call errcheck((select 10&1+1 == 2), "(select 10&1+1 == 2)", 1836);
  call errcheck(1+10&1 == 1, "1+10&1 == 1", 1837); call errcheck((select 1+10&1 == 1), "(select 1+10&1 == 1)", 1837);
  call errcheck(1+10&7 == 3, "1+10&7 == 3", 1838); call errcheck((select 1+10&7 == 3), "(select 1+10&7 == 3)", 1838);
  call errcheck(10-1&7 == 1, "10-1&7 == 1", 1839); call errcheck((select 10-1&7 == 1), "(select 10-1&7 == 1)", 1839);
  call errcheck(10-4&7 == 6, "10-4&7 == 6", 1840); call errcheck((select 10-4&7 == 6), "(select 10-4&7 == 6)", 1840);

  call errcheck(10|1+1 == 10, "10|1+1 == 10", 1842); call errcheck((select 10|1+1 == 10), "(select 10|1+1 == 10)", 1842);
  call errcheck(10|4 == 14, "10|4 == 14", 1843); call errcheck((select 10|4 == 14), "(select 10|4 == 14)", 1843);
  call errcheck(1+10|4 == 15, "1+10|4 == 15", 1844); call errcheck((select 1+10|4 == 15), "(select 1+10|4 == 15)", 1844);
  call errcheck(10-1|7 == 15, "10-1|7 == 15", 1845); call errcheck((select 10-1|7 == 15), "(select 10-1|7 == 15)", 1845);
  call errcheck(10-3|7 == 7, "10-3|7 == 7", 1846); call errcheck((select 10-3|7 == 7), "(select 10-3|7 == 7)", 1846);

  call errcheck(6&4 == 4, "6&4 == 4", 1848); call errcheck((select 6&4 == 4), "(select 6&4 == 4)", 1848);
  call errcheck(6&4|12 == 12, "6&4|12 == 12", 1849); call errcheck((select 6&4|12 == 12), "(select 6&4|12 == 12)", 1849);
  call errcheck(6&4|12|2 == 14, "6&4|12|2 == 14", 1850); call errcheck((select 6&4|12|2 == 14), "(select 6&4|12|2 == 14)", 1850);
  call errcheck(6&4|12|2|2 == 14, "6&4|12|2|2 == 14", 1851); call errcheck((select 6&4|12|2|2 == 14), "(select 6&4|12|2|2 == 14)", 1851);
  call errcheck(6&4|12|2|2<<3 == 112, "6&4|12|2|2<<3 == 112", 1852); call errcheck((select 6&4|12|2|2<<3 == 112), "(select 6&4|12|2|2<<3 == 112)", 1852);
  call errcheck(6&4|12|2|2<<3>>3<<2 == 56, "6&4|12|2|2<<3>>3<<2 == 56", 1853); call errcheck((select 6&4|12|2|2<<3>>3<<2 == 56), "(select 6&4|12|2|2<<3>>3<<2 == 56)", 1853);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "shift_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "shift_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_shift_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "shift_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Test precedence of inequality (< <= > >=) with binary (<< >> & |)
create procedure test_inequality_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(10 < 10<<1, "10 < 10<<1", 1858); call errcheck((select 10 < 10<<1), "(select 10 < 10<<1)", 1858);
  call errcheck(10 <= 10<<1, "10 <= 10<<1", 1859); call errcheck((select 10 <= 10<<1), "(select 10 <= 10<<1)", 1859);
  call errcheck(10 > 10>>1, "10 > 10>>1", 1860); call errcheck((select 10 > 10>>1), "(select 10 > 10>>1)", 1860);
  call errcheck(10 >= 10>>1, "10 >= 10>>1", 1861); call errcheck((select 10 >= 10>>1), "(select 10 >= 10>>1)", 1861);
  call errcheck(0 >= 0>>1, "0 >= 0>>1", 1862); call errcheck((select 0 >= 0>>1), "(select 0 >= 0>>1)", 1862);
  call errcheck(0 <= 0<<1, "0 <= 0<<1", 1863); call errcheck((select 0 <= 0<<1), "(select 0 <= 0<<1)", 1863);
  call errcheck(5 >= 0<<31, "5 >= 0<<31", 1864); call errcheck((select 5 >= 0<<31), "(select 5 >= 0<<31)", 1864);
  call errcheck(5 > 0<<31, "5 > 0<<31", 1865); call errcheck((select 5 > 0<<31), "(select 5 > 0<<31)", 1865);
  call errcheck(16>>1 >= 4<<1, "16>>1 >= 4<<1", 1866); call errcheck((select 16>>1 >= 4<<1), "(select 16>>1 >= 4<<1)", 1866);
  call errcheck(4<<1 <= 16>>1, "4<<1 <= 16>>1", 1867); call errcheck((select 4<<1 <= 16>>1), "(select 4<<1 <= 16>>1)", 1867);
  call errcheck(16>>1 > 3<<1, "16>>1 > 3<<1", 1868); call errcheck((select 16>>1 > 3<<1), "(select 16>>1 > 3<<1)", 1868);
  call errcheck(16>>1 >= 3<<1, "16>>1 >= 3<<1", 1869); call errcheck((select 16>>1 >= 3<<1), "(select 16>>1 >= 3<<1)", 1869);
  call errcheck(16>>1 <= 4<<1, "16>>1 <= 4<<1", 1870); call errcheck((select 16>>1 <= 4<<1), "(select 16>>1 <= 4<<1)", 1870);

  call errcheck(16&8 <= 4|8, "16&8 <= 4|8", 1872); call errcheck((select 16&8 <= 4|8), "(select 16&8 <= 4|8)", 1872);
  call errcheck(16&8 < 15, "16&8 < 15", 1873); call errcheck((select 16&8 < 15), "(select 16&8 < 15)", 1873);
  call errcheck(16&8 <= 15, "16&8 <= 15", 1874); call errcheck((select 16&8 <= 15), "(select 16&8 <= 15)", 1874);
  call errcheck(16&17 > 4, "16&17 > 4", 1875); call errcheck((select 16&17 > 4), "(select 16&17 > 4)", 1875);
  call errcheck(16&17 >= 4, "16&17 >= 4", 1876); call errcheck((select 16&17 >= 4), "(select 16&17 >= 4)", 1876);
  call errcheck(6 > 4&5, "6 > 4&5", 1877); call errcheck((select 6 > 4&5), "(select 6 > 4&5)", 1877);
  call errcheck(6 >= 4&5, "6 >= 4&5", 1878); call errcheck((select 6 >= 4&5), "(select 6 >= 4&5)", 1878);
  call errcheck(6 > 4|5, "6 > 4|5", 1879); call errcheck((select 6 > 4|5), "(select 6 > 4|5)", 1879);
  call errcheck(6 >= 4|5, "6 >= 4|5", 1880); call errcheck((select 6 >= 4|5), "(select 6 >= 4|5)", 1880);

  call errcheck(3|8 >= 4&5, "3|8 >= 4&5", 1882); call errcheck((select 3|8 >= 4&5), "(select 3|8 >= 4&5)", 1882);
  call errcheck(3|8 > 4&5, "3|8 > 4&5", 1883); call errcheck((select 3|8 > 4&5), "(select 3|8 > 4&5)", 1883);
  call errcheck(3|4 >= 4&5, "3|4 >= 4&5", 1884); call errcheck((select 3|4 >= 4&5), "(select 3|4 >= 4&5)", 1884);
  call errcheck(3|4 > 4&5, "3|4 > 4&5", 1885); call errcheck((select 3|4 > 4&5), "(select 3|4 > 4&5)", 1885);
  call errcheck(4&5 <= 3|8, "4&5 <= 3|8", 1886); call errcheck((select 4&5 <= 3|8), "(select 4&5 <= 3|8)", 1886);
  call errcheck(4&5 < 3|8, "4&5 < 3|8", 1887); call errcheck((select 4&5 < 3|8), "(select 4&5 < 3|8)", 1887);
  call errcheck(4&5 <= 3|4, "4&5 <= 3|4", 1888); call errcheck((select 4&5 <= 3|4), "(select 4&5 <= 3|4)", 1888);
  call errcheck(4&5 < 3|4, "4&5 < 3|4", 1889); call errcheck((select 4&5 < 3|4), "(select 4&5 < 3|4)", 1889);
  call errcheck(4|3 <= 3|4, "4|3 <= 3|4", 1890); call errcheck((select 4|3 <= 3|4), "(select 4|3 <= 3|4)", 1890);
  call errcheck(4&5 <= 5&4, "4&5 <= 5&4", 1891); call errcheck((select 4&5 <= 5&4), "(select 4&5 <= 5&4)", 1891);
  call errcheck(4&5 >= 5&4, "4&5 >= 5&4", 1892); call errcheck((select 4&5 >= 5&4), "(select 4&5 >= 5&4)", 1892);

  call errcheck(4&5 >= 5&4 > 0, "4&5 >= 5&4 > 0", 1894); call errcheck((select 4&5 >= 5&4 > 0), "(select 4&5 >= 5&4 > 0)", 1894);
  call errcheck(4&5 >= 5&4 <= 1, "4&5 >= 5&4 <= 1", 1895); call errcheck((select 4&5 >= 5&4 <= 1), "(select 4&5 >= 5&4 <= 1)", 1895);
  call errcheck(4&5 >= 5&4 >= 1, "4&5 >= 5&4 >= 1", 1896); call errcheck((select 4&5 >= 5&4 >= 1), "(select 4&5 >= 5&4 >= 1)", 1896);
  call errcheck(3&10 <= 100 <= 3&2, "3&10 <= 100 <= 3&2", 1897); call errcheck((select 3&10 <= 100 <= 3&2), "(select 3&10 <= 100 <= 3&2)", 1897);
  call errcheck((3&10 <= 100) <= 3&2 == 3&10 <= 100 <= 3&2, "(3&10 <= 100) <= 3&2 == 3&10 <= 100 <= 3&2", 1898); call errcheck((select (3&10 <= 100) <= 3&2 == 3&10 <= 100 <= 3&2), "(select (3&10 <= 100) <= 3&2 == 3&10 <= 100 <= 3&2)", 1898);
  call errcheck(5 > 3 > -1 > 0, "5 > 3 > -1 > 0", 1899); call errcheck((select 5 > 3 > -1 > 0), "(select 5 > 3 > -1 > 0)", 1899);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "inequality_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "inequality_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_inequality_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "inequality_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Test precedence of equality (= == != <> LIKE GLOB MATCH IN NOT IN IS_NOT_NULL IS_NULL) with binary (< <= > >=)
create procedure test_equality_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare null_ int;

  call errcheck(5 == 5, "5 == 5", 1906); call errcheck((select 5 == 5), "(select 5 == 5)", 1906);
  call errcheck(5 < 6 == 6 > 5, "5 < 6 == 6 > 5", 1907); call errcheck((select 5 < 6 == 6 > 5), "(select 5 < 6 == 6 > 5)", 1907);
  call errcheck(5 <= 6 == 6 >= 5, "5 <= 6 == 6 >= 5", 1908); call errcheck((select 5 <= 6 == 6 >= 5), "(select 5 <= 6 == 6 >= 5)", 1908);
  call errcheck(5 < 6 == 6 >= 5, "5 < 6 == 6 >= 5", 1909); call errcheck((select 5 < 6 == 6 >= 5), "(select 5 < 6 == 6 >= 5)", 1909);
  call errcheck(5 <= 6 == 6 > 5, "5 <= 6 == 6 > 5", 1910); call errcheck((select 5 <= 6 == 6 > 5), "(select 5 <= 6 == 6 > 5)", 1910);
  call errcheck(5 <= 6 == 1, "5 <= 6 == 1", 1911); call errcheck((select 5 <= 6 == 1), "(select 5 <= 6 == 1)", 1911);
  call errcheck(1 == 5 < 6, "1 == 5 < 6", 1912); call errcheck((select 1 == 5 < 6), "(select 1 == 5 < 6)", 1912);
  call errcheck(1 == 5 <= 6, "1 == 5 <= 6", 1913); call errcheck((select 1 == 5 <= 6), "(select 1 == 5 <= 6)", 1913);
  call errcheck(1 == 0 + 1, "1 == 0 + 1", 1914); call errcheck((select 1 == 0 + 1), "(select 1 == 0 + 1)", 1914);
  call errcheck(1 == 1 + 0 * 1, "1 == 1 + 0 * 1", 1915); call errcheck((select 1 == 1 + 0 * 1), "(select 1 == 1 + 0 * 1)", 1915);
  call errcheck(1 == 0 * 1 + 1, "1 == 0 * 1 + 1", 1916); call errcheck((select 1 == 0 * 1 + 1), "(select 1 == 0 * 1 + 1)", 1916);
  call errcheck(1 == 0 * -1 + 1, "1 == 0 * -1 + 1", 1917); call errcheck((select 1 == 0 * -1 + 1), "(select 1 == 0 * -1 + 1)", 1917);
  call errcheck(1 + 1 == 3 - 1 == 1, "1 + 1 == 3 - 1 == 1", 1918); call errcheck((select 1 + 1 == 3 - 1 == 1), "(select 1 + 1 == 3 - 1 == 1)", 1918);
  call errcheck(1 + 1 == 3 - 1 != 0, "1 + 1 == 3 - 1 != 0", 1919); call errcheck((select 1 + 1 == 3 - 1 != 0), "(select 1 + 1 == 3 - 1 != 0)", 1919);
  call errcheck(1 + 1 == 3 - 1 != 30, "1 + 1 == 3 - 1 != 30", 1920); call errcheck((select 1 + 1 == 3 - 1 != 30), "(select 1 + 1 == 3 - 1 != 30)", 1920);

  call errcheck(5 = 5, "5 = 5", 1922); call errcheck((select 5 = 5), "(select 5 = 5)", 1922);
  call errcheck(5 < 6 = 6 > 5, "5 < 6 = 6 > 5", 1923); call errcheck((select 5 < 6 = 6 > 5), "(select 5 < 6 = 6 > 5)", 1923);
  call errcheck(5 <= 6 = 6 >= 5, "5 <= 6 = 6 >= 5", 1924); call errcheck((select 5 <= 6 = 6 >= 5), "(select 5 <= 6 = 6 >= 5)", 1924);
  call errcheck(5 < 6 = 6 >= 5, "5 < 6 = 6 >= 5", 1925); call errcheck((select 5 < 6 = 6 >= 5), "(select 5 < 6 = 6 >= 5)", 1925);
  call errcheck(5 <= 6 = 6 > 5, "5 <= 6 = 6 > 5", 1926); call errcheck((select 5 <= 6 = 6 > 5), "(select 5 <= 6 = 6 > 5)", 1926);
  call errcheck(5 <= 6 = 1, "5 <= 6 = 1", 1927); call errcheck((select 5 <= 6 = 1), "(select 5 <= 6 = 1)", 1927);
  call errcheck(1 = 5 < 6, "1 = 5 < 6", 1928); call errcheck((select 1 = 5 < 6), "(select 1 = 5 < 6)", 1928);
  call errcheck(1 = 5 <= 6, "1 = 5 <= 6", 1929); call errcheck((select 1 = 5 <= 6), "(select 1 = 5 <= 6)", 1929);
  call errcheck(1 = 0 + 1, "1 = 0 + 1", 1930); call errcheck((select 1 = 0 + 1), "(select 1 = 0 + 1)", 1930);
  call errcheck(1 = 1 + 0 * 1, "1 = 1 + 0 * 1", 1931); call errcheck((select 1 = 1 + 0 * 1), "(select 1 = 1 + 0 * 1)", 1931);
  call errcheck(1 = 0 * 1 + 1, "1 = 0 * 1 + 1", 1932); call errcheck((select 1 = 0 * 1 + 1), "(select 1 = 0 * 1 + 1)", 1932);
  call errcheck(1 = 0 * -1 + 1, "1 = 0 * -1 + 1", 1933); call errcheck((select 1 = 0 * -1 + 1), "(select 1 = 0 * -1 + 1)", 1933);
  call errcheck(1 + 1 = 3 - 1 = 1, "1 + 1 = 3 - 1 = 1", 1934); call errcheck((select 1 + 1 = 3 - 1 = 1), "(select 1 + 1 = 3 - 1 = 1)", 1934);
  call errcheck(1 + 1 = 3 - 1 <> 0, "1 + 1 = 3 - 1 <> 0", 1935); call errcheck((select 1 + 1 = 3 - 1 <> 0), "(select 1 + 1 = 3 - 1 <> 0)", 1935);
  call errcheck(1 + 1 == 3 - 1 <> 0, "1 + 1 == 3 - 1 <> 0", 1936); call errcheck((select 1 + 1 == 3 - 1 <> 0), "(select 1 + 1 == 3 - 1 <> 0)", 1936);
  call errcheck(1 + 1 = 3 - 1 <> 30, "1 + 1 = 3 - 1 <> 30", 1937); call errcheck((select 1 + 1 = 3 - 1 <> 30), "(select 1 + 1 = 3 - 1 <> 30)", 1937);
  call errcheck(1 + 1 == 3 - 1 <> 30, "1 + 1 == 3 - 1 <> 30", 1938); call errcheck((select 1 + 1 == 3 - 1 <> 30), "(select 1 + 1 == 3 - 1 <> 30)", 1938);

  call errcheck(1 == 1 <> 0 == 1 = 1 != 0 = 1 == 1, "1 == 1 <> 0 == 1 = 1 != 0 = 1 == 1", 1940); call errcheck((select 1 == 1 <> 0 == 1 = 1 != 0 = 1 == 1), "(select 1 == 1 <> 0 == 1 = 1 != 0 = 1 == 1)", 1940);

  -- CQL requires both operands of binary_like to be text, so there is no way to test
  -- order of operations with <, <=, etc. When concat (||) is implemented, it is
  -- possible to write a test case.

  -- CQL requires both operands of binary_like to be text, so there is no way to test
  -- order of operations with <, <=, etc. When concat (||) is implemented, it is
  -- possible to write a test case.

  -- GLOB must be inside a select statement so it also cannot be tested
  -- MATCH can only be in a select statement, no test necessary

  -- Test IS_NOT and IS
  call errcheck(nullable(1) + nullable(1) IS NULL == 0, "nullable(1) + nullable(1) IS NULL == 0", 1954); call errcheck((select nullable(1) + nullable(1) IS NULL == 0), "(select nullable(1) + nullable(1) IS NULL == 0)", 1954);
  call errcheck(nullable(1) + nullable(1) IS NOT NULL == 1, "nullable(1) + nullable(1) IS NOT NULL == 1", 1955); call errcheck((select nullable(1) + nullable(1) IS NOT NULL == 1), "(select nullable(1) + nullable(1) IS NOT NULL == 1)", 1955);
  call errcheck(nullable(1) + nullable(1) IS NULL + 1 == 0, "nullable(1) + nullable(1) IS NULL + 1 == 0", 1956); call errcheck((select nullable(1) + nullable(1) IS NULL + 1 == 0), "(select nullable(1) + nullable(1) IS NULL + 1 == 0)", 1956); -- Evaluated as: (1 + 1) IS (NULL + 1) == 0;
  call errcheck(nullable(1) + nullable(1) IS NOT NULL, "nullable(1) + nullable(1) IS NOT NULL", 1957); call errcheck((select nullable(1) + nullable(1) IS NOT NULL), "(select nullable(1) + nullable(1) IS NOT NULL)", 1957);
  call errcheck((nullable(1) + nullable(1) IS NOT NULL) + 1 == 2, "(nullable(1) + nullable(1) IS NOT NULL) + 1 == 2", 1958); call errcheck((select (nullable(1) + nullable(1) IS NOT NULL) + 1 == 2), "(select (nullable(1) + nullable(1) IS NOT NULL) + 1 == 2)", 1958);
  call errcheck(1 + 1 IS NOT NULL + 1 == 1, "1 + 1 IS NOT NULL + 1 == 1", 1959); call errcheck((select 1 + 1 IS NOT NULL + 1 == 1), "(select 1 + 1 IS NOT NULL + 1 == 1)", 1959);
  call errcheck(1 + NULL IS NULL, "1 + NULL IS NULL", 1960); call errcheck((select 1 + NULL IS NULL), "(select 1 + NULL IS NULL)", 1960);
  call errcheck(NULL + 1 IS NULL, "NULL + 1 IS NULL", 1961); call errcheck((select NULL + 1 IS NULL), "(select NULL + 1 IS NULL)", 1961);
  call errcheck(NULL * 1 IS NULL, "NULL * 1 IS NULL", 1962); call errcheck((select NULL * 1 IS NULL), "(select NULL * 1 IS NULL)", 1962);
  call errcheck(NULL * 0 IS NULL, "NULL * 0 IS NULL", 1963); call errcheck((select NULL * 0 IS NULL), "(select NULL * 0 IS NULL)", 1963);
  call errcheck(0 * NULL * 0 IS NULL, "0 * NULL * 0 IS NULL", 1964); call errcheck((select 0 * NULL * 0 IS NULL), "(select 0 * NULL * 0 IS NULL)", 1964);
  call errcheck(NULL > 0 IS NULL, "NULL > 0 IS NULL", 1965); call errcheck((select NULL > 0 IS NULL), "(select NULL > 0 IS NULL)", 1965);
  call errcheck(NULL >= 1 IS NULL, "NULL >= 1 IS NULL", 1966); call errcheck((select NULL >= 1 IS NULL), "(select NULL >= 1 IS NULL)", 1966);
  call errcheck(NULL < 2 IS NULL, "NULL < 2 IS NULL", 1967); call errcheck((select NULL < 2 IS NULL), "(select NULL < 2 IS NULL)", 1967);
  call errcheck(NULL <= 3 IS NULL, "NULL <= 3 IS NULL", 1968); call errcheck((select NULL <= 3 IS NULL), "(select NULL <= 3 IS NULL)", 1968);
  call errcheck(1 + NULL == 3 IS NULL, "1 + NULL == 3 IS NULL", 1969); call errcheck((select 1 + NULL == 3 IS NULL), "(select 1 + NULL == 3 IS NULL)", 1969);
  call errcheck(1 + NULL != 3 IS NULL, "1 + NULL != 3 IS NULL", 1970); call errcheck((select 1 + NULL != 3 IS NULL), "(select 1 + NULL != 3 IS NULL)", 1970);
  call errcheck(1 + NULL <> 3 IS NULL, "1 + NULL <> 3 IS NULL", 1971); call errcheck((select 1 + NULL <> 3 IS NULL), "(select 1 + NULL <> 3 IS NULL)", 1971);
  call errcheck(1 = NULL * 1 + 1 IS NULL, "1 = NULL * 1 + 1 IS NULL", 1972); call errcheck((select 1 = NULL * 1 + 1 IS NULL), "(select 1 = NULL * 1 + 1 IS NULL)", 1972);
  call errcheck(1 = NULL * -1 + 1 IS NULL, "1 = NULL * -1 + 1 IS NULL", 1973); call errcheck((select 1 = NULL * -1 + 1 IS NULL), "(select 1 = NULL * -1 + 1 IS NULL)", 1973);
  call errcheck(1 + NULL = 3 - 1 = 1 IS NULL, "1 + NULL = 3 - 1 = 1 IS NULL", 1974); call errcheck((select 1 + NULL = 3 - 1 = 1 IS NULL), "(select 1 + NULL = 3 - 1 = 1 IS NULL)", 1974);
  call errcheck(1 + NULL = 3 - 1 <> 0 IS NULL, "1 + NULL = 3 - 1 <> 0 IS NULL", 1975); call errcheck((select 1 + NULL = 3 - 1 <> 0 IS NULL), "(select 1 + NULL = 3 - 1 <> 0 IS NULL)", 1975);
  call errcheck(1 + NULL == 3 - 1 <> 0 IS NULL, "1 + NULL == 3 - 1 <> 0 IS NULL", 1976); call errcheck((select 1 + NULL == 3 - 1 <> 0 IS NULL), "(select 1 + NULL == 3 - 1 <> 0 IS NULL)", 1976);
  call errcheck(1 + NULL = 3 - 1 <> 30 IS NULL, "1 + NULL = 3 - 1 <> 30 IS NULL", 1977); call errcheck((select 1 + NULL = 3 - 1 <> 30 IS NULL), "(select 1 + NULL = 3 - 1 <> 30 IS NULL)", 1977);
  call errcheck(1 + NULL == 3 - 1 <> 30 IS NULL, "1 + NULL == 3 - 1 <> 30 IS NULL", 1978); call errcheck((select 1 + NULL == 3 - 1 <> 30 IS NULL), "(select 1 + NULL == 3 - 1 <> 30 IS NULL)", 1978);
  call errcheck((NULL IS NOT NULL) == 0, "(NULL IS NOT NULL) == 0", 1979); call errcheck((select (NULL IS NOT NULL) == 0), "(select (NULL IS NOT NULL) == 0)", 1979);
  call errcheck(nullable(1) + nullable(1) IS NOT NULL, "nullable(1) + nullable(1) IS NOT NULL", 1980); call errcheck((select nullable(1) + nullable(1) IS NOT NULL), "(select nullable(1) + nullable(1) IS NOT NULL)", 1980);
  call errcheck(null_ == 3 IS NULL, "null_ == 3 IS NULL", 1981); call errcheck((select null_ == 3 IS NULL), "(select null_ == 3 IS NULL)", 1981);
  call errcheck(((null_ == 3) IS NULL) == 1, "((null_ == 3) IS NULL) == 1", 1982); call errcheck((select ((null_ == 3) IS NULL) == 1), "(select ((null_ == 3) IS NULL) == 1)", 1982);
  call errcheck((null_ == 3 IS NULL) == 1, "(null_ == 3 IS NULL) == 1", 1983); call errcheck((select (null_ == 3 IS NULL) == 1), "(select (null_ == 3 IS NULL) == 1)", 1983);
  call errcheck((null_ == 3 IS NULL) == 1, "(null_ == 3 IS NULL) == 1", 1984); call errcheck((select (null_ == 3 IS NULL) == 1), "(select (null_ == 3 IS NULL) == 1)", 1984);
  call errcheck(nullable(null_ == 3 IS NULL) IS NOT NULL, "nullable(null_ == 3 IS NULL) IS NOT NULL", 1985); call errcheck((select nullable(null_ == 3 IS NULL) IS NOT NULL), "(select nullable(null_ == 3 IS NULL) IS NOT NULL)", 1985);
  call errcheck((1 + NULL == 3 IS NOT NULL) == 0, "(1 + NULL == 3 IS NOT NULL) == 0", 1986); call errcheck((select (1 + NULL == 3 IS NOT NULL) == 0), "(select (1 + NULL == 3 IS NOT NULL) == 0)", 1986);
  call errcheck((1 + NULL = 3 - 1 <> 0 IS NOT NULL) == 0, "(1 + NULL = 3 - 1 <> 0 IS NOT NULL) == 0", 1987); call errcheck((select (1 + NULL = 3 - 1 <> 0 IS NOT NULL) == 0), "(select (1 + NULL = 3 - 1 <> 0 IS NOT NULL) == 0)", 1987);
  call errcheck((1 + NULL == 3 - 1 <> 0 IS NOT NULL) == 0, "(1 + NULL == 3 - 1 <> 0 IS NOT NULL) == 0", 1988); call errcheck((select (1 + NULL == 3 - 1 <> 0 IS NOT NULL) == 0), "(select (1 + NULL == 3 - 1 <> 0 IS NOT NULL) == 0)", 1988);
  call errcheck((1 + NULL = 3 - 1 <> 30 IS NOT NULL) == 0, "(1 + NULL = 3 - 1 <> 30 IS NOT NULL) == 0", 1989); call errcheck((select (1 + NULL = 3 - 1 <> 30 IS NOT NULL) == 0), "(select (1 + NULL = 3 - 1 <> 30 IS NOT NULL) == 0)", 1989);

  -- Basic IS tests, all non null
  call errcheck(2 * 3 IS 4 + 2, "2 * 3 IS 4 + 2", 1992); call errcheck((select 2 * 3 IS 4 + 2), "(select 2 * 3 IS 4 + 2)", 1992);
  call errcheck(2 * 3 IS 4 + 2, "2 * 3 IS 4 + 2", 1993); call errcheck((select 2 * 3 IS 4 + 2), "(select 2 * 3 IS 4 + 2)", 1993);
  call errcheck(10-4*2 IS 2, "10-4*2 IS 2", 1994); call errcheck((select 10-4*2 IS 2), "(select 10-4*2 IS 2)", 1994);
  call errcheck(25%3/2 IS 0, "25%3/2 IS 0", 1995); call errcheck((select 25%3/2 IS 0), "(select 25%3/2 IS 0)", 1995);
  call errcheck(25/5%2 IS 1, "25/5%2 IS 1", 1996); call errcheck((select 25/5%2 IS 1), "(select 25/5%2 IS 1)", 1996);
  call errcheck(25*5%2 IS 1, "25*5%2 IS 1", 1997); call errcheck((select 25*5%2 IS 1), "(select 25*5%2 IS 1)", 1997);
  call errcheck(25*5%4%2 IS 1, "25*5%4%2 IS 1", 1998); call errcheck((select 25*5%4%2 IS 1), "(select 25*5%4%2 IS 1)", 1998);
  call errcheck(25-5%2 IS 24, "25-5%2 IS 24", 1999); call errcheck((select 25-5%2 IS 24), "(select 25-5%2 IS 24)", 1999);
  call errcheck(15%3-2 IS -2, "15%3-2 IS -2", 2000); call errcheck((select 15%3-2 IS -2), "(select 15%3-2 IS -2)", 2000);
  call errcheck(15-30%4 IS 13, "15-30%4 IS 13", 2001); call errcheck((select 15-30%4 IS 13), "(select 15-30%4 IS 13)", 2001);
  call errcheck(15-30/2 IS 0, "15-30/2 IS 0", 2002); call errcheck((select 15-30/2 IS 0), "(select 15-30/2 IS 0)", 2002);
  call errcheck(15/5-3 IS 0, "15/5-3 IS 0", 2003); call errcheck((select 15/5-3 IS 0), "(select 15/5-3 IS 0)", 2003);
  call errcheck(15*5-3 IS 72, "15*5-3 IS 72", 2004); call errcheck((select 15*5-3 IS 72), "(select 15*5-3 IS 72)", 2004);
  call errcheck(5*5-3 IS 22, "5*5-3 IS 22", 2005); call errcheck((select 5*5-3 IS 22), "(select 5*5-3 IS 22)", 2005);
  call errcheck(25+5%2 IS 26, "25+5%2 IS 26", 2006); call errcheck((select 25+5%2 IS 26), "(select 25+5%2 IS 26)", 2006);
  call errcheck(15%3+2 IS 2, "15%3+2 IS 2", 2007); call errcheck((select 15%3+2 IS 2), "(select 15%3+2 IS 2)", 2007);
  call errcheck(15+30%4 IS 17, "15+30%4 IS 17", 2008); call errcheck((select 15+30%4 IS 17), "(select 15+30%4 IS 17)", 2008);
  call errcheck(15+30/2 IS 30, "15+30/2 IS 30", 2009); call errcheck((select 15+30/2 IS 30), "(select 15+30/2 IS 30)", 2009);
  call errcheck(15/5+3 IS 6, "15/5+3 IS 6", 2010); call errcheck((select 15/5+3 IS 6), "(select 15/5+3 IS 6)", 2010);
  call errcheck(15*5+3 IS 78, "15*5+3 IS 78", 2011); call errcheck((select 15*5+3 IS 78), "(select 15*5+3 IS 78)", 2011);
  call errcheck(5*5+3 IS 28, "5*5+3 IS 28", 2012); call errcheck((select 5*5+3 IS 28), "(select 5*5+3 IS 28)", 2012);
  call errcheck(5*12/3 IS 20, "5*12/3 IS 20", 2013); call errcheck((select 5*12/3 IS 20), "(select 5*12/3 IS 20)", 2013);
  call errcheck(5*12/3%7 IS 6, "5*12/3%7 IS 6", 2014); call errcheck((select 5*12/3%7 IS 6), "(select 5*12/3%7 IS 6)", 2014);
  call errcheck(9%12/3*7 IS 21, "9%12/3*7 IS 21", 2015); call errcheck((select 9%12/3*7 IS 21), "(select 9%12/3*7 IS 21)", 2015);

  -- IS tests with null
  call errcheck(1 IS 1 == 1 IS 1 == 1, "1 IS 1 == 1 IS 1 == 1", 2018); call errcheck((select 1 IS 1 == 1 IS 1 == 1), "(select 1 IS 1 == 1 IS 1 == 1)", 2018);
  call errcheck(5 > 6 IS 2 < 1, "5 > 6 IS 2 < 1", 2019); call errcheck((select 5 > 6 IS 2 < 1), "(select 5 > 6 IS 2 < 1)", 2019);
  call errcheck(5 <= 6 IS 2 > 1, "5 <= 6 IS 2 > 1", 2020); call errcheck((select 5 <= 6 IS 2 > 1), "(select 5 <= 6 IS 2 > 1)", 2020);
  call errcheck(5 == 5 IS 2 > 1, "5 == 5 IS 2 > 1", 2021); call errcheck((select 5 == 5 IS 2 > 1), "(select 5 == 5 IS 2 > 1)", 2021);
  call errcheck("1" IS "2" == 0, "\"1\" IS \"2\" == 0", 2022); call errcheck((select "1" IS "2" == 0), "(select \"1\" IS \"2\" == 0)", 2022);
  call errcheck(nullable("1") IS NULL == 0, "nullable(\"1\") IS NULL == 0", 2023); call errcheck((select nullable("1") IS NULL == 0), "(select nullable(\"1\") IS NULL == 0)", 2023);
  call errcheck(NULL IS "1" == 0, "NULL IS \"1\" == 0", 2024); call errcheck((select NULL IS "1" == 0), "(select NULL IS \"1\" == 0)", 2024);
  call errcheck(NULL IS NULL, "NULL IS NULL", 2025); call errcheck((select NULL IS NULL), "(select NULL IS NULL)", 2025);
  call errcheck(null_ == 0 IS NULL, "null_ == 0 IS NULL", 2026); call errcheck((select null_ == 0 IS NULL), "(select null_ == 0 IS NULL)", 2026);
  call errcheck(NULL IS NULL == 1 != 0, "NULL IS NULL == 1 != 0", 2027); call errcheck((select NULL IS NULL == 1 != 0), "(select NULL IS NULL == 1 != 0)", 2027);
  call errcheck(NULL IS NULL = 1 <> 0, "NULL IS NULL = 1 <> 0", 2028); call errcheck((select NULL IS NULL = 1 <> 0), "(select NULL IS NULL = 1 <> 0)", 2028);
  call errcheck(null_ == null_ IS NULL, "null_ == null_ IS NULL", 2029); call errcheck((select null_ == null_ IS NULL), "(select null_ == null_ IS NULL)", 2029);
  call errcheck(NULL IS (null_ == 0), "NULL IS (null_ == 0)", 2030); call errcheck((select NULL IS (null_ == 0)), "(select NULL IS (null_ == 0))", 2030);
  call errcheck(NULL IS NOT NULL == 0, "NULL IS NOT NULL == 0", 2031); call errcheck((select NULL IS NOT NULL == 0), "(select NULL IS NOT NULL == 0)", 2031);
  call errcheck((NULL IS NOT NULL) == 0, "(NULL IS NOT NULL) == 0", 2032); call errcheck((select (NULL IS NOT NULL) == 0), "(select (NULL IS NOT NULL) == 0)", 2032);
  call errcheck(nullable(5) > nullable(2) IS NOT NULL, "nullable(5) > nullable(2) IS NOT NULL", 2033); call errcheck((select nullable(5) > nullable(2) IS NOT NULL), "(select nullable(5) > nullable(2) IS NOT NULL)", 2033);
  call errcheck(NULL IS NOT 2 < 3, "NULL IS NOT 2 < 3", 2034); call errcheck((select NULL IS NOT 2 < 3), "(select NULL IS NOT 2 < 3)", 2034);
  call errcheck(nullable(NULL IS 2 < 3) IS NOT NULL, "nullable(NULL IS 2 < 3) IS NOT NULL", 2035); call errcheck((select nullable(NULL IS 2 < 3) IS NOT NULL), "(select nullable(NULL IS 2 < 3) IS NOT NULL)", 2035);
  call errcheck(NULL IS NULL + 1, "NULL IS NULL + 1", 2036); call errcheck((select NULL IS NULL + 1), "(select NULL IS NULL + 1)", 2036);
  call errcheck(NULL IS 1 + NULL, "NULL IS 1 + NULL", 2037); call errcheck((select NULL IS 1 + NULL), "(select NULL IS 1 + NULL)", 2037);
  call errcheck(NULL IS 1 << NULL, "NULL IS 1 << NULL", 2038); call errcheck((select NULL IS 1 << NULL), "(select NULL IS 1 << NULL)", 2038);

  -- Test IN
  call errcheck(3 IN (1, 2) == 0, "3 IN (1, 2) == 0", 2041); call errcheck((select 3 IN (1, 2) == 0), "(select 3 IN (1, 2) == 0)", 2041);
  call errcheck(3 + 2 IN (1, 5), "3 + 2 IN (1, 5)", 2042); call errcheck((select 3 + 2 IN (1, 5)), "(select 3 + 2 IN (1, 5))", 2042);
  call errcheck(3 / 3 IN (1, 2), "3 / 3 IN (1, 2)", 2043); call errcheck((select 3 / 3 IN (1, 2)), "(select 3 / 3 IN (1, 2))", 2043);
  call errcheck(3 / 3 IN (1, 2) IN (1), "3 / 3 IN (1, 2) IN (1)", 2044); call errcheck((select 3 / 3 IN (1, 2) IN (1)), "(select 3 / 3 IN (1, 2) IN (1))", 2044);
  call errcheck(1 IN (NULL, 1), "1 IN (NULL, 1)", 2045); call errcheck((select 1 IN (NULL, 1)), "(select 1 IN (NULL, 1))", 2045);
  call errcheck(NOT (1 IN (NULL, 5)), "NOT (1 IN (NULL, 5))", 2046);
  call errcheck((SELECT NULL IS (NOT (1 IN (NULL, 5)))), "(SELECT NULL IS (NOT (1 IN (NULL, 5))))", 2047); -- known sqlite and CQL IN difference for NULL
  call errcheck(NULL IS (NULL IN (1)), "NULL IS (NULL IN (1))", 2048); call errcheck((select NULL IS (NULL IN (1))), "(select NULL IS (NULL IN (1)))", 2048);

  -- Test NOT IN
  call errcheck(3 NOT IN (1, 2) == 1, "3 NOT IN (1, 2) == 1", 2051); call errcheck((select 3 NOT IN (1, 2) == 1), "(select 3 NOT IN (1, 2) == 1)", 2051);
  call errcheck(1 NOT IN (1, 2) == 0, "1 NOT IN (1, 2) == 0", 2052); call errcheck((select 1 NOT IN (1, 2) == 0), "(select 1 NOT IN (1, 2) == 0)", 2052);
  call errcheck(3 + 1 NOT IN (1, 5), "3 + 1 NOT IN (1, 5)", 2053); call errcheck((select 3 + 1 NOT IN (1, 5)), "(select 3 + 1 NOT IN (1, 5))", 2053);
  call errcheck(3 / 1 NOT IN (1, 2), "3 / 1 NOT IN (1, 2)", 2054); call errcheck((select 3 / 1 NOT IN (1, 2)), "(select 3 / 1 NOT IN (1, 2))", 2054);
  call errcheck(3 / 1 NOT IN (1, 2) NOT IN (0), "3 / 1 NOT IN (1, 2) NOT IN (0)", 2055); call errcheck((select 3 / 1 NOT IN (1, 2) NOT IN (0)), "(select 3 / 1 NOT IN (1, 2) NOT IN (0))", 2055);
  call errcheck(NOT (1 NOT IN (NULL, 1)), "NOT (1 NOT IN (NULL, 1))", 2056); call errcheck((select NOT (1 NOT IN (NULL, 1))), "(select NOT (1 NOT IN (NULL, 1)))", 2056);
  call errcheck(1 NOT IN (NULL, 5), "1 NOT IN (NULL, 5)", 2057);
  call errcheck((SELECT NULL IS (1 NOT IN (NULL, 5))), "(SELECT NULL IS (1 NOT IN (NULL, 5)))", 2058); -- known sqlite and CQL IN difference for NULL
  call errcheck(NULL IS (NULL NOT IN (1)), "NULL IS (NULL NOT IN (1))", 2059); call errcheck((select NULL IS (NULL NOT IN (1))), "(select NULL IS (NULL NOT IN (1)))", 2059);

  declare x text;
  set x := NULL;

  call errcheck((x IN ("foo", "goo")) IS NULL, "(x IN (\"foo\", \"goo\")) IS NULL", 2064); call errcheck((select (x IN ("foo", "goo")) IS NULL), "(select (x IN (\"foo\", \"goo\")) IS NULL)", 2064);
  call errcheck((x NOT IN ("foo", "goo")) IS NULL, "(x NOT IN (\"foo\", \"goo\")) IS NULL", 2065); call errcheck((select (x NOT IN ("foo", "goo")) IS NULL), "(select (x NOT IN (\"foo\", \"goo\")) IS NULL)", 2065);

  -- Test IS TRUE and IS FALSE
  call errcheck(1 is true, "1 is true", 2068); call errcheck((select 1 is true), "(select 1 is true)", 2068);
  call errcheck(0 is false, "0 is false", 2069); call errcheck((select 0 is false), "(select 0 is false)", 2069);
  call errcheck(not 0 is true, "not 0 is true", 2070); call errcheck((select not 0 is true), "(select not 0 is true)", 2070);
  call errcheck(not 1 is false, "not 1 is false", 2071); call errcheck((select not 1 is false), "(select not 1 is false)", 2071);
  call errcheck(not null is false, "not null is false", 2072); call errcheck((select not null is false), "(select not null is false)", 2072);
  call errcheck(not null is true, "not null is true", 2073); call errcheck((select not null is true), "(select not null is true)", 2073);

  -- Test IS NOT TRUE and IS NOT FALSE
  call errcheck(not 1 is not true, "not 1 is not true", 2076); call errcheck((select not 1 is not true), "(select not 1 is not true)", 2076);
  call errcheck(not 0 is not false, "not 0 is not false", 2077); call errcheck((select not 0 is not false), "(select not 0 is not false)", 2077);
  call errcheck(0 is not true, "0 is not true", 2078); call errcheck((select 0 is not true), "(select 0 is not true)", 2078);
  call errcheck(1 is not false, "1 is not false", 2079); call errcheck((select 1 is not false), "(select 1 is not false)", 2079);
  call errcheck(null is not false, "null is not false", 2080); call errcheck((select null is not false), "(select null is not false)", 2080);
  call errcheck(null is not true, "null is not true", 2081); call errcheck((select null is not true), "(select null is not true)", 2081);

  -- priority of same
  call errcheck(not (1>=0 is false), "not (1>=0 is false)", 2084); call errcheck((select not (1>=0 is false)), "(select not (1>=0 is false))", 2084);
  call errcheck(not ((1>=0) is false), "not ((1>=0) is false)", 2085); call errcheck((select not ((1>=0) is false)), "(select not ((1>=0) is false))", 2085);
  call errcheck(1 >= (0 is false), "1 >= (0 is false)", 2086); call errcheck((select 1 >= (0 is false)), "(select 1 >= (0 is false))", 2086);

  call errcheck(-1 > -2 is not false, "-1 > -2 is not false", 2088); call errcheck((select -1 > -2 is not false), "(select -1 > -2 is not false)", 2088);
  call errcheck((-1 > -2) is not false, "(-1 > -2) is not false", 2089); call errcheck((select (-1 > -2) is not false), "(select (-1 > -2) is not false)", 2089);
  call errcheck(not -1 > (-2 is not false), "not -1 > (-2 is not false)", 2090); call errcheck((select not -1 > (-2 is not false)), "(select not -1 > (-2 is not false))", 2090);

  call errcheck(-1 > -2 is true, "-1 > -2 is true", 2092); call errcheck((select -1 > -2 is true), "(select -1 > -2 is true)", 2092);
  call errcheck((-1 > -2) is true, "(-1 > -2) is true", 2093); call errcheck((select (-1 > -2) is true), "(select (-1 > -2) is true)", 2093);
  call errcheck(not -1 > (-2 is true), "not -1 > (-2 is true)", 2094); call errcheck((select not -1 > (-2 is true)), "(select not -1 > (-2 is true))", 2094);

  call errcheck(-5 > -2 is not true, "-5 > -2 is not true", 2096); call errcheck((select -5 > -2 is not true), "(select -5 > -2 is not true)", 2096);
  call errcheck((-5 > -2) is not true, "(-5 > -2) is not true", 2097); call errcheck((select (-5 > -2) is not true), "(select (-5 > -2) is not true)", 2097);
  call errcheck(not -5 > (-2 is not true), "not -5 > (-2 is not true)", 2098); call errcheck((select not -5 > (-2 is not true)), "(select not -5 > (-2 is not true))", 2098);

  -- https:
  --
  -- sqlite> select false is true < false;
  -- 1
  -- sqlite> select sqlite_version();
  -- 3.32.3
  --
  -- vs.
  --
  -- PostgreSQL> select false is true < false;
  -- false
  --
  -- When CQL emits this operator, it naturally adds parens around (false is true)
  -- because is true binds weaker than < which ensures the "correct" eval order even
  -- though SQLite would do it the other way. CQL is like other SQL systems in that "is true"
  -- is an operator. In SQLite the way it works is that if the right operator of "IS" happens
  -- to the the literal "true" then you get "is true" behavior.
  -- This is wrong. And hard to emulate. CQL forces it the normal way with parens.
  -- SQLite will see "not ((false is true) < false)";
  --
  -- This may be fixed in future SQLites, but even if that happens the below will still pass.
  --
  call errcheck(not(false is true < false), "not(false is true < false)", 2122); call errcheck((select not(false is true < false)), "(select not(false is true < false))", 2122);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "equality_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "equality_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_equality_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "equality_pri", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_between_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- between is the same as = but binds left to right

  call errcheck(0 == (1=2 between 2 and 2), "0 == (1=2 between 2 and 2)", 2129); call errcheck((select 0 == (1=2 between 2 and 2)), "(select 0 == (1=2 between 2 and 2))", 2129);
  call errcheck(1 == (1=(2 between 2 and 2)), "1 == (1=(2 between 2 and 2))", 2130); call errcheck((select 1 == (1=(2 between 2 and 2))), "(select 1 == (1=(2 between 2 and 2)))", 2130);
  call errcheck(0 == ((1=2) between 2 and 2), "0 == ((1=2) between 2 and 2)", 2131); call errcheck((select 0 == ((1=2) between 2 and 2)), "(select 0 == ((1=2) between 2 and 2))", 2131);

  LET four := 4;

  -- verifying binding when = is on the right, still left to right
  call errcheck(0 == (0 between -2 and -1 = four), "0 == (0 between -2 and -1 = four)", 2136); call errcheck((select 0 == (0 between -2 and -1 = four)), "(select 0 == (0 between -2 and -1 = four))", 2136);
  call errcheck(0 == ((0 between -2 and -1) = four), "0 == ((0 between -2 and -1) = four)", 2137); call errcheck((select 0 == ((0 between -2 and -1) = four)), "(select 0 == ((0 between -2 and -1) = four))", 2137);
  call errcheck(1 == (0 between -2 and (-1 = four)), "1 == (0 between -2 and (-1 = four))", 2138); call errcheck((select 1 == (0 between -2 and (-1 = four))), "(select 1 == (0 between -2 and (-1 = four)))", 2138);

  -- not is weaker than between
  let neg := -1;

  call errcheck(0 == (not 0 between neg and 2), "0 == (not 0 between neg and 2)", 2143); call errcheck((select 0 == (not 0 between neg and 2)), "(select 0 == (not 0 between neg and 2))", 2143);
  call errcheck(1 == ((not 0) between neg and 2), "1 == ((not 0) between neg and 2)", 2144); call errcheck((select 1 == ((not 0) between neg and 2)), "(select 1 == ((not 0) between neg and 2))", 2144);
  call errcheck(0 == (not (0 between neg and 2)), "0 == (not (0 between neg and 2))", 2145); call errcheck((select 0 == (not (0 between neg and 2))), "(select 0 == (not (0 between neg and 2)))", 2145);

  -- between binds left to right
  call errcheck(0 == (0 between 0 and 3 between 2 and 3), "0 == (0 between 0 and 3 between 2 and 3)", 2148); call errcheck((select 0 == (0 between 0 and 3 between 2 and 3)), "(select 0 == (0 between 0 and 3 between 2 and 3))", 2148);
  call errcheck(0 == ((0 between 0 and 3) between 2 and 3), "0 == ((0 between 0 and 3) between 2 and 3)", 2149); call errcheck((select 0 == ((0 between 0 and 3) between 2 and 3)), "(select 0 == ((0 between 0 and 3) between 2 and 3))", 2149);
  call errcheck(1 == (0 between 0 and (3 between 2 and 3)), "1 == (0 between 0 and (3 between 2 and 3))", 2150); call errcheck((select 1 == (0 between 0 and (3 between 2 and 3))), "(select 1 == (0 between 0 and (3 between 2 and 3)))", 2150);

  -- nested betweens are actually not ambiguous
  call errcheck(1 == (0 between 1 between 3 and 4 and (3 between 2 and 3)), "1 == (0 between 1 between 3 and 4 and (3 between 2 and 3))", 2153); call errcheck((select 1 == (0 between 1 between 3 and 4 and (3 between 2 and 3))), "(select 1 == (0 between 1 between 3 and 4 and (3 between 2 and 3)))", 2153);
  call errcheck(1 == (0 between (1 between 3 and 4) and (3 between 2 and 3)), "1 == (0 between (1 between 3 and 4) and (3 between 2 and 3))", 2154); call errcheck((select 1 == (0 between (1 between 3 and 4) and (3 between 2 and 3))), "(select 1 == (0 between (1 between 3 and 4) and (3 between 2 and 3)))", 2154);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "between_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "between_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_between_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "between_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- AND tests with = == != <> IS IS_NOT IN NOT IN
create procedure test_and_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare null_ int;

  call errcheck(3 + 3 AND 5, "3 + 3 AND 5", 2162); call errcheck((select 3 + 3 AND 5), "(select 3 + 3 AND 5)", 2162);
  call errcheck((3 + 3 AND 0) == 0, "(3 + 3 AND 0) == 0", 2163); call errcheck((select (3 + 3 AND 0) == 0), "(select (3 + 3 AND 0) == 0)", 2163);
  call errcheck((NULL AND true) IS NULL, "(NULL AND true) IS NULL", 2164); call errcheck((select (NULL AND true) IS NULL), "(select (NULL AND true) IS NULL)", 2164);
  call errcheck((NULL AND true = null_) IS NULL, "(NULL AND true = null_) IS NULL", 2165); call errcheck((select (NULL AND true = null_) IS NULL), "(select (NULL AND true = null_) IS NULL)", 2165);
  call errcheck(NOT (NULL AND nullable(true) IS NULL), "NOT (NULL AND nullable(true) IS NULL)", 2166); call errcheck((select NOT (NULL AND nullable(true) IS NULL)), "(select NOT (NULL AND nullable(true) IS NULL))", 2166);
  call errcheck((NULL AND false) == 0, "(NULL AND false) == 0", 2167); call errcheck((select (NULL AND false) == 0), "(select (NULL AND false) == 0)", 2167);
  call errcheck(NOT (NULL AND false), "NOT (NULL AND false)", 2168); call errcheck((select NOT (NULL AND false)), "(select NOT (NULL AND false))", 2168);
  call errcheck(1 AND false == false, "1 AND false == false", 2169); call errcheck((select 1 AND false == false), "(select 1 AND false == false)", 2169);
  call errcheck(1 AND false = false, "1 AND false = false", 2170); call errcheck((select 1 AND false = false), "(select 1 AND false = false)", 2170);
  call errcheck(1 AND true != false, "1 AND true != false", 2171); call errcheck((select 1 AND true != false), "(select 1 AND true != false)", 2171);
  call errcheck(1 AND true <> false, "1 AND true <> false", 2172); call errcheck((select 1 AND true <> false), "(select 1 AND true <> false)", 2172);
  call errcheck(5 IS 5 AND 2 IS 2, "5 IS 5 AND 2 IS 2", 2173); call errcheck((select 5 IS 5 AND 2 IS 2), "(select 5 IS 5 AND 2 IS 2)", 2173);
  call errcheck(nullable(5) IS NOT NULL AND 2 IS 2, "nullable(5) IS NOT NULL AND 2 IS 2", 2174); call errcheck((select nullable(5) IS NOT NULL AND 2 IS 2), "(select nullable(5) IS NOT NULL AND 2 IS 2)", 2174);
  call errcheck(nullable(5) IS NOT NULL AND 2 IS 2, "nullable(5) IS NOT NULL AND 2 IS 2", 2175); call errcheck((select nullable(5) IS NOT NULL AND 2 IS 2), "(select nullable(5) IS NOT NULL AND 2 IS 2)", 2175);
  call errcheck(5 AND false + 1, "5 AND false + 1", 2176); call errcheck((select 5 AND false + 1), "(select 5 AND false + 1)", 2176);
  call errcheck(5 AND false * 1 + 1, "5 AND false * 1 + 1", 2177); call errcheck((select 5 AND false * 1 + 1), "(select 5 AND false * 1 + 1)", 2177);
  call errcheck(5 AND false >> 4 >= -1, "5 AND false >> 4 >= -1", 2178); call errcheck((select 5 AND false >> 4 >= -1), "(select 5 AND false >> 4 >= -1)", 2178);
  call errcheck(5 AND false | 4 & 12, "5 AND false | 4 & 12", 2179); call errcheck((select 5 AND false | 4 & 12), "(select 5 AND false | 4 & 12)", 2179);
  call errcheck(5 AND 6 / 3, "5 AND 6 / 3", 2180); call errcheck((select 5 AND 6 / 3), "(select 5 AND 6 / 3)", 2180);
  call errcheck((5 AND 25 % 5) == false, "(5 AND 25 % 5) == false", 2181); call errcheck((select (5 AND 25 % 5) == false), "(select (5 AND 25 % 5) == false)", 2181);
  call errcheck(5 AND false IN (0), "5 AND false IN (0)", 2182); call errcheck((select 5 AND false IN (0)), "(select 5 AND false IN (0))", 2182);
  call errcheck(5 AND true NOT IN (false), "5 AND true NOT IN (false)", 2183); call errcheck((select 5 AND true NOT IN (false)), "(select 5 AND true NOT IN (false))", 2183);
  call errcheck(NOT(5 AND false NOT IN (false)), "NOT(5 AND false NOT IN (false))", 2184); call errcheck((select NOT(5 AND false NOT IN (false))), "(select NOT(5 AND false NOT IN (false)))", 2184);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "and_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "and_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_and_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "and_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Test AND with OR
create procedure test_or_pri() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- The following tests show that if AND and OR were evaluated from
  -- left to right, then the output would be different
  call errcheck((0 OR 1 OR 1 AND 0 OR 0) != ((((0 OR 1) OR 1) AND 0) OR 0), "(0 OR 1 OR 1 AND 0 OR 0) != ((((0 OR 1) OR 1) AND 0) OR 0)", 2191); call errcheck((select (0 OR 1 OR 1 AND 0 OR 0) != ((((0 OR 1) OR 1) AND 0) OR 0)), "(select (0 OR 1 OR 1 AND 0 OR 0) != ((((0 OR 1) OR 1) AND 0) OR 0))", 2191);
  call errcheck((1 OR 1 AND 0 AND 1 AND 0) != ((((1 OR 1) AND 0) AND 1) AND 0), "(1 OR 1 AND 0 AND 1 AND 0) != ((((1 OR 1) AND 0) AND 1) AND 0)", 2192); call errcheck((select (1 OR 1 AND 0 AND 1 AND 0) != ((((1 OR 1) AND 0) AND 1) AND 0)), "(select (1 OR 1 AND 0 AND 1 AND 0) != ((((1 OR 1) AND 0) AND 1) AND 0))", 2192);
  call errcheck((0 OR 1 OR 1 AND 0 AND 1) != ((((0 OR 1) OR 1) AND 0) AND 1), "(0 OR 1 OR 1 AND 0 AND 1) != ((((0 OR 1) OR 1) AND 0) AND 1)", 2193); call errcheck((select (0 OR 1 OR 1 AND 0 AND 1) != ((((0 OR 1) OR 1) AND 0) AND 1)), "(select (0 OR 1 OR 1 AND 0 AND 1) != ((((0 OR 1) OR 1) AND 0) AND 1))", 2193);
  call errcheck((1 OR 1 OR 1 AND 0 AND 0) != ((((1 OR 1) OR 1) AND 0) AND 0), "(1 OR 1 OR 1 AND 0 AND 0) != ((((1 OR 1) OR 1) AND 0) AND 0)", 2194); call errcheck((select (1 OR 1 OR 1 AND 0 AND 0) != ((((1 OR 1) OR 1) AND 0) AND 0)), "(select (1 OR 1 OR 1 AND 0 AND 0) != ((((1 OR 1) OR 1) AND 0) AND 0))", 2194);
  call errcheck((1 OR 1 OR 1 AND 0 OR 0) != ((((1 OR 1) OR 1) AND 0) OR 0), "(1 OR 1 OR 1 AND 0 OR 0) != ((((1 OR 1) OR 1) AND 0) OR 0)", 2195); call errcheck((select (1 OR 1 OR 1 AND 0 OR 0) != ((((1 OR 1) OR 1) AND 0) OR 0)), "(select (1 OR 1 OR 1 AND 0 OR 0) != ((((1 OR 1) OR 1) AND 0) OR 0))", 2195);
  call errcheck((1 AND 1 AND 1 OR 1 AND 0) != ((((1 AND 1) AND 1) OR 1) AND 0), "(1 AND 1 AND 1 OR 1 AND 0) != ((((1 AND 1) AND 1) OR 1) AND 0)", 2196); call errcheck((select (1 AND 1 AND 1 OR 1 AND 0) != ((((1 AND 1) AND 1) OR 1) AND 0)), "(select (1 AND 1 AND 1 OR 1 AND 0) != ((((1 AND 1) AND 1) OR 1) AND 0))", 2196);
  call errcheck((1 OR 0 AND 0 AND 1 OR 0) != ((((1 OR 0) AND 0) AND 1) OR 0), "(1 OR 0 AND 0 AND 1 OR 0) != ((((1 OR 0) AND 0) AND 1) OR 0)", 2197); call errcheck((select (1 OR 0 AND 0 AND 1 OR 0) != ((((1 OR 0) AND 0) AND 1) OR 0)), "(select (1 OR 0 AND 0 AND 1 OR 0) != ((((1 OR 0) AND 0) AND 1) OR 0))", 2197);
  call errcheck((1 AND 1 OR 0 AND 0 AND 1) != ((((1 AND 1) OR 0) AND 0) AND 1), "(1 AND 1 OR 0 AND 0 AND 1) != ((((1 AND 1) OR 0) AND 0) AND 1)", 2198); call errcheck((select (1 AND 1 OR 0 AND 0 AND 1) != ((((1 AND 1) OR 0) AND 0) AND 1)), "(select (1 AND 1 OR 0 AND 0 AND 1) != ((((1 AND 1) OR 0) AND 0) AND 1))", 2198);
  call errcheck((1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0), "(1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0)", 2199); call errcheck((select (1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0)), "(select (1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0))", 2199);
  call errcheck((1 OR 0 AND 0 OR 1 AND 0) != ((((1 OR 0) AND 0) OR 1) AND 0), "(1 OR 0 AND 0 OR 1 AND 0) != ((((1 OR 0) AND 0) OR 1) AND 0)", 2200); call errcheck((select (1 OR 0 AND 0 OR 1 AND 0) != ((((1 OR 0) AND 0) OR 1) AND 0)), "(select (1 OR 0 AND 0 OR 1 AND 0) != ((((1 OR 0) AND 0) OR 1) AND 0))", 2200);
  call errcheck((1 OR 1 AND 1 AND 1 AND 0) != ((((1 OR 1) AND 1) AND 1) AND 0), "(1 OR 1 AND 1 AND 1 AND 0) != ((((1 OR 1) AND 1) AND 1) AND 0)", 2201); call errcheck((select (1 OR 1 AND 1 AND 1 AND 0) != ((((1 OR 1) AND 1) AND 1) AND 0)), "(select (1 OR 1 AND 1 AND 1 AND 0) != ((((1 OR 1) AND 1) AND 1) AND 0))", 2201);
  call errcheck((0 AND 0 OR 1 OR 0 AND 0) != ((((0 AND 0) OR 1) OR 0) AND 0), "(0 AND 0 OR 1 OR 0 AND 0) != ((((0 AND 0) OR 1) OR 0) AND 0)", 2202); call errcheck((select (0 AND 0 OR 1 OR 0 AND 0) != ((((0 AND 0) OR 1) OR 0) AND 0)), "(select (0 AND 0 OR 1 OR 0 AND 0) != ((((0 AND 0) OR 1) OR 0) AND 0))", 2202);
  call errcheck((0 OR 1 OR 1 AND 0 AND 0) != ((((0 OR 1) OR 1) AND 0) AND 0), "(0 OR 1 OR 1 AND 0 AND 0) != ((((0 OR 1) OR 1) AND 0) AND 0)", 2203); call errcheck((select (0 OR 1 OR 1 AND 0 AND 0) != ((((0 OR 1) OR 1) AND 0) AND 0)), "(select (0 OR 1 OR 1 AND 0 AND 0) != ((((0 OR 1) OR 1) AND 0) AND 0))", 2203);
  call errcheck((1 AND 1 AND 1 OR 0 AND 0) != ((((1 AND 1) AND 1) OR 0) AND 0), "(1 AND 1 AND 1 OR 0 AND 0) != ((((1 AND 1) AND 1) OR 0) AND 0)", 2204); call errcheck((select (1 AND 1 AND 1 OR 0 AND 0) != ((((1 AND 1) AND 1) OR 0) AND 0)), "(select (1 AND 1 AND 1 OR 0 AND 0) != ((((1 AND 1) AND 1) OR 0) AND 0))", 2204);
  call errcheck((1 OR 1 OR 1 AND 0 AND 1) != ((((1 OR 1) OR 1) AND 0) AND 1), "(1 OR 1 OR 1 AND 0 AND 1) != ((((1 OR 1) OR 1) AND 0) AND 1)", 2205); call errcheck((select (1 OR 1 OR 1 AND 0 AND 1) != ((((1 OR 1) OR 1) AND 0) AND 1)), "(select (1 OR 1 OR 1 AND 0 AND 1) != ((((1 OR 1) OR 1) AND 0) AND 1))", 2205);
  call errcheck((1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0), "(1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0)", 2206); call errcheck((select (1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0)), "(select (1 OR 0 OR 0 OR 0 AND 0) != ((((1 OR 0) OR 0) OR 0) AND 0))", 2206);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "or_pri")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "or_pri")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_or_pri(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "or_pri", start_refs, end_refs)); set fails := fails + 1; end if;

-- Take some priority tests and replace constants with nullable variables
create procedure test_nullable_test() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let x0 := nullable(0);
  let x1 := nullable(1);
  let x2 := nullable(2);
  let x3 := nullable(3);
  let x4 := nullable(4);
  let x5 := nullable(5);
  let x6 := nullable(6);
  let x7 := nullable(7);
  let x8 := nullable(8);
  let x9 := nullable(9);
  declare temp0 integer;
  declare temp1 integer;
  declare temp2 integer;
  declare temp3 integer;
  declare temp4 integer;

  set temp0 := nullable(27);
  call errcheck(x1+x2*x3+x4*x5 == temp0, "x1+x2*x3+x4*x5 == temp0", 2228); call errcheck((select x1+x2*x3+x4*x5 == temp0), "(select x1+x2*x3+x4*x5 == temp0)", 2228);
  call errcheck(x1+x2/x2 == x2, "x1+x2/x2 == x2", 2229); call errcheck((select x1+x2/x2 == x2), "(select x1+x2/x2 == x2)", 2229);
  call errcheck(x1+x2/x2*x4 == x5, "x1+x2/x2*x4 == x5", 2230); call errcheck((select x1+x2/x2*x4 == x5), "(select x1+x2/x2*x4 == x5)", 2230);
  call errcheck(x1+x2/x2*x4 == x5, "x1+x2/x2*x4 == x5", 2231); call errcheck((select x1+x2/x2*x4 == x5), "(select x1+x2/x2*x4 == x5)", 2231);
  call errcheck(x1*x2+x3 == x5, "x1*x2+x3 == x5", 2232); call errcheck((select x1*x2+x3 == x5), "(select x1*x2+x3 == x5)", 2232);
  call errcheck(x1*x2+x6/x3 == x4, "x1*x2+x6/x3 == x4", 2233); call errcheck((select x1*x2+x6/x3 == x4), "(select x1*x2+x6/x3 == x4)", 2233);
  call errcheck(x1*x2+x6/x3 == x4, "x1*x2+x6/x3 == x4", 2234); call errcheck((select x1*x2+x6/x3 == x4), "(select x1*x2+x6/x3 == x4)", 2234);
  set temp0 := nullable(25);
  call errcheck(x2*x3*x4+x3/x3 == temp0, "x2*x3*x4+x3/x3 == temp0", 2236); call errcheck((select x2*x3*x4+x3/x3 == temp0), "(select x2*x3*x4+x3/x3 == temp0)", 2236);
  set temp0 := nullable(-25);
  call errcheck(-x5*x5 == temp0, "-x5*x5 == temp0", 2238); call errcheck((select -x5*x5 == temp0), "(select -x5*x5 == temp0)", 2238);
  set temp0 := nullable(-20);
  call errcheck(x5-x5*x5 == temp0, "x5-x5*x5 == temp0", 2240); call errcheck((select x5-x5*x5 == temp0), "(select x5-x5*x5 == temp0)", 2240);
  set temp0 := nullable(29);
  call errcheck(x4+x5*x5 == temp0, "x4+x5*x5 == temp0", 2242); call errcheck((select x4+x5*x5 == temp0), "(select x4+x5*x5 == temp0)", 2242);
  set temp0 := nullable(25);
  call errcheck(x4*x5+x5 == temp0, "x4*x5+x5 == temp0", 2244); call errcheck((select x4*x5+x5 == temp0), "(select x4*x5+x5 == temp0)", 2244);
  set temp0 := nullable(15);
  call errcheck(x4*x4-x1 == temp0, "x4*x4-x1 == temp0", 2246); call errcheck((select x4*x4-x1 == temp0), "(select x4*x4-x1 == temp0)", 2246);
  set temp0 := nullable(10);
  call errcheck(10-x4*x2 == x2, "10-x4*x2 == x2", 2248); call errcheck((select 10-x4*x2 == x2), "(select 10-x4*x2 == x2)", 2248);

  set temp0 := nullable(10);

  set temp1 := nullable(40);
  call errcheck(temp0<<x1+x1 == temp1, "temp0<<x1+x1 == temp1", 2253); call errcheck((select temp0<<x1+x1 == temp1), "(select temp0<<x1+x1 == temp1)", 2253);
  set temp1 := nullable(22);
  call errcheck(x1+temp0<<x1 == temp1, "x1+temp0<<x1 == temp1", 2255); call errcheck((select x1+temp0<<x1 == temp1), "(select x1+temp0<<x1 == temp1)", 2255);
  call errcheck(temp0<<x1-x1 == temp0, "temp0<<x1-x1 == temp0", 2256); call errcheck((select temp0<<x1-x1 == temp0), "(select temp0<<x1-x1 == temp0)", 2256);
  set temp1 := nullable(80);
  call errcheck(temp0<<x4-x1 == temp1, "temp0<<x4-x1 == temp1", 2258); call errcheck((select temp0<<x4-x1 == temp1), "(select temp0<<x4-x1 == temp1)", 2258);
  set temp1 := nullable(18);
  call errcheck(temp0-x1<<x1 == temp1, "temp0-x1<<x1 == temp1", 2260); call errcheck((select temp0-x1<<x1 == temp1), "(select temp0-x1<<x1 == temp1)", 2260);

  call errcheck(temp0>>x3-x1 == x2, "temp0>>x3-x1 == x2", 2262); call errcheck((select temp0>>x3-x1 == x2), "(select temp0>>x3-x1 == x2)", 2262);
  set temp1 := nullable(11);
  call errcheck(temp1-x1>>x1 == x5, "temp1-x1>>x1 == x5", 2264); call errcheck((select temp1-x1>>x1 == x5), "(select temp1-x1>>x1 == x5)", 2264);
  call errcheck(temp0>>x1+x1 == x2, "temp0>>x1+x1 == x2", 2265); call errcheck((select temp0>>x1+x1 == x2), "(select temp0>>x1+x1 == x2)", 2265);
  call errcheck(x1+temp0>>x1 == x5, "x1+temp0>>x1 == x5", 2266); call errcheck((select x1+temp0>>x1 == x5), "(select x1+temp0>>x1 == x5)", 2266);

  call errcheck(temp0&x1+x1 == x2, "temp0&x1+x1 == x2", 2268); call errcheck((select temp0&x1+x1 == x2), "(select temp0&x1+x1 == x2)", 2268);
  call errcheck(x1+temp0&x1 == x1, "x1+temp0&x1 == x1", 2269); call errcheck((select x1+temp0&x1 == x1), "(select x1+temp0&x1 == x1)", 2269);
  call errcheck(x1+temp0&x7 == x3, "x1+temp0&x7 == x3", 2270); call errcheck((select x1+temp0&x7 == x3), "(select x1+temp0&x7 == x3)", 2270);
  call errcheck(temp0-x1&x7 == x1, "temp0-x1&x7 == x1", 2271); call errcheck((select temp0-x1&x7 == x1), "(select temp0-x1&x7 == x1)", 2271);
  call errcheck(temp0-x4&x7 == x6, "temp0-x4&x7 == x6", 2272); call errcheck((select temp0-x4&x7 == x6), "(select temp0-x4&x7 == x6)", 2272);

  call errcheck(temp0|x1+x1 == temp0, "temp0|x1+x1 == temp0", 2274); call errcheck((select temp0|x1+x1 == temp0), "(select temp0|x1+x1 == temp0)", 2274);
  set temp1 := nullable(14);
  call errcheck(temp0|x4 == temp1, "temp0|x4 == temp1", 2276); call errcheck((select temp0|x4 == temp1), "(select temp0|x4 == temp1)", 2276);
  set temp1 := nullable(15);
  call errcheck(x1+temp0|x4 == temp1, "x1+temp0|x4 == temp1", 2278); call errcheck((select x1+temp0|x4 == temp1), "(select x1+temp0|x4 == temp1)", 2278);
  call errcheck(temp0-x1|x7 == temp1, "temp0-x1|x7 == temp1", 2279); call errcheck((select temp0-x1|x7 == temp1), "(select temp0-x1|x7 == temp1)", 2279);
  call errcheck(temp0-x3|x7 == x7, "temp0-x3|x7 == x7", 2280); call errcheck((select temp0-x3|x7 == x7), "(select temp0-x3|x7 == x7)", 2280);

  set temp1 := nullable(12);

  call errcheck(x6&x4 == x4, "x6&x4 == x4", 2284); call errcheck((select x6&x4 == x4), "(select x6&x4 == x4)", 2284);
  call errcheck(x6&x4|temp1 == temp1, "x6&x4|temp1 == temp1", 2285); call errcheck((select x6&x4|temp1 == temp1), "(select x6&x4|temp1 == temp1)", 2285);
  set temp2 := nullable(14);
  call errcheck(x6&x4|temp1|x2 == temp2, "x6&x4|temp1|x2 == temp2", 2287); call errcheck((select x6&x4|temp1|x2 == temp2), "(select x6&x4|temp1|x2 == temp2)", 2287);
  call errcheck(x6&x4|temp1|x2|x2 == temp2, "x6&x4|temp1|x2|x2 == temp2", 2288); call errcheck((select x6&x4|temp1|x2|x2 == temp2), "(select x6&x4|temp1|x2|x2 == temp2)", 2288);
  set temp2 := nullable(112);
  call errcheck(x6&x4|temp1|x2|x2<<x3 == temp2, "x6&x4|temp1|x2|x2<<x3 == temp2", 2290); call errcheck((select x6&x4|temp1|x2|x2<<x3 == temp2), "(select x6&x4|temp1|x2|x2<<x3 == temp2)", 2290);
  set temp2 := nullable(56);
  call errcheck(x6&x4|temp1|x2|x2<<x3>>x3<<x2 == temp2, "x6&x4|temp1|x2|x2<<x3>>x3<<x2 == temp2", 2292); call errcheck((select x6&x4|temp1|x2|x2<<x3>>x3<<x2 == temp2), "(select x6&x4|temp1|x2|x2<<x3>>x3<<x2 == temp2)", 2292);

  call errcheck(temp0 < temp0<<x1, "temp0 < temp0<<x1", 2294); call errcheck((select temp0 < temp0<<x1), "(select temp0 < temp0<<x1)", 2294);
  call errcheck(temp0 <= temp0<<x1, "temp0 <= temp0<<x1", 2295); call errcheck((select temp0 <= temp0<<x1), "(select temp0 <= temp0<<x1)", 2295);
  set temp1 := nullable(31);
  call errcheck(x5 >= x0<<temp1, "x5 >= x0<<temp1", 2297); call errcheck((select x5 >= x0<<temp1), "(select x5 >= x0<<temp1)", 2297);
  call errcheck(x5 > x0<<temp1, "x5 > x0<<temp1", 2298); call errcheck((select x5 > x0<<temp1), "(select x5 > x0<<temp1)", 2298);
  set temp1 := nullable(16);
  call errcheck(temp1>>x1 >= x4<<x1, "temp1>>x1 >= x4<<x1", 2300); call errcheck((select temp1>>x1 >= x4<<x1), "(select temp1>>x1 >= x4<<x1)", 2300);
  call errcheck(x4<<x1 <= temp1>>x1, "x4<<x1 <= temp1>>x1", 2301); call errcheck((select x4<<x1 <= temp1>>x1), "(select x4<<x1 <= temp1>>x1)", 2301);
  call errcheck(temp1>>x1 > x3<<x1, "temp1>>x1 > x3<<x1", 2302); call errcheck((select temp1>>x1 > x3<<x1), "(select temp1>>x1 > x3<<x1)", 2302);
  call errcheck(temp1>>x1 >= x3<<x1, "temp1>>x1 >= x3<<x1", 2303); call errcheck((select temp1>>x1 >= x3<<x1), "(select temp1>>x1 >= x3<<x1)", 2303);
  call errcheck(temp1>>x1 <= x4<<x1, "temp1>>x1 <= x4<<x1", 2304); call errcheck((select temp1>>x1 <= x4<<x1), "(select temp1>>x1 <= x4<<x1)", 2304);

  call errcheck(temp1&x8 <= x4|x8, "temp1&x8 <= x4|x8", 2306); call errcheck((select temp1&x8 <= x4|x8), "(select temp1&x8 <= x4|x8)", 2306);
  set temp2 := nullable(15);
  call errcheck(temp1&8 < temp2, "temp1&8 < temp2", 2308); call errcheck((select temp1&8 < temp2), "(select temp1&8 < temp2)", 2308);
  call errcheck(x6 > x4|x5, "x6 > x4|x5", 2309); call errcheck((select x6 > x4|x5), "(select x6 > x4|x5)", 2309);
  call errcheck(x6 >= x4|x5, "x6 >= x4|x5", 2310); call errcheck((select x6 >= x4|x5), "(select x6 >= x4|x5)", 2310);

  call errcheck(x4&x5 <= x3|x4, "x4&x5 <= x3|x4", 2312); call errcheck((select x4&x5 <= x3|x4), "(select x4&x5 <= x3|x4)", 2312);
  call errcheck(x4&x5 < x3|x4, "x4&x5 < x3|x4", 2313); call errcheck((select x4&x5 < x3|x4), "(select x4&x5 < x3|x4)", 2313);
  call errcheck(x4|x3 <= x3|x4, "x4|x3 <= x3|x4", 2314); call errcheck((select x4|x3 <= x3|x4), "(select x4|x3 <= x3|x4)", 2314);
  call errcheck(x4&x5 <= x5&x4, "x4&x5 <= x5&x4", 2315); call errcheck((select x4&x5 <= x5&x4), "(select x4&x5 <= x5&x4)", 2315);
  call errcheck(x4&x5 >= x5&x4, "x4&x5 >= x5&x4", 2316); call errcheck((select x4&x5 >= x5&x4), "(select x4&x5 >= x5&x4)", 2316);

  call errcheck(x4&x5 >= x5&x4 > x0, "x4&x5 >= x5&x4 > x0", 2318); call errcheck((select x4&x5 >= x5&x4 > x0), "(select x4&x5 >= x5&x4 > x0)", 2318);
  call errcheck(x4&x5 >= x5&x4 <= x1, "x4&x5 >= x5&x4 <= x1", 2319); call errcheck((select x4&x5 >= x5&x4 <= x1), "(select x4&x5 >= x5&x4 <= x1)", 2319);
  call errcheck(x4&x5 >= x5&x4 >= x1, "x4&x5 >= x5&x4 >= x1", 2320); call errcheck((select x4&x5 >= x5&x4 >= x1), "(select x4&x5 >= x5&x4 >= x1)", 2320);
  set temp1 := nullable(100);
  call errcheck(x3&temp0 <= temp1 <= x3&x2, "x3&temp0 <= temp1 <= x3&x2", 2322); call errcheck((select x3&temp0 <= temp1 <= x3&x2), "(select x3&temp0 <= temp1 <= x3&x2)", 2322);
  call errcheck((x3&temp0 <= temp1) <= x3&x2 == x3&temp0 <= temp1 <= x3&x2, "(x3&temp0 <= temp1) <= x3&x2 == x3&temp0 <= temp1 <= x3&x2", 2323); call errcheck((select (x3&temp0 <= temp1) <= x3&x2 == x3&temp0 <= temp1 <= x3&x2), "(select (x3&temp0 <= temp1) <= x3&x2 == x3&temp0 <= temp1 <= x3&x2)", 2323);
  call errcheck(x5 > x3 > -x1 > x0, "x5 > x3 > -x1 > x0", 2324); call errcheck((select x5 > x3 > -x1 > x0), "(select x5 > x3 > -x1 > x0)", 2324);

  set temp1 := nullable(30);
  call errcheck(x5 == x5, "x5 == x5", 2327); call errcheck((select x5 == x5), "(select x5 == x5)", 2327);
  call errcheck(x5 < x6 == x6 > x5, "x5 < x6 == x6 > x5", 2328); call errcheck((select x5 < x6 == x6 > x5), "(select x5 < x6 == x6 > x5)", 2328);
  call errcheck(x5 <= x6 == x6 >= x5, "x5 <= x6 == x6 >= x5", 2329); call errcheck((select x5 <= x6 == x6 >= x5), "(select x5 <= x6 == x6 >= x5)", 2329);
  call errcheck(x5 < x6 == x6 >= x5, "x5 < x6 == x6 >= x5", 2330); call errcheck((select x5 < x6 == x6 >= x5), "(select x5 < x6 == x6 >= x5)", 2330);
  call errcheck(x5 <= x6 == x6 > x5, "x5 <= x6 == x6 > x5", 2331); call errcheck((select x5 <= x6 == x6 > x5), "(select x5 <= x6 == x6 > x5)", 2331);
  call errcheck(x5 <= x6 == x1, "x5 <= x6 == x1", 2332); call errcheck((select x5 <= x6 == x1), "(select x5 <= x6 == x1)", 2332);
  call errcheck(x1 == x5 < x6, "x1 == x5 < x6", 2333); call errcheck((select x1 == x5 < x6), "(select x1 == x5 < x6)", 2333);
  call errcheck(x1 == x5 <= x6, "x1 == x5 <= x6", 2334); call errcheck((select x1 == x5 <= x6), "(select x1 == x5 <= x6)", 2334);
  call errcheck(x1 == x0 + x1, "x1 == x0 + x1", 2335); call errcheck((select x1 == x0 + x1), "(select x1 == x0 + x1)", 2335);
  call errcheck(x1 == x1 + x0 * x1, "x1 == x1 + x0 * x1", 2336); call errcheck((select x1 == x1 + x0 * x1), "(select x1 == x1 + x0 * x1)", 2336);
  call errcheck(x1 == x0 * x1 + x1, "x1 == x0 * x1 + x1", 2337); call errcheck((select x1 == x0 * x1 + x1), "(select x1 == x0 * x1 + x1)", 2337);
  call errcheck(x1 == x0 * -x1 + x1, "x1 == x0 * -x1 + x1", 2338); call errcheck((select x1 == x0 * -x1 + x1), "(select x1 == x0 * -x1 + x1)", 2338);
  call errcheck(x1 + x1 == x3 - x1 == x1, "x1 + x1 == x3 - x1 == x1", 2339); call errcheck((select x1 + x1 == x3 - x1 == x1), "(select x1 + x1 == x3 - x1 == x1)", 2339);
  call errcheck(x1 + x1 == x3 - x1 != x0, "x1 + x1 == x3 - x1 != x0", 2340); call errcheck((select x1 + x1 == x3 - x1 != x0), "(select x1 + x1 == x3 - x1 != x0)", 2340);
  call errcheck(x1 + x1 == x3 - x1 != temp1, "x1 + x1 == x3 - x1 != temp1", 2341); call errcheck((select x1 + x1 == x3 - x1 != temp1), "(select x1 + x1 == x3 - x1 != temp1)", 2341);

  call errcheck(x5 = x5, "x5 = x5", 2343); call errcheck((select x5 = x5), "(select x5 = x5)", 2343);
  call errcheck(x5 < x6 = x6 > x5, "x5 < x6 = x6 > x5", 2344); call errcheck((select x5 < x6 = x6 > x5), "(select x5 < x6 = x6 > x5)", 2344);
  call errcheck(x5 <= x6 = x6 >= x5, "x5 <= x6 = x6 >= x5", 2345); call errcheck((select x5 <= x6 = x6 >= x5), "(select x5 <= x6 = x6 >= x5)", 2345);
  call errcheck(x5 < x6 = x6 >= x5, "x5 < x6 = x6 >= x5", 2346); call errcheck((select x5 < x6 = x6 >= x5), "(select x5 < x6 = x6 >= x5)", 2346);
  call errcheck(x5 <= x6 = x6 > x5, "x5 <= x6 = x6 > x5", 2347); call errcheck((select x5 <= x6 = x6 > x5), "(select x5 <= x6 = x6 > x5)", 2347);
  call errcheck(x5 <= x6 = x1, "x5 <= x6 = x1", 2348); call errcheck((select x5 <= x6 = x1), "(select x5 <= x6 = x1)", 2348);
  call errcheck(x1 = x5 < x6, "x1 = x5 < x6", 2349); call errcheck((select x1 = x5 < x6), "(select x1 = x5 < x6)", 2349);
  call errcheck(x1 = x5 <= x6, "x1 = x5 <= x6", 2350); call errcheck((select x1 = x5 <= x6), "(select x1 = x5 <= x6)", 2350);
  call errcheck(x1 = x0 + x1, "x1 = x0 + x1", 2351); call errcheck((select x1 = x0 + x1), "(select x1 = x0 + x1)", 2351);
  call errcheck(x1 = x1 + x0 * x1, "x1 = x1 + x0 * x1", 2352); call errcheck((select x1 = x1 + x0 * x1), "(select x1 = x1 + x0 * x1)", 2352);
  call errcheck(x1 = x0 * x1 + x1, "x1 = x0 * x1 + x1", 2353); call errcheck((select x1 = x0 * x1 + x1), "(select x1 = x0 * x1 + x1)", 2353);
  call errcheck(x1 = x0 * -x1 + x1, "x1 = x0 * -x1 + x1", 2354); call errcheck((select x1 = x0 * -x1 + x1), "(select x1 = x0 * -x1 + x1)", 2354);
  call errcheck(x1 + x1 = x3 - x1 = x1, "x1 + x1 = x3 - x1 = x1", 2355); call errcheck((select x1 + x1 = x3 - x1 = x1), "(select x1 + x1 = x3 - x1 = x1)", 2355);
  call errcheck(x1 + x1 = x3 - x1 <> x0, "x1 + x1 = x3 - x1 <> x0", 2356); call errcheck((select x1 + x1 = x3 - x1 <> x0), "(select x1 + x1 = x3 - x1 <> x0)", 2356);
  call errcheck(x1 + x1 == x3 - x1 <> x0, "x1 + x1 == x3 - x1 <> x0", 2357); call errcheck((select x1 + x1 == x3 - x1 <> x0), "(select x1 + x1 == x3 - x1 <> x0)", 2357);
  call errcheck(x1 + x1 = x3 - x1 <> temp1, "x1 + x1 = x3 - x1 <> temp1", 2358); call errcheck((select x1 + x1 = x3 - x1 <> temp1), "(select x1 + x1 = x3 - x1 <> temp1)", 2358);
  call errcheck(x1 + x1 == x3 - x1 <> temp1, "x1 + x1 == x3 - x1 <> temp1", 2359); call errcheck((select x1 + x1 == x3 - x1 <> temp1), "(select x1 + x1 == x3 - x1 <> temp1)", 2359);

  set temp1 := nullable(30);
  declare temp_null integer;
  set temp_null := NULL;

  call errcheck(x1 + x1 IS NULL == x0, "x1 + x1 IS NULL == x0", 2365); call errcheck((select x1 + x1 IS NULL == x0), "(select x1 + x1 IS NULL == x0)", 2365);
  call errcheck(x1 + x1 IS NOT NULL == x1, "x1 + x1 IS NOT NULL == x1", 2366); call errcheck((select x1 + x1 IS NOT NULL == x1), "(select x1 + x1 IS NOT NULL == x1)", 2366);
  call errcheck(x1 + x1 IS NULL + x1 == x0, "x1 + x1 IS NULL + x1 == x0", 2367); call errcheck((select x1 + x1 IS NULL + x1 == x0), "(select x1 + x1 IS NULL + x1 == x0)", 2367);
  call errcheck(x1 + x1 IS NOT NULL, "x1 + x1 IS NOT NULL", 2368); call errcheck((select x1 + x1 IS NOT NULL), "(select x1 + x1 IS NOT NULL)", 2368);
  call errcheck((x1 + x1 IS NOT NULL) + x1 == x2, "(x1 + x1 IS NOT NULL) + x1 == x2", 2369); call errcheck((select (x1 + x1 IS NOT NULL) + x1 == x2), "(select (x1 + x1 IS NOT NULL) + x1 == x2)", 2369);
  call errcheck(x1 + x1 IS NOT NULL + x1 == x1, "x1 + x1 IS NOT NULL + x1 == x1", 2370); call errcheck((select x1 + x1 IS NOT NULL + x1 == x1), "(select x1 + x1 IS NOT NULL + x1 == x1)", 2370);
  call errcheck(x1 + NULL IS NULL, "x1 + NULL IS NULL", 2371); call errcheck((select x1 + NULL IS NULL), "(select x1 + NULL IS NULL)", 2371);
  call errcheck(NULL + x1 IS NULL, "NULL + x1 IS NULL", 2372); call errcheck((select NULL + x1 IS NULL), "(select NULL + x1 IS NULL)", 2372);
  call errcheck(NULL * x1 IS NULL, "NULL * x1 IS NULL", 2373); call errcheck((select NULL * x1 IS NULL), "(select NULL * x1 IS NULL)", 2373);
  call errcheck(NULL * x0 IS NULL, "NULL * x0 IS NULL", 2374); call errcheck((select NULL * x0 IS NULL), "(select NULL * x0 IS NULL)", 2374);
  call errcheck(x0 * NULL * x0 IS NULL, "x0 * NULL * x0 IS NULL", 2375); call errcheck((select x0 * NULL * x0 IS NULL), "(select x0 * NULL * x0 IS NULL)", 2375);
  call errcheck(NULL > x0 IS NULL, "NULL > x0 IS NULL", 2376); call errcheck((select NULL > x0 IS NULL), "(select NULL > x0 IS NULL)", 2376);
  call errcheck(NULL >= x1 IS NULL, "NULL >= x1 IS NULL", 2377); call errcheck((select NULL >= x1 IS NULL), "(select NULL >= x1 IS NULL)", 2377);
  call errcheck(NULL < x2 IS NULL, "NULL < x2 IS NULL", 2378); call errcheck((select NULL < x2 IS NULL), "(select NULL < x2 IS NULL)", 2378);
  call errcheck(NULL <= x3 IS NULL, "NULL <= x3 IS NULL", 2379); call errcheck((select NULL <= x3 IS NULL), "(select NULL <= x3 IS NULL)", 2379);
  call errcheck(x1 + NULL == x3 IS NULL, "x1 + NULL == x3 IS NULL", 2380); call errcheck((select x1 + NULL == x3 IS NULL), "(select x1 + NULL == x3 IS NULL)", 2380);
  call errcheck(x1 + NULL != x3 IS NULL, "x1 + NULL != x3 IS NULL", 2381); call errcheck((select x1 + NULL != x3 IS NULL), "(select x1 + NULL != x3 IS NULL)", 2381);
  call errcheck(x1 + NULL <> x3 IS NULL, "x1 + NULL <> x3 IS NULL", 2382); call errcheck((select x1 + NULL <> x3 IS NULL), "(select x1 + NULL <> x3 IS NULL)", 2382);
  call errcheck(x1 = temp_null * x1 + x1 IS temp_null, "x1 = temp_null * x1 + x1 IS temp_null", 2383); call errcheck((select x1 = temp_null * x1 + x1 IS temp_null), "(select x1 = temp_null * x1 + x1 IS temp_null)", 2383);
  call errcheck(x1 = temp_null * -x1 + x1 IS temp_null, "x1 = temp_null * -x1 + x1 IS temp_null", 2384); call errcheck((select x1 = temp_null * -x1 + x1 IS temp_null), "(select x1 = temp_null * -x1 + x1 IS temp_null)", 2384);
  call errcheck(x1 + temp_null = x3 - x1 = x1 IS temp_null, "x1 + temp_null = x3 - x1 = x1 IS temp_null", 2385); call errcheck((select x1 + temp_null = x3 - x1 = x1 IS temp_null), "(select x1 + temp_null = x3 - x1 = x1 IS temp_null)", 2385);
  call errcheck(x1 + temp_null = x3 - x1 <> x0 IS temp_null, "x1 + temp_null = x3 - x1 <> x0 IS temp_null", 2386); call errcheck((select x1 + temp_null = x3 - x1 <> x0 IS temp_null), "(select x1 + temp_null = x3 - x1 <> x0 IS temp_null)", 2386);
  call errcheck(x1 + temp_null == x3 - x1 <> x0 IS temp_null, "x1 + temp_null == x3 - x1 <> x0 IS temp_null", 2387); call errcheck((select x1 + temp_null == x3 - x1 <> x0 IS temp_null), "(select x1 + temp_null == x3 - x1 <> x0 IS temp_null)", 2387);
  call errcheck(x1 + temp_null = x3 - x1 <> temp1 IS temp_null, "x1 + temp_null = x3 - x1 <> temp1 IS temp_null", 2388); call errcheck((select x1 + temp_null = x3 - x1 <> temp1 IS temp_null), "(select x1 + temp_null = x3 - x1 <> temp1 IS temp_null)", 2388);
  call errcheck(x1 + temp_null == x3 - x1 <> temp1 IS temp_null, "x1 + temp_null == x3 - x1 <> temp1 IS temp_null", 2389); call errcheck((select x1 + temp_null == x3 - x1 <> temp1 IS temp_null), "(select x1 + temp_null == x3 - x1 <> temp1 IS temp_null)", 2389);
  call errcheck((temp_null IS NOT temp_null) == x0, "(temp_null IS NOT temp_null) == x0", 2390); call errcheck((select (temp_null IS NOT temp_null) == x0), "(select (temp_null IS NOT temp_null) == x0)", 2390);
  call errcheck(x1 + x1 IS NOT temp_null, "x1 + x1 IS NOT temp_null", 2391); call errcheck((select x1 + x1 IS NOT temp_null), "(select x1 + x1 IS NOT temp_null)", 2391);
  call errcheck(temp_null == x3 IS temp_null, "temp_null == x3 IS temp_null", 2392); call errcheck((select temp_null == x3 IS temp_null), "(select temp_null == x3 IS temp_null)", 2392);
  call errcheck(((temp_null == x3) IS temp_null) == x1, "((temp_null == x3) IS temp_null) == x1", 2393); call errcheck((select ((temp_null == x3) IS temp_null) == x1), "(select ((temp_null == x3) IS temp_null) == x1)", 2393);
  call errcheck((temp_null == x3 IS temp_null) == x1, "(temp_null == x3 IS temp_null) == x1", 2394); call errcheck((select (temp_null == x3 IS temp_null) == x1), "(select (temp_null == x3 IS temp_null) == x1)", 2394);
  call errcheck((temp_null == x3 IS temp_null) == x1, "(temp_null == x3 IS temp_null) == x1", 2395); call errcheck((select (temp_null == x3 IS temp_null) == x1), "(select (temp_null == x3 IS temp_null) == x1)", 2395);
  call errcheck((temp_null == x3 IS temp_null) IS NOT temp_null, "(temp_null == x3 IS temp_null) IS NOT temp_null", 2396); call errcheck((select (temp_null == x3 IS temp_null) IS NOT temp_null), "(select (temp_null == x3 IS temp_null) IS NOT temp_null)", 2396);
  call errcheck((x1 + temp_null == x3 IS NOT temp_null) == x0, "(x1 + temp_null == x3 IS NOT temp_null) == x0", 2397); call errcheck((select (x1 + temp_null == x3 IS NOT temp_null) == x0), "(select (x1 + temp_null == x3 IS NOT temp_null) == x0)", 2397);
  call errcheck((x1 + temp_null = x3 - x1 <> x0 IS NOT temp_null) == x0, "(x1 + temp_null = x3 - x1 <> x0 IS NOT temp_null) == x0", 2398); call errcheck((select (x1 + temp_null = x3 - x1 <> x0 IS NOT temp_null) == x0), "(select (x1 + temp_null = x3 - x1 <> x0 IS NOT temp_null) == x0)", 2398);
  call errcheck((x1 + temp_null == x3 - x1 <> x0 IS NOT temp_null) == x0, "(x1 + temp_null == x3 - x1 <> x0 IS NOT temp_null) == x0", 2399); call errcheck((select (x1 + temp_null == x3 - x1 <> x0 IS NOT temp_null) == x0), "(select (x1 + temp_null == x3 - x1 <> x0 IS NOT temp_null) == x0)", 2399);
  call errcheck((x1 + temp_null = x3 - x1 <> temp1 IS NOT temp_null) == x0, "(x1 + temp_null = x3 - x1 <> temp1 IS NOT temp_null) == x0", 2400); call errcheck((select (x1 + temp_null = x3 - x1 <> temp1 IS NOT temp_null) == x0), "(select (x1 + temp_null = x3 - x1 <> temp1 IS NOT temp_null) == x0)", 2400);

  set temp0 := nullable(25);

  call errcheck(x2 * x3 IS x4 + x2, "x2 * x3 IS x4 + x2", 2404); call errcheck((select x2 * x3 IS x4 + x2), "(select x2 * x3 IS x4 + x2)", 2404);
  call errcheck(x2 * x3 IS x4 + x2, "x2 * x3 IS x4 + x2", 2405); call errcheck((select x2 * x3 IS x4 + x2), "(select x2 * x3 IS x4 + x2)", 2405);
  set temp1 := nullable(10);
  call errcheck(temp1-x4*x2 IS x2, "temp1-x4*x2 IS x2", 2407); call errcheck((select temp1-x4*x2 IS x2), "(select temp1-x4*x2 IS x2)", 2407);
  call errcheck(temp0%x3/x2 IS x0, "temp0%x3/x2 IS x0", 2408); call errcheck((select temp0%x3/x2 IS x0), "(select temp0%x3/x2 IS x0)", 2408);
  call errcheck(temp0/x5%x2 IS x1, "temp0/x5%x2 IS x1", 2409); call errcheck((select temp0/x5%x2 IS x1), "(select temp0/x5%x2 IS x1)", 2409);
  call errcheck(temp0*x5%x2 IS x1, "temp0*x5%x2 IS x1", 2410); call errcheck((select temp0*x5%x2 IS x1), "(select temp0*x5%x2 IS x1)", 2410);
  call errcheck(temp0*x5%x4%x2 IS x1, "temp0*x5%x4%x2 IS x1", 2411); call errcheck((select temp0*x5%x4%x2 IS x1), "(select temp0*x5%x4%x2 IS x1)", 2411);
  set temp1 := nullable(24);
  call errcheck(temp0-x5%x2 IS temp1, "temp0-x5%x2 IS temp1", 2413); call errcheck((select temp0-x5%x2 IS temp1), "(select temp0-x5%x2 IS temp1)", 2413);
  set temp1 := nullable(15);
  call errcheck(temp1%x3-x2 IS -x2, "temp1%x3-x2 IS -x2", 2415); call errcheck((select temp1%x3-x2 IS -x2), "(select temp1%x3-x2 IS -x2)", 2415);
  set temp2 := nullable(30);
  set temp3 := nullable(13);
  call errcheck(temp1-temp2%x4 IS temp3, "temp1-temp2%x4 IS temp3", 2418); call errcheck((select temp1-temp2%x4 IS temp3), "(select temp1-temp2%x4 IS temp3)", 2418);
  call errcheck(temp1-temp2/x2 IS x0, "temp1-temp2/x2 IS x0", 2419); call errcheck((select temp1-temp2/x2 IS x0), "(select temp1-temp2/x2 IS x0)", 2419);
  call errcheck(temp1/x5-x3 IS x0, "temp1/x5-x3 IS x0", 2420); call errcheck((select temp1/x5-x3 IS x0), "(select temp1/x5-x3 IS x0)", 2420);
  set temp3 := nullable(72);
  call errcheck(temp1*x5-x3 IS temp3, "temp1*x5-x3 IS temp3", 2422); call errcheck((select temp1*x5-x3 IS temp3), "(select temp1*x5-x3 IS temp3)", 2422);
  set temp3 := nullable(22);
  call errcheck(x5*x5-x3 IS temp3, "x5*x5-x3 IS temp3", 2424); call errcheck((select x5*x5-x3 IS temp3), "(select x5*x5-x3 IS temp3)", 2424);
  set temp3 := 26;
  call errcheck(temp0+x5%x2 IS temp3, "temp0+x5%x2 IS temp3", 2426); call errcheck((select temp0+x5%x2 IS temp3), "(select temp0+x5%x2 IS temp3)", 2426);
  call errcheck(temp1%x3+x2 IS x2, "temp1%x3+x2 IS x2", 2427); call errcheck((select temp1%x3+x2 IS x2), "(select temp1%x3+x2 IS x2)", 2427);
  set temp1 := nullable(17);
  set temp2 := nullable(30);
  set temp3 := nullable(15);
  call errcheck(temp3+temp2%x4 IS temp1, "temp3+temp2%x4 IS temp1", 2431); call errcheck((select temp3+temp2%x4 IS temp1), "(select temp3+temp2%x4 IS temp1)", 2431);
  set temp1 := nullable(30);
  call errcheck(temp3+temp1/x2 IS temp1, "temp3+temp1/x2 IS temp1", 2433); call errcheck((select temp3+temp1/x2 IS temp1), "(select temp3+temp1/x2 IS temp1)", 2433);
  call errcheck(temp3/x5+x3 IS x6, "temp3/x5+x3 IS x6", 2434); call errcheck((select temp3/x5+x3 IS x6), "(select temp3/x5+x3 IS x6)", 2434);
  set temp1 := nullable(78);
  call errcheck(temp3*x5+x3 IS temp1, "temp3*x5+x3 IS temp1", 2436); call errcheck((select temp3*x5+x3 IS temp1), "(select temp3*x5+x3 IS temp1)", 2436);
  set temp1 := nullable(28);
  call errcheck(x5*x5+x3 IS temp1, "x5*x5+x3 IS temp1", 2438); call errcheck((select x5*x5+x3 IS temp1), "(select x5*x5+x3 IS temp1)", 2438);
  set temp1 := nullable(20);
  set temp2 := nullable(12);
  call errcheck(x5*temp2/x3 IS temp1, "x5*temp2/x3 IS temp1", 2441); call errcheck((select x5*temp2/x3 IS temp1), "(select x5*temp2/x3 IS temp1)", 2441);
  call errcheck(x5*temp2/x3%x7 IS x6, "x5*temp2/x3%x7 IS x6", 2442); call errcheck((select x5*temp2/x3%x7 IS x6), "(select x5*temp2/x3%x7 IS x6)", 2442);
  set temp1 := nullable(21);
  set temp2 := nullable(12);
  call errcheck(x9%temp2/x3*x7 IS temp1, "x9%temp2/x3*x7 IS temp1", 2445); call errcheck((select x9%temp2/x3*x7 IS temp1), "(select x9%temp2/x3*x7 IS temp1)", 2445);

  call errcheck(x1 IS x1 == x1 IS x1 == x1, "x1 IS x1 == x1 IS x1 == x1", 2447); call errcheck((select x1 IS x1 == x1 IS x1 == x1), "(select x1 IS x1 == x1 IS x1 == x1)", 2447);
  call errcheck(x5 > x6 IS x2 < x1, "x5 > x6 IS x2 < x1", 2448); call errcheck((select x5 > x6 IS x2 < x1), "(select x5 > x6 IS x2 < x1)", 2448);
  call errcheck(x5 <= x6 IS x2 > x1, "x5 <= x6 IS x2 > x1", 2449); call errcheck((select x5 <= x6 IS x2 > x1), "(select x5 <= x6 IS x2 > x1)", 2449);
  call errcheck(x5 == x5 IS x2 > x1, "x5 == x5 IS x2 > x1", 2450); call errcheck((select x5 == x5 IS x2 > x1), "(select x5 == x5 IS x2 > x1)", 2450);
  call errcheck(NULL IS NULL, "NULL IS NULL", 2451); call errcheck((select NULL IS NULL), "(select NULL IS NULL)", 2451);
  call errcheck(temp_null == x0 IS NULL, "temp_null == x0 IS NULL", 2452); call errcheck((select temp_null == x0 IS NULL), "(select temp_null == x0 IS NULL)", 2452);
  call errcheck(NULL IS NULL == x1 != x0, "NULL IS NULL == x1 != x0", 2453); call errcheck((select NULL IS NULL == x1 != x0), "(select NULL IS NULL == x1 != x0)", 2453);
  call errcheck(NULL IS NULL = x1 <> x0, "NULL IS NULL = x1 <> x0", 2454); call errcheck((select NULL IS NULL = x1 <> x0), "(select NULL IS NULL = x1 <> x0)", 2454);
  call errcheck(temp_null == temp_null IS NULL, "temp_null == temp_null IS NULL", 2455); call errcheck((select temp_null == temp_null IS NULL), "(select temp_null == temp_null IS NULL)", 2455);
  call errcheck(NULL IS (temp_null == x0), "NULL IS (temp_null == x0)", 2456); call errcheck((select NULL IS (temp_null == x0)), "(select NULL IS (temp_null == x0))", 2456);
  call errcheck(NULL IS NOT NULL == x0, "NULL IS NOT NULL == x0", 2457); call errcheck((select NULL IS NOT NULL == x0), "(select NULL IS NOT NULL == x0)", 2457);
  call errcheck((NULL IS NOT NULL) == x0, "(NULL IS NOT NULL) == x0", 2458); call errcheck((select (NULL IS NOT NULL) == x0), "(select (NULL IS NOT NULL) == x0)", 2458);
  call errcheck(x5 > x2 IS NOT NULL, "x5 > x2 IS NOT NULL", 2459); call errcheck((select x5 > x2 IS NOT NULL), "(select x5 > x2 IS NOT NULL)", 2459);
  call errcheck(NULL IS NOT x2 < x3, "NULL IS NOT x2 < x3", 2460); call errcheck((select NULL IS NOT x2 < x3), "(select NULL IS NOT x2 < x3)", 2460);
  call errcheck(NULL IS NULL + x1, "NULL IS NULL + x1", 2461); call errcheck((select NULL IS NULL + x1), "(select NULL IS NULL + x1)", 2461);
  call errcheck(NULL IS x1 + NULL, "NULL IS x1 + NULL", 2462); call errcheck((select NULL IS x1 + NULL), "(select NULL IS x1 + NULL)", 2462);
  call errcheck(NULL IS x1 << NULL, "NULL IS x1 << NULL", 2463); call errcheck((select NULL IS x1 << NULL), "(select NULL IS x1 << NULL)", 2463);

  let one := nullable("1");
  let two := nullable("2");
  call errcheck(one IS two == x0, "one IS two == x0", 2467); call errcheck((select one IS two == x0), "(select one IS two == x0)", 2467);
  call errcheck(one IS NULL == x0, "one IS NULL == x0", 2468); call errcheck((select one IS NULL == x0), "(select one IS NULL == x0)", 2468);
  call errcheck(NULL IS one == x0, "NULL IS one == x0", 2469); call errcheck((select NULL IS one == x0), "(select NULL IS one == x0)", 2469);

  -- Test IN
  call errcheck(x3 IN (x1, x2) == x0, "x3 IN (x1, x2) == x0", 2472); call errcheck((select x3 IN (x1, x2) == x0), "(select x3 IN (x1, x2) == x0)", 2472);
  call errcheck(x3 + x2 IN (x1, x5), "x3 + x2 IN (x1, x5)", 2473); call errcheck((select x3 + x2 IN (x1, x5)), "(select x3 + x2 IN (x1, x5))", 2473);
  call errcheck(x3 / x3 IN (x1, x2), "x3 / x3 IN (x1, x2)", 2474); call errcheck((select x3 / x3 IN (x1, x2)), "(select x3 / x3 IN (x1, x2))", 2474);
  call errcheck(x3 / x3 IN (x1, x2) IN (x1), "x3 / x3 IN (x1, x2) IN (x1)", 2475); call errcheck((select x3 / x3 IN (x1, x2) IN (x1)), "(select x3 / x3 IN (x1, x2) IN (x1))", 2475);
  call errcheck(x1 IN (NULL, x1), "x1 IN (NULL, x1)", 2476); call errcheck((select x1 IN (NULL, x1)), "(select x1 IN (NULL, x1))", 2476);
  call errcheck(NOT (x1 IN (NULL, x5)), "NOT (x1 IN (NULL, x5))", 2477); -- known difference between CQL and SQLite in IN
  call errcheck(NULL IS (NULL IN (x1)), "NULL IS (NULL IN (x1))", 2478); call errcheck((select NULL IS (NULL IN (x1))), "(select NULL IS (NULL IN (x1)))", 2478);

  -- Test NOT IN
  call errcheck(x1 NOT IN (x1, x2) == x0, "x1 NOT IN (x1, x2) == x0", 2481); call errcheck((select x1 NOT IN (x1, x2) == x0), "(select x1 NOT IN (x1, x2) == x0)", 2481);
  call errcheck(x3 NOT IN (x1, x2) == x1, "x3 NOT IN (x1, x2) == x1", 2482); call errcheck((select x3 NOT IN (x1, x2) == x1), "(select x3 NOT IN (x1, x2) == x1)", 2482);
  call errcheck(x3 + x2 NOT IN (x1, x2), "x3 + x2 NOT IN (x1, x2)", 2483); call errcheck((select x3 + x2 NOT IN (x1, x2)), "(select x3 + x2 NOT IN (x1, x2))", 2483);
  call errcheck(x3 / x1 NOT IN (x1, x2), "x3 / x1 NOT IN (x1, x2)", 2484); call errcheck((select x3 / x1 NOT IN (x1, x2)), "(select x3 / x1 NOT IN (x1, x2))", 2484);
  call errcheck(x3 / x1 NOT IN (x1, x2) IN (x1), "x3 / x1 NOT IN (x1, x2) IN (x1)", 2485); call errcheck((select x3 / x1 NOT IN (x1, x2) IN (x1)), "(select x3 / x1 NOT IN (x1, x2) IN (x1))", 2485);
  call errcheck(NOT (x1 NOT IN (NULL, x1)), "NOT (x1 NOT IN (NULL, x1))", 2486); call errcheck((select NOT (x1 NOT IN (NULL, x1))), "(select NOT (x1 NOT IN (NULL, x1)))", 2486);
  call errcheck(x1 NOT IN (NULL, x5), "x1 NOT IN (NULL, x5)", 2487); -- known difference between CQL and SQLite in IN
  call errcheck(NULL IS (NULL NOT IN (x1)), "NULL IS (NULL NOT IN (x1))", 2488); call errcheck((select NULL IS (NULL NOT IN (x1))), "(select NULL IS (NULL NOT IN (x1)))", 2488);

  declare x text;
  set x := NULL;
  call errcheck((x IN ("foo", "goo")) IS NULL, "(x IN (\"foo\", \"goo\")) IS NULL", 2492); call errcheck((select (x IN ("foo", "goo")) IS NULL), "(select (x IN (\"foo\", \"goo\")) IS NULL)", 2492);
  call errcheck((x NOT IN ("foo", "goo")) IS NULL, "(x NOT IN (\"foo\", \"goo\")) IS NULL", 2493); call errcheck((select (x NOT IN ("foo", "goo")) IS NULL), "(select (x NOT IN (\"foo\", \"goo\")) IS NULL)", 2493);

  call errcheck(x3 + x3 AND x5, "x3 + x3 AND x5", 2495); call errcheck((select x3 + x3 AND x5), "(select x3 + x3 AND x5)", 2495);
  call errcheck((x3 + x3 AND x0) == x0, "(x3 + x3 AND x0) == x0", 2496); call errcheck((select (x3 + x3 AND x0) == x0), "(select (x3 + x3 AND x0) == x0)", 2496);
  call errcheck((NULL AND x1) IS NULL, "(NULL AND x1) IS NULL", 2497); call errcheck((select (NULL AND x1) IS NULL), "(select (NULL AND x1) IS NULL)", 2497);
  call errcheck((NULL AND x1 = temp_null) IS NULL, "(NULL AND x1 = temp_null) IS NULL", 2498); call errcheck((select (NULL AND x1 = temp_null) IS NULL), "(select (NULL AND x1 = temp_null) IS NULL)", 2498);
  call errcheck(NOT (NULL AND x1 IS NULL), "NOT (NULL AND x1 IS NULL)", 2499); call errcheck((select NOT (NULL AND x1 IS NULL)), "(select NOT (NULL AND x1 IS NULL))", 2499);
  call errcheck((NULL AND x0) == x0, "(NULL AND x0) == x0", 2500); call errcheck((select (NULL AND x0) == x0), "(select (NULL AND x0) == x0)", 2500);
  call errcheck(NOT (NULL AND x0), "NOT (NULL AND x0)", 2501); call errcheck((select NOT (NULL AND x0)), "(select NOT (NULL AND x0))", 2501);
  call errcheck(x1 AND x0 == x0, "x1 AND x0 == x0", 2502); call errcheck((select x1 AND x0 == x0), "(select x1 AND x0 == x0)", 2502);
  call errcheck(x1 AND x0 = x0, "x1 AND x0 = x0", 2503); call errcheck((select x1 AND x0 = x0), "(select x1 AND x0 = x0)", 2503);
  call errcheck(x1 AND x1 != x0, "x1 AND x1 != x0", 2504); call errcheck((select x1 AND x1 != x0), "(select x1 AND x1 != x0)", 2504);
  call errcheck(x1 AND x1 <> x0, "x1 AND x1 <> x0", 2505); call errcheck((select x1 AND x1 <> x0), "(select x1 AND x1 <> x0)", 2505);
  call errcheck(x5 IS x5 AND x2 IS x2, "x5 IS x5 AND x2 IS x2", 2506); call errcheck((select x5 IS x5 AND x2 IS x2), "(select x5 IS x5 AND x2 IS x2)", 2506);
  call errcheck(x5 IS NOT NULL AND x2 IS x2, "x5 IS NOT NULL AND x2 IS x2", 2507); call errcheck((select x5 IS NOT NULL AND x2 IS x2), "(select x5 IS NOT NULL AND x2 IS x2)", 2507);
  call errcheck(x5 IS NOT NULL AND x2 IS x2, "x5 IS NOT NULL AND x2 IS x2", 2508); call errcheck((select x5 IS NOT NULL AND x2 IS x2), "(select x5 IS NOT NULL AND x2 IS x2)", 2508);
  call errcheck(x5 AND x0 + x1, "x5 AND x0 + x1", 2509); call errcheck((select x5 AND x0 + x1), "(select x5 AND x0 + x1)", 2509);
  call errcheck(x5 AND x0 * x1 + x1, "x5 AND x0 * x1 + x1", 2510); call errcheck((select x5 AND x0 * x1 + x1), "(select x5 AND x0 * x1 + x1)", 2510);
  call errcheck(x5 AND x0 >> x4 >= -x1, "x5 AND x0 >> x4 >= -x1", 2511); call errcheck((select x5 AND x0 >> x4 >= -x1), "(select x5 AND x0 >> x4 >= -x1)", 2511);
  set temp1 := nullable(12);
  call errcheck(x5 AND x0 | x4 & temp1, "x5 AND x0 | x4 & temp1", 2513); call errcheck((select x5 AND x0 | x4 & temp1), "(select x5 AND x0 | x4 & temp1)", 2513);
  call errcheck(x5 AND x6 / x3, "x5 AND x6 / x3", 2514); call errcheck((select x5 AND x6 / x3), "(select x5 AND x6 / x3)", 2514);
  set temp1 := nullable(25);
  call errcheck((x5 AND temp1 % x5) == x0, "(x5 AND temp1 % x5) == x0", 2516); call errcheck((select (x5 AND temp1 % x5) == x0), "(select (x5 AND temp1 % x5) == x0)", 2516);
  call errcheck(x5 AND x0 IN (x0), "x5 AND x0 IN (x0)", 2517); call errcheck((select x5 AND x0 IN (x0)), "(select x5 AND x0 IN (x0))", 2517);

  call errcheck((x0 OR x1 OR x1 AND x0 OR x0) != ((((x0 OR x1) OR x1) AND x0) OR x0), "(x0 OR x1 OR x1 AND x0 OR x0) != ((((x0 OR x1) OR x1) AND x0) OR x0)", 2519); call errcheck((select (x0 OR x1 OR x1 AND x0 OR x0) != ((((x0 OR x1) OR x1) AND x0) OR x0)), "(select (x0 OR x1 OR x1 AND x0 OR x0) != ((((x0 OR x1) OR x1) AND x0) OR x0))", 2519);
  call errcheck((x1 OR x1 AND x0 AND x1 AND x0) != ((((x1 OR x1) AND x0) AND x1) AND x0), "(x1 OR x1 AND x0 AND x1 AND x0) != ((((x1 OR x1) AND x0) AND x1) AND x0)", 2520); call errcheck((select (x1 OR x1 AND x0 AND x1 AND x0) != ((((x1 OR x1) AND x0) AND x1) AND x0)), "(select (x1 OR x1 AND x0 AND x1 AND x0) != ((((x1 OR x1) AND x0) AND x1) AND x0))", 2520);
  call errcheck((x0 OR x1 OR x1 AND x0 AND x1) != ((((x0 OR x1) OR x1) AND x0) AND x1), "(x0 OR x1 OR x1 AND x0 AND x1) != ((((x0 OR x1) OR x1) AND x0) AND x1)", 2521); call errcheck((select (x0 OR x1 OR x1 AND x0 AND x1) != ((((x0 OR x1) OR x1) AND x0) AND x1)), "(select (x0 OR x1 OR x1 AND x0 AND x1) != ((((x0 OR x1) OR x1) AND x0) AND x1))", 2521);
  call errcheck((x1 OR x1 OR x1 AND x0 AND x0) != ((((x1 OR x1) OR x1) AND x0) AND x0), "(x1 OR x1 OR x1 AND x0 AND x0) != ((((x1 OR x1) OR x1) AND x0) AND x0)", 2522); call errcheck((select (x1 OR x1 OR x1 AND x0 AND x0) != ((((x1 OR x1) OR x1) AND x0) AND x0)), "(select (x1 OR x1 OR x1 AND x0 AND x0) != ((((x1 OR x1) OR x1) AND x0) AND x0))", 2522);
  call errcheck((x1 OR x1 OR x1 AND x0 OR x0) != ((((x1 OR x1) OR x1) AND x0) OR x0), "(x1 OR x1 OR x1 AND x0 OR x0) != ((((x1 OR x1) OR x1) AND x0) OR x0)", 2523); call errcheck((select (x1 OR x1 OR x1 AND x0 OR x0) != ((((x1 OR x1) OR x1) AND x0) OR x0)), "(select (x1 OR x1 OR x1 AND x0 OR x0) != ((((x1 OR x1) OR x1) AND x0) OR x0))", 2523);
  call errcheck((x1 AND x1 AND x1 OR x1 AND x0) != ((((x1 AND x1) AND x1) OR x1) AND x0), "(x1 AND x1 AND x1 OR x1 AND x0) != ((((x1 AND x1) AND x1) OR x1) AND x0)", 2524); call errcheck((select (x1 AND x1 AND x1 OR x1 AND x0) != ((((x1 AND x1) AND x1) OR x1) AND x0)), "(select (x1 AND x1 AND x1 OR x1 AND x0) != ((((x1 AND x1) AND x1) OR x1) AND x0))", 2524);
  call errcheck((x1 OR x0 AND x0 AND x1 OR x0) != ((((x1 OR x0) AND x0) AND x1) OR x0), "(x1 OR x0 AND x0 AND x1 OR x0) != ((((x1 OR x0) AND x0) AND x1) OR x0)", 2525); call errcheck((select (x1 OR x0 AND x0 AND x1 OR x0) != ((((x1 OR x0) AND x0) AND x1) OR x0)), "(select (x1 OR x0 AND x0 AND x1 OR x0) != ((((x1 OR x0) AND x0) AND x1) OR x0))", 2525);
  call errcheck((x1 AND x1 OR x0 AND x0 AND x1) != ((((x1 AND x1) OR x0) AND x0) AND x1), "(x1 AND x1 OR x0 AND x0 AND x1) != ((((x1 AND x1) OR x0) AND x0) AND x1)", 2526); call errcheck((select (x1 AND x1 OR x0 AND x0 AND x1) != ((((x1 AND x1) OR x0) AND x0) AND x1)), "(select (x1 AND x1 OR x0 AND x0 AND x1) != ((((x1 AND x1) OR x0) AND x0) AND x1))", 2526);
  call errcheck((x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0), "(x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0)", 2527); call errcheck((select (x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0)), "(select (x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0))", 2527);
  call errcheck((x1 OR x0 AND x0 OR x1 AND x0) != ((((x1 OR x0) AND x0) OR x1) AND x0), "(x1 OR x0 AND x0 OR x1 AND x0) != ((((x1 OR x0) AND x0) OR x1) AND x0)", 2528); call errcheck((select (x1 OR x0 AND x0 OR x1 AND x0) != ((((x1 OR x0) AND x0) OR x1) AND x0)), "(select (x1 OR x0 AND x0 OR x1 AND x0) != ((((x1 OR x0) AND x0) OR x1) AND x0))", 2528);
  call errcheck((x1 OR x1 AND x1 AND x1 AND x0) != ((((x1 OR x1) AND x1) AND x1) AND x0), "(x1 OR x1 AND x1 AND x1 AND x0) != ((((x1 OR x1) AND x1) AND x1) AND x0)", 2529); call errcheck((select (x1 OR x1 AND x1 AND x1 AND x0) != ((((x1 OR x1) AND x1) AND x1) AND x0)), "(select (x1 OR x1 AND x1 AND x1 AND x0) != ((((x1 OR x1) AND x1) AND x1) AND x0))", 2529);
  call errcheck((x0 AND x0 OR x1 OR x0 AND x0) != ((((x0 AND x0) OR x1) OR x0) AND x0), "(x0 AND x0 OR x1 OR x0 AND x0) != ((((x0 AND x0) OR x1) OR x0) AND x0)", 2530); call errcheck((select (x0 AND x0 OR x1 OR x0 AND x0) != ((((x0 AND x0) OR x1) OR x0) AND x0)), "(select (x0 AND x0 OR x1 OR x0 AND x0) != ((((x0 AND x0) OR x1) OR x0) AND x0))", 2530);
  call errcheck((x0 OR x1 OR x1 AND x0 AND x0) != ((((x0 OR x1) OR x1) AND x0) AND x0), "(x0 OR x1 OR x1 AND x0 AND x0) != ((((x0 OR x1) OR x1) AND x0) AND x0)", 2531); call errcheck((select (x0 OR x1 OR x1 AND x0 AND x0) != ((((x0 OR x1) OR x1) AND x0) AND x0)), "(select (x0 OR x1 OR x1 AND x0 AND x0) != ((((x0 OR x1) OR x1) AND x0) AND x0))", 2531);
  call errcheck((x1 AND x1 AND x1 OR x0 AND x0) != ((((x1 AND x1) AND x1) OR x0) AND x0), "(x1 AND x1 AND x1 OR x0 AND x0) != ((((x1 AND x1) AND x1) OR x0) AND x0)", 2532); call errcheck((select (x1 AND x1 AND x1 OR x0 AND x0) != ((((x1 AND x1) AND x1) OR x0) AND x0)), "(select (x1 AND x1 AND x1 OR x0 AND x0) != ((((x1 AND x1) AND x1) OR x0) AND x0))", 2532);
  call errcheck((x1 OR x1 OR x1 AND x0 AND x1) != ((((x1 OR x1) OR x1) AND x0) AND x1), "(x1 OR x1 OR x1 AND x0 AND x1) != ((((x1 OR x1) OR x1) AND x0) AND x1)", 2533); call errcheck((select (x1 OR x1 OR x1 AND x0 AND x1) != ((((x1 OR x1) OR x1) AND x0) AND x1)), "(select (x1 OR x1 OR x1 AND x0 AND x1) != ((((x1 OR x1) OR x1) AND x0) AND x1))", 2533);
  call errcheck((x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0), "(x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0)", 2534); call errcheck((select (x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0)), "(select (x1 OR x0 OR x0 OR x0 AND x0) != ((((x1 OR x0) OR x0) OR x0) AND x0))", 2534);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "nullable_test")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "nullable_test")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_nullable_test(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "nullable_test", start_refs, end_refs)); set fails := fails + 1; end if;

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
    FALSE, 0, 0, 0.0, "0", cast("0" as blob),
    TRUE, 1, 1, 1.1, "1", cast("1" as blob)
  );

  select * from all_types_encoded_table;
end;

@attribute(cql:vault_sensitive=(context, (b0, i0, l0, d0, s0, bl0, b1, i1, l1, d1, s1, bl1)))
create procedure load_encoded_with_context_table()
begin
  create table all_types_encoded_with_context_table(
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
    bl1 blob not null @sensitive,

    context text not null
  );

  insert into all_types_encoded_with_context_table values (
    FALSE, 0, 0, 0.0, "0", cast("0" as blob),
    TRUE, 1, 1, 1.1, "1", cast("1" as blob), "cxt"
  );

  select * from all_types_encoded_with_context_table;
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

@attribute(cql:vault_sensitive=(z, (y)))
create proc out_union_dml_with_encode_context()
begin
  create table some_type_encoded_table(x integer, y text @sensitive, z text);
  insert into some_type_encoded_table using 66 x, 'abc' y, 'xyz' z;
  declare x cursor for select * from some_type_encoded_table;
  fetch x;
  out union x;
end;

create procedure test_decoded_value_with_encode_context() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor for call out_union_dml_with_encode_context();
 fetch C;

 call errcheck(C.x IS 66, "C.x IS 66", 2667);
 call errcheck(C.y IS 'abc', "C.y IS 'abc'", 2668);
 call errcheck(C.z IS 'xyz', "C.z IS 'xyz'", 2669);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "decoded_value_with_encode_context")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "decoded_value_with_encode_context")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_decoded_value_with_encode_context(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "decoded_value_with_encode_context", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_encoded_values() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call load_encoded_table();
  fetch C;
  call errcheck(C.b0 IS 0, "C.b0 IS 0", 2675);
  call errcheck(C.i0 IS 0, "C.i0 IS 0", 2676);
  call errcheck(C.l0 IS 0, "C.l0 IS 0", 2677);
  call errcheck(C.d0 IS 0.0, "C.d0 IS 0.0", 2678);
  call errcheck(C.s0 IS "0", "C.s0 IS \"0\"", 2679);
  call errcheck(string_from_blob(C.bl0) IS "0", "string_from_blob(C.bl0) IS \"0\"", 2680);
  call errcheck(C.b1 IS 1, "C.b1 IS 1", 2681);
  call errcheck(C.i1 IS 1, "C.i1 IS 1", 2682);
  call errcheck(C.l1 IS 1, "C.l1 IS 1", 2683);
  call errcheck(C.d1 IS 1.1, "C.d1 IS 1.1", 2684);
  call errcheck(C.s1 IS "1", "C.s1 IS \"1\"", 2685);
  call errcheck(string_from_blob(C.bl1) IS "1", "string_from_blob(C.bl1) IS \"1\"", 2686);

  declare C1 cursor for call out_union_dml();
  fetch C1;
  call errcheck(cql_cursor_diff_val(C, C1) IS NULL, "cql_cursor_diff_val(C, C1) IS NULL", 2690);

  declare C2 cursor for call out_union_not_dml();
  fetch C2;
  call errcheck(cql_cursor_diff_val(C, C2) IS NULL, "cql_cursor_diff_val(C, C2) IS NULL", 2694);

  declare C3 cursor fetch from call load_decoded_out_union();
  call errcheck(cql_cursor_diff_val(C, C3) IS NULL, "cql_cursor_diff_val(C, C3) IS NULL", 2697);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "encoded_values")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "encoded_values")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_encoded_values(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "encoded_values", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_encoded_null_values() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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

  call errcheck(C.b0 IS null, "C.b0 IS null", 2720);
  call errcheck(C.i0 IS null, "C.i0 IS null", 2721);
  call errcheck(C.l0 IS null, "C.l0 IS null", 2722);
  call errcheck(C.d0 IS null, "C.d0 IS null", 2723);
  call errcheck(C.s0 IS null, "C.s0 IS null", 2724);
  call errcheck(C.bl0 IS null, "C.bl0 IS null", 2725);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "encoded_null_values")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "encoded_null_values")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_encoded_null_values(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "encoded_null_values", start_refs, end_refs)); set fails := fails + 1; end if;


declare proc obj_shape(set_ object) out union (o object);
declare proc not_null_obj_shape(set_ object not null) out union (o object not null);

create proc emit_object_result_set(set_ object)
begin
  declare C cursor like obj_shape;
  fetch C using set_ o;
  out union C;

  fetch C using null o;
  out union C;
end;

create proc emit_object_result_set_not_null(set_ object not null)
begin
  declare C cursor like not_null_obj_shape;
  fetch C using set_ o;
  out union C;
end;

create procedure test_object_result_set_value() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let s := set_create();
  declare D cursor for call emit_object_result_set(s);
  fetch D;
  call errcheck(D, "D", 2753);
  call errcheck(D.o is s, "D.o is s", 2754);

  fetch D;
  call errcheck(D, "D", 2757);
  call errcheck(D.o is null, "D.o is null", 2758);

  declare E cursor for call emit_object_result_set_not_null(s);
  fetch E;
  call errcheck(E, "E", 2762);
  call errcheck(E.o is s, "E.o is s", 2763);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "object_result_set_value")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "object_result_set_value")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_object_result_set_value(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "object_result_set_value", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:vault_sensitive=(y))
create procedure load_some_encoded_field()
begin
  create table some_encoded_field_table(x integer, y text @sensitive);
  insert into some_encoded_field_table using 66 x, 'bogus' y;

  declare C cursor for select * from some_encoded_field_table;
  fetch C;
  out C;
end;

create procedure test_read_partially_vault_cursor() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor fetch from call load_some_encoded_field();

 call errcheck(C.x IS 66, "C.x IS 66", 2780);
 call errcheck(C.y IS 'bogus', "C.y IS 'bogus'", 2781);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "read_partially_vault_cursor")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "read_partially_vault_cursor")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_read_partially_vault_cursor(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "read_partially_vault_cursor", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:vault_sensitive=(z, (y)))
create procedure load_some_encoded_field_with_encode_context()
begin
  create table some_encoded_field_context_table(x integer, y text @sensitive, z text);
  insert into some_encoded_field_context_table using 66 x, 'bogus' y, 'context' z;

  declare C cursor for select * from some_encoded_field_context_table;
  fetch C;
  out C;
end;

create procedure test_read_partially_encode_with_encode_context_cursor() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
 declare C cursor fetch from call load_some_encoded_field_with_encode_context();

 call errcheck(C.x IS 66, "C.x IS 66", 2798);
 call errcheck(C.y IS 'bogus', "C.y IS 'bogus'", 2799);
 call errcheck(C.z IS 'context', "C.z IS 'context'", 2800);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "read_partially_encode_with_encode_context_cursor")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "read_partially_encode_with_encode_context_cursor")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_read_partially_encode_with_encode_context_cursor(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "read_partially_encode_with_encode_context_cursor", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:emit_setters)
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
  insert into all_types_table(bl0, bl1) values(cast("bl0_1" as blob), cast("bl1_1" as blob)) @dummy_seed(1) @dummy_nullables;
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
-- to get the auto-cleanup. If you are using the statement
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
-- where info.db is NULL. There can be no autodrop tables here.
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


/*
declare select function rscount(rs long) long;
declare select function rscol(rs long, row integer not null, col integer not null) long;

-- This test is is going to create a rowset using a stored proc, then
-- using the helper proc some_integers_fetch() get access to the result set pointer
-- rather than the sqlite statement. Then it iterates over the result set as though
-- that result set were a virtual table. The point of all of this is to test
-- the virtual-table-like construct that we have created and in so doing
-- test the runtime binding facilities needed by ptr(x)

create procedure test_rowset_reading() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
    call errcheck(C.v == cur, "C.v == cur", 2933);
    call errcheck(C.v * C.v == C.vsq, "C.v * C.v == C.vsq", 2934);
    set cur := cur + 1;
  end;

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "rowset_reading")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "rowset_reading")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_rowset_reading(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "rowset_reading", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_rowset_reading_language_support() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare cur integer not null;
  set cur := 7;
  declare C cursor for call some_integers(7, 12);
  loop fetch C
  begin
    call errcheck(C.v == cur, "C.v == cur", 2946);
    call errcheck(c.vsq == cur * cur, "c.vsq == cur * cur", 2947);
    set cur := cur + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "rowset_reading_language_support")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "rowset_reading_language_support")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_rowset_reading_language_support(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "rowset_reading_language_support", start_refs, end_refs)); set fails := fails + 1; end if;

*/

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

create procedure test_read_all_types_rowset() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call all_types_union();
  fetch C;
  call errcheck(C, "C", 2968);

  call errcheck(C.b0 IS NULL, "C.b0 IS NULL", 2970);
  call errcheck(C.i0 IS NULL, "C.i0 IS NULL", 2971);
  call errcheck(C.l0 IS NULL, "C.l0 IS NULL", 2972);
  call errcheck(C.d0 IS NULL, "C.d0 IS NULL", 2973);
  call errcheck(C.s0 IS NULL, "C.s0 IS NULL", 2974);
  call errcheck(C.bl0 IS NULL, "C.bl0 IS NULL", 2975);
  call errcheck(C.b1 IS 0, "C.b1 IS 0", 2976);
  call errcheck(C.i1 IS 0, "C.i1 IS 0", 2977);
  call errcheck(C.l1 IS 0, "C.l1 IS 0", 2978);
  call errcheck(C.d1 IS 0, "C.d1 IS 0", 2979);
  call errcheck(C.s1 == "s1_0", "C.s1 == \"s1_0\"", 2980);
  call errcheck(C.bl1 == blob_from_string("bl1_0"), "C.bl1 == blob_from_string(\"bl1_0\")", 2981);

  fetch C;
  call errcheck(C, "C", 2984);

  call errcheck(C.b0 IS 1, "C.b0 IS 1", 2986);
  call errcheck(C.i0 IS 1, "C.i0 IS 1", 2987);
  call errcheck(C.l0 IS 1, "C.l0 IS 1", 2988);
  call errcheck(C.d0 IS 1, "C.d0 IS 1", 2989);
  call errcheck(C.s0 IS "s0_1", "C.s0 IS \"s0_1\"", 2990);
  call errcheck(C.bl0 IS blob_from_string("bl0_1"), "C.bl0 IS blob_from_string(\"bl0_1\")", 2991);
  call errcheck(C.b1 IS 1, "C.b1 IS 1", 2992);
  call errcheck(C.i1 IS 1, "C.i1 IS 1", 2993);
  call errcheck(C.l1 IS 1, "C.l1 IS 1", 2994);
  call errcheck(C.d1 IS 1, "C.d1 IS 1", 2995);
  call errcheck(C.s1 == "s1_1", "C.s1 == \"s1_1\"", 2996);
  call errcheck(C.bl1 IS blob_from_string("bl1_1"), "C.bl1 IS blob_from_string(\"bl1_1\")", 2997);

  fetch C;
  call errcheck(not C, "not C", 3000);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "read_all_types_rowset")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "read_all_types_rowset")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_read_all_types_rowset(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "read_all_types_rowset", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_read_all_types_auto_fetcher() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- we want to force the auto fetcher to be called, so we capture the result set
  -- rather than cursoring over it. Then we cursor over the captured result set

  let result_set := load_all_types_table();
  declare C cursor for result_set;
  fetch C;
  call errcheck(C, "C", 3010);

  call errcheck(C.b0 IS NULL, "C.b0 IS NULL", 3012);
  call errcheck(C.i0 IS NULL, "C.i0 IS NULL", 3013);
  call errcheck(C.l0 IS NULL, "C.l0 IS NULL", 3014);
  call errcheck(C.d0 IS NULL, "C.d0 IS NULL", 3015);
  call errcheck(C.s0 IS NULL, "C.s0 IS NULL", 3016);
  call errcheck(C.bl0 IS NULL, "C.bl0 IS NULL", 3017);
  call errcheck(C.b1 IS 0, "C.b1 IS 0", 3018);
  call errcheck(C.i1 IS 0, "C.i1 IS 0", 3019);
  call errcheck(C.l1 IS 0, "C.l1 IS 0", 3020);
  call errcheck(C.d1 IS 0, "C.d1 IS 0", 3021);
  call errcheck(C.s1 == "s1_0", "C.s1 == \"s1_0\"", 3022);
  call errcheck(string_from_blob(C.bl1) == "bl1_0", "string_from_blob(C.bl1) == \"bl1_0\"", 3023);

  fetch C;
  call errcheck(C, "C", 3026);

  call errcheck(C.b0 IS 1, "C.b0 IS 1", 3028);
  call errcheck(C.i0 IS 1, "C.i0 IS 1", 3029);
  call errcheck(C.l0 IS 1, "C.l0 IS 1", 3030);
  call errcheck(C.d0 IS 1, "C.d0 IS 1", 3031);
  call errcheck(C.s0 IS "s0_1", "C.s0 IS \"s0_1\"", 3032);
  call errcheck(string_from_blob(C.bl0) == "bl0_1", "string_from_blob(C.bl0) == \"bl0_1\"", 3033);
  call errcheck(C.b1 IS 1, "C.b1 IS 1", 3034);
  call errcheck(C.i1 IS 1, "C.i1 IS 1", 3035);
  call errcheck(C.l1 IS 1, "C.l1 IS 1", 3036);
  call errcheck(C.d1 IS 1, "C.d1 IS 1", 3037);
  call errcheck(C.s1 == "s1_1", "C.s1 == \"s1_1\"", 3038);
  call errcheck(string_from_blob(C.bl1) == "bl1_1", "string_from_blob(C.bl1) == \"bl1_1\"", 3039);
  call errcheck(cql_get_blob_size(C.bl1) == 5, "cql_get_blob_size(C.bl1) == 5", 3040);

  fetch C;
  call errcheck(not C, "not C", 3043);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "read_all_types_auto_fetcher")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "read_all_types_auto_fetcher")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_read_all_types_auto_fetcher(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "read_all_types_auto_fetcher", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_rowset_via_union_failed() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
  call errcheck(ok_after_all, "ok_after_all", 3061);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "rowset_via_union_failed")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "rowset_via_union_failed")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_rowset_via_union_failed(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "rowset_via_union_failed", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_boxing_cursors() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
    call errcheck(C.x == 1, "C.x == 1", 3080);
    call errcheck(C.y == 2, "C.y == 2", 3081);

    fetch D;
    -- C did not change
    call errcheck(C.x == 1, "C.x == 1", 3085);
    call errcheck(C.y == 2, "C.y == 2", 3086);
    call errcheck(D.x == 3, "D.x == 3", 3087);
    call errcheck(D.y == 4, "D.y == 4", 3088);

    fetch C;
    -- C advanced D values held
    call errcheck(C.x == 5, "C.x == 5", 3092);
    call errcheck(C.y == 6, "C.y == 6", 3093);
    call errcheck(D.x == 3, "D.x == 3", 3094);
    call errcheck(D.y == 4, "D.y == 4", 3095);

    set i := i + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "boxing_cursors")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "boxing_cursors")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_boxing_cursors(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "boxing_cursors", start_refs, end_refs)); set fails := fails + 1; end if;

create proc a_few_rows()
begin
  with data(x,y) as (values (1,2), (3,4), (5,6))
  select * from data;
end;

create procedure test_boxing_from_call() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i integer not null;

  set i := 0;
  while i < 5
  begin
    declare C cursor for call a_few_rows();

    declare box object<C cursor>;
    set box from cursor C;
    declare D cursor for box;

    fetch C;
    call errcheck(C.x == 1, "C.x == 1", 3120);
    call errcheck(C.y == 2, "C.y == 2", 3121);

    fetch D;
    -- C did not change
    call errcheck(C.x == 1, "C.x == 1", 3125);
    call errcheck(C.y == 2, "C.y == 2", 3126);
    call errcheck(D.x == 3, "D.x == 3", 3127);
    call errcheck(D.y == 4, "D.y == 4", 3128);

    fetch C;
    -- C advanced D values held
    call errcheck(C.x == 5, "C.x == 5", 3132);
    call errcheck(C.y == 6, "C.y == 6", 3133);
    call errcheck(D.x == 3, "D.x == 3", 3134);
    call errcheck(D.y == 4, "D.y == 4", 3135);

    set i := i + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "boxing_from_call")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "boxing_from_call")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_boxing_from_call(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "boxing_from_call", start_refs, end_refs)); set fails := fails + 1; end if;

@enforce_normal cast;

create procedure test_numeric_casts() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
  call errcheck(b == 1, "b == 1", 3155);
  set i := cast(1.9 as integer);
  call errcheck(i == 1, "i == 1", 3157);
  set l := cast(12.9 as long);
  call errcheck(l == 12, "l == 12", 3159);
  set r := cast(12 as real);
  call errcheck(r == 12.0, "r == 12.0", 3161);

  -- null cases
  call errcheck(cast(b0 as bool) is null, "cast(b0 as bool) is null", 3164);
  call errcheck(cast(b0 as int) is null, "cast(b0 as int) is null", 3165);
  call errcheck(cast(b0 as long) is null, "cast(b0 as long) is null", 3166);
  call errcheck(cast(b0 as real) is null, "cast(b0 as real) is null", 3167);

  -- force conversion (nullable)
  declare x real;
  set x := 7.5;
  set b0 := cast(x as bool);
  call errcheck(b0 == 1, "b0 == 1", 3173);
  set x := 1.9;
  set i0 := cast(x as integer);
  call errcheck(i0 == 1, "i0 == 1", 3176);
  set x := 12.9;
  set l0 := cast(x as long);
  call errcheck(l0 == 12, "l0 == 12", 3179);
  set x := 12.0;
  set r0 := cast(x as real);
  call errcheck(r0 == 12.0, "r0 == 12.0", 3182);
  set l := 12;
  set r0 := cast(l as real);
  call errcheck(r0 == 12.0, "r0 == 12.0", 3185);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "numeric_casts")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "numeric_casts")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_numeric_casts(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "numeric_casts", start_refs, end_refs)); set fails := fails + 1; end if;

@enforce_strict cast;

create proc dummy(seed integer not null, i integer not null, r real not null, b bool not null)
begin
   call errcheck(seed == i, "seed == i", 3193);
   call errcheck(seed == r, "seed == r", 3194);
   call errcheck(not seed == not b, "not seed == not b", 3195);
end;

create procedure test_cursor_args() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare args cursor like dummy arguments;
  fetch args() from values() @dummy_seed(12);
  call dummy(from args);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cursor_args")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cursor_args")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cursor_args(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cursor_args", start_refs, end_refs)); set fails := fails + 1; end if;

DECLARE PROCEDURE cql_exec_internal(sql TEXT NOT NULL) USING TRANSACTION;
create table xyzzy(id integer, name text, data blob);

create procedure test_exec_internal() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
  call errcheck(cql_cursor_diff_val(C,D) is null, "cql_cursor_diff_val(C,D) is null", 3219);
  fetch C;
  fetch D using 2 id, 'y' name, bl2 data;
  call errcheck(cql_cursor_diff_val(C,D) is null, "cql_cursor_diff_val(C,D) is null", 3222);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "exec_internal")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "exec_internal")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_exec_internal(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "exec_internal", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_const_folding() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(const(1 + 1) == 2, "const(1 + 1) == 2", 3226);
  call errcheck(const(1.0 + 1) == 2.0, "const(1.0 + 1) == 2.0", 3227);
  call errcheck(const(1 + 1L) == 2L, "const(1 + 1L) == 2L", 3228);
  call errcheck(const(1 + (1==1) ) == 2, "const(1 + (1==1) ) == 2", 3229);
  call errcheck(const(1.0 + 1L) == 2.0, "const(1.0 + 1L) == 2.0", 3230);
  call errcheck(const(1.0 + (1 == 1)) == 2.0, "const(1.0 + (1 == 1)) == 2.0", 3231);
  call errcheck(const((1==1) + 1L) == 2L, "const((1==1) + 1L) == 2L", 3232);

  call errcheck(2 == const(1 + 1), "2 == const(1 + 1)", 3234);
  call errcheck(2.0 == const(1.0 + 1), "2.0 == const(1.0 + 1)", 3235);
  call errcheck(2L == const(1 + 1L), "2L == const(1 + 1L)", 3236);
  call errcheck(2 == const(1 + (1==1) ), "2 == const(1 + (1==1) )", 3237);

  call errcheck(const(1 - 1) == 0, "const(1 - 1) == 0", 3239);
  call errcheck(const(1.0 - 1) == 0.0, "const(1.0 - 1) == 0.0", 3240);
  call errcheck(const(1 - 1L) == 0L, "const(1 - 1L) == 0L", 3241);
  call errcheck(const(1 - (1==1) ) == 0, "const(1 - (1==1) ) == 0", 3242);

  call errcheck(const(3 * 2) == 6, "const(3 * 2) == 6", 3244);
  call errcheck(const(3.0 * 2) == 6.0, "const(3.0 * 2) == 6.0", 3245);
  call errcheck(const(3 * 2L) == 6L, "const(3 * 2L) == 6L", 3246);
  call errcheck(const(3 * (1==1) ) == 3, "const(3 * (1==1) ) == 3", 3247);

  call errcheck(const(3 / 1) == 3, "const(3 / 1) == 3", 3249);
  call errcheck(const(3.0 / 1) == 3.0, "const(3.0 / 1) == 3.0", 3250);
  call errcheck(const(3 / 1L) == 3L, "const(3 / 1L) == 3L", 3251);
  call errcheck(const(3 / (1==1) ) == 3, "const(3 / (1==1) ) == 3", 3252);

  call errcheck(const(3 % 1) == 0, "const(3 % 1) == 0", 3254);
  call errcheck(const(3 % 1L) == 0L, "const(3 % 1L) == 0L", 3255);
  call errcheck(const(3 % (1==1) ) == 0, "const(3 % (1==1) ) == 0", 3256);

  call errcheck(const(8 | 1) == 9, "const(8 | 1) == 9", 3258);
  call errcheck(const(8 | 1L) == 9L, "const(8 | 1L) == 9L", 3259);
  call errcheck(const(8 | (1==1) ) == 9, "const(8 | (1==1) ) == 9", 3260);

  call errcheck(const(7 & 4) == 4, "const(7 & 4) == 4", 3262);
  call errcheck(const(7 & 4L) == 4L, "const(7 & 4L) == 4L", 3263);
  call errcheck(const(7 & (1==1) ) == 1, "const(7 & (1==1) ) == 1", 3264);

  call errcheck(const(16 << 1) == 32, "const(16 << 1) == 32", 3266);
  call errcheck(const(16 << 1L) == 32L, "const(16 << 1L) == 32L", 3267);
  call errcheck(const(16 << (1==1) ) == 32, "const(16 << (1==1) ) == 32", 3268);

  call errcheck(const(16 >> 1) == 8, "const(16 >> 1) == 8", 3270);
  call errcheck(const(16 >> 1L) == 8L, "const(16 >> 1L) == 8L", 3271);
  call errcheck(const(16 >> (1==1) ) == 8, "const(16 >> (1==1) ) == 8", 3272);

  call errcheck(const(NULL) is null, "const(NULL) is null", 3274);

  call errcheck(const( 1 or 1/0) == 1, "const( 1 or 1/0) == 1", 3276);
  call errcheck(const( 0 or null) is null, "const( 0 or null) is null", 3277);
  call errcheck(const( 0 or 0) == 0, "const( 0 or 0) == 0", 3278);
  call errcheck(const( 0 or 1) == 1, "const( 0 or 1) == 1", 3279);
  call errcheck(const( null or null) is null, "const( null or null) is null", 3280);
  call errcheck(const( null or 0) is null, "const( null or 0) is null", 3281);
  call errcheck(const( null or 1) is 1, "const( null or 1) is 1", 3282);

  call errcheck(const( 0 and 1/0) == 0, "const( 0 and 1/0) == 0", 3284);
  call errcheck(const( 1 and null) is null, "const( 1 and null) is null", 3285);
  call errcheck(const( 1 and 0) == 0, "const( 1 and 0) == 0", 3286);
  call errcheck(const( 1 and 1) == 1, "const( 1 and 1) == 1", 3287);
  call errcheck(const( null and null) is null, "const( null and null) is null", 3288);
  call errcheck(const( null and 0) == 0, "const( null and 0) == 0", 3289);
  call errcheck(const( null and 1) is null, "const( null and 1) is null", 3290);

  call errcheck(const(3 == 3), "const(3 == 3)", 3292);
  call errcheck(const(3 == 3.0), "const(3 == 3.0)", 3293);
  call errcheck(const(3 == 3L), "const(3 == 3L)", 3294);
  call errcheck(const((0 == 0) == (1 == 1)), "const((0 == 0) == (1 == 1))", 3295);

  call errcheck(const(4 != 3), "const(4 != 3)", 3297);
  call errcheck(const(4 != 3.0), "const(4 != 3.0)", 3298);
  call errcheck(const(4 != 3L), "const(4 != 3L)", 3299);
  call errcheck(const((1 == 0) != (1 == 1)), "const((1 == 0) != (1 == 1))", 3300);

  call errcheck(const(4 >= 3), "const(4 >= 3)", 3302);
  call errcheck(const(4 >= 3.0), "const(4 >= 3.0)", 3303);
  call errcheck(const(4 >= 3L), "const(4 >= 3L)", 3304);
  call errcheck(const((1 == 1) >= (1 == 0)), "const((1 == 1) >= (1 == 0))", 3305);

  call errcheck(const(3 >= 3), "const(3 >= 3)", 3307);
  call errcheck(const(3 >= 3.0), "const(3 >= 3.0)", 3308);
  call errcheck(const(3 >= 3L), "const(3 >= 3L)", 3309);
  call errcheck(const((1 == 1) >= (1 == 1)), "const((1 == 1) >= (1 == 1))", 3310);

  call errcheck(const(4 > 3), "const(4 > 3)", 3312);
  call errcheck(const(4 > 3.0), "const(4 > 3.0)", 3313);
  call errcheck(const(4 > 3L), "const(4 > 3L)", 3314);
  call errcheck(const((1 == 1) > (1 == 0)), "const((1 == 1) > (1 == 0))", 3315);

  call errcheck(const(2 <= 3), "const(2 <= 3)", 3317);
  call errcheck(const(2 <= 3.0), "const(2 <= 3.0)", 3318);
  call errcheck(const(2 <= 3L), "const(2 <= 3L)", 3319);
  call errcheck(const((1 == 0) <= (1 == 1)), "const((1 == 0) <= (1 == 1))", 3320);

  call errcheck(const(3 <= 3), "const(3 <= 3)", 3322);
  call errcheck(const(3 <= 3.0), "const(3 <= 3.0)", 3323);
  call errcheck(const(3 <= 3L), "const(3 <= 3L)", 3324);
  call errcheck(const((1 == 1) <= (1 == 1)), "const((1 == 1) <= (1 == 1))", 3325);

  call errcheck(const(2 < 3), "const(2 < 3)", 3327);
  call errcheck(const(2 < 3.0), "const(2 < 3.0)", 3328);
  call errcheck(const(2 < 3L), "const(2 < 3L)", 3329);
  call errcheck(const((1 == 0) < (1 == 1)), "const((1 == 0) < (1 == 1))", 3330);

  call errcheck((NULL + NULL) is NULL, "(NULL + NULL) is NULL", 3332);
  call errcheck((NULL - NULL) is NULL, "(NULL - NULL) is NULL", 3333);
  call errcheck((NULL * NULL) is NULL, "(NULL * NULL) is NULL", 3334);
  call errcheck((NULL / NULL) is NULL, "(NULL / NULL) is NULL", 3335);
  call errcheck((NULL % NULL) is NULL, "(NULL % NULL) is NULL", 3336);
  call errcheck((NULL | NULL) is NULL, "(NULL | NULL) is NULL", 3337);
  call errcheck((NULL & NULL) is NULL, "(NULL & NULL) is NULL", 3338);
  call errcheck((NULL << NULL) is NULL, "(NULL << NULL) is NULL", 3339);
  call errcheck((NULL >> NULL) is NULL, "(NULL >> NULL) is NULL", 3340);

  call errcheck(const(NULL + NULL) is NULL, "const(NULL + NULL) is NULL", 3342);
  call errcheck(const(NULL - NULL) is NULL, "const(NULL - NULL) is NULL", 3343);
  call errcheck(const(NULL * NULL) is NULL, "const(NULL * NULL) is NULL", 3344);
  call errcheck(const(NULL / NULL) is NULL, "const(NULL / NULL) is NULL", 3345);
  call errcheck(const(NULL % NULL) is NULL, "const(NULL % NULL) is NULL", 3346);
  call errcheck(const(NULL | NULL) is NULL, "const(NULL | NULL) is NULL", 3347);
  call errcheck(const(NULL & NULL) is NULL, "const(NULL & NULL) is NULL", 3348);
  call errcheck(const(NULL << NULL) is NULL, "const(NULL << NULL) is NULL", 3349);
  call errcheck(const(NULL >> NULL) is NULL, "const(NULL >> NULL) is NULL", 3350);

  call errcheck(const((NULL + NULL) is NULL), "const((NULL + NULL) is NULL)", 3352);
  call errcheck(const((NULL - NULL) is NULL), "const((NULL - NULL) is NULL)", 3353);
  call errcheck(const((NULL * NULL) is NULL), "const((NULL * NULL) is NULL)", 3354);
  call errcheck(const((NULL / NULL) is NULL), "const((NULL / NULL) is NULL)", 3355);
  call errcheck(const((NULL % NULL) is NULL), "const((NULL % NULL) is NULL)", 3356);
  call errcheck(const((NULL | NULL) is NULL), "const((NULL | NULL) is NULL)", 3357);
  call errcheck(const((NULL & NULL) is NULL), "const((NULL & NULL) is NULL)", 3358);
  call errcheck(const((NULL << NULL) is NULL), "const((NULL << NULL) is NULL)", 3359);
  call errcheck(const((NULL >> NULL) is NULL), "const((NULL >> NULL) is NULL)", 3360);

  call errcheck(const(NULL IS NOT NULL) == 0, "const(NULL IS NOT NULL) == 0", 3362);
  call errcheck(const(NULL IS NOT 1), "const(NULL IS NOT 1)", 3363);
  call errcheck(const((1 OR NULL) IS NOT NULL), "const((1 OR NULL) IS NOT NULL)", 3364);

  call errcheck(const(1 IS 1), "const(1 IS 1)", 3366);
  call errcheck(const(1L IS 1L), "const(1L IS 1L)", 3367);
  call errcheck(const(1.0 IS 1.0), "const(1.0 IS 1.0)", 3368);
  call errcheck(const((1==1) is (2==2)), "const((1==1) is (2==2))", 3369);

  call errcheck(const(cast(3.2 as integer) == 3), "const(cast(3.2 as integer) == 3)", 3371);
  call errcheck(const(cast(3.2 as long_int) == 3L), "const(cast(3.2 as long_int) == 3L)", 3372);
  call errcheck(const(cast(3.2 as bool) == 1), "const(cast(3.2 as bool) == 1)", 3373);
  call errcheck(const(cast(0.0 as bool) == 0), "const(cast(0.0 as bool) == 0)", 3374);
  call errcheck(const(cast(null+0 as bool) is null), "const(cast(null+0 as bool) is null)", 3375);
  call errcheck(const(cast(3L as real) == 3.0), "const(cast(3L as real) == 3.0)", 3376);
  call errcheck(const(cast(3L as integer) == 3), "const(cast(3L as integer) == 3)", 3377);
  call errcheck(const(cast(3L as bool) == 1), "const(cast(3L as bool) == 1)", 3378);
  call errcheck(const(cast(0L as bool) == 0), "const(cast(0L as bool) == 0)", 3379);

  call errcheck(const(not 0) == 1, "const(not 0) == 1", 3381);
  call errcheck(const(not 1) == 0, "const(not 1) == 0", 3382);
  call errcheck(const(not 2) == 0, "const(not 2) == 0", 3383);
  call errcheck(const(not 0L) == 1, "const(not 0L) == 1", 3384);
  call errcheck(const(not 1L) == 0, "const(not 1L) == 0", 3385);
  call errcheck(const(not 2L) == 0, "const(not 2L) == 0", 3386);
  call errcheck(const(not 2.0) == 0, "const(not 2.0) == 0", 3387);
  call errcheck(const(not 0.0) == 1, "const(not 0.0) == 1", 3388);
  call errcheck(const(not not 2) == 1, "const(not not 2) == 1", 3389);
  call errcheck(const(not NULL) is NULL, "const(not NULL) is NULL", 3390);

  call errcheck(const(~0) == -1, "const(~0) == -1", 3392);
  call errcheck(const(~0L) == -1L, "const(~0L) == -1L", 3393);
  call errcheck(const(~ ~0L) == 0L, "const(~ ~0L) == 0L", 3394);
  call errcheck(const(~NULL) is NULL, "const(~NULL) is NULL", 3395);
  call errcheck(const(~(0==0)) == -2, "const(~(0==0)) == -2", 3396);
  call errcheck(const(~(0==1)) == -1, "const(~(0==1)) == -1", 3397);

  call errcheck(const(-1) == -1, "const(-1) == -1", 3399);
  call errcheck(const(-2) == -2, "const(-2) == -2", 3400);
  call errcheck(const(-1.0) == -1.0, "const(-1.0) == -1.0", 3401);
  call errcheck(const(-2.0) == -2.0, "const(-2.0) == -2.0", 3402);
  call errcheck(const((0 + -2)) == -2, "const((0 + -2)) == -2", 3403);
  call errcheck(const(-(1 + 1)) == -2, "const(-(1 + 1)) == -2", 3404);
  call errcheck(const(-1L) == -1L, "const(-1L) == -1L", 3405);
  call errcheck(const(- -1L) == 1L, "const(- -1L) == 1L", 3406);
  call errcheck(const(-NULL) is NULL, "const(-NULL) is NULL", 3407);
  call errcheck(const(-(0==0)) == -1, "const(-(0==0)) == -1", 3408);
  call errcheck(const(-(0==1)) == 0, "const(-(0==1)) == 0", 3409);

  -- IIF gets rewritten to case/when so we use that here for convenience
  call errcheck(const(iif(1, 3, 5)) == 3, "const(iif(1, 3, 5)) == 3", 3412);
  call errcheck(const(iif(0, 3, 5)) == 5, "const(iif(0, 3, 5)) == 5", 3413);
  call errcheck(const(iif(1L, 3, 5)) == 3, "const(iif(1L, 3, 5)) == 3", 3414);
  call errcheck(const(iif(0L, 3, 5)) == 5, "const(iif(0L, 3, 5)) == 5", 3415);
  call errcheck(const(iif(1.0, 3, 5)) == 3, "const(iif(1.0, 3, 5)) == 3", 3416);
  call errcheck(const(iif(0.0, 3, 5)) == 5, "const(iif(0.0, 3, 5)) == 5", 3417);
  call errcheck(const(iif((1==1), 3, 5)) == 3, "const(iif((1==1), 3, 5)) == 3", 3418);
  call errcheck(const(iif((1==0), 3, 5)) == 5, "const(iif((1==0), 3, 5)) == 5", 3419);

  call errcheck(const(case 1 when 2 then 20 else 10 end) == 10, "const(case 1 when 2 then 20 else 10 end) == 10", 3421);
  call errcheck(const(case 2 when 2 then 20 else 10 end) == 20, "const(case 2 when 2 then 20 else 10 end) == 20", 3422);
  call errcheck(const(case 2 when 1 then 10 when 2 then 20 else 40 end) == 20, "const(case 2 when 1 then 10 when 2 then 20 else 40 end) == 20", 3423);
  call errcheck(const(case 1 when 1 then 10 when 2 then 20 else 40 end) == 10, "const(case 1 when 1 then 10 when 2 then 20 else 40 end) == 10", 3424);
  call errcheck(const(case 5 when 1 then 10 when 2 then 20 else 40 end) == 40, "const(case 5 when 1 then 10 when 2 then 20 else 40 end) == 40", 3425);
  call errcheck(const(case null when 1 then 10 when 2 then 20 else 40 end) == 40, "const(case null when 1 then 10 when 2 then 20 else 40 end) == 40", 3426);

  call errcheck(const(case 1.0 when 2 then 20 else 10 end) == 10, "const(case 1.0 when 2 then 20 else 10 end) == 10", 3428);
  call errcheck(const(case 2.0 when 2 then 20 else 10 end) == 20, "const(case 2.0 when 2 then 20 else 10 end) == 20", 3429);
  call errcheck(const(case 2.0 when 1 then 10 when 2 then 20 else 40 end) == 20, "const(case 2.0 when 1 then 10 when 2 then 20 else 40 end) == 20", 3430);
  call errcheck(const(case 1.0 when 1 then 10 when 2 then 20 else 40 end) == 10, "const(case 1.0 when 1 then 10 when 2 then 20 else 40 end) == 10", 3431);
  call errcheck(const(case 5.0 when 1 then 10 when 2 then 20 else 40 end) == 40, "const(case 5.0 when 1 then 10 when 2 then 20 else 40 end) == 40", 3432);

  call errcheck(const(case 1L when 2 then 20 else 10 end) == 10, "const(case 1L when 2 then 20 else 10 end) == 10", 3434);
  call errcheck(const(case 2L when 2 then 20 else 10 end) == 20, "const(case 2L when 2 then 20 else 10 end) == 20", 3435);
  call errcheck(const(case 2L when 1 then 10 when 2 then 20 else 40 end) == 20, "const(case 2L when 1 then 10 when 2 then 20 else 40 end) == 20", 3436);
  call errcheck(const(case 1L when 1 then 10 when 2 then 20 else 40 end) == 10, "const(case 1L when 1 then 10 when 2 then 20 else 40 end) == 10", 3437);
  call errcheck(const(case 5L when 1 then 10 when 2 then 20 else 40 end) == 40, "const(case 5L when 1 then 10 when 2 then 20 else 40 end) == 40", 3438);

  call errcheck(const(case (1==1) when (1==1) then 10 else 20 end) == 10, "const(case (1==1) when (1==1) then 10 else 20 end) == 10", 3440);
  call errcheck(const(case (1==0) when (1==1) then 10 else 20 end) == 20, "const(case (1==0) when (1==1) then 10 else 20 end) == 20", 3441);
  call errcheck(const(case (1==1) when (0==1) then 10 else 20 end) == 20, "const(case (1==1) when (0==1) then 10 else 20 end) == 20", 3442);
  call errcheck(const(case (1==0) when (0==1) then 10 else 20 end) == 10, "const(case (1==0) when (0==1) then 10 else 20 end) == 10", 3443);
  call errcheck(const(case (1==0) when null then 10 else 20 end) == 20, "const(case (1==0) when null then 10 else 20 end) == 20", 3444);
  call errcheck(const(case (1==0) when null then 10 end ) is null, "const(case (1==0) when null then 10 end ) is null", 3445);

  call errcheck(const(case 5L when 1 then 10 when 2 then 20 end) is NULL, "const(case 5L when 1 then 10 when 2 then 20 end) is NULL", 3447);
  call errcheck(const(case when NULL then 1 else 2 end) == 2, "const(case when NULL then 1 else 2 end) == 2", 3448);

  call errcheck(const(0x10) == 16, "const(0x10) == 16", 3450);
  call errcheck(const(0x10 + 0xf) == 31, "const(0x10 + 0xf) == 31", 3451);
  call errcheck(const(0x100100100) == 0x100100100, "const(0x100100100) == 0x100100100", 3452);
  call errcheck(const(0x100100100L) == 0x100100100, "const(0x100100100L) == 0x100100100", 3453);
  call errcheck(const(0x100100100) == 0x100100100L, "const(0x100100100) == 0x100100100L", 3454);
  call errcheck(const(0x100100100L) == 0x100100100L, "const(0x100100100L) == 0x100100100L", 3455);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "const_folding")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "const_folding")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_const_folding(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "const_folding", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_long_literals() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare x long not null;
  declare z long;

  set x := 1L;
  call errcheck(x == 1, "x == 1", 3464);

  set x := 10000000000;
  call errcheck(x = 10000000000, "x = 10000000000", 3467);
  call errcheck(x != const(cast(10000000000L as integer)), "x != const(cast(10000000000L as integer))", 3468);
  call errcheck(x > 0x7fffffff, "x > 0x7fffffff", 3469);

  set x := 10000000000L;
  call errcheck(x = 10000000000L, "x = 10000000000L", 3472);
  call errcheck(x != const(cast(10000000000L as integer)), "x != const(cast(10000000000L as integer))", 3473);
  call errcheck(x > 0x7fffffff, "x > 0x7fffffff", 3474);

  set x := 0x1000000000L;
  call errcheck(x = 0x1000000000L, "x = 0x1000000000L", 3477);
  call errcheck(x != const(cast(0x10000000000L as integer)), "x != const(cast(0x10000000000L as integer))", 3478);
  call errcheck(x > 0x7fffffff, "x > 0x7fffffff", 3479);

  set x := 0x1000000000;
  call errcheck(x = 0x1000000000L, "x = 0x1000000000L", 3482);
  call errcheck(x != const(cast(0x10000000000L as integer)), "x != const(cast(0x10000000000L as integer))", 3483);
  call errcheck(x > 0x7fffffff, "x > 0x7fffffff", 3484);

  set x := const(0x1000000000);
  call errcheck(x = 0x1000000000L, "x = 0x1000000000L", 3487);
  call errcheck(x != const(cast(0x1000000000L as integer)), "x != const(cast(0x1000000000L as integer))", 3488);
  call errcheck(x > 0x7fffffff, "x > 0x7fffffff", 3489);

  set x := 1000L * 1000 * 1000 * 1000;
  call errcheck(x = 1000000000000, "x = 1000000000000", 3492);
  call errcheck(x != const(cast(1000000000000 as integer)), "x != const(cast(1000000000000 as integer))", 3493);
  set x := const(1000L * 1000 * 1000 * 1000);

  set z := 1L;
  call errcheck(z == 1, "z == 1", 3497);

  set z := 10000000000;
  call errcheck(z = 10000000000, "z = 10000000000", 3500);
  call errcheck(z != const(cast(10000000000L as integer)), "z != const(cast(10000000000L as integer))", 3501);
  call errcheck(z > 0x7fffffff, "z > 0x7fffffff", 3502);

  set z := 10000000000L;
  call errcheck(z = 10000000000L, "z = 10000000000L", 3505);
  call errcheck(z != const(cast(10000000000L as integer)), "z != const(cast(10000000000L as integer))", 3506);
  call errcheck(z > 0x7fffffff, "z > 0x7fffffff", 3507);

  set z := 0x1000000000L;
  call errcheck(z = 0x1000000000L, "z = 0x1000000000L", 3510);
  call errcheck(z != const(cast(0x1000000000L as integer)), "z != const(cast(0x1000000000L as integer))", 3511);
  call errcheck(z > 0x7fffffff, "z > 0x7fffffff", 3512);

  set z := 0x1000000000;
  call errcheck(z = 0x1000000000L, "z = 0x1000000000L", 3515);
  call errcheck(z != const(cast(0x1000000000L as integer)), "z != const(cast(0x1000000000L as integer))", 3516);
  call errcheck(z > 0x7fffffff, "z > 0x7fffffff", 3517);

  set z := const(0x1000000000);
  call errcheck(z = 0x1000000000L, "z = 0x1000000000L", 3520);
  call errcheck(z != const(cast(0x1000000000L as integer)), "z != const(cast(0x1000000000L as integer))", 3521);
  call errcheck(z > 0x7fffffff, "z > 0x7fffffff", 3522);

  set z := 1000L * 1000 * 1000 * 1000;
  call errcheck(z = 1000000000000, "z = 1000000000000", 3525);
  call errcheck(z != const(cast(1000000000000 as integer)), "z != const(cast(1000000000000 as integer))", 3526);
  set z := const(1000L * 1000 * 1000 * 1000);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "long_literals")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "long_literals")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_long_literals(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "long_literals", start_refs, end_refs)); set fails := fails + 1; end if;

create proc no_statement_really(x integer)
begin
  if x then
    select 1 x;
  end if;
end;

create procedure test_null_statement() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call no_statement_really(0);
  declare x integer;
  set x := 0;
  loop fetch C
  begin
     set x := x + 1;
  end;
  call errcheck(x == 0, "x == 0", 3546);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "null_statement")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "null_statement")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_null_statement(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "null_statement", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_if_nothing_forms() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  create table tdata (
    id integer,
    v integer,
    t text);

  declare t1 text;
  set t1 := (select t from tdata if nothing "nothing");
  call errcheck(t1 == "nothing", "t1 == \"nothing\"", 3557);

  declare v1 integer;
  set v1 := (select v from tdata if nothing -1);
  call errcheck(v1 == -1, "v1 == -1", 3561);

  insert into tdata values(1, 2, null);
  set t1 := (select t from tdata if nothing "nothing");
  call errcheck(t1 is null, "t1 is null", 3565);

  set v1 := (select v from tdata if nothing -1);
  call errcheck(v1 == 2, "v1 == 2", 3568);

  set t1 := (select t from tdata if nothing or null "still nothing");
  call errcheck(t1 == "still nothing", "t1 == \"still nothing\"", 3571);

  insert into tdata values(2, null, "x");
  set v1 := (select v from tdata where id == 2 if nothing or null -1);
  call errcheck(v1 == -1, "v1 == -1", 3575);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "if_nothing_forms")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "if_nothing_forms")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_if_nothing_forms(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "if_nothing_forms", start_refs, end_refs)); set fails := fails + 1; end if;

create proc simple_select()
begin
  select 1 x;
end;

create procedure test_call_in_loop() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call simple_select();
     fetch C;
     call errcheck(C.x == 1, "C.x == 1", 3592);
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "call_in_loop")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "call_in_loop")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_call_in_loop(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "call_in_loop", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_call_in_loop_boxed() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
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
     call errcheck(D.x == 1, "D.x == 1", 3607);
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "call_in_loop_boxed")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "call_in_loop_boxed")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_call_in_loop_boxed(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "call_in_loop_boxed", start_refs, end_refs)); set fails := fails + 1; end if;

create proc out_union_helper()
begin
  declare C cursor like select 1 x;
  fetch C using 1 x;
  out union C;
end;

create procedure test_call_out_union_in_loop() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare i integer;
  set i := 0;
  while i < 5
  begin
     set i := i + 1;
     declare C cursor for call out_union_helper();
     fetch C;
     call errcheck(C.x == 1, "C.x == 1", 3626);
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "call_out_union_in_loop")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "call_out_union_in_loop")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_call_out_union_in_loop(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "call_out_union_in_loop", start_refs, end_refs)); set fails := fails + 1; end if;

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

create procedure test_rc_simple_select() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call simple_select();
  call errcheck(@rc == 0, "@rc == 0", 3650);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "rc_simple_select")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "rc_simple_select")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_rc_simple_select(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "rc_simple_select", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_rc_simple_insert_and_select() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  create table simple_rc_table(id integer, foo text);

  call simple_insert();
  call errcheck(@rc == 0, "@rc == 0", 3657);

  call select_if_nothing(1);
  call errcheck(@rc == 0, "@rc == 0", 3660);

  call select_if_nothing(2);
  call errcheck(@rc == 0, "@rc == 0", 3663);

  begin try
    call select_if_nothing_throw(2);
  end try;
  begin catch
    call errcheck(@rc != 0, "@rc != 0", 3669);
  end catch;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "rc_simple_insert_and_select")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "rc_simple_insert_and_select")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_rc_simple_insert_and_select(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "rc_simple_insert_and_select", start_refs, end_refs)); set fails := fails + 1; end if;

create proc out_union()
begin
  declare C cursor like select 1 x;
  fetch C using 1 x;
  out union C;
end;

-- claims to be an out-union proc but isn't really going to produce anything
-- non dml path
create proc out_union_nil_result()
begin
  if 0 then
     call out_union();
  end if;
end;

-- claims to be an out-union proc but isn't really going to produce anything
-- dml path
create proc out_union_nil_result_dml()
begin
  if 0 then
     call out_union_dml();
  end if;
end;

create procedure test_empty_out_union() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call out_union_nil_result();
  fetch C;
  call errcheck(NOT C, "NOT C", 3701); -- cursor empty but not null

  declare D cursor for call out_union_nil_result_dml();
  fetch D;
  call errcheck(NOT D, "NOT D", 3705); -- cursor empty but not null
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "empty_out_union")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "empty_out_union")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_empty_out_union(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "empty_out_union", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_nested_rc_values() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let e0 := @rc;
  call errcheck(e0 = 0, "e0 = 0", 3710); -- SQLITE_OK
  begin try
    -- force duplicate table error
    create table foo(id integer primary key);
    create table foo(id integer primary key);
  end try;
  begin catch
    let e1 := @rc;
    call errcheck(e1 == 1, "e1 == 1", 3718); -- SQLITE_ERROR
    begin try
       let e2 := @rc;
       call errcheck(e2 == 1, "e2 == 1", 3721); -- SQLITE_ERROR
       -- force constraint error
       insert into foo using 1 id;
       insert into foo using 1 id;
    end try;
    begin catch
       let e3 := @rc;
       call errcheck(e3 == 19, "e3 == 19", 3728); -- SQLITE_CONSTRAINT
    end catch;
    let e4 := @rc;
    call errcheck(e4 == 1, "e4 == 1", 3731); -- back to SQLITE_ERROR
  end catch;
  let e7 := @rc;
  call errcheck(e7 = 0, "e7 = 0", 3734); -- back to SQLITE_OK
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "nested_rc_values")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "nested_rc_values")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_nested_rc_values(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "nested_rc_values", start_refs, end_refs)); set fails := fails + 1; end if;

-- facet helper functions, used by the schema upgrader
DECLARE facet_data TYPE OBJECT<facet_data>;
DECLARE FUNCTION cql_facets_create() create facet_data not null;
DECLARE FUNCTION cql_facet_add(facets facet_data, facet TEXT NOT NULL, crc LONG NOT NULL) BOOL NOT NULL;
DECLARE FUNCTION cql_facet_find(facets facet_data, facet TEXT NOT NULL) LONG NOT NULL;

create procedure test_facet_helpers() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let facets := cql_facets_create();

  -- add some facets
  let i := 0;
  while i < 1000
  begin
    call errcheck(cql_facet_add(facets, printf('fake facet %d', i), i*i), "cql_facet_add(facets, printf('fake facet %d', i), i*i)", 3750);
    set i := i + 1;
  end;

  -- all duplicates, all the adds should return false
  set i := 0;
  while i < 1000
  begin
    call errcheck(NOT cql_facet_add(facets, printf('fake facet %d', i), i*i), "NOT cql_facet_add(facets, printf('fake facet %d', i), i*i)", 3758);
    set i := i + 1;
  end;

  -- we should be able to find all of these
  set i := 0;
  while i < 1000
  begin
    call errcheck(i*i == cql_facet_find(facets, printf('fake facet %d', i)), "i*i == cql_facet_find(facets, printf('fake facet %d', i))", 3766);
    set i := i + 1;
  end;

  -- we should be able to find none of these
  set i := 0;
  while i < 1000
  begin
    call errcheck(-1 == cql_facet_find(facets, printf('fake_facet %d', i)), "-1 == cql_facet_find(facets, printf('fake_facet %d', i))", 3774);
    set i := i + 1;
  end;

  -- NOTE the test infra is counting refs so that if we fail
  -- to clean up the test fails; no expectation is required
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "facet_helpers")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "facet_helpers")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_facet_helpers(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "facet_helpers", start_refs, end_refs)); set fails := fails + 1; end if;

-- not null result
create proc f(x integer not null, out y integer not null)
begin
  set y := x;
end;

-- nullable version (not null arg)
create proc fn(x integer not null, out y integer)
begin
  set y := x;
end;

-- nullable arg and result version (forces boxing)
create proc fnn(x integer, out y integer)
begin
  set y := x;
end;

-- the point of this is to force the temporaries from previous calls to
-- survive into the next expression, the final expression should be
-- something like t1+t2+t3+t4+t5+t6 with no sharing
create procedure test_verify_temp_non_reuse() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call errcheck(f(1)+f(2)+f(4)+f(8)+f(16)+f(32)==63, "f(1)+f(2)+f(4)+f(8)+f(16)+f(32)==63", 3804);
  call errcheck(fn(1)+fn(2)+fn(4)+fn(8)+fn(16)+fn(32)==63, "fn(1)+fn(2)+fn(4)+fn(8)+fn(16)+fn(32)==63", 3805);
  call errcheck(f(1)+fn(2)+f(4)+fn(8)+f(16)+fn(32)==63, "f(1)+fn(2)+f(4)+fn(8)+f(16)+fn(32)==63", 3806);
  call errcheck(fn(1)+f(2)+fn(4)+f(8)+fn(16)+f(32)==63, "fn(1)+f(2)+fn(4)+f(8)+fn(16)+f(32)==63", 3807);

  call errcheck(fnn(1)+fnn(2)+fnn(4)+fnn(8)+fnn(16)+fnn(32)==63, "fnn(1)+fnn(2)+fnn(4)+fnn(8)+fnn(16)+fnn(32)==63", 3809);
  call errcheck(fn(1)+fnn(2)+fn(4)+fnn(8)+fn(16)+fnn(32)==63, "fn(1)+fnn(2)+fn(4)+fnn(8)+fn(16)+fnn(32)==63", 3810);
  call errcheck(f(1)+fn(2)+fnn(4)+fn(8)+fnn(16)+fn(32)==63, "f(1)+fn(2)+fnn(4)+fn(8)+fnn(16)+fn(32)==63", 3811);
  call errcheck(fn(1)+fnn(2)+fn(4)+f(8)+fnn(16)+f(32)==63, "fn(1)+fnn(2)+fn(4)+f(8)+fnn(16)+f(32)==63", 3812);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "verify_temp_non_reuse")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "verify_temp_non_reuse")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_verify_temp_non_reuse(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "verify_temp_non_reuse", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_compressible_batch() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- nest the batch so that it doesn't conflict with the macro proc preamble
  IF 1 THEN
    drop table if exists foo;
    create table goo(id integer);
    insert into goo values (1), (2), (3);
  END IF;
  call errcheck((select sum(id) from goo) == 6, "(select sum(id) from goo) == 6", 3822);
  drop table goo;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "compressible_batch")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "compressible_batch")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_compressible_batch(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "compressible_batch", start_refs, end_refs)); set fails := fails + 1; end if;

-- a simple proc that creates a result set with out union
-- this reference must be correctly managed
create proc get_row()
begin
  declare D cursor like select 'x' facet;
  fetch D using 'x' facet;
  out union D;
end;

-- the test here is to ensure that when we call get_row we correctly
-- release the previous result set
create proc get_row_thrice()
begin
  -- these are redundant but they force the previous pending result to be freed
  -- this still returns a single row
  call get_row();
  call get_row();
  call get_row();
end;

create procedure test_out_union_refcounts() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  DECLARE C CURSOR FOR CALL get_row();
  FETCH C;
  call errcheck(C, "C", 3849);
  call errcheck(C.facet = 'x', "C.facet = 'x'", 3850);
  FETCH C;
  call errcheck(NOT C, "NOT C", 3852);

  DECLARE D CURSOR FOR CALL get_row_thrice();
  FETCH D;
  call errcheck(D, "D", 3856);
  call errcheck(D.facet = 'x', "D.facet = 'x'", 3857);
  FETCH D;
  call errcheck(NOT D, "NOT D", 3859);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "out_union_refcounts")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "out_union_refcounts")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_out_union_refcounts(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "out_union_refcounts", start_refs, end_refs)); set fails := fails + 1; end if;


@attribute(cql:shared_fragment)
create proc f1(pattern text)
begin
  with source(*) LIKE (select 1 id, "x" t)
  select * from source where t like pattern;
end;

@attribute(cql:shared_fragment)
create proc f2(pattern text, idstart int not null, idend int not null, lim int not null)
begin
  with
  source(*) LIKE f1,
  data(*) as (call f1(pattern) using source as source)
  select * from data where data.id between idstart and idend
  limit lim;
end;

@attribute(cql:private)
create proc shared_consumer()
begin
  with
   source1(id, t) as (values (1100, 'x_x'), (1101, 'zz')),
   source2(id, t) as (values (4500, 'y_y'), (4501, 'zz')),
   t1(*) as (call f2('x%', 1000, 2000, 10) using source1 as source),
   t2(*) as (call f2('y%', 4000, 5000, 20) using source2 as source)
  select * from t1
  union all
  select * from t2;
end;

create procedure test_shared_fragments() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for call shared_consumer();
  fetch C;
  call errcheck(C.id = 1100, "C.id = 1100", 3896);
  call errcheck(C.t = 'x_x', "C.t = 'x_x'", 3897);
  fetch C;
  call errcheck(C.id = 4500, "C.id = 4500", 3899);
  call errcheck(C.t = 'y_y', "C.t = 'y_y'", 3900);
  fetch C;
  call errcheck(not C, "not C", 3902);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "shared_fragments")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "shared_fragments")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_shared_fragments(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "shared_fragments", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:shared_fragment)
create proc get_values()
begin
  select 1 id, 'x' t
  union all
  select 2 id, 'y' t;
end;

create table x(id integer, t text);

create procedure test_shared_exec() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  drop table if exists x;
  create table x(id integer, t text);
  with
    (call get_values())
  insert into x select * from get_values;

  declare C cursor for select * from x;
  fetch C;
  call errcheck(C.id = 1, "C.id = 1", 3924);
  call errcheck(C.t = 'x', "C.t = 'x'", 3925);
  fetch C;
  call errcheck(C.id = 2, "C.id = 2", 3927);
  call errcheck(C.t = 'y', "C.t = 'y'", 3928);
  fetch C;
  call errcheck(not C, "not C", 3930);

  drop table x;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "shared_exec")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "shared_exec")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_shared_exec(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "shared_exec", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:shared_fragment)
create proc conditional_values_base(x_ integer)
begin
  if x_ == 2 then
    select x_ id, 'y' t;
  else
    select x_ id, 'u' t
    union all
    select x_+1 id, 'v' t;
  end if;
end;

@attribute(cql:shared_fragment)
create proc conditional_values(x_ integer not null)
begin
  if x_ == 1 then
    select nullable(x_) id, 'x' t;
  else if x_ == 99 then -- this branch won't run
    select nullable(99) id, 'x' t;
  else
    with result(*) as (call conditional_values_base(x_))
    select * from result;
  end if;
end;

create procedure test_conditional_fragment() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for
    with some_cte(*) as (call conditional_values(1))
    select * from some_cte;

  fetch C;

  call errcheck(C.id = 1, "C.id = 1", 3967);
  call errcheck(C.t = 'x', "C.t = 'x'", 3968);
  fetch C;
  call errcheck(not C, "not C", 3970);

  declare D cursor for
    with some_cte(*) as (call conditional_values(2))
  select * from some_cte;

  fetch D;
  call errcheck(D.id = 2, "D.id = 2", 3977);
  call errcheck(D.t = 'y', "D.t = 'y'", 3978);
  fetch D;
  call errcheck(not D, "not D", 3980);

  declare E cursor for
    with some_cte(*) as (call conditional_values(3))
  select * from some_cte;

  fetch E;
  call errcheck(E.id = 3, "E.id = 3", 3987);
  call errcheck(E.t = 'u', "E.t = 'u'", 3988);
  fetch E;
  call errcheck(E.id = 4, "E.id = 4", 3990);
  call errcheck(E.t = 'v', "E.t = 'v'", 3991);
  fetch E;
  call errcheck(not E, "not E", 3993);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "conditional_fragment")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "conditional_fragment")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_conditional_fragment(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "conditional_fragment", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_conditional_fragment_no_with() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for select * from (call conditional_values(1));

  fetch C;
  call errcheck(C.id = 1, "C.id = 1", 4000);
  call errcheck(C.t = 'x', "C.t = 'x'", 4001);
  fetch C;
  call errcheck(not C, "not C", 4003);

  declare D cursor for select * from (call conditional_values(2));

  fetch D;
  call errcheck(D.id = 2, "D.id = 2", 4008);
  call errcheck(D.t = 'y', "D.t = 'y'", 4009);
  fetch D;
  call errcheck(not D, "not D", 4011);

  declare E cursor for select * from (call conditional_values(3));

  fetch E;
  call errcheck(E.id = 3, "E.id = 3", 4016);
  call errcheck(E.t = 'u', "E.t = 'u'", 4017);
  fetch E;
  call errcheck(E.id = 4, "E.id = 4", 4019);
  call errcheck(E.t = 'v', "E.t = 'v'", 4020);
  fetch E;
  call errcheck(not E, "not E", 4022);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "conditional_fragment_no_with")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "conditional_fragment_no_with")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_conditional_fragment_no_with(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "conditional_fragment_no_with", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:shared_fragment)
create proc skip_notnulls(
 a_ integer not null,
 b_ bool not null,
 c_ long not null,
 d_ real not null,
 e_ text not null,
 f_ blob not null,
 g_ object not null)
begin
  if a_ == 0 then
    select a_ - 100 result;
  else if a_ == 1 then
    select case when
      a_ == a_ and
      b_ == b_ and
      c_ == c_ and
      d_ == d_ and
      e_ == e_ and
      f_ == f_ and
      ptr(g_) == ptr(g_)
    then a_ + 100
    else a_ + 200
    end result;
  else
    select a_ result;
  end if;
end;

create procedure test_skip_notnulls() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare _set object not null;
  set _set := set_create();
  declare _bl blob not null;
  set _bl := blob_from_string('hi');

  declare C cursor for
    with some_cte(*) as (call skip_notnulls(123, false, 1L, 2.3, 'x', _bl, _set))
    select * from some_cte;

  fetch C;
  call errcheck(C.result == 123, "C.result == 123", 4065);
  fetch C;
  call errcheck(not C, "not C", 4067);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "skip_notnulls")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "skip_notnulls")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_skip_notnulls(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "skip_notnulls", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:shared_fragment)
create proc skip_nullables(
 a_ integer,
 b_ bool,
 c_ long,
 d_ real,
 e_ text,
 f_ blob,
 g_ object)
begin
  if a_ == 0 then
    select a_ - 100 result;
  else if a_ == 1 then
    select case when
      a_ == a_ and
      b_ == b_ and
      c_ == c_ and
      d_ == d_ and
      e_ == e_ and
      f_ == f_ and
      ptr(g_) == ptr(g_)
    then a_ + 100
    else a_ + 200
    end result;
  else
    select a_ result;
  end if;
end;

create procedure test_skip_nullables() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare _set object not null;
  set _set := set_create();
  declare _bl blob not null;
  set _bl := blob_from_string('hi');

  declare C cursor for
    with some_cte(*) as (call skip_nullables(456, false, 1L, 2.3, 'x', _bl, _set))
    select * from some_cte;

  fetch C;
  call errcheck(C.result == 456, "C.result == 456", 4110);
  fetch C;
  call errcheck(not C, "not C", 4112);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "skip_nullables")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "skip_nullables")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_skip_nullables(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "skip_nullables", start_refs, end_refs)); set fails := fails + 1; end if;

@attribute(cql:shared_fragment)
create proc abs_func(x integer not null)
begin
  select case
    when x < 0 then x * -1
    else x
  end x;
end;

@attribute(cql:shared_fragment)
create proc max_func(x integer not null, y integer not null)
begin
  select case when x <= y then y else x end result;
end;

@attribute(cql:shared_fragment)
create proc ten()
begin
  select 10 ten;
end;

@attribute(cql:shared_fragment)
create proc numbers(lim integer not null)
begin
  with N(x) as (
    select 1 x
    union all
    select x+1 x from N
    limit lim)
  select x from N;
end;

create procedure test_inline_proc() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor for
    select
      abs_func(x - ten()) s1,
      abs(x-10) s2,
      max_func(x - ten(), abs_func(x - ten())) m1,
      max(x - 10, abs(x - 10)) m2
   from
     (call numbers(20));

  loop fetch C
  begin
    call errcheck(C.s1 == C.s2, "C.s1 == C.s2", 4159);
    call errcheck(C.m1 == C.m2, "C.m1 == C.m2", 4160);
  end;

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "inline_proc")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "inline_proc")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_inline_proc(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "inline_proc", start_refs, end_refs)); set fails := fails + 1; end if;

declare proc alltypes_nullable() (
  t bool,
  f bool,
  i integer,
  l long,
  r real,
  bl blob,
  str text
);

declare proc alltypes_notnull() (
  t_nn bool not null,
  f_nn bool not null,
  i_nn integer not null,
  l_nn long not null,
  r_nn real not null,
  bl_nn blob not null,
  str_nn text not null
);

@attribute(cql:blob_storage)
create table storage_notnull(
  like alltypes_notnull
);

@attribute(cql:blob_storage)
create table storage_nullable(
  like alltypes_nullable
);

@attribute(cql:blob_storage)
create table storage_both(
  like alltypes_notnull,
  like alltypes_nullable
);

@attribute(cql:blob_storage)
create table storage_with_extras(
  like alltypes_notnull,
  x integer not null
);

@attribute(cql:blob_storage)
create table storage_one_int(
  x integer not null
);

@attribute(cql:blob_storage)
create table storage_one_long(
  x long not null
);

/*

create procedure test_blob_serialization() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let a_blob := blob_from_string("a blob");
  let b_blob := blob_from_string("b blob");
  declare cursor_both cursor like storage_both;
  fetch cursor_both using
      false f, true t, 22 i, 33L l, 3.14 r, a_blob bl, "text" str,
      false f_nn, true t_nn, 88 i_nn, 66L l_nn, 6.28 r_nn, b_blob bl_nn, "text2" str_nn;

  -- note: using cursor_both and cursor_both ensures codegen is canonicalizing the name
  declare blob_both blob<storage_both>;
  set blob_both from cursor cursor_both;
  declare test_cursor_both cursor like cursor_both;
  fetch test_cursor_both from blob_both;

  call errcheck(test_cursor_both, "test_cursor_both", 4231);
  call errcheck(test_cursor_both.t_nn == cursor_both.t_nn, "test_cursor_both.t_nn == cursor_both.t_nn", 4232);
  call errcheck(test_cursor_both.f_nn == cursor_both.f_nn, "test_cursor_both.f_nn == cursor_both.f_nn", 4233);
  call errcheck(test_cursor_both.i_nn == cursor_both.i_nn, "test_cursor_both.i_nn == cursor_both.i_nn", 4234);
  call errcheck(test_cursor_both.l_nn == cursor_both.l_nn, "test_cursor_both.l_nn == cursor_both.l_nn", 4235);
  call errcheck(test_cursor_both.r_nn == cursor_both.r_nn, "test_cursor_both.r_nn == cursor_both.r_nn", 4236);
  call errcheck(test_cursor_both.bl_nn == cursor_both.bl_nn, "test_cursor_both.bl_nn == cursor_both.bl_nn", 4237);
  call errcheck(test_cursor_both.str_nn == cursor_both.str_nn, "test_cursor_both.str_nn == cursor_both.str_nn", 4238);
  call errcheck(test_cursor_both.t == cursor_both.t, "test_cursor_both.t == cursor_both.t", 4239);
  call errcheck(test_cursor_both.f == cursor_both.f, "test_cursor_both.f == cursor_both.f", 4240);
  call errcheck(test_cursor_both.i == cursor_both.i, "test_cursor_both.i == cursor_both.i", 4241);
  call errcheck(test_cursor_both.l == cursor_both.l, "test_cursor_both.l == cursor_both.l", 4242);
  call errcheck(test_cursor_both.r == cursor_both.r, "test_cursor_both.r == cursor_both.r", 4243);
  call errcheck(test_cursor_both.bl == cursor_both.bl, "test_cursor_both.bl == cursor_both.bl", 4244);
  call errcheck(test_cursor_both.str == cursor_both.str, "test_cursor_both.str == cursor_both.str", 4245);

  declare cursor_notnulls cursor like storage_notnull;
  fetch cursor_notnulls from cursor_both(like cursor_notnulls);
  declare blob_notnulls blob<storage_notnull>;
  set blob_notnulls from cursor cursor_notnulls;
  declare test_cursor_notnulls cursor like cursor_notnulls;
  fetch test_cursor_notnulls from blob_notnulls;

  call errcheck(test_cursor_notnulls, "test_cursor_notnulls", 4254);
  call errcheck(test_cursor_notnulls.t_nn == cursor_both.t_nn, "test_cursor_notnulls.t_nn == cursor_both.t_nn", 4255);
  call errcheck(test_cursor_notnulls.f_nn == cursor_both.f_nn, "test_cursor_notnulls.f_nn == cursor_both.f_nn", 4256);
  call errcheck(test_cursor_notnulls.i_nn == cursor_both.i_nn, "test_cursor_notnulls.i_nn == cursor_both.i_nn", 4257);
  call errcheck(test_cursor_notnulls.l_nn == cursor_both.l_nn, "test_cursor_notnulls.l_nn == cursor_both.l_nn", 4258);
  call errcheck(test_cursor_notnulls.r_nn == cursor_both.r_nn, "test_cursor_notnulls.r_nn == cursor_both.r_nn", 4259);
  call errcheck(test_cursor_notnulls.bl_nn == cursor_both.bl_nn, "test_cursor_notnulls.bl_nn == cursor_both.bl_nn", 4260);
  call errcheck(test_cursor_notnulls.str_nn == cursor_both.str_nn, "test_cursor_notnulls.str_nn == cursor_both.str_nn", 4261);

  -- deserializing should not screw up the reference counts
  set blob_notnulls from cursor cursor_notnulls;
  set blob_notnulls from cursor cursor_notnulls;
  set blob_notnulls from cursor cursor_notnulls;

  -- The next tests verify various things with blobs that are
  -- not directly the right type so we're cheesing the type system.
  -- We need to be able to handle different version sources
  -- as well as assorted corruptions without crashing hence
  -- we pass in blobs of dubious pedigree.

  -- There are missing nullable columns at the end
  -- this is ok and it is our versioning strategy.
  declare any_blob blob;
  let stash_both := blob_both;
  let stash_notnulls := blob_notnulls;
  set any_blob := blob_notnulls;
  set blob_both := any_blob;
  fetch test_cursor_both from blob_both;

  call errcheck(test_cursor_both, "test_cursor_both", 4283);
  call errcheck(test_cursor_both.t_nn == cursor_both.t_nn, "test_cursor_both.t_nn == cursor_both.t_nn", 4284);
  call errcheck(test_cursor_both.f_nn == cursor_both.f_nn, "test_cursor_both.f_nn == cursor_both.f_nn", 4285);
  call errcheck(test_cursor_both.i_nn == cursor_both.i_nn, "test_cursor_both.i_nn == cursor_both.i_nn", 4286);
  call errcheck(test_cursor_both.l_nn == cursor_both.l_nn, "test_cursor_both.l_nn == cursor_both.l_nn", 4287);
  call errcheck(test_cursor_both.r_nn == cursor_both.r_nn, "test_cursor_both.r_nn == cursor_both.r_nn", 4288);
  call errcheck(test_cursor_both.bl_nn == cursor_both.bl_nn, "test_cursor_both.bl_nn == cursor_both.bl_nn", 4289);
  call errcheck(test_cursor_both.str_nn == cursor_both.str_nn, "test_cursor_both.str_nn == cursor_both.str_nn", 4290);
  call errcheck(test_cursor_both.t is null, "test_cursor_both.t is null", 4291);
  call errcheck(test_cursor_both.f is null, "test_cursor_both.f is null", 4292);
  call errcheck(test_cursor_both.i is null, "test_cursor_both.i is null", 4293);
  call errcheck(test_cursor_both.l is null, "test_cursor_both.l is null", 4294);
  call errcheck(test_cursor_both.r is null, "test_cursor_both.r is null", 4295);
  call errcheck(test_cursor_both.bl is null, "test_cursor_both.bl is null", 4296);
  call errcheck(test_cursor_both.str is null, "test_cursor_both.str is null", 4297);

  set blob_both := null;

  -- null blob, throws exception
  let caught := false;
  begin try
    fetch test_cursor_both from blob_both;
  end try;
  begin catch
    call errcheck(not test_cursor_both, "not test_cursor_both", 4307);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4310);

  -- big blob will have too many fields...
  set caught := false;
  set any_blob := stash_both;
  set blob_notnulls := any_blob;
  fetch test_cursor_notnulls from blob_notnulls;

  -- we still expect to be able to read the fields we know without error
  call errcheck(test_cursor_notnulls, "test_cursor_notnulls", 4319);
  call errcheck(test_cursor_notnulls.t_nn == cursor_both.t_nn, "test_cursor_notnulls.t_nn == cursor_both.t_nn", 4320);
  call errcheck(test_cursor_notnulls.f_nn == cursor_both.f_nn, "test_cursor_notnulls.f_nn == cursor_both.f_nn", 4321);
  call errcheck(test_cursor_notnulls.i_nn == cursor_both.i_nn, "test_cursor_notnulls.i_nn == cursor_both.i_nn", 4322);
  call errcheck(test_cursor_notnulls.l_nn == cursor_both.l_nn, "test_cursor_notnulls.l_nn == cursor_both.l_nn", 4323);
  call errcheck(test_cursor_notnulls.r_nn == cursor_both.r_nn, "test_cursor_notnulls.r_nn == cursor_both.r_nn", 4324);
  call errcheck(test_cursor_notnulls.bl_nn == cursor_both.bl_nn, "test_cursor_notnulls.bl_nn == cursor_both.bl_nn", 4325);
  call errcheck(test_cursor_notnulls.str_nn == cursor_both.str_nn, "test_cursor_notnulls.str_nn == cursor_both.str_nn", 4326);

  -- we're missing fields and they aren't nullable, this will make errors
  declare cursor_with_extras cursor like storage_with_extras;
  set caught := false;
  set any_blob := stash_notnulls;
  declare blob_with_extras blob<storage_with_extras>;
  set blob_with_extras := any_blob;
  begin try
    fetch cursor_with_extras from blob_with_extras;
  end try;
  begin catch
    call errcheck(not cursor_with_extras, "not cursor_with_extras", 4338);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4341);

  -- attempting to read from an empty cursor will throw
  call errcheck(not cursor_with_extras, "not cursor_with_extras", 4344);
  set caught := false;
  begin try
    set blob_with_extras from cursor cursor_with_extras;
  end try;
  begin catch
    call errcheck(not cursor_with_extras, "not cursor_with_extras", 4350);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4353);

  -- the types are all wrong but they are simply not null values of the same types
  -- we can safely decode that
  declare blob_nullables blob<storage_nullable>;
  set any_blob := stash_notnulls;
  set blob_nullables := any_blob;
  declare cursor_nullables cursor like storage_nullable;
  fetch cursor_nullables from blob_nullables;

  -- note that we read the not null versions of the fields
  call errcheck(cursor_nullables, "cursor_nullables", 4364);
  call errcheck(cursor_nullables.t == cursor_both.t_nn, "cursor_nullables.t == cursor_both.t_nn", 4365);
  call errcheck(cursor_nullables.f == cursor_both.f_nn, "cursor_nullables.f == cursor_both.f_nn", 4366);
  call errcheck(cursor_nullables.i == cursor_both.i_nn, "cursor_nullables.i == cursor_both.i_nn", 4367);
  call errcheck(cursor_nullables.l == cursor_both.l_nn, "cursor_nullables.l == cursor_both.l_nn", 4368);
  call errcheck(cursor_nullables.r == cursor_both.r_nn, "cursor_nullables.r == cursor_both.r_nn", 4369);
  call errcheck(cursor_nullables.bl == cursor_both.bl_nn, "cursor_nullables.bl == cursor_both.bl_nn", 4370);
  call errcheck(cursor_nullables.str == cursor_both.str_nn, "cursor_nullables.str == cursor_both.str_nn", 4371);

  -- now blob_nullables really does have nullable types
  set blob_nullables from cursor cursor_nullables;
  set any_blob := blob_nullables;
  set blob_notnulls := any_blob;

  -- we can't read possibly null types into not null types
  set caught := false;
  begin try
    fetch test_cursor_notnulls from blob_notnulls;
  end try;
  begin catch
    call errcheck(not test_cursor_notnulls, "not test_cursor_notnulls", 4384);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4387);

  -- set up a totally different stored blob
  declare cursor_other cursor like storage_one_int;
  fetch cursor_other using 5 x;
  declare blob_other blob<storage_one_int>;
  set blob_other from cursor cursor_other;
  declare test_cursor_other cursor like cursor_other;
  fetch test_cursor_other from blob_other;
  call errcheck(test_cursor_other, "test_cursor_other", 4396);
  call errcheck(test_cursor_other.x = cursor_other.x, "test_cursor_other.x = cursor_other.x", 4397);

  set any_blob := blob_other;
  set blob_nullables := any_blob;

  -- the types in this blob do not match the cursor we're going to use it with
  set caught := false;
  begin try
    fetch cursor_nullables from blob_nullables;
  end try;
  begin catch
    call errcheck(not cursor_nullables, "not cursor_nullables", 4408);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4411);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "blob_serialization")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "blob_serialization")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_blob_serialization(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "blob_serialization", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_blob_serialization_null_cases() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare cursor_nulls cursor like storage_nullable;
  fetch cursor_nulls using
    null f, null t, null i, null l, null r, null bl, null str;

  declare blob_nulls blob<storage_nullable>;
  set blob_nulls from cursor cursor_nulls;
  declare test_cursor cursor like cursor_nulls;
  fetch test_cursor from blob_nulls;

  call errcheck(test_cursor, "test_cursor", 4425);
  call errcheck(test_cursor.t is null, "test_cursor.t is null", 4426);
  call errcheck(test_cursor.f is null, "test_cursor.f is null", 4427);
  call errcheck(test_cursor.i is null, "test_cursor.i is null", 4428);
  call errcheck(test_cursor.l is null, "test_cursor.l is null", 4429);
  call errcheck(test_cursor.r is null, "test_cursor.r is null", 4430);
  call errcheck(test_cursor.bl is null, "test_cursor.bl is null", 4431);
  call errcheck(test_cursor.str is null, "test_cursor.str is null", 4432);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "blob_serialization_null_cases")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "blob_serialization_null_cases")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_blob_serialization_null_cases(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "blob_serialization_null_cases", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_corrupt_blob_deserialization() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let a_blob := blob_from_string("a blob");
  let b_blob := blob_from_string("b blob");
  declare cursor_both cursor like storage_both;
  fetch cursor_both using
      false f, true t, 22 i, 33L l, 3.14 r, a_blob bl, "text" str,
      false f_nn, true t_nn, 88 i_nn, 66L l_nn, 6.28 r_nn, b_blob bl_nn, "text2" str_nn;

  declare blob_both blob<storage_both>;
  set blob_both from cursor cursor_both;
  if blob_both is null throw;

  -- sanity check the decode of the full blob
  declare test_cursor_both cursor like cursor_both;
  fetch test_cursor_both from blob_both;

  -- sanity check the blob size of the full encoding
  let full_size := get_blob_size(blob_both);
  call errcheck(full_size > 50, "full_size > 50", 4454);
  call errcheck(full_size < 100, "full_size < 100", 4455);

  -- try truncated blobs of every size
  let i := 0;
  while i < full_size
  begin
    declare blob_broken blob<storage_both>;
    set blob_broken := create_truncated_blob(blob_both, i);
    -- the types in this blob do not match the cursor we're going to use it with
    let caught := false;
    begin try
      -- this is gonna fail
      fetch cursor_both from blob_broken;
    end try;
    begin catch
      call errcheck(not cursor_both, "not cursor_both", 4470);
      set caught := true;
    end catch;
    call errcheck(caught, "caught", 4473);
    set i := i + 1;
  end;

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "corrupt_blob_deserialization")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "corrupt_blob_deserialization")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_corrupt_blob_deserialization(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "corrupt_blob_deserialization", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_bogus_varint() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let control_blob := (select X'490001'); -- one byte zigzag encoding of -1
  declare test_blob blob<storage_one_int>;
  set test_blob := control_blob;
  declare C cursor like storage_one_int;

  -- correctly encoded control case
  fetch C from test_blob;
  call errcheck(C, "C", 4487);
  call errcheck(C.x == -1, "C.x == -1", 4488);

  -- this int has 6 bytes, 5 is the most you can need
  let bogus_int := (select X'4900818181818100');

  set test_blob := bogus_int;

  let caught := false;
  begin try
    -- this is gonna fail
    fetch C from test_blob;
  end try;
  begin catch
    call errcheck(not C, "not C", 4501);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4504);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bogus_varint")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bogus_varint")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bogus_varint(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bogus_varint", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_bogus_varlong() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let control_blob := (select X'4C0001'); -- one byte zigzag encoding of -1
  declare test_blob blob<storage_one_long>;
  set test_blob := control_blob;
  declare C cursor like storage_one_long;

  -- correctly encoded control case
  fetch C from test_blob;
  call errcheck(C, "C", 4515);
  call errcheck(C.x == -1, "C.x == -1", 4516);

  -- this long has 11 bytes, 10 is the most you can need
  let bogus_long := (select X'4C008181818181818181818100');

  set test_blob := bogus_long;

  let caught := false;
  begin try
    -- this is gonna fail
    fetch C from test_blob;
  end try;
  begin catch
    call errcheck(not C, "not C", 4529);
    set caught := true;
  end catch;
  call errcheck(caught, "caught", 4532);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "bogus_varlong")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "bogus_varlong")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_bogus_varlong(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "bogus_varlong", start_refs, end_refs)); set fails := fails + 1; end if;

*/

create proc round_trip_int(value integer not null)
begin
  DECLARE C cursor LIKE storage_one_int;
  FETCH C using value x;
  call errcheck(C.x == value, "C.x == value", 4539);
  declare int_blob blob<storage_one_int>;
  set int_blob from cursor C;
  DECLARE D cursor like C;
  fetch D from int_blob;
  call errcheck(C.x == D.x, "C.x == D.x", 4544);
end;

create proc round_trip_long(value long not null)
begin
  DECLARE C cursor LIKE storage_one_long;
  FETCH C using value x;
  call errcheck(C.x == value, "C.x == value", 4551);
  declare int_blob blob<storage_one_long>;
  set int_blob from cursor C;
  DECLARE D cursor like C;
  fetch D from int_blob;
  call errcheck(C.x == D.x, "C.x == D.x", 4556);
end;

declare const group long_constants (
  long_const_1 = -9223372036854775807L,
  long_const_2 = -9223372036854775808L,
  long_const_3 = -9223372036854775808
);

@emit_constants long_constants;

create procedure test_verify_long_constant_forms() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
   let reference := long_const_1 - 1;

   call errcheck(reference = -9223372036854775808L, "reference = -9223372036854775808L", 4570); call errcheck((select reference = -9223372036854775808L), "(select reference = -9223372036854775808L)", 4570);
   call errcheck(reference = -9223372036854775808, "reference = -9223372036854775808", 4571); call errcheck((select reference = -9223372036854775808), "(select reference = -9223372036854775808)", 4571);
   call errcheck(reference = const(-9223372036854775808L), "reference = const(-9223372036854775808L)", 4572); call errcheck((select reference = const(-9223372036854775808L)), "(select reference = const(-9223372036854775808L))", 4572);
   call errcheck(reference = const(-9223372036854775808), "reference = const(-9223372036854775808)", 4573); call errcheck((select reference = const(-9223372036854775808)), "(select reference = const(-9223372036854775808))", 4573);
   call errcheck(reference = long_const_2, "reference = long_const_2", 4574); call errcheck((select reference = long_const_2), "(select reference = long_const_2)", 4574);
   call errcheck(reference = long_const_3, "reference = long_const_3", 4575); call errcheck((select reference = long_const_3), "(select reference = long_const_3)", 4575);

   LET x := -9223372036854775808L;
   call errcheck(reference == x, "reference == x", 4578); call errcheck((select reference == x), "(select reference == x)", 4578);

   SET x := -9223372036854775808;
   call errcheck(reference == x, "reference == x", 4581); call errcheck((select reference == x), "(select reference == x)", 4581);

   SET x := const(-9223372036854775808L);
   call errcheck(reference == x, "reference == x", 4584); call errcheck((select reference == x), "(select reference == x)", 4584);

   SET x := const(-9223372036854775808);
   call errcheck(reference == x, "reference == x", 4587); call errcheck((select reference == x), "(select reference == x)", 4587);

   SET x := long_const_2;
   call errcheck(reference == x, "reference == x", 4590); call errcheck((select reference == x), "(select reference == x)", 4590);

   SET x := long_const_3;
   call errcheck(reference == x, "reference == x", 4593); call errcheck((select reference == x), "(select reference == x)", 4593);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "verify_long_constant_forms")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "verify_long_constant_forms")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_verify_long_constant_forms(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "verify_long_constant_forms", start_refs, end_refs)); set fails := fails + 1; end if;

/*
create procedure test_serialization_tricky_values() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call round_trip_int(0);
  call round_trip_int(1);
  call round_trip_int(-1);
  call round_trip_int(129);
  call round_trip_int(32769);
  call round_trip_int(-129);
  call round_trip_int(-32769);
  call round_trip_int(0x7fffffff);
  call round_trip_int(-214783648);

  call round_trip_long(0);
  call round_trip_long(1);
  call round_trip_long(-1);
  call round_trip_long(129);
  call round_trip_long(32769);
  call round_trip_long(-129);
  call round_trip_long(-32769);
  call round_trip_long(0x7fffffffL);
  call round_trip_long(-214783648L);
  call round_trip_long(0x7fffffffffffffffL); -- max int64
  call round_trip_long(0x8000000000000000L); -- min int64

  -- these are actually testing constant handling rather than
  -- the blob but this is a convenient way to ensure that it was
  -- all on the up and up. Especially since we already confirmed
  -- above that it works in hex.
  call round_trip_long(-9223372036854775808L); -- min int64 in decimal
  call round_trip_long(-9223372036854775808); -- min int64 in decimal
  call round_trip_long(9223372036854775807L); -- max int64 in decimal
  call round_trip_long(9223372036854775807); -- max int64 in decimal
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "serialization_tricky_values")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "serialization_tricky_values")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_serialization_tricky_values(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "serialization_tricky_values", start_refs, end_refs)); set fails := fails + 1; end if;

declare proc rand_reset();
declare proc corrupt_blob_with_invalid_shenanigans(b blob not null);

create procedure test_clobber_blobs() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  -- the point of the test is to ensure that we don't segv or get ASAN failures
  -- or leak memory when dealing with broken blobs. Some of the blobs
  -- may still be valid since we corrupt them randomly. But this will
  -- help us to be sure that nothing horrible happens if you corrupt blobs

  -- we're going to make a good blob with various data in it and then clobber it
  let a_blob := blob_from_string("a blob");
  let b_blob := blob_from_string("b blob");
  declare cursor_both cursor like storage_both;
  fetch cursor_both using
      false f, true t, 22 i, 33L l, 3.14 r, a_blob bl, "text" str,
      false f_nn, true t_nn, 88 i_nn, 66L l_nn, 6.28 r_nn, b_blob bl_nn, "text2" str_nn;

  -- storage both means nullable types and not null types
  declare my_blob blob<storage_both>;
  set my_blob from cursor cursor_both;

  -- sanity check the decode of the full blob
  declare test_cursor_both cursor like storage_both;
  fetch test_cursor_both from my_blob;

  call rand_reset();

  let good := 0;
  let bad := 0;

  -- if this test fails you can use this count to set a breakpoint
  -- on the attempt that crashed, check out this value in the debugger
  let attempt := 0;

  let i := 0;
  while i < 100
  begin
     set i := i + 1;

     -- refresh the blob from the cursor, it's good now (again)
     set my_blob from cursor cursor_both;
     if my_blob is null throw;

     -- same buffer will be smashed 10 times
     let j := 0;
     while j < 10
     begin
       set j := j + 1;

       -- invoke da smasher
       call corrupt_blob_with_invalid_shenanigans(my_blob);

       begin try
         -- almost certainly going to get an error, that's fine, but no segv, no leaks, etc.
         fetch test_cursor_both from my_blob;
         set good := good + 1;
       end try;
       begin catch
         set bad := bad + 1;
       end catch;

       set attempt := attempt + 1;
     end;
  end;

  call print(printf("blob corruption results: good: %d, bad: %d\n", good, bad));
  call print(printf("1000 bad results is normal\n"));
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "clobber_blobs")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "clobber_blobs")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_clobber_blobs(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "clobber_blobs", start_refs, end_refs)); set fails := fails + 1; end if;

*/

create proc change_arg(x text)
begin
  set x := 'hi';
end;

create procedure test_arg_mutation() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  call change_arg(null);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "arg_mutation")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "arg_mutation")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_arg_mutation(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "arg_mutation", start_refs, end_refs)); set fails := fails + 1; end if;

declare proc lotsa_types() (
  i integer not null,
  l long not null,
  b bool not null,
  r real not null,
  i0 integer,
  l0 long,
  b0 bool,
  r0 real,
  t text not null,
  t0 text
);

declare function cql_cursor_hash(C cursor) long not null;

create procedure test_cursor_hash() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor like lotsa_types;
  declare D cursor like C;

  -- empty cursor hashes to nothing
  call errcheck(0 == cql_cursor_hash(C), "0 == cql_cursor_hash(C)", 4727);

  let i := 0;
  while i < 5
  begin
    -- no explicit values, all dummy
    fetch C() from values () @DUMMY_SEED(i);
    fetch D() from values () @DUMMY_SEED(i);

    let hash0 := cql_cursor_hash(C);
    let hash1 := cql_cursor_hash(C);
    let hash2 := cql_cursor_hash(D);

    call errcheck(hash0 == hash1, "hash0 == hash1", 4740); -- control for sanity
    call errcheck(hash1 == hash2, "hash1 == hash2", 4741); -- equivalent data -> same hash (not strings are dynamic)

    fetch C() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    set hash0 := cql_cursor_hash(C);
    set hash1 := cql_cursor_hash(C);
    set hash2 := cql_cursor_hash(D);

    call errcheck(hash0 == hash1, "hash0 == hash1", 4750); -- control for sanity
    call errcheck(hash1 == hash2, "hash1 == hash2", 4751); -- equivalent data -> same hash (not strings are dynamic)

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       not C.b as b;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4760); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.i + 1 as i;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4769); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.l + 1 as l;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4778); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.r + 1 as r;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4787); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       "different" as t;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4796); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       not C.b as b0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4805); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.i + 1 as i0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4814); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.l + 1 as l0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4823); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.r + 1 as r0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4832); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       "different" as t0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4841); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as b0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4850); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as i0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4859); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as l0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4868); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as r0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4877); -- now different

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as t0;

    set hash2 := cql_cursor_hash(D);
    call errcheck(hash1 != hash2, "hash1 != hash2", 4886); -- now different

    set i := i + 1;
  end;

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cursor_hash")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cursor_hash")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cursor_hash(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cursor_hash", start_refs, end_refs)); set fails := fails + 1; end if;

declare function cql_cursors_equal(C1 cursor, C2 cursor) bool not null;

create procedure test_cursor_equal() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  declare C cursor like lotsa_types;
  declare D cursor like C;

  -- empty cursor hashes to nothing
  call errcheck(cql_cursors_equal(C, D), "cql_cursors_equal(C, D)", 4900);

  -- one cursor empty
  fetch C() from values () @DUMMY_SEED(0);
  call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4904);
  call errcheck(NOT cql_cursors_equal(D, C), "NOT cql_cursors_equal(D, C)", 4905);

  let i := 0;
  while i < 5
  begin
    -- no explicit values, all dummy
    fetch C() from values () @DUMMY_SEED(i);
    fetch D() from values () @DUMMY_SEED(i);

    call errcheck(cql_cursors_equal(C, C), "cql_cursors_equal(C, C)", 4914); -- control for sanity
    call errcheck(cql_cursors_equal(C, D), "cql_cursors_equal(C, D)", 4915); -- control for sanity

    fetch C() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    call errcheck(cql_cursors_equal(C, C), "cql_cursors_equal(C, C)", 4920); -- control for sanity
    call errcheck(cql_cursors_equal(C, D), "cql_cursors_equal(C, D)", 4921); -- control for sanity

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       not C.b as b;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4929);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.i + 1 as i;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4937);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.l + 1 as l;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4945);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.r + 1 as r;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4953);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       "different" as t;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4961);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       not C.b as b0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4969);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.i + 1 as i0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4977);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.l + 1 as l0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4985);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       C.r + 1 as r0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 4993);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       "different" as t0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5001);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as b0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5009);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as i0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5017);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as l0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5025);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as r0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5033);

    ---------
    fetch D() from values () @DUMMY_SEED(i) @DUMMY_NULLABLES;

    update cursor D using
       NULL as t0;

    call errcheck(NOT cql_cursors_equal(C, D), "NOT cql_cursors_equal(C, D)", 5041);

    set i := i + 1;
  end;

  -- different number of columns
  declare E cursor like select 1 x;
  call errcheck(NOT cql_cursors_equal(C, E), "NOT cql_cursors_equal(C, E)", 5048);

/* Lua has no type info or offsets for the cursor so this test is meaningless
  -- different types (same offset)
  declare F cursor like select 1L x;
  -- call errcheck(NOT cql_cursors_equal(E, F), "NOT cql_cursors_equal(E, F)", 5052);

  -- different offsets (this is checked before types)
  declare G cursor like select 1L x, 1L y;
  declare H cursor like select 1 x, 1 y;
  -- call errcheck(NOT cql_cursors_equal(G, H), "NOT cql_cursors_equal(G, H)", 5057);
*/

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cursor_equal")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cursor_equal")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cursor_equal(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cursor_equal", start_refs, end_refs)); set fails := fails + 1; end if;

DECLARE PROC get_rows(result object not null) OUT UNION (x INTEGER NOT NULL, y TEXT NOT NULL, z BOOL);

create procedure test_child_results() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let p := cql_partition_create();

  declare v cursor like (x integer not null, y text not null, z bool);
  declare k cursor like v(x, y);

  -- empty cursors, not added to partition
  let added := cql_partition_cursor(p, k, v);
  call errcheck(not added, "not added", 5071);

  let i := 0;

  while i < 10
  begin
    fetch v() from values() @DUMMY_SEED(i) @DUMMY_NULLABLES;
    fetch k from v(like k);
    set added := cql_partition_cursor(p, k, v);
    call errcheck(added, "added", 5080);

    if (i % 3 == 0) THEN
      set added := cql_partition_cursor(p, k, v);
      call errcheck(added, "added", 5084);
    end if;

    if (i % 6 == 0) THEN
      set added := cql_partition_cursor(p, k, v);
      call errcheck(added, "added", 5089);
    end if;

    set i := i + 1;
  end;

  set i := -2;
  while i < 12
  begin

    if i != 6 then
      fetch k() from values() @DUMMY_SEED(i) @DUMMY_NULLABLES;
      declare rs1 object<get_rows set>;
      set rs1 := cql_extract_partition(p, k);
      let rs2 := cql_extract_partition(p, k);

      -- if we ask for the same key more than once, we should get the exact same result
      -- this is object identity we are checking here (i.e. it's the same pointer!)
      call errcheck(rs1 == rs2, "rs1 == rs2", 5107);

      declare C cursor for rs1;

      let row_count := 0;
      loop fetch C
      begin
        call errcheck(C.x == i, "C.x == i", 5114);
        call errcheck(C.y == printf("y_%d", i), "C.y == printf(\"y_%d\", i)", 5115);
        call errcheck(C.z == NOT NOT i, "C.z == NOT NOT i", 5116);
        set row_count := row_count + 1;
      end;

      switch i
        when -2, -1, 10, 11
          then call errcheck(row_count == 0, "row_count == 0", 5122);
        when 1, 2, 4, 5, 7, 8
          then call errcheck(row_count == 1, "row_count == 1", 5124);
        when 3, 9
          then call errcheck(row_count == 2, "row_count == 2", 5126);
        when 0
          then call errcheck(row_count == 3, "row_count == 3", 5128);
      end;
    end if;

    set i := i + 1;
  end;
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "child_results")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "child_results")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_child_results(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "child_results", start_refs, end_refs)); set fails := fails + 1; end if;

create proc ch1()
begin
   let i := 0;
   let base := 500;
   declare C cursor like (k1 integer, k2 text, v1 bool, v2 text, v3 real);
   declare K cursor like C(k1,k2);
   while i < 10
   begin
      -- note that 1/3 of parents do not have this child
      if i % 3 != 2 then
        fetch K() from values() @dummy_seed(base+i) @dummy_nullables;
        fetch C(like K) from values(from K) @dummy_seed(base+i*2) @dummy_nullables;
        out union C;
        fetch C(like K) from values(from K) @dummy_seed(base+i*2+1) @dummy_nullables;
        out union C;
      end if;
      set i := i + 1;
   end;
end;

create proc ch2()
begin
   let i := 0;
   let base := 1000;
   declare C cursor like (k3 integer, k4 text, v1 bool, v2 text, v3 real);
   declare K cursor like C(k3, k4);
   while i < 10
   begin
      -- note that 1/3 of parents do not have this child
      if i % 3 != 1 then
        fetch K() from values() @dummy_seed(base+i) @dummy_nullables;
        fetch C(like K) from values(from K) @dummy_seed(base+i*2) @dummy_nullables;
        out union C;
        fetch C(like K) from values(from K) @dummy_seed(base+i*2+1) @dummy_nullables;
        out union C;
      end if;
      set i := i + 1;
   end;
end;


create proc parent()
begin
   let i := 0;
   declare C cursor like (k1 integer, k2 text, k3 integer, k4 text, v1 bool, v2 text, v3 real);
   declare D cursor like C;
   while i < 10
   begin
      fetch C() from values() @dummy_seed(i) @dummy_nullables;

      -- child1 keys are +500
      fetch D() from values() @dummy_seed(i+500) @dummy_nullables;
      update cursor C using D.k1 k1, D.k2 k2;

      -- child2 keys are +1000
      fetch D() from values() @dummy_seed(i+1000) @dummy_nullables;
      update cursor C using D.k3 k3, D.k4 k4;

      out union C;
      set i := i + 1;
   end;
end;

create proc parent_child()
begin
  OUT UNION CALL parent() JOIN
     call ch1() USING (k1, k2) AND
     call ch2() USING (k3, k4);
end;

create procedure test_parent_child_results() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let i := 0;
  declare P cursor for call parent_child();
  loop fetch P
  begin
     -- call printf("%d) %d %s %d %s\n", i, P.k1, P.k2, P.k3, P.k4);
     call errcheck(P.k1 == i+500, "P.k1 == i+500", 5212);
     call errcheck(P.k2 == printf("k2_%d", i+500), "P.k2 == printf(\"k2_%d\", i+500)", 5213);
     call errcheck(P.k3 == i+1000, "P.k3 == i+1000", 5214);
     call errcheck(P.k4 == printf("k4_%d", i+1000), "P.k4 == printf(\"k4_%d\", i+1000)", 5215);
     call errcheck(P.k4 == printf("k4_%d", i+1000), "P.k4 == printf(\"k4_%d\", i+1000)", 5216);
     call errcheck(P.v1 == not not i, "P.v1 == not not i", 5217);
     call errcheck(P.v2 == printf("v2_%d", i), "P.v2 == printf(\"v2_%d\", i)", 5218);
     call errcheck(P.v3 == i, "P.v3 == i", 5219);

     let count_rows := 0;
     declare C1 cursor for P.child1;
     loop fetch C1
     begin
        -- call printf("  child1: %d %s %d %s %f\n", C1.k1, C1.k2, C1.v1, C1.v2, C1.v3);
        call errcheck(P.k1 == C1.k1, "P.k1 == C1.k1", 5226);
        call errcheck(P.k2 == C1.k2, "P.k2 == C1.k2", 5227);
        call errcheck(C1.v1 == not not 500 + i*2 + count_rows, "C1.v1 == not not 500 + i*2 + count_rows", 5228);
        call errcheck(C1.v2 == printf("v2_%d", 500 + i*2 + count_rows), "C1.v2 == printf(\"v2_%d\", 500 + i*2 + count_rows)", 5229);
        call errcheck(C1.v3 == 500 + i*2 + count_rows, "C1.v3 == 500 + i*2 + count_rows", 5230);
        set count_rows := count_rows + 1;
     end;

     call errcheck(count_rows == case when i % 3 == 2 then 0 else 2 end, "count_rows == case when i % 3 == 2 then 0 else 2 end", 5234);

     set count_rows := 0;
     declare C2 cursor for P.child2;
     loop fetch C2
     begin
        -- call printf("  child2: %d %s %d %s %f\n", C2.k3, C2.k4, C2.v1, C2.v2, C2.v3);
        call errcheck(P.k3 == C2.k3, "P.k3 == C2.k3", 5241);
        call errcheck(P.k4 == C2.k4, "P.k4 == C2.k4", 5242);
        call errcheck(C2.v1 == not not 1000 + i*2 + count_rows, "C2.v1 == not not 1000 + i*2 + count_rows", 5243);
        call errcheck(C2.v2 == printf("v2_%d", 1000 + i*2 + count_rows), "C2.v2 == printf(\"v2_%d\", 1000 + i*2 + count_rows)", 5244);
        call errcheck(C2.v3 == 1000 + i*2 + count_rows, "C2.v3 == 1000 + i*2 + count_rows", 5245);
        set count_rows := count_rows + 1;
     end;

     call errcheck(count_rows = case when i % 3 == 1 then 0 else 2 end, "count_rows = case when i % 3 == 1 then 0 else 2 end", 5249);

     set i := i + 1;
  end;

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "parent_child_results")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "parent_child_results")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_parent_child_results(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "parent_child_results", start_refs, end_refs)); set fails := fails + 1; end if;

create procedure test_string_dictionary() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;

  let i := 1;
  while i <= 512
  begin
     let dict := cql_string_dictionary_create();

     let j := 0;
     while j < i
     begin
       let added := cql_string_dictionary_add(dict, printf("%d", j), printf("%d", j*100));
       call errcheck(added, "added", 5267);

       set added := cql_string_dictionary_add(dict, printf("%d", j), "0");
       call errcheck(NOT added, "NOT added", 5270);
       set j := j + 2;
     end;

     set j := 0;
     while j < i
     begin
       let result := cql_string_dictionary_find(dict, printf("%d", j));
       call errcheck(case when j % 2 then result IS NULL else result == printf("%d", j*100) end, "case when j % 2 then result IS NULL else result == printf(\"%d\", j*100) end", 5278);
       set j := j + 1;
     end;

     set i := i * 2;
  end;

  -- test null lookup, always fails
  call errcheck(cql_string_dictionary_find(dict, NULL) IS NULL, "cql_string_dictionary_find(dict, NULL) IS NULL", 5286);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "string_dictionary")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "string_dictionary")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_string_dictionary(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "string_dictionary", start_refs, end_refs)); set fails := fails + 1; end if;

DECLARE FUNCTION _cql_contains_column_def(haystack TEXT, needle TEXT) BOOL NOT NULL;

-- _cql_contains_column_def is used by the upgrader to find string matches the indicate a column is present
-- it's the same as this expression: haystack GLOB printf('*[) ]%s*', needle)
-- any null arguments yield a false result
create procedure test_cql_contains_column_def() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;

  -- trivial cases all fail, the "needle" has to be reasonable to even have a chance to match
  call errcheck(NOT _cql_contains_column_def(null, 'x'), "NOT _cql_contains_column_def(null, 'x')", 5298);
  call errcheck(NOT _cql_contains_column_def('x', NULL), "NOT _cql_contains_column_def('x', NULL)", 5299);
  call errcheck(NOT _cql_contains_column_def('', 'bar'), "NOT _cql_contains_column_def('', 'bar')", 5300);
  call errcheck(NOT _cql_contains_column_def('foo', ''), "NOT _cql_contains_column_def('foo', '')", 5301);

  call errcheck(_cql_contains_column_def("create table foo(x integer)", "x integer"), "_cql_contains_column_def(\"create table foo(x integer)\", \"x integer\")", 5303);
  call errcheck(NOT _cql_contains_column_def("create table foo(xx integer)", "x integer"), "NOT _cql_contains_column_def(\"create table foo(xx integer)\", \"x integer\")", 5304);
  call errcheck(_cql_contains_column_def("create table foo(id integer, x integer)", "x integer"), "_cql_contains_column_def(\"create table foo(id integer, x integer)\", \"x integer\")", 5305);
  call errcheck(NOT _cql_contains_column_def("create table foo(id integer, xx integer)", "x integer"), "NOT _cql_contains_column_def(\"create table foo(id integer, xx integer)\", \"x integer\")", 5306);

  -- it's expecting normalized text so non-canonical matches don't count
  call errcheck(NOT _cql_contains_column_def("create table foo(id integer, x Integer)", "x integer"), "NOT _cql_contains_column_def(\"create table foo(id integer, x Integer)\", \"x integer\")", 5309);

  -- column name at the start isn't a match, there has to be a space or paren
  call errcheck(NOT _cql_contains_column_def("x integer", "x integer"), "NOT _cql_contains_column_def(\"x integer\", \"x integer\")", 5312);

end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cql_contains_column_def")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cql_contains_column_def")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cql_contains_column_def(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cql_contains_column_def", start_refs, end_refs)); set fails := fails + 1; end if;

-- cql utilities for making a basic string list
-- this is not a very functional list but schema helpers might need
-- generic lists of strings so we offer these based on bytebuf

DECLARE cql_string_list TYPE OBJECT<cql_string_list>;
DECLARE FUNCTION cql_string_list_create() CREATE cql_string_list;
DECLARE FUNCTION cql_string_list_get_string(list cql_string_list, index_ INTEGER NOT NULL) TEXT;
DECLARE FUNCTION cql_string_list_get_count(list cql_string_list) INTEGER NOT NULL;
DECLARE PROCEDURE cql_string_list_add_string(list cql_string_list, string TEXT NOT NULL);

create procedure test_cql_string_list() begin begin try set tests := tests + 1; declare starting_fails integer not null; set starting_fails := fails;
  let list := cql_string_list_create();
  call errcheck(0 == cql_string_list_get_count(list), "0 == cql_string_list_get_count(list)", 5328);
  CALL cql_string_list_add_string(list, "hello");
  CALL cql_string_list_add_string(list, "goodbyte");
  call errcheck(2 == cql_string_list_get_count(list), "2 == cql_string_list_get_count(list)", 5331);
  call errcheck("hello" == cql_string_list_get_string(list, 0), "\"hello\" == cql_string_list_get_string(list, 0)", 5332);
  call errcheck("goodbyte" == cql_string_list_get_string(list, 1), "\"goodbyte\" == cql_string_list_get_string(list, 1)", 5333);
end try; begin catch call print(printf("%s had an unexpected CQL exception (usually a db error)\n", "cql_string_list")); set fails := fails + 1; throw; end catch; if starting_fails != fails then call print(printf("%s failed.\n", "cql_string_list")); else set tests_passed := tests_passed + 1; end if; end; set start_refs := get_outstanding_refs(); call test_cql_string_list(); set end_refs := get_outstanding_refs(); if start_refs != end_refs then call print(printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", "cql_string_list", start_refs, end_refs)); set fails := fails + 1; end if;

call end_suite();

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
     -- the sqlite3_errmsg() result. Tracing lets you see the error as it happens.
     drop table if exists does_not_exist;
     -- now we save the code
     throw;
   end catch;
end;

-- Called in the test client to verify that we hit tripwires when passing NULL
-- inappropriately for various argument types and at various argument indices.
create proc proc_with_notnull_args(
  a text not null,
  b text not null,
  out c text not null,
  out d text not null,
  inout e text not null,
  inout f text not null,
  inout g text not null,
  inout h text not null,
  i text not null,
  out j text not null,
  inout k text not null,
  inout l text not null,
)
begin
  set c := "text";
  set d := "text";
  set j := "text";
end;

@echo c,"#undef cql_error_trace\n";
@echo c,"#define cql_error_trace()\n";

@emit_enums;

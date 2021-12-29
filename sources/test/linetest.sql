/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare proc printf no check;

-- testing line numbers for simple statement boundaries, any statement will do
-- something with no "interior" works best.
create proc based_statements()
begin
  declare x integer not null;
  set x := 1;
  set x := 2;
  set x := 3;
  @echo c, "/* hello "; -- no line info for these!
  @echo c, "world \n";  -- no line info for these!
  set x := 4;
  set x := 5;
end;

-- testing simple if statements in various forms
create proc if_test()
begin
  declare x integer not null;
  if x = 1 then
     set x := 10;
  end if;

  if x = 2 then
     set x := 21;
  else
     set x := 22;
  end if;

  if x = 3 then
     set x := 31;
  else if x = 3  then
     set x := 32;
  else
     set x := 32;
  end if;
end;

-- testing if and case together with a coalesce thrown in
create proc case_if()
begin
  if 1 then
    call printf("one");
  else
    call printf("two");
  end if;

  declare x integer;

  set x :=  coalesce( case 
             when  1 
           then 200
         when 2
   then 300
    end, 
    3000);
end;

-- testing the in predicate by itself
create proc in_pred_lines(i integer not null, out b bool not null )
begin
  set b := i in (
           1,
           3,
           7);
end;

-- testing in predicate mixed with case
create proc in_pred_and_case(i integer not null, out b bool not null )
begin
  set b := case when
    i > 8
    then
      i in (
      10,
      12,
      14)
    else
      i in (
      1,
      3,
      7)
    end;
end;

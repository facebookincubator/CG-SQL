/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- this is not going to be defined so calling it will crash lua to exit with a fail stack
declare proc fatal_error();
declare proc print no check;

create proc expect(true_expr bool not null)
begin
  if not true_expr then
     call fatal_error();
  end if;
end;

create proc go()
begin
  call print("testing code gen for min long and max long");

  let min_l0 := -9223372036854775808; -- min long
  let min_l1 := 0x8000000000000000;   -- this also  min_long
  let min_l2 := (1L<<63);              -- min long with arithmetic
  let min_l3 := const(1L<<63);         -- min long with constant folding

  let max_l0 := 9223372036854775807;  -- max long
  let max_l1 := 0x7fffffffffffffff;   -- this is also max_long
  let max_l2 := ~(1L<<63);             -- max long with arithmetic
  let max_l3 := const(~(1L<<63));      -- max long with constant folding

  -- several ways of generating min long
  call expect(min_l0 == min_l1);
  call expect(min_l0 == min_l2);
  call expect(min_l0 == min_l3);
  -- several ways of generating max long
  call expect(max_l0 == max_l1);
  call expect(max_l0 == max_l2);
  call expect(max_l0 == max_l3);
  -- no rounding or float wierdness going on, exact integer check

  call expect(min_l0 + max_l0 == -1);
  call expect(min_l1 + max_l0 == -1);
  call expect(min_l2 + max_l0 == -1);
  call expect(min_l3 + max_l0 == -1);
  call expect(-9223372036854775808 + 9223372036854775807  == -1); 
  call expect(9223372036854775807 + -9223372036854775808  == -1); 
  call expect(0x7fffffffffffffff + 0x8000000000000000  == -1); 
  call expect(0x8000000000000000 + 0x7fffffffffffffff   == -1); 

  -- if you don't do the constants just write you get errors
  -- here we expect the error to illustrate the problem
  -- we can never generate the literal -9223372036854775808 
  -- if we do it gets converted to a float

  @echo lua, "\n-- this code gen would be wrong, note the expect says ~=\n";
  @echo lua, "-- we should expect == here but min_long will be rounded\n";
  @echo lua, "-- we must generate (-9223372036854775807-1) as above\n";
  @echo lua, "expect(-1 ~= -9223372036854775808 + 9223372036854775807)\n";
  
end;

@echo lua, "go(sqlite3.open_memory())\n";

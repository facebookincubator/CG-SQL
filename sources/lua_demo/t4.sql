/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@attribute(cql:shared_fragment)
create proc foo(x integer)
begin
   if x is 7 then
     select 9 x, 8 y;
   else if x is not null then
     select x x, 1 y
     union all
     select x+1 x, 2 y;
   else
     select 999 x, 998 y;
   end if;
end;

create proc bar(config integer)
begin
   with
     x(*) as (call foo(config)),
     y(*) as (call foo(config+100))
   select x.*, y.x as a, y.y as b from x join y;
end;

declare proc print no check;

create proc dump(config integer)
begin
   call print("dumping results for config value:", config);
   declare C cursor for call bar(config);
   loop fetch C
   begin
     call print(C.x, C.y, C.a, C.b);
   end;
end;

create proc go()
begin
  call dump(7);
  call dump(null);
  call dump(20);
end;

@echo lua, "go(sqlite3.open_memory())\n";

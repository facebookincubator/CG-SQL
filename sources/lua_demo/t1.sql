/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create table foo(x integer);
declare proc print no check;

create proc foo()
begin
   let z := case when 5 then 3 else 7 end;
   switch z
   when 1, 3 then set z := 5;
   when 2, 4 then set z := 6;
   else set z := 7;
   end;

   while 1 
   begin
     while 2
     begin
        continue;
     end;
     continue;
   end;
end;

create proc curs(out x integer)
begin
  declare C cursor for select 1 x, 2 y;
  fetch C;
  out C;
end;

create proc bar(u integer not null)
begin
  declare C cursor for select u+1 x, u+2 y;
  let z := 0L;
  loop fetch C
  begin
    set z := z + C.x + C.y;
  end;
end;

create proc fib(n integer not null, out result integer not null)
begin
   if n <= 2 then
       set result := 1;
   else
       set result := fib(n-1) + fib(n-2);
   end if;
end;

create proc out_stmt_dml_proc()
begin
  declare C cursor for select 157 x;
  fetch C;
  out C;
end;

create proc out_stmt_proc_helper()
begin
  declare C cursor like (x integer);
  fetch C from values (287);
  out C;
end;

create proc out_stmt_proc()
begin
  declare C cursor fetch from call out_stmt_proc_helper();
  out C;
end;

create proc f1()
begin
  select * from foo where x > 100;
end;

create proc use_f(x integer)
begin
  call f1();
end;

declare proc read_stuff() using transaction;

@echo lua, "function read_stuff(db)\n";
@echo lua, "  local rc\n";
@echo lua, "  local result\n";
@echo lua, "  rc, result = use_f_fetch_results(db, 5)\n";
@echo lua, "  print(rc, #result, result[1].x)\n";
@echo lua, "  rc, result = out_stmt_dml_proc_fetch_results(db)\n";
@echo lua, "  print(rc, #result, result[1].x)\n";
@echo lua, "  result = out_stmt_proc_fetch_results()\n";
@echo lua, "  print(#result, result[1].x)\n";
@echo lua, "  return rc\n";
@echo lua, "end\n\n";

create proc go()
begin
   create table foo(x integer);
   insert into foo values(105);
   insert into foo values(6);
   call read_stuff();

   let i := 1;
   while i <= 8
   begin
      call print(printf("i:%2d  fib_i:%2d", i, fib(i)));
      set i := i + 1;
   end;
end;

@echo lua, "go(sqlite3.open_memory())\n";

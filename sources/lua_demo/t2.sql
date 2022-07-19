/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare proc print no check;

create proc foo(out x integer )
begin
   set x := (select 5);
end;

create proc bar(out x integer )
begin
   set x := 5;
end;

create proc out_union()
begin
   declare C cursor like (id integer, name text);
   let i := 1;
   while i <= 5
   begin
     fetch C from values(i, printf("str --%d", i));
     out union C;
     set i := i + 1;
   end;
end;

create proc out_union_dml()
begin
   declare C cursor for select 1 id, "xxx" name;
   fetch C;
   out union C;
   out union C;
end;

declare proc print no check;

create proc call_out_union()
begin
   declare C cursor for call out_union();
   loop fetch C
   begin
      call print(C.id, C.name);
   end;
end;

create proc call_out_union_fetch2()
begin
   declare C cursor for call out_union();
   fetch C;
   fetch C;
end;

create proc out_cursor()
begin
   declare C cursor for select 1 id, "xxx" name;
   fetch C;
   out C;
end;

create proc call_out()
begin
   declare C cursor fetch from call out_cursor();
end;

create proc go()
begin
  call print(foo());
  call call_out_union();
end;

@echo lua, "go(sqlite3.open_memory())\n";

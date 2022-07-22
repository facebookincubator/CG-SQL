/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare function make_str(x integer) text;
declare procedure printf no check;

create proc make_values()
begin
   let i := 0;
   while i < 25
   begin
      declare C cursor like (x integer, y text);
      fetch C from values (i, printf("%d", i));
      out union C;
      set i := i + 1;
   end;
end;


create proc print_values()
begin
   declare C cursor for call make_values();
   loop fetch C
   begin
     call printf("%d %d\n", C.x, C.y);
   end;
end;

@echo lua, "function printf(...) io.write(cql_printf(...)) end\n";
@echo lua, "print_values()\n";

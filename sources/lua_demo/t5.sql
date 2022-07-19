/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare proc print no check;

create proc make_schema()
begin
  create table p_table (
    id integer primary key,
    name text
  );

  create table c_table (
    id integer references p_table(id),
    info text
  );

  insert into p_table values (1, "foo"), (2, "bar");
  insert into c_table values (1, "1_info1"), (1, "1_info2");
  insert into c_table values (2, "2_info3"), (2, "2_info4");
  insert into c_table values (3, "3_info5"), (3, "3_info6");
end;

create proc parent()
begin
   select * from p_table;
end;

create proc child()
begin
   select * from c_table;
end;

create proc parent_child()
begin
   out union call parent() join call child() using(id);
end;

create proc child2(id_ integer not null)
begin
  select * from c_table where id = id_;
end;


create proc parent_manual_child()
begin
   let i := 1;
   while i < 3
   begin
     declare C cursor like (id integer, child1 object<child2 set>);
     fetch C from values (i, child2(i));
     out union C;
     set i := i + 1;
   end;
end;

create proc go()
begin
   call make_schema();

   call print("testing parentchild result sets created with sugar syntax");

   declare C cursor for call parent_child();
   loop fetch C
   begin
      call print("cursor C", C.id, C.name);
      declare D cursor for C.child1;
      loop fetch D
      begin
         call print("", D.id, D.info);
      end;
   end;

   call print("testing manually created child result sets");

   declare C2 cursor for call parent_manual_child();
   loop fetch C2
   begin
      call print("cursor C2", C2.id);
      declare D2 cursor for C2.child1;
      loop fetch D2
      begin
         call print("", D2.id, D2.info);
      end;
   end;
end;

@echo lua, "go(sqlite3.open_memory())\n";


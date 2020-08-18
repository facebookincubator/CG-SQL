-- (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
--
-- This file generates dbhelp.c, dbhelp.c is licensed per the below.

-- ------ cql-verify db helpers

@echo c, "// (c) Facebook, Inc. and its affiliates.\n";
@echo c, "//\n";
@echo c, "// This source code is licensed under the MIT license found in the\n";
@echo c, "// LICENSE file in the root directory of this source tree.\n";
@echo c, "//\n";

-- setup the table and the index
create procedure dbhelp_setup()
begin
  create table test_output(
     line integer not null,
     data text not null
  );

  create index __idx__test_lines on test_output (line);

  create table source_input(
     line integer not null,
     data text not null
  );

  create index __idx__source_lines on source_input (line);
end;

create procedure dbhelp_prev_line(line_ integer not null, out prev integer not null)
begin
  begin try
     set prev := (select ifnull(max(line),-1) from test_output where line < line_);
  end try;
  begin catch
     set prev := 0;
  end catch;
end;

-- add a row to the results table
create procedure dbhelp_add(line integer not null, data text not null)
begin
  insert into test_output values (line, data);
end;

create procedure dbhelp_add_source(line integer not null, data text not null)
begin
  insert into source_input values (line, data);
end;

create procedure dbhelp_dump_line(line_ integer not null)
begin
  declare C cursor for select * from test_output where line = line_;
  loop fetch C
  begin
    call printf('%s', C.data);
  end;
end;

-- find the statement that came after line_
-- search the results of that statement for the indicated pattern
create proc dbhelp_find(line_ integer not null, pattern text not null, out search_line integer not null, out found integer not null)
begin
  set search_line := (select line from test_output where line >= line_ limit 1);
  set found := (select count(*) from test_output where line = search_line and data like pattern);
end;

create procedure dbhelp_dump_source(line1 integer not null, line2 integer not null)
begin
  declare C cursor for select * from source_input where line > line1 and line <= line2;
  loop fetch C
  begin
    call printf('%s', C.data);
  end;
end;

create procedure dbhelp_source()
begin
 select * from source_input;
end;

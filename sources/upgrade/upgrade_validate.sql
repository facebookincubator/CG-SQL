/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- look at the database and see if it is ok for the indicated version
-- we can tell what version it is on by inspecting the facet tables

declare proc test_cql_get_facet_version(facet text not null, out version long not null) using transaction;
declare proc printf no check;

-- declare sqlite_master --
create table sqlite_master (
  type text not null,
  name text not null,
  tbl_name text not null,
  rootpage integer not null,
  sql text
);

create proc validate_transition()
begin
  let version := cast(test_cql_get_facet_version("cql_schema_version") as integer);

  call printf("reference results for version %d\n\n", version);

  declare C cursor for select * from sqlite_master order by name;
  loop fetch C
  begin
    call printf("----- %s -----\n\n", C.name);
    call printf("type: %s\n", C.type);
    call printf("tbl_name: %s\n", C.tbl_name);

    -- Canonicalize and put in the split markers so we get some useful line breaks consistently
    -- Different SQLite versions will either preserve whitespace or not so this is an effort to
    -- normalize.  It's not perfect but it doesn't need to be, it only needs to work for
    -- schema the test will ever see.

    let s := (select replace(C.sql, "\n", " "));
    set s := (select replace(s, " ,", ","));
    set s := (select replace(s, " )", ")"));
    set s := (select replace(s, "( ", "("));
    set s := (select replace(s, "  ", " "));
    set s := (select replace(s, ",", ",$"));
    set s := (select replace(s, "(", "($"));

    -- split the string at the $ marks
    declare lines cursor for
      with split(line, str) as (
          select '', s || '$'
        union all
          select substr(str, 1, instr(str, '$') - 1), substr(str, instr(str, '$') + 1)
        from split
        where str != '')
      select line from (select trim(line) line from split) where line != '';

    -- some standard indenting, very simple
    let indent := 0;
    loop fetch lines
    begin
      let i := 0;
      while i < indent 
      begin
        call printf("  ");
        set i := i + 1;
      end;
      call printf("%s\n", lines.line);

      -- trailing ( starts indent
      -- trailing ) ends indent
      let tail := (select substr(lines.line, length(lines.line)));
      if tail == '(' then
        set indent := indent + 1;
      else if tail == ')' then
        set indent := indent - 1;
      end if;

      -- trailing ), ends indent
      set tail := (select substr(lines.line, length(lines.line)-1));
      if tail == '),' then
        set indent := indent - 1;
      end if;
    end;

    call printf("\n");
  end;

  let unsub_sql := (
    select sql from sqlite_master
    where name = 'test_for_unsub'
    if nothing null);

  switch version
  when 0,2,4 then
    if unsub_sql is null then 
      call printf("ERROR! unsub_sql should have been present in v%d\n", version);
      throw;
    end if;
  else
    if unsub_sql is not null then 
      call printf("ERROR! unsub_sql should have been unsubscribed in v%d\n", version);
      throw;
    end if;
  end;

  let recreate_sql := (
    select sql from sqlite_master
    where name = 'test_this_table_will_become_create'
    if nothing null);

  switch version
  when 0 then
    if recreate_sql is null or recreate_sql not like '%xyzzy INTEGER%' then
      call printf("ERROR! test_this_table_will_become_create should have a column named xyzzy in v%d\n", version);
      throw;
    end if;
  when 1,2 then
    if recreate_sql is null or recreate_sql like '%xyzzy%' then
      call printf("ERROR! test_this_table_will_become_create should not have a column named xyzzy in v%d\n", version);
      throw;
    end if;
    if recreate_sql not like '%id INTEGER PRIMARY KEY%' then
      call printf("ERROR! test_this_table_will_become_create should have a column named id in v%d\n", version);
      throw;
    end if;
  when 3,4 then
    if recreate_sql is not null then
      call printf("ERROR! test_this_table_will_become_create be deleted in v%d\n", version);
      throw;
    end if;
  else
    call printf("ERROR! unexpected schema version v%d\n", version);
    throw;
  end;
end;

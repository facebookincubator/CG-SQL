/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- look at the database and see if it is ok for the indicated version
-- we can tell what version it is on by inspecting the facet tables

declare proc test_cql_get_facet_version(facet text not null, out version long not null) using transaction;

-- declare sqlite_master --
CREATE TABLE sqlite_master (
  type TEXT NOT NULL,
  name TEXT NOT NULL,
  tbl_name TEXT NOT NULL,
  rootpage INTEGER NOT NULL,
  sql TEXT
);

declare select function replace(str text, pat text, replacement text) text;

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
    -- Different SQL versions will either preserve whitespace or not so this is an effort to
    -- normalize.  It's not perfect but it doesn't need to be, it only needs to work for
    -- schema the test will ever see.

    LET s := (select replace(C.sql, "\n", " "));
    SET s := (select replace(s, " ,", ","));
    SET s := (select replace(s, " )", ")"));
    SET s := (select replace(s, "( ", "("));
    SET s := (select replace(s, "  ", " "));
    SET s := (select replace(s, ",", ",$"));
    SET s := (select replace(s, "(", "($"));

    -- split the string at the $ marks
    declare lines cursor for
      WITH split(line, str) AS (
      SELECT '', s || '$'
      UNION ALL SELECT
      substr(str, 0, instr(str, '$')),
      substr(str, instr(str, '$')+1)
      FROM split WHERE str!=''
    ) SELECT trim(line) line FROM split WHERE line != '';

    -- some standard indenting, very simple
    let indent := 0;
    loop fetch lines
    begin
      LET i := 0;
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

  let recreate_sql := (
    select sql from sqlite_master
    where name = 'test_this_table_will_become_create'
    if nothing NULL);

  switch version
  when 0 then
    if recreate_sql IS NULL OR recreate_sql not like '%xyzzy INTEGER%' then
      call printf("ERROR! test_this_table_will_become_create should have a column named xyzzy in v%d\n", version);
      throw;
    end if;
  when 1,2 then
    if recreate_sql IS NULL OR recreate_sql like '%xyzzy%' then
      call printf("ERROR! test_this_table_will_become_create should not have a column named xyzzy in v%d\n", version);
      throw;
    end if;
    if recreate_sql IS NULL OR recreate_sql not like '%id INTEGER PRIMARY KEY%' then
      call printf("ERROR! test_this_table_will_become_create should have a column named id in v%d\n", version);
      throw;
    end if;
  when 3 then
    if recreate_sql IS NOT NULL then
      call printf("ERROR! test_this_table_will_become_create be deleted in v%d\n", version);
      throw;
    end if;
  else
    call printf("ERROR! expected schema version v%d\n", version);
    throw;
  end;
end;

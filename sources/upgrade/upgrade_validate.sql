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


create proc validate_transition()
begin
  let version := cast(test_cql_get_facet_version("cql_schema_version") as integer);

  call printf("reference results for version %d\n\n", version);

  declare C cursor for select * from sqlite_master;
  loop fetch C
  begin
    call printf("----- %s -----\n\n%s\n\n", C.name, cql_cursor_format(C));
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

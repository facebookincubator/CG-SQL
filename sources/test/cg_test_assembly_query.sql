/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- test base table with combination of fields
@attribute(bar_is_good=1)
create table bar(
  id INTEGER NOT NULL,
  @attribute(collossal_cave='xyzzy')
  name TEXT,
  rate LONG INT,
  type INTEGER,
  size REAL @create(2)
);

-- test table for extension query fragment join
create table plugin_table(
  id integer primary key,
  name text,
  flag BOOL
);

-- TEST: base fragment attribute
-- - base_fragment
@attribute(cql:base_fragment=assembly_core)
create proc base_fragment(id_ integer not null, name_ text not null)
begin
with
  assembly_core(x, y, z) as (select id, name, rate from bar where id = id_)
select * from assembly_core;
end;

-- TEST: extension fragment one
-- - extension_fragment_one
@attribute(cql:extension_fragment=assembly_core)
create proc extension_fragment_one(id_ INTEGER NOT NULL, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)), -- this is stub for the core
    plugin_one(*) as (
    select * from assembly_core
    union all
    select 1 x, "y" y, 7 z)
  select * from plugin_one;
end;

-- TEST: extension fragment one
-- - extension_fragment_two
@attribute(cql:extension_fragment=assembly_core)
create proc extension_fragment_two(id_ integer not null, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)), -- this is stub for the core
    plugin_two(x, y, z, flag) as (
    select assembly_core.*, plugin_table.flag from assembly_core
    left outer join plugin_table on plugin_table.name = assembly_core.y AND plugin_table.id = id_)
  select * from plugin_two;
end;

-- TEST: assembly fragment attribute
-- + assembly_core
-- + FROM plugin_one
-- + FROM plugin_two
@attribute(cql:assembly_fragment=assembly_core)
CREATE PROC assembly_core (id_ INTEGER NOT NULL, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)) -- this is stub for the core
  select * from assembly_core;
end;

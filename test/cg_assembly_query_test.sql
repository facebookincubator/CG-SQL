-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

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
create proc extension_fragment_one(id_ integer not null, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)), -- this is stub for the core
    plugin_one(x, y, z, flag) as (
    select assembly_core.*, plugin_table.flag from assembly_core
    left outer join plugin_table on plugin_table.name = assembly_core.y AND plugin_table.id = id_)
  select * from plugin_one;
end;

-- TEST: extension fragment two
-- - extension_fragment_two
@attribute(cql:extension_fragment=assembly_core)
create proc extension_fragment_two(id_ INTEGER NOT NULL, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)), -- this is stub for the core
    plugin_two(x,y,z,name) as (
    select assembly_core.*, CAST(NULL as TEXT) as name from assembly_core
    union all
    select assembly_core.*, plugin_table.name from assembly_core inner join plugin_table on plugin_table.name = name_)
  select * from plugin_two;
end;

-- TEST: assembly fragment attribute
-- + assembly_core
@attribute(cql:assembly_fragment=assembly_core)
CREATE PROC assembly_core (id_ INTEGER NOT NULL, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)) -- this is stub for the core
  select * from assembly_core;
end;

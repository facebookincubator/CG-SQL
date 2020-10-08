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

-- second test base table
create table foo(
  id TEXT PRIMARY KEY NOT NULL,
  name TEXT,
  col INTEGER
);

-- test table for extension query fragment join
create table plugin_table(
  id INTEGER primary key,
  name TEXT,
  flag BOOL,
  timestamp_ms LONG INT NOT NULL DEFAULT 0
);

-- TEST: base fragment attribute
-- - base_fragment
-- - Error
@attribute(cql:base_fragment=assembly_core)
create proc base_fragment(id_ integer not null, name_ text not null)
begin
with
  assembly_core(x, y, z) as (select id, name, rate from bar where id = id_)
select * from assembly_core;
end;

-- TEST: extension fragment one on assembly core
-- + extension_fragment_one
-- - Error
@attribute(cql:extension_fragment=assembly_core)
create proc extension_fragment_one(id_ INTEGER NOT NULL, name_ text not null)
begin
  with
    assembly_core (x, y, z) AS (SELECT 1, nullable("a"), nullable(3L)), -- this is stub for the core
    plugin_one(*) as (
    select * from assembly_core
    union all
    select 1 x, "b" y, 7L z)
  select * from plugin_one;
end;

-- TEST: second base fragment attribute
-- - second_base_fragment
-- - Error
@attribute(cql:base_fragment=assembly_non_core)
create proc second_base_fragment(name_ TEXT not null)
begin
with
  assembly_non_core(a, b, c) as (select id, name, col from foo where name = name_)
select * from assembly_non_core;
end;

-- TEST: extension fragment two
-- + extension_fragment_two
-- - Error
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

-- TEST: extension fragment on different base
-- + extension_fragment_three
-- - Error
@attribute(cql:extension_fragment=assembly_non_core)
create proc extension_fragment_three(name_ TEXT not null)
begin
  with
    assembly_non_core (a, b, c) AS (SELECT "a", nullable("x"), nullable(1)), -- this is stub for non-core
    plugin_three(a, b, c, d) as (
    select assembly_non_core.*, plugin_table.timestamp_ms from assembly_non_core
    left outer join plugin_table on plugin_table.name = assembly_non_core.b AND plugin_table.name = name_)
  select * from plugin_three;
end;

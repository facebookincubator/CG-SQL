/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- test base table with combination of fields
create table bar(
  id INTEGER NOT NULL @sensitive,
  @attribute(collossal_cave='xyzzy')
  name TEXT @sensitive,
  rate LONG INT @sensitive,
  type INTEGER,
  size REAL @create(2)
);

-- TEST: base fragment attribute
-- - base_fragment
-- - Error
@attribute(cql:base_fragment=assembly_core)
@attribute(cql:vault_sensitive)
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
@attribute(cql:vault_sensitive)
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

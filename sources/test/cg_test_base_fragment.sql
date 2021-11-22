/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- test base table with combination of fields
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
-- - cql_cleanup
@attribute(cql:base_fragment=assembly_core)
create proc base_fragment(id_ integer not null, name_ text not null)
begin
with
  assembly_core(x, y, z) as (select id, name, rate from bar where id = id_)
select * from assembly_core;
end;

-- TEST: second base fragment attribute
-- - cql_cleanup
@attribute(cql:base_fragment=assembly_non_core)
create proc second_base_fragment(name_ TEXT not null)
begin
with
  assembly_non_core(a, b, c) as (select id, name, col from foo where name = name_)
select * from assembly_non_core;
end;

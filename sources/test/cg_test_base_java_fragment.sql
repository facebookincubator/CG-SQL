/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

-- TEST: base fragment attribute
-- - cql_cleanup
@attribute(cql:base_fragment=assembly_core)
create proc base_fragment(id_ integer not null, name_ text not null)
begin
with
  assembly_core(x, y, z) as (select id, name, rate from bar where id = id_)
select * from assembly_core;
end;

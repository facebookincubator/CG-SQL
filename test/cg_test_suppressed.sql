-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

create table bar(
  id INTEGER NOT NULL,
  name TEXT,
  rate LONG INT,
  type INTEGER,
  size REAL
);

@attribute(cql:suppress_getters)
create procedure suppressed_getters_test()
begin
  select * from bar;
end;

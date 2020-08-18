-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

-- This provides a single non-empty proc with non-nullable column of all supported
-- types

create table bar(
  intcol INTEGER,
  longcol LONG INT,
  realcol REAL,
  boolcol BOOL,
  textcol TEXT,
  blobcol BLOB
);

@attribute(cql:identity=(boolcol))
create proc non_empty_proc()
begin
  select * from bar;
end;

-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

-- This provides a single non-empty proc with non-nullable column of all supported
-- types

create table bar(
  intcol INTEGER NOT NULL,
  longcol LONG INT NOT NULL,
  realcol REAL NOT NULL,
  boolcol BOOL NOT NULL,
  textcol TEXT NOT NULL,
  blobcol BLOB NOT NULL
);

@attribute(cql:identity=(intcol, longcol))
create proc non_empty_proc()
begin
  select * from bar;
end;

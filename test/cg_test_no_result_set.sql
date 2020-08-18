-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

create table bar(
  id INTEGER NOT NULL,
  name TEXT,
  rate LONG INT,
  type INTEGER,
  size REAL
);

create procedure outparm_test(out foo integer not null)
begin
 set foo := 1;
 delete from bar where id = foo;
end;

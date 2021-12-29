/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

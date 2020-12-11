/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- A sample table.
create table bar(
  id INTEGER NOT NULL PRIMARY KEY,
  @attribute(collossal_cave='xyzzy')
  name TEXT,
  rate LONG INT,
  type INTEGER,
  size REAL @create(2)
);

-- A sample procedure.
create procedure sample_proc()
begin
end;

-- Testing @echo both with and without a newline ending
@echo c, "/* A";
@echo c, " comment */\n";

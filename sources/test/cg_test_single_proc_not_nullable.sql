/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- This provides a single non-empty proc with non-nullable column of all supported
-- types

DECLARE PROC child() (some_col text);

create table bar(
  intcol INTEGER NOT NULL @sensitive,
  longcol LONG INT NOT NULL,
  realcol REAL NOT NULL,
  boolcol BOOL NOT NULL,
  textcol TEXT NOT NULL @sensitive,
  blobcol BLOB NOT NULL @sensitive
);

@attribute(cql:identity=(intcol, longcol))
@attribute(cql:vault_sensitive=(blobcol, textcol))
@attribute(cql:custom_type_for_encoded_column)
create proc non_empty_proc()
begin
  -- get a row and attach a child result set to it
  declare C cursor for select * from bar;
  fetch C;

  declare RESULT cursor like (like C, objcol OBJECT<child set> NOT NULL @sensitive);
  fetch RESULT from values (from C, child());
  out union RESULT;
end;

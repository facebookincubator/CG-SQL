/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- This provides a single non-empty proc with non-nullable column of all supported
-- types

create table bar(
  intcol INTEGER @sensitive,
  longcol LONG INT,
  realcol REAL,
  boolcol BOOL,
  textcol TEXT,
  blobcol BLOB @sensitive
);

@attribute(cql:identity=(boolcol))
@attribute(cql:vault_sensitive)
create proc non_empty_proc()
begin
  select * from bar;
end;

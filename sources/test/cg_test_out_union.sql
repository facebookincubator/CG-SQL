/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@attribute(cql:vault_sensitive)
create proc out_union_proc()
begin
  declare x integer not null @sensitive;
  set x := 1;
  declare C cursor for select x;
  fetch c;
  out c;
end;

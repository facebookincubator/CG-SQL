/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create proc out_union_proc()
begin
  declare C cursor for select 1 x;
  fetch c;
  out c;
end;

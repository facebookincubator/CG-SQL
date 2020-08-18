-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

create proc out_union_proc()
begin
  declare C cursor for select 1 x;
  fetch c;
  out c;
end;

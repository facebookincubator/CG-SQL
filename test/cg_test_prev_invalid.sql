-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

/* you can't codegen C if previous schema mode was enabled */

create proc whatever()
begin
 select 1 x;
end;

@previous_schema;

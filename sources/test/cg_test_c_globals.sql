/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST
-- this generates nothing in the header
-- - blobshape
@attribute(cql:blob_storage)
create table blobshape(
  id integer not null,
  t text
);

-- + #ifndef _foo_var_group_decl_
-- + #define _foo_var_group_decl_ 1
-- + typedef struct c_row {
-- + cql_bool _has_row_;
-- + cql_uint16 _refs_count_;
-- + cql_uint16 _refs_offset_;
-- + cql_int32 x;
-- + } c_row;
-- + extern c_row c;
-- + extern cql_nullable_int32 x;
-- + #endif
-- + typedef struct serialized_cursor_row {
-- + cql_bool _has_row_;
-- + cql_uint16 _refs_count_;
-- + cql_uint16 _refs_offset_;
-- + cql_int32 id;
-- + cql_string_ref _Nullable t;
-- + } serialized_cursor_row;
-- + extern serialized_cursor_row serialized_cursor;
-- + extern uint16_t serialized_cursor_cols[];
-- + extern uint8_t serialized_cursor_data_types[];
declare group foo
begin
  declare c cursor like select 1 x;
  declare x integer;
  declare serialized_cursor cursor like blobshape;
end;

create proc p(x blob<blobshape>)
begin
  fetch serialized_cursor from blob x;
end;

-- TEST: When we emit the group, nothing goes in the header
-- - extern
-- - c_row
-- - nullable_int32
@emit_group foo;

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * Note this file is set up to verify the .h file rather than the .c file in test.sh
 */

create table foo (
  f1 integer not null,
  f2 text not null,
  f3 real not null,
  f4 bool not null,
  f5 long not null,
  f6 blob not null,

  g1 integer,
  g2 text,
  g3 real,
  g4 bool,
  g5 long,
  g6 blob
);

-- TEST: try the inline getters form for a a simple procedure
-- not null types
-- +1 static inline cql_int32 selector_get_f1(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_int32_col((cql_result_set_ref)result_set, row, 0);
-- +1 static inline cql_string_ref _Nonnull selector_get_f2(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_string_col((cql_result_set_ref)result_set, row, 1);
-- +1 static inline cql_double selector_get_f3(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_double_col((cql_result_set_ref)result_set, row, 2);
-- +1 static inline cql_bool selector_get_f4(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_bool_col((cql_result_set_ref)result_set, row, 3);
-- +1 static inline cql_int64 selector_get_f5(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_int64_col((cql_result_set_ref)result_set, row, 4);
-- +1 static inline cql_blob_ref _Nonnull selector_get_f6(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_blob_col((cql_result_set_ref)result_set, row, 5);
--
-- nullable int
-- +1 static inline cql_bool selector_get_g1_is_null(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 6);
-- +1 static inline cql_int32 selector_get_g1_value(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_int32_col((cql_result_set_ref)result_set, row, 6);
--
-- nullable text
-- +1 static inline cql_string_ref _Nullable selector_get_g2(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 7) ? NULL : cql_result_set_get_string_col((cql_result_set_ref)result_set, row, 7);
--
-- nullable real
-- +1 static inline cql_bool selector_get_g3_is_null(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 8);
-- +1 static inline cql_double selector_get_g3_value(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_double_col((cql_result_set_ref)result_set, row, 8);
--
-- nullable bool
-- +1 static inline cql_bool selector_get_g4_is_null(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 9);
-- +1 static inline cql_bool selector_get_g4_value(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_bool_col((cql_result_set_ref)result_set, row, 9);
--
-- nullable long
-- +1 static inline cql_bool selector_get_g5_is_null(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 10);
-- +1 static inline cql_int64 selector_get_g5_value(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +1   return cql_result_set_get_int64_col((cql_result_set_ref)result_set, row, 10);
-- +1 static inline cql_blob_ref _Nullable selector_get_g6(selector_result_set_ref _Nonnull result_set, cql_int32 row) {
--
-- nullable blob
-- +1   return cql_result_set_get_is_null_col((cql_result_set_ref)result_set, row, 11) ? NULL : cql_result_set_get_blob_col((cql_result_set_ref)result_set, row, 11);
create proc selector()
begin
  select * from foo;
end;

-- TEST: define a base fragment, no C output for this, only header stuff
-- there should be nothing at really, this pattern ensures the main proc is absent
-- - cql_cleanup
@attribute(cql:base_fragment=frag_test)
create proc baseline()
begin
  with
    frag_test(*) as (select 1 id)
  select * from frag_test;
end;

-- TEST: extension creates getters for the base columns and the new columns it added
-- + static inline cql_int32 ext_get_id(frag_test_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   extern cql_int32 frag_test_get_id(frag_test_result_set_ref _Nonnull result_set, int32_t row);
-- +   return frag_test_get_id(result_set, row);
-- + static inline cql_string_ref _Nullable ext_get_f2(frag_test_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   extern cql_string_ref _Nullable __PRIVATE__frag_test_get_f2(frag_test_result_set_ref _Nonnull result_set, int32_t row);
-- +   return __PRIVATE__frag_test_get_f2(result_set, row);
@attribute(cql:extension_fragment=frag_test)
create proc ext()
begin
  with
    frag_test(*) as (select 1 id),
    ext(*) as (select frag_test.*, f2 from frag_test left outer join foo on f1 = id)
  select * from ext;
end;

-- TEST: another extension, this one should not include anything about f2, it doesn't "know" about that column
-- - f2
@attribute(cql:extension_fragment=frag_test)
create proc ext2()
begin
  with
    frag_test(*) as (select 1 id),
    ext2(*) as (select frag_test.*, f3 from frag_test left outer join foo on f1 = id)
  select * from ext2;
end;

-- TEST: emit an object result set with type getters
-- + static inline cql_object_ref _Nonnull emit_object_result_set_get_o(emit_object_result_set_result_set_ref _Nonnull result_set, cql_int32 row) {
-- +   return cql_result_set_get_object_col((cql_result_set_ref)result_set, row, 0);
create proc emit_object_result_set(o object not null)
begin
   declare C cursor like emit_object_result_set arguments;
   fetch C from arguments;
   out union C;
end;

-- TEST: a copy function will be generated
-- + #define sproc_copy_func_copy(result_set, result_set_to, from, count)
@attribute(cql:generate_copy)
create proc sproc_copy_func()
begin
  select * from foo;
end;

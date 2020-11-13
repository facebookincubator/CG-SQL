/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <sqlite3.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef __cplusplus
#define CQL_EXTERN_C_BEGIN extern "C" {
#define CQL_EXTERN_C_END }
#else
#define CQL_EXTERN_C_BEGIN
#define CQL_EXTERN_C_END
#endif // __cplusplus

#define CQL_EXPORT extern __attribute__((visibility("default")))
#define CQL_WARN_UNUSED __attribute__((warn_unused_result))

CQL_EXTERN_C_BEGIN

// Define CQL_COMPAT_VERSION_NUMBER in order to override and test compatibility APIs with any real version of sqlite.
static inline int cql_sqlite3_libversion_number(void) {
#ifdef CQL_COMPAT_VERSION_NUMBER
  return CQL_COMPAT_VERSION_NUMBER;
#else // CQL_COMPAT_VERSION_NUMBER
  return sqlite3_libversion_number();
#endif // CQL_COMPAT_VERSION_NUMBER
}

typedef struct cql_nullable_int32 {
 cql_bool is_null;
 cql_int32 value;
} cql_nullable_int32;

typedef struct cql_nullable_int64 {
 cql_bool is_null;
 cql_int64 value;
} cql_nullable_int64;

typedef struct cql_nullable_double {
 cql_bool is_null;
 cql_double value;
} cql_nullable_double;

typedef struct cql_nullable_bool {
 cql_bool is_null;
 cql_bool value;
} cql_nullable_bool;

// These macros are only used when generate_type_getters is enabled.
#define CQL_DATA_TYPE_INT32     1
#define CQL_DATA_TYPE_INT64     2
#define CQL_DATA_TYPE_DOUBLE    3
#define CQL_DATA_TYPE_BOOL      4
#define CQL_DATA_TYPE_STRING    5
#define CQL_DATA_TYPE_BLOB      6
#define CQL_DATA_TYPE_OBJECT    7
#define CQL_DATA_TYPE_CORE      0x7f    // bit mask for core types
#define CQL_DATA_TYPE_NOT_NULL  0x80    // set if and only if null is not possible
#define CQL_CORE_DATA_TYPE_OF(type) ((type) & CQL_DATA_TYPE_CORE)

CQL_EXPORT int cql_outstanding_refs;

CQL_EXPORT void cql_copyoutrow(cql_result_set_ref _Nonnull rs, cql_int32 row, cql_int32 count, ...);
CQL_EXPORT void cql_multifetch(cql_code rc, sqlite3_stmt *_Nullable stmt, cql_int32 count, ...);
CQL_EXPORT void cql_multibind(cql_code *_Nonnull rc, sqlite3 *_Nonnull db, sqlite3_stmt *_Nullable *_Nonnull pstmt, cql_int32 count, ...);
CQL_EXPORT void cql_best_error(cql_code *_Nonnull prc);

typedef struct cql_fetch_info {
  cql_code rc;
  sqlite3 *_Nonnull db;
  sqlite3_stmt *_Nullable stmt;
  uint8_t *_Nonnull data_types;
  uint16_t *_Nonnull col_offsets;
  uint16_t refs_count;
  uint16_t refs_offset;
  uint16_t *_Nullable identity_columns;
  int32_t rowsize;
  const char *_Nullable autodrop_tables;
  int64_t crc;
  int32_t *_Nullable perf_index;
} cql_fetch_info;

CQL_EXPORT void cql_multifetch_meta(char *_Nonnull data, cql_fetch_info *_Nonnull info);

CQL_EXPORT cql_code cql_fetch_all_results(cql_fetch_info *_Nonnull info,
                                          cql_result_set_ref _Nullable *_Nonnull result_set);

CQL_EXPORT cql_code cql_one_row_result(cql_fetch_info *_Nonnull info,
                                       char *_Nullable data,
                                       int32_t count,
                                       cql_result_set_ref _Nullable *_Nonnull result_set);

CQL_EXPORT void cql_set_blob_ref(cql_blob_ref _Nullable *_Nonnull target, cql_blob_ref _Nullable source);
CQL_EXPORT void cql_set_string_ref(cql_string_ref _Nullable *_Nonnull target, cql_string_ref _Nullable source);
CQL_EXPORT void cql_set_object_ref(cql_object_ref _Nullable *_Nonnull target, cql_object_ref _Nullable source);

CQL_EXPORT cql_code cql_prepare(sqlite3 *_Nonnull db, sqlite3_stmt *_Nullable *_Nonnull pstmt, const char *_Nonnull sql);
CQL_EXPORT cql_code cql_exec(sqlite3 *_Nonnull db, const char *_Nonnull sql);
CQL_EXPORT cql_code cql_exec_internal(sqlite3 *_Nonnull db, cql_string_ref _Nonnull str_ref);

CQL_EXPORT cql_code cql_prepare_frags(sqlite3 *_Nonnull db,
                                      sqlite3_stmt *_Nullable *_Nonnull pstmt,
                                      const char *_Nonnull base,
                                      const char *_Nonnull frags);

CQL_EXPORT cql_code cql_exec_frags(sqlite3 *_Nonnull db,
                                   const char *_Nonnull base,
                                   const char *_Nonnull frags);

CQL_EXPORT void cql_finalize_on_error(cql_code rc, sqlite3_stmt *_Nullable *_Nonnull pstmt);
CQL_EXPORT void cql_finalize_stmt(sqlite3_stmt *_Nullable *_Nonnull pstmt);

CQL_EXPORT void cql_column_nullable_bool(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_nullable_bool *_Nonnull data);
CQL_EXPORT void cql_column_nullable_int32(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_nullable_int32 *_Nonnull data);
CQL_EXPORT void cql_column_nullable_int64(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_nullable_int64 *_Nonnull data);
CQL_EXPORT void cql_column_nullable_double(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_nullable_double *_Nonnull data);
CQL_EXPORT void cql_column_nullable_string_ref(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_string_ref _Nullable *_Nonnull data);
CQL_EXPORT void cql_column_nullable_blob_ref(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_blob_ref _Nullable *_Nonnull data);
CQL_EXPORT void cql_column_string_ref(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_string_ref _Nonnull *_Nonnull data);
CQL_EXPORT void cql_column_blob_ref(sqlite3_stmt *_Nonnull stmt, cql_int32 index, cql_blob_ref _Nonnull *_Nonnull data);

#define cql_teardown_row(r) cql_release_offsets(&(r), (r)._refs_count_, (r)._refs_offset_)
#define cql_retain_row(r) cql_retain_offsets(&(r), (r)._refs_count_, (r)._refs_offset_)
#define cql_offsetof(_struct, _field) ((cql_uint16)offsetof(_struct, _field))

#define cql_combine_nullables(output, l_is_null, r_is_null, value_) \
  { cql_bool __saved_is_null = (l_is_null) || (r_is_null); \
   (output).value = __saved_is_null ? 0 : (value_); \
   (output).is_null = __saved_is_null; }

#define cql_set_null(output) \
   (output).is_null = cql_true; (output).value = 0;

#define cql_set_notnull(output, val) \
   (output).value = val; (output).is_null = cql_false;

#define cql_set_nullable(output, isnull_, value_) \
   { cql_bool __saved_is_null = isnull_; \
   (output).value = __saved_is_null  ? 0 : (value_); \
   (output).is_null = __saved_is_null; }

#define cql_is_nullable_true(is_null, value) (!(is_null) && (value))
#define cql_is_nullable_false(is_null, value) (!(is_null) && !(value))

#define BYTEBUF_GROWTH_SIZE 1024
#define BYTEBUF_EXP_GROWTH_CAP 1024 * 1024
#define BYTEBUF_GROWTH_SIZE_AFTER_CAP 200 * 1024

typedef struct cql_bytebuf
{
  char *_Nullable ptr;   // pointer to stored data, if any
  int used;    // bytes used in current buffer
  int max;     // max bytes in current buffer
} cql_bytebuf;

CQL_EXPORT int bytebuf_open_count;

CQL_EXPORT void cql_bytebuf_open(cql_bytebuf *_Nonnull b);
CQL_EXPORT void cql_bytebuf_close(cql_bytebuf *_Nonnull b);
CQL_EXPORT void *_Nonnull cql_bytebuf_alloc(cql_bytebuf *_Nonnull b, int needed);
CQL_EXPORT void cql_bytebuf_append(cql_bytebuf *_Nonnull buffer, const void *_Nonnull data, int32_t bytes);

// sqlite3 compat functions for implementations that do not exist in lib versions that are supported.  If the runtime
// version of sqlite3 supports these functions, it will use those, otherwise it will use the compat version.
CQL_EXPORT int cql_compat_sqlite3_strlike(const char *_Nonnull zGlob, const char *_Nonnull zStr, unsigned int cEsc);

// teardown all the internal data for the given result_set
CQL_EXPORT void cql_result_set_teardown(cql_result_set_ref _Nonnull result_set);

// retain/release references in a row using the given offset array
CQL_EXPORT void cql_retain_offsets(void *_Nonnull pv, cql_uint16 refs_count, cql_uint16 refs_offset);
CQL_EXPORT void cql_release_offsets(void *_Nonnull pv, cql_uint16 refs_count, cql_uint16 refs_offset);

// hash a row in a row set using the metadata
CQL_EXPORT cql_hash_code cql_row_hash(cql_result_set_ref _Nonnull result_set, cql_int32 row);

// compare two rows for equality
CQL_EXPORT cql_bool cql_rows_equal(cql_result_set_ref _Nonnull rs1, cql_int32 row1, cql_result_set_ref _Nonnull rs2, cql_int32 row2);

// compare two rows for same identity column values
CQL_EXPORT cql_bool cql_rows_same(cql_result_set_ref _Nonnull rs1, cql_int32 row1, cql_result_set_ref _Nonnull rs2, cql_int32 row2);

// copy a set of rows from a result_set
CQL_EXPORT void cql_rowset_copy(cql_result_set_ref _Nonnull result_set, cql_result_set_ref _Nonnull *_Nonnull to_result_set, int32_t from, cql_int32 count);

CQL_EXPORT cql_int32 cql_result_set_get_int32_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_int64 cql_result_set_get_int64_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_bool cql_result_set_get_bool_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_double cql_result_set_get_double_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_string_ref _Nullable cql_result_set_get_string_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_blob_ref _Nullable cql_result_set_get_blob_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_bool cql_result_set_get_is_null_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);

// result set metadata management
CQL_EXPORT void cql_initialize_meta(cql_result_set_meta *_Nonnull meta, cql_fetch_info *_Nonnull info);

#ifndef cql_sqlite3_exec
#define cql_sqlite3_exec(db, sql) sqlite3_exec((db), (sql), NULL, NULL, NULL)
#endif // cql_sqlite3_exec
#ifndef cql_sqlite3_prepare_v2
#define cql_sqlite3_prepare_v2(db, sql, len, stmt, tail) sqlite3_prepare_v2((db), (sql), (len), (stmt), (tail))
#endif // cql_sqlite3_prepare_v2
#ifndef cql_sqlite3_finalize
#define cql_sqlite3_finalize(stmt) sqlite3_finalize((stmt))
#endif // cql_sqlite3_finalize

void cql_results_from_data(cql_code rc,
                           cql_bytebuf *_Nonnull buffer,
                           cql_fetch_info *_Nonnull info,
                           cql_result_set_ref _Nullable *_Nonnull result_set);
CQL_EXTERN_C_END

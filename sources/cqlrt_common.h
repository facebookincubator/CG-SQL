/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
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
#include <limits.h>
#include <setjmp.h>

#ifdef __cplusplus
#define CQL_EXTERN_C_BEGIN extern "C" {
#define CQL_EXTERN_C_END }
#else
#define CQL_EXTERN_C_BEGIN
#define CQL_EXTERN_C_END
#endif // __cplusplus

#if LONG_MAX > 0x7fffffff
#define _64(x) x##L
#else
#define _64(x) x##LL
#endif

#ifndef __has_attribute         // Optional of course.
  #define __has_attribute(x) 0  // Compatibility with non-clang compilers.
#endif

#if __has_attribute(optnone)
  #define CQL_OPT_NONE __attribute__((optnone))
#elif __has_attribute(optimize)
  #define CQL_OPT_NONE __attribute__((optimize("O0")))
#else
  #define CQL_OPT_NONE
#endif

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
#define CQL_DATA_TYPE_CORE      0x3f    // bit mask for core types
#define CQL_DATA_TYPE_ENCODED   0x40    // set if and only if encode
#define CQL_DATA_TYPE_NOT_NULL  0x80    // set if and only if null is not possible
#define CQL_CORE_DATA_TYPE_OF(type) ((type) & CQL_DATA_TYPE_CORE)

CQL_EXPORT int cql_outstanding_refs;

CQL_EXPORT void cql_copyoutrow(sqlite3 *_Nullable db, cql_result_set_ref _Nonnull rs, cql_int32 row, cql_int32 count, ...);
CQL_EXPORT void cql_multifetch(cql_code rc, sqlite3_stmt *_Nullable stmt, cql_int32 count, ...);
CQL_EXPORT void cql_multibind(cql_code *_Nonnull rc, sqlite3 *_Nonnull db, sqlite3_stmt *_Nullable *_Nonnull pstmt, cql_int32 count, ...);
CQL_EXPORT void cql_multibind_var(cql_code *_Nonnull rc, sqlite3 *_Nonnull db, sqlite3_stmt *_Nullable *_Nonnull pstmt, cql_int32 count, const char *_Nullable vpreds, ...);
CQL_EXPORT cql_code cql_best_error(cql_code rc);
CQL_EXPORT void cql_set_encoding(uint8_t *_Nonnull data_types, cql_int32 count, cql_int32 col, cql_bool encode);

typedef struct cql_fetch_info {
  cql_code rc;
  sqlite3 *_Nullable db;
  sqlite3_stmt *_Nullable stmt;
  uint8_t *_Nonnull data_types;
  uint16_t *_Nonnull col_offsets;
  uint16_t refs_count;
  uint16_t refs_offset;
  uint16_t *_Nullable identity_columns;
  int16_t encode_context_index;
  int32_t rowsize;
  const char *_Nullable autodrop_tables;
  int64_t crc;
  int32_t *_Nullable perf_index;
  cql_object_ref _Nullable encoder;
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

CQL_EXPORT cql_code cql_prepare_var(sqlite3 *_Nonnull db,
                                    sqlite3_stmt *_Nullable *_Nonnull pstmt,
                                    cql_int32 count,
                                    const char *_Nullable preds, ...);

CQL_EXPORT cql_code cql_no_rows_stmt(sqlite3 *_Nonnull db, sqlite3_stmt *_Nullable *_Nonnull pstmt);
CQL_EXPORT cql_result_set_ref _Nonnull cql_no_rows_result_set(void);
CQL_EXPORT cql_code cql_exec(sqlite3 *_Nonnull db, const char *_Nonnull sql);
CQL_EXPORT cql_code cql_exec_var(sqlite3 *_Nonnull db, cql_int32 count, const char *_Nullable preds, ...);
CQL_EXPORT CQL_WARN_UNUSED cql_code cql_exec_internal(sqlite3 *_Nonnull db, cql_string_ref _Nonnull str_ref);

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

// Enforces (via `cql_tripwire`) that an argument passed in from C to a stored
// procedure is not NULL. `position` indicates for which argument we're doing
// the checking, counting from 1, *not* 0.
void cql_contract_argument_notnull(void *_Nullable argument, cql_uint32 position);

// Like `cql_contract_argument_notnull`, but also checks that `*argument` is not
// NULL. This should only be used for INOUT arguments of a NOT NULL reference
// type; `cql_contract_argument_notnull` should be used in all other cases.
void cql_contract_argument_notnull_when_dereferenced(void *_Nullable argument, cql_uint32 position);

#ifdef CQL_RUN_TEST
// If compiled with `CQL_RUN_TEST`, `cql_contract_argument_notnull` and
// `cql_contract_argument_notnull_when_dereferenced` will longjmp here instead
// of calling `cql_tripwire` when this is not NULL. The jump will be performed
// with a value of `position`, thus allowing tests to know for which argument a
// tripwire would have normally been encountered.
extern jmp_buf *_Nullable cql_contract_argument_notnull_tripwire_jmp_buf;
#endif

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

// getters
CQL_EXPORT cql_int32 cql_result_set_get_int32_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_int64 cql_result_set_get_int64_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_bool cql_result_set_get_bool_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_double cql_result_set_get_double_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_string_ref _Nullable cql_result_set_get_string_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_object_ref _Nullable cql_result_set_get_object_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_blob_ref _Nullable cql_result_set_get_blob_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);

// setters
CQL_EXPORT void cql_result_set_set_int32_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_nullable_int32 new_value);
CQL_EXPORT void cql_result_set_set_int32_col_not_null(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_int32 new_value);
CQL_EXPORT void cql_result_set_set_int64_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_nullable_int64 new_value);
CQL_EXPORT void cql_result_set_set_int64_col_not_null(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_int64 new_value);
CQL_EXPORT void cql_result_set_set_bool_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_nullable_bool new_value);
CQL_EXPORT void cql_result_set_set_bool_col_not_null(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_bool new_value);
CQL_EXPORT void cql_result_set_set_double_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_nullable_double new_value);
CQL_EXPORT void cql_result_set_set_double_col_not_null(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_double new_value);
CQL_EXPORT void cql_result_set_set_string_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_string_ref _Nullable new_value);
CQL_EXPORT void cql_result_set_set_object_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_object_ref _Nullable new_value);
CQL_EXPORT void cql_result_set_set_blob_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col, cql_blob_ref _Nullable new_value);


CQL_EXPORT cql_bool cql_result_set_get_is_null_col(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col);
CQL_EXPORT cql_bool cql_result_set_get_is_encoded_col(cql_result_set_ref _Nonnull result_set, cql_int32 col);

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

CQL_EXPORT void cql_results_from_data(
  cql_code rc,
  cql_bytebuf *_Nonnull buffer,
  cql_fetch_info *_Nonnull info,
  cql_result_set_ref _Nullable *_Nonnull result_set);

// data entry for a closed hash table
typedef struct cql_hashtab_entry {
 cql_string_ref _Nullable key;
 cql_int64 val;
} cql_hashtab_entry;

// hash table with payloads and capacity info
typedef struct cql_hashtab {
  cql_int32 count;
  cql_int32 capacity;
  cql_hashtab_entry *_Nullable payload;
} cql_hashtab;

// elementary hash table functions
CQL_EXPORT cql_hashtab *_Nonnull cql_hashtab_new(void);
CQL_EXPORT void cql_hashtab_delete(cql_hashtab *_Nonnull ht);
CQL_EXPORT cql_bool cql_hashtab_add(cql_hashtab *_Nonnull ht, cql_string_ref _Nonnull key, cql_int64 val);
CQL_EXPORT cql_hashtab_entry *_Nullable cql_hashtab_find(cql_hashtab *_Nonnull ht, cql_string_ref _Nonnull key);

// CQL friendly versions of the above, easy to call from CQL
CQL_EXPORT cql_int64 cql_facets_new(void);
CQL_EXPORT void cql_facets_delete(cql_int64 facets);
CQL_EXPORT cql_bool cql_facet_add(cql_int64 facets, cql_string_ref _Nonnull name, cql_int64 crc);
CQL_EXPORT cql_int64 cql_facet_find(cql_int64 facets, cql_string_ref _Nonnull key);

CQL_EXTERN_C_END

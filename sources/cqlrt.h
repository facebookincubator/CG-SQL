/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <sqlite3.h>

#ifndef __clang__
#ifndef _Nonnull
    /* Hide Clang-only nullability specifiers if not Clang */
    #define _Nonnull
    #define _Nullable
#endif
#endif

#define cql_contract assert
#define cql_invariant assert
#define cql_log_database_error(...)
#define cql_error_trace()

// value types
typedef unsigned char cql_bool;
#define cql_true (cql_bool)1
#define cql_false (cql_bool)0

// metatypes for the straight C implementation
#define CQL_C_TYPE_STRING 0
#define CQL_C_TYPE_BLOB 1
#define CQL_C_TYPE_RESULTS 2
#define CQL_C_TYPE_BOXED_STMT 3

typedef unsigned long cql_hash_code;
typedef int32_t cql_int32;
typedef uint32_t cql_uint32;
typedef uint16_t cql_uint16;
typedef sqlite3_int64 cql_int64;
typedef double cql_double;
typedef int cql_code;

// base ref counting struct
typedef struct cql_type *cql_type_ref;
typedef struct cql_type {
  int type;
  int ref_count;
  void (*_Nullable finalize)(cql_type_ref _Nonnull ref);
} cql_type;
void cql_retain(cql_type_ref _Nullable ref);
void cql_release(cql_type_ref _Nullable ref);

// builtin object
typedef struct cql_object *cql_object_ref;
typedef struct cql_object {
  cql_type base;
  const void *_Nonnull ptr;
} cql_object;
#define cql_object_retain(object) cql_retain((cql_type_ref)object);
#define cql_object_release(object) cql_release((cql_type_ref)object);

// builtin statement box
typedef struct cql_boxed_stmt *cql_boxed_stmt_ref;
typedef struct cql_boxed_stmt {
  cql_type base;
  sqlite3_stmt *_Nullable stmt;
} cql_boxed_stmt;

// builtin blob
typedef struct cql_blob *cql_blob_ref;
typedef struct cql_blob {
  cql_type base;
  const void *_Nonnull ptr;
  cql_uint32 size;
} cql_blob;
#define cql_blob_retain(object) cql_retain((cql_type_ref)object);
#define cql_blob_release(object) cql_release((cql_type_ref)object);
cql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull data, cql_uint32 size);
#define cql_get_blob_bytes(data) (data->ptr)
#define cql_get_blob_size(data) (data->size)

// builtin string
typedef struct cql_string *cql_string_ref;
typedef struct cql_string {
  cql_type base;
  const char *_Nullable ptr;
} cql_string;
cql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr);
#define cql_string_retain(string) cql_retain((cql_type_ref)string);
#define cql_string_release(string) cql_release((cql_type_ref)string);

#define cql_string_literal(name, text) \
  cql_string name##_ = { \
    .base = { \
      .type = CQL_C_TYPE_STRING, \
      .ref_count = 1, \
      .finalize = NULL, \
    }, \
    .ptr = text, \
  }; \
  cql_string_ref name = &name##_

int cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
cql_hash_code cql_string_hash(cql_string_ref _Nullable str);
cql_hash_code cql_blob_hash(cql_blob_ref _Nullable str);
cql_bool cql_string_equal(cql_string_ref _Nullable s1, cql_string_ref _Nullable s2);
int cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
#define cql_alloc_cstr(cstr, str) const char *_Nonnull cstr = (str)->ptr
#define cql_free_cstr(cstr, str) 0

cql_bool cql_blob_equal(cql_blob_ref _Nonnull blob1, cql_blob_ref _Nonnull blob2);
cql_hash_code cql_ref_hash(cql_type_ref _Nonnull typeref);
cql_bool cql_ref_equal(cql_type_ref _Nullable typeref1, cql_type_ref _Nullable typeref2);

// builtin result set
typedef struct cql_result_set *cql_result_set_ref;

typedef struct cql_result_set_meta {
  // release the internal memory for the rowset
  void (*_Nonnull teardown)(cql_result_set_ref _Nonnull result_set);

  // copy a slice of a result set starting at from of length count
  void (*_Nullable copy)(cql_result_set_ref _Nonnull result_set,
                         cql_result_set_ref _Nullable *_Nonnull to_result_set,
                         cql_int32 from,
                         cql_int32 count);

 // hash a row in a row set using the metadata
  cql_hash_code (*_Nullable rowHash)(cql_result_set_ref _Nonnull result_set, cql_int32 row);

 // compare two rows for equality
  cql_bool (*_Nullable rowsEqual)(cql_result_set_ref _Nonnull rs1,
                                  cql_int32 row1,
                                  cql_result_set_ref _Nonnull rs2,
                                  cql_int32 row2);

  // compare two rows for the same identity column value(s)
  cql_bool (*_Nullable rowsSame)(cql_result_set_ref _Nonnull rs1,
                                 cql_int32 row1,
                                 cql_result_set_ref _Nonnull rs2,
                                 cql_int32 row2);

  // count of references and offset to the first
  uint16_t refsCount;
  uint16_t refsOffset;

  // offsets to all the columns
  uint16_t *_Nullable columnOffsets;

  // size of the row
  size_t rowsize;

  // number of columns
  cql_int32 columnCount;

  // count and column indexes of all the columns in the identity
  uint16_t *_Nullable identityColumns;

  // all datatypes of the columns
  uint8_t *_Nullable dataTypes;

} cql_result_set_meta;

typedef struct cql_result_set {
  cql_type base;
  cql_result_set_meta meta;
  cql_int32 count;
  void *_Nonnull data;
} cql_result_set;

#define cql_result_set_type_decl(result_set_type, result_set_ref) typedef struct _##result_set_type *result_set_ref;
cql_result_set_ref _Nonnull cql_result_set_create(void *_Nonnull data,
                                                  cql_int32 count,
                                                  cql_result_set_meta meta);

#define cql_result_set_retain(result_set) cql_retain((cql_type_ref)result_set);
#define cql_result_set_release(result_set) cql_release((cql_type_ref)result_set);
#define cql_result_set_get_meta(result_set) (&((cql_result_set_ref)result_set)->meta)
#define cql_result_set_get_data(result_set) ((cql_result_set_ref)result_set)->data
#define cql_result_set_get_count(result_set) ((cql_result_set_ref)result_set)->count

#ifdef CQL_RUN_TEST
#define sqlite3_step mockable_sqlite3_step
SQLITE_API cql_code mockable_sqlite3_step(sqlite3_stmt *_Nonnull);
#endif

// No-op implementation of profiling
// * Note: we emit the crc as an expression just to be sure that there are no compiler
//   errors caused by names being incorrect.  This improves the quality of the CQL
//   code gen tests significantly.  If these were empty macros (as they once were)
//   you could emit any junk in the call and it would still compile.
#define cql_profile_start(crc, index) (void)crc; (void)index;
#define cql_profile_stop(crc, index)  (void)crc; (void)index;

// the basic version doesn't use column getters
#define CQL_NO_GETTERS 1

cql_object_ref _Nonnull cql_box_stmt(sqlite3_stmt *_Nullable stmt);
sqlite3_stmt *_Nullable cql_unbox_stmt(cql_object_ref _Nonnull ref);

// NOTE: This must be included *after* all of the above symbols/macros.
#include "cqlrt_common.h"

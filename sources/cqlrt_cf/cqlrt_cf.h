/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <math.h>
#include <sqlite3.h>

#include <CoreFoundation/CoreFoundation.h>
#include <CoreFoundation/CFBase.h>
#include <CoreFoundation/CFString.h>

#ifndef __clang__
#ifndef _Nonnull
    /* Hide Clang-only nullability specifiers if not Clang */
    #define _Nonnull
    #define _Nullable
#endif
#endif

#define cql_contract assert
#define cql_invariant assert
#define cql_tripwire assert

// Default database loggign does nothing
#define cql_log_database_error(db, cat, msg)

// Default tracing does nothing
#ifndef CQL_TRACING_ENABLED
#define cql_error_trace()
#else
// whatever tracing you want, for example this might help in test code.
#define cql_error_trace() \
  fprintf(stderr, "Error at %s:%d in %s: %d %s\n", __FILE__, __LINE__, _PROC_, _rc_, sqlite3_errmsg(_db_))
#endif

// value types
typedef Boolean cql_bool;
#define cql_true TRUE
#define cql_false FALSE

// metatypes for the straight C implementation
#define CQL_C_TYPE_STRING 0
#define CQL_C_TYPE_BLOB 1
#define CQL_C_TYPE_RESULTS 2
#define CQL_C_TYPE_BOXED_STMT 3
#define CQL_C_TYPE_OBJECT 4

typedef uint64_t cql_hash_code;
typedef int32_t cql_int32;
typedef uint32_t cql_uint32;
typedef uint16_t cql_uint16;
typedef sqlite3_int64 cql_int64;
typedef double cql_double;
typedef int cql_code;

// CF base helpers
typedef CFTypeRef cql_type_ref;
void cql_retain(CFTypeRef _Nullable ref);
void cql_release(CFTypeRef _Nullable ref);
cql_hash_code cql_ref_hash(CFTypeRef _Nonnull ref);
cql_bool cql_ref_equal(CFTypeRef  _Nonnull r1, CFTypeRef  _Nonnull r2);

// CF Blob
typedef CFDataRef cql_blob_ref ;
void cql_blob_retain(cql_blob_ref _Nullable obj);
void cql_blob_release(cql_blob_ref _Nullable obj);
void *_Nonnull cql_get_blob_bytes(cql_blob_ref _Nonnull blob);
cql_int64 cql_get_blob_size(cql_blob_ref _Nonnull blob);
cql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull bytes, cql_int64 size);
cql_bool cql_blob_equal(cql_blob_ref _Nonnull b1, cql_blob_ref _Nonnull b2);

// CF object
typedef CFTypeRef cql_object_ref ;
void cql_object_retain(cql_object_ref _Nullable obj);
void cql_object_release(cql_object_ref _Nullable obj);

#define CF_C_STRING_STACK_MAX_LENGTH 512

#define CF_STRING_CREATE_C_STRING_STACK(cStringVar) \
  char __##cStringVar##Stack[CF_C_STRING_STACK_MAX_LENGTH]; \
  char *_Nonnull cStringVar = __##cStringVar##Stack;

// The strange calling convention is designed to catch errors.
// the stack storage is passed in via cStringVar
// if it's usable, great, if not, cStringVar is mutated to point to the heap.
// The heap allocation, if any, is returned.  If you fail to use the CF_STRING_FREE_C_STRING
// then the compiler will warn you that __cStringVarHeap is unused.
// The funky argument choice creates a safer macro and better code-gen.
// CF_STRING_CREATE_C_STRING will produce a NULL cStringVar if passed a NULL cfString.
#define CF_STRING_CREATE_C_STRING(cStringVar, cfString) \
  CF_STRING_CREATE_C_STRING_STACK(cStringVar) \
  char *_Nullable __##cStringVar##Heap = cql_copy_string_to_stack_or_heap(cfString, &cStringVar);

// CF_STRING_FREE_C_STRING will do nothing if passed a NULL cStringVar.
#define CF_STRING_FREE_C_STRING(cStringVar, cfString) \
  if (__##cStringVar##Heap) free(__##cStringVar##Heap);

char *_Nullable cql_copy_string_to_stack_or_heap(CFStringRef _Nullable cfString, char *_Nullable *_Nonnull result);

// CF String
typedef CFStringRef cql_string_ref;
void cql_string_retain(cql_string_ref _Nullable str);
void cql_string_release(cql_string_ref _Nullable str);
cql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr);
cql_hash_code cql_string_hash(cql_string_ref _Nonnull str);
cql_int32 cql_string_equal(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
cql_int32 cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
cql_int32 cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);

// useful for declaring strings in C, not useful to other languages
#define cql_alloc_cstr(cstr, str) CF_STRING_CREATE_C_STRING(cstr, str)
#define cql_free_cstr(cstr, str) CF_STRING_FREE_C_STRING(cstr, str)
#define CF_CONST_STRING(name, value) CFStringRef _Nonnull name = CFSTR(value);
#define cql_string_literal CF_CONST_STRING
#define cql_string_proc_name CF_CONST_STRING

// This is the abstract holder for these items with only the C interface to get the contents
typedef CFTypeRef CQLHolderRef;
typedef CQLHolderRef cql_result_set_ref;

typedef struct cql_result_set_meta {
  // release the internal memory for the rowset
  void (*_Nonnull teardown)(cql_result_set_ref _Nonnull result_set);

  // copy a slice of a result set starting at from of length count
  void (*_Nullable copy)(
    cql_result_set_ref _Nonnull result_set,
    cql_result_set_ref _Nullable *_Nonnull to_result_set,
    cql_int32 from,
    cql_int32 count);

 // hash a row in a row set using the metadata
  cql_hash_code (*_Nullable rowHash)(
    cql_result_set_ref _Nonnull result_set,
    cql_int32 row);

 // compare two rows for equality
  cql_bool (*_Nullable rowsEqual)(
    cql_result_set_ref _Nonnull rs1,
    cql_int32 row1,
    cql_result_set_ref _Nonnull rs2,
    cql_int32 row2);

  // compare two rows for the same identity column value(s)
  cql_bool (*_Nullable rowsSame)(
    cql_result_set_ref _Nonnull rs1,
    cql_int32 row1,
    cql_result_set_ref _Nonnull rs2,
    cql_int32 row2);

  // check whether the column value is encoded
  cql_bool(*_Nullable getIsEncoded)(
   cql_result_set_ref _Nonnull result_set,
   cql_int32 col);

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

  // index of the encode context column
  int16_t encodeContextIndex;
} cql_result_set_meta;

typedef struct cql_result_set {
  cql_result_set_meta meta;
  cql_int32 count;
  void *_Nonnull data;
} cql_result_set;

#define cql_result_set_type_decl(result_set_type, result_set_ref) \
  typedef cql_result_set_ref result_set_ref;

cql_result_set_ref _Nonnull cql_result_set_create(
  void *_Nonnull data,
  cql_int32 count,
  cql_result_set_meta meta);

cql_result_set *_Nonnull cql_get_result_set_from_ref(cql_result_set_ref _Nonnull ref);

#define cql_result_set_retain(result_set) cql_retain((cql_type_ref)result_set);
#define cql_result_set_release(result_set) cql_release((cql_type_ref)result_set);
#define cql_result_set_note_ownership_transferred(result_set)

#define cql_result_set_get_meta(result_set_ref) (&cql_get_result_set_from_ref(result_set_ref)->meta)
#define cql_result_set_get_data(result_set_ref) (cql_get_result_set_from_ref(result_set_ref)->data)
#define cql_result_set_get_count(result_set_ref) (cql_get_result_set_from_ref(result_set_ref)->count)

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

// implementation of encoding values. All sensitive values read from sqlite db will
// be encoded at the source. CQL never decode encoded sensitive string unless the
// user call explicitly decode function from code.
cql_object_ref _Nullable cql_copy_encoder(sqlite3 *_Nonnull db);

cql_bool cql_encode_bool(
  cql_object_ref _Nullable encoder,
  cql_bool value,
  cql_int32 context_type,
  void *_Nullable context);

cql_int32 cql_encode_int32(
  cql_object_ref _Nullable encoder,
  cql_int32 value,
  cql_int32 context_type,
  void *_Nullable context);

cql_int64 cql_encode_int64(
  cql_object_ref _Nullable encoder,
  cql_int64 value,
  cql_int32 context_type,
  void *_Nullable context);

cql_double cql_encode_double(
  cql_object_ref _Nullable encoder,
  cql_double value,
  cql_int32 context_type,
  void *_Nullable context);

cql_string_ref _Nonnull cql_encode_string_ref_new(
  cql_object_ref _Nullable encoder,
  cql_string_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context);

cql_blob_ref _Nonnull cql_encode_blob_ref_new(
  cql_object_ref _Nullable encoder,
  cql_blob_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context);

cql_bool cql_decode_bool(
  cql_object_ref _Nullable encoder,
  cql_bool value,
  cql_int32 context_type,
  void *_Nullable context);

cql_int32 cql_decode_int32(
  cql_object_ref _Nullable encoder,
  cql_int32 value,
  cql_int32 context_type,
  void *_Nullable context);

cql_int64 cql_decode_int64(
  cql_object_ref _Nullable encoder,
  cql_int64 value,
  cql_int32 context_type,
  void *_Nullable context);

cql_double cql_decode_double(
  cql_object_ref _Nullable encoder,
  cql_double value,
  cql_int32 context_type,
  void *_Nullable context);

cql_string_ref _Nonnull cql_decode_string_ref_new(
  cql_object_ref _Nullable encoder,
  cql_string_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context);

cql_blob_ref _Nonnull cql_decode_blob_ref_new(
  cql_object_ref _Nullable encoder,
  cql_blob_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context);

// NOTE: This must be included *after* all of the above symbols/macros.
#include "cqlrt_common.h"

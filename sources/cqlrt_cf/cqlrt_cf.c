/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqlrt_cf.h"
#include <memory.h>
#include <stdbool.h>

cql_int32 cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2) {
  cql_invariant(s1 != NULL);
  cql_invariant(s2 != NULL);

  cql_alloc_cstr(c1, s1);
  cql_alloc_cstr(c2, s2);
  cql_int32 code = (cql_int32)cql_compat_sqlite3_strlike(c1, c2, '\0');
  cql_free_cstr(c2, s2);
  cql_free_cstr(c1, s1);

  return code;
}

// naive implementation of encode for cql_bool. It flip the boolean value
cql_bool cql_encode_bool(
  cql_object_ref _Nullable encoder,
  cql_bool value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return !value;
}

// naive implementation of decode for cql_bool. It flip the boolean value
cql_bool cql_decode_bool(
  cql_object_ref _Nullable encoder,
  cql_bool value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return !value;
}

// naive implementation of encode for cql_int32.
cql_int32 cql_encode_int32(
  cql_object_ref _Nullable encoder,
  cql_int32 value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return (value >> 16) | (value << 16);
}

// naive implementation of decode for cql_int32.
cql_int32 cql_decode_int32(
  cql_object_ref _Nullable encoder,
  cql_int32 value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return (value << 16) | (value >> 16);
}

// naive implementation of encode for cql_int64.
cql_int64 cql_encode_int64(
  cql_object_ref _Nullable encoder,
  cql_int64 value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return (value >> 32) | (value << 32);
}

// naive implementation of decode for cql_int64.
cql_int64 cql_decode_int64(
  cql_object_ref _Nullable encoder,
  cql_int64 value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return (value << 32) | (value >> 32);
}

// naive implementation of encode for double.
cql_double cql_encode_double(
  cql_object_ref _Nullable encoder,
  cql_double value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return -value;
}

// naive implementation of decode for double.
cql_double cql_decode_double(
  cql_object_ref _Nullable encoder,
  cql_double value,
  cql_int32 context_type,
  void *_Nullable context)
{
  return -value;
}

// naive implementation of encode for string.  No-op.
cql_string_ref cql_encode_string_ref_new(
  cql_object_ref _Nullable encoder,
  cql_string_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context)
{
  cql_string_retain(value);
  return value;
}

// naive implementation of decode for string. No-op.
// and encode context in the string
cql_string_ref cql_decode_string_ref_new(
  cql_object_ref _Nullable encoder,
  cql_string_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context)
{
  cql_string_retain(value);
  return value;
}

// naive implementation of encode for blob. No-op.
cql_blob_ref cql_encode_blob_ref_new(
  cql_object_ref _Nullable encoder,
  cql_blob_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context)
{
  cql_blob_retain(value);
  return value;
}

// naive implementation of decode for blob. No-op.
// in the blob.
cql_blob_ref cql_decode_blob_ref_new(
  cql_object_ref _Nullable encoder,
  cql_blob_ref _Nonnull value,
  cql_int32 context_type,
  void *_Nullable context)
{
  cql_blob_retain(value);
  return value;
}


cql_object_ref cql_copy_encoder(sqlite3* db) {
  return NULL;  // no encoder object needed
}

char *_Nullable cql_copy_string_to_stack_or_heap(
  CFStringRef _Nullable cf_string_ref,
  char *_Nullable *_Nonnull result)
{
  // "result" starts with the stack string we can use if we want to
  char *_Nullable stack_string = *result;
  char *_Nullable heap_string = NULL;     // no heap storage to start
  char *cstr = NULL;
  if (cf_string_ref != NULL) {
    cstr = (char *)CFStringGetCStringPtr(cf_string_ref, kCFStringEncodingUTF8);
    if (cstr == NULL) {
      CFIndex length = CFStringGetLength(cf_string_ref);
      CFIndex size = CFStringGetMaximumSizeForEncoding(length, kCFStringEncodingUTF8) + 1;
      if (size <= CF_C_STRING_STACK_MAX_LENGTH &&
          CFStringGetCString(cf_string_ref, stack_string, CF_C_STRING_STACK_MAX_LENGTH, kCFStringEncodingUTF8)) {
          cstr = stack_string;
      } else {
        heap_string = malloc(size);
        if (CFStringGetCString(cf_string_ref, heap_string, size, kCFStringEncodingUTF8)) {
          cstr = heap_string;
        }
      }
    }
  }
  *result = cstr;

  // this is what has to be freed, we return NULL if we did not allocate a heap string
  return heap_string;
}

void cql_retain(CFTypeRef _Nullable ref) {
  if (ref) CFRetain(ref);
}

void cql_release(CFTypeRef _Nullable ref) {
  if (ref) CFRelease(ref);
}

cql_hash_code cql_ref_hash(CFTypeRef _Nonnull ref) {
  return CFHash(ref);
}

cql_bool cql_ref_equal(CFTypeRef  _Nonnull r1, CFTypeRef  _Nonnull r2) {
 return CFEqual(r1, r2);
}

void cql_blob_retain(cql_blob_ref _Nullable obj) {
  cql_retain(obj);
}

void cql_blob_release(cql_blob_ref _Nullable obj) {
  cql_release(obj);
}

void *_Nonnull cql_get_blob_bytes(cql_blob_ref _Nonnull blob)  {
  return (void *_Nonnull)CFDataGetBytePtr(blob);
}

cql_int64 cql_get_blob_size(cql_blob_ref _Nonnull blob)  {
  return CFDataGetLength(blob);
}

cql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull bytes, cql_int64 size) {
  return CFDataCreate(NULL, bytes, size);
}

cql_bool cql_blob_equal(cql_blob_ref _Nonnull b1, cql_blob_ref _Nonnull b2) {
  return CFEqual(b1,b2);
}

void cql_object_retain(cql_object_ref _Nullable obj) {
  cql_retain(obj);
}

void cql_object_release(cql_object_ref _Nullable obj) {
  cql_release(obj);
}

void cql_string_retain(cql_string_ref _Nullable str) {
  cql_retain(str);
}

void cql_string_release(cql_string_ref _Nullable str) {
  cql_release(str);
}

cql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr) {
  return CFStringCreateWithCString(NULL, cstr, kCFStringEncodingUTF8);
}

cql_hash_code cql_string_hash(cql_string_ref _Nonnull str) {
  return CFHash(str);
}

cql_int32 cql_string_equal(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2) {
  return CFEqual(s1, s2);
}

cql_int32 cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2) {
  return (cql_int32)CFStringCompare(s1, s2, 0);
}

#include "cqlrt_common.c"

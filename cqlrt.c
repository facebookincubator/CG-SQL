/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqlrt.h"
#include <memory.h>
#include <stdbool.h>

int cql_outstanding_refs = 0;

void cql_retain(cql_type_ref _Nullable ref) {
  if (ref) {
    ref->ref_count++;
    cql_outstanding_refs++;
  }
}

void cql_release(cql_type_ref _Nullable ref) {
  if (ref)  {
    if (--ref->ref_count == 0) {
      if (ref->finalize) {
        ref->finalize(ref);
      }
      free((void *)ref);
    }
    cql_outstanding_refs--;
    cql_invariant(cql_outstanding_refs >= 0);
  }
}

static void cql_blob_finalize(cql_type_ref _Nonnull ref) {
  cql_blob_ref blob = (cql_blob_ref)ref;
  cql_invariant(blob->ptr != NULL);
  free((void *)blob->ptr);
  blob->size = 0;
}

cql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull bytes, cql_uint32 size) {
  cql_invariant(bytes != NULL);
  cql_blob_ref result = malloc(sizeof(cql_blob));
  result->base.type = CQL_C_TYPE_BLOB;
  result->base.ref_count = 1;
  result->base.finalize = &cql_blob_finalize;
  result->ptr = malloc(size);
  result->size = size;
  memcpy((void *)result->ptr, bytes, size);
  cql_outstanding_refs++;
  return result;
}

cql_hash_code cql_blob_hash(cql_blob_ref _Nullable blob) {
  cql_hash_code hash = 0;
  if (blob) {
    // djb2
    hash = 5381;
    const unsigned char *bytes = blob->ptr;
    cql_uint32 size = blob->size;
    while (size--) {
      hash = ((hash << 5) + hash) + *bytes++; /* hash * 33 + c */
    }
  }
  return hash;
}

cql_bool cql_blob_equal(cql_blob_ref _Nonnull blob1, cql_blob_ref _Nonnull blob2) {
  const unsigned char *bytes1 = blob1->ptr;
  cql_uint32 size1 = blob1->size;
  const unsigned char *bytes2 = blob2->ptr;
  cql_uint32 size2 = blob2->size;

  return size1 == size2 && !memcmp(bytes1, bytes2, size1);
}

static void cql_string_finalize(cql_type_ref _Nonnull ref) {
  cql_string_ref string = (cql_string_ref)ref;
  cql_invariant(string->ptr != NULL);
  free((void *)string->ptr);
  string->ptr = NULL;  // in case of use after free, fail fast
}

cql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr) {
  cql_invariant(cstr != NULL);
  cql_string_ref result = malloc(sizeof(cql_string));
  result->base.type = CQL_C_TYPE_STRING;
  result->base.ref_count = 1;
  result->base.finalize = &cql_string_finalize;
  size_t cstrlen = strlen(cstr);
  result->ptr = malloc(cstrlen + 1);
  memcpy((void *)result->ptr, cstr, cstrlen + 1);
  cql_outstanding_refs++;
  return result;
}

int cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2) {
  cql_invariant(s1 != NULL);
  cql_invariant(s2 != NULL);
  return strcmp(s1->ptr, s2->ptr);
}

cql_hash_code cql_string_hash(cql_string_ref _Nullable str) {
  cql_hash_code hash = 0;
  if (str) {
    // djb2
    hash = 5381;
    const char *chars = str->ptr;
    int c;
    while ((c = *chars++))
      hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  }
  return hash;
}

cql_bool cql_string_equal(cql_string_ref _Nullable s1, cql_string_ref _Nullable s2) {
  if (s1 == s2) {
    return cql_true;
  }
  if (!s1 || !s2) {
    return cql_false;
  }
  return cql_string_compare(s1, s2) == 0;
}

int cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2) {
  cql_invariant(s1 != NULL);
  cql_invariant(s2 != NULL);
  return cql_compat_sqlite3_strlike(s2->ptr, s1->ptr, '%');
}

static void cql_result_set_finalize(cql_type_ref _Nonnull ref) {
  cql_result_set_ref result_set = (cql_result_set_ref)ref;
  if (result_set->meta.teardown) {
    result_set->meta.teardown(result_set);
  }
}

cql_result_set_ref _Nonnull cql_result_set_create(void *_Nonnull data, cql_int32 count, cql_result_set_meta meta) {
  cql_result_set_ref result = malloc(sizeof(cql_result_set));
  result->base.type = CQL_C_TYPE_RESULTS;
  result->base.ref_count = 1;
  result->base.finalize = &cql_result_set_finalize;
  result->meta = meta;
  result->count = count;
  result->data = data;
  cql_outstanding_refs++;
  return result;
}

cql_hash_code cql_ref_hash(cql_type_ref typeref) {
  if (typeref == NULL) {
    return 0;
  }

  if (typeref->type == CQL_C_TYPE_STRING) {
    return cql_string_hash((cql_string_ref)typeref);
  }

  // only these two types are ever invoked
  cql_contract(typeref->type == CQL_C_TYPE_BLOB);
  return cql_blob_hash((cql_blob_ref)typeref);
}

cql_bool cql_ref_equal(cql_type_ref typeref1, cql_type_ref typeref2) {
  if (typeref1 == typeref2) {
    return true;
  }

  // both are not null, so if either is null then false
  if (typeref1 == NULL || typeref2 == NULL) {
    return false;
  }

  // not used for arbitrary comparisons, types already checked
  cql_contract(typeref1->type == typeref2->type);

  if (typeref1->type == CQL_C_TYPE_STRING) {
    return cql_string_equal((cql_string_ref)typeref1, (cql_string_ref)typeref2);
  }

  // only these two types are ever invoked
  cql_contract(typeref1->type == CQL_C_TYPE_BLOB);
  return cql_blob_equal((cql_blob_ref)typeref1, (cql_blob_ref)typeref2);
}

#include "cqlrt_common.c"

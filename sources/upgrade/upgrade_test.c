/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <sqlite3.h>
#include <stdio.h>

// All versions have the same signatures, include them all! 
// If we screwed this up the compiler will complain!
#include "generated_upgrade0.h"
#include "generated_upgrade1.h"
#include "generated_upgrade2.h"
#include "generated_upgrade3.h"

#include "upgrade_validate.h"

int32_t pre_validate(sqlite3* db, cql_int64 *version) {
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  int rv = test_cql_get_facet_version(db, facet, version);
  cql_string_release(facet);
  return rv;
}

int32_t upgrade(sqlite3* db) {
  test_result_set_ref result_set;
  int32_t rv = test_fetch_results(db, &result_set);
  cql_result_set_release(result_set);
  return rv;
}

int32_t post_validate(sqlite3* db, cql_int64 old_version) {
  cql_int64 new_version = -1;
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  if (test_cql_get_facet_version(db, facet, &new_version)) {
    printf("Unable to read cql_schema_version facet\n");
    cql_string_release(facet);
    return SQLITE_ERROR;
  }

  if (validate_transition(db)) {
    cql_string_release(facet);
    return SQLITE_ERROR;
  }

  cql_string_release(facet);
  int32_t result = old_version <= new_version ? SQLITE_OK : SQLITE_ERROR;

  if (result != SQLITE_OK) {
    printf("unexpected version old:%d, new:%d\n", (int32_t)old_version, (int32_t)new_version);
  }

  return result;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Expected usage: ./upgrade_test /path/to/db/\n");
    return SQLITE_ERROR;
  }

  sqlite3* db;
  if (sqlite3_open_v2(argv[1], &db,
      SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL)) {
    printf("Unable to open DB.\n");
    return SQLITE_ERROR;
  }

  cql_int64 version = -1;
  if (pre_validate(db, &version)) {
    printf("Unable to validate DB contents pre-upgrade.\n");
    return SQLITE_ERROR;
  }

  if (upgrade(db)) {
    printf("Unable to upgrade DB.\n");
    return SQLITE_ERROR;
  }

  if (post_validate(db, version)) {
    printf("Unable to validate DB contents post-upgrade.\n");
    return SQLITE_ERROR;
  }

  if (sqlite3_close_v2(db)) {
    printf("Unable to close DB.\n");
    return SQLITE_ERROR;
  }

  return SQLITE_OK;
}

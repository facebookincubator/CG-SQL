// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

#include <sqlite3.h>
#include <stdio.h>

#include "generated_upgrade_test.h"

int pre_validate(sqlite3* db, cql_int64 *version) {
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  int rv = test_cql_get_facet_version(db, facet, version);
  cql_string_release(facet);
  return rv;
}

int upgrade(sqlite3* db) {
  test_result_set_ref result_set;
  int rv = test_fetch_results(db, &result_set);
  cql_result_set_release(result_set);
  return rv;
}

int post_validate(sqlite3* db, cql_int64 old_version) {
  cql_int64 new_version = -1;
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  if (test_cql_get_facet_version(db, facet, &new_version)) {
    cql_string_release(facet);
    return SQLITE_ERROR;
  }
  cql_string_release(facet);
  return old_version < new_version ? SQLITE_OK : SQLITE_ERROR;
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

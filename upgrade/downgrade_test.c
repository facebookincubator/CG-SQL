// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

#include <sqlite3.h>
#include <stdio.h>

#include "generated_downgrade_test.h"

int downgrade(sqlite3* db, test_result_set_ref* result_set) {
  return test_fetch_results(db, result_set);
}

int validate(sqlite3* db, test_result_set_ref result_set) {
  cql_int32 count = test_result_count(result_set);
  if (count != 1) {
    printf("Expected 1 facet for database downgrade, got %d\n", count);
    return SQLITE_ERROR;
  }
  cql_string_ref expected_facet = cql_string_ref_new("downgrade detected");
  cql_string_ref actual_facet = test_get_facet(result_set, 0);
  if (!cql_string_equal(expected_facet, actual_facet)) {
    cql_alloc_cstr(actual_facet_c, actual_facet);
    printf("Expected 'downgrade detected' facet, got '%s'\n", actual_facet_c);
    cql_free_cstr(actual_facet_c, actual_facet);
    cql_string_release(expected_facet);
    return SQLITE_ERROR;
  }
  const char* sql = "DROP VIEW test_view";
  if (sqlite3_exec(db, sql, NULL, NULL, NULL) != SQLITE_OK) {
    printf("Expected 'test_view' to remain in the DB\n");
    return SQLITE_ERROR;
  }
  return SQLITE_OK;
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    printf("Expected usage: ./downgrade_test /path/to/db/\n");
    return SQLITE_ERROR;
  }

  sqlite3* db;
  if (sqlite3_open_v2(
          argv[1], &db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL)) {
    printf("Unable to open DB.\n");
    return SQLITE_ERROR;
  }

  test_result_set_ref result_set;
  if (downgrade(db, &result_set)) {
    printf("Unable to downgrade DB.\n");
    return SQLITE_ERROR;
  }

  if (validate(db, result_set)) {
    printf("Unable to validate that the downgrade was detected.\n");
    cql_result_set_release(result_set);
    return SQLITE_ERROR;
  }

  if (sqlite3_close_v2(db)) {
    printf("Unable to close DB.\n");
    cql_result_set_release(result_set);
    return SQLITE_ERROR;
  }

  cql_result_set_release(result_set);
  return SQLITE_OK;
}

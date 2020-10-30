/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include "cqlrt.h"

extern cql_code test_sample_proc13_create_tables(sqlite3 *_Nonnull _db_);
extern cql_code test_sample_proc13_create_triggers(sqlite3 *_Nonnull _db_);
extern cql_code test_sample_proc13_populate_tables(sqlite3 *_Nonnull _db_);
extern cql_code test_sample_proc13_drop_triggers(sqlite3 *_Nonnull _db_);
extern cql_code test_sample_proc13_drop_tables(sqlite3 *_Nonnull _db_);
extern cql_code test_sample_proc13_drop_indexes(sqlite3 *_Nonnull _db_);

extern cql_code test_test_too_many_row_in_child_table_create_tables(sqlite3 *_Nonnull _db_);
extern cql_code test_test_too_many_row_in_child_table_populate_tables(sqlite3 *_Nonnull _db_);
extern cql_code test_test_too_many_row_in_child_table_drop_tables(sqlite3 *_Nonnull _db_);

int main(int argc, char **argv) {
  // Setup database
  sqlite3 *db = NULL;
  cql_code rc;

  rc = sqlite3_open(":memory:", &db);

  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_create_tables(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_test_too_many_row_in_child_table_create_tables(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_create_triggers(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_populate_tables(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_test_too_many_row_in_child_table_populate_tables(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_drop_triggers(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_drop_indexes(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_sample_proc13_drop_tables(db);
  }
  if (rc == SQLITE_OK) {
    rc = test_test_too_many_row_in_child_table_drop_tables(db);
  }
  sqlite3_close(db);
  return rc;
}

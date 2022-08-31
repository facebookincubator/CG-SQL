/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// These are some UDFs that are used to access result sets in test code.

// Clarifying the original comments regarding the pedigree of this code:
//
// This code is a combination of code that is original to CG/SQL and public domain
// code copied from SQLite examples from the SQLite docs.
//
// There are no license issues.

#include "cqlrt.h"
#include "run_test.h"

// Read the indicated row and column from the result set, if either are out of range
// then we will produce null.  Otherwise we use the type of the column to create
// a suitable result variable.  Note that we only support integral values which is
// good enough for the test code.
static void rs_col(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  cql_result_set_ref rs = (cql_result_set_ref)sqlite3_value_int64(argv[0]);
  int32_t row = sqlite3_value_int(argv[1]);
  int32_t col = sqlite3_value_int(argv[2]);

  int64_t result = 0;
  cql_bool null_out = row < 0 || row >= rs->count || col < 0 || col >= rs->meta.columnCount;

  if (!null_out) {
    char dataType = CQL_CORE_DATA_TYPE_OF(rs->meta.dataTypes[col]);
    null_out = cql_result_set_get_is_null_col(rs, row, col);

    if (!null_out) {
      switch (dataType) {
        case CQL_DATA_TYPE_INT64:
          result = cql_result_set_get_int64_col(rs, row, col);
          break;
        case CQL_DATA_TYPE_INT32:
          result = cql_result_set_get_int32_col(rs, row, col);
          break;
        case CQL_DATA_TYPE_BOOL:
          result = cql_result_set_get_bool_col(rs, row, col);
          break;
        default:
          null_out = true;
          break;
      }
    }
  }

  if (null_out) {
    sqlite3_result_null(context);
  }
  else {
    sqlite3_result_int64(context, result);
  }
}

// Returns the count of rows in the indicated result set
static void rs_count(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  cql_result_set_ref rs = (cql_result_set_ref)sqlite3_value_int64(argv[0]);
  sqlite3_result_int64(context, rs->count);
}

// Register the indicated UDFs.  Note, this will be called directly from run_test.sql as a proc
cql_code cql_init_extensions(sqlite3 *db) {
  cql_code rc = sqlite3_create_function_v2(db, "rscount", 1, SQLITE_UTF8, 0, rs_count, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "rscol", 3, SQLITE_UTF8, 0, rs_col, NULL, NULL, NULL);
  return rc;
}

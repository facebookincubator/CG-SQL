/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// As applicable given the shared pedigree of this code.

#include "cqlrt.h"
#include "run_test.h"
#include "alloca.h"

#include <assert.h>
#include <string.h>

void rsColFunc(
  sqlite3_context *context,
  int32_t argc,
  sqlite3_value **argv)
{
  cql_result_set_ref rs = (cql_result_set_ref)sqlite3_value_int64(argv[0]);
  int32_t row = sqlite3_value_int(argv[1]);
  int32_t col = sqlite3_value_int(argv[2]);

  int64_t result = 0;
  int32_t null_out = row < 0 || row >= rs->count || col < 0 || col >= rs->meta.columnCount;

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
          null_out = 1;
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

void rsCountFunc(
  sqlite3_context *context,
  int32_t argc,
  sqlite3_value **argv
){
  cql_result_set_ref rs = (cql_result_set_ref)sqlite3_value_int64(argv[0]);
  sqlite3_result_int64(context, rs->count);
}

cql_code cql_init_extensions(sqlite3 *db)
{
  cql_code rc = sqlite3_create_function_v2(db, "rscount", 1, SQLITE_UTF8, 0, rsCountFunc, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "rscol", 3, SQLITE_UTF8, 0, rsColFunc, NULL, NULL, NULL);
  return rc;
}

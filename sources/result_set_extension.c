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

// this is nothing like a generic blob format but it's enough to do some tests
// the blobs that are stored can only be integers of course but since the
// codegen is the same for all types it still makes for a useful test
typedef struct test_blob {
  int64_t type;
  int64_t int1;
  int64_t int2;
  int64_t int3;
} test_blob;

// Returns the indicated integer
static void bgetval_type(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  test_blob *b = (test_blob *)sqlite3_value_blob(argv[0]);
  if (sizeof(*b) != sqlite3_value_bytes(argv[0])) {
    sqlite3_result_null(context);
    return;
  }
  sqlite3_result_int64(context, b->type);
}

// Returns the indicated integer
static void bgetval(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  test_blob *b = (test_blob *)sqlite3_value_blob(argv[0]);
  if (sizeof(*b) != sqlite3_value_bytes(argv[0])) {
    sqlite3_result_null(context);
    return;
  }

  int64_t index = sqlite3_value_int64(argv[1]);

  switch (index) {
  case 0:
    sqlite3_result_int64(context, b->int1);
    break;
  case 1:
    sqlite3_result_int64(context, b->int2);
    break;
  case 2:
    sqlite3_result_int64(context, b->int3);
    break;
  default:
    sqlite3_result_null(context);
    break;
  }
}

// Update the value store
static void bupdateval(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  if (argc < 1) {
    sqlite3_result_null(context);
    return;
  }

  test_blob *b = (test_blob *)sqlite3_value_blob(argv[0]);
  if (sizeof(*b) != sqlite3_value_bytes(argv[0])) {
    sqlite3_result_null(context);
    return;
  }

  for (int32_t i = 1; i + 3 <= argc; i += 3) {
    int64_t index = sqlite3_value_int64(argv[i]);
    int64_t val = sqlite3_value_int64(argv[i+1]);
    // type would be argv[i+2] but for this test it's all integers

    switch (index) {
    case 0:
      b->int1 = val;
      break;
    case 1:
      b->int2 = val;
      break;
    case 2:
      b->int3 = val;
      break;
    }
  }
  sqlite3_result_blob(context, b, sizeof(*b), SQLITE_TRANSIENT);
}

// Update the value store
static void bupdatekey(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  if (argc < 1) {
    sqlite3_result_null(context);
    return;
  }

  test_blob *b = (test_blob *)sqlite3_value_blob(argv[0]);
  if (sizeof(*b) != sqlite3_value_bytes(argv[0])) {
    sqlite3_result_null(context);
    return;
  }

  for (int32_t i = 1; i + 2 <= argc; i += 2) {
    int64_t index = sqlite3_value_int64(argv[i]);
    int64_t val = sqlite3_value_int64(argv[i+1]);

    switch (index) {
    case 0:
      b->int1 = val;
      break;
    case 1:
      b->int2 = val;
      break;
    case 2:
      b->int3 = val;
      break;
    }
  }
  sqlite3_result_blob(context, b, sizeof(*b), SQLITE_TRANSIENT);
}

// Returns a blob with the given integers in place
static void bcreatekey(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  if (argc < 1) {
    sqlite3_result_null(context);
  }

  test_blob b = {
    .type = sqlite3_value_int64(argv[0])
  };

  if (argc > 2) {
    b.int1 = sqlite3_value_int64(argv[1]);
  }
  if (argc > 4) {
    b.int2 = sqlite3_value_int64(argv[3]);
  }
  if (argc > 6) {
    b.int3 = sqlite3_value_int64(argv[5]);
  }
  sqlite3_result_blob(context, &b, sizeof(b), SQLITE_TRANSIENT);
}

// Create a value store, note that some offsets be missing
static void bcreateval(sqlite3_context *context, int32_t argc, sqlite3_value **argv) {
  if (argc < 1) {
    sqlite3_result_null(context);
    return;
  }

  test_blob b = {
    .type = sqlite3_value_int64(argv[0])
  };

  for (int32_t i = 1; i + 3 <= argc; i += 3) {
    int64_t index = sqlite3_value_int64(argv[i]);
    int64_t val = sqlite3_value_int64(argv[i+1]);
    // type would be argv[i+2] but for this test it's all integers

    switch (index) {
    case 0:
      b.int1 = val;
      break;
    case 1:
      b.int2 = val;
      break;
    case 2:
      b.int3 = val;
      break;
    }
  }
  sqlite3_result_blob(context, &b, sizeof(b), SQLITE_TRANSIENT);
}

// Register the indicated UDFs.  Note, this will be called directly from run_test.sql as a proc
cql_code _cql_init_extensions(sqlite3 *db) {
  cql_code rc = sqlite3_create_function_v2(db, "rscount", 1, SQLITE_UTF8, 0, rs_count, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "rscol", 3, SQLITE_UTF8, 0, rs_col, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bgetval_type", 1, SQLITE_UTF8, 0, bgetval_type, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bgetkey_type", 1, SQLITE_UTF8, 0, bgetval_type, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bgetval", 2, SQLITE_UTF8, 0, bgetval, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bgetkey", 2, SQLITE_UTF8, 0, bgetval, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bcreateval", -1, SQLITE_UTF8, 0, bcreateval, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bcreatekey", -1, SQLITE_UTF8, 0, bcreatekey, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bupdateval", -1, SQLITE_UTF8, 0, bupdateval, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  rc = sqlite3_create_function_v2(db, "bupdatekey", -1, SQLITE_UTF8, 0, bupdatekey, NULL, NULL, NULL);
  if (rc != SQLITE_OK) {
    return rc;
  }

  return rc;
}

/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include "cqlrt.h"

extern cql_code query_plan(sqlite3 *_Nonnull _db_);
extern void create_udf(sqlite3 *_Nonnull _db_);

int main(int argc, char **argv) {
  // Setup database
  sqlite3 *db = NULL;
  cql_code rc;

  rc = sqlite3_open(":memory:", &db);

  create_udf(db);

  if (rc == SQLITE_OK) {
    rc = query_plan(db);
  }

  if (rc) {
    fprintf(stderr, "sqlite error: %s\n", sqlite3_errmsg(db));
  }

  sqlite3_close(db);
  return rc;
}

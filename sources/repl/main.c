/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqlrt.h"
#include "go.h"

// super cheesy error handling
#define _E(c, x) if (!(c)) { \
  printf("!" #x "%s:%d\n", __FILE__, __LINE__); \
  goto error; \
}

#define E(x) _E(x, x)
#define SQL_E(x) _E(SQLITE_OK == (x), x)

// patternlint-disable-next-line prefer-sized-ints-in-msys
int main(int argc, char **argv) {
  printf("CQL Mini App Thingy\n");

  sqlite3 *db = NULL;
  SQL_E(sqlite3_open(":memory:", &db));

  SQL_E(go(db));
  return 0;

error:
  return 1;
}

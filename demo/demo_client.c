/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqlrt.h"
#include "demo.h"

// super cheesy error handling
#define _E(c, x) if (!(c)) { \
  printf("!" #x "%s:%d\n", __FILE__, __LINE__); \
  goto error; \
}
#define E(x) _E(x, x)
#define SQL_E(x) _E(SQLITE_OK == (x), x)

static void print_result_set(const char *label, get_mixed_result_set_ref result_set) {
  cql_int32 id;
  cql_bool b_is_null;
  cql_bool b_value;
  cql_bool code_is_null;
  cql_int64 code_value;
  cql_string_ref name;

  for (cql_int32 i = 0; i < get_mixed_result_count(result_set); i++) {
    id = get_mixed_get_id(result_set, i);
    b_is_null = get_mixed_get_flag_is_null(result_set, i);
    b_value = get_mixed_get_flag_value(result_set, i);
    code_is_null = get_mixed_get_code_is_null(result_set, i);
    code_value = get_mixed_get_code_value(result_set, i);
    name = get_mixed_get_name(result_set, i);

    printf("%s: row %d) %d %d %d %d %lld %s\n", label, i, id, b_is_null, b_value, code_is_null, code_value, name->ptr);
  }
}

int main(int argc, char **argv) {
  printf("CQL data access demo:  creating and reading from a table\n");

  sqlite3 *db = NULL;
  SQL_E(sqlite3_open(":memory:", &db));

  get_mixed_result_set_ref result_set;
  get_mixed_result_set_ref result_set_copy;
  get_mixed_result_set_ref result_set_updated;
  cql_int32 count = 5;
  cql_int32 copy_index = 1;
  cql_int32 copy_count = 3;

  SQL_E(make_mixed(db));
  SQL_E(load_mixed(db));
  SQL_E(get_mixed_fetch_results(db, &result_set, count));
  get_mixed_copy(result_set, &result_set_copy, copy_index, copy_count);
  SQL_E(update_mixed(db, get_mixed_get_id(result_set, 0), 1234.5));
  SQL_E(get_mixed_fetch_results(db, &result_set_updated, count));

  cql_int32 result_set_count = get_mixed_result_count(result_set);
  E(result_set_count == count);

  cql_int32 result_set_copy_count = get_mixed_result_count(result_set_copy);
  E(result_set_copy_count == copy_count);

  print_result_set("result_set", result_set);
  print_result_set("result_set_copy", result_set_copy);
  print_result_set("result_set_updated", result_set_updated);

  for (cql_int32 i = 0; i < copy_count; ++i) {
    E(get_mixed_row_equal(result_set, copy_index + i, result_set_copy, i));
  }

  E(get_mixed_row_same(result_set, 0, result_set_updated, 0));
  E(get_mixed_row_same(result_set, 1, result_set_updated, 1));
  E(!get_mixed_row_same(result_set, 0, result_set_updated, 1));

  cql_result_set_release(result_set);
  cql_result_set_release(result_set_copy);

  (void)mandelbrot(db);
  return 0;

error:
  return 1;
}

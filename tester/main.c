/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <inttypes.h>

#include "cqlrt.h"
#include "dbhelp.h"

// super cheesy error handling
#define E(x) \
if (SQLITE_OK != (x)) { \
 fprintf(stderr, "error encountered at: %s (%s:%d)\n", #x, __FILE__, __LINE__); \
 fprintf(stderr, "args: %s, %s\n", sql_name, result_name); \
 fprintf(stderr, "sqlite3_errmsg: %s\n", sqlite3_errmsg(db)); \
 goto error; \
}

const char *prefix = "The statement ending at line ";

int32_t tests = 0;
int32_t errors = 0;
int32_t attempts = 0;
sqlite3 *db = NULL;

const char *sql_name;
const char *result_name;

static void print_error_message(char *buffer, int32_t line, int32_t expected) {
  printf("error: at line %d, expected '%s' %spresent", line, buffer, expected ? "" : "not ");
  if (expected) {
    printf(" %s%d times\n", expected == -1 ? "at least " : "", expected == -1 ? 1 : expected);
  }
  printf("\n");
}

void do_match(char *buffer, int32_t line) {
  int32_t search_line;
  int32_t count;
  int32_t expected;

  if (buffer[0] == '-' && buffer[1] == ' ') {
    buffer++;
    expected = 0;
  }
  else if (buffer[0] == '+' && buffer[1] == ' ') {
    buffer++;
    expected = -1;
  }
  else if (buffer[0] == '+' && buffer[1] >= '0' && buffer[1] <= '9' && buffer[2] == ' ') {
    expected = buffer[1] - '0';
    buffer += 2;
  }
  else {
    return;
  }

  attempts++;

  // replace the trailing linefeed with %
  int32_t len = strlen(buffer);
  buffer[len-1] = '%';
  buffer[0] = '%';
  cql_string_ref ref = cql_string_ref_new(buffer);
  E(dbhelp_find(db, line, ref, &search_line, &count));
  cql_string_release(ref);
  if (expected == count || (expected == -1 && count > 0)) {
    return;
  }

  errors++;
  print_error_message(buffer, line, expected);
  printf("found:\n");
  E(dbhelp_dump_line(db, search_line));
  int32_t prev;
  E(dbhelp_prev_line(db, search_line, &prev));
  printf("\nThe corresponding test case is:\n");
  E(dbhelp_dump_source(db, prev, search_line));
  print_error_message(buffer, line, expected);
  printf("test file: %s\n", sql_name);
  printf("result file: %s\n", result_name);
  printf("\n");
  return;

error:
  printf("unexpected sqlite error\n");
  exit(1);
}

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("usage cql-verify foo.sql foo.out\n");
    printf("cql-verify is a test tool.  It processes the input foo.sql\n");
    printf("looking for patterns to match in the CQL output foo.out\n");
    exit(0);
  }

  sql_name = argv[1];
  result_name = argv[2];

  FILE *sql = fopen(sql_name, "r");
  if (!sql) {
    fprintf(stderr, "unable to open file '%s'\n", sql_name);
  }

  FILE *result = fopen(result_name, "r");
  if (!result) {
    fprintf(stderr, "unable to open file '%s'\n", result_name);
  }

  E(sqlite3_open(":memory:", &db));

  E(dbhelp_setup(db));

  char buffer[40960];  // good enough for test purposes
  int32_t line = 0;

  int32_t len = strlen(prefix);

  while (fgets(buffer, sizeof(buffer), result)) {

    const char *p = strstr(buffer, prefix);

    if (p) {
      line = atoi(p + len);
      continue;
    }

    cql_string_ref ref = cql_string_ref_new(buffer);

    if (line == 0) continue;
    E(dbhelp_add(db, line, ref));

    cql_string_release(ref);
  }

  fclose(result);

  line = 1;

  while (fgets(buffer, sizeof(buffer), sql)) {
    cql_string_ref ref = cql_string_ref_new(buffer);
    E(dbhelp_add_source(db, line, ref));
    cql_string_release(ref);
    line++;
  }
  fclose(sql);

  dbhelp_source_result_set_ref result_set;

  E(dbhelp_source_fetch_results(db, &result_set));

  cql_int32 count = dbhelp_source_result_count(result_set);
  for (cql_int32 i = 0; i < count; i++) {
    cql_string_ref ref;
    line = dbhelp_source_get_line(result_set, i);
    ref = dbhelp_source_get_data(result_set, i);

    char *text = (char*)ref->ptr;

    if (strstr(text, "-- TEST:")) {
      tests++;
    }

    if (!strncmp(text, "-- +", 4) || !strncmp(text, "-- -", 4)) {
      do_match(text + 3, line);
    }
  }
  cql_result_set_release(result_set);

  printf("Verification results: %d tests matched %d patterns of which %d were errors.\n", tests, attempts, errors);

  exit(errors);

error:
  exit(1);
}

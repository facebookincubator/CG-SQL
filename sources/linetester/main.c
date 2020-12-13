/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <inttypes.h>

#include "cqlrt.h"
#include "linetest.h"

// super cheesy error handling
#define E(x) \
if (SQLITE_OK != (x)) { \
 fprintf(stderr, "error encountered at: %s (%s:%d)\n", #x, __FILE__, __LINE__); \
 fprintf(stderr, "args: %s, %s\n", expected_name, actual_name); \
 fprintf(stderr, "sqlite3_errmsg: %s\n", sqlite3_errmsg(db)); \
 exit(1); \
}

const char *prefix1 = "#define _PROC_ \"";
const char *prefix2 = "#undef _PROC_";

int32_t tests = 0;
int32_t errors = 0;
int32_t attempts = 0;
sqlite3 *db = NULL;

const char *expected_name;
const char *actual_name;

void read_file(FILE *input, const char *source) {
  cql_string_ref name = cql_string_ref_new(source);
  cql_string_ref proc = NULL;
  char buffer[40960];  // good enough for test purposes
  bool base_at_next_line = false;
  int32_t line = 0;
  int32_t line_base = 0;
  int32_t physical_line = 0;

  while (fgets(buffer, sizeof(buffer), input)) {
    physical_line++;
    char *p = strchr(buffer, '\n');
    *p = 0;

    const char *p1 = strstr(buffer, prefix1);
    if (p1 == buffer) {
      cql_string_release(proc);
      proc = cql_string_ref_new(buffer + strlen(prefix1));
      base_at_next_line = true;
      line = 0;
      size_t plen = strlen(proc->ptr);
      ((char*)proc->ptr)[plen-1] = 0;
    }
    
    const char *p2 = strstr(buffer, prefix2);
    if (p2 == buffer) {
      cql_string_release(proc);
      proc = NULL;
      line = 0;
      line_base = 0;
    }

    if (buffer[0] == '#' && buffer[1] == ' ') {
      line = atoi(buffer + 2);
      if (base_at_next_line) {
        line_base = line - 1;
        base_at_next_line = false;
      }
      line -= line_base;
      continue;
    }

    cql_string_ref ref = cql_string_ref_new(buffer);

    if (!proc) continue;
    E(linetest_add(db, name, proc, line, ref, physical_line));

    cql_string_release(ref);
  }
}

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("usage cql-linetest expected actual\n");
    printf("cql-linetest is a test tool.  It processes the input files\n");
    printf("normalizing the lines to the start of each procedure\n");
    printf("and verifies that the line numbers are as expected\n");
    exit(0);
  }

  expected_name = argv[1];
  actual_name = argv[2];

  FILE *expected = fopen(expected_name, "r");
  if (!expected) {
    fprintf(stderr, "unable to open file '%s'\n", expected_name);
  }

  FILE *actual = fopen(actual_name, "r");
  if (!actual) {
    fprintf(stderr, "unable to open file '%s'\n", actual_name);
  }

  E(sqlite3_open(":memory:", &db));

  E(linetest_setup(db));

  read_file(expected, "exp");
  read_file(actual, "act");

  fclose(actual);
  fclose(expected);

  int32_t procs, compares, errors;

  E(compare_lines(db, &procs, &compares, &errors));

  printf("\n");
  if (errors) {
    printf("EXPECTED INPUT FILE: %s\n", expected_name);
    printf("  ACTUAL INPUT FILE: %s\n", actual_name);
  }

  printf("Verification results: %d procedures matched %d patterns of which %d were errors.\n", procs, compares, errors);

  exit(errors);
}

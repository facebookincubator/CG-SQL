/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include<stdio.h>

#pragma clang diagnostic ignored "-Wnullability-completeness"

#define CQL_IS_NOT_MAIN
#include "out/cql_amalgam.c"

// Runs cql parsing multiple times in the same run session.
// This is to verify that cql emits expected output for each
// parsing even with no exits between.  This requires lots of
// bison and CQL state to be reset properly.
int32_t main(int32_t argc, char **argv) {
  if (argc != 4) {
    fprintf(
        stderr,
        "Usage: amalgam_test file1 file2 file3\n\n"
        "file1: a cql file with no errors\n"
        "file2: a cql file with a semantic error\n"
        "file3: a cql file with a syntax error\n");
    return 1;
  }

  const char *cql_success_file = (char *) argv[1];
  const char *cql_semantic_error_file = (char *) argv[2];
  const char *cql_syntax_error_file = (char *) argv[3];

  fprintf(stdout, "-- RUN %s:\n", cql_success_file);
  fprintf(stderr, "-- RUN %s:\n", cql_success_file);
  const char *args_1[5] = {"cql", "--in", cql_success_file, "--sem", "--echo"};
  if (cql_main(5, (char **)args_1) != 0) {
    fprintf(stderr, "%s reported an error: this is unexpected\n", cql_success_file);
    exit(1);
  }

  fprintf(stdout, "\n-- RUN %s:\n", cql_semantic_error_file);
  fprintf(stderr, "\n-- RUN %s:\n", cql_semantic_error_file);
  const char *args_2[5] = {"cql", "--in", cql_semantic_error_file, "--sem", "--echo"};
  if (cql_main(5, (char **)args_2) == 0) {
    fprintf(stderr, "%s did not reported an error: this is unexpected\n", cql_semantic_error_file);
    exit(1);
  }

  fprintf(stdout, "\n-- RUN %s:\n", cql_syntax_error_file);
  fprintf(stderr, "\n-- RUN %s:\n", cql_syntax_error_file);
  const char *args_3[5] = {"cql", "--in", cql_syntax_error_file, "--sem", "--echo"};
  if (cql_main(5, (char **)args_3) == 0) {
    fprintf(stderr, "%s did not reported an error: this is unexpected\n", cql_syntax_error_file);
    exit(1);
  }

  fprintf(stdout, "\n-- RUN %s:\n", cql_success_file);
  fprintf(stderr, "\n-- RUN %s:\n", cql_success_file);
  if (cql_main(5, (char **)args_1) != 0) {
    fprintf(stderr, "%s reported an error: this is unexpected\n", cql_success_file);
    exit(1);
  }

  return 0;
}

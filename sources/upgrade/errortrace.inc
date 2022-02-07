/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * Include this file to enable error tracing in interesting places.
 * It isn't used anywhere by default but you can jam it into a failing upgrader
 * to help you debug.
 */

@echo c, "#undef cql_error_trace\n";
@echo c, "#define cql_error_trace() fprintf(stderr, \"SQL Failure %d %s: %s %d\\n\", _rc_, sqlite3_errmsg(_db_), __FILE__, __LINE__)\n";

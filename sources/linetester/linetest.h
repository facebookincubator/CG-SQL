/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma once

#include "cqlrt.h"


// Generated from linetest.sql:37
extern CQL_WARN_UNUSED cql_code linetest_setup(sqlite3 *_Nonnull _db_);

// Generated from linetest.sql:44
extern CQL_WARN_UNUSED cql_code linetest_add(sqlite3 *_Nonnull _db_, cql_string_ref _Nonnull source_, cql_string_ref _Nonnull procname_, cql_int32 line_, cql_string_ref _Nonnull data_, cql_int32 physical_line_);

// Generated from linetest.sql:53
extern CQL_WARN_UNUSED cql_code linetest_dump(sqlite3 *_Nonnull _db_);

// Generated from linetest.sql:62
extern CQL_WARN_UNUSED cql_code dump_proc_records(sqlite3 *_Nonnull _db_, cql_string_ref _Nonnull source_, cql_string_ref _Nonnull procname_);

// Generated from linetest.sql:71
extern CQL_WARN_UNUSED cql_code dump(sqlite3 *_Nonnull _db_, cql_string_ref _Nonnull procname);

// Generated from linetest.sql:135
extern CQL_WARN_UNUSED cql_code compare_lines(sqlite3 *_Nonnull _db_, cql_int32 *_Nonnull procs, cql_int32 *_Nonnull compares, cql_int32 *_Nonnull errors);

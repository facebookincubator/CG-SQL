// @generated SignedSource<<deadbeef8badf00ddefec8edfacefeed>>

#pragma once

#include "cqlrt.h"

extern CQL_WARN_UNUSED cql_code dbhelp_setup(sqlite3 *_Nonnull _db_);
extern CQL_WARN_UNUSED cql_code dbhelp_prev_line(sqlite3 *_Nonnull _db_, cql_int32 line_, cql_int32 *_Nonnull prev);
extern CQL_WARN_UNUSED cql_code dbhelp_add(sqlite3 *_Nonnull _db_, cql_int32 line, cql_string_ref _Nonnull data);
extern CQL_WARN_UNUSED cql_code dbhelp_add_source(sqlite3 *_Nonnull _db_, cql_int32 line, cql_string_ref _Nonnull data);
extern CQL_WARN_UNUSED cql_code dbhelp_dump_line(sqlite3 *_Nonnull _db_, cql_int32 line_);
extern CQL_WARN_UNUSED cql_code dbhelp_find(sqlite3 *_Nonnull _db_, cql_int32 line_, cql_string_ref _Nonnull pattern, cql_int32 *_Nonnull search_line, cql_int32 *_Nonnull found);
extern CQL_WARN_UNUSED cql_code dbhelp_dump_source(sqlite3 *_Nonnull _db_, cql_int32 line1, cql_int32 line2);
#define CRC_dbhelp_source -2582919431962037637L

extern cql_string_ref _Nonnull dbhelp_source_stored_procedure_name;

#define dbhelp_source_data_types_count 2

cql_result_set_type_decl(dbhelp_source_result_set, dbhelp_source_result_set_ref);
extern cql_int32 dbhelp_source_get_line(dbhelp_source_result_set_ref _Nonnull result_set, cql_int32 row);
extern cql_string_ref _Nonnull dbhelp_source_get_data(dbhelp_source_result_set_ref _Nonnull result_set, cql_int32 row);
extern cql_int32 dbhelp_source_result_count(dbhelp_source_result_set_ref _Nonnull result_set);
extern CQL_WARN_UNUSED cql_code dbhelp_source_fetch_results(sqlite3 *_Nonnull _db_, dbhelp_source_result_set_ref _Nullable *_Nonnull result_set);
#define dbhelp_source_row_hash(result_set, row) cql_result_set_get_meta((cql_result_set_ref)(result_set))->rowHash((cql_result_set_ref)(result_set), row)
#define dbhelp_source_row_equal(rs1, row1, rs2, row2) \
cql_result_set_get_meta((cql_result_set_ref)(rs1))->rowsEqual( \
  (cql_result_set_ref)(rs1), \
  row1, \
  (cql_result_set_ref)(rs2), \
  row2)

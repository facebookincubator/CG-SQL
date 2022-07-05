/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <sqlite3.h>
#include <stdio.h>
#include <stdbool.h>

// All versions have the same signatures, include them all!
// If we screwed this up the compiler will complain!
#include "generated_upgrade0.h"
#include "generated_upgrade1.h"
#include "generated_upgrade2.h"
#include "generated_upgrade3.h"

#include "upgrade_validate.h"

cql_code pre_validate(sqlite3* db, cql_int64 *version) {
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  int rv = test_cql_get_facet_version(db, facet, version);
  cql_string_release(facet);
  return rv;
}

static cql_code _vtab_create(sqlite3 *db, void *pAux, int argc, const char *const *argv, sqlite3_vtab **ppVTab, char **outErrorString)
{
  return SQLITE_OK;
}

static cql_code _vtab_connect(sqlite3 *db, void *aux, int argc, const char *const *argv, sqlite3_vtab **outVtable, char **outErrorString)
{
  return SQLITE_OK;
}

static cql_code _vtab_disconnect(sqlite3_vtab *vtab)
{
  return SQLITE_OK;
}

static cql_code _vtab_destroy(sqlite3_vtab *pVTab) {
  return SQLITE_OK;
}

cql_code create_test_module(sqlite3 *db)
{
  // This is an eponymous-only virtual table, so no xCreate/xDestroy callbacks are defined.  The table
  // does not need to be created in the schema.
  static sqlite3_module module = {
    .iVersion = 0,
    .xCreate = _vtab_create,
    .xDestroy = _vtab_destroy,
    .xConnect = _vtab_connect,
    .xDisconnect = _vtab_disconnect,
  };

  return sqlite3_create_module(db, "test_module", &module, NULL);
}


cql_code upgrade(sqlite3* db, bool should_use_virtual) {
  cql_code rv;

  if (begin_transaction(db)) {
    fprintf(stderr, "failed to begin transaction\n");
  }

  if (should_use_virtual) {
    rv = create_test_module(db);
    if (rv != SQLITE_OK) {
      fprintf(stderr, "failed sqlite3_create_module for test_module\n");
      return rv;
    }

    test_result_set_ref result_set;
    rv = test_fetch_results(db, &result_set);
    if (rv != SQLITE_OK) {
      fprintf(stderr, "failed fetching upgrade results (including virtuals)\n");
      return rv;
    }
    cql_result_set_release(result_set);
  } else {
    test_no_virtual_tables_result_set_ref result_set;
    rv = test_no_virtual_tables_fetch_results(db, &result_set);
    if (rv != SQLITE_OK) {
      fprintf(stderr, "failed fetching upgrade results (no virtuals)\n");
      return rv;
    }
    cql_result_set_release(result_set);
  }

  if (commit_transaction(db)) {
    fprintf(stderr, "failed to commit transaction\n");
  }

  return rv;
}

cql_code post_validate(sqlite3* db, cql_int64 old_version) {
  cql_int64 new_version = -1;
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  if (test_cql_get_facet_version(db, facet, &new_version)) {
    fprintf(stderr, "Unable to read cql_schema_version facet\n");
    cql_string_release(facet);
    return SQLITE_ERROR;
  }

  if (validate_transition(db)) {
    cql_string_release(facet);
    return SQLITE_ERROR;
  }

  cql_string_release(facet);
  int32_t result = old_version <= new_version ? SQLITE_OK : SQLITE_ERROR;

  if (result != SQLITE_OK) {
    fprintf(stderr, "unexpected version old:%d, new:%d\n", (int32_t)old_version, (int32_t)new_version);
  }

  return result;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Expected usage: ./upgrade_test /path/to/db/\n");
    return SQLITE_ERROR;
  }

  sqlite3* db;
  if (sqlite3_open_v2(argv[1], &db,
      SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL)) {
    fprintf(stderr, "Unable to open DB '%s'\n", argv[1]);
    return SQLITE_ERROR;
  }

  // Excluding virtual tables on v3 upgrade tests that no-vt and full upgrade
  // function correctly between arbitrary schema versions
  bool should_use_virtual_tables = strcmp(argv[0], "out/upgrade3");

  cql_int64 version = -1;
  if (pre_validate(db, &version)) {
    fprintf(stderr, "Unable to validate DB contents pre-upgrade.\n");
    return SQLITE_ERROR;
  }

  cql_int64 current_version = -1;
  cql_int64 proposed_version = -1;
  if (test_get_current_and_proposed_versions(db, &current_version, &proposed_version)) {
    fprintf(stderr, "Error getting current and proposed versions\n");
    return SQLITE_ERROR;
  }

  if (current_version != version) {
    fprintf(stderr, "Error getting DB versions: current_version %lld does not match expected version: %lld", current_version, version);
    return SQLITE_ERROR;
  }

  if (upgrade(db, should_use_virtual_tables)) {
    fprintf(stderr, "Unable to upgrade DB.\n");
    return SQLITE_ERROR;
  }

  cql_int64 current_version_after = -1;
  cql_int64 proposed_version_after = -1;
  if (test_get_current_and_proposed_versions(db, &current_version_after, &proposed_version_after)) {
    fprintf(stderr, "Error getting current and proposed versions after upgrade\n");
    return SQLITE_ERROR;
  }

  if( proposed_version != current_version_after || proposed_version_after != current_version_after) {
    fprintf(stderr, "Error - proposed_version %lld (before upgrade) and current_version_after %lld"
           " and proposed_version_after %lld do not match.",
          proposed_version, current_version_after, proposed_version_after);
    return SQLITE_ERROR;
  }

  if (post_validate(db, version)) {
    fprintf(stderr, "Unable to validate DB contents post-upgrade.\n");
    return SQLITE_ERROR;
  }

  if (sqlite3_close_v2(db)) {
    fprintf(stderr, "Unable to close DB.\n");
    return SQLITE_ERROR;
  }

  return SQLITE_OK;
}

cql_code mockable_sqlite3_step(sqlite3_stmt *stmt) {
  return sqlite3_step(stmt);
}

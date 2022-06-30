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

int32_t pre_validate(sqlite3* db, cql_int64 *version) {
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  int rv = test_cql_get_facet_version(db, facet, version);
  cql_string_release(facet);
  return rv;
}

static int32_t _vtab_connect(sqlite3 *_Nonnull db,
                             void *_Nullable aux,
                             int32_t argc,
                             const char *_Nonnull const *_Nonnull argv,
                             sqlite3_vtab *_Nullable *_Nonnull outVtable,
                             char *_Nullable *_Nonnull outErrorString)
{
  return SQLITE_OK;
}

static int32_t _vtab_create(sqlite3 *_Nonnull db,
                            void *_Nullable pAux,
                            int32_t argc,
                            const char *_Nonnull const *_Nonnull argv,
                            sqlite3_vtab *_Nullable *_Nonnull ppVTab,
                            char *_Nullable *_Nonnull outErrorString)
{
  return SQLITE_OK;
}

int32_t upgrade(sqlite3* db, bool should_use_virtual) {
  int32_t rv;
  sqlite3_module module;
  if (should_use_virtual) {
    module.iVersion = 1;
    module.xConnect = _vtab_connect;
    module.xCreate = _vtab_create;
    rv = sqlite3_create_module(
      db,
     "test_module",
      &module,
      NULL
    );
    if (rv != SQLITE_OK) {
      return rv;
    }
    test_result_set_ref result_set;
    rv = test_fetch_results(db, &result_set);
    cql_result_set_release(result_set);
  } else {
    test_no_virtual_tables_result_set_ref result_set;
    rv = test_no_virtual_tables_fetch_results(db, &result_set);
    cql_result_set_release(result_set);
  }
  return rv;
}

int32_t post_validate(sqlite3* db, cql_int64 old_version) {
  cql_int64 new_version = -1;
  cql_string_ref facet = cql_string_ref_new("cql_schema_version");
  if (test_cql_get_facet_version(db, facet, &new_version)) {
    printf("Unable to read cql_schema_version facet\n");
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
    printf("unexpected version old:%d, new:%d\n", (int32_t)old_version, (int32_t)new_version);
  }

  return result;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Expected usage: ./upgrade_test /path/to/db/\n");
    return SQLITE_ERROR;
  }

  sqlite3* db;
  if (sqlite3_open_v2(argv[1], &db,
      SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL)) {
    printf("Unable to open DB.\n");
    return SQLITE_ERROR;
  }

  // Excluding virtual tables on v3 upgrade tests that no-vt and full upgrade
  // function correctly between arbitrary schema versions
  bool should_use_virtual_tables = strcmp(argv[0], "out/upgrade3");

  cql_int64 version = -1;
  if (pre_validate(db, &version)) {
    printf("Unable to validate DB contents pre-upgrade.\n");
    return SQLITE_ERROR;
  }

  cql_int64 current_version = -1;
  cql_int64 proposed_version = -1;
  if (test_get_current_and_proposed_versions(db, &current_version, &proposed_version)) {
    printf("Error getting current and proposed versions\n");
    return SQLITE_ERROR;
  }

  if (current_version != version) {
    printf("Error getting DB versions: current_version %lld does not match expected version: %lld", current_version, version);
    return SQLITE_ERROR;
  }

  if (upgrade(db, should_use_virtual_tables)) {
    printf("Unable to upgrade DB.\n");
    return SQLITE_ERROR;
  }

  cql_int64 current_version_after = -1;
  cql_int64 proposed_version_after = -1;
  if (test_get_current_and_proposed_versions(db, &current_version_after, &proposed_version_after)) {
    printf("Error getting current and proposed versions after upgrade\n");
    return SQLITE_ERROR;
  }

  if( proposed_version != current_version_after || proposed_version_after != current_version_after) {
    printf("Error - proposed_version %lld (before upgrade) and current_version_after %lld"
           " and proposed_version_after %lld do not match.",
          proposed_version, current_version_after, proposed_version_after);
    return SQLITE_ERROR;
  }

  if (post_validate(db, version)) {
    printf("Unable to validate DB contents post-upgrade.\n");
    return SQLITE_ERROR;
  }

  if (sqlite3_close_v2(db)) {
    printf("Unable to close DB.\n");
    return SQLITE_ERROR;
  }

  return SQLITE_OK;
}

cql_code mockable_sqlite3_step(sqlite3_stmt *stmt) {
  return sqlite3_step(stmt);
}

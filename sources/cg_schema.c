/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SCHEMA)

// stubs to avoid link errors
cql_noexport void cg_schema_main(ast_node *head) {}
cql_noexport void cg_schema_upgrade_main(ast_node *head) {}

#else

// Creates schema migration assets

#include "ast.h"
#include "cg_common.h"
#include "charbuf.h"
#include "cql.h"
#include "gen_sql.h"
#include "list.h"
#include "sem.h"
#include "symtab.h"
#include "cg_schema.h"

static void cg_generate_schema_by_mode(charbuf *output, int32_t mode);
static void cg_generate_baseline_tables(charbuf *output);
static void cg_schema_emit_baseline_tables_proc(charbuf *output, charbuf *baseline);
static void cg_schema_manage_views(charbuf *output, int32_t *drops, int32_t *creates);
static void cg_schema_manage_triggers(charbuf *output, int32_t *drops, int32_t *creates);
static void cg_schema_manage_indices(charbuf *output, int32_t *drops, int32_t *creates);
static void cg_schema_manage_recreate_tables(charbuf *output, recreate_annotation *recreates, size_t count);

// We declare all schema we might depend on in this upgrade (this is the include list)
// e.g. we need all our dependent tables so that we can legally use them in an FK
#define SCHEMA_TO_DECLARE 1

// We only emit schema that we are actually updating (this is include - exclude)
// e.g. a table on the exclude list is assumed to be upgraded by its own script
// in a different run.
#define SCHEMA_TO_UPGRADE 2

// We get TEMP items IF and ONLY IF this bit is set
#define SCHEMA_TEMP_ITEMS 4

// If the mode is SCHEMA_TO_DECLARE then we include all the regions we are upgrading
// and all their dependencies.
//
// If the mode is SCHEMA_TO_UPGRADE then we include the above but we reject
// anything on the exclude list.  That list corresponds to things that are upgraded
// elsewhere.
static bool_t include_from_region(CSTR region, int32_t mode) {

  // if the object is in no region then we only include it if included regions is unconstrained.
  // a no-region object can't be excluded, so this test is all we need for no region objects.
  if (region == NULL) {
    return included_regions == NULL;
  }

  // if included regions were specified and this region isn't in the list... it's out
  if (included_regions && !symtab_find(included_regions, region)) {
    return false;
  }

  // if we are making the "stuff we plan to upgrade list" then consider the excluded regions
  // if this region is on the list, it's out.
  if (mode & SCHEMA_TO_UPGRADE) {
    if (excluded_regions && symtab_find(excluded_regions, region)) {
      return false;
    }
  }

  return true;
}

// Sort the annotations in place: the order is:
//  * schema version
//  * annotation types (all creates before deletes)
//  * table name
//  * column ordinal
//  * there can be no ties, the above is a unique annotation key
// patternlint-disable-next-line prefer-sized-ints-in-msys
static int annotation_comparator(const void *v1, const void *v2) {
  const schema_annotation *a1 = (const schema_annotation *)v1;
  const schema_annotation *a2 = (const schema_annotation *)v2;

  if (a1->version < a2->version) return -1;
  if (a1->version > a2->version) return 1;
  if (a1->annotation_type < a2->annotation_type) return -1;
  if (a1->annotation_type > a2->annotation_type) return 1;

  // patternlint-disable-next-line prefer-sized-ints-in-msys
  int ret = Strcasecmp(a1->target_name, a2->target_name);
  if (ret) return ret;

  // It can't be a tie!  table+ordinal+create/delete is unique
  Invariant(a1->column_ordinal != a2->column_ordinal);

  return (a1->column_ordinal < a2->column_ordinal) ? -1 : 1;
}

// Sort the @recreate annotations in place: the order is:
//  * group name
//  * ordinal reversed
//
//  We use this order so that when we drop tables we will likely drop
//  tables that are weak first and strong last.  That is the later tables
//  may have FK to the earlier tables but not the reverse.   We don't want
//  to cause FK action for no reason since the whole group is being dropped anyway.
// patternlint-disable-next-line prefer-sized-ints-in-msys
static int recreate_comparator(const void *v1, const void *v2) {
  const recreate_annotation *a1 = (const recreate_annotation *)v1;
  const recreate_annotation *a2 = (const recreate_annotation *)v2;

  // patternlint-disable-next-line prefer-sized-ints-in-msys
  int ret = Strcasecmp(a1->group_name, a2->group_name);
  if (ret) return ret;

  // It can't be a tie! ordinal is unique!
  Invariant(a1->ordinal != a2->ordinal);

  // reverse ordinal order
  return (a1->ordinal < a2->ordinal) ? 1 : -1;
}

// Emit the template for ending the upgrade to a particular schema version.
static void cg_schema_end_version(charbuf *output, charbuf *upgrade, charbuf *pending, int32_t vers) {
  if (pending->used > 1) {
    bprintf(upgrade, "      -- data migration procedures\n");
    bprintf(upgrade, "%s", pending->ptr);
    bprintf(upgrade, "\n");
  }

  if (upgrade->used > 1) {
    llint_t upgrade_crc = (llint_t)crc_charbuf(upgrade);
    bprintf(output, "    ---- upgrade to schema version %d ----\n\n", vers);
    bprintf(output, "    CALL %s_cql_get_version_crc(%d, schema_version);\n", global_proc_name, vers);
    bprintf(output, "    IF schema_version != %lld THEN\n", (llint_t)upgrade_crc);
    bprintf(output, "%s", upgrade->ptr);
    bprintf(output, "      CALL %s_cql_set_version_crc(%d, %lld);\n", global_proc_name, vers, upgrade_crc);
    bprintf(output, "    END IF;\n\n");
  }

  bclear(pending);
  bclear(upgrade);
}

// This is the callback method handed to the gen_ method that creates SQL for us
// it will call us every time it a col definition to give us a chance to suppress it
static bool_t cg_suppress_new_col_def(ast_node *ast, void *context, charbuf *buffer) {
  Contract(is_ast_col_def(ast));
  Contract(ast->sem);

  // any column created in not the original schema is not emitted when creating the table during migration
  // later migration steps will add these columns
  return ast->sem->create_version != -1;
}

// This is the callback method handed to the gen_ method to force a
// IF NOT EXISTS qualifier on create table statements.
static bool_t cg_schema_force_if_not_exists(ast_node *ast, void *context, charbuf *output) {
  bprintf(output, "IF NOT EXISTS ");
  return true;
}

// Emit the helper procedures for the upgrade
static void cg_schema_helpers(charbuf *decls) {
  bprintf(decls, "-- facets table declaration --\n");
  bprintf(decls, "CREATE TABLE IF NOT EXISTS %s_cql_schema_facets(\n", global_proc_name);
  bprintf(decls, "  facet TEXT NOT NULL PRIMARY KEY,\n");
  bprintf(decls, "  version LONG INTEGER NOT NULL\n");
  bprintf(decls, ");\n\n");

  bprintf(decls, "-- saved facets table declaration --\n");
  bprintf(decls, "CREATE TEMP TABLE %s_cql_schema_facets_saved(\n", global_proc_name);
  bprintf(decls, "  facet TEXT NOT NULL PRIMARY KEY,\n");
  bprintf(decls, "  version LONG INTEGER NOT NULL\n");
  bprintf(decls, ");\n\n");

  bprintf(decls, "-- helper proc for testing for the presence of a column/type\n");
  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_check_column_exists(table_name TEXT NOT NULL, decl TEXT NOT NULL, OUT present BOOL NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  SET present := (SELECT EXISTS(SELECT * FROM sqlite_master WHERE tbl_name = table_name AND sql GLOB decl));\n");
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc for creating the schema version table\n");
  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_create_cql_schema_facets_if_needed()\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  CREATE TABLE IF NOT EXISTS %s_cql_schema_facets(\n", global_proc_name);
  bprintf(decls, "    facet TEXT NOT NULL PRIMARY KEY,\n");
  bprintf(decls, "    version LONG INTEGER NOT NULL\n");
  bprintf(decls, "  );\n");
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc for saving the schema version table\n");
  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_save_cql_schema_facets()\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  DROP TABLE IF EXISTS %s_cql_schema_facets_saved;\n", global_proc_name);
  bprintf(decls, "  CREATE TEMP TABLE %s_cql_schema_facets_saved(\n", global_proc_name);
  bprintf(decls, "    facet TEXT NOT NULL PRIMARY KEY,\n");
  bprintf(decls, "    version LONG INTEGER NOT NULL\n");
  bprintf(decls, "  );\n");
  bprintf(decls, "  INSERT INTO %s_cql_schema_facets_saved\n",  global_proc_name);
  bprintf(decls, "    SELECT * FROM %s_cql_schema_facets;\n", global_proc_name);
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc for setting the schema version of a facet\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_set_facet_version(_facet TEXT NOT NULL, _version LONG INTEGER NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  INSERT OR REPLACE INTO %s_cql_schema_facets (facet, version) VALUES(_facet, _version);\n", global_proc_name);
  bprintf(decls, "END;\n\n");

  // Note this procedure has to handle the case where the table doesn't exist yet for retro-version validation
  // (this happens in test code so it's validated)
  // We stil use the IF NOTHING -1 pattern so that it doesn't produce suprious errors when there is no row, that's not an error.

  bprintf(decls, "-- helper proc for getting the schema version of a facet\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_get_facet_version(_facet TEXT NOT NULL, out _version LONG INTEGER NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  BEGIN TRY\n");
  bprintf(decls, "    SET _version := (SELECT version FROM %s_cql_schema_facets WHERE facet = _facet LIMIT 1 IF NOTHING -1);\n", global_proc_name);
  bprintf(decls, "  END TRY;\n");
  bprintf(decls, "  BEGIN CATCH\n");
  bprintf(decls, "    SET _version := -1;\n"); // this is here to handle the case where the table doesn't exist
  bprintf(decls, "  END CATCH;\n");
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc for getting the schema version CRC for a version index\n");
  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_get_version_crc(_v INTEGER NOT NULL, out _crc LONG INTEGER NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  SET _crc := cql_facet_find(%s_facets, printf('cql_schema_v%%d', _v));\n", global_proc_name);
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc for setting the schema version CRC for a version index\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_set_version_crc(_v INTEGER NOT NULL, _crc LONG INTEGER NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  INSERT OR REPLACE INTO %s_cql_schema_facets (facet, version) VALUES('cql_schema_v'||_v, _crc);\n", global_proc_name);
  bprintf(decls, "END;\n\n");

  bprintf(decls, "-- helper proc to reset any triggers that are on the old plan --\n");
  bprintf(decls, "DECLARE PROCEDURE cql_exec_internal(sql TEXT NOT NULL) USING TRANSACTION;\n\n");

  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_drop_legacy_triggers()\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  DECLARE C CURSOR FOR SELECT name from sqlite_master\n");
  bprintf(decls, "     WHERE type = 'trigger' AND name GLOB 'tr__*';\n");
  bprintf(decls, "  LOOP FETCH C\n");
  bprintf(decls, "  BEGIN\n");
  bprintf(decls, "    call cql_exec_internal(printf('DROP TRIGGER %%s;', C.name));\n");
  bprintf(decls, "  END;\n");
  bprintf(decls, "END;\n\n");
}

static void cg_schema_emit_one_time_drop(charbuf *decls) {
  bprintf(decls, "@attribute(cql:private)\n");
  bprintf(decls, "CREATE PROCEDURE %s_cql_one_time_drop(name TEXT NOT NULL, version INTEGER NOT NULL)\n", global_proc_name);
  bprintf(decls, "BEGIN\n");
  bprintf(decls, "  LET facet := printf('1_time_drop_%%s', name);\n");
  bprintf(decls, "  IF cql_facet_find(%s_facets, facet) != version THEN\n", global_proc_name);
  bprintf(decls, "    call cql_exec_internal(printf('DROP TABLE IF EXISTS %%s;', name));\n");
  bprintf(decls, "    call %s_cql_set_facet_version(facet, version);\n", global_proc_name);
  bprintf(decls, "  END IF;\n");
  bprintf(decls, "END;\n\n");
}

// Emit the delcaration of the sqlite_master table so we can read from it.
static void cg_schema_emit_sqlite_master(charbuf *decls) {
  bprintf(decls, "-- declare sqlite_master -- \n");
  bprintf(decls, "CREATE TABLE sqlite_master (\n");
  bprintf(decls, "  type TEXT NOT NULL,\n");          // The type of database object such as table, index, trigger or view.
  bprintf(decls, "  name TEXT NOT NULL,\n");          // The name of the database object.
  bprintf(decls, "  tbl_name TEXT NOT NULL,\n");      // The table name that the database object is associated with.
  bprintf(decls, "  rootpage INTEGER NOT NULL,\n");   // Root page.
  bprintf(decls, "  sql TEXT NOT NULL\n);\n\n");      // the DDL to CREATE this object
}

static void cg_schema_emit_facet_functions(charbuf *decls) {
  bprintf(decls, "-- declare facet helpers-- \n");
  bprintf(decls, "DECLARE facet_data TYPE LONG<facet_data> not null;\n");
  bprintf(decls, "DECLARE %s_facets facet_data;\n", global_proc_name);
  bprintf(decls, "DECLARE FUNCTION cql_facets_new() facet_data;\n");
  bprintf(decls, "DECLARE PROCEDURE cql_facets_delete(facets facet_data);\n");
  bprintf(decls, "DECLARE FUNCTION cql_facet_add(facets facet_data, facet TEXT NOT NULL, crc LONG NOT NULL) BOOL NOT NULL;\n");
  bprintf(decls, "DECLARE FUNCTION cql_facet_find(facets facet_data, facet TEXT NOT NULL) LONG NOT NULL;\n\n");
}

// Emit all tables versioned as they before modifications, just the original items
// Note these items correspond to create version -1 (no annotation)
// See cg_generate_baseline_tables for more details on which tables are included
// (e.g. not temp, not @recreate)
static void cg_schema_emit_baseline_tables_proc(charbuf *output, charbuf *baseline) {
  cg_generate_baseline_tables(baseline);

  if (baseline->used > 1) {
    bprintf(output, "CREATE PROCEDURE %s_cql_install_baseline_schema()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bindent(output, baseline, 2);
    bprintf(output, "END;\n");
  }
}

// Emit all temp schema
static bool_t cg_schema_emit_temp_schema_proc(charbuf *output) {
  CHARBUF_OPEN(temp_schema);

  cg_generate_schema_by_mode(&temp_schema, SCHEMA_TO_UPGRADE | SCHEMA_TEMP_ITEMS);
  bool_t has_temp_schema = temp_schema.used > 1;

  if (has_temp_schema) {
    bprintf(output, "CREATE PROCEDURE %s_cql_install_temp_schema()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bindent(output, &temp_schema, 2);
    bprintf(output, "END;\n");
  }

  CHARBUF_CLOSE(temp_schema);

  return has_temp_schema;
}

// This handles any schema that has no verison number associated with it.  That is
// all the things that were in the original schema before @create/@delete annotations
// started being used.  Once we're done with  the original items for the baseline,
// the normal processing of deltas for will take care of the rest.
//
// We emit create table statements for all non-temp tables that are
// are in the original version (version -1).  Note that any @created
// table is not on this list... Likewise @recreate tables are not here
// they are on their own plan.
//
// Note: we don't have to deal with indices, triggers, or views they are always
// on the @recreate plan.  So this is tables exclusively.
static void cg_generate_baseline_tables(charbuf *output) {
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.col_def_callback = cg_suppress_new_col_def;
  callbacks.if_not_exists_callback = cg_schema_force_if_not_exists;
  callbacks.mode = gen_mode_no_annotations;

  for (list_item *item = all_tables_list; item; item = item->next) {
    ast_node *ast = item->ast;
    ast_node *ast_output = ast;

    Invariant(is_ast_create_table_stmt(ast));

    if (is_virtual_ast(ast)) {
      // virtual tables are always on the recreate plan
      continue;
    }

    if (!include_from_region(ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    EXTRACT(create_table_name_flags, ast->left);
    EXTRACT(table_flags_attrs, create_table_name_flags->left);
    EXTRACT_OPTION(flags, table_flags_attrs->left);

    bool_t temp = !!(flags & TABLE_IS_TEMP);

    if (ast->sem->create_version == -1 && !ast->sem->recreate && !temp) {
      gen_set_output_buffer(output);
      gen_statement_with_callbacks(ast_output, &callbacks);
      bprintf(output, ";\n\n");
    }
  }
}

// We use this entry point to create the schema definitions we will
// CRC to see if there is an update.  We also use this to create the
// declarations we will need to have all schema objects available to the upgrade.
// Note that in an upgrade script there are loose declarations AND
// DDL inside of procs.  In schema upgrade mode we do not error on that.
// The normal situation is that there must be exactly one DDL fragment
// for one object.
static void cg_generate_schema_by_mode(charbuf *output, int32_t mode) {

  // non-null-callbacks will generate SQL for Sqlite (no attributes)
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_no_annotations;

  // If the mode is SCHEMA_TO_DECLARE then we include all the regions we are upgrading
  // and all their dependencies.  We do not exclude things that are upgraded elsewhere.
  // We do this because we need a logically consistent set of declarations.

  // If the mode is SCHEMA_TO_UPGRADE then we include the above but we reject
  // anything on the exclude list.  That list corresponds to things that are upgraded
  // elsewhere.  Here we do not need schema that has consistent declarations but rather
  // canonical schema that we can checksum for changes. SCHEMA_TO_UPGRADE won't compile
  // because it's missing dependencies.

  // If the mode includes SCHEMA_TEMP_ITEMS then we emit only temp items
  // otherwise we emit only NON temp items.
  bool_t temp_required = !!(mode & SCHEMA_TEMP_ITEMS);
  bool_t schema_declare = !!(mode & SCHEMA_TO_DECLARE);
  bool_t schema_upgrade = !!(mode & SCHEMA_TO_UPGRADE);

  // full annotations for declarations, no annotations for temp items upgrade
  gen_sql_callbacks *use_callbacks = (temp_required && schema_upgrade) ? &callbacks : NULL;

  // emit all the delare select function statements (they may appear in the SQL)
  if (!temp_required && schema_declare) {
    // select functions are never temp, they go in the main phase
    // we never upgrade them, so they don't go in the upgrade section only the declare section
    // they appear in the previous section and the normal section so on previous validation
    // runs the same declaration will be duplicated.  That's ok, we're tolerant to that now.
    for (list_item *item = all_select_functions_list; item; item = item->next) {
      ast_node *ast = item->ast;
      Contract(is_ast_declare_select_func_stmt(ast));
      gen_set_output_buffer(output);
      gen_statement_with_callbacks(ast, use_callbacks);
      bprintf(output, ";\n\n");
    }

    for (list_item *item = all_regions_list; item; item = item->next) {
      ast_node *ast = item->ast;
      Contract(is_ast_declare_schema_region_stmt(ast) || is_ast_declare_deployable_region_stmt(ast));
      gen_set_output_buffer(output);
      gen_statement_with_callbacks(ast, use_callbacks);
      bprintf(output, ";\n\n");
    }
  }

  // emit all tables
  for (list_item *item = all_tables_list; item; item = item->next) {
    ast_node *ast = item->ast;
    ast_node *ast_output = ast;

    if (is_virtual_ast(ast)) {
      ast_output = ast->parent;
      Invariant(is_ast_create_virtual_table_stmt(ast_output));
    }

    Invariant(is_ast_create_table_stmt(ast));

    CSTR region = ast->sem->region;

    if (!include_from_region(region, mode)) {
      continue;
    }

    EXTRACT(create_table_name_flags, ast->left);
    EXTRACT(table_flags_attrs, create_table_name_flags->left);
    EXTRACT_OPTION(flags, table_flags_attrs->left);

    bool_t temp = !!(flags & TABLE_IS_TEMP);
    if (temp != temp_required) {
      continue;
    }

    if (region && schema_declare) {
      bprintf(output, "@begin_schema_region %s;\n", region);
    }

    gen_set_output_buffer(output);
    gen_statement_with_callbacks(ast_output, use_callbacks);
    bprintf(output, ";\n");

    if (region && schema_declare) {
      bprintf(output, "@end_schema_region;\n");
    }
    bprintf(output, "\n");
  }

  for (list_item *item = all_views_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_view_stmt(ast));

    CSTR region = ast->sem->region;

    if (!include_from_region(region, mode)) {
      continue;
    }

    EXTRACT_OPTION(flags, ast->left);
    bool_t temp = !!(flags & VIEW_IS_TEMP);
    if (temp != temp_required) {
      continue;
    }

    if (region && schema_declare) {
      bprintf(output, "@begin_schema_region %s;\n", region);
    }

    gen_set_output_buffer(output);
    gen_statement_with_callbacks(ast, use_callbacks);
    bprintf(output, ";\n");

    if (region && schema_declare) {
      bprintf(output, "@end_schema_region;\n");
    }
    bprintf(output, "\n");
  }

  // Indices are never TEMP in Sqlite, so if temp required then skip entirely
  if (!temp_required) {
    for (list_item *item = all_indices_list; item; item = item->next) {
      ast_node *ast = item->ast;
      Invariant(is_ast_create_index_stmt(ast));

      CSTR region = ast->sem->region;

      if (!include_from_region(region, mode)) {
        continue;
      }

      if (region && schema_declare) {
        bprintf(output, "@begin_schema_region %s;\n", region);
      }

      gen_set_output_buffer(output);
      gen_statement_with_callbacks(ast, use_callbacks);
      bprintf(output, ";\n");

      if (region && schema_declare) {
        bprintf(output, "@end_schema_region;\n");
      }
      bprintf(output, "\n");
    }
  }

  for (list_item *item = all_triggers_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_trigger_stmt(ast));

    CSTR region = ast->sem->region;

    if (!include_from_region(region, mode)) {
      continue;
    }

    EXTRACT_OPTION(flags, ast->left);
    bool_t temp = !!(flags & TRIGGER_IS_TEMP);
    if (temp != temp_required) {
      continue;
    }

    if (region && schema_declare) {
      bprintf(output, "@begin_schema_region %s;\n", region);
    }

    gen_set_output_buffer(output);
    gen_statement_with_callbacks(ast, use_callbacks);
    bprintf(output, ";\n");

    if (region && schema_declare) {
      bprintf(output, "@end_schema_region;\n");
    }
    bprintf(output, "\n");
  }

  // there are no "temp" migrations, so don't emit these at all if "temp required" is set
  if (!temp_required) {
    for (list_item *item = all_ad_hoc_list; item; item = item->next) {
      ast_node *ast = item->ast;
      Invariant(is_ast_schema_ad_hoc_migration_stmt(ast));

      CSTR region = ast->sem->region;

      if (!include_from_region(region, mode)) {
        continue;
      }

      if (region && schema_declare) {
        bprintf(output, "@begin_schema_region %s;\n", region);
      }

      gen_set_output_buffer(output);
      gen_statement_with_callbacks(ast, use_callbacks);
      bprintf(output, ";\n");

      if (region && schema_declare) {
        bprintf(output, "@end_schema_region;\n");
      }
      bprintf(output, "\n");
    }
  }
}

// This entry point is for generating a full image of the declared schema
// this is used to create the "previous" schema for the next run.
cql_noexport void cg_schema_main(ast_node *head) {
  Invariant(options.file_names_count == 1);
  cql_exit_on_semantic_errors(head);

  // Here we're going to output all the necessary declarations for all the schema in the indicated regions.
  CHARBUF_OPEN(output_file);
  bprintf(&output_file, "%s", rt->source_prefix);
  cg_generate_schema_by_mode(&output_file, SCHEMA_TO_DECLARE);
  cql_write_file(options.file_names[0], output_file.ptr);
  CHARBUF_CLOSE(output_file);
}

static void cg_schema_manage_triggers(charbuf *output, int32_t *drops, int32_t *creates) {
  Contract(creates);
  Contract(drops);
  CHARBUF_OPEN(create);
  CHARBUF_OPEN(drop);

  // non-null-callbacks will generate SQL for Sqlite (no attributes)
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_no_annotations;

  *creates = 0;
  *drops = 1;  // for now we have at least one for the legacy drops

  for (list_item *item = all_triggers_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_trigger_stmt(ast));

    if (!include_from_region(ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    EXTRACT_OPTION(flags, ast->left);
    EXTRACT_NOTNULL(trigger_body_vers, ast->right);
    EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
    EXTRACT_ANY_NOTNULL(trigger_name_ast, trigger_def->left);
    EXTRACT_STRING(name, trigger_name_ast);

    if (flags & TRIGGER_IS_TEMP) {
      continue;
    }

    bprintf(&drop, "  DROP TRIGGER IF EXISTS %s;\n", name);
    (*drops)++;

    // if not deleted, emit the create
    if (ast->sem->delete_version < 0) {
      gen_set_output_buffer(&create);
      gen_statement_with_callbacks(ast, &callbacks);
      bprintf(&create, ";\n");
      (*creates)++;
    }
  }

  if (*drops) {
    bprintf(output, "-- drop all the triggers we know\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_drop_all_triggers()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bprintf(output, "  CALL %s_cql_drop_legacy_triggers();\n", global_proc_name);
    bprintf(output, "%s", drop.ptr);
    bprintf(output, "END;\n\n");
  }

  if (*creates) {
    bprintf(output, "-- create all the triggers we know\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_create_all_triggers()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bindent(output, &create, 2);
    bprintf(output, "END;\n\n");
  }

  CHARBUF_CLOSE(drop);
  CHARBUF_CLOSE(create);
}

static void cg_schema_manage_views(charbuf *output, int32_t *drops, int32_t *creates) {
  Contract(creates);
  Contract(drops);
  CHARBUF_OPEN(create);
  CHARBUF_OPEN(drop);

  // non-null-callbacks will generate SQL for Sqlite (no attributes)
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_no_annotations;

  *drops = *creates = 0;

  for (list_item *item = all_views_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_view_stmt(ast));

    if (!include_from_region(ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    EXTRACT_OPTION(flags, ast->left);
    EXTRACT(view_and_attrs, ast->right);
    EXTRACT(name_and_select, view_and_attrs->left);
    EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
    EXTRACT_STRING(name, name_ast);

    if (flags & VIEW_IS_TEMP) {
      continue;
    }

    bprintf(&drop, "  DROP VIEW IF EXISTS %s;\n", name);
    (*drops)++;

    // if not deleted, emit the create
    if (ast->sem->delete_version < 0) {
      gen_set_output_buffer(&create);
      gen_statement_with_callbacks(ast, &callbacks);
      bprintf(&create, ";\n");
      (*creates)++;
    }
  }

  if (*drops) {
    bprintf(output, "-- drop all the views we know\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_drop_all_views()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bprintf(output, "%s", drop.ptr);
    bprintf(output, "END;\n\n");
  }

  if (*creates) {
    bprintf(output, "-- create all the views we know\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_create_all_views()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bindent(output, &create, 2);
    bprintf(output, "END;\n\n");
  }

  CHARBUF_CLOSE(drop);
  CHARBUF_CLOSE(create);
}

static void cg_schema_manage_indices(charbuf *output, int32_t *drops, int32_t *creates) {
  Contract(creates);
  Contract(drops);
  CHARBUF_OPEN(create);
  CHARBUF_OPEN(drop);

  // non-null-callbacks will generate SQL for Sqlite (no attributes)
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_no_annotations;

  *drops = *creates = 0;

  for (list_item *item = all_indices_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_index_stmt(ast));

    if (!include_from_region(ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    Contract(is_ast_create_index_stmt(ast));
    EXTRACT_NOTNULL(create_index_on_list, ast->left);
    EXTRACT_NOTNULL(flags_names_attrs, ast->right);
    EXTRACT_NOTNULL(connector, flags_names_attrs->right);
    EXTRACT_NOTNULL(index_names_and_attrs, connector->left);
    EXTRACT_NOTNULL(indexed_columns, index_names_and_attrs->left);
    EXTRACT(opt_where, index_names_and_attrs->right);
    EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
    EXTRACT_STRING(index_name, index_name_ast);
    EXTRACT_ANY_NOTNULL(table_name_ast, create_index_on_list->right);
    EXTRACT_STRING(table_name, table_name_ast);

    if (ast->sem->delete_version > 0) {
      // delete only, we're done here
      bprintf(&drop, "  DROP INDEX IF EXISTS %s;\n", index_name);
      (*drops)++;
      continue;
    }

    // If this index is attached to a table marked @recreate then we recreate the index with the table
    // as a unit so there is nothing to do here.  The index will be in the same @recreate group as
    // the table if it has one.
    ast_node *table_ast = find_table_or_view_even_deleted(table_name);

    Invariant(table_ast);
    Invariant(table_ast->sem);
    if (table_ast->sem->recreate) {
      // recreate table ... skip it as above.
      continue;
    }

    // drop then recreate after other migrate steps

    CHARBUF_OPEN(make_index);

    gen_set_output_buffer(&make_index);
    gen_statement_with_callbacks(ast, &callbacks);
    bprintf(&make_index, ";\n");

    llint_t index_crc = (llint_t)crc_charbuf(&make_index);

    bprintf(&drop, "  IF cql_facet_find(%s_facets, '%s_index_crc') != %lld THEN\n", global_proc_name, index_name, index_crc);
    bprintf(&drop, "    DROP INDEX IF EXISTS %s;\n", index_name);
    bprintf(&drop, "  END IF;\n");

    bprintf(&create, "  IF cql_facet_find(%s_facets, '%s_index_crc') != %lld THEN\n", global_proc_name, index_name, index_crc);
    bindent(&create, &make_index, 4);
    bprintf(&create, "    CALL %s_cql_set_facet_version('%s_index_crc', %lld);\n", global_proc_name, index_name, index_crc);
    bprintf(&create, "  END IF;\n");

    CHARBUF_CLOSE(make_index);

    // we always have a mutation plan for potentially changed indices so
    // that means there is a drop and a create
    (*creates)++;
    (*drops)++;
  }

  if (*drops) {
    bprintf(output, "-- drop all the indices that are deleted or changing\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_drop_all_indices()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bprintf(output, "%s", drop.ptr);
    bprintf(output, "END;\n\n");
  }

  if (*creates) {
    bprintf(output, "-- create all the indices we need\n");
    bprintf(output, "@attribute(cql:private)\n");
    bprintf(output, "CREATE PROCEDURE %s_cql_create_all_indices()\n", global_proc_name);
    bprintf(output, "BEGIN\n");
    bprintf(output, "%s", create.ptr);
    bprintf(output, "END;\n\n");
  }

  CHARBUF_CLOSE(drop);
  CHARBUF_CLOSE(create);
}

static void cg_schema_manage_recreate_tables(charbuf *output, recreate_annotation *notes, size_t count) {
  Contract(notes);
  Contract(count);

  CHARBUF_OPEN(recreate);
  CHARBUF_OPEN(update_tables);

  // non-null-callbacks will generate SQL for Sqlite (no attributes)
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.mode = gen_mode_no_annotations;

  crc_t table_crc = 0;

  for (size_t i = 0; i < count; i++) {
    recreate_annotation *note = &notes[i];

    ast_node *ast = note->target_ast;
    ast_node *ast_output = ast;

    Invariant(is_ast_create_table_stmt(ast));

    if (is_virtual_ast(ast)) {
      ast_output = ast->parent;
      Invariant(is_ast_create_virtual_table_stmt(ast_output));
    }

    if (!include_from_region(ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    EXTRACT(create_table_name_flags, ast->left);
    EXTRACT(table_flags_attrs, create_table_name_flags->left);
    EXTRACT_STRING(table_name, create_table_name_flags->right);

    // recreate if needed

    CHARBUF_OPEN(make_table);

    gen_set_output_buffer(&make_table);
    gen_statement_with_callbacks(ast_output, &callbacks);
    bprintf(&make_table, ";\n");

    // note that this will also drop any indices that are on the table
    bprintf(&update_tables, "    DROP TABLE IF EXISTS %s;\n", table_name);

    list_item *index_list = ast->sem->index_list;

    // now create the various indices but not the deleted ones
    for (list_item *item = index_list; item; item = item->next) {
      ast_node *index = item->ast;
      // deleted index, don't recreate it
      if (index->sem->delete_version > 0) {
        continue;
      }
      gen_statement_with_callbacks(index, &callbacks);
      bprintf(&make_table, ";\n");
    }

    table_crc ^= crc_charbuf(&make_table);
    bindent(&update_tables, &make_table, 4);

    CHARBUF_CLOSE(make_table);

    CSTR gname = note->group_name;

    // if there is a group and and this node can be merged with the next
    // the hold the update and accumulate the CRC
    if (i + 1 < count && gname[0] && !Strcasecmp(gname, (note+1)->group_name)) {
      continue;
    }

    CHARBUF_OPEN(facet);

    if (gname[0]) {
      // we're updating the whole group
      bprintf(&facet, "%s_group_crc", gname);
    }
    else {
      bprintf(&facet, "%s_table_crc", table_name);
    }

    bprintf(&recreate, "  IF cql_facet_find(%s_facets, '%s') != %lld THEN\n", global_proc_name, facet.ptr, (llint_t)table_crc);
    bprintf(&recreate, "%s", update_tables.ptr);
    bprintf(&recreate, "    CALL %s_cql_set_facet_version('%s', %lld);\n", global_proc_name, facet.ptr, (llint_t)table_crc);
    bprintf(&recreate, "  END IF;\n");

    CHARBUF_CLOSE(facet);

    // once we emit, we reset the CRC we've been accumulating and reset the buffer of table recreates
    table_crc = 0;
    bclear(&update_tables);
  }

  bprintf(output, "-- recreate all the @recreate tables that might have changed\n");
  bprintf(output, "@attribute(cql:private)\n");
  bprintf(output, "CREATE PROCEDURE %s_cql_recreate_tables()\n", global_proc_name);
  bprintf(output, "BEGIN\n");
  bprintf(output, "%s", recreate.ptr);
  bprintf(output, "END;\n\n");

  CHARBUF_CLOSE(update_tables);
  CHARBUF_CLOSE(recreate);
}

// Main entry point for schema upgrade code-gen.
cql_noexport void cg_schema_upgrade_main(ast_node *head) {
  Contract(options.file_names_count == 1);

  cql_exit_on_semantic_errors(head);
  exit_on_no_global_proc();

  // first sort the schema annotations according to version, type etc.
  // we want to process these in an orderly fashion and the upgrade rules
  // are nothing like the declared order.
  void *base = schema_annotations->ptr;
  size_t schema_items_size = sizeof(schema_annotation);
  size_t schema_items_count = schema_annotations->used / schema_items_size;
  schema_annotation *notes = (schema_annotation*)base;
  int32_t max_schema_version = 0;
  if (schema_items_count) {
     qsort(base, schema_items_count, schema_items_size, annotation_comparator);
     max_schema_version = notes[schema_items_count - 1].version;
  }

  // likewise, @recreate annotations, in the correct upgrade order (see comparator)
  base = recreate_annotations->ptr;
  size_t recreate_items_size = sizeof(recreate_annotation);
  size_t recreate_items_count = recreate_annotations->used / recreate_items_size;
  if (recreate_items_count) {
    qsort(base, recreate_items_count, recreate_items_size, recreate_comparator);
  }
  recreate_annotation *recreates = (recreate_annotation *)base;

  CHARBUF_OPEN(all_schema);
  // emit canonicalized schema for everything we will upgrade
  // this will include the schema declarations for the ad hoc migrations, too;
  cg_generate_schema_by_mode(&all_schema, SCHEMA_TO_UPGRADE);

  // compute the master CRC using schema and migration scripts
  llint_t schema_crc = (llint_t)crc_charbuf(&all_schema);

  CHARBUF_CLOSE(all_schema);

  CHARBUF_OPEN(preamble);
  CHARBUF_OPEN(main);
  CHARBUF_OPEN(decls);
  CHARBUF_OPEN(pending);
  CHARBUF_OPEN(upgrade);
  CHARBUF_OPEN(baseline);

  bprintf(&decls, "%s", rt->source_prefix);
  bprintf(&decls, "-- no columns will be considered hidden in this script\n");
  bprintf(&decls, "-- DDL in procs will not count as declarations\n");
  bprintf(&decls, "@SCHEMA_UPGRADE_SCRIPT;\n\n");
  bprintf(&decls, "-- schema crc %lld\n\n", schema_crc);

  cg_schema_emit_facet_functions(&decls);
  cg_schema_emit_sqlite_master(&decls);
  bprintf(&decls, "-- declare full schema of tables and views to be upgraded and their dependencies -- \n");
  cg_generate_schema_by_mode(&decls, SCHEMA_TO_DECLARE);
  cg_schema_helpers(&decls);

  bprintf(&decls, "-- declared upgrade procedures if any\n");

  cg_schema_emit_baseline_tables_proc(&preamble, &baseline);

  int32_t view_creates = 0, view_drops = 0;
  cg_schema_manage_views(&preamble, &view_drops, &view_creates);

  int32_t index_creates = 0, index_drops = 0;
  cg_schema_manage_indices(&preamble, &index_drops, &index_creates);

  int32_t trigger_creates = 0, trigger_drops = 0;
  cg_schema_manage_triggers(&preamble, &trigger_drops, &trigger_creates);

  if (recreate_items_count) {
    cg_schema_manage_recreate_tables(&preamble, recreates, recreate_items_count);
  }

  bool_t has_temp_schema = cg_schema_emit_temp_schema_proc(&preamble);
  bool_t one_time_drop_needed = false;

  bprintf(&main, "\n@attribute(cql:private)\n");
  bprintf(&main, "CREATE PROCEDURE %s_perform_upgrade_steps()\n", global_proc_name);
  bprintf(&main, "BEGIN\n");
  bprintf(&main, "  DECLARE schema_version LONG INTEGER NOT NULL;\n");

  if (view_drops) {
    bprintf(&main, "    -- dropping all views --\n");
    bprintf(&main, "    CALL %s_cql_drop_all_views();\n\n", global_proc_name);
  }

  if (index_drops) {
    bprintf(&main, "    -- dropping condemned or changing indices --\n");
    bprintf(&main, "    CALL %s_cql_drop_all_indices();\n\n", global_proc_name);
  }

  if (trigger_drops) {
    bprintf(&main, "    -- dropping condemned or changing triggers --\n");
    bprintf(&main, "    CALL %s_cql_drop_all_triggers();\n\n", global_proc_name);
  }

  if (baseline.used > 1) {
    llint_t baseline_crc = (llint_t)crc_charbuf(&baseline);
    bprintf(&main, "    ---- install baseline schema if needed ----\n\n");
    bprintf(&main, "    CALL %s_cql_get_version_crc(0, schema_version);\n", global_proc_name);
    bprintf(&main, "    IF schema_version != %lld THEN\n", baseline_crc);
    bprintf(&main, "      CALL %s_cql_install_baseline_schema();\n", global_proc_name);
    bprintf(&main, "      CALL %s_cql_set_version_crc(0, %lld);\n", global_proc_name, baseline_crc);
    bprintf(&main, "    END IF;\n\n");
  }

  int32_t prev_version = 0;

  for (int32_t i = 0; i < schema_items_count; i++) {
    schema_annotation *note = &notes[i];

    ast_node *version_annotation = note->annotation_ast;

    uint32_t type = note->annotation_type;
    Contract(type >= SCHEMA_ANNOTATION_FIRST && type <= SCHEMA_ANNOTATION_LAST);

    Contract(is_ast_version_annotation(version_annotation));
    EXTRACT_OPTION(vers, version_annotation->left);

    Invariant(note->version == vers);
    Invariant(vers > 0);

    if (prev_version != vers) {
      cg_schema_end_version(&main, &upgrade, &pending, prev_version);
      prev_version = vers;
    }

    CSTR target_name = note->target_name;

    Invariant(type >= SCHEMA_ANNOTATION_FIRST && type <= SCHEMA_ANNOTATION_LAST);

    if (!include_from_region(note->target_ast->sem->region, SCHEMA_TO_UPGRADE)) {
      continue;
    }

    switch (type) {
      case SCHEMA_ANNOTATION_CREATE_COLUMN: {
        ast_node *def = note->column_ast;
        Contract(is_ast_col_def(def));
        EXTRACT_NOTNULL(col_def_type_attrs, def->left);
        EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
        EXTRACT_STRING(col_name, col_def_name_type->left);

        CSTR col_type = coretype_string(def->sem->sem_type);
        gen_sql_callbacks callbacks;
        init_gen_sql_callbacks(&callbacks);
        callbacks.mode = gen_mode_no_annotations;

        CHARBUF_OPEN(sql_out);
        gen_set_output_buffer(&sql_out);
        // no-op callbacks still suppress @create/@delete which is not legal in alter table
        gen_col_def_with_callbacks(def, &callbacks);

        bprintf(&upgrade, "      -- altering table %s to add column %s %s;\n\n",
          target_name,
          col_name,
          col_type);
        bprintf(&upgrade, "      IF NOT %s_check_column_exists('%s', '*[( ]%s %s*') THEN \n",
          global_proc_name,
          target_name,
          col_name,
          col_type);
        bprintf(&upgrade, "        ALTER TABLE %s ADD COLUMN %s;\n",
          target_name,
          sql_out.ptr);
        bprintf(&upgrade, "      END IF;\n\n");

        CHARBUF_CLOSE(sql_out);
        break;
      }

      case SCHEMA_ANNOTATION_DELETE_COLUMN: {
        ast_node *def = note->column_ast;
        Contract(is_ast_col_def(def));
        EXTRACT_NOTNULL(col_def_type_attrs, def->left);
        EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
        EXTRACT_STRING(col_name, col_def_name_type->left);

        bprintf(&upgrade, "      -- logical delete of column %s from %s; -- no ddl\n\n", col_name, target_name);
        break;
      }

      case SCHEMA_ANNOTATION_CREATE_TABLE: {
        // check for one time drop

        EXTRACT_ANY(dot, version_annotation->right);
        if (dot && is_ast_dot(dot)) {
          EXTRACT_STRING(lhs, dot->left);
          EXTRACT_STRING(rhs, dot->right);

          if (!Strcasecmp(lhs, "cql") && !Strcasecmp(rhs, "from_recreate")) {
            bprintf(&upgrade, "      -- one time drop %s\n\n", target_name);
            bprintf(&upgrade, "      CALL %s_cql_one_time_drop('%s', %d);\n\n", global_proc_name, target_name, vers);
            one_time_drop_needed = true;
          }
        }

        bprintf(&upgrade, "      -- creating table %s\n\n", target_name);


        gen_sql_callbacks callbacks;
        init_gen_sql_callbacks(&callbacks);
        callbacks.col_def_callback = cg_suppress_new_col_def;
        callbacks.if_not_exists_callback = cg_schema_force_if_not_exists;
        callbacks.mode = gen_mode_no_annotations;

        CHARBUF_OPEN(sql_out);
        gen_set_output_buffer(&sql_out);
        gen_statement_with_callbacks(note->target_ast, &callbacks);  // only the original columns

        bindent(&upgrade, &sql_out, 6);
        bprintf(&upgrade, ";\n\n");

        CHARBUF_CLOSE(sql_out);
        break;
      }

      case SCHEMA_ANNOTATION_DELETE_TABLE: {
        // this is all that's left
        Contract(note->annotation_type == SCHEMA_ANNOTATION_DELETE_TABLE);

        bprintf(&upgrade, "      -- dropping table %s\n\n", target_name);
        bprintf(&upgrade, "      DROP TABLE IF EXISTS %s;\n\n", target_name);
        break;
      }

      // Note: @create is invalid for INDEX/VIEW/TRIGGER so there can be no such annotation

      case SCHEMA_ANNOTATION_DELETE_INDEX:
      case SCHEMA_ANNOTATION_DELETE_VIEW:
      case SCHEMA_ANNOTATION_DELETE_TRIGGER:
        // no annotation based actions other than migration proc (handled below
        Contract(version_annotation->right);
        bprintf(&upgrade, "      -- delete migration proc for %s will run\n\n", target_name);
        break;

      case SCHEMA_ANNOTATION_AD_HOC:
        // no annotation based actions other than migration proc (handled below)
        Contract(version_annotation->right);
        bprintf(&upgrade, "      -- ad hoc migration proc %s will run\n\n", target_name);
        break;
    }

    // handle any migration proc for any annotation
    if (version_annotation->right) {
      // call any non-builtin migrations the generic way, builtins get whatever special handling they need
      if (!is_ast_dot(version_annotation->right)) {
        EXTRACT_STRING(proc, version_annotation->right);
        bprintf(&pending, "      IF cql_facet_find(%s_facets, '%s') = -1 THEN\n", global_proc_name, proc);
        bprintf(&pending, "        CALL %s();\n", proc);
        bprintf(&pending, "        CALL %s_cql_set_facet_version('%s', %d);\n", global_proc_name, proc, vers);
        bprintf(&pending, "      END IF;\n");
        bprintf(&decls, "DECLARE proc %s() USING TRANSACTION;\n", proc);
      }
    }
  }

  cg_schema_end_version(&main, &upgrade, &pending, prev_version);

  if (recreate_items_count) {
    bprintf(&main, "    CALL %s_cql_recreate_tables();\n", global_proc_name);
  }

  if (view_creates) {
    bprintf(&main, "    CALL %s_cql_create_all_views();\n", global_proc_name);
  }

  if (index_creates) {
    bprintf(&main, "    CALL %s_cql_create_all_indices();\n", global_proc_name);
  }

  if (trigger_creates) {
    bprintf(&main, "    CALL %s_cql_create_all_triggers();\n", global_proc_name);
  }

  bprintf(&main, "    CALL %s_cql_set_facet_version('cql_schema_version', %d);\n", global_proc_name, prev_version);
  bprintf(&main, "    CALL %s_cql_set_facet_version('cql_schema_crc', %lld);\n", global_proc_name, schema_crc);
  bprintf(&main, "END;\n\n");

  bprintf(&main, "@attribute(cql:private)\n");
  bprintf(&main, "CREATE PROCEDURE %s_setup_facets()\n", global_proc_name);
  bprintf(&main, "BEGIN\n");
  bprintf(&main, "  BEGIN TRY\n");
  bprintf(&main, "    SET %s_facets := cql_facets_new();\n", global_proc_name);
  bprintf(&main, "    DECLARE C CURSOR FOR SELECT * from %s_cql_schema_facets;\n", global_proc_name);
  bprintf(&main, "    LOOP FETCH C\n");
  bprintf(&main, "    BEGIN\n");
  bprintf(&main, "      LET added := cql_facet_add(%s_facets, C.facet, C.version);\n", global_proc_name);
  bprintf(&main, "    END;\n");
  bprintf(&main, "  END TRY;\n");
  bprintf(&main, "  BEGIN CATCH\n");
  bprintf(&main, "   -- if table doesn't exist we just have empty facets, that's ok\n");
  bprintf(&main, "  END CATCH;\n");
  bprintf(&main, "END;\n\n");

  bprintf(&main, "@attribute(cql:private)\n");
  bprintf(&main, "CREATE PROCEDURE %s_perform_needed_upgrades()\n", global_proc_name);
  bprintf(&main, "BEGIN\n");
  bprintf(&main, "  -- check for downgrade --\n");
  bprintf(&main, "  IF cql_facet_find(%s_facets, 'cql_schema_version') > %d THEN\n", global_proc_name, max_schema_version);
  bprintf(&main, "    SELECT 'downgrade detected' facet;\n");
  bprintf(&main, "  ELSE\n");
  bprintf(&main, "    -- save the current facets so we can diff them later --\n");
  bprintf(&main, "    CALL %s_save_cql_schema_facets();\n", global_proc_name);
  bprintf(&main, "    CALL %s_perform_upgrade_steps();\n\n", global_proc_name);
  bprintf(&main, "    -- finally produce the list of differences\n");
  bprintf(&main, "    SELECT T1.facet FROM\n");
  bprintf(&main, "      %s_cql_schema_facets T1\n", global_proc_name);
  bprintf(&main, "      LEFT OUTER JOIN %s_cql_schema_facets_saved T2\n", global_proc_name);
  bprintf(&main, "        ON T1.facet = T2.facet\n", global_proc_name);
  bprintf(&main, "      WHERE T1.version is not T2.version;\n");
  bprintf(&main, "  END IF;\n");
  bprintf(&main, "END;\n\n");

  bprintf(&main, "CREATE PROCEDURE %s()\n", global_proc_name);
  bprintf(&main, "BEGIN\n");
  bprintf(&main, "  DECLARE schema_crc LONG INTEGER NOT NULL;\n");
  bprintf(&main, "\n");
  bprintf(&main, "  -- create schema facets information table --\n");
  bprintf(&main, "  CALL %s_create_cql_schema_facets_if_needed();\n\n", global_proc_name);
  bprintf(&main, "  -- fetch the last known schema crc, if it's different do the upgrade --\n");
  bprintf(&main, "  CALL %s_cql_get_facet_version('cql_schema_crc', schema_crc);\n\n", global_proc_name);
  bprintf(&main, "  IF schema_crc <> %lld THEN\n", (llint_t)schema_crc);
  bprintf(&main, "    BEGIN TRY\n");
  bprintf(&main, "      CALL %s_setup_facets();\n", global_proc_name);
  bprintf(&main, "      CALL %s_perform_needed_upgrades();\n", global_proc_name);
  bprintf(&main, "    END TRY;\n");
  bprintf(&main, "    BEGIN CATCH\n");
  bprintf(&main, "      CALL cql_facets_delete(%s_facets);\n", global_proc_name);
  bprintf(&main, "      SET %s_facets := 0;\n", global_proc_name);
  bprintf(&main, "      THROW;\n");
  bprintf(&main, "    END CATCH;\n");
  bprintf(&main, "    CALL cql_facets_delete(%s_facets);\n", global_proc_name);
  bprintf(&main, "    SET %s_facets := 0;\n", global_proc_name);
  bprintf(&main, "  ELSE\n");
  bprintf(&main, "    -- some canonical result for no differences --\n");
  bprintf(&main, "    SELECT 'no differences' facet;\n");
  bprintf(&main, "  END IF;\n");

  if (has_temp_schema) {
    bprintf(&main, "  ---- install temp schema after upgrade is complete ----\n");
    bprintf(&main, "  CALL %s_cql_install_temp_schema();\n\n", global_proc_name);
  }

  bprintf(&main, "END;\n\n");

  if (one_time_drop_needed) {
    cg_schema_emit_one_time_drop(&decls);
  }

  CHARBUF_OPEN(output_file);
  bprintf(&output_file, "%s\n", decls.ptr);
  bprintf(&output_file, "%s", preamble.ptr);
  bprintf(&output_file, "%s", main.ptr);

  cql_write_file(options.file_names[0], output_file.ptr);

  CHARBUF_CLOSE(output_file);

  CHARBUF_CLOSE(baseline);
  CHARBUF_CLOSE(upgrade);
  CHARBUF_CLOSE(pending);
  CHARBUF_CLOSE(decls);
  CHARBUF_CLOSE(main);
  CHARBUF_CLOSE(preamble);
}

#endif

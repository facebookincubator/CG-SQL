---
id: int06
title: "Part 6: Schema Management"
sidebar_label: "Part 6: Schema Management"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 6 continues with a discussion of the essentials schema management in the CQL compiler.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how schema management happens in general -- the core strategies and implementation choices --
so that when reading the management code you will have an idea how it all hangs together. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## Schema Management

The primary goal of the schema management features of the CQL compiler is to provide the ability
to create a "schema upgrader" that can move a given users database from a previous version
of the schema to the current version.  Because of the limitations of SQL in general and
SQLite in particular not all transforms are possible; so additionally the system must correctly
detect and prevent upgrades that cannot be safely performed.

The full set of schema attributes and their meaning is described in [Chapter 10](https://cgsql.dev/cql-guide/ch10)
and the full set of validations is described in [Chapter 11](https://cgsql.dev/cql-guide/ch11).  Briefly the
directives are:

* `@create(n)`: indicates a table/column is to be created at version `n`.
* `@delete(n)`: indicates a table/column is to be deleted at version `n`.
* `@recreate`: indicates the table contents are not precious
   * it and can be dropped and created when its schema changes
   * this does not combine with `@create`
   * it applies only to tables
   * views, triggers, and indices are always on the `@recreate` plan and do not have to be marked so

Now the various annotations can occur substantially in any order as there are no rules that require that
tables that are created later appear later in the input.  This would be most inconvenient for any upgrading
logic.  However, the semantic validation pass gathers all such annotations into two large `bytebuf`
objects which can be readily sorted -- one for things on the `@create` plan and one for the `@recreate` plan.

And at his point it's probably best to start looking at some of the code fragments.

We're going to be looking at all the steps in the top level function:

```C
// Main entry point for schema upgrade code-gen.
cql_noexport void cg_schema_upgrade_main(ast_node *head) {
  Contract(options.file_names_count == 1);
  ...
}
```

Note that the schema upgrader does not produce `C` but rather it produces more `CQL` which then
has to be compiled down to `C`.  This choice means that the codegen is a lot more readable
and gets the benefit of the usual CQL error checking and exception management.

### Check for errors, check for `--global_proc`

We start with some simple error checks:  Any semantic errors abort the code-generation.
The `--global_proc` names the procedure that will do the upgrade and also is used
as a prefix on all of the tables that the upgrader requires, so that it is possible
to have separate upgraders for different parts of your schema if desired.  Or two
combine upgraders from two different unrelated subsystems in the same database.

```C
  cql_exit_on_semantic_errors(head);
  exit_on_no_global_proc();
```

### Preparing the Attributes

The two arrays `schema_annotations` and `recreate_annotations` are sorted.
The item count can be easily computed using the allocated size of these items,
both of which are of type `bytebuf`.  The comparators provided to `qsort`
put these arrays in exactle the order needed.

```C
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
```

### Creating the Global CRC

Schema upgrade is expensive, so we want to be able to quickly detect if the schema
installed is already the latest version. To do this we compute a single global
64 bit CRC for the current version of the schema.  This can be compared against a
stored schema CRC from the last run. If the CRCs match, no work needs to be done.

```C
  CHARBUF_OPEN(all_schema);
  // emit canonicalized schema for everything we will upgrade
  // this will include the schema declarations for the ad hoc migrations, too;
  cg_generate_schema_by_mode(&all_schema, SCHEMA_TO_UPGRADE);

  // compute the master CRC using schema and migration scripts
  llint_t schema_crc = (llint_t)crc_charbuf(&all_schema);

  CHARBUF_CLOSE(all_schema);
```

The schema generator is used to emit the full schema, including anannotations into 
a buffer. A raw CRC of the buffer gives us the "global" or "overall" CRC for the
whole schema.

### Output Fragments

A number of buffers will hold the various pieces of output.

```C
  CHARBUF_OPEN(preamble);
  CHARBUF_OPEN(main);
  CHARBUF_OPEN(decls);
  CHARBUF_OPEN(pending);
  CHARBUF_OPEN(upgrade);
  CHARBUF_OPEN(baseline);
```

These will be assembled as follows:

```C
  CHARBUF_OPEN(output_file);
  bprintf(&output_file, "%s\n", decls.ptr);
  bprintf(&output_file, "%s", preamble.ptr);
  bprintf(&output_file, "%s", main.ptr);

  cql_write_file(options.file_names[0], output_file.ptr);

  CHARBUF_CLOSE(output_file);
```

In short: 
* first `decls`, this declares the schema among other things
* then, `preamble`, this contains helper procedures
* then, `main`, the primary upgrader steps go here


We'll go over all of these in subsequent sections.

### Declarations Section

The result type includes a customizable prefix string.  This is the first thing to go out.
Typically this is the appropriate copyright notice.  `rt.c` has this information and that
file is replaceable.

```C
  bprintf(&decls, "%s", rt->source_prefix);
```

The schema upgrade script is in the business of creating tables from old versions and then altering them.
The table declarations will be for the final shape.  We need to emit `@SCHEMA_UPGRADE_SCRIPT` so that
the CQL compiler knows that there will be multiple declarations of the same table and they might not
be identical. The upgrade script is in the business of getting things to the end state.  Likewise
it is normal for the schema upgrade script to refer to columns that have been deleted, this is because
a column might be created in say version 5 and then deleted in version 10.  The upgrade code goes
through the columns lifecycle, so even though the declarations already say the column is doomed
to die in version 10, the creation code in version 5 is legal -- and necessary.  Schema migration steps
that run in version 6, 7, 8, or 9 might use the contents of the column as part of essential data migration.
We can never know what version we might find in a database that is being upgraded, it could be very far in
the past, at a time where a deleted column still existed.

```C
  bprintf(&decls, "-- no columns will be considered hidden in this script\n");
  bprintf(&decls, "-- DDL in procs will not count as declarations\n");
  bprintf(&decls, "@SCHEMA_UPGRADE_SCRIPT;\n\n");
```

A convenience comment goes in the `decls` section with the CRC.

```C
  bprintf(&decls, "-- schema crc %lld\n\n", schema_crc);
```

There are a set of functions that allow the creation of, and access to, an in-memory
cache of the facet state.  These functions are all defined in `cqlrt_common.c`.  But
they have to be declared to CQL to use them.

```C
  cg_schema_emit_facet_functions(&decls);
```

The table `sqlite_master` is used to read schema state.  That table has to be declared.

```C
  cg_schema_emit_sqlite_master(&decls);
```

The full schema may be used by the upgraders, we need a declaration of all of that.

```C
  bprintf(&decls, "-- declare full schema of tables and views to be upgraded and their dependencies -- \n");
  cg_generate_schema_by_mode(&decls, SCHEMA_TO_DECLARE);
```

At this point a quick side-step to the output modes and region arguments is appropriate.

#### Schema Region Arguments

The upgrader honors the arguments `--include_regions` and `--exclude_regions`.  If they are absent
that is the same as "include everything" and "exclude nothing".  Recall that schema regions allow
you to group schema as you wish.  A typical use might be to define some "core" schema in a set
of regions (maybe just one) and then a set of "optional" schema in some additional regions.

An upgrader for just "core" could be created by adding `--include_regions core`.  When creating
upgraders for the optional parts, there are two choices:

* `--include-regions optional1` : makes an upgrader for `optional1` and `core` (the assumption being that `optional1` was declared to depend on `core`)
* `--include-regions optional1` `--exclude-regions core` : makes an upgrader for `optional1` which should run after the standalone `core` upgrader has already run
  * this allows you to share the "core" parts between any number of "optional" parts
  * and of course this can nest; there can be several "core" parts; and so forth

#### Schema Output Modes

The flag bits are these:

```C
// We declare all schema we might depend on in this upgrade (this is the include list)
// e.g. we need all our dependent tables so that we can legally use them in an FK
#define SCHEMA_TO_DECLARE 1

// We only emit schema that we are actually updating (this is include - exclude)
// e.g. a table on the exclude list is assumed to be upgraded by its own script
// in a different run.
#define SCHEMA_TO_UPGRADE 2

// We get TEMP items IF and ONLY IF this bit is set
#define SCHEMA_TEMP_ITEMS 4
```

As we saw before, the schema we CRC is `SCHEMA_TO_UPGRADE`.  This is all the regions that were selected
but not their dependencies.  The point of this is that you might make an upgrader for say a "core"
part of your schema which can be shared and then make additional upgraders for various parts that
use the "core" but are otherwise "optional".  Each of those "optional" upgraders needs its own CRC that
includes its schema but not the "core" schema.  However the "optional" schema can refer to "core"
schema (e.g. in foreign keys) so all of the tables are declared.  This is `SCHEMA_TO_DECLARE` mode.

* declare all schema you are allowed to refer to
* CRC, and upgrade, only the parts selected by the region arguments

### The Schema Helpers

This bit generates the `facets` table, the full name is `your_global_proc_cql_schema_facets` where
`your_global_proc` is the `--global_proc` argument. This is referred to simply as the `facets` table.
There is an identical temporary table that is used to store the contents of the `facets` table
upon startup.  This allows the upgrader to produce a complete difference.  The `facets` table
is nothing more than a mapping between the name of some facet of the schema (like a table, a view,
a column) and its last known verison info -- usually its CRC.

* NOTE: this temp table predates the in-memory facets data structure so it could probably be removed
  * the diff would have to work against the in-memory datastructure which is immutable hence just as good as a temp table
  * look for a change like this soon

The remaining procedures are for testing facet state or `sqlite_master` state.  All of them get the
usual global prefix.  For ease of discussion I will elide the prefix for the rest of this document.

* `check_column_exists` : checks if the indicated column is present in `sqlite_master`
  * necessary because there is no `ALTER TABLE ADD COLUMN IF NOT EXISTS` command
* `create_cql_schema_facets_if_needed` : actually creates the `facets` table if it does not exist
* `save_cql_schema_facets` : creates the `cql_schema_facets_saved` temp table and populates it
* `cql_set_facet_version` : sets one facet to the indicated value
  * this writes to the database, not the in-memory version of the table
* `cql_get_facet_version` : reads a facet value from the facet table
  * this is only used to check the master schema value, after that the in-memory version is used
* `cql_get_version_crc` : gets the CRC for a given schema version
  * each schema version has its own CRC in addition to the global CRC
  * this information is stored in the facets table with a simple naming convention for the facet name
  * the in memory version of the table is always used here
* `cql_set_version_crc` : sets the CRC for a given schema version in the facet table
  * this writes to the database, not the in-memory version of the table
* `cql_drop_legacy_triggers` : drops any triggers of the from `tr__*`
  * for historical reasons the original triggers did not include tombstones when deleted
  * this kludge is here to clean up legacy triggers and its peculiar to Messenger only
  * this should really be removed from the OSS version but it's never been a priority
  * sorry...

```C
  cg_schema_helpers(&decls);
```

### Declared Upgrade Procedures

The annotations can include an upgrade procedure the term "migration" procedure is sometimes used
as well and is synonymous.  This is some code that should run after the schema alteration has
been made to create/delete/move some data around in the new schema.  Each of these must be
declared before it is used and the declarations will be here, at the end of the `decls` section
after this introductory comment.

```C
  bprintf(&decls, "-- declared upgrade procedures if any\n");
```

### The Upgrading Workers

The main upgrader will invoke these key workers to do its job.  This is where the `preamble`
section starts. It contains the meat of the upgrade steps wrapped in procedures that do
the job.  

```C
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
```

These are the last of the worker methods:

* `cg_schema_emit_baseline_tables_proc` : emits a procedure that will create the schema at its baseline version
  * this means whatever "v0" of the schema was, no creates or deletes have yet happened
* `cg_schema_manage_views` : creates the view management procedures
  * `cql_drop_all_views` : drops all views
  * `cql_create_all_views` : creates all views
  * both of these run unless the global CRC matches
* `cg_schema_manage_indices` : creates the index management procedures
  * `cql_drop_all_indices` : drops any index that exists and whose CRC changed
  * `cql_create_all_indices` : creates any index whose CRC changed
  * recreating indices can be costly so it is only done if the index actually changed
* `cg_schema_manage_triggers` : creates the trigger management procedures
  * `cql_drop_all_triggers` : drops all triggers
  * `cql_create_all_triggers` : creates all triggers
  * both of these run unless the global CRC matches
  * additionally any legacy triggers will be deleted (see `cql_drop_legacy_triggers`)
* `cg_schema_manage_recreate_tables` : creates the `cql_recreate_tables` worker
  * the `recreate_annotations` array is used to find all the recreate tables
  * the entires are sorted by group then name so that annotions with a group are together
  * the procedure contains code to delete the procedure or group and recreate it if the CRC does not match
  * the CRC is computed using the code for create instructions and is stored in a facet with a suitable name
  * the easist way to think of this code is that it always emits a chunk of recreates for a group
     * ungrouped tables are a group of 1
     * group delete/create instructions accumulate until the next entry is in a different group
* `cg_schema_emit_temp_schema_proc` : emits a procedure to create any temporary schema
  * temp tables are always created in full at the latest version
  * this code is run regardless of whether the global CRC matches or not

### The Main Upgrader

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

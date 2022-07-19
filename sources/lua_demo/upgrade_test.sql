/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- no columns will be considered hidden in this script
-- DDL in procs will not count as declarations
@SCHEMA_UPGRADE_SCRIPT;

-- schema crc -7461702544329564919

-- declare facet helpers-- 
DECLARE facet_data TYPE OBJECT<facet_data>;
DECLARE lua_upgrade_facets facet_data;
DECLARE FUNCTION cql_facets_create() create facet_data not null;
DECLARE FUNCTION cql_facet_add(facets facet_data, facet TEXT NOT NULL, crc LONG NOT NULL) BOOL NOT NULL;
DECLARE FUNCTION cql_facet_upsert(facets facet_data, facet TEXT NOT NULL, crc LONG NOT NULL) BOOL NOT NULL;
DECLARE FUNCTION cql_facet_find(facets facet_data, facet TEXT NOT NULL) LONG NOT NULL;

-- declare sqlite_master -- 
CREATE TABLE sqlite_master (
  type TEXT NOT NULL,
  name TEXT NOT NULL,
  tbl_name TEXT NOT NULL,
  rootpage INTEGER NOT NULL,
  sql TEXT NOT NULL
);

-- declare full schema of tables and views to be upgraded and their dependencies -- 
CREATE VIRTUAL TABLE test_virtual_table_A USING test_module AS (
  name TEXT
);

CREATE TABLE test_create_table_A(
  colA INTEGER,
  colB LONG_INT,
  colC TEXT,
  colD TEXT @CREATE(1)
) @DELETE(2);

CREATE TABLE test_create_table_B(
  colA TEXT,
  colB LONG_INT,
  colC INTEGER
) @CREATE(1) @DELETE(3);

CREATE TABLE test_create_table_C(
  colA TEXT,
  colB LONG_INT,
  colC LONG_INT
) @CREATE(2);

CREATE TABLE test_recreate_table_A(
  colA INTEGER,
  colC TEXT,
  colD TEXT
) @RECREATE @DELETE;

CREATE TABLE test_recreate_table_B(
  colA INTEGER,
  colB LONG_INT,
  colC INTEGER
) @RECREATE;

CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT
) @RECREATE(gr1);

CREATE TABLE use_g1(
  id INTEGER PRIMARY KEY REFERENCES g1 (id),
  name2 TEXT
) @RECREATE(gr1);

CREATE TABLE test_this_table_will_become_create(
  id INTEGER PRIMARY KEY
) @CREATE(1, cql:from_recreate) @DELETE(3);

CREATE TABLE test_for_unsub(
  unsub_id INTEGER,
  x TEXT
);

CREATE TABLE recreate_test_for_unsub(
  unsub_id INTEGER,
  x TEXT
) @RECREATE(a_recreate_group);

CREATE VIEW test_view AS
SELECT colA, colB
  FROM test_create_table_C;

CREATE VIEW staying_view AS
SELECT *
  FROM g1;

CREATE INDEX test_for_unsub_index ON test_for_unsub (x);

CREATE INDEX recreate_test_for_unsub_index ON recreate_test_for_unsub (x);

CREATE INDEX staying_index ON g1 (id);

CREATE TRIGGER test_for_unsub_trigger
  BEFORE DELETE ON test_for_unsub
  WHEN old.unsub_id = 3
BEGIN
DELETE FROM test_for_unsub WHERE unsub_id = 3;
END;

CREATE TRIGGER recreate_test_for_unsub_trigger
  BEFORE DELETE ON recreate_test_for_unsub
  WHEN old.unsub_id = 3
BEGIN
DELETE FROM recreate_test_for_unsub WHERE unsub_id = 3;
END;

CREATE TRIGGER staying_trigger
  BEFORE DELETE ON g1
BEGIN
SELECT 1;
END;

@UNSUB(1, test_for_unsub);

@UNSUB(1, recreate_test_for_unsub);

@RESUB(2, test_for_unsub);

@RESUB(2, recreate_test_for_unsub);

@UNSUB(3, test_for_unsub);

@UNSUB(3, recreate_test_for_unsub);

@RESUB(4, test_for_unsub);

@RESUB(4, recreate_test_for_unsub);

-- facets table declaration --
CREATE TABLE IF NOT EXISTS lua_upgrade_cql_schema_facets(
  facet TEXT NOT NULL PRIMARY KEY,
  version LONG INTEGER NOT NULL
);

-- helper proc for getting the schema version of a facet
CREATE PROCEDURE lua_upgrade_cql_get_facet_version(_facet TEXT NOT NULL, out _version LONG INTEGER NOT NULL)
BEGIN
  BEGIN TRY
    SET _version := (SELECT version FROM lua_upgrade_cql_schema_facets WHERE facet = _facet LIMIT 1 IF NOTHING -1);
  END TRY;
  BEGIN CATCH
    SET _version := -1;
  END CATCH;
END;

-- saved facets table declaration --
CREATE TEMP TABLE lua_upgrade_cql_schema_facets_saved(
  facet TEXT NOT NULL PRIMARY KEY,
  version LONG INTEGER NOT NULL
);

-- holds all the table definitions out of sqlite_master
DECLARE lua_upgrade_tables_dict_ OBJECT<string_dictionary>;

-- helper proc for creating the dictionary of table defs from sqlite_master
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_get_table_defs()
BEGIN
  DECLARE C CURSOR FOR SELECT name, sql from sqlite_master where type = 'table';
  SET lua_upgrade_tables_dict_ := cql_string_dictionary_create();
  LOOP FETCH C
  BEGIN
    LET added := cql_string_dictionary_add(lua_upgrade_tables_dict_, C.name, C.sql);
  END;
END;

-- helper proc for creating the schema version table
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_create_cql_schema_facets_if_needed()
BEGIN
  CREATE TABLE IF NOT EXISTS lua_upgrade_cql_schema_facets(
    facet TEXT NOT NULL PRIMARY KEY,
    version LONG INTEGER NOT NULL
  );
END;

-- helper proc for saving the schema version table
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_save_cql_schema_facets()
BEGIN
  DROP TABLE IF EXISTS lua_upgrade_cql_schema_facets_saved;
  CREATE TEMP TABLE lua_upgrade_cql_schema_facets_saved(
    facet TEXT NOT NULL PRIMARY KEY,
    version LONG INTEGER NOT NULL
  );
  INSERT INTO lua_upgrade_cql_schema_facets_saved
    SELECT * FROM lua_upgrade_cql_schema_facets;
END;

-- helper proc for setting the schema version of a facet
CREATE PROCEDURE lua_upgrade_cql_set_facet_version(_facet TEXT NOT NULL, _version LONG INTEGER NOT NULL)
BEGIN
  INSERT OR REPLACE INTO lua_upgrade_cql_schema_facets (facet, version) VALUES(_facet, _version);
  LET added := cql_facet_upsert(lua_upgrade_facets, _facet, _version);
END;

-- helper proc for getting the schema version CRC for a version index
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_get_version_crc(_v INTEGER NOT NULL, out _crc LONG INTEGER NOT NULL)
BEGIN
  SET _crc := cql_facet_find(lua_upgrade_facets, printf('cql_schema_v%d', _v));
END;

-- helper proc for setting the schema version CRC for a version index
CREATE PROCEDURE lua_upgrade_cql_set_version_crc(_v INTEGER NOT NULL, _crc LONG INTEGER NOT NULL)
BEGIN
  INSERT OR REPLACE INTO lua_upgrade_cql_schema_facets (facet, version) VALUES('cql_schema_v'||_v, _crc);
END;

-- helper proc to reset any triggers that are on the old plan --
DECLARE PROCEDURE cql_exec_internal(sql TEXT NOT NULL) USING TRANSACTION;

-- declared upgrade procedures if any
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_one_time_drop(name TEXT NOT NULL, version INTEGER NOT NULL)
BEGIN
  LET facet := printf('1_time_drop_%s', name);
  IF cql_facet_find(lua_upgrade_facets, facet) != version THEN
    call cql_exec_internal(printf('DROP TABLE IF EXISTS %s;', name));
    call lua_upgrade_cql_set_facet_version(facet, version);
  END IF;
END;


CREATE PROCEDURE lua_upgrade_cql_install_baseline_schema()
BEGIN
  CREATE TABLE IF NOT EXISTS test_create_table_A(
    colA INTEGER,
    colB LONG_INT,
    colC TEXT
  );

  CREATE TABLE IF NOT EXISTS test_for_unsub(
    unsub_id INTEGER,
    x TEXT
  );

END;
-- drop all the views we know
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_drop_all_views()
BEGIN
  DROP VIEW IF EXISTS test_view;
  DROP VIEW IF EXISTS staying_view;
END;

-- create all the views we know
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_create_all_views()
BEGIN
  CREATE VIEW test_view AS
  SELECT colA, colB
    FROM test_create_table_C;
  CREATE VIEW staying_view AS
  SELECT *
    FROM g1;
END;


-- drop all the indices that are deleted or changing
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_drop_all_indices()
BEGIN
  IF cql_facet_find(lua_upgrade_facets, 'test_for_unsub_index_index_crc') != 7034177079592179941 THEN
    DROP INDEX IF EXISTS test_for_unsub_index;
  END IF;
END;

-- create all the indices we need
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_create_all_indices()
BEGIN
  IF cql_facet_find(lua_upgrade_facets, 'test_for_unsub_index_index_crc') != 7034177079592179941 THEN
    CREATE INDEX test_for_unsub_index ON test_for_unsub (x);
    CALL lua_upgrade_cql_set_facet_version('test_for_unsub_index_index_crc', 7034177079592179941);
  END IF;
END;

-- drop all the triggers we know
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_drop_all_triggers()
BEGIN
  DROP TRIGGER IF EXISTS test_for_unsub_trigger;
  DROP TRIGGER IF EXISTS recreate_test_for_unsub_trigger;
  DROP TRIGGER IF EXISTS staying_trigger;
END;

-- create all the triggers we know
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_create_all_triggers()
BEGIN
  CREATE TRIGGER test_for_unsub_trigger
    BEFORE DELETE ON test_for_unsub
    WHEN old.unsub_id = 3
  BEGIN
  DELETE FROM test_for_unsub WHERE unsub_id = 3;
  END;
  CREATE TRIGGER recreate_test_for_unsub_trigger
    BEFORE DELETE ON recreate_test_for_unsub
    WHEN old.unsub_id = 3
  BEGIN
  DELETE FROM recreate_test_for_unsub WHERE unsub_id = 3;
  END;
  CREATE TRIGGER staying_trigger
    BEFORE DELETE ON g1
  BEGIN
  SELECT 1;
  END;
END;

-- recreate all the non-virtual @recreate tables that might have changed
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_recreate_non_virtual_tables()
BEGIN
  IF cql_facet_find(lua_upgrade_facets, 'all_nonvirtual_tables_crc') == 3646222684755303712 RETURN; 
  IF cql_facet_find(lua_upgrade_facets, 'test_recreate_table_B_table_crc') != 3376021155436702751 THEN
    DROP TABLE IF EXISTS test_recreate_table_B;
    CREATE TABLE test_recreate_table_B(
      colA INTEGER,
      colB LONG_INT,
      colC INTEGER
    );
    CALL lua_upgrade_cql_set_facet_version('test_recreate_table_B_table_crc', 3376021155436702751);
  END IF;
  IF cql_facet_find(lua_upgrade_facets, 'test_recreate_table_A_table_crc') != 2282658103124508505 THEN
    DROP TABLE IF EXISTS test_recreate_table_A;
    CALL lua_upgrade_cql_set_facet_version('test_recreate_table_A_table_crc', 2282658103124508505);
  END IF;
  IF cql_facet_find(lua_upgrade_facets, 'a_recreate_group_group_crc') != 292428789434790812 THEN
    DROP TABLE IF EXISTS recreate_test_for_unsub;
    CREATE TABLE recreate_test_for_unsub(
      unsub_id INTEGER,
      x TEXT
    );
    CREATE INDEX recreate_test_for_unsub_index ON recreate_test_for_unsub (x);
    CALL lua_upgrade_cql_set_facet_version('a_recreate_group_group_crc', 292428789434790812);
  END IF;
  IF cql_facet_find(lua_upgrade_facets, 'gr1_group_crc') != 4170388871155161988 THEN
    DROP TABLE IF EXISTS use_g1;
    DROP TABLE IF EXISTS g1;
    CREATE TABLE g1(
      id INTEGER PRIMARY KEY,
      name TEXT
    );
    CREATE INDEX staying_index ON g1 (id);
    CREATE TABLE use_g1(
      id INTEGER PRIMARY KEY REFERENCES g1 (id),
      name2 TEXT
    );
    CALL lua_upgrade_cql_set_facet_version('gr1_group_crc', 4170388871155161988);
  END IF;
  CALL lua_upgrade_cql_set_facet_version('all_nonvirtual_tables_crc', 3646222684755303712);
END;

-- recreate all the virtual @recreate tables that might have changed
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_cql_recreate_virtual_tables()
BEGIN
  IF cql_facet_find(lua_upgrade_facets, 'all_virtual_tables_crc') == -5234476629932279302 RETURN; 
  IF cql_facet_find(lua_upgrade_facets, 'test_virtual_table_A_table_crc') != -3815188615208421166 THEN
    DROP TABLE IF EXISTS test_virtual_table_A;
    CREATE VIRTUAL TABLE test_virtual_table_A USING test_module AS (
      name TEXT
    );
    CALL lua_upgrade_cql_set_facet_version('test_virtual_table_A_table_crc', -3815188615208421166);
  END IF;
  CALL lua_upgrade_cql_set_facet_version('all_virtual_tables_crc', -5234476629932279302);
END;

@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_setup_facets()
BEGIN
  BEGIN TRY
    SET lua_upgrade_facets := cql_facets_create();
    DECLARE C CURSOR FOR SELECT * from lua_upgrade_cql_schema_facets;
    LOOP FETCH C
    BEGIN
      LET added := cql_facet_add(lua_upgrade_facets, C.facet, C.version);
    END;
  END TRY;
  BEGIN CATCH
   -- if table doesn't exist we just have empty facets, that's ok
  END CATCH;
END;

DECLARE FUNCTION _cql_contains_column_def(needle TEXT, haystack TEXT) BOOL NOT NULL;
@attribute(cql:private)
CREATE PROC lua_upgrade_column_exists(table_ TEXT NOT NULL, col_info TEXT NOT NULL, OUT exists_ BOOL NOT NULL)
BEGIN
  IF lua_upgrade_tables_dict_ IS NULL THROW;
  LET table_str := cql_string_dictionary_find(lua_upgrade_tables_dict_, table_);
  SET exists_ := _cql_contains_column_def(table_str, col_info);
END;

@attribute(cql:private)
CREATE PROC lua_upgrade_cql_drop_tables()
BEGIN
  DROP TABLE IF EXISTS test_create_table_A;
  DROP TABLE IF EXISTS test_this_table_will_become_create;
  DROP TABLE IF EXISTS test_create_table_B;
END;

@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_perform_upgrade_steps(include_virtual_tables BOOL NOT NULL)
BEGIN
  IF cql_facet_find(lua_upgrade_facets, 'cql_schema_crc_no_virtual') <> 2400283448839381909 THEN
    DECLARE schema_version LONG INTEGER NOT NULL;
    -- dropping all views --
    CALL lua_upgrade_cql_drop_all_views();

    -- dropping condemned or changing indices --
    CALL lua_upgrade_cql_drop_all_indices();

    -- dropping condemned or changing triggers --
    CALL lua_upgrade_cql_drop_all_triggers();

    ---- install baseline schema if needed ----

    CALL lua_upgrade_cql_get_version_crc(0, schema_version);
    IF schema_version != 129889370206825069 THEN
      CALL lua_upgrade_cql_install_baseline_schema();
      CALL lua_upgrade_cql_set_version_crc(0, 129889370206825069);
    END IF;

    CALL lua_upgrade_get_table_defs();

    ---- upgrade to schema version 1 ----

    CALL lua_upgrade_cql_get_version_crc(1, schema_version);
    IF schema_version != -2931676696904352514 THEN
      -- unsubscription of test_for_unsub

      DROP TABLE IF EXISTS test_for_unsub;
      CALL lua_upgrade_cql_set_facet_version('test_for_unsub_index_index_crc', -1);

      -- creating table test_create_table_B

      CREATE TABLE IF NOT EXISTS test_create_table_B(
        colA TEXT,
        colB LONG_INT,
        colC INTEGER
      );

      -- one time drop test_this_table_will_become_create

      CALL lua_upgrade_cql_one_time_drop('test_this_table_will_become_create', 1);

      -- creating table test_this_table_will_become_create

      CREATE TABLE IF NOT EXISTS test_this_table_will_become_create(
        id INTEGER PRIMARY KEY
      );

      -- altering table test_create_table_A to add column colD TEXT;

      IF NOT lua_upgrade_column_exists('test_create_table_A', 'colD TEXT') THEN 
        ALTER TABLE test_create_table_A ADD COLUMN colD TEXT;
      END IF;

      CALL lua_upgrade_cql_set_version_crc(1, -2931676696904352514);
    END IF;

    ---- upgrade to schema version 2 ----

    CALL lua_upgrade_cql_get_version_crc(2, schema_version);
    IF schema_version != 6808174517738439042 THEN
      -- creating table test_create_table_C

      CREATE TABLE IF NOT EXISTS test_create_table_C(
        colA TEXT,
        colB LONG_INT,
        colC LONG_INT
      );

      -- resubscribe to test_for_unsub

      CREATE TABLE IF NOT EXISTS test_for_unsub(
        unsub_id INTEGER,
        x TEXT
      );

      CALL lua_upgrade_cql_set_version_crc(2, 6808174517738439042);
    END IF;

    ---- upgrade to schema version 3 ----

    CALL lua_upgrade_cql_get_version_crc(3, schema_version);
    IF schema_version != -3054984079713597464 THEN
      -- unsubscription of test_for_unsub

      DROP TABLE IF EXISTS test_for_unsub;
      CALL lua_upgrade_cql_set_facet_version('test_for_unsub_index_index_crc', -1);

      CALL lua_upgrade_cql_set_version_crc(3, -3054984079713597464);
    END IF;

    ---- upgrade to schema version 4 ----

    CALL lua_upgrade_cql_get_version_crc(4, schema_version);
    IF schema_version != 4525541148634024559 THEN
      -- resubscribe to test_for_unsub

      CREATE TABLE IF NOT EXISTS test_for_unsub(
        unsub_id INTEGER,
        x TEXT
      );

      CALL lua_upgrade_cql_set_version_crc(4, 4525541148634024559);
    END IF;

    CALL lua_upgrade_cql_drop_tables();
    CALL lua_upgrade_cql_recreate_non_virtual_tables();
    CALL lua_upgrade_cql_create_all_views();
    CALL lua_upgrade_cql_create_all_indices();
    CALL lua_upgrade_cql_create_all_triggers();

    CALL lua_upgrade_cql_set_facet_version('cql_schema_version', 4);
    CALL lua_upgrade_cql_set_facet_version('cql_schema_crc_no_virtual', 2400283448839381909);
  END IF;
  IF include_virtual_tables THEN
    CALL lua_upgrade_cql_recreate_virtual_tables();
    CALL lua_upgrade_cql_set_facet_version('cql_schema_crc', -7461702544329564919);
  END IF;
END;

CREATE PROCEDURE lua_upgrade_get_current_and_proposed_versions(
    out current long not null,
    out proposed long not null
    )
BEGIN
    SET current := lua_upgrade_cql_get_facet_version('cql_schema_version');
    SET proposed := 4;
END;
@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_perform_needed_upgrades(include_virtual_tables BOOL NOT NULL)
BEGIN
  -- check for downgrade --
  IF cql_facet_find(lua_upgrade_facets, 'cql_schema_version') > 4 THEN
    SELECT 'downgrade detected' facet;
  ELSE
    -- save the current facets so we can diff them later --
    CALL lua_upgrade_save_cql_schema_facets();
    CALL lua_upgrade_perform_upgrade_steps(include_virtual_tables);

    -- finally produce the list of differences
    SELECT T1.facet FROM
      lua_upgrade_cql_schema_facets T1
      LEFT OUTER JOIN lua_upgrade_cql_schema_facets_saved T2
        ON T1.facet = T2.facet
      WHERE T1.version is not T2.version;
  END IF;
END;

@attribute(cql:private)
CREATE PROCEDURE lua_upgrade_helper(include_virtual_tables BOOL NOT NULL)
BEGIN
  DECLARE schema_crc LONG INTEGER NOT NULL;

  -- create schema facets information table --
  CALL lua_upgrade_create_cql_schema_facets_if_needed();

  -- fetch the last known schema crc, if it's different do the upgrade --
  CALL lua_upgrade_cql_get_facet_version('cql_schema_crc', schema_crc);

  IF schema_crc <> -7461702544329564919 THEN
    BEGIN TRY
      CALL lua_upgrade_setup_facets();
      CALL lua_upgrade_perform_needed_upgrades(include_virtual_tables);
    END TRY;
    BEGIN CATCH
      SET lua_upgrade_facets := NULL;
      SET lua_upgrade_tables_dict_ := NULL;
      THROW;
    END CATCH;
    SET lua_upgrade_facets := NULL;
    SET lua_upgrade_tables_dict_ := NULL;
  ELSE
    -- some canonical result for no differences --
    SELECT 'no differences' facet;
  END IF;
END;

CREATE PROCEDURE lua_upgrade()
BEGIN
  CALL lua_upgrade_helper(TRUE);
END;

CREATE PROCEDURE lua_upgrade_no_virtual_tables()
BEGIN
  CALL lua_upgrade_helper(FALSE);
END;

declare proc print no check;

-- Note that the same validator runs against every version, the validator knows what
-- to expect at each version and does different things.  This makes it a lot easier
-- to build each upgrader with built-in validation.  There's one central place (here)
-- where all validation happens.
create proc print_schema()
begin
  let version := cast(lua_upgrade_cql_get_facet_version("cql_schema_version") as integer);

  call print(printf("reference results for version %d\n\n", version));

  declare C cursor for select * from sqlite_master order by name;
  loop fetch C
  begin
    call print(printf("----- %s -----\n\n", C.name));
    call print(printf("type: %s\n", C.type));
    call print(printf("tbl_name: %s\n", C.tbl_name));

    -- Canonicalize and put in the split markers so we get some useful line breaks consistently
    -- Different SQLite versions will either preserve whitespace or not so this is an effort to
    -- normalize.  It's not perfect but it doesn't need to be, it only needs to work for
    -- schema the test will ever see.

    let s := (select replace(C.sql, "\n", " "));
    set s := (select replace(s, " ,", ","));
    set s := (select replace(s, " )", ")"));
    set s := (select replace(s, "( ", "("));
    set s := (select replace(s, "  ", " "));
    set s := (select replace(s, ",", ",$"));
    set s := (select replace(s, "(", "($"));

    -- split the string at the $ marks
    declare lines cursor for
      with split(line, str) as (
          select '', s || '$'
        union all
          select substr(str, 1, instr(str, '$') - 1), substr(str, instr(str, '$') + 1)
        from split
        where str != '')
      select line from (select trim(line) line from split) where line != '';

    -- some standard indenting, very simple
    let indent := 0;
    loop fetch lines
    begin
      let i := 0;
      let indent_str := "";
      while i < indent
      begin
        set indent_str := printf("%s%s", indent_str, "  "); -- ugh
        set i := i + 1;
      end;
      call print(printf("%s%s\n", indent_str, lines.line));

      -- trailing ( starts indent
      -- trailing ) ends indent
      let tail := (select substr(lines.line, length(lines.line)));
      if tail == '(' then
        set indent := indent + 1;
      else if tail == ')' then
        set indent := indent - 1;
      end if;

      -- trailing ), ends indent
      set tail := (select substr(lines.line, length(lines.line)-1));
      if tail == '),' then
        set indent := indent - 1;
      end if;
    end;

    call print("");
  end;
end;


create procedure go()
begin
  call lua_upgrade_no_virtual_tables();
  call print_schema();
end;

@echo lua, "go(sqlite3.open_memory())\n";

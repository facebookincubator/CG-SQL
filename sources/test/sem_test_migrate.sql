/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- this script pretends that it is upgrading to version 5 from version 4.
-- pick a number we can easily go over or under for validation
@schema_upgrade_version (5);

-- TEST: mondo table with assorted thingies that should be deleted
-- See below for the fields and why
-- + {create_table_stmt}: foo: { id: integer notnull, rate: longint, id2: integer, name: text }
-- - Error
create table foo(
  id integer not null,               -- not deleted
  rate long int @delete(5, deletor), -- not deleted
  rate_2 long int @delete(4),        -- deleted (deleted previously)
  id2 integer @create(4),            -- not deleted
  name text @create(5),              -- not deleted
  name_2 text @create(6)             -- deleted (appears later)
);

-- TEST: try to create an index on a column that's been not created yet
-- + {create_index_stmt}: ok
-- - Error
create index recent_index on foo(name_2);

-- TEST: try to create a table with an FK on a column that doesn't exist yet
-- this must be allowed to succeed as the table decl is ok
-- + {create_table_stmt}: fk_not_there_yet: { id: integer, text1: text foreign_key, text2: text }
-- + {fk_def}: ok
-- + {col_attrs_fk}: ok
-- - Error
create table fk_not_there_yet(
 id integer,
 text1 text references foo(name_2),
 text2 text,
 foreign key (text2) references foo(name_2)
);

-- TEST: try to read a missing field
-- + error: % name not found 'rate_2'
-- +1 error:
select rate_2 from foo;

-- TEST: try to declare 'deletor' but it doesn't do anything
-- + {create_proc_stmt}: err
-- + error: % procedure is supposed to do schema migration but it doesn't have any DML 'deletor'
-- +1 error:
create proc deletor()
begin
 declare i integer;
end;

-- set up test table
create table bar(
  id INTEGER NOT NULL,
  name TEXT @create(5, name_creator)
);

-- TEST: try to make an upgrade proc with args
-- + {create_proc_stmt}: err
-- + error: % procedure previously declared as schema upgrade proc, it can have no args 'name_creator'
-- +1 error:
create proc name_creator(i integer)
begin
  select 1 x;
end;

-- TEST: create a table that is not yet visible in this schema version
-- + {create_table_stmt}: t1: { id: integer } deleted
-- - Error
create table t1(
 id integer
) @create(7);

-- TEST: create a table that is visible in this schema version
-- + {create_table_stmt}: t2: { id: integer }
-- - deleted
-- - Error
create table t2(
 id integer
) @create(5);

-- TEST: create a table that is deleted in this schema version
-- + {create_table_stmt}: t3: { id: integer } deleted
-- - Error
create table t3(
 id integer
) @delete(4);

-- TEST: create a table that is not yet deleted in this schema version
-- + {create_table_stmt}: t4: { id: integer }
-- - deleted
-- - Error
create table t4(
 id integer
) @delete(5);

-- TEST: t1 is not visible in this schema version (v5)
-- + {delete_stmt}: err
-- + error: % table in delete statement does not exist (not visible in schema version 5) 't1'
-- +1 error:
delete from t1;

-- TEST: t2 is visible in this schema version (v5)
-- + {delete_stmt}: ok
-- + {name t2}: t2: { id: integer }
-- - Error
delete from t2;

-- TEST: t3 is not visible in this schema version (v5)
-- + {delete_stmt}: err
-- + error: % table in delete statement does not exist (not visible in schema version 5) 't3'
-- +1 error:
delete from t3;

-- TEST: t4 is visible in this schema version (v5)
-- + {delete_stmt}: ok
-- + {name t4}: t4: { id: integer }
-- - Error
delete from t4;

-- TEST: create a trigger, triggers are ignored in this context because they might refer to things
-- that do not exist in this schema version.  So trigger declarations could be bogus.  That's ok
-- because they are validated in the normal contexts.  So even though this trigger is full of
-- bogus references no errors are generated
-- + {create_trigger_stmt}: ok
-- - Error
create trigger will_not_be_processed_trigger
   after insert on lol_table
begin
  delete from rofl where id = 1;
end;

-- TEST: declare an ad hoc migrator
-- + {schema_ad_hoc_migration_stmt}: ok @create(5)
-- - Error
@schema_ad_hoc_migration(5, MyAdHocMigration);

-- TEST: createe the ad hoc migrator, this forces validation of the adhoc type
-- + {create_proc_stmt}: ok dml_proc
-- + {name MyAdHocMigration}: ok dml_proc
-- - Error
create proc MyAdHocMigration()
begin
  update foo set id = 1 where id = 2;
end;

-- TEST: try to switch to previous schema validation mode in a migrate script
-- + error: % switching to previous schema validation mode not allowed if @schema_upgrade_version was used
-- +1 error:
@previous_schema;

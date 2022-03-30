/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@declare_deployable_region base;
@declare_deployable_region different_region;

@begin_schema_region base;

-- TEST: basic table to verify
-- See below for the fields and why
-- + {create_table_stmt}: foo: { id: integer notnull, id2: integer, name: text, name_2: text } validated
-- - error:
create table foo(
  id integer not null,
  rate long int @delete(5, deletor),
  rate_2 long int @delete(4),
  id2 integer @create(4),
  name text @create(5),
  name_2 text @create(6)
);

-- This table legitimately loses a column
create table column_deleted_in_this_table(
  id integer,
  being_deleted text @delete(6)
);

-- This table tries to undelete a column
create table column_undeleted_in_this_table(
  id integer,
  being_undeleted text
);

-- TEST: basic table to verify a non-sensitive column can become sensitive column
-- See below for the fields and why
-- + {create_table_stmt}: become_sensitive: { id2: integer sensitive, name: text sensitive } validated
-- - error:
create table become_sensitive(
  id2 integer @sensitive,
  name text @sensitive
);

-- create version will change (error)
create table t_create_verison_changed(id integer) @create(1);

-- delete version will change (error)
create table t_delete_verison_changed(id integer) @delete(1);

-- t_not_present_in_new_schema is gone

-- t_not_present_in_new_schema was removed rather than marking it for delete

-- table is now a view (bogus)
create view t_became_a_view as select 1 id;

-- table was in base schema, now created (bogus)
create table t_created_in_wrong_version(id integer) @create(1);

-- table was in base schema, now deleted (ok)
create table t_was_correctly_deleted(id integer) @delete(1);

-- column name changed
create table t_column_name_changed(id_ integer);

-- column type changed
create table t_column_type_changed(id real);

-- column attribute changed
create table t_column_attribute_changed(id integer not null);

-- column version changed for delete
create table t_column_delete_version_changed(id integer, id2 integer @delete(1));

-- column version changed for create
create table t_column_create_version_changed(id integer, id2 integer @create(1));

-- column default value changed
create table t_column_default_value_changed(id integer, id2 integer not null default 2);

-- column default value did not change
create table t_column_default_value_ok(id integer, id2 integer not null default 1);

-- create table with additional attribute (matches)
create table t_additional_attribute_present(a int not null, b int, primary key (a,b));

-- create table with additional attribute (doesn't match)
create table t_additional_attribute_mismatch(a int not null, b int not null, primary key (a));

-- columns removed rather than annotated with @delete
create table t_columns_removed(id integer);

-- create table with added facet not present in the previous
create table t_attribute_added(a int not null, primary key (a));

-- create table with additional column and no @create
create table t_additional_column(a int not null, b int);

-- create table with additional column and @create (ok)
-- + {create_table_stmt}: t_additional_column_ok: { a: integer notnull, b: integer, c: integer } validated
-- - error:
create table t_additional_column_ok(a int not null, b int @create(2), c int @create(6));

-- create table with different flags (like TEMP)
create TEMP table t_becomes_temp_table(a int not null, b int);

-- TEST: create table and apply annotation
-- + {create_table_stmt}: t_new_table_ok: { a: integer notnull, b: integer } @create(26)
-- Not validated against previous schema since there was no previous schema
-- - validated
-- - error:
create table t_new_table_ok(a int not null, b int) @create(26);

-- TEST: create new table without annotation (error)
-- + {create_table_stmt}: err
-- + error: % new table must be added with @create(26) or later 't_new_table_no_annotation'
-- +1 error:
create table t_new_table_no_annotation(a int not null, b int);

-- TEST: create new table stale annotation (error)
-- + {create_table_stmt}: err
-- + error: % new table must be added with @create(26) or later 't_new_table_stale_annotation'
-- +1 error:
create table t_new_table_stale_annotation(a int not null, b int) @create(2);

-- TEST: add columns to table, marked @create and @delete
-- The node is marked in error because previous validation will fail but the error message
-- is charged to the previous schema node where it is found.
-- + {create_table_stmt}: err
-- - error:
create table t_new_table_create_and_delete(a int not null, b int @create(6) @delete(7));

-- TEST: add columns to table, marked @create correctly
-- + {create_table_stmt}: t_new_legit_column: { a: integer notnull, b: integer } validated
-- - error:
create table t_new_legit_column(a int not null, b int @create(6));

-- create table with a create migration proc
create table with_create_migrator(id integer) @create(1, ACreateMigrator);

-- create table with a delete migration proc
create table with_delete_migrator(id integer) @delete(1, ADeleteMigrator);

-- create a table which was a view in the previous schema
-- No error is charged to this node, the error is reported in the previous validation
-- however the node is marked in error so that additional errors are not reported
-- In particular we don't give version number errors because the table looks new
-- and possibly not now in the right version
-- + {create_table_stmt}: err
-- - error:
create table view_becomes_a_table(id int);

-- create a new version of this view that is not temp
create view view_was_temp_but_now_it_is_not as select 1 X;

-- TEST: create a view with no annotation that is not in the previous schema
-- + {create_view_stmt}: view_created_with_no_annotation: { X: integer notnull }
-- - error:
create view view_created_with_no_annotation as select 1 X;

-- TEST: create an index that is new, no @create is needed
-- + create_index_stmt}: ok
-- - error:
create index this_index_was_created_with_no_annotation on foo(id);

-- create a table with a column def that has a different create migrator
create table create_column_migrate_test(
  id int unique,
  id2 int @create(2, ChangedColumnCreateMigrator)
);

-- create a table with a column def that has a different delete migrator
create table delete_column_migrate_test(
  id int,
  id2 int @delete(2, ChangedColumnDeleteMigrator)
);

-- create a table with an interesting complex facet
create table fk_facet
(
  id int,
  foreign key (id) references create_column_migrate_test(id) on delete cascade
);

-- try to change a table to the recreate plan after it was on the other plan
-- - error:
create table cannot_change_to_recreate
(
  id int
) @recreate;

-- the new version of this table is on the delete plan, that's ok to go
-- - error:
create table ok_to_delete_recreate_table
(
  id int
) @recreate @delete;

-- the new version of this table is on the create plan, that's ok to go
-- - error:
create table ok_to_create_recreate_table
(
  id int
) @create(26, cql:from_recreate);

-- the new version of this table is on the create plan, but attribute missing -> error
-- - error:
create table not_ok_to_create_recreate_table
(
  id int
) @create(6);

-- TEST : the new version of this table is ok the delete plan, the version number can be low, it's ok for deleted
-- + {create_table_stmt}: recreate_deleted_in_the_past: { id: integer } deleted validated @delete(1) @recreate
-- - error:
create table recreate_deleted_in_the_past
(
  id int
) @recreate @delete;

-- TEST : the new version of this table is ok on the create plan but the version number is too small
-- + {create_table_stmt}: err
-- + error: % Table must leave @recreate management with @create(26) or later 'recreate_created_in_the_past'
-- +1 error:
create table recreate_created_in_the_past
(
  id int
) @create(2, cql:from_recreate);

-- mega changes to the table, it's recreate so whatever
create table recreate_feel_the_power
(
  id int,
  payload real,
  whatever text
) @recreate;

-- create a new table directly on the recreate plan, totally ok
-- - new table must be added with @create
-- - error:
-- + create_table_stmt}: direct_to_recreate: { id: integer } @recreate
create table direct_to_recreate
(
  id int
) @recreate;

-- TEST: adding a trigger, totally ok to do so
-- + {create_trigger_stmt}: ok
-- - error:
create trigger trigger_added_no_problemo
  before delete on foo
begin
  select old.id;
end;

-- TEST: trigger deleted correctly
-- + {create_trigger_stmt}: ok
-- + END @DELETE(2);
-- - error:
create trigger trigger_will_be_deleted
  before delete on foo
begin
  select old.id;
end @delete(2);

create table t_removed_facet(
  id integer not null
);

create table t_subtle_column_change(
  id integer references create_column_migrate_test(id)
);

-- TEST: columns added interleaved
-- - error:
create table t_several_columns_added_interleaved(
  col1 integer,
  col2 integer @create(2),
  col3 integer @create(2),
  primary key (col1)
);

-- this will cause a failure, it's changed from version 1
-- + {schema_ad_hoc_migration_stmt}: err
@schema_ad_hoc_migration(2, WhoopsItChanged);

-- TEST: no problems here
-- - error:
@schema_ad_hoc_migration(3, MigrateGoodToGo);

-- TEST: we're trying to create an ad hoc rule in the past... not allowed
-- this item is not present in the previous schema
-- + {schema_ad_hoc_migration_stmt}: err
-- + error: % new ad hoc rule must be added at version 26 or later 'MigrateInThePast'
-- +1 error:
@schema_ad_hoc_migration(3, MigrateInThePast);

-- TEST: create a new ad hoc rule in the present
-- this item is not present in the previous schema, but it's ok because newer than any of the old items
-- + {schema_ad_hoc_migration_stmt}: ok @create(26)
-- + {version_annotation}
-- + {int 26}
-- + {name MigrateNewCurrent}
@schema_ad_hoc_migration(26, MigrateNewCurrent);

-- These two tables are changing from recreate group foo to create group bar
-- this now generates errors as motion cannot be allowed due to possible FK
-- issues.  The issue is that the version of the table in the DB could have
-- different FK references than the current version of the table.  The
-- delete order when the table moves is not clear.  See the docs for CQL0449
-- for more details.

-- TEST: simple recreate in a new group, no issues until previous schema validation
-- - error:
create table Recreated1 ( id integer primary key) @recreate(bar);

-- TEST: simple recreate in a new group, with FK, no issues until previous schema validation
-- - error:
create table Recreated2 ( id integer references Recreated1(id) ) @recreate(bar);

-- TEST: creating a table that will move to a different deployment region
-- + {create_table_stmt}: err
-- + error: % object's deployment region changed from 'different_region' to 'base' 'TChanging'
-- +1 error:
create table TChanging(id integer);

-- TEST: creating an index that will move to a different deployment region
-- + {create_index_stmt}: err
-- + error: % object's deployment region changed from 'different_region' to 'base' 'IChanging'
-- +1 error:
create index IChanging on TChanging(id);

-- TEST: creating a view that will move to a different deployment region
-- + {create_view_stmt}: err
-- + error: % object's deployment region changed from 'different_region' to 'base' 'VChanging'
-- +1 error:
create view VChanging as select * from TChanging;

-- TEST: creating a trigger that will move to a different deployment region
-- + {create_trigger_stmt}: err
-- + error: % object's deployment region changed from 'different_region' to 'base' 'TrigChanging'
-- +1 error:
create trigger TrigChanging
  before delete on foo
begin
  select old.id;
end;

-- TEST: creating an ad hoc migration that will move to a different deployment region
-- + {schema_ad_hoc_migration_stmt}: err
-- + error: % object's deployment region changed from 'different_region' to 'base' 'AdHocChanging'
-- +1 error:
@schema_ad_hoc_migration(2, AdHocChanging);

@end_schema_region;

@declare_schema_region SomeLeaf;

@begin_schema_region SomeLeaf;

-- TEST: even though this table's deployment region was not known when it was declared we
-- still figured it out later because the errors are deferred
-- + {create_table_stmt}: err
-- + error: % object's deployment region changed from 'DeployableRegion1' to 'DeployableRegion2' 'TableWithDeferredOwner'
-- +1 error:
create table TableWithDeferredOwner(id integer);
@end_schema_region;

-- TEST: find errors due to a conflicting deployable region  appearing later in the file
-- Nothing wrong with this line, it causes errors elsewhere
-- - error:
@declare_deployable_region DeployableRegion2 using SomeLeaf;

@declare_schema_region simple_region_1;

@begin_schema_region simple_region_1;

-- TEST : these logical moves are legal and innocuous
-- - error:
create table logical_moving_T1(id integer primary key);

-- TEST : these logical moves are legal and innocuous
-- - error:
create table logical_moving_T2(id integer references logical_moving_T1(id), name text);

@end_schema_region;

@declare_schema_region high_numbered_thing;

@begin_schema_region high_numbered_thing;

-- TEST: this object doesn't change but it would force a huge version number for all new things
-- however it will be excluded so it won't count towards the total.
-- - error:
create table high_numbered_thing( id integer) @create(9999);

@end_schema_region;

create table table_staying(
  col1 int primary key not null
) @recreate(my_recreate_group);

-- This table references a different recreate group but it's being deleted so that's ok
-- you can exit the recreate group when you are deleted, your existing foreign keys don't matter
-- - error:
create table table_going(
  col1 text,
  col2 int,
  foreign key(col2) references table_staying(col1) on update cascade on delete cascade
) @recreate(my_recreate_group) @delete;

-- TEST: it's ok for items to appear with a migration
-- create validated in normal processing, delete validated in previous
-- - error:
create table adding_with_migrators_ok(
  id integer primary key,
  id2 integer @delete(3, delete_me),
  id3 integer @create(3, create_me)
) @create(2);

declare enum foo_enum integer (
 a = 1,
 b = 2
);

create table foo_with_check(
 x integer check (x == foo_enum.a)
);

-- TEST: no change case
-- - error:
create virtual table unchanged_virtual using my_virtual(goo) as (
  id integer
);

-- TEST: legal delete case
-- - error:
create virtual table deleted_virtual using my_virtual(goo) as (
  id integer
) @delete(3, cql:module_must_not_be_deleted_see_docs_for_CQL0392);

-- TEST: zombie comes back to life (invalid)
create virtual table undead_virtual using my_virtual(goo) as (
  id integer
);

-- TEST it's ok at add things to a virtual table and change args
-- - error:
create virtual table changing_virtual using my_virtual(goo, goo) as (
  id integer,
  t text
);

-- TEST: changing the delete version is bad
-- - error:
create virtual table delete_change_virtual using my_virtual(goo) as (
  id integer
) @delete(1, cql:module_must_not_be_deleted_see_docs_for_CQL0392);


-- TEST: test create table with not null column on conflict clause abort
-- - error:
create table conflict_clause_t(id int not null on conflict fail);

-- TEST: test create table with pk column on conflict clause rollback
-- - error:
create table conflict_clause_pk(
  id int not null,
  constraint pk1 primary key (id) on conflict rollback
);

-- TEST: attempt to transition with a normal delete
-- this table should stay on the recreate plan
-- error will be detected during previous validation
-- so far so good
-- - error:
create table dropping_this
(
  f1 integer,
  f2 text
) @delete(5);

-- TEST: this table had a group but gets one
-- error will be detected during previous validation
-- - error:
create table losing_group
(
  id integer
) @recreate;

-- TEST: this table gets a group, that's valid
-- - error:
create table gaining_group
(
  id integer
) @recreate(new_group);

-- TEST table ok
create table unsub_resub_trickery(id integer);

-- TEST unsub valid
-- - error:
@unsub (20, unsub_resub_trickery);

-- TEST resub valid
-- - error:
@resub (22, unsub_resub_trickery);

-- TEST unsub valid
-- - error:
@unsub (24, unsub_resub_trickery);

-- TEST resub valid
-- - error:
@resub (26, unsub_resub_trickery);

------------------------------------------------------------------------------------------------------------
@previous_schema;
------------------------------------------------------------------------------------------------------------

@declare_deployable_region base;
@declare_deployable_region different_region;

@begin_schema_region base;

-- TEST: valid previous schema
-- - error:
create table foo(
  id integer not null,
  rate long int @delete(5, deletor),
  rate_2 long int @delete(4),
  id2 integer @create(4),
  name text @create(5),
  name_2 text @create(6)
);

-- TEST: legit column delete, totally ok
-- - error:
create table column_deleted_in_this_table(
  id integer,
  being_deleted text
);

-- TEST: tries a bogus "undelete" operation
-- + error: % column current delete version not equal to previous delete version 'being_undeleted'
create table column_undeleted_in_this_table(
  id integer,
  being_undeleted text @delete(6)
);

-- TEST: valid previous schema with non-sensitive columns
-- - error:
create table become_sensitive(
  id2 integer,
  name text
);

-- TEST: previous schema created table at v1
-- + {create_table_stmt}: err
-- + error: % current create version not equal to previous create version for 't_create_verison_changed'
-- +1 error:
create table t_create_verison_changed(id integer) @create(2);

-- TEST: previous schema deleted table at v1
-- + {create_table_stmt}: err
-- + error: % current delete version not equal to previous delete version for 't_delete_verison_changed'
-- +1 error:
create table t_delete_verison_changed(id integer) @delete(2);

-- TEST: previous schema deleted table at v1
-- + {create_table_stmt}: err
-- + error: % table was present but now it does not exist (use @delete instead) 't_not_present_in_new_schema'
-- +1 error:
create table t_not_present_in_new_schema(id integer);

-- TEST: previous table is now a view
-- + {create_table_stmt}: err
-- + error: % object was a table but is now a view 't_became_a_view'
-- +1 error:
create table t_became_a_view(id integer);

-- TEST: previous schema created table at v1
-- + {create_table_stmt}: err
-- + error: % current create version not equal to previous create version for 't_created_in_wrong_version'
-- +1 error:
create table t_created_in_wrong_version(id integer);

-- TEST: previous schema had the table in base, it's now deleted
-- + {create_table_stmt}: t_was_correctly_deleted: { id: integer }
-- - error:
create table t_was_correctly_deleted(id integer);

-- TEST: column name changed between schema
-- + {create_table_stmt}: err
-- + error: % column name is different between previous and current schema 'id_'
-- +1 error:
create table t_column_name_changed(id integer);

-- TEST: column type changed between schema
-- + {create_table_stmt}: err
-- + error: % column type is different between previous and current schema 'id'
-- +1 error:
create table t_column_type_changed(id integer);

-- TEST: column attribute changed between schema (same error)
-- + {create_table_stmt}: err
-- + error: % column type is different between previous and current schema 'id'
-- +1 error:
create table t_column_attribute_changed(id integer);

-- TEST: column delete version number changed
-- + {create_table_stmt}: err
-- + error: % column current delete version not equal to previous delete version 'id2'
-- +1 error:
create table t_column_delete_version_changed(id integer, id2 integer @delete(2));

-- TEST: column create version number changed
-- + {create_table_stmt}: err
-- + error: % column current create version not equal to previous create version 'id2'
-- +1 error:
create table t_column_create_version_changed(id integer, id2 integer @create(2));

-- TEST: column default value changed
-- + {create_table_stmt}: err
-- + error: % column current default value not equal to previous default value 'id2'
create table t_column_default_value_changed(id integer, id2 integer not null default 1);

-- TEST: column default value did not change
-- + {create_table_stmt}: t_column_default_value_ok: { id: integer, id2: integer notnull has_default }
-- The previous schema isn't marked validated only the original schema is
-- - validated
-- - error:
create table t_column_default_value_ok(id integer, id2 integer not null default 1);

-- TEST: create table with additional attribute
-- + {create_table_stmt}: t_additional_attribute_present: { a: integer notnull, b: integer notnull }
-- The previous schema isn't marked validated only the original schema is
-- - validated
-- - error:
create table t_additional_attribute_present(a int not null, b int, primary key (a,b));

-- TEST: create table with additional attribute (doesn't match)
-- + {create_table_stmt}: err
-- + error: in pk_def % table has a facet that is different in the previous and current schema 't_additional_attribute_mismatch'
create table t_additional_attribute_mismatch(a int not null, b int not null, primary key (a,b));

-- TEST: columns were removed
-- + {create_table_stmt}: err
-- + error: % a column was removed from the table rather than marked with @delete 'id2'
-- +1 error:
create table t_columns_removed(id integer, id2 integer);

-- TEST: new table has added facet not present in the previous
-- + {create_table_stmt}: err
-- + error: % table has a new non-column facet in the current schema 't_attribute_added
-- +1 error:
create table t_attribute_added(a int not null);

-- TEST: new table with additional column and no @create
-- + {create_table_stmt}: err
-- + error: % table has columns added without marking them @create 't_additional_column'
-- +1 error:
create table t_additional_column(a int not null);

-- TEST: new table with additional column and @create correct
-- + {create_table_stmt}: t_additional_column_ok: { a: integer notnull, b: integer }
-- The previous schema isn't marked validated only the original schema is marked.
-- - validated
-- - error:
create table t_additional_column_ok(a int not null, b int @create(2));

-- TEST: new table changes a flag like TEMP
-- + {create_table_stmt}: err
-- + error: % table create statement attributes different than previous version 't_becomes_temp_table'
-- +1 error:
create table t_becomes_temp_table(a int not null, b int);

-- TEST: table added a column with @delete and @create
-- + {create_table_stmt}: err
-- + error: % table has newly added columns that are marked both @create and @delete 't_new_table_create_and_delete'
-- +1 error:
create table t_new_table_create_and_delete(a int not null);

-- TEST: add columns to table, marked @create correctly
-- + {create_table_stmt}: t_new_legit_column: { a: integer notnull }
-- The previous schema isn't marked validated only the original schema is
-- - validated
-- - error:
create table t_new_legit_column(a int not null);

-- TEST: create table but previous version had no create migration proc
-- + error: % @create procedure changed in object 'with_create_migrator'
-- + {create_table_stmt}: err
-- + @CREATE(1);
-- +1 error:
create table with_create_migrator(id integer) @create(1);

-- TEST: create table but previous version had different create migration proc
-- + error: % @create procedure changed in object 'with_create_migrator'
-- + {create_table_stmt}: err
-- + @CREATE(1, ADifferentCreateMigrator);
-- +1 error:
create table with_create_migrator(id integer) @create(1, ADifferentCreateMigrator);

-- TEST: delete table but previous version had no delete migration proc
-- + error: % @delete procedure changed in object 'with_delete_migrator'
-- + {create_table_stmt}: err
-- + @DELETE(1);
-- +1 error:
create table with_delete_migrator(id integer) @delete(1);

-- TEST: create table but previous version had different migration proc
-- + error: % @delete procedure changed in object 'with_delete_migrator'
-- + {create_table_stmt}: err
-- + @DELETE(1, ADifferentDeleteMigrator);
-- +1 error:
create table with_delete_migrator(id integer) @delete(1, ADifferentDeleteMigrator);

-- TEST: create a view in the previous schema, it becomes a table in the current (above) schema
-- + {create_view_stmt}: err
-- + error: % object was a view but is now a table 'view_becomes_a_table'
-- +1 error:
create view view_becomes_a_table as select 1 X;

-- TEST: create a view in the previous schema that is absent entirely in the new schema
-- + {create_view_stmt}: err
-- + error: % view was present but now it does not exist (use @delete instead) 'view_was_zomg_deleted'
-- +1 error:
create view view_was_zomg_deleted as select 1 X;

-- TEST: create a temp view in the previous schema, changing the flags is fine, we don't care
-- + {create_view_stmt}: ok
-- - error:
create temp view view_was_temp_but_now_it_is_not as select 1 X;

-- TEST: create an index that is now totally gone in the new schema
-- + error: % index was present but now it does not exist (use @delete instead) 'this_index_was_deleted_with_no_annotation'
-- +1 error:
create index this_index_was_deleted_with_no_annotation on foo(id);

-- TEST: create a table with a column def that has a create different migrator
-- + {create_table_stmt}: err
-- + error: % column @create procedure changed 'id2'
-- +1 error:
create table create_column_migrate_test(
 id int unique,
 id2 int @create(2, PreviousColumnCreateMigrator)
);

-- TEST: create a table with a column def that has a delete different migrator
-- + {create_table_stmt}: err
-- + error: % column @delete procedure changed 'id2'
-- +1 error:
create table delete_column_migrate_test(
 id int,
 id2 int @delete(2, PreviousColumnDeleteMigrator)
);

-- TEST : create a table with an interesting complex facet, different from the above
-- + error: % table has a facet that is different in the previous and current schema 'fk_facet'
create table fk_facet
(
 id int,
 foreign key (id) references create_column_migrate_test(id) on update cascade
);

-- TEST : new version of this table is on the recreate plan, this is not valid
-- + error: % current schema can't go back to @recreate semantics for 'cannot_change_to_recreate'
-- +1 error:
create table cannot_change_to_recreate
(
 id int
) @create(1);

-- TEST: the new version of this table is on the delete plan, that's ok to go
-- + create_table_stmt}: ok_to_delete_recreate_table: { id: integer } @recreate
-- - error:
create table ok_to_delete_recreate_table
(
 id int
) @recreate;

-- TEST: the new version of this table is on the create plan, with cql_from_recreate that's ok to go
-- + {create_table_stmt}: ok_to_create_recreate_table: { id: integer } @recreate
-- - error:
create table ok_to_create_recreate_table
(
 id int
) @recreate;

-- TEST: the new version of this table is on the create plan, but missing cql:from_recreate
-- + {create_table_stmt}: err
-- + error: % table transitioning from @recreate to @create must use @create(nn,cql:from_recreate) 'not_ok_to_create_recreate_table'
-- +1 error:
create table not_ok_to_create_recreate_table
(
 id int
) @recreate;

-- the new version of this table is on the delete plan, but it was deleted in the past
create table recreate_deleted_in_the_past
(
 id int
) @recreate;

-- the new version of this table is on the create plan, but it was created in the past
create table recreate_created_in_the_past
(
 id int
) @recreate;

-- TEST: mega changes to the table, it's recreate so whatever
-- + create_table_stmt}: recreate_feel_the_power: { id: text, payload: text, whatever: integer } @recreate
-- - error:
create table recreate_feel_the_power
(
  id text,
  payload text,
  whatever int
) @recreate;

-- TEST: recreate disappeared
-- + error: % table was present but now it does not exist (use @delete instead) 'disapparing_recreate'
-- +1 error:
create table disapparing_recreate
(
  id int
) @recreate;

-- TEST: try to remove a trigger without marking it @delete
-- + error: % trigger was present but now it does not exist (use @delete instead) 'trigger_removed_with_no_annotation'
-- +1 error:
create trigger trigger_removed_with_no_annotation
  before delete on foo
  for each row
  when old.id > 7
begin
  select old.id;
end;

-- TEST: trigger deleted correctly
-- + {create_trigger_stmt}: ok
-- - error:
create trigger trigger_will_be_deleted
  before delete on foo
begin
  select old.id;
end;

-- TEST: remove a facet from the schema
-- + error: % non-column facets have been removed from the table 't_removed_facet'
-- +1 error:
create table t_removed_facet(
  id integer not null,
  primary key (id)
);

-- TEST: column different in not a typical way
-- + error: % table has a column that is different in the previous and current schema 'id'
create table t_subtle_column_change(
  id integer references create_column_migrate_test(id) on delete cascade
);

-- TEST: columns added interleaved
-- +  {create_table_stmt}: t_several_columns_added_interleaved: { col1: integer notnull }
-- - error:
create table t_several_columns_added_interleaved(
  col1 integer,
  primary key (col1)
);

-- TEST: verify that you can't simply remove an existing migration
-- + error: % ad hoc schema migration directive was removed; this is not allowed 'WhoopsItsGone'
-- +1 error:
@schema_ad_hoc_migration(1, WhoopsItsGone);

-- TEST: verify that you can't simply remove an existing migration
-- + {schema_ad_hoc_migration_stmt}: err
-- + error: % ad hoc schema migration directive version number changed 'WhoopsItChanged'
-- +1 error:
@schema_ad_hoc_migration(1, WhoopsItChanged);

-- TEST: no problems here, no change, all is well
-- - error:
@schema_ad_hoc_migration(3, MigrateGoodToGo);

-- These two tables are changing from recreate group foo to create group bar
-- this must not cause any error in current or previous schema

-- TEST: simple recreate in a new group -- this cannot be allowed (see error docs)
-- + error: % recreate group annotation changed in table 'Recreated1'
create table Recreated1 ( id integer) @recreate(foo);

-- TEST: recreate in a new group is not allowed because it can cause FK violations
-- table delete order is required to be within the group and also globally correct and
-- these two are not miscible.  Therefore we cannot allow tables to move groups
-- + error: % recreate group annotation changed in table 'Recreated2'
create table Recreated2 ( id integer references Recreated1(id) ) @recreate(foo);


-- TEST: this table is moving into the base region that will generate an error

@end_schema_region;

@begin_schema_region different_region;

-- TEST: creating a table that will move to a different deployment region
-- + {create_table_stmt}: err
create table TChanging(id integer);

-- TEST: creating an index that will move to a different deployment region
-- + {create_index_stmt}: err
create index IChanging on TChanging(id);

-- TEST: creating a view that will move to a different deployment region
-- + {create_view_stmt}: err
create view VChanging as select * from TChanging;

-- TEST: creating a trigger that will move to a different deployment region
-- + {create_trigger_stmt}: err
create trigger TrigChanging
  before delete on foo
begin
  select old.id;
end;

-- TEST: creating an ad hoc migration that will move to a different deployment region
-- + {schema_ad_hoc_migration_stmt}: err
@schema_ad_hoc_migration(2, AdHocChanging);

@end_schema_region;

-- This is a new region to test for detecting errors even if the owning region happens later
@declare_schema_region SomeLeaf;

-- At the time we see this declaration, we only know that is is going into SomeLeaf
@begin_schema_region SomeLeaf;
create table TableWithDeferredOwner(id integer);
@end_schema_region;

-- TEST: find errors due to a conflicting deployable region appearing later in the file
-- Now we know that the above table is changing owners, but we deferred the checking
-- so we'll still find the problem.  This line is not the error so it will be marked ok.
-- + {declare_deployable_region_stmt}: DeployableRegion1: region deployable
-- - error:
@declare_deployable_region DeployableRegion1 using SomeLeaf;

@declare_schema_region simple_region_2;

@begin_schema_region simple_region_2;

-- TEST : these logical moves are legal and innocuous
create table logical_moving_T1(id integer primary key);

-- TEST : these logical moves are legal and innocuous
create table logical_moving_T2(id integer references logical_moving_T1(id), name text);
@end_schema_region;

@declare_schema_region high_numbered_thing;

@begin_schema_region high_numbered_thing;

-- TEST: check large numbers
-- The high numbered object is present in both current and previous
-- it's supposed to represent something that came from a different schema world
-- all we need to do is exclude its version number from consideration towards the
-- max used numbers so we can keep adding our local items with normal numbers
-- - error:
create table high_numbered_thing( id integer) @create(9999);

-- TEST: index deleted on deleted table the index goes away completely
-- - error:
create index deleted_index on t_was_correctly_deleted(id);

-- TEST: trigger deleted on deleted table the trigger  goes away completely
-- - error:
create trigger trigger_deleted_no_problemo
  before delete on t_was_correctly_deleted
begin
  select old.id;
end;

@end_schema_region;

create table table_staying(
  col1 int primary key not null
) @recreate(my_recreate_group);

-- This table (in the previous schema) has an FK within the same recreate group so that's ok
-- - error:
create table table_going(
  col1 text,
  col2 int,
  foreign key(col2) references table_staying(col1) on update cascade on delete cascade
) @recreate(my_recreate_group);

-- TEST: new attributes will be added with migrators that's ok
-- - error:
create table adding_with_migrators_ok(
  id integer primary key,
  id2 integer
) @create(2);

declare enum foo_enum integer (
 a = 10,
 b = 2
);

-- TEST: the value of 'a' has changed
-- + error: % table has a column that is different in the previous and current schema 'x'
create table foo_with_check(
 x integer check (x == foo_enum.a)
);

-- TEST: no change
-- - error:
create virtual table unchanged_virtual using my_virtual(goo) as (
  id integer
);

-- TEST: normal delete
-- - error:
create virtual table deleted_virtual using my_virtual(goo) as (
  id integer
);

-- + error: % current schema can't go back to @recreate semantics for 'undead_virtual'
-- +1 error:
create virtual table undead_virtual using my_virtual(goo) as (
  id integer
) @delete(3, cql:module_must_not_be_deleted_see_docs_for_CQL0392);

-- TEST: it's ok at add things to a virtual table and change args
-- - error:
create virtual table changing_virtual using my_virtual(goo) as (
  id integer
);

-- TEST: changing the delete version is bad
-- + error: % current delete version not equal to previous delete version for 'delete_change_virtual'
-- +1 error:
create virtual table delete_change_virtual using my_virtual(goo) as (
  id integer
) @delete(3, cql:module_must_not_be_deleted_see_docs_for_CQL0392);

-- TEST: ok for a temp view to just vanish
-- + {create_view_stmt}: ok
-- - error:
create temp view this_view_is_gone as select 1 x;

-- TEST: ok for a temp table to just vanish
-- + {create_table_stmt}: this_table_is_gone: { id: integer }
-- - error:
create temp table this_table_is_gone(id integer);

-- TEST: ok for a temp trigger to just vanish
-- + {create_trigger_stmt}: ok
-- - error:
create temp trigger this_trigger_is_gone
  before delete on foo
begin
  select old.id;
end;

-- TEST: test create table with not null column on conflict clause abort
-- + {create_table_stmt}: conflict_clause_t: { id: integer notnull }
-- + {col_attrs_not_null}: ok
-- + {int 2}
-- - error:
create table conflict_clause_t(id int not null on conflict fail);

-- TEST: test create table with pk column on conflict clause rollback
-- + {create_table_stmt}: conflict_clause_pk: { id: integer notnull }
-- + {indexed_columns_conflict_clause}
-- + {int 0}
-- - error:
create table conflict_clause_pk(
  id int not null,
  constraint pk1 primary key (id) on conflict rollback
);

-- TEST: this table was on the recreate plan, it needs to stay on that plan
-- + {create_table_stmt}: err
-- + error: % table was marked @delete but it needs to be marked @recreate @delete 'dropping_this'
-- +1 error:
create table dropping_this
(
  f1 integer,
  f2 text
) @recreate(foo);

-- TEST: this table had a group and losses it
-- + {create_table_stmt}: err
-- + error: % recreate group annotation changed in table 'losing_group'
-- +1 error:
create table losing_group
(
  id integer
) @recreate(foo);

-- TEST: this table gains a group, that's ok
-- {create_table_stmt}: gaining_group: { id: integer } @recreate
-- - error:
create table gaining_group
(
  id integer
) @recreate;

-- TEST: table matches
-- - error:
create table unsub_resub_trickery(id integer);

-- TEST: unsub matches
-- - error:
@unsub (20, unsub_resub_trickery);

-- TEST: resub matches
-- - error:
@resub (22, unsub_resub_trickery);

-- TEST: unsub matches
-- - error:
@unsub (24, unsub_resub_trickery);

-- TEST: resub matches
-- - error:
@resub (26, unsub_resub_trickery);

-- TEST: badly formed directive in previous section
-- + {schema_unsub_stmt}: err
-- + error: % @unsub directive must provide a table
-- +1 error:
@unsub (28);

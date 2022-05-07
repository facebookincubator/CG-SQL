/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- schema migration generation tests

@declare_schema_region shared;
@declare_schema_region extra using shared;
@declare_schema_region other;

@begin_schema_region shared;

create table foo(
  id integer primary key,
  rate long integer @delete(5),
  rate_2 long integer @delete(4, DeleteRate2Proc),
  id2 integer default 12345 @create(4, CreateId2Proc),
  name text @create(5),
  name_2 text @create(6)
);

create table added_table(
  id integer not null,
  name1 text,
  name2 text @create(4)
) @create(3) @delete(5);

-- this view will be declared in extra schema but not upgraded
create view shared_view as select * from foo;

-- this index will be declared in extra schema but not upgraded
create index shared_index on foo(name, name_2);

-- this trigger will be declared in extra schema but not upgraded
create trigger shared_trigger
  before insert on foo
begin
  select 1;
end;

-- this view is present in the output
create view live_view as select * from foo;

-- this view is not present in the output
create view dead_view as select * from foo @delete(2, DeadViewMigration);

-- make a recreate-group with an FK dependency (legal)
create table g1(
  id integer primary key,
  name text
) @recreate(gr1);

create table use_g1(
  id integer primary key references g1(id),
  name2 text
) @recreate(gr1);

create index gr1_index on g1(name);
create index gr1_index2 on g1(name, id);
create index gr1_index3 on g1(name, id) @delete(5);

@end_schema_region;

@begin_schema_region extra;

-- this table will be declared in the extra schema upgrade and upgraded
create table table2(
  id integer not null references foo(id),
  name1 text @create(2, CreateName1Proc),
  name2 text @create(2, CreateName2Proc),
  name3 text @create(2), -- no proc
  name4 text @create(2) -- no proc
);

-- this view will be declared and upgraded in extra schema
create view another_live_view as select * from table2;

-- this index will be declared and upgraded in extra schema
create index not_shared_present_index on table2(name1, name2);

-- this index is going away
create index index_going_away on table2(name3) @delete(3);

-- this trigger will be declared and upgraded in extra schema
create trigger not_shared_trigger
  before insert on foo
begin
  select new.id;
end;

@end_schema_region;

@begin_schema_region other;

create table other_table(id integer);

@end_schema_region;

-- this table is on the recreate plan
create table table_to_recreate(
  id integer not null,
  name text
) @recreate;

-- these tables are in a recreate group
create table grouped_table_1( id integer not null, name text ) @recreate(my_group);
create table grouped_table_2( id integer not null, name text ) @recreate(my_group);
create table grouped_table_3( id integer not null, name text ) @recreate(my_group);

-- temp tables go into the temp table section
create temp table this_table_appears_in_temp_section(
 temp_section_integer integer
);

-- temp views go into the temp section
create temp view temp_view_in_temp_section as select * from foo;

@begin_schema_region shared;

-- temp triggers go into the temp section
create temp trigger temp_trigger_in_temp_section
  before delete on foo
  for each row
  when old.id > 7
begin
  select old.id;
end;

-- an actual trigger, this will be managed using recreate rules
create trigger insert_trigger
  before insert on foo
  for each row
  when new.id > 7
begin
  select new.id;
end;

-- this trigger was retired
create trigger old_trigger_was_deleted
  before insert on foo
begin
  select new.id;
end @delete(3);

-- do an ad hoc migration at version 5 (inside the region)
@schema_ad_hoc_migration(5, MyAdHocMigrationScript);

-- do an ad hoc migration for recreation
@schema_ad_hoc_migration for @recreate(gr1, RecreateGroup1Migration);

@end_schema_region;

-- declare a select function that we will use
declare select function filter_(id integer) integer not null;

-- now use that function in a trigger
create trigger trig_with_filter
  before insert on foo
  when filter_(new.id) = 3
begin
  delete from foo where id = 77;
end;

-- test that column type of id in t5, t6 tables is not converted to integer.
create table t5(
  id long int primary key autoincrement,
  data text
);

create table t6(
  id long int primary key,
  foreign key (id) references t5 (id) on update cascade on delete cascade
);

create virtual table a_virtual_table using a_module ( this, that, the_other )
as (
  id integer @sensitive,
  t text
);

create virtual table @eponymous epon using epon
as (
  id integer @sensitive,
  t text
);

create virtual table complex_virtual_table using a_module(arguments following)
as (
  id integer @sensitive,
  t text
);

create virtual table deleted_virtual_table using a_module(arguments following)
as (
  id integer @sensitive,
  t text
) @delete(4, cql:module_must_not_be_deleted_see_docs_for_CQL0392);

create table migrated_from_recreate(
  id integer,
  t text
) @create(4, cql:from_recreate);

create table conflict_clause_t(id int not null on conflict fail);

create table conflict_clause_pk(
  id int not null,
  constraint pk1 primary key (id) on conflict rollback
);

create table expression_pk(
  id int not null,
  constraint pk1 primary key (id/2, id%2)
);

create table expression_uk(
  id int not null,
  constraint uk1 unique (id/2, id%2)
);

-- This table has to be deleted after delete_first
-- even though it sorts in the other order by name
-- the old algorithm would have got this wrong
create table delete__second
(
 id integer primary key
) @delete(7);

create table delete_first
(
  id integer references delete__second(id)
) @delete(7);

-- This table has to be created before create__second
-- even though it sorts in the other order by name.
-- the old algorithm would have got this wrong
create table create_first
(
 id integer primary key
) @create(7);

create table create__second
(
  id integer references create_first(id)
) @create(7);


@attribute(cql:blob_storage)
create table blob_storage_at_create_table(
  x integer,
  y text
) @create(5);

@attribute(cql:blob_storage)
create table blob_storage_baseline_table(
  x integer,
  y text
);

create table unsub_recreated(
 anything text
) @recreate;

create index unsub_recreated_index on unsub_recreated(anything);

create trigger unsub_recreated_trigger
  before insert on unsub_recreated
begin
  select 1;
end;

@unsub(1, unsub_recreated);

@begin_schema_region other;

create table unsub_voyage(
 v1 integer,
 v3 text @create(3),
 v5 text @create(5),
 v7 text @create(7)
);

create index unsub_voyage_index on unsub_voyage(v1);

create trigger unsub_voyage_trigger
  before insert on unsub_voyage
begin
  select 1;
end;

@unsub(1, unsub_voyage);
@resub(5, unsub_voyage);

create table some_table(id integer);

create view foo_view_unsubscribed as select * from some_table;
create view foo_view_normal as select * from some_table;

@unsub(5, foo_view_unsubscribed);

@end_schema_region;

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST: declare a base region
-- + "name" : "region0"
-- + "using" : [  ]
-- + "usingPrivately" : [  ]
-- + "name" : "region0",
-- + "isDeployableRoot" : 0,
-- + "deployedInRegion" : "region2",
@declare_schema_region region0;

-- TEST: declare an orphan region
-- + @DECLARE_SCHEMA_REGION orphan_region
-- + "name" : "orphan_region",
-- + "isDeployableRoot" : 0,
-- + "deployedInRegion" : "(orphan)",
-- + "using" : [  ]
-- + "usingPrivately" : [  ]
@declare_schema_region orphan_region;

-- note the name is canonicalized to the delared name, so the results should still be "region0" not "Region0"
@begin_schema_region Region0;

-- TEST: simple table with 2 columns
-- validating the basic shape of the output
-- + "name" : "Foo"
-- + "columns" : [
-- + "name" : "id"
-- + "type" : "integer",
-- +1 "isNotNull" : 1
-- + "name" : "name"
-- +1 "type" : "text"
-- + "primaryKey" : [
-- + "foreignKeys" : [
-- + "uniqueKeys" : [
-- + "indices" : [ "region_0_index", "MyIndex", "MyOtherIndex" ]
-- + "region" : "region0",
create table Foo
(
  id int not null,
  name text
);

-- TEST: ensure the view is marked in its region
-- + "name" : "region_0_view",
-- + "isTemp" : 0,
-- + "isDeleted" : 0,
-- + "region" : "region0",
-- + "select" : "SELECT id, name FROM Foo",
create view region_0_view as select * from Foo;

-- TEST: ensure this index is in the right region
-- + "name" : "region_0_index",
-- + "table" : "Foo",
-- + "region" : "region0",
-- + "columns" : [ "name", "id" ],
create index region_0_index on Foo(name, id);

@end_schema_region;

-- TEST: force primary key flag and autoinc
-- primary key implies not null
-- + "name" : "T2"
-- + "name" : "id"
-- + "type" : "integer",
-- + "isPrimaryKey" : 1
-- + "isAutoIncrement" : 1
-- + "primaryKey" : [ "id" ],
-- + "isNotNull" : 1
-- - "region"
create table T2
(
  id integer primary key autoincrement
);

-- TEST: force unique  key flag
-- unique key implies not null
-- + "name" : "T3"
-- + "name" : "id"
-- + "type" : "integer",
-- + "isUniqueKey" : 1
-- + "isNotNull" : 1
-- + "isAutoIncrement" : 0
-- - "isPrimaryKey" : 1
create table T3
(
  id integer unique not null
);

-- TEST: force some misc attributes
-- + "name" : "T4"
-- + "name" : "id"
-- +2 "attributes" : [
-- + "name" : "cool"
-- + "value" : 1
-- + "name" : "foo"
-- + "value" : "bar"
-- + "name" : "num"
-- + "value" : -7
-- + "name" : "hex"
-- + "value" : 83
@attribute(foo=bar)
@attribute(num=-7)
@attribute(hex=0x53)
create table T4
(
  @attribute(cool)
  id integer
);

/* NOTE:
 *   \a is converted to \u0007 \a isn't a valid JSON escape sequence (it is in C)
 *   \v is converted to \u000b because python doesn't like \v even though it's in the spec
 */

-- TEST: use strange string escapes
-- + "name" : "T5"
-- + "name" : "r"
-- + "type" : "real"
-- + "name" : "b"
-- + "type" : "bool"
-- + "name" : "bl"
-- + "type" : "blob"
-- + "name" : "l"
-- + "type" : "long"
-- + "name" : "crazy"
-- + "value" : "\\ ' \u0007 \b \f \n \t \r \u000b \\ \" "
@attribute(crazy="\\ ' \a \b \f \n \t \r \v \\ \" ")
create table T5
(
  r real,
  bl blob,
  b bool,
  l long integer
);

-- TEST: use crazy strings in a SQL fragment
-- the raw string
-- + SELECT "\\ ' \a \b \f \n \t \r \v \\ \" " AS crazy;
-- + "name" : "crazy_string",
-- + "statement" : "SELECT '\\ '' \u0007 \b \f \n \t \r \u000b \\ \" ' AS crazy",
create proc crazy_string()
begin
  select "\\ ' \a \b \f \n \t \r \v \\ \" " as crazy;
end;

-- TEST: use long constant attributes and compound name
-- + "name" : "T6"
-- + "isTemp" : 1,
-- + "ifNotExists" : 1,
-- + "withoutRowid" : 1,
-- + "attributes" : [
-- + "name" : "this_that"
-- + "value" : 1
@attribute(this:that=1L)
create temp table if not exists T6 (
  id integer not null
) without rowid;

-- TEST: use create/delete on column
-- + "name" : "T7a"
-- + "isAdded" : 1,
-- + "addedVersion" : 1,
-- + "isDeleted" : 1,
-- + "deletedVersion" : 3
create table T7a (
  id integer @create(1) @delete(3)
);

-- TEST: use create/delete on column, with migration procs
-- + "name" : "T7b"
-- + "isAdded" : 1,
-- + "addedVersion" : 1,
-- + "addedMigrationProc" : "t7_col_create"
-- + "isDeleted" : 1,
-- + "deletedMigrationProc" : "t7_col_delete"
-- + "deletedVersion" : 3
create table T7b (
  id integer @create(1, t7_col_create) @delete(3, t7_col_delete)
);

-- TEST: use create/delete on table
-- + "name" : "T8a"
-- + "isAdded" : 1,
-- + "addedVersion" : 1,
-- + "isDeleted" : 1,
-- + "deletedVersion" : 3
create table T8a (
  id integer
) @create(1) @delete(3);

-- TEST: use create/delete on table, with migration procs
-- + "name" : "T8b"
-- + "isAdded" : 1,
-- + "addedVersion" : 1,
-- + "addedMigrationProc" : "t8_table_create",
-- + "isDeleted" : 1,
-- + "deletedVersion" : 3
-- + "deletedMigrationProc" : "t8_table_delete",
create table T8b (
  id integer
) @create(1, t8_table_create) @delete(3, t8_table_delete);

-- TEST: test other attribute types
-- + "name" : "T9"
-- + "name" : "an_integer"
-- + "value" : 1
-- +2 "name" : "a_double"
-- +2 "value" : 2.5
-- + "defaultValue" : 3
-- + "defaultValue" : "xyzzy"
-- + "defaultValue" : -3.5
-- + "defaultValue" : 123456789123456789
@attribute(an_integer=1)
@attribute(a_double=2.5)
@attribute(a_double=2.5)
create table T9 (
  id1 integer default 3 primary key,
  id2 integer,
  id3 integer,
  name text default "xyzzy",
  val real default -3.5,
  val2 long default 123456789123456789,
  constraint uk1 unique (id2, id3)
);

-- TEST: create an fk
-- + "name" : "T10"
-- + "columns" : [ "id1", "id2" ],
-- + "referenceTable" : "T9",
-- + "referenceColumns" : [ "id2", "id3" ]
-- + "name" : "uk1"
-- + "columns" : [ "id2", "id3" ]
-- + "columns" : [ "id1", "id2" ]
-- + "name" : "id1_uk"
-- + "columns" : [ "id1" ]
-- + "columns" : [ "id3", "id4" ]
-- + "name" : "id4_uk"
-- + "columns" : [ "id4" ]
-- pk columns are not null
-- +2 "isNotNull" : 1,
-- other columns are nullable
-- +2 "isNotNull" : 0,
create table T10 (
  id1 integer unique,
  id2 integer,
  id3 integer,
  id4 integer unique,
  primary key (id1, id2),
  foreign key (id1, id2) references T9 (id2, id3),
  constraint uk1 unique ( id2, id3 ),
  unique ( id3, id4 )
);

-- TEST: create an fk
-- + "name" : "T11"
-- + "onDelete" : "CASCADE",
-- + "onUpdate" : "SET NULL",
create table T11 (
  id1 integer,
  id2 integer,
  id3 integer,
  foreign key (id1) references T9 (id1) on delete cascade,
  foreign key (id1) references T9 (id1) on update set null
);

-- TEST: create an fk with more exotic options
-- + "name" : "T12"
-- + "onDelete" : "SET DEFAULT",
-- + "onUpdate" : "NO ACTION",
-- exactly one of each
-- +1 "isDeferred" : 0
-- +1 "isDeferred" : 1
create table T12 (
  id1 integer,
  id2 integer,
  id3 integer,
  foreign key (id1) references T9 (id1) on delete set default deferrable initially deferred,
  foreign key (id2) references T9 (id1) on update no action
);

-- TEST: create an fk with the restrict option
-- + "name" : "T12a"
-- + "onDelete" : "RESTRICT",
-- exactly one
-- +1 "isDeferred" : 0
create table T12a (
  id1 integer,
  id2 integer,
  id3 integer,
  foreign key (id1) references T9 (id1) on delete restrict not deferrable
);

-- TEST: create an fk with the restrict option
-- + "name" : "T12b"
-- + "onUpdate" : "NO ACTION",
-- + "onDelete" : "NO ACTION",
-- exactly one
-- +1 "isDeferred" : 0
create table T12b (
  id1 integer,
  id2 integer,
  foreign key (id1) references T9 (id1) not deferrable
);

-- TEST: simple parameters
-- + "name" : "a_query"
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "name" : "pattern"
-- +2 "isNotNull" : 1
-- + "name" : "reject"
-- +2 "type" : "text"
-- + "statement" : "SELECT id FROM Foo WHERE name LIKE ? AND name <> ?",
-- + "statementArgs" : [ "pattern", "reject" ]
create proc a_query(pattern text not null, reject text)
begin
  select id from Foo where name like pattern and name <> reject;
end;

-- TEST: complex parameters
-- + "name" : "with_complex_args"
-- + "name" : "pattern",
-- + "type" : "text",
-- + "isNotNull" : 1
-- + "binding" : "out",
-- + "name" : "arg",
-- + "type" : "real",
-- + "isNotNull" : 0
-- +  "binding" : "inout",
create proc with_complex_args(out pattern text not null, inout arg real)
begin
  select 1 a;
end;

-- TEST: more clauses, including having and others
-- + "name" : "bigger_query"
-- + "args" : [
-- + "name" : "pattern"
-- +3 "type" : "text"
-- +2 "isNotNull" : 1
-- + "name" : "reject"
-- + "projection" : [
-- + "name" : "id"
-- + "type" : "integer",
-- + "name" : "name",
-- + "statement" : "SELECT DISTINCT id, name FROM Foo WHERE name LIKE ? AND name <> ? GROUP BY name HAVING name > ? ORDER BY ? LIMIT 1 OFFSET 3",
-- + "statementArgs" : [ "pattern", "reject", "reject", "pattern" ]
create proc bigger_query(pattern text not null, reject text)
begin
  select distinct * from Foo where name like pattern and name <> reject group by name having name > reject order by pattern limit 1 offset 3;
end;

-- TEST: insert proc
-- + "name" : "insert_proc",
-- + "args" : [
-- + "name" : "id_",
-- + "type" : "integer",
-- + "isNotNull" : 1
-- + "name" : "name_",
-- + "type" : "text",
-- + "isNotNull" : 0
-- + "table" : "Foo",
-- + "statement" : "INSERT OR REPLACE INTO Foo(id, name) VALUES(?, ?)",
-- + "statementArgs" : [ "id_", "name_" ]
create proc insert_proc(id_ integer not null, name_ text)
begin
  insert or replace into Foo(id, name) values(id_, name_);
end;

-- TEST: general form but no return type
-- + "name" : "atypical_noreturn",
-- + "usesDatabase" : 0
create proc atypical_noreturn()
begin
  declare C cursor like select 1 A;
end;

-- TEST: general form with single row result
-- + "name" : "typical_outresult",
-- + "usesTables" : [  ],
-- - "hasSelectResult"
-- + "hasOutResult" : 1,
-- - "hasOutUnionResult"
-- + "projection" : [
-- + "name" : "A",
-- + "type" : "integer",
-- + "isNotNull" : 1
-- + "usesDatabase" : 0
create proc typical_outresult()
begin
  declare C cursor like select 1 A;
  fetch C from values (7);
  out C;
end;

-- TEST: general form with single row result
-- + "name" : "typical_out_union_result",
-- + "usesTables" : [  ],
-- - "hasSelectResult"
-- - "hasOutResult"
-- + "hasOutUnionResult" : 1,
-- + "projection" : [
-- + "name" : "A",
-- + "type" : "integer",
-- + "isNotNull" : 1
-- + "usesDatabase" : 0
create proc typical_out_union_result()
begin
  declare C cursor like select 1 A;
  fetch C from values (7);
  out union C;
  out union C;
end;

-- TEST: general form with full result set
-- + "name" : "typical_select",
-- + "args" : [
-- + "usesTables" : [ "T5" ],
-- + "hasSelectResult" : 1,
-- - "hasOutResult"
-- - "hasOutUnionResult"
-- + "projection" : [
-- + "usesDatabase" : 1
create proc typical_select()
begin
  -- this declare forces this to be a non-single-statement proc
  declare x integer;
  select * from T5;
end;

-- TEST: delete proc
-- + "name" : "delete_proc"
-- + "name" : "name_",
-- +  "type" : "text"
-- + "table" : "Foo",
-- + "statement" : "DELETE FROM Foo WHERE name LIKE ?",
-- + "statementArgs" : [ "name_" ]
create proc delete_proc(name_ text)
begin
  delete from foO where name like name_; -- name should normalize
end;

-- TEST: with delete form
create proc delete_with_values(name_ text)
begin
  with names(n) as ( values ("this") , ("that") )
  delete from foO where name in (select * from names);
end;

-- TEST: an update statement
-- + "table" : "Foo",
-- + "statement" : "UPDATE Foo SET name = ? WHERE id = ? ORDER BY name LIMIT 1",
-- + "statementArgs" : [ "name_", "id_" ]
create proc update_proc(id_ integer not null, name_ text)
begin
  update foO set name = name_ where id = id_ order by name limit 1;
end;

-- TEST: an update statement and with clause
-- + "name" : "update_with_proc",
-- + "table" : "Foo",
-- + "statement" : "WITH names (n) AS (VALUES('this'), ('that')) UPDATE foO SET name = ? WHERE name IN (SELECT n FROM names)",
-- + "statementArgs" : [ "name_" ]
create proc update_with_proc(id_ integer not null, name_ text)
begin
  with names(n) as ( values ("this") , ("that") )
  update foO set name = name_ where name in (select * from names);
end;

-- TEST: an index
-- + "name" : "MyIndex",
-- + "table" : "Foo",
-- + "isUnique" : 1,
-- + "ifNotExists" : 1,
-- + "columns" : [ "name", "id" ]
-- + "sortOrders" : [ "desc", "asc" ]
create unique index if not exists MyIndex on Foo(name desc, id asc);

-- TEST: an index
-- + "name" : "MyOtherIndex",
-- + "table" : "Foo",
-- + "isDeleted" : 0,
-- + "columns" : [ "id" ]
create index MyOtherIndex on Foo(id);

-- TEST: an index
-- + "name" : "YetAnotherIndex",
-- + "table" : "Foo",
-- + "columns" : [ "id" ],
-- + "isDeleted" : 1,
-- + "deletedVersion" : 1
create index YetAnotherIndex on Foo(id) @delete(1);

-- TEST: a view
-- + "name" : "MyView",
-- + "isDeleted" : 0
-- + "select" : "SELECT id, name FROM Foo",
-- + "name" : "id",
-- + "type" : "integer",
-- + "isNotNull" : 1
-- + "name" : "name",
-- + "type" : "text",
-- + "isNotNull" : 0
create view MyView as select * from Foo;

-- TEST: a second view, forces comma handling, caught by JSON parse test in test.sh
-- +  CREATE VIEW MyOtherView AS
create view MyOtherView as select * from Foo;

-- TEST: dummy inserts
-- + "statement" : "INSERT INTO Foo(id, name) VALUES(?, printf('name_%d', ?))",
-- + "columns" : [ "id", "name" ],
-- + "value" : "?",
-- + "valueArgs" : [ "_seed_" ]
-- + "value" : "printf('name_%d', ?)",
-- + "valueArgs" : [ "_seed_" ]
create proc dummy_insert_proc(seed_ integer not null)
begin
  insert into fOo() values() @dummy_seed(seed_) @dummy_nullables;
end;

-- TEST: this view has been deleted and should be marked as such
-- + "isDeleted" : 1,
-- + "deletedVersion" : 1
create view ADeletedView as select * from Foo @delete(1);

-- TEST: this view has been deleted and should be marked as such. This also uses migration procs
-- + "isDeleted" : 1
-- + "deletedVersion" : 1
-- + "deletedMigrationProc" : "view_delete"
create view ADeletedViewWithMigrationProc as select * from Foo @delete(1, view_delete);

-- TEST: join tables, create new dependencies
-- + "statement" : "SELECT id, name, r, bl, b, l FROM Foo AS T1 INNER JOIN T5 ON T1.id = ? AND T1.id = T5.l",
@attribute(my_attribute = 'This is a string attribute')
create proc joiner(id_ integer not null)
begin
  select * from Foo T1 inner join T5 on T1.id = id_ and T1.id = T5.l;
end;

-- TEST: declare database with attributes
-- + "name" : "dbname",
-- + "value" : "fred.sql"
-- + "name" : "dbfile",
-- + "value" : "test/cg_test_json_schema.sql"
-- + "name" : "my_other_attribute",
-- + "value" : ["any", ["tree", "of"], "values"]
@attribute(my_other_attribute = ('any', ('tree', 'of'), 'values'))
@attribute(dbname = 'fred.sql')
@attribute(dbfile = @FILE('xplat/'))
declare database object;

-- TEST: declare a table with some fk columns on the column
-- + "columns" : [ "id1" ],
-- + "referenceTable" : "T2",
-- + "referenceColumns" : [ "id" ],
-- + "onUpdate" : "CASCADE",
-- + "onDelete" : "NO ACTION",
-- + "isDeferred" : 1
-- + "columns" : [ "id2" ],
-- + "referenceTable" : "T10",
-- + "referenceColumns" : [ "id4" ],
-- + "onUpdate" : "NO ACTION",
-- + "onDelete" : "CASCADE",
-- + "isDeferred" : 0
-- + "columns" : [ "id1", "id2" ],
-- + "referenceTable" : "T10",
-- + "referenceColumns" : [ "id3", "id4" ],
-- + "onUpdate" : "NO ACTION",
-- + "onDelete" : "NO ACTION",
create table with_fk_on_columns(
 id1 integer not null references T2(id) on update cascade deferrable initially deferred,
 id2 integer not null references T10(id4) on delete cascade,
 foreign key (id1, id2) references T10(id3, id4)
);

-- TEST: emit recreate annotation with group
-- +  CREATE TABLE recreated_in_a_group(
-- +    id INTEGER
-- +  ) @RECREATE(my_recreate_group)
-- +  "name" : "recreated_in_a_group",
-- +  "isAdded" : 0,
-- +  "isDeleted" : 0,
-- +  "isRecreated": 1,
-- +  "recreateGroupName" : "my_recreate_group",
create table recreated_in_a_group(
 id integer
) @recreate(my_recreate_group);


-- TEST: insert with select is not a simple insert form
-- + "name" : "insert_with_select",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "insertTables" : [ "T3" ],
-- + "usesTables" : [ "T3" ],
-- + "table" : "T3",
-- + "statement" : "INSERT INTO T3(id) SELECT 1",
-- + "statementArgs" : [  ],
-- + "statementType" : "INSERT",
-- + "columns" : [ "id" ]
-- - "values"
create procedure insert_with_select()
begin
  insert into T3 select 1;
end;

-- TEST: insert compound form is not simple even though it starts with values
-- + "name" : "insert_compound",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "insertTables" : [ "T3" ],
-- + "usesTables" : [ "T3" ],
-- + "table" : "T3",
-- + "statement" : "INSERT INTO T3(id) VALUES(1) UNION ALL SELECT 1 AS column1",
-- + "statementArgs" : [  ],
-- + "statementType" : "INSERT",
-- + "columns" : [ "id" ]
-- - "values"
create procedure insert_compound()
begin
  insert into T3 values (1) union all select 1 column1;
end;

-- TEST: insert multi_value form is not simple, it goes into the insert general section
-- + "name" : "insert_multi_value",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "insertTables" : [ "T3" ],
-- + "usesTables" : [ "T3" ],
-- + "table" : "T3",
-- + "statement" : "INSERT INTO T3(id) VALUES(1), (2), (3)",
-- + "statementArgs" : [  ],
-- + "statementType" : "INSERT",
-- - "values"
create procedure insert_multi_value()
begin
  insert into T3 values (1), (2), (3);
end;

-- TEST: declare a table with sensitive column
-- + danger TEXT @SENSITIVE
-- +1 @SENSITIVE
-- +1 "isSensitive" : 1,
create table radioactive(
 id integer not null,
 danger text @sensitive
);

-- TEST: declare a simple query that has sensitive data
-- +1 "isSensitive" : 1
create proc radioactive_proc()
begin
 select * from radioactive;
end;

-- TEST: upsert statement
-- + "name" : "upsert_proc",
-- + "args" : [
-- + ],
-- + "usesTables" : [ "T3" ],
-- + "statement" : "INSERT INTO T3(id) VALUES(1) ON CONFLICT DO UPDATE SET id = 1 WHERE id = 9",
-- + "statementArgs" : [  ],
-- + "statementType" : "INSERT",
create proc upsert_proc()
begin
 insert into T3(id) values(1) on conflict do update set id=1 where id=9;
end;

-- TEST: with upsert statement
-- + "args" : [
-- + ],
-- + "name" : "with_upsert_proc",
-- + "usesTables" : [ "T3" ],
-- + "statement" : "WITH data (id) AS (VALUES(1), (2), (3)) INSERT INTO T3(id) SELECT id FROM data WHERE 1 ON CONFLICT DO UPDATE SET id = 1 WHERE id = 9",
-- + "statementArgs" : [  ],
-- + "statementType" : "INSERT",
create proc with_upsert_proc()
begin
 with data(id) as (values (1), (2), (3))
 insert into T3(id) select id from data where 1 on conflict do update set id=1 where id=9;
end;

-- TEST: with insert statement
-- + "name" : "with_insert_proc",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "insertTables" : [ "T3" ],
-- + "usesTables" : [ "T3" ],
-- + "table" : "T3",
-- + "statement" : "WITH data (id) AS (VALUES(1), (2), (?)) INSERT INTO T3(id) SELECT id FROM data",
-- + "statementArgs" : [ "x" ],
-- + "statementType" : "INSERT",
-- + "columns" : [ "id" ]
create proc with_insert_proc(x integer not null)
begin
 with data(id) as (values (1), (2), (x))
 insert into T3(id) select * from data;
end;

-- TEST: procedure with object arguments, ensure object type emitted
-- + CREATE PROC object_proc (anObject OBJECT)
-- + "name" : "object_proc",
-- + "args" : [
-- + "name" : "anObject",
-- + "type" : "object",
-- + "isNotNull" : 0
create proc object_proc(anObject OBJECT)
begin
  select 1 x; /* any body will do */
end;


-- some assets to use in the arg orgin tests
declare proc result_proc(id integer, t text) (x integer, y integer);

create table T1 (
  id integer,
  name text);

declare a_cursor cursor like select 1 x, 2 y;

declare b_cursor cursor like T1;

create view my_view as select 1 foo, T1.* from T1;

-- TEST: args like a proc result
-- + "name" : "arg1_x",
-- + "argOrigin" : "arg1 result_proc x"
-- + "name" : "arg1_y",
-- + "argOrigin" : "arg1 result_proc y"
create proc proc_args_1(arg1 like result_proc)
begin
end;

-- TEST: args like a table
-- + "argOrigin" : "T1 id",
-- + "argOrigin" : "T1 name",
create proc proc_args_2(like T1)
begin
end;

-- TEST: args like a cursor (ad hoc shape)
-- The cursor makes its own struct shape that has no name
-- so the cursor name is the best we can do.
-- + "argOrigin" : "a_cursor x",
-- + "argOrigin" : "a_cursor y",
create proc proc_args_3(like a_cursor)
begin
end;

-- TEST: args like a cursor (named shape)
-- note the original type name is used
-- + "argOrigin" : "T1 id",
-- + "argOrigin" : "T1 name",
create proc proc_args_4(like b_cursor)
begin
end;

-- TEST: args like a procedures arguments 
-- + "argOrigin" : "proc_args_1[arguments] arg1_x",
-- + "argOrigin" : "proc_args_1[arguments] arg1_y",
create proc proc_args_5(like proc_args_1 arguments)
begin
end;

-- TEST: args like a view
-- + "argOrigin" : "my_view foo",
-- + "argOrigin" : "my_view id",
-- + "argOrigin" : "my_view name",
create proc proc_args_6(like my_view)
begin
end;

-- TEST: declare a region with one dependency (generates dep list)
-- +  "name" : "region1"
-- +  "using" : [ "region0" ]
-- +  "usingPrivately" : [ 1 ]
@declare_schema_region region1 using region0 private;

-- TEST: declare a region with two dependencies (forces the comma in output)
-- + @DECLARE_DEPLOYABLE_REGION region2 USING region1, region0
-- +  "name" : "region2"
-- +  "using" : [ "region1", "region0" ]
-- +  "usingPrivately" : [ 0, 0 ]
-- + "isDeployableRoot" : 1,
-- + "deployedInRegion" : "region2",
@declare_deployable_region region2 using region1, region0;

-- TEST: basic delete trigger
-- + "name" : "trigger1",
-- + "target" : "Foo",
-- + "isTemp" : 1,
-- + "ifNotExists" : 1,
-- + "isBeforeTrigger" : 1,
-- + "isDeleteTrigger" : 1,
-- + "forEachRow" : 1,
-- + "whenExpr" : "old.id = 3",
-- + "whenExprArgs" : [  ],
-- + "statement" : "CREATE TEMP TRIGGER IF NOT EXISTS trigger1 BEFORE DELETE ON Foo FOR EACH ROW WHEN old.id = 3 BEGIN DELETE FROM Foo WHERE id = id + 1;  DELETE FROM Foo WHERE id = old.id; END",
-- + "statementArgs" : [  ],
-- + "usesTables" : [ "Foo" ]
-- + "deleteTables" : [ "Foo" ],
-- - "insertTables"
-- - "updateTables"
create temp trigger if not exists trigger1
  before delete on foo -- name should normalize
  for each row
  when old.id = 3
begin
  delete from Foo where id = id + 1;
  delete from Foo where id = old.id;
end;

-- TEST: basic insert trigger
-- + "name" : "trigger2",
-- + "target" : "Foo",
-- + "isTemp" : 0,
-- + "ifNotExists" : 0,
-- + "isAfterTrigger" : 1,
-- + "isInsertTrigger" : 1,
-- + "statement" : "CREATE TRIGGER trigger2 AFTER INSERT ON Foo BEGIN DELETE FROM Foo WHERE id > new.id; END",
-- + "statementArgs" : [  ],
-- + "usesTables" : [ "Foo" ]
-- + "deleteTables" : [ "Foo" ],
-- - "insertTables"
-- - "updateTables"
create trigger trigger2
  after insert on Foo
begin
  delete from Foo where id > new.id;
end;

-- TEST: use update instead of on a view
-- + "name" : "trigger3",
-- + "target" : "MyView",
-- + "isTemp" : 0,
-- + "ifNotExists" : 0,
-- + "isInsteadOfTrigger" : 1,
-- + "isUpdateTrigger" : 1,
-- + "whenExpr" : "old.id > 1 AND new.id < 3",
-- + "insertTables" : [ "Foo" ]
-- + "updateTables" : [ "Foo" ]
-- - "deleteTables"
create trigger trigger3
  instead of update on MyView
  when old.id > 1 and new.id < 3
begin
  update Foo set id = 7 where name > old.name and name < new.name;
  insert into Foo values (7, 'goo');
end;

-- TEST: specify update columns
-- + "name" : "trigger4",
-- + "target" : "MyView",
-- + "isTemp" : 0,
-- + "ifNotExists" : 0,
-- + "isInsteadOfTrigger" : 1,
-- + "isUpdateTrigger" : 1,
-- + "statement" : "CREATE TRIGGER trigger4 INSTEAD OF UPDATE OF id, name ON MyView BEGIN SELECT 1; END",
create trigger trigger4
  instead of update of id, name on MyView
begin
  select 1;
end;

@begin_schema_region region0;

-- TEST: basic delete trigger with RAISE expression
-- + "name" : "trigger5",
-- + "target" : "Foo",
-- + "isTemp" : 1,
-- + "ifNotExists" : 1,
-- + "isDeleted" : 0,
-- + "isBeforeTrigger" : 1,
-- + "isDeleteTrigger" : 1,
-- + 'omg roll it back!'
-- + "region" : "region0"
create temp trigger if not exists trigger5
  before delete on Foo
begin
  select RAISE(rollback, "omg roll it back!");
end;

-- TEST: use delete on trigger
-- + "name" : "trigger6",
-- + "isDeleted" : 1,
-- + "deletedVersion" : 3
create trigger trigger6
  after insert on Foo
begin
  select 1;
end @delete(3);

-- TEST: procedure inside region
-- + "name" : "proc_inside_region"
-- + "region" : "region0",
create proc proc_inside_region()
begin
select 1 a;
end;

@end_schema_region;

-- TEST: a "with select" form is still simple enough to be simple
-- + "name" : "with_select_proc",
-- +   "definedInFile" : "cg_test_json_schema.sql",
-- +      "projection" : [
-- +         "name" : "v",
-- +         "type" : "integer",
-- +         "isNotNull" : 1
-- +   "statement" : "WITH nums (i) AS (SELECT 0 UNION ALL SELECT i + 1 FROM nums LIMIT 1), vals (v) AS (SELECT i FROM nums) SELECT v FROM vals",
-- +   "statementArgs" : [  ]
create procedure with_select_proc()
begin
  with
  nums(i) as (
    select 0
    union all
    select i+1 from nums
    limit 1
   ),
  vals(v) as (select i from nums)
  select * from vals;
END;

-- TEST: empty proc
-- no exciting valiations here just making sure we cover the cases with no statement list
-- + {
-- + "name" : "empty_proc",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "usesTables" : [  ],
-- + "usesDatabase" : 0
-- + },
create proc empty_proc()
begin
end;

-- TEST: empty blocks in all the places
-- + {
-- + "name" : "empty_blocks",
-- + "definedInFile" : "cg_test_json_schema.sql",
-- + "args" : [
-- + ],
-- + "usesTables" : [  ],
-- + "usesDatabase" : 1
-- + }
create proc empty_blocks()
begin
  if 1 then
  end if;

  if 2 then
  else
  end if;

  if 3 then
  else if 4
  then
  else
  end if;

  while 1
  begin
  end;

  declare c cursor for select 1 x;
  loop fetch c
  begin
  end;

  begin try
  end try;
  begin catch
  end catch;
end;

declare proc proc_as_func(out x integer not null);
declare proc other_proc();

-- TEST: check proc to proc usage
-- + "name" : "proc_with_deps",
-- + "usesTables" : [  ],
-- + "usesProcedures" : [ "other_proc", "proc_as_func" ]
create proc proc_with_deps(out x integer not null)
begin
  -- note unusal casing, the JSON output should use the canonical name in the dependencies
  call other_Proc();
  set x := proC_aS_Func();
end;

-- TEST: escape characters in JSON
-- + "statement" : "SELECT '\\\r\n\t\b\f\"\u0001' AS quoted_text",
create proc json_escapes()
begin
  select "\\\r\n\t\b\f\"\x01" quoted_text;
end;

-- TEST: ad-hoc migration proc
-- + "name" : "ad_hoc_migration_proc_1",
-- + "version" : 1
@schema_ad_hoc_migration(1, ad_hoc_migration_proc_1);

-- TEST: another ad-hoc migration proc
-- + "name" : "ad_hoc_migration_proc_2",
-- + "version" : 2
@schema_ad_hoc_migration(2, ad_hoc_migration_proc_2);

-- TEST: make sure we can walk dependencies from a view to a table
-- + "usesTables" : [ "Foo" ],
-- + "fromTables" : [ "Foo" ],
create proc use_view()
begin
  select * from MyView;
end;

-- TEST: make sure that null values in the attributes are lowercase in json
-- +  "value" : [["dummy_test", ["Foo", ["id", "name"], [1, null], [2, "hi"]]]]
@attribute(cql:autotest=((dummy_test, (Foo, (id, name), (1, null), (2, "hi")))))
create proc null_attribute()
begin
  select * from Foo;
end;

-- TEST: verify collate and checkExpression
-- + "collate" : "bar"
-- + "checkExpr" : "id >= '_' AND id <= 'zzzzz'",
-- + "checkExprArgs" : [  ]
create table with_collate_and_check
( 
 id text collate bar check (id >= '_' and id <= 'zzzzz')
);

-- TEST: generate some enums in real form
-- + "name" : "some_reals",
-- + "type" : "real",
-- + "name" : "one",
-- + "value" : 1.000000e+00
-- + "name" : "e",
-- + "value" : 2.718000e+00
-- + "name" : "pi",
-- + "value" : 3.140000e+00
declare enum some_reals real (
  one = 1.0,
  e = 2.718,
  pi = 3.14
);

-- TEST: generate some enums in long form
-- + "name" : "some_longs",
-- + "type" : "long",
-- + "name" : "neg",
-- + "value" : -1000
-- + "name" : "pos",
-- + "value" : 17592454483968
declare enum some_longs long (
  neg = -1000,
  pos = 0x100010001000
);

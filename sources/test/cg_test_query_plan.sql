/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST: query plan
-- +1 DECLARE SELECT FUNC is_declare_func_enabled () BOOL NOT NULL;
-- +1 DECLARE SELECT FUNC is_declare_func_wall (id LONG_INT) BOOL NOT NULL;
-- + CREATE PROC create_schema()
-- + CREATE TABLE t1
-- + CREATE TABLE t2
-- + CREATE TABLE t3
-- + CREATE TABLE t4
-- + CREATE TABLE t5
-- + CREATE TABLE scan_ok
-- + CREATE TABLE sql_temp
-- + CREATE TABLE plan_temp
-- + CREATE TABLE no_table_scan
-- + CREATE TABLE ok_table_scan
-- + CREATE TABLE foo
-- + CREATE TABLE foo_
-- + CREATE TABLE _foo
-- +17 CREATE PROC populate_query_plan_%()
-- + CREATE PROC populate_table_scan_alert_table(table_ text not null)
-- + CREATE PROC populate_b_tree_alert_table()
-- + CALL populate_table_scan_alert_table(C.table_name);
-- + CALL populate_b_tree_alert_table()
-- +17 INSERT INTO sql_temp(id, sql)
-- +17 INSERT INTO plan_temp(sql_id, iselectid, iorder, ifrom, zdetail) VALUES(%, C.iselectid, C.iorder, C.ifrom, C.zdetail);
-- +17 DECLARE C CURSOR FOR EXPLAIN QUERY PLAN
-- +1 INSERT INTO ok_table_scan(sql_id, proc_name, table_names) VALUES(%, "use_ok_table_scan_attr", "#scan_ok#,#t3#");
-- + CREATE PROC print_sql_statement(sql_id integer not null)
-- + CREATE PROC print_query_plan_stat(id_ integer not null)
-- + CREATE PROC print_query_plan_graph(id_ integer not null)
-- + CREATE PROC print_query_plan(sql_id integer not null)
-- + CREATE PROC print_query_violation()
-- + CALL print_sql_statement(sql_id);
-- + CALL print_query_plan_stat(sql_id);
-- + CALL print_query_plan_graph(sql_id);
-- + CREATE PROC query_plan()
-- + CALL create_schema();
-- +17 CALL populate_query_plan_%();
-- + CREATE PROC populate_no_table_scan()
-- +1  INSERT OR IGNORE INTO no_table_scan(table_name)
-- +1  INSERT OR IGNORE INTO table_scan_alert
-- + SELECT ifnull(nullable(1), 42) AS nullable_result;
-- + CREATE PROC split_commas (str text)
-- + CREATE PROC ids_from_string (str text)
-- + I (id) AS (CALL ids_from_string('1')),
-- + E (id) AS (CALL ids_from_string('1'))
-- - Error
@attribute(cql:no_table_scan)
create table t1(id int primary key, name text);
@attribute(cql:no_table_scan)
create table t2(id int primary key, name text);
create table t3(id int primary key, name text);
create table t4(id long int primary key autoincrement, data blob);
create table t5(id long int, foreign key (id) references t4(id) on update cascade on delete cascade);
create table t6(id int primary key, name text) @delete(1);
@attribute(cql:no_table_scan)
create table scan_ok(id int);
@attribute(cql:no_table_scan)
create table foo(id int);
create table _foo(id int);
create table foo_(id int);
create index it1 ON t1(name, id);
create index it4 ON t4(data, id);
create index it5 ON t4(data) @delete(1);
create view my_view as select * from t1 inner join t2 using(id);
declare function any_func() bool not null;
declare select function is_declare_func_enabled() bool not null;
declare select function is_declare_func_wall(id long integer) bool not null;
declare select function array_num_at(array_object_ptr LONG NOT NULL, idx integer not null) long;
declare function blob_from_string(str text) create blob not null;
declare timer_var int;
declare label_var text;
declare data_var blob;
set timer_var := 1;
set label_var := 'Eric';
set data_var := blob_from_string('1');
create trigger my_trigger
  after insert on t1 when is_declare_func_enabled() and (is_declare_func_wall(new.id) = 1)
begin
  delete from t2 where id > new.id;
end;

create trigger my_trigger_deleted
  after insert on t1 when is_declare_func_enabled() and (is_declare_func_wall(new.id) = 1)
begin
  delete from t2 where id > new.id;
end @delete(1);

create virtual table virtual_table using module_name(this, that, the_other) as (
  id integer,
  t text
);

declare select function select_virtual_table(b text) (id long int, t text);

-- Proc with SELECT stmt
create proc sample()
begin
  select * from t1
    where name = 'Nelly' and
    id IN (
      select id from t2
        where id = timer_var
      union
      select id from t3
    )
    order by name asc;
end;

-- SELECT stmt
select is_declare_func_wall(id) from t4 where data = data_var;

-- UPDATE stmt
update t1 set id = 1, name = label_var where name in (select NAME from t3);

-- DELETE stmt
delete from t1
  where name in (
    select t2.name
      from t2 inner join t3 using(name)
  );

-- [WITH ... DELETE] stmt
with
  some_cte(name) as (
    select t2.name from t2 inner join t3 using(id)
  )
  delete from t1 where name not in (select * from some_cte);

-- INSERT stmt
insert into t1 select * from t2 union all select * from t3;

-- [WITH... INSERT] stmt
with some_cte(id, name) as (select 1, 'x')
insert into t1 select * from some_cte;

-- BEGIN stmt
begin transaction;

-- UPSERT stmt
insert into t1(id, name) values(1, 'Irene') on conflict(id) do update set name = excluded.name || 'replace' || ' • ' || '\x01\x02\xA1\x1b\x00\xg' || 'it''s high noon\r\n\f\b\t\v' || "it's" || name;

-- COMMIT stmt
commit transaction;

-- DROP TABLE stmt
drop table if exists t1;

-- DROP VIEW stmt
drop view my_view;

-- DROP INDEX stmt
drop index it1;

-- DROP TRIGGER stmt
drop trigger if exists my_trigger;

-- [WITH ... SELECT] stmt
with
  some_cte(name) as (
    select t2.name from t2 inner join t3 using(id)
  )
  select * from some_cte;

-- Object type in stmt
-- + SELECT array_num_at(ptr(cast('1' as object)), id) AS idx
create proc read_object(sync_group_ids_ object not null)
begin
  select array_num_at(ptr(sync_group_ids_), id) as idx from t1;
end;

-- ok_table_scan attr
@attribute(cql:ok_table_scan=(scan_ok, t3))
create proc use_ok_table_scan_attr()
begin
  select * from scan_ok;
end;

-- test no table scan on "foo_", "_foo" but should be on "foo"
create proc table_name_like_t1()
begin
  select 1 as n from foo_, _foo;
end;

create proc nullable_variables_remain_nullable(a int)
begin
  -- analysis of this would fail if `a` were replaced with a value of a nonnull
  -- type when generating the query plan
  select ifnull(a, 42) as nullable_result;
end;

create table C(
 id integer not null,
 name text);

@attribute(cql:shared_fragment)
CREATE PROC split_commas(str text)
BEGIN
  WITH splitter(tok, rest) AS (
    SELECT "", IFNULL(str || ",", "")
    UNION ALL
    SELECT
      substr(rest, 1, instr(rest, ",") - 1),
      substr(rest, instr(rest, ",") + 1)
    FROM splitter
    WHERE rest <> "")
  SELECT tok FROM splitter WHERE tok <> "";
END;

@attribute(cql:shared_fragment)
CREATE PROC ids_from_string(str text)
BEGIN
  WITH toks(tok) AS (CALL split_commas(str))
  SELECT CAST(tok AS LONG) AS id FROM toks;
END;

CREATE PROC use_shared(inc_ text not null, exc_ text not null)
begin
  WITH
  I(id) as (call ids_from_string(inc_)),
  E(id) as (call ids_from_string(exc_))
  select C.* from C
  where C.id in (select * from I)
  and C.id not in (select * from E);
end;

@attribute(cql:shared_fragment)
@attribute(cql:query_plan_branch=011)
CREATE PROC frag1(x int)
BEGIN
  IF x == 2 THEN
    SELECT 1 a;
  ELSE
    SELECT 2 a;
  END IF;
END;

@attribute(cql:shared_fragment)
@attribute(cql:query_plan_branch=4)
CREATE PROC frag2(y int)
BEGIN
  IF y == 2 THEN
    SELECT 10 b;
  ELSE IF y == -1 THEN
    SELECT 20 b;
  ELSE IF y == 0 THEN
    SELECT 30 b;
  ELSE IF y == 3 THEN
    SELECT 40 b;
  ELSE
    SELECT 50 b;
  END IF;
END;

@attribute(cql:shared_fragment)
@attribute(cql:query_plan_branch=1)
CREATE PROC frag3(z int)
BEGIN
  IF z == 2 THEN
    SELECT 100 c;
  ELSE
    SELECT 200 c;
  END IF;
END;

-- proc call a virtual table
CREATE PROC call_virtual_table()
BEGIN
  select id, t from select_virtual_table("abc");
END;

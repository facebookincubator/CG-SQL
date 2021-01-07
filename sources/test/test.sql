/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- *  note to readers
-- *
-- *  This file contains test cases for the parser only
-- *  as such there are many things here that can be parsed legally
-- *  but are semantically invalid or meaningless.
-- *
-- *  The purpose of these test cases is to exercise all the legal
-- *  parse paths only so don't worry about that business.
-- *
-- *  You cannot expect correct output if you also add the --sem flag to cql
-- *  it will start to whine about all the mistakes.

-- create very simple table
create table foo(id int);

-- create temp table
create temp table foo(id int);

-- create table with if not exists
create table if not exists foo (id int);

-- create temp table with if not exists
create temp table if not exists foo(id int);

-- create temp table with like
create temp table temp_foo(like foo);

-- create tiny table
create table foo(id int);

-- create table with 3 fields
create table foo(id int, name text, rate long int);

-- create table with not null
create table foo(id int not null, name text, rate long int);

-- create table with auto inc
create table foo(id int not null primary key autoincrement, name text, rate long int);

-- create table with primary key
create table foo(id int not null, name text primary key, rate long int);

-- create table with default
create table foo(id int not null, name text primary key, rate long int default 22);

-- create table using without rowid
create table foo(id int not null, name text primary key, rate long int default 22) without rowid;

-- create table with bool
create table foo(id bool);

-- create table with primary key as it's own row
create table foo(id int not null, name text, rate long int, primary key (id, name));

-- create table with foreign key
create table foo(id int not null, name text, rate long int, primary key (id, name), foreign key (id, name) references baz(id, name2) );

-- create table with fk attributes
create table foo(id int, foreign key (name) references baz(name2) on update cascade);

-- create table with int default value
create table T1(C11 int not null, C12 int default 37);

-- create table whith mixed attributes
create table T2(C21 int not null, C22 int default 93, primary key (a,b,c));

-- create table with more mixed attributes
create table T(C1 int not null, C2 real, C3 text);

-- create index
create index __idx__participants__thread_key__contact_id on participants (thread_key, contact_id);

-- basic binary expression
select 2 * 3;

-- basic from clause
select a from b;

-- select list with more than one item
select a, b from c;

-- select list with aliases
select a as x, b as y from c;

-- select with where
select a, b from c where d = 1;

-- select with distinct
select distinct a, b from c;

-- select with simple join syntax
select a b, c d from e;

-- select with explicit join
select a, b from c join d;

-- select with explicit join and on clause
select a, b from c join d on e = f;

-- select with explicit join and on clause with table names specified
select a, b from c join d on e.f = g.h;

-- select with left outer join
select a, b from c left outer join d on e.f = g.h;

-- select with many tables and columns
select P.thread_key, P.contact_id, P.is_admin, P.nickname, P.read_watermark_timestamp_ms, P.delivered_watermark_timestamp_ms, C.profile_picture_url, C.profile_picture_fallback_url, C.profile_picture_url_expiration_timestamp_ms, C.name, C.is_messenger_user
from participants P
left outer join contacts C on C.id = P.contact_id;

-- select with more aliasing options
select T.thread_key,
       T.thread_type,
       UTN.thread_name as thread_name,
       T.thread_picture_url,
       T.mute_expire_time_ms
from threads T
left outer join _unified_thread_name UTN on UTN.thread_key = T.thread_key;

-- select *
select * from foo;

-- select simple expression with parens (not a nested select)
select (a) from b;

-- select with slightly trickier expression forms
select T.thread_key,
         T.thread_type,
         (UTN.thread_name) as thread_name,
         T.thread_picture_url,
         T.mute_expire_time_ms
  from threads T
  left outer join _unified_thread_name UTN on UTN.thread_key = T.thread_key;

-- select with inequality
select 2 <> 3;

-- select with equality
select x = '';

-- select logical and
select 1 and 2;

-- select with group by
select a, b from c group by f;

-- select with order by
select a, b from c order by f;

-- select with more complex order by clauses
select C.id as contact_id, C.contact_type, C.profile_picture_url, C.profile_picture_fallback_url, C.profile_picture_url_expiration_timestamp_ms, C.name, C.rank
from contacts C
where C.name is not null and C.name <> ''
order by C.name;

-- select with is null
select y is null;

-- select variable
select zz;

-- assign variable
set zz := 123;

-- select with function call
select A_Func(a, 3);

-- case with no initial expression
select case when x = 2 then 3 else 4 end;

-- case with more  alternations
select case when x = 1 then 2 when x = 3 then 4 else 5 end;

-- case with nested select
select case when a = 2 then 3 when b = 4 then (select c from d where e = f) else g end as h;

-- compound crazy select with lots of when and else from lightspeed
select T.thread_key,
         case when count(P.contact_id) > 1 then group_concat(coalesce(P.nickname, C.first_name, C.name, 'Facebook User'), ', ')
                         when count(P.contact_id) = 1 then group_concat(coalesce(P.nickname, C.name, 'Facebook User'), ', ')
                         else (select name from contacts where id = UI.facebook_user_id) end as thread_name,
         case when T.thread_type = 2 then null when T.thread_key = UI.facebook_user_id then (select profile_picture_url from contacts where id = UI.facebook_user_id)
                         else C.profile_picture_url end as other_participant_profile_picture_url,
         case when T.thread_type = 2 then null when T.thread_key = UI.facebook_user_id then (select profile_picture_fallback_url from contacts where id = UI.facebook_user_id)
                         else C.profile_picture_fallback_url end as other_participant_profile_picture_fallback_url,
         case when T.thread_type = 2 then null when T.thread_key = UI.facebook_user_id then (select profile_picture_url_expiration_timestamp_ms from contacts where id = UI.facebook_user_id)
                             else C.profile_picture_url_expiration_timestamp_ms end as other_participant_url_expiration_timestamp_ms
  from _user_info UI
  inner join threads T
  left outer join participants P on P.thread_key = T.thread_key and P.contact_id <> UI.facebook_user_id
  left outer join contacts C on C.id = P.contact_id
  where UI.id = 1
  group by T.thread_key;

-- simple create view
create view a as select b from c;

-- simple create view
create view if not exists qq as select b from c;

-- more complex create view
create view _self_thread_name as
  select UI.facebook_user_id as thread_key,
         C.name as thread_name
  from _user_info UI, contacts C
  where UI.facebook_user_id = C.id;

-- simple delete
delete from a;

-- delete with where clause
delete from a where b > c and e <= f;


-- simple insert
insert into _sync_status values (1, null, null);

-- assorted weird expression cases for coverage, mixed floats and logical
select a.b - ((c('now') - 2440587) * 86400000) > 0 or d < 0;

-- complex logical combination
select (T.mute_expire_time_ms - ((julianday('now') - 2440587.5) * 86400000) > 0 or T.mute_expire_time_ms < 0);

-- real constant
select 12.3;

-- view with left outer joins and distinct
create view zzz as
  select distinct a, b as c, d
  from e E
  left outer join f F on F.g = e.g
  left outer join x X on y.z = q.w and h.j = k.l
  order by o.p desc;

-- complex view from lightspeed
create view thread_messages as
  select distinct M.message_id, M.thread_key, M.offline_threading_id, M.sender_id,
  M.send_status, C.name, C.profile_picture_url, C.profile_picture_fallback_url,
  C.profile_picture_url_expiration_timestamp_ms,
  M.body,
  M.sticker_id, M.timestamp_ms,
  (A.has_xma or A.has_media) as has_attachment, M.is_admin_message
  from messages M
  left outer join contacts C on C.id = M.sender_id
  left outer join attachments A on A.message_id = M.message_id and A.thread_key = M.thread_key
  order by M.timestamp_ms desc;


-- simple select with limit
select * from threads order by last_activity_timestamp_ms desc limit 20;

-- simple select with limit and offset
select * from threads order by last_activity_timestamp_ms desc limit 20 offset 7;

-- nested select in from clause
select a from (select b from c) X;

-- complex create view with limit and lots of math
create view unread_not_muted_threads_count as
  select count(*) as count from
  (select * from threads
    order by last_activity_timestamp_ms desc
    limit 20) T
  where T.last_activity_timestamp_ms > T.last_read_watermark_timestamp_ms
  and (T.mute_expire_time_ms >= 0
  and T.mute_expire_time_ms - strftime('%s', 'now') * 1000 <= 0)
  and T.folder_name == 'inbox';


-- complex table create from lightspeed
create table pending_tasks (
  task_id                           integer primary key autoincrement,
  queue_name                        text not null,
  task_value                        text not null,
  context                           text,
  attempt_count                     int not null default 0,
  enqueue_timestamp_ms              long int not null default 0,
  first_executed_timestamp_ms       long int not null default 0,
  last_executed_timestamp_ms        long int not null default 0,
  next_retry_timestamp_ms           long int not null default 0,
  failure_count                     int not null default 0
);


-- simple table from lightspeed
create table pending_query_changes (
  id                                integer primary key autoincrement,
  query_id                          int unique not null
);

-- simplest update
update a set b = 1;

-- multi-column update
update a set x = 1, y = 2 where z = 3;

-- use == syntax for coverage
update a set x = 1, y = 2 where z == 3;

-- simple set
set a1 := 20;

-- set with real constant
set b3 := 40.75;

-- simple procedure with a couple of statements
create procedure a()
begin
  insert into d values (e, f);
  update g set h=j, k=l;
end;

-- procedure with arguments
create procedure a(b int, c text)
begin
  insert into d values (e, f);
  update g set h=j, k=l;
end;

-- proc with in/out args
create procedure a(b int, in c text, out d long, inout f text)
begin
  insert into d values (e, f);
  update g set h=j, k=l;
end;

-- simple open
open x;

-- simple close
close y;

-- simple continue
continue;

-- simple leave
leave;

-- simple declare
declare a, b, c int;

-- simple cursor declare
declare a cursor for select b from c;

-- open cursor
open a;

-- loop over cursor
loop fetch a into b
begin
  insert into d values(e, f);
  fetch a into x, y;
  leave;
  continue;
end;

-- close cursor
close a;

-- simple if statement
if x > 20 then
  update x set y = z;
end if;

-- if with else
if x > 20 then
  update x set y = z;
else
  update w set k = m;
end if;

-- if with elseif
if a > 1 then
  delete from b;
else if c > 2 then
  delete from d;
else if d > 3 then
  delete from e;
else
  delete from f;
end if;

-- use of 'in'
if 1 in (1,2) then
  delete from b;
else
  delete from c;
end if;

-- use of 'not in' expression list
if 1 not in (1,2) then
  delete from b;
else
  delete from c;
end if;

-- use of 'not in' select statement
if 1 not in (select 1) then
  delete from b;
else
  delete from c;
end if;

-- use of 'like'
if 'x' like 'y' then
  delete from b;
end if;

-- use of between
if 1 between 0 and 2 and 3 in (3, 4) then
  delete from b;
end if;

-- use of not between
if 3 not between 1 and 2 and 1 not in (2, 3) then
  delete from b;
end if;

-- this has to parse as unary minus and then postive 1
select -1;

-- simple sum
select 1+2;

-- product, should echo no parens
select 1+2*3;

-- abonormal order output should preserve parens
select (1+2)*3;

-- abonormal order output should preserve parens
select -1+2*(-3+4);

-- abonormal order output should preserve parens but only one set
select (1+2)*3-4;

-- cannot be parsed as anything other than subtraction
select 1- 2;

-- this could look like two adjectent literals but it isn't
-- this is why -2 must be parsed as unary minus 2 and not a literal -2
select 1-2;

-- what follows is a lot of cases that validate order of operations and ensure that we emit parens where needed
-- there are several dozen
select x;
select x.y+1;
select 3*x.y+1;
select 3*(x+y+1);
select (3);
select (5*5)+3;
select 'x';
select '''x'''+2;
select foo(1);
select foo((1+3)*4, 2/7, 5%3);
select 1==2;
select 1!=2;
select 1<=2;
select 1>=2;
select 1>2;
select 1<2;
select 3 between 4 and 3;
select 3+7 between 4 and 3;
select 3 not between 4 and 3;
select 3+7 not between 4 and 3;
select 1<2 and 2>3;
select 1<(2 and 2)>3;
select not (1+2);
select not (1 and 2);
select not 1 and 2;
select 1 and 2 and 3 and 4;
select 1 and (2 or 3) or  4;
select 1 and (9 and 10) or  4;
select 1, (select 2);
select x is null;
select x is not null;
select x is not null and y is not null;
select x is not null and y or z is not null;
select x is not null and (y or z) is not null;
select x is not null and (y + z) is not null;
select x is not null and y + z is null;
select 1 + not x is null;
select 1 + (not x) is null;
select x is not null and y + (not z) is null;
select x is not null and y + not z is null;
select x + (not z is null);
select x + not z is null;
select x + not ( z is null );
select 1 between 0 and 2 and 3 in (3, 4);
select 1 between 0 and (2 and 3 in (3, 4));
select 1 between 0 and (2 and 3) in (true, false);
select 1 not between 0 and 2 and 3 not in (3, 4);
select 1 not between 0 and (2 and 3 not in (3, 4));
select 1 not between 0 and (2 and 3) not in (true, false);
select 1 + (2 and 3) in (true, false);
select 1 + 3 in (3, 4);
select 1 + (3 in (3, 4));
select 1 and ( 3 in (3,4));
select 1 and 3 in (3,4);


-- alternate inner join syntax
select x, y from T inner join Y using (a,b,c);

-- update with where and limit
update X set x = 1, y = 2 where z = 5 limit 3;

-- having clause
select * from foo  where x = y group by x having z = w;

-- distinctrow
select distinctrow x from goo;

-- select all test
select all x,y from foo;

-- all the join types

-- plain join
select a, b from X join Y;

-- plain left
select a, b from X left join Y;

-- right join
select a, b from X as XX right join Y;

-- right outer join
select a, b from X right outer join Y;

-- left outer join
select a, b from X left outer join Y;

-- cross join
select a, b from X cross join Y;

-- case with expression specified and else
select case 1
when 1 then 'a'
when 2 then 'b'
else 'c'
end;

-- simplified join syntax
select x from (A, B, C);

-- asc in order by
select x from A order by x asc;

-- desc in order by
select x from A order by x desc;

-- simple group by
select x,y from A group by a,b;

-- update with order by
update bar set x = 1 order by a limit 5;

-- call with no args
call foo();

-- call with args
call foo(2);

-- proc with try catch
create proc foo()
begin
  begin try
    select 'try';
  end try;
  begin catch
    select 'catch';
  end catch;
end;

-- control statements
begin transaction;
begin deferred transaction;
begin immediate  transaction;
begin exclusive  transaction;
begin deferred;
begin immediate;
begin exclusive;
begin;
commit transaction;
commit;
savepoint foo;
release savepoint foo;
release foo;
rollback;
rollback transaction;
rollback transaction to savepoint foo;
rollback to savepoint foo;
rollback transaction to foo;
rollback to foo;
rollback transaction to savepoint @proc;
rollback to savepoint @proc;
rollback transaction to @proc;
rollback to @proc;

-- new cursor construct
declare test cursor for call foo(1,2);

-- select with exists
select * from foo where exists (select * from bar);

-- select with exists
select * from foo where not exists (select * from bar);

-- simplified cursor syntax
fetch my_cursor;

-- nested selects
select a from (X inner join Y) left outer join (W inner join Q);

-- table refs
select a from (X inner join Y) left outer join (W inner join Q);

-- more flexible group_by format
select a, b from x group by a asc, b desc;

-- simple cast expression
select cast(1 as text);

-- declare procedure
declare proc decl1(id integer);

-- declare procedure with DB params
declare proc decl2(id integer) using transaction;

-- declare procedure with select output
declare proc decl3(id integer) ( A integer not null, B bool);

-- declare procedure with fetch output
declare proc decl3(id integer) out ( A integer not null, B bool);

-- an out proc that uses the database
declare proc decl4(id integer) out ( A integer not null, B bool) using transaction;

-- an enumeration
declare enum things integer (
  foo,
  bar,
  baz = 3 + 1,
  bing
);

-- make a compound select statement
select 1, 2, 3
union
select 4, 5, 6;

-- make a compound select statement
select 1, 2, 3
union all
select 4, 5, 6;

-- make a compound select statement with nested compound union all
select 1, 2, 3
union all
select 4, 5, 6
union all
select 7, 8, 9;

-- make a compound select statement with nested compound union
select 1 a, 2, 3
union
select 4 a, 5, 6
union
select 7 a, 8, 9
order by a
limit 2
offset 3;

-- use nullable in a select
select nullable(1);

-- basic with clause
with x(a, b) as (select 1,2)
select a, b from x;

-- more complex with clause
with x(a, b) as (select 1,2), y(c,d) as (select 5.4, 7.3)
select a, b from x inner join y on x.a = y.c;

-- nested with clause
with x(a,b) as (select 1,2)
select * from x as X
inner join ( with y(a,b) as (select 1,3) select * from y ) as Y
on X.a = y.A;

-- with resursive case for counting
with recursive
  cnt(x) as (
     select 1
     union all
     select x+1 from cnt
     limit 20
  )
select x from cnt;

-- table.* syntax form
select T.* from (select 1) as T;

-- declare function syntax
declare function foo(x integer, y text not null) integer not null;

-- declare an object variable
declare obj_var object;

-- declare a function that returns a created object
declare function createobj() create object;

-- declare a function that returns a created object
declare function createobj() create object not null;

-- echo code to the output file
@echo c, "foo\n";

-- typed object variable
declare foo_obj object<Foo>;

-- valid utf8 function
select valid_utf8_or_null('xxx');

-- insert with column names
insert into foo (a,b) values (1,2);

-- insert or replace
insert or replace into foo (a,b) values (1,2);

-- insert or ignore
insert or ignore into foo (a,b) values (1,2);

-- temporary view
create temp view foo as select 1 as A;

-- alter table add column
alter table foo add column bar integer not null;

-- table with column create version attribute
create table foo (id int @create(1, Foo)) ;

-- table with column delete  version attribute
create table foo (id int @delete(2, Bar));

-- table with column create and delete version attribute
create table foo (id int @create(1, Foo) @delete(2, Bar));

-- table with column create and delete version attribute (no procs)
create table foo (id int @create(1) @delete(2));

-- declare schema version
@schema_upgrade_version (112);

-- declare a table with attributes
create table foo(id integer) @create(1, foo) @delete(2, bar);

-- drop a table
drop table foo;

-- drop a table with check
drop table if exists foo;

-- set previous schema mode
@previous_schema;

-- use text as a name
select text from foo;

-- force emit of script upgrade instruction
@schema_upgrade_script;

-- create a view with versioning
create view foo as select 1 from bar @delete(2);

-- use a long constant
select 3147483647L;

-- try to parse drop view
drop view foo;

-- index with attributes
create index foo on bazzle(goo) @delete(7);

-- drop an index
drop index foo;

-- insert with dummy values
insert into foo(x, y) values(1,2) @dummy_seed(1 + 2*3) @dummy_defaults @dummy_nullables;

-- all defaults insert
insert into foo() values();

-- use a blob the simplest way
declare foo blob;

-- trickier usage
declare function foo(x blob not null) create blob not null;

-- out cursor statement
out my_cursor;

--  parse fetch from a call
declare C cursor fetch from call x();

-- like above, but for a cursor declared elsewhere
fetch C from call x();

-- parse fetch from values, both forms

fetch C from values(1,2,3);
fetch C(id, x) from values (1.3, 4);
fetch C(x, y) from values(1,2) @dummy_seed(1 + 2*3) @dummy_defaults @dummy_nullables;
fetch C using 1 a, 2 b @dummy_seed(1 + 2*3) @dummy_defaults @dummy_nullables;


-- declare a cursor of the type of another cursor
declare C0 cursor like C1;

-- declare a cursor of the type of a select result
declare C cursor like select 1 A, 2.5 B;

-- test C style literals
set x := "this is a test";

set x := "\"this is a test\" '\n";

@attribute( foo )
@attribute( foo = 1 )
@attribute( foo = bar )
@attribute( foo = 'bazzle' )
@attribute( foo = ('bazzle', (2.5, 'razzle'), foo) )
create table bar(
@attribute(cute)
@attribute(smart)
@attribute(coolstuff:main=1)
@attribute(coolstuff:alt=1)
id integer);

-- TEST: complex index
-- + {create_index_stmt}: ok
-- + {name my_unique_index}
-- + {name bar}
-- + {flags_names_attrs}
-- + {int 3}
-- + {name id}: id: integer notnull
-- + {asc}
-- + {name name}: name: text
-- + {desc}
-- + {name rate}: rate: longint
create unique index if not exists my_unique_index on foo(id asc, name desc, zone);

-- create table with exotic foreign keys
create table foo(
id int not null,
name text,
rate long int,
primary key (id, name),
foreign key (id, name) references baz(id, name2)
   on update cascade
   on delete no action
   not deferrable initially deferred,
foreign key (rate) references rater(r)
   on delete set default
   on update restrict
   deferrable initially immediate,
foreign key (name) references name_thing(n)
   on update set null
   on delete set default
   deferrable initially immediate
);

-- create table with FK in a column
create table foo (
  id integer references goo ( another_id) on update cascade on delete set null
);

-- exercise all the default casese
create table lots_o_defaults
(
  c1 integer default 2,
  c2 integer default -7,
  c3 real default 2.5,
  c4 text default '''',
  c5 text default "xyzzy"
);

-- create a table using recreate
create table foo (id integer) @recreate;

-- use with CTE in an insert statement
with x(a,b) as (select 111,222)
insert into foo values ( (select a from x), (select b from x) );

with recursive x(a,b) as (select 111,222)
insert into foo(a,b) values ( (select a from x), (select b from x) );

insert into foo() select 1,2,3 from bar;

declare select function foo(id integer) integer not null;

fetch a_cursor from arguments;

fetch a_cursor from arguments(like some_table);

fetch a_cursor(x) from arguments @dummy_seed(7) @dummy_defaults;

fetch to_cursor from from_cursor;

insert into foo(a,b) from arguments;

create proc x( like table_name, id integer)
begin
  declare x integer;
end;

select x match y;

select 1 | 2 & 3;

select 1 & (~2 | 3);

select 1 << 2 + 1 >> 4;

select 1 || 2;

select 'a' || 'b';

select 'x' GLOB 'y';

select 'x' REGEXP 'y';

select 'x' MATCH 'y';

create temp trigger if not exists trigger1
   before delete on target_table1
   for each row
   when complex_when_expression
begin
  select a_result;
end;

create temp trigger if not exists trigger1
   delete on target_table1
   for each row
   when complex_when_expression
begin
  select a_result;
end;

create trigger trigger2
   after insert on target_table2
begin
  delete from foo where id = 1;
end;

create trigger trigger3
   instead of update on target_table3
begin
  update goo set foo = bar where miz = wiz;
  insert into stew values (1);
end;

create trigger trigger4
   instead of update of x, y, z on target_table4
begin
  select 1;
end;

-- try to parse drop trigger
drop trigger foo;

drop trigger if exists foo;

select raise(ignore);
select raise(fail, 'fail it');
select raise(abort, 'abort it');
select raise(rollback, 'roll it back');

@attribute(cql:identity=(id))
create proc simple_identity()
begin
  select 1 as id, 2 as data;
end;

@attribute(cql:identity=(col1, col2))
create proc complex_identity()
begin
  select 1 as col1, 2 as col2, 3 as data;
end;

create table with_sensitive(
 id integer,
 name text @sensitive
);

with x(a, b) as (select 1,2)
delete from foo where foo.id in (select * from x);

with x(a, b) as (select 1,2)
update foo set xyzzy = 7 where foo.id in (select * from x);

@enforce_strict foreign key on update;

@enforce_normal foreign key on delete;

@enforce_strict join;

@enforce_normal join;

@enforce_strict upsert statement;

@enforce_normal upsert statement;

@declare_schema_region foo;
@declare_schema_region bar using foo;
@declare_schema_region baz using foo, bar;
@declare_schema_region goo using foo, bar, baz private;
@declare_deployable_region dep1;
@declare_deployable_region dep2 using foo, bar;
@begin_schema_region foo;
@end_schema_region;

declare select function garbonzo(id integer) (t text);

select * from garbonzo(1,2,3) T1 inner join xyzzy() T2 on T1.id = T2.id;

create trigger deleted_trigger
   after insert on target_table2
begin
  delete from foo where id = 1;
end @delete(100);

insert into foo select C11 from T1 where 1 on conflict(id) do nothing;

insert into foo(id) values(1) on conflict(id) where id=10 do update set id=id+1 where id=20;

insert into foo(id) values(1) on conflict do nothing;

select 10*a as T where T = 1;

explain query plan select * from foo;

explain select 1;

@schema_ad_hoc_migration(11, YourProcHere);

create proc emit_several_rows()
begin
   declare C cursor like select 1 x, "2" y;
   fetch C from values(1, "2");
   out union C;
   out union C;
end;

create proc compound_select_expr()
begin
  declare x integer;
  set x := (select 1 where 0 union select 2);
end;

select id, row_number() filter (where 1) over (range between id >= 70 preceding and id < 100 following exclude no others) as row_num from foo;

select id, row_number() over win1, rank() over win2
from foo
window win1 as (),
       win2 as ()
order by id;

create proc foo(x integer, y integer)
begin
  insert into bar(x,y) from arguments @dummy_seed(1) @dummy_nullables;
end;

update cursor d(a, b) from values (1, 'x');

update cursor d using 1 a, 'x' b;

with foo(*) as (select 1 x, '2' y)
 select * from foo;

update cursor d(like x) from values (1, 2);

update cursor d(like x) from cursor zz;

fetch x(a,b) from cursor n @dummy_seed(1);

select count(distinct id) filter (where 1) from foo;

select 'x' collate nocase;

select nullif('a', 'b');

select char(1);

select x'1234abcd';

insert into foo default values;

declare proc p(like foo) (like bar);

call something(from a_cursor);

call something(from a_cursor like another);

call something(from arguments, from arguments);

call something(from arguments like something);

set x := foo(from arguments like X, from C like A, from X, from arguments);

create proc foo()
begin
  declare w text;
  set w := @PROC;
  savepoint @proc;
  release savepoint @proc;
  rollback transaction to savepoint @proc;
end;

declare obj object<foo cursor>;

declare C cursor for obj;

set X from cursor C;

declare C cursor like P2 arguments;

-- check column constraint
create table foo
(
 id integer collate bar check (id = 3 and goo = 5)
);

-- proc savepoint forms
create proc foo()
begin
  proc savepoint
  begin
     if 1 then
       rollback return;
     else
       commit return;
     end if;
  end;
end;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, SUM(amount) OVER
  (ORDER BY month) RunningTotal
FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 3 PRECEDING AND 4 FOLLOWING EXCLUDE CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 4 PRECEDING AND 5 FOLLOWING EXCLUDE GROUP)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 6 PRECEDING AND 7 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month RANGE BETWEEN 8 PRECEDING AND 9 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN 10 PRECEDING AND 11 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND 12 FOLLOWING EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN 13 FOLLOWING AND 14 PRECEDING)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (PARTITION BY something ORDER BY month GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW EXCLUDE TIES)
SalesMovingAverage FROM SalesInfo;

SELECT month, amount, AVG(amount) OVER
  (GROUPS CURRENT ROW)
SalesMovingAverage FROM SalesInfo;

-- trigger is a valid name
set trigger := 1;

declare x integer not null @sensitive;
declare x integer @sensitive not null;

set z := 1 between 1 & 2 and 3;
set z := 1 between 1 | 2 and 3;
set z := 1 between 1 << 2 and 3;
set z := 1 between 1 >> 2 and 3;
set z := 1 between 1 + 2 and 3;
set z := 1 between 1 - 2 and 3;
set z := 1 between 1 * 2 and 3;
set z := 1 between 1 / 2 and 3;
set z := 1 between 1 % 2 and 3;
set z := 1 between -1 and 3;
set z := 1 between 'x' || 'y' and 'z';

-- key is a valid name
set key := 3;

-- virtual is a valid name
set virtual := 3;


-- arg bundle shapes
create proc foo(x like this, y like that)
begin
end;

-- a simple virtual table form
create virtual table foo using bar(this, that, the_other) as (
  id integer,
  t text
);

create virtual table foo using bar as (
  id integer,
  t text
);

create virtual table foo using bar(arguments following) as (
  id integer,
  t text
);

create virtual table foo using bar(arguments following) as (
  id integer,
  t text
) @delete(5);

create table foo (
  id integer,
  v integer,
  check (v > 5),
  constraint goo check (v < 20)
);

@emit_enums;
@emit_enums foo;
@emit_enums foo, bar;

-- simple named type declaration
declare fbid type text @sensitive;

declare my_fbid fbid;


--- keep this at the end because the line numbers will be whack after this so syntax errors will be annoying...

# 1 "long/path/I/do/not/like"

set file := @FILE('path/');  -- take starting at path
set file := @FILE('');  -- keep the whole string
set file := @FILE('xxxx');  -- pattern not found, keep it all

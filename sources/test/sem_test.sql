/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST: basic test table with an auto inc field (implies not null)
-- + create_table_stmt% foo: % id: integer notnull primary_key autoinc
-- - Error
create table foo(
  id integer PRIMARY KEY AUTOINCREMENT
);

-- TEST: exact duplicate table is ok
-- + create_table_stmt% foo: % id: integer notnull primary_key autoinc
-- - Error
create table foo(
  id integer PRIMARY KEY AUTOINCREMENT
);

-- TEST: create a table using type discrimation: kinds
-- + {create_table_stmt}: with_kind: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> }
-- + {col_def}: id: integer<some_key>
-- + {col_def}: cost: real<dollars>
-- + {col_def}: value: real<dollars>
-- - Error
create table with_kind(
  id integer<some_key>,
  cost real<dollars>,
  value real<dollars>
);

-- useful in later tests
declare price_d real<dollars>;
declare price_e real<euros>;

-- TEST: second test table with combination of fields
-- + {create_table_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- - Error
create table bar(
  id integer not null,
  name text @create(2),
  rate LONG INT @create(2)
);

-- TEST: duplicate table name, creates error, will be ignored -- types will not be resolved due to early out
-- + Error % duplicate table/view
-- + {create_table_stmt}: err
create table foo(
  id integer
);

-- TEST: duplicate column names, creates error will be ignored
-- + Error % duplicate column name 'id'
-- + {create_table_stmt}: err
create table baz(
  id integer,
  id integer
);

-- TEST: ok to get ID from foo, unique
-- + select: { id: integer notnull }
-- - Error
select ID from foo;

-- TEST: make sure the type includes the kinds
-- + {select_stmt}: select: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> }
-- - Error
select * from with_kind;

-- TEST: classic join
-- + select: { id: integer notnull, name: text }
-- + JOIN { T1: foo, T2: bar }
-- - Error
select T1.id, name
from foo AS T1
inner join bar AS T2 ON T1.id = T2.id
where rate > 0;

-- TEST: alternate join syntax
-- + select: { name: text }
-- + {select_from_etc}: JOIN { foo: foo, bar: bar }
-- - Error
select name from foo, bar;

-- TEST: duplicate table alias in the join, error
-- + Error % duplicate table name in join 'T1'
-- + {select_stmt}: err
select name from foo T1, bar T1, bar T1;

-- TEST: ambiguous id in foo and bar
-- + Error % identifier is ambiguous 'id'
select id from foo, bar;

-- TEST: column not present
-- + Error % name not found 'not_found'
select not_found from foo, bar;

-- TEST: simple string select, string literals
-- + select: { _anon: text notnull }
-- - Error
select 'foo';

-- TEST: string add not valid, further adding 3 does not create new errors
-- + Error % left operand cannot be a string in '+'
-- + {select_stmt}: err
select 'foo' + 'bar' + 3;

-- TEST: correct like expression
-- + {like}: bool notnull
-- + select_stmt}: select: { _anon: bool notnull }
-- - Error
select 'foo' like 'baz';

-- TEST: correct not like expression
-- + {not_like}: bool notnull
-- + {select_stmt}: select: { _anon: bool notnull }
-- - Error
select 'foo' not like 'baz';

-- TEST: 1 is not a string
-- + Error % left operand must be a string in 'LIKE'
-- + {select_stmt}: err
select 1 like 'baz';

-- TEST: 1 is not a string in a "NOT LIKE" expr
-- + Error % left operand must be a string in 'NOT LIKE'
-- + {select_stmt}: err
select 1 not like 'baz';

-- TEST: 2 is not a string
-- + Error % right operand must be a string in 'LIKE'
-- + {select_stmt}: err
select 'foo' like 2;

-- TEST: correct concat strings
-- + {concat}: text notnull
-- + select_stmt}: select: { _anon: text notnull }
-- - Error
select 'foo' || 'baz';

-- TEST: correct concat string or number case one
-- + {concat}: text notnull
-- + select_stmt}: select: { _anon: text notnull }
-- - Error
select 'foo' || 1;

-- TEST: correct concat string or number case two
-- + {concat}: text notnull
-- + select_stmt}: select: { _anon: text notnull }
-- - Error
select 1.0 || 'baz';

-- TEST: converts to REAL
-- + {select_stmt}: select: { _anon: real notnull }
-- + {add}: real notnull
-- + {int 1}: integer notnull
-- + {dbl 2.0%}: real notnull
-- - Error
select 1 + 2.0;

-- TEST: stays integer
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {add}: integer notnull
-- + {int 3}: integer notnull
-- + {int 4}: integer notnull
-- - Error
select 3 + 4;

-- TEST: invalid addition of string to id
-- + Error % right operand cannot be a string in '+'
-- + {select_stmt}: err
select T1.id + 'foo' from foo T1;

-- TEST: invalid addition of id to string
-- + Error % left operand cannot be a string in '+'
-- {select_stmt}: err
select 'foo' + T1.id from foo T1;

-- TEST: boolean is flexible with numerics
-- + {and}: bool notnull
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- - Error
select 1 AND 2;

-- TEST: logical operators can include null, creates nullable bool
-- + {or}: bool
-- + {null}: null
-- + {int 1}: integer notnull
-- - Error
select null or 1;

-- TEST: logical operators can include null, creates nullable bool
-- + {and}: bool
-- + {null}: null
-- + {int 1}: integer notnull
-- - Error
select null and 1;

-- TEST: ok to add to a boolean
-- + {add}: integer notnull
-- + {eq}: bool notnull
-- - Error
select (1 == 2) + 1;

-- TEST: can't do a logical AND with a string
-- + Error % left operand cannot be a string in 'AND'
-- + {select_stmt}: err
select 'foo' and 1;

-- TEST: error prop handled correctly after invalid boolean
-- + Error % right operand cannot be a string in 'AND'
-- exactly one error --  OR does NOT get an error, just AND
-- +1 Error
-- + {or}: err
-- + {and}: err
-- + {int 1}: integer notnull
-- + {strlit 'foo'}
select 1 and 'foo' or 1;

-- TEST: can't compare string and number
-- + Error % incompatible types in expression '<'
-- + {lt}: err
select 'foo' < 1;

-- TEST: can't compare string and number
-- + Error % incompatible types in expression '>'
-- + {gt}: err
select 1 > 'foo';

-- TEST: string comparison is ok
-- + {ne}: bool notnull
-- + {strlit 'baz'}: text notnull
-- + {strlit 'foo'}: text notnull
-- - Error
select 'baz' != 'foo';

-- TEST: can't compare string and number, error prop ok.
-- + Error % incompatible types in expression '>'
-- +1 Error
-- + {gt}: err
-- + {select_stmt}: err
select 1 > 'foo' > 2;

-- TEST: foo unknown gives error, error doesn't prop through like
-- + Error % name not found 'foo'
-- - Error % LIKE
select foo like 'bar';

-- TEST: selecting negative ordinal (this has to be unary minus and 1)
-- + {uminus}: integer notnull
-- + {int 1}: integer notnull
-- not negative one
-- - {int -1}
-- - Error
select -1;

-- TEST: can't do unary minus on string
-- + Error % string operand not allowed in '-'
-- + {uminus}: err
select - 'x';

-- TEST: can't do NOT on strings
-- + {not}: err
-- + Error % string operand not allowed in 'NOT'
select NOT 'x';

-- TEST: real is ok as a boolean, it's truthy
-- + {not}: bool notnull
-- + {dbl 1.2%}: real notnull
-- - Error
select NOT 1.2;

-- TEST: non-null bool result even with null input
-- + {is}: bool notnull
-- + {null}: null
-- - Error
select null is null;

-- TEST: incompatible types: is
-- + {is}: err
-- + Error % incompatible types in expression 'IS'
-- +1 Error
select 'x' is 1.2;

-- TEST: non-null bool result even with null input
-- + {is_not}: bool notnull
-- + {null}: null
-- - Error
select null is not null;

-- TEST: unary math does not double report errors
-- + {uminus}: err
-- + Error % string operand not allowed in 'NOT'
-- exactly one error
-- +1 Error
select  - NOT 'x';

-- TEST: unary logical does not double report errors
-- + {not}: err
-- + Error % string operand not allowed in '-'
-- exactly one error
-- +1 Error
select NOT - 'x';

-- TEST: unary is null or is not null does not double report errors
-- + {is}: err
-- + Error % string operand not allowed in '-'
-- exactly one error
-- +1 Error
select (- 'x') is null;

-- TEST: negative boolean is ok
-- + {uminus}: integer notnull
-- + {not}: bool notnull
-- + {int 1}: integer notnull
-- - Error
select - NOT 1;

-- TEST: negative float is ok
-- + {uminus}: real notnull
-- + {dbl 1.2%}: real notnull
-- - Error
select - 1.2;

-- TEST: int*int -> int
-- + {mul}: integer notnull
-- - Error
select 1 * 2;

-- TEST: int-int -> int
-- + {sub}: integer notnull
-- - Error
select 3 - 4;

-- TEST: int / int -> int
-- + {div}: integer notnull
-- - Error
select 6 / 3;

-- TEST: int % int -> int
-- + {mod}: integer notnull
-- - Error
select 6 % 3;

-- TEST: int >= int -> bool
-- + {ge}: bool notnull
-- - Error
select 2 >= 1;

-- TEST: int <= int -> bool
-- + {le}: bool notnull
-- - Error
select 1 <= 2;

-- TEST: int == int -> bool
-- + {eq}: bool notnull
-- - Error
select 2 = 2;

-- TEST: select * produces correct tables joining foo and bar
-- - Error
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, name: text, rate: longint }
-- + {select_from_etc}: JOIN { foo: foo, bar: bar }
select * from foo, bar;

-- TEST: select expression alias to one, two works
-- - Error
-- + {select_stmt}: select: { one: integer notnull, two: integer notnull }
select 1 as one, 2 as two;

-- TEST: select * with no from is an error
-- + Error % select * cannot be used with no FROM clause
-- +1 Error
-- + {select_stmt}: err
-- + {star}: err
select *;

-- TEST: select where statement
-- + {select_stmt}: select: { T: integer notnull }
-- + {select_core}: select: { T: integer notnull }
-- + {select_expr_list_con}: select: { T: integer notnull }
-- + {select_expr_list}: select: { T: integer notnull }
-- + {select_from_etc}: ok
-- + {select_where}
-- - Error
select 10 as T where T = 1;

-- TEST: select where with a column specified
-- + {select_stmt}: err
-- + {name c}: err
-- + {select_from_etc}: ok
-- + Error % name not found 'c'
-- + Error
select c where 1;

-- TEST: select * from bogus table doesn't give more errors
-- + Error % table/view not defined 'goo'
-- +1 Error
-- + {select_stmt}: err
-- + {table_or_subquery}: err
select * from goo;

-- TEST: add a table with some big stuff
-- - Error
-- + {col_def}: l: longint
-- + {col_def}: r: real
create table big (
  l LONG integer,
  r REAL
);

-- TEST: create a long int
-- - Error
-- + {select_stmt}: select: { l: longint }
select l from big;

-- TEST: long * int -> long
-- - Error
-- + {select_stmt}: select: { _anon: longint }
-- + {select_from_etc}: TABLE { big: big }
select l * 1 from big;

-- TEST: long * bool -> long (nullables)
-- - Error
-- + {select_stmt}: select: { _anon: longint }
-- + {select_from_etc}: TABLE { big: big }
select l * (1==1) from big;

-- TEST: long * real -> real (nullables)
-- - Error
-- + {select_stmt}: select: { _anon: real }
-- + {select_from_etc}: TABLE { big: big }
select l * 2.0 from big;

-- TEST: null can be used in numeric comparisons to give nullable bool
-- - Error
-- + {select_stmt}: select: { _anon: bool }
-- + {int 1}: integer notnull
select null = 1;

-- TEST: null can be used in string comparison to give nullable bool
-- - Error
-- + {select_stmt}: select: { _anon: bool }
-- + {strlit 'foo'}: text notnull
select null = 'foo';

-- TEST: not x is an error, no cascade error reported just one error
-- + Error % incompatible types in expression '='
-- +1 Error
-- + {select_stmt}: err
-- + {eq}: err
-- + {not}: err
select not 'x' == 1;

-- TEST: ok to have two different strings
-- - Error
-- note there was no else case, so nullable result
-- + {select_stmt}: select: { _anon: text }
-- + {case_list}: text
-- +2 {when}: text notnull
select case
  when 1 = 2 then 'foo'
  when 3 = 4 then 'bar'
end;

-- TEST: can't combine a string and a number
-- + Error % incompatible types in expression 'then'
-- +1 Error
-- + {select_stmt}: err
-- + {case_expr}: err
-- + {case_list}: err
select case
  when 1 = 2 then 'foo'
  when 3 = 4 then 2
end;

-- TEST: when expression should be a boolean
-- + Error % incompatible types in expression 'when'
-- +1 Error
-- + {select_stmt}: err
-- + {case_expr}: err
-- + {strlit 'x'}: err
select case
  when 'x' then 'foo'
end;

-- TEST: ok to compare strings to each other
-- - Error
-- note the result type is nullable, there was no else case!
-- + {select_stmt}: select: { _anon: integer }
-- + {case_expr}: integer
select case 'x'
  when 'y' then 1
  when 'z' then 2
end;

-- TEST: ok to compare a real to an int
-- - Error
-- note the result type is nullable, there was no else case!
-- + {select_stmt}: select: { _anon: integer }
-- + {case_expr}: integer
select case 2
  when 1.0 then 1
  when 3 then 2
end;

-- TEST: can't compare a string and a number
-- + Error % incompatible types in expression 'when'
-- +1 Error
-- + {select_stmt}: err
-- + case_expr}: err
-- + {strlit 'x'}: err
select case 3
  when 'x' then 1
end;

-- TEST: int combines with real to give real
-- - Error
-- {select_stmt}: select: { _anon: real notnull }
-- {case_expr}: real notnull
select case 4
  when 1 then 1
  else 2.0
end;

-- TEST: null combines with int to give nullable int
-- - Error
-- + {select_stmt}: select: { _anon: integer }
-- + {case_expr}: integer
-- - {case_expr}: integer notnull
select case 5
  when 0 then null
  when 1 then 1
end;

-- TEST: real combines with real to give real
-- - Error
-- + {select_stmt}: select: { _anon: real notnull }
-- + {case_expr}: real notnull
select case 6
  when 0 then 1.0
  else 2.0
end;


-- TEST: bool combines with null to give nullable bool
-- - Error
-- + {select_stmt}: select: { _anon: bool }
-- + {case_expr}: bool
-- - {case_expr}: bool notnull
select case 7
  when 0 then (1==2)
  else null
end;

-- TEST: else statement not compatible type with when
-- + Error % incompatible types in expression 'else'
-- +1 Error
-- + {select_stmt}: err
-- + {case_expr}: err
-- + {strlit 'bar'}: err
select case 8
  when 0 then 1
  else 'bar'
end;

-- TEST: case statement has expression type error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {case_expr}: err
-- + {not}: err
select case NOT 'x'
when 1 then 0
end;

-- TEST: ranges ok as integer
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {between}: bool notnull
select 1 between 0 and 2;

-- TEST: ranges ok as string
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {between}: bool notnull
select 'x' between 'a' and 'z';

-- TEST: string cannot be compared to integers
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
-- + {select_stmt}: err
-- + {between}: err
select 'x' between 2 and 3;

-- TEST: string cannot be compared to integers -- second item
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
-- + {select_stmt}: err
-- + {between}: err
select 'x' between null and 3;

-- TEST: null can be compared to anything
-- - Error
-- note nullable result
-- + {select_stmt}: select: { _anon: bool }
-- + {between}: bool
select null between 1 and 2;

-- TEST: range items must be comparable to each other
-- + Error % incompatible types in expression 'BETWEEN/AND'
-- +1 Error
-- + {select_stmt}: err
-- + {between}: err
select null between 1 and 'x';

-- TEST: don't re-report errors if there is already a failure in the expression
-- Note: here we also verify that NOT is weaker than between hence requires the parens stay
-- + SELECT (NOT 'x') BETWEEN 1122 AND 3344;
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {between}: err
select (NOT 'x') between 1122 and 3344;

-- TEST: ranges ok as integer (NOT BETWEEN)
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {not_between}: bool notnull
select 1 not between 0 and 2;

-- TEST: ranges ok as string
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {not_between}: bool notnull
select 'x' not between 'a' and 'z';

-- TEST: string cannot be compared to integers
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
-- + {select_stmt}: err
-- + {not_between}: err
select 'x' not between 2 and 3;

-- TEST: string cannot be compared to integers -- second item
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
-- + {select_stmt}: err
-- + {not_between}: err
select 'x' not between null and 3;

-- TEST: null can be compared to anything
-- - Error
-- note nullable result
-- + {select_stmt}: select: { _anon: bool }
-- + {not_between}: bool
select null not between 1 and 2;

-- TEST: range items must be comparable to each other
-- + Error % incompatible types in expression 'NOT BETWEEN/AND'
-- +1 Error
-- + {select_stmt}: err
-- + {not_between}: err
select null not between 1 and 'x';

-- TEST: don't re-report errors if there is already a failure in the expression
-- Note: here we also verify that NOT is weaker than not between hence requires the parens stay
-- + SELECT (NOT 'x') NOT BETWEEN 1122 AND 3344;
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {not_between}: err
select (NOT 'x') not between 1122 and 3344;

-- TEST: nested select statement in the from clause
-- - Error
-- +2 {select_stmt}: select: { id: integer notnull, name: text notnull }
-- + {select_from_etc}: TABLE { Item: select }
-- + {select_stmt}: select: { id: integer notnull, name: text notnull }
select * from ( select 1 as id, 'x' as name ) as Item;

-- TEST: nested select statement with join
-- - Error
-- + {select_stmt}: select: { id1: integer notnull, name: text notnull, id2: integer notnull, brand: text notnull }
-- + {select_stmt}: select: { id1: integer notnull, name: text notnull }
-- + {select_stmt}: select: { id2: integer notnull, brand: text notnull }
-- + {join_cond}: JOIN { Item: select, ItemBrand: select }
select * from
( select 1 as id1, 'x' as name ) as Item
inner join (select 1 as id2, 'b' as brand) as ItemBrand
on ItemBrand.id2 = Item.id1;

-- TEST: nested select expression
-- - Error
-- + {select_stmt}: select: { result: integer notnull }
-- + {select_expr}: result: integer notnull
-- + {select_stmt}: unused: integer notnull
select (select 1 as unused) as result;

-- TEST: nested select expression with wrong # of items
-- + Error % nested select expression must return exactly one column
-- +1 Error
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {select_expr_list_con}: select: { _anon: integer notnull, _anon: integer notnull }
select (select 1, 2);

-- TEST: nested select used for simple math
-- - Error
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {select_expr}: integer notnull
select 1 * (select 1);

-- TEST: nested select used for string concat
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {select_expr}: integer notnull
select (select 1) || (select 2);

-- TEST: multiple table refs
-- - Error
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, name: text, rate: longint }
-- + {select_from_etc}: JOIN { foo: foo, bar: bar }
select * from (foo, bar);

-- TEST: duplicate table refs
-- + Error % duplicate table name in join 'foo'
-- +1 Error
-- + {select_stmt}: err
-- + {select_from_etc}: err
-- +2 {table_or_subquery}: TABLE { foo: foo }
select * from (foo, foo);

-- TEST: full join with all expression options (except offset which was added later)
-- - Error
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, name: text, rate: longint }
-- + {opt_where}: bool notnull
-- + {opt_groupby}: ok
-- + {opt_having}: bool
-- + {opt_orderby}: ok
-- + {opt_limit}: integer notnull
select * from foo as T1
inner join bar as T2 on T1.id = T2.id
where T2.id > 5
group by T2.name
having T2.name = 'x'
order by T2.rate
limit 5;

-- TEST: join with bogus ON expression type
-- + Error % expected numeric expression 'ON'
-- +1 Error
-- + {select_stmt}: err
-- + {on}: err
select * from foo
inner join bar as T2 on 'v'
where 'w'
having 'x'
limit 'y';

-- TEST: join with bogus other expression types
--       one of few cases where error processing continues
-- + Error % expected numeric expression 'WHERE'
-- + Error % expected numeric expression 'HAVING'
-- + Error % HAVING clause requires GROUP BY clause
-- +3 Error
-- + {select_stmt}: err
select * from foo
where 'w'
having 'x'
limit 'y';

-- TEST: select with bogus order by x limit x
-- + Error % name not found 'bogus'
-- + Error % expected numeric expression 'LIMIT'
-- +2 Error
-- + {select_stmt}: err
select * from foo
order by bogus limit 'y';

-- TEST: force the case where a nested select has an error
--       the top level select should be marked with an error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- +2 {select_stmt}: err
-- + {not}: err
select (select not 'x');

-- TEST: basic IN statement -- null is ok anywhere
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {in_pred}: bool notnull
-- +2 {int 1}: integer notnull
-- +1 {int 2}: integer notnull
-- + {null}: null
select 1 in (1, 2, null);

-- TEST: can't match strings against a number
-- + Error % incompatible types in expression 'IN'
-- +1 Error
-- + {select_stmt}: err
-- + {in_pred}: err
select 1 in ('x', 2);

-- TEST: simple string works
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- note null in the list changes nothing
-- + {in_pred}: bool notnull
-- +2 {strlit 'x'}: text notnull
-- +1 {strlit 'y'}: text notnull
-- +1 {null}: null
select 'x' in ('x', 'y', null);

-- TEST: string can't be matched against number
-- + Error % incompatible types in expression 'IN'
-- +1 Error
-- + {select_stmt}: err
-- + {in_pred}: err
select 'x' in ('x', 1);

-- TEST: null can match against numbers
-- - Error
-- nullable result! CG will make the answer null
-- + {select_stmt}: select: { _anon: bool }
-- + {expr_list}: integer notnull
select null in (1, 2);

-- TEST: null can match against strings
-- - Error
-- nullable result! CG will make the answer null
-- + {select_stmt}: select: { _anon: bool }
-- + {expr_list}: text notnull
select null in ('x', 'y', null);

-- TEST: numbers are ok and so are strings, but you can't mix and match
-- + Error % incompatible types in expression 'IN'
-- +1 Error
-- + {select_stmt}: err
-- + {in_pred}: err
select null in (1, 'x');

-- TEST: no casade errors if the left arg of in has an error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {in_pred}: err
-- + {not}: err
select (not 'x') in (1, 'x');

-- TEST: no casade errors if the predicate has an error
--       "select distinct" used here just to force that option to run
--       semantic analysis does not care about it (so verify successfully ignored?)
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {in_pred}: err
-- + {not}: err
select distinct 1 in (1, not 'x', 'y');

-- TEST: basic NOT IN statement -- null is ok anywhere
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- + {not_in}: bool notnull
-- +2 {int 1}: integer notnull
-- +1 {int 2}: integer notnull
-- + {null}: null
select 1 not in (1, 2, null);

-- TEST: can't match strings against a number
-- + Error % incompatible types in expression 'NOT IN'
-- +1 Error
-- + {select_stmt}: err
-- + {not_in}: err
select 1 not in ('x', 2);

-- TEST: simple string works
-- - Error
-- + {select_stmt}: select: { _anon: bool notnull }
-- note null in the list changes nothing
-- + {not_in}: bool notnull
-- +2 {strlit 'x'}: text notnull
-- +1 {strlit 'y'}: text notnull
-- +1 {null}: null
select 'x' not in ('x', 'y', null);

-- TEST: string can't be matched against number
-- + Error % incompatible types in expression 'NOT IN'
-- +1 Error
-- + {select_stmt}: err
-- + {not_in}: err
select 'x' not in ('x', 1);

-- TEST: null can match against numbers
-- - Error
-- nullable result! CG will make the answer null
-- + {select_stmt}: select: { _anon: bool }
-- + {expr_list}: integer notnull
select null not in (1, 2);

-- TEST: null can match against strings
-- - Error
-- nullable result! CG will make the answer null
-- + {select_stmt}: select: { _anon: bool }
-- + {expr_list}: text notnull
select null not in ('x', 'y', null);

-- TEST: numbers are ok and so are strings, but you can't mix and match
-- + Error % incompatible types in expression 'NOT IN'
-- +1 Error
-- + {select_stmt}: err
-- + {not_in}: err
select null not in (1, 'x');

-- TEST: create a view
-- - Error
-- + {create_view_stmt}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + {name MyView}
-- + {select_stmt}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
create view MyView as select 1 as f1, 2 as f2, 3 as f3;

-- TEST: create a view -- exact duplicate is allowed
-- - Error
-- + {create_view_stmt}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + {name MyView}
-- + {select_stmt}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
create view MyView as select 1 as f1, 2 as f2, 3 as f3;

-- TEST: try to use the view
-- - Error
-- + {select_stmt}: select: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + select_from_etc}: TABLE { ViewAlias: MyView }
select f1, f2, ViewAlias.f3 from MyView as ViewAlias;

-- TEST: try to make a duplicate view (re-use a view)
-- + Error % duplicate table/view name 'MyView'
-- + {create_view_stmt}: err
-- + {name MyView}: err
create view MyView as select 1 y;

-- TEST: try to make a duplicate view (re-use a table)
-- + Error % duplicate table/view name 'foo'
-- + {create_view_stmt}: err
create view foo as select 2 x;

-- TEST: no error cascade (one error, just the internal error)
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- - Error % duplicate
-- + {create_view_stmt}: err
create view MyView as select NOT 'x';

-- TEST: this view create will fail with one error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {create_view_stmt}: err
-- + {not}: err
create view V as select NOT 'x';

-- TEST: can't select from V, it failed.
-- + Error % table/view not defined 'V'
-- + {select_stmt}: err
-- + {select_from_etc}: err
-- + {table_or_subquery}: err
select * from V;

-- TEST: create an index
-- - Error
-- + {create_index_stmt}: ok
-- + {name id}: id: integer notnull
create index index_1 on foo(id);

-- TEST: exact duplicate index is ok
-- - Error
-- + {create_index_stmt}: ok
-- + {name id}: id: integer notnull
create index index_1 on foo(id);

-- TEST: exact duplicate index is ok
-- + {create_index_stmt}: err
-- + Error % migration proc not allowed on object 'index_4'
-- +1 Error
create index index_4 on foo(id) @delete(1, AMigrateProc);

-- TEST: try to create a duplicate index
-- + Error % duplicate index name 'index_1'
-- -- + {create_index_stmt}: err
create index index_1 on bar(id);

-- TEST: try to create an index on a table that doesn't exist
-- + Error % create index table name not found 'doesNotExist'
-- +1 Error
-- + {create_index_stmt}: err
create index index_2 on doesNotExist(id);

-- TEST: try to create an index on columns that do not exist
-- + Error % name not found 'doesNotExist'
-- +1 Error
-- + {create_index_stmt}: err
-- + {name doesNotExist}: err
create index index_3 on foo(doesNotExist);

-- TEST: validate primary key columns, ok
-- - Error
-- + {create_table_stmt}: simple_pk_table: { id: integer notnull }
create table simple_pk_table(
  id integer not null,
  PRIMARY KEY (id)
);

-- TEST: validate primary key columns, bogus name
-- + Error % table does not have pk column 'pk_col_not_exist'
-- +1 Error
-- + {create_table_stmt}: err
-- +  {name pk_col_not_exist}: err
create table baz(
  id integer not null,
  PRIMARY KEY (pk_col_not_exist)
);

-- TEST: validate PK not duplicated
-- + Error % more than one primary key in table 'baz'
-- +1 Error
-- + {create_table_stmt}: err
create table baz(
  id integer not null,
  PRIMARY KEY (id),
  PRIMARY KEY (id)
);

-- TEST: validate simple unique key
-- - Error
-- + {create_table_stmt}: simple_ak_table: { id: integer notnull }
-- + {name ak1}
create table simple_ak_table (
  id integer not null,
  CONSTRAINT ak1 UNIQUE (id)
);

-- TEST: validate simple in group of unique key overlapping each others
-- + {create_table_stmt}: simple_ak_table_2: { a: integer notnull, b: text, c: real, d: longint }
-- - Error
create table simple_ak_table_2 (
  a integer not null,
  b text,
  c real,
  d long int,
  UNIQUE (a, b),
  UNIQUE (a, c),
  UNIQUE (d)
);

-- TEST: validate simple in group of unique key containing one column in common
-- + {create_table_stmt}: simple_ak_table_3: { a: integer notnull, b: text, c: real, d: longint }
-- - Error
create table simple_ak_table_3 (
  a integer not null,
  b text,
  c real,
  d long int,
  UNIQUE (a, b),
  UNIQUE (b, d)
);

-- TEST: invalidate unique key that is the subset (in order) of another, (a, b, c) is invalid because (a, b) is already unique key
-- + {create_table_stmt}: err
-- + Error % at least part of this unique key is redundant with previous unique keys
-- + Error
create table simple_ak_table_4 (
  a integer not null,
  b text,
  c real,
  UNIQUE (a, b),
  UNIQUE (a, b, c)
);

-- TEST: invalidate same column in two unique key, (b, a) is invalid because (a, b) is already unique key
-- + {create_table_stmt}: err
-- + Error % at least part of this unique key is redundant with previous unique keys
-- + Error
create table simple_ak_table_5 (
  a integer not null,
  b text,
  c real,
  d long int,
  UNIQUE (a, b),
  UNIQUE (b, a)
);

-- TEST: invalidate unique key that is the subset (at end) of another, (c, d, b, a) is invalid because subset (a, b) is already unique key
-- + {create_table_stmt}: err
-- + Error % at least part of this unique key is redundant with previous unique keys
-- + Error
create table simple_ak_table_6 (
  a integer not null,
  b text,
  c real,
  d long int,
  UNIQUE (a, b),
  UNIQUE (c, d, b, a)
);

-- TEST: invalidate unique key that is the subset (at start) of another, (a, b) is invalid because (a) is unique key
-- + {create_table_stmt}: err
-- + Error % at least part of this unique key is redundant with previous unique keys
-- + Error
create table simple_ak_table_7 (
  a integer not null,
  b text,
  c real,
  d long int,
  UNIQUE (a, b),
  UNIQUE (a)
);

-- TEST: validate duplicate unique key
-- + Error % duplicate constraint name in table 'ak1'
-- +1 Error
-- + {create_table_stmt}: err
create table baz_dup_uk (
  id integer PRIMARY KEY AUTOINCREMENT not null,
  CONSTRAINT ak1 UNIQUE (id),
  CONSTRAINT ak1 UNIQUE (id)
);

-- TEST: validate duplicate primary unique key
-- + Error % duplicate constraint name in table 'pk1'
-- +1 Error
-- + {create_table_stmt}: err
create table baz_dup_pk (
  id integer not null,
  CONSTRAINT pk1 PRIMARY KEY (id),
  CONSTRAINT pk1 PRIMARY KEY (id)
);

-- TEST: validate duplicate in group of unique key
-- + Error % at least part of this unique key is redundant with previous unique keys
-- +1 Error
-- + {create_table_stmt}: err
create table baz_2 (
  id integer PRIMARY KEY AUTOINCREMENT not null,
  name text,
  UNIQUE (id, name),
  UNIQUE (name, id)
);

-- TEST: validate unique key columns
-- + Error % name not found 'ak_col_not_exist'
-- +1 Error
-- + {create_table_stmt}: err
-- + {name ak_col_not_exist}: err
create table baz (
  id integer PRIMARY KEY AUTOINCREMENT not null,
  CONSTRAINT ak1 UNIQUE (ak_col_not_exist)
);

-- TEST: validate group of unique key columns
-- + Error % name not found 'ak_col_not_exist'
-- +1 Error
-- + {create_table_stmt}: err
-- + {name ak_col_not_exist}: err
create table baz_3 (
  id integer PRIMARY KEY AUTOINCREMENT not null,
  UNIQUE (ak_col_not_exist)
);

-- TEST: make a valid FK
-- - Error
-- + {create_table_stmt}: fk_table: { id: integer foreign_key }
-- +2 {name_list}
-- +2 {name id}: id: integer
create table fk_table (
  id integer,
  FOREIGN KEY (id) REFERENCES foo(id)
);

-- TEST: make a valid FK
-- + Error % duplicate constraint name in table 'x'
create table fk_table_dup (
  id integer,
  constraint x foreign key (id) references foo(id),
  constraint x foreign key (id) references foo(id)
);

-- TEST: make an FK that refers to a bogus column in the current table
-- + Error % name not found 'bogus'
-- +1 Error
-- + {create_table_stmt}: err
create table baz (
  id integer,
  FOREIGN KEY (bogus) REFERENCES foo(id)
);

-- TEST: make an FK that refers to a bogus column in the reference table
-- + Error % name not found 'bogus'
-- +1 Error
-- + {create_table_stmt}: err
create table baz (
  id integer,
  FOREIGN KEY (id) REFERENCES foo(bogus)
);

-- TEST: make an FK that refers to a bogus table
-- + Error % foreign key refers to non-existent table 'bogus'
-- +1 Error
-- create_table_stmt}: err
create table baz (
  id integer,
  FOREIGN KEY (id) REFERENCES bogus(id)
);

-- TEST: well formed if statement
-- - Error
-- +1 {if_stmt}: integer notnull
-- +1 {cond_action}: integer notnull
-- +1 {if_alt}: ok
-- +1 {else}: ok
-- +2 {stmt_list}: ok
if 1 then
  select 1;
else
  select 2;
end if;

-- TEST: if with bad predicate
-- + Error % expected numeric expression in IF predicate
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {cond_action}: err
-- - {stmt_list}: err
if 'x' then
  select 1;
end if;

-- TEST: if with error predicate, no double error reporting
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {cond_action}: err
-- - {stmt_list}: err
if not 'x' then
  select 1;
end if;

-- TEST: if with bogus statement list, no double error reporting
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {cond_action}: err
-- +1 {stmt_list}: err
if 1 then
  select not 'x';
end if;

-- TEST: if with bogus statement list in else blocki, no double error reporting
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {cond_action}: integer notnull
-- +1 {if_alt}: err
-- +1 {else}: err
if 1 then
  select 1;
else
  select not 'x';
end if;

-- TEST: if with else if clause
-- - Error
-- +1 {if_stmt}: integer notnull
-- +2 {cond_action}: integer notnull
-- +1 {if_alt}: ok
-- +1 {elseif}: integer notnull
-- +1 {else}: ok
if 1 then
 select 1;
else if 2 then
 select 2;
else
 select 3;
end if;

-- TEST: if with else if clause bogus expression type
-- + Error % expected numeric expression in IF predicate
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {if_alt}: err
-- +1 {cond_action}: integer notnull
-- +1 {cond_action}: err
if 1 then
 select 1;
else if '2' then
 select 2;
else
 select 3;
end if;

-- TEST: create an error down the else if list and make sure it props to the front of the list
--       that causes the whole statement to be correctly reported as having an error
-- + Error % expected numeric expression in IF predicate
-- +1 Error
-- +1 {if_stmt}: err
-- +1 {if_alt}: err
-- +3 {cond_action}: integer notnull
-- +1 {cond_action}: err
if 1 then
  select 1;
else if 2 then
  select 2;
else if 3
  then select 3;
else if '4'
  then select 4;
end if;

-- TEST: force an error in the group by clause, this error must spoil the whole statement
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {opt_groupby}: err
select id from foo group by id, not 'x';

-- TEST: force an error in the order by clause, this error must spoil the whole statement
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- + {opt_orderby}: err
select id from foo order by id, not 'x';

-- TEST: smallish table to cover some missing cases, bool field and an int with default
-- - Error
-- + {create_table_stmt}: booly: { id: integer has_default, flag: bool }
create table booly (
  id integer DEFAULT 8675309,
  flag BOOL
);

declare enum ints integer (
 negative_one = -1,
 postive_one = 1
);

-- TEST: use const expr where normally literals go in default value
-- + {col_attrs_default}: err
-- + {const}: err
-- + x INTEGER DEFAULT -1,
-- + y INTEGER DEFAULT CONST(1 / 0)
-- + Error % evaluation of constant failed
create table bad_constants_table(
  x integer default const(ints.negative_one),
  y integer default const(1/0)
);

-- TEST: use const expr where literals go in attribute
-- + {const}: err
-- + @ATTRIBUTE(whatever=-1)
-- + @ATTRIBUTE(whatever=CONST(1 / 0))
-- + Error % evaluation of constant failed
@attribute(whatever=const(ints.negative_one))
@attribute(whatever=const(1/0))
declare proc bad_constants_proc();

-- TEST: use bad constant expr in nested context
-- + {const}: err
-- + @ATTRIBUTE(whatever=(1, CONST(1 / 0), 1))
-- + Error % evaluation of constant failed
@attribute(whatever=(1, const(1/0), 1))
declare proc bad_constants_nested_proc();

-- TEST: try to use a NULL default value on a non nullable column
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + Error % cannot assign/copy possibly null expression to not null target 'default value'
-- +1 Error
create table bad_conversions(
  data integer not null default const(NULL)
);

-- TEST: try to use a lossy conversion in a const expr default value
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + Error % lossy conversion from type 'REAL' in 2.200000e+00
-- +1 Error
create table bad_conversions(
  data integer not null default const(1+1.2)
);

-- TEST: allowable conversion, the constant becomes real
-- + data REAL NOT NULL DEFAULT 1
-- - Error
create table good_conversions(
  data real not null default const(1)
);

-- TEST: verify the correct types are extracted, also cover the final select option
-- - Error
-- + {select_stmt}: select: { id: integer, flag: bool }
-- + {select_opts}
-- + {distinctrow}
select distinctrow id, flag from booly;

-- TEST: make variables (X/Y are nullable)
-- - Error
-- + {declare_vars_type}: integer
-- + {name X}: X: integer variable
-- + {name Y}: Y: integer variable
declare X, Y integer;

-- TEST: make variables (X/Y are not null)
-- - Error
declare X_not_null integer not null;

-- TEST: try to declare X again
-- + Error % duplicate variable name in the same scope 'X'
-- +1 Error
-- + {declare_vars_type}: err
-- + {name X}: err
declare X integer;

-- TEST: try to declare a variable that hides a table
-- + Error % global variable hides table/view name 'foo'
-- +1 Error
-- + {declare_vars_type}: err
-- + {name foo}: err
declare foo integer;

-- TEST: try to access a variable
-- - Error
-- + {select_stmt}: select: { X: integer variable }
-- + {name X}: X: integer variable
select X;

-- TEST: create a cursor with select statement
-- - Error
-- + {declare_cursor}: my_cursor: select: { one: integer notnull, two: integer notnull } variable
declare my_cursor cursor for select 1 as one, 2 as two;

-- TEST: create a cursor with primitive kinds
-- + {declare_cursor}: kind_cursor: select: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> } variable
-- - Error
declare kind_cursor cursor for select * from with_kind;

-- TEST: make a value cursor of the same shape
-- + {declare_cursor_like_name}: kind_value_cursor: select: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> } variable shape_storage value_cursor
-- - Error
declare kind_value_cursor cursor like kind_cursor;

-- TEST: try to create a duplicate cursor
-- + Error % duplicate variable name in the same scope 'my_cursor'
-- +1 Error
-- + {declare_cursor}: err
-- + {name my_cursor}: err
declare my_cursor cursor for select 1;

-- TEST: the select statement is bogus, error cascade halted so the duplicate name is not reported
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- - duplicate
-- + {declare_cursor}: err
-- + {select_stmt}: err
-- + {not}: err
declare my_cursor cursor for select not 'x';

-- TEST: standard loop with leave
-- - Error
-- + {loop_stmt}: ok
-- + {leave_stmt}: ok
loop fetch my_cursor into X, Y begin
  leave;
end;

-- TEST: loop with leave, leave not the last statement
-- + Error % in leave_stmt % statement should be the last thing in a statement list
-- +1 Error
while 1
begin
  leave;
  leave;
end;

-- TEST: loop with continue, continue not the last statement
-- + Error % in continue_stmt % statement should be the last thing in a statement list
-- +1 Error
while 1
begin
  continue;
  leave;
end;

-- TEST: standard loop with continue
-- - Error
-- + {loop_stmt}: ok
-- + {continue_stmt}: ok
loop fetch my_cursor into X, Y begin
  continue;
end;

-- TEST: try to loop over a scalar
-- + Error % variable is not a cursor 'X'
-- +1 Error
-- + {loop_stmt}: err
-- + {fetch_stmt}: err
-- + {name X}: err
loop fetch X into y begin
  leave;
end;

-- TEST: try to loop over something that isn't present
-- + Error % cursor not found 'not_a_variable'
-- +1 Error
-- + {loop_stmt}: err
-- + {fetch_stmt}: err
-- + {name not_a_variable}: err
loop fetch not_a_variable into x
begin
  leave;
end;

-- TEST: try to leave outside of a loop
-- + Error % leave must be inside of a 'loop' or 'while' statement
-- +1 Error
-- + {leave_stmt}: err
leave;

-- TEST: try to continue outside of a loop
-- + Error % continue must be inside of a 'loop' or 'while' statement
-- +1 Error
-- + {continue_stmt}: err
continue;

-- TEST: legal return out of a procedure
-- we have to check the next statement and that is tricky if there was
-- attribute;  this tests that case.
-- + {return_stmt}: ok
-- - Error
create proc return_with_attr()
begin
  if 1 then
    @attribute(goo)
    return;
  end if;
end;

-- TEST: legal return, no attribute on the return this time
-- + {return_stmt}: ok
-- - Error
create proc return_no_attr()
begin
  if 1 then
    return;
  end if;
end;

-- TEST: return must be the last statement (attr form)
-- + Error % in return_stmt % statement should be the last thing in a statement list
-- +1 Error
create proc return_not_last_with_attr()
begin
  if 1 then
    @attribute(goo)
    return;
    return;
  end if;
end;

-- TEST: return must be the last statement (no attr form)
-- + Error % in return_stmt % statement should be the last thing in a statement list
-- +1 Error
create proc return_not_last_no_attr()
begin
  if 1 then
    return;
    return;
  end if;
end;

-- TEST: return outside of any proc
-- + Error % return statement should be in a procedure and not at the top level
-- +1 Error
return;

-- TEST: return at top level, that's just goofy
-- + Error % return statement should be in a procedure and not at the top level
-- +1 Error
create proc return_at_top_level()
begin
  return;
end;

-- TEST: loop must prop errors inside it up so the overall loop is a semantic failure
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- {loop_stmt}: err
loop fetch my_cursor into X, Y
begin
 select not 'X';
end;

-- TEST: open a valid cursor
-- - Error
-- + {open_stmt}: my_cursor: select: { one: integer notnull, two: integer notnull } variable
open my_cursor;

-- TEST: close a valid cursor
-- - Error
-- + {close_stmt}: my_cursor: select: { one: integer notnull, two: integer notnull } variable
close my_cursor;

-- TEST: open invalid cursor
-- + Error % variable is not a cursor 'X'
-- +1 Error
-- + {open_stmt}: err
-- + {name X}: err
open X;

-- TEST: close invalid cursor
-- + Error % variable is not a cursor 'X'
-- +1 Error
-- + {close_stmt}: err
-- + {name X}: err
close X;

-- TEST: a working delete
-- - Error
-- + {delete_stmt}: ok
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- + {opt_where}: bool notnull
delete from foo where id = 33;

-- TEST: delete from bogus table
-- + Error % table in delete statement does not exist 'bogus_table'
-- +1 Error
-- + {delete_stmt}: err
delete from bogus_table;

-- TEST: delete from a view
-- + Error % cannot delete from a view 'MyView'
-- +1 Error
-- + {delete_stmt}: err
delete from MyView;

-- TEST: delete with bogus expression
-- + Error % name not found 'missing_column'
-- +1 Error
-- + {delete_stmt}: err
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- + {name missing_column}: err
delete from foo where missing_column = 1;

-- TEST: regular insert
-- - Error
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 1}: integer notnull
-- + {strlit 'bazzle'}: text notnull
-- + {int 3}: integer notnull
insert into bar values (1, 'bazzle', 3);

-- TEST: replace statement
-- - Error
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 1}: integer notnull
-- + {strlit 'bazzle'}: text notnull
-- + {int 3}: integer notnull
replace into bar values (1, 'bazzle', 3);

-- TEST: insert or fail
-- - Error
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 1}: integer notnull
-- + {strlit 'bazzle'}: text notnull
-- + {int 3}: integer notnull
insert or fail into bar values (1, 'bazzle', 3);

-- TEST: insert or rollback
-- - Error
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 1}: integer notnull
-- + {strlit 'bazzle'}: text notnull
-- + {int 3}: integer notnull
insert or rollback into bar values (1, 'bazzle', 3);

-- TEST: insert or abort
-- - Error
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 1}: integer notnull
-- + {strlit 'bazzle'}: text notnull
-- + {int 3}: integer notnull
insert or abort into bar values (1, 'bazzle', 3);

-- TEST: insert default values
-- - Error
-- + {insert_stmt}: ok
-- + {name_columns_values}
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- + {default_columns_values}
insert into foo default values;

-- TEST: insert default values
-- + {insert_stmt}: err
-- + Error % mandatory column with no default value in INSERT INTO name DEFAULT VALUES statement 'id'
-- +1 Error
insert into bar default values;

-- TEST: insert into a table that isn't there
-- + Error % table in insert statement does not exist 'bogus_table'
-- +1 Error
-- + {insert_stmt}: err
-- + {name bogus_table}
insert into bogus_table values (1);

-- TEST: insert into a view
-- + Error % cannot insert into a view 'MyView'
-- +1 Error
-- + {name MyView}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + {insert_stmt}: err
insert into MyView values (1);

-- TEST: insert with errors -- note that id is a field name of bar but it must not be found
-- + Error % name not found 'id'
-- +1 Error
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {name id}: err
insert into bar values (id, 'bazzle', 3);

-- TEST: insert into foo, one column, it is autoinc, so use NULL
-- - Error
-- + {insert_stmt}: ok
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
insert into foo values (NULL);

-- TEST: insert into bar, type mismatch
-- + Error % incompatible types in expression 'id'
-- +1 Error
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {strlit 'string is wrong'}: err
insert into bar values ('string is wrong', 'string', 1);

-- TEST: insert into bar, type mismatch, 2 is wrong
-- + Error % incompatible types in expression 'name'
-- +1 Error
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {int 2}: err
insert into bar values (1, 2, 3);

-- TEST: insert too many columns
-- + Error
-- +1 Error
-- + Error % count of columns differs from count of values
-- + {insert_stmt}: err
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
insert into foo values(NULL, 2);

-- TEST: insert too few columns
-- + Error
-- +1 Error
-- + Error % select statement with VALUES clause requires a non empty list of values
-- + {insert_stmt}: err
-- + {select_stmt}: err
-- + {select_core}: err
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
insert into foo values();

-- TEST: insert into bar, null not allowed in non-null field
-- + Error % cannot assign/copy possibly null expression to not null target 'id'
-- +1 Error
-- + {insert_stmt}: err
insert into bar values (null, 'string', 1);

-- TEST: table cannot have more than one autoinc
-- + Error % table can only have one autoinc column 'id2'
-- +1 Error
-- + {create_table_stmt}: err
create table two_autoincs_is_bad(
  id1 integer PRIMARY KEY AUTOINCREMENT not null,
  id2 integer PRIMARY KEY AUTOINCREMENT not null
);

-- TEST: valid assignment
-- - Error
-- + {assign}: X: integer variable
set X := 1;

-- TEST: bogus variable name
-- + Error % variable not found 'XX'
-- +1 Error
-- + {assign}: err
-- + {name XX}
set XX := 1;

-- TEST: try to assign a cursor
-- + Error % cannot set a cursor 'my_cursor'
-- +1 Error
-- + {assign}: err
-- + {name my_cursor}: my_cursor: select: { one: integer notnull, two: integer notnull } variable
set my_cursor := 1;

-- TEST: variable type mismatch
-- + Error % incompatible types in expression 'X'
-- +1 Error
-- + {assign}: err
-- + {name X}: err
set X := 'x';

-- TEST: null ok with everything
-- - Error
-- + {assign}: X: integer variable
-- + {null}: null
set X := null;

-- TEST: error propagates up, no other reported error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {assign}: err
-- + {not}: err
set X := not 'x';

-- TEST: simple cursor and fetch test
-- - Error
-- + {declare_cursor}: fetch_cursor: select: { _anon: integer notnull, _anon: text notnull, _anon: null } variable
declare fetch_cursor cursor for select 1, 'foo', null;

-- setup variables for the upcoming tests
declare an_int integer;
declare an_int2 integer;
declare a_string text;
declare a_string2 text;
declare a_nullable text;
declare an_long long integer;

-- TEST: ok to fetch_stmt
-- - Error
-- + {fetch_stmt}: fetch_cursor: select: { _anon: integer notnull, _anon: text notnull, _anon: null } variable
-- + {name an_int}: an_int: integer variable
-- + {name a_string}: a_string: text variable
-- + {name a_nullable}: a_nullable: text variable
fetch fetch_cursor into an_int, a_string, a_nullable;

-- TEST: fetch too few columns
-- + Error % number of variables did not match count of columns in cursor 'fetch_cursor'
-- +1 Error
-- + {fetch_stmt}: err
fetch fetch_cursor into an_int, a_string;

-- TEST: fetch too many columns
-- + Error % number of variables did not match count of columns in cursor 'fetch_cursor'
-- +1 Error
-- + {fetch_stmt}: err
fetch fetch_cursor into an_int, a_string, a_nullable, a_string2;

-- TEST: fetch an int into a string
-- + Error % incompatible types in expression 'a_string2'
-- +1 Error
-- + {fetch_stmt}: err
-- + {name a_string2}: err
fetch fetch_cursor into a_string2, a_string, a_nullable;

-- TEST: fetch a string into an int
-- + Error % incompatible types in expression 'an_int2'
-- +1 Error
-- + {fetch_stmt}: err
-- + {name an_int2}: err
fetch fetch_cursor into an_int, an_int2, a_nullable;

-- TEST: fetch using a bogus cursor
-- + Error % cursor not found 'not_a_cursor'
-- +1 Error
-- + {fetch_stmt}: err
-- + {name not_a_cursor}: err
fetch not_a_cursor into i;

-- TEST: fetch into a variable that doesn't exist
-- + Error % FETCH variable not found 'non_existent_variable'
-- +1 Error
-- + {fetch_stmt}: err
fetch fetch_cursor into non_existent_variable;

-- TEST: fetch into variables, duplicate in the list
-- + Error % duplicate name in list 'var_id'
-- +1 Error
-- + {fetch_stmt}: err
-- +2 {name var_id}
fetch fetch_cursor into var_id, var_id;

-- TEST: create an index, duplicate name in index list
-- + Error % name list has duplicate name 'id'
-- +1 Error
-- + {create_index_stmt}: err
-- +2 {name id}
create index index_7 on foo(id, id);

-- TEST: validate no duplictes allowed in unique key
-- + Error % name list has duplicate name 'key_id'
-- +1 Error
-- + {create_table_stmt}: err
-- key_id shows up in its definition once, then 2 more times due to duplication
-- +3 {name key_id}
create table bad_table (
  key_id integer PRIMARY KEY AUTOINCREMENT not null,
  CONSTRAINT ak1 UNIQUE (key_id, key_id)
);

-- TEST: validate no duplictes allowed in group of unique key
-- + Error % name list has duplicate name 'key_id'
-- +1 Error
-- + {create_table_stmt}: err
-- key_id shows up in its definition once, then 2 more times due to duplication
-- +3 {name key_id}
create table bad_table_2 (
  key_id integer PRIMARY KEY AUTOINCREMENT not null,
  UNIQUE (key_id, key_id)
);

-- TEST: make an FK with duplicate id in the columns
-- + Error % name list has duplicate name 'col_id'
-- +1 Error
-- + {create_table_stmt}: err
-- col_id shows up in its definition once, then 2 more times due to duplication
-- +3 {name col_id}
create table bad_table (
  col_id integer,
  FOREIGN KEY (col_id, col_id) REFERENCES foo(id)
);

create table ref_target (
  ref_id1 integer,
  ref_id2 integer
);

-- TEST: make an FK with duplicate id in the reference columns
-- + Error % name list has duplicate name 'ref_id1'
-- +1 Error
-- + {create_table_stmt}: err
-- +2 {name ref_id1}
create table bad_table (
  id1 integer,
  id2 integer,
  FOREIGN KEY (id1, id2) REFERENCES ref_target(ref_id1, ref_id1)
);

-- TEST: try to use a cursor as a value -- you get the "cursor has row" boolean
-- - Error
-- + {assign}: X: integer variable
-- + {name X}: X: integer variable
-- + {name my_cursor}: _my_cursor_has_row_: bool notnull variable
set X := my_cursor;

-- TEST: valid update
-- - Error
-- + {update_stmt}: foo: { id: integer notnull primary_key autoinc }
-- + {opt_where}: bool notnull
-- + {eq}: bool notnull
-- + {name id}: id: integer notnull
-- + {int 2}: integer notnull
update foo set id = 1 where id = 2;

-- TEST: update with view
-- + Error % cannot update a view 'myView'
-- +1 Error
-- + {update_stmt}: err
update myView set id = 1;

-- TEST: update with bogus where
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {update_stmt}: err
-- + {opt_where}: err
-- + {not}: err
update foo set id = 1 where not 'x';

-- TEST: update with bogus limit
-- + Error % expected numeric expression 'LIMIT'
-- +1 Error
-- + {update_stmt}: err
-- + {opt_limit}: err
-- + {strlit 'x'}: err
update foo set id = 1 limit 'x';

-- TEST: update with bogus order by
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {update_stmt}: err
-- + {opt_orderby}: err
-- + {update_stmt}: err
update foo set id = 1 order by not 'x' limit 2;

-- TEST: update with bogus column specified
-- + Error % name not found 'non_existent_column'
-- +1 Error
-- + {update_stmt}: err
-- + {name non_existent_column}: err
update foo set non_existent_column = 1;

-- TEST: update with type mismatch (number <- string)
-- + Error % incompatible types in expression 'id'
-- +1 Error
-- + {update_stmt}: err
-- + {update_list}: err
-- + {update_entry}: err
-- + {name id}: id: integer notnull
-- + {strlit 'x'}: text notnull
update foo set id = 'x';

-- TEST: update with string type mismatch (string <- number)
-- + Error % incompatible types in expression 'name'
-- +1 Error
-- + {update_stmt}: err
-- + {update_list}: err
-- + {update_entry}: err
-- + {name name}: name: text
-- + {int 2}: integer notnull
update bar set name = 2;

-- TEST: update not null column to constant null
-- + Error % cannot assign/copy possibly null expression to not null target 'id'
-- +1 Error
-- + {update_stmt}: err
-- + {update_list}: err
-- + {name id}: id: integer notnull
-- + {null}: null
update bar set id = null;

-- TEST: try to use a variable in an update
-- + Error % name not found 'X'
-- +1 Error
-- + {update_stmt}: err
-- + {update_entry}: err
-- + {name X}: err
update bar set X = 1;

-- TEST: update nullable column to constant null
-- - Error
-- + {update_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {update_list}: ok
-- + {update_entry}: rate: longint
-- + {null}: null
update bar set rate = null;

-- TEST: update column to error, no extra errors reported
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {update_stmt}: err
-- + {not}: err
update bar set id = not 'x';

-- TEST: simple procedure
-- - Error
-- + {create_proc_stmt}: ok dml_proc
-- + {delete_stmt}: ok
create procedure proc1()
begin
  delete from foo;
end;

-- TEST: duplicate proc name
-- + Error % duplicate stored proc name 'proc1'
-- + {create_proc_stmt}: err
-- + {name proc1}
create procedure proc1()
begin
  delete from foo;
end;

-- TEST: procedure with arguments
-- - Error
-- + {create_proc_stmt}: ok dml_proc
-- + {delete_stmt}: ok
-- + {eq}: bool
-- + {name arg1}: arg1: integer variable in
-- + {in_pred}: bool notnull
-- + {name arg2}: arg2: text variable in
-- Here we're going to check that the parens came out right in the walk
-- This is a case where precendence is equal and left to right
-- The parents force it to be right to left, we have to honor that even though
-- all priorities in sight are equal
-- + DELETE FROM foo WHERE arg1 = ('x' IN (arg2));
create procedure proc2(arg1 INT, arg2 text)
begin
 delete from foo where arg1 == ('x' in (arg2));
end;

-- TEST: try to use locals that are gone
-- + Error % name not found 'arg1'
select arg1;
-- + Error % name not found 'arg2'
select arg2;

-- TEST: procedure with duplicate arguments
-- + Error % duplicate parameter name 'arg1'
-- +1 Error
-- + {create_proc_stmt}: err
-- + {params}: err
create procedure proc3(arg1 INT, arg1 text)
begin
  call anything(arg1, arg2);
end;

-- TEST: proc name no longer available even though there were errors
-- + Error % duplicate stored proc name 'proc3'
-- +1 Error
-- + {create_proc_stmt}: err
create procedure proc3()
begin
  throw; -- whatever, anything
end;

-- TEST: throw not at the end of a block
-- + Error % statement should be the last thing in a statement list
-- +1 Error
-- + {create_proc_stmt}: err
create procedure proc_throw_not_at_end()
begin
  throw;
  declare x integer;
end;

-- TEST: the out statement will force the proc type to be recomputed, it must not lose the
-- throw state when that happens.
-- + {create_proc_stmt}: C: select: { x: integer notnull } variable dml_proc shape_storage uses_out uses_throw
-- - Error
create proc throw_before_out()
begin
  begin try
    declare C cursor for select 1 x;
    fetch C;
  end try;
  begin catch
    throw;
  end catch;
  out C;
end;

-- TEST: procedure call with arguments mixing in/out legally
-- - Error
-- + {create_proc_stmt}: ok
-- + {params}: ok
-- + {call_stmt}: ok
-- + {name anything}: ok
-- + {name arg1}: arg1: integer variable in
-- + {name arg3}: arg3: real variable in out
create procedure proc4(in arg1 integer, out arg2 text, inout arg3 real)
begin
  call anything(arg1, arg3);
end;

-- TEST: local name conflicts with arg
-- + Error % duplicate variable name in the same scope 'arg1'
-- +1 Error
-- + {params}: ok
-- + {declare_vars_type}: err
-- + {name arg1}: err
create procedure proc5(in arg1 integer, out arg2 text, inout arg3 real)
begin
  declare arg1 int;
end;

-- TEST: try to select out a whole table by table name
-- The name is not in scope
-- + Error % name not found 'bar'
select bar from bar as T;

-- TEST: try to select a whole table by aliased table name
-- The name is not in scope
-- + Error % name not found 'T'
select T from bar as T;

-- TEST: goofy nested select to verify name reachability
-- - Error
-- the nested table matches the outer table
-- +2 {select_stmt}: select: { id: integer notnull, rate: longint }
-- + {select_from_etc}: TABLE { bar: bar }
select id, rate from (select id, rate from bar);

-- TEST: slighly less goofy nested select to verify name reachability
-- - Error
-- + {select_stmt}: select: { id: integer notnull, rate: longint }
-- the nested select had more columns
-- + {select_stmt}: select: { id: integer notnull, name: text, rate: longint }
-- + {select_from_etc}: TABLE { bar: bar }
select id, rate from (select * from bar);

-- TEST: use the table name as its scope
-- + {select_stmt}: select: { id: integer notnull }
-- + select_from_etc}: TABLE { foo: foo }
-- + {dot}: id: integer notnull
-- - Error
select foo.id from foo;

-- TEST: error: try to use the table name as its scope after aliasing
-- + Error % in dot % name not found 'id'
-- + {select_from_etc}: TABLE { T1: foo }
-- + {dot}: err
-- + {select_stmt}: err
-- + {name id}
select foo.id from foo as T1;

-- make a not null variable for the next test
declare int_nn int not null;

-- TEST: bogus assignment
-- + Error % cannot assign/copy possibly null expression to not null target 'int_nn'
-- +1 Error
-- + {assign}: err
set int_nn := NULL;

-- TEST: call some method
-- - Error
-- + {call_stmt}: ok
-- + {name foo}: ok
call foo();

-- TEST: call external method with args
-- - Error
-- + {call_stmt}: ok
-- + {name printf}: ok
-- + {strlit 'Hello, world'}: text notnull
call printf('Hello, world');

-- TEST: call known method with correct args (zero)
-- - Error
-- + {call_stmt}: ok dml_proc
-- + {name proc1}: ok dml_proc
call proc1();

-- TEST: call known method with correct args (two)
-- - Error
-- + {name proc2}: ok dml_proc
-- + {call_stmt}: ok dml_proc
-- + {int 1}: integer notnull
-- + {strlit 'foo'}: text notnull
call proc2(1, 'foo');

-- TEST: call known method with correct bogus int (arg1 should be an int)
-- + Error % incompatible types in expression 'arg1'
-- +1 Error
-- + {name proc2}: ok dml_proc
-- + {call_stmt}: err
-- + {strlit 'bar'}: err
-- + {name proc2}: ok dml_proc
call proc2('bar', 'foo');

-- TEST: call known method with bogus string  (arg2 should be a string)
-- + Error % incompatible types in expression 'arg2'
-- +1 Error
-- + {name proc2}: ok dml_proc
-- + {call_stmt}: err
-- + {int 2}: err
-- + {name proc2}: ok dml_proc
call proc2(1, 2);

-- TEST: call known method with too many args
-- + Error % too many arguments provided to procedure 'proc2'
-- +1 Error
-- + {name proc2}: ok dml_proc
-- + {call_stmt}: err
-- + {name proc2}: ok dml_proc
call proc2(1, 'foo', 1);

-- TEST: call known method with too few args
-- + Error % too few arguments provided to procedure 'proc2'
-- +1 Error
-- + {name proc2}: ok dml_proc
-- + {call_stmt}: err
-- + {name proc2}: ok dml_proc
call proc2(1);

-- TEST: call on a method that had errors
-- + Error % procedure had errors, can't call 'proc3'
-- +1 Error
-- + {call_stmt}: err
-- + {name proc3}
-- - {name proc3}: ok
call proc3(1, 'foo');

-- test method with some out arguments, used in tests below
create procedure proc_with_output(in arg1 integer, inout arg2 integer, out arg3 integer)
begin
end;

-- TEST: can't use an integer for inout arg
-- + Error % formal cannot be fulfilled by non-variable 'arg2'
-- + {call_stmt}: err
-- +1 Error
call proc_with_output(1, 2, X);

-- TEST: can't use an integer for out arg
-- + Error % formal cannot be fulfilled by non-variable 'arg3'
-- + {call_stmt}: err
-- +1 Error
call proc_with_output(1, X, 3);

-- TEST: out values satisfied
-- - Error
-- + {call_stmt}: ok
-- + {int 1}: integer notnull
-- + {name X}: X: integer variable
-- + {name Y}: Y: integer variable
call proc_with_output(1, X, Y);

-- TEST: try to use an in/out arg in an out slot -> ok
-- - Error
-- + {create_proc_stmt}: ok
-- + {name proc_with_output}: ok
-- + {param_detail}: arg1: integer variable in out
-- + {name arg1}: arg1: integer variable in out
create procedure test_proc2(inout arg1 integer)
begin
  call proc_with_output(1, X, arg1);
end;

-- TEST: try to use an out arg in an out slot -> ok
-- - Error
-- + {create_proc_stmt}: ok
-- + {name proc_with_output}: ok
-- + {param_detail}: arg1: integer variable out
-- + {name arg1}: arg1: integer variable out
create procedure test_proc3(out arg1 integer)
begin
  call proc_with_output(1, X, arg1);
end;

-- TEST: try count function
-- - Error
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {name count}: integer notnull
-- + {star}: integer
select count(*) from foo;

-- TEST: try count distinct function
-- - Error
-- + {select_stmt}: select: { id: integer notnull }
-- + {name count}: id: integer notnull
-- + {distinct}
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
select count(distinct id) from foo;

-- TEST: try count distinct function with filter clause
-- - Error
-- + {select_stmt}: select: { id: integer notnull }
-- + {name count}: id: integer notnull
-- + {distinct}
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
select count(distinct id) filter (where id = 0) from foo;

-- TEST: try count distinct function with star
-- + {select_stmt}: err
-- + {call}: err
-- + {name count}
-- + Error % DISTINCT may only be used with one explicit argument in an aggregate function 'count'
-- +1 Error
select count(distinct *) from foo;

-- TEST: try sum functions
-- - Error
-- + {select_stmt}: select: { id: integer }
-- + {name sum}: id: integer
select sum(id) from foo;

-- TEST: try total functions
-- - Error
-- + {select_stmt}: select: { id: real notnull }
-- + {name total}: id: real notnull
select total(id) from foo;

-- TEST: try sum functions with too many param
-- + Error % function got incorrect number of arguments 'total'
-- +1 Error
-- + {select_stmt}: err
-- + {name total}: err
select total(id, rate) from bar;

-- TEST: try sum functions with star -- bogus
-- + Error % argument can only be used in count(*) '*'
-- +1 Error
-- + {star}: err
-- + {select_stmt}: err
-- + {name sum}
select sum(*) from foo;

-- TEST: try average, this should give a real
-- + {select_stmt}: select: { id: real }
-- + {name avg}: id: real
-- - Error
select avg(id) from foo;

-- TEST: try min, this should give an integer
-- + {select_stmt}: select: { id: integer }
-- + {name min}: id: integer
-- - Error
select min(id) from foo;

-- TEST: bogus number of arguments in count
-- + Error % function got incorrect number of arguments 'count'
-- +1 Error
-- + {call}: err
-- + {assign}: err
set X := (select count(1,2) from foo);

-- TEST: bogus number of arguments in max
-- + Error % function got incorrect number of arguments 'max'
-- +1 Error
-- + {call}: err
-- + {assign}: err
set X := (select max() from foo);

-- TEST: bogus number of arguments in average
-- + Error % function got incorrect number of arguments 'avg'
-- +1 Error
-- + {call}: err
-- + {assign}: err
set X := (select avg(1,2) from foo);

-- TEST: bogus string type in average
-- + Error % argument must be numeric 'avg'
-- +1 Error
-- + {call}: err
-- + {assign}: err
set X := (select avg('foo') from foo);

-- TEST: bogus null literal in average
-- + Error % argument must be numeric 'avg'
-- +1 Error
-- + {call}: err
-- + {assign}: err
set X := (select avg(null) from foo);

-- TEST: assign select where statement to nullable variable
-- + {assign}: X: integer variable
-- - Error
set X := (select X*10 as v where v = 1);

-- TEST: assign select where statement to not null variable
-- + {assign}: X_not_null: integer notnull variable
-- + {name X_not_null}: X_not_null: integer notnull variable
-- + {select_stmt}: _anon: integer notnull
-- - Error
set X_not_null := (select 1 where 0);

-- TEST: bogus function
-- + Error % function not yet implemented 'some_unknown_function'
-- +1 Error
-- {select_stmt}: err
set X := (select some_unknown_function(null));

-- TEST: simple while statement
-- - Error
-- + {while_stmt}: ok
-- + {name X}: X: integer variable
while X
begin
  select 1;
end;

-- TEST: not numeric while
-- + Error % expected numeric expression 'WHILE'
-- + {strlit 'X'}: err
-- +1 Error
while 'X'
begin
  select 1;
end;

-- TEST: error in while block should be propogated up
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {while_stmt}: err
while X
begin
  select NOT 'x';
end;

-- TEST: try to make a nested proc
-- + Error % stored procedures cannot be nested 'bar'
-- +1 Error
-- The containing proc is also in error
-- +2 {create_proc_stmt}: err
create proc foo()
begin
   create proc bar()
   begin
     select 1;
   end;
end;

-- TEST: verify that a procedure that calls a DML proc is a DML proc
-- - Error
-- + {create_proc_stmt}: ok dml_proc
-- + {name proc1}: ok dml_proc
create proc calls_dml()
begin
  call proc1();  -- it does a select
end;

-- TEST: not much to go wrong with try/catch
-- - Error
-- + {trycatch_stmt}: ok
-- + {throw_stmt}: ok
begin try
  select 1;
end try;
begin catch
  throw;
end catch;

-- TEST: error in try block should be propogated to top of tree
-- + Error % string operand not allowed in 'NOT'
-- + {trycatch_stmt}: err
-- + {stmt_list}: err
-- +1 Error
begin try
  select not 'x';
end try;
begin catch
  throw;
end catch;

-- TEST: error in catch block should be propogated to top of tree
-- + Error % string operand not allowed in 'NOT'
-- + {trycatch_stmt}: err
-- + {stmt_list}: ok
-- + {stmt_list}: err
-- +1 Error
begin try
  throw;
end try;
begin catch
  select not 'x';
end catch;

-- TEST: this procedure will have a structured semantic type
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- - Error
-- +1 {select_expr_list}: select: { id: integer notnull, name: text, rate: longint }
create procedure with_result_set()
begin
  select * from bar;
end;

-- TEST: this procedure will have a structured semantic type
-- + {create_proc_stmt}: select: { A: integer notnull, B: real notnull } dml_proc
-- - Error
-- +2 {select_stmt}: select: { A: integer notnull, B: real notnull }
create procedure with_matching_result(i integer)
begin
  if i then
    select 1 as A, 2.5 as B;
  else
    select 3 as A, 4.7 as B;
  end if;
end;

-- TEST: this procedure will have have not matching arg types
-- + Error % in multiple select statements, all columns must be an exact type match (expected real notnull; found integer notnull) 'B'
-- + {select_expr_list}: select: { A: integer notnull, B: real notnull }
-- + {select_expr_list}: select: { A: integer notnull, B: integer notnull }
create procedure with_wrong_types(i integer)
begin
  if i then
    select 1 as A, 2.5 as B;
  else
    select 3 as A, 4 as B;
  end if;
end;

-- TEST: this procedure will have have not matching arg counts
-- + Error % in multiple select statements, all must have the same column count
-- + {select_expr_list}: select: { A: integer notnull, B: real notnull }
-- + {select_expr_list}: select: { A: integer notnull }
create procedure with_wrong_count(i integer)
begin
  if i then
    select 1 as A, 2.5 as B;
  else
    select 3 as A;
  end if;
end;

-- TEST: this procedure will have nullability mismatch
-- + Error % in multiple select statements, all columns must be an exact type match (including nullability) (expected integer notnull; found integer) 'A'
-- + {create_proc_stmt}: err
-- + {select_stmt}: select: { A: integer notnull variable in }
-- + {select_expr_list_con}: select: { A: integer variable }
create procedure with_wrong_flags(i integer not null)
begin
  if i then
    select i as A;
  else
    select X as A;
  end if;
end;

-- TEST: this procedure will match variables
-- + {create_proc_stmt}: select: { A: integer notnull variable in }
-- use the important fragment for the match, one is a variable so the tree is slightly different
-- +2 {select_expr_list}: select: { A: integer notnull
-- - Error
create procedure with_ok_flags(i integer not null)
begin
  if i then
    select i as A;
  else
    select 2 as A;
  end if;
end;

-- TEST: this procedure will not match column names
-- + Error % in multiple select statements, all column names must be identical so they have unambiguous names 'B'
-- + {create_proc_stmt}: err
-- + {select_stmt}: select: { A: integer notnull }
-- + {select_expr_list_con}: select: { B: integer notnull }
create procedure with_bad_names(i integer not null)
begin
  if i then
    select 1 as A;
  else
    select 2 as B;
  end if;
end;

-- TEST: this procedure doesn't specify a name for the result
-- + Error % all columns in the select must have a name
-- + {create_proc_stmt}: err
-- + {stmt_list}: err
-- + {select_expr_list_con}: select: { _anon: integer notnull }
create procedure with_no_names(i integer not null)
begin
  select 1;
end;

-- TEST: good cursor
-- + {declare_cursor}: curs: select: { id: integer notnull, name: text, rate: longint } variable
-- + {name with_result_set}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- - Error
declare curs cursor for call with_result_set();

-- TEST: bad invocation, needs cursor
-- + Error % procedures with results can only be called using a cursor in global context 'with_result_set'
-- {call_stmt}: err
call with_result_set();

-- TEST: bad invocation, bogus method
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {declare_cursor}: err
-- + {call_stmt}: err
declare curs cursor for call bogus_call(not 'x');

-- TEST: bad invocation, this method doesn't return a result set
-- + Error % cursor requires a procedure that returns a result set via select 'curs'
-- + {declare_cursor}: err
-- + {name proc1}: ok dml_proc
declare curs cursor for call proc1();

-- TEST: full join with all expression options, including offset
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, name: text, rate: longint }
-- + {opt_where}: bool notnull
-- + {groupby_list}: ok
-- + {opt_having}: bool
-- + {opt_orderby}: ok
-- + {opt_limit}: integer notnull
-- + {opt_offset}: integer notnull
select * from foo as T1
inner join bar as T2 on T1.id = T2.id
where T2.id > 5
group by T2.name asc, T2.id desc
having T2.name = 'x'
order by T2.rate
limit 5
offset 7;

-- TEST: full join with all expression options and bogus offset
-- + Error % expected numeric expression 'OFFSET'
-- +1 Error
-- + {select_stmt}: err
-- + {opt_offset}: err
select * from foo as T1
inner join bar as T2 on T1.id = T2.id
where T2.id > 5
group by T2.name
having T2.name = 'x'
order by T2.rate
limit 5
offset 'x';

-- TEST: You can't aggregate if there is no FROM clause, try that out for count
-- + Error % aggregates only make sense if there is a FROM clause 'count'
select count(1);

-- TEST: checking use of aggregates in the wrong context (not allowed in where)
-- + Error % function may not appear in this context 'count'
-- +1 Error
-- + {select_stmt}: err
select * from foo where count(*) == 1;

-- TEST: You can't aggregate if there is no FROM clause, try that out for max
-- + Error % aggregates only make sense if there is a FROM clause 'max'
select max(1);

-- TEST: You can't aggregate if there is no FROM clause, try that out for avg
-- + Error % aggregates only make sense if there is a FROM clause 'avg'
select avg(1);

-- TEST: assign a not null to a nullable output, that's ok.
-- + {create_proc_stmt}: ok
-- - Error
-- + {param_detail}: result: integer variable out
-- + {assign}: result: integer variable out
-- + {int 5}: integer notnull
create proc out_proc(out result integer)
begin
  set result := 5;
end;

-- TEST: Set up a not null int for the tested
-- + {name my_int}: my_int: integer notnull variable
declare my_int int not null;

-- TEST: my_int is not nullable, must be exact match in out parameter, ordinarily this would be compatible
-- + Error % cannot assign/copy possibly null expression to not null target 'my_int'
call out_proc(my_int);

-- TEST: my_real is real, must be exact match in out parameter, ordinarily this would be compatible
-- + {name my_real}: my_real: real variable
declare my_real real;

-- TEST: Try to make the call with a bogus out arg now
-- + Error % proc out parameter: arg must be an exact type match (expected integer; found real) 'my_real'
call out_proc(my_real);

-- TEST: try an exists clause
-- + {select_stmt}: select: { id: integer notnull }
-- + {exists_expr}: bool notnull
-- - Error
select * from foo where exists (select * from foo);

-- TEST: try a not exists clause
-- + {select_stmt}: select: { id: integer notnull }
-- + {not}: bool notnull
-- + {exists_expr}: bool notnull
-- - Error
select * from foo where not exists (select * from foo);

-- TEST: try an exists clause with an error
-- + Error % string operand not allowed in 'NOT'
-- only one error reported
-- +1 Error
-- + {exists_expr}: err
select * from foo where exists (select not 'x' from foo);

-- TEST: try a not exists clause with an error
-- + Error % string operand not allowed in 'NOT'
-- only one error reported
-- +1 Error
-- + {exists_expr}: err
select * from foo where not exists (select not 'x' from foo);

-- TEST: try to use exists in a bogus place
-- + Error % exists_expr % function may not appear in this context 'exists'
-- + {exists_expr}: err
-- + {assign}: err
set X := exists(select * from foo);

-- TEST: try to use not exists in a bogus place
-- + Error % function may not appear in this context 'exists'
-- + {not}: err
-- + {exists_expr}: err
-- + {assign}: err
set X := not exists(select * from foo);

-- TEST: release a savepoint out of the blue
-- + Error % savepoint has not been mentioned yet, probably wrong 'garbonzo'
-- + {release_savepoint_stmt}: err
release savepoint garbonzo;

-- TEST: rollback to  a savepoint out of the blue
-- + Error % savepoint has not been mentioned yet, probably wrong 'another_garbonzo'
-- + {rollback_trans_stmt}: err
rollback transaction to savepoint another_garbonzo;

-- TEST: Test the shorthand syntax for cursors. The shape_storage flag for the
-- cursor itself comes from the following fetch statement.
-- + {declare_cursor}: shape_storage: select: { one: integer notnull, two: integer notnull } variable
-- + {name shape_storage}: shape_storage: select: { one: integer notnull, two: integer notnull } variable shape_storage
-- - Error
declare shape_storage cursor for select 1 as one, 2 as two;

-- TEST: Fetch the auto cursor
-- + {fetch_stmt}: shape_storage: select: { one: integer notnull, two: integer notnull } variable
-- + {name shape_storage}: shape_storage: select: { one: integer notnull, two: integer notnull } variable shape_storage
-- - Error
fetch shape_storage;

-- TEST: Now access the cursor
-- + {select_stmt}: select: { shape_storage.one: integer notnull variable }
-- + {dot}: shape_storage.one: integer notnull variable
-- + {name shape_storage}
-- + {name one}
-- -Error
select shape_storage.one;

-- TEST: a field that is not present
-- + Error % field not found in cursor 'three'
-- + {dot}: err
-- + {name shape_storage}
-- + {name three}
select shape_storage.three;

-- TEST: a cursor that did not use the auto-cursor feature
-- + Error % cursor was not used with 'fetch [cursor]' 'my_cursor'
-- + {dot}: err
-- + {name my_cursor}
-- + {name one}
select my_cursor.one;

-- TEST: test the join using syntax
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- + {select_from_etc}: JOIN { T1: foo, T2: foo }
-- + {using}
-- + {name id}
select * from foo as T1 inner join foo as T2 using(id);

-- TEST: duplicate column names
-- + Error % duplicate name in list 'id'
-- +1 Error
-- + {select_stmt}: err
select * from foo as T1 inner join foo as T2 using(id, id);

-- TEST: invalid column names (missing on the left)
-- + Error % join using column not found on the left side of the join 'idx'
-- +1 Error
select * from foo as T1 inner join foo as T2 using(id, idx);

-- TEST: invalid column names (missing on the right)
-- + Error % join using column not found on the right side of the join 'name'
-- +1 Error
select * from bar as T1 inner join foo as T2 using(id, name);

-- TEST: helper tables for different join types

-- {create_table_stmt}: payload1: { id: integer notnull, data1: integer notnull }
-- -Error
create table payload1 (id integer not null, data1 integer not null);

-- {create_table_stmt}: payload2: { id: integer notnull, data2: integer notnull }
-- -Error
create table payload2 (id integer not null, data2 integer not null);

-- TEST: all not null
-- {select_stmt}: select: { id: integer notnull, data1: integer notnull, id: integer notnull, data2: integer notnull }
-- - Error
select * from payload1 inner join payload2 using (id);

-- TEST: right part nullable
-- + {select_stmt}: select: { id: integer notnull, data1: integer notnull, id: integer, data2: integer }
-- - Error
select * from payload1 left outer join payload2 using (id);

-- TEST: left part nullable
-- + {select_stmt}: select: { id: integer, data1: integer, id: integer notnull, data2: integer notnull }
-- - Error
select * from payload1 right outer join payload2 using (id);

-- TEST: both parts nullable due to cross join
-- + {select_stmt}: select: { id: integer, data1: integer, id: integer, data2: integer }
-- - Error
select * from payload1 cross join payload2 using (id);

-- TEST: compound select
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, id: integer notnull, id: integer notnull }
-- + {select_from_etc}: JOIN { A: foo, B: foo, C: foo, D: foo }
-- - Error
select * from (foo A, foo B) inner join (foo C, foo D);

-- TEST: select with embedded error in an interior join
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {select_stmt}: err
-- +2 {join_clause}: err
select id from (foo inner join bar on not 'x') inner join foo on 1;

-- TEST: simple ifnull : note X is nullable
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {name X}: X: integer variable
-- - Error
select ifnull(X, 0);

-- TEST: simple coalesce with not null result, note X,Y are nullable
-- + {select_stmt}: select: { _anon: real notnull }
-- + {call}: real notnull
-- + {name coalesce}
-- + {name X}: X: integer variable
-- + {name Y}: Y: integer variable
-- + {dbl 1.5%}: real notnull
select coalesce(X, Y, 1.5);

-- TEST: null in a coalesce is obviously wrong
-- + Error % Null literal is useless in function 'coalesce'
-- + {call}: err
-- + {null}: err
-- + {select_stmt}: err
select coalesce(X, null, 1.5);

-- TEST: not null before the end is obviously wrong
-- + Error % encountered arg known to be not null before the end of the list
-- +1 Error
-- + {call}: err
-- + {name coalesce}
select coalesce(X, 5, 1.5);

-- TEST: wrong number of args (too many)
-- + Error % Incorrect number of arguments 'ifnull'
-- + {call}: err
-- + {name ifnull}
-- +1 Error
select ifnull(X, 5, 1.5);

-- TEST: wrong number of args (too few)
-- + Error % Too few arguments provided 'ifnull'
-- + {call}: err
-- + {name ifnull}
-- +1 Error
select ifnull(5);

-- TEST: not compatible args in ifnull
-- + Error % incompatible types in expression 'ifnull'
-- + {call}: err
-- + {name ifnull}
-- +1 Error
select ifnull(X, 'hello');

-- TEST: error in expression in ifnull
-- + Error % string operand not allowed in 'NOT'
-- + {call}: err
-- + {name ifnull}
-- + {arg_list}: err
-- +1 Error
select ifnull(not 'x', not 'hello');

-- TEST: make make an FK with the column count wrong
-- + Error % The number of columns on both sides of a foreign key must match
-- + {create_table_stmt}: err
-- + fk_def}: err
create table fk_table_2 (
  id1 integer,
  id2 integer,
  FOREIGN KEY (id1, id2) REFERENCES foo(id)
);

-- TEST: make make an FK with the column types not matching
-- + Error % the exact type of both sides of a foreign key must match (expected real; found integer notnull) 'id'
-- + {create_table_stmt}: err
-- + fk_def}: err
create table fk_table_2 (
  id REAL,
  FOREIGN KEY (id) REFERENCES foo(id)
);

-- TEST: helper table for join/using test
-- + {create_table_stmt}: join_clause_1: { id: real }
create table join_clause_1 (
  id REAL
);

-- TEST: helper table for join/using test
-- {create_table_stmt}: join_clause_1: { id: integer }
create table join_clause_2 (
  id integer
);

-- TEST: join using syntax with column type mismatch test
-- + Error % left/right column types in join USING(...) do not match exactly 'id'
-- + {table_or_subquery}: TABLE { join_clause_1: join_clause_1 }
-- + {table_or_subquery}: TABLE { join_clause_2: join_clause_2 }
-- + {join_clause}: err
-- + {select_stmt}: err
select * from join_clause_1 inner join join_clause_2 using(id);

-- TEST: use last insert rowid, validate it's ok
-- + {select_stmt}: select: { _anon: longint notnull }
-- + {name last_insert_rowid}: longint notnull
-- - Error
select last_insert_rowid();

-- TEST: last_insert_row doesn't take args
-- + Error % function got incorrect number of arguments 'last_insert_rowid'
-- + {name last_insert_rowid}: err
-- + {select_stmt}: err
select last_insert_rowid(1);

-- TEST: last_insert_rowid is not ok in a limit
-- + Error % function may not appear in this context 'last_insert_rowid'
-- + {call}: err
-- + {select_stmt}: err
select * from foo limit last_insert_rowid();

-- declare result for last_insert_rowid
declare rowid_result long int not null;

-- TEST: set last_insert_rowid outside of select statement
-- + {assign}: rowid_result: longint notnull variable
-- + {name rowid_result}: rowid_result: longint notnull variable
-- + {call}: longint notnull
-- + {name last_insert_rowid}: longint notnull
-- - Error
set rowid_result := last_insert_rowid();

-- TEST: use changes, validate it's ok
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {name changes}: integer notnull
-- - Error
select changes();

-- TEST: changes doesn't take args
-- + Error % function got incorrect number of arguments 'changes'
-- + {name changes}: err
-- + {select_stmt}: err
select changes(1);

-- TEST: changes is not ok in a limit
-- + Error % function may not appear in this context 'changes'
-- + {call}: err
-- + {select_stmt}: err
select * from foo limit changes();

-- declare result for changes function
declare changes_result int not null;

-- TEST: set changes outside of select statement
-- + {assign}: changes_result: integer notnull variable
-- + {name changes_result}: changes_result: integer notnull variable
-- + {call}: integer notnull
-- + {name changes}: integer notnull
-- - Error
set changes_result := changes();

-- TEST: printf is ok in a select
-- + {select_stmt}: select: { _anon: text notnull }
-- + {select_expr}: text notnull
-- + {name printf}: text notnull
-- - Error
select printf('%s %d', 'x', 5);

-- TEST: printf is ok in a loose expression
-- + {assign}: a_string: text variable
-- + {name printf}: text notnull
-- - Error
set a_string := printf('Hello');

-- TEST: printf needs at least one arg
-- + Error % function got incorrect number of arguments 'printf'
-- + {name printf}: err
-- + {assign}: err
set a_string := printf();

-- TEST: printf first arg must be a string
-- + Error % first argument must be a string in function 'printf'
-- + {call}: err
-- {assign}: err
set a_string := printf(7);

-- TEST: printf is not ok in a limit
-- + Error % function may not appear in this context 'printf'
-- + {opt_limit}: err
-- + {select_stmt}: err
select 1 from (select 1) limit printf('%s %d', 'x', 5) == 'x';

-- TEST: update with duplicate columns
-- + Error % duplicate target column name in update statement 'id'
-- + {update_stmt}: err
-- + {name id}: err
update foo set id = 1, id = 3 where id = 2;

-- TEST: bogus number of arguments in sum
-- + Error % function got incorrect number of arguments 'sum'
-- + {assign}: err
-- + {call}: err
set X := (select sum(1,2) from foo);

-- TEST: sum used in a limit, bogus
-- + Error % function may not appear in this context 'sum'
-- + {assign}: err
-- + {call}: err
set X := (select id from foo limit sum(1));

-- TEST: sum used with text
-- + Error % argument must be numeric 'sum'
-- + {assign}: err
-- + {call}: err
set X := (select sum('x') from foo);

-- tables for the following test
create table A1(foo int);
create table B1(foo int);
create table C1(foo int);

-- TEST: duplicate table name logic needs different left and right table counts
--       this test case with 3 tables will have one join with 2 on the left 1
--       on the right
-- - Error
-- + {select_from_etc}: JOIN { T1: A1, T2: B1, T3: C1 }
select * from A1 as T1
left outer join B1 as T2 on T1.foo = t2.foo
left outer join C1 as T3 on T2.foo = t3.foo;

-- TEST: group_concat basic correct case
-- - Error
-- + {select_stmt}: select: { id: integer notnull, name: text }
-- +  {name group_concat}: name: text
select id, group_concat(name) from bar group by id;

-- TEST: group_concat with second arg
-- - Error
-- + {select_stmt}: select: { id: integer notnull, name: text }
-- +  {name group_concat}: name: text
select id, group_concat(name, 'x') from bar group by id;

-- TEST: group_concat with bogus second arg
-- + Error % second argument must be a string in function 'group_concat'
-- +1 Error
-- + {select_stmt}: err
select id, group_concat(name, 0) from bar group by id;

-- TEST: group_concat with zero args
-- + Error % function got incorrect number of arguments 'group_concat'
-- +1 Error
-- + {select_stmt}: err
select id, group_concat() from bar group by id;

-- TEST: group_concat with three args
-- + Error % function got incorrect number of arguments 'group_concat'
-- +1 Error
-- + {select_stmt}: err
select id, group_concat('x', 'y', 'z') from bar group by id;

-- TEST: group_concat outside of aggregate context
-- + Error % function may not appear in this context 'group_concat'
-- +1 Error
-- + {select_stmt}: err
select id from bar where group_concat(name) = 'foo';

-- TEST: strftime basic correct case
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {name strftime}: text notnull
select strftime('%s', 'now');

-- TEST: strftime with a modifier
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name strftime}: text
select strftime('%YYYY-%mm-%DDT%HH:%MM:%SS.SSS', 'now', '+1 month');

-- TEST: strftime with multiple modifiers
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name strftime}: text
select strftime('%W', 'now', '+1 month', 'start of month', '-3 minutes', 'weekday 4');

-- TEST: strftime with non-string modifier
-- + Error
-- +1 Error
-- + {select_stmt}: err
select strftime('%s', 'now', 3);

-- TEST: strftime with bogus format
-- + Error % all arguments must be strings 'strftime'
-- +1 Error
-- + {select_stmt}: err
select strftime(42, 'now');

-- TEST: strftime with bogus timestring
-- + Error % all arguments must be strings 'strftime'
-- +1 Error
-- + {select_stmt}: err
select strftime('%s', 42);

-- TEST: strftime is not ok in a loose expression
-- + Error % function may not appear in this context 'strftime'
-- +1 Error
-- + {assign}: err
-- + {name strftime}
set a_string := strftime('%s', 'now');

-- TEST: strftime without enough arguments
-- + Error % function got incorrect number of arguments 'strftime'
-- +1 Error
-- + {select_stmt}: err
select strftime('now');

-- TEST: date basic correct case
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {name date}: text notnull
select date('now');

-- TEST: date with a modifier
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name date}: text
select date('now', '+1 month');

-- TEST: date with multiple modifiers
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name date}: text
select date('now', '+1 month', 'start of month', '-3 minutes', 'weekday 4');

-- TEST: date with non-string modifier
-- + Error
-- +1 Error
-- + {select_stmt}: err
select date('now', 3);

-- TEST: date with bogus timestring
-- + Error % all arguments must be strings 'date'
-- +1 Error
-- + {select_stmt}: err
select date(42);

-- TEST: date is not ok in a loose expression
-- + Error % function may not appear in this context 'date'
-- +1 Error
-- + {assign}: err
-- + {name date}
set a_string := date('now');

-- TEST: date without enough arguments
-- + Error % function got incorrect number of arguments 'date'
-- +1 Error
-- + {select_stmt}: err
select date();

-- TEST: time basic correct case
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {name time}: text notnull
select time('now');

-- TEST: time with a modifier
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name time}: text
select time('now', '+1 month');

-- TEST: time with multiple modifiers
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name time}: text
select time('now', '+1 month', 'start of month', '-3 minutes', 'weekday 4');

-- TEST: time with non-string modifier
-- + Error
-- +1 Error
-- + {select_stmt}: err
select time('now', 3);

-- TEST: time with bogus timestring
-- + Error % all arguments must be strings 'time'
-- +1 Error
-- + {select_stmt}: err
select time(42);

-- TEST: time is not ok in a loose expression
-- + Error % function may not appear in this context 'time'
-- +1 Error
-- + {assign}: err
-- + {name time}
set a_string := time('now');

-- TEST: time without enough arguments
-- + Error % function got incorrect number of arguments 'time'
-- +1 Error
-- + {select_stmt}: err
select time();

-- TEST: datetime basic correct case
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {name datetime}: text notnull
select datetime('now');

-- TEST: datetime with a modifier
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name datetime}: text
select datetime('now', '+1 month');

-- TEST: datetime with multiple modifiers
-- - Error
-- + {select_stmt}: select: { _anon: text }
-- + {name datetime}: text
select datetime('now', '+1 month', 'start of month', '-3 minutes', 'weekday 4');

-- TEST: datetime with non-string modifier
-- + Error
-- +1 Error
-- + {select_stmt}: err
select datetime('now', 3);

-- TEST: datetime with bogus timestring
-- + Error % all arguments must be strings 'datetime'
-- +1 Error
-- + {select_stmt}: err
select datetime(42);

-- TEST: datetime is not ok in a loose expression
-- + Error % function may not appear in this context 'datetime'
-- +1 Error
-- + {assign}: err
-- + {name datetime}
set a_string := datetime('now');

-- TEST: datetime without enough arguments
-- + Error % function got incorrect number of arguments 'datetime'
-- +1 Error
-- + {select_stmt}: err
select datetime();

-- TEST: julianday basic correct case
-- - Error
-- + {select_stmt}: select: { _anon: real notnull }
-- + {name julianday}: real notnull
select julianday('now');

-- TEST: julianday with a modifier
-- - Error
-- + {select_stmt}: select: { _anon: real }
-- + {name julianday}: real
select julianday('now', '+1 month');

-- TEST: julianday with multiple modifiers
-- - Error
-- + {select_stmt}: select: { _anon: real }
-- + {name julianday}: real
select julianday('now', '+1 month', 'start of month', '-3 minutes', 'weekday 4');

-- TEST: julianday with non-string modifier
-- + Error
-- +1 Error
-- + {select_stmt}: err
select julianday('now', 3);

-- TEST: julianday with bogus timestring
-- + Error % all arguments must be strings 'julianday'
-- +1 Error
-- + {select_stmt}: err
select julianday(42);

-- TEST: julianday is not ok in a loose expression
-- + Error % function may not appear in this context 'julianday'
-- +1 Error
-- + {assign}: err
-- + {name julianday}
set a_string := julianday('now');

-- TEST: julianday without enough arguments
-- + Error % function got incorrect number of arguments 'julianday'
-- +1 Error
-- + {select_stmt}: err
select julianday();

-- TEST: simple cast expression
-- - Error
-- + {select_stmt}: select: { _anon: text notnull }
-- + {cast_expr}: text notnull
select cast(1 as text);

-- TEST: cast expression in bogus context
-- + Error % CAST may only appear in the context of SQL statement
-- +1 Error
-- + {cast_expr}: err
set X := cast(5.0 as text);

-- TEST: cast expression with expression error
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
-- + {cast_expr}: err
select cast(not 'x' as int);

-- TEST: create table with PK to force not null
-- - Error
-- + {create_table_stmt}: pk_test: { id: integer notnull primary_key }
-- + {col_def}: id: integer notnull
create table pk_test(id integer primary key);

-- TEST: create table with PK out of line to force not null
-- - Error
-- semantic type and coldef must both be notnull
-- + {create_table_stmt}: pk_test_2: { id: integer notnull }
-- + {col_def}: id: integer notnull
create table pk_test_2(
  id integer,
  PRIMARY KEY (id)
);

-- TEST: ensure that table factors are visible in order
create table AA1(id1 int not null);
create table BB2(id2 int not null);
create table CC3(id3 int not null);

-- - Error
-- + {select_stmt}: select: { id1: integer notnull, id2: integer notnull, id3: integer }
SELECT *
FROM (AA1 A, BB2 B)
LEFT OUTER JOIN CC3 C ON C.id3 == A.id1;

-- TEST: declare procedure basic
-- - Error
-- + {declare_proc_stmt}: ok
-- + {name decl1}: ok
-- - decl1%dml
-- + {params}: ok
-- + {param}: id: integer variable in
declare proc decl1(id integer);

-- TEST: declare procedure with DB params
-- - Error
-- + {declare_proc_stmt}: ok dml_proc
-- + {name decl2}: ok dml_proc
-- + {param}: id: integer variable in
declare proc decl2(id integer) using transaction;

-- TEST: declare procedure with select result set
-- - Error
-- + declare_proc_stmt}: decl3: { A: integer notnull, B: bool } dml_proc
declare proc decl3(id integer) ( A integer not null, B bool );

-- TEST: try an arg bundle inside of a declared proc
-- make sure the rewrite was accurate
-- + DECLARE PROC decl4 (x_A INTEGER NOT NULL, x_B BOOL);
-- - Error
declare proc decl4(x like decl3);

-- TEST: declare inside of a proc
-- + Error
-- +1 Error
-- + Error % declared procedures must be top level 'yy'
-- + {create_proc_stmt}: err
create proc bogus_nested_declare()
begin
 declare proc yy();
end;

-- TEST: duplicate declaration, all matches
-- + DECLARE PROC decl1 (id INTEGER);
-- + param}: id: integer variable in
-- + {declare_proc_stmt}: ok
-- - Error
declare proc decl1(id integer);

-- TEST: duplicate declaration, mismatch
-- + Error % in declare_proc_stmt % procedure declarations/definitions do not match 'decl1'
-- + {declare_proc_stmt}: err
declare proc decl1(id integer not null);

-- TEST: bogus parameters
-- + Error % duplicate parameter name 'id'
-- +1 Error
-- + {declare_proc_stmt}: err
-- + {params}: err
declare proc bogus_duplicate_params(id integer, id integer);

-- TEST: declare procedure with select error
-- + Error % duplicate column name 'id'
-- +1 Error
-- + {declare_proc_stmt}: err
-- + {params}: ok
-- + {typed_names}: err
declare proc bogus_select_list(id integer) (id integer, id integer);

-- TEST: subquery within in clause
-- - Error
-- + {in_pred}: bool notnull
-- + {select_from_etc}: TABLE { bar: bar }
select id from foo where id in (select id from bar);

-- TEST: subquery within in clause with multiple columns
-- +1 Error
-- + nested select expression must return exactly one column
-- + {select_stmt}: err
select id from foo where id in (select id, id from bar);

-- TEST: subquery within in clause with wrong type
-- +1 Error
-- + incompatible types in expression 'IN'
-- + {select_stmt}: err
select id from foo where id in (select name from bar);

-- TEST: subquery within not in clause
-- - Error
-- + {not_in}: bool notnull
-- + {select_from_etc}: TABLE { bar: bar }
select id from foo where id not in (select id from bar);

-- TEST: subquery within not in clause with wrong type
-- +1 Error
-- + incompatible types in expression 'NOT IN'
-- + {select_stmt}: err
select id from foo where id not in (select name from bar);

-- TEST: basic union pattern
-- - Error
-- + {select_core_list}: union: { A: integer notnull, B: integer notnull }
select 1 as A, 2 as B
union
select 3 as A, 4 as B;

-- TEST: basic union all pattern
-- - Error
-- + {select_core_list}: union_all: { A: integer notnull, B: integer notnull }
select 1 as A, 2 as B
union all
select 3 as A, 4 as B;

-- TEST: union all with not matching columns
-- + Error % if multiple selects, all column names must be identical so they have unambiguous names 'B'
-- +1 Error
select 1 as A, 2 as C
union all
select 3 as A, 4 as B;

-- TEST: union all with not matching types (but compat)
-- + {select_core_list}: union_all: { A: integer notnull, B: real notnull }
-- - Error
select 1 as A, 2 as B
union all
select 3 as A, 4.3 as B;

-- TEST: union all with error on the left
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
select not 'x' as A
union all
select 1 as A;

-- TEST: union all with error on the right
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
select 'x' as A
union all
select not 'x' as A;

-- TEST: compound operator intersect
-- - Error
-- + {select_core_list}: intersect: { A: integer notnull, B: integer notnull }
select 1 as A, 2 as B
intersect
select 3 as A, 4 as B;

-- TEST: compound operator except
-- - Error
-- + {select_core_list}: except: { A: integer notnull, B: integer notnull }
select 1 as A, 2 as B
except
select 3 as A, 4 as B;

-- TEST: use nullable in a select
-- - Error
select nullable(1);

-- helper variable
declare sens_notnull text not null @sensitive;

-- TEST: ensure nullable() doesn't strip the sensitive bit
-- notnull is gone, sensitive stays
-- + {select_stmt}: select: { sens_notnull: text variable sensitive }
-- + {name sens_notnull}: sens_notnull: text notnull variable sensitive
-- - Error
select nullable(sens_notnull);

-- TEST: ensure kind is preserved in nullable
-- + {select_stmt}: select: { price_e: real<euros> variable }
-- + {name nullable}: price_e: real<euros> variable
-- - Error
select nullable(price_e);

-- TEST: affirmative error generated after nullable with kind
-- + {assign}: err
-- + Error % expressions of different kinds can't be mixed: 'dollars' vs. 'euros'
-- +1 Error
set price_d := (select nullable(price_e));

-- TEST: use nullable in a select with wrong args
-- + Error % function got incorrect number of arguments 'nullable'
-- +1 Error
select nullable(1, 2);

-- try some const cases especially those with errors

-- TEST: variables not allowed in constant expressions (duh)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x);

-- TEST: divide by zero yields error in all forms (integer)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1/0);

-- TEST: divide by zero yields error in all forms (real)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1/0.0);

-- TEST: divide by zero yields error in all forms (long)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1/0L);

-- TEST: divide by zero yields error in all forms (bool)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1/not 1);

-- TEST: divide by zero yields error in all forms (integer)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1%0);

-- TEST: divide by zero yields error in all forms (long)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1%0L);

-- TEST: divide by zero yields error in all forms (bool)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1%not 1);

-- TEST: variables not allowed in constant expressions (duh)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(case x when 1 then 2 end);

-- TEST: variables not allowed in constant expressions (duh)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(case 1 when x then 2 end);

-- TEST: variables not allowed in constant expressions (duh)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(case 1 when 1 then x end);

-- TEST: variables not allowed in constant expressions (duh)
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(case when x then 2 end);

-- TEST: non integer arguments not allowed
-- + {const}: err
-- + Error % operands must be an integer type, not real '~'
-- +1 Error
select const(~1.3);

-- TEST: forcing errors in binary operators to make them prop:  comparison type
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x == x);

-- TEST: forcing errors in binary operators to make them prop:  is/is_not comparison type
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x is x);

-- TEST: forcing errors in binary operators to make them prop:  normal binary
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x + x);

-- TEST: forcing errors in binary operators to make them prop:  and error in first arg
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x and 0);

-- TEST: forcing errors in binary operators to make them prop:  and error in second arg
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1 and x);

-- TEST: forcing errors in binary operators to make them prop:  or: error in first arg
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(x or 0);

-- TEST: forcing errors in binary operators to make them prop:  or: force error in second arg
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(0 or x);

-- TEST: forcing errors in binary operators to make them prop:  and: force error in 2nd arg
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(1 and x);

-- TEST: forcing errors in cast
-- + {const}: err
-- + Error % evaluation of constant failed
-- +1 Error
select const(cast(x as real));

-- TEST: use nullable in the wrong context
-- + Error % function may not appear in this context 'nullable'
-- +1 Error
select 1 from bar where nullable(1) = 1;

-- TEST: with expression, duplicate columnms
-- + Error % duplicate name in list 'a'
-- +1 Error
with foo(a, a) as (select 1,2)
select 1;

-- TEST: with expression, duplicate cte name
-- + Error % duplicate common table name 'foo'
-- +1 Error
with
 foo(a, b) as (select 1,2),
 foo(a, b) as (select 1,2)
select 1;

-- TEST: with expression, too few columns
-- + Error % too few column names specified in common table expression 'foo'
-- +1 Error
with foo(a) as (select 1,2)
select 1;

-- TEST: with expression, too few columns
-- + Error % too many column names specified in common table expression 'foo'
-- +1 Error
with foo(a, b, c) as (select 1,2)
select 1;

-- TEST: with expression, broken inner select
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
with foo(a) as (select not 'x')
select 1;

-- TEST: with expression, broken inner select
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
with foo(a) as (select 1)
select not 'x';

-- TEST: basic with expression
-- - Error
with some_cte(a, b) as (select 1,2)
select a, b from some_cte;

-- TEST: make sure that the overall result of the CTE is nullable
-- even if the first branch of the CTE (which is its provisional definition)
-- is not nullable
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- + {with_select_stmt}: select: { a: integer }
-- + {cte_tables}: ok
-- + {cte_table}: foo: { a: integer }
-- + {cte_decl}: foo: { a: integer }
-- + {select_stmt}: union_all: { x: integer }
-- + {select_core}: select: { x: integer notnull }
-- + {select_core}: select: { x: null }
-- - Error
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
-- WARNING easily broken do not change this test especially not nullability
with
  foo(a) as (select 1 x union all select null x)
  select * from foo;

-- TEST: nested CTE -- note scoping
-- - Error
-- +2 {cte_table}: y: { a: integer notnull, b: integer notnull }
with x(a,b) as (select 1,2)
select * from x as X
inner join ( with y(a,b) as (select 1,3) select * from y ) as Y
on X.a = Y.a
inner join ( with y(a,b) as (select 1,3) select * from y ) as Z
on X.a = Z.a;

-- TEST: with recursive
-- - Error
-- + {with_select_stmt}: select: { current: integer notnull }
-- + {with_recursive}
-- + {cte_decl}: cnt: { current: integer notnull }
with recursive
  cnt(current) AS (
     select 1
     union all
     select current+1 from cnt
     limit 10
  )
select current from cnt;

-- TEST: with recursive with error in the definition
-- + Error % duplicate name in list 'current'
-- +1 Error
with recursive
  cnt(current, current) AS (
     select 1
     union all
     select current+1 from cnt
     limit 10
  )
select current from cnt;

-- TEST: with recursive error in the base case
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
with recursive
  cnt(current) AS (
     select not 'x'
     union all
     select current+1 from cnt
     limit 10
  )
select current from cnt;

-- TEST: with recursive error in the main case
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
with recursive
  cnt(current) AS (
     select 1
     union all
     select not 'x'
  )
select current from cnt;

-- TEST: with recursive error in the output select
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
with recursive
  cnt(current) AS (
     select 1
     union all
     select current+1 from cnt
     limit 10
  )
select not 'x';

-- TEST: verify the shape of tree with many unions
-- here we're checking to make sure Y is loose at the end of the chain
-- and the two X variables came before in the tree
-- +2 |   | {name X}: X: integer variable
-- + | {name Y}: Y: integer variable
select X as A
union all
select X as A
union all
select Y as A;

-- TEST: verify that we can create a view that is based on a CTE
-- -Error
-- + {with_select_stmt}: view_with_cte: { x: integer notnull }
-- + {cte_table}: goo: { x: integer notnull }
create view view_with_cte as
with
 goo(x) as (select 1)
select * from goo;

-- TEST: verify that we can use non-simple selects inside of an IN
-- - Error
-- + {in_pred}: bool notnull
-- + {select_stmt}: _anon: integer
select 1 in (select 1 union all select 2 union all select 3) as A;

-- TEST: use table.* syntax to get one table
-- - Error
-- + {select_stmt}: select: { _first: integer notnull, A: integer notnull, B: integer notnull, _last: integer notnull }
-- + {table_star}: T: select: { A: integer notnull, B: integer notnull }
select 0 as _first, T.*, 3 as _last from (select 1 as A, 2 as B) as T;

-- TEST: use table.* syntax to get two tables
-- - Error
-- + {table_star}: T: select: { A: integer notnull, B: integer notnull }
-- + {select_stmt}: select: { _first: integer notnull, A: integer notnull, B: integer notnull, C: integer notnull, _last: integer notnull }
select 0 as _first, T.*, S.*, 3 as _last from (select 1 as A, 2 as B) as T, (select 1 as C) as S;

-- TEST: try to use T.* with no from clause
-- + Error % select [table].* cannot be used with no FROM clause
-- + {table_star}: err
select T.*;

-- TEST: try to use T.* where T does not exist
-- + Error % table not found 'T'
-- + {table_star}: err
select T.* from (select 1) as U;


-- TEST: simple test for declare function
-- + name simple_func}: real notnull
-- + {param}: arg1: integer notnull variable in
-- + {params}: ok
-- + DECLARE FUNC simple_func (arg1 integer not null) REAL not null;
-- - Error
declare function simple_func(arg1 integer not null) real not null;

-- TEST: error duplicate function
-- + Error % duplicate function name 'simple_func'
-- +1 Error
declare function simple_func(arg1 integer) real not null;

-- TEST: error declare proc conflicts with func
-- + Error % proc name conflicts with func name 'simple_func'
-- +1 Error
declare proc simple_func(arg1 integer not null);

-- TEST: error declare proc conflicts with func
-- + Error % proc name conflicts with func name 'simple_func'
-- +1 Error
create proc simple_func(arg1 integer not null)
begin
  select 1;
end;

-- TEST: error declare function that conflicts with a proc
-- + Error % func name conflicts with proc name 'proc1'
-- +1 Error
declare function proc1(i integer) integer;

-- TEST: try to declare a function inside a proc
-- + Error % declared functions must be top level 'foo'
-- +1 Error
create proc nested_func_wrapper()
begin
  declare function foo() integer;
end;

-- TEST: duplicate function formal
-- + Error % duplicate parameter name 'a'
-- +1 Error
declare function dup_formal(a integer, a integer) integer;

-- result for the next test
declare real_result real;

-- TEST: simple function call simple return
-- + {assign}: real_result: real variable
-- + {name real_result}: real_result: real variable
-- + {call}: real notnull
-- + {name simple_func}
-- + {arg_list}: ok
-- + {int 1}: integer notnull
-- - Error
set real_result := simple_func(1);

-- TEST: function call with bogus arg type
-- + {call}: err
-- + {assign}: err
-- + Error % incompatible types in expression 'arg1'
-- +1 Error
set real_result := simple_func('xx');

-- TEST: function call with invalid args
-- + {call}: err
-- + {assign}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
set real_result := simple_func(not 'xx');

-- TEST: try to use user func in a sql statement
-- + {select_stmt}: err
-- + {call}: err
-- + Error % User function may not appear in the context of a SQL statement 'simple_func'
-- +1 Error
select simple_func(1);

-- TEST: declare an object variable
-- + {name obj_var}: obj_var: object variable
-- - Error
declare obj_var object;

-- TEST: error on ordered comparisons (left)
-- + Error % left operand cannot be an object in '<'
-- +1 Error
set X := obj_var < 1;

-- TEST: error on ordered comparisons (right)
-- + Error % right operand cannot be an object in '<'
-- +1 Error
set X := 1 < obj_var;

-- TEST: ok to compare objects to each other with equality
-- + {eq}: bool
-- - Error
set X := obj_var == obj_var;

-- TEST: ok to compare objects to each other with inequality
-- + {ne}: bool
-- - Error
set X := obj_var <> obj_var;

-- TEST: error on math with object (left)
-- + Error % left operand cannot be an object in '+'
-- +1 Error
set X := obj_var + 1;

-- TEST: error on ordered comparisons (right)
-- + Error % right operand cannot be an object in '+'
-- +1 Error
set X := 1 + obj_var;

-- TEST: error on unary not
-- + Error % object operand not allowed in 'NOT'
-- + {not}: err
-- +1 Error
set X := not obj_var;

-- TEST: error on unary negation
-- + {uminus}: err
-- + Error % object operand not allowed in '-'
-- +1 Error
set X := - obj_var;

-- TEST: assign object to string
-- + {name a_string}: err
-- + Error % incompatible types in expression 'a_string'
-- +1 Error
set a_string := obj_var;

-- TEST: assign string to an object
-- + {name obj_var}: err
-- + Error % incompatible types in expression 'obj_var'
-- +1 Error
set obj_var := a_string;

-- TEST: create proc with object arg
-- + {param}: an_obj: object variable out
-- - Error
create proc obj_proc(out an_obj object)
begin
  set an_obj := null;
end;

-- TEST: try to create a table with an object column
-- + {col_def}: err
-- + Error % tables cannot have object columns 'obj'
-- +1 Error
create table object_table_test(
  obj object
);

-- TEST: try to use an object variable in a select statement
-- + {name obj_var}: err
-- + Error % object variables may not appear in the context of a SQL statement (except table-valued functions) 'obj_var'
-- +1 Error
select obj_var;

-- TEST: try to use an object variable in an IN statement, that's ok
-- + {in_pred}: bool
-- + {expr_list}: obj_var: object variable
-- - Error
set X := obj_var in (obj_var, null);

-- TEST: bogus in statement with object variable, combining with numeric
-- + {in_pred}: err
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := obj_var in (obj_var, 1);

-- TEST: bogus in statement with object variable, combining with text
-- + {in_pred}: err
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := obj_var in ('foo', obj_var);

-- TEST: bogus in statement with object variable, searching for text with object in list
-- + {in_pred}: err
-- + {expr_list}: text notnull
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := 'foo' in ('foo', obj_var);

-- TEST: case statement using objects as test condition
-- + {assign}: X: integer variable
-- + {case_expr}: integer notnull
-- + {name obj_var}: obj_var: object variable
-- - Error
set X := case obj_var when obj_var then 2 else 3 end;

-- TEST: case statement using objects as result
-- + {assign}: obj_var: object variable
-- + {name obj_var}: obj_var: object variable
-- + {case_expr}: object
-- + {case_list}: object variable
-- + {when}: obj_var: object variable
-- + {null}: null
-- - Error
set obj_var := case 1 when 1 then obj_var else null end;

-- TEST: between with objects is just not happening, first case
-- + Error % first operand cannot be an object in 'BETWEEN'
-- +1 Error
set X := obj_var between 1 and 3;

-- TEST: between with objects is just not happening, second case;
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
set X := 2 between obj_var and 3;

-- TEST: between with objects is just not happening, third case;
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
set X := 2 between 1 and obj_var;

-- TEST: not between with objects similarly not supported, first case
-- + Error % first operand cannot be an object in 'NOT BETWEEN'
-- +1 Error
set X := obj_var not between 1 and 3;

-- TEST: not between with objects similarly not supported, second case;
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
set X := 2 not between obj_var and 3;

-- TEST: not between with objects similarly not supported, third case;
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
set X := 2 not between 1 and obj_var;

-- TEST: printf cannot have any object arguments
-- + {call}: err
-- + {name printf}
-- + Error % no object/blob types are allowed in arguments for function 'printf'
-- +1 Error
set a_string := printf('Foo', obj_var);

-- TEST: make a function that creates an not null object
-- + {name creater_func}: object notnull create_func
-- - Error
declare function creater_func() create object not null;

-- TEST: make a function that creates an nullable
-- + declare_func_stmt}: object create_func
-- - Error
declare function maybe_create_func() create object;

-- Storage for these next few tests
-- - Error
declare not_null_object object not null;

-- TEST: convert object to not null
-- + {assign}: not_null_object: object notnull variable
-- + {name not_null_object}: not_null_object: object notnull variable
-- - Error
set not_null_object := attest_notnull(obj_var);

-- TEST: attest with matching kind, ok to go
-- + {assign}: price_d: real<dollars> variable
-- + {name price_d}: price_d: real<dollars> variable
-- + {call}: price_d: real<dollars> notnull variable
set price_d := attest_notnull(price_d);

-- TEST: attest should copy the semantic info including kind, hence can produce errors
-- + {assign}: err
-- + Error % expressions of different kinds can't be mixed: 'dollars' vs. 'euros'
-- +1 Error
set price_d := attest_notnull(price_e);

-- TEST: convert to not null -- fails already not null
-- + {call}: err
-- + Error % argument must be a nullable type (but not constant NULL) in 'attest_notnull'
-- +1 Error
set not_null_object := attest_notnull(not_null_object);

-- TEST: convert to not null -- fails can't do this to 'null'
-- + {call}: err
-- + Error % argument must be a nullable type (but not constant NULL) in 'attest_notnull'
-- +1 Error
set not_null_object := attest_notnull(null);

-- TEST: convert to not null -- fails wrong arg count
-- + {call}: err
-- + Error % function got incorrect number of arguments 'attest_notnull'
-- +1 Error
set not_null_object := attest_notnull(1, 7);

-- TEST: convert to not null -- fails in SQL context
-- + {call}: err
-- + Error % operator may only appear in the context of a SQL statement 'attest_notnull'
-- +1 Error
set not_null_object := (select attest_notnull(1));

-- TEST: echo statement is ok in any top level context
-- + {echo_stmt}: ok
-- + {name c}
-- + {strlit 'foo\n'}
-- - Error
@echo c, 'foo\n';

-- TEST: simple typed object declaration
-- + declare_vars_type}: object<Foo>
-- + {name_list}: foo_obj: object<Foo> variable
-- + {name foo_obj}: foo_obj: object<Foo> variable
-- + {type_object}: object<Foo>
-- + {name Foo}
-- - Error
declare foo_obj object<Foo>;

-- TEST: simple typed object assignment
-- + {assign}: foo_obj: object<Foo> variable
-- + {name foo_obj}: foo_obj: object<Foo> variable
-- + {name foo_obj}: foo_obj: object<Foo> variable
-- - Error
set foo_obj := foo_obj;

-- TEST: function with typed object return type
-- + {declare_func_stmt}: object<Foo>
-- - Error
declare function foo_func() object<Foo>;

-- TEST: function with typed object return type
-- + {assign}: foo_obj: object<Foo> variable
-- + {name foo_obj}: foo_obj: object<Foo> variable
-- + {call}: object<Foo>
-- - Error
set foo_obj := foo_func();

-- TEST: some different object type
-- + {declare_vars_type}: object<Bar>
-- + {name_list}: bar_obj: object<Bar> variable
-- - Error
declare bar_obj object<Bar>;

-- TEST: assign Bar to a Foo
-- + Error % expressions of different kinds can't be mixed: 'Bar' vs. 'Foo'
-- +1 Error
set bar_obj := foo_obj;

-- TEST: case statement must have uniform return type
-- + Error % expressions of different kinds can't be mixed: 'Bar' vs. 'Foo'
-- +1 Error
set bar_obj := case 1 when 1 then bar_obj when 2 then foo_obj end;

-- TEST: case statement errors in then expr
-- + Error % name not found 'bar_object'
-- +1 Error
set bar_obj := case 1 when 1 then bar_object when 2 then foo_obj end;

-- TEST: case statement must have uniform return type
-- + Error % expressions of different kinds can't be mixed: 'Bar' vs. 'Foo'
-- +1 Error
set bar_obj := case 1 when 1 then bar_obj else foo_obj end;

-- TEST: case statement errors in else
-- + Error % name not found 'foo_object'
-- +1 Error
set bar_obj := case 1 when 1 then bar_obj else foo_object end;

-- TEST: case statement typed object no errors
-- + {assign}: bar_obj: object<Bar> variable
-- + {name bar_obj}: bar_obj: object<Bar> variable
-- + {case_expr}: object<Bar>
-- - Error
set bar_obj := case 1 when 1 then bar_obj when 2 then bar_obj else bar_obj end;

-- TEST: non-user func with bogus arg
-- + {arg_list}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
call printf('%d', simple_func(not 'x'));

-- TEST: insert with column names, types match
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- - Error
insert into bar(id, name, rate) values (1, '2', 3);

-- TEST: insert with auto increment column null ok
-- + {insert_stmt}: ok
-- + {name_columns_values}
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- - Error
insert into foo(id) values(NULL);

-- TEST: insert missing column
-- + {insert_stmt}: err
-- + Error % required column missing in INSERT statement 'id'
-- +1 Error
insert into bar(name) values('x');

-- TEST: insert column name doesn't exist
-- + {insert_stmt}: err
-- + Error % name not found 'garbonzo'
-- +1 Error
insert into bar(garbonzo) values('x');

-- TEST: insert duplicate column name
-- + {insert_stmt}: err
-- + Error % name list has duplicate name 'id'
-- +1 Error
insert into bar(id, id) values('x');

-- TEST: insert column with default value
-- + {insert_stmt}: ok
-- + {name booly}: booly: { id: integer has_default, flag: bool }
-- - Error
insert into booly(id) values(1);

-- TEST: insert into a view (with columns)
-- + Error % cannot insert into a view 'MyView'
-- +1 Error
-- + {name MyView}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + {insert_stmt}: err
insert into MyView(id) values (1);

-- TEST: insert into non existent table
-- + {insert_stmt}: err
-- + Error % table in insert statement does not exist 'garbonzo'
-- +1 Error
insert into garbonzo(id) values('x');

-- TEST: declare a function with object arg type
-- + {param_detail}: goo: object<Goo> variable in
-- - Error
declare function goo_func(goo object<Goo>) text;

-- TEST: function with mismatched arg type
-- + {assign}: err
-- + Error % expressions of different kinds can't be mixed: 'Goo' vs. 'Bar'
-- +1 Error
set a_string := goo_func(bar_obj);

-- TEST: user function with bogus arg
-- + {assign}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
set a_string := goo_func(not 'x');

-- TEST: insert columns with mismatched count
-- + {insert_stmt}: err
-- + Error % count of columns differs from count of values
-- +1 Error
insert into foo(id) values(NULL, NULL);

-- TEST: insert columns with error in expression
-- + {insert_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
insert into foo(id) values(not 'x');

-- TEST: insert auto inc column with not null value
-- + {insert_stmt}: ok
-- + {name_columns_values}
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- - Error
insert into foo(id) values(1);

-- TEST: insert with not matching column types
-- + {insert_stmt}: err
-- + Error % incompatible types in expression 'id'
-- +1 Error
insert into bar(id) values('x');

-- TEST: create a temporary view
-- + {create_view_stmt}: temp_view: { A: integer notnull, B: integer notnull }
-- this is the temp flag
-- + {int 1}
-- + {name_and_select}
-- + {name temp_view}
-- + {select_stmt}: temp_view: { A: integer notnull, B: integer notnull }
-- - Error
create temp view temp_view as select 1 A, 2 B;

-- TEST: alter a table, adding a nullable column
-- {alter_table_add_column_stmt}: ok
-- {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- {col_def}: name: text
-- - Error
alter table bar add column name text;

-- TEST: alter a table, adding a nullable column
-- {alter_table_add_column_stmt}: err
-- + Error % adding a not nullable column with no default value is not allowed 'name'
alter table bar add column name text not null;

-- TEST: alter a table, adding a column whose declared type does not match
-- {alter_table_add_column_stmt}: err
-- + Error % added column must be an exact match for the column type declared in the table 'name'
alter table bar add column name integer;

-- TEST: alter a table, adding a column that was not declared
-- {alter_table_add_column_stmt}: err
-- + Error % added column must already be reflected in declared schema, with @create, exact name match required 'goo'
alter table bar add column goo integer;

-- TEST: alter a table, adding a column that was not declared
-- {alter_table_add_column_stmt}: err
-- + Error % added column must already be reflected in declared schema, with @create, exact name match required 'NAME'
alter table bar add column NAME text;

-- TEST: alter a table, adding a nullable column
-- {alter_table_add_column_stmt}: err
-- + Error % tables cannot have object columns 'foo'
alter table bar add column foo object;

-- TEST: alter a table, adding an autoinc column
-- {alter_table_add_column_stmt}: err
-- + Error % adding an auto increment column is not allowed 'id'
alter table bar add column id integer primary key autoincrement;

-- TEST: alter a table, table doesn't exist
-- {alter_table_add_column_stmt}: err
-- + Error % table in alter statement does not exist 'garbonzo'
alter table garbonzo add column id integer primary key autoincrement;

-- TEST: alter a table, table is a view
-- {alter_table_add_column_stmt}: err
-- + Error % cannot alter a view 'MyView'
alter table MyView add column id integer primary key autoincrement;

-- TEST: try to declare a schema version inside of a proc
-- + Error % schema upgrade version declaration must be outside of any proc
-- +1 Error
create proc bogus_version()
begin
  @schema_upgrade_version(11);
end;

-- TEST: try to declare a schema version after tables already defined
-- + Error % schema upgrade version declaration must come before any tables are declared
-- +1 Error
@schema_upgrade_version(11);

-- TEST: try to declare a bogus version number
-- + Error % schema upgrade version must be a positive integer
-- +1 Error
@schema_upgrade_version(0);

-- TEST: try to alter a column with create version specs
-- + Error % version annotations not valid in alter statement 'name'
-- +1 Error
alter table bar add column name text @create(1, foo);

-- TEST: try to alter a column with delete version specs
-- + Error % version annotations not valid in alter statement 'name'
-- +1 Error
alter table bar add column name text @delete(1);

-- TEST: try to alter a column with multiple version specs
-- + Error % duplicate version annotation
-- +1 Error
alter table bar add column name text @delete(1) @delete(1);

-- TEST: try to alter a column with multiple version specs
-- + Error % duplicate version annotation
-- +1 Error
alter table bar add column name text @create(1) @create(1);

-- TEST: try to alter a column with bogus version number
-- + Error % version number in annotation must be positive
-- +1 Error
alter table bar add column name text @create(0);

-- TEST: declare a table with a deleted column (should be hidden)
-- + {create_table_stmt}: hides_id_not_name: { name: text }
-- + {col_def}: id: integer hidden
-- + {col_def}: name: text
create table hides_id_not_name(
  id int @delete(2),
  name text @create(3)
);

-- TEST: try to use id from the above
-- + {name id}: err
-- + Error % name not found 'id'
-- +1 Error
select id from hides_id_not_name;

-- TEST: try to use name from the above
-- + {select_stmt}: select: { name: text }
-- - Error
select name from hides_id_not_name;

-- TEST: duplicate procedure annotation
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + {create_attr}: err
-- + Error % a procedure can appear in only one annotation 'creator'
-- +1 Error
create table migrate_test(
  id integer not null,
  id2 integer @create(4, creator),
  id3 integer @create(4, creator)
);

-- TEST: try to declare 'creator' in the wrong version (it should be in 4)
-- + {create_proc_stmt}: err
-- + Error % @schema_upgrade_version not declared or doesn't match upgrade version 4 for proc 'creator'
-- +1 Error
create proc creator()
begin
 select 1;
end;

-- TEST: create a table with @create annotations in a bogus order
-- + Error % created columns must be at the end and must be in version order 'col3'
-- +1 Error
create table migrate_annotions_broken(
  col1 integer,
  col2 integer @create(3),
  col3 integer
);

-- TEST: create a table with @create annotations on a not null column
-- + Error % create/delete version numbers can only be applied to columns that are nullable or have a default value 'col2'
-- +1 Error
create table migrate_annotions_broken_not_null_create(
  col1 integer,
  col2 integer not null @create(3)
);

-- TEST: create a table with @delete annotations on a not null column
-- + Error % create/delete version numbers can only be applied to columns that are nullable or have a default value 'col2'
-- +1 Error
create table migrate_annotions_broken_not_null_delete(
  col1 integer,
  col2 integer not null @delete(3)
);

-- TEST: create a table with @delete on earlier version than create
-- + Error % column delete version can't be <= column create version 'col2'
-- +1 Error
create table migrate_annotions_delete_out_of_order(
  col1 integer,
  col2 integer @delete(3) @create(4)
);

-- TEST: create a table with versioning
-- + {create_attr}
-- + {int 1}
-- + {name table_create_proc}
-- + {delete_attr}
-- + {int 2}
-- + {name table_delete_proc}
-- + hidden
-- - Error
create table versioned_table(
   id integer @create(2)
) @create(1, table_create_proc) @delete(3, table_delete_proc);

-- TEST: try to use a migration procedure name that ends in _crc
-- + Error % the name of a migration procedure may not end in '_crc' 'x_crc'
-- +1 Error
create table bogus_migration_proc(
   id integer
) @create(1, x_crc);

-- TEST: create a table with double creates
-- +1 Error % duplicate version annotation
-- +1 Error
create table versioned_table_double_create(
   id integer
) @create(1) @create(1);

-- TEST: create a table with double delete
-- +1 Error % duplicate version annotation
-- +1 Error
create table versioned_table_double_delete(
   id integer
) @delete(1) @delete(1);

-- TEST: try to create an index on deprecated table
-- + {create_index_stmt}: err
-- + Error % create index table name not found (hidden by @delete) 'versioned_table'
-- +1 Error
create index index_broken on versioned_table(id);

-- TEST: make an FK that refers to a versioned table
-- + Error % foreign key refers to non-existent table (hidden by @delete) 'versioned_table'
-- +1 Error
-- create_table_stmt}: err
create table baz (
  id integer,
  foreign key (id) references versioned_table(id)
);

-- TEST: try to select from a deprecated table
-- + Error % table/view not defined (hidden by @delete) 'versioned_table'
-- +1 Error
select * from versioned_table;

-- TEST: try to alter a deleted table -- DDL is exempt from the existence rules
-- - Error
alter table versioned_table add column id integer;

-- TEST: try to delete from a deprecated table
-- + Error % table in delete statement does not exist (hidden by @delete) 'versioned_table'
-- +1 Error
delete from versioned_table;

-- TEST: try to insert into a deprecated table
-- + Error % table in insert statement does not exist (hidden by @delete) 'versioned_table'
-- +1 Error
insert into versioned_table values(1);

-- TEST: try to insert into a deprecated table (column syntax)
-- + Error % table in insert statement does not exist (hidden by @delete) 'versioned_table'
-- +1 Error
insert into versioned_table(id) values(1);

-- TEST: try to create a view with the same name as the versioned table
-- note: the name is found even though the table is hidden
-- + {create_view_stmt}: err
-- + Error % duplicate table/view name 'versioned_table'
create view versioned_table as select 1 x;

-- TEST: try to create a global variable with the same name as the versioned table
-- note: the name is found even though the table is hidden
-- + {declare_vars_type}: err
-- + Error % global variable hides table/view name 'versioned_table'
-- +1 Error
declare versioned_table integer;

-- TEST: try to create a table with the same name as the versioned table
-- note: the name is found even though the table is hidden
-- + {create_table_stmt}: err
-- + Error % duplicate table/view name 'versioned_table'
create table versioned_table(id2 integer);

-- TEST: drop the table (note that DDL works on any version)
-- + {drop_table_stmt}: ok
-- + {name versioned_table}: versioned_table: { id: integer } hidden
-- - Error
drop table if exists versioned_table;

-- TEST: drop table that doesn't exist
-- + Error % table in drop statement does not exist 'garbonzo'
-- +1 Error
drop table garbonzo;

-- TEST: try to drop table on a view
-- + Error % cannot drop a view with drop table 'MyView'
-- +1 Error
drop table MyView;

-- TEST: use a proc to get the result set
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- + {call_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- - Error
create procedure uses_proc_for_result()
begin
  call with_result_set();
end;

-- TEST: table with a column deleted too soon
-- + Error % column delete version can't be <= the table create version 'id'
-- +1 Error
create table t_col_early_delete (
  id integer @delete(2)
) @create(3);

-- TEST: table with a column created too soon
-- + Error % column create version can't be <= the table create version 'id'
-- +1 Error
create table t_col_early_delete (
  id integer @create(2)
) @create(3);

-- TEST: table with a column deleted too late
-- + Error % column delete version can't be >= the table delete version 'id'
-- +1 Error
create table t_col_early_delete (
  id integer @delete(2)
) @delete(1);

-- TEST: table with a column created too late
-- + Error % column create version can't be >= the table delete version 'id'
-- +1 Error
create table t_col_early_delete (
  id integer @create(2)
) @delete(1);

-- TEST: table deleted not null column with default
-- + {col_def}: id: integer notnull has_default hidden @delete(2)
-- - Error
create table t_col_delete_notnull (
  id integer not null DEFAULT 8675309 @delete(2)
);

-- TEST: negative default value
-- + {create_table_stmt}: neg_default: { id: integer notnull has_default }
-- + {col_def}: id: integer notnull has_default
-- + {col_attrs_default}
-- + {uminus}
-- + {int 1}
-- - Error
create table neg_default (
  id integer not null default -1 @create(2)
);

-- TEST: alter a table, adding a nullable column
-- {alter_table_add_column_stmt}: ok
-- {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- {col_def}: name: text
-- - Error
alter table neg_default add column id integer not null default -1;

-- TEST: try to validate previous schema in a proc
-- + {create_proc_stmt}: err
-- + {previous_schema_stmt}: err
-- + Error % switching to previous schema validation mode must be outside of any proc
-- +1 Error
create proc bogus_validate()
begin
  @previous_schema;
end;

-- TEST: make a select * with a duplicate result column name and try to fetch the fields
-- + {fetch_stmt}: err
-- + {name C}: err
-- + Error % duplicate column name in result not allowed 'id'
-- +1 Error
create proc bogus_fetch()
begin
  declare C cursor for select * from foo T1 join foo T2 on T1.id = T2.id;
  fetch C;
end;

-- TEST: make a select * with a duplicate result column name and use that as a proc result set
-- + {create_proc_stmt}: err
-- + Error % duplicate column name in result not allowed 'id'
-- +1 Error
create proc bogus_result_duplicate_names()
begin
  select * from foo T1 join foo T2 on T1.id = T2.id;
end;

-- TEST: make table with text as a column name
-- + {create_table_stmt}: table_with_text_as_name: { text: text, text2: text }
-- - Error
create table table_with_text_as_name(
  text text,
  text2 text
);

-- TEST: use text as a column
-- + {select_stmt}: select: { text: text, text2: text }
-- - Error
select text, text2 from table_with_text_as_name;

-- TEST: extract a column named text -- brutal renames
-- + {select_stmt}: select: { text: text, other_text: text }
-- + {select_from_etc}: TABLE { table_with_text_as_name: table_with_text_as_name }
-- + {name text2}: text2: text
-- + {name text}: text: text
-- - Error
select text2 as text, text as other_text from table_with_text_as_name;

-- TEST: try to start a schema upgrade after there are tables
-- {schema_upgrade_script_stmt}: err
-- + Error % schema upgrade declaration must come before any tables are declared
-- +1 Error
@schema_upgrade_script;

-- TEST: try to start a schema upgrade inside a proc
-- {schema_upgrade_script_stmt}: err
-- + Error % schema upgrade declaration must be outside of any proc
-- +1 Error
create proc schema_upgrade_you_wish()
begin
  @schema_upgrade_script;
end;

-- TEST: try to use the non-column insert syntax on a table with hidden columns
-- we should get a fully formed insert on the non hidden column
-- + INSERT INTO hides_id_not_name(name) VALUES('x');
-- + {name hides_id_not_name}: hides_id_not_name: { name: text }
insert into hides_id_not_name values('x');

-- TEST: create a table with more mixed column stuff for use testing alter statements later
-- + {create_table_stmt}: trickier_alter_target: { id: integer notnull, added: text }
-- - Error
create table trickier_alter_target(
  id integer,
  something_deleted text @create(1) @delete(2),
  added text @create(2),
  primary key(id)
);

-- TEST: try to add id --> doesn't work
-- + {alter_table_add_column_stmt}: err
-- + {name trickier_alter_target}: trickier_alter_target: { id: integer notnull, added: text }
-- + Error % added column must already be reflected in declared schema, with @create, exact name match required 'id'
-- +1 Error
alter table trickier_alter_target add column id integer;

-- TEST: try to add something_deleted --> doesn't work
-- + {alter_table_add_column_stmt}: err
-- + {name trickier_alter_target}: trickier_alter_target: { id: integer notnull, added: text }
-- + Error % added column must already be reflected in declared schema, with @create, exact name match required 'something_deleted'
-- +1 Error
alter table trickier_alter_target add column something_deleted text;

-- TEST: try to add 'added' -> works!
-- + {name trickier_alter_target}: trickier_alter_target: { id: integer notnull, added: text }
-- + alter_table_add_column_stmt}: ok
-- + {col_def}: added: text
-- - Error
alter table trickier_alter_target add column added text;

-- TEST: select as table with error
-- + {select_stmt}: err
-- + {table_or_subquery}: err
-- + string operand not allowed in 'NOT'
-- +1 Error
select * from (select not 'x' X);

-- TEST: create a view with versions
-- + FROM bar @DELETE(2);
-- + {delete_attr}
-- - Error
create view view_with_version as select * from bar @delete(2);

-- long int variable
declare ll long integer not null;

-- TEST: use a long literal
-- + {longint 3147483647}: longint notnull
-- - Error
set ll := 3147483647L;

-- TEST: try to drop a view that doesn't exist
-- + {drop_view_stmt}: err
-- + Error % view in drop statement does not exist 'view_not_present'
-- +1 Error
drop view view_not_present;

-- TEST: try to drop a view that is a table
-- + {drop_view_stmt}: err
-- + Error % cannot drop a table with drop view 'foo'
-- +1 Error
drop view foo;

-- TEST: drop a view that is really a view
-- + {drop_view_stmt}: ok
-- + {name MyView}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- - Error
drop view if exists MyView;

-- TEST: drop an index that exists
-- + DROP INDEX index_1;
-- {drop_index_stmt}: ok
-- - Error
drop index index_1;

-- TEST: drop an index that exists
-- {drop_index_stmt}: err
-- + Error % index in drop statement was not declared 'I_dont_see_no_steekin_index'
-- +1 Error
drop index if exists I_dont_see_no_steekin_index;

-- TEST: specify a column attribute twice (put something in between)
-- + {create_table_stmt}: err
-- + Error % a column attribute was specified twice on the same column
-- +1 Error
create table two_not_null(
  id integer not null unique not null
);

-- TEST: specify incompatible constraints
-- + {create_table_stmt}: err
-- + Error % column can't be primary key and also unique key 'id'
-- +1 Error
create table mixed_pk_uk(
  id integer primary key unique
);

-- TEST: verify unique column flag recorded
-- + {create_table_stmt}: table_with_uk: { id: integer unique_key }
-- - Error
create table table_with_uk(
  id integer unique
);

-- TEST: validate PK not duplicated (mixed metho)
-- + {create_table_stmt}: err
-- + Error % more than one primary key in table 'baz'
-- +1 Error
create table baz(
  id integer primary key AUTOINCREMENT not null,
  PRIMARY KEY (id)
);

-- TEST: seed value is a string -- error
-- + {insert_stmt}: err
-- + Error % seed expression must be a non-nullable integer
-- +1 Error
insert into bar (id, name, rate) values (1, 'bazzle', 3) @dummy_seed('x');

-- TEST: seed value is a string -- expression error
-- + {insert_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
insert into bar (id, name, rate) values (1, 'bazzle', 3) @dummy_seed(not 'x');

-- TEST: ok to go insert with dummy values
-- note that the insert statement has been mutated!!
-- + INSERT INTO bar(id, name, rate) VALUES(_seed_, printf('name_%d', _seed_), _seed_) @DUMMY_SEED(1 + 2) @DUMMY_DEFAULTS @DUMMY_NULLABLES;
-- + {insert_dummy_spec}: integer notnull
-- + {call}: text notnull
-- + {name printf}: text notnull
-- + {strlit 'name_%d'}: text notnull
-- - Error
insert into bar () values () @dummy_seed(1+2) @dummy_nullables @dummy_defaults;

-- TEST: use default value of a table
-- + {name booly}: booly: { id: integer has_default, flag: bool }
-- - Error
insert into booly(flag) values(1);

-- TEST: try to declare a blob variable
-- + {declare_vars_type}: blob
-- + {name_list}: blob_var: blob variable
-- - Error
declare blob_var blob;

-- TEST: error on ordered comparisons (left)
-- + Error % left operand cannot be a blob in '<'
-- +1 Error
set X := blob_var < 1;

-- TEST: error on ordered comparisons (right)
-- + Error % right operand cannot be a blob in '<'
-- +1 Error
set X := 1 < blob_var;

-- TEST: ok to compare blobs to each other with equality
-- + {eq}: bool
-- - Error
set X := blob_var == blob_var;

-- TEST: ok to compare blobs to each other with inequality
-- + {ne}: bool
-- - Error
set X := blob_var <> blob_var;

-- TEST: error on math with blob (left)
-- + Error % left operand cannot be a blob in '+'
-- +1 Error
set X := blob_var + 1;

-- TEST: error on ordered comparisons (right)
-- + Error % right operand cannot be a blob in '+'
-- +1 Error
set X := 1 + blob_var;

-- TEST: error on unary not
-- + Error % blob operand not allowed in 'NOT'
-- + {not}: err
-- +1 Error
set X := not blob_var;

-- TEST: error on unary negation
-- + {uminus}: err
-- + Error % blob operand not allowed in '-'
-- +1 Error
set X := - blob_var;

-- TEST: assign blob to string
-- + {name a_string}: err
-- + Error % incompatible types in expression 'a_string'
-- +1 Error
set a_string := blob_var;

-- TEST: assign string to a blob
-- + {name blob_var}: err
-- + Error % incompatible types in expression 'blob_var'
-- +1 Error
set blob_var := a_string;

-- TEST: report error to use concat outside SQL statement
-- + Error % CONCAT may only appear in the context of a SQL statement
-- +1 Error
set a_string := blob_var || 2.0;

-- TEST: report error to concat blob and number
-- + Error % blob operand must be converted to string first in '||'
-- +1 Error
select blob_var || 2.0;

-- TEST: report error to concat number and blob
-- + Error % blob operand must be converted to string first in '||'
-- +1 Error
select 1 || blob_var;

-- TEST: create proc with blob arg
-- + CREATE PROC blob_proc (OUT a_blob BLOB)
-- + {create_proc_stmt}: ok
-- + {param}: a_blob: blob variable out
-- - Error
create proc blob_proc(out a_blob blob)
begin
  set a_blob := null;
end;

-- TEST: try to create a table with a blob column
-- + {create_table_stmt}: blob_table_test: { b: blob }
-- - Error
create table blob_table_test(
  b blob
);

-- TEST: try to use a blob variable in a select statement
-- + {select_stmt}: select: { blob_var: blob variable }
-- - Error
select blob_var;

-- TEST: try to use a blob variable in an IN statement, that's ok
-- + {in_pred}: bool
-- + {expr_list}: blob_var: blob variable
-- - Error
set X := blob_var in (blob_var, null);

-- TEST: bogus in statement with blob variable, combining with numeric
-- + {in_pred}: err
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := blob_var in (blob_var, 1);

-- TEST: bogus in statement with blob variable, combining with text
-- + {in_pred}: err
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := blob_var in ('foo', blob_var);

-- TEST: bogus in statement with blob variable, searching for text with blob in list
-- + {in_pred}: err
-- + {expr_list}: text notnull
-- + Error % incompatible types in expression 'IN'
-- +1 Error
set X := 'foo' in ('foo', blob_var);

-- TEST: case statement using blobs as test condition
-- + {assign}: X: integer variable
-- + {case_expr}: integer notnull
-- + {name blob_var}: blob_var: blob variable
-- - Error
set X := case blob_var when blob_var then 2 else 3 end;

-- TEST: case statement using blobs as result
-- + {assign}: blob_var: blob variable
-- + {name blob_var}: blob_var: blob variable
-- + {case_expr}: blob
-- + {case_list}: blob variable
-- + {when}: blob_var: blob variable
-- + {null}: null
-- - Error
set blob_var := case 1 when 1 then blob_var else null end;

-- TEST: between with blobs is just not happening, first case
-- + Error % first operand cannot be a blob in 'BETWEEN'
-- +1 Error
set X := blob_var between 1 and 3;

-- TEST: between with blobs is just not happening, second case;
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
set X := 2 between blob_var and 3;

-- TEST: between with blobs is just not happening, third case;
-- + Error % incompatible types in expression 'BETWEEN'
-- +1 Error
set X := 2 between 1 and blob_var;

-- TEST: not between with blobs similarly not supported, first case
-- + Error % first operand cannot be a blob in 'NOT BETWEEN'
-- +1 Error
set X := blob_var not between 1 and 3;

-- TEST: not between with blobs similarly not supported, second case;
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
set X := 2 not between blob_var and 3;

-- TEST: not between with blobs similarly not supported, third case;
-- + Error % incompatible types in expression 'NOT BETWEEN'
-- +1 Error
set X := 2 not between 1 and blob_var;

-- TEST: printf cannot have any blob arguments
-- + {call}: err
-- + {name printf}
-- + Error % no object/blob types are allowed in arguments for function 'printf'
-- +1 Error
set a_string := printf('Foo', blob_var);


-- TEST: try to fetch into object variables
-- + {fetch_stmt}: err
-- + Error % incompatible types in expression 'o1'
-- +1 Error
create proc bogus_object_read()
begin
  declare o1, o2, o3 object;
  declare C cursor for select * from bar;
  fetch C into o1, o2, o3;
end;

-- TEST: try to use in (select...) in a bogus context
-- + Error % [not] in (select ...) is only allowed inside of select lists, where, on, and having clauses
-- +1 Error
create proc fool(x integer)
begin
  set x := x in (select 1);
end;

-- TEST: try to use not in (select...) in a bogus context
-- + Error % [not] in (select ...) is only allowed inside of select lists, where, on, and having clauses
-- +1 Error
create proc notfool(x integer)
begin
  set x := x not in (select 1);
end;

-- TEST: try to make a dummy blob -- not supported
-- + {insert_stmt}: ok
-- + {cast_expr}: blob notnull
-- + {call}: text notnull
-- + {name printf}: text notnull
-- + {strlit 'b_%d'}: text notnull
-- + {name _seed_}: _seed_: integer notnull variable
-- + INSERT INTO blob_table_test(b) VALUES(CAST(printf('b_%d', _seed_) AS BLOB)) @DUMMY_SEED(1) @DUMMY_NULLABLES;
-- - Error
insert into blob_table_test() values() @dummy_seed(1) @dummy_nullables;

-- TEST: simple out statement case
create proc out_cursor_proc()
begin
  declare C cursor for select 1 A, 2 B;
  fetch C;
  out C;
end;

-- TEST: use non-fetched cursor for out statement
-- + {create_proc_stmt}: err
-- + {out_stmt}: err
-- + Error % the cursor was not fetched with the auto-fetch syntax 'fetch [cursor]' 'C'
-- +1 Error
create proc out_cursor_proc_not_shape_storage()
begin
  declare a, b integer not null;
  declare C cursor for select 1 A, 2 B;
  fetch C into a, b;
  out C;
end;

-- TEST: use non-fetched cursor for out statement
-- + {create_proc_stmt}: err
-- + {out_stmt}: err
-- + Error % in multiple select statements, all column names must be identical so they have unambiguous names 'C'
-- +1 Error
create proc out_cursor_proc_incompat_results()
begin
  declare a, b integer not null;
  declare C cursor for select 1 A, 2 B;
  declare D cursor for select 1 A, 2 C;
  fetch C;
  fetch D;
  out C;
  out D;
end;

-- TEST: use mixed select and out
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + Error % can't mix and match out, out union, or select/call for return values 'out_cursor_proc_mixed_cursor_select'
-- +1 Error
create proc out_cursor_proc_mixed_cursor_select()
begin
  declare a, b integer not null;
  declare C cursor for select 1 A, 2 B;
  fetch C;
  out C;
  select 1 A, 2 B;
end;

-- TEST: use mixed select and out (other order)
-- + {create_proc_stmt}: err
-- + {out_stmt}: err
-- + Error % can't mix and match out, out union, or select/call for return values 'out_cursor_proc_mixed_cursor_select_select_first'
-- +1 Error
create proc out_cursor_proc_mixed_cursor_select_select_first()
begin
  declare a, b integer not null;
  declare C cursor for select 1 A, 2 B;
  fetch C;
  select 1 A, 2 B;
  out C;
end;

-- TEST: use mixed select and out union
-- + {create_proc_stmt}: err
-- + {out_union_stmt}: err
-- + Error % can't mix and match out, out union, or select/call for return values 'out_cursor_proc_mixed_cursor_select_then_union'
-- +1 Error
create proc out_cursor_proc_mixed_cursor_select_then_union()
begin
  declare a, b integer not null;
  declare C cursor for select 1 A, 2 B;
  fetch C;
  select 1 A, 2 B;
  out union C;
end;

-- TEST: use out statement with non cursor
-- + {create_proc_stmt}: err
-- + {out_stmt}: err
-- + Error % variable is not a cursor 'C'
-- +1 Error
create proc out_not_cursor()
begin
  declare C integer;
  out C;
end;

-- TEST: out cursor outside of a proc
-- + {out_stmt}: err
-- + Error % the out cursor statement only makes sense inside of a procedure
-- +1 Error
out curs;

-- TEST: read the result of a proc with an out cursor
-- + {create_proc_stmt}: ok dml_proc
-- + {declare_value_cursor}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {call_stmt}: C: select: { A: integer notnull, B: integer notnull } variable dml_proc shape_storage uses_out
-- - Error
create proc result_reader()
begin
  declare C cursor fetch from call out_cursor_proc();
end;

-- TEST: read the result of a proc with an out cursor
-- + {fetch_stmt}: err
-- + Error % value cursors are not used with FETCH C, or FETCH C INTO 'C'
-- +1 Error
create proc fails_result_reader()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C;
end;

-- TEST: declare a fetch proc with a result set
-- + {declare_proc_stmt}: declared_proc: { t: text } uses_out
-- - Error
declare proc declared_proc(id integer) out (t text);

-- TEST: fetch call a procedure with bogus args
-- + {create_proc_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc invalid_proc_fetch_bogus_call()
begin
  declare C cursor fetch from call out_cursor_proc(not 'x');
end;

-- TEST: call a procedure that is just all wrong
-- + {create_proc_stmt}: err
-- + Error % cursor requires a procedure that returns a cursor with OUT 'C'
-- +1 Error
create proc invalid_proc_fetch()
begin
  declare C cursor fetch from call xyzzy();
end;

-- TEST: read the result of a proc with an out cursor, use same var twice
-- +1 {declare_value_cursor}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- +1 {declare_value_cursor}: err
-- + Error % duplicate variable name in the same scope 'C'
-- +1 Error
create proc fails_result_reader_double_decl()
begin
  declare C cursor fetch from call out_cursor_proc();
  declare C cursor fetch from call out_cursor_proc();
end;

-- TEST: use proc_with_output like it was a function
-- + SET an_int := proc_with_output(1, an_int);
-- + {assign}: an_int: integer variable
-- + {call}: integer
-- + {name proc_with_output}
-- + {arg_list}: ok
-- - Error
set an_int := proc_with_output(1, an_int);

-- TEST: helper proc to test distinc in proc used as a function
create procedure proc_func(in arg1 integer, out arg2 integer)
begin
  drop table foo;
end;

-- TEST: Use distinct in a procedure used as a function
-- + {assign}: err
-- + {call}: err
-- + {distinct}
-- + {arg_list}: ok
-- + Error % DISTINCT may only be used in function that are aggregated or user defined 'proc_func'
-- +1 Error
SET an_int := proc_func(distinct 1);

-- TEST: use proc_with_output like it was a function, too many args
-- + {call}: err
-- + Error % too many arguments provided to procedure 'proc_with_output'
-- +1 Error
set an_int := proc_with_output(1, an_int, an_int);

-- TEST: try to use a proc that deals with struct results
-- + {call}: err
-- + Error % Stored procs that deal with result sets or cursors cannot be invoked as functions 'out_cursor_proc'
-- +1 Error
set an_int := out_cursor_proc();

-- TEST: this proc has no out arg that can be used as a result
-- + {call}: err
-- + Error % last formal arg of procedure is not an out arg, cannot use proc as a function 'proc2'
-- +1 Error
set an_int := proc2(1);

-- TEST: user proc calls can't happen inside of SQL
-- + {call}: err
-- + Error % Stored proc calls may not appear in the context of a SQL statement 'proc_with_output'
-- +1 Error
set an_int := (select proc_with_output(1, an_int, an_int));

-- a helper proc that is for sure using dml
create proc dml_func(out a integer not null)
begin
 set a := (select 1);
end;

-- TEST: create a proc that calls a dml proc as a function, must become a dml proc itself
-- - Error
-- + {assign}: a: integer notnull variable out
-- + {create_proc_stmt}: ok dml_proc
create proc should_be_dml(out a integer not null)
begin
  set a := dml_func();
end;

-- TEST: fetch cursor from values
-- + {name C}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {fetch_values_stmt}: ok
create proc fetch_values()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C from values(1,2);
end;

-- TEST: fetch cursor from values with dummy values
-- + FETCH C(A, B) FROM VALUES(_seed_, _seed_) @DUMMY_SEED(123) @DUMMY_NULLABLES;
-- + {name C}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {fetch_values_stmt}: ok
-- +2 {name _seed_}: _seed_: integer notnull variable
create proc fetch_values_dummy()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C() from values() @dummy_seed(123) @dummy_nullables;
end;

-- TEST: fetch cursor from call
-- + FETCH C FROM CALL out_cursor_proc();
-- + {fetch_call_stmt}: ok
-- + {name C}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {call_stmt}: C: select: { A: integer notnull, B: integer notnull } variable dml_proc shape_storage uses_out
-- + {name out_cursor_proc}: C: select: { A: integer notnull, B: integer notnull } variable dml_proc shape_storage uses_out
-- - Error
create proc fetch_from_call()
begin
  declare C cursor like out_cursor_proc;
  fetch C from call out_cursor_proc();
  out C;
end;

-- TEST: fetch cursor from call to proc that doesn't exist
-- + {create_proc_stmt}: err
-- + {stmt_list}: err
-- + {fetch_call_stmt}: err
-- + {call_stmt}: err
-- + Error % cursor requires a procedure that returns a cursor with OUT 'C'
-- +1 Error
create proc fetch_from_call_to_proc_that_does_not_exist()
begin
  declare C cursor like out_cursor_proc;
  fetch C from call does_not_exist();
  out C;
end;

-- TEST: fetch cursor from call to proc with invalid arguments
-- + {create_proc_stmt}: err
-- + {name fetch_from_call_to_proc_with_invalid_arguments}: err
-- + {stmt_list}: err
-- + {fetch_call_stmt}: err
-- + {call_stmt}: err
-- + Error % too many arguments provided to procedure 'out_cursor_proc'
-- +1 Error
create proc fetch_from_call_to_proc_with_invalid_arguments()
begin
  declare C cursor like out_cursor_proc;
  fetch C from call out_cursor_proc(42);
  out C;
end;

-- TEST: fetch cursor from call with invalid cursor
-- + {create_proc_stmt}: err
-- + {stmt_list}: err
-- + {fetch_call_stmt}: err
-- +2 {name C}: err
-- +2 Error % variable is not a cursor 'C'
create proc fetch_from_call_to_proc_with_invalid_cursor()
begin
  declare C text;
  fetch C from call out_cursor_proc();
  out C;
end;

-- TEST: fetch cursor from call to proc with different column names
-- + {create_proc_stmt}: err
-- + {name fetch_from_call_to_proc_with_different_column_names}: err
-- + {stmt_list}: err
-- + {fetch_call_stmt}: err
-- + {name C}: err
-- + {call_stmt}: err
-- + Error % in multiple select statements, all column names must be identical so they have unambiguous names 'B'
-- +1 Error
create proc fetch_from_call_to_proc_with_different_column_names()
begin
  declare C cursor like select 1 A, 2 C;
  fetch C from call out_cursor_proc();
  out C;
end;

-- TEST: fetch non cursor
-- + {fetch_values_stmt}: err
-- + Error % cursor not found 'not_a_cursor'
-- +1 Error
fetch not_a_cursor from values(1,2,3);

-- TEST: try to use fetch values on a statement cursor
-- + {fetch_values_stmt}: err
-- + Error % fetch values is only for value cursors, not for sqlite cursors 'my_cursor'
-- +1 Error
fetch my_cursor from values(1,2,3);

-- TEST: attempt bogus seed
-- + {fetch_values_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc fetch_values_bogus_seed_value()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C() from values() @dummy_seed(not 'x');
end;

-- TEST: missing columns in fetch values
-- + {fetch_values_stmt}: err
-- + Error % count of columns differs from count of values
-- +1 Error
create proc fetch_values_missing_value()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C from values();
end;


-- TEST: helper proc that returns a blob
-- + {create_proc_stmt}: C: select: { B: blob } variable dml_proc shape_storage uses_out
create proc blob_out()
begin
  -- cheesy nullable blob
  declare C cursor for select case when 1 then cast('x' as blob) else null end B;
  fetch C;
  out C;
end;

-- TEST: fetch cursor from values with dummy values but one is a blob
-- + {fetch_values_stmt}: err
-- + Error % there's no good way to generate dummy blobs; not supported for now
-- +1 Error
create proc fetch_values_blob_dummy()
begin
  declare C cursor fetch from call blob_out();
  fetch C() from values() @dummy_seed(123) @dummy_nullables;
end;

-- TEST: fetch cursor from values but not all columns mentioned
-- + {fetch_values_stmt}: err
-- + Error % required column missing in FETCH statement 'B'
-- +1 Error
create proc fetch_values_missing_columns()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C(A) from values(1);
end;

-- TEST: fetch cursor from values bogus value expression
-- + {fetch_values_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc fetch_values_bogus_value()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C(A,B) from values(1, not 'x');
end;

-- TEST: fetch cursor from values bogus value type
-- + {fetch_values_stmt}: err
-- + Error % incompatible types in expression 'B'
-- +1 Error
create proc fetch_values_bogus_type()
begin
  declare C cursor fetch from call out_cursor_proc();
  fetch C(A,B) from values(1, 'x');
end;

-- TEST: fetch cursor from values provide null for blob (works)
-- + fetch_values_stmt}: ok
-- + FETCH C(B) FROM VALUES(NULL) @DUMMY_SEED(123);
-- - Error
create proc fetch_values_blob_dummy_with_null()
begin
  declare C cursor fetch from call blob_out();
  fetch C() from values() @dummy_seed(123);
end;

-- TEST: fetch to a cursor from another cursor
-- + FETCH C0(A, B) FROM VALUES(1, 2);
-- + FETCH C1(A, B) FROM VALUES(C0.A, C0.B);
-- + {create_proc_stmt}: C1: select: { A: integer notnull, B: integer notnull } variable shape_storage uses_out
-- + {fetch_values_stmt}: ok
-- - Error
create proc fetch_to_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, 2 B;
  declare C1 cursor like C0;
  fetch C0 from values(1, 2);
  fetch C1 from C0;
  out C1;
end;

-- TEST: fetch to a cursor from an invalid cursor
-- + {create_proc_stmt}: err
-- + {name fetch_to_cursor_from_invalid_cursor}: err
-- + {stmt_list}: err
-- + {fetch_values_stmt}: err
-- + {name C0}: err
-- + Error % variable is not a cursor 'C0'
-- +1 Error
create proc fetch_to_cursor_from_invalid_cursor()
begin
  declare C0 int;
  declare C1 cursor like select 1 A, 2 B;
  fetch C1 from C0;
  out C1;
end;

-- TEST: fetch to an invalid cursor from a cursor
-- + {create_proc_stmt}: err
-- + {name fetch_to_invalid_cursor_from_cursor}: err
-- + {stmt_list}: err
-- + {fetch_values_stmt}: err
-- + {name C1}: err
-- + Error % variable is not a cursor 'C1'
-- +1 Error
create proc fetch_to_invalid_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, 2 B;
  declare C1 int;
  fetch C0 from values(1, 2);
  fetch C1 from C0;
end;

-- TEST: fetch to a statement cursor from another cursor
-- + {create_proc_stmt}: err
-- + {name fetch_to_statement_cursor_from_cursor}: err
-- + {stmt_list}: err
-- + {fetch_values_stmt}: err
-- + Error % fetch values is only for value cursors, not for sqlite cursors 'C1'
-- +1 Error
create proc fetch_to_statement_cursor_from_cursor()
begin
  declare C0 cursor like select 1 A, 2 B;
  declare C1 cursor for select 1 A, 2 B;
  fetch C0 from values(1, 2);
  fetch C1 from C0;
end;

-- TEST: fetch to a cursor from a cursor with different columns
-- + {create_proc_stmt}: err
-- + {name fetch_to_cursor_from_cursor_with_different_columns}: err
-- + {stmt_list}: err
-- + {fetch_values_stmt}: err
-- + Error % [shape] has too few fields 'C0'
-- +1 Error
create proc fetch_to_cursor_from_cursor_with_different_columns()
begin
  declare C0 cursor like select 1 A, 2 B;
  declare C1 cursor like select 1 A, 2 B, 3 C;
  fetch C0 from values(1, 2);
  fetch C1 from C0;
end;

-- TEST: fetch to a cursor from a cursor without fields
-- + {create_proc_stmt}: err
-- + {name fetch_to_cursor_from_cursor_without_fields}: err
-- + {stmt_list}: err
-- + {fetch_values_stmt}: err
-- + {name C0}: err
-- + Error % cannot read from a cursor without fields 'C0'
-- +1 Error
create proc fetch_to_cursor_from_cursor_without_fields()
begin
  declare X int;
  declare Y real;
  declare C0 cursor for select 1 A, 2.5;
  declare C1 cursor like C0;
  fetch C0 into X, Y;
  fetch C1 from C0;
end;

-- TEST: declare a cursor like an existing cursor
-- + {create_proc_stmt}: ok dml_proc
-- + {name declare_cursor_like_cursor}: ok dml_proc
-- + {declare_cursor_like_name}: C1: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {name C1}: C1: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- - Error
create proc declare_cursor_like_cursor()
begin
  declare C0 cursor fetch from call out_cursor_proc();
  declare C1 cursor like C0;
end;

-- TEST: declare a cursor like a variable that's not a cursor
-- + {create_proc_stmt}: err
-- + {name declare_cursor_like_non_cursor_variable}: err
-- + {stmt_list}: err
-- + {declare_cursor_like_name}: err
-- + {name C0}: err
-- + Error % variable is not a cursor 'C0'
-- +1 Error
create proc declare_cursor_like_non_cursor_variable()
begin
    declare C0 integer;
    declare C1 cursor like C0;
end;

-- TEST: declare a cursor with the same name as an existing variable
-- + {create_proc_stmt}: err
-- + {name declare_cursor_like_cursor_with_same_name}: err
-- + {stmt_list}: err
-- + {declare_cursor_like_name}: err
-- + {name C0}: err
-- + Error % duplicate variable name in the same scope 'C0'
-- +1 Error
create proc declare_cursor_like_cursor_with_same_name()
begin
  declare C0 cursor fetch from call out_cursor_proc();
  declare C0 cursor like C0;
end;

-- TEST: declare a cursor like something that's not defined
-- + {create_proc_stmt}: err
-- + {name declare_cursor_like_undefined_variable}: err
-- + {stmt_list}: err
-- + {declare_cursor_like_name}: err
-- + {name C0}: err
-- + Error % must be a cursor, proc, table, or view 'C0'
-- +1 Error
create proc declare_cursor_like_undefined_variable()
begin
    declare C1 cursor like C0;
end;

-- TEST: declare a cursor like a proc
-- + {create_proc_stmt}: ok
-- + {name declare_cursor_like_proc}: ok
-- + {declare_cursor_like_name}: C: decl3: { A: integer notnull, B: bool } variable shape_storage value_cursor
-- + {name C}: C: decl3: { A: integer notnull, B: bool } variable shape_storage value_cursor
-- - ok dml_proc
-- - Error
create proc declare_cursor_like_proc()
begin
  declare C cursor like decl3;
end;

-- TEST: declare a cursor like a proc with no result
-- + {create_proc_stmt}: err
-- + {name declare_cursor_like_proc_with_no_result}: err
-- + {stmt_list}: err
-- + {declare_cursor_like_name}: err
-- + {name decl1}: err
-- + Error % proc has no result 'decl1'
-- +1 Error
create proc declare_cursor_like_proc_with_no_result()
begin
  declare C cursor like decl1;
end;

-- TEST: declare a cursor like a table
-- + {create_proc_stmt}: ok
-- + {name declare_cursor_like_table}: ok
-- + {declare_cursor_like_name}: C: bar: { id: integer notnull, name: text, rate: longint } variable shape_storage value_cursor
-- + {name C}: C: bar: { id: integer notnull, name: text, rate: longint } variable shape_storage value_cursor
-- - dml_proc
-- - Error
create proc declare_cursor_like_table()
begin
  declare C cursor like bar;
end;

-- TEST: declare a cursor like a view
-- + {create_proc_stmt}: ok
-- + {name declare_cursor_like_view}: ok
-- + {declare_cursor_like_name}: C: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull } variable shape_storage value_cursor
-- + {name C}: C: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull } variable shape_storage value_cursor
-- - dml_proc
-- - Error
create proc declare_cursor_like_view()
begin
  declare C cursor like MyView;
end;

-- TEST: use like syntax to declare a cursor of the type of a select statement
-- + CREATE PROC declare_cursor_like_select ()
-- + DECLARE C CURSOR LIKE SELECT 1 AS A, 2.5 AS B, 'x' AS C;
-- + FETCH C(A, B, C) FROM VALUES(_seed_, _seed_, printf('C_%d', _seed_)) @DUMMY_SEED(123);
-- + {declare_cursor_like_select}: C: select: { A: integer notnull, B: real notnull, C: text notnull } variable shape_storage value_cursor
-- + {fetch_values_stmt}: ok
-- - dml_proc
-- - Error
create proc declare_cursor_like_select()
begin
  declare C cursor like select 1 A, 2.5 B, 'x' C;
  fetch C() from values() @dummy_seed(123);
  out C;
end;

-- TEST: a bogus cursor due to bogus expression in select
-- + {declare_cursor_like_select}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
declare some_cursor cursor like select 1 A, 2.5 B, not 'x' C;

-- TEST: duplicate cursor name
-- + {declare_cursor_like_select}: err
-- + Error % duplicate variable name in the same scope 'X'
-- +1 Error
declare X cursor like select 1 A, 2.5 B, 'x' C;

-- TEST: pull the rowid out of a table
-- + {select_stmt}: select: { rowid: longint notnull }
-- - Error
select rowid from foo;

-- TEST: pull a rowid from a particular table
-- + {select_stmt}: select: { rowid: longint notnull }
-- + SELECT T1.rowid
-- - Error
select T1.rowid from foo T1, bar T2;

-- TEST: name not unique, not found
-- + {select_stmt}: err
-- + Error % name not found 'rowid'
-- +1 Error
select T1.rowid from foo T2, foo T3;

-- TEST: rowid name ambiguous
-- + {select_stmt}: err
-- + Error % identifier is ambiguous 'rowid'
select rowid from foo T1, foo T2;

-- TEST: read the result of a non-dml proc;  we must not become a dml proc for doing so
-- - dml_proc
-- + {create_proc_stmt}: ok
-- + declare_value_cursor}: C: select: { A: integer notnull, B: real notnull, C: text notnull } variable shape_storage value_cursor
-- + call_stmt}: C: select: { A: integer notnull, B: real notnull, C: text notnull } variable shape_storage uses_out
-- - Error
create proc value_result_reader()
begin
  declare C cursor fetch from call declare_cursor_like_select();
end;

-- TEST: create table with misc attributes
-- + {stmt_and_attr}
-- + {create_table_stmt}: misc_attr_table: { id: integer notnull }
-- +4 {misc_attrs}
-- +4 {misc_attr}
-- +1 {name foo}
-- +1 {name goo}
-- +1 {name bar}
-- +1 {name baz}
-- +1 {name num}
-- +1 {uminus}
-- +1 {int 9}
-- + @ATTRIBUTE(foo)
-- + @ATTRIBUTE(goo)
-- + @ATTRIBUTE(num=-9)
-- + @ATTRIBUTE(bar=baz)
@attribute(foo)
@attribute(goo)
@attribute(num=-9)
create table misc_attr_table
(
  @attribute(bar = baz)
  id integer not null
);

-- TEST: complex index
create unique index if not exists my_unique_index on bar(id asc, name desc, rate);

-- TEST: try to update a table that does not exist
-- + {update_stmt}: err
-- + Error % table in update statement does not exist 'This_Table_Does_Not_Exist'
-- +1 Error
update This_Table_Does_Not_Exist set x = 1;

-- TEST: create a table with a valid FK on a column
-- + {create_table_stmt}: fk_on_col: { fk_src: integer foreign_key }
-- + {col_attrs_fk}: ok
-- + {name foo}
-- + {name id}: id: integer notnull
-- - Error
create table fk_on_col(
  fk_src integer references foo ( id ) on update cascade on delete set null
);

-- TEST: create a table with a bogus FK : too many cols
-- + {create_table_stmt}: err
-- + Error % the FK reference must be exactly one column with the correct type 'fk_src'
-- +1 Error
create table bogus_fk_on_col_1(
  fk_src integer references bar ( id, name ) on update cascade on delete set null
);

-- TEST: create a table with a bogus FK : wrong type
-- + {create_table_stmt}: err
-- + Error % the FK reference must be exactly one column with the correct type 'fk_src'
-- +1 Error
create table bogus_fk_on_col_1(
  fk_src integer references bar ( name )
);

-- TEST: create a table with a bogus FK : no such table
-- + {create_table_stmt}: err
-- + Error % foreign key refers to non-existent table 'no_such_table'
-- +1 Error
create table bogus_fk_on_col_1(
  fk_src integer references no_such_table ( name )
);

-- TEST: create a table with a bogus FK : no such column
-- + {create_table_stmt}: err
-- + Error % name not found 'no_such_column'
-- +1 Error
create table bogus_fk_on_col_1(
  fk_src integer references bar ( no_such_column )
);

-- TEST: create a table with a non-integer autoinc
-- + {create_table_stmt}: err
-- + Error % autoincrement column must be [LONG_]INTEGER PRIMARY KEY 'id'
-- +1 Error
create table bogus_autoinc_type(id bool primary key autoincrement);

-- TEST: create a table an autoinc and without rowid
-- + {create_table_stmt}: err
-- + Error % table has an AUTOINCREMENT column; it cannot also be WITHOUT ROWID 'bogus_without_rowid'
-- +1 Error
create table bogus_without_rowid(id integer primary key autoincrement) without rowid;

-- TEST: create a table that is going to be on the recreate plan
-- + {create_table_stmt}: recreatable: { id: integer notnull primary_key, name: text } @recreate
-- +  @RECREATE;
-- +  {recreate_attr}
-- - Error
create table recreatable(
  id integer primary key,
  name text
) @recreate;

-- TEST: create a table that is going to be on the recreate plan, try to version a column in it
-- + {create_table_stmt}: err
-- +  @RECREATE;
-- +  {recreate_attr}
-- + Error % columns in a table marked @recreate cannot have @create or @delete 'id'
create table column_marked_delete_on_recreate_table(
  id integer primary key @create(2),
  name text
) @recreate;

-- TEST: create a proc that uses the same CTE name twice, these should not conflict
-- + {create_proc_stmt}: select: { a: integer notnull, b: integer notnull } dml_proc
-- +2 {cte_tables}: ok
-- +2 {cte_table}: should_not_conflict: { a: integer notnull, b: integer notnull }
-- - Error
create proc cte_test()
begin
  with should_not_conflict(a,b) as (select 111,222)
  select * from should_not_conflict;
  with should_not_conflict(a,b) as (select 111,222)
  select * from should_not_conflict;
end;

-- TEST: use a CTE on a insert statement, all ok
-- - Error
-- + {with_insert_stmt}: ok
-- + {cte_table}: x: { a: integer notnull, b: text notnull, c: longint notnull }
-- + {insert_stmt}: ok
-- + {select_stmt}: a: integer
-- + {select_stmt}: b: text
-- + {select_stmt}: c: longint
-- + {insert_normal}
-- + {name_columns_values}
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
create proc with_insert_form()
begin
  with x(a,b,c) as (select 12, 'foo', 35L)
  insert into bar values (
     ifnull((select a from x), 0),
     ifnull((select b from x), 'foo'),
     ifnull((select 1L as c where 0), 0)
  );
end;

-- TEST: use a CTE on a insert statement using columns, all ok
-- - Error
-- + {with_insert_stmt}: ok
-- + {cte_table}: x: { a: integer notnull, b: text notnull, c: longint notnull }
-- + {insert_stmt}: ok
-- + {insert_normal}
-- + {name_columns_values}
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
create proc with_column_spec_form()
begin
  with x(a,b,c) as (select 12, 'foo', 35L)
  insert into bar(id,name,rate) values (
     ifnull((select a from x), 0),
     ifnull((select b from x), 'foo'),
     ifnull((select 1L as c where 0), 0)
  );
end;

-- TEST: with-insert form, CTE is bogus
-- + {with_insert_stmt}: err
-- + {cte_tables}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc with_insert_bogus_cte()
begin
  with x(a) as (select not 'x')
  insert into bar(id,name,rate) values (1, 'x', 2);
end;

-- TEST: with-insert form, insert clause is bogus
-- + {with_insert_stmt}: err
-- + {cte_tables}: ok
-- + {insert_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc with_insert_bogus_insert()
begin
  with x(a) as (select 1)
  insert into bar(id,name,rate) values (1, not 'x', 1);
end;

-- TEST: insert from select (this couldn't possibly run but it makes sense semantically)
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {select_stmt}: select: { id: integer notnull, name: text, rate: longint }
insert into bar select * from bar where id > 5;


-- TEST: insert from select, wrong number of columns
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {select_stmt}: select: { id: integer notnull }
-- + Error % count of columns differs from count of values
-- +1 Error
insert into bar select id from bar;

-- TEST: insert from select, type mismatch in args
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {select_stmt}: select: { name: text, id: integer notnull, rate: longint }
-- + Error % incompatible types in expression 'id'
-- +1 Error
insert into bar select name, id, rate from bar;

-- TEST: insert from select, bogus select
-- + {insert_stmt}: err
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {select_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
insert into bar select not 'x';

-- TEST: declare a function for use in select statements, this is a sqlite udf
-- + {declare_select_func_stmt}: real notnull select_func
-- + {param_detail}: id: integer variable in
-- - Error
declare select func SqlUserFunc(id integer) real not null;

-- TEST: now try to use the user function in a select statement
-- + SELECT SqlUserFunc(1);
-- + {select_stmt}: select: { _anon: real notnull }
-- + {call}: real notnull
-- + {name SqlUserFunc}
-- - Error
select SqlUserFunc(1);

-- TEST: now try to use the user function with distinct keyword
-- + SELECT SqlUserFunc(DISTINCT id)
-- + {select_stmt}: select: { _anon: real notnull }
-- + {call}: real notnull
-- + {name SqlUserFunc}
-- + {distinct}
-- + {arg_list}: ok
-- - Error
select SqlUserFunc(distinct id) from foo;

-- TEST: now try to use the user function with filter clause
-- + SELECT SqlUserFunc(DISTINCT id)
-- + {select_stmt}: select: { _anon: real notnull }
-- + {call}: real notnull
-- + {name SqlUserFunc}
-- + {call_filter_clause}
-- + {distinct}
-- + {arg_list}: ok
-- - Error
select SqlUserFunc(distinct id) filter (where 1) from foo;

-- TEST: now try to use the select user function loose
-- + {assign}: err
-- + {name my_real}: my_real: real variable
-- + {call}: err
-- + Error % User function may only appear in the context of a SQL statement 'SqlUserFunc'
-- +1 Error
set my_real := SqlUserFunc(1);

-- TEST: now try to use the select user function loose with distinct
-- + {assign}: err
-- + {name my_real}: my_real: real variable
-- + {call}: err
-- + Error % User function may only appear in the context of a SQL statement 'SqlUserFunc'
-- +1 Error
set my_real := SqlUserFunc(distinct 1);

-- TEST: now try to use the select user function loose with filter clause
-- + {assign}: err
-- + {name my_real}: my_real: real variable
-- + {call}: err
-- + Error % User function may only appear in the context of a SQL statement 'SqlUserFunc'
-- +1 Error
set my_real := SqlUserFunc(1) filter (where 0);

-- TEST: declare select func with an error in declartion
-- + Error % func name conflicts with proc name 'foo'
declare select func foo(x integer, x integer) integer;

-- TEST: create a cursor and fetch from arguments
-- AST rewritten
-- + CREATE PROC arg_fetcher (arg1 TEXT NOT NULL, arg2 INTEGER NOT NULL, arg3 REAL NOT NULL)
-- + FETCH curs(A, B, C) FROM VALUES(arg1, arg2, arg3);
-- +  | {fetch_values_stmt}: ok
-- +  | {name_columns_values}
-- +  | {name curs}: curs: select: { A: text notnull, B: integer notnull, C: real notnull } variable shape_storage value_cursor
-- +  | {columns_values}: ok
-- +  | {column_spec}
-- +  | | {name_list}
-- +  |   | {name A}: A: text notnull
-- +  |   | {name_list}
-- +  |     | {name B}: B: integer notnull
-- +  |     | {name_list}
-- +  |       | {name C}: C: real notnull
-- +  | {insert_list}
-- +  | {name arg1}: arg1: text notnull variable in
-- +  | {insert_list}
-- +  | {name arg2}: arg2: integer notnull variable in
-- +  | {insert_list}
-- +  | {name arg3}: arg3: real notnull variable in
create proc arg_fetcher(arg1 text not null, arg2 integer not null, arg3 real not null)
begin
  declare curs cursor like select 'x' A, 1 B, 3.5 C;
  fetch curs from arguments;
end;

-- TEST: use the arguments like "bar" even though there are other arguments
-- AST rewritten, note "extra" does not appear
-- + CREATE PROC fetch_bar (extra INTEGER, id_ INTEGER NOT NULL, name_ TEXT, rate_ LONG_INT)
-- + FETCH curs(id, name, rate) FROM VALUES(id_, name_, rate_);
-- + {create_proc_stmt}: ok
-- - Error
create proc fetch_bar(extra integer, like bar)
begin
  declare curs cursor like bar;
  fetch curs from arguments(like bar);
end;

-- TEST: scoped like arguments
-- + CREATE PROC qualified_like (x_id INTEGER NOT NULL, x_name TEXT, x_rate LONG_INT, y_id INTEGER NOT NULL, y_name TEXT, y_rate LONG_INT)
create proc qualified_like(x like bar, y like bar)
begin
end;

-- TEST: use the arguments like "bar" even though there are other arguments
-- AST rewritten, note "extra" does not appear
-- + CREATE PROC insert_bar (extra INTEGER, id_ INTEGER NOT NULL, name_ TEXT, rate_ LONG_INT)
-- + INSERT INTO bar(id, name, rate) VALUES(id_, name_, rate_);
-- + {create_proc_stmt}: ok
-- - Error
create proc insert_bar(extra integer, like bar)
begin
  insert into bar from arguments(like bar);
end;

-- TEST: use the arguments like "bar" some have trailing _ and some do not
-- AST rewritten, note some have _ and some do not
-- + INSERT INTO bar(id, name, rate) VALUES(id, name_, rate);
-- + {create_proc_stmt}: ok
-- - Error
create proc insert_bar_explicit(extra integer, id integer not null, name_ text, rate long integer)
begin
  insert into bar from arguments(like bar);
end;

-- TEST: use the arguments like "bar" but some args are missing
-- AST rewritten, note some have _ and some do not
-- + {create_proc_stmt}: err
-- + Error % expanding FROM ARGUMENTS, there is no argument matching 'name'
-- +1 Error
create proc insert_bar_missing(extra integer, id integer not null)
begin
  insert into bar from arguments(like bar);
end;

-- TEST: bogus name in the like part of from arguments
-- + {create_proc_stmt}: err
-- + {insert_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'bogus_name_here'
-- +1 Error
create proc insert_bar_from_bogus(extra integer, like bar)
begin
  insert into bar from arguments(like bogus_name_here);
end;

declare val_cursor cursor like my_cursor;

-- TEST: try to fetch a cursor from arguments but not in a procedure
-- + {fetch_values_stmt}: err
-- + Error % FROM ARGUMENTS construct is only valid inside a procedure
-- +1 Error
fetch val_cursor from arguments;

-- TEST: try to fetch a cursor but not enough arguments
-- + {fetch_values_stmt}: err
-- + Error % [shape] has too few fields 'ARGUMENTS'
-- +1 Error
create proc arg_fetcher_not_enough_args(arg1 text not null)
begin
  declare curs cursor like select 'x' A, 1 B, 3.5 C;
  fetch curs from arguments;
end;

-- TEST: rewrite insert statement using arguments
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + INSERT INTO bar(id, name, rate) VALUES(id, name, rate);
-- These appear as a parameter AND in the insert list
-- +1 {name id}: id: integer notnull variable in
-- +1 {name name}: name: text variable in
-- +1 {name rate}: rate: longint variable in
-- - Error
create proc bar_auto_inserter(id integer not null, name text, rate LONG INT)
begin
 insert into bar from arguments;
end;

-- TEST: rewrite insert statement but minimal columns
-- + {insert_stmt}: ok
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + INSERT INTO bar(id) VALUES(id);
-- These appear as a parameters
-- +1 {name id}: id: integer notnull variable in
-- +1 {name name}: name: text variable in
-- +1 {name rate}: rate: longint variable in
-- - Error
create proc bar_auto_inserter_mininal(id integer not null, name text, rate LONG INT)
begin
 insert into bar(id) from arguments;
end;

-- TEST: rewrite insert statement but no columns, bogus
-- + {insert_stmt}: err
-- + INSERT INTO bar() FROM ARGUMENTS
-- + Error % FROM [shape] is redundant if column list is empty
create proc bar_auto_inserter_no_columns(id integer not null, name text, rate LONG INT)
begin
 insert into bar() from arguments @dummy_seed(1);
end;

-- TEST: rewrite insert statement but not enough columns
-- + {insert_stmt}: err
-- + INSERT INTO bar(id, name, rate) FROM ARGUMENTS(id);
-- + Error % [shape] has too few fields 'ARGUMENTS'
create proc bar_auto_inserter_missing_columns(id integer)
begin
 insert into bar from arguments;
end;

-- TEST: rewrite proc arguments using the LIKE table form
-- - Error
-- + CREATE PROC rewritten_like_args (id_ INTEGER NOT NULL, name_ TEXT, rate_ LONG_INT)
-- + INSERT INTO bar(id, name, rate) VALUES(id_, name_, rate_);
-- + {create_proc_stmt}: ok dml_proc
-- + {param}: id_: integer notnull variable in
-- + {param}: name_: text variable in
-- + {param}: rate_: longint variable in
-- + {name bar}: bar: { id: integer notnull, name: text, rate: longint }
-- + {insert_stmt}: ok
-- these appear as a parameter and also in the insert list
-- +1 {name id_}: id_: integer notnull variable in
-- +1 {name name_}: name_: text variable in
-- +1 {name rate_}: rate_: longint variable in
-- the clean name appears in the insert list and as a column
-- +2 {name id}
-- +2 {name name}
-- +2 {name rate}
-- the ARGUMENTS dot name resolves to the correct arg name
-- +1 {dot}: id_: integer notnull variable in
-- +1 {dot}: name_: text variable in
-- +1 {dot}: rate_: longint variable in
create proc rewritten_like_args(like bar)
begin
  insert into bar from arguments;
end;

-- TEST: try to rewrite args on a bogus table
-- + {create_proc_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'garbonzo'
-- +1 Error
create proc rewrite_args_fails(like garbonzo)
begin
  declare x integer;
end;

-- a fake table for some args
create table args1(
 id integer primary key,
 name text,
 data blob
);

-- a fake table for some more args
create table args2(
 id integer references args1(id),
 name2 text,
 rate real
);

-- TEST: this procedure uses two tables for its args, the trick here is that both tables
--       have the id_ column;  we should only emit it once
-- note that id_ was skipped the second time
-- + CREATE PROC two_arg_sources (id_ INTEGER NOT NULL, name_ TEXT, data_ BLOB, name2_ TEXT, rate_ REAL)
-- {create_proc_stmt): ok
-- - Error
create proc two_arg_sources(like args1, like args2)
begin
end;

-- TEST: test the case where 2nd and subsequent like forms do nothing
-- + CREATE PROC two_arg_sources_fully_redundant (id_ INTEGER NOT NULL, name_ TEXT, data_ BLOB)
-- {create_proc_stmt): ok
-- - Error
create proc two_arg_sources_fully_redundant(like args1, like args1, like args1)
begin
end;

create view ViewShape as select cast(1 as bool) a, 2.5 b, 'xyz' c;

-- + CREATE PROC like_a_view (a_ BOOL NOT NULL, b_ REAL NOT NULL, c_ TEXT NOT NULL)
-- +   SELECT *
-- +     FROM ViewShape AS v
-- +     WHERE v.a = a_ AND v.b = b_ AND v.c > c_;
-- +   {create_proc_stmt}: select: { a: bool notnull, b: real notnull, c: text notnull } dml_proc
create proc like_a_view(like ViewShape)
begin
  select * from ViewShape v where v.a = a_ and v.b = b_ and v.c > c_;
end;

-- TEST: try to create a cursor that is like a select statement with not all names present
-- + Error % all columns in the select must have a name
-- +1 Error
create proc bogus_cursor_shape()
begin
  declare C cursor like select 1, 2;
end;

-- TEST: views must have a name for every column
-- + all columns in the select must have a name
-- + Error
create view MyBogusView as select 1, 2;

-- TEST: make this proc accept args to fake the result of another proc
-- + {create_proc_stmt}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage uses_out
-- + {declare_cursor_like_name}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- + {out_stmt}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- - Error
create proc like_other_proc(like out_cursor_proc)
begin
 declare C cursor like out_cursor_proc;
 fetch C from arguments;
 out C;
end;

-- TEST: create a proc using another proc that doesn't have a result type
-- + {create_proc_stmt}: err
-- + Error % proc has no result 'proc1'
-- +1 Error
create procedure bogus_like_proc(like proc1)
begin
  declare x int;
end;

-- TEST: create a non-temporary table using another table
-- + create_table_stmt% nontemp_table_like_table: % id: integer
-- - Error
create temp table nontemp_table_like_table(
  like foo
);

-- TEST: create a temporary table using another table
-- + create_table_stmt% table_like_table: % id: integer
-- - Error
create temp table table_like_table(
  like foo
);

-- TEST: create a temporary table using a view
-- + {create_table_stmt}: table_like_view: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- - Error
create temp table table_like_view(
  like MyView
);

-- TEST: create a temporary table using a proc
-- + {create_table_stmt}: table_like_proc: { id: integer notnull, name: text, rate: longint }
-- - Error
create temp table table_like_proc(
  like with_result_set
);

-- TEST: try to create a table with a proc with no result
-- + {create_table_stmt}: err
-- + {col_key_list}: err
-- + Error % proc has no result 'proc1'
-- +1 Error
create temp table table_like_proc_with_no_result(
  like proc1
);

-- TEST: try to create a table with non existent view/proc/view
-- + {create_table_stmt}: err
-- + {col_key_list}: err
-- + Error % must be a cursor, proc, table, or view 'this_thing_doesnt_exist'
-- +1 Error
create temp table table_like_nonexistent_view(
  like this_thing_doesnt_exist
);

-- TEST: create a temp table using two like arguments
-- + {create_table_stmt}: table_multiple_like: { f1: integer notnull, f2: integer notnull, f3: integer notnull, id: integer notnull, name: text, rate: longint }
-- - Error
create temp table table_multiple_like(
  like MyView, like with_result_set
);

-- TEST: create a temp table using mix of like and normal columns
-- + {create_table_stmt}: table_like_mixed: { garbage: text, f1: integer notnull, f2: integer notnull, f3: integer notnull, happy: integer }
-- - Error
create temp table table_like_mixed(
  garbage text, like MyView, happy integer
);

-- TEST: try to create a temp table but there is a duplicate column after expanding like
-- + {create_table_stmt}: err
-- + Error % duplicate column name 'f1'
-- +1 Error
create temp table table_with_dup_col(
  f1 text, like MyView
);

-- TEST: try to create a temp table with versioning -- not allowed
-- + {create_table_stmt}: err
-- + Error % temp tables may not have versioning annotations 'bogus_temp_with_create_versioning'
-- +1 Error
create temp table bogus_temp_with_create_versioning(
  id integer
) @create(1);

-- TEST: try to create a temp table with versioning -- not allowed
-- + {create_table_stmt}: err
-- + Error % temp tables may not have versioning annotations 'bogus_temp_with_delete_versioning'
-- +1 Error
create temp table bogus_temp_with_delete_versioning(
  id integer
) @delete(1);

-- TEST: try to create a temp table with recreate versioning -- not allowed
-- + {create_table_stmt}: err
-- + Error % temp tables may not have versioning annotations 'bogus_temp_with_recreate_versioning'
-- +1 Error
create temp table bogus_temp_with_recreate_versioning(
  id integer
) @recreate;

-- TEST: try to create a temp table with versioning in a column -- not allowed
-- + {create_table_stmt}: err
-- + Error % columns in a temp table may not have versioning attributes 'id'
-- +1 Error
create temp table bogus_temp_with_versioning_in_column(
  id integer @create(2)
);

-- TEST: try to use match in a select statement
-- - Error
-- + {select_expr}: bool notnull
-- + {match}: bool notnull
-- + {strlit 'x'}: text notnull
-- + {strlit 'y'}: text notnull
select 'x' match 'y';

-- TEST: try to use match not in a select statement
-- + {match}: err
-- + {assign}: err
-- + Error % operator may only appear in the context of a SQL statement 'MATCH'
-- +1 Error
set X := 'x' match 'y';

-- TEST: try to use glob in a select statement
-- - Error
-- + {select_expr}: bool notnull
-- + {glob}: bool notnull
-- + {strlit 'x'}: text notnull
-- + {strlit 'y'}: text notnull
select 'x' glob 'y';

-- TEST: try to use glob not in a select statement
-- + {glob}: err
-- + {assign}: err
-- + Error % operator may only appear in the context of a SQL statement 'GLOB'
-- +1 Error
set X := 'x' GLOB 'y';

-- TEST: try to use match not in a select statement
-- + {match}: err
-- + {assign}: err
-- + Error % operator may only appear in the context of a SQL statement 'MATCH'
-- +1 Error
set X := 'x' MATCH 'y';

-- TEST: try to use regexp not in a select statement
-- + {regexp}: err
-- + {assign}: err
-- + Error % operator may only appear in the context of a SQL statement 'REGEXP'
-- +1 Error
set X := 'x' REGEXP 'y';

-- TEST: REGEXP inside of SQL is ok
-- + {regexp}: bool notnull
-- - Error
set X := (select 'x' REGEXP 'y');

-- TEST: shift and bitwise operators
-- + {assign}: X: integer variable
-- + {name X}: X: integer variable
-- + {rshift}: integer notnull
-- + {bin_and}: integer notnull
-- + {lshift}: integer notnull
-- + {bin_or}: integer notnull
-- + {lshift}: integer notnull
-- - Error
-- + SET X := 1 << 2 | 1 << 4 & 1 >> 8;
set X := 1 << 2 | 1 << 4 & 1 >> 8;

-- TEST: try a integer operator with a real
-- + {assign}: err
-- + {bin_and}: err
-- + Error % operands must be an integer type, not real '&'
-- +1 Error
set X := 3.0 & 2;

-- TEST: try a integer unary operator with a real
-- + {assign}: err
-- + {tilde}: err
-- + Error % operands must be an integer type, not real '~'
-- +1 Error
set X := ~3.0;

-- TEST: try to access a column by its select alias in the later portions of the select
-- + {select_stmt}: select: { a: integer notnull, b: integer notnull }
-- + {name b}: b: integer notnull
-- - Error
select 1 a, 2 b from (select 1) as T where b = 2;

-- TEST: use column aliases in ORDER BY statement
-- + {create_proc_stmt}: select: { bar_id: integer notnull } dml_proc
-- + {name bar_id}: bar_id: integer notnull
-- - Error
create proc simple_alias_order_by()
begin
  select id as bar_id
  from bar
  order by bar_id;
end;

-- TEST: use column aliases for fabricated columns in ORDER BY statement
-- + {create_proc_stmt}: union_all: { sort_order_value: integer notnull, id: integer notnull } dml_proc
-- + {name sort_order_value}: sort_order_value: integer notnull
-- - Error
create proc complex_alias_order_by()
begin
  select 1 as sort_order_value, id from bar
  union all
  select 2 as sort_order_value, id from bar
  order by sort_order_value, id;
end;

-- TEST: fake stories table for test case stolen from the real schema
create table stories(media_id long);

-- TEST: ensure that the column alias is used as the second choice compared to a column name
-- This won't compile correctly if the binding is wrong, the int will be compared to a string
-- + {select_stmt}: select: { media_id: text notnull }
-- + {name media_id}: media_id: longint
-- - {name media_id}: media_id: text notnull
-- - Error
SELECT 'xyzzy' AS media_id
FROM stories
WHERE media_id = 123;

-- TEST: basic delete trigger
-- + CREATE TEMP TRIGGER IF NOT EXISTS trigger1
-- +   BEFORE DELETE ON bar
-- +   FOR EACH ROW
-- +   WHEN old.id = 3
-- + {create_trigger_stmt}: ok
-- + {eq}: bool notnull
-- + {dot}: id: integer notnull
-- +2 {delete_stmt}: ok
-- - Error
create temp trigger if not exists trigger1
  before delete on bar
  for each row
  when old.id = 3
begin
  delete from bar where rate > id;
  delete from bar where rate = old.id;
end;

-- TEST: basic delete trigger, try to use "new"
-- + CREATE TRIGGER trigger1a
-- +   BEFORE DELETE ON bar
-- +   WHEN new.id = 3
-- + {create_trigger_stmt}: err
-- + Error % name not found 'id'
-- +1 Error
create trigger trigger1a
  before delete on bar
  when new.id = 3
begin
  delete from bar where rate > id;
end;

-- TEST: basic insert trigger
-- + CREATE TRIGGER trigger2
-- +   AFTER INSERT ON bar
-- + BEGIN
-- +   DELETE FROM bar WHERE rate > new.id;
-- + END;
-- +  {create_trigger_stmt}: ok
-- +1 {delete_stmt}: ok
-- - Error
create trigger trigger2
  after insert on bar
begin
  delete from bar where rate > new.id;
end;

-- TEST: basic insert trigger, try to use "old"
-- + CREATE TRIGGER trigger2a
-- +   AFTER INSERT ON bar
-- +   WHEN old.id = 3
-- + {create_trigger_stmt}: err
-- + Error % name not found 'id'
-- +1 Error
create trigger trigger2a
  after insert on bar
  when old.id = 3
begin
  delete from bar where rate > id;
end;

-- TEST: use update instead of on a view
-- + {create_trigger_stmt}: ok
-- +4 {dot}: b: real notnull
-- + {name ViewShape}
-- + {update_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- + {insert_stmt}: ok
-- - Error
create trigger trigger3
  instead of update on ViewShape
  when old.b > 1 and new.b < 3
begin
  update bar set id = 7 where rate > old.b and rate < new.b;
  insert into bar values (7, 'goo', 17L);
end;

-- TEST: exact duplicate trigger is ok
-- + {create_trigger_stmt}: ok
-- +4 {dot}: b: real notnull
-- + {name ViewShape}
-- + {update_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- + {insert_stmt}: ok
-- - Error
create trigger trigger3
  instead of update on ViewShape
  when old.b > 1 and new.b < 3
begin
  update bar set id = 7 where rate > old.b and rate < new.b;
  insert into bar values (7, 'goo', 17L);
end;

-- TEST: duplicate trigger
-- + {create_trigger_stmt}: err
-- + {name ViewShape}
-- + Error % trigger already exists 'trigger3'
create trigger trigger3
  instead of update on ViewShape
begin
  select 1;
end;

-- TEST: specify update columns
-- + {create_trigger_stmt}: ok
-- + {name a}: a: bool notnull
-- + {name b}: b: real notnull
-- + {name c}: c: text notnull
-- - Error
create trigger trigger4
  instead of update of a, b, c on ViewShape
begin
  select 1;
end;

-- TEST: specify update columns
-- + {create_trigger_stmt}: err
-- + Error % name list has duplicate name 'a'
-- + Error
create trigger trigger4a
  instead of update of a, a, c on ViewShape
begin
  select 1;
end;

-- TEST: specify a view where one is not allowed
-- + {create_trigger_stmt}: err
-- + Error % a trigger on a view must be the INSTEAD OF form 'ViewShape'
-- + Error
create trigger trigger4b
  before update on ViewShape
begin
  select 1;
end;

-- TEST: specify a bogus table name
-- + {create_trigger_stmt}: err
-- + Error % table/view not found 'no_such_table_dude'
-- + Error
create trigger trigger4c
  before update on no_such_table_dude
begin
  select 1;
end;

-- TEST: specify a bogus executed statement
-- + {create_trigger_stmt}: err
-- + {stmt_list}: err
-- + {select_stmt}: err
-- + Error % name not found 'id'
-- + Error
create trigger trigger4d
  before insert on bar
begin
  select old.id;
end;

-- TEST: this proc is not a result proc even though it looks like it has a loose select...
-- the select is inside a trigger, it is NOT a return for this proc
-- - {create_proc_stmt}: select: { id: integer notnull } dml_proc
-- + {create_proc_stmt}: ok dml_proc
-- - Error
create proc make_trigger()
begin
  create trigger selecting_trigger
    before delete on bar
    for each row
    when old.id > 7
  begin
    select old.id;
  end;
end;

-- TEST: try to drop a trigger (bogus)
-- + {drop_trigger_stmt}: err
-- + Error % trigger in drop statement was not declared 'this_trigger_does_not_exist'
-- +1 Error
drop trigger this_trigger_does_not_exist;

-- TEST: try to drop a trigger (bogus)
-- + {drop_trigger_stmt}: ok
-- + {int 1}
-- - Error
drop trigger if exists trigger1;

-- TEST: try to delete  a table before it was created
-- + {create_table_stmt}: err
-- + Error % delete version can't be <= create version 'retro_deleted_table'
-- +1 Error
create table retro_deleted_table( id integer) @create(3) @delete(1);

-- TEST: basic delete trigger with RAISE expression
-- + {create_trigger_stmt}: ok
-- + {raise}: null
-- - Error
create temp trigger if not exists trigger5
  before delete on bar
begin
  select RAISE(rollback, "omg roll it back!");
end;

-- TEST: try to use raise in a non trigger context
-- + {select_stmt}: err
-- + {raise}: err
-- + Error % RAISE may only be used in a trigger statement
-- +1 Error
select RAISE(ignore);

-- TEST: try to use raise with a bogus string
-- + {create_trigger_stmt}: err
-- + {raise}: err
-- + Error % RAISE 2nd argument must be a string
-- +1 Error
create temp trigger if not exists trigger6
  before delete on bar
begin
  select RAISE(rollback, 0);
end;

-- TEST: try to use raise with a bogus expression
-- + {create_trigger_stmt}: err
-- + {raise}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create temp trigger if not exists trigger7
  before delete on bar
begin
  select RAISE(rollback, not 'x');
end;

-- TEST: try to create a trigger with a migrate proc
-- + {create_trigger_stmt}: err
-- + Error % migration proc not allowed on object 'trigger8'
-- +1 Error
create temp trigger if not exists trigger8
  before delete on bar
begin
  select 1 x;
end @delete(1, MigrateProcFoo);

-- TEST: try to select union with different number of columns
-- + {select_core_list}: err
-- + {select_core_compound}
-- +2 {int 2}
-- + Error % if multiple selects, all must have the same column count
-- +1 Error
select 1 as A, 2 as B, 3 as C
union all
select 3 as A, 4 as B;

-- TEST: try to select union with different incompatible types
-- + {select_core_list}: err
-- + {select_core_compound}
-- +2 {int 2}
-- + Error % incompatible types in expression 'A'
-- +1 Error
select 1 as A, 2 as B
union all
select 'x' as A, 4 as B;

-- TEST: try to select union with different compatible types (null checks)
-- + {select_core_list}: union_all: { A: integer, B: integer }
-- + {select_core_compound}
-- + {int 2}
-- + {select_core}: select: { A: integer notnull, B: integer }
-- + {select_core}: select: { A: null, B: integer notnull }
-- - Error
select 1 as A, nullable(2) as B
union all
select NULL as A, 4 as B;

-- TEST: try to select union multiple times
-- + {select_stmt}: union_all: { A: integer notnull, B: integer notnull }
-- + {select_core_compound}
-- +7 {int 2}
-- +3 {select_core_list}: union_all: { A: integer notnull, B: integer notnull }
-- +4 {select_core}: select: { A: integer notnull, B: integer notnull }
-- - Error
select 1 as A, 2 as B
union all
select 1 as A, 2 as B
union all
select 1 as A, 2 as B
union all
select 1 as A, 2 as B;

-- TEST: try to return untyped NULL
-- + {create_proc_stmt}: err
-- + Error % NULL column did not specify a type 'n'
-- +1 Error
create proc returns_bogus_null()
begin
  select null AS n;
end;

-- TEST: try to declare cursor for untyped NULL
-- + {create_proc_stmt}: err
-- + Error % NULL column did not specify a type 'n'
-- +1 Error
create proc fetch_null_column()
begin
  declare C cursor for select null AS n;
  fetch C;
end;

-- TEST: declare a column as
-- + {create_table_stmt}: with_sensitive: { id: integer, name: text sensitive, info: integer sensitive }
-- - Error
create table with_sensitive(
 id integer,
 name text @sensitive,
 info integer @sensitive
);

-- TEST: declare a table to test with with_sensitive table with non-sensitive column as
-- + {create_table_stmt}: without_sensitive: { name: text }
-- - Error
create table without_sensitive(
 name text
);

-- TEST: select out some
-- + {create_proc_stmt}: select: {
-- + safe: integer notnull,
-- + sensitive_1: integer sensitive,
-- + sensitive_2: text sensitive,
-- + not_sensitive_1: text notnull,
-- + sensitive_3: integer sensitive,
-- + sensitive_4: bool sensitive
-- + } dml_proc
create proc get_sensitive()
begin
  select 1 as safe,
        info+1 sensitive_1,
        name as sensitive_2,
        'x' as not_sensitive_1,
        -info as sensitive_3,
        info between 1 and 3 as sensitive_4
  from with_sensitive;
end;

-- TEST: making a sensitive variable
-- + {declare_vars_type}: integer sensitive
-- - Error
declare _sens integer @sensitive;

-- TEST: using sensitive in the LIMIT clause
-- + {select_stmt}: select: { safe: integer notnull sensitive }
-- - Error
select 1 as safe
limit _sens;

-- TEST: using sensitive in the LIMIT clause (control case)
-- + {select_stmt}: select: { safe: integer notnull }
-- - Error
select 1 as safe
limit 1;

-- TEST: using sensitive in the OFFSET clause (control case)
-- + {select_stmt}: select: { safe: integer notnull sensitive }
-- - Error
select 1 as safe
limit 1
offset _sens;

-- TEST: using sensitive in the OFFSET clause (control case)
-- + {select_stmt}: select: { safe: integer notnull }
-- - Error
select 1 as safe
limit 1
offset 1;

-- TEST: local  arithmetic
-- + {add}: integer sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- - Error
set _sens := _sens + 1;

-- TEST: in an IN expression (needle)
-- + {in_pred}: bool sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- - Error
set _sens := _sens in (1, 2);

-- TEST: in an IN expression (haystack)
-- + {in_pred}: bool notnull sensitive
-- + {int 1}: integer notnull
-- + {expr_list}: _sens: integer variable sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- - Error
set _sens := 1 in (1, _sens);

-- TEST: in an IN expression (select form)
-- + {in_pred}: bool notnull sensitive
-- + {select_stmt}: _anon: bool notnull sensitive
-- - Error
set _sens := (select 1 in (select info from with_sensitive));

-- TEST: in a CASE statement (control case)
-- + {case_expr}: integer notnull
-- + {int 0}: integer notnull
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- + {int 3}: integer notnull
-- - Error
set _sens := case 0 when 1 then 2 else 3 end;

-- TEST: in a CASE statement (sensitive in the main expression)
-- + {case_expr}: integer notnull sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- + {case_list}: integer notnull
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- + {int 3}: integer notnull
-- - Error
set _sens := case _sens when 1 then 2 else 3 end;

-- TEST: in a CASE statement (sensitive in the when part)
-- + {case_expr}: integer notnull sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- + {case_list}: integer notnull
-- + {int 0}: integer notnull
-- + {int 2}: integer notnull
-- + {int 3}: integer notnull
-- - Error
set _sens := case 0 when _sens then 2 else 3 end;

-- TEST: in a CASE statement (sensitive in the then part)
-- + {case_expr}: integer sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- + {case_list}: integer variable sensitive
-- + {int 0}: integer notnull
-- + {int 1}: integer notnull
-- + {int 3}: integer notnull
-- - Error
set _sens := case 0 when 1 then _sens else 3 end;

-- TEST: in a CASE statement (sensitive in the else part)
-- + {case_expr}: integer sensitive
-- + {name _sens}: _sens: integer variable sensitive
-- + {case_list}: integer notnull
-- + {int 0}: integer notnull
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- - Error
set _sens := case 0 when 1 then 2 else _sens end;

-- TEST: make sure that cast preserves
-- + {select_stmt}: _anon: integer sensitive
-- - Error
set _sens := (select cast(_sens as INT));

-- TEST: make sure AVG preserves
-- + {name AVG}: info: real sensitive
-- - Error
select AVG(T1.info) from with_sensitive T1;

-- TEST: make sure MIN preserves
-- + {name MIN}: info: integer sensitive
-- - Error
select MIN(T1.info) from with_sensitive T1;

-- TEST: make sure MAX preserves
-- + {name MAX}: info: integer sensitive
-- - Error
select MAX(T1.info) from with_sensitive T1;

-- TEST: make sure SUM preserves
-- + {name SUM}: info: integer sensitive
-- - Error
select SUM(T1.info) from with_sensitive T1;

-- TEST: make sure COUNT preserves
-- + {name COUNT}: info: integer notnull sensitive
-- - Error
select COUNT(T1.info) from with_sensitive T1;

-- TEST: control  AVG
-- - {name AVG}: id: % sensitive
-- + {name AVG}: id: real
-- - Error
select AVG(T1.id) from with_sensitive T1;

-- TEST: control  MAX
-- - {name MAX}: id: % sensitive
-- + {name MAX}: id: integer
-- - Error
select MAX(T1.id) from with_sensitive T1;

-- TEST: control  SUM
-- - {name SUM}: id: % sensitive
-- + {name SUM}: id: integer
-- - Error
select SUM(T1.id) from with_sensitive T1;

-- TEST: control  COUNT
-- - {name COUNT}: id: % sensitive
-- + {name COUNT}: id: integer notnull
-- - Error
select COUNT(T1.id) from with_sensitive T1;

-- TEST: coalesce
-- + {call}: integer notnull sensitive
-- - Error
set _sens := coalesce(_sens, 0);

-- TEST: coalesce  control
-- - {call}: % sensitive
-- - Error
set _sens := coalesce(1, 0);

-- TEST: sensitive with IS right
-- + {is}: bool notnull sensitive
-- - Error
set _sens := 0 is _sens;

-- TEST: sensitive with IS left
-- + {is}: bool notnull sensitive
-- - Error
set _sens := _sens is 0;

-- TEST: sensitive with IS control
-- - {is}: % sensitive
-- + {is}: bool notnull
-- - Error
set _sens := 0 is 0;

-- TEST: sensitive with IS NOT right
-- + {is_not}: bool notnull sensitive
-- - Error
set _sens := 0 is not _sens;

-- TEST: sensitive with IS NOT left
-- + {is_not}: bool notnull sensitive
-- - Error
set _sens := _sens is not 0;

-- TEST: sensitive with IS NOT control
-- - {is_not}: % sensitive
-- + {is_not}: bool notnull
-- - Error
set _sens := 0 is not 0;

-- TEST: sensitive with EXISTS(select *)
-- + {exists_expr}: bool notnull sensitive
-- - Error
set _sens := (select exists(select * from with_sensitive));

-- TEST: sensitive with EXISTS(select sensitive)
-- + {exists_expr}: bool notnull sensitive
-- - Error
set _sens := (select exists(select info from with_sensitive));

-- TEST: sensitive with EXISTS(select not sensitive)
-- - {exists_expr}: bool notnull sensitive
-- + {exists_expr}: bool notnull
-- - Error
set _sens := (select exists(select id from with_sensitive));

-- TEST: sensitive implicit due to where clause
-- + {select_stmt}: id: integer sensitive
-- + {opt_where}: bool sensitive
-- - Error
set _sens := (select id from with_sensitive where info = 1);

-- TEST: select implicit control case (where not sensitive)
-- - {select_stmt}: id: integer sensitive
-- + {select_stmt}: id: integer
-- - {opt_where}: % sensitive
-- + {opt_where}: bool
-- - Error
set _sens := (select id from with_sensitive where id = 1);

-- TEST: sensitive implicit due to having clause
-- + {select_stmt}: id: integer sensitive
-- + {opt_having}: bool sensitive
-- - Error
set _sens := (select id from with_sensitive group by info having info = 1);

-- TEST: assign sensitive column value to a non-sensitive colunm
-- + {insert_stmt}: err
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'name'
insert into without_sensitive select name from with_sensitive;

create table a (
  key_ int not null primary key,
  sort_key int not null
);

create table b (
  key_ int not null primary key,
  a_key_ int not null,
  sort_key int not null
);

-- TEST: compound select name lookup from select list (other places ambiguous, still ok)
-- + {select_stmt}: union_all: { key_: integer notnull, sort_key: integer notnull }
-- + {select_core_list}: union_all: { key_: integer notnull, sort_key: integer notnull }
-- + {select_core_compound}
-- + ORDER BY sort_key, key_;
-- + {opt_orderby}: ok
-- - Error
select a.key_, a.sort_key
  from a
union all
select b.key_, b.sort_key
  from a
  inner join b ON b.a_key_ = a.key_
order by sort_key, key_;

-- TEST: compound select name lookup using something other than the select list
-- + {opt_orderby}: err
-- + ORDER BY a_key_
-- + Error % name not found 'a_key_'
-- + {int 2}: integer notnull
-- + {int 3}: integer notnull
-- +1 Error
select a.key_, a.sort_key
  from a
union all
select b.key_, b.sort_key
  from a
  inner join b on b.a_key_ = a.key_
order by a_key_
limit 2
offset 3;

-- TEST: compound select name lookup using something other than the select list (explicit)
-- + {opt_orderby}: err
-- + ORDER BY b.a_key_;
-- + Error % name not found 'a_key_'
-- +1 Error
select a.key_, a.sort_key
  from a
union ALL
select b.key_, b.sort_key
  from a
  inner join b ON b.a_key_ = a.key_
order by b.a_key_;

-- TEST: join columns become  because ON condition is SENSITIVE
-- + {select_stmt}: select: { id: integer notnull sensitive }
-- - Error
select T1.id from bar T1 inner join with_sensitive T2 on T1.id = T2.id and T2.info = 1;

-- TEST: join columns  flag ON condition (control case)
-- + {select_stmt}: select: { id: integer notnull }
-- - {select_stmt}: select: { id: % sensitive }
-- - Error
select T1.id from bar T1 inner join with_sensitive T2 on T1.id = T2.id;

-- TEST: join columns become  because USING condition has SENSITIVE columns
-- + {select_stmt}: select: { id: integer sensitive }
-- + {name_list}: info: integer sensitive
-- - Error
select T1.id from with_sensitive T1 inner join with_sensitive T2 using(info);

-- TEST: join columns do not become  because USING condition has no SENSITIVE columns
-- + {select_stmt}: select: { id: integer }
-- - {select_stmt}: select: { id: % sensitive }
-- + {name_list}: id: integer
-- - Error
select T1.id from with_sensitive T1 inner join with_sensitive T2 using(id);

-- TEST: try to assign sensitive data to a non-sensitive variable
-- + {assign}: err
-- + {name _sens}: _sens: integer variable sensitive
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'X'
-- +1 Error
set X := _sens;

-- TEST: try to call a normal proc with a sensitive parameter
-- + {call_stmt}: err
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'id'
-- +1 Error
call decl1(_sens);

declare proc sens_proc(out foo integer @sensitive);
declare proc non_sens_proc(out foo integer);
declare proc non_sens_proc_nonnull(out foo integer not null);

-- TEST: try to call a proc with a sensitive out parameter
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'X'
-- +1 Error
call sens_proc(X);

-- TEST: control case: ok to call a proc with a non-sensitive out parameter
-- + {name _sens}: _sens: integer variable sensitive
-- - Error
call non_sens_proc(_sens);

-- TEST: make sure we can't call a proc that takes a nullable int out with a not-null integer
-- Error % cannot assign/copy possibly null expression to not null target 'int_nn'
-- +1 Error
call non_sens_proc(int_nn);

-- TEST: make sure we can't call a proc that takes a non-nullable int out with a nullable integer
-- Error % proc out parameter: arg must be an exact type match (even nullability) (expected integer notnull; found integer)
-- +1 Error
call non_sens_proc_nonnull(X);

-- TEST: try to insert sensitive data to a non-sensitive column
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'id'
-- +1 Error
insert into foo(id) values(coalesce(_sens,0));

-- TEST: try to update to sensitive
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'id'
-- +1 Error
update bar set id = coalesce(_sens,0) where name = 'x';

-- Do various validations on this func in the following tests
declare function sens_func(id integer @sensitive, t text) text @sensitive;
declare sens_text text @sensitive;
declare non_sens_text text;

-- TEST: ok to assign to sensitive text, ok to pass non-sensitive integer as a sensitive integer
-- + {assign}: sens_text: text variable sensitive
-- + {name sens_text}: sens_text: text variable sensitive
-- + {call}: text sensitive
-- - Error
set sens_text := sens_func(1, 'x');

-- TEST: not ok to assign to non-sensitive text
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 'non_sens_text'
-- + {assign}: err
-- + {name non_sens_text}: err
-- + {call}: text sensitive
-- +1 Error
set non_sens_text := sens_func(1, 'x');

-- TEST: not ok to pass sensitive text as non-sensitive arg
-- + Error % cannot assign/copy sensitive expression to non-sensitive target 't'
-- + {call}: err
-- +1 Error
set sens_text := sens_func(1, sens_text);

-- TEST: make sure that the expression in the update is evaluated in the select context
--       this allows you to use things like CAST or date operations
-- + {update_stmt}: foo: { id: integer notnull primary_key autoinc }
-- + {cast_expr}: integer notnull
-- - Error
update foo set id = cast(1 as integer);

-- TEST: basic delete stmt with CTE form
-- + {with_delete_stmt}: ok
-- + {select_from_etc}: TABLE { x: x }
-- - Error
create proc with_delete_form()
begin
  with x(id) as (select 1 union all select 2)
  delete from bar where id in (select * from x);
end;

-- TEST: basic delete stmt with CTE form (CTE bogus)
-- + {create_proc_stmt}: err
-- + {with_delete_stmt}: err
-- + {cte_tables}: err
-- + {select_expr_list_con}: select: { _anon: integer notnull }
-- + {select_expr_list_con}: select: { _anon: text notnull }
-- + Error % incompatible types in expression '_anon'
-- +1 Error
create proc with_delete_form_bogus_cte()
begin
  with x(id) as (select 1 union all select 'x')
  delete from bar where id in (select * from x);
end;

-- TEST: basic delete stmt with CTE form (delete bogus)
-- + {create_proc_stmt}: err
-- + {with_delete_stmt}: err
-- + Error % table in delete statement does not exist 'not_valid_table'
-- +1 Error
create proc with_delete_form_bogus_delete()
begin
  with x(id) as (select 1 union all select 2)
  delete from not_valid_table where id in (select * from x);
end;

-- TEST: basic update stmt with CTE form
-- + {with_update_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- + {select_from_etc}: TABLE { x: x }
-- - Error
create proc with_update_form()
begin
  with x(id) as (select 1 union all select 2)
  update bar set name = 'xyzzy' where id in (select * from x);
end;

-- TEST: basic update stmt with CTE form (CTE bogus)
-- + {create_proc_stmt}: err
-- + {with_update_stmt}: err
-- + {cte_tables}: err
-- + {select_expr_list_con}: select: { _anon: integer notnull }
-- + {select_expr_list_con}: select: { _anon: text notnull }
-- + Error % incompatible types in expression '_anon'
-- +1 Error
create proc with_update_form_bogus_cte()
begin
  with x(id) as (select 1 union all select 'x')
  update bar set name = 'xyzzy' where id in (select * from x);
end;

-- TEST: basic update stmt with CTE form (update bogus)
-- + {create_proc_stmt}: err
-- + {with_update_stmt}: err
-- + Error % table in update statement does not exist 'not_valid_table'
-- +1 Error
create proc with_update_form_bogus_delete()
begin
  with x(id) as (select 1 union all select 2)
  update not_valid_table set name = 'xyzzy' where id in (select * from x);
end;

-- TEST: match a proc that was previously created
-- + DECLARE PROC out_cursor_proc () OUT (A INTEGER NOT NULL, B INTEGER NOT NULL) USING TRANSACTION;
-- + {declare_proc_stmt}: out_cursor_proc: { A: integer notnull, B: integer notnull } dml_proc uses_out
-- - Error
declare proc out_cursor_proc() OUT (A int not null, B int not null) using transaction;

-- TEST: declare the proc first then create it
-- + CREATE PROC decl1 (id INTEGER)
-- + {create_proc_stmt}: ok
-- - Error
create proc decl1(id integer)
begin
 declare i integer;
end;

-- TEST: try to create it again, even though it matches, no dice
-- + {create_proc_stmt}: err
-- + Error % duplicate stored proc name 'decl1'
-- +1 Error
create proc decl1(id integer)
begin
 declare i integer;
end;

-- TEST: try to create a proc that doesn't match the signature
-- the only difference here is that the declaration specified
-- that this was to be a proc that uses the database... we will not do so
-- + CREATE PROC decl2 (id INTEGER)
-- + Error % procedure declarations/definitions do not match 'decl2'
-- + {create_proc_stmt}: err
create proc decl2(id integer)
begin
 declare i integer;
end;

-- TEST: autotest attribute with all attributes
-- + {stmt_and_attr}
-- + {misc_attrs}: ok
-- + {dot}
-- + {name cql}
-- + {name autotest}
-- + {misc_attr_value_list}
-- + {name dummy_table}: ok
-- + {name dummy_insert}: ok
-- + {name dummy_select}: ok
-- + {name dummy_result_set}: ok
-- + {name dummy_test}: ok
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
@attribute(cql:autotest=(dummy_test, dummy_table, dummy_insert, dummy_select, dummy_result_set))
create proc autotest_all_attribute()
begin
  select * from bar;
end;

-- TEST: autotest attribute with dummy_test info on multiple columns
-- + {stmt_and_attr}
-- + {misc_attrs}: ok
-- + {dot}
-- + {name cql}
-- + {name autotest}
-- + {misc_attr_value_list}
-- + {name dummy_table}: ok
-- + {name dummy_test}: ok
-- + {name bar}: ok
-- + {name id}: ok
-- + {int 1}: ok
-- + {uminus}
-- + | {int 2}: ok
-- + {name name}: ok
-- + {strlit 'Nelly'}: ok
-- + {strlit 'Babeth'}: ok
-- + {name foo}: ok
-- + {name id}: ok
-- + {int 777}: ok
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id, name), (1, 'Nelly'), (-2, 'Babeth')), (foo, (id), (777)))))
create proc autotest_dummy_test_with_others_attributes()
begin
  select * from bar;
end;

-- TEST: autotest attribute with dymmy_test info on a single table and column
-- + {stmt_and_attr}
-- + {misc_attrs}: ok
-- + {dot}
-- + {name cql}
-- + {name autotest}
-- + {misc_attr_value_list}
-- + {name dummy_test}: ok
-- + {name bar}: ok
-- + {name id}: ok
-- + {int 1}: ok
-- + {int 2}: ok
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
@attribute(cql:autotest=((dummy_test, (bar, (id), (1), (2)))))
create proc autotest_dummy_test_without_other_attributes()
begin
  select * from bar;
end;

-- TEST: dummy_test info with invalid column value type (value type str is incorrect)
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name one}: err
-- + autotest attribute 'dummy_test' has invalid value type in 'id'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id), (one)))))
create proc autotest_dummy_test_invalid_col_str_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with invalid column value type (value type dbl is incorrect)
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {dbl 0.1}: err
-- + autotest attribute 'dummy_test' has invalid value type in 'id'
-- +1 Error
@attribute(cql:autotest=((dummy_test, (bar, (id), (0.1)))))
create proc autotest_dummy_test_invalid_col_dbl_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with int value for a long column
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- + {misc_attrs}: ok
-- - Error
@attribute(cql:autotest=((dummy_test, (bar, (rate), (1)))))
create proc autotest_dummy_test_long_col_with_int_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with int value for a negative long column
-- + {create_proc_stmt}: select: { id: integer notnull, name: text, rate: longint } dml_proc
-- + {misc_attrs}: ok
-- + {uminus}
-- + | {int 1}
-- - Error
@attribute(cql:autotest=((dummy_test, (bar, (rate), (-1)))))
create proc autotest_dummy_test_neg_long_col_with_int_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with invalid column value type (value type strlit is incorrect)
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {strlit 'bogus'}: err
-- + autotest attribute 'dummy_test' has invalid value type in 'id'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id) , ('bogus')))))
create proc autotest_dummy_test_invalid_col_strlit_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with invalid column value type (value type lng is incorrect)
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {longint 1}: err
-- + autotest attribute 'dummy_test' has invalid value type in 'id'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id), (1L)))))
create proc autotest_dummy_test_invalid_col_lng_value()
begin
  select * from bar;
end;

-- TEST: dummy_test info with column name not nested
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (column name should be nested) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, id, (1), (2)))))
create proc autotest_dummy_test_invalid_col_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info with two column value for one column name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (too many column values) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id), (1, 2)))))
create proc autotest_dummy_test_too_many_value_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info with one column value for 2 column name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (mismatch number of column and values) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id, name), (1)))))
create proc autotest_dummy_test_missing_value_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info missing column value for each column name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (column value should be nested) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id, name)))))
create proc autotest_dummy_test_no_value_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info with column value as column name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {misc_attr_value_list}: err
-- + autotest attribute has incorrect format (table name should be nested) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (1, (id), (1)))))
create proc autotest_bogus_table_name_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info missing column name but has column value
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (column name should be nested) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (1), (1)))))
create proc autotest_bogus_colum_name_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info with column value not nested
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: err
-- + autotest attribute has incorrect format (column value should be nested) in 'dummy_test'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (id), 1))))
create proc autotest_colum_value_incorrect_format()
begin
  select * from bar;
end;

-- TEST: dummy_test info with bogus column name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bar}: ok
-- + {name bogus_col}: err
-- + autotest attribute 'dummy_test' has non existent column 'bogus_col'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bar, (bogus_col), (1), (2)))))
create proc autotest_dummy_test_bogus_col_name()
begin
  select * from bar;
end;

-- TEST: dummy_test info with bogus table name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_test}: err
-- + {name bogus_table}: err
-- + autotest attribute 'dummy_test' has non existent table 'bogus_table'
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_test, (bogus_table, (id), (1), (2)))))
create proc autotest_dummy_test_bogus_table_name()
begin
  select * from bar;
end;

-- TEST: autotest attribute with bogus attribute name
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_bogus}: err
-- + autotest attribute name is not valid 'dummy_bogus'
-- +1 Error
@attribute(cql:autotest=(dummy_bogus))
create proc autotest_dummy_bogus()
begin
  select * from bar;
end;

-- TEST: autotest attribute with bogus attribute name nested
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_bogus}: err
-- + {name dummy_table}: ok
-- + autotest has incorrect format
-- +1 Error
@attribute(cql:autotest=(dummy_table, (dummy_bogus)))
create proc autotest_bogus_nested_attribute()
begin
  select * from bar;
end;

-- TEST: dummy_test info not nested
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name bar}: err
-- + autotest has incorrect format
-- +1 Error
@attribute(cql:autotest=(dummy_test, (bar, (id), (1))))
create proc autotest_dummy_test_not_nested()
begin
  select * from bar;
end;

-- TEST: autotest attribute with dummy_table
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + {name dummy_table}: err
-- + autotest has incorrect format
-- +1 Error
@attribute(cql:autotest=dummy_table)
create proc autotest_incorrect_formatting()
begin
  select * from bar;
end;

-- some declrations for autodrop tests
create temp table table1( id integer);
create temp table table2( id integer);
create table not_a_temp_table( id integer);

-- TEST: autodrop attribute (correct usage)
-- + {stmt_and_attr}
-- + {misc_attrs}: ok
-- + {dot}
-- + {name cql}
-- + {name autodrop}
-- +  {name table1}: ok
-- + {name table2}: ok
-- + {create_proc_stmt}: select: { id: integer } dml_proc
-- + {name autodropper}: select: { id: integer } dml_proc
@attribute(cql:autodrop=(table1, table2))
create proc autodropper()
begin
  select * from table1;
end;

-- TEST: autodrop attribute: name is not an object
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + autodrop temp table does not exist 'not_an_object'
-- +1 Error
@attribute(cql:autodrop=(not_an_object))
create proc autodropper_not_an_objecte()
begin
  select * from table1;
end;

-- TEST: autodrop attribute: name is a view
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + Error % autodrop target is not a table 'ViewShape'
-- +1 Error
@attribute(cql:autodrop=(ViewShape))
create proc autodropper_dropping_view()
begin
  select * from table1;
end;

-- TEST: autodrop attribute: name is not a temp table
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + Error % autodrop target must be a temporary table 'not_a_temp_table'
-- +1 Error
@attribute(cql:autodrop=(not_a_temp_table))
create proc autodropper_not_temp_table()
begin
  select * from table1;
end;

-- TEST: autodrop attribute: proc doesn't select anything
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + Error % autodrop annotation can only go on a procedure that returns a result set 'autodrop_not_really_a_result_set_proc'
-- +1 Error
@attribute(cql:autodrop=(table1, table2))
create proc autodrop_not_really_a_result_set_proc()
begin
  declare i integer;
end;

-- TEST: autodrop attribute: proc doesn't use the database
-- + {create_proc_stmt}: err
-- + {misc_attrs}: err
-- + Error % autodrop annotation can only go on a procedure that uses the database 'autodrop_no_db'
-- +1 Error
@attribute(cql:autodrop=(table1, table2))
create procedure autodrop_no_db()
begin
  declare C cursor like select 1 id;
  fetch c (id) from values(1);
  out c;
end;

-- TEST: table to test referenceable (primary key, unique key) column
-- {create_table_stmt}: referenceable: { a: integer notnull primary_key, b: real unique_key, c: text unique_index_key, d: text }
-- - Error
create table referenceable (
  a int primary key,
  b real unique,
  c text,
  d text,
  e long int
);

-- TEST: table to test referenceable group of columns
-- {create_table_stmt}: referenceable: { a: integer notnull, b: real }
-- - Error
create table referenceable_2 (
  a int,
  b real,
  primary key (a, b)
);

-- TEST: index to test referenceable (unique index key) column
-- - Error
create unique index referenceable_index on referenceable(c, d);

-- TEST: test foreign key on a primary key
-- +1 {create_table_stmt}: reference_pk: { id: integer foreign_key }
-- +1 {fk_def}: ok
-- - Error
create table reference_pk(
  id int,
  foreign key (id) references referenceable(a)
);

-- TEST: test foreign key on a group of primary key
-- +1 {create_table_stmt}: reference_2_pk: { id: integer foreign_key, size: real foreign_key }
-- +1 {fk_def}: ok
-- - Error
create table reference_2_pk(
  id int,
  size real,
  foreign key (id, size) references referenceable_2(a, b)
);

-- TEST: test foreign key on a group of primary key in the wrong order
-- +1 {create_table_stmt}: reference_2_wrong_order_pk: { id: integer foreign_key, size: real foreign_key }
-- +1 {fk_def}: ok
-- - Error
create table reference_2_wrong_order_pk(
  id int,
  size real,
  foreign key (size, id) references referenceable_2(b, a)
);

-- TEST: test foreign key on a unique key
-- +1 {create_table_stmt}: reference_uk: { id: real foreign_key }
-- +1 {fk_def}: ok
-- - Error
create table reference_uk(
  id real,
  foreign key (id) references referenceable(b)
);

-- TEST: test foreign key on a mixed of primary and unique key
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- +1 Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'referenceable'
-- +1 Error
create table reference_pk_and_uk(
  id1 int,
  id2 real,
  foreign key (id1, id2) references referenceable(a, b)
);

-- TEST: test foreign key on a unique key
-- +1 {create_table_stmt}: referenceable_unique_index: { id: text foreign_key, label: text foreign_key }
-- +1 {fk_def}: ok
-- - Error
create table referenceable_unique_index(
  id text,
  label text,
  foreign key (id, label) references referenceable(c, d)
);

-- TEST: test foreign key on a mixed of a primary and unique index
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- +1 Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'referenceable'
-- +1 Error
create table reference_pk_and_unique_index(
  id1 int,
  id2 text,
  foreign key (id1, id2) references referenceable(a, c)
);

-- TEST: test foreign key on a mixed of a unique key and unique index
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- +1 Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'referenceable'
-- +1 Error
create table reference_uk_and_unique_index(
  id1 real,
  id2 text,
  id3 text,
  foreign key (id1, id2, id3) references referenceable(b, c, d)
);

-- TEST: test foreign key on a single non referenceable column
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- + Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'referenceable'
-- +1 Error
create table reference_not_referenceable_column(
  id long int primary key,
  foreign key (id) references referenceable(e)
);

-- TEST: test foreign key on multiple non referenceable columns
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- + Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table
-- +1 Error
create table reference_not_referenceable_columns(
  id1 text primary key,
  id2 text,
  id3 text,
  foreign key (id1, id2, id3) references referenceable(c, d, e)
);

-- TEST: test foreign key on a subset of unique index
-- +1 {create_table_stmt}: err
-- +1 {fk_def}: err
-- + Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table
-- +1 Error
create table reference_not_referenceable_column(
  id text,
  foreign key (id) references referenceable(c)
);

-- TEST: validate enforcement parse and analysis (fk on update)
-- + @ENFORCE_STRICT FOREIGN KEY ON UPDATE
-- + {enforce_strict_stmt}: ok
-- + {int 1}
@enforce_strict foreign key on update;

-- TEST: validate enforcement parse and analysis (fk on delete)
-- + @ENFORCE_STRICT FOREIGN KEY ON DELETE;
-- + {enforce_strict_stmt}: ok
-- + {int 2}
@enforce_strict foreign key on delete;

-- TEST: validate enforcement parse and analysis (fk on update)
-- + @ENFORCE_NORMAL FOREIGN KEY ON UPDATE
-- + {enforce_normal_stmt}: ok
-- + {int 1}
@enforce_normal foreign key on update;

-- TEST: validate enforcement parse and analysis (fk on delete)
-- + @ENFORCE_NORMAL FOREIGN KEY ON DELETE;
-- + {enforce_normal_stmt}: ok
-- + {int 2}
@enforce_normal foreign key on delete;

-- switch back to strict mode for the validation tests
@enforce_strict foreign key on update;
@enforce_strict foreign key on delete;

-- TEST: strict validation ok
-- + id INTEGER REFERENCES foo (id) ON UPDATE CASCADE ON DELETE CASCADE
-- + {create_table_stmt}: fk_strict_ok: { id: integer foreign_key }
-- + {col_attrs_fk}: ok
-- - Error
create table fk_strict_ok (
  id integer REFERENCES foo(id) ON DELETE CASCADE ON UPDATE CASCADE
);

-- TEST: strict failure ON UPDATE missing
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + {col_attrs_fk}: err
-- + Error % strict FK validation requires that some ON UPDATE option be selected for every foreign key
-- +1 Error
create table fk_strict_failure_update(
  id integer REFERENCES foo(id)
);

-- TEST: strict failure ON DELETE missing
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + {col_attrs_fk}: err
-- + id INTEGER REFERENCES foo (id) ON UPDATE NO ACTION
-- + Error % strict FK validation requires that some ON DELETE option be selected for every foreign key
-- +1 Error
CREATE TABLE fk_strict_failure_delete(
  id INTEGER REFERENCES foo (id) ON UPDATE NO ACTION
);

-- TEST: strict failure ON DELETE missing (loose FK)
-- + {create_table_stmt}: err
-- + {fk_def}: err
-- + Error % strict FK validation requires that some ON DELETE option be selected for every foreign key
-- +1 Error
CREATE TABLE fk_strict_failure_delete_loose(
  id INTEGER,
  FOREIGN KEY (id) REFERENCES foo(id) ON UPDATE NO ACTION
);

-- TEST: strict failure ON UPDATE missing (loose FK)
-- + {create_table_stmt}: err
-- + {fk_def}: err
-- + Error % strict FK validation requires that some ON UPDATE option be selected for every foreign key
-- +1 Error
CREATE TABLE fk_strict_failure_update_loose(
  id INTEGER,
  FOREIGN KEY (id) REFERENCES foo(id)
);

-- TEST: strict success with loose fk
-- + {create_table_stmt}: fk_strict_success_loose: { id: integer foreign_key }
-- + {fk_def}: ok
-- - Error
CREATE TABLE fk_strict_success_loose(
  id INTEGER,
  FOREIGN KEY (id) REFERENCES foo(id) ON DELETE NO ACTION ON UPDATE CASCADE
);

-- TEST: create proc with an invalid column name in the  identity attribute
-- + Error % procedure identity column does not exist in result set 'col3'
-- +1 Error
@attribute(cql:identity=(col1, col3))
create proc invalid_identity()
begin
  select 1 as col1, 2 as col2, 3 as data;
end;

-- TEST: create proc with an identity attribute but it has no result
-- + Error % identity annotation can only go on a procedure that returns a result set 'no_result_set_identity'
-- +1 Error
@attribute(cql:identity=(col1, col3))
create proc no_result_set_identity()
begin
  declare x integer;  /* no op */
end;

-- TEST: base_fragment attribute (correct usage)
@attribute(cql:base_fragment=core)
create proc test_base_fragment(id_ integer not null)
begin
  with
    core(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from core;
end;

-- TEST: extension_fragment attribute (correct usage)
-- + {create_proc_stmt}: select: { x: integer notnull, y: text, z: longint } dml_proc
-- + {select_stmt}: select: { x: integer notnull, y: text, z: longint }
-- + {select_core_list}: union_all: { x: integer notnull, y: text, z: longint }
-- + {select_core_compound}
-- - Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_union(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    plugin_two(*) as (
    select * from core
    union all
    select 3 x, "y" y, 5L z
  )
  select * from plugin_two;
end;

-- TEST: a second extension_fragment attribute (correct usage) (verify you can add several out union in a row)
-- + {create_proc_stmt}: select: { x: integer notnull, y: text, z: longint } dml_proc
-- + {select_stmt}: select: { x: integer notnull, y: text, z: longint }
-- + {select_core_list}: union_all: { x: integer notnull, y: text, z: longint }
-- + {select_core_compound}
-- - Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_union_two(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    plugin_two_a(*) as (
    select * from core
    union all
    select 3 x, "y" y, 5L z
  )
  select * from plugin_two_a;
end;

-- TEST: base_fragment attribute (correct usage)
@attribute(cql:base_fragment=another_core)
create proc test_base_fragment_two(id_ integer not null)
begin
  with
    another_core(x,y) as (select T1.id, name from foo AS T1 inner join bar AS T2 ON T1.id = T2.id where T1.id = id_)
  select * from another_core;
end;

-- TEST: base_fragment attribute (duplicate creation)
-- + Error % fragment name conflicts with existing base fragment 'core'
-- +1 Error
@attribute(cql:base_fragment=core)
create proc test_duplicate_base_fragment(id_ integer not null)
begin
  with
    core(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from core;
end;

-- TEST: base_fragment attribute (erroneous usage)
-- + Error % fragment must end with exactly 'SELECT * FROM core_one'
-- +1 Error
@attribute(cql:base_fragment=core_one)
create proc bad_base_fragment_one(id_ integer not null)
begin
  with
    core_one(id,name,rate) as (select id,name,rate from bar where id = id_)
  select id from core_one;
end;

-- TEST: base_fragment attribute (erroneous usage)
-- + Error % fragment must end with exactly 'SELECT * FROM core_two'
-- +1 Error
@attribute(cql:base_fragment=core_two)
create proc bad_base_fragment_two(id_ integer not null)
begin
  with
    core_two(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from bar;
end;

-- TEST: base_fragment attribute (erroneous usage)
-- + Error % base fragment must include a single CTE named same as the fragment 'bar'
-- +1 Error
@attribute(cql:base_fragment=bar)
create proc bad_base_fragment_three(id_ integer not null)
begin
  with
    core_three(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from bar;
end;

-- TEST: base_fragment attribute (erroneous usage)
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
-- +1 Error
@attribute(cql:base_fragment=core_four)
create proc bad_base_fragment_four(id_ integer not null)
begin
  select id,name,rate from bar where id = id_;
end;

-- TEST: create a plugin table that is going to add a column to the base fragment
-- + {create_table_stmt}: plugin_table: { id: integer notnull primary_key, name: text, flag: bool }
-- - Error
create table plugin_table(
  id integer primary key,
  name text,
  flag BOOL
);

-- TEST: extension_fragment attribute (incorrect usage, not UNION ALL everywhere)
-- + {create_proc_stmt}: err
-- + {with_select_stmt}: err
-- + Error % all the compound operators in this CTE must be UNION ALL 'not_union_all'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_not_union_all(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    not_union_all(*) as (
    select * from core
    union all
    select 3 x, "y" y, 5L z
    except
    select 3 x, "y" y, 5L z
  )
  select * from not_union_all;
end;

-- TEST: extension_fragment attribute (correct usage)
-- - Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_one(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_one;
end;

-- TEST: extension_fragment attribute (incorrect usage, out of order)
-- + {create_proc_stmt}: err
-- + {with_select_stmt}: err
-- + Error % all extension fragments that use UNION ALL must come before those that use LEFT OUTER JOIN 'out_of_order'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_union_out_of_order(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    out_of_order(*) as (
    select * from core
    union all
    select 3 x, "y" y, 5L z
  )
  select * from out_of_order;
end;

-- TEST: extension_fragment attribute (correct usage)
-- + Error % fragment parameters must be exactly '(id_ INTEGER NOT NULL)'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_bad_args(id_ integer)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_one(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_one;
end;

-- TEST: extension_fragment attribute (erroneous usage)
--  + Error % fragment name is not a previously declared base fragmen
-- +1 Error
@attribute(cql:extension_fragment=wrong_core)
create proc test_bad_extension_fragment_one(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_one(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_wrong_one;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment must have the CTE named same as the base fragment
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_two(id_ integer not null)
begin
  with
    core_not_exist(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_two(x,y,z,a) as (
    select core_not_exist.*, plugin_table.flag from core_not_exist
    left outer join plugin_table on plugin_table.name = core_not_exist.y AND plugin_table.id = id_)
  select * from plugin_wrong_two;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension fragment must add exactly one CTE; found extra named 'plugin_wrong_three'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_three(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_extra(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_),
    plugin_wrong_three(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_wrong_three;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we check for the case where you try to do something other than a join in the CTE
-- + Error % extension fragment CTE must be a simple left outer join from 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bogus_extension_shape(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_shape(x,y,z,a) as (select core.*, 1 a from core)
  select * from bogus_shape;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that we detect the case when you try to join from something other than a table
-- + Error % extension fragment CTE must be a simple left outer join from 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bogus_extension_data_source(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_not_a_table(x,y,z,a) as (select core.*, junk.a from (select * from core) core inner join (select 1 a) junk)
  select * from bogus_not_a_table;
end;

-- make a bogus thing with the same shape as core but a different name
-- - Error
create view core2 as select 1 x, nullable("a") y, nullable(3L) z;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that we detect the case when you try to join from something other than a table
-- + Error % extension fragment CTE must be a simple left outer join from 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bogus_extension_table_name(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_wrong_table(x,y,z,a) as (select core.*, junk.a from core2 core left outer join (select 1 a) junk)
  select * from bogus_wrong_table;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment must use base CTE column list same as from the base fragment 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_four(id_ integer not null)
begin
  with
    core(x,y,z,a) as (select 1,nullable("a"),nullable(3L),4),
    plugin_wrong_four(x,y,z,a,flag) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_wrong_four;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that there are no top level restrictions (like 'where')
-- + Error % extension fragment CTE must have a FROM clause and no other top level clauses 'bogus_extra_clauses'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_where_in_cte(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_extra_clauses(x,y,z,a) as (
       select core.*, junk.a from core
       left outer join (select 1 a) junk
       where core.x = 1)
  select * from bogus_extra_clauses;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that there are no top level restrictions (like 'limit' in the union all case)
-- + Error % extension fragment CTE must have not have ORDER BY or LIMIT clauses 'bogus_extra_clauses2'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_limit_in_union_all(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_extra_clauses2(*) as (
       select * from core
       union all
       select * from core
       limit 1)
  select * from bogus_extra_clauses2;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that there are no post restrictions (like 'limit')
-- + Error % extension fragment CTE must have a FROM clause and no other top level clauses 'bogus_extra_clauses'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_limit_in_cte(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_extra_clauses(x,y,z,a) as (
       select core.*, junk.a from core
       left outer join (select 1 a) junk
       limit 3)
  select * from bogus_extra_clauses;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- Here we're checking that no FROM clause is illegal
-- + Error % extension fragment CTE must select T.* from base CTE 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_missing_from_in_cte(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    bogus_missing_from(x,y,z,a) as (select 1, nullable("a"),nullable(3L), 1a)
  select * from bogus_missing_from;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- here the join is "inner" rather than left outer
-- + Error % extension fragment CTE must be a simple left outer join from 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_five(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_five(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    inner join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from plugin_wrong_five;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % fragment must end with exactly 'SELECT * FROM plugin_wrong_six'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_six(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_six(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select plugin_wrong_six.x from plugin_wrong_six;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % fragment must end with exactly 'SELECT * FROM plugin_wrong_seven'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_seven(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_seven(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.name = core.y AND plugin_table.id = id_)
  select * from core;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension fragment CTE must select T.* from base CTE 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_eight(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_eight(x,y,z,a) as (
    select bar.*, plugin_table.flag from bar
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_eight;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension fragment CTE must select T.* from base CTE 'core'
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_nine(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_nine(x,y) as (
    select core.y, plugin_table.flag from core
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_nine;
end;

-- TEST: create plugin_wrong_nine extension, using up the name plugin_wrong_nine
-- + {create_proc_stmt}: select: { x: integer notnull, y: text, z: longint, a: bool } dml_proc
-- - Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_nine_ok(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_nine(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_nine;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension fragment name conflicts with existing fragment 'plugin_wrong_nine'
-- + {name test_bad_extension_fragment_ten}: err
-- + {cte_tables}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_ten(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    plugin_wrong_nine(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_nine;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_eleven(id_ integer not null)
begin
  select * from plugin_table;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment must add stub for base CTE with same types from base fragment (expected longint; found integer) 'rate'
-- + {name test_bad_extension_fragment_with_wrong_base_sem_type}: err
-- + {select_expr_list_con}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_with_wrong_base_sem_type(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3)),
    plugin_wrong_eleven(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_eleven;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment stub for base CTE column must be exact type match (including nullability) (expected longint; found longint notnull) 'rate'
-- + {name test_bad_extension_fragment_with_wrong_base_nullability}: err
-- + {select_expr_list_con}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_with_wrong_base_nullability(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),3L),
    plugin_wrong_twelve(x,y,z,a) as (
    select core.*, plugin_table.flag from core
    left outer join plugin_table on plugin_table.id = id_)
  select * from plugin_wrong_twelve;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % if multiple selects, all column names must be identical so they have unambiguous names 'name'
-- + {create_proc_stmt}: err
-- + {with_select_stmt}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_union_one(id_ integer)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    test_bad_extension_fragment_union_one(x,y,z,name) as (
    select core.*, CAST(NULL as TEXT) as name2 from core
    union all
    select core.*, plugin_table.name from core inner join plugin_table on plugin_table.name = "test")
  select * from test_bad_extension_fragment_union_one;
end;

-- TEST: extension_fragment attribute (erroneous usage)
-- + Error % if multiple selects, all must have the same column count
-- + {create_proc_stmt}: err
-- + {with_select_stmt}: err
-- + {cte_tables}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_union_two(id_ integer)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    test_bad_extension_fragment_union_two(x,y,z,name) as (
    select core.*, CAST(NULL as TEXT) as name from core
    union all
    select core.* from core inner join plugin_table on plugin_table.name = "test")
  select * from test_bad_extension_fragment_union_two;
end;

-- TEST: extension_fragment attribute (incorrect usage)
-- + Error % fragment must start with exactly 'SELECT * FROM core'
-- + {create_proc_stmt}: err
-- + {with_select_stmt}: err
-- +1 Error
@attribute(cql:extension_fragment=core)
create proc test_extension_fragment_union_three(id_ integer not null)
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    test_bad_extension_fragment_union_three(*) as (
    select * from bar
    union all
    select * from bar)
  select * from test_bad_extension_fragment_union_three;
end;

-- Setting up a fragment for the assembly test (only trivial validation here)
-- - Error
@attribute(cql:base_fragment=assembly_core)
create proc test_assembly_base_fragment(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from assembly_core;
end;

-- Setting up a fragment for the assembly test (only trivial validation here)
-- - Error
@attribute(cql:extension_fragment=assembly_core)
create proc test_assembly_extension_fragment_one(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    assembly_one(x,y,z) as (
    select * from assembly_core
    union all
    select 3 x, "y" y, 5L z)
  select * from assembly_one;
end;

-- Setting up a fragment for the assembly test (only trivial validation here)
-- - Error
@attribute(cql:extension_fragment=assembly_core)
create proc test_assembly_extension_fragment_two(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    assembly_two(x,y,z,a,b,c) as (
    select assembly_core.*, plugin_table.flag, min(plugin_table.id, 10L), min(plugin_table.id, 10.05)
    from assembly_core
    left outer join plugin_table on plugin_table.name = assembly_core.y AND plugin_table.id = id_)
  select * from assembly_two;
end;

-- Setting up a fragment for the assembly test (only trivial validation here)
-- - Error
@attribute(cql:extension_fragment=assembly_core)
create proc test_assembly_extension_fragment_three(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    assembly_three(*) as (
    select assembly_core.*, plugin_table.id d
     from assembly_core
     left outer join plugin_table on plugin_table.id = id_)
  select * from assembly_three;
end;

-- TEST: assembly_fragment attribute (correct usage)
-- - Error
-- + {misc_attrs}: ok
-- + {name assembly_fragment}
-- + {create_proc_stmt}: select: { x: integer notnull, y: text, z: longint, a: bool, b: longint, c: real, d: integer } dml_proc
-- + {select_stmt}: select: { x: integer notnull, y: text, z: longint, a: bool, b: longint, c: real, d: integer }
-- + {cte_table}: assembly_core: { x: integer notnull, y: text, z: longint }
-- + {cte_table}: assembly_one: { x: integer notnull, y: text, z: longint }
-- + {cte_table}: assembly_two: { x: integer notnull, y: text, z: longint, a: bool, b: longint, c: real }
-- + {cte_table}: assembly_three: { x: integer notnull, y: text, z: longint, a: bool, b: longint, c: real, d: integer }
@attribute(cql:assembly_fragment=assembly_core)
create proc assembly_core(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select 1,nullable("a"),nullable(3L)) -- this is stub for the core
  select * from assembly_core;
end;

-- TEST: base fragment with atypical body
-- + create_proc_stmt}: err
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
@attribute(cql:base_fragment=for_bad2)
create proc test_assembly_base_for_bad2(id_ integer not null)
begin
  declare foo integer;
  with
    for_bad(x) as (select 1)
  select * from for_bad;
end;

-- setup for the assembly fragment
-- - Error
@attribute(cql:base_fragment=for_bad)
create proc test_assembly_base_for_bad(id_ integer not null)
begin
  with
    for_bad(x) as (select 1)
  select * from for_bad;
end;

-- TEST: extension fragment with bogus content
-- + create_proc_stmt}: err
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
@attribute(cql:extension_fragment=for_bad)
create proc test_bad_ext(id_ integer not null)
begin
  declare foo integer;
  with
    for_bad(x) as (select 1),
    plugin_one(x,a) as (select 1, 2)
  select * from plugin_one;
end;

-- TEST: assembly fragment with atypical body
-- + {create_proc_stmt}: err
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
@attribute(cql:assembly_fragment=for_bad)
create proc test_assembly_fragment2(id_ integer not null)
begin
  declare foo integer;
  with
    for_bad(x) as (select 1)
  select * from for_bad;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % duplicate assembly fragments of base fragment 'assembly_core'
-- + {misc_attrs}: err
-- + {name duplicate_assembly_fragment}: err
-- +1 Error
@attribute(cql:assembly_fragment=assembly_core)
create proc duplicate_assembly_fragment(id_ integer not null)
begin
  with
    assembly_core(x,y,z) as (select 1,2,nullable(3L)) -- this is stub for the core
  select * from assembly_core;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % assembly fragment can only have one CTE 'another_core'
-- + {stmt_list}: err
-- + {cte_tables}: err
-- +1 Error
@attribute(cql:assembly_fragment=another_core)
create proc bad_assembly_fragment_one(id_ integer not null)
begin
  with
    another_core(x,y) as (select 1,nullable("a")), -- this is stub for the core
    another_cte(x,y) as (
    select another_core.* from another_core
    )
  select * from another_cte;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % fragment name is not a previously declared base fragment 'wrong_core'
-- + {misc_attrs}: err
-- + {name bad_assembly_fragment_two}: err
-- +1 Error
@attribute(cql:assembly_fragment=wrong_core)
create proc bad_assembly_fragment_two(id_ integer not null)
begin
  with
    wrong_core(x,y,z) as (select 1,nullable("a"),nullable(3L)) -- this is stub for the core
  select * from wrong_core;
end;

-- - Error
@attribute(cql:base_fragment=test_bad_assembly_base_fragment)
create proc test_bad_assembly_base_fragment(id_ integer not null)
begin
  with
    test_bad_assembly_base_fragment(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from test_bad_assembly_base_fragment;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % fragments can only have one statement in the statement list and it must be a WITH..SELECT
-- +1 Error
@attribute(cql:assembly_fragment=test_bad_assembly_base_fragment)
create proc bad_assembly_fragment_three(id_ integer not null)
begin
  select id,name,rate from bar where id = id_;
end;

-- - Error
@attribute(cql:base_fragment=assembly_core_bad_one)
create proc test_bad_assembly_base_fragment_one(id_ integer not null)
begin
  with
    assembly_core_bad_one(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from assembly_core_bad_one;
end;

-- TEST: a simple ext fragment with the same name as the next, this is ok in isolation
-- - Error
@attribute(cql:extension_fragment=assembly_core_bad_one)
create proc test_bad_assembly_extension_fragment_one(id_ integer not null)
begin
  with
    assembly_core_bad_one(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    assembly_bad_one(x,y,z,a) as (
    select assembly_core_bad_one.*, plugin_table.flag from assembly_core_bad_one
    left outer join plugin_table on plugin_table.name = assembly_core_bad_one.y AND plugin_table.id = id_)
  select * from assembly_bad_one;
end;

-- TEST: a simple ext fragment with the same name as the previous, this is ok in isolation
-- - Error
@attribute(cql:extension_fragment=assembly_core_bad_one)
create proc test_bad_assembly_extension_fragment_two(id_ integer not null)
begin
  with
    assembly_core_bad_one(x,y,z) as (select 1,nullable("a"),nullable(3L)), -- this is stub for the core
    assembly_bad_two(x,y,z,a) as (
    select assembly_core_bad_one.*, plugin_table.flag from assembly_core_bad_one
    left outer join plugin_table on plugin_table.name = assembly_core_bad_one.y AND plugin_table.id = id_)
  select * from assembly_bad_two;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % extension fragments of same base fragment share the same cte column 'a'
-- + {name assembly_core_bad_one}: err
-- +1 Error
@attribute(cql:assembly_fragment=assembly_core_bad_one)
create proc assembly_core_bad_one(id_ integer not null)
begin
  with
    assembly_core_bad_one(x,y,z) as (select 1,nullable("a"),nullable(3L)) -- this is stub for the core
  select * from assembly_core_bad_one;
end;

-- Set up base fragment for bad_assembly_fragment_with_wrong_base_sem_type
-- - Error
@attribute(cql:base_fragment=assembly_core_bad_two)
create proc create_bad_assembly_base_fragment_two(id_ integer not null)
begin
  with
    assembly_core_bad_two(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from assembly_core_bad_two;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment stub for base CTE column must be exact type match (including nullability) (expected longint; found longint notnull) 'rate'
-- + {name test_bad_assembly_fragment_with_wrong_base_sem_type}: err
-- + {select_expr_list_con}: err
-- +1 Error
@attribute(cql:assembly_fragment=assembly_core_bad_two)
create proc test_bad_assembly_fragment_with_wrong_base_sem_type(id_ integer not null)
begin
  with
    assembly_core_bad_two(x,y,z) as (select 1,nullable("a"),3L)
  select * from assembly_core_bad_two;
end;

-- TEST: Set up base fragment for bad_assembly_fragment_with_wrong_base_nullability
-- - Error
@attribute(cql:base_fragment=assembly_core_bad_three)
create proc create_bad_assembly_base_fragment_three(id_ integer not null)
begin
  with
    assembly_core_bad_three(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from assembly_core_bad_three;
end;

-- TEST: assembly_fragment attribute (erroneous usage)
-- + Error % extension/assembly fragment must add stub for base CTE with same types from base fragment (expected text; found integer) 'name'
-- + {name test_bad_assembly_fragment_with_wrong_base_nullability}: err
-- + {select_expr_list_con}: err
-- +1 Error
@attribute(cql:assembly_fragment=assembly_core_bad_three)
create proc test_bad_assembly_fragment_with_wrong_base_nullability(id_ integer not null)
begin
  with
    assembly_core_bad_three(x,y,z) as (select 1,nullable(2),nullable(3L))
  select * from assembly_core_bad_three;
end;

-- setup a new base fragment
-- - Error
@attribute(cql:base_fragment=assembly_core_simple)
create proc base_with_args(idx_ integer not null)
begin
  with
    assembly_core_simple(x) as (select 1 x)
  select * from assembly_core_simple;
end;

-- Set up an assembly fragment with the wrong parameters
-- + Error % fragment parameters must be exactly '(idx_ INTEGER NOT NULL)'
-- +1 Error
@attribute(cql:assembly_fragment=assembly_core_simple)
create proc assembly_core_simple(id_ integer not null)
begin
  with
    assembly_core_simple(x) as (select 1 x)
  select * from assembly_core_simple;
end;

-- TEST: declare a valid root region
-- + {declare_schema_region_stmt}: root_region: region
-- + {name root_region}
-- - Error
@declare_schema_region root_region;

-- TEST: declare a valid region with dependencies
-- + {declare_schema_region_stmt}: dep_region: region
-- + {name dep_region}
-- + {name root_region}
-- - Error
@declare_schema_region dep_region using root_region;

-- TEST: try to redefine a region
-- + {declare_schema_region_stmt}: err
-- + Error % schema region already defined 'root_region'
-- +1 Error
@declare_schema_region root_region;

-- TEST: try to use a region that doesn't exist
-- + {declare_schema_region_stmt}: err
-- + Error % unknown schema region 'unknown_region'
-- +1 Error
@declare_schema_region root_region using unknown_region;

-- TEST: try to use the same region twice
-- + {declare_schema_region_stmt}: err
-- + Error % duplicate name in list 'root_region'
-- +1 Error
@declare_schema_region root_region using root_region, root_region;

-- TEST: enter a schema region
-- + {begin_schema_region_stmt}: ok
-- + | {name root_region}
-- - Error
@begin_schema_region root_region;

-- TEST: enter a schema region while there is already one active
-- + {begin_schema_region_stmt}: err
-- + Error % schema regions do not nest; end the current region before starting a new one
-- +1 Error
@begin_schema_region root_region;

-- TEST: exit a schema region
-- + {end_schema_region_stmt}: ok
-- - Error
@end_schema_region;

-- add some more regions to create a diamond shape (two ways to get to root)
@declare_schema_region dep2_region USING root_region;
@declare_schema_region diamond_region USING dep_region, dep2_region;

-- TEST: exit a schema region when there is no region active
-- + {end_schema_region_stmt}: err
-- + Error % you must begin a schema region before you can end one
-- +1 Error
@end_schema_region;

-- TEST: try to enter a schema region that is not known
-- + {begin_schema_region_stmt}: err
-- + Error % unknown schema region 'what_is_this_region'
-- +1 Error
@begin_schema_region what_is_this_region;

-- TEST: try to use schema region declaration inside of a procedure
-- + {create_proc_stmt}: err
-- + {declare_schema_region_stmt}: err
-- + Error % schema region directives may not appear inside of a procedure
-- +1 Error
create proc decl_region_in_proc()
begin
  @declare_schema_region fooey;
end;

-- TEST: try to use begin schema region inside of a procedure
-- + {create_proc_stmt}: err
-- + {begin_schema_region_stmt}: err
-- + Error % schema region directives may not appear inside of a procedure
-- +1 Error
create proc begin_region_in_proc()
begin
  @begin_schema_region fooey;
end;

-- TEST: try to use end schema region inside of a procedure
-- + {create_proc_stmt}: err
-- + {end_schema_region_stmt}: err
-- + Error % schema region directives may not appear inside of a procedure
-- +1 Error
create proc end_region_in_proc()
begin
  @end_schema_region;
end;

-- TEST: division of reals is ok (promotes to real)
-- + {assign}: my_real: real variable
-- + {div}: real notnull
-- - Error
set my_real := 1.3 / 2;

-- TEST: modulus of reals is NOT ok (this makes no sense)
-- + {mod}: err
-- + Error % operands must be an integer type, not real '%'
-- +1 Error
set X := 1.3 % 2;

-- TEST: make sure || aborts if one of the args is already an error
-- + {select_stmt}: err
-- + {concat}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
select (NOT 'x') || 'plugh';

@begin_schema_region root_region;
create table a_table_in_root_region(id integer);
create trigger a_trigger_in_root_region
  before delete on a_table_in_root_region
  begin
    delete from a_table_in_root_region where id > 3;
  end;
create index a_index_in_root_region on a_table_in_root_region(id);
@end_schema_region;

@begin_schema_region dep_region;
create table a_table_in_dep_region(id integer);

-- TEST: create a legal view using tables from two regions
-- + {create_view_stmt}: a_view_in_dep_region: { id1: integer, id2: integer }
-- - Error
create view a_view_in_dep_region as
  select T1.id as id1, T2.id as id2
  from a_table_in_root_region T1
  inner join a_table_in_dep_region T2
  using(id);

-- TEST: try to drop a non-region trigger from dep_region
-- + {drop_trigger_stmt}: err
-- + {name trigger2}
-- + Error % trigger in drop statement was not declared (while in schema region 'dep_region', accessing an object that isn't in a region is invalid) 'trigger2'
-- +1 Error
drop trigger trigger2;

-- TEST: try to drop a non-region view from dep_region
-- + {drop_view_stmt}: err
-- + Error % view in drop statement does not exist (while in schema region 'dep_region', accessing an object that isn't in a region is invalid) 'MyView'
-- +1 Error
drop view MyView;

-- TEST: try to drop a non-region table from dep_region
-- + {drop_table_stmt}: err
-- + Error % table in drop statement does not exist (while in schema region 'dep_region', accessing an object that isn't in a region is invalid) 'foo'
-- +1 Error
drop table foo;

-- TEST: try to drop a non-region index from dep_region
-- + {drop_index_stmt}: err
-- + Error % index in drop statement was not declared (while in schema region 'dep_region', accessing an object that isn't in a region is invalid) 'index_1'
-- +1 Error
drop index index_1;

-- TEST: create a table like non-region table from dep_region
-- + {create_table_stmt}: a_table_like_table_in_dep_region: { id: integer notnull }
-- - Error
create table a_table_like_table_in_dep_region (like foo);

-- TEST: create a table like view in dep_region from dep_region
-- + {create_table_stmt}: a_table_like_table_in_dep_region_2: { id1: integer, id2: integer }
-- - Error
create table a_table_like_table_in_dep_region_2 (like a_view_in_dep_region);

-- TEST: create a table like a non-region view from dep_region
-- + {create_table_stmt}: a_table_like_view_in_dep_region: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- - Error
create table a_table_like_view_in_dep_region (like MyView);

-- TEST: create a table like a non-region proc from dep_region
-- + {create_table_stmt}: a_table_like_proc_in_dep_region: { id: integer notnull, name: text, rate: longint }
-- - Error
create table a_table_like_proc_in_dep_region (like with_result_set);

@end_schema_region;

-- entering a different region now, it partly overlaps
@begin_schema_region dep2_region;

-- TEST: create a legal view using tables from root region
-- + {create_view_stmt}: ok_view_in_dep2_region: { id: integer }
-- - Error
create view ok_view_in_dep2_region as select * from a_table_in_root_region;

-- TEST: try to access objects in dep_region
-- + {create_view_stmt}: err
-- + Error % table/view not defined (object is in schema region 'dep_region' not accessible from region 'dep2_region') 'a_table_in_dep_region'
-- +1 Error
create view bogus_view_in_dep2_region as
  select T1.id as id1, T2.id as id2
  from a_table_in_root_region T1
  inner join a_table_in_dep_region T2
  using(id);

-- TEST: try to use a non-region object while in a region
-- + {create_view_stmt}: err
-- + Error % table/view not defined (while in schema region 'dep2_region', accessing an object that isn't in a region is invalid) 'bar'
-- +1 Error
create view bogus_due_to_non_region_object as select * from bar;

@end_schema_region;

-- TEST: enter a schema region that has diamond shaped dependencies
-- + {begin_schema_region_stmt}: ok
-- + {name diamond_region}
-- - Error
@begin_schema_region diamond_region;

-- TEST: drop a dep_region table from diamond_region
-- + {drop_table_stmt}: ok
-- - Error
drop table a_table_like_proc_in_dep_region;

-- TEST: drop a root_region table from diamond_region
-- + {drop_table_stmt}: ok
-- - Error
drop table a_table_in_root_region;

-- TEST: drop a dep_region view from diamond_region
-- + {drop_view_stmt}: ok
-- - Error
drop view a_view_in_dep_region;

-- TEST: drop a root_region trigger from diamond_region
-- + {drop_trigger_stmt}: ok
-- - Error
drop trigger a_trigger_in_root_region;

-- TEST: drop a root_region index from diamond_region
-- + {drop_index_stmt}: ok
-- - Error
drop index a_index_in_root_region;

-- TEST: creating a table for use later, we'll try to create an index on the wrong group
-- - Error
create table diamond_region_table(id integer) @recreate(d_group);

@end_schema_region;

-- TEST: try to create an index on the diamond group table from not in the same region
--       it's a recreate table so that's not allowed
-- + {create_index_stmt}: err
-- + Error % if a table is marked @recreate, its indices must be in its schema region 'invalid_wrong_group_index'
-- +1 Error
create index invalid_wrong_group_index on diamond_region_table(id);

-- TEST: try to use a WITH_SELECT form in a select expression
-- + {assign}: X: integer variable
-- + {with_select_stmt}: _anon: integer notnull
-- - Error
SET x := (WITH threads2 (count) AS (SELECT 1 foo) SELECT COUNT(*) FROM threads2);

-- TEST: declare a table valued function
-- + {declare_select_func_stmt}: select: { foo: text } select_func
-- + {name tvf}: select: { foo: text }
-- - Error
declare select function tvf(id integer) (foo text);

-- TEST: use a table valued function
-- + {create_proc_stmt}: select: { foo: text } dml_proc
-- + {select_stmt}: select: { foo: text }
-- - Error
create proc using_tvf()
begin
  select * from tvf(1);
end;

-- TEST: use a table valued function but with a arg error
-- + {select_stmt}: err
-- + {table_function}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc using_tvf_invalid_arg()
begin
  select * from tvf(NOT 'x');
end;

-- TEST: use a table valued function but with a bogus arg type
-- + {select_stmt}: err
-- + {table_function}: err
-- + Error % incompatible types in expression 'id'
-- +1 Error
create proc using_tvf_arg_mismatch()
begin
  select * from tvf('x');
end;

-- TEST: use a table valued function
-- + {create_proc_stmt}: select: { foo: text } dml_proc
-- + {select_stmt}: select: { foo: text }
-- + {dot}: foo: text
-- - Error
create proc using_tvf_unaliased()
begin
  select * from tvf(1) where tvf.foo = 'x';
end;

-- TEST: use a table valued function aliased
-- + {create_proc_stmt}: select: { foo: text } dml_proc
-- + {select_stmt}: select: { foo: text }
-- + {dot}: foo: text
-- - Error
create proc using_tvf_aliased()
begin
  select * from tvf(1) T1 where T1.foo = 'x';
end;

-- TEST: use a non-table-valued function in FROM
-- + {select_stmt}: err
-- + {table_function}: err
-- + Error % function is not a table-valued-function 'SqlUserFunc'
-- +1 Error
create proc using_not_a_tvf()
begin
  select * from SqlUserFunc(1);
end;

-- TEST: use a invalid symbol in FROM
-- + {select_stmt}: err
-- + {table_function}: err
-- + Error % table-valued function not declared 'ThisDoesNotExist'
-- +1 Error
create proc using_not_a_func()
begin
  select * from ThisDoesNotExist(1);
end;

-- TEST: declare table valued function that consumes an object
-- +  {declare_select_func_stmt}: select: { id: integer } select_func
-- + {params}: ok
-- + {param}: rowset: object<rowset> variable in
-- - Error
declare select function ReadFromRowset(rowset Object<rowset>) (id integer);

-- TEST: use a table valued function that consumes an object
-- + {create_proc_stmt}: select: { id: integer } dml_proc
-- + {table_function}: TABLE { ReadFromRowset: select }
-- + {name ReadFromRowset}: TABLE { ReadFromRowset: select }
-- + {name rowset}: rowset: object<rowset> variable in
-- - Error
create proc rowset_object_reader(rowset Object<rowset>)
begin
  select * from ReadFromRowset(rowset);
end;

-- TEST: convert pointer to long for binding
-- + {assign}: ll: longint notnull variable
-- + {name ptr}: longint notnull
-- - Error
set ll := (select ptr(obj_var));

-- TEST: convert pointer to long for binding -- failure case
-- + {assign}: err
-- + {arg_list}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
set ll := (select ptr(not 'x'));

-- TEST: try to use 'ptr' outside of sql context
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'ptr'
-- +1 Error
set ll := ptr(obj_var);

-- TEST: try to use 'ptr' with wrong arg count
-- + {assign}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'ptr'
-- +1 Error
set ll := ptr(obj_var, 1);

-- TEST: try to alias a column with a local variable of the same name
-- + {assign}: err
-- + {select_stmt}: err
-- + Error % a variable name might be ambiguous with a column name, this is an anti-pattern 'id'
-- +1 Error
create proc variable_conflict()
begin
  declare id integer;
  set id := (select id from foo);
end;

-- TEST: group concat has to preserve sensitivity
-- + {select_stmt}: select: { gc: text sensitive }
-- - Error
select group_concat(name) gc from with_sensitive;

-- TEST: group concat must always return nullable
-- + {select_stmt}: select: { gc: text }
-- + {strlit 'not-null'}: text notnull
-- - Error
select group_concat('not-null') gc from foo;

-- TEST: min/max (same code) only accept numerics and strings
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + Error % argument must be a string or numeric in 'min'
-- +1 Error
create proc min_gets_blob(a_blob blob)
begin
  select min(a_blob) from foo;
end;

-- TEST: non aggregate version basic test
-- this version of min is still allowed to return not null, it isn't an aggregate
-- it also doesn't need a from clause
-- + {select_expr_list_con}: select: { min_stuff: real notnull }
-- - Error
set my_real := (select min(1.2, 2, 3) as min_stuff);

-- TEST: create a sum using a bool
-- + {select_stmt}: select: { _anon: integer }
-- + {and}: bool notnull
-- - Error
select sum(1 and 1) from foo;

-- TEST: create a sum using a long integer
-- + {select_stmt}: select: { _anon: longint }
-- - Error
select sum(1L) from foo;

-- TEST: create a sum using a real
-- + {select_stmt}: select: { _anon: real }
-- - Error
select sum(1.2) from foo;

-- TEST: try to do a min with incompatible arguments (non aggregate form)
-- + {select_stmt}: err
-- + Error % incompatible types in expression 'min'
-- +1 Error
select min(1, 'x');

-- TEST: try to do a min with non-numeric arguments (first position) (non aggregate form)
-- + {select_stmt}: err
-- + Error % argument must be a string or numeric in 'min'
-- +1 Error
select min(NULL, 'x');

-- TEST: try to do a min with non-numeric arguments (not first position) (non aggregate form)
-- + {select_stmt}: err
-- + Error % argument must be a string or numeric in 'min'
-- +1 Error
select min('x', NULL, 'y');

-- TEST: min on strings
-- + {select_stmt}: select: { _anon: text notnull }
-- - Error
select min('x', 'y');

-- TEST: min on numerics (upgraded to real in this case)
-- + {select_stmt}: select: { _anon: real notnull }
-- - Error
select min(1, 1.2);

-- TEST: min on numerics (checks sensitivy and nullable)
-- + {select_stmt}: select: { _anon: longint sensitive }
-- - Error
select min(_sens, 1L);

-- TEST: create a non-recreate table that references a recreated table
-- + create_table_stmt}: err
-- + col_attrs_fk}: err
-- +1 Error % referenced table can be independently be recreated so it cannot be used in a foreign key 'recreatable'
-- + Error
create table recreatable_reference_1(
  id integer primary key references recreatable(id),
  name text
);

-- TEST: create a recreate table that references a recreated table
-- + create_table_stmt}: err
-- + col_attrs_fk}: err
-- +1 Error % referenced table can be independently be recreated so it cannot be used in a foreign key 'recreatable'
-- + Error
create table recreatable_reference_2(
  id integer primary key references recreatable(id),
  name text
) @recreate;

-- TEST: make a recreate table, put it in a group "rtest"
-- + {create_table_stmt}: in_group_test: { id: integer notnull primary_key, name: text } @recreate(rtest)
-- + {recreate_attr}
-- + {name rtest}
-- - Error
create table in_group_test(
  id integer primary key,
  name text
) @recreate(rtest);

-- TEST: create a recreate table that references a recreated table, it's in a group, but I'm not
-- + create_table_stmt}: err
-- + col_attrs_fk}: err
-- +1 Error % referenced table can be independently be recreated so it cannot be used in a foreign key 'in_group_test'
-- + Error
create table recreatable_reference_3(
  id integer primary key references in_group_test(id),
  name text
) @recreate;

-- TEST: create a recreate table that references a recreated table, it's in a group, but I'm in a different group
-- + create_table_stmt}: err
-- + col_attrs_fk}: err
-- +1 Error % referenced table can be independently be recreated so it cannot be used in a foreign key 'in_group_test'
-- + Error
create table recreatable_reference_4(
  id integer primary key references in_group_test(id),
  name text
) @recreate(rtest_other_group);

-- TEST: create a recreate table that references a recreated table, it's in the same group so this one is ok (finally)
-- + {create_table_stmt}: recreatable_reference_5: { id: integer notnull primary_key foreign_key, name: text } @recreate(rtest)
-- + {recreate_attr}
-- + {name rtest}
-- + {name in_group_test}
-- + {col_attrs_fk}: ok
-- - Error
create table recreatable_reference_5(
  id integer primary key references in_group_test(id) on delete cascade on update cascade,
  name text
) @recreate(rtest);

-- TEST: once we have found one error in the constraint section it's not safe to proceed to look for more
--       errors because the semantic type of the node has arleady been changed to "error"
--       so we have to early out.  To prove this is happening we force an error in the PK section here
--       this error will not be reported becuase we bail before that.
-- + {create_table_stmt}: err
-- + Error % foreign key refers to non-existent table 'table_not_found'
-- + {pk_def}
-- - {pk_def}: err
-- +1 Error
CREATE TABLE early_out_on_errs(
  result_index INTEGER NOT NULL,
  query TEXT NOT NULL,
  FOREIGN KEY (query) REFERENCES table_not_found(q),
  PRIMARY KEY (garbonzo)
) @RECREATE;

-- TEST: create a recreate table that references a recreated table
--       we're doing this in the context of a proc so we we MUST generate the table
--       best we can.  This is exactly what schema upgrade will do.  Note that
--       in the context of schema upgrade the recreate annotations and such will be
--       missing from the statements that do the work.  The declarations are right though.
-- + {create_table_stmt}: recreatable_reference_6: { id: integer notnull primary_key foreign_key, name: text } @recreate(rtest_other_group)
-- + {col_attrs_fk}: ok
-- - Error
create proc some_fake_upgrader()
begin
  create table recreatable_reference_6(
    id integer primary key references in_group_test(id) on update cascade on delete cascade,
    name text
  ) @recreate(rtest_other_group);
end;

-- TEST: enable strict join mode
-- + {enforce_strict_stmt}: ok
-- + {int 3}
-- - Error
@enforce_strict join;

-- TEST: non-ansi join is used... error in strict mode
-- + {select_stmt}: err
-- + Error % non-ANSI joins are forbidden if strict join mode is enabled
-- +1 Error
select * from foo, bar;

-- TEST: try to use an out cursor like a statement cursor, not valid
-- + {create_proc_stmt}: err
-- + {declare_cursor}: err
-- + Error % use FETCH FROM for procedures that returns a cursor with OUT 'C'
-- +1 Error
create proc bar()
begin
  declare C cursor for call out_cursor_proc();
end;

-- TEST: can't use offset without limit
-- + {select_stmt}: err
-- + {opt_offset}: err
-- + Error % the OFFSET clause may only be used if LIMIT is also present
-- +1 Error
select * from foo offset 1;

-- TEST: upsert with insert/select and do nothing statement
-- + {create_proc_stmt}: ok dml_proc
-- + {name upsert_do_nothing}: ok dml_proc
-- + {upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {upsert_update}: ok
-- + {conflict_target}: excluded: { id: integer notnull }
-- - Error
create proc upsert_do_nothing()
begin
  insert into foo select id from bar where 1 on conflict(id) do nothing;
end;

-- TEST: with upsert with insert/select and do nothing statement
-- + {create_proc_stmt}: ok dml_proc
-- + {name with_upsert_do_nothing}: ok dml_proc
-- + {with_upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {upsert_update}: ok
-- + {conflict_target}: excluded: { id: integer notnull }
-- - Error
create proc with_upsert_do_nothing()
begin
  with data(id) as (values (1), (2), (3))
  insert into foo select id from data where 1 on conflict(id) do nothing;
end;

-- TEST: with upsert with error in the CTE
-- + {create_proc_stmt}: err
-- + {with_upsert_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc with_upsert_cte_err()
begin
  with data(id) as (values (not 'x'))
  insert into foo select id from data where 1 on conflict(id) do nothing;
end;

-- TEST: with upsert with error in the insert
-- + {create_proc_stmt}: err
-- + {with_upsert_stmt}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
create proc with_upsert_insert_err()
begin
  with data(id) as (values (1))
  insert into foo select id from data where not 'x' on conflict(id) do nothing;
end;

-- TEST: upsert with insert and do nothing statement
-- + {create_proc_stmt}: ok dml_proc
-- + {name upsert_without_conflict_target}: ok dml_proc
-- + {upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {upsert_update}: ok
-- + {conflict_target}: excluded: { id: integer notnull }
-- - Error
create proc upsert_without_conflict_target()
begin
  insert into foo(id) values(1) on conflict do nothing;
end;

-- TEST: upsert or update statement
-- + {create_proc_stmt}: ok dml_proc
-- + {name upsert_update}: ok dml_proc
-- + {upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {update_stmt}: foo: { id: integer notnull primary_key autoinc }
-- + {upsert_update}: ok
-- + {conflict_target}: excluded: { id: integer notnull }
-- + {opt_where}: bool notnull
-- - Error
create proc upsert_update()
begin
  insert into foo(id) values(1) on conflict(id) where id=10 do update set id=id+1 where id=20;
end;

-- TEST: upsert with conflict on unknown column
-- + {create_proc_stmt}: err
-- + {upsert_stmt}: err
-- + {conflict_target}: err
-- + Error % name not found 'bogus'
-- +1 Error
create proc upsert_conflict_on_unknown_column()
begin
  insert into foo(id) values(1) on conflict(id, bogus) do nothing;
end;

-- TEST: upsert with table name added to update statement
-- + {create_proc_stmt}: err
-- + {upsert_stmt}: err
-- + {update_stmt}: err
-- + Error % upsert statement does not include table name in the update statement 'foo'
-- +1 Error
create proc upsert_invalid_update_stmt()
begin
  insert into foo(id) values(1) on conflict(id) do update foo set id = 0;
end;

-- TEST: upsert with select statement without WHERE
-- + {create_proc_stmt}: err
-- + {upsert_stmt}: err
-- + {insert_stmt}: err
-- + Error % upsert statement requires a where clause if the insert clause uses select
-- +1 Error
create proc upsert_no_where_stmt()
begin
  insert into foo select id from (select * from bar) on conflict(id) do nothing;
end;

-- TEST: upsert with a not normal insert statement
-- + {create_proc_stmt}: err
-- + {name upsert_or_ignore}: err
-- + {upsert_stmt}: err
-- + {insert_stmt}: err
-- + Error % upsert syntax only supports INSERT INTO 'foo'
-- + Error
create proc upsert_or_ignore()
begin
  insert or ignore into foo select id from bar where 1 on conflict(id) do nothing;
end;

-- TEST: upsert with bogus column where statement
-- + {create_proc_stmt}: err
-- + {name upsert_with_bogus_where_stmt}: err
-- + {upsert_stmt}: err
-- + {insert_stmt}: ok
-- + {upsert_update}: err
-- + {conflict_target}: err
-- + {name bogus}: err
-- + Error % name not found 'bogus'
-- + Error
create proc upsert_with_bogus_where_stmt()
begin
  insert into foo(id) values(1) on conflict(id) where bogus=1 do nothing;
end;

-- TEST: update statement without table name
-- + {create_proc_stmt}: err
-- + {name update_without_table_name}: err
-- + {create_trigger_stmt}: err
-- + {update_stmt}: err
-- + Error % update statement require table name
-- +1 Error
create proc update_without_table_name()
begin
  create temp trigger update_without_table_name_trigger
    before delete on bar
  begin
    update set id=1 where id=9;
  end;
end;

-- TEST: upsert statement. The unique column in conflict target is not a unique key
-- + {create_proc_stmt}: err
-- + {name upsert_conflict_target_column_not_unique_key}: err
-- + {upsert_stmt}: err
-- + {conflict_target}: err
-- + Error % the set of columns referenced in the conflict target should match exactly a unique key in table we apply upsert
-- +1 Error
create proc upsert_conflict_target_column_not_unique_key()
begin
  insert into bar(id) values(1) on conflict(name) do nothing;
end;

-- TEST: upsert statement. The set of columns in conflict target do match unique key
-- + {create_proc_stmt}: ok dml_proc
-- + {name upsert_conflict_target_columns_valid}: ok dml_proc
-- + {upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {upsert_update}: ok
-- + {conflict_target}: excluded: { a: integer notnull, b: text, c: real, d: longint }
-- - Error
create proc upsert_conflict_target_columns_valid()
begin
  insert into simple_ak_table_2(a, b, c, d) values(1, "t", 1.7, 1) on conflict(a, b) do nothing;
end;

-- TEST: enforce strict upsert statement
-- + @ENFORCE_STRICT UPSERT STATEMENT;
-- + {enforce_strict_stmt}: ok
-- + {int 4}
-- - Error
@enforce_strict upsert statement;

-- TEST: upsert statement failed validation in strict mode
-- + {upsert_stmt}: err
-- + Error % upsert statement are forbidden if strict upsert statement mode is enabled
-- +1 Error
insert into bar(id) values(1) on conflict do nothing;

-- TEST: enforcement normal upsert statement
-- + @ENFORCE_NORMAL UPSERT STATEMENT;
-- + {enforce_normal_stmt}: ok
-- + {int 4}
@enforce_normal upsert statement;

-- TEST: upsert statement succeed validation in normal mode
-- + {upsert_stmt}: ok
-- - Error
insert into bar(id) values(1) on conflict do nothing;

-- TEST: enforce strict window function
-- + @ENFORCE_STRICT WINDOW FUNCTION;
-- + {enforce_strict_stmt}: ok
-- + {int 5}
-- - Error
@enforce_strict window function;

-- TEST: window function invocaction failed validation in strict mode
-- + {window_func_inv}: err
-- + Error % window function invocation are forbidden if strict window function mode is enabled
-- +1 Error
select id, rank() over () from foo;

-- TEST: enforcement normal window function
-- + @ENFORCE_NORMAL WINDOW FUNCTION;
-- + {enforce_normal_stmt}: ok
-- + {int 5}
@enforce_normal window function;

-- TEST: window function invocation succeed validation in normal mode
-- + {window_func_inv}: integer notnull
-- - Error
select id, rank() over () from foo;

-- TEST: min/max may not appear outside of a SQL statement
-- (there is no codegen support for this, though it could be added)
-- the code path for min an max is identical so one test suffices
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'max'
set X := max(1,2);

-- TEST: substr may not appear outside of a SQL statement
-- (there is no codegen support for this, though it could be added)
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'substr'
set a_string := substr('x', 1, 2);

-- TEST: simple success -- substr not nullable string
-- + {create_proc_stmt}: select: { t: text notnull } dml_proc
-- + {name substr}: text notnull
-- - Error
create proc substr_test_notnull(t text not null)
begin
  select substr(t, 1, 2) as t ;
end;

-- TEST: simple success -- substr not nullable string one arg
-- + {create_proc_stmt}: select: { t: text notnull } dml_proc
-- + {name substr}: text notnull
-- - Error
create proc substr_test_onearg(t text not null)
begin
  select substr(t, 1) as t ;
end;

-- TEST: simple success -- substr nullable string
-- + {create_proc_stmt}: select: { t: text } dml_proc
-- + {name substr}: text
-- - Error
create proc substr_test_nullable(t text)
begin
  select substr(t, 1, 2) as t;
end;

-- TEST: simple success -- substr sensitive string
-- + {create_proc_stmt}: select: { t: text sensitive } dml_proc
-- + {name substr}: text sensitive
-- - Error
create proc substr_test_sensitive(t text @sensitive)
begin
  select substr(t, 1, 2) as t;
end;

-- TEST: substr error -- arg1 is not a string
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % first argument must be a string in function 'substr'
-- +1 Error
create proc substr_test_notstring()
begin
  select substr(3, 1, 2);
end;

-- TEST: substr error -- arg2 is not a number
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % argument must be numeric 'substr'
-- +1 Error
create proc substr_test_arg2string()
begin
  select substr('x', '1', 2);
end;

-- TEST: substr error -- arg3 is not a number
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % argument must be numeric 'substr'
-- +1 Error
create proc substr_test_arg3string()
begin
  select substr('x', 1, '2');
end;

-- TEST: substr error -- too few arguments
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'substr'
-- +1 Error
create proc substr_test_toofew()
begin
  select substr('x');
end;

-- TEST: substr error -- too many arguments
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'substr'
-- +1 Error
create proc substr_test_toomany()
begin
  select substr('x', 1, 2, 4);
end;

-- TEST: create ad hoc version migration -- success
-- + {schema_ad_hoc_migration_stmt}: ok
-- + {version_annotation}
-- + {int 5}
-- + {name MyAdHocMigration}
-- - Error
@schema_ad_hoc_migration(5, MyAdHocMigration);

-- TEST: create ad hoc version migration -- bogus name
-- + Error % the name of a migration procedure may not end in '_crc' 'not_allowed_crc'
-- +1 Error
@schema_ad_hoc_migration(5, not_allowed_crc);

-- TEST: create ad hoc version migration -- duplicate proc
-- + {schema_ad_hoc_migration_stmt}: err
-- + Error % a procedure can appear in only one annotation 'MyAdHocMigration'
-- +1 Error
@schema_ad_hoc_migration(5, MyAdHocMigration);

-- TEST: create ad hoc version migration -- missing proc
-- + {schema_ad_hoc_migration_stmt}: err
-- + Error % ad hoc schema migration directive must provide a procedure to run
-- +1 Error
@schema_ad_hoc_migration(2);

-- make a test table for the upsert test with a pk and some columns
create table upsert_test( id integer primary key, name text, rate real);

-- TEST: use the excluded version of the names in an upsert
-- + {upsert_stmt}: ok
-- + {insert_stmt}: ok
-- + {name upsert_test}: upsert_test: { id: integer notnull primary_key, name: text, rate: real }
-- + {conflict_target}: excluded: { id: integer notnull, name: text }
-- + {update_stmt}: upsert_test: { id: integer notnull primary_key, name: text, rate: real }
-- - Error
insert into upsert_test(id, name) values(1, 'name')
on conflict(id) do update set name = excluded.name, rate = id+1;

-- TEST: upsert statement with insert default values
-- + {upsert_stmt}: err
-- + {insert_stmt}: err
-- + {name_columns_values}
-- + {name foo}: foo: { id: integer notnull primary_key autoinc }
-- + {default_columns_values}
-- + {upsert_update}
-- + {conflict_target}
-- +1 Error % the upsert-clause is not compatible with DEFAULT VALUES
-- + Error
insert into foo default values on conflict do nothing;

-- TEST declare a value fetcher that doesn't use DML
-- + {declare_proc_stmt}: val_fetch: { id: text } uses_out
-- - dml_proc
-- + DECLARE PROC val_fetch (seed INTEGER NOT NULL) OUT (id TEXT);
-- - USING TRANSACTION
DECLARE PROC val_fetch (seed INTEGER NOT NULL) OUT (id TEXT);

-- TEST declare a value fetcher that does use DML
-- + {declare_proc_stmt}: val_fetch_dml: { id: text } dml_proc uses_out
-- + DECLARE PROC val_fetch_dml (seed INTEGER NOT NULL) OUT (id TEXT) USING TRANSACTION;
DECLARE PROC val_fetch_dml (seed INTEGER NOT NULL) OUT (id TEXT) USING TRANSACTION;

-- TEST: declare a valid root deployable region
-- + {declare_deployable_region_stmt}: root_deployable_region: region deployable
-- + {name root_deployable_region}
-- - Error
@declare_deployable_region root_deployable_region;

-- TEST: create an error in a deployoable region (duplicate name)
-- + {declare_deployable_region_stmt}: err
-- + Error % schema region already defined 'root_deployable_region'
-- +1 Error
@declare_deployable_region root_deployable_region;

-- TEST: a simple leaves to use
-- + {declare_schema_region_stmt}: leaf1: region
-- - Error
@declare_schema_region leaf1;

-- + {declare_schema_region_stmt}: leaf2: region
-- - Error
@declare_schema_region leaf2;

-- + {declare_schema_region_stmt}: leaf3: region
-- - Error
@declare_schema_region leaf3;

-- TEST: this looks ok but leaf region will be subsumed later... so we will create an error later
-- + {declare_schema_region_stmt}: err
-- This node won't be an error when it's created, the error is emitted later when uses_leaf_3 is declared so no error message yet
-- The node does ultimately resolve into an error
-- - Error at
@declare_schema_region pending_leaf_user using leaf3;

-- TEST leaf region is claimed, this makes pending_leaf_user in error
-- + {declare_deployable_region_stmt}: err
-- + Error % region links into the middle of a deployable region; you must point to the root of 'uses_leaf_3' not into the middle: 'pending_leaf_user'
-- +1 Error
@declare_deployable_region uses_leaf_3  using leaf3;

-- TEST: declare a valid deployable region with dependencies
-- + {declare_deployable_region_stmt}: depl1: region deployable
-- + {name depl1}
-- + {name leaf1}
-- + {name leaf2}
-- - Error
@declare_deployable_region depl1 using leaf1, leaf2;

-- TEST: make a region that links into into the middle of outer_deployable_region
-- + Error % region links into the middle of a deployable region; you must point to the root of 'depl1' not into the middle: 'error_region'
-- +1 Error at
@declare_schema_region error_region using leaf1;

-- TEST: this is a procedure that emits several rows "manually"
-- +  {create_proc_stmt}: C: select: { A: integer notnull, B: integer notnull } variable dml_proc shape_storage uses_out_union value_cursor
-- +2 {out_union_stmt}: C: select: { A: integer notnull, B: integer notnull } variable shape_storage value_cursor
-- - Error
create proc many_row_emitter()
begin
  declare C cursor like out_cursor_proc;
  fetch C from call out_cursor_proc();
  out union C;
  out union C;
end;

-- TEST: compound selects are allowed as a select expression, they can still return one row
-- + {create_proc_stmt}: ok dml_proc
-- + {assign}: x: integer variable
-- + {name x}: x: integer variable
-- + {select_stmt}: _anon: integer notnull
-- + {select_core_compound}
-- + {int 1}
-- - Error
create proc compound_select_expr()
begin
  declare x integer;

  set x := (select 1 where 0 union select 2 limit 1);
end;

-- TEST: declare a region with a private interior
-- + {declare_schema_region_stmt}: region_hiding_something: region
-- + | {name region_hiding_something}
-- + | {region_list}
-- +   | {region_spec}
-- +     | {name depl1}
-- +     | {int 1}
-- - Error
@declare_schema_region region_hiding_something using depl1 private;

-- TEST: declare a region with non-private interior
-- + {declare_schema_region_stmt}: region_not_hiding_something: region
-- + | {name region_not_hiding_something}
-- + | {region_list}
-- +   | {region_spec}
-- +     | {name depl1}
-- +     | {int 0}
-- - Error
@declare_schema_region region_not_hiding_something using depl1;

-- - Error
@enforce_normal foreign key on update;
-- - Error
@enforce_normal foreign key on delete;

-- test regions the innermost one here "private_region" can't be reached from client_region
-- - Error
@declare_schema_region private_region;

-- - Error
@declare_schema_region containing_region using private_region private;

-- - Error
@declare_schema_region client_region using containing_region;

-- - Error
@begin_schema_region private_region;

-- - Error
create table private_region_table(id integer primary key);

-- - Error
@end_schema_region;

-- - Error
@begin_schema_region containing_region;

-- - Error
create table containing_region_table(id integer primary key references private_region_table(id));

-- - Error
@end_schema_region;

-- - Error
@begin_schema_region client_region;

-- TEST : not able to access private region
-- + {create_table_stmt}: err
-- + Error % (object is in schema region 'private_region' not accessible from region 'client_region') 'private_region_table'
-- +1 Error
create table client_region_table_1(id integer primary key references private_region_table(id));

-- TEST: non-private table is good to go
-- + {create_table_stmt}: client_region_table_2: { id: integer notnull primary_key foreign_key }
-- + {fk_target_options}
-- +   | {fk_target}
-- +   | | {name containing_region_table}
-- - Error
create table client_region_table_2(id integer primary key references containing_region_table(id));

-- - Error
@end_schema_region;

-- TEST: explain not supported
-- + {explain_stmt}: err
-- + {int 1}
-- + Error % Only [EXPLAIN QUERY PLAN ...] statement is supported
-- +1 Error
explain select 1;

-- TEST: explain query plan with select
-- + {explain_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {int 2}
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull, name: text, rate: longint }
-- - Error
explain query plan select * from foo inner join bar where foo.id = 1;

-- TEST: explain query plan with update
-- + {explain_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {int 2}
-- + {update_stmt}: bar: { id: integer notnull, name: text, rate: longint }
-- - Error
explain query plan update bar set id = 1 where name = 'Stella';

-- TEST: explain query plan with incorrect select stmt
-- + {explain_stmt}: err
-- + {int 2}
-- + {select_stmt}: err
-- + Error % name not found 'bogus'
-- +1 Error
explain query plan select bogus;

-- TEST: explain query plan as result set of a proc
-- + {create_proc_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull } dml_proc
-- + {name explain_query}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {explain_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {int 2}
-- - Error
create proc explain_query()
begin
  explain query plan select 1;
end;

-- TEST: explain query plan cursor
-- + {declare_cursor}: c: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull } variable
-- + {name c}: c: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull } variable
-- + {explain_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {int 2}
-- - Error
declare c cursor for explain query plan select * from foo inner join bar;

-- TEST: explain query plan cursor in proc
-- + {create_proc_stmt}: ok dml_proc
-- + {name explain_query_with_cursor}: ok dml_proc
-- + {declare_cursor}: c: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull } variable
-- + {name c}: c: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull } variable shape_storage
-- + {explain_stmt}: explain_query: { iselectid: integer notnull, iorder: integer notnull, ifrom: integer notnull, zdetail: text notnull }
-- + {int 2}
-- - Error
create proc explain_query_with_cursor()
begin
  declare c cursor for explain query plan select 1;
  fetch c;
end;

-- TEST: test nullability result on column X in a union select
-- + {select_stmt}: union_all: { X: text }
-- + {select_core_list}: union_all: { X: text }
-- + {select_core}: select: { X: text notnull }
-- + {select_core_list}: select: { X: null }
-- + {select_core}: select: { X: null }
select "x" as X
union all
select null as X;

-- TEST: test nullability result on column X in a union select without alias
-- + {create_proc_stmt}: union_all: { X: text } dml_proc
-- + {name mixed_union}: union_all: { X: text } dml_proc
-- + {select_stmt}: union_all: { X: text }
-- + {select_core_list}: union_all: { X: text }
-- + {select_core}: select: { X: text notnull }
-- + {select_core_list}: select: { X: null }
-- + {select_core}: select: { X: null }
create proc mixed_union()
begin
  select "x" X
  union all
  select null X;
end;

-- TEST: test nullability result on column X in a union select without alias
-- + {create_proc_stmt}: select: { X: text } dml_proc
-- + {name mixed_union_cte}: select: { x: text }
-- + {with_select_stmt}: select: { X: text }
-- + {select_stmt}: union_all: { X: text }
-- + {select_core_list}: union_all: { X: text }
-- + {select_core}: select: { X: text notnull }
-- + {select_core_list}: select: { X: null }
-- + {select_core}: select: { X: null }
-- + {select_stmt}: select: { x: text }
-- + {select_core_list}: select: { x: text }
create proc mixed_union_cte()
begin
  with core(x) as (
    select "x" X
    union all
    select null X
  )
  select * from core;
end;

-- TEST: select with a basic window function invocation
-- + {select_stmt}: select: { id: integer notnull, row_num: integer notnull }
-- + {select_expr}: row_num: integer notnull
-- + {window_func_inv}: integer notnull
-- + {call}: integer notnull
-- + {name row_number}: integer notnull
-- + {call_filter_clause}
-- + {window_defn}: ok
-- + {opt_as_alias}
-- + {name row_number}
-- - Error
select id, row_number() over () as row_num from foo;

-- TEST: window function invocation like regular function
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {call}: err
-- + Error % function may not appear in this context 'row_number'
-- + Error
select id, row_number() as row_num from foo;

-- TEST: window function invocation outside [SELECT expr] statement
-- + {select_stmt}: err
-- + {select_from_etc}: err
-- + {opt_where}: err
-- + {window_func_inv}: err
-- + {call}
-- + Error % Window function invocations can only appear in the select list of a select statement
-- + Error
select 1 where row_number() over ();

-- TEST: test invalid number of argument on window function row_number()
-- + {select_stmt}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name row_number}: err
-- + {select_from_etc}: TABLE { foo: foo }
-- + Error % function got incorrect number of arguments 'row_number'
-- + Error
select id, row_number(1) over () as row_num from foo;

-- TEST: window function invocatin with a window clause
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull, _anon: integer notnull }
-- + {select_expr}: integer notnull
-- +2 {window_func_inv}: integer notnull
-- +2 {call}: integer notnull
-- +2 {name row_number}: integer notnull
-- +2 {call_filter_clause}
-- + {name win1}
-- + {name win2}
-- + {opt_select_window}: ok
-- + {window_clause}: ok
-- +2 {window_name_defn_list}
-- +2 {window_name_defn}: ok
-- + {name win1}
-- + {name win2}
-- +2 {window_defn}: ok
-- - Error
select id, row_number() over win1, row_number() over win2
  from foo
  window
    win1 as (),
    win2 as ()
order by id;

-- TEST: test invalid window name
-- + {select_stmt}: err
-- + {window_func_inv}: err
-- + {call_filter_clause}
-- + {name bogus}: err
-- + Error % Window name is not defined 'bogus'
-- + Error
select id, row_number() over bogus
  from foo;

-- TEST: test window name definition not used
-- + {select_stmt}: err
-- + {opt_select_window}: err
-- + {window_clause}: err
-- + {window_name_defn}: err
-- + {name win}: err
-- + {window_defn}
-- + Error % Window name definition is not used 'win'
-- + Error
select id
  from foo
  window
    win as ();

-- TEST: test filter clause in window function invocation
-- + {select_stmt}: select: { id: integer notnull, row_num: text }
-- + {select_expr}: row_num: text
-- + {window_func_inv}: id: text
-- + {call}: id: text
-- + {name group_concat}: id: text
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- + {arg_list}
-- + {strlit '.'}: text notnull
-- + {call_filter_clause}
-- + {opt_filter_clause}: bool notnull
-- + {opt_where}: bool notnull
-- + {ge}: bool notnull
-- + {name id}: id: integer notnull
-- + {int 99}: integer notnull
-- + {window_defn}: ok
-- + {opt_as_alias}
-- + {name row_num}
-- - Error
select id, group_concat(id, '.') filter (where id >= 99) over () as row_num from foo;

-- TEST: test filter clause do not support referencing on alias column
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name avg}
-- + {arg_list}
-- + {name id}
-- + {call_filter_clause}
-- + {opt_filter_clause}: err
-- + {opt_where}: err
-- + {eq}: err
-- + {name alias}: err
-- + Error % name not found 'alias'
-- + Error
select id as alias, avg(id) filter (where alias = 0) over () from foo;

-- TEST: test FILTER clause may only be used with aggregate window functions
-- + {select_stmt}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name row_number}
-- + Error % function may not appear in this context 'row_number'
-- + Error
select 1, row_number() filter (where 1) over ();

-- TEST: test partition by grammar
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {window_func_inv}: integer notnull
-- + {call_filter_clause}
-- + {window_defn}: ok
-- + {opt_partition_by}: ok
-- + {expr_list}
-- + {name id}: id: integer notnull
-- - Error
select id, row_number() over (partition by id) from foo;

-- TEST: test order by grammar
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {window_func_inv}: integer notnull
-- + {call_filter_clause}
-- + {window_defn}: ok
-- + {opt_orderby}: ok
-- + {groupby_list}: ok
-- + {name id}: id: integer notnull
-- - Error
select id, row_number() over (order by id asc) from foo;

-- TEST: test order by bogus value
-- + {select_stmt}: err
-- + {window_func_inv}: err
-- + {call_filter_clause}
-- + {window_defn}: err
-- + {opt_orderby}: err
-- + {groupby_list}: err
-- + {name bogus}: err
-- + Error % name not found 'bogus'
-- + Error
select id, row_number() over (order by bogus asc) from foo;

-- TEST: test frame spec grammar combination
-- + {select_stmt}: select: { id: integer notnull, avg: real, _anon: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {select_expr}: avg: real
-- + {select_expr}: integer notnull
-- + {window_func_inv}: id: real
-- +2 {opt_frame_spec}: ok
-- + {int 131084}
-- +2 {expr_list}
-- + {window_func_inv}: integer notnull
-- + {opt_frame_spec}: ok
-- + {int 36994}
-- - Error
select id,
       avg(id) filter (where id > 0) over (groups unbounded preceding exclude ties) as avg,
       row_number() over (rows between id = 1 preceding and id = 45 following exclude current row)
  from foo;

-- TEST: test frame spec grammar combination
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {select_expr}: integer notnull
-- + {window_func_inv}: integer notnull
-- + {opt_frame_spec}: ok
-- - Error
select id,
       row_number() over (rows between current row and unbounded following exclude group)
  from foo;

-- TEST: test frame spec grammar combination
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {select_expr}: integer notnull
-- + {window_func_inv}: integer notnull
-- + {opt_frame_spec}: ok
-- - Error
select id,
       row_number() over (rows id > 0 preceding exclude ties)
  from foo;

-- TEST: test frame spec grammar with bogus expr
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {opt_frame_spec}: err
-- + {name bogus}: err
-- + Error % name not found 'bogus'
-- + Error
select id,
       row_number() over (rows bogus = null preceding exclude ties)
  from foo;

-- TEST: test rank() window function
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {select_expr}: integer notnull
-- + {window_func_inv}: integer notnull
-- + {call}: integer notnull
-- + {name rank}: integer notnull
-- - Error
select id, rank() over () from foo;

-- TEST: test dense_rank() window function
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {select_expr}: integer notnull
-- + {window_func_inv}: integer notnull
-- + {call}: integer notnull
-- + {name dense_rank}: integer notnull
-- - Error
select id, dense_rank() over () from foo;

-- TEST: test percent_rank() window function
-- + {select_stmt}: select: { id: integer notnull, _anon: real notnull }
-- + {select_expr}: real notnull
-- + {window_func_inv}: real notnull
-- + {call}: real notnull
-- + {name percent_rank}: real notnull
-- - Error
select id, percent_rank() over () from foo;

-- TEST: test cume_dist() window function
-- + {select_stmt}: select: { id: integer notnull, _anon: real notnull }
-- + {select_expr}: real notnull
-- + {window_func_inv}: real notnull
-- + {call}: real notnull
-- + {name cume_dist}: real notnull
-- - Error
select id, cume_dist() over () from foo;

-- TEST: test ntile() window function
-- + {select_stmt}: select: { id: integer notnull, _anon: integer notnull }
-- + {select_expr}: integer notnull
-- + {window_func_inv}: integer notnull
-- + {call}: integer notnull
-- + {name ntile}: integer notnull
-- + {int 7}: integer notnull
-- - Error
select id, ntile(7) over () from foo;

-- TEST: test ntile() window function with a non integer param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name ntile}
-- + {longint 9898989889989}: longint notnull
-- + Error % Argument must be an integer (between 1 and max integer) in function 'ntile'
-- +1 Error
select id, ntile(9898989889989) over () from foo;

-- TEST: test ntile() window function with invalid int param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name ntile}
-- + {arg_list}: err
-- + {int 0}: integer notnull
-- + Error % Argument must be an integer (between 1 and max integer) in function 'ntile'
-- +1 Error
select id, ntile(0) over () from foo;

-- TEST: test ntile() window function with too many params
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name ntile}
-- + {arg_list}:
-- + {int 1}: integer notnull
-- + {int 2}: integer notnull
-- + Error % function got incorrect number of arguments 'ntile'
-- +1 Error
select id, ntile(1, 2) over () from foo;

-- TEST: test ntile() window function outside window context
-- + {select_stmt}: err
-- + {select_where}
-- + {opt_where}: err
-- + {call}: err
-- + {name ntile}
-- + {int 7}: integer notnull
-- + Error % function may not appear in this context 'ntile'
-- +1 Error
select id from foo where ntile(7);

-- TEST: test lag() window function
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {window_func_inv}: id: integer notnull
-- + {call}: id: integer notnull
-- + {name lag}: id: integer notnull
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- + {int 1}: integer notnull
-- + {int 0}: integer notnull
-- - Error
select id, lag(id, 1, 0) over () from foo;

-- TEST: kind not compatible in lag between arg3 and arg1
-- + {select_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'dollars' vs. 'some_key'
-- + Error % The first and third arguments must be compatible in function 'lag'
-- +2 Error
select lag(cost, 1, id) over () from with_kind;

-- TEST lag with non integer offset
-- + {select_stmt}: err
-- + Error % The second argument must be an integer (between 0 and max integer) in function 'lag'
-- +1 Error
select id, lag(id, 1.3, 0) over () from foo;

-- TEST: test lag() window function with non constant index (this is ok)
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- - Error
select id, lag(id, X, 0) over () from foo;

-- TEST: test lag() window function with lag() nullable even though id is not nullable
-- + {select_stmt}: select: { id: integer notnull, id: integer }
-- + {select_expr}: id: integer
-- + {window_func_inv}: id: integer
-- + {call}: id: integer
-- + {name lag}: id: integer
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- + {int 1}: integer notnull
-- - Error
select id, lag(id, 1) over () from foo;

-- TEST: test lag() window function with first param sensitive
-- + {select_stmt}: select: { id: integer, info: integer sensitive }
-- + {select_expr}: info: integer sensitive
-- + {window_func_inv}: info: integer sensitive
-- + {call}: info: integer sensitive
-- + {name lag}: info: integer sensitive
-- + {arg_list}: ok
-- + {name info}: info: integer sensitive
-- + {int 1}: integer notnull
-- - Error
select id, lag(info, 1) over () from with_sensitive;

-- TEST: test lag() window function with third param sensitive
-- + {select_stmt}: select: { id: integer, _anon: integer sensitive }
-- + {select_expr}: integer
-- + {window_func_inv}: integer sensitive
-- + {call}: integer sensitive
-- + {arg_list}: ok
-- + {mul}: integer
-- + {int 1}: integer notnul
-- + {name info}: info: integer sensitive
-- - Error
select id, lag(id * 3, 1, info) over () from with_sensitive;

-- TEST: test lag() window function with negative integer param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name lag}
-- + {arg_list}: err
-- + Error % Argument must be an integer (between 0 and max integer) in function 'lag'
-- +1 Error
select id, lag(id, -1) over () from foo;

-- TEST: test lag() window function with invalid first param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name lag}
-- + {arg_list}: err
-- + Error % right operand cannot be a string in '|'
-- +1 Error
select id, lag(id | " ") over () from foo;

-- TEST: test lag() window function with first and third param are not same type
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name lag}
-- + {arg_list}: err
-- + Error % The first and third arguments must be compatible in function 'lag'
-- +2 Error
select id, lag(id, 0, 0.7) over () from foo;

-- TEST: test lag() window function with no param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name lag}
-- + Error % function got incorrect number of arguments 'lag'
-- +1 Error
select id, lag() over () from foo;

-- TEST: test lead() window function
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {window_func_inv}: id: integer notnull
-- + {call}: id: integer notnull
-- + {name lead}: id: integer notnull
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- + {int 1}: integer notnull
-- + {mul}: integer notnull
-- - Error
select id, lead(id, 1, id * 3) over () from foo;

-- TEST: test first_value() window function
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {window_func_inv}: id: integer notnull
-- + {call}: id: integer notnull
-- + {name first_value}: id: integer notnull
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- - Error
select id, first_value(id) over () from foo;

-- TEST: ensure the kind of the first_value is preserved
-- + {select_stmt}: select: { id: integer<some_key> }
-- + {window_func_inv}: id: integer<some_key>
-- - Error
select first_value(id) over () from with_kind;

-- TEST: ensure the kind of the first_value is preserved
-- + {select_stmt}: select: { id: integer<some_key> }
-- + {window_func_inv}: id: integer<some_key>
-- - Error
select last_value(id) over () from with_kind;

-- TEST: ensure the kind of the nth_value is preserved
-- + {select_stmt}: select: { id: integer<some_key> }
-- + {window_func_inv}: id: integer<some_key>
-- - Error
select nth_value(id, 5) over () from with_kind;

-- TEST: test first_value() window function outside window context
-- + {select_stmt}: err
-- + {select_where}
-- + {opt_where}: err
-- + {call}: err
-- + {name first_value}
-- + {int 7}: integer notnull
-- + Error % function may not appear in this context 'first_value'
-- +1 Error
select id from foo where first_value(7);

-- TEST: test last_value() window function
-- + {select_stmt}: select: { id: integer notnull, id: integer notnull }
-- + {select_expr}: id: integer notnull
-- + {window_func_inv}: id: integer notnull
-- + {call}: id: integer notnull
-- + {name last_value}: id: integer notnull
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- - Error
select id, last_value(id) over () from foo;

-- TEST: test nth_value() window function
-- + {select_stmt}: select: { id: integer notnull, id: integer }
-- + {select_expr}: id: integer
-- + {window_func_inv}: id: integer
-- + {call}: id: integer
-- + {name nth_value}: id: integer
-- + {arg_list}: ok
-- + {name id}: id: integer notnull
-- - Error
select id, nth_value(id, 1) over () from foo;

-- TEST: test nth_value() window function outside window context
-- + {select_stmt}: err
-- + {select_where}
-- + {opt_where}: err
-- + {call}: err
-- + {name nth_value}
-- + {int 7}: integer notnull
-- + Error % function may not appear in this context 'nth_value'
-- +1 Error
select id from foo where nth_value(7, 1);

-- TEST: test nth_value() window function with incorrect number of param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name nth_value}
-- + Error % function got incorrect number of arguments 'nth_value'
-- +1 Error
select id, nth_value(id) over () from foo;

-- TEST: test nth_value() window function with invalid value on second param
-- + {select_stmt}: err
-- + {select_expr}: err
-- + {window_func_inv}: err
-- + {call}: err
-- + {name nth_value}: ok
-- + {name id}: id: integer notnull
-- + {int 0}: integer notnull
-- + Error % The second argument must be an integer between 1 and max integer in function 'nth_value'
-- +1 Error
select id, nth_value(id, 0) over () from foo;

-- TEST: try total functions with sensitive param
-- + {select_stmt}: select: { info: real notnull sensitive }
-- + {name total}: info: real notnull sensitive
-- + {name info}: info: integer sensitive
-- - Error
select total(info) from with_sensitive;

-- TEST: combine dummy data and FROM arguments in INSERT
-- This is all sugar
-- + INSERT INTO referenceable(a, b, c, d, e) VALUES(x, y, printf('c_%d', _seed_), printf('d_%d', _seed_), _seed_) @DUMMY_SEED(1) @DUMMY_NULLABLES;
-- - Error
create proc insert_using_args_with_dummy(x integer not null, y real not null)
begin
  insert into referenceable(a, b) from arguments @dummy_seed(1) @dummy_nullables;
end;

-- TEST: combine dummy data and FROM arguments in FETCH
-- This is all sugar
-- + FETCH C(a, b, c, d, e) FROM VALUES(x, y, printf('c_%d', _seed_), printf('d_%d', _seed_), _seed_) @DUMMY_SEED(1) @DUMMY_NULLABLES;
-- - Error
create proc fetch_using_args_with_dummy(x integer not null, y real not null)
begin
  declare C cursor like referenceable;
  fetch C(a,b) from arguments @dummy_seed(1) @dummy_nullables;
end;

-- TEST: ensure that empty list is expanded
-- + FETCH C(a, b, c, d, e) FROM VALUES(1, 2, 'x', 'y', 5);
-- - Error
create proc fetch_from_empty_col_list()
begin
  declare C cursor like referenceable;
  fetch C from values(1, 2, 'x', 'y', 5);
  out C;
END;

-- we'll need this cursor for the FROM cursor tests
declare c_bar cursor like referenceable;

-- TEST: verify that we can insert from a match cursor
-- This is a sugar feature, so we only need to check the rewrite
-- Further semantic validation of the expansion happens normally as though the fields had been typed manually
-- + INSERT INTO referenceable(a, b, c, d, e) VALUES(c_bar.a, c_bar.b, c_bar.c, c_bar.d, c_bar.e);
-- + {insert_stmt}: ok
-- + {name referenceable}: referenceable: { a: integer notnull primary_key, b: real unique_key, c: text, d: text, e: longint }
-- - Error
insert into referenceable from cursor c_bar;

-- TEST: try to use no columns from the cursor
-- + {insert_stmt}: err
-- + Error % FROM [shape] is redundant if column list is empty
-- +1 Error
insert into referenceable() from cursor c_bar;

-- TEST: try to use a cursor that has no storage (a non automatic cursor)
-- + {insert_stmt}: err
-- + Error % cannot read from a cursor without fields 'fetch_cursor'
-- +1 Error
insert into referenceable from cursor fetch_cursor;

-- we need this cursor with only one field to test the case where the cursor is too small
declare small_cursor cursor like select 1 x;

-- TEST: try to use a cursor that has not enough fields
-- + {insert_stmt}: err
-- + Error % [shape] has too few fields 'small_cursor'
-- +1 Error
insert into referenceable from cursor small_cursor;

-- TEST: try to use something that isn't a cursor
-- + {insert_stmt}: err
-- + Error % variable is not a cursor 'X'
-- +1 Error
insert into referenceable from cursor X;

-- TEST -- simple use of update cursor statement
-- + {update_cursor_stmt}: ok
-- + | {name small_cursor}: small_cursor: select: { x: integer notnull } variable shape_storage value_cursor
-- + | {columns_values}
-- +   | {column_spec}
-- +   | | {name_list}
-- +   |   | {name x}: x: integer notnull
-- +   | {insert_list}
-- +     | {int 2}: integer notnull
-- - Error
update cursor small_cursor(x) from values (2);

-- TEST -- wrong type
-- + {update_cursor_stmt}: err
-- + Error % incompatible types in expression 'x'
-- +1 Error
update cursor small_cursor(x) from values ('x');

-- TEST -- wrong number of columns
-- + {update_cursor_stmt}: err
-- + Error % count of columns differs from count of values
-- +1 Error
update cursor small_cursor(x) from values (1, 2);

-- TEST -- invalid column
-- + {update_cursor_stmt}: err
-- + Error % name not found 'w'
-- +1 Error
update cursor small_cursor(w) from values (1);

-- TEST -- not an auto cursor
-- + {update_cursor_stmt}: err
-- + Error % cursor was not used with 'fetch [cursor]' 'my_cursor'
-- +1 Error
update cursor my_cursor(one) from values (2);

-- TEST -- like statement can't be resolved in an update statement
-- + Error % must be a cursor, proc, table, or view 'not_a_symbol'
-- +1 Error
update cursor my_cursor(like not_a_symbol) from values(1);

-- TEST -- not a cursor
-- + {update_cursor_stmt}: err
-- + Error % variable is not a cursor 'X'
-- +1 Error
update cursor X(one) from values (2);

-- TEST -- CTE * rewrite
-- This is just sugar so all we have to do is verify that we
-- did the rewrite correctly
-- + foo (a, b, c) AS (SELECT 1 AS a, 'b' AS b, 3.0 AS c)
-- + {with_select_stmt}: select: { a: integer notnull, b: text notnull, c: real notnull }
-- - Error
with foo(*) as (select 1 a, 'b' b, 3.0 c)
  select * from foo;

-- TEST -- CTE * rewrite but some columns were anonymous
-- + {with_select_stmt}: err
-- + Error % all columns in the select must have a name
-- +1 Error
with foo(*) as (select 1)
  select * from foo;

-- we never actully make this table, we just use its shape
create temp table foo_data (
  c1 text not null, c2 integer, c3 real, c4 real, c5 real, c6 real, c7 real, c8 real, c9 real, c10 real
);

-- make a cursor on it
declare nully_cursor cursor like foo_data;

-- TEST: use the "null fill" feature of value cursors to rewrite this monster into valid full row fetch
-- + FETCH nully_cursor(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) FROM VALUES('x', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
-- + {fetch_values_stmt}: ok
-- +10 {insert_list}
-- +9 {null}: null
-- - Error
fetch nully_cursor(c1) from values('x');

-- TEST: the one and only non-null column is missing, that's an error
-- + Error % required column missing in FETCH statement 'c1'
-- +1 Error
-- + {fetch_values_stmt}: err
fetch nully_cursor(c2) from values('x');

-- make a small cursor and load it up, it has only 2 of the columns
declare c1c7 cursor like select 'x' c1, nullable(3.2) c7;
fetch c1c7 from values('x', 3.2);

-- TEST: rewrite to use the columns of small cursor
-- + UPDATE CURSOR nully_cursor(c1, c7) FROM VALUES(c1c7.c1, c1c7.c7);
-- + {update_cursor_stmt}: ok
-- - Error
update cursor nully_cursor(like c1c7) from values (c1c7.c1, c1c7.c7);

-- TEST: full rewrite to use the columns of small cursor
-- + UPDATE CURSOR nully_cursor(c1, c7) FROM VALUES(c1c7.c1, c1c7.c7);
-- + {update_cursor_stmt}: ok
-- - Error
update cursor nully_cursor(like c1c7) from cursor c1c7;

-- TEST: try to update cursor from a bogus symbol
-- + {update_cursor_stmt}: err
-- + Error % cursor not found 'not_a_symbol'
-- +1 Error
update cursor nully_cursor(like c1c7) from cursor not_a_symbol;

-- TEST: rewrite to use the columns of small cursor
-- note that c7 did not get null and it's out of order, that confirms it came form the LIKE expression
-- + FETCH nully_cursor(c1, c7, c2, c3, c4, c5, c6, c8, c9, c10) FROM VALUES(c1c7.c1, c1c7.c7, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
-- + {fetch_values_stmt}: ok
-- - Error
fetch nully_cursor(like c1c7) from values (c1c7.c1, c1c7.c7);

-- TEST: full rewrite get the values from the cursor, same as above
-- + FETCH nully_cursor(c1, c7, c2, c3, c4, c5, c6, c8, c9, c10) FROM VALUES(c1c7.c1, c1c7.c7, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
-- + {fetch_values_stmt}: ok
-- - Error
fetch nully_cursor(like c1c7) from cursor c1c7;

-- TEST: fetch cursor form bogus cursor
-- + {fetch_values_stmt}: err
-- + Error % cursor not found 'not_a_symbol'
-- +1 Error
fetch nully_cursor(like c1c7) from cursor not_a_symbol;

-- TEST: fetch using like form -- bogus symbol
-- + {fetch_values_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'not_a_symbol'
-- +1 Error
fetch nully_cursor(like not_a_symbol) from values (1, 2);

-- make a cursor with some of the bar columns
declare id_name_cursor cursor like select 1 id, 'x' name;

-- TEST: rewrite the columns of an insert from a cursor source
-- + INSERT INTO bar(id, name) VALUES(1, 'x');
-- + {insert_stmt}: ok
-- - Error
insert into bar(like id_name_cursor) values(1, 'x');

-- TEST: insert using the like form, bogus symbol
-- + {insert_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'not_a_symbol'
-- +1 Error
insert into bar(like not_a_symbol) values(1, 'x');

-- TEST: fetch using from a cursor using the like form
-- this is sugar, again we just verify the rewrite
-- we got a subset of the nully_cursor columns as desired.
-- + FETCH c1c7(c1, c7) FROM VALUES(nully_cursor.c1, nully_cursor.c7);
-- + {fetch_values_stmt}: ok
-- - Error
fetch c1c7 from cursor nully_cursor(like c1c7);

-- TEST: fetch from cursor using the like form, bogus symbol
-- + {fetch_values_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'not_a_symbol'
-- +1 Error
fetch c1c7 from cursor nully_cursor(like not_a_symbol);

-- TEST: try to declare a procedure that uses out union
-- + DECLARE PROC out_union_user (x INTEGER) OUT UNION (id INTEGER, x TEXT);
-- + {declare_proc_stmt}: out_union_user: { id: integer, x: text } uses_out_union
-- - Error
declare proc out_union_user(x integer) out union (id integer, x text);

-- TEST: make a cursor for an externally defined out union func
-- + {declare_cursor}: out_union_cursor: out_union_user: { id: integer, x: text } variable uses_out_union
-- - Error
declare out_union_cursor cursor for call out_union_user(2);

-- a table with one sensitive column
create table sens_table(t text @sensitive);

-- TEST: introduce a declaration for the proc we are about to create, it has a sensitive result.
-- + {declare_proc_stmt}: sens_result_proc: { t: text sensitive } dml_proc
-- - Error
declare proc sens_result_proc () (t text @sensitive);

-- TEST this is compatible with the above declaration, it won't be if SENSITIVE is not preserved.
-- + {create_proc_stmt}: select: { t: text sensitive } dml_proc
-- - Error
@attribute(cql:autotest=(dummy_test))
create proc sens_result_proc()
begin
  select * from sens_table;
end;

-- TEST: simple proc decl
declare proc incompatible_result_proc () (t text);

-- TEST: this is compatible with the above declaration, it won't be if SENSITIVE is not preserved.
-- + Incompatible declarations found
-- + Error % in declare_proc_stmt : DECLARE PROC incompatible_result_proc () (t TEXT)
-- + Error % in create_proc_stmt : DECLARE PROC incompatible_result_proc () (t INTEGER NOT NULL)
-- + The above must be identical.
-- + Error % procedure declarations/definitions do not match 'incompatible_result_proc'
-- + {create_proc_stmt}: err
@attribute(cql:autotest=(dummy_test))
create proc incompatible_result_proc ()
begin
  select 1 t;
end;

-- TEST: use collate in an expression
-- + {groupby_item}
-- + {collate}: name: text
-- + {name name}: name: text
-- + {name nocase}
-- - Error
select * from bar
order by name collate nocase;

-- TEST: verify collate cannot be used in a loose expression
-- + {collate}: err
-- + Error % COLLATE may only appear in the context of a SQL statement
-- +1 Error
set a_string := 'x' collate nocase;

-- TEST: Verify error propogation through collate
-- + {collate}: err
-- + Error % string operand not allowed in 'NOT'
-- +1 Error
select (not 'x') collate nocase;

-- TEST: verify that duplicate table with different "IF NOT EXISTS" is still ok
-- + {create_table_stmt}: foo: { id: integer notnull primary_key autoinc }
-- + {table_flags_attrs}
-- + {int 2}
-- - Error
create table if not exists foo(
  id integer PRIMARY KEY AUTOINCREMENT
);

-- TEST: verify that duplicate view with different "IF NOT EXISTS" is still ok
-- + {create_view_stmt}: MyView: { f1: integer notnull, f2: integer notnull, f3: integer notnull }
-- + {int 2}
-- - Error
create view if not exists MyView as select 1 as f1, 2 as f2, 3 as f3;

-- TEST: verify that duplicate trigger  with different "IF NOT EXISTS" is still ok
-- + {create_trigger_stmt}: ok
-- + {int 2}
-- - Error
create trigger if not exists trigger2
  after insert on bar
begin
  delete from bar where rate > new.id;
end;

-- TEST: verify that duplicate index  with different "IF NOT EXISTS" is still ok
-- + {create_index_stmt}: ok
-- + {int 2}
-- - Error
create index if not exists index_1 on foo(id);

-- TEST: verify blob literal semantic type
-- + {select_stmt}: select: { _anon: blob notnull }
-- + {blob x'FAB1'}: blob notnull
-- - Error
select x'FAB1';

-- TEST: verify that blob literals can be copied in a tree copy op
-- This sets up a fragment with a blob literal in it that has to be copied
-- - Error
@attribute(cql:base_fragment=blob_stuff)
create proc blob_base_fragment(id_ integer not null)
begin
  with
    blob_stuff(*) as (select id_ id, x'123456' bl)
  select * from blob_stuff;
end;

-- TEST: verify that blob literals can be copied in a tree copy op
-- This sets up a fragment with a blob literal in it that has to be copied
-- - Error
@attribute(cql:extension_fragment=blob_stuff)
create proc blob_ext_fragment(id_ integer not null)
begin
  with
    blob_stuff(*) as (select 1 id, x'123456' bl),
    blob_ext(*) as (select blob_stuff.*, T2.* from blob_stuff left outer join (select x'abcd' bl2) as T2)
  select * from blob_ext;
end;

-- TEST: verify that blob literals can be copied in a tree copy op
-- This fragment had to copy blob literals to create this text which verifies the tree copy is ok
-- + @ATTRIBUTE(cql:assembly_fragment=blob_stuff)
-- + CREATE PROC blob_stuff (id_ INTEGER NOT NULL)
-- + BEGIN
-- +   WITH
-- +   blob_stuff (id, bl) AS (SELECT id_ AS id, x'123456' AS bl),
-- +   blob_ext (id, bl, bl2) AS (SELECT blob_stuff.*, T2.*
-- +   FROM blob_stuff
-- +     LEFT OUTER JOIN (SELECT x'abcd' AS bl2) AS T2)
-- +   SELECT *
-- +     FROM blob_ext;
-- + END;
-- - Error
@attribute(cql:assembly_fragment=blob_stuff)
create proc blob_stuff(id_ integer not null)
begin
  with
    blob_stuff(*) as (select 1 id, x'123456' bl)
  select * from blob_stuff;
end;

-- TEST: blob literals are good in SQL only
-- + {assign}: err
-- + {blob x'12abcdef'}: err
-- + Error % blob literals may only appear in the context of a SQL statement
-- +1 Error
create proc blob_literal_out(out b blob)
begin
  set b := x'12abcdef';
end;

-- TEST: test nullif with one param
-- + {select_stmt}: err
-- + {call}: err
-- + {name nullif}: err
-- + Error % function got incorrect number of arguments 'nullif'
-- +1 Error
select nullif(id) from bar;

-- TEST: test nullif with non null integer column table
-- + {select_stmt}: select: { id: integer }
-- + {call}: id: integer
-- + {name nullif}: id: integer
-- + {name id}: id: integer notnull
-- - Error
select nullif(id, 1) from bar;

-- TEST: kind preserved and matches
-- + {select_stmt}: select: { price_d: real<dollars> variable }
-- + {call}: price_d: real<dollars> variable
-- - Error
select nullif(price_d, price_d);

-- TEST: kind preserved and doesn't match -> error
-- + {select_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'dollars' vs. 'euros'
-- +1 Error
select nullif(price_d, price_e);

-- TEST: test nullif with incompatble type
-- + {select_stmt}: err
-- + Error % incompatible types in expression 'NULLIF'
-- +1 Error
select id, nullif(name, 1) from bar;

-- TEST: nullif may not appear outside of a SQL statement
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'nullif'
set a_string := nullif('x', 1);

-- TEST: test nullif with sensitive value
-- + {select_stmt}: select: { name: text sensitive }
-- + {call}: name: text sensitive
-- + {name nullif}: name: text sensitive
-- + {name name}: name: text sensitive
-- - Error
select nullif(name, 'a') from with_sensitive;

-- TEST: declare a select function with name match SQLite function.
-- + {declare_select_func_stmt}: err
-- + Error % select function does not require a declaration, it is a CQL built-in 'nullif'
-- +1 Error
declare select function nullif(value INT, defaultValue int not null) int;

-- TEST: test upper with sensitive value
-- + {select_stmt}: select: { _anon: text sensitive }
-- + {call}: text sensitive
-- + {name upper}: text sensitive
-- + {name name}: name: text sensitive
-- - Error
select upper(name) from with_sensitive;

-- TEST: test upper with incompatible param type
-- + {select_stmt}: err
-- + {call}: err
-- + {name upper}
-- + Error % first argument must be a string in function 'upper'
-- +1 Error
select upper(id) from bar;

-- TEST: test upper with incompatible param count
-- + {select_stmt}: err
-- + {call}: err
-- + {name upper}
-- + Error % function got incorrect number of arguments 'upper'
-- +1 Error
select upper(name, 1) from bar;

-- TEST: upper may not appear outside of a SQL statement
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'upper'
set a_string := upper('x');

-- TEST: test char with sensitive value
-- + {select_stmt}: select: { id: text sensitive }
-- + {call}: id: text sensitive
-- + {name char}: id: text sensitive
-- + {name id}: id: integer
-- + {name info}: info: integer sensitive
-- - Error
select char(id, info) from with_sensitive;

-- TEST: test char with incompatible param type
-- + {select_stmt}: err
-- + {call}: err
-- + {name name}: name: text
-- + Error % char function arguments must be integer 'char'
-- +1 Error
select char(name) from bar;

-- TEST: test char with incompatible param count
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'char'
-- +1 Error
select char() from bar;

-- TEST: char may not appear outside of a SQL statement
-- + {assign}: err
-- + {call}: err
-- + Error % function may not appear in this context 'char'
set a_string := char(1);

-- TEST: test abs with sensitive value
-- + {select_stmt}: select: { info: integer sensitive }
-- + {call}: info: integer sensitive
-- + {name abs}: info: integer sensitive
-- + {name info}: info: integer sensitive
-- - Error
select abs(info) from with_sensitive;

-- TEST: abs should preserve kind
-- + {assign}: price_d: real<dollars> variable
-- + {call}: price_d: real<dollars> variable
-- - Error
set price_d := (select abs(price_d));

-- TEST: test abs with incompatible param count
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'abs'
-- +1 Error
select abs() from bar;

-- TEST: abs may not appear outside of a SQL statement
-- + {assign}: err
-- + {call}: err
-- +1 Error % function may not appear in this context 'abs'
set an_int := abs(1);

-- TEST: test abs with non numeric param
-- + {select_stmt}: err
-- + {call}: err
-- + {name abs}
-- + Error % argument must be numeric 'abs'
-- + Error
select abs('Horty');

-- TEST: test abs with null param
-- + {select_stmt}: select: { _anon: null }
-- + {call}: null
-- + {name abs}: null
-- - Error
select abs(null);

-- TEST: instr may not appear outside of a SQL statement
-- + {assign}: err
-- + {call}: err
-- +1 Error % function may not appear in this context 'instr'
set an_int := instr(1);

-- TEST: test instr with incompatible param count
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'instr'
-- +1 Error
select instr();

-- TEST: test instr with sensitive value
-- + {select_stmt}: select: { name: integer sensitive }
-- + {call}: name: integer sensitive
-- + {name name}: name: text sensitive
-- - Error
select instr(name, 'a') from with_sensitive;

-- TEST: test instr with all param not null
-- + {select_stmt}: select: { _anon: integer notnull }
-- + {call}: integer notnull
-- +2 {strlit 'a'}: text notnull
-- - Error
select instr('a', 'a');

-- TEST: test instr with all param not null
-- + {select_stmt}: err
-- + {call}: err
-- + {name instr}
-- + Error % CQL0085: all arguments must be strings 'instr'
-- +1 Error
select instr(1, 'a');

-- TEST: you can use other literal types as values, they are just ignored
-- This is not a valid base fragment, it just gets normal processing
-- if it were a base fragment it would be full of errors.
-- + {create_proc_stmt}: ok
-- - Error
@attribute(cql:base_fragment=1)
create proc wonky_value_type(id_ integer)
begin
  declare x integer;
end;

-- TEST: no mixed or duplicate fragment type annotations are allowed
-- + Error % more than one fragment annotation on procedure 'dup_base'
-- +1 Error
@attribute(cql:base_fragment=foo)
@attribute(cql:base_fragment=goo)
create proc dup_base(id_ integer)
begin
  declare x integer;
end;

-- TEST: no mixed or duplicate fragment type annotations are allowed
-- + Error % more than one fragment annotation on procedure
-- +1 Error
@attribute(cql:extension_fragment=foo)
@attribute(cql:base_fragment=goo)
create proc mixed_frag_types1(id_ integer)
begin
  declare x integer;
end;

-- TEST: no mixed or duplicate fragment type annotations are allowed
-- + Error % more than one fragment annotation on procedure
-- +1 Error
@attribute(cql:extension_fragment=foo)
@attribute(cql:assembly_fragment=goo)
create proc mixed_frag_types2(id_ integer)
begin
  declare x integer;
end;

-- TEST: no mixed or duplicate fragment type annotations are allowed
-- + Error % more than one fragment annotation on procedure
-- +1 Error
@attribute(cql:assembly_fragment=foo)
@attribute(cql:base_fragment=goo)
create proc mixed_frag_types3(id_ integer)
begin
  declare x integer;
end;

-- TEST: use the wrong name for the assembly fragment
-- + {create_proc_stmt}: err
-- + Error % the name of the assembly procedure must match the name of the base fragment 'wrong_assembly_frag_name'
-- +1 Error
@attribute(cql:assembly_fragment=core)
create proc wrong_assembly_frag_name(id_ integer not null)
begin
  with
    core(x,y,z) as (select id,name,rate from bar where id = id_)
  select * from core;
end;

-- TEST: base_fragment attribute, not using '*' (no from clause)
-- + Error % fragment must end with exactly 'SELECT * FROM core'
-- +1 Error
@attribute(cql:base_fragment=core)
create proc base_frag_broken_1(id_ integer not null)
begin
  with
    core(*) as (select * from foo)
  select 1 x;
end;

-- TEST: base_fragment attribute, selecting from not a table
-- + Error % fragment must end with exactly 'SELECT * FROM core'
-- +1 Error
@attribute(cql:base_fragment=core)
create proc base_frag_broken_2(id_ integer not null)
begin
  with
    core(*) as (select * from foo)
  select * from (select 1 x) y;
end;

-- TEST: enable normal join mode
@enforce_normal join;

-- TEST: base_fragment attribute, selecting from two tables
-- + Error % fragment must end with exactly 'SELECT * FROM core'
-- +1 Error
@attribute(cql:base_fragment=core)
create proc base_frag_broken_3(id_ integer not null)
begin
  with
    core(*) as (select * from foo)
  select * from foo, (select 1 xyz);
end;

-- TEST: base_fragment attribute, selecting from the wrong table
-- + Error % fragment must end with exactly 'SELECT * FROM core'
-- +1 Error
@attribute(cql:base_fragment=core)
create proc base_frag_broken_4(id_ integer not null)
begin
  with
    core(*) as (select * from foo)
  select * from foo inner join (select 2 x) T;
end;

-- TEST: refer to non-existent table in an fk
-- + {create_table_stmt}: err
-- + Error % foreign key refers to non-existent table 'this_table_does_not_exist'
-- +1 Error
-- the @delete is necessary so that there will be table flags
create table bogus_reference_in_fk(
  col1 text,
  col2 int,
  foreign key(col2) references this_table_does_not_exist(col1) on update cascade on delete cascade
) @delete(1);

-- - Error
@enforce_strict procedure;

-- TEST: try to call an undeclared proc while in strict mode
-- + Error % calls to undeclared procedures are forbidden if strict procedure mode is enabled 'some_external_thing'
-- +1 Error
call some_external_thing();

-- - Error
@enforce_normal procedure;

-- TEST: same call in non stict mode -> fine
-- - Error
call some_external_thing();

-- a proc with a return type for use
declare proc _stuff() (id integer, name text);

-- TEST: type list base case, simple replacement
-- checking the rewrite (that's all that matters here)
-- + DECLARE PROC _stuff1 () (id INTEGER, name TEXT);
-- - Error
declare proc _stuff1() (like _stuff);

-- TEST: type list insert in the middle of some other args, and dedupe
-- checking the rewrite (that's all that matters here)
-- + DECLARE PROC _stuff2 () (h1 INTEGER, id INTEGER, name TEXT, t1 INTEGER);
-- - Error
declare proc _stuff2() ( h1 integer, like _stuff1, like _stuff, t1 integer);

-- TEST: type list insert in the middle of some other args, and dedupe
-- checking the rewrite (that's all that matters here)
-- + DECLARE PROC _stuff3 () (h2 INTEGER, h1 INTEGER, id INTEGER, name TEXT, t1 INTEGER, t2 INTEGER);
-- - Error
declare proc _stuff3() ( h2 integer, like _stuff2, t2 integer);

-- TEST: try to make a name list from a bogus type
-- {declare_proc_stmt}: err
-- + Error % must be a cursor, proc, table, or view 'invalid_type_name'
-- +1 Error
declare proc _stuff4() (like invalid_type_name);

-- TEST: rewrite with formal name, formals all duplicated with no qualifier
-- + DECLARE PROC _stuff5 () (id INTEGER, name TEXT);
-- - Error
declare proc _stuff5() (like _stuff1, like _stuff1);

-- TEST: rewrite with formal name for each shape
-- + DECLARE PROC _stuff6 () (x_id INTEGER, x_name TEXT, y_id INTEGER, y_name TEXT);
-- - Error
declare proc _stuff6() (x like _stuff1, y like _stuff1);

-- TEST: access shape args using dot notation
-- + {dot}: x_id: integer variable
create proc using_like_shape(x like _stuff1)
begin
  call printf("%s\n", x.id);
end;

-- TEST: access invald shape args using dot notation
-- + Error % field not found in shape 'xyzzy'
-- +1 Error
create proc using_like_shape_bad_name(x like _stuff1)
begin
  call printf("%s\n", x.xyzzy);
end;

-- TEST try to pass some of my args along
-- + CREATE PROC arg_shape_forwarder (args_arg1 INTEGER, args_arg2 TEXT, extra_args_id INTEGER, extra_args_name TEXT)
-- + CALL proc2(args.arg1, args.arg2);
-- - Error
create proc arg_shape_forwarder(args like proc2 arguments, extra_args like _stuff1)
begin
  call proc2(from args);
end;

-- create a table in the future
-- - Error
create table from_the_future(
  col1 text primary key
) @create(5);

-- TEST: trying to reference the future in an FK is an error
-- + {create_table_stmt}: err
-- + Error % referenced table was created in a later version so it cannot be used in a foreign key 'from_the_future'
-- +1 Error
create table in_the_past(
  col1 text,
  foreign key (col1) references from_the_future(col1)
) @create(4);

-- TEST: ok to reference in the same version
-- + {create_table_stmt}: in_the_future: { col1: text foreign_key } @create(5)
-- - Error
create table in_the_future(
  col1 text,
  foreign key (col1) references from_the_future(col1)
) @create(5);

-- Set up a proc we could call
-- - Error
declare proc basic_source() out union (id integer, name text);

-- TEST: this proc should be OUT not OUT UNION
-- + {create_proc_stmt}: C: basic_source: { id: integer, name: text } variable dml_proc shape_storage uses_out
-- - {create_proc_stmt}: % uses_out_union
-- - Error
create proc basic_wrapper_out()
begin
  declare C cursor for call basic_source();
  fetch C;
  out C;
end;

-- TEST: this proc should be OUT not OUT UNION
-- + {create_proc_stmt}: C: basic_source: { id: integer, name: text } variable dml_proc shape_storage uses_out_union
-- - {create_proc_stmt}: % uses_out %uses_out_union
-- - {create_proc_stmt}: % uses_out_union %uses_out
-- - Error
create proc basic_wrapper_out_union()
begin
  declare C cursor for call basic_source();
  fetch C;
  out union C;
end;

-- TEST: simple self reference
-- + {create_table_stmt}: self_ref1: { id: integer notnull primary_key, id2: integer foreign_key }
-- - Error
create table self_ref1(
 id integer primary key,
 id2 integer references self_ref1(id)
);

-- TEST: simple self reference with constraint notation
-- + {create_table_stmt}: self_ref2: { id: integer notnull primary_key, id2: integer foreign_key }
-- - Error
create table self_ref2(
 id integer primary key,
 id2 integer,
 foreign key (id2) references self_ref2(id)
);

-- TEST: refer to a column in myself -- column does not exist
-- + {create_table_stmt}: err
-- + Error % name not found 'idx'
-- +1 Error
create table self_ref3(
 id integer primary key,
 id2 integer references self_ref3(idx)
);

-- TEST: refer to a column in myself -- column does not exist -- via constraint
-- + {create_table_stmt}: err
-- + Error % name not found 'idx'
-- +1 Error
create table self_ref4(
 id integer primary key,
 id2 integer,
 foreign key (id2) references self_ref4(idx)
);

-- TEST: refer to a column in myself -- column not a key -- via constraint
-- + {create_table_stmt}: err
-- + Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'self_ref5'
-- +1 Error
create table self_ref5(
 id integer primary key,
 id2 integer,
 foreign key (id2) references self_ref5(id2)
);

-- TEST: refer to a table id that isn't a part of a PK/UK via the attribute
-- + {create_table_stmt}: err
-- + Error % the set of columns referenced in the foreign key statement should match exactly a unique key in the parent table 'self_ref2'
-- +1 Error
create table fk_to_non_key(
 id integer references self_ref2(id2)
);

-- TEST: make sure we can parse the dummy test params that include null
-- + {create_proc_stmt}: select: { id: integer notnull, id2: integer } dml_proc
-- + | {null}: ok
-- - Error
@attribute(cql:autotest=((dummy_test, (self_ref1, (id, id2), (1, null), (2, 1)))))
create proc self_ref_proc_table()
begin
  select * from self_ref1;
end;

-- TEST: test ok_scan_table attribution
-- + {stmt_and_attr}: ok
-- + {misc_attrs}: ok
-- + {name cql}
-- + {name ok_table_scan}
-- + {name foo}: ok
-- - Error
@attribute(cql:ok_table_scan=foo)
create proc ok_table_scan()
begin
  select * from foo;
end;

-- TEST: test list of value for ok_scan_table attribution
-- + {stmt_and_attr}: err
-- + {misc_attrs}: err
-- + {name cql}
-- + {name ok_table_scan}
-- + {name foo}: ok
-- + {int 1}: err
-- + Error % ok_table_scan attribute must be a name
-- +1 Error
@attribute(cql:ok_table_scan=(foo, 1))
create proc ok_table_scan_value()
begin
  select * from foo;
end;

-- TEST: bogus table name in ok_scan_table attribution
-- + misc_attrs}: err
-- + {name bogus}: err
-- + {name foo}
-- Error % the table name in ok_table_scan does not exist 'bogus'
-- +1 Error
@attribute(cql:ok_table_scan=bogus)
@attribute(cql:attr)
create proc ok_table_scan_bogus()
begin
  select * from foo;
end;

-- TEST: bogus integer in ok_scan_table attribution
-- + misc_attrs}: err
-- + {int 1}: err
-- Error %  ok_table_scan attribute must be a name
-- +1 Error
@attribute(cql:ok_table_scan=1)
create proc ok_table_scan_value_int()
begin
  select * from foo;
end;

-- TEST: ok_scan_table attribution not on a create proc statement
-- + misc_attrs}: err
-- + {select_stmt}: err
-- Error %  ok_table_scan attribute can only be used in a create procedure statement
-- +1 Error
@attribute(cql:ok_table_scan=foo)
select * from foo;

-- TEST: no_scan_table attribution is not on create table node
-- + {stmt_and_attr}: err
-- + {misc_attrs}: err
-- + {select_stmt}: err
-- Error % no_table_scan attribute may only be added to a create table statement
-- +1 Error
@attribute(cql:no_table_scan)
select * from foo;

-- TEST: no_scan_table attribution on create table node
-- + {stmt_and_attr}: ok
-- + {misc_attrs}: ok
-- - Error
@attribute(cql:no_table_scan)
create table no_table_scan(id text);

-- TEST: no_scan_table attribution with a value
-- + {stmt_and_attr}: err
-- + {misc_attrs}: err
-- + {select_stmt}: err
-- + {int 1}: err
-- Error % a value should not be assigned to no_table_scan attribute
-- +1 Error
@attribute(cql:no_table_scan=1)
select * from foo;

-- TEST: test select with values clause
-- + {select_stmt}: values: { column1: integer notnull }
-- + {select_core_list}: values: { column1: integer notnull }
-- + {select_core}: values: { column1: integer notnull }
-- + {values}: values: { column1: integer notnull }
-- + {int 1}: integer notnull
-- - Error
values (1);

-- TEST: test select with values clause (multi row values)
-- + {select_stmt}: values: { column1: integer notnull }
-- + {values}: values: { column1: integer notnull }
-- + {int 1}: integer notnull
-- + {int 5}: integer notnull
-- - Error
values (1), (5);

-- TEST: test sensitive value carry on in values clause
-- + {select_stmt}: values: { column1: integer sensitive }
-- + {values}: values: { column1: integer sensitive }
-- + {name _sens}: _sens: integer variable sensitive
-- - Error
values (1), (_sens);

-- TEST: number of column values not identical in values clause
-- + {select_stmt}: err
-- + {values}: err
-- + {dbl 4.5}: err
-- + Error % the number of columns values for each row should be identical in VALUES clause
-- +1 Error
values (1), (3, 4.5);

-- TEST: incompatible types in values clause
-- + {select_stmt}: err
-- + {values}: err
-- + {int 1}: err
-- + Error % incompatible types in expression 'VALUES clause'
-- +1 Error
values ("ok"), (1);

-- TEST: test values clause compounded in insert stmt with dummy_seed
-- + {insert_stmt}: err
-- + Error % @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row
-- +1 Error
insert into foo (id) values (1) union values(2) @dummy_seed(1);

-- TEST: test values from a with statement, and seed, this not a supported form
-- + {insert_stmt}: err
-- + Error % @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row
-- +1 Error
insert into foo with T(x) as (values (1), (2), (3)) select * from T @dummy_seed(1);

-- TEST: test values from a with statement, no seed, this is fine.
-- + {insert_stmt}: ok
-- + {with_select_stmt}: select: { x: integer notnull }
insert into foo with T(x) as (values (1), (2), (3)) select * from T;

-- TEST: test values from simple select statement, and seed, this not a supported form
-- + {insert_stmt}: err
-- + Error % @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row
-- +1 Error
insert into foo select 1 @dummy_seed(1);

-- TEST: test multi row values in values clause with dummy_seed
-- + {insert_stmt}: err
-- + Error % @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row
-- +1 Error
insert into foo (id) values (1), (2) @dummy_seed(1);

-- TEST: test invalid expr in values clause
-- + {insert_stmt}: err
-- + {name bogus}: err
-- + Error % name not found 'bogus'
-- +1 Error
insert into foo values (bogus) @dummy_seed(1);

-- TEST: test null type expr in values clause with dummy_seed
-- + {insert_stmt}: ok
-- + {columns_values}: ok
-- + {null}: null
-- - Error
insert into foo values (null) @dummy_seed(1);

-- TEST: test incompatible type in values clause with dummy_seed
-- + {insert_stmt}: err
-- + {strlit 'k'}: err
-- + Error % incompatible types in expression 'id'
-- +1 Error
insert into foo values ("k") @dummy_seed(1);

-- TEST: test invalid expr in values clause
-- + {select_stmt}: err
-- + {values}: err
-- + Error % name not found 'l'
-- + {name l}: err
-- +1 Error
values (l);

-- TEST: test insert statement with compound select
-- + {insert_stmt}: ok
-- + {select_stmt}: UNION ALL: { column1: integer notnull }
-- + {select_core}: values: { column1: integer notnull }
-- + {select_core}: select: { column1: integer notnull }
-- - Error
insert into foo values (1) union all select 2 column1;

-- TEST: test multi row values in values clause with dummy_seed
-- + {insert_stmt}: err
-- + Error % @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row
-- +1 Error
insert into foo (id) values (1), (2) @dummy_seed(1);

-- TEST: number of column in second row is not correct in values clause
-- + {select_stmt}: err
-- + {values}: err
-- + {int 10}: err
-- + Error % the number of columns values for each row should be identical in VALUES clause
-- +1 Error
values (1, 2), (10);

-- TEST: test invalid value in second row in values clause
-- + {select_stmt}: err
-- + {values}: err
-- + {strlit 'ok'}: text notnull
-- + {name bogus}: err
-- + Error % name not found 'bogus'
-- +1 Error
values ("ok"), (bogus);

-- TEST: basic table to test columns in wrong order in the insert statement
create table values_table(
  id integer PRIMARY KEY AUTOINCREMENT,
  name text
);

-- TEST: test columns in wrong order in insert statement.
-- + {insert_stmt}: ok
-- + {select_stmt}: values: { column1: text notnull, column2: null }
insert into values_table(name, id) values ("ok", null);

-- TEST: enforce strict without rowid
-- + @ENFORCE_STRICT WITHOUT ROWID;
-- + {enforce_strict_stmt}: ok
-- + {int 7}
-- - Error
@enforce_strict without rowid;

-- TEST: without rowid failed validation in strict mode
-- + {create_table_stmt}: err
-- + Error % WITHOUT ROWID tables are forbidden if strict without rowid mode is enabled 'table_with_invalid_without_rowid_mode'
-- +1 Error
create table table_with_invalid_without_rowid_mode(
  id integer primary key
) without rowid;

-- TEST: enforcement normal without rowid
-- + @ENFORCE_NORMAL WITHOUT ROWID;
-- + {enforce_normal_stmt}: ok
-- + {int 7}
@enforce_normal without rowid;

-- TEST: without rowid succeed validation in normal mode
-- + {create_table_stmt}: table_with_valid_without_rowid_mode: { id: integer notnull primary_key }
-- - Error
create table table_with_valid_without_rowid_mode(
  id integer primary key
) without rowid;

-- TEST: negating 9223372036854775808L requires first representing the positive value
-- this value does not fit in 64 bits signed.  As a consequence the numeric representation
-- of integers cannot just be an int64_t.  To avoid all these problems and more we
-- simply hold the string value of the integer as the need for math is very limited, nearly zero
-- anyway due to lack of constant folding and whatnot.
-- the text in the comment has the original string with the L
-- the positive version of the integer does not and there is
-- no kidding around negation going on here.
-- + SELECT -9223372036854775808L AS x;
-- + {create_proc_stmt}: select: { x: longint notnull } dml_proc
-- + {uminus}: longint notnull
-- + {longint 9223372036854775808}: longint notnull
CREATE PROC min_int_64_test ()
BEGIN
  SELECT -9223372036854775808L AS x;
END;

-- TEST: complex floating point and integer literals
-- first verify round trip through the AST
-- + SELECT 2147483647 AS a, 2147483648L AS b, 3.4e11 AS c, .001e+5 AS d, .4e-9 AS e;
-- + {int 2147483647}: integer notnull
-- + {longint 2147483648}: longint notnull
-- + {dbl 3.4e11}: real notnull
-- + {dbl .001e+5}: real notnull
-- + {dbl .4e-9}: real notnull
create proc exotic_literals()
begin
  select 2147483647 a, 2147483648 b,  3.4e11 c, .001e+5 d, .4e-9 e;
end;

-- TEST: hex literal processing
-- + SELECT 0x13aF AS a, 0x234L AS b, 0x123456789L AS c;
-- + {int 0x13aF}: integer notnull
-- + {longint 0x234}: longint notnull
-- + {longint 0x123456789}: longint notnull
create proc hex_literals()
begin
  select 0x13aF a, 0x234L b,  0x123456789 c;
end;

-- a type shape we will use for making args and cursors
declare proc shape() (x integer not null, y text not null);

-- just one column of the type, we'll use this to call with a slice of the cursor
declare proc small_shape() (y text not null);

-- some procedure we can call
declare proc shape_consumer(like shape);

-- TEST: try to call shape_consumer from a suitable cursor
-- This is strictly a rewrite so all we have to do here is make sure that we are calling the proc correctly
-- + CALL shape_consumer(C.x, C.y);
-- - Error
create proc shape_all_columns()
begin
   declare C cursor like shape;
   fetch C from values(1, 'x');
   call shape_consumer(from C);
end;

-- TEST: try to call shape_consumer from not a cursor...
-- This is strictly a rewrite so all we have to do here is make sure that we are calling the proc correctly
-- + {create_proc_stmt}: err
-- + {call_stmt}: err
-- + Error % cursor not found 'not_a_cursor'
-- +1 Error
create proc shape_thing_bogus_cursor()
begin
   call shape_consumer(from not_a_cursor);
end;

-- TEST: try to call shape_consumer using a statement cursor.  This is bogus...
-- + {create_proc_stmt}: err
-- + {call_stmt}: err
-- + Error % Cursor was not used with 'fetch [cursor]' 'C'
-- +1 Error
create proc shape_some_columns_statement_cursor()
begin
   declare C cursor for select 1 x, 'y' y;
   call shape_consumer(from C);
end;

declare proc shape_y_only(like small_shape);

-- TEST: try to call shape_y_only using the LIKE form
-- This is strictly a rewrite so all we have to do here is make sure that we are calling the proc correctly
-- + CALL shape_y_only(C.y);
-- - Error
create proc shape_some_columns()
begin
   declare C cursor like shape;
   fetch C(x, y) from values(1, 'x');
   call shape_y_only(from C like small_shape);
end;

-- TEST: try to call shape_y_only using the LIKE form with bogus like name
-- + Error % must be a cursor, proc, table, or view 'not_a_real_shape'
-- +1 Error
create proc shape_some_columns_bogus_name()
begin
   declare C cursor like shape;
   fetch C(x, y) from values(1, 'x');
   call shape_y_only(from C like not_a_real_shape);
end;

declare proc lotsa_ints(a integer not null, b integer not null, c integer not null, d integer not null);

-- TEST: try inserting arguments into the middle of the arg list
-- + CALL lotsa_ints(C.x, C.y, 1, 2);
-- + CALL lotsa_ints(1, C.x, C.y, 2);
-- + CALL lotsa_ints(1, 2, C.x, C.y);
-- + CALL lotsa_ints(C.x, C.y, C.x, C.y);
-- - Error
create proc shape_args_middle()
begin
   declare C cursor like select 1 x, 2 y;
   fetch C from values(1, 2);
   call lotsa_ints(from C, 1, 2);
   call lotsa_ints(1, from C, 2);
   call lotsa_ints(1, 2, from C);
   call lotsa_ints(from C, from C);
end;

-- TEST: try a variety of standard arg replacements
-- Just rewrites to verify
-- +  CALL lotsa_ints(x, y, 1, 2);
-- +  CALL lotsa_ints(1, x, y, 2);
-- +  CALL lotsa_ints(1, 2, x, y);
-- +  CALL lotsa_ints(x, y, x, y);
-- - Error
create proc arg_rewrite_simple(x integer not null, y integer not null)
begin
   call lotsa_ints(from arguments, 1, 2);
   call lotsa_ints(1, from arguments, 2);
   call lotsa_ints(1, 2, from arguments);
   call lotsa_ints(from arguments, from arguments);
end;

-- TEST: try from arguments with no arguments
-- + Error % FROM ARGUMENTS used in a procedure with no arguments 'arg_rewrite_no_args'
-- +1 Error
create proc arg_rewrite_no_args()
begin
   call lotsa_ints(from arguments, 1, 2);
end;

-- TEST: try to use from arguments outside of any procedure
-- + Error % FROM ARGUMENTS construct is only valid inside a procedure
-- +1 Error
call lotsa_ints(from arguments, 1, 2);

-- TEST: try a variety of standard arg replacements with type constraint
-- Just rewrites to verify
-- +  CALL lotsa_ints(y, 1, 2, 3);
-- +  CALL lotsa_ints(1, y, 2, 3);
-- +  CALL lotsa_ints(1, 2, y, 3);
-- +  CALL lotsa_ints(1, 2, 3, y);
-- +  CALL lotsa_ints(y, y, y, y);
-- - Error
create proc arg_rewrite_with_like(x integer not null, y integer not null)
begin
   call lotsa_ints(from arguments like small_shape, 1, 2, 3);
   call lotsa_ints(1, from arguments like small_shape, 2, 3);
   call lotsa_ints(1, 2, from arguments like small_shape, 3);
   call lotsa_ints(1, 2, 3, from arguments like small_shape);
   call lotsa_ints(from arguments like small_shape,
                   from arguments like small_shape,
                   from arguments like small_shape,
                   from arguments like small_shape);
end;

-- TEST: try a variety of standard arg replacements with type constraint
--       this version matches the arg with a trailing underscore
-- Just rewrites to verify
-- +  CALL lotsa_ints(y_, 1, 2, 3);
-- +  CALL lotsa_ints(1, y_, 2, 3);
-- +  CALL lotsa_ints(1, 2, y_, 3);
-- +  CALL lotsa_ints(1, 2, 3, y_);
-- +  CALL lotsa_ints(y_, y_, y_, y_);
-- - Error
create proc arg_rewrite_with_like_with_underscore(x integer not null, y_ integer not null)
begin
   call lotsa_ints(from arguments like small_shape, 1, 2, 3);
   call lotsa_ints(1, from arguments like small_shape, 2, 3);
   call lotsa_ints(1, 2, from arguments like small_shape, 3);
   call lotsa_ints(1, 2, 3, from arguments like small_shape);
   call lotsa_ints(from arguments like small_shape,
                   from arguments like small_shape,
                   from arguments like small_shape,
                   from arguments like small_shape);
end;

-- TEST: try a variety of standard arg replacements with type constraint
--       this version matches the arg with a trailing underscore
--       this version also writes more than one column
-- Just rewrites to verify
-- +  CALL lotsa_ints(x_, y_, 1, 2);
-- +  CALL lotsa_ints(1, x_, y_, 2);
-- +  CALL lotsa_ints(1, 2, x_, y_);
-- +  CALL lotsa_ints(x_, y_, x_, y_);
-- - Error
create proc arg_rewrite_with_like_many_cols_with_underscore(x_ integer not null, y_ integer not null)
begin
   call lotsa_ints(from arguments like shape, 1, 2);
   call lotsa_ints(1, from arguments like shape, 2);
   call lotsa_ints(1, 2, from arguments like shape);
   call lotsa_ints(from arguments like shape, from arguments like shape);
end;

-- TEST: try to do from arguments with a type but there is no matching arg
-- + {call_stmt}: err
-- + {expr_list}: err
-- + Error % expanding FROM ARGUMENTS, there is no argument matching 'id'
-- +1 Error
create proc call_with_missing_type(x integer)
begin
  -- the table foo has a column 'id' but we have no such arg
  call lotsa_ints(1, 2, 3, from arguments like foo);
end;

-- TEST: try to do from arguments with a type but there is no such type
-- + {call_stmt}: err
-- + {expr_list}: err
-- + Error % must be a cursor, proc, table, or view 'no_such_type_dude'
-- +1 Error
create proc call_from_arguments_bogus_type(x integer)
begin
  -- the table foo has a column 'id' but we have no such arg
  call lotsa_ints(1, 2, 3, from arguments like no_such_type_dude);
end;

-- this procedure ends with an out arg, can be called as a function
declare proc funclike(like shape, out z integer not null);

-- TEST: use argument expansion in a function call context
-- This is strictly a rewrite
-- + CREATE PROC arg_caller (x_ INTEGER NOT NULL, y_ TEXT NOT NULL, OUT z INTEGER NOT NULL)
-- + SET z := funclike(x_, y_);
-- - Error
create proc arg_caller(like shape, out z integer not null)
begin
   set z := funclike(from arguments like shape);
end;

-- TEST: use argument expansion in a function call context
-- + Error % must be a cursor, proc, table, or view 'not_a_shape'
-- +1 Error
-- + {call}: err
-- + {arg_list}: err
-- + {name not_a_shape}: err
-- from arguments not replaced because the rewrite failed
-- + {from_shape}
create proc arg_caller_bogus_shape(like shape, out z integer not null)
begin
   set z := funclike(from arguments like not_a_shape);
end;

-- TEST: @proc in bad context (assign)
-- + {assign}: err
-- + Error % @PROC literal can only appear inside of procedures
-- +1 Error
set a_string := @PROC;

-- TEST: @proc in bad context (savepoint)
-- + {savepoint_stmt}: err
-- + Error
savepoint @proc;

-- TEST: @proc in bad context (release)
-- + {release_savepoint_stmt}: err
-- + Error % @PROC literal can only appear inside of procedures
-- +1 Error
release savepoint @proc;

-- TEST: @proc in bad context (rollback)
-- + {rollback_trans_stmt}: err
-- + Error % @PROC literal can only appear inside of procedures
-- +1 Error
rollback transaction to savepoint @proc;

-- TEST: @proc rewrites
-- + SET p := 'savepoint_proc_stuff';
-- + SAVEPOINT savepoint_proc_stuff;
-- + ROLLBACK TO savepoint_proc_stuff;
-- + RELEASE savepoint_proc_stuff;
-- - Error
create proc savepoint_proc_stuff()
begin
  declare p text;
  set p := @proc;
  savepoint @proc;
  rollback transaction to savepoint @proc;
  release savepoint @proc;
end;

-- TEST: call cql_cursor_diff_col with non variable arguments
-- + {assign}: err
-- + {call}: err
-- + Error % argument must be a variable in function 'cql_cursor_diff_col'
-- +1 Error
set a_string := cql_cursor_diff_col(1, "bogus");

-- TEST: call cql_cursor_diff_col with invalid variable arguments
-- + {assign}: err
-- + {call}: err
-- + Error % variable is not a cursor 'an_int'
-- +1 Error
set a_string := cql_cursor_diff_col(an_int, an_int2);

-- TEST: call cql_cursor_diff_col with incorrect number of arguments
-- + {assign}: err
-- + {call}: err
-- + {name cql_cursor_diff_col}: err
-- + Error % function got incorrect number of arguments 'cql_cursor_diff_col'
-- +1 Error
set a_string := cql_cursor_diff_col(an_int, an_int2, 1);

-- TEST: call cql_cursor_diff_val with incorrect number of arguments
-- + {assign}: err
-- + {call}: err
-- + {name cql_cursor_diff_val}: err
-- + Error % function got incorrect number of arguments 'cql_cursor_diff_val'
-- +1 Error
set a_string := cql_cursor_diff_val(an_int, an_int2, 1);

-- TEST: call cql_cursor_diff_col with cursor with fetch value and same shape
-- + {create_proc_stmt}: err
-- + {assign}: err
-- + {call}: err
-- + {name c1}: err
-- + Error % cursor was not used with 'fetch [cursor]' 'c1'
-- +1 Error
create proc cql_cursor_diff_col_without_cursor_arg()
begin
  declare c1 cursor for select 1 x, 'y' y;
  declare c2 cursor for select 1 x, 'y' y;
  fetch c2;
  set a_string := cql_cursor_diff_col(c1, c2);
end;

-- TEST: call cql_cursor_diff_col with incompatible cursor types
-- + {create_proc_stmt}: err
-- + {assign}: err
-- + {call}: err
-- + {name c1}: err
-- + Error % in cql_cursor_diff_col, all columns must be an exact type match (expected integer notnull; found text notnull) 'x'
-- +1 Error
create proc cql_cursor_diff_col_wrong_cursor_type()
begin
  declare c1 cursor for select 1 x;
  declare c2 cursor for select '1' x;
  fetch c1;
  fetch c2;
  set a_string := cql_cursor_diff_col(c1, c2);
end;

-- TEST: call cql_cursor_diff_col with invalid column count arguments
-- + {create_proc_stmt}: err
-- + {assign}: err
-- + {call}: err
-- + Error % the cursor arguments must have identical column count 'cql_cursor_diff_col'
-- +1 Error
create proc cql_cursor_diff_col_with_wrong_col_count_arg()
begin
  declare c1 cursor for select 1 x, 'z' z;
  declare c2 cursor for select 1 x;
  fetch c1;
  fetch c2;
  set a_string := cql_cursor_diff_col(c1, c2);
end;

-- TEST: call cql_cursor_diff_col with valid cursor param but different column name
-- + {create_proc_stmt}: err
-- + {assign}: err
-- + {call}: err
-- + Error % in cql_cursor_diff_col, all column names must be identical so they have unambiguous names 'z'
-- +1 Error
create proc cql_cursor_diff_col_compatible_cursor_with_diff_col_name()
begin
  declare c1 cursor for select 1 x, 'y' y;
  declare c2 cursor for select 1 z, 'v' v;
  fetch c1;
  fetch c2;
  set a_string := cql_cursor_diff_col(c1, c2);
end;

-- TEST: call cql_cursor_diff_col with cursor with fetch value and same shape
-- + {create_proc_stmt}: ok dml_proc
-- + {assign}: a_string: text variable
-- + SET a_string := CASE WHEN c1.x IS NOT c2.x THEN 'x'
-- + WHEN c1.y IS NOT c2.y THEN 'y'
-- + END;
-- - Error
create proc cql_cursor_diff_col_with_shape_storage()
begin
  declare c1 cursor for select 1 x, 'y' y;
  declare c2 cursor for select 1 x, 'y' y;
  fetch c1;
  fetch c2;
  set a_string := cql_cursor_diff_col(c1, c2);
end;

-- TEST: call cql_cursor_diff_col from another func
-- + {create_proc_stmt}: ok dml_proc
-- + {call_stmt}: ok
-- + CALL printf(CASE WHEN c1.x IS NOT c2.x THEN 'x'
-- + WHEN c1.y IS NOT c2.y THEN 'y'
-- + END);
-- - Error
create proc print_call_cql_cursor_diff_col()
begin
  declare c1 cursor for select 1 x, 'y' y;
  declare c2 cursor for select 1 x, 'v' y;
  fetch c1;
  fetch c2;
  call printf(cql_cursor_diff_col(c1, c2));
end;

-- TEST: call cql_cursor_diff_val from another func
-- + {create_proc_stmt}: ok dml_proc
-- + {call_stmt}: ok
-- + CALL printf(CASE WHEN c1.x IS NOT c2.x THEN printf('column:%s c1:%s c2:%s', 'x', CASE WHEN c1.x IS NULL THEN 'null'
-- + ELSE printf('%d', c1.x)
-- + END, CASE WHEN c2.x IS NULL THEN 'null'
-- + ELSE printf('%d', c2.x)
-- + END)
-- + WHEN c1.y IS NOT c2.y THEN printf('column:%s c1:%s c2:%s', 'y', CASE WHEN c1.y IS NULL THEN 'null'
-- + ELSE printf('%s', c1.y)
-- + END, CASE WHEN c2.y IS NULL THEN 'null'
-- + ELSE printf('%s', c2.y)
-- + END)
-- + END);
-- - Error
create proc print_call_cql_cursor_diff_val()
begin
  declare c1 cursor for select 1 x, 'y' y;
  declare c2 cursor for select 1 x, 'v' y;
  fetch c1;
  fetch c2;
  call printf(cql_cursor_diff_val(c1, c2));
end;

-- TEST: simple trim call (two args)
-- + {call}: text notnull
-- + {name trim}: text notnull
-- - sensitive
-- - Error
set a_string := (select trim("x", "y"));

-- TEST: simple trim call (one arg)
-- + {call}: text notnull
-- + {name trim}: text notnull
-- - sensitive
-- - Error
set a_string := (select trim("x"));

declare kind_string text<surname>;

-- TEST: substr preserves kind
-- + {select_stmt}: _anon: text<surname>
-- + {name kind_string}: kind_string: text<surname> variable
-- - Error
set a_string := (select substr(kind_string, 2, 3));

-- TEST: verify that kind is preserved
-- + {select_stmt}: _anon: text<surname>
-- + {name kind_string}: kind_string: text<surname> variable
-- - Error
set kind_string := (select trim(kind_string));

-- TEST: verify that kind is preserved
-- + {select_stmt}: _anon: text<surname>
-- + {name kind_string}: kind_string: text<surname> variable
-- - Error
set kind_string := (select upper(kind_string));

-- TEST: verify that kind is preserved
-- + {select_stmt}: _anon: text<surname>
-- + {name kind_string}: kind_string: text<surname> variable
-- - Error
set kind_string := (select lower(kind_string));

-- TEST: simple ltrim call
-- + {call}: text notnull
-- + {name ltrim}: text notnull
-- - sensitive
-- - Error
set a_string := (select ltrim("x", "y"));

-- TEST: simple rtrim call
-- + {call}: text notnull
-- + {name rtrim}: text notnull
-- - sensitive
-- - Error
set a_string := (select rtrim("x", "y"));

-- TEST: trim failure: no args
-- + {call}: err
-- + Error % function got incorrect number of arguments 'trim'
-- +1 Error
set a_string := (select trim());

-- TEST: trim failure: three args
-- + {call}: err
-- + Error % function got incorrect number of arguments 'trim'
-- +1 Error
set a_string := (select trim(1,2,3));

-- TEST: trim failure: arg 1 is not a string
-- + {call}: err
-- + Error % all arguments must be strings 'trim'
-- +1 Error
set a_string := (select trim(1,"x"));

-- TEST: trim failure: arg 2 is not a string
-- + {call}: err
-- + Error % all arguments must be strings 'trim'
-- +1 Error
set a_string := (select trim("x", 1));

-- TEST: trim failure: not in a SQL context
-- + {call}: err
-- + Error % function may not appear in this context 'trim'
-- +1 Error
set a_string := trim("x", 1);

-- TEST: trim must preserve sensitivity
-- + {call}: text sensitive
-- + {name trim}: text sensitive
-- - Error
set sens_text := (select trim(name) from with_sensitive);

-- TEST: trim must preserve sensitivity (2nd arg too, 1st arg not null)
-- + {select_stmt}: result: text notnull sensitive
-- + {call}: text notnull sensitive
-- + {name trim}: text notnull sensitive
-- - Error
set sens_text := (select trim("xyz", name) result from with_sensitive);

-- TEST: call cql_cursor_format on a auto cursor
-- + {create_proc_stmt}: ok dml_proc
-- + DECLARE c1 CURSOR FOR SELECT CAST(1 AS BOOL) AS a, 1 AS b, 99L AS c, 'x' AS d, 1.1 AS e, CAST('y' AS BLOB) AS f;
-- + FETCH c1;
-- + SET a_string := printf('a:%s|b:%s|c:%s|d:%s|e:%s|f:%s', CASE WHEN c1.a IS NULL THEN 'null'
-- + ELSE printf('%d', c1.a)
-- + END, CASE WHEN c1.b IS NULL THEN 'null'
-- + ELSE printf('%d', c1.b)
-- + END, CASE WHEN c1.c IS NULL THEN 'null'
-- + ELSE printf('%lld', c1.c)
-- + END, CASE WHEN c1.d IS NULL THEN 'null'
-- + ELSE printf('%s', c1.d)
-- + END, CASE WHEN c1.e IS NULL THEN 'null'
-- + ELSE printf('%f', c1.e)
-- + END, CASE WHEN c1.f IS NULL THEN 'null'
-- + ELSE printf('length %lld blob', cql_get_blob_size(c1.f))
-- + END);
-- - Error
create proc print_call_cql_cursor_format()
begin
  declare c1 cursor for select cast(1 as bool) a, 1 b, 99L c, 'x' d, 1.1 e, cast('y' as blob) f;
  fetch c1;
  set a_string := cql_cursor_format(c1);
end;

-- TEST: call cql_cursor_format in select context
-- + {create_proc_stmt}: err
-- + {select_stmt}: err
-- + {call}: err
-- + Error % function may not appear in this context 'cql_cursor_format'
-- +1 Error
create proc select_cql_cursor_format()
begin
  declare c1 cursor for select 1 as a;
  fetch c1;
  select cql_cursor_format(c1) as p;
end;

-- TEST: call cql_cursor_format on a not auto cursor
-- + {create_proc_stmt}: err
-- + {call}: err
-- + {name c}: err
-- + Error % cursor was not used with 'fetch [cursor]' 'c'
-- +1 Error
create proc print_call_cql_not_fetch_cursor_format()
begin
  declare c cursor for select 1;
  set a_string := cql_cursor_format(c);
end;

-- TEST: test cql_cursor_format with a non cursor params
-- + {assign}: err
-- + {call}: err
-- + Error % argument must be a variable in function 'cql_cursor_format'
-- +1 Error
set a_string := cql_cursor_format(1);

-- TEST: test cql_cursor_format with a non variable cursor
-- + {assign}: err
-- + {call}: err
-- + Error % function got incorrect number of arguments 'cql_cursor_format'
-- +1 Error
set a_string := cql_cursor_format(1, 2);

-- TEST: assigning an int64 to an int is not ok
-- + {assign}: err
-- + Error % lossy conversion from type 'LONG_INT'
-- +1 Error
set an_int := 1L;

-- TEST: assigning a real to an int is not ok
-- + {assign}: err
-- + Error % lossy conversion from type 'REAL'
-- +1 Error
set an_int := 1.0;

-- TEST: assigning a real to a long int is not ok
-- + {assign}: err
-- + Error % lossy conversion from type 'REAL'
-- +1 Error
set ll := 1.0;

-- TEST: length failure: no args
-- + {call}: err
-- + Error % function got incorrect number of arguments 'length'
-- +1 Error
set an_int := (select length());

-- TEST: length failure: arg is not a string
-- + {call}: err
-- + Error % all arguments must be strings 'length'
-- +1 Error
set an_int := (select length(1));

-- TEST: length failure: not in a SQL context
-- + {call}: err
-- + Error % function may not appear in this context 'length'
-- +1 Error
set an_int := length("x");

-- TEST: length must preserve sensitivity
-- + {call}: integer sensitive
-- + {name length}: integer sensitive
-- - Error
set _sens := (select length(name) from with_sensitive);

-- TEST: length must preserve nullability
-- + {assign}: an_int: integer variable
-- + {select_stmt}: _anon: integer notnull
-- + {call}: integer notnull
-- - Error
set an_int := (select length("x"));

-- TEST: box a cursor (success path)
-- + {name C}: C: select: { id: integer notnull, name: text, rate: longint } variable
-- + {set_from_cursor}: C: select: { id: integer notnull, name: text, rate: longint } variable boxed
-- - Error
create proc cursor_box(out B object<bar cursor>)
begin
  declare C cursor for select * from bar;
  set B from cursor C;
end;

-- TEST: unbox a cursor (success path)
-- + {declare_cursor}: C: bar: { id: integer notnull, name: text, rate: longint } variable boxed
-- + {name C}: C: bar: { id: integer notnull, name: text, rate: longint } variable boxed
-- + {name box}: box: object<bar CURSOR> variable in
-- - Error
create proc cursor_unbox(box object<bar cursor>)
begin
  declare C cursor for box;
end;

-- TEST: unbox from an object that has no type spec
-- + Error % the variable must be of type object<T cursor> where T is a valid shape name 'box'
-- +1 Error
create proc cursor_unbox_untyped(box object)
begin
  declare C cursor for box;
end;

-- TEST: unbox from an object that is not marked CURSOR
-- + Error % the variable must be of type object<T cursor> where T is a valid shape name 'box'
-- +1 Error
create proc cursor_unbox_not_cursor(box object<bar>)
begin
  declare C cursor for box;
end;

-- TEST: unbox from an object that has a type spec that isn't a valid shape
-- + Error % must be a cursor, proc, table, or view 'not_a_type'
-- +1 Error
create proc cursor_unbox_not_a_type(box object<not_a_type cursor>)
begin
  declare C cursor for box;
end;

-- TEST: unbox and attempt to redeclare the same cursor
-- + Error % duplicate variable name in the same scope 'C'
-- +1 Error
create proc cursor_unbox_duplicate(box object<bar cursor>)
begin
  declare C cursor for box;
  declare C cursor for box;
end;

-- TEST: unbox from a variable that does not exist
-- + Error % name not found 'box'
-- +1 Error
create proc cursor_unbox_not_exists()
begin
  declare C cursor for box;
end;

-- TEST: try to box a value cursor
-- + Error % the cursor did not originate from a SQLite statement, it only has values 'C'
-- +1 Error
create proc cursor_box_value(out box object<bar cursor>)
begin
  declare C cursor like bar;
  set box from cursor C;
end;

-- TEST: try to box but the type isn't a shape
-- + Error % must be a cursor, proc, table, or view 'barf'
-- +1 Error
create proc cursor_box_not_a_shape(out box object<barf cursor>)
begin
  declare C cursor for select * from bar;
  set box from cursor C;
end;

-- TEST: try to box but the type doesn't match
-- + Error % in the cursor and the variable type, all must have the same column count
-- +1 Error
create proc cursor_box_wrong_shape(out box object<foo cursor>)
begin
  declare C cursor for select * from bar;
  set box from cursor C;
end;

-- TEST: try to box but the source isnt a cursor
-- + Error % cursor not found 'XYZZY'
-- +1 Error
create proc cursor_box_not_a_cursor(out box object<foo cursor>)
begin
  set box from cursor XYZZY;
end;

-- TEST: try to box but the source isnt a cursor
-- + Error % variable not found 'box'
-- +1 Error
create proc cursor_box_var_not_found()
begin
  declare C cursor for select * from bar;
  set box from cursor C;
end;

-- TEST: test cql_get_blob_size cql builtin function
-- + {assign}: an_long: longint variable
-- + {name cql_get_blob_size}: longint
-- - Error
set an_long := cql_get_blob_size(blob_var);

-- TEST: test cql_get_blob_size with too many arguments
-- + {assign}: err
-- + {name cql_get_blob_size}: err
-- + Error % function got incorrect number of arguments 'cql_get_blob_size'
-- +1 Error
set an_long := cql_get_blob_size(blob_var, 0);

-- TEST: test cql_get_blob_size with invalid argument type
-- + {assign}: err
-- + {call}: err
-- + {name cql_get_blob_size}
-- + Error % the argument must be of type blob 'cql_get_blob_size'
-- +1 Error
set an_long := cql_get_blob_size(an_int);

-- TEST: test cql_get_blob_size used in SQL context
-- + {assign}: err
-- + {call}: err
-- + {name cql_get_blob_size}
-- + Error % function may not appear in this context 'cql_get_blob_size'
-- +1 Error
set an_long := (select cql_get_blob_size(an_int));

declare proc some_proc(id integer, t text, t1 text not null, b blob, out x integer not null);

-- TEST: make a cursor using the arguments of a procedure as the shape
-- + DECLARE Q CURSOR LIKE some_proc ARGUMENTS;
-- + {declare_cursor_like_name}: Q: some_proc[arguments]: { id: integer in, t: text in, t1: text notnull in, b: blob in, x: integer notnull in } variable shape_storage value_cursor
-- - Error
declare Q cursor like some_proc arguments;

-- TEST: make a procedure using a declared shape (rewrite test)
-- + CREATE PROC some_proc_proxy (id INTEGER, t TEXT, t1 TEXT NOT NULL, b BLOB, OUT x INTEGER NOT NULL)
-- - Error
create proc some_proc_proxy(like some_proc arguments)
begin
   call some_proc(from arguments);
end;

declare proc some_proc2(inout id integer, t text, t1 text not null, b blob, out x integer not null);

-- TEST: make a procedure using a declared shape (rewrite test)
-- + CREATE PROC some_proc2_proxy (INOUT id INTEGER, t TEXT, t1 TEXT NOT NULL, b BLOB, OUT x INTEGER NOT NULL)
-- - Error
create proc some_proc2_proxy(like some_proc2 arguments)
begin
   call some_proc(from arguments);
end;

-- TEST: there is no some_proc3 -- error
-- + CREATE PROC some_proc3_proxy (LIKE some_proc3 ARGUMENTS)
-- + Error % name not found 'some_proc3'
-- +1 Error
create proc some_proc3_proxy(like some_proc3 arguments)
begin
   call some_proc(from arguments);
end;

-- TEST: there is no some_proc3 -- error
-- + Error % LIKE ... ARGUMENTS used on a procedure with no arguments 'proc1'
-- +1 Error
create proc some_proc4_proxy(like proc1 arguments)
begin
end;

-- TEST: object arguments are not yet supported (this requires cursor generalizations)
-- + Error % the procedure has an object argument, this is not yet supported 'obj_proc'
-- +1 Error
declare invalid_object_cursor cursor like obj_proc arguments;

-- TEST: test rewrite for [FETCH [c] USING ... ] grammar
-- + {create_proc_stmt}: ok
-- + FETCH C(id, name, rate) FROM VALUES(1, NULL, 99);
-- - Error
create proc test_fetch_using()
begin
  declare C cursor like bar;
  fetch C using 1 id, NULL as name, 99 rate;
end;

-- TEST: test rewrite for [FETCH [c] USING ... ] grammar with dummy_seed
-- + {create_proc_stmt}: ok
-- + FETCH C(id, name, rate) FROM VALUES(1, printf('name_%d', _seed_), _seed_) @DUMMY_SEED(9) @DUMMY_DEFAULTS @DUMMY_NULLABLES;
-- - Error
create proc test_fetch_using_with_dummy_seed()
begin
  declare C cursor like bar;
  fetch C using 1 id @dummy_seed(9) @dummy_defaults @dummy_nullables;
end;

-- TEST: try to return object from a select function
-- + {declare_select_func_stmt}: err
-- + Error % select function may not return type OBJECT 'returns_object_is_bogus'
-- +1 Error
declare select function returns_object_is_bogus() object;

-- TEST: simple check expression -> valid case
-- + {create_table_stmt}: with_check: { id: integer, lo: integer has_check, hi: integer }
-- + {col_attrs_check}: ok
-- + {le}: bool
-- + {name lo}: lo: integer
-- + {name hi}: hi: integer
-- - Error
create table with_check
(
  id integer,
  lo integer check (lo <= hi),
  hi integer
);

-- TEST: simple check expression -> bogus identifier
-- + {create_table_stmt}: err
-- + {col_attrs_check}: err
-- + {le}: err
-- + Error % name not found 'hip'
-- +1 Error
create table with_check_bogus_column
(
  id integer,
  lo integer check (lo <= hip),
  hi integer
);

-- TEST: simple collate, no problem
-- + {create_table_stmt}: with_collate: { id: integer, t: text has_collate }
-- + {col_attrs_collate}: ok
-- - Error
create table with_collate
(
  id integer,
  t text collate garbonzo
);

-- TEST: simple collate, bogus column type
-- + {create_table_stmt}: err
-- + {col_attrs_collate}: err
-- + Error % collate applied to a non-text column 'i'
-- +1 Error
create table with_collate
(
  id integer,
  i real collate garbonzo
);

-- TEST: make sure all constraints come after all columns
-- + {create_table_stmt}: err
-- + Error % column definitions may not come after constraints 'id'
-- +1 Error
create table bad_order(
 id integer,
 primary key (id),
 t text
);

-- TEST: test rewrite for [INSERT name USING ... ] grammar
-- + {create_proc_stmt}: ok dml_proc
-- + INSERT INTO foo(id) VALUES(1);
-- - Error
create proc test_insert_using()
begin
  insert into foo using 1 id;
end;

-- TEST: test rewrite for [INSERT name USING ... ] grammar with dummy_seed
-- + {create_proc_stmt}: ok dml_proc
-- + INSERT INTO bar(id, name, rate) VALUES(1, printf('name_%d', _seed_), _seed_) @DUMMY_SEED(9) @DUMMY_DEFAULTS @DUMMY_NULLABLES
-- - Error
create proc test_insert_using_with_dummy_seed()
begin
  insert into bar using 1 id @dummy_seed(9) @dummy_defaults @dummy_nullables;
end;

-- TEST: test rewrite for [INSERT name USING ... ] grammar printed
-- + {create_proc_stmt}: err
-- note: because the proc is a duplicate it won't be further analyzed
-- which means that we get to see the printout of the proc before
-- it is rewritten so this is a test for printing the before SQL
-- not a semantic test of the rewrite.  gen_sql code is exercised here.
-- + INSERT INTO foo USING 1 AS bogus;
-- + Error % duplicate stored proc name 'test_insert_using'
-- +1 Error
create proc test_insert_using()
begin
  insert into foo using 1 bogus;
end;

-- TEST: test rewrite for IIF func
-- + {select_stmt}: select: { _anon: integer notnull }
-- + SELECT CASE WHEN an_int IS NULL THEN 3
-- + ELSE 2
-- + END;
-- - Error
select iif(an_int is null, 3, 2);

-- TEST: test rewrite for IIF func with invalid argument count
-- + {select_stmt}: err
-- + Error % function got incorrect number of arguments 'iif'
-- + Error
select iif(an_int is null, 2, 3, 4);

-- TEST: test rewrite for IIF func with invalid argument count
-- + {select_stmt}: err
-- + Error % argument must be numeric 'iif'
-- + Error
select iif('x', 2, 3);

-- TEST: test rewrite for IIF func with invalid argument count
-- + {select_stmt}: err
-- + Error % incompatible types in expression 'iif'
-- + Error
select iif(an_int is null, 2, x'23');

-- TEST: test rewrite for IIF func out of sql context
-- + {assign}: an_int: integer variable
-- + SET an_int := CASE WHEN an_int IS NULL THEN CASE WHEN 4 THEN 5
-- + ELSE 6
-- + END
-- + ELSE 2
-- + END;
-- - Error
set an_int := iif(an_int is null, iif(4, 5, 6), 2);

-- TEST: test rewrite for [UPDATE cursor USING ... ] grammar
-- + {create_proc_stmt}: ok dml_proc
-- + UPDATE CURSOR small_cursor(x) FROM VALUES(2);
-- - Error
create proc test_update_cursor_using()
begin
  update cursor small_cursor using 2 x;
end;

-- TEST basic use of proc savepoint rollback return and commit return
-- + {create_proc_stmt}: ok dml_proc
-- + {name proc_savepoint_basic}: ok dml_proc
-- + {proc_savepoint_stmt}: ok
-- + {rollback_return_stmt}: ok
-- + {commit_return_stmt}: ok
create proc proc_savepoint_basic()
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

-- TEST proc savepoint with an error, the outer statement should be marked error
-- + {create_proc_stmt}: err
-- + {proc_savepoint_stmt}: err
-- + Error % string operand not allowed in 'NOT'
create proc proc_savepoint_error_in_stmt_list()
begin
  proc savepoint
  begin
     set X := not 'x';
  end;
end;

-- TEST: proc savepoint invalid outside of a proc
-- + {proc_savepoint_stmt}: err
-- + Error % should be in a procedure and at the top level
-- +1 Error
proc savepoint begin end;

-- TEST: proc savepoint invalid outside of a proc
-- + {proc_savepoint_stmt}: err
-- + Error % should be in a procedure and at the top level
-- +1 Error
create proc savepoint_nested()
begin
   if 1 then
     proc savepoint begin end;
   end if;
end;

-- TEST: rollback return invalid outside of proc savepoint
-- + {rollback_return_stmt}: err
-- + Error % statement must appear inside of a PROC SAVEPOINT block
-- +1 Error
create proc rollback_return_invalid()
begin
   if 1 then
     rollback return;
   end if;
end;

-- TEST: commit return invalid outside of proc savepoint
-- + {commit_return_stmt}: err
-- + Error % statement must appear inside of a PROC SAVEPOINT block
-- +1 Error
create proc commit_return_invalid()
begin
   if 1 then
     commit return;
   end if;
end;

-- TEST: may not use a return statement inside of a savepoint block
-- + {create_proc_stmt}: err
-- + {proc_savepoint_stmt}: err
-- + {return_stmt}: err
-- + Error % use COMMIT RETURN or ROLLBACK RETURN in within a proc savepoint block
-- +1 Error
create proc regular_return_invalid()
begin
   proc savepoint
   begin
     return;
   end;
end;

-- TEST: create an integer enum
-- + {declare_enum_stmt}: integer_things: integer
-- + {name pen}: integer = 1
-- + {name paper}: integer = 7
-- + {name pencil}: integer = 8
declare enum integer_things integer (
  pen,
  paper = 7,
  pencil
);

-- TEST: create an integer enum exact copy is OK!
-- + {declare_enum_stmt}: integer_things: integer
-- + {name pen}: integer = 1
-- + {name paper}: integer = 7
-- + {name pencil}: integer = 8
declare enum integer_things integer (
  pen,
  paper = 7,
  pencil
);

-- TEST: create an real enum
-- + {declare_enum_stmt}: real_things: real
-- + {name pen}: real = 1.000000e+00
-- + {name paper}: real = 7.000000e+00
-- + {name pencil}: real = 8.000000e+00
declare enum real_things real (
  pen,
  paper = 7,
  pencil
);

-- TEST: x is declared with correct type and kind (real and <real_things>
-- + {declare_vars_type}: real<real_things> notnull
-- + {name rt}: rt: real<real_things> notnull variable
-- - Error
declare rt real_things;

-- TEST: ok to assign a pen to a x becasue it's a real_thing
-- + {assign}: rt: real<real_things> notnull variable
-- + {name rt}: rt: real<real_things> notnull variable
-- + {dbl 1.000000e+00}: real<real_things> notnull
set rt := real_things.pen;

-- TEST: not ok to assign integer_things.pen because it's the wrong kind
-- + {assign}: err
-- + {name rt}: rt: real<real_things> notnull variable
-- + {int 1}: integer<integer_things> notnull
-- + Error % expressions of different kinds can't be mixed: 'real_things' vs. 'integer_things'
-- +1 Error
set rt := integer_things.pen;

-- TEST: try to use an enum value, this is a rewrite
-- + SELECT 8.000000e+00;
select real_things.pencil;

-- TEST: try to use an enum value, invalid name
-- + {select_stmt}: err
-- + {dot}: err
-- + Error % enum does not contain 'nope'
-- +1 Error
select real_things.nope;

-- TEST: create a bool enum (it all becomes true/false)
-- + {declare_enum_stmt}: bool_things: bool
-- + {name pen}: bool = 1
-- + {name paper}: bool = 1
-- + {name pencil}: bool = 0
declare enum bool_things bool (
  pen,
  paper = 7,
  pencil
);

-- TEST: create a long integer enum
-- +  {declare_enum_stmt}: long_things: longint
-- + {name pen}: longint = 1
-- + {name paper}: longint = -7
-- + {name pencil}: longint = -6
declare enum long_things long_int (
  pen,
  paper = -7,
  pencil
);

-- TEST: duplicate enum name
-- + {declare_enum_stmt}: err
-- + Error % enum definitions do not match 'long_things'
-- there will be three reports, 1 each for the two versions and one overall error
-- +3 Error
declare enum long_things integer (
  foo
);

-- TEST: duplicate enum member name
-- + {declare_enum_stmt}: err
-- + Error % duplicate enum member 'two'
-- +1 Error
declare enum duplicated_things integer (
  two,
  two
);

-- TEST: invalid enum member
-- + {declare_enum_stmt}: err
-- + Error % evaluation failed 'boo'
-- +1 Error
declare enum invalid_things integer (
  boo = 1/0
);

-- TEST: refer to the enum from within itself
-- + DECLARE ENUM sizes REAL (
-- + big = 100,
-- + medium = 1.000000e+02 / 2
-- + small = 5.000000e+01 / 2
-- + tiny = 2.500000e+01 / 2
-- + {name big}: real = 1.000000e+02
-- + {name medium}: real = 5.000000e+01
-- + {name small}: real = 2.500000e+01
-- + {name tiny}: real = 1.250000e+01
-- - Error
declare enum sizes real (
  big = 100,
  medium = big/2,
  small = medium/2,
  tiny = small/2
);

-- TEST: reference other enums in this enum
-- + DECLARE ENUM misc REAL (
-- +   one = 1.000000e+02 - 2.500000e+01,
-- +   two = 7.500000e+01 - 1.250000e+01
-- + );
-- + {name one}: real = 7.500000e+01
-- + {name two}: real = 6.250000e+01
-- - Error
declare enum misc real (
  one = sizes.big - sizes.small,
  two = one - sizes.tiny
);

-- TEST: enum declarations must be top level
-- + {create_proc_stmt}: err
-- + {declare_enum_stmt}: err
-- + Error % declared enums must be top level 'bogus_inside_proc'
-- +1 Error
create proc enum_in_proc_bogus()
begin
  declare enum bogus_inside_proc integer (foo);
end;

create table SalesInfo(
  month integer,
  amount real
);

-- TEST: sum is not allowed in a window range
-- + Error % function may not appear in this context 'sum'
-- +1 Error
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND sum(month) FOLLOWING)
SalesMovingAverage FROM SalesInfo;

-- TEST: sum is not allowed in a window range
-- + Error % function may not appear in this context 'sum'
-- +1 Error
SELECT month, amount, AVG(amount) OVER
  (PARTITION BY sum(month) ROWS BETWEEN 1 PRECEDING AND 3 FOLLOWING)
SalesMovingAverage FROM SalesInfo;

-- TEST: sum is not allowed in a window range
-- + Error % function may not appear in this context 'sum'
-- +1 Error
SELECT month, amount, AVG(amount) OVER
  (ORDER BY month ROWS BETWEEN sum(month) PRECEDING AND 1 FOLLOWING)
SalesMovingAverage FROM SalesInfo;

-- TEST: sum is not allowed in a filter expression
-- + Error % function may not appear in this context 'sum'
-- +1 Error
SELECT month, amount, AVG(amount) FILTER(WHERE sum(month) = 1) OVER
  (ORDER BY month ROWS BETWEEN 1 PRECEDING AND 2 FOLLOWING EXCLUDE NO OTHERS)
SalesMovingAverage FROM SalesInfo;

create table AB(
  a integer,
  b text
);

create table CD(
  c integer,
  d text
);

create table BA(
  b integer,
  a text
);

declare proc use_c() (c integer);

-- TEST: arg bundle with a specific column
-- + INSERT INTO AB(a) VALUES(a2.c);
-- - Error
create proc arg_bundle_1(a1 like AB, a2 like CD)
begin
  insert into AB(a) from a2(c);
end;

-- TEST: arg bundle with a specific column using LIKE
-- + INSERT INTO AB(a) VALUES(a2.c);
-- - Error
create proc arg_bundle_2(a1 like AB, a2 like CD)
begin
  insert into AB(a) from a2(like use_c);
end;

-- TEST: arg bundle one column, in order
-- + INSERT INTO AB(a) VALUES(a2.c);
-- - Error
create proc arg_bundle_3(a1 like AB, a2 like CD)
begin
  insert into AB(a) from a2;
end;

-- TEST: arg bundle all columns
-- + INSERT INTO AB(a, b) VALUES(a1.a, a1.b);
-- - Error
create proc arg_bundle_4(a1 like AB, a2 like CD)
begin
  insert into AB from a1;
end;

-- TEST: arg bundle reverse order using LIKE (arg mismatch)
-- + INSERT INTO AB(a, b) VALUES(a1.b, a1.a);
-- + incompatible types in expression 'a'
-- +1 Error
create proc arg_bundle_5(a1 like AB, a2 like CD)
begin
  insert into AB from a1(like BA);
end;

-- TEST: arg bundle reverse order using LIKE both reversed
-- + INSERT INTO AB(b, a) VALUES(a1.b, a1.a);
-- - Error
create proc arg_bundle_6(a1 like AB, a2 like CD)
begin
  insert into AB(like BA) from a1(like BA);
end;

-- TEST: arg bundle non-name matching columns (this is ok, all in order)
-- + INSERT INTO AB(a, b) VALUES(a2.c, a2.d);
-- - Error
create proc arg_bundle_7(a1 like AB, a2 like CD)
begin
  insert into AB from a2;
end;

-- TEST: arg bundle out of order, no autoexpand (types mismatch)
-- + INSERT INTO AB(b, a) VALUES(a1.a, a1.b);
-- + Error % incompatible types in expression 'b'
-- +1 Error
create proc arg_bundle_8(a1 like AB, a2 like CD)
begin
  insert into AB(b,a) from a1;
end;

-- TEST: arg bundle out of order, no autoexpand, loading from alternate names (types mismatch)
-- + INSERT INTO AB(b, a) VALUES(a2.c, a2.d);
-- + Error % incompatible types in expression 'b'
-- +1 Error
create proc arg_bundle_9(a1 like AB, a2 like CD)
begin
  insert into AB(b,a) from a2;
end;

-- TEST: arg bundle into cursor in order but field names different
-- + FETCH C(a, b) FROM VALUES(a2.c, a2.d);
-- - Error
create proc arg_bundle_10(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  fetch C from a2;
end;

-- TEST: arg bundle into cursor in order field names same
-- + FETCH C(a, b) FROM VALUES(a1.a, a1.b);
-- - Error
create proc arg_bundle_11(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  fetch C from a1;
end;

-- TEST: arg bundle into cursor in order, but not all fields
-- + FETCH C(a, b) FROM VALUES(a1.a, NULL);
-- - Error
create proc arg_bundle_12(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  fetch C(a) from a1;
end;

-- TEST: arg bundle update cursor, all fields, autoexpand
-- + UPDATE CURSOR C(a, b) FROM VALUES(a1.a, a1.b);
-- - Error
create proc arg_bundle_13(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  update cursor C from a1;
end;

-- TEST: arg bundle update cursor, one field, name doesn't match
-- + UPDATE CURSOR C(a) FROM VALUES(a2.c);
-- - Error
create proc arg_bundle_14(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  update cursor C(a) from a2;
end;

-- TEST: arg bundle update cursor, all fields, names don't match
-- + UPDATE CURSOR C(a, b) FROM VALUES(a2.c, a2.d);
-- - Error
create proc arg_bundle_15(a1 like AB, a2 like CD)
begin
  declare C cursor like AB;
  update cursor C from a2;
end;

-- TEST: arg bundle update cursor, all fields, names don't match
-- + UPDATE CURSOR C(a, b) FROM VALUES(a2.c, a2.d);
-- - Error
create proc arg_bundle_16(a1 like AB, a2 like CD)
begin
  declare C cursor like a1;
  update cursor C from a2;
end;

-- TEST: a simple virtual table form
-- + {create_virtual_table_stmt}: basic_virtual: { id: integer, t: text } virtual @recreate
-- the exact module name encodes this list so keeping the whole tree shape here
-- misc attributes are tested elsewhere so there's no need to go crazy on arg varieties here
-- +  | {module_info}
-- +  | | {name module_name}
-- +  | | {misc_attr_value_list}
-- +  |   | {name this}
-- +  |   | {misc_attr_value_list}
-- +  |     | {name that}
-- +  |     | {misc_attr_value_list}
-- +  |       | {name the_other}
create virtual table basic_virtual using module_name(this, that, the_other) as (
  id integer,
  t text
);

-- TEST: virtual table error case
-- + {create_virtual_table_stmt}: err
-- + {create_table_stmt}: err
-- + Error % duplicate column name 'id'
-- +1 Error
create virtual table broken_virtual_table using module_name as (
  id integer,
  id integer
);

-- TEST: no indices on virtual tables
-- + {create_index_stmt}: err
-- + Error % cannot add an index to a virtual table 'basic_virtual'
-- +1 Error
create index some_index on basic_virtual(id);

-- TEST: no triggers on virtual tables
-- + {create_trigger_stmt}: err
-- + Error % cannot add a trigger to a virtual table 'basic_virtual'
-- +1 Error
create trigger no_triggers_on_virtual
  before delete on basic_virtual
begin
  delete from bar where rate > id;
end;

-- TEST: no alters on virtual tables
-- + {alter_table_add_column_stmt}: err
-- + Error % cannot use ALTER TABLE on a virtual table 'basic_virtual'
-- +1 Error
alter table basic_virtual add column xname text;

-- TEST: emit an enum
-- + {emit_enums_stmt}: ok
-- - Error
@emit_enums ints;

-- TEST: emit an enum (failed case)
-- + {emit_enums_stmt}: err
-- + Error % enum not found 'bogus_enum_name'
-- +1 Error
@emit_enums bogus_enum_name;

-- TEST: try a check expression
-- + {create_table_stmt}: with_check_expr: { v: integer }
-- + {check_def}: ok
-- + {gt}: bool
-- + {name v}: v: integer
-- + {int 5}: integer notnull
-- + CHECK (v > 5)
-- - Error
create table with_check_expr(
  v integer,
  check (v > 5)
);

-- TEST: check expression error
-- + {create_table_stmt}: err
-- + {check_def}: err
-- + Error % name not found 'q'
-- +1 Error
create table with_bogus_check_expr(
  v integer,
  check (q > 5)
);

-- TEST: declare type definition
-- + {declare_named_type}: text sensitive
-- + {name my_type}: text sensitive
-- + {sensitive_attr}: text sensitive
-- + {type_text}: text
-- - Error
declare my_type type text @sensitive;

-- TEST: declare type using another declared type
-- + DECLARE my_type_1 TYPE TEXT @SENSITIVE;
-- - Error
declare my_type_1 type my_type;

-- TEST: declare type using another declared type
-- + DECLARE my_type_2 TYPE TEXT @SENSITIVE;
-- - Error
declare my_type_2 type my_type_1;

-- TEST: declare type using another declared type
-- + {declare_named_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
declare my_type type bogus_type;

-- TEST: duplicate declare type definition
-- + {declare_named_type}: err
-- + Error % duplicate type declaration 'my_type'
-- +1 Error
declare my_type type integer;

-- TEST: use declared type in variable declaration
-- + DECLARE my_var TEXT @SENSITIVE;
-- + {declare_vars_type}: text sensitive
-- - Error
declare my_var my_type;

-- TEST: use bogus declared type in variable declaration
-- + {declare_vars_type}: err
-- + {name bogus_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
declare my_var bogus_type;

-- TEST: create local named type with same name. the local type have priority
-- + {create_proc_stmt}: ok
-- + DECLARE my_type TYPE INTEGER;
-- + DECLARE my_var INTEGER;
create proc named_type ()
begin
  declare my_type type integer;
  declare my_var my_type;
end;

-- TEST declare a sensitive and not null type
-- + DECLARE my_type_sens_not TYPE TEXT NOT NULL @SENSITIVE;
-- - Error
declare my_type_sens_not type text not null @sensitive;

-- TEST: declared type in column definition
-- + id TEXT @SENSITIVE NOT NULL
-- + {create_table_stmt}: t: { id: text notnull sensitive }
-- + {col_def}: id: text notnull sensitive
-- + {col_def_type_attrs}: ok
-- + {name id}
-- + {type_text}: text
-- + {sensitive_attr}: ok
-- + {col_attrs_not_null}
-- - Error
create table t(id my_type_sens_not);

-- TEST: declared type in column definition with error
-- + {create_table_stmt}: err
-- + {col_def}: err
-- + {col_def_type_attrs}: err
-- + {name bogus_type}
-- + Error % unknown type 'bogus_type'
-- +1 Error
create table t(id bogus_type);

-- TEST: declared type in cast expr
-- + SELECT CAST(1 AS TEXT);
-- + {select_stmt}: select: { _anon: text notnull }
-- - Error
select cast(1 as my_type);

-- TEST: declared type in cast expr with error
-- + SELECT CAST(1 AS bogus_type);
-- + {name bogus_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
select cast(1 as bogus_type);

-- TEST: declared type in param
-- + CREATE PROC decl_type (label TEXT @SENSITIVE)
-- + {create_proc_stmt}: ok
-- - Error
create proc decl_type(label my_type)
begin
end;

-- TEST: declared type in param with error
-- + {create_proc_stmt}: err
-- + {name bogus_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
create proc decl_type_err(label bogus_type)
begin
end;

-- TEST: declared type in declare function
-- + DECLARE FUNC decl_type_func (arg1 INTEGER) TEXT @SENSITIVE;
-- + {declare_func_stmt}: text sensitive
-- - Error
declare func decl_type_func (arg1 integer) my_type;

-- TEST: declared type in declare function with err
-- + DECLARE FUNC decl_type_func_err (arg1 INTEGER) bogus_type;
-- + {declare_func_stmt}: err
-- + {name bogus_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
declare func decl_type_func_err (arg1 integer) bogus_type;

create table to_copy(
  f1 integer,
  f2 integer not null,
  f3 integer not null @sensitive,
  f4 integer @sensitive
);

-- TEST: ensure all attributes correctly copied
-- + CREATE TABLE the_copy(
-- + f1 INTEGER,
-- + f2 INTEGER NOT NULL,
-- + f3 INTEGER @SENSITIVE NOT NULL,
-- + f4 INTEGER @SENSITIVE
-- - Error
create table the_copy(
   like to_copy
);

-- TEST: ensure proc arguments are rewritten correctly
-- + CREATE PROC uses_complex_table_attrs (f1_ INTEGER, f2_ INTEGER NOT NULL, f3_ INTEGER NOT NULL @SENSITIVE, f4_ INTEGER @SENSITIVE)
-- - Error
create proc uses_complex_table_attrs(like to_copy)
begin
end;

-- TEST: ensure proc arguments are rewritten correctly
-- + DECLARE PROC uses_complex_table_attrs (f1_ INTEGER, f2_ INTEGER NOT NULL, f3_ INTEGER NOT NULL @SENSITIVE, f4_ INTEGER @SENSITIVE)
-- - Error
declare proc uses_complex_table_attrs(like to_copy);

-- TEST: ensure func arguments are rewritten correctly
-- + DECLARE FUNC function_uses_complex_table_attrs (f1_ INTEGER, f2_ INTEGER NOT NULL, f3_ INTEGER NOT NULL @SENSITIVE, f4_ INTEGER @SENSITIVE) INTEGER;
-- - Error
declare function function_uses_complex_table_attrs(like to_copy) integer;

-- TEST: ensure cursor includes not-null and sensitive
-- + {declare_cursor_like_name}: complex_attr_cursor: to_copy: { f1: integer, f2: integer notnull, f3: integer notnull sensitive, f4: integer sensitive } variable shape_storage value_cursor
-- - Error
declare complex_attr_cursor cursor like to_copy;

-- TEST: make a function that creates sensitive
-- + {declare_func_stmt}: object create_func sensitive
-- + {create_data_type}: object create_func sensitive
-- + {sensitive_attr}: object sensitive
-- + {type_object}: object
-- - Error
declare function maybe_create_func_sensitive() create object @sensitive;

-- TEST: make a function that creates blob
-- + {declare_func_stmt}: blob notnull create_func
-- + {create_data_type}: blob notnull create_func
-- + {notnull}: blob notnull
-- + {type_blob}: blob
-- - Error
declare function maybe_create_func_blob() create blob not null;

-- TEST: make a function that creates text
-- + {declare_func_stmt}: text create_func
-- + {create_data_type}: text create_func
-- + {type_text}: text
-- - Error
declare function maybe_create_func_text() create text;

-- TEST: make a function that creates int
-- + {declare_func_stmt}: err
-- + {create_data_type}: err
-- + Error % Return data type in a create function declaration can only be Text, Blob or Object
-- +1 Error
declare function maybe_create_func_int() create int;

-- TEST: make a function that creates bool
-- + {declare_func_stmt}: err
-- + {create_data_type}: err
-- + Error % Return data type in a create function declaration can only be Text, Blob or Object
-- +1 Error
declare function maybe_create_func_bool() create bool;

-- TEST: make a function that creates long
-- + {declare_func_stmt}: err
-- + {create_data_type}: err
-- + Error % Return data type in a create function declaration can only be Text, Blob or Object
-- +1 Error
declare function maybe_create_func_long() create long not null @sensitive;

-- TEST: declare a named type for object Foo
-- + {declare_named_type}: object<Foo> notnull sensitive
-- + {sensitive_attr}: object<Foo> notnull sensitive
-- + {notnull}: object<Foo> notnull
-- + {type_object}: object<Foo>
-- + {name Foo}
-- - Error
declare type_obj_foo type object<Foo> not null @sensitive;

-- TEST: declared function that return create object
-- + DECLARE FUNC type_func_return_create_obj () CREATE OBJECT<Foo> NOT NULL @SENSITIVE;
-- + {declare_func_stmt}: object<Foo> notnull create_func sensitive
-- - Error
declare function type_func_return_create_obj() create type_obj_foo;

-- TEST: declared function that return create bogus object
-- + {declare_func_stmt}: err
-- + {create_data_type}: err
-- + Error % unknown type 'bogus_type'
-- +1 Error
declare function type_func_return_create_bogus_obj() create bogus_type;

-- TEST: declared function that return object
-- + DECLARE FUNC type_func_return_obj () OBJECT<Foo> NOT NULL @SENSITIVE;
-- + {declare_func_stmt}: object<Foo> notnull sensitive
-- - Error
declare function type_func_return_obj() type_obj_foo;

-- TEST: declare type as enum name
-- + DECLARE my_enum_type TYPE INTEGER<ints> NOT NULL;
-- + {declare_named_type}: integer<ints> notnull
-- + {notnull}: integer<ints> notnull
-- - Error
declare my_enum_type type ints;

-- TEST: used a named type's name to declare an enum
-- + {declare_enum_stmt}: err
-- + Error % duplicate type declaration 'my_type'
-- +1 Error
declare enum my_type integer (
 negative_one = -1,
 postive_one = 1
);

-- TEST: make x coordinate for use later, validate that it has a kind
-- + {type_int}: integer<x_coord>
-- - Error
declare x1, x2, x3 integer<x_coord>;

-- TEST: make x coordinate for use later, validate that it has a kind
-- + {type_int}: integer<y_coord>
-- - Error
declare y1, y2, y3 integer<y_coord>;

-- TEST: try to assign an x to a y
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := y1;

-- TEST: try to assign an x to a y
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := y1;

-- TEST: try to add and x and a y
-- + {add}: err
-- + {name x1}: x1: integer<x_coord> variable
-- + {name y1}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := x1 + y1;

-- TEST: this is ok, it's still an x
-- + {mul}: integer<x_coord>
-- - Error
set x1 := x1 * 2;

-- TEST: this is ok, it's still an x
-- + {add}: integer<x_coord>
-- - Error
set x1 := x1 + x2;

declare bb bool;

-- TEST: this is ok, comparison of same types (equality)
-- + {eq}: bool
-- - Error
set bb := x1 = x2;

-- TEST: this is ok, comparison of same types (inequality)
-- + {lt}: bool
-- - Error
set bb := x1 < x2;

-- TEST: comparison of two incompatible types (equality)
-- + {eq}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set bb := x1 = y1;

-- TEST: comparison of two incompatible types (inequality)
-- + {lt}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set bb := x1 < y1;

-- TEST: make an alias for an integer with kind (x)
-- + {declare_named_type}: integer<x_coord>
-- + {name _x}: integer<x_coord>
-- + {type_int}: integer<x_coord>
-- + {name x_coord}
-- - Error
declare _x type integer<x_coord>;

-- TEST: make an alias for an integer with kind (y)
-- + {name y_coord}
-- - Error
declare _y type integer<y_coord>;

-- TEST: declare an integer with the type alias
-- + DECLARE x4 INTEGER<x_coord>;
-- + {declare_vars_type}: integer<x_coord>
-- + {name_list}: x4: integer<x_coord> variable
-- + {name x4}: x4: integer<x_coord> variable
-- + {type_int}: integer<x_coord>
-- + {name x_coord}
-- - Error
declare x4 _x;

-- TEST: use the named type version, should be the same
-- + {assign}: x1: integer<x_coord> variable
-- + {name x1}: x1: integer<x_coord> variable
-- + {name x4}: x4: integer<x_coord> variable
-- - Error
set x1 := x4;

-- TEST: make a table that has mixed kinds
-- + {create_table_stmt}: xy: { x: integer<x_coord>, y: integer<y_coord> }
-- + {col_def}: x: integer<x_coord>
-- + {col_def}: y: integer<y_coord>
create table xy(
  x _x,
  y _y
);

-- TEST: valid insert the kinds match
-- + {insert_stmt}: ok
-- + {name xy}: xy: { x: integer<x_coord>, y: integer<y_coord> }
-- - Error
insert into xy using x1 x, y1 y;

-- TEST: invalid insert the kinds don't match (y1 is not an xcoord)
-- + {insert_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
insert into xy using y1 x, x1 y;

-- TEST: insert into the table with matching coordinates
-- + {insert_stmt}: ok
-- + {name xy}: xy: { x: integer<x_coord>, y: integer<y_coord> }
insert into xy select xy.x, xy.y from xy where xy.x = 1;

-- TEST: insert into the table with coordinates reversed (error)
-- + {insert_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'y_coord' vs. 'x_coord'
-- +1 Error
insert into xy select xy.y, xy.x from xy where xy.x = 1;

-- TEST: compound select with matching object kinds (use as to make the names match)
-- +  {select_stmt}: UNION ALL: { x: integer<x_coord>, y: integer<y_coord> }
-- - Error
select x1 as x, y1 as y
union all
select x2 as x, y2 as y;
 
-- TEST: compound select with not matching object kinds (as makes the name match)
-- but the kind is wrong so you still get an error (!)
-- + {select_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'y_coord' vs. 'x_coord'
-- +1 Error
select x1 as x, y1 as y
union all
select y2 as x, x2 as y;

-- TEST: insert into xy with values, kinds are ok
-- + {insert_stmt}: ok
-- - Error
insert into xy values (x1, y1), (x2, y2);

-- TEST: insert into xy with values, kinds are ok
-- + {insert_stmt}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
insert into xy values 
  (x1, y1), 
  (y2, x2),
  (x3, y3);

-- TEST: cursor should have the correct shape including kinds
-- + {declare_cursor_like_name}: xy_curs: xy: { x: integer<x_coord>, y: integer<y_coord> } variable shape_storage value_cursor
declare xy_curs cursor like xy;

-- TEST: fetch cursor, ok, kinds match
-- + {fetch_values_stmt}: ok
-- + {name xy_curs}: xy_curs: xy: { x: integer<x_coord>, y: integer<y_coord> } variable shape_storage value_cursor
-- - Error
fetch xy_curs from values(x1, y1);

-- TEST: fetch cursor but kinds do not match
-- + Error % expressions of different kinds can't be mixed: 'y_coord' vs. 'x_coord'
-- +1 Error
fetch xy_curs from values(y1, x1);

-- some variables of a different type
-- - Error
declare v1, v2, v3 integer<v>;

-- TEST: when with matching variable kinds this is ok, it's x1 or x1
-- + {assign}: x1: integer<x_coord> variable
-- + {name x1}: x1: integer<x_coord> variable
-- + {case_expr}: integer<x_coord>
-- - Error
set x1 := case when 1 then x1 else x1 end;

-- TEST: when with non-matching variable x and y mixed
-- + {case_expr}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := case when 1 then x1 else y1 end;

-- TEST: case expressions match (x and x), this is ok
-- + {assign}: v1: integer<v> variable
-- + {name v1}: v1: integer<v> variable
-- - Error
set v1 := case x1 when x2 then v1 else v2 end;

-- TEST: invalid mixing of x and y in the when expression
-- note extra line breaks to ensure any reported errors are on different lines for help with diagnosis
-- + {case_expr}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set v1 := case x1
               when x2
               then v1
               when y1
               then v2
               else v3
               end;

-- TEST: need a bool for the subsequent stuff
-- - Error
declare b0 bool;

-- TEST: in expression has valid kinds, no problem here
-- + {assign}: b0: bool variable
-- + {in_pred}: bool
-- + {name x1}: x1: integer<x_coord> variable
-- + {expr_list}: x1: integer<x_coord> variable
-- + {expr_list}: x2: integer<x_coord> variable
-- + {expr_list}: x3: integer<x_coord> variable
-- - Error
set b0 := x1 in (x1, x2, x3);

-- TEST: in expression has mixed kinds
-- + {assign}: err
-- + {in_pred}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set b0 := x1 in (x1, y2, x3);

-- TEST: in expression using select
-- + {assign}: b0: bool variable
-- + {in_pred}: bool
-- + {select_stmt}: x2: integer<x_coord> variable
set b0 := (select x1 in (select x2));

-- TEST: in expression using select, but select result is the wrong kind
-- + {assign}: err
-- + {in_pred}: err
-- + {select_stmt}: err
-- + {select_core_list}: select: { y1: integer<y_coord> variable }
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set b0 := (select x1 in (select y1));

-- TEST: between with kinds, all matching
-- + {assign}: b0: bool variable
-- + {between_rewrite}: bool
-- - Error
set b0 := x1 between x2 and x3;

-- TEST: left between operand is of the wrong kind
-- + {assign}: err
-- + {between}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set b0 := x1 between y2 and 12;

-- TEST: right between operand is of the wrong kind
-- + {assign}: err
-- + {between}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set b0 := x1 between 34 and y3;

-- TEST: left and right could be used but they don't match each other
-- + {assign}: err
-- + {between}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set b0 := 56 between x2 and y3;

-- TEST: negation preserves the kind, kind ok so this works
-- +  {assign}: x1: integer<x_coord> variable
-- +  | {name x1}: x1: integer<x_coord> variable
-- +  | {uminus}: integer<x_coord>
set x1 := -x2;

-- TEST: negation preserves the kind, hence we get an error
-- + {assign}: err
-- + {uminus}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := -y1;

-- TEST: coalesce compatible kinds (should preserve kind)
-- + {assign}: x1: integer<x_coord> variable
-- + {call}: integer<x_coord>
-- - Error
set x1 := coalesce(x1, x2, x3);

-- TEST: coalesce incompatible kinds (should preserve kind)
-- + {assign}: err
-- + {call}: err
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := coalesce(x1, y2, x3);

-- TEST: cast ok direct conversion
-- + {assign}: x1: integer<x_coord> variable
-- + {name x1}: x1: integer<x_coord> variable
-- + {cast_expr}: integer<x_coord>
-- - Error
set x1 := cast(y1 as integer<x_coord>);

-- TEST: cast ok direct conversion (using type name) (check for rewrite too)
-- + SET x1 := CAST(y1 AS INTEGER<x_coord>);
-- + {assign}: x1: integer<x_coord> variable
-- + {name x1}: x1: integer<x_coord> variable
-- + {cast_expr}: integer<x_coord>
-- - Error
set x1 := cast(y1 as _x);

-- TEST: cast ok, strip kind explicitly
-- + {assign}: x1: integer<x_coord> variable
-- + {name x1}: x1: integer<x_coord> variable
-- + {cast_expr}: integer
-- + {name y1}: y1: integer<y_coord> variable
-- - Error
set x1 := cast(y1 as integer);

-- TEST: cast bad, kinds don't match now
-- + {assign}: err
-- + {name x1}: x1: integer<x_coord> variable
-- + Error % expressions of different kinds can't be mixed: 'x_coord' vs. 'y_coord'
-- +1 Error
set x1 := cast(x1 as integer<y_coord>);

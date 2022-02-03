---
slug: columns-sugar
title: Using the LIKE form in the SELECT statement
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

One of the signature features of the CQL language is the ability to use the "LIKE" form to
slice out columns that conform to a shape.  This notion appears in many places in the language.
For instance if I have a table `Foo`. I can make a cursor for that shape like so:

```sql
declare C cursor like Foo;
```

Which says I want the columns of `C` to be like the columns of `Foo`.

If I have a cursor `D` that has the `Foo` columns but maybe more and maybe in a different order I can load `C`
as follows:

```sql
fetch C from D(like Foo)
```

Which again saves me from having to list all the (potentially dozens) of Foo columns.  This construct is in many places:

```sql
declare proc P(like Foo)
begin
  insert into Foo from arguments;
end;
```

even

```sql
declare proc P(f like Foo, b like Bar)
begin
  insert into Foo from f;
  insert into Bar from b;
end;
```

And other examples...  This is discussed more fully in
[Chapter 5](https://cgsql.dev/cql-guide/ch05#reshaping-data-cursor-like-forms) of the Guide.

However, one of the few places that shapes are interesting but not supported was in the select list.
And so, just a couple of days ago, we added the `COLUMNS` construct to the language which allows for
a sugared syntax for extracting columns in bulk.  It's kind of a generalization of the `select T.*`
pattern but with CQL-style slicing and type-checking.

These forms are supported:

  * columns from a join table or tables

```
-- same as A.*
select columns(A) from ...;

-- same as A.*, B.*
select columns(A, B) from ...;
```
  * columns from a particular join table that match a shape

```
-- the columns of A that match the shape Foo
select columns(A like Foo) from ...;

-- get the Foo shape from A and the Far shape from B
select columns(A like Foo, B like Bar) from ...;
```

* columns from any join table that match a shape

```
--- get the Foo shape from anywhere in the join
select columns(like Foo) from ...;

-- get the Foo and Bar shapes, from anywhere in the join
select columns(like Foo, like Bar) from ...;
```
  * specific columns

```
-- x and y columns plus the foo shape
select columns(T1.x, T2.y, like Foo) from ...;
```

  * distinct columns from the above (not distinct values!)

```
-- removes duplicate column names
-- e.g. there will be one copy of 'pk'
select columns(distinct A, B) from A join B using(pk);

-- if both Foo and Bar have an (e.g.) 'id' field you only get one copy
select columns(distinct like Foo, like Bar) from ...;

-- if a specific column is mentioned it is always included
-- but later clauses that are not a specific column will avoid it
-- if F or B has an x it won't appear again, just T.x
select columns(distinct T.x, F like Foo, B like Bar) from F, B ..;
```

Of course this is all just sugar, so it all ends up being a column list with table
qualifications -- but the syntax is very powerful.  For instance, for narrowing a
wide table, or for fusing joins that share common keys

```
-- just the Foo columns
select columns(like Foo) from Superset_Of_Foo_From_Many_Joins_Even;

-- only one copy of 'pk'
select columns(distinct A,B,C) from
  A join B using (pk) join C using (pk);
```

And of course you can define shapes however you like and then use them
to slice off column chucks of your choice.  There are many ways to build
up shapes from other shapes.  Probably the easiest is to declare procedures
that return the shape you want and never actual create them.  E.g.

```
declare proc shape1() (x integer, y real, z text);
declare proc shape2() (like shape1, u bool, v bool);
```

With this combination you can easily define common column shapes and slice them
out of complex queries without having to type the columns names over and over...

Note that the `COLUMNS(...)` form is not a general replacement for the select list.
For instance, general expressions are not allowed inside of `COLUMNS(...)` but,
where extraction of lots of columns is needed, or even re-ordering of colummns,
it's a very good option indeed and it composes well with the other `select` features.

This was the last significant area where shapes are useful but totally absent.

---
slug: expression-frags
title: Introducing Expression Fragments
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

Following on the heels of shared fragments we're introducing the same kind of thing
for shared fragments that are expressions rather than tables.  The syntax is as follows:

```sql
-- this isn't very exciting because regular max would do the job
@attribute(cql:shared_fragment)
create proc max_func(x integer, y integer)
begin
  select case when x >= y then x else y end;
end;
```

The above can be used in the context of a SQL statement like so:

```sql
  select max_func(T1.column1,T1.column2) the_max from foo T1;
```

The consequence of the above is that the body of `max_func` is inlined into the generated SQL.  However, like
the other shared fragments this is done in such a way that the text can be shared between instances so
you only pay for the cost of the text\* in your program one time, no matter how many time you use it.

\* you still pay for the cost of a pointer to the text

In particular, for the above the compiler will generate the following SQL:


```sql
select (
  select case when x >= y then x else y end 
    from (select T1.column1 x, column2 y))
```

But each line will be its own string literal, so more accurately it will concatenate the following three strings:

```C
  "select (",                                      // string1
  " select case when x >= y then x else y end",    // string2
  " from (select T1.column1 x, column2 y))"        // string3
```

Importantly `string2` is fixed for any given fragment.  The only thing that changes is `string3`, i.e. the arguments.
The C compiler and then the linker will unify the `string2` literal across all translation units so you only
pay for the cost of that text one time.  It also means that the text of the arguments appears exactly one time,
no matter how complex they are.  For these benefits we pay the cost of the select wrapper on the arguments.  This
is cost is frequently negative.  Consider this following

```sql
select max_func((select max(T.m) from T), (select max(U.m) from U))
```

A direct expansion of the above would result in something like

```sql
case when (select max(T.m) from T) >= (select max(U.m) from U)
   then (select max(T.m) from T) 
   else (select max(U.m) from U) 
end;
```

The above could be accomplished with a simple pre-processor macro.  But the fragments code generates:


```sql
select (
  select case when x >= y then x else y end 
    from select (select max(T.m) from T) x, (select max(U.m) from U) y))
```


Expression fragments can nest, so you could write:

```sql
@attribute(cql:shared_fragment)
create proc max3_func(x integer, y integer, z integer)
begin
  select max_func(x, max_func(y, z));
end;
```

Again this particular example is a waste because regular `max` would already do the job but,
for instance, common mappings from one kind of code to another using case/when can be written
and shared this way.

```sql
-- this sort of thing happens all the time
@attribute(cql:shared_fragment)
create proc remap(x integer not null)
begin
   select case x
     when 1 then 1001
     when 2 then 1057
     when 3 then 2010
     when 4 then 2011
     else 9999
   end;
end;
```

In the following:

```sql
select remap(T1.c), remap(T2.d), remap(T3.e) from C, D, E;
```

The text for `remap` will appear three times in the generated SQL query but only one time in your binary.

Restrictions:

* the function must consist of exactly one simple select statement
  * no `FROM`, `WHERE`, `HAVING`, etc. -- the result is an expression
* the select list must have exactly one value
  * Note: the expression can be a nested `SELECT` which could have all the usual `SELECT` elements
* the usual shared fragment rules apply, e.g. no out-parameters, exactly one statement, etc.


FAQ:

Q: Why does the expression fragment have a `select` in it?

A: Expression fragments are only interesting in SQL contexts where normal procedure and function calls are not available.
The `select` keyword makes it clear to the author and the compiler that the expression will be evaluated by
SQLite and the rules for what is allowed to go in the expression are the SQLite rules.

Q: Why no `FROM` clause

A: We're trying to produce an expression, not a table-value with one column.  If you want a table-value with
one column the original shared fragments solution already do exactly that.  This gives you a solution for
sharing code in say the `WHERE` clause or the select list.

Q: Isn't this just the same as doing say `#define max_func(x,y) case when (x) >= (y) then x else y end;`

A: Macros can give you a ton of flexibility but they have many problems:
* if the macro has an error you see the error in the call site with really bad diagnostic info
* the compiler doesn't know that the sharing is going on so it won't be able to share text between call sites
* the arguments can be evaluated many times each which could be expensive, bloaty, or wrong
* there is no type-checking of arguments to the macro so you may or may not get compilation errors after expansion
* you have to deal with all the usual pre-processor hazards

In general, macros _can_ be used for meta-programming, (happens in C/C++ too) but that doesn't mean it's a good idea.

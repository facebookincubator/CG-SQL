---
slug: like-forms-tutorial
title: A quick tutorial on LIKE forms
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql, errors]
---

Everyone knows the usual expression syntax `x LIKE y` to do a string match.  But the CQL compiler also uses
`LIKE` in a different way that's powerful and important.  CQL has the notion of data shapes and you use
`LIKE` to refer to them.  The simplest source of a data shape, and maybe the most common, is a table.
Maybe something like this:

```sql
create table T(
 id integer not null,
 name text not null,
 age integer not null
);
```

Now suppose you want to write a procedure that can insert a row into that table, You could write

```sql
create proc insert_into_T(
  id_ integer not null, 
  name_ text not null, 
  age_ integer not null
)
begin
  insert into T(id, name, age)  values(id_, name_, age_);
end;
```

This is all fine and well but what if T had 50 columns?  That gets old fast.  And how can you
be sure that you inserted the columns into T in the right order?  This second example also compiles
even though it's clearly wrong:

```
  insert into T(id, name, age) values(age_, name_, id_);
```

And of course you can imagine things get only more complicated with more columns in T.

We started adding the `LIKE` form to ease these issues and to ensure some consistency in the APIs while preventing
simple transpostion errors.  So you can instead write:

```
create proc insert_into_T(like T)
begin
  insert into T from arguments;
end;
```

so here the `like T` in the argument list simply means "make arguments that are the same as the columns of table T" -- well,
almost. It also adds an `_` to the end of each name so you end up exactly the same declaration as the long form above.
But you won't miss any arguments, and they'll be in the right order.

And notice that we used `from arguments` to indicate that we wanted the values to come from the arguments in order. Again
this saves you from a lot of typing and a lot of error checking.  You can't get the arguments in the wrong order.

These are the most basic patterns. But there are quite a few more.

Let's suppose you want to write a procedure that returns in row with the highest age in the above.  Maybe you write
something like this:


```sql
create proc highest_age()
begin
  declare C cursor for select * from T;
  declare M cursor like C;
  loop fetch C
  begin
     if (not M or M.age < C.age) then
       fetch M from C;
     end if;
  end;
  out M;
end;
```

Here we made a cursor `M` that is the same as the cursor `C` and then we are going to generate a single row result
from the cursor.  Note that if you use a cursor name like `M` in an expression it refers to the hidden boolean
that says if the cursor has a row in it or not.  So `M` begins empty and we will load it if it's empty or if the age
is higher than what we've already got.

Let's show a few more of the forms.  Suppose we don't want to return `name`, just the `id` and the `age`.  We can
change things up a tiny bit.

```sql
create proc highest_age()
begin
  declare C cursor for select * from T;
  declare M cursor like select 1 id, 99 age;
  loop fetch C
  begin
     if (not M or M.age < C.age) then
       fetch M from cursor C(like M);
     end if;
  end;
  out M;
end;
```

So two things to notice.  We used an *ad hoc* shape, making a fake `select` statement that returns the shape we want.  This
select doesn't run but it does define types and columns easily.  Two not null integers in this case.  Now `M` is not the
same as `C` so we can't use the simplest form `fetch M from C` we have to use the more general form. 

Fully expanded, what we wrote becomes:

```sql
  FETCH M(id, age) FROM VALUES(C.id, C.age);
```

But as you can see, we didn't have to type all those column names.  And that's kind of the point of the `LIKE` construct.

So we've covered a bunch of the shape sources already:

* a table name
* a cursor name
* a select statement that gives the shape in an ad hoc fashion

There are three more

* a view name 
* the return shape of a procedure that returns a result set
* the arguments of a procedure

View names are pretty simple, and they work the same as table names so we don't need to discuss those. Let's look
at some of the other uses with procedures.

Suppose we have a procedure that can return a result set shape but we want to be able to mock its results so we
can fake whatever result we need for testing.  

We'll complicate this a bit adding a new table (keeping short table names for the sample to save typing)

```sql
create table U(
 id integer not null,
 email text not null
);
```

And here's a procedure:

```sql
create proc my_proc()
begin
   select T.*, U.email from T inner join U on T.id = U.id;
end;
```

Now we want to be able to make any fake result we want, so maybe want a temp table. No problem:

```sql
create proc _init_fake_results()
begin
  create temp table if not exists fake_results(
   like my_proc
  );
end;

create proc add_fake_result(like fake_results)
begin
  insert into fake_results from arguments;
end;

create proc get_fake_results()
begin
  select * from fake_results;
end;
```

The above is very generic and will maintain well.  You can see we made a temp table that will have
exactly the same shape as whatever `my_proc` returns.  In this case it becomes:

```sql
CREATE PROC _init_fake_results ()
BEGIN
  CREATE TEMP TABLE IF NOT EXISTS fake_results(
    id INTEGER NOT NULL,
    name TEXT NOT NULL,
    age INTEGER NOT NULL,
    email TEXT NOT NULL
  );
END;
```

And the rest are patterns we've seem before.

The last source of shapes are procedure arguments.  There's lots of good cases for those, I wrote an [entry](https://cgsql.dev/blog/update) on those previously but I'll give a simple example here too.

Suppose we have this weird procedure:

```sql
create proc delete_stuff(age_ integer, name_ text)
begin
  if age_ is not null then
     delete from T where T.age = age_;
  end if;

  if name_ is not null then
     delete from T where T.name = name_;
  end if;
end;
```

What if we wanted to log any errors that happen here?  Maybe make a verison that logs.  We can do it like this:

```sql
create proc delete_and_log(like delete_stuff arguments)
begin
  begin try
    call delete_stuff(from arguments);
  end try;
  begin catch
    call printf("delete failed\n"); -- or whatever
    throw;
  end catch;
end;
```

The nice thing about this logging wrapper procedure is that if `delete_stuff` changes, the wrapper will change with it.

That covers all of the shape sources and as we saw we can use them to create things like cursors, tables, and argument lists.
We can use them to specify a subset of columns that might be of interest when fetching or updating cursors.  And we can use
them in one last way -- to restrict arguments to a particular shape.  Let's see how that works by making the previous logger
a little different.  Here we added an argument which tells if we should look.  And that might look like it would
spoil the `from arguments` part of the forwarding, but there is the final way to use `LIKE`.

```sql
create proc delete_and_log2(log bool not null, like delete_stuff arguments)
begin
  if log and age_ is not null then
    call printf("deleting %d\n", age_); -- or whatever
  end if;
  if log and name_ is not null then
    call printf("deleting %d\n", name_); -- or whatever
  end if;

  call delete_stuff(from arguments like delete_stuff arguments);
end;
```

So this form lets you use some of your arguments, the ones that match a certain shape.  And as we saw in
the previous article you can also use `from C` to pass arguments where `C` is a cursor and in that case
you can also specify that arguments be matched by name `from C like shape`.  In both those cases the
formal parameter names of the called procedure are matched against the names of the shape and passed in
the order of the formals.  So this is like "call by name", the fields of the cursor or the order of
arguments in the argument list might be different than the formals but you'll get the correct items
in the correct order regardless, because it matches by name.

These forms can save you a lot of typing... and are excellent at avoiding errors and improving maintainability.
Where they appear in SQL statements, everything is expanded before it goes to SQLite so SQLite will see
normal syntax forms.  Which is good because obviously SQLite doesn't know anything about this enhanced
`LIKE` business.

In the examples above there were only one or two columns with very short names, but in real world code
there can easily be dozens of columns with very long names.  In those cases, these forms really shine.

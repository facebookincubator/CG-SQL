---
slug: from-general
title: Using the FROM construct in more places
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

This new feature is a pretty simple generalization of the `FROM` construct as applied to expression lists.
Note this isn't the same as using `FROM` the usual way in a `select` statement.   An example will clear
this right up.

Suppose you wanted to create a procedure that can insert a row into a table.  You could write this:

```sql
create table Shape_xy (x int, y int);

create proc insert_xy(like Shape_xy)
begin
  insert into Shape_xy from arguments;
end;
```

Here we're using `from` to introduce some shape of values.  It can appear in a lot of places.

Suppose now I want to write insert two of those shapes. I could write this slightly more complicated procedure:

```sql
create proc insert_two_xy(xy1 like Shape_xy, xy2 like Shape_xy)
begin
   call insert_xy(from xy1);
   call insert_xy(from xy2);
end;
```

And this also composes with cursors.  So maybe you need to get two `xy` values from diverse
locations.  You can mix an match.

```sql
create proc write_xy()
begin
   declare C cursor for select T.x, T.y from somewhere T;
   fetch C;
   declare D cursor for select T.x, T.y from somewhere_else T;
   fetch D;
   if C and D then
     -- strange combos for illustration only
     call insert_two_xy(from C, from D);
     call insert_two_xy(from D, 5, 3);
     call insert_two_xy(4, 2, from C);
     call insert_two_xy(4, from C, 8);
   end if;
end;
```
So as you can see, we can start from data in one ore more cursors and we can turn that data,
plus other expressions into arguments, composing them as we like. This gives you the ability
to call procedures and functions using shapes from a mixed set of sources.  None of this is new.

However, the other places where expression lists happen: `fetch`, `update cursor`, and `insert` only
allowed you specify a single object as the input source such as `insert into Shape_xy from C`.

With a little work this is trivially generalized so that all value lists can use the `from` construct.

Here's a complete example showing all the new forms.

```sql
create table Shape_xy (x int, y int);
create table Shape_uv (u text, v text);
create table Shape_uvxy (like Shape_xy, like Shape_uv);

create proc ShapeTrix()
begin
  declare C cursor for select Shape_xy.*, '1' u, '2' v from Shape_xy;
  fetch C;

  -- This new form is equivalent to the old form:
  --    insert into Shape_xy from C(like Shape_xy)
  -- but the values(...) form generalizes, see below.
  insert into Shape_xy values(from C like Shape_xy);

  declare D cursor for select * from Shape_uv;
  fetch D;

  declare R cursor like Shape_uvxy;

  -- This form works just like the function call case
  -- that was previously supported (it uses the same code even).
  -- This form lets you load R from any combination of sources
  -- as long as you make a suitable row.
  fetch R from values (from C like Shape_xy, from D);

  -- Same thing is supported in update cursor
  -- the x, y come from C and the u,v come from D.x, D.y.
  -- Note that C.u and C.v would not even be type compatible.
  update cursor R from values (from C like Shape_xy, from D);

  -- And in a select-values clause
  declare S cursor for
    with cte(l,m,n,o) as (values (from C like Shape_xy, from D))
     select * from cte;
  fetch S;
  insert into Shape_uvxy from S;
end;
```
As you can see, you can choose a subset of the `from` shape using `like`.

These combinations let you flexibily assemble rows of data for
cursors, calls, insertion, using any combination of data sources
you might want, and without resorting to listing every column by hand.

---
slug: boxed-cursors-intro
title: More Flexible Cursor Patterns Using "Boxing"
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql, cursors]
---

I was reviewing the update posting that just went out and I realized I'd forgotten to mention another big ticket item. So
consider this an appendix to the update.

In some cases we started seeing a need to "ship cursors around" a little bit more flexibly.
Note shipping values around is already doable so this new work is largely about being able to create a "statement cursor"
in one procedure and consume it safely elsewhere.  The general pattern looks like this:

Declare a statement cursor as usual, maybe something like this:

```sql
declare C cursor for select * from shape_source;

-- or

declare C cursor for call proc_that_returns_a_shape();
```

Make an object that can hold a cursor:

```sql
declare obj object<T cursor>;
```

Where `T` is the name of a shape. It can be a table name, or a view name, or it can be the name of the canonical procedure that returns the result.  You really want this to be  some kind of global name though.  Something you can get with a `#include` in various places. In this case choices for `T` might be `shape_source` the table or `proc_that_returns_a_shape` the procedure.

Remember you can always make a fake procedure that returns a result to sort of typedef a shape name.  e.g.

```sql
declare proc my_shape() (id integer not null, name text);
```

The procedure here `my_shape` doesn’t have to actually ever be created, in fact it’s probably better if it doesn’t.  You won’t call it, you’re just using its hypothetical result as a shape.  This could be useful if you have several procedures like `proc_that_returns_a_shape` that all return `my_shape`.


At this point you could use the cursor maybe something like:

```sql
loop fetch C
begin
  -- do stuff with C
end;
```

Those are the usual patterns and they let you consume statement cursors sort of “up” from where it was created, but what if you want some worker procedures that consume a cursor there is no good way to pass your cursor down again.  Well, there wasn't. Now there is.  Let's go back to that box object creation and use it

```sql
-- recap: declare the box that holds the cursor (T changed to my_shape for this example)
declare obj object<my_shape cursor>;

-- box the cursor into the object (the cursor shape must match the box shape)
set obj from cursor C;
```

The variable `obj` can now be passed around as usual.  Then, later, you can "unbox" it to get a cursor back. Like so

```sql
-- unboxing a cursor from an object
declare D cursor for obj;
```

These primitives will allow cursors to be passed around with managed lifetime.
Example:

```sql
-- consumes a cursor
create proc cursor_user(box object<my_shape cursor>)
begin
   declare C cursor for box;  -- the cursors shape will be my_shape matching box
   loop fetch C
   begin
      -- do something with C
   end;
end;

-- captures a cursor and passes it on
create proc cursor_boxer()
begin
   declare C cursor for select * from something_like_my_shape;
   declare box object<my_shape cursor>
   set box from cursor C; -- produces error if shape doesn't match
   call cursor_user(box);
end;
```

Importantly, once you box a cursor the underlying SQLite statement’s lifetime is managed by the box object with normal
retain/release semantics so timely release becomes imperative.

With this pattern it's possible to, for instance, consume some of the rows in one procedure and the rest in another procedure.

Now, the main reason for doing this is if you have some standard helper methods that can get a cursor from a variety of places and process it.
But remember, that boxing isn’t the usual pattern at all and returning cursors in a box, while possible, should be avoided in favor of the simpler
pattern of doing your `select` or `call` at the end to compute the result as we do now, if only because then then lifetime is very simple in all those cases.
Durably storing a boxed cursor could lead to all manner of problems -- it's just like holding on to a `sqlite3_stmt *` for a long time.
Actually "just like" is an understatement, it's *exactly* the same as holding on to a statement for a long time with all the same problems because that
is exactly what's going on here.

So, good generalization, but possibly less Pit of Success, especially with complex box patterns.  So watch the sharp edges.

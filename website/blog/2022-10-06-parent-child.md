---
slug: parent-child
title: Introducing Parent/Child Result Sets
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

## Introduction and Context

There are many cases where you might want to nest one result set inside of another one.  In order to
do this ecomomically there was a great desire to be able to run a parent query and a child query and
then link the child rows to the parent rows.  One way to do this is of course to run one query for
each "child" but then you end up with `O(n)` child queries and if there are sub-children it would be
`O(n*m)` and so forth. What you really want to do here is something more like a join, only without
the cross-product part of the join.  Many systems have such features, sometimes they are called
"chaptered rowsets" but in any case there is a general need for such a thing.


We did a bunch of work in the name of Parent/Child results sets but like many goals of this kind it
caused us to ripen the CQL language in a variety of ways and its interesting to talk about those
changes.  Importantly, we wanted to be able to do work of this kind in the language while adding
the fewest new notions and basically enabling the language to express a concept like a child rowset
in the first place.

Here are some things that happened along the way that are interesting.

## Cursor Types and Result Types
One of the first problems we run into thinking about how a CQL program might express pieces of a rowset
and turn them into child results is that you need to be able to hash a row, append row data, and
extract a result set from a key.

Let's think about that for just a second: in order to do anything at all with a child rowset,
no matter how we got such a thing, we have to be able to describe it in a type-safe way.
These objects already exist at runtime but they do not appear anywhere in the language explicitly
and that was going to have to change.

To address this we added a new object type, kind of like we did with boxed statements.  A result set
has a type that looks like this `object <proc_name set>`.  Here `proc_name` must the the name of a
procedure that returns a result set and the object will represent a result set with the
corresponding columns in it.

That step may seem like it's super important but actually it's kind of optional, it provides type-safety
but the initial versions of the feature just used the type `object` which works fine provided you make
no mistakes... it turns out there are even more fundamental needs that aren't optional.

## Creating New Cursor Types From Existing Cursor Types

The first thing you need to be able to to is take the type of the parent query and add to it one
more columns to whole the child result set or sets (note that you can have more than one child
result set per parent).  So for instance you might have a list of people, and one child result might
be the names of the schools they attended and another is the names of the jobs they worked.

So while adding columns to existing rows might sound like a bizarre thing to do but actually it's
actually fundamental to the job here.  We must be able to create a new output row is that is the
sames as the parent but includes columns for the the child results too.  There was no good syntax for this.
The cursor declaration forms were:

```
/* option 1 */ declare C cursor like shape_name;
/* option 2 */ declare C cursor like select 1 x, "2" y, false z;
```

The first option implies that you already have a shape from (e.g.) a procedure or table and you want
to make an identical cursor.  That doesn't work here because we're trying to modify an existing shape,
not use it as is.

The second form was supposed to be able to create any kind of cursor shape by simply declaring a `select`
statement that is an example of what you want to capture.  In principle this can define almost anything.
However, there's a catch -- you can't get object types to come out of a `select` so it's hopeless for result set types.
And, maybe just as important, you can't just add a few columns to an existing type with any kind of ease,
you have to list all columns.

Fortunately there was a pretty simple solution to this problem.  There were already lots of cases where
a typed name list happens in the language -- for example in the return type of a function you can
specify something like `(id integer, name text)`.  That construction also defines a shape just like a
select statement and there was already code to handle all the correctness analysis.  Additionally,
the `LIKE` construct can be used in such a list to refer to existing types.  So for instance a function that
returns all the columns of tables A and B could be defined like so

```
declare function foo() (LIKE A, LIKE B);
```

So we could solve all the cursor type problems by allowing a typed name list to be used to define a cursor shape.
Probably the approach that should have been taken in the first place. The select option seems weird by comparison.

With the already existing support for shapes in a type list we could make the result shape for this parent/child case
with ease, like so:

```
declare result cursor like (like parent, child_result object<child_proc set>);
```

So, all the parent columns plus a child result set.  Or more than one child result set if needed.

Lastly there were going to be cases where we needed to make a new cursor using only some of the field of an existing cursor.
The case in particular I'm thinking of is that we might have a big row from the parent and it might
have only one or two columns that we need that form the key columns for the child.  We didn't have a good way to do that
either, but solving this turns out to be simple enough.  We already had this form:

```
declare D cursor like C;
```

we just added:

```
declare D cursor like C(a, b, c);
```

Which chooses just the 3 named fields from `C` and makes a cursor with only those.  Recently we added
the form:

```
declare D cursor like C(-x);
```

To mean take all the columns of `C` except `x`

With the a shape for the key fields defined, we can use existing syntax to load the fields
economically:


```
fetch D from C(like D);
```

Which says we want to load `D` from the fields of `C`, but using only the columns of `D`.  That operation
is of course going to be an exact type match by construction.  So now we could describe the key columns from
child rows, and the key columns from parent rows.  And we could add columns to the parent type to create space
to hold child result sets.  All of our type problems are solved.  Almost.

### Cursor Arguments

It was clear that we would need to be able to do things like "hash a cursor" (any cursor) or "store this row
into the appropriate partition" and this requirement meant that we had to be able to write functions that
could take any cursor and dynamically do things to it based on its type information.  There is no good way
to write these generic helper things in CQL, but:

* we don't need very many of them,
* it's pretty easy to do that job in C

The main thing we need is to create a way to declare such functions and call them a with cursor and the necessary shape info.

So we added this notion of being able to call an external function with any cursor.  Like so:

```
declare function cursor_hash(C cursor) long not null;
```

you can call it like so:

```
let hash := cursor_hash(C);
```

where `C` is any cursor.

When such a call is made the C function `cursor_hash` gets passed what we call a "dynamic cursor".
This includes:
* a pointer to the data for the cursor
* the count of fields
* the names of the fields
* the type/offset of every field in the cursor

So you can (e.g.) generically do the hash by applying a hash to each field and then combining all of those.
This kind of function works on any cursor and all the extra data about the shape that's needed to make the
call is static, so really the cost of the call stays modest.  Details of the dynamic cursor type are in
`cqlrt_common.h` and there are many example functions now in the `cqlrt_common.c` file.

Again, creating this facility was a pretty minor matter, the compiler already has all this data and uses it
to create result sets in the first place.  We just allowed other functions to use that same data and
made a public type for it.

## The Specific Parent/Child Functions
To do the parent/child operations we needed three helper functions:

```
DECLARE FUNC cql_partition_create ()
   CREATE OBJECT<partitioning> NOT NULL;

DECLARE FUNC cql_partition_cursor (
  part OBJECT<partitioning> NOT NULL,
  key CURSOR,
  value CURSOR)
    BOOL NOT NULL;

DECLARE FUNC cql_extract_partition (
  part OBJECT<partitioning> NOT NULL,
  key CURSOR)
    CREATE OBJECT NOT NULL;
```

The first function makes a new partitioning.

The second function hashes the key columns of a cursor (specified by the key argument) and appends
the values provided into a bucket for that key.  By making a pass over the child rows you can easily
create a partitioning with each unique key combo having a buffer of all the matching rows.

The third function is used once the partitioning is done.  Given a key again, which you now presumably
get from the parent rows, you get the buffer you had accumulated and then make a result set out of it
and return that.  Note that this function returns the vanilla object type because it could be returning
any shape.

## Result Set Sugar
With the type system mentioned above you could now join together any kind of complex parent and
child combo you needed, but it might be a lot of code, and it's error prone.  This is a good job
for a little sugar.  So we added some simple syntax to specify the usual partitioning.

It looks like this:

```
-- parent and child defined elsewhere
declare proc parent(x integer not null) (id integer not null, a integer, b integer);
declare proc child(y integer not null) (id integer not null, u text, v text);

-- join together parent and child using 'id'
create proc parent_child(x_ integer not null, y_ integer not null)
begin
  out union call parent(x_) join call child(y_) using (id);
end;
```

The generated code is simple enough, even though there's a good bit of it.
But it's a useful exercise to look at it once.  Comments added for clarity.

```
CREATE PROC parent_child (x_ INTEGER NOT NULL, y_ INTEGER NOT NULL)
BEGIN
  DECLARE __result__0 BOOL NOT NULL;

  -- we need a cursor to hold just the key of the child row
  DECLARE __key__0 CURSOR LIKE child(id);

  -- we need our partitioning object (there could be more than one per function
  -- so it gets a number, likewise everything else gets a number
  LET __partition__0 := cql_partition_create();

  -- we invoke the child and then iterate its rows
  DECLARE __child_cursor__0 CURSOR FOR CALL child(y_);
  LOOP FETCH __child_cursor__0
  BEGIN
    -- we extract just the key fields (id in this case)
    FETCH __key__0(id) FROM VALUES(__child_cursor__0.id);

    -- we add this child to the partition using its key
    SET __result__0 := cql_partition_cursor(__partition__0, __key__0, __child_cursor__0);
  END;

  -- we need a shape for our result, it is the columns of the parent plus the child rowset
  DECLARE __out_cursor__0 CURSOR LIKE (id INTEGER NOT NULL, a INTEGER, b INTEGER,
                                       child1 OBJECT<child SET> NOT NULL);

  -- now we call the parent and iterate it
  DECLARE __parent__0 CURSOR FOR CALL parent(x_);
  LOOP FETCH __parent__0
  BEGIN
    -- we load the key values out of the parent this time, same key fields
    FETCH __key__0(id) FROM VALUES(__parent__0.id);

    -- now we create a result row using the parent columns and the child result set
    FETCH __out_cursor__0(id, a, b, child1) FROM VALUES(__parent__0.id, __parent__0.a, __parent__0.b, cql_extract_partition(__partition__0, __key__0));

    -- and then we emit that row
    OUT UNION __out_cursor__0;
  END;
END;
```

This code iterates the child once and the parent once and only has two database calls,
one for the child and one for the parent.  And this is enough to create parent/child result
sets for the most common examples.

## Result Set Values
While the above is probably the most common case, another case can happen where you might
want to make a procedure call for each parent row to compute the child.  And, more generally,
there was no good way to work with result sets from procedure calls other than iterating them
with a cursor.  The iteration pattern is very good if the data is coming from a select statement
-- we don't want to materialize all of the results if we can stream instead.  However, when working
with result sets the whole point is to create materialized results for use elsewhere.
We now had the power to express a result set type with `object<proc_name set>` but no way to
actually get such a set from an existing procedure.  Procedures generated them,
but they could only be consumed in the C layer.

Fortunately this is also an easy problem to solve.  We already supported the ability to use
procedures as functions in expressions if they had the right signature.  We now add the ability
to call a procedure that returns a result set and capture that result.
Previously this was not supported and would have produced an error.

With the new features you can write:

```
declare child_result object<child set>;
set child_result := child(args);
```

or better still:

```
let child_result := child(args);
```

With this simple change we had the power to write something like this:


```
declare proc parent(x integer not null) (id integer not null, a integer, b integer);
declare proc child(id integer not null) (id integer not null, u text, v text);

create proc parent_child(x_ integer not null, y_ integer not null)
begin
  -- the result is like the parent with an extra column for the child
  declare result cursor like (like parent, child object<child set>);

  -- call the parent and loop over the results
  declare P cursor for call parent(x_);
  loop fetch P
  begin
     -- compute the child for each P and then emit it
     fetch result from values(from P, child(P.id));
     out union result;
  end;
end;
```

After the sugar is applied this compiles down to this program:

```
DECLARE PROC parent (x INTEGER NOT NULL) (id INTEGER NOT NULL, a INTEGER, b INTEGER);
DECLARE PROC child (id INTEGER NOT NULL) (id INTEGER NOT NULL, u TEXT, v TEXT);

CREATE PROC parent_child (x_ INTEGER NOT NULL, y_ INTEGER NOT NULL)
BEGIN
  DECLARE result CURSOR LIKE (id INTEGER NOT NULL, a INTEGER, b INTEGER,
                              child OBJECT<child SET>);

  DECLARE P CURSOR FOR CALL parent(x_);
  LOOP FETCH P
  BEGIN
    FETCH result(id, a, b, child) FROM VALUES(P.id, P.a, P.b, child(P.id));
    OUT UNION result;
  END;
END;
```

The `LIKE` and `FROM` forms are very powerful but they aren't new.  They do make
it a lot easier to express this notion of just adding one more column to the result.
Note that the code for emitting the `parent_child` result before the transformation
doesn't need to specify what the columns of the parent are or the columns of the child,
only that the parent has at least the `id` column.  Even that could have been removed.

This call could have been used instead:

```
fetch result from values(from P, child(from P like child arguments));
```

That syntax would result in using the columns of P that match the arguments of `child` -- just
`P.id` in this case.  But if there were 7 such columns the sugar might be easier to understand.


## Additional Language Support

Last, but not least, to make this more accessible we wanted more support in the generated code.
The C interface would have produced generic object results for the child result columns.
This isn't wrong exactly but it would mean that a cast would be required in every use case on the
native side, and it's easy to get the cast wrong.  So the result type of column getters was
adjusted to be a `child_result_set_ref` instead of just `cql_object_ref`.

Similar transforms were needed if column setters were being emitted (yes that's an option!)
and of course the Java and Objective C output needed the same transform.

## Conclusion

The prosecution of native support for parent/child result sets in CQL resulted in a bunch of
very useful generalizations for declaring and managing cursors.  The old special case code for
blobs was actually replaced by these forms.  The language overall expressiveness increased far
more than just the ability to do this one kind of join.  It's now possible to write general
purpose debug helpers for cursors.  It's possible to store and return pre-cooked result sets,
creating useful caches and other such combinations.  The type extensions to allow extending
and narrowing existing types allow even more return flexibility while keeping everything
strongly typed.

Parent/Child result sets exploit all of these things.

---
slug: backed-tables
title: Introducing Backed Tables
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

## Introduction and Context

Most production databases include some tables that are fairly generic, they use maybe a simple key-value combination to store some simple
settings or something like that.  In the course of feature development this kind of thing comes up pretty often and in large client
applications (like Messenger, but certainly not limited to Messenger) there are many small features that need a little bit of state.
It's easy enough to model whatever state you need with a table or two but this soon results in an explosion of tiny tables.  In some cases
there are only a few rows of configuration data and indeed the situation can be so bad that the text of the schema for the little state table is
larger than the sum of all the data you will ever store there.  This is a bit tragic because SQLite has initialization cost associated with
each table.  So these baby tables are really not paying for themselves at all.  What we'd like to do is use some kind of generic table
as the backing store for many of these small tables while preserving type safety.  The cost of access might be a bit higher but since
data volumes are expected to be low anyway this would be a good trade-off.  And we can have as many as we like.  In some cases the state doesn't
even need to be persisted, so we're talking about tables in an in-memory database.  Here low cost of initialization is especially important.
And lastly, if your product has dozens or even hundreds of small features like this, the likelihood that all of them are even used in a session
is quite low and so again, having a low fixed cost for the schema is a good thing.  No need to create 100 in-memory tables on the off chance that
they are needed.

See also the related feature: [blob storage](https://cgsql.dev/blog/blob-storage).

### How do I define one of these backed tables?

First you need a place to store the data, we define a backing table in the usual way.  A simple backing table is just a key/value store and looks
like this:

```sql
@ATTRIBUTE(cql:backing_table)
CREATE TABLE backing(
  k BLOB PRIMARY KEY,
  v BLOB NOT NULL
);
```

The `backing_table` attribute indicates that the table we're about to define is to be used for backing storage.  At present it is signficantly restricted.
It has to have exactly two columns, both of which are blobs, one is the key and one is the value.  It should be either baseline schema or annotated with
`@create` as it is expected to be precious data.  If it's an in-memory table the versioning is somewhat moot but really the backing store is not supposed
to change over time, that's the point.   In future versions we expect to allow some number of additional physical columns which can be used by the backed
tables (discussed below) but for now it's this simple pattern.

Backed table looks like this:

```sql
@ATTRIBUTE(cql:backed_by=backing)
CREATE TABLE backed(
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  bias REAL
);

@ATTRIBUTE(cql:backed_by=backing)
CREATE TABLE backed2(
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);
```


The `backed_by` attribute indicates that the table we're about to define is not really going to be its own table.  As a result, you will not be
able to (e.g.) `DROP` the table or `CREATE INDEX`  or `CREATE TRIGGER` on it, and there will be no schema upgrade for it should you request one.
It may not contain constraints as there would be no way to enforce them.  But as compensation for these restrictions it can be changed freely and
has no physical schema cost associated with it.

### How do I read this data?

To understand how this works imagine that we had a view for each backed table which simply read the blobs out of the backing store and then extracted the backed columns using some blob extraction functions. This would work, but then we'd be trading view schema for table schema so the schema savings we're trying to achieve would go up in smoke.

We might be lost here but CQL already has something very "view like" and that's the shared fragment structure.  So what we do instead of views is to automatically create a shared fragment just like the view we could have made.  They look like this:

```sql
@ATTRIBUTE(cql:shared_fragment)
CREATE PROC _backed ()
BEGIN
  SELECT
   rowid,
   cql_blob_get(T.k, backed.id) AS id,
   cql_blob_get(T.v, backed.name) AS name,
   cql_blob_get(T.v, backed.bias) AS bias
    FROM backing AS T
    WHERE cql_blob_get_type(T.k) = 2105552408096159860L;
END;
```

So some things to notice right away:

First, this fragment has the right shape, but the shared fragment doesn't directly call blob extractors.  Rather it uses these `cql_blob_get`.
The point of this is to make the actual blob functions configurable.  The test suites include some very simple extraction functions for blobs
with just integers in them, but you can create whatever blob format you want. You could use the [blob storage](https://cgsql.dev/blog/blob-storage)
feature for encoding or you can encode it as you see fit.  You can even have different encodings in different backed tables.

Second, there is a type code embedded in the procedure.  The type code is a hash of the type name and the names and types of all the not-null fields
in the backed table.  The hash is arbitrary but repeatable, any system can compute the same hash and find the records they want without having to share
headers. The actual hash is open source but it's just a SHA256 reduced to 64 bits with some name canonicalization.  Shortly the JSON will also include
the relevant hashes so you can easily consume them without even having to know the hash function.

Here's the slightly smaller shared fragment for `backed2`
```sql
@ATTRIBUTE(cql:shared_fragment)
CREATE PROC _backed2 ()
BEGIN
  SELECT
    rowid,
    cql_blob_get(T.k, backed2.id) AS id,
    cql_blob_get(T.v, backed2.name) AS name
    FROM backing AS T
    WHERE cql_blob_get_type(T.k) = -1844763880292276559L;
END;
```
As you can see it's very similar -- the type hash is different and of course it has different columns.

### Why does the type hash include only the non-null fields?

The idea is that the backed table might change over time and you can add new optional fields without invalidating your existing data.  If you change
the name of the type or if you add new not null fields the type identity changes and any data you have in the backing table will basically be
ignored because the type hash will not match.

### What do `cql_blob_get` and `cql_blob_get_type` turn into?

You can configure them as you see fit.  By default cql_blob_get turns into either `bgetkey` or `bgetval` depending on if you are
reading from the key blob or the value blob.  The directives for configuring this function are:

```sql
@blob_get_key bgetkey offset;
@blob_get_val bgetval;
```

You can configure the system to ask for the column by offset (this is normal for the primary key because it has a fixed number of columns
for any given key type and they are all mandatory), or by hash code (this is normal for the value type because it might be missing some
columns and so offset is probably not appropriate).  However both are configurable so you want to do key by hashcode simply omit the "offset"
part of the directive.  Likewise if your values are offset addressable you can add "offset" to the value directive.  Here the offset means
the zero based ordinal of the column in the key or the value.

The type access functions are similarly configurable (they never need a code or an offset).

```sql
@blob_get_key_type bgetkey_type;
@blob_get_val_type bgetval_type;
```

### What does this end up looking like?

Armed with these basic transforms we can already do a simple transform to make select statement work.  Suppose CQL sees:

```sql
declare C cursor for select * from backed;
```

We can make this work with a simple transform:

```sql
 DECLARE C CURSOR FOR WITH
  backed (*) AS (CALL _backed())
  SELECT *
    FROM backed;
```

Now remember  `_backed` was the automatically created shared fragment.  Basically, if we see a select statement that mentions any backed table
we simply add a call to the corresponding shared fragment in the WITH clause.  This effectively creates that "view" we need.  And because we're
using the shared fragment form, all users of this fragment will share the text.  So there's no schema and the text of the backed appears only
once in the binary.  More precisely we get this:

```sql
WITH
backed (rowid, id, name, bias) AS (
  SELECT
    rowid,
    bgetkey(T.k, 0),                      -- 0 is offset of backed.id in key blob
    bgetval(T.v, -6639502068221071091L),  -- note hash of backed.name
    bgetval(T.v, -3826945563932272602L)   -- note hash of backed.bias
  FROM backing AS T
  WHERE bgetkey_type(T.k) = 2105552408096159860L)
SELECT rowid, id, name, bias
  FROM backed;
```

Now with this in mind we can see that it would be very beneficial to also add this:

```sql
CREATE INDEX backing_index ON backing(bgetkey_type(k));
```

or more cleanly:

```sql
CREATE INDEX backing_index ON backing(cql_blob_get_type(k));
```

Either of these result in a computed index on the row type stored in the blob.  Other physical indices might be helpful too and these can potentially
be shared by many backed tables, or used in partial indicies.

Of course your type function might be named something other than the default `bgetkey_type`.

Now consider a slightly more complex example:

A slightly more complex example:
```
select T1.* from backed T1 join backed2 T2 where T1.id = T2.id;
```

becomes:

```
WITH
  backed (rowid, id, name, bias) AS (CALL _backed()),
  backed2 (rowid, id, name) AS (CALL _backed2())
  SELECT T1.*
    FROM backed AS T1
    INNER JOIN backed2 AS T2
    WHERE T1.id = T2.id;
```
Now even though two different backed tables will be using the backing store the select "just works".  All the compiler had to do was add both backed
table fragments.  And of course if `backed` was joined against itself, that would also just work.

### How do I insert data like this?

Consider:
```sql
insert into backed values (1, "n001", 1.2), (2, "n002", 3.7);
```

This has to insert into the backing storage and convert the various values into key and value blobs.  A simple transform does this job as well:

```sql
 WITH
  _vals (id, name, bias) AS (
    VALUES(1, "n001", 1.2), (2, "n002", 3.7)
  )
  INSERT INTO backing(k, v) SELECT
    cql_blob_create(backed, V.id, backed.id),
    cql_blob_create(backed,
      V.name, backed.name,
      V.bias, backed.bias)
    FROM _vals AS V;
```

What's going on here? Well, the issue is that the data to be inserted can be arbitrarily complicated.  It might refer to all kinds of things.
In this case it's just literal values but in general it could be anything.  So the transform takes the original values and puts them in a
 _vals(...) CTE.  Then we insert into the backing store by converting _vals into blobs -- one for the key and one for the value.
 There is only the one place we need to do this for any given insert statement no matter now many items or how complex the insertion is.

`cql_blob_create` similarly expands to a user configured function with optional hash codes and mandatory field types.  There is default
configuration that corresponds to this:

```sql
@blob_create_key bcreatekey offset;
@blob_create_val bcreateval;
```


The final SQL looks like this:

```sql
WITH
_vals (id, name, bias) AS (
  VALUES(1, "n001", 1.2), (2, "n002", 3.7)
)
INSERT INTO backing(k, v) SELECT
  bcreatekey(2105552408096159860, V.id, 1), -- type 1 is integer, offset implied
  bcreateval(2105552408096159860,
    -6639502068221071091, V.name, 4,  -- hash as before, type 4 is text,
    -3826945563932272602, V.bias, 3)  -- hash as before, type 3 is real,
  FROM _vals AS V
```

Note that both blobs have the same overall type code (2105552408096159860) as before.  The key blob did not use per-field type codes, so the argument
positions give the implied offset.  In contrast the value blob is using hash codes (offset was not specified).  This configuration is typical.

A more complex insert works just as well:
```sql
insert into backed
  select id+10, name||'x', bias+3 from backed where id < 3;
```

The above insert statement is a bit of a mess.  It's taking some of the backed data and using it to create new backed data.  But the simple transforms we have  work just as before.  We add the needed `backed` CTE and create `_vals` like before.

```sql
WITH
  backed (*) AS (CALL _backed()),
  _vals (id, name, bias) AS (
    SELECT id + 10, name || 'x', bias + 3
    FROM backed
    WHERE id < 3
  )
  INSERT INTO backing(k, v)
   SELECT
     cql_blob_create(backed, V.id, backed.id),
     cql_blob_create(backed, V.name, backed.name, V.bias, backed.bias)
   FROM _vals AS V;
```

Looking closely at the above we see a few things:
- `cql_blob_create` will expand as before (not shown)
- we added `backed(*)` as usual
- `_vals` once again just has the exact unchanged insert clause
- the `insert into backing(k, v)` part is identical, the same recipe always works


### How does the delete operation work?

Now let's look at a simple delete example:

```sql
delete from backed where id = 7;
```

Now remember we're again looking for a pattern that will generalize when the where condition gets crazy. But fortunately this is not so hard.
The following form is fully general:

```sql
WITH
  backed (*) AS (CALL _backed())
DELETE FROM backing
  WHERE rowid IN (
    SELECT rowid
    FROM backed
    WHERE id = 7
  );
```

All we had to do here was:
* add the usual `_backed` CTE
* move the original `WHERE` clause into a subordinate `SELECT` that gives us the rowids to delete.

With the backed table in scope, any `WHERE` clause works. If other backed tables are mentioned, the compiler
simply adds those as usual.

Here's a more complicated delete, it's a bit crazy but illustrative:

```sql
delete from backed where
  id in (select id from backed2 where name like '%x%');
```

So this is using rows in `backed2` to decide which rows to deleted in `backed`.  The same simple transform works directly.

```sql
WITH
  backed2 (*) AS (CALL _backed2()),
  backed (*) AS (CALL _backed())
DELETE FROM backing
  WHERE rowid IN (
    SELECT rowid
    FROM backed
    WHERE id IN (
      SELECT id FROM backed2 WHERE name LIKE '%x%'
    )
  );
```

What happened here:
* the `WHERE` clause went directly into the body of the rowid select
* `backed` was used as before but now we also need `backed2`

The delete pattern does not need any additional cql helpers beyond what we've already seen.

### What about updating tables?

The update statement is the most complicated of the bunch and it requires all the tricks from all the previous statements plus one more.

First, we'll need two more blob helpers that are configurable.  By default they look like this:

```sql
@blob_update_key bupdatekey offset;
@blob_update_val bupdateval;
```

These are used to replace particular columns in a stored blob.  Now let's start with a very simple update to see now it all works:

```sql
update backed set name = 'foo' where id = 5;
```

Fundamentally we need to do these things:
* the target of the update has to end up being the backing table
* we need the backed table CTE so we can do the filtering
* we want to use the rowid trick to figure out which rows to update which handles our where clause
* we need to modify the existing key and/or value blobs rather than create them from scratch

Let's see how this looks:

```sql
WITH
  backed (*) AS (CALL _backed())
UPDATE backing
  SET v = cql_blob_update(v, 'foo', backed.name)
    WHERE rowid IN (SELECT rowid
    FROM backed
    WHERE id = 5);
```

Tearing this down a bit:
* we needed the normal CTE so that we can use `backed` rows
* the `WHERE` clause moved into a `WHERE rowid` sub-select just like in the `DELETE` case
* we changed the SET targets to be `k` and `v` very much like the `INSERT` case, except we used an update helper that takes the current blob and creates a new blob to store
  * the helper is varargs so as we'll see it can mutate many columns in one call

This gives us a working update statement... with one hitch.  It's possible to use the existing column values in the update expressions and there's no way to use our `backed` CTE to get them since the final update has to be all relative to the backing table.

Let's look at another example to illustrate the problem:

```sql
update backed set name = name || 'y' where bias < 5;
```

So this is adding the letter 'y' to some rows.  Kind of goofy but similar mutations do happen and have to work.  To make this work the reference to
`name` inside of the set expression has to change. We end up with something like this:

```sql
WITH
  backed (*) AS (CALL _backed())
UPDATE backing
  SET v = cql_blob_update(v,
    cql_blob_get(v, backed.name) || 'y',
    backed.name)
  WHERE rowid IN (SELECT rowid
    FROM backed
    WHERE bias < 5);
```

Importantly the reference to `name` in the set expression was changed to `cql_blob_get(v, backed.name)` -- extracting the name from the value blob. After which it is appended with 'y' as usual.

The rest of the pattern is just as it was, in fact literally everything else is unchanged.  But it's easy to see that the `WHERE` clause could be arbitrarily complex and it just works.  Since the `UPDATE` statement has no `FROM` clause only the fields in the target table might need to be rewritten, so in this case `name`, `id`, and `bias` were possible but only `name` was mentioned.

After the `cql_blob_get` and `cql_blob_update` are expanded the result looks like this:

```sql
WITH
backed (rowid, id, name, bias) AS (
  SELECT
    rowid,
    bgetkey(T.k, 0),
    bgetval(T.v, -6639502068221071091L),
    bgetval(T.v, -3826945563932272602L)
  FROM backing AS T
  WHERE bgetkey_type(T.k) = 2105552408096159860L
)
UPDATE backing
SET v =
  bupdateval(
    v,
    -6639502068221071091L, bgetval(v, -6639502068221071091L) || 'y', 4
  )
  WHERE rowid IN (SELECT rowid
  FROM backed
  WHERE bias < 5);
```
The blob update function for the value blob requires the original blob, the hash or offset to update, the new value, and the type of the new value.
The blob update function for the key blob is the same (blob, hash/offset, value) but the type is not required since the key blob necessarily has all
the fields present because they are necessarily not null.  Therefore the type codes are already all present and so the type of every column is known.
The value blob might be missing nullable values hence their type might not be stored/known.

To illustrate these cases we can make another small example; we'll set up yet another small table that uses the same backing store:

```sql
@attribute(cql:backed_by=backing)
create table meta(
 name text,
 state long,
 prev_state long,
 primary key(name, state)
);
```

This update mixes all kinds of values around...

```sql
update meta
 set state = state + 1, prev_state = state
 where name = 'foo';
```

And the final output will be:

```sql
WITH
meta (rowid, name, state, prev_state) AS (
  SELECT
    rowid,
    bgetkey(T.k, 0),
    bgetkey(T.k, 1),
    bgetval(T.v, -4464241499905806900)
  FROM backing AS T
  WHERE bgetkey_type(T.k) = 3397981749045545394
)
SET
  k = bupdatekey(k, bgetkey(k, 1) + 1, 1),
  v = bupdateval(v, -4464241499905806900, bgetkey(k, 1),  2)
  WHERE rowid IN (SELECT rowid
  FROM meta
  WHERE name = 'foo');
```

As expected the `bupdatekey` call gets the column offset (1) but not the type code (2).  `bupdateval` gets a hash code and a type.

All of these transforms are live in the code as of a few days ago.

The upshot is that, if you write some simple encoding and decoding functions, you can have very flexible blob storage.

### Appendix

If you want to refer to your blob functions in your own code, such as for indices you'll also need to do something like this:

```sql
declare select function bgetkey_type(b blob) long;
declare select function bgetval_type(b blob) long;
declare select function bgetkey(b blob, iarg integer) long;
declare select function bgetval(b blob, iarg integer) long;
declare select function bcreateval no check blob;
declare select function bcreatekey no check blob;
declare select function bupdateval no check blob;
declare select function bupdatekey no check blob;
```

`bgetval` and `bgetkey` are not readily declarable generally because their result is polymorphic so it's preferable to use `cql_blob_get` as above which then does the rewrite for you.  But it is helpful to have a UDF declaration for each of the above, especially if you want the `--rt query_plan` output to work seamlessly.  Typically `bgetval` would only be needed in the context of a `create index` statement.

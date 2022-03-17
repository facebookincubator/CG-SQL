---
slug: blob-storage
title: Introducing Blob Storage
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

## Introduction and Context

The general idea here is that you might want to store composite data in a single column in the database.  This is a common way to get more generic schema, the idea being that you can have one or more blob columns that store in tables a lot of data that doesn't have to be indexed. You could store it in other ways, like a JSON blob or some such, but we'll be using blobs as the basis for storage here -- hence
the name blob "storage".

### How do I define one of these blobs?
In SQL/CQL, the main way you define structures, especially those that you want to maintain, is with tables.  Hence we introduce this

```sql
@attribute(cql:blob_storage)
create table news_info(
  who text,
  what text,
  when_ long -- timestamp of some kind
);
```

The `blob_storage attribute` indicates that the thing we're about to define here is not really going to be a materialized table.  As a result, you will not be
able to (e.g.) `DROP` the table or `SELECT` from it, and there will be no schema upgrade for it should you request one. However, the usual schema rules still
apply which help you to create compatible versions of this structure.  For instance, new columns can be added only at the end, and only if they are nullable.
Here we add `source` to the schema in a hypothetical "version 6".  Note that schema versions move forward globally in the schema, not locally in one table; this implies there are versions 1-5 elsewhere, not shown.

```sql
@attribute(cql:blob_storage)
create table news_info(
  who text,
  what text,
  when_ long -- timestamp of some kind
  source text @create(6)
);
```

Additionally, since the storage is not backed by SQL with SQL's constraint system, default values and constraints are not allowed in a table marked
with `cql:blob_storage`; it's just data. Similarly, triggers, views, and indices may not use the "table".

### Where do you keep your blob storage?

Naturally, blob storage goes in a blob field, but recall CQL has discriminated types so we could make something like this:

```sql
create table info(
  id long primary key,
  news_info blob<news_info>
);
```

From a SQL perspective `news_info` is just a blob.  That means if you want to do a `WHERE` clause or something like that on the info,
you're out of luck.  Maybe you could write a user-defined function to crack the blob and so forth but really this isn't the point.
If you're using this feature then, by construction, you don't need to index on this data.  It's simply not suitable for use at all
if you need field-at-a-time access within SQL.

### How do I make one of these blobs?

The natural place that CQL stores structures is in value cursors so the most natural thing to do is to provide a variation of the `SET`
statement that lets you load a blob from a cursor like so:

```sql
create proc make_blob(like news_info, out result blob<news_info>)
begin
  declare c cursor like news_info;
  fetch c from arguments;
  set result from cursor c;
END;
```

This declares a cursor, loads it from argument values, and converts it to a blob.  Of course all of the usual cursor building forms can be
used to power your blob creation, you just do one serialization at the end.  The above is assembling a blob from arguments but you could
equally make the blob from data.

```sql
create proc get_news_info(id_ long not null, out result blob<news_info>)
begin
   -- use our columns sugar syntax for getting just news_info columns from
   -- a table with potentially lots of stuff (or an error if it's missing columns)
   declare c cursor for
     select columns(like news_info) from some_source_of_info where info.id = id_;
   fetch c;
   set result from cursor c;
END;
```

There are *many* cursor fetch forms, including dummy data forms and other interesting bits of sugar.  You can fetch a cursor from arguments,
from other cursors, and even combinations.  We want all of that to work for blobs as well without adding tons of new syntax and code generation.
The obvious way to accomplish that is for cursors to be the source of blobs.

### How do I unpack one of these blobs?

Again, the normal way that you work with records in CQL is by creating suitable cursors. These can be economically accessed on a field-by-field basis.
What we need is a way to easily recreate a cursor from the blob so we can read the data values. This gives rise to this form:

```sql
let b := (select news_info from info where id = id_ if nothing null);
declare c cursor like b;
fetch c from b; -- note this can fail
-- now use c.who, c.what, etc.
```

Data loaded in a cursor is very economical to access on a field-by-field basis, and, since the deserialization of the blob happens all at once, that is also economical.
Importantly, we cannot assume that the blob is well formed, it could be coming from anywhere.  For secure-code reasons we must assume it is hostile.  Hence the
decoding validates the shape, internal lengths, and so forth.

If we had instead started with something this:

```sql
let b := (select news_info from info where id = id_ if nothing null);
```

Then maybe we might like to write:
```
if b.who == 'U2') then ... end if;
```

However, this sort of thing would be very uneconomical. For one thing, the blob does not have fixed-offset fields: It is carrying all the serialized data for the string fields
and so forth.  Each "dot" operation would be costly and, furthermore, each "dot" operation could fail if the blob is badly formed.  Having to deal with a `b.who` that might fail
seems very bad indeed.

Once you have the cursor you can make new blobs with different combinations, slice the cursor fields using the `LIKE` operator, return the cursor with `OUT`, or `OUT UNION`,
or pass the blob fields as arguments to functions using the `FROM` forms. Cursors already are super flexible in terms of what you can do with their contents.

### What is the representation of one of these blobs?

It's important that we allow the blobs to evolve over time, so each blob has to be self-describing.  We also want to be able to throw an error if you use the wrong kind of blob when
loading a cursor, so the blob has to contain the following:

* the number of columns in the blob data type when it was stored
* the type of each field is encoded as a single plain-text character
  * the types are bool, int, long, (double) real, (string) text, blob;
  * we use 'f' (flag) for bools, hence "fildsb"
  * these are encoded with one letter each, upper case meaning 'not null' so the storage might be "LFss"
  * the buffer begins with a null terminated string that serve for both the count and the types
* Each nullable field may be present or null; 1 bit is used to store this fact.  The bits are in an array of bytes that comes immediately after the type info (which implicitly tells us its size)
* Boolean values are likewise encoded as bits within the same array, so the total number of bits stored is nullables plus booleans (nullable booleans use 2 bits)
* If you are reading a newer version of a record from an older piece of data that is missing a column then the column is assumed to be NULL
* Any columns you add after the initial version (using @create) must be nullable; this is normal for adding columns to existing schema
* Integers and longs are stored in varint format after zigzag encoding
* Text is stored inline in null terminated strings (embedded nulls are not allowed in CQL text)
* Nested blobs are stored inline, with a length prefix encoded like any other int


### What about more than one row in a blob?

Well, this is a bit more advanced but in principle this could be done as well.  To make it useful, we would want to make a new cursor type that can iterate over rows in a blob. The syntax leaves
room for this, something like so:

```sql
declare c cursor for blob b;
loop fetch c
begin
  -- the usual stuff
end;
```

This cursor would be another variation; it would keep its current index into the blob to read data out of it.  Such a blob would also have to include a count of rows as part of its storage.

However, that's future looking. There is no such support at present.

### Conclusion

With a fairly modest amount of work, we now support structured storage natively and have pretty rich language constructs.  We carefully chose language constructs that lead to economical
serialization and deserialization patterns and a record format that is versioned well, without resorting to something super loose like JSON.

As with many other features, it's possible to replace the (de)serialization with code of your choice by supplying your own runtime methods.  So for instance, thrift encoding is possible;
though it is more flexible than is strictly necessary for the few SQL data types, it might be convenient.

Storage types that are going to be persisted in the database or go over a wire-protocol should be managed like schema with the usual validation rules.  On the other hand,
formats that will be used only transiently in memory can be changed at whim from version to version.  As mentioned above, the design specifically considers cases where a new
client discovers and old-format blob (with fewer columns) and, the reverse, cases where an old client recieves a datagram from a new client with too many columns.

#### Appendix

A more complete example is included for reference.

```sql
@attribute(cql:blob_storage)
create table news_info(
  who text,
  what text,
  when_ long -- timestamp of some kind
);

-- a place where the blob appears in storage
create table some_table(
  x integer,
  y integer,
  news_blob blob<news_info>
);

-- a procedure that creates the blob from loose args
create proc make_blob(like news_info, out result blob<news_info>)
begin
  declare c cursor like news_info;
  fetch c from arguments;
  set result from cursor c;
end;

-- a procedure that cracks the blob
create proc crack_blob(data blob<news_info>)
begin
  declare c cursor like news_info;
  fetch c from data;
  out c;
end;

-- a procedure that cracks the blob into loose args if needed
-- the OUT statement was created specifically to allow you to avoid this sort mass OUT awfulness
create proc crack_blob_to_vars(
  data blob<news_info>,
  out who text,
  out what text,
  out when_ long)
begin
  declare c cursor like news_info;
  fetch c from data;
  set who := c.who;
  set what := c.what;
  set when_ := c.when_;
end;

-- this just defines a shape for the part we are keeping from the original structure
declare proc my_basic_columns() (
  x int,
  y int
);

-- this just defines a shape for the result we want
-- we're never actually defining this procedure
declare proc my_result_shape() (
  like my_basic_columns,
  like news_info
);

create proc select_and_crack(whatever_condition bool)
begin
  declare c cursor for select * from some_table where whatever_condition;
  loop fetch c
  begin
    -- crack the blob in c
    declare n cursor like news_info;
    fetch n from blob c.news_blob;

    -- assemble the result we want from the parts we have
    declare result cursor like my_result_shape;
    fetch result from values (from c like my_basic_columns, from n);

    -- emit one row
    out union result;
  end;
end;
```

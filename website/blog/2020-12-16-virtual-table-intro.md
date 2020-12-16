---
slug: virtual-table-into
title: Introducing  Virtual Tables
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

Language support for virtual tables has lagged since I always thought they were of little interest to
the language anyway. The `CREATE TABLE` forms in general are only declarations (except if you're doing
the schema installer/upgrader output) and so you could just declare say a temp table that corresponds to the
virtual table that you made in the same way that you might declare say `sqlite_master` if you wanted to use it.
And since you have to register the module anyway, you may as well create the virtual table at the same time.

So there was no point in adding language support for the thing.

Furthermore the `CREATE VIRTUAL TABLE` form includes no information about the schema of the table so you'd
need some kind of declaration anyway in order to tell the language what the columns are for the table you
just created.  So again you may as well just declare it like a normal table and not include that table in your
schema upgrade file and be done with it.

And that was my thinking for the last 2 years. And then I learned something.

Virtual tables are durable.

Yeah, I always assumed that virtual tables were temp tables and they vanish and have to be redeclared every
session but that is not the case.  They are part of the durable schema so while you must pre-register the
module associated with the virtual table, the virtual table is like other tables in that you only create it
once and from then on it's part of the schema every time the database loads until you `DROP` it.

This changes everything.

With virtual tables being durable they belong in the schema upgrade process.  And if they go there they also
have to go into the JSON output.  But we can't use the vanilla syntax that SQLite uses because that syntax is:

* not parseable, because the module arguments can be literally anything (or nothing), even a letter to your gramma.
* the arguments do not necessarily say anything about the table's schema at all

So in the CQL langauge I change the syntax a bit, the generalized form looks like this:

```
create virtual table virt_table using my_module [(module arguments)]  as (
  id integer not null,
  name text
);
```

The part after the `AS` is used by CQL as a table declaration for the virtual table.  The grammar for that
is exactly the same as a normal `CREATE TABLE` statement.  However that part is not transmitted to
SQLite; when the table is created, SQLite sees only the part it cares about, the part before the `AS`.

Now this leaves the module arguments, they can be one of three things:

1. no arguments at all
2. a list of identifiers, constants, and parenthesized sublists just like in the `@attribute` form
3. the words `arguments following`


### Case 1 Example

```
create virtual table virt_table using my_module as (
  id integer not null,
  name text
);
```

becomes (to SQLite)

```
CREATE TABLE virt_table USING my_module;
```

Note: empty arguments `USING my_module()` are not allowed in the SQLite docs but do seem to work in SQLite.
We take the position that no args should be done with no parens, at least for now.

### Case 2 Example

```
create virtual table virt_table using my_module(foo, 'goo', (1.5, (bar, baz))) as (
  id integer not null,
  name text
);
```

```
CREATE VIRTUAL TABLE virt_table USING my_module(foo, "goo", (1.5, (bar, baz)));
```

This form allows for very flexible arguments but not totally arbitary arguments, so it can still be
parsed and validated.

### Case 3 Example

This case recognizes the popular choice that the arguments are often the actual schema declaration
for the table in question. So

```
create virtual table virt_table using my_module(arguments following) as (
  id integer not null,
  name text
);
```

becomes

```
CREATE VIRTUAL TABLE virt_table USING my_module(
  id INTEGER NOT NULL,
  name TEXT
);
```

The normalized text (keywords capitalized, whitespace normalized) of the table declaration in the `as` clause is used as the arguments.


### Other details

Virtual tables go into their own section in the JSON and they include the `module` and `moduleArgs` entries, they are additionally
marked `isVirtual` in case you want to use the same processing code for virtual tables as normal tables.  The JSON format is otherwise
the same, although some things can't happen in virtual tables (e.g. there is no `TEMP` option so `"isTemp"` must be false in the JSON.

For purposes of schema processing, virtual tables are on the `@recreate` plan, just like indices, triggers, etc.  This is the only option since
the `alter table` form is not allowed on a virtual table.

Semantic validation enforces "no alter statements on virtual tables" as well as other things like, no indices, and no triggers, since SQLite
does not support any of those things.

Finally, because virtual tables are on the `@recreate` plan, you may not have foreign keys that reference virtual tables. Such keys seem
like a bad idea in any case.

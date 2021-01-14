---
slug: named-types-into
title: Introducing Named Types
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key
for an entity might need to be `LONG` or even always `LONG NOT NULL` or `LONG NOT NULL @SENSITIVE` and the only
way to do this in the past was maybe with some `#define` thing.  Otherwise you have to diligently get the type right
in all the places, and should it ever change, again you have to visit all the places.   To help with this situation,
and to make code a little more self-describing we add named types to the language.  This is a lot like `typedef` in
the C language.  They do not create different incompatible types but do let you name things well.

You can now write these sorts of forms:

```
declare foo_id type long not null;

create table foo(
  id foo_id primary key autoincrement,
  name text
);

create proc inserter(name_ text, out id foo_id)
begin
  insert into foo(id, name) values(NULL, name_);
  set id := last_insert_rowid();
end;
```

Refer to the [railroad diagram](https://cgsql.dev/program-diagram#declare_stmt) for the grammar details.

Additionally any enumerated type can be used as a type name.  e.g.

```
declare enum thing integer (
  thing1,
  thing2
);

declare x thing;
```

Enumerations always get "not null" in addition to their base type.

This isn't a very complex feature but we hope that it will help create clearer code that is less likely to have type-related bugs.

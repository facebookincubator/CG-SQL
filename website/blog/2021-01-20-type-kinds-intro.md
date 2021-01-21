---
slug: type-kinds-intro
title: Introducing Type "Kinds"
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

Further adding to the type calculus of the CQL language we introduced the ability to encode the "kind" of primtive types.  This
can be used in a number of ways, like "units" for natural things and like a "type" for synthetic keys and other such.  It's easier
to illustrate by example.

```
declare job_id type long<job_id>;
declare person_id type long<person_id>;

declare j job_id;
decalre p person_id;

set p := j;  -- this is an error
```

in face other expressions like  `p == j` would also produce errors as these `long` values are no longer type compatible.  This is
a great way to add enforcement to your schema.  Likewise you can use this to add "units" to your data types.  e.g.

```
declare meters type real<meters>;
declare grams type real<meters>;
```

Variables of type `grams` are not compatible with variables of type `meters` even though both are `real`.

Likewise attemping to insert `grams` into a column that is typed to `meters` will give errors.  Of course SQLite doesn't know about any of this so all the `<>` stuff is
removed in the generated SQL.  This is just about type enforcement at compile time.

Enumerations like:

```
declare enum surface integer (paper, canvas);
declare enum writer integer (pen, paper, brush);
```

enables this:

```
declare s surface;                  -- s is now of type integer<surface>
declare w writer;                   -- w is now of type integer<writer>
set s := surface.paper;             -- ok
set s := writer.pen;                -- error
set w := writer.pencil;             -- ok
case when s == w then 1 else 0 end; -- error (w/s not comparable)
set w := s;                         -- error again
```

additionally in the database:

```
create table draw_action(
  w writer,
  s surface
);

insert into draw_action values(w, s); -- ok
insert into draw_action values(s, w); -- error!
```

So the types can be quite helpful when dealing with loose variables.

The notion of specific types was added to the language nearly two years ago to support the `object` type because there was a great desire
to prevent `object<dictionary>` being assigned from `object<list>` but this type kind, whether it's with units (e.g. "meters", "grams")
or a type name (e.g. "job_id") adds a lot of high valued type checking.

The kind can be added, stripped, or changed with a `cast` operation and the type system allows a constant or varable with no kind (e.g. "1")
to mix and match with anything.  So you get the most value by using the specific type consistently but you won't go insane adding test cases
that use constants for instance.

As of this writing the expression kinds are checked for compatibility everywhere except for the `update` statement which should join the fun tomorrow.

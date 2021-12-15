---
slug: shared-fragments-intro
title: Introducing Shared Fragments
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

Shared fragments are a real game-changer for CQL.

Remember, these are designed to let you write part of a query and then substitute in parameters.  So it's like a parameterized view in normal SQL terms.  But actually it's more powerful than that, fragments also provide features that are more like Java generics.  Let's do some examples.

Suppose we have a procedure which looks something like this:

```SQL
CREATE PROC get_stuff(to_include_ text, to_exclude_ text)
BEGIN
  WITH
  to_exclude_recursive_query (tok, rest) AS (
    SELECT
      '',
      to_exclude_ || ','
    UNION ALL
    SELECT
      substr(rest, 1, instr(rest, ',') - 1),
      substr(rest, instr(rest, ',') + 1)
    FROM to_exclude_recursive_query
    WHERE rest <> ''
  ),
  to_exclude (id) AS (
    SELECT CAST(tok AS LONG)
    FROM to_exclude_recursive_query
    WHERE tok <> ''
  )
  to_include_recursive_query (tok, rest) AS (
    SELECT
      '',
      to_include_ || ','
    UNION ALL
    SELECT
      substr(rest, 1, instr(rest, ',') - 1),
      substr(rest, instr(rest, ',') + 1)
    FROM to_include_recursive_query
    WHERE rest <> ''
  ),
  to_include (id) AS (
    SELECT CAST(tok AS LONG)
    FROM to_include_recursive_query
    WHERE tok <> ''
  )
  SELECT * from stuff S
  WHERE
    S.id in (select * from to_include) AND
    S.id not in (select * from to_exclude);
END;
```

With shared fragments you could write something like this:

```SQL
@attribute(cql:shared_fragment)
CREATE PROC split_commas(str text)
BEGIN
  WITH splitter(tok, rest) AS (
    SELECT '', IFNULL(str || ',', '')
    UNION ALL
    SELECT
      substr(rest, 1, instr(rest, ',') - 1),
      substr(rest, instr(rest, ',') + 1)
    FROM splitter
    WHERE rest <> '')
  select tok from splitter where tok <> '';
END;

@attribute(cql:shared_fragment)
CREATE PROC ids_from_string(str text)
BEGIN
  WITH toks(tok) AS (CALL split_commas(str))
  SELECT CAST(tok AS LONG) AS id from toks;
END;
```

We now have a shared fragment called `split_commas` which can be anywhere like maybe in a standard include file.  There are some immediate benefits:

* the fragment is compiled on its own before usage so any errors are reported in the fragment
  * in contrast, with macros you get errors when you try to use the macro and they are all charged to the line the macro appears on so it's hopeless figuring out what's wrong
* the text of the shared fragment will be the same, so it can be re-used in all locations, this can be a big binary size savings
  * in contrast, macros are pre-processed before CQL ever sees the text so it doesn't "know" it's the same code
* fragments compose cleanly as we'll see; and they have typed arguments
* fragments can be independently tested outside of the context in which they appear
  * make a test context and explore the fragment, no worries about it breaking on edge cases later


The first fragment called `split_commas` does exactly what it sounds like, it takes a string argument and makes a list of the strings in it.

The second fragment uses the first to split a string and then it converts all the strings to long integers.

Now instead of the above we could write:

```SQL
#include <stringsplit.sql> /* whereever you put the fragments */

CREATE PROC get_stuff(to_include_ text, to_exclude_ text)
BEGIN
  WITH
    to_include(id) AS (CALL ids_from_string(to_include_)),
    to_exclude(id) AS (CALL ids_from_string(to_exclude_))
  SELECT * from stuff S
  WHERE
    S.id in (select * from to_include) AND
    S.id not in (select * from to_exclude);
END;
```

And of course since `ids_from_string` is somewhere shared (`stringsplit.sql`) so these fragments can be used
all over your code and you'll only pay for the text one time.  This gives you great flexibility, very much
like parameterized views. You can have any number of these fragments, they will share code, they compose like crazy
and there is no schema cost!

### Generics

A series of useful fragments for generating data would go a long way but there are other applications
of fragments and you might want to operate on various data sources without hard coding them all.  This is
where the generic form of fragments comes in. Consider a case where you want to be able to filter `stuff`
by say name and age.  You could create this fragment:

```SQL
@attribute(cql:shared_fragment)
CREATE PROC filter_stuff(pattern_ text not null, min_age_ integer not null, max_age_ integer not null)
BEGIN
  WITH
    source(*) LIKE stuff
  SELECT * from source S
  WHERE
    S.name LIKE pattern_ AND
    S.age BETWEEN min_age_ and max_age_;
END;
```

Now imagine that we had added the shared fragment annotation to `get_stuff` (just like the above).
We could then write the following:

```SQL
CREATE PROC the_right_stuff(to_include_ text, to_exclude_ text, pattern_ text not null, min_age_ integer not null, max_age_ integer not null)
BEGIN
  WITH
    get_stuff(*) AS (call get_stuff(to_include_, to_exclude_)),
    filter_stuff(*) AS (call filter_stuff(pattern_, min_age_, max_age_) using get_stuff as source)
  SELECT * from filter_stuff S
  ORDER BY name
  LIMIT 5;
END;
```

Or with some sugar to forward arguments and assume the CTE name matches, more economically:

```SQL
CREATE PROC the_right_stuff(to_include_ text, to_exclude_ text, pattern_ text not null, min_age_ integer not null, max_age_ integer not null)
BEGIN
  WITH
    (call get_stuff(*)),
    (call filter_stuff(*) using get_stuff as source)
  SELECT * from filter_stuff S
  ORDER BY name
  LIMIT 5;
END;
```

The arg syntax `(*)` simply indicates that the arg names in the caller should match to the same names in the callee.  In
general `call foo(*)` expands to `call foo(from arguments like foo arguments)`.  `*` is rather more economical than that.

In this example `filter_stuff` doesn't know where its data will be coming from, you bind its table parameter `source`
to a compatible data source of your choice. For example, this would also be legal:

```SQL
CREATE PROC almost_the_right_stuff(pattern_ text not null, min_age_ integer not null, max_age_ integer not null)
BEGIN
  WITH
    (call filter_stuff(*) using stuff as source)
  SELECT * from filter_stuff S
  ORDER BY name
  LIMIT 5;
END;
```

### Conditionals

It's often desirable to have some options in the generated SQL without having to fork your entire query.  Shared
fragments address this as well with the conditional form.  In this form the top level of the fragment is an
`IF` statement and there are a number of alternatives.  Here are some simple modifications to the above that illustrate
some of the possibilities.

```SQL
@attribute(cql:shared_fragment)
CREATE PROC filter_stuff(pattern_ text, min_age_ integer not null, max_age_ integer not null)
BEGIN
  IF pattern_ IS NOT NULL THEN
    WITH
        source(*) LIKE stuff
    SELECT * from source S
    WHERE
        S.name LIKE pattern_ AND
        S.age BETWEEN min_age_ and max_age_;
  ELSE
    WITH
        source(*) LIKE stuff
    SELECT * from source S
    WHERE
        S.age BETWEEN min_age_ and max_age_;
  END IF;
END;
```

In the above if the input pattern is NULL then it is not considered, it won't be part of the generated SQL at all. Note that
`source` (same name) appears in both branches and therefore must be the same type as it will be fulfilled by one actual table
parameter.

Now the above could have been achieved with something like this:

```SQL
pattern_ IS NULL OR S.name LIKE pattern_
```

But that would have no useful selectivity.  But in general you might be able to avoid joins and so forth
with your constraints.  Consider something like this hypothetical:

```SQL
@attribute(cql:shared_fragment)
CREATE PROC filter_stuff(pattern_ text, min_age_ integer not null, max_age_ integer not null)
BEGIN
  IF pattern_ IS NOT NULL THEN
    WITH
        source(*) LIKE stuff
    SELECT DISTINCT S.* from source S
    INNER JOIN keywords K
    WHERE
        K.keyword LIKE pattern_ AND
        S.age BETWEEN min_age_ and max_age_;
  ELSE
    WITH
        source(*) LIKE stuff
    SELECT * from source S
    WHERE
        S.age BETWEEN min_age_ and max_age_;
  END IF;
END;
```

Here we save the DISTINCT and the JOIN if there is no pattern which might be important.  Of course
there are probably better ways to match keywords but this is just an illustration of what's possible.

There are numerous ways this flexibility can be used, again a simple example, a real schema
transform would be more complex.

```SQL
@attribute(cql:shared_fragment)
CREATE PROC get_stuff(to_include_ text, to_exclude_ text, schema_v2 bool not null)
BEGIN
  IF schema_v2 THEN
    WITH
        to_include(id) AS (CALL ids_from_string(to_include_)),
        to_exclude(id) AS (CALL ids_from_string(to_exclude_))
    SELECT * from stuff_2 S
    WHERE
        S.id in (select * from to_include) AND
        S.id not in (select * from to_exclude);
  ELSE
    WITH
        to_include(id) AS (CALL ids_from_string(to_include_)),
        to_exclude(id) AS (CALL ids_from_string(to_exclude_))
    SELECT * from stuff S
    WHERE
        S.id in (select * from to_include) AND
        S.id not in (select * from to_exclude);
   END IF;
END;
```

### Validation

All of this requires a bunch of checking, at least this:

* the LIKE forms can only appear in a shared fragment
* the CALL forms must refer to shared fragments
* the CALL args must be compatible
* the number and type of the provided tables in USING must be correct
* the shared fragment must be a single select statement or an IF statement with an ELSE
  * the statement lists of the IF/ELSE combo must all be single select statements
  * all the choices in the IF block must return the same shape (this is normal for procedures)
* the shared fragment can't have any out arguments
* the provided fragment arguments cannot themselves use the nested SELECT construct

I think this is a total game changer for SQL authoring and should go a long way to making it easier to get your work done
on SQLite. A good base set of shared fragments as part any suite of procedures seems like a good idea.

There are more details in the section on shared fragments in [Chapter 14](https://cgsql.dev/cql-guide/ch14) of The Guide.

These features are in the current build as of today (12/14/2021).

Happy Holidays and stay safe.

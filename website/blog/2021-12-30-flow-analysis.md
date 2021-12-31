---
slug: flow-analysis
title: Control Flow Analysis in CQL
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

One of the biggest changes to CQL in 2021 was the addition of control flow
analysis. Given an understanding of how execution can flow within a user's
program, CQL can do things like infer when a nullable variable must contain a
nonnull value and improve its type appropriately, or issue an error when a
nonnull variable may be used before it has been initialized.

### Improving Nullability

As of mid-2021, and with increasing sophistication throughout the remainder of
the year, CQL has been able to infer that a variable of a nullable type must not
be `NULL` within a portion of a user's program:

```sql
DECLARE PROC another_proc(t0 TEXT NOT NULL, t1 TEXT NOT NULL);

CREATE PROC some_proc(t0 TEXT, t1 TEXT)
BEGIN
  IF t0 IS NULL RETURN;
  -- `t0` must be nonnull here if we made it this far

  IF t1 IS NOT NULL THEN
    -- `t0` and `t1` are nonnull here
    CALL another_proc(t0, t1);
  ELSE
    -- `t0` is nonnull here
    CALL another_proc(t0, "default");
  END IF;
END;
```

The ability of the CQL compiler to infer non-nullability greatly reduces the
need to use the functions `ifnull_crash` and `ifnull_throw` to coerce values to
a nonnull type—functions that, if they are ever used incorrectly, usually result
in programs misbehaving.

For a detailed description and many additional examples of what is possible—CQL
can handle much more than what is shown above—see [the user guide's section on
nullability
improvements](https://cgsql.dev/cql-guide/ch03#nullability-improvements).

### Enforcing Initialization Before Use

In CQL, it is possible to declare a variable of a nonnull type without giving it
a value. If the variable is of a non-reference type, it is assigned a default
value of `0`. If the variable is of a reference type (`BLOB`, `OBJECT`, or
`TEXT`), however, it is simply set to `NULL` despite the nonnull type as no
default value exists.

To help prevent accessing a reference variable of a nonnull type and getting
back `NULL`, CQL recently began enforcing that such variables are initialized
before use. The following code, therefore, now results in an error:

```sql
DECLARE t TEXT NOT NULL;
CALL requires_text_notnull(t); -- error!
```

Using the same engine for control flow analysis that is behind nullability
improvements, CQL can improve a variable to be initialized:

```sql
DECLARE t TEXT NOT NULL;

IF some_condition THEN
  SET t := "some example text";
  -- `t` is initialized here
ELSE
  THROW;
END IF;
-- `t` must be initialized here if we made it this far

CALL requires_text_notnull(t); -- okay!
```

Thanks to CQL's ability to understand the control flow of users' programs, the
above example works just fine.

CQL now also enforces that all procedures with `OUT` parameters of a nonnull
reference type properly initialize said parameters before they return:

```sql
CREATE PROC some_proc(b BOOL NOT NULL, OUT t TEXT NOT NULL)
BEGIN
  IF b THEN
    SET t := another_proc(t);
    -- `t` is initialized here
  ELSE
    SET t := yet_another_proc(t);
    -- `t` is initialized here
  END IF;
  -- `t` must be initialized here because all possible
  -- branches initialized it, so `some_proc` is okay!
END;
```

As with nullability improvements, understanding the nuances of what will be
considered initialized is easier if one has a sense for how control flow
analysis works in the compiler.

### Understanding Control Flow Analysis in CQL

To develop an intuition for how control flow analysis works in CQL, let's begin
by taking a look at the following example:

```sql
DECLARE PROC p1(OUT t TEXT NOT NULL);
DECLARE PROC p2(i INTEGER NOT NULL, OUT t TEXT NOT NULL);

CREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)
BEGIN
  IF i IS NULL THEN
    IF b THEN
      CALL p1(t);
    ELSE
      SET t := "";
    END IF;
    RETURN;
  END IF;

  IF i == 0 THEN
    SET t := "";
  ELSE IF i > 0 THEN
    SET t := p2(i);
  ELSE
    THROW;
  END IF;
END;
```

There are a couple of things we must verify in order to ensure the code is
type-safe:

- With regard to the parameters of `p0`: Since `t` is an `OUT` parameter of type
  `TEXT NOT NULL`, `p0` must always assign it a value before it returns. If it
  does not, a caller of `p0` may end up with a variable of a `NOT NULL` type
  that actually contains `NULL`.

- With regard to the calling of `p2` in `p0`: Since `p2` requires a first
  argument of type `INTEGER NOT NULL`, some sort of check must be performed to
  ensure that `i` is not `NULL` before `p2(i)` is executed.

If we carefully study `p0`, we can determine that both of the above conditions
are satisfied. Making this determination, however, is not exactly trivial, and
real-world code is often significantly more complicated than this—and it evolves
over time. For these reasons, having a compiler that can make such
determinations automatically is critical; most modern production compilers
perform these sorts of checks.

The easiest way to understand how CQL does its job is to take the above example
line-by-line. This is not exactly how CQL works under the hood, but it should
provide an intuitive sense of how control flow analysis works in the compiler:

```sql
==> CREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)
    BEGIN
      ...
    END;
```

Right away, CQL can see that `t` is declared both `OUT` and `TEXT NOT NULL` and
thus requires initialization before `p0` returns. CQL can, therefore, add a fact
about what it is analyzing to its previously null set of facts:

> * `t` requires initialization.

We can then continue:

```sql
==>   IF i IS NULL THEN
        ...
      END IF;
```

Here, the compiler notices that we're at an `IF` statement. In CQL, `IF`
statements contain one or more **branches**, and the compiler considers every
`IF` to be the start of a **branch group**. The same line also indicates the
condition for the first branch: `i IS NULL`. CQL can update its set of facts:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:

It then proceeds to the next line:

```sql
      IF i IS NULL THEN
    ==> IF b THEN
          CALL p1(t);
        ELSE
          SET t := "";
        END IF;
        RETURN;
      END IF;
```

Another branch group and branch:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:
>         * In branch group:
>             * In branch when `b`:

Continuing:

```sql
      IF i IS NULL THEN
        IF b THEN
      ==> CALL p1(t);
        ELSE
          SET t := "";
        END IF;
        RETURN;
      END IF;
```

Since `p1` takes an `OUT` argument of type `TEXT NOT NULL`, this call
initializes `t`, and so CQL can update its set of facts once again:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:
>         * In branch group:
>             * In branch when `b`:
>                 * 't' is initialized.

Jumping ahead a couple of lines:

```sql
      IF i IS NULL THEN
        IF b THEN
          CALL p1(t);
        ELSE
      ==> SET t := "";
        END IF;
        RETURN;
      END IF;
```

At this point, we're in another branch. We also have yet another fact to add
because `t` is initialized here as well due to the `SET`:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:
>         * In branch group:
>             * In branch when `b`:
>                 * 't' is initialized.
>             * In ELSE branch:
>                 * 't' is initialized.

Moving ahead one more line, things get a bit more interesting:

```sql
      IF i IS NULL THEN
        IF b THEN
          CALL p1(t);
        ELSE
          SET t := "";
    ==> END IF;
        RETURN;
      END IF;
```

Here, we're at the end of an `IF`, and thus the end of a branch group. Whenever
CQL reaches the end of a branch group, it *merges* the effects of all of its
branches.

One very important thing to note here is that the current branch group has an
`ELSE` branch, and so the set of branches covers all possible cases. That means
if something is initialized in every branch within the branch group, we can
consider it to be initialized after the branch group has ended: Initialization
will always occur. This allows CQL to simplify its set of facts as follows as it
leaves the branch group:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:
>         * 't' is initialized.

Stepping forward one line again, we reach a `RETURN`:

```sql
      IF i IS NULL THEN
        ...
    ==> RETURN;
      END IF;
```

We're now at a point where we can exit the procedure. CQL will, therefore,
verify that if something requires initialization, it has been initialized. Since
we have both the facts "`t` requires initialization" and "`t` is initialized",
all is well!

The fact that the current branch returns early is added to the set of facts:

> * `t` requires initialization.
> * In branch group:
>     * In branch when `i IS NULL`:
>         * 't' is initialized.
>         * Returns.

Moving ahead one more line, we reach the end of another branch and branch group,
and again something interesting happens:

```sql
      ...
      IF i IS NULL THEN
        ...
  ==> END IF;
```

Upon ending the branch group, we know that the branch group has exactly one
branch, that the branch is entered only when `i IS NULL`, and that the branch
returns. What that tells CQL is that, if execution is going to continue after
the branch group, its sole branch must *not* have been taken, and so CQL knows
the *opposite* of its condition for entry will be true from this point onward:

> * `t` requires initialization.
> * `i` is not null.

The next `IF` is rather similar to what we've seen already in its structure, so
we can jump ahead several lines to the next point of interest:

```sql
      IF i == 0 THEN
        SET t := "";
      ELSE IF i > 0 THEN
    ==> SET t := p2(i);
      ELSE
        THROW;
      END IF;
```

*Before* we analyze the above-indicated line, we have the following set of facts:

> * `t` requires initialization.
> * `i` is not null.
> * In branch group:
>     * In branch when `i == 0`:
>         * 't' is initialized.
>     * In branch when `i > 0`:

In the call `p2(i)`, we know that `i` was declared to have type `INTEGER` and
that `p2` requires an `INTEGER NOT NULL`, but we also have the fact "`i` is not
null". For this reason, we can consider `p2(i)` to be a valid call. We can also
add the fact that `t` is initialized to our current set of facts:

> * ...
>     * In branch when `i > 0`:
>         * `t` is initialized.

**NOTE:** When it comes to code generation, it is not so simple as to say
`p2(i)` is valid and proceed as usual. That's because `p2` expects an argument
of type `INTEGER NOT NULL`, but we merely have a value of type `INTEGER` *that
we happen to know* cannot be null: `INTEGER NOT NULL` and `INTEGER` do not share
the same underlying representation, and so we cannot pass the declared-nullable
variable `i` directly to `p2`. To solve this problem, CQL *rewrites the
expression* such that `p2(i)` becomes `p2(cql_inferred_notnull(i))`, where
`cql_inferred_notnull` is an internal-only function that handles the
nullable-to-nonnull representational conversion for us. This explains its
presence in the following examples.

Jumping ahead again, we encounter a `THROW`:

```sql
      IF i == 0 THEN
        SET t := "";
      ELSE IF i > 0 THEN
        SET t := p2(cql_inferred_notnull(i));
      ELSE
    ==> THROW;
      END IF;
```

The fact that the branch will throw is added to the current set of facts:

> * `t` requires initialization.
> * `i` is not null.
> * In branch group:
>     * In branch when `i == 0`:
>         * 't' is initialized.
>     * In branch when `i > 0`:
>         * 't' is initialized.
>     * In ELSE branch:
>         * Throws.

We then proceed to the end of the `IF`:

```sql
      IF i == 0 THEN
        SET t := "";
      ELSE IF i > 0 THEN
        SET t := p2(cql_inferred_notnull(i));
      ELSE
        THROW;
  ==> END IF;
```

Once again, CQL merges the effects of the branches in the branch group to finish
the analysis of the `IF`. Since it can see that `t` was initialized in all
branches except the one that throws, and since the branches cover all possible
cases, the set of facts is simplified as follows given the knowledge that, if
`THROW` was not encountered, `t` must have been initialized:

> * `t` requires initialization.
> * `i` is not null.
> * `t` is initialized.

Moving ahead one final time, we encounter the end of the procedure:

```sql
    CREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)
    BEGIN
      ...
==> END;
```

The only thing left to do at this point is to validate that anything requiring
initialization has been initialized. Since we have both "`t` requires
initialization" and "`t` is initialized", everything is in order.

### Looking Ahead

As a recently generalized piece of functionality within the CQL compiler,
control flow analysis will soon be used to enforce additional properties of
users' programs. In particular, CQL will be able to ensure that cursors are
always fetched before they're used and that cursors are always checked to have a
row before their fields are accessed.

Hopefully you now understand the fundamentals of control flow analysis in CQL
and the benefits it brings to your programs. Best wishes for 2022!

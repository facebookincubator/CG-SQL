---
slug: declare-enum-intro
title: Introducing Declare Enum
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql, errors]
---

There is an unfortunate pattern of hard coding constants in SQL which I think comes from the
fact that there's not an especially good way to encode constants in SQL.  Things are a little
better In CG/SQL's CQL language because it's normal to run things through the pre-processor first
so you can do things like:

```
#define BUSINESS_TYPE_RESTAURANT 1
#define BUSINESS_TYPE_LAUNDROMAT 2
```

Having done so, you could write:

```
insert into Business using
   "Rico's Laundry"  name,
   BUSINESS_TYPE_LAUNDROMAT type;

-- by the time SQL sees this it becomes
insert into Business(name, type) values('Rico''s Laundry', 2);
```

And at least you don't have to see these loose '2' values all over. An especially unfortunate
form is the below, in which the auther is clearly crying for a symbol to use:

```
insert into Business using
   "Rico's Laundry"  name,
   2 type; /* laundromat */
```

But if we use `#define` the language knows nothing of the names and it can't help you manage them
or export them consistently or anything like that.  I guess `#define` is pretty useful in several
langauges (C and C++) so you could maybe `#include` the macros somehow but that doesn't seem
especially great.  And if you need them in Java you're getting no help at all.

So to this world we add enumerated constants.  This is a bit short of enumerated types as we'll
see later.  You can now write something like this:

```
declare enum business_type integer (
  restuarant,
  laundromat,
  corner_store = 11+3  /* math added for demo purposes only */
);
```

After this:

```
select business_type.corner_store;
```
is the same as

```
select 14;
```

And that is exactly what SQLite will see, the literal 14.

What's going on here?  There's just a few rules:

* the enumeration can be any numeric type (bool, integer, long integer, real)
* the values of the enumeration start at 1 (i.e. if there is no `= expression` the first item will be 1, not 0)
* if you don't specify a value, the next value is the previous value + 1
* if you do specify a value it can be any constant expression and it will be cast to the type of the enumeration (even if thatis lossy)
* the enumeration can refer to previous values in itself with no qualification `(big = 100.0, medium = big/2, small = medium/2)`
* the enumeration can refer to previously defined enumerations as usual `(code = business_type.restaurant)`
* Once the enumeration is defined you refer to its members in a fully qualfied fashion `enum_name.member_name` elsewhere

Why is this better than macros?  Well for one thing the enum values can be checked at their declaration site, so if you
have errors you will hear about them in a more reasonable place.  But additionally since the structure is known to the
compiler it can give you useful information in the outputs.

In the `.h` files you get:

```
enum business_type {
  business_type__restaurant = 1,
  business_type__laundromat = 2,
  business_type__corner_store = 14
};
```

In case of floating point values such as:

```
declare enum floating real (
  one = 1.0,
  two = 2.0,
  e = 2.71828,
  pi = 3.14159
);
```

You get:

```
// enum floating (floating point values)
#define floating__one 1.000000e+00
#define floating__two 2.000000e+00
#define floating__e 2.718280e+00
#define floating__pi 3.141590e+00
```

Which is unfortunately the best you can do since C has no floating point enums.

But in both cases the `enums` section of the JSON has the name of the enums and their members and values ready to go.
With these values you can readily generate (with moustache or something) the language interfaces of your choice.  This
is a real help if you're trying to make helpers to call your CQL from say Java or something.

To do all this we needed to add some constant folding and general evaluation to the compiler.  It's not much,
just the normal numeric types and null.  The supported operations include:

`+`, `-`, `*`, `/`, `%`, `|`, `&`, `<<`, `>>`, `~`, `and`, `or`, `not`, `==`, `<=`, `>=`, `!=`, `<`, `>`, the `cast` operator
and the `case` forms.  These are enough to make a lot of very interesting expressions, all of which are envaluated at
compile time.

While the constant folding was added to allow for rich `enum` expressions, there is also the `const()` primitive in the
language now which can appear anywhere a literal could appear.  This allows you do things that were previously not
allowed such as:

```
create table something(
  x integer default const((1<<16)|0xf) /*  again the math is just for illustration */
);
```

The `const` form is also very useful in macros:

```
#define SOMETHING const(12+3)
```
This form ensures that the constant will be evaluated at compile time. Const can also also nest so you can build these
kinds of macros from other macros or you can build enums this way. Anywhere you might need literals, you can use `const`.

Importantly, no enumerated data types were added to the language to do any of this.  The values can help you to
achieve some correctness by avoiding transcription mistakes but there is no additional type-safety provided here.
Indeed given the rich mix between these types in SQLite, and with SQLite having no knowledge of enumerations at
all it would be tricky to do a complete job.  Still, this might happen in the future.

But for now, declaring constants that are really an intimate part of your schema is now possible and the addition
of the constants to the `.h` files and the `.json` output should hopefully make these generally useful.  At least
we might see less of the hard-coded constant business with good values baked right into the schema declarations.

Happy Holidays.

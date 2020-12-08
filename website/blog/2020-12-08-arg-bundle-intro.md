---
slug: arg-bungle-intro
title: Introducing  Argument Bundles
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql, errors]
---

There are many cases where stored procedures require complex arguments using data shapes well known
to higher level languages or that come from the schema.  There is already some affordance for this
sort of thing in the form of this kind of pattern (I'll continue to use this simple example as I
discuss the generalization below).

```
create table Person (
   id text primary key,
   name text not null,
   address text not null,
   birthday real
);
```

Then maybe something like this

```
create proc insert_person(like Person)
begin
    insert into Person from arguments;
end;
```

The above expands into:

```
create proc insert_person(
    id_ text not null,
    name_ text not null,
    address_ text not null,
    birthday_ real)
begin
    insert into Person(id, name, address, birthday)
        values(id_, name_, address_, birthday_);
end;
```

And I think we can all agree the sugared version is a lot easier to reason about
and much less prone to errors as well.

Those features have been in the language for a long time and that's all fine and well
but it isn't general enough to handle the usual mix of situations.  For instance what
if you need a procedure that works with two people?  A hypothetical `insert_two_people`
procedure cannot be written with the old form.  This is where argument bundles come in.
The idea here is to name the bundle which provides useful reference.  To wit:

```
create proc insert_two_people(p1 like Person, p2 like Person)
begin
    call insert_person(from p1);
    call insert_person(from p2);
end;
```

or alternatively

```
create proc insert_two_people(p1 like Person, p2 like Person)
begin
    insert into Person from p1;
    insert into Person from p2;
end;
```

So what's going on here?  Well, there are lots of reasons to keep the API to procedures simple
and adding general purpose structured types would be at odds with that.  It would require lots
of knowledge about C structure layout and whatnot.  And trying to call from java would require
very complex JNI for any such procedure.  So we avoid all that.  We keep simple arguments.
The above expands into:

```
create proc insert_person(
    p1_id text not null,
    p1_name text not null,
    p1_address text not null,
    p1_birthday real,
    p2_id text not null,
    p2_name text not null,
    p2_address text not null,
    p2_birthday real)
begin
    insert into Person(id, name, address, birthday)
        values(p1_id, p1_name, p1_address, p1_birthday);
    insert into Person(id, name, address, birthday)
        values(p2_id, p2_name, p2_address, p2_birthday);
end;
```

Or course the types don't have to be the same, you can create and name shapes of your choice.  The language allow
you to use and argument bundle in all the places that you a cursor was previously a valid source.  So `insert`,
`fetch`, `update cursor`, and procedure calls.  You can refer to the arguments by their expanded name `p1_address`
or alternatively `p1.address` means the same thing.

Here's another example showing a silly but illustrative thing you could do:

```
create proc insert_lotsa_people(P like Person)
begin
    declare C cursor like P;
    fetch C from P;
    declare i integer not null;
    set i := 0;
    while (i < 20)
    begin
        update cursor C using
            printf("id_%d", i) id;
        insert into Person from C;
    end;
end;
```

The above illustrates that you can use a bundle as the source of a shape elsewhere and you can
use a bundle as a source of data to load a cursor.  After which you can do all the usual value cursor things.

In order to call these procedures more readily from other languages, the JSON output now includes additional
information about where procedure arguments originated;  this is creatively called "argOrigin:" it has 3 forms.

* "arg_name" -> the argument is not an expansion of anything
* "T arg_name" -> the argument came from `like T`
   * there will be one arg for each member of T
   * the formal argument name for this arg will be arg_name_
   * if T is procedure arguments `like p1 arguments` then you'll get  "p1[arguments] arg_name"
* "name T arg_name" -> the argument came from `name like T` (a named bundle)
   * there will be one arg for each member of T
   * the formal argument name for this arg will be T_arg_name
   * T could be procedure arguments as above
* If the source of an argument was a cursor or argument bundle name you get instead that thing's shape source name
  * this is always better because cursor names and bundle names are not globally unique.
* If the cursor had an anonymous source (e.g. `like select 1 x`) then you get the useless shape name "select"
  * this is an indicator that you should make some ad hoc struct for this procedure because there is no useful name for the arg bundle's type

None of this matters unless you're trying to make wrappers for a CQL procedure for some other language
and you'd like to have your wrapper deal with structs rather than all loose arguments.  the JSON
basically tells you the structs.

Interestingly argument bundles resulted in a significant reduction of code in the compiler.  The argument bundle
name has to be usable in the contexts where a cursor was previously usable.  It is another source of shaped data.
That proved to be super simple as they look almost identical to the compiler -- no coincidence there.  Very little
code was required to make `from [cursor_name]` work with `from [any_shape_name]` in the half dozen or so places
that this construct is allowed (e.g. procedure call arguments, insert statements, etc.).  However, there was as
much code associated with `from arguments` as there was `from cursor_name`.  And the code was nearly identical..

When argument bundles were introduced the natural thing to do was to create an artifical bundle called "arguments" which
represents the bundle that is ALL the arguments.  With that done all the code for `from arguments` could be deleted
because `arguments` itself was a valid shape name.  Hence `insert into T from arguments` "just works".  And so half
the rewrites were deleted.  The only cost was that the form `from arguments like shape` became the cursor form
`from arguments(like shape)` which only adds mandatory parens to a form that was largely unused anyway (there were 2
cases in our entire codebase).  The cursor form is more general as you can do from `C(like A, like B)` to get the
fields that match A then those that match B.  Arguments get this for free as well (well, at the cost of parens).

So overall, this feature was added and the compiler got smaller and cleaner.  Only the test suite had to grow.

Stay safe out there.

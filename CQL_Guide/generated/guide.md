<!--- @generated -->
## Chapter 1: Introduction
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL was designed as a precompiled addition to the SQLite runtime system.  SQLite lacks
stored procedures, but has a rich C runtime interface that allows you to create any kind
of control flow mixed with any SQL operations that you might need.  However, SQLite's programming
interface is both verbose and error-prone in that small changes in SQL statements can require
significant swizzling of the C code that calls them. Additionally, many of the SQLite runtime functions have error codes
which must be strictly checked to ensure correct behavior.  In practice, it's easy to get some or all of this wrong.

CQL simplifies this situation by providing a high level SQL language not unlike the stored procedure
forms that are available in client/server SQL solutions and lowering that language to "The C you could
have written to do that job using the normal SQLite interfaces."

As a result, the C generated is generally very approachable but now the source language does not suffer from
brittleness due to query or table changes and CQL always generates correct column indices, nullability
checks, error checks, and the other miscellany needed to use SQLite correctly.

CQL is also strongly typed, whereas SQLite is very forgiving with regard to what operations
are allowed on which data.  Strict type checking is much more reasonable given CQL's compiled programming model.

:::note
CQL was created to help solve problems in the building of Meta Platforms's Messenger application, but this
content is free from references to Messenger. The CQL code generation here is done in the simplest mode with the
fewest runtime dependencies allowed for illustration.
:::

### Getting Started

Before starting this tutorial, make sure you have built the `cql` executable first in [Building CG/SQL](/docs/getting-started#building).

The "Hello World" program rendered in CQL looks like this:

```sql title="hello.sql"
-- needed to allow vararg calls to C functions
declare procedure printf no check;

create proc hello()
begin
  call printf("Hello, world\n");
end;
```

This very nearly works exactly as written but we'll need a little bit of glue to wire it all up.

First, assuming you have [built](/docs/getting-started#building) `cql`, you should have the power to do this:

```bash
$ cql --in hello.sql --cg hello.h hello.c
```

This will produce the C output files `hello.c` and `hello.h` which can be readily compiled.

However, hello.c will not have a `main` -- rather it will have a function like this:

```c title="hello.c"
...
void hello(void);
...
```

The declaration of this function can be found in `hello.h`.


> Note: hello.h tries to include [`cqlrt.h`](https://github.com/facebookincubator/CG-SQL/blob/main/sources/cqlrt.h). To
> avoid configuring include paths for the compiler, you might keep `cqlrt.h` in the same directory as the examples and
> avoid that complication. Otherwise you must make arrangements for the compiler to be able to find `cqlrt.h` either by
> adding it to an `INCLUDE` path or by adding some `-I` options to help the compiler find the source.

That `hello` function is not quite adequate to get a running program, which brings us to the next step in
getting things running.  Typically you have some kind of client program that will execute the procedures you
create in CQL.  Let's create a simple one in a file we'll creatively name `main.c`.

A very simple CQL main might look like this:

```c title="main.c"
#include <stdlib.h>
#include "hello.h"
int main(int argc, char **argv)
{
   hello();
   return 0;
}
```

Now we should be able to do the following:

```bash
$ cc -o hello main.c hello.c
$ ./hello
Hello, world
```

Congratulations, you've printed `"Hello, world"` with CG/SQL!

### Why did this work?

A number of things are going on even in this simple program that are worth discussing:

* the procedure `hello` had no arguments, and did not use the database
  * therefore its type signature when compiled will be simply `void hello(void);` so we know how to call it
  * you can see the declaration for yourself by examining the `hello.c` or `hello.h`
* since nobody used a database we didn't need to initialize one
* since there are no actual uses of SQLite we didn't need to provide that library
* for the same reason we didn't need to include a reference to the CQL runtime
* the function `printf` was declared "no check", so calling it creates a regular C call using whatever arguments are provided, in this case a string
* the `printf` function is declared in `stdio.h` which is pulled in by `cqlrt.h`, which appears in `hello.c`, so it will be available to call in the generated C code
* CQL allows string literals with double quotes, and those literals may have most C escape sequences in them, so the "\n" bit works
  * Normal SQL string literals (also supported) use single quotes and do not allow, or need escape characters other than `''` to mean one single quote

All of these facts put together mean that the normal, simple linkage rules result in an executable that prints
the string "Hello, world" and then a newline.

### Variables and Arithmetic

Borrowing once again from examples in "The C Programming Language", it's possible to do significant control flow in CQL without reference to databases.  The following program illustrates a variety of concepts:

```sql
-- needed to allow vararg calls to C functions
declare procedure printf no check;

-- print a conversion table  for temperatures from 0 to 300
create proc conversions()
begin
  declare fahr, celsius integer not null;
  declare lower, upper, step integer not null;

  set lower := 0;   /* lower limit of range */
  set upper := 300; /* upper limit of range */
  set step := 20;   /* step size */

  set fahr := lower;
  while fahr <= upper
  begin
    set celsius := 5 * (fahr - 32) / 9;
    call printf("%d\t%d\n", fahr, celsius);
    set fahr := fahr + step;
  end;
end;
```

You may notice that both the SQL style `--` line prefix comments and the C style `/* */` forms [note you haven't used the second for of comment style yet]
are acceptable comment forms. Indeed, it's actually quite normal to pass CQL source through the C pre-processor before giving
it to the CQL compiler, thereby gaining `#define` and `#include` as well as other pre-processing options
like token pasting in addition to the aforementioned comment forms.  More on this later.

Like C, in CQL all variables must be declared before they are used.  They remain in scope until the end of the
procedure in which they are declared, or they are global scoped if they are declared outside of any procedure.  The
declarations announce the names and types of the local variables.   Importantly, variables stay in scope for the whole
procedure even if they are declared within a nested `begin` and `end` block.

The most basic types are the scalar or "unitary" types (as they are referred to in the compiler)

|type        |aliases      | notes                              |
|------------|-------------|------------------------------------|
|`integer`   |int          | a 32 bit integer                   |
|`long`      |long integer | a 64 bit integer                   |
|`bool`      |boolean      | an 8 bit integer, normalized to 0/1|
|`real`      |n/a          | a C double                         |
|`text`      |n/a          | an immutable string reference      |
|`blob`      |n/a          | an immutable blob reference        |
|`object`    |n/a          | an object reference                |

Note: SQLite makes no distinction between integer storage and long integer storage, but the declarations
tell CQL whether it should use the SQLite methods for binding and reading 64-bit or 32-bit quantities
when using the variable or column so declared.

There will be more notes on these types later, but importantly, all keywords and names in CQL
are case insensitive just like in the underlying SQL language.   Additionally all of the
above may be combined with `not null` to indicate that a `null` value may not be stored
in that variable (as in the example).  When generating the C code, the case used in the declaration
becomes the canonical case of the variable and all other cases are converted to that in the emitted
code.  As a result the C remains case sensitively correct.

The size of the reference types is machine dependent, whatever the local pointer size is.  The
non-reference types use machine independent declarations like `int32_t` to get exactly the desired
sizes in a portable fashion.

All variables of a reference type are set to `NULL` when they are declared,
including those that are declared `NOT NULL`. For this reason, all nonnull
reference variables must be initialized (i.e., assigned a value) before anything
is allowed to read from them. This is not the case for nonnull variables of a
non-reference type, however: They are automatically assigned an initial value of
0, and thus may be read from at any point.

The programs execution begins with three assignments:

```sql
set lower := 0;
set upper := 300;
set step := 20;
```

This initializes the variables just like in the isomorphic C code.  Statements are seperated by semicolons,
just like in C.

The table is then printed using a `while` loop

```sql
while fahr <= upper
begin
  ...
end;
```

This has the usual meaning, with the statements in the `begin/end` block being executed repeatedly
until the condition becomes false.

The body of a `begin/end` block such as the one in the `while` statement can contain one or more statements.

The typical computation of Celsius temperature ensues with this code:

```sql
set celsius := 5 * (fahr - 32) / 9;
call printf("%d\t%d\n", fahr, celsius);
set fahr := fahr + step;
```

This computes the celsuis and then prints it out, moving on to the next entry in the table.

Importantly, the CQL compiler uses the normal SQLite order of operations, which is NOT the C order of operations.
As a result, the compiler may need to add parentheses in the C output to get the correct order; or it may remove
some parentheses because they are not needed in the C order even though they were in the SQL order.

The `printf` call operates as before, with the `fahr` and `celsius` variables being passed on to the C runtime library
for formatting, unchanged.

NOTE: when calling unknown foreign functions like `printf` string literals are simply passed right through unchanged
as C string literals. No CQL string object is created.

### Basic Conversion Rules

As a rule, CQL does not perform its own conversions, leaving that instead to the C compiler.  An exception
to this is that boolean expressions are normalized to a 0 or 1 result before they are stored.

However, even with no explicit conversions, there are compatibility checks to ensure that letting the C compiler
do the conversions will result in something sensible.  The following list summarizes the essential facts/rules as
they might be applied when performing a `+` operation.

* the numeric types are bool, int, long, real
* non-numeric types cannot be combined with numerics, e.g. 1 + 'x' always yields an error
* any numeric type combined with itself yields the same type
* bool combined with int yields int
* bool or int combined with long yields long
* bool, int, or long combined with real yields real

### Preprocessing Features

CQL does not include its own pre-processor but it is designed to consume the output of the C pre-processor.  To do this, you can either write the output of the pre-processor to a temporary file and read it into CQL as usual or you can set up a pipeline something like this:

```bash
$ cc -x c -E your_program.sql | cql --cg your_program.h your_program.c
```

The above causes the C compiler to invoke only the pre-processor `-E` and to treat the input as though it were C code `-x c` even though it is in a `.sql` file. Later examples will assume that you have configured CQL to be used with the C pre-processor as above.


## Chapter 2: Using Data
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
The point of using CQL is to facilitate access to a SQLite database so we'll switch gears to a slightly more complicated setup.  We'll
still keep things fairly simple but let's start to use some database features.  Note: it is not the intent of this tutorial to also be
a primer for the SQLite programming language which is so ably documented on https://sqlite.org/.  Please refer to that site for details
on the meaning of the SQL statements used here if you are new to SQL.

### A Sample Program

Suppose we have the following program:

```sql title="hello.sql"
-- needed to allow vararg calls to C functions
declare procedure printf no check;

create table my_data(t text not null);

create proc hello()
begin
  insert into my_data(t) values("Hello, world\n");
  declare t text not null;
  set t := (select * from my_data);
  call printf('%s', t);
end;
```

That looks like an interesting little baby program and it appears as though it would once again print that most famous of salutations, "Hello, world".

Well, it doesn't.  At least, not yet.  Let's walk through the various things that are going to go wrong as this will teach us everything we need to know about activating CQL from some environment of your choice.

### Providing a Suitable Database

CQL is just a compiler, it doesn't know how the code it creates will be provisioned any more than say clang does.
It creates functions with predictable signatures so that they can be called from C just as easily as the SQLite API
itself, and using the same currency.  Our new version of `hello` now requires a database handle because it performs
database operations.  Also there are now opportunities for the database operations to fail, and so `hello` now provides a
return code.

A new minimal `main` program might look something like this:

```c title="main.c"
#include <stdlib.h>
#include <sqlite3.h>

#include "hello.h"

int main(int argc, char **argv)
{
  sqlite3 *db;
  int rc = sqlite3_open(":memory:", &db);
  if (rc != SQLITE_OK) {
    exit(1); /* not exactly world class error handling but that isn't the point */
  }
  rc = hello(db);
  if (rc != SQLITE_OK) {
    exit(2);
  }

  sqlite3_close(db);
}
```

If we re-run CQL and look in the `hello.h` output file we'll see that the declaration of the `hello` function is now:

```c title="hello.h"
...
extern CQL_WARN_UNUSED cql_code hello(sqlite3 *_Nonnull _db_);
...
```

This indicates that the database is used and a SQLite return code is provided.  We're nearly there.  If you attempt
to build the program as before there will be several link-time errors due to missing functions.  Typically these
are resolved by providing the SQLite library to the command line and also adding the CQL runtime.
The new command line looks something like this:

```bash
$ cc -o hello main.c hello.c cqlrt.c -lsqlite3
$ ./hello
Hello, world
```

The cql runtime can be anywhere you want it to be, and of course the usual C separate compilation methods
can be applied. More on that later.

But actually, that program doesn't quite work yet.  If you run it, you'll get an error result code, not the message
"Hello, world".

Let's talk about the final missing bit.

### Declaring Schema

In CQL a loose piece of Data Definition Language (henceforth DDL) does not actually create or drop anything.
In most CQL programs the normal situation is that "something" has already created the database and put some
data in it.  You need to tell the CQL compiler about the schema so that it knows what the tables are and what to
expect to find in those tables.  This is because typically you're reconnecting to some sort of existing database.
So, in CQL, loose DDL simply *declares* schema, it does not create it.  To create schema you have to put the DDL
into a procedure you can run.  If you do that, then the DDL still serves a declaration, but also the schema will be
created when the procedure is executed.

We need to change our program a tiny bit.

```sql title="hello.sql"
-- needed to allow vararg calls to C functions
declare procedure printf no check;

create proc hello()
begin
  create table my_data(t text not null);
  insert into my_data(t) values("Hello, world\n");
  declare t text not null;
  set t := (select * from my_data);
  call printf('%s', t);
  drop table my_data;
end;
```

If we rebuild the program, it will now behave as expected.

### Explaining The New Hello World

Let's go over every important line of the new program, starting from main.

```c
int rc = sqlite3_open(":memory:", &db);
```

This statement gives us an empty, private, in-memory only database to work with.  This is the simplest case
and it's still very useful.  The `sqlite_open` and `sqlite_open_v2` functions can be used to create a variety of
databases per the SQLite documentation.

We'll need such a database to use our procedure, and we use it in the call here:

```c
rc = hello(db);
```

This provides a valid db handle to our procedure.  Note that the procedure doesn't know what database it is
supposed to operate on, it expects to be handed a suitable database on a silver platter.  In fact any given proc
could be used with various databases at various times.  Just like SQLite, CQL does not enforce any particular
database setup; it does what you tell it to.

When `hello` runs we begin with

```sql
create table my_data(t text not null);
```

This will create the `my_data` table with a single column `t`, of type `text not null`.  That will work because
we know we're going to call this with a fresh/empty database.  More typically you might do `create table if not exists ...` or otherwise have a general attach/create phase or something to that effect.  We'll dispense with that here.

Next we'll run the insert statement:

```sql
insert into my_data(t) values("Hello, world\n");
```

This will add a single row to the table.  Note that we have again used double quotes, meaning that this is a C string literal.  This is highly convenient given the escape sequences.  Normally SQLite text has the newlines directly embedded in it; that practice isn't very compiler friendly, hence the alternative.

Next we declare a local variable to hold our data:

```sql
declare t text not null;
```

Then, we can read back our data:

```sql
set t := (select * from my_data);
```

This form of database reading has very limited usability but it does work for this case and it is illustrative.
The presence of `(select ...)` indicates to the CQL compiler that the parenthesized expression should be given to
SQLite for evaluation according to the SQLite rules.  The expression is statically checked at compile time to
ensure that it has exactly one result column. In this case the `*` is just column `t`, and actually it would have
been clearer to use `t` directly here but then there wouldn't be a reason to talk about `*` and multiple columns.
At run time, the `select` query must return exactly one row or an error code will be returned.  It's not uncommon
to see `(select ... limit 1)` to force the issue.  But that still leaves the possibility of zero rows, which would
be an error.  We'll talk about more flexible ways to read from the database later.


> You can declare a variable and assign it in one step with the `LET` keyword, e.g.
> ```sql
> let t := (select * from my_data);
> ```
>
> The code would normally be written in this way but for discussion purposes, these examples continue to avoid `LET`.


At this point it seems wise to bring up the unusual expression evaluation properties of CQL.
CQL is by necessity a two-headed beast.  On the one side there is a rich expression evaluation language for
working with local variables. [What about the other side?] Those expressions are compiled into C logic that emulates the behavior of SQLite
on the data.  It provides complex expression constructs such as `IN` and `CASE` but it is ultimately evaluated by C
execution.  Alternately, anything that is inside of a piece of SQL is necessarily evaluated by SQLite itself.
To make this clearer let's change the example a little bit before we move on.

```sql
set t := (select "__"||t||' '||1.234 from my_data);
```

This is a somewhat silly example but it illustrates some important things:

* even though SQLite doesn't support double quotes, that's no problem because CQL will convert the expression into single quotes with the correct escape values as a matter of course during compilation
* the `||` concatenation operator is evaluated by SQLite
* you can mix and match both kinds of string literals, they will all be the single quote variety by the time SQLite sees them
* the `||` operator has lots of complex formatting conversions (such as converting real values to strings)
* in fact the conversions are so subtle as to be impossible to emulate in loose C code with any economy, so, like a few other operators, `||` is only supported in the SQLite context

Returning now to our code as written, we see something very familiar:

```sql
call printf('%s', t);
```

Note that we've used the single quote syntax here for no good reason other than illustration. There are no escape
sequences here so either form would do the job. Importantly, the string literal will not create a string object as before
but the text variable `t` is of course a string reference.  Before it can be used in a call to an un-declared function it
must be converted into a temporary C string.  This might require allocation in general, that allocation is automatically
managed.

Also, note that CQL assumes that calls to "no check" functions should be emitted as written.  In this way you can use
`printf` even though CQL knows nothing about it.

Lastly we have:

```sql
drop table my_data;
```

This is not strictly necessary because the database is in memory anyway and the program is about to exit but there
it is for illustration.

Now the Data Manipulation Language (i.e. insert and select here; and henceforth DML) and the DDL might fail for various reasons.  If that happens the proc will `goto` a cleanup handler and return the failed return code instead of running the rest of the code.  Any temporary memory allocations will be freed and any pending
SQLite statements will be finalized.  More on that later when we discuss error handling.

With that we have a much more complicated program that prints "Hello, world"

### Introducing Cursors

In order to read data with reasonable flexibility, we need a more powerful construction.
Let's change our example again and start using some database features.

```sql
declare procedure printf no check;

create proc hello()
begin
  create table my_data(
    pos integer not null primary key,
    txt text not null
  );

  insert into my_data values(2, 'World');
  insert into my_data values(0, 'Hello');
  insert into my_data values(1, 'There');

  declare C cursor for select * from my_data order by pos;

  loop fetch C
  begin
    call printf("%d: %s\n", C.pos, C.txt);
  end;
  close C;

  drop table my_data;
end;
```

Reviewing the essential parts of the above.

```sql
create table my_data(
  pos integer not null primary key,
  txt text not null
);
```

The table now includes a position column to give us some ordering.  That is the primary key.

```sql
insert into my_data values(2, 'World');
```

The insert statements provide both columns, not in the printed order.  The insert form where the columns are not
specified indicates that all the columns will be present, in order; this is more economical to type.  CQL will generate errors at compile time if there are any missing columns or if any of the values are not type compatible with the indicated column.

The most important change is here:

```sql
declare C cursor for select * from my_data order by pos;
```

We've created a non-scalar variable `C`, a cursor over the indicated result set.  The results will be ordered by `pos`.

```sql
loop fetch C
begin
  ...
end;
```

This loop will run until there are no results left (it might not run at all if there are zero rows, that is not an error).
The `FETCH` construct allows you to specify target variables, but if you do not do so, then a synthetic structure is
automatically created to capture the projection of the `select`.  In this case the columns are `pos` and `txt`.
The automatically created storage exactly matches the type of the columns in the select list which could itself be tricky to calculate
if the `select` is complex.  In this case the `select` is quite simple and the columns of the result directly match the schema for `my_data`.
An integer and a string reference.  Both not null.


```sql
call printf("%d: %s\n", C.pos, C.txt);
```

The storage for the cursor is given the same names as the columns of the projection of the select, in this case the columns were not renamed so `pos` and `txt` are the fields in the cursor.
Double quotes were used in the format string to get the newline in there easily.

```sql
close C;
```

The cursor is automatically released at the end of the procedure but in this case we'd like to release it before the
`drop table` happens so there is an explicit `close`. This is frequently elided in favor of the automatic cleanup.
There is an `open` cursor statement as well but it doesn't do anything.  It's there because many systems have that
construct and it does balance the `close`.


If you compile and run this program, you'll get this output:

```bash
$ cc -x c -E hello.sql | cql --cg hello.h hello.c
$ cc -o hello main.c hello.c cqlrt.c -lsqlite3
$ ./hello
0: Hello
1: There
2: World
```

So the data was inserted and then sorted.

### Going Crazy

We've only scratched the surface of what SQLite can do and most DML constructs are supported by CQL.
This includes common table expressions, and even recursive versions of the same. But remember, when it
comes to DML, the CQL compiler only has to validate the types and figure out what the result shape will be --
SQLite always does all the heavy lifting of evaluation. All of this means with remarkably little additional code,
the example below from the SQLite documentation can be turned into a CQL stored proc using the constructs
we have defined above.


```sql
-- needed to allow vararg calls to C functions
declare procedure printf no check;

create proc mandelbrot()
begin
  -- this is basically one giant select statement
  declare C cursor for
    with recursive
      -- x from -2.0 to +1.2
      xaxis(x) as (select -2.0 union all select x + 0.05 from xaxis where x < 1.2),

      -- y from -1.0 to +1.0
      yaxis(y) as (select -1.0 union all select y + 0.1 from yaxis where y < 1.0),

      m(iter, cx, cy, x, y) as (
        -- initial seed iteration count 0, at each of the points in the above grid
        select 0 iter, x cx, y cy, 0.0 x, 0.0 y from xaxis, yaxis
        union all
        -- the next point is always iter +1, same (x,y) and the next iteration of z^2 + c
        select iter+1 iter, cx, cy, x*x-y*y + cx x, 2.0*x*y + cy y from m
        -- stop condition, the point has escaped OR iteration count > 28
        where (m.x*m.x + m.y*m.y) < 4.0 and m.iter < 28
      ),
      m2(iter, cx, cy) as (
       -- find the last iteration for any given point to get that count
       select max(iter), cx, cy from m group by cx, cy
      ),
      a(t) as (
        -- convert the iteration count to a printable character, grouping by line
        select group_concat(substr(" .+*#", 1 + min(iter/7,4), 1), '')
        from m2 group by cy
      )
    -- group all the lines together
    select rtrim(t) line from a;

  -- slurp out the data
  loop fetch C
  begin
    call printf("%s\n", C.line);
  end;
end;
```

This code uses all kinds of SQLite features to produce this text:

```bash
$
                                    ....#
                                   ..#*..
                                 ..+####+.
                            .......+####....   +
                           ..##+*##########+.++++
                          .+.##################+.
              .............+###################+.+
              ..++..#.....*#####################+.
             ...+#######++#######################.
          ....+*################################.
 #############################################...
          ....+*################################.
             ...+#######++#######################.
              ..++..#.....*#####################+.
              .............+###################+.+
                          .+.##################+.
                           ..##+*##########+.++++
                            .......+####....   +
                                 ..+####+.
                                   ..#*..
                                    ....#
                                    +.
```

Which probably doesn't come up very often but it does illustrate several things:

 * `WITH RECURSIVE` actually provides a full lambda calculus so arbitrary computation is possible
 * You can use `WITH RECURSIVE` to create table expressions that are sequences of numbers easily, with no reference to any real data

Note:
 * A working version of this code can be found in the `sources/demo` directory of CG/SQL project.
 * Additional demo code is available in [Appendix 10](https://cgsql.dev/cql-guide/x10).


## Chapter 3: Expressions, Literals, Nullability, Sensitivity
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
Until this point we've only discussed simple kinds of expressions as well as variables and table columns marked with `NOT NULL`. These are indeed the easiest types for CQL to work with as they tend to correspond most directly to the types known to C.  However,
SQL provides for many more types of expressions as well as nullable types and these require handling in any language that purports to be like SQL.

### Expression Examples

The usual arithmetic operators apply in CQL:

Example expressions (these are all true)

```
(1 + 2) * 3 == 9
1 + 2 * 3 == 7
6 / 3 == 2
7 - 5 == 2
6 % 5 == 1
5 / 2.5 == 2
7 & 3 == 2 | 1
1 << 2 == 4
```
However, before going any further it's important to note that CQL is inherently a two-headed beast.  Expressions are either evaluated by transpiling to C (like the predicate of an IF statement, or a variable assignment) or by sending them to SQLIte for evaluation (like expressions inside a `SELECT` statement or the `WHERE` part of a `DELETE`).

CQL evaluation rules are designed to be as similar as possible but some variance is inevitable because evaluation is done in two fundamentally different ways.

### Operator Precedence

The operator precedence rules in CQL are as follows; the top-most rule binds the most loosely and the bottom-most rule binds the most tightly:

```
ASSIGNMENT:     :=
LOGICAL_OR:     OR
LOGICAL_AND:    AND
LOGICAL_NOT:    NOT
EQUALITY:       = == != <>  IS [NOT], [NOT] IN, [NOT] LIKE,
                [NOT] MATCH, [NOT] GLOB, [NOT] BETWEEN
INEQUALITY:     <  <=  >  >=
BINARY:         << >> & |
ADDITION:       + -
MULTIPLICATION: * / %
CONCAT:         ||
COLLATE:        COLLATE
UNARY:          ~  -
```

The above rules are **not** the same as C's operator precedence rules! Instead,
CQL follows SQLite's rules. Parentheses are emitted in the C output as needed to
force that order.

**NOTE:** CQL emits minimal parentheses in all outputs. Different parentheses
are often needed for SQL output as opposed to C output.

### Order of Evaluation

In contrast to C, CQL guarantees a left-to-right order of evaluation for
arguments. This applies both to arguments provided to the operators mentioned in
the previous section as well as arguments provided to procedures.

### Variables, Columns, Basic Types and Nullability

CQL needs type information for both variables in the code and columns in the database.
Like SQL, CQL allows variables to hold a NULL value and just as in SQL the absence
of `NOT NULL` implies that `NULL` is a legal value. Consider these examples:

```sql
-- real code should use better names than this :)
create table all_the_nullables(
  i1 integer,
  b1 bool,
  l1 long,
  r1 real,
  t1 text,
  bl1 blob
);

declare i2 integer;
declare b2 bool;
declare l2 long;
declare r2 real;
declare t2 text;
declare bl2 blob;
```

ALL of `i1`, `i2`, `b1`, `b2`, `l1`, `l2`, `r1`, `r2`, `t1`, `t2`, and `bl1`, `bl2` are nullable.
In some sense variables and columns declared nullable (by virtue of the missing `NOT NULL`) are the
root sources of nullability in the SQL language.  That and the `NULL` literal.  Though there are other
sources as we will see.

`NOT NULL` could be added to any of these, e.g.

```sql
-- real code should use better names than this :)
declare i_nn integer not null;
```

In the context of computing the types of expressions, CQL is statically typed and so it must make a decision about the type of any expression based on the type information at hand at compile time.  As a result it handles the static type of an expression conservatively.  If the result might be null then the expression is of a nullable type and the compiled code will include an affordance for the possibility of a null value at runtime.

The generated code for nullable types is considerably less efficient and so it should be avoided if that is reasonably possible.

#### LET Statement

You can declare and initialize a variable in one step using the `LET` form, e.g.

```sql
LET x := 1;
```

The named variable is declared to be the exact type of the expression on the right.  More on expressions in the coming sections.  The right side is often a constant
in these cases but does not need to be.

```sql
LET i := 1;  -- integer not null
LET l := 1L;  -- long not null
LET t := "x";  -- text not null
LET b := x IS y; -- bool not null
LET b := x = y;  -- bool (maybe not null depending on x/y)
```

The pseudo function "nullable" removes `not null` from the type of its argument but otherwise does no computation.
This can be useful to initialize nullable types.

```sql
LET n_i := nullable(1);  -- nullable integer variable initialized to 1
LET n_l := nullable(1L);  -- nullable long variable initialized to 1
```

The pseudo function "sensitive" adds `@sensitive` to the type of its argument but otherwise does no computation.
This also can be useful to initialize nullable types.

```sql
LET s_i := sensitive(1);  -- sensitive nullable integer variable initialized to 1
LET s_l := sensitive(1L);  -- sensitive nullable long variable initialized to 1
```

#### The `@RC` special variable

CQL also has the special built-in variable `@RC` which refers to the most recent error code returned by a SQLite operation, e.g. 0 == `SQLITE_OK`, 1 == `SQLITE_ERROR`.   `@RC` is of type `integer not null`.  Specifically:

* each catch block captures the error code when it is entered into its own local variable
* this variable is created lazily, so it only exists if it is used
  * the variable is called `_rc_thrown_n` where n is the catch block number in the procedure
* any reference to `@RC` refers to the above error variable of the innermost catch block the `@RC` reference is in
* if the `@RC` reference happens outside of any catch block its value is `SQLITE_OK` (i.e. zero).


### Types of Literals

There are a number of literal objects that may be expressed in CQL.  These are as follows:

#### String Literals

* A double quoted string is a C style string literal
  * the usual simple C escape sequences are supported
  * the \xNN form for embedded hex characters is supported, however
  * the \0NNN octal form is not supported, and
  * embedded nulls in string literals (\0 or \0x00) are not supported (you must use blobs in such cases)
* A single quoted string is a SQL style string literal
  * No escape sequences are supported other than `''` to indicate a single quote character (this is just like normal SQLite)
* A sequence of single or double quoted strings separated by whitespace such as "xx" 'yy' "zz" which are concatenated to make one literal
* The sequence @FILE("some_string") is a special string literal
  * the value of this literal is the path of the current compiland starting at the letters in `some_string`, or
  * the entire path of the current compiland if `some_string` does not occur in the path
  * the purpose of the `@FILE` construct is to provide a partial path to a file for diagnostics that is consistent even if the file is built in various different root paths on different build machines

#### Blob Literals
* SQLite Blob literals are supported in SQL contexts (i.e. where they will be processed by SQLite), CQL produces an error if you attempt to use a blob literal in a loose expression

#### Numeric Literals

* All numeric literals are considered to be positive; negative numbers are actually a positive literal combined with unary minus (the negation operator)
* Base 10 and hexadecimal literals are supported
* Literals with a decimal point are of type `REAL` and stored as the C type `double`
* Literals that can fit in a signed integer without loss, and do not end in the letter `L` are integer literals
* Larger literals, or those ending with the letter `L` are long integer literals.
* Literals that begin with 0x are interpreted as hex

Examples:

```sql
  1.3            -- real
  2L             -- long
  123456789123   -- long
  123            -- integer
  0x10           -- hex integer
  0x10L          -- hex long integer
```

#### The NULL literal
The use of `NULL` always gives a nullable result however this literal is special in that it has no storage class. `NULL` is neither numeric nor string itself but rather mutates into
whatever it is first combined with.   For instance `NULL + 1` results in a nullable integer.  Because `NULL` has no primitive type in some cases where type knowledge
is required you might have to use the CAST() function to cast the NULL to a specific type such as `CAST(NULL as TEXT)`.   This construct guarantees type consistence in cases like `SELECT` from different sources combined with `UNION ALL`

Note:  constructs like `CAST(NULL as TEXT)` are always rewritten to just `NULL` before going to SQLite as the cast is uninteresting except for the type information which SQLite doesn't need/use anyway.

#### Other Considerations

There are no boolean literals other than the integers `0` and `1`.

The C pre-processor is often combined with CQL in which case the `_FILE_` and `_LINE_` directives may be used to create literals; they will be preprocessed into normal literals.

The use of `_FILE_` can give surprising results in the presence of build systems, hence the existence of `@FILE(...)`.


### Const and Enumerations

It's possible to use named constants in CQL with nothing more than the C pre-processor features that have already appeared,
however use of #define in such a way is not entirely satisfactory.  For one thing, CQL will not know these constants
exist in any way as they will be replaced before it ever sees them.  This means CQL can't provide their values for you
in the JSON output for instance.

To help with this problem, CQL includes constants, note, this is not the same as enumerated types as we'll
see later.  You can now write something like this:

```sql
declare enum business_type integer (
  restaurant,
  laundromat,
  corner_store = 11+3  /* math added for demo purposes only */
);
```

After this enum is declared, this:

```sql
select business_type.corner_store;
```
is the same as this:

```sql
select 14;
```

And that is exactly what SQLite will see, the literal `14`.

You can also use the enum to define column types:
```sql
CREATE TABLE businesses (
name  TEXT,
type  business_type
);
```

CQL will then enforce that you use the correct enum to access those columns. For example, this is valid:
```sql
SELECT * FROM businesses WHERE type = business_type.laundromat;
```

While this does not type check:
```sql
SELECT * FROM businesses WHERE type = business_corp_state.delaware;
```

Enumerations follow these rules:

* the enumeration can be any numeric type (bool, integer, long integer, real)
* the values of the enumeration start at 1 (i.e. if there is no `= expression` the first item will be `1`, not `0`)
* if you don't specify a value, the next value is the previous value plus one
* if you do specify a value it can be any constant expression and it will be cast to the type of the enumeration (even if that is lossy)
* the enumeration can refer to previous values in itself with no qualification `(big = 100.0, medium = big/2, small = medium/2)`
* the enumeration can refer to previously defined enumerations as usual `(code = business_type.restaurant)`
* once the enumeration is defined you refer to its members in a fully qualified fashion `enum_name.member_name` elsewhere

With these forms you get some additional useful output:
* the JSON includes the enumerations and their values in their own section
* you can use the `@emit_enums` directive to put declarations like this into the `.h` file that corresponds to the current compiland

```c
enum business_type {
  business_type__restaurant = 1,
  business_type__laundromat = 2,
  business_type__corner_store = 14
};
```

Note that C does not allow for floating point enumerations, so in case of floating point values such as:

```sql
declare enum floating real (
  one = 1.0,
  two = 2.0,
  e = 2.71828,
  pi = 3.14159
);
```

you get:

```c
// enum floating (floating point values)
#define floating__one 1.000000e+00
#define floating__two 2.000000e+00
#define floating__e 2.718280e+00
#define floating__pi 3.141590e+00
```

In order to get useful expressions in enumeration values, constant folding and general evaluation was added to the compiler;
these expressions work on any numeric type and the literal null.  The supported operations include:

`+`, `-`, `*`, `/`, `%`, `|`, `&`, `<<`, `>>`, `~`, `and`, `or`, `not`, `==`, `<=`, `>=`, `!=`, `<`, `>`, the `cast` operator
and the `case` forms (including the `iif` function).  These are enough to make a lot of very interesting expressions, all of
which are evaluated at compile time.

Constant folding was added to allow for rich `enum` expressions, but there is also the `const()` primitive in the
language which can appear anywhere a literal could appear.  This allows you do things like:

```sql
create table something(
  x integer default const((1<<16)|0xf) /* again the math is just for illustration */
);
```

The `const` form is also very useful in macros:

```c
#define SOMETHING const(12+3)
```
This form ensures that the constant will be evaluated at compile time. The `const` pseudo-function can also nest
so you can build these kinds of macros from other macros or you can build enum values this way.
Anywhere you might need literals, you can use `const`.

### Named Types

A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key
for an entity might need to be `LONG` or even always `LONG NOT NULL` or `LONG NOT NULL @SENSITIVE` and the only
way to do this in the past was maybe with some `#define` thing.  Otherwise you have to diligently get the type right
in all the places, and should it ever change, again you have to visit all the places.   To help with this situation,
and to make the code a little more self-describing we added named types to the language.  This is a lot like `typedef` in
the C language.  They do not create different incompatible types but they do let you name things well.

You can now write these sorts of forms:

```sql
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

declare function func_return_foo_id() foo_id;

declare var foo_id;
```

Additionally any enumerated type can be used as a type name.  e.g.

```sql
declare enum thing integer (
  thing1,
  thing2
);

declare thing_type type thing;
```

Enumerations always get "not null" in addition to their base type.  Enumerations also have a unique "kind" associated,
specifically the above enum has type `integer<thing> not null`.  The rules for type kinds are described below.

### Type Kinds

Any CQL type can be tagged with a "kind" for instance `real` can become `real<meters>`, `integer` can become `integer<job_id>`.  The idea here is that the additional tag, the "kind" can help prevent type mistakes
in arguments, in columns and in procedure calls.  For instance:

```sql
create table things(
  size real<meters>,
  duration real<seconds>
);

create proc do_something(size_ real<meters>, duration_ real<seconds>)
begin
  insert into things(size, duration) values(size_, duration_);
end;
```

In this situation you couldn't accidentally switch the columns in `do_something` even though both
are `real`, and indeed SQLite will only see the type `real` for both.  If you have your own variables
typed `real<size>` and `real<duration>` you can't accidentally do:

```sql
  call do_something(duration, size);
```

even though both are real.  The type kind won't match.

Importantly, an expression with no type kind is compatible with any type kind (or none).  Hence all of
the below are legal.

```sql
declare generic real;
set generic := size;        -- no kind may accept <meters>
set generic := duration;    -- no kind may accept <seconds>
set duration := generic;    -- no kind may be stored in <seconds>
```

Only mixing types where both have a kind, and the kind is different generates errors.  This choice allows you to
write procedures that (for instance) log any `integer` or any `real`, or that return an `integer` out of a collection.

These rules are applied to comparisons, assignments, column updates, anywhere and everywhere types are checked for compatibility.

To get the most value out of these constructs, the authors recommend that type kinds be used universally except
when the extra compatibility described above is needed (like low level helper functions.)

Importantly, type kind can be applied to object types as well, allowing `object<dict>` to be distinct from `object<list>`.

At run time the kind information is lost. But it does find it's way into the JSON output so external tools
also get to see the kinds.

### Nullability

#### Nullability Rules

Nullability is tracked via CQL's type system. To understand whether or not an
expression will be assigned a nullable type, you can follow these rules; they
will hopefully be intuitive if you are familiar with SQL:

* The literal `NULL` is, of course, always assigned a nullable type. All other
  literals are nonnull.

* In general, the type of an expression involving an operator (e.g., `+`, `==`,
  `!=`, `~`, `LIKE`, et cetera) is nullable if any of its arguments are
  nullable. For example, `1 + NULL` is assigned the type `INTEGER`, implying
  nullability. `1 + 2`, however, is assigned the type `INTEGER NOT NULL`.

* `IN` and `NOT IN` expressions are assigned a nullable type if and only if
  their left argument is nullable: The nullability of the right side is
  irrelevant. For example, `"foo" IN (a, b)` will always have the type `BOOL NOT
  NULL`, whereas `some_nullable IN (a, b)` will have the type `BOOL`.

  * **NOTE:** In CQL, the `IN` operator behaves like a series of equality tests
    (i.e., `==` tests, not `IS` tests), and `NOT IN` behaves symmetrically.
    SQLite has slightly different nullability rules for `IN` and `NOT IN`. *This
    is the one place where CQL has different evaluation rules from SQLite, by
    design.*

* The result of `IS` and `IS NOT` is always of type `BOOL NOT NULL`, regardless
  of the nullability of either argument.

* For `CASE` expressions, the result is always of a nullable type if no `ELSE`
  clause is given. If an `ELSE` is given, the result is nullable if any of the
  `THEN` or `ELSE` expressions are nullable.

  * **NOTE:** The SQL `CASE` construct is quite powerful: Unlike the C `switch`
    statement, it is actually an expression. In this sense, it is rather more
    like a highly generalized ternary `a ? b : c` operator than a C switch
    statement. There can be arbitrarily many conditions specified, each with
    their own result, and the conditions need not be constants; typically, they
    are not.

* `IFNULL` and `COALESCE` are assigned a `NOT NULL` type if one or more of their
  arguments are of a `NOT NULL` type.

* In most join operations, the nullability of each column participating in the
  join is preserved. However, in a `LEFT OUTER` join, the columns on the right
  side of the join are always considered nullable; in a `RIGHT OUTER` join, the
  columns on the left side of the join are considered nullable.

* As in most other languages, CQL does not perform evaluation of value-level
  expressions during type checking. There is one exception to this rule: An
  expression within a `const` is evaluated at compilation time, and if its
  result is then known to be nonnull, it will be given a `NOT NULL` type. For
  example, `const(NULL or 1)` is given the type `BOOL NOT NULL`, whereas merely
  `NULL or 1` has the type `BOOL`.

#### Nullability Improvements

CQL is able to "improve" the type of some expressions from a nullable type to a
`NOT NULL` type via occurrence typing, also known as flow typing. There are
three kinds of improvements that are possible:

* Positive improvements, i.e., improvements resulting from the knowledge that
  some condition containing one or more `AND-`linked `IS NOT NULL` checks must
  have been _true_:

  * `IF` statements:

    ```sql
    IF a IS NOT NULL AND c.x IS NOT NULL THEN
      -- `a` and `c.x` are not null here
    ELSE IF b IS NOT NULL THEN
      -- `b` is not null here
    END IF;
    ```

  * `CASE` expressions:

    ```sql
    CASE
      WHEN a IS NOT NULL AND c.x IS NOT NULL THEN
        -- `a` and `c.x` are not null here
      WHEN b IS NOT NULL THEN
        -- `b` is not null here
      ELSE
        ...
    END;
    ```

  * `IIF` expressions:

    ```sql
    IIF(a IS NOT NULL AND c.x IS NOT NULL,
      ..., -- `a` and `c.x` are not null here
      ...
    )
    ```

  * `SELECT` expressions:

    ```sql
    SELECT
      -- `t.x` and `t.y` are not null here
    FROM t
    WHERE x IS NOT NULL AND y IS NOT NULL
    ```

* Negative improvements, i.e., improvements resulting from the knowledge that
  some condition containing one or more `OR`-linked `IS NULL` checks must have
  been _false_:

  * `IF` statements:

    ```sql
    IF a IS NULL THEN
      ...
    ELSE IF c.x IS NULL THEN
      -- `a` is not null here
    ELSE
      -- `a` and `c.x` are not null here
    END IF;
    ```

  * `IF` statements, guard pattern:

    ```sql
    IF a IS NULL RETURN;
    -- `a` is not null here

    IF c.x IS NULL THEN
      ...
      THROW;
    END IF;
    -- `a` and `c.x` are not null here
    ```

  * `CASE` expressions:

    ```sql
    CASE
      WHEN a IS NULL THEN
        ...
      WHEN c.x IS NULL THEN
        -- `a` is not null here
      ELSE
        -- `a` and `c.x` are not null here
    END;
    ```

  * `IIF` expressions:

    ```sql
    IIF(a IS NULL OR c.x IS NULL,
      ...,
      ... -- `a` and `c.x` are not null here
    )
    ```

* Assignment improvements, i.e., improvements resulting from the knowledge that
  the right side of a statement (or a portion therein) cannot be `NULL`:

  * `SET` statements:

    ```sql
    SET a := 42;
    -- `a` is not null here
    ```

  **NOTE:** Assignment improvements from `FETCH` statements are not currently
  supported. This may change in a future version of CQL.

There are several ways in which improvements can cease to be in effect:

* The scope of the improved variable or cursor field has ended:

  ```sql
  IF a IS NOT NULL AND c.x IS NOT NULL THEN
    -- `a` and `c.x` are not null here
  END IF;
  -- `a` and `c.x` are nullable here
  ```

* An improved variable was `SET` to a nullable value:

  ```sql
  IF a IS NOT NULL THEN
    -- `a` is not null here
    SET a := some_nullable;
    -- `a` is nullable here
  END IF;
  ```

* An improved variable was used as an `OUT` (or `INOUT`) argument:

  ```sql
  IF a IS NOT NULL THEN
    -- `a` is not null here
    CALL some_procedure_that_requires_an_out_argument(a);
    -- `a` is nullable here
  END IF;
  ```

* An improved variable was used as a target for a `FETCH` statement:

  ```sql
  IF a IS NOT NULL THEN
    -- `a` is not null here
    FETCH c INTO a;
    -- `a` is nullable here
  END IF;
  ```

* An improved cursor field was re-fetched:

  ```sql
  IF c.x IS NOT NULL THEN
    -- `c.x` is not null here
    FETCH c;
    -- `c.x` is nullable here
  END IF;
  ```

* A procedure call was made (which removes improvements from _all globals_
  because the procedure may have mutated any of them; locals are unaffected):

  ```sql
  IF a IS NOT NULL AND some_global IS NOT NULL THEN
    -- `a` and `some_global` are not null here
    CALL some_procedure();
    -- `a` is still not null here
    -- `some_global` is nullable here
  END IF;
  ```

CQL is generally smart enough to understand the control flow of your program and
infer nullability appropriately; here are a handful of examples:

  ```sql
  IF some_condition THEN
    SET a := 42;
  ELSE
    THROW;
  END IF;
  -- `a` is not null here because it must have been set to 42
  -- if we've made it this far
  ```

  ```sql
  IF some_condition THEN
    SET a := 42;
  ELSE
    SET a := 100;
  END IF;
  -- `a` is not null here because it was set to a value of a
  -- `NOT NULL` type in all branches and the branches cover
  -- all of the possible cases
  ```

  ```sql
  IF a IS NOT NULL THEN
    IF some_condition THEN
      SET a := NULL;
    ELSE
      -- `a` is not null here despite the above `SET` because
      -- CQL understands that, if we're here, the previous
      -- branch must not have been taken
    END IF;
  END IF;
  ```

  ```sql
  IF a IS NOT NULL THEN
    WHILE some_condition
    BEGIN
      -- `x` is nullable here despite `a IS NOT NULL` because
      -- `a` was set to `NULL` later in the loop and thus `x`
      -- will be `NULL` when the loop repeats
      LET x := a;
      SET a := NULL;
      ...
    END;
  END IF;
  ```

Here are some additional details to note regarding conditions:

* For positive improvements, the check must be exactly of the form `IS NOT
  NULL`; other checks that imply a variable or cursor field must not be null
  when true have no effect:

  ```sql
  IF a > 42 THEN
    -- `a` is nullable here
  END IF;
  ```

  **NOTE:** This may change in a future version of CQL.

* For multiple positive improvements to be applied from a single condition, they
  must be linked by `AND` expressions along the outer spine of the condition;
  uses of `IS NOT NULL` checks that occur as subexpressions within anything
  other than `AND` have no effect:

  ```sql
  IF
    (a IS NOT NULL AND b IS NOT NULL)
    OR c IS NOT NULL
  THEN
    -- `a`, `b`, and `c` are all nullable here
  END IF;
  ```

* For negative improvements, the check must be exactly of the form `IS NULL`;
  other checks that imply a variable or cursor field must not be null when false
  have no effect:

  ```sql
  DECLARE equal_to_null INT;
  IF a IS equal_to_null THEN
    ...
  ELSE
    -- `a` is nullable here
  END IF;
  ```

* For multiple negative improvements to be applied from a single condition, they
  must be linked by `OR` expressions along the outer spine of the condition;
  uses of `IS NULL` checks that occur as subexpressions within anything other
  than `OR` have no effect:

  ```sql
  IF
    (a IS NULL OR b IS NULL)
    AND c IS NULL
  THEN
    ...
  ELSE
    -- `a`, `b`, and `c` are all nullable here
  END IF;
  ```

#### Forcing Nonnull Types

If possible, it is best to use the techniques described in "Nullability
Improvements" to verify that the value of a nullable type is nonnull before using it
as such.

Sometimes, however, you may know that a value with a nullable type cannot be
null and simply wish to use it as though it were nonnull.  The `ifnull_crash`
and `ifnull_throw` "attesting" functions convert the type of an expression to be
nonnull and ensure that the value is nonnull with a runtime check.  They cannot
be used in SQLite contexts because the functions are not known to SQLite, but
they can be used in loose expressions. For example:

```sql
CREATE PROC square_if_odd(a INT NOT NULL, OUT result INT)
BEGIN
  IF a % 2 = 0 THEN
    SET result := NULL;
  ELSE
    SET result := a * a;
  END IF;
END;

-- `x` has type `INT`, but we know it can't be `NULL`
let x := call square_if_odd(3);

-- `y` has type `INT NOT NULL`
let y := ifnull_crash(x);
```

Above, the `ifnull_crash` attesting function is used to coerce the expression
`x` to be of type `INT NOT NULL`. If our assumptions were somehow wrong,
however—and `x` were, in fact, `NULL`—our program would crash.

As an alternative to crashing, you can use `ifnull_throw`. The following two
pieces of code are equivalent:

```sql
CREATE PROC y_is_not_null(x INT)
BEGIN
  let y := ifnull_throw(x);
END;
```

```sql
CREATE PROC y_is_not_null(x INT)
BEGIN
  DECLARE y INT NOT NULL;
  IF x IS NOT NULL THEN
    SET y := x;
  ELSE
    THROW;
  END IF;
END;
```

### Expression Types

CQL supports a variety of expressions, nearly everything from the SQLite world.  The following are the various supported operators; they are presented in order from the weakest binding strength to the strongest.
Note that the binding order is NOT the same as C, and in some cases it is radically different (e.g. boolean math)

#### UNION and UNION ALL
These appear only in the context of `SELECT` statements.  The arms of a compound select may include `FROM`, `WHERE`, `GROUP BY`, `HAVING`, and `WINDOW`.  If `ORDER BY` or `LIMIT ... OFFSET` are present, these apply to the entire UNION.

example:

```sql
select A.x x from A inner join B using(z)
union all
select C.x x from C
where x = 1;
```
The `WHERE` applies only to the second select in the union.  And each `SELECT` is evaluated before the the `UNION ALL`

```sql
select A.x x from A inner join B using(z)
where x = 3
union all
select C.x x from C
where x = 1
order by x;
```
The `ORDER BY` applies to the result of the union, so any results from the 2nd branch will sort before any results from the first branch (because `x` is constrained in both).

#### Assignment

Assignment only occurs in the `UPDATE` statement or in the `SET` statement.  In both cases the left side
is a simple target and the right side is a general expression.  The expression is evaluated before the assignment.

example:

```sql
SET x := 1 + 3 AND 4;  -- + before AND then :=
```

#### Logical OR

The logical `OR` operator does shortcut evaluation, much like the C `||` operator (not to be confused with SQL's concatenation operator with the same lexeme).

The truth table for logical `OR` is as follows:

| A    | B     | A OR B  |
|:----:|:-----:|:-------:|
| 0    |  0    |  0      |
| 0    |  1    |  1      |
| 0    |  NULL |  NULL   |
| 1    |  0    |  1      |
| 1    |  1    |  1      |
| 1    |  NULL |  1      |
| NULL |  0    |  NULL   |
| NULL |  1    |  1      |
| NULL |  NULL |  NULL   |


#### Logical AND
The logical `AND` operator does shortcut evaluation, much like the C `&&` operator, so if the left side is zero the result is 0 and the right side is not evaluated.

The truth table for logical `AND` is as follows:

| A    | B     | A AND B |
|:----:|:-----:|:-------:|
| 0    |  0    |  0      |
| 0    |  1    |  0      |
| 0    |  NULL |  0      |
| 1    |  0    |  0      |
| 1    |  1    |  1      |
| 1    |  NULL |  NULL   |
| NULL |  0    |  0      |
| NULL |  1    |  NULL   |
| NULL |  NULL |  NULL   |


#### BETWEEN and NOT BETWEEN

These are ternary operators.  The general forms are:

```sql
  expr1 BETWEEN expr2 AND expr3
  expr1 NOT BETWEEN expr2 AND expr3
```
Note that there is an inherent ambiguity in the language because `expr2` or `expr3` could be logical expressions that include `AND`. CQL resolves this ambiguity by insisting that `expr2` and `expr3` be "math expressions" in the grammar.  These expressions may not have ungrouped `AND` or `OR` operators.

Examples::

```sql
-- oh hell no (syntax error)
a between 1 and 2 and 3;

-- all ok
a between (1 and 2) and 3;
a between 1 and (2 and 3);
a between 1 and b between c and d; -- binds left to right
a between 1 + 2 and 12 / 2;
```

#### Logical NOT

The one operand of logical `NOT` must be a numeric.  `NOT 'x'` is illegal.

#### Non-ordering tests `!=`, `<>`, `=`, `==`, `LIKE`, `GLOB`, `MATCH`, `REGEXP`, `IN`, `IS`, `IS NOT`

These operations do some non-ordered comparison of their two operands.

* `IS` and `IS NOT` never return `NULL`,  So for instance `X IS NOT NULL` gives the natural answer.  `x IS y` is true if and only if: 1. both `x` and `y` are `NULL` or 2. if they are equal.
* The other operators return `NULL` if either operand is `NULL` and otherwise perform their usual test to produce a boolean
* `!=` and `<>` are equivalent as are `=` and `==`
* strings and blobs compare equal based on their value, not their identity (i.e. not the string/blob pointer)
* objects compare equal based on their address, not their content (i.e. reference equality)
* `MATCH`, `GLOB`, and `REGEXP` are only valid in SQL contexts, `LIKE` can be used in any context (a helper method to do `LIKE` in C is provided by SQLite, but not the others)
* `MATCH`, `GLOB`, `REGEXP`, `LIKE`, and `IN` may be prefixed with `NOT` which reverses their value

```sql
 NULL IS NULL  -- this is true
(NULL == NULL) IS NULL  -- this is also true because NULL == NULL is not 1, it's NULL.
(NULL != NULL) IS NULL  -- this is also true because NULL != NULL is not 0, it's also NULL.
'xy' NOT LIKE 'z%'` -- this is true
```

#### Ordering comparisons <, >, <=, >=

These operators do the usual order comparison of their two operands.

* If either operand is `NULL` the result is `NULL`
* Objects and Blobs may not be compared with these operands
* Strings are compared based on their value (as with other comparisons) not their address
* Numerics are compared as usual with the usual promotion rules

NOTE: CQL uses `strcmp` for string comparison. In SQL expressions the comparison happens in whatever way SQLite has been configured. Typically general purpose string comparison should be done with helper functions that deal with collation and other considerations.  This is a very complex topic and CQL is largely silent on it.

#### Bitwise operators <<, >>, &, |

These are the bit-manipulation operations.  Their binding strength is VERY different than C so beware.
And notably the `&` operator has the same binding strength as the `|` operator so they bind left to right,
which is utterly unlike most systems.  Many parentheses are likely to be needed to get the usual "or of ands" patterns codified correctly.  Likewise, the shift operators `<<` and `>>` are the same strength as `&` and `|` which is very atypical. Consider:

```sql
x & 1 << 7;    -- probably doesn't mean what you think (this is not ambiguous, it's well defined, but unlike C)
(x & 1) << 7;   -- means the same as the above
x & (1 << 7)   -- probably what you intended
```

Note that these operators only work on integer and long integer data.  If any operand is `NULL` the result is `NULL.

#### Addition and Subtraction +, -
These operators do the typical math.  Note that there are no unsigned numerics so it's always signed math that is happening here.

* operands are promoted to the "biggest" type involved as previously described (bool -> int -> long -> real)
* only numeric operands are legal (no adding strings)
* if any operand is `NULL` the result is `NULL`

#### Multiplication, Division, Modulus *, /, %
These operators do the typical math.  Note that there are no unsigned numerics so it's always signed math that is happening here.

* operands are promoted to the "biggest" type as previously described (bool -> int -> long -> real)
* only numeric operands are legal (no multiplying strings)
* if any operand is `NULL` the result is `NULL`

EXCEPTION: the `%` operator doesn't make sense on real values, so real values produce an error.

#### Unary operators -, ~
Unary negation (`-`) and bitwise invert (`~`) are the strongest binding operators.

* The `~` operator only works on integer types (not text, not real)
* the usual promotion rules otherwise apply
* if the operand is `NULL` the result is `NULL`

### CASE Expressions

The `case` expression has two major forms and provides a great deal of flexibility in an expression.  You can kind of think of it as the C `?:` operator on steroids.

```sql
set x := 'y';
select case x
  when 'y' then 1
  when 'z' then 2
  else 3
end;
```

In this form the `case` expression (`x` here) is evaluated exactly once and then compared against each `when` clause. Every `when` clause must be type compatible with the `case` expression.  The `then` expression that corresponds to the matching `when` is evaluated and becomes the result. If no `when` matches then the `else` expression is used.  If there is no `else` and no matching `when` then the result is `null`.

If that's not general enough, there is an alternate form:

```sql
set y := 'yy';
set z := 'z';
select case
  when y = 'y' then 1
  when z = 'z' then 2
  else 3
end;
```

The second form, where there is no value before the first `when` keyword, each `when` expression is a separate independent boolean expression.  The first one that evaluates to true causes the corresponding `then` to be evaluated and that becomes the result.  As before, if there is no matching `when` clause then the result is the `else` expression if present, or `null` if there is no `else`.

The result types must be compatible and the best type to hold the answer is selected with the usual promotion rules.

#### SELECT expressions

Single values can be extracted from SQLite using an inline `select` expression.  For instance:

```sql
set x_ := (select x from somewhere where id = 1);
```

The select statement in question must extract exactly one column and the type of the expression becomes the type of the column.  This form can appear
anywhere an expression can appear, though it is most commonly used in assignments.  Something like this would also be valid:

```sql
if (select x from somewhere where id = 1) == 3 then
  ...
end if;
```

The select statement can of course be arbitrarily complex.

Note, if the select statement returns no rows this will result in the normal error flow.  In that case, the error code will be SQLITE_DONE, which is treated like an
error because in this context SQLITE_ROW is expected as a result of the select.  This is not a typical error code and can be quite surprising to callers. If you're
seeing this failure mode it usually means the code had no affordance for the case where there were no rows and probably that situation should have been handled.
This is an easy mistake to make, so to avoid it, CQL also supports these more tolerant forms:

```sql
set x_ := (select x from somewhere where id = 1 if nothing -1);
```

And even more generally if the schema allows for null values and those are not desired:

```sql
set x_ := (select x from somewhere where id = 1 if nothing or null -1);
```

Both of these are much safer to use as only genuine errors (e.g. the table was dropped and no longer exists) will result in the error control flow.

Again note that:

```sql
set x_ := (select ifnull(x,-1) from somewhere where id = 1);
```

Would not avoid the SQLITE_DONE error code, because no rows returned is not at all the same as a null value returned.

The `if nothing or null` form above is equivalent to the following, but it is more economical, and probably clearer:

```sql
set x_ := (select ifnull(x,-1) from somewhere where id = 1 if nothing -1);
```

To compute the type of the overall expression, the rules are almost the same as normal binary operators.  In particular:
* if the default expression is present it must be type compatible with the select result
  * the result type is the smallest type that holds both the select value and the default expression (see normal promotion rules above)
* object types are not allowed (SQLite cannot return an object)
* in `(select ...)` the result type is not null if and only if the select result type is not null (see select statement, many cases)
* in `(select ... if nothing)` the result type is not null if and only if both the select result and the default expression types are not null (normal binary rules)
* in `(select ... if nothing or null)` the result type is not null if and only if the default expression type is not null

Finally, the form  `(select ... if nothing throw)` is allowed; this form is exactly the same as normal
`(select ...)` but makes the explicit that the error control flow will happen if there is no row.  Consequently
this form is allowed even if `@enforce_strict select if nothing` is in force.

### Marking Data as Sensitive

CQL supports the notion of 'sensitive' data in a first class way.  You can think of it as very much like nullability;  It largely begins by tagging data columns with `@sensitive`

Rather than go through the whole calculus, it's easier to understand by a series of examples.  So let's start with a table with some sensitive data.

```sql
create table with_sensitive(
 id integer,
 name text @sensitive,
 sens integer @sensitive
);
```

The most obvious thing you might do at this point is create a stored proc that would read data out of that table.  Maybe something like this:

```sql
create proc get_sensitive()
begin
  select id as not_sensitive_1,
        sens + 1 sensitive_1,
        name as sensitive_2,
        'x' as not_sensitive_2,
        -sens as sensitive_3,
        sens between 1 and 3 as sensitive_4
  from with_sensitive;
end;
```

So looking at that procedure we can see that it's reading sensitive data, so the result will have some sensitive columns in it.

 * the "id" is not sensitive (at least not in this example)
 * sens + 1 is sensitive, math on a sensitive field leaves it sensitive
 * name is sensitive, it began that way and is unchanged
 * 'x' is just a string literal, it's not sensitive
 * -sens is sensitive, that's more math
 * and the between expression is also sensitive

Generally sensitivity is "radioactive" - anything it touches becomes sensitive.  This is very important because even a simple looking expression like `sens IS NOT NULL` must lead to a sensitive result or the whole process would be largely useless.  It has to be basically impossible to wash away sensitivity.

These rules apply to normal expressions as well as expressions in the context of SQL.  Accordingly:

Sensitive variables can be declared:

```sql
declare sens integer @sensitive;
```

Simple operations on the variables are sensitive
```sql
-- this is sensitive (and the same would be true for any other math)
sens + 1;
```

The `IN` expression gives you sensitive results if anything about it is sensitive

```sql
-- all of these are sensitive
sens in (1, 2);
1 in (1, sens);
(select id in (select sens from with_sensitive));
```

Similarly sensitive constructs in `CASE` expressions result in a sensitive output

```sql
-- not sensitive
case 0 when 1 then 2 else 3 end;

-- all of these are sensitive
case sens when 1 then 2 else 3 end;
case 0 when sens then 2 else 3 end;
case 0 when 1 then sens else 3 end;
case 0 when 1 then 2 else sens end;
```

Cast operations preserve sensitivity
```sql
-- sensitive result
select cast(sens as INT);
```

Aggregate functions likewise preserve sensitivity

```sql
-- all of these are sensitive
select AVG(T1.sens) from with_sensitive T1;
select MIN(T1.sens) from with_sensitive T1;
select MAX(T1.sens) from with_sensitive T1;
select SUM(T1.sens) from with_sensitive T1;
select COUNT(T1.sens) from with_sensitive T1;
```

There are many operators that get similar treatment such as `COALESCE`, `IFNULL`, `IS` and `IS NOT`.

Things get more interesting when we come to the `EXISTS` operator:

```sql
-- sensitive if and only if any selected column is sensitive
exists(select * from with_sensitive)

-- sensitive because "info" is sensitive
exists(select info from with_sensitive)

-- not sensitive because "id" is not sensitive
exists(select id from with_sensitive)
```

If this is making you nervous, it probably should, we need a little more protection because of the way EXISTS is typically used.  The predicates matter, consider the following:

```sql
-- id is now sensitive because the predicate of the where clause was sensitive
select id from with_sensitive where sens = 1;

-- this expression is now sensitive because id is sensitive in this context
exists(select id from with_sensitive where sens = 1)
```

In general: if the predicate of a `WHERE` or `HAVING` clause is sensitive then all columns in the result become sensitive.

Similarly when performing joins, if the column specified in the `USING` clause is sensitive or the predicate of the `ON` clause is sensitive then the result of the join is considered to be all sensitive columns (even if the columns were not sensitive in the schema).

Likewise a sensitive expression in `LIMIT` or `OFFSET` will result in 100% sensitive columns as these can be used in a `WHERE`-ish way.  There is no reasonable defense against using `LIMIT` and testing for the presence or absence of a row as a way to wash away sensitivity so that is a weakness, but the rules that are present are likely to be very helpful.

```sql
-- join with ON
select T1.id from with_sensitive T1 inner join with_sensitive T2 on T1.sens = T2.sens

-- join with USING
select T1.id from with_sensitive T1 inner join with_sensitive T2 using(sens);
```

All of these expression and join propagations are designed to make it impossible to simply wash-away sensitivity with a little bit of math.

Now we come to enforcement, which boils down to what assignments or "assignment-like" operations we allow.

If we have these:

```sql
declare sens integer @sensitive;
declare not_sens integer;
```

We can use those as stand-ins for lots of expressions, but the essential calculus goes like this:

```sql
-- assigning a sensitive to a sensitive is ok
set sens := sens + 1;

-- assigning not sensitive data to a sensitive is ok
-- this is needed so you can (e.g.) initialize to zero
set sens := not_sens;

-- not ok
set not_sens := sens;
```

Now these "assignments" can happen in a variety of ways:

 * you can set an out parameter of your procedure
 * when calling a function or procedure, we require:
   * any IN parameters of the target be "assignable" from the value of the argument expression
   * any OUT parameters of the target be "assignable" from the procedures type to the argument variable
   * any IN/OUT parameters require both the above

Now it's possible to write a procedure that accepts sensitive things and returns non-sensitive things.  This is fundamentally necessary because the proc must be able return (e.g.) a success code, or encrypted data, that is not sensitive.  However, if you write the procedure in CQL it, too, will have to follow the assignment rules and so cheating will be quite hard.  The idea here is to make it easy to handle sensitive data well and make typical mistakes trigger errors.

With these rules  it's possible to compute the the type of procedure result sets and also to enforce IN/OUT parameters.  Since the signature of procedures is conveniently generated with --generate_exports good practices are fairly easy to follow and sensitivity checks flow well into your programs.

This is a brief summary of CQL semantics for reference types -- those types that are ref counted by the runtime.

The three reference types are:

* TEXT
* OBJECT
* BLOB

Each of these has their own macro for `retain` and `release` though all three actually turn into the exact same code in all the current CQL runtime implementations.  In all cases the object is expected to be promptly freed when the reference count falls to zero.

### Reference Semantics

#### Stored Procedure Arguments

* `in` and `inout` arguments are not retained on entry to a stored proc
* `out` arguments are assumed to contain garbage and are nulled without retaining on entry
* if your `out` argument doesn't have garbage in it, then it is up to you do `release` it before you make a call
* When calling a proc with an `out` argument CQL will `release` the argument variable before the call site, obeying its own contract

#### Local Variables

* assigning to a local variable `retains` the object, and then does a `release` on the previous object
* this order is important; all assignments are done in this way in case of aliasing (`release` first might accidentally free too soon)
* CQL calls `release` on all local variable when the method exits

#### Assigning to an `out` parameter or a global variable

* `out`, `inout` parameters, and global variables work just like local variables except that CQL does not call `release` at the end of the procedure

### Function Return Values

Stored procedures do not return values, they only have `out` arguments and those are well defined as above.  Functions however are also supported and they can have either `get` or `create` semantics

#### Get Semantics

If you declare a function like so:

```sql
declare function Getter() object;
```

Then CQL assumes that the returned object should follow the normal rules above, retain/release will balance by the end of the procedure for locals and globals or `out` arguments could retain the object

#### Create Semantics

If you declare a function like so:

```sql
declare function Getter() create text;
```

then CQL assumes that the function created a new result which it is now responsible for releasing.  In short, the returned object is assumed to arrive with a retain count of 1 already on it.  When CQL stores this return value it will:

* release the object that was present at the storage location (if any)
* copy the returned pointer without further retaining it this one time

As a result if you store the returned value in a local variable it will be released when the procedure exits (as usual) or if you instead store the result in a global or an out parameter the result will survive to be used later.

### Comparison

CQL tries to adhere to normal SQL comparison rules but with a C twist.

### `OBJECT`

The object type has no value based comparison, so there is no `<`, `>` and so forth.

The following table is useful.  Let's suppose there are exactly two distinct objects 'X' and 'Y'

true expressions: `X = X`   `X <> Y` `Y = Y`   `Y <> X`  `X IN (X, Y)`  `X NOT IN (Y)`

false expressions: `X = Y`  `X <> X` `Y = X`  `Y <> Y`  `X NOT IN (X, Y)`

null expressions: `null = null`  ` X <> null`   `x = null` `null <> null`  `Y <> null`   `y = null`

`null = null` resulting in `null` is particular surprising but consistent with the usual SQL rules.  And again, as in SQL, the `IS` operator returns true for `X IS Y` even if both are `null`.

### `TEXT`

Text has value comparison semantics but normal string comparison is done only with `strcmp` which is of limited value.  Typically you'll want to either delegate the comparison to Sqlite (with `(select x < y)`) or else use a helper function with a suitable comparison mechanism.

For text comparisons including equality:

true:   if and only if both operands are not null and the comparison matches (using strcmp)
false:  if and only if  both operands are not null and the comparison does not match (using strcmp)
null:   if and only if at least one operand is null

EXAMPLE: `'x' < 'y'`  is true because `strcmp("x", "y") < 0`

The `IS` and `IS NOT` operators behave similarly to equality and inequality, but never return `null`.  If `X` is some value that doesn't happen to be `null` then we have the following:

true:  `null is null` `X is X` `X is not null` `null is not X`
false: `null is not null` `X is not X` `X is null` `null  is X`

The `IN` and `NOT IN` operators also work for text using the same value comparisons as above.  Additionally there are special text comparison
operators such as `LIKE`, `MATCH` and `GLOB`.  These comparisons are defined by SQLite.

### `BLOB`

Blobs are compared by value (equivalent to `memcmp`) but have no well-defined ordering. The `memcmp` order is deemed not helpful as blobs
usually have internal structure hence the valid comparisons are only equality and inequality.
You can use user defined functions to do better comparisons of your particular blobs if needed.

The net comparison behavior is otherwise just like strings.

### Sample Code

#### Out Argument Semantics

```sql
DECLARE FUNCTION foo() OBJECT;

CREATE PROC foo_user (OUT baz OBJECT)
BEGIN
  SET baz := foo();
END;
```

```c
void foo_user(cql_object_ref _Nullable *_Nonnull baz) {
  *(void **)baz = NULL; // set out arg to non-garbage
  cql_set_object_ref(baz, foo());
}
```

#### Function with Create Semantics

```sql
DECLARE FUNCTION foo() CREATE OBJECT;

CREATE PROCEDURE foo_user (INOUT baz OBJECT)
BEGIN
  DECLARE x OBJECT;
  SET x := foo();
  SET baz := foo();
END;
```

```c
void foo_user(cql_object_ref _Nullable *_Nonnull baz) {
  cql_object_ref x = NULL;

  cql_object_release(x);
  x = foo();
  cql_object_release(*baz);
  *baz = foo();

cql_cleanup:
  cql_object_release(x);
}
```

#### Function with Get Semantics

```sql
DECLARE FUNCTION foo() OBJECT;

CREATE PROCEDURE foo_user (INOUT baz OBJECT)
BEGIN
  DECLARE x OBJECT;
  SET x := foo();
  SET baz := foo();
END;
```

```c
void foo_user(cql_object_ref _Nullable *_Nonnull baz) {
  cql_object_ref x = NULL;

  cql_set_object_ref(&x, foo());
  cql_set_object_ref(baz, foo());

cql_cleanup:
  cql_object_release(x);
}
```


## Chapter 4: Procedures, Functions, and Control Flow
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
All kinds of control flow happens in the context of some procedure. Though we've already introduced examples of procedures let's
now go over some of the additional aspects we have not yet illustrated.

### Out Parameters

Consider this procedure:

```sql
create procedure echo_integer(in arg1 integer not null, out arg2 integer not null)
begin
  set arg2 := arg1;
end;
```

`arg1` has been declared `in`. This is the default: `in arg1 integer not null`
and `arg1 integer not null` mean the exact same thing.

`arg2`, however, has been declared `out`. When a parameter is declared using
`out`, arguments for it are passed by reference. This is similar to by-reference
arguments in other languages; indeed, they compile into a simple pointer
reference in the generated C code.

Given that `arg2` is passed by reference, the statement `set arg2 := arg1;`
actually updates a variable in the caller. For example:

```sql
declare x int not null;
call echo_integer(42, x);
-- `x` is now 42
```

It is important to note that values cannot be passed *into* a procedure via an
`out` parameter. In fact, `out` parameters are immediately assigned a new value
as soon as the procedure is called:

- All nullable `out` parameters are set to `null`.

- Nonnull `out` parameters of a non-reference type (e.g., `integer`, `long`,
  `bool`, et cetera) are set to their default values (`0`, `0.0`, `false`, et
  cetera).

- Nonnull `out` parameters of a reference type (e.g., `blob`, `object`, and
  `text`) are set to `null` as there are no default values for reference types.
  They must, therefore, be assigned a value within the procedure so that they
  will not be `null` when the procedure returns. CQL enforces this.

In addition to `in` and `out` parameters, there are also `inout` parameters.
`inout` parameters are, as one might expect, a combination of `in` and `out`
parameters: The caller passes in a value as with `in` parameters, but the value
is passed by reference as with `out` parameters.

`inout` parameters allow for code such as the following:

```sql
create procedure times_two(inout arg integer not null)
begin
  -- note that a variable in the caller is both
  -- read from and written to
  set arg := arg + arg;
end;

let x := 2;
call times_two(x);
-- `x` is now 4
```

### Procedure Calls

The usual `call` syntax is used to invoke a procedure.  It returns no value but it can have any number of `out` arguments.

```
  declare scratch integer not null;
  call echo_integer(12, scratch);
  scratch == 12; -- true
```

Let's go over the most essential bits of control flow.

### The IF statement

The CQL `IF` statement has no syntatic ambiguities at the expense of being somewhat more verbose than many other languages.
In CQL the `ELSE IF` portion is baked into the `IF` statement, so what you see below is logically a single statement.

```sql
create proc checker(foo integer, out result integer not null)
begin
  if foo = 1 then
   set result := 1;
  else if foo = 2 then
   set result := 3;
  else
   set result := 5;
  end if;
end;
```

### The WHILE statement

What follows is a simple procedure that counts down its input argument.

```sql
declare procedure printf no check;

create proc looper(x integer not null)
begin
  while x > 0
  begin
   call printf('%d\n', x);
   set x := x - 1;
  end;
end;
```

The `WHILE` loop has additional keywords that can be used within it to better control the loop.  A more general
loop might look like this:

```sql
declare procedure printf no check;

create proc looper(x integer not null)
begin
  while 1
  begin
   set x := x - 1;
   if x < 0 then
     leave;
   else if x % 100 = 0 then
     continue;
   else if x % 10 = 0 then
     call printf('%d\n', x);
   end if;
  end;
end;
```

Let's go over this peculiar loop:

```sql
  while 1
  begin
    ...
  end;
```

This is an immediate sign that there will be an unusual exit condition.  The loop will never end without one because `1` will never be false.

```sql
   if x < 0 then
     leave;
```
Now here we've encoded our exit condition a bit strangely: we might have done the equivalent job with a normal condition in the predicate
part of the `while` statement but for illustration anyway, when x becomes negative `leave` will cause us to exit the loop.  This is like
`break` in C.

```sql
   else if x % 100 = 0 then
     continue;
```

This bit says that on every 100th iteration we go back to the start of the loop.  So the next bit will not run, which is the printing.

```sql
   else if x % 10 = 0 then
     call printf('%d\n', x);
   end if;
```

Finishing up the control flow, on every 10th iteration we print the value of the loop variable.

### The SWITCH Statement

The  CQL `SWITCH` is designed to map to the C `switch` statement for better codegen and also to give us the opportunity to do better error checking.
`SWITCH` is a *statement* like `IF` not an *expression* like `CASE..WHEN..END` so it combines with other statements. The general form looks like this:

```SQL
SWITCH switch-expression [optional ALL VALUES]
WHEN expr1, expr2, ... THEN
  [statement_list]
WHEN expr3, ... THEN
  [statement_list]
WHEN expr4 THEN
  NOTHING
ELSE
  [statement_list]
END;
```
* the switch-expression must be a not-null integral type (`integer not null` or `long integer not null`)
* the `WHEN` expressions [expr1, expr2, etc.] are made from constant integer expressions (e.g. `5`, `1+7`, `1<<2`, or `my_enum.thing`)
* the `WHEN` expressions must be compatible with the switch expression (long constants cannot be used if the switch expression is an integer)
* the values in the `WHEN` clauses must be unique (after evaluation)
* within one of the interior statement lists the `LEAVE` keyword exits the `SWITCH` prematurely, just like `break` in C
   * a `LEAVE` is not required before the next `WHEN`
   * there are no fall-through semantics as you can find in `C`, if fall-through ever comes to `SWITCH` it will be explicit
* if the keyword `NOTHING` is used after `THEN` it means there is no code for that case, which is useful with `ALL VALUES` (see below)
* the `ELSE` clause is optional and works just like `default` in `C`, covering any cases not otherwise explicitly listed
* if you add `ALL VALUES` then:
   * the expression must be an from an enum type
   * the `WHEN` values must cover every value of the enum
      * enum members that start with a leading `_` are by convention considered pseudo values and do not need to be covered
   * there can be no extra `WHEN` values not in the enum
   * there can be no `ELSE` clause (it would defeat the point of listing `ALL VALUES` which is to get an error if new values come along)

Some more complete examples:

```sql
let x := get_something();
switch x
  when 1,1+1 then -- constant expressions ok
    set y := 'small';
    -- other stuff
  when 3,4,5 then
    set y := 'medium';
    -- other stuff
  when 6,7,8 then
    set y := 'large';
    -- other stuff
  else
    set y := 'zomg enormous';
end;

declare enum item integer (
  pen = 0, pencil, brush,
  paper, canvas,
  _count
);

let x := get_item(); -- returns one of the above

switch x all values
  when item.pen, item.pencil then
     call write_something();
  when item.brush then nothing
     -- itemize brush but it needs no code
  when item.paper, item.canvas then
    call setup_writing();
end;
```

Using `THEN NOTHING` allows the compiler to avoid emitting a useless `break` in the C code.  Hence that choice is better/clearer than `when brush then leave;`

Note that the presence of `_count` in the enum will not cause an error in the above because it starts with `_`.

The `C` output for this statement will be a direct mapping to a `C` switch statement.

### The TRY, CATCH, and THROW Statements

This example illustrates catching an error from some DML, and recovering rather than letting the error cascade up.
This is the common "upsert" pattern (insert or update)

```sql
declare procedure printf no check;

create procedure upsert_foo(id_ integer, t_ text)
begin
  begin try
    insert into foo(id, t) values(id_, t_)
  end try;
  begin catch
    begin try
      update foo set t = t_ where id = id_;
    end try;
    begin catch
      call printf("Error code %d!\n", @rc);
      throw;
    end catch;
  end catch;
end;
```

Once again, let's go over this section by section:

```sql
  begin try
    insert into foo(id, t) values(id_, t_)
  end try;
```

Normally if the `insert` statement fails, the procedure will exit with a failure result code.  Here, instead,
we prepare to catch that error.

```sql
  begin catch
    begin try
      update foo set t = t_ where id = id_;
    end try;
```

Now, having failed to insert, presumably because a row with the provided `id` already exists, we try to update
that row instead.  However that might also fail, so we  wrap it in another try.  If the update fails, then there is a final catch block:

```sql
    begin catch
      call printf("Error code %d!\n", @rc);
      throw;
    end catch;
```

Here we see a usage of the `@rc` variable to observe the failed error code.  In this case we simply print a diagnostic message and
then use the `throw` keyword to rethrow the previous failure (exactly what is stored in `@rc`).  In general, `throw` will create a
failure in the current block using the most recent failed result code from SQLite (`@rc`) if it is an error, or else the general
`SQLITE_ERROR` result code if there is no such error.  In this case the failure code for the `update` statement will become the
result code of the current procedure.

This leaves only the closing markers:

```sql
  end catch;
end;
```

If control flow reaches the normal end of the procedure it will return `SQLITE_OK`.

### Procedures as Functions: Motivation and Example


The calling convention for CQL stored procedures often (usually) requires that the procedure returns a result code from SQLite.
This makes it impossible to write a procedure that returns a result like a function, as the result position is already used for
the error code.  You can get around this problem by using `out` arguments as your return codes.  So for instance, this version
of the Fibonacci function is possible.


```sql
-- this works, but it is awkward
create procedure fib (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    declare t integer not null;
    call fib(arg - 1, result);
    call fib(arg - 2, t);
    set result := t + result;
  end if;
end;
```

The above works, but the notation is very awkward.


CQL has a "procedures as functions" feature that tries to make this more pleasant by making it possible to use function call notation
on a procedure whose last argument is an `out` variable.  You simply call the procedure like it was a function and omit the last argument in the call.
A temporary variable is automatically created to hold the result and that temporary becomes the logical return of the function.
For semantic analysis, the result type of the function becomes the type of the `out` argument.

```sql
-- rewritten with function call syntax
create procedure fib (in arg integer not null, out result integer not null)
begin
  if (arg <= 2) then
    set result := 1;
  else
    set result := fib(arg - 1) + fib(arg - 2);
  end if;
end;
```

This form is allowed when:

* all but the last argument of the procedure was specified
* the formal parameter for that last argument was marked with `out` (neither `in` nor `inout` are acceptable)
* the procedure does not return a result set using a `select` statement or `out` statement (more on these later)

If the procedure in question uses SQLite, or calls something that uses SQLite, then it might fail.
If that happens the result code will propagate just like it would have with the usual `call` form.
Any failures can be caught with `try/catch` as usual.
This feature is really only syntatic sugar for the "awkward" form above, but it does allow for slightly better generated C code.


## Chapter 5: Types of Cursors, Shapes, OUT and OUT UNION, and FETCH
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
In the previous chapters we have used cursor variables without fully discussing them.
Most of the uses are fairly self-evident but a more exhaustive discussion is also useful.

First there are three types of cursors, as we will see below.

### Statement Cursors

A statement cursor is based on a SQL `SELECT` statement.  A full example might look like this:

```sql
-- elsewhere
create table xy_table(x integer, y integer);

declare C cursor for select x, y from xy_table;
```

When compiled, this will result in creating a SQLite statement object (type `sqlite_stmt *`)
and storing it in a variable called `C_stmt`.  This statement can then be used later in various ways.

Here's perhaps the simplest way to use the cursor above:

```sql
declare x, y integer;
fetch C into x, y;
```

This will have the effect of reading one row from the results of the query into
the local variables `x` and `y`.

These variables might then be used to create some output such as:

```sql
/* note use of double quotes so that \n is legal */
call printf("x:%d y:%d\n", ifnull(x, 0), ifnull(y,0));
```

More generally, there the cursor may or may not be holding fetched values.
The cursor variable `C` can be used by itself as a boolean indicating the
presence of a row.  So a more complete example might be

```sql
if C then
  call printf("x:%d y:%d\n", ifnull(x, 0), ifnull(y,0));
else
  call printf("nada\n");
end if
```

And even more generally

```sql
loop fetch C into x, y
begin
  call printf("x:%d y:%d\n", ifnull(x, 0), ifnull(y,0));
end;
```

The last example above reads all the rows and prints them.

Now if the table `xy_table` had instead had dozens of columns, those declarations
would be very verbose and error prone, and frankly annoying, especially if
the table definition was changing over time.

To make this a little easier, there are so-called 'automatic' cursors.  These
happen implicitly and include all the necessary storage to exactly match
the rows in their statement.  Using the automatic syntax for the above looks like so:

```sql
declare C cursor for select * from xy_table;
fetch C;
if C then
  call printf("x:%d y:%d\n", ifnull(C.x, 0), ifnull(C.y,0));
end if;
```

or the equivalent loop form:

```sql
declare C cursor for select * from xy_table;
loop fetch C
begin
  call printf("x:%d y:%d\n", ifnull(C.x, 0), ifnull(C.y,0));
end;
```

All the necessary local state is automatically created, hence "automatic" cursor.
This pattern is generally preferred, but the loose variables pattern is in
some sense more general.

In all the cases if the number or type of variables do not match the select statement,
semantic errors are produced.

### Value Cursors

The purpose of value cursors is to make it possible for a stored procedure to
work with structures as a unit rather than only field by field.  SQL doesn't have
the notion of structure types, but structures actually appear pretty directly
in many places.  Generally we call these things "Shapes" and there are a variety
of source for shapes including:

* the columns of a table
* the projection of a `SELECT` statement
* the columns of a cursor
* the result type of a procedure that returns a select
* the arguments of a procedure
* other things derived from the above

Let's first start with how you declare a value cursor.  It is providing one of the
shape sources above.

So:

```sql
declare C cursor like xy_table;
declare C cursor like select 1 a, 'x' b;
declare C cursor like (a integer not null, b text not null);
declare C cursor like my_view;
declare C cursor like my_other_cursor;
declare C cursor like my_previously_declared_stored_proc;
declare C cursor like my_previously_declared_stored_proc arguments;
```

Any of those forms define a valid set of columns -- a shape.  Note that the
`select` example in no way causes the query provided to run. Instead, the select
statement is analyzed and the column names and types are computed.  The cursor
gets the same field names and types.  Nothing happens at run time.

The last two examples assume that there is a stored procedure defined somewhere
earlier in the same translation unit and that the procedure returns a result set
or has arguments, respectively.

In all cases the cursor declaration makes a cursor that could hold the indicated result.
That result can then be loaded with `FETCH` or emitted with `OUT` or `OUT UNION` which
will be discussed below.

Once we have declared a value cursor we can load it with values using `FETCH` in its
value form. Here are some examples:

Fetch from compatible values:

```sql
fetch C from values(1,2);
```

Fetch from a call to a procedure that returns a single row:

```sql
fetch C from call my_previously_declared_stored_proc();
```

Fetch from another cursor:
```sql
fetch C from D;
```

In this last case if D is a statement cursor it must also be "automatic" (i.e. it has
the storage).  This form lets you copy a row and save it for later.  For instance, in
a loop you could copy the current max-value row into a value cursor and use it after
the loop, like so:

```sql
declare C cursor for select * from somewhere;
declare D cursor like C;

loop fetch C
begin
  if (not D or D.something < C.something) then
    fetch D from C;
  end if;
end;
```

After the loop, D either empty because there were no rows (thus `if D` would fail)
or else it has the row with the maximum value of `something`, whatever that is.

Value cursors are always have their own storage, so you could say all value cursors
are "automatic".

And as we saw above, value cursors may or may not be holding a row.

```sql
declare C cursor like xy_table;
if not C then
  call printf("this will always print because C starts empty\n");
end if;
```

When you call a procedure you may or may not get a row as we'll see below.

The third type of cursor is a "result set" cursor but that won't make any sense
until we've discussed result sets a little which requires `OUT` and/or `OUT UNION`
and so we'll go on to those statements next.  As it happens, we are recapitulating
the history of cursor features in the CQL language by exploring the system in this way.

#### Benefits of using named typed to declare a cursor

This form allows any kind of declaration, for instance:

```
declare C cursor like ( id integer not null, val real, flag boolean );
```

This wouldn't really give us much more than the other forms, however typed name lists can include LIKE in them again, as part of the list.  Which means you can do this kind of thing:

```
declare C cursor like (like D, extra1 real, extra2 bool)
```

You could then load that cursor like so:

```
fetch C from values (from D, 2.5, false);
```

and now you have D plus 2 more fields which maybe you want to output.

Importantly this way of doing it means that C always includes D, even if D changes over time.  As long as the `extra1` and `extra2` fields don't conflict names it will always work.


### OUT Statement

Value cursors were initially designed to create a convenient way for
a procedure to return a single row from a complex query
without having a crazy number of `OUT` parameters.  It's easiest
to illustrate this with an example.

Suppose you want to return several variables, the "classic" way to do so
would be a procedure like this:
```sql
create proc get_a_row(
  id_ integer not null,
  out got_row bool not null,
  out w integer not null,
  out x integer,
  out y text not null,
  out z real)
begin
  declare C cursor for
    select w, x, y, z from somewhere where id = id_;
  fetch C into w, x, y, z;
  set got_row := C;
end;
```

This is already verbose, but you can imagine the situation gets very annoying
if `get_a_row` has to produce a couple dozen column values.  And of course you
have to get the types exactly right. And they might evolve over time.  Joy.

On the receiving side you get to do something just as annoying:

```sql
declare w integer not null
declare x integer;
declare y text;
declare z real;
declare got_row bool not null;
call get_a_row(id, got_row, w, x, y, z);
```

Using the `out` statement we get the equivalent functionality with a much
simplified pattern. It looks like this:
```sql
create proc get_a_row(id_ integer not null)
begin
  declare C cursor for
    select w, x, y, z from somewhere where id = id_;
  fetch C;
  out C;
end;
```

To use the new procedure you simply do this:
```sql
declare C cursor like get_a_row;
fetch C from call get_a_row(id);
```

In fact, originally you did the two steps above in one statement and that was the
only way to load a value cursor. Later, the calculus was generalized. The original
form still works:
```sql
declare C cursor fetch from call get_a_row(id);
```

The `OUT` statement lets you return a single row economically and
lets you then test if there actually was a row and if so, read the columns.
It infers all the various column names and types so it is resilient
to schema change and generally a lot less error prone than having a
large number of `out` arguments to your procedure.

Once you have the result in a value cursor you can do the usual
cursor operations to move it around or otherwise work with it.

The use of the `LIKE` keyword to refer to groups of columns spread
to other places in CQL as a very useful construct, but it began here
with the need to describe a cursor shape economically, by reference.

### OUT UNION Statement
The semantics of the `OUT` statement are that it always produces one row
of output (a procedure can produce no row if an `out` never actually rans
but the procedure does use `OUT`).

If an `OUT` statement runs more than once, the most recent row becomes the
result.  So the `OUT` statement really does mirror having one `out` variable
for each output column.  This was its intent and procedures that return at most,
or exactly, one row are very common so it works well enough.

However, in general, one row results do not suffice; you might want to produce a
result set from various sources, possibly with some computation as part of
the row creation process.  To make general results, you need to be able to emit
multiple rows from a computed source.  This is exactly what `OUT UNION` provides.

Here's a (somewhat contrived) example of the kind of thing you can do with this form:

```sql
create proc foo(n integer not null)
begin
  declare C cursor like select 1 value;
  let i := 0;
  while i < n
  begin
     -- emit one row for every integer
     fetch C from values(i);
     out union C;
     set i := i + 1;
  end;
end;
```

In `foo` above, we make an entire result set out of thin air.  It isn't a very
interesting result, but of course any computation would have been possible.

This pattern is very flexible as we see below in `bar` where
we merge two different data streams.

```sql
create table t1(id integer, stuff text, [other things too]);
create table t2(id integer, stuff text, [other things too]);

create proc bar()
begin
  declare C cursor for select * from t1 order by id;
  declare D cursor for select * from t2 order by id;

  fetch C;
  fetch D;

  -- we're going to merge these two queries
  while C or D
  begin
    -- if both have a row pick the smaller id
    if C and D then
       if C.id < D.id then
         out union C;
         fetch C;
       else
         out union D;
         fetch D;
       end if;
    else if C then
      -- only C has a row, emit that
      out union C;
      fetch C;
    else
      -- only D has a row, emit that
      out union D;
      fetch D;
    end if;
  end;
end;
```

Just like `foo`, in `bar`, each time `OUT UNION` runs a new row is accumulated.

Now, if you build a procedure that ends with a `SELECT` statement CQL automatically
creates a fetcher function that does something like an `OUT UNION` loop -- it loops
over the SQLite statement for the `SELECT` and fetches each row, materializing a result.

With `OUT UNION` you take manual control of this process, allowing you to build arbitrary
result sets.  Note that either of `C` or `D` above could have been modified, replaced,
skipped, normalized, etc. with any kind of computation.  Even entirely synthetic rows can
be computed and inserted into the output as we saw in `foo`.

### Result Set Cursors

Now that we have `OUT UNION` it makes sense to talk about the final type of cursor.

`OUT UNION` makes it possible to create arbitrary result sets using a mix of sources
and filtering.  Unfortunately this result type is not a simple row, nor is it a SQLite
statement.  This meant that neither of the existing types of cursors could hold the
result of a procedure that used `OUT UNION`. -- CQL could not itself consume its own
results.

To address this hole, we need an additional cursor type.  The syntax is exactly the same
as the statement cursor cases described above but, instead of holding a SQLite statement,
the cursor holds a result set pointer and the current and maximum row numbers.
Stepping through the cursor simply increments the row number and fetches the next row
out of the rowset instead of from SQLite.

Example:
```sql
-- reading the above
create proc reader()
begin
  declare C cursor for call bar();
  loop fetch C
  begin
    call printf("%d %s\n", C.id, C.stuff);  -- or whatever fields you need
  end;
end;
```

If `bar` had been created with a `SELECT`, `UNION ALL`, and `ORDER BY` to merge the
results, the above would have worked with `C` being a standard statement cursor,
iterating over the union. Since `foo` produces a result set, CQL transparently produces
a suitable cursor implementation behind the scenes, but otherwise the usage is the same.

Note this is a lousy way to simply iterate over rows; you have to materialize the entire
result set so that you can just step over it.  Re-consuming like this is not recommended
at all for production code, but it is ideal for testing result sets that were made with
`OUT UNION` which otherwise would require C/C++ to test.  Testing CQL with CQL is
generally a lot easier.

### Reshaping Data, Cursor `LIKE` forms

There are lots of cases where you have big rows with many columns, and there are various manipulations you might need to do.

What follows is a set of useful syntactic sugar constructs that simplify handling
complex rows.  The idea is that pretty much anywhere you can specify a list of columns
you can instead use the `LIKE x` construct to get the columns as they appear in the
shape `x` -- which is usually a table or a cursor.

It’s a lot easier to illustrate with examples, even though these are, again, a bit contrived.

First we need some table with lots of columns -- usually the column names are much bigger which makes it all the more important to not have to type them over and over, but
in the interest of some brevity, here's a big table:

```sql
create table big (
  id integer primary key,
  id2 integer unique,
  a integer,
  b integer,
  c integer,
  d integer,
  e integer,
  f integer);
```

This example showcases several of the cursor and shape slicing features by emitting
two related rows:

```sql
create proc foo(id_ integer not null)
begin
  -- this is the shape of the result we want -- it's some of the columns of "big"
  -- note this query doesn't run, we just use its shape to create a cursor
  -- with those columns.
  declare result cursor like select id, b, c, d from big;

  -- fetch the main row, specified by id_
  -- main row has all the fields, including id2
  declare main_row cursor for select * from big where id = id_;
  fetch main_row;

  -- now fetch the result columns out of the main row
  -- 'like result' means "the column names found in 'result'"
  fetch result from cursor main_row(like result);

  -- this is our first result row
  out union result;

  -- now we want the related row, but we only need two columns
  -- from the related row, 'b' and 'c'
  declare alt_row cursor for select b, c from big where big.id2 = main_row.id2;
  fetch alt_row;

  -- update some of the fields 'result' from the the new cursor
  update cursor result(like alt_row) from cursor alt_row;

  -- and emit the 2nd row
  out union result;
end;
```

Now let's briefly discuss what is above.  The two essential parts are:

`fetch result from cursor main_row(like result);`

and

`update cursor result(like alt_row) from cursor alt_row;`

In the first case what we're saying is that we want to load the columns
of `result` from `main_row` but we only want to take the columns that are
actually present in `result`.  So this is a narrowing of a wide row into a
smaller row.  In this case, the smaller row, `result`, is what we want to emit.
We needed the other columns to compute `alt_row`.

The second case, what we're saying is that we want to update `result` by
replacing the columns found in `alt_row` with the values in `alt_row`.
So in this case we're writing a smaller cursor into part of a wider cursor.
Note that we used the `update cursor` form here because it preserves all other
columns.  If we used `fetch` we would be rewriting the entire row contents,
using `NULL` if necessary, and that is not desired here.

Here is the rewritten version of the above procedure; this is what ultimately gets
compiled into C.

```sql
CREATE PROC foo (id_ INTEGER NOT NULL)
BEGIN
  DECLARE result CURSOR LIKE SELECT id, b, c, d FROM big;
  DECLARE main_row CURSOR FOR SELECT * FROM big WHERE id = id_;
  FETCH main_row;

  FETCH result(id, b, c, d)
    FROM VALUES(main_row.id, main_row.b, main_row.c, main_row.d);
  OUT UNION result;

  DECLARE alt_row CURSOR FOR SELECT b, c FROM big WHERE big.id2 = main_row.id2;
  FETCH alt_row;

  UPDATE CURSOR result(b, c) FROM VALUES(alt_row.b, alt_row.c);
  OUT UNION result;
END;
```

Of course you could have typed the above directly but if there are 50 odd columns it
gets old fast and is very error prone.  The sugar form is going to be 100% correct
and will require much less typing and maintenance.

Finally, while I've shown both `LIKE` forms separately, they can also be used together.  For instance:

```sql
  update cursor C(like X) from cursor D(like X);
```

The above would mean, "move the columns that are found in `X` from cursor `D` to cursor `C`", presuming `X` has columns common to both.

### Fetch Statement Specifics

Many of the examples used the `FETCH` statement in a sort of demonstrative way that is
hopefully self-evident but the statement has many forms and so it's worth going over
them specifically.  Below we'll use the letters `C` and `D` for the names of cursors.  Usually `C`;

#### Fetch with Statement or Result Set Cursors

A cursor declared in one of these forms:

* `declare C cursor for select * from foo;`
* `declare C cursor for call foo();`  (foo might end with a `select` or use `out union`)

is either a statement cursor or a result set cursor.  In either case the cursor moves
through the results.  You load the next row with:

* `FETCH C`, or
* `FETCH C into x, y, z;`

In the first form `C` is said to be *automatic* in that it automatically declares the storage needed to hold all its columns.  As mentioned above, automatic cursors have
storage for their row.

Having done this fetch you can use C as a scalar variable to see if it holds a row, e.g.

```sql
declare C cursor for select * from foo limit 1;
fetch C;
if C then
  -- bingo we have a row
  call printf("%s\n", C.whatever);
end if
```

You can easily iterate, e.g.

```sql
declare C cursor for select * from foo;
loop fetch C
begin
  -- one time for every row
  call printf("%s\n", C.whatever);
end;
```

Automatic cursors are so much easier to use than explicit storage that explicit storage
is rarely seen.  Storing to `out` parameters is one case where explicit storage actually
is the right choice, as the `out` parameters have to be declared anyway.

#### Fetch with Value Cursors

 A value cursor is declared in one of these ways:

 * `declare C cursor fetch from call foo(args)`
   * `foo` must be a procedure that returns one row with `OUT`
 * `declare C cursor like select 1 id, "x" name;`
 * `declare C cursor like X;`
   * where X is the name of a table, a view, another cursor, or a procedure that returns a structured result

 A value cursor is *always* automatic; it's purpose is to hold a row.  It doesn't iterate over anything but it can be re-loaded in a loop.

 * `fetch C` or `fetch C into ...` is not valid on such a cursor, because it doesn't have a source to step through.

 The canonical way to load such a cursor is:

 * `fetch C from call foo(args);`
   * `foo` must be a procedure that returns one row with `OUT`
 * `fetch C(a,b,c...) from values(x, y, z);`

The first form is in some sense the origin of the value cursor.  Value cursors were added to the language initially to provide a way to capture the single row `OUT` statement results, much like result set cursors were added to capture procedure results from `OUT UNION`.  In the first form, the cursor storage (a C struct) is provided by reference as a hidden out parameter to the procedure and the procedure fills it in.  The procedure may or may not use the `OUT` statement in its control flow, as the cursor might not hold a row.  You can use `if C then ...` as before to test for a row.

The second form is more interesting as it allows the cursor to be loaded from arbitrary expressions subject to some rules:
 * you should think of the cursor as a logical row: it's either fully loaded or it's not, therefore you must specify enough columns in the column list to ensure that all `NOT NULL` columns will get a value
 * if not mentioned in the list, NULL will be loaded where possible
 * if insufficient columns are named, an error is generated
 * if the value types specified are not compatible with the column types mentioned, an error is generated
 * later in this chapter, we'll show that columns can also be filled with dummy data using a seed value

With this form, any possible valid cursor values could be set, but many forms of updates
that are common would be awkward. So there are various forms of syntactic sugar that are
automatically rewritten into the canonical form.  See the examples below:

* `fetch C from values(x, y, z)`
  * if no columns are specified this is the same as naming all the columns, in declared order

* `fetch C from arguments`
  * the arguments to the procedure in which this statement appears are used as the values, in order
  * in this case `C` was also rewritten into `C(a,b,c,..)` etc.

* `fetch C from arguments like C`
  * the arguments to the procedure in which this statement appears are used, by name, as the values, using the names of of the indicated shape
  * the order in which the arguments appeared no longer matters, the names that match the columns of C are used if present
  * the formal parameter name may have a single trailing underscore (this is what `like C` would generate)
  * e.g. if `C` has columns `a` and `b` then there must exist formals named `a` or `a_` and `b` or `b_`, in any position

* `fetch C(a,b) from cursor D(a,b)`
  * the named columns of D are used as the values
  * in this case the statement becomes: `fetch C(a,b) from values(D.a, D.b);`

That most recent form doesn't seem like it saves much, but recall the first rewrite:

* `fetch C from cursor D`
  * both cursors are expanded into all their columns, creating a copy from one to the other
  * `fetch C from D` can be used if the cursors have the exact same column names and types; it also generates slightly better code and is a common case

 It is very normal to want to use only some of the columns of a cursor; these `like` forms do that job.  We saw some of these forms in an earlier example.

 * `fetch C from cursor D(like C)`
   * here `D` is presumed to be "bigger" than `C`, in that it has all of the `C` columns and maybe more.  The `like C` expands into the names of the `C` columns so `C` is loaded from the `C` part of `D`
   * the expansion might be `fetch C(a, b, g) from values (D.a, D.b, D.g)`
   * `D` might have had fields `c, d, e, f` which were not used because they are not in `C`.

 The symmetric operation, loading some of the columns of a wider cursor can be expressed neatly:

 * `fetch C(like D) from cursor D`
   * the `like D` expands into the columns of `D` causing the cursor to be loaded with what's in `D` and `NULL` (if needed)
   * when expanded, this might look like `fetch C(x, y) from values(D.x, D.y)`

`LIKE` can be used in both places, for instance suppose `E` is a shape that has a subset of the rows of both `C` and `D`.  You can write a form like this:

* `fetch C(like E) from cursor D(like E)`
  * this means take the column names found in `E` and copy them from D to C.
  * the usual type checking is done

 As is mentioned above, the `fetch` form means "load an entire row into the cursor". This
 is important because "half loaded" cursors would be semantically problematic.  However
 there are many cases where you might like to amend the values of an already loaded
 cursor.  You can do this with the `update` form.

 * `update cursor C(a,b,..) from values(1,2,..);`
   * the update form is a no-op if the cursor is not already loaded with values (!!)
   * the columns and values are type checked so a valid row is ensured (or no row)
   * all the re-writes above are legal so `update cursor C(like D) from D` is possible; it is in fact the use-case for which this was designed.

### Calling Procedures with Argument Bundles

It's often desirable to treat bundles of arguments as a unit, or cursors as a unit, especially when calling other procedures.  The shape patterns above are very helpful
for moving data between cursors, and the database.  These can be rounded out with
similar constructs for procedure definitions and procedure calls as follows.

First we'll define some shapes to use in the examples.  Note that we made `U` using `T`.

```sql
create table T(x integer not null, y integer not null,  z integer not null);
create table U(like T, a integer not null, b integer not null);
```

We haven't mentioned this before but the implication of the above is that you can
use the `LIKE` construct inside a table definition to add columns from a shape.

We can also use the `LIKE` construct to create procedure arguments.  To avoid conflicts
with column names, when used this way the procedure arguments all get a trailing
underscore appended to them.  The arguments will be `x_`, `y_`, and `z_` as we can
see if the following:

```sql
create proc p1(like T)
begin
  call printf("%d %d %d\n", x_, y_, z_);
end;
```

Shapes can also be used in a procedure call, as showed below. This next example is
obviously contrived, but of course it generalizes. It is exactly equivalent to the above.

```sql
create proc p2(like T)
begin
  call printf("%d %d %d\n", from arguments);
end;
```

Now we might want to chain these things together.  This next example uses a cursor to
call `p1`.

```sql
create proc q1()
begin
 declare C cursor for select * from T;
 loop fetch C
 begin
   /* this is the same as call p(C.x, C.y, C.z) */
   call p1(from C);
 end;
end;
```

The `like` construct allows you to select some of the arguments, or
some of a cursor to use as arguments.  This next procedure has more arguments
than just `T`. The arguments will be `x_`, `y_`, `z_`, `a_`, `b_`.  But the
call will still have the `T` arguments `x_`, `y_`, and `z_`.

```sql
create proc q2(like U)
begin
  /* just the args that match T: so this is still call p(x_, y_, z_) */
  call p1(from arguments like T);
end;
```

Or similarly, using a cursor.

```sql
create proc q3(like U)
begin
 declare C cursor for select * from U;
 loop fetch C
 begin
  /* just the columns that match T so this is still call p(C.x, C.y, C.z) */
  call p1(from C like T);
 end;
end;
```

Note that the `from` argument forms do not have to be all the arguments.  For instance
you can get columns from two cursors like so:

```sql
  call something(from C, from D)
```

All the varieties can be combined but of course the procedure signature must match.  And
all these forms work in function expressions as well as procedure calls.

e.g.

```sql
  set x := a_function(from C);
```

Since these forms are simply syntatic sugar, they can also appear inside of function calls
that are in SQL statements. The variables mentioned will be expanded and become bound
variables just like any other variable that appears in a SQL statement.

Note the form `x IN (from arguments)` is not supported at this time, though this would be
a relatively easy addition.

### Using Named Argument Bundles

There are many cases where stored procedures require complex arguments using data shapes
that come from the schema, or from other procedures.  As we have seen the `LIKE` construct
for arguments can help with this, but it has some limitations. Let's consider a specific
example to study:

```sql
create table Person (
  id text primary key,
  name text not null,
  address text not null,
  birthday real
);
```

To manage this table we might need something like this:

```sql
create proc insert_person(like Person)
begin
  insert into Person from arguments;
end;
```

As we have seen, the above expands into:

```sql
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

It's clear that the sugared version is a lot easier to reason about than the
fully expanded version, and much less prone to errors as well.

This much is already helpful, but just those forms aren't general enough to handle
the usual mix of situations.  For instance, what if we need a procedure that works
with two people? A hypothetical `insert_two_people` procedure cannot be written with
the forms we have so far.

To generalize this the language adds the notion of named argument bundles. The idea here
is to name the bundles which provides a useful scoping.  Example:

```sql
create proc insert_two_people(p1 like Person, p2 like Person)
begin
  -- using a procedure that takes a Person args
  call insert_person(from p1);
  call insert_person(from p2);
end;
```

or alternatively

```sql
create proc insert_two_people(p1 like Person, p2 like Person)
begin
  -- inserting a Person directly
  insert into Person from p1;
  insert into Person from p2;
end;
```

The above expands into:

```sql
create proc insert_two_people(
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

Or course different named bundles can have different types -- you can create and name
shapes of your choice.  The language allows you to use an argument bundle name in all
the places that a cursor was previously a valid source.  That includes `insert`,
`fetch`, `update cursor`, and procedure calls.  You can refer to the arguments by
their expanded name such as `p1_address` or alternatively `p1.address` -- they mean
the same thing.

Here's another example showing a silly but illustrative thing you could do:

```sql
create proc insert_lotsa_people(P like Person)
begin
  -- make a cursor to hold the arguments
  declare C cursor like P;

  -- convert arguments to a cursor
  fetch C from P;

  -- set up to patch the cursor and use it 20 times
  let i := 0;
  while i < 20
  begin
    update cursor C(id) from values(printf("id_%d", i));
    insert into Person from C;
    set i := i + 1;
  end;
end;
```

The above shows that you can use a bundle as the source of a shape, and you can
use a bundle as a source of data to load a cursor.  After which you can do all the
usual value cursor things.  Of course in this case the value cursor was redundant,
we could just as easily have done something like this:

```sql
  set P_id := printf("id_%d", i);
  insert into Person from P;
  set i := i + 1;
```

Note: the CQL JSON output includes extra information about procedure arguments
if they originated as part of a shape bundle do identify the shape source
for tools that might need that information.

### The COLUMNS/LIKE construct in the SELECT statement

The select list of a `SELECT` statement already has complex syntax and functionality,
but it is a very interesting place to use shapes.  To make it possible to use
shape notations and not confuse them with standard SQL the `COLUMNS` construct was
added to the language.  This allows for a sugared syntax for extracting columns in bulk.

The `COLUMNS` clause is like of a generalization of the `select T.*` with shape slicing and type-checking.  The forms are discussed below:


#### Columns from a join table or tables

This is the simplest form, it's just like `T.*`:

```
-- same as A.*
select columns(A) from ...;

-- same as A.*, B.*
select columns(A, B) from ...;
```

#### Columns from a particular joined table that match a shape

This allows you to choose some of the columns of one table of the FROM clause.

```
-- the columns of A that match the shape Foo
select columns(A like Foo) from ...;

-- get the Foo shape from A and the Bar shape from B
select columns(A like Foo, B like Bar) from ...;
```

#### Columns from any that match a shape, from anywhere in the FROM

Here we do not specify a particular table that contains the columns,
the could come from any of the tables in the FROM clause.

```
--- get the Foo shape from anywhere in the join
select columns(like Foo) from ...;

-- get the Foo and Bar shapes, from anywhere in the join
select columns(like Foo, like Bar) from ...;
```

#### Specific columns

This form allows you to slice out a few columns without defining a shape, you
simply list the exact columns you want.

```
-- T1.x and T2.y plus the Foo shape
select columns(T1.x, T2.y, like Foo) from ...;
```

#### Distinct columns

Its often the case that there are duplicate column names in the `FROM` clause.
For instance, you could join `A` to `B` with both having a column `pk`. The
final result set can only have one column named `pk`, the distinct clause
helps you to get distinct column names.  In this context `distinct` is about
column names, not values.

```
-- removes duplicate column names
-- e.g. there will be one copy of 'pk'
select columns(distinct A, B) from A join B using(pk);

-- if both Foo and Bar have an (e.g.) 'id' field you only get one copy
select columns(distinct like Foo, like Bar) from ...;
```

If a specific column is mentioned it is always included, but later expressions
that are not a specific column will avoid that column name.

```
-- if F or B has an x it won't appear again, just T.x
select columns(distinct T.x, F like Foo, B like Bar) from F, B ..;
```

Of course this is all just sugar, so it all compiles to a column list with table
qualifications -- but the syntax is very powerful.  You can easily narrowin a
wide table, or fusing joins that share common keys.

```
-- just the Foo columns
select columns(like Foo) from Superset_Of_Foo_From_Many_Joins_Even;

-- only one copy of 'pk'
select columns(distinct A,B,C) from
  A join B using (pk) join C using (pk);
```

And of course you can define shapes however you like and then use them
to slice off column chucks of your choice.  There are many ways to build
up shapes from other shapes.  For instance, you can declare procedures
that return the shape you want and never actually create the procedure --
a pattern is very much like a shape "typedef".  E.g.

```
declare proc shape1() (x integer, y real, z text);
declare proc shape2() (like shape1, u bool, v bool);
```

With this combination you can easily define common column shapes and slice them
out of complex queries without having to type the columns names over and over.

### Missing Data Columns, Nulls and Dummy Data

What follows are the rules for columns that are missing in an `INSERT`, or `FETCH` statement. As with many of the other things discussed here, the forms result in
automatic rewriting of the code to include the specified dummy data.  So SQLite
will never see these forms.

Two things to note: First, the dummy data options described below are really only interesting in test code, it's hard to imagine them being useful in production code.
Second, none of what follows applies to the `update cursor` statement because its
purpose is to do partial updates on exactly the specified columns and we're about to
talk about what happens with the columns that were not specified.

When fetching a row all the columns must come from somewhere; if the column is
mentioned or mentioned by rewrite then it must have a value mentioned, or a value
mentioned by rewrite. For columns that are not mentioned, a NULL value is used if
it is legal to do so.  For example, `fetch C(a) from values(1)` might turn into `fetch C(a,b,c,d) from values (1, NULL, NULL, NULL)`

In addition to the automatic NULL you may add the annotation `@dummy_seed([long integer expression])`. If this annotion is present then:
* the expression is evaluated and stored in the hidden variable _seed_
* all integers, and long integers get _seed_ as their value (possibly truncated)
* booleans get 1 if and only if _seed_ is non-zero
* strings get the name of the string column an underscore and the value as text (e.g.   "myText_7" if _seed_ is 7)
* blobs are not currently supported for dummy data (CQL is missing blob conversions which are needed first)

This construct is hugely powerful in a loop to create many complete rows with very little effort, even if the schema change over time.

```sql
declare i integer not null;
declare C like my_table;
set i := 0;
while (i < 20)
begin
   fetch C(id) from values(i+10000) @dummy_seed(i);
   insert into my_table from cursor C;
end;
```

Now in this example we don't need to know anything about `my_table` other than that it has a column named `id`. This example shows several things:
 * we got the shape of the cursor from the table we were inserting into
 * you can do your own computation for some of the columns (those named) and leave the unnamed values to be defaulted
 * the rewrites mentioned above work for the `insert` statement as well as `fetch`
 * in fact `insert into my_table(id) values(i+10000) @dummy_seed(i)` would have worked too with no cursor at all
   * bonus, dummy blob data does work in insert statements because SQLite can do the string conversion easily
   * the dummy value for a blob is a blob that holds the text of the column name and the text of the seed just like a string column

The `@dummy_seed` form can be modified with `@dummy_nullables`, this indicates that rather than using NULL for any nullable value that is missing, CQL should use the seed value.  This overrides the default behavior of using NULL where columns are needed.  Note the NULL filling works a little differently on insert statements.  Since SQLite will provide a NULL if one is legal the column doesn't have to be added to the list with a NULL value during rewriting, it can simply be omitted, making the statement smaller.

Finally for `insert` statement only, SQLite will normally use the default value of a column if it has one, so there is no need to add missing columns with default values to the insert statement.  However if you specify `@dummy_defaults` then columns with a default value will instead be rewritten and they will get `_seed_` as their value.

Some examples.  Suppose columns a, b, c are not null;  m, n are nullable; and x, y have defaults.

```
-- as written
insert into my_table(a) values(7) @dummy_seed(1000)
-- rewrites to
insert into my_table(a, b, c) values(7, 1000, 1000);
```

```
-- as written
insert into my_table(a) values(7) @dummy_seed(1000) @dummy_nullables
-- rewrites to
insert into my_table(a, b, c, m, n) values(7, 1000, 1000, 1000, 1000);
```

```
-- as written
insert into my_table(a) values(7) @dummy_seed(1000) @dummy_nullables @dummy_defaults
-- rewrites to
insert into my_table(a, b, c, m, n, x, y) values(7, 1000, 1000, 1000, 1000, 1000, 1000);
```

The sugar features on `fetch`, `insert`, and `update cursor` are as symmetric as possible, but again, dummy data is generally only interesting in test code. Dummy  data will continue to give you valid test rows even if columns are added or removed from the tables in question.

### Generalized Cursor Lifetimes aka Cursor "Boxing"

Generalized Cursor Lifetime refers to capturing a Statement Cursor in an object so that it can used more flexibly.  Wrapping something
in an object is often called "boxing".  Since Generalized Cursor Lifetime is a mouthful we'll refer to it as "boxing"
from here forward. The symmetric operation "unboxing" refers to converting the boxed object back into a cursor.

The normal cursor usage pattern is by far the most common, a cursor is created directly with something like
these forms:

```sql
declare C cursor for select * from shape_source;

declare D cursor for call proc_that_returns_a_shape();
```

At this point the cursor can be used normally as follows:

```sql
loop fetch C
begin
  -- do stuff with C
end;
```

Those are the usual patterns and they allow statement cursors to be consumed sort of "up" the call chain from where the cursor was created.
But what if you want some worker procedures that consume a cursor? There is no way to pass your cursor down again with these normal patterns alone.

To generalize the patterns, allowing, for instance, a cursor to be returned as an out parameter or accepted as
an in parameter you first need to declare an object variable that can hold the cursor and has a type indicating
the shape of the cursor.

To make an object that can hold a cursor:

```sql
declare obj object<T cursor>;
```

Where `T` is the name of a shape. It can be a table name, or a view name, or it can be the name of the canonical procedure that returns the result.
T should be some kind of global name, something that could be accessed with `#include` in various places.
Referring to the examples above, choices for `T` might be `shape_source` the table or `proc_that_returns_a_shape` the procedure.

Note: it's always possible make a fake procedure that returns a result to sort of "typedef" a shape name.  e.g.

```sql
declare proc my_shape() (id integer not null, name text);
```

The procedure here `my_shape` doesn’t have to actually ever be created, in fact it’s better if it isn't.  It won’t ever be called;
its hypothetical result is just being as a shape.  This can be useful if you have several procedures like `proc_that_returns_a_shape`
that all return results with the columns of `my_shape`.

To create the boxed cursor, first declare the object variable that will hold it and then set object from the cursor.
Note that in the following example the cursor `C` must have the shape defined by `my_shape` or an error is produced.
The type of the object is crucial because, as we'll see, during unboxing that type defines the shape
of the unboxed cursor.


```sql
-- recap: declare the box that holds the cursor (T changed to my_shape for this example)
declare box_obj object<my_shape cursor>;

-- box the cursor into the object (the cursor shape must match the box shape)
set box_obj from cursor C;
```

The variable `box_obj` can now be passed around as usual.  It could be stored in a suitable `out` variable
or it could be passed to a procedure as an `in` parameter.  Then, later, you can "unbox" `box_obj` to get a
cursor back. Like so

```sql
-- unboxing a cursor from an object, the type of box_obj defines the type of the created cursor
declare D cursor for box_obj;
```

These primitives will allow cursors to be passed around with general purpose lifetime.

Example:

```sql
-- consumes a cursor
create proc cursor_user(box_obj object<my_shape cursor>)
begin
   declare C cursor for box_obj;  -- the cursors shape will be my_shape matching box
   loop fetch C
   begin
      -- do something with C
   end;
end;

-- captures a cursor and passes it on
create proc cursor_boxer()
begin
   declare C cursor for select * from something_like_my_shape;
   declare box_obj object<my_shape cursor>
   set box from cursor C; -- produces error if shape doesn't match
   call cursor_user(box_obj);
end;
```

Importantly, once you box a cursor the underlying SQLite statement’s lifetime is managed by the box object with normal
retain/release semantics.  The box and underlying statement can be released simply by setting all references to it to null
as usual.

With this pattern it's possible to, for instance, create a cursor, box it, consume some of the rows in one procedure, do some other stuff,
and then consume the rest of the rows in another different procedure.

Important Notes:

* the underlying SQLite statement is shared by all references to it.  Unboxing does not reset the cursor's position.  It is possible, even desirable, to have different procedures advancing the same cursor
* there is no operation for "peeking" at a cursor without advancing it; if your code requires that you inspect the row and then delegate it, you can do this simply by passing the cursor data as a value rather than the cursor statement.  Boxing and unboxing are for cases where you need to stream data out of the cursor in helper procedures
* durably storing a boxed cursor (e.g. in a global) could lead to all manner of problems -- it is *exactly* like holding on to a `sqlite3_stmt *` for a long time with all the same problems because that is exactly is happening

Summarizing, the main reason for using the boxing patterns is to allow for standard helper procedures that can get a cursor from a variety of places and process it.
Boxing isn’t the usual pattern at all and returning cursors in a box, while possible, should be avoided in favor of the simpler patterns, if only because then then lifetime management is very simple in all those cases.


## Chapter 6: Calling Procedures Defined Elsewhere
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL generally doesn't see the whole world in one compilation.
In this way it's a lot more like, say, the C compiler than it is like, say, Java
or C# or something like that.  This means several things:

* You don't have to tell CQL about all your schema in all your files,
so particular stored procs can be more encapsulated
* You can have different databases mounted in different places and CQL
won't care; you provide the database connection to the stored procedures when you call them, and that database is assumed to have the tables declared in this translation unit
* Several different schema can be maintained by CQL,
even in the same database, and they won't know about each other

To make this possible there are a few interesting features

### Declaring Procedures Defined Elsewhere

Stored procedures defined in another file can be declared to CQL in various
ways for each major type of stored procedure.  These are covered in
the sections below.

### Simple Procedures (database free):

```sql
DECLARE PROCEDURE foo(id integer, out name text not null);
```

This introduces the symbol name without providing the body.
This has important variations.

### Procedures that use the database

```sql
DECLARE PROCEDURE foo(id integer, out name text not null) USING TRANSACTION;
```

Most procedures you write will use SQLite in some fashion,
maybe a `select` or something.  The `USING TRANSACTION` annotation indicates that
the proc in question uses the database and therefore the generated code
will need a database connection in-argument and it will return a SQLite error code.

### Procedures that create a result set

If the procedure in question is going to use `select` or `call` to create a result set,
the type of that result set has to be declared.  An example might look like this:

```sql
DECLARE PROC with_result_set () (id INTEGER NOT NULL,
                                 name TEXT,
                                 rate LONG INTEGER,
                                 type INTEGER,
                                 size REAL);
```

This says that the procedure takes no arguments (other than the implicit database
connection) and it has an implicit out-argument that can be read to get a result
set with the indicated columns: id, name, rate, type, and size.
This form implies `USING TRANSACTION`.

### Procedures that return a single row with a value cursor

If the procedure emits a cursor with the `OUT` statement to produce a single
row then it can be declared as follows:

```sql
DECLARE PROC with_result_set () OUT (id INTEGER NOT NULL,
                                     name TEXT,
                                     rate LONG INTEGER,
                                     type INTEGER,
                                     size REAL);
```

This form can have `USING TRANSACTION`  or not, since it is possible to emit a row with a value cursor and never use the database.  See the previous chapter for details on the `OUT` statement.

### Procedures that return a full result set

If the procedure emits many rows with the `OUT UNION` statement to produce a full result set
then it can be declared as follows:

```sql
DECLARE PROC with_result_set () OUT UNION (id INTEGER NOT NULL,
                                     name TEXT,
                                     rate LONG INTEGER,
                                     type INTEGER,
                                     size REAL);
```

This form can have `USING TRANSACTION`  or not, since it is possible to emit a rows with a value cursor and never use the database.  See the previous chapter for details on the `OUT UNION` statement.

### Exporting Declared Symbols Automatically

To avoid errors, the declarations for any given file can be automatically created
by adding something like `--generate_exports` to the command line. This will require an additonal file name to be passed in the `--cg` portion to capture the exports.

That file can then be used with `#include` when you combine the C pre-processor
with CQL as is normally done.

Nomenclature is perhaps a bit weird here.  You use `--generate_exports` to export
the stored procedure declarations from the translation units.  Of course those
exported symbols are what you then import in some other module.  Sometimes this
output file is called `foo_imports.sql` because those exports are of course exactly
what you need to import `foo`.  You can use whatever convention you like of course,
CQL doesn't care.  The full command line might look something like this:

```
cql --in foo.sql --cg foo.h foo.c foo_imports.sql --generate_exports
```

Using the pre-processor you can get declarations from elsewhere with
a directive like this:

```
#include "foo_imports.sql"
```

### Declaration Examples

Here are some more examples directly from the CQL test cases; these are all
auto-generated with `--generate_exports`.

```sql
DECLARE PROC test (i INTEGER NOT NULL);

DECLARE PROC out_test (OUT i INTEGER NOT NULL, OUT ii INTEGER);

DECLARE PROC outparm_test (OUT foo INTEGER NOT NULL) USING TRANSACTION;

DECLARE PROC select_from_view () (id INTEGER NOT NULL, type INTEGER);

DECLARE PROC make_view () USING TRANSACTION;

DECLARE PROC copy_int (a INTEGER, OUT b INTEGER);

DECLARE PROC complex_return ()
  (_bool BOOL NOT NULL,
   _integer INTEGER NOT NULL,
   _longint LONG INTEGER NOT NULL,
   _real REAL NOT NULL,
   _text TEXT NOT NULL,
   _nullable_bool BOOL);

DECLARE PROC outint_nullable (
  OUT output INTEGER,
  OUT result BOOL NOT NULL)
USING TRANSACTION;

DECLARE PROC outint_notnull (
  OUT output INTEGER NOT NULL,
  OUT result BOOL NOT NULL)
USING TRANSACTION;

DECLARE PROC obj_proc (OUT an_object OBJECT);

DECLARE PROC insert_values (
  id_ INTEGER NOT NULL,
  type_ INTEGER)
  USING TRANSACTION;
```

So far we've avoided discussing the generated C code in any details but here
it seems helpful to show exactly what these declarations correspond to in the
generated C to demystify all this.  There is a very straightforward conversion.

```c
void test(cql_int32 i);

void out_test(
  cql_int32 *_Nonnull i,
  cql_nullable_int32 *_Nonnull ii);

cql_code outparm_test(
  sqlite3 *_Nonnull _db_,
  cql_int32 *_Nonnull foo);

cql_code select_from_view_fetch_results(
  sqlite3 *_Nonnull _db_,
  select_from_view_result_set_ref _Nullable *_Nonnull result_set);

cql_code make_view(sqlite3 *_Nonnull _db_);

void copy_int(cql_nullable_int32 a, cql_nullable_int32 *_Nonnull b);

cql_code complex_return_fetch_results(
  sqlite3 *_Nonnull _db_,
  complex_return_result_set_ref _Nullable *_Nonnull result_set);

cql_code outint_nullable(
  sqlite3 *_Nonnull _db_,
  cql_nullable_int32 *_Nonnull output,
  cql_bool *_Nonnull result);

cql_code outint_notnull(
  sqlite3 *_Nonnull _db_,
  cql_int32 *_Nonnull output,
  cql_bool *_Nonnull result);

void obj_proc(
  cql_object_ref _Nullable *_Nonnull an_object);

cql_code insert_values(
  sqlite3 *_Nonnull _db_,
  cql_int32 id_,
  cql_nullable_int32 type_);
```

As you can see, these declarations use exactly the normal SQLite
types and so it is very easy to declare a procedure in CQL and then implement it
yourself in straight C, simply by conforming to the contract.

Importantly, SQLite does not know anything about CQL stored procedures, or anything at all about CQL
really so CQL stored procedure names cannot be used in any way in SQL statements.  CQL
control flow like the `call` statement can be used to invoke other procedures and
results can be captured by combing the `OUT` statement and a `DECLARE CURSOR` construct
but SQLite is not involved in those things.  This is another place where the inherent
two-headed nature of CQL leaks out.

Finally, this is a good place to reinforce that procedures with any of the structured
result types (`select`, `out`, `out union`) can be used with a suitable cursor.

```sql
create procedure get_stuff()
begin
  select * from stuff;
end;
```

Can be used in two interesting ways:

```sql
create procedure meta_stuff(meta bool)
begin
  if meta then
    call get_stuff();
  else
    call get_other_stuff();
  end if;
end;
```
Assuming that `get_stuff` and `get_other_stuff` have the same shape, then
this procedure simply passes on one or the other's result set unmodified
as its own return value.

But you could do more than simply pass on the result.

```sql
create procedure meta_stuff(meta bool)
begin
  declare C cursor for call get_stuff();  -- or get_meta_stuff(...)
  loop fetch C
  begin
     -- do stuff with C
     -- may be out union some of the rows after adjustment even
  end;
end;
```

Here we can see that we used the procedure to get the results and then
process them directly somehow.

And of course the result of an `OUT` can similarly be processed using
a value cursor, as previously seen.

These combinations allow for pretty general composition of stored procedures
so long as they are not recombined with SQLite statements.


## Chapter 7: CQL Result Sets
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
Most of this tutorial is about the CQL language itself but here we must diverge a bit.  The purpose of the
result set feature of CQL is to create a C interface to SQLite data.  Because of this
there are a lot of essential details that require looking carefully at the generated C code.  Appendix 2
covers this code in even more detail but here it makes sense to at least talk about the interface.

Let's say we have this simple stored procedure:

```sql
create table foo(id integer not null, b bool, t text);

create proc read_foo(id_ integer not null)
begin
  select * from foo where id = id_;
end;
```

We've created a simple data reader: this CQL code will cause the compiler to
generate helper functions to read the data and materialize a result set.

Let's look at the public interface of that result set now considering the most essential pieces.

```c
/* this is almost everything in the generated header file */
#define read_foo_data_types_count 3
cql_result_set_type_decl(
  read_foo_result_set, \
  read_foo_result_set_ref);

extern cql_int32 read_foo_get_id(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_bool read_foo_get_b_is_null(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_bool read_foo_get_b_value(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_string_ref _Nullable read_foo_get_t(
   read_foo_result_set_ref  _Nonnull result_set,
   cql_int32 row);
extern cql_int32 read_foo_result_count(read_foo_result_set_ref
  _Nonnull result_set);
extern cql_code read_foo_fetch_results(sqlite3 *_Nonnull _db_,
  read_foo_result_set_ref _Nullable *_Nonnull result_set,
  cql_int32 id_);
#define read_foo_row_hash(result_set, row) \
  cql_result_set_get_meta((cql_result_set_ref)(result_set))->\
  rowHash((cql_result_set_ref)(result_set), row)
#define read_foo_row_equal(rs1, row1, rs2, row2) \
cql_result_set_get_meta((cql_result_set_ref)(rs1)) \
 ->rowsEqual( \
   (cql_result_set_ref)(rs1),  row1,  \
   (cql_result_set_ref)(rs2),  row2)
```

Let's consider some of these individually now
```c
cql_result_set_type_decl(
  read_foo_result_set,
  read_foo_result_set_ref);
```
This declares the data type for `read_foo_result_set` and the associated object reference `read_foo_result_set_ref`.
As it turns out, the underlying data type for all result sets is the same, and only the shape of the data varies.


```c
extern cql_code read_foo_fetch_results(sqlite3 *_Nonnull _db_,
  read_foo_result_set_ref _Nullable *_Nonnull result_set,
  cql_int32 id_);
```
The result set fetcher method gives you a `read_foo_result_set_ref` if it succeeds.  It accepts the `id_` argument which it
will internally pass along to `read_foo(...)`.  The latter function provides a `sqlite3_stmt*` which can then be iterated in the fetcher.
This method is the main public entry point for result sets.

Once you have a result set, you can read values out of it.

```c
extern cql_int32 read_foo_result_count(read_foo_result_set_ref
  _Nonnull result_set);
```
That function tells you how many rows are in the result set.

For each row you can use any of the row readers:

```c
extern cql_int32 read_foo_get_id(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_bool read_foo_get_b_is_null(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_bool read_foo_get_b_value(read_foo_result_set_ref
  _Nonnull result_set, cql_int32 row);
extern cql_string_ref _Nullable read_foo_get_t(
   read_foo_result_set_ref  _Nonnull result_set,
   cql_int32 row);
```

These let you read the `id` of a particular row, and get a `cql_int32` or you can read the nullable boolean,
using the `read_foo_get_b_is_null` function first to see if the boolean is null and then `read_foo_get_b_value`
to get the value.  Finally the string can be accessed with `read_foo_get_t`.  As you can see, there is a
simple naming convention for each of the field readers.

Note:  The compiler has runtime arrays that control naming conventions as well as using CamelCasing.  Additional customizations may be created by adding new runtime arrays into the CQL compiler.

Finally, also part of the public interface, are these macros:

```c
#define read_foo_row_hash(result_set, row)
#define read_foo_row_equal(rs1, row1, rs2, row2)
```

These use the CQL runtime to hash a row or compare two rows from identical result
set types.  Metadata included in the result set allows general purpose code to work for
every result set.  Based on configuration, result set copying methods can also
be generated.   When you're done with a result set you can use the `cql_release(...)`
method to free the memory.

Importantly, all of the rows from the query in the stored procedure are materialized
immediately and become part of the result set.  Potentially large amounts of memory can
be used if a lot of rows are generated.

The code that actually creates the result set starting from the prepared statement is always the same.
The essential parts are:


First, a constant array that holds the data types for each column.

```
uint8_t read_foo_data_types[read_foo_data_types_count] = {
  CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // id
  CQL_DATA_TYPE_BOOL, // b
  CQL_DATA_TYPE_STRING, // t
};
```

All references are stored together at the end of the row, so we only need the count
of references and the offset of the first one to do operations like `cql_retain` or `cql_release`
on the row.

```
#define read_foo_refs_offset cql_offsetof(read_foo_row, t) // count = 1
```

Lastly we need metadata to tell us count of columns and the offset of each column within the row.
```
static cql_uint16 read_foo_col_offsets[] = { 3,
  cql_offsetof(read_foo_row, id),
  cql_offsetof(read_foo_row, b),
  cql_offsetof(read_foo_row, t)
};
```

Using the above we can now write this fetcher
```
CQL_WARN_UNUSED cql_code
read_foo_fetch_results(
  sqlite3 *_Nonnull _db_,
  read_foo_result_set_ref _Nullable *_Nonnull result_set,
  cql_int32 id_)
{
  sqlite3_stmt *stmt = NULL;
  cql_profile_start(CRC_read_foo, &read_foo_perf_index);

  // we call the original procedure, it gives us a prepared statement
  cql_code rc = read_foo(_db_, &stmt, id_);

  // this is everything you need to know to fetch the result
  cql_fetch_info info = {
    .rc = rc,
    .db = _db_,
    .stmt = stmt,
    .data_types = read_foo_data_types,
    .col_offsets = read_foo_col_offsets,
    .refs_count = 1,
    .refs_offset = read_foo_refs_offset,
    .rowsize = sizeof(read_foo_row),
    .crc = CRC_read_foo,
    .perf_index = &read_foo_perf_index,
  };

  // this function does all the work, it cleans up if .rc is an error code.
  return cql_fetch_all_results(&info, (cql_result_set_ref *)result_set);
}
```

### Results Sets From `OUT UNION`

The `out` keyword was added for writing procedures that produce a single row result set.  With that, it became possible to make any single row result you wanted, assembling it from whatever sources you needed.  That is an important
case as single row results happen frequently and they are comparatively easy to create and pass around using C
structures for the backing store.  However, it's not everything; there are also cases where full flexibility is needed
while producing a standard many-row result set.  For this we have `out union` which was discussed fully in Chapter 5.  Here we'll discuss the code generation behind that.


Here’s an example from the CQL tests:
```sql
create proc some_integers(start integer not null, stop integer not null)
begin
  declare C cursor like select 1 v, 2 v_squared, "xx" some_text;
  declare i integer not null;
  set i := start;
  while (i < stop)
  begin
   fetch C(v, v_squared, junk) from values (i, i*i, printf("%d", i));
   out union C;
   set i := i + 1;
 end;
end;
```

In this example the entire result set is made up out of thin air.  Of course any combination of this computation or data-access is possible, so you can ultimately make any rows you want in any order using SQLite to help you as much or as little as you need.

Virtually all the code pieces to do this already exist for normal result sets.  The important parts of the output code look like this in your generated C.

We need a buffer to hold the rows we are going to accumulate;  We use `cql_bytebuf` just like the normal fetcher above.

```c
// This bit creates a growable buffer to hold the rows
// This is how we do all the other result sets, too
cql_bytebuf _rows_;
cql_bytebuf_open(&_rows_);
```

We need to be able to copy the cursor into the buffer and retain any internal references

```
// This bit is what you get when you "out union" a cursor "C"
// first we +1 any references in the cursor then we copy its bits
cql_retain_row(C_);   // a no-op if there is no row in the cursor
if (C_._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C_, sizeof(C_));
```

Finally, we make the rowset when the procedure exits. If the procedure is returning with no errors the result set is created, otherwise the buffer is released.  The global `some_integers_info` has constants that describe the shape produced by this procedure just like the other cases that produce a result set.
```
cql_results_from_data(_rc_,
                      &_rows_,
                      &some_integers_info,
                      (cql_result_set_ref *)_result_set_);
```
The operations here are basically the same ones that will happen inside of the standard helper
`cql_fetch_all_results`, the difference, of course, is that you write the loop manually and therefore have
full control of the rows as they go in to the result set.

In short, the overhead is pretty low.  What you’re left with is pretty much the base cost of your algorithm.  The cost here is very similar to what it would be for any other thing that make rows.

Of course, if you make a million rows, well, that would burn a lot of memory.

### A Working Example

Here's a fairly simple example illustrating some of these concepts including the reading of rowsets.

```sql
-- hello.sql:

create proc hello()
begin

  create table my_data(
    pos integer not null primary key,
    txt text not null
  );

  insert into my_data values(2, 'World');
  insert into my_data values(0, 'Hello');
  insert into my_data values(1, 'There');

  select * from my_data order by pos;
end;
```

And this main code to open the database and access the procedure:

```c
// main.c

#include <stdlib.h>
#include <sqlite3.h>

#include "hello.h"

int main(int argc, char **argv)
{
  sqlite3 *db;
  int rc = sqlite3_open(":memory:", &db);
  if (rc != SQLITE_OK) {
    exit(1); /* not exactly world class error handling but that isn't the point */
  }
  hello_result_set_ref result_set;
  rc = hello_fetch_results(db, &result_set);
  if (rc != SQLITE_OK) {
    printf("error: %d\n", rc);
    exit(2);
  }

  cql_int32 result_count = hello_result_count(result_set);

  for(cql_int32 row = 0; row < result_count; row++) {
    cql_string_ref text = hello_get_txt(result_set, row);
    cql_alloc_cstr(ctext, text);
    printf("%d: %s\n", row, ctext);
    cql_free_cstr(ctext, text);
  }
  cql_result_set_release(result_set);

  sqlite3_close(db);
}
```

From these pieces you can make a working example like so:

```sh
# ${cgsql} refers to the root directory of the CG-SQL sources
#
cql --in hello.sql --cg hello.h hello.c
cc -o hello -I ${cgsql}/sources main.c hello.c ${cgsql}/sources/cqlrt.c -lsqlite3
./hello
```

Additional demo code is available in [Appendix 10](https://cgsql.dev/cql-guide/x10).

### Nested Result Sets (Parent/Child)

There are many cases where you might want to nest one result set inside of another one.  In order to
do this ecomomically you must be able to run a parent query and a child query and
then link the child rows to the parent rows.  One way to do this is of course to run one query for
each "child" but then you end up with `O(n)` child queries and if there are sub-children it would be
`O(n*m)` and so forth. What you really want to do here is something more like a join, only without
the cross-product part of the join.  Many systems have such features, sometimes they are called
"chaptered rowsets" but in any case there is a general need for such a thing.

To reasonably support nested results sets the CQL language has to be extended a variety of ways,
as discussed below.

Here are some things that happened along the way that are interesting.

#### Cursor Types and Result Types

One of the first problems we run into thinking about how a CQL program might express pieces of a rowset
and turn them into child results is that a program must be able to hash a row, append row data, and
extract a result set from a key.  These are the essential operations required. In order to do anything
at all with a child rowset, a program must be able to describe its type. Result sets must appear
in the type system as well as in the runtime.

To address this we use an object type with a special "kind", similar to how boxed statements are handled.
A result set has a type that looks like this: `object <proc_name set>`.  Here `proc_name` must the the name of a
procedure that returns a result set and the object will represent a result set with the corresponding columns in it.

#### Creating New Cursor Types From Existing Cursor Types

In addition to creating result set types, the language must be able to express cursors that capture the necessary
parent/child column. These are rows with all of the parent columns plus additional columns for the child rows
(note that you can have more than one child result set per parent).  So for instance you might have a list of
people, and one child result might be the details of the schools they attended and another could be the details
of the jobs they worked.

To accomplish this kind of shape, the language must be able to describe a new output row is that is the
same as the parent but includes columns for the the child results, too. This is done using a cursor
declaration that comes from a typed name list.  An example might be:

```sql
declare C cursor like (id integer, name text);
```

Importantly, such constructs include the ability to reference existing shapes by name. So we might create
a cursor we need like so:

```sql
declare result cursor like (like parent_proc, child_result object<child_proc set>);
```

Where the above indicates all the parent columns plus a child result set.  Or more than one child result set if needed.

In addition, the language needs a way to conveniently declare a cursor that is only some of the columns of an existing cursor.
In particular, nested result sets require us to extract the columns that link the parent and child result sets.  The columns
we will "join" on.  To accomplish this the language extends the familiar notion:

```sql
declare D cursor like C;
```

To the more general form:

```sql
declare pks cursor like C(pk1, pk2);
```

Which chooses just the named fields from `C` and makes a cursor with only those. In this case
this primary key fields, `pk1` and `pk2`.  Additionally, for completeness, we add this form:

```sql
declare vals cursor like C(-pk1, -pk2);
```

To mean the cursor vals should have all the columns of `C` except `pk1` and `pk2` i.e. all the "values".

Using any number of intermediate construction steps, and maybe some `declare X type ...` statements,
any type can be formed from existing shapes by adding and removing columns.

Having done the above we can load a cursor that has just the primary keys with the usual form

```sql
fetch pks from C(like pks);
```

Which says we want to load `pks` from the fields of `C`, but using only the columns of `pks`.  That operation
is of course going to be an exact type match by construction.

#### Cursor Arguments

In order to express the requisite parent/child join, the language must be able to express operations like
"hash a cursor" (any cursor) or "store this row into the appropriate partition". The language provides no way
to write functions that can take any cursor and dynamically do things to it based on type information, but:

* we don't need very many of them,
* it's pretty easy to do that job in C (or lua if lua codegen is being used)

The minimum requirement is that the language must be able to declare a functions that takes a generic cursor argument
and to call such functions a generic cursor construct that has the necessary shape info.  This form does the job:

```sql
declare function cursor_hash(C cursor) long not null;
```

And it can be used like so:

```sql
let hash := cursor_hash(C); -- C is any cursor
```

When such a call is made the C function `cursor_hash` is passed a so-called "dynamic cursor" pointer which includes:

* a pointer to the data for the cursor
* the count of fields
* the names of the fields
* the type/offset of every field in the cursor

With this information you can (e.g.) generically do the hash by applying a hash to each field and then combining
all of those hashes. This kind of function works on any cursor and all the extra data about the shape that's needed
to make the call is static, so really the cost of the call stays modest.  Details of the dynamic cursor type are in
`cqlrt_common.h` and there are many example functions now in the `cqlrt_common.c` file.

#### The Specific Parent/Child Functions

Three helper functions are used to do the parent/child join, they are:

```sql
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
the values provided in the second argument into a bucket for that key.  By making a pass over the
child rows a procedure can easily create a partitioning with each unique key combo having a buffer of all
the matching rows.

The third function is used once the partitioning is done.  Given a key again, this time from the parent rows,
a procedure can get the buffer it had accumulated and then make a result set out of it and return that.

Note that the third function returns a vanilla object type because it could be returning a result set of
any shape so a cast is required for correctness.

#### Result Set Sugar

Using the features mentioned above a developer could now join together any kind of complex parent and
child combo as needed, but the result would be a lot of error-prone code, To avoid this CQL adds
language sugar to do such partitionings automatically and type-safely, like so:


```sql
-- parent and child defined elsewhere
declare proc parent(x integer not null) (id integer not null, a integer, b integer);
declare proc child(y integer not null) (id integer not null, u text, v text);

-- join together parent and child using 'id'
-- example x_, y_ arguments for illustration only
create proc parent_child(x_ integer not null, y_ integer not null)
begin
  out union call parent(x_) join call child(y_) using (id);
end;
```

The generated code is simple enough, even though there's a good bit of it.
But it's a useful exercise to look at it once.  Comments added for clarity.

```sql
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

#### Result Set Values

While the above is probably the most common case,  a developer might also want to make a procedure call
for each parent row to compute the child.  And, more generally, to work with result sets from procedure calls
other than iterating them with a cursor.

The iteration pattern:

```sql
declare C cursor for call foo(args);
```

is very good if the data is coming from (e.g.) a select statement and we don't want to materialize all
of the results if we can stream instead.  However, when working with result sets the whole point is to
create materialized results for use elsewhere.

Since we can express a result set type with `object<proc_name set>` the language also includes the ability
to call a procedure that returns a result set and capture that result.  This yields these forms:

```sql
declare child_result object<child set>;
set child_result := child(args);
```

or better still:

```sql
let child_result := child(args);
```

And more generally, this examples shows a manual iteration:

```sql
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

After the sugar is applied to expand the types out, the net program is the following:

```sql
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

Note the `LIKE` and `FROM` forms are make it a lot easier to express this notion
of just adding one more column to the result. The code for emitting the `parent_child`
result doesn't need to specify the columns of the parent or the columns of the child,
only that the parent has at least the `id` column. Even that could have been removed.

This call could have been used instead:

```sql
fetch result from values(from P, child(from P like child arguments));
```

That syntax would result in using the columns of P that match the arguments of `child` -- just
`P.id` in this case.  But if there were many such columns the sugar would be easier to understand
and much less error prone.

#### Generated Code Details

Normally all result sets that have an object type in them use a generic object `cql_object_ref`
as their C data type. This isn't wrong exactly but it would mean that a cast would be required
in every use case on the native side, and it's easy to get the cast wrong.  So the result type
of column getters is adjusted to be a `child_result_set_ref` instead of just `cql_object_ref`
where `child` is the name of the child procedure.


## Chapter 8: Functions
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL stored procs have a very simple contract so it is easy to declare procedures and then implement them in regular C; the C functions just have to conform to the contract.  However, CQL procedures have their own calling conventions and this makes it very inconvenient to use external code that is not doing database things and wants to return values.  Even a random number generator or something would be difficult to use because it could not be called in the context of an expression.  To allow for this CQL adds declared functions

In another example of the two-headed nature of CQL, there are two ways to declare functions.  As we have already
seen you can make function-like procedures and call them like functions simply by making a procedure with an `out` parameter. However, there are also cases where it is reasonable to make function calls to external functions of other kinds.  There are three major types of functions you might wish to call.

### Function Types

#### Ordinary Scalar Functions

These functions are written in regular C and provide for the ability to do operations on in-memory objects.  For instance,
you could create functions that allow you to read and write from a dictionary.  You can declare these functions like so:

```sql
declare function dict_get_value(dict object, key_ text not null) text;
```

Such a function is not known to SQLite and therefore cannot appear in SQL statements.  CQL will enforce this.

The above function returns a text reference, and, importantly, this is a borrowed reference.  The dictionary
is presumably holding on to the reference and as long as it is not mutated the reference is valid.  CQL will
retain this reference as soon as it is stored and release it automatically when it is out of scope.  So, in
this case, the dictionary continues to own the object.

It is also possible to declare functions that create objects.  Such as this example:

```sql
declare function dict_create() create object;
```

This declaration tells CQL that the function will create a new object for our use.  CQL does not retain the
provided object, rather assuming ownership of the presumably one reference count the object already has.
When the object goes out of scope it is released as usual.

If we also declare this procedure:

```sql
declare procedure dict_add(
    dict object not null,
    key_ text not null,
    value text not null);
```

then with this family of declarations we could write something like this:

```sql
create proc create_and_init(out dict object not null)
begin
  set dict := dict_create();
  call dict_add(dict, "k1", "v1");
  call dict_add(dict, "k2", "v2");
  if (dict_get_value(dict, "k1") == dict__get_value(dict, "k2")) then
    call printf("insanity has ensued\n");
  end if;
end;
```

Note: Ordinary scalar functions may not use the database in any way. When they are invoked they will not
be provided with the database pointer and so they will be unable to do any database operations.  To do
database operations, use regular procedures.  You can create a function-like-procedure using the `out` convention
discussed previously.

#### SQL Scalar Functions

SQLite includes the ability to add new functions to its expressions using `sqlite3_create_function`.  In
order to use this function in CQL, you must also provide its prototype definition to the compiler.  You
can do so following this example:

```sql
declare select function strencode(t text not null) text not null;
```

This introduces the function `strencode` to the compiler for use in SQL constructs.  With this done you
could write a procedure something like this:

```sql
create table foo(id integer, t text);

create procedure bar(id_ integer)
begin
   select strencode(T1.t) from foo T1 where T1.id = id_;
end;
```

This presumably returns the "encoded" text, whatever that might be.  Note that if `sqlite3_create_function`
is not called before this code runs, a run-time error will ensue.  Just as CQL must assume that declared
tables really are created, it also assumes that declared function really are created.  This is another case
of telling the compiler in advance what the situation will be at runtime.

SQLite allows for many flexible kinds of user defined functions.  CQL doesn't concern itself with the details of the implementation of the function, it only needs the signature so that it can validate calls.

Note that SQL Scalar Functions cannot contain `object` parameters. To pass an `object`, you should instead pass
the memory address of this object using a `LONG INT` parameter. To access the address of an `object` at runtime, you should use
the `ptr()` function. See [the notes section below](#notes-on-builtin-functions) for more information.

See also: [Create Or Redefine SQL Functions](https://www.sqlite.org/c3ref/create_function.html).

#### SQL Table Valued Functions

More recent versions of SQLite also include the ability to add table-valued functions to statements in place of actual tables. These functions can use their arguments to create a "virtual table" value for use in place of a table.  For this
to work, again SQLite must be told of the existence of the table.  There are a series of steps to make this happen
beginning with `sqlite3_create_module` which are described in the SQLite documents under "The Virtual Table Mechanism Of SQLite."

Once that has been done, a table-valued function can be defined for most object types.  For instance it is possible to
create a table-valued function like so:

```sql
declare select function dict_contents(dict object not null)
   (k text not null, v text not null);
```

This is just like the previous type of select function but the return type is a table shape.  Once the above has been done you can legally write something like this:

```sql
create proc read_dict(dict object not null, pattern text)
begin
  if pattern is not null then
    select k, v from dict_contents(dict) T1 where T1.k LIKE pattern;
  else
    select k, v from dict_contents(dict);
  end if;
end;
```

This construct is very general indeed but the runtime set up for it is much more complicated than scalar functions
and only more modern versions of SQLite even support it.

### SQL Functions with Unchecked Parameter Types

Certain SQL functions like [`json_extract`](https://www.sqlite.org/json1.html#jex) are variadic (they accept variable number of arguments). To use such functions within CQL, you can declare a SQL function to have untyped parameters by including the `NO CHECK` clause instead of parameter types.

For example:
```sql
declare select function json_extract no check text;
```

This is also supported for SQL table-valued functions:
```sql
declare select function table_valued_function no check (t text, i int);
```

> Note: currently the `NO CHECK` clause is not supported for non SQL [Ordinary Scalar Functions](#Ordinary-Scalar-Functions).

### Notes on Builtin Functions

Some of the SQLite builtin functions are hard-coded;  these are the functions that have semantics that are not readily captured with a simple prototype.  Other SQLite functions can be declared with `declare select function ...` and then used.

CQL's hard-coded builtin list includes:

*Aggregate Functions*

 * count
 * max
 * min
 * sum
 * total
 * avg
 * group_concat

*Scalar Functions*

 * ifnull
 * nullif
 * upper
 * char
 * abs
 * instr
 * coalesce
 * last_insert_rowid
 * printf
 * strftime
 * date
 * time
 * datetime
 * julianday
 * substr
 * replace
 * round
 * trim
 * ltrim
 * rtrim

*Window Functions*

 * row_number
 * rank
 * dense_rank
 * percent_rank
 * cume_dist
 * ntile
 * lag
 * lead
 * first_value
 * last_value
 * nth_value

Special Functions
 * nullable
 * sensitive
 * ptr

`Nullable` casts an operand to the nullable version of its type and otherwise does nothing.  This cast might be useful if you need an exact type match in a situation.  It is stripped from any generated SQL and generated C so it has no runtime effect at all other than the indirect consequences of changing the storage class of its operand.

`Sensitive` casts an operand to the sensitive version of its type and otherwise does nothing.  This cast might be useful if you need an exact type match in a situation.  It is stripped from any generated SQL and generated C so it has no runtime effect at all other than the indirect consequences of changing the storage class of its operand.

`Ptr` is used to cause a reference type variable to be bound as a long integer to SQLite. This is a way of giving object pointers to SQLite UDFs. Not all versions of Sqlite support
binding object variables, so passing memory addresses is the best we can do on all versions.


## Chapter 9: Statements Summary and Error Checking
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
The following is a brief discussion of the major statement types and the semantic rules that CQL enforces for each of the statements.  A detailed discussion of SQL statements (the bulk of these) is beyond the scope of this document and you should refer to the SQLite documentation for most details.  However, in many cases CQL does provide additional enforcement and it is helpful to describe the basic checking that happens for each fragment of CQL.  A much
more authoritative list of the things CQL checks for can be inferred from the error documentation.  "Tricky" errors have examples and suggested remediation.

### The Primary SQL Statements

These are, roughly, the statements that involve the database.

#### The `SELECT` Statement
Top level statement list processing for select.  This is easily the hardest
statement to process. Each clause has its own set of complex rules and
the result of previous clauses constrains the next in a complex fashion.
Among the things that are verified:
* the mentioned tables exist and have the mentioned columns
* the columns are type compatible in their context
* any variables in the expressions are compatible
* aggregate functions are used only in places where aggregation makes sense
* column and table names are unambiguous, especially when self-joins are involved
* compound selects (e.g. with UNION) are type-consistent in all the fragments
* the projection of a select has unique column labels if they are used

#### The `SELECT *` Statement
`SELECT *` is special in that it creates its own struct type by assembling
all the columns of all the tables in the select's join result.  CQL rewrites these
column names into a new `SELECT` with the specific columns explicitly listed.
While this makes the program slightly bigger it means that logically deleted columns
are never present in results because `SELECT *` won't select them and attempting
to use a logically deleted column results in an error.

#### The `CREATE TABLE` Statement
Unlike the other parts of DDL we actually deeply care about the tables.
We have to grab all the columns and column types out of it and create
the appropriate structure type for the table.
Along the way we validate a bunch of stuff like:
* verify unique table name
* no duplicate column names
* recursive correctness of constraints (see constraints discussion below)

##### The `UNIQUE KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again, we use the usual helpers
for name lookup within the context of the table that contains the constraint.

##### The `FOREIGN KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again, we use the usual helpers
for name lookup within the context of the table with the foreign key.
 Note that the foreign key has to be validated against two tables to fully validate it.

##### The `PRIMARY KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again, we use the usual helpers
for name lookup within the context of the table with the primary key.

##### The `CHECK` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  The `CHECK` clause is validated
after the entire table has been processed so that even if it appears
early in the table, the clause can use any columns defined later in the
table.

#### The `CREATE INDEX` Statement
CQL doesn't really do anything with indices but we do validate that they make sense (so we lookup all the names of all the columns and so forth.)

#### The `CREATE VIEW` Statement
Create view analysis is very simple because the `select` analysis does the heavy lifting.  All we
have to do is validate that the view is unique, then validate the select statement.

Additionally, views must not be allowed to have any NULL type columns; all nulls must be converted to
some type with a CAST.   e.g. `create view foo as select NULL n` is not valid.  NULL is not a real storage type.

#### The `CREATE TRIGGER` Statement
The create trigger statement is quite a beast, and validations include:
 * The trigger name must be unique
 * For `insert` the "new.*" table is available in expressions/statement
 * For `delete` the "old.*" table is available in expressions/statements
 * For `update` both are available
    * If optional columns present in the `update`, they must be unique/valid
 * The `when` expression must evaluate to a numeric
 * The statement list must be error free with the usual rules plus new/old
 * The `raise` function may be used inside a trigger (NYI)
 * The table name must be a table (not a view) UNLESS the trigger type is `INSTEAD OF`
 * Select statements inside the statement block do not count as returns for the procedure and that includes the create trigger

#### The `DROP TABLE` Statement
This is the basic checking for the drop table statement:
* the table must exist in some version
* it has to be a table and not a view

#### The `DROP VIEW` Statement
This is the basic checking for the drop view statement:
* the view must exist in some version
* it has to be a view and not a table

#### The `DROP INDEX` Statement
This is the basic checking for the drop index statement:
* the index must exist in some version
* it could be deleted now, that's ok, but the name has to be valid

#### The `DROP TRIGGER` Statement
This is the basic checking for the drop trigger statement
* the trigger  must exist in some version
* it could be deleted now, that's ok, but the name has to be valid

#### The `RAISE` Statement
CQL validates that `RAISE` is being used in the context of a trigger and that
it has the correct arguments

#### The `ALTER TABLE ADD COLUMN` Statement
To validate `alter table add column` we check the following:
* the table must exist and not be a view (in any version)
* the column definition of the new column must be self-consistent
* no auto-increment columns may be added
* added columns must be either nullable or have a default value

Note: Alter statements are typically used in the context of migration, so it's
possible the table that is mentioned is condemned in a future version.  We still have to run
the intervening upgrade steps so basically DDL gets to ignore the current deadness
of the table as in context it might be "not dead yet".  This will be more obvious
in the context of the schema maintenance features. (q.v.)

#### The `DELETE` Statement
The delete analyzer sets up a scope for the table being
deleted and then validates the WHERE clause, if present, against that scope.
Additionally, we verify that the table actually was defined and is not a view.

#### The `UPDATE` Statement
The update analyzer sets up the scope for the table(s) being updated.  If there are
optional clauses (e.g. `LIMIT`), they are evaluated just like in a select statement
with those same helper methods.  Expression fragments are evaluated similarly as
in a select statement.

#### The `INSERT` Statement
We check that the table exists and then we walk the columns and the value list
to make sure they are valid for the table. Also, we cannot insert into a view.

Details:
* The column list specifies the columns we will provide; they must exist and be unique.
* The columns specified must suffice to insert a row (all not nulls and not default present.)
* The insert list specifies the values that are to be inserted.
* The type of each value must match the type of the column.
* Auto-increment columns may be specified as NULL.
* If there are too many or too few columns, that is considered an error.
* If no columns are specified, that is the same as if all columns had been specified, in table order.

#### The `THROW` Statement
Throw can literally go anywhere, so it's always ok.

#### The `BEGIN TRANSACTION` Statement
Begin transaction can go anywhere, so it's always ok.

The sqlite documentation can be helpful here (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `COMMIT TRANSACTION` Statement
Commit transaction can go anywhere, so it's always ok.

The sqlite documentation can be helpful here (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `ROLLBACK TRANSACTION` Statement
Rollback transaction can go anywhere but if you're using the format
where you rollback to a particular save point, then we must have
seen that name in a `savepoint` statement previously. Otherwise, it's an error.

The sqlite documentation can be helpful here again (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `SAVEPOINT` Statement
The `savepoint` statement can go anywhere but we do record this savepoint name
as having been seen, so that we can verify it in rollback.  So this is sort of a weak declaration of the savepoint name.

The sqlite documentation can be helpful here (CQL syntax is a subset).  https://www.sqlite.org/lang_savepoint.html

#### The `RELEASE SAVEPOINT` Statement
Release savepoint can go anywhere but we must have
seen that name in a previous `savepoint` statement, otherwise it's an error.

The sqlite documentation can be helpful here (CQL syntax is a subset). https://www.sqlite.org/lang_savepoint.html

#### The `PROCEDURE SAVEPOINT` Statement
A common pattern is to have a savepoint associated with a particular procedure. The savepoint's scope is the same
as the procedure's scope.  More precisely

```sql
create procedure foo()
begin
  proc savepoint
  begin
   -- your code
  end;
end;
```

becomes:

```sql
create procedure foo()
begin
  savepoint @proc;  -- @proc is always the name of the current procedure
  begin try
    -- your code
    release savepoint @proc;
  end try;
  begin catch
    rollback transaction to savepoint @proc;
    release savepoint @proc;
    throw;
  end catch;
end;
```

This form is not quite syntactic sugar because there are some interesting rules:

* the `proc savepoint` form must be used at the top level of the procedure, hence no `leave` or `continue` may escape it
* within `begin`/`end` the `return` form may not be used; you must use `rollback return` or `commit return` (see below)
* `throw` may be used to return an error as usual
* `proc savepoint` may be used again, at the top level, in the same procedure, if there are, for instance, several sequential stages
* a procedure using `proc savepoint` could call another such procedure, or a procedure that manipulates savepoints in some other way

#### The `ROLLBACK RETURN` Statement

This form may be used only inside of  a `proc savepoint` block.  It indicates that the savepoint should be rolled back and then the procedure
should return.  It is exactly equivalent to:

```sql
  rollback transaction to savepoint @proc;
  release savepoint @proc;
  return; -- wouldn't actually be allowed inside of proc savepoint; see note below
```

Note: to avoid errors, the loose `return` above is not actually allowed inside of `proc savepoint` -- you must use `rollback return` or `commit return`.

#### The `COMMIT RETURN` Statement

This form may be used only inside of  a `proc savepoint` block.  It indicates that the savepoint should be released and then the procedure
should return.  It is exactly equivalent to:

```sql
  release savepoint @proc;
  return; -- wouldn't actually be allowed inside of proc savepoint; see note below
```

Of course this isn't exactly a commit, in that there might be an outer savepoint or outer transaction that might
still be rolled back, but it is commited at its level of nesting, if you will.  Or, equivalently, you can think of
it as merging the savepoint into the transaction in flight.

Note: to avoid errors, the loose `return` above is not actually allowed inside of `proc savepoint` and you must use `rollback return` or `commit return`.

#### The `CREATE VIRTUAL TABLE` Statement

The SQLite `CREATE VIRTUAL TABLE` form (https://sqlite.org/lang_createvtab.html) is problematic from CQL because:

* it is not parseable, because the module arguments can be literally anything (or nothing), even a letter to your grandma
* the arguments do not necessarily say anything about the table's schema at all

So the CQL form departs from the standard syntax to this form:

```sql
create virtual table virt_table using my_module [(module arguments)]  as (
  id integer not null,
  name text
);
```

The part after the `AS` is used by CQL as a table declaration for the virtual table.  The grammar for that
is exactly the same as a normal `CREATE TABLE` statement.  However, that part is not transmitted to
SQLite; when the table is created, SQLite sees only the part it cares about, which is the part before the `AS`.

In order to have strict parsing rules, the module arguments follow one of these forms:

1. no arguments at all
2. a list of identifiers, constants, and parenthesized sublists, just like in the `@attribute` form
3. the words `arguments following`


##### Case 1 Example

```sql
create virtual table virt_table using my_module as (
  id integer not null,
  name text
);
```

becomes (to SQLite)

```sql
CREATE VIRTUAL TABLE virt_table USING my_module;
```

Note: empty arguments `USING my_module()` are not allowed in the SQLite docs but do seem to work in SQLite.
We take the position that no args should be formatted with no parentheses, at least for now.

##### Case 2 Example

```
create virtual table virt_table using my_module(foo, 'goo', (1.5, (bar, baz))) as (
  id integer not null,
  name text
);
```

```
CREATE VIRTUAL TABLE virt_table USING my_module(foo, "goo", (1.5, (bar, baz)));
```

This form allows for very flexible arguments but not totally arbitrary arguments, so it can still be
parsed and validated.

##### Case 3 Example

This case recognizes the popular choice that the arguments are often the actual schema declaration
for the table in question. So:

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

##### Other details

Virtual tables go into their own section in the JSON and they include the `module` and `moduleArgs` entries; they are additionally
marked `isVirtual` in case you want to use the same processing code for virtual tables as normal tables.  The JSON format is otherwise
the same, although some things can't happen in virtual tables (e.g. there is no `TEMP` option so `"isTemp"` must be false in the JSON.)

For purposes of schema processing, virtual tables are on the `@recreate` plan, just like indices, triggers, etc.  This is the only option since
the `alter table` form is not allowed on a virtual table.

Semantic validation enforces "no alter statements on virtual tables" as well as other things like no indices, and no triggers, since SQLite
does not support any of those things.

CQL supports the notion of [eponymous virtual tables](https://www.sqlite.org/vtab.html#epovtab).  If you intend to register the virtual
table's module in this fashion, you can use `create virtual table @eponymous ...` to declare this to CQL.  The only effect this has
is to ensure that CQL will not try to drop this table during schema maintenance as dropping such a table is an invalid operation.  In
all other ways, the fact that the table is eponymous makes no difference.

Finally, because virtual tables are on the `@recreate` plan, you may not have foreign keys that reference virtual tables. Such keys seem
like a bad idea in any case.


### The Primary Procedure Statements

These are the statements which form the language of procedures, and do not involve the database.

#### The `CREATE PROCEDURE` Statement
Semantic analysis of stored procedures is fairly easy at the core:
 * check for duplicate names
 * validate the parameters are well formed
 * set the current proc in flight (this not allowed to nest)
 * recurse on the statement list and prop errors
 * record the name of the procedure for callers
In addition, while processing the statement:
 * we determine if it uses the database; this will change the emitted signature of the proc to include a `sqlite3 *db`
     input argument and it will return a sqlite error code (e.g. `SQLITE_OK`)
 * select statements that are loose in the proc represent the "return" of that
   select;  this changes the signature to include a `sqlite3_stmt **pstmt` parameter corresponding to the returned statement

#### The `IF` Statement
The top level if node links the initial condition with a possible
series of else_if nodes and then the else node.  Each condition is
checked for validity. The conditions must be valid expressions that
can each be converted to a boolean.

#### The `SET` Statement
The set statement is for variable assignment.  We just validate
that the target exists and is compatible with the source.
Cursor variables cannot be set with simple assignment and CQL generates
errors if you attempt to do so.

#### The `LET` Statement
Let combines a `DECLARE` and a `SET`.  The variable is declared to be
the exact type of the right hand side.  All the validations for `DECLARE`
and `SET` are applicable, but there is no chance that the variable will
not be compatible with the expression.  The expression could still be
erroneous in the first place.  The variable could be a duplicate.

#### The `SWITCH` Statement
The `SWITCH` form requires a number of conditions to successfully map
down to a `C` `switch` statement.  These are:

* the switch-expression must be a not-null integral type (`integer not null` or `long integer not null`)
  * the `WHEN` expressions must be losslessly promotable to the type of the switch-expression
* the values in the `WHEN` clauses must be unique
* If `ALL VALUES` is present then:
   * the switch-expression must be of an enum type
   * the `WHEN` values must cover every value of the enum except those beginning with '_'
   * there can be no extra `WHEN` values not in the enum
   * there can be no `ELSE` clause

#### The `DECLARE PROCEDURE` Statement
There are three forms of this declaration:
* a regular procedure with no DML
   * e.g. `declare proc X(id integer);`
* a regular procedure that uses DML (it will need a db parameter and returns a result code)
   * e.g. `declare proc X(id integer) using transaction;`
* a procedure that returns a result set, and you provide the result columns
   * e.g. `declare proc X(id integer) : (A bool not null, B text);`
The main validations here are that there are no duplicate parameter names, or return value columns.

#### The `DECLARE FUNCTION` Statement
Function declarations are similar to procedures; there must be a return type
(use proc if there is none).  The `DECLARE SELECT FUNCTION` form indicates a function
visible to SQLite; other functions are usable in the `call` statement.

#### The `DECLARE` Variable Statement
This declares a new local or global variable that is not a cursor.
The type is computed with the same helper that is used for analyzing
column definitions.  Once we have the type we walk the list of variable
names, check them for duplicates and such (see above) and assign their type.  The canonical
name of the variable is defined here. If it is later used with a different casing the output
will always be as declared.   e.g. `declare Foo integer;  set foo = 1;` is legal but the output
will always contain the variable written as `Foo`.

#### The `DECLARE` Cursor Statement
There are two forms of the declare cursor, both of which allow CQL to infer the exact type of the cursor.
  * `declare foo cursor for select etc.`
    * the type of the cursor is the net struct type of the select list
  * `declare foo cursor for call proc();`
    * proc must be statement that produces a result set via select (see above)
    * the type of the cursor is the struct of the select returned by the proc
    * note if there is more than one loose select in the proc they must match exactly
  * cursor names have the same rules regarding duplicates as other variables
With this in mind, both cases simply recurse on either the select or the call
and then pull out the structure type of that thing and use it for the cursor's shape.  If the
`call` is not semantically valid according to the rules for calls or the `select` is not semantically valid,
 then of course this declaration will generate errors.

#### The `DECLARE` Value Cursor Statement
This statement declares a cursor that will be based on the return type of a procedure.
When using this form the cursor is also fetched, hence the name.  The fetch result of
the stored proc will be used for the value.  At this point, we use its type only.
* the call must be semantically valid
* the procedure must return an OUT parameter (not a result set)
* the cursor name must be unique

#### The `WHILE` Statement
While semantic analysis is super simple.
 * the condition must be numeric
 * the statement list must be error-free
 * loop_depth is increased allowing the use of interior leave/continue

#### The `LOOP` Statement
Loop analysis is just as simple as "while" -- because the loop_stmt
literally has an embedded fetch, you simply use the fetch helper to
validate that the fetch is good and then visit the statement list.
Loop depth is increased as it is with while.

#### The `CALL` Statement
There are three ways that a call can happen:
  * signatures of procedures that we know in full:
    * call foo();
    * declare cursor for call foo();
  * some external call to some outside function we don't know
    * e.g. call printf('hello, world\n');

The cursor form can be used if and only if the procedure has a loose select
or a call to a procedure with a loose select. In that case, the procedure will
have a structure type, rather than just "ok" (the normal signature for a proc).
If the user is attempting to do the second case, cursor_name will be set and
the appropriate verification happens here.

Note:  Recursively calling fetch cursor is not really doable in general
because at the point in the call we might not yet know that the method
does in fact return a select.  You could make it work if you put the select
before the recursive call.

Semantic rules:
 * for all cases each argument must be error-free (no internal type conflicts)
 * for known procs
   * the call has to have the correct number of arguments
   * if the formal is an out parameter the argument must be a variable
     * the type of the variable must be an exact type match for the formal
   * non-out parameters must be type-compatible, but exact match is not required

#### The `DECLARE OUT CALL` Statement
This form is syntactic sugar and corresponds to declaring any `OUT` parameters
of the `CALL` portion that are not already declared as the exact type of the
`OUT` parameter.  This is intended to save you from declaring a lot of variables
just so that you can use them as `OUT` arguments.

Since any variables that already exist are not re-declared, there are no
additional semantic rules beyond the normal call except that it is an error
to use this form if no `OUT` variables needed to be declared.

#### The `FETCH` Statement
The fetch statement has two forms:
  * fetch C into var1, var2, var3 etc.
  * fetch C;
The second form is called the auto_cursor.
In the first form the variables of the cursor must be assignment compatible
with declared structure type of the cursor and the count must be correct.
In the second form, the codegen will implicitly create local variables that
are exactly the correct type, but we'll cover that later.  Since no semantic error is
possible in that case, we simply record that this is an auto_cursor and then
later we will allow the use of C.field during analysis.
Of course "C" must be a valid cursor.

#### The `CONTINUE` Statement
We just need to ensure that `continue` is inside a `loop` or `while`.

#### The `LEAVE` Statement
We only need to ensure that `leave` is inside a `loop`, `while` or `switch`.

#### The `TRY/CATCH` Statements
No analysis needed here other than that the two statement lists are ok.

#### The `CLOSE` CURSOR Statement
For close [cursor], we just validate that the name is in fact a cursor
and it is not a boxed cursor.  Boxed cursor lifetime is managed by the
box object so manually closing it is not allowed.  Instead, the usual
reference-counting semantics apply; the boxed cursor variable typically falls out of
scope and is released, or is perhaps set to NULL to release its reference early.

#### The `OUT` CURSOR Statement
For out [cursor], we first validate that the name is a cursor
then we set the output type of the procedure we're in accordingly.

### The "Meta" Statements
The program's control/ the overall meaning of the program / or may give the compiler specific directives
as to how the program should be compiled.

#### The `@ECHO` Statement
Echo is valid in any top level contexts.

#### The `@PREVIOUS SCHEMA` Statement
Begins the region where previous schema will be compared against what has been
declared before this directive for alterations that could not be upgraded.

#### The `@SCHEMA_UPGRADE_SCRIPT` Statement
When upgrading the DDL, it's necessary to emit create table statements
for the original version of the schema.  These create statements may conflict
with the current version of the schema.  This attribute tells CQL to
1) ignore DDL in stored procedures for declaration purposes; only DDL outside of a proc counts
2) do not make any columns "hidden" thereby allowing all annotations to be present
   so they can be used to validate other aspects of the migration script.

#### The `@SCHEMA_UPGRADE_VERSION` Statement
For sql stored procedures that are supposed to update previous schema versions
you can use this attribute to put CQL into that mindset.  This will make
the columns hidden for the version in question rather than the current version.
This is important because older schema migration procedures might still refer to
old columns.  Those columns truly exist at that schema version.

#### The `@ENFORCE_STRICT` Statement
Switch to strict mode for the indicated item.  The choices and their meanings are:

  * "FOREIGN KEY ON DELETE" indicates there must be some `ON DELETE` action in every FK
  * "FOREIGN KEY ON UPDATE" indicates there must be some `ON UPDATE` action in every FK
  * "INSERT SELECT" indicates that insert with `SELECT` for values may not include top level joins (avoiding a SQLite bug)
  * "IS TRUE" indicates that `IS TRUE` `IS FALSE` `IS NOT TRUE` `IS NOT FALSE` may not be used (*)
  * "JOIN" indicates only ANSI style joins may be used, and "from A,B" is rejected
  * "PROCEDURE" indicates no calls to undeclared procedures (like loose printf calls)
  * "SELECT IF NOTHING" indicates `(select ...)` expressions must include an `IF NOTHING` clause if they have a `FROM` part
  * "TABLE FUNCTIONS" indicates table valued functions cannot be used on left/right joins (avoiding a SQLite bug)
  * "TRANSACTION" indicates no transactions may be started, committed, or aborted
  * "UPSERT" indicates no upsert statement may be used (*)
  * "WINDOW FUNCTION" indicates no window functions may be used (*)
  * "WITHOUT ROWID" indicates `WITHOUT ROWID` may not be used

The items marked with * are present so that features can be disabled to target downlevel versions of SQLite
that may not have those features.

See the grammar details for exact syntax.

#### The `@ENFORCE_NORMAL` Statement
Turn off strict enforcement for the indicated item.

#### The `@ENFORCE_PUSH` Statement
Push the current strict settings onto the enforcement stack.  This does not change the current settings.

#### The `@ENFORCE_POP` Statement
Pop the previous current strict settings from the enforcement stack.

#### The `@ENFORCE_RESET` Statement
Turns off all the strict modes.  Best used immediately after `@ENFORCE_PUSH`.

#### The `@DECLARE_SCHEMA_REGION` Statement
A schema region is a partitioning of the schema such that it
only uses objects in the same partition or one of its declared
dependencies.  One schema region may be upgraded independently
from any others (assuming they happen such that dependents are done first.)
Here we validate:
 * the region name is unique
 * the dependencies (if any) are unique and exist
 * the directive is not inside a procedure

#### The `@BEGIN_SCHEMA_REGION` Statement
Entering a schema region makes all the objects that follow part of that
region.  It also means that all the contained objects must refer to
only pieces of schema that are in the same region or a dependent region.
Here we validate that region we are entering is in fact a valid region
and that there isn't already a schema region.

#### The `@END_SCHEMA_REGION` Statement
Leaving a schema region puts you back in the default region.
Here we check that we are in a schema region.

#### The `@EMIT_ENUMS` Statement
Declared enumarations can be voluminous and it is undesirable for every
emitted `.h` file to contain every enumeration.  To avoid this problem
you can emit enumeration values of your choice using `@emit_enums x, y, z`
which places the named enumerations into the `.h` file associated with
the current translation unit. If no enumerations are listed, all enums
are emitted.

Note: generated enum definitions are protected by `#ifndef X ... #endif` so multiple
definitions are harmless and hence you can afford to use `@emit_enums`
for the same enum in several translations units, if desired.

Note: Enumeration values also appear in the JSON output in their own section.

#### The `@EMIT_CONSTANTS` Statement
This statement is entirely analogous to the the `@EMIT_ENUMS` except that
the parameters are one or more constant groups.  In fact constants are put
into groups precisely so that they can be emitted in logical bundles (and
to encourage keeping related constants together).  Placing `@EMIT_CONSTANTS`
causes the C version of the named groups to go into the current `.h` file.

Note: Global constants also appear in the JSON output in their own section.

### Important Program Fragments

These items appear in a variety of places and are worthy of discussion.  They are generally handled uniformly.

#### Argument Lists
In each case we walk the entire list and do the type inference on each argument.
Note that this happens in the context of a function call, and depending
on what the function is, there may be additional rules for compatibility of the
arguments with the function.  The generic code doesn't do those checks, there
is per-function code that handles that sort of thing.

At this stage the compiler computes the type of each argument and makes sure
that, independently, they are not bogus.

#### Procedures that return a Result Set
If a procedure is returning a select statement then we need to attach a
result type to the procedure's semantic info.  We have to do some extra validation
at this point, especially if the procedure already has some other select that
might be returned.  The compiler ensures that all the possible select results are
are 100% compatible.

#### General Name Lookups
Every name is checked in a series of locations.  If the name is known to be
a table, view, cursor, or some other specific type of object then only those
name are considered.  If the name is more general a wider search is used.

Among the places that are considered:
* columns in the current join if any (this must not conflict with #2)
* local or global variables
* fields in an open cursor
* fields in enumerations and global constants

#### Data Types with a Discriminator
Discriminators can appear on any type, `int`, `real`, `object`, etc.

Where there is a discriminator the compiler checks that (e.g.) `object<Foo>` only combines
with `object<Foo>` or `object`.  `real<meters>` only combines with `real<meters>` or `real`.
In this way its not possible to accidentally add `meters` to `kilograms` or to store
an `int<task_id>` where an `int<person_id>` is required.

#### The `CASE` Expression
There are two parts to this: the "when" expression and the "then" expression.
We compute the aggregate type of the "when" expressions as we go, promoting it
up to a larger type if needed (e.g. if one "when" is an int and the other is
a real, then the result is a real).  Likewise, nullability is computed as
the aggregate.  Note that if nothing matches, the result is null, so we always
get a nullable resultm unless there is an "else" expression.
If we started with case expression, then each "when" expression must be comparable
to the case expression.  If we started with case when xx then yy;  then
each case expression must be numeric (typically boolean).

#### The `BETWEEN` EXPRESSIONS
Between requires type compatibility between all three of its arguments.
Nullability follows the usual rules: if any might be null then the result
type might be null.  In any case, the result's core type is BOOL.

#### The `CAST` Expression
For cast expressions we use the provided semantic type;
the only trick is that we preserve the extra properties of the input argument.
e.g. CAST does not remove `NOT NULL`.

#### The `COALESCE` Function
Coalesce requires type compatibility between all of its arguments.  The result
is a not null type if we find a not null item in the list.  There should be
nothing after that item.  Note that ifnull and coalesce are really the same thing
except ifnull must have exactly two arguments.

#### The `IN` AND `NOT IN` Expressions
The in predicate is like many of the other multi-argument operators.  All the
items must be type compatible.  Note that in this case the nullablity of
the items does not matter, only the nullability of the item being tested.
Note that null in (null) is null, not true.

#### Aggregate Functions
Aggregate functions can only be used in certain places.  For instance
they may not appear in a `WHERE` clause.

#### User Defined Functions
User defined function - this is an external function.
There are a few things to check:
 * If this is declared without the select keyword then
    * we can't use these in SQL, so this has to be a loose expression
 * If this is declared with the select keyword then
    * we can ONLY use these in SQL, not in a loose expression
 * args have to be compatible with formals

#### Calling a procedure as a function
There are a few things to check:
 * we can't use these in SQL, so this has to be a loose expression
 * args have to be compatible with formals, except
 * the last formal must be an OUT arg and it must be a scalar type
 * that out arg will be treated as the return value of the "function"
 * in code-gen we will create a temporary for it; semantic analysis doesn't care

#### Root Expressions
A top level expression defines the context for that evaluation.  Different expressions
can have constraints.  e.g. aggregate functions may not appear in the `WHERE` clause of a statement.  There are cases where expression nesting can happen. This nesting changes the evaluation context accordingly, e.g. you can put a nested select in a where clause and that
nested select could legally have aggregates.  Root expressions keep a stack of nested contexts to facilitate the changes.

#### Table Factors
A table factor is one of three things:
* a table name (a string)  select * from X
* a select subquery (select X,Y from..) as T2
* a list of table references select * from (X, Y, Z)
Here we dispatch to the appropriate helper for each case.

#### Joining with the `USING` Clause
When specifying joins, one of the alternatives is to give the shared
columns in the join e.g. select * from X inner join Y using (a,b).
This method validates that all the columns are present on both sides of the
join, that they are unique, and they are comparable.
The return code tells us if any columns had SENSITIVE data.   See Special Note on JOIN...USING below

#### JOIN WITH THE `ON` Clause
The most explicit join condition is a full expression in an ON clause
this is like `select a,b from X inner join Y on X.id = Y.id;`
The on expression should be something that can be used as a bool,
so any numeric will do.
The return code tells us if the ON condition used SENSITIVE data.

#### TABLE VALUED FUNCTIONS
Table valued functions can appear anywhere a table is allowed.
The validation rules are:
* must be a valid function
* must return a struct type (i.e. a table-valued-function)
* must have valid arg expressions
* arg expressions must match formal parameters
The name of the resulting table is the name of the function
 * but it can be aliased later with "AS"

 ### Special Note on the `select *` and `select T.*` forms

 The `select *` construct is very popular in many codebases but it can be unsafe to use in production code because, if the schema changes, the code might get columns it does not expect.  Note the extra columns could have appeared anywhere in the result set because the `*` applies to the entire result of the `FROM` clause, joins and all, so extra columns are not necessarily at the end and column ordinals are not preserved.  CQL mitigates this situation somewhat with some useful constraints/features:

* in a `select *`, and indeed in any query, the column names of the select must be unique, this is because:
   * they could form the field names of an automatically generated cursor (see the section on cursors)
   * they could form the field names in a CQL result set (see section on result sets)
   * it's weird/confusing to not have unique names generally
* when issuing a `select *` or a `select T.*` CQL will automatically expand the `*` into the actual logical columns that exist in the schema at the time the code was compiled
   * this is important because if a column had been logically deleted from a table it would be unexpected in the result set even though it is still present in the database and would throw everything off
   * likewise if the schema were to change without updating the code, the code will still get the columns it was compiled with, not new columns

Expanding the `*` at compile time means Sqlite cannot see anything that might tempt it to include different columns in the result.

With this done we just have to look at the places a `select *` might appear so we can see if it is safe (or at least reasonably safe) to use `*` and, by extension of the same argument, `T.*`.

*In an `EXISTS` or `NOT EXISTS` clause like `where not exists (select * from x)`*

* this is perfectly safe; the particular columns do not matter; `select *` is not even expanded in this case.

*In a statement that produces a result set like `select * from table_or_view`*

* binding to a CQL result set is done by column name and we know those names are unique
* we won't include any columns that are logically deleted, so if you try to use a deleted column you'll get a compile time error

In a cursor statement like `declare C cursor for select * from table_or_view` there are two cases here:

*Automatic Fetch  `fetch C;`*

* in this case you don't specify the column names yourself;2 they are inferred
* you are therefore binding to the columns by name, so new columns in the cursor would be unused (until you choose to start using them)
* if you try to access a deleted column you get a compile-time error

*Manual Fetch:  `fetch C into a, b, c;`*

* In this case the number and type of the columns must match exactly with the specified variables
* If new columns are added, deleted, or changed, the above code will not compile

So considering the cases above we can conclude that auto expanding the `*` into the exact columns present in the compile-time schema version ensures that any incompatible changes result in compile time errors. Adding columns to tables does not cause problems even if the code is not recompiled. This makes the `*` construct much safer, if not perfect, but no semantic would be safe from arbitrary schema changes without recompilation.  At the very least here we can expect a meaningful runtime error rather than silently fetching the wrong columns.

### Special Note on the JOIN...USING form

CQL varies slightly from SQLite in terms of the expected results for joins if the USING syntax is employed.  This is not the most common syntax (typically an ON clause is used) but Sqlite has special rules for this kind of join.

Let's take a quick look.  First some sample data:

```
create table A( id integer, a text, b text);
create table B( id integer, c text, d text);

insert into A values(1, 'a1', 'b1');
insert into B values(1, 'c1', 'd1');
insert into A values(2, 'a2', 'b2');
insert into B values(2, 'c2', 'd2');
```

Now let's look at the normal join; this is our reference:
```
select * from A T1 inner join B T2 on T1.id = T2.id;

result:

1|a1|b1|1|c1|d1
2|a2|b2|2|c2|d2
```
As expected, you get all the columns of A, and all the columns of B.  The 'id' column appears twice.


However, with the `USING` syntax:

```
select * T1 inner join B T2 using (id);

result:

1|a1|b1|c1|d1
2|a2|b2|c2|d2
```
The `id` column is now appearing exactly once.  However, the situation is not so simple as that.  It seems that what hapened was that the `*` expansion has not included two copies of the `id`.  The following cases show that both copies of `id` are still logically in the join.

```
select T1.*, 'xxx', T2.* from A T1 inner join B T2 using (id);

result:

1|a1|b1|xxx|1|c1|d1
2|a2|b2|xxx|2|c2|d2
```
The `T2.id` column is part of the join, it just wasn't part of the `*`


In fact, looking further:

```
select T1.id, T1.a, T1.b, 'xxx', T2.id, T2.c, T2.d from A T1 inner join B T2 using (id);

result:

1|a1|b1|xxx|1|c1|d1
2|a2|b2|xxx|2|c2|d2
```
There is no doubt, `T2.id` is a valid column and can be used in expressions freely. That means the column cannot be removed from the type calculus.

Now in CQL, the `*` and `T.*` forms are automatically expanded; SQLite doesn't see the `*`.  This is done so that if any columns have been logically deleted they can be elided from the result set.  Given that this happens, the `*` operator will expand to ALL the columns.  Just the same as if you did `T1.*` and `T2.*`.

*As a result, in CQL, there is no difference between  the `USING` form of a join and the `ON` form of a join.*

In fact, only the `select *` form could possibly be different, so in most cases this ends up being moot anyway.  Typically, you don't need to use `*` in the presence of joins because of name duplication and ambiguity of the column names of the result set.  CQL's automatic expansion means you have a much better idea exactly what columns you will get - those that were present in the schema you declared.


## Chapter 10: Schema Management Features
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL has a lot of schema knowledge already and so it's well positioned to think about schema upgrades and versioning.

It seemed essential to be able to record changes to the schema over time so CQL got an understanding of versioning.  This lets you do things like:

* ensure columns are only added where they should be
* generate compiler errors if you try to access columns that are deprecated
* move from one version to another tracking schema facets that have to be added

To use cql in this fashion, the sequence will be something like the below.  See Appendix 1 for command line details.

```
cql --in input.sql --rt schema_upgrade --cg schema_upgrader.sql \
                   --global_proc desired_upgrade_proc_name
```

### Annotations

There are three basic flavors of annotation

* `@create(version [, migration proc])`
* `@delete(version [, migration proc])`
* `@recreate`

They have various constraints:

* `@create` and `@delete` can only be applied to tables and columns
* `@recreate` can only be applied to tables (nothing else needs it anyway)
* `@recreate` cannot mix with `@create` or `@delete`
* `@recreate` can include a group name as in `@recreate(musketeers)`; if a group name is specified then all the tables in that group are recreated if any of them change

Indices, Views, and Triggers are always "recreated" (just like tables can be) and so neither the `@recreate` nor the `@create` annotations are needed (or allowed).  However when an Index, View, or Trigger is retired it must be marked with `@delete` so that it isn't totally forgotten but can be deleted anywhere it might still exist.  Note that when one of these items is deleted, the definition is not used as it will only be dropped anyway. The simplest creation of the object with the correct name will do the job as a tombstone.

e.g. `create view used_to_be_fabulous as select 1 x @delete(12);`  suffices to drop the `used_to_be_fabulous` view in version 12 no matter how complicated it used to be.  Its `CREATE VIEW` will not be emitted into the upgrade procedure in any case.  Similarly, trivial indices and triggers of the correct name can be used for the tombstone.

In addition, if there is some data migration that needs to happen at a particular schema version that isn't associated with any particular change in schema, you can run an *ad hoc* migrator at any time.  The syntax for that is `@schema_ad_hoc_migration(version, migration proc);`.  Ad hoc migrations are the last to run in any given schema version; they happen after table drop migrations.

### Semantics

`@create` declares that the annotated object first appeared in the indicated version, and at that time the migration proc needs to be executed to fill in default values, denormalize values, or whatever the case may be.

`@delete` declares that the annotated object disappeared in the indicated version, and at that time the migration proc needs to be executed to clean up the contents, or potentially move them elsewhere.

`@recreate` declares that the annotated object can be dropped and recreated when it changes because there is no need to preserve its contents during an upgrade. Such objects may be changed arbitrarily from version to version.


* no columns in a `@recreate` table may have `@create` or `@delete` (these aren't needed anyway)
   * therefore tables with `@recreate` never have deprecated columns (since `@delete` isn't allowed on their columns)

NOTE: all annotations are suppressed from generated SQL.  SQLite never sees them.

NOTE: looking at the annotations it is possible to compute the logical schema at any version, especially the original schema -- it's what you get if you disregard all ```@delete``` entirely (don't delete) and then remove anything marked with ```@create``` directives.

### Allowable changes
Not all migrations are possible in a sensible fashion, therefore CQL enforces certain limitations:

* the "original" schema has no annotations or just delete annotations
* new tables may be added (with ```@create```)
* tables may be deleted (with ```@delete```)
* columns may be added to a table, but only at the end of the table
* added columns must be nullable or have a default value (otherwise all existing insert statements would break for sure)
* columns may not be renamed
* columns may be deleted but this is only a logical delete, SQLite has no primitive to remove columns; once deleted you may no longer refer to that column in queries
* deleted columns must be nullable or have a default value (otherwise all existing and future insert statements would break for sure, the column isn't really gone)
* views, indices, and triggers may be added (no annotation required) and removed (with `@delete`) like tables
* views, indices, and triggers may be altered completely from version to version
* no normal code is allowed to refer to deleted columns, tables, etc.  This includes views, indices, and triggers
* schema migration stored procs see the schema as it existed in their annotation (so an older version). They are also forbidden from using views (see below)
* recreated objects (tables marked with @recreate, views, tables, and indices) have no change restrictions


### Prosecution

Moving from one schema version to another is done in an orderly fashion with the migration proc taking these essential steps in this order:

* the ```cql_schema_facets``` table is created if needed -- this records the current state of the schema
* the last known schema hash is read from the ```cql_schema_facets``` tables (it is zero by default)
* if the overall schema hash code matches what is stored, processing stops; otherwise an upgrade ensues
* all known views are dropped (hence migration procs won't see them!)
* any index that needs to change is dropped (this includes items marked ```@delete``` or indices that are different than before)
  * change is detected by hash (crc64) of the previous index definition vs. the current
* all known triggers are dropped (hence they will not fire during migration!)
* the current schema version is extracted from ```cql_schema_facets``` (it is zero by default)
* if the current schema version is zero, then the original versions of all the tables are created

* if the current schema version is <= 1 then
  * any tables that need to be created at schema version 1 are created as they exist at schema version 1
  * any columns that need to be created at schema version 1 are created as they exist at schema version 1
  * migration procedures schema version 1 are run in this order:
    * create table migration
    * create column migration
    * delete trigger migration (these are super rare and supported for uniformity)
    * delete index migration (these are super rare and supported for uniformity)
    * delete view migration  (these are super rare and supported for uniformity)
    * delete column migration
    * delete table migration
    * ad hoc migration
    * each proc is run exactly one time
  * any tables that need to be dropped at schema version 1 are dropped
  * the schema version is marked as 1 in ```cql_schema_facets```
  * each sub-step in the above is recorded in ```cql_schema_facets``` as it happens so it is not repeated
    * all that checking not shown for brevity

* the above process is repeated for all schema versions up to the current version
* all tables that are marked with `@recreate` are re-created if necessary
  * i.e. if the checksum of the table definition has changed for any  table (or group) then `drop` it and create the new version.
* all indices that changed and were not marked with `@delete` are re-created
* all views not marked with `@delete` are re-created
* all triggers not marked with `@delete` are re-installed
* the current schema hash is written to the ```cql_schema_facets``` table

### Example Migration

Here's an example of a schema directly from the test cases:

```
-- crazy amount of versioning here
create table foo(
  id integer not null,
  rate long integer @delete(5),
  rate_2 long integer @delete(4, DeleteRate2Proc),
  id2 integer default 12345 @create(4, CreateId2Proc),
  name text @create(5),
  name_2 text @create(6)
);

-- much simpler table, lots of stuff added in v2.
-- note v1 is the first new version and v0 is base version
create table table2(
  id integer not null,
  name1 text @create(2, CreateName1Proc),
  name2 text @create(2, CreateName2Proc),
  name3 text @create(2), -- no proc
  name4 text @create(2) -- no proc
);

create table added_table(
  id integer not null,
  name1 text,
  name2 text @create(4)
) @create(3) @delete(5);

-- this view is present in the output
create view live_view as select * from foo;

-- this view is also present in the output
create view another_live_view as select * from foo;

-- this view is not present in the output
create view dead_view as select * from foo @delete(2);

-- this index is present
create index index_still_present on table2(name1, name2);

-- this index is going away
create index index_going_away on table2(name3) @delete(3);

-- this is a simple trigger, and it's a bit silly but that doesn't matter
create trigger trigger_one
  after insert on foo
begin
  delete from table2 where table2.id = new.id;
end;
```

This schema has a LOT of versioning... you can see tables and columns appearing in versions 2 through 6.  There is a lot of error checking happening.

* things with no create annotation were present in the base schema
* only things with no delete annotation are visible to normal code
* created columns have to be at the end of their table (required by SQLite)
* they have to be in ascending schema version order (but you can add several columns in one version)
* there may or may not be a proc to run to populate data in that column when it's added or to remove data when it's deleted
   * proc names must be unique
* you can't delete a table or column in a version before it was created
* you can't delete a column in a table in a version before the table was created
* you can't create a column in a table in a version after the table was deleted
* there may be additional checks not listed here

### Sample Upgrade Script
With just those annotations you can automatically create the following upgrade script which is itself CQL (and hence has to be compiled). Notice that this code is totally readable!

The script has been split into logical pieces to make it easier to explain what's going on.

#### Preamble

```sql
-- ...copyright notice... possibly generated source tag... elided to avoid confusion

-- no columns will be considered hidden in this script
-- DDL in procs will not count as declarations
@SCHEMA_UPGRADE_SCRIPT;
```

Schema upgrade scripts need to see all the columns even the ones that would be logically deleted in normal mode.  This is so that things like `alter table add column` can refer to real columns and `drop table` can refer to a table that shouldn't even be visible.  Remember in CQL the declarations tell you the logical state of the universe and DLL mutations are expected to create that condition, so you should be dropping tables that are marked with `@delete`
CQL stores the current state of the universe in this table.

```sql
-- schema crc -7714030317354747478
```
The schema crc is computed by hashing all the schema declarations in canonical form.  That's everything in this next section.

#### Facet Helpers

CQL uses a set of four functions to manage a dictionary.  The implementation is in `cqlrt_common.c` but it's really
just a simple hash table that maps from a string key to a number.  This functionality was added because over time
the facets table can get pretty big and running a SQL query every time to read a single integer is not economical.

```sql
-- declare facet helpers--
DECLARE facet_data TYPE LONG<facet_data> not null;
DECLARE test_facets facet_data;
DECLARE FUNCTION cql_facets_new() facet_data;
DECLARE PROCEDURE cql_facets_delete(facets facet_data);
DECLARE FUNCTION cql_facet_add(facets facet_data, facet TEXT NOT NULL, crc LONG NOT NULL) BOOL NOT NULL;
DECLARE FUNCTION cql_facet_find(facets facet_data, facet TEXT NOT NULL) LONG NOT NULL;
```

#### Declaration Section
Wherein all the necessary objects are declared...

```sql
-- declare sqlite_master --
CREATE TABLE sqlite_master (
  type TEXT NOT NULL,
  name TEXT NOT NULL,
  tbl_name TEXT NOT NULL,
  rootpage INTEGER NOT NULL,
  sql TEXT NOT NULL
);
```
The `sqlite_master` table is built-in but it has to be introduced to CQL so that we can query it. Like all the other loose DDL declarations here there is no code generated for this.  We are simply declaring tables.  To create code you have to put the DDL in a proc.  Normally DDL in procs also declares the table but since we may need the original version of a table created and the final version declared we have `@schema_upgrade_script` to help avoid name conflicts.

```sql
-- declare full schema of tables and views to be upgraded --
CREATE TABLE foo(
  id INTEGER NOT NULL,
  rate LONG INT @DELETE(5),
  rate_2 LONG INT @DELETE(4, DeleteRate2Proc),
  id2 INTEGER DEFAULT 12345 @CREATE(4, CreateId2Proc),
  name TEXT @CREATE(5),
  name_2 TEXT @CREATE(6)
);

CREATE TABLE table2(
  id INTEGER NOT NULL,
  name1 TEXT @CREATE(2, CreateName1Proc),
  name2 TEXT @CREATE(2, CreateName2Proc),
  name3 TEXT @CREATE(2),
  name4 TEXT @CREATE(2)
);

CREATE TABLE added_table(
  id INTEGER NOT NULL,
  name1 TEXT,
  name2 TEXT @CREATE(4)
) @CREATE(3) @DELETE(5);
```

NOTE: all the tables are emitted including all the annotations.  This lets us do the maximum validation when we compile this script.

```sql
CREATE VIEW live_view AS
SELECT *
  FROM foo;

CREATE VIEW another_live_view AS
SELECT *
  FROM foo;

CREATE VIEW dead_view AS
SELECT *
  FROM foo @DELETE(2);
```
These view declarations do very little.  We only need the view names so we can legally drop the views.  We create the views elsewhere.

```sql
CREATE INDEX index_still_present ON table2 (name1, name2);
CREATE INDEX index_going_away ON table2 (name3) @DELETE(3);
```
Just like views, these declarations introduce the index names and nothing else.

```sql
CREATE TRIGGER trigger_one
  AFTER INSERT ON foo
BEGIN
DELETE FROM table2 WHERE table2.id = new.id;
END;
```
We have only the one trigger; we declare it here.

```sql
-- facets table declaration --
CREATE TABLE IF NOT EXISTS test_cql_schema_facets(
  facet TEXT NOT NULL PRIMARY KEY,
  version LONG INTEGER NOT NULL
);
```
This is where we will store everything we know about the current state of the schema.  Below we define a few helper procs for reading and writing that table and reading `sqlite_master`

```sql
-- saved facets table declaration --
CREATE TEMP TABLE test_cql_schema_facets_saved(
  facet TEXT NOT NULL PRIMARY KEY,
  version LONG INTEGER NOT NULL
);
```
We will snapshot the facets table at the start of the run so that we can produce a summary of the changes
at the end of the run.  This table will hold that snapshot.

NOTE: the prefix "test" was specified when this file was built so all the methods and tables begin with `test_`.


#### Helper Procedures
```sql
-- helper proc for testing for the presence of a column/type
CREATE PROCEDURE test_check_column_exists(table_name TEXT NOT NULL,
                                          decl TEXT NOT NULL,
                                          OUT present BOOL NOT NULL)
BEGIN
  SET present := (SELECT EXISTS(SELECT * FROM sqlite_master
                  WHERE tbl_name = table_name AND sql GLOB decl));
END;
```
`check_column_exists` inspects `sqlite_master` and returns true if a column matching `decl` exists.


```sql
-- helper proc for creating the schema version table
CREATE PROCEDURE test_create_cql_schema_facets_if_needed()
BEGIN
  CREATE TABLE IF NOT EXISTS test_cql_schema_facets(
    facet TEXT NOT NULL PRIMARY KEY,
    version LONG INTEGER NOT NULL
  );
END;
```
Here we actually create the `cql_schema_facets` table with DDL inside a proc.  In a non-schema-upgrade script the above would give a name conflict.

```sql
-- helper proc for saving the schema version table
CREATE PROCEDURE test_save_cql_schema_facets()
BEGIN
  DROP TABLE IF EXISTS test_cql_schema_facets_saved;
  CREATE TEMP TABLE test_cql_schema_facets_saved(
    facet TEXT NOT NULL PRIMARY KEY,
    version LONG INTEGER NOT NULL
  );
  INSERT INTO test_cql_schema_facets_saved
    SELECT * FROM test_cql_schema_facets;
END;
```

The `save_sql_schema_facets` procedure simply makes a snapshot of the current facets table.  Later we use
this snapshot to report the differences by joining these tables.

```sql
-- helper proc for setting the schema version of a facet
CREATE PROCEDURE test_cql_set_facet_version(_facet TEXT NOT NULL,
                                            _version LONG INTEGER NOT NULL)
BEGIN
  INSERT OR REPLACE INTO test_cql_schema_facets (facet, version)
       VALUES(_facet, _version);
END;

-- helper proc for getting the schema version of a facet
CREATE PROCEDURE test_cql_get_facet_version(_facet TEXT NOT NULL,
                                            out _version LONG INTEGER NOT NULL)
BEGIN
  BEGIN TRY
    SET _version := (SELECT version FROM test_cql_schema_facets
                       WHERE facet = _facet LIMIT 1 IF NOTHING -1);
  END TRY;
  BEGIN CATCH
    SET _version := -1;
  END CATCH;
END;
```
The two procedures `cql_get_facet_version` and `cql_set_facet_version` do just what you would expect.  Note the use of `try` and `catch` to return a default value if the select fails.

There are two additional helper procedures that do essentially the same thing using a schema version index.  These two methods exist only to avoid unnecessary repeated string literals in the output file which cause bloat.

```sql
-- helper proc for getting the schema version CRC for a version index
CREATE PROCEDURE test_cql_get_version_crc(_v INTEGER NOT NULL, out _crc LONG INTEGER NOT NULL)
BEGIN
  SET _crc := cql_facet_find(test_facets, printf('cql_schema_v%d', _v));
END;

-- helper proc for setting the schema version CRC for a version index
CREATE PROCEDURE test_cql_set_version_crc(_v INTEGER NOT NULL,
                                          _crc LONG INTEGER NOT NULL)
BEGIN
  INSERT OR REPLACE INTO test_cql_schema_facets (facet, version)
       VALUES('cql_schema_v'||_v, _crc);
END;
```
As you can see, these procedures are effectively specializations of `cql_get_facet_version` and `cql_set_facet_version` where the facet name is computed from the integer.


Triggers require some special processing.  There are so-called "legacy" triggers that crept into the system.  These
begin with `tr__` and they do not have proper tombstones.  In fact some are from early versions of CQL before
they were properly tracked.  To fix any old databases that have these in them, we delete all triggers that start with `tr__`.
Note we have to use the `GLOB` operator to do this, because `_` is the `LIKE` wildcard.

```sql
-- helper proc to reset any triggers that are on the old plan --
DECLARE PROCEDURE cql_exec_internal(sql TEXT NOT NULL) USING TRANSACTION;
CREATE PROCEDURE test_cql_drop_legacy_triggers()
BEGIN
  DECLARE C CURSOR FOR SELECT name from sqlite_master
     WHERE type = 'trigger' AND name GLOB 'tr__*';
  LOOP FETCH C
  BEGIN
    call cql_exec_internal(printf('DROP TRIGGER %s;', C.name));
  END;
END;
```

#### Baseline Schema
The 'baseline' or 'v0' schema is unannotated (no `@create` or `@recreate`).    The first real schema
management procedures are for creating and dropping these tables.

```sql
CREATE PROCEDURE test_cql_install_baseline_schema()
BEGIN
  CREATE TABLE foo(
    id INTEGER NOT NULL,
    rate LONG_INT,
    rate_2 LONG_INT
  );

  CREATE TABLE table2(
    id INTEGER NOT NULL
  );

END;
```

```sql
-- helper proc for dropping baseline tables before installing the baseline schema
CREATE PROCEDURE test_cql_drop_baseline_tables()
BEGIN
  DROP TABLE IF EXISTS foo;
  DROP TABLE IF EXISTS table2;
END;
```

#### Migration Procedures

The next section declares the migration procedures that were in the schema.  These are expected to be
defined elsewhere.

```sql
-- declared upgrade procedures if any
DECLARE proc CreateName1Proc() USING TRANSACTION;
DECLARE proc CreateName2Proc() USING TRANSACTION;
DECLARE proc CreateId2Proc() USING TRANSACTION;
DECLARE proc DeleteRate2Proc() USING TRANSACTION;
```
The code below will refer to these migration procedures.  We emit a declaration so that we can use the names in context.
NOTE: `USING TRANSACTION` when applied to a proc declaration simply means the proc will access the database so it needs to be provided with a `sqlite3 *db` parameter.


#### Views
```sql
-- drop all the views we know
CREATE PROCEDURE test_cql_drop_all_views()
BEGIN
  DROP VIEW IF EXISTS live_view;
  DROP VIEW IF EXISTS another_live_view;
  DROP VIEW IF EXISTS dead_view;
END;

-- create all the views we know
CREATE PROCEDURE test_cql_create_all_views()
BEGIN
  CREATE VIEW live_view AS
  SELECT *
    FROM foo;
  CREATE VIEW another_live_view AS
  SELECT *
    FROM foo;
END;
```
View migration is done by dropping all views and putting all views back.

NOTE: `dead_view` was not created, but we did try to drop it if it existed.

#### Indices

```sql
-- drop all the indices that are deleted or changing
CREATE PROCEDURE test_cql_drop_all_indices()
BEGIN
  IF cql_facet_find(test_facets, 'index_still_present_index_crc') != -6823087563145941851 THEN
    DROP INDEX IF EXISTS index_still_present;
  END IF;
  DROP INDEX IF EXISTS index_going_away;
END;

-- create all the indices we need
CREATE PROCEDURE test_cql_create_indices()
BEGIN
  IF cql_facet_find(test_facets, 'index_still_present_index_crc') != -6823087563145941851 THEN
    CREATE INDEX index_still_present ON table2 (name1, name2);
    CALL test_cql_set_facet_version('index_still_present_index_crc', -6823087563145941851);
  END IF;
END;

```
Indices are processed similarly to views, however we do not want to drop indices that are not changing.  Therefore we compute the CRC of the index definition.  At the start of the script any indices that are condemned (e.g. `index_going_away`) are dropped as well as any that have a new CRC. At the end of migration, changed or new indices are (re)created using `cql_create_indices`.

#### Triggers
```sql
- drop all the triggers we know
CREATE PROCEDURE test_cql_drop_all_triggers()
BEGIN
  CALL test_cql_drop_legacy_triggers();
  DROP TRIGGER IF EXISTS trigger_one;
END;

-- create all the triggers we know
CREATE PROCEDURE test_cql_create_all_triggers()
BEGIN
  CREATE TRIGGER trigger_one
    AFTER INSERT ON foo
  BEGIN
  DELETE FROM table2 WHERE table2.id = new.id;
  END;
END;
```

Triggers are always dropped before migration begins and are re-instated quite late in the processing
as we will see below.

#### Caching the state of the facets

To avoid selecting single rows out of the facets table repeatedly we introduce this procedure
whose job is to harvest the facets table and store it in a dictionary.  The helpers that do this
were declared above.  You've already seen usage of the facets in the
code above.

```sql
CREATE PROCEDURE test_setup_facets()
BEGIN
  BEGIN TRY
    SET test_facets := cql_facets_new();
    DECLARE C CURSOR FOR SELECT * from test_cql_schema_facets;
    LOOP FETCH C
    BEGIN
      LET added := cql_facet_add(test_facets, C.facet, C.version);
    END;
  END TRY;
  BEGIN CATCH
   -- if table doesn't exist we just have empty facets, that's ok
  END CATCH;
END;
```

#### Main Migration Script

The main script orchestrates everything.  There are inline comments for all of it.  The general order of events is:

* create schema facets table if needed
* check main schema crc; if it matches we're done here, otherwise continue...

These operations are done in `test_perform_needed_upgrades`
* drop all views
* drop condemned indices
* fetch the current schema version
* if version 0 then install the baseline schema (see below)
* for each schema version with changes do the following:
  * create any tables that need to be created in this version
  * add any columns that need to be added in this version
  * run migration procs in this order:
    * create table
    * create column
    * delete trigger
    * delete view
    * delete index
    * delete column
    * delete table
  * drop any tables that need to be dropped in this version
  * mark schema upgraded to the current version so far, and proceed to the next version
  * each partial step is also marked as completed so that it can be skipped if the script is run again
* create all the views
* (re)create any indices that changed and are not dead
* set the schema CRC to the current CRC

That's it... the details are below.

```sql
CREATE PROCEDURE test_perform_upgrade_steps()
BEGIN
  DECLARE column_exists BOOL NOT NULL;
  DECLARE schema_version LONG INTEGER NOT NULL;
    -- dropping all views --
    CALL test_cql_drop_all_views();

    -- dropping condemned or changing indices --
    CALL test_cql_drop_all_indices();

    -- dropping condemned or changing triggers --
    CALL test_cql_drop_all_triggers();

    ---- install baseline schema if needed ----

    CALL test_cql_get_version_crc(0, schema_version);
    IF schema_version != -9177754326374570163 THEN
      CALL test_cql_install_baseline_schema();
      CALL test_cql_set_version_crc(0, -9177754326374570163);
    END IF;

    ---- upgrade to schema version 2 ----

    CALL test_cql_get_version_crc(2, schema_version);
    IF schema_version != -6840158498294659234 THEN
      -- altering table table2 to add column name1 TEXT;

      CALL test_check_column_exists('table2', '*[( ]name1 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE table2 ADD COLUMN name1 TEXT;
      END IF;

      -- altering table table2 to add column name2 TEXT;

      CALL test_check_column_exists('table2', '*[( ]name2 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE table2 ADD COLUMN name2 TEXT;
      END IF;

      -- altering table table2 to add column name3 TEXT;

      CALL test_check_column_exists('table2', '*[( ]name3 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE table2 ADD COLUMN name3 TEXT;
      END IF;

      -- altering table table2 to add column name4 TEXT;

      CALL test_check_column_exists('table2', '*[( ]name4 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE table2 ADD COLUMN name4 TEXT;
      END IF;

      -- data migration procedures
      IF cql_facet_find(test_facets, 'CreateName1Proc') = -1 THEN
        CALL CreateName1Proc();
        CALL test_cql_set_facet_version('CreateName1Proc', 2);
      END IF;
      IF cql_facet_find(test_facets, 'CreateName2Proc') = -1 THEN
        CALL CreateName2Proc();
        CALL test_cql_set_facet_version('CreateName2Proc', 2);
      END IF;

      CALL test_cql_set_version_crc(2, -6840158498294659234);
    END IF;

    ---- upgrade to schema version 3 ----

    CALL test_cql_get_version_crc(3, schema_version);
    IF schema_version != -4851321700834943637 THEN
      -- creating table added_table

      CREATE TABLE IF NOT EXISTS added_table(
        id INTEGER NOT NULL,
        name1 TEXT
      );

      CALL test_cql_set_version_crc(3, -4851321700834943637);
    END IF;

    ---- upgrade to schema version 4 ----

    CALL test_cql_get_version_crc(4, schema_version);
    IF schema_version != -6096284368832554520 THEN
      -- altering table added_table to add column name2 TEXT;

      CALL test_check_column_exists('added_table', '*[( ]name2 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE added_table ADD COLUMN name2 TEXT;
      END IF;

      -- altering table foo to add column id2 INTEGER;

      CALL test_check_column_exists('foo', '*[( ]id2 INTEGER*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE foo ADD COLUMN id2 INTEGER DEFAULT 12345;
      END IF;

      -- logical delete of column rate_2 from foo; -- no ddl

      -- data migration procedures
      IF cql_facet_find(test_facets, 'CreateId2Proc') = -1 THEN
        CALL CreateId2Proc();
        CALL test_cql_set_facet_version('CreateId2Proc', 4);
      END IF;
      IF cql_facet_find(test_facets, 'DeleteRate2Proc') = -1 THEN
        CALL DeleteRate2Proc();
        CALL test_cql_set_facet_version('DeleteRate2Proc', 4);
      END IF;

      CALL test_cql_set_version_crc(4, -6096284368832554520);
    END IF;

    ---- upgrade to schema version 5 ----

    CALL test_cql_get_version_crc(5, schema_version);
    IF schema_version != 5720357430811880771 THEN
      -- altering table foo to add column name TEXT;

      CALL test_check_column_exists('foo', '*[( ]name TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE foo ADD COLUMN name TEXT;
      END IF;

      -- logical delete of column rate from foo; -- no ddl

      -- dropping table added_table

      DROP TABLE IF EXISTS added_table;

      CALL test_cql_set_version_crc(5, 5720357430811880771);
    END IF;

    ---- upgrade to schema version 6 ----

    CALL test_cql_get_version_crc(6, schema_version);
    IF schema_version != 3572608284749506390 THEN
      -- altering table foo to add column name_2 TEXT;

      CALL test_check_column_exists('foo', '*[( ]name_2 TEXT*', column_exists);
      IF NOT column_exists THEN
        ALTER TABLE foo ADD COLUMN name_2 TEXT;
      END IF;

      CALL test_cql_set_version_crc(6, 3572608284749506390);
    END IF;

    CALL test_cql_create_all_views();
    CALL test_cql_create_all_indices();
    CALL test_cql_create_all_triggers();
    CALL test_cql_set_facet_version('cql_schema_version', 6);
    CALL test_cql_set_facet_version('cql_schema_crc', -7714030317354747478);
END;
```

We have one more helper that will look for evidence that we're trying
to move backwards to a previous schema version.  This is not supported.
This procedure also arranges for the original facet versions to be saved
and it proceduces a difference in facets after the upgrade is done.

```sql
CREATE PROCEDURE test_perform_needed_upgrades()
BEGIN
  -- check for downgrade --
  IF cql_facet_find(test_facets, 'cql_schema_version') > 6 THEN
    SELECT 'downgrade detected' facet;
  ELSE
    -- save the current facets so we can diff them later --
    CALL test_save_cql_schema_facets();
    CALL test_perform_upgrade_steps();

    -- finally produce the list of differences
    SELECT T1.facet FROM
      test_cql_schema_facets T1
      LEFT OUTER JOIN test_cql_schema_facets_saved T2
        ON T1.facet = T2.facet
      WHERE T1.version is not T2.version;
  END IF;
END;
```

This is the main function for upgrades, it checks only the master schema version.
This function is separate so that the normal startup path doesn't have to have
the code for the full upgrade case in it.  This lets linker order files do a superior job
(since full upgrade is the rare case).

```sql
CREATE PROCEDURE test()
BEGIN
  DECLARE schema_crc LONG INTEGER NOT NULL;

  -- create schema facets information table --
  CALL test_create_cql_schema_facets_if_needed();

  -- fetch the last known schema crc, if it's different do the upgrade --
  CALL test_cql_get_facet_version('cql_schema_crc', schema_crc);

  IF schema_crc <> -7714030317354747478 THEN
    BEGIN TRY
      CALL test_setup_facets();
      CALL test_perform_needed_upgrades();
    END TRY;
    BEGIN CATCH
      CALL cql_facets_delete(test_facets);
      SET test_facets := 0;
      THROW;
    END CATCH;
    CALL cql_facets_delete(test_facets);
    SET test_facets := 0;
  ELSE
    -- some canonical result for no differences --
    SELECT 'no differences' facet;
  END IF;
END;
```

#### Temp Tables
We had no temporary tables in this schema, but if there were some they get added
to the schema after the upgrade check.

A procedure like this one is generated:

```sql
CREATE PROCEDURE test_cql_install_temp_schema()
BEGIN
  CREATE TEMP TABLE tempy(
    id INTEGER
  );
END;
```

This entry point can be used any time you need the temp tables.  But normally it is
automatically invoked.

```sql
  ---- install temp schema after upgrade is complete ----
  CALL test_cql_install_temp_schema();
```

That logic is emitted at the end of the test procedure.

### Schema Regions
Schema Regions are designed to let you declare your schema in logical regions whose dependencies are specified.  It enforces the dependencies you specify creating errors if you attempt to break the declared rules.  Schema regions allow you to generate upgrade scripts for parts of your schema that can compose and be guaranteed to remain self-consistent.

#### Details

In many cases schema can be factored into logical and independent islands.  This is desireable for a number of reasons:

* so that the schema can go into different databases
* so that the schema can be upgraded on a different schedule
* so that "not relevant" schema can be omitted from distributions
* so that parts of your schema that have no business knowing about each other can be prevented from taking dependencies on each other

These all have very real applications:

##### E.g. Your Application has an on-disk and an in-memory database

This creates basically three schema regions:

1. on disk: which cannot refer to the in-memory at all
2. in-memory: which cannot refer to the on-disk schema at all
3. cross-db: which refers to both, also in memory (optional)

##### Your Application Needs To Upgrade Each of the Above

There must be a separate upgrade script for both the island databases and yet a different one for the "cross-db" database

##### Your Customer Doesn't Want The Kitchen Sink of Schema

If you're making a library with database support, your customers likely want to be able to create databases that have only features they want; you will want logical parts within your schema that can be separated for cleanliness and distribution.

#### Declaring Regions and Dependencies

Schema Regions let you create logical groupings, you simply declare the regions you want and then start putting things into those regions.  The regions form a directed acyclic graph -- just like C++ base classes.  You create regions like this:

```sql
@declare_schema_region root;

@declare_schema_region extra using root;
```

The above simply declares the regions -- it doesn't put anything into them.  In this case we now have a `root` region and an `extra` region.  The `root` schema items will not be allowed to refer to anything in `extra`.

Without regions, you could also ensure that the above is true by putting all the `extra` items afer the `root` in the input file but things can get more complicated than that in general, and the schema might also be in several files, complicating ordering as the option.  Also, relying on order could be problematic as it is quite easy to put things in the wrong place (e.g. add a new `root` item after the `extra` items).  Making this a bit more complicated, we could have:


```sql
@declare_schema_region feature1 using extra;
@declare_schema_region feature2 using extra;
@declare_schema_region everything using feature1, feature2;
```

And now there are many paths to `root` from the `everything` region;  that's ok but certainly it will be tricky to do all that with ordering.

#### Using Regions

An illustrative example, using the regions defined above:

```sql
@begin_schema_region root;

create table main(
  id integer,
  name text
);

create view names as select name from main order by name;

@end_schema_region;

@begin_schema_region extra;

create table details(
   id integer references main(id),
   details text
);

create proc get_detail(id_ integer)
begin
  select T1.id, T1.details, T2.name from details T1
  inner join main T2 on T1.id = T2.id
  where T1.id = id_;
end;

@end_schema_region;

@begin_schema_region feature1;

create table f1(
   id integer references details(id),
   f1_info text
);

create proc get_detail(id_ integer)
begin
  select T1.id, T1.details, T2.name, f1_info from details T1
  inner join f T2 on T1.id = T2.id
  inner join f1 on f1.id = T1.id
  where T1.id = id_;
end;

@end_schema_region;

@begin_schema_region feature2;
  -- you can use details, and main but not f1
@end_schema_region;
```

With the structure above specified, even if a new contribution to the `root` schema appears later, the rules enforce that this region cannot refer to anything other than things in `root`.  This can be very important if schema is being included via `#include` and might get pulled into the compilation in various orders.  A feature area might also have a named public region that others things can depend on (e.g. some views) and private regions (e.g. some tables, or whatever).

#### Region Visibility

Schema regions do not provide additional name spaces -- the names of objects should be unique across all regions. In other words, regions do not hide or scope entity names; rather they create errors if inappropriate names are used.

Case 1: The second line will fail semantic validation because table `A` already exists
```sql
-- obvious standard name conflict
create table A (id integer);
create table A (id integer, name text);
```

Case 2: This fails for the same reason as case #1. Table `A` already exists
```sql
@declare_region root;
-- table A is in no region
create table A (id integer);
@begin_region root:
-- this table A is in the root region, still an error
create table A (id integer, name text);
@end_region;
```

Case 3: Again fails for the same reason as case #1. Table `A` already exist in region `extra`, and you cannot define another table with the same name in another region.
```sql
@declare_region root;
@declare_region extra;

@begin_region extra;
-- so far so good
create table A (id integer);
@end_region;

@begin_region root;
-- no joy, this A conflicts with the previous A
create table A (id integer, name text);
@end_region;
```

Really the visibility rules couldn't be anything other than the above, as SQLite has no knowledge of regions at all and so any exotic name resolution would just doom SQLite statements to fail when they finally run.

##### Exception for `"... LIKE <table>"` statement

The rules above are enforced for all constructs except for where the syntactic sugar `... LIKE <table>` forms, which can happen in a variety of statements. This form doesn't create a dependence on the table (but does create a dependence on its shape). When CQL generates output, the `LIKE` construct is replaced with the actual names of the columns it refers to.  But these are independent columns, so this is simply a keystroke saver.  The table (or view, cursor, etc.) reference will be gone.

These cases below will succeed.
```sql
@declare_region root;
create table A (...);
create view B (....);
create procedure C {...}

@begin_region root;
create table AA(LIKE A);
create table BB(LIKE B);
create table CC(LIKE C);
@end_region;
```

Note: this exception may end up causing maintenance problems and so it might be revisited in the future.

#### Maintaining Schema in Pieces

When creating upgrade scripts, using the `--rt schema_upgrade` flags you can add region options `--include_regions a b c` and `--exclude_regions d e f` per the following:

Included regions:
* must be valid region names -- the base types are walked to compute all the regions that are "in"
* declarations are emitted in the upgrade for all of the "in" objects -- "exclude" does not affect the declarations

Excluded regions:
* must be valid region names and indicate parts of schema that are upgraded elsewhere, perhaps with a seperate CQL run, a different automatic upgrade, or even a manual mechanism
* upgrade code will be generated for all the included schema, but not for the excluded regions and their contents


Example: Referring to the regions above you might do something like this

```bash

# All of these also need a --global_proc param for the entry point but that's not relevant here
cql --in schema.sql --cg shared.sql --rt schema_upgrade  --include_regions extra
cql --in schema.sql --cg f1.cql --rt schema_upgrade --include_regions feature1 --exclude_regions extra
cql --in schema.sql --cg f2.cql --rt schema_upgrade --include_regions feature2 --exclude_regions extra
```

The first command generates all the shared schema for regions `root` and `extra` because `extra` contains `root`

The second command declares all of `root` and `extra` so that the `feature1` things can refer to them, however the upgrade code for these shared regions is not emitted.  Only the upgrade for schema in `feature1` is emitted.  `feature2` is completely absent.  This will be ok because we know `feature1` cannot depend on `feature2` and `extra` is assumed to be upgraded elsewhere (such as in the previous line).

The third command declares all of `root` and `extra` so that the `feature2` things can refer to them, however the upgrade code for these shared regions is not emitted.  Only the upgrade for schema in `feature2` is emitted.  `feature1` is completely absent.

Note that in the above examples, CQL is generating more CQL to be compiled again (a common pattern).  The CQL upgrade scripts need to be compiled as usual to produce executable code.  Thus the output of this form includes the schema declarations and executable DDL.


##### Schema Not In Any Region

For schema that is not in any region you might imagine that it is a special region `<none>` that depends on everything.  So basically you can put anything there.  Schema that is in any region cannot ever refer to schema that is in `<none>`.

When upgrading, if any include regions are specified then `<none>` will not be emitted at all.  If you want an upgrader for just `<none>` this is possible with an assortment of exclusions.  You can always create arbitrary grouping regions to make this easier. A region named `any` that uses all other regions would make this simple.

In general, best practice is that there is no schema in `<none>`, but since most SQL code has no regions some sensible meaning has to be given to DDL before it gets region encodings.

#### Deployable Regions

Given the above we note that some schema regions correspond to the way that we will deploy the schema.  We want those bundles to be safe to deploy but to in order to be so we need a new notion -- a deployable region.  To make this possible CQL includes the following:

* You can declare a region as deployable using `@declare_deployable_region`
* CQL computes the covering of a deployable region: its transitive closure up to but not including any deployable regions it references
* No region is allowed to depend on a region that is within the interior of a different deployable region, but you can depend on the deployable region itself

Because of the above, each deployable region is in fact a well defined root for the regions it contains.  The deployable region becomes the canonical way in which a bundle of regions (and their content) is deployed and any given schema item can be in only one deployable region.

##### Motivation and Examples
As we saw above, regions are logical groupings of tables/views/etc such that if an entity is in some region `R` then it is allowed to only refer to the things that `R` declared as dependencies `D1`, `D2`, etc. and their transitive closures.  You can make as many logical regions as you like and you can make them as razor thin as you like; they have no physical reality but they let you make as many logical groups of things as you might want.

Additionally, when we’re deploying schema you generally need to do it in several pieces. E.g. if we have tables that go in an in-memory database then defining a region that holds all the in-memory tables makes it easy to, say, put all those in-memory tables into a particular deployment script.

Now we come to the reason for deployable regions. From CQL’s perspective, all regions are simply logical groups; some grouping is then meaningful to programmers but has no physical reality. This means you’re free to reorganize tables etc. as you see fit into new or different regions when things should move. Only, that’s not quite true. The fact that we deploy our schema in certain ways means while most logical moves are totally fine, if you were to move a table from, say, the main database region to the in-memory region you would be causing a major problem.  Some installations may already have the table in the main area and there would be nothing left in the schema to tell CQL to drop the table from the main database -- the best you can hope for is the new location gets a copy of the table the old location keeps it and now there are name conflicts forever.

So, the crux of the problem is this: We want to let you move schema freely between logical regions in whatever way makes sense to you, but once you pick the region you are going to deploy in, you cannot change that.

To accomplish this, CQL needs to know that some of the regions are deployable regions and there have to be rules to make it all makes sense.  Importantly, every region has to be contained in at most one deployable region.

Since the regions form a DAG we must create an error if any region could ever roll up to two different deployable regions. The easiest way to describe this rule is “no peeking” – the contents of a deployable region are “private” they can refer to each other in any DAG shape but outside of the deployable region you can only refer to its root. So you can still compose them but each deployable region owns a well-defined covering. Note that you can make as many fine-grained deployable regions as you want; you don’t actually have to deploy them separately, but you get stronger rules about the sharing when you do.

Here’s an example:

```
Master Deployment 1
  Feature 1 (Deployable)
    logical regions for feature 1
    Core (Deployable)
      logical regions for core
  Feature 2 (Deployable)
    logical regions for feature 2
    Core
      ...

Master Deployment 2
  Feature 1 (Deployable)
    ...

  Feature 3 (Deployable)
    logical regions for feature 3
```

In the above:
* none of the logical regions for feature 1, 2, 3 are allowed to refer to logical regions in any other feature, though any of them could refer to Core (but not directly to what is inside Core)
* within those regions you can make any set of groupings that makes sense and you can change them over time as you see fit, with some restrictions
* any such regions are not allowed to move to a different Feature group (because those are deployment regions)
* the Master Deployment regions just group features in ways we’d like to deploy them; in this case there are two deployments: one that includes Feature 1 & 2 and another that includes Feature 1 & 3
* the deployable region boundaries are preventing Feature 1 regions from using Feature 2 regions in an ad hoc way (i.e. you can't cheat by taking a direct dependency on something inside a different feature), but both Features can use Core
* Feature 3 doesn’t use Core but Core will still be in Master Deployment 2 due to Feature 1

Note that the deployable regions for Feature 1, 2, and 3 aren't actually deployed alone, but they are adding enforcement that makes the features cleaner

Because of how upgrades work, “Core” could have its own upgrader. Then when you create the upgrader for Master Deployment 1 and 2, you can specify “exclude Core” in which case those tables are assumed to be updated independently. You could create as many or as few independently upgrade-able things with this pattern. Because regions are not allowed to "peek" inside of a deployable region, you can reorganize your logical regions without breaking other parts of the schema.

#### Private Regions

The above constructs create a good basis for creating and composing regions, but a key missing aspect is the ability to hide internal details in the logical groups.  This becomes increasingly important as your desire to modularize schema grows; you will want to have certain parts that can change without worrying about breaking others and without fear that there are foreign keys and so forth referring to them.

To accomplish this, CQL provides the ability to compose schema regions with the optional `private` keyword.  In the following example there will be three regions creatively named `r1`, `r2`, and `r3`.  Region `r2` consumes `r1` privately and therefore `r3` is not allowed to use things in `r1` even though it consumes `r2`.  When creating an upgrade script for `r3` you will still need (and will get) all of `r2` and `r1`, but from a visibility perspective `r3` can only directly depend on `r2`.

```sql
@declare_schema_region r1;
@declare_schema_region r2 using r1 private;
@declare_schema_region r3 using r2;

@begin_schema_region r1;
create table r1_table(id integer primary key);
@end_schema_region;

@begin_schema_region r2;
create table r2_table(id integer primary key references r1_table(id));
@end_schema_region;

@begin_schema_region r3;

-- this is OK
create table r3_table_2(id integer primary key references r2_table(id));

-- this is an error, no peeking into r1
create table r3_table_1(id integer primary key references r1_table(id));

@end_schema_region;
```

As expected `r2` is still allowed to use `r1` because your private regions are not private from yourself.  So you may think it’s easy to work around this privacy by simply declaring a direct dependency on r1 wherever you need it.

```
@declare_schema_region my_sneaky_region using r1, other_stuff_I_need;
```

That would seem to make it all moot.  However, this is where deployable regions come in.  Once you bundle your logical regions in a deployable region there’s no more peeking inside the the deployable region.  So we could strengthen the above to:

```
@declare_deployable_region r2 using r1 private;
```

Once this is done it becomes an error to try to make new regions that peek into `r2`; you have to take all of `r2` or none of it -- and you can’t see the private parts.  Of course you can do region wrapping at any level so you can have as many subgroups as you like, whatever is useful. You can even add additional deployable regions that aren’t actually deployed to get the "hardened" grouping at no cost.

So, in summary, to get true privacy, first make whatever logical regions you like that are helpful.  Put privacy where you need/want it.  Import logical regions as much as you want in your own bundle of regions.  Then wrap that bundle up in a deployable region (they nest) and then your private regions are safe from unwanted usage.


### Unsubscription and Resubscription Features

Any significant library that is centered around a database is likely to accrue significant amounts of schema to support its features.
Often users of the library don’t want all its features and therefore don’t want all of its schema.  CQL’s primary strategy is to allow
the library author to divide the schema into regions and then the consumer of the library  may generate a suitable schema deployer
that deploys only the desired regions.  You simply subscribe to the regions you want.

The `@unsub` construct deals with the unfortunate situation of over-subscription.  In the event that a customer has subscribed to regions
that it turns out they don’t need, or if indeed the regions are not fine-grained enough, they may wish to (possibly much later) unsubscribe
from particular tables or entire regions that they previously had included.

Unfortunately it’s not so trivial as to simply remove the regions after the fact. The problem is that there might be billions
of devices that already have the undesired tables and are paying the initialization costs for them.  Affirmatively removing the tables
is highly desirable and that means a forward-looking annotation is necessary to tell the upgrader to generate `DROP` statements at some point.
Furthermore, a customer  might decide at some point later that now is the time they need the schema in question, so resubcription also
has to be possible.

#### Unsubscription and Resubscription
To accomplish this we add the following construct:

```sql
@unsub(table_name);
```

The effects of a valid `@unsub` are as follows:

* The table is no longer accessible by statements
* If the table is marked `@create`, then “DROP IF EXISTS table_name” is emitted into the upgrade steps for _version_number_
* If the table is `@recreate` the table is unconditionally dropped as though it had been deleted
* The JSON includes the unsub details in a new subscriptions section

The compiler ensures that the directives are valid and stay valid.

#### Validations for @unsub(_table_):

* _table_ must be a valid table name
* _table_ must not be already unsubscribed
* If _table_ must not be marked with `@delete`
  * unsubscribing from a table after it’s been outright deleted is clearly a mistake
* For every child table -- those that mention this table using `REFERENCES`
  * The child must be already deleted or unsubscribed
  * The deletion or unsubscription must have happened at a version <= _version_
* _table_ is marked unsubscribed for purposes of further analysis

:::caution
The legacy form `@unsub`(_version_, _table_) is supported but deprecated, and will soon be an error.  The _version_ is ignored.
The legacy `@resub` directive is now an error;  Resubscription is accomplished by simply removing the relevant `@unsub`
directive(s).
:::

#### Previous Schema validations for @unsub

Unsubscriptions may be removed when they are no longer desired in order to resubscribe as long as this results in a valid
chain of foreign keys.

These validations are sufficient to guarantee a constistent logical history for unsubscriptions.


## Chapter 11: Previous Schema Validation
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
As we saw in the previous chapter, CQL includes powerful schema management tools for creating automatic
upgrade scripts for your databases.  However, not all schema alterations are possible after-the-fact
and so CQL also includes schema comparison tools to help you avoid problems as you version your schema over time.

You can compare the previous version of a schema with the current version to do additional checks such as:

* the data type of a column may not change
* the attributes of a column (e.g. nullable, default value) may not change
* columns can't be renamed
* columns can't be removed, only marked delete
* new columns must be at the end of the table and marked with create
* created columns have to be created in a schema version >= any that previously existed (no creating columns in the past)
* nothing other than new columns at the end may be added to a table (e.g. new PK/UK is right out)
* new tables must be marked create, deleted tables must be marked delete
* new views must be marked create, deleted views must be marked delete
* new indices must be marked create, deleted indices must be marked delete
* an item that was previously a table/view cannot turn into the other one
* version numbers in the annotations may not ever change
* if any annotation has a migration proc associated with it, it cannot change to a different proc later
* created tables, views, indices have to be created in a schema version >= any that previously existed (no creating tables in the past)
* there may be other checks not mentioned here

When checking `@recreate` tables against the previous schema version for errors, these checks are done:

* suppress checking of any table facet changes in previous schema on recreate tables; you can do anything you want
* allow new `@recreate` tables to appear with no `@create` needed
* allow a table to go from "original schema" (no annotation) to `@recreate` but not back
* allow a table to go from `@recreate` to `@create` at the current schema version
* allow a table to go from recreate directly to `@delete` at the current schema version
* do not allow a table to go from `@create` or `@delete` state to `@recreate`

All of these are statically checked.


To use these tools, you must run CQL in a mode where it has both the proposed and existing schema in its input stream,
then it can provide suitable errors if any unsupported change is about to happen.

### Basic Usage

The normal way that you do previous schema validation is to create an input file that provides both schema.

This file may look something like this:

```sql
-- prev_check.sql
create table foo(
  id integer,
  new_field text @create(1)
);

@previous_schema;

create table foo(
  id integer
);
```

So, here the old version of `foo` will be validated against the new version and all is well.  A new nullable text field was added at the end.

In practice these comparisons are likely to be done in a somewhat more maintainable way, like so:

```sql
-- prev_check.sql
#include "table1.sql"
#include "table2.sql"
#include "table3.sql"

@previous_schema;

#include "previous.sql"
```

Now importantly, in this configuration, everything that follows the `@previous_schema` directive does not actually contribute to
the declared schema.  This means the `--rt schema` result type will not see it.   Because of this, you can do your checking
operation like so:

```bash
cc -E -x c prev_check.sql | cql --cg new_previous_schema.sql --rt schema
```

The above command will generate the schema in new_previous_schema and, if this command succeeds, it's safe to replace the existing
`previous.sql` with `new_previous_schema`.

NOTE: you can bootstrap the above by leaving off the `@previous_schema` and what follows to get your first previous schema from the command above.

Now, as you can imagine, comparing against the previous schema allows many more kinds of errors to be discovered.
What follows is a large chunk of the CQL tests for this area taken from the test files themselves.
For easy visibility I have brought each fragment of current and previous schema close to each other
and I show the errors that are reported.  We start with a valid fragment and go from there.


#### Case 1 : No problemo

```sql
create table foo(
  id integer not null,
  rate long int @delete(5, deletor),
  rate_2 long int @delete(4),
  id2 integer @create(4),
  name text @create(5),
  name_2 text @create(6)
);
-------
create table foo(
  id integer not null,
  rate long int @delete(5, deletor),
  rate_2 long int @delete(4),
  id2 integer @create(4),
  name text @create(5),
  name_2 text @create(6)
);
```
The table `foo` is the same!  It doesn't get any easier than that.

#### Case 2 : table create version changed
```sql
create table t_create_version_changed(id integer) @create(1);
-------
create table t_create_version_changed(id integer) @create(2);

Error at sem_test_prev.sql:15 : in str : current create version not equal to
previous create version for 't_create_version_changed'
```
You can't change the version a table was created in.  Here the new schema says it appeared in version 1.  The old schema says 2.

#### Case 3 : table delete version changed
```sql
create table t_delete_version_changed(id integer) @delete(1);
-------
create table t_delete_version_changed(id integer) @delete(2);

Error at sem_test_prev.sql:18 : in str : current delete version not equal to
previous delete version for 't_delete_version_changed'
```
You can't change the version a table was deleted in.  Here the new schema says it was gone in version 1.  The old schema says 2.

#### Case 4 : table not present in new schema
```sql
-- t_not_present_in_new_schema is gone
-------
create table t_not_present_in_new_schema(id integer);

Error at sem_test_prev.sql:176 : in create_table_stmt : table was present but now it
does not exist (use @delete instead) 't_not_present_in_new_schema'
```
So here `t_not_present_in_new_schema` was removed, it should have been marked with `@delete`.  You don't remove tables.

#### Case 5 : table is now a view
```sql
create view t_became_a_view as select 1 id @create(6);
-------
create table t_became_a_view(id integer);

Error at sem_test_prev.sql:24 : in create_view_stmt : object was a table but is now a
view 't_became_a_view'
```
Tables can't become views...

#### Case 6 : table was in base schema, now created
```sql
create table t_created_in_wrong_version(id integer) @create(1);
-------
create table t_created_in_wrong_version(id integer);

Error at sem_test_prev.sql:27 : in str : current create version not equal to previous
create version for 't_created_in_wrong_version'
```
Here a version annotation is added after the fact.  This item was already in the base schema.

#### Case 7: table was in base schema, now deleted (ok)
```sql
create table t_was_correctly_deleted(id integer) @delete(1);
-------
create table t_was_correctly_deleted(id integer);
```
No errors here, just a regular delete.

#### Case 8: column name changed
```sql
create table t_column_name_changed(id_ integer);
-------
create table t_column_name_changed(id integer);

Error at sem_test_prev.sql:33 : in str : column name is different between previous and
current schema 'id_'
```
You can't rename columns.  We could support this but it's a bit of a maintenance nightmare and logical renames are possible easily without doing physical renames.

#### Case 9 : column type changed
```sql
create table t_column_type_changed(id real);
-------
create table t_column_type_changed(id integer);

Error at sem_test_prev.sql:36 : in str : column type is different between previous
and current schema 'id'
```
You can't change the type of a column.

#### Case 10 : column attribute changed
```sql
create table t_column_attribute_changed(id integer not null);
-------
create table t_column_attribute_changed(id integer);

Error at sem_test_prev.sql:39 : in str : column type is different between previous
and current schema 'id'
```
Change of column attributes counts as a change of type.

#### Case 11: column version changed for delete
```sql
create table t_column_delete_version_changed(id integer, id2 integer @delete(1));
-------
create table t_column_delete_version_changed(id integer, id2 integer @delete(2));

Error at sem_test_prev.sql:42 : in str : column current delete version not equal to
previous delete version 'id2'
```

You can't change the delete version after it has been set.

#### Case 12 : column version changed for create
```sql
create table t_column_create_version_changed(id integer, id2 integer @create(1));
-------
create table t_column_create_version_changed(id integer, id2 integer @create(2));

Error at sem_test_prev.sql:45 : in str : column current create version not equal to
previous create version 'id2'
```

You can't change the create version after it has been set.

#### Case 13 : column default value changed
```sql
create table t_column_default_value_changed(id integer, id2 integer not null default 2);
-------
create table t_column_default_value_changed(id integer, id2 integer not null default 1);

Error at sem_test_prev.sql:48 : in str : column current default value not equal to
previous default value 'id2'
```

You can't change the default value after the fact.  There's no alter statement that would allow this even though it does make some logical sense.

#### Case 14 : column default value did not change (ok)
```sql
create table t_column_default_value_ok(id integer, id2 integer not null default 1);
-------
create table t_column_default_value_ok(id integer, id2 integer not null default 1);
```

No change. No error here.

#### Case 15 : create table with additional attribute present and matching (ok)
```sql
create table t_additional_attribute_present(a int not null, b int, primary key (a,b));
-------
create table t_additional_attribute_present(a int not null, b int, primary key (a,b));
```

No change. No error here.

#### Case 16 : create table with additional attribute (doesn't match)
```sql
create table t_additional_attribute_mismatch(a int not null, primary key (a));
-------
create table t_additional_attribute_mismatch(a int not null, b int, primary key (a,b));

Error at sem_test_prev.sql:57 : in pk_def : a table facet is different in the previous
and current schema
```

This is an error because the additional attribute does not match the previous schema.

#### Case 17 : column removed
```sql
create table t_columns_removed(id integer);
-------
create table t_columns_removed(id integer, id2 integer);

Error at sem_test_prev.sql:255 : in col_def : items have been removed from the table
rather than marked with @delete 't_columns_removed'
```

You can't remove columns from tables.  You have to mark them with `@delete` instead.

#### Case 18 : create table with added facet not present in the previous
```sql
create table t_attribute_added(a int not null, primary key (a));
-------
create table t_attribute_added(a int not null);

Error at sem_test_prev.sql:63 : in pk_def : table has a facet that is different in the
previous and current schema 't_attribute_added'
```

Table facets like primary keys cannot be added after the fact. There is no way to do this in sqlite.

#### Case 19 : create table with additional column and no `@create`
```sql
create table t_additional_column(a int not null, b int);
-------
create table t_additional_column(a int not null);

Error at sem_test_prev.sql:66 : in col_def : table has columns added without marking
them @create 't_additional_column'
```

If you add a new column like `b` above you have to mark it with `@create` in a suitable version.

#### Case 20 : create table with additional column and ``@create` (ok)
```sql
create table t_additional_column_ok(a int not null, b int @create(2), c int @create(6));
-------
create table t_additional_column_ok(a int not null, b int @create(2));
```

Column properly created.  No errors here.

#### Case 21 : create table with different flags (like TEMP)
```sql
create TEMP table t_becomes_temp_table(a int not null, b int);
-------
create table t_becomes_temp_table(a int not null, b int);

Error at sem_test_prev.sql:72 : in create_table_stmt : table create statement attributes
different than previous version 't_becomes_temp_table'
```

Table became a TEMP table, there is no way to generate an alter statement for that.  Not allowed.

#### Case 22 : create table and apply annotation (ok)
```sql
create table t_new_table_ok(a int not null, b int) @create(6);
-------
-- no previous version
```
No errors here; this is a properly created new table.

#### Case 23 : create new table without annotation (error)
```sql
create table t_new_table_no_annotation(a int not null, b int);
-------
-- no previous version

Error at sem_test_prev.sql:85 : in create_table_stmt : new table must be added with
@create(6) or later 't_new_table_no_annotation'
```
This table was added with no annotation.  It has to have an @create and be at least version 6, the current largest.

#### Case 24 : create new table stale annotation (error)
```sql
create table t_new_table_stale_annotation(a int not null, b int) @create(2);
-------
-- no previous version

Error at sem_test_prev.sql:91 : in create_table_stmt : new table must be added with
@create(6) or later 't_new_table_stale_annotation'
```
The schema is already up to version 6.  You can't then add a table in the past at version 2.

#### Case 25 : add columns to table, marked `@create` and `@delete`
```sql
create table t_new_table_create_and_delete(a int not null, b int @create(6) @delete(7));
-------
create table t_new_table_create_and_delete(a int not null);

Error at sem_test_prev.sql:96 : in col_def : table has newly added columns that are
marked both @create and @delete 't_new_table_create_and_delete'
```
Adding a column in the new version and marking it both create and delete is ... weird... don't do that.  Technically you can do it (sigh) but it must be done one step at a time.

#### Case 26 : add columns to table, marked `@create` correctly
```sql
create table t_new_legit_column(a int not null, b int @create(6));
-------
create table t_new_legit_column(a int not null);
```
No errors here; new column added in legit version.

#### Case 27 : create table with a create migration proc where there was none
```sql
create table with_create_migrator(id integer) @create(1, ACreateMigrator);
-------
create table with_create_migrator(id integer) @create(1);

Error at sem_test_prev.sql:104 : in str : @create procedure changed in object
'with_create_migrator'
```

You can't add a create migration proc after the fact.

#### Case 28 : create table with a different create migration proc
```sql
create table with_create_migrator(id integer) @create(1, ACreateMigrator);
-------
create table with_create_migrator(id integer) @create(1, ADifferentCreateMigrator);

Error at sem_test_prev.sql:104 : in str : @create procedure changed in object
'with_create_migrator'
```

You can't change a create migration proc after the fact.

#### Case 29 : create table with a delete migration proc where there was none
```sql
create table with_delete_migrator(id integer) @delete(1, ADeleteMigrator);
-------
create table with_delete_migrator(id integer) @delete(1);

Error at sem_test_prev.sql:107 : in str : @delete procedure changed in object
'with_delete_migrator'
```

You can't add a delete migration proc after the fact.

#### Case 30 : create table with a different delete migration proc
```sql
create table with_delete_migrator(id integer) @delete(1, ADeleteMigrator);
-------
create table with_delete_migrator(id integer) @delete(1, ADifferentDeleteMigrator);

Error at sem_test_prev.sql:107 : in str : @delete procedure changed in object
'with_delete_migrator'
```

You can't change a delete migration proc after the fact.

#### Case 31 : create a table which was a view in the previous schema
```sql
create table view_becomes_a_table(id int);
-------
create view view_becomes_a_table as select 1 X;

Error at sem_test_prev.sql:110 : in create_table_stmt : object was a view but is now a
table 'view_becomes_a_table'
```
Converting views to tables is not allowed.

#### Case 32 : delete a view without marking it deleted
```sql
--- no matching view in current schema
-------
create view view_was_zomg_deleted as select 1 X;

Error at sem_test_prev.sql:333 : in create_view_stmt : view was present but now it does
not exist (use @delete instead) 'view_was_zomg_deleted'
```
Here the view was deleted rather than marking it with `@delete`, resulting in an error.

#### Case 33 : create a new version of this view that is not temp
```sql
create view view_was_temp_but_now_it_is_not as select 1 X;
-------
create temp view view_was_temp_but_now_it_is_not as select 1 X;

Error at sem_test_prev.sql:339 : in create_view_stmt : TEMP property changed in new
schema for view 'view_was_temp_but_now_it_is_not'
```

A temp view became a view.  This flag is not allowed to change.  Side note: temp views are weird.

#### Case 34 : create a new version of this view that was created in a different version
```sql
create view view_with_different_create_version as select 1 X @create(3);
-------
create view view_with_different_create_version as select 1 X @create(2);

Error at sem_test_prev.sql:116 : in str : current create version not equal to previous
create version for 'view_with_different_create_version'
```
You can't change the create version of a view after the fact.


#### Case 35 : create an index that is now totally gone in the new schema
```sql
--- no matching index in current schema
-------
create index this_index_was_deleted_with_no_annotation on foo(id);

Error at sem_test_prev.sql:349 : in create_index_stmt : index was present but now it
does not exist (use @delete instead) 'this_index_was_deleted_with_no_annotation'
```
You have to use `@delete` on indices to remove them correctly.

#### Case 36 : create a view with no annotation that is not in the previous schema
```sql
create view view_created_with_no_annotation as select 1 X;
-------
--- there is no previous version

Error at sem_test_prev.sql:122 : in create_view_stmt : new view must be added with
@create(6) or later 'view_created_with_no_annotation'
```
You have to use `@create` on views to create them correctly.

#### Case 37 : index created in different version
```sql
create index this_index_has_a_changed_attribute on foo(id) @create(2);
-------
create index this_index_has_a_changed_attribute on foo(id) @create(1);

Error at sem_test_prev.sql:125 : in str : current create version not equal to previous
create version for 'this_index_has_a_changed_attribute'
```
You can't change the `@create` version of an index.

#### Case 38 : create a new index but with no `@create` annotation
```sql
create index this_index_was_created_with_no_annotation on foo(id);
-------
--- there is no previous version

Error at sem_test_prev.sql:130 : in create_index_stmt : new index must be added with
@create(6) or later 'this_index_was_created_with_no_annotation'
```

You have to use `@create` on indices to make new ones.

#### Case 39 : create a table with a column def that has a different create migrator proc
```sql
create table create_column_migrate_test(
 id int,
 id2 int @create(2, ChangedColumnCreateMigrator)
);
-------
create table create_column_migrate_test(
 id int,
 id2 int @create(2, PreviousColumnCreateMigrator)
);

Error at sem_test_prev.sql:136 : in str : column @create procedure changed 'id2'
```
You can't change the `@create` migration stored proc on columns.


#### Case 40 : create a table with a column def that has a different delete migrator proc
```sql
create table delete_column_migrate_test(
 id int,
 id2 int @delete(2, ChangedColumnDeleteMigrator)
);
-------
create table delete_column_migrate_test(
 id int,
 id2 int @delete(2, PreviousColumnDeleteMigrator)
);

Error at sem_test_prev.sql:142 : in str : column @delete procedure changed 'id2'
```

You can't change the `@delete` migration stored proc on columns.

NOTE: in addition to these errors, there are many more that do not require the previous schema which are also checked (not shown here).  These comprise things like making sure the delete version is greater than the create version on any item.  There is a lot of sensibility checking that can happen without reference to the previous schema.


## Chapter 12: Testability Features
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL includes a number of features to make it easier to create what you might call "Test" procedures.  These primarily are concerned with loading up the database with dummy data, and/or validating the result of normal procedures that query the database.  There are several interesting language features in these dimensions.

### Dummy Data

Test code can be needlessly brittle, especially when creating dummy data; any column changes typically cause all sorts of data insertion code to need to be repaired.
In many cases the actual data values are completely uninteresting to the test -- any values would do.  There are several strategies you can use to get good dummy data into your database in a more maintainable way.

#### Simple Inserts With Dummy Data

The simplest form uses a variant of the insert statement that fills in any missing columns with a seed value.  An example might be something like the below:

```sql
create proc dummy_user()
begin
  insert into users () values () @dummy_seed(123)
     @dummy_nullables @dummy_defaults;
end;
```
This statement causes all values including columns that are nullable or have a default value to get the value `123` for any numeric type and
`'column_name_123'` for any text.

If you omit the `@dummy_nullables` then any nullable fields will be null as usual.  And likewise if you omit `@dummy_defaults` then any fields with a default value will use that value as usual.  You might want any combination of these for your tests (null values are handy in your tests and default behavior is also handy.)

The `@dummy_seed` expression provided can be anything that resolves to a non-null integer value, so it can be pretty flexible.  You might use a `while` loop to insert a bunch of
rows with the seed value being computed from the `while` loop variable.

The form above is sort of like `insert * into table` in that it is giving dummy values for all columns but you can also specify some of the columns while using the seed value for others.  Importantly, you can specify values you particularly want to control either for purposes of creating a more tailored test or because you need them
to match existing or created rows in a table referenced by a foreign key.

As an example:

```sql
insert into users (id) values (1234) @dummy_seed(123)
   @dummy_nullables @dummy_defaults;
```
will provide dummy values for everything but the `id` column.

#### Using `WITH RECURSIVE`


Sometimes what you want to do is create a dummy result set without necessarily populating the database at all.  If you have code
that consumes a result set of a particular shape, it's easy enough to create a fake result set with a pattern something like this:

```sql
create procedure dummy_stuff(lim integer not null)
begin
  WITH RECURSIVE
  dummy(x) AS (
     SELECT 1
     UNION ALL
     SELECT x+1 FROM dummy WHERE x < lim)
  SELECT
    x id,
    printf("name_%d", x) name,
    cast(x % 2 as bool) is_cool,
    x * 1.3 as rate,
    x etc1,
    x etc2
  FROM dummy;
end;
```
The first part of the above creates a series of numbers from 1 to `lim`.  The second uses those values to create dummy columns.
Any result shape can be generated in this fashion.

You get data like this from the above:
```text
1|name_1|1|1.3|1|1
2|name_2|0|2.6|2|2
3|name_3|1|3.9|3|3
4|name_4|0|5.2|4|4
5|name_5|1|6.5|5|5
6|name_6|0|7.8|6|6
7|name_7|1|9.1|7|7
8|name_8|0|10.4|8|8
9|name_9|1|11.7|9|9
10|name_10|0|13.0|10|10
```

The result of the select statement is itself quite flexible and if more dummy data is what you wanted, this form can be combined with
`INSERT ... FROM SELECT...` to create dummy data in real tables.   And of course once you have a core query you could use it in a variety of ways,
combined with cursors or any other strategy to `select` out pieces and `insert` them into various tables.

#### Using Temporary Tables

If you need an API to create very flexible dummy data with values of your choice you can use temporary tables and a series of helper procedures.

First, create a table to hold the results. You can of course make this table however you need to but the `like` construct in the table creation
is especially helpful; it creates columns in the table that match the name and type of the named object.  For instance `like my_proc` is shorthand for the column names ands of the shape that `my_proc` returns.  This is perfect for
emulating the results of `my_proc`.


```sql
create proc begin_dummy()
begin
   drop table if exists my_dummy_data;

   -- the shape of my_dummy_data matches the columns
   -- returned by proc_I_want_to_emulate
   create temp table my_dummy_data(
     like proc_I_want_to_emulate;
   );
end;
```

Next, you will need a procedure that accepts and writes a single row to your temp table.  You can of course write this all explicitly but the testing
support features provide more support to make things easier; In this example, arguments  of the procedure will exactly match the output of the procedure we emulating,
one argument for each column the proc returns. The `insert` statement gets its values from the arguments.

```sql
create proc add_dummy(like proc_I_want_to_emulate)
begin
   insert into my_dummy_data from arguments;
end;
```

This allows you to create the necessary helper methods automatically even if the procedure changes over time.

Next we need a procedure to get our result set.

```sql
create proc get_dummy()
begin
  select * from my_dummy_data;
end;
```

And finally, some cleanup.

```sql
create proc cleanup_dummy()
begin
   drop table if exists my_dummy_data;
end;
```

Again the temp table could be combined with `INSERT INTO ...FROM SELECT...` to create dummy data in real tables.

#### Other Considerations

Wrapping your `insert` statements in `try/catch` can be very useful if there may be dummy data conflicts.  In test code searching for a new suitable seed is pretty easy.  Alternatively

```sql
set seed := 1 + (select max(id) from foo);
```

could be very useful.  Many alternatives are also possible.

The dummy data features are not suitable for use in production code, only tests.  But the LIKE features are generally useful for creating contract-like behavior in procs and there
are reasonable uses for them in production code.

#### Complex Result Set Example

Here's a more complicated example that can be easily rewritten using the sugar features.  This method is designed to
return a single-row result set that can be used to mock a method.  I've replaced the real fields with 'f1, 'f2' etc.

```sql
CREATE PROCEDURE test_my_subject(
  f1_ LONG INTEGER NOT NULL,
  f2_ TEXT NOT NULL,
  f3_ INTEGER NOT NULL,
  f4_ LONG INTEGER NOT NULL,
  f5_ TEXT,
  f6_ TEXT,
  f7_ TEXT,
  f8_ BOOL NOT NULL,
  f9_ TEXT,
  f10_ TEXT
)
BEGIN
  DECLARE data_cursor CURSOR LIKE my_subject;
  FETCH data_cursor()
        FROM VALUES (f1_, f2_, f3_, f4_, f5_, f6_, f7_, f8_, f9_, f10);
  OUT data_cursor;
END;
```

This can be written much more maintainably as:

```sql
CREATE PROCEDURE test_my_subject(like my_subject)
BEGIN
  DECLARE C CURSOR LIKE my_subject;
  FETCH C FROM ARGUMENTS;
  OUT C;
END;
```

Naturally, real columns have much longer names and there are often many more than 10.

### Autotest Attributes

Some of the patterns described above are so common that CQL offers a mechanism to automatically generate those test procedures.

#### Temporary Table Pattern

The attributes dummy_table, dummy_insert, and dummy_select can be used together to create and populate temp tables.

Example:

To create a dummy row set for `sample_proc`, add the cql:autotest attribute with dummy_table, dummy_insert, and dummy_select values.

```sql
create table foo(
  id integer not null,
  name text not null
);

@attribute(cql:autotest=(dummy_table, dummy_insert, dummy_select))
create proc sample_proc(foo int)
begin
  select * from Foo;
end;
```

`dummy_table` generates procedures for creating and dropping a temp table with the same shape as `sample_proc`.

```sql
CREATE PROC open_sample_proc()
BEGIN
  CREATE TEMP TABLE test_sample_proc(LIKE sample_proc);
END;

CREATE PROC close_sample_proc()
BEGIN
  DROP test_sample_proc;
END;
```

The `dummy_insert` attribute generates a procedure for inserting into the temp table.

```sql
CREATE PROC insert_sample_proc(LIKE sample_proc)
BEGIN
  INSERT INTO test_sample_proc FROM ARGUMENTS;
END;
```

The `dummy_select` attribute generates procedures for selecting from the temp table.

```sql
CREATE PROC select_sample_proc()
BEGIN
  SELECT * FROM test_sample_proc;
END;
```
It's interesting to note that the generated test code does not ever
need to mention the exact columns it is emulating because it can always use `like`, `*`, and `from arguments` in a generic way.

When compiled, the above will create C methods that can create, drop, insert, and select from the temp table.  They will have the following signatures:

```
CQL_WARN_UNUSED cql_code open_sample_proc(
  sqlite3 *_Nonnull _db_);

CQL_WARN_UNUSED cql_code close_sample_proc(
  sqlite3 *_Nonnull _db_);

CQL_WARN_UNUSED cql_code insert_sample_proc(
  sqlite3 *_Nonnull _db_,
  cql_int32 id_,
  cql_string_ref _Nonnull name_);

CQL_WARN_UNUSED cql_code select_sample_proc_fetch_results(
  sqlite3 *_Nonnull _db_,
  select_sample_proc_result_set_ref _Nullable *_Nonnull result_set);
```

#### Single Row ResultSet

In some cases, using four APIs to generate fake data can be verbose.
In the case that only a single row of data needs to be faked, the dummy_result_set attribute can be more convenient.

Example:

```sql
@attribute(cql:autotest=(dummy_result_set))
create proc sample_proc()
begin
  select id from Foo;
end;
```

Will generate the following procedure

```sql
CREATE PROC generate_sample_proc_row(LIKE sample_proc)
BEGIN
  DECLARE curs CURSOR LIKE sample_proc;
  FETCH curs FROM ARGUMENTS;
  OUT curs;
END;
```

Which generates this C API:

```c
void generate_sample_proc_row_fetch_results(
    generate_sample_proc_row_rowset_ref _Nullable *_Nonnull result_set,
    string_ref _Nonnull foo_,
    int64_t bar_);
```

These few test helpers are useful in a variety of scenarios and can save you a lot of typing and maintenance.  They evolve automatically as the code
changes, always matching the signature of the attributed procedure.

#### Generalized Dummy Test Pattern
The most flexible test helper is the `dummy_test` form.  This is far more advanced than the simple helpers above.  While the choices above were designed to help you create fake result sets pretty easily, `dummy_test` goes much further letting you set up arbitrary schema and data so that you can run your procedure on actual data.  The `dummy_test` code generator uses the features above to do its job and like the other autotest options, it works by automatically generating CQL code from your procedure definition.  However, you get a lot more code in this mode.  It's easiest to study an example so let's begin there.

To understand `dummy_test` we'll need a more complete example, so we start with this simple two-table schema with a trigger and some indices. To this we add a very small procedure that we might want to test.

```
create table foo(
 id integer not null primary key,
 name text
);

create table bar(
 id integer not null primary key references foo(id),
 data text
);

create index foo_index on foo(name);

create index bar_index on bar(data);

create temp trigger if not exists trigger1
  before delete on foo
begin
  delete from foo where name = 'this is so bogus';
end;

@attribute(cql:autotest=(
  dummy_table,
  dummy_insert,
  dummy_select,
  dummy_result_set,
  (dummy_test, (bar, (data), ('plugh'))))
)
create proc the_subject()
begin
  select * from bar;
end;
```

As you can see, we have two tables, `foo` and `bar`; the `foo` table has a trigger;  both `foo` and `bar` have indices.  This schema is very simple, but of course it could be a lot more complicated, and real cases typically are.

The procedure we want to test is creatively called `the_subject`.  It has lots of test attributes on it.  We've already discussed `dummy_table`, `dummy_insert`, `dummy_select`, and `dummy_result_set` above but as you can see they can be mixed in with `dummy_test`.  Now let's talk about `dummy_test`.  First you'll notice that annotation has additional sub-attributes; the attribute grammar is sufficiently flexible such that, in principle, you could represent an arbitrary LISP program, so the instructions can be very detailed.  In this case, the attribute provides table and column names, as well as sample data.  We'll discuss that when we get to the population code.

First let's dispense with the attributes we already discussed -- since we had all the attributes, the output will include those helpers, too.  Here they are again:

```sql
-- note that the code does not actually call the test subject
-- this declaration is used so that CQL will know the shape of the result
DECLARE PROC the_subject () (id INTEGER NOT NULL, data TEXT);

CREATE PROC open_the_subject()
BEGIN
  CREATE TEMP TABLE test_the_subject(LIKE the_subject);
END;

CREATE PROC close_the_subject()
BEGIN
  DROP TABLE test_the_subject;
END;

CREATE PROC insert_the_subject(LIKE the_subject)
BEGIN
  INSERT INTO test_the_subject FROM ARGUMENTS;
END;

CREATE PROC select_the_subject()
BEGIN
  SELECT * FROM test_the_subject;
END;

CREATE PROC generate_the_subject_row(LIKE the_subject)
BEGIN
  DECLARE curs CURSOR LIKE the_subject;
  FETCH curs FROM ARGUMENTS;
  OUT curs;
END;
```

That covers what we had before, so, what's new?  Actually, quite a bit.  We'll begin with the easiest:

```sql
CREATE PROC test_the_subject_create_tables()
BEGIN
  CREATE TABLE IF NOT EXISTS foo(
    id INTEGER NOT NULL PRIMARY KEY,
    name TEXT
  );
  CREATE TABLE IF NOT EXISTS bar(
    id INTEGER NOT NULL PRIMARY KEY REFERENCES foo (id),
    data TEXT
  );
END;
```

Probably the most important of all the helpers, `test_the_subject_create_tables` will create all the tables you need to run the procedure.  Note that in this case, even though the subject code only references `bar`, CQL determined that `foo` is also needed because of the foreign key.

The symmetric drop procedure is also generated:

```sql
CREATE PROC test_the_subject_drop_tables()
BEGIN
  DROP TABLE IF EXISTS bar;
  DROP TABLE IF EXISTS foo;
END;
```

Additionally, in this case there were triggers and indices.  This caused the creation of helpers for those aspects.

```sql
CREATE PROC test_the_subject_create_indexes()
BEGIN
  CREATE INDEX bar_index ON bar (data);
  CREATE INDEX foo_index ON foo (name);
END;

CREATE PROC test_the_subject_create_triggers()
BEGIN
  CREATE TEMP TRIGGER IF NOT EXISTS trigger1
    BEFORE DELETE ON foo
  BEGIN
  DELETE FROM foo WHERE name = 'this is so bogus';
  END;
END;

CREATE PROC test_the_subject_drop_indexes()
BEGIN
  DROP INDEX IF EXISTS bar_index;
  DROP INDEX IF EXISTS foo_index;
END;

CREATE PROC test_the_subject_drop_triggers()
BEGIN
  DROP TRIGGER IF EXISTS trigger1;
END;
```

If there are no triggers or indices, the corresponding create/drop methods will not be generated.

With these helpers available, when writing test code you can then choose if you want to create just the tables, or the tables and indices, or tables and indices and triggers by invoking the appropriate combination of helper methods.  Since all the implicated triggers and indices are automatically included, even if they change over time, maintenance is greatly simplified.

Note that in this case the code simply reads from one of the tables, but in general the procedure under test might make modifications as well.  Test code frequently has to read back the contents of the tables to verify that they were modified correctly.  So these additional helper methods are also included:

```sql
CREATE PROC test_the_subject_read_foo()
BEGIN
 SELECT * FROM foo;
END;

CREATE PROC test_the_subject_read_bar()
BEGIN
 SELECT * FROM bar;
END;
```

These procedures will allow you to easily create result sets with data from the relevant tables which can then be verified for correctness.  Of course if more tables were implicated, those would have been included as well.

As you can see, the naming always follows the convention `test_[YOUR_PROCEDURE]_[helper_type]`

Finally, the most complicated helper is the one that used that large annotation.  Recall that we provided the fragment `(dummy_test, (bar, (data), ('plugh'))))` to the compiler.  This fragment helped to produce this last helper function:

```sql
CREATE PROC test_the_subject_populate_tables()
BEGIN
  INSERT OR IGNORE INTO foo(id) VALUES(1) @dummy_seed(123);

  INSERT OR IGNORE INTO foo(id) VALUES(2) @dummy_seed(124)
      @dummy_nullables @dummy_defaults;

INSERT OR IGNORE INTO bar(data, id) VALUES('plugh', 1) @dummy_seed(125);

  INSERT OR IGNORE INTO bar(id) VALUES(2) @dummy_seed(126)
     @dummy_nullables @dummy_defaults;
END;
```

In general the `populate_tables` helper will fill all implicated tables with at least two rows of data.  It uses the dummy data features discussed earlier to generate the items using a seed.  Recall that if `@dummy_seed` is present in an `insert` statement then any missing columns are generated using that value, either as a string, or as an integer (or true/false for a boolean).   Note that the second of the two rows that is generated also specifies `@dummy_nullables` and `@dummy_defaults`.  This means that even nullable columns, and columns with a default value will get the non-null seed instead.  So you get a mix of null/default/explicit values loaded into your tables.

Of course blindly inserting data doesn't quite work.  As you can see, the insert code used the foreign key references in the schema to figure out the necessary insert order and the primary key values for `foo` were automatically specified so that they could then be used again in `bar`.

Lastly, the autotest attribute included explicit test values for the table `bar`, and  in particular the `data` column has the value `'plugh'`.  So the first row of data for table `bar` did not use dummy data for the `data` column but rather used `'plugh'`.

In general, the `dummy_test` annotation can include any number of tables, and for each table you can specify any of the columns and you can have any number of tuples of values for those columns.

NOTE: if you include primary key and/or foreign key columns among the explicit values, it's up to you to ensure that they are valid combinations.  SQLite will complain as usual if they are not, but the CQL compiler will simply emit the data you asked for.

Generalizing the example a little bit, we could use the following:

```
(dummy_test, (foo, (name), ('fred'), ('barney'), ('wilma'), ('betty')),
                        (bar, (id, data), (1, 'dino'), (2, 'hopparoo'))))
```

to generate this population:

```
CREATE PROC test_the_subject_populate_tables()
BEGIN
  INSERT OR IGNORE INTO foo(name, id) VALUES('fred', 1) @dummy_seed(123);

  INSERT OR IGNORE INTO foo(name, id) VALUES('barney', 2) @dummy_seed(124)
    @dummy_nullables @dummy_defaults;

  INSERT OR IGNORE INTO foo(name, id) VALUES('wilma', 3) @dummy_seed(125);

  INSERT OR IGNORE INTO foo(name, id) VALUES('betty', 4) @dummy_seed(126)
    @dummy_nullables @dummy_defaults;

  INSERT OR IGNORE INTO bar(id, data) VALUES(1, 'dino') @dummy_seed(127);

  INSERT OR IGNORE INTO bar(id, data) VALUES(2, 'hopparoo') @dummy_seed(128)
    @dummy_nullables @dummy_defaults;
END;
```

And of course if the annotation is not flexible enough, you can write your own data population.

The CQL above results in the usual C signatures.  For instance:

```c
CQL_WARN_UNUSED cql_code test_the_subject_populate_tables(sqlite3 *_Nonnull _db_);
```

So, it's fairly easy to call from C/C++ test code or from CQL test code.

#### Cross Procedure Limitations

Generally it's not possible to compute table usages that come from called procedures. This is
because to do so you need to see the body of the called procedure and typically that body is in a different
translation -- and is therefore not available.  A common workaround for this particular problem is to create
a dummy procedure that explicitly uses all of the desired tables.  This is significantly easier than creating all
the schema manually and still gets you triggers and indices automatically.  Something like this:

```sql
@attribute(cql:autotest=(dummy_test))
create proc use_my_stuff()
begin
  let x := select 1 from t1, t2, t3, t4, t5, t6, etc..;
end;
```

The above can be be done as a macro if desired.  But in any case `use_my_stuff` simply and directly lists the desired tables.
Using this approach you can have one set of test helpers for an entire unit rather than one per procedure.  This
is often desirable and the maintenance is not too bad.  You just use the `use_my_stuff` test helpers everywhere.


## Chapter 13: JSON Output
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
To help facilitate additional tools that might want to depend on CQL input files further down the toolchain, CQL includes a JSON output format for SQL DDL as well as stored procedure information, including special information for a single-statement DML.  "Single-statement DML" refers to those stored procedures that consist of a single `insert`, `select`, `update`, or `delete`.   Even though such procedures comprise just one statement, good argument binding can create very powerful DML fragments that are re-usable.  Many CQL stored procedures are of this form (in practice maybe 95% are just one statement.)

To use CQL in this fashion, the sequence will be something like the below.  See [Appendix 1](/cql-guide/x1) for command line details.

```bash
cql --in input.sql --rt json_schema --cg out.json
```

The output contains many different sections for the various types of entities that CQL can process.  There is a full description of
the possible outputs available at https://cgsql.dev/json-diagram.

In the balance of this chapter we'll deal with the contents of the sections and their meaning rather than the specifics of the format,
which are better described with the grammar above.

## Tables

The "tables" section has zero or more tables, each table is comprised of these fields:

* **name** : the table name
* **crc** : the schema CRC for the entire table definition, including columns and constraints
* **isTemp** : true if this is a temporary table
* **ifNotExists** : true if the table was created with "if not exists"
* **withoutRowid** : true if the table was created using "without rowid"
* **isAdded** : true if the table has an @create directive
  * **addedVersion** : optional, the schema version number in the @create directive
* **isDeleted** : true if the table was marked with @delete or is currently _unsubscribed_
  * **deletedVersion** : optional, the schema version number in the @delete directive
* **isRecreated** : true if the table is marked with @recreate
  * **recreateGroupName** : optional, if the @recreate attribute specifies a group name, it is present here
* **unsubscribedVersion** : optional, if the table was last unsubscribed, the version number when this happened
* **resubscribedVersion** : optional, if the table was last resubscribed, the version number when this happened
* **_region information_** : optional, see the section on Region Info
* **indices** : optional, a list of the names of the indices on this table, see the [indices section](#indices)
* **_attributes_** : optional, see the section on attributes, they appear in many places
* **_columns_** : an array of column definitions, see the section on columns
* **primaryKey** : a list of column names, possibly empty if no primary key
* **primaryKeySortOrders** : a list of corresponding sort orders, possibly empty, for each column of the primary key if specified
* **primaryKeyName** : optional, the name of the primary key, if it has one
* **_foreignKeys_** : a list of foreign keys for this table, possibly empty, see the [foreign keys section](#foreign-keys)
* **_uniqueKeys_** : a list of unique keys for this table, possibly empty, see the [unique keys section](#unique-keys)
* **_checkExpressions_** : a list of check expressions for this table, possibly empty, see the [check expression section](#check-expressions)

Example:

```sql
@attribute(an_attribute=(1,('foo', 'bar')))
CREATE TABLE foo(
  id INTEGER,
  name TEXT
);
```

generates:

```json
    {
      "name" : "foo",
      "CRC" : "-1869326768060696459",
      "isTemp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 0,
      "isDeleted" : 0,
      "isRecreated": 0,
      "indices" : [ "foo_name" ],
      "attributes" : [
        {
          "name" : "an_attribute",
          "value" : [1, ["foo", "bar"]]
        }
      ],
      "columns" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        }
      ],
      "primaryKey" : [  ],
      "primaryKeySortOrders" : [  ],
      "foreignKeys" : [
      ],
      "uniqueKeys" : [
      ],
      "checkExpressions" : [
      ]
    }
```

## Region Information

Region Information can appear on many entities, it consists of two optional elements:

* **region** : optional, the name of the region in which the entity was defined
* **deployedInRegion** : optional, the deployment region in which that region is located

## Attributes

Miscellaneous attributes can be present on virtual every kind of entity.  They are optional.  The root node
introduces the attributes:

* **attributes** : a list at least one attribute

Each attribute is a name and value pair:

* **name** : any string
  * attribute names are often compound like "cql:shared_fragment"
  * they are otherwise simple identifiers
* **value** : any _attribute value_

Each _attribute value_ can be:

* any literal
* an array of _attribute values_

Since the _attribute values_ can nest its possible to represent arbitrarily complex data types in an attribute.  You can even represent a LISP program.

### Global attributes

While the most common use case for attributes is to be attached to other entities (e.g., tables, columns), CQL also lets you define
"global" attributes, which are included in the top level `attributes` section of the JSON output. To specify global attributes you can
declare a variable ending with the suffix "database" and attach attributes to it. CQL will merge together all the attributes
from all the variables ending with "database" and place them in the `attributes` section of the JSON output.

The main usage of global attributes is as a way to propagate configurations across an entire CQL build. You can, for instance,
include these attributes in some root file that you `#include` in the rest of your CQL code, and by doing this these attributes
will be visible everywhere else.


Example:

```sql
@attribute(attribute_1 = "value_1")
@attribute(attribute_2 = "value_2")
declare database object;

@attribute(attribute_3 = "value_3")
declare some_other_database object;
```

Generates:

```json
    {
      "attributes": [
        {
          "name": "attribute_1",
          "value": "value_1"
        },
        {
          "name": "attribute_2",
          "value": "value_2"
        },
        {
          "name": "attribute_3",
          "value": "value_3"
        }
      ]
    }
```

## Foreign Keys

Foreign keys appear only in tables, the list of keys contains zero or more entries of this form:

* **name** : optional, the name of the foreign key if specified
* **columns** : the names of the constrained columns in the current table (the "child" table)
* **referenceTable** : the name of the table that came after REFERENCES in the foreign key
* **referenceColumns** : the constraining columns in the referenced table
* **onUpdate** : the ON UPDATE action (e.g. "CASCADE", "NO ACTION", etc.)
* **onDelete** : the ON DELETE action (e.g. "CASCADE", "NO ACTION", etc.)
* **isDeferred** : boolean, indicating the deferred or not deferred setting for this foreign key

## Unique Keys

Unique keys appear only in tables, the list of keys contains zero or more entries of this form:

* **name**: optional, the name of the unique key if specified
* **columns**: a list of 1 or more constrained column names
* **sortOrders**: a list of corresponding sort orders for the columns


## Check Expressions

Check Expressions appear only in tables, the list of keys contains zero or more entries of this form:

* **name** : optional, the name of the unique key if specified
* **checkExpr** : the check expression in plain text
* **checkExprArgs**: an array of zero or more local variables that should be bound to the `?` items in the check expression

The checkExprArgs will almost certainly be the empty list `[]`.  In the exceedingly rare situation that the table
in question was defined in a procedure and some of parts of the check expression were arguments to that procedure
then the check expression is not fully known until that procedure runs and some of its literals will be decided
at run time.  This is an extraordinary choice but technically possible.


## Columns

Columns are themselves rather complex, there are 1 or more of them in each table.  The table will have
a list of records of this form:

* **name** : the name of the columns
* **_attributes_** : optional, see the [section on attributes](#attributes), they appear in many places
* **type** : the column type (e.g. bool, real, text, etc.)
* **kind** : optional, if the type is qualified by a discriminator such as int<task_id> it appears here
* **isSensitive** : optional, indicates a column that holds sensitive information such as PII
* **isNotNull** : true if the column is not null
* **isAdded** : true if the column has an @create directive
  * **addedVersion** : optional, the schema version number in the @create directive
* **isDeleted** : true if the column was marked with @delete
  * **deletedVersion** : optional, the schema version number in the @delete directive
* **defaultValue** : optional, can be any literal, the default value of the column
* **collate** : optional, the collation string (e.g. nocase)
* **checkExpr** : optional, the _check expression_ for this column (see the related section)
* **isPrimaryKey** : true if the column was marked with PRIMARY KEY
* **isUniqueKey** : true if the column was marked with UNIQUE
* **isAutoIncrement** : true if the column was marked with AUTOINCREMENT


## Virtual Tables

The "virtualTables" section is very similar to the "tables" section with zero or more virtual table entries.
Virtual table entries are the same as table entries with the following additions:

* **module** : the name of the module that manages this virtual table
* **isEponymous** : true if the virtual table was declared eponymous
* **isVirtual** : always true for virtual tables

The JSON schema for these items was designed to be as similar as possible so that typically the same code can handle both
with possibly a few extra tests of the isVirtual field.


## Views

The views section contains the list of all views in the schema, it is zero or more view entires of this form.

* **name** : the view name
* **crc** : the schema CRC for the entire view definition
* **isTemp** : true if this is a temporary view
* **isDeleted** : true if the view was marked with @delete
  * **deletedVersion** : optional, the schema version number in the @delete directive
* **_region information_** : optional, see the section on Region Info
* **_attributes_** : optional, see the section on attributes, they appear in many places
* **_projection_** : an array of projected columns from the view, the view result if you will, see the section on projections
* **select** : the text of the select statement that defined the view
* **selectArgs** : the names of arguments any unbound expressions ("?") in the view
* **_dependencies_** : several lists of tables and how they are used in the view, see the [section on dependencies](#dependencies)

Note that the use of unbound expressions in a view truly extraordinary so selectArgs is essentially always going to be an empty list.

Example:

```sql
CREATE VIEW MyView AS
SELECT *
  FROM foo
```

Generates:

```json
    {
      "name" : "MyView",
      "CRC" : "5545408966671198580",
      "isTemp" : 0,
      "isDeleted" : 0,
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 0
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "select" : "SELECT id, name FROM foo",
      "selectArgs" : [  ],
      "fromTables" : [ "foo" ],
      "usesTables" : [ "foo" ]
    }
```

## Projections

A projection defines the output shape of something that can return a table-like value such as a view or a procedure.

The projection consists of a list of one or more _projected columns_, each of which is:

* **name** : the name of the result column  (e.g. in select 2 as foo) the name is "foo"
* **type** : the type of the column (e.g. text, real, etc.)
* **kind** : optional, the discriminator of the type if it has one (e.g. if the result is an `int<job_id>` the kind is "job_id")
* **isSensitive** : optional, true if the result is sensitive (e.g. PII or something like that)
* **isNotNull** : true if the result is known to be not null

## Dependencies

The dependencies section appears in many entities, it indicates things that were used by the object and how they were used.
Most of the fields are optional, some fields are impossible in some contexts (e.g. inserts can happen inside of views).

* **insertTables** : optional, a list of tables into which values were inserted
* **updateTables** : optional, a list of tables whose values were updated
* **deleteTables** : optional, a list of tables which had rows deleted
* **fromTables** : optional, a list of tables that appeared in a FROM clause (maybe indirectly inside a VIEW or CTE)
* **usesProcedures** : optional, a list of procedures that were accessed via CALL (not shared fragments, those are inlined)
* **usesViews** : optional, a list of views which were accessed (these are recursively visited to get to tables)
* **usesTables** : the list of tables that were used in any way at all by the current entity (i.e. the union of the previous table sections)

## Indices

The indices section contains the list of all indices in the schema, it is zero or more view entires of this form:

* **name** : the index name
* **crc** : the schema CRC for the entire index definition
* **table** : the name of the table with this index
* **isUnique** : true if this is a unique index
* **ifNotExists** : true if this index was created with IF NOT EXISTS
* **isDeleted** : true if the view was marked with @delete
  * **deletedVersion** : optional, the schema version number in the @delete directive
* **_region information_** : optional, see the section on Region Info
* **where** : optional, if this is partial index then this has the partial index where expression
* **_attributes_** : optional, see the section on attributes, they appear in many places
* **columns** : the list of column names in the index
* **sortOrders** : the list of corresponding sort orders

Example:

```sql
create index foo_name on foo(name);
```

Generates:

```json
    {
      "name" : "foo_name",
      "CRC" : "6055860615770061843",
      "table" : "foo",
      "isUnique" : 0,
      "ifNotExists" : 0,
      "isDeleted" : 0,
      "columns" : [ "name" ],
      "sortOrders" : [ "" ]
    }

```

## Procedures

The next several sections:

* Queries
* Inserts
* General Inserts
* Updates
* Deletes
* General

All provide information about various types of procedures.  Some "simple" procedures that consist only of the type of statement
correspond to their section (and some other rules) present additional information about their contents.  This can sometimes
be useful.  All the sections define certain common things about procedures so that basic information is available about
all procedures.  This is is basically the contents of the "general" section which deals with procedures that have a complex
body of which little can be said.


### Queries

The queries section corresponds to the stored procedures that are a single SELECT statement with no fragments.

The fields of a query record are:

* **name** : the name of the procedure
* **definedInFile** : the file that contains the procedure (the path is as it was specified to CQL so it might be relative or absolute)
* **definedOnLine** : the line number of the file where the procedure is declared
* **args** : _procedure arguments_ see the relevant section
* **_dependencies_** : several lists of tables and how they are used in the view, see the section on dependencies
* **_region information_** : optional, see the section on Region Info
* **_attributes_** : optional, see the section on attributes, they appear in many places
* **_projection_** : an array of projected columns from the procedure, the view if you will, see [the section on projections](#projections)
* **statement** : the text of the select statement that is the body of the procedure
* **statementArgs** : a list of procedure arguments (possibly empty) that should be used to replace the corresponding "?" parameters in the statement

Example:

```sql
create proc p(name_ text)
begin
  select * from foo where name = name_;
end;
```

Generates:

```json
    {
      "name" : "p",
      "definedInFile" : "x",
      "definedOnLine" : 3,
      "args" : [
        {
          "name" : "name_",
          "argOrigin" : "name_",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "fromTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 0
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "statement" : "SELECT id, name FROM foo WHERE name = ?",
      "statementArgs" : [ "name_" ]
    }
```

### Procedure Arguments

Procedure arguments have several generalities that don't come up very often but are important to describe.  The argument list
of a procedure is 0 or more arguments of the form:

* **name** : the argument name, any valid identifier
* **argOrigin** : either the name repeated if it's just a name or a 3 part string if it came from a bundle, see below
* **type** : the type of the argument (e.g. text, real, etc.)
* **kind** : optional, the discriminated type if any e.g. in `int<job_id>` it's "job_id"
* **isSensitive** : optional, true if the argument is marked with @sensitive (e.g. it has PII etc.)
* **isNotNull** : true if the argument is declared not null

An example of a simple argument was shown above, if we change the example a little bit to use the argument bundle syntax
(even though it's overkill) we can see the general form of argOrigin.

Example:

```sql
create proc p(a_foo like foo)
begin
  select * from foo where name = a_foo.name or id = a_foo.id;
end;
```

Generates:

```json
    {
      "name" : "p",
      "definedInFile" : "x",
      "definedOnLine" : 3,
      "args" : [
        {
          "name" : "a_foo_id",
          "argOrigin" : "a_foo foo id",
          "type" : "integer",
          "isNotNull" : 0
        },
        {
          "name" : "a_foo_name",
          "argOrigin" : "a_foo foo name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "fromTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 0
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "statement" : "SELECT id, name FROM foo WHERE name = ? OR id = ?",
      "statementArgs" : [ "a_foo_name", "a_foo_id" ]
    }
```

Note the synthetic names `a_foo_id` and `a_foo_name` the argOrigin indicates that the bundle name is `a_foo`
which could have been anything, the shape was `foo` and the column in `foo` was `id` or `name` as appropriate.

The JSON is often used to generate glue code to call procedures from different languages.  The argOrigin can be useful if
you want to codegen something other normal arguments in your code.


### General Inserts

The general insert section corresponds to the stored procedures that are a single INSERT statement with no fragments.
The fields of a general insert record are:

* **name** : the name of the procedure
* **definedInFile** : the file that contains the procedure (the path is as it was specified to CQL so it might be relative or absolute)
* **definedOnLine** : the line number of the file where the procedure is declared
* **args** : _procedure arguments_ see [the relevant section](#procedure-arguments)
* **_dependencies_** : several lists of tables and how they are used in the view, see the [section on dependencies](#dependencies)
* **_region information_** : optional, see the [section on Region Info](#region-information)
* **_attributes_** : optional, see the [section on attributes](#attributes), they appear in many places
* **table** : the name of the table the procedure inserts into
* **statement** : the text of the select statement that is the body of the procedure
* **statementArgs** : a list of procedure arguments (possibly empty) that should be used to replace the corresponding "?" parameters in the statement
* **statementType** : there are several insert forms such as "INSERT", "INSERT OR REPLACE", "REPLACE", etc. the type is encoded here

General inserts does not include the inserted values because they are not directly extractable in general.  This form is used if one of
these is true:

 * insert from multiple value rows
 * insert from a select statement
 * insert using a `WITH` clause
 * insert using the upsert clause

If fragments are in use then even "generalInsert" cannot capture everything and "general" must be used (see below).

Example:

```sql
create proc p()
begin
  insert into foo values (1, "foo"), (2, "bar");
end;
```

Generates:

```json
    {
      "name" : "p",
      "definedInFile" : "x",
      "args" : [
      ],
      "insertTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "table" : "foo",
      "statement" : "INSERT INTO foo(id, name) VALUES(1, 'foo'), (2, 'bar')",
      "statementArgs" : [  ],
      "statementType" : "INSERT",
      "columns" : [ "id", "name" ]
    }
```

### Simple Inserts

The vanilla inserts section can be used for procedures that just insert a single row.  This is a
very common case and if the JSON is being used to drive custom code generation it is useful
to provide the extra information.  The data in this section is exactly the same as the General Inserts
section except that includes the inserted values.  The "values" property has this extra information.

Each value in the values list corresponds 1:1 with a column and has this form:

* **value** : the expression for this value
* **valueArgs**: the array of procedure arguments that should replace the "?" entries in the value

Example:

```sql
create proc p(like foo)
begin
  insert into foo from arguments;
end;
```

Generates:

```json
    {
      "name" : "p",
      "definedInFile" : "x",
      "definedOnLine" : 3,
      "args" : [
        {
          "name" : "name_",
          "argOrigin" : "foo name",
          "type" : "text",
          "isNotNull" : 0
        },
        {
          "name" : "id_",
          "argOrigin" : "foo id",
          "type" : "integer",
          "isNotNull" : 0
        }
      ],
      "insertTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "table" : "foo",
      "statement" : "INSERT INTO foo(id, name) VALUES(?, ?)",
      "statementArgs" : [ "id_", "name_" ],
      "statementType" : "INSERT",
      "columns" : [ "id", "name" ],
      "values" : [
        {
          "value" : "?",
          "valueArgs" : [ "id_" ]
        },
        {
          "value" : "?",
          "valueArgs" : [ "name_" ]
        }
      ]
    }
```

### Updates

The updates section corresponds to the stored procedures that are a single UPDATE statement with no fragments. The
fields of an update record are:

* **name** : the name of the procedure
* **definedInFile** : the file that contains the procedure (the path is as it was specified to CQL so it might be relative or absolute)
* **definedOnLine** : the line number of the file where the procedure is declared
* **args** : _procedure arguments_ see [the relevant section](#procedure-arguments)
* **_dependencies_** : several lists of tables and how they are used in the view, see the section on dependencies
* **_region information_** : optional, see [the section on Region Info](#region-information)
* **_attributes_** : optional, see [the section on attributes](#attributes), they appear in many places
* **table** : the name of the table the procedure inserts into
* **statement** : the text of the update statement that is the body of the procedure
* **statementArgs** : a list of procedure arguments (possibly empty) that should be used to replace the corresponding "?" parameters in the statement


Example:

```sql
create proc p(like foo)
begin
  update foo set name = name_ where id = id_;
end;
```

Generates:

```json
    {
      "name" : "p",
      "definedInFile" : "x",
      "definedOnLine" : 3,
      "args" : [
        {
          "name" : "name_",
          "argOrigin" : "foo name",
          "type" : "text",
          "isNotNull" : 0
        },
        {
          "name" : "id_",
          "argOrigin" : "foo id",
          "type" : "integer",
          "isNotNull" : 0
        }
      ],
      "updateTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "table" : "foo",
      "statement" : "UPDATE foo SET name = ? WHERE id = ?",
      "statementArgs" : [ "name_", "id_" ]
    }
```


### Deletes

The deletes section corresponds to the stored procedures that are a single DELETE statement with no fragments. The
fields of a delete record are exactly the same as those of update.  Those are the basic fields needed to bind any
statement.

Example:

```sql
create proc delete_proc (name_ text)
begin
  delete from foo where name like name_;
end;
```

Generates:

```json
    {
      "name" : "delete_proc",
      "definedInFile" : "x",
      "definedOnLine" : 3,
      "args" : [
        {
          "name" : "name_",
          "argOrigin" : "name_",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "deleteTables" : [ "foo" ],
      "usesTables" : [ "foo" ],
      "table" : "foo",
      "statement" : "DELETE FROM foo WHERE name LIKE ?",
      "statementArgs" : [ "name_" ]
    }
```

### General

And finally the section for procedures that were encountered that are not one of the simple prepared statement forms.  The principle reasons for being in this category are:
* the procedure has out arguments
* the procedure uses something other than a single DML statement
* the procedure has no projection (no result of any type)
* the procedure uses shared fragments and hence has complex argument binding

The fields of a general procedure are something like a union of update and delete and query but with no statement info.  The are
as follows:

* **name** : the name of the procedure
* **definedInFile** : the file that contains the procedure (the path is as it was specified to CQL so it might be relative or absolute)
* **definedOnLine** : the line number of the file where the procedure is declared
* **args** : _complex procedure arguments_ see the relevant section
* **_dependencies_** : several lists of tables and how they are used in the view, see the section on dependencies
* **_region information_** : optional, see the section on Region Info
* **_attributes_** : optional, see the section on attributes, they appear in many places
* **_projection_** : optional, an array of projected columns from the procedure, the view if you will, see the section on projections
* **_result_contract_** : optional,
* **table** : the name of the table the procedure inserts into
* **statement** : the text of the update statement that is the body of the procedure
* **statementArgs** : a list of procedure arguments (possibly empty) that should be used to replace the corresponding "?" parameters in the statement
* **usesDatabase** : true if the procedure requires you to pass in a sqlite connection to call it

The result contract is at most one of these:

* **hasSelectResult** : true if the procedure generates its projection using SELECT
* **hasOutResult**: true if the procedure generates its projection using OUT
* **hasOutUnionResult**: true if the procedure generates its projection using OUT UNION

A procedure that does not produce a result set in any way will set none of these and have no projection entry.

Example:

```sql
create proc with_complex_args (inout arg real)
begin
  set arg := (select arg+1 as a);
  select "foo" bar;
end;
```

Generates:

```json
    {
      "name" : "with_complex_args",
      "definedInFile" : "x",
      "definedOnLine" : 1,
      "args" : [
        {
          "binding" : "inout",
          "name" : "arg",
          "argOrigin" : "arg",
          "type" : "real",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [  ],
      "projection" : [
        {
          "name" : "bar",
          "type" : "text",
          "isNotNull" : 1
        }
      ],
      "hasSelectResult" : 1,
      "usesDatabase" : 1
    }
```

#### Complex Procedure Arguments

The complex form of the arguments allows for an optional "binding"

* **binding** : optional, if present it can take the value "out" or "inout"
  * if absent then binding is the usual "in"

Note that atypical binding forces procedures into the "general" section.

## Interfaces

* **name** : the name of the procedure
* **definedInFile** : the file that contains the procedure (the path is as it was specified to CQL so it might be relative or absolute)
* **definedOnLine** : the line number of the file where the procedure is declared
* **attributes** : optional, see the section on attributes, they appear in many places
* **projection**: An array of projections. See [the section on projections](#projections)

Example

```sql
declare interface interface1 (id integer);
```

Generates:
```json
    {
      "name" : "interface1",
      "definedInFile" : "x.sql",
      "definedOnLine" : 1,
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 0
        }
      ]
    }
```

## Procecdure Declarations
The `declareProcs` section contains a list of procedure declaractions. Each declaration is of the form:

* **name** : the name of the procedure
* **args** : _procedure arguments_ see the relevant section
* **attributes** : optional, see the section on attributes, they appear in many places
* **projection** : An array of projections. See [the section on projections](#projections)
* **usesDatabase** : true if the procedure requires you to pass in a sqlite connection to call it

## Function Declarations

The `declareFuncs` section contains a list of function declarations, Each declaration is of the form:

* **name** : the name of the function
* **args** : see [the relevant section](#procedure-arguments)
* **attributes** : optional, see the section on attributes, they appear in many places
* **returnType** : see the relevant section below.
* **createsObject** : true if the function will create a new object (e.g. `declare function dict_create() create object;`)

### Return Type

* **type** : base type of the return value (e.g. INT, LONG)
* **kind** : optional, if the type is qualified by a discriminator such as int<task_id> it appears here
* **isSensitive** : optional, true if the result is sensitive (e.g. PII)
* **isNotNull** : true if the result is known to be not null

## Regions

The regions section contains a list of all the region definitions.  Each region is of the form:

* **name** : the name of the region
* **isDeployableRoot** : is this region itself a deployment region (declared with @declare_deployable_region)
* **deployedInRegion** : name, the deployment region that contains this region or "(orphan)" if none
   * note that deploymentRegions form a forest
* **using** : a list of zero or more parent regions
* **usingPrivately**: a list of zero more more booleans, one corresponding to each region
  * the boolean is true if the inheritance is private, meaning that sub-regions cannot see the contents of the inherited region

There are more details on regions and the meaning of these terms in Chapter 10.

## Ad Hoc Migrations

This section lists all of the declared ad hoc migrations.  Each entry is of the form:

* **name** : the name of the procedure to be called for the migration step
* **crc** : the CRC of this migration step, a hash of the call
* **_attributes_** : optional, see the section on attributes, they appear in many places

Exactly one of:

* **version**: optional, any positive integer, the version at which the migration runs, OR
* **onRecreateOf**: optional, if present indicates that the migration runs when the indicated group is recreated

There are more details on ad hoc migrations in Chapter 10.

## Enums

This section list all the enumeration types and values.  Each entry is of the form:

* **name** : the name of the enumeration
* **type** : the base type of the enumeration (e.g. INT, LONG)
* **isNotNull**: always true, all enum values are not null (here for symmetry with other uses of "type")
* **values**: a list of legal enumeration values

Each enumeration value is of the form:

* **name** : the name of the value
* **value** : a numeric literal

Example:

```sql
declare enum an_enumeration integer ( x = 5, y = 12 );
```

Generates:

````json
    {
      "name" : "an_enumeration",
      "type" : "integer",
      "isNotNull" : 1,
      "values" : [
        {
          "name" : "x",
          "value" : 5
        },
        {
          "name" : "y",
          "value" : 12
        }
      ]
    }
````

## Constant Groups

This section list all the constant groups and values.  Each entry is of the form:

* **name** : the name of the constant group
* **values**: a list of declared constant values, this can be of mixed type

Each constant value is of the form:

* **name** : the name of the constant
* **type** : the base type of the constant (e.g. LONG, REAL, etc.)
* **kind** : optional, the type kind of the constant (this can be set with a CAST on a literal, e.g. CAST(1 as int<job_id>))
* **isNotNull** : true if the constant type is not null (which is anything but the NULL literal)
* **value** : the numeric or string literal value of the constant


Example:

```sql
declare const group some_constants (
  x = cast(5 as integer<job_id>),
  y = 12.0,
  z = 'foo'
);
```

Generates:

```json
    {
      "name" : "some_constants",
      "values" : [
        {
          "name" : "x",
          "type" : "integer",
          "kind" : "job_id",
          "isNotNull" : 1,
          "value" : 5
        },
        {
          "name" : "y",
          "type" : "real",
          "isNotNull" : 1,
          "value" : 1.200000e+01
        },
        {
          "name" : "z",
          "type" : "text",
          "isNotNull" : 1,
          "value" : "foo"
        }
      ]
    }
```

## Subscriptions

This section list all the schema subscriptions in order of appearance.  Each entry is of the form:

* **type** : always "unsub" at this time
* **table** : the target of the subscription directive
* **version** : the version at which this operation is to happen (always 1 at this time)

This section is a little more complicated than it needs to be becasue of the legacy/deprecated `@resub` directive.  At
this point only the table name is relevant.  The version is always 1 and the type is always "unsub".

Example:

```sql
@unsub(foo);
```

Generates:

```json
    {
      "type" : "unsub",
      "table" : "foo",
      "version" : 1
    }
```

## Summary

These sections general provide all the information about everything that was declared in a translation unit.  Typically
not the full body of what was declared but its interface.  The schema information provide the core type and context
while the procedure information illuminates the code that was generated and how you might call it.


## Chapter 14: CQL Query Fragments
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

:::caution
CQL base fragments, extension fragments, and assembly fragments are now deprecated and will be removed. Please use [shared fragments](#shared-fragments) instead.
:::

CQL Query fragments are the most sophisticated rewrite CQL offers for productivity.  The idea is that a very large query
can be represented in "fragments" that add columns or add rows based on the original "core" query.  The final query
will be an assembled rewrite of all the fragments chained together.  Specifically, the motivation for this is that you
can have a "core" query that fetches the essential columns for some UI design and then you can add query extension
fragments that add new/additional columns for some new set of features.  The core and extended columns can be in their
own fragment and they can be compiled independently.  The result of this is that any errors are in much smaller
and easier to understand fragments rather than in some monster "fetch everything" query;  any given extension does not
have to know all the details of all the other extensions and can take a limited dependency on even the core query.

It's easiest to illustrate this with an example so let's begin there.

Let's first start with this very simple schema:

```sql
create table my_table(
 id integer primary key,
 name text not null,
 rate real not null
);

create table added_rows(
 like my_table -- sugar to duplicate the columns of my_table
);

create table added_columns(
 id integer references my_table(id),
 data text
);

```
Typically there would be a lot more columns but where you see `flag1` and `flag2` appear in fragments you can imagine any number
of additional columns of any type.  So we can keep the examples simple.

### Base Query Fragments

The base fragment might look something like this:

```sql
@attribute(cql:base_fragment=base_frag)
create proc base_frag_template(id_ integer not null)
begin
  with
    base_frag(*) as (select * from my_table where my_table.id = id_)
    select * from base_frag;
end;
```

Here are the essential aspects:

* the base fragment is given a name, it can be anything, probably something that describes the purpose of the fragments
* the procedure name can be anything at all
* the procedure must consist of exactly one `with...select` statement
* the fragment name must be the one and only CTE in the select statement
* you must select all the columns from the CTE

Note the syntax helper `base_frag(*)` is just shorthand to avoid retyping all the column names of `my_table`.

The interesting part is `(select * from my_table where my_table.id = id_)` which could have been any select statement
of your choice. Everything else in the procedure must follow the designated format, and the format is enforced due to
the presence of `@attribute(cql:base_fragment=base_frag)`.

The point of putting everything on rails like this is that all base fragments will look the same and it will be clear how to transform any base fragment into the final query when it is assembled with its extensions.

Note: the base fragment produces no codegen at all.  There is no `base_frag_template` procedure in the output.  This is just a template.  Also, the name of the procedure cannot be `base_frag` this name will be used by the assembly fragment later.  Really any descriptive unique name will do since the name does not appear in the output at all.

### Extension Query Fragments

#### Adding Columns

The most common thing that an extension might want to do is add columns to the result.  There can be any number of such extensions in the final assembly.  Here's a simple example that adds one column.

```sql
@attribute(cql:extension_fragment=base_frag)
create proc adds_columns(id_ integer not null)
begin
  with
    base_frag(*) as (select 1 id, "name" name, 1.0 rate),
    col_adder_frag(*) as (
    select base_frag.*, added_columns.data
      from base_frag
      left outer join added_columns on base_frag.id = added_columns.id)
  select * from col_adder_frag;
end;
```
Again there are some important features to this extension and they are largely completely constrained, i.e. you must follow the pattern.

* the attribute indicates `extension_fragment` and the name (here `base_frag`) must have been previously declared in a `base_fragment`
* the procedure name can be any unique name other than `base_frag` - it corresponds to this particular extension's purpose
* the procedure arguments must be identical to those in the base fragment
* the first CTE must match the `base_fragment` attribute value, `base_frag` in this case
* you do not need to repeat the full select statement for `base_frag`, any surrogate with the same column names and types will do
  * the base fragment code might include a #define to make this easier
    * e.g. `#define base_frags_core as base_frag(*) as (select 1 id, "name" name, 1.0 rate)`
  * doing so will make maintenance easier if new columns are added to the base fragment
* there must be exactly one additional CTE
  * it may have any unique descriptive name you like
  * it must begin with `select base_frags.*` with the appropriate CTE name matching the base fragment CTE
  * it must add at least one column (or it would be uninteresting)
  * it may not have any clause other than the first `from` (e.g. no `where`, `having`, `limit` etc.)
    * if any of these were allowed they would remove or re-order rows in the base query which is not allowed
    * the `from` clause often includes nested selects which have no restrictions
  * it must select from the base fragment name and left outer join to wherever it likes to get optional additional columns
    * because of this the additional column(s) will certainly be a nullable type in the projection
* the final select must be of the form `select * from col_adder_frag` with the appropriate name
* keeping all this in mind, the interesting bit happens here:  `left outer join added_columns on base_frag.id = added_columns.id`
  * this is where you get the data for your additional column using values in the core columns

This fragment can be (and should be) compiled in its own compiland while using `#include` to get the base fragment only.  This will result in code gen for the accessor functions for a piece of the overall query -- the part this extension knows about.  Importantly code that uses this extension's data does not need or want to know about any other extensions that may be present, thereby keeping
dependencies under control.

The C signatures generated would look like this:

```c
extern cql_int32 adds_columns_get_id(
    base_frag_result_set_ref _Nonnull result_set,
    cql_int32 row);

extern cql_string_ref _Nonnull adds_columns_get_name(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_double adds_columns_get_rate(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_string_ref _Nullable adds_columns_get_data(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_int32 adds_columns_result_count(
  base_frag_result_set_ref _Nonnull result_set);
```

Even if there were dozens of other extensions, the functions for reading those columns would not be declared in the header for
this extension.  Any given extension "sees" only the core columns plus any columns it added.

#### Adding Rows

Query extensions also frequently want to add additional rows to the main result set, based on the data that is already present.

The second form of extension allows for this; it is similarly locked in form.  Here is an example:

```sql
@attribute(cql:extension_fragment=base_frag)
create proc adds_rows(id_ integer not null)
begin
  with
    base_frag(*) as (select 1 id, "name" name, 1.0 rate),
    row_adder_frag(*) as (
    select * from base_frag
    union all
    select * from added_rows)
  select * from row_adder_frag;
end;
```

Let's review the features of this second template form:
* there is a surrogate for the core query
* there is a mandatory second CTE
* the second CTE is a compound query with any number of branches, all `union all`
* the first branch must be `select * from base_frag` (the base fragment) to ensure that the original rows remain
  * this is also why all the branches must be `union all`
* this form cannot add new columns
* the extension CTE may not include `order by` or `limit` because that might reorder or remove rows of the base
* any extensions of this form must come before those of the `left outer join` form for a given base fragment
  * which ironically means `row_adder_frag` has to come before `col_adder_frag`
* the usual restrictions are in place on compound selects (same type and number of columns) to ensure a consistent result
* the final select after the CTE section must be exactly in the form `select * from row_adder_frag` which is the name of the one and only additional CTE with no other clauses or options
  * in practice only the CTE will be used to create the final assembly, so even if you did change the final select to something else it would be moot

The signatures generated for this will look something like so:

```c
extern cql_int32 adds_rows_get_id(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_string_ref _Nonnull adds_rows_get_name(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_double adds_rows_get_rate(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_int32 adds_rows_result_count(
  base_frag_result_set_ref _Nonnull result_set);
```

which gives you access to the core columns.  Again this fragment can and should be compiled standalone with only the declaration
for the base fragment in the same translation unit to get the cleanest possible output.  This is so that consumers of this
extension do not "see" other extensions which may or may not be related and may or may not always be present.

#### Assembling the Fragments

With all the fragments independently declared they need to be unified to create one final query. This is where the
major rewriting happens.  The `assembly_fragment` looks something like this:

```sql
@attribute(cql:assembly_fragment=base_frag)
create proc base_frag(id_ integer not null)
begin
  with
    base_frag(*) as (select 1 id, "name" name, 1.0 rate)
    select * from base_frag;
end;
```

It will always be as simple as this; all the complexity is in the fragments.

* the `assembly_fragment` name must match the core fragment name
* the procedure arguments must be identical to the base fragment arguments
* the  procedure must have the same name as the assembly fragment (`base_frag` in this case)
  * the code that was generated for the previous fragments anticipates this and makes reference to what will be generated here
  * this is enforced
* the assembled query is what you run to get the result set, this has real code behind it
  * the other fragments only produce result set readers that call into the helper methods to get columns
* there is a surrogate for the core fragment as usual
* all of CTE section will ultimately be replaced with the fragments chained together
* the final select should be of the form `select * from your_frags` but it can include ordering and/or filtering, this statement will be present in final codegen, the final order is usually defined here


When compiling the assembly fragment, you should include the base, and all the other fragments, and the assembly template.  The presence of the assembly_fragment will cause codegen for the extension fragments to be suppressed. The assembly translation unit only contains the assembly query as formed from the fragments.

Now let's look at how the query is rewritten, the process is pretty methodical.

After rewriting the assembly looks like this:

```sql
CREATE PROC base_frag (id_ INTEGER NOT NULL)
BEGIN
  WITH
  base_frag (id, name, rate) AS (SELECT *
    FROM my_table
    WHERE my_table.id = id_),
  row_adder_frag (id, name, rate) AS (SELECT *
    FROM base_frag
  UNION ALL
  SELECT *
    FROM added_rows),
  col_adder_frag (id, name, rate, data) AS (SELECT row_adder_frag.*, added_columns.data
    FROM row_adder_frag
    LEFT OUTER JOIN added_columns ON row_adder_frag.id = added_columns.id)
  SELECT *
    FROM col_adder_frag;
END;
```

Let's dissect this part by part. Each CTE serves a purpose:

* the core CTE was replaced by the CTE in the base_fragment, and it appears directly
* next, the first extension was added as a CTE referring to the base fragment just as before
  * recall that the first extension has to be `row_adder_frag`, as that type must come first
  * looking at the chain you can see why it would be hard to write a correct fragment if it came after columns were added
* next the second extension was added as a CTE
  * all references to the base fragment were replaced with references to row_adder_frag
  * the extra column names in the CTE were added such that all previous column names are introduced
* this process continues until all extensions are exhausted
* the final select statement reads all the columns from the last extension CTE and includes and ordering and so forth that was present in the assembly query

The result of all this is a single query that gets all the various columns that were requested in all the extensions
and all the `union all` operations play out as written.  The extensions are emitted in the order that they appear
in the translation unit with the assembly, which again must have the row adding extensions first.

This facility provides considerable ability to compose a large query, but each fragment can be independently checked for errors
so that nobody ever has to debug the (possibly monstrous) overall result.  Fragments can be removed simply by
excluding them from the final assembly (with e.g. #ifdefs, or build rules)

With the rewrite of the assembly_fragment complete, the codegen for that procedure is the normal codegen for a procedure with a single select.

As always, Java and Objective C codegen on these pieces will produce suitable wrappers for the C.

The output code for the assembly fragment generates these reading functions:

```c
extern cql_int32 base_frag_get_id(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_string_ref _Nonnull base_frag_get_name(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_double base_frag_get_rate(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

// used by adds_columns_get_data() to read its data
extern cql_string_ref _Nullable __PRIVATE__base_frag_get_data(
  base_frag_result_set_ref _Nonnull result_set,
  cql_int32 row);

extern cql_int32 base_frag_result_count(
  base_frag_result_set_ref _Nonnull result_set);
```

These are exactly what you would get for a normal query except that the pieces that came from extensions are marked `PRIVATE`.  Those methods should not be used directly but instead the methods generated for each extension proc should be used.

Additionally, to create the result set, as usual.

```c
extern CQL_WARN_UNUSED cql_code base_frag_fetch_results(
  sqlite3 *_Nonnull _db_,
  base_frag_result_set_ref _Nullable *_Nonnull result_set,
  cql_int32 id_);
```

With the combined set of methods you can create a variety of assembled queries from extensions in a fairly straightforward way.

### Shared Fragments

Shared fragments do not have the various restrictions that the "extension" style fragments have.  While extensions
were created to allow a single query to be composed by authors that did not necessarily work with each other,
and therefore they are full of restrictions on the shape, shared queries instead are designed to give you
maximum flexibility in how the fragments are re-used.  You can think of them as being somewhat like a parameterized
view, but the parameters are both value parameters and type parameters.  In Java or C#, a shared fragments might have
had an invocation that looked something like this:  `my_fragment(1,2)<table1, table2>.  As with the other fragment types
the common table expression (CTE) is the way that they plug in.

It's helpful to consider a real example:

```sql
split_text(tok) AS (
  WITH RECURSIVE
    splitter(tok,rest) AS (
      SELECT
        '' tok,
        IFNULL( some_variable_ || ',', '') rest
      UNION ALL
      SELECT
        substr(rest, 1, instr(rest, ',') - 1) tok,
        substr(rest, instr(rest, ',') + 1) rest
        FROM splitter
        WHERE rest != ''
  )
  SELECT tok from splitter where tok != ''
)
```

This text might appear in dozens of places where a comma separated list needs to be split into pieces and there is no good way
to share the code between these locations.  CQL is frequently used in conjunction with the C-pre-processor so you could
come up with something using the #define construct but this is problematic for several reasons:

* the compiler does not then know that the origin of the text really is the same
  * thus it has no clue that sharing the text of the string might be a good idea
* any error messages happen in the context of the use of the macro not the definition
* bonus: a multi-line macro like the above gets folded into one line so any error messages are impenetrable
* if you try to compose such macros it only gets worse; it's more code duplication and harder error cases
* any IDE support for syntax coloring and so forth will be confused by the macro as it's not part of the language

None of this is any good but the desire to create helpers like this is real both for correctness and for performance.

To make these things possible, we introduce the notion of shared fragments.  We need to give them parameters
and the natural way to create a select statement that is bindable in CQL is the procedure. So the shape we choose
looks like this:

```sql
@attribute(cql:shared_fragment)
CREATE PROC split_text(value TEXT)
BEGIN
  WITH RECURSIVE
    splitter(tok,rest) AS (
      SELECT
        '' tok,
        IFNULL( value || ',', '') rest
      UNION ALL
      SELECT
        substr(rest, 1, instr(rest, ',') - 1) tok,
        substr(rest, instr(rest, ',') + 1) rest
        FROM tokens
        WHERE rest != ''
  )
  SELECT tok from splitter where tok != ''
END;
```

The introductory attribute `@attribute(cql:shared_fragment)` indicates that the procedure is to produce
no code, but rather will be inlined as a CTE in other locations.  To use it, we introduce the ability
to call a procedure as part of a CTE declaration.  Like so:

```sql
WITH
  result(v) as (call split_text('x,y,z'))
  select * from result;
```

Once the fragment has been defined, the statement above could appear anywhere, and of course the
text `'x,y,z'` need not be constant.  For instance:

```sql
CREATE PROC print_parts(value TEXT)
BEGIN
  DECLARE C CURSOR FOR
    WITH
      result(v) as (CALL split_text('x,y,z'))
      SELECT * from result;

  LOOP FETCH C
  BEGIN
     CALL printf("%s\n", C.v);
  END;
END;
```

Fragments are also composable, so for instance, we might also want some shared code that
extracts comma separated numbers.  We could do this:

```sql
@attribute(cql:shared_fragment)
CREATE PROC ids_from_string(value TEXT)
BEGIN
  WITH
    result(v) as (CALL split_text(value))
  SELECT CAST(v as LONG) as id from result;
END;
```

Now we could write:

```sql
CREATE PROC print_ids(value TEXT)
BEGIN
  DECLARE C CURSOR FOR
    WITH
      result(id) as (CALL ids_from_string('1,2,3'))
      SELECT * from result;

  LOOP FETCH C
  BEGIN
     CALL printf("%ld\n", C.id);
  END;
END;
```

Of course these are very simple examples but in principle you can use the generated tables in whatever
way is necessary.  For instance, here's a silly but illustrative example:

```sql
/* This is a bit silly */
CREATE PROC print_common_ids(value TEXT)
BEGIN
  DECLARE C CURSOR FOR
    WITH
      v1(id) as (CALL ids_from_string('1,2,3')),
      v2(id) as (CALL ids_from_string('2,4,6'))
      SELECT * from v1
      INTERSECT
      SELECT * from v2;

  LOOP FETCH C
  BEGIN
     CALL printf("%ld\n", C.id);
  END;
END;
```

With a small amount of dynamism in the generation of the SQL for the above, it's possible to share the body
of v1 and v2.  SQL will of course see the full expansion but your program only needs one copy no matter
how many times you use the fragment anywhere in the code.

So far we have illustrated the "parameter" part of the flexibility.  Now let's look at the "generics" part;
even though it's overkill for this example, it should still be illustrative.  You could imagine that
the procedure we wrote above `ids_from_string` might do something more complicated, maybe filtering out
negative ids, ids that are too big, or that don't match some pattern, whatever the case might be.  You
might want these features in a variety of contexts, maybe not just starting from a string to split.

We can rewrite the fragment in a "generic" way like so:

```sql
@attribute(cql:shared_fragment)
CREATE PROC ids_from_string_table()
BEGIN
  WITH
    source(v) LIKE (select "x" v)
  SELECT CAST(v as LONG) as id from source;
END;
```

Note the new construct for a CTE definition: inside a fragment we can use "LIKE" to define a plug-able CTE.
In this case we used a `select` statement to describe the shape the fragment requires.  We could also
have used a name `source(*) LIKE shape_name` just like we use shape names when describing cursors.  The
name can be any existing view, table, a procedure with a result, etc.  Any name that describes a shape.

Now when the fragment is invoked, you provide the actual data source (some table, view, or CTE) and
that parameter takes the role of "values".  Here's a full example:

```sql
CREATE PROC print_ids(value TEXT)
BEGIN
  DECLARE C CURSOR FOR
    WITH
      my_data(*) as (CALL split_text(value)),
      my_numbers(id) as (CALL ids_from_string_table() USING my_data AS source)
      SELECT id from my_numbers;

  LOOP FETCH C
  BEGIN
     CALL printf("%ld\n", C.id);
  END;
END;
```

We could actually rewrite the previous simple id fragment as follows:

```sql
@attribute(cql:shared_fragment)
CREATE PROC ids_from_string(value TEXT)
BEGIN
  WITH
    tokens(v) as (CALL split_text(value))
    ids(id) as (CALL ids_from_string_table() USING tokens as source)
  SELECT * from ids;
END;
```

And actually we have a convenient name we could use for the shape we need so
we could have used the shape syntax to define `ids_from_string_table`.

```sql
@attribute(cql:shared_fragment)
CREATE PROC ids_from_string_table()
BEGIN
  WITH
    source(*) LIKE split_text
  SELECT CAST(tok as LONG) as id from source;
END;
```

These examples have made very little use of the database but of course
normal data is readily available, so shared fragments can make a great
way to provide access to complex data with shareable, correct code.
For instance, you could write a fragment that provides the ids of all
open businesses matching a name from a combination of tables.  This is
similar to what you could do with a `VIEW` plus a `WHERE` clause but:

* such a system can give you well controlled combinations known to work well
* there is no schema required, so your database load time can still be fast
* parameterization is not limited to filtering VIEWs after the fact
* "generic" patterns are available, allowing arbitrary data sources to be filtered, validated, augmented
* each fragment can be tested separately with its own suite rather than only in the context of some larger thing
* code generation can be more economical because the compiler is aware of what is being shared

In short, shared fragments can help with the composition of any complicated kinds of queries.
If you're producing an SDK to access a data set, they are indispensible.

#### Creating and Using Valid Shared Fragments

When creating a fragment the following rules are enforced:

* the fragment many not have any out arguments
* it must consist of exactly one valid select statement (but see future forms below)
* it may use the LIKE construct in CTE definitions to create placeholder shapes
  * this form is illegal outside of shared fragments (otherwise how would you bind it)
* the LIKE form may only appear in top level CTE expressions in the fragment
* the fragment is free to use other fragments, but it may not call itself
  * calling itself would result in infinite inlining

Usage of a fragment is always introduced by a "call" to the fragment name in a CTE body.
When using a fragment the following rules are enforced.

* the provided parameters must create a valid procedure call just like normal procedure calls
  * i.e. the correct number and type of arguments
* the provided parameters may not use nested `(SELECT ...)` expressions
  * this could easily create fragment building within fragment building which seems not worth the complexity
  * if database access is required in the parameters simply wrap it in a helper procedure
* the optional USING clause must specify each required table parameter exactly once and no other tables
  * a fragment that requires table parameters be invoked without a USING clause
* every actual table provided must match the column names of the corresponding table parameter
  * i.e. in `USING my_data AS values` the actual columns in `my_data` must be the same as in the `values` parameter
  * the columns need not be in the same order
* each column in any actual table must be "assignment compatible" with its corresponding column in the parameters
  * i.e. the actual type could be converted to the formal type using the same rules as the := operator
  * these are the same rules used for procedure calls, for instance, where the call is kind of like assigning the actual parameter values to the formal parameter variables
* the provided table values must not conflict with top level CTEs in the shared fragment
  * exception: the top level CTEs that were parameters do not create conflicts
  * e.g. it's common to do `values(*) as (CALL something() using source as source)` - here the caller's "source" takes the value of the fragment's "source", which is not a true conflict
  * however, the caller's source might itself have been a parameter in which case the value provided could create an inner conflict
    * all these problems are easily avoided with a simple naming convention for parameters so that real arguments never look like parameter names and parameter forwarding is apparent
    * e.g. `USING _source AS _source` makes it clear that a parameter is being forwarded and `_source` is not likely to conflict with real table or view names

Note that when shared fragments are used, the generated SQL has the text split into parts, with each fragment and its surroundings separated, therefore
the text of shared fragments is shared(!) between usages if normal linker optimizations for text folding are enabled (common in production code.)

### Shared Fragments with Conditionals

Shared fragments use dynamic assembly of the text to do the sharing but it is also possible to create alternative texts.
There are many instances where it is desirable to not just replace parameters but use, for instance, an entirely different join sequence.
Without shared fragments, the only way to accomplish this is to fork the desired query at the topmost level (because SQLite has no internal
possibility of "IF" conditions.)  This is expensive in terms of code size and also cognitive load because the entire alternative sequences
have to be kept carefully in sync.  Macros can help with this but then you get the usual macro maintenance problems, including poor diagnostics.
And of course there is no possibility to share the common parts of the text of the code if it is forked.

However, conditional shared fragments allow forms like this:

```sql
@attribute(cql:shared_fragment)
CREATE PROC ids_from_string(val TEXT)
BEGIN
  IF val IS NULL OR val IS '' THEN
    SELECT 0 id WHERE 0; -- empty result
  ELSE
    WITH
      tokens(v) as (CALL split_text(val))
      ids(id) as (CALL ids_from_string_table() USING tokens as source)
    SELECT * from ids;
  END IF;
END;
```

Now we can do something like:

```sql
  ids(*) AS (CALL ids_from_string(str))
```

In this case, if the string `val` is empty then SQLite will not see the complex comma splitting code, and instead will see
the trivial case `select 0 id where 0`.  The code in a conditional fragment might be entirely different between the branches
removing unnecessary code, or swapping in a new experimental cache in your test environment, or anything like that.

The generalization is simply this:

* instead of just one select statement there is one top level "IF" statement
* each statement list of the IF must be exactly one select statement
* there must be an ELSE clause
* the select statements must be type compatible, just like in a normal procedure
* any table parameters with the same name in different branches must have the same type
  * otherwise it would be impossible to provide a single actual table for those table parameters

With this additional flexibility a wide variety of SQL statements can be constructed economically and maintainability.  Importantly,
consumers of the fragments need not deal with all these various alternate possibilities but they can readily create their own
useful combinations out of building blocks.

Ultimately, from SQLite's perspective, all of these shared fragment forms result in nothing more complicated than a chain of CTE expressions.

See Appendix 8 for an extensive section on best practices around fragments and common table expressions in general.

:::tip
If you use CQL's query planner on shared fragments with conditionals, the query planner will only analyze the first branch by default. You need to use `@attribute(cql:query_plan_branch=[integer])` to modify the behaviour. Read [Query Plan Generation](/cql-guide/ch15) for details.
:::

### Shared Fragments as Expressions

The shared fragment system also has the ability to create re-usable expression-style fragments giving you something like SQL inline functions.  These do come with
some performance cost so they should be used for larger fragments.  In many systems a simple shared fragment would not compete well with an equivalent `#define`.
Expression fragments shine when:

* the fragment is quite large
* its used frequently (hence providing significant space savings)
* the arguments are complex, potentially used many times in the expression

From a raw performance perspective, the best you can hope for with any of the fragment approaches is a "tie" on speed compared do directly inlining equivalent
SQL or using a macro to do the same.  However, from a correctness and space perspective it is very possible to come out ahead.  It's fair to say that
expression fragments have the greatest overhead compared to the other types and so they are best used in cases where the size benefits are going to be important.

#### Syntax

An expression fragment is basically a normal shared fragment with no top-level `FROM` clause that generates a single column.  A typical one might look like this:

```sql
-- this isn't very exciting because regular max would do the job
@attribute(cql:shared_fragment)
create proc max_func(x integer, y integer)
begin
  select case when x >= y then x else y end;
end;
```

The above can be used in the context of a SQL statement like so:

```sql
select max_func(T1.column1, T1.column2) the_max from foo T1;
```

#### Discussion

The consequence of the above is that the body of `max_func` is inlined into the generated SQL.  However, like
the other shared fragments, this is done in such a way that the text can be shared between instances so
you only pay for the cost of the text of the SQL in your program one time, no matter how many time you use it.

In particular, for the above, the compiler will generate the following SQL:

```sql
select (
  select case when x >= y then x else y end
    from (select T1.column1 x, column2 y))
```

But each line will be its own string literal, so, more accurately, it will concatenate the following three strings:

```c
"select (",                                      // string1
" select case when x >= y then x else y end",    // string2
" from (select T1.column1 x, column2 y))"        // string3
```

Importantly, `string2` is fixed for any given fragment.  The only thing that changes is `string3`, i.e., the arguments.
In any modern C compilation system, the linker will unify the `string2` literal across all translation units so you only
pay for the cost of that text one time.  It also means that the text of the arguments appears exactly one time,
no matter how complex they are.  For these benefits, we pay the cost of the select wrapper on the arguments.  If
the arguments are complex that "cost" is negative.  Consider the following:

```sql
select max_func((select max(T.m) from T), (select max(U.m) from U))
```

A direct expansion of the above would result in something like this:

```sql
case when (select max(T.m) from T) >= (select max(U.m) from U)
   then (select max(T.m) from T)
   else (select max(U.m) from U)
end;
```

The above could be accomplished with a simple `#define` style macro. However, the expression fragment
generates the following code:

```sql
select (
  select case when x >= y then x else y end
    from select (select max(T.m) from T) x, (select max(U.m) from U) y))
```

Expression fragments can nest, so you could write:

```sql
@attribute(cql:shared_fragment)
create proc max3_func(x integer, y integer, z integer)
begin
  select max_func(x, max_func(y, z));
end;
```

Again, this particular example is a waste because regular `max` would already do the job better.

To give another example, common mappings from one kind of code to another using case/when can be written
and shared this way:

```sql
-- this sort of thing happens all the time
@attribute(cql:shared_fragment)
create proc remap(x integer not null)
begin
   select case x
     when 1 then 1001
     when 2 then 1057
     when 3 then 2010
     when 4 then 2011
     else 9999
   end;
end;
```

In the following:

```sql
select remap(T1.c), remap(T2.d), remap(T3.e) from T1, T2, T3... etc.
```

The text for `remap` will appear three times in the generated SQL query but only one time in your binary.

#### Restrictions

* the fragment must consist of exactly one simple select statement
  * no `FROM`, `WHERE`, `HAVING`, etc. -- the result is an expression
* the select list must have exactly one value
  * Note: the expression can be a nested `SELECT` which could then have all the usual `SELECT` elements
* the usual shared fragment rules apply, e.g. no out-parameters, exactly one statement, etc.


#### Additional Notes

A simpler syntax might have been possible but expression fragments are only interesting in SQL contexts where
(among other things) normal procedure and function calls are not available. So the `select` keyword makes it
clear to the coder and the compiler that the expression will be evaluated by SQLite and the rules for what is
allowed to go in the expression are the SQLite rules.

The fragment has no `FROM` clause because we're trying to produce an expression, not a table-value with one column.
If you want a table-value with one column, the original shared fragments solution already do exactly that.
Expression fragments give you a solution for sharing code in, say, the `WHERE` clause of a larger select statement.

Commpared to something like

```sql
#define max_func(x,y) case when (x) >= (y) then x else y end;
```

The macro does give you a ton of flexibility, but it has many problems:
* if the macro has an error, you see the error in the call site with really bad diagnostic info
* the compiler doesn't know that the sharing is going on so it won't be able to share text between call sites
* the arguments can be evaluated many times each which could be expensive, bloaty, or wrong
* there is no type-checking of arguments to the macro so you may or may not get compilation errors after expansion
* you have to deal with all the usual pre-processor hazards

In general, macros _can_ be used (as in C and C++) as an alternative to expression fragments, especially for small fragments.
But, this gets to be a worse idea as such macros grow.  For larger cases, C and C++ provide inline functions --
CQL provides expression fragments.


## Chapter 15: Query Plan Generation
CQL offers a way to run SQLite's [`EXPLAIN QUERY PLAN` command](https://www.sqlite.org/eqp.html) for all the SQL statements used in a CQL file using a set of special compilation steps.

Generating query plans is inherently complicated. Any given stored procedure might include many SQL statements, each of which has a plan. To get the plans, those statements must be executed in the appropriate mode. In order for them to execute, whatever tables, views, and user-defined functions they use must exist. The statements can have any number of parameters, those have to be swapped out because they might come from anywhere. When run in `--rt query_plan` mode, the compiler accomplishes all of this by analyzing the original code and creating entirely new code. Running this new code creates the schema and, with the appropriate transforms, runs all the SQL statements in the original to give us the query plans. The process requires many steps as we'll see below.

## Query Plan Generation Compilation Steps
:::tip
The following steps are used in `./repl/go_query_plan.sh`, you can [run it to get a quick demonstration of this feature in action](/docs/playground#query-plan-playground). The rest of the section explains how query plan generation works and some of its quirks.
:::

To execute query plans for a given CQL file, the following commands need to be run:

```bash
CQL_FILE= # The CQL file to compile
CQL_ROOT_DIR= # Path to cql directory
CQL=$CQL_ROOT_DIR/out/cql

# Generate Query Plan Script
$CQL --in $CQL_FILE --rt query_plan --cg go-qp.sql

# Generate UDF stubs
$CQL --in $CQL_FILE --rt udf --cg go-qp-udf.h go-qp-udf.c

# Compile and link CQL artifacts, with a main C file query_plan_test.c
$CQL --in go-qp.sql --cg go-qp.h go-qp.c --dev
cc -I$CQL_ROOT_DIR -I. -c $CQL_ROOT_DIR/query_plan_test.c go-qp.c go-qp-udf.c
cc -I$CQL_ROOT_DIR -I. -O -o go_query_plan go-qp.o go-qp-udf.o query_plan_test.o $CQL_ROOT_DIR/cqlrt.c -lsqlite3

# Run and generate query plans
./go_query_plan
```

Contrary to what one might expect, rather than providing query plans directly, CQL uses `--rt query_plan` to generate a second CQL script that returns query plans for each SQL statement used.

A separate command, `--rt udf` is required to generate any stubbed [user defined functions](/cql-guide/ch08) that are used in the original script. Afterwards, the generated query plan script, udf stubs needs to compiled like any CQL file and run by a "main" function that needs to be created separately.

The CQL repository provides the file [`query_plan_test.c`](https://github.com/facebookincubator/CG-SQL/blob/main/sources/query_plan_test.c) that can be used as the "main" function, otherwise you can make your own.

::note
When compiling the CQL file generated by `--rt query_plan`, the `--dev` flag is required.
:::

### Special Handling of CQL features in Query Plan Generation
CQL's query planner generator modifies the usage of the following features to allow SQLite run `EXPLAIN QUERY PLAN` successfully:

- Variables
- User Defined Functions
- Conditionals in Shared Fragments

:::caution
Generating query plans of CQL files that use table valued functions, or [virtual tables](https://sqlite.org/vtab.html#:~:text=2.-,Table%2Dvalued%20functions,columns%20of%20the%20virtual%20table.) is not supported.
:::

#### Variables
Variables used in SQL statements are stubbed into constants. The exact value varies depending on the type of the variable, but it is always equivalent to some form of `"1"`.

```sql title="original.sql"
...
SELECT *
FROM my_table
WHERE id = x;
...
```

```sql title="query_plan.sql"
...
EXPLAIN QUERY PLAN
SELECT *
FROM my_table
WHERE my_table.id = nullable(1);
...
```

#### User Defined Functions
_Read [Functions](/cql-guide/ch08) on details about Function Types._

Since the implementation of UDFs in a CQL file do not affect SQLite query plans, CQL's query plan script expects stubs generated by `cql --rt udf` to be used instead.

#### Conditionals in Shared Fragments
_Read [CQL Query Fragments](/cql-guide/ch14) on details about shared fragments_

Only one branch of a conditional is chosen for query plan analysis. By default this will be the first branch, which is the initial `SELECT` statement following the `IF` conditional.
The branch to analyze can be configured with the `cql:query_plan_branch` [@attribute](/cql-guide/x3).

Here's an example of `cql:query_plan_branch` being used:

```sql title="original.sql"
@attribute(cql:shared_fragment)
@attribute(cql:query_plan_branch=1)
CREATE PROC frag2(y int)
BEGIN
  IF y == 2 THEN
    SELECT 10 b;
  ELSE IF y == -1 THEN
    SELECT 20 b;
  ELSE
    SELECT 30 b;
  END IF;
END;
```

```sql title="query_plan.sql"
EXPLAIN QUERY PLAN
SELECT 20 b;
```

Setting `cql:query_plan_branch=1` selects the second branch. Providing `cql:query_plan_branch=2` instead would yield the `ELSE` clause `SELECT 30 b`. `cql:query_plan_branch=0` would yield `SELECT 10 b`, which is the same as the default behaviour.


## Appendix 1: Command Line Options
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
CQL has a variety of command line (CLI) options but many of them are only interesting for cql development.  Nonetheless this is a comprehensive list:

* note CQL is often used after the c pre-processor is run so this kind of invocation is typical:

```
cc -E -x c foo.sql | cql [args]
```

### With No Options
* emits a usage message

### --in file

* reads the given file for the input instead of stdin
* the input should probably have already been run through the C pre-processor as above
* returns non-zero if the file fails to parse

Example:
```
cql --in test.sql
```

### --sem
* performs semantic analysis on the input file ONLY
* the return code is zero if there are no errors

Example:
```
cql --in sem_test.sql --sem
```

### --ast
* walks the AST and prints it to stdout in human readable text form
* may be combined with --sem (semantic info will be included)
Example
```
cql --in sem_test.sql --sem --ast >sem_ast.out
```

### --echo
* walks the AST and emits the text of a program that would create it
* this has the effect of "beautifying" badly formatted input or "canonicalizing" it
  * some sensible indenting is added but it might not be the original indenting
  * extraneous whitespace, parens, etc. are removed
* may be combined with --sem (in which case you see the source after any rewrites for sugar)
* this also validates that the input can be parsed

Example
```
cql --in test.sql --echo >test.out  # test.out is "equivalent" to test.sql
```

### --dot
* prints the internal AST to stdout in DOT format for graph visualization
* this is really only interesting for small graphs for discussion as it rapidly gets insane

Example:
```
cql --dot --in dottest.sql
```
### --cg output1 output2 ...

* any number of output files may be needed for a particular result type, two is common
* the return code is zero if there are no errors
* any --cg option implies --sem

Example:

```
cql --in foo.sql --cg foo.h foo.c
```

### --nolines

* Suppress the # directives for lines.  Useful if you need to debug the C code.

Example:

```
cql --in test.sql --nolines --cg foo.h foo.c
```

### --global_proc name
* any loose SQL statements not in a stored proc are gathered and put into a procedure of the given name
* when generating a schema migrate script the global proc name is used as a prefix on all of the artifacts so that there can be several independent migrations linked into a single executable

### --compress
* for use with the C result type, (or any similar types added to the runtime array in your compiler)
* string literals for the SQL are broken into "fragments" the DML is then represented by an array of fragments
* since DML is often very similar there is a lot of token sharing possible
* the original string is recreated at runtime from the fragments and then executed
* comments show the original string inline for easier debugging and searching

NOTE: different result types require a different number of output files with different meanings

### --test
* some of the output types can include extra diagnostics if `--test` is included
* the test output often makes the outputs badly formed so this is generally good for humans only

### --dev
* some codegen features only make sense during development, this enables dev mode to turn those one
** example: [explain query plan](/cql-guide/ch15)

### --c_include_namespace
* for the C codegen runtimes, it determines the header namespace (as in #include "namespace/file.h") that goes into the output C file
* if this option is used, it is prefixed to the first argment to --cg to form the include path in the C file
* if absent there is no "namespace/" prefix

### --c_include_path
* for the C codegen runtimes, it determines the full header path (as in #include "your_arg") that goes into the output C file
* if this option is used, the first argment to --cg controls only the output path and does not appear in include path at all
* this form overrides --c_include_namespace if both are specified

### --objc_c_include_path
* for ObjC codegen runtimes that need to refer to the generated C code, this represents the header of the C generated code that will be used during inclusion from the ObjC file

### Result Types (--rt *)

These are the various outputs the compiler can produce.

#### --rt c
* requires two output files (foo.h and foo.c)
* this is the standard C compilation of the sql file

##### --cqlrt foo.h
* emits `#include "foo.h"` into the C output instead of `#include "cqlrt.h"`

##### --generate_type_getters
* changes C output for CQL result sets so that the field readers used shared functions to get fields of a certain type
* this style of codegen makes result-sets more interoperable with each other if they have similar shape so it can be useful

##### --generate_exports
* adds an additional output file
 * example:  `--in foo.sql --generate_exports --rt c --cg foo.h foo.c foo_exports.sql
* the output file `foo_exports.sql` includes procedure declarations for the contents of `foo.sql`
* basically automatically generates the CQL header file you need to access the procedures in the input from some other file
 * if it were C it would be like auto-generating `foo.h` from `foo.c`

#### --rt objc
* objective C wrappers for result sets produced by the stored procedures in the input
* these depend on the output of a standard codegen run so this is additive
* requires one output file (foo.h)

#### --rt schema
* produces the canonical schema for the given input files
* stored procedures etc. are removed
* whitespace etc. is removed
* suitable for use to create the next or first "previous" schema for schema validation
* requires one output file

#### --rt schema_upgrade
* produces a CQL schema upgrade script which can then be compiled with CQL itself
* see the chapter on schema upgrade/migration: [Chapter 10](https://cgsql.dev/cql-guide/ch10/)
* requires one output file (foo.sql)

##### --include_regions a b c
* the indicated regions will be declared
* used with `--rt schema_upgrade` or `--rt schema`
* in the upgrade case excluded regions will not be themselves upgraded, but may be referred, to by things that are being upgraded

##### --exclude_regions x y z
* the indicated regions will still be declared but the upgrade code will be suppressed, the presumption being that a different script already upgrades x y z
* used with `--rt schema_upgrade`

##### --min_schema_version n
* the schema upgrade script will not include upgrade steps for schema older than the version specified

##### --schema_exclusive
* the schema upgrade script assumes it owns all the schema in the database, it aggressively removes other things

#### --rt json_schema
* produces JSON output suitable for consumption by downstream codegen
* the JSON includes a definition of the various entities in the input
* see the section on JSON output for details

#### --rt query_plan
* produces CQL output which can be re-compiled by CQL as normal input
* the output consists of a set of procedures that will emit all query plans for the DML that was in the input
* see also `--rt udf` and [Chapter 15](/cql-guide/ch15)

#### --rt stats
* produces  a simple .csv file with node count information for AST nodes per procedure in the input
* requires one output file (foo.csv)

#### --rt udf
* produces stub UDF implementations for all UDFS that were seen in the input
* this output is suitable for use with `--rt query_plan` so that SQL with UDFs will run in a simple context
* requires two output files (e.g. udfs.h and udfs.c)
* See also [Chapter 15](/cql-guide/ch15)



## Appendix 2: CQL Grammar
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from a grammar snapshot with the tree building rules removed.
It should give a fair sense of the syntax of CQL (but not semantic validation).

Snapshot as of Fri Mar  3 21:02:20 PST 2023

### Operators and Literals

These are in order of priority lowest to highest

```
"UNION ALL" "UNION" "INTERSECT" "EXCEPT"
":="
"OR"
"AND"
"NOT"
"BETWEEN" "NOT BETWEEN" "<>" "!=" '=' "==" "LIKE" "NOT LIKE" "GLOB" "NOT GLOB" "MATCH" "NOT MATCH" "REGEXP" "NOT REGEXP" "IN" "NOT IN" "IS NOT" "IS" "IS TRUE" "IS FALSE" "IS NOT TRUE" "IS NOT FALSE"
"ISNULL" "NOTNULL"
'<' '>' ">=" "<="
"<<" ">>" '&' '|'
'+' '-'
'*' '/' '%'
"||"
"COLLATE"
"UMINUS" '~'
```
NOTE: The above varies considerably from the C binding order!!!

Literals:
```
ID  /* a name */
STRLIT /* a string literal in SQL format e.g. 'it''s sql' */
CSTRLIT /* a string literal in C format e.g. "hello, world\n" */
BLOBLIT /* a blob literal in SQL format e.g. x'12ab' */
INTLIT /* integer literal */
LONGLIT /* long integer literal */
REALLIT /* floating point literal */
```
### Statement/Type Keywords
```
"@ATTRIBUTE" "@BEGIN_SCHEMA_REGION" "@BLOB_CREATE_KEY"
"@BLOB_CREATE_VAL" "@BLOB_GET_KEY" "@BLOB_GET_KEY_TYPE"
"@BLOB_GET_VAL" "@BLOB_GET_VAL_TYPE" "@BLOB_UPDATE_KEY"
"@BLOB_UPDATE_VAL" "@CREATE" "@DECLARE_DEPLOYABLE_REGION"
"@DECLARE_SCHEMA_REGION" "@DELETE" "@DUMMY_SEED" "@ECHO"
"@EMIT_CONSTANTS" "@EMIT_ENUMS" "@EMIT_GROUP"
"@END_SCHEMA_REGION" "@ENFORCE_NORMAL" "@ENFORCE_POP"
"@ENFORCE_PUSH" "@ENFORCE_RESET" "@ENFORCE_STRICT"
"@EPONYMOUS" "@FILE" "@PREVIOUS_SCHEMA" "@PROC" "@RC"
"@RECREATE" "@SCHEMA_AD_HOC_MIGRATION"
"@SCHEMA_UPGRADE_SCRIPT" "@SCHEMA_UPGRADE_VERSION"
"@SENSITIVE" "@UNSUB" "ABORT" "ACTION" "ADD" "AFTER" "ALL"
"ALTER" "ARGUMENTS" "AS" "ASC" "AUTOINCREMENT" "BEFORE"
"BEGIN" "BLOB" "BY" "CALL" "CASCADE" "CASE" "CAST" "CATCH"
"CHECK" "CLOSE" "COLUMN" "COLUMNS" "COMMIT" "CONST"
"CONSTRAINT" "CONTEXT COLUMN" "CONTEXT TYPE" "CONTINUE"
"CREATE" "CROSS" "CURRENT ROW" "CURSOR HAS ROW" "CURSOR"
"DECLARE" "DEFAULT" "DEFERRABLE" "DEFERRED" "DELETE" "DESC"
"DISTINCT" "DISTINCTROW" "DO" "DROP" "ELSE IF" "ELSE"
"ENCODE" "END" "ENUM" "EXCLUDE CURRENT ROW" "EXCLUDE GROUP"
"EXCLUDE NO OTHERS" "EXCLUDE TIES" "EXCLUSIVE" "EXISTS"
"EXPLAIN" "FAIL" "FETCH" "FILTER" "FIRST" "FOLLOWING" "FOR
EACH ROW" "FOR" "FOREIGN" "FROM BLOB" "FROM" "FUNC"
"FUNCTION" "GROUP" "GROUPS" "HAVING" "HIDDEN" "IF" "IGNORE"
"IMMEDIATE" "INDEX" "INITIALLY" "INNER" "INOUT" "INSERT"
"INSTEAD" "INT" "INTEGER" "INTERFACE" "INTO" "JOIN" "KEY"
"LAST" "LEAVE" "LEFT" "LET" "LIMIT" "LONG" "LONG_INT"
"LONG_INTEGER" "LOOP" "NO" "NOT DEFERRABLE" "NOTHING"
"NULL" "NULLS" "OBJECT" "OF" "OFFSET" "ON CONFLICT" "ON"
"OPEN" "ORDER" "OUT" "OUTER" "OVER" "PARTITION" "PRECEDING"
"PRIMARY" "PRIVATE" "PROC" "PROCEDURE" "QUERY PLAN" "RAISE"
"RANGE" "REAL" "RECURSIVE" "REFERENCES" "RELEASE" "RENAME"
"REPLACE" "RESTRICT" "RETURN" "RIGHT" "ROLLBACK" "ROWID"
"ROWS" "SAVEPOINT" "SELECT" "SET" "SIGN FUNCTION"
"STATEMENT" "SWITCH" "TABLE" "TEMP" "TEXT" "THEN" "THROW"
"TO" "TRANSACTION" "TRIGGER" "TRY" "TYPE" "TYPE_CHECK"
"UNBOUNDED" "UNIQUE" "UPDATE" "UPSERT" "USING" "VALUES"
"VAR" "VIEW" "VIRTUAL" "WHEN" "WHERE" "WHILE" "WINDOW"
"WITH" "WITHOUT"
```
### Rules

Note that in many cases the grammar is more generous than the overall language and errors have to be checked on top of this, often this is done on purpose because even when it's possible it might be very inconvenient to do checks with syntax.  For example the grammar cannot enforce non-duplicate ids in id lists, but it could enforce non-duplicate attributes in attribute lists.  It chooses to do neither as they are easily done with semantic validation.  Thus the grammar is not the final authority on what constitutes a valid program but it's a good start.
```


program:
  opt_stmt_list
  ;

opt_stmt_list:
  /*nil*/
  | stmt_list
  ;

stmt_list:
  stmt ';'
  | stmt_list stmt ';'
  ;

stmt:
  misc_attrs any_stmt
  ;

any_stmt:
    alter_table_add_column_stmt
  | begin_schema_region_stmt
  | begin_trans_stmt
  | blob_get_key_type_stmt
  | blob_get_val_type_stmt
  | blob_get_key_stmt
  | blob_get_val_stmt
  | blob_create_key_stmt
  | blob_create_val_stmt
  | blob_update_key_stmt
  | blob_update_val_stmt
  | call_stmt
  | close_stmt
  | commit_return_stmt
  | commit_trans_stmt
  | continue_stmt
  | create_index_stmt
  | create_proc_stmt
  | create_table_stmt
  | create_trigger_stmt
  | create_view_stmt
  | create_virtual_table_stmt
  | declare_deployable_region_stmt
  | declare_enum_stmt
  | declare_const_stmt
  | declare_group_stmt
  | declare_select_func_no_check_stmt
  | declare_func_stmt
  | declare_out_call_stmt
  | declare_proc_no_check_stmt
  | declare_proc_stmt
  | declare_interface_stmt
  | declare_schema_region_stmt
  | declare_vars_stmt
  | declare_forward_read_cursor_stmt
  | declare_fetched_value_cursor_stmt
  | declare_type_stmt
  | delete_stmt
  | drop_index_stmt
  | drop_table_stmt
  | drop_trigger_stmt
  | drop_view_stmt
  | echo_stmt
  | emit_enums_stmt
  | emit_group_stmt
  | emit_constants_stmt
  | end_schema_region_stmt
  | enforce_normal_stmt
  | enforce_pop_stmt
  | enforce_push_stmt
  | enforce_reset_stmt
  | enforce_strict_stmt
  | explain_stmt
  | select_nothing_stmt
  | fetch_call_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_cursor_from_blob_stmt
  | guard_stmt
  | if_stmt
  | insert_stmt
  | leave_stmt
  | let_stmt
  | loop_stmt
  | out_stmt
  | out_union_stmt
  | out_union_parent_child_stmt
  | previous_schema_stmt
  | proc_savepoint_stmt
  | release_savepoint_stmt
  | return_stmt
  | rollback_return_stmt
  | rollback_trans_stmt
  | savepoint_stmt
  | select_stmt
  | schema_ad_hoc_migration_stmt
  | schema_unsub_stmt
  | schema_upgrade_script_stmt
  | schema_upgrade_version_stmt
  | set_stmt
  | switch_stmt
  | throw_stmt
  | trycatch_stmt
  | update_cursor_stmt
  | update_stmt
  | upsert_stmt
  | while_stmt
  | with_delete_stmt
  | with_insert_stmt
  | with_update_stmt
  | with_upsert_stmt
  ;

explain_stmt:
  "EXPLAIN" opt_query_plan explain_target
  ;

opt_query_plan:
  /* nil */
  | "QUERY PLAN"
  ;

explain_target: select_stmt
  | update_stmt
  | delete_stmt
  | with_delete_stmt
  | with_insert_stmt
  | insert_stmt
  | upsert_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | begin_trans_stmt
  | commit_trans_stmt
  ;

previous_schema_stmt:
  "@PREVIOUS_SCHEMA"
  ;

schema_upgrade_script_stmt:
  "@SCHEMA_UPGRADE_SCRIPT"
  ;

schema_upgrade_version_stmt:
  "@SCHEMA_UPGRADE_VERSION" '(' "integer-literal" ')'
  ;

set_stmt:
  "SET" name ":=" expr
  | "SET" name "FROM" "CURSOR" name
  ;

let_stmt:
  "LET" name ":=" expr
  ;

version_attrs_opt_recreate:
  /* nil */
  | "@RECREATE"  opt_delete_plain_attr
  | "@RECREATE" '(' name ')'  opt_delete_plain_attr
  | version_attrs
  ;

opt_delete_plain_attr:
  /* nil */
  | "@DELETE"
  ;

opt_version_attrs:
  /* nil */
  | version_attrs
  ;

version_attrs:
  "@CREATE" version_annotation opt_version_attrs
  | "@DELETE" version_annotation opt_version_attrs
  ;

opt_delete_version_attr:
  /* nil */
  | "@DELETE" version_annotation
  ;

drop_table_stmt:
  "DROP" "TABLE" "IF" "EXISTS" name
  | "DROP" "TABLE" name
  ;

drop_view_stmt:
  "DROP" "VIEW" "IF" "EXISTS" name
  | "DROP" "VIEW" name
  ;

drop_index_stmt:
  "DROP" "INDEX" "IF" "EXISTS" name
  | "DROP" "INDEX" name
  ;

drop_trigger_stmt:
  "DROP" "TRIGGER" "IF" "EXISTS" name
  | "DROP" "TRIGGER" name
  ;

create_virtual_table_stmt: "CREATE" "VIRTUAL" "TABLE" opt_vtab_flags name
                           "USING" name opt_module_args
                           "AS" '(' col_key_list ')' opt_delete_version_attr ;

opt_module_args: /* nil */
  | '(' misc_attr_value_list ')'
  | '(' "ARGUMENTS" "FOLLOWING" ')'
  ;

create_table_prefix_opt_temp:
  "CREATE" opt_temp "TABLE" ;

create_table_stmt:
  create_table_prefix_opt_temp opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate
  ;

opt_temp:
  /* nil */
  | "TEMP"
  ;

opt_if_not_exists:
  /* nil */
  | "IF" "NOT" "EXISTS"
  ;

opt_no_rowid:
  /* nil */
  | "WITHOUT" "ROWID"
  ;

opt_vtab_flags:
  /* nil */
  | "IF" "NOT" "EXISTS"
  | "@EPONYMOUS"
  | "@EPONYMOUS" "IF" "NOT" "EXISTS"
  | "IF" "NOT" "EXISTS" "@EPONYMOUS"
  ;

col_key_list:
  col_key_def
  | col_key_def ',' col_key_list
  ;

col_key_def:
  col_def
  | pk_def
  | fk_def
  | unq_def
  | check_def
  | shape_def
  ;

check_def:
  "CONSTRAINT" name "CHECK" '(' expr ')'
  | "CHECK" '(' expr ')'
  ;

shape_exprs :
  shape_expr ',' shape_exprs
  | shape_expr
  ;

shape_expr:
  name
  | '-' name
  ;

shape_def:
    shape_def_base
  | shape_def_base '(' shape_exprs ')'
  ;

shape_def_base:
    "LIKE" name
  | "LIKE" name "ARGUMENTS"
  ;

col_name:
  name
  ;

misc_attr_key:
  name
  | name ':' name
  ;

misc_attr_value_list:
  misc_attr_value
  | misc_attr_value ',' misc_attr_value_list
  ;

misc_attr_value:
  name
  | any_literal
  | const_expr
  | '(' misc_attr_value_list ')'
  | '-' num_literal
  | '+' num_literal
  ;

misc_attr:
  "@ATTRIBUTE" '(' misc_attr_key ')'
  | "@ATTRIBUTE" '(' misc_attr_key '=' misc_attr_value ')'
  ;

misc_attrs:
  /* nil */
  | misc_attr misc_attrs
  ;

col_def:
  misc_attrs col_name data_type_any col_attrs
  ;

pk_def:
  "CONSTRAINT" name "PRIMARY" "KEY" '(' indexed_columns ')' opt_conflict_clause
  | "PRIMARY" "KEY" '(' indexed_columns ')' opt_conflict_clause
  ;

opt_conflict_clause:
  /* nil */
  | conflict_clause
  ;

conflict_clause:
  "ON CONFLICT" "ROLLBACK"
  | "ON CONFLICT" "ABORT"
  | "ON CONFLICT" "FAIL"
  | "ON CONFLICT" "IGNORE"
  | "ON CONFLICT" "REPLACE"
  ;

opt_fk_options:
  /* nil */
  | fk_options
  ;

fk_options:
  fk_on_options
  | fk_deferred_options
  | fk_on_options fk_deferred_options
  ;

fk_on_options:
  "ON" "DELETE" fk_action
  | "ON" "UPDATE" fk_action
  | "ON" "UPDATE" fk_action "ON" "DELETE" fk_action
  | "ON" "DELETE" fk_action "ON" "UPDATE" fk_action
  ;

fk_action:
  "SET" "NULL"
  | "SET" "DEFAULT"
  | "CASCADE"
  | "RESTRICT"
  | "NO" "ACTION"
  ;

fk_deferred_options:
  "DEFERRABLE" fk_initial_state
  | "NOT DEFERRABLE" fk_initial_state
  ;

fk_initial_state:
  /* nil */
  | "INITIALLY" "DEFERRED"
  | "INITIALLY" "IMMEDIATE"
  ;

fk_def:
  "CONSTRAINT" name "FOREIGN" "KEY" '(' name_list ')' fk_target_options
  | "FOREIGN" "KEY" '(' name_list ')' fk_target_options
  ;

fk_target_options:
  "REFERENCES" name '(' name_list ')' opt_fk_options
  ;

unq_def:
  "CONSTRAINT" name "UNIQUE" '(' indexed_columns ')' opt_conflict_clause
  | "UNIQUE" '(' indexed_columns ')' opt_conflict_clause
  ;

opt_unique:
  /* nil */
  | "UNIQUE"
  ;

indexed_column:
  expr opt_asc_desc
  ;

indexed_columns:
  indexed_column
  | indexed_column ',' indexed_columns
  ;

create_index_stmt:
  "CREATE" opt_unique "INDEX" opt_if_not_exists name "ON" name '(' indexed_columns ')' opt_where opt_delete_version_attr
  ;

name:
  "ID"
  | "TEXT"
  | "TRIGGER"
  | "ROWID"
  | "REPLACE"
  | "KEY"
  | "VIRTUAL"
  | "TYPE"
  | "HIDDEN"
  | "PRIVATE"
  | "FIRST"
  | "LAST"
  ;

opt_name:
  /* nil */
  | name
  ;

name_list:
  name
  |  name ',' name_list
  ;

opt_name_list:
  /* nil */
  | name_list
  ;

cte_binding_list:
  cte_binding
  | cte_binding ',' cte_binding_list
  ;

cte_binding: name name
  | name "AS" name
  ;

col_attrs:
  /* nil */
  | "NOT" "NULL" opt_conflict_clause col_attrs
  | "PRIMARY" "KEY" opt_conflict_clause col_attrs
  | "PRIMARY" "KEY" opt_conflict_clause "AUTOINCREMENT" col_attrs
  | "DEFAULT" '-' num_literal col_attrs
  | "DEFAULT" '+' num_literal col_attrs
  | "DEFAULT" num_literal col_attrs
  | "DEFAULT" const_expr col_attrs
  | "DEFAULT" str_literal col_attrs
  | "COLLATE" name col_attrs
  | "CHECK" '(' expr ')' col_attrs
  | "UNIQUE" opt_conflict_clause col_attrs
  | "HIDDEN" col_attrs
  | "@SENSITIVE" col_attrs
  | "@CREATE" version_annotation col_attrs
  | "@DELETE" version_annotation col_attrs
  | fk_target_options col_attrs
  ;

version_annotation:
  '(' "integer-literal" ',' name ')'
  | '(' "integer-literal" ',' name ':' name ')'
  | '(' "integer-literal" ')'
  ;

opt_kind:
  /* nil */
  | '<' name '>'
  ;

data_type_numeric:
  "INT" opt_kind
  | "INTEGER" opt_kind
  | "REAL" opt_kind
  | "LONG" opt_kind
  | "BOOL" opt_kind
  | "LONG" "INTEGER" opt_kind
  | "LONG" "INT" opt_kind
  | "LONG_INT" opt_kind
  | "LONG_INTEGER" opt_kind
  ;

data_type_any:
  data_type_numeric
  | "TEXT"  opt_kind
  | "BLOB"  opt_kind
  | "OBJECT" opt_kind
  | "OBJECT" '<' name "CURSOR" '>'
  | "OBJECT" '<' name "SET" '>'
  | "ID"
  ;

data_type_with_options:
  data_type_any
  | data_type_any "NOT" "NULL"
  | data_type_any "@SENSITIVE"
  | data_type_any "@SENSITIVE" "NOT" "NULL"
  | data_type_any "NOT" "NULL" "@SENSITIVE"
  ;

str_literal:
  str_chain
  ;

str_chain:
  str_leaf
  | str_leaf str_chain
  ;

str_leaf:
  "sql-string-literal"
  | "c-string-literal"
  ;

num_literal:
  "integer-literal"
  | "long-literal"
  | "real-literal"
  | "TRUE"
  | "FALSE"
  ;

const_expr:
  "CONST" '(' expr ')'
  ;

any_literal:
  str_literal
  | num_literal
  | "NULL"
  | "@FILE" '(' str_literal ')'
  | "@PROC"
  | "sql-blob-literal"
  ;

raise_expr:
  "RAISE" '(' "IGNORE" ')'
  | "RAISE" '(' "ROLLBACK" ','  expr ')'
  | "RAISE" '(' "ABORT" ','  expr ')'
  | "RAISE" '(' "FAIL" ','  expr ')'
  ;

call:
  name '(' arg_list ')' opt_filter_clause
  | name '(' "DISTINCT" arg_list ')' opt_filter_clause
  | basic_expr ':' name '(' arg_list ')'
  ;

basic_expr:
  name
  | "@RC"
  | name '.' name
  | any_literal
  | const_expr
  | '(' expr ')'
  | call
  | window_func_inv
  | raise_expr
  | '(' select_stmt ')'
  | '(' select_stmt "IF" "NOTHING" expr ')'
  | '(' select_stmt "IF" "NOTHING" "OR" "NULL" expr ')'
  | '(' select_stmt "IF" "NOTHING" "THROW"')'
  | "EXISTS" '(' select_stmt ')'
  | "CASE" expr case_list "END"
  | "CASE" expr case_list "ELSE" expr "END"
  | "CASE" case_list "END"
  | "CASE" case_list "ELSE" expr "END"
  | "CAST" '(' expr "AS" data_type_any ')'
  | "TYPE_CHECK" '(' expr "AS" data_type_with_options ')'

math_expr:
  basic_expr
  | math_expr '&' math_expr
  | math_expr '|' math_expr
  | math_expr "<<" math_expr
  | math_expr ">>"  math_expr
  | math_expr '+' math_expr
  | math_expr '-' math_expr
  | math_expr '*' math_expr
  | math_expr '/' math_expr
  | math_expr '%' math_expr
  | math_expr "IS NOT TRUE"
  | math_expr "IS NOT FALSE"
  | math_expr "ISNULL"
  | math_expr "NOTNULL"
  | math_expr "IS TRUE"
  | math_expr "IS FALSE"
  | '-' math_expr
  | '+' math_expr
  | '~' math_expr
  | "NOT" math_expr
  | math_expr '=' math_expr
  | math_expr "==" math_expr
  | math_expr '<' math_expr
  | math_expr '>' math_expr
  | math_expr "<>" math_expr
  | math_expr "!=" math_expr
  | math_expr ">=" math_expr
  | math_expr "<=" math_expr
  | math_expr "NOT IN" '(' expr_list ')'
  | math_expr "NOT IN" '(' select_stmt ')'
  | math_expr "IN" '(' expr_list ')'
  | math_expr "IN" '(' select_stmt ')'
  | math_expr "LIKE" math_expr
  | math_expr "NOT LIKE" math_expr
  | math_expr "MATCH" math_expr
  | math_expr "NOT MATCH" math_expr
  | math_expr "REGEXP" math_expr
  | math_expr "NOT REGEXP" math_expr
  | math_expr "GLOB" math_expr
  | math_expr "NOT GLOB" math_expr
  | math_expr "BETWEEN" math_expr "AND" math_expr
  | math_expr "NOT BETWEEN" math_expr "AND" math_expr
  | math_expr "IS NOT" math_expr
  | math_expr "IS" math_expr
  | math_expr "||" math_expr
  | math_expr "COLLATE" name
  ;

expr:
  math_expr
  | expr "AND" expr
  | expr "OR" expr
  ;

case_list:
  "WHEN" expr "THEN" expr
  | "WHEN" expr "THEN" expr case_list
  ;

arg_expr: '*'
  | expr
  | shape_arguments
  ;

arg_list:
  /* nil */
  | arg_expr
  | arg_expr ',' arg_list
  ;

expr_list:
  expr
  | expr ',' expr_list
  ;

shape_arguments:
  "FROM" name
  | "FROM" name shape_def
  | "FROM" "ARGUMENTS"
  | "FROM" "ARGUMENTS" shape_def
  ;

column_calculation:
  "COLUMNS" '(' col_calcs ')'
  | "COLUMNS" '(' "DISTINCT" col_calcs ')'
  ;

col_calcs:
  col_calc
  | col_calc ',' col_calcs
  ;

col_calc:
  name
  | shape_def
  | name shape_def
  | name '.' name
  ;

call_expr:
  expr
  | shape_arguments
  ;

call_expr_list:
  call_expr
  | call_expr ',' call_expr_list
  ;

cte_tables:
  cte_table
  | cte_table ',' cte_tables
  ;

cte_decl:
  name '(' name_list ')'
  | name '(' '*' ')'
  ;

shared_cte:
  call_stmt
  | call_stmt "USING" cte_binding_list
  ;

cte_table:
  cte_decl "AS" '(' select_stmt ')'
  | cte_decl "AS" '(' shared_cte')'
  | '(' call_stmt ')'
  | '(' call_stmt "USING" cte_binding_list ')'
  | cte_decl "LIKE" '(' select_stmt ')'
  | cte_decl "LIKE" name
  ;

with_prefix:
  "WITH" cte_tables
  | "WITH" "RECURSIVE" cte_tables
  ;

with_select_stmt:
  with_prefix select_stmt_no_with
  ;

select_nothing_stmt:
  "SELECT" "NOTHING"
  ;

select_stmt:
  with_select_stmt
  | select_stmt_no_with
  ;

select_stmt_no_with:
  select_core_list opt_orderby opt_limit opt_offset
  ;

select_core_list:
  select_core
  | select_core compound_operator select_core_list
  ;

values:
  '(' insert_list ')'
  | '(' insert_list ')' ',' values
  ;

select_core:
  "SELECT" select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window
  | "VALUES" values
  ;

compound_operator:
  "UNION"
  | "UNION ALL"
  | "INTERSECT"
  | "EXCEPT"
  ;

window_func_inv:
  name '(' arg_list ')' opt_filter_clause "OVER" window_name_or_defn
  ;

opt_filter_clause:
  /* nil */
  | "FILTER" '(' opt_where ')'
  ;

window_name_or_defn: window_defn
  | name
  ;

window_defn:
  '(' opt_partition_by opt_orderby opt_frame_spec ')'
  ;

opt_frame_spec:
  /* nil */
  | frame_type frame_boundary_opts frame_exclude
  ;

frame_type:
  "RANGE"
  | "ROWS"
  | "GROUPS"
  ;

frame_exclude:
  /* nil */
  | "EXCLUDE NO OTHERS"
  | "EXCLUDE CURRENT ROW"
  | "EXCLUDE GROUP"
  | "EXCLUDE TIES"
  ;

frame_boundary_opts:
  frame_boundary
  | "BETWEEN" frame_boundary_start "AND" frame_boundary_end
  ;

frame_boundary_start:
  "UNBOUNDED" "PRECEDING"
  | expr "PRECEDING"
  | "CURRENT ROW"
  | expr "FOLLOWING"
  ;

frame_boundary_end:
  expr "PRECEDING"
  | "CURRENT ROW"
  | expr "FOLLOWING"
  | "UNBOUNDED" "FOLLOWING"
  ;

frame_boundary:
  "UNBOUNDED" "PRECEDING"
  | expr "PRECEDING"
  | "CURRENT ROW"
  ;

opt_partition_by:
  /* nil */
  | "PARTITION" "BY" expr_list
  ;

opt_select_window:
  /* nil */
  | window_clause
  ;

window_clause:
  "WINDOW" window_name_defn_list
  ;

window_name_defn_list:
  window_name_defn
  | window_name_defn ',' window_name_defn_list
  ;

window_name_defn:
  name "AS" window_defn
  ;

region_spec:
    name
  | name "PRIVATE"
  ;

region_list:
  region_spec ',' region_list
  | region_spec
  ;

declare_schema_region_stmt:
  "@DECLARE_SCHEMA_REGION" name
  | "@DECLARE_SCHEMA_REGION" name "USING" region_list
  ;

declare_deployable_region_stmt:
  "@DECLARE_DEPLOYABLE_REGION"  name
  | "@DECLARE_DEPLOYABLE_REGION" name "USING" region_list
  ;

begin_schema_region_stmt:
  "@BEGIN_SCHEMA_REGION" name
  ;

end_schema_region_stmt:
  "@END_SCHEMA_REGION"
  ;

schema_unsub_stmt:
  "@UNSUB"  '(' name ')'
  ;

schema_ad_hoc_migration_stmt:
  "@SCHEMA_AD_HOC_MIGRATION" version_annotation
  | "@SCHEMA_AD_HOC_MIGRATION" "FOR" "@RECREATE" '(' name ',' name ')'
  ;

emit_enums_stmt:
  "@EMIT_ENUMS" opt_name_list
  ;

emit_group_stmt:
  "@EMIT_GROUP" opt_name_list
  ;

emit_constants_stmt:
  "@EMIT_CONSTANTS" name_list
  ;

opt_from_query_parts:
  /* nil */
  | "FROM" query_parts
  ;

opt_where:
  /* nil */
  | "WHERE" expr
  ;

opt_groupby:
  /* nil */
  | "GROUP" "BY" groupby_list
  ;

groupby_list:
  groupby_item
  | groupby_item ',' groupby_list
  ;

groupby_item:
  expr
  ;

opt_asc_desc:
  /* nil */
  | "ASC"  opt_nullsfirst_nullslast
  | "DESC"  opt_nullsfirst_nullslast
  ;

opt_nullsfirst_nullslast:
  /* nil */
  | "NULLS" "FIRST"
  | "NULLS" "LAST"
  ;

opt_having:
  /* nil */
  | "HAVING" expr
  ;

opt_orderby:
  /* nil */
  | "ORDER" "BY" orderby_list
  ;

orderby_list:
  orderby_item
  | orderby_item ',' orderby_list
  ;

orderby_item:
  expr opt_asc_desc
  ;

opt_limit:
  /* nil */
  | "LIMIT" expr
  ;

opt_offset:
  /* nil */
  | "OFFSET" expr
  ;

select_opts:
  /* nil */
  | "ALL"
  | "DISTINCT"
  | "DISTINCTROW"
  ;

select_expr_list:
  select_expr
  | select_expr ',' select_expr_list
  | '*'
  ;

select_expr:
  expr opt_as_alias
  | name '.' '*'
  | column_calculation
  ;

opt_as_alias:
  /* nil */
  | as_alias
  ;

as_alias:
  "AS" name
  | name
  ;

query_parts:
  table_or_subquery_list
  | join_clause
  ;

table_or_subquery_list:
  table_or_subquery
  | table_or_subquery ',' table_or_subquery_list
  ;

join_clause:
  table_or_subquery join_target_list
  ;

join_target_list:
  join_target
  | join_target join_target_list
  ;

table_or_subquery:
  name opt_as_alias
  | '(' select_stmt ')' opt_as_alias
  | '(' shared_cte ')' opt_as_alias
  | table_function opt_as_alias
  | '(' query_parts ')'
  ;

join_type:
  /*nil */
  | "LEFT"
  | "RIGHT"
  | "LEFT" "OUTER"
  | "RIGHT" "OUTER"
  | "INNER"
  | "CROSS"
  ;

join_target: join_type "JOIN" table_or_subquery opt_join_cond
  ;

opt_join_cond:
  /* nil */
  | join_cond
  ;

join_cond:
  "ON" expr
  | "USING" '(' name_list ')'
  ;

table_function:
  name '(' arg_list ')'
  ;

create_view_stmt:
  "CREATE" opt_temp "VIEW" opt_if_not_exists name "AS" select_stmt opt_delete_version_attr
  ;

with_delete_stmt:
  with_prefix delete_stmt
  ;

delete_stmt:
  "DELETE" "FROM" name opt_where
  ;

opt_insert_dummy_spec:
  /*nil*/
  | "@DUMMY_SEED" '(' expr ')' dummy_modifier
  ;

dummy_modifier:
  /* nil */
  | "@DUMMY_NULLABLES"
  | "@DUMMY_DEFAULTS"
  | "@DUMMY_NULLABLES" "@DUMMY_DEFAULTS"
  | "@DUMMY_DEFAULTS" "@DUMMY_NULLABLES"
  ;

insert_stmt_type:
  "INSERT" "INTO"
  | "INSERT" "OR" "REPLACE" "INTO"
  | "INSERT" "OR" "IGNORE" "INTO"
  | "INSERT" "OR" "ROLLBACK" "INTO"
  | "INSERT" "OR" "ABORT" "INTO"
  | "INSERT" "OR" "FAIL" "INTO"
  | "REPLACE" "INTO"
  ;

with_insert_stmt:
  with_prefix insert_stmt
  ;

opt_column_spec:
  /* nil */
  | '(' opt_name_list ')'
  | '(' shape_def ')'
  ;

from_shape:
  "FROM" "CURSOR" name opt_column_spec
  | "FROM" name opt_column_spec
  | "FROM" "ARGUMENTS" opt_column_spec
  ;

insert_stmt:
  insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec
  | insert_stmt_type name opt_column_spec from_shape opt_insert_dummy_spec
  | insert_stmt_type name "DEFAULT" "VALUES"
  | insert_stmt_type name "USING" select_stmt
  | insert_stmt_type name "USING" expr_names opt_insert_dummy_spec
  ;

insert_list_item:
  expr
  | shape_arguments
  ;

insert_list:
  /* nil */
  | insert_list_item
  | insert_list_item ',' insert_list
  ;

basic_update_stmt:
  "UPDATE" opt_name "SET" update_list opt_from_query_parts opt_where
  ;

with_update_stmt:
  with_prefix update_stmt
  ;

update_stmt:
  "UPDATE" name "SET" update_list opt_from_query_parts opt_where opt_orderby opt_limit
  ;

update_entry:
  name '=' expr
  ;

update_list:
  update_entry
  | update_entry ',' update_list
  ;

with_upsert_stmt:
  with_prefix upsert_stmt
  ;

upsert_stmt:
  insert_stmt "ON CONFLICT" conflict_target "DO" "NOTHING"
  | insert_stmt "ON CONFLICT" conflict_target "DO" basic_update_stmt
  ;

update_cursor_stmt:
  "UPDATE" "CURSOR" name opt_column_spec "FROM" "VALUES" '(' insert_list ')'
  | "UPDATE" "CURSOR" name opt_column_spec from_shape
  | "UPDATE" "CURSOR" name "USING" expr_names
  ;

conflict_target:
  /* nil */
  | '(' indexed_columns ')' opt_where
  ;

function: "FUNC" | "FUNCTION"
  ;

declare_out_call_stmt:
  "DECLARE" "OUT" call_stmt
  ;

declare_enum_stmt:
  "DECLARE" "ENUM" name data_type_numeric '(' enum_values ')'
  ;

enum_values:
    enum_value
  | enum_value ',' enum_values
  ;

enum_value:
    name
  | name '=' expr
  ;

declare_const_stmt:
  "DECLARE" "CONST" "GROUP" name '(' const_values ')'
  ;

declare_group_stmt:
  "DECLARE" "GROUP" name "BEGIN" simple_variable_decls "END"
  ;

simple_variable_decls:
  declare_vars_stmt ';'
  | declare_vars_stmt ';' simple_variable_decls
  ;

const_values:
   const_value
  | const_value ',' const_values
  ;

const_value:  name '=' expr
  ;

declare_select_func_no_check_stmt:
  "DECLARE" "SELECT" function name "NO" "CHECK" data_type_with_options
  | "DECLARE" "SELECT" function name "NO" "CHECK" '(' typed_names ')'
  ;

declare_func_stmt:
  "DECLARE" function name '(' func_params ')' data_type_with_options
  | "DECLARE" "SELECT" function name '(' params ')' data_type_with_options
  | "DECLARE" function name '(' func_params ')' "CREATE" data_type_with_options
  | "DECLARE" "SELECT" function name '(' params ')' '(' typed_names ')'
  ;

procedure: "PROC" | "PROCEDURE"
  ;

declare_proc_no_check_stmt:
  "DECLARE" procedure name "NO" "CHECK"
  ;

declare_proc_stmt:
  "DECLARE" procedure name '(' params ')'
  | "DECLARE" procedure name '(' params ')' '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "USING" "TRANSACTION"
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')' "USING" "TRANSACTION"
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')' "USING" "TRANSACTION"
  ;

declare_interface_stmt:
  "DECLARE" "INTERFACE" name '(' typed_names ')'
  | "INTERFACE" name '(' typed_names ')'
  ;

create_proc_stmt:
  "CREATE" procedure name '(' params ')' "BEGIN" opt_stmt_list "END"
  ;

inout:
  "IN"
  | "OUT"
  | "INOUT"
  ;

typed_name:
  name data_type_with_options
  | shape_def
  | name shape_def
  ;

typed_names:
  typed_name
  | typed_name ',' typed_names
  ;

func_param:
  param
  | name "CURSOR"
  ;

func_params:
  /* nil */
  | func_param
  |  func_param ',' func_params
  ;

param:
  name data_type_with_options
  | inout name data_type_with_options
  | shape_def
  | name shape_def
  ;

params:
  /* nil */
  | param
  |  param ',' params
  ;

declare_value_cursor:
  "DECLARE" name "CURSOR" shape_def
  | "CURSOR" name shape_def
  | "DECLARE" name "CURSOR" "LIKE" select_stmt
  | "CURSOR" name "LIKE" select_stmt
  | "DECLARE" name "CURSOR" "LIKE" '(' typed_names ')'
  | "CURSOR" name "LIKE" '(' typed_names ')'
  ;

declare_forward_read_cursor_stmt:
  "DECLARE" name "CURSOR" "FOR" select_stmt
  | "CURSOR" name "FOR" select_stmt
  | "DECLARE" name "CURSOR" "FOR" explain_stmt
  | "CURSOR" name "FOR" explain_stmt
  | "DECLARE" name "CURSOR" "FOR" call_stmt
  | "CURSOR" name "FOR" call_stmt
  | "DECLARE" name "CURSOR" "FOR" expr
  | "CURSOR" name "FOR" expr
  ;

declare_fetched_value_cursor_stmt:
  "DECLARE" name "CURSOR" "FETCH" "FROM" call_stmt
  | "CURSOR" name "FETCH" "FROM" call_stmt
  ;

declare_type_stmt:
  "DECLARE" name "TYPE" data_type_with_options
  | "TYPE" name data_type_with_options
  ;

declare_vars_stmt:
  "DECLARE" name_list data_type_with_options
  | "VAR" name_list data_type_with_options
  | declare_value_cursor
  ;

call_stmt:
  "CALL" name '(' ')'
  | "CALL" name '(' call_expr_list ')'
  | "CALL" name '(' '*' ')'
  ;

while_stmt:
  "WHILE" expr "BEGIN" opt_stmt_list "END"
  ;

switch_stmt:
  "SWITCH" expr switch_case switch_cases
  | "SWITCH" expr "ALL" "VALUES" switch_case switch_cases
  ;

switch_case:
  "WHEN" expr_list "THEN" stmt_list
  | "WHEN" expr_list "THEN" "NOTHING"
  ;

switch_cases:
  switch_case switch_cases
  | "ELSE" stmt_list "END"
  | "END"
  ;

loop_stmt:
  "LOOP" fetch_stmt "BEGIN" opt_stmt_list "END"
  ;

leave_stmt:
  "LEAVE"
  ;

return_stmt:
  "RETURN"
  ;

rollback_return_stmt:
  "ROLLBACK" "RETURN"
  ;

commit_return_stmt:
  "COMMIT" "RETURN"
  ;

throw_stmt:
  "THROW"
  ;

trycatch_stmt:
  "BEGIN" "TRY" opt_stmt_list "END" "TRY" ';' "BEGIN" "CATCH" opt_stmt_list "END" "CATCH"
  ;

continue_stmt:
  "CONTINUE"
  ;

fetch_stmt:
  "FETCH" name "INTO" name_list
  | "FETCH" name
  ;

fetch_cursor_from_blob_stmt:
  "FETCH" name "FROM BLOB" expr
  ;

fetch_values_stmt:
  "FETCH" name opt_column_spec "FROM" "VALUES" '(' insert_list ')' opt_insert_dummy_spec
  | "FETCH" name opt_column_spec from_shape opt_insert_dummy_spec
  | "FETCH" name "USING" expr_names opt_insert_dummy_spec
  ;

expr_names:
  expr_name
  |  expr_name ',' expr_names
  ;

expr_name: expr as_alias
  ;

fetch_call_stmt:
  "FETCH" name opt_column_spec "FROM" call_stmt
  ;

close_stmt:
  "CLOSE" name
  ;

out_stmt:
  "OUT" name
  ;

out_union_stmt:
  "OUT" "UNION" name
  ;

out_union_parent_child_stmt:
  "OUT" "UNION" call_stmt "JOIN" child_results
  ;

child_results:
   child_result
   | child_result "AND" child_results
   ;

child_result:
  call_stmt "USING" '(' name_list ')'
  | call_stmt "USING" '(' name_list ')' "AS" name
  ;

if_stmt:
  "IF" expr "THEN" opt_stmt_list opt_elseif_list opt_else "END" "IF"
  ;

opt_else:
  /* nil */
  | "ELSE" opt_stmt_list
  ;

elseif_item:
  "ELSE IF" expr "THEN" opt_stmt_list
  ;

elseif_list:
  elseif_item
  | elseif_item elseif_list
  ;

opt_elseif_list:
  /* nil */
  | elseif_list
  ;

control_stmt:
  commit_return_stmt
  | continue_stmt
  | leave_stmt
  | return_stmt
  | rollback_return_stmt
  | throw_stmt

guard_stmt:
  "IF" expr control_stmt
  ;

transaction_mode:
  /* nil */
  | "DEFERRED"
  | "IMMEDIATE"
  | "EXCLUSIVE"
  ;

begin_trans_stmt:
  "BEGIN" transaction_mode "TRANSACTION"
  | "BEGIN" transaction_mode
  ;

rollback_trans_stmt:
  "ROLLBACK"
  | "ROLLBACK" "TRANSACTION"
  | "ROLLBACK" "TO" savepoint_name
  | "ROLLBACK" "TRANSACTION" "TO" savepoint_name
  | "ROLLBACK" "TO" "SAVEPOINT" savepoint_name
  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" savepoint_name
  ;

commit_trans_stmt:
  "COMMIT" "TRANSACTION"
  | "COMMIT"
  ;

proc_savepoint_stmt:  procedure "SAVEPOINT" "BEGIN" opt_stmt_list "END"
  ;

savepoint_name:
  "@PROC"
  | name
  ;

savepoint_stmt:
  "SAVEPOINT" savepoint_name
  ;

release_savepoint_stmt:
  "RELEASE" savepoint_name
  | "RELEASE" "SAVEPOINT" savepoint_name
  ;

echo_stmt:
  "@ECHO" name ',' str_literal
  ;

alter_table_add_column_stmt:
  "ALTER" "TABLE" name "ADD" "COLUMN" col_def
  ;

create_trigger_stmt:
  "CREATE" opt_temp "TRIGGER" opt_if_not_exists trigger_def opt_delete_version_attr
  ;

trigger_def:
  name trigger_condition trigger_operation "ON" name trigger_action
  ;

trigger_condition:
  /* nil */
  | "BEFORE"
  | "AFTER"
  | "INSTEAD" "OF"
 ;

trigger_operation:
  "DELETE"
  | "INSERT"
  | "UPDATE" opt_of
  ;

opt_of:
  /* nil */
  | "OF" name_list
  ;

trigger_action:
  opt_foreachrow opt_when_expr "BEGIN" trigger_stmts "END"
  ;

opt_foreachrow:
  /* nil */
  | "FOR EACH ROW"
  ;

opt_when_expr:
  /* nil */
  | "WHEN" expr
  ;

trigger_stmts:
  trigger_stmt
  | trigger_stmt  trigger_stmts
  ;

trigger_stmt:
  trigger_update_stmt ';'
  | trigger_insert_stmt ';'
  | trigger_delete_stmt ';'
  | trigger_select_stmt ';'
  ;

trigger_select_stmt:
  select_stmt_no_with
  ;

trigger_insert_stmt:
  insert_stmt
  ;

trigger_delete_stmt:
  delete_stmt
  ;

trigger_update_stmt:
  basic_update_stmt
  ;

enforcement_options:
  "FOREIGN" "KEY" "ON" "UPDATE"
  | "FOREIGN" "KEY" "ON" "DELETE"
  | "JOIN"
  | "UPSERT" "STATEMENT"
  | "WINDOW" function
  | "WITHOUT" "ROWID"
  | "TRANSACTION"
  | "SELECT" "IF" "NOTHING"
  | "INSERT" "SELECT"
  | "TABLE" "FUNCTION"
  | "ENCODE" "CONTEXT COLUMN"
  | "ENCODE" "CONTEXT TYPE" "INTEGER"
  | "ENCODE" "CONTEXT TYPE" "LONG_INTEGER"
  | "ENCODE" "CONTEXT TYPE" "REAL"
  | "ENCODE" "CONTEXT TYPE" "BOOL"
  | "ENCODE" "CONTEXT TYPE" "TEXT"
  | "ENCODE" "CONTEXT TYPE" "BLOB"
  | "IS TRUE"
  | "CAST"
  | "SIGN FUNCTION"
  | "CURSOR HAS ROW"
  | "UPDATE" "FROM"
  ;

enforce_strict_stmt:
  "@ENFORCE_STRICT" enforcement_options
  ;

enforce_normal_stmt:
  "@ENFORCE_NORMAL" enforcement_options
  ;

enforce_reset_stmt:
  "@ENFORCE_RESET"
  ;

enforce_push_stmt:
  "@ENFORCE_PUSH"
  ;

enforce_pop_stmt:
  "@ENFORCE_POP"
  ;

opt_use_offset:
  /* nil */
  | "OFFSET"
  ;

blob_get_key_type_stmt:
  "@BLOB_GET_KEY_TYPE" name
  ;

blob_get_val_type_stmt:
  "@BLOB_GET_VAL_TYPE" name
  ;

blob_get_key_stmt:
  "@BLOB_GET_KEY" name opt_use_offset
  ;

blob_get_val_stmt:
  "@BLOB_GET_VAL" name opt_use_offset
  ;

blob_create_key_stmt:
  "@BLOB_CREATE_KEY" name opt_use_offset
  ;

blob_create_val_stmt:
  "@BLOB_CREATE_VAL" name opt_use_offset
  ;

blob_update_key_stmt:
  "@BLOB_UPDATE_KEY" name opt_use_offset
  ;

blob_update_val_stmt:
  "@BLOB_UPDATE_VAL" name opt_use_offset
  ;

```



## Appendix 3: Control Directives
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
The control directives are those statements that begin with `@` and they are distinguished from other statements because they influence the compiler rather than the program logic.  Some of these are of great importance and discussed elsewhere.

The complete list (as of this writing) is:

`@ENFORCE_STRICT`
`@ENFORCE_NORMAL`

* These enable or disable more strict semanic checking the sub options are
  * `FOREIGN KEY ON UPDATE`: all FK's must choose some `ON UPDATE` strategy
  * `FOREIGN KEY ON DELETE`: all FK's must choose some `ON DELETE` strategy
  * `PROCEDURE`: all procedures must be declared before they are called (eliminating the vanilla `C` call option)
  * `JOIN`: all joins must be ANSI style, the form `FROM A,B` is not allowed (replace with `A INNER JOIN B`
  * `WINDOW FUNC`: window functions are disallowed (useful if targeting old versions of SQLite)
  * `UPSERT STATEMENT`: the upsert form is disallowed (useful if targeting old versions of SQLite)

`@SENSITIVE`
 * marks a column or variable as 'sensitive' for privacy purposes, this behaves somewhat like nullability (See Chapter 3) in that it is radioactive, contaminating anything it touches
 * the intent of this annotation is to make it clear where sensitive data is being returned or consumed in your procedures
 * this information appears in the JSON output for further codegen or for analysis (See Chapter 13)

`@DECLARE_SCHEMA_REGION`
`@DECLARE_DEPLOYABLE_REGION`
`@BEGIN_SCHEMA_REGION`
`@END_SCHEMA_REGION`

 * These directives control the declaration of schema regions and allow you to place things into those regions -- see [Chapter 10](https://cgsql.dev/cql-guide/ch10)

`@SCHEMA_AD_HOC_MIGRATION`
 * Allows for the creation of a ad hoc migration step at a given schema version, (See Chapter 10)

`@ECHO`
 * Emits text into the C output stream, useful for emiting things like function prototypes or preprocessor directives
 * e.g. `echo C, '#define foo bar'

`@RECREATE`
`@CREATE`
`@DELETE`
  * used to mark the schema version where an object is created or deleted, or alternatively indicate the the object is always dropped and recreated when it changes (See Chapter 10)

`@SCHEMA_UPGRADE_VERSION`
 * used to indicate that the code that follows is part of a migration script for the indicated schema version
 * this has the effect of making the schema appear to be how it existed at the indicated version
 * the idea here is that migration procedures operate on previous versions of the schema where (e.g.) some columns/tables hadn't been deleted yet

`@PREVIOUS_SCHEMA`
 * indicates the start of the previous version of the schema for comparison (See Chapter 11)

`@SCHEMA_UPGRADE_SCRIPT`
 * CQL emits a schema upgrade script as part of its upgrade features, this script declares tables in their final form but also creates the same tables as they existed when they were first created
 * this directive instructs CQL to ignore the incompatible creations, the first declaration controls
 * the idea here is that the upgrade script is in the business of getting you to the finish line in an orderly fashion and some of the interim steps are just not all the way there yet
 * note that the upgrade script recapitulates the version history, it does not take you directly to the finish line, this is so that all instances get to the same place the same way (and this fleshes out any bugs in migration)

`@DUMMY_NULLABLES`
`@DUMMY_DEFAULTS`
`@DUMMY_SEED`
 * these control the creation of dummy data for `insert` and `fetch` statements (See Chapters 5 and 12)

`@FILE`
 * a string literal that corresponds to the current file name with a prefix stripped (to remove build lab junk in the path)

`@ATTRIBUTE`
  * the main purpose of `@attribute` is to appear in the JSON output so that it can control later codegen stages in whatever way you deem appropriate
  * the nested nature of attribute values is sufficiently flexible than you could encode an arbitrary LISP program in an attribute, so really anything you might need to express is possible
  * there are a number of attributes known to the compiler which I list below (complete as of this writing)

  * `cql:autodrop=(table1, table2, ...)` when present the indicated tables, which must be temp tables, are dropped when the results of the procedure have been fetched into a rowset
  * `cql:identity=(column1, column2, ...)` the indicated columns are used to create a row comparator for the rowset corresponding to the procedure, this appears in a C macro of the form `procedure_name_row_same(rowset1, row1, rowset2, row2)`
  * `cql:suppress_getters` the annotated procedure should not emit its related column getter functions.
    * Useful if you only indend to call the procedure from CQL.
    * Saves code generation and removes the possibility of C code using the getters.
  * `cql:suppress_result_set` the annotated procedure should not emit its related "fetch results" function.
    * Useful if you only indend to call the procedure from CQL.
    * Saves code generation and removes the possibility of C code using the result set or getters.
    * Implies `cql:suppress_getters`; since there is no result set, getters would be redundant.
    * Note: an `OUT UNION` procedure cannot have a suppressed result set since all such a procedure does is produce a result set. This attribute is ignored for out union procedures.
  * `cql:private` the annotated procedure will be static in the generated C
    * Because the generated function is `static` it cannot be called from other modules and therefore will not go in any CQL exports file (that would be moot since you couldn't call it).
    * This attribute also implies `cql:suppress_result_set` since only CQL code in the same translation unit could possibly call it and hence the result set procedure is useless to other C code.
  * `cql:generate_copy` the code generation for the annotated procedure will produce a `[procedure_name]_copy` function that can make complete or partial copies of its result set.
  * `cql:base_fragment=frag_name` used for base fragments (See [Chapter 14](https://cgsql.dev/cql-guide/ch14#base-query-fragments))
  * `cql:extension_fragment=frag_name` used for extension fragments (See [Chapter 14](https://cgsql.dev/cql-guide/ch14#extension-query-fragments))
  * `cql:assembly_fragment=frag_name` used for assembly fragments (See [Chapter 14](https://cgsql.dev/cql-guide/ch14#extension-query-fragments))
  * `cql:shared_fragment` is used to create shared fragments (See [Chapter 14](https://cgsql.dev/cql-guide/ch14#shared-fragments))
  * `cql:no_table_scan` for query plan processing, indicates that the table in question should never be table scanned in any plan (for better diagnostics)
  * `cql:autotest=([many forms])` declares various autotest features (See Chapter 12)
  * `cql:query_plan_branch=[integer]` is used by the query plan generator to determine which conditional branch to use in query plan analysis when a shared fragment that contains an `IF` statement is used. (See [Chapter 15](/cql-guide/ch15))
  * `cql:alias_of=[c_function_name]` are used on [function declarations](https://cgsql.dev/cql-guide/ch08/#ordinary-scalar-functions) to declare a function or procedure in CQL that calls a function of a different name. This is intended to used for aliasing native (C) functions. Both the aliased function name and the original function name may be declared in CQL at the same time. Note that the compiler does not enforce any consistency in typing between the original and aliased functions.



## Appendix 4: CQL Error Codes
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### CQL0001: operands must be an integer type, not real

integer math operators like << >> & and | are not compatible with real-valued arguments

-----

### CQL0002: left operand cannot be an object in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on objects.  Basically comparison is all you can do.

-----

### CQL0003: left operand cannot be an object in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on objects.  Basically comparison is all you can do.

-----

### CQL0004: left operand cannot be a blob in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on blobs.  Basically comparison is all you can do.

-----

### CQL0005: right operand cannot be a blob in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on blobs.  Basically comparison is all you can do.

-----

### CQL0007: left operand cannot be a string in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on strings.  Basically comparison is all you can do.

-----

### CQL0008: right operand cannot be a string in 'operator'

Most arithmetic operators (e.g. +, -, *) do not work on strings.  Basically comparison is all you can do.

-----

### CQL0009: incompatible types in expression 'subject'

The expression type indicated by subject required a TEXT as the next item and found something else.
This could be a binary operator, part of a CASE expression, the parts of an IN expression or any other
place where several expressions might need to be compatible with each other.

-----

### CQL0010: incompatible types in expression 'subject'

The expression type indicated by subject required an OBJECT as the next item and found something else.
This could be a binary operator, part of a CASE expression, the parts of an IN expression or any other
place where several expressions might need to be compatible with each other.

-----

### CQL0011: incompatible types in expression 'subject'

The expression type indicated by subject required a BLOB as the next item and found something else.
This could be a binary operator, part of a CASE expression, the parts of an IN expression or any other
place where several expressions might need to be compatible with each other.

-----

### CQL0012: incompatible types in expression 'subject'

The expression type indicated by subject required a numeric as the next item and found something else.
This could be a binary operator, part of a CASE expression, the parts of an IN expression or any other
place where several expressions might need to be compatible with each other.

-----

### CQL0013: cannot assign/copy possibly null expression to not null target 'target'

Here assign/copy can be the simplest case of assigning to a local variable or an OUT parameter but this error also appears when calling functions.  You should think of the IN arguments as requiring that the actual argument be assignable to the formal variable and OUT arguments requiring that the formal be assignable to the actual argument variable.

------

### CQL0014: cannot assign/copy sensitive expression to not null target 'target'

Here assign/copy can be the simplest case of assigning to a local variable or an OUT parameter but this error also appears when calling functions.  You should think of the IN arguments as requiring that the actual argument be assignable to the formal variable and OUT arguments requiring that the formal be assignable to the actual argument variable.

------

### CQL0015: expected numeric expression 'context'

Many SQL clauses require a numeric expression such as WHERE/HAVING/LIMIT/OFFSET.  This expression indicates the expression in the given context is not a numeric.

------

### CQL0016: duplicate table name in join 'table'

When this error is produced it means the result of the join would have the same table twice with no disambiguation between the two places.  The conflicting name is provided.  To fix this, make an alias both tables.
e.g.
```sql
SELECT T1.id AS parent_id, T2.id AS child_id
  FROM foo AS T1
  INNER JOIN foo AS T2 ON T1.id = T2.parent_id;
```
-----

### CQL0017: index was present but now it does not exist (use `@delete` instead) 'index'

The named index is in the previous schema bit it is not in the current schema.  All entities need some kind of tombstone in the  schema so that they can be correctly deleted if they are still present.

-----

### CQL0018: duplicate index name 'index'

An index with the indicated name already exists.

-----

### CQL0019: create index table name not found 'table_name'

The table part of a CREATE INDEX statement was not a valid table name.

------

### CQL0020: duplicate constraint name in table 'constraint_name'

A table contains two constraints with the same name.

------

### CQL0021: foreign key refers to non-existent table 'table_name'

The table in a foreign key REFERENCES clause is not a valid table.

------

### CQL0022: exact type of both sides of a foreign key must match (expected expected_type; found actual_type) 'key_name'

The indicated foreign key has at least one column with a different type than corresponding column in the table it references.
This usually means that you have picked the wrong table or column in the foreign key declaration.

-----

### CQL0023: number of columns on both sides of a foreign key must match

The number of column in the foreign key must be the same as the number of columns specified in the foreign table.
This usually means a column is missing in the REFERENCES part of the declaration.

-----

CQL0024: no longer in use

-----

### CQL0025: version number in annotation must be positive

In an `@create` or `@delete` annotation, the version number must be > 0.
This error usually means there is a typo in the version number.

-----

### CQL0026: duplicate version annotation

There can only be one `@create`, `@delete`, or `@recreate` annotation for any given table/column.  More than one `@create` is redundant.
This error usually means the `@create` was cut/paste to make an `@delete` and then not edited or something like that.

-----

### CQL0027: a procedure can appear in only one annotation 'procedure_name'

The indicated migration procedure e.g. the foo in `@create(5, foo)` appears in another annotation.  Migration steps should happen exactly once.
This probably means the annotation was cut/paste and the migration proc was not removed.

-----

### CQL0028: FK reference must be exactly one column with the correct type 'column_name'

When a foreign key is specified in the column definition it is the entire foreign key.  That means the references part of the declaration can only be for that one column.
If you need more columns, you have to declare the foreign key independently.

-----

### CQL0029: autoincrement column must be [LONG_]INTEGER PRIMARY KEY 'column name'

SQLite is very fussy about autoincrement columns.  The column in question must be either a LONG INTEGER or an INTEGER and it must be PRIMARY KEY.
In fact, CQL will rewrite LONG INTEGER into INTEGER because only that exact form is supported, but SQLite INTEGERs can hold LONG values so that's ok.
Any other autoincrement form results in this error.

----

### CQL0030: a column attribute was specified twice on the same column 'column_name'

This error indicates a pattern like "id text not null not null" was found.
The same attribute shouldn't appear twice.

-----

### CQL0031: column can't be primary key and also unique key 'column'

In a column definition, the column can only be marked with at most one of PRIMARY KEY or UNIQUE

-----

### CQL0032: created columns must be at the end and must be in version order", 'column'

The SQLite ALTER TABLE ADD COLUMN statement is used to add new columns to the schema.  This statement puts the columns at the end of the table.
In order to make the CQL schema align as closely as possible to the actual sqlite schema you will get you are required to add
columns where SQLite will put them.  This will help a lot if you ever connect to such a database and start doing `select * from <somewhere with creates>`

-----

### CQL0033: columns in a table marked @recreate cannot have @create or `@delete`, 'column'

If the table is using the `@recreate` plan then you can add and remove columns (and other things freely)  you don't need to mark columns with `@create` or `@delete` just add/remove them.
This error prevents the build up of useless annotations.

-----

### CQL0034: create/delete version numbers can only be applied to columns that are nullable or have a default value 'column'

Any new column added to a schema must have a default value or be nullable so that its initial state is clear
and so that all existing insert statements do not have to be updated to include it.  Either make the column
nullable or give it a default value.

Similarly, any column being deleted must be nullable or have a default value.  The column can't actually be deleted
(not all versions of SQLite support this) so it will only be "deprecated".  But if the column is not null and has no default
then it would be impossible to write a correct insert statement for the table with the deleted column.

As a consequence you can neither add nor remove columns that are not null and have no default.

-----

### CQL0035: column delete version can't be <= column create version", 'column'

You can't `@delete` a column in a version before it was even created.  Probably there is a typo in one or both of the versions.

-----

### CQL0036: column delete version can't be <= the table create version 'column'

The indicated column is being deleted in a version that is before the table it is found in was even created.  Probably there is a typo in the delete version.

-----

### CQL0037: column delete version can't be >= the table delete version

The indicated column is being deleted in a version that is after the table has already been deleted.  This would be redundant.  Probably one or both have a typo in their delete version.

-----

### CQL0038: column create version can't be `<=` the table create version 'column'

The indicated column is being created in a version that is before the table it is found in was even created.  Probably there is a typo in the delete version.

-----

### CQL0039: column create version can't be `>=` the table delete version 'column'

The indicated column is being created in a version that that is after it has already been supposedly deleted.  Probably there is a typo in one or both of the version numbers.

-----

### CQL0040: table can only have one autoinc column 'column'

The indicated column is the second column to be marked with AUTOINCREMENT in its table.  There can only be one such column.

-----

### CQL0041: tables cannot have object columns 'column'

The OBJECT data type is only for use in parameters and local variables.  SQLite has no storage for object references.
The valid data types include `INTEGER`, `LONG INTEGER`, `REAL`, `BOOL`, `TEXT`, `BLOB`

-----

### CQL0042: left operand must be a string in 'LIKE/MATCH/GLOB'

The indicated operator can only be used to compare two strings.

-----

### CQL0043: right operand must be a string in 'LIKE/MATCH/GLOB'

The indicated operator can only be used to compare two strings.

-----

### CQL0044: operator may only appear in the context of a SQL statement 'MATCH'

The MATCH operator is a complex sqlite primitive.  It can only appear within SQL expressions.
See the CQL documentation about it being a two-headed-beast when it comes to expression evaluation.

-----

### CQL0045: blob operand not allowed in 'operator'

None of the unary math operators e.g. '-' and '~' allow blobs as an operand.

-----

### CQL0046: object operand not allowed in 'operator'

None of the unary math operators e.g. '-' and '~' allow objects as an operand.

-----

### CQL0047: string operand not allowed in 'operator'

None of the unary math operators e.g. '-' and '~' allow strings as an operand.

-----

### CQL0051: argument can only be used in count(*) '*'

The '*' special operator can only appear in the COUNT function.
e.g. `select count(*) from some_table`
It is not a valid function argument in any other context.

-----

### CQL0052: select * cannot be used with no FROM clause

Select statements of the form `select 1, 'foo';` are `valid but select '*';` is not.
The `*` shortcut for columns only makes sense if there is something to select from.
e.g. `select * from some_table;` is valid.

-----

### CQL0053: select [table].* cannot be used with no FROM clause

Select statements of the form `select 1, 'foo';` are `valid but select 'T.*';` is not.
The `T.*` shortcut for all the columns from table T only makes sense if there is
something to select form.
e.g. `select T.* from some_table T;` is valid.

-----

### CQL0054: table not found 'table'

The indicated table was used in a select statement like `select T.* from ...` but no such table was present in the `FROM` clause.

-----

### CQL0055: all columns in the select must have a name

Referring to the select statement on the failing line, that select statement was used in a context where all the columns must have a name.
Examples include defining a view, a cursor, or creating a result set from a procedure.  The failing code might look something like this.
`select 1, 2 B;`  it needs to look like this `select 1 A, 2 B`;

-----

### CQL0056: NULL expression has no type to imply a needed type 'variable'

In some contexts the type of a constant is used to imply the type of the expression.  The NULL literal cannot be used in such contexts
because it has no specific type.

In a SELECT statement the NULL literal has no type.  If the type of the column cannot be inferred then it must be declared specifically.

In a  LET statement, the same situation arises  `LET x := NULL;`  doesn't specify what type 'x' is to be.

You can fix this error by changing the `NULL` to something like `CAST(NULL as TEXT)`.

A common place this problem happens is in defining a view or returning a result set from a stored procedure.  In those cases
all the columns must have a name and a type.

-----

### CQL0057: if multiple selects, all must have the same column count

If a stored procedure might return one of several result sets, each of the select statements it might return must have the same number of columns.
Likewise, if several select results are being combined with `UNION` or `UNION ALL` they must all have the same number of columns.

------

### CQL0058: if multiple selects, all column names must be identical so they have unambiguous names; error in column N: 'X' vs. 'Y'


If a stored procedure might return one of several result sets, each of the select statements must have the same column names for its result.
Likewise, if several select results are being combined with `UNION` or `UNION ALL` they must all have the same column names.

This is important so that there can be one unambiguous column name for every column for group of select statements.

e.g.
```sql
select 1 A, 2 B
union
select 3 A, 4 C;
```
Would provoke this error.  In this case the error would report that the problem was in column 2 and that error was 'B' vs. 'C'

-----

### CQL0059: a variable name might be ambiguous with a column name, this is an anti-pattern 'name'

The referenced name is the name of a local or a global in the same scope as the name of a column.  This can lead to surprising results as it is not clear which name takes priority (previously the variable did rather than the column, now it's an error).

example:

```sql
create proc foo(id integer)
begin
  -- this is now an error, in all cases the argument would have been selected
  select id from bar T1 where T1.id != id;
end;
```

To correct this, rename the local/global.  Or else pick a more distinctive column name, but usually the local is the problem.

-----

### CQL0060: referenced table can be independently recreated so it cannot be used in a foreign key, 'referenced_table'

The referenced table is marked recreate so it must be in the same recreate
group as the current table or in a recreate group that does not introduce a cyclic foreign key
dependency among recreate groups. Otherwise, the referenced table might be recreated away leaving all the
foreign key references in current table as orphans.

So we check the following:
If the referenced table is marked recreate then any of the following result in CQL0060

 * the containing table is not recreate at all (non-recreate table can't reference recreate tables at all), OR
 * the new foreign key dependency between the referenced table and the current table introduces a cycle

The referenced table is a recreate table and one of the 4 above conditions was not met.  Either don't reference it or
else put the current table and the referenced table into the same recreate group.

----

### CQL0061: if multiple selects, all columns must be an exact type match (expected expected_type; found actual_type) 'column'

In a stored proc with multiple possible selects providing the result, all of the columns of all the selects must be an exact type match.

e.g.

```sql
if x then
  select 1 A, 2 B
else
  select 3 A, 4.0 B;
end if;
```

Would provoke this error.  In this case 'B' would be regarded as the offending column and the error is reported on the second B.

----

### CQL0062: if multiple selects, all columns must be an exact type match (including nullability) (expected expected_type; found actual_type) 'column'

In a stored proc with multiple possible selects providing the result, all of the columns of all the selects must be an exact type match.
This error indicates that the specified column differs by nullability.

-----

### CQL0063: can't mix and match out statement with select/call for return values 'procedure_name'

If the procedure is using SELECT to create a result set it cannot also use the OUT keyword
to create a one-row result set.

-----

### CQL0064: object variables may not appear in the context of a SQL statement

SQLite doesn't understand object references, so that means you cannot try to use a variable or parameter of type object inside of a SQL statement.

e.g.
```sql
create proc foo(X object)
begin
  select X is null;
end;
```

In this example X is an object parameter, but even to use X for an `is null` check in a select statement would require binding an object which is not possible.

On the other hand this compiles fine.

```sql
create proc foo(X object, out is_null bool not null)
begin
  set is_null := X is null;
end;
```

This is another example of XQL being a two-headed beast.

-----

### CQL0065: identifier is ambiguous 'name'

There is more than one variable/column with indicated name in equally near scopes.  The most common reason for this is that there are two column in a join with the same name and that name has not been qualified elsewhere.

e.g.
```sql
SELECT A
  FROM (SELECT 1 AS A, 2 AS B) AS T1
  INNER JOIN (SELECT 1 AS A, 2 AS B) AS T2;
```
There are two possible columns named `A`.  Fix this by using `T1.A` or `T2.A`.

----

### CQL0066: if a table is marked `@recreate`, its indices must be in its schema region 'index_name'

If a table is marked `@recreate` that means that when it changes it is dropped and created during schema maintenance.  Of course when it is dropped its indices are also dropped.  So the indices must also be recreated when the table changes.  So with such a table it makes no sense to have indices that are in a different schema region.  This can only work if they are all always visible together.

Tables on the `@create` plan are not dropped so their indices can be maintained separately.  So they get a little extra flexibility.

To fix this error move the offending index into the same schema region as the table.  And probably put them physically close for maintenance sanity.

----

### CQL0067: cursor was not used with 'fetch [cursor]'  'cursor_name'

The code is trying to access fields in the named cursor but the automatic field generation form was not used so there are no such fields.

e.g.

```sql
declare a integer;
declare b integer;
declare C cursor for select 1 A, 2 B;
fetch C into a, b; -- C.A and C.B not created (!)
if (C.A) then -- error
  ...
end if;
```

Correct usage looks like this:

```sql
declare C cursor for select 1 A, 2 B;
fetch C;  -- automatically creates C.A and C.B
if (C.A) then
  ...
end if;
```

-----

### CQL0068: field not found in cursor 'field'

The indicated field is not a valid field in a cursor expression.

e.g.

```sql
declare C cursor for select 1 A, 2 B;
fetch C;  -- automatically creates C.A and C.B
if (C.X) then -- C has A and B, but no X
  ...
end if;
```

----

### CQL0069: name not found 'name'

The indicated name could not be resolved in the scope in which it appears.
Probably there is a typo.  But maybe the name you need isn't available in
the scope you are trying to use it in.

----

### CQL0070: incompatible object type 'incompatible_type'

Two expressions of type object are holding a different object type e.g.

```sql
declare x object<Foo>;
declare y object<Bar>;
set x := y;
```

Here the message would report that 'Bar' is incompatible. The message generally
refers to the 2nd object type as the first one was ok by default then the
second one caused the problem.

-----

### CQL0071: first operand cannot be a blob in 'BETWEEN/NOT BETWEEN'

The BETWEEN operator works on numerics and strings only.

-----

### CQL0072: first operand cannot be a blob in 'BETWEEN/NOT BETWEEN'

The BETWEEN operator works on numerics and strings only.

-----

### CQL0073: CAST may only appear in the context of SQL statement

The CAST function does highly complex and subtle conversions, including date/time functions and other things.  It's not possibly to emulate this accurately and there is no sqlite helper to do the job directly from a C call.  Consequently it's only supported in the context of CQL statements.  It can be used in normal expressions by using the nested `SELECT` form `(select ...)`

-----

### CQL0074: too few arguments provided 'coalesce'

There must be at least two arguments in a call to `coalesce`.

-----

### CQL0075: incorrect number of arguments 'ifnull'

The  `ifnull` function requires exactly two arguments.

-----

### CQL0076: NULL literal is useless in function 'ifnull/coalesce'

Adding a NULL literal to `IFNULL` or `COALESCE` is a no-op.  It's most likely an error.

-----

### CQL0077: encountered arg known to be not null before the end of the list, rendering the rest useless 'expression'

In an `IFNULL` or `COALESCE` call, only the last argument may be known to be not null.  If a not null argument comes earlier in the list, then none of the others could ever be used.  That is almost certainly an error.  The
most egregious form of this error is if the first argument is known to be not null in which case the entire
`IFNULL` or `COALESCE` can be removed.

-----

### CQL0078: [not] in (select ...) is only allowed inside of select lists, where, on, and having clauses

The (select...) option for `IN` or `NOT IN` only makes sense in certain expression contexts.    Other uses are most likely errors.  It cannot appear in a loose expression because it fundamentally requires sqlite to process it.

-----

### CQL0079: function got incorrect number of arguments 'name'

The indicated function was called with the wrong number of arguments.  There are various functions supported each with different rules.  See the SQLite documentation for more information about the specified function.

-----

### CQL0080: function may not appear in this context 'name'

Many functions can only appear in certain contexts.  For instance most aggregate functions are limited to the select list or the HAVING clause.  They cannot appear in, for instance, a WHERE, or ON clause.  The particulars vary by function.

-----

### CQL0081: aggregates only make sense if there is a FROM clause 'name'

The indicated aggregate function was used in a select statement with no tables.  For instance

```sql
select MAX(7);
```

Doesn't make any sense.

-----

### CQL0082: argument must be numeric

The argument of function must be numeric.

-----

### CQL0083: argument must be numeric 'SUM'

The argument of SUM must be numeric.

-----

### CQL0084: second argument must be a string in function 'group_concat'

The second argument of group_concat is the separator, it must be a string.  The first argument will be converted to a string.

-----

### CQL0085: all arguments must be strings 'strftime'

The `strftime` function does complex data formatting.  All the arguments are strings.  See the sqlite documentation for more details on the options (there are many).

-----

### CQL0086: first argument must be a string in function 'function'

The first argument of the function is the formatting string.  The other arguments are variable and many complex conversions will apply.

-----

### CQL0087: first argument must be of type real 'function'

The first argument of the function (e.g. round) should be of type 'real'.

-----

### CQL0088: user function may not appear in the context of a SQL statement 'function_name'

External C functions declared with `declare function ...` are not for use in sqlite.  They may not appear inside statements.

-----

### CQL0089: user function may only appear in the context of a SQL statement 'function_name'

SQLite user defined functions (or builtins) declared with  `declare select function` may only appear inside of sql statements.  In the case of user defined functions they must be added to sqlite by the appropriate C APIs before they can be used in CQL stored procs (or any other context really).   See the sqlite documentation on how to add user defined functions. [Create Or Redefine SQL Functions](http://www.sqlite.org/c3ref/create_function.html)

-----

### CQL0090: `object<T SET>` has a T that is not a procedure with a result set, 'name'

The data type `object<T SET>` refers to the shape of a result set of a particular procedure.  In this case the indicated name is not such a procedure.

The most likely source of this problem is that there is a typo in the indicated name.  Alternatively the name might be a valid shape like a cursor name or some other shape name but it's a shape that isn't coming from a procedure.

-----

CQL0091: -- generalized so that this is not an error anymore


-----

### CQL0092: RAISE may only be used in a trigger statement

SQLite only supports this kind of control flow in the context of triggers, certain trigger predicates might need to unconditionally fail and complex logic can be implemented in this way.  However this sort of thing is not really recommended.  In any case this is not a general purpose construct.

-----

### CQL0093: RAISE 2nd argument must be a string

Only forms with a string as the second argument are supported by SQLite.

-----

### CQL0094: function not yet implemented 'function'

The indicated function is not implemented in CQL.  Possibly you intended to declare it with `declare function` as an external function or `declare select function` as a sqlite builtin.  Note not all sqlite builtins are automatically declared.

-----

### CQL0095: table/view not defined 'name'

The indicated name is neither a table nor a view.  It is possible that the table/view is now deprecated with `@delete` and therefore will appear to not exist in the current context.

-----

### CQL0096: join using column not found on the left side of the join 'column_name'

In the `JOIN ... USING(x,y,z)` form, all the columns in the using clause must appear on both sides of the join.  Here the indicated name is not present on the left side of the join.

-----

### CQL0097: join using column not found on the right side of the join 'column_name'

In the `JOIN ... USING(x,y,z)` form, all the columns in the using clause must appear on both sides of the join.  Here the indicated name is not present on the right side of the join.

-----

### CQL0098: left/right column types in join USING(...) do not match exactly 'column_name'

In the `JOIN ... USING(x,y,z)` form, all the columns in the using clause must appear on both sides of the join and have the same data type.  Here the data types differ in the named column.

-----

### CQL0099: HAVING clause requires GROUP BY clause

The `HAVING` clause makes no sense unless there is also a `GROUP BY` clause.  SQLite enforces this as does CQL.

-----

### CQL0100: duplicate common table name 'name'

In a `WITH` clause, the indicated common table name was defined more than once.

-----

### CQL0101: too few column names specified in common table expression 'name'

In a `WITH` clause the indicated common table expression doesn't include enough column names to capture the result of the `select` statement it is associated with.

e.g.

```sql
WITH foo(a) as (SELECT 1 A, 2 B) ...`
```

The select statement produces two columns the `foo` declaration specifies one.

-----

### CQL0102: too many column names specified in common table expression 'name'


In a `WITH` clause the indicated common table expression has more  column names than the `select` expression it is associated with.

e.g.

```sql
WITH foo(a, b) as (SELECT 1) ... `
```

The select statement produces one column the `foo` declaration specifies two.

-----

### CQL0103: duplicate table/view name 'name'

The indicated table or view must be unique in its context.  The version at the indicated line number is a duplicate of a previous declaration.

-----

### CQL0104: view was present but now it does not exist (use `@delete` instead) 'name'

During schema validation, CQL found a view that used to exist but is now totally gone.  The correct procedure is to mark the view with `@delete` (you can also make it stub with the same name to save a little space).  This is necessary so that CQL can know what views should be deleted on client devices during an upgrade.  If the view is eradicated totally there would be no way to know that the view should be deleted if it exists.

-----

### CQL0105: object was a view but is now a table 'name'

Converting a view into a table, or otherwise creating a table with the same name as a view is not legal.

-----

### CQL0106: trigger was present but now it does not exist (use `@delete` instead) 'name'

During schema validation, CQL found a trigger that used to exist but is now totally gone.  The correct procedure is to mark the trigger with `@delete` (you can also make it stub with the same name to save a little space).  This is necessary so that CQL can know what triggers should be deleted on client devices during an upgrade.  If the trigger is eradicated totally there would be no way to know that the trigger should be deleted if it exists.  That would be bad.

-----

### CQL0107: delete version can't be <= create version 'name'

Attempting to declare that an object has been deleted before it was created is an error.  Probably there is a typo in one or both of the version numbers of the named object.

-----

### CQL0108: table in drop statement does not exist 'table_name'

The indicated table was not declared anywhere.  Note that CQL requires that you declare all tables you will work with, even if all you intend to do with the table is drop it.  When you put a `CREATE TABLE` statement in global scope this only declares a table, it doesn't actually create the table. See the documentation on DDL for more information.

-----

### CQL0109: cannot drop a view with drop table 'view_name'

The object named in a `DROP TABLE` statement must be a table, not a view.

-----

### CQL0110: view in drop statement does not exist 'view_name'

The indicated view was not declared anywhere.  Note that CQL requires that you declare all views you will work with, even if all you intend to do with the view is drop it.  When you put a `CREATE VIEW` statement in global scope this only declares a view, it doesn't actually create the view.  See the documentation on DDL for more information.

-----

### CQL0111: cannot drop a table with drop view 'name'

The object named in a `DROP VIEW` statement must be a view, not a table.

-----

### CQL0112: index in drop statement was not declared 'index_name'

The indicated index was not declared anywhere.  Note that CQL requires that you declare all indices you will work with, even if all you intend to do with the index is drop it.  When you put a `CREATE INDEX` statement in global scope this only declares an index, it doesn't actually create the index.  See the documentation on DDL for more information.

-----

### CQL0113: trigger in drop statement was not declared 'name'

The indicated trigger was not declared anywhere.  Note that CQL requires that you declare all triggers you will work with, even if all you intend to do with the trigger is drop it.  When you put a `CREATE TRIGGER` statement in global scope this only declares a trigger, it doesn't actually create the trigger.  See the documentation on DDL for more information.

-----

### CQL0114: current schema can't go back to recreate semantics for 'table_name'

The indicated table was previously marked with `@create` indicating it has precious content and should be upgraded carefully.  The current schema marks the same table with `@recreate` meaning it has discardable content and should be upgraded by dropping it and recreating it.  This transition is not allowed.  If the table really is non-precious now you can mark it with `@delete` and then make a new similar table with `@recreate`.  This really shouldn't happen very often if at all.  Probably the error is due to a typo or wishful thinking.

-----

### CQL0115: current create version not equal to previous create version for 'table'

The indicated table was previously marked with `@create` at some version (x) and now it is being created at some different version (y !=x ).  This not allowed (if it were then objects might be created in the wrong/different order during upgrade which would cause all kinds of problems).

-----

### CQL0116: current delete version not equal to previous delete version for 'table'

The indicated table was previously marked with `@delete` at some version (x) and now it is being deleted at some different version (y != x).  This not allowed (if it were then objects might be deleted in the wrong/different order during upgrade which would cause all kinds of problems).

-----

### CQL0117: `@delete` procedure changed in object 'table_name'

The `@delete` attribute can optional include a "migration proc" that is run when the upgrade happens.  Once set, this proc can never be changed.

-----

### CQL0118: `@create` procedure changed in object 'table_name'

The `@create` attribute can optional include a "migration proc" that is run when the upgrade happens.  Once set, this proc can never be changed.

-----

### CQL0119: column name is different between previous and current schema 'name'

Since there is no sqlite operation that allows for columns to be renamed, attempting to rename a column is not allowed.

NOTE: you can also get this error if you remove a column entirely, or add a column in the middle of the list somewhere.

Since columns (also) cannot be reordered during upgrade, CQL expects to find all the columns in exactly the same order in the previous and new schema.  Any reordering, or deletion could easily look like an erroneous rename.  New columns must appear at the end of any existing columns.

-----

### CQL0120: column type is different between previous and current schema 'name'

It is not possible to change the data type of a column during an upgrade, SQLite provides no such options.  Attempting to do so results in an error.  This includes nullability.

-----

### CQL0121: column current create version not equal to previous create version 'name'

The indicated column was previously marked with `@create` at some version (x) and now it is being created at some different version (y !=x ).  This not allowed (if it were then objects might be created in the wrong/different order during upgrade which would cause all kinds of problems).


### CQL0122: column current delete version not equal to previous delete version 'name'

The indicated column was previously marked with `@delete` at some version (x) and now it is being deleted at some different version (y != x).  This not allowed (if it were then objects might be deleted in the wrong/different order during upgrade which would cause all kinds of problems).

-----

### CQL0123: column `@delete` procedure changed 'name'

The `@delete` attribute can optional include a "migration proc" that is run when the upgrade happens.  Once set, this proc can never be changed.

-----

### CQL0124: column `@create` procedure changed 'name'

The `@create` attribute can optional include a "migration proc" that is run when the upgrade happens.  Once set, this proc can never be changed.

-----

### CQL0125: column current default value not equal to previous default value 'column'

The default value of a column may not be changed in later versions of the schema.  There is no SQLite operation that would allow this.

-----

### CQL0126: table was present but now it does not exist (use `@delete` instead) 'table'

During schema validation, CQL found a table that used to exist but is now totally gone.  The correct procedure is to mark the table with `@delete`.  This is necessary so that CQL can know what tables should be deleted on client devices during an upgrade.  If the table is eradicated totally there would be no way to know that the table should be deleted if it exists.  That would be bad.

-----

### CQL0127: object was a table but is now a view 'name'

The indicated object was a table in the previous schema but is now a view in the current schema.  This transformation is not allowed.

-----

### CQL0128: table has a column that is different in the previous and current schema 'column'

The indicated column changed in one of its more exotic attributes, examples:

* its `FOREIGN KEY` rules changed in some way
* its `PRIMARY KEY` status changed
* its `UNIQUE` status changed

Basically the long form description of the column is now different and it isn't different in one of the usual way like type or default value.  This error is the catch all for all the other ways a column could change such as "the FK rule for what happens when an update fk violation occurs is now different" -- there are dozens of such errors and they aren't very helpful anyway.

-----

### CQL0129: a column was removed from the table rather than marked with `@delete` 'column_name'

During schema validation, CQL found a column that used to exist but is now totally gone.  The correct procedure is to mark the column with `@delete`.  This is necessary so that CQL can know what columns existed during any version of the schema, thereby allowing them to be used in migration scripts during an upgrade.  If the column is eradicated totally there would be no way to know that the exists, and should no longer be used.  That would be bad.

Of course `@recreate` tables will never get this error because they can be altered at whim.

-----

### CQL0130: table has columns added without marking them `@create` 'column_name'

The indicated column was added but it was not marked with `@create`.  The table in question is not on the `@recreate` plan so this is an error.  Add a suitable `@create` annotation to the column declaration.

-----

### CQL0131: table has newly added columns that are marked both `@create` and `@delete` 'column_name'

The indicated column was simultaneously marked `@create` and `@delete`.  That's surely some kind of typo.  Creating a column and deleting it in the same version is weird.

-----

### CQL0132: table has a facet that is different in the previous and current schema 'table_name'

The indicated table has changes in one of its non-column features.  These changes might be:

* a primary key declaration
* a unique key declaration
* a foreign key declaration

None of these are allowed to change.  Of course `@recreate` tables will never get this error because they can be altered at whim.

-----

### CQL0133: non-column facets have been removed from the table 'name'

The error indicates that the table has had some stuff removed from it.  The "stuff" might be:

* a primary key declaration
* a unique key declaration
* a foreign key declaration

Since there is no way to change any of the constraints after the fact, they may not be changed at all if the table is on the `@create` plan.  Of course `@recreate` tables will never get this error because they can be altered at whim.

-----

### CQL0134: table has a new non-column facet in the current schema 'table_name'

The error indicates that the table has had some stuff added to it.  The "stuff" might be:

* a primary key declaration
* a unique key declaration
* a foreign key declaration

Since there is no way to change any of the constraints after the fact, they may not be changed at all if the table is on the `@create` plan.  Of course `@recreate` tables will never get this error because they can be altered at whim.

-----

### CQL0135: table create statement attributes different than previous version 'table_name'

The 'flags' on the `CREATE TABLE` statement changed between versions.  These flags capture the options like the`TEMP` in `CREATE TEMP TABLE` and the `IF NOT EXISTS`.   Changing these is not allowed.

-----

### CQL0136: trigger already exists 'trigger_name'

Trigger names may not be duplicated.  Probably there is copy/pasta going on here.

-----

### CQL0137: table/view not found 'name'

In a `CREATE TRIGGER` statement, the indicated name is neither a table or a view.  Either a table or
a view was expected in this context.

-----

### CQL0138: a trigger on a view must be the INSTEAD OF form 'name'

In a `CREATE TRIGGER` statement, the named target of the trigger was a view but the trigger type is not `INSTEAD OF`.  Only `INSTEAD OF` can be applied to views because views are not directly mutable so none of the other types make sense.  e.g. there can be no delete operations, on a view, so `BEFORE DELETE` or `AFTER DELETE` are not really a thing.

-----

### CQL0139: temp objects may not have versioning annotations 'object_name'

The indicated object is a temporary.  Since temporary  do not survive sessions it makes no sense to try to version them for schema upgrade.
They are always recreated on demand.  If you need to remove one, simply delete it entirely, it requires no tombstone.

-----

### CQL0140: columns in a temp table may not have versioning attributes 'column_name'

The indicated column is part of a temporary table.  Since temp tables do not survive sessions it makes no sense to try to version their columns for schema upgrade.  They are always recreated on demand.

-----

### CQL0141: table has an AUTOINCREMENT column; it cannot also be WITHOUT ROWID 'table_name'

SQLite uses its `ROWID` internally for `AUTOINCREMENT` columns.  Therefore `WITHOUT ROWID` is not a possibility if `AUTOINCREMENT` is in use.

-----

### CQL0142: duplicate column name 'column_name'

In a `CREATE TABLE` statement, the indicated column was defined twice.  This is probably a copy/pasta issue.

-----

### CQL0143: more than one primary key in table 'table_name'

The indicated table has more than one column with the `PRIMARY KEY` attribute or multiple `PRIMARY KEY` constraints, or a combination of these things.  You'll have to decide which one is really intended to be primary.

-----

### CQL0144: cannot alter a view 'view_name'

In an `ALTER TABLE` statement, the table to be altered is actually a view.  This is not allowed.

-----

### CQL0144: table in alter statement does not exist 'table_name'

In an `ALTER TABLE` statement, the table to be altered was not defined, or perhaps was marked with `@delete` and is no longer usable in the current schema version.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0145: version annotations not valid in alter statement 'column_name'

In an `ALTER TABLE` statement, the attributes on the column may not include `@create` or `@delete`.  Those annotations go on the columns declaration in the corresponding `CREATE TABLE` statement.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0146: adding an auto increment column is not allowed 'column_name'

In an `ALTER TABLE` statement, the attributes on the column may not include `AUTOINCREMENT`.  SQLite does not support the addition of new `AUTOINCREMENT` columns.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0147: adding a not nullable column with no default value is not allowed 'column_name'

In an `ALTER TABLE` statement the attributes on the named column must include a default value or else the column must be nullable.  This is so that SQLite knows what value to put on existing rows when the column is added and also so that any existing insert statements will not suddenly all become invalid.  If the column is nullable or has a default value then the existing insert statements that don't specify the column will continue to work, using either NULL or the default.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0148: added column must already be reflected in declared schema, with `@create`, exact name match required 'column_name'

In CQL loose schema is a declaration, it does not actually create anything unless placed inside of a procedure.  A column that is added with `ALTER TABLE` is not actually declared as part of the schema by the `ALTER`.  Rather the schema declaration is expected to include any columns you plan to add.  Normally the way this all happens is that you put `@create` notations on a column in the schema and the automatic schema upgrader then creates suitable `ALTER TABLE` statements to arrange for that column to be added.  If you manually write an `ALTER TABLE` statement it isn't allowed to add columns at whim; in some sense it must be creating the reality already described in the declaration.  This is exactly what the automated schema upgrader does -- it declares the end state and then alters the world to get to that state.

It's important to remember that from CQL's perspective the schema is fixed for any given compilation, so runtime alterations to it are not really part of the type system.  They can't be.  Even `DROP TABLE` does not remove the table from type system -- it can't -- the most likely situation is that you are about to recreate that same table again for another iteration with the proc that creates it.

This particular error is saying that the column you are trying to add does not exist in the declared schema.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0149: added column must be an exact match for the column type declared in the table 'column_name'

In CQL loose schema is a declaration, it does not actually create anything unless placed inside of a procedure.  A column that is added with `ALTER TABLE` is not actually declared as part of the schema by the `ALTER`.  Rather the schema declaration is expected to include any columns you plan to add.  Normally the way this all happens is that you put `@create` notations on a column in the schema and the automatic schema upgrader then creates suitable `ALTER TABLE` statements to arrange for that column to be added.  If you manually write an `ALTER TABLE` statement it isn't allowed to add columns at whim; in some sense it must be creating the reality already described in the declaration.  This is exactly what the automated schema upgrader does -- it declares the end state and then alters the world to get to that state.

It's important to remember that from CQL's perspective the schema is fixed for any given compilation, so runtime alterations to it are not really part of the type system.  They can't be.  Even `DROP TABLE` does not remove the table from type system -- it can't -- the most likely situation is that you are about to recreate that same table again for another iteration with the proc that creates it.

This particular error is saying that the column you are trying to add exists in the declared schema, but its definition is different than you have specified in the `ALTER TABLE` statement.

NOTE: `ALTER TABLE` is typically not used directly; the automated schema upgrade script generation system uses it.

-----

### CQL0150: expected numeric expression in IF predicate

In an `IF` statement the condition (predicate) must be a numeric.  The body of the `IF` runs if the value is not null and not zero.

-----

### CQL0151: table in delete statement does not exist 'table_name'

In a `DELETE` statement, the indicated table does not exist. Probably it's a spelling mistake, or else the table has been marked with `@delete` and may no longer be used in `DELETE` statements.

-----

### CQL0152: cannot delete from a view 'view_name'

In a `DELETE` statement, the target of the delete must be a table, but the indicated name is a view.

-----

### CQL0153: duplicate target column name in update statement 'column_name'

In an `UPDATE` statement, you can only specify any particular column to update once.

e.g. `UPDATE coordinates set x = 1, x = 3;`  will produce this error. `UPDATE coordinates set x = 1, y = 3;` might be correct.

This error is most likely caused by a typo or a copy/pasta of the column names, especially if they were written one per line.

-----

### CQL0154: table in update statement does not exist 'table_name'

In an `UPDATE` statement, the target table does not exist.  Probably it's a spelling mistake, or else the table has been marked with `@delete` and may no longer be used in `UPDATE` statements.

-----

### CQL0155: cannot update a view 'view_name'

In an `UPDATE` statement, the target of the update must be a table but the name of a view was provided.

-----

### CQL0156: seed expression must be a non-nullable integer

The `INSERT` statement statement supports the notion of synthetically generated values for dummy data purposes.  A 'seed' integer is used to derive the values.  That seed (in the `@seed()` position) must be a non-null integer.

The most common reason for this error is that the seed is an input parameter and it was not declared `NOT NULL`.

-----

### CQL0157: count of columns differs from count of values

In an `INSERT` statement of the form `INSERT INTO foo(a, b, c) VALUES(x, y, z)` the number of values (x, y, z) must
be the same as the number of columns (a, b, c).  Note that there are many reasons you might not have to specify all the columns of the table but whichever columns you do specify should have values.

-----

### CQL0158: required column missing in INSERT statement 'column_name'

In an `INSERT` statement such as `INSERT INTO foo(a,b,c) VALUES(x,yz)` this error is indicating that there is a column in `foo` (the one indicated in the error) which was not in the list (i.e. not one of a, b, c) and that column is neither nullable, nor does it have a default value.  In order to insert a row a value must be provided.  To fix this include the indicated column in your insert statement.

-----

### CQL0159: cannot add an index to a virtual table 'table_name'

Adding an index to a virtual table isn't possible, the virtual table includes whatever indexing its module provides, no further indexing is possible.

From the SQLite documentation: "One cannot create additional indices on a virtual table. (Virtual tables can have indices but that must be built into the virtual table implementation. Indices cannot be added separately using CREATE INDEX statements.)"

-----

### CQL0160: table in insert statement does not exist 'table_name'

In an `INSERT` statement attempting to insert into the indicated table name is not possible because there is no such table.
This error might happen because of a typo, or it might happen because the indicated table has been marked with `@delete` and is logically hidden.

-----

### CQL0161: cannot insert into a view 'view_name'

In an `INSERT` statement attempting to insert into the indicated name is not possible because that name is a view not a table.  Inserting into views is not supported.

-----

### CQL0162: cannot add a trigger to a virtual table 'table_name'

Adding a trigger to a virtual table isn't possible.

From the SQLite documentation: "One cannot create a trigger on a virtual table."

-----

### CQL0163: FROM ARGUMENTS construct is only valid inside a procedure

Several statements support the `FROM ARGUMENTS` sugar format like `INSERT INTO foo(a,b,c) FROM ARGUMENTS` which causes the arguments of the current procedure to be used as the values.  This error is complaining that you have used this form but the statement does not occur inside of a procedure so there can be no arguments.  This form does not make sense outside of any procedure.

-----

### CQL0164: cannot use ALTER TABLE on a virtual table 'table_name'

This is not supported by SQLite.

From the SQLite documentation: "One cannot run ALTER TABLE ... ADD COLUMN commands against a virtual table."

-----

### CQL0165: fetch values is only for value cursors, not for sqlite cursors 'cursor_name'

Cursors come in two flavors.  There are "statement cursors" which are built from something like this:

```sql
declare C cursor for select * from foo;
fetch C;
-- or --
fetch C into a, b, c;
```

That is, they come from a SQLite statement and you can fetch values from that statement.  The second type comes from procedural values like this.

```sql
declare C cursor like my_table;
fetch C from values(1, 2, 3);
```

In the second example `C`'s data type will be the same as the columns in `my_table` and we will fetch its values from `1,2,3` -- this version has no database backing at all, it's just data.

This error says that you declared the cursor in the first form (with a SQL statement) but then you tried to fetch it using the second form, the one for data. These forms don't mix.   If you need a value cursor for a row you can copy data from one cursor into another.

-----

### CQL0166: count of columns differs from count of values

In a value cursor, declared something like this:
```sql
declare C cursor like my_table;
fetch C from values(1, 2, 3);
```
The type of the cursor ( in this case from `my_table`) requires a certain number of columns, but that doesn't match the number that were provided in the values.

To fix this you'll need to add/remove values so that the type match.

-----

### CQL0167: required column missing in FETCH statement 'column_name'

In a value cursor, declared something like this:
```sql
declare C cursor like my_table;
fetch C(a,b,c) from values(1, 2, 3);
```

This error is saying that there is some other field in the table 'd' and it was not specified in the values.  Nor was there a usable dummy data for that column that could be used.  You need to provide a value for the missing column.

-----

### CQL0168: CQL has no good way to generate dummy blobs; not supported for now

In a value cursor with dummy data specified, one of the columns in the cursor is of type blob.  There's no good way to create dummy data for blobs so that isn't supported.

-----

### CQL0169: enum not found 'enum_name'

The indicated name was used in a context where an enumerated type name was expected but there is no such type.

Perhaps the enum was not included (missing a #include) or else there is a typo.

-----

### CQL0170: cast is redundant, remove to reduce code size 'expression'

The operand of the `CAST` expression is already the type that it is being cast to.  The cast will
do nothing but waste space in the binary and make the code less clear.  Remove it.

-----

### CQL0171: name not found 'name'

In a scoped name list, like the columns of a cursor (for a fetch), or the columns of a particular table (for an index) a name appeared that did not belong to the universe of legal names.  Trying to make a table index using a column that is not in the table would produce this error.  There are many instances where a list of names belongs to some limited scope.

-----

### CQL0172: name list has duplicate name 'name'

In a scoped name list, like the columns of a cursor (for a fetch), or the columns of a particular table (for an index) a name appeared twice in the list where the names must be unique.  Trying to make a table index using the same column twice would produce this error.

-----

### CQL0173: variable not found 'variable_name'

In a `SET` statement, the target of the assignment is not a valid variable name in that scope.

-----

### CQL0174: cannot set a cursor 'cursor_name'

In a `SET` statement, the target of the assignment is a cursor variable, you cannot assign to a cursor variable.

-----

### CQL0175: duplicate parameter name 'parameter_name'

In a parameter list for a function or a procedure, the named parameter appears more than once.  The formal names for function arguments must be unique.

-----

### CQL0176: indicated procedure or group already has a recreate action 'name'

There can only be one migration rule for a table or group, the indicated item already has such an action.  If you need more than one migration action you can
create a containing procedure that dispatches to other migrators.

-----

### CQL0177: global constants must be either constant numeric expressions or string literals 'constant_definition'

Global constants must be either a combination other constants for numeric expressions or else string literals.  The indicated expression was not one of those.

This can happen if the expression uses variables, or has other problems that prevent it from evaluating, or if a function is used that is not supported.

-----

### CQL0178: proc has no result 'like_name'

In an argument list, the `LIKE` construct was used to create arguments that are the same as the return type of the named procedure.  However the named procedure does not produce a result set and therefore has no columns to mimic.  Probably the name is wrong.

-----

### CQL0179: shared fragments must consist of exactly one top level statement 'procedure_name'

Any shared fragment can have only one statement.  There are three valid forms -- IF/ELSE, WITH ... SELECT, and SELECT.

This error indicates the named procedure, which is a shared fragment, has more than one statement.

-----

### CQL0180: duplicate column name in result not allowed 'column_name'

In a procedure that returns a result either with a loose `SELECT` statement or in a place where the result of a `SELECT` is captured with a `FETCH` statement the named column appears twice in the projection of the `SELECT` in question.  The column names must be unique in order to have consistent cursor field names or consistent access functions for the result set of the procedure.  One instance of the named column must be renamed with something like `select T1.foo first_foo, T2.foo second_foo`.

-----

### CQL0181: autodrop temp table does not exist 'name'

In a `cql:autodrop` annotation, the given name is unknown entirely.

-----

### CQL0182: autodrop target is not a table 'name'

In a `cql:autodrop` annotation, the given name is not a table (it's probably a view).

-----

### CQL0183: autodrop target must be a temporary table 'name'

In a `cql:autodrop` annotation, the given name is a table but it is not a temp table.  The annotation is only valid on temp tables, it's not for "durable" tables.

-----

### CQL0184: stored procedures cannot be nested 'name'

The `CREATE PROCEDURE` statement may not appear inside of another stored procedure.  The named procedure appears in a nested context.

-----

### CQL0185: proc name conflicts with func name 'name'

In a `CREATE PROCEDURE` statement, the given name conflicts with an already declared function (`DECLARE FUNCTION` or `DECLARE SELECT FUNCTION`).  You'll have to choose a different name.

-----

### CQL0186: duplicate stored proc name 'name'

In a `CREATE PROCEDURE` statement, the indicated name already corresponds to a created (not just declared) stored procedure.  You'll have to choose a different name.

-----

### CQL0187: @schema_upgrade_version not declared or doesn't match upgrade version `N` for proc 'name'

The named procedure was declared as a schema migration procedure in an `@create` or `@delete` annotation for schema version `N`.  In order to correctly type check such a procedure it must be compiled in the context of schema version `N`.  This restriction is required so that the tables and columns the procedure sees are the ones that existed in version `N` not the ones that exist in the most recent version as usual.

To create this condition, the procedure must appear in a file that begins with the line:

```sql
@schema_upgrade_version <N>;
```

And this declaration must come before any `CREATE TABLE` statements.  If there is no such declaration, or if it is for the wrong version, then this error will be generated.

-----

### CQL0188: procedure is supposed to do schema migration but it doesn't have any DML 'name'

The named procedure was declared as a schema migration procedure in an `@create` or `@delete` annotation, however the procedure does not have any DML in it.  That can't be right.  Some kind of data reading and writing is necessary.

-----

### CQL0189: procedure declarations/definitions do not match 'name'

The named procedure was previously declared with a `DECLARE PROCEDURE` statement but when the `CREATE PROCEDURE` was encountered, it did not match the previous declaration.

-----

### CQL0190: duplicate column name 'name'

In a context with a typed name list (e.g. `id integer, t text`) the named column occurs twice.  Typed name lists happen in many contexts, but a common one is the type of the result in a declared procedure statement or declared function statement.

-----

### CQL0191: declared functions must be top level 'function_name'

A `DECLARE FUNCTION` statement for the named function is happening inside of a procedure.  This is not legal.  To correct this move the declaration outside of the procedure.

-----

### CQL0192: func name conflicts with proc name 'name'

The named function in a `DECLARE FUNCTION` statement conflicts with an existing declared or created procedure.  One or the other must be renamed to resolve this issue.

-----

### CQL0193: duplicate function name 'name'

The named function in a `DECLARE FUNCTION` statement conflicts with an existing declared function, or it was declared twice.  One or the other declaration must be renamed or removed to resolve this issue.

-----

### CQL0194: declared procedures must be top level 'name'

A `DECLARE PROCEDURE` statement for the named procedure is itself happening inside of a procedure.  This is not legal.  To correct this move the declaration outside of the procedure.

-----

### CQL0195: proc name conflicts with func name 'name'

The named procedure in a `DECLARE PROCEDURE` statement conflicts with an existing declared function.  One or the other declaration must be renamed or removed to resolve this issue.

-----

### CQL0196: procedure declarations/definitions do not match 'name'

The named procedure was previously declared with a `DECLARE PROCEDURE` statement.  Now there is another declaration and it does not match the previous declaration

-----

### CQL0197: duplicate variable name in the same scope 'name'

In a `DECLARE` statement, a variable of the same name already exists in that scope.  Note that CQL does not have block level scope, all variables are procedure level, so they are in scope until the end of the procedure.  To resolve this problem, either re-use the old variable if appropriate or rename the new variable.

-----

### CQL0198: global variable hides table/view name 'name'

In a `DECLARE` statement, the named variable is a global (declared outside of any procedure) and has the same name as a table or view.  This creates a lot of confusion and is therefore disallowed.  To correct the problem, rename the variable.  Global variables generally are problematic, but sometimes necessary.

-----

### CQL0199: cursor requires a procedure that returns a result set via select 'procedure_name'

In a `DECLARE` statement that declares a `CURSOR FOR CALL` the procedure that is being called does not produce a result set with the `SELECT` statement.  As it has no row results it is meaningless to try to put a cursor on it.  Probably the error is due to a copy/pasta of the procedure name.

-----

### CQL0200: variable is not a cursor 'another_cursor'

In a `DECLARE` statement that declares a `CURSOR LIKE` another cursor, the indicated name is a variable but it is not a cursor, so we cannot make another cursor like it.  Probably the error is due to a typo in the 'like_name'.

-----

### CQL0201: expanding FROM ARGUMENTS, there is no argument matching 'required_arg'

In an `INSERT` or `FETCH` statement using the form `FROM ARGUMENTS(LIKE [name])`
The shape `[name]` had columns that did not appear in as arguments to the current procedure.
Maybe arguments are missing or maybe the name in the `like` part is the wrong name.

-----

### CQL0202: must be a cursor, proc, table, or view 'like_name'

In a `DECLARE` statement that declares a `CURSOR LIKE` some other name, the indicated name is not the name of any of the things that might have a valid shape to copy, like other cursors, procedures, tables, or views.  Probably there is a typo in the name.

-----

### CQL0203: cursor requires a procedure that returns a cursor with OUT 'cursor_name'

In the `DECLARE [cursor_name] CURSOR FETCH FROM CALL <something>` form, the code is trying to create the named cursor by calling a procedure that doesn't actually produce a single row result set with the `OUT` statement.  The procedure is valid (that would be a different error) so it's likely that the wrong procedure is being called rather than being an outright typo.  Or perhaps the procedure was changed such that it no longer produces a single row result set.

This form is equivalent to:

```sql
DECLARE [cursor_name] LIKE procedure;
FETCH [cursor_name] FROM CALL procedure(args);
```

It's the declaration that's failing here, not the call.

-----

CQL0204 : Unused.

-----

### CQL0205: not a cursor 'name'

The indicated name appeared in a context where the name of a cursor was
expected, but the name does not refer to a cursor.

-----

### CQL0206: duplicate name in list 'name'

There are many contexts where a list of names appears in the CQL grammar and the list must not contain duplicate names.  Some examples are:

* the column names in a `JOIN ... USING(x,y,z,...)` clause
* the fetched variables in a `FETCH [cursor] INTO x,y,z...` statement
* the column names listed in a common table expression `CTE(x,y,z,...) as (SELECT ...)`
* the antecedent schema region names in `@declare_schema_region <name> USING x,y,z,...`

The indicated name was duplicated in such a context.

-----

### CQL0207: expected a variable name for OUT or INOUT argument 'param_name'

In a procedure call, the indicated parameter of the procedure is an OUT or INOUT parameter but the call site doesn't have a variable in that position in the argument list.

Example:

```sql
declare proc foo(out x integer);

-- the constant 1 cannot be used in the out position when calling foo
call foo(1); '
```
-----

### CQL0208: shared fragments cannot have any out or in/out parameters 'param_name'

A shared fragment will be expanded into the body of a SQL select statement, as such it can have no side-effects such as out arguments.

-----

### CQL0209: proc out parameter: arg must be an exact type match (expected expected_type; found actual_type) 'param_name'

In a procedure call, the indicated parameter is in an 'out' position, it is a viable local variable but it is not an exact type match for the parameter.  The type of variable used to capture out parameters must be an exact match.

```sql
declare proc foo(out x integer);

create proc bar(out y real)
begin
  call foo(y); -- y is a real variable, not an integer.
end;
```

The above produces:
```sql
CQL0209: proc out parameter: arg must be an exact type match
(expected integer; found real) 'y'
```

-----

### CQL0210: proc out parameter: arg must be an exact type match (even nullability)
(expected expected_type; found actual_type) 'variable_name'

In a procedure call, the indicated parameter is in an 'out' position, it is a viable local variable of the correct type but the nullability does not match.  The type of variable used to capture out parameters must be an exact match.

```sql
declare proc foo(out x integer not null);

create proc bar(out y integer)
begin
  call foo(y); -- y is nullable but foo is expecting not null.
end;
```

The above produces:
```sql
CQL0210: proc out parameter: arg must be an exact type match (even nullability)
(expected integer notnull; found integer) 'y'
```

-----

### CQL0211: procedure without trailing OUT parameter used as function 'procedure_name'

In a function call, the target of the function call was a procedure, procedures can be used like functions but their last parameter must be marked `out`. That will be the return value.  In this case the last argument was not marked as `out` and so the call is invalid.

Example:

```sql
declare proc foo(x integer);

create proc bar(out y integer)
begin
  set y := foo(); -- foo does not have an out argument at the end
end;
```

-----

### CQL0212: too few arguments provided to procedure 'name'

In a procedure call to the named procedure, not enough arguments were provided to make the call.  One or more arguments may have been omitted or perhaps the called procedure has changed such that it now requires more arguments.

-----

### CQL0213: procedure had errors, can't call. 'procedure_name'

In a procedure call to the named procedure, the target of the call had compilation errors.  As a consequence this call cannot be checked and therefore must be marked in error, too.  Fix the errors in the named procedure.

-----

### CQL0214: procedures with results can only be called using a cursor in global context 'name'

The named procedure results a result set, either with the `SELECT` statement or the `OUT` statement.  However it is being called from outside of any procedure.  Because of this, its result cannot then be returned anywhere.  As a result, at the global level the result must be capture with a cursor.

Example:
```sql
create proc foo()
begin
  select * from bar;
end;

call foo();  -- this is not valid
declare cursor C for call foo();  -- C captures the result of foo, this is ok.
```
-----

### CQL0215: value cursors are not used with FETCH C, or FETCH C INTO 'cursor_name'

In a `FETCH` statement of the form `FETCH [cursor]` or `FETCH [cursor] INTO` the named cursor
is a value cursor.  These forms of the `FETCH` statement apply only to statement cursors.

Example:good

```sql
-- value cursor shaped like a table
declare C cursor for select * from bar;
--ok, C is fetched from the select results
fetch C;
```
Example: bad
```sql
-- value cursor shaped like a table
declare C cursor like bar;
-- invalid, there is no source for fetching a value cursor
fetch C;
-- ok assuming bar is made up of 3 integers
fetch C from values(1,2,3);
```

* statement cursors come from SQL statements and can be fetched
* value cursors are of a prescribed shape and can only be loaded from value sources

-----

### CQL0216: FETCH variable not found 'cursor_name'

In a `FETCH` statement,  the indicated name, which is supposed to be a cursor, is not in fact a valid name at all.

Probably there is a typo in the name.  Or else the declaration is entirely missing.

-----

### CQL0217: number of variables did not match count of columns in cursor 'cursor_name'

In a `FETCH [cursor] INTO [variables]` the number of variables specified did not match the number of columns in the named cursor.  Perhaps the source of the cursor (a select statement or some such) has changed.

-----

### CQL0218: continue must be inside of a 'loop' or 'while' statement

The `CONTINUE` statement may only appear inside of looping constructs.  CQL only has two `LOOP FETCH ...` and `WHILE`

-----

### CQL0219: leave must be inside of a 'loop', 'while', or 'switch' statement

The `LEAVE` statement may only appear inside of looping constructs or the switch statement.

CQL has two loop types: `LOOP FETCH ...` and `WHILE` and of course the `SWITCH` statement.

The errant `LEAVE` statement is not in any of those.

-----

### CQL0220: savepoint has not been mentioned yet, probably wrong 'name'

In a `ROLLBACK` statement that is rolling back to a named savepoint, the indicated savepoint was never mentioned before.  It should have appeared previously in a `SAVEPOINT` statement.  This probably means there is a typo in the name.

-----

### CQL0221: savepoint has not been mentioned yet, probably wrong 'name'

In a `RELEASE SAVEPOINT` statement that is rolling back to a named savepoint, the indicated savepoint was never mentioned before.  It should have appeared previously in a `SAVEPOINT` statement.  This probably means there is a typo in the name.

-----

### CQL0222: out cursor statement only makes sense inside of a procedure

The statement form `OUT [cursor_name]` makes a procedure that returns a single row result set.  It doesn't make any
sense to do this outside of any procedure because there is no procedure to return that result.  Perhaps the `OUT` statement was mis-placed.

-----

### CQL0223: cursor was not fetched with the auto-fetch syntax 'fetch [cursor]' 'cursor_name'

The statement form `OUT [cursor_name]` makes a procedure that returns a single row result set that corresponds to the current value of the cursor.  If the cursor never held values directly then there is nothing to return.

Example:

```sql
declare C cursor for select * from bar;
out C;  -- error C was never fetched

declare C cursor for select * from bar;
fetch C into x, y, z;
-- error C was used to load x, y, z so it's not holding any data
out C;

declare C cursor for select * from bar;
-- create storage in C to hold bar columns (e.g. C.x, C,y, C.z)
fetch C;
-- ok, C holds data
out C;
```

-----

### CQL0224: a CALL statement inside SQL may call only a shared fragment i.e. @attribute(cql:shared_fragment)

Inside of a WITH clause you can create a CTE by calling a shared fragment like so:

```
WITH
  my_shared_something(*) AS (CALL shared_proc(5))
SELECT * from my shared_something;
```

or you can use a nested select expression like

```
 SELECT * FROM (CALL shared_proc(5)) as T;
```

However `shared_proc` must define a shareable fragment, like so:

```
@attribute(cql:shared_fragment)
create proc shared_proc(lim_ integer)
begin
   select * from somewhere limit lim_;
end;
```

Here the target of the CALL is not a shared fragment.

-----

### CQL0225: switching to previous schema validation mode must be outside of any proc

The `@previous_schema` directive says that any schema that follows should be compared against what was declared before this point.  This gives CQL the opportunity to detect changes in schema that are not supportable.

The previous schema directive must be outside of any stored procedure.

Example:
```sql
@previous_schema;  -- ok here

create proc foo()
begin
  @previous schema; -- nope
end;
```
-----

### CQL0226: schema upgrade declaration must be outside of any proc

The `@schema_upgrade_script` directive tells CQL that the code that follows is intended to upgrade schema from one version to another.  This kind of script is normally generated by the `--rt schema_upgrade` option discussed elsewhere.  When processing such a script, a different set of rules are used for DDL analysis.  In particular, it's normal to declare the final versions of tables but have DDL that creates the original version and more DDL to upgrade them from wherever they are to the final version (as declared).  Ordinarily these duplicate definitions would produce errors.  This directive allows those duplications.

This error is reporting that the directive happened inside of a stored procedure, this is not allowed.

Example:
```sql
@schema_upgrade_script;  -- ok here

create proc foo()
begin
  @schema_upgrade_script; -- nope
end;
```
-----

### CQL0227: schema upgrade declaration must come before any tables are declared

The `@schema_upgrade_script` directive tells CQL that the code that follows is intended to upgrade schema from one version to another.  This kind of script is normally generated by the `--rt schema_upgrade` option discussed elsewhere.  When processing such a script, a different set of rules are used for DDL analysis.  In particular, it's normal to declare the final versions of tables but have DDL that creates the original version and more DDL to upgrade them from wherever they are to the final version (as declared).  Ordinarily these duplicate definitions would produce errors.  This directive allows those duplications.

In order to do its job properly the directive must come before any tables are created with DDL.  This error tells you that the directive came too late in the stream. Or perhaps there were two such directives and one is late in the stream.

-----

### CQL0228: schema upgrade version must be a positive integer

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive, the code in that procedure expects to see the schema as it existed at the version it is to migrate.  The `@schema_upgrade_version` directive allows you to set the visible schema version to something other than the latest. There can only be one such directive.

This error says that the version you are trying to view is not a positive integer version (e.g version -2)

-----

### CQL0229: schema upgrade version declaration may only appear once

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive, the code in that procedure expects to see the schema as it existed at the version it is to migrate.  The `@schema_upgrade_version` directive allows you to set the visible schema version to something other than the latest.  There can only be one such directive.

This error says that a second `@schema_upgrade_version` directive has been found.

-----

### CQL0230: schema upgrade version declaration must be outside of any proc

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive, the code in that procedure expects to see the schema as it existed at the version it is to migrate.  The `@schema_upgrade_version` directive allows you to set the visible schema version to something other than the latest.  There can only be one such directive.

This error says that the `@schema_upgrade_version` directive was found inside of a stored procedure.  This is not allowed.

-----

### CQL0231: schema upgrade version declaration must come before any tables are declared

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive, the code in that procedure expects to see the schema as it existed at the version it is to migrate.  The `@schema_upgrade_version` directive allows you to set the visible schema version to something other than the latest.  There can only be one such directive.

This error says that the `@schema_upgrade_version` directive came after tables were already declared.  This is not allowed, the directive must come before any DDL.

-----

### CQL0232: nested select expression must return exactly one column

In a `SELECT` expression like `set x := (select id from bar)` the select statement must return exactly one column as in the example provided.  Note that a runtime error will ensue if the statement returns zero rows, or more than one row,  so this form is very limited.  To fix this error change your select statement to return exactly one column.  Consider how many rows you will get very carefully also, that cannot be checked at compile time.

-----

### CQL0233: procedure previously declared as schema upgrade proc, it can have no args 'procedure_name'

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive that procedure will be called during schema migration with no context available.  Therefore, the schema migration proc is not allowed to have any arguments.

-----

### CQL0234: autodrop annotation can only go on a procedure that returns a result set 'procedure_name'

The named procedure has the `autodrop` annotation (to automatically drop a temporary table) but the procedure in question doesn't return a result set so it has no need of the autodrop feature.  The purpose that that feature is to drop the indicated temporary tables once all the select results have been fetched.

-----

### CQL0235: too many arguments provided to procedure 'procedure_name'

In a `CALL` statement, or a function call, the named procedure takes fewer arguments than were provided. This error might be due to some copy/pasta going on or perhaps the argument list of the procedure/function changed to fewer items.
To fix this, consult the argument list and adjust the call accordingly.

-----

### CQL0236: autodrop annotation can only go on a procedure that uses the database 'name'

The named procedure has the `autodrop` annotation (to automatically drop a temporary table) but the procedure in question doesn't even use the database at all, much less the named table.  This annotation is therefore redundant.

-----

### CQL0237: strict FK validation requires that some ON UPDATE option be selected for every foreign key

`@enforce_strict` has been use to enable strict foreign key enforcement.  When enabled every foreign key must have an action for the `ON UPDATE` rule.  You can specify `NO ACTION` but you can't simply leave the action blank.

-----

### CQL0238: strict FK validation requires that some ON DELETE option be selected for every foreign key

`@enforce_strict` has been use to enable strict foreign key enforcement.  When enabled every foreign key must have an action for the `ON DELETE` rule.  You can specify `NO ACTION` but you can't simply leave the action blank.

-----

### CQL0239: 'annotation' column does not exist in result set 'column_name'

The `@attribute(cql:identity=(col1, col2, ...))` form has been used to list the identity columns of a stored procedures result set.  These columns must exist in the result set and they must be unique.  The indicated column name is not part of the result of the procedure that is being annotated.

The `@attribute(cql:vault_sensitive=(col1, col2, ...)` form has been used to list the columns of a stored procedures
result set. These columns must exist in the result set. The indicated column name will be encoded if they are sensitive
and the cursor that produced the result_set is a DML.

-----

### CQL0240: identity annotation can only go on a procedure that returns a result set 'procedure_name'

The `@attribute(cql:identity=(col1, col2,...))` form has been used to list the identity columns of a stored procedures result set.  These columns must exist in the result set and they must be unique.  In this case, the named procedure doesn't even return a result set.  Probably there is a copy/pasta going on.  The identity attribute can likely be removed.

-----

### CQL0241: CONCAT may only appear in the context of SQL statement

The SQLite `||` operator has complex string conversion rules making it impossible to faithfully emulate.  Since there is no helper function for doing concatenations, CQL choses to support this operator only in contexts where it will be evaluated by SQLite.  That is, inside of some SQL statement.

Examples:
```sql
declare X text;

set X := 'foo' || 'bar';   -- error

set X := (select 'foo' || 'bar');  -- ok
```

If concatenation is required in some non-sql context, use the `(select ..)` expression form to let SQLite do the evaluation.

-----

### CQL0242: lossy conversion from type 'type'

There is an explicit (`set x := y`) or implicit assignment (e.g. conversion of a parameter) where the storage for the
target is a smaller numeric type than the expression that is being stored.   This usually means a variable that should
have been declared `LONG` is instead declared `INTEGER` or that you are typing to pass a LONG to a procedure that expects an `INTEGER`

-----

### CQL0243: blob operand must be converted to string first in '||'

We explicitly do not wish to support string concatenation for blobs that holds non-string data. If the blob contains string data, make your intent clear by converting it to string first using `CAST` before doing the concatenation.

-----

### CQL0244: unknown schema region 'region'

In a `@declare_schema_region` statement one of the USING regions is not a valid region name.  Or in `@begin_schema_region` the region name is not valid.  This probably means there is a typo in your code.

-----

### CQL0245: schema region already defined 'region'

The indicated region was previously defined, it cannot be redefined.

-----

### CQL0246: schema regions do not nest; end the current region before starting a new one

Another `@begin_schema_region` directive was encountered before the previous `@end_schema_region` was found.

-----

### CQL0247: you must begin a schema region before you can end one

An `@end_schema_region` directive was encountered but there was no corresponding `@begin_schema_region` directive.

-----

### CQL0248: schema region directives may not appear inside of a procedure

All of the `*_schema_region` directives must be used at the top level of your program, where tables are typically declared.  They do not belong inside of procedures.  If you get this error, move the directive out of the procedure near the DDL that it affects.

-----

### CQL0249: function is not a table-valued-function 'function_name'

The indicated identifier appears in the context of a table, it is a function, but it is not a table-valued function.  Either the declaration is wrong (use  something like `declare select function foo(arg text) (id integer, t text)`) or the name is wrong.  Or both.

-----

### CQL0250: table-valued function not declared 'function_name'

In a select statement, there is a reference to the indicated table-valued-function.  For instance:

```sql
-- the above error happens if my_function has not been declared
-- as a table valued function
select * from my_function(1,2,3);
```

However , `my_function` has not been declared as a function at all.  A correct declaration might look like this:

```sql
declare select function my_function(a int, b int, c int)
                                   (x int, y text);
```

Either there is a typo in the name or the declaration is missing, or both...

-----

### CQL0251: fragment must end with exactly 'SELECT * FROM CTE'

Query fragments have an exact prescription for their shape.  This prescription includes `select * from CTE` as the final query where CTE is the common table expression that they define.  This the error message includes the specific name that is required in this context.

-----

### CQL0252: @PROC literal can only appear inside of procedures

An @PROC literal was used outside of any procedure.  It cannot be resolved if it isn't inside a procedure.

-----

### CQL0253: base fragment must have only a single CTE named the same as the fragment 'name'

Query fragments have an exact prescription for their shape.  This prescription includes  `select * from CTE` where CTE is the single common table expression with the same name as the base query.

This error says that the final select came from something other than the single CTE that is the base name or there was more than one CTE in the fragment.

You can also get this error if you have an extension fragment but you accidentally marked it as a base fragment.

-----

### CQL0254: switching to previous schema validation mode not allowed if `@schema_upgrade_version` was used

When authoring a schema migration script (a stored proc named in an `@create` or `@delete` annotation) you must create that procedure in a file that is marked with `@schema_upgrade_verison` specifying the schema version it is upgrading.  If you do this, then the proc (correctly) only sees the schema as it existed at that version.  However that makes the schema unsuitable for analysis using `@previous_schema` because it could be arbitrarily far in the past.  This error prevents you from combining those features.  Previous schema validation should only happen against the current schema.

-----

### CQL0255: fragment name is not a previously declared base fragment 'bad_fragment_name'

In an extension or assembly fragment declaration, the specified base fragment name has not been previously defined and that is not allowed.

Probably there is a typo, or the declarations are in the wrong order. The base fragment has to come first.

-----

### CQL0256: fragment name conflicts with existing base fragment 'NAME'

Extension query fragment can only be created with a custom procedure name different from all existing base fragment names otherwise we throw this error.

-----

### CQL0257: argument must be a string or numeric in 'function'

The indicated function (usually min or max) only works on strings and numerics.  `NULL` literals, blobs, or objects
are not allowed in this context.

-----

### CQL0258: extension fragment must add exactly one CTE; found extra named 'name'

The extension fragment includes more than one additional CTE, it must have exactly one.

In the following example, `ext2` is not valid,  you have to stop at `ext1`

```sql
-- example bad extension fragment
@attribute(cql:extension_fragment=core)
create proc test_bad_extension_fragment_three()
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    ext1(x,y,z,a) as (select core.*, extra1.* from core left outer join extra1),
    ext2(x,y,z,b) as (select core.*, extra2.* from core left outer join extra2)
  select * from ext2;
end;
```

-----

### CQL0259: extension fragment CTE must select T.* from base CTE

For the select expression in extension fragment, it must include all columns from the base table. This error indicates the select expression doesn't select from the base table. It should look like this
```sql
select core.*, ... from core
```
Here core is the name of its base table.

-----

### CQL0260: extension fragment CTE must be a simple left outer join from 'table_name'

Extension fragments may add columns to the result, to do this without lose any rows you must always left outer join to the new data that you with to include.  There is a specific prescription for this.  It has
to look like this:

```sql
@attribute(cql:extension_fragment=core)
create proc an_extension()
begin
  with
    core(x,y,z) as (select 1,nullable("a"),nullable(3L)),
    ext1(x,y,z,a) as (select core.*, extra_column from core left outer join extra_stuff),
  select * from ext1;
end;
```

Here extension `ext1` is adding `extra_column` which came from `extra_stuff`.  There could have
been any desired join condition or indeed any join expression at all but it has to begin with `from core left outer join` so that all the core columns will be present and now rows can be removed.

-----

### CQL0261: cursor did not originate from a SQLite statement, it only has values 'cursor_name'

The form:

```sql
  SET [name] FROM CURSOR [cursor_name]
```

Is used to wrap a cursor in an object so that it can be returned for forwarded.  This is the so-called "boxing" operation
on the cursor.  The object can then be "unboxed" later to make a cursor again.  However the point of this is to
keep reading forward on the cursor perhaps in another procedure.  You can only read forward on a cursor that
has an associated SQLite statement.  That is the cursor was created with something like this

```sql
  DECLARE [name] CURSOR FOR SELECT ... | CALL ...
```

If the cursor isn't of this form it's just values, you can't move it forward and so "boxing" it is of no value.  Hence not allowed.
You can return the cursor values with `OUT` instead.

----

### CQL0262: LIKE ... ARGUMENTS used on a procedure with no arguments 'procedure_name'

The LIKE [procedure] ARGUMENTS form creates a shape for use in a cursor or procedure arguments.

The indicated name is a procedure with no arguments so it cannot be used to make a shape.

-----

### CQL0263: non-ANSI joins are forbidden if strict join mode is enabled.

You can enable strict enforcement of joins to avoid the form

```sql
select * from A, B;
```

which sometimes confuses people (the above is exactly the same as

```sql
select * from A inner join B on 1;
```
Usually there are constraints on the join also in the WHERE clause but there don't have to be.

`@enforce_strict join` turns on this mode.

-----

### CQL0264: duplicate assembly fragments of base fragment
For each base fragment, it only allows to exist one assembly fragment of that base fragment.

-----

### CQL0265: assembly fragment can only have one CTE
Assembly fragment can only have one base table CTE.

-----

### CQL0266: extension fragment name conflicts with existing fragment
Two or more extension fragments share the same name.

-----

### CQL0267: extension fragments of same base fragment share the same cte column
Two or more extension fragments which have the same base fragments share the same name for one of their unique columns. E.g. the base table is `core(x,y)` and one extension table is `plugin_one(x,y,z)` and another is `plugin_two(x,y,z)`.
Here, z in both extension fragments share the same name but may refer to different values.

-----

### CQL0268: extension/assembly fragment must have the CTE columns same as the base fragment
 Extension and assembly fragments have an exact prescription for their shape. For each extension and assembly fragment, the first CTE must be a stub for their base table. This error means this stub in extension/assembly fragment differs from the definition of the base table.

-----

### CQL0269: at least part of this unique key is redundant with previous unique keys
The new unique key must have at least one column that is not in a previous key AND it must not have all the columns from any previous key.

e.g:
```sql
create table t1 (
  a int,
  b long,
  c text,
  d real,
  UNIQUE (a, b),
  UNIQUE (a, b, c), -- INVALID  (a, b) is already unique key
  UNIQUE (b, a), -- INVALID (b, a) is the same as (a, b)
  UNIQUE (c, d, b, a), -- INVALID subset (b, a) is already unique key
  UNIQUE (a), -- INVALID a is part of (a, b) key
  UNIQUE (a, c), -- VALID
  UNIQUE (d), -- VALID
  UNIQUE (b, d) -- VALID
);
```

-----

### CQL0270: use FETCH FROM for procedures that returns a cursor with OUT 'cursor'

If you are calling a procedure that returns a value cursor (using `OUT`) then you accept that cursor using the pattern

```sql
DECLARE C CURSOR FETCH FROM CALL foo(...);
```

The pattern

```sql
DECLARE C CURSOR FOR CALL foo(...);
```

Is used for procedures that provide a full `select` statement.

Note that in the former cause you don't then use `fetch` on the cursor.  There is at most one row anyway and it's fetched for you so a fetch would be useless.  In the second case you fetch as many rows as there are and/or you want.

-----

### CQL0271: OFFSET clause may only be used if LIMIT is also present

```sql
select * from foo offset 1;
```

Is not supported by SQLite.  `OFFSET` may only be used if `LIMIT` is also present.  Also, both should be small because offset is not cheap.  There is no way to do offset other than to read and ignore the indicated number of rows.  So something like `offset 1000` is always horrible.

-----

### CQL0272: columns referenced in the foreign key statement should match exactly a unique key in parent table

If you're creating a table t2 with foreign keys on table t1, then the set of t1's columns reference in the foreign key statement for table t2 should be:
- A primary key in `t1`
```sql
e.g:
create table t1(a text primary key);
create table t2(a text primary key, foreign key(a) references t1(a));
```
- A unique key in `t1`
```sql
e.g:
create table t1(a text unique);
create table t2(a text primary key, foreign key(a) references t1(a));
```
- A group of unique key in `t1`
```sql
e.g:
create table t1(a text, b int, unique(a, b));
create table t2(a text, b int, foreign key(a, b) references t1(a, b));
```
- A group of primary key in `t1`
```sql
e.g:
create table t1(a text, b int, primary key(a, b));
create table t2(a text, b int, foreign key(a, b) references t1(a, b));
```
- A unique index in `t1`
```sql
e.g:
create table t1(a text, b int);
create unique index unq on t1(a, b);
create table t2(a text, b int, foreign key(a, b) references t1(a, b));
```

-----

### CQL0273: autotest attribute has incorrect format (...) in 'dummy_test'

In a `cql:autotest` annotation, the given **dummy_test** info (table name, column name, column value) has incorrect format.

-----

### CQL0274: autotest attribute 'dummy_test' has non existent table

In a `cql:autotest` annotation, the given table name for **dummy_test** attribute does not exist.

-----

### CQL0275: autotest attribute 'dummy_test' has non existent column

In a `cql:autotest` annotation, the given column name for **dummy_test** attribute does not exist.

-----

### CQL0276: autotest attribute 'dummy_test' has invalid value type in

In a `cql:autotest` annotation, the given column value's type for **dummy_test** attribute does not match the column type.

-----

### CQL0277: autotest has incorrect format

In a `cql:autotest` annotation, the format is incorrect.

-----

### CQL0278: autotest attribute name is not valid

In a `cql:autotest` annotation, the given attribute name is not valid.

-----

### CQL0279: columns referenced in an UPSERT conflict target must exactly match a unique key the target table

If you're doing an UPSERT on table `T`, the columns listed in the conflict target should be:
- A primary key in `T`
- A unique key in `T`
- A group of unique key in `T`
- A group of primary key in `T`
- A unique index in `T`

-----

### CQL0280: upsert statement requires a where clause if the insert clause uses select

When the `INSERT` statement to which the UPSERT is attached takes its values from a `SELECT` statement, there is a potential parsing ambiguity. The SQLite parser might not be able to tell if the `ON` keyword is introducing the UPSERT or if it is the `ON` clause of a join. To work around this, the `SELECT` statement should always include a `WHERE` clause, even if that `WHERE` clause is just `WHERE 1` (always true).   Note: The CQL parser doesn't have this ambiguity because it treats "ON CONFLICT" as a single token so this is CQL reporting that SQLite might have trouble with the query as written.

e.g:
```sql
insert into foo select id from bar where 1 on conflict(id) do nothing;
```

-----

### CQL0281: upsert statement does not include table name in the update statement

The UPDATE statement of and UPSERT should not include the table name because the name is already known from the INSERT statement part of the UPSERT
e.g:
```sql
insert into foo select id from bar where 1 on conflict(id) do update set id=10;
```

-----

### CQL0282: update statement require table name

The UPDATE statement should always include a table name except if the UPDATE statement is part of an UPSERT statement.

e.g:
```sql
update foo set id=10;
insert into foo(id) values(1) do update set id=10;
```

-----

### CQL0283: upsert syntax only support INSERT INTO

The INSERT statement part of an UPSERT statement can only uses INSERT INTO ...
```sql
e.g:
insert into foo(id) values(1) on conflict do nothing;
insert into foo(id) values(1) on conflict do update set id=10;
```

-----

### CQL0284: ad hoc schema migration directive must provide a procedure to run

`@schema_ad_hoc_migration` must provide both a version number and a migrate procedure name.
This is unlike the other version directives like `@create` where the version number is optional.  This
is because the whole point of this migrator is to invoke a procedure of your choice.

-----

### CQL0285: ad hoc schema migration directive version number changed 'procedure_name'

In `@schema_ad_hoc_migration` you cannot change the version number of the directive once
it has been added to the schema because this could cause inconsistencies when upgrading.

You can change the body of the method if you need to but this is also not recommended because
again there could be inconsistencies.  However careful replacement and compensation is possible.  This is like
going to 110% on the reactor... possible, but not recommended.

-----

### CQL0286: ad hoc schema migration directive was removed; this is not allowed 'procedure_name'

An `@schema_ad_hoc_migration` cannot be removed because it could cause inconsistencies on upgrade.

You can change the body of the method if you need to but this is also not recommended because
again there could be inconsistencies.  However careful replacement and compensation is possible.  This is like
going to 110% on the reactor... possible, but not recommended.

-----

### CQL0287: extension/assembly fragment must add stub "

-----

### CQL0288: extension/assembly fragment stub for base CTE column must be "

-----

### CQL0289: upsert statement are forbidden if strict upsert statement mode is enabled

`@enforce_strict` has been use to enable strict upsert statement enforcement.  When enabled all sql statement should not use the upsert statement. This is because sqlite version running in some iOS and Android version is old. Upsert statement was added to sqlite in the version **3.24.0 (2018-06-04)**.

-----

### CQL0290: fragments can only have one statement in the statement list and it must be a WITH...SELECT

All of the extendable query fragment types consist of a procedure with exactly one statement and that statement is a WITH...SELECT statement.  If you have more than one statement or some other type of statement you'll get this error.

-----

### CQL0291: region links into the middle of a deployable region; you must point to the root of `<deployable_region>` not into the middle: `<error_region>`

Deployable regions have an "inside" that is in some sense "private".  In order to keep the consistent (and independently deployable) you can't peek into the middle of such
a region, you have to depend on the root (i.e. `<deployable_region>` itself).  This allows the region to remain independently deployable and for its internal logical regions to be reorganized in whatever manner makes sense.

To fix this error probably you should change `error_region` so that it depends directly on `deployable_region`

-----

### CQL0292: explain statement is only available in dev mode because its result set may vary between sqlite versions

The EXPLAIN statement is intended for interactive debugging only. It helps engineer understand how Sqlite will execute their query and the cost attached to it. This is why this grammar is only available in dev mode in CQL and should never be used in production.

-----

### CQL0293: only [EXPLAIN QUERY PLAN ...] statement is supported

CQL only support [EXPLAIN QUERY PLAN stmt] sql statement.

------

### CQL0294: window function invocations can only appear in the select list of a select statement

Not all SQLite builtin function can be used as a window function.

------

### CQL0295: window name is not defined

Window name referenced in the select list should be defined in the Window clause of the same select statement.

------

### CQL0296: window name definition is not used

Window name defined in the window clause of a select statement should always be used within that statement.

------

### CQL0297: FROM [shape] is redundant if column list is empty

In this form:

`insert into YourTable() FROM your_cursor;`

The `()` means no columns are being specified, the cursor will never be used.  The only source of columns is maybe dummy data (if it was specified) or the default values or null.  In no case will the cursor be used.  If you really want this use `FROM VALUES()` and don't implicate a cursor or an argument bundle.

------

### CQL0298: cannot read from a cursor without fields 'cursor_name'

The cursor in question has no storage associated with it.  It was loaded with something like:

`fetch C into x, y, z;`

You can only use a cursor as a source of data if it was fetched with its own storage like

`fetch C`

This results in a structure for the cursor.  This gives you C.x, C.y, C.z etc.

If you fetched the cursor into variables then you have to use the variables for any inserting.

------

### CQL0299: [cursor] has too few fields, 'shape_name'

The named shape was used in a fetch statement but the number of columns fetched is smaller than the number required by the statement we are processing.

If you need to use the cursor plus some other data then you can't use this form, you'll have to use each field individually like `from values(C.x, C.y, C.z, other_stuff)`.

The shape with too few fields might be the source or the target of the statement.

------

### CQL0300: argument must be an integer (between 1 and max integer) in function 'function_name'

The argument of the function should be an integer.

------

### CQL0301: second argument must be an integer (between 0 and max integer) in function 'function_name'

The second argument of the function should be an integer between 0 and INTEGER_MAX.

------

### CQL0302: first and third arguments must be compatible in function 'function_name'

The first and third arguments of the function have to be of the same type because the third argument provide a default value in cause the first argument is NULL.

------

### CQL0303: second argument must be an integer between 1 and max integer in function 'function_name'

The second argument of the function must be and integer between 1 and INTEGER_MAX.

------

### CQL0304: DISTINCT may only be used with one explicit argument in an aggregate function

The keyword DISTINCT can only be used with one argument in an aggregate function.

------

### CQL0305: DISTINCT may only be used in function that are aggregated or user defined

Only aggregated functions and user defined functions can use the keyword DISTINCT. Others type of functions are not allowed to use it.

------

### CQL0306: FILTER clause may only be used in function that are aggregated or user defined

------

### CQL0307: return statement should be in a procedure and not at the top level

There are basically two checks here both of which have to do with the "nesting level" at which the `return` occurs.

A loose `return` statement (not in a procedure) is meaningless so that produce an error.  There is nothing to return from.

If the return statement is not inside of an "if" or something like that then it will run unconditionally.  Nothing should follow the return (see CQL0308) so if we didn't fall afoul of CQL0308 and we're at the top level then the return is the last thing in the proc, in which case it is totally redundant.

Both these situations produce an error.

------

### CQL0308: statement should be the last thing in a statement list

Control flow will exit the containing procedure after a `return` statement, so any statements that follow in its statement list will certainly not run.  So the return statement must be the last statement, otherwise there are dead/unreachable statements which is most likely done by accident.

To fix this probably the things that came after the return should be deleted.  Or alternately there was a condition on the return that should have been added but wasn't, so the return should have been inside a nested statement list (like the body of an `if` maybe).

------

### CQL0309: new table must be added with @create([number]) or later 'table_name'

The indicated table was newly added -- it is not present in the previous schema.  However the version number it was added at is in the past.  The new table must appear at the current schema version or later.  That version is provided in the error message.

To fix this, change the `@create` annotation on the table to be at the indicated version or later.

------

### CQL0310: new column must be added with @create([number]) or later" 'column_name'

The indicated column was newly added -- it is not present in the previous schema.  However the version number it was added at is in the past.  The new column must appear at the current schema version or later.  That version is provided in the error message.

To fix this, change the `@create` annotation on the table to be at the indicated version or later.

------

### CQL0311: object's deployment region changed from '<previous_region>' to '<current_region>' 'object_name'

An object may not move between deployment regions, because users of the schema will depend on its contents.  New objects can be added to a deployment region but nothing can move from one region to another.  The indicated object appears to be moving.

------

### CQL0312: window function invocation are forbidden if strict window function mode is enabled

`@enforce_strict` has been use to enable strict window function enforcement.  When enabled all sql statement should not invoke window function. This is because sqlite version running in some iOS version is old. Window function was added to SQLite in the version **3.25.0 (2018-09-15)**.

------

### CQL0313: blob literals may only appear in the context of a SQL statement

CQL (currently)  limits use of blob literals to inside of SQL fragments.  There's no easy way to get a blob
constant variable into the data section so any implementation would be poor.  These don't come up very often
in any case so this is a punt basically.  You can fake it with (select x'1234') which makes it clear that
you are doing something expensive.  This is not recommended.  Better to pass the blob you need into CQL
rather than cons it from a literal.  Within SQL it's just text and SQLite does the work as usual so that poses no
problems.  And of course non-literal blobs (as args) work find and are bound as usual.

------

### CQL0314: select function does not require a declaration, it is a CQL built-in

CQL built-in function does not require a select function declaration. You can used it directly in your SQL statement.

------

### CQL0315: mandatory column with no default value in INSERT INTO name DEFAULT VALUES statement.

Columns on a table must have default value or be nullable in order to use INSERT INTO `<table>` DEFAULT VALUES statement.

------

### CQL0316: upsert-clause is not compatible with DEFAULT VALUES

INSERT statement with DEFAULT VALUES can not be used in a upsert statement.  This form is not supported by SQLite.

-----

### CQL0317: char function arguments must be integer

All parameters of the built-In scalar CQL functions `char(...)` must be of type integer.

-----

### CQL0318: more than one fragment annotation on procedure 'procedure_name'

The indicated procedure has several cql:*_fragment annotations such as cql:base_fragment and cql:extension_fragment.  You can have at most one of these.

example:

```sql
@attribute(cql:assembly_fragment=foo)
@attribute(cql:base_fragment=goo)
create proc mixed_frag_types3(id_ integer)
begin
  ...
end;
```

-----

### CQL0319: name of the assembly procedure must match the name of the base fragment 'procedure_name'

The name of the procedure that carries the assembly attribute (cql:assembly_fragment) has to match the name of the base fragment.  This is because the code that is generated for the extension fragments refers to some shared code that is generated in the assembly fragment.  If the assembly fragment were allowed to have a distinct name the linkage could never work.

example:
```sql
-- correct example
-- note: 'foo' must be a valid base fragment, declared elsewhere
@attribute(cql:assembly_fragment=foo)
create proc foo(id_ integer)
begin
  ...
end;

-- incorrect example
@attribute(cql:assembly_fragment=foo)
create proc bar(id_ integer)
begin
  ...
end;
```

-----

### CQL0320: extension fragment CTE must have a FROM clause and no other top level clauses 'frag_name'

In the extension fragment form that uses `LEFT OUTER JOIN` to add columns you cannot include top level restrictions/changes like `WHERE`, `ORDER BY`, `LIMIT` and so forth.  Any of these would remove or reorder the rows from the core fragment and that is not allowed, you can only add columns.  Hence you must have a `FROM` clause and you can have no other top level clauses.  You can use any clauses you like in a nested select to get your additional columns.

Note: you could potentially add rows with a `LEFT OUTER JOIN` and a generous `ON` clause. That's allowed but not recommended. The `ON` clause can't be forbidden because it's essential in the normal case.

-----

### CQL0321: migration proc not allowed on object 'object_name'

The indicated name is an index or a trigger. These objects may not have a migration script associated with them when they are deleted.

The reason for this is that both these types of objects are attached to a table and the table itself might be deleted.  If the table is deleted it becomes impossible to express even a tombstone for the deleted trigger or index without getting errors.  As a consequence the index/trigger must be completely removed.  But if there had been a migration procedure on it then some upgrade sequences would have run it, but others would not (anyone who upgraded after the table was deleted would not get the migration procedure).  To avoid this problem, migration procedures are not allowed on indices and triggers.

-----

### CQL0322: fragment parameters must be exactly '[arguments]' 'procedure_name'

The named procedure is an extension fragment or an assembly fragment. It must have exactly the same arguments as the base fragment.  These arguments are provided.

Recall that the code for the procedure that does the select comes from the assembly fragment, so its arguments are in some sense the only ones that matter.  But the extension fragments are also allowed to use the arguments.  Of course we must ensure that the extension fragments do not use any arguments that aren't in the assembly, and so the only choice we have is to make sure the extensions conform to the base.  And so for that to work the assembly also has to conform to the base.  So the base fragment must define the args for all the other fragments.

You could imagine a scheme where the extension fragments are allowed to use a subset of the parameters defined in the base but if that were the case you might have names that mean different things in different fragments and then you could get errors or different semantics when the fragments were assembled. To avoid all of these problems, and for simplicity, we demand that the arguments of all fragments match exactly.

-----

### CQL0323: calls to undeclared procedures are forbidden; declaration missing or typo 'procedure'

If you get this error it means that there is a typo in the name of the procedure you are trying to call, or else the declaration for the
procedure is totally missing.  Maybe a necessary `#include` needs to be added to the compiland.

Previously if you attempted to call an unknown CQL would produce a generic function call. If you need to do this, especially a function with varargs,
then you must declare the function  with something like:

`DECLARE PROCEDURE printf NO CHECK;`

This option only works for void functions.  For more complex signatures check `DECLARE FUNCTION` and `DECLARE SELECT FUNCTION`.  Usually these will require a
simple wrapper to call from CQL.

In all cases there must be some kind of declaration,to avoid mysterious linker failures or argument signature mismatches.

-----

### CQL0324: referenced table was created in a later version so it cannot be used in a foreign key 'referenced_table'

In a foreign key, we enforce the following rules:
* `@recreate` tables can see any version they like, if the name is in scope that's good enough
* other tables may only "see" the same version or an earlier version.

Normal processing can't actually get into this state because if you tried to create the referencing
table with the smaller version number first you would get errors because the name of the referenced
table doesn't yet exist.  But if you created them at the same time and you made a typo in the version
number of the referenced table such that it was accidentally bigger you'd create a weirdness.
So we check for that situation here and reject it to prevent that sort of typo.

If you see this error there is almost certainly a typo in the version number of the referenced table; it should be fixed.

-----

### CQL0325: ok_table_scan attribute must be a name

The values for the attribute `ok_table_scan` can only be names.

CQL attributes can have a variety of values but in this case the attribute refers to the names
of tables so no other type of attribute is reasonable.

-----

### CQL0326: table name in ok_table_scan does not exist 'table_name'

The names provided to `ok_table_scan` attribute should be names of existing tables.

The attribute indicates tables that are ok to scan in this procedure even though they
are typically not ok to scan due to 'no_table_scan'.  Therefore the attribute must
refer to an existing table.  There is likely a typo in the the table name that needs
to be corrected.

-----

### CQL0327: a value should not be assigned to 'attribute_name' attribute

The attribute `attribute_name` doesn't take a value.

When marking a statement with `@attribute(cql:<attribute_name>)` there is no need
for an attribute value.

-----

### CQL0328: 'attribute_name' attribute may only be added to a 'statement_name'

The `attribute_name` attribute can only be assigned to specific statements.

The marking `@attribute(cql:<attribute_name>)` only makes sense on specific statement. It's likely
been put somewhere strange, If it isn't obviously on the wrong thing, look into
possibly how the source is after macro expansion.

-----

### CQL0329: ok_table_scan attribute can only be used in a create procedure statement

The `ok_table_scan` can only be placed on a create procedure statement.

The marking `@attribute(cql:ok_table_scan=...)` indicates that the procedure may scan
the indicated tables. This marking doesn't make sense on other kinds of statements.

-----

### CQL0330: fragment must start with exactly 'SELECT * FROM CTE'

Query fragments have an exact prescription for their shape.  This prescription includes `select * from CTE` in the
first branch of the UNION ALL operator when using that form.  Here CTE
is the common table expression that they define.  This the error message includes the
specific name that is required in this context.

-----

### CQL0331: extension fragment CTE must have not have ORDER BY or LIMIT clauses 'frag_name'

In the extension fragment form that uses `UNION ALL` to add rows you cannot include the top level operators `ORDER BY`, or `LIMIT`  Any of these would remove or reorder the rows from the core fragment and that is not allowed, you can only add new rows.
You can use any clauses you like in a nested selects as they will not remove rows from the base query.

-----

### CQL0332: all extension fragments that use UNION ALL must come before those that use LEFT OUTER JOIN 'frag_name'

Query fragments that add rows using the UNION ALL form have no way to refer columns that may have been added before
them in the part of the query that adds rows (the second and subsequent branches of UNION ALL).  As a result,
in order to get a assembled query that makes sense the row-adding form must always come before any columns
were added.  Hence all of these fragments must come before any of the LEFT OUTER JOIN form.

If you get this error, you should re-order your fragments such that the UNION ALL form comes before any
LEFT OUTER JOIN fragments.  The offending fragment is named in the error.

-----

### CQL0333: all the compound operators in this CTE must be UNION ALL

The compound operators in CTE must and always be an UNION ALL.

-----

### CQL0334: @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row

Dummy insert feature makes only sense when it's used in a VALUES clause that is not part of a compound select statement.

### CQL0336: select statement with VALUES clause requires a non empty list of values

VALUES clause requires at least a value for each of the values list. Empty values list are not supported.

-----

### CQL0337: number of columns values for each row should be identical in VALUES clause

The number of values for each values list in VALUES clause should always be the same.

-----

### CQL0338: name of a migration procedure may not end in '_crc' 'procedure_name'

To avoid name conflicts in the upgrade script, migration procedures are not allowed to end in '_crc'
this suffix is reserved for internal use.

-----

### CQL0339: WITHOUT ROWID tables are forbidden if strict without rowid mode is enabled

`@enforce_strict` has been used to enable strict `WITHOUT ROWID` enforcement. When enabled no CREATE TABLE statement can have WITHOUT ROWID clause.

-----

### CQL0340: FROM ARGUMENTS used in a procedure with no arguments 'procedure_name'

The named procedure has a call that uses the FROM ARGUMENTS pattern but it doesn't have any arguments.
This is almost certainly a cut/paste from a different location that needs to be adjusted.

-----

### CQL0341: argument must be a variable in function 'function_name'

The argument for the CQL builtin function 'function_name' should always be a variable. It can not be an expression for example

-----

### CQL0342: cursor arguments must have identical column count 'function_name'

The number of column in the cursor arguments must be identical to accurately do diffing between two cursors.

-----

### CQL0343: all arguments must be blob 'cql_get_blob_size'

The argument for the CQL builtin function cql_get_blob_size should always be of type blob

-----

### CQL0344: argument must be a nullable type (but not constant NULL) in 'function'

Functions like `ifnull_crash` only make sense if the argument is nullable.  If it's already
not null the operation is uninteresting/redundant.

The most likely cause is that the function call in question is vestigial and you can simply remove it.

-----

### CQL0345: arguments must be of type blob 'function_name'

The indicated function accepts only a single argument of type blob.

-----

### CQL0346: expression must be of type `object<T cursor>` where T is a valid shape name 'variable'

It's possible to take the statement associated with a statement cursor and store it in an object variable. Using the form:

```sql
declare C cursor for X;
```

The object variable 'X' must be declared as follows:

```sql
declare X object<T cursor>;
```

Where `T` refers to a named object with a shape, like a table, a view, or a stored procedure that returns a result set. This type `T` must match the shape of the cursor exactly i.e. having the column names and types.

The reverse operation, storing a statement cursor in a variable is also possible with this form:

```sql
set X from cursor C;
```

This has similar constraints on the variable `X`.

This error indicates that the variable in question (`X` in this example) is not a typed object variable so it can't be the source of a cursor, or accept a cursor.

See Chapter 5 of the CQL Programming Language.

-----

### CQL0347: select function may not return type OBJECT 'function_name'

The indicated function was declared with `DECLARE SELECT FUNCTION` meaning it is to be used in the
context of SQLite statements. However, SQLite doesn't understand functions that return type object
at all.  Therefore declaration is illegal.

When working with pointer type through SQLite it is often possibly to encode the object as an long integer assuming
it can pass through unchanged with no retain/release semantics or any such thing.  If that is practical
you can move objects around by returning long integers.

----

### CQL0348: collate applied to a non-text column 'column_name'

Collation order really only makes sense on text fields.  Possibly blob fields but we're taking a stand on blob
for now.  This can be relaxed later if that proves to be a mistake.  For now, only text

----

### CQL0349: column definitions may not come after constraints 'column_name'

In a CREATE TABLE statement, the indicated column name came after a constraint.  SQLite expects all the column definitions
to come before any constraint definitions.  You must move the offending column definition above the constraints.

----

### CQL0350: statement must appear inside of a PROC SAVEPOINT block

The `ROLLBACK RETURN` and `COMMIT RETURN` forms are only usable inside of a `PROC SAVEPOINT` block
because they rollback or commit the savepoint that was created at the top level.

----

### CQL0351: statement should be in a procedure and at the top level

The indicated statement may only appear inside procedure and not nested.  The classic example
of this is the `PROC SAVEPOINT` form which can only be used at the top level of procedures.

----

### CQL0352: use COMMIT RETURN or ROLLBACK RETURN in within a proc savepoint block

The normal `RETURN` statement cannot be used inside of `PROC SAVEPOINT` block, you have to
indicate if you want to commit or rollback the savepoint when you return.  This makes
it impossible to forget to do so which is in some sense the whole point of `PROC SAVEPOINT`.

----

### CQL0353: evaluation of constant failed

The constant expression could not be evaluated.  This is most likely because it includes an operator that is
not supported or a function call which is not support.  Very few functions can be used in constant expressions
The supported functions include `iif`, which is rewritten; `abs`; `ifnull`, `nullif`, and `coalesce`.

----

### CQL0354: duplicate enum member 'enum_name'

While processing a `declare enum` statement the indicated member of the enum appeared twice.

This is almost certainly a copy/paste of the same enum member twice.

----

### CQL0355: evaluation failed 'enum_name'

While processing a `declare enum` statement the indicated member of the enum could not be evaluated as a constant expression.

There could be a non-constant in the expression or there could be a divide-by-zero error.

----

### CQL0356: enum definitions do not match 'name'

The two described `declare enum` statements have the same name but they are not identical.

The error output contains the full text of both declarations to compare.

----

### CQL0357: enum does not contain 'enum_name'

The indicated member is not part of the enumeration.

----

### CQL0358: declared enums must be top level 'enum'

A `DECLARE ENUM` statement for the named enum is happening inside of a procedure.  This is not legal.

To correct this move the declaration outside of the procedure.

----

### CQL0359: duplicate type declaration 'type_name'

The name of a declared type should always be unique.

----

### CQL0360: unknown type 'type_name'

The indicated name is not a valid type name.

----

### CQL0361: return data type in a create function declaration can only be Text, Blob or Object

Return data type in a create function definition can only be TEXT, BLOB or OBJECT.

These are the only reference types and so CREATE makes sense only with those types.  An integer, for instance, can't start with a +1 reference count.

----

### CQL0362: HIDDEN column attribute must be the first attribute if present

In order to ensure that SQLite will parse HIDDEN as part of the type it has to come before any other attributes like NOT NULL.

This limitation is due to the fact that CQL and SQLite use slightly different parsing approaches for attributes and in SQLite
HIDDEN isn't actually an attribute.  The safest place to put the attribute is right after the type name and before any other
attributes as it is totally unambiguous there so CQL enforces this.

----

### CQL0363: all arguments must be names 'vault_sensitive'

vault_sensitive attribution only allow names. Integer, string literal, c string or blob are not allowed, only IDs should be provided.

----

### CQL0364: vault_sensitive annotation can only go on a procedure that uses the database

The named procedure has the `vault_sensitive` annotation to automatically encode sensitive value in the result set. Encoding value require the database, but the procedure in question doesn't even use the database at all.  This annotation is therefore useless.

----

### CQL0365: @enforce_pop used but there is nothing to pop

Each `@enforce_pop` should match an `@enforce_push`, but there is nothing to pop on the stack now.

----

### CQL0366: transaction operations disallowed while STRICT TRANSACTION enforcement is on

`@enforce_strict transaction` has been used, while active no transaction operations are allowed.  Savepoints may be used.  This is
typically done to prevent transactions from being used in any ad hoc way because they don't nest and typically need to be used
with some "master plan" in mind.

----

### CQL0367: an attribute was specified twice 'attribute_name'

In the indicated type declaration, the indicated attribute was specified twice.  This is almost certainly happening because the line in question looks like this
`declare x type_name not null;` but `type_name` is already `not null`.

----

### CQL0368: strict select if nothing requires that all (select ...) expressions include 'if nothing'

`@enforce_strict select if nothing` has been enabled.  This means that select expressions must include
`if nothing throw` (the old default) `if nothing [value]` or `if nothing or null [value]`.  This options exists
because commonly the case where a row does not exist is not handled correctly when `(select ...)` is used
without the `if nothing` options.

If your select expression uses a [built-in aggregate function](https://www.sqlite.org/lang_aggfunc.html), this check may not be enforced because they can always return a row. But there are exceptions. The check is still enforced when one of the following is in the expression:
- a `GROUP BY` clause
- a `LIMIT` that evaluates to less than 1, or is a variable
- an `OFFSET` clause
- You have a `min` or `max` function with more than 1 argument. Those are [scalar functions](https://sqlite.org/lang_corefunc.html#max_scalar).

----

### CQL0369: (SELECT ... IF NOTHING) construct is for use in top level expressions, not inside of other DML

This form allows for error control of (select...) expressions.  But SQLite does not
understand the form at all, so it can only appear at the top level of expressions where
CQL can strip it out. Here are some examples:

good:

```sql
  set x := (select foo from bar where baz if nothing 0);
  if (select foo from bar where baz if nothing 1) then ... end if;
```

bad:

```sql
  select foo from bar where (select something from somewhere if nothing null);
  delete from foo where (select something from somewhere if nothing 1);
```

Basically if you are already in a SQL context, the form isn't usable because SQLite
simply doesn't understand if nothing at all. This error makes it so that you'll
get a build time failure from CQL rather than a run time failure from SQLite.

----

### CQL0370: due to a memory leak bug in old SQLite versions, the select part of an insert must not have a top level join or compound operator. Use WITH and a CTE, or a nested select to work around this.

There is an unfortunate memory leak in older versions of SQLite (research pending on particular versions, but 3.28.0 has it).  It causes this pattern to leak:

```sql
-- must be autoinc table
create table x (
  pk integer primary key autoincrement
);

-- any join will do (this is a minimal repro)
insert into x
  select NULL pk from
  (select 1) t1 inner join (select 1) t2;
```

You can workaround this with a couple of fairly simple rewrites.  This form is probably the cleanest.

```sql
with
cte (pk) as (select .. anything you need)
insert into x
  select * from cte;
```

Simply wrapping your desired select in a nested select also suffices.  So long as the top level is simple.

```sql
insert into x
  select * from (
    select anything you need....
  );
```

----

### CQL0371: table valued function used in a left/right/cross context; this would hit a SQLite bug.  Wrap it in a CTE instead.

This error is generated by `@enforce_strict table function`. It is there to allow safe use of Table Valued Functions (TVFs)
even though there was a bug in SQLite prior to v 3.31.0 when joining against them.  The bug appears when the TVF is on
the right of a left join. For example:

```sql
select * from foo left join some_tvf(1);
```

In this case the join becomes an INNER join even though you wrote a left join.  Likewise

```sql
select * from some_tvf(1) right join foo;
```

Becomes an inner join even though you wrote a right join.  The same occurs when a TVF is on either side of a cross join.

The workaround is very simple.  You don't want the TVF to be the target of the join directly.  Instead:

```sql
with tvf_(*) as (select * from some_tvf(1))
select * from foo left join tvf_;
```

OR

```sql
select * from foo left join (select * from some_tvf(1));
```

----

### CQL0372: SELECT ... IF NOTHING OR NULL NULL is redundant; use SELECT ... IF NOTHING NULL instead.

It is always the case that `SELECT ... IF NOTHING OR NULL NULL` is equivalent to `SELECT ... IF NOTHING NULL`. As such, do not do this:

```sql
select foo from bar where baz if nothing or null null
```

Do this instead:

```sql
select foo from bar where baz if nothing null
```

----

### CQL0373: comparing against NULL always yields NULL; use IS and IS NOT instead.

Attempting to check if some value `x` is NULL via `x = NULL` or `x == NULL`, or isn't NULL via `x <> NULL` or `x != NULL`, will always produce NULL regardless of the value of `x`. Instead, use `x IS NULL` or `x IS NOT NULL` to get the expected boolean result.

----

### CQL0374: SELECT expression is equivalent to NULL.

CQL found a redundant select operation (e.g., `set x := (select NULL);`).

There is no need to write a select expression that always evaluates to NULL. Simply use NULL instead (e.g., `set x := NULL;`).

----

CQL 0375 : unused, this was added to prevent merge conflicts at the end on literally every checkin

----

CQL 0376 : unused, this was added to prevent merge conflicts at the end on literally every checkin

----

### CQL0377: table transitioning from `@recreate` to `@create` must use `@create(nn,cql:from_recreate)` 'table name'

The indicated table is moving from `@recreate` to `@create` meaning it will now be schema managed in an
upgradable fashion.  When this happens end-user databases might have some stale version of the table
from a previous installation.  This stale version must get a one-time cleanup in order to ensure that the
now current schema is correctly applied.  The `cql:from_recreate` annotation does this.  It is required
because otherwise there would be no record that this table "used to be recreate" and therefore might have
an old version still in the database.

A correct table might look something like this:

```sql
create table correct_migration_to_create(
 id integer primary key,
 t text
) @create(7, cql:from_recreate);
```

----

### CQL0378: built-in migration procedure not valid in this context 'name'

The indicated name is a valid built-in migration procedure but it is not valid on
this kind of item.  For instance `cql:from_recreate` can only be applied to tables.

----

### CQL0379: unknown built-in migration procedure 'name'

Certain schema migration steps are built-in.  Currently the only one is `cql:from_recreate` for
moving to `@create` from `@recreate`.  Others may be added in the future.  The `cql:` prefix
ensures that this name cannot conflict with a valid user migration procedure.

----

### CQL0380: WHEN expression cannot be evaluated to a constant

In a `SWITCH` statement each expression each expression in a `WHEN` clause must be made up
of constants and simple numeric math operations.  See the reference on the `const(..)` expression
for the valid set.

It's most likely that a variable or function call appears in the errant expression.

----

### CQL0381: case expression must be a not-null integral type

The `SWITCH` statement can only switch over integers or long integers.  It will be translated
directly to the C switch statement form.  `TEXT`, `REAL`, `BLOB`, `BOOL`, and `OBJECT` cannot
be used in this way.

----

### CQL0382: type of a WHEN expression is bigger than the type of the SWITCH expression

The `WHEN` expression evaluates to a `LONG INTEGER` but the expression in the `SWITCH` is `INTEGER`.

----

### CQL0383: switch ... ALL VALUES is useless with an ELSE clause

The `ALL VALUES` form of switch means that:
 * the `SWITCH` expression is an enumerated type
 * the `WHEN` cases will completely cover the values of the enum

If you allow the `ELSE` form then `ALL VALUES` becomes meaningless because of
course they are all covered.  So with `ALL VALUES` there can be no `ELSE`.

You can list items that have no action with this form:

```sql
   WHEN 10, 15 THEN NOTHING -- explicitly do nothing in these cases so they are still covered
```

No code is generated for such cases.

----

### CQL0384: switch statement did not have any actual statements in it

Either there were no `WHEN` clauses at all, or they were all `WHEN ... THEN NOTHING` so
there is no actual code to execute.  You need to add some cases that do work.

----

### CQL0385: WHEN clauses contain duplicate values 'value'

In a `SWITCH` statement all of the values in the `WHEN` clauses must be unique.  The indicated
errant entry is a duplicate.

----

### CQL0386: SWITCH ... ALL VALUES is used but the switch expression is not an enum type

In a `SWITCH` statement with `ALL VALUES` specified the switch expression was not an enumerated type.
`ALL VALUES` is used to ensure that there is a case for every value of an enumerated type
so this switch cannot be so checked.  Either correct the expression, or remove `ALL VALUES`.

----

### CQL0387: a value exists in the enum that is not present in the switch 'enum_member'

In a `SWITCH` statement with `ALL VALUES` specified the errant enum member did not appear
in any `WHEN` clause.  All members must be specified when `ALL VALUES` is used.

----

### CQL0388: a value exists in the switch that is not present in the enum 'numeric_value'

In a `SWITCH` statement with `ALL VALUES` specified the errant integer value appeared in
in a `WHEN` clause.  This value is not part of the members of the enum.  Note that enum members
that begin with '_' are ignored as they are, by convention, considered to be pseudo-members.
e.g. in `declare enum v integer (v0 = 0, v1 =1, v2 =2, _count = 3)` `_count` is a pseudo-member.

The errant entry should probably be removed. Alternatively, `ALL VALUES` isn't appropriate as the
domain of the switch is actually bigger than the domain of the enumeration.  One of these
changes must happen.

----

### CQL0389: DECLARE OUT requires that the procedure be already declared

The purpose of the `DECLARE OUT` form is to automatically declare the out parameters for that procedure.

This cannot be done if the type of the procedure is not yet known.

----

### CQL0390: DECLARE OUT CALL used on a procedure with no missing OUT arguments

The `DECLARE OUT CALL` form was used, but the procedure has no `OUT` arguments that need
any implicit declaration.  Either they have already all been declared or else there are no
`OUT` arguments at all, or even no arguments of any kind.

----

### CQL0391: CLOSE cannot be used on a boxed cursor

When a cursor is boxed—i.e., wrapped in an object—the lifetime of the box and underlying statement are automatically managed via reference counting. Accordingly, it does not make sense to manually call CLOSE on such a cursor as it may be retained elsewhere. Instead, to allow the box to be freed and the underlying statement to be finalized, set all references to the cursor to NULL.

Note: As with all other objects, boxed cursors are automatically released when they fall out of scope. You only have to set a reference to NULL if you want to release the cursor sooner, for some reason.

----

### CQL0392: when deleting a virtual table you must specify @delete(nn, cql:module_must_not_be_deleted_see_docs_for_CQL0392) as a reminder not to delete the module for this virtual table

When the schema upgrader runs, if the virtual table is deleted it will attempt to do `DROP TABLE IF EXISTS` on the indicated table.  This table
is a virtual table.  SQLite will attempt to initialize the table even when you simply try to drop it.  For that to work the module must still
be present.  This means modules can never be deleted!  This attribute is here to remind you of this fact so that you are not tempted to
delete the module for the virtual table when you delete the table.  You may, however, replace it with a shared do-nothing stub.

The attribute itself does nothing other than hopefully cause you to read this documentation.

----

### CQL0393: not deterministic user function cannot appear in a constraint expression 'function_name'

`CHECK` expressions and partial indexes (`CREATE INDEX` with a `WHERE` clause) require that the expressions
be deterministic.  User defined functions may or may not be deterministic.

Use @attribute(cql:deterministic) on a UDF declaration (declare select function...) to mark it deterministic and allow its use in an index.

----

### CQL0394: nested select expressions may not appear inside of a constraint expression

SQLite does not allow the use of correlated subqueries or other embedded select statements inside of
a CHECK expression or the WHERE clauses of a partial index.  This would require additional joins
on every such operation which would be far too expensive.

----

### CQL0395: table valued functions may not be used in an expression context 'function_name'

A table valued function should be used like a table e.g.

```sql
-- this is right
select * from table_valued_func(5);
```

Not like a value e.g.

```sql
-- this is wrong
select table_valued_func(5);

-- this is also wrong
select 1 where table_valued_func(5) = 3;
```

----

### CQL0396: versioning attributes may not be used on DDL inside a procedure

If you are putting DDL inside of a procedure then that is going to run regardless of any `@create`,
`@delete`, or `@recreate` attributes;

DDL in entires do not get versioning attributes, attributes are reserved for schema declarations outside
of any procedure.

----

### CQL0397: object is an orphan because its table is deleted. Remove rather than @delete 'object_name'

This error is about either a trigger or an index. In both cases you are trying to use `@delete` on the index/trigger
but the table that the named  object is based on is itself deleted, so the object is an orphan.
Because of this, the orphaned object doesn't need, or no longer needs, an `@delete` tombstone because
when the table is dropped, all of its orphaned indices and triggers will also be dropped.

To fix this error, remove the named object entirely rather than marking it `@delete`.

Note: if the index/trigger was previously deleted and now the table is also deleted, it is now safe to remove
the index/trigger `@delete` tombstone and this error reminds you to do so.

----

### CQL0398: a compound select cannot be ordered by the result of an expression

When specifying an `ORDER BY` for a compound select, you may only order by indices (e.g., `3`) or names (e.g., `foo`) that correspond to an output column, not by the result of an arbitrary expression (e.g., `foo + bar`).

For example, this is allowed:

```sql
SELECT x, y FROM t0 UNION ALL select x, y FROM t1 ORDER BY y
```

The equivalent using an index is also allowed:

```sql
SELECT x, y FROM t0 UNION ALL select x, y FROM t1 ORDER BY 2
```

This seemingly equivalent version containing an arbitrary expression, however, is not:

```sql
SELECT x, y FROM t0 UNION ALL select x, y FROM t1 ORDER BY 1 + 1;
```

----

### CQL0399: table must leave @recreate management with @create(nn) or later 'table_name'

The indicated table changed from `@recreate` to `@create` but it did so in a past schema version.  The change
must happen in the current schema version.  That version is indicated by the value of nn.

To fix this you can change the `@create` annotation so that it matches the number in this error message

----

### CQL0400: encode context column can't be sensitive

The encode context column will be used to encode sensitive fields, it can't be exposed to encode functions

----

### CQL0401: encode context column must be specified if strict encode context column mode is enabled

encode context column must be specified in vault_sensitive attribute with format:
@attribute(cql:vault_sensitive=(encode_context_col, (col1, col2, ...))

----

### CQL0402: encode context column in vault_sensitive attribute must match the specified type in strict mode

encode context column must match the specified type in vault_sensitive attribute with format:
@attribute(cql:vault_sensitive=(encode_context_col, (col1, col2, ...))

----

### CQL0403: operator may not be used because it is not supported on old versions of SQLite, 'operator'

The indicated operator has been suppressed with `@enforce_strict is true` because it is not available
on older versions of sqlite.

----

### CQL0404: procedure cannot be both a normal procedure and an unchecked procedure, 'procedure_name'

The construct:

```sql
DECLARE PROCEDURE printf NO CHECK;
```

Is used to tell CQL about an external procedure that might take any combination of arguments.  The canonical example is
`printf`. All the arguments are converted from CQL types to basic C types when making the call (e.g. TEXT variables
become temporary C strings).  Once a procedure has been declared in this way it can't then also be declared as a
normal CQL procedure via `CREATE` or `DECLARE PROCEDURE`.  Likewise a normal procedure can't be redeclared with the `NO CHECK`
pattern.

----

### CQL0405: procedure of an unknown type used in an expression 'procedure_name'

If a procedure has no known type—that is, it was originally declared with `NO
CHECK`, and has not been subsequently re-declared with `DECLARE FUNCTION` or
`DECLARE SELECT FUNCTION`—it is not possible to use it in an expression. You
must either declare the type before using it or call the procedure outside of an
expression via a `CALL` statement:

```sql
DECLARE PROCEDURE some_external_proc NO CHECK;

-- This works even though `some_external_proc` has no known type
-- because we're using a CALL statement.
CALL some_external_proc("Hello!");

DECLARE FUNCTION some_external_proc(t TEXT NOT NULL) INT NOT NULL;

-- Now that we've declared the type, we can use it in an expression.
let result := some_external_proc("Hello!");
```

----

### CQL0406: substr uses 1 based indices, the 2nd argument of substr may not be zero"

A common mistake with substr is to assume it uses zero based indices like C does.  It does not.  In fact
the result when using 0 as the second argument is not well defined.  If you want the first
`n` characters of a string you use `substr(haystack, 1, n)`.

----

CQL 0407 : unused, this was added to prevent merge conflicts at the end on literally every checkin

----

### CQL0408: encode context column can be only specified once

The encode context column can be only specified once in @vault_sensitive attribute

----

### CQL0409: cannot use IS NULL or IS NOT NULL on a value of a NOT NULL type 'nonnull_expr'

If the left side of an `IS NULL` or `IS NOT NULL` expression is of a `NOT NULL`
type, the answer will always be the same (`FALSE` or `TRUE`, respectively). Such
a check often indicates confusion that may lead to unexpected behavior (e.g.,
checking, incorrectly, if a cursor has a row via `cursor IS NOT NULL`).

**NOTE**: Cursor fields of cursors without a row and uninitialized variables of
a NOT NULL reference type are exceptions to the above rule: Something may be
NULL even if it is of a NOT NULL type in those cases. CQL will eventually
eliminate these exceptions. In the cursor case, one can check whether or not a
cursor has a row by using the cursor-as-boolean-expression syntax (e.g., `IF
cursor THEN ... END IF;`, `IF NOT cursor ROLLBACK RETURN;`, et cetera). In the
uninitialized variables case, writing code that checks for initialization is not
recommended (and, indeed, use before initialization will soon be impossible
anyway): One should simply always initialize the variable.

----

CQL 0410 : unused, this was added to prevent merge conflicts at the end on literally every checkin

----

### CQL0411: duplicate flag in substitution 'flag'

The same flag cannot be used more than once per substitution within a format
string.

----

### CQL0412: cannot combine '+' flag with space flag

It is not sensible to use both the `+` flag and the space flag within the same
substitution (e.g., `%+ d`) as it is equivalent to just using the `+` flag
(e.g., `%+d`).


----

### CQL0413: width required when using flag in substitution 'flag'

The flag used (`-` or `0`) for a substitution within a format string does not
make sense unless accompanied by a width (e.g., `%-10d`).

----

### CQL0414: 'l' length specifier has no effect; consider 'll' instead

The use of the `l` length specifier within a format string, e.g. `%ld`, has no
effect in SQLite. If the argument is to be a `LONG`, use `ll` instead (e.g.,
`%lld`). If the argument is to be an `INTEGER`, simply omit the length specifier
entirely (e.g., `%d`).

----

### CQL0415: length specifier cannot be combined with '!' flag

Length specifiers are only for use with integer type specifiers (e.g. `%lld`)
and the `!` flag is only for use with non-integer type specifiers (e.g. `%!10s`
and `%!f`). It therefore makes no sense to use both within the same
substitution.

----

### CQL0416: type specifier not allowed in CQL 'type_specifier'

The type specifier used is accepted by SQLite, but it would be either useless or
unsafe if used within the context of CQL.

----

### CQL0417: unrecognized type specifier 'type_specifier'

The type specifier used within the format string is not known to SQLite.

----

### CQL0418: type specifier combined with inappropriate flags 'type_specifier'

The type specifier provided does not make sense given one or more flags that
appear within the same substitution. For example, it makes no sense to have a
substitution like `%+u`: the `+` indicates the sign of the number will be shown,
while the `u` indicates the number will be shown as an unsigned integer.

----

### CQL0419: type specifier cannot be combined with length specifier 'type_specifier'

The type specifier provided cannot be used with a length specifier. For example,
`%lls` makes no sense because `ll` only makes sense with integer types and `s`
is a type specifier for strings.

----

### CQL0420: incomplete substitution in format string

The format string ends with a substitution that is incomplete. This can be the
case if a format string ends with a `%` (e.g., `"%d %s %"`). If the intent is to
have a literal `%` printed, use `%%` instead (e.g., "%d %s %%"`).

----

### CQL0421: first argument must be a string literal 'function'

The first argument to the function must be a string literal.

----

### CQL0422: more arguments provided than expected by format string 'function'

More arguments were provided to the function than its format string indicates
are necessary. The most likely cause for this problem is that the format string
is missing a substitution.

----

### CQL0423: fewer arguments provided than expected by format string 'function'

Fewer arguments were provided to the function than its format string indicates
are necessary. The most likely cause for this problem is that an argument was
accidentally omitted.


### CQL0424: procedure with INOUT parameter used as function 'procedure_name'

If a procedure has an `INOUT` parameter, it cannot be used as a function: It may
only be called via a `CALL` statement.


### CQL0425: procedure with non-trailing OUT parameter used as function 'procedure_name'

For a procedure to be used as a function, it must have exactly one `OUT`
parameter, and that parameter must be the last parameter of the procedure. In
all other cases, procedures with one or more `OUT` parameters may only be called
via a `CALL` statement.

----

### CQL0426: OUT or INOUT argument cannot be used again in same call 'variable'

When a variable is passed as an `OUT` or `INOUT` argument, it may not be used as
another argument within the same procedure call. It can, however, be used within
a _subexpression_ of another argument. For example:

```sql
CREATE PROC some_proc(IN a TEXT, OUT b TEXT)
BEGIN
  ...
END

DECLARE t TEXT;

-- This is NOT legal.
CALL some_proc(t, t);

-- This, however, is perfectly fine.
CALL some_proc(some_other_proc(t), t);
```

----

### CQL0427: LIKE CTE form may only be used inside a shared fragment at the top level i.e. @attribute(cql:shared_fragment) 'procedure_name'

When creating a shared fragment you can specify "table parameters" by defining their shape like so:

```
@attribute(cql:shared_fragment)
create proc shared_proc(lim_ integer)
begin
   with source(*) LIKE any_shape
   select * from source limit lim_;
end;
```

However this LIKE form only makes sense withing a shared fragment, and only as a top level CTE in such a fragment.  So either:

* the LIKE appeared outside of any procedure
* the LIKE appeared in a procedure, but that procedure is not a shared fragment
* the LIKE appeared in a nested WITH clause

----

### CQL0428: duplicate binding of table in CALL/USING clause 'table_name'

In a CALL clause to access a shared fragment there is a duplicate table name in the USING portion.

Example:

```
my_cte(*) AS (call my_fragment(1) USING something as param1, something_else as param1),
```

Here `param1` is supposed to take on the value of both `something` and `something_else`.  Each parameter
may appear only once in the `USING` clause.

### CQL0429: called procedure has no table arguments but a USING clause is present 'procedure_name'

In a CALL clause to access a shared fragment there are table bindings but the shared fragment that
is being called does not have any table bindings.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 select * from a_location limit lim;
end;

-- here we try to use my_fragment with table parameter but it has none
with
  my_cte(*) AS (call my_fragment(1) USING something as param)
  select * from my_cte;
```

----

### CQL0430: no actual table was provided for the table parameter 'table_name'

In a CALL clause to access a shared fragment the table bindings are missing a table parameter.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 with source LIKE source_shape
 select * from source limit lim;
end;

-- here we try to use my_fragment but no table was specified to play the role of "source"
with
  my_cte(*) AS (call my_fragment(1))
  select * from my_cte;
```

----

### CQL0431: an actual table was provided for a table parameter that does not exist 'table_name'

In a CALL clause to access a shared fragment the table bindings refer to a table parameter
that does not exist.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 with source LIKE source_shape
 select * from source limit lim;
end;

-- here we try to use my_fragment but there is a table name "soruce" that doesn't match source
with
  my_cte(*) AS (call my_fragment(1) USING something as soruce)
  select * from my_cte;
```

----

### CQL0432: table provided must have the same number of columns as the table parameter 'table_name'

In a CALL clause to access a shared fragment the table bindings are trying to use a table
that has the wrong number of columns.  The column count, names, and types must be compatible.
Extra columns for instance are not allowed because they might create ambiguities that were not
present in the shared fragment.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 with source LIKE (select 1 x, 2 y)
 select * from source limit lim;
end;

-- here we try to use my_fragment but we provided 3 columns not 2
with
  my_source(*) AS (select 1 x, 2 y, 3 z),
  my_cte(*) AS (call my_fragment(1) USING my_source as source)
  select * from my_cte;
```

Here `my_fragment` wants a `source` table with 2 columns (x, y).  But 3 were provided.

----

### CQL0433: table argument 'formal_name' requires column 'column_name' but it is missing in provided table 'actual_name'

In a CALL clause to access a shared fragment the table bindings are trying to use a table
that is missing a required column.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 with source LIKE (select 1 x, 2 y)
 select * from source limit lim;
end;

-- here we try to use my_fragment but we passed in a table with (w,x) not (x,y)
with
  my_source(*) AS (select 1 w, 2 x),
  my_cte(*) AS (call my_fragment(1) USING my_source as source)
  select * from my_cte;
```

----

### CQL0434: shared fragments may not be called outside of a SQL statement 'procedure_name'

The indicated name is the name of a shared fragment, these fragments may be used inside
of SQL code (e.g. select statements) but they have no meaning in a normal call outside
of a SQL statement.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
 select * from somewhere limit lim;
end;

call my_fragment();
```

Here `my_fragment` is being used like a normal procedure. This is not valid.  A correct
use of a fragment might look something like this:

```
with
  (call my_fragment())
  select * from my_fragment;
```

----

### CQL0435: must use qualified form to avoid ambiguity with alias 'column'

In a SQLite `SELECT` expression, `WHERE`, `GROUP BY`, `HAVING`, and `WINDOW`
clauses see the columns of the `FROM` clause before they see any aliases in the
expression list. For example, assuming some table `t` has columns `x` and `y`,
the following two expressions are equivalent:

```
SELECT x AS y FROM t WHERE y > 100
SELECT x AS y FROM t WHERE t.y > 100
```

In the first expression, the use of `y > 100` makes it seem as though the `y`
referred to could be the `y` resulting from `x as y` in the expression list, but
that is not the case. To avoid such confusion, CQL requires the use of the
qualified form `t.y > 100` instead.

----

### CQL0436: alias referenced from WHERE, GROUP BY, HAVING, or WINDOW clause

Unlike many databases (e.g., PostgreSQL and SQL Server), SQLite allows the
aliases of a `SELECT` expression list to be referenced from clauses that are
evaluated _before_ the expression list. It does this by replacing all such alias
references with the expressions to which they are equivalent. For example,
assuming `t` does _not_ have a column `x`, the following two expressions are
equivalent:

```
SELECT a + b AS x FROM t WHERE x > 100
SELECT a + b AS x FROM t WHERE a + b > 100
```

This can be convenient, but it is also error-prone. As mentioned above, the
above equivalency only holds if `x` is _not_ a column in `t`: If `x` _is_ a
column in `t`, the `WHERE` clause would be equivalent to `t.x > 100` instead,
and there would be no syntactically obvious way to know this without first
manually determining all of the columns present in `t`.

To avoid such confusion, CQL disallows referencing expression list aliases from
`WHERE`, `GROUP BY`, `HAVING`, and `WINDOW` clauses altogether. Instead, one
should simply use the expression to which the alias is equivalent (as is done in
the second example above).

----

### CQL0437: common table name shadows previously declared table or view 'name'

The name of a common table expression may not shadow a previously declared table
or view. To rectify the problem, simply use a different name.

----

### CQL0438: variable possibly used before initialization 'name'

The variable indicated must be initialized before it is used because it is of
a reference type (`BLOB`, `OBJECT`, or `TEXT`) that is also `NOT NULL`.

CQL is usually smart enough to issue this error only in cases where
initialization is truly lacking. Be sure to verify that the variable will be
initialized before it is used for all possible code paths.

----

### CQL0439: nonnull reference OUT parameter possibly not always initialized 'name'

The parameter indicated must be initialized before the procedure returns because
it is of a reference type (`BLOB`, `OBJECT`, or `TEXT`) that is also `NOT NULL`.

CQL is usually smart enough to issue this error only in cases where
initialization is truly lacking. Be sure to verify that the parameter will be
initialized both before the end of the procedure and before all cases of
`RETURN` and `ROLLBACK RETURN`. (Initialization before `THROW` is not required.)

----

### CQL0440: fragments may not have an empty body 'procedure_name'

The indicated procedure is one of the fragment types but has an empty body.
This is not valid for any fragment type.

Example:

```
@attribute(cql:shared_fragment)
create proc my_fragment(lim integer not null)
begin
  /* something has to go here */
end;
```

----

### CQL0441: shared fragments may only have IF, SELECT, or  WITH...SELECT at the top level 'procedure_name'

A shared fragment may consist of just one SELECT statement (including WITH...SELECT)
or it can be an IF/ELSE statement that has a series of compatible select statements.
There are no other valid options.


### CQL0442: shared fragments with conditionals must include an else clause 'procedure_name'

In shared fragment with conditionals (i.e. it has an IF statement at the top) the fragment
must have an ELSE block so that it is guaranteed to create chunk of SQL text in its expansion.
When no rows are required you can do so with something like:

```
IF ... THEN
  ...
ELSE
   select 1 x, '2' y WHERE 0;
END IF;
```

If the `ELSE` condition indicates that some join should not happen you might generate default values
or NULLs for the join result like so:

```
IF ... THEN
  ...
ELSE
  select input_stuff.*, NULL x, -1 y;
END IF;
```


### CQL0443: shared fragments with conditionals must have exactly one SELECT or WITH...SELECT in each statement list 'procedure_name'

In a shared fragment with conditionals the top level statement is an "IF".  All of the statement lists in
the IF must have exactly one valid select statement.  This error indicates that a statement list has the wrong number
or type of statement.


### CQL0444: this use of the named shared fragment is not legal because of name conflict 'procedure_name'

This error will be followed by additional diagnostic information about the call chain that is problematic.  For instance:

```
Procedure innermost has a different CTE that is also named foo
The above originated from CALL inner USING foo AS source
The above originated from CALL middle USING foo AS source
The above originated from CALL outer USING foo AS source
```

This indicates that you are trying to call `outer` which in turn calls `middle` which in turn called `inner`.  The conflict
happened when the `foo` parameter was passed in to `inner` because it already has a CTE named `foo` that means something else.

The way to fix this problem is to rename the CTE in probably the outermost call as that is likely the one you control.
Renaming it in the innermost procedure might also be wise if that procedure is using a common name likely to conflict.

It is wise to name the CTEs in shared fragments such that they are unlikely to eclipse outer CTEs that will be needed as table parameters.


### CQL0445: @attribute(cql:try_is_proc_body) accepts no values

The attribute `cql:try_is_proc_body` cannot be used with any values (e.g.,
`cql:try_is_proc_body=(...)`).


### CQL0446: @attribute(cql:try_is_proc_body) cannot be used more than once per procedure

The purpose of `cql:try_is_proc_body` is to indicate that a particular `TRY`
block contains what should be considered to be the true body of the procedure.
As it makes no sense for a procedure to have multiple bodies,
`cql:try_is_proc_body` must appear only once within any given procedure.


### CQL0447: virtual table 'table' claims to be eponymous but its module name 'module' differs from its table name

By definition, an eponymous virtual table has the same name as its module.  If you use the @eponymous notation
on a virtual table, you must also make the module and table name match.


### CQL0448: table was marked @delete but it needs to be marked @recreate @delete 'table'

The indicated table was on the recreate plan and was then deleted by adding an `@delete(version)` attribute.

However, the previous `@recreate` annotation was removed.  This would make the table look like
it was a baseline table that had been deleted, and it isn't.  To correctly drop a table on
the `@recreate` you leave the recreate directive as it was and simply add `@delete`.  No
version information is required because the table is on the recreate plan anyway.

Example:

```
create table dropping_this
(
  f1 integer,
  f2 text
) @recreate(optional_group) @delete;
```

This error indicates that the `@recreate(optional_group)` annotation was removed.  You should
put it back.


### CQL0449: unsubscribe does not make sense on non-physical tables 'table_name'

The indicated table was marked for blob storage or is a backed table.  In both cases there
is no physical schema associated with it so unsubscribe does not make any sense there.
If it's a backed table perhaps the intent was to remove the backing table?

### CQL0450: a shared fragment used like a function must be a simple SELECT with no FROM clause

When using a shared fragment like an expression, the shared fragment must consist of a
simple SELECT without a FROM clause. That SELECT, however, may contain a nested SELECT
expression which, itself, may have a FROM clause.

Additional constraints:

* the target of the call is a shared fragment
  * the target therefore a single select statement
  * the target therefore has no out-arguments
* the target has no select clauses other than the select list, e.g. no FROM, WHERE, LIMIT etc.
* the target returns exactly one column, i.e. it's just one SQL expression


### CQL0451: procedure as function call is not compatible with DISTINCT or filter clauses

Certain built-in functions like `COUNT` can be used with `DISTINCT` or `FILTER` options like so:

```
select count(distinct ...);

select sum(...) filter(where ...) over (...)
```

These options are not valid when calling a procedure as a function and so they generate errors if used.


### CQL0452: function may not be used in SQL because it is not supported on old versions of SQLite 'function'

Due to an enabled enforcement (e.g., `@enforce_strict sign function;`), the
indicated function may not be used within SQL because it is not supported on old
versions of SQLite.


### CQL0453: blob type is not a valid table 'table_name'

The CQL forms `SET [blob] FROM CURSOR [cursor]` and `FETCH [cursor] FROM [blob]` require
that the blob variable be declared with a type kind and the type of the blob
matches a suitable table.

In this case the blob was declared like so:

```
DECLARE blob_var blob<table_name>
```

But the named table `table_name` is not a table.


### CQL0454: cursor was not declared for storage 'cursor_name'

The CQL forms `SET [blob] FROM CURSOR [cursor]` and `FETCH [cursor] FROM [blob]` require
that the cursor variable have storage associated with it.  This means it must
be a value cursor or else a cursor that was fetched using the `fetch C` form
and not the `fetch C into [variables]` form.

The indicated cursor was either not fetched at all, or else is using only
the `fetch into` form so it does not have storage that could be used to
create a blob.


### CQL0455: blob variable must have a type kind for type safety, 'blob_name'

The CQL forms `SET [blob] FROM CURSOR [cursor]` and `FETCH [cursor] FROM [blob]` require
that the blob variable be declared with a type kind and the type of the blob
matches a suitable table.

In this case the blob was declared like so:

```
DECLARE blob_name blob;
```

But it must be:

```
DECLARE blob_name blob<table_name>;
```

Where `table_name` is a suitable table.


### CQL0456: blob type is a view, not a table 'view_name'

The CQL forms `SET [blob] FROM CURSOR [cursor]` and `FETCH [cursor] FROM [blob]` require
that the blob variable be declared with a type kind and the type of the blob
matches a suitable table.

In this case the blob was declared like:

```
DECLARE blob_var blob<view_name>
```

Where the named type `view_name` is a view, not a table.


### CQL0457: the indicated table is not marked with @attribute(cql:blob_storage) 'table_name'

The CQL forms `SET [blob] FROM CURSOR [cursor]` and `FETCH [cursor] FROM [blob]` require
that the blob variable be declared with a type kind and the type of the blob
matches a suitable table.

In this case the blob was declared like:

```
DECLARE blob_var blob<table_name>
```

but the indicated table is missing the necessary attribute `@attribute(cql:blob_storage)`.

This attribute is necessary so that CQL can enforce additional rules on the table
to ensure that it is viable for blob storage.  For instance, the table can have no
primary key, no foreign keys, and may not be used in normal SQL statements.


### CQL0458: the indicated table may only be used for blob storage 'table_name'

The indicated table has been marked with `@attribute(cql:blob_storage)`.  This means
that it isn't a real table -- it will have no SQL schema.  Since it's only a storage
shape, it cannot be used in normal operations that use tables such as `DROP TABLE`,
`CREATE INDEX`, or inside of `SELECT` statements.

The `CREATE TABLE` construct is used to declare a blob storage type because it's the
natural way to define a structure in SQL and also because the usual versioning rules are
helpful for such tables.  But otherwise, blob storage isn't really a table at all.


### CQL0459: table is not suitable for use as blob storage: [reason] 'table_name'

The indicated table was marked with `@attribute(cql:blob_storage)`.  This indicates
that the table is going to be used to define the shape of blobs that could be
stored in the database. It isn't going to be a "real" table.

There are a number of reasons why a table might not be a valid as blob storage.

For instance:

* it has a primary key
* it has foreign keys
* it has constraints
* it is a virtual table

This error indicates that one of these items is present. The specific cause
is included in the text of the message.


### CQL0460: field of a nonnull reference type accessed before verifying that the cursor has a row 'cursor.field'

If a cursor has a field of a nonnull reference type (e.g., `TEXT NOT NULL`), it
is necessary to verify that the cursor has a row before accessing the field
(unless the cursor has been fetched in such a way that it *must* have a row,
e.g., via `FETCH ... FROM VALUES` or `LOOP FETCH`). The reason for this is that,
should the cursor _not_ have a row, the field will be `NULL` despite the nonnull
type.

Assume we have the following:

```sql
create table t (x text not null);
declare proc requires_text_notnull(x text not null);
```

The following code is **illegal**:

```sql
declare c cursor for select * from t;
fetch c;
-- ILLEGAL because `c` may not have a row and thus
-- `c.x` may be `NULL`
call requires_text_notnull(c.x);
```

To fix it, the cursor must be verified to have a row before the field is
accessed:

```sql
declare c cursor for select * from t;
fetch c;
if c then
  -- legal due to the above check
  call requires_text_notnull(c.x);
end if;
```

Alternatively, one can perform a "negative" check by returning (or using another
control flow statement) when the cursor does not have a row:

```sql
declare c cursor for select * from t;
fetch c;
if not c then
  call some_logging_function("no rows in t");
  return;
end if;
-- legal as we would have returned if `c` did not
-- have a row
call requires_text_notnull(c.x);
```

If you are sure that a row *must* be present, you can throw to make that
explicit:

```sql
declare c cursor for select * from t;
fetch c;
if not c throw;
-- legal as we would have thrown if `c` did not
-- have a row
call requires_text_notnull(c.x);
```


### CQL0461: fetch from blob operand is not a blob

The blob operand in the form `FETCH [cursor] FROM BLOB [blob]`
must be a blob.  The given expression is of some other type.


### CQL0462: group declared variables must be top level 'name'

A `DECLARE GROUP` statement for the named enum is happening inside of a procedure.  This is not legal.

To correct this, move the declaration outside of the procedure.


#### CQL0463: variable definitions do not match in group 'name'

The two described `DECLARE GROUP` statements have the same name but they are not identical.

The error output contains the full text of both declarations to compare.


### CQL0464: group not found 'group_name'

The indicated name was used in a context where a variable group name was expected but there is no such group.

Perhaps the group was not included (missing an #include) or else there is a typo.


### CQL0465 avaiable for re-use


### CQL0466: the table/view named in an @unsub directive does not exist 'name'

The indicated name is not a valid table or view.


### CQL0467 available for re-use


### CQL0468: @attribute(cql:shared_fragment) may only be placed on a CREATE PROC statement 'proc_name'

In order to use a shared fragment the compiler must see the full body of the fragment, this is
because the fragment will be inlined into the SQL in which it appears.  As a consequence it
makes no sense to try to apply the attribute to a procedure declaration.  Instead put the
shared fragment you want to use somewhere where it can be #included in full.

example error:

```sql
@attribute(cql:shared_fragment)
declare proc x() (x integer);

create proc y()
begin
  with (call x())
  select * from x;
end;
```

Instead, include the entire body like so (this example is ultra simple).

```sql
@attribute(cql:shared_fragment)
create proc x()
begin
  select 1 x; -- the procedure body must be present
end;
```


### CQL0469: table/view is already deleted 'name'

In an @unsub directive, the indicated table/view has already been deleted. It can no longer
be managed via subscriptions.


### CQL0470 available for re-use


### CQL0471 available for re-use


### CQL0472: table/view is already unsubscribed 'name'

In an @unsub directive, the indicated table/view has already been unsubscribed. It doesn't need
another unsubscription.


### CQL0473: @unsub is invalid because the table/view is still used by 'name'

This error indicates that you are attempting to @unsub a table/view while there are still other
tables/views that refer to it (e.g. by FK).  You must @unsub all of those as well
in order to safely @unsub the present table/view.  All such dependencies  will be listed.  Note that
some of those might, in turn, have the same issue.  In short, a whole subtree has to
be removed in order to do this operation safely.


### CQL0474 available for re-use


### CQL0475 available for re-use


### CQL0476 available for re-use


### CQL0477: interface name conflicts with func name 'name'

In a `DECLARE INTERFACE` statement, the given name conflicts with an already declared function (`DECLARE FUNCTION` or `DECLARE SELECT FUNCTION`).  You'll have to choose a different name.

### CQL0478: interface name conflicts with procedure name 'name'

In a `DECLARE INTERFACE` statement, the indicated name already corresponds to a created or declared stored procedure.  You'll have to choose a different name.

### CQL0479: interface declarations do not match 'name'

The interface was previously declared with a `DECLARE INTERFACE` statement but when subsequent `DECLARE INTERFACE` was encountered, it did not match the previous declaration.

### CQL0480: declared interface must be top level 'name'

A `DECLARE INTERFACE` statement is happening inside of a procedure.  This is not legal.  To correct this move the declaration outside of the procedure.

### CQL0481: proc name conflicts with interface name 'name'

In a `CREATE PROCEDURE` / `DECLARE PROCEDURE` statement, the given name conflicts with an already declared interface (`DECLARE INTERFACE`).  You'll have to choose a different name.

### CQL0482: interface not found 'name'

Interface with the name provided in `cql:implements` attribute does not exist

### CQL0483: table is not suitable for use as backing storage: [reason] 'table_name'

The indicated table was marked with `@attribute(cql:backing_table)`.  This indicates
that the table is going to be used to as a generic storage location stored in the database.

There are a number of reasons why a table might not be a valid as backing storage.

For instance:

* it has foreign keys
* it has constraints
* it is a virtual table
* it has schema versioning

This error indicates that one of these items is present. The specific cause is included in the text of the message.

### CQL0484: procedure '%s' is missing column '%s' of interface '%s'

Procedure should return all columns defined by the interface (and possibly others).  The columns may be returned in any order.

### CQL0485: column types returned by proc need to be the same as defined on the interface

Procedure should return at least all columns defined by the interface and column type should be the same.

### CQL0486: function cannot be both a normal function and an unchecked function, 'function_name'

The same function cannot be declared as a function with unchecked parameters with the `NO CHECK` clause and then redeclared with typed parameters, or vice versa.

```sql
--- Declaration of an external function foo with unchecked parameters.
DECLARE SELECT FUNCTION foo NO CHECK t text;

...

--- A redeclaration of foo with typed paramters. This would be invalid if the previous declaration exists.
DECLARE SELECT FUNCTION foo() t text;
```

Make sure the redeclaration of the function is consistent with the original declaration, or remove the redeclaration.

### CQL0487: table is not suitable as backed storage: [reason] 'table_name'

The indicated table was marked with `@attribute(cql:backing_table)`.  This indicates
that the table is going to be used to as a generic storage location stored in the database.

There are a number of reasons why a table might not be a valid as backing storage.

For instance:

* it has foreign keys
* it has constraints
* it is a virtual table
* it has schema versioning

This error indicates that one of these items is present. The specific cause is included in the text of the message.

### CQL0488: the indicated table is not declared for backed storage 'table_name'

When declaring a backed table, you must specify the physical table that will hold its data.  The backed table
is marked with `@attribute(cql:backed_by=table_name)`.  The backing table is marked with `@attribute(cql:backing)`.
The backing and backed_by attributes applies extra checks to tables to ensure they are suitable candidates.

This error indicates that the named table is not marked as a backed table.

### CQL0489: the indicated column is not present in the named backed storage 'table_name.column_name'

The named table is a backed table, but it does not have the indicated column.

### CQL0490: argument must be table.column where table is a backed table 'function"

The database blob access functions `cql_blob_get`, `cql_blob_create`, `cql_blob_update` all allow you to specify the backed
table name and column you are trying to read/create/update.  The named function was called with a table.column combination
where the table is not a backed table, hence the call is invalid.  Note that normally this error doesn't happen
because these functions are typically called by CQL itself as part of the rewriting process for backed tables.  However it is
possible to use them manually, hence they are error checked.


### CQL0491: argument 1 must be a table name that is a backed table 'cql_blob_create'

When using the `cql_blob_create` helper function, the first argument must be a valid backed table (i.e. one that
was marked with `@attribute(cql:backed_by=some_backing_table))`.  The type signature of this table is used
to create a hash valid for the type of the blob that is created.  This error indicates that the first argument
is not even an identifier, much less a table name that is backed.  There are more specific errors if the table
is not found or the table is not backed.  Note that normally this error doesn't happen because this functions is
typically called by CQL itself as part of the rewriting process for backed tables.  However it is possible
to `cql_blob_create` manually, hence it is error checked.

### CQL0492 available for use


### CQL0493: backed storage tables may not be used in indexes/triggers/drop 'table_name'

The indicated table name was marked as backed storage.  Therefore it does not have a physical manifestation,
and therefore it cannot be used in an index or in a trigger.  You may be able to get the index or trigger
you want by creating an index on the backing storage and then using the blob access functions to index
a column or check colulmns in a trigger.  For instance this index is pretty normal:

```
@attribute(cql:backing_table)
create table backing (
  k blob primary key,
  v blob not null
);

create index backing_type_index on backing(cql_blob_get_type(k));
```

This gives you a useful index on the type field of the blob for all backed tables that use `backing_table`.

But generally, physical operations like indices, triggers, and drop are not applicable to backed tables.

### CQL0494: mixing adding and removing columns from a shape 'name'

When selecting columns from a shape you can use this form

```
LIKE some_shape(name1, name2)
```

to extract the named columns or this form

```
LIKE some_shape(-name1, -name2)
```

to extract everything but the named columns.  You can't mix the positive and negative forms

### CQL0495: no columns were selected in the LIKE expression

An expression that is supposed to select some columns from a shape such as

```
LIKE some_shape(-name1, -name2)
```

ended up removing all the columns from `some_shape`.

### CQL0496: SELECT NOTHING may only appear in the else clause of a shared fragment

A common case for conditional shared fragments is that there are rows that should be optionally
included.  The normal way this is handled is to have a condition like this

```sql
IF something THEN
  SELECT your_data;
ELSE
  SELECT dummy data WHERE 0;
END IF;
```

The problem here is that dummy_data could be complex and involve a lot of typing to get nothing.
To make this less tedious CQL allows:

```sql
IF something THEN
  SELECT your_data;
ELSE
  SELECT NOTHING;
END IF;
```

However this is the only place SELECT NOTHING is allowed.  It must be:
* in a procedure
* which is a conditional shared fragment
* in the else clause

Any violation results in the present error.

### CQL0497: FROM clause not supported when updating backed table, 'table_name'

SQLite supports an extended format of the update statement with a FROM clause.  At this time backed tables cannot be updated using this form.  This is likely to change fairly soon.

### CQL0498: strict UPDATE ... FROM validation requires that the UPDATE statement not include a FROM clause

`@enforce_strict` has been use to enable strict update enforcement.  When enabled update statements may not include a FROM clause.
This is done if the code expects to target SQLite version 3.33 or lower.

### CQL0499: alias_of attribute may only be added to a declare function statement

`cql:alias_of` attributes may only be used in [`DECLARE FUNC` statements](https://cgsql.dev/cql-guide/ch08/#ordinary-scalar-functions) or [`DECLARE PROC` statements](https://cgsql.dev/cql-guide/ch06/#declaring-procedures-defined-elsewhere).

### CQL0500: alias_of attribute must be a non-empty string argument

`cql:alias_of` must have a string argument to indicate the underlying function name that the aliased function references. For example:

```sql
@attribute(cql:alias_of=foo)
declare function bar() int
```

All subsequent calls to `bar()` in CQL will call the `foo()` function.



## Appendix 5: JSON Schema Grammar
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from the JSON validation grammar with the tree building rules removed.

Snapshot as of Fri Mar  3 21:02:20 PST 2023

### Rules

```


json_schema: '{'
         '"tables"' ':' '[' opt_tables ']' ','
         '"virtualTables"' ':' '[' opt_virtual_tables ']' ','
         '"views"' ':' '[' opt_views ']' ','
         '"indices"' ':' '[' opt_indices ']' ','
         '"triggers"' ':' '[' opt_triggers ']' ','
         '"attributes"' ':' '[' opt_attribute_list ']' ','
         '"queries"' ':' '[' opt_queries ']' ','
         '"inserts"' ':' '[' opt_inserts ']' ','
         '"generalInserts"' ':' '[' opt_inserts_general ']' ','
         '"updates"' ':' '[' opt_updates ']' ','
         '"deletes"' ':' '[' opt_deletes ']' ','
         '"general"' ':' '[' opt_generals ']' ','
         '"declareProcs"' ':' '[' opt_declare_procs']' ','
         '"declareFuncs"' ':' '[' opt_declare_funcs']' ','
         '"interfaces"' ':' '[' opt_interfaces ']' ','
         '"regions"' ':' '[' opt_regions ']' ','
         '"adHocMigrationProcs"' ':' '[' opt_ad_hoc_migrations ']' ','
         '"enums"' ':'  '[' opt_enums ']' ','
         '"constantGroups"' ':'  '[' opt_const_groups ']' ','
         '"subscriptions"' ':'  '[' opt_subscriptions ']'
         '}'
  ;

BOOL_LITERAL: '0' | '1'
  ;

opt_tables: | tables
  ;

tables: table | table ',' tables
  ;

opt_backing_details: | '"isBacking"' ':' '1' ',' | '"isBacked"' ':' '1' ',' '"typeHash"' ':' num_literal ','
  ;

opt_type_hash: | '"typeHash"' ':' num_literal ','
  ;

table: '{'
       '"name"' ':' STRING_LITERAL ','
       '"schema"' ':' STRING_LITERAL ','
       '"crc"' ':' STRING_LITERAL ','
       '"isTemp"' ':' BOOL_LITERAL ','
       '"ifNotExists"' ':' BOOL_LITERAL ','
       '"withoutRowid"' ':' BOOL_LITERAL ','
       '"isAdded"' ':' BOOL_LITERAL ','
       opt_added_version
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       '"isRecreated"' ':' BOOL_LITERAL ','
       opt_recreate_group_name
       opt_unsub_version
       opt_backing_details
       opt_region_info
       opt_table_indices
       opt_attributes
       '"columns"' ':' '[' columns ']' ','
       '"primaryKey"' ':' '[' opt_column_names ']' ','
       '"primaryKeySortOrders"' ':' '[' opt_sort_order_names ']' ','
       opt_primary_key_name
       '"foreignKeys"' ':' '[' opt_foreign_keys ']' ','
       '"uniqueKeys"' ':' '[' opt_unique_keys ']' ','
       '"checkExpressions"' ':' '[' opt_check_expressions ']'
       '}'
  ;

opt_primary_key_name:  | '"primaryKeyName"' ':' STRING_LITERAL ','
  ;

opt_virtual_tables: | virtual_tables
  ;

virtual_tables: virtual_table | virtual_table ',' virtual_tables
  ;

virtual_table: '{'
       '"name"' ':' STRING_LITERAL ','
       '"schema"' ':' STRING_LITERAL ','
       '"crc"' ':' STRING_LITERAL ','
       '"isTemp"' ':' '0' ','
       '"ifNotExists"' ':' BOOL_LITERAL ','
       '"withoutRowid"' ':' BOOL_LITERAL ','
       '"isAdded"' ':' BOOL_LITERAL ','
       opt_added_version
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       '"isRecreated"' ':' BOOL_LITERAL ','
       opt_region_info
       '"isVirtual"' ':' '1' ','
       '"isEponymous"' ':' BOOL_LITERAL ','
       '"module"' ':' STRING_LITERAL ','
       opt_module_args
       opt_attributes
       '"columns"' ':' '[' columns ']' ','
       '"primaryKey"' ':' '[' opt_column_names ']' ','
       '"primaryKeySortOrders"' ':' '[' opt_sort_order_names ']' ','
       '"foreignKeys"' ':' '[' opt_foreign_keys ']' ','
       '"uniqueKeys"' ':' '[' opt_unique_keys ']' ','
       '"checkExpressions"' ':' '[' opt_check_expressions ']'
       '}'
  ;

opt_module_args: | '"moduleArgs"' ':'  STRING_LITERAL ','
  ;

opt_added_version: | '"addedVersion"' ':' any_integer ',' opt_added_migration_proc
  ;

opt_added_migration_proc: | '"addedMigrationProc"' ':' STRING_LITERAL ','
  ;

opt_unsub_version: | '"unsubscribedVersion"' ':' any_integer ','
  ;

opt_deleted_version: | '"deletedVersion"' ':' any_integer ',' opt_deleted_migration_proc
  ;

opt_deleted_migration_proc: | '"deletedMigrationProc"' ':' STRING_LITERAL ','
  ;

opt_recreate_group_name: | '"recreateGroupName"' ':' STRING_LITERAL ','
  ;

opt_index_names: | index_names
  ;

index_names: STRING_LITERAL | STRING_LITERAL ',' index_names
  ;

opt_arg_names: | arg_names
  ;

arg_names: STRING_LITERAL | STRING_LITERAL ',' arg_names
  ;

opt_column_names: | column_names
  ;

column_names: STRING_LITERAL | STRING_LITERAL ',' column_names
  ;

opt_table_names: | table_names
  ;

table_names: STRING_LITERAL | STRING_LITERAL ',' table_names
  ;

opt_view_names: | view_names
  ;

view_names: STRING_LITERAL | STRING_LITERAL ',' view_names
  ;

opt_procedure_names: | procedure_names
  ;

procedure_names: STRING_LITERAL | STRING_LITERAL ',' procedure_names
  ;

opt_sort_order_names: | sort_order_names
  ;

sort_order_names: STRING_LITERAL | STRING_LITERAL ',' sort_order_names
  ;

columns: column | column ',' columns
  ;

column: '{'
        '"name"' ':' STRING_LITERAL ','
        opt_attributes
        '"type"' ':' STRING_LITERAL ','
        opt_kind
        opt_is_sensitive
        '"isNotNull"' ':' BOOL_LITERAL ','
        '"isAdded"' ':' BOOL_LITERAL ','
        opt_added_version
        '"isDeleted"' ':' BOOL_LITERAL ','
        opt_deleted_version
        opt_default_value
        opt_collate
        opt_check_expr
        opt_type_hash
        '"isPrimaryKey"' ':' BOOL_LITERAL ','
        '"isUniqueKey"' ':' BOOL_LITERAL ','
        '"isAutoIncrement"' ':' BOOL_LITERAL
        '}'
  ;

opt_collate : | '"collate"' ':' STRING_LITERAL ','
  ;

opt_check_expr: | '"checkExpr"' ':' STRING_LITERAL ',' '"checkExprArgs"' ':' '[' opt_arg_names ']' ','
  ;

opt_default_value: | '"defaultValue"' ':' any_literal ','
  ;

opt_foreign_keys : | foreign_keys
  ;

opt_kind: | '"kind"' ':' STRING_LITERAL ','
  ;

opt_is_sensitive: | '"isSensitive"' ':' '1' ','
  ;

foreign_keys :  foreign_key | foreign_key ',' foreign_keys
  ;

foreign_key : '{'
               opt_name
               '"columns"' ':' '[' column_names ']' ','
               '"referenceTable"' ':' STRING_LITERAL ','
               '"referenceColumns"' ':' '[' column_names ']' ','
               '"onUpdate"' ':' STRING_LITERAL ','
               '"onDelete"' ':' STRING_LITERAL ','
               '"isDeferred"' ':' BOOL_LITERAL
              '}'
  ;

opt_unique_keys :  | unique_keys
  ;

unique_keys : unique_key | unique_key ',' unique_keys
  ;

unique_key:  '{'
              opt_name
              '"columns"' ':' '[' column_names ']' ','
              '"sortOrders"' ':' '[' sort_order_names ']'
             '}'
  ;

opt_check_expressions: | check_expressions
  ;

check_expressions: check_expression | check_expression ',' check_expressions
  ;

check_expression: '{'
                   opt_name
                   '"checkExpr"' ':' STRING_LITERAL ','
                   '"checkExprArgs"' ':' '[' ']'
                  '}'
  ;

opt_name: | '"name"' ':' STRING_LITERAL ','
  ;

opt_table_indices: | table_indices
  ;

table_indices: '"indices"' ':' '[' opt_index_names ']' ','
  ;

opt_attributes:  | attributes
  ;

attributes: '"attributes"' ':' '[' attribute_list ']' ','
  ;

opt_attribute_list: | attribute_list
  ;

attribute_list: attribute | attribute ',' attribute_list
  ;

attribute:  '{'
             '"name"' ':' STRING_LITERAL ','
             '"value"' ':' attribute_value
            '}'
  ;

attribute_array: '[' opt_attribute_value_list ']'
  ;

opt_attribute_value_list: | attribute_value_list
  ;

attribute_value_list: attribute_value | attribute_value ',' attribute_value_list
  ;

attribute_value: any_literal | attribute_array
  ;

any_integer: BOOL_LITERAL | INT_LITERAL
  ;

any_literal:  BOOL_LITERAL |
              INT_LITERAL | '-' INT_LITERAL |
              LONG_LITERAL | '-' LONG_LITERAL |
              REAL_LITERAL | '-' REAL_LITERAL |
              STRING_LITERAL | NULL_LITERAL
  ;

num_literal:  BOOL_LITERAL |
              INT_LITERAL | '-' INT_LITERAL |
              LONG_LITERAL | '-' LONG_LITERAL |
              REAL_LITERAL | '-' REAL_LITERAL
  ;

opt_views: | views
  ;

views: view | view ',' views
  ;

view:  '{'
       '"name"' ':' STRING_LITERAL ','
       '"crc"' ':' STRING_LITERAL ','
       '"isTemp"' ':' BOOL_LITERAL ','
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       opt_region_info
       opt_attributes
       projection
       '"select"' ':' STRING_LITERAL ','
       '"selectArgs"' ':' '[' ']' ','
       dependencies
       '}'
  ;

opt_region_info: | '"region"' ':' STRING_LITERAL ',' |  '"region"' ':' STRING_LITERAL ',' '"deployedInRegion"' ':' STRING_LITERAL ','
  ;

opt_projection: | projection
  ;

projection: '"projection"' ':' '[' projected_columns ']' ','
  ;

projected_columns: projected_column | projected_column ',' projected_columns
  ;

projected_column: '{'
                   '"name"' ':' STRING_LITERAL ','
                   '"type"' ':' STRING_LITERAL ','
                   opt_kind
                   opt_is_sensitive
                   '"isNotNull"' ':' BOOL_LITERAL
                  '}'
  ;

opt_indices:  | indices
  ;

indices: index  | index ',' indices
  ;

index: '{'
        '"name"' ':' STRING_LITERAL ','
        '"crc"' ':' STRING_LITERAL ','
        '"table"' ':' STRING_LITERAL ','
        '"isUnique"' ':' BOOL_LITERAL ','
        '"ifNotExists"' ':' BOOL_LITERAL ','
        '"isDeleted"' ':' BOOL_LITERAL ','
        opt_deleted_version
        opt_region_info
        opt_partial_index_where
        opt_attributes
        '"columns"' ':' '[' column_names ']' ','
        '"sortOrders"' ':' '[' sort_order_names ']'
       '}'
  ;

opt_partial_index_where: | '"where"' ':' STRING_LITERAL ','
  ;

opt_triggers: | triggers
  ;

triggers: trigger | trigger ',' triggers
  ;

trigger: '{'
          '"name"' ':' STRING_LITERAL ','
          '"crc"' ':' STRING_LITERAL ','
          '"target"' ':' STRING_LITERAL ','
          '"isTemp"' ':' BOOL_LITERAL ','
          '"ifNotExists"' ':' BOOL_LITERAL ','
          '"isDeleted"' ':' BOOL_LITERAL ','
          opt_deleted_version
          before_after_instead ','
          delete_insert_update ','
          opt_for_each_row
          opt_when_expr
          '"statement"' ':' STRING_LITERAL ','
          '"statementArgs"' ':' '[' opt_arg_names ']' ','
          opt_region_info
          opt_attributes
          dependencies
         '}'
  ;

before_after_instead: '"isBeforeTrigger"' ':' '1' | '"isAfterTrigger"' ':' '1'  | '"isInsteadOfTrigger"' ':' '1'
  ;

delete_insert_update: '"isDeleteTrigger"' ':' '1' | '"isInsertTrigger"' ':' '1' | '"isUpdateTrigger"' ':' '1'
  ;

opt_for_each_row: | '"forEachRow"' ':' BOOL_LITERAL ','
  ;

opt_when_expr: | '"whenExpr"' ':' STRING_LITERAL ',' '"whenExprArgs"' ':' '[' opt_arg_names ']' ','
  ;

dependencies: opt_insert_tables
            opt_update_tables
            opt_delete_tables
            opt_from_tables
            opt_uses_procedures
            opt_uses_views
            '"usesTables"' ':' '[' opt_table_names ']'
  ;

opt_uses_views: | '"usesViews"' ':' '[' opt_view_names ']' ','
  ;

opt_insert_tables: | '"insertTables"' ':' '[' opt_table_names ']' ','
  ;

opt_update_tables: | '"updateTables"' ':' '[' opt_table_names ']' ','
  ;

opt_delete_tables: | '"deleteTables"' ':' '[' opt_table_names ']' ','
  ;

opt_from_tables: | '"fromTables"' ':' '[' opt_table_names ']' ','
  ;

opt_uses_procedures : | '"usesProcedures"' ':' '[' opt_procedure_names ']' ','
  ;

opt_queries: | queries ;

queries: query | query ',' queries ;

query: '{'
       '"name"' ':' STRING_LITERAL ','
       '"definedInFile"' ':' STRING_LITERAL ','
       '"definedOnLine"' ':' INT_LITERAL ','
       '"args"' ':' '[' opt_args ']' ','
       dependencies ','
       opt_region_info
       opt_attributes
       projection
       '"statement"' ':' STRING_LITERAL ','
       '"statementArgs"' ':' '[' opt_arg_names ']'
       '}'
  ;

opt_args: | args
  ;

args: arg | arg ',' args
  ;

arg: '{'
      '"name"' ':' STRING_LITERAL ','
      '"argOrigin"' ':' STRING_LITERAL ','
      '"type"' ':' STRING_LITERAL ','
      opt_kind
      opt_is_sensitive
      '"isNotNull"' ':' BOOL_LITERAL
      '}'
  ;

opt_inserts: | inserts
  ;

inserts: insert | insert ',' inserts
  ;

insert : '{' insert_details ',' '"values"' ':' '[' opt_values ']' '}'
  ;

opt_inserts_general: | inserts_general
  ;

inserts_general: insert_general | insert_general ',' inserts_general
  ;

insert_details:
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"definedOnLine"' ':' INT_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']' ','
         '"statementType"' ':' STRING_LITERAL ','
         '"columns"' ':' '[' column_names ']'

insert_general : '{' insert_details '}'
  ;

opt_values: | values
  ;

values: value | value ',' values
  ;

value:  '{'
         '"value"' ':' STRING_LITERAL ','
         '"valueArgs"' ':' '[' opt_arg_names ']'
        '}'
  ;

opt_updates: | updates
  ;

updates: update | update ',' updates
  ;

update : '{'
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"definedOnLine"' ':' INT_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']'
         '}'
  ;

opt_deletes: | deletes
  ;

deletes: delete | delete ',' deletes
  ;

delete : '{'
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"definedOnLine"' ':' INT_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']'
         '}'
  ;

opt_generals: | generals
  ;

generals: general | general ',' generals
  ;

general: '{'
          '"name"' ':' STRING_LITERAL ','
          '"definedInFile"' ':' STRING_LITERAL ','
          '"definedOnLine"' ':' INT_LITERAL ','
          '"args"' ':' '[' opt_complex_args ']' ','
          dependencies ','
          opt_regions
          opt_attributes
          opt_projection
          opt_result_contract
          '"usesDatabase"' ':' BOOL_LITERAL
         '}'
  ;

opt_result_contract: | '"hasSelectResult"' ':' '1' ',' | '"hasOutResult"' ':' '1' ',' | '"hasOutUnionResult"' ':''1' ','
  ;

opt_complex_args: | complex_args
  ;

complex_args: complex_arg | complex_arg ',' complex_args
  ;

complex_arg: '{'
              binding
              '"name"' ':' STRING_LITERAL ','
              opt_arg_origin
              '"type"' ':' STRING_LITERAL ','
              opt_kind
              opt_is_sensitive
              '"isNotNull"' ':' BOOL_LITERAL
             '}'
  ;

binding: | '"binding"' ':' '"inout"' ',' | '"binding"' ':' '"out"' ','
  ;

opt_arg_origin: | arg_origin
  ;

arg_origin: '"argOrigin"' ':' STRING_LITERAL ','
  ;

opt_enums: | enums
  ;

enums: enum | enum ',' enums
  ;

enum: '{'
      '"name"' ':' STRING_LITERAL ','
      '"type"' ':' STRING_LITERAL ','
      '"isNotNull"' ':' '1' ','
      '"values"' ':' '[' enum_values ']'
      '}'
  ;

enum_values: enum_value | enum_value ',' enum_values
  ;

enum_value: '{'
             '"name"' ':' STRING_LITERAL ','
             '"value"' ':' num_literal
            '}'
  ;

opt_declare_procs: | declare_procs
  ;

declare_procs: declare_proc | declare_proc ',' declare_procs

declare_proc: '{'
          '"name"' ':' STRING_LITERAL ','
          '"args"' ':' '[' opt_complex_args ']' ','
          opt_attributes
          opt_projection
          '"usesDatabase"' ':' BOOL_LITERAL
         '}'
  ;

opt_declare_funcs:  | declare_funcs
  ;

declare_funcs: declare_func | declare_func ',' declare_funcs
  ;

declare_func: '{'
          '"name"' ':' STRING_LITERAL ','
          '"args"' ':' '[' opt_complex_args ']' ','
          opt_attributes
          opt_return_type
          '"createsObject"' ':' BOOL_LITERAL
         '}'
  ;

opt_return_type: | '"returnType"' ':' return_type ','
  ;

return_type: '{'
          '"type"' ':' STRING_LITERAL ','
          opt_kind
          opt_is_sensitive
          '"isNotNull"' ':' BOOL_LITERAL
         '}'
  ;

opt_interfaces: | interfaces
  ;

interfaces: interface | interface ',' interfaces
  ;

interface: '{'
          '"name"' ':' STRING_LITERAL ','
          '"definedInFile"' ':' STRING_LITERAL ','
          '"definedOnLine"' ':' INT_LITERAL ','
          opt_attributes
          '"projection"' ':' '[' projected_columns ']'
         '}'
  ;

opt_subscriptions: | subscriptions
  ;

subscriptions: subscription | subscription ',' subscriptions
  ;

subscription: '{'
     '"type"' ':' STRING_LITERAL ','
     '"table"' ':' STRING_LITERAL ','
     opt_region_info
     '"version"' ':' any_integer
     '}'
  ;

opt_const_groups: | const_groups
  ;

const_groups: const_group | const_group ',' const_groups
  ;

const_group: '{'
      '"name"' ':' STRING_LITERAL ','
      '"values"' ':' '[' const_values ']'
      '}'
  ;

const_values: const_value | const_value ',' const_values
  ;

const_value: '{'
             '"name"' ':' STRING_LITERAL ','
             '"type"' ':' STRING_LITERAL ','
             opt_kind
             '"isNotNull"' ':' BOOL_LITERAL ','
             '"value"' ':' num_literal
            '}'
  | '{'
             '"name"' ':' STRING_LITERAL ','
             '"type"' ':' STRING_LITERAL ','
             opt_kind
             '"isNotNull"' ':' BOOL_LITERAL ','
             '"value"' ':' STRING_LITERAL
            '}'
  ;

opt_regions: | regions
  ;

regions: region | region ',' regions
  ;

region:  '{'
          '"name"' ':' STRING_LITERAL ','
          '"isDeployableRoot"' ':' BOOL_LITERAL ','
          '"deployedInRegion"' ':' STRING_LITERAL ','
          '"using"' ':' '[' opt_region_names ']' ','
          '"usingPrivately"' ':' '[' opt_bool_list ']'
         '}'
  ;

opt_region_names: | region_names
  ;

region_names: STRING_LITERAL | STRING_LITERAL ',' region_names
  ;

opt_bool_list: | bool_list
  ;

bool_list: BOOL_LITERAL | BOOL_LITERAL ',' bool_list
  ;

opt_ad_hoc_migrations: | ad_hoc_migrations
  ;

ad_hoc_migrations: ad_hoc_migration | ad_hoc_migration ',' ad_hoc_migrations
  ;

ad_hoc_migration: '{'
                  '"name"' ':' STRING_LITERAL ','
                  '"crc"' ':' STRING_LITERAL ','
                  opt_attributes
                  '"version"' ':' any_integer
                  '}'
  | '{'
                  '"name"' ':' STRING_LITERAL ','
                  '"crc"' ':' STRING_LITERAL ','
                  opt_attributes
                  '"onRecreateOf"' ':' STRING_LITERAL
                  '}'
  ;

```



## Appendix 6: CQL In 20 Minutes
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is a series of examples intended to illustrate the most important features of
the CQL language. This appendix was significantly influenced by a similar article on Python
at https://learnxinyminutes.com/docs/python/

Also of interest:
* http://sqlite.org
* https://learnxinyminutes.com/docs/sql

And with no further delay, CQL in 20 minutes...

```
-- Single line comments start with two dashes

/* C style comments also work
 *
 * C pre-processor features like #include and #define are generally available
 * CQL is typically run through the C pre-processor before it is compile.
 */

/**********************************************************
 * 1. Primitive Datatypes and Operators
 *********************************************************/

-- You have numbers
3     -- an integer
3L    -- a long integer
3.5   -- a real literal
0x10  -- 16 in hex

-- Math is what you would expect
1 + 1     --> 2
8 - 1     --> 7
10 * 2    --> 20
35.0 / 5  --> 7.0

-- Modulo operation, same as C and SQLite
7 % 3    --> 1
-7 % 3   --> -1
7 % -3   --> 1
-7 % 3   --> -1

-- Bitwise operators bind left to right like in SQLite
1 | 4 & 3  -->  1  (not 0)

-- Enforce precedence with parentheses
1 + 3 * 2    --> 7
(1 + 3) * 2  --> 8

-- Use true and false for bools, nullable bool is possible
true    --> how to true
false   --> how to false
null    --> null means "unknown" in CQL like SQLite

-- Negate with not
not true   --> false
not false  --> true
not null   --> null (not unknown is unknown)

-- Logical Operators
1 and 0 --> 0
0 or 1  --> 1
0 and x --> 0 and x not evaluated
1 or x  --> 1 and x not evaluated

-- Remember null is "unknown"
null or false  --> null
null or true   --> true
null and false --> false
null and true  --> null

-- Non-zero values are truthy
0        --> false
4        --> true
-6       --> true
0 and 2  --> 0 (false)
-5 or 0  --> 1 (true)

-- Equality is == or =
1 == 1       --> true
1 = 1        --> true  (= and == are the same thing)
2 == 1       --> false

-- Note that null is not equal to anything (like SQL)
null == 1    --> null (hence not true)
null == null --> null (hence not true)
"x" == "x"   --> true

-- IS lets you compare against null
1 IS 1       --> true
2 IS 1       --> false
null IS 1    --> false
null IS null --> true  (Unknown is Unknown?  Yes it is!)
"x" IS "x"   --> true

-- x IS NOT y is the same as NOT (x IS y)
1 IS NOT 1       --> false
2 IS NOT 1       --> true
null IS NOT 1    --> true
null IS NOT null --> false
"x" IS NOT "x"   --> false

-- Inequality is != or <>
1 != 1       --> false
2 <> 1       --> true
null != 1    --> null
null <> null --> null

-- More comparisons
1 < 10    --> true
1 > 10    --> false
2 <= 2    --> true
2 >= 2    --> true
10 < null --> null

-- To test if a value is in a range
1 < 2 and 2 < 3  --> true
2 < 3 and 3 < 2  --> false

-- BETWEEN makes this look nicer
2 between 1 and 3 --> true
3 between 2 and 2 --> false

-- Strings are created with "x" or 'x'
"This is a string.\n"           -- can have C style escapes (no embedded nulls)
"Th\x69s is a string.\n"        -- even hex literals
'This isn''t a C style string'  -- use '' to escape single quote ONLY

/**********************************************************
 * 2. Simple Variables
 *********************************************************/

-- CQL can call simple libc methods with a no-check declaration
-- we'll need this for later examples so we can do something
-- with our expressions (i.e. print them)
declare procedure printf no check;

call printf("I'm CQL. Nice to meet you!\n");

-- Variables are declared with DECLARE.
-- Keywords and identifiers are not case sensitive.
declare x integer not null;

-- You can call it X, it is the same thing.
set X := 0;

-- All variables begin with a null value if allowed, else a zero value.
declare y integer not null;
if y == 0 then
  call printf("Yes, this will run.\n");
end if;

-- A nullable variable (i.e. not marked with not null) is initialized to null
declare z real;
if z is null then
  call printf("Yes, this will run.\n");
end if;

-- The various types
declare a_blob blob;
declare a_string text;
declare a_real real;
declare an_int integer;
declare a_long long;
declare an_object object;

-- There are some typical SQL synonyms
declare an_int int;
declare a_long long integer;
declare a_long long int;
declare a_long long_int;

-- The basic types can be tagged to make them less miscible
declare m real<meters>;
declare kg real<kilos>;

set m := kg;  -- error!

-- Object variables can also be tagged so that they are not mixed-up easily
declare dict object<dict> not null;
declare list object<list> not null;
set dict := create_dict();  -- an external function that creates a dict
set dict := create_list();  -- error
set list := create_list();  -- ok
set list := dict;           -- error

-- Implied type initialization
LET i := 1;      -- integer not null
LET l := 1L;     -- long not null
LET t := "x";    -- text not null
LET b := x IS y; -- bool not null
LET b := x = y;  -- bool (maybe not null depending on x/y)

-- The psuedo function "nullable" converts the type of its arg to the nullable
-- version of the same thing.

LET n_i := nullable(1);   -- nullable integer variable initialized to 1
LET l_i := nullable(1L);  -- nullable long variable initialized to 1

/**********************************************************
 * 3. Control Flow
 *********************************************************/

-- Just make a variable
declare some_var integer not null;
set some_var := 5

-- Here is an IF statement
if some_var > 10 then
    call printf("some_var is totally bigger than 10.\n")
else if some_var < 10 then  -- else if is optional
    call printf("some_var is smaller than 10.\n")
else -- else is optional
    call printf("some_var is indeed 10.\n")
end if;


-- WHILE loops iterate as usual
declare i integer not null;
set i := 0;
while i < 5
begin
   call printf("%d\n", i);
   set i := i + 1;
end;

-- Use LEAVE to end a loop early
declare i integer not null;
set i := 0;
while i < 500
begin
   if i >= 5 then
     -- we are not going to get anywhere near 500
     leave;
   end if;

   call printf("%d\n", i);
   set i := i + 1;
end;

-- Use CONTINUE to go back to the loop test
declare i integer not null;
set i := 0;
while i < 500
begin
   set i := i + 1;
   if i % 2 then
     -- Note: we to do this after "i" is incremented!
     -- to avoid an infinite loop
     continue;
   end if;

   -- odd numbers will not be printed because of continue above
   call printf("%d\n", i);
end;

 /**********************************************************
 * 4. Complex Expression Forms
 *********************************************************/

 -- Case is an expression, so it is more like the C "?:" operator
 -- than a switch statement.  It is like "?:" on steroids.

 case i              -- a switch expression is optional
   when 1 then "one" -- one or more cases
   when 2 then "two"
   else "other"      -- else is optional
 end;

-- Case with no common expression is a series of independent tests
case
   when i == 1 then "i = one"   -- booleans could be completely unrelated
   when j == 2 then "j = two"   -- first match wins
   else "other"
end;

-- If nothing matches the cases, the result is null.
-- The following expression yields null because 7 is not 1.
case 7 when 1 then "one" end


-- Case is just an expression, so it can nest
case X
  when 1
    case y when 1 "x:1 y:1"
           else "x:1 y:other"
    end
  else
    case when z == 1 "x:other z:1"
         else "x:other z:other"
    end
end;

-- IN is used to test for membership
5 IN (1, 2, 3, 4, 5)  --> true
7 IN (1, 2)           --> false
null in (1, 2, 3)     --> null
null in (1, null, 3)  --> null  (null == null is not true)
7 NOT IN (1, 2)       --> true
null not in (null, 3) --> null

/**********************************************************
 * 4. Working with and "getting rid of" null
 *********************************************************/

-- Null can be annoying, you might need a not null value.
-- In most operations null is radioactive:
null + x     --> null
null * x     --> null
null == null --> null

-- IS and IS NOT always return 0 or 1
null is 1     -> 0
1 is not null -> 1

-- COALESCE returns the first non null arg, or the last arg if all were null.
-- If the last arg is not null, you get a non null result for sure.
-- The following is never null, but it's false if either x or y is null
COALESCE(x==y, false) -> thought excercise: how is this different than x IS y?

-- IFNULL is coalesce with 2 args only (COALESCE is more general)
IFNULL(x, -1) --> use -1 if x is null

-- The reverse, NULLIF, converts a sentinel value to unknown, more exotic
NULLIF(x, -1) --> if x is -1 then use null

-- the else part of a case can get rid of nulls
CASE when x == y then 1 else 0 end;  --> true iff x = y and neither is null

-- CASE can be used to give you a default value after various tests
-- The following expression is never null; "other" is returned if x is null.
CASE when x > 0 then "pos" when x < 0 then "neg" else "other" end;

-- You can "throw" out of the current procedure (see exceptions below)
declare x integer not null;
set x := ifnull_throw(nullable_int_expr); -- returns non null, throws if null

-- If you have already tested the expression then control flow analysis
-- improves its type to "not null".  Many common check patterns are recognized.
if nullable_int_expr is not null then
  -- nullable_int_expression is known to be not null in this context
  set x := nullable_int_expr;
end if;

/**********************************************************
 * 5. Tables, Views, Indices, Triggers
 *********************************************************/

-- Most forms of data definition language DDL are supported.
-- "Loose" DDL (outside of any procedure) simply declares
-- schema, it does not actually create it; the schema is assumed to
-- exist as you specified.

create table T1(
  id integer primary key,
  t text,
  r real
);

create table T2(
  id integer primary key references T1(id),
  l long,
  b blob
);

-- CQL can take a series of schema declarations (DDL) and
-- automatically create a procedure that will materialize
-- that schema and even upgrade previous versions of the schema.
-- This system is discussed in Chapter 10 of The Guide.
-- To actually create tables and other schema you need
-- procedures that look like the below:

create proc make_tables()
begin
  create table T1 if not exists (
    id integer primary key,
    t text,
    r real
  );
end;

-- Views are supported
create view V1 as (select * from T1);

-- Triggers are supported
create trigger if not exists trigger1
  before delete on T1
begin
  delete from T2 where id = old.id;
end;

-- Indices are supported
create index I1 on T1(t);
create index I2 on T1(r);

-- The various drop forms are supported
drop index I1;
drop index I2;
drop view V1;
drop table T2;
drop table T1;

-- A complete discussion of DDL is out of scope, refer to sqlite.org

/**********************************************************
 * 6. Selecting Data
 *********************************************************/

-- We will use this scratch variable in the following examples
declare rr real;

-- First observe CQL is a two-headed language
set rr := 1+1;           -- this is evaluated in generated C code
set rr := (select 1+1);  -- this expresion goes to SQLite; SQLite does the addition

-- CQL tries to do most things the same as SQLite in the C context
-- but some things are exceedingly hard to emulate correctly.
-- Even simple looking things such as:
set rr := (select cast("1.23" as real));   -->  rr := 1.23
set rr := cast("1.23" as real);            -->  error (not safe to emulate SQLite)

-- In general, numeric/text conversions have to happen in SQLite context
-- because the specific library that does the conversion could be and usually
-- is different than the one CQL would use.  It would not do to give different answers
-- in one context or another so those conversions are simply not supported.

-- Loose concatenation is not supported because of the implied conversions.
-- Loose means "not in the context of a SQL statement".
set r := 1.23;
set r := (select cast("100"||r as real));  --> 1001.23 (a number)
set r := cast("100"||r as real);  --> error, concat not supported in loose expr

-- A simple insertion
insert into T1 values (1, "foo", 3.14);

-- Finally, reading from the database
set r := (select r from T1 where id = 1);  --> r = 3.14

-- The (select ...) form requires the result to have at least one row.
-- You can use IF NOTHING forms to handle other cases such as:
set r := (select r from T1
          where id = 2
          if nothing -1);  --> r = -1

-- If the SELECT statement might return a null result you can handle that as well
set r := (select r from T1
          where id = 2
          if nothing or null -1);  --> r = -1

-- With no IF NOTHING clause, lack of a row will cause the SELECT expression to throw
-- an exception.  IF NOTHING THROW merely makes this explicit.
set r := (select r from T1 where id = 2 if nothing throw);  --> will throw

/**********************************************************
 * 6. Procedures, Results, Exceptions
 *********************************************************/

-- Procedures are a list of statements that can be executed, with arguments.
create proc hello()
begin
  call printf("Hello, world\n");
end;

-- IN, OUT, and INOUT parameters are possible
create proc swizzle(x integer, inout y integer, out z real not null)
begin
  set y := x + y;  -- any computation you like

  -- bizarre way to compute an id but this is just an illustration
  set z := (select r from T1 where id = x if nothing or null -1);
end;

-- Procedures like "hello" (above) have a void signature -- they return nothing
-- as nothing can go wrong. Procedures that use the database like "swizzle" (above)
-- can return an error code if there is a problem.
-- "will_fail" (below)  will always return SQLITE_CONSTRAINT, the second insert
-- is said to "throw".  In CQL exceptions are just result codes.
create proc will_fail()
begin
   insert into T1 values (1, "x", 1);
   insert into T1 values (1, "x", 1);  --> duplicate key
end;

-- DML that fails generates an exception and
-- exceptions can be caught. Here is a example:
create proc upsert_t1(
  id_ integer primary key,
  t_ text,
  r_ real
)
begin
  begin try
    -- try to insert
    insert into T1(id, t, r) values (id_, t_, r_);
  end try;
  begin catch
    -- if the insert fails, try to update
    update T1 set t = t_, r = r_ where id = id_;
  end catch;
end;

-- Shapes can be very useful in avoiding boilerplate code
-- the following is equivalent to the above.
-- More on shapes later.
create proc upsert_t1(LIKE t1) -- my args are the same as the columns of T1
begin
  begin try
    insert into T1 from arguments
  end try;
  begin catch
    update T1 set t = t_, r = r_ where id = id_;
  end catch;
end;

-- You can (re)throw an error explicitly.
-- If there is no current error you get SQLITE_ERROR
create proc upsert_wrapper(LIKE t1) -- my args are the same as the columns of T1
begin
  if r_ > 10 then throw end if; -- throw if r_ is too big
  call upsert_t1(from arguments);
end;

-- Procedures can also produce a result set.
-- The compiler generates the code to create this result set
-- and helper functions to read rows out of it.
create proc get_low_r(r_ real)
begin
   -- optionally insert some rows or do other things
   select * from T1 where T1.r <= r_;
end;

-- A procedure can choose between various results, the choices must be compatible.
-- The last "select" to run controls the ultimate result.
create proc get_hi_or_low(r_ real, hi_not_low bool not null)
begin
  -- trying to do this with one query would result in a poor plan, so
  -- instead we use two economical queries.
  if hi_not_low then
    select * from T1 where T1.r >= r_;
  else
    select * from T1 where T1.r <= r_;
  end if;
end;

-- Using IF to create to nice selects above is a powerful thing.
-- SQLite has no IF, if we tried to create a shared query we get
-- something that does not use indices at all.  As in the below.
-- The two-headed CQL beast has its advantages!
select * from T1 where case hi_not_low then T1.r >= r_ else T1.r <= r_ end;

-- You can get the current return code and use it in your CATCH logic.
-- This upsert is a bit better than the first:
create proc upsert_t1(LIKE t1) -- my args are the same as the columns of T1
begin
  begin try
    insert into T1 from arguments
  end try;
  begin catch;
    if @rc == 19 /* SQLITE_CONSTRAINT */ then
      update T1 set t = t_, r = r_ where id = id_;
    else
      throw;  -- rethrow, something bad happened.
    end if;
  end catch;
end;

-- By convention, you can call a procedure that has an OUT argument
-- as its last argument using function notation.  The out argument
-- is used as the return value.   If the called procedure uses the
-- database then it could throw which causes the caller to throw
-- as usual.
create proc fib(n integer not null, out result integer not null)
begin
   set result := case n <= 2 then 1 else fib(n-1) + fib(n-2) end;
end;

/**********************************************************
 * 7. Statement Cursors
 *********************************************************/

-- Statement cursors let you iterate over a select result.
-- Here we introduce cursors, LOOP and FETCH.
create proc count_t1(r_ real, out rows_ integer not null)
begin
  declare rows integer not null;  -- starts at zero guaranteed
  declare C cursor for select * from T1 where r < r_;
  loop fetch C -- iterate until fetch returns no row
  begin
    -- goofy code to illustrate you can process the cursor
    -- in whatever way you deem appropriate
    if C.r < 5 then
      rows := rows + 1; -- count rows with C.r < 5
    end if;
  end;
  set rows_ := rows;
end;

-- Cursors can be tested for presence of a row
-- and they can be closed before the enumeration is finished.
-- As before the below is somewhat goofy example code.
create proc peek_t1(r_ real, out rows_ integer not null)
begin
   /* rows_ is set to zero for sure! */
   declare C cursor for select * from T1 where r < r_ limit 2;
   open C;  -- this is no-op, present because other systems have it
   fetch C;  -- fetch might find a row or not
   if C then  -- cursor name as bool indicates presence of a row
     set rows_ = rows_ + (C.r < 5);
     fetch C;
     set rows_ = rows_ + (C and C.r < 5);
   end if;
   close C;  -- cursors auto-closed at end of method but early close possible
end;

-- The FETCH...INTO form can be used to fetch directly into variables
fetch C into id_, t_, r_;  --> loads named locals instead of C.id, C.t, C.r

-- A procedure can be the source of a cursor
declare C cursor for call get_low_r(3.2);  -- valid cursor source

-- OUT can be used to create a result set that is just one row
create proc one_t1(r_ real)
begin
   declare C cursor for select * from T1 where r < r_ limit 1;
   fetch C;
   out C;  -- emits a row if we have one, no row is ok too, empty result set.
end;

/**********************************************************
 * 8. Value Cursors, Out, and Out Union
 *********************************************************/

-- To consume a procedure that uses "out" you can declare a value cursor.
-- By itself such as cursor does not imply use of the database, but often
-- the source of the cursor uses the database.  In this example
-- consume_one_t1 uses the database because of the call to one_t1.
create proc consume_one_t1()
begin
  -- a cursor whose shape matches the one_t1 "out" statement
  declare C cursor like one_t1;

  -- load it from the call
  fetch C from call one_t1(7);
  if C.r > 10 then
    -- use values as you see fit
    call printf("woohoo");
  end if;
end;

-- You can do the above in one step with the compound form:
declare C cursor fetch from call one_t1(7); -- declare and fetch

-- Value cursors can come from anywhere and can be a procedure result
create proc use_t1_a_lot()
begin
  -- T1 is the same shape as one_t1, this will work, too
  declare C cursor like T1;
  fetch C from call one_t1(7);  -- load it from the call

  -- some arbitrary logic might be here

  -- load C again with different args
  fetch C from call one_t1(12);   -- load it again

  -- some arbitrary logic might be here

  -- now load C yet again with explicit args
  fetch C using
     1 id,
     "foo" t,
     8.2 r;

  -- now return it
  out C;
end;

-- Make a complex result set one row at a time
create proc out_union_example()
begin
  -- T1 is the same shape as one_t1, this will work, too
  declare C cursor like T1;

  -- load it from the call
  fetch C from call one_t1(7);

  -- note out UNION rather than just out, indicating potentially many rows
  out union C;

  -- load it again with different args
  fetch C from call one_t1(12);
  out union C;

  -- do something, then maybe load it again with explicit args
  fetch C using
     1 id,
     "foo" t,
     8.2 r;
  out union C;

  -- we have generated a 3 row result set
end;

-- Consume the above
create proc consume_result()
begin
  declare C cursor for call out_union_example();
  loop fetch C
  begin
    -- use builtin cql_cursor_format to make the cursor into a string
    call printf("%s\n", cql_cursor_format(C)); --> prints every column and value
  end;
end;

/**********************************************************
 * 9. Named Types and Enumerations
 *********************************************************/

-- Create a simple named types
declare my_type type integer not null;   -- make an alias for integer not null
declare i my_type;  -- use it, "i" is an integer

-- Mixing in type kinds is helpful
declare distance type real<meters>;  -- e.g., distances to be measured in meters
declare time type real<seconds>;     -- e.g., time to be measured in seconds
declare job_id type long<job_id>;
declare person_id type long<person_id>;

-- With the above done
--  * vars/cols of type "distance" are incompatible with those of type "time"
--  * vars/cols of types job_id are incompatible with person_id
-- This is true even though the underlying type is the same for both!

-- ENUM declarations can have any numeric type as their base type
declare enum implement integer (
   pencil,       -- values start at 1 unless you use = to choose something
   pen,          -- the next enum gets previous + 1 as its value (2)
   brush = 7     -- with = expression you get the indicated value
);

-- The above also implicitly does this
declare implement type integer<implement> not null;

-- Using the enum -- simply use dot notation
declare impl implement;
set impl := implement.pen;  -- value "2"

-- You can emit an emum into the current .h file we are going to generate.
-- Do not put this directive in an include file, you want it to go to one place.
-- Instead, pick one compiland that will "own" the emission of the enum.
-- C code can then #include that one .h file.
@emit_enums implement;

/**********************************************************
 * 10. Shapes and Their Uses
 *********************************************************/

-- Shapes first appeared to help define value cursors like so:

-- A table or view name defines a shape
declare C cursor like T1;

-- The result of a proc defines a shape
declare D cursor like one_t1;

-- A dummy select statement defines a shape (the select does not run)
-- this one is the same as (x integer not null, y text not null)
declare E cursor like select 1 x, "2" y;

-- Another cursor defines a shape
declare F cursor like C;

-- The arguments of a procedure define a shape. If you have
-- create proc count_t1(r_ real, out rows_ integer not null) ...
-- the shape will be:
--  (r_ real, rows_ integer not null)
declare G cursor like count_t1 arguments;

-- A loaded cursor can be used to make a call
call count_t1(from G);  -- the args become G.r_, G.rows_

-- A shape can be used to define a procedures args, or some of the args
-- In the following "p" will have arguments:s id_, t_, and r_ with types
-- matching table T1.
-- Note: To avoid ambiguity, an _ was added to each name!
create proc p(like T1)
begin
  -- do whatever you like
end;

-- The arguments of the current procedure are a synthetic shape
-- called "arguments" and can used where other shapes can appear.
-- For instance, you can have "q" shim to "p" using this form:
create proc q(like T1, print bool not null)
begin
  -- maybe pre-process, silly example
  set id_ := id_ + 1;

  -- shim to p
  call p(from arguments); -- pass my args through, whatever they are

  -- maybe post-process, silly example
  set r_ := r_ - 1;

  if print then
    -- convert args to cursor
    declare C like q arguments;
    fetch C from arguments;
    call printf("%s\n", cql_cursor_format(C)); --> prints every column and value
  end if;

  -- insert a row based on the args
  insert into T1 from arguments;
end;

-- You an use a given shape more than once if you name each use.
-- This would be more exciting if T1 was like a "person" or something.
create proc r(a like T1, b like T1)
begin
  call p(from a);
  call p(from b);
  -- you can refer to a.id, b.id etc.
  declare C like a;
  fetch C from a;
  call printf("%s\n", cql_cursor_format(C));
  fetch C from b;
  call printf("%s\n", cql_cursor_format(C));
end;

-- Shapes can be subsetted, for instance in the following example
-- only the arguments that match C are used in the FETCH.
fetch C from arguments(like C);

-- Fetch the columns of D into C using the cursor D for the data source.
-- Other columns get default values.
fetch C(like D) from D;

-- Use the D shape to load C, dummy values for the others.
-- In this example, dummy_seed means use the provided value, 11, for
-- any numerics that are not specified (not in D) and and use
-- "col_name_11" for any strings/blobs.  This pattern is useful in test code
-- to create dummy data, hence the name.
fetch C(like D) from D @dummy_seed(11);

-- Use the Z shape to control which fields are copied.
-- Use the dummy value even if the field is nullable and null would have be ok.
fetch C(like Z) from D(like Z) @dummy_seed(11) @dummy_nullables;

-- The above patterns also work for insert statements
-- The shape constraints are generally useful.  The dummy data
-- sources are useful for inserting test data.
insert into T1(like Z) from D(like Z) @dummy_seed(11) @dummy_nullables;

-- We'll need this dummy procedure some_shape so we can use its return
-- value in the examples that follow.  We will never actual create this
-- proc, we only declare it to define the shape, so this is kind of like
-- a typedef.
declare proc some_shape() (x integer not null, y integer not null, z integer not null);

-- You can make a helper procedure to create test args that are mostly constant
-- or computable.
create get_foo_args(X like some_shape, seed_ integer not null)
begin
  declare C cursor like foo arguments;
  -- any way of loading C could work this is one
  fetch C(like X) from X @dummy_seed(seed_);
  out C;
end;

-- Now we can use the "get_foo_args" to get full set of arguments for "foo" and then
-- call "foo" with those arguments.  In this example we're providing
-- some of the arguments explicitly, "some_shape" is the part of the args that
-- needs to manually vary in each test iteration, the rest of the arguments will
-- be dummy values.  There could be zillions of args in either category.
-- In the below "some_shape" is going to get the manual values 1, 2, 3 while 100
-- will be the seed for the dummy args.
declare foo_args cursor fetch from call get_foo_args(1,2,3, 100);
call foo(from foo_args);

/**********************************************************
 * 11. INSERT USING and FETCH USING
 *********************************************************/

 -- This kind of thing is a pain
 insert into foo(a, b, c, d, e, f, g)
    values(1, 2, 3, null, null, 5, null);

-- Instead, write this form:
insert into foo USING
    1 a, 2 b, 3 c, null d, null e, 5 f, null g;

-- The FETCH statement can also be "fetch using"
declare C cursor like foo;
fetch C USING
    1 a, 2 b, 3 c, null d, null e, 5 f, null g;
```

If you've read this far you know more than most now.  :)



## Appendix 7: CQL Anti-patterns
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

These are a few of the antipatterns I've seen while travelling through various CQL source files.  They are in various categories.

Refer also to Appendix 8: CQL Best Practices.

### Common Schema

For these examples let's create a couple of tables we might need for examples

```sql
CREATE TABLE foo (
    id integer primary key,
    name text
);

CREATE TABLE bar (
    id integer primary key,
    rate real
);
```

### Declarations

```sql
DECLARE v LONG NOT NULL;
SET v := 1;
```

better

```sql
LET v := 1L;  -- long literals have the L suffix like in C
```

Similarly

```sql
DECLARE v REAL NOT NULL;
SET v := 1;
```

better

```sql
LET v := 1.0; -- use scientific notation or add .0 to make a real literal
```

### Casts

Redundant casts fatten the code and don't really add anything to readability.  Sometimems it's necessary to cast NULL to
a  particular type so that you can be sure that generated result set has the right data type, but most of the casts
below are not necessary.

```sql
  SELECT
    CAST(foo.id as INTEGER) as id,
    CAST(foo.name as TEXT) as name,
    CAST(NULL as REAL) as rate
  FROM foo
UNION ALL
  SELECT
    CAST(bar.id as INTEGER) as id,
    CAST(NULL as TEXT) as name,
    CAST(bar.rate as REAL) as rate
  FROM bar
```

Better

```sql
  SELECT
    foo.id,
    foo.name,
    CAST(NULL as REAL) as rate
  FROM foo
UNION ALL
  SELECT
    bar.id,
    CAST(NULL as TEXT) as name,
    bar.rate
  FROM bar
```

It's possible to do the following to make this even cleaner:

```sql
-- somewhere central
#define NULL_TEXT CAST(NULL as TEXT)
#define NULL_REAL CAST(NULL as REAL)
#define NULL_INT CAST(NULL as INTEGER)
#define NULL_LONG CAST(NULL as LONG)
```

Then you can write

```sql
  SELECT
    foo.id,
    foo.name,
    NULL_REAL as rate
  FROM foo
UNION ALL
  SELECT
    bar.id,
    NULL_TEXT as name,
    bar.rate
  FROM bar
```

#### Booleans

TRUE and FALSE can be used as boolean literals.

SQLite doesn't care about the type but CQL will get the type information it needs to make the columns of type BOOL

```sql
  SELECT
    foo.id,
    foo.name,
    NULL_REAL as rate,
    TRUE as has_name,  -- this is a bit artificial but you get the idea
    FALSE as has_rate
  FROM foo
UNION ALL
  SELECT
    bar.id,
    NULL_TEXT as name,
    bar.rate,
    FALSE as has_name,
    TRUE as has_rate
  FROM bar
```

### Boolean expressions and CASE/WHEN

It's easy to get carried away with the power of `CASE` expressions, I've seen this kind of thing:

```sql
CAST(CASE WHEN foo.name IS NULL THEN 0 ELSE 1 END AS BOOL)
```

But this is simply

```sql
foo.name IS NOT NULL
```

In general, if your case alternates are booleans a direct boolean expression would have served you better.

### CASE and CAST and NULL

Somtimes there's clamping or filtering going on in a case statement

```sql
CAST(CASE WHEN foo.name > 'm' THEN foo.name ELSE NULL END AS TEXT)
```

Here the `CAST` is not needed at all so we could go to

```sql
CASE WHEN foo.name > 'm' THEN foo.name ELSE NULL END
```

`NULL` is already the default value for the `ELSE` clause so you never need `ELSE NULL`

So better:

```sql
CASE WHEN foo.name > 'm' THEN foo.name END
```

### Filtering out NULLs

Consider

```sql
SELECT *
    FROM foo
    WHERE foo.name IS NOT NULL AND foo.name > 'm';
```

There's no need to test for `NOT NULL` here, the boolean will result in `NULL` if `foo.name` is null
which is not true so the `WHERE` test will fail.

Better:

```sql
SELECT *
    FROM foo
    WHERE foo.name > 'm';
```

### Not null boolean expressions

In this statement we do not want to have a null result for the boolean expression

```sql
SELECT
    id,
    name,
    CAST(IFNULL(name > 'm', 0) AS BOOL) AS name_bigger_than_m
    FROM FOO;
```

So now we've made several mistakes.  We could have used the usual `FALSE` defintion to avoid the cast.
But even that would have left us with an IFNULL that's harder to read.  Here's a much simpler formulation:

```sql
SELECT
    id,
    name,
    name > 'm' IS TRUE AS name_bigger_than_m
    FROM FOO;
```

Even without the `TRUE` macro you could do `IS 1` above and still get a result of type `BOOL NOT NULL`

### Using `IS` when it makes sense to do so

This kind of boolean expression is also verbose for no reason

```sql
    rate IS NOT NULL AND rate = 20
```

In a `WHERE` clause probably `rate = 20` suffices but even if you really need a `NOT NULL BOOL`
result the expression above is exactly what the `IS` operator is for.  e.g.

```sql
    rate IS 20
```

The `IS` operator is frequently avoided except for `IS NULL` and `IS NOT NULL` but it's a general equality operator
with the added semantic that it never returns `NULL`.   `NULL IS NULL` is true.  `NULL IS [anything not null]` is false.

### Left joins that are not left joins

Consider

```sql
  SELECT foo.id,
         foo.name,
         bar.rate
  FROM foo
  LEFT JOIN bar ON foo.id = bar.id
  WHERE bar.rate > 5;
```

This is no longer a left join because the `WHERE` clause demands a value for at least one column from `bar`.

Better:

```sql
  SELECT foo.id,
         foo.name,
         bar.rate
  FROM foo
  INNER JOIN bar ON foo.id = bar.id
  WHERE bar.rate > 5;
```



## Appendix 8: CQL Best Practices
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

This is a brief discussion of every statement type and some general best practices for that statement.
The statements are in mostly alphabetical order except related statements were moved up in the order
to make logical groups.

Refer also to Appendix 7: CQL Anti-patterns.

### Data Definition Language (DDL)

* `ALTER TABLE ADD COLUMN`
* `CREATE INDEX`
* `CREATE PROC`
* `CREATE TABLE`
* `CREATE TRIGGER`
* `CREATE VIEW`
* `CREATE VIRTUAL TABLE`
* `DROP INDEX`
* `DROP TABLE`
* `DROP TRIGGER`
* `DROP VIEW`

These statements almost never appear in normal procedures and generally should be avoided.  The normal way of handling schema in CQL
is to have one or more files declare all the schema you need and then let CQL create a schema upgrader for you.  This means you'll
never manually drop tables or indices etc.  The `create` declarations with their annotations will totally drive the schema.

Any ad hoc DDL is usually a very bad sign.  Test code is an obvious exception to this as it often does setup and teardown
of schema to set up things for the test.

### Ad Hoc Migrations

* `@SCHEMA_AD_HOC_MIGRATION`

This is a special upgrade step that should be taken at the version indicated in the statement.  These can be quite complex and even super important
but should not be used lightly.  Any migration procedure has to be highly tolerant of a variety of incoming schema versions and previous partial successes.
In any case this directive should not appear in normal code.  It should be part of the schema DDL declarations.

### Transactions

* `BEGIN TRANSACTION`
* `COMMIT TRANSACTION`
* `ROLLBACK TRANSACTION`

Transactions do not nest and most procedures do not know the context in which they will be called, so the vast majority of
procedures will not and should not actually start transactions.  You can only do this if you know, somehow, for sure, that
the procedure in question is somehow a "top level" procedure.  So generally, don't use these statements.


### Savepoints

* `SAVEPOINT`
* `ROLLBACK TO SAVEPOINT`
* `RELEASE SAVEPOINT`
* `PROC SAVEPOINT`
* `COMMIT RETURN`
* `ROLLBACK RETURN`

Savepoints are the preferred tool for having interim state that can be rolled back if needed.  You can use ad hoc
savepoints, just give your save point and name then use `RELEASE SAVEPOINT` to commit it, or else `ROLLBACK TO SAVEPOINT`
followed by a `RELEASE` to abort it.  Note that you always `RELEASE` savepoints in both the rollback and the commit case.

Managing savepoints can be tricky, especially given the various error cases.  They combine nicely with `TRY CATCH` to do
this job.  However, even that is a lot of boilerplate.  The best way to use savepoints is with `PROC SAVEPOINT BEGIN` .. `END`;

When you use `PROC SAVEPOINT`, a savepoint is created for you with the name of your procedure.  When the block exits
the savepoint is released (committed).  However you also get an automatically generated try/catch block which will
rollback the savepoint if anything inside the block were to invoke `THROW`.  Also, you may not use a regular `RETURN`
inside this block, you must use either `ROLLBACK RETURN` or `COMMIT RETURN`.  Both of these directly indicate the fate
of the automatically generated statement when they run.  This gives you useful options to early-out (with no error)
while keeping or abandoning any work in progress.  Of course you can use `THROW` to return an error and
abandon the work in progress.

### Compilation options

* `@ENFORCE_NORMAL`
* `@ENFORCE_POP`
* `@ENFORCE_PUSH`
* `@ENFORCE_RESET`
* `@ENFORCE_STRICT`

CQL allows you to specify a number of useful options such as "do not allow Window Functions" or "all foreign keys must choose some update or delete strategy".
These additional enforcements are designed to prevent errors.  Because of this they should be established once, somewhere central and they should be rarely
if ever overridden.  For instance `@ENFORCE_NORMAL WINDOW FUNCTION` would allow you to use window functions again, but this is probably a bad idea. If
strict mode is on, disallowing them, that probably means your project is expected to target versions of SQLite that do not have window functions.  Overriding
that setting is likely to lead to runtime errors.

In general you don't want to see these options in most code.

### Previous Schema

* `@PREVIOUS_SCHEMA`

CQL can ensure that the current schema is compatible with the previous schema, meaning that an upgrade script could reasonably be generated to go from the
previous to the current.  This directive demarks the start of the previous schema section when that validation happens.  This directive is useless except
for creating that schema validation so it should never appear in normal procedures.

### Schema Regions

* `@BEGIN_SCHEMA_REGION`
* `@DECLARE_DEPLOYABLE_REGION`
* `@DECLARE_SCHEMA_REGION`
* `@END_SCHEMA_REGION`

CQL allows you to declare arbitrary schema regions and limit what parts of the schema any given region may consume.  This helps you to prevent schema from getting
entangled.  There is never a reason to use this directives inside normal procedures;  They should appear only in your schema declaration files.

### Schema Version

* `@SCHEMA_UPGRADE_SCRIPT`
* `@SCHEMA_UPGRADE_VERSION`

The `@SCHEMA_UPGRADE_SCRIPT` directive is only used by CQL itself to declare that the incoming file is an autogenerated schema upgrade script.
These scripts have slightly different rules for schema declaration that are not useful outside of such scripts.  So you should never use this.

`@SCHEMA_UPGRADE_VERSION` on the other hand is used if you are creating a manual migration script.  You need this script to run in the context
of the schema version that it affects.  Use this directive at the start of the file to do so.  Generally manual migration scripts are to be
avoided so hopefully this directive is rarely if ever used.


### C Text Echo

* `@ECHO`

This directive emits plain text directly into the compiler's output stream.  It can be invaluable for adding new runtime features and for ensuring that
(e.g.) additional `#include` or `#define` directives are present in the output but you can really break things by over-using this feature.  Most parts
of the CQL output are subject to change so any use of this should be super clean.  The intended use was, as mentioned, to allow an extra `#include` in your code
so that CQL could call into some library.  Most uses of this combine with `DECLARE FUNCTION` or `DECLARE PROCEDURE` to declare an external entity.

### Enumerations

* `DECLARE ENUM`
* `@EMIT_ENUMS`

Avoid embedded constants whenever possible.  Instead declare a suitable enumeration.   Use `@EMIT_ENUMS Some_Enum` to get the enumeration
constants into the generated .h file for C. But be sure to do this only from one compiland.  You do not want the enumerations in every .h file.
Choose a single .sql file (not included by lots of other things) to place the `@EMIT_ENUMS` directive. You can make a file specifically for this
purpose if nothing else is serviceable.

### Cursor Lifetime

* `CLOSE`
* `OPEN`

The `OPEN` statement is a no-op, SQLite has no such notion.  It was included because it is present in `MYSQL` and other variants and its inclusion can
ease readability sometimes.  But it does nothing.   The `CLOSE` statement is normally not necessary because all cursors are closed at the end of the
procedure they are declared in (unless they are boxed, see below).  You only need `CLOSE` if you want to close a global cursor (which has no scope)
or if you want to close a local cursor "sooner" because waiting to the end of the procedure might be a very long time.  Using close more than once
is safe, the second and later close operations do nothing.

### Procedure Calls and Exceptions

* `CALL`
* `THROW`
* `TRY CATCH`

Remember that if you call a procedure and it uses `THROW` or else uses some SQL that failed, this return code will cause your
code to `THROW` when the procedure returns.  Normally that's exactly what you want, the error will ripple out and some top-level
`CATCH` will cause a `ROLLBACK` and the top level callers sees the error.  If you have your own rollback needs be sure to install
your own `TRY`/`CATCH` block or else use `PROC SAVEPOINT` as above to do it for you.

Inside of a `CATCH` block you can use the special variable `@RC` to see the most recent return code from SQLite.


### Control Flow with "Big Moves"

* `CONTINUE`
* `LEAVE`
* `RETURN`

These work as usual but beware, you can easily use any of these to accidentally leave a block with a savepoint or transaction
and you might skip over the `ROLLBACK` or `COMMIT` portions of the logic.  Avoid this problem by using `PROC SAVEPOINT`.


### Getting access to external code

* `DECLARE FUNCTION`
* `DECLARE SELECT FUNCTION`
* `DECLARE PROCEDURE`

The best practice is to put any declarations into a shared header file which you can `#include` in all the places it is needed.
This is especially important should you have to forward declare a procedure.  CQL normally provides exports for all procedures
so you basically get an automatically generated and certain-to-be-correct `#include` file.  But, if the procedures are being compiled
together then an export file won't have been generated yet at the time you need it;  To work around this you use the ``DECLARE PROCEDURE``
form.  However, procedure declarations are tricky;  they include not just the type of the arguments but the types of any/all of the
columns in any result set the procedure might have.  This must not be wrong or callers will get unpredictable failures.

The easiest way to ensure it is correct is to use the same trick as you would in C -- make sure that you `#include` the declaration
the in the translation unit with the definition.  If they don't match there will be an error.

A very useful trick: the error will include the exact text of the correct declaration.  So if you don't know it, or are too lazy to
figure it out; simply put `ANY` declaration in the shared header file and then paste in the correct declaration from the error.  should
the definition ever change you will get a compilation error which you can again harvest to get the correct declaration.

In this way you can be sure the declarations are correct.

Functions have no CQL equivalent, but they generally don't change very often.  Use `DECLARE FUNCTION` to allow access to some C code
that returns a result of some kind.   Be sure to add the `CREATE` option if the function returns a reference that the caller owns.

Use `DECLARE SELECT FUNCTION` to tell CQL about any User Defined Functions you have added to SQLite so that it knows how to call them.
Note that CQL does not register those UDFs, it couldn't make that call lacking the essential C information required to do so.  If you
find that you are getting errors when calling a UDF the most likely reason for the failure is that the UDF was declared but never
registered with SQLite at runtime.  This happens in test code a lot -- product code tends to have some central place to register the
UDFs and it normally runs at startup, e.g. right after the schema is upgraded.


### Regular Data Manipulation Language (DML)

* `DELETE`
* `INSERT`
* `SELECT`
* `UPDATE`
* `UPSERT`

These statements are the most essential and they'll appear in almost every procedure. There are a few general best practices we can go over.

 * Try to do as much as you can in one batch rather than iterating;  e.g.
   * don't write a loop with a `DELETE` statement that deletes one row if you can avoid it, write a delete statement that deletes all you need to delete
   * don't write a loop with of `SELECT` statement that fetches one row, try to fetch all the rows you need with one select

 * Make sure `UPSERT` is supported on the SQLite system you are using, older versions do not support it

 * Don't put unnecessary casts in your `SELECT` statements, they just add fat
 * Don't use `CASE`/`WHEN` to compute a boolean, the boolean operations are more economical (e.g. use `IS`)
 * Don't use `COUNT` if all you need to know is whether a row exists or not, use `EXISTS`
 * Don't use `GROUP BY`, `ORDER BY`, or `DISTINCT` on large rowsets, the sort is expensive and it will make your `SELECT` statements write to disk rather than just read

 * Always use the `INSERT INTO FOO USING` form of the `INSERT` statement, it's much easier to read than the standard form and compiles to the same thing


### Variable and Cursor declarations

* `DECLARE OUT CALL`
* `DECLARE`
* `LET`
* `SET`

These are likely to appear all over as well.  If you can avoid a variable declaration by using `LET` then do so;  The code will be more concise and you'll
get the exact variable type you need.  This is the same as `var x = foo();` in other languages.  Once the variable is declared use `SET`.

You can save yourself a lot of declarations of `OUT` variables with `DECLARE OUT CALL`.  That declaration form automatically declares the `OUT` variables used
in the call you are about to make with the correct type.  If the number of arguments changes you just have to add the args you don't have to also add
new declarations.

The `LIKE` construct can be used to let you declare things whose type is the same as another thing.  Patterns like `DECLARE ARGS CURSOR LIKE FOO ARGUMENTS`
save you a lot of typing and also enhance correctness.  There's a whole chapter dedicated to "shapes" defined by `LIKE`.


### Query Plans

* `EXPLAIN`

Explain can be used in front of other queries to generate a plan.  The way SQLite handles this is that you fetch the rows of the plan as usual.  So basically
`EXPLAIN` is kind of like `SELECT QUERY PLAN OF`.  This hardly ever comes up in normal coding.  CQL has an output option where it will generate code that gives you
the query plan for a procedures queries rather than the normal body of the procedure.

### Fetching Data from a Cursor or from Loose Data

* `FETCH`
* `UPDATE CURSOR`

The `FETCH` statement has many variations, all are useful at some time or another. There are a few helpful guidelines.

* If fetching from loose values into a cursor use the `FETCH USING` form (as you would with `INSERT INTO USING`) because it is less error prone
* `FETCH INTO` is generally a bad idea, you'll have to declare a lot of variables, instead just rely on automatic storage in the cursor e.g.
  `fetch my_cursor` rather than `fetch my_cursor into a, b, c`
* If you have data already in a cursor you can mutate some of the columns using `UPDATE CURSOR`, this can let you adjust values or apply defaults

### Control Flow

* `IF`
* `LOOP`
* `SWITCH`
* `WHILE`

These are your bread and butter and they will appear all over.  One tip: Use the `ALL VALUES` variant of switch whenever possible to ensure that you haven't missed any cases.

### Manual Control of Results

* `OUT`
* `OUT UNION`

* If you know you are producing exactly one row `OUT` is more economical than `SELECT`
* If you need complete flexibility on what rows to produce (e.g. skip some, add extras, mutate some) then `OUT UNION` will give you that, use it only when needed, it's more expensive than just `SELECT`


### CTEs and Shared Fragments

To understand what kinds of things you can reasonably do with fragments, really you
just have to understand the things that you can do with common table expressions or
CTEs.  For those who don't know, CTEs are the things you declare
in the WITH clause of a SELECT statement.  They're kind of like local views.  Well,
actually, they are exactly like local views.

Query fragments help you to define useful CTEs so basically what you can do
economically in a CTE directly determines what you can do economically in a fragment.

To demonstrate some things that happen with CTEs we're going to use these three
boring tables.

```sql
create table A
(
   id integer primary key,
   this text not null
);

create table B
(
   id integer primary key,
   that text not null
);

create table C
(
   id integer primary key,
   other text not null
);
```

Let's start with a very simple example, the first few examples are like control cases.

```sql
explain query plan
select * from A
inner join B on B.id = A.id;

QUERY PLAN
|--SCAN TABLE A
\--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
```

OK as we can see `A` is not constrained so it has to be scanned but `B` isn't scanned,
we use its primary key for the join.  This is the most common kind of join: a search
based on a key of the table you are joining to.

Let's make it a bit more realistic.

```sql
explain query plan
select * from A
inner join B on B.id = A.id
where A.id = 5;

QUERY PLAN
|--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
```

Now `A` is constrained by the `WHERE` clause so we can use its index and then use the `B` index.
So we get a nice economical join from `A` to `B` and no scans at all.

Now suppose we try this with some CTE replacements for `A` and `B`.  Does this make it worse?

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  BB(id, that) as (select * from B)
select * from AA
left join BB on BB.id = AA.id
where AA.id = 5;

QUERY PLAN
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
```

The answer is a resounding no.  The CTE `AA` was not materialized it was expanded in place,
as was the CTE `BB`.  We get *exactly* the same query plan.  Now this means that the
inner expressions like `select * from A` could have been fragments such as:

```sql
@attribute(cql:shared_fragment)
create proc A_()
begin
  select * from A;
end;

@attribute(cql:shared_fragment)
create proc B_()
begin
  select * from B;
end;

explain query plan
with
  (call A_()),    -- short for A_(*) AS (call A_())
  (call B_())     -- short for B_(*) AS (call B_())
select * from A_
left join B_ on B_.id = A_.id
where A_.id = 5;
```

*Note:* I'll use the convention that `A_` is the fragment proc that could have generated the CTE `AA`,
likewise with `B_` and so forth.

The above will expand into exactly what we had before and hence will have the exactly
same good query plan.  Of course this is totally goofy, why make a fragment like that --
it's just more typing.  Well now lets generalize the fragments just a bit.

```sql
@attribute(cql:shared_fragment)
create proc A_(experiment bool not null)
begin
  -- data source might come from somewhere else due to an experiment
  if not experiment then
    select * from A;
  else
    select id, this from somewhere_else;
  end if;
end;

@attribute(cql:shared_fragment)
create proc B_()
begin
  -- we don't actually refer to "B" if the filter is null
  if b_filter is not null then
    -- applies b_filter if specified
    select * from B where B.other like b_filter;
  else
    -- generates the correct shape but zero rows of it
    select null as id, null as that where false;
  end if;
end;

create proc getAB(
    id_ integer not null,
    experiment bool not null,
    b_filter text)
begin
  with
    (call A_(experiment)),
    (call B_(b_filter))
  select * from A_
  left join B_ on B_.id = A_.id
  where A_.id = id_;
end;
```

The above now has 4 combos economically encoded and all of them have a good plan.
Importantly though, if `b_filter` is not specified then we don't actually join to `B`.
The `B_` CTE will have no reference to `B`, it just has zero rows.

Now lets look at some things you don't want to do.

Consider this form:

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  BB(id, that) as (select A.id, B.that from A left join B on B.id = A.id)
select * from AA
left join BB on BB.id = AA.id
where AA.id = 5;

QUERY PLAN
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
```

Note that here we get 3 joins.  Now a pretty cool thing happened here -- even though the expression
for `BB` does not include a `WHERE` clause SQLite has figured out the `AA.id` being 5 forces `A.id` to be 5
which in turn gives a constraint on `BB`. Nice job SQLite.  If it hadn't been able to figure that out
then the expansion of `BB` would have resulted in a table scan.

Still, 3 joins is bad when we only need 2 joins to do the job.  What happened?  Well, when we did
the original fragments with extensions and stuff we saw this same pattern in fragment code.
Basically the fragment for `BB` isn't just doing the `B` things it's restarting from `A` and doing its
own join to get `B`. This results in a wasted join.  And it might result in a lot of work on the `A` table
as well if the filtering was more complex and couldn't be perfectly inferred.

You might think, "oh, no problem, I can save this, I'll just refer to `AA` instead of `A` in the second query."

This does not help (but it's going in the right direction):

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  BB(id, that) as (select AA.id, B.that from AA left join B on B.id = AA.id)
select * from AA
left join BB on BB.id = AA.id
where AA.id = 5;

QUERY PLAN
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
```

In terms of fragments the anti-pattern is this.

```sql
@attribute(cql:shared_fragment)
create proc B_()
begin
  select B.* from A left join B on B.id = A.id;
end;
```

The above starts the query for `B` again from the root.  You can save this, the trick is to not
try to generate just the `B` columns and then join them later.  You can get a nice data
flow going with chain of CTEs.

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  AB(id, this, that) as (select AA.*, B.that from AA left join B on B.id = AA.id)
select * from AB
where AB.id = 5;

QUERY PLAN
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
```

And we're right back to the perfect plan.  The good form creates a CTE chain
where we only need the result of the final CTE.  A straight line of CTEs
each depending on the previous one results in a excellent data flow.

In terms of fragments this is now:

```sql
@attribute(cql:shared_fragment)
create proc A_()
begin
  select * from A;
end;

@attribute(cql:shared_fragment)
create proc AB_()
begin
  with
  (call A_)
  select A_.*, B.that from A_ left join B on B.id = A_.id
end;

with (call AB_())
select * from AB_ where AB_.id = 5;
```

For brevity I didn't include the possibility of using IF and such.  Another option
that makes the same good query plan.  We can generalize `AB_` so that it doesn't know
where the base data is coming from and can be used in more cases.


```sql
@attribute(cql:shared_fragment)
create proc A_()
begin
  select * from A;
end;

@attribute(cql:shared_fragment)
create proc AB_()
begin
  with
  source(*) like A -- you must provide some source that is the same shape as A
  select source.*, B.that from source left join B on B.id = source.id
end;

with
(call A_())
(call AB_() using A_ as source)
select * from AB_ where AB_.id = 5;
```

Again this results in a nice straight chain of CTEs and even though the where
clause is last the A table is constrained properly.

It's important not to fork the chain... if you do that then whatever
came before the fork must be materialized for use in both branches.
That can be quite bad because then the filtering might come after the
materialization.  This is an example that is quite bad.

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  BB(id, that) as (select AA.id, B.that from AA left join B on B.id = AA.id),
  CC(id, other) as (select AA.id, C.other from AA left join C on C.id = AA.id)
select * from AA
left join BB on BB.id = AA.id
left join CC on CC.id = AA.id
where AA.id = 5;

QUERY PLAN
|--MATERIALIZE 2
|  |--SCAN TABLE A
|  \--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
|--MATERIALIZE 3
|  |--SCAN TABLE A
|  \--SEARCH TABLE C USING INTEGER PRIMARY KEY (rowid=?)
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
|--SCAN SUBQUERY 2
\--SEARCH SUBQUERY 3 USING AUTOMATIC COVERING INDEX (id=?)
```

Things have gone way of the rails here. As you can see `A` is now
scanned twice. and there are many more joins.  We could make this
a lot better by moving the A condition all the way up into the first
CTE.  With fragments that would just mean creating something like

```sql
@attribute(cql:shared_fragment)
create proc A_(id_)
begin
  select * from A where A.id = id_;
end;
```

At least then if we have to materialize we'll get only one row.  This could be
a good thing to do universally, but it's especially important if you know that
forking in the query shape is mandatory for some reason.

A better pattern might be this:

```sql
explain query plan
with
  AA(id, this) as (select * from A),
  AB(id, this, that) as (select AA.*, B.that from AA left join B on B.id = AA.id),
  ABC(id, this, that, other) as (select AB.*, C.other from AB left join C on C.id = AB.id)
select * from ABC
where ABC.id = 5;

QUERY PLAN
|--SEARCH TABLE A USING INTEGER PRIMARY KEY (rowid=?)
|--SEARCH TABLE B USING INTEGER PRIMARY KEY (rowid=?)
\--SEARCH TABLE C USING INTEGER PRIMARY KEY (rowid=?)
```

Here we've just extended the chain.  With shared fragments you could easily build an
`AB_` proc as before and then build an `ABC_` proc either by calling `AB_` directly or by
having a table parameter that is `LIKE AB_`.

Both cases will give you a great plan.

So the most important things are:

* Avoid forking the chain of CTEs/fragments, a straight chain works great.
* Avoid re-joining to tables, even unconstrained CTEs result in great plans if they don't have to be materialized.
* If you do need to fork in your CTE chain, because of your desired shape, be sure to move as many filters as you can further upstream so that by the time you materialize only a very small number of rows need to be materialiized.

These few rules will go far in helping you to create shapes.

One last thing, without shared fragments, if you wanted to create a large 10 way join or something you had to type that
join into your file and it would be very much in your face.  Now that join might be hidden from you in a nice easy-to-use
fragment.  Which you might then decide you want to use 3 times... And now with a tiny amount of code you have 30 joins.

The thing is shared fragments make it easy to generate a lot of SQL.  It's not bad that shared fragments make things easy,
but with great power comes great responsibility, so give a care as to what it is you are assembling.  Understanding
your fragments, especially any big ones, will help you to create great code.



## Appendix 9: Using the CQL Amalgam
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

This is a brief discussion of the CQL Amalgam and its normal usage patterns.

### Building the Amalgam

The amalgam has to include the results of bison and flex, so a normal build must run first.  The simplest
way to build it starting from the `sources` directory is:

```bash
make
./make_amalgam.sh
```

The result goes in `out/cql_amalgam.c`.  It can then be built using `cc` with whatever flags you might
desire.  With a few `-D` directives it can readily be compiled with Microsoft C and it also works with
Emscripten (`emcc`) basically unchanged.  Clang and Gcc of course also work.

The standard test script `test.sh` builds the amalgam and attempts to compile it as well, which ensures
that the amalgam can at least compile at all times.

### Testing the Amalgam

Of course you can do whatever tests you might like by simply compiling the amalgam as is and then using
it to compile things.  But importantly the test script `test.sh` can test the amalgam build like so:

```bash
test.sh --use_amalgam
```

This runs all the normal tests using the binary built from the amalgam rather than the normal binary.

Normal CQL development practices result in this happening pretty often so the amalgam tends to stay
in good shape. The code largely works in either form with very few affordances for the amalgam build needed.
Most developers don't even think about the amalgam build flavor; to a first approximation "it just works".

### Using the Amalgam

To use the amalgam you'll want to do something like this:

```c
#define CQL_IS_NOT_MAIN 1

// Suppresses a bunch of warnings because the code
// is in an #include context
// PR's to remove these are welcome :D
#pragma clang diagnostic ignored "-Wnullability-completeness"

#include "cql_amalgam.c"

void go_for_it(const char *your_buffer) {
  YY_BUFFER_STATE my_string_buffer = yy_scan_string(your_buffer);

  // Note: "--in" is irrelevant because the scanner is
  // going to read from the buffer above.
  //
  // If you don't use yy_scan_string, you could use "--in"
  // to get data from a file.

  int argc = 4;
  char *argv[] = { "cql", "--cg", "foo.h", "foo.c" };

  cql_main(argc, argv);
  yy_delete_buffer(my_string_buffer);
}
```

So the general pattern is:

* predefine the options you want to use (see below)
* include the amalgam
* add any functions you want that will call the amalgam

Most amalgam functions are `static` to avoid name conflicts. You will want to create your own public functions such as `go_for_it` above that use the amalgam in all the ways you desire.

You'll want to avoid calling any internal functions other than `cql_main` because they are liable to change.

NOTE: The amalgam is C code not C++ code.  Do not attempt to use it inside of an `extern "C"` block in a C++ file.  It won't build.  If you want a C++ API, expose the C functions you need and write a wrapper class.

### CQL Amalgam Options

The amalgam includes the following useful `#ifdef` options to allow you to customize it.

* CQL_IS_NOT_MAIN
* CQL_NO_SYSTEM_HEADERS
* CQL_NO_DIAGNOSTIC_BLOCK
* cql_emit_error
* cql_emit_output
* cql_open_file_for_write
* cql_write_file

#### CQL_IS_NOT_MAIN

If this symbol is defined then `cql_main` will not be redefined to be `main`.

As the comments in the source say:

```c
#ifndef CQL_IS_NOT_MAIN

// Normally CQL is the main entry point.  If you are using CQL
// in an embedded fashion then you want to invoke its main at
// some other time. If you define CQL_IS_NOT_MAIN then cql_main
// is not renamed to main.  You call cql_main when you want.

  #define cql_main main
#endif
```

Set this symbol so that you own main and cql_main is called at your pleasure.

#### CQL_NO_SYSTEM_HEADERS

The amalgam includes the normal `#include` directives needed to make it compile, things like stdio and such.
In your situation these headers may not be appropriate.  If `CQL_NO_SYSTEM_HEADERS` is defined then the amalgam
will not include anything; you can then add whatever headers you need before you include the amalgam.


#### CQL_NO_DIAGNOSTIC_BLOCK

The amalgam includes a set of recommended directives for warnings to suppress and include.  If you want
to make other choices for these you can suppress the defaults by defining `CQL_NO_DIAGNOSTIC_BLOCK`;
you can then add whatever diagnostic pragmas you want/need.

#### cql_emit_error

The amalgam uses `cql_emit_error` to write its messages to stderr.  The documentation is included in the
code which is attached here.  If you want the error messages to go somewhere else, define `cql_emit_error`
as the name of your error handling function.  It should accept a `const char *` and record that string
however you deem appropriate.

```c
#ifndef cql_emit_error

// CQL "stderr" outputs are emitted with this API.
//
// You can define it to be a method of your choice with
// "#define cql_emit_error your_method" and then your method
// will get the data instead. This will be whatever output the
// compiler would have emitted to stderr.  This includes
// semantic errors or invalid argument combinations.  Note that
// CQL never emits error fragments with this API; you always
// get all the text of one error.  This is important if you
// are filtering or looking for particular errors in a test
// harness or some such.
//
// You must copy the memory if you intend to keep it. "data" will
// be freed.
//
// Note: you may use cql_cleanup_and_exit to force a failure from
// within this API but doing so might result in unexpected cleanup
// paths that have not been tested.

void cql_emit_error(const char *err) {
  fprintf(stderr, "%s", err);
  if (error_capture) {
    bprintf(error_capture, "%s", err);
  }
}

#endif
```

Typically you would `#define cql_emit_error your_error_function` before you include the amalgam and then
define your_error_function elsewhere in that file (before or after the amalgam is included are both fine).

#### cql_emit_output

The amalgam uses `cql_emit_output` to write its messages to stdout.  The documentation is included in the
code which is attached here.  If you want the standard output to go somewhere else, define `cql_emit_output`
as the name of your output handling function.  It should accept a `const char *` and record that string
however you deem appropriate.

```c
#ifndef cql_emit_output

// CQL "stdout" outputs are emitted (in arbitrarily small pieces)
// with this API.
//
// You can define it to be a method of your choice with
// "#define cql_emit_output your_method" and then your method will
// get the data instead. This will be whatever output the
// compiler would have emitted to stdout.  This is usually
// reformated CQL or semantic trees and such -- not the normal
// compiler output.
//
// You must copy the memory if you intend to keep it. "data" will
// be freed.
//
// Note: you may use cql_cleanup_and_exit to force a failure from
// within this API but doing so might result in unexpected cleanup
// paths that have not been tested.

void cql_emit_output(const char *msg) {
  printf("%s", msg);
}

#endif
```

Typically you would `#define cql_emit_output your_output_function` before you include the amalgam and then
define your_error_function elsewhere in that file (before or after the amalgam is included are both fine).

#### cql_open_file_for_write

If you still want normal file i/o for your output but you simply want to control the placement of the output
(such as forcing it to be on some virtual drive) you can replace this function by defining `cql_open_file_for_write`.

If all you need to do is control the origin of the `FILE *` that is written to, you can replace just this function.

```c
#ifndef cql_open_file_for_write

// Not a normal integration point, the normal thing to do is
// replace cql_write_file but if all you need to do is adjust
// the path or something like that you could replace
// this method instead.  This presumes that a FILE * is still ok
// for your scenario.

FILE *_Nonnull cql_open_file_for_write(
  const char *_Nonnull file_name)
{
  FILE *file;
  if (!(file = fopen(file_name, "w"))) {
    cql_error("unable to open %s for write\n", file_name);
    cql_cleanup_and_exit(1);
  }
  return file;
}

#endif
```

Typically you would `#define cql_open_file_for_write your_open_function` before you include the amalgam and then
define your_open_function elsewhere in that file (before or after the amalgam is included are both fine).

#### cql_write_file

The amalgam uses `cql_write_file` to write its compilation outputs to the file system.  The documentation is included in the
code which is attached here.  If you want the compilation output to go somewhere else, define `cql_write_file`
as the name of your output handling function.  It should accept a `const char *` for the file name and another
for the data to be written.  You can then store those compilation results however you deem appropriate.

```c
#ifndef cql_write_file

// CQL code generation outputs are emitted in one "gulp" with this
// API. You can define it to be a method of your choice with
// "#define cql_write_file your_method" and then your method will
// get the filename and the data. This will be whatever output the
// compiler would have emitted to one of it's --cg arguments.
// You can then write it to a location of your choice.
// You must copy the memory if you intend to keep it. "data" will
// be freed.

// Note: you *may* use cql_cleanup_and_exit to force a failure
// from within this API.  That's a normal failure mode that is
// well-tested.

void cql_write_file(
  const char *_Nonnull file_name,
  const char *_Nonnull data)
{
  FILE *file = cql_open_file_for_write(file_name);
  fprintf(file, "%s", data);
  fclose(file);
}

#endif
```

Typically you would `#define cql_write_file your_write_function` before you include the amalgam and then
define your_write_function elsewhere in that file (before or after the amalgam is included are both fine).

### Amalgam LEAN choices

When you include the amalgam, you get everything by default. You may, however, only want some
limited subset of the compiler's functions in your build.

To customize the amalgam, there are a set of configuration pre-processor options.  To opt-in to
configuration, first define `CQL_AMALGAM_LEAN`. You then have to opt-in to the various pieces
you might want. The system is useless without the parser, so you can't remove that; but you can
choose from the list below.

The options are:
* CQL_AMALGAM_LEAN` : enable lean mode; this must be set or you get everything
* `CQL_AMALGAM_CG_C` : C codegen
* `CQL_AMALGAM_CG_COMMON` : common code generator pieces
* `CQL_AMALGAM_GEN_SQL` : the echoing features
* `CQL_AMALGAM_JSON` : JSON schema output
* `CQL_AMALGAM_OBJC` : Objective-C code gen
* `CQL_AMALGAM_QUERY_PLAN` : the query plan creator
* `CQL_AMALGAM_SCHEMA` : the assorted schema output types
* `CQL_AMALGAM_SEM` : semantic analysis (needed by most things)
* `CQL_AMALGAM_TEST_HELPERS` : test helper output
* `CQL_AMALGAM_UDF` : the UDF stubs used by the query plan output
* `CQL_AMALGAM_UNIT_TESTS` : some internal unit tests, which are pretty much needed by nobody

Note that `CQL_AMALGAM_SEM` is necessary for any of the code generation
features to work. Likewise, several generators require `CQL_AMALGAM_CG_COMMON` (e.g., C does).

Pick what you want; stubs are created for what you omit to avoid linkage errors.

### Other Notes

The amalgam will use malloc/calloc for its allocations and it is designed to release all memory it
has allocated when cql_main returns control to you, even in the face of error.

Internal compilation errors result in an `assert` failure leading to an abort.  This is not supposed
to ever happen but there can always be bugs.  Normal errors just prevent later phases of the compiler
from running so you might not see file output, but rather just error output.  In all cases things
should be cleaned up.

The compiler can be called repeatedly with no troubles; it re-initializes on each use. The compiler is
 not multi-threaded so if there is threading you should use some mutex arrangement to keep it safe.
A thread-safe version would require extensive modifications.



## Appendix 10: CQL Working Example
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

This is a working example that shows all of the basic DML statements and the call patterns
to access them. The code also includes the various helpers you can use to convert C types to
CQL types.

#### `todo.sql`

```SQL
-- This is a simple schema for keeping track of tasks and whether or not they have been completed

-- this serves to both declare the table and create the schema
create proc todo_create_tables()
begin
  create table if not exists tasks(
    description text not null,
    done bool default false not null
  );
end;

-- adds a new not-done task
create proc todo_add(task TEXT NOT null)
begin
  insert into tasks values(task, false);
end;

-- gets the tasks in inserted order
create proc todo_tasks()
begin
  select rowid, description, done from tasks order by rowid;
end;

-- updates a given task by rowid
create proc todo_setdone_(rowid_ integer not null, done_ bool not null)
begin
  update tasks set done = done_ where rowid == rowid_;
end;

-- deletes a given task by rowid
create proc todo_delete(rowid_ integer not null)
begin
  delete from tasks where rowid == rowid_;
end;
```

#### `main.c`

```c
#include <stdlib.h>
#include <sqlite3.h>

#include "todo.h"

int main(int argc, char **argv)
{
  /* Note: not exactly world class error handling but that isn't the point */

  // create a db
  sqlite3 *db;
  int rc = sqlite3_open(":memory:", &db);
  if (rc != SQLITE_OK) {
    exit(1);
  }

  // make schema if needed (in memory databases always begin empty)
  rc = todo_create_tables(db);
   if (rc != SQLITE_OK) {
    exit(2);
  }

  // add some tasks
  const char * const default_tasks[] = {
    "Buy milk",
    "Walk dog",
    "Write code"
  };

  for (int i = 0; i < 3; i++) {
    // note we make a string reference from a c string here
    cql_string_ref dtask = cql_string_ref_new(default_tasks[i]);
    rc = todo_add(db, dtask);
    cql_string_release(dtask); // and then dispose of the reference
    if (rc != SQLITE_OK) {
      exit(3);
    }
  }

  // mark a task as done
  rc = todo_setdone_(db, 1, true);
  if (rc != SQLITE_OK) {
    exit(4);
  }

  // delete a row in the middle, rowid = 2
  rc = todo_delete(db, 2);
  if (rc != SQLITE_OK) {
    exit(5);
  }

  // select out some results
  todo_tasks_result_set_ref result_set;
  rc = todo_tasks_fetch_results(db, &result_set);
  if (rc != SQLITE_OK) {
    printf("error: %d\n", rc);
    exit(6);
  }

  // get result count
  cql_int32 result_count = todo_tasks_result_count(result_set);

  // loop to print
  for (cql_int32 row = 0; row < result_count; row++) {
    // note "get" semantics mean that a ref count is not added
    // if you want to keep the string you must "retain" it
    cql_string_ref text = todo_tasks_get_description(result_set, row);
    cql_bool done = todo_tasks_get_done(result_set, row);
    cql_int32 rowid = todo_tasks_get_rowid(result_set, row);

    // convert to c string format
    cql_alloc_cstr(ctext, text);
    printf("%d: rowid:%d %s (%s)\n",
      row, rowid, ctext, done ? "done" : "not done");
    cql_free_cstr(ctext, text);
  }

  // done with results, free the lot
  cql_result_set_release(result_set);

  // and close the database
  sqlite3_close(db);
}
```

### Build Steps

```sh
# ${cgsql} refers to the root of the CG/SQL repo
% cql --in todo.sql --cg todo.h todo.c
% cc -o todo -I${cqsql}/sources main.c todo.c ${cgsql}/sources/cqlrt.c -lsqlite3
```

### Results

Note that rowid 2 has been deleted, the leading number is the index in
the result set. The rowid is of course the database rowid.

```
% ./todo
0: rowid:1 Buy milk (done)
1: rowid:3 Write code (not done)
```



## Appendix 11: Production Considerations
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

### Production Considerations

This system as it appears in the sources here is designed to get some basic SQLite scenarios working but
the runtime systems that are packaged here are basic, if only for clarity.  There are some important
things you should think about improving or customizing for your production environment. Here's a brief list.


#### Concurrency

The reference counting solution in the stock `CQLRT` implementation is single threaded.  This might be ok,
in many environments only one thread is doing all the data access.  But if you plan to share objects
between threads this is something you'll want to address.  `CQLRT` is designed to be replacable.  In fact
there is another version included in the distribution `cqlrt_cf` that is more friendly to iOS and CoreFoundation.
This alternate version is an excellent demonstration of what is possible.  There are more details
in [Internals Part 5: CQL Runtime](https://cgsql.dev/cql-guide/int05).

#### Statement Caching

SQLite statement management includes the ability to reset and re-prepare statements.  This is an
important performance optimization but the stock `CQLRT` does not take advantage of this.  This is
for two reasons:  first, simplicity, and secondly (though more importantly), any kind of statement cache would require
a caching policy and this simple `CQLRT` cannot possibly know what might consitute a good policy
for your application.

The following three macros can be defined in your `cqlrt.h` and they can be directed at a version that
keeps a cache of your choice.

```c
#ifndef cql_sqlite3_exec
#define cql_sqlite3_exec(db, sql) sqlite3_exec((db), (sql), NULL, NULL, NULL)
#endif

#ifndef cql_sqlite3_prepare_v2
#define cql_sqlite3_prepare_v2(db, sql, len, stmt, tail) sqlite3_prepare_v2((db), (sql), (len), (stmt), (tail))
#endif

#ifndef cql_sqlite3_finalize
#define cql_sqlite3_finalize(stmt) sqlite3_finalize((stmt))
#endif
```
As you might expect, `prepare` creates a statement or else returns one from the cache.
When the `finalize` API is called the indicated statement can be returned to the cache or discarded.
The `exec` API does both of these operations, but also, recall that `exec` can get a semicolon
separated list of statements. Your `exec` implementation will have to use SQLite's prepare functions
to split the list and get prepared statements for part of the string.  Alternately, you could choose
not to cache in the `exec` case.

#### Your Underlying Runtime

As you can see in `cqlrt_cf`, there is considerable ability to define what the basic data types mean.  Importantly,
the reference types `text`, `blob`, and `object` can become something different (e.g., something
already supported by your environment).  For instance, on Windows you could use COM or .NET types
for your objects.  All object references are substantially opaque to `CQLRT`; they have comparatively
few APIs that are defined in the runtime:  things like getting the text out of the string reference
and so forth.

In addition to the basic types and operations you can also define a few helper functions that
allow you to create some more complex object types.  For instance, list, set, and dictionary
creation and management functions can be readily created and then you can declare them using
the `DECLARE FUNCTION` language features.  These objects will then be whatever list, set, or
dictionary they need to be in order to interoperate with the rest of your environment.  You can
define all the data types you might need in your `CQLRT` and you can employ whatever
threading model and locking primitives you need for correctness.

#### Debugging and Tracing

The `CQLRT` interface includes some helper macros for logging.  These are defined
as no-ops by default but, of course, they can be changed.

```
#define cql_contract assert
#define cql_invariant assert
#define cql_tripwire assert
#define cql_log_database_error(...)
#define cql_error_trace()
```

`cql_contract` and `cql_invariant` are for fatal errors. They both assert something
that is expected to always be true (like `assert`) with the only difference being that
the former is conventionally used to validate preconditions of functions.

`cql_tripwire` is a slightly softer form of assert that should crash in debug
builds but only log an error in production builds. It is generally used to enforce
a new condition that may not always hold with the goal of eventually transitioning
over to `cql_contract` or `cql_invariant` once logging has demonstrated that the
tripwire is never hit.
When a `fetch_results` method is called, a failure results in a call to `cql_log_database_error`.
Presently the log format is very simple.  The invocation looks like this:

```c
 cql_log_database_error(info->db, "cql", "database error");
```
The logging facility is expected to send the message to wherever is appropriate for your environment.
Additionally it will typically get the failing result code and error message from SQLite, however
these are likely to be stale. Failed queries usually still require cleanup and so the SQLite error
codes be lost because (e.g.) a `finalize` has happened, clearing the code. You can do better if,
for instance, your runtime caches the results of recent failed `prepare` calls. In any case,
what you log and where you log it is entirely up to you.

The `cql_error_trace` macro is described in [Internals Chapter 3](https://cgsql.dev/cql-guide/int03#cleanup-and-errors).
It will typically invoke `printf` or `fprintf` or something like that to trace the origin of thrown
exceptions and to get the error text from SQLite as soon as possible.

An example might be:

```
#define cql_error_trace() fprintf(stderr, "error %d in %s %s:%d\n", _rc_, _PROC_, __FILE__, __LINE_)
```
Typically the cost of all these diagnostics is too high to include in production code so this is
turned on when debugging failures.  But you can make that choice for yourself.

#### Customizing Code Generation

The file `rt_common.c` defines the common result types, but the skeleton file `rt.c`
includes affordances to add your own types without having to worry about conflicts with the
common types.  These macros define

```c
#define RT_EXTRAS
#define RT_EXTRA_CLEANUP
```

Simply define these two to create whatever `rt_` data structures you want and add any
cleanup function that might be needed to release resources.  The other cleanup
functions should provide a good template for you to make your own.

The C data type `rtdata` includes many text fragments that directly control the
code generation.  If you want to make your generated code look more like say
CoreFoundation you can define an `rtdata` that will do the job.  This will mean
a lot of your generated code won't require the `#defines` for the CQL types,
it can use your runtime directly.  You can also enable things like Pascal casing
for procedure names and a common prefix on procedure names if those are useful
in your environment.  However, the system is designed so that such changes
aren't necessary.  The data types in `cqlrt.h` are enough for any remapping,
additional changes with `rtdata` are merely cosmetic.

#### Summary

The `CQLRT` macros are very powerful, they allow you to target almost any
runtime with a C API.  The `cqlrt_cf` version is a good example of the
sorts of changes you can make.

Concurrency and Statement Caching are not supported in the basic version
for `cqlrt.h`.  If this is important to you you might want to customize for that.

Helper functions for additional data types can be added, and they can be
unique to your runtime.

There are tracing macros to help with debugability.  Providing some
useful versions of those can be of great help in production environments.




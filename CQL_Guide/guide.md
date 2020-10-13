<!--- @generated -->
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 1. Introduction

CQL was designed as a precompiled addition to the the SQLite runtime system.  SQLite lacks
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

NOTE: CQL was created to help solve problems in the building of Facebook's Messenger application, but this content is free from references to Messenger. The CQL code generation here is done in the simplest
mode with the fewest runtime dependencies allowed for illustration.

### Getting Started

The "Hello World" program rendered in CQL looks like this:

```sql
create proc hello()
begin
  call printf("Hello, world\n");
end;
```

This very nearly works exactly as written but we'll need a little bit of glue to wire it all up.
Let's talk about that glue.

First, to build this example we'll use cql in its simplest mode.  You may need to build the `cql` executable first.
From a source distribution you can run `make` in the `cql` directory to do the job.  You can get the binary from the `out` directory after the build.  Arrange for `cql` to be on your PATH.

With that done you should have the power to do this:

```sh
cql --in hello.sql --cg hello.h hello.c
```

This will produce the C output files `hello.c` and `hello.h` which can be readily compiled.

However, hello.c will not have a `main` -- rather it will have a function like this:

```C
void hello(void);
```

The declaration of this function can be found in `hello.h`.  

That `hello` function is not quite adequate to do get a running program, which brings us to the next step in
getting things running.  Typically you have some kind of client program that will execute the procedures you 
create in CQL.  Let's create a simple one in a file we'll creatively name `main.c`.

A very simple CQL main might look like this:

```C
#include <stdlib.h>
int main(int argc, char **argv)
{
   hello();
   return 0;
}
```

Now we should be able to get do the following:

```bash
$ cc -o hello main.c hello.c
$ ./hello
Hello, world
```

NOTE: hello.c will attempt to `#include "cqlrt.h"` the declarations for CQL runtime functions.
You must make arrangements for the compiler to be able to find `cqlrt.h` either by adding it to an
`INCLUDE` path or by adding some -I options to help the compiler find the source.  For now you could
keep `cqlrt.h` in the same directory as the examples and avoid that complication.

### Why did this work?

A number of things are going on even in this simple program that are worth discussing:

* the procedure `hello` had no arguments, and did not use the database
  * therefore its type signature when compiled will be simply `void hello(void);` so we know how to call it
  * you can see the declaration for yourself by examining the `hello.c` or `hello.h`
* since nobody used a database we didn't need to initialize one.
* since there are no actual uses of SQLite we didn't need to provide that library
* for the same reason we didn't need to include a reference to the CQL runtime
* the function `printf` was not declared, so attempting to `call` it creates a regular C call using whatever arguments are provided, in this case a string
* the `printf` function is declared in `stdio.h` which is pulled in by `cqlrt.h`, which appears in `hello.c`, so it will be available to call
* CQL allows string literal with double quotes, those literals may have most C escape sequences in them, so the "\n" bit works
  * Normal SQL string literals (also supported) use single quotes and do not allow, or need escape characters other than `''` to mean one single quote

All of these facts put together mean that the normal simple linkage rules result in an executable that prints
the string "Hello, world" and then a newline.

### Variables and Arithmetic

Borrowing once again from examples in "The C Programming Language", it's possible to do significant control flow in CQL without reference to databases.  The following program illustrates a variety of concepts:

```sql
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

You may notice that both the SQL style `--` line prefix comments and the C style `/* */` forms 
are acceptable comment forms. Indeed, it's actually quite normal to pass CQL source through the C pre-processor before giving
it to the CQL compiler, thereby gaining `#define` and `#include` as well as other pre-processing options
like token pasting in addition to the other comment forms.  More on this later.

Like C, in CQL all variables must be declared before they are used.  They remain in scope until the end of the
procedure in which they are declared, or they are global scoped if they are declared outside of any procedure.  The
declarations announce the names and types of the local variables.   Importantly, variables stay in scope for the whole
procedure even if they are declared within a nested `begin` and `end` block.

The most basic types are the scalar or "unitary" types (as they are referred to in the compiler)

|type      |aliases      | notes                              |
|----------|-------------|------------------------------------|
|integer   |int          | a 32 bit integer                   |
|long *    |long integer | a 64 bit integer                   |
|bool      |boolean      | an 8 bit integer, normalized to 0/1|
|real      |n/a          | a C double                         |
|text      |n/a          | an immutable string reference      |
|blob      |n/a          | an immutable blob reference        |
|object    |n/a          | an object reference                |
|object<T> |n/a          | an object reference of type T      |

\* SQLite makes no distinction between integer storage and long integer storage, but the declaration
tells CQL whether it should use the SQLite methods for binding and reading 64 bit or 32 bit quantities
from the column.

There will be more notes on these types later, but importantly, all keywords and names in CQL
are case insensitive just like in the underlying SQL language.   Additionally all of the 
above may be combined with `not null` to indicate that a `null` value may not be stored
in that variable (as in the example).  When generating the C code, the case used in the declaration
becomes the canonical case of the variable and all other cases are converted to that in the emitted
code.  As a result the C remains case sensitively correct.

The size of the reference types is machine dependent, whatever the local pointer size is.  The
non-reference types use machine independent declarations like `int32_t` to get exactly the desired
sizes in a portable fashion.

All reference types are initialized to `NULL` when they are declared.

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

Importantly, the CQL compiler uses the normal SQLite order of operations, which is NOT the C order of operatations.
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

CQL does not include its own pre-processor but it is designed to consume the output the C pre-processor.  To do this, you can either write the output of the pre-processor to a temporary file and read it into CQL as usual or you can set up a pipeline something like this:

```bash
cc -x c -E your_program.sql | cql --cg your_program.h your_program.c
```

The above causes the C compiler to invoke only the pre-processor `-E` and to treat the input as though it were C code `-x c` even though it is in a `.sql` file. Later examples will assume that you have configured CQL to be used with the C pre-processor as above.

<div style="page-break-after: always; visibility: hidden"></div>



<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

# 2. Using Data

The point of using CQL is to facilitate access to a SQLite database so we'll switch gears to a slightly more complicated setup.  We'll
still keep things fairly simple but let's start to use some database features.  Note: it is not the intent of this tutorial to also be
a primer for the SQLite programming language which is so ably documented on https://sqlite.org/.  Please refer to that site for details
on the meaning of the SQL statements used here if you are new to SQL.

### A Sample Program

Suppose we have the following program:

```sql
create table my_data(t text not null);

create proc hello()
begin
  insert into my_data(t)  values("Hello, world\n");
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

```C
#include <stdlib.h>
#include <sqlite3.h>

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

```C
  cql_code hello(sqlite3 *_db_);
```

indicating that the database is used and a SQLite return code is provided.  We're nearly there.  If you attempt
to build the program as before there will be several link-time errors due to missing functions.  Typically these
are resolved by providing the SQLite library to the command line and also adding the CQL runtime.  
The new command line looks something like this:

```bash
$ cc -o hello main.c hello.c cqlrt.c -lsqlite3
$ ./hello
Hello, world
```

The cql runtime can be anywhere you want it to be, and of course the usual C seperate compilation methods
can be applied. More on that later.

But actually, that program doesn't quite work yet.  If you run it, you'll get an error result code, not the message 
"Hello, world".

Let's talk about the final missing bit.

### Declaring Schema

In CQL a loose piece of Data Definition Language (henceforth DDL) does not actually create or drop anything.  
In most CQL programs,  the normal situation is that "something" has already created the database and put some 
data in it.  You need to tell the CQL compiler about the schema so that it knows what the tables are and what to 
expect to find in those tables.  This is because typically you're reconnecting to some sort of existing database. 
So, in CQL, loose DDL simply *declares* schema, it does not create it.  To create schema you have to put the DDL
into a procedure you can run.  If you do that, then the DDL still serves a declaration, but also the schema will be 
created when the procedure is executed.

We need to change our program a tiny bit.

```sql
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

```C
  int rc = sqlite3_open(":memory:", &db);
```

This statement gives us an empty private in-memory only database to work with.  This is the simplest case 
and it's still very useful.  The `sqlite_open` and `sqlite_open_v2` functions can be used to create a variety of 
databases per the SQLite documentation.

We'll need such a database to use our procedure, and we use it in the call here:

```C
  rc = hello(db);
```

This provides a valid db handle to our procedure.  Note that the procedure doesn't know what database it is 
supposed to operate on, it expects to be handed a suitable database on a silver platter.  In fact any given proc
could be used with various databases at various times.  Just like SQLite, CQL does not enforce any particular
database setup;  it does what you tell it to.

When `hello` runs we begin with

```sql
  create table my_data(t text not null);
```

This will create the `my_data` table with a single column `t`, of type `text not null`.  That will work because
we know we're going to be called with a fresh/empty database.  More typically you might do `create table if not exists ...` or otherwise have a general attach/create phase or something like that.  We'll dispense with that here.

Next we'll run the insert statement:

```sql
  insert into my_data(t) values("Hello, world\n");
```

This will add a single row to the table.  Note that we have again used double quotes, meaning this is a C string literal.  This is highly convenient given the escape sequences.  Normally SQLite text has the newlines directly embedded in it; that practice isn't very compiler friendly, hence the alternative.

Next we declare a local variable to hold our data.

```sql
  declare t text not null;
```

This is a simple string reference, it will be initialized to `NULL` by default.  That's actually important; 
even though the variable is `NOT NULL` there is no reasonable default value for it other than `NULL`.  
The `NOT NULL` declaration will guard against `NULL` assignments but it will not prevent reference types 
from beginning with `NULL` as their value.  Junk would be worse and some random initialized value
would create unnecessary cost.  This mirrors the choice the C language makes with its `_Nonnull` extensions.

At this point we can read back our data.

```sql
  set t := (select * from my_data);
```

This form of database reading has very limited usability but it does work for this case and it is illustrative.  
The presence of `(select ...)` indicates to the CQL compiler that parenthesized expression should be given to
SQLite for evaluation according to the SQLite rules.  The expression is statically checked at compile time to 
ensure that it has exactly one result column. In this case the `*` is just column `t`, and actually it would have
be clearer to use `t` directly here but then there wouldn't have a reason to talk about `*` and multiple columns.  
At run time, the `select` query must return exactly one row or an error code will be returned.  It's not uncommmon
to see `(select ... limit 1)` to force the issue.  But that still leaves the possibility of zero rows, which would be an error.  We'll talk about more flexible ways to read from the database later.

At this point it seems wise to bring up the unusual expression evaluation properties of CQL.  
CQL is by necessity a two-headed beast.  On the one side there is a rich expression evaluation language for 
working with local variables. Those expressions are compiled into C logic that emulates the behavior of SQLite 
on the data.  It provides complex expression constructs such `IN` and `CASE` but it is ultimately evaluated by C
execution.  Alternately, anything that is inside of a piece of SQL is necessarily evaluated by SQLite itself.  To make this clearer let's change the example a little bit before we move on.

```sql
  set t := (select "__"||t||' '||1.234 from my_data);
```

This is a somewhat silly example but it illustrates some important things:

* even though SQLite doesn't support double quotes that's no problem because CQL will convert the expression into single quotes with the correct escape values as a matter of course during compilation
* the `||` concatenation operator is evaluated by SQLite
* you can mix and match both kinds of string literals, they will be all be the single quote variety by the time SQLite sees them
* the `||` operator has lots of complex formatting conversions (such as converting real values to strings)
* in fact the conversions are so subtle as to be impossible to emulate in loose C code with any economy, so, like a few other operators, `||` is only supported in the SQLite context

Returning now to our code as written, we see something very familiar:

```sql
  call printf('%s', t);
```

Note we've used the single quote syntax here for no good reason other than illustration. There are no escape 
sequences here so either form would the job. Importantly, the string literal will not create a string object as before
but the text variable `t` is of course a string reference.  Before it can be used in a call to an un-declared function it 
must be converted into a temporary C string.  This might require allocation in general, that allocation is automatically
managed.  Note that by default CQL assumes that calls to unknown C functions should be emitted as written.  In this way you can use `printf` even though CQL knows nothing about it.

Lastly we have:

```sql
  drop table my_data;
```

This is not strictly necessary because the database is in memory anyway and the program is about to exit but there
it is for illustration.

Now the Data Manipulation Langauge (i.e. insert and select here; and henceforth DML) and the DDL might fail for various reasons.  If that happens the proc will `goto` a cleanup handler and return the failed return code instead of running the rest of the code.  Any temporary memory allocations will be freed and any pending 
SQLite statements will be finalized.  More on that later when we discuss error handling.

With that we have a much more complicated program that prints "Hello, world"

### Introducing Cursors

In order to read data with reasonable flexibility, we need a more powerful construction.  
Let's change our example again and start using some database features.

```sql
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
specified indicates that all the columns will be present, in order, this is more economical to type.  CQL will generate errors at compile time if there are any missing columns or if any of the values are not type compatible with the indicated column.

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

This loop will run until there are no results left (it might not run at all if there are zero rows, that is not an error).  The `FETCH` construct allows you to specify target variables, but if you do not do so, then a synethetic structure is
automatically created to capture the projection of the `select`.  In this case the columns are `pos` and `txt`.  
The automatically created storage exactly matches the type of the columns in the select list which could itself be tricky to calculate if the `select` is complex.  In this case the `select` is quite simple and the columns of the result directly match the schema for `my_data`.  An integer and a string reference.  Both not null.


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


If you compile and run this program  you'll get this output.

```bash
$ cc -x c -E hello.sql | cql --cg hello.h hello.c
$ cc -o hello main.c hello.c cqlrt.c -lsqlite3
$ ./hello
0: Hello
1: There
2: World
```

So the data was inserted and then sorted.

<div style="page-break-after: always; visibility: hidden"></div>

### Going Crazy

We've only scratched the surface of what SQLite can do and most DML constructs are supported by CQL.  
This includes common table expressions, and even recursive versions of the same. But remember, when it 
comes to DML, the CQL compiler only has to validate the types and figure out what the result shape will be -- 
SQLite always does all the heavy lifting of evaluation. All of this means with remarkably little additional code, 
the example below from the SQLite documentation can be turned into a CQL stored proc using the constructs
we have defined above.


```sql
-- declare the types of two useful sqlite builtins
declare select function rtrim(t text) text;
declare select function substr(t text, offs integer, len integer) text;

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
        where (x*x + y*y) < 4.0 and iter < 28
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

<div style="page-break-after: always; visibility: hidden"></div>

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
 * You can announce SQLite builtin functions other than the most standard ones, or SQLite user defined functions with `DECLARE SELECT FUNCTION` -- this creates a function whose type is known and is only useable inside of SQL evaluation contexts (just like the `||` operator)
   *  in general CQL doesn't know what these declared functions do, so it cannot emulate them
   *  some would be very hard to emulate correctly under the best of circumstances

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 3. Expressions, Literals, Nullability, Sensitivity

Until this point we've only discussed simple kinds of expressions and as well as variables and table columns marked with `NOT NULL` . These are indeed the easiest types for CQL to work with as they tend to correspond most directly to the types known to C.  However
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

CQL evaluation rules are designed to be as similar as possible but some variance is inevitable because evaluation is done in two fundamentally different way. 

### Order of Evaluation

```
ASSIGNMENT:    :=
LOGICAL_OR:    OR
LOGICAL_AND:   AND
EQUALITY:      = == != <> IS, IS NOT, IN, NOT IN, LIKE, GLOB, MATCH, REGEXP
INEQUALITY:    <   <=  >   >=
LOGICAL_NOT:   NOT
BINARY:        << >> & |
BETWEEN:       BETWEEN  NOT BETWEEN
ADDITION:      + -
MULIPLICATION: * / %
BINARY_NOT:    ~
CONCAT:        ||
```

NOTE: the above is NOT the C binding order (!!!)  The Sqlite binding order is used in the language and parens are added in the C output as needed to force that order.   CQL's rewriting emits minimal parens in all outputs.  Different parens are often needed for SQL output.

### Nullability

Just as in SQL the absence of `NOT NULL` implies that `NULL` is a legal value. Consider this example:

```sql
create table mixed_nulls(
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

ALL of `i1`, `i2`, `b1`, `b2`, `l1`, `l2`, `r1`, `r2`, `t1`, `t2`, and `bl1`, `bl2` are nullable. In some sense variables and columns declared nullable
(by virtual of the missing `NOT NULL`) are the root sources of nullability in the SQL language.  That and the `NULL` literal.  Though there are other sources
as we will see.

In the context of computing the types of expressions, CQL is statically typed and so it must make a decision about the type of any expression based on the type information at hand at compile time.  As a result it handles the static type of an expression conservatively.  If the result might be null then the expression is of a nullable type and the compiled code will include an affordance for the possibility of a null value at runtime.

The generated code for nullable types is considerably less efficient and so it should be avoided if that is reasonably possible.

### Types of Literals

There are a number of literal objects that may be expressed in CQL.  These are as follows:

#### String Literals

* A double quoted string is a C style string literal
  * the usual simple C escape sequences are supported, however 
  * the \xNN form is not supported, nor is the \0NNN octal form supported
    * these are actually invaluable and likely to be supported in the near future
* a single quoted string is a SQL style string literal
  * No escape sequences are supported other than `''` to indicate a single quote character
* The sequence @FILE("some_string") is a special string literal
  * the value of this literal is the path of the current compiland starting at the letters in `some_string`, or
  * the entire path of the current compiland if `some_string` does not occur in the path
  * the purpose of the `@FILE` construct is to provide a partial path to a file for diagnostics that is consistent even if the file is built in various different root paths on different build machines
  
#### Blob Literals
* SQLite Blob literals are supported in SQL contexts (i.e. where they will be processed by SQLite), CQL produces an error if you attempt to use a blob literal in a loose expression

#### Numeric Literals

* All numeric literals are considered to be positive; negative numbers are actually a positive literal combined with unary minus (the negation operator)
* Only base 10 literals are supported
* Literals with a decimal point are of type `REAL` and stored as the C type `double`
* Literals that can fit in a signed integer without loss, and do not end in the letter `L` are integer literals
* Larger literals, or those ending with the letter `L` are long integer literals.

Examples:

```
  1.3            -- real
  2L             -- long
  123456789123   -- long
  123            -- integer
```

#### The NULL literal
The use of `NULL` always gives a nullable result however this literal is special in that it has no storage class. `NULL` is not numeric, or string but rather mutates into
whatever it is first combined with.   For instance `NULL + 1` results in a nullable integer.  Because `NULL` has no primitive type in some cases where type knowledge
is required you might have to use the CAST() function to cast the NULL to a specific type such as `CAST(NULL as TEXT)`.   This construct guarantees type consistence in cases like `SELECT` from different sources combined with `UNION ALL`

Note:  constructs like `CAST(NULL as TEXT)` are always rewritten to just `NULL` before going to SQLite as the cast is uninteresting except for the type information which SQLite doesn't need/use anyway.

#### Other Considerations

There are no boolean literals other than the integers `0` and `1`.

The C pre-processor is often combined with CQL in which case the `_FILE_` and `_LINE_` directives may be used to create literals; they will be preprocessed into normal literals.  

The use of `_FILE_` can give surprising results in the presence of build systems, hence the existence of `@FILE(...)`.

### Nullability Rules

#### General Rule

Except as noted in the exceptions below, the result of an operator is nullable if and only if any of its operands 
are nullable. This applies no matter the number of operands the operator requires.

Note: CQL does not do constant folding or other inferencing, it only uses the types of the values

#### Identifiers and Literals

Nullable variables and columns are always nullable.  The `NULL` literal is nullable.  Other literals are not nullable.

#### IS and IS NOT

These operators always return a non-null boolean.

#### IN and NOT IN

In an expression like `needle IN (haystack)` the result is always a boolean. The boolean is nullable if and only if `needle` is nullable. The presence of nulls in the the haystack is irrelevant.

NOTE: SQLite has slightly different nullability rules for `IN` and `NOT IN` q.v.

#### CASE ..WHEN ..THEN.. ELSE.. END

The following rules apply when considering nullability of a `CASE` expression.

* if there is no `ELSE` clause, the result is nullable.
* if any of the output values (i.e. any `THEN` or `ELSE` values) are nullable, the result is nullable
* otherwise the result is not nullable

The SQL `CASE` construct is quite powerful and unlike the C `switch` statement it is actually an expression.  So it's rather more like a highly generalized ternary `a ? b : c` operator rather than the switch statement.   There can be
arbitrarily many conditions specified each with their own result and the conditions need not be constants and 
typically are not.

#### The IFNULL and COALESCE Functions

These functions are nullable or non-nullable based on their arguments. If the `IFNULL` or `COALESCE` call has
at least one non-nullable argument then the result is non-nullable.

#### LEFT and RIGHT OUTER JOINS
In most join operations the nullability of each column participating in the join is preserved.  However in a 
`LEFT OUTER` join the columns on the right side of the join are always considered nullable and in a 
`RIGHT OUTER` join the columns on the left side of the join are considered nullable.

NOTE: CQL does not use constraints in the `WHERE`, `ON`, or `HAVING` clause to infer non-null status even where this would be possible.  CQL does no data-flow at all.

### Expression Types

CQL supports a variety of expressions, nearly everything from the SQLite world.  The following are the various supported operators, they are presented in order from the weakest binding strength to the strongest.  
Note that the binding order is NOT the same as C, in some cases it is radically different (e.g. boolean math)

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

These are a ternary type operation.  The general forms are:

```sql
  expr1 BETWEEN expr2 AND expr3
  expr1 NOT BETWEEN expr2 AND expr3
```
Note that there is an inherent ambiguity in the language because `expr2` or `expr3` could be logical expressions that include `AND`. CQL resolves this ambiguity by insisting that `expr2` and `expr3` be "math expressions" in the grammar.

"Math expressions" consist of:

* bitwise `&` and `|`
* bitwise left shift or right shift (`<<` and `>>`)
* addition (`+`)
* subtraction (`-`)
* multiplication (`*`)
* division (`/`)
* modulus (`%`)
* unary negation (`-`)
* literals
* any parenthesized expression

Hence:

```sql
-- oh hell no (syntax error)
a between 1 and 2 and 3;

-- all ok
a between (1 and 2) and 3;
a between 1 and (2 and 3);
a between 1 + 2 and 12 / 2;
```
These considerations force the grammar to contain special rules for expressions after `BETWEEN`.

#### Logical NOT

The one operand of logical not must be a numeric.  `NOT 'x'` is illegal.


#### Non-ordering tests !=, <>, =, ==, LIKE, GLOB, MATCH, IN, NOT IN, IS, IS NOT

These operations do some non-ordered comparison of their two operands.

* `IS` and `IS NOT` never return `NULL`,  So for instance `X IS NOT NULL` gives the natural answer.  `x IS y` is true if and only if: 1. both `x` and `y` are `NULL` or 2. if they are equal.
* The other operators return `NULL` if either operand is `NULL` and otherwise perform their usual test to produce a boolean
* `!=` and `<>` are equivalent as are `=` and `==`
* strings and blobs compare equal based on their value, not their identity (i.e. not the string/blob pointer)
* objects compare equal based on their address, not their content (i.e. reference equality)
* `MATCH` and `GLOB` only valid in SQL contexts, `LIKE` can be used in any context (a helper method to do `LIKE` in C is provided by SQLite, but not the others)

```sql
 NULL IS NULL  -- this is true
(NULL == NULL) IS NULL  -- this is also true because NULL == NULL is not 1, it's NULL.
(NULL != NULL) IS NULL  -- this is also true because NULL != NULL is not 0, it's also NULL.
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
this is utterly unlike most systems.  Many parenthesis are likely to be needed to get the usual "or of ands" patterns codified correctly.  Likewise the shift operators `<<` and `>>` are the same strength as `&` and `|` which is very atypical. Consider:

```sql
x & 1 << 7;    -- probably doesn't mean what you think (this is not ambigous, it's well defined, but unlike C)
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

EXCEPTION: the `%` operator doesn't make sense on real values, so real values produces an error. 

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

In this form the expression in the case `x` here is evaluated exactly once and then compared against each `when` clause, they must be type compatible with the expression.  The `then` expression that corresponds is evaluated and becomes the result, or the `else` expression if present and no `when` matches.  If there is no else and no match the result is `null`.

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

The second form, where there is no value before the first `when` keyword, each `when` expression is a separate independent boolean expression.  The first one that evaluates to true causes the corresponding `then` to be evaluated and that becomes the result.  If there are no matches the result is the `else` expression, or `null` if there is no `else`.

The result types must be compatible and the best type to hold the answer is selected with the usual promotion rules.

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

So looking at that procedure we can see that it's reading sensitive data, the result will have some sensitive columns in it.

 * the "id" is not sensitive (at least not in this example)
 * sens + 1 is sensitive, math on a sensitive field leaves it sensitive
 * name is sensitive, it began that way and is unchanged
 * 'x' is just a string literal, it's not sensitive
 * -sens is sensitive, that's more math
 * and the between expression is also sensitive

Generally sensitivity is "radioactive" anything it touches becomes sensitive.  This is very important because even a simple looking boolean expression like `is_gay IS NOT NULL` must lead to a sensitive result or the whole process would be largely useless.  It has to be basically impossible to wash away sensitivity.

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

Likewise a sensitive expression in `LIMIT` or `OFFSET` will result in 100% sensitive columns as these can be used in a `WHERE`-ish way.  There is no reasonble defense against using `LIMIT` and testing for the presence or absence of a row as a way to wash away sensitivity so that is a weakness, but the rules that are present are likely to be very helpful.

```sql
-- join with ON
select T1.id from with_sensitive T1 inner join with_sensitive T2 on T1.sens = T2.sens

-- join with USING
select T1.id from with_sensitive T1 inner join with_sensitive T2 using(sens);
```

All of these expression and join propagations are designed to make it impossible to simply wash-away sensitivity with a little bit of math.

Now we come to enforcement, which boils down to what assignments or "assignment-like" operations we allow.

If we have these:

```
declare sens integer @sensitive;
declare not_sens integer;
```

We can use those as stand-ins for lots of expressions, but the essential calculus goes like this:

```
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

Now it's possible to write a procedure that accepts sensitive things and returns non-sensitive things.  This is fundamentally necessary because the proc must be able return (e.g.) a success code, or encrypted data, that is not sensitive.  However, if you write the procedure in CQL it, too, will have to follow the assignment rules and so cheating will be quite hard.  The idea here is to make it easy to do handle sensitive data well and make typical mistakes trigger errors.

With these rules  it's possible to compute the the type of procedure result sets and also to enforce IN/OUT parameters.  Since the signature of procedures is conveniently generated with --generate_exports good practices are fairly easy to follow and sensitivity checks flow well into your programs.

This is a brief summary of CQL semantics for reference types -- those types that are ref counted by the runtime.

The three reference types are:

* TEXT
* OBJECT
* BLOB

Each of these has their own macro for `retain` and `release` though all three actually turn into the exact same code in all the cu rrent CQL runtime implementations.  In all cases the object is expected to be promptly freed when the reference count falls to zero.

### Reference Semantics

#### Stored Procedure Arguments

* `in` and `inout` arguments are not retained on entry to a stored proc
* `out` arguments are assumed to contain garbage and are nulled without retaining on entry
* if your `out` argument doesn't have garbage in it, then it is up to you do `release` it before you make a call
* When calling a proc with an `out` argument CQL will `release` the argument variable before the call site, obeying its own contract

#### Local Variables

* assigning to a local variable `retains` the object, and then does a `release` on the previous object
* this order is important, all assignments are done in this way in case of aliasing (`release` first might accidentally free too soon)
* CQL calls `release` on all local variable when the method exits

#### Assigning to an `out` parameter or a global variable

* `out,`inout`parameters, and global variables work just like local variables except that CQL does not call`release` at the end of the procedure

### Function Return Values

Stored procedures do not return values, they only have `out` arguments and those are well defined as above.  Functions however are also supported and they can have either `get` or `create` semantics

#### Get Semantics

If you declare a function like so:

```
declare function Getter() object;
```

Then CQL assumes that the returned object should follow the normal rules above, retain/release will balance by the end of the procedure for locals and globals or `out` arguments could retain the object.

#### Create Semantics

If you declare a function like so:

```
declare function Getter() create text;
```

Then CQL assumes that the function created a new result which it is now responsible for releasing.  In short the returned object is assumed to arrive with a retain count of 1 already on it.  When CQL stores this return value it will:

* release the object that was present at the storage location (if any)
* copy the returned pointer without further retaining it this one time

As a result if you store the returned value in a local variable it will be released when the procedure exits (as usual) or if you instead store the result in a global or an out parameter the result will survive to be used later.

### Comparison

CQL tries to adhere to normal SQL comparison rules but with a C twist.

### `BLOB` and `OBJECT`

These types have no value based comparison, so there is no `<`, `>` and so forth.

The following table is useful.  Let's suppose there are exactly two objects 'X' and 'Y'

true expressions: `X = X`   `X <> Y` `Y = Y`   `Y <> X`

false expressions: `X = Y`  `X <> X` `Y = X`  `Y <> Y`

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

### Sample Code

#### Out Argument Semantics

```
DECLARE FUNCTION foo() OBJECT;

CREATE PROC foo_user (OUT baz OBJECT)
BEGIN
  SET baz := foo();
END;

void foo_user(cql_object_ref _Nullable *_Nonnull baz) {
  *(void **)baz = NULL; // set out arg to non-garbage
  cql_set_object_ref(baz, foo());
}
```

#### Function with Create Semantics

```
DECLARE FUNCTION foo() CREATE OBJECT;

CREATE PROCEDURE foo_user (INOUT baz OBJECT)
BEGIN
  DECLARE x OBJECT;
  SET x := foo();
  SET baz := foo();
END;

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

```
DECLARE FUNCTION foo() OBJECT;

CREATE PROCEDURE foo_user (INOUT baz OBJECT)
BEGIN
  DECLARE x OBJECT;
  SET x := foo();
  SET baz := foo();
END;

void foo_user(cql_object_ref _Nullable *_Nonnull baz) {
  cql_object_ref x = NULL;

  cql_set_object_ref(&x, foo());
  cql_set_object_ref(baz, foo());

cql_cleanup:
  cql_object_release(x);
}
```

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 4. Procedures, Functions, and Control Flow


All kinds of control flow happens in the context of some procedure, though we've already introduced examples of procedures let's 
now go over some of the additional aspects we have not yet illustrated.

### Out Parameters

Consider this procedure:

```sql
create procedure echo (in arg1 integer not null, out arg2 integer not null)
begin 
  set arg2 := arg1; 
end;
```

Here `arg2` has been declared `out`.  CQL out parameters are very similar to "by reference" arguments in other langauges and
indeed they compile into a simple pointer reference in the generated C code.  One notable difference is that, in CQL, `out` parameters
for reference types and nullable types are always set to NULL by default.  This is another way that an otherwise non-null reference
variable can end up with a null in it.

Looking at the one line in the body of this procedure:

```sql
  set arg2 := arg1; 
```

The input argument `arg1` is unconditionally stored in the output.  Note that the `in` keyword is entirely optional and does
nothing other than perhaps add some clarity.  CQL also supports `inout` arguments which are expected to contain non-garbage values on
entry.  If the procedure is called from CQL, the compiler will arrange for this to be true.

* `in` arguments contain a valid value
* `out` arguments are assumed to contain garbage and are aggressively cleared on entry
* `inout` arguments contain a valid value


These invariants are very important when considering how reference types are handled.

* `in` reference arguments are borrowed, CQL will not further retain unless they are stored elsewhere
* `out` reference arguments are assumed to be garbage, they are not released on entry, but instead set to NULL
* `inout` reference arguments are assumed valid at entry

If CQL changes an `out` or `inout` value it first releases the existing value and then retains the new value.
In all cases the caller will ultimately release any non-null out reference either because it was borrowed (`in`) or 
the caller now/still owns it (`inout` or `in`).  

Aggressively putting `NULL` into `out` argumetns normalizes pointer handling for all `out` types.


### Procedure Calls

The usual `call` syntax is used to invoke a procedure.  It returns no value but it can have any number of `out` arguments.

```
  declare scratch integer not null; 
  call echo(12, scratch); 
  scratch == 12; -- true
```

The let's go over the most essential bits of control flow.

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
create proc looper(x integer not null)
begin
  while 1
  begin
   set x := x - 1;
   if x < 0 then
     leave;
   else if x % 100 = 0 then
     continue;
   else if x % 10 = 0
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
Now here we've encoded our exit condition a bit strangely we might have done the equivalent job with a normal conditionn in the predicate
part of the `while` statement but for illustration anyway, when x becomes negative `leave` will cause us to exit the loop.  This is like 
`break` in C.

```sql
   else if x % 100 = 0 then
     continue;
```

This bit says that on every 100th iteration we go back to the start of the loop.  So the next bit will not run, which is the printing.

```sql
   else if x % 10 = 0
     call printf('%d\n', x);
   end if;
```

Finishing up the control flow, on every 10th iteration we print the value of the loop variable.


### The TRY, CATCH, and THROW Statements

This example illustrates catching an error from some DML, and recovering rather than letting the error cascade up.  
This is the common "upsert" pattern (insert or update)

```sql
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
      printf("Error!\n");
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
      printf("Error!\n");
      throw;
    end catch;
```

Here we print a diagnostic message and then use the `throw` keyword to rethrow the previous failure.  Throw will create a failure in 
the current block using the most recent result code from SQLite if it is an error, or else the general `SQLITE_ERROR` result code
if there is no such error.  In this case the failure code for the `update` statement will become the result code of the current procedure.

This leaves only the closing markers:

```sql
  end catch; 
end;
```

If control flow reaches the normal end of the procedure it will return `SQLITE_OK`.  

### Procedures as Functions: Motivation and Example


The calling convention for CQL stored procedures often (usually) requires that the procedure returns a result code from SQLite.  
This makes it impossible to write a procedure that returns a result like a function, the result position is already used for
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
    call fib(arg - 1,  result); 
    call fib(arg - 2,  t); 
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
* the procedure did not also be returning a result set using a `select` statement or `out` statement (more on these later)

If the procedure in question uses SQLite, or calls something that uses SQLite, then it might fail.  
If that happens the result code will propagate just like it would have with a the usual `call` form.  
Any failures can be caught with `try/catch` as usual.
This feature is really only syntatic sugar for the "awkward" form above, but it does allow for slightly better generated C code.


<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 5. Types of Cursors, OUT and OUT UNION, and FETCH flavors

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

When compiled, this will result in creating a SQLite statement object (type `sqlite_stmt*`)
and storing it in a variable called `C`.  This statement can then be used later in various ways.

Here's perhaps the simplest way to use a cursor:

```sql
declare x, y  integer;
fetch C into x, y;
```

This will have the effect of reading one row from the results of the query into
the local variables `x` and `y`.

These variables might then be used to create some output such as

```sql
/* note use of double quotes so that \n is legal */
call printf("x:%d y:%d\n", ifnull(x, 0), ifnull(y,0));
```

Or any other use.

More generally, there may or may not be a fetched value.  The cursor variable `C`
can be used by itself as a virtual boolean indicating the presence of a row.
So a more complete example might be

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

Here we read all the rows out and print them.

Now if the table `xy_table` had instead had dozens of columns those declarations
would be very verbose and error prone.  And frankly annoying, especially if
the table definition was changing over time.

To make this a little easier, there are also so-called 'automatic' cursors.  These
happen implicitly and include all the necessary storage to exactly match
the rows in their statement.  Using the automatic syntax for the above we might get

```sql
declare C cursor for select * from xy_table;
fetch C;
if (C) then
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

All the necessary local state is automatically created hence "automatic" cursor.
This pattern is generally preferred but the loose variables pattern is in
some sense more general.

In all the cases if the number or type of variables do not match the select statement,
semantic errors are produced.


### Value Cursors

The purpose of value cursors is to make it possible for a stored procedure to
work with structures as a unit rather than field by field.  SQL doesn't have
the notion of structure types but structures actually appear pretty directly
in many places:

* the columns of a table are a structure
* the projection of a `SELECT` statement is a structure
* other things directly derived from the above  (like the columns of a statement cursor)
are likewise structures

Let's first start by how you declare a value cursor.  It is by analogy to one of the structure types above.

So:

```sql
declare C cursor like xy_table;
declare C cursor like select 1 a, 'x' b;
declare C cursor like my_view;
declare C cursor like my_other_cursor;
declare C cursor like my_previously_declared_stored_proc;
```

Any of those forms define a valid set of columns.  Note that the `select` example in no way causes the query provided to run. Instead, the select statement is analyzed and the column names and types are computed.  The cursor get the same field names and types.  Nothing happens at run time.
The last example assumes that there is a stored procedure defined somewhere earlier in this translation unit and that procedure returns a result set. The cursor declaration makes a cursor that could receive the result of that procedure.  We'll cover
that particular case in more detail below when we deal with the `OUT` statement.

Now once we have declared the cursor we can load it with values using `fetch` in the value form.

You can load up a cursor from values.  The values must be type-compatible of course.

```sql
fetch C from values(1,2);
```

You can call a procedure that returns a single row:

```sql
fetch C from call my_previously_declared_stored_proc();
```

You can fetch a cursor from another cursor:
```sql
fetch C from D;
```

In this case D must be an 'automatic' cursor but it could be coming from a statement.
This lets you copy a row and save it for later.  E.g. you could copy the current max-valued
row into a value cursor and use it after the loop.

```sql
declare C cursor for select id, value, <other_stuff> from <somewhere> where <conditions>;
declare D cursor like C;

fetch D from values (-1, -999);

loop fetch C
begin
  if (D.max < C.max) then
    fetch D from C;
  end if;
end;

-- this could print <other stuff> too
call printf("id:%d value:%d", D.id, D.value);
```

Value cursors are always 'automatic' -- they have their own storage.

Value cursors also may or may not be holding a row.

```sql
declare C like xy_table;
if not C then
  call printf("this will always be true because it starts empty\n");
end if;
```

When you call a procedure you may or may not get a row as we'll see below.

### OUT Statement

Value cursors were initially designed to create a convenient way for
a procedure to return a single row from a complex query
without having a crazy number of `OUT` parameters.  It's easiest
to illustrate this with an example.

The older verbose pattern looks like this:
```sql
create proc get_a_row(id_ integer not null,
                      out got_row bool not null,
                      out w integer not null,
                      out x integer,
                      out y text not null,
                      out z real)
begin
  declare C for select w, x, y, z from somewhere where id = id_;
  fetch C into w, x, y, z;
  set got_row := C;
end;
```

Now you can imagine this gets very annoying if `get_a_row` has to produce
a couple dozen column values.  And of course you have to get the types
exactly right.  And they might evolve over time.  Joy.

On the receiving side you get to do something just as annoying:

```sql
declare w integer not null
declare x integer;
declare y text;
declare z real;
declare got_row bool not null;
call get_a_row(id, got_row, w, x, y, z);
```

Using the `out` statement we get the equivalent functionality with a much simplified pattern. It looks like this:
```sql
create proc get_a_row(id_ integer not null)
begin
   declare C for select a, b, c, d from somewhere where id = id_;
   fetch C;
   out C;
end;
```

To use it you simply do this:
```sql
declare C like get_a_row;
fetch C from call get_a_row(id);
```

In fact originally the above was the only way to load a value cursor, before
the calculus was generalized. The original form still works, and does both
things in one step:
```sql
declare C fetch from call get_a_row(id);
```

The `OUT` statement lets you return a single row economically and
lets you then test if there actually was a row and read the columns.
It infers all the various column names and types so it is resilient
to schema change and generally a lot less error prone than having a
large number of `out` arguments to your procedure.

Once you have the result in a value cursor you can do the usual
cursor operations to move it around or otherwise work with it.

The use of the `LIKE` keyword to refer to the types of complex entities spread to other
places in CQL as a very useful construct, but it began here with the
need to describe a cursor shape economically, by reference.

### OUT UNION Statement
The semantics of the `out` statement are that it always producesone row
of output (a procedure can produce no row if an `out` never actually ran but the procedure does use `out`).
If an `out` statement runs more than once the most recent row becomes the result.  So the `out` statement really does
mirror having one `out` variable for each column.  This was its intent and procedures
that return at most, or exactly, one row are very common.  However, in general, one row results
do not suffice; you might want to produce a result set from various sources
with maybe arbitary compution in there as well.  For that you need to be able to emit multiple
rows from a computed source.  This is exactly what `out union` provides.

Here's a (somewhat contrived) example of the kind of thing you can do with this form:

```sql
create proc foo(n integer not null)
begin
  declare C cursor like select 1 value;
  declare i integer not null;
  set i := 0;
  while (i < n)
  begin
     -- emit one row for every integer
     fetch C from values(i);
     out union C;
  end;
end;
```

In `foo` above, we make an entire result set out of thin air.  It isn't very
interesting but of course any computation would have been possible.

This pattern is very flexibe as we see below in `bar` where
we're going to merge two different data streams.

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
  while (C or D)
  begin
    -- if both have a row pick the smaller id
    if (C and D) then
       if (C.id < D.id) then
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

Just like `foo`, in `bar`, each time `out union` runs a new row is accumulated.  Now, if you build
a procedure that ends with a `select` statement CQL automatically creates a fetcher function
that does exactly the same thing -- it loops over the SQLite statement for the select and fetches
each row, materializing a result.  With `out union` you take manual control of this process, allowing you
to build arbitrary result sets.  Note that either of `C` or `D` above could have been modified, replaced, skipped,
normalized, etc. with any kind of computation.  Even entirely synthetic rows can be computed
and inserted into the output as we saw in `foo`.


### Result Set Cursors

So `OUT UNION` makes it possible to create arbitrary result sets using a mix of sources and filtering.  Unfortunately this result type is not a simple row, nor is it a SQLite statement and so while it can produce ordinary result sets for CQL callers, CQL could not itself consume that result type.

To address this hole, and thereby make it a lot easier to test these result sets (which really is the most interesting use case for re-consuming a a result set) we need an additional cursor type.  The syntax is exactly the same as the statement cursor cases described above but, instead of holding a SQLite statement, the cursor holds a result set pointer and the current/max row numbers.  Stepping through it simply increments the row number and fetches the next row out of the rowset instead of from SQLite.

Example:
```sql
-- reading the above
create proc reader()
begin
  declare C cursor for call bar();
  loop fetch C
  begin
    call printf("%d %s\n", C.id, C.stuff);  -- or whatever
  end;
end;
```

If `bar` had been created with a `select union` and `order by` to merge the results, the above would have worked with `C` being a standard statement cursor, iterating over the union.   Since `foo` produces a result set, CQL transparently produces a suitable cursor implementation behind the scenes but otherwise the usage is the same.

Note this is a lousy way to iterate over rows; you have to materialize the entire result set so that you can just step over it.  Re-consuming like this is not recommended at all for production code but it is ideal for testing result sets that were made with `out union` which otherwise would require C/C++ to test.  Testing CQL with CQL is generally a lot easier.

### Reshaping Data, Cursor `LIKE` forms

There are lots of cases where you have big rows with many columns and there are various manipulations you need to do.  Some of these choices are emitting extra, related, rows, some of them are altering some of the columns before emitting the rows into the result set for use by some client.

What follows is a set of useful syntactic sugar constructs that simplify handling complex rows.  The idea is that pretty much anywhere you can specify a list of columns you can instead use the `LIKE x` construct to get the columns as the appear in object `x` -- which is usually a cursor.  It’s a lot easier to illustrate with examples, even though these are, again, a bit contrived.

First we need some table with lots of columns usually the column names are much bigger which makes it all the more important to not have to type them over and over.

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

We're going to emit two rows as the result of this proc.  Easy enough...

```sql
create proc foo(id_ integer not null)
begin
  -- this is the shape of the result we want, it's some of the columns of "big"
  -- note this query doesn't run, we just use it's shape to create a cursor
  -- with those columns.
  declare result cursor like select id, b, c, d from big;

  -- fetch the main row, specified by id_
  declare main_row cursor for select * from big where id = id_;
  fetch main_row;

  -- now fetch the result columns out of the main row
  -- like result means "the column names found in 'result'"
  fetch result from cursor main_row(like result);

  -- this is our first result row
  out union result;

  -- now we want the related row, but we only need 2 columns
  declare alt_row cursor for select b, c from big where big.id2 = main_row.id2;
  fetch alt_row;

  -- update some of the fields of the result from the alt result
  update cursor result(like alt_row) from cursor alt_row;

  -- and emit that row
  out union result;
end;
```

Now let's briefly discuss what is above.  The two essential parts are:

`fetch result from cursor main_row(like result);`

and

`update cursor result(like alt_row) from cursor alt_row;`


In the first case what we're saying is that we want to load the columns of `result` from `main_row`
but we only want to take the columns that are actually present in `result`.  So this is a narrowing
of a wide row into a smaller row.  In this case the smaller row, `result` is what we want to emit.
We needed the other columns to compute `alt_row`.

The second case, what we're saying is that we want update `result` by replacing the columns
found in `alt_row` with the values in `alt_row`.   So in this case we're writing a smaller cursor
into part of a wider cursor.  Note that we used the `update` form here becuase it preserves
all other columns.  If we used `fetch` we would be rewriting the entire row contents, using `NULL`
if necessary, not desired here.

Here is the rewritten version of the above procedure; this is what ultimately gets compiled into C.

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

Of course you could have typed all that before but when there’s 50 odd columns it gets old fast and it’s very error prone.  The sugar form is going to be 100% correct and much less typing.

Finally, while I've shown both `LIKE` forms seperately they can also be used together.  For instance

```sql
    update cursor C(like X) from cursor D(like X);
```

The above would mean, "move the columns that are found in `X` from cursor `D` to cursor `C`", presumably `X` has columns common to both.

### Fetch Statement Specifics

Many of the examples used the `FETCH` statement in a sort of demonstrative way that is hopefully self-evident but the statement has many forms and so it's wroth going over them specifically.  Below we'll use the letters `C` and `D` for the names of cursors.  Usually `C`;

#### For Statement or Result Set Cursors

A cursor declared in one of these forms:

* `declare C for select * from foo;`
* `declare C for call foo();`  (foo might end with a `select` or use `out union`)

Is either a statement cursor or a result set cursor.  In either case it moves through the results.  You load the next row with

* `FETCH C`, or
* `FETCH C into x, y, z;`

In the first form `C` is said to be *automatic* in that it automatically declares the storage needed to hold all its columns.  As mentioned above automatic cursors have storage for their row.

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
 Automatic cursors are so much easier to use than explicit storage that explicit storage is rarely seen.  Storing to `out` parameters is a case where explicit is ok, the `out` parameters have to be declared anyway.

 #### For Value Cursors

 A cursor declared in one of these forms:

 * `declare C cursor fetch from call foo(args)`
   * `foo` must be a procedure that returns one row with `OUT`
 * `declare C cursor like select 1 id, "x" name;
 * `declare C cursor like X;`
   * where X is the name of a table, a view, another cursor, or a procedure that returns a structured result

 Is a value cursor.  A value cursor is always *automatic*, it's purpose is to hold a row.  It doesn't iterate over anything but it can be re-loaded in a loop.

 * `fetch C` or `fetch C into ...` is not valid on such a cursor, it doesn't have a source to step through

 The canonical ways to load such a cursor is:

 * `fetch C from call foo(args);`
   * `foo` must be a procedure that returns one row with `OUT`
 * `fetch C(a,b,c...) from values(x, y, z);`

The first form is in some sense the origin of value cursor.  Value cursors were added to the language initially to have a way to capture the single row `out` statement results, much like result set cursors were added to capture procedure results from `out union`.  In the first form the cursor storage (a C struct) is provided by reference as a hidden out parameter to procedure and the procedure fills it in.  The procedure may or may not use the `out` statement in its control flow the the cursor might not hold a row.  You can use `if C then ...` as before to test for a row.

The second form is more interesting as it allows the cursor to be loaded from arbitary expressions subject to some rules:
 * you should think of the cursor as a logical row, it's fully loaded or not, therefore you must specify enough columns in the column list to ensure that all `NOT NULL` columns will get a value
 * if not mentioned in the list, NULL will be loaded if possible
 * if insufficient columns are named, an error is generated
 * if the value types specified are not compatible with the column types mentioned, an error is generated

With this form, any possible valid cursor values could be set, but many forms of updates that are common would be awkward. So there are various forms of syntatic sugar that are automatically rewitten into the canonical form.  Several standard rewrites happen.

* `fetch C from values(x, y, z)`
  * if no columns are specified this is the same as naming all the columns, in order

* `fetch C from arguments`
  * the arguments to the procedure in which this statement appears are used, in order, as the values
  * in this case `C` is also rewritten into `C(a,b,c,..)`

* `fetch C from arguments like C`
  * the arguments to the procedure in which this statement appears are used, by name, as the values
  * the order in which the arguments appeared no longer matters, the names that match the columsn of C are used if present
  * the formal parameter name may have a single trailing underscore (this is what `like C` would generate)
  * e.g. if `C` has columns `a` and `b` then there must exist formals named `a` or `a_` and `b` or `b_` in any position

* `fetch C(a,b) from cursor D(a,b)`
  * the named columns of D are used as the values
  * in this case it becomes: `fetch C(a,b) from values(D.a, D.b);

That most recent form does seem like it saves much but recall the first rewrite:

* `fetch C from cursor D`
  * both cursors are expanded into all their columns, creating a copy from one to the other
  * `fetch C from D` can be used  if the cursors have the exact same column names and types; it also generates slightly better code and is a common case

 It is very normal to want to use some of the columns of a cursor in a standard way, these `like` forms do that job.

 * `fetch C from cursor D(like C)`
   * here `D` is presumed to be "bigger" than `C`, in that it has all of the `C` columns and maybe more.  The `like C` expands into the names of the `C` columns so `C` is loaded from the `C` part of `D`
   * the expansion might be `fetch C(a, b, g) from values (D.a, D.b, D.g)`
   * `D` might have had fields `c, d, e, f` which were not used because they are not in `C`.

 The symmetric operation, loading some of the columns of a wider cursor can be expressed neatly:

 * `fetch C(like D) from cursor D`
   * the `like D` expands into the columns of `D` causing the cursor to be loaded with what's in `D` and `NULL` (if needed)
   * this might look like `fetch C(d1, d2) from values(D.d1, D.d2)`

Like can be used in both places, for instance suppose `E` is a cursor that has a subset of the rows of both `C` and `D`.  Without ever loading this cursor, just by defining its type you can write a form like this:

* `fetch C(like E) from cursor D(like E)`
  * this means take the column names found in `E` and copy them from D to C.
  * the usual type checking is done of course but the types of the columns in `E` won't matter, only those in `C` and `D`

 As is mentioned above, the `fetch` form means to load an entire row into the cursor.  This is important because "half loaded" cursors would be semantically problematic.  However there are many cases where you might like to amend the values of an already loaded cursor.  You can do this with the `update` form.

 * `update cursor C(a,b,..) from values(1,2,..);
   * the update form is a no-op if the cursor is not already loaded with values (!!)
   * the columns and values are type checked so a valid row is ensured (or no row)
   * all the re-writes above are legal so `update cursor C(like D) from D` is possible, it is in fact the use-case for which this was designed.

### Calling Procedures with Bulk Arguments

It's often desireable to treat bundles of arguments as a unit, or cursors as a unit, especially calling other procedures.  The patterns above
are very helpful for moving data between cursors, arguments, and the database.  These can be rounded out with similar constructs for
procedure calls as follows.

First we'll define some shapes to use in the examples.  Note that we made `U` using `T`.

```sql
create table T(x integer not null, y integer not null,  z integer not null);
create table U(like T, a integer not null, b integer not null);
```

As we've seen, we can do this:

```sql
create proc p1(like T)
begin
   call printf("%d %d %d\n", x_, y_, z_);
end;
```

But the following is also possible. It isn't an especially fabulous example but of course
it generalizes. The arguments will be `x_`, `y_`, and `z_`.

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
than just `T`. The arguments will be `x_`, `y_`, `z_`, `a_`, `b_`

```sql
create proc q2(like U)
begin
  /* just the args that match T: so this is still call p(x_, y_, z_) */
  call p1(from arguments like T);
end;
```

Or similarly. using a cursor.

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

Since these forms are simply syntatic sugar, they can also appear inside of functions in
SQL statements. The variables mentioned will be expanded and become bound variables just
like any other variable that appears in a SQL statement.

Note the form  x IN (from arguments) is not supported at this time, though this is a realitively
easy addition.

### Missing Data Columns, Nulls and Dummy Data

What follows are the rules for columns that are missing.  This are also done by rewriting the AST. There are several options, with the dummy data choices (see below) being really only interesting in test code.  None of what follows applies to the `update cursor` statement because its purpose is to do partial updates.

* When fetching a row all the columns must come from somewhere, if the column is mentioned or mentioned by rewrite then it must have a value mentioned, or mentioned by rewrite
* For columns that are not mentioned, a NULL value is used if it is legal
  * `fetch C(a) from values(1)` might turn into `fetch C(a,b,c,d) from values (1, NULL, NULL, NULL)`

In addition to the automatic NULL you may add the annotation `@dummy_seed([long integer expression])`, if present
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

Now in this example we don't need to know anything about `my_table` other than that it has a column named `id`.  As the example shows several things.
 * we got the shape of the cursor from the table we were inserting into
 * you can do your own computation for some of the columns (those named) and leave the unnamed values to be defaulted
 * the rewrites mentioned above work for the `insert` statement as well as `fetch`
 * in fact `insert into my_table(id) values(i+10000) @dummy_seed(i)` would have worked too with no cursor at all
   * bonus, dummy blob data does work in insert statements because SQLite can do the string conversion easily
   * the dummy value for a blob is a blob that holds the text of the column name and the text of the seed just like a string column

The `@dummy_seed` form can be modified with `@dummy_nullables`, this indicates that rather than using NULL for any nullable value that is missing, CQL should use the seed value.  This overrides the default behavior of using NULL where columns are needed.  Note the NULL filling works a little differently on insert statements.  Since SQLite will provide a NULL if one is legal the column doesn't have to be added to the list with a NULL value during rewriting, it can simply be omitted, making the statement smaller.

Finally for `insert` statement only, SQLite will normally use the default value of a column if it has one, so there is no need to add missing columsn with default values to the insert statement.  However if you specify `@dummy_defaults` then columns with a default value will instead be rewritten and they will get `_seed_` as their value.

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



<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 6. Calling Procedures Defined Elsewhere

CQL generally doesn't see the whole world in one compilation.  
In this way it's a lot more like say the C compiler than it is like say Java
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

```C
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

Can be used two interesting ways:

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
a value cursor as we previously saw.

These combinations allow for pretty general composition of stored procedures
as long as they are not recombined with SQLite statements.

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 7. CQL Result Sets
Most of this tutorial is about the CQL language itself but here we must diverge a bit.  The purpose of the 
result set features of CQL is to create a C interface to SQLite data.  Because of this 
there are a lot of essential details that require looking carefully at the generated C code.  Appendix 2
covers this code in even more detail but here it makes sense to at least talk about the interface.

If we have this simple stored procedure:

```sql
create table foo(id integer not null, b bool, t text);

create proc read_foo(id_ integer not null)
begin
  select * from foo where id = id_;
end;
```

We've created a simple data reader, this CQL code will cause the compiler to
generated helper functions to read the data and materialize a result set.  

Let's look at the public interface of that result set now considering the most essential pieces.

```C
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
```C
cql_result_set_type_decl(
  read_foo_result_set, 
  read_foo_result_set_ref);
```
This declares the data type for `read_foo_result_set` and the associated object reference `read_foo_result_set_ref`.  
As it turns out the underlying data type for all result sets is the same, only the shape of the data varies.


```C
extern cql_code read_foo_fetch_results(sqlite3 *_Nonnull _db_, 
  read_foo_result_set_ref _Nullable *_Nonnull result_set, 
  cql_int32 id_);
```
The result set fetcher method gives you a `read_foo_result_set_ref` if it succeeds.  It accepts the `id_` argument which it
will internally pass along to `read_foo(...)`.  The latter function provides a `sqlite3_stmt*` which can then be iterated in the fetcher.
This method is the main public entry point for result sets.

Once you have a result set, you can read values out of it.

```C
extern cql_int32 read_foo_result_count(read_foo_result_set_ref 
  _Nonnull result_set);
```
That function tells you how many rows are in the result set.  

For each row you can use any of the row readers:

```C
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
to get the value.  Finally the string can be accessed with `read_foo_get_t`.  As you can see there is a
simple naming convention for each of the field readers.

Note:  The compiler has runtime arrays that control naming conventions as well as using CamelCasing.  Additional customizations may be created by adding new runtime arrays into the CQL compiler.

Finally, also part of the public interface, are these macros:

```C
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
structures for the backing store.  However, it's not everything, there are also cases where full flexibility is needed
while producing a standard many-row result set.  For this we have `out union` which was dicussed fully in Chapter 5.  Here we'll discuss the code generation behind that.


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

```C
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
`cql_fetch_all_results`, the difference is of course that you write the loop manually and therefore have 
full control of the rows as they go in to the result set.

In short, the overhead is pretty low.  What you’re left with is pretty much the base cost of your algorithm.  The cost here is very similar to what it would be for any other thing that make rows.

Of course, if you make a million rows, well, that would burn a lot of memory.

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 8. Functions

CQL stored procs have a very simple contract so it is easy to declare procedures and then implement them in regular C, the C functions just have to conform to the contract.  However, there is no notion of functions at all and this makes it very inconvenient to use some external code and is not doing database things and wants to return values.  Even a random number generator or something would be difficult to use because it could not be called in the context of an expression.  To allow for this CQL adds declared functions

In an other example of the two-headed nature of CQL, there are two ways to declare functions.  As we have already
seen you can make function-like procedures and call them like functions simply by making a procedure with an `out` parameter. However, there are also cases where it is reasonable to make function calls to external functions of other kinds.  There are three major types of functions you might wish to call.

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
When the object goes out of scope it is release as usual.

If we also declare this procedure:

```sql
declare procedure dict_add(
    dict object not null, 
    key_ text not null, 
    value text not null);
```

Then with this family of declarations we could write something like this:

```sql
create proc create_and_init(out dict object not null)
begin
  set dict := dict_create();
  call dict_add(dict, "k1", "v1");
  call dict_add(dict, "k2", "v2");
  if (dict_get_value(dict, "k1") == dict__get_value(dict, "k2)) then
    call printf("insanity has ensued\n");
  end if;
end;
```

Note: Ordinary scalar functions may not use the database in any way, when they are invoked they will not
be provided with the database pointer and so they will be unable to do any database operations.  To do
database operations use regular procedures.  You can create a function-like-procedure using the `out` convention
discussed previously.

#### SQL Scalar Functions

SQLite includes the ability to add new functions to its expressions using `sqlite3_create_function`.  In
order to use this function in CQL, you must also provide its prototype definition to the compiler.  You
can do so like in this example:

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

See [Create Or Redefine SQL Functions](https://www.sqlite.org/c3ref/create_function.html).

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

### Notes on Builtin Functions

Some of the SQLite builtin functions are hard-coded,  these are the functions that have semantics that are not readily captured with a simple prototype.  Other SQLite functions can be declared wtih `declare select funtion ...` and then used.

CQL's hard-coded builtin list includes:

*Aggregate Functions*

 * count
 * max
 * min
 * sum
 * total
 * avg
 * average
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
 * ptr

`Nullable` casts an operand to the nullable version of its type and otherwise does nothing.  This cast might be useful if you need an exact type match in a situation.  It is stripped from any generated SQL and generated C so it has no runtime effect at all other than the indirect consequences of changing the storage class of its operand.

`Ptr` is used to cause a reference type variable to be bound as a long integer to SQLite. This is a way of giving object pointers to SQLite UDFs.

See [SQLite Core Functions](https://www.sqlite.org/lang_corefunc.html).


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 9. Statements Summary and Error Checking

The following is a brief discussion of the major statement types and the semantic rules that CQL enforces for each of the statements.  A detailed discussion of SQL statements (the bulk of these) is beyond the scope of this document and you should refer to the SQLite documentation for most details.  However, in many cases CQL does provide additional enforcement and it is helpful to describe the basic checking that happens for each fragment of CQL.  A much
more authoritative list of the things CQL checks for can be inferred from the error documentation.  "Tricky" errors have examples and suggested remediation.

### The Primary SQL Statements

These are roughly, the statements that involve the database.

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
Select * is special in that it creates its own struct type by assembling
all the columns of all the tables in the selects join result.  CQL rewrites these statement into 
a select with the specific columns explicitly listed.  While this makes the program
slightly bigger it means that logically deleted columns are never present in results
because `SELECT *` won't select them and attempting to use a logically deleted
column results in an error.

#### The `CREATE TABLE` Statement
Unlike the other parts of DDL we actually deeply care about the tables.
We have to grab all the columns and column types out of it and create
the appropriate structure type for the table.
Along the way we validate a bunch of stuff like:
* verify unique table name
* no duplicate column names
* recursive correctness of constraints (see constraints discussion below)

#### The `CREATE INDEX` Statement
CQL doesn't really do anything with indices but we do validate that they make sense (so we lookup all the names of all the columns and so forth).

#### The `CREATE VIEW` Statement
Create view analysis is very simple because the `select` analysis does the heavy lifting.  All we
have to do is validate that the view is unique, then validate the select statement.

Additionally, views must not be allowed to have any NULL type columns, all nulls must be converted to
some type with a CAST.   e.g. `create view foo as select NULL n` is not valid.  NULL is not a real storage type.

#### The `CREATE TRIGGER` Statement
The create trigger statement is quite a beast, validations include:
 * the trigger name must be unique
 * For `insert` the "new.*" table is available in expressions/statement
 * For `delete` the "old.*" table is available in expressions/statements
 * For `update` both are available
    * If optional columns present in the `update`, they must be unique/valid
 * The `when` expression must evaluate to a numeric
 * The statement list must be error free with the usual rules plus new/old
 * The `raise` function may be used inside a trigger (NYI)
 * The table name must be a table (not a view) UNLESS the trigger type is `INSTEAD OF`
 * select statements inside the statement block do not count as returns for the procedure that includes the create trigger

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

#### The `UNIQUE KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again we use the usual helpers
for name lookup within the context of the table that contains the constraint.

#### The `FOREIGN KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again we use the usual helpers
for name lookup within the context of the table with the foreign key.
 Note that the foreign has to be validated against two tables to fully validate it.

#### The `PRIMARY KEY` Clause
Similar to other constraints, we don't actually do anything with this
other than offer some validation.  Again we use the usual helpers
for name lookup within the context of the table with the primary key.

#### The `RAISE` Statement
CQL validates that `RAISE` is being used in the context of a trigger and that
it has the correct arguments.

#### The `ALTER TABLE ADD COLUMN` Statement
To validate `alter table add column` we check the following:
* the table must exist and not be a view (in any version)
* the column definition of the new column must be self-consistent
* no auto increment columns may be added
* added columns must be either nullable or have a default value

Note: Alter statements are typically used in the context of migration so it's
possible the table that is mentioned is condemned in a future version.  We still have to run
the intervening upgrade steps so basically DDL gets to ignore the current deadness
of the table as in context it's might be "not dead yet".  This will be more obvious
in the context of the schema maintenance features. (q.v.)

#### The `DELETE` Statement
The delete analyzer sets up a scope for the table being
deleted and the validates the WHERE clause if present against that scope.
Additionally we verify that the table actually was defined and is not a view.

#### The `UPDATE` Statement
The update analyzer sets up the scope for the table(s) being updated.  If there are
optional clauses (e.g. `LIMIT`) they are evaluated just like in a select statement
with those same helper methods.  Expression fragments are evaluated just as
in a select statement.

#### The `INSERT` Statement
We check that the table exists and then we walk the columns and the value list
to make sure they are valid for the table. Also we cannot insert into a view.

Details:
* The column list specifies the columns we will provide, they must exist and be unique.
* The columns specified must suffice to insert a row (all not nulls and not default present)
* The insert list specifies the values that are to be inserted.
* The type of each value must match the type of the column.
* Autoinc columns may be specified as NULL.
* If there are too many or too few columns, that is an error.
* If no columns are specified that is the same as if all columns had been specified, in table order

#### The `THROW` Statement
Throw can literally go anywhere, so it's always ok.

#### The `BEGIN TRANSACTION` Statement
Begin transaction can go anywhere, it's always ok.

The sqlite documentation can be helpful (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `COMMIT TRANSACTION` Statement
Commit transaction can go anywhere, it's always ok.

The sqlite documentation can be helpful (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `ROLLBACK TRANSACTION` Statement
Rollback trans can go anywhere but if you're using the format
where you rollback to a particular save point then we must have
seen that name in a `savepoint` statement previously or it's an error.

The sqlite documentation can be helpful (CQL syntax is a subset).  See: https://www.sqlite.org/lang_transaction.html

#### The `SAVEPOINT` Statement
The `savepoint` statement can go anywhere but we do record this savepoint name
as having been seen so we can verify it in rollback.  So this is sort of a weak declaration of the savepoint name.

The sqlite documentation can be helpful (CQL syntax is a subset).  https://www.sqlite.org/lang_savepoint.html

#### The `RELEASE SAVEPOINT` Statement
Release savepoint can go anywhere but we must have
seen that name in a previous `savepoint` statement or it's an error.

The sqlite documentation can be helpful (CQL syntax is a subset). https://www.sqlite.org/lang_savepoint.html

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
 * we determine if it uses the database, this will change the emitted signature of the proc to include a `sqlite3 *db`
     input argument and it will return a sqlite error code (e.g. `SQLITE_OK`)
 * select statements that are loose in the proc represent the "return" of that
   select;  this changes the signature to include a `sqlite3_stmt **pstmt` parameter corresponding to the returned statement

#### The `IF` Statement
The top level if node links the initial condition with a possible
series of else_if nodes and then the else node.  Each condition is
checked for validity. The conditions must be valid expressions that
can be converted to a boolean.

#### The `SET` Statement
The set statement is for variable assignment.  We just validate
that the target exists and is compatible with the source.
Cursor variables cannot be set with simple assignment and CQL generates
errors if you attempt to do so.

#### The `DECLARE PROCEDURE` Statement
There are three forms of this declaration:
* a regular procedure with no DML
   * e.g. `declare proc X(id integer);`
* a regular procedure that uses DML (it will need a db parameter and returns a result code)
   * e.g. `declare proc X(id integer) using transaction;`
* a procedure that returns a result set, you provide the result columns
   * e.g. `declare proc X(id integer) : (A bool not null, B text);`
The main validations here are that there are no duplicate parameter names, or return value columns.

#### The `DECLARE FUNCTION` Statement
Function declarations are similar to than procedures; there must be a return type
(use proc if there is none).  The `DECLARE SELECT FUNCTION` form indicates a function
visible to SQLite, other functions are usable in the `call` statement.

#### The `DECLARE` Variable Statement
This declares a new local or global variable that is not a cursor.
The type is computed with the same helper that is used for analyzing
column definitions.  Once we have the type we walk the list of variable
names, check them for duplicates and such (see above) and assign their type.  The canonical
name of the variable is defined here, if it is later used with a different casing the output
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
  * cursor names have the same rules duplicates as other variables
With this in mind, both cases simply recurse on either the select or the call
and then pull out the structure type of that thing and use it for the cursor's shape.  If the
`call` is not semantically valid according to the rules for calls or the `select` is not semantically valid,
 then of course this declaration will generate errors.

[marker: initial proof reading ended here, what follows is much rougher]

#### The `DECLARE` Value Cursor Statement
This statement declares a cursor that will be based on the return type of a procedure
when using this form the cursor is also fetched, hence the name.  The fetch result of
the stored proc will be used for the value.  At this point we use its type only.
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
Loop depth is increased as with while.

#### The `CALL` Statement
There are three ways that a call can happen:
  * signatures of procedures that we know in full:
    * call foo();
    * declare cursor for call foo();
  * some external call to some outside function we don't known
    * e.g. call printf('hello, world\n');

The cursor form can be used if and only if the procedure has a loose select
or a call to a procedure with a loose select. In that case the procedure will
have a structure type, rather than just "ok" (the normal signature for a proc).
If the user is attempting to do the second case, cursor_name will be set and
the appropriate verification happens here.

Note:  Recursively calling fetch cursor is not really doable in general
because at the point of the call we might not yet know that the method
does in fact return a select.  You could make it work if you put the select
before the recursive call.

Semantic rules:
 * for all cases each argument must be error-free (no internal type conflicts)
 * for known procs
   * the call has to have the correct number of arguments
   * if the formal is an out parameter the argument must be a variable
     * the type of the variable must be an exact type match for the formal
   * non-out parameters must be type-compatable, but exact match is not required

#### The `FETCH` Statement
The fetch statement has two forms:
  * fetch C into var1, var2, var3 etc.
  * fetch C;
The second form is called the auto_cursor.
In the first form the variables of the cursor must be assignment compatable
with declared structure type of the cursor and the count must be correct.
In the second form, the codegen will implicitly create local variables that
are exactly the correct type, but that's later.  Since no semantic error is
possible in that case we simply record that this is an auto_cursor and then
later we will allow the use of C.field during analysis.
Of course "C" must be a valid cursor.

#### The `CONTINUE` Statement
We just need to ensure that continue is inside a loop.

#### The `LEAVE` Statement
We just need to ensure that leave is inside a loop.

#### The `TRY/CATCH` Statements
No analysis needed here other than that the two statement lists are ok.

#### The `OPEN` CURSOR Statement
For open [cursor], we just validate that the name is in fact a cursor.

#### The `CLOSE` CURSOR Statement
For close [cursor], we just validate that the name is in fact a cursor.

#### The `OUT` CURSOR Statement
For out [cursor], we first validate that the name is a cursor
then we set the output type of the procedure we're in accordingly

### The "Meta" Statements

The programs control the overall meaning the program or give the compiler specific directives
as to how the program should be compiled.  

#### The `@ECHO` Statement
echo is valid in any top level contexts

#### The `@PREVIOUS SCHEMA` Statement
Begins the region where previous schema will be compared against what has been
declared before this directive for alterations that could not be upgraded.

#### The `@SCHEMA_UPGRADE_SCRIPT` Statement
When upgrading the DDL it's necessary to emit create table statements
for the original version of the schema.  These create statements conflict
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
Switch to strict mode.  Presently this is only supported for `FOREIGN KEY` verification.  In strict mode every foreign key must have some delete/update policy such as `ON DELETE CASCADE`.  Any policy suffices.

#### The `@ENFORCE_NORMAL` Statement
Turn off strict enforcement (currently only affects `FOREIGN KEY` clauses).

#### The `@DECLARE_SCHEMA_REGION` Statement
A schema region is an partitioning of the schema such that it
only uses objects in the same partition or one of its declared
dependencies.  One schema region may be upgraded independently
from any others (assuming they happen such that dependents are done first).
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

### Important Program Fragments

These items appear in a variety of places and are worth of discussion.  They are generally handled uniformly.

#### Argument Lists
Walk an entire argument list and do the type inference on each argument.
Not that this happens in the context of a function call and depending
on what the function is, there may be rules for compatibility of the
arguments with the function and each other.  That doesn't happen here.
This just gets the type of each arg and makes sure independently they are
not bogus.

#### Procedures that return a Result Set
If a procedure is returning a select statement then we need to attach that
type to the procedures semantic info.  We have to do some extra validation
at this point, especially if the proc already has some other select return.
This is where we make sure all the kinds of selects that might be returned
are 100% compatible.

#### General Name Lookups
Try to look up a [possibly] scoped name in one of the places:
1. a column in the current join if any (this must not conflict with #2)
2. a local or global variable
3. a field in an open cursor
otherwise, name not found.

#### Object Types with a Discriminator
We check that object<Foo> only combines with object<Foo> or object in lists of objects (like IN)
* If there is a current object type, then the next item in the expression or must match
* If there is no such type, then an object type that arrives becomes the required type
* If they ever don't match record an error

#### The `CASE` Expression
There are two parts to this, the "when" expression and the "then" expression.
We compute the aggregate type of the when expressions as we go, promoting it
up to a larger type if needed (e.g. if one when is an int and the other is
a real then the result is a real).   Likewise nullability is computed as
the aggregate.  Note that if nothing matches the result is null, so we always
get a nullable result unless there is an "else" expression.
If we started with case expr then each when expression must be comparable
to the case expression.  If we started with case when xx then yy;  then
each case expression must be numeric (typically boolean).

#### The `BETWEEN` EXPRESSIONS
Between requires type compatibility between all three of its arguments.
Nullability follows the usual rules, if any might be null then the result
type might be null.  In any case the result's core type is BOOL.

#### The `CAST` Expression
For cast expressions we use the type provided for the semantic type
the only trick is that we preserve the combined_flags of the input argument.

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
User defined function, this is an external function
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
 * in code-gen we will create a temporary for it, semantic analysis doesn't care

#### Root Expressions
A top level expression defines the context for that evaluation.  Different expressions
can have constraints.  e.g. aggregate functions may not appear in the `WHERE` clause of a statement.  There are cases where expression nesting can happen, this nesting changes the evaluation context accordingly, e.g. you can put a nested select in a where clause and that
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
The on expression should be something that can be used as a bool
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
 
 The `select *` construct is very popular in many codebases but it can be unsafe to use in production code because, if the schema changes, the code might get columns it does not expect.  Note the extra columns could have appeared anywhere in the result set because the `*` applies to the entire result of the `FROM` clause, joins and all,  so extra columns are not necessarily at the end and column ordinals are not preserved.  CQL mitigates this situation somewhat with some useful constraints/features:

* in a `select *`, and indeed in any query, the column names of the select must be unique, this is because:
   * they could form the field names of an automatically generated cursor (see the section on cursors)
   * they could form the field names in a CQL result set (see section on result sets)
   * it's weird/confusing to not have unique names generally
* when issuing a `select *` or a `select T.*` CQL will automatically expand the `*` into the actual logical columns that exist in the schema at the time the code was compiled
   * this is important because if a column had been logically deleted from a table it would be unexpected in the result set even though it is still present in the database and would throw everything off
   * likewise if the schema were to change without updating the code, the code will still get the columns it was compiled with, not new columns
   
Expanding the `*` at compile time means Sqlite cannot see anything that might tempt it to include different columns in the result.

With this done we just have to look at the places a `select *` might appear so we can see if it is safe to use `*` and, by extension of the same argument, `T.*`, or at least reasonably safe.

*In an `EXISTS` or `NOT EXISTS` clause like `where not exists (select * from x)`*

* this is perfectly safe, the particular columns do not matter, `select *` is not even expanded in this case.

*In a statement that produces a result set like `select * from table_or_view`*

* binding to a CQL result set is done by column name and we know those names are unique
* we won't include any columns that are logically deleted, so if you try to use a deleted column you'll get a compile time error

In a cursor statement like `declare C cursor for select * from table_or_view` there are two cases here

*Automatic Fetch  `fetch C;`*

* in this case you don't specify the column names yourself, they are inferred
* you are therefore binding to the columns by name, so new columns in the cursor would be unused (until you choose to start using them)
* if you try to access a deleted column you get a compile-time error

*Manual Fetch:  `fetch C into a, b, c;`*

* In this case the number and type of the columns must match exactly with the specified variables
* If new columns are added, deleted, or changed, the above code will not compile

So consdering these cases above we can conclude that auto expanding the `*` into the exact columns present in the compile-time schema version ensures that any incompatible changes result in compile time errors. Adding columns to tables does not cause problems even if the code is not recompiled. This makes the `*` construct much safer, if not perfect, but no semantic would be safe from arbitary schema changes without recompilation.  At the very least here we can expect a meaningful runtime error rather than silently fetching the wrong columns.  
 
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

Now let's look at the normal join, this is our reference:
```
select * from A T1 inner join B T2 on T1.id = T2.id;

result:

1|a1|b1|1|c1|d1
2|a2|b2|2|c2|d2
```
As expected, you get all the columns of A, and all the columns of B.  The 'id' column appears twice.


However with the `USING` syntax:

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

Now in CQL, the `*` and `T.*` forms are automatically expanded, SQLite doesn't see the `*`.  This is done so that if any columns have been logically deleted they can be elided from the result set.  Given that this happens, the `*` operator will expand to ALL the columns.  Just the same as if you did `T1.*` and `T2.*`.

*As a result, in CQL, there is no difference between  the `USING` form of a join and the `ON` form of a join.*

In fact, only the `select *` form could possibly be different, so in most cases this ends up being moot anyway.  Typically you don't to use `*` in the presence of joins because of name duplication and ambiguity of the column names of the result set.  CQL's automatic expansion means you have a much better idea exactly what columns you will get -- those that were present in the schema you declared.

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 10. Schema Management Features

CQL has a lot of schema knowledge already and so it's well positioned to think about schema upgrades and versioning.

It seemed essential to be able to record changes to the schema over time so CQL got an understanding of versioning.  This lets you do things like:

* ensure columns are only added where they should be
* generate compiler errors if you try to access columns that are deprecated
* move from one version to another tracking schema facets that have to be added

### Annotations

There are three basic flavors of annotation

* `@create(version [, migration proc])`
* `@delete(version [, migration proc])`
* `@recreate`

They have various constraints:

* `@create` and `@delete` can only be applied to tables and columns
* `@recreate` can only be applied to tables (nothing else needs it anyway)
* `@recreate` cannot mix with `@create` or `@delete`
* `@recreate` can include a group name as in `@recreate(musketeers)`, if a group name is specified then all the tables in that group are recreated if any of them change

Indices, Views, and Triggers are always "recreated" (just like tables can be) and so neither the `@recreate` nor the `@create` annotations are needed (or allowed).  However when an Index, View, or Trigger is retired it must be marked with `@delete` so that it isn't totally forgotten but can be deleted anywhere it might still exist.  Note that when one of these items is deleted the definition is not used as it will only be dropped anyway, the simplest creation of the object with the correct name will do the job as a tombstone.

e.g. `create view used_to_be_fabulous as select 1 x @delete(12);`  suffices to drop the `used_to_be_fabulous` view in version 12 no matter how complicated it used to be.  It's `CREATE VIEW` will not be emitted into the upgrade procedure in any case.  Similarly trivial indices and triggers of the correct name can be used for the tombstone.

In addition, if there is some data migration that needs to happen at a particular schema version that isn't associated with any particular change in schema, you can run an *ad hoc* migrator at any time.  The syntax for that is`@schema_ad_hoc_migration(version, migration proc);`  Ad hoc migrations are the last to run in any given schema version, they happen after table drop migrations.

### Semantics

`@create` declares that the annotated object first appeared in the indicated version, and at that time the migration proc needs to be executed to fill in default values, denormalize values, or whatever the case may be.

`@delete` declares that the annotated object disappeared in the indicated version, and at that time the migration proc needs to be executed to clean up the contents, or potentially move them elsewhere. 

`@recreate` declares that the annotated object can be dropped and recreated when it changes, there is no need to preserve its contents during an upgrade. Such objects may be changed arbitrarily from version to version

* no columns in a `@recreate` table may have `@create` or `@delete` (it isn't needed anyway)
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
* columns may be deleted but this is only a logical delete, SQLite has no primitive to remove columns; once deleted you may not longer refer to that column in queries
* deleted columns must be nullable or have a default value (otherwise all existing and future insert statements would break for sure, the column isn't really gone)
* views, indices, and triggers may be added (no annotation required) and removed (with `@delete`) like tables
* views, indices, and triggers may be altered completely from version to version
* no normal code is allowed to refer to deleted columns, tables, etc.  this includes views, indices, and triggers
* schema migration stored procs see the schema as it existed in their annotation (so an older version), they are also forbidden from using views (see below)
* recreated objects (tables marked with @recreate, views, tables, and indices) have no change restrictions


### Prosecution

Moving from one schema version to another is done in an orderly fashion with the migration proc taking these essential steps in this order

* the ```cql_schema_facets``` table is created if needed, this records the current state of the schema
* the last known schema hash is read from the ```cql_schema_facets``` tables (it's zero by default)
* if the overall schema hash code matches what is stored processing stops, otherwise an upgrade ensues
* all known views are dropped (hence migration procs won't see them!)
* any index that needs to change is dropped (this includes items marked ```@delete``` or indices that are different than before)
  * change is detect by hash (crc64) of the previous index definition vs. the current
* all known triggers are dropped (hence they will not fire during migration!)
* the current schema version is extracted from ```cql_schema_facets``` (it's zero by default)
* if the current schema version is 0 then the original version of all the tables are created

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
* all views not marked with `@delete` are recreated
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

-- this is a simple trigger, it's a bit silly but that doesn't matter
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
* they have to be in ascending  schema version order (but you can add several columns in one version)
* there may or may not be a proc to run to populate data in that column when it's added or removed data when it's deleted
   * proc names must be unique
* you can't delete a table or column in a version before it was created
* you can't delete a column in a table in a version before the table was created
* you can't create a column in a table in a version after the table was deleted
* there's probably more I forgot...

### Sample Upgrade Script
With just those annotations you can automatically create the following upgrade script which is itself CQL (and hence has to be compiled). This code is totally readable!

I've split the script into logical pieces to explain what's going on.

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
We have only the one trigger, we declare it here.

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
        WHERE facet = _facet LIMIT 1);
  END TRY;
  BEGIN CATCH
    SET _version := 0;
  END CATCH;
END;
```
The two procs `cql_get_facet_version` and `cql_set_facet_version` do just what you would expect.  Note the use of `try` and `catch` to return a default value if the select fails.

There are two additional helper procedures that do essentially the same thing using a schema version index.  These two methods exist only to avoid unnecessary repeated string literals in the output file which cause bloat.

```sql
-- helper proc for getting the schema version CRC for a version index
CREATE PROCEDURE test_cql_get_version_crc(_v INTEGER NOT NULL, 
                                          out _crc LONG INTEGER NOT NULL)
BEGIN
  BEGIN TRY
    SET _crc := (SELECT version FROM test_cql_schema_facets 
        WHERE facet = 'cql_schema_v'||_v LIMIT 1);
  END TRY;
  BEGIN CATCH
    SET _crc := -1;
  END CATCH;
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
```
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
CREATE PROCEDURE test_cql_drop_indices()
BEGIN
  DECLARE index_crc LONG INTEGER NOT NULL;
  CALL test_cql_get_facet_version('index_still_present_index_crc', index_crc);
  IF index_crc <> -6823087563145941851 THEN
    DROP INDEX IF EXISTS index_still_present;
  END IF;
  DROP INDEX IF EXISTS index_going_away;
END;

-- create all the indices we need
CREATE PROCEDURE test_cql_create_indices()
BEGIN
  DECLARE index_crc LONG INTEGER NOT NULL;
  CALL test_cql_get_facet_version('index_still_present_index_crc', index_crc);
  IF index_crc <> -6823087563145941851 THEN
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

#### Main Migration Script

The main script orchestrates everything.  There are inline comments for all of it.  The general order of events is:

* create schema facets table if needed
* check main schema crc, if it matches we're done here, otherwise continue...

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
  * mark schema upgraded to the current version so far, proceed to the next version
  * each partial step is also marked as completed so it can be skipped if the script is run again
* create all the views
* (re)create any indices that changed and are not dead
* set the schema CRC to the current CRC

That's it... the details are below.

```sql
CREATE PROCEDURE test_perform_needed_upgrades()
BEGIN
  DECLARE column_exists BOOL NOT NULL;
  DECLARE facet_version LONG INTEGER NOT NULL;
  DECLARE schema_version LONG INTEGER NOT NULL;

    -- save the current facets so we can diff them later --
    CALL test_save_cql_schema_facets();

    -- dropping all views --
    CALL test_cql_drop_all_views();

    -- dropping condemned or changing indices --
    CALL test_cql_drop_all_indices();

    -- dropping condemned or changing triggers --
    CALL test_cql_drop_all_triggers();

    -- fetch current schema version --
    CALL test_cql_get_facet_version('cql_schema_version', schema_version);

    ---- install baseline schema if needed ----

    IF schema_version == -1 THEN
      CALL test_cql_drop_baseline_tables();
      CALL test_cql_install_baseline_schema();
      CALL test_cql_set_facet_version('cql_schema_version', 0);
    END IF;

    ---- upgrade to schema version 2 ----

    CALL test_cql_get_version_crc(2, schema_version);
    IF schema_version != 5380988243166701961 THEN
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
      CALL test_cql_get_facet_version('CreateName1Proc', facet_version);
      IF facet_version = 0 THEN
        CALL CreateName1Proc();
        CALL test_cql_set_facet_version('CreateName1Proc', 2);
      END IF;
      CALL test_cql_get_facet_version('CreateName2Proc', facet_version);
      IF facet_version = 0 THEN
        CALL CreateName2Proc();
        CALL test_cql_set_facet_version('CreateName2Proc', 2);
      END IF;

      CALL test_cql_set_version_crc(2, 5380988243166701961);
    END IF;

    ---- upgrade to schema version 3 ----

    CALL test_cql_get_version_crc(3, schema_version);
    IF schema_version != -1945462407517765645 THEN
      -- creating table added_table

      CREATE TABLE IF NOT EXISTS added_table(
        id INTEGER NOT NULL,
        name1 TEXT
      );

      CALL test_cql_set_version_crc(3, -1945462407517765645);
    END IF;

    ---- upgrade to schema version 4 ----

    CALL test_cql_get_version_crc(4, schema_version);
    IF schema_version != -4889894059346952297 THEN
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
      CALL test_cql_get_facet_version('CreateId2Proc', facet_version);
      IF facet_version = 0 THEN
        CALL CreateId2Proc();
        CALL test_cql_set_facet_version('CreateId2Proc', 4);
      END IF;
      CALL test_cql_get_facet_version('DeleteRate2Proc', facet_version);
      IF facet_version = 0 THEN
        CALL DeleteRate2Proc();
        CALL test_cql_set_facet_version('DeleteRate2Proc', 4);
      END IF;

      CALL test_cql_set_version_crc(4, -4889894059346952297);
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

    -- finally produce the list of differences
    SELECT T1.facet FROM
      test_cql_schema_facets T1
      LEFT OUTER JOIN test_cql_schema_facets_saved T2
        ON T1.facet = T2.facet
      WHERE T1.version is not T2.version;
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
    -- save the current facets so we can diff them later --
    CALL test_perform_needed_upgrades();
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

The above simply declares the region, it doesn't put anything into them.  In this case we now have a `root` region and an `extra` region.  The `root` schema items will not be allowed to refer to anything in `extra`.

Without regions, you could also ensure that the above is true by putting all the `extra` items afer the `root` in the input file but things can get more complicated than that in general, and the schema might also be in several files, complicating ordering as the option.  Also relying on order could be problematic as it is quite easy to put things in the wrong place (e.g. add a new `root` item after the `extra` items).  Making this a bit more complicated, we could have:


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

create proc get_detail(integer id_)
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

create proc get_detail(integer id_)
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

With the structure above specified, even if a new contribution to the `root` schema appears later, the rules enforce that this region cannot refer to anything other other things in `root`.  This can be very important if schema is being included via `#include` and might get pulled into the compilation in various orders.  A feature area might also have a named public region that others things can depend on (e.g. some views) and private regions (e.g. some tables, or whatever).

#### Region Visibility

Schema region do not provide additional name spaces, the names of objects should be unique across all regions.  i.e. regions do not hide or scope entity names, rather they create errors if inappropriate names are used.

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

Case 3: Again fails for the same reason as case #1. Table `A` already exist in region `extra`, you can not define another table with the same name in another region.
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

Really the visibility rules couldn't be anything other than the above, SQLite has no knowledge of regions at all and so any exotic name resolution would just doom SQLite statements to fail when they finally run.

##### Exception for `"... LIKE <table>"` statement

The rules above are enforced for all constructs except for where the syntactic sugar `... LIKE <table>` forms, which can happen in a variety of statement. This form doesn't create a dependence on the table (but does create a dependence on its shape). When CQL generates output the `LIKE` construct is replaced with the actual names of columns it refers to.  But these are independent columns, so this is simply a typing-saver.  The table (or view, cursor, etc.) reference will be gone.

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

When creating upgrade scripts using the `--rt schema_upgrade` flags you can add region options `--include_regions a b c` and `--exclude_regions d e f` per the following:

Included regions:
* included regions must be valid region names, the base types are walked to compute all the regions that are "in"
* declarations are emitted in the upgrade for all of the "in" objects, "exclude" does not affect the declarations

Excluded regions:
* excluded regions must be valid region names and indicate parts of schema that are upgraded elsewhere, perhaps with a seperate CQL run, a different automatic upgrade, or even a manual mechanism
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

Note that in the above examples CQL is generating more CQL to be compiled again (a common pattern).  The CQL upgrade scripts need to be compiled as usual to produce executable code.  Thus the output of this form includes the schema declarations and executable DDL.


##### Schema Not In Any Region

For schema that is not in any region you might imagine that it is a special region `<none>` that depends on everything.  So basically you can put anything there.  Schema that is in any region cannot ever refer to schema that is in `<none>`.

When upgrading, if any include regions are specified then `<none>` will not be emitted at all.  If you want an upgrader for just `<none>` this is possible with an assortment of exclusions.  You can always create arbitrary grouping regions to make this easier. A region named `any` that uses all other regions would make this simple.

In general, best practice is that there is no schema in `<none>`, but since most SQL code has no regions some sensible meaning has to be given to DDL before it gets region encodings.

#### Deployable Regions

Given the above we note that some schema regions correspond to the way that we will deploy the schema, we want those bundles to be safe to deploy but to so we need a new notion -- a deployable region.  To make this possible CQL includes the following:

* You can declare a region as deployable using `@declare_deployable_region`
* CQL computes the covering of a deployable region: its transitive closure up to but not including any deployable regions it references
* No region is allowed to depend on a region that is within the interior of a different deployable region, but you can depend on the deployable region itself

Because of the above, each deployable region is in fact a well defined root for the regions it contains.  The deployable region becomes the canonical way in which a bundle of regions (and their content) is deployed and any given schema item can be in only one deployable region.

##### Motivation and Examples
As we saw above, regions are logical groupings of tables/views/etc such that if an entity is in some region `R` then it is allowed to only refer to the things that `R` declared as dependencies `D1`, `D2`, etc. and their transitive closure.  You can make as many logical regions as you like and you can make them as razor thin as you like; they have no physical reality but they let you make as many logical groups of things as you might want.

Additionally, when we’re deploying schema you generally need to do it in several pieces. E.g. if we have tables that go in an in-memory database then defining a region that holds all the in-memory tables makes it easy to say put all those in memory tables into a particular deployment script.

Now we come to the reason for deployable regions. From CQL’s perspective all regions are simply logical groups, some grouping that is meaningful to programmers but has no physical reality. This means you’re free to reorganize tables etc. as you see fit into new or different regions when things should move. Only that’s not quite true. The fact that we deploy our schema in certain ways means while most logical moves are totally fine, if you were to move a table from say the main database region to the in-memory region you would be causing a major problem.  Some installations may already have the table in the main area and there would be nothing left in the schema to tell CQL to drop the table from the main database -- the best you can hope for is the new location gets a copy of the table the old location keeps it and now there are name conflicts forever.

So, the crux of the problem is this. We want to let you move schema freely between logical regions however it makes sense to you but once you pick the region you are going to be deployed in, you can’t change that.

To do this, CQL needs to know that some of the regions are deployable regions and there have to be rules so that it all makes sense.  Importantly, every region has to be contained in at most one deployable region.

Since the regions form a DAG we must create an error if any region could ever roll up to two different deployable regions. The easiest way to describe this rule is “no peeking” – the contents of a deployable region are “private” they can refer to each other in any dag shape but outside of the deployable region you can only refer to its root. So you can still compose them but each deployable region owns a well defined covering. Note you can make as many fine-grained deployable regions as you want, you don’t actually have to deploy them separately, but you get stronger rules about the sharing.

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
* none of the logical regions for feature 1, 2, 3 are allowed to refer to logical regions in any other feature, any of them could refer to Core (but not directly to what is inside Core)
* within those regions you can make any set of groupings that makes sense and change them over time as you see fit
* any such regions are not allowed to move to a different Feature group (because those are deployment regions)
* the Master Deployment regions just group features in ways we’d like to deploy them, in this case there are two deployments one that includes Feature 1 & 2 and another that includes Feature 1 & 3
* the deployable region boundaries are preventing Feature 1 regions from using Feature 2 regions in an ad hoc way (i.e. you can't cheat by taking a direct dependency on something inside a different feature), but both Features can use Core
* Feature 3 doesn’t use Core but Core will still be in Master Deployment 2 due to Feature 1

Note that the deployable regions for Feature 1, 2, and 3 aren't actually deployed alone, but they are adding enforcement that makes the features cleaner

Because of how upgrades work, “Core” could have its own upgrader. Then when you create the upgrader for Master Deployment 1 and 2 you can specify “exclude Core” in which case those tables are assumed to be updated independently. You could create as many or as few independently upgrade-able things with this pattern. Because regions are not allowed to "peek" inside of a deployable region, you can reorganize your logical regions without breaking other parts of the schema.

#### Private Regions

The above constructs create a good basis for creating and composing regions, but a key missing aspect is the ability to hide internal details in the logical groups.  This becomes increasingly important as your desire to modularize schema grows; you will want to have certain parts that can change without worrying about breaking others and without fear that there are foreign keys and so forth to them.

To accomplish this CQL provides the ability to compose schema regions with the optional `private` keyword.  In the following example there will be three regions creatively named `r1`, `r2`, and `r3`.  Region `r2` consumes `r1` privately and therefore `r3` is not allowed to use things in `r1` even though it consumes `r2`.  When creating an upgrade script for `r3` you will still need (and will get) all of `r2` and `r1`, but from a visibility perspective `r3` can only directly depend on `r2`.

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

So, in summary, to get true privacy first make whatever logical regions you like that are helpful.  Put privacy where you need/want it.  Import logical regions as much as you want in your own bundle of regions.  Then wrap that bundle up in a deployable region (they nest) and then your private regions are safe from unwanted usage.


<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 11. Previous Schema Validation

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
* there's probably more I forgot :D

When checking `@recreate` tables against the previous schema version for errors, these checks are done:

* suppress checking of any table facet changes in previous schema on recreate tables, you can do anything you want
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

This file looks maybe something like this:

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

So here the old version of `foo` will be validated against the new version and all is well.  A new nullable text field was added at the end.

In practice these comparisons are liklely to be done in a somewhat more maintainable way, like this:

```sql
-- prev_check.sql
#include "table1.sql"
#include "table2.sql"
#include "table3.sql"

@previous_schema;

#include "previous.sql"
```

Now importantly in this configuration, everything that follows the `@previous_schema` directive does not actually contribute to
the declared schema.  Which means the `--rt schema` result type will not see it.   Because of this you can do your checking
operation like so:

```bash
cc -E -x c prev_check.sql | cql --cg new_previous_schema.sql --rt schema 
```

The above command will generate the schema in new_previous_schema and, if this command succeeds, it's safe to replace the existing
`previous.sql` with `new_previous_schema`.

NOTE: you can bootstrap the above by leaving off the `@previous_schema` and what follows to get your first previous schema from the command above.

Now as, you can imagine, comparing against the previous schema allows many more kinds of errors to be discovered.
What follows is a large chuck of the CQL tests for this area taken from the test files themselves.  
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
The table `foo` is the same!  Doesn't get any easier than that.

#### Case 2 : table create version changed
```sql
create table t_create_verison_changed(id integer) @create(1);
-------
create table t_create_verison_changed(id integer) @create(2);

Error at sem_test_prev.sql:15 : in str : current create version not equal to
previous create version for 't_create_verison_changed'
```
You can't change the version a table was created in.  Here the new schema says it appeared in version 1.  The old schema says 2.

#### Case 3 : table delete version changed
```sql
create table t_delete_verison_changed(id integer) @delete(1);
-------
create table t_delete_verison_changed(id integer) @delete(2);

Error at sem_test_prev.sql:18 : in str : current delete version not equal to
previous delete version for 't_delete_verison_changed'
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
No errors here, regular delete.

#### Case 8: column name changed
```sql
create table t_column_name_changed(id_ integer);
-------
create table t_column_name_changed(id integer);

Error at sem_test_prev.sql:33 : in str : column name is different between previous and
current schema 'id_'
```
You can't rename columns.  We could support this but it's a bit of maintenance nightmare and logical renames are possible easily without doing physical renames.

#### Case 9 : column type changed
```sql
create table t_column_type_changed(id real);
-------
create table t_column_type_changed(id integer);

Error at sem_test_prev.sql:36 : in str : column type is different between previous
and current schema 'id'
```
Can't change the type of a column.

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

You can't change the default value after the fact.  There's no alter statement that would allow this even though it makes some logical sense.

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
No errors here, properly created new table.

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
Adding a column in the new version and marking it both create and delete is ... weird... don't do that.  You can do it but you have to do it one step at a time.

#### Case 26 : add columns to table, marked `@create` correctly
```sql
create table t_new_legit_column(a int not null, b int @create(6));
-------
create table t_new_legit_column(a int not null);
```
No errors here, new column added in legit version.

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
Here the view was deleted rather than marking it with `@delete`.

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

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 12. Testability Features
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

If you omit the `@dummy_nullables` then any nullable fields will be null as usual.  And likewise if you omit `@dummy_defaults` then any fields with a default value will use thatt value as usual.  You might want any combination of those for your tests (null values are handy in your tests and default behavior is also handy).

The `@dummy_seed` expression provided can be anything that resolves to a non-null integer value, so it can be pretty flexible.  You might use a `while` loop to insert a bunch of 
rows with the seed value being computed from the `while` loop variable.

The form above is sort of like `insert * into table` in that it is giving dummy values for all columns but you can also specify some of the columns while using the seed value for others.  Importantly, you can specify values you particularly want to control either for purposes of creating a more tailored test or because you need them
to match existing or created rows in a table referenced by a foreign key.

As an example:

```sql
insert into users (id) values (1234) @dummy_seed(123)
   @dummy_nullables @dummy_defaults;
```
Will provide dummy values for everything but the `id` column.

#### Using `WITH RECURSIVE`


Sometimes what you want to do is create a dummy result set without necessarly popuplating the database at all.  If you have code
that consumes a result set of a particular shape it's easy enough to create a fake result set with a pattern something like this:

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
`INSERT ... FROM SELECT...` to create dummy data in real tables.   And of course once you have a core query you could use it in a variety of ways 
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

Could be very useful.  Many alternatives are possible.

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

Naturally real columns have much longer names and there are often a lot more than 10.

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

```C
void generate_sample_proc_row_fetch_results(
    generate_sample_proc_row_rowset_ref _Nullable *_Nonnull result_set, 
    string_ref _Nonnull foo_, 
    int64_t bar_);
```

These few test helpers are useful in a variety of scenarios and can save you a lot of typing and maintenance.  They evolve automatically as the code
changes, always matching the signature of the attributed procedure.

#### Generalized Dummy Test Pattern
The most flexible test helper is the `dummy_test` form.  This is far more advanced than the simple helpers above.  While the choices above were designed to help you create fake result sets pretty easily, `dummy_test` goes much further letting you set up arbitrary schema and data so that you can run your procedure on actual data.  The `dummy_test` code generator uses the features above to do its job and like the other autotest options, it works by automatically generating CQL code from your procedure definition.  However, you get a lot more code in this mode.  It's easiest to study an example so let's begin there.

To understand `dummy_test` we'll need a more complete example, so let's start with this simple two-table schema with a trigger and some indices. To this we add a very small procedure that we might want to test.

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

As you can see, we have two tables `foo` and `bar`; the `foo` table has a trigger;  both `foo` and `bar` have indices.  This schema is very simple, but of course it could be a lot more complicated, and real cases typically are.

The procedure we want to test is creatively called `the_subject`.  It has lots of test attributes on it.  We've already discussed `dummy_table`, `dummy_insert`, `dummy_select`, and `dummy_result_set` above but as you can see they can be mixed in with `dummy_test`.  Now let's talk about `dummy_test`.  First you'll notice that annotation has additional sub-attributes;  The attribute grammar is sufficiently flexible that,  in principle, you could represent an arbitary LISP program, so the instructions can be very detailed.  In this case the attribute provides table and column names, as well as sample data.  We'll discuss that when we get to the population code.

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

That covers what we had before, so, what's new?  Actually quite a bit.  We'll begin with the easiest:

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

If there are no triggers or indices the corresponding create/drop methods will not be generated.

With these helpers available, when writing test code you can then choose if you want to create just the tables, or the tables and indices, or tables and indices and triggers by invoking the appropriate combination of helper methods.  Since all the implicated triggers and indices are automatically included, even if they change over time, maintenance is greatly simplified.

Note that in this case the code simply reads from one of the tables but in general the procedure under test might make modifications as well.  Test code frequently has to read back the contents of the tables to verify that they were modified correctly.  So these additional helper methods are also included:

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

Those procedures will allow you to easily create result sets with data from the relevant tables which can then be verified for correctness.  Of course if more tables were implicated those would have been included as well.

As you can see the naming always follows the convention `test_[YOUR_PROCEDURE]_[helper_type]`

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

In general, the `dummy_test` annotation can include any number of tables, for each table you can specify any of the columns and you can have any number of tuples of values for those columns.

NOTE: if you include primary key and/or foreign key columns among the explicit values it's up to you to ensure that they are valid combinations.  SQLite will complain as usual if they are not, but the CQL compiler will simply emit the data you asked for.

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

And of course if the annotation is not flexible enough, you can write your own data population as usual.

The CQL above results in the usual C signatures.  For instance:

```C
CQL_WARN_UNUSED cql_code test_the_subject_populate_tables(sqlite3 *_Nonnull _db_);
```

So it's fairly easy to call from C/C++ test code or from CQL test code.

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 13. JSON Output

To help facilitate additional tools that might want to depend on CQL input files further down the toolchain CQL includes a JSON output format for SQL DDL as well as stored procedure information, including special information for a single-statement DML.  "Single-statement DML" refers to those stored procedures that that consist of a single `insert`, `select`, `update`, or `delete`.   Even though such procedures are just one statement, good argument binding can create very powerful DML fragments that are re-usable.  Many CQL stored procedures are of this form (in practice maybe 95% are just one statement).

Below are some examples of the JSON output taken from a CQL test file.  Note that the JSON has free text inserted into it as part of the test output, that obviously doesn't appear in the final output but it is especially illustrative here.  This example illustrates almost all the possible JSON fragments.

```
{
  "tables" : [
    
```
Each table appears fully formed in its own JSON hunk as below.  `isAdded` and `isDeleted` correspond to the presence of an `@create` or `@delete` annotation respectively.

```
 
    CREATE TABLE Foo(
      id INTEGER NOT NULL,
      name TEXT
    )
    
    {
      "name" : "Foo",
      "temp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 0,
      "isDeleted" : 0,
      "columns" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1,
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
      "foreignKeys" : [
      ],
      "uniqueKeys" : [
      ],
      "indices" : [ "region_0_index", "MyIndex", "MyOtherIndex" ]
    },
    
```
Here we introduce a primary key and its JSON.
```
    
    CREATE TABLE T2(
      id INTEGER PRIMARY KEY
    )
    
    {
      "name" : "T2",
      "temp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 0,
      "isDeleted" : 0,
      "columns" : [
        {
         ...
        }
      ],
      "primaryKey" : [ "id" ],
      ...
    },
    
```
General purpose column information is also present.  Again a fragment for brevity.
```
    
    CREATE TABLE T3(
      id INTEGER UNIQUE AUTOINCREMENT
    )
    
    {
      "name" : "T3",
       ...
      "columns" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 1,
          "isAutoIncrement" : 1
        }
      ],
     ...
    },
    
```
Columns and tables can have flexible attributes which are used downstream.
```
    
    @ATTRIBUTE(foo=bar)
    CREATE TABLE T4(
      @ATTRIBUTE(cool)
      id INTEGER
    )
    
    {
      "name" : "T4",
      ...
      "columns" : [
        {
          "name" : "id",
          "attributes" : [
            {
              "name" : "cool",
              "value" : 1
            }
          ],
          "type" : "integer",
           ...
        }
      ],
      ...
      "attributes" : [
        {
          "name" : "foo",
          "value" : "bar"
        }
      ]
    },
    
    
```
Here's an example with revision marks
```
    CREATE TABLE T8(
      id INTEGER
    ) @CREATE(1) @DELETE(3)
    
    {
      "name" : "T8",
      "temp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 1,
      "isDeleted" : 1,
      "columns" : [
        {
          "name" : "id",
          "type" : "integer",
          ...
        }
      ],
      ...
    },
    
```
The usual constraints are also recorded.   This example has a unqiue key on a column and foreign keys.  Note that the unique key is reported the same as if it had been declared in a standalone fashion.  There is a lot of stuff in this table...
```    
    CREATE TABLE T10(
      id1 INTEGER UNIQUE,
      id2 INTEGER,
      id3 INTEGER,
      id4 INTEGER UNIQUE,
      PRIMARY KEY (id1, id2),
      FOREIGN KEY (id1, id2) REFERENCES T9 (id2, id1),
      CONSTRAINT uk1 UNIQUE (id2, id3),
      CONSTRAINT uk2 UNIQUE (id3, id4)
    )
    
    {
      "name" : "T10",
      ...
      "columns" : [
        {
          "name" : "id1",
          "type" : "integer",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 1,
          "isAutoIncrement" : 0
        },
        {
          "name" : "id2",
          "type" : "integer",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        },
        {
          "name" : "id3",
          "type" : "integer",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        },
        {
          "name" : "id4",
          "type" : "integer",
          "isNotNull" : 0,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 1,
          "isAutoIncrement" : 0
        }
      ],
      "primaryKey" : [ "id1", "id2" ],
      "foreignKeys" : [
        {
          "columns" : [ "id1", "id2" ],
          "referenceTable" : "T9",
          "referenceColumns" : [ "id2", "id1" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "NO ACTION",
          "isDeferred" : 0
        }
      ],
      "uniqueKeys" : [
        {
          "name" : "id1_uk",
          "columns" : [ "id1" ]
        },
        {
          "name" : "id4_uk",
          "columns" : [ "id4" ]
        },
        {
          "name" : "uk1",
          "columns" : [ "id2", "id3" ]
        },
        {
          "name" : "uk2",
          "columns" : [ "id3", "id4" ]
        }
      ]
    },
    
```
Foreign keys can include the full set of actions.  Here are a couple of examples:
```
    
    CREATE TABLE T11(
      id1 INTEGER,
      id2 INTEGER,
      id3 INTEGER,
      FOREIGN KEY (id1) REFERENCES T9 (id1) ON DELETE CASCADE,
      FOREIGN KEY (id1) REFERENCES T9 (id1) ON UPDATE SET NULL
    )
    
    {
      "name" : "T11",
       ...
      "columns" : [
        {
          "name" : "id1",
          "type" : "integer",
          ...
        },
        {
          "name" : "id2",
          "type" : "integer",
           ...
        },
        {
          "name" : "id3",
          "type" : "integer",
          ...
        }
      ],
      "primaryKey" : [  ],
      "foreignKeys" : [
        {
          "columns" : [ "id1" ],
          "referenceTable" : "T9",
          "referenceColumns" : [ "id1" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "CASCADE",
          "isDeferred" : 0
        },
        {
          "columns" : [ "id1" ],
          "referenceTable" : "T9",
          "referenceColumns" : [ "id1" ],
          "onUpdate" : "SET NULL",
          "onDelete" : "NO ACTION",
          "isDeferred" : 0
        }
      ],
      ...
    },
    
```
Deferred FK actions can also be specified.  Note: per the SQLite documentation, the norm is immediate on everything except `deferrable initially deferred`.
```
    
    CREATE TABLE T12(
      id1 INTEGER,
      id2 INTEGER,
      id3 INTEGER,
      FOREIGN KEY (id1) REFERENCES T9 (id1) ON DELETE SET DEFAULT
       DEFERRABLE INITIALLY DEFERRED,
      FOREIGN KEY (id2) REFERENCES T9 (id1) ON UPDATE NO ACTION
    )
    
    {
      "name" : "T12",
      ...
      "columns" : [
        {
          "name" : "id1",
          ...
        },
        {
          "name" : "id2",
          ...
        },
        {
          "name" : "id3",
           ...
        }
      ],
      ...
      "foreignKeys" : [
        {
          "columns" : [ "id1" ],
          "referenceTable" : "T9",
          "referenceColumns" : [ "id1" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "SET DEFAULT",
          "isDeferred" : 1
        },
        {
          "columns" : [ "id2" ],
          "referenceTable" : "T9",
          "referenceColumns" : [ "id1" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "NO ACTION",
          "isDeferred" : 0
        }
      ],
    ...
    },
    
```
Just like unique keys, foreign keys on the columns are moved down as though they had been independently declared.  There are 3 foreign keys below.
```   
    CREATE TABLE with_fk_on_columns(
      id1 INTEGER NOT NULL REFERENCES T2 (id) ON UPDATE CASCADE
        DEFERRABLE INITIALLY DEFERRED,
      id2 INTEGER NOT NULL REFERENCES T10 (id4) ON DELETE CASCADE,
      FOREIGN KEY (id1, id2) REFERENCES T10 (id3, id4)
    )
    
    {
      "name" : "with_fk_on_columns",
      "temp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 0,
      "isDeleted" : 0,
      "columns" : [
        {
          "name" : "id1",
          "type" : "integer",
          "isNotNull" : 1,
           ...
        },
        {
          "name" : "id2",
          "type" : "integer",
          "isNotNull" : 1,
           ...
        }
      ],
      "primaryKey" : [  ],
      "foreignKeys" : [
        {
          "columns" : [ "id1" ],
          "referenceTable" : "T2",
          "referenceColumns" : [ "id" ],
          "onUpdate" : "CASCADE",
          "onDelete" : "NO ACTION",
          "isDeferred" : 1
        },
        {
          "columns" : [ "id2" ],
          "referenceTable" : "T10",
          "referenceColumns" : [ "id4" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "CASCADE",
          "isDeferred" : 0
        },
        {
          "columns" : [ "id1", "id2" ],
          "referenceTable" : "T10",
          "referenceColumns" : [ "id3", "id4" ],
          "onUpdate" : "NO ACTION",
          "onDelete" : "NO ACTION",
          "isDeferred" : 0
        }
      ],
      "uniqueKeys" : [
      ]
    }
```
Columns can be marked with `@sensitive` for privacy reasons.  This declaration flows to the column description as `isSensitive`.  For economy, `isSenstive` is only emitted when true.

```
    CREATE TABLE radioactive(
      id INTEGER NOT NULL,
      danger TEXT @SENSITIVE
    )

   {
      "name" : "radioactive",
      "isTemp" : 0,
      "ifNotExists" : 0,
      "withoutRowid" : 0,
      "isAdded" : 0,
      "isDeleted" : 0,
      "isRecreated": 0,
      "columns" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        },
        {
          "name" : "danger",
          "type" : "text",
          "isNotNull" : 0,
          "isSensitive" : 1,
          "isAdded" : 0,
          "isDeleted" : 0,
          "isPrimaryKey" : 0,
          "isUniqueKey" : 0,
          "isAutoIncrement" : 0
        }
      ],
      "primaryKey" : [  ],
      "foreignKeys" : [
      ],
      "uniqueKeys" : [
      ]
    }
  ],
```
The next major section is the views.  Each view includes its projection (that is the net columns it creates from the select clause) and its general statement information.  One example tells the story pretty clearly.  Views don't have arguments in any supported cases but the arguments are included for symmetry with the other forms.  Note: projection columns can be sensitive and will be so-marked if they are.
```
  "views" : [
    
    CREATE VIEW MyView AS
    SELECT *
      FROM Foo
    
    {
      "name" : "MyView",
      "temp" : 0,
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "select" : "SELECT id, name FROM Foo",
      "selectArgs" : [  ]
    }
  ],

```
Likewise indices contain the table and indexed columns.  This one example illustrates things fairly clearly. 
```
  "indices" : [
    
    CREATE UNIQUE INDEX IF NOT EXISTS MyIndex ON Foo (name DESC, id ASC)
    
    {
      "name" : "MyIndex",
      "table" : "Foo",
      "isUnique" : 1,
      "ifNotExists" : 1,
      "columns" : [ "name", "id" ],
      "sortOrders" : [ "desc", "asc" ]
    }
  ],
```
The top level attributes go, by convention, on a the global variable named `database` of type `object`.  These attributes move into the JSON.  Other globals are ignored. 

NOTE: attributes are very flexible, allowing nesting of arrays.  Attributes values can either be any literal, or a name, or an array of values, recursively.
```
  
  @ATTRIBUTE(my_other_attribute=('any', ('tree', 'of'), 'values'))
  @ATTRIBUTE(dbname='fred.sql')
  @ATTRIBUTE(dbfile='cg_test_mlite_query.sql')
  DECLARE database OBJECT
  
  "attributes" : [
    {
      "name" : "my_other_attribute",
      "value" : ["any", ["tree", "of"], "values"]
    },
    {
      "name" : "dbname",
      "value" : "fred.sql"
    },
    {
      "name" : "dbfile",
      "value" : "cg_test_mlite_query.sql"
    }
  ],

```
The queries section corresponds to the stored procedures with a SELECT statement.  There is significant data provided about each one.

* the name of the procedure
* the number and type of arguments
* the set of tables used anywhere in the query (for dependencies)
  * this includes tables used within views that were used
* the query projection (aka the result shape of the select)
* the full SQL statement and the arguments that should be bound to each `?`

There are two examples below: 

```
  "queries" : [
 
    
    CREATE PROC a_query (pattern TEXT NOT NULL, reject TEXT)
    BEGIN
    SELECT id
      FROM Foo
      WHERE name LIKE pattern AND name <> reject;
    END
    
    {
      "name" : "a_query",
      "args" : [
        {
          "name" : "pattern",
          "type" : "text",
          "isNotNull" : 1
        },
        {
          "name" : "reject",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [ "Foo" ],
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1
        }
      ],
      "statement" : "SELECT id FROM Foo WHERE name LIKE ? AND name <> ?",
      "statementArgs" : [ "pattern", "reject" ],
    },
    
    
    CREATE PROC bigger_query (pattern TEXT NOT NULL, reject TEXT)
    BEGIN
    SELECT DISTINCT *
      FROM Foo
      WHERE name LIKE pattern AND name <> reject
      GROUP BY name
      HAVING name > reject
      ORDER BY pattern
      LIMIT 1
      OFFSET 3;
    END
    
    {
      "name" : "bigger_query",
      "args" : [
        {
          "name" : "pattern",
          "type" : "text",
          "isNotNull" : 1
        },
        {
          "name" : "reject",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [ "Foo" ],
      "projection" : [
        {
          "name" : "id",
          "type" : "integer",
          "isNotNull" : 1
        },
        {
          "name" : "name",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "statement" : "SELECT DISTINCT id, name FROM Foo WHERE name LIKE ? AND 
          name <> ? GROUP BY name HAVING name > ? ORDER BY ? 
          LIMIT 1 OFFSET 3",
      "statementArgs" : [ "pattern", "reject", "reject", "pattern" ],
    },
    
```
The section on insert statements is very similar in shape.  Again the fields are:

* the name of the procedure
* the arguments and argument types
* the tables used by the insert statement (usually just the one but value expressions can be select statements so it can be more)
* the table we are inserting into (certainly present in `usesTables`) 
* the overall statement and its arguments (easiest form to use)
* the statement type (e.g. `INSERT` or `INSERT OR REPLACE`)
* the inserted columns 
  * present even if the `insert into table values (...)` form was used
* the array of value expressions and arguments, one for each value

Again, simple insert forms are readily recognized and complex forms are supported.
```
  "inserts" : [
    
    The statement ending at line 277
    
    CREATE PROC insert_proc (id_ INTEGER NOT NULL, name_ TEXT)
    BEGIN
    INSERT OR REPLACE INTO Foo (id, name) VALUES (id_, name_);
    END
    
    {
      "name" : "insert_proc",
      "args" : [
        {
          "name" : "id_",
          "type" : "integer",
          "isNotNull" : 1
        },
        {
          "name" : "name_",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [ "Foo" ],
      "table" : "Foo",
      "statement" : "INSERT OR REPLACE INTO Foo (id, name) VALUES (?, ?)",
      "statementArgs" : [ "id_", "name_" ],
      "statementType" : "INSERT OR REPLACE",
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
    },
    
```
As another example, this fairly easy to write CQL transparently creates dummy values.  Great for use in testing.  The JSON shows the net insert created from the original source below.
```
    
    CREATE PROC dummy_insert_proc (seed_ INTEGER NOT NULL)
    BEGIN
    INSERT INTO Foo () VALUES () @DUMMY_SEED(seed_) @DUMMY_NULLABLES;
    END
    
    {
      "name" : "dummy_insert_proc",
      "args" : [
        {
          "name" : "seed_",
          "type" : "integer",
          "isNotNull" : 1
        }
      ],
      "usesTables" : [ "Foo" ],
      "table" : "Foo",
      "statement" : "INSERT INTO Foo (id, name) 
            VALUES (?, printf('name_%d', ?))",
      "statementArgs" : [ "_seed_", "_seed_" ],
      "statementType" : "INSERT",
      "columns" : [ "id", "name" ],
      "values" : [
        {
          "value" : "?",
          "valueArgs" : [ "_seed_" ]
        },
        {
          "value" : "printf('name_%d', ?)",
          "valueArgs" : [ "_seed_" ]
        }
      ]
    }
  ],

```
The above form can capture the simplest of the insert statements allowed in SQLite.  This is especially
interesting because the JSON above can cleanly capture each value and the only place where there might be
references to the procedure arguments is in the `valueArgs` portion. There is simply no room for any other
kind of variability.   As a result, it's actually possible to take this type of insert and potentially
re-codegen it into an upsert or something else starting from the JSON.  This is isn't in general possible
with the other forms of insert.  More compilicated forms of insert go into a section called "generalInesrts"
this includes any other single insert statement such as these forms:

 * insert from multiple value rows
 * insert from a select statement
 * insert using a `WITH` clause
 * insert using the upsert clause

The "generalInserts" section looks exactly like the "inserts" section except that it does not include "values".

Here's an example:

```
  "generalInserts" : [

   CREATE PROC insert_compound ()
    BEGIN
    INSERT INTO T3(id) VALUES(1)
    UNION ALL
    SELECT 1 AS column1;
    END

    {
      "name" : "insert_compound",
      "definedInFile" : "cg_test_json_schema.sql",
      "args" : [
      ],
      "insertTables" : [ "T3" ],
      "usesTables" : [ "T3" ],
      "table" : "T3",
      "statement" : "INSERT INTO T3(id) VALUES(1) UNION ALL SELECT 1 AS column1",
      "statementArgs" : [  ],
      "statementType" : "INSERT",
      "columns" : [ "id" ]
    }
    ...
  ],
```


Update statements are handled very much like the others, but there are no statement fragments.  You get these pieces:
* the name of the procedure and its arguments
* dependency information
* the statement text and its arguments

This is the minimum information needed to bind and run the statement.  Note that arguments can be in any part of the update.
```
  "updates" : [
    
    The statement ending at line 306
    
    CREATE PROC update_proc (id_ INTEGER NOT NULL, name_ TEXT)
    BEGIN
    UPDATE Foo
    SET name = name_
      WHERE id = id_
      ORDER BY name
      LIMIT 1;
    END
    
    {
      "name" : "update_proc",
      "args" : [
        {
          "name" : "id_",
          "type" : "integer",
          "isNotNull" : 1
        },
        {
          "name" : "name_",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [ "Foo" ],
      "table" : "Foo",
      "statement" : "UPDATE Foo SET name = ? 
              WHERE id = ? ORDER BY name LIMIT 1",
      "statementArgs" : [ "name_", "id_" ]
    }
  ],

```
The delete section looks exactly like the update section.
* procedure name and arguments
* dependency information
* statement and arguments
```
  "deletes" : [
    
    The statement ending at line 297
    
    CREATE PROC delete_proc (name_ TEXT)
    BEGIN
    DELETE FROM Foo WHERE name LIKE name_;
    END
    
    {
      "name" : "delete_proc",
      "args" : [
        {
          "name" : "name_",
          "type" : "text",
          "isNotNull" : 0
        }
      ],
      "usesTables" : [ "Foo" ],
      "table" : "Foo",
      "statement" : "DELETE FROM Foo WHERE name LIKE ?",
      "statementArgs" : [ "name_" ]
    }
  ],

```
And finally the section for procedures that were encountered that are not one of the simple prepared statement forms.  The principle reasons for being in this category are:
* the procedure has out arguments
* the procedure uses something other than a single DML statement
* the procedure has no projection (no result of any type)
```
  "general" : [
    
    CREATE PROC with_complex_args (OUT pattern TEXT NOT NULL, INOUT arg REAL)
    BEGIN
      SELECT 1 AS a;
    END
    
    {
      "name" : "with_complex_args",
      "args" : [
        {
          "name" : "pattern",
          "type" : "text",
          "isNotNull" : 1,
          "binding" : "out"
        },
        {
          "name" : "arg",
          "type" : "real",
          "isNotNull" : 0,
          "binding" : "inout"
        }
      ],
      "usesTables" : [  ],
      "hasSelectResult" : 1,
      "projection" : [
        {
          "name" : "a",
          "type" : "integer",
          "isNotNull" : 1
        }
      ],
      "usesDatabase" : 1
    },
    
    
    CREATE PROC atypical_noreturn ()
    BEGIN
      DECLARE C CURSOR LIKE SELECT 1 AS A;
    END
    
    {
      "name" : "atypical_noreturn",
      "args" : [
      ],
      "usesTables" : [  ],
      "usesDatabase" : 0
    },
    
  
    CREATE PROC typical_outresult ()
    BEGIN
      DECLARE C CURSOR LIKE SELECT 1 AS A; 
      FETCH C (A) FROM VALUES (7);    
      OUT C;
    END
    
    {
      "name" : "typical_outresult",
      "args" : [
      ],
      "usesTables" : [  ],
      "hasOutResult" : 1,
      "projection" : [
        {
          "name" : "A",
          "type" : "integer",
          "isNotNull" : 1
        }
      ],
      "usesDatabase" : 0
    },
    
```

Some additional properties not mentioned above that are worth noting:

* where `usesTables` appears there will also be more detailed information about how the tables were used
  * the `insertTables` key will give you an array of the tables that were used as the target of an `insert` statement
  * the `updateTables` key will give you an array of the tables that were used as the target of an `update` statement
  * the `deleteTables` key will give you an array of the tables that were used as the target of an `delete` statement
  * the `fromTables` key will give you an array of tables that were used the the `from` clause of a select or some other `select`-ish context in which you only read from the table
* the `usesProcedures` key for a given proc has an array of the procedures it calls, this allows for complete dependency analysis if needed 


To use cql in this fashion:

```
cql --in input.sql --rt json_schema --cg out.json
```

NOTE: `@ATTRIBUTE` can be applied any number of times to the entities here, including the procedures (i.e. immediately before the `CREATE PROCEDURE`) .  Those attributes appear in the JSON in an optional `attributes` chunk.  Attributes are quite flexible (you can easily encode a lisp program in attributes if you were so inclined) so you can use them very effectively to annotate your CQL entities as needed for downstream tools.

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## 14. CQL Query Fragments

CQL Query fragments are the most sophisticated rewrite CQL offers for productivity.  The idea is that a very large query
can be represented in "fragments" that add columns or add rows based on the original "core" query.  The final query
will be an assembled rewrite of all the fragments chained together.  Specifically, the motivation for this is that you
can have a "core" query that fetches the essential columns for some UI design and then you can add query extension
fragments that add new/additional columns for some new set of features.  The core and extended columns can be in their
own fragment and they can be compiled independently.  The result of this is that any errors are in much smaller
and easier to understand fragments rather than in some monster "fetch everything" query;  any given extension does not
have to know all the details of all the other extensions and can take a limited dependency on even the core query.

It's easiest to illustrate this with an example so let's begin there.

Let's first start with this very simple schema.

```
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

```
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
* the procedure must consiste of exactly one `with...select` statement
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
* the procedure name can be any unique name other than `base_frag`, it corresponds to this particular extension's purpose
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
  * it must select from the base fragment name and left outer join to whereever it likes to get optional additional columns
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

The second form of extension allows for this, it is similarly locked in form.  Here is an example:

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
* there is a manatory second CTE
* the second CTE is a compound query with any number of branches, all `union all`
* the first branch must be `select * from base_frag` (the base fragment) to ensure that the original rows remain
  * this is also why all the branches must be `union all`
* this form cannot add new columns
* the extension CTE may not include `order by` or `limit` because that might reorder or remove rows of the base
* any extensions of this form must come before those of the `left outer join` form for a given base fragment
  * which ironically means `row_adder_frag` has to come before `col_adder_frag`
* the usual restrictions on compound selects (same type and number of columns) ensure a consistent result
* the final select after the CTE section must exactly in the form `select * from row_adder_frag` which is the name of the one and only additional CTE with no other clauses or options
  * in practice only the CTE will be used to create the final assembly so even if you did change the final select to something else it would be moot

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

Which gives you access to the core columns.  Again this fragment can and should be compiled standalone with only the declaration
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

It will always be as simple as this, all the complexity is in the fragments.

* the `assembly_fragment` name must match the core fragment name
* the procedure arguments must be identical to the base fragment arguments
* the  procedure must have the same name as the assembly fragment (`base_frag` in this case)
  * the code that was generated for the previous fragments anticipates this and makes reference to what will be generated here
  * this is enforced
* the assembled query is what you run to get the result set, this has real code behind it
  * the other fragments only produce result set readers that call into the helper meethods to get columns
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

Let's dissect this part by part, each CTE serves a purpose.

* the core CTE was replaced by the CTE in the base_fragment, it appears directly
* next the first extension was added as a CTE referring to the base fragment just as before
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

<div style="page-break-after: always; visibility: hidden"></div>


<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## Appendix 1 : Command Line Options

CQL has a variety of command line options but many of them are only interesting for cql development.  Nonetheless this is a comprehensive list:

### no-options
* CQL reads its input and echos it back as normalized SQL
* this only validates that the input can be parsed
* note CQL is often used after the c pre-processor is run so this kind of invocation is typical:

```
cc -E -x c foo.sql | cql [args]
```
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

### --print
* may be combined with --sem (semantic info will be included)
* prints the internal AST to stdout instead of echoing the inputs
Example
```
cql --in sem_test.sql --sem --print >sem_ast.out
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
* that often makes the outputs badly formed so this is generally good for humans only

### --java_package_name
* used by java code generators when they output a class. Allows to specify the name of package the class will be a part of

### --c_include_namespace
* for the C codegen runtimes, it determines the header namespace (as in #include <namespace/file.h) that the headers will have to be referred when included from other sources.

### --objc_c_include_path
* for ObjC codegen runtimes that need to refer to the generated C code, this represents the header of the C generated code that will be used during inclusion from the ObjC file

# Result Types

### --rt c
* requires two output files (foo.h and foo.c)
* this is the standard C compilation of the sql file

### --rt objc
* objective C wrappers for result sets produced by the stored procedures in the input
* these depend on the output of a standard codegen run so this is additive
* requires one output file (foo.h)

### --rt java
* java wrappers for result sets produced by the stored procedures in the input
* these depend on the output of a standard codegen run so this is additive
* requires one output file (foo.java)

### --rt schema
* produces the canonical schema for the given input files
* stored procedures etc. are removed
* whitespace etc. is removed
* suitable for use to create the next or first "previous" schema for schema validation
* requires one output file

### --rt schema_upgrade
* produces a CQL schema upgrade script (one file) which can then be compiled with CQL itself
* see the section on schema upgrade/migration
* requires one output file (foo.sql)

### --include_regions a b c
* the indicated regions will be declared
* used with `--rt schema_upgrade` or `--rt schema`
* in the upgrade case excluded regions will not be themselves upgraded, but may be referred, to by things that are being upgraded

### --exclude_regions x y z
* the indicated regions will still be declared but the upgrade code will be suppressed, the presumption being that a different script already upgrades x y z
* used with `--rt schema_upgrade`

### --rt json_schema
* produces JSON output suitable for consumption by downstream codegen like the android mlite system
* the JSON includes a definition of the various entities in the input
* see the section on JSON output for details

<div style="page-break-after: always; visibility: hidden"></div>


<div style="page-break-after: always; visibility: hidden"></div>

<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## Appendix 2: CQL Grammar

What follows is taken from a grammar snapshot with the tree building rules removed.
It should give a fair sense of the syntax of CQL (but not semantic validation).

Snapshot as of Fri Oct  9 13:41:39 PDT 2020

### Operators and Literals

These are in order of priority lowest to highest

```
UNION_ALL UNION INTERSECT EXCEPT
ASSIGN
OR
AND
BETWEEN
NOT
'<>' '!=' '=' '==' LIKE NOT_LIKE GLOB MATCH REGEXP IN IS_NOT IS
'<' '>' '>=' '<='
'<<' '>>' '&' '|'
'+' '-'
'*' '/' '%'
UMINUS '~' COLLATE
CONCAT
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
EXCLUDE_GROUP EXCLUDE_CURRENT_ROW EXCLUDE_TIES EXCLUDE_NO_OTHERS CURRENT_ROW UNBOUNDED PRECEDING FOLLOWING
CREATE DROP TABLE WITHOUT ROWID PRIMARY KEY NULL_ DEFAULT AT_DUMMY_SEED
OBJECT TEXT BLOB LONG_ INT_ INTEGER LONG_INTEGER REAL ON UPDATE CASCADE ON_CONFLICT DO NOTHING
DELETE INDEX FOREIGN REFERENCES CONSTRAINT UPSERT STATEMENT
INSERT INTO VALUES VIEW SELECT QUERY_PLAN EXPLAIN OVER WINDOW FILTER PARTITION RANGE ROWS GROUPS
AS CASE WHEN FROM THEN ELSE END LEFT
OUTER JOIN WHERE GROUP BY ORDER ASC
DESC INNER FCOUNT AUTOINCREMENT DISTINCT
LIMIT OFFSET TEMP TRIGGER IF ALL CROSS USING RIGHT
UNIQUE HAVING SET TO DISTINCTROW
FUNC FUNCTION PROC PROCEDURE BEGIN_ OUT INOUT CURSOR CURSOR_FOR DECLARE FETCH LOOP LEAVE CONTINUE
OPEN CLOSE ELSE_IF WHILE CALL TRY CATCH THROW RETURN
SAVEPOINT ROLLBACK COMMIT TRANSACTION RELEASE ARGUMENTS
CAST WITH RECURSIVE REPLACE IGNORE ADD COLUMN RENAME ALTER
AT_ECHO AT_CREATE AT_RECREATE AT_DELETE AT_SCHEMA_UPGRADE_VERSION AT_PREVIOUS_SCHEMA AT_SCHEMA_UPGRADE_SCRIPT
AT_FILE AT_ATTRIBUTE AT_SENSITIVE DEFERRED NOT_DEFERRABLE DEFERRABLE IMMEDIATE RESTRICT ACTION INITIALLY NO
BEFORE AFTER INSTEAD OF FOR_EACH_ROW EXISTS RAISE FAIL ABORT AT_ENFORCE_STRICT AT_ENFORCE_NORMAL
AT_BEGIN_SCHEMA_REGION AT_END_SCHEMA_REGION
AT_DECLARE_SCHEMA_REGION AT_DECLARE_DEPLOYABLE_REGION AT_SCHEMA_AD_HOC_MIGRATION PRIVATE
```
### Rules

Note that in many cases the grammar is more generous than the overall language and errors have to be checked on top of this, often this is done on purpose because even when it's possible it might be very inconvenient to do checks with syntax.  For example the grammar cannot enforce non-duplicate ids in id lists, but it could enforce non-duplicate attributes in attribute lists.  It chooses to do neither as they are easily done with semantic validation.  Thus the grammar is not the final authority on what constitutes a valid program but it's a good start.
```


program: opt_stmt_list
  ;

opt_stmt_list: /*nil*/
  | stmt_list

stmt_list: stmt ';'
  | stmt ';' stmt_list
  ;

stmt: misc_attrs any_stmt

any_stmt: select_stmt
  | explain_stmt
  | create_trigger_stmt
  | create_table_stmt
  | create_index_stmt
  | create_view_stmt
  | alter_table_add_column_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | with_delete_stmt
  | delete_stmt
  | call_stmt
  | with_insert_stmt
  | insert_stmt
  | with_update_stmt
  | update_stmt
  | update_cursor_stmt
  | upsert_stmt
  | with_upsert_stmt
  | set_stmt
  | create_proc_stmt
  | declare_proc_stmt
  | declare_func_stmt
  | declare_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_call_stmt
  | fetch_cursor_stmt
  | while_stmt
  | loop_stmt
  | leave_stmt
  | return_stmt
  | continue_stmt
  | if_stmt
  | open_stmt
  | close_stmt 
  | out_stmt 
  | out_union_stmt 
  | throw_stmt 
  | trycatch_stmt 
  | begin_trans_stmt 
  | rollback_trans_stmt 
  | commit_trans_stmt 
  | savepoint_stmt 
  | release_savepoint_stmt 
  | echo_stmt 
  | schema_upgrade_version_stmt 
  | schema_upgrade_script_stmt 
  | previous_schema_stmt 
  | enforce_strict_stmt 
  | enforce_normal_stmt 
  | declare_schema_region_stmt 
  | declare_deployable_region_stmt 
  | begin_schema_region_stmt 
  | end_schema_region_stmt 
  | schema_ad_hoc_migration_stmt 
  ; 
 
explain_stmt: "EXPLAIN" opt_query_plan explain_target  
  ; 
 
opt_query_plan: /* nil */  
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
 
previous_schema_stmt: "@PREVIOUS_SCHEMA"  
  ; 
 
schema_upgrade_script_stmt: "@SCHEMA_UPGRADE_SCRIPT"  
  ; 
 
schema_upgrade_version_stmt: "@SCHEMA_UPGRADE_VERSION" '(' "integer-literal" ')'  
  ; 
 
set_stmt: "SET" name ":=" expr    
  ; 
 
version_attrs_opt_recreate: /* nil */  
  | "@RECREATE"  
  | "@RECREATE" '(' name ')'   
  | version_attrs  
  ; 
 
opt_version_attrs: /* nil */  
  | version_attrs  
  ; 
 
version_attrs: "@CREATE" version_annotation opt_version_attrs  
  | "@DELETE" version_annotation opt_version_attrs  
  ; 
 
opt_delete_version_attr: /* nil */  
  | "@DELETE" version_annotation  
  ; 
 
drop_table_stmt: "DROP" "TABLE" "IF" "EXISTS" name  
  | "DROP" "TABLE" name  
  ; 
 
drop_view_stmt: "DROP" "VIEW" "IF" "EXISTS" name  
  | "DROP" "VIEW" name  
  ; 
 
drop_index_stmt: "DROP" "INDEX" "IF" "EXISTS" name  
  | "DROP" "INDEX" name  
  ; 
 
drop_trigger_stmt: "DROP" "TRIGGER" "IF" "EXISTS" name  
  | "DROP" "TRIGGER" name  
  ; 
 
create_table_stmt: "CREATE" opt_temp "TABLE" opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate  
  ; 
 
opt_temp: /* nil */  
  | "TEMP"  
  ; 
 
opt_if_not_exists: /* nil */  
  | "IF" "NOT" "EXISTS"  
  ; 
 
opt_no_rowid: /* nil */  
  | "WITHOUT" "ROWID"  
  ; 
 
col_key_list: col_key_def  
  | col_key_def ',' col_key_list   
  ; 
 
col_key_def: col_def 
  | pk_def 
  | fk_def 
  | unq_def 
  | "LIKE" name  
  ; 
 
col_name: name  
  ; 
 
misc_attr_key: name  
  | name ':' name  
  ; 
 
misc_attr_value_list: misc_attr_value  
  | misc_attr_value ',' misc_attr_value_list  
  ; 
 
misc_attr_value: name  
  | any_literal  
  | '(' misc_attr_value_list ')'  
  | '-' num_literal  
  ; 
 
misc_attr:  "@ATTRIBUTE" '(' misc_attr_key ')'  
  | "@ATTRIBUTE" '(' misc_attr_key '=' misc_attr_value ')'  
  ; 
 
misc_attrs: /* nil */  
  | misc_attr misc_attrs  
  ; 
 
col_def: misc_attrs col_name data_type col_attrs  
  ; 
 
pk_def: "PRIMARY" "KEY" '(' name_list ')'   
  ; 
 
opt_fk_options: /* nil */  
  | fk_options  
  ; 
 
fk_options: fk_on_options  
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
 
fk_initial_state: /* nil */  
  | "INITIALLY" "DEFERRED"  
  | "INITIALLY" "IMMEDIATE"  
  ; 
 
fk_def: "FOREIGN" "KEY" '(' name_list ')' fk_target_options  
  ; 
 
fk_target_options : "REFERENCES" name '(' name_list ')' opt_fk_options  
  ; 
 
unq_def: "CONSTRAINT" name "UNIQUE" '(' name_list ')'  
  | "UNIQUE" '(' name_list ')'  
  ; 
 
opt_unique: /* nil */  
  | "UNIQUE"  
  ; 
 
indexed_column: name opt_asc_desc  
  ; 
 
indexed_columns: indexed_column  
  | indexed_column ',' indexed_columns  
  ; 
 
create_index_stmt: "CREATE" opt_unique "INDEX" opt_if_not_exists name "ON" name '(' indexed_columns ')' opt_delete_version_attr  
  ; 
 
name: "ID"  
  | "TEXT"  
  | "TRIGGER"  
  | "ROWID"  
  ; 
 
opt_name: /* nil */  
  | name  
  ; 
 
name_list: name  
  |  name ',' name_list   
  ; 
 
opt_name_list: /* nil */  
  | name_list  
  ; 
 
col_attrs: /* nil */  
  | "NOT" "NULL" col_attrs  
  | "PRIMARY" "KEY" col_attrs  
  | "PRIMARY" "KEY" "AUTOINCREMENT" col_attrs  
  | "DEFAULT" '-' num_literal col_attrs  
  | "DEFAULT" num_literal col_attrs  
  | "DEFAULT" str_literal col_attrs  
  | "UNIQUE" col_attrs  
  | "@SENSITIVE" col_attrs  
  | "@CREATE" version_annotation col_attrs  
  | "@DELETE" version_annotation col_attrs  
  | fk_target_options col_attrs  
  ; 
 
version_annotation: '(' "integer-literal" ',' name ')'  
  | '(' "integer-literal" ')'  
  ; 
 
object_type: 
    "OBJECT"  
  | "OBJECT" '<' name '>'  
  ; 
 
data_type: 
    "INT"  
  | "INTEGER"  
  | "TEXT"  
  | "REAL"  
  | "LONG"  
  | "BOOL"  
  | "LONG" "INTEGER"  
  | "LONG" "INT"  
  | "LONG_INT" | "LONG_INTEGER"  
  | "BLOB"  
  | object_type  
  ; 
 
data_type_opt_notnull: data_type  
  | data_type "NOT" "NULL"  
  | data_type "@SENSITIVE"  
  | data_type "@SENSITIVE" "NOT" "NULL"  
  | data_type "NOT" "NULL" "@SENSITIVE"   
  ; 
 
str_literal: "sql-string-literal"  
  | "c-string-literal"  
  ; 
 
num_literal:  "integer-literal"  
  | "long-literal"  
  | "real-literal"  
  ; 
 
any_literal: str_literal  
  | num_literal  
  | "NULL"  
  | "@FILE" '(' str_literal ')'  
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
  ; 
 
basic_expr: name  
  | name '.' name  
  | any_literal  
  | '(' expr ')'  
  | call  
  | window_func_inv  
  | raise_expr  
  | '(' select_stmt ')'  
  | "EXISTS" '(' select_stmt ')'  
  ; 
 
math_expr: basic_expr  
  | math_expr '&' math_expr  
  | math_expr '|' math_expr  
  | math_expr "<<" math_expr  
  | math_expr ">>"  math_expr  
  | math_expr '+' math_expr  
  | math_expr '-' math_expr  
  | math_expr '*' math_expr  
  | math_expr '/' math_expr  
  | math_expr '%' math_expr  
  | '-' math_expr   
  | math_expr "||" math_expr  
  ; 
 
expr: basic_expr  
  | expr '&' expr  
  | expr '|' expr  
  | expr "<<" expr  
  | expr ">>" expr  
  | expr '+' expr  
  | expr '-' expr  
  | expr '*' expr  
  | expr '/' expr  
  | expr '%' expr  
  | '-' expr   
  | "NOT" expr  
  | '~' expr  
  | expr "COLLATE" name  
  | expr "AND" expr  
  | expr "OR" expr  
  | expr '=' expr  
  | expr "==" expr  
  | expr '<' expr  
  | expr '>' expr  
  | expr "<>" expr  
  | expr "!=" expr  
  | expr ">=" expr  
  | expr "<=" expr  
  | expr "NOT" "IN" '(' expr_list ')'  
  | expr "NOT" "IN" '(' select_stmt ')'  
  | expr "IN" '(' expr_list ')'  
  | expr "IN" '(' select_stmt ')'  
  | expr "LIKE" expr  
  | expr "NOT LIKE" expr  
  | expr "MATCH" expr  
  | expr "REGEXP" expr  
  | expr "GLOB" expr  
  | expr "NOT" "BETWEEN" math_expr "AND" math_expr   
  | expr "BETWEEN" math_expr "AND" math_expr   
  | expr "IS NOT" expr  
  | expr "IS" expr  
  | expr "||" expr  
  | "CASE" expr case_list "END"  
  | "CASE" expr case_list "ELSE" expr "END"  
  | "CASE" case_list "END"  
  | "CASE" case_list "ELSE" expr "END"  
  | "CAST" '(' expr "AS" data_type ')'  
  ; 
 
case_list: "WHEN" expr "THEN" expr  
  | "WHEN" expr "THEN" expr case_list  
  ; 
 
arg_expr: '*'  
  | expr  
  | cursor_arguments  
  | from_arguments  
  ; 
 
arg_list: /* nil */  
  | arg_expr  
  | arg_expr ',' arg_list  
  ; 
 
expr_list: expr  
  | expr ',' expr_list  
  ; 
 
cursor_arguments : "FROM" name  
  | "FROM" name "LIKE" name  
  ; 
 
call_expr: expr  
  | cursor_arguments  
  | from_arguments  
  ; 
 
call_expr_list: call_expr  
  | call_expr ',' call_expr_list  
  ; 
 
cte_tables:  cte_table  
  | cte_table ',' cte_tables  
  ; 
 
cte_table: cte_decl "AS" '(' select_stmt_no_with ')'  
  ; 
 
cte_decl: name '(' name_list ')'  
  | name '(' '*' ')'  
  ; 
 
with_prefix: "WITH" cte_tables  
  | "WITH" "RECURSIVE" cte_tables  
  ; 
 
with_select_stmt: with_prefix select_stmt_no_with  
  ; 
 
select_stmt: with_select_stmt  
  | select_stmt_no_with  
  ; 
 
select_stmt_no_with: select_core_list opt_orderby opt_limit opt_offset  
  ; 
 
select_core_list: select_core  
  | select_core select_core_compound  
  ; 
 
select_core_compound: compound_operator select_core_list  
  ; 
 
 
values: '(' insert_list ')'  
  | '(' insert_list ')' ',' values  
  ; 
 
select_core: "SELECT" select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window  
  | "VALUES" values  
  ; 
 
compound_operator: 
    "UNION"  
  | "UNION ALL"  
  | "INTERSECT"  
  | "EXCEPT"  
  ; 
 
window_func_inv: name '(' arg_list ')' opt_filter_clause "OVER" window_name_or_defn  
  ; 
 
opt_filter_clause: /* nil */  
  | "FILTER" '(' opt_where ')'  
  ; 
 
window_name_or_defn: window_defn 
  | name 
  ; 
 
window_defn: '(' opt_partition_by opt_orderby opt_frame_spec ')'  
  ; 
 
opt_frame_spec: /* nil */  
  | frame_type frame_boundary_opts frame_exclude  
  ; 
 
frame_type: "RANGE"  
  | "ROWS"  
  | "GROUPS"  
  ; 
 
frame_exclude: /* nil */  
  | "EXCLUDE NO OTHERS"  
  | "EXCLUDE CURRENT ROW"  
  | "EXCLUDE GROUP"  
  | "EXCLUDE TIES"  
  ; 
 
frame_boundary_opts: frame_boundary  
  | "BETWEEN" frame_boundary_start "AND" frame_boundary_end  
  ; 
 
frame_boundary_start: "UNBOUNDED" "PRECEDING"  
  | expr "PRECEDING"  
  | "CURRENT ROW"  
  | expr "FOLLOWING"  
  ; 
 
frame_boundary_end: expr "PRECEDING"  
  | "CURRENT ROW"  
  | expr "FOLLOWING"  
  | "UNBOUNDED" "FOLLOWING"  
  ; 
 
frame_boundary: "UNBOUNDED" "PRECEDING"  
  | expr "PRECEDING"  
  | "CURRENT ROW"  
  ; 
 
opt_partition_by: /* nil */  
  | "PARTITION" "BY" expr_list  
  ; 
 
opt_select_window: /* nil */  
  | window_clause  
  ; 
 
window_clause: "WINDOW" window_name_defn_list  
  ; 
 
window_name_defn_list: window_name_defn  
  | window_name_defn ',' window_name_defn_list  
  ; 
 
window_name_defn: name "AS" window_defn  
 
region_spec: 
    name   
  | name "PRIVATE"  
  ; 
 
region_list : 
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
 
begin_schema_region_stmt: "@BEGIN_SCHEMA_REGION" name  
  ; 
 
end_schema_region_stmt: "@END_SCHEMA_REGION"  
  ; 
 
schema_ad_hoc_migration_stmt: "@SCHEMA_AD_HOC_MIGRATION" version_annotation  
  ; 
 
opt_from_query_parts: /* nil */  
  | "FROM" query_parts  
  ; 
 
opt_where: /* nil */  
  | "WHERE" expr  
  ; 
 
opt_groupby: /* nil */  
  | "GROUP" "BY" groupby_list  
  ; 
 
groupby_list: groupby_item  
  | groupby_item ',' groupby_list  
  ; 
 
groupby_item: expr opt_asc_desc  
  ; 
 
opt_asc_desc: /* nil */  
  | "ASC"  
  | "DESC"  
  ; 
 
opt_having: /* nil */  
  | "HAVING" expr  
  ; 
 
opt_orderby: /* nil */  
  | "ORDER" "BY" groupby_list  
  ; 
 
opt_limit: /* nil */  
  | "LIMIT" expr  
  ; 
 
opt_offset: /* nil */  
  | "OFFSET" expr  
  ; 
 
select_opts: /* nil */  
  | "ALL"   
  | "DISTINCT"  
  | "DISTINCTROW"  
  ; 
 
select_expr_list: select_expr  
  | select_expr ',' select_expr_list  
  | '*'  
  ; 
 
select_expr: expr opt_as_alias  
  |  name '.' '*'  
  ; 
 
opt_as_alias: /* nil */  
  | "AS" name  
  | name  
  ; 
 
query_parts: table_or_subquery_list  
  | join_clause  
  ; 
 
table_or_subquery_list: table_or_subquery  
  | table_or_subquery ',' table_or_subquery_list  
  ; 
 
join_clause: table_or_subquery join_target_list  
  ; 
 
join_target_list: join_target  
  | join_target join_target_list  
  ; 
 
table_or_subquery: name opt_as_alias  
  | '(' select_stmt ')' opt_as_alias  
  | table_function opt_as_alias  
  | '(' query_parts ')'  
  ; 
 
join_target: opt_inner_cross "JOIN" table_or_subquery opt_join_cond  
  | left_or_right opt_outer "JOIN" table_or_subquery opt_join_cond  
  ; 
 
opt_inner_cross: /* nil */  
  | "INNER"  
  | "CROSS"  
  ; 
 
opt_outer: /* nil */  
  | "OUTER"  
  ; 
 
left_or_right: "LEFT"  
  | "RIGHT"  
  ; 
 
opt_join_cond: /* nil */  
  | join_cond 
  ; 
 
join_cond: "ON" expr  
  | "USING" '(' name_list ')'  
  ; 
 
table_function: name '(' arg_list ')'  
  ; 
 
create_view_stmt: "CREATE" opt_temp "VIEW" opt_if_not_exists name "AS" select_stmt opt_delete_version_attr  
  ; 
 
with_delete_stmt: with_prefix delete_stmt  
  ; 
 
delete_stmt: "DELETE" "FROM" name opt_where  
  ; 
 
opt_insert_dummy_spec : /*nil*/   
  | "@DUMMY_SEED" '(' expr ')' dummy_modifier  
  ; 
 
dummy_modifier: /* nil */  
  | "@DUMMY_NULLABLES"  
  | "@DUMMY_DEFAULTS"   
  | "@DUMMY_NULLABLES" "@DUMMY_DEFAULTS"   
  | "@DUMMY_DEFAULTS" "@DUMMY_NULLABLES"   
  ; 
 
insert_stmt_type : "INSERT" "INTO"  
  | "INSERT" "OR" "REPLACE" "INTO"  
  | "INSERT" "OR" "IGNORE" "INTO"  
  | "INSERT" "OR" "ROLLBACK" "INTO"  
  | "INSERT" "OR" "ABORT" "INTO"  
  | "INSERT" "OR" "FAIL" "INTO"  
  | "REPLACE" "INTO"  
  ; 
 
with_insert_stmt: with_prefix insert_stmt  
  ; 
 
opt_column_spec: /* nil */  
  | '(' opt_name_list ')'  
  | '(' "LIKE" name ')'  
  ; 
 
from_cursor:  "FROM" "CURSOR" name opt_column_spec  
  ; 
 
from_arguments: "FROM" "ARGUMENTS"  
  | "FROM" "ARGUMENTS" "LIKE" name  
  ; 
 
insert_stmt: insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec  
  | insert_stmt_type name opt_column_spec from_arguments opt_insert_dummy_spec  
  | insert_stmt_type name opt_column_spec from_cursor opt_insert_dummy_spec  
  | insert_stmt_type name "DEFAULT" "VALUES"  
  ; 
 
insert_list:  
  | expr  
  | expr ',' insert_list  
  ; 
 
basic_update_stmt: "UPDATE" opt_name "SET" update_list opt_where  
  ; 
 
with_update_stmt: with_prefix update_stmt  
  ; 
 
update_stmt: "UPDATE" name "SET" update_list opt_where opt_orderby opt_limit  
  ; 
 
update_entry: name '=' expr  
  ; 
 
update_list: update_entry  
  | update_entry ',' update_list  
  ; 
 
with_upsert_stmt: with_prefix upsert_stmt  
  ; 
 
upsert_stmt: insert_stmt "ON CONFLICT" conflict_target "DO" "NOTHING"  
  | insert_stmt "ON CONFLICT" conflict_target "DO" basic_update_stmt  
  ; 
 
update_cursor_stmt: 
    "UPDATE" "CURSOR" name opt_column_spec "FROM" "VALUES" '(' insert_list ')'   
  | "UPDATE" "CURSOR" name opt_column_spec from_cursor  
  ; 
 
conflict_target:  /* nil */  
  | '(' indexed_columns ')' opt_where  
  ; 
 
creation_type: object_type  
  | object_type "NOT" "NULL"  
  | "TEXT"  
  | "TEXT" "NOT" "NULL"  
  | "BLOB"  
  | "BLOB" "NOT" "NULL"  
  ; 
 
function: "FUNC" | "FUNCTION" 
  ; 
 
declare_func_stmt: "DECLARE" function name '(' params ')' data_type_opt_notnull  
  | "DECLARE" "SELECT" function name '(' params ')' data_type_opt_notnull  
  | "DECLARE" function name '(' params ')' "CREATE" creation_type  
  | "DECLARE" "SELECT" function name '(' params ')' '(' typed_names ')'  
  ; 
 
procedure: "PROC" | "PROCEDURE" 
  ; 
 
declare_proc_stmt: "DECLARE" procedure name '(' params ')'  
  | "DECLARE" procedure name '(' params ')' '(' typed_names ')'  
  | "DECLARE" procedure name '(' params ')' "USING" "TRANSACTION"  
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')'  
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')' "USING" "TRANSACTION"  
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')'  
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')' "USING" "TRANSACTION"  
  ; 
 
create_proc_stmt: "CREATE" procedure name '(' params ')' "BEGIN" opt_stmt_list "END"  
  ; 
 
opt_inout: /* nil */  
  | "IN"  
  | "OUT"  
  | "INOUT"  
  ; 
 
typed_name: name data_type_opt_notnull  
  | "LIKE" name  
  ; 
 
typed_names: typed_name   
  | typed_name ',' typed_names  
  ; 
 
param: opt_inout name data_type_opt_notnull  
  | "LIKE" name  
  ; 
 
params: /* nil */  
  | param  
  |  param ',' params   
  ; 
 
declare_stmt: "DECLARE" name_list data_type_opt_notnull  
  | "DECLARE" name "CURSOR FOR" select_stmt  
  | "DECLARE" name "CURSOR FOR" explain_stmt  
  | "DECLARE" name "CURSOR FOR" call_stmt   
  | "DECLARE" name "CURSOR" "FETCH" "FROM" call_stmt  
  | "DECLARE" name "CURSOR" "LIKE" name  
  | "DECLARE" name "CURSOR" "LIKE" select_stmt  
  ; 
 
call_stmt: "CALL" name '(' ')'  
  | "CALL" name '(' call_expr_list ')'  
  ; 
 
while_stmt: "WHILE" expr "BEGIN" opt_stmt_list "END"  
  ; 
 
loop_stmt: "LOOP" fetch_stmt "BEGIN" opt_stmt_list "END"  
  ; 
 
leave_stmt: "LEAVE"  
  ; 
 
return_stmt: "RETURN"  
  ; 
 
throw_stmt: "THROW"  
  ; 
 
trycatch_stmt: "BEGIN" "TRY" opt_stmt_list "END" "TRY" ';' "BEGIN" "CATCH" opt_stmt_list "END" "CATCH"  
  ; 
 
continue_stmt: "CONTINUE"  
  ; 
 
fetch_stmt: "FETCH" name "INTO" name_list  
  | "FETCH" name  
  ; 
 
fetch_values_stmt: 
    "FETCH" name opt_column_spec "FROM" "VALUES" '(' insert_list ')' opt_insert_dummy_spec  
  | "FETCH" name opt_column_spec from_arguments opt_insert_dummy_spec  
  | "FETCH" name opt_column_spec from_cursor opt_insert_dummy_spec  
  ; 
 
fetch_call_stmt: "FETCH" name opt_column_spec "FROM" call_stmt  
  ; 
 
fetch_cursor_stmt: "FETCH" name opt_column_spec "FROM" name  
  ; 
 
open_stmt: "OPEN" name  
  ; 
 
close_stmt: "CLOSE" name   
  ; 
 
out_stmt: "OUT" name   
  ; 
 
out_union_stmt: "OUT" "UNION" name   
  ; 
 
if_stmt: "IF" expr "THEN" opt_stmt_list opt_elseif_list opt_else "END" "IF"  
  ; 
 
opt_else: /* nil */  
  | "ELSE" opt_stmt_list  
  ; 
 
elseif_item: "ELSE IF" expr "THEN" opt_stmt_list  
  ; 
 
elseif_list: elseif_item  
  | elseif_item elseif_list  
  ; 
 
opt_elseif_list: /* nil */  
  | elseif_list  
  ; 
 
begin_trans_stmt: "BEGIN" "TRANSACTION"  
  ; 
 
rollback_trans_stmt: "ROLLBACK" "TRANSACTION"  
  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" name  
  ; 
 
commit_trans_stmt: "COMMIT" "TRANSACTION"  
  ; 
 
savepoint_stmt: "SAVEPOINT" name  
  ; 
 
release_savepoint_stmt: "RELEASE" "SAVEPOINT" name  
  ; 
 
echo_stmt: "@ECHO" name ',' str_literal  
  ; 
 
alter_table_add_column_stmt: "ALTER" "TABLE" name "ADD" "COLUMN" col_def  
  ; 
 
create_trigger_stmt:  "CREATE" opt_temp "TRIGGER" opt_if_not_exists trigger_def opt_delete_version_attr  
  ; 
 
trigger_def: name trigger_condition trigger_operation "ON" name trigger_action  
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
 
trigger_action:  opt_foreachrow opt_when_expr "BEGIN" trigger_stmts "END"  
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
 
/* These forms are slightly different than the normal statements, not all variations are allowed. 
 * This section clearly states the mapping.  It could be done more tersely but this costs us nothing. 
 */ 
 
trigger_stmt: 
    trigger_update_stmt ';'  
  | trigger_insert_stmt ';'  
  | trigger_delete_stmt ';'  
  | trigger_select_stmt ';'  
  ; 
 
trigger_select_stmt : select_stmt_no_with  
  ; 
 
trigger_insert_stmt : insert_stmt  
  ; 
 
trigger_delete_stmt : delete_stmt  
  ; 
 
trigger_update_stmt : basic_update_stmt  
  ; 
 
enforcement_options: 
    "FOREIGN" "KEY" "ON" "UPDATE"  
  | "FOREIGN" "KEY" "ON" "DELETE"  
  | "JOIN"  
  | "UPSERT" "STATEMENT"  
  | "WINDOW" function  
  | procedure  
  | "WITHOUT" "ROWID"  
  ; 
 
enforce_strict_stmt: "@ENFORCE_STRICT" enforcement_options  
  ; 
 
enforce_normal_stmt: "@ENFORCE_NORMAL" enforcement_options  
  ; 
 
```


<div style="page-break-after: always; visibility: hidden"></div>

<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
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
  * `WINDOW FUNC`: window functions are disallowed (useful if targetting old versions of SQLite)
  * `UPSERT STATEMENT`: the upsert form is disallowed (useful if targetting old versions of SQLite)

`@SENSITIVE`
 * marks a column or variable as 'sensitive' for privacy purposes, this behaves somewhat like nullability (See Chapter 3) in that it is radioactive, contaminating anything it touches
 * the intent of this annotation is to make it clear where sensitive data is being returned or consumed in your procedures
 * this information appears in the JSON output for further codegen or for analysis (See Chapter 13)
 
`@DECLARE_SCHEMA_REGION`
`@DECLARE_DEPLOYABLE_REGION`
`@BEGIN_SCHEMA_REGION`
`@END_SCHEMA_REGION`

 * These directives controlt he declaration of schema regions and allow you to place things into those regions (See Chapter 10)

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
  * the nested nature of attribute values is sufficiently flexible than you could encode an arbitary LISP program in an attribute, so really anything you might need to express is possible
  * there are a number of attributes known to the compiler which I list below (complete as of this writing)
  
  * `cql:autodrop=(table1, table2, ...)` when present the indicated tables, which must be temp tables, are dropped when the results of the procedure have been fetched into a rowset
  * `cql:indentity=(column1, column2, ...)` the indicated columns are used to create a row comparator for the rowset corresponding to the procedure, this appears in a C macro of the form `procedure_name_row_same(rowset1, row1, rowset2, row2)`
  * `cql:suppres_getters` the indicated procedure should not emit the column getter functions (useful if you only indend to call the procedure from CQL, or if you wish to restrict access in C)
  * `cql:base_fragment=frag_name` for base fragments (See Chapter 14)
  * `cql:extension_fragment=frag_name` for extension fragments (See Chapter 14)
  * `cql:assembly_fragment=frag_name` for assembly fragments (See Chapter 14)
  * `cql:no_table_scan` for query plan processing, indicates that the table in question should never be table scanned in any plan (for better diagnostics)
  * `cql:autotest=([many forms])` declares various autotest features (See Chapter 12)


<div style="page-break-after: always; visibility: hidden"></div>

<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## Appendix 3: CQL Error Codes

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
```
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

### CQL0020: duplicate unique key in table 'key_name'

A table contains two unique keys with the same name.

------

### CQL0021: foreign key refers to non-existent table 'table_name'

The table in a foreign key REFERENCES clause is not a valid table.

------

### CQL0022: the exact type of both sides of a foreign key must match (expected expected_type; found actual_type) 'key_name'

The indicated foreign key has at least one column with a different type than corresponding column in the table it references.
This usually means that you have picked the wrong table or column in the foreign key declaration.

-----

### CQL0023: The number of columns on both sides of a foreign key must match

The number of column in the foreign key must be the same as the number of columns specified in the foreign table.
This usually means a column is missing in the REFERENCES part of the declaration.

-----

### CQL0024: table does not have pk column 'column'

In a primary key declaration, the indicated column, found in the primary is not actually a column of the table.
This usually means there is a typo in the primary key column declaration.

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

### CQL0028: the FK reference must be exactly one column with the correct type 'column_name'

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

### CQL0035: column delete version can't be <= column create version", 'column'

You can't `@delete` a column in a version before it was even created.  Probably there is a typo in one or both of the versions.

-----

### CQL0036: column delete version can't be <= the table create version 'column'

The indicated column is being deleted in a version that is before the table it is found in was even created.  Probably there is a typo in the delete version.

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

### CQL0048: blob operand not allowed in 'NOT'

The logical not operator only works on numbers.  Blobs are not allow as an operand.

-----

### CQL0049: object operand not allowed in 'NOT'

The logical not operator only works on numbers.  Objects are not allow as an operand.

-----

### CQL0050: string operand not allowed in 'NOT'

The logical not operator only works on numbers.  Strings are not allow as an operand.

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

### CQL0056: NULL column did not specify a type 'column'

In a select statement the NULL literal has no type.  If the type of the column cannot be inferred then it must be declared specifically.
You can fix this error by changing the `NULL` to something like `CAST(NULL as TEXT)`.
A common place this problem happens is in defining a view or returning a result set from a stored procedure.  In those cases
all the columns must have a name and a type.

-----

### CQL0057: if multiple selects, all must have the same column count

If a stored procedure might return one of several result sets, each of the select statements it might return must have the same number of columns.
Likewise, if several select results are being combined with `UNION` or `UNION ALL` they must all have the same number of columns.

------

### CQL0058: if multiple selects, all column names must be identical so they have unambiguous names 'column'

If a stored procedure might return one of several result sets, each of the select statements must have the same column names for its result.
Likewise, if several select results are being combined with `UNION` or `UNION ALL` they must all have the same column names.

This is important so that there can be one unambiguous column name for every column for group of select statements.

e.g.
```
select 1 A, 2 B
union
select 3 A, 4 C;
```
Would provoke this error.  In this case 'C' would be regarded as the offending column.

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

### CQL0060: referenced table can be independently be recreated so it cannot be used in a foreign key, 'referenced_table'

The referenced table is marked recreate so it must be in the same recreate
group as the current table because the referenced table might be recreated away leaving all the
foreign key references in current table as orphans.

So we check the following:
If the referenced table is marked recreate then any of the following result in CQL0060

 * the referenced table is in no group, OR
 * the containing table is not recreate at all (non-recreate table can't reference recreate tables at all), OR
 * the containing table is in no recreate group (it's recreate but not in any group so they might not rev together), OR
 * the recreate groups of the two tables are different (it's in a recreate group but not same one so they my not rev together)

The referenced table is a recreate table and one of the 4 above conditions was not met.  Either don't reference it or
else put the current table and the referenced table into the same recreate group.

----

### CQL0061: if multiple selects, all columns must be an exact type match (expected expected_type; found actual_type) 'column'

In a stored proc with multiple possible selects providing the result, all of the columns of all the selects must be an exact type match.

e.g.

```
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
```
create proc foo(X object)
begin
  select X is null;
end;
```

In this example X is an object parameter, but even to use X for an `is null` check in a select statement would require binding an object which is not possible.

On the other hand this compiles fine.

```
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
```
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

```
declare a integer;
declare b integer;
declare C cursor for select 1 A, 2 B;
fetch C into a, b; -- C.A and C.B not created (!)
if (C.A) then -- error
  ...
end if;
```

Correct usage looks like this:

```
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

```
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

### CQL0074: Too few arguments provided 'coalesce'

There must be at least two arguments in a call to `coalesce`.

-----

### CQL0075: Incorrect number of arguments 'ifnull'

The  `ifnull` function requires exactly two arguments.

-----

### CQL0076: Null literal is useless in function 'ifnull/coalesce'

Adding a NULL literal to `IFNULL` or `COALESCE` is a no-op.  It's most likely an error.

-----

### CQL0077: encountered arg known to be not null before the end of the list, rendering the rest useless.

In an `IFNULL` or `COALESCE` call, only the last argument should be known to be not null.  If the not null argument comes earlier in the list, then none of the others could ever be used.  That is almost certainly an error.

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

```
select MAX(7);
```

Doesn't make any sense.

-----

### CQL0082: argument must be numeric 'AVERAGE'

The argument of AVERAGE must be numeric.

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

### CQL0086: first argument must be a string in function 'printf'

The first argument of `printf` is the formatting string.  The other arguments are variable and many complex conversions will apply.

-----

### CQL0087: no object/blob types are allowed in arguments for function 'printf'

The `printf` has no meaningful conversions for blobs.  Object references are entirely unsupported.

-----

### CQL0088: User function may not appear in the context of a SQL statement 'function_name'

External C functions declared with `declare function ...` are not for use in sqlite.  They may not appear inside statements.

-----

### CQL0089: User function may only appear in the context of a SQL statement 'function_name'

SQLite user defined functions (or builtins) declared with  `declare select function` may only appear inside of sql statements.  In the case of user defined functions they must be added to sqlite by the appropriate C APIs before they can be used in CQL stored procs (or any other context really).   See the sqlite documentation on how to add user defined functions. [Create Or Redefine SQL Functions](http://www.sqlite.org/c3ref/create_function.html)

-----

### CQL0090: Stored proc calls may not appear in the context of a SQL statement 'proc_name'

While it's possible to call a CQL stored procedure as though it was a function (if it has an OUT argument as its last arg) you may not do this from inside of a SQL statement.  Just like external C functions SQLite cannot call stored procs.

-----

### CQL0091: Stored procs that deal with result sets or cursors cannot be invoked as functions 'name'

The function syntax for procs cannot be used on procedures that return a result set.  Such procedures already have a result and it isn't even a scalar result.

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

```
WITH foo(a) as (SELECT 1 A, 2 B) ...`
```

The select statement produces two columns the `foo` declaration specifies one.

-----

### CQL0102: too many column names specified in common table expression 'name'


In a `WITH` clause the indicated common table expression has more  column names than the `select` expression it is associated with.

e.g.

```
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

### CQL0139: temp tables may not have versioning annotations 'table_name'

The indicated table is a temporary table.  Since temp tables do not survive sessions it makes no sense to try to version them for schema upgrade.  They are always recreated on demand.

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

### CQL0159: there's no good way to generate dummy blobs; not supported for now

The dummy data feature does not support blobs.  You can make your own blob with the CAST operator and a numeric if need be.

-----

### CQL0160: table in insert statement does not exist 'table_name'

In an `INSERT` statement attempting to insert into the indicated table name is not possible because there is no such table.
This error might happen because of a typo, or it might happen because the indicated table has been marked with `@delete` and is logically hidden.

-----

### CQL0161: cannot insert into a view 'view_name'

In an `INSERT` statement attempting to insert into the indicated name is not possible because that name is a view not a table.  Inserting into views is not supported.

-----

### CQL0162: FROM ARGUMENTS is redundant if insert column list is empty

The `INSERT` statement supports the sugar format  `INSERT INTO foo(a,b,c) FROM ARGUMENTS` which causes the arguments of the current procedure to be used as the values.  This error is complaining that no columns were specified, that is, you have written `INSERT INTO foo() FROM ARGUMENTS`.   With no specified columns the arguments would be redundant.  To fix this error there are two choices.   Perhaps the column names were omitted, in which case add `a,b,c` or something like that in the parenthesis.  Or else remove them entirely `INSERT INTO foo FROM ARGUMENTS` means insert ALL the columns of foo from the arguments.

-----

### CQL0163: FROM ARGUMENTS construct is only valid inside a procedure

The `INSERT` statement supports the sugar format  `INSERT INTO foo(a,b,c) FROM ARGUMENTS` which causes the arguments of the current procedure to be used as the values.  This error is complaining that you have used this form of insert but the statement does not occur inside of a procedure so there can be no arguments.  This form does not make sense outside of any procedure.

-----

### CQL0164: too few arguments available

The `INSERT` statement supports the sugar format  `INSERT INTO foo(a,b,c) FROM ARGUMENTS` which causes the arguments of the current procedure to be used as the values.  This error is complaining that you have used this form of insert but you have specified more columns (a,b,c) than there are arguments.  Either add arguments or remove columns to fix this error.

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

### CQL0168: there's no good way to generate dummy blobs; not supported for now

In a value cursor with dummy data specified, one of the columns in the cursor is of type blob.  There's no good way to create dummy data for blobs so that isn't supported.

-----

### CQL0169: cannot fetch from a cursor without fields 'cursor_name'

This error corresponds to a pretty specific instance.  First there are two cursors and we're processing a statement that looks something like this.

```
fetch C from D;
```
`C` and `D` are both cursor variables.  However, D does not have any storage associated with it so it isn't a valid source.

How does that happen?  More specifically this situation has happened almost certainly.

```
declare D cursor for select * from foo;
fetch D into a, b, c;
```
Here we did fetch from the cursor but we used local variables to hold the results, the cursor has no storage.

Had we done this instead:

```
fetch D;
```
then you use `D.a`, `D.b`, and `D.c` for the fetched values: the cursor has storage.  So if you use the second form (the easier one really) then the cursor can be used as a source of data.

If you have done the above and got this error you can always do something like this instead:

```
fetch C from values(a,b,c);
```

Which is where the cursor was stored.

-----

### CQL0170: cursor must be a value cursor, not a statement cursor 'cursor_name'

This error corresponds to a pretty specific instance.  First there are two cursors and we're processing a statement that looks something like this.

```
fetch C from D;
```
`C` and `D` are both cursor variables.  However, `C` is a statement cursor, it was declared something like this:

```
declare C cursor for select * from foo;
```

This kind of cursor is associated with a SQLite statement for its values.  It should not be used like a value cursor (which has no statement).  You may need to declare a different cursor to hold the result of the copy.

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

### CQL0176: proc has no result 'proc_name'

In a table definition, the `LIKE` construct was used to create columns that are the same as the return type of the named procedure.  However the named procedure does not produce a result set and therefore has no columns to mimic.  Probably the name is wrong.

-----

### CQL0177: table/view/proc not found 'name'

In a table definition, the `LIKE` construct was used to create columns that are the same shape as the named object.  However the indicated name is not the name of a table, view, or stored procedure at all.

-----

### CQL0178: proc has no result 'like_name'

In an argument list, the `LIKE` construct was used to create arguments that are the same as the return type of the named procedure.  However the named procedure does not produce a result set and therefore has no columns to mimic.  Probably the name is wrong.

-----

### CQL0179: table/view/proc not found 'like_name'

In an argument list the `LIKE` construct was used to create arguments that are the same shape as the named object.  However the indicated name is not the name of a table, view, or stored procedure at all.

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

### CQL0187: @schema_upgrade_version not declared or doesn't match upgrade version <N> for proc 'name'

The named procedure was declared as a schema migration procedure in an `@create` or `@delete` annotation for schema version `N`.  In order to correctly type check such a procedure it must be compiled in the context of schema version `N`.  This restriction is required so that the tables and columns the procedure sees are the ones that existed in version `N` not the ones that exist in the most recent version as usual.

To create this condition, the procedure must appear in a file that begins with the line:

```
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

### CQL0199: cursor requires a procedure that returns a result set via select 'proc_name'

In a `DECLARE` statement that declares a `CURSOR FOR CALL` the procedure that is being called does not produce a result set with the `SELECT` statement.  As it has no row results it is meaningless to try to put a cursor on it.  Probably the error is due to a copy/pasta of the procedure name.

-----

### CQL0200: variable is not a cursor 'another_cursor'

In a `DECLARE` statement that declares a `CURSOR LIKE` another cursor, the indicated name is a variable but it is not a cursor, so we cannot make another cursor like it.  Probably the error is due to a typo in the 'like_name'.

-----

### CQL0201: expanding FROM ARGUMENTS, there is no argument matching 'required_arg'

In an `insert` or `fetch` statment using the form `from arguments like [name]` the named entity
had columns that did not appear in as arguments to the current procedure.  Maybe arguments
are missing or maybe the name in the `like` part is the wrong name.

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

### CQL0204: cursor not found 'name'

The indicated name appeared in a context where a cursor name was expected, but that name does not correspond to any variable at all.  Probably there is a typo here.

-----

### CQL0205: variable is not a cursor 'name'

The indicated name appeared in a context where a cursor name was expected. There is a variable of that name but it is not a cursor.  Probably there is a copy/pasta type error here.

-----

### CQL0206: duplicate name in list 'name'

There are many contexts where a list of names appears in the CQL grammar and the list must not contain duplicate names.  Some examples are:

* the column names in a `JOIN ... USING(x,y,z,...)` clause
* the fetched variables in a `FETCH [cursor] INTO x,y,z...` statement
* the column names listed in a common table expression `CTE(x,y,z,...) as (SELECT ...)`
* the antecedent schema region names in `@declare_schema_region <name> USING x,y,z,...`

The indicated name was duplicated in such a context.

-----

### CQL0207: proc out parameter: formal cannot be fulfilled by non-variable 'param_name'

In a procedure call, the indicated parameter of the procedure is an OUT or INOUT parameter but the call site doesn't have a variable in that position in the argument list.

Example:

```sql
declare proc foo(out x integer);

-- the constant 1 cannot be used in the out position when calling foo
call foo(1); '
```
-----

### CQL0208: proc out parameter: formal cannot be fulfilled by in-only variable 'variable_name'

In a procedure call, the indicated parameter of the procedure is an OUT or INOUT parameter but the call site has an 'in' argument in that position.  While this is doable it's probably a mistake.

Example

```sql
declare proc foo(out x integer);

create proc bar(y integer) -- note 'in' is the default
begin
  call foo(y); -- clobbering our 'in' variable
end;
```

You should use a scratch local to capture the result.

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
```
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
```
CQL0210: proc out parameter: arg must be an exact type match (even nullability)
(expected integer notnull; found integer) 'y'
```

-----

### CQL0211: last formal arg of procedure is not an out arg, cannot use proc as a function 'name'

In a function call, the target of the function call was a procedure, procedures can be used like functions but their last parameter must be marked `out`. That will be the return value.  In this case the last argument was not marked as `out` and so the call is invalid.

Example:

```
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

### CQL0213: procedure had errors, can't call. 'proc_name'

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

The `CONTINUE` statement may only appear inside of looping constructs.  CQL only has two `loop fetch ...` and `while`

-----

### CQL0219: leave must be inside of a 'loop' or 'while' statement

The `LEAVE` statement may only appear inside of looping constructs.  CQL only has two `loop fetch ...` and `while`

-----

### CQL0220: savepoint has not been mentioned yet, probably wrong 'name'

In a `ROLLBACK` statement that is rolling back to a named savepoint, the indicated savepoint was never mentioned before.  It should have appeared previously in a `SAVEPOINT` statement.  This probably means there is a typo in the name.

-----

### CQL0221: savepoint has not been mentioned yet, probably wrong 'name'

In a `RELEASE SAVEPOINT` statement that is rolling back to a named savepoint, the indicated savepoint was never mentioned before.  It should have appeared previously in a `SAVEPOINT` statement.  This probably means there is a typo in the name.

-----

### CQL0222: the out cursor statement only makes sense inside of a procedure

The statement form `OUT [cursor_name]` makes a procedure that returns a single row result set.  It doesn't make any
sense to do this outside of any procedure because there is no procedure to return that result.  Perhaps the `OUT` statement was mis-placed.

-----

### CQL0223: the cursor was not fetched with the auto-fetch syntax 'fetch [cursor]' 'cursor_name'

The statement form `OUT [cursor_name]` makes a procedure that returns a single row result set that corresponds to the current value of the cursor.  If the cursor never held values directly then there is nothing to return.

Example:

```
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

### CQL0224: literal output can only appear outside of procedures

The `@echo` construct cannot appear inside of procedures, this would be far to fragile.  It's indented to let you insert C style declarations into the output stream if necessary.

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

### CQL0233: procedure previously declared as schema upgrade proc, it can have no args 'proc_name'

When authoring a schema migration procedure that was previously declared in an `@create` or `@delete` directive that procedure will be called during schema migration with no context available.  Therefore, the schema migration proc is not allowed to have any arguments.

-----

### CQL0234: autodrop annotation can only go on a procedure that returns a result set 'proc_name'

The named procedure has the `autodrop` annotation (to automatically drop a temporary table) but the procedure in question doesn't return a result set so it has no need of the autodrop feature.  The purpose that that feature is to drop the indicated temporary tables once all the select results have been fetched.

-----

### CQL0235: too many arguments provided to procedure 'proc_name'

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

### CQL0239: procedure identity column does not exist in result set 'column_name'

The `@attribute(cql:identity=(col1, col2,...))` form has been used to list the identity columns of a stored procedures result set.  These columns must exist in the result set and they must be unique.  The indicated column name is not part of the result of the procedure that is being annotated.

-----

### CQL0240: identity annotation can only go on a procedure that returns a result set 'proc_name'

The `@attribute(cql:identity=(col1, col2,...))` form has been used to list the identity columns of a stored procedures result set.  These columns must exist in the result set and they must be unique.  In this case, the named procedure doesn't even return a result set.  Probably there is a copy/pasta going on.  The identity attribute can likely be removed.

-----

### CQL0241: CONCAT may only appear in the context of SQL statement

The SQLite `||` operator has complex string conversion rules making it impossible to faithfully emulate.  Since there is no helper function for doing concatenations, CQL choses to support this operator only in contexts where it will be evaluated by SQLite.  That is, inside of some SQL statement.

Examples:
```
declare X text;

set X := 'foo' || 'bar';   -- error

set X := (select 'foo' || 'bar');  -- ok
```

If concatenation is required in some non-sql context, use the `(select ..)` expression form to let SQLite do the evaluation.

-----

### CQL0242

available for re-use

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

```
-- the above error happens if my_function has not been declared
-- as a table valued function
select * from my_function(1,2,3);
```

However , `my_function` has not been declared as a function at all.  A correct declaration might look like this:

```
declare select function my_function(a int, b int, c int)
                                   (x int, y text);
```

Either there is a typo in the name or the declaration is missing, or both...

-----

### CQL0251: fragment must end with exactly 'SELECT * FROM CTE'

Query fragments have an exact prescription for their shape.  This prescription includes `select * from CTE` as the final query where CTE is the common table expression that they define.  This the error message includes the specific name that is required in this context.

-----

### CQL0252:  available for re-use

-----

### CQL0253: base fragment must include a single CTE named same as the fragment 'name'

Query fragments have an exact prescription for their shape.  This prescription includes  `select * from CTE` where CTE is the common table expression that is the name of the base query.  This error says that the final select came from something other than the single CTE that is the base name.

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
```
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

### CQL0261: available for re-use

-----

### CQL0262: available for re-use

-----

### CQL0263: non-ANSI joins are forbidden if strict join mode is enabled.

You can enable strict enforcement of joins to avoid the form

```
select * from A, B;
```

which sometimes confuses people (the above is exactly the same as

```
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
Two or more extension fragments which have the same base fragments share the same name for one of their unique columns. E.g. the base table is `core(x,y)` and one extension table is `plugin_one(x,y,z)` and antoher is `plugin_two(x,y,z)`.
Here, z in both extension fragments share the same name but may refer to different values.

-----

### CQL0268: extension/assembly fragment must have the CTE columns same as the base fragment
 Extension and assembly fragments have an exact prescription for their shape. For each extension and assembly fragment, the first CTE must be a stub for their base table. This error means this stub in extension/assembly fragment differs from the definition of the base table.

-----

### CQL0269: at least part of this unique key is redundant with previous unique keys
The new unique key must have at least one column that is not in a previous key AND it must not have all the columns from any previous key.

e.g:
```
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

```
DECLARE C CURSOR FETCH FROM CALL foo(...);
```

The pattern

```
DECLARE C CURSOR FOR CALL foo(...);
```

Is used for procedures that provide a full `select` statement.

Note that in the former cause you don't then use `fetch` on the cursor.  There is at most one row anyway and it's fetched for you so a fetch would be useless.  In the second case you fetch as many rows as there are and/or you want.

-----

### CQL0271: the OFFSET clause may only be used if LIMIT is also present

```
select * from foo offset 1;
```

Is not supported by SQLite.  `OFFSET` may only be used if `LIMIT` is also present.  Also, both should be small because offset is not cheap.  There is no way to do offset other than to read and ignore the indicated number of rows.  So something like `offset 1000` is always horrible.

-----

### CQL0272: THE SET OF COLUMNS REFERENCED IN THE FOREIGN KEY STATEMENT SHOULD MATCH EXACTLY A UNIQUE KEY IN PARENT TABLE

If you're creating a table t2 with foreign keys on table t1, then the set of t1's columns reference in the foreign key statement for table t2 should be:
- A primary key in `t1`
```
e.g:
create table t1(a text primary key);
create table t2(a text primary key, foreign key(a) references t1(a));
```
- A unique key in `t1`
```
e.g:
create table t1(a text unique);
create table t2(a text primary key, foreign key(a) references t1(a));
```
- A group of unique key in `t1`
```
e.g:
create table t1(a text, b int, unique(a, b));
create table t2(a text, b int, foreign key(a, b) references t1(a, b));
```
- A group of primary key in `t1`
```
e.g:
create table t1(a text, b int, primary key(a, b));
create table t2(a text, b int, foreign key(a, b) references t1(a, b));
```
- A unique index in `t1`
```
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

### CQL0279: the set of columns referenced in the conflict target should match exactly a unique key in table we apply upsert

If you're doing an UPSERT on table t1, the columns listed in the conflict target should be:
- A primary key in `t1`
- A unique key in `t1`
- A group of unique key in `t1`
- A group of primary key in `t1`
- A unique index in `t1`

-----

### CQL0280: upsert statement requires a where clause if the insert clause uses select

When the `INSERT` statement to which the UPSERT is attached takes its values from a `SELECT` statement, there is a potential parsing ambiguity. The SQLite parser might not be able to tell if the `ON` keyword is introducing the UPSERT or if it is the `ON` clause of a join. To work around this, the `SELECT` statement should always include a `WHERE` clause, even if that `WHERE` clause is just `WHERE 1` (always true).   Note: The CQL parser doesn't have this ambiguity because it treats "ON CONFLICT" as a single token so this is CQL reporting that SQLite might have trouble with the query as written.

e.g:
```
insert into foo select id from bar where 1 on conflict(id) do nothing;
```

-----

### CQL0281: upsert statement does not include table name in the update statement

The UPDATE statement of and UPSERT should not include the table name because the name is already known from the INSERT statement part of the UPSERT
e.g:
```
insert into foo select id from bar where 1 on conflict(id) do update set id=10;
```

-----

### CQL0282: update statement require table name

The UPDATE statement should always include a table name except if the UPDATE statement is part of an UPSERT statement.

e.g:
```
update foo set id=10;
insert into foo(id) values(1) do update set id=10;
```

-----

### CQL0283: upsert syntax only support INSERT INTO

The INSERT statement part of an UPSERT statement can only uses INSERT INTO ...
```
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

### CQL0285: ad hoc schema migration directive version number changed 'proc_name'

In `@schema_ad_hoc_migration` you cannot change the version number of the directive once
it has been added to the schema because this could cause inconsistencies when upgrading.

You can change the body of the method if you need to but this is also not recommended because
again there could be inconsistencies.  However careful replacement and compensation is possible.  This is like
going to 110% on the reactor... possible, but not recommended.

-----

### CQL0286: ad hoc schema migration directive was removed; this is not allowed 'proc_name'

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


### CQL0290: fragments can only have one statement in the statement list and it must be a WITH..SELECT

All of the query fragment types consist of a procedure with exactly one statement and that statement is a WITH...SELECT statement.  If you have more than one statement or some other type of statement you'll get this error.

-----

### CQL0291: region links into the middle of a deployable region; you must point to the root of `<deployable_region>` not into the middle: `<error_region>`

Deployable regions have an "inside" that is in some sense "private".  In order to keep the consistent (and independently deployable) you can't peek into the middle of such
a region, you have to depend on the root (i.e. `<deployable_region>` itself).  This allows the region to remain independently deployable and for its internal logical regions to be reorganized in whatever manner makes sense.

To fix this error probably you should change `error_region` so that it depends directly on `deployable_region`

-----

### CQL0292: Explain statement is only available in dev mode because its result set may vary between sqlite versions

The EXPLAIN statement is intended for interactive debugging only. It helps engineer understand how Sqlite will execute their query and the cost attached to it. This is why this grammar is only available in dev mode in CQL and should never be used in production.

-----

### CQL0293: Only [EXPLAIN QUERY PLAN ...] statement is supported

CQL only support [EXPLAIN QUERY PLAN stmt] sql statement.

------

### CQL0294: Window function invocations can only appear in the select list of a select statement

Not all SQLite builtin function can be used as a window function.

------

### CQL0295: Window name is not defined

Window name referenced in the select list should be defined in the Window clause of the same select statement.

------

### CQL0296: Window name definition is not used

Window name defined in the window clause of a select statement should always be used within that statement.

------

### CQL0297: FROM CURSOR is redundant if column list is empty

This form:

`insert into YourTable() FROM CURSOR your_cursor;`

The `()` means no columns are being specified, the cursor will never be used.  The only source of columns is maybe dummy data (if it was specified) or the default values or null.  In no case will the cursor be used.  If you really want this use `FROM VALUES()` and don't implicate a cursor.

------

### CQL0298: cannot insert from a cursor without fields 'cursor_name'

The cursor in question has no storage associated with it.  It was loaded with something like:

`fetch C into x, y, z;`

You can only use a cursor as a source of data if it was fetched with its own storage like

`fetch C`

This results in a structure for the cursor.  This gives you C.x, C.y, C.z etc.

If you fetched the cursor into variables then you have to use the variables for any inserting.

------

### CQL0299: cursor has too few fields for this insert, 'cursor_name'

The cursor was used in a fetch statement but the number of columns fetched is smaller than the number required by the insert statement we are processing.

If you need to use the cursor plus some other data then you can't use this form, you'll have to use each field individually like `from values(C.x, C.y, C.z, other_stuff)`.

------

### CQL0300: Argument must be an integer (between 1 and max integer) in function 'function_name'

The argument of the function should be an integer.

------

### CQL0301: The second argument must be an integer (between 0 and max integer) in function 'function_name'

The second argument of the function should be an integer between 0 and INTEGER_MAX.

------

### CQL0302: The first and third arguments must be of the same type in function 'function_name'

The first and third arguments of the function have to be of the same type because the third argument provide a default value in cause the first argument is NULL.

------

### CQL0303: The second argument must be an integer between 1 and max integer in function 'function_name'

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

### CQL0311: CQL0311: object's deployment region changed from '<previous_region>' to '<current_region>' 'object_name'

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

Columns on a table must have default value or be nullable in order to use INSERT INTO <table> DEFAULT VALUES statement.

------

### CQL0316: the upsert-clause is not compatible with DEFAULT VALUES

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

### CQL0319: the name of the assembly procedure must match the name of the base fragment 'procedure_name'

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

Note: you could potentially add rows with a `LEFT OUTER JOIN` and a generous `ON` clause. That's allowed but not recommended. The `ON` clause can't be forbidden because it's essentail in the normal case.

-----

### CQL0321: migration proc not allowed on object 'object_name'

The indicated name is an index or a trigger. These objects may not have a migration script associated with them when they are deleted.

The reason for this is that both these types of objects are attached to a table and the table itself might be deleted.  If the table is deleted it becomes impossible to express even a tombstone for the deleted trigger or index without getting errors.  As a conseqence the index/trigger must be completely removed.  But if there had been a migration procedure on it then some upgrade sequences would have run it, but others would not (anyone who upgraded after the table was deleted would not get the migration procedure).  To avoid this problem, migration procedures are not allowed on indices and triggers.

-----

### CQL0322: fragment parameters must be exactly '[arguments]' 'procedure_name'

The named procedure is an extension fragment or an assembly fragment. It must have exactly the same arguments as the base fragment.  These arguments are provided.

Recall that the code for the procedure that does the select comes from the assembly fragment, so its arguments are in some sense the only ones that matter.  But the extension fragments are also allowed to use the arguments.  Of course we must ensure that the extension fragments do not use any arguments that aren't in the assembly, and so the only choice we have is to make sure the extensions conform to the base.  And so for that to work the assembly also has to conform to the base.  So the base fragment must define the args for all the other fragments.

You could imagine a scheme where the extension fragments are allowed to use a subset of the parameters defined in the base but if that were the case you might have names that mean different things in different fragments and then you could get errors or different semantics when the fragments were assembled. To avoid all of these problems, and for simplicity, we demand that the arguments of all fragments match exactly.

-----

### CQL0323: calls to undeclared procedures are forbidden if strict procedure mode is enabled 'procedure_name'

`@enforce_strict PROCEDURE` has been enabled.   In this mode you may only call procedures that have a declaration.
In `@enforce_normal PROCEDURE` mode, a call to an unknown proc is interpreted as a simple C call.  This lets you call
functions like `printf` in normal mode, even if they have a strange calling convention.  Strict mode limits you to declared procedures
and is generally safer.

If you get this error the most likely reason is that there is a typo in the name of the procedure you are trying to call.  If you really
need to call a c runtime function then you must temporarily switch back to `@enforce_normal` for procedures.

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

### CQL0326: the table name in ok_table_scan does not exist 'table_name'

The names provided to `ok_table_scan` attribute should be names of existing tables.

The attribute indicates tables that are ok to scan in this procedure even though they
are typically not ok to scan due to 'no_table_scan'.  Therefore the attribute must
refer to an existing table.  There is likely a typo in the the table name that needs
to be corrected.

-----

### CQL0327: a value should not be assigned to no_table_scan attribute

The attribute `no_table_scan` doesn't take a value.

When marking a table with `@attribute(cql:no_table_scan)` there is no need
for an attribute value.

-----

### CQL0328: no_table_scan attribute may only be added to a create table statement

The `no_table_scan` attrubute can only be assigned to a create table statement.

The marking `@attribute(cql:no_table_scan)` only makes sense on tables.  It's likely
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
LEFT OUTER JOIN fragments.  The offendeding fragment is named in the error.

-----

### CQL0333: all the compound operators in this CTE must be UNION ALL

The compound operators in CTE must and always be an UNION ALL.

-----

### CQL0334: @dummy_seed @dummy_nullables @dummy_defaults many only be used with a single VALUES row

Dummy insert feature makes only sense when it's used in a VALUES clause that is not part of a compound select statement.

-----

CQL0335

Available for re-use

-----

### CQL0336: select statement with VALUES clause requires a non empty list of values

VALUES clause requires at least a value for each of the values list. Empty values list are not supported.

-----

### CQL0337: the number of columns values for each row should be identical in VALUES clause

The number of values for each values list in VALUES clause should always be the same.

-----

### CQL0338: the name of a migration procedure may not end in '_crc' 'procedure_name'

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


<div style="page-break-after: always; visibility: hidden"></div>

<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

## Appendix 5: JSON Schema Grammar

What follows is taken from the JSON validation grammar with the tree building rules removed.

Snapshot as of Tue Sep  8 13:06:45 PDT 2020
### Rules

```
 
 
json_schema: '{' 
         '"tables"' ':' '[' opt_tables ']' ',' 
         '"views"' ':' '[' opt_views ']' ',' 
         '"indices"' ':' '[' opt_indices ']' ',' 
         '"triggers"' ':' '[' opt_triggers ']' ',' 
         '"attributes"' ':' '[' opt_attribute_list ']' ',' 
         '"queries"' ':' '[' opt_queries ']' ',' 
         '"inserts"' ':' '[' opt_inserts ']' ',' 
         '"updates"' ':' '[' opt_updates ']' ',' 
         '"deletes"' ':' '[' opt_deletes ']' ',' 
         '"general"' ':' '[' opt_generals ']' ',' 
         '"regions"' ':' '[' opt_regions ']' ',' 
         '"adHocMigrationProcs"' ':' '[' opt_ad_hoc_migrations ']' 
         '}' 
  ; 
 
BOOL_LITERAL: '0' | '1' 
  ; 
 
opt_tables: | tables 
  ; 
 
tables: table | table ',' tables 
  ; 
 
table: '{' 
       '"name"' ':' STRING_LITERAL ',' 
       '"isTemp"' ':' BOOL_LITERAL ',' 
       '"ifNotExists"' ':' BOOL_LITERAL ',' 
       '"withoutRowid"' ':' BOOL_LITERAL ',' 
       '"isAdded"' ':' BOOL_LITERAL ',' 
       opt_added_version 
       '"isDeleted"' ':' BOOL_LITERAL ',' 
       opt_deleted_version 
       '"isRecreated"' ':' BOOL_LITERAL ',' 
       opt_recreate_group_name 
       opt_region_info 
       opt_table_indices 
       opt_attributes 
       '"columns"' ':' '[' columns ']' ',' 
       '"primaryKey"' ':' '[' opt_column_names ']' ',' 
       '"foreignKeys"' ':' '[' opt_foreign_keys ']' ',' 
       '"uniqueKeys"' ':' '[' opt_unique_keys ']' 
       '}' 
  ; 
 
opt_added_version: | '"addedVersion"' ':' any_integer ',' opt_added_migration_proc 
  ; 
 
opt_added_migration_proc: | '"addedMigrationProc"' ':' STRING_LITERAL ',' 
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
 
opt_procedure_names: | procedure_names 
  ; 
 
procedure_names: STRING_LITERAL | STRING_LITERAL ',' procedure_names 
  ; 
 
sort_order_names: STRING_LITERAL | STRING_LITERAL ',' sort_order_names 
  ; 
 
columns: column | column ',' columns 
  ; 
 
column: '{' 
        '"name"' ':' STRING_LITERAL ',' 
        opt_attributes 
        '"type"' ':' STRING_LITERAL ',' 
        opt_is_sensitive 
        '"isNotNull"' ':' BOOL_LITERAL ',' 
        '"isAdded"' ':' BOOL_LITERAL ',' 
        opt_added_version 
        '"isDeleted"' ':' BOOL_LITERAL ',' 
        opt_deleted_version 
        '"isPrimaryKey"' ':' BOOL_LITERAL ',' 
        '"isUniqueKey"' ':' BOOL_LITERAL ',' 
        '"isAutoIncrement"' ':' BOOL_LITERAL 
        opt_default_value 
        '}' 
  ; 
 
opt_default_value: | ',' '"defaultValue"' ':' any_literal 
  ; 
 
opt_foreign_keys : | foreign_keys 
  ; 
 
opt_is_sensitive: | '"isSensitive"' ':' '1' ',' 
  ; 
 
foreign_keys :  foreign_key | foreign_key ',' foreign_keys 
  ; 
 
foreign_key : '{' 
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
              '"columns"' ':' '[' column_names ']' 
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
 
opt_views: | views 
  ; 
 
views: view | view ',' views 
  ; 
 
view:  '{' 
       '"name"' ':' STRING_LITERAL ',' 
       '"isTemp"' ':' BOOL_LITERAL ',' 
       '"isDeleted"' ':' BOOL_LITERAL ',' 
       opt_deleted_version 
       opt_region_info 
       projection 
       '"select"' ':' STRING_LITERAL ',' 
       '"selectArgs"' ':' '[' ']' 
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
        '"table"' ':' STRING_LITERAL ',' 
        '"isUnique"' ':' BOOL_LITERAL ',' 
        '"ifNotExists"' ':' BOOL_LITERAL ',' 
        '"isDeleted"' ':' BOOL_LITERAL ',' 
        opt_deleted_version 
        opt_region_info 
        '"columns"' ':' '[' column_names ']' ',' 
        '"sortOrders"' ':' '[' sort_order_names ']' 
       '}' 
  ; 
 
opt_triggers: | triggers 
  ; 
 
triggers: trigger | trigger ',' triggers 
  ; 
 
trigger: '{' 
          '"name"' ':' STRING_LITERAL ',' 
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
            '"usesTables"' ':' '[' opt_table_names ']' 
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
      '"type"' ':' STRING_LITERAL ',' 
      opt_is_sensitive 
      '"isNotNull"' ':' BOOL_LITERAL 
      '}' 
  ; 
 
opt_inserts: | inserts 
  ; 
 
inserts: insert | insert ',' inserts 
  ; 
 
insert : '{' 
         '"name"' ':' STRING_LITERAL ',' 
         '"definedInFile"' ':' STRING_LITERAL ',' 
         '"args"' ':' '[' opt_args ']' ',' 
         dependencies ',' 
         opt_region_info 
         opt_attributes 
         '"table"' ':' STRING_LITERAL ',' 
         '"statement"' ':' STRING_LITERAL ',' 
         '"statementArgs"' ':' '[' opt_arg_names ']' ',' 
         '"statementType"' ':' STRING_LITERAL ',' 
         '"columns"' ':' '[' column_names ']' ',' 
         '"values"' ':' '[' opt_values ']' 
         '}' 
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
          '"args"' ':' '[' opt_complex_args ']' ',' 
          dependencies ',' 
          opt_regions 
          opt_attributes 
          '"hasStructResult"' ':' BOOL_LITERAL ',' 
          '"hasRowsetResult"' ':' BOOL_LITERAL ',' 
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
              opt_binding 
              '"name"' ':' STRING_LITERAL ',' 
              '"type"' ':' STRING_LITERAL ',' 
              opt_is_sensitive 
              '"isNotNull"' ':' BOOL_LITERAL 
             '}' 
  ; 
 
opt_binding: | '"binding"' ':' STRING_LITERAL ',' 
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
 
ad_hoc_migrations: ad_hoc_migration | ad_hoc_migrations ',' ad_hoc_migrations 
  ; 
 
ad_hoc_migration: '{' 
                  '"name"' ':' STRING_LITERAL ',' 
                  '"version"' ':' any_integer 
                  '}' 
  ; 
 
```


<div style="page-break-after: always; visibility: hidden"></div>


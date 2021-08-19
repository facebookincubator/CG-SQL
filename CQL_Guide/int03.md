---
id: int03
title: "Part 3: C Code Generation"
sidebar_label: "Part 3: C Code Generation"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 3 continues with a discussion of the essentials of the C code generation pass of the CQL compiler.
As in the previous sections, the goal here is not to go over every detail of code generation but rather to give
a sense of how codegen happens in general -- the core strategies and implementation choices --
so that when reading the code you have an idea how smaller pieces would fit into the whole. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## C Code Generation

There are several key pieces of C code that we have to generate to make working CQL procedures using C
functions.  This all happens in `cg_c.c`.  From a big picture perspective, these are the essential problems:

* we have to compile SQL expressions into C
  * including expressions with variables that are nullable
  * including SQL expressions that are highly complex like `CASE..WHEN..THEN..END` and `IN (..)`
* we have to generate control flow for things like `IF`, `WHILE` and, `SWITCH`
* we have to make result sets
  * including the code to slurp up all the rows from a SQL statement into an array of values
  * we want to do this very economically
* we have to be able to create the text for every SQLite statement and bind any variables to it
* we have to check every SQLite API for errors and throw exceptions consistently and deal with them
  * including constructs that allow users to handle exceptions, such as `TRY/CATCH`
* we have to track any reference types carefully so that retain/release pairs are done consistently
  * even in the presence of SQLite errors or other exceptions
* we have to produce a `.h` and a `.c` file for the C compiler
  * contributions to these files could come from various places
  * the `.c` file will itself have various sections and we might need to contribute to them at various points in the compilation
* we want to do this all in one pass over the AST
* we get to assume that the program is error free, codegen never runs unless semantic analysis reports zero errors
  * so nothing can be wrong by the time the codegen pass runs, we never detect errors here
  * sometimes we add `Contract` and `Invariant` statements to `cg.c` that make our assumptions clear and prevent regressions

There are some very important building blocks used to solve these problems we will start with those, then move to
a discussion of each of the essential kinds of code generation that we have to do to get working programs.

### Character Buffers and Byte Buffers

The first kind of text output that CQL could produce was the AST echoing.  This was original done directly with `fprintf` but
that was not flexible enough as the output had to be captured to be emitted into other places like comments or the text of
SQL statements to go to SQLite.  This forces that pass to use character buffers, which we touched on in Part 1.  Code generation
has a more profound dependency on character buffers -- they are literally all over `cg_c.c` and we need to go over how hey are used.

The public interace is in `charbuf.h` and it's really quite simple.  You allocate a `charbuf` and then you can `bprintf` into it.
Let's be a bit more specific:

```C
#define CHARBUF_INTERNAL_SIZE 1024
#define CHARBUF_GROWTH_SIZE 1024

typedef struct charbuf
{
  char *ptr;      // pointer to stored data, if any
  uint32_t used;  // bytes used in current buffer
  uint32_t max;   // max bytes in current buffer

  // builtin buffer storage
  char internal[CHARBUF_INTERNAL_SIZE];
} charbuf;

cql_data_decl( int32_t charbuf_open_count );

cql_noexport void bopen(charbuf* b);
cql_noexport void bclose(charbuf *b);
cql_noexport void bprintf(charbuf *b, const char *format, ...);
```

The typical pattern goes something like this:

```C
  charbuf foo;
  bopen(&foo);
  bprintf(&foo, "Hello %s\n", "World");
  // do something with foo.ptr
  bclose(&foo);
```

Note that `charbuf` includes `CHARBUF_INTERNAL_SIZE` of storage that does not
have to be allocated with `malloc` and it doesn't grow very aggressively.
This reflects that fact that most `charbuf` instances are very small.
Of course a `charbuf` could go on the heap if it needs to outlive
the function it appears in, but this is exceedingly rare.

To make sure buffers are consistently closed (and this is a problem because
there are often a lot of them.  They are allocated with these simple helper
macros.

```C
#define CHARBUF_OPEN(x) \
  int32_t __saved_charbuf_count##x = charbuf_open_count; \
  charbuf x; \
  bopen(&x)

#define CHARBUF_CLOSE(x) \
  bclose(&x); \
  Invariant(__saved_charbuf_count##x == charbuf_open_count)
```

the earlier example would be written more properly:

```C
  CHARBUF_OPEN(foo);
    bprintf(&foo, "Hello %s\n", "World");
    // do something with foo.ptr
  CHARBUF_CLOSE(foo);
```

If you forget to close a buffer the count will get messed up and the next close will trigger an assertion failure.

It's normal to create several buffers in the course of doing code generation.  In fact some of these buffers
become "globally" visible and get swapped out as needed.  For instance this kind of chaining is normal.
Inside of `cg_create_proc_stmt` there is these sequence:

Make new buffers...
```C
  CHARBUF_OPEN(proc_fwd_ref);
  CHARBUF_OPEN(proc_body);
  CHARBUF_OPEN(proc_locals);
  CHARBUF_OPEN(proc_cleanup);
```

Save what we got...
```C
  charbuf *saved_main = cg_main_output;
  charbuf *saved_decls = cg_declarations_output;
  charbuf *saved_scratch = cg_scratch_vars_output;
  charbuf *saved_cleanup = cg_cleanup_output;
  charbuf *saved_fwd_ref = cg_fwd_ref_output;
```

Switch to the new...
```C
  cg_fwd_ref_output = &proc_fwd_ref;
  cg_main_output = &proc_body;
  cg_declarations_output = &proc_locals;
  cg_scratch_vars_output = &proc_locals;
  cg_cleanup_output = &proc_cleanup;
```

And of course the code puts the original values back when it's done and closes the buffers.

This means that while processing a procedure the codegen that declares say scratch variables,
which would go to `cg_scratch_vars_output` is going to target the `proc_locals` buffer
which will be emitted before the `body`.  By the time `cg_stmt_list` is invoked the
`cg_main_output` variable will be pointing to the procedure body, thus any statements
will go into there rather than being acculated at the global level -- it's possible to
have code that is not in a procedure (see [`--global_proc`](https://cgsql.dev/cql-guide/x1#--global_proc-name)).

But in general, it's very useful to have different buffers going on at the same time.  New local variables
or scratch variables can be added to their own buffer which goes before the code runs.  New cleanup
steps that are necessary can be added to the cleanup output which will appear at the end.  The final
function combines all of these pieces with maybe some glue.  Everything works like this, `IF` statements,
expressions, all of it.

One interesting but unexpected feature of `charbuf` is that it provides helper methods for indenting
buffer by whatever amount you like.  This turns out to be invaluable in creating well formatted C
code because of course you want (e.g.) the body of an `if` statement to be indented.  CQL tries to create
well formatted code that is readable by humans as much as possible.

#### Byte Buffers

These are less commonly used but there is a peer to `charbuf` creatively called `bytebuf`.  This gives you
a growable binary buffer.  It's often used to hold arrays of structures.  Interestingly, `cg_c.c` doesn't
currently consume byte buffers, the presence of `bytebuf.c` actually came late to the CQL compiler. However
the CQL runtime `cqlrt.c` (and `cqlrt_common.c`) provide `cql_bytebuf_open`, `cql_bytebuf_alloc` and,
`cql_bytebuf_close` which are akin to the `charbuf` methods.  These functions are used in the generated
code to create result sets at runtime.  The `bytebuf` was so useful that it found its way back from the
runtime into the compiler itself, and is used by other code-generators like the schema upgrader.   The
semantic analyzer also uses it to help with query fragments and to track the various upgrade annotations.

Both `charbuf` and `bytebuf` are simple enough that they don't need discussion. It's easier to just read
the code and the comments.

### Expressions

Many of the output needs of CQL stemmed from the base case of creating expressions.  A simple CQL
expression like

```sql
  SET x := x + y;
````

seems innocuous enough, we'd like this to compile to this code:

```C
  x = x + y;
```

And indeed, it might.  Here's some actual output from the compiler:

```C
/*
CREATE PROC p ()
BEGIN
  DECLARE x, y INTEGER NOT NULL;
  SET x := x + y;
END;
*/

#define _PROC_ "p"
void p(void) {
  cql_int32 x = 0;
  cql_int32 y = 0;

  x = x + y;

}
#undef _PROC_
```

(*) the output above was created by using `out/cql --in x --cg x.h x.c --nolines` to avoid all the # directives

Looks easy enough.  And indeed if all expressions were like this, you could do expression compilation pretty simply --
every binary operator would look something like this:

* recurse left
* emit infix operator
* recurse right

This would sort of build up your expressions inside out and your final buffer after all the recursion was done would have
the whole expression.

This doesn't work at all.  To illustrate what goes wrong, we only have to change the test case a tiny bit.  The result
is telling:

```C
/*
CREATE PROC p ()
BEGIN
  DECLARE x, y INTEGER;
  SET x := x + y;
END;
*/

#define _PROC_ "p"
void p(void) {
  cql_nullable_int32 x;
  cql_set_null(x);
  cql_nullable_int32 y;
  cql_set_null(y);

  cql_combine_nullables(x, x.is_null, y.is_null, x.value + y.value);

}
#undef _PROC_
```

All that's happened in the above is that `x` and `y` became nullable variables, that is the `NOT NULL` was
removed from the declaration.  This makes all the difference in the world, and this is a fairly easy case.
The problem is that nullable value types like cql_nullable_int32 have an integer and a boolean and these
don't flow into expressions that use operators like `+`, `-`, `/` and so forth.  This means that even
simple expressions involving nullable types actually expand into several statements.  And, in general,
these statements need a place to put their temporary results to accumulate the answer, so scratch variables
are required to make all this work.

Here's a more realistic example:

```C
/*
CREATE PROC combine (x INTEGER, y INTEGER, OUT result INTEGER)
BEGIN
  SET result := 5 * x + 3 * y;
END;
*/

#define _PROC_ "combine"
void combine(cql_nullable_int32 x, cql_nullable_int32 y, cql_nullable_int32 *_Nonnull result) {
  cql_contract_argument_notnull((void *)result, 3);

  cql_nullable_int32 _tmp_n_int_1;
  cql_set_null(_tmp_n_int_1);
  cql_nullable_int32 _tmp_n_int_2;
  cql_set_null(_tmp_n_int_2);

  cql_set_null(*result); // set out arg to non-garbage
  cql_set_nullable(_tmp_n_int_1, x.is_null, 5 * x.value);
  cql_set_nullable(_tmp_n_int_2, y.is_null, 3 * y.value);
  cql_combine_nullables(*result, _tmp_n_int_1.is_null, _tmp_n_int_2.is_null, _tmp_n_int_1.value + _tmp_n_int_2.value);

}
#undef _PROC_
#pragma clang diagnostic pop
```

* `_tmp_n_int_1` holds the product of x and 5, it's null if `x.is_null` is true
* `_tmp_n_int_2` holds the product of y and 3, it's null if `y.is_null` is true
* `*result` holds the answer, it's null if either of `_tmp_n_int_1.is_null`, `_tmp_n_int_2.is_null` is true
   * otherwise it's `_tmp_n_int_1.value + _tmp_n_int_2.value`

So, in general, we need to emit arbitarily many statements in the course of evaluating even simple looking expressions
and we need good mechanisms to manage that.  This is what we'll talk about in the coming sections.

### Scratch Variables, CG_PUSH_TEMP, CG_POP_TEMP

### CG_PUSH_EVAL, CG_POP_EVAL, value, and is_null

### CG_RESERVE_RESULT_VAR, CG_USE_RESULT_VAR, CG_SETUP_RESULT_VAR, CG_CLEANUP_RESULT_VAR

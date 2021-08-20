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

### Launching the Code Generator

Once semantic analysis is done all of the code generators have the same contract: they
have a main function like `cg_c_main` for the C code generator.  It gets the root of
the AST and it can use the public interface of the semantic analyzer to get additional
information.  See [Part 2](https://cgsql.dev/cql-guide/int02) for those details.

```C
// Main entry point for code-gen.  This will set up the buffers for the global
// variables and any loose calls or DML.  Any code that needs to run in the
// global scope will be added to the global_proc.  This is the only codegen
// error that is possible.  If you need global code and you don't have a global
// proc then you can't proceed.  Semantic analysis doƒesn't want to know that stuff.
// Otherwise all we do is set up the most general buffers for the global case and
// spit out a function with the correct name.
cql_noexport void cg_c_main(ast_node *head) { ... }
```

In addition to initializing its scratch storage, the main entry point also sets up a
symbol table for AST dispatch just like the `gen_` and `sem_` functions do.  Here
are some samples from that table with the most common options:

```C
  DDL_STMT_INIT(drop_table_stmt);
  DDL_STMT_INIT(drop_view_stmt);
  DDL_STMT_INIT(create_table_stmt);
  DDL_STMT_INIT(create_view_stmt);
```
The DDL (Data Definition Lanaguage) statements all get the same handling:  The text of the statement
is generated from the AST. Any variables are bound and then the statement is executed.  The work
is done with `cg_bound_sql_statement` which will be discussed later.

```C
// Straight up DDL invocation.  The ast has the statement, execute it!
// We don't minify the aliases because DDL can have views and the view column names
// can be referred to in users of the view.  Loose select statements can have
// no external references to column aliases.
static void cg_any_ddl_stmt(ast_node *ast) {
  cg_bound_sql_statement(NULL, ast, CG_EXEC|CG_NO_MINIFY_ALIASES);
}
```

DML (Data Manipulation Language) statements are declared similarly:

```
  STD_DML_STMT_INIT(begin_trans_stmt);
  STD_DML_STMT_INIT(commit_trans_stmt);
  STD_DML_STMT_INIT(rollback_trans_stmt);
  STD_DML_STMT_INIT(savepoint_stmt);
  STD_DML_STMT_INIT(delete_stmt);
```

They are handled by `cg_std_dml_exec_stmt`; the processing is identical to
DDL except `CG_MINIFY_ALIASES` is specified.  This allows the code generator
to remove unused column aliases in select statements to save space.

```
// Straight up DML invocation.  The ast has the statement, execute it!
static void cg_std_dml_exec_stmt(ast_node *ast) {
  cg_bound_sql_statement(NULL, ast, CG_EXEC|CG_MINIFY_ALIASES);
}
```

Note that this flag difference only matters for the `create view` statement
but for symmetry all the DDL is handled with one macro and all the DML
with the second macro.

Next, the easiest case... there are a bunch of statements that create
no code-gen at all.  These are type defintions that are interesting
only to the semantic analyzer or other control statements.  Some examples:

```C
  NO_OP_STMT_INIT(declare_enum_stmt);
  NO_OP_STMT_INIT(declare_named_type);
```

Next, the general purpose statement handler.  This creates a mapping
from the `if_stmt` AST node to `cg_if_stmt`.

```C
  STMT_INIT(if_stmt);
  STMT_INIT(switch_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(assign);
```

The next group is the expressions, with precedence and operator specified. There is a lot of code sharing
as you can see from this sample:

```C
  EXPR_INIT(num, cg_expr_num, "num", C_EXPR_PRI_ROOT);
  EXPR_INIT(str, cg_expr_str, "STR", C_EXPR_PRI_ROOT);
  EXPR_INIT(null, cg_expr_null, "NULL", C_EXPR_PRI_ROOT);
  EXPR_INIT(dot, cg_expr_dot, "DOT", C_EXPR_PRI_ROOT);

  EXPR_INIT(mul, cg_binary, "*", C_EXPR_PRI_MUL);
  EXPR_INIT(div, cg_binary, "/", C_EXPR_PRI_MUL);
  EXPR_INIT(mod, cg_binary, "%", C_EXPR_PRI_MUL);
  EXPR_INIT(add, cg_binary, "+", C_EXPR_PRI_ADD);
  EXPR_INIT(sub, cg_binary, "-", C_EXPR_PRI_ADD);
  EXPR_INIT(not, cg_unary, "!", C_EXPR_PRI_UNARY);
  EXPR_INIT(tilde, cg_unary, "~", C_EXPR_PRI_UNARY);
  EXPR_INIT(uminus, cg_unary, "-", C_EXPR_PRI_UNARY);
```

Most (not all) of the binary operators are handled with one function `cg_binary` and likewise
most unary operators are handled with `cg_unary`.

Note: the precedence constants are the `C_EXPR_PRI_*` flavor because parentheses will be
generated based on the C rules at this point.  Importantly, the AST still, and always
has the user-specified order of operations encoded in it, there's no change there.  The
only thing that changes is where parentheses are needed to get the desired result.  Parens
may need to be added and some that were present in the original text might no longer be needed.

e.g.
```SQL
CREATE PROC p ()
BEGIN
  /* NOT is weaker than + */
  LET x := (NOT 1) + (NOT 2);
  SET x := NOT 1 + 2;
END;
```

```C
void p(void) {
  cql_bool x = 0;

  /* ! is stronger than + */
  x = ! 1 + ! 2;
  x = ! (1 + 2);
}
```

Finally, many built-in functions need special codegen.

```C
  FUNC_INIT(coalesce);
  FUNC_INIT(printf);
```

`FUNC_INIT(coalesce)` creates a mapping between the function name `coalesce` and the generator `cg_func_coalesce`.

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

#### Managing Scratch Variables

The function that actually assigns scratch variables is `cg_scratch_var`

```C
// The scratch variable helper uses the given sem_type and the current
// stack level to create a temporary variable name for that type at that level.
// If the variable does not already have a declaration (as determined by the masks)
// then a declaration is added to the scratch_vars section.  This is one of the root
// ways of getting an .is_null and .value back.  Note that not null variables always
// have a .is_null of "0" which becomes important when deciding how to assign
// one result to another.  Everything stays uniform.
static void cg_scratch_var(ast_node *ast, sem_t sem_type, charbuf *var, charbuf *is_null, charbuf *value)
```

The signature is a bit unexpected so we'll go over this, some of this will make more
sense as we learn about expressions generally but this is as good an introduction as any.

* `ast` holds a reference to a variable we want to assign to, this is normally `NULL` for scratch variables, it's not null for the `RESULT` macros which we'll study later, so for now ignore this
* `sem_type` holds the type of the variable we need, it must be a unitary type, optionally with `SEM_TYPE_NOTNULL` set
* `var` is a character buffer that will get the name of the variable
* `is_null` is a character buffer that will get the `is_null` expression for this variable (more below)
* `value` is a character buffer that will get the `value` expression for this variable (more below)

And this is a good time to talk about `is_null` and `value` because they will be everywhere.

Every expression evaluation in the C code generator has two essential results, the text that corresponds to the current
value so far (e.g. "(1+2)*3") and the text for the current expression that will tell you if the result is null,
this could be as simple as "0" for a expression that is known to be not null.  So let's make this a little more concrete:

Suppose you ask for a scratch not null integer we get results like this:

* `var`:  `"_tmp_n_int_1"`
* `is_null`: `"0"`
* `value`: `"_tmp_n_int_1"`

Meaning: if you want the value, use the text "_tmp_n_int_1" if you want to know if the variable is null, use the text "0"
Note: many parts of `cg_c.c` special case an `is_null` value of `"0"` to make better code because such a thing is known to
be not null at compile time.

Now let's suppose you ask for a scratch nullable integer, we get results like this:

* `var`:  `"_tmp_int_1"`
* `is_null`: `"_tmp_int_1.is_null"`
* `value`: `"_tmp_int_1.value"`

So again, you have exactly the text you need to test for null and the test you need to get the value.

Additional notes:

* scratch variables can be re-used, they are on a "stack"
* a bitmask is used to track which scratch variables have aleady had a declaration emitted, so they are only declared once
* the variable name is based on the current value of the `stack_level` variable which is increased in a push/pop fashion as temporaries come in and out of scope
* this strategy isn't perfect, but the C compiler can consolidate locals even if the CQL codegen is not perfect so it ends up being not so bad
* importantly there is one stacklevel variable for all temporaries not one stacklevel for every type of temporary, this seemed like a reasonable simplification


#### Allocating Scratch Variables

The most common reason for a scratch variable is that a temporary is needed for some part of the computation.
The most common reason for a temporary variable is to hold an intermediate result of a computation involving
nullable arithmetic.

These temporaries are created with `CG_PUSH_TEMP` which simply creates the three `charbuf` variables you need and then asks for a
scratch variable of the type you need.  The variables follow a simple naming convention.  The stack level is increased.

```C
// Create buffers for a temporary variable.  Use cg_scratch_var to fill in the buffers
// with the text needed to refer to the variable.  cg_scratch_var picks the name
// based on stack level-and type.
#define CG_PUSH_TEMP(name, sem_type) \
CHARBUF_OPEN(name); \
CHARBUF_OPEN(name##_is_null); \
CHARBUF_OPEN(name##_value); \
cg_scratch_var(NULL, sem_type, &name, &name##_is_null, &name##_value); \
stack_level++;
```

Symetrically, `CG_POP_TEMP` releases the charbufs and restores the stack level.  As with the other macros, these are designed to
make it impossible to forget to free your buffers or get the stack wrong.  In fact, the stack is checked at strategic places
to ensure its back to baseline.  You can always just snapshot `stacklevel`, do some work that should be clean, and then
add an `Invariant` that `stacklevel` is back to where it was.

```C
// Release the buffers for the temporary, restore the stack level.
#define CG_POP_TEMP(name) \
CHARBUF_CLOSE(name##_value); \
CHARBUF_CLOSE(name##_is_null); \
CHARBUF_CLOSE(name); \
stack_level--;
```

#### Recursing Sub-expressions

Now that we understand that we can create scratch variables as needed, it's time to take a look at the typical evaluation patterns
and how the evaluation works within that pattern.  This is everywhere in `cg_c.c`.

So let's look at an actual evaluator, the simplest of them all, this one does code generation for the `NULL` literal.

```C
static void cg_expr_null(ast_node *expr, CSTR op, charbuf *is_null, charbuf *value, int32_t pri, int32_t pri_new) {
  Contract(is_ast_null(expr));
  // null literal
  bprintf(value, "NULL");
  bprintf(is_null, "1");
}
```

Now this may be looking familiar: the signature of the code generator is something very much like the
signature of the the `gen_` functions in the echoing code.  That's really because in some sense
the echoing code is like a very simple code generator itself.

* `expr` : the AST we are generating code for
* `op` : the relevant operator if any (operators share code)
* `is_null` : a `charbuf` into which we can write the `is_null` expression text
* `value` : a `charbuf` into which we can write the `value` expression text
* `pri` : the binding strength of the node above this one
* `pri_new` : the binding strength of this node

This particular generator is going to produce `"NULL"` for the `value` and `"1"` for the `is_null` expression.

`is_null` and `value` are the chief outputs, and the caller will use these to create its own expression results
with recursive logic.  But the expression logic can also write into the statement stream, and as we'll see,
it does.

`pri` and `pri_new` work exactly like they did in the echoing code (see [Part 1](https://cgsql.dev/cql-guide/int01)),
they are used to allow the code generator to decide if it needs to emit parentheses.  But recall that the binding strengths
now will be the C binding strengths NOT the SQL binding strengths (discussed above).

Let's look at one of the simplest operators: the `IS NULL` operator handled by `cg_expr_is_null`

```C
// The code-gen for is_null is one of the easiest.  The recursive call
// produces is_null as one of the outputs.  Use that.  Our is_null result
// is always zero because IS NULL is never, itself, null.
static void cg_expr_is_null(ast_node *expr, charbuf *is_null, charbuf *value) {
  sem_t sem_type_expr = expr->sem->sem_type;

  // expr IS NULL
  bprintf(is_null, "0"); // the result of is null is never null

  // The fact that this is not constant not null for not null reference types reflects
  // the weird state of affairs with uninitualized reference variables which
  // must be null even if they are typed not null.

  if (is_not_nullable(sem_type_expr) && !is_ref_type(sem_type_expr)) {
    // Note, sql has no side-effects so we can fold this away.
    bprintf(value, "0");
  }
  else {
    CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);
    bprintf(value, "%s", expr_is_null.ptr);
    CG_POP_EVAL(expr);
  }
}
```

So walking through this:
* the result of `IS NULL` is never null, so we can immediately put "0" into the `is_null` buffer
* if the operand is a not-null numeric type then the result of `IS NULL` is `0`
* if the operand might actually be null then
  * use `CG_PUSH_EVAL` to recursively do codegen for it
  * copy its `expr_is_null` text into our `value` text

Note: the code reveals one of the big CQL secrets that not null reference variables can be null...  C has the same issue with `_Nonnull` globals.

Now let's look at those helper macros, they are pretty simple:

```C
// Make a temporary buffer for the evaluation results using the canonical naming convention
// burn the stack slot so that any type and numbered temporary that was needed
// won't be re-used until this scope is over.
#define CG_PUSH_EVAL(expr, pri) \
CHARBUF_OPEN(expr##_is_null); \
CHARBUF_OPEN(expr##_value); \
cg_expr(expr, &expr##_is_null, &expr##_value, pri); \
stack_level++;
````

The push macro simply creates buffers to hold the `is_null` and `value` results, then it calls `cg_expr` to dispatch the indicated expression.
The `pri` value provided to this macro represents the binding strength that the callee should assume its parent has.  Usually this is your `pri_new`
value but often you can use `C_EXPR_PRI_ROOT` if you know that, because of your current context, the callee will never need parentheses.

How do we know this here? It seems like the operand of `IS NULL` could be anything surely it might need parentheses?  Let's consider:

* if the operand is of not null numeric type then we aren't even going to evaluate it, we're on the easy "no it's not null" path
* if the operand is nullable then the only place the answer can be stored is in a scratch variable and its `is_null` expression will be exactly like `var.is_null`
* if the operand is a reference type, there are no operators that combine reference types to get more reference types, so again the result must be in a variable, and is `is_null` expression will be like `!var`

None of these require further wrapping regardless of what is above this node in the tree because of he strength of the `.` and `!` operators.

Other cases are usually simpler, such as "no parentheses need to be added by the child node becasue it will be used as the argument to a helper
function so there will always be parens hard-coded anyway".  However these things need to be carefully tested hence the huge variety of codegen tests.

Note that after calling `cg_expr` the stack level was artificially increased.  We'll get to that in the next section.  For now, looking at `POP_EVAL` we
can see it's very straightforward:

```C
// Close the buffers used for the above.  Return the stack level to its original state.
// Numbered scratch variables are re-used as though they were a stack.
#define CG_POP_EVAL(expr) \
CHARBUF_CLOSE(expr##_value); \
CHARBUF_CLOSE(expr##_is_null); \
stack_level--;
```

`CG_POP_EVAL` simply closes the buffers and restores the stack.

#### Result Variables

When recursion happens in the codegen, the common place that the result will be found is
in a temporary variable -- the generated code will use one or more statements to arrange for the correct
answer to be in that variable.  To do this, the codegen needs to first get the name of a suitable
result variable of a suitable type.  This is the "other" reason for making scratch variables.

There are three macros that make this pretty simple.  The first is `CG_RESERVE_RESULT_VAR`

```C
// Make a scratch variable to hold the final result of an evaluation.
// It may or may not be used.  It should be the first thing you put
// so that it is on the top of your stack.  This only saves the slot.
#define CG_RESERVE_RESULT_VAR(ast, sem_type) \
int32_t stack_level_reserved = stack_level; \
sem_t sem_type_reserved = sem_type; \
ast_node *ast_reserved = ast; \
CHARBUF_OPEN(result_var); \
CHARBUF_OPEN(result_var_is_null); \
CHARBUF_OPEN(result_var_value); \
stack_level++;
```

If this looks a lot like `PUSH_TEMP` that shouldn't be surprising.  The name of the variable
and the expression parts always go into `charbuf` variables named `result_var` `result_var_is_null` and `result_var_value`
but the scratch variable isn't actually allocated!  However -- we burn the stack_level as though it had been
allocated.  What's up with that?

The name might be a clue, this macro reserves stack level slot for the result variable, it's used if you might
need a result variable, but you might not.  When you want it we can artificially move the stack level back
to this spot where the slot was burned, allocate the scratch variable, and then put the stack back.
The `CG_USE_RESULT_VAR` macro does exactly that.

```C
// If the result variable is going to be used, this writes its name
// and .value and .is_null into the is_null and value fields.
#define CG_USE_RESULT_VAR() \
int32_t stack_level_now = stack_level; \
stack_level = stack_level_reserved; \
cg_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_is_null, &result_var_value); \
stack_level = stack_level_now; \
Invariant(result_var.used > 1); \
bprintf(is_null, "%s", result_var_is_null.ptr); \
bprintf(value, "%s", result_var_value.ptr)
```

Once the code generator decides that it will in fact be using a result variable to represent the answer, then
the `is_null` and `value` buffers can be immediately populated to whatever the values were
for the result variable.  That text will be correct regardless of what codegen is used
to populate the variable.

There is a simpler macro that reserves and uses the result variable in one step, it's very common.  The
"reserve" pattern is only necessary when there are some paths that need a result variable and some
that don't.

```
// This does reserve and use in one step
#define CG_SETUP_RESULT_VAR(ast, sem_type) \
CG_RESERVE_RESULT_VAR(ast, sem_type); \
CG_USE_RESULT_VAR();
```

And now armed with this knowledge we can go back to a previous mystery, let's look at `CG_PUSH_EVAL` again

```C
// Make a temporary buffer for the evaluation results using the canonical naming convention
// burn the stack slot so that any type and numbered temporary that was needed
// won't be re-used until this scope is over.
#define CG_PUSH_EVAL(expr, pri) \
CHARBUF_OPEN(expr##_is_null); \
CHARBUF_OPEN(expr##_value); \
cg_expr(expr, &expr##_is_null, &expr##_value, pri); \
stack_level++;
````

The reason that `CG_PUSH_EVAL` includes `stack_level++` is that it is entirely possible, even likely,
that the result of `cg_expr` is in a result variable.  The convention is that if the codegen
requires a result variable it is allocated *first* before any other temporaries.  This is why
there is a way to reserve a variable that you *might* need.  When the codegen is complete,
and before anything else happens, `stack_level` is increased so that the temporary that is
holding the result will not be re-used!  Any other temporaries are available but the result
is still live.  This might be easy to get wrong but the macros make it easy to get it right.

Now, armed with the knowledge that there a result variables and temporary variables and both
come from the scratch variable we can resolve the last mystery we left hanging.  Why does
the scratch variable API accept an AST pointer?

The only place that pointer can be not null is in the `CG_USE_RESULT_VAR` macro, it was
this line:

```
cg_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_is_null, &result_var_value);
```

And `ast_reserved` refers to the AST that we are trying to evaluate.  There's an important
special case that we want to optimize that saves a lot of scratch variables.  It's handled
by this code in `cg_scratch_var`:

```C
  // try to avoid creating a scratch variable if we can use the target of an assignment in flight.
  if (is_assignment_target_reusable(ast, sem_type)) {
    Invariant(ast && ast->parent && ast->parent->left);
    EXTRACT_ANY_NOTNULL(name_ast, ast->parent->left);
    EXTRACT_STRING(name, name_ast);
    if (is_out_parameter(name_ast->sem->sem_type)) {
      bprintf(var, "*%s", name);
    }
    else {
      bprintf(var, "%s", name);
    }
  }
```

The idea is that if the generator is doing an assignment like:

```SQL
  SET x := a + b;
```

Then the code generator doesn't need a scratch variable to hold the result of the expression `a + b` like it would
in many other contexts.  It can use `x` as the result variable!  The `SET` codegen will discover
that the value it's supposed to set is already in `x` so it does nothing and everything just
works out.  The price of this is a call to `is_assignment_target_reusable` and then some
logic to handle the case where `x` is an out argument (hence call by reference, hence needs to be used as `*x`).

### Basic Control Flow Patterns

To get a sense of how the compiler generates code for statements, we can look at some of the easiest cases.

```C
// "While" suffers from the same problem as IF and as a consequence
// generating while (expression) would not generalize.
// The overall pattern for while has to look like this:
//
//  for (;;) {
//    prep statements;
//    condition = final expression;
//    if (!condition) break;
//
//    statements;
//  }
//
// Note that while can have leave and continue substatements which have to map
// to break and continue.   That means other top level statements that aren't loops
// must not create a C loop construct or break/continue would have the wrong target.
static void cg_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);
  sem_t sem_type = expr->sem->sem_type;

  // WHILE [expr] BEGIN [stmt_list] END

  bprintf(cg_main_output, "for (;;) {\n");

  CG_PUSH_EVAL(expr, C_EXPR_PRI_ROOT);

  if (is_nullable(sem_type)) {
    bprintf(cg_main_output, "if (!cql_is_nullable_true(%s, %s)) break;\n", expr_is_null.ptr, expr_value.ptr);
  }
  else {
    bprintf(cg_main_output, "if (!(%s)) break;\n", expr_value.ptr);
  }

  bool_t loop_saved = cg_in_loop;
  cg_in_loop = true;

  CG_POP_EVAL(expr);

  cg_stmt_list(stmt_list);

  bprintf(cg_main_output, "}\n");

  cg_in_loop = loop_saved;
}
```

The comment before the `cg_while_stmt` actually says it pretty clearly; the issue is
that the expression in the while statement might actually require many C statements
to evaluate.  There are many cases of this sort of thing, but the simplest is
probably when any nullable types are in that expression.  A particular example
illustrates this pretty clearly.

```C
CREATE PROC p ()
BEGIN
  DECLARE x INTEGER NOT NULL;
  SET x := 1;
  WHILE x < 5
  BEGIN
    SET x := x + 1;
  END;
END;
*/

void p(void) {
  cql_int32 x = 0;

  x = 1;
  for (;;) {
  /* in trickier cases there would be code right here */
  if (!(x < 5)) break;
    x = x + 1;
  }
}
```

In this case, the `while` pattern could have been used because the condition is simply `x < 5` so this whole pattern is
overkill.  But consider this program just a tiny bit different.

```C
/*
CREATE PROC p ()
BEGIN
  DECLARE x INTEGER;  -- x is nullable
  SET x := 1;
  WHILE x < 5
  BEGIN
    SET x := x + 1;
  END;
END;
*/

void p(void) {
  cql_nullable_int32 x;
  cql_set_null(x);
  cql_nullable_bool _tmp_n_bool_0;
  cql_set_null(_tmp_n_bool_0);

  cql_set_notnull(x, 1);
  for (;;) {
  cql_set_nullable(_tmp_n_bool_0, x.is_null, x.value < 5);
  if (!cql_is_nullable_true(_tmp_n_bool_0.is_null, _tmp_n_bool_0.value)) break;
    cql_set_nullable(x, x.is_null, x.value + 1);
  }
}
```

Even for this small little case, the nullable arithmetic macros have to be used to keep `x` up to date.
The result of `x < 5` is of type "bool" rather than "bool not null" so a temporary variable captures
the result of the expression.  This is an easy case but similar things happen if the expression
includes `CASE...WHEN...` or `IN` constructs.  There are many other cases.

So with this in mind, let's reconsider what `cg_while_stmt` is doing:

* we start the `for` statement in the output
  * there's a bprintf for that
* we evaluate the while expression, the details will be in `is_null` and `value`
  * we use CG_PUSH_EVAL for that
* if the result is nullable there is a helper macro `cql_is_nullable_true` that tells us if the value is not null and true
* if the result is not nullable we can use `expr_value.ptr` directly
* we make a note that we're in a loop (this matters for statement cleanup, more on that later)
* we recurse to do more statements with `cg_stmt_list`
* finally we end the `for` that we began

This kind of structure is common to all the control flow cases.  Generally, we have to deal with the
fact that CQL expressions become C statements so we use a more general flow control strategy. But with this
in mind, it's easy to imagine how `IF` `LOOP` and `SWITCH` are handled.

### Cleanup and Errors

There are a number of places where things can go wrong when running a CQL procedure.  The most
common sources are: (1) SQLite APIs, almost all of which can fail, and, (2) calling other procedures
which also might fail.  Here's a very simple example:

```C
/*
DECLARE PROC something_that_might_fail (arg TEXT) USING TRANSACTION;

CREATE PROC p ()
BEGIN
  LET arg := "test";
  CALL something_that_might_fail(arg);
END;
*/

cql_string_literal(_literal_1_test_p, "test");

CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_) {
  cql_code _rc_ = SQLITE_OK;
  cql_string_ref arg = NULL;

  cql_set_string_ref(&arg, _literal_1_test_p);
  _rc_ = something_that_might_fail(_db_, arg);
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_string_release(arg);
  return _rc_;
}
```

Let's look at this carefully:

* first, we had to declare `something_that_might_fail`
  * the declaration includes `USING TRANSACTION` indicating the procedure uses the database
  * we didn't provide the definition, that will lead to a link time error but we're ignoring that for now
* there is a string literal named `_literal_1_test_p` that is auto-created
  * `cql_string_literal` can expand into a variety of things, whatever you want "make a string literal" to mean
  * its defined in `cqlrt.h` and it's designed to be replaced
* `cql_set_string_ref(&arg, _literal_1_test_p);` is expected to "retain" the string (+1 ref count)
* `cql_cleanup` is the exit label, this code will run for sure
  * cleanup statements are accumulated by writing to `cg_cleanup_output` which usually writes to the `proc_cleanup` buffer
  * because cleanup is in its own buffer you can add to it freely whenever a new declaration that requires cleanup arises
  * in this case the declaration of the string literal caused the `C` variable `arg` to be created and also the cleanup code
*  now we call `something_that_might_fail` passing it our database pointer and the argument
  * the hidden `_db_` pointer is passed to all procedures that use the database
  * these are also the ones that can fail
* any failed return code (not `SQLITE_OK`) causes two things
  * the `cql_error_trace()` macro is invoked (this macro typically expands to nothing)
  * the code stops what it's doing and runs the cleanup code via `goto cql_cleanup;`

The essential sequence is this one:

```C
 if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
```

The C code generator uses this pattern all over to check if anything went wrong and to exit with an error code.
Extensive logging can be very expensive but in debug builds, it's quite normal for `cql_error_trace` to expand
into something like `fprintf(stderr, "error %d in %s %s:%d\n", _rc_, _PROC_, __FILE__, __LINE_)` which probably
a lot more logging than you want in a production build but great if you're debugging.  Recall that CQL generates
`#define _PROC_ "p"` before every procedure.

This pattern generalizes well and indeed if we use the exception handling pattern, we get a lot of control.
Let's generalize this a tiny bit.

```SQL
CREATE PROC p (OUT success BOOL NOT NULL)
BEGIN
  LET arg := "test";
  BEGIN TRY
    CALL something_that_might_fail(arg);
    SET success := 1;
  END TRY;
  BEGIN CATCH
    SET success := 0;
  END CATCH;
END;
```

CQL doesn't have complicated exception objects or anything like that, exceptions are just simple
control flow.  Here's the code for the above:

```C
CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_, cql_bool *_Nonnull success) {
  cql_contract_argument_notnull((void *)success, 1);

  cql_code _rc_ = SQLITE_OK;
  cql_string_ref arg = NULL;

  *success = 0; // set out arg to non-garbage
  cql_set_string_ref(&arg, _literal_1_test_p);
  // try
  {
    _rc_ = something_that_might_fail(_db_, arg);
    if (_rc_ != SQLITE_OK) { cql_error_trace(); goto catch_start_1; }
    *success = 1;
    goto catch_end_1;
  }
  catch_start_1: {
    *success = 0;
  }
  catch_end_1:;
  _rc_ = SQLITE_OK;

  cql_string_release(arg);
  return _rc_;
}
```

The code is nearly the same.  Let's look at the essential differences:

* If there is an error, the code hits `goto catch_start_1`
* If the try block succeeds, the code hits `goto catch_end_1`
* both branches set the `success` out parameter
* we added that out argument, CQL generated an error check to ensure that arg 1 is not null
  * `cql_contract_argument_notnull((void *)success, 1)`, the 1 means arg 1
  * the hidden `_db_` arg doesn't count

How does this happen?  `cg_trycatch_helper`

```C
// Very little magic is needed to do try/catch in our context.  The error
// handlers for all the sqlite calls check _rc_ and if it's an error they
// "goto" the current error target.  That target is usually CQL_CLEANUP_DEFAULT_LABEL.
// Inside the try block, the cleanup handler is changed to the catch block.
// The catch block puts it back.  Otherwise, generate nested statements as usual.
static void cg_trycatch_helper(ast_node *try_list, ast_node *try_extras, ast_node *catch_list) {
  CHARBUF_OPEN(catch_start);
  CHARBUF_OPEN(catch_end);

  // We need unique labels for this block
  ++catch_block_count;
  bprintf(&catch_start, "catch_start_%d", catch_block_count);
  bprintf(&catch_end, "catch_end_%d", catch_block_count);

  // Divert the error target.
  CSTR saved_error_target = error_target;
  bool_t saved_error_target_used = error_target_used;
  error_target = catch_start.ptr;
  error_target_used = 0;
 ...
```
All of the error handling does goto `error_target` whatever that is.  The
try/catch pattern simply changes the current error target.  The rest of
the code is just to save the current error target and to create unique
labels for the the control flow.

The important notion is that, if anything goes wrong, whatever it is,
the generator simply does a `goto error_target` and that will either
hit the catch block or else go to cleanup.

The `THROW` operation illustrates this well:

```C
// Convert _rc_ into an error code.  If it already is one keep it.
// Then go to the current error target.
static void cg_throw_stmt(ast_node *ast) {
  Contract(is_ast_throw_stmt(ast));

  bprintf(cg_main_output, "_rc_ = cql_best_error(%s);\n", rcthrown_current);
  bprintf(cg_main_output, "goto %s;\n", error_target);
  error_target_used = 1;
  rcthrown_used = 1;
}
```

* first we make sure _rc_ has some kind of error in it either `rcthrown_current` or else `SQLITE_ERROR`
* then we goto the current error target
* `error_target_used` tracks whether there were any possible errors, this is just to avoid C compiler errors about unused labels.
  * if the label is not used it won't be emitted
  * the code never jump back to an error label so we'll always know if it was used before we need to emit it

Note: every catch block captures the value of `_rc_` in a local variable whose name is in `rcthrown_current`.
This is the current failing result code accessible by `@RC` in CQL.

A catch block can therefore do stuff like:

```sql
IF @RC = 1 THEN
  THROW;
ELSE
  call attempt_retry();
END IF;
```

or something like that.

This entire mechanism is built with basically just a few state variables that nest.  There is no complicated stack walking
or anything like that.  All the code has to do is chain the error labels together and let users create new catch blocks
with new error labels.  All that together gives you very flexible try/catch behaviour.

---
id: int03
title: "Part 3: C Code Generation"
sidebar_label: "Part 3: C Code Generation"
---
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 3 continues with a discussion of the essentials of the C code generation pass of the CQL compiler.
As in the previous sections, the goal here is not to go over every detail of code generation but rather to give
a sense of how codegen happens in general -- the core strategies and implementation choices --
so that when reading the code you will have an idea how smaller pieces would fit into the whole. To accomplish
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
  * contributions to these files could come from various places, not necessarily in order
  * the `.c` file will itself have various sections and we might need to contribute to them at various points in the compilation
* we want to do this all in one pass over the AST
* we get to assume that the program is error-free -- codegen never runs unless semantic analysis reports zero errors
  * so nothing can be wrong by the time the codegen pass runs, we never detect errors here
  * sometimes we add `Contract` and `Invariant` statements to `cg.c` to make our assumptions clear and to prevent regressions

There are some very important building blocks used to solve these problems: we will start with those, then move to
a discussion of each of the essential kinds of code generation that we have to do to get working programs.

### Launching the Code Generator

Once semantic analysis is done, all of the code generators have the same contract: they
have a main function like `cg_c_main` for the C code generator.  It gets the root of
the AST and it can use the public interface of the semantic analyzer to get additional
information.  See [Part 2](https://cgsql.dev/cql-guide/int02) for those details.

```c
// Main entry point for code-gen.  This will set up the buffers for the global
// variables and any loose calls or DML.  Any code that needs to run in the
// global scope will be added to the global_proc.  This is the only codegen
// error that is possible.  If you need global code and you don't have a global
// proc then you can't proceed.  Semantic analysis doesn't want to know that stuff.
// Otherwise all we do is set up the most general buffers for the global case and
// spit out a function with the correct name.
cql_noexport void cg_c_main(ast_node *head) { ... }
```

In addition to initializing its scratch storage, the main entry point also sets up a
symbol table for AST dispatch just like the `gen_` and `sem_` functions do.  Here
are some samples from that table with the most common options:

```c
  DDL_STMT_INIT(drop_table_stmt);
  DDL_STMT_INIT(drop_view_stmt);
  DDL_STMT_INIT(create_table_stmt);
  DDL_STMT_INIT(create_view_stmt);
```
The DDL (Data Definition Language) statements all get the same handling:  The text of the statement
is generated from the AST. Any variables are bound and then the statement is executed.  The work
is done with `cg_bound_sql_statement` which will be discussed later.

```c
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

The DML statements are handled by `cg_std_dml_exec_stmt`; the processing is identical to
DDL except `CG_MINIFY_ALIASES` is specified.  This allows the code generator
to remove unused column aliases in `SELECT` statements to save space.

```
// Straight up DML invocation.  The ast has the statement, execute it!
static void cg_std_dml_exec_stmt(ast_node *ast) {
  cg_bound_sql_statement(NULL, ast, CG_EXEC|CG_MINIFY_ALIASES);
}
```

Note that this flag difference only matters for the `CREATE VIEW` statement
but for symmetry all the DDL is handled with one macro and all the DML
with the second macro.

Next, the easiest case... there are a bunch of statements that create
no code-gen at all.  These statements are type definitions that are interesting
only to the semantic analyzer, or other control statements.  Some examples:

```c
  NO_OP_STMT_INIT(declare_enum_stmt);
  NO_OP_STMT_INIT(declare_named_type);
```

Next, the general purpose statement handler.  `STMT_INIT` creates mappings
such as the `if_stmt` AST node mapping to `cg_if_stmt`.

```c
  STMT_INIT(if_stmt);
  STMT_INIT(switch_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(assign);
```

The next group of declarations are the expressions, with precedence and operator specified.
There is a lot of code sharing between AST types as you can see from this sample:

```c
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

Note: the precedence constants are the `C_EXPR_PRI_*` flavor because, naturally, parentheses
will be generated based on the C rules during C codegen.  Importantly, the AST still, and always,
authoritatively encodes the user-specified order of operations -- there's no change there.  The
only thing that changes is where parentheses are needed to get the desired result.  Some parens
may need to be added, and some that were present in the original text might no longer be needed.

Here are some helpful examples:

```SQL
CREATE PROC p ()
BEGIN
  /* NOT is weaker than + */
  LET x := (NOT 1) + (NOT 2);
  SET x := NOT 1 + 2;
END;
```

```c
void p(void) {
  cql_bool x = 0;

  /* ! is stronger than + */
  x = ! 1 + ! 2;
  x = ! (1 + 2);
}
```

Finally, many built-in functions need special codegen, such as:

```c
  FUNC_INIT(coalesce);
  FUNC_INIT(printf);
```

`FUNC_INIT(coalesce)` creates a mapping between the function name `coalesce` and the generator `cg_func_coalesce`.

### Character Buffers and Byte Buffers

The first kind of text output that CQL could produce was the AST echoing.  This was originally done directly with `fprintf` but
that was never going to be flexible enough -- we have to be able to emit that output into other places like comments, or the text of
SQL statements.  This need forces that pass to use character buffers, which we touched on in Part 1.  C Code generation
has a more profound dependency on character buffers -- they are literally all over `cg_c.c` and we need to go over how they are used
if we're going to understand the codegen passes.

The public  interface for `charbuf` is in `charbuf.h` and it's really quite simple.  You allocate a `charbuf` and then you can
`bprintf` into it. Let's be a bit more specific:

```c
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

```c
  charbuf foo;
  bopen(&foo);
  bprintf(&foo, "Hello %s\n", "World");
  // do something with foo.ptr
  bclose(&foo);
```

Note that `charbuf` includes `CHARBUF_INTERNAL_SIZE` of storage that does not
have to be allocated with `malloc` and it doesn't grow very aggressively.
This economy reflects that fact that most `charbuf` instances are very small.
Of course a `charbuf` could go on the heap if it needs to outlive
the function it appears in, but this is exceedingly rare.

To make sure buffers are consistently closed -- and this is a problem because
there are often a lot of them -- they are allocated with these simple helper
macros:

```c
#define CHARBUF_OPEN(x) \
  int32_t __saved_charbuf_count##x = charbuf_open_count; \
  charbuf x; \
  bopen(&x)

#define CHARBUF_CLOSE(x) \
  bclose(&x); \
  Invariant(__saved_charbuf_count##x == charbuf_open_count)
```

The earlier example would be written more properly:

```c
  CHARBUF_OPEN(foo);
    bprintf(&foo, "Hello %s\n", "World");
    // do something with foo.ptr
  CHARBUF_CLOSE(foo);
```

If you forget to close a buffer the count will get messed up and the next close will trigger an assertion failure.

It's normal to create several buffers in the course of doing code generation.  In fact some of these buffers
become "globally" visible and get swapped out as needed.  For instance, the kind of chaining we see
inside of `cg_create_proc_stmt` is normal, here is the sequence:

Make new buffers...
```c
  CHARBUF_OPEN(proc_fwd_ref);
  CHARBUF_OPEN(proc_body);
  CHARBUF_OPEN(proc_locals);
  CHARBUF_OPEN(proc_cleanup);
```

Save the current buffer pointers...
```c
  charbuf *saved_main = cg_main_output;
  charbuf *saved_decls = cg_declarations_output;
  charbuf *saved_scratch = cg_scratch_vars_output;
  charbuf *saved_cleanup = cg_cleanup_output;
  charbuf *saved_fwd_ref = cg_fwd_ref_output;
```

Switch to the new buffers...
```c
  cg_fwd_ref_output = &proc_fwd_ref;
  cg_main_output = &proc_body;
  cg_declarations_output = &proc_locals;
  cg_scratch_vars_output = &proc_locals;
  cg_cleanup_output = &proc_cleanup;
```

And of course the code puts the original values back when it's done and then closes what it opened.

This means that while processing a procedure the codegen that declares say scratch variables,
which would go to `cg_scratch_vars_output`, is going to target the `proc_locals` buffer
which will be emitted before the `proc_body`.  By the time `cg_stmt_list` is invoked the
`cg_main_output` variable will be pointing to the procedure body, thus any statements
will go into there rather than being accumulated at the global level.

Note: it's possible to have code that is not in a procedure (see [`--global_proc`](https://cgsql.dev/cql-guide/x1#--global_proc-name)).

In general, it's very useful to have different buffers open at the same time.  New local variables
or scratch variables can be added to their own buffer. New cleanup steps that are necessary can be added to
`cg_cleanup_output` which will appear at the end of a procedure. The final steps of procedure codegen
combines all of these pieces plus a little glue to make a working procedure.

All codegen works like this -- statements, expressions, all of it.

One interesting but unexpected feature of `charbuf` is that it provides helper methods for indenting
a buffer by whatever amount you like.  This turns out to be invaluable in creating well formatted C
code because of course we want (e.g.) the body of an `if` statement to be indented.  CQL tries to create
well formatted code that is readable by humans as much as possible.

#### Byte Buffers

The byte buffers type, creatively called `bytebuf` is less commonly used.  It is a peer to `charbuf`
and provides a growable binary buffer.  `bytebuf` is often used to hold arrays of structures.
Interestingly, `cg_c.c` doesn't currently consume byte buffers, the presence of `bytebuf.c` actually
came late to the CQL compiler. However the CQL runtime `cqlrt.c` (and `cqlrt_common.c`) provide
`cql_bytebuf_open`, `cql_bytebuf_alloc` and, `cql_bytebuf_close` which are akin to the `charbuf` methods.
These functions *are* used in the generated code to create result sets at runtime.

`bytebuf` was so useful that it found its way back from the runtime into the compiler itself, and is used by
other code-generators like the schema upgraded.   The semantic analyzer also uses it to help with query
fragments and to track the various upgrade annotations.

Both `charbuf` and `bytebuf` are simple enough that they don't need special discussion. Surveying
their code and comments is an excellent exercise for the reader.

### Expressions

Many of the output needs of CQL stemmed from the base case of creating the code for CQL expressions.
A simple CQL expression like:

```sql
  SET x := x + y;
````

seems innocuous enough, and we'd like that expression to compile to this code:

```c
  x = x + y;
```

And indeed, it does.  Here's some actual output from the compiler:

```c
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

(*) the output above was created by using `out/cql --in x --cg x.h x.c --nolines` to avoid all the `#` directives

That expression looks easy enough. And indeed if all expressions were like this, we could do expression compilation
pretty simply -- every binary operator would look something like this:

* recurse left
* emit infix operator
* recurse right

This would sort of build up your expressions inside out and your final buffer after all the recursion was done would have
the whole expression.

This doesn't work at all.

To illustrate what goes wrong, we only have to change the test case a tiny bit.  The result is telling:

```c
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

In this new example above, `x` and `y` became nullable variables i.e. the `NOT NULL` was
removed from their declarations -- this makes all the difference in the world.

Let's take a quick look at `cql_nullable_int32` and we'll see the crux of the problem immediately:

```c
typedef struct cql_nullable_int32 {
 cql_bool is_null;
 cql_int32 value;
} cql_nullable_int32;
```

The problem is that nullable value types like `cql_nullable_int32` have both their `value` field
and a boolean `is_null` and these don't flow into expressions that use operators like `+`, `-`, `/` and so forth.
This means that even simple expressions involving nullable types actually expand into several statements.  And, in general,
these statements need a place to put their temporary results to accumulate the correct answer, so scratch variables
are required to make all this work.

Here's a more realistic example:

```c
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

* `_tmp_n_int_1` : holds the product of x and 5, it's null if `x.is_null` is true
* `_tmp_n_int_2` : holds the product of y and 3, it's null if `y.is_null` is true
* `*result` : holds the answer, it's null if either of `_tmp_n_int_1.is_null`, `_tmp_n_int_2.is_null` is true
   * otherwise it's `_tmp_n_int_1.value + _tmp_n_int_2.value`

So, in general, we need to emit arbitrarily many statements in the course of evaluating even simple looking expressions
and we need good mechanisms to manage that.  This is what we'll talk about in the coming sections.

#### Managing Scratch Variables

The function that actually assigns scratch variables is `cg_scratch_var`

```c
// The scratch variable helper uses the given sem_type and the current
// stack level to create a temporary variable name for that type at that level.
// If the variable does not already have a declaration (as determined by the masks)
// then a declaration is added to the scratch_vars section.  This is one of the root
// ways of getting an .is_null and .value back.  Note that not null variables always
// have a .is_null of "0" which becomes important when deciding how to assign
// one result to another.  Everything stays uniform.
static void cg_scratch_var(ast_node *ast, sem_t sem_type, charbuf *var, charbuf *is_null, charbuf *value)
```

The signature is a bit unexpected so we'll go over it, some of below will make more
sense as we learn about expressions generally, but this is as good an introduction as any.

* `ast` : holds a reference to a variable we want to assign to
  * this argument is normally `NULL` for scratch variables
  * `ast` is not null for the `RESULT` macros which we'll study later
  * for now, we can basically ignore this argument
* `sem_type` : holds the type of the variable we need
  * it must be a unitary type, optionally with `SEM_TYPE_NOTNULL` set
* `var` : a character buffer that will get the name of the variable
* `is_null` : a character buffer that will get the `is_null` expression for this variable (more below)
* `value` : a character buffer that will get the `value` expression for this variable (more below)

And this is a good time to talk about `is_null` and `value` because they will be everywhere.

The codegen for expressions in the C code generator produces two results:
* the text that corresponds to the current value so far (e.g. "(1+2)*3"), and,
* the text that will tell you if the current value is null
  * this could be as simple as "0" for an expression that is known to be not null

Let's make this a little more concrete:

Suppose we ask for a scratch "not null integer", we get results like this:

* `var`:  `"_tmp_n_int_1"`
* `is_null`: `"0"`
* `value`: `"_tmp_n_int_1"`

Meaning: if we want the value, use the text `"_tmp_n_int_1"` if we want to know if the variable is null, we use the text `"0"`

Note: many parts of `cg_c.c` special case an `is_null` value of `"0"` to make better code because such a thing is known to
be not null at compile time.

Now let's suppose we ask for a scratch nullable integer, we get results like this:

* `var`:  `"_tmp_int_1"`
* `is_null`: `"_tmp_int_1.is_null"`
* `value`: `"_tmp_int_1.value"`

So again, we have exactly the text we need to test for null, and the test we need to get the value.

Additional notes:

* scratch variables can be re-used, they are on a "stack"
* a bitmask is used to track which scratch variables have already had a declaration emitted, so they are only declared once
* the variable name is based on the current value of the `stack_level` variable which is increased in a push/pop fashion as temporaries come in and out of scope
  * this strategy isn't perfect, but the C compiler can consolidate locals even if the CQL codegen is not perfect so it ends up being not so bad
  * importantly, there is one `stack_level` variable for all temporaries not one `stack_level` for every type of temporary, this seemed like a reasonable simplification


#### Allocating Scratch Variables

The most common reason to create a "scratch" variable is that a temporary variable is needed for some part of the computation.
The most common reason for a temporary variable is to hold an intermediate result of a computation involving nullable arithmetic.

These temporaries are created with `CG_PUSH_TEMP` which simply creates the three `charbuf` variables needed and then asks for a
scratch variable of the required type.  The variables follow a simple naming convention.  The stack level is increased after
each temporary is allocated.

```c
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

Symmetrically, `CG_POP_TEMP` closes the `charbuf` variables and restores the stack level.

```c
// Release the buffers for the temporary, restore the stack level.
#define CG_POP_TEMP(name) \
  CHARBUF_CLOSE(name##_value); \
  CHARBUF_CLOSE(name##_is_null); \
  CHARBUF_CLOSE(name); \
  stack_level--;
```

As with the other `PUSH/POP` `OPEN/CLOSE` macro types, these macros are designed to make it impossible
to forget to free the buffers, or to get the stack level wrong.  The stack level can be (and is) checked
at strategic places to ensure it's back to baseline -- this is easy because the code can always just
snapshot `stack_level`, do some work that should be clean, and then check that `stack_level` is back to
 where it's supposed to be with an `Invariant`.

#### Recursing Sub-expressions

Now that we understand that we can create scratch variables as needed, it's time to take a look at the typical evaluation patterns
and how the evaluation works within that pattern.  This is everywhere in `cg_c.c`.

So let's look at an actual evaluator, the simplest of them all, this one does code generation for the `NULL` literal.

```c
static void cg_expr_null(
  ast_node *expr,
  CSTR op,
  charbuf *is_null,
  charbuf *value,
  int32_t pri,
  int32_t pri_new)
{
  Contract(is_ast_null(expr));
  // null literal
  bprintf(value, "NULL");
  bprintf(is_null, "1");
}
```

Now this may be looking familiar: the signature of the code generator is something very much like the
signature of the the `gen_` functions in the echoing code.  That's really because in some sense
the echoing code is a very simple code generator itself.

* `expr` : the AST we are generating code for
* `op` : the relevant operator if any (operators share code)
* `is_null` : a `charbuf` into which we can write the `is_null` expression text
* `value` : a `charbuf` into which we can write the `value` expression text
* `pri` : the binding strength of the node above this one
* `pri_new` : the binding strength of this node

This particular generator is going to produce `"NULL"` for the `value` and `"1"` for the `is_null` expression.

`is_null` and `value` are the chief outputs, and the caller will use these to create its own expression results
with recursive logic.  But the expression logic can also write into the statement stream, the cleanup stream,
even into the header file stream, and as we'll see, it does.

`pri` and `pri_new` work exactly like they did in the echoing code (see [Part 1](https://cgsql.dev/cql-guide/int01)),
they are used to allow the code generator to decide if it needs to emit parentheses.  But recall that the binding strengths
now will be the C binding strengths NOT the SQL binding strengths (discussed above).

Let's look at one of the simplest operators: the `IS NULL` operator handled by `cg_expr_is_null`

Note: this code has a simpler signature because it's actually part of codegen for `cg_expr_is` which
has the general contract.

```c
// The code-gen for is_null is one of the easiest.  The recursive call
// produces is_null as one of the outputs.  Use that.  Our is_null result
// is always zero because IS NULL is never, itself, null.
static void cg_expr_is_null(ast_node *expr, charbuf *is_null, charbuf *value) {
  sem_t sem_type_expr = expr->sem->sem_type;

  // expr IS NULL
  bprintf(is_null, "0"); // the result of is null is never null

  // The fact that this is not constant not null for not null reference types reflects
  // the weird state of affairs with uninitialized reference variables which
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

So walking through the above:
* the result of `IS NULL` is never null, so we can immediately put "0" into the `is_null` buffer
* if the operand is a not-null numeric type then the result of `IS NULL` is `0`
* if the operand might actually be null then
  * use `CG_PUSH_EVAL` to recursively do codegen for it
  * copy its `expr_is_null` text into our `value` text

Note: the code reveals one of the big CQL secrets -- that not null reference variables can be null...  C has the same issue with `_Nonnull` globals.

Now let's look at those helper macros, they are pretty simple:

```c
// Make a temporary buffer for the evaluation results using the canonical
// naming convention.  This might exit having burned some stack slots
// for its result variables, that's normal.
#define CG_PUSH_EVAL(expr, pri) \
CHARBUF_OPEN(expr##_is_null); \
CHARBUF_OPEN(expr##_value); \
cg_expr(expr, &expr##_is_null, &expr##_value, pri);
```

The push macro simply creates buffers to hold the `is_null` and `value` results, then it calls `cg_expr` to dispatch the indicated expression.
The `pri` value provided to this macro represents the binding strength that the callee should assume its parent has.  Usually this is the `pri_new` of the caller.
but often `C_EXPR_PRI_ROOT` can be used if the current context implies that the callee will never need parentheses.

How do we know that parens are not needed here? It seems like the operand of `IS NULL` could be anything, surely it might need parentheses?  Let's consider:

* if the operand is of not null numeric type then we aren't even going to evaluate it, we're on the easy "no it's not null" path
  * no parens there
* if the operand is nullable then the only place the answer can be stored is in a scratch variable and its `is_null` expression will be exactly like `var.is_null`
  * no parens there
* if the operand is a reference type, there are no operators that combine reference types to get more reference types, so again the result must be in a variable, and is `is_null` expression will be like `!var`
  * no parens there

So, none of these require further wrapping regardless of what is above the `IS NULL` node in the tree because of the high strength of the `.` and `!` operators.

Other cases are usually simpler, such as "no parentheses need to be added by the child node because it will be used as the argument to a helper
function so there will always be parens hard-coded anyway".  However these things need to be carefully tested hence the huge variety of codegen tests.

Note that after calling `cg_expr` the temporary stack level might be increased.  We'll get to that in the next section.  For now, looking at `POP_EVAL` we
can see it's very straightforward:

```c
// Close the buffers used for the above.
// The scratch stack is not restored so that any temporaries used in
// the evaluation of expr will not be re-used prematurely.  They
// can't be used again until either the expression is finished,
// or they have been captured in a less-nested result variable.
#define CG_POP_EVAL(expr) \
CHARBUF_CLOSE(expr##_value); \
CHARBUF_CLOSE(expr##_is_null);
```

`CG_POP_EVAL` simply closes the buffers, leaving the stack level unchanged.  More on this in the coming section.

#### Result Variables

When recursion happens in the codegen, a common place that the result will be found is
in a temporary variable i.e. the generated code will use one or more statements to arrange for the correct
answer to be in a variable.  To do this, the codegen needs to first get the name of a
result variable of a suitable type.  This is the "other" reason for making scratch variables.

There are three macros that make this pretty simple.  The first is `CG_RESERVE_RESULT_VAR`

```c
// Make a scratch variable to hold the final result of an evaluation.
// It may or may not be used.  It should be the first thing you put
// so that it is on the top of your stack.  This only saves the slot.
// If you use this variable you can reclaim other temporaries that come
// from deeper in the tree since they will no longer be needed.
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
and the expression parts always go into `charbuf` variables named `result_var`, `result_var_is_null`, and `result_var_value`
but the scratch variable isn't actually allocated!  However -- we burn the stack_level as though it had been
allocated.

The name of the macro provides a clue: this macro reserves a slot for the result variable, it's used if the codegen might
need a result variable, but it might not.  If/when the result variable is needed, it we can artificially move the stack level
back to the reserved spot, allocate the scratch variable, and then put the stack level back.  When the name is set we know
that the scratch variable was actually used.

The `CG_USE_RESULT_VAR` macro does exactly this operation.

```c
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
to populate the variable.  The variable is the result.

There is a simpler macro that reserves and uses the result variable in one step, it's used frequently.
The "reserve" pattern is only necessary when there are some paths that need a result variable and some
that don't.

```c
// This does reserve and use in one step
#define CG_SETUP_RESULT_VAR(ast, sem_type) \
  CG_RESERVE_RESULT_VAR(ast, sem_type); \
  CG_USE_RESULT_VAR();
```

And now armed with this knowledge we can look at the rest of the scratch stack management.

```c
// Release the buffer holding the name of the variable.
// If the result variable was used, we can re-use any temporaries
// with a bigger number.  They're no longer needed since they
// are captured in this result.  We know it was used if it
// has .used > 1 (there is always a trailing null so empty is 1).
#define CG_CLEANUP_RESULT_VAR() \
if (result_var.used > 1) stack_level = stack_level_reserved + 1; \
CHARBUF_CLOSE(result_var_value); \
CHARBUF_CLOSE(result_var_is_null); \
CHARBUF_CLOSE(result_var);
```

As it happens when you use `CG_PUSH_EVAL` it is entirely possible, even likely,
that the result of `cg_expr` is in a result variable.  The convention is that if the codegen
requires a result variable it is allocated *first*, before any other temporaries.  This is why
there is a way to reserve a variable that you *might* need.

Now if it turns out that you used the result variable at your level it means that any
temporary result variables from deeper levels have been used and their values plus
whatever math was needed is now in your result variable.  This means that the `stack_level`
variable can be decreased to one more than the level of the present result.  This is
in the fact the only time it is safe to start re-using result variables because
you otherwise never know how many references to result variables that were "deep in the tree"
are left in the contents of `expr_value` or `expr_is_null`.

Now, armed with the knowledge that there are result variables and temporary variables and both
come from the scratch variables we can resolve the last mystery we left hanging.  Why does
the scratch variable API accept an AST pointer?

The only place that AST pointer can be not null is in the `CG_USE_RESULT_VAR` macro, it was
this line:

```c
cg_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_is_null, &result_var_value);
```

And `ast_reserved` refers to the AST that we are trying to evaluate.  There's an important
special case that we want to optimize that saves a lot of scratch variables.  That case is handled
by this code in `cg_scratch_var`:

```c
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

```c
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

The comment before the `cg_while_stmt` actually describes the situation pretty clearly;
the issue with this codegen is that the expression in the while statement might actually
require many C statements to evaluate.  There are many cases of this sort of thing, but the
simplest is probably when any nullable types are in that expression.  A particular example
illustrates this pretty clearly:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE x INTEGER NOT NULL;
  SET x := 1;
  WHILE x < 5
  BEGIN
    SET x := x + 1;
  END;
END;
```

which generates:

```c
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

In this case, a `while` statement could have been used because the condition is simply `x < 5`
so this more general pattern is overkill.  But consider this program, just a tiny bit different:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE x INTEGER;  -- x is nullable
  SET x := 1;
  WHILE x < 5
  BEGIN
    SET x := x + 1;
  END;
END;
```

which produces:

```c
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
The result of `x < 5` is of type `BOOL` rather than `BOOL NOT NULL` so a temporary variable captures
the result of the expression.  This is an easy case, but similar things happen if the expression
includes e.g. `CASE...WHEN...` or `IN` constructs.  There are many other cases.

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
fact that CQL expressions often become C statements so we use a more general flow control strategy.
But with this in mind, it's easy to imagine how the `IF`, `LOOP`, and `SWITCH` switch statements are
handled.

### Cleanup and Errors

There are a number of places where things can go wrong when running a CQL procedure.  The most
common sources are: (1) SQLite APIs, almost all of which can fail, and, (2) calling other procedures
which also might fail.  Here's a very simple example:

```SQL
DECLARE PROC something_that_might_fail (arg TEXT) USING TRANSACTION;

CREATE PROC p ()
BEGIN
  LET arg := "test";
  CALL something_that_might_fail(arg);
END;
```

Which generates:

```c
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

Let's look at those fragments carefully:

* first, we had to declare `something_that_might_fail`
  * the declaration included `USING TRANSACTION` indicating the procedure uses the database
  * we didn't provide the procedure definition, this is like an `extern ... foo(...);` declaration
* there is a string literal named `_literal_1_test_p` that is auto-created
  * `cql_string_literal` can expand into a variety of things, whatever you want "make a string literal" to mean
  * it's defined in `cqlrt.h` and it's designed to be replaced
* `cql_set_string_ref(&arg, _literal_1_test_p);` is expected to "retain" the string (+1 ref count)
* `cql_cleanup` is the exit label, this cleanup code will run on all exit paths
  * cleanup statements are accumulated by writing to `cg_cleanup_output` which usually writes to the `proc_cleanup` buffer
  * because cleanup is in its own buffer you can add to it freely whenever a new declaration that requires cleanup arises
  * in this case the declaration of the string variable caused the `C` variable `arg` to be created and also the cleanup code
*  now we call `something_that_might_fail` passing it our database pointer and the argument
  * the hidden `_db_` pointer is passed to all procedures that use the database
  * these procedures are also the ones that can fail
* any failed return code (not `SQLITE_OK`) causes two things:
  * the `cql_error_trace()` macro is invoked (this macro typically expands to nothing)
  * the code is redirected to the cleanup block via `goto cql_cleanup;`

The essential sequence is this one:

```c
 if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
```

The C code generator consistently uses this pattern to check if anything went wrong and to exit with an error code.
Extensive logging can be very expensive, but in debug builds it's quite normal for `cql_error_trace` to expand
into something like `fprintf(stderr, "error %d in %s %s:%d\n", _rc_, _PROC_, __FILE__, __LINE_)` which is probably
a lot more logging than you want in a production build but great if you're debugging.  Recall that CQL generates
something like `#define _PROC_ "p"` before every procedure.

This error pattern generalizes well and indeed if we use the exception handling pattern, we get a lot of control.
Let's generalize this example a tiny bit:

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

```c
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

The code in this case is nearly the same as the previous example.  Let's look at the essential differences:

* If there is an error, `goto catch_start_1` will run
* If the try block succeeds, `goto catch_end_1` will run
* both the `TRY` and `CATCH` branches set the `success` out parameter
* since an out argument was added, CQL generated an error check to ensure that `success` is not null
  * `cql_contract_argument_notnull((void *)success, 1)`, the 1 means "argument 1" and will appear in the error message if this test fails
  * the hidden `_db_` argument doesn't count for error message purposes, so `success` is still the first argument

How does this happen?  Let's look at `cg_trycatch_helper` which does this work:

```c
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

The secret is the `error_target` global variable.
All of the error handling will emit a goto `error_target` statement. The
try/catch pattern simply changes the current error target.  The rest of
the code in the helper is just to save the current error target and to
create unique labels for the try/catch block.

The important notion is that, if anything goes wrong, whatever it is,
the generator simply does a `goto error_target` and that will either
hit the catch block or else go to cleanup.

The `THROW` operation illustrates this well:

```c
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

* first we make sure `_rc_` has some kind of error in it, either `rcthrown_current` or else `SQLITE_ERROR`
* then we go to the current error target
* `error_target_used` tracks whether if the error label was used, this is just to avoid C compiler errors about unused labels.
  * if the label is not used it won't be emitted
  * the code never jumps back to an error label, so we'll always know if the label was used before we need to emit it

Note: every catch block captures the value of `_rc_` in a local variable whose name is in `rcthrown_current`.
This captured value is the current failing result code accessible by `@RC` in CQL.

A catch block can therefore do stuff like:

```sql
IF @RC = 1 THEN
  THROW;
ELSE
  call attempt_retry();
END IF;
```

This entire mechanism is built with basically just a few state variables that nest.  There is no complicated stack walking
or anything like that.  All the code has to do is chain the error labels together and let users create new catch blocks
with new error labels.  All that together gives you very flexible try/catch behaviour with very little overhead.

### String Literals

Before we move on to more complex statements we have to discuss string literals a little bit.  We've mentioned before
that the compiler is going to generate something like this:

```c
cql_string_literal(_literal_1_test_p, "test");
```

To create a reference counted object `_literal_1_test_p` that it can use.  Now we're going to talk about how
the text `"test"` was created and how that gets more complicated.

The first thing to remember is that the generator creates C programs.  That means
no matter what kind of literal we might be processing it's ending up encoded as a C string for the C
compiler.  The C compiler will be the first thing the decodes the text the generator produces and
puts the byte we need into the final programs data segment or wherever.  That means if we have
SQL format strings that need to go to SQLite they will be twice-encoded, the SQL string is escaped
as needed for SQLite and *that* is escaped again for the C compiler.

An example might make this clearer consider the following SQL:

```SQL
  SELECT '"x''y"' AS a, "'y'\n" AS b;
```

The generated text for this statement will be:

```c
  "SELECT '\"x''y\"', '''y''\n'"
```

Let's review that in some detail:

* the first string "a" is a standard SQL string
  * it is represented unchanged in the AST, it is *not* unescaped
  * even the outer single quotes are preserved, CQL has no need to change it at all
  * when we emit it into our output it will be read by the C compiler, so
  * at that time it is escaped *again* into C format
    * the double quotes which required no escaping in SQL become `\"`
  * the single quote character requires no escape but there are still two of them because SQLite will also process this string

* the second string "b" is a C formatted string literal
  * SQLite doesn't support this format or its escapes, therefore
  * as discussed in [Part 1](https://cgsql.dev/cql-guide/int01), it is decoded to plain text, then re-encoded as a SQL escaped string
  * internal newlines do not require escaping in SQL, they are in the string as the newline character not '\n' or anything like that
    * to be completely precise the byte value 0x0a is in the string unescaped
  * internal single quotes don't require escaping in C, these have to be doubled in a SQL string
  * the outer double quotes are removed and replaced by single quotes during this process
  * the AST now has a valid SQL formatted string possibly with weird characters in it
  * as before, this string has to be formatted for the C compiler so now it has to be escaped again
  * the single quotes require no further processing, though now there are quite a few of them
  * the embedded newline is converted to the escape sequence "\n" so we're back to sort of where we started
    * the C compiler will convert this back to the byte 0x0a which is what ends up in the data segment

In the above example we were making one overall string for the `SELECT` statement so the outer double quotes
are around the whole statement.  That was just for the convenience of this example.  If the literals had
been in some other loose context then individual strings would be produced the same way.  Except, not so fast,
not every string literal is heading for SQLite.  Some are just making regular strings.  In that case even
if they are destined for SQLite they will go as bound arguments to a statement not in the text of the SQL.
That means *those* strings do not need SQL escaping.

Consider:

```SQL
  LET a := '"x''y"';
  LET b := "'y'\n";
```

To do those assignments we need:

```c
cql_string_literal(_literal_1_x_y_p, "\"x'y\"");
cql_string_literal(_literal_2_y_p, "'y'\n");
```

In both of these cases the steps are:

* unescape the escaped SQL string in the AST to plain text
  * removing the outer single quotes of course
* re-escape the plain text (which might include newlines and such) as a C string
  * emit that text, including its outer double quotes

Trivia: the name of the string literal variables include a fragment of the string to make them a little easier to spot.

`encoders.h` has the encoding functions
* `cg_decode_string_literal`
* `cg_encode_string_literal`
* `cg_encode_c_string_literal`
* `cg_decode_c_string_literal`

As well as similar functions for single characters to make all this possible.  Pretty much every combination
of encoding and re-encoding happens in some path through the code generator.

### Executing SQLite Statements

By way of example let's consider a pretty simple piece of SQL we might want to run.

```SQL
CREATE TABLE foo(id INTEGER, t TEXT);

CREATE PROC p (id_ INTEGER, t_ TEXT)
BEGIN
  UPDATE foo
  SET t = t_
    WHERE id = id_;
END;
```

To make this happen we're going to have to do the following things:
 * create a string literal with the statement we need
 * the references to `id_` and `t_` have to be replaced with `?`
 * we prepare that statement
 * we bind the values of `id_` and `t_`
 * we `step` the statement
 * we `finalize` the statement
 * suitable error checks have to be done at each stage

That's quite a bit of code and it's easy to forget a step, this is an area where CQL shines.  The
code we had to write in CQL was very clear and all the error checking is implicit.

This is the generated code.  We'll walk through it and discuss how it is created.

```c
CQL_WARN_UNUSED cql_code p(
  sqlite3 *_Nonnull _db_,
  cql_nullable_int32 id_,
  cql_string_ref _Nullable t_)
{
  cql_code _rc_ = SQLITE_OK;
  sqlite3_stmt *_temp_stmt = NULL;

  _rc_ = cql_prepare(_db_, &_temp_stmt,
    "UPDATE foo "
    "SET t = ? "
      "WHERE id = ?");
  cql_multibind(&_rc_, _db_, &_temp_stmt, 2,
                CQL_DATA_TYPE_INT32, &id_,
                CQL_DATA_TYPE_STRING, t_);
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = sqlite3_step(_temp_stmt);
  if (_rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
  cql_finalize_stmt(&_temp_stmt);
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_finalize_stmt(&_temp_stmt);
  return _rc_;
}
```
* the functions signature includes the hidden `_db_` parameter plus the two arguments
* we need a hidden `_rc_` variable to hold the result codes from SQLite
* we need a scratch `sqlite3_stmt *` named `_temp_stmt` to talk to SQLite
  * when this is created, the cleanup section gets `cql_finalize_stmt(&_temp_stmt);`
  * `cql_finalize_stmt` sets the statement to null and does nothing if it's already null
* the string `"INSERT INTO foo(id, t) VALUES(?, ?)"` is created from the AST
  * recall that we have `variables_callback` as an option, it's used here to track the variables and replace them with `?`
  * more on this shortly
* `cql_multibind` is used to bind the values of `id_` and `t_`
  * this is just a varargs version of the normal SQLite binding functions, it's only done this way to save space
  * only one error check is needed for any binding failure
  * the type of binding is encoded very economically
  * the "2" here refers to two arguments
* the usual error processing happens with `cql_error_trace` and `goto cql_cleanup`
* the statement is executed with `sqlite3_step`
* temporary statements are finalized immediately with `cql_finalize_stmt`
  * in this case its redundant because the code is going to fall through to cleanup anyway
  * in general there could be many statements and we want to finalize immediately
  * this is an optimization opportunity, procedures with just one statement are very common

Most of these steps are actually hard coded.  There is no variability in the sequence
after the `multibind` call, so that's just boiler-plate the compiler can inject.

We don't want to declare `_temp_stmt` over and over so there's a flag that records
whether it has already been declared in the current procedure.

```c
// Emit a declaration for the temporary statement _temp_stmt_ if we haven't
// already done so.  Also emit the cleanup once.
static void ensure_temp_statement() {
  if (!temp_statement_emitted) {
    bprintf(cg_declarations_output, "sqlite3_stmt *_temp_stmt = NULL;\n");
    bprintf(cg_cleanup_output, "  cql_finalize_stmt(&_temp_stmt);\n");
    temp_statement_emitted = 1;
  }
}
```

This is a great example of how, no matter where the processing happens to be,
the generator can emit things into the various sections.  Here it adds
a declaration and an cleanup with no concern about what else might be going on.

So most of the above is just boiler-plate, the tricky part is:
 * getting the text of the SQL
 * binding the variables

All of this is the business of this function:

```c
// This is the most important function for sqlite access;  it does the heavy
// lifting of generating the C code to prepare and bind a SQL statement.
// If cg_exec is true (CG_EXEC) then the statement is executed immediately
// and finalized.  No results are expected.  To accomplish this we do the following:
//   * figure out the name of the statement, either it's given to us
//     or we're using the temp statement
//   * call get_statement_with_callback to get the text of the SQL from the AST
//     * the callback will give us all the variables to bind
//     * count the variables so we know what column numbers to use (the list is backwards!)
//   * if CG_EXEC and no variables we can use the simpler sqlite3_exec form
//   * bind any variables
//   * if there are variables CG_EXEC will step and finalize
static void cg_bound_sql_statement(CSTR stmt_name, ast_node *stmt, int32_t cg_flags)
{
  ...
}
```

The core of this function looks like this:

```c
  gen_sql_callbacks callbacks;
  init_gen_sql_callbacks(&callbacks);
  callbacks.variables_callback = cg_capture_variables;
  callbacks.variables_context = &vars;
  // ... more flags

  CHARBUF_OPEN(temp);
  gen_set_output_buffer(&temp);
  gen_statement_with_callbacks(stmt, &callbacks);
```

It's set up the callbacks for variables and it calls the echoing function on the buffer.  We've
talked about `gen_statement_with_callbacks` in  [Part 1](https://cgsql.dev/cql-guide/int01).

Let's take a look at that callback function:

```
// This is the callback method handed to the gen_ method that creates SQL for us
// it will call us every time it finds a variable that needs to be bound.  That
// variable is replaced by ? in the SQL output.  We end up with a list of variables
// to bind on a silver platter (but in reverse order).
static bool_t cg_capture_variables(ast_node *ast, void *context, charbuf *buffer) {
  list_item **head = (list_item**)context;
  add_item_to_list(head, ast);

  bprintf(buffer, "?");
  return true;
}
```

The `context` variable was set to be `vars`, we convert it back to the correct type
and add the current ast to that list.  `add_item_to_list` always puts things at the
head so the list will be in reverse order.

With this done, we're pretty much set.  We'll produce the statement with a sequence
like this one (there are a couple of variations, but this is the most general)

```c
  bprintf(cg_main_output, "_rc_ = cql_prepare(_db_, %s%s_stmt,\n  ", amp, stmt_name);
  cg_pretty_quote_plaintext(temp.ptr, cg_main_output, PRETTY_QUOTE_C | PRETTY_QUOTE_MULTI_LINE);
  bprintf(cg_main_output, ");\n");
```

`cg_pretty_quote_plaintext` is one of the C string encoding formats, it could have been just the regular C string encoding
but that would have been a bit wasteful and it wouldn't have looked as nice.  This function does a little transform.

The normal echo of the update statement in question looks like this:

```
  UPDATE foo
  SET t = ?
    WHERE id = ?;
```

Note that it has indenting and newlines embedded in it.  The standard encoding of that would look like this:

```c
"  UPDATE foo\n  SET t = ?\n    WHERE id = ?;"
```

That surely works, but it's wasteful and ugly. The pretty format instead produces:

```c
    "UPDATE foo "
    "SET t = ? "
      "WHERE id = ?"
```

So, the newlines are gone from the string (they aren't needed), instead the string literal was broken into lines for readability.
The indenting is gone from the string, instead the string fragments are indented.  So what you get is a string literal that
reads nicely but doesn't have unnecessary whitespace for SQLite.  Obviously you can't use pretty-quoted literals in all cases,
it's exclusively for SQLite formatting.

All that's left to do is bind the arguments.  Remember that arg list is in reverse order:

```c
  uint32_t count = 0;
  for (list_item *item = vars; item; item = item->next, count++) ;

  // ...

  reverse_list(&vars);

  if (count) {
    bprintf(cg_main_output, "cql_multibind(&_rc_, _db_, %s%s_stmt, %d", amp, stmt_name, count);

    // Now emit the binding args for each variable
    for (list_item *item = vars; item; item = item->next)  {
      Contract(item->ast->sem->name);
      bprintf(cg_main_output, ",\n              ");
      cg_bind_column(item->ast->sem->sem_type, item->ast->sem->name);
    }

    bprintf(cg_main_output, ");\n");
  }
```

* first compute the count, we don't need to bind if there are no variables
* `reverse_list` does exactly what is sounds like (finally a real-world use-case for reverse-list-in-place)
* `cg_bind_column` creates one line of the var-args output: column type and variable name
  * the type and name information is right there on the `AST` in the `sem_node`

And that's it.  With those few helpers we can bind any SQLite statement the same way.  All of the
`DDL_STMT_INIT` and `DML_STMT_INIT` statements are completely implemented by this path.

### Reading Single Values

In many cases you need just one value

```SQL
CREATE PROC p (id_ INTEGER NOT NULL, OUT t_ TEXT)
BEGIN
  SET t_ := ( SELECT t
    FROM foo
    WHERE id = id_ );
END;
```

This is going to be very similar to the examples we've seen so far:

```c
CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_, cql_int32 id_, cql_string_ref _Nullable *_Nonnull t_) {
  cql_contract_argument_notnull((void *)t_, 2);

  cql_code _rc_ = SQLITE_OK;
  cql_string_ref _tmp_text_0 = NULL;
  sqlite3_stmt *_temp_stmt = NULL;

  *(void **)t_ = NULL; // set out arg to non-garbage
  _rc_ = cql_prepare(_db_, &_temp_stmt,
    "SELECT t "
      "FROM foo "
      "WHERE id = ?");
  cql_multibind(&_rc_, _db_, &_temp_stmt, 1,
                CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, id_);
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = sqlite3_step(_temp_stmt);
  if (_rc_ != SQLITE_ROW) { cql_error_trace(); goto cql_cleanup; }
    cql_column_string_ref(_temp_stmt, 0, &_tmp_text_0);
  cql_finalize_stmt(&_temp_stmt);
  cql_set_string_ref(&*t_, _tmp_text_0);
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_string_release(_tmp_text_0);
  cql_finalize_stmt(&_temp_stmt);
  return _rc_;
}
```

* `_db_` : incoming arg for a procedure that uses the database same as always, check
* `*(void **)t_ = NULL;` : out args are always set to NULL on entry, note, there is no `release` here
   * argument is assumed to be garbage, that's the ABI
   * if argument is non-garbage caller must release it first, that's the ABI
* `_rc_` : same as always, check
* `_tmp_text_0` : new temporary text, including cleanup (this could have been avoided)
* `_temp_stmt` : as before, including cleanup
* `cql_prepare` : same as always, check
* `cql_multibind` : just one integer bound this time
* `sqlite3_step` : as before, we're stepping once, this time we want the data
* `if (_rc_ != SQLITE_ROW)` new error check and goto cleanup if no row
  * this is the same as the `IF NOTHING THROW` variant of construct, that's the default
* `cql_column_string_ref` : reads one string from `_temp_stmt`
* `cql_finalize_stmt` : as before
* `cql_set_string_ref(&*t_, _tmp_text_0)` : copy the temporary string to the out arg
  * includes retain, out arg is NULL so `cql_set_string_ref` will do no release
  * if this were (e.g.) running in a loop, the out arg would not be null and there would be a release, as expected
  * if something else had previously set the out arg, again, there would be a release as expected

There are variations of this form such as:

```SQL
CREATE PROC p (id_ INTEGER NOT NULL, OUT t_ TEXT)
BEGIN
  SET t_ := ( SELECT t
    FROM foo
    WHERE id = id_
    IF NOTHING '');
END;
```

This simply changes the handling of the case where there is no row.  The that part of the code
ends up looking like this:

```c
  if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
  if (_rc_ == SQLITE_ROW) {
    cql_column_string_ref(_temp_stmt, 0, &_tmp_text_1);
    cql_set_string_ref(&_tmp_text_0, _tmp_text_1);
  }
  else {
    cql_set_string_ref(&_tmp_text_0, _literal_1_p);
  }
```

* any error code leads to cleanup
* `SQLITE_ROW` : leads to the same fetch as before
* `SQLITE_DONE` : leads to the no row case which sets `_tmp_text_0` to the empty string
  * `cql_string_literal(_literal_1_p, "");` is included as a data declaration

There is also the `IF NOTHING OR NULL` variant which is left as an exercise to the reader.
You can find all the flavors in `cg_c.c` in the this function:

```c
// This is a nested select expression.  To evaluate we will
//  * prepare a temporary to hold the result
//  * generate the bound SQL statement
//  * extract the exactly one argument into the result variable
//    which is of exactly the right type
//  * use that variable as the result.
// The helper methods take care of sqlite error management.
static void cg_expr_select(...
```

This handles all of the `(select ...)` expressions and it has the usual expression handler
syntax. Another great example of a CQL expressions that might require many C statements
to implement.

### Reading Rows With Cursors

This section is about the cases where we are expecting results back from SQLite.  By results here
I mean the results of some kind of query, not like a return code.  SQLite does this by giving
you a `sqlite3_stmt *` which you can then use like a cursor to read out a bunch of rows.  So
it should be no surprise that CQL cursors map directly to SQLite statements.

Most of the code to get a statement we've already seen before, we only saw the `_temp_stmt`
case and we did very little with it.  Let's look at the code for something a little bit more
general and we'll see how little it takes to generalize.

First, let's look at how a CQL cursor is initialized:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
END;
```

Now in this case there can only be one row in the result, but it would be no different if there were more.

Here's the C code:

```c
CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_) {
  cql_code _rc_ = SQLITE_OK;
  sqlite3_stmt *C_stmt = NULL;
  cql_bool _C_has_row_ = 0;

  _rc_ = cql_prepare(_db_, &C_stmt,
    "SELECT 1, 2");
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_finalize_stmt(&C_stmt);
  return _rc_;
}
```

Let's look over that code very carefully and see what is necessary to make it happen.

* `_db_` : incoming arg for a procedure that uses the database same as always, check
* `_rc_` : same as always, check
* `C_stmt` : we need to generate this instead of using `_temp_stmt`
  * `cql_finalize_stmt(&C_stmt)` in cleanup, just like `_temp_stmt`
* `cql_prepare` : same as always, check
* `cql_multibind` : could have been binding, not none needed here, but same as always anyway, check
* no step, no finalize (until cleanup) : that boiler-plate is removed

And that's it, we now have a statement in `C_stmt` ready to go.  We'll see later that `_C_has_row_`
will track whether or not the cursor has any data in it.

How do we make this happen?  Well you could look at `cg_declare_cursor` and your eye might hurt at
first.  The truth is there are many kinds of cursors in CQL and this method handles all of them.
We're going to go over the various flavors but for now we're only discussing the so-called
"statement cursor", so named because it simply holds a SQLite statement.  This was the
first, and for a while only, type of cursor added to the CQL language.

OK so how do we make a statement cursor.  It's once again `cg_bound_sql_statement` just like so:

```c
cg_bound_sql_statement(cursor_name, select_stmt, CG_PREPARE|CG_MINIFY_ALIASES);
```

The entire difference is that the first argument is the cursor name rather than NULL.  If you
pass NULL it means use the temporary statement.

And you'll notice that even in this simple example the SQLite text was altered a bit:
the text that went to SQLite was `"SELECT 1, 2"` -- that's CG_MINIFY_ALIASES at work.
SQLite didn't need to see those column aliases, it makes no difference in the result.
Column aliases are often long and numerous.  Even in this simple example we saved 4 bytes.
But the entire query was only 12 bytes long (including trailing null) so that's 25%.
It's not a huge savings in general but it's something.

The other flag `CG_PREPARE` tells the binder that it should not step or finalize the query.
The alternative is `CG_EXEC` (which was used in the previous section for the `UPDATE` example).

### Fetching Data From Cursors

The first cursor reading primitive that was implemented as `FETCH [cursor] INTO [variables]` and
it's the simplest to understand so let's start there.

We change the example just a bit:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE x INTEGER NOT NULL;
  DECLARE y INTEGER NOT NULL;
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
  FETCH C INTO x, y;
END;
```

For simplicity I will only include the code that is added.  The rest is the same.

```c
  cql_int32 x = 0;
  cql_int32 y = 0;

  // same as before

  _rc_ = sqlite3_step(C_stmt);
  _C_has_row_ = _rc_ == SQLITE_ROW;
  cql_multifetch(_rc_, C_stmt, 2,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &x,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &y);
  if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
```

Do to the `FETCH` we do the following:

* step the cursor
* set the `_C_has_row_` variable so to indicate if we got a row or not
* use the varargs `cql_multifetch` to read 2 columns from the cursor
  * this helper simply uses the usual `sqlite3_*_column` functions to read the data out
  * again, we do it this way so that there is less error checking needed in the generated code
  * also, there are fewer function calls so the code is overall smaller
  * trivia: `multibind` and `multifetch` are totally references to _The Fifth Element_
    * hence, they should be pronounced like Leeloo saying "multipass"
* `multifetch` uses the varargs to clobber the contents of the target variables if there is no row according to `_rc_`
* `multifetch` uses the `CQL_DATA_TYPE_NOT_NULL` to decide if it should ask SQLite first if the column is null

So now this begs the question, in the CQL, how do you know if a row was fetched or not?

The answer is, you can use the cursor name like a boolean.  Let's complicate this up a little more.

```SQL
DECLARE PROCEDURE printf NO CHECK;

CREATE PROC p ()
BEGIN
  DECLARE x INTEGER NOT NULL;
  DECLARE y INTEGER NOT NULL;
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
  FETCH C INTO x, y;
  WHILE C
  BEGIN
    CALL printf("%d, %d\n", x, y);
    FETCH C INTO x, y;
  END;
END;
```

Again here is what is now added, we've seen the `WHILE` pattern before:

```c
  for (;;) {
  if (!(_C_has_row_)) break;
    printf("%d, %d\n", x, y);
    _rc_ = sqlite3_step(C_stmt);
    _C_has_row_ = _rc_ == SQLITE_ROW;
    cql_multifetch(_rc_, C_stmt, 2,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &x,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &y);
    if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
  }
```

So, if you use the cursors name in an ordinary expression that is converted to a reference to
the boolean `_C_has_row_`.  Within the loop we're going to print some data and then fetch the next row.
The internal fetch is of course the same as the first.

The next improvement that was added to the language was the `LOOP` statement.  Let's take a look:

```SQL
BEGIN
  DECLARE x INTEGER NOT NULL;
  DECLARE y INTEGER NOT NULL;
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
  LOOP FETCH C INTO x, y
  BEGIN
    CALL printf("%d, %d\n", x, y);
  END;
END;
```

The generated code is very similar:

```c
  for (;;) {
    _rc_ = sqlite3_step(C_stmt);
    _C_has_row_ = _rc_ == SQLITE_ROW;
    cql_multifetch(_rc_, C_stmt, 2,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &x,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &y);
    if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
    if (!_C_has_row_) break;
    printf("%d, %d\n", x, y);
  }
```

This is done by:

* emit the `for (;;) {` to start the loop
* generate the `FETCH` just as if it was standalone
* emit `if (!_C_has_row_) break;` (with the correct cursor name)
* use `cg_stmt_list` to emit the internal statement list (`CALL printf` in this case)
* close the loop with `}` and we're done

### Cursors With Storage

We now come to the big motivating reasons for having the notion of shapes in the CQL language.
This particular case was the first such example in the language and it's very commonly
used and saves you a lot of typing.  Like the other examples it's only sugar in that
it doesn't give you any new language powers you didn't have, but it does give clarity
and maintenance advantages.  And it's just a lot less to type.

Let's go back to one of the earlier examples, but write it the modern way:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
  FETCH C;
END;
```

And the generated C code:

```c
typedef struct p_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 x;
  cql_int32 y;
} p_C_row;

CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_) {
  cql_code _rc_ = SQLITE_OK;
  sqlite3_stmt *C_stmt = NULL;
  p_C_row C = { 0 };

  _rc_ = cql_prepare(_db_, &C_stmt,
    "SELECT 1, 2");
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = sqlite3_step(C_stmt);
  C._has_row_ = _rc_ == SQLITE_ROW;
  cql_multifetch(_rc_, C_stmt, 2,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.x,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
  if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_finalize_stmt(&C_stmt);
  return _rc_;
}
```

Let's look at what's different here:

* `struct p_C_row` has been created, it contains:
  * `_has_row_` for the cursor
  * `x` and `y` the data fields
  * `_refs_count` the number of reference fields in the cursor (0 in this case)
  * `_refs_offset` the offset of the references fields (they always go at the end)
  * because the references are together a cursor with lots of reference fields can be cleaned up easily
* in the generated code the variable `C` refers to the current data that has been fetched
  * convenient for debugging `p C` in lldb shows you the row
* references to `x` and `y` became `C.x` and `C.y`
* references to `_C_has_row_` became `C._has_row_`

That's pretty much it.  The beauty of this is that you can't get the declarations of your locals wrong
and you don't have to list them all no matter how big the data is.  If the data shape changes the
cursor change automatically changes to accommodate it.  Everything is still statically typed.

Now lets look at the loop pattern:


```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR SELECT 1 AS x, 2 AS y;
  LOOP FETCH C
  BEGIN
    CALL printf("%d, %d\n", C.x, C.y);
  END;
END;
```

Note that the columns of the cursor were defined by the column aliases of the `SELECT`.

```c
  for (;;) {
    _rc_ = sqlite3_step(C_stmt);
    C._has_row_ = _rc_ == SQLITE_ROW;
    cql_multifetch(_rc_, C_stmt, 2,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.x,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
    if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
    if (!C._has_row_) break;
    printf("%d, %d\n", C.x, C.y);
  }
```

The loop is basically the same except `x` and `y` have been replaced with `C.x` and `C.y`
and again `_C_has_row_` is now `C._has_row_`.

The code generator knows that it should allocate storage for the `C` cursor if it has
the flag `SEM_TYPE_HAS_SHAPE_STORAGE` on it. The semantic analyzer adds that flag
if it ever finds `FETCH C` with no `INTO` part.

Finally let's look at an example with cleanup required.  We'll just change the test case a tiny bit.

```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR SELECT 1 AS x, "2" AS y;
  LOOP FETCH C
  BEGIN
    CALL printf("%d, %s\n", C.x, C.y);
  END;
END;
```

The `x` column is now text.  We'll get this code which will be studied below:

```
typedef struct p_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} p_C_row;

#define p_C_refs_offset cql_offsetof(p_C_row, x) // count = 1

CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_) {
  cql_code _rc_ = SQLITE_OK;
  sqlite3_stmt *C_stmt = NULL;
  p_C_row C = { ._refs_count_ = 1, ._refs_offset_ = p_C_refs_offset };

  _rc_ = cql_prepare(_db_, &C_stmt,
    "SELECT '1', 2");
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  for (;;) {
    _rc_ = sqlite3_step(C_stmt);
    C._has_row_ = _rc_ == SQLITE_ROW;
    cql_multifetch(_rc_, C_stmt, 2,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &C.x,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
    if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
    if (!C._has_row_) break;
    cql_alloc_cstr(_cstr_1, C.x);
    printf("%s, %d\n", _cstr_1, C.y);
    cql_free_cstr(_cstr_1, C.x);
  }
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_finalize_stmt(&C_stmt);
  cql_teardown_row(C);
  return _rc_;
}
```

It's very similar to what we had before, let's quickly review the differences.

```c
typedef struct p_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} p_C_row;

#define p_C_refs_offset cql_offsetof(p_C_row, x) // count = 1
```

* `x` is now `cql_string_ref _Nonnull x;` rather than `cql_int32`
* `x` has moved to the end (because it's a reference type)
* the offset of the first ref is computed in a constant

Recall the reference types are always at the end and together.

```c
  p_C_row C = { ._refs_count_ = 1, ._refs_offset_ = p_C_refs_offset };
```

* `p_C_row` is now initialized to to ref count 1 and refs offset `p_C_refs_offset` defined above

```c
  cql_multifetch(_rc_, C_stmt, 2,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &C.x,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
```

* C.x is now of type string

```c
    cql_alloc_cstr(_cstr_1, C.x);
    printf("%s, %d\n", _cstr_1, C.y);
    cql_free_cstr(_cstr_1, C.x);
```

* C.x has to be converted to a C style string before it can be used with `printf` as a `%s` argument

```c
  cql_teardown_row(C);
```

* the cleanup section has to include code to teardown the cursor, this will release all of its reference variables in bulk
  * remember we know the count, and the offset of the first one -- that's all we need to do them all

With these primitives we can easily create cursors of any shape and load them up with data.  We don't have to
redundantly declare locals that match the shape of our select statements which is both error prone and
a hassle.

All of this is actually very easy for the code-generator.  The semantic analysis phase knows if the cursor needs
shape storage.  And it also recognizes when a variable reference like `C.x` happens, the variable references are
re-written in the AST so that the code-generator doesn't even have to know there was a cursor reference, from
its perspective the variable IS `C.x` (which it sort of is).  The code generator does have to create the
storage for the cursor but it knows it should do so because the cursor variable is marked with `SEM_TYPE_HAS_SHAPE_STORAGE`.
A cursor without this marking only gets its statement (but not always as we'll see later) and its `_cursor_has_row_`
hidden variable.

### Flowing SQLite Statements Between Procedures

Earlier we saw that we can get a cursor from a SQLite `SELECT` statement.  The cursor is used to iterate
over the `sqlite3_stmt *` that SQLite provides to us.  This process can be done between procedures.  Here's a
simple example:

```SQL
@ATTRIBUTE(cql:private)
CREATE PROC q ()
BEGIN
  SELECT "1" AS x, 2 AS y;
END;
```

This is the first example of a procedure that return a result set that we've seen.  The wiring for this is
very simple.

```c
static CQL_WARN_UNUSED cql_code q(
  sqlite3 *_Nonnull _db_,
  sqlite3_stmt *_Nullable *_Nonnull _result_stmt)
{
  cql_code _rc_ = SQLITE_OK;
  *_result_stmt = NULL;
  _rc_ = cql_prepare(_db_, _result_stmt,
    "SELECT '1', 2");
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  if (_rc_ == SQLITE_OK && !*_result_stmt) _rc_ = cql_no_rows_stmt(_db_, _result_stmt);
  return _rc_;
}
```

First note that there are now *two* hidden parameters to `q`:

* `_db_` : the database pointer as usual,
* `_result_stmt` : the statement produced by this procedure

The rest of the code is just like any other bound SQL statement.  Note that if
`_result_stmt` isn't otherwise set by the code it will be initialized to a statement
that will return zero rows.

All of this is pretty much old news except for the new hidden variable.  Note let's look how we might use
this.  We can write a procedure that calls `q`, like so:

```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR CALL q();
  FETCH C;
END;
```

This generates:

```c
CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_) {
  cql_code _rc_ = SQLITE_OK;
  sqlite3_stmt *C_stmt = NULL;
  p_C_row C = { ._refs_count_ = 1, ._refs_offset_ = p_C_refs_offset };

  _rc_ = q(_db_, &C_stmt);
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = sqlite3_step(C_stmt);
  C._has_row_ = _rc_ == SQLITE_ROW;
  cql_multifetch(_rc_, C_stmt, 2,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &C.x,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
  if (_rc_ != SQLITE_ROW && _rc_ != SQLITE_DONE) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  cql_finalize_stmt(&C_stmt);
  cql_teardown_row(C);
  return _rc_;
}
```

All of the above is *exactly* the same as the previous cases where we got
data from the database except that instead of using `cql_prepare` the
compiler produced `_rc_ = q(_db_, &C_stmt);`  That function call gives us,
of course, a ready-to-use `sqlite3_stmt *` which we can then step, and
use to fetch values.  The shape of the cursor `C` is determined by the
result type of procedure `q` -- hence they always match.

If q was in some other module, it could be declared with:

```SQL
DECLARE PROC q () (x TEXT NOT NULL, y INTEGER NOT NULL);
```

This is a procedure that takes no arguments and returns a result with the indicated shape.

CQL can generate this declaration for you if you add `--generate_exports` to the command line.  Note
that in this case `q` was marked with `@attribute(cql:private)` which caused `q` to be `static`
in the output. Hence it can't be called outside this translation unit and `--generate_exports`
won't provide the declaration.

If the private annotation were removed, the full exports for this file would be:

```SQL
DECLARE PROC q () (x TEXT NOT NULL, y INTEGER NOT NULL);
DECLARE PROC p () USING TRANSACTION;
```

And these would allow calling both procedures from elsewhere.  Simply `#include` the exports file.

There is a special function in the echoing code that can emit a procedure that was created in the
form that is needed to declare it, this is `gen_declare_proc_from_create_proc`.

### Value Cursors

Once CQL had the ability to fetch rows into a cursor with no need to declare all the locals
it was clear that it could benefit from the ability to save a copy of any given row. That is
basic cursor operations seemed like they should be part of the calculus of CQL.  Here's a
simple sample program that illustrates this.

```SQL
CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FOR SELECT "1" AS x, 2 AS y;
  FETCH C;
  DECLARE D CURSOR LIKE C;
  FETCH D from C;
END;
```

We already have a good idea what is going to happen with `C` in this program.  Let's look at the
generated code focusing just on the parts that involve `D`.

First there is a row defintion for `D`. Unsurprisingly it is exactly the samea as the one for `C`.
This must be the case since we specified `D CURSOR LIKE C`.

```c
typedef struct p_D_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} p_D_row;

#define p_D_refs_offset cql_offsetof(p_D_row, x) // count = 1
```

Then the `D` cursor variables will be needed:

```c
p_D_row D = { ._refs_count_ = 1, ._refs_offset_ = p_D_refs_offset };
```

The above also implies the cleanup code:

```c
  cql_teardown_row(D);
```

finally, we fetch `D` from `C`.  That's just some assignments:

```c
  D._has_row_ = 1;
  cql_set_string_ref(&D.x, C.x);
  D.y = C.y;
```

Importantly, there is no `D_stmt` variable.  `D` is *not* a statement cursor like `C`, it's
a so-called "value" cursor.  In that it can only hold values.

A value cursor can actually be loaded from anywhere, it just holds data.  You don't
loop over it (attempts to do so will result in errors).

The general syntax for loading such a cursor is something like this:

```SQL
 FETCH D(x, y) FROM VALUES(C.x, C.y);
```

And indeed the form `FETCH D FROM C` was rewritten automatically into the general form.
The short form is just sugar.

Once loaded, `D.x` and `D.y` can be used as always.  The data type of `D` is similar to `C`.

The AST would report:

```
{declare_cursor_like_name}: D: select: { x: text notnull, y: integer notnull }
   variable shape_storage value_cursor
```

meaning `D` has the flags `SEM_TYPE_STRUCT`, `SEM_TYPE_VARIABLE`, `SEM_TYPE_HAS_SHAPE_STORAGE`, and `SEM_TYPE_VALUE_CURSOR`.
That last flag indicates that there is no statement for this cursor, it's just values. And all such cursors must have
`SEM_TYPE_HAS_SHAPE_STORAGE` -- if they had no statement and no storage they would be -- nothing.

Value cursors are enormously helpful and there is sugar for loading them from all kinds of sources with a shape.
These forms are described more properly in [Chapter 5](https://cgsql.dev/cql-guide/ch05) of the Guide but they
all end up going through the general form, making the codegen considerably simpler. There are many examples where the semantic
analyzer rewrites a sugar form to a canonical form to keep the codegen from forking into dozens of special cases
and most of them have to do with shapes and cursors.

### Returning Value Cursors

Let's look at an example that is similar to the previous one:

```SQL
@ATTRIBUTE(cql:private)
CREATE PROC q ()
BEGIN
  DECLARE C CURSOR LIKE SELECT "1" AS x, 2 AS y;
  FETCH C USING
   "foo" x,
   3 y;
  OUT C;
END;

CREATE PROC p ()
BEGIN
  DECLARE C CURSOR FETCH FROM CALL q();
  -- do something with C
END;
```

Let's discuss some of what is above, first looking at `q`:

* `DECLARE C CURSOR LIKE SELECT "1" AS x, 2 AS y;` : this makes an empty value cursor
  * note the shape is `LIKE` the indicated `SELECT`, the `SELECT` does not actually run
* `FETCH ... USING` : this form is sugar, it lets you put the column names `x` and `y` adjacent to the values but is otherwise equivalent to the canonical form
  * `FETCH C(x, y) FROM VALUES("foo", 3);` is the canonical form
  * codegen only ever sees the canonical form
* `OUT C` is new, we'll cover this shortly

Now let's look at the C for `q`

```c
typedef struct q_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} q_row;

#define q_refs_offset cql_offsetof(q_row, x) // count = 1

cql_string_literal(_literal_1_foo_q, "foo");

typedef struct q_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} q_C_row;

#define q_C_refs_offset cql_offsetof(q_C_row, x) // count = 1

static void q(q_row *_Nonnull _result_) {
  memset(_result_, 0, sizeof(*_result_));
  q_C_row C = { ._refs_count_ = 1, ._refs_offset_ = q_C_refs_offset };

  C._has_row_ = 1;
  cql_set_string_ref(&C.x, _literal_1_foo_q);
  C.y = 3;
  _result_->_has_row_ = C._has_row_;
  _result_->_refs_count_ = 1;
  _result_->_refs_offset_ = q_refs_offset;
  cql_set_string_ref(&_result_->x, C.x);
  _result_->y = C.y;

  cql_teardown_row(C);
}
```

* the `_result_` variable is clobbed with zeros, it is assumed to be junk coming in
  * if it had valid data, the caller is expected to use `cql_teardown_row` to clean it up *before* calling
* `q_row` : this is new, this is the structure type for the result of `q`
  * it's exactly the same shape as C
  * it has its own `q_refs_offset` like other shapes
* `q_C_row` : this is the same old same old row structure for cursor C
* `static void q(q_row *_Nonnull _result_)` : q now accepts a q_row to fill in!
  * note that `q` does not have the `_db_` parameter, it doesn't use the database!
  * it is entirely possible to fill value cursors from non-database sources, e.g. constants, math, whatever
* `C` : the value cursor is declared as usual
* `C.x` and `C.y` are loaded, this resolves the `FETCH` statement
* the `_result_` fields are copied from `C`, this resolves the `OUT` statement
* `C` can be torn down
* there is no cleanup label, there are no error cases, nothing can go wrong!


The net of all this is that we have loaded a value cursor that was passed in to the procedure via
a hidden argument and it has retained references as appropriate.

Now let's look at `p`:

```c
typedef struct p_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 y;
  cql_string_ref _Nonnull x;
} p_C_row;

#define p_C_refs_offset cql_offsetof(p_C_row, x) // count = 1
void p(void) {
  p_C_row C = { ._refs_count_ = 1, ._refs_offset_ = p_C_refs_offset };

  cql_teardown_row(C);
  q((q_row *)&C); // q_row identical to cursor type

  // usually you do something with C at this point

  cql_teardown_row(C);
}
```

* `p_C_row` : the cursor type for `C` in the procedure `p` is defined
* `p_C_refs_offset` : the refs offset for `C` in `p` as usual
* `C = {...}` : the usual initialization for a cursor with shape
  * note that `C` is a value cursor, so it has no `C_stmt`
* `cql_teardown_row(C)` : releases any references in C, there are none
  * this pattern is general purpose, the call to `q` might be in a loop or something
  * in this instance the teardown here is totally redundant, but harmless
* `q((q_row *)&C)` : fetch `C` by calling `q`
   * `p_C_row` has been constructed to be exactly the same as `q_row` so this cast is safe
   * there are no error checks because `q` can't fail!
* some code that would use `C` is absent for this sample, it would go where the comment is
* the cleanup label is missing because there are no error cases, emitting the label would just cause warnings
  * such warnings are often escalated to errors in production builds...
* `cql_teardown_row(C)` is needed as always,
   * even though there is no cleanup label the `teardown` is in the cleanup section
   * the `teardown` was added as usual when `C` was declared

So with just normal value cursor codegen we can pretty easily create a situation where
procedures can move structures from one to another.  As we saw, the source of value cursors
may or may not be the database.  Value cursors are frequently invaluable in test code
as they can easily hold mock rows based on any kind of computation.

### Result Sets

In addition to returning a single row into a value cursor, or returning a statement to consume with a statement cursor,
it's possible to generate a result set.  So far the samples have included `@attribute(cql:private)` to suppress that
code.  This pattern is intended to let regular C code access the data so `private` suppresses it.

Let's consider a simple example, this example returns only one row but the mechanism works for any number of rows,
we're just using this form because it's what we've used so far and its simple.  Let's begin:

```SQL
CREATE PROC p ()
BEGIN
  SELECT "1" AS x, 2 AS y;
END;
```

The core generated function is this one:

```c
CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_, sqlite3_stmt *_Nullable *_Nonnull _result_stmt) {
  cql_code _rc_ = SQLITE_OK;
  *_result_stmt = NULL;
  _rc_ = cql_prepare(_db_, _result_stmt,
    "SELECT '1', 2");
  if (_rc_ != SQLITE_OK) { cql_error_trace(); goto cql_cleanup; }
  _rc_ = SQLITE_OK;

cql_cleanup:
  if (_rc_ == SQLITE_OK && !*_result_stmt) _rc_ = cql_no_rows_stmt(_db_, _result_stmt);
  return _rc_;
}
```

We've seen this before, it creates the SQLite statement.  But that isn't all the code that was generated,
let's have a look at what else we got in our outputs:

```c
CQL_WARN_UNUSED cql_code p_fetch_results(
  sqlite3 *_Nonnull _db_,
  p_result_set_ref _Nullable *_Nonnull result_set)
{
  sqlite3_stmt *stmt = NULL;
  cql_profile_start(CRC_p, &p_perf_index);
  cql_code rc = p(_db_, &stmt);
  cql_fetch_info info = {
    .rc = rc,
    .db = _db_,
    .stmt = stmt,
    .data_types = p_data_types,
    .col_offsets = p_col_offsets,
    .refs_count = 1,
    .refs_offset = p_refs_offset,
    .encode_context_index = -1,
    .rowsize = sizeof(p_row),
    .crc = CRC_p,
    .perf_index = &p_perf_index,
  };
  return cql_fetch_all_results(&info, (cql_result_set_ref *)result_set);
}
```

`p_fetch_results` does two main things:

* `cql_code rc = p(_db_, &stmt)` : it calls the underlying function to get the statement
* `cql_fetch_all_results` : it calls a standard helper to read all the results from the statement and put them into `result_set`
  * to do the fetch, it sets up a `cql_fetch_info` for this result set, this has all the information needed to do the fetch
  * the intent here is that even a complex fetch with lots of columns can be done economically, and
  * the code that does the fetching is shared

Let's look at the things that are needed to load up that `info` structure.

```c
typedef struct p_row {
  cql_int32 y;
  cql_string_ref _Nonnull x;
} p_row;

uint8_t p_data_types[p_data_types_count] = {
  CQL_DATA_TYPE_STRING | CQL_DATA_TYPE_NOT_NULL, // x
  CQL_DATA_TYPE_INT32 | CQL_DATA_TYPE_NOT_NULL, // y
};

#define p_refs_offset cql_offsetof(p_row, x) // count = 1

static cql_uint16 p_col_offsets[] = { 2,
  cql_offsetof(p_row, x),
  cql_offsetof(p_row, y)
};
```

* `p_row` : the row structure for this result set, same as always, reference types last
* `p_data_types` : an array with the data types encoded as bytes
* `p_refs_offset` : the offset of the first reference type
* `p_col_offsets` : this is the offset of each column within the row structure
  * these are in column order, not offset order

Code generation creates a `.c` file and a `.h` file, we haven't talked much about the `.h`
because it's mostly prototypes for the functions in the `.c` file.  But in this case we have
a few more interesting things.  We need just two of them:

```c
#define CRC_p -6643602732498616851L

#define p_data_types_count 2
```

Now we're armed to discuss loading the `info` structure:

* `.rc` : the fetcher needs to know if `p` was successful, it won't read from the statement if it wasn't
* `.db` : the database handle, the fetcher needs this to call SQLite APIs
* `.stmt` : the statement that came from `p` that is to be enumerated
* `.data_types` : types of the columns, this tells the fetcher what columns to read to the statement in what order
* `.col_offsets` : the column offsets, this tells the fetcher were to store the column data within each row
* `.refs_count` : the number of references in the row, this is needed to tear down the rows in the result set when it is released
* `.refs_offset` : the first reference offset, as usual this tells the fetcher where the references that need to be released are
* `.encode_context_index` : it's possible to have sensitive fields encoded, this identifies an optional column that will be combined with the sensitive data
. `.rowsize` : the size of `p_row`, this is needed to allocate rows in a growable buffer
* `.crc` : this is a CRC of the code of `p`, it's used to uniquely identify `p` economically, performance logging APIs typically use this CRC in a begin/end logging pair
* `.perf_index` : performance data for stored procedures is typically stored in an array of stats, CQL provides storage for the index for each procedure

With this data (which is in the end pretty small) the `cql_fetch_all_results` can do all the things it needs to do:

* `cql_profile_start` has already been called, it can call `cql_profile_end` once the data is fetched
  * `cql_profile_start` and `_end` do nothing by default, but those macros can be defined to log performance data however you like
* it can allocate a `bytebuf` with `cql_bytebuf_open` and then grow it with `cql_bytebuf_alloc`
  * in the end all the rows are in one contiguous block of storage
* `cql_multifetch_meta` is used to read each row from the result set, it's similar to `cql_multifetch`
  * the `meta` version uses `data_types` and `column_offsets` instead of varargs but is otherwise the same
  * the first member of the `col_offsets` array is the count of columns

With this background, `cql_fetch_all_results` should be very approachable.  There's a good bit of work but it's all very simple.

```c
// By the time we get here, a CQL stored proc has completed execution and there is
// now a statement (or an error result).  This function iterates the rows that
// come out of the statement using the fetch info to describe the shape of the
// expected results.  All of this code is shared so that the cost of any given
// stored procedure is minimized.  Even the error handling is consolidated.
cql_code cql_fetch_all_results(
  cql_fetch_info *_Nonnull info,
  cql_result_set_ref _Nullable *_Nonnull result_set) {...}
```

The core of that function looks like this:

```c
  ...
  cql_bytebuf_open(&b);
  ...
  for (;;) {
    rc = sqlite3_step(stmt);
    if (rc == SQLITE_DONE) break;
    if (rc != SQLITE_ROW) goto cql_error;
    count++;
    row = cql_bytebuf_alloc(&b, rowsize);
    memset(row, 0, rowsize);

    cql_multifetch_meta((char *)row, info);
  }
  ...
  cql_profile_stop(info->crc, info->perf_index);
  ...
```

* `cql_bytebuf_open` : open the buffer, get ready to start appending rows
* `sqlite3_step` : keep reading while we get `SQLITE_ROW`, stop on `SQLITE_DONE`
* `cql_bytebuf_alloc` : allocate a new row in the buffer
* `memset` : zero the row
* `cql_multifetch_meta` : read the data from the the statement into the row
* `cql_profile_stop` : signals that processing is done and profiling can stop
* if all goes well, `SQLITE_OK` is returned as usual

The remaining logic is largely about checking for errors and tearing down the result set
if anything goes wrong.  There is not very much to it, and it's worth a read.

Now recall that the way `cql_fetch_all_results` was used, was as follows:

```c
  return cql_fetch_all_results(&info, (cql_result_set_ref *)result_set)
```

And `result_set` was the out-argument for the the `p_fetch_results` method.

So `p_fetch_results` is used to get that result set.  But what can you do with it?
Well, the result set contains copy of all the selected data, ready to use in with a C-friendly API.
The interface is in the generated `.h` file.  Let's look at that now, it's the final piece of the puzzle.

```c
#ifndef result_set_type_decl_p_result_set
#define result_set_type_decl_p_result_set 1
cql_result_set_type_decl(p_result_set, p_result_set_ref);
#endif

extern cql_string_ref _Nonnull p_get_x(p_result_set_ref _Nonnull result_set, cql_int32 row);
extern cql_int32 p_get_y(p_result_set_ref _Nonnull result_set, cql_int32 row);
extern cql_int32 p_result_count(p_result_set_ref _Nonnull result_set);
extern CQL_WARN_UNUSED cql_code p_fetch_results(sqlite3 *_Nonnull _db_, p_result_set_ref _Nullable *_Nonnull result_set);

#define p_row_hash(result_set, row) cql_result_set_get_meta( \
  (cql_result_set_ref)(result_set))->rowHash((cql_result_set_ref)(result_set), row)

#define p_row_equal(rs1, row1, rs2, row2) \
  cql_result_set_get_meta((cql_result_set_ref)(rs1))->rowsEqual( \
    (cql_result_set_ref)(rs1), \
    row1, \
    (cql_result_set_ref)(rs2), \
```

* `cql_result_set_type_decl` : declares `p_result_set_ref`
  * to avoid being defined more than once, the declaration is protected by `#ifndef result_set_type_decl_p_result_set`
* `p_get_x`, `p_get_y` : allow access to the named fields of the result set at any given row
* `p_result_count` :  provides the count of rows in the result set
* `p_fetch_results` : the declaration of the fetcher (previously discussed)
* `p_row_hash` : provides a hash of any given row, useful for detecting changes between result sets
* `p_row_equal` : tests two rows in two results sets of the same shape for equality

The getters are defined very simply:

```c
cql_string_ref _Nonnull p_get_x(p_result_set_ref _Nonnull result_set, cql_int32 row) {
  p_row *data = (p_row *)cql_result_set_get_data((cql_result_set_ref)result_set);
  return data[row].x;
}

cql_int32 p_get_y(p_result_set_ref _Nonnull result_set, cql_int32 row) {
  p_row *data = (p_row *)cql_result_set_get_data((cql_result_set_ref)result_set);
  return data[row].y;
}
```

The `p_row` is exactly the right size, and of course the right shape, the final access looks something like `data[row].x`.

#### Result Sets from the OUT statement

Recalling this earlier example:

```SQL
CREATE PROC q ()
BEGIN
  DECLARE C CURSOR LIKE SELECT "1" AS x, 2 AS y;
  FETCH C USING
   "foo" x,
   3 y;
  OUT C;
END;
```

The original example had `@attribute(cql:private)` to suppress the result set, but normally a one-row result is
is generated from such a method.  The C API is almost identical.  However, there count is always 0 or 1.

The getters do not have the row number:

```c
extern cql_string_ref _Nonnull q_get_x(q_result_set_ref _Nonnull result_set);
extern cql_int32 q_get_y(q_result_set_ref _Nonnull result_set);
```

The actual getters are nearly the same as well

```c
cql_string_ref _Nonnull q_get_x(q_result_set_ref _Nonnull result_set) {
  q_row *data = (q_row *)cql_result_set_get_data((cql_result_set_ref)result_set);
  return data->x;
}

cql_int32 q_get_y(q_result_set_ref _Nonnull result_set) {
  q_row *data = (q_row *)cql_result_set_get_data((cql_result_set_ref)result_set);
  return data->y;
}
```

Basically `data[row].x` just became `data->x` and the rest is nearly the same.
Virtually all the code for this is shared.

You can find all this and more in `cg_c.c` by looking here:

```c
// If a stored procedure generates a result set then we need to do some extra work
// to create the C friendly rowset creating and accessing helpers.  If stored
// proc "foo" creates a row set then we need to:
//  * emit a struct "foo_row" that has the shape of each row
//    * this isn't used by the client code but we use it in our code-gen
//  * emit a function "foo_fetch_results" that will call "foo" and read the rows
//    from the statement created by "foo".
//    * this method will construct a result set object via cql_result_create and store the data
//    * the remaining functions use cql_result_set_get_data and _get_count to get the data back out
//  * for each named column emit a function "foo_get_[column-name]" which
//    gets that column out of the rowset for the indicated row number.
//  * prototypes for the above go into the main output header file
static void cg_proc_result_set(ast_node *ast)
```

There are many variations in that function to handle the cases mentioned so far, but they are
substantially similar to each other with a lot of shared code.  There is one last variation
we should talk about and that is the `OUT UNION` form.  It is the most flexible of them all.

#### Result Sets from the OUT UNION statement

The `OUT` statement, allows the programmer to produce a result set that has exactly one row,
`OUT UNION` instead accumulates rows.  This is very much like writing your own `fetcher` procedure
with your own logic.  The data could come from the database, by, for instance, enumerating
a cursor.  Or it can come from some computation or a mix of both.  Here's a very simple example:

```SQL
CREATE PROC q ()
BEGIN
  DECLARE C CURSOR LIKE SELECT 1 AS x;
  LET i := 0;
  WHILE i < 5
  BEGIN
    FETCH C(x) FROM VALUES(i);
    OUT UNION C;
    SET i := i + 1;
  END;
END;
```

Let's look at the code for the above, it will be very similar to other examples we've seen so far:

```c
typedef struct q_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 x;
} q_C_row;
void q_fetch_results(q_result_set_ref _Nullable *_Nonnull _result_set_) {
  cql_bytebuf _rows_;
  cql_bytebuf_open(&_rows_);
  *_result_set_ = NULL;
  q_C_row C = { 0 };
  cql_int32 i = 0;

  cql_profile_start(CRC_q, &q_perf_index);
  i = 0;
  for (;;) {
  if (!(i < 5)) break;
    C._has_row_ = 1;
    C.x = i;
    cql_retain_row(C);
    if (C._has_row_) cql_bytebuf_append(&_rows_, (const void *)&C, sizeof(C));
    i = i + 1;
  }

  cql_results_from_data(SQLITE_OK, &_rows_, &q_info, (cql_result_set_ref *)_result_set_);
}
```

* `q_C_row` : the shape of the cursor, as always
* `_rows_` : the `bytebuf` that will hold our data
* `cql_bytebuf_open(&_rows_);` : initializes the buffer
* `cql_profile_start(CRC_q, &q_perf_index);` : start profiling as before
* `for (;;)` : the while pattern as before
* `C.x = i;` : loads the cursor
* `cql_retain_row(C);` : retains any references in the cursor (there are none)
  * we're about to copy the cursor into the buffer so all refs need to be +1
* `cql_bytebuf_append` : append the the cursor's bytes into the buffer
* the loop does its repetitions until finally
* `cql_results_from_data` : used instead of `cql_fetch_all_results` because all the data is already prepared
  * in this particular example there is nothing to go wrong so it always gets `SQLITE_OK`
  * in a more complicated example, `cql_results_from_data` frees any partly created result set in case of error
  * `cql_results_from_data` also performs any encoding of sensitive data that might be needed
* `q_info` : created as before, but it can be static as it's always the same now

Importantly, when using `OUT UNION` the codegen only produces `q_fetch_results`, there is no `q`.
If you try to call `q` from CQL you will instead call `q_fetch_results`. But since
many results as possible, a cursor is needed to make the call.

Here's an example, here `p` calls the `q` method above:

```SQL
CREATE PROC p (OUT s INTEGER NOT NULL)
BEGIN
  DECLARE C CURSOR FOR CALL q();
  LOOP FETCH C
  BEGIN
    SET s := s + C.x;
  END;
END;
```

And the relevant code for this is as follows:

```c
typedef struct p_C_row {
  cql_bool _has_row_;
  cql_uint16 _refs_count_;
  cql_uint16 _refs_offset_;
  cql_int32 x;
} p_C_row;

CQL_WARN_UNUSED cql_code p(sqlite3 *_Nonnull _db_, cql_int32 *_Nonnull s) {
  cql_contract_argument_notnull((void *)s, 1);

  cql_code _rc_ = SQLITE_OK;
  q_result_set_ref C_result_set_ = NULL;
  cql_int32 C_row_num_ = 0;
  cql_int32 C_row_count_ = 0;
  p_C_row C = { 0 };

  *s = 0; // set out arg to non-garbage
  q_fetch_results(&C_result_set_);
  C_row_num_ = C_row_count_ = -1;
  C_row_count_ = cql_result_set_get_count((cql_result_set_ref)C_result_set_);
  for (;;) {
    C_row_num_++;
    C._has_row_ = C_row_num_ < C_row_count_;
    cql_copyoutrow(NULL, (cql_result_set_ref)C_result_set_, C_row_num_, 1,
                   CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.x);
    if (!C._has_row_) break;
    *s = (*s) + C.x;
  }
  _rc_ = SQLITE_OK;

  cql_object_release(C_result_set_);
  return _rc_;
}
```

* `p_C_row` : the cursor row as always
* `cql_contract_argument_notnull((void *)s, 1)` : verify that the out arg pointer is not null
* `C_result_set_` : this will hold the result set from `q_fetch_results`
* `C_row_num_` : the current row number being processed in the result set
* `C_row_count_` : the total number of rows in the result set
* other locals are intialized as usual
* `*s = 0;` : set the out arg to non-garbage as usual
* `q_fetch_results` : get the result set from `q_fetch_results`
  * in this case no database access was required so this API can't fail
  * `C_row_num` : set to -1
  * `C_row_count` : set to the row count
* `cql_copyoutrow` : copies one row from the result set into the cursor
* `*s = (*s) + C.x;` : computes the sum
* `cql_object_release` : the result set is torn down
* if there are any reference fields in the cursor there would have been a `cql_teardown_row(C)`

In short, this is another form of cursor, it's a value cursor, so it has no statement but
it also needs a result set, a count and an index to go with it so that it can enumerate the result set.

In the AST it looks like this:

```
{name C}: C: select: { x: integer notnull } variable shape_storage uses_out_union
```

This implies we have the semantic flags: `SEM_TYPE_STRUCT`, `SEM_TYPE_VARIABLE`, `SEM_TYPE_HAS_SHAPE_STORAGE`, and `SEM_TYPE_USES_OUT_UNION`.

The difference is of course the presence of `SEM_TYPE_USES_OUT_UNION`.

This is the last of the cursor forms and the final complication of `cg_proc_result_set`.

### Recap

At present `cg_c.c` is a little over 7400 lines of code, maybe 1500 of those lines are comments.   So `cg_c.c` is actually quite a
bit smaller and simpler than `sem.c` (roughly 1/3 the size).  It is, however, the most complex of the code generators by far.
Part 3 of the internals guide has come out a lot larger than Part 2 but that's mainly because there are a few more cases worth
discussing in detail and the code examples of Part 3 are bigger than the AST examples of Part 2.

Topics covered included:

* compiling expressions into C, including nullable types
* techniques used to generate control flow
* creation of result sets, including:
   * various helpers to do the reading economically
   * the use of `cql_bytebuf` to manage the memory
* create the text for SQLite statements and binding variables to that text
* error management, and how it relates to `TRY` and `CATCH` blocks
* use of cleanup sections to ensure that references and SQLite statement lifetime is always correct
* the contents of the `.c` and `.h` files and the key sections in them
* the use of `charbuf` to create and assemble fragments

As with the other parts, this is not a complete discussion of the code but a useful survey that
should give readers enough context to understand `cg_c.c` and the runtime helpers in `cqlrt.c`
and `cqlrt_common.c`.  Good luck in your personal exploration!

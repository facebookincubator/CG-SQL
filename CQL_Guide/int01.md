---
id: int01
title: "CQL Internals Guide: Part 1"
sidebar_label: "CQL Internals Guide: Part 1"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Overview

The following is a summary of the implementation theory of the CQL compiler.  This is
an adjuct to the Guide proper, which describes the language, and to a lesser extent
the code that the compiler generates.

The actual code is heavily commented, so it's better to read the code to see the details
of how any particular operation happens rather than try to guess from the language specification
or from this overview.  However, some things, like general principles, really are nowhere,
or everywhere, in the codebase and its important to understand how things hang together.

If you choose to go on adventures in the source code, especially if you aren't already familiar
with compilers and how they are typically built, this is a good place to start.

## Lexical Analysis, Parsing, and the Abstract Syntax Tree

The CQL compiler uses a very standard lex+yacc parser, though to be more precise it's flex+bison.
The grammar is a large subset of the SQLite dialect of SQL augmented with control flow and compiler
directives.  As a consequence its a useful asset in and of itself -- if you're looking for an
economical SQL grammar you could do a lot worse than start with the one CQL uses.  The grammar is
of course in the usual `.y` format that bison consumes but its also extracted into more readable
versions for use in the railroad diagram and the documentation.  Any of those sources would be
a good starting place for a modest SQL project.

### Lexical Analysis

Inside of `cql.l` you'll find the formal defintion of all the lexemes.  There are many that correspond to the
various tokens needed to parse the SQL language.  There's no need to dicsuss the approximately 150 such tokens
but the following points are of general interest:

* the lexer expects plain text files, and all the lexemes are defined in plain ASCII only, however
  * the presence of UTF8 characters in places where any text is legal (such as string literals) should just work
* all of the lexemes are case-insensitive
  * this means only vanilla ASCII insensitivity, no attempt is made to understand more complex code points
* multi-word lexemes typically are defined with an expression like this:  `IS[ \t]+NOT[ \t]+FALSE/[^A-Z_]`
  * in most cases, to avoid ambiguity, and to get order of operations correct, the entire word sequence is one lexeme
  * only spaces and tabs are allowed between the words
  * the token ends on non-identifier characters, so the text "X IS NOT FALSEY" must become the lexemes { `X`, `IS_NOT`, `FALSEY` } and not { `X`, `IS_NOT_FALSE`, `Y` }
    * the latter would result in the longest lexeme so without the trailing qualifier and hence would be preferred, hence where a contuation is possible the trailing context must be specified in multi=word lexemes
  * Note: a quick reading shows this isn't done completely consistently and that should be fixed
* there is special processing needed to lex `/* ... */` comments correctly
* there are token types for each of the sorts of literals that can be encountered, special care is taken to keep the literals in string form so that no precision is lost
  * integer literals are compared against 0x7fffffff and if greater they automatically become long literals even if they are not marekd with the trailing `L` as in `1L`
  * string literals include the quotation marks in the lexeme text which distinguishes them from identifiers, they are otherwise encoded similarly
* the character class `[-+&~|^/%*(),.;!<>:=]` produces single character tokens for identifiers, other non-matching single characters (e.g. `'$'` produce an error)
* line directives `^#\ [0-9]+\ \"[^"]*\".*` get special processing so that pre-processed input does not lose file and line number fidelity

### Parsing and the Abstract Syntax Tree

Inside of `cql.y` you will find the token declarations, precededence rules, and all of the productions in the overall grammar. The grammar processing
does as little as possible in that stage to create an abstract syntax tree.  AST itself is a simple binary tree;  where nodes might require more than just
left and right children to specify things fully, additional nodes are used in the tree shape rather than introducy n-ary nodes.  This means the tree is
sometimes bigger but generally not very much bigger.  The benefit is that the AST can always be walked generically as a binary tree, so if you need
to find all the `table_factor` nodes it is easy to do so without having to worry about how every kind of node expands.  If new node types come along
the generic walkers can go through those as well.  All of the grammar productions simply make one or more AST nodes and link them together so that in the
end there is a single root for the entire program.

There are 4 kinds of ast nodes, they all begin with the following 5 fields, these represent the AST base type if you like.

```C
  const char *_Nonnull type;
  struct sem_node *_Nullable sem;
  struct ast_node *_Nullable parent;
  int32_t lineno;
  const char *_Nonnull filename;
```

* `type` -- a string literal that uniquely identifies the node type
  * the string literal is compared for identity (it's an exact pointer match) you don't `strcmp` it
* `sem` -- begins as `NULL` this is where the semantic type goes once semantic processing happens
* `parent` -- the parent node in the AST (not often used but sometimes indispensible)
* `lineno` -- the line number of the file that had the text that led to this AST (useful for errors)
* `filename` -- the name of the file that had the text that led to this AST (useful for errors)
  * this string is durable, should not be mutated, and is shared between MANY nodes

#### Generic AST node

```C
typedef struct ast_node {
  ... the common fields
  struct ast_node *_Nullable left;
  struct ast_node *_Nullable right;
} ast_node;
```

This node gives the tree its shape, this is is how all the expression operators and statments get encoded.  An example says this more clearly

```
SET X := 1 + 3;

  {assign}
  | {name X}
  | {add}
    | {int 1}
    | {int 3}
```

In the above "assign" and "add" are the generic nodes.  Node that these can be leaves but often are not.

Note that in the above the node type was directly printed (because it's a meaningful name).  Likewise, the type needs no decoding
when viewing the AST in a debugger.  Simply printing the node with something like `p *ast` in lldb will show you
all the node fields and the type in a human readable fashion.

#### Grammar Code Node

```C
typedef struct int_ast_node {
  ... the common fields
  int64_t value;
} int_ast_node;
```

This kind of node holds an integer that quantifies some kind of choice in the grammar.  Note that this does NOT hold numeric literals (see below).
The file `ast.h` includes many `#define` constants such as:

```C
define JOIN_INNER 1
define JOIN_CROSS 2
define JOIN_LEFT_OUTER 3
define JOIN_RIGHT_OUTER 4
define JOIN_LEFT 5
define JOIN_RIGHT 6
```

The integer here is one of those values.  It can be a bitmask, or an enumeration.  In this statement:

```sql
SELECT x
  FROM a
  LEFT OUTER JOIN b;
```

a part of the AST will look like this:

```
| {join_clause}
| | {table_or_subquery}
| | | {name a}
| | {join_target_list}
|   | {join_target}
|     | {int 3}
|     | {table_join}
|       | {table_or_subquery}
|         | {name b}
```
The `{int 3}` ia an int_ast_node and it corresponds to `JOIN_LEFT_OUTER`.

This node type is always a leaf.

#### String Node

```C
typedef struct str_ast_node {
  ... the common fields
  const char *_Nullable value;
  bool_t cstr_literal;
} str_ast_node;
```

* `value` -- the text of the string
* `cstr_literal` -- true if the string was specified using "C" syntax (see below)

This node type holds:
 * string literals
 * blob literals
 * identifiers

CQL supports C style string literals with C style escapes such as `"foo\n"`.  These are normalized into the SQL version of the same literal
so that SQLite will see a literal it understands.  However, if the origin of the string was the C string form (i.e. `"foo"` rather than `'bar'`)
then the `cstr_literal` boolean flag will be set.  When echoing the program back as plain text, the C string will be converted back to the C form
for display.  This means, for instance, that the comments in the output C correspond to the original string format even though the code that gets
set to SQLite is always in SQL format.

Identifiers can be distinguised from string literals because the quotation marks are stil in the string.

This node type is always a leaf.

#### Number Node

```C
typedef struct num_ast_node {
  ... the common fields
  int32_t num_type;
  const char *_Nullable value;
} num_ast_node;
```

* `num_type` the kind of numeric
* `value` the text of the number

All numerics are stored as strings so that there is no loss of precision.  This is important because it is entirely possible that
the CQL compiler is build with a different floating point library, or different integer sizes, than the target system.  As a result
CQL does not evaluate anything outside of an explicit `const()` expression.  This avoids integer overflows at compile time or loss
of floating point precesion;  Constants in the text of the output are emitted byte-for-byte as they appeared in the source code.

This node type is always a leaf.

### Examples

#### Example 1: A let statement and expression

```
LET x := 1 + (3 - 2);

  {let_stmt}
  | {name x}
  | {add}
    | {int 1}
    | {sub}
      | {int 3}
      | {int 2}
```
Note that there are no parentheses in the AST but it exactly and authoritatively captures the precedence with its shape.
This means, among other things, that when CQL echos its input, any redundant parentheses will be gone.

#### Example 2: An if/else construct

```
IF x THEN
  LET x := 1.5e7;
ELSE IF y THEN
  LET y := 'that';
ELSE
  LET z := "this";
END IF;

  {if_stmt}
  | {cond_action}
  | | {name x}
  | | {stmt_list}
  |   | {let_stmt}
  |     | {name x}
  |     | {dbl 1.5e7}
  | {if_alt}
    | {elseif}
    | | {cond_action}
    |   | {name y}
    |   | {stmt_list}
    |     | {let_stmt}
    |       | {name y}
    |       | {strlit 'that'}
    | {else}
      | {stmt_list}
        | {let_stmt}
          | {name z}
          | {strlit 'this'}
```
Note that the string "this" was normalized to 'this' (which was trivial in this case) but rest assured that
`cstr_literal` was set.  This is shown because the text of the statement came out with double quotes.
The text above was not the input to the compiler, the compiler was actually given this text

```
if x then let x := 1.5e7; else if y then let y := 'that'; else let z := "this"; end if;
```

And it was normalized into what you see as part of the output.  We'll talk about this output echoing in coming sections,
but as you can see, the compiler can be used as a SQL normalizer/beautifier.

#### Example 3: A select statement

```
SELECT *
  FROM foo
  INNER JOIN bar
  WHERE foo.x = 1
LIMIT 3;

  {select_stmt}
  | {select_core_list}
  | | {select_core}
  |   | {select_expr_list_con}
  |     | {select_expr_list}
  |     | | {star}
  |     | {select_from_etc}
  |       | {join_clause}
  |       | | {table_or_subquery}
  |       | | | {name foo}
  |       | | {join_target_list}
  |       |   | {join_target}
  |       |     | {int 1}
  |       |     | {table_join}
  |       |       | {table_or_subquery}
  |       |         | {name bar}
  |       | {select_where}
  |         | {opt_where}
  |         | | {eq}
  |         |   | {dot}
  |         |   | | {name foo}
  |         |   | | {name x}
  |         |   | {int 1}
  |         | {select_groupby}
  |           | {select_having}
  | {select_orderby}
    | {select_limit}
      | {opt_limit}
      | | {int 3}
      | {select_offset}
```

As you can see the trees rapidly get more complex.  The select statement has many optional pieces and
so the AST actually has places in its skeleton where these could go but are abasent (e.g. group by,
having, order, and offset are all missing).

The shape of the AST is self evident.

The compiler can produce these diagrams in 'dot' format which makes pretty pictures but the reality is that
for non-trivial examples that pictures are so large as to be unreadable whereas the simple text format
remains readable even up to several hundred lines of output and is readily searchable, and diffable.  The test
suites for semantic analysis do pattern matching on the text of the AST to verify correctness.

We'll discuss semantic analysis in later sections.

### AST definitions

`ast.h` defines the all the tree types mentioned above.  There are helper methods to create AST nodes
with type safety.  It includes helper functions for the various leaf types mentioned above
but also for the various "normal" types.  These are specified using the AST macros `AST`, `AST1`, and `AST0`

Examples:

```
AST0(star)
AST1(not)
AST(or)
```

This says that:

* the `star` AST node (used in `select *`) is a leaf, it has 0 children
  * this means the left and right nodes will always be `NULL`
* the `not` AST node (used in `select NOT x`) is unary
  * this means only the left node is populated, the right is always `NULL`
  * node many unary nodes have optional children, so the left node might still be `NULL`
* the `or` AST node (used in `select x OR y`) is binary
  * this means both its left and right children are populated
  * note that some binary nodes have optional children, so left or right still might be `NULL`

At present there are about 300 unique AST node types.

## Echoing the AST

The first set of features that were built after parsing was the ability to echo back the parse tree as SQL again.
This all happens in `gen_sql.c` and since it has to be able to echo back any tree it often has the best
and simplest examples of how to crack the AST for a particular type of node.

There are several reasons why we might want to echo the SQL but the inescapable one is this:  any hunk of
SQL that appears as part of a CQL program (i.e. DDL/DML rather than control flow like IF/WHILE) has to go
to SQLite and SQLite expects it to be plain text.  So the AST must be reformatted as plain text that is
exactly equivalent to the input.  The process of parsing removes extra white space and parentheses and
instead some standard formatting (including indenting) is applied to the output text.  This has the effect
of normalizing the input and potentially beautifying it as well if it was particularly poorly formatted initially.

To see these features you need only run cql with no arguments, by default it reads stdin, makes the AST, and
then emits the normalized formatted text. If there are no syntax errors, the input and the output should be
equivalent.  And here is where we start to see some of the extra demands.

CQL includes a lot of versioning directives like `@create(...)` `@delete(...)` and so forth.  SQLite should
never see these things when the DDL for SQLite is emitted.  But when echoing the input they should be included.
Additionally, any local or global variables in a SQL statement should be replaced with `?` in the text
that goes to SQLite and then followed up with binding instructions.  We'll cover the binding more in the
section code generation, but importantly this also has to significantly alter the desired outcome.
As a result the standard formatter includes extensive configurably to get these results.

### Configuring the Output with Callbacks and Flags

Some of these features, like variable binding, require a callback to formatter's client to get a notification,
these callbacks along with a few control variables decide exactly how the output will be created.  The
control structure is `struct gen_sql_callbacks`, described below.  It's quite flexible so it ends up
not changing very often.  It includes the various callbacks (all of which are optional) and each callback
gets a 'context' pointer. This is some `void *` value that you provide which will be given to your function
along with the AST pointer relevant to the call.  The callback also gets the current output buffer so
it can choose to emit something (like '?' into the stream)


```C
// signature for a callback, you get your context plus the ast
// if you return true then the normal output is suppressed
// in any case the output you provide is emitted
typedef bool_t (*_Nullable gen_sql_callback)(
  struct ast_node *_Nonnull ast,
  void *_Nullable context,
  charbuf *_Nonnull output
);
```

The meaning of the `bool_t` return value varies depend on which callback it is.

The coarsest control is provided by the generation mode.  It is one of these values.

```C
// These modes control the overall style of the output
enum gen_sql_mode {
  gen_mode_echo,          // Prints everything in the original, with standard whitespace and parentheses
  gen_mode_sql,           // Prints the AST formatted for SQLite consumption, omits anything CQL specific
  gen_mode_no_annotations // Equivalent to gen_mode_echo without versioning attributes or generic attribues
                          // * @create, @delete, @recreate, and @attribute are removed
                          // * statements like @echo are not affected, nor is the type specifier @sensitive
};
```

The actual callbacks structure is optional, if it is null then you get full echo of the AST with no changes.  Otherwise
the callbacks and flags alter the behavior somewhat.

```C
// Callbacks allow you to significantly alter the generated sql, see the particular flags below.
typedef struct gen_sql_callbacks {
  // Each time a local/global variable is encountered in the AST, this callback is invoked
  // this is to allow the varialbe reference to be noted and replaced with ? in the generated SQL
  gen_sql_callback _Nullable variables_callback;
  void *_Nullable variables_context;

  // Each time a column definition is emitted this callback is invoked, it may choose to
  // suppress that column.  This is used to remove columns that were added in later schema
  // versions from the baseline schema.
  gen_sql_callback _Nullable col_def_callback;
  void *_Nullable col_def_context;

  // This callback is used to expland the * in select * or select T.*
  gen_sql_callback _Nullable star_callback;
  void *_Nullable star_context;

  // This callback is used to force the "IF NOT EXISTS" form of DDL statements when generating
  // schema upgrade steps.  e.g. a "CREATE TABLE Foo declarations get "IF NOT EXISTS" added
  // to them in upgrade steps.
  gen_sql_callback _Nullable if_not_exists_callback;
  void *_Nullable if_not_exists_context;

  // If true, hex literals are converted to decimal.  This is for JSON which does not support hex literals.
  bool_t convert_hex;

  // If true casts like "CAST(NULL as TEXT)" are reduced to just NULL.  The type information is not needed
  // by SQLite so it just wasts space.
  bool_t minify_casts;

  // If true then unused aliases in select statements are elided to save space.  This is safe because
  // CQL always binds the top level select statement by ordinal anyway.
  bool_t minify_aliases;

  // mode to print cql statement: gen_mode_echo, gen_mode_sql, gen_mode_no_annotations.
  // gen_mode_sql mode causes the AS part of virtual table to be suppressed
  enum gen_sql_mode mode;

  // If CQL finds a column such as 'x' below'
  //
  // create table foo(
  //   x long_int primary key autoincrement
  // );
  //
  // that column must be converted to this form:
  //
  // create table foo(
  //   x integer primary key autoincrement
  // );
  //
  // This is because SQLite mandates that autoincrement must be exactly
  // in the second example above however, it is also the case that in SQLite
  // an integer can store a 64 bit value.  So sending "integer" to SQLite while
  // keeping the sense that the column is to be treated as 64 bits in CQL works
  // just fine.
  //
  // However, when we are emitting CQL (rather than SQL) we want to keep
  // the original long_int type so as not to lose fidelity when processing
  // schema for other semantic checks (such as matching FK data types).
  //
  // This flag is for that purpose: It tells us that the target isn't SQLite
  // and we don't need to do the mapping (yet). Indeed, we shouldn't, or the
  // types will be messed up.
  //
  // In short, if CQL is going to process the output again, use this flag
  // to control the autoincrement transform.  It might be possible to fold
  // this flag with the mode flag but it's sufficiently wierd that this
  // extra documention and special handling is probably worth the extra
  // boolean storage.
  bool_t long_to_int_conv;
} gen_sql_callbacks;
```

Each callback can be best understood by reading the source, so we'll avoid
trying to precisely define it here.  But it is helpful to give the jist
of these options.

* `mode` : one of the three enum modes that control overall behavior
* `variables_callback` : invoked when a variable appears in the SQL, the caller can record the specific variable and then use it for binding
* `col_def_callback` : when creating the "baseline" schema you don't want column defintions from later schema to be included, this gives you a chance to suppress them
* `star_callback` : normally the `*` in `select *` or `select T.*` is expanded when emitting for SQLite, this callback does the expansion when appropriate
* `if_not_exists_callback` : when generating DDL for schema upgrade you typically want to force `IF NOT EXISTS` to be added to the schema even if it wasn't present in the declaration, this callback lets you do that
* `convert_hex` : hex constants are converted to decimal if true, used when emitting JSON because it doesn't understand hex constants
* `minify_casts` : minification conversts casts like `CAST(NULL AS TEXT)` to just `NULL` -- the former is only useful for type information, SQLite does need to see it
* `minify_aliases` : unused column aliases as in `select foo.x as some_really_long_alias` can be removed from the output when targetting SQLite top save space

### Invoking the Generator

There are several generation functions but they all follow a similar pattern, the differences are essentially what fragment of the AST
they expect to begin on.  We'll just cover one here.

```C
cql_noexport void gen_statement_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
```

This has the typical signature
* `ast` : the part of the tree to print
* `_callbacks` : the optional callbacks described above

To use these you'll need

```C
cql_noexport void gen_init(void);
cql_noexport void gen_cleanup(void);
```

You'll want to call `gen_init()` one time before doing any generation.  That sets up the necessary tables.
When you're done use `gen_cleanup()` to release any memory that was allocated in setup.
You don't have to do the cleanup step if the process is going to exit anyway, but because of the amalgam
options, `cql_main()` assumes it might be called again and so it tidies things up rather than leak.

With the one time initialization in place there are these preliminaries.

```C
cql_noexport void init_gen_sql_callbacks(gen_sql_callbacks *_Nullable callbacks);
```

Use `init_gen_sql_callbacks` to fill in your callback structure with the normal defaults.  This give you normal echo for SQL by default.
To get full echo, `NULL` callback may be used.  And of course other options are possible.

Finally,

```C
cql_noexport void gen_set_output_buffer(struct charbuf *_Nonnull buffer);
```

Use this before the call to `gen_<something>_with_callbacks` to redirect the output into a growable character buffer of your choice.

The buffers can then be written where they are needed.  Maybe into a C string literal for compiler output, or into a comment, or
just right back to stdout.

There are a few simplified versions of this sequence like this one:

```C
cql_noexport void gen_stmt_list_to_stdout(ast_node *_Nullable ast);
```

This does `NULL` callbacks and emits directly to stdout with no extra steps.  The extra wiring is done for you.

### Generator Internals

The generator has to be able to walk the entire tree and emit plain text, and in many areas the tree is very flexible
so we want a simple dynamic dispatch mechanism that can call the right formatting function from anyplace in the tree.

It turns out two different signatures are needed to do this properly, one for formatting statements and the other
for expressions.  The difference being that expressions have to concern themselves with the precedence of the various
operators so that parentheses can be correctly re-inserted into the output.

To do this there are two symbol tables that map from a AST node type string to a formatting function.  They are initialized
with a series of statements similar to these:

#### Generating Expressions

```C
cql_noexport void gen_init() {
  gen_stmts = symtab_new();
  gen_exprs = symtab_new();

  STMT_INIT(if_stmt);
  ...

  EXPR_INIT(mul, gen_binary, "*", EXPR_PRI_MUL);
  EXPR_INIT(div, gen_binary, "/", EXPR_PRI_MUL);
  EXPR_INIT(mod, gen_binary, "%", EXPR_PRI_MUL);
  EXPR_INIT(add, gen_binary, "+", EXPR_PRI_ADD);
  EXPR_INIT(sub, gen_binary, "-", EXPR_PRI_ADD);
  EXPR_INIT(not, gen_unary, "NOT ", EXPR_PRI_NOT);
  EXPR_INIT(tilde, gen_unary, "~", EXPR_PRI_TILDE);
  ...
}
```

These statements populate the symbol tables.
* For statements,  the entry maps `if_stmt` to the function `gen_if_stmt`
* For expressions, the entry maps `mul` to `gen_binary` including the metadata "*" and `EXPR_PRI_MUL`

As you can see (nearly) all binary operators are handled identically as are all unary operators.
Let's look at those two in detail.

```C
static void gen_binary(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {

  // We add parens if our priority is less than the parent prioirty
  // meaning something like this:
  // * we're a + node, our parent is a * node
  // * we need parens because the tree specifies that the + happens before the *
  //
  // Also, grouping of equal operators is left to right
  // so for so if our right child is the same precendence as us
  // that means there were parens there in the original expression
  // e.g.  3+(4-7);
  // effectively it's like we're one binding strength higher for our right child
  // so we call it with pri_new + 1.  If it's equal to us it must emit parens

  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" %s ", op);
  gen_expr(ast->right, pri_new + 1);
  if (pri_new < pri) gen_printf(")");
}
```

The convention gives us:
* `ast` : pointer to the current AST node
* `op` : the text of the operator (`CSTR` is simply `const char *`)
* `pri` : the binding strength of the node above us
* `pri_new` : the binding strength of this node (the new node)

So generically, if our binding strength is weaker than the context we are contained in,
then we must wrap the current node in parentheses to preserve order of operations. See
the comment for more details.

With parens taken care of, we emit the left expression, the operator, and the right expression.

And as you can see below, unary operators are much the same.

```C
static void gen_unary(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  if (pri_new < pri) gen_printf("(");
  gen_printf("%s", op);
  gen_expr(ast->left, pri_new);
  if (pri_new < pri) gen_printf(")");
}
```

There are special case formatters for some of the postfix operators and other cases that are special
like `CASE... WHEN... THEN... ELSE... END` but they operate on the same principles down to the leaf nodes.

#### Generating Statements

With no binding strenght to worry about, statement processing is quite a bit simpler.

Here's the code for the `IF` statement mentioned above.

```C
static void gen_if_stmt(ast_node *ast) {
  Contract(is_ast_if_stmt(ast));
  EXTRACT_NOTNULL(cond_action, ast->left);
  EXTRACT_NOTNULL(if_alt, ast->right);
  EXTRACT(elseif, if_alt->left);
  EXTRACT_NAMED(elsenode, else, if_alt->right);

  gen_printf("IF ");
  gen_cond_action(cond_action);

  if (elseif) {
    gen_elseif_list(elseif);
  }

  if (elsenode) {
    gen_printf("ELSE\n");
    EXTRACT(stmt_list, elsenode->left);
    gen_stmt_list(stmt_list);
  }

  gen_printf("END IF");
}
```

There is a general boiler plate sort of recursive form to all of these, they follow the same basic shape.
These patterns are designed to make it impossible to walk the tree incorrectly, so if the tree shape changes
because of a grammar change, you get immediate concrete failures were the tree walk has to change.  Since
there are test cases to cover every tree shape you can always be sure you have it exactly right if the
macros do not force assertion failures.

* use `Contract` to assert that the node we are given is the type we expect
* use `EXTRACT` macros (detailed below) to get the tree parts you want starting from your root
* use `gen_printf` to emit the constant pieces of the statement
* use recursion to print sub fragments (like the IF condition in this case)
* test the tree fragments where optional peices are present, emit them as needed

It might be instructive to include `gen_cond_action`, it is entirely unremarkable

```C
static void gen_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);

  gen_root_expr(ast->left);
  gen_printf(" THEN\n");
  gen_stmt_list(stmt_list);
}
```

A `cond_action` node has an expression on the left and a statement list on the right it can appear
in the base `IF x THEN y` part of the `IF` or as `ELSE IF x THEN y`.  Either case is formatted the same.

#### Extraction Macros

These macros are used by all the parts of CQL that walk the AST.  They are designed to make it impossible
for you to get the tree shape wrong without immediately failing.  We do not ever want to walk off
the tree in some exotic way and then continue to several levels of recursion before things go wrong.  CQL
locks this down by checking the node type at every step -- any problems are found immediately exactly
at the extraction site and can be quickly corrected.  Again 100% coverage of all the tree shapes makes
this rock solid, so CQL never compromises on 100% code coverage.  The standard macros all appear in this
example:

* `EXTRACT_NOTNULL(cond_action, ast->left);`
  * read ast->left, assert that it is of type `cond_action`, it must not be NULL
  * declare a local variable named `cond_action` to hold the result
* `EXTRACT_NOTNULL(if_alt, ast->right);`
  * read ast->right, assert that it is of type `if_alt`, it must not be NULL
  * declare a local variable named `if_alt` to hold the result
* `EXTRACT(elseif, if_alt->left);`
  * read `if_alt->left`, assert that it is either NULL or else of type `elseif`
  * declare a variable named `elseif` to hold the result
* `EXTRACT_NAMED(elsenode, else, if_alt->right);`
  * read `if_alt->right`, assert that it is either NULL or else of type `else`
  * declare a variable named `elsenode` to hold the result
  * note that we can't use a variable named `else` because `else` is a keyword in C

Other options:

  * `EXTRACT_NAMED_NOTNULL` : like the `NAMED` variant
  * `EXTRACT_ANY` : if the tree type is not known (e.g. expr->left could be any expression type)
  * `EXTRACT_ANY_NOTNULL` : as above but not optional
  * `EXTRACT_NUM_TYPE` : extracts the num_type field from a numeric AST node

The `ANY` variants are usually redispatched with something like `gen_expr` that uses the name table again and that will check thet type or
else they are checked with ad hoc logic immediately if it's perhaps one of two or three variations.  In all cases the idea is to force
a failure very quickly.  `gen_root_expr()` for instance in the `if_cond` example will fail immediately if the node is not an expression type.

Because of the clear use of `EXTRACT`, the `gen_` family of functions are often the best/fastest way to understand the shape of the AST.
You can dump a few sample and look at the `gen_` function and quickly see exactly what the options are authoritatively.  As a result
it's very normal to paste the extraction code from a `gen_` into a new/needed semantic analysis or code-generation functions which we
will come to in later sections.

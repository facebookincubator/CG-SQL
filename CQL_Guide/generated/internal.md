<!--- @generated -->
## Part 1: Lexing, Parsing, and the AST
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

The following is a summary of the implementation theory of the CQL compiler.  This is
an adjuct to the Guide proper, which describes the language, and to a lesser extent
the code that the compiler generates.

The actual code is heavily commented, so it's better to read the code to see the details
of how any particular operation happens rather than try to guess from the language specification
or from this overview.  However, some things, like general principles, really are nowhere
(or everywhere) in the codebase and it's important to understand how things hang together.

If you choose to go on adventures in the source code, especially if you aren't already familiar
with compilers and how they are typically built, this document is a good place to start.

## General Structure

The CQL compiler uses a very standard lex+yacc parser, though to be more precise it's flex+bison.
The grammar is a large subset of the SQLite dialect of SQL augmented with control flow and compiler
directives.  As a consequence, it's a useful asset in-and-of-itself. If you're looking for an
economical SQL grammar, you could do a lot worse than start with the one CQL uses.  The grammar is
of course in the usual `.y` format that bison consumes but it's also extracted into more readable
versions for use in the railroad diagram and the Guide documentation.  Any of those sources would be
a good starting place for a modest SQL project in need of a grammar.

### Lexical Analysis

Inside of `cql.l` you'll find the formal defintion of all the tokens.  These of course correspond to the
various tokens needed to parse the SQL language, plus a few more of the CQL control flow extensions.
There's no need to discuss the approximately 150 such tokens, but the following points are of general interest:

* the lexer expects plain text files, and all the tokens are defined in plain ASCII only, however
  * the presence of UTF8 characters in places where any text is legal (such as string literals) should just work
* all of the tokens are case-insensitive
  * this means only vanilla ASCII insensitivity, no attempt is made to understand more complex UNICODE code-points
* multi-word tokens typically are defined with an expression like this:  `IS[ \t]+NOT[ \t]+FALSE/[^A-Z_]`
  * in most cases, to avoid ambiguity, and to get order of operations correct, the entire word sequence is one token
  * only spaces and tabs are allowed between the words
  * the token ends on non-identifier characters, so the text "X IS NOT FALSEY" must become the tokens { `X`, `IS_NOT`, `FALSEY` } and not { `X`, `IS_NOT_FALSE`, `Y` }
    * the second option is actually the longest token, so without the trailing qualifier it would be preferred
    * hence, where a continuation is possible, the trailing context must be specified in multi=word tokens
    * a quick reading shows this isn't done completely consistently and that should be fixed...
* there is special processing needed to lex `/* ... */` comments correctly
* there are token types for each of the sorts of literals that can be encountered
  * special care is taken to keep the literals in string form so that no precision is lost
  * integer literals are compared against 0x7fffffff and if greater they automatically become long literals even if they are not marked with the trailing `L` as in `1L`
  * string literals include the quotation marks in the token text which distinguishes them from identifiers, they are otherwise encoded similarly
* the character class `[-+&~|^/%*(),.;!<>:=]` produces single character tokens for operators, other non-matching single characters (e.g. `'$'` produce an error)
* line directives `^#\ [0-9]+\ \"[^"]*\".*` get special processing so that pre-processed input does not lose file and line number fidelity

### Parsing and the Abstract Syntax Tree

Inside of `cql.y` you will find the token declarations, precedence rules, and all of the productions in the overall grammar. The grammar processing
does as little as possible in that stage to create an abstract syntax tree (AST). The AST itself is a simple binary tree; where nodes might require more than just
left and right children to specify the syntax fully, additional nodes are used in the tree shape rather than introduce n-ary nodes.  This means the tree is
sometimes bigger, but generally not very much bigger.  The benefit of this choice is that the AST can always be walked generically as a binary tree, so if you need
to find all the `table_factor` nodes it is easy to do so without having to worry about how every kind of node expands.  If new node types come along
the generic walkers can go through those new nodes as well.  All of the grammar productions simply make one or more AST nodes and link them together so that in the
end there is a single root for the entire program in a binary tree.

There are 4 kinds of AST nodes, they all begin with the following five fields, these represent the AST "base type" if you like.

```C
  const char *_Nonnull type;
  struct sem_node *_Nullable sem;
  struct ast_node *_Nullable parent;
  int32_t lineno;
  const char *_Nonnull filename;
```

* `type` : a string literal that uniquely identifies the node type
  * the string literal is compared for identity (it's an exact pointer match) you don't `strcmp` types
* `sem` : begins as `NULL` this is where the semantic type goes once semantic processing happens
* `parent` : the parent node in the AST (not often used but sometimes indispensible)
* `lineno` : the line number of the file that had the text that led to this AST (useful for errors)
* `filename` : the name of the file that had the text that led to this AST (useful for errors)
  * this string is durable, should not be mutated, and is shared between MANY nodes

#### The Generic Binary AST node `ast_node`

```C
typedef struct ast_node {
  ... the common fields
  struct ast_node *_Nullable left;
  struct ast_node *_Nullable right;
} ast_node;
```

This node gives the tree its shape, this is is how all the expression operators and statments get encoded.  An example shows this more clearly:

```
SET X := 1 + 3;

  {assign}
  | {name X}
  | {add}
    | {int 1}
    | {int 3}
```

In the above "assign" and "add" are the generic nodes.  Note that this node type can be a leaf but usually is not.  The other types
are always leaves.

Note that in the above output, the node `type` was directly printed (because it's a meaningful name).  Likewise, the type needs no decoding
when viewing the AST in a debugger.  Simply printing the node with something like `p *ast` in lldb will show you
all the node fields and the type in a human readable fashion.

#### The Grammar Code Node `int_ast_node`

```C
typedef struct int_ast_node {
  ... the common fields
  int64_t value;
} int_ast_node;
```

This kind of node holds an integer that quantifies some kind of choice in the grammar.  Note that this does NOT hold numeric literals (see below).
The file `ast.h` includes many `#define` constants for this purpose such as:

```C
define JOIN_INNER 1
define JOIN_CROSS 2
define JOIN_LEFT_OUTER 3
define JOIN_RIGHT_OUTER 4
define JOIN_LEFT 5
define JOIN_RIGHT 6
```

The integer for this fragment will be one of those defined values.  It can be a bitmask, or an enumeration.  In this statement:

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

The `{int 3}` above is an `int_ast_node` and it corresponds to `JOIN_LEFT_OUTER`.

This node type is always a leaf.

#### The String Node `str_ast_node`

```C
typedef struct str_ast_node {
  ... the common fields
  const char *_Nullable value;
  bool_t cstr_literal;
} str_ast_node;
```

This node type holds:
 * string literals
 * blob literals
 * identifiers

* `value` : the text of the string
* `cstr_literal` : true if the string was specified using "C" syntax (see below)

CQL supports C style string literals with C style escapes such as `"foo\n"`.  These are normalized into the SQL version of the same literal
so that SQLite will see a literal it understands.  However, if the origin of the string was the C string form (i.e. like `"foo"` rather than `'bar'`)
then the `cstr_literal` boolean flag will be set.  When echoing the program back as plain text, the C string will be converted back to the C form
for display to a user. But when providing the string to Sqlite, it's in SQL format.

Identifiers can be distinguised from string literals because the quotation marks (always `''`) are still in the string.

This node type is always a leaf.

#### The Number Node `num_ast_node`

```C
typedef struct num_ast_node {
  ... the common fields
  int32_t num_type;
  const char *_Nullable value;
} num_ast_node;
```

* `num_type` : the kind of numeric
* `value` : the text of the number

All numerics are stored as strings so that there is no loss of precision.  This is important because it is entirely possible that
the CQL compiler is built with a different floating point library, than the target system, or different integer sizes.  As a result
CQL does not evaluate anything outside of an explicit `const(...)` expression.  This policy avoids integer overflows at compile time or loss
of floating point precesion. Constants in the text of the output are emitted byte-for-byte as they appeared in the source code.

This node type is always a leaf.

### Examples

#### Example 1: A LET statement and expression

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

#### Example 2: An IF/ELSE construct

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

And it was normalized into what you see as part of the output.  We'll talk about this output echoing in coming sections.
As you can see, the compiler can be used as a SQL normalizer/beautifier.

#### Example 3: A SELECT statement

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

As you can see the trees rapidly get more complex.  The `SELECT` statement has many optional pieces and
so the AST actually has places in its skeleton where these could go but are absent (e.g. `GROUP BY`,
`HAVING`, `ORDER BY`, and `OFFSET` are all missing).

The shape of the AST is largely self evident from the above, but you can easily cross check it against
what's in `cql.y` for details and then look at `gen_sql.c` for decoding tips (discussed below).

The compiler can produce these diagrams in 'dot' format which makes pretty pictures, but the reality is that
for non-trivial examples those pictures are so large as to be unreadable whereas the simple text format
remains readable even up to several hundred lines of output. The text is also readily searchable, and diffable.
The test suites for semantic analysis do pattern matching on the text of the AST to verify correctness.

We'll discuss semantic analysis in [Part 2](https://cgsql.dev/cql-guide/int02).

### AST definitions

`ast.h` defines all the tree types mentioned above. There are helper methods to create AST nodes
with type safety.  It includes helper functions for the various leaf types mentioned above
but also for the various "normal" types.  These are specified using the AST macros `AST`, `AST1`, and `AST0`.

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

The first set of features that were built (after parsing) provided the ability to echo back the parse tree as SQL again.
This all happens in `gen_sql.c`. Since this code has to be able to echo back any tree, it often has the best
and simplest examples of how to crack the AST for any particular type of node you might be interested in.

There are several reasons why we might want to echo the SQL, but the inescapable one is this: any hunk of
SQL that appears as part of a CQL program (i.e. DDL/DML rather than control flow like `IF`/`WHILE`) has to go
to SQLite and SQLite expects that code to be plain text.  So the AST must be reformatted as plain text that is
exactly equivalent to the original input.  The process of parsing removes extra white space and parentheses, so
to get something that looks reasonable some standard formatting (including indenting) is applied to the output text.
This has the effect of normalizing the input and potentially beautifying it as well (especially if it was
poorly formatted initially).

To see these features you need only run cql with no arguments, by default it reads `stdin`, makes the AST, and
then emits the normalized, formatted text. If there are no syntax errors, the input and the output should be
equivalent.

Standard formatting is essential, but CQL also has a number of extra demands.

CQL includes a lot of versioning directives like `@create(...)` `@delete(...)` and so forth.  SQLite should
never see these things when the DDL for SQLite is emitted.  But when echoing the input they should be included.
Additionally, any local or global variables in a SQL statement should be replaced with `?` in the text
that goes to SQLite and then followed up with binding instructions.  We'll cover the binding more in the
section code generation, but importantly this also has to significantly alter the output.
As a result the standard formatter includes extensive configurably to get these various results.

### Configuring the Output with Callbacks and Flags

Some of these features, like variable binding, require a callback to the formatter's client.  The client
gets a notification, along with a few control variables, and it can then decide exactly what goes in the output
The control structure is `struct gen_sql_callbacks`, and it is described below.  This structure includes the various
callbacks (all of which are optional) and each callback gets a 'context' pointer of its choice.  The context pointer
is some arbitrary `void *` value that you provide, which will be given to your function along with the AST pointer
relevant to the call.  The callback also gets the current output buffer so it can choose to emit something (like '?')
into the stream.

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

The coarsest control is provided by the generation mode.  It is one of these values:

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

The actual callbacks structure is optional, if it is `NULL` then a full echo of the AST with no changes will
be produced.  Otherwise the callbacks and flags alter the behavior of the echoer somewhat.

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
* `convert_hex` : if true, hex constants are converted to decimal; used when emitting JSON because JSON doesn't understand hex constants
* `minify_casts` : minification converts casts like `CAST(NULL AS TEXT)` to just `NULL` -- the former is only useful for type information, SQLite does need to see it
* `minify_aliases` : unused column aliases as in `select foo.x as some_really_long_alias` can be removed from the output when targetting SQLite to save space

### Invoking the Generator

There are several generation functions but they all follow a similar pattern, the differences are essentially what fragment of the AST
they expect to begin on.  We'll just cover one here.

```C
cql_noexport void gen_statement_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
```

This has the typical signature for all these generators:

* `ast` : the part of the tree to print
* `_callbacks` : the optional callbacks described above

To use these you'll need to these functions as well.

```C
cql_noexport void gen_init(void);
cql_noexport void gen_cleanup(void);
```

You'll want to call `gen_init()` one time before doing any generation.  That sets up the necessary tables.
When you're done use `gen_cleanup()` to release any memory that was allocated in setup.
You don't have to do the cleanup step if the process is going to exit anyway, however, because of the amalgam
options, `cql_main()` assumes it might be called again and so it tidies things up rather than risk leaking.

With the one time initialization in place there are these preliminaries:

```C
cql_noexport void init_gen_sql_callbacks(gen_sql_callbacks *_Nullable callbacks);
```

Use `init_gen_sql_callbacks` to fill in your callback structure with the normal defaults.  This give you normal echo for SQL by default.
To get a full echo, a `NULL` callback may be used.  And of course other options are possible.

Finally,

```C
cql_noexport void gen_set_output_buffer(struct charbuf *_Nonnull buffer);
```

Use this before the call to `gen_<something>_with_callbacks` to redirect the output into a growable character buffer of your choice.

The buffers can then be written where they are needed.  Maybe further processed into a C string literal for compiler output, or into
a C style comment, or just right back to stdout.

There are a few simplified versions of this sequence like this one:

```C
cql_noexport void gen_stmt_list_to_stdout(ast_node *_Nullable ast);
```

This uses `NULL` for the callbacks and emits directly to stdout with no extra steps.  The extra wiring is done for you.

### Generator Internals

The generator has to be able to walk the entire tree and emit plain text, and in many areas the tree is very flexible
so we want a simple dynamic dispatch mechanism that can call the right formatting function from anywhere in the tree.

It turns out two different signatures are needed to do this properly, one for formatting statements and the other
for expressions -- the difference being that expressions have to concern themselves with the precedence of the various
operators so that parentheses can be correctly (re)inserted into the output.

To do this there are two symbol tables that map from an AST node type string to a formatting function.  They are initialized
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
* For statements, the entry maps `if_stmt` to the function `gen_if_stmt`
* For expressions, the entry maps `mul` to `gen_binary` including the metadata `"*"` and `EXPR_PRI_MUL`

As you can see, nearly all binary operators are handled identically as are all unary operators.
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

So generically, if the binding strength of the current opereator `pri_new` is weaker than the context it is contained in `pri`,
then parentheses are required to preserve order of operations. See the comment for more details.

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

With no binding strength to worry about, statement processing is quite a bit simpler.

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
These patterns are designed to make it impossible to walk the tree incorrectly. If the tree shape changes
because of a grammar change, you get immediate concrete failures were the tree walk has to change.  Since
there are test cases to cover every tree shape you can always be sure you have it exactly right if the
macros do not force assertion failures.

The steps were:

* use `Contract` to assert that the node we are given is the type we expect
* use `EXTRACT` macros (detailed below) to get the tree parts you want starting from your root
* use `gen_printf` to emit the constant pieces of the statement
* use recursion to print sub fragments (like the IF condition in this case)
* test the tree fragments where optional peices are present, emit them as needed

It might be instructive to include `gen_cond_action`, it is entirely unremarkable:

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
locks this down by checking the node type at every step -- any problems are found immediately, exactly
at the extraction site, and can be quickly corrected.  Again 100% coverage of all the tree shapes makes
this rock solid, so CQL never compromises on 100% code coverage.  The most common macros all appear in this
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
  * `EXTRACT_ANY` : if the tree type is not known (e.g. `expr->left` could be any expression type)
  * `EXTRACT_ANY_NOTNULL` : as above but not optional
  * `EXTRACT_NUM_TYPE` : extracts the num_type field from a numeric AST node

The `ANY` variants are usually re-dispatched with something like `gen_expr` that uses the name table again (and that will check the type) or
else extracted value is checked with ad hoc logic immediately after extraction if it's perhaps one of two or three variations.
In all cases the idea is to force a failure very quickly.  `gen_root_expr()` for instance in the `if_cond` example will fail immediately
if the node it gets is not an expression type.

Because of the clear use of `EXTRACT`, the `gen_` family of functions are often the best/fastest way to understand the shape of the AST.
You can dump a few sample and look at the `gen_` function and quickly see exactly what the options are authoritatively.  As a result
it's very normal to paste the extraction code from a `gen_` function into a new/needed semantic analysis or code-generation functions.


## Part 2: Semantic Analysis
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 2 continues with a discussion of the essentials of the semantic analysis pass of the CQL compiler.
As in the previous sections, the goal here is not to go over every single rule but rather to give
a sense of how semantic analysis happens in general -- the core strategies and implementation choices --
so that when reading the code you have an idea how smaller pieces fit into the whole. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## Semantic Analysis

The overall goal of the semantic analysis pass is to verify that a correct program has been submitted
to the compiler. The compiler does this by "decorating" the AST with semantic information.  This information
is mainly concerned about the "types" of the various things in the program.  A key function of the semantic
analyzer, the primary "weapon" in computing these types if you will, is name resolution.  The semantic analyzer
decides what any given name means in any context and then uses that meaning, which is itself based on the
AST constructs that came before, to compute types and then check those types for errors.

Broadly speaking the errors that can be discovered are of these forms:

  * mentioned names do not exist
    * e.g. using a variable or table or column without declaring it
  * mentioned names are not unique or are ambiguous
    * e.g. table names need to be unique or aliased when joining tables
  * operands are not compatible with each other or with the operation
    * e.g. you can't add a string to a real
    * e.g. you can't do the '%' operation on a real
    * e.g. the expression in a `WHERE` clause must be numeric
    * e.g. the first argument to `printf` must be a string literal.
    * e.g. you can't assign a long value to an integer variable
    * e.g. you can't assign a possibly null result to a not-null variable
  * there are too many or two few operands for an operation
    * e.g. an `INSERT` statement must include sufficiently many columns and no extras
    * e.g. a function or procedure call must have the correct number of operands
  * an operation is happening in a context where it is not allowed
    * e.g. use of aggregate functions in the `WHERE` clause
    * e.g. use of unique SQLite functions outside of a SQL statement

There are several hundred possible errors, no attempt will be made to cover them all here but we will
talk about how errors are created, recorded, and reported.

### Decorated AST examples

Recalling the AST output from Part 1, this is what it looks like with semantic information attached.

```
LET X := 1 + 3;

  {let_stmt}: X: integer notnull variable
  | {name X}: X: integer notnull variable
  | {add}: integer notnull
    | {int 1}: integer notnull
    | {int 3}: integer notnull
```

And here's an example with some structure types

```
SELECT 1 AS x, 3.2 AS y;

  {select_stmt}: select: { x: integer notnull, y: real notnull }
  | {select_core_list}: select: { x: integer notnull, y: real notnull }
  | | {select_core}: select: { x: integer notnull, y: real notnull }
  |   | {select_expr_list_con}: select: { x: integer notnull, y: real notnull }
  |     | {select_expr_list}: select: { x: integer notnull, y: real notnull }
  |     | | {select_expr}: x: integer notnull
  |     | | | {int 1}: integer notnull
  |     | | | {opt_as_alias}
  |     | |   | {name x}
  |     | | {select_expr_list}
  |     |   | {select_expr}: y: real notnull
  |     |     | {dbl 3.2}: real notnull
  |     |     | {opt_as_alias}
  |     |       | {name y}
  |     | {select_from_etc}: ok
  |       | {select_where}
  |         | {select_groupby}
  |           | {select_having}
  | {select_orderby}
    | {select_limit}
      | {select_offset}
```

These can be generated by adding `--sem --print` to the CQL command line along with `--in your_file.sql`.

Keep these shapes in mind as we discuss the various sources of type information.

### The Base Data Structures

First recall that every AST node has this field in it:

```C
struct sem_node *_Nullable sem;
```

This is the pointer to the semantic information for that node. Semantic analysis happens immediately
after parsing and before any of the code-generators run.  Importantly code generators never run
if semantic analysis reported any errors.  Before we get into the shape of the semantic node, we
should start with the fundamental unit of type info `sem_t` which is usually stored in a variable
called `sem_type`.

```C
typedef uint64_t sem_t;
```

The low order bits of a `sem_t` encode the core type and indeed there is an helper macro
to extract the core type from a `sem_t`.

```C
// Strips out all the flag bits and gives you the base/core type.
cql_noexport sem_t core_type_of(sem_t sem_type) {
  return sem_type & SEM_TYPE_CORE;
}
```

The core bits are as follows:

```C
#define SEM_TYPE_NULL 0         // the subtree is a null literal (not just nullable)
#define SEM_TYPE_BOOL 1         // the subtree is a bool
#define SEM_TYPE_INTEGER 2      // the subtree is an integer
#define SEM_TYPE_LONG_INTEGER 3 // the subtree is a long integer
#define SEM_TYPE_REAL 4         // the subtree is a real
#define SEM_TYPE_TEXT 5         // the subtree is a text type
#define SEM_TYPE_BLOB 6         // the subtree is a blob type
#define SEM_TYPE_OBJECT 7       // the subtree is any object type
#define SEM_TYPE_STRUCT 8       // the subtree is a table/view
#define SEM_TYPE_JOIN 9         // the subtree is a join
#define SEM_TYPE_ERROR 10       // marks the subtree as having a problem
#define SEM_TYPE_OK 11          // sentinel for ok but no type info
#define SEM_TYPE_PENDING 12     // sentinel for type calculation in flight
#define SEM_TYPE_REGION 13      // the ast is a schema region
#define SEM_TYPE_CORE 0xff      // bit mask for the core types

#define SEM_TYPE_MAX_UNITARY (SEM_TYPE_OBJECT+1) // the last unitary type
```

These break into a few categories:
* `NULL` to `OBJECT` are the "unitary" types, these are the types that a single simple variable can be
  * a column can be any of these except `OBJECT` or `NULL`
  * the `NULL` type begins only from the `NULL` literal which has no type
  * instance of say a `TEXT` column might have a `NULL` value but they are known to be `TEXT`
* `STRUCT` indicates that the object has many fields, like a table, or a cursor
* `JOIN` indicates that the object is the concatenation of many `STRUCT` types
  * e.g. `T1 inner join T2` is a `JOIN` type with `T1` and `T2` being the parts
  * a `JOIN` can be flattend to `STRUCT` but this is typically not done
  * the type of a `SELECT` statement will be a `STRUCT` representing the columns that were selected which in turn came from the `JOIN` that was the `FROM` clause
* `ERROR` indicates that the subtree had an error, it will have been already reported, this generally cascades up the AST to the root
* `OK` indicates that there is no type information but there was no problem, for instance a correct `IF` statement will resolve to simply `OK` (no error)
* `PENDING` is used sometimes while a type computation is in progress, it doesn't appear in the AST but has its own unique value so as to not conflict with any others
* `REGION` used to identify AST fragments that correspond to schema regions (see the Guide for mor info on regions)
* `CORE` is the mask for the core parts, `0xf` would do the job but for easy reading in the debugger we use `0xff`
  * new core types are not added very often, adding a new one is usually a sign that you are doing something wrong


The core type can be modified by various flags.  The flags in principle can be combined in any way but in practice many combinations make no sense.
for instance, `HAS_DEFAULT` is for table columns and `CREATE_FUNC` is for function declarations. There is no one object that could require both of these.

The full list as of this writing is as follows:

```C
#define SEM_TYPE_NOTNULL               _64(0x0100) // set if and only if null is not possible
#define SEM_TYPE_HAS_DEFAULT           _64(0x0200) // set for table columns with a default
#define SEM_TYPE_AUTOINCREMENT         _64(0x0400) // set for table columns with autoinc
#define SEM_TYPE_VARIABLE              _64(0x0800) // set for variables and parameters
#define SEM_TYPE_IN_PARAMETER          _64(0x1000) // set for in parameters (can mix with below)
#define SEM_TYPE_OUT_PARAMETER         _64(0x2000) // set for out paramters (can mix with above)
#define SEM_TYPE_DML_PROC              _64(0x4000) // set for stored procs that have DML/DDL
#define SEM_TYPE_HAS_SHAPE_STORAGE     _64(0x8000) // set for a cursor with simplified fetch syntax
#define SEM_TYPE_CREATE_FUNC          _64(0x10000) // set for a function that returns a created object +1 ref
#define SEM_TYPE_SELECT_FUNC          _64(0x20000) // set for a sqlite UDF function declaration
#define SEM_TYPE_DELETED              _64(0x40000) // set for columns that are not visible in the current schema version
#define SEM_TYPE_VALIDATED            _64(0x80000) // set if item has already been validated against previous schema
#define SEM_TYPE_USES_OUT            _64(0x100000) // set if proc has a one rowresult using the OUT statement
#define SEM_TYPE_USES_OUT_UNION      _64(0x200000) // set if proc uses the OUT UNION form for multi row result
#define SEM_TYPE_PK                  _64(0x400000) // set if column is a primary key
#define SEM_TYPE_FK                  _64(0x800000) // set if column is a foreign key
#define SEM_TYPE_UK                 _64(0x1000000) // set if column is a unique key
#define SEM_TYPE_VALUE_CURSOR       _64(0x2000000) // set only if SEM_TYPE_HAS_SHAPE_STORAGE is set and the cursor has no statement
#define SEM_TYPE_SENSITIVE          _64(0x4000000) // set if the object is privacy sensitive
#define SEM_TYPE_DEPLOYABLE         _64(0x8000000) // set if the object is a deployable region
#define SEM_TYPE_BOXED             _64(0x10000000) // set if a cursor's lifetime is managed by a box object
#define SEM_TYPE_HAS_CHECK         _64(0x20000000) // set for table column with a "check" clause
#define SEM_TYPE_HAS_COLLATE       _64(0x40000000) // set for table column with a "collate" clause
#define SEM_TYPE_INFERRED_NOTNULL  _64(0x80000000) // set if inferred to not be nonnull (but was originally nullable)
#define SEM_TYPE_VIRTUAL          _64(0x100000000) // set if and only if this is a virtual table
#define SEM_TYPE_HIDDEN_COL       _64(0x200000000) // set if and only if hidden column on a virtual table
#define SEM_TYPE_TVF              _64(0x400000000) // set if and only table node is a table valued function
#define SEM_TYPE_IMPLICIT         _64(0x800000000) // set if and only the variable was declare implicitly (via declare out)
#define SEM_TYPE_CALLS_OUT_UNION _64(0x1000000000) // set if proc calls an out union proc for
```

Going over the meaning of all of the above is again beyond the scope of this document, some of them are very specialized and essentially the validation
requires a bit of storage in the tree to do its job so that storage is provided with a bit.  However two flag bits are especially important and
are computed almost everywhere `sem_t` is used.  These are `SEM_TYPE_NOTNULL` and `SEM_TYPE_SENSITIVE`.

* `SEM_TYPE_NOTNULL` indicates that the marked item is known to be `NOT NULL`, probably because it was declared as such or directly derived from a not null item
  * Typically when two operands are combined both must be marked `NOT NULL` for the result to still be NOT NULL (there are exceptions like `COALESCE`)
  * Values that might be null cannot be assigned to targets that must be not null
* `SEM_TYPE_SENSITIVE` indicates that the marked item is some kind of PII or other sensitive data.
  * Any time a sensitive item is combined with some other piece of data the result is a new sensitive piece of data, there are very few ways to "get rid" of the sensitive bit.  It corresponds to the presence of `@sensitive` in the data type declaration.
  * Values that are sensitive cannot be assigned to targets that are not also marked sensitive

The semantic node `sem_node` carries all the possible semantic info we might need, the `sem_type` holds the flags above and tells us how to interpret it.
There are many fields, we'll talk about some of the most important ones here to give you a sense of how things hang together.

Note that `CSTR` is simply an alias for `const char *`.

```C
typedef struct sem_node {
  sem_t sem_type;                   // core type plus flags
  CSTR name;                        // for named expressions in select columns etc.
  CSTR kind;                        // the Foo in object<Foo>, not a variable or column name
  CSTR error;                       // error text for test output, not used otherwise
  struct sem_struct *sptr;          // encoded struct if any
  struct sem_join *jptr;            // encoded join if any
  int32_t create_version;           // create version if any (really only for tables and columns)
  int32_t delete_version;           // create version if any (really only for tables and columns)
  bool_t recreate;                  // for tables only, true if marked @recreate
  CSTR recreate_group_name;         // for tables only, the name of the recreate gruop if they are in one
  CSTR region;                      // the schema region, if applicable, null means unscoped (default)
  symtab *used_symbols;             // for select statements, we need to know which of the ids in the select list was used if any
  list_item *index_list;            // for tables we need the list of indices that use this table (so we can recreate them together if needed)
  struct eval_node *value;          // for enum values we have to store the evaluated constant value of each member of the enum
} sem_node;
```

* `sem_type` : already discussed above, this tells you how to interpret everything else
* `name` : variables, columns, etc. have a canonical name, when a name case-insenstively resolves, the canonical name is stored here
  * typically you want to emit the canonical variable name (e.g. `FoO` and `fOO` might both resolve to `foo` if that is how it was declared)
* `kind` : in CQL any type can be discriminated as in `declare foo real<meters>`, the kind here is `meters`
  * expressions of the same core type (e.g. `real`) are incompatible if they have a `kind` and the `kind` does not match
  * e.g. you can't assign if you have `bar real<liters>` then `set foo := bar;` is an error even though both are `real`.
* `sptr` : if the item's core type is `SEM_TYPE_STRUCT` then this is populated, see below
* `jptr` : if the item's core type is `SEM_TYPE_JOIN` then this is populated, see below

If the object is a structure type then this is simply an array of names, kinds, and semantic types.  In fact the semantic types will be all be unitary possibly modified by `NOT_NULL` or `SENSITIVE` but none of the other flags apply.  A single `sptr` directly corresponds to the notion of a `shape` in the analyzer.  Shapes come from anything
that looks like a table, such as a cursor, or the result of a `SELECT` statement.

```C
// for tables and views and the result of a select

typedef struct sem_struct {
  CSTR struct_name;               // struct name
  uint32_t count;                 // count of fields
  CSTR *names;                    // field names
  CSTR *kinds;                    // the "kind" text of each column, if any, e.g. integer<foo> foo is the kind
  sem_t *semtypes;                // typecode for each field
} sem_struct;
```

If the object is a join type (such as the parts of the `FROM` clause) then the `jptr` field will be populated. This is nothing more than a named list of struct types.

```C
// for the data type of (parts of) the FROM clause
// sometimes I refer to as a "joinscope"

typedef struct sem_join {
  uint32_t count;                 // count of table/views in the join
  CSTR *names;                    // names of the table/view
  struct sem_struct **tables;     // struct type of each table/view
} sem_join;
```

With these building blocks we can represent the type of anything in the CQL language.

### Initiating Semantic Analysis

The semantic analysis pass runs much the same way as the AST emitter.  In `sem.c` there is the essential function `sem_main`. It suffices
to call `sem_main` on the root of your AST, that is expect to be a `stmt_list` node.

```C
// This method loads up the global symbol tables in either empty state or
// with the appropriate tokens ready to go.  Using our own symbol tables for
// dispatch saves us a lot of if/else string comparison verbosity.
cql_noexport void sem_main(ast_node *ast) {
  // restore all globals and statics we own
  sem_cleanup();
  eval_init();
  ...
}
```

As you can see, `sem_main` begins by reseting all the state.  You can of course do this yourself after calling `sem_main` (when you're done with the results).

`sem_main` sets a variety of useful and public global variables that describe the results of the analysis.  The ones in `sem.h` are part of the contract and
you should feel free to use them in a downstream code-generator.  Other items are internal and should be avoided.  These are typically defined statically in `sem.c`.

The cleanup has this structure:

```C
// This method frees all the global state of the semantic analyzer
cql_noexport void sem_cleanup() {
  eval_cleanup();

  BYTEBUF_CLEANUP(deployable_validations);
  BYTEBUF_CLEANUP(recreate_annotations);
  BYTEBUF_CLEANUP(schema_annotations);

  SYMTAB_CLEANUP(funcs);
  SYMTAB_CLEANUP(globals);
  SYMTAB_CLEANUP(indices);
  SYMTAB_CLEANUP(locals);
  ...

  // these are getting zeroed so that leaksanitizer will not count those objects as reachable from a global root.

  all_ad_hoc_list = NULL;
  all_functions_list = NULL;
    ...
```

`sem_main` of course has to walk the AST and it does so in much the same way as we saw in `gen_sql.c` there are a set of symbol tables
whose key is an ast type and whose value is a function plus arguments to dispatch (effectively a lambda).  The semantic analyzer doesn't
have to think about things like "should I emit parentheses" so the signature of each type of lambda can be quite a bit simpler.  We'll
go over each kind with some examples.

First we have the non-sql statements, these are basic flow control or other things that SQLite will never see directly.

```C
  symtab *syms = non_sql_stmts;

  STMT_INIT(if_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(switch_stmt);
  STMT_INIT(leave_stmt);
  ...
```

Here `STMT_INIT` creates a binding between (e.g.) the AST type `if_stmt` and the function `sem_if_stmt`.  This lets us dispatch any part of the AST
directly.

Next we have the SQL statements.  These get analyzed in the same way as the others, and with functions that have the same signature, however,
if you use one of these it means that procedure that contained this statement must get a database connection in order to run.  This changes
its signature and causes the `SEM_TYPE_DML_PROC` flag bit to be set on it.

```C
  syms = sql_stmts;

  STMT_INIT(create_table_stmt);
  STMT_INIT(drop_table_stmt);
  STMT_INIT(create_index_stmt);
  STMT_INIT(create_view_stmt);
  STMT_INIT(select_stmt);
  STMT_INIT(delete_stmt);
  STMT_INIT(update_stmt);
  STMT_INIT(insert_stmt);
  ...
```

Again `STMT_INIT` creates a binding between (e.g.) the AST type `delete_stmt` and the function `sem_delete_stmt`.  This lets us dispatch any part of the AST
directly.

Next we have expression types, these are set up with `EXPR_INIT`.  Many of the operators require exactly the same kinds of verification so in order to be
able to share the code, the expression analysis functions get an extra argument for the operator in question.  Typically the string of the operator
is only needed so make a good quality error message and validation is otherwise identical.  Here are some samples...

```C
  EXPR_INIT(num, sem_expr_num, "NUM");
  EXPR_INIT(str, sem_expr_str, "STR");
  EXPR_INIT(blob, sem_expr_blob, "BLB");
  EXPR_INIT(null, sem_expr_null, "NULL");
  EXPR_INIT(dot, sem_expr_dot, "DOT");
  EXPR_INIT(const, sem_expr_const, "CONST");
  EXPR_INIT(mul, sem_binary_math, "*");
  EXPR_INIT(mod, sem_binary_integer_math, "%");
  EXPR_INIT(not, sem_unary_logical, "NOT");
  EXPR_INIT(is_true, sem_unary_is_true_or_false, "IS TRUE");
  EXPR_INIT(tilde, sem_unary_integer_math, "~");
  EXPR_INIT(uminus, sem_unary_math, "-");
```

Looking at the very first entry as an example we see that `EXPR_INIT` creates a mapping between the AST type `num` and the analysis function `sem_expr_num` and that function will get the text `"NUM"` as an extra argument.

Let's quickly go over this list as these are the most important analyzers

* `sem_expr_num` : analyzes any numeric constant
* `sem_expr_str` : analyzes any string literal or identifier
* `sem_expr_blob` : analyzes any blob literal
* `sem_expr_null` : analyzes the NULL literal (and nothing else)
* `sem_expr_dot` : analyzes a compound name like `T1.id`
* `sem_expr_const` : analyzes a `const(...)` expression, doing the constant evaluation
* `sem_binary_math` : analyzes any normal binary math operator like '+', '-', '/' etc.
* `sem_binary_integer_math` : analyzes any binary math operator where the operands must be integers like '%' or '|'
* `sem_unary_logical` : analyzes any unary logical operator (the result is a bool), this is really only `NOT`
* `sem_unary_is_true_or_false` : analyzes any of the `IS TRUE`, `IS FALSE`, family of postfix unary operators
* `sem_unary_integer_math` : any unary operator where the operand must be an integer, this is really only `~`
* `sem_unary_math` : any math unary, presently only unary negation (but in the future unary `+` too)

The final plentiful group of associations are for builtin functions, like these.

```C
  FUNC_INIT(changes);
  FUNC_INIT(printf);
  FUNC_INIT(strftime);
  FUNC_INIT(date);
  FUNC_INIT(time);
```

Each of these is dispatched when a function call is found in the tree.  By way of example `FUNC_INIT(changes)` causes the `changes` function to map to `sem__func_changes`.

There are a few other similar macros for more exotic cases but the general pattern should be clear now.  We these in place it's very easy to traverse arbitary statement lists and arbitary expressions with sub expressions and have the correct function be called without having large `switch` blocks all over.

### Semantic Errors

Some of the following examples will show the handling of semantic errors more precisely but the theory is pretty simple.  Each of these analyzers that has
been registered is responsible for putting an appropriate `sem_node` into the AST it was invoked on.  The caller will look to see if that `sem_node`
is of type `SEM_TYPE_ERROR` using `is_error(ast)`.  If it is the caller will mark its own AST as errant using `record_error(ast)` and this continues all
the way up the tree.  The net of this is that wherever you begin semantic analysis you can know if there were any problems by checking for an error at the
top of the tree you provided.

At the point of the initial error, the analyzer is expected to also call `report_error` providing a suitable message.  This will be logged to stderr.  In test mode it is also stored in the AST so that verification steps can confirm that errors were reported at exactly the right place.

If there is no error, then either a suitable `sem_node` is created or else at minimum `record_ok(ast)` is used to place the shared "OK" type on the node.
The OK type indicates no type information but no errors either.  "OK" is helpful for statements that don't involve expressions like `DROP TABLE Foo`.

### The Primitive Types

Perhaps the simplest analysis of all happens at the leaves of the AST.  By way of example, here is the code for expression nodes of type `num`, the numeric literals.

```C
// Expression type for numeric primitives
static void sem_expr_num(ast_node *ast, CSTR cstr) {
  Contract(is_ast_num(ast));
  EXTRACT_NUM_TYPE(num_type, ast);
  switch (num_type) {
  case NUM_BOOL:
    ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL);
    break;

  case NUM_INT:
    ast->sem = new_sem(SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL);
    break;

  case NUM_LONG:
    ast->sem = new_sem(SEM_TYPE_LONG_INTEGER | SEM_TYPE_NOTNULL);
    break;

  default:
    // this is all that's left
    Contract(num_type == NUM_REAL);
    ast->sem = new_sem(SEM_TYPE_REAL | SEM_TYPE_NOTNULL);
    break;
  }
}
```

As you can see the code simply looks at the AST node, confirming first that it is a `num` node.  Then it extract the `num_type`.
`ast->sem` is set to a semantic node of the matching type adding in `SEM_TYPE_NOTNULL` because literals are never null.

The `new_sem` function is used to make an empty `sem_node` with the `sem_type` filled in as specified.  Nothing can go wrong creating a literal so there are no failure modes.

It doesn't get much simpler unless maybe...

```C
// Expression type for constant NULL
static void sem_expr_null(ast_node *ast, CSTR cstr) {
  Contract(is_ast_null(ast));
  // null literal
  ast->sem = new_sem(SEM_TYPE_NULL);
}
```

### Unary Operators

Let's dive in to a simple case -- the unary operators.  There are comparatively few and there isn't much code required to handle them all.


```C
// The only unary math operators are '-' and '~'
// Reference types are not allowed
static void sem_unary_math(ast_node *ast, CSTR op) {
  sem_t core_type, combined_flags;
  if (!sem_unary_prep(ast, &core_type, &combined_flags)) {
    return;
  }

  if (!sem_validate_numeric(ast, core_type, op)) {
    return;
  }

  // The result of unary math promotes to integer.  Basically this converts
  // bool to integer.  Long integer and Real stay as they are.  Text is
  // already ruled out.
  sem_t sem_type_result = sem_combine_types(
      (SEM_TYPE_INTEGER | SEM_TYPE_NOTNULL),
      (core_type | combined_flags));

  ast->sem = new_sem(sem_type_result);
  ast->sem->kind = ast->left->sem->kind;

  // note ast->sem->name is NOT propogated because SQLite doesn't let you refer to
  // the column 'x' in 'select -x' -- the column name is actually '-x' which is useless
  // so we have no name once you apply unary math (unless you use 'as')
  // hence ast->sem->name = ast->left->sem->name is WRONG here and it is not missing on accident
}
```

*Unary Prep*

OK already we need to pause, there is a "prep" pattern here common to most of the shared operators.
This takes care of most of the normal error handling which is the same for all the unary operators.
The same pattern happens in binary operators.  Let's take a look at that function.

```C
// The unary operators all have a similar prep to the binary.  We need
// to visit the left side (it's always the left node even if the operator goes on the right)
// if that's ok then we need the combined_flags and core type.  There is only
// the one.  Returns true if everything is ok.
static bool_t sem_unary_prep(ast_node *ast, sem_t *core_type, sem_t *combined_flags) {
  // op left | left op
  sem_expr(ast->left);

  if (is_error(ast->left)) {
    *core_type = SEM_TYPE_ERROR;
    *combined_flags = 0;
    record_error(ast);
    return false;
  }

  sem_node *sem = ast->left->sem;
  sem_t sem_type = sem->sem_type;

  *core_type = core_type_of(sem_type);
  *combined_flags = not_nullable_flag(sem_type) | sensitive_flag(sem_type);

  Invariant(is_unitary(*core_type));
  return true;
}
```

Reviewing the steps:

* first evaluate the operand, it will be in `ast->left`
* if that's an error, just return the error code from the prep steps
* now that it's not an error, pull the core type out of the operand
* pull the not nullable and sensitive flag bits out of the operand
* return a boolean indicating the presence of an error or not for convenience

This is useful setup for all the unary operators, and as we'll see, the binary operator case has a similar prep step.

*Back to Unary Processing*

Looking at the overall step we see:

* `sem_unary_prep` : verifies that the operand is not an error, and gets its core type and flag bits
* `sem_validate_numeric` : verifies that the operand is a numeric type
* `sem_combine_types` : creates the smallest type that holds two compatible types
   * by combining with integer not null we ensure that the resulting type is at least as big as an integer
   * if the argument is of type `long` or `real` then it will be the bigger type and the resulting type will be `long` or `real`
   * in short, `bool` is promoted to `int`, everything else stays the same
   * `sem_combine_types` also combines the nullability and sensitivity appropriately
* a new `sem_node` is created of the combined type
  * the type kind of the operand is preserved (e.g. the `meters` in `real<meters>`)
  * any column alias or variable name is not preserved, the value is now anonymous

These primitives are designed to combine well, for instance, consider `sem_unary_integer_math`, the steps are

```C
static void sem_unary_integer_math(ast_node *ast, CSTR op) {
  sem_unary_math(ast, op);
  sem_reject_real(ast, op);
}
```

* `sem_unary_math` : do all the above
* `sem_reject_real` : report/record an error if the type is `real` otherwise do nothing

Note that in all cases the `op` string simply gets pushed down to the place where the errors happen.  Let's take a quick look at one of
the sources of errors in the above.  Here's the numeric validator:

```C
static bool_t sem_validate_numeric(ast_node *ast, sem_t core_type, CSTR op) {
  if (is_blob(core_type)) {
    report_error(ast->left, "CQL0045: blob operand not allowed in", op);
    record_error(ast);
    return false;
  }

  if (is_object(core_type)) {
    report_error(ast->left, "CQL0046: object operand not allowed in", op);
    record_error(ast);
    return false;
  }

  if (is_text(core_type)) {
    report_error(ast->left, "CQL0047: string operand not allowed in", op);
    record_error(ast);
    return false;
  }

  return true;
}
```

This is pretty much dumb as rocks.  The non-numeric types are blob, object, and text.  There is a custom error for each type (it could have been shared
but specific error messages seem to help users).  This code doesn't know it's context, but all it needs is `op` to tell it what the numeric-only
operator was and it can produce a nice error message.  It leaves an error in the AST using `record_error` and so its caller can simply `return`
if anything goes wrong.

It's not hard to guess how `sem_reject_real` works:

```C
// Some math operators like << >> & | % only make sense on integers
// This function does the extra checking to ensure they do not get real values
// as arguments.  It's a post-pass after the normal math checks.
static void sem_reject_real(ast_node *ast, CSTR op) {
  if (!is_error(ast)) {
    sem_t core_type = core_type_of(ast->sem->sem_type);
    if (core_type == SEM_TYPE_REAL) {
      report_error(ast, "CQL0001: operands must be an integer type, not real", op);
      record_error(ast);
    }
  }
}
```

* if the AST node isn't already an error, and it is of type real, report an error
* it assumes the type is already known to be numeric
* the pre-check for errors is to avoid double reporting if something has already gone wrong, the core type will be `SEM_TYPE_ERROR`

### Binary Operators

#### Binary Prep

The code pretty much speaks for itself, we'll walk through it

```C
// All the binary ops do the same preparation, they evaluate the left and the
// right expression, then they check those for errors.  Then they need
// the types of those expressions and the combined_flags of the result.  This
// does exactly that for its various callers.  Returns true if all is well.
static bool_t sem_binary_prep(ast_node *ast, sem_t *core_type_left, sem_t *core_type_right, sem_t *combined_flags) {
  EXTRACT_ANY_NOTNULL(left, ast->left);
  EXTRACT_ANY_NOTNULL(right, ast->right);

  // left op right
  sem_expr(left);
  sem_expr(right);

  if (is_error(left) || is_error(right)) {
    record_error(ast);
    *core_type_left = SEM_TYPE_ERROR;
    *core_type_right = SEM_TYPE_ERROR;
    *combined_flags = 0;
    return false;
  }

  *core_type_left = core_type_of(left->sem->sem_type);
  *core_type_right = core_type_of(right->sem->sem_type);
  *combined_flags = combine_flags(left->sem->sem_type, right->sem->sem_type);

  Invariant(is_unitary(*core_type_left));
  Invariant(is_unitary(*core_type_right));

  return true;
}
```

* `sem_expr` is used to recursively walk the left and right nodes
* `is_error` checks if either had errors, if so, simply propogate the error
* extract the left and right core types
* combine nullability and sensitivity flags

These are the standard prep steps, the caller now has the core types of left and right plus combined flags on a silver platter.

#### Example: Is or Is Not

This analyzer is the simplest of all the binaries

```C
// IS and IS NOT are special in that they return a not null boolean.
static void sem_binary_is_or_is_not(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;

  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (!sem_verify_compat(ast, core_type_left, core_type_right, op)) {
    return;
  }

  // the result of is or is not is always a bool and never null
  ast->sem = new_sem(SEM_TYPE_BOOL | SEM_TYPE_NOTNULL | sensitive_flag(combined_flags));
}
```

* `sem_binary_prep` checks for errors in the left or right
* `sem_verify_compat` ensures that left and right operands are type compatible (discussed later)
* the result is always of type `bool not null`

If either step goes wrong the error will naturally propogate.

#### Example: Binary Math

This is the general worker for binary math operations, the most common operations like '+', '-', '*' and so forth.

```C
// For all math operations, we combine the types and yield the type that
// holds both using the helper.  If any text, that's an error.
static void sem_binary_math(ast_node *ast, CSTR op) {
  sem_t core_type_left, core_type_right, combined_flags;
  if (!sem_binary_prep(ast, &core_type_left, &core_type_right, &combined_flags)) {
    return;
  }

  if (error_any_object(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (error_any_blob_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  if (error_any_text_types(ast, core_type_left, core_type_right, op)) {
    return;
  }

  sem_t core_type = sem_combine_types(core_type_left, core_type_right);

  CSTR kind = sem_combine_kinds(ast->right, ast->left->sem->kind);
  if (is_error(ast->right)) {
    record_error(ast);
    return;
  }

  ast->sem = new_sem(core_type | combined_flags);
  ast->sem->kind = kind;
}
```

* `sem_binary_prep` checks for errors on the left or right
* `error_any_object` reports an error if the left or right is of type object
* `error_any_blob_types` reports an error if the left or right is of type blob
* `error_any_text_types` reports an error if the left or right is of type text
* `sem_combine_type` computes the combined type, the smallest numeric type that holds both left and right
  * note the operands are now known to be numeric
  * the three type error checkers give nice right errors about the left or right operand
* `sem_combine_kinds` tries to create a single type `kind` for both operands
  * if their `kind` is incompatible, records an error on the right
* `new_sem` creates a `sem_node` with the combined type, flags, and then the `kind` is set.

At this point it might help to look a few more of the base validators, they are very unremarkable.

#### Example Validator: error_any_object

```C
// If either of the types is an object then produce an error on the ast.
static bool_t error_any_object(ast_node *ast, sem_t core_type_left, sem_t core_type_right, CSTR op) {
  if (is_object(core_type_left)) {
    report_error(ast->left, "CQL0002: left operand cannot be an object in", op);
    record_error(ast);
    return true;
  }

  if (is_object(core_type_right)) {
    report_error(ast->right, "CQL0003: right operand cannot be an object in", op);
    record_error(ast);
    return true;
  }

  return false;
}
```

* `is_object` checks a `sem_type` against `SEM_TYPE_OBJECT`
* if left or right is an object an appropraite error is generated
* there is no strong convention about returning `true` if ok or `true` if error, it's pretty ad hoc
  * this doesn't seem to cause a lot of problems

#### Example Validator: sem_combine_kinds

```C
// Here we check that type<Foo> only combines with type<Foo> or type.
// If there is a current object type, then the next item must match
// If there is no such type, then an object type that arrives becomes the required type
// if they ever don't match record an error
static CSTR sem_combine_kinds_general(ast_node *ast, CSTR kleft, CSTR kright) {
  if (kright) {
    if (kleft) {
      if (strcmp(kleft, kright)) {
        CSTR errmsg = dup_printf("CQL0070: expressions of different kinds can't be mixed: '%s' vs. '%s'", kright, kleft);
        report_error(ast, errmsg, NULL);
        record_error(ast);
      }
    }
    return kright;
  }

  return kleft;
}

// helper to crack the ast nodes first and then call the normal comparisons
static CSTR sem_combine_kinds(ast_node *ast, CSTR kright) {
  CSTR kleft = ast->sem->kind;
  return sem_combine_kinds_general(ast, kleft, kright);
}
```

* `sem_combine_kinds` uses the worker `sem_combine_kinds_general` after extracting the `kind` from the left node
  * usually you already have one `kind` and you want to know if another `kind` is compatible hence this helper
* `sem_combine_kinds_general` applies the general rules
  * NULL + NULL => NULL
  * NULL + x  => x
  * x + NULL => x
  * x + x => x
  * x + y => error if x != y
* this is one of the rare functions that creates a dynamic error message


#### Example Validator : is_numeric_compat

This helper is frequently called several times in the course of other semantic checks.
This one produces no errors, that's up to the caller. Often there is a numeric path
and a non-numeric path so this helper can't create the errors.

```C
cql_noexport bool_t is_numeric_compat(sem_t sem_type) {
  sem_type = core_type_of(sem_type);
  return sem_type >= SEM_TYPE_NULL && sem_type <= SEM_TYPE_REAL;
}
```

It operates by checking the core type for the numeric range.  Note that `NULL` is compatible with numerics
because expressions like `NULL + 2` have meaning in SQL.

#### Example Validator : sem_combine_types

```C
// The second workhorse of semantic analysis, given two types that
// are previously known to be compatible, it returns the smallest type
// that holds both.  If either is nullable the result is nullable.
// Note: in the few cases where that isn't true the normal algorithm for
// nullablity result must be overrided (see coalesce for instance).
static sem_t sem_combine_types(sem_t sem_type_1, sem_t sem_type_2) {
  ...
}
```

This beast is rather lengthy but unremarkable. It follows these rules:
* text is only compatible with text
* object is only compatible with object
* blob is only compatible with blob
* numerics are only compatible with any other numerics
  * NULL promotes the other operand, whatever it is
  * bool promotes to integer
  * integer promotes to long integer
  * long integer promotes to real
  * the combined type is the smallest numeric type according to the promotion rules

Some examples might be helpful:

* 1 + 2L  ->  long
* false + 3.1 -> real
* 2L + 3.1 -> real
* true + 2 -> integer
* 'x' + 1 -> not compatible

Note that `sem_combine_types` assumes the types have already been checked for compatiblitiy and will use `Contract` to enforce
this.  You should be using other helpers like `is_numeric_compat` and friends to ensure the types agree before computing
the combined type.  A list of values that must be compatible with each other (e.g. in `needle IN (haystack)`) can be
checked using `sem_verify_compat` repeatedly.

#### Example Validator : sem_verify_assignment

The `sem_verify_assignment` function is use any time there is something like a logical `assignment` going on.  There are
two important cases:

* `SET x := y` : an actual assignment
* `call foo(x)` : the expression `x` must be "assignable" to the formal variable for the argument of `foo`

This is a lot like normal binary operator compatibility with one extra rule.  The expression must not be a bigger type than the target.
i.e. you cannot assign a `long` to an `integer`.

```C
// This verifies that the types are compatible and that it's ok to assign
// the expression to the variable.  In practice that means:
// * the variable type core type and kind must be compatible with the expression core type and kind
// * the variable must be nullable if the expression is nullable
// * the variable must be sensitive if the assignment is sensitive
// * the variable type must be bigger than the expression type
// Here ast is used only to give a place to put any errors.
static bool_t sem_verify_assignment(ast_node *ast, sem_t sem_type_needed, sem_t sem_type_found, CSTR var_name) {
  if (!sem_verify_compat(ast, sem_type_needed, sem_type_found, var_name)) {
    return false;
  }

  if (!sem_verify_safeassign(ast, sem_type_needed, sem_type_found, var_name)) {
    return false;
  }

  if (is_nullable(sem_type_found) && is_not_nullable(sem_type_needed)) {
    report_error(ast, "CQL0013: cannot assign/copy possibly null expression to not null target", var_name);
    return false;
  }

  if (sensitive_flag(sem_type_found) && !sensitive_flag(sem_type_needed)) {
    report_error(ast, "CQL0014: cannot assign/copy sensitive expression to non-sensitive target", var_name);
    return false;
  }

  return true;
}
```

* `sem_verify_compat` checks for standard type compatibility between the left and the right
* `sem_verify_safeassign` checks that if the types are different the right operand is the smaller
* nullability checks ensure you aren't trying to assign a nullable value to a not null variable
* sensitivity checks ensure you aren't trying to assign a sensitive value to a not sensitive variable

### Simple Statement Validation

With the expression building blocks, most of the usual kind of language statements become quite simple to check
for correctness.  It's probably easiest to illustrate this with an example, let's look at validation for
the `WHILE` statement.

```C
// While semantic analysis is super simple.
//  * the condition must be numeric
//  * the statement list must be error-free
//  * loop_depth is increased allowing the use of interior leave/continue
static void sem_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);

  // WHILE [expr] BEGIN [stmt_list] END

  sem_numeric_expr(expr, ast, "WHILE", SEM_EXPR_CONTEXT_NONE);

  if (is_error(expr)) {
    record_error(ast);
    return;
  }

  if (stmt_list) {
    loop_depth++;

    sem_stmt_list(stmt_list);

    loop_depth--;

    if (is_error(stmt_list)) {
      record_error(ast);
      return;
    }
  }

  record_ok(ast);
}
```

* first we pull out the tree parts we need using `EXTRACT` macros
* the loop expression verified to be numeric
* then the statement list is recursively validated

Note: the while expression is one of the loop constructs which means `LEAVE` and `CONTINUE` are legal inside it, the `loop_depth` global tracks the fact that we are in a loop so that `LEAVE` and `CONTINUE` can report errors if we are not.

It's not hard to imagine that `sem_stmt_list` will basically walk the AST, pulling out statements and dispatching them using the `STMT_INIT` tables previously discussed.  Hence you could land right back in `sem_while_stmt` for a nested `WHILE`.  It's turtles all the way down.

If `SEM_EXPR_CONTEXT_NONE` is a mystery, don't worry it's covered in the next section.

### Expression Contexts

It turns out that in the SQL language some expression types are only valid in some parts of a SQL statement (e.g. aggregate functions can't appear in a `LIMIT` clause) and so there is always a context for any numeric expression.  When a new root expression is being evaluated, it sets the xpression context according to the caller.

The expression contexts are as follows:

```C
#define SEM_EXPR_CONTEXT_NONE           0x0001
#define SEM_EXPR_CONTEXT_SELECT_LIST    0x0002
#define SEM_EXPR_CONTEXT_WHERE          0x0004
#define SEM_EXPR_CONTEXT_ON             0x0008
#define SEM_EXPR_CONTEXT_HAVING         0x0010
#define SEM_EXPR_CONTEXT_ORDER_BY       0x0020
#define SEM_EXPR_CONTEXT_GROUP_BY       0x0040
#define SEM_EXPR_CONTEXT_LIMIT          0x0080
#define SEM_EXPR_CONTEXT_OFFSET         0x0100
#define SEM_EXPR_CONTEXT_TABLE_FUNC     0x0200
#define SEM_EXPR_CONTEXT_WINDOW         0x0400
#define SEM_EXPR_CONTEXT_WINDOW_FILTER  0x0800
#define SEM_EXPR_CONTEXT_CONSTRAINT     0x1000
```

The idea here is simple, you set the context bit that correponds to the current context such as `SEM_EXPR_CONTEXT_WHERE` if the expression is in
the `WHERE` clause.  The validators check this context, in particular anything that is only available in some contexts has a bit-mask of
the context bits where it can be used.  It checks the possibilities against the current context with one bitwise "and" operation. A zero result
indicates that the operation is not valid in the current context.

This bitwise "and" is performed by one of these two helper macros which makes the usage a little clearer

```C
#define CURRENT_EXPR_CONTEXT_IS(x)  (!!(current_expr_context & (x)))
#define CURRENT_EXPR_CONTEXT_IS_NOT(x)  (!(current_expr_context & (x)))
```

#### Expression Context Example : Concat

The concatenation operator `||` is challenging to successfully emulate because it does many different kinds of
numeric conversions automatically.  Rather than perenially getting this wrong, we simply do not support
this operator in a context where SQLite isn't going to be doing the concatenation.  So typically you
use "printf" instead to get your formatting done outside of a SQL state.  The check for this is very simple
and it happens of course in `sem_concat`.

```C
  if (CURRENT_EXPR_CONTEXT_IS(SEM_EXPR_CONTEXT_NONE)) {
    report_error(ast, "CQL0241: CONCAT may only appear in the context of a SQL statement", NULL);
    record_error(ast);
    return;
  }
```

#### Expression Context Example : IN

A slightly more complex example happens processing the `IN` operator.  This operator has two forms,
the form with an expression list, which can be used anywhere, and the form with a select statement.
The latter form can only appear in some sections of SQL and not at all in loose expressions.  For
instance, that form may not appear in the `LIMIT` or `OFFSET` sections of a SQLite statement.

We use this construct to get all the bits we like.

```C
    uint32_t valid = SEM_EXPR_CONTEXT_SELECT_LIST
                    |SEM_EXPR_CONTEXT_WHERE
                    |SEM_EXPR_CONTEXT_ON
                    |SEM_EXPR_CONTEXT_HAVING
                    |SEM_EXPR_CONTEXT_TABLE_FUNC;

    if (CURRENT_EXPR_CONTEXT_IS_NOT(valid)) {
      report_error( ast, "CQL0078: [not] in (select ...) is only allowed inside "
                         "of select lists, where, on, and having clauses", NULL);
      record_error(ast);
      return;
    }
```

If the reader is interested in a simple learning exercise, run down the purpose of `SEM_EXPR_CONTEXT_TABLE_FUNC`, it's simple
but important and it only has one use case so it's easy to find.

### Name Resolution

We've gotten pretty far without talking about the elephant in the room: name resolution.

Like SQL, many statements in CQL have names in positions where the type of the name is completely unambiguous.  For instance
nobody could be confused what sort of symbol `Foo` is in `DROP INDEX Foo;`

These are the easiest name resolutions, and there are a lot in this form.  Let's do an example

#### Example: Index Name Resolution

```C
// This is the basic checking for the drop index statement
// * the index must exist (have been declared) in some version
// * it could be deleted now, that's ok, but the name has to be valid
static void sem_drop_index_stmt(ast_node *ast) {
  Contract(is_ast_drop_index_stmt(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->right);
  EXTRACT_STRING(name, name_ast);

  ast_node *index_ast = find_usable_index(name, name_ast,  "CQL0112: index in drop statement was not declared");
  if (!index_ast) {
    record_error(ast);
    return;
  }

  record_ok(ast);
}
```

Well, this is interesting.  But what's going on with `find_usable_index` what is usable?  Why aren't we just looking up the index
name in some name table and that's it.  Let's have a look at the details:

```C
// returns the node only if it exists and is not restricted by the schema region.
static ast_node *find_usable_index(CSTR name, ast_node *err_target, CSTR msg) {
  ast_node *index_ast = find_index(name);
  if (!index_ast) {
    report_error(err_target, msg, name);
    return NULL;
  }

  if (!sem_validate_object_ast_in_current_region(name, index_ast, err_target, msg)) {
    return NULL;
  }

  return index_ast;
}
```

We haven't discussed schema regions yet but what you need to know about them for now is this:  any piece of schema can be
in a region and these can be nested.  A region may depend on other regions.  If this is done then the region may only
use schema parts that are in its dependencies (transitively).  The point of this is that you might have a rather large
schema and you probably don't want any peice of code to use any piece of schema.  You can use regions to ensure that
the code for feature "X" doesn't try to use schema designed exclusively for feature "Y".

So now `usable` simply means, we can find the name in the symbol table for indices that's `find_index` and it is
accessible by the current region.

If we had used an example requiring a table or a column the same considerations would apply however additionally
tables can be deprecated with `@delete` so we might need additional checks to make sure we're talking about
a table, not a table's tombstone.

In short, these cases just require looking up the entity and verifying that it's accessible in the current context.

#### Flexible Name Resolution

The "hard case" for name resolution is where the name is occuring in an expression.  Such a name might mean a lot
of things.  It could be a global variable, a local variable, an argument, a table column, a field in a cursor, and
others.  The general name resolution goes through several phases looking for the name.  Each phase can either report
an affirmative success or error (in which case the search stops), or it may simply report that the name was not found
but the search should continue.

We can demystify this a bit by looking at the two most common ways to get this done.

```C
// Resolves a (potentially qualified) identifier, writing semantic information
// into `ast` if successful, or reporting and recording an error for `ast` if
// not.
static void sem_resolve_id(ast_node *ast, CSTR name, CSTR scope) {
  Contract(is_id(ast) || is_ast_dot(ast));
  Contract(name);

  // We have no use for `type` and simply throw it away.
  sem_t *type = NULL;
  sem_resolve_id_with_type(ast, name, scope, &type);
}
```

The name resolver works on either a vanilla name (e.g. `x`) or a scoped name (e.g. `T1.x`).  The name and scope are provided.
The `ast` parameter is used only as a place to report errors, there is no further cracking of the ast needed to resolve
the name.  As you can see `sem_resolve_id` just calls the more general function `sem_resolve_id_with_type` and is used
in the most common case where you don't need the sematic type info for the identifier.

Let's move on to the "real" resolver.

```C
// This function is responsible for resolving both unqualified identifiers (ids)
// and qualified identifiers (dots). It performs the following two roles:
//
// - If an optional `ast` is provided, it works the same way most semantic
//   analysis functions work: semantic information will be written into into the
//   ast, errors will be reported to the user, and errors will be recorded in
//   the AST.
//
// - `*typr_ptr` will be set to mutable type (`sem_t *`) in the current
//   environment if the identifier successfully resolves to a type. (There are,
//   unfortunately, a few exceptions in which a type will be successfully
//   resolved and yet `*typr_ptr` will not be set. These include when a cursor
//   in an expression position, when the expression is `rowid` (or similar), and
//   when the id resolves to an enum case. The reason no mutable type is
//   returned in these cases is that a new type is allocated as part of semantic
//   analysis, and there exists no single, stable type in the environment to
//   which a pointer could be returned. This is a limitation of this function,
//   albeit one that's currently not problematic.)
//
//  Resolution is attempted in the order that the `sem_try_resolve_*` functions
//  appear in the `resolver` array. Each takes the same arguments: An (optional)
//  AST, a mandatory name, an optional scope, and mandatory type pointer. If the
//  identifier provided to one of these resolvers is resolved successfully, *or*
//  if the correct resolver was found but there was an error in the program,
//  `SEM_RESOLVE_STOP` is returned and resolution is complete, succesful or not.
//  If a resolver is tried and it determines that it is not the correct resolver
//  for the identifier in question, `SEM_RESOLVE_CONTINUE` is returned and the
//  next resolver is tried.
//
// This function should not be called directly. If one is interested in
// performing semantic analysis, call `sem_resolve_id` (or, if within an
// expression, `sem_resolve_id_expr`). Alternatively, if one wants to get a
// mutable type from the environment, call `find_mutable_type`.
static void sem_resolve_id_with_type(ast_node *ast, CSTR name, CSTR scope, sem_t **type_ptr) {
  Contract(name);
  Contract(type_ptr);

  *type_ptr = NULL;

  sem_resolve (*resolver[])(ast_node *ast, CSTR, CSTR, sem_t **) = {
    sem_try_resolve_arguments,
    sem_try_resolve_column,
    sem_try_resolve_rowid,
    sem_try_resolve_cursor_as_expression,
    sem_try_resolve_variable,
    sem_try_resolve_enum,
    sem_try_resolve_cursor_field,
    sem_try_resolve_arg_bundle,
  };

  for (uint32_t i = 0; i < sizeof(resolver) / sizeof(void *); i++) {
    if (resolver[i](ast, name, scope, type_ptr) == SEM_RESOLVE_STOP) {
      return;
    }
  }

  report_resolve_error(ast, "CQL0069: name not found", name);
  record_resolve_error(ast);
}
```

A lot is well described in the comments, but already we can see the structure.  There are "mini-resolvers"
which are attempted in order

* `sem_try_resolve_arguments` : an argument in the argument list
* `sem_try_resolve_column` : a column name (possibly scoped)
* `sem_try_resolve_rowid` : the virtual rowid column (possibly scoped)
* `sem_try_resolve_cursor_as_expression` : use of a cursor as a boolean, the bool is true if the cursor has data
* `sem_try_resolve_variable` : local or global variables
* `sem_try_resolve_enum` : the constant value of an enum (must be scoped)
* `sem_try_resolve_cursor_field` : a field in a cursor (must be scoped)
* `sem_try_resolve_arg_bundle` : a field in an argument bundle (must be scoped)

These all use this enum to communicate progress:

```C
// All `sem_try_resolve_*` functions return either `SEM_RESOLVE_CONTINUE` to
// indicate that another resolver should be tried, or `SEM_RESOLVE_STOP` to
// indicate that the correct resolver was found. Continuing implies that no
// failure has (yet) occurred, but stopping implies neither success nor failure.
typedef enum {
  SEM_RESOLVE_CONTINUE = 0,
  SEM_RESOLVE_STOP = 1
} sem_resolve;
```

Each of these mini-resolvers will have a series of rules, for example `sem_try_resolve_cursor_field` is going to have to do
something like this:

* if there is no scope it can't be a cursor field, return `CONTINUE`
* if the scope is not the name of a cursor, return `CONTINUE`
* if the name is a field in the cursor, return `STOP` with success
* else, report that the name is not a valid member of the cursor, and return `STOP` with an error

All the mini-resolvers are similarly structured:

* if it's not my case, return `CONTINUE`
* if it is my case return `STOP` with an error as appropriate

Some of the resolvers have quite a few steps but any one resolver is only about a screenful of code
and it does one job.

### Structure types and the notion of Shapes

Earlier we discussed `SEM_TYPE_STRUCT` briefly and recall the basic notion of the `structure` type

```C
// for tables and views and the result of a select

typedef struct sem_struct {
  CSTR struct_name;               // struct name
  uint32_t count;                 // count of fields
  CSTR *names;                    // field names
  CSTR *kinds;                    // the "kind" text of each column, if any, e.g. integer<foo> foo is the kind
  sem_t *semtypes;                // typecode for each field
} sem_struct;
```

The structure is nothing more than an array of names, types and kinds with a count.  But it creates the notion of
what's usually called a "shape" in the codebase. Shapes can be used in a variety of ways as is described in
[Chapter 5](https://cgsql.dev/cql-guide/ch05#reshaping-data-cursor-like-forms) of the CQL Guide. But before we get
into shapes, let's look at an example of how a structure type is created.

The code that follows is the back end of `sem_create_table_stmt`.  At this point the bulk of the analysis is
done and the columns all have their types.  We're about to build the struct type for the table.  Let's see how
that goes.

```C
  // now create a struct type with the correct number of columns
  // the types have already been computed so all we have to do is
  // check for duplicates
  sem_struct *sptr = new_sem_struct(name, cols);

  symtab *columns = symtab_new();

  int32_t col = 0;
  for (ast_node *item = col_key_list; item; item = item->right) {
    Contract(is_ast_col_key_list(item));
    EXTRACT_ANY_NOTNULL(def, item->left);

    if (is_ast_col_def(def)) {
      Invariant(def->sem->name);
      Invariant(col <= cols);  // it's possible that the rest are deleted and we're at the end.

      // columns must be unique, including deleted columns
      if (!symtab_add(columns, def->sem->name, NULL)) {
        EXTRACT_NOTNULL(col_def_type_attrs, def->left);
        EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
        EXTRACT_ANY_NOTNULL(col_def_ast, col_def_name_type->left);

        report_error(col_def_ast, "CQL0142: duplicate column name", def->sem->name);
        record_error(ast);
        symtab_delete(columns);
        goto cleanup;;
      }

      if (is_deleted(def->sem->sem_type)) {
        continue;
      }

      Invariant(col < cols);

      sptr->names[col] = def->sem->name;
      sptr->semtypes[col] = def->sem->sem_type;
      sptr->kinds[col] = def->sem->kind;
      col++;
    }
  }

  symtab_delete(columns);

  Invariant(col == cols);

  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->jptr = sem_join_from_sem_struct(sptr);
  ast->sem->region = current_region;
```

* `new_sem_struct` makes a struct to hold the result, we already have the count of columns and the table name
* `symtab_new` is going to gives us a scratch symbol table so we can check for duplicate column names
* we walk all the items in the table and use `is_ast_col_def(def)` to find the column definitions
* `Invariant(def->sem->name)` claims that we must have already computed the semantic info for the column and it has its name populated
  * this was done earlier
* `symtab_add(columns, def->sem->name, NULL)` adds a nil entry under the column name, if this fails we have a duplicate column
  * in which case we report errors and stop
* `is_deleted` tells us if the column was marked with `@delete` in which case it no longer counts as part of the table
* if all this is good we set the `names`, `kinds`, and `semtypes` from the column definition's semnatic info
* `symtab_delete` cleans up the temporary symbol table
* and finally we create a `sem_node` of type `SEM_TYPE_STRUCT` and fill it in
* `sem_join_from_sem_struct` will be discussed shortly, but it creates a jptr with one table in it

Structure types are often rooted in the shape of a table, but other things can create a structure type.  For instance, the
columns of a view, or any select statement are also described by a structure type and are therefore valid "shapes".  The
return type of a procedure usually comes from a `SELECT` statement so the procedure too can be the source of a shape.
The arguments of a procedure form a shape.  The fields of a cursor form a shape.  You can even have a named subset
of the arguments of a procedure and use them like a shape. All of these things are described by structure types.

#### Shapes and the LIKE construct

There are many cases where you want to be able to capture or re-use something with a known shape and you don't
want to have to fully re-declare the thing.  CQL uses the `LIKE` construct to do these sorts of things.  This is
more fully explained in [Chapter 5](https://cgsql.dev/cql-guide/ch05#reshaping-data-cursor-like-forms) of the Guide,
but for now let's look at two different cases that are of interest.

First, a cursor:

```
DECLARE C CURSOR LIKE Foo;  -- Foo something with a shape
```

So, in the above, Foo could be a table, a view, a procedure with a result, another cursor, and so forth.

How might we do this?  This is the business of `sem_declare_cursor_like_name`

```C
// Here we're going to make a new value cursor using the indicated name for the shape.
// The name has to be "likeable" meaning it refers to some named thing with a shape
// such as a table, a view, another cursor, or a procedure that returns a result set.
// These are the so called "value cursors" in that they have no underlying statement
// that they move through.  You can just load them up with a row and pass them around.
static void sem_declare_cursor_like_name(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_name(ast));
  EXTRACT_ANY_NOTNULL(new_cursor_ast, ast->left);
  EXTRACT_STRING(new_cursor_name, new_cursor_ast);
  EXTRACT_ANY_NOTNULL(like_ast, ast->right);
  EXTRACT_ANY_NOTNULL(name_ast, like_ast->left);
  EXTRACT_STRING(like_name, name_ast);

  // no duplicates allowed
  if (!sem_verify_legal_variable_name(ast, new_cursor_name)) {
    record_error(new_cursor_ast);
    record_error(ast);
    return;
  }

  // must be a valid shape
  ast_node *found_shape = sem_find_likeable_ast(like_ast, LIKEABLE_FOR_VALUES);
  if (!found_shape) {
    record_error(ast);
    return;
  }

  // good to go, make our cursor, with storage.
  name_ast->sem = like_ast->sem = found_shape->sem;
  new_cursor_ast->sem = new_sem(SEM_TYPE_STRUCT | SEM_TYPE_VARIABLE | SEM_TYPE_VALUE_CURSOR | SEM_TYPE_HAS_SHAPE_STORAGE);
  new_cursor_ast->sem->sptr = found_shape->sem->sptr;
  new_cursor_ast->sem->name = new_cursor_name;
  ast->sem = new_cursor_ast->sem;

  symtab_add(current_variables, new_cursor_name, new_cursor_ast);
}
```

* `EXTRACT` the pieces we need from the AST
* `sem_verify_legal_variable_name` makes sure the cursor name is unique and doesn't hide a table name
* `sem_find_likeable_ast` searches for something with a suitable name that has a shape
* re-use the semantic type of what we found in the name node
* make a new `sem_node` for the cursor variable
* use the `sptr` from the discovered shape for the type

Note: `name_ast->sem` isn't actually interesting but it is helpful for debugging and if the AST is printed it
shows the original unmodified semantic type on those nodes.

Briefly `sem_find_likeable_ast` does these steps:

* if the right of the `LIKE` refers to procedure arguments (e.g. C LIKE Foo ARGUMENTS), get the args of the named procedure and use them as a shape
* if the right is a local or global, and its a cursor, use the shape of that cursor for the new cursor
* if the right is the name of an argument bundle, use the shape of the bundle
  * e.g. in `CREATE PROC Foo(p1 like Person, p2 like Person)` `p1` and `p2` are the names of argument bundles shaped like `Person`
* if the right is the name of a table or view, use that shape
* if the right is the name of a procedure with a structure result, use that shape
* if it's none of these, produce an error

This is the primary source of shape reuse.  Let's look at how we might use that.  Suppose we want to write a procedure
that inserts a row into the table `Foo`.  We could certainly list the columns of `Foo` as arguments like this:

```SQL
CREATE PROC InsertIntoFoo(id integer, t text, r real, b blob)
BEGIN
  INSERT INTO Foo(id, t, r, b) VALUES(id, t, r, b);
END;
```

That is going to get a lot less exciting when there are lots of columns and it will be increasingly a maintenance headach.

Compare with

```SQL
CREATE PROC InsertIntoFoo(row LIKE Foo)
BEGIN
  INSERT INTO Foo FROM row;
END;
```

These two things compile into the same code.  The semantic analyzer expands the `(row LIKE Foo)` into
`(row_id integer, row_t text, row_r real, row_b blob)` and then replaces `FROM row` with
`(row_id, row_t, row_r, row_b)`.  In both case it simply looked up the shape using `sem_find_likeable_ast`
and then altered the AST to the canonical pattern.  This kind of "shape sugar" is all over CQL and
greatly increases maintainability while eliminating common errors.  The most common operation is simply
to expland a "shape" into a list of arguments or columns (maybe with or without type).  SQLite doesn't
know any of this shape magic so by the time SQLite sees the code it has to look "normal" -- the shapes
are all resolved.

### Join Types

The last of the type building data structure is the join type.  Recall that we have this shape:

```C
// for the data type of (parts of) the FROM clause
// sometimes I refer to as a "joinscope"

typedef struct sem_join {
  uint32_t count;                 // count of table/views in the join
  CSTR *names;                    // names of the table/view
  struct sem_struct **tables;     // struct type of each table/view
} sem_join;
```

This is an array of named structure types, which is exactly what you get when you do something like this

```SQL
select * from T1 INNER JOIN T2;
```

The result has all of the columns of T1 and all of the columns of T2.  They can be referred to with scoped
names like `T1.x` which means "find the `sptr` corresponding to the name `T1` then within that structure
find the column named `x`".  In general, when we join, we take a `jptr` on the left and concatenate it
with a `jptr` on the right.  And for all this to work we have to start somewhere, usually single tables.
As we saw when we make a table we use `sem_join_from_sem_struct` to make its initial `jptr`.  Let's
have a look at that now.

```C
// Create a base join type from a single struct.
static sem_join *sem_join_from_sem_struct(sem_struct *sptr) {
  sem_join *jptr = new_sem_join(1);
  jptr->names[0] = sptr->struct_name;
  jptr->tables[0] = new_sem_struct_strip_table_flags(sptr);

  return jptr;
}
```

It doesn't get much simpler than the above:

* `new_sem_join` gives us an empty `sem_join` with room for 1 table
* we use the struct name for the name and the table's `sptr` for the shape
* `new_sem_struct_strip_table_flags` copies the table's `sptr` keeping only the essential flags
  * `SEM_TYPE_HIDDEN_COL`
  * `SEM_FLAG_NOTNULL`
  * `SEM_FLAG_SENSITIVE`

The other flags (e.g. `SEM_TYPE_PK`) have no value in doing type checking and were only needed to help validate the table itself.  They
would be harmless but they would also contaminate all of the debug output so they are stripped.  As a result the type of columns
as they appear in say `SELECT` statements is simpler than how they appear in a `CREATE TABLE` statement.

When we need to create a new join type we simply (*) make a new join type that is the concatenation of the left and right parts of the join.

* some join types change the nullability of columns like `LEFT JOIN` so we have to handle that too
* the names of the table in the new joinscope have to be unique so there is also error checking to do
* but basically it's just a concat...

Importantly, we call the thing a "joinscope" because it creates a namespace.  When we are evaluating names inside of the `FROM` clause or
even later in say a `WHERE` clause, the joinscope that we have created so far controls the same of `table.column` combinations you can
use in expressions.  This changes again when there is a subquery, so the joinscopes can be pushed and popped as needed.

By way of example, you'll see these two patterns in the code:

```C
  PUSH_JOIN(from_scope, select_from_etc->sem->jptr);
  error = sem_select_orderby(select_orderby);
  POP_JOIN();
```
* use the `jptr` from the `FROM` clause to put things back in scope for the `ORDER BY` clause


```C
  PUSH_JOIN_BLOCK();
  sem_numeric_expr(ast->left, ast, "LIMIT", SEM_EXPR_CONTEXT_LIMIT);
  POP_JOIN();
```
* `PUSH_JOIN_BLOCK` causes the name search to stop, nothing deeper in the stack is searched
* in this case we do not allow `LIMIT` expressions to see any joinscopes, they may not use any columns.
   * even if the `LIMIT` clause is appearing in a subquery it can't refer to columns in the parent query.

### Schema Regions

We touched briefly on schema regions earlier in this section.  The purpose and language for regions
is described more fully in [Chapter 10](https://cgsql.dev/cql-guide/ch10#schema-regions) of the Guide.
In this section we'll deal with how they are implemented and what you should expect to see in the code.

When a region declaration is found this method is used.

```C
// A schema region is an partitioning of the schema such that it
// only uses objects in the same partition or one of its declared
// dependencies.  One schema region may be upgraded independently
// from any others (assuming they happen such that dependents are done first).
// Here we validate:
//  * the region name is unique
//  * the dependencies (if any) are unique and exist
static void sem_declare_schema_region_stmt(ast_node *ast)  { ... }
```

The general rules are described in the comment, but effectively it accumulates the list of
this regions dependencies.  Sometimes these are called the antecedent regions.  Since
a region can only depend on other regions that have already been declared, it's not possible
to make any cycles.  Regions are declared before you put anything into them.

Pieces of schema or procedures (or anything really) can go into a region by putting it
in a begin/end pair for the name region.

```sql
@begin_schema_region your_region;
  -- your stuff
@end_schema_region;
```

Now whatever happens to be in "your stuff" is:

* limited to seeing only the things that `your_region` is allowed to see and
* contributes its contents to `your_region` thereby limiting how others will be able to use "your stuff"

To see how this happens, let's have a look at `sem_begin_schema_region_stmt`

```C
// Entering a schema region makes all the objects that follow part of that
// region.  It also means that all the contained objects must refer to
// only pieces of schema that are in the same region or a dependent region.
// Here we validate that region we are entering is in fact a valid region
// and that there isn't already a schema region.
static void sem_begin_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_begin_schema_region_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  // @BEGIN_SCHEMA_REGION name

  if (!verify_schema_region_out_of_proc(ast)) {
    record_error(ast);
    return;
  }

  if (current_region) {
    report_error(ast, "CQL0246: schema regions do not nest; end the current region before starting a new one", NULL);
    record_error(ast);
    return;
  }

  ast_node *region = find_region(name);
  if (!region) {
    report_error(ast->left, "CQL0244: unknown schema region", name);
    record_error(ast);
    return;
  }

  // Get the canonical name of the region (case adjusted)
  Contract(is_region(region));
  EXTRACT_STRING(region_name, region->left);

  // we already know we are not in a region
  Invariant(!current_region_image);
  current_region_image = symtab_new();
  sem_accumulate_public_region_image(current_region_image, region_name);

  // this is the one and only text pointer value for this region
  current_region = region_name;
  record_ok(ast);
}
```

We see these basic steps:

* `EXTRACT` the region name
* `verify_schema_region_out_of_proc` makes sure we are out of any procedure (we have to be at the top level)
  * errors if in a procedure
* `current_region` is tested to make sure we are not already in a region (no nesting)
  * errors if already in a region
* `find_region` is used to find the region AST by name
  * errors if the region name isn't valid
* `EXTRACT` is used again to get the canoncial name of the region
  * you could say `@begin_schema_region YoUr_ReGION;` we want the canonical name `your_region` as it was declared
*  `symtab_new` creates a new symbol table `current_region_image`
* `sem_accumulate_public_region_image` populates `current_region_image` by recursively walking this region adding the names of all the regions we find along the way
  * note the regions form a DAG so we might find the same name twice, we can stop if we find a region that is already in the symbol table
* `current_region` is set to the now new current region

Now we're all set up.
* We can use `current_region` to set the region name in the `sem_node` of anything we encounter
* We can use `current_region_image` to quickly see if we are allowed to use any given region (if it's in the table we can use it)

Recall that at the end of `sem_create_table_stmt` we do this:

```C
  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->jptr = sem_join_from_sem_struct(sptr);
  ast->sem->region = current_region;
```

Which should make a lot more sense now.

When doing the symmetric check in `sem_validate_object_ast_in_current_region` we see this pattern:

```C
// Validate whether or not an object is usable with a schema region. The object
// can only be a table, view, trigger or index.
static bool_t sem_validate_object_ast_in_current_region(
  CSTR name,
  ast_node *table_ast,
  ast_node *err_target,
  CSTR msg)
{
  // We're in a non-region therefore no validation needed because non-region stmt
  // can reference schema in any region.
  if (!current_region) {
    return true;
  }

  if (table_ast->sem && table_ast->sem->region) {
    // if we have a current region then the image is always computed!
    Invariant(current_region_image);
    if (!symtab_find(current_region_image, table_ast->sem->region)) {
      // The target region is not accessible from this region
      ...
      return false;
    }
  } else {
    // while in schema region '%s', accessing an object that isn't in a region is invalid
    ...
    return false;
  }

  return true;
}
```

I've elided some of the code here, but only the part that generates error messages.  The essential logic is:

* if we are not in a region we can access anything
* if we're in a region then...
  * the thing we're trying to access must also be in a region, and
  * that region must be in `current_region_image`
  * otherwise, we can't access it

This is enough to do all the region validation we need.

### Results of Semantic Analysis

Semantic Analysis leaves a lot of global state ready for the remaining stages to harvest.  If the state
is defined in `sem.h` then it's ok to harvest.  We'll highlight some of the most important things you
can use.  These are heavily used in the code generators.

```C
cql_data_decl( struct list_item *all_tables_list );
cql_data_decl( struct list_item *all_functions_list );
cql_data_decl( struct list_item *all_views_list );
cql_data_decl( struct list_item *all_indices_list );
cql_data_decl( struct list_item *all_triggers_list );
cql_data_decl( struct list_item *all_regions_list );
cql_data_decl( struct list_item *all_ad_hoc_list );
cql_data_decl( struct list_item *all_select_functions_list );
cql_data_decl( struct list_item *all_enums_list );
```

These linked lists are authoritiative, they let you easily enumerate all the objects of the specified type.  If you
wanted to do some validation of all indices you could simply walk the all indices list.

```C
cql_noexport ast_node *find_proc(CSTR name);
cql_noexport ast_node *find_region(CSTR name);
cql_noexport ast_node *find_func(CSTR name);
cql_noexport ast_node *find_table_or_view_even_deleted(CSTR name);
cql_noexport ast_node *find_enum(CSTR name);
```

These functions give you access to the core name tables (which are still valid!) so that you can look up procedures, functions,
tables, etc. by name.

Finally, information about all the schema annotations is invaluable for building schema upgraders.  These
two buffers hold dense arrays of annotation records as shown below.

```C
cql_data_decl( bytebuf *schema_annotations );
cql_data_decl( bytebuf *recreate_annotations );

typedef struct recreate_annotation {
  CSTR target_name;               // the name of the target
  CSTR group_name;                // group name or "" if no group (not null, safe to sort)
  ast_node *target_ast;           // top level target (table, view, or index)
  ast_node *annotation_ast;       // the actual annotation
  int32_t ordinal;                // when sorting we want to use the original order (reversed actually) within a group
} recreate_annotation;

typedef struct schema_annotation {
  int32_t version;                // the version number (always > 0)
  ast_node *target_ast;           // top level target (table, view, or index)
  CSTR target_name;               // the name of the target
  uint32_t annotation_type;       // one of the codes below for the type of annotation
  ast_node *annotation_ast;       // the actual annotation
  int32_t column_ordinal;         // -1 if not a column
  ast_node *column_ast;           // a particular column if column annotation
} schema_annotation;

// Note: schema annotations are processed in the indicated order: the numbers matter
#define SCHEMA_ANNOTATION_INVALID 0
#define SCHEMA_ANNOTATION_FIRST 1
#define SCHEMA_ANNOTATION_CREATE_TABLE 1
#define SCHEMA_ANNOTATION_CREATE_COLUMN 2
#define SCHEMA_ANNOTATION_DELETE_TRIGGER 3
#define SCHEMA_ANNOTATION_DELETE_VIEW 4
#define SCHEMA_ANNOTATION_DELETE_INDEX 5
#define SCHEMA_ANNOTATION_DELETE_COLUMN 6
#define SCHEMA_ANNOTATION_DELETE_TABLE 7
#define SCHEMA_ANNOTATION_AD_HOC 8
#define SCHEMA_ANNOTATION_LAST 8
```

And of course, each "back end" is provided with the root of the AST so that it can also search
and/or walk the AST in its own manner.  We will see examples of this in later sections.

### Recap

At present, there are nearly 20000 lines in `sem.c` and it would no doubt take more than 20000 lines of text
to explain what they all do, and that would be more imprecise than the source code, and probably less
readable.  `sem.c` includes over 4000 lines of comments, and probably should have more.  While there
is a lot of code there it's very readable and I encourage you to do so to get your answers.

The point of this part of the Internals Guide isn't to fully explain all 400+ error checks in about
as many semantic error checking functions, it's to showcase the key concepts shared by all of them. Things like:

* errors are reported largely in the AST and percolate up
* expressions and statements have general purpose dispatch logic for continuing a statement walk
* EXTRACT macros are used to keep the tree walk on track and correct in the face of changes
* regions are used for visibility
* versioning contributes to visibility
* nullability and sensitivity are tracked throughout using type bits
* type kind is managed by a simple string in the `sem_node` payload
* the three main payloads are
  * `sem_node` for basic info, and
  * `sem_struct` or `sem_join` for the non-unitary types

This isn't everything but it should leave you well armed to begin your own exploration of `sem.c`.


## Part 3: C Code Generation
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



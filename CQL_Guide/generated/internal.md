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
an adjunct to the Guide proper, which describes the language, and to a lesser extent
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

Inside of `cql.l` you'll find the formal definition of all the tokens.  These of course correspond to the
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

This node gives the tree its shape and is how all the expression operators and statements get encoded.  An example shows this more clearly:

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

Identifiers can be distinguished from string literals because the quotation marks (always `''`) are still in the string.

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
of floating point precision. Constants in the text of the output are emitted byte-for-byte as they appeared in the source code.

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
The text above was not the input to the compiler, the compiler was actually given this text:

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

The shape of the AST is largely self-evident from the above, but you can easily cross check it against
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

To see these features, you need only run cql with no arguments. By default, it reads `stdin`, makes the AST, and
then emits the normalized, formatted text. If there are no syntax errors, the input and the output should be
equivalent.

Standard formatting is essential, but CQL also has a number of extra demands.

CQL includes a lot of versioning directives like `@create(...)` `@delete(...)` and so forth.  SQLite should
never see these things when the DDL for SQLite is emitted.  But when echoing the input they should be included.
Additionally, any local or global variables in a SQL statement should be replaced with `?` in the text
that goes to SQLite and then followed up with binding instructions.  We'll cover the binding more in the
section code generation, but importantly this also has to significantly alter the output.
As a result the standard formatter includes extensive configurability to get these various results.

### Configuring the Output with Callbacks and Flags

Some of these features, like variable binding, require a callback to the formatter's client.  The client
gets a notification, along with a few control variables, and it can then decide exactly what goes in the output.
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
  gen_mode_no_annotations // Equivalent to gen_mode_echo without versioning attributes or generic attributes
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
  // this is to allow the variable reference to be noted and replaced with ? in the generated SQL
  gen_sql_callback _Nullable variables_callback;
  void *_Nullable variables_context;

  // Each time a column definition is emitted this callback is invoked, it may choose to
  // suppress that column.  This is used to remove columns that were added in later schema
  // versions from the baseline schema.
  gen_sql_callback _Nullable col_def_callback;
  void *_Nullable col_def_context;

  // This callback is used to explain the * in select * or select T.*
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
  // this flag with the mode flag but it's sufficiently weird that this
  // extra documentation and special handling is probably worth the extra
  // boolean storage.
  bool_t long_to_int_conv;
} gen_sql_callbacks;
```

Each callback can be best understood by reading the source, so we'll avoid
trying to precisely define it here.  But it is helpful to give the gist
of these options.

* `mode` : one of the three enum modes that control overall behavior
* `variables_callback` : invoked when a variable appears in the SQL, the caller can record the specific variable and then use it for binding
* `col_def_callback` : when creating the "baseline" schema you don't want column definitions from later schema to be included, this gives you a chance to suppress them
* `star_callback` : normally the `*` in `select *` or `select T.*` is expanded when emitting for SQLite, this callback does the expansion when appropriate
* `if_not_exists_callback` : when generating DDL for schema upgrade you typically want to force `IF NOT EXISTS` to be added to the schema even if it wasn't present in the declaration; this callback lets you do that
* `convert_hex` : if true, hex constants are converted to decimal; used when emitting JSON because JSON doesn't understand hex constants
* `minify_casts` : minification converts casts like `CAST(NULL AS TEXT)` to just `NULL` -- the former is only useful for type information, SQLite does need to see it
* `minify_aliases` : unused column aliases as in `select foo.x as some_really_long_alias` can be removed from the output when targeting SQLite to save space

### Invoking the Generator

There are several generation functions but they all follow a similar pattern, the differences are essentially what fragment of the AST
they expect to begin on.  We'll just cover one here.

```C
cql_noexport void gen_statement_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
```

This has the typical signature for all these generators:

* `ast` : the part of the tree to print
* `_callbacks` : the optional callbacks described above

To use these you'll need to these functions as well:

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

  // We add parens if our priority is less than the parent priority
  // meaning something like this:
  // * we're a + node, our parent is a * node
  // * we need parens because the tree specifies that the + happens before the *
  //
  // Also, grouping of equal operators is left to right
  // so for so if our right child is the same precedence as us
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

So generically, if the binding strength of the current operator `pri_new` is weaker than the context it is contained in `pri`,
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

There is a general boilerplate sort of recursive form to all of these; they follow the same basic shape.
These patterns are designed to make it impossible to walk the tree incorrectly. If the tree shape changes
because of a grammar change, you get immediate concrete failures where the tree walk has to change.  Since
there are test cases to cover every tree shape you can always be sure you have it exactly right if the
macros do not force assertion failures.

The steps were:

* use `Contract` to assert that the node we are given is the type we expect
* use `EXTRACT` macros (detailed below) to get the tree parts you want starting from your root
* use `gen_printf` to emit the constant pieces of the statement
* use recursion to print sub-fragments (like the IF condition in this case)
* test the tree fragments where optional pieces are present, emit them as needed

It might be instructive to include `gen_cond_action`; it is entirely unremarkable:

```C
static void gen_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);

  gen_root_expr(ast->left);
  gen_printf(" THEN\n");
  gen_stmt_list(stmt_list);
}
```

A `cond_action` node has an expression on the left and a statement list on the right; it can appear
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
else the extracted value is checked with ad hoc logic immediately after extraction if it's perhaps one of two or three variations.
In all cases the idea is to force a failure very quickly.  `gen_root_expr()` for instance in the `if_cond` example will fail immediately
if the node it gets is not an expression type.

Because of the clear use of `EXTRACT`, the `gen_` family of functions are often the best/fastest way to understand the shape of the AST.
You can dump a few samples and look at the `gen_` function and quickly see exactly what the options are authoritatively.  As a result
it's very normal to paste the extraction code from a `gen_` function into a new/needed semantic analysis or code-generation function.


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
so that when reading the code you will have an idea how smaller pieces fit into the whole. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## Semantic Analysis

The overall goal of the semantic analysis pass is to verify that a correct program has been submitted
to the compiler. The compiler does this by "decorating" the AST with semantic information.  This information
is mainly concerned with the "types" of the various things in the program.  A key function of the semantic
analyzer, the primary "weapon" in computing these types if you will, is name resolution.  The semantic analyzer
decides what any given name means in any context and then uses that meaning, which is itself based on the
AST constructs that came before, to compute types and then check those types for errors.

Broadly speaking the errors that can be discovered are of these forms:

  * mentioned names do not exist
    * e.g. using a variable or table or column without declaring it
  * mentioned names are not unique, or are ambiguous
    * e.g. every view must have a unique name
    * e.g. table names need to be unique, or aliased when joining tables
  * operands are not compatible with each other or with the intended operation
    * e.g. you can't add a string to a real
    * e.g. you can't do the `%` operation on a real
    * e.g. the expression in a `WHERE` clause must result in a numeric
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

Recalling the AST output from [Part 1](https://cgsql.dev/cql-guide/int01), this is what that same tree
looks like with semantic information attached.

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
after parsing and before any of the code-generators run.  Importantly, code generators never run
if semantic analysis reported any errors.  Before we get into the shape of the semantic node, we
should start with the fundamental unit of type info `sem_t` which is usually stored in a variable
called `sem_type`.

```C
typedef uint64_t sem_t;
```

The low order bits of a `sem_t` encode the core type and indeed there is an helper function
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
  * the `NULL` type comes only from the `NULL` literal which has no type
  * instances of say a `TEXT` column might have a `NULL` value but they are known to be `TEXT`
* `STRUCT` indicates that the object has many fields, like a table, or a cursor
* `JOIN` indicates that the object is the concatenation of many `STRUCT` types
  * e.g. `T1 inner join T2` is a `JOIN` type with `T1` and `T2` being the parts
  * a `JOIN` could be flattened to `STRUCT`, but this is typically not done
  * the type of a `SELECT` statement will be a `STRUCT` representing the expressions that were selected
  * those expressions in turn used columns from the `JOIN` that was the `FROM` clause
* `ERROR` indicates that the subtree had an error
  * the error will have been already reported
  * the error type generally cascades up the AST to the root
* `OK` indicates that there is no type information but there was no problem
  * e.g. a correct `IF` statement will resolve to simply `OK` (no error)
* `PENDING` is used sometimes while a type computation is in progress
  * this type doesn't appear in the AST, but has its own unique value so as to not conflict with any others
* `REGION` is used to identify AST fragments that correspond to schema regions
   * see [Chapter 10](https://cgsql.dev/cql-guide/ch10) of the Guide for more information on regions
* `CORE` is the mask for the core parts, `0xf` would do the job but for easy reading in the debugger we use `0xff`
  * new core types are not added very often, adding a new one is usually a sign that you are doing something wrong

The core type can be modified by various flags.  The flags in principle can be combined in any way but in practice many combinations make no sense.
For instance, `HAS_DEFAULT` is for table columns and `CREATE_FUNC` is for function declarations. There is no one object that could require both of these.

The full list as of this writing is as follows:

```C
#define SEM_TYPE_NOTNULL               _64(0x0100) // set if and only if null is not possible
#define SEM_TYPE_HAS_DEFAULT           _64(0x0200) // set for table columns with a default
#define SEM_TYPE_AUTOINCREMENT         _64(0x0400) // set for table columns with autoinc
#define SEM_TYPE_VARIABLE              _64(0x0800) // set for variables and parameters
#define SEM_TYPE_IN_PARAMETER          _64(0x1000) // set for in parameters (can mix with below)
#define SEM_TYPE_OUT_PARAMETER         _64(0x2000) // set for out parameters (can mix with above)
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

Note: `_64(x)` expands to either a trailing `L` or a trailing `LL` depending on the bitness of the compiler, whichever yields an `int64_t`.

Going over the meaning of all of the above is again beyond the scope of this document; some of the flags are very specialized and essentially the validation
just requires a bit of storage in the tree to do its job so that storage is provided with a flag.  However two flag bits are especially important and
are computed almost everywhere `sem_t` is used.  These are `SEM_TYPE_NOTNULL` and `SEM_TYPE_SENSITIVE`.

* `SEM_TYPE_NOTNULL` indicates that the marked item is known to be `NOT NULL`, probably because it was declared as such, or directly derived from a not null item
  * Typically when two operands are combined both must be marked `NOT NULL` for the result to still be `NOT NULL` (there are exceptions like `COALESCE`)
  * Values that might be null cannot be assigned to targets that must not be null
* `SEM_TYPE_SENSITIVE` indicates that the marked item is some kind of PII or other sensitive data.
  * Any time a sensitive operand is combined with another operand the resulting type is sensitive
  * There are very few ways to "get rid" of the sensitive bit -- it corresponds to the presence of `@sensitive` in the data type declaration.
  * Values that are sensitive cannot be assigned to targets that are not marked sensitive

The semantic node `sem_node` carries all the possible semantic info we might need, the `sem_type` holds the flags above and tells us how to interpret the rest of the node.
There are many fields, we'll talk about some of the most important ones here to give you a sense of how things hang together.

Note that `CSTR` is simply an alias for `const char *`.  `CSTR` is used extensively in the codebase for brevity.

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
  CSTR recreate_group_name;         // for tables only, the name of the recreate group if they are in one
  CSTR region;                      // the schema region, if applicable, null means unscoped (default)
  symtab *used_symbols;             // for select statements, we need to know which of the ids in the select list was used if any
  list_item *index_list;            // for tables we need the list of indices that use this table (so we can recreate them together if needed)
  struct eval_node *value;          // for enum values we have to store the evaluated constant value of each member of the enum
} sem_node;
```

* `sem_type` : already discussed above, this tells you how to interpret everything else
* `name` : variables, columns, etc. have a canonical name, when a name case-insensitivity resolves, the canonical name is stored here
  * typically later passes emit the canonical variable name everywhere
  * e.g. `FoO` and `fOO` might both resolve to an object declared as `foo`, we always emit `foo` in codegen
* `kind` : in CQL any type can be discriminated as in `declare foo real<meters>`, the kind here is `meters`
  * two expressions of the same core type (e.g. `real`) are incompatible if they have a `kind` and the `kind` does not match
  * e.g. if you have `bar real<liters>` then `set foo := bar;` is an error even though both are `real` because `foo` above is `real<meters>`
* `sptr` : if the item's core type is `SEM_TYPE_STRUCT` then this is populated, see below
* `jptr` : if the item's core type is `SEM_TYPE_JOIN` then this is populated, see below

If the object is a structure type then this is simply an array of names, kinds, and semantic types.  In fact the semantic types will be all be unitary, possibly modified by `NOT_NULL` or `SENSITIVE` but none of the other flags apply.  A single `sptr` directly corresponds to the notion of a "shape" in the analyzer.  Shapes come from anything
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
to call `sem_main` on the root of the AST. That root node is expected to be a `stmt_list` node.

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

As you can see, `sem_main` begins by resetting all the global state.  You can of course do this yourself after calling `sem_main` (when you're done with the results).

`sem_main` sets a variety of useful and public global variables that describe the results of the analysis.  The ones in `sem.h` are part of the contract and
you should feel free to use them in a downstream code-generator.  Other items are internal and should be avoided.
The internal items are typically defined statically in `sem.c`. The essential outputs will be described in the last section of this part.

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
This basically deallocates everything and resets all the globals to `NULL`.

`sem_main` of course has to walk the AST and it does so in much the same way as we saw in `gen_sql.c`. There is a series of symbol tables
whose key is an AST type and whose value is a function plus arguments to dispatch (effectively a lambda).  The semantic analyzer doesn't
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
to its handler directly.

Next we have the SQL statements.  These get analyzed in the same way as the others, and with functions that have the same signature, however,
if you use one of these it means that procedure that contained this statement must get a database connection in order to run.  Use of the database
will require the procedure's signature to change; this is recorded by the setting the `SEM_TYPE_DML_PROC` flag bit to be set on the procedures AST node.

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

Again `STMT_INIT` creates a binding between (e.g.) the AST type `delete_stmt` and the function `sem_delete_stmt` so we can dispatch to the handler.

Next we have expression types, these are set up with `EXPR_INIT`.  Many of the operators require exactly the same kinds of verification, so in order to be
able to share the code, the expression analysis functions get an extra argument for the operator in question.  Typically the string of the operator
is only needed to make a good quality error message with validation being otherwise identical.  Here are some samples...

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

Looking at the very first entry as an example, we see that `EXPR_INIT` creates a mapping between the AST type `num`
and the analysis function `sem_expr_num` and that function will get the text `"NUM"` as an extra argument.
As it happens `sem_expr_num` doesn't need the extra argument, but `sem_binary_math` certainly needs the `"*"`
as that function handles a large number of binary operators.

Let's quickly go over this list as these are the most important analyzers:

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
* `sem_unary_integer_math` : analyzes any unary operator where the operand must be an integer, this is really only `~`
* `sem_unary_math` : analyzes any any math unary operator, presently only negation (but in the future unary `+` too)

The last group of normal associations are for builtin functions, like these:

```C
  FUNC_INIT(changes);
  FUNC_INIT(printf);
  FUNC_INIT(strftime);
  FUNC_INIT(date);
  FUNC_INIT(time);
```

Each of these is dispatched when a function call is found in the tree.  By way of example `FUNC_INIT(changes)`
causes the `changes` function to map to `sem_func_changes` for validation.

There are a few other similar macros for more exotic cases but the general pattern should be clear now.  With these in place
it's very easy to traverse arbitrary statement lists and arbitrary expressions with sub expressions and have the correct function
invoked without having large `switch` blocks all over.

### Semantic Errors

Some of the following examples will show the handling of semantic errors more precisely but the theory is pretty simple.  Each of the analyzers that has
been registered is responsible for putting an appropriate `sem_node` into the AST it is invoked on.  The caller will look to see if that `sem_node`
is of type `SEM_TYPE_ERROR` using `is_error(ast)`.  If it is, the caller will mark its own AST as errant using `record_error(ast)` and this continues all
the way up the tree.  The net of this is that wherever you begin semantic analysis, you can know if there were any problems by checking for an error at the
top of the tree you provided.

At the point of the initial error, the analyzer is expected to also call `report_error` providing a suitable message.  This will be logged to `stderr`.
In test mode it is also stored in the AST so that verification steps can confirm that errors were reported at exactly the right place.

If there are no errors, then a suitable `sem_node` is created for the resulting type or else, at minimum, `record_ok(ast)` is used to place the shared "OK" type on the node.
The "OK" type indicates no type information, but no errors either.  "OK" is helpful for statements that don't involve expressions like `DROP TABLE Foo`.

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

As you can see the code simply looks at the AST node, confirming first that it is a `num` node.  Then it extracts the `num_type`.
Then `ast->sem` is set to a semantic node of the matching type adding in `SEM_TYPE_NOTNULL` because literals are never null.

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

It's hard to get simpler than doing semantic analysis of the `NULL` literal.  Its code should be clear with no further explanation needed.

### Unary Operators

Let's dive in to a simple case that does require some analysis -- the unary operators.  There are comparatively few and there isn't much code required to handle them all.

Here's the code for the unary math operators:

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

  // note ast->sem->name is NOT propagated because SQLite doesn't let you refer to
  // the column 'x' in 'select -x' -- the column name is actually '-x' which is useless
  // so we have no name once you apply unary math (unless you use 'as')
  // hence ast->sem->name = ast->left->sem->name is WRONG here and it is not missing on accident
}
```

*Unary Prep*

OK already we need to pause, there is a "prep" pattern here common to most of the shared operators that we should discuss.
The prep step takes care of most of the normal error handling which is the same for all the unary operators
and same pattern happens in binary operators.  Let's take a look at `sem_unary_prep`.

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

* first we analyze the operand, it will be in `ast->left`
* if that's an error, we just return the error code from the prep steps
* now that it's not an error, we pull the core type out of the operand
* then we pull the not nullable and sensitive flag bits out of the operand
* finally return a boolean indicating the presence of an error (or not) for convenience

This is useful setup for all the unary operators, and as we'll see, the binary operators have a similar prep step.

*Back to Unary Processing*

Looking at the overall steps we see:

* `sem_unary_prep` : verifies that the operand is not an error, and gets its core type and flag bits
* `sem_validate_numeric` : verifies that the operand is a numeric type
  * recall these are the math unary operators, so the operand must be numeric
* `sem_combine_types` : creates the smallest type that holds two compatible types
   * by combining with "integer not null" we ensure that the resulting type is at least as big as an integer
   * if the argument is of type `long` or `real` then it will be the bigger type and the resulting type will be `long` or `real` as appropriate
   * in short, `bool` is promoted to `int`, everything else stays the same
   * `sem_combine_types` also combines the nullability and sensitivity appropriately
* a new `sem_node` of the combined type is created
  * the type "kind" of the operand is preserved (e.g. the `meters` in `real<meters>`)
  * any column alias or variable name is not preserved, the value is now anonymous

These primitives are designed to combine well, for instance, consider `sem_unary_integer_math`

```C
static void sem_unary_integer_math(ast_node *ast, CSTR op) {
  sem_unary_math(ast, op);
  sem_reject_real(ast, op);
}
```

The steps are:

* `sem_unary_math` : do the sequence we just discussed
* `sem_reject_real` : report/record an error if the result type is `real` otherwise do nothing

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

That function is pretty much dumb as rocks.  The non-numeric types are blob, object, and text.  There is a custom error for each type (it could have been shared
but specific error messages seem to help users).  This code doesn't know its context, but all it needs is `op` to tell it what the numeric-only
operator was and it can produce a nice error message.  It leaves an error in the AST using `record_error`, its caller can then simply `return`
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

* if the AST node isn't already an error, and the node is of type "real", report an error
* it assumes the type is already known to be numeric
* the pre-check for errors is to avoid double reporting; if something has already gone wrong, the core type will be `SEM_TYPE_ERROR`
  * no new error recording is needed in that case, obviously an error was already recorded

### Binary Operators

#### Binary Prep

With the knowledge we have so far, this code pretty much speaks for itself, but we'll walk through it.

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

* `sem_expr` : used to recursively walk the left and right nodes
* `is_error` : checks if either side had errors, if so, simply propagate the error
* extract the left and right core types
* combine nullability and sensitivity flags

And that's it!  These are the standard prep steps for all binary operators. With this done,
the caller has the core types of the left and right operands plus combined flags on a silver platter
and one check is needed to detect if anything went wrong.

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

* `sem_binary_prep` : checks for errors in the left or right
* `sem_verify_compat` : ensures that left and right operands are type compatible (discussed later)
* the result is always of type `bool not null`

If either step goes wrong the error will naturally propagate.

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

Let's have a look at those steps:

* `sem_binary_prep` : checks for errors on the left or right
* `error_any_object` : reports an error if the left or right is of type object
* `error_any_blob_types` : reports an error if the left or right is of type blob
* `error_any_text_types` : reports an error if the left or right is of type text
* `sem_combine_type` : computes the combined type, the smallest numeric type that holds both left and right
  * note the operands are now known to be numeric
  * the three type error checkers give nice tight errors about the left or right operand
* `sem_combine_kinds` : tries to create a single type `kind` for both operands
  * if their `kind` is incompatible, records an error on the right
* `new_sem` : creates a `sem_node` with the combined type, flags, and then the `kind` is set.

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

* `is_object` : checks a `sem_type` against `SEM_TYPE_OBJECT`
  * if the left or right child is an object an appropriate error is generated
* there is no strong convention about returning `true` if ok, or `true` if error, it's pretty ad hoc
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

* `sem_combine_kinds` : uses the worker `sem_combine_kinds_general` after extracting the `kind` from the left node
  * usually you already have one `kind` and you want to know if another `kind` is compatible hence this helper
* `sem_combine_kinds_general` : applies the general rules for "kind" strings:
  * NULL + NULL => NULL
  * NULL + x  => x
  * x + NULL => x
  * x + x => x
  * x + y => error (if x != y)
* this is one of the rare functions that creates a dynamic error message


#### Example Validator : is_numeric_compat

This helper is frequently called several times in the course of other semantic checks.
This one produces no errors, that's up to the caller. Often there is a numeric path
and a non-numeric path so this helper can't create the errors as it doesn't yet know
if anything bad has happened.  Most of the `is_something` functions are the same way.

```C
cql_noexport bool_t is_numeric_compat(sem_t sem_type) {
  sem_type = core_type_of(sem_type);
  return sem_type >= SEM_TYPE_NULL && sem_type <= SEM_TYPE_REAL;
}
```

`is_numeric_compat` operates by checking the core type for the numeric range.
Note that `NULL` is compatible with numerics because expressions like `NULL + 2`
have meaning in SQL.  The type of that expression is nullable integer and
the result is `NULL`.

#### Example Validator : sem_combine_types

```C
// The second workhorse of semantic analysis, given two types that
// are previously known to be compatible, it returns the smallest type
// that holds both.  If either is nullable the result is nullable.
// Note: in the few cases where that isn't true the normal algorithm for
// nullablity result must be overridden (see coalesce for instance).
static sem_t sem_combine_types(sem_t sem_type_1, sem_t sem_type_2) {
  ... too much code ... summary below
}
```

This beast is rather lengthy but unremarkable. It follows these rules:
* text is only compatible with text
* object is only compatible with object
* blob is only compatible with blob
* numerics are only compatible with other numerics and NULL
  * NULL promotes the other operand, whatever it is (might still be NULL)
  * bool promotes to integer if needed
  * integer promotes to long integer if needed
  * long integer promotes to real if needed
  * the combined type is the smallest numeric type that holds left and right according to the above

Some examples might be helpful:

* 1 + 2L  ->  long
* false + 3.1 -> real
* 2L + 3.1 -> real
* true + 2 -> integer
* 'x' + 1 -> not compatible

Note that `sem_combine_types` assumes the types have already been checked for compatibility and will use `Contract` to enforce
this.  You should be using other helpers like `is_numeric_compat` and friends to ensure the types agree before computing
the combined type.  A list of values that must be compatible with each other (e.g. in `needle IN (haystack)`) can be
checked using `sem_verify_compat` repeatedly.

#### Example Validator : sem_verify_assignment

The `sem_verify_assignment` function is used any time there is something like a logical `assignment` going on.  There are
two important cases:

* `SET x := y` : an actual assignment
* `call foo(x)` : the expression `x` must be "assignable" to the formal variable for the argument of `foo`

This is a lot like normal binary operator compatibility with one extra rule: the source expression must
not be a bigger type than the target.  e.g. you cannot assign a `long` to an `integer`, nor pass a long
expression to a function that has an integer parameter.

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

* `sem_verify_compat` : checks for standard type compatibility between the left and the right
* `sem_verify_safeassign` : checks that if the types are different the right operand is the smaller
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

* `EXTRACT*` : pulls out the tree parts we need
* `sem_numeric_expr` : verifies the loop expression is numeric
* `sem_stmt_list` : recursively validates the body of the loop

Note: the while expression is one of the loop constructs which means `LEAVE` and `CONTINUE` are legal inside it.
The `loop_depth` global tracks the fact that we are in a loop so that analysis for `LEAVE` and `CONTINUE` can report errors if we are not.

It's not hard to imagine that `sem_stmt_list` will basically walk the AST, pulling out statements and dispatching them using the `STMT_INIT` tables previously discussed.
You might land right back in `sem_while_stmt` for a nested `WHILE` -- it's turtles all the way down.

If `SEM_EXPR_CONTEXT_NONE` is a mystery, don't worry it's covered in the next section.

### Expression Contexts

It turns out that in the SQL language some expression types are only valid in some parts of a SQL statement (e.g. aggregate functions can't appear in a `LIMIT` clause) and so there is always a context for any numeric expression.  When a new root expression is being evaluated, it sets the expression context per the caller's specification.

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

The idea here is simple, when calling a root expression, the analyzer provides the context value that has the bit that corresponds to the current context.
For instance, the expression being validated in is the `WHERE` clause, the code will provide `SEM_EXPR_CONTEXT_WHERE`.
The inner validators check this context, in particular anything that is only available in some contexts has a bit-mask of that is the union
of the context bits where it can be used.  The validator can check those possibilities against the current context with one bitwise "and" operation.
A zero result indicates that the operation is not valid in the current context.

This bitwise "and" is performed by one of these two helper macros which makes the usage a little clearer:

```C
#define CURRENT_EXPR_CONTEXT_IS(x)  (!!(current_expr_context & (x)))
#define CURRENT_EXPR_CONTEXT_IS_NOT(x)  (!(current_expr_context & (x)))
```

#### Expression Context Example : Concat

The concatenation operator `||` is challenging to successfully emulate because it does many different kinds of
numeric to string conversions automatically.  Rather than perennially getting this wrong, we simply do not support
this operator in a context where SQLite isn't going to be doing the concatenation.  So typically users
use "printf" instead to get formatting done outside of a SQL context.  The check for invalid use of `||` is very simple
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
the form with an expression list, which can be used anywhere, and the form with a `SELECT` statement.
The latter form can only appear in some sections of SQL, and not at all in loose expressions.  For
instance, that form may not appear in the `LIMIT` or `OFFSET` sections of a SQLite statement.

We use this construct to do the validation:

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

If the reader is interested in a simple learning exercise, run down the purpose of `SEM_EXPR_CONTEXT_TABLE_FUNC` -- it's simple,
but important, and it only has one use case so it's easy to find.

### Name Resolution

We've gotten pretty far without talking about the elephant in the room: name resolution.

Like SQL, many statements in CQL have names in positions where the type of the name is completely unambiguous.  For instance
nobody could be confused what sort of symbol `Foo` is in `DROP INDEX Foo`.

This type, with a clear name category, are the easiest name resolutions, and there are a lot in this form.  Let's do an example.

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

Well, this is interesting.  But what's going on with `find_usable_index`? What is usable?  Why aren't we just looking up the index
name in some name table? Let's have a look at the details of `find_usable_index`.

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

We haven't discussed schema regions yet but what you need to know about them for now is this:
* any object can be in a region.
* a region may depend on other regions

If an object is in a region, then it may only use schema parts that are in
the same region, or the region's dependencies (transitively).

The point of this is that you might have a rather large schema and you probably don't want any piece
of code to use any piece of schema.  You can use regions to ensure that the code for feature "X" doesn't
try to use schema designed exclusively for feature "Y".  That "X" code probably has no business even
knowing of the existence of "Y" schema.

So now `usable` simply means this:
* `find_index` can find the name in the symbol table for indices
* the found index is accessible in the current region

If we had used an example that was looking up a table name, the same region considerations would apply,
however, additionally tables can be deprecated with `@delete` so there would be additional checks to make
sure we're talking about a live table and not a table's tombstone.

In short, these simple cases just require looking up the entity and verifying that it's accessible in the current context.

#### Flexible Name Resolution

The "hard case" for name resolution is where the name is occurring in an expression.  Such a name can refer to
all manner of things. It could be a global variable, a local variable, an argument, a table column, a field in a cursor,
and others.  The general name resolver goes through several phases looking for the name.  Each phase can either report
an affirmative success or error (in which case the search stops), or it may simply report that the name was not found
but the search should continue.

We can demystify this a bit by looking at the most common way to get name resolution done.

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
The `ast` parameter is used only as a place to report errors, there is no further cracking of the AST needed to resolve
the name.  As you can see `sem_resolve_id` just calls the more general function `sem_resolve_id_with_type` and is used
in the most common case where you don't need to be able to mutate the sematic type info for the identifier.  That's the 99% case.

So let's move on to the "real" resolver.

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
//  `SEM_RESOLVE_STOP` is returned and resolution is complete, successful or not.
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

This function is well described in its own comments.  We can easily see the "mini-resolvers"
which attempt to find the name in order:

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

* if there is no scope, it can't be a cursor field, return `CONTINUE`
* if the scope is not the name of a cursor, return `CONTINUE`
* if the name is a field in the cursor, return `STOP` with success
* else, report that the name is not a valid member of the cursor, and return `STOP` with an error

All the mini-resolvers are similarly structured, generically:

* if it's not my case, return `CONTINUE`
* if it is my case return `STOP` (maybe with an error)

Some of the mini-resolvers have quite a few steps, but any one mini-resolver is only about a screenful of code
and it does one job.

### Structure types and the notion of Shapes

Earlier we discussed `SEM_TYPE_STRUCT` briefly. Recall the basic notion of the `structure` type:

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

* `new_sem_struct` : makes a struct to hold the result, we already have the count of columns and the table name
* `symtab_new` : is going to gives us a scratch symbol table so we can check for duplicate column names
* we walk all the items in the table and use `is_ast_col_def(def)` to find the column definitions
* `Invariant(def->sem->name)` : claims that we must have already computed the semantic info for the column and it has its name populated
  * this was done earlier
* `symtab_add(columns, def->sem->name, NULL)` : adds a nil entry under the column name, if this fails we have a duplicate column
  * in which case we report errors and stop
* `is_deleted` : tells us if the column was marked with `@delete` in which case it no longer counts as part of the table
* if all this is good we set the `names`, `kinds`, and `semtypes` from the column definition's semnatic info
* `symtab_delete` : cleans up the temporary symbol table
* `new_sem` : creates a `sem_node` of type `SEM_TYPE_STRUCT` which is filled in
  * `sem_join_from_sem_struct` will be discussed shortly, but it creates a `jptr` with one table in it

Structure types often come from the shape of a table, but other things can create a structure type.  For instance, the
columns of a view, or any select statement, are also described by a structure type and are therefore valid "shapes".  The
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

* `EXTRACT` : gets the pieces we need from the AST
* `sem_verify_legal_variable_name` : makes sure the cursor name is unique and doesn't hide a table name
* `sem_find_likeable_ast` : searches for something with a suitable name that has a shape
* we populate the name node with the  semantic type that we found
* `new_sem` : makes a new `sem_node` for the cursor variable with `SEM_TYPE_STRUCT`
  * set the `sptr` field using the discovered shape

Note: `name_ast->sem` isn't actually used for anything but it is helpful for debugging. If the AST is printed it
shows the original unmodified semantic type which can be helpful.

Briefly `sem_find_likeable_ast` does these steps:

* if the right of the `LIKE` refers to procedure arguments (e.g. C LIKE Foo ARGUMENTS), get the args of the named procedure and use them as a shape
* if the right is a local or global, and its a cursor, use the shape of that cursor for the new cursor
* if the right is the name of an argument bundle, use the shape of the bundle
  * e.g. in `CREATE PROC Foo(p1 like Person, p2 like Person)` `p1` and `p2` are the names of argument bundles shaped like `Person`
* if the right is the name of a table or view, use that shape
* if the right is the name of a procedure with a structure result, use that shape
* if it's none of these, produce an error

This is the primary source of shape reuse.  Let's look at how we might use that.

Suppose we want to write a procedure that inserts a row into the table `Foo`, we could certainly list the columns of `Foo` as arguments like this:

```SQL
CREATE PROC InsertIntoFoo(id integer, t text, r real, b blob)
BEGIN
  INSERT INTO Foo(id, t, r, b) VALUES(id, t, r, b);
END;
```

But that approach is going to get a lot less exciting when there are lots of columns and it will be increasingly a maintenance headache.

Compare that with the following:

```SQL
CREATE PROC InsertIntoFoo(row LIKE Foo)
BEGIN
  INSERT INTO Foo FROM row;
END;
```

Those two versions of `InsertIntoFoo` compile into the same code.  The semantic analyzer expands the `(row LIKE Foo)` into
`(row_id integer, row_t text, row_r real, row_b blob)` and then replaces `FROM row` with
`(row_id, row_t, row_r, row_b)`.  In both case it simply looked up the shape using `sem_find_likeable_ast`
and then altered the AST to the canonical pattern.  This kind of "shape sugar" is all over CQL and
greatly increases maintainability while eliminating common errors.  The most common operation is simply
to expland a "shape" into a list of arguments or columns (maybe with or without type names).  SQLite doesn't
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

This is an array of named structure types, which is exactly what you get when you do something like this:

```SQL
select * from T1 INNER JOIN T2;
```

The result has all of the columns of `T1` and all of the columns of `T2`.  They can be referred to with scoped
names like `T1.x` which means "find the `sptr` corresponding to the name `T1` then within that structure
find the column named `x`".  In general, when we join, we take a `jptr` on the left and concatenate it
with a `jptr` on the right.  For all this to work we have to start somewhere, usually single tables.

As we saw when we make a table we use `sem_join_from_sem_struct` to make its initial `jptr`.  Let's
have a look at that now:

```C
// Create a base join type from a single struct.
static sem_join *sem_join_from_sem_struct(sem_struct *sptr) {
  sem_join *jptr = new_sem_join(1);
  jptr->names[0] = sptr->struct_name;
  jptr->tables[0] = new_sem_struct_strip_table_flags(sptr);

  return jptr;
}
```

It doesn't get much simpler than the above, here are the steps briefly:

* `new_sem_join` : gives us an empty `sem_join` with room for 1 table
* we use the struct name for the name and the table's `sptr` for the shape
* `new_sem_struct_strip_table_flags` : copies the table's `sptr` keeping only the essential flags
  * `SEM_TYPE_HIDDEN_COL`
  * `SEM_FLAG_NOTNULL`
  * `SEM_FLAG_SENSITIVE`

The other flags (e.g. `SEM_TYPE_PK`) have no value in doing type checking and were only needed to help validate the table itself.
Those extra flags would be harmless but they would also contaminate all of the debug output, so they are stripped.  As a result
the type of columns as they appear in say `SELECT` statements is simpler than how they appear in a `CREATE TABLE` statement.

When we need to create a new join type we simply (*) make a new `sem_join` that is the concatenation of the left and right sides of the join
operation.

* some join types change the nullability of columns like `LEFT JOIN`, so we have to handle that too
* the names of the tables in the new concatenated joinscope have to be unambiguous so there is also error checking to do
* but basically it's just a concat...

Importantly, we call the thing a "joinscope" because it creates a namespace.  When we are evaluating names inside of the `FROM` clause or
even later in say a `WHERE` clause, the joinscope that we have created so far controls the `table.column` combinations you can
use in expressions.  This changes again when there is a subquery, so the joinscopes can be pushed and popped as needed.

By way of example, you'll see these two patterns in the code:

```C
  PUSH_JOIN(from_scope, select_from_etc->sem->jptr);
  error = sem_select_orderby(select_orderby);
  POP_JOIN();
```

* `PUSH_JOIN` : use the `jptr` from the `FROM` clause to put things back in scope for the `ORDER BY` clause

```C
  PUSH_JOIN_BLOCK();
  sem_numeric_expr(ast->left, ast, "LIMIT", SEM_EXPR_CONTEXT_LIMIT);
  POP_JOIN();
```
* `PUSH_JOIN_BLOCK` : causes the name search to stop, nothing deeper in the stack is searched
* in this case we do not allow `LIMIT` expressions to see any joinscopes, they may not use any columns
   * even if the `LIMIT` clause is appearing in a subquery it can't refer to columns in the parent query.

### Schema Regions

We touched briefly on schema regions earlier in this section.  The purpose and language for regions
is described more fully in [Chapter 10](https://cgsql.dev/cql-guide/ch10#schema-regions) of the Guide.
In this section we'll deal with how they are implemented and what you should expect to see in the code.

When a region declaration is found this method is used:

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
the declared region's dependencies.  Sometimes these are called the antecedent regions.  Since
a region can only depend on regions that have already been declared, it's not possible
to make any cycles. Regions are declared before you put anything into them.

Pieces of schema or procedures (or anything really) can go into a region by putting that code
inside a begin/end pair for the named region.  Like so:

```sql
@begin_schema_region your_region;
  -- your stuff
@end_schema_region;
```

Now whatever happens to be in "your stuff" is:

* limited to seeing only the things that `your_region` is allowed to see, and
* contributes its contents to `your_region` thereby limiting how others will be able to use "your stuff"

To see how this happens, let's have a look at `sem_begin_schema_region_stmt`.

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

* `EXTRACT` : gets the region name
* `verify_schema_region_out_of_proc` : makes sure we are out of any procedure (we have to be at the top level)
  * errors if in a procedure
* `current_region` : is tested to make sure we are not already in a region (no nesting)
  * errors if already in a region
* `find_region` : is used to find the region AST by name
  * errors if the region name isn't valid
* `EXTRACT` : is used again to get the canonical name of the region
  * you could write `@begin_schema_region YoUr_ReGION;` but we want the canonical name `your_region`, as it was declared
*  `symtab_new` : creates a new symbol table `current_region_image`
* `sem_accumulate_public_region_image` : populates `current_region_image` by recursively walking this region adding the names of all the regions we find along the way
  * note the regions form a DAG so we might find the same name twice, we can stop if we find a region that is already in the image symbol table
* `current_region` : set it to the now new current region

Now we're all set up.

* We can use `current_region` to set the `region` in the `sem_node` of anything we encounter
* We can use `current_region_image` to quickly see if we are allowed to use any given region
  * if it's in the symbol table we can use it

Recall that at the end of `sem_create_table_stmt` we do this:

```C
  ast->sem = new_sem(SEM_TYPE_STRUCT);
  ast->sem->sptr = sptr;
  ast->sem->jptr = sem_join_from_sem_struct(sptr);
  ast->sem->region = current_region;
```

That should make a lot more sense now.

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
is defined in `sem.h` then it's ok to harvest.  Here we'll highlight some of the most important things you
can use in later passes.  These are heavily used in the code generators.

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

These linked lists are authoritiative, they let you easily enumerate all the objects of the specified type.  For
instance, if you wanted to do some validation of all indices you could simply walk `all_indices_list`.

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
* `EXTRACT` macros are used to keep the tree walk on track and correct in the face of changes
* regions are used for visibility
* versioning contributes to visibility
* nullability and sensitivity are tracked throughout using type bits
* type "kind" is managed by a simple string in the `sem_node` payload
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

```C
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

```C
  DDL_STMT_INIT(drop_table_stmt);
  DDL_STMT_INIT(drop_view_stmt);
  DDL_STMT_INIT(create_table_stmt);
  DDL_STMT_INIT(create_view_stmt);
```
The DDL (Data Definition Language) statements all get the same handling:  The text of the statement
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

```C
  NO_OP_STMT_INIT(declare_enum_stmt);
  NO_OP_STMT_INIT(declare_named_type);
```

Next, the general purpose statement handler.  `STMT_INIT` creates mappings
such as the `if_stmt` AST node mapping to `cg_if_stmt`.

```C
  STMT_INIT(if_stmt);
  STMT_INIT(switch_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(assign);
```

The next group of declarations are the expressions, with precedence and operator specified.
There is a lot of code sharing between AST types as you can see from this sample:

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

```C
void p(void) {
  cql_bool x = 0;

  /* ! is stronger than + */
  x = ! 1 + ! 2;
  x = ! (1 + 2);
}
```

Finally, many built-in functions need special codegen, such as:

```C
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
This economy reflects that fact that most `charbuf` instances are very small.
Of course a `charbuf` could go on the heap if it needs to outlive
the function it appears in, but this is exceedingly rare.

To make sure buffers are consistently closed -- and this is a problem because
there are often a lot of them -- they are allocated with these simple helper
macros:

```C
#define CHARBUF_OPEN(x) \
  int32_t __saved_charbuf_count##x = charbuf_open_count; \
  charbuf x; \
  bopen(&x)

#define CHARBUF_CLOSE(x) \
  bclose(&x); \
  Invariant(__saved_charbuf_count##x == charbuf_open_count)
```

The earlier example would be written more properly:

```C
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
```C
  CHARBUF_OPEN(proc_fwd_ref);
  CHARBUF_OPEN(proc_body);
  CHARBUF_OPEN(proc_locals);
  CHARBUF_OPEN(proc_cleanup);
```

Save the current buffer pointers...
```C
  charbuf *saved_main = cg_main_output;
  charbuf *saved_decls = cg_declarations_output;
  charbuf *saved_scratch = cg_scratch_vars_output;
  charbuf *saved_cleanup = cg_cleanup_output;
  charbuf *saved_fwd_ref = cg_fwd_ref_output;
```

Switch to the new buffers...
```C
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

```C
  x = x + y;
```

And indeed, it does.  Here's some actual output from the compiler:

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

In this new example above, `x` and `y` became nullable variables i.e. the `NOT NULL` was
removed from their declarations -- this makes all the difference in the world.

Let's take a quick look at `cql_nullable_int32` and we'll see the crux of the problem immediately:

```C
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

* `_tmp_n_int_1` : holds the product of x and 5, it's null if `x.is_null` is true
* `_tmp_n_int_2` : holds the product of y and 3, it's null if `y.is_null` is true
* `*result` : holds the answer, it's null if either of `_tmp_n_int_1.is_null`, `_tmp_n_int_2.is_null` is true
   * otherwise it's `_tmp_n_int_1.value + _tmp_n_int_2.value`

So, in general, we need to emit arbitrarily many statements in the course of evaluating even simple looking expressions
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

Symmetrically, `CG_POP_TEMP` closes the `charbuf` variables and restores the stack level.

```C
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

```C
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

```C
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

```C
// Make a temporary buffer for the evaluation results using the canonical naming convention
// burn the stack slot so that any type and numbered temporary that was needed
// won't be re-used until this scope is over.
#define CG_PUSH_EVAL(expr, pri) \
  CHARBUF_OPEN(expr##_is_null); \
  CHARBUF_OPEN(expr##_value); \
  cg_expr(expr, &expr##_is_null, &expr##_value, pri); \
  stack_level++;
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

`CG_POP_EVAL` simply closes the buffers and restores the stack level.

#### Result Variables

When recursion happens in the codegen, a common place that the result will be found is
in a temporary variable i.e. the generated code will use one or more statements to arrange for the correct
answer to be in a variable.  To do this, the codegen needs to first get the name of a
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
and the expression parts always go into `charbuf` variables named `result_var`, `result_var_is_null`, and `result_var_value`
but the scratch variable isn't actually allocated!  However -- we burn the stack_level as though it had been
allocated.

The name of the macro provides a clue: this macro reserves a slot for the result variable, it's used if the codegen might
need a result variable, but it might not.  If/when the result variable is needed, it we can artificially move the stack level
back to the reserved spot, allocate the scratch variable, and then put the stack level back.

The `CG_USE_RESULT_VAR` macro does exactly this operation.

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
to populate the variable.  The variable is the result.

There is a simpler macro that reserves and uses the result variable in one step, it's used frequently.
The "reserve" pattern is only necessary when there are some paths that need a result variable and some
that don't.

```C
// This does reserve and use in one step
#define CG_SETUP_RESULT_VAR(ast, sem_type) \
  CG_RESERVE_RESULT_VAR(ast, sem_type); \
  CG_USE_RESULT_VAR();
```

And now armed with this knowledge we can go back to a previous mystery, let's look at `CG_PUSH_EVAL` again:

```C
// Make a temporary buffer for the evaluation results using the canonical naming convention
// burn the stack slot so that any type and numbered temporary that was needed
// won't be re-used until this scope is over.
#define CG_PUSH_EVAL(expr, pri) \
  CHARBUF_OPEN(expr##_is_null); \
  CHARBUF_OPEN(expr##_value); \
  cg_expr(expr, &expr##_is_null, &expr##_value, pri); \
  stack_level++;
```

The reason that `CG_PUSH_EVAL` includes `stack_level++` is that it is entirely possible, even likely,
that the result of `cg_expr` is in a result variable.  The convention is that if the codegen
requires a result variable it is allocated *first*, before any other temporaries.  This is why
there is a way to reserve a variable that you *might* need.  When the codegen is complete,
and before anything else happens, `stack_level` is increased so that if a temporary is
holding the result, it will not be re-used!  Any other temporaries are available, but the result
is still live.  The macros make it easy to get this stack convention consistently correct.

Now, armed with the knowledge that there are result variables and temporary variables and both
come from the scratch variables we can resolve the last mystery we left hanging.  Why does
the scratch variable API accept an AST pointer?

The only place that AST pointer can be not null is in the `CG_USE_RESULT_VAR` macro, it was
this line:

```C
cg_scratch_var(ast_reserved, sem_type_reserved, &result_var, &result_var_is_null, &result_var_value);
```

And `ast_reserved` refers to the AST that we are trying to evaluate.  There's an important
special case that we want to optimize that saves a lot of scratch variables.  That case is handled
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

```C
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

```C
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

```C
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

```C
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

The code in this case is nearly the same as the previous example.  Let's look at the essential differences:

* If there is an error, `goto catch_start_1` will run
* If the try block succeeds, `goto catch_end_1` will run
* both the `TRY` and `CATCH` branches set the `success` out parameter
* since an out argument was added, CQL generated an error check to ensure that `success` is not null
  * `cql_contract_argument_notnull((void *)success, 1)`, the 1 means "argument 1" and will appear in the error message if this test fails
  * the hidden `_db_` argument doesn't count for error message purposes, so `success` is still the first argument

How does this happen?  Let's look at `cg_trycatch_helper` which does this work:

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

The secret is the `error_target` global variable.
All of the error handling will emit a goto `error_target` statement. The
try/catch pattern simply changes the current error target.  The rest of
the code in the helper is just to save the current error target and to
create unique labels for the try/catch block.

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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
"  UPDATE foo\n  SET t = ?\n    WHERE id = ?;"
```

That surely works, but it's wasteful and ugly. The pretty format instead produces:

```C
    "UPDATE foo "
    "SET t = ? "
      "WHERE id = ?"
```

So, the newlines are gone from the string (they aren't needed), instead the string literal was broken into lines for readability.
The indenting is gone from the string, instead the string fragments are indented.  So what you get is a string literal that
reads nicely but doesn't have unnecessary whitespace for SQLite.  Obviously you can't use pretty-quoted literals in all cases,
it's exclusively for SQLite formatting.

All that's left to do is bind the arguments.  Remember that arg list is in reverse order:

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
  p_C_row C = { ._refs_count_ = 1, ._refs_offset_ = p_C_refs_offset };
```

* `p_C_row` is now initialized to to ref count 1 and refs offset `p_C_refs_offset` defined above

```C
  cql_multifetch(_rc_, C_stmt, 2,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_STRING, &C.x,
                 CQL_DATA_TYPE_NOT_NULL | CQL_DATA_TYPE_INT32, &C.y);
```

* C.x is now of type string

```C
    cql_alloc_cstr(_cstr_1, C.x);
    printf("%s, %d\n", _cstr_1, C.y);
    cql_free_cstr(_cstr_1, C.x);
```

* C.x has to be converted to a C style string before it can be used with `printf` as a `%s` argument

```C
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

```C
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

```C
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

```C
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

```C
p_D_row D = { ._refs_count_ = 1, ._refs_offset_ = p_D_refs_offset };
```

The above also implies the cleanup code:

```C
  cql_teardown_row(D);
```

finally, we fetch `D` from `C`.  That's just some assignments:

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
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

```C
  return cql_fetch_all_results(&info, (cql_result_set_ref *)result_set)
```

And `result_set` was the out-argument for the the `p_fetch_results` method.

So `p_fetch_results` is used to get that result set.  But what can you do with it?
Well, the result set contains copy of all the selected data, ready to use in with a C-friendly API.
The interface is in the generated `.h` file.  Let's look at that now, it's the final piece of the puzzle.

```C
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

```C
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

```C
extern cql_string_ref _Nonnull q_get_x(q_result_set_ref _Nonnull result_set);
extern cql_int32 q_get_y(q_result_set_ref _Nonnull result_set);
```

The actual getters are nearly the same as well

```C
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

```C
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

```C
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

```C
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
should enable readers enough context to understand `cg_c.c` and the runtime helpers in `cqlrt.c`
and `cqlrt_common.c`.  Good luck in your personal exploration.


## Part 4: Testing
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 4 continues with a discussion of the essentials testing frameworks for the CQL compiler.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how testing happens in general -- the core strategies and implementation choices --
so that when reading the tests you will have an idea how it all hangs together. To accomplish
this, various key tools will be explained in detail as well as selected examples of their use.

## Testing

There are several types of tests in the system, all of which are launched by the `test.sh`
script which builds the compiler and does a full test pass, there are well over 3000 tests
as of this writing.  Broadly these are in these few categories:

* **parse tests** : these are in `test.sql`
  * the test script verifies that the compiler can parse this file with no errors
  * the parse pass echoes what it read in normalized form, this is compared against a reference copy and any differences are noted
  * differences can be accepted by using the `ok.sh` script
  * verification here is very light and in fact much of parsing is actually tested in the next pass

* **semantic tests** : these are in `sem_test.sql`
  * the file has no parse errors but it has MANY semantic errors, nearly every such error in fact
  * semantic analysis is run with the `--test` flag which produces AST fragments and echoed CQL
  * the test file includes patterns which either must appear, or must not appear, in the output to pass the test
  * the AST includes full type information, so virtually anything about the semantic results can be, and is, verified
  * many tests are designed to exercise the parser as well, ensuring that the correct AST was built and then analyzed
    * e.g. operator precedence can be verified here
    * the AST echoing logic can also be verified here, e.g. placement of parenthesis in the echoed output
  * any semantic rewrites can be verified here because the rewritten form is emitted in the test output, not the original input
  * all other operations that happen during the semantic pass (e.g. constant evaluation) are also tested here
  * the full semantic output is also normalized (e.g. removing line numbers) and is compared against a reference copy, any differences are noted
  * differences can be accepted by using the `ok.sh` script
  * there are additional files to test different modes like "previous schema" validation (q.v.) as well as dev mode and the schema migrator, the files in this family are: `sem_test.sql`, `sem_test_dev.sql`, `sem_test_migrate.sql`, `sem_test_prev.sql`

* **code gen tests** : the basic test in this family is `cg_test.sql` which has the C codegen tests
  * these test files do pattern matching just like the semantic case except the codegen output is checked rather than the AST
  * the test output is normalized and checked against a reference, just like the semantic tests
  * there is generally no need to check for errors in test output because all errors are detected during semantic analysis
  * there are MANY tests in this family, at least one for each of the various generators:
    * `cg_test.sql`, `cg_test_assembly_query.sql`, `cg_test_base_fragment.sql`, `cg_test_base_java_fragment.sql`, `cg_test_c_type_getters.sql`, `cg_test_extension_fragment.sql`, `cg_test_extension_java_fragment.sql`, `cg_test_generate_copy.sql`, `cg_test_generated_from.sql`, `cg_test_json_schema.sql`, `cg_test_no_result_set.sql`, `cg_test_out_object.sql`, `cg_test_out_union.sql`, `cg_test_prev_invalid.sql`, `cg_test_query_plan.sql`, `cg_test_schema_upgrade.sql`, `cg_test_single_proc_not_nullable.sql`, `cg_test_single_proc_nullable.sql`, `cg_test_suppressed.sql`, `cg_test_test_helpers.sql`, `cg_test_with_object.sql`,

* **run tests** : the main run test creatively named `run_test.sql`
  * this test code is compiled and excuted
  * the test contains expectations like any other unit test
  * it has CQL parts and C parts, the C parts test the C API to the procedures, plus do initial setup
  * these test include uses of all CQL features and all of the CQL runtime features
  * the schema upgrader tests are arguably "run tests" as well in that they run the code but they have a much different verification strategy

* **unit test** : the compiler supports the `--run_unit_tests` flag
  * this causes the compile to self-test certain of its helper functions that are otherwise difficult to test
  * mostly this is buffers that need to be growable to but in practice only grow with huge input files
  * other exotic cases that would be hard to reliability hit in some other fashion are covered by this code

Test coverage is maintained at 100% line coverage (sometimes there are a few
hours when it drops to 99.9% or something like that but this never lasts).
Branch coverage is not especially targetted but is nonethless quite high. To
see the true branch coverage you have to build the compiler with the asserts
(Contract and Invariant) off.  Last time it was measured, it was well over 80%.

To start the tests you should run `test.sh`, this launches `common/test_common.sh` to do the work.
This structure allows anyone to make their own harness that launches the common test passes and adds
their own extra tests, or passes in additional flags.  `test.sh` itself uses `make` to
build the compiler.

To get the coverage report, use `cov.sh` which in turn launches `test.sh` with suitable flags
and then assembles the coverage report using `gcovr`.

### Parse Tests

Looking at `test/test_common.sh` we find the source for the most basic test.  This is entirely
unremarkable stuff.

```bash
basic_test() {
  echo '--------------------------------- STAGE 2 -- BASIC PARSING TEST'
  echo running "${TEST_DIR}/test.sql"
  if ! ${CQL} --dev --in "${TEST_DIR}/test.sql" >"${OUT_DIR}/test.out"
  then
   echo basic parsing test failed
   failed
  fi
  echo "  computing diffs (empty if none)"
  on_diff_exit test.out
}
```

* it's "STAGE 2" because "STAGE 1" was the build
* all it tries to do is run the compiler over `test/test.sql`
* if there are errors the test fails
* if there are any differences between `test.out` and `test.out.ref` the test fails

That's it.

### Sematic Tests

The semantic tests are not much different but this is where the pattern matching comes in.

First let's look at the shell script:

```bash
semantic_test() {
  echo '--------------------------------- STAGE 4 -- SEMANTIC ANALYSIS TEST'
  echo running semantic analysis test
  if ! sem_check --sem --print --dev --in "${TEST_DIR}/sem_test.sql" >"${OUT_DIR}/sem_test.out" 2>"${OUT_DIR}/sem_test.err"
  then
     echo "CQL semantic analysis returned unexpected error code"
     cat "${OUT_DIR}/sem_test.err"
     failed
  fi

  echo validating output trees
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/sem_test.sql" "${OUT_DIR}/sem_test.out"
  then
    echo failed verification
    failed
  fi

  echo running dev semantic analysis test
  ... same thing again for sem_test_dev.sql

  echo "  computing diffs (empty if none)"
  on_diff_exit sem_test.out
  on_diff_exit sem_test.err
  ... same thing again for sem_test_dev.out and .err
}
```

There are basically 3 steps:

* run the compiler over `test/sem_test.sql`
  * fail if this generates no errors (yes you read that right, see below)
* do the pattern matching on the output using `cql-verify` to ensure the patterns match (discussed below)
  * fail if the output is not consistent with the patterns
* compare the reference output for the AST and the errors
  * fail if there are any differences

In the first step the compiler MUST produce an error code, let's look at `sem_check` to see why:

```bash
sem_check() {
  ${CQL} "$@"
  if [ "$?" -ne "1" ]
  then
     echo 'All semantic analysis checks have errors in the test'
     echo 'the normal return code is "1" -- any other return code is bad news'
     echo 'A return code of zero indicates we reported success in the face of errors'
     echo 'A return code other than 1 indicates an unexpected fatal error of some type'
     return 1
  fi
}
```

In short `sem_test.sql` is FULL of semantic errors, that's part of the test.  If the compiler
reports success something is *seriously* wrong.

In the next phase we're going to do some pattern matching, let's look at a couple of examples
to illustrate how this works.  The program `cql-verify` actually does all this matching and
that program is itself written in (mostly) CQL which is cute.
It can be found in the `tester` directory.

Here's a very simple example:

```sql
-- TEST: we'll be using printf in lots of places in the tests as an external proc
-- + {declare_proc_no_check_stmt}: ok
-- - Error
DECLARE PROCEDURE printf NO CHECK;
```

The code under test is of course `DECLARE PROCEDURE printf NO CHECK`.  The patterns happen
immediately before this code.  Let's look at each line:

* `-- TEST: etc.` : this is just a comment, it means nothing and serves no purpose other than documentation
* `-- + {declare_proc_no_check_stmt}: ok` : the comment stats with `" + "`, this is a trigger
  * the test output from the statement under test must include indicated text
  * this happens to be the text for the AST of `declare_proc_no_check_stmt` after semantic success
  * there is no type info hence the `ok` designation (recall `SEM_TYPE_OK`)
* `-- Error` : the comment starts with `" - "`, this is a trigger
  * the test output from the statement under test must NOT include indicated text
  * in this case that means no reported erros

Easy enough.  Now does this happen?

The test output includes:

* text like "The statement ending at line XXXX" where XXXX is appropriate line number
* an echo of the statement that was analyzed (after any rewrites)
* the AST of that statement including semantic type info that was computed

Using the value of XXXX the tester searches the test file in this case `sem_test.sql`, it
extracts the test patterns that happen AFTER the previous XXXX value for the previous statement
and up to the indicated line number.  This is The Price Is Right algorithm where you
read up to the designated lines without going over.

Each pattern is matched, or not matched, using the SQL `LIKE` or `NOT LIKE` operator.  In case
of errors the tester writes out the actual output and the expected patterns having all this information
handy.

The line numbers are all changed to literally "XXXX" after this pass so that the difference in
later passes is not a cascade of of trivial line number changes in otherwise identical output.

Let's look at another example:

```sql
-- TEST: create a table using type discrimation: kinds
-- + {create_table_stmt}: with_kind: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> }
-- + {col_def}: id: integer<some_key>
-- + {col_def}: cost: real<dollars>
-- + {col_def}: value: real<dollars>
-- - Error
create table with_kind(
  id integer<some_key>,
  cost real<dollars>,
  value real<dollars>
);
```

This reads pretty easily now:

* `{create_table_stmt}` : the struct type of the table must be an exact match for what is expected
* `{col_def}` : there are 3 different `{col_def}` nodes, one for each column
* `- Error` : there are no reported errors

So there are no errors reported nor are there any in the AST.  At least the part of the AST that was
checked.  The AST actually had other stuff too but it's normal to just test the "essential" stuff.
There are many tests that try many variations and we don't want to check every fact in every case
of every test.

If you want to see the whole AST output for this, it's easy enough.  It's sitting in `sem_test.out.ref`

```
The statement ending at line XXXX

CREATE TABLE with_kind(
  id INTEGER<some_key>,
  cost REAL<dollars>,
  value REAL<dollars>
);

  {create_table_stmt}: with_kind: { id: integer<some_key>, cost: real<dollars>, value: real<dollars> }
  | {create_table_name_flags}
  | | {table_flags_attrs}
  | | | {int 0}
  | | {name with_kind}
  | {col_key_list}
    | {col_def}: id: integer<some_key>
    | | {col_def_type_attrs}: ok
    |   | {col_def_name_type}
    |     | {name id}
    |     | {type_int}: integer<some_key>
    |       | {name some_key}
    | {col_key_list}
      | {col_def}: cost: real<dollars>
      | | {col_def_type_attrs}: ok
      |   | {col_def_name_type}
      |     | {name cost}
      |     | {type_real}: real<dollars>
      |       | {name dollars}
      | {col_key_list}
        | {col_def}: value: real<dollars>
          | {col_def_type_attrs}: ok
            | {col_def_name_type}
              | {name value}
              | {type_real}: real<dollars>
                | {name dollars}
```

As you can see there was potentially a lot more than could have been verified but those view key lines were
selected because their correctness really implies the rest.  In fact just the `{create_table_stmt}` line
really was enough to know that everthing was fine.

Let's look at one more example, this time on that is checking for errors.  Many tests check for
errors because correctly reporting errors is the primary job of `sem.c`.  It's fair to say that
there are more tests for error cases than there are for correct cases because there are a lot
more ways to write code incorrectly than correctly.  Here's the test:

```SQL
-- TEST: join with bogus ON expression type
-- + Error % expected numeric expression 'ON'
-- +1 Error
-- + {select_stmt}: err
-- + {on}: err
select * from foo
inner join bar as T2 on 'v'
where 'w'
having 'x'
limit 'y';
```

* `+ Error % expected numeric expression 'ON'` : there must be a reported Error message with the indicated error text
* `+1 Error` : this indicates that there must be *exactly* 1 match for the pattern "Error" (i.e. exactly one error)
  * note that there are several problems with the test statement but error processing is supposed to stop after the first
* `-- + {on}: err` : verifies that the ON clause was marked as being in error
* `-- + {select_stmt}: err` : verifies that the error correctly propogated up to the top level statement

Note that the patterns can be in any order and every pattern is matched against the whole input so for instance:

```
-- + {on}: err
-- + {on}: err
```

The above does not imply that there are two such `{on}` nodes.  The second line will match the same text as the first.
To to enforce that there were exactly two matches you use:

```
-- +2 {on}: err
```

There is no syntax for "at least two matches" though one could easily be added.  So far it hasn't been especially
necessary.

As we'll see this simple pattern is used in many other tests.  All that is required for it work is output with
lines of the form "The statement ending at line XXXX"

The `sem_test_dev.sql` test file is a set of tests that are run with the `--dev` flag passed to CQL.  This
is the mode where certain statements that are prohibited in production code are verified.  This file is
very small indeed and the exact prohibitions are left as an exercise to the reader.

### Code Generation Tests

The test logic for the "codegen" family of tests (`cg_test*.sql`) is virtually identical to the semantic
test family. The same testing utililty is used, and it works the same way, looking for the same marker.
The only difference in this stage is that the test output is generated code, not an AST. The codegen tests
are a great way to lock down important code fragments in the output.  Note that the codegen tests do not actually
execute any generated code.  That's the next category.

Here's an sample test:

```sql
-- TEST: unused temp in unary not emitted
-- - cql_int32 _tmp_int_0 = 0;
-- - cql_int32 _tmp_int_1 = 0;
-- + o = i.value;
-- + o = - 1;
create proc unused_temp(i integer, out o integer not null)
begin
  set o := coalesce(i, -1);
end;
```

This test is verifying one of the optimizations that we talked about in
[Part 3](https://cgsql.dev/cql-guide/int03#result-variables).
In many cases temporary variables for results (such as function calls) can be elided.

* `- cql_int32 _tmp_int_0 = 0;` : verifies that this temporary is NOT created
* `- cql_int32 _tmp_int_1 = 0;` : likewise
* `+ o = i.value;` : the first alternative in coalesce directly assigns to `o`
* `+ o = - 1;` : as does the second

It might be helpful to look at the full output, which as always is in a `.ref` file.
In this case `cg_test.c.ref`.  Here is the full output with the line number
normalized:

```C
// The statement ending at line XXXX

/*
CREATE PROC unused_temp (i INTEGER, OUT o INTEGER NOT NULL)
BEGIN
  SET o := coalesce(i, -1);
END;
*/

#define _PROC_ "unused_temp"
// export: DECLARE PROC unused_temp (i INTEGER, OUT o INTEGER NOT NULL);
void unused_temp(cql_nullable_int32 i, cql_int32 *_Nonnull o) {
  cql_contract_argument_notnull((void *)o, 2);

  *o = 0; // set out arg to non-garbage
  do {
    if (!i.is_null) {
      *o = i.value;
      break;
    }
    *o = - 1;
  } while (0);

}
#undef _PROC_
```

As we can see, the test has picked out the bits that it wanted to verify. The `coalesce`
function is verified elsewhere -- in this test we're making sure that this pattern doesn't cause
extra temporaries.

Let's take a quick look at the part of `test_common.sh` that runs this:

```bash
code_gen_c_test() {
  echo '--------------------------------- STAGE 5 -- C CODE GEN TEST'
  echo running codegen test
  if ! ${CQL} --test --cg "${OUT_DIR}/cg_test_c.h" "${OUT_DIR}/cg_test_c.c" \
    "${OUT_DIR}/cg_test_exports.out" --in "${TEST_DIR}/cg_test.sql" \
    --global_proc cql_startup --generate_exports 2>"${OUT_DIR}/cg_test_c.err"
  then
    echo "ERROR:"
    cat "${OUT_DIR}/cg_test_c.err"
    failed
  fi

  echo validating codegen
  if ! "${OUT_DIR}/cql-verify" "${TEST_DIR}/cg_test.sql" "${OUT_DIR}/cg_test_c.c"
  then
    echo "ERROR: failed verification"
    failed
  fi

  echo testing for successful compilation of generated C
  rm -f out/cg_test_c.o
  if ! do_make out/cg_test_c.o
  then
    echo "ERROR: failed to compile the C code from the code gen test"
    failed
  fi

  ...

  echo "  computing diffs (empty if none)"
  on_diff_exit cg_test_c.c
  on_diff_exit cg_test_c.h

  ... other tests
}
```

Briefly reviewing this, we see the following important steps:

* `{CQL} --test --cg etc.` : run the compiler on the test input
  * the test fails if there are any errors
* `cql-verify` : performs the pattern matching
  * the output has the same statement markers as in the semantic case
* `do_make` : use `make` to build the generated code ensuring it compiles cleanly
  * if the C compiler returns any failure, the test fails
* `on_diff_exit` : compares the test output to the reference output
  * any difference fails the test

This is all remarkably similar to the semantic tests. All the code generators
are tested in the same way.

### Run Tests

The last category of tests actually does execution.  The main "run test" happens
at "stage 13", because there are *many* codegen tests for the various
output formats and these all pass before before we try to execute anything.
This is not so bad because the tests are quite quick with a full test pass taking
less than 90s on my laptop.

```bash
run_test() {
  echo '--------------------------------- STAGE 13 -- RUN CODE TEST'
  echo running codegen test with execution
  if ! cc -E -x c -w "${TEST_DIR}/run_test.sql" \
    >"${OUT_DIR}/run_test_cpp.out"
  then
    echo preprocessing failed.
    failed
  elif ! ${CQL} --nolines \
    --cg "${OUT_DIR}/run_test.h" "${OUT_DIR}/run_test.c" \
    --in "${OUT_DIR}/run_test_cpp.out" \
    --global_proc cql_startup --rt c
  then
    echo codegen failed.
    failed
  elif ! (echo "  compiling code"; do_make run_test )
  then
    echo build failed
    failed
  elif ! (echo "  executing tests"; "./${OUT_DIR}/a.out")
  then
    echo tests failed
    failed
  fi
  ...
```

The main structure is mostly what one would expect:

* `cc -E -x c` : this is used to pre-process the run test file so that we can use C pre-processor features to define tests
  * there are quite a few helpful macros as we'll see
  * if pre-processing fails, the test fails
* `{CQL} --nolines --cg ...` : this is used to create the `.h` and `.c` file for the compiland
  * `--nolines` is used to suppress the `#` directives that would associate the generated code with the .sql file
  * compilation failures cause the test to fail
* `do_make` : as before this causes `make` to build the compiland (`run_test`)
  * this build target includes the necessary bootstrap code to open a database and start the tests
  * any failures cause the test to fail
* `a.out` : the tests execute
  * the tests return a failure status code if anything goes wrong
  * any failure causes the test to fail

The test file `run_test.sql` includes test macros from `cqltest.h` -- all of these are very
simple.  The main ones are `BEGIN_SUITE`, `END_SUITE`, `BEGIN_TEST` and `END_TEST` for
structure; and `EXPECT` to verify a boolean expression.

Here's a simple test case with several expectations:

```
BEGIN_TEST(arithmetic)
  EXPECT_SQL_TOO((1 + 2) * 3 == 9);
  EXPECT_SQL_TOO(1 + 2 * 3 == 7);
  EXPECT_SQL_TOO(6 / 3 == 2);
  EXPECT_SQL_TOO(7 - 5 == 2);
  EXPECT_SQL_TOO(6 % 5 == 1);
  EXPECT_SQL_TOO(5 / 2.5 == 2);
  EXPECT_SQL_TOO(-(1+3) == -4);
  EXPECT_SQL_TOO(-1+3 == 2);
  EXPECT_SQL_TOO(1+-3 == -2);
  EXPECT_SQL_TOO(longs.neg == -1);
  EXPECT_SQL_TOO(-longs.neg == 1);
  EXPECT_SQL_TOO(- -longs.neg == -1);
END_TEST(arithmetic)
```

We should also reveal `EXPECT_SQL_TOO`, discussed below:

```C
-- use this for both normal eval and SQLite eval
#define EXPECT_SQL_TOO(x) EXPECT(x); EXPECT((select x))
```

Now back to the test:

* `EXPECT(x)` : verifies that `x` is true (i.e. a non-zero numeric)
  * not used directly in this example
* `EXPECT_SQL_TOO` : as the definition shows,
  * `x` must be true (as above)
  * `(select x)` must also be true,
    * i.e. when SQLite is asked to evaluate the expression the result is also a "pass"
  * this is used to verify consistency of order of operations and other evaluations that must be the same in both forms
  * note that when `(select ...)` is used, CQL plays no part in evaluating the expression, the text of the expression goes to SQLite and any variables are bound as described in Part 3.

The run test exercises many features, but the testing strategy is always the same:

* exercise some code pattern
* use `EXPECT` to validate the results are correct
* the expressions in the `EXPECT` are usually crafted carefully to show that a certain mistake is not being made
  * e.g. expressions where the result would be different if there are bugs in order of operations
  * e.g. expressions that would crash with divide by zero if code that isn't supposed to run actually ran


### Schema Upgrade Testing

The schema upgrade tester is quite a bit different than the others and relies heavily on execution
of the upgraders.  Before we get into that there is a preliminary topic:

#### "Previous Schema" Validation

In order to ensure that it is possible to create an upgrader, CQL provides features to validate
the current schema against the previous schema ensuring that nothing has been done that would
make an upgrader impossible. This is more fully discussed in
[Chapter 11](https://cgsql.dev/cql-guide/ch11) of the Guide.

"Previous Schema" validation is a form of semantic check and so its testing happens as
described above. Importantly, as with the other back-end passes the schema upgrader does
not have to concern itself with error cases as they are already ruled out.  The upgrader
itself will be the subject of Part 5.

#### Packing List

The test assets for upgrade tests are found in the `upgrade` directory and consist of
* `SchemaPersistentV0.sql` : baseline version of the test schema
* `SchemaPersistentV1.sql` : v1 of the test schema
* `SchemaPersistentV2.sql` : v2 of the test schema
* `SchemaPersistentV3.sql` : v3 of the test schema
* `downgrade_test.c` : a test that simulates attemping to go backwards in schema versions
* `upgrade_test.c` : the C harness that launches the upgraders and fires the tests
* `upgrade_test.sh` : the shell script that makes all this happen
* `upgrade_validate.sql` : some simple code that sanity checks the recorded schema version against tables in it
  * used to ensure that the schema we are on is the schema we think we are on, not to validate all facets of it
  * also renders the contents of `sqlite_master` in a canonical form

We haven't yet discussed the internals of schema upgrade, so for purposes of this part we're only going
to discuss how the testing proceeds.  The upgrade will be considered "magic" for now.

In addition to these assets, we also have reference files:
* `upgrade_schema_v0.out.ref` : expected content of v0
* `upgrade_schema_v1.out.ref` : expected content of v1
* `upgrade_schema_v2.out.ref` : expected content of v2
* `upgrade_schema_v3.out.ref` : expected content of v3

#### `upgrade_validate.sql`

This file has a single procedure `validate_transition` which does the two jobs:
* emits the canonicalized version of `sqlite_master` to the output
  * this is needed because `sqlite_master` text can vary between Sqlite versions
* checks for basic things that should be present in a given version

The output of the validator looks like this:

```
reference results for version 0

----- g1 -----

type: table
tbl_name: g1
CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT)

----- sqlite_autoindex_test_cql_schema_facets_1 -----

type: index
tbl_name: test_cql_schema_facets

----- test_cql_schema_facets -----

type: table
tbl_name: test_cql_schema_facets
CREATE TABLE test_cql_schema_facets(
  facet TEXT NOT NULL PRIMARY KEY,
  version LONG_INT NOT NULL)
```

The formatting rules are very simple and so the output is pretty readable.

The verifications are very simple.

First this happens:

```sql
let version := cast(test_cql_get_facet_version("cql_schema_version") as integer);
```

The printing happens, then this simple validation:

```sql
  let recreate_sql := (
    select sql from sqlite_master
    where name = 'test_this_table_will_become_create'
    if nothing null);

...
 switch version
  when 0 then
    if recreate_sql is null or recreate_sql not like '%xyzzy INTEGER%' then
      call printf("ERROR! test_this_table_will_become_create should have a column named xyzzy in v%d\n", version);
      throw;
    end if;
  ...
  else
    call printf("ERROR! expected schema version v%d\n", version);
    throw;
  end;
```

In short, the version number must be one of the valid versions and each version is expecting
that particular table to be in some condition it can recognize.

The real validation is done by noting any changes in the reference output plus a series of invariants.

#### Prosecution of the Upgrade Test

** Launch **

We kick things off as follows:

* `test.sh` calls `upgrade/upgrade_test.sh`
  * this test doesn't usually run standalone (but it can)

** Build Stage **

This creates the various binaries we will need:

* `upgrade_validate.sql` is compiled down to C
  * this code works for all schema versions, it's generic
* `SchemaPersistentV[0-3].sql` are compiled into C (this takes two steps)
  * first, the CQL upgrader is generated from the schema
  * second, the CQL upgrader is compiled to C
* `make` is used to lower all of the C into executables `upgrade[0-3]` plus `downgrade_test`
  * the shared validation code is linked into all 4 upgraders
  * `downgrade_test.c` is linked with the code for `upgrade1`


** Basic Upgrades **

Here we test going from scratch to each of the 4 target versions:

* `upgrade[0-3]` are each run in turn with no initial database
  * i.e. their target database is deleted before each run
* the validation output is compared against the reference output
  * any differences fail the test

** Previous Schema Validation **

This sanity checks that the chain of schema we have built should work
when upgrading from one version to the next:

* try each schema with this predecessor:
  * `SchemaPersistentV1.sql` with `SchemaPersistentV0.sql` as the previous
  * `SchemaPersistentV2.sql` with `SchemaPersistentV1.sql` as the previous
  * `SchemaPersistentV3.sql` with `SchemaPersistentV2.sql` as the previous
* if any of these produce errors something is structurally wrong with the test or else previous schema validation is broken


** Two-Step Upgrades **

Now we verify that we can go from any version to any other version with a stop in between to persist.

An example should make this clearer:

* We start from scratch and go to v2
  * this should produce the v2 reference schema output as before
* We run the v4 upgrader on this v2 schema
  * this should produce the v4 reference schema output as before
  * i.e. if we go from nothing to v2 to v4 we get the same as if we just go to v4 directly

There are quite a few combinations like this, the test output lists them all:

```
Upgrade from nothing to v0, then to v0 -- must match direct update to v0
Upgrade from nothing to v0, then to v1 -- must match direct update to v1
Upgrade from nothing to v1, then to v1 -- must match direct update to v1
Upgrade from nothing to v0, then to v2 -- must match direct update to v2
Upgrade from nothing to v1, then to v2 -- must match direct update to v2
Upgrade from nothing to v2, then to v2 -- must match direct update to v2
Upgrade from nothing to v0, then to v3 -- must match direct update to v3
Upgrade from nothing to v1, then to v3 -- must match direct update to v3
Upgrade from nothing to v2, then to v3 -- must match direct update to v3
Upgrade from nothing to v3, then to v3 -- must match direct update to v3
```
Note that one of the combinations tested is starting on `Vn` and "upgrading"
from there to `Vn`. This should do nothing.

** Testing downgrade **

Here we make sure that any attempt to "go backwards" results in an error.

* the `v3` schema created by the previous test is used as input to the downgrade test
* the downgrade test was linked with the `v2` upgrader
* when executed the `v2` upgrader should report the error
  * this test's verifier checks for a correct error report
* the test test fails if the error is no correctly reported

The combination of testing reference outputs plus testing these many invariants
at various stages results in a powerful integration test.  The actual schema
for the varios versions includes all the supported transitions such as
creating and deleting tables and columns, and recreating views, indicies, and triggers.

All of the possible transitions are more fully discussed in
[Chapter 10](https://cgsql.dev/cql-guide/ch10) of the Guide which pairs nicely
with the previous schema validions discussed in
[Chapter 11](https://cgsql.dev/cql-guide/ch11).

### Testing the `#line` directives produced by CQL

[An additional section should be added for the code that verifies the source line number mappings
even though this is a pretty exotic case.]

### Summary

While there are a few more isolated verifications that happen in `test.sh` and of course
there is the plumbing necessary to let `cov.sh` use the test script to create coverage reports,
the above forms make up the vast majority of the test patterns.

Generally, the test files are designed to hold as many tests as can reasonably fit with
the gating factor being cases where different flags are necessary.  There are two different
stages were many different tiny input files are used to create trivial failures like missing
command line arguments and such.  But those cases are all just looking for simple error
text and a failure code, so they should be self-evident.  With so many options, many
such baby tests are needed.



---
id: int01
title: "CQL Internals Guide"
sidebar_label: "CQL Internals Guide"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
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

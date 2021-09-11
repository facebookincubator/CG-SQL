---
id: int07
title: "Part 7: JSON Generation"
sidebar_label: "Part 7: JSON Generation"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 7 continues with a discussion of the JSON generation code.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how JSON creation works in general -- the core strategies and implementation choices --
so that when reading the source you will have an idea how it all hangs together. To accomplish
this, we'll illustrate the key pieces that can be customized and we'll discuss some
interesting cases.

## JSON Schema

The JSON schema is described in [Chapter 13](https://cgsql.dev/cql-guide/ch13) of the Guide and there
is a nice diagram of its [grammar](https://cgsql.dev/json-diagram) for reference.  So, we won't be
discussing all the details of the output.  Instead we're going to go over the theory of how the
JSON generator works. It is structured very much like the other code generators but it happens
to produce a JSON file.  It's call the "JSON Schema" because most of the content is a description
of the database schema in JSON form.  As such it's almost entirely just a simple walk of the AST
in the right order.  The only really tricky bit is the extra dependency analysis that happens
and is in the output for downstream tools to use as needed.  So we're going to cover these topics:

* walking the AST bits
* formatting particulars
* computing the dependencies

This should be a short chapter compared to the others, this output really is much simpler to
create than the C or the schema upgrader.

### Walking the AST

If you run this command

```bash
$ cql --in x --rt json_schema --cg x.json
```

Where `x` is an empty file, you'll get the following skeletal JSON, lightly reformatted for brevity:

```JSON
{
  "tables" : [  ],
  "virtualTables" : [  ],
  "views" : [  ],
  "indices" : [  ],
  "triggers" : [  ],
  "attributes" : [  ],
  "queries" : [  ],
  "inserts" : [  ],
  "generalInserts" : [  ],
  "updates" : [  ],
  "deletes" : [  ],
  "general" : [  ],
  "regions" : [  ],
  "adHocMigrationProcs" : [  ],
  "enums" : [  ]
}
```

From this we can deduce a great deal of the structure of the code:

```C
// Main entry point for json schema format
cql_noexport void cg_json_schema_main(ast_node *head) {
  Contract(options.file_names_count == 1);

  cql_exit_on_semantic_errors(head);

  tables_to_procs = symtab_new();

  CHARBUF_OPEN(main);
  charbuf *output = &main;

  bprintf(output, "%s", rt->source_prefix);

  // master dictionary begins
  bprintf(output, "\n{\n");
  BEGIN_INDENT(defs, 2);
  cg_json_tables(output);
  bprintf(output, ",\n");
  cg_json_virtual_tables(output);
  bprintf(output, ",\n");
  cg_json_views(output);
  bprintf(output, ",\n");
  cg_json_indices(output);
  bprintf(output, ",\n");
  cg_json_triggers(output);
  bprintf(output, ",\n");
  cg_json_stmt_list(output, head);
  bprintf(output, ",\n");
  cg_json_regions(output);
  bprintf(output, ",\n");
  cg_json_ad_hoc_migration_procs(output);
  bprintf(output, ",\n");
  cg_json_enums(output);

  if (options.test) {
    bprintf(output, ",\n");
    cg_json_table_users(output);
  }

  END_INDENT(defs);
  bprintf(output, "\n}\n");

  cql_write_file(options.file_names[0], output->ptr);
  CHARBUF_CLOSE(main);

  SYMTAB_CLEANUP(tables_to_procs);
}
```

This is the main function and you can see that it mirrors the JSON output exactly.  There is some additional test output possible,
this output is simply a reverse index that lets you go from a table to the procedures that use that table.  The mapping can
easily be created by processing the JSON for procedures, each includes its dependency information as we will see.

#### Example JSON Writer: Views

These are sufficiently easy that we can just walk through one of the procedures front to back.  Let's look at the "views" section.

```C
// The set of views look rather like the query section in as much as
// they are in fact nothing more than named select statements.  However
// the output here is somewhat simplified.  We only emit the whole select
// statement and any binding args, we don't also emit all the pieces of the select.
static void cg_json_views(charbuf *output) {
  bprintf(output, "\"views\" : [\n");
  BEGIN_INDENT(views, 2);

  int32_t i = 0;
  for (list_item *item = all_views_list; item; item = item->next) {
    ast_node *ast = item->ast;
    Invariant(is_ast_create_view_stmt(ast));

    ast_node *misc_attrs = NULL;
    ast_node *attr_target = ast->parent;
    if (is_ast_stmt_and_attr(attr_target)) {
      EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc, attr_target->parent);
      misc_attrs = misc;
    }

    cg_json_test_details(output, ast, misc_attrs);

    EXTRACT_OPTION(flags, ast->left);
    EXTRACT(view_and_attrs, ast->right);
    EXTRACT(name_and_select, view_and_attrs->left);
    EXTRACT_ANY_NOTNULL(select_stmt, name_and_select->right);
    EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
    EXTRACT_STRING(name, name_ast);

    if (i > 0) {
      bprintf(output, ",\n");
    }
    bprintf(output, "{\n");

    bool_t is_deleted = ast->sem->delete_version > 0;
    BEGIN_INDENT(view, 2);
    bprintf(output, "\"name\" : \"%s\"", name);
    bprintf(output, ",\n\"CRC\" : \"%lld\"", crc_stmt(ast));
    bprintf(output, ",\n\"isTemp\" : %d", !!(flags & VIEW_IS_TEMP));
    bprintf(output, ",\n\"isDeleted\" : %d", is_deleted);
    if (is_deleted) {
      bprintf(output, ",\n\"deletedVersion\" : %d", ast->sem->delete_version);
      cg_json_deleted_migration_proc(output, view_and_attrs);
    }

    if (ast->sem->region) {
      cg_json_emit_region_info(output, ast);
    }

    if (misc_attrs) {
      bprintf(output, ",\n");
      cg_json_misc_attrs(output, misc_attrs);
    }

    cg_json_projection(output, select_stmt);
    cg_fragment_with_params(output, "select", select_stmt, gen_one_stmt);
    cg_json_dependencies(output, ast);
    END_INDENT(view);
    bprintf(output, "\n}\n");
    i++;
  }

  END_INDENT(views);
  bprintf(output, "]");
}
```

#### View Loop

Already we can see the structure emerging, and of course its nothing
more than a bunch of `bprintf`.  Let's do it section by section:

```C

bprintf(output, "\"views\" : [\n");
BEGIN_INDENT(views, 2);

for (list_item *item = all_views_list; item; item = item->next) {
   ...
}

END_INDENT(views);
bprintf(output, "]");
```

#### View Extraction

Unsurprisingly, this code will iterate the `all_views_list` which was
created precisely for this kind of output.  The semantic pass populates
this list for use downstream.

We'll deal with `BEGIN_INDENT` a bit later, but it should be clear what
it does by the name for now.  So we've made the "views" section and
we'll put 0 or more views in it.

The next section extracts the necessary information and emits
the test output

```
    ast_node *ast = item->ast;
    Invariant(is_ast_create_view_stmt(ast));

    ast_node *misc_attrs = NULL;
    ast_node *attr_target = ast->parent;
    if (is_ast_stmt_and_attr(attr_target)) {
      EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc, attr_target->parent);
      misc_attrs = misc;
    }

    cg_json_test_details(output, ast, misc_attrs);

    EXTRACT_OPTION(flags, ast->left);
    EXTRACT(view_and_attrs, ast->right);
    EXTRACT(name_and_select, view_and_attrs->left);
    EXTRACT_ANY_NOTNULL(select_stmt, name_and_select->right);
    EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
    EXTRACT_STRING(name, name_ast);
```

The `is_ast_stmt_and_attr` node tell us if there were any misc attributes on the
statement.  Those attributes can be extracted and printed.  We have to look up
the tree a little bit from where we are because this is the "all views" list,
if there were attributes on this view they were attached two levels up.  In any
case `misc_attrs` ends with attributes if there are any.

After the test output, the necessary view attributes are extracted the usual
way with `EXTRACT` macros for the view shape.

#### Test Output

```C
static void cg_json_test_details(charbuf *output, ast_node *ast, ast_node *misc_attrs) {
  if (options.test) {
    bprintf(output, "\nThe statement ending at line %d\n", ast->lineno);
    bprintf(output, "\n");

    gen_set_output_buffer(output);
    if (misc_attrs) {
      gen_with_callbacks(misc_attrs, gen_misc_attrs, NULL);
    }
    gen_with_callbacks(ast, gen_one_stmt, NULL);
    bprintf(output, "\n\n");
  }
}
```

All of the JSON fragments have the usual test pattern "The statement ending at line nnn".
This means that the normal validator will be able to find comments in the test file
and associate them with json parts.  The testing strategies are discussed in
[Part 4]((https://cgsql.dev/cql-guide/int04).

In addition, while in test mode, we also emit the original statement that caused 
this JSON fragment to be created. This allows the test patterns to cross check
the input and output and also makes the test output more readable for humans.

Note that in test mode the JSON is effectively corrupted by the test output as it
is not well-formed JSON in any way.  So use of --test is strictly for validation only.

#### Fixed View Things

All of the things that go into the JSON have some attributes that are universally present
and generally come directly from the AST.

```C
  if (i > 0) {
    bprintf(output, ",\n");
  }
  bprintf(output, "{\n");

  bool_t is_deleted = ast->sem->delete_version > 0;
  
  BEGIN_INDENT(view, 2);
  bprintf(output, "\"name\" : \"%s\"", name);
  bprintf(output, ",\n\"CRC\" : \"%lld\"", crc_stmt(ast));
  bprintf(output, ",\n\"isTemp\" : %d", !!(flags & VIEW_IS_TEMP));
  bprintf(output, ",\n\"isDeleted\" : %d", is_deleted);
  if (is_deleted) {
    bprintf(output, ",\n\"deletedVersion\" : %d", ast->sem->delete_version);
    cg_json_deleted_migration_proc(output, view_and_attrs);
  }

  ...

  END_INDENT(view);
  bprintf(output, "\n}\n");
  i++;
}  
```

This part of the output is the simplest

* we emit a comma if we need one (only the first entry doesn't)
* we start the view object '{'
* more indenting for the interior of the view
* emit the view name
* emit the CRC of the view (this makes it easy to see if the view changed)
  * `crc_stmt` computes the CRC by echoing the statement into a scratch buffer and then running the CRC algorithm on that buffer
* note the ",\n" pattern, this pattern is used because sometimes there are optional parts and using a leading ",\n" makes it clear which part is supposed to emit the comma
  * it turns out getting the commas right is one of the greater annoyances of JSON output
* emit "isTemp" 
* emit "isDeleted" 
* if deleted, emit "deletedVersion"
* if there is a migration procedure emit that as well
  * `cg_json_deleted_migration_proc` scans the attribute list for deleted attribute and emits the procedure name on that attribute if there is one

Hopefully this all looks very straighforward.

#### Optional Info

The next fragment emits two optional pieces that are present in many types of objects:

```C
    if (ast->sem->region) {
      cg_json_emit_region_info(output, ast);
    }

    if (misc_attrs) {
      bprintf(output, ",\n");
      cg_json_misc_attrs(output, misc_attrs);
    }
```

* if there is a region assocatied with this view, we emit it here
  * `cg_json_emit_region_info` emits two things:
    * the view's region
    * the "deployment region" of that region if any (regions are contained in deployable groups)
    * see [Chapter 10](https://cgsql.dev/cql-guide/ch10#schema-regions) for more info on regions and deployment regions
    
* if there are any miscellaneous attributes they are emitted
  * we'll use `cg_json_misc_attrs` as our general formatting example when we get to that

#### The View Details

There is very little left in the view emitting code:

```C
  cg_json_projection(output, select_stmt);
  cg_fragment_with_params(output, "select", select_stmt, gen_one_stmt);
  cg_json_dependencies(output, ast);
```

* `cg_json_projection` emits the name and type of each column in the view
* `cg_fragment_with_params` emits the statement that creates the view in an attribute named "select"
  * the normal echoing code does this
  * in other statement forms inside of procedures there can be variables bound in the statement
  * the variable names are replace with "?" in the statement
  * the names of the variable appear in "selectArgs" (there can't be any for views)
* `cg_json_dependencies` emits the tables and views that were used by this view, it gets its own section


Those few things proceduce all JSON for a view and all the other schema elements do basically the same things.  Most of the
helpers are shared so, for instance, regions, misc attributes, and dependencies appear in nearly every kind of object
in the JSON.

### Formatting the JSON

To make the JSON look nice we want to indent it appropriately and put commas in the right
places.  There are some useful macros for this, and they all rely on the fact that
the emitted text goes to a variable creatively called `output`.

Here's a sample procedure that came up before that does the usual tricks:

```C
// Emit a list of attributes for the current entity, it could be any kind of entity.
// Whatever it is we spit out the attributes here in array format.
static void cg_json_misc_attrs(charbuf *output, ast_node *_Nonnull list) {
  Contract(is_ast_misc_attrs(list));
  bprintf(output, "\"attributes\" : [\n");
  BEGIN_INDENT(attr, 2);
  BEGIN_LIST;

  for (ast_node *item = list; item; item = item->right) {
    COMMA;
    cg_json_misc_attr(output, item->left);
  }
  END_LIST;
  END_INDENT(attr);
  bprintf(output, "]");
}
```

So this is going to recursively walk a list of attributes, noting that attributes can contain attributes.

Let's look at the helpers.

From `charbuf.h`:

```C
// These helpers push a buffer and use it for the output temporarily.
// When the buffer is finished (at END_INDENT) bindent is used to
// indent it by the indicated amount.  They assume the output buffer is called
// "output".
#define BEGIN_INDENT(name, level) \
  charbuf *name##_saved = output; \
  int32_t name##_level = level; \
  CHARBUF_OPEN(name); \
  output = &name;

#define END_INDENT(name) \
  output = name##_saved; \
  bindent(output, &name, name##_level); \
  CHARBUF_CLOSE(name); \
```

* `BEGIN_INDENT` : sets up the indenting
  * save the current output buffer
  * stash the desired indent level in a named local
  * make a new scratch buffer using the given name
  * set the output to be the scratch buffer
* `END_INDENT` : flushes the indented stuff
  * restores the output buffer to what it was
  * writes the temporary buffer into the output buffer, indenting it by the desired abount
  * close the temporrary buffer
* `bindent` : a `charbuf` helper that reads the input line by line and writes it with indenting spaces to the output


The rest is to manage the commas in the lists:

```C
// These little helpers are for handling comma seperated lists where you may or may
// not need a comma in various places.  The local tracks if there is an item already
// present and you either get ",\n"  or just "\n" as needed.
#define BEGIN_LIST bool_t list_start = 1
#define CONTINUE_LIST bool_t list_start = 0
#define COMMA if (!list_start) bprintf(output, ",\n"); else list_start = 0
#define END_LIST if (!list_start) bprintf(output, "\n")
```

* `BEGIN_LIST` : starts a list, records that we are at the beginning of the list
* `CONTINUE_LIST` : starts a list, but assumes things have already been put into it
* `COMMA` : a new item is about to be emitted, add a comma if one is needed
  * i.e. add a comma if we are not on the first item
* `END_LIST` : emits a blank line if anything went into the list
  * this puts us in the write place to put an end marker such as ']' or '}'

So reviewing this bit of code, 
 * emit the attribute name and start the array "["
 * we start indenting
 * we start a list
 * we emit a comma if needed
 * we emit the new misc attribute
   * this will crack the AST, and get the attribute name and value
   * this can recurse
   * `cg_json_misc_attr` is pretty simple and a good exercise for the reader
 * repeat for all attributes
 * end the list
 * end the indenting
 * emit the attribute end "]"

#### Quoted Text

Most quoted text in the JSON output is either hard-coded and hence known,
or else is a CQL identifier and therefore has no special characters.  However
there are cases where general text might need to be emitted.  When that
happens a call like this is used:

```C
cg_pretty_quote_plaintext(
    sql.ptr, 
    output, 
    PRETTY_QUOTE_JSON | PRETTY_QUOTE_SINGLE_LINE);
```

`cg_pretty_quote_plaintext` has been discussed before when it was used to
create SQL strings before for the C output. This usage is similar.
Here we're using `PRETTY_QUOTE_JSON` to indicate that only
escape sequences supported by JSON should appear.  The format for
hex is different than C and some of the C short escapes are not supported
(e.g. "\a" is not legal JSON).  We always use `PRETTY_QUOTE_SINGLE_LINE`
in the JSON output, so that multi-line SQL is rendered as one line.
Remember here we are are JSON escaping the SQL so the embedded newlines
in the original SQL were already converted to '\' 'n' (two characters)
and so any newlines still in the string are those placed there by the
line breaking of the SQL not by newlines in string literals.  Hence
they are optional.

In any case, `cg_pretty_quote_plaintext` is just the function to do what we need.

### Dependency Analysis

There are a number of places where dependencies have to be computed.  To do this job,
this function is used universally:

```C
// For procedures and triggers we want to walk the statement list and emit a set
// of dependency entries that show what the code in question is using and how.
// We track tables that are used and if they appear in say the FROM clause
// (or some other read-context) or if they are the subject of an insert, update,
// or delete.  We also track the use of nested procedures and produce a list of
// procs the subject might call.  Of course no proc calls ever appear in triggers.
static void cg_json_dependencies(charbuf *output, ast_node *ast) {
    ...
}
```

In general this code walks any statement AST looking for a variety of patterns in the AST.
Actually more accurately it uses `find_table_refs` to do the job, and it does so by:

* create an output buffer for each kind of thing `find_table_refs` might find
* set up a simple callback to fill in the buffer
* invoke `find_table_refs`
* format the buffers that have any data and emit them as dependencies

This works for any kind of AST really, though typically you do this for procedures
and for triggers because they have a body.  It also makes sense for views because
views can refer to other views and to tables.

The primary code looks like this:

```C
  table_callbacks callbacks = {
      .callback_any_table = cg_found_table,
      .callback_any_view = cg_found_view,
      .callback_inserts = cg_found_insert,
      .callback_updates = cg_found_update,
      .callback_deletes = cg_found_delete,
      .callback_from = cg_found_from,
      .callback_proc = cg_found_proc,
      .callback_context = &context,
  };
  find_table_refs(&callbacks, ast);
```

And an example callback:

```C
// This is the callback function that tells us a view name was found in the body
// of the stored proc we are currently examining.  The void context information
// is how we remember which proc we were processing.   For each table we have
// a character buffer.  We look it up, create it if not present, and write into it.
// We also write into the buffer for the current proc which came in with the context.
static void cg_found_view(
  CSTR view_name, 
  ast_node* table_ast, 
  void* pvContext) 
{
  json_context *context = (json_context *)pvContext;
  Contract(context->cookie == cookie_str);  // sanity check
  Contract(context->used_views);

  add_name_to_output(context->used_views, view_name);
}
```

The callback gets the `pvContext` back, which is the `context` local variable
from `cg_json_dependencies`.  This has all the buffers in it.  All
we have to do is add the name to the buffer, which is done as follows:

```C
static void add_name_to_output(charbuf* output, CSTR table_name) {
  Contract(output);
  if (output->used > 1) {
    bprintf(output, ", ");
  }
  bprintf(output, "\"%s\"", table_name);
}
```

* add a comma if needed
* add the name
* done :D

Note: The added name of course doesn't have to be a table name but once upon a time only
tables were added and the formal has never been changed.

So we can see that `find_table_refs` will tell us the kind of thing it found and the name of the thing.

When all this is done each kind of dependency is emitted if it exists, like so:

```C
  if (used_views.used > 1) {
    bprintf(output, ",\n\"usesViews\" : [ %s ]", used_views.ptr);
  }
```

This gives us a quoted list of the dependencies.  Now, how do we find these?

#### Walking the AST for Dependencies

`find_table_refs` is a fairly simple tree walk that looks for certain key patterns
actually the tree walk happens in `find_table_node` which looks for tables and
procedure calls in the nested AST.

`find_table_refs` records the callbacks that were specified, and it makes some symbol
tables so that the same table/view/procedure is not reported twice.  After that
it starts walking the AST recursively looking for the patterns.  Here's an example:


```C

// Recursively finds table nodes, executing the callback for each that is found.  The
// callback will not be executed more than once for the same table name.
static void find_table_node(table_callbacks *callbacks, ast_node *node) {
  // Check the type of node so that we can find the direct references to tables. We
  // can't know the difference between a table or view in the ast, so we will need to
  // later find the definition to see if it points to a create_table_stmt to distinguish
  // from views.

  find_ast_str_node_callback alt_callback = NULL;
  symtab *alt_visited = NULL;
  ast_node *table_or_view_name_ast = NULL;

  ...
  else if (is_ast_delete_stmt(node)) {
    EXTRACT_ANY_NOTNULL(name_ast, node->left);
    table_or_view_name_ast = name_ast;
    alt_callback = callbacks->callback_deletes;
    alt_visited = callbacks->visited_delete;
  }

  ...

}
```

The code afterward will do these steps:

 * notice that `table_or_view_name_ast` was set, hence something was found
 * determine that it is in fact a table
 * call the general callback for any table seen (but only once for this table)
 * call the alternate callback that this is a table being deleted (but only once)

Except for procedure calls, all the other operations work similarly, 
 * `table_or_view_name_ast` is set
 * `alt_callback` is called but only if
 * `alt_visited` doesn't already have the symbol

If a procedure call is found then `callbacks->callback_proc` is used and 
`callbacks->visited_proc` verifies that there are no duplicates.

Note that the code does not do transitive closure of procedure calls because
in general the called procedure is likely in a different translation unit.
However with the direct calls in place it is easy enough to do transitive closure
from the JSON if you do have all the procedures in one unit.

In contrast, when a view usage is encountered, the code follows into the view
and reports what the view uses. This means that the reported tables include
any tables that were used indirectly in views.  

Note: any CTEs that are used will not be reported because 
`find_table_or_view_even_deleted` will fail for a CTE.

### Recap

The JSON output produced by `cg_json_schema.c` is similar to other codegen
output but lacks most of the complexities.  It deals largely with the
declared schema and the declared procedures and their parameters.  Most
of the output it needs to produce is well supported by the normal text
emission features in the compiler and so we end up with a very straightforward
walk of the AST, visiting each of the relevant kinds of nodes in order.

Topics covered included:

* the types of output that will be produced
* the general structure of the main JSON emitter
* an example emitter
* typical formatting features necessary to produce good quality JSON
* a tour of the dependency emitter

As with the other parts, no attempt was made to cover every function in detail.  That is
best done by reading the source code. But there is overall structure here and an understanding
of the basic principles is helpful before diving into the source code.

---
id: int08
title: "Part 8: Test Helpers"
sidebar_label: "Part 8: Test Helpers"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 8 continues with a discussion of the Test Helper  generation code.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how helpers are created in general -- the core strategies and implementation choices --
so that when reading the source you will have an idea how it all hangs together.

## Test Helpers

The testability features are described in [Chapter 12](https://cgsql.dev/cql-guide/ch12) of the Guide
So, we won't be discussing all the details of what can be created.  Instead we're going to go over
the theory of how the generator works. This generator is somewhat different than others in that
it only concerns itself with procedures and only those that have been suitably annotated --
there are large parts of the tree that are of no interest to the test helper logic, including,
importantly the body of procedures.  Only the signature matters.  As we'll see there is a fairly
large family of generators that are like this.

We'll have one section for every kind of output, but really only the `dummy_test` helper is
worthy of detailed discussion the others, as we'll see, are very simple.

### Initialization

The generator is wired like the others with a suitable main, this one is pretty simple:

```C
// Main entry point for test_helpers
cql_noexport void cg_test_helpers_main(ast_node *head) {
  Contract(options.file_names_count == 1);
  cql_exit_on_semantic_errors(head);
  exit_on_validating_schema();
  cg_test_helpers_reset_globals();

  CHARBUF_OPEN(output_buf);

  cg_th_output = &output_buf;

  bprintf(cg_th_output, "%s", rt->source_prefix);
  cg_test_helpers_stmt_list(head);
  cql_write_file(options.file_names[0], cg_th_output->ptr);

  CHARBUF_CLOSE(output_buf);
  cg_test_helpers_reset_globals();
}
```

The text output will be ultimately put into `output_buf` defined here and `helper_flags` will track which kinds of helpers
we saw.  This helps us to emit the right sections of output as we'll see.

The code iterates the AST looking at the top level statement list only and in particular looking for `CREATE PROC`
statements.

```C
// Iterate through statement list
static void cg_test_helpers_stmt_list(ast_node *head) {
  Contract(is_ast_stmt_list(head));
  init_all_trigger_per_table();
  init_all_indexes_per_table();
  CHARBUF_OPEN(procs_buf);
  CHARBUF_OPEN(decls_buf);
  cg_th_procs = &procs_buf;
  cg_th_decls = &decls_buf;

  for (ast_node *ast = head; ast; ast = ast->right) {
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, ast);

    if (is_ast_create_proc_stmt(stmt)) {
      EXTRACT_STRING(proc_name, stmt->left);
      cg_test_helpers_create_proc_stmt(stmt, misc_attrs);
    }
  }

  bprintf(cg_th_output, "%s", decls_buf.ptr);
  bprintf(cg_th_output, "\n");
  bprintf(cg_th_output, "%s", procs_buf.ptr);

  CHARBUF_CLOSE(decls_buf);
  CHARBUF_CLOSE(procs_buf);
  symtab_delete(all_tables_with_triggers);
  all_tables_with_triggers = NULL;
  symtab_delete(all_tables_with_indexes);
  all_tables_with_indexes = NULL;
}
```

There are some preliminaries:

* we make a symbol table that maps from tables names to the list of triggers on that table by walking all the triggers
* we make a symbol table that maps from tables names to the list of indices on that table by walking all the indices
* we'll need two buffers one for declarations (that must go first) and one for procedure bodies
* each `CREATE PROC` statement potentially contributes to both sections
* `cg_test_helpers_create_proc_stmt` checks for the helper attributes and sets up the dispatch to emit the test helpers

To do this we have to walk any misc attributes on the procedure we're looking for things of the form `@attribute(cql:autotest=xxx)`

```C
static void cg_test_helpers_create_proc_stmt(ast_node *stmt, ast_node *misc_attrs) {
  Contract(is_ast_create_proc_stmt(stmt));

  if (misc_attrs) {
    helper_flags = 0;
    dummy_test_infos = symtab_new();

    find_misc_attrs(misc_attrs, test_helpers_find_ast_misc_attr_callback, stmt);

    symtab_delete(dummy_test_infos);
    dummy_test_infos = NULL;
  }
}
```

`find_misc_attrs` calls `test_helpers_find_ast_misc_attr_callback`.  We're going to keep track of
which kinds of helpers we have found to help us with the output.  This is where `helper_flags`
comes in. The flags are:

```C
#define DUMMY_TABLE           1 // dummy_table attribute flag
#define DUMMY_INSERT          2 // dummy_insert attribute flag
#define DUMMY_SELECT          4 // dummy_select attribute flag
#define DUMMY_RESULT_SET      8 // dummy_result_set attribute flag
#define DUMMY_TEST         0x10 // dummy_test attribute flag
```

And now we're ready for actual dispatch:

```C
// This is invoked for every misc attribute on every create proc statement
// in this translation unit.  We're looking for attributes of the form cql:autotest=(...)
// and we ignore anything else.
static void test_helpers_find_ast_misc_attr_callback(
  CSTR _Nullable misc_attr_prefix,
  CSTR _Nonnull misc_attr_name,
  ast_node *_Nullable ast_misc_attr_value_list,
  void *_Nullable context)
{
  ast_node *stmt = (ast_node *)context;
  Contract(is_ast_create_proc_stmt(stmt));

  if (misc_attr_prefix &&
      misc_attr_name &&
      !Strcasecmp(misc_attr_prefix, "cql") &&
      !Strcasecmp(misc_attr_name, "autotest")) {
    ...
  }
}
```

The main dispatch looks like this:

```C
// In principle, any option can be combined with any other but some only make sense for procs with
// a result.

EXTRACT_STRING(autotest_attr_name, misc_attr_value);
if (is_autotest_dummy_test(autotest_attr_name)) {
  cg_test_helpers_dummy_test(stmt);
}

// these options are only for procs that return a result set
if (has_result_set(stmt) || has_out_stmt_result(stmt) || has_out_union_stmt_result(stmt)) {
  if (is_autotest_dummy_table(autotest_attr_name)) {
    helper_flags |= DUMMY_TABLE;
    cg_test_helpers_dummy_table(proc_name);
  }
  else if (is_autotest_dummy_insert(autotest_attr_name)) {
    helper_flags |= DUMMY_INSERT;
    cg_test_helpers_dummy_insert(proc_name);
  }
  else if (is_autotest_dummy_select(autotest_attr_name)) {
    helper_flags |= DUMMY_SELECT;
    cg_test_helpers_dummy_select(proc_name);
  }
  else if (is_autotest_dummy_result_set(autotest_attr_name)) {
    helper_flags |= DUMMY_RESULT_SET;
    cg_test_helpers_dummy_result_set(proc_name);
  }
}
```

Most of these options are very simple indeed.   `cg_test_helpers_dummy_test` is the trickiest
by far and we'll save it for last, let's dispense with the easy stuff.

### Dummy Table, Dummy Insert, Dummy Select, Dummy Result Set

All of these are a very simple template.  The language includes just the right features
to emit these procedures as nearly constant strings. The `LIKE` construct was literally
designed to make these patterns super simple.  You can see all the patterns
in [Chapter 12](https://cgsql.dev/cql-guide/ch12) but let's look at the code for
the first one.  This is "dummy table".

```C
// Emit an open proc which creates a temp table in the form of the original proc
// Emit a close proc which drops the temp table
static void cg_test_helpers_dummy_table(CSTR name) {
  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC open_%s()\n", name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  CREATE TEMP TABLE test_%s(LIKE %s);\n", name, name);
  bprintf(cg_th_procs, "END;\n");

  bprintf(cg_th_procs, "\n");
  bprintf(cg_th_procs, "CREATE PROC close_%s()\n", name);
  bprintf(cg_th_procs, "BEGIN\n");
  bprintf(cg_th_procs, "  DROP TABLE test_%s;\n", name);
  bprintf(cg_th_procs, "END;\n");
}
```

The purpose of this is to create helper functions that can create a temporary
table with the same columns in it as the procedure you are trying to mock.
You can then select rows out of that table (with `dummy_select`) or insert
rows into the table (with `dummy_insert`).  Or you can make a single
row result set (often enough) with `dummy_result_set`.

As we can see we simply prepend `open_` to the procedure name and use
that to create a test helper that make the temporary table.  The table's
columns are defined to be `LIKE` the result shape of the procedure under
test.  Recall this helper is only available to procedures that return a result set.
The temporary table gets a `test_` prefix.  Assuming the procedure with the
annotation is `foo` then this code is universal:

```sql
CREATE TEMP TABLE test_foo(LIKE foo);
```

Is universal, no matter the result shape of `foo` you get a table with those columns.

For this to work we need to emit a declaration of `foo` before this code.  However,
since we have the full definition of `foo` handy that is no problem.  We remember
that we'll need it by setting a flag in `helper_flags`.

The code for `close_foo` is even simpler if that's possible.  The great thing is
all need to know the columns of `foo` has been removed from the test helper.  The
CQL compiler handles this as a matter of course and it is generally useful.
See [Chapter 5](https://cgsql.dev/cql-guide/ch05#reshaping-data-cursor-like-forms)
for more examples.

All the others are equally simple and use similar tricks.  These were the first
test helpers.  They're actually not that popular because they are so easy to create
yourself anyway.

### Dummy Test

The dummy test code emitter is non-trivial.  Let's quickly review the things it has to
do and then we can go over how each of these is accomplished.  Assuming we have an procedure
`your_proc` that has been annotated like this:

```SQL
@attribute(cql:autotest=(dummy_test))
create proc your_proc(..args...)
begin
  -- assorted references to tables and views
end;
```

Dummy test will produce the following:

* `test_your_proc_create_tables`
  * a procedure that creates all the tables and views that `your_proc` needs
* `test_your_proc_drop_tables`
  * a procedure that drops those same tables and views
* `test_your_proc_create_indexes`
  * a procedure that creates your indices, in a test you may or may not want to create the indices
* `test_your_proc_drop_indexes`
  * a procedure the drops those same indices
* `test_your_proc_create_triggers`
  * a procedure that creates your trigger, in a test you may or may not want to create the triggers
* `test_your_proc_drop_triggers`
  * a procedure the drops those same triggers
* `test_your_proc_read_table1`
  * for each table or view in the `create_tables` a procedure that selects all the data out of that object is created in case you need it
* `test_your_proc_populate_tables`
  * a procedure that loads all the tables from `create_tables` with sample data
  * FK relationships are obeyed
  * user data may be specified in an attribute and that data will be used in preference to auto-generated data

These are more fully discussed in [Chapter 12](https://cgsql.dev/cql-guide/ch12#generalized-dummy-test-pattern).

#### Building the Trigger and Index mappings

In order to know which indices and triggers we might need we have to be able to map from the tables/views in `your_proc` to the indices.
To set up for this a general purpose reverse mapping is created.  We'll look at the triggers version. The indices version is nearly identical.

```C
// Walk through all triggers and create a dictionnary of triggers per tables.
static void init_all_trigger_per_table() {
  Contract(all_tables_with_triggers == NULL);
  all_tables_with_triggers = symtab_new();

  for (list_item *item = all_triggers_list; item; item = item->next) {
    EXTRACT_NOTNULL(create_trigger_stmt, item->ast);
    EXTRACT_NOTNULL(trigger_body_vers, create_trigger_stmt->right);
    EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
    EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
    EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
    EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
    EXTRACT_ANY_NOTNULL(table_name_ast, trigger_target_action->left);
    EXTRACT_STRING(table_name, table_name_ast);

    if (create_trigger_stmt->sem->delete_version > 0) {
      // dummy_test should not emit deleted trigger
      continue;
    }

    symtab_append_bytes(all_tables_with_triggers, table_name, &create_trigger_stmt, sizeof(create_trigger_stmt));
  }
}
```

The steps are pretty simple:

* we make a symbol table that will map from the table name to an array of statements
* there is a convenient `all_triggers` list that has all the triggers
* from each trigger we `EXTRACT` the table or view name (named `table_name` even if it's a view)
* we append the trigger statement pointer to the end of such statements for the table
* any triggers marked with `@delete` are not included for obvious reasons

At the end of this looking up the table name will give you a list of trigger statement AST pointers.  From there
of course you can everything you need.

The index version is basically the same, the details of the `EXTRACT` ops to go from index to table name are different
and of course we start from the `all_indices_list`

#### Computing The Dependencies of a Procedure

Sticking with our particular example, in order to determine that tables/views that `your_proc` might need,
the generator has to walk its entire body looking for things that are tables.  This is handled by the
`find_all_table_nodes` function.

```C
// Recursively finds all view, triggers and table nodes but also all table to tables relationship
//    and all table to triggers relationship. During the walk through we mark a table visited if
//    all its dependences (foreign key table, tables references in the a view) are marked visited.
//
// found_tables:
//    When a table is marked visited then we add that table on top of the list however by that time
//    the recursion is already complete.  That means the starting items will be at the front of the
//    list and the dependencies will be later.  This is the opposite of the order that could be
//    used to safely create the items but it is the order that we could use to delete the items.
// table_triggers_visited:
//    This is a symbol table to avoid searching the triggers of the same table twice
// node:
//    The ast node to walk through. The node is a create_proc_stmt when this function is first called.
//
// This function is similar to its sister function in cg_commmon.h
//   void find_table_refs(
//     ast_node *_Nonnull node,
//     find_ast_node_callback _Nonnull callback,
//     void *_Nullable callback_context
//
// This version does these things:
//  - looks up views (both do this)
//  - looks up all table relationships instead of just tables reference in a proc (follows the FKs)
//  - looks up drop table statements
//  - looks up triggers, and then the tables referenced in those triggers
//
// Not wanting to change the downstream dependency info generated by find_table_refs and
// because that also affects the JSON output which is used for all kinds of things we
// opt to duplicate some of that logic here creating something unique to dummy test's needs.
static void find_all_table_nodes(dummy_test_info *info, ast_node *node) {
  ...
}
```

This is a recursive method and the general steps for prosecution go something like this:

* starting from `your_proc` the entire body of the procedure is visited
 * references to tables or views in update, delete, insert, select etc. statements are identified
 * each such table/view is added to the found tables list (at most once)
 * for views, the recursion proceeds to the body of the view as though the body had been inline in the procedure
 * if any found item has triggers, the trigger body is walked, any tables/views mentioned there are found items
   * any given trigger is only visited once

The net of all this, the "found items", is a list of all the tables and views that the procedure uses, directly
or indirectly.  As discussed in [Chapter 12](https://cgsql.dev/cql-guide/ch12#generalized-dummy-test-pattern)
this walk does not include tables and views used by procedures that `your_proc` calls.

#### Emitting Indices and Triggers

With the "found tables" computed (creatively stored in a field called `found_tables`) it's very easy to loop over these
and generate the necessary indices for each found table (keeping in mind the "found table" can be a view).

The `create index statement` is emitted by the usual `gen_statement_with_callbacks` form that echos the AST.

The `drop index` can be trivially created by name.

```C
// Emit create and drop index statement for all indexes on a table.
static void cg_emit_index_stmt(
  CSTR table_name,
  charbuf *gen_create_indexes,
  charbuf *gen_drop_indexes,
  gen_sql_callbacks *callback)
{
  symtab_entry *indexes_entry = symtab_find(all_tables_with_indexes, table_name);
  bytebuf *buf = indexes_entry ? (bytebuf *)indexes_entry->val : NULL;
  ast_node **indexes_ast = buf ? (ast_node **)buf->ptr : NULL;
  int32_t count = buf ? buf->used / sizeof(*indexes_ast) : 0;
  gen_set_output_buffer(gen_create_indexes);

  for (int32_t i = 0; i < count; i++) {
    ast_node *index_ast = indexes_ast[i];
    EXTRACT_NOTNULL(create_index_stmt, index_ast);
    EXTRACT_NOTNULL(create_index_on_list, create_index_stmt->left);
    EXTRACT_ANY_NOTNULL(index_name_ast, create_index_on_list->left);
    EXTRACT_STRING(index_name, index_name_ast);

    gen_statement_with_callbacks(index_ast, callback);
    bprintf(gen_create_indexes, ";\n");
    bprintf(gen_drop_indexes, "DROP INDEX IF EXISTS %s;\n", index_name);
  }
}
```

Triggers are done in exactly the same way except that instead of looping over found tables we can
actually generate them as they are discovered inside of `find_all_triggers_node`.  Recal that we
had to visit the triggers when computing the found tables anyway.  We did not have to visit the indices
hence the difference.

These walks allow us to produce: `test_your_proc_create_indexes`, `test_your_proc_drop_indexes`, `test_your_proc_create_triggers`, `test_your_proc_drop_triggers`

#### Emitting Tables and Views

Starting from the found tables, again it is very easy to generate the code to create and drop the tables and views.  The only trick here is that the
tables depend on one another so order is important.  The tables are discovered with the deepest dependency first, new found items are added to the head
of the found tables but it's a post-order walk so that means that the deepest tables/views are at the front of the list.  This means the list
is naturally in the order that it needs to be to delete the tables (parent tables at the end).  So the algorithm goes like this:

* emit the drop tables/views in the found order
* reverse the list
* emit the create tables/views in the reverse order
* for each table/view emit the reader `test_your_proc_read_[item]
* for tables we emit an insertion fragment into `test_your_proc_populate_tables` using `cg_dummy_test_populate`
  * population is discussed in the following sections

As in the other cases `gen_statement_with_callbacks` is used to create the DDL statements:
  * `CREATE TABLE`
  * `CREATE VIEW`
  * `CREATE VIRTUAL TABLE`

The delete side is easily created with ad hoc `DROP TABLE` or `DROP VIEW` statements.

The reading procedure is always of the form `SELECT * FROM foo` so that too is trivial to generate with a fixed template.  The "echoing" system
once again is doing a lot of the heavy lifting.

These walks give us `test_your_proc_create_tables`, `test_your_proc_drop_tables`, and `test_your_proc_read_[item]` and drive the population process

#### Gathering Ad Hoc Data To Be Inserted

Before we get into the mechanics of the population code, we have to visit one more area.  It's possible to include data in the the
`dummy_test` annotaiton itself.  This is data that you want to have populated.  This data will be included in the overall data populator.
If there is enough of it (at least 2 rows per candidate table) then it might be all the data you get.  Now the data format here is
not designed to be fully general, after all it's not that hard to just write `INSERT ... VALUES` for all your tables anyway.  The goal
is to provide something that will help you not have to remember all the FK relationships and maybe let you economically specify some leaf
data you need and get the rest for free.  It's also possible to manually create dummy data that just won't work, again, scrubbing all
this is is way beyond the ability of a simple test helper.  When the code runs you'll get SQLite errors which can be readily addressed.

So keeping in mind this sort of "entry level data support" as the goal, we can take a look at how the system works -- it's all
in the function `collect_dummy_test_info` which includes this helpful comment on structure.

```C
// the data attribute looks kind of like this:
// @attribute(cql:autotest = (
//   .. other auto test attributes
//   (dummy_test,
//     (table_name1, (col1, col2), (col1_val1, col2_val1), (col1_val2, col2_val2) ),
//     (table_name2, (col1, col2), (col1_val1, col2_val1), (col1_val2, col2_val2) ),
//     ...
//   )
//   .. other auto test attributes
// ))
//
// we're concerned with the dummy_test entries here, they have a very specific format
// i.e. first the table then the column names, and then a list of matching columns and values
```

So we're going to walk a list of attributes each one begins with a table name, then a list of columns, and then a list of values.

All of the data is in the symbol table `dummy_test_infos` which is indexed by table name.  For each table name we find
we ensure there is a symbol table at that slot.  So `dummy_test_infos` is a symbol table of symbol tables.  It's actually
going to be something like `value_list = dummy_test_infos['table']['column']`

```C
  // collect table name from dummy_test info
  ast_node *table_list = dummy_attr->left;
  EXTRACT_STRING(table_name, table_list->left);
  symtab *col_syms = symtab_ensure_symtab(dummy_test_infos, table_name);
```

Next we're going to find the column names, they are the next entry in the list so we go `right` to get the `column_name_list`

```C
// collect column names from dummy_test info
ast_node *column_name_list = table_list->right;
for (ast_node *list = column_name_list->left; list; list = list->right) {
  EXTRACT_STRING(column_name, list->left);
  sem_t col_type = find_column_type(table_name, column_name);

  bytebuf *column_values = symtab_ensure_bytebuf(col_syms, column_name);

  // store the column meta data, create space to hold values in databuf
  bytebuf_append_var(&col_data_buf, column_values);
  bytebuf_append_var(&col_type_buf, col_type);
  bytebuf_append_var(&col_name_buf, column_name);
}
```

The primary purpose of this part of the loop is then to add the column names to `col_syms` so that they are linked to the dummy info for this table.
The line `bytebuf *column_values = symtab_ensure_bytebuf(col_syms, column_name);` does this.  And this also creates the byte buffer that will hold
the eventual values.

We also keep a side set of buffers that has the column name, type, and the values in the `col_name`, `col_type`, and `col_data` buffers respectively.
These are used to handle the foreign key work shortly and they allow us to not have to look up all the names over and over.

```
// collect column value from dummy_test info. We can have multiple rows of column value
for (ast_node *values_ast = column_name_list->right;
     values_ast;
     values_ast = values_ast->right) {

  int32_t column_index = 0;

  // collect one row of column value
  for (ast_node *list = values_ast->left; list; list = list->right) {
    ast_node *misc_attr_value = list->left;
    Contract(col_data_buf.used);
    bytebuf *column_values = ((bytebuf **) col_data_buf.ptr)[column_index];
    sem_t column_type = ((sem_t *) col_type_buf.ptr)[column_index];
    CSTR column_name = ((CSTR *) col_name_buf.ptr)[column_index];

    bytebuf_append_var(column_values, misc_attr_value);
    column_index++;

    ...foreign key stuff goes here...
  }
  .. some cleanup
}
```

The most important part is `bytebuf_append_var(column_values, misc_attr_value);` this is where the
attribute value is added to the list of values that are on the column.

Finally, the "foreign key stuff".  What we need to here is check the column name in the table to see if it's part of a foreign
key and if it is we recursively add the current data value to the referenced column in the reference table.  That way
if you add an initalizer to a leaf table you don't also have to add it to all the parent tables.  If it wasn't for this
feature the manual data wouldn't be very useful at all, hand written `INSERT` statements would be just as good.


```C
// If a column value is added to dummy_test info for a foreign key column then
// we need to make sure that same column value is also added as a value in the
// the referenced table's dummy_test info.
// e.g.
//   create table A(id integer primary key);
//   create table B(id integer primary key references A(id));
//
// If there is sample data provided for B.id then we must also ensure that
// the value provided for B.id is also add as a sample row in A with the same
// value for id.
if (is_foreign_key(column_type)) {
  add_value_to_referenced_table(table_name, column_name, column_type, misc_attr_value);
}
```

When this is a done all of the initializers will have been added to the appropriate column of the appropriate table.
Again the overall structure is something like: `value_list = dummy_test_infos['table']['column']`

#### Emitting the Table Population Fragments

With any custom initalizers in the `dummy_test_infos` structure we can do the population fragment for any given table.

The general algorithm here goes like this:

* the total number of rows we will generate will be the number of column values in the initializers or else `DUMMY_TEST_INSERT_ROWS`, whichever is larger
* the insert statement generated will include `dummy_seed([value_seed])` where value_seed starts at 123 and goes up 1 for every row generated
  * dummy_seed will create values for any missing columns using the seed so any combination of included columns is ok, we'll always get a complete insert
* foreign key columns use a provided intializer from the parent table if there is one, or else they use 1, 2, 3 etc.
  * likewise if a column is referenceable by some other table it uses the known sequence 1, 2, 3 etc. for its value rather than the varying seed
  * in this way child tables can know that partent tables will have a value they can use since both tables will have at least `DUMMY_TEST_INSERT_ROWS` and any rows that were not manually initialized will match
  * note that foreign key columns always get this treatment, whether they were mentioned or not
* to mix things up the `dummy_nullables` and `dummy_defaults` are added on every other row which makes missing values be NULL and/or the default value if one is present

This is enough to generate a set of insert statements for the table in question and since the fragments are generated in the table creation order the resulting insert statements will have the parent tables first so the foreign keys of later tables will be correct.

This can go wrong if the manual initializations use keys that conflict with the default generation or if the manual intializations have PK conflicts or other such things.  No attempt is made to sort that out.  The run time errors should be clear and these are, after all, only test helpers.  It's very easy to avoid these hazards
and you get a pretty clear error message if you don't so that seems good enough.

These fragments are ultimately combined to make the body of the procedure `test_your_proc_populate_tables`.

### Recap

The test helpers in `cg_test_helpers.c` are very simple nearly-constant templates with the exception of `dummy_test` which includes:

* table and view creation
* index creation
* trigger creation
* data population

Topics covered included:

* how the candidate procedures are discovered
* how the attributes are scanned for test directives
* how each dummy test type is dispatched
* how `dummy_test` handles data initialization
* how `dummy_test` does its dependency analysis

As with the other parts, no attempt was made to cover every function in detail.  That is
best done by reading the source code. But there is overall structure here and an understanding
of the basic principles is helpful before diving into the source code.

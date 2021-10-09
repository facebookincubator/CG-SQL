---
id: int08
title: "Part 8: Test Helpers""
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

the hard stuff...

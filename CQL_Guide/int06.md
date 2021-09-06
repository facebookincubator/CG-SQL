---
id: int06
title: "Part 6: Schema Management"
sidebar_label: "Part 6: Schema Management"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 6 continues with a discussion of the essentials schema management in the CQL compiler.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how schema management happens in general -- the core strategies and implementation choices --
so that when reading the management code you will have an idea how it all hangs together. To accomplish
this, various key data structures will be explained in detail as well as selected examples of their use.

## Schema Management

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

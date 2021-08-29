---
id: int04
title: "Part 4: Testing"
sidebar_label: "Part 4: Testing"
---
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
* do the pattern matching on the output to ensure the patterns match (discussed below)
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

In the next phase we're doing to do some pattern matching, let's look at a couple of examples
to illustrate how this works.  The program `tester` actually does all this and that program
is itself written in (mostly) CQL which is cute.

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

[Examples to come but the short answer is that it's just like the semantic tests except we're scanning
codegen instead of an AST.  The same patterns work in the same way for the same reason]


### Run Tests

Coming soon.

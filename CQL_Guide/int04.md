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

### Parse Tests
...

### Sematic Tests
...

### Code Generation Tests
...

### Run Tests
...

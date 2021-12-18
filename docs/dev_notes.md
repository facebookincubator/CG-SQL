---
id: dev-notes
title: Developer Notes on CQL Development
sidebar_label: Developer Notes
---
1. If you aren't good with `yacc`/`lex` you probably should do some homework before you start. CQL development is all about building and walking a syntax tree.  It's possible to make local changes without knowing the details but it can be hard to figure out where to make changes without context.
2. CQL development is basically test driven, to create a new feature:
   1. Add the language feature to `test.sql`
   2. run `test.sh`; it will fail due to parse error
   3. Add the syntax to `cql.y` and create the necessary tree pieces in `ast.h`
   4. run `test.sh`; accept any file differences to install this as the new reference baseline.
   5. Add a test case to `sem_test.sql` that uses your new feature. `sem_test.sql` can contain pattern matching for the semantic output.
   6. run `test.sh`; it will fail because it will find an AST node it doesn't understand
   7. edit `sem.c` to do the analysis for your new node type
   8. adjust the verification in `sem_test.sql` accordingly
   9. run `test.sh` until it passes making fixes as needed
   10. there will be new diff output and it will be spitting out the diffs; if you are happy with the new output, accept the diffs to update the reference outputs; note the pattern matching validations will still fail if the output goes bad even if the reference comparison is good, the reference output is a double check
   11. add code that uses your new feature to `cg_test.sql`, this is the code gen test, verifications using pattern matching are also allowed there
   12. run `test.sh`
   13. it will fail because codegen doesn't know about your new feature
   14. edit `cg_c.c` (or a different code gen if you're doing test helpers or some such) to support your new code
   15. cycle running `test.sh` until it passes
   16. accept each diff when you're happy with the new output
   17. Add code that runs your new feature using run_test.sql
   18. Run `test.sh`, if your codegen was perfect it could pass; it probably won't at first
   19. fix your code until it's done; you shouldn't need to accept any more diffs at this point
   20. run `cov.sh` to confirm 100% coverage
   21. sanity check the GCC build (I use a linux box for this)

3. Get a solid code review and land as usual.

By the time you have done this you will have passed the tests dozens of times and you will know exactly what your code is doing to the entire battery of cql combinations.  Missing tests can be painful and cause downstream regressions so be ruthless about adding enough combinations and validating the essential parts.  The snapshot diffing is helpful but the real gating is done by the pattern matching logic.


Note: none of this works unless you are standing the main source directory

Note: the test scripts make a lot of turds, at this point almost everything should be going into the `out`
directory but it wasn't always so.  You can use `make clean` to get rid of the build stuff wherever it may be.
Alternatively use source control to get rid of any junk.

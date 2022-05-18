---
id: code-coverage
title: Code Coverage CG/SQL
sidebar_label: Code Coverage CG/SQL
---
:::note
Due to issues that we don't currently understand, `gcovr` on macOS with the latest XCode tools is not working.
Consequently, the following script fails on macOS.  See https://github.com/facebookincubator/CG-SQL/issues/92 for
a more complete discussion.  The problem doesn't seem to have anything to do with CG/SQL per se, but there it is...
:::

Run this command in the [/sources](https://github.com/facebookincubator/CG-SQL/tree/main/sources) directory:
```
./cov.sh
```

This will run the test scripts with the coverage flag, which causes the coverage build.
If the tests pass a coverage report is created.

The same build options are available as `cov.sh` uses `test.sh` to do the heavy lifting.

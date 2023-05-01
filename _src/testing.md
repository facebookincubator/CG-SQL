---
id: testing
title: Testing CG/SQL
sidebar_label: Testing CG/SQL
---

Run this command in the [/sources](https://github.com/facebookincubator/CG-SQL/tree/main/sources) directory:
```
./test.sh
```

This will build and run the test suite

```
./test.sh --use_amalgam
```

Does the same thing but it tests the built amalgam rather than the normal build

> See details in our [CQL Internals documentation](/cql-guide/int04#testing).

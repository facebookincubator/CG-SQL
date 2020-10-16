---
id: building
title: Building CG/SQL
sidebar_label: Building CG/SQL
---
```
make clean
make
```

This puts the result in `out/cql`

## Options

* If you add `CGSQL_GCC` to your environment the `Makefile` will add `CFLAGS += -std=c99`
to try to be more interoperable with gcc.

* If you add `SQLITE_PATH` to your environment the `Makefile` will try to compile `sqlite3-all.c` from that path
and it will link that in instead of using `-lsqlite3`.

# Amalgam Build

The amalgam is created by `./make_amalgam.sh` and the result is in `out/cql_amalgam.c`

You can create and test the amalgam in one step (preferred) using

```
./test.sh --use_amalgam
```

This will cause the amalgam to be created and compiled.  Then the test suite will run against that binary.

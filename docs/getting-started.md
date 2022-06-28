---
id: getting-started
title: Getting Started with CG/SQL
sidebar_label: Getting Started
---

## Building
:::caution
Please make sure you meet the [requirements](#requirements).
:::

Set your current directory to the CG/SQL [`sources`](https://github.com/facebookincubator/CG-SQL/tree/main/sources) directory, wherever that may be, then:

```
make clean
make
```

This compiles CQL and puts the result at `out/cql`. Now you can run it to show available command options (also [documented here](https://cgsql.dev/cql-guide/x1)):
```bash
$ out/cql
```

You might want to alias the location of `out/cql`. For example, by using the `alias` command in Linux or MacOS.

## Next Steps

- Go to the [first chapter of the CQL Guide](/cql-guide/ch01#getting-started) to write your first CQL program!
The [second chapter](/cql-guide/ch02#a-sample-program) has a less trivial program that walks through how to query a SQLite database with CQL.
- [CGL Language Cheatsheet](/cql-guide/x6)
- [CQL Playground](/docs/playground)

## Requirements

### MacOS Users
The default bison and flex on Mac are quite old.  You'll need to replace them. The Build
produces an error if this is happening.  You can get a more recent versions like this:

```
  brew install bison
  brew link bison --force
  brew install flex
  brew link flex --force
```

### Linux Users
The default SQLite on Ubuntu systems is also fairly old.  Some of the tests (particularly
the query plan tests) use features not available in this version.  You'll want to link
against a newer sqlite to pass all the tests.

From a bare Ubuntu installation, you might need to add these components:

sudo apt install

* make
* gcc
* flex
* bison
* sqlite3
* libsqlite3-dev

After which I was able to do the normal installations.

For the coverage build you need
* gcovr

And if you want to do the AST visualizations in PNG form you need
* graphviz

## Options

* If you add `CGSQL_GCC` to your environment the `Makefile` will add `CFLAGS += -std=c99`
to try to be more interoperable with gcc.

* If you add `SQLITE_PATH` to your environment the `Makefile` will try to compile `sqlite3-all.c` from that path
and it will link that in instead of using `-lsqlite3`.

## Amalgam Build

The amalgam is created by `./make_amalgam.sh` and the result is in `out/cql_amalgam.c`

You can create and test the amalgam in one step (preferred) using

```
./test.sh --use_amalgam
```

This will cause the amalgam to be created and compiled.  Then the test suite will run against that binary.

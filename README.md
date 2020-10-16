## Licensing

See `LICENSE` in the root directory

## Documentation

See `CQL_Guide/guide.html` for the language summary.

## Building

With requirements met (see below)

```
make clean
make
```

This puts the result in `out/cql`


### Requirements
The default bison on Mac is quite old (v2.3).  You'll need to replace it. The Build
produces an error if this is happening.  You can get a more recent bison like this:

```
  brew install bison
  brew link bison --force
```

The default SQLite on Ubuntu systems is also fairly old.  Some of the tests (particularly
the query plan tests) use features not available in this version.  You'll want to link
against a newer sqlite to pass all the tests.

### Options

* If you add `CGSQL_GCC` to your environment the `Makefile` will add `CFLAGS += -std=c99 -D_Nullable= -D_Nonnull=`
to try to be more interoperable with gcc.

* If you add `SQLITE_PATH` to your environment the `Makefile` will try to compile `sqlite3-all.c` from that path
and it will link that in instead of using `-lsqlite3`.

## Testing

```
./test.sh
```

This will build and run the test suite

```
./test.sh --use_amalgam
```

Does the same thing but it tests the built amalgam rather than the normal build

## Code Coverage

```
./cov.sh
```

This will run the test scripts with the coverage flag, which causes the coverage build.
If the tests pass a coverage report is created.

The same build options are available as `cov.sh` uses `test.sh` to do the heavy lifting.

## Amalgam Build

The amalgam is created by `./make_amalgam.sh` and the result is in `out/cql_amalgam.c`

You can create and test the amalgam in one step (preferred) using

```
./test.sh --use_amalgam
```

This will cause the amalgam to be created and compiled.  Then the test suite will run against that binary.

## Dev Cycle

See `docs/dev_notes.md` for workflow tips

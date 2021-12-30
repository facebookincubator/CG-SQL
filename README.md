## Licensing

See `LICENSE` in the root directory

## Documentation

See `CQL_Guide/guide.html` for the language summary.

## Building

With requirements met (see below)

Set your current directory to the CG/SQL `sources` directory, wherever that may be, then:

```
make clean
make
```

This puts the result in `out/cql`

### Requirements
The default bison and flex on Mac are quite old.  You'll need to replace them. The Build
produces an error if this is happening.  You can get a more recent versions like this:

```
  brew install bison
  brew link bison --force
  brew install flex
  brew link flex --force
```

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

### Options

* If you add `CGSQL_GCC` to your environment the `Makefile` will add `CFLAGS += -std=c99
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

NOTE: Due to issues that we don't currently understand, `gcovr` on macOS with the latest XCode tools is not working.
Consequently, the following script fails on macOS.  See https://github.com/facebookincubator/CG-SQL/issues/92 for
a more complete discussion.  The problem doesn't seem to have anything to do with CG/SQL per se, but there it is...

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

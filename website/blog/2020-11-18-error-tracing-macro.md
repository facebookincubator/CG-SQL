---
slug: error-tracing-macro
title: Error Tracing Helper Macro
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql, errors]
---

Following up on the last blog entry, I thought it would be useful to present a simple error tracing macro that you can use
to see what kind of error flow is going on when you're having trouble understanding why a procedure is returning
an error code. The idea is we want to create a macro that we can use like this:

```
BEGIN_VERBOSE_STDERR_TRACING;

-- Some procedure(s) that you want to trace

END_VERBOSE_STDERR_TRACING;
```

We can do that with something like the below macros.  These particular ones cause the output to go to `stderr` via `fprintf` but if that isn't what you need you can simply edit the macro. The macros looks like this:

```
-- manually force tracing on by redefining the cql_error_trace macro
#define BEGIN_VERBOSE_STDERR_TRACING \
    @echo c, "#undef cql_error_trace\n"; \
    @echo c, "#define cql_error_trace() fprintf(stderr, \"CQL Trace at %s:%d in %s: %d %s\\n\", __FILE__, __LINE__, _PROC_, _rc_, sqlite3_errmsg(_db_))\n"

#define END_VERBOSE_STDERR_TRACING \
    @echo c, "#undef cql_error_trace\n"; \
    @echo c, "#define cql_error_trace()\n"
```


So basically it's telling CQL to emit a `#define` into its output stream.  In this case:

```
#define cql_error_trace() fprintf(stderr, "CQL Trace at %s:%d in %s: %d %s\n", __FILE__, __LINE__, _PROC_, _rc_, sqlite3_errmsg(_db_))
```

You could change that to any function you like, you can have it dump the errors where you like, or you can make it some dummy function you add so that you can set a breakpoint on it.

Whatever you do, do not leave your code with this sort of tracing enabled -- it's far too expensive in terms of code size.  But it's perfect if you have this one procedure that is failing and it's hard for you to see where.

Obviously if you're making a custom trace thingy you don't need the macro at all, you can just emit your own `#define` with `@echo` as needed.

Note: `@echo` is quite a sledgehammer so don't use it lightly and not in production code but it is quite helpful for this sort of thing.  CQL tests often use it to help make things visible to the tests.  If you use `@echo` in weird ways you might not get working code when the codegen changes in the future.

The relevant state that is available to you inside a macro like this is:

* `__FILE__` the current filename (comes from the C pre-processor, this is the .c file name not the .sql)
* `__LINE__` the current line number (comes from the C pre-processor, this is the .c line number)
* `_rc_` the current SQLite result code (always the current return code in every CQL procedure that uses SQLite)
* `_db_` the current SQLite database pointer (always the current database in every CQL procedure that uses SQLite)
* `_PROC_` the current procedure name (CQL has a `#define` for this for you)

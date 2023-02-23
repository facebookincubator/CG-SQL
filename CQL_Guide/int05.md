---
id: int05
title: "Part 5: CQL Runtime"
sidebar_label: "Part 5: CQL Runtime"
---
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->
### Preface

Part 5 continues with a discussion of the essentials of the CQL Runtime.
As in the previous sections, the goal here is not to go over every detail but rather to give
a sense of how the runtime works in general -- the core strategies and implementation choices --
so that when reading the source you will have an idea how it all hangs together. To accomplish
this, we'll illustrate the key pieces that can be customized and we'll discuss some
interesting cases.

## CQL Runtime

The parts of the runtime that you can change are in `cqlrt.h`, that file invariably ends by including
`cqlrt_common.h` which are the runtime parts that you shouldn't change.  Of course this is open source
so you can change anything, but the common things usually don't need to change -- `cqlrt.h` should
provide you with everything you need to target new environments.

The compiler itself can be customized see `rt.c` to emit different strings to work with your runtime.
This is pretty easy to do without creating a merge hell for yourself. Meta Platforms, for instance,  has its
own CQL runtime customized for use on phones that is not open source (and really I don't think anyone
would want it anyway).  But the point is that you can make your own. In fact I know of two just within
Meta Platforms.

We'll go over `cqlrt.h` bit by bit.  Keeping in mind it might change but this is
essentially what's going on.  And the essentials don't change very often.

### Standard headers

The rest of the system will use these, `cqlrt.h` is responsible for bringing in what you need
later, or what `cqlrt_common.h` needs on your system.

```c
#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <math.h>
#include <sqlite3.h>

#ifndef __clang__
#ifndef _Nonnull
    /* Hide Clang-only nullability specifiers if not Clang */
    #define _Nonnull
    #define _Nullable
#endif
#endif
```

### Contract and Error Macros

CQL has a few different macros it uses for errors.  `contract`, `invariant`, and `tripwire`
usually all map to `assert`.  Note that `tripwire` doesn't have to be fatal, it can log
in production and continue.  This is a "softer" assertion.  Something that you're trying out
that you'd like to be a `contract` but maybe there are lingering cases that have to be fixed
first.

```c
#define cql_contract assert
#define cql_invariant assert
#define cql_tripwire assert
#define cql_log_database_error(...)
#define cql_error_trace()
```

### The Value Types

You can define these types to be whatever is appropriate on your system.
Usually the mapping is pretty obvious.

```c
// value types
typedef unsigned char cql_bool;
#define cql_true (cql_bool)1
#define cql_false (cql_bool)0

typedef unsigned long cql_hash_code;
typedef int32_t cql_int32;
typedef uint32_t cql_uint32;
typedef uint16_t cql_uint16;
typedef sqlite3_int64 cql_int64;
typedef double cql_double;
typedef int cql_code;
```

### The Reference Types

The default runtime first defines 4 types of reference objects.
These are the only reference types that CQL creates itself. In
fact CQL doesn't actually create `CQL_C_TYPE_OBJECT` but the tests
do.  CQL never creates raw object things, only external functions
can do that.

```c
// metatypes for the straight C implementation
#define CQL_C_TYPE_STRING 0
#define CQL_C_TYPE_BLOB 1
#define CQL_C_TYPE_RESULTS 2
#define CQL_C_TYPE_BOXED_STMT 3
#define CQL_C_TYPE_OBJECT 4
```

All the reference types are reference counted. So they
need a simple shape that allows them to know their own
type and have a count.  They also have a finalize method
to clean up their memory when the count goes to zero.

You get to define `cql_type_ref` to be whatever you want.

```c
// base ref counting struct
typedef struct cql_type *cql_type_ref;
typedef struct cql_type {
  int type;
  int ref_count;
  void (*_Nullable finalize)(cql_type_ref _Nonnull ref);
} cql_type;
```

Whatever you do with the types you'll need to define
a retain and release method that uses them as the signature.
Normal references should have a generic value comparison and a hash.

```c
void cql_retain(cql_type_ref _Nullable ref);
void cql_release(cql_type_ref _Nullable ref);

cql_hash_code cql_ref_hash(cql_type_ref _Nonnull typeref);
cql_bool cql_ref_equal(cql_type_ref _Nullable typeref1, cql_type_ref _Nullable typeref2);
```

Now each of the various kinds of reference types needs an
object which probably includes the base type above.  It doesn't
have to.  You can arrange for some other universal way to do
these.  On iOS these can be easily mapped to `CF` types.

The `retain` and `release` macros should all map to the same thing.
The compiler emits different variations for readability only. It
doesn't really work if they don't have common retain/release
semantics.

```c
// builtin object
typedef struct cql_object *cql_object_ref;
typedef struct cql_object {
  cql_type base;
  const void *_Nonnull ptr;
} cql_object;

#define cql_object_retain(object) cql_retain((cql_type_ref)object);
#define cql_object_release(object) cql_release((cql_type_ref)object);
```

Boxed statement gets its own implementation, same as object.

```
// builtin statement box
typedef struct cql_boxed_stmt *cql_boxed_stmt_ref;
typedef struct cql_boxed_stmt {
  cql_type base;
  sqlite3_stmt *_Nullable stmt;
} cql_boxed_stmt;
```

Same for blob, and blob has a couple of additional helper macros
that are used to get information. Blobs also have hash and equality
functions.

```c
// builtin blob
typedef struct cql_blob *cql_blob_ref;
typedef struct cql_blob {
  cql_type base;
  const void *_Nonnull ptr;
  cql_uint32 size;
} cql_blob;
#define cql_blob_retain(object) cql_retain((cql_type_ref)object);
#define cql_blob_release(object) cql_release((cql_type_ref)object);
cql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull data, cql_uint32 size);
#define cql_get_blob_bytes(data) (data->ptr)
#define cql_get_blob_size(data) (data->size)
cql_hash_code cql_blob_hash(cql_blob_ref _Nullable str);
cql_bool cql_blob_equal(cql_blob_ref _Nullable blob1, cql_blob_ref _Nullable blob2);
```

Strings are the same as the others but they have many more functions
associated with them.

```c
// builtin string
typedef struct cql_string *cql_string_ref;
typedef struct cql_string {
  cql_type base;
  const char *_Nullable ptr;
} cql_string;
cql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr);
#define cql_string_retain(string) cql_retain((cql_type_ref)string);
#define cql_string_release(string) cql_release((cql_type_ref)string);
```

The compiler uses this macro to create a named string literal. You decide
how those will be implemented right here.

```c
#define cql_string_literal(name, text) \
  cql_string name##_ = { \
    .base = { \
      .type = CQL_C_TYPE_STRING, \
      .ref_count = 1, \
      .finalize = NULL, \
    }, \
    .ptr = text, \
  }; \
  cql_string_ref name = &name##_
```

Strings get assorted comparison and hashing functions. Note blob also had a hash.

```c
int cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
cql_hash_code cql_string_hash(cql_string_ref _Nullable str);
cql_bool cql_string_equal(cql_string_ref _Nullable s1, cql_string_ref _Nullable s2);
int cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);
```

Strings can be converted from their reference form to standard C form. These
macros define how this is done.  Note that temporary allocations are possible
here but the standard implementation does not actually need to do an alloc.  It
stores UTF8 in the string pointer so it's ready to go.

```c
#define cql_alloc_cstr(cstr, str) const char *_Nonnull cstr = (str)->ptr
#define cql_free_cstr(cstr, str) 0
```

The macros for result sets have somewhat less flexibility.  The main thing
that you can do here is add additional fields to the "meta" structure.  It
needs those key fields because it is created by the compiler.  However the
API is used to create a result set so that can be any object you like.  It
only has to respond to the `get_meta`, `get_data`, and `get_count` apis.
Those can be mapped as you desire.  In principle there could have been
a macro to create the "meta" as well (a PR for this is welcome) but it's
really a pain for not much benefit.  The advantage of defining your own "meta"
is that you can use it to add additional custom APIs to your result set that
might need some storage.

The additional API `cql_result_set_note_ownership_transferred(result_set)`
is used in the event that you are moving ownership of the buffers from
out of CQL's universe.  So like maybe JNI is absorbing the result, or
Objective C is absorbing the result.  The default implementation is a no-op.

```c
// builtin result set
typedef struct cql_result_set *cql_result_set_ref;

typedef struct cql_result_set_meta {
 ...
}

typedef struct cql_result_set {
  cql_type base;
  cql_result_set_meta meta;
  cql_int32 count;
  void *_Nonnull data;
} cql_result_set;

#define cql_result_set_type_decl(result_set_type, result_set_ref) \
  typedef struct _##result_set_type *result_set_ref;

cql_result_set_ref _Nonnull cql_result_set_create(
  void *_Nonnull data,
  cql_int32 count,
  cql_result_set_meta meta);

#define cql_result_set_retain(result_set) cql_retain((cql_type_ref)result_set);
#define cql_result_set_release(result_set) cql_release((cql_type_ref)result_set);
#define cql_result_set_note_ownership_transferred(result_set)
#define cql_result_set_get_meta(result_set) (&((cql_result_set_ref)result_set)->meta)
#define cql_result_set_get_data(result_set) ((cql_result_set_ref)result_set)->data
#define cql_result_set_get_count(result_set) ((cql_result_set_ref)result_set)->count
```

### Mocking

The CQL run test needs to do some mocking.  This bit is here for that test.  If you
want to use the run test with your version of `cqlrt` you'll need to define a
shim for `sqlite3_step` that can be intercepted.  This probably isn't going to come up.

```c
#ifdef CQL_RUN_TEST
#define sqlite3_step mockable_sqlite3_step
SQLITE_API cql_code mockable_sqlite3_step(sqlite3_stmt *_Nonnull);
#endif
```

### Profiling

If you want to support profiling you can implement `cql_profile_start` and `cql_profile_stop`
to do whatever you want.  The CRC uniquely identifies a procedure (you can log that).  The
`index` provides you with a place to store something that you can use as a handle in
your logging system.  Typically an integer.  This lets you assign indices to the procedures
you actually saw in any given run and then log them or something like that.  No data
about parameters is provided, this is deliberate.

```c
// No-op implementation of profiling
// * Note: we emit the crc as an expression just to be sure that there are no compiler
//   errors caused by names being incorrect.  This improves the quality of the CQL
//   code gen tests significantly.  If these were empty macros (as they once were)
//   you could emit any junk in the call and it would still compile.
#define cql_profile_start(crc, index) (void)crc; (void)index;
#define cql_profile_stop(crc, index)  (void)crc; (void)index;
#define cql_profile_index_declaration(index) static int32_t index;
```

The definitions in `cqlrt_common.c` can provide codegen than either has generic
"getters" for each column type (useful for JNI) or produces a unique getter that isn't
shared.  The rowset metadata will include the values for `getBoolean`, `getDouble` etc.
if `CQL_NO_GETTERS` is 0.  Getters are a little slower for C but give you a small number
of functions that need to have JNI if you are targeting Java.

```c
// the basic version doesn't use column getters
#define CQL_NO_GETTERS 1
```

### Encoding of Sensitive Columns

By setting an attribute on any procedure that produces a result set you can
have the selected sensitive values encoded.  If this happens CQL first asks
for the encoder and then calls the encode methods passing in the encoder.
These aren't meant to be cryptograhically secure but rather to provide some
ability to prevent mistakes.  If you opt in, sensitive values have to be deliberately
decoded and that provides an audit trail.

The default implementation of all this is a no-op.

```c
// implementation of encoding values. All sensitive values read from sqlite db will
// be encoded at the source. CQL never decode encoded sensitive string unless the
// user call explicitly decode function from code.
cql_object_ref _Nullable cql_copy_encoder(sqlite3 *_Nonnull db);
cql_bool cql_encode_bool(...)
cql_int32 cql_encode_int32(...)
cql_int64 cql_encode_int64(...)
cql_double cql_encode_double(...)
cql_string_ref _Nonnull cql_encode_string_ref_new(...);
cql_blob_ref _Nonnull cql_encode_blob_ref_new(..);
cql_bool cql_decode_bool(...);
cql_int32 cql_decode_int32(...);
cql_int64 cql_decode_int64(...);
cql_double cql_decode_double(...);
cql_string_ref _Nonnull cql_decode_string_ref_new(...);
cql_blob_ref _Nonnull cql_decode_blob_ref_new(...);
```

### The Common Headers

The standard APIs all build on the above, so they should be included last.

Now in some cases the signature of the things you provide in `cqlrt.h` is basically fixed,
so it seems like it would be easier to move the prototpyes into `cqlrt_common.h`.
However, in many cases additional things are needed like `declspec` or `export` or
other system specific things.  The result is that `cqlrt.h` is maybe a bit more
verbose that it strictly needs to be.  Also some versions of cqlrt.h choose to
implement some of the APIs as macros...

```c
// NOTE: This must be included *after* all of the above symbols/macros.
#include "cqlrt_common.h"
```

### The `cqlrt_cf` Runtime

In order to use the Objective-C code-gen (`--rt objc`) you need a runtime that has reference
types that are friendly to Objective-C.  For this purpose we created an open-source
version of such a runtime: it can be found in the `sources/cqlrt_cf` directory.
This runtime is also a decent example of how much customization you can do with just
a little code. Some brief notes:

* This runtime really only makes sense on macOS, iOS, or maybe some other place that Core Foundation (`CF`) exists
  * As such its build process is considerably less portable than other parts of the system
* The CQL reference types have been redefined so that they map to:
   * `CFStringRef` (strings)
   * `CFTypeRef` (objects)
   * `CFDataRef` (blobs)
* The key worker functions use `CF`, e.g.
   * `cql_ref_hash` maps to `CFHash`
   * `cql_ref_equal` maps to `CFEqual`
   * `cql_retain` uses `CFRetain` (with a null guard)
   * `cql_release` uses `CFRelease` (with a null guard)
* Strings use `CF` idioms, e.g.
   * string literals are created with `CFSTR`
   * C strings are created by using `CFStringGetCStringPtr` or `CFStringGetCString` when needed

Of course, since the meaning of some primitive types has changed, the contract to the CQL generated
code has changed accordingly.  For instance:

* procedures compiled against this runtime expect string arguments to be `CFStringRef`
* result sets provide `CFStringRef` values for string columns

The consequence of this is that the Objective-C code generation `--rt objc` finds friendly
contracts that it can freely convert to types like `NSString *` which results in
seamless integration with the rest of an Objective-C application.

Of course the downside of all this is that the `cqlrt_cf` runtime is less portable.  It can only go
where `CF` exists.  Still, it is an interesting demonstration of the flexablity of the system.

The system could be further improved by creating a custom result type (e.g. `--rt c_cf`) and using
some of the result type options for the C code generation. For instance, the compiler could do these things:

  * generate `CFStringRef foo;` instead of `cql_string_ref foo;` for declarations
  * generate `SInt32 an_integer` instead of `cql_int32 an_integer`

Even though `cqlrt_cf` is already mapping `cql_int32` to something compatible with `CF`,
making such changes would make the C output a little bit more `CF` idiomatic. This educational
exercise could probably be completed in just a few minutes by interested readers.

The `make.sh` file in the `sources/cqlrt_cf` directory illustrates how to get CQL to use
this new runtime.  The demo itself is a simple port of the code in [Appendix 10](https://cgsql.dev/cql-guide/x10).

### Recap

The CQL runtime, `cqlrt.c`, is intended to be replaced.  The version that ships with the distribution
is a simple, portable implementation that is single threaded. Serious users of CQL will likely
want to replace the default version of the runtime with something more tuned to their use case.

Topics covered included:

* contract, error, and tracing macros
* how value types are defined
* how reference types are defined
* mocking (for use in a test suite)
* profiling
* encoding of sensitive columns
* boxing statements
* the `cqlrt_cf` runtime

As with the other parts, no attempt was made to cover every detail.  That is
best done by reading the source code.

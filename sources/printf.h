/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SEM)

// minimal stuff goes here (none at this point)

#else

//
// A `printf_iterator` allows for analysis of SQLite printf format strings. It
// should be used in the following manner:
//
//   1. Allocate `sizeof_printf_iterator` bytes of memory.
//   2. Initialize it with `printf_iterator_init`.
//   3. Call `printf_iterator_next` to get the type of each substitution until
//      either `SEM_TYPE_OK` (indicating success and no more substitutions) or
//      `SEM_TYPE_ERROR` (indicating a problem with the format string).
//   4. Make no futher calls to `printf_iterator_next`.
//
// Attempts have been made to capture the vast majority of possible errors that
// can exist in a SQLite format string, including many "harmless" cases that
// could nevertheless be surprising or confusing. Certain features of SQLite
// format strings that do not make sense in the presence of CQL have been
// disallowed.
//

#include "cql.h"
#include "ast.h"
#include "sem.h"

// The opaque type of the iterator.
typedef struct printf_iterator printf_iterator;

// To create a `printf_iterator`, allocate `sizeof_printf_iterator` bytes of
// memory and then call `printf_iterator_init` to initialize it.
extern size_t sizeof_printf_iterator;

// Initializes a `printf_iterator` with an optional `format_strlit` (used for
// reporting errors) and a decoded format string (i.e., the format string absent
// any surrounding quotes). This must be called before `printf_iterator_next`.
cql_noexport void printf_iterator_init(printf_iterator *iterator, ast_node *format_strlit, CSTR format_string);

// Attempts to parse the next substitution in the format string returning one
// of the following:
//
//   - A core type if a substitution was found:
//     - `SEM_TYPE_INTEGER`
//     - `SEM_TYPE_LONG_INTEGER`
//     - `SEM_TYPE_REAL`
//     - `SEM_TYPE_STRING`
//
//   - `SEM_TYPE_OK` if the format string was parsed successfully and no
//      substitutions remain.
//
//   - `SEM_TYPE_ERROR` if an error was encountered.
//
// In the lattermost two cases, parsing is finished and `printf_iterator_next`
// must not be called again.
cql_noexport sem_t printf_iterator_next(printf_iterator *iterator);

#endif

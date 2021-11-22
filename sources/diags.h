/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// enable lots of extra warnings for cql

#pragma clang diagnostic error "-Wall"

// in case your compiler doesn't have some of the below
#pragma clang diagnostic ignored "-Wunknown-warning-option"

#pragma clang diagnostic error "-Wduplicate-method-match"
#pragma clang diagnostic error "-Wparentheses"
#pragma clang diagnostic error "-Wreturn-type"
#pragma clang diagnostic error "-Wshadow"
#pragma clang diagnostic error "-Wunguarded-availability"
#pragma clang diagnostic error "-Wuninitialized"
#pragma clang diagnostic error "-Wunknown-pragmas"
#pragma clang diagnostic error "-Wimplicit-int-conversion"
#pragma clang diagnostic error "-Wshorten-64-to-32"
#pragma clang diagnostic error "-Wsign-conversion"
#pragma clang diagnostic error "-Wconversion"

#ifdef NDEBUG
// Many symbols are "unused" becuase they only appear in asserts, so we have to suppress
// those warnings in a non-debug build.
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wunused-function"
#else
// The debug build should be clean
#pragma clang diagnostic error "-Wunused-variable"
#pragma clang diagnostic error "-Wunused-function"
#endif

#ifndef __clang__
#ifndef _Nonnull
    /* Hide Clang-only nullability specifiers if not Clang */
    #define _Nonnull
    #define _Nullable
#endif
#endif

#if defined(CQL_AMALGAM_LEAN)
// in this version there are going to be unused stubs a-plenty
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wunused-variable"
#endif

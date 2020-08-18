#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# We do this cheesy business so that the @ and generated are never together in the script
# if they were it wuold look like the script is auto generated and it isn't.
echo "// (c) Facebook, Inc. and its affiliates. Confidential and proprietary." >out/cql_amalgam.c
echo -n "// @" >>out/cql_amalgam.c

# We now resume normality starting from the trailing @ with no newline
# the generated line needs to be first, the rest is normal source order.
cat <<EOF >>out/cql_amalgam.c
generated SignedSource<<898e90290e7f0a026553a4ccbe6b0bd8>>

#ifndef CQL_NO_SYSTEM_HEADERS

// If you are a different environment you might not want all of these
// or you might want different ones.  You can include your own preamble
// to get the system symbols or you can adjust it.  Simply copy these
// headers as a starting point and then make your equivalents.

#include <alloca.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#endif

#define CQL_AMALGAM 1

typedef uint8_t bool_t;
typedef long long int llint_t;
typedef const char *CSTR;

// In the amalgam build, most things do not need to be visible.  Only the
// API exported by the amalgam should be extern so as to avoid contamination
// of the namespace.

#define cql_noexport static
#define cql_export extern

// the declaration will always come first and once, that becomes the new definition
#define cql_data_decl(x) static x;

// the definition is redundant, strip it
#define cql_data_defn(x)

// map the global yy parser variables and functions to something not likely to conflict

#define yy_flex_debug cql_yy_flex_debug
#define yychar cql_yychar
#define yyin cql_yyin
#define yyleng cql_yyleng
#define yylineno cql_yylineno
#define yylval cql_yylval
#define yynerrs cql_yynerrs
#define yyout cql_yyout
#define yytext cql_yytext

#define yy_create_buffer cql_yy_create_buffer
#define yy_delete_buffer cql_yy_delete_buffer
#define yy_flush_buffer cql_yy_flush_buffer
#define yy_scan_buffer cql_yy_scan_buffer
#define yy_scan_bytes cql_yy_scan_bytes
#define yy_scan_string cql_yy_scan_string
#define yy_switch_to_buffer cql_yy_switch_to_buffer
#define yyalloc cql_yyalloc
#define yyerror cql_yyerror
#define yyfree cql_yyfree
#define yyget_debug cql_yyget_debug
#define yyget_in cql_yyget_in
#define yyget_leng cql_yyget_leng
#define yyget_lineno cql_yyget_lineno
#define yyget_out cql_yyget_out
#define yyget_text cql_yyget_text
#define yylex cql_yylex
#define yylex_destroy cql_yylex_destroy
#define yyparse cql_yyparse
#define yypop_buffer_state cql_yypop_buffer_state
#define yypush_buffer_state cql_yypush_buffer_state
#define yyrealloc cql_yyrealloc
#define yyrestart cql_yyrestart
#define yyset_debug cql_yyset_debug
#define yyset_in cql_yyset_in
#define yyset_lineno cql_yyset_lineno
#define yyset_out cql_yyset_out

#pragma clang diagnostic push

EOF

echo "#ifndef CQL_NO_DIAGNOSTIC_BLOCK" >out/pass1
cat "diags.h" >>out/pass1
echo "#endif" >>out/pass1

cat "bytebuf.h" >>out/pass1
cat "charbuf.h" >>out/pass1
cat "symtab.h" >>out/pass1
cat "minipool.h" >>out/pass1
cat "list.h" >>out/pass1

cat "ast.h" >>out/pass1
cat "sem.h" >>out/pass1
cat "cql.h" >>out/pass1
cat "encoders.h" >>out/pass1
cat "gen_sql.h" >>out/pass1
cat "crc64xz.h" >>out/pass1
cat "cg_common.h" >>out/pass1
cat "cg_c.h" >>out/pass1
cat "cg_dasm.h" >>out/pass1
cat "cg_java.h" >>out/pass1
cat "cg_json_schema.h" >>out/pass1
cat "cg_msys_schema_utils.h" >>out/pass1
cat "cg_objc.h" >>out/pass1
cat "cg_query_plan.h" >>out/pass1
cat "cg_udf.h" >>out/pass1
cat "cg_schema.h" >>out/pass1
cat "cg_test_helpers.h" >>out/pass1
cat "compat.h" >>out/pass1
cat "rt.h" >>out/pass1

cat "ast.c" >>out/pass1
cat "bytebuf.c" >>out/pass1
cat "cg_dasm.c" >>out/pass1
cat "cg_c.c" >>out/pass1
cat "cg_common.c" >>out/pass1
cat "cg_java.c" >>out/pass1
cat "cg_json_schema.c" >>out/pass1
cat "cg_msys_schema_utils.c" >>out/pass1
cat "cg_objc.c" >>out/pass1
cat "cg_query_plan.c" >>out/pass1
cat "cg_udf.c" >>out/pass1
cat "cg_schema.c" >>out/pass1
cat "cg_test_helpers.c" >>out/pass1
cat "charbuf.c" >>out/pass1
cat "compat.c" >>out/pass1
cat "crc64xz.c" >>out/pass1
cat "encoders.c" >>out/pass1
cat "gen_sql.c" >>out/pass1
cat "list.c" >>out/pass1
cat "minipool.c" >>out/pass1
cat "rt.c" >>out/pass1
cat "sem.c" >>out/pass1
cat "symtab.c" >>out/pass1
cat "unit_tests.c" >>out/pass1

# the generated parser has free conversions not easily removed
cat <<EOF >>out/pass1
#pragma clang diagnostic ignored "-Wimplicit-int-conversion"
#pragma clang diagnostic ignored "-Wsign-conversion"
EOF

cat "out/cql.y.c" >>out/pass1
cat "out/cql.c" >>out/pass1

cat <<EOF >>out/pass1
#pragma clang diagnostic pop
EOF

echo "static void cql_reset_globals() {" >>out/pass1
grep cql_data_decl *.h | grep -v '#define' | \
  sed -e "s/ );//" -e "s/.* //" -e "s/\*//" -e "s/^/  /" -e "s/$/ = 0;/"  -e "s/options = 0/memset(\&options, 0, sizeof(options));/" \
  >>out/pass1
echo "}" >>out/pass1

grep -v "^ *#include" out/pass1 | grep -v "^ *#pragma once" | grep -v "#line" >>out/cql_amalgam.c
rm out/pass1

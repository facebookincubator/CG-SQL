/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define RT_IP_NOTICE(x) // none will be emitted

// note the @ is split from the generated so that tools don't think this is a generated file
#define RT_AUTOGEN(x) x " @" "generated S" "ignedSource<<deadbeef8badf00ddefec8edfacefeed>>\n"
#define RT_SIGNSRC(x) x " @" "generated S" "ignedSource<<deadbeef8badf00ddefec8edfacefeed>>\n"

#define RT_SYM_PREFIX "CGS_"
#define RT_STRING_ENCODE "cql_string_ref_encode"
#define RT_IMPL_SYMBOL_PREFIX ""
#define RT_OBJC_CASE cg_symbol_case_snake

#define RT_SHOULD_GENERATE_COPY NULL
#define RT_REGISTER_PROC_NAME NULL

#define RT_JAVA_RT_PACKAGE "com.facebook.cgsql"

#define RT_EXTRAS
#define RT_EXTRA_CLEANUP

#include "rt_common.c"

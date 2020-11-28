/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "ast.h"
#include "bytebuf.h"
#include "symtab.h"
#include "charbuf.h"
#include "list.h"
#include "sem.h"

typedef struct {
  sem_t sem_type;
  int64_t _int64;
  int32_t _int32;
  double  _real;
  bool_t  _bool;
  CSTR    _str;
} eval_node;

cql_noexport void eval_init();
cql_noexport void eval_cleanup();
cql_noexport void eval(ast_node *expr, eval_node *result);
cql_noexport ast_node *_Nonnull eval_set(ast_node *expr, eval_node *result);

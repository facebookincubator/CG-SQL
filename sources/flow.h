/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "sem.h"

// Pushes a normal context. This should be used in most cases (e.g., for
// statement lists that do not execute conditionally).
#define FLOW_PUSH_CONTEXT_NORMAL() \
  void *flow_context_normal; \
  _flow_push_context_normal();

// Pops a normal context.
#define FLOW_POP_CONTEXT_NORMAL() \
  (void)flow_context_normal; \
  _flow_pop_context_normal();

// Pushes a branch set context for an IF, CASE, et cetera.
#define FLOW_PUSH_CONTEXT_BRANCH_SET() \
  void *flow_context_branch_set; \
  _flow_push_context_branch_set();

// Pops a branch set context.
#define FLOW_POP_CONTEXT_BRANCH_SET() \
  (void)flow_context_branch_set; \
  _flow_pop_context_branch_set();

// Pushes a branch context. This must only be used within a branch set context
// and must be used for all branches within it.
#define FLOW_PUSH_CONTEXT_BRANCH() \
  void *flow_context_branch; \
  _flow_push_context_branch();

// Pops a branch context.
#define FLOW_POP_CONTEXT_BRANCH() \
  (void)flow_context_branch; \
  _flow_pop_context_branch();

// Sets `flag` on `*type`. This must not be called if the flag is already set or
// if a flow context is not in effect.
cql_noexport void flow_set_flag_for_type(sem_t flag, sem_t *type);

// Un-sets `flag` on `*type`. This must not be called unless the flag is already
// set or if a flow context is not in effect.
cql_noexport void flow_unset_flag_for_type(sem_t flag, sem_t *type);

// Indicates that the current branch set context will (or does) contain an
// "else" branch or some other type of catch-all branch. This must only be
// called while the current flow context is a branch set context. If this is not
// called, it will be assumed that such a branch is not present.
cql_noexport void flow_set_context_branch_set_has_else(bool_t has_else);

cql_noexport void _flow_push_context_normal();
cql_noexport void _flow_pop_context_normal();
cql_noexport void _flow_push_context_branch_set();
cql_noexport void _flow_pop_context_branch_set();
cql_noexport void _flow_push_context_branch();
cql_noexport void _flow_pop_context_branch();

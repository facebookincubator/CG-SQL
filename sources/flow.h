/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "sem.h"

// Pushes a normal context that unsets all improvements made within it when it
// is popped. This should be used in most cases.
#define FLOW_PUSH_CONTEXT_NORMAL() \
  void *flow_context_normal; \
  _flow_push_context_normal();

// Pops a normal context.
#define FLOW_POP_CONTEXT_NORMAL() \
  (void)flow_context_normal; \
  _flow_pop_context_normal();

// Pushes a branch group context for an IF, CASE, et cetera.
#define FLOW_PUSH_CONTEXT_BRANCH_GROUP() \
  void *flow_context_branch_group; \
  _flow_push_context_branch_group();

// Pops a branch group context.
#define FLOW_POP_CONTEXT_BRANCH_GROUP() \
  (void)flow_context_branch_group; \
  _flow_pop_context_branch_group();

// Pushes a branch context. This must only be used within a branch group context
// and must be used for all branches within it. (For empty branches with no
// statements, one may use `flow_context_branch_group_add_empty_branch` instead
// of pushing and immediately popping a new branch context.)
#define FLOW_PUSH_CONTEXT_BRANCH() \
  void *flow_context_branch; \
  _flow_push_context_branch();

// Pops a branch context.
#define FLOW_POP_CONTEXT_BRANCH() \
  (void)flow_context_branch; \
  _flow_pop_context_branch();

// Pushes a jump context. This should be used for statement lists where control
// flow can jump to the end of the context (e.g., TRY via THROW (or a CALL
// containing a THROW), WHILE and LOOP via CONTINUE and LEAVE, et cetera). It
// should not be used for statement lists merely because of the possibility of
// an early RETURN: Jump contexts are specifically designed to safely manage
// the setting and unsetting of improvements *within* a procedure.
#define FLOW_PUSH_CONTEXT_JUMP() \
  void *flow_context_jump; \
  _flow_push_context_jump();

// Pops a jump context.
#define FLOW_POP_CONTEXT_JUMP() \
  (void)flow_context_jump; \
  _flow_pop_context_jump();

// Sets `flag` on `*type`. This must not be called if the flag is already set or
// if a flow context is not in effect.
cql_noexport void flow_set_flag_for_type(sem_t flag, sem_t *type);

// Un-sets `flag` on `*type`. This must not be called unless the flag is already
// set or if a flow context is not in effect.
cql_noexport void flow_unset_flag_for_type(sem_t flag, sem_t *type);

// Indicates that the current branch group context will (or does) contain a
// catch-all branch or otherwise covers all possible cases. This must only be
// called while the current flow context is a branch group context. If this is
// not called, it will be assumed that all cases are not covered.
cql_noexport void flow_set_context_branch_group_covers_all_cases(bool_t covers_all_cases);

// Adds an empty branch to the current branch group context. It is equivalent to
// `FLOW_PUSH_CONTEXT_BRANCH` immediately followed by `FLOW_POP_CONTEXT_BRANCH`
// and exists only for the sake of convenience.
cql_noexport void flow_context_branch_group_add_empty_branch();

cql_noexport void _flow_push_context_normal();
cql_noexport void _flow_pop_context_normal();
cql_noexport void _flow_push_context_branch_group();
cql_noexport void _flow_pop_context_branch_group();
cql_noexport void _flow_push_context_branch();
cql_noexport void _flow_pop_context_branch();
cql_noexport void _flow_push_context_jump();
cql_noexport void _flow_pop_context_jump();

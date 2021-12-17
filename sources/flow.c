/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "ast.h"

#include "flow.h"

// Indicates whether a history item resulted from setting a flag or unsetting a
// flag. The values associated with each enum case allow a series of deltas to
// be totalled up to determine an overall effect when popping a branch group.
typedef enum {
  FLOW_HISTORY_DELTA_UNSET = -1,
  FLOW_HISTORY_DELTA_SET = 1
} flow_history_delta;

// Records a call to `flow_set_flag_for_type` or `flow_unset_flag_for_type`.
// These form a linked list referred to as a "history".
typedef struct history_item {
  struct history_item *next;
  sem_t *type;
  sem_t flag;
  flow_history_delta delta;
} flow_history_item;

// A list of `history_item` values.
typedef flow_history_item *flow_history;

// There are different types of flow contexts, each of which is indicated by its
// `flow_context_kind`.
typedef enum {
  // Normal flow contexts simply revert all improvements set within them when
  // they are popped.
  FLOW_CONTEXT_KIND_NORMAL,

  // Branch group contexts contain zero or more branch contexts. Branch group
  // contexts perform extra steps to merge the effects of their branches.
  FLOW_CONTEXT_KIND_BRANCH_GROUP,

  // Branch contexts are contexts that are only entered into if some condition
  // is true. They must only be created within a branch group context. They are
  // special in that they completely reverse all effects within them when they
  // are popped so that subsequent branches are unaffected.
  FLOW_CONTEXT_KIND_BRANCH,

  // Jump contexts are pessimistic contexts that assume, after the context has
  // ended, that all possible un-sets within it happened and all sets within it
  // did not happen. They can be used to ensure safety for TRY blocks, loops,
  // and other blocks containing statement lists that may not be executed in
  // full due to one statement within them jumping to the end of the context (or
  // to the end of an enclosing jump context).
  //
  // NOTE: Jump contexts do not take the locations of control statements within
  // them, if any, into account, and thus are presently more conservative than
  // is necessary. Experience with previous versions of CQL that treated *all*
  // contexts this pessimistically strongly suggests that this is not a problem
  // in practice.
  FLOW_CONTEXT_KIND_JUMP
} flow_context_kind;

// A flow context is used to encapsulate a region of a program so that effects
// within it (e.g., nullability improvements, variable initialization
// improvements, row check improvements, et cetera) can be managed
// appropriately.
typedef struct flow_context {
  // The parent of the context, if any.
  struct flow_context *parent;

  // The kind of the context. The value of `kind` indicates which of the
  // anonymous structs within the union below may be accessed, if any.
  flow_context_kind kind;

  // The history of sets and un-sets made within the context. This does not
  // necessarily include *all* sets and un-sets as branch group contexts merge
  // the effects of their branches.
  flow_history history;

  union {
    // Only used when `kind` is `FLOW_CONTEXT_KIND_BRANCH_GROUP`.
    struct {
      // The concatenated histories of all of the branches created directly
      // within the branch group.
      flow_history branch_histories;

      // The number of subcontexts (i.e., branches) created directly within the
      // branch group.
      uint32_t subcontext_count;

      // `true` if the branch group includes (or will include) an ELSE or other
      // type of catch-all branch.
      bool_t has_else;
    } branch_group;

    // Only used when `kind` is `FLOW_CONTEXT_KIND_JUMP`.
    struct {
      // The nearest enclosing context with kind `FLOW_CONTEXT_KIND_JUMP`, if
      // any. This is used to update `top_jump_context` when a jump context is
      // popped.
      struct flow_context *nearest_jump_context;

      // The histories of *all* un-sets made within the jump context's
      // subcontexts.
      flow_history unset_histories;
    } jump;
  };
} flow_context;

// The global that holds all control flow information managed within this file.
static flow_context *current_context;

// The topmost jump context, if any. This exists merely to avoid the need to
// search upwards from the current context for this every time an improvement is
// unset: It provides no additional information beyond what is already available
// in `current_context`.
static flow_context *top_jump_context;

// Given a pointer to `history`, sets its tail to `history_to_append`.
static void append_history(flow_history *history, flow_history history_to_append) {
  flow_history *tail = history;

  while (*tail) {
    tail = &(*tail)->next;
  }

  *tail = history_to_append;
}

// Given `history`, returns an array that points to each item within it and sets
// `*count` to the total number of items.
static flow_history_item **array_from_history(flow_history history, uint32_t *count) {
  Contract(count);

  uint32_t length = 0;
  for (flow_history_item *item = history; item; item = item->next) {
    length++;
  }

  flow_history_item **array = _ast_pool_new_array(flow_history_item *, length);

  uint32_t i = 0;
  for (flow_history_item *item = history; item; item = item->next, i++) {
    array[i] = item;
  }

  *count = length;

  return array;
}

// Compares two history items by their type addresses (which, in this instance,
// constitute a unique identifier), followed by their flags. This is used to
// allow `qsort` to group together effects across a group of branches in order
// to be able to produce the total set of effects for the enclosing branch
// group.
static int history_item_comparator(const void *a, const void *b) {
  flow_history_item *item_a = *(flow_history_item **)a;
  flow_history_item *item_b = *(flow_history_item **)b;

  if (item_a->type < item_b->type) {
    return -1;
  }

  if (item_a->type > item_b->type) {
    return 1;
  }


  // TODO: We currently only deal with one type of flag, so these are
  // temporarily disabled for the sake of code coverage.
#if 0
  if (item_a->flag < item_b->flag) {
    return -1;
  }

  if (item_a->flag > item_b->flag) {
    return 1;
  }
#endif

  return 0;
}

// Returns a new history item given initial values.
static flow_history_item *history_item_new(sem_t *type, sem_t flag, flow_history_delta delta) {
  Contract(type);
  Contract(is_single_flag(flag));

  flow_history_item *item = _ast_pool_new(flow_history_item);
  item->next = NULL;
  item->type = type;
  item->flag = flag;
  item->delta = delta;

  return item;
}

// Reverses a history in place.
static void reverse_history(flow_history *history) {
  flow_history_item *current = *history;
  flow_history_item *previous = NULL;
  flow_history_item *next = NULL;

  while (current) {
    next = current->next;
    current->next = previous;
    previous = current;
    current = next;
  }

  *history = previous;
}

// Clones a history and returns the new copy.
static flow_history clone_history(flow_history history) {
  flow_history new_history = NULL;

  for (flow_history_item *item = history; item; item = item->next) {
    flow_history_item *new_item = history_item_new(item->type, item->flag, item->delta);
    new_item->next = new_history;
    new_history = new_item;
  }

  reverse_history(&new_history);

  return new_history;
}

// Given a pointer to a history, adds a new history item to it with the initial
// values provided.
static void record_in_history(flow_history *history, sem_t *type, sem_t flag, flow_history_delta delta) {
  Contract(history);
  Contract(type);
  Contract(is_single_flag(flag));

  flow_history_item *item = history_item_new(type, flag, delta);
  item->next = *history;
  *history = item;
}

// Sets `flag` on `*type` and records it in the history of the the current
// context. This must never be called if the flag is already set.
cql_noexport void flow_set_flag_for_type(sem_t flag, sem_t *type) {
  Contract(current_context);
  Contract(is_single_flag(flag));
  Contract(type);
  Contract(!(*type & flag));

  *type |= flag;

  record_in_history(&current_context->history, type, flag, FLOW_HISTORY_DELTA_SET);
}

// Un-sets `flag` on `*type` and records it in the history of the the current
// context. This must never be called unless the flag is already set.
cql_noexport void flow_unset_flag_for_type(sem_t flag, sem_t *type) {
  Contract(current_context);
  Contract(is_single_flag(flag));
  Contract(type);
  Contract(*type & flag);

  *type &= sem_not(flag);

  record_in_history(&current_context->history, type, flag, FLOW_HISTORY_DELTA_UNSET);
 
  // If we're within a jump context, record the unset there too so we can
  // re-unset it later. We can skip this if the current context is a jump
  // context as the unset was just recorded in its history above directly.
  if (top_jump_context && current_context != top_jump_context) {
    record_in_history(&top_jump_context->jump.unset_histories, type, flag, FLOW_HISTORY_DELTA_UNSET);
  }
}

// Creates a new context with the kind provided and adds it to the current
// context. This does NOT initialize any kind-specific members.
static void push_context_with_kind(flow_context_kind kind) {
  flow_context *context = _ast_pool_new(flow_context);
  context->parent = current_context;
  context->kind = kind;
  context->history = NULL;

  current_context = context;
}

// Given a history, iterates over it and calls `func` with the delta sum of each
// type/flag combination.
static void with_delta_sums_of_history(flow_history history, void func(sem_t *type, sem_t flag, int32_t delta_sum)) {
  Contract(func);

  uint32_t item_count;
  flow_history_item **item_array = array_from_history(history, &item_count);

  if (item_count == 0) {
    return;
  }

  // Sort the total history by type, then by flag.
  qsort(item_array, item_count, sizeof(flow_history_item *), history_item_comparator);

  // Iterate over the branch histories, calling `func` the `delta_sum` of each
  // type/flag combination.
  int32_t delta_sum = item_array[0]->delta;
  for (uint32_t i = 1; i < item_count; i++) {
    flow_history_item *previous_item = item_array[i - 1];
    flow_history_item *current_item = item_array[i];
    if (previous_item->type != current_item->type || previous_item->flag != current_item->flag) {
      func(previous_item->type, previous_item->flag, delta_sum);
      delta_sum = 0;
    }
    delta_sum += current_item->delta;
  }
  func(item_array[item_count - 1]->type, item_array[item_count - 1]->flag, delta_sum);
}

// Asserts that the `delta_sum` calculated for a particular type/flag
// combination present in some context's history is within [-1, 1] as is
// required for `merge_effects` to work properly.
static void invariant_delta_sum(sem_t *type, sem_t flag, int32_t delta_sum) {
  Invariant(delta_sum >= -1 && delta_sum <= 1);
}

// Moves the history of the current context to the start of the history of the
// parent context (if any) so that it can manage it however it chooses.
static void move_history_to_parent() {
  Contract(current_context);

  if (current_context->parent) {
    append_history(&current_context->history, current_context->parent->history);
    current_context->parent->history = current_context->history;
  }

  // In order for `merge_effects` to work properly, the history field of every
  // context must contain a history that, for every type/flag combination, has a
  // delta sum of -1, 0, or 1. We enforce that invariant here.
  with_delta_sums_of_history(
    // If we have a parent context, we check that (purely for the sake of
    // checking more things rather than fewer) as it now also contains the
    // history of the current context.
    (current_context->parent ? current_context->parent : current_context)->history,
    invariant_delta_sum
  );

  // This is no longer valid.
  current_context->history = NULL;
}

// Pushes a normal context. Unless one is pushing a context for a group of
// branches (e.g., within an IF or CASE) or pushing a branch itself, one should
// simply push a normal context.
cql_noexport void _flow_push_context_normal() {
  push_context_with_kind(FLOW_CONTEXT_KIND_NORMAL);
}

// Un-sets all of the improvements in the current history while leaving the
// history itself unchanged.
static void unset_all_improvements() {
  for (flow_history_item *item = current_context->history; item; item = item->next) {
    switch (item->delta) {
      case FLOW_HISTORY_DELTA_UNSET:
        break;
      case FLOW_HISTORY_DELTA_SET:
        if (*item->type & item->flag) {
          flow_unset_flag_for_type(item->flag, item->type);
        }
    }
  }
}

// Pops a normal context, unsetting all improvements made within it.
cql_noexport void _flow_pop_context_normal() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_NORMAL);

  unset_all_improvements();

  move_history_to_parent();

  current_context = current_context->parent;
}

// Pushes a new branch group context. Only branch contexts may be created within
// it.
cql_noexport void _flow_push_context_branch_group() {
  push_context_with_kind(FLOW_CONTEXT_KIND_BRANCH_GROUP);

  current_context->branch_group.branch_histories = NULL;
  current_context->branch_group.subcontext_count = 0;
  current_context->branch_group.has_else = false;
}

// Merges the effects of the branches of the current branch group context for a
// particular type and flag to produce the overall effect.
//
// For a given type and flag combination, each branch could have ultimately
// unset a flag that was initially set, set a flag that was initially unset, or
// been neutral with respect to the flag (e.g., by setting it, then unsetting it
// when it was initially unset, or by simply not affecting it at all). Each
// branch, therefore, can be assigned a value of -1, 1, or 0 respectively. These
// are totalled up and represent the `delta_sum`.
static void merge_effects(sem_t *type, sem_t flag, int32_t delta_sum) {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_GROUP);
  Contract(current_context->branch_group.subcontext_count > 0);
  Contract(type);
  Contract(is_single_flag(flag));

  // If the delta sum is positive, it must have been the case that the flag
  // began unset otherwise no branches could have set it.
  Invariant(delta_sum <= 0 || !(*type & flag));

  // If the delta sum is negative, it must have been the case that the flag
  // began set otherwise no branches could have unset it.
  Invariant(delta_sum >= 0 || *type & flag);

  // `subcontext_count` is effectively the total number of branches within the
  // current branch context, including any sort of "else" or catch-all branch,
  // if present.
  uint32_t subcontext_count = current_context->branch_group.subcontext_count;

  // If all branches set a flag, `delta_sum` will equal `subcontext_count`.
  // Likewise, if all branches unset it, `abs(delta_sum)` will equal
  // `subcontext_count`.
  Invariant(abs(delta_sum) <= subcontext_count);

  // Indicates whether or not there was a catch-all branch.
  bool_t has_else = current_context->branch_group.has_else;

  if (has_else && delta_sum == subcontext_count) {
    // There is a catch-all branch and the `delta_sum` is equal to the number of
    // branches. This means the branches cover all possibilities and every
    // branch made the same improvement, so we can consider the entire branch
    // set to have made the improvement.
    flow_set_flag_for_type(flag, type);
  } else if (delta_sum < 0) {
    // The delta sum is negative, so at least one of the branches unset the
    // flag. We must, therefore, consider the entire branch group as having
    // unset the flag.
    flow_unset_flag_for_type(flag, type);
  } else {
    // If `delta_sum` is 0, that means all branches were neutral with respect to
    // the flag. If `delta_sum` is positive, but less than `subcontext_count`,
    // that means some branches improved it and the rest were netural. Since all
    // branches were at least neutral, it's safe to do nothing here and allow
    // things to remain as they are.
    //
    // The reason we know this is that we only set something when it is unset
    // and only unset something when it is set, and so all branches must have
    // the same overall effect if they are to have any effect at all. For
    // example, if a variable is nullable before an IF, branches can either
    // leave it as it is or improve it. Likewise, if a variable is already
    // inferred to be nonnull before an IF, branches can either leave it as it
    // is or un-improve it. Since it is not possible for one branch to improve
    // something if another un-improved it and vice versa, we know any
    // non-negative delta sum implies all branches were at least neutral, and so
    // no unset is required.
  }
}

// Records whether or not the current branch group context has a catch-all
// branch.
cql_noexport void flow_set_context_branch_group_has_else(bool_t has_else) {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_GROUP);

  current_context->branch_group.has_else = has_else;
}

// Pops the current branch group, calculating the total effect of all of its
// branches in the process.
cql_noexport void _flow_pop_context_branch_group() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_GROUP);

  // Unset all of the negative improvements made within the branch group itself.
  // Negative improvements are those made because the condition of a previous
  // branch must have been false if a later branch was taken.
  unset_all_improvements();

  // Zero out the current history. We will rebuild it based on what occurred in
  // the branches via `merge_effects`.
  current_context->history = NULL;

  // Merge the effects of all of the branches.
  with_delta_sums_of_history(current_context->branch_group.branch_histories, merge_effects);

  // The history we move to the parent contains only the result of the
  // `unset_all_improvements` call above and the result of merging the effects:
  // The full histories of the branches are completely discarded. This is
  // critical for the delta sum approach that `merge_effects` uses: If N
  // branches within a branch group all improve the same type/flag combination,
  // we need that to count as a single improvement, not N+1 improvements, for
  // any further effect merging that might take place in enclosing branch groups
  // to work correctly.
  move_history_to_parent();

  current_context = current_context->parent;
}

// Pushes a new branch context. This must only be done within a branch group
// context. It is critical that this be done for ALL branches of a given
// conditional, including any "else" or catch-all branch, otherwise
// `_flow_pop_context_branch_group` may improve something too optimistically due
// to a lack of sufficient information.
cql_noexport void _flow_push_context_branch() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_GROUP);

  push_context_with_kind(FLOW_CONTEXT_KIND_BRANCH);

  // Clone the current history of (typically negative) improvements within the
  // branch group itself and add it to the accumulated history of its branches.
  // The reason we do this is so that we can calculate the correct delta sum
  // later on.
  //
  // Consider the following code:
  //
  // IF x IS NULL THEN
  //   SET x := 42;
  // ELSE IF y THEN
  //   -- do nothing
  // ELSE
  //   -- do nothing
  // END IF;
  //
  // Here, even though the second and third branches have not improved the type
  // of `x` to be nonnull themselves, it is still the case that x is nonnull
  // at the end of the second and third branches due to the condition of the
  // first branch. It is therefore safe and appropriate to treat the branches as
  // though they each made the improvement themselves so that x can be treated
  // as nonnull after END IF.
  //
  // To put it more plainly, we want a delta sum of 3 for the improvement to x.
  // If we didn't perform this step, it would only be 1.
  flow_history branch_group_history = clone_history(current_context->parent->history);
  append_history(&branch_group_history, current_context->parent->branch_group.branch_histories);
  current_context->parent->branch_group.branch_histories = branch_group_history;

  // Increment the subcontext count of the parent branch group so that it can be
  // later compared against delta sums.
  current_context->parent->branch_group.subcontext_count++;
}

// Pop a branch context, reverting the history within it such that it will be as
// though nothing ever happened.
cql_noexport void _flow_pop_context_branch() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH);
  
  Invariant(current_context->parent);
  Invariant(current_context->parent->kind == FLOW_CONTEXT_KIND_BRANCH_GROUP);

  // Revert the history by interating over it, starting with the most recent
  // item, and doing the opposite of what was originally done. We only adjust
  // the flags and do not record the changes: The parent branch group will
  // handle the recording when it merges the effects of all of its branches.
  for (flow_history_item *item = current_context->history; item; item = item->next) {
    switch (item->delta) {
      case FLOW_HISTORY_DELTA_UNSET:
        *item->type |= item->flag;
        break;
      case FLOW_HISTORY_DELTA_SET:
        *item->type &= sem_not(item->flag);
        break;
    }
  }

  // Add the history of the branch to the total set of branch histories for the
  // current branch group.
  append_history(&current_context->history, current_context->parent->branch_group.branch_histories);
  current_context->parent->branch_group.branch_histories = current_context->history;

  current_context = current_context->parent;
}

// Pushes a jump context such that `current_context` will be identical to
// `top_jump_context`.
cql_noexport void _flow_push_context_jump() {
  push_context_with_kind(FLOW_CONTEXT_KIND_JUMP);

  current_context->jump.nearest_jump_context = top_jump_context;
  current_context->jump.unset_histories = NULL;

  top_jump_context = current_context;
}

// Pops the current jump context and updates `top_jump_context` accordingly.
cql_noexport void _flow_pop_context_jump() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_JUMP);

  // Re-unset all un-sets made anywhere within the jump context's subcontexts.
  // This is done to remain safe in the presence of code like the following:
  //
  //   DECLARE x INT;
  //   -- improve x
  //   SET x := 42;
  //   WHILE some_condition
  //   BEGIN
  //     -- use an IF/ELSE to trigger effect merging in the branch group; this
  //     -- is critical to this example as we'd be safe otherwise!
  //     IF another_condition THEN
  //       -- un-improve x
  //       SET x := NULL;
  //       IF yet_another_condition THEN
  //         -- jump before re-improving x
  //         LEAVE;
  //       END IF;
  //       -- re-improve x
  //       SET x := 100;
  //     ELSE
  //       -- neutral; do nothing
  //     END IF;
  //     -- the branch group context considers the one un-improvement of 'x' and
  //     -- the one improvement of 'x' in the first branch to be neutral and
  //     -- thus cancels out their effects since the ELSE is also netural
  //   END;
  //   -- if the body of the WHILE were analyzed within a normal flow context,
  //   -- x would be unsafely treated as nonnull here due to the cancelling
  //   -- mentioned above
  //
  // NOTE: The approach taken here is more conservative than is necessary and
  // may be revisited in the future.
  for (flow_history_item *item = current_context->jump.unset_histories; item; item = item->next) {
    Invariant(item->delta == FLOW_HISTORY_DELTA_UNSET);
    if (*item->type & item->flag) {
      flow_unset_flag_for_type(item->flag, item->type);
    }
  }

  // This un-sets improvements initially made within the jump context itself, of
  // course, but it also propagates any un-sets made in the previous step to the
  // nearest enclosing jump context, if any. This is important because all of
  // the unsetting we're doing now may be reverted by a branch context in
  // between this context and the nearest enclosing jump context.
  unset_all_improvements();

  move_history_to_parent();

  top_jump_context = current_context->jump.nearest_jump_context;
  current_context = current_context->parent;
}

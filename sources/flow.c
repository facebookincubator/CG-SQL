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
// be totalled up to determine an overall effect when popping a branch set.
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
  // Branch set contexts are used to hold a set of branch contexts. Branch set
  // contexts perform extra steps to merge the effects of their branches.
  FLOW_CONTEXT_KIND_BRANCH_SET,
  // Branch contexts are contexts that are only entered into if some condition
  // is true. They must only be created within a branch set context. They are
  // special in that they completely reverse all effects within them when they
  // are popped so that subsequent branches are unaffected.
  FLOW_CONTEXT_KIND_BRANCH
} flow_context_kind;

// A flow context is used to encapsulate a region of a program so that effects
// within it (e.g., nullability improvements, variable initialization
// improvements, row check improvements, et cetera) can be managed
// appropriately.
typedef struct flow_context {
  struct flow_context *parent;
  flow_context_kind kind;
  flow_history history;
  union {
    // This must only be accessed when `kind` is `FLOW_CONTEXT_KIND_BRANCH_SET`.
    struct {
      flow_history histories;
      uint32_t subcontext_count;
      bool_t has_else;
    } branch_set;
  };
} flow_context;

// The single global that holds all control flow information managed within this
// file.
static flow_context *current_context;

// Given a pointer to `history`, set its tail to `history_to_append`.
static void append_history(flow_history *history, flow_history history_to_append) {
  flow_history *tail = history;

  while (*tail) {
    tail = &(*tail)->next;
  }

  *tail = history_to_append;
}

// Given `history`, return an array that points to each item within it and set
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
// allow `qsort` to group together effects across a set of branches in order to
// be able to produce the total set of effects for the enclosing branch set.
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

// Adds a new history item with the initial values provided to the history of
// the current context.
static void record_set_or_unset(sem_t *type, sem_t flag, flow_history_delta delta) {
  flow_history_item *item = history_item_new(type, flag, delta);
  item->next = current_context->history;
  current_context->history = item;
}

// Sets `flag` on `*type` and records it in the history of the the current
// context. This must never be called if the flag is already set.
cql_noexport void flow_set_flag_for_type(sem_t flag, sem_t *type) {
  Contract(current_context);
  Contract(is_single_flag(flag));
  Contract(type);
  Contract(!(*type & flag));

  *type |= flag;
  record_set_or_unset(type, flag, FLOW_HISTORY_DELTA_SET);
}

// Un-sets `flag` on `*type` and records it in the history of the the current
// context. This must never be called unless the flag is already set.
cql_noexport void flow_unset_flag_for_type(sem_t flag, sem_t *type) {
  Contract(current_context);
  Contract(is_single_flag(flag));
  Contract(type);
  Contract(*type & flag);

  *type &= sem_not(flag);
  record_set_or_unset(type, flag, FLOW_HISTORY_DELTA_UNSET);
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

// Moves the history of the current context to the start of the history of the
// parent context (if any) so that it can manage it however it chooses.
static void move_history_to_parent() {
  Contract(current_context);

  if (current_context->parent) {
    append_history(&current_context->history, current_context->parent->history);
    current_context->parent->history = current_context->history;
  }

  current_context->history = NULL;
}

// Pushes a normal context. Unless one is pushing a context for a set of
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

// Pushes a new branch set context. Only branch contexts may be created within
// it.
cql_noexport void _flow_push_context_branch_set() {
  push_context_with_kind(FLOW_CONTEXT_KIND_BRANCH_SET);

  current_context->branch_set.histories = NULL;
  current_context->branch_set.subcontext_count = 0;
  current_context->branch_set.has_else = false;
}

// Merges the effects of the branches of the current branch set context for a
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
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_SET);
  Contract(current_context->branch_set.subcontext_count > 0);
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
  uint32_t subcontext_count = current_context->branch_set.subcontext_count;

  // If all branches set a flag, `delta_sum` will equal `subcontext_count`.
  // Likewise, if all branches unset it, `abs(delta_sum)` will equal
  // `subcontext_count`.
  Invariant(abs(delta_sum) <= subcontext_count);

  // Indicates whether or not there was a catch-all branch.
  bool_t has_else = current_context->branch_set.has_else;

  if (has_else && delta_sum == subcontext_count) {
    // There is a catch-all branch and the `delta_sum` is equal to the number of
    // branches. This means the branches cover all possibilities and every
    // branch made the same improvement, so we can consider the entire branch
    // set to have made the improvement.
    flow_set_flag_for_type(flag, type);
  } else if (delta_sum < 0) {
    // The delta sum is negative, so at least one of the branches unset the
    // flag. We must, therefore, consider the entire branch set as having unset
    // the flag.
    flow_unset_flag_for_type(flag, type);
  } else {
    // If `delta_sum` is 0, that means all branches were neutral with respect to
    // the flag. If `delta_sum` is positive, but less than `subcontext_count`,
    // that means some branches improved it and the rest were netural. The
    // reason we know this is that we only set something when it is unset and
    // only unset it when it is set, and so all branches must have the same
    // overall effect if they are to have any effect at all. Since all branches
    // were at least neutral, it's safe to do nothing here and allow things to
    // remain as they are.
  }
}

// Records whether or not the current branch set context has a catch-all branch.
cql_noexport void flow_set_context_branch_set_has_else(bool_t has_else) {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_SET);

  current_context->branch_set.has_else = has_else;
}

// Pops the current branch set, calculating the total effect of all of its
// branches in the process.
cql_noexport void _flow_pop_context_branch_set() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_SET);

  // Unset all of the negative improvements made within the branch set itself.
  // Negative improvements are those made because the condition of a previous
  // branch must have been false if a later branch was taken.
  unset_all_improvements();

  // Zero out the current history. We will rebuild it based on what occurred in
  // the branches via `merge_effects`.
  current_context->history = NULL;

  // Create an array holding the history of all of the branches put together.
  uint32_t item_count;
  flow_history_item **item_array = array_from_history(current_context->branch_set.histories, &item_count);

  if (item_count == 0) {
    // The branches themselves have no history, and so there is nothing to merge.
    move_history_to_parent();
    current_context = current_context->parent;
    return;
  }

  // Sort the total history of the branches by type, then by flag.
  qsort(item_array, item_count, sizeof(flow_history_item *), history_item_comparator);

  // Iterate over the branch histories calculating the `delta_sum` for each
  // type/flag combination.
  int32_t delta_sum = item_array[0]->delta;
  for (uint32_t i = 1; i < item_count; i++) {
    flow_history_item *previous_item = item_array[i - 1];
    flow_history_item *current_item = item_array[i];
    if (previous_item->type != current_item->type || previous_item->flag != current_item->flag) {
      // We've finished calculating a `delta_sum`. We can now merge the effects
      // of all of the branches for a given type/flag combination.
      merge_effects(previous_item->type, previous_item->flag, delta_sum);
      delta_sum = 0;
    }
    delta_sum += current_item->delta;
  }

  // Merge the effects of the last type/flag combination in the array.
  merge_effects(item_array[item_count - 1]->type, item_array[item_count - 1]->flag, delta_sum);

  move_history_to_parent();

  current_context = current_context->parent;
}

// Pushes a new branch context. This must only be done within a branch set
// context. It is critical that this be done for ALL branches of a given
// conditional, including any "else" or catch-all branch, otherwise
// `_flow_pop_context_branch_set` may improve something too optimistically due
// to a lack of sufficient information.
cql_noexport void _flow_push_context_branch() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH_SET);

  push_context_with_kind(FLOW_CONTEXT_KIND_BRANCH);

  // Clone the current history of (typically negative) improvements within the
  // branch set itself and add it to the accumulated history of its branches.
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
  flow_history branch_set_history = clone_history(current_context->parent->history);
  append_history(&branch_set_history, current_context->parent->branch_set.histories);
  current_context->parent->branch_set.histories = branch_set_history;

  // Increment the subcontext count of the parent branch set so that it can be
  // later compared against delta sums.
  current_context->parent->branch_set.subcontext_count++;
}

// Pop a branch context, reverting the history within it such that it will be as
// though nothing ever happened.
cql_noexport void _flow_pop_context_branch() {
  Contract(current_context);
  Contract(current_context->kind == FLOW_CONTEXT_KIND_BRANCH);
  
  Invariant(current_context->parent);
  Invariant(current_context->parent->kind == FLOW_CONTEXT_KIND_BRANCH_SET);

  // Revert the history by interating over it, starting with the most recent
  // item, and doing the opposite of what was originally done. We only adjust
  // the flags and do not record the changes: The parent branch set will handle
  // the recording when it merges the effects of all of its branches.
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
  // current branch set.
  append_history(&current_context->history, current_context->parent->branch_set.histories);
  current_context->parent->branch_set.histories = current_context->history;

  current_context = current_context->parent;
}

/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// super simple linked list handlers

#include "cql.h"
#include "ast.h"
#include "list.h"

cql_noexport void add_item_to_list(list_item **head, struct ast_node *ast) {
  list_item *item = _ast_pool_new(list_item);
  item->next = *head;
  item->ast = ast;
  *head = item;
}

cql_noexport void reverse_list(list_item **head) {
  list_item *cur = *head;
  list_item *prev = NULL;
  list_item *next = NULL;

  while (cur) {
      next = cur->next;
      cur->next = prev;
      prev = cur;
      cur = next;
  }
  *head = prev;
}

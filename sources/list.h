/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// super simple linked list handlers

#pragma once

// no need to free this list anymore minipool will do it for 
// you automatically at the end of a CQL run.
// @see minipool
typedef struct list_item {
  struct ast_node *ast;
  struct list_item *next;
} list_item;

cql_noexport void add_item_to_list(list_item **head, struct ast_node *ast);
cql_noexport void reverse_list(list_item **head);

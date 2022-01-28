/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "cql.h"


// Entry point for cg_stats result time

// This type takes as input the AST and emits aggregated
// statistics about its AST shape on a per-proc basis in CSV form.
// This output help you to understand the nature of your procs

cql_noexport void cg_stats_main(struct ast_node *root);

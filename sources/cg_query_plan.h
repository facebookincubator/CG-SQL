/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "cql.h"

// Entry point for cql query plan runtime.
// The runtime takes as an input a cql with one or many sql or cql statements
// It codegen a CQL stored proc that print out the query plan report of all
// elegible sql statement in the input. All sql statement are not eligible to
// run in an explain statement, this is the reason why we used in the previous
// sentence the word eligible.
// The query plan report contains:
//   - the target sql statement
//   - the list of algorithm used in the target sql statement
//   - the sequence of execution of the plan for the target sql statement

cql_noexport void cg_query_plan_main(struct ast_node *root);

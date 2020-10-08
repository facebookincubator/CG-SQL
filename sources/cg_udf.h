/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "cql.h"

// Entry point for user defined function runtime.
// The runtime takes as an input a cql file and out put c code to registered
// into sqlite database all user defined functions used in the cql file. The
// actual user defined function implement is a bogus implementation that we
// might have to revisit in the future. For now it's enough to allow query
// plan runtime to support udf.

cql_noexport void cg_udf_main(struct ast_node *root);

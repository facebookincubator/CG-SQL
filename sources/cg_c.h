/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "cql.h"

cql_noexport void cg_c_main(struct ast_node *root);
cql_noexport void cg_c_cleanup(void);

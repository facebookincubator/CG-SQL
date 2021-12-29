/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- there is no x, this causes the fail path to be taken and no code is generated
-- that's the entire test... any eror will do
set x := 1;

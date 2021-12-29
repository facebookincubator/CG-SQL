/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* you can't codegen C if previous schema mode was enabled */

create proc whatever()
begin
 select 1 x;
end;

@previous_schema;

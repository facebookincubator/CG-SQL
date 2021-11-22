/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create proc cursor_with_object(object_ Object)
begin
  declare C cursor like cursor_with_object arguments;
  fetch C from arguments;
  out C;
end;

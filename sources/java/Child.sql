/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- any kind of child result set will do the job for this test
create proc Child(i integer not null)
begin
  declare C cursor like (x integer not null, y text not null);
  let j := 0;
  while j < i 
  begin
    set j := j + 1;
    fetch C using
       j x,
       printf("<< %d >>", j)  y;
    out union C;
  end;
end;

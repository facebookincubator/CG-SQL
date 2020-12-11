/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create proc foo()
begin
  if 1 then
    call printf("one");
  else
    call printf("two");
  end if;

  declare x integer;

  set x :=  coalesce( case 
             when  1 
           then 200
         when 2
   then 300
    end, 
    3000);
end;

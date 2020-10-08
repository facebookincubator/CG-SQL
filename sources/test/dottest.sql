/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- general dot print example for nodes of non primitive types
select x,y,5.2 from A
order by a,b,c,d ASC limit 5;

-- print select binary expression with all numerical data types
select 1.2 + 2147483648 * 3;

-- print string type ast node
select 'a' || 'b';

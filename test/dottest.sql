-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

-- general dot print example for nodes of non primitive types
select x,y,5.2 from A
order by a,b,c,d ASC limit 5;

-- print select binary expression with all numerical data types
select 1.2 + 2147483648 * 3;

-- print string type ast node
select 'a' || 'b';

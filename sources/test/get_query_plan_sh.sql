/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create table t1(id int primary key, name text);
create table t2(id int primary key, name text);
create table t3(id int primary key, name text);
create table t4(id int primary key, data blob);

create proc sample()
begin
  select * from t1
    where name = 'Nelly' and
    id IN (
      select id from t2
        where id = 1
      union
      select id from t3
    )
    order by name asc;
end;

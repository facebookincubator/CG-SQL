/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- demo stored procs

create procedure make_mixed()
begin
  create table mixed(
    id integer not null,
    name text check(length(name) >= 6) collate nocase,
    code long int,   -- these are nullable to make the cg harder
    flag bool,
    rate real
  );
end;

create procedure load_mixed()
begin
  delete from mixed;
  insert into mixed values (1, 'a name', 12, 1, 5.0);
  insert into mixed values (2, 'some name', 14, 3, 7.0);
  insert into mixed values (3, 'yet another name', 15, 3, 17.4);
  insert into mixed values (4, 'some name', 19, 4, 9.1);
  insert into mixed values (5, 'what name', 21, 8, 12.3);
end;

create procedure update_mixed(id_ int not null, rate_ real not null)
begin
  update mixed set rate = rate_ where id = id_;
end;

@attribute(cql:identity=(id, code))
create procedure get_mixed(lim integer not null)
begin
  select * from mixed order by id limit lim;
end;

declare select func rtrim(x text) text;

create procedure mandelbrot()
begin
 declare C cursor for
    WITH RECURSIVE
      xaxis(x) AS (select -2.0 UNION ALL SELECT x+0.05 FROM xaxis WHERE x<1.2),
      yaxis(y) AS (select -1.0 UNION ALL SELECT y+0.1 FROM yaxis WHERE y<1.0),
      m(iter, cx, cy, x, y) AS (
	SELECT 0 iter, x cx, y cy, 0.0 x, 0.0 y FROM xaxis, yaxis
	UNION ALL
	SELECT iter+1 iter, cx, cy, x*x-y*y + cx x, 2.0*x*y + cy y FROM m
	 WHERE (x*x + y*y) < 4.0 AND iter<28
      ),
      m2(iter, cx, cy) AS (
	SELECT max(iter), cx, cy FROM m GROUP BY cx, cy
      ),
      a(t) AS (
	SELECT group_concat( substr(' .+*#', 1 + min(iter/7, 4), 1), '')
	FROM m2 GROUP BY cy
      )
    SELECT group_concat(rtrim(t),"\n") line FROM a;

   loop fetch C
   begin
     call printf("%s\n", C.line);
   end;
end;

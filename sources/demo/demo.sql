/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- demo stored procs

DECLARE PROC printf NO CHECK;

CREATE PROC make_mixed ()
BEGIN
  CREATE TABLE mixed(
    id INTEGER NOT NULL,
    name TEXT,
    code LONG_INT,
    flag BOOL,
    rate REAL
  );
END;

CREATE PROC load_mixed ()
BEGIN
  DELETE FROM mixed;
  INSERT INTO mixed VALUES(1, 'a name', 12, 1, 5.0);
  INSERT INTO mixed VALUES(2, 'some name', 14, 3, 7.0);
  INSERT INTO mixed VALUES(3, 'yet another name', 15, 3, 17.4);
  INSERT INTO mixed VALUES(4, 'some name', 19, 4, 9.1);
  INSERT INTO mixed VALUES(5, 'what name', 21, 8, 12.3);
END;

CREATE PROC update_mixed (id_ INTEGER NOT NULL, rate_ REAL NOT NULL)
BEGIN
  UPDATE mixed
  SET rate = rate_
    WHERE id = id_;
END;

@ATTRIBUTE(cql:identity=(id, code))
@ATTRIBUTE(cql:generate_copy)
CREATE PROC get_mixed (lim INTEGER NOT NULL)
BEGIN
  SELECT *
    FROM mixed
  ORDER BY id
  LIMIT lim;
END;

CREATE PROC mandelbrot ()
BEGIN
  DECLARE C CURSOR FOR WITH RECURSIVE
  xaxis (x) AS ( SELECT -2.0 UNION ALL SELECT x + 0.05 FROM xaxis WHERE x < 1.2),
  yaxis (y) AS ( SELECT -1.0 UNION ALL SELECT y + 0.1 FROM yaxis WHERE y < 1.0),
  m (iter, cx, cy, x, y) AS (
    SELECT 0 AS iter, x AS cx, y AS cy, 0.0 AS x, 0.0 AS y
      FROM xaxis, yaxis
    UNION ALL
    SELECT iter + 1 AS iter, cx, cy, x * x - y * y + cx AS x, 2.0 * x * y + cy AS y
      FROM m
      WHERE m.x * m.x + m.y * m.y < 4.0 AND m.iter < 28),
  m2 (iter, cx, cy) AS (
    SELECT max(iter), cx, cy
     FROM m
     GROUP BY cx, cy),
  a (t) AS (
    SELECT group_concat(substr(' .+*#', 1 + min(iter / 7, 4), 1), '')
      FROM m2
      GROUP BY cy)
  SELECT group_concat(rtrim(t), "\n") AS line
    FROM a;

  LOOP FETCH C
  BEGIN
    CALL printf("%s\n", C.line);
  END;
END;

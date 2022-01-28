/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @create/@delete tables */
CREATE TABLE test_create_table_A(
  colA    INT,
  colB    LONG INT,
  colC    TEXT,
  colD    TEXT @create(1)
) @delete(2);

CREATE TABLE test_create_table_B(
  colA    TEXT,
  colB    LONG INT,
  colC    INT
) @create(1) @delete(3);

CREATE TABLE test_create_table_C(
  colA    TEXT,
  colB    LONG INT,
  colC    LONG INT
) @create(2);

CREATE VIEW test_view AS SELECT colA, colB FROM test_create_table_C;

/* @recreate tables */
CREATE TABLE test_recreate_table_A(
  colA    INT,
  colC    TEXT,
  colD    TEXT
) @recreate @delete;

CREATE TABLE test_recreate_table_B(
  colA    INT,
  colB    LONG INT,
  colC    INT
) @recreate;

-- make a recreate-group with an FK dependency (legal)

CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT
) @recreate(gr1);

CREATE TABLE use_g1(
  id INTEGER PRIMARY KEY REFERENCES g1(id),
  name2 TEXT
) @recreate(gr1);

-- deleted in version 3
CREATE TABLE test_this_table_will_become_create(
  id integer primary key
) @create(1, cql:from_recreate) @delete(3);

-- additional items that will not disappear even in exclusive mode
CREATE VIEW staying_view AS SELECT * FROM g1;

CREATE INDEX staying_index ON g1(id);

CREATE TRIGGER staying_trigger BEFORE DELETE ON g1 BEGIN SELECT 1; END;

-- extra items removed entirely
-- this file is compiled in exclusive schema mode so that's ok

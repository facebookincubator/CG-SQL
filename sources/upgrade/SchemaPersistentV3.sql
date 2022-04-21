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

-- we will be unsubscribing and resubscribing this table in later versions
CREATE TABLE test_for_unsub(
  unsub_id integer,
  x text
);

-- making an index for the table, this will appear and disappear
CREATE INDEX test_for_unsub_index ON test_for_unsub(x);

-- making a trigger for the table, this will appear and disappear
CREATE TRIGGER test_for_unsub_trigger
  BEFORE DELETE ON test_for_unsub
  WHEN old.unsub_id = 3
BEGIN
  DELETE FROM test_for_unsub WHERE unsub_id = 3;
END;

-- we will be unsubscribing and resubscribing this table in later versions
CREATE TABLE recreate_test_for_unsub(
  unsub_id integer,
  x text
) @recreate(a_recreate_group);

-- making an index for the table, this will appear and disappear
CREATE INDEX recreate_test_for_unsub_index ON recreate_test_for_unsub(x);

-- making a trigger for the table, this will appear and disappear
CREATE TRIGGER recreate_test_for_unsub_trigger
  BEFORE DELETE ON recreate_test_for_unsub
  WHEN old.unsub_id = 3
BEGIN
  DELETE FROM recreate_test_for_unsub WHERE unsub_id = 3;
END;

@unsub(1, test_for_unsub);
@unsub(1, recreate_test_for_unsub);
@resub(2, test_for_unsub);
@resub(2, recreate_test_for_unsub);
@unsub(3, test_for_unsub);
@unsub(3, recreate_test_for_unsub);

-- extra items that will disappear when we switch to exclusive mode
CREATE VIEW extra_view AS SELECT * FROM g1;

CREATE INDEX extra_index ON g1(id);

CREATE TRIGGER extra_trigger BEFORE DELETE ON g1 BEGIN SELECT 1; END;

-- additional items that will not disappear even in exclusive mode
CREATE VIEW staying_view AS SELECT * FROM g1;

CREATE INDEX staying_index ON g1(id);

CREATE TRIGGER staying_trigger BEFORE DELETE ON g1 BEGIN SELECT 1; END;

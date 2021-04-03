/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @create/@delete tables */
CREATE TABLE test_create_table_A(
  colA    INT,
  colB    LONG INT,
  colC    TEXT
);

/* @recreate tables */
CREATE TABLE test_recreate_table_A(
  colA    INT,
  colB    LONG INT,
  colC    TEXT
) @recreate;

CREATE TRIGGER tr__dummy
  BEFORE DELETE ON test_create_table_A
BEGIN
  SELECT 1;
END;

-- make a recreate-group with an FK dependency (legal)
CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT
) @recreate(gr1);

CREATE TABLE use_g1(
  id INTEGER PRIMARY KEY REFERENCES g1(id),
  name2 TEXT
) @recreate(gr1);

-- we will migrate this table to create in version 1
CREATE TABLE test_this_table_will_become_create(
  xyzzy integer -- we'll change the table in a significant way
) @recreate;

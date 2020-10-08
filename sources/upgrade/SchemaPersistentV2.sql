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
  colC    TEXT,
  colD    TEXT @create(1)
) @delete(2);

CREATE TABLE test_create_table_B(
  colA    TEXT,
  colB    LONG INT,
  colC    INT
) @create(1);

CREATE TABLE test_create_table_C(
  colA    TEXT,
  colB    LONG INT,
  colC    LONG INT
) @create(2);

/* @recreate tables */
CREATE TABLE test_recreate_table_A(
  colA    INT,
  colC    TEXT,
  colD    TEXT
) @delete(2);

CREATE TABLE test_recreate_table_B(
  colA    INT,
  colB    LONG INT,
  colC    INT
) @recreate;

// make a recreate-group with an FK dependency (legal)

CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT
) @recreate(gr1);

CREATE TABLE use_g1(
  id INTEGER PRIMARY KEY REFERENCES g1(id),
  name2 TEXT
) @recreate(gr1);

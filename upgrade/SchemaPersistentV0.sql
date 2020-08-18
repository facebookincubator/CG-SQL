-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

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

// make a recreate-group with an FK dependency (legal)
CREATE TABLE g1(
  id INTEGER PRIMARY KEY,
  name TEXT
) @recreate(gr1);

CREATE TABLE use_g1(
  id INTEGER PRIMARY KEY REFERENCES g1(id),
  name2 TEXT
) @recreate(gr1);

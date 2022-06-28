/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

DECLARE PROC printf NO CHECK;

CREATE PROC make_schema ()
BEGIN
  CREATE TABLE my_table(
    str text
  );
END;

CREATE PROC go ()
BEGIN
  call make_schema();

  insert into my_table
    values
     ("Hello from CQL."),
     ("Edit as you please.");

  declare C cursor for select * from my_table;
  loop fetch C
  begin
    call printf("%s\n", C.str);
  end;
END;

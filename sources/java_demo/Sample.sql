/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- Any kind of child result set will do the job for this test
-- note that with the json based code generation you can have
-- as many procs per file as you like. 
create proc Child(i integer not null)
begin
  declare C cursor like (x integer not null, y text not null);
  let j := 0;
  while j < i
  begin
    set j := j + 1;
    fetch C using
       j x,
       printf("<< %d >>", j)  y;
    out union C;
  end;
end;

/* this is a demo procedure, it's rather silly... */
@attribute(cql:vault_sensitive)
@attribute(cql:custom_type_for_encoded_column)
create proc Sample()
begin
  /* add the table we will be using */
  create table my_data(
    name text,
    age integer @sensitive,
    thing real,
    bytes blob,
    key1 text,
    key2 text @sensitive);

  /* insert some data */
  declare i integer not null;
  set i := 0;
  while (i < 5)
  begin
    /* avoiding @dummy_seed even though it's perfect here just so that
     * we don't take a dependency on the printf sqlite function.  If
     * your sqlite is very old you won't have that and we don't want the
     * JNI test to fail just because of a printf
     */
    insert into my_data using 
      "name_"||i AS name,
      i AS age, 
      i AS thing, 
      cast("blob_"||i as blob) AS bytes,
      "code_1"||i AS key1,
      "code_2"||i AS key2;
    set i := i + 1;
  end;

  set i := 0;
  /* the result will have a variety of data types to exercise the JNI helpers */
  declare C cursor for select * from my_data;
  loop fetch C
  begin
    declare result cursor like (like C, my_child_result object<Child set>);
    fetch result from values(from C, Child(i));
    out union result;
    set i := i + 1;
  end;
end;

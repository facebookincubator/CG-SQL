/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* this is a demo procedure, it's rather silly... */
@attribute(cql:vault_sensitive)
create proc Sample()
begin
  /* add the table we will be using */
  create table my_data(
    name text,
    age integer @sensitive,
    thing real,
    bytes blob);

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
    insert into my_data values("name_"||i, i, i, cast("blob_"||i as blob));
    set i := i + 1;
  end;

  /* the result will have a variety of data types to exercise the JNI helpers */
  select * from my_data;
end;

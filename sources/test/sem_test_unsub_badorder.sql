/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create table foo(id integer);

create table bar(id integer) @create(10);

-- TEST: this unsub must happen at v10
-- + {schema_unsub_stmt}: err
-- + error: % new @unsub/@resub must be added at version 10 or later
-- +1 error:
@unsub(2, foo);


------------------------------------------------------------------------------------------------------------
@previous_schema;
------------------------------------------------------------------------------------------------------------

create table foo(id integer);

create table bar(id integer) @create(10);

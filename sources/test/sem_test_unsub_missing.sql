/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

create table foo(id integer);

-- TEST: all good, one good unsub
-- - error:
@unsub(2, foo);

------------------------------------------------------------------------------------------------------------
@previous_schema;
------------------------------------------------------------------------------------------------------------

create table foo(id integer);

-- TEST: first entry matches
-- + {schema_unsub_stmt}: ok
-- - error:
@unsub(2, foo);

-- TEST: second entry is missing
-- + {schema_resub_stmt}: err
-- + error: % previous schema had more unsub/resub directives than the current schema
-- +1 error:
@resub(3, foo);

-- TEST: after this we stop reporting errors of this type to avoid spam
-- + {schema_unsub_stmt}: ok
-- - error:
@unsub(3, foo);


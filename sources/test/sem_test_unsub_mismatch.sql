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

-- TEST: still good, no worries
-- - error:
@resub(3, foo);

------------------------------------------------------------------------------------------------------------
@previous_schema;
------------------------------------------------------------------------------------------------------------

create table foo(id integer);

-- TEST: mismatched directive
-- + {schema_unsub_stmt}: err
-- + error: % @unsub/@resub directives did not match between current and previous schema
-- + looking for @unsub (1, foo)
-- + found @unsub(2, foo)
-- +1 error:
@unsub(1, foo);

-- TEST: after this we stop reporting errors of this type to avoid spam
-- + {schema_resub_stmt}: ok
-- - error:
@resub(3, foo);


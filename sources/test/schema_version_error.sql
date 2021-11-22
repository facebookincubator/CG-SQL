/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST : set up for schema upgrade
-- + @SCHEMA_UPGRADE (2);
-- + {schema_upgrade_version_stmt}: ok
-- + {int 2}
-- - Error
@schema_upgrade_version(2);

-- TEST : double declaration
-- + {schema_upgrade_version_stmt}: err
-- + error: % schema upgrade version declaration may only appear once
-- +1 error:
@schema_upgrade_version(2);

create view X as select 1 a, 2 b;

--  TEST: try to use a view in a proc in a migration scenario
-- + {create_proc_stmt}: err
-- + error: % table/view not defined (view hidden in migration script) 'X'
-- +1 error:
create proc p()
begin
  select * from X;
end;

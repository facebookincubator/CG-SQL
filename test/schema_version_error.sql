-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

-- TEST : set up for schema upgrade
-- + @SCHEMA_UPGRADE (2);
-- + {schema_upgrade_version_stmt}: ok
-- + {int 2}
-- - Error
@schema_upgrade_version(2);

-- TEST : double declaration
-- + {schema_upgrade_version_stmt}: err
-- + Error % schema upgrade version declaration may only appear once
-- +1 Error
@schema_upgrade_version(2);

create view X as select 1 a, 2 b;

--  TEST: try to use a view in a proc in a migration scenario
-- + {create_proc_stmt}: err
-- + Error % table/view not defined (view hidden in migration script) 'X'
-- +1 Error
create proc p()
begin
  select * from X;
end;

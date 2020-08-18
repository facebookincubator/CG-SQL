-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

-- TEST: explain not supported
-- + {explain_stmt}: err
-- + {int 1}
-- + Error % Explain statement is only available in dev mode because its result set may vary between sqlite versions
-- +1 Error
explain select 1;

-- TEST: explain query plan with select
-- + {explain_stmt}: err
-- + {int 2}
-- + Error % Explain statement is only available in dev mode because its result set may vary between sqlite versions
-- +1 Error
explain query plan select * from foo inner join bar where foo.id = 1;

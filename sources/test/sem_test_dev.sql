/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

-- TEST: explain not supported
-- + {explain_stmt}: err
-- + {int 1}
-- + error: % Explain statement is only available in dev mode because its result set may vary between sqlite versions
-- +1 error:
explain select 1;

-- TEST: explain query plan with select
-- + {explain_stmt}: err
-- + {int 2}
-- + error: % Explain statement is only available in dev mode because its result set may vary between sqlite versions
-- +1 error:
explain query plan select * from foo inner join bar where foo.id = 1;

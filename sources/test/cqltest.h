/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare tests integer not null;
declare tests_passed integer not null;
declare fails integer not null;
declare expectations integer not null;
declare function get_outstanding_refs() integer not null;
declare start_refs integer not null;
declare end_refs integer not null;

#define EXPECT(pred) call errcheck(pred, #pred, __LINE__)

#define BEGIN_SUITE()

#define END_SUITE() call end_suite();

#define BEGIN_TEST(x) \
  create procedure test_##x() \
  begin \
  begin try \
  set tests := tests + 1; \
  declare starting_fails integer not null; \
  set starting_fails := fails;

#define END_TEST(x) \
  end try; \
  begin catch \
    call printf("%s had a db error\n", #x); \
    set fails := fails + 1; \
    throw; \
  end catch; \
  if starting_fails != fails then \
    call printf("%s failed.\n", #x); \
  else \
    set tests_passed := tests_passed + 1; \
  end if; \
end; \
set start_refs := get_outstanding_refs(); \
call test_##x(); \
set end_refs := get_outstanding_refs(); \
if start_refs != end_refs then \
  call printf("Test %s unbalanced refs.  Starting refs %d, ending refs %d.\n", #x, start_refs, end_refs); \
  set fails := fails + 1; \
end if;

create procedure errcheck(passed bool @sensitive, message text, line integer not null)
begin
  set expectations := expectations + 1;
  if not coalesce(passed, 0) then
    call printf("test: %s: FAIL on line %d\n", message, line);
    set fails := fails + 1;
  end if;
end;

create procedure end_suite()
begin
  call printf("%d tests executed. %d passed, %d failed.  %d expectations failed of %d.\n",
    tests, tests_passed, tests - tests_passed, fails, expectations);
  call exit(fails);
end;

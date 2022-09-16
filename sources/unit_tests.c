/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_UNIT_TESTS)

// stubs to avoid link errors
cql_noexport void run_unit_tests() {}

#else

#include "cql.h"
#include "cg_common.h"
#include "unit_tests.h"

// This file implement very simple unit tests for functions that are too complicated
// to test directly through invocations of the CQL tool.
//
// This test suite is extremely simple and it does not (purposefully) use common
// test infrastructure such as gtest or gmock. This is just a simple C program
// that calls test functions and asserts their results on every step.

#define TEST_ASSERT assert
#define STR_EQ(s1, s2) strcmp(s1, s2) == 0

cql_noexport void cg_c_init(void);
cql_noexport void cg_c_cleanup(void);
cql_noexport uint32_t cg_statement_pieces(CSTR in, charbuf *output);

static bool test_frag_tricky_case() {
  options.compress = 1;
  CHARBUF_OPEN(tmp);
  cg_c_init();
  // get into a state with a single trailing space
  uint32_t count = cg_statement_pieces("atest btest ", &tmp);
  cg_c_cleanup();
  CHARBUF_CLOSE(tmp);

  // two tokens, no going off the end and making extra tokens!
  return count == 2;
}

static bool test_Strdup__empty_string() {
  char* str_copy = Strdup("");
  bool result = STR_EQ(str_copy, "");
  return result;
}

static bool test_Strdup__one_character_string() {
  char* str_copy = Strdup("a");
  bool result = STR_EQ(str_copy, "a");
  return result;
}

static bool test_Strdup__long_string() {
  char* str_copy = Strdup("abcd");
  bool result = STR_EQ(str_copy, "abcd");
  return result;
}

static bool test_Strcasecmp__empty_strings() {
  return Strcasecmp("", "") == 0;
}

static bool test_Strcasecmp__one_char_strings__result_is_less_than() {
  return Strcasecmp("a", "B") < 0;
}

static bool test_Strcasecmp__one_char_strings__result_is_greater_than() {
  return Strcasecmp("B", "a") > 0;
}

static bool test_Strcasecmp__one_char_strings__result_is_equals() {
  return Strcasecmp("Aab", "aaB") == 0;
}

static bool test_Strcasecmp__long_strings__result_is_less_than() {
  return Strcasecmp("aca", "acD") < 0;
}

static bool test_Strcasecmp__long_strings__result_is_greater_than() {
  return Strcasecmp("bab", "baA") > 0;
}

static bool test_Strcasecmp__long_strings__result_is_equals() {
  return Strcasecmp("Aab", "aaB") == 0;
}

static bool test_Strcasecmp__different_length_strings__result_is_less_than() {
  return Strcasecmp("aab", "AABc") < 0;
}

static bool test_Strcasecmp__different_length_strings__result_is_greater_than() {
  return Strcasecmp("AABc", "aab") > 0;
}

static bool test_Strncasecmp__empty_strings__zero_cmp_size__result_is_equals() {
  return Strncasecmp("", "", 0) == 0;
}

static bool test_Strncasecmp__empty_strings__past_length_cmp_size__result_is_equals() {
  return Strncasecmp("", "", 1) == 0;
}

static bool test_Strncasecmp__one_char_strings__zero_cmp_size__result_is_equals() {
  return Strncasecmp("a", "b", 0) == 0;
}

static bool test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_less_than() {
  return Strncasecmp("a", "B", 2) < 0;
}

static bool test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_greater_than() {
  return Strncasecmp("B", "a", 2) > 0;
}

static bool test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_equals() {
  return Strncasecmp("B", "b", 2) == 0;
}

static bool test_Strncasecmp__long_strings__past_length_cmp_size__result_is_less_than() {
  return Strncasecmp("aca", "acD", 4) < 0;
}

static bool test_Strncasecmp__long_strings__past_length_cmp_size__result_is_greater_than() {
  return Strncasecmp("bab", "baA", 4) > 0;
}

static bool test_Strncasecmp__long_strings__past_length_cmp_size__result_is_equals() {
  return Strncasecmp("Aab", "aaB", 4) == 0;
}

static bool test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_less_than() {
  return Strncasecmp("abd", "Aca", 2) < 0;
}

static bool test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_greater_than() {
  return Strncasecmp("Bbd", "baa", 2) > 0;
}

static bool test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_equals() {
  return Strncasecmp("Aac", "aaB", 2) == 0;
}

static bool test_sha256_example1() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "Foo:x:String");
  bool result = sha256_charbuf(&temp) == -5028419846961717871L;
  CHARBUF_CLOSE(temp);
  return result;
}

static bool test_sha256_example2() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "id:?Int64");
  bool result = sha256_charbuf(&temp) == -9155171551243524439L;
  CHARBUF_CLOSE(temp);
  return result;
}

static bool test_sha256_example3() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "x:String");
  bool result = sha256_charbuf(&temp) == -6620767298254076690L;
  CHARBUF_CLOSE(temp);
  return result;
}

static bool test_sha256_example4() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "fooBar:?Int64");
  bool result = sha256_charbuf(&temp) == -6345014076009057275L;
  CHARBUF_CLOSE(temp);
  return result;
}

static bool test_sha256_example5() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.");
  bool result = sha256_charbuf(&temp) == -8121930428982087348L;
  CHARBUF_CLOSE(temp);
  return result;
}

static bool test_sha256_example6() {
  CHARBUF_OPEN(temp);
  bprintf(&temp, "XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.XXXXXXXXX.123456789");
  bool result = sha256_charbuf(&temp) ==  -4563262961718308998L;
  CHARBUF_CLOSE(temp);
  return result;
}

cql_noexport void run_unit_tests() {
  TEST_ASSERT(test_Strdup__empty_string());
  TEST_ASSERT(test_Strdup__one_character_string());
  TEST_ASSERT(test_Strdup__long_string());
  TEST_ASSERT(test_Strcasecmp__empty_strings());
  TEST_ASSERT(test_Strcasecmp__one_char_strings__result_is_less_than());
  TEST_ASSERT(test_Strcasecmp__one_char_strings__result_is_greater_than());
  TEST_ASSERT(test_Strcasecmp__one_char_strings__result_is_equals());
  TEST_ASSERT(test_Strcasecmp__long_strings__result_is_less_than());
  TEST_ASSERT(test_Strcasecmp__long_strings__result_is_greater_than());
  TEST_ASSERT(test_Strcasecmp__long_strings__result_is_equals());
  TEST_ASSERT(test_Strcasecmp__different_length_strings__result_is_less_than());
  TEST_ASSERT(test_Strcasecmp__different_length_strings__result_is_greater_than());
  TEST_ASSERT(test_Strncasecmp__empty_strings__zero_cmp_size__result_is_equals());
  TEST_ASSERT(test_Strncasecmp__empty_strings__past_length_cmp_size__result_is_equals());
  TEST_ASSERT(test_Strncasecmp__one_char_strings__zero_cmp_size__result_is_equals());
  TEST_ASSERT(test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_less_than());
  TEST_ASSERT(test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_greater_than());
  TEST_ASSERT(test_Strncasecmp__one_char_strings__past_length_cmp_size__result_is_equals());
  TEST_ASSERT(test_Strncasecmp__long_strings__past_length_cmp_size__result_is_less_than());
  TEST_ASSERT(test_Strncasecmp__long_strings__past_length_cmp_size__result_is_greater_than());
  TEST_ASSERT(test_Strncasecmp__long_strings__past_length_cmp_size__result_is_equals());
  TEST_ASSERT(test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_less_than());
  TEST_ASSERT(test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_greater_than());
  TEST_ASSERT(test_Strncasecmp__long_strings__shorter_than_length_cmp_size__result_is_equals());
  TEST_ASSERT(test_frag_tricky_case());
  TEST_ASSERT(test_sha256_example1());
  TEST_ASSERT(test_sha256_example2());
  TEST_ASSERT(test_sha256_example3());
  TEST_ASSERT(test_sha256_example4());
  TEST_ASSERT(test_sha256_example5());
  TEST_ASSERT(test_sha256_example6());
}

#endif

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "compat.h"
#include "minipool.h"

cql_noexport char *_Nonnull Strdup(const char *_Nonnull s) {
  uint32_t length = (uint32_t)(strlen(s) + 1);
  void *result = minipool_alloc(str_pool, length);
  Invariant(result);
  return (char *)memcpy(result, s, length);
}

cql_noexport int32_t Strcasecmp(const char *_Nonnull s1, const char *_Nonnull s2) {
  const char *p1 = s1;
  const char *p2 = s2;
  int32_t result;
  if (p1 == p2)
    return 0;
  while ((result = Tolower(*p1) - Tolower(*p2++)) == 0)
    if (*p1++ == '\0')
      break;
  return result;
}

cql_noexport int32_t Strncasecmp(const char *_Nonnull s1, const char *_Nonnull s2, size_t n) {
  const char *p1 = s1;
  const char *p2 = s2;
  int32_t result = 0;

  for (; n != 0; --n) {
    if ((result = Tolower(*p1) - Tolower(*p2++)) != 0) {
        return result;
    }
    if (*p1++ == '\0')
        return 0;
  }
  return result;
}

cql_noexport int32_t Strendswith(const char *_Nonnull haystack, const char *_Nonnull needle) {
  size_t haystack_len = strlen(haystack);
  size_t needle_len = strlen(needle);

  return (haystack_len >= needle_len) &&
         (!Strncasecmp(haystack + haystack_len - needle_len, needle, needle_len));
}

cql_noexport bool_t Islower(char c) {
  return c >= 'a' && c <= 'z';
}

cql_noexport bool_t Isupper(char c) {
  return c >= 'A' && c <= 'Z';
}

cql_noexport bool_t Isalpha(char c) {
  return Islower(c) || Isupper(c);
}

cql_noexport bool_t Isdigit(char c) {
  return c >= '0' && c <= '9';
}

cql_noexport bool_t Isxdigit(char c) {
  return Isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

cql_noexport char Tolower(char c) {
  return Isupper(c) ? c + ('a' - 'A') : c;
}

cql_noexport char Toupper(char c) {
  return Islower(c) ? c - ('a' - 'A') : c;
}

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  const unsigned char *p1 = (const unsigned char *) s1;
  const unsigned char *p2 = (const unsigned char *) s2;
  int32_t result;
  if (p1 == p2)
    return 0;
  while ((result = tolower(*p1) - tolower(*p2++)) == 0)
    if (*p1++ == '\0')
      break;
  return result;
}

cql_noexport int32_t Strncasecmp(const char *_Nonnull s1, const char *_Nonnull s2, size_t n) {
  const unsigned char *p1 = (const unsigned char *) s1;
  const unsigned char *p2 = (const unsigned char *) s2;
  int32_t result = 0;

  for (; n != 0; --n) {
    if ((result = tolower(*p1) - tolower(*p2++)) != 0) {
        return result;
    }
    if (*p1++ == '\0')
        return 0;
  }
  return result;
}

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "bytebuf.h"

cql_noexport void bytebuf_open(bytebuf *_Nonnull buf) {
  buf->max = 0;
  buf->ptr = 0;
  buf->used = 0;
}

cql_noexport void bytebuf_close(bytebuf *_Nonnull buf) {
  free(buf->ptr);
  buf->max = 0;
  buf->ptr = NULL;
}

cql_noexport void *_Nonnull bytebuf_alloc(bytebuf *_Nonnull buf, uint32_t needed) {
  uint32_t avail = buf->max - buf->used;

  if (needed > avail) {
    buf->max += needed + BYTEBUF_GROWTH_SIZE;
    char *newptr = _new_array(char, buf->max);

    if (buf->used) memcpy(newptr, buf->ptr, buf->used);

    free(buf->ptr);
    buf->ptr = newptr;
  }

  void *result = buf->ptr + buf->used;
  buf->used += needed;
  return result;
}

cql_noexport void bytebuf_append(bytebuf *_Nonnull buf, const void *_Nonnull bytes, uint32_t count) {
  void *mem = bytebuf_alloc(buf, count);
  memcpy(mem, bytes, count);
}

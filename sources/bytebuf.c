/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "bytebuf.h"

// The open operations simply zeros the buffer contents
cql_noexport void bytebuf_open(bytebuf *_Nonnull buf) {
  buf->max = 0;
  buf->ptr = 0;
  buf->used = 0;
}

// The close operation releases any allocated memory
// Note that bytebuf itself is most often on the stack so it's not freed
// if the bytebuf was allocated it's owned by whoever allocated it.
cql_noexport void bytebuf_close(bytebuf *_Nonnull buf) {
  free(buf->ptr);
  buf->max = 0;
  buf->ptr = NULL;
}

// This creates the needed space in the bytebuf, the buffer grows if it has to.
// The newly allocated memory begins with garbage data..
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

// The most common operation is to append into an existing bytebuf, this is the alloc operation
// followed by a copy.  Callers often do other things like retain embedded pointers or whatnot
// but the bytebuf is below anything like that.
cql_noexport void bytebuf_append(bytebuf *_Nonnull buf, const void *_Nonnull bytes, uint32_t count) {
  void *mem = bytebuf_alloc(buf, count);
  memcpy(mem, bytes, count);
}

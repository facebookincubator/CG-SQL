/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This is a simple growable memory buffer, it may have arbitrary
// binary in it.  It's the peer to charbuf for bytes.

#pragma once

#include "cql.h"

#define BYTEBUF_GROWTH_SIZE 1024

typedef struct bytebuf {
  char *_Nullable ptr;   // pointer to stored data, if any
  uint32_t used;         // bytes used in current buffer
  uint32_t max;          // max bytes in current buffer
} bytebuf;

cql_noexport void bytebuf_open(bytebuf *_Nonnull buf);
cql_noexport void bytebuf_close(bytebuf *_Nonnull buf);
cql_noexport void *_Nonnull bytebuf_alloc(bytebuf *_Nonnull buf, uint32_t needed);
cql_noexport void bytebuf_append(bytebuf *_Nonnull buf, const void *_Nonnull bytes, uint32_t count);

// helper macro for new from a bufferß
#define bytebuf_new(buf, T) ((T*)bytebuf_alloc(buf, sizeof(T)))

// helper macro to append a normal variable
#define bytebuf_append_var(buf, var) bytebuf_append(buf, &var, sizeof(var))

// cleanup a non-null buffer
#define BYTEBUF_CLEANUP(b) if (b) {bytebuf_close(b); b = NULL; }

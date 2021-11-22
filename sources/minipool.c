/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cql.h"
#include "minipool.h"
#include <stdlib.h>

#define MAX(a,b) ((a >b ) ? a : b)

// This is the stupidest pool allocator ever, it's only useful for cases where everything lives
// until the end.  Currently it's used to hold ast nodes and duplicated strings.  It may hold
// more.  The old CQL strategy was to just let exit clean up everything since we were only
// an executable and we can't free the tree until we're exiting anyway.  However there is some
// desire to use CQL in library form now which means it has to be able to do a compile and
// then end up clean.  To help with this we make these ultra-dumb pools that simply keep 
// the allocated items together.  This means that we don't have to do zillions of seperate free
// calls when we're exiting.  It also helps with locality and fragmentation in the client.
// It's dumb as rocks.


// Make a pool node, set it's size to MINIBLOCK
cql_noexport void minipool_open(minipool **pool) {
  *pool = _new(minipool);
  (*pool)->bytes = malloc(MINIBLOCK);
  (*pool)->current = (*pool)->bytes;
  (*pool)->available = MINIBLOCK;
  (*pool)->next = NULL;
}

// Give back all the memory in the pool and nil out the pool pointer
// To accomplish this all we need to to is walk the chain of blocks
// freeing the bytes from each block as well as the minipool object.
cql_noexport void minipool_close(minipool **head) {
  minipool *pool = *head;
  while (pool) {
    minipool *next = pool->next;
    free(pool->bytes);
    free(pool);
    pool = next;
  }
  *head = NULL;
}

// Get needed memory; if the memory is not available then
// we allocate a new block and thread it into the list.  Note
// that this is different than the other CQL helper bytebuf.
// This allocation never moves bytes or copies bytes.  Once 
// you get memory it is valid until you close the buffer.
// Bytebuf cannot hold pointers because the structure moves when it
// grows and it is one continuous allocation so it's only for byte streams.
// Minipool doesn't move objects around so pointers remain valid.
cql_noexport void *minipool_alloc(minipool *pool, uint32_t needed) {
  void *result;
 
  // for alignment, all allocs will be on a 8 byte boundary
  needed += 7;
  needed &= u32_not(7);

  if (needed > pool->available) {
    // Make a copy of the most recent head node and link to it
    // we have to do this because the head of the pool never
    // changes once it's created.  Note that none of the byte
    // buffers are copied, only the minipool struct.  Once
    // we've copied it, we re-initialize the minipool to a size
    // that is at least big enough for the next allocation.
    minipool *old = malloc(sizeof(minipool));
    *old = *pool;
    uint32_t blocksize = MAX(needed, MINIBLOCK);
    pool->bytes = malloc(blocksize);
    pool->current = pool->bytes;
    pool->available = blocksize;
    pool->next = old;
  }

  // For sure safe to get the memory now, so get it.π
  result = pool->current;
  pool->current += needed;
  pool->available -= needed;
  return result;
}

static lazy_free *_Nullable lazy_frees;

cql_noexport void add_lazy_free(lazy_free *p) {
  p->next = lazy_frees;
  lazy_frees = p;
}

cql_noexport void run_lazy_frees() {
  lazy_free *head = lazy_frees;
  while (head) {
    lazy_free *next = head->next;
    head->teardown(head->context);
    free(head);
    head = next;
  }
  lazy_frees = NULL;
}

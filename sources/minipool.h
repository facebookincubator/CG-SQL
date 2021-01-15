/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

typedef struct minipool {
  struct minipool *_Nullable next;
  char *_Nullable bytes;
  char *_Nullable current;
  uint32_t available;
} minipool;

#define MINIBLOCK (1024*64)

cql_noexport void minipool_open(minipool *_Nullable *_Nonnull pool);
cql_noexport void minipool_close(minipool *_Nullable *_Nonnull pool);
cql_noexport void *_Nonnull minipool_alloc(minipool *_Nonnull pool, uint32_t needed);


// lazy free service for misc pool contents

typedef struct lazy_free {
  struct lazy_free *_Nullable next;
  void *_Nullable context;
  void (*_Nonnull teardown)(void *_Nullable context);
} lazy_free;

cql_noexport void add_lazy_free(lazy_free *_Nonnull p);
cql_noexport void run_lazy_frees(void);

#define _pool_new(p, x) ((x*)minipool_alloc(p, (int32_t)sizeof(x)))
#define _pool_new_array(p, x, c) ((x*)minipool_alloc(p, c*(int32_t)sizeof(x)))

#define _ast_pool_new(x) _pool_new(ast_pool, x)
#define _ast_pool_new_array(x, c) _pool_new_array(ast_pool, x, c)

cql_data_decl( minipool *_Nullable ast_pool );
cql_data_decl( minipool *_Nullable str_pool );

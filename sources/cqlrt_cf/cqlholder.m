/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cqlrt_cf.h"

// CQLHolder is entirly private

@interface CQLHolder : NSObject

@property (nonatomic) void *bytes;
@property (nonatomic) int type;

@end

@implementation CQLHolder

- (instancetype)initWithBytes:(void *)bytesIn andType:(int)typeIn
{
  self = [super init];
  self.bytes = bytesIn;
  self.type = typeIn;
  return self;
}

- (void)dealloc
{
  if (self.bytes)
  {
    [self dynamicTeardown];
  }
  [super dealloc];
}

// The two kinds of things we know how to hold.

#define CF_HELD_TYPE_RESULT_SET 1
#define CF_HELD_TYPE_BOXED_STMT 2

- (void)dynamicTeardown
{
  switch (self.type) {
  case CF_HELD_TYPE_RESULT_SET:
    {
      cql_result_set_ref ref = (__bridge cql_result_set_ref)self;
      cql_result_set *result_set = (cql_result_set *)self.bytes;
      result_set->meta.teardown(ref);
      break;
    }

  case CF_HELD_TYPE_BOXED_STMT:
    {
      cql_boxed_stmt *box = (cql_boxed_stmt *)self.bytes;

      // note this is a no-op on null statements, and nulls stmt after finalization
      cql_finalize_stmt(&box->stmt);  
      break;
    }
  }

  free(self.bytes);
  self.bytes = NULL;
  self.type = 0;
}

@end

// The public interface is C, the CQLHolder object is a detail.

// For holding result sets.

cql_result_set_ref _Nonnull cql_result_set_create(
  void *_Nonnull data,
  cql_int32 count,
  cql_result_set_meta meta)
{
  cql_result_set *result_set = malloc(sizeof(cql_result_set));
  result_set->count = count;
  result_set->data = data;
  result_set->meta = meta;

  CQLHolder *holder = [[CQLHolder alloc] initWithBytes:(void *)result_set andType:CF_HELD_TYPE_RESULT_SET];
  return (__bridge cql_result_set_ref)holder;
}

cql_object_ref _Nonnull cql_box_stmt(sqlite3_stmt *_Nullable stmt)
{
  cql_boxed_stmt *box = (cql_boxed_stmt *)malloc(sizeof(cql_boxed_stmt));
  box->stmt = stmt;

  CQLHolder *holder = [[CQLHolder alloc] initWithBytes:(void *)box andType:CF_HELD_TYPE_BOXED_STMT];
  return (__bridge cql_object_ref)holder;
}

// For holding boxed statements.

sqlite3_stmt *_Nullable cql_unbox_stmt(cql_object_ref _Nonnull ref)
{
  CQLHolder *holder = (__bridge CQLHolder *)ref;
  cql_boxed_stmt *box = (cql_boxed_stmt *)holder.bytes;
  return box->stmt;
}

cql_result_set *_Nonnull cql_get_result_set_from_ref(cql_result_set_ref _Nonnull ref)
{ 
  CQLHolder *holder = (__bridge CQLHolder *)ref;
  cql_result_set *_Nonnull result_set = (cql_result_set *)holder.bytes;
  return result_set;
}


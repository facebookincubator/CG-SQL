/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <sqlite3.h>

#import "demo_objc.h"

int main(int argc, char **argv)
{

  @autoreleasepool {

    // Note: not exactly world class error handling but that isn't the point

    // create a db
    sqlite3 *db;
    int rc = sqlite3_open(":memory:", &db);
    if (rc != SQLITE_OK) {
      exit(1);
    }

    // make schema if needed (it always will be here because memory databases begin empty
    rc = todo_create_tables(db);
     if (rc != SQLITE_OK) {
      exit(2);
    }

    // add some tasks
    const char *const default_tasks[] = {
      "Buy milk",
      "Walk dog",
      "Write code"
    };

    for (int i = 0; i < 3; i++) {
      // note we make a string reference from a c string here
      cql_string_ref dtask = cql_string_ref_new(default_tasks[i]);
      rc = todo_add(db, dtask);
      cql_string_release(dtask); // and then dispose of the reference
      if (rc != SQLITE_OK) {
        exit(3);
      }
    }

    // mark a task as done
    rc = todo_setdone_(db, 1, true);
    if (rc != SQLITE_OK) {
      exit(4);
    }

    // delete a row in the middle, rowid = 2
    rc = todo_delete(db, 2);
    if (rc != SQLITE_OK) {
      exit(5);
    }

    // select out some results
    todo_tasks_result_set_ref result_set_ref;
    rc = todo_tasks_fetch_results(db, &result_set_ref);
    if (rc != SQLITE_OK) {
      printf("error: %d\n", rc);
      exit(2);
    }

    CGS_todo_tasks *rs = CGS_todo_tasks_from_todo_tasks(result_set_ref);
    cql_release(result_set_ref);  // objective C is now in control of the refs

    // get result count
    cql_int32 result_count = CGS_todo_tasks_result_count(rs);

    // loop to print
    for (cql_int32 row = 0; row < result_count; row++) {
      // note the usual "get" semantics, no retain here
      NSString *text = CGS_todo_tasks_get_description(rs, row);
      cql_bool done = CGS_todo_tasks_get_done(rs, row);
      cql_int64 rowid = CGS_todo_tasks_get_rowid(rs, row);

      NSLog(@"%d: rowid:%lld %@ (%s)\n", row, rowid, text, done ? "done" : "not done");
    }

    // and close the database
    sqlite3_close(db);
  }
}

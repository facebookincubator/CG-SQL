#include <stdlib.h>
#include <sqlite3.h>

#import "demo_objc.h"

int main(int argc, char **argv)
{
  /* Note: not exactly world class error handling but that isn't the point */


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

  CGS_todo_tasks *rs = NULL;

  // select out some results
  todo_tasks_result_set_ref result_set;
  rc = todo_tasks_fetch_results(db, &result_set);
  if (rc != SQLITE_OK) {
    printf("error: %d\n", rc);
    exit(2);
  }
  
  // Note that this is not a create/copy method, objc knows
  // so rs will not be released. You have to use ARC tricks
  // if you want a retain here but usually you don't.  You
  // can simply pass rs around at this point and it will retain
  // if it needs to in the usual way.
  rs = CGS_todo_tasks_from_todo_tasks(result_set);
  
  // get result count
  cql_int32 result_count = CGS_todo_tasks_result_count(rs);
  
  // loop to print
  for(cql_int32 row = 0; row < result_count; row++) {
    // note the usual "get" semantics, no retain here
    NSString *text = CGS_todo_tasks_get_description(rs, row);
    cql_bool done = CGS_todo_tasks_get_done(rs, row);
    cql_int32 rowid = CGS_todo_tasks_get_rowid(rs, row);
  
    NSLog(@"%d: rowid:%d %@ (%s)\n", row, rowid, text, done ? "done" : "not done");
  }

  // this is going to cause the dealloc to run (rs = NULL won't do the job)
  cql_release(result_set);

  // and close the database
  sqlite3_close(db);
}

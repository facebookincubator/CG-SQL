---
slug: result-variable
title: Introducing @RC builtin variable
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

We've long needed a way to see the most recent SQLite result code SQLite in the context
of say a `catch` block (most other times you can assume SQLITE_OK was the last
result code otherwise control flow would transfer elsewhere. Sometimes SQLITE_ROW
or SQLITE_DONE might be the current result code.

Soon we'll provide a sample header that declares the most common error codes in an enum but
for now you could do something like this:

```sql
-- pasted from the sqlite.c
#define SQLITE_BUSY         5   /* The database file is locked */

-- this is a contrived example
create proc get_first_foo(out can_retry bool not null)
begin

  -- can_retry is set to 0 automatically, language semantics guarantee this

  begin try
    select foo from bar limit 1;
  end try;
  begin catch
    set can_retry := (@rc == SQLITE_BUSY);
    throw; -- rethrow the original error
  end catch;
end;
```

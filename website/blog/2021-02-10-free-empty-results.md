---
slug: type-kinds-intro
title: Introducing Type "Kinds"
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

Important change in CQL semantics.

Previously if you did an early return, or fall through the end, from a procedure that is supposed to return a result set
but did not in fact provide one, you would get a fake SQLITE_ERROR.  Now you get an empty result set for "free".

This interpretation seems much more natural and avoids a lot of really annoying stub selects to comply with the contract.

This also works for the `out` statement in the same fashion.

If you want to return an error, use `throw`. This is a lot more natural...

examples:

```sql
-- this gives you an empty result set if x <= 0
create proc maybe_return(x integer)
begin
   if x > 0 then
     select * from foo where foo.y > x;
   end if;
end;

-- so does this
create proc maybe_return(x integer)
begin
  if x <= 0 then
     return;
  end if;
  select * from foo where foo.y > x;
end;

-- so does this
create proc maybe_out(x integer)
begin
  if x <= 0 then
    declare C cursor for select etc.
    out C;
  end if;
end;
```

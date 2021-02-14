---
slug: select-if-nothing
title: Introducing Select .. If Nothing
author: CG/SQL Team
author_title: Maintainer of CG/SQL
author_url: https://github.com/facebookincubator
author_image_url: https://avatars2.githubusercontent.com/u/69631?s=200&v=4
tags: [facebook, cg-sql]
---

The nested select statement is frequently misused, in particular if you get no rows back from the expression that's an error.  So for instance:

```
set x_ := (select x from foo.x where id = y);
```

This will throw (with a peculiar error, SQLITE_DONE) if there is no such row.

Sometimes people try to fix this problem with a nullcheck:

```
set x_ := IFNULL((select x from foo.x where id = y), -1);
```

That doesn't help at all.  It's not a null value situation, there's no row at all.

```
set x_ := (select IFNULL(x,-1) from foo.x where id = y), -1);
```

Is likewise unhelpful.  To help with this situation we add two forms:


```
-- useful if foo.x is already known to be not null
set x_ := (select x from foo.x where id = y IF NOTHING -1);

-- useful if foo.x might be null
set x_ := (select x from foo.x where id = y IF NOTHING OR NULL -1);
```

Both of these deal with the case where there is no row.  The second lets you have a simple default for both
no row or null value.  That form is equivalent to:

```
set x_ := (select IFNULL(x,-1) from foo.x where id = y IF NOTHING -1);
```

i.e. both problem cases are handled.

Of course the -1 here could be any valid expression, even a second `(select...)`

"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[4825],{3905:function(e,n,t){t.d(n,{Zo:function(){return c},kt:function(){return p}});var r=t(7294);function o(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function a(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function s(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?a(Object(t),!0).forEach((function(n){o(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):a(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function l(e,n){if(null==e)return{};var t,r,o=function(e,n){if(null==e)return{};var t,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)t=a[r],n.indexOf(t)>=0||(o[t]=e[t]);return o}(e,n);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)t=a[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(o[t]=e[t])}return o}var i=r.createContext({}),u=function(e){var n=r.useContext(i),t=n;return e&&(t="function"==typeof e?e(n):s(s({},n),e)),t},c=function(e){var n=u(e.components);return r.createElement(i.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},h=r.forwardRef((function(e,n){var t=e.components,o=e.mdxType,a=e.originalType,i=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),h=u(t),p=o,m=h["".concat(i,".").concat(p)]||h[p]||d[p]||a;return t?r.createElement(m,s(s({ref:n},c),{},{components:t})):r.createElement(m,s({ref:n},c))}));function p(e,n){var t=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var a=t.length,s=new Array(a);s[0]=h;var l={};for(var i in n)hasOwnProperty.call(n,i)&&(l[i]=n[i]);l.originalType=e,l.mdxType="string"==typeof e?e:o,s[1]=l;for(var u=2;u<a;u++)s[u]=t[u];return r.createElement.apply(null,s)}return r.createElement.apply(null,t)}h.displayName="MDXCreateElement"},3007:function(e,n,t){t.r(n),t.d(n,{assets:function(){return c},contentTitle:function(){return i},default:function(){return p},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return d}});var r=t(7462),o=t(3366),a=(t(7294),t(3905)),s=["components"],l={id:"x6",title:"Appendix 6: CQL In 20 Minutes",sidebar_label:"Appendix 6: CQL In 20 Minutes"},i=void 0,u={unversionedId:"x6",id:"x6",title:"Appendix 6: CQL In 20 Minutes",description:"\x3c!---",source:"@site/../CQL_Guide/x6.md",sourceDirName:".",slug:"/x6",permalink:"/cql-guide/x6",draft:!1,tags:[],version:"current",lastUpdatedBy:"Ricardo Juan Palma Duran",lastUpdatedAt:1659070222,formattedLastUpdatedAt:"7/29/2022",frontMatter:{id:"x6",title:"Appendix 6: CQL In 20 Minutes",sidebar_label:"Appendix 6: CQL In 20 Minutes"},sidebar:"someSidebar",previous:{title:"Appendix 5: JSON Schema Grammar",permalink:"/cql-guide/x5"},next:{title:"Appendix 7: CQL Anti-patterns",permalink:"/cql-guide/x7"}},c={},d=[],h={toc:d};function p(e){var n=e.components,t=(0,o.Z)(e,s);return(0,a.kt)("wrapper",(0,r.Z)({},h,t,{components:n,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"What follows is a series of examples intended to illustrate the most important features of\nthe CQL language. This appendix was significantly influenced by a similar article on Python\nat ",(0,a.kt)("a",{parentName:"p",href:"https://learnxinyminutes.com/docs/python/"},"https://learnxinyminutes.com/docs/python/")),(0,a.kt)("p",null,"Also of interest:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("a",{parentName:"li",href:"http://sqlite.org"},"http://sqlite.org")),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("a",{parentName:"li",href:"https://learnxinyminutes.com/docs/sql"},"https://learnxinyminutes.com/docs/sql"))),(0,a.kt)("p",null,"And with no further delay, CQL in 20 minutes..."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},'-- Single line comments start with two dashes\n\n/* C style comments also work\n *\n * C pre-processor features like #include and #define are generally available\n * CQL is typically run through the C pre-processor before it is compile.\n */\n\n/**********************************************************\n * 1. Primitive Datatypes and Operators\n *********************************************************/\n\n-- You have numbers\n3     -- an integer\n3L    -- a long integer\n3.5   -- a real literal\n0x10  -- 16 in hex\n\n-- Math is what you would expect\n1 + 1     --\x3e 2\n8 - 1     --\x3e 7\n10 * 2    --\x3e 20\n35.0 / 5  --\x3e 7.0\n\n-- Modulo operation, same as C and SQLite\n7 % 3    --\x3e 1\n-7 % 3   --\x3e -1\n7 % -3   --\x3e 1\n-7 % 3   --\x3e -1\n\n-- Bitwise operators bind left to right like in SQLite\n1 | 4 & 3  --\x3e  1  (not 0)\n\n-- Enforce precedence with parentheses\n1 + 3 * 2    --\x3e 7\n(1 + 3) * 2  --\x3e 8\n\n-- Use true and false for bools, nullable bool is possible\ntrue    --\x3e how to true\nfalse   --\x3e how to false\nnull    --\x3e null means "unknown" in CQL like SQLite\n\n-- Negate with not\nnot true   --\x3e false\nnot false  --\x3e true\nnot null   --\x3e null (not unknown is unknown)\n\n-- Logical Operators\n1 and 0 --\x3e 0\n0 or 1  --\x3e 1\n0 and x --\x3e 0 and x not evaluated\n1 or x  --\x3e 1 and x not evaluated\n\n-- Remember null is "unknown"\nnull or false  --\x3e null\nnull or true   --\x3e true\nnull and false --\x3e false\nnull and true  --\x3e null\n\n-- Non-zero values are truthy\n0        --\x3e false\n4        --\x3e true\n-6       --\x3e true\n0 and 2  --\x3e 0 (false)\n-5 or 0  --\x3e 1 (true)\n\n-- Equality is == or =\n1 == 1       --\x3e true\n1 = 1        --\x3e true  (= and == are the same thing)\n2 == 1       --\x3e false\n\n-- Note that null is not equal to anything (like SQL)\nnull == 1    --\x3e null (hence not true)\nnull == null --\x3e null (hence not true)\n"x" == "x"   --\x3e true\n\n-- IS lets you compare against null\n1 IS 1       --\x3e true\n2 IS 1       --\x3e false\nnull IS 1    --\x3e false\nnull IS null --\x3e true  (Unknown is Unknown?  Yes it is!)\n"x" IS "x"   --\x3e true\n\n-- x IS NOT y is the same as NOT (x IS y)\n1 IS NOT 1       --\x3e false\n2 IS NOT 1       --\x3e true\nnull IS NOT 1    --\x3e true\nnull IS NOT null --\x3e false\n"x" IS NOT "x"   --\x3e false\n\n-- Inequality is != or <>\n1 != 1       --\x3e false\n2 <> 1       --\x3e true\nnull != 1    --\x3e null\nnull <> null --\x3e null\n\n-- More comparisons\n1 < 10    --\x3e true\n1 > 10    --\x3e false\n2 <= 2    --\x3e true\n2 >= 2    --\x3e true\n10 < null --\x3e null\n\n-- To test if a value is in a range\n1 < 2 and 2 < 3  --\x3e true\n2 < 3 and 3 < 2  --\x3e false\n\n-- BETWEEN makes this look nicer\n2 between 1 and 3 --\x3e true\n3 between 2 and 2 --\x3e false\n\n-- Strings are created with "x" or \'x\'\n"This is a string.\\n"           -- can have C style escapes (no embedded nulls)\n"Th\\x69s is a string.\\n"        -- even hex literals\n\'This isn\'\'t a C style string\'  -- use \'\' to escape single quote ONLY\n\n/**********************************************************\n * 2. Simple Variables\n *********************************************************/\n\n-- CQL can call simple libc methods with a no-check declaration\n-- we\'ll need this for later examples so we can do something\n-- with our expressions (i.e. print them)\ndeclare procedure printf no check;\n\ncall printf("I\'m CQL. Nice to meet you!\\n");\n\n-- Variables are declared with DECLARE.\n-- Keywords and identifiers are not case sensitive.\ndeclare x integer not null;\n\n-- You can call it X, it is the same thing.\nset X := 0;\n\n-- All variables begin with a null value if allowed, else a zero value.\ndeclare y integer not null;\nif y == 0 then\n  call printf("Yes, this will run.\\n");\nend if;\n\n-- A nullable variable (i.e. not marked with not null) is initialized to null\ndeclare z real;\nif z is null then\n  call printf("Yes, this will run.\\n");\nend if;\n\n-- The various types\ndeclare a_blob blob;\ndeclare a_string text;\ndeclare a_real real;\ndeclare an_int integer;\ndeclare a_long long;\ndeclare an_object object;\n\n-- There are some typical SQL synonyms\ndeclare an_int int;\ndeclare a_long long integer;\ndeclare a_long long int;\ndeclare a_long long_int;\n\n-- The basic types can be tagged to make them less miscible\ndeclare m real<meters>;\ndeclare kg real<kilos>;\n\nset m := kg;  -- error!\n\n-- Object variables can also be tagged so that they are not mixed-up easily\ndeclare dict object<dict> not null;\ndeclare list object<list> not null;\nset dict := create_dict();  -- an external function that creates a dict\nset dict := create_list();  -- error\nset list := create_list();  -- ok\nset list := dict;           -- error\n\n-- Implied type initialization\nLET i := 1;      -- integer not null\nLET l := 1L;     -- long not null\nLET t := "x";    -- text not null\nLET b := x IS y; -- bool not null\nLET b := x = y;  -- bool (maybe not null depending on x/y)\n\n-- The psuedo function "nullable" converts the type of its arg to the nullable\n-- version of the same thing.\n\nLET n_i := nullable(1);   -- nullable integer variable initialized to 1\nLET l_i := nullable(1L);  -- nullable long variable initialized to 1\n\n/**********************************************************\n * 3. Control Flow\n *********************************************************/\n\n-- Just make a variable\ndeclare some_var integer not null;\nset some_var := 5\n\n-- Here is an IF statement\nif some_var > 10 then\n    call printf("some_var is totally bigger than 10.\\n")\nelse if some_var < 10 then  -- else if is optional\n    call printf("some_var is smaller than 10.\\n")\nelse -- else is optional\n    call printf("some_var is indeed 10.\\n")\nend if;\n\n\n-- WHILE loops iterate as usual\ndeclare i integer not null;\nset i := 0;\nwhile i < 5\nbegin\n   call printf("%d\\n", i);\n   set i := i + 1;\nend;\n\n-- Use LEAVE to end a loop early\ndeclare i integer not null;\nset i := 0;\nwhile i < 500\nbegin\n   if i >= 5 then\n     -- we are not going to get anywhere near 500\n     leave;\n   end if;\n\n   call printf("%d\\n", i);\n   set i := i + 1;\nend;\n\n-- Use CONTINUE to go back to the loop test\ndeclare i integer not null;\nset i := 0;\nwhile i < 500\nbegin\n   set i := i + 1;\n   if i % 2 then\n     -- Note: we to do this after "i" is incremented!\n     -- to avoid an infinite loop\n     continue;\n   end if;\n\n   -- odd numbers will not be printed because of continue above\n   call printf("%d\\n", i);\nend;\n\n /**********************************************************\n * 4. Complex Expression Forms\n *********************************************************/\n\n -- Case is an expression, so it is more like the C "?:" operator\n -- than a switch statement.  It is like "?:" on steroids.\n\n case i              -- a switch expression is optional\n   when 1 then "one" -- one or more cases\n   when 2 then "two"\n   else "other"      -- else is optional\n end;\n\n-- Case with no common expression is a series of independent tests\ncase\n   when i == 1 then "i = one"   -- booleans could be completely unrelated\n   when j == 2 then "j = two"   -- first match wins\n   else "other"\nend;\n\n-- If nothing matches the cases, the result is null.\n-- The following expression yields null because 7 is not 1.\ncase 7 when 1 then "one" end\n\n\n-- Case is just an expression, so it can nest\ncase X\n  when 1\n    case y when 1 "x:1 y:1"\n           else "x:1 y:other"\n    end\n  else\n    case when z == 1 "x:other z:1"\n         else "x:other z:other"\n    end\nend;\n\n-- IN is used to test for membership\n5 IN (1, 2, 3, 4, 5)  --\x3e true\n7 IN (1, 2)           --\x3e false\nnull in (1, 2, 3)     --\x3e null\nnull in (1, null, 3)  --\x3e null  (null == null is not true)\n7 NOT IN (1, 2)       --\x3e true\nnull not in (null, 3) --\x3e null\n\n/**********************************************************\n * 4. Working with and "getting rid of" null\n *********************************************************/\n\n-- Null can be annoying, you might need a not null value.\n-- In most operations null is radioactive:\nnull + x     --\x3e null\nnull * x     --\x3e null\nnull == null --\x3e null\n\n-- IS and IS NOT always return 0 or 1\nnull is 1     -> 0\n1 is not null -> 1\n\n-- COALESCE returns the first non null arg, or the last arg if all were null.\n-- If the last arg is not null, you get a non null result for sure.\n-- The following is never null, but it\'s false if either x or y is null\nCOALESCE(x==y, false) -> thought excercise: how is this different than x IS y?\n\n-- IFNULL is coalesce with 2 args only (COALESCE is more general)\nIFNULL(x, -1) --\x3e use -1 if x is null\n\n-- The reverse, NULLIF, converts a sentinel value to unknown, more exotic\nNULLIF(x, -1) --\x3e if x is -1 then use null\n\n-- the else part of a case can get rid of nulls\nCASE when x == y then 1 else 0 end;  --\x3e true iff x = y and neither is null\n\n-- CASE can be used to give you a default value after various tests\n-- The following expression is never null; "other" is returned if x is null.\nCASE when x > 0 then "pos" when x < 0 then "neg" else "other" end;\n\n-- You can "throw" out of the current procedure (see exceptions below)\ndeclare x integer not null;\nset x := ifnull_throw(nullable_int_expr); -- returns non null, throws if null\n\n-- If you have already tested the expression then control flow analysis\n-- improves its type to "not null".  Many common check patterns are recognized.\nif nullable_int_expr is not null then\n  -- nullable_int_expression is known to be not null in this context\n  set x := nullable_int_expr;\nend if;\n\n/**********************************************************\n * 5. Tables, Views, Indices, Triggers\n *********************************************************/\n\n-- Most forms of data definition language DDL are supported.\n-- "Loose" DDL (outside of any procedure) simply declares\n-- schema, it does not actually create it; the schema is assumed to\n-- exist as you specified.\n\ncreate table T1(\n  id integer primary key,\n  t text,\n  r real\n);\n\ncreate table T2(\n  id integer primary key references T1(id),\n  l long,\n  b blob\n);\n\n-- CQL can take a series of schema declarations (DDL) and\n-- automatically create a procedure that will materialize\n-- that schema and even upgrade previous versions of the schema.\n-- This system is discussed in Chapter 10 of The Guide.\n-- To actually create tables and other schema you need\n-- procedures that look like the below:\n\ncreate proc make_tables()\nbegin\n  create table T1 if not exists (\n    id integer primary key,\n    t text,\n    r real\n  );\nend;\n\n-- Views are supported\ncreate view V1 as (select * from T1);\n\n-- Triggers are supported\ncreate trigger if not exists trigger1\n  before delete on T1\nbegin\n  delete from T2 where id = old.id;\nend;\n\n-- Indices are supported\ncreate index I1 on T1(t);\ncreate index I2 on T1(r);\n\n-- The various drop forms are supported\ndrop index I1;\ndrop index I2;\ndrop view V1;\ndrop table T2;\ndrop table T1;\n\n-- A complete discussion of DDL is out of scope, refer to sqlite.org\n\n/**********************************************************\n * 6. Selecting Data\n *********************************************************/\n\n-- We will use this scratch variable in the following examples\ndeclare rr real;\n\n-- First observe CQL is a two-headed language\nset rr := 1+1;           -- this is evaluated in generated C code\nset rr := (select 1+1);  -- this expresion goes to SQLite; SQLite does the addition\n\n-- CQL tries to do most things the same as SQLite in the C context\n-- but some things are exceedingly hard to emulate correctly.\n-- Even simple looking things such as:\nset rr := (select cast("1.23" as real));   --\x3e  rr := 1.23\nset rr := cast("1.23" as real);            --\x3e  error (not safe to emulate SQLite)\n\n-- In general, numeric/text conversions have to happen in SQLite context\n-- because the specific library that does the conversion could be and usually\n-- is different than the one CQL would use.  It would not do to give different answers\n-- in one context or another so those conversions are simply not supported.\n\n-- Loose concatenation is not supported because of the implied conversions.\n-- Loose means "not in the context of a SQL statement".\nset r := 1.23;\nset r := (select cast("100"||r as real));  --\x3e 1001.23 (a number)\nset r := cast("100"||r as real);  --\x3e error, concat not supported in loose expr\n\n-- A simple insertion\ninsert into T1 values (1, "foo", 3.14);\n\n-- Finally, reading from the database\nset r := (select r from T1 where id = 1);  --\x3e r = 3.14\n\n-- The (select ...) form requires the result to have at least one row.\n-- You can use IF NOTHING forms to handle other cases such as:\nset r := (select r from T1\n          where id = 2\n          if nothing -1);  --\x3e r = -1\n\n-- If the SELECT statement might return a null result you can handle that as well\nset r := (select r from T1\n          where id = 2\n          if nothing or null -1);  --\x3e r = -1\n\n-- With no IF NOTHING clause, lack of a row will cause the SELECT expression to throw\n-- an exception.  IF NOTHING THROW merely makes this explicit.\nset r := (select r from T1 where id = 2 if nothing throw);  --\x3e will throw\n\n/**********************************************************\n * 6. Procedures, Results, Exceptions\n *********************************************************/\n\n-- Procedures are a list of statements that can be executed, with arguments.\ncreate proc hello()\nbegin\n  call printf("Hello, world\\n");\nend;\n\n-- IN, OUT, and INOUT parameters are possible\ncreate proc swizzle(x integer, inout y integer, out z real not null)\nbegin\n  set y := x + y;  -- any computation you like\n\n  -- bizarre way to compute an id but this is just an illustration\n  set z := (select r from T1 where id = x if nothing or null -1);\nend;\n\n-- Procedures like "hello" (above) have a void signature -- they return nothing\n-- as nothing can go wrong. Procedures that use the database like "swizzle" (above)\n-- can return an error code if there is a problem.\n-- "will_fail" (below)  will always return SQLITE_CONSTRAINT, the second insert\n-- is said to "throw".  In CQL exceptions are just result codes.\ncreate proc will_fail()\nbegin\n   insert into T1 values (1, "x", 1);\n   insert into T1 values (1, "x", 1);  --\x3e duplicate key\nend;\n\n-- DML that fails generates an exception and\n-- exceptions can be caught. Here is a example:\ncreate proc upsert_t1(\n  id_ integer primary key,\n  t_ text,\n  r_ real\n)\nbegin\n  begin try\n    -- try to insert\n    insert into T1(id, t, r) values (id_, t_, r_);\n  end try;\n  begin catch\n    -- if the insert fails, try to update\n    update T1 set t = t_, r = r_ where id = id_;\n  end catch;\nend;\n\n-- Shapes can be very useful in avoiding boilerplate code\n-- the following is equivalent to the above.\n-- More on shapes later.\ncreate proc upsert_t1(LIKE t1) -- my args are the same as the columns of T1\nbegin\n  begin try\n    insert into T1 from arguments\n  end try;\n  begin catch\n    update T1 set t = t_, r = r_ where id = id_;\n  end catch;\nend;\n\n-- You can (re)throw an error explicitly.\n-- If there is no current error you get SQLITE_ERROR\ncreate proc upsert_wrapper(LIKE t1) -- my args are the same as the columns of T1\nbegin\n  if r_ > 10 then throw end if; -- throw if r_ is too big\n  call upsert_t1(from arguments);\nend;\n\n-- Procedures can also produce a result set.\n-- The compiler generates the code to create this result set\n-- and helper functions to read rows out of it.\ncreate proc get_low_r(r_ real)\nbegin\n   -- optionally insert some rows or do other things\n   select * from T1 where T1.r <= r_;\nend;\n\n-- A procedure can choose between various results, the choices must be compatible.\n-- The last "select" to run controls the ultimate result.\ncreate proc get_hi_or_low(r_ real, hi_not_low bool not null)\nbegin\n  -- trying to do this with one query would result in a poor plan, so\n  -- instead we use two economical queries.\n  if hi_not_low then\n    select * from T1 where T1.r >= r_;\n  else\n    select * from T1 where T1.r <= r_;\n  end if;\nend;\n\n-- Using IF to create to nice selects above is a powerful thing.\n-- SQLite has no IF, if we tried to create a shared query we get\n-- something that does not use indices at all.  As in the below.\n-- The two-headed CQL beast has its advantages!\nselect * from T1 where case hi_not_low then T1.r >= r_ else T1.r <= r_ end;\n\n-- You can get the current return code and use it in your CATCH logic.\n-- This upsert is a bit better than the first:\ncreate proc upsert_t1(LIKE t1) -- my args are the same as the columns of T1\nbegin\n  begin try\n    insert into T1 from arguments\n  end try;\n  begin catch;\n    if @rc == 19 /* SQLITE_CONSTRAINT */ then\n      update T1 set t = t_, r = r_ where id = id_;\n    else\n      throw;  -- rethrow, something bad happened.\n    end if;\n  end catch;\nend;\n\n-- By convention, you can call a procedure that has an OUT argument\n-- as its last argument using function notation.  The out argument\n-- is used as the return value.   If the called procedure uses the\n-- database then it could throw which causes the caller to throw\n-- as usual.\ncreate proc fib(n integer not null, out result integer not null)\nbegin\n   set result := case n <= 2 then 1 else fib(n-1) + fib(n-2) end;\nend;\n\n/**********************************************************\n * 7. Statement Cursors\n *********************************************************/\n\n-- Statement cursors let you iterate over a select result.\n-- Here we introduce cursors, LOOP and FETCH.\ncreate proc count_t1(r_ real, out rows_ integer not null)\nbegin\n  declare rows integer not null;  -- starts at zero guaranteed\n  declare C cursor for select * from T1 where r < r_;\n  loop fetch C -- iterate until fetch returns no row\n  begin\n    -- goofy code to illustrate you can process the cursor\n    -- in whatever way you deem appropriate\n    if C.r < 5 then\n      rows := rows + 1; -- count rows with C.r < 5\n    end if;\n  end;\n  set rows_ := rows;\nend;\n\n-- Cursors can be tested for presence of a row\n-- and they can be closed before the enumeration is finished.\n-- As before the below is somewhat goofy example code.\ncreate proc peek_t1(r_ real, out rows_ integer not null)\nbegin\n   /* rows_ is set to zero for sure! */\n   declare C cursor for select * from T1 where r < r_ limit 2;\n   open C;  -- this is no-op, present because other systems have it\n   fetch C;  -- fetch might find a row or not\n   if C then  -- cursor name as bool indicates presence of a row\n     set rows_ = rows_ + (C.r < 5);\n     fetch C;\n     set rows_ = rows_ + (C and C.r < 5);\n   end if;\n   close C;  -- cursors auto-closed at end of method but early close possible\nend;\n\n-- The FETCH...INTO form can be used to fetch directly into variables\nfetch C into id_, t_, r_;  --\x3e loads named locals instead of C.id, C.t, C.r\n\n-- A procedure can be the source of a cursor\ndeclare C cursor for call get_low_r(3.2);  -- valid cursor source\n\n-- OUT can be used to create a result set that is just one row\ncreate proc one_t1(r_ real)\nbegin\n   declare C cursor for select * from T1 where r < r_ limit 1;\n   fetch C;\n   out C;  -- emits a row if we have one, no row is ok too, empty result set.\nend;\n\n/**********************************************************\n * 8. Value Cursors, Out, and Out Union\n *********************************************************/\n\n-- To consume a procedure that uses "out" you can declare a value cursor.\n-- By itself such as cursor does not imply use of the database, but often\n-- the source of the cursor uses the database.  In this example\n-- consume_one_t1 uses the database because of the call to one_t1.\ncreate proc consume_one_t1()\nbegin\n  -- a cursor whose shape matches the one_t1 "out" statement\n  declare C cursor like one_t1;\n\n  -- load it from the call\n  fetch C from call one_t1(7);\n  if C.r > 10 then\n    -- use values as you see fit\n    call printf("woohoo");\n  end if;\nend;\n\n-- You can do the above in one step with the compound form:\ndeclare C cursor fetch from call one_t1(7); -- declare and fetch\n\n-- Value cursors can come from anywhere and can be a procedure result\ncreate proc use_t1_a_lot()\nbegin\n  -- T1 is the same shape as one_t1, this will work, too\n  declare C cursor like T1;\n  fetch C from call one_t1(7);  -- load it from the call\n\n  -- some arbitrary logic might be here\n\n  -- load C again with different args\n  fetch C from call one_t1(12);   -- load it again\n\n  -- some arbitrary logic might be here\n\n  -- now load C yet again with explicit args\n  fetch C using\n     1 id,\n     "foo" t,\n     8.2 r;\n\n  -- now return it\n  out C;\nend;\n\n-- Make a complex result set one row at a time\ncreate proc out_union_example()\nbegin\n  -- T1 is the same shape as one_t1, this will work, too\n  declare C cursor like T1;\n\n  -- load it from the call\n  fetch C from call one_t1(7);\n\n  -- note out UNION rather than just out, indicating potentially many rows\n  out union C;\n\n  -- load it again with different args\n  fetch C from call one_t1(12);\n  out union C;\n\n  -- do something, then maybe load it again with explicit args\n  fetch C using\n     1 id,\n     "foo" t,\n     8.2 r;\n  out union C;\n\n  -- we have generated a 3 row result set\nend;\n\n-- Consume the above\ncreate proc consume_result()\nbegin\n  declare C cursor for call out_union_example();\n  loop fetch C\n  begin\n    -- use builtin cql_cursor_format to make the cursor into a string\n    call printf("%s\\n", cql_cursor_format(C)); --\x3e prints every column and value\n  end;\nend;\n\n/**********************************************************\n * 9. Named Types and Enumerations\n *********************************************************/\n\n-- Create a simple named types\ndeclare my_type type integer not null;   -- make an alias for integer not null\ndeclare i my_type;  -- use it, "i" is an integer\n\n-- Mixing in type kinds is helpful\ndeclare distance type real<meters>;  -- e.g., distances to be measured in meters\ndeclare time type real<seconds>;     -- e.g., time to be measured in seconds\ndeclare job_id type long<job_id>;\ndeclare person_id type long<person_id>;\n\n-- With the above done\n--  * vars/cols of type "distance" are incompatible with those of type "time"\n--  * vars/cols of types job_id are incompatible with person_id\n-- This is true even though the underlying type is the same for both!\n\n-- ENUM declarations can have any numeric type as their base type\ndeclare enum implement integer (\n   pencil,       -- values start at 1 unless you use = to choose something\n   pen,          -- the next enum gets previous + 1 as its value (2)\n   brush = 7     -- with = expression you get the indicated value\n);\n\n-- The above also implicitly does this\ndeclare implement type integer<implement> not null;\n\n-- Using the enum -- simply use dot notation\ndeclare impl implement;\nset impl := implement.pen;  -- value "2"\n\n-- You can emit an emum into the current .h file we are going to generate.\n-- Do not put this directive in an include file, you want it to go to one place.\n-- Instead, pick one compiland that will "own" the emission of the enum.\n-- C code can then #include that one .h file.\n@emit_enums implement;\n\n/**********************************************************\n * 10. Shapes and Their Uses\n *********************************************************/\n\n-- Shapes first appeared to help define value cursors like so:\n\n-- A table or view name defines a shape\ndeclare C cursor like T1;\n\n-- The result of a proc defines a shape\ndeclare D cursor like one_t1;\n\n-- A dummy select statement defines a shape (the select does not run)\n-- this one is the same as (x integer not null, y text not null)\ndeclare E cursor like select 1 x, "2" y;\n\n-- Another cursor defines a shape\ndeclare F cursor like C;\n\n-- The arguments of a procedure define a shape. If you have\n-- create proc count_t1(r_ real, out rows_ integer not null) ...\n-- the shape will be:\n--  (r_ real, rows_ integer not null)\ndeclare G cursor like count_t1 arguments;\n\n-- A loaded cursor can be used to make a call\ncall count_t1(from G);  -- the args become G.r_, G.rows_\n\n-- A shape can be used to define a procedures args, or some of the args\n-- In the following "p" will have arguments:s id_, t_, and r_ with types\n-- matching table T1.\n-- Note: To avoid ambiguity, an _ was added to each name!\ncreate proc p(like T1)\nbegin\n  -- do whatever you like\nend;\n\n-- The arguments of the current procedure are a synthetic shape\n-- called "arguments" and can used where other shapes can appear.\n-- For instance, you can have "q" shim to "p" using this form:\ncreate proc q(like T1, print bool not null)\nbegin\n  -- maybe pre-process, silly example\n  set id_ := id_ + 1;\n\n  -- shim to p\n  call p(from arguments); -- pass my args through, whatever they are\n\n  -- maybe post-process, silly example\n  set r_ := r_ - 1;\n\n  if print then\n    -- convert args to cursor\n    declare C like q arguments;\n    fetch C from arguments;\n    call printf("%s\\n", cql_cursor_format(C)); --\x3e prints every column and value\n  end if;\n\n  -- insert a row based on the args\n  insert into T1 from arguments;\nend;\n\n-- You an use a given shape more than once if you name each use.\n-- This would be more exciting if T1 was like a "person" or something.\ncreate proc r(a like T1, b like T1)\nbegin\n  call p(from a);\n  call p(from b);\n  -- you can refer to a.id, b.id etc.\n  declare C like a;\n  fetch C from a;\n  call printf("%s\\n", cql_cursor_format(C));\n  fetch C from b;\n  call printf("%s\\n", cql_cursor_format(C));\nend;\n\n-- Shapes can be subsetted, for instance in the following example\n-- only the arguments that match C are used in the FETCH.\nfetch C from arguments(like C);\n\n-- Fetch the columns of D into C using the cursor D for the data source.\n-- Other columns get default values.\nfetch C(like D) from D;\n\n-- Use the D shape to load C, dummy values for the others.\n-- In this example, dummy_seed means use the provided value, 11, for\n-- any numerics that are not specified (not in D) and and use\n-- "col_name_11" for any strings/blobs.  This pattern is useful in test code\n-- to create dummy data, hence the name.\nfetch C(like D) from D @dummy_seed(11);\n\n-- Use the Z shape to control which fields are copied.\n-- Use the dummy value even if the field is nullable and null would have be ok.\nfetch C(like Z) from D(like Z) @dummy_seed(11) @dummy_nullables;\n\n-- The above patterns also work for insert statements\n-- The shape constraints are generally useful.  The dummy data\n-- sources are useful for inserting test data.\ninsert into T1(like Z) from D(like Z) @dummy_seed(11) @dummy_nullables;\n\n-- We\'ll need this dummy procedure some_shape so we can use its return\n-- value in the examples that follow.  We will never actual create this\n-- proc, we only declare it to define the shape, so this is kind of like\n-- a typedef.\ndeclare proc some_shape() (x integer not null, y integer not null, z integer not null);\n\n-- You can make a helper procedure to create test args that are mostly constant\n-- or computable.\ncreate get_foo_args(X like some_shape, seed_ integer not null)\nbegin\n  declare C cursor like foo arguments;\n  -- any way of loading C could work this is one\n  fetch C(like X) from X @dummy_seed(seed_);\n  out C;\nend;\n\n-- Now we can use the "get_foo_args" to get full set of arguments for "foo" and then\n-- call "foo" with those arguments.  In this example we\'re providing\n-- some of the arguments explicitly, "some_shape" is the part of the args that\n-- needs to manually vary in each test iteration, the rest of the arguments will\n-- be dummy values.  There could be zillions of args in either category.\n-- In the below "some_shape" is going to get the manual values 1, 2, 3 while 100\n-- will be the seed for the dummy args.\ndeclare foo_args cursor fetch from call get_foo_args(1,2,3, 100);\ncall foo(from foo_args);\n\n/**********************************************************\n * 11. INSERT USING and FETCH USING\n *********************************************************/\n\n -- This kind of thing is a pain\n insert into foo(a, b, c, d, e, f, g)\n    values(1, 2, 3, null, null, 5, null);\n\n-- Instead, write this form:\ninsert into foo USING\n    1 a, 2 b, 3 c, null d, null e, 5 f, null g;\n\n-- The FETCH statement can also be "fetch using"\ndeclare C cursor like foo;\nfetch C USING\n    1 a, 2 b, 3 c, null d, null e, 5 f, null g;\n')),(0,a.kt)("p",null,"If you've read this far you know more than most now.  :)"))}p.isMDXComponent=!0}}]);
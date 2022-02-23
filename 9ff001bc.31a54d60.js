(window.webpackJsonp=window.webpackJsonp||[]).push([[67],{122:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return i})),n.d(t,"metadata",(function(){return s})),n.d(t,"rightToc",(function(){return c})),n.d(t,"default",(function(){return u}));var a=n(2),o=n(6),l=(n(0),n(160)),r=["components"],i={id:"ch02",title:"Chapter 2: Using Data",sidebar_label:"Chapter 2: Using Data"},s={unversionedId:"ch02",id:"ch02",isDocsHomePage:!1,title:"Chapter 2: Using Data",description:"\x3c!---",source:"@site/../CQL_Guide/ch02.md",slug:"/ch02",permalink:"/cql-guide/ch02",version:"current",lastUpdatedBy:"Dmitry Vinnik",lastUpdatedAt:1645576132,sidebar_label:"Chapter 2: Using Data",sidebar:"someSidebar",previous:{title:"Chapter 1: Introduction",permalink:"/cql-guide/ch01"},next:{title:"Chapter 3: Expressions, Literals, Nullability, Sensitivity",permalink:"/cql-guide/ch03"}},c=[{value:"A Sample Program",id:"a-sample-program",children:[]},{value:"Providing a Suitable Database",id:"providing-a-suitable-database",children:[]},{value:"Declaring Schema",id:"declaring-schema",children:[]},{value:"Explaining The New Hello World",id:"explaining-the-new-hello-world",children:[]},{value:"Introducing Cursors",id:"introducing-cursors",children:[]},{value:"Going Crazy",id:"going-crazy",children:[]}],b={rightToc:c};function u(e){var t=e.components,n=Object(o.a)(e,r);return Object(l.b)("wrapper",Object(a.a)({},b,n,{components:t,mdxType:"MDXLayout"}),Object(l.b)("p",null,"The point of using CQL is to facilitate access to a SQLite database so we'll switch gears to a slightly more complicated setup.  We'll\nstill keep things fairly simple but let's start to use some database features.  Note: it is not the intent of this tutorial to also be\na primer for the SQLite programming language which is so ably documented on ",Object(l.b)("a",{parentName:"p",href:"https://sqlite.org/"},"https://sqlite.org/"),".  Please refer to that site for details\non the meaning of the SQL statements used here if you are new to SQL."),Object(l.b)("h3",{id:"a-sample-program"},"A Sample Program"),Object(l.b)("p",null,"Suppose we have the following program:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"-- needed to allow vararg calls to C functions\ndeclare procedure printf no check;\n\ncreate table my_data(t text not null);\n\ncreate proc hello()\nbegin\n  insert into my_data(t) values(\"Hello, world\\n\");\n  declare t text not null;\n  set t := (select * from my_data);\n  call printf('%s', t);\nend;\n")),Object(l.b)("p",null,'That looks like an interesting little baby program and it appears as though it would once again print that most famous of salutations, "Hello, world".'),Object(l.b)("p",null,"Well, it doesn't.  At least, not yet.  Let's walk through the various things that are going to go wrong as this will teach us everything we need to know about activating CQL from some environment of your choice."),Object(l.b)("h3",{id:"providing-a-suitable-database"},"Providing a Suitable Database"),Object(l.b)("p",null,"CQL is just a compiler, it doesn't know how the code it creates will be provisioned any more than say clang does.\nIt creates functions with predictable signatures so that they can be called from C just as easily as the SQLite API\nitself, and using the same currency.  Our new version of ",Object(l.b)("inlineCode",{parentName:"p"},"hello")," now requires a database handle because it performs\ndatabase operations.  Also there are now opportunities for the database operations to fail, and so ",Object(l.b)("inlineCode",{parentName:"p"},"hello")," now provides a\nreturn code."),Object(l.b)("p",null,"A new minimal ",Object(l.b)("inlineCode",{parentName:"p"},"main")," program might look something like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-C"},'#include <stdlib.h>\n#include <sqlite3.h>\n\n#include "hello.h"\n\nint main(int argc, char **argv)\n{\n  sqlite3 *db;\n  int rc = sqlite3_open(":memory:", &db);\n  if (rc != SQLITE_OK) {\n    exit(1); /* not exactly world class error handling but that isn\'t the point */\n  }\n  rc = hello(db);\n  if (rc != SQLITE_OK) {\n    exit(2);\n  }\n\n  sqlite3_close(db);\n}\n')),Object(l.b)("p",null,"If we re-run CQL and look in the ",Object(l.b)("inlineCode",{parentName:"p"},"hello.h")," output file we'll see that the declaration of the ",Object(l.b)("inlineCode",{parentName:"p"},"hello")," function is now:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-C"},"extern CQL_WARN_UNUSED cql_code hello(sqlite3 *_Nonnull _db_);\n")),Object(l.b)("p",null,"This indicates that the database is used and a SQLite return code is provided.  We're nearly there.  If you attempt\nto build the program as before there will be several link-time errors due to missing functions.  Typically these\nare resolved by providing the SQLite library to the command line and also adding the CQL runtime.\nThe new command line looks something like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-bash"},"$ cc -o hello main.c hello.c cqlrt.c -lsqlite3\n$ ./hello\nHello, world\n")),Object(l.b)("p",null,"The cql runtime can be anywhere you want it to be, and of course the usual C separate compilation methods\ncan be applied. More on that later."),Object(l.b)("p",null,"But actually, that program doesn't quite work yet.  If you run it, you'll get an error result code, not the message\n\"Hello, world\"."),Object(l.b)("p",null,"Let's talk about the final missing bit."),Object(l.b)("h3",{id:"declaring-schema"},"Declaring Schema"),Object(l.b)("p",null,'In CQL a loose piece of Data Definition Language (henceforth DDL) does not actually create or drop anything.\nIn most CQL programs the normal situation is that "something" has already created the database and put some\ndata in it.  You need to tell the CQL compiler about the schema so that it knows what the tables are and what to\nexpect to find in those tables.  This is because typically you\'re reconnecting to some sort of existing database.\nSo, in CQL, loose DDL simply ',Object(l.b)("em",{parentName:"p"},"declares")," schema, it does not create it.  To create schema you have to put the DDL\ninto a procedure you can run.  If you do that, then the DDL still serves a declaration, but also the schema will be\ncreated when the procedure is executed."),Object(l.b)("p",null,"We need to change our program a tiny bit."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"-- needed to allow vararg calls to C functions\ndeclare procedure printf no check;\n\ncreate proc hello()\nbegin\n  create table my_data(t text not null);\n  insert into my_data(t) values(\"Hello, world\\n\");\n  declare t text not null;\n  set t := (select * from my_data);\n  call printf('%s', t);\n  drop table my_data;\nend;\n")),Object(l.b)("p",null,"If we rebuild the program, it will now behave as expected."),Object(l.b)("h3",{id:"explaining-the-new-hello-world"},"Explaining The New Hello World"),Object(l.b)("p",null,"Let's go over every important line of the new program, starting from main."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-C"},'  int rc = sqlite3_open(":memory:", &db);\n')),Object(l.b)("p",null,"This statement gives us an empty, private, in-memory only database to work with.  This is the simplest case\nand it's still very useful.  The ",Object(l.b)("inlineCode",{parentName:"p"},"sqlite_open")," and ",Object(l.b)("inlineCode",{parentName:"p"},"sqlite_open_v2")," functions can be used to create a variety of\ndatabases per the SQLite documentation."),Object(l.b)("p",null,"We'll need such a database to use our procedure, and we use it in the call here:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-C"},"  rc = hello(db);\n")),Object(l.b)("p",null,"This provides a valid db handle to our procedure.  Note that the procedure doesn't know what database it is\nsupposed to operate on, it expects to be handed a suitable database on a silver platter.  In fact any given proc\ncould be used with various databases at various times.  Just like SQLite, CQL does not enforce any particular\ndatabase setup; it does what you tell it to."),Object(l.b)("p",null,"When ",Object(l.b)("inlineCode",{parentName:"p"},"hello")," runs we begin with"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  create table my_data(t text not null);\n")),Object(l.b)("p",null,"This will create the ",Object(l.b)("inlineCode",{parentName:"p"},"my_data")," table with a single column ",Object(l.b)("inlineCode",{parentName:"p"},"t"),", of type ",Object(l.b)("inlineCode",{parentName:"p"},"text not null"),".  That will work because\nwe know we're going to call this with a fresh/empty database.  More typically you might do ",Object(l.b)("inlineCode",{parentName:"p"},"create table if not exists ...")," or otherwise have a general attach/create phase or something to that effect.  We'll dispense with that here."),Object(l.b)("p",null,"Next we'll run the insert statement:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'  insert into my_data(t) values("Hello, world\\n");\n')),Object(l.b)("p",null,"This will add a single row to the table.  Note that we have again used double quotes, meaning that this is a C string literal.  This is highly convenient given the escape sequences.  Normally SQLite text has the newlines directly embedded in it; that practice isn't very compiler friendly, hence the alternative."),Object(l.b)("p",null,"At this point, we can read back our data:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  let t := (select * from my_data);\n")),Object(l.b)("p",null,"This form of database reading has very limited usability but it does work for this case and it is illustrative.\nThe presence of ",Object(l.b)("inlineCode",{parentName:"p"},"(select ...)")," indicates to the CQL compiler that the parenthesized expression should be given to\nSQLite for evaluation according to the SQLite rules.  The expression is statically checked at compile time to\nensure that it has exactly one result column. In this case the ",Object(l.b)("inlineCode",{parentName:"p"},"*")," is just column ",Object(l.b)("inlineCode",{parentName:"p"},"t"),", and actually it would have\nbeen clearer to use ",Object(l.b)("inlineCode",{parentName:"p"},"t")," directly here but then there wouldn't be a reason to talk about ",Object(l.b)("inlineCode",{parentName:"p"},"*")," and multiple columns.\nAt run time, the ",Object(l.b)("inlineCode",{parentName:"p"},"select")," query must return exactly one row or an error code will be returned.  It's not uncommon\nto see ",Object(l.b)("inlineCode",{parentName:"p"},"(select ... limit 1)")," to force the issue.  But that still leaves the possibility of zero rows, which would\nbe an error.  We'll talk about more flexible ways to read from the database later."),Object(l.b)("p",null,"Note: you can declare a variable and assign it in one step with the ",Object(l.b)("inlineCode",{parentName:"p"},"LET")," keyword, e.g."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre"},"  let t := (select * from my_data);\n")),Object(l.b)("p",null,"The code would normally be written in this way but for discussion purposes, these examples continue to avoid ",Object(l.b)("inlineCode",{parentName:"p"},"LET"),"."),Object(l.b)("p",null,"At this point it seems wise to bring up the unusual expression evaluation properties of CQL.\nCQL is by necessity a two-headed beast.  On the one side there is a rich expression evaluation language for\nworking with local variables. ","[What about the other side?]"," Those expressions are compiled into C logic that emulates the behavior of SQLite\non the data.  It provides complex expression constructs such as ",Object(l.b)("inlineCode",{parentName:"p"},"IN")," and ",Object(l.b)("inlineCode",{parentName:"p"},"CASE")," but it is ultimately evaluated by C\nexecution.  Alternately, anything that is inside of a piece of SQL is necessarily evaluated by SQLite itself.\nTo make this clearer let's change the example a little bit before we move on."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  set t := (select \"__\"||t||' '||1.234 from my_data);\n")),Object(l.b)("p",null,"This is a somewhat silly example but it illustrates some important things:"),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"even though SQLite doesn't support double quotes, that's no problem because CQL will convert the expression into single quotes with the correct escape values as a matter of course during compilation"),Object(l.b)("li",{parentName:"ul"},"the ",Object(l.b)("inlineCode",{parentName:"li"},"||")," concatenation operator is evaluated by SQLite"),Object(l.b)("li",{parentName:"ul"},"you can mix and match both kinds of string literals, they will all be the single quote variety by the time SQLite sees them"),Object(l.b)("li",{parentName:"ul"},"the ",Object(l.b)("inlineCode",{parentName:"li"},"||")," operator has lots of complex formatting conversions (such as converting real values to strings)"),Object(l.b)("li",{parentName:"ul"},"in fact the conversions are so subtle as to be impossible to emulate in loose C code with any economy, so, like a few other operators, ",Object(l.b)("inlineCode",{parentName:"li"},"||")," is only supported in the SQLite context")),Object(l.b)("p",null,"Returning now to our code as written, we see something very familiar:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  call printf('%s', t);\n")),Object(l.b)("p",null,"Note that we've used the single quote syntax here for no good reason other than illustration. There are no escape\nsequences here so either form would do the job. Importantly, the string literal will not create a string object as before\nbut the text variable ",Object(l.b)("inlineCode",{parentName:"p"},"t")," is of course a string reference.  Before it can be used in a call to an un-declared function it\nmust be converted into a temporary C string.  This might require allocation in general, that allocation is automatically\nmanaged."),Object(l.b)("p",null,'Also, note that CQL assumes that calls to "no check" functions should be emitted as written.  In this way you can use\n',Object(l.b)("inlineCode",{parentName:"p"},"printf")," even though CQL knows nothing about it."),Object(l.b)("p",null,"Lastly we have:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  drop table my_data;\n")),Object(l.b)("p",null,"This is not strictly necessary because the database is in memory anyway and the program is about to exit but there\nit is for illustration."),Object(l.b)("p",null,"Now the Data Manipulation Language (i.e. insert and select here; and henceforth DML) and the DDL might fail for various reasons.  If that happens the proc will ",Object(l.b)("inlineCode",{parentName:"p"},"goto")," a cleanup handler and return the failed return code instead of running the rest of the code.  Any temporary memory allocations will be freed and any pending\nSQLite statements will be finalized.  More on that later when we discuss error handling."),Object(l.b)("p",null,'With that we have a much more complicated program that prints "Hello, world"'),Object(l.b)("h3",{id:"introducing-cursors"},"Introducing Cursors"),Object(l.b)("p",null,"In order to read data with reasonable flexibility, we need a more powerful construction.\nLet's change our example again and start using some database features."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"declare procedure printf no check;\n\ncreate proc hello()\nbegin\n  create table my_data(\n    pos integer not null primary key,\n    txt text not null\n  );\n\n  insert into my_data values(2, 'World');\n  insert into my_data values(0, 'Hello');\n  insert into my_data values(1, 'There');\n\n  declare C cursor for select * from my_data order by pos;\n\n  loop fetch C\n  begin\n    call printf(\"%d: %s\\n\", C.pos, C.txt);\n  end;\n  close C;\n\n  drop table my_data;\nend;\n")),Object(l.b)("p",null,"Reviewing the essential parts of the above."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  create table my_data(\n    pos integer not null primary key,\n    txt text not null\n  );\n")),Object(l.b)("p",null,"The table now includes a position column to give us some ordering.  That is the primary key."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  insert into my_data values(2, 'World');\n")),Object(l.b)("p",null,"The insert statements provide both columns, not in the printed order.  The insert form where the columns are not\nspecified indicates that all the columns will be present, in order; this is more economical to type.  CQL will generate errors at compile time if there are any missing columns or if any of the values are not type compatible with the indicated column."),Object(l.b)("p",null,"The most important change is here:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  declare C cursor for select * from my_data order by pos;\n")),Object(l.b)("p",null,"We've created a non-scalar variable ",Object(l.b)("inlineCode",{parentName:"p"},"C"),", a cursor over the indicated result set.  The results will be ordered by ",Object(l.b)("inlineCode",{parentName:"p"},"pos"),"."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  loop fetch C\n  begin\n   ...\n  end;\n")),Object(l.b)("p",null,"This loop will run until there are no results left (it might not run at all if there are zero rows, that is not an error).\nThe ",Object(l.b)("inlineCode",{parentName:"p"},"FETCH")," construct allows you to specify target variables, but if you do not do so, then a synthetic structure is\nautomatically created to capture the projection of the ",Object(l.b)("inlineCode",{parentName:"p"},"select"),".  In this case the columns are ",Object(l.b)("inlineCode",{parentName:"p"},"pos")," and ",Object(l.b)("inlineCode",{parentName:"p"},"txt"),".\nThe automatically created storage exactly matches the type of the columns in the select list which could itself be tricky to calculate\nif the ",Object(l.b)("inlineCode",{parentName:"p"},"select")," is complex.  In this case the ",Object(l.b)("inlineCode",{parentName:"p"},"select")," is quite simple and the columns of the result directly match the schema for ",Object(l.b)("inlineCode",{parentName:"p"},"my_data"),".\nAn integer and a string reference.  Both not null."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'  call printf("%d: %s\\n", C.pos, C.txt);\n')),Object(l.b)("p",null,"The storage for the cursor is given the same names as the columns of the projection of the select, in this case the columns were not renamed so ",Object(l.b)("inlineCode",{parentName:"p"},"pos")," and ",Object(l.b)("inlineCode",{parentName:"p"},"txt")," are the fields in the cursor.\nDouble quotes were used in the format string to get the newline in there easily."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"  close C;\n")),Object(l.b)("p",null,"The cursor is automatically released at the end of the procedure but in this case we'd like to release it before the\n",Object(l.b)("inlineCode",{parentName:"p"},"drop table")," happens so there is an explicit ",Object(l.b)("inlineCode",{parentName:"p"},"close"),". This is frequently elided in favor of the automatic cleanup.\nThere is an ",Object(l.b)("inlineCode",{parentName:"p"},"open")," cursor statement as well but it doesn't do anything.  It's there because many systems have that\nconstruct and it does balance the ",Object(l.b)("inlineCode",{parentName:"p"},"close"),"."),Object(l.b)("p",null,"If you compile and run this program, you'll get this output:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-bash"},"$ cc -x c -E hello.sql | cql --cg hello.h hello.c\n$ cc -o hello main.c hello.c cqlrt.c -lsqlite3\n$ ./hello\n0: Hello\n1: There\n2: World\n")),Object(l.b)("p",null,"So the data was inserted and then sorted."),Object(l.b)("h3",{id:"going-crazy"},"Going Crazy"),Object(l.b)("p",null,"We've only scratched the surface of what SQLite can do and most DML constructs are supported by CQL.\nThis includes common table expressions, and even recursive versions of the same. But remember, when it\ncomes to DML, the CQL compiler only has to validate the types and figure out what the result shape will be --\nSQLite always does all the heavy lifting of evaluation. All of this means with remarkably little additional code,\nthe example below from the SQLite documentation can be turned into a CQL stored proc using the constructs\nwe have defined above."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'-- needed to allow vararg calls to C functions\ndeclare procedure printf no check;\n\ncreate proc mandelbrot()\nbegin\n  -- this is basically one giant select statement\n  declare C cursor for\n    with recursive\n      -- x from -2.0 to +1.2\n      xaxis(x) as (select -2.0 union all select x + 0.05 from xaxis where x < 1.2),\n\n      -- y from -1.0 to +1.0\n      yaxis(y) as (select -1.0 union all select y + 0.1 from yaxis where y < 1.0),\n\n      m(iter, cx, cy, x, y) as (\n        -- initial seed iteration count 0, at each of the points in the above grid\n        select 0 iter, x cx, y cy, 0.0 x, 0.0 y from xaxis, yaxis\n        union all\n        -- the next point is always iter +1, same (x,y) and the next iteration of z^2 + c\n        select iter+1 iter, cx, cy, x*x-y*y + cx x, 2.0*x*y + cy y from m\n        -- stop condition, the point has escaped OR iteration count > 28\n        where (m.x*m.x + m.y*m.y) < 4.0 and m.iter < 28\n      ),\n      m2(iter, cx, cy) as (\n       -- find the last iteration for any given point to get that count\n       select max(iter), cx, cy from m group by cx, cy\n      ),\n      a(t) as (\n        -- convert the iteration count to a printable character, grouping by line\n        select group_concat(substr(" .+*#", 1 + min(iter/7,4), 1), \'\')\n        from m2 group by cy\n      )\n    -- group all the lines together\n    select rtrim(t) line from a;\n\n  -- slurp out the data\n  loop fetch C\n  begin\n    call printf("%s\\n", C.line);\n  end;\nend;\n')),Object(l.b)("p",null,"This code uses all kinds of SQLite features to produce this text:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-bash"},"$\n                                    ....#\n                                   ..#*..\n                                 ..+####+.\n                            .......+####....   +\n                           ..##+*##########+.++++\n                          .+.##################+.\n              .............+###################+.+\n              ..++..#.....*#####################+.\n             ...+#######++#######################.\n          ....+*################################.\n #############################################...\n          ....+*################################.\n             ...+#######++#######################.\n              ..++..#.....*#####################+.\n              .............+###################+.+\n                          .+.##################+.\n                           ..##+*##########+.++++\n                            .......+####....   +\n                                 ..+####+.\n                                   ..#*..\n                                    ....#\n                                    +.\n")),Object(l.b)("p",null,"Which probably doesn't come up very often but it does illustrate several things:"),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},Object(l.b)("inlineCode",{parentName:"li"},"WITH RECURSIVE")," actually provides a full lambda calculus so arbitrary computation is possible"),Object(l.b)("li",{parentName:"ul"},"You can use ",Object(l.b)("inlineCode",{parentName:"li"},"WITH RECURSIVE")," to create table expressions that are sequences of numbers easily, with no reference to any real data")),Object(l.b)("p",null,"Note:"),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"A working version of this code can be found in the ",Object(l.b)("inlineCode",{parentName:"li"},"sources/demo")," directory of CG/SQL project."),Object(l.b)("li",{parentName:"ul"},"Additional demo code is available in ",Object(l.b)("a",{parentName:"li",href:"https://cgsql.dev/cql-guide/x10"},"Appendix 10"),".")))}u.isMDXComponent=!0},160:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return h}));var a=n(0),o=n.n(a);function l(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){l(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},l=Object.keys(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var c=o.a.createContext({}),b=function(e){var t=o.a.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=b(e.components);return o.a.createElement(c.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return o.a.createElement(o.a.Fragment,{},t)}},d=o.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,l=e.originalType,r=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),u=b(n),d=a,h=u["".concat(r,".").concat(d)]||u[d]||p[d]||l;return n?o.a.createElement(h,i(i({ref:t},c),{},{components:n})):o.a.createElement(h,i({ref:t},c))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=n.length,r=new Array(l);r[0]=d;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i.mdxType="string"==typeof e?e:a,r[1]=i;for(var c=2;c<l;c++)r[c]=n[c];return o.a.createElement.apply(null,r)}return o.a.createElement.apply(null,n)}d.displayName="MDXCreateElement"}}]);
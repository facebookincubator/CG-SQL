"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[6628],{3905:function(e,t,n){n.d(t,{Zo:function(){return u},kt:function(){return m}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),d=p(n),m=r,h=d["".concat(s,".").concat(m)]||d[m]||c[m]||i;return n?a.createElement(h,l(l({ref:t},u),{},{components:n})):a.createElement(h,l({ref:t},u))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,l=new Array(i);l[0]=d;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var p=2;p<i;p++)l[p]=n[p];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},528:function(e,t,n){n.r(t),n.d(t,{assets:function(){return u},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return o},metadata:function(){return p},toc:function(){return c}});var a=n(7462),r=n(3366),i=(n(7294),n(3905)),l=["components"],o={id:"ch04",title:"Chapter 4: Procedures, Functions, and Control Flow",sidebar_label:"Chapter 4: Procedures, Functions, and Control Flow"},s=void 0,p={unversionedId:"ch04",id:"ch04",title:"Chapter 4: Procedures, Functions, and Control Flow",description:"\x3c!---",source:"@site/../CQL_Guide/ch04.md",sourceDirName:".",slug:"/ch04",permalink:"/cql-guide/ch04",draft:!1,tags:[],version:"current",lastUpdatedBy:"Rico Mariani",lastUpdatedAt:1663643478,formattedLastUpdatedAt:"9/20/2022",frontMatter:{id:"ch04",title:"Chapter 4: Procedures, Functions, and Control Flow",sidebar_label:"Chapter 4: Procedures, Functions, and Control Flow"},sidebar:"someSidebar",previous:{title:"Chapter 3: Expressions, Literals, Nullability, Sensitivity",permalink:"/cql-guide/ch03"},next:{title:"Chapter 5: Types of Cursors, Shapes, OUT and OUT UNION, and FETCH",permalink:"/cql-guide/ch05"}},u={},c=[{value:"Out Parameters",id:"out-parameters",level:3},{value:"Procedure Calls",id:"procedure-calls",level:3},{value:"The IF statement",id:"the-if-statement",level:3},{value:"The WHILE statement",id:"the-while-statement",level:3},{value:"The SWITCH Statement",id:"the-switch-statement",level:3},{value:"The TRY, CATCH, and THROW Statements",id:"the-try-catch-and-throw-statements",level:3},{value:"Procedures as Functions: Motivation and Example",id:"procedures-as-functions-motivation-and-example",level:3}],d={toc:c};function m(e){var t=e.components,n=(0,r.Z)(e,l);return(0,i.kt)("wrapper",(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"All kinds of control flow happens in the context of some procedure. Though we've already introduced examples of procedures let's\nnow go over some of the additional aspects we have not yet illustrated."),(0,i.kt)("h3",{id:"out-parameters"},"Out Parameters"),(0,i.kt)("p",null,"Consider this procedure:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"create procedure echo_integer(in arg1 integer not null, out arg2 integer not null)\nbegin\n  set arg2 := arg1;\nend;\n")),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"arg1")," has been declared ",(0,i.kt)("inlineCode",{parentName:"p"},"in"),". This is the default: ",(0,i.kt)("inlineCode",{parentName:"p"},"in arg1 integer not null"),"\nand ",(0,i.kt)("inlineCode",{parentName:"p"},"arg1 integer not null")," mean the exact same thing."),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"arg2"),", however, has been declared ",(0,i.kt)("inlineCode",{parentName:"p"},"out"),". When a parameter is declared using\n",(0,i.kt)("inlineCode",{parentName:"p"},"out"),", arguments for it are passed by reference. This is similar to by-reference\narguments in other languages; indeed, they compile into a simple pointer\nreference in the generated C code."),(0,i.kt)("p",null,"Given that ",(0,i.kt)("inlineCode",{parentName:"p"},"arg2")," is passed by reference, the statement ",(0,i.kt)("inlineCode",{parentName:"p"},"set arg2 := arg1;"),"\nactually updates a variable in the caller. For example:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"declare x int not null;\ncall echo_integer(42, x);\n-- `x` is now 42\n")),(0,i.kt)("p",null,"It is important to note that values cannot be passed ",(0,i.kt)("em",{parentName:"p"},"into")," a procedure via an\n",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameter. In fact, ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters are immediately assigned a new value\nas soon as the procedure is called:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"All nullable ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters are set to ",(0,i.kt)("inlineCode",{parentName:"p"},"null"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Nonnull ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters of a non-reference type (e.g., ",(0,i.kt)("inlineCode",{parentName:"p"},"integer"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"long"),",\n",(0,i.kt)("inlineCode",{parentName:"p"},"bool"),", et cetera) are set to their default values (",(0,i.kt)("inlineCode",{parentName:"p"},"0"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"0.0"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"false"),", et\ncetera).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Nonnull ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters of a reference type (e.g., ",(0,i.kt)("inlineCode",{parentName:"p"},"blob"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"object"),", and\n",(0,i.kt)("inlineCode",{parentName:"p"},"text"),") are set to ",(0,i.kt)("inlineCode",{parentName:"p"},"null")," as there are no default values for reference types.\nThey must, therefore, be assigned a value within the procedure so that they\nwill not be ",(0,i.kt)("inlineCode",{parentName:"p"},"null")," when the procedure returns. CQL enforces this."))),(0,i.kt)("p",null,"In addition to ",(0,i.kt)("inlineCode",{parentName:"p"},"in")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters, there are also ",(0,i.kt)("inlineCode",{parentName:"p"},"inout")," parameters.\n",(0,i.kt)("inlineCode",{parentName:"p"},"inout")," parameters are, as one might expect, a combination of ",(0,i.kt)("inlineCode",{parentName:"p"},"in")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"out"),"\nparameters: The caller passes in a value as with ",(0,i.kt)("inlineCode",{parentName:"p"},"in")," parameters, but the value\nis passed by reference as with ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," parameters."),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"inout")," parameters allow for code such as the following:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"create procedure times_two(inout arg integer not null)\nbegin\n  -- note that a variable in the caller is both\n  -- read from and written to\n  set arg := arg + arg;\nend;\n\nlet x := 2;\ncall times_two(x);\n-- `x` is now 4\n")),(0,i.kt)("h3",{id:"procedure-calls"},"Procedure Calls"),(0,i.kt)("p",null,"The usual ",(0,i.kt)("inlineCode",{parentName:"p"},"call")," syntax is used to invoke a procedure.  It returns no value but it can have any number of ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," arguments."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},"  declare scratch integer not null;\n  call echo_integer(12, scratch);\n  scratch == 12; -- true\n")),(0,i.kt)("p",null,"Let's go over the most essential bits of control flow."),(0,i.kt)("h3",{id:"the-if-statement"},"The IF statement"),(0,i.kt)("p",null,"The CQL ",(0,i.kt)("inlineCode",{parentName:"p"},"IF")," statement has no syntatic ambiguities at the expense of being somewhat more verbose than many other languages.\nIn CQL the ",(0,i.kt)("inlineCode",{parentName:"p"},"ELSE IF")," portion is baked into the ",(0,i.kt)("inlineCode",{parentName:"p"},"IF")," statement, so what you see below is logically a single statement."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"create proc checker(foo integer, out result integer not null)\nbegin\n  if foo = 1 then\n   set result := 1;\n  else if foo = 2 then\n   set result := 3;\n  else\n   set result := 5;\n  end if;\nend;\n")),(0,i.kt)("h3",{id:"the-while-statement"},"The WHILE statement"),(0,i.kt)("p",null,"What follows is a simple procedure that counts down its input argument."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"declare procedure printf no check;\n\ncreate proc looper(x integer not null)\nbegin\n  while x > 0\n  begin\n   call printf('%d\\n', x);\n   set x := x - 1;\n  end;\nend;\n")),(0,i.kt)("p",null,"The ",(0,i.kt)("inlineCode",{parentName:"p"},"WHILE")," loop has additional keywords that can be used within it to better control the loop.  A more general\nloop might look like this:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"declare procedure printf no check;\n\ncreate proc looper(x integer not null)\nbegin\n  while 1\n  begin\n   set x := x - 1;\n   if x < 0 then\n     leave;\n   else if x % 100 = 0 then\n     continue;\n   else if x % 10 = 0 then\n     call printf('%d\\n', x);\n   end if;\n  end;\nend;\n")),(0,i.kt)("p",null,"Let's go over this peculiar loop:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"  while 1\n  begin\n    ...\n  end;\n")),(0,i.kt)("p",null,"This is an immediate sign that there will be an unusual exit condition.  The loop will never end without one because ",(0,i.kt)("inlineCode",{parentName:"p"},"1")," will never be false."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"   if x < 0 then\n     leave;\n")),(0,i.kt)("p",null,"Now here we've encoded our exit condition a bit strangely: we might have done the equivalent job with a normal condition in the predicate\npart of the ",(0,i.kt)("inlineCode",{parentName:"p"},"while")," statement but for illustration anyway, when x becomes negative ",(0,i.kt)("inlineCode",{parentName:"p"},"leave")," will cause us to exit the loop.  This is like\n",(0,i.kt)("inlineCode",{parentName:"p"},"break")," in C."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"   else if x % 100 = 0 then\n     continue;\n")),(0,i.kt)("p",null,"This bit says that on every 100th iteration we go back to the start of the loop.  So the next bit will not run, which is the printing."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"   else if x % 10 = 0 then\n     call printf('%d\\n', x);\n   end if;\n")),(0,i.kt)("p",null,"Finishing up the control flow, on every 10th iteration we print the value of the loop variable."),(0,i.kt)("h3",{id:"the-switch-statement"},"The SWITCH Statement"),(0,i.kt)("p",null,"The  CQL ",(0,i.kt)("inlineCode",{parentName:"p"},"SWITCH")," is designed to map to the C ",(0,i.kt)("inlineCode",{parentName:"p"},"switch")," statement for better codegen and also to give us the opportunity to do better error checking.\n",(0,i.kt)("inlineCode",{parentName:"p"},"SWITCH")," is a ",(0,i.kt)("em",{parentName:"p"},"statement")," like ",(0,i.kt)("inlineCode",{parentName:"p"},"IF")," not an ",(0,i.kt)("em",{parentName:"p"},"expression")," like ",(0,i.kt)("inlineCode",{parentName:"p"},"CASE..WHEN..END")," so it combines with other statements. The general form looks like this:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-SQL"},"SWITCH switch-expression [optional ALL VALUES]\nWHEN expr1, expr2, ... THEN\n  [statement_list]\nWHEN expr3, ... THEN\n  [statement_list]\nWHEN expr4 THEN\n  NOTHING\nELSE\n  [statement_list]\nEND;\n")),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"the switch-expression must be a not-null integral type (",(0,i.kt)("inlineCode",{parentName:"li"},"integer not null")," or ",(0,i.kt)("inlineCode",{parentName:"li"},"long integer not null"),")"),(0,i.kt)("li",{parentName:"ul"},"the ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")," expressions ","[expr1, expr2, etc.]"," are made from constant integer expressions (e.g. ",(0,i.kt)("inlineCode",{parentName:"li"},"5"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"1+7"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"1<<2"),", or ",(0,i.kt)("inlineCode",{parentName:"li"},"my_enum.thing"),")"),(0,i.kt)("li",{parentName:"ul"},"the ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")," expressions must be compatible with the switch expression (long constants cannot be used if the switch expression is an integer)"),(0,i.kt)("li",{parentName:"ul"},"the values in the ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")," clauses must be unique (after evaluation)"),(0,i.kt)("li",{parentName:"ul"},"within one of the interior statement lists the ",(0,i.kt)("inlineCode",{parentName:"li"},"LEAVE")," keyword exits the ",(0,i.kt)("inlineCode",{parentName:"li"},"SWITCH")," prematurely, just like ",(0,i.kt)("inlineCode",{parentName:"li"},"break")," in C",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"a ",(0,i.kt)("inlineCode",{parentName:"li"},"LEAVE")," is not required before the next ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")),(0,i.kt)("li",{parentName:"ul"},"there are no fall-through semantics as you can find in ",(0,i.kt)("inlineCode",{parentName:"li"},"C"),", if fall-through ever comes to ",(0,i.kt)("inlineCode",{parentName:"li"},"SWITCH")," it will be explicit"))),(0,i.kt)("li",{parentName:"ul"},"if the keyword ",(0,i.kt)("inlineCode",{parentName:"li"},"NOTHING")," is used after ",(0,i.kt)("inlineCode",{parentName:"li"},"THEN")," it means there is no code for that case, which is useful with ",(0,i.kt)("inlineCode",{parentName:"li"},"ALL VALUES")," (see below)"),(0,i.kt)("li",{parentName:"ul"},"the ",(0,i.kt)("inlineCode",{parentName:"li"},"ELSE")," clause is optional and works just like ",(0,i.kt)("inlineCode",{parentName:"li"},"default")," in ",(0,i.kt)("inlineCode",{parentName:"li"},"C"),", covering any cases not otherwise explicitly listed"),(0,i.kt)("li",{parentName:"ul"},"if you add ",(0,i.kt)("inlineCode",{parentName:"li"},"ALL VALUES")," then:",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"the expression must be an from an enum type"),(0,i.kt)("li",{parentName:"ul"},"the ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")," values must cover every value of the enum",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"enum members that start with a leading ",(0,i.kt)("inlineCode",{parentName:"li"},"_")," are by convention considered pseudo values and do not need to be covered"))),(0,i.kt)("li",{parentName:"ul"},"there can be no extra ",(0,i.kt)("inlineCode",{parentName:"li"},"WHEN")," values not in the enum"),(0,i.kt)("li",{parentName:"ul"},"there can be no ",(0,i.kt)("inlineCode",{parentName:"li"},"ELSE")," clause (it would defeat the point of listing ",(0,i.kt)("inlineCode",{parentName:"li"},"ALL VALUES")," which is to get an error if new values come along)")))),(0,i.kt)("p",null,"Some more complete examples:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"let x := get_something();\nswitch x\n  when 1,1+1 then -- constant expressions ok\n    set y := 'small';\n    -- other stuff\n  when 3,4,5 then\n    set y := 'medium';\n    -- other stuff\n  when 6,7,8 then\n    set y := 'large';\n    -- other stuff\n  else\n    set y := 'zomg enormous';\nend;\n\ndeclare enum item integer (\n  pen = 0, pencil, brush,\n  paper, canvas,\n  _count\n);\n\nlet x := get_item(); -- returns one of the above\n\nswitch x all values\n  when item.pen, item.pencil then\n     call write_something();\n  when item.brush then nothing\n     -- itemize brush but it needs no code\n  when item.paper, item.canvas then\n    call setup_writing();\nend;\n")),(0,i.kt)("p",null,"Using ",(0,i.kt)("inlineCode",{parentName:"p"},"THEN NOTHING")," allows the compiler to avoid emitting a useless ",(0,i.kt)("inlineCode",{parentName:"p"},"break")," in the C code.  Hence that choice is better/clearer than ",(0,i.kt)("inlineCode",{parentName:"p"},"when brush then leave;")),(0,i.kt)("p",null,"Note that the presence of ",(0,i.kt)("inlineCode",{parentName:"p"},"_count")," in the enum will not cause an error in the above because it starts with ",(0,i.kt)("inlineCode",{parentName:"p"},"_"),"."),(0,i.kt)("p",null,"The ",(0,i.kt)("inlineCode",{parentName:"p"},"C")," output for this statement will be a direct mapping to a ",(0,i.kt)("inlineCode",{parentName:"p"},"C")," switch statement."),(0,i.kt)("h3",{id:"the-try-catch-and-throw-statements"},"The TRY, CATCH, and THROW Statements"),(0,i.kt)("p",null,'This example illustrates catching an error from some DML, and recovering rather than letting the error cascade up.\nThis is the common "upsert" pattern (insert or update)'),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},'declare procedure printf no check;\n\ncreate procedure upsert_foo(id_ integer, t_ text)\nbegin\n  begin try\n    insert into foo(id, t) values(id_, t_)\n  end try;\n  begin catch\n    begin try\n      update foo set t = t_ where id = id_;\n    end try;\n    begin catch\n      call printf("Error code %d!\\n", @rc);\n      throw;\n    end catch;\n  end catch;\nend;\n')),(0,i.kt)("p",null,"Once again, let's go over this section by section:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"  begin try\n    insert into foo(id, t) values(id_, t_)\n  end try;\n")),(0,i.kt)("p",null,"Normally if the ",(0,i.kt)("inlineCode",{parentName:"p"},"insert")," statement fails, the procedure will exit with a failure result code.  Here, instead,\nwe prepare to catch that error."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"  begin catch\n    begin try\n      update foo set t = t_ where id = id_;\n    end try;\n")),(0,i.kt)("p",null,"Now, having failed to insert, presumably because a row with the provided ",(0,i.kt)("inlineCode",{parentName:"p"},"id")," already exists, we try to update\nthat row instead.  However that might also fail, so we  wrap it in another try.  If the update fails, then there is a final catch block:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},'    begin catch\n      call printf("Error code %d!\\n", @rc);\n      throw;\n    end catch;\n')),(0,i.kt)("p",null,"Here we see a usage of the ",(0,i.kt)("inlineCode",{parentName:"p"},"@rc")," variable to observe the failed error code.  In this case we simply print a diagnostic message and\nthen use the ",(0,i.kt)("inlineCode",{parentName:"p"},"throw")," keyword to rethrow the previous failure (exactly what is stored in ",(0,i.kt)("inlineCode",{parentName:"p"},"@rc"),").  In general, ",(0,i.kt)("inlineCode",{parentName:"p"},"throw")," will create a\nfailure in the current block using the most recent failed result code from SQLite (",(0,i.kt)("inlineCode",{parentName:"p"},"@rc"),") if it is an error, or else the general\n",(0,i.kt)("inlineCode",{parentName:"p"},"SQLITE_ERROR")," result code if there is no such error.  In this case the failure code for the ",(0,i.kt)("inlineCode",{parentName:"p"},"update")," statement will become the\nresult code of the current procedure."),(0,i.kt)("p",null,"This leaves only the closing markers:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"  end catch;\nend;\n")),(0,i.kt)("p",null,"If control flow reaches the normal end of the procedure it will return ",(0,i.kt)("inlineCode",{parentName:"p"},"SQLITE_OK"),"."),(0,i.kt)("h3",{id:"procedures-as-functions-motivation-and-example"},"Procedures as Functions: Motivation and Example"),(0,i.kt)("p",null,"The calling convention for CQL stored procedures often (usually) requires that the procedure returns a result code from SQLite.\nThis makes it impossible to write a procedure that returns a result like a function, as the result position is already used for\nthe error code.  You can get around this problem by using ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," arguments as your return codes.  So for instance, this version\nof the Fibonacci function is possible."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"-- this works, but it is awkward\ncreate procedure fib (in arg integer not null, out result integer not null)\nbegin\n  if (arg <= 2) then\n    set result := 1;\n  else\n    declare t integer not null;\n    call fib(arg - 1,  result);\n    call fib(arg - 2,  t);\n    set result := t + result;\n  end if;\nend;\n")),(0,i.kt)("p",null,"The above works, but the notation is very awkward."),(0,i.kt)("p",null,'CQL has a "procedures as functions" feature that tries to make this more pleasant by making it possible to use function call notation\non a procedure whose last argument is an ',(0,i.kt)("inlineCode",{parentName:"p"},"out")," variable.  You simply call the procedure like it was a function and omit the last argument in the call.\nA temporary variable is automatically created to hold the result and that temporary becomes the logical return of the function.\nFor semantic analysis, the result type of the function becomes the type of the ",(0,i.kt)("inlineCode",{parentName:"p"},"out")," argument."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql"},"-- rewritten with function call syntax\ncreate procedure fib (in arg integer not null, out result integer not null)\nbegin\n  if (arg <= 2) then\n    set result := 1;\n  else\n    set result := fib(arg - 1) + fib(arg - 2);\n  end if;\nend;\n")),(0,i.kt)("p",null,"This form is allowed when:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"all but the last argument of the procedure was specified"),(0,i.kt)("li",{parentName:"ul"},"the formal parameter for that last argument was marked with ",(0,i.kt)("inlineCode",{parentName:"li"},"out")," (neither ",(0,i.kt)("inlineCode",{parentName:"li"},"in")," nor ",(0,i.kt)("inlineCode",{parentName:"li"},"inout")," are acceptable)"),(0,i.kt)("li",{parentName:"ul"},"the procedure does not return a result set using a ",(0,i.kt)("inlineCode",{parentName:"li"},"select")," statement or ",(0,i.kt)("inlineCode",{parentName:"li"},"out")," statement (more on these later)")),(0,i.kt)("p",null,"If the procedure in question uses SQLite, or calls something that uses SQLite, then it might fail.\nIf that happens the result code will propagate just like it would have with the usual ",(0,i.kt)("inlineCode",{parentName:"p"},"call")," form.\nAny failures can be caught with ",(0,i.kt)("inlineCode",{parentName:"p"},"try/catch"),' as usual.\nThis feature is really only syntatic sugar for the "awkward" form above, but it does allow for slightly better generated C code.'))}m.isMDXComponent=!0}}]);
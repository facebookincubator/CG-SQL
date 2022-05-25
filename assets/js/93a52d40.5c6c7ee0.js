"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[1519],{3905:function(e,t,n){n.d(t,{Zo:function(){return c},kt:function(){return h}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var i=a.createContext({}),u=function(e){var t=a.useContext(i),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},c=function(e){var t=u(e.components);return a.createElement(i.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},p=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),p=u(n),h=r,d=p["".concat(i,".").concat(h)]||p[h]||m[h]||o;return n?a.createElement(d,s(s({ref:t},c),{},{components:n})):a.createElement(d,s({ref:t},c))}));function h(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,s=new Array(o);s[0]=p;var l={};for(var i in t)hasOwnProperty.call(t,i)&&(l[i]=t[i]);l.originalType=e,l.mdxType="string"==typeof e?e:r,s[1]=l;for(var u=2;u<o;u++)s[u]=n[u];return a.createElement.apply(null,s)}return a.createElement.apply(null,n)}p.displayName="MDXCreateElement"},3223:function(e,t,n){n.r(t),n.d(t,{assets:function(){return c},contentTitle:function(){return i},default:function(){return h},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return m}});var a=n(7462),r=n(3366),o=(n(7294),n(3905)),s=["components"],l={slug:"update",title:"One Month Update",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql","update"]},i=void 0,u={permalink:"/blog/update",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2020-11-12-update.md",source:"@site/blog/2020-11-12-update.md",title:"One Month Update",description:"It's hard to believe it's been a month since the welcome message went up. We were",date:"2020-11-12T00:00:00.000Z",formattedDate:"November 12, 2020",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"},{label:"update",permalink:"/blog/tags/update"}],readingTime:8,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"update",title:"One Month Update",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql","update"]},prevItem:{title:'More Flexible Cursor Patterns Using "Boxing"',permalink:"/blog/boxed-cursors-intro"},nextItem:{title:"Welcome",permalink:"/blog/welcome"}},c={authorsImageUrls:[void 0]},m=[{value:"Here&#39;s a quick summary of what&#39;s been going on:",id:"heres-a-quick-summary-of-whats-been-going-on",level:2},{value:"And now a few notes on The Big Stuff",id:"and-now-a-few-notes-on-the-big-stuff",level:2},{value:"Declare cursors in the shape of a procedure&#39;s arguments and use them",id:"declare-cursors-in-the-shape-of-a-procedures-arguments-and-use-them",level:3},{value:"Loading cursors and inserting columns",id:"loading-cursors-and-inserting-columns",level:3},{value:"Putting these together",id:"putting-these-together",level:3},{value:"Cursor Differencing",id:"cursor-differencing",level:3}],p={toc:m};function h(e){var t=e.components,n=(0,r.Z)(e,s);return(0,o.kt)("wrapper",(0,a.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,"It's hard to believe it's been a month since the welcome message went up. We were\nhappy to see interest right away and even a bunch of forks but most of all\npull requests.  A sweeping change to modernize the cql.y grammar was much\nappreciated.  That ",(0,o.kt)("inlineCode",{parentName:"p"},"$1")," stuff was very old school (I'm showing my age now)."),(0,o.kt)("h2",{id:"heres-a-quick-summary-of-whats-been-going-on"},"Here's a quick summary of what's been going on:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"@mingodad gave us an implementation of check and collate column attributes (the check attribute on tables should be easy to add from here)"),(0,o.kt)("li",{parentName:"ul"},"the ",(0,o.kt)("inlineCode",{parentName:"li"},"select function")," form should never return objects, only SQLite types, enforced"),(0,o.kt)("li",{parentName:"ul"},"@attribute(cql:suppress_result_set) was added to save code gen for procedures that don't need the C result set wrappers"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"cql_cursor_diff_col")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"cql_cursor_diff_val")," methods were added to report what's different about two cursors (highly useful in test code)"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"cql_cursor_format")," was added so you can quickly convert any cursor into columns and values as string for debug output (no matter the shape)"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"sqlite3_changes")," was added to the builtin list so you don't have to use ",(0,o.kt)("inlineCode",{parentName:"li"},"declare select function")," to use it anymore"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"cql_get_blob_size")," was added so you can see how big your blobs are (useful for diagnostics)"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"trim"),", ",(0,o.kt)("inlineCode",{parentName:"li"},"rtrim")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"ltrim")," were added to the builtin list so you can use them without ",(0,o.kt)("inlineCode",{parentName:"li"},"declare select function")),(0,o.kt)("li",{parentName:"ul"},"the builtin function ",(0,o.kt)("inlineCode",{parentName:"li"},"ifnull_crash")," was added so that nullables that have already checked can be safely typecast to not null"),(0,o.kt)("li",{parentName:"ul"},"the bug we saw in demo video number 2 where some foreign keys were not properly linked up in autotest code was fixed (yay videos)"),(0,o.kt)("li",{parentName:"ul"},"time functions are now known to be ",(0,o.kt)("inlineCode",{parentName:"li"},"not null")," for a bunch of simple cases such as 'now' arguments"),(0,o.kt)("li",{parentName:"ul"},"you can use the ",(0,o.kt)("inlineCode",{parentName:"li"},"cast(.. as ..)")," operator on numeric types outside of the SQL context"),(0,o.kt)("li",{parentName:"ul"},"@mingodad replaced all the positional references by named references in cql.y (yes! thank you!)"),(0,o.kt)("li",{parentName:"ul"},"several minor bug fixes"),(0,o.kt)("li",{parentName:"ul"},"the railroad diagrams were updated")),(0,o.kt)("p",null,'NOTE: I often refer to "sugar" in the below.  This is short for syntatic sugar which, in case you\'re not familiar with the term, refers to a syntatically more pleasing way of writing a concept that is otherwise totally doable with normal syntax.  Many languages have sugar for forms that are common -- for brevity, clarity, and/or correctness.'),(0,o.kt)("h2",{id:"and-now-a-few-notes-on-the-big-stuff"},"And now a few notes on The Big Stuff"),(0,o.kt)("p",null,"We often add new features to the language to facilitate the writing of tests. The tests have a lot of boilerplate often setting up\nand calling the same procedures again and again with slightly different arguments. Long argument lists and long insert column\nlists are especially problematic as these can be very error prone. Here good language constructs are very helpful.\nWe've found good test constructs are often invaluable in production code as well, though in our experience the\ntests often have a lot more repitition that needs refactoring than production code. To that end we added some very useful things\nin the last month:"),(0,o.kt)("h3",{id:"declare-cursors-in-the-shape-of-a-procedures-arguments-and-use-them"},"Declare cursors in the shape of a procedure's arguments and use them"),(0,o.kt)("p",null,"The most common way to create a cursor is from a ",(0,o.kt)("inlineCode",{parentName:"p"},"select")," statement but you can also make a cursor that can hold values for you\nby declaring it to be ",(0,o.kt)("inlineCode",{parentName:"p"},"LIKE")," something else with a shape.  A classic example is:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"declare C cursor like some_table;\n")),(0,o.kt)("p",null,"Now ",(0,o.kt)("inlineCode",{parentName:"p"},"C")," has the same columns and types as ",(0,o.kt)("inlineCode",{parentName:"p"},"some_table")),(0,o.kt)("p",null,"Many procedures have a result type that is also a shape, for instance any procedure that ends with a ",(0,o.kt)("inlineCode",{parentName:"p"},"select")," statement has a result\nshape defined by the columns of the select statement.  You could always do this sort of thing:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"declare C cursor like some_proc;\n")),(0,o.kt)("p",null,"Meaning make ",(0,o.kt)("inlineCode",{parentName:"p"},"C")," a cursor whose shape is whatever ",(0,o.kt)("inlineCode",{parentName:"p"},"some_proc"),"returns, which is of course exactly the kind of cursor you need to capture\nthe result of ",(0,o.kt)("inlineCode",{parentName:"p"},"some_proc"),"."),(0,o.kt)("p",null,"Now we add:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"declare C cursor like some_proc arguments;\n")),(0,o.kt)("p",null,"The idea being that the arguments of ",(0,o.kt)("inlineCode",{parentName:"p"},"some_proc")," are also a shape (unless it has none). With this done you want to use that cursor\nto call the procedure -- that being sort of the whole point.  So we add this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"call some_proc(from C);\n")),(0,o.kt)("p",null,"How do we use this effectively?  Hold on just a second -- for that answer we need one more big tool to really help the syntax."),(0,o.kt)("h3",{id:"loading-cursors-and-inserting-columns"},"Loading cursors and inserting columns"),(0,o.kt)("p",null,"Loading up a cursor is done with syntax that is very much like an ",(0,o.kt)("inlineCode",{parentName:"p"},"insert")," statement.  An example might be something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"fetch C(x,y,z) from values(1,2,3);\n")),(0,o.kt)("p",null,"This is simple enough but it becomes more problematic if there are many values and especially if the values have complex names.\nTo make this a little less error prone CQL now has this sugar form for ",(0,o.kt)("inlineCode",{parentName:"p"},"fetch"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"insert"),", and soon ",(0,o.kt)("inlineCode",{parentName:"p"},"update cursor")," (like maybe\nbefore you see this blog).  The more readable form is:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"fetch C using\n  1  x,\n  2  y,\n  3  z;\n")),(0,o.kt)("p",null,"This form has the values next to their names just like in a select statement, like all sugars, it is automatically rewritten to the normal form."),(0,o.kt)("p",null,"Likewise"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"insert into some_table using\n  1            id,\n  'fred'       first_name,\n  'flintstone' last_name,\n  'bedrock'    home_town,\n  'dino'       favorite_pet,\n  'wilma'      life_partner;\n")),(0,o.kt)("p",null,"becomes"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"insert into some_table(id, first_name, last_name, home_town, favorite_pet, life_partner)\n  values(1, 'fred', 'flintstone', 'bedrock', 'dino', 'wilma');\n")),(0,o.kt)("p",null,"except the sugar form is much less error prone.  This form doesn't generalize to many values but the single row case is super common."),(0,o.kt)("p",null,"Since this form is automatically rewritten SQLite will never see the sugar syntax, it will get the normal syntax."),(0,o.kt)("p",null,"NOTE: the insert rewrite is coming later today, and will likely be live by the time you read this."),(0,o.kt)("h3",{id:"putting-these-together"},"Putting these together"),(0,o.kt)("p",null,"Let's suppose you have to write a test.  You have a procedure ",(0,o.kt)("inlineCode",{parentName:"p"},"test_subject")," that takes some arguments plus\nyou have another helper procedure ",(0,o.kt)("inlineCode",{parentName:"p"},"test_setup")," that puts seed data in the right places for your subject.\nBut there are many variations and  a lot of what you do between variations is the same.  How can you write this\neconomically making it clear what is different between variations without a lot of fuss.\nWell you can do something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"-- use defaults for all the named values\n-- use 'seed' for everything else that isn't named\ncreate proc default_setup_args(seed integer not null)\nbegin\n  declare args cursor like test_setup arguments;\n  fetch args using\n    1334    primary_id,\n    98012   secondary_id,\n    'foo'   useful_name,\n    'bar'   other_useful_name,\n    1       fast_mode\n    @dummy_seed(seed);\n  out args;\nend;\n")),(0,o.kt)("p",null,"With the above you can easily see which values go to which arguments"),(0,o.kt)("p",null,"Your test setup can now look something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"declare setup_args cursor like test_setup arguments;\nfetch setup_args from call default_setup_args(1999);\nupdate cursor setup_args using\n   0 fast_mode;  -- override fast mode for this test\ncall test_setup(from setup_args);\n")),(0,o.kt)("p",null,"To call the test subject you probably need some of those setup arguments and maybe some more things."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"create proc default_subject_args(like default_setup_args, other_thing bool not null)\nbegin\n  declare args cursor like test_subject arguments;\n  fetch args using\n     primary_id    primary_id,    -- this came from the default_setup_args result\n     secondary_id  secondary_id,  -- so did this\n     useful_name   name,          -- the field names don't have to match\n     fast_mode     fast_mode,\n     other_thing   other_thing;\n  out args;\nend;\n")),(0,o.kt)("p",null,"Then the test code"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"declare test_args cursor like test_subject arguments;\nfetch test_args from call default_subject_args(0);\ncall test_subject(from test_args);\n")),(0,o.kt)("p",null,"Importantly, the cursor set operations are all by name so the order doesn't matter.  Which means even if there are many arguments\nyou don't have to worry that you got them in the wrong order or that they are the wrong type.  Effectively you have\na simple call by name strategy and you can easily read off the arguments.  You could do something similarly brief with\nhelper functions to provide the default arguments but then you can't readily re-use those arguments in later calls or\nfor verification so this way seems a lot more useful in a test context."),(0,o.kt)("p",null,"When it comes time to validate, probably your test subject is returning a cursor from a select that you want to check.\nA slightly different call will do the job there."),(0,o.kt)("h3",{id:"cursor-differencing"},"Cursor Differencing"),(0,o.kt)("p",null,"With the setup above you can verify results very easily.  Let's change it a little bit:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"-- same as before, with a cursor\ndeclare results cursor for call test_subject(from test_args);\n\n-- get the first row\nfetch results;\n\ndeclare expected cursor like results;\nfetch expected using\n   setup_args.primary_id     primary_id,\n   setup_args.useful_name    name,\n   test_args.other_thing     other_thing\n   @dummy_seed(1999);   -- dummy values for all other columns\n\n-- make a macro like EXPECT_CURSOR_EQ(x,y) for this\n-- if the cursors are different the result is a string with the first\n-- different column name and the left and right values ready to print\n\ncall ExpectNull(cql_cursor_diff_val(expected, result));\n")),(0,o.kt)("p",null,"ExpectEqual could be"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"create proc ExpectNull(t text)\nbegin\n  if t is not null then\n    call printf('%s\\n', t); -- or whatever\n    throw;\n  end if;\nend;\n")),(0,o.kt)("p",null,"All that testing support comes from:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"cursors in the shape of arguments"),(0,o.kt)("li",{parentName:"ul"},"cleaner fetch/insert syntax"),(0,o.kt)("li",{parentName:"ul"},"cursors passed as arguments"),(0,o.kt)("li",{parentName:"ul"},"cursor differences")),(0,o.kt)("p",null,"It kills a lot of boilerplate resulting in tests that are much clearer."),(0,o.kt)("p",null,"And that's what's been going on for the last month in CG/SQL land."),(0,o.kt)("p",null,"If you got this far thanks for reading.  If you didn't get this far,\nyou aren't reading this anyway so thanking you is moot =P"),(0,o.kt)("p",null,"Stay safe."),(0,o.kt)("p",null,"Rico for CG/SQL"),(0,o.kt)("p",null,"P.S. most of these fragments don't actually compile because of missing schema and maybe the odd typo.  If there is interest I'll make a demo that\nworks soup to nuts."))}h.isMDXComponent=!0}}]);
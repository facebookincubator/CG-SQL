"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9321],{3905:function(e,t,n){n.d(t,{Zo:function(){return h},kt:function(){return p}});var a=n(7294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=a.createContext({}),u=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},h=function(e){var t=u(e.components);return a.createElement(s.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,s=e.parentName,h=l(e,["components","mdxType","originalType","parentName"]),d=u(n),p=o,m=d["".concat(s,".").concat(p)]||d[p]||c[p]||r;return n?a.createElement(m,i(i({ref:t},h),{},{components:n})):a.createElement(m,i({ref:t},h))}));function p(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,i=new Array(r);i[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:o,i[1]=l;for(var u=2;u<r;u++)i[u]=n[u];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},6981:function(e,t,n){n.r(t),n.d(t,{assets:function(){return h},contentTitle:function(){return s},default:function(){return p},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return c}});var a=n(7462),o=n(3366),r=(n(7294),n(3905)),i=["components"],l={slug:"parent-child",title:"Introducing Parent/Child Result Sets",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},s=void 0,u={permalink:"/blog/parent-child",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2022-10-06-parent-child.md",source:"@site/blog/2022-10-06-parent-child.md",title:"Introducing Parent/Child Result Sets",description:"Introduction and Context",date:"2022-10-06T00:00:00.000Z",formattedDate:"October 6, 2022",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],readingTime:14.64,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"parent-child",title:"Introducing Parent/Child Result Sets",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},prevItem:{title:"Some updates on the CQL schema upgrade system",permalink:"/blog/schema-notes-2022"},nextItem:{title:"Introducing Backed Tables",permalink:"/blog/backed-tables"}},h={authorsImageUrls:[void 0]},c=[{value:"Introduction and Context",id:"introduction-and-context",level:2},{value:"Cursor Types and Result Types",id:"cursor-types-and-result-types",level:2},{value:"Creating New Cursor Types From Existing Cursor Types",id:"creating-new-cursor-types-from-existing-cursor-types",level:2},{value:"Cursor Arguments",id:"cursor-arguments",level:3},{value:"The Specific Parent/Child Functions",id:"the-specific-parentchild-functions",level:2},{value:"Result Set Sugar",id:"result-set-sugar",level:2},{value:"Result Set Values",id:"result-set-values",level:2},{value:"Additional Language Support",id:"additional-language-support",level:2},{value:"Conclusion",id:"conclusion",level:2}],d={toc:c};function p(e){var t=e.components,n=(0,o.Z)(e,i);return(0,r.kt)("wrapper",(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"introduction-and-context"},"Introduction and Context"),(0,r.kt)("p",null,'There are many cases where you might want to nest one result set inside of another one.  In order to\ndo this ecomomically there was a great desire to be able to run a parent query and a child query and\nthen link the child rows to the parent rows.  One way to do this is of course to run one query for\neach "child" but then you end up with ',(0,r.kt)("inlineCode",{parentName:"p"},"O(n)")," child queries and if there are sub-children it would be\n",(0,r.kt)("inlineCode",{parentName:"p"},"O(n*m)"),' and so forth. What you really want to do here is something more like a join, only without\nthe cross-product part of the join.  Many systems have such features, sometimes they are called\n"chaptered rowsets" but in any case there is a general need for such a thing.'),(0,r.kt)("p",null,"We did a bunch of work in the name of Parent/Child results sets but like many goals of this kind it\ncaused us to ripen the CQL language in a variety of ways and its interesting to talk about those\nchanges.  Importantly, we wanted to be able to do work of this kind in the language while adding\nthe fewest new notions and basically enabling the language to express a concept like a child rowset\nin the first place."),(0,r.kt)("p",null,"Here are some things that happened along the way that are interesting."),(0,r.kt)("h2",{id:"cursor-types-and-result-types"},"Cursor Types and Result Types"),(0,r.kt)("p",null,"One of the first problems we run into thinking about how a CQL program might express pieces of a rowset\nand turn them into child results is that you need to be able to hash a row, append row data, and\nextract a result set from a key."),(0,r.kt)("p",null,"Let's think about that for just a second: in order to do anything at all with a child rowset,\nno matter how we got such a thing, we have to be able to describe it in a type-safe way.\nThese objects already exist at runtime but they do not appear anywhere in the language explicitly\nand that was going to have to change."),(0,r.kt)("p",null,"To address this we added a new object type, kind of like we did with boxed statements.  A result set\nhas a type that looks like this ",(0,r.kt)("inlineCode",{parentName:"p"},"object <proc_name set>"),".  Here ",(0,r.kt)("inlineCode",{parentName:"p"},"proc_name")," must the the name of a\nprocedure that returns a result set and the object will represent a result set with the\ncorresponding columns in it."),(0,r.kt)("p",null,"That step may seem like it's super important but actually it's kind of optional, it provides type-safety\nbut the initial versions of the feature just used the type ",(0,r.kt)("inlineCode",{parentName:"p"},"object")," which works fine provided you make\nno mistakes... it turns out there are even more fundamental needs that aren't optional."),(0,r.kt)("h2",{id:"creating-new-cursor-types-from-existing-cursor-types"},"Creating New Cursor Types From Existing Cursor Types"),(0,r.kt)("p",null,"The first thing you need to be able to to is take the type of the parent query and add to it one\nmore columns to whole the child result set or sets (note that you can have more than one child\nresult set per parent).  So for instance you might have a list of people, and one child result might\nbe the names of the schools they attended and another is the names of the jobs they worked."),(0,r.kt)("p",null,"So while adding columns to existing rows might sound like a bizarre thing to do but actually it's\nactually fundamental to the job here.  We must be able to create a new output row is that is the\nsames as the parent but includes columns for the the child results too.  There was no good syntax for this.\nThe cursor declaration forms were:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},'/* option 1 */ declare C cursor like shape_name;\n/* option 2 */ declare C cursor like select 1 x, "2" y, false z;\n')),(0,r.kt)("p",null,"The first option implies that you already have a shape from (e.g.) a procedure or table and you want\nto make an identical cursor.  That doesn't work here because we're trying to modify an existing shape,\nnot use it as is."),(0,r.kt)("p",null,"The second form was supposed to be able to create any kind of cursor shape by simply declaring a ",(0,r.kt)("inlineCode",{parentName:"p"},"select"),"\nstatement that is an example of what you want to capture.  In principle this can define almost anything.\nHowever, there's a catch -- you can't get object types to come out of a ",(0,r.kt)("inlineCode",{parentName:"p"},"select")," so it's hopeless for result set types.\nAnd, maybe just as important, you can't just add a few columns to an existing type with any kind of ease,\nyou have to list all columns."),(0,r.kt)("p",null,"Fortunately there was a pretty simple solution to this problem.  There were already lots of cases where\na typed name list happens in the language -- for example in the return type of a function you can\nspecify something like ",(0,r.kt)("inlineCode",{parentName:"p"},"(id integer, name text)"),".  That construction also defines a shape just like a\nselect statement and there was already code to handle all the correctness analysis.  Additionally,\nthe ",(0,r.kt)("inlineCode",{parentName:"p"},"LIKE")," construct can be used in such a list to refer to existing types.  So for instance a function that\nreturns all the columns of tables A and B could be defined like so"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare function foo() (LIKE A, LIKE B);\n")),(0,r.kt)("p",null,"So we could solve all the cursor type problems by allowing a typed name list to be used to define a cursor shape.\nProbably the approach that should have been taken in the first place. The select option seems weird by comparison."),(0,r.kt)("p",null,"With the already existing support for shapes in a type list we could make the result shape for this parent/child case\nwith ease, like so:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare result cursor like (like parent, child_result object<child_proc set>);\n")),(0,r.kt)("p",null,"So, all the parent columns plus a child result set.  Or more than one child result set if needed."),(0,r.kt)("p",null,"Lastly there were going to be cases where we needed to make a new cursor using only some of the field of an existing cursor.\nThe case in particular I'm thinking of is that we might have a big row from the parent and it might\nhave only one or two columns that we need that form the key columns for the child.  We didn't have a good way to do that\neither, but solving this turns out to be simple enough.  We already had this form:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare D cursor like C;\n")),(0,r.kt)("p",null,"we just added:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare D cursor like C(a, b, c);\n")),(0,r.kt)("p",null,"Which chooses just the 3 named fields from ",(0,r.kt)("inlineCode",{parentName:"p"},"C")," and makes a cursor with only those.  Recently we added\nthe form:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare D cursor like C(-x);\n")),(0,r.kt)("p",null,"To mean take all the columns of ",(0,r.kt)("inlineCode",{parentName:"p"},"C")," except ",(0,r.kt)("inlineCode",{parentName:"p"},"x")),(0,r.kt)("p",null,"With the a shape for the key fields defined, we can use existing syntax to load the fields\neconomically:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"fetch D from C(like D);\n")),(0,r.kt)("p",null,"Which says we want to load ",(0,r.kt)("inlineCode",{parentName:"p"},"D")," from the fields of ",(0,r.kt)("inlineCode",{parentName:"p"},"C"),", but using only the columns of ",(0,r.kt)("inlineCode",{parentName:"p"},"D"),".  That operation\nis of course going to be an exact type match by construction.  So now we could describe the key columns from\nchild rows, and the key columns from parent rows.  And we could add columns to the parent type to create space\nto hold child result sets.  All of our type problems are solved.  Almost."),(0,r.kt)("h3",{id:"cursor-arguments"},"Cursor Arguments"),(0,r.kt)("p",null,'It was clear that we would need to be able to do things like "hash a cursor" (any cursor) or "store this row\ninto the appropriate partition" and this requirement meant that we had to be able to write functions that\ncould take any cursor and dynamically do things to it based on its type information.  There is no good way\nto write these generic helper things in CQL, but:'),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"we don't need very many of them,"),(0,r.kt)("li",{parentName:"ul"},"it's pretty easy to do that job in C")),(0,r.kt)("p",null,"The main thing we need is to create a way to declare such functions and call them a with cursor and the necessary shape info."),(0,r.kt)("p",null,"So we added this notion of being able to call an external function with any cursor.  Like so:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare function cursor_hash(C cursor) long not null;\n")),(0,r.kt)("p",null,"you can call it like so:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"let hash := cursor_hash(C);\n")),(0,r.kt)("p",null,"where ",(0,r.kt)("inlineCode",{parentName:"p"},"C")," is any cursor."),(0,r.kt)("p",null,"When such a call is made the C function ",(0,r.kt)("inlineCode",{parentName:"p"},"cursor_hash"),' gets passed what we call a "dynamic cursor".\nThis includes:'),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"a pointer to the data for the cursor"),(0,r.kt)("li",{parentName:"ul"},"the count of fields"),(0,r.kt)("li",{parentName:"ul"},"the names of the fields"),(0,r.kt)("li",{parentName:"ul"},"the type/offset of every field in the cursor")),(0,r.kt)("p",null,"So you can (e.g.) generically do the hash by applying a hash to each field and then combining all of those.\nThis kind of function works on any cursor and all the extra data about the shape that's needed to make the\ncall is static, so really the cost of the call stays modest.  Details of the dynamic cursor type are in\n",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.h")," and there are many example functions now in the ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.c")," file."),(0,r.kt)("p",null,"Again, creating this facility was a pretty minor matter, the compiler already has all this data and uses it\nto create result sets in the first place.  We just allowed other functions to use that same data and\nmade a public type for it."),(0,r.kt)("h2",{id:"the-specific-parentchild-functions"},"The Specific Parent/Child Functions"),(0,r.kt)("p",null,"To do the parent/child operations we needed three helper functions:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"DECLARE FUNC cql_partition_create ()\n   CREATE OBJECT<partitioning> NOT NULL;\n\nDECLARE FUNC cql_partition_cursor (\n  part OBJECT<partitioning> NOT NULL,\n  key CURSOR,\n  value CURSOR)\n    BOOL NOT NULL;\n\nDECLARE FUNC cql_extract_partition (\n  part OBJECT<partitioning> NOT NULL,\n  key CURSOR)\n    CREATE OBJECT NOT NULL;\n")),(0,r.kt)("p",null,"The first function makes a new partitioning."),(0,r.kt)("p",null,"The second function hashes the key columns of a cursor (specified by the key argument) and appends\nthe values provided into a bucket for that key.  By making a pass over the child rows you can easily\ncreate a partitioning with each unique key combo having a buffer of all the matching rows."),(0,r.kt)("p",null,"The third function is used once the partitioning is done.  Given a key again, which you now presumably\nget from the parent rows, you get the buffer you had accumulated and then make a result set out of it\nand return that.  Note that this function returns the vanilla object type because it could be returning\nany shape."),(0,r.kt)("h2",{id:"result-set-sugar"},"Result Set Sugar"),(0,r.kt)("p",null,"With the type system mentioned above you could now join together any kind of complex parent and\nchild combo you needed, but it might be a lot of code, and it's error prone.  This is a good job\nfor a little sugar.  So we added some simple syntax to specify the usual partitioning."),(0,r.kt)("p",null,"It looks like this:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"-- parent and child defined elsewhere\ndeclare proc parent(x integer not null) (id integer not null, a integer, b integer);\ndeclare proc child(y integer not null) (id integer not null, u text, v text);\n\n-- join together parent and child using 'id'\ncreate proc parent_child(x_ integer not null, y_ integer not null)\nbegin\n  out union call parent(x_) join call child(y_) using (id);\nend;\n")),(0,r.kt)("p",null,"The generated code is simple enough, even though there's a good bit of it.\nBut it's a useful exercise to look at it once.  Comments added for clarity."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"CREATE PROC parent_child (x_ INTEGER NOT NULL, y_ INTEGER NOT NULL)\nBEGIN\n  DECLARE __result__0 BOOL NOT NULL;\n\n  -- we need a cursor to hold just the key of the child row\n  DECLARE __key__0 CURSOR LIKE child(id);\n\n  -- we need our partitioning object (there could be more than one per function\n  -- so it gets a number, likewise everything else gets a number\n  LET __partition__0 := cql_partition_create();\n\n  -- we invoke the child and then iterate its rows\n  DECLARE __child_cursor__0 CURSOR FOR CALL child(y_);\n  LOOP FETCH __child_cursor__0\n  BEGIN\n    -- we extract just the key fields (id in this case)\n    FETCH __key__0(id) FROM VALUES(__child_cursor__0.id);\n\n    -- we add this child to the partition using its key\n    SET __result__0 := cql_partition_cursor(__partition__0, __key__0, __child_cursor__0);\n  END;\n\n  -- we need a shape for our result, it is the columns of the parent plus the child rowset\n  DECLARE __out_cursor__0 CURSOR LIKE (id INTEGER NOT NULL, a INTEGER, b INTEGER,\n                                       child1 OBJECT<child SET> NOT NULL);\n\n  -- now we call the parent and iterate it\n  DECLARE __parent__0 CURSOR FOR CALL parent(x_);\n  LOOP FETCH __parent__0\n  BEGIN\n    -- we load the key values out of the parent this time, same key fields\n    FETCH __key__0(id) FROM VALUES(__parent__0.id);\n\n    -- now we create a result row using the parent columns and the child result set\n    FETCH __out_cursor__0(id, a, b, child1) FROM VALUES(__parent__0.id, __parent__0.a, __parent__0.b, cql_extract_partition(__partition__0, __key__0));\n\n    -- and then we emit that row\n    OUT UNION __out_cursor__0;\n  END;\nEND;\n")),(0,r.kt)("p",null,"This code iterates the child once and the parent once and only has two database calls,\none for the child and one for the parent.  And this is enough to create parent/child result\nsets for the most common examples."),(0,r.kt)("h2",{id:"result-set-values"},"Result Set Values"),(0,r.kt)("p",null,"While the above is probably the most common case, another case can happen where you might\nwant to make a procedure call for each parent row to compute the child.  And, more generally,\nthere was no good way to work with result sets from procedure calls other than iterating them\nwith a cursor.  The iteration pattern is very good if the data is coming from a select statement\n-- we don't want to materialize all of the results if we can stream instead.  However, when working\nwith result sets the whole point is to create materialized results for use elsewhere.\nWe now had the power to express a result set type with ",(0,r.kt)("inlineCode",{parentName:"p"},"object<proc_name set>")," but no way to\nactually get such a set from an existing procedure.  Procedures generated them,\nbut they could only be consumed in the C layer."),(0,r.kt)("p",null,"Fortunately this is also an easy problem to solve.  We already supported the ability to use\nprocedures as functions in expressions if they had the right signature.  We now add the ability\nto call a procedure that returns a result set and capture that result.\nPreviously this was not supported and would have produced an error."),(0,r.kt)("p",null,"With the new features you can write:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare child_result object<child set>;\nset child_result := child(args);\n")),(0,r.kt)("p",null,"or better still:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"let child_result := child(args);\n")),(0,r.kt)("p",null,"With this simple change we had the power to write something like this:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"declare proc parent(x integer not null) (id integer not null, a integer, b integer);\ndeclare proc child(id integer not null) (id integer not null, u text, v text);\n\ncreate proc parent_child(x_ integer not null, y_ integer not null)\nbegin\n  -- the result is like the parent with an extra column for the child\n  declare result cursor like (like parent, child object<child set>);\n\n  -- call the parent and loop over the results\n  declare P cursor for call parent(x_);\n  loop fetch P\n  begin\n     -- compute the child for each P and then emit it\n     fetch result from values(from P, child(P.id));\n     out union result;\n  end;\nend;\n")),(0,r.kt)("p",null,"After the sugar is applied this compiles down to this program:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"DECLARE PROC parent (x INTEGER NOT NULL) (id INTEGER NOT NULL, a INTEGER, b INTEGER);\nDECLARE PROC child (id INTEGER NOT NULL) (id INTEGER NOT NULL, u TEXT, v TEXT);\n\nCREATE PROC parent_child (x_ INTEGER NOT NULL, y_ INTEGER NOT NULL)\nBEGIN\n  DECLARE result CURSOR LIKE (id INTEGER NOT NULL, a INTEGER, b INTEGER,\n                              child OBJECT<child SET>);\n\n  DECLARE P CURSOR FOR CALL parent(x_);\n  LOOP FETCH P\n  BEGIN\n    FETCH result(id, a, b, child) FROM VALUES(P.id, P.a, P.b, child(P.id));\n    OUT UNION result;\n  END;\nEND;\n")),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"LIKE")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"FROM")," forms are very powerful but they aren't new.  They do make\nit a lot easier to express this notion of just adding one more column to the result.\nNote that the code for emitting the ",(0,r.kt)("inlineCode",{parentName:"p"},"parent_child")," result before the transformation\ndoesn't need to specify what the columns of the parent are or the columns of the child,\nonly that the parent has at least the ",(0,r.kt)("inlineCode",{parentName:"p"},"id")," column.  Even that could have been removed."),(0,r.kt)("p",null,"This call could have been used instead:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"fetch result from values(from P, child(from P like child arguments));\n")),(0,r.kt)("p",null,"That syntax would result in using the columns of P that match the arguments of ",(0,r.kt)("inlineCode",{parentName:"p"},"child")," -- just\n",(0,r.kt)("inlineCode",{parentName:"p"},"P.id")," in this case.  But if there were 7 such columns the sugar might be easier to understand."),(0,r.kt)("h2",{id:"additional-language-support"},"Additional Language Support"),(0,r.kt)("p",null,"Last, but not least, to make this more accessible we wanted more support in the generated code.\nThe C interface would have produced generic object results for the child result columns.\nThis isn't wrong exactly but it would mean that a cast would be required in every use case on the\nnative side, and it's easy to get the cast wrong.  So the result type of column getters was\nadjusted to be a ",(0,r.kt)("inlineCode",{parentName:"p"},"child_result_set_ref")," instead of just ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_object_ref"),"."),(0,r.kt)("p",null,"Similar transforms were needed if column setters were being emitted (yes that's an option!)\nand of course the Java and Objective C output needed the same transform."),(0,r.kt)("h2",{id:"conclusion"},"Conclusion"),(0,r.kt)("p",null,"The prosecution of native support for parent/child result sets in CQL resulted in a bunch of\nvery useful generalizations for declaring and managing cursors.  The old special case code for\nblobs was actually replaced by these forms.  The language overall expressiveness increased far\nmore than just the ability to do this one kind of join.  It's now possible to write general\npurpose debug helpers for cursors.  It's possible to store and return pre-cooked result sets,\ncreating useful caches and other such combinations.  The type extensions to allow extending\nand narrowing existing types allow even more return flexibility while keeping everything\nstrongly typed."),(0,r.kt)("p",null,"Parent/Child result sets exploit all of these things."))}p.isMDXComponent=!0}}]);
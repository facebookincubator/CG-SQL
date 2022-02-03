(window.webpackJsonp=window.webpackJsonp||[]).push([[24],{156:function(e,t,n){"use strict";n.d(t,"a",(function(){return p})),n.d(t,"b",(function(){return f}));var o=n(0),a=n.n(o);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);t&&(o=o.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,o)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,o,a=function(e,t){if(null==e)return{};var n,o,a={},r=Object.keys(e);for(o=0;o<r.length;o++)n=r[o],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(o=0;o<r.length;o++)n=r[o],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=a.a.createContext({}),u=function(e){var t=a.a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},p=function(e){var t=u(e.components);return a.a.createElement(s.Provider,{value:t},e.children)},b={inlineCode:"code",wrapper:function(e){var t=e.children;return a.a.createElement(a.a.Fragment,{},t)}},m=a.a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,l=e.parentName,s=i(e,["components","mdxType","originalType","parentName"]),p=u(n),m=o,f=p["".concat(l,".").concat(m)]||p[m]||b[m]||r;return n?a.a.createElement(f,c(c({ref:t},s),{},{components:n})):a.a.createElement(f,c({ref:t},s))}));function f(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,l=new Array(r);l[0]=m;var c={};for(var i in t)hasOwnProperty.call(t,i)&&(c[i]=t[i]);c.originalType=e,c.mdxType="string"==typeof e?e:o,l[1]=c;for(var s=2;s<r;s++)l[s]=n[s];return a.a.createElement.apply(null,l)}return a.a.createElement.apply(null,n)}m.displayName="MDXCreateElement"},79:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return c})),n.d(t,"metadata",(function(){return i})),n.d(t,"rightToc",(function(){return s})),n.d(t,"default",(function(){return p}));var o=n(2),a=n(6),r=(n(0),n(156)),l=["components"],c={slug:"columns-sugar",title:"Using the LIKE form in the SELECT statement",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},i={permalink:"/blog/columns-sugar",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2022-02-03-columns-sugar.md",source:"@site/blog/2022-02-03-columns-sugar.md",description:'One of the signature features of the CQL language is the ability to use the "LIKE" form to',date:"2022-02-03T00:00:00.000Z",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],title:"Using the LIKE form in the SELECT statement",readingTime:3.675,truncated:!1,nextItem:{title:"Control Flow Analysis in CQL",permalink:"/blog/flow-analysis"}},s=[],u={rightToc:s};function p(e){var t=e.components,n=Object(a.a)(e,l);return Object(r.b)("wrapper",Object(o.a)({},u,n,{components:t,mdxType:"MDXLayout"}),Object(r.b)("p",null,'One of the signature features of the CQL language is the ability to use the "LIKE" form to\nslice out columns that conform to a shape.  This notion appears in many places in the language.\nFor instance if I have a table ',Object(r.b)("inlineCode",{parentName:"p"},"Foo"),". I can make a cursor for that shape like so:"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre",className:"language-sql"},"declare C cursor like Foo;\n")),Object(r.b)("p",null,"Which says I want the columns of ",Object(r.b)("inlineCode",{parentName:"p"},"C")," to be like the columns of ",Object(r.b)("inlineCode",{parentName:"p"},"Foo"),"."),Object(r.b)("p",null,"If I have a cursor ",Object(r.b)("inlineCode",{parentName:"p"},"D")," that has the ",Object(r.b)("inlineCode",{parentName:"p"},"Foo")," columns but maybe more and maybe in a different order I can load ",Object(r.b)("inlineCode",{parentName:"p"},"C"),"\nas follows:"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre",className:"language-sql"},"fetch C from D(like Foo)\n")),Object(r.b)("p",null,"Which again saves me from having to list all the (potentially dozens) of Foo columns.  This construct is in many places:"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre",className:"language-sql"},"declare proc P(like Foo)\nbegin\n  insert into Foo from arguments;\nend;\n")),Object(r.b)("p",null,"even"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre",className:"language-sql"},"declare proc P(f like Foo, b like Bar)\nbegin\n  insert into Foo from f;\n  insert into Bar from b;\nend;\n")),Object(r.b)("p",null,"And other examples...  This is discussed more fully in\n",Object(r.b)("a",{parentName:"p",href:"https://cgsql.dev/cql-guide/ch05#reshaping-data-cursor-like-forms"},"Chapter 5")," of the Guide."),Object(r.b)("p",null,"However, one of the few places that shapes are interesting but not supported was in the select list.\nAnd so, just a couple of days ago, we added the ",Object(r.b)("inlineCode",{parentName:"p"},"COLUMNS")," construct to the language which allows for\na sugared syntax for extracting columns in bulk.  It's kind of a generalization of the ",Object(r.b)("inlineCode",{parentName:"p"},"select T.*"),"\npattern but with CQL-style slicing and type-checking."),Object(r.b)("p",null,"These forms are supported:"),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"columns from a join table or tables")),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"-- same as A.*\nselect columns(A) from ...;\n\n-- same as A.*, B.*\nselect columns(A, B) from ...;\n")),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"columns from a particular join table that match a shape")),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"-- the columns of A that match the shape Foo\nselect columns(A like Foo) from ...;\n\n-- get the Foo shape from A and the Far shape from B\nselect columns(A like Foo, B like Bar) from ...;\n")),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"columns from any join table that match a shape")),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"--- get the Foo shape from anywhere in the join\nselect columns(like Foo) from ...;\n\n-- get the Foo and Bar shapes, from anywhere in the join\nselect columns(like Foo, like Bar) from ...;\n")),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"specific columns")),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"-- x and y columns plus the foo shape\nselect columns(T1.x, T2.y, like Foo) from ...;\n")),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"distinct columns from the above (not distinct values!)")),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"-- removes duplicate column names\n-- e.g. there will be one copy of 'pk'\nselect columns(distinct A, B) from A join B using(pk);\n\n-- if both Foo and Bar have an (e.g.) 'id' field you only get one copy\nselect columns(distinct like Foo, like Bar) from ...;\n\n-- if a specific column is mentioned it is always included\n-- but later clauses that are not a specific column will avoid it\n-- if F or B has an x it won't appear again, just T.x\nselect columns(T.x, F like Foo, B like Bar) from F, B ..;\n")),Object(r.b)("p",null,"Of course this is all just sugar, so it all ends up being a column list with table\nqualifications -- but the syntax is very powerful.  For instance, for narrowing a\nwide table, or for fusing joins that share common keys"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"-- just the Foo columns\nselect columns(like Foo) from Superset_Of_Foo_From_Many_Joins_Even;\n\n-- only one copy of 'pk'\nselect columns(distinct A,B,C) from\n  A join B using (pk) join C using (pk);\n")),Object(r.b)("p",null,"And of course you can define shapes however you like and then use them\nto slice off column chucks of your choice.  There are many ways to build\nup shapes from other shapes.  Probably the easiest is to declare procedures\nthat return the shape you want and never actual create them.  E.g."),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"declare proc shape1() (x integer, y real, z text);\ndeclare proc shape2() (like shape1, u bool, v bool);\n")),Object(r.b)("p",null,"With this combination you can easily define common column shapes and slice them\nout of complex queries without having to type the columns names over and over..."),Object(r.b)("p",null,"Note that the ",Object(r.b)("inlineCode",{parentName:"p"},"COLUMNS(...)")," form is not a general replacement for the select list.\nFor instance, general expressions are not allowed inside of ",Object(r.b)("inlineCode",{parentName:"p"},"COLUMNS(...)")," but,\nwhere extraction of lots of columns is needed, or even re-ordering of colummns,\nit's a very good option indeed and it composes well with the other ",Object(r.b)("inlineCode",{parentName:"p"},"select")," features."),Object(r.b)("p",null,"This was the last significant area where shapes are useful but totally absent."))}p.isMDXComponent=!0}}]);
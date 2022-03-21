(window.webpackJsonp=window.webpackJsonp||[]).push([[96],{150:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return s})),n.d(t,"metadata",(function(){return i})),n.d(t,"rightToc",(function(){return c})),n.d(t,"default",(function(){return u}));var a=n(2),r=n(6),o=(n(0),n(162)),l=["components"],s={slug:"expression-frags",title:"Introducing Expression Fragments",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},i={permalink:"/blog/expression-frags",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2022-02-19-expression-frags.md",source:"@site/blog/2022-02-19-expression-frags.md",description:"Following on the heels of shared fragments, we're introducing the same kind of thing",date:"2022-02-19T00:00:00.000Z",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],title:"Introducing Expression Fragments",readingTime:4.42,truncated:!1,prevItem:{title:"Using the FROM construct in more places",permalink:"/blog/from-general"},nextItem:{title:"Using the LIKE form in the SELECT statement",permalink:"/blog/columns-sugar"}},c=[],b={rightToc:c};function u(e){var t=e.components,n=Object(r.a)(e,l);return Object(o.b)("wrapper",Object(a.a)({},b,n,{components:t,mdxType:"MDXLayout"}),Object(o.b)("p",null,"Following on the heels of shared fragments, we're introducing the same kind of thing\nfor shared fragments that are expressions rather than tables.  The syntax is as follows:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"-- this isn't very exciting because regular max would do the job\n@attribute(cql:shared_fragment)\ncreate proc max_func(x integer, y integer)\nbegin\n  select case when x >= y then x else y end;\nend;\n")),Object(o.b)("p",null,"The above can be used in the context of a SQL statement like so:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"select max_func(T1.column1, T1.column2) the_max from foo T1;\n")),Object(o.b)("p",null,"The consequence of the above is that the body of ",Object(o.b)("inlineCode",{parentName:"p"},"max_func")," is inlined into the generated SQL.  However, like\nthe other shared fragments, this is done in such a way that the text can be shared between instances so\nyou only pay for the cost of the text","*"," in your program one time, no matter how many time you use it."),Object(o.b)("p",null,"*"," You still pay for the cost of a pointer to the text."),Object(o.b)("p",null,"In particular, for the above, the compiler will generate the following SQL:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"select (\n  select case when x >= y then x else y end\n    from (select T1.column1 x, column2 y))\n")),Object(o.b)("p",null,"But each line will be its own string literal, so, more accurately, it will concatenate the following three strings:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-C"},'"select (",                                      // string1\n" select case when x >= y then x else y end",    // string2\n" from (select T1.column1 x, column2 y))"        // string3\n')),Object(o.b)("p",null,"Importantly, ",Object(o.b)("inlineCode",{parentName:"p"},"string2")," is fixed for any given fragment.  The only thing that changes is ",Object(o.b)("inlineCode",{parentName:"p"},"string3"),", i.e., the arguments.\nThe C compiler, and then the linker, will unify the ",Object(o.b)("inlineCode",{parentName:"p"},"string2")," literal across all translation units so you only\npay for the cost of that text one time.  It also means that the text of the arguments appears exactly one time,\nno matter how complex they are.  For these benefits, we pay the cost of the select wrapper on the arguments.  This\nis cost is frequently negative.  Consider this following:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"select max_func((select max(T.m) from T), (select max(U.m) from U))\n")),Object(o.b)("p",null,"A direct expansion of the above would result in something like this:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"case when (select max(T.m) from T) >= (select max(U.m) from U)\n   then (select max(T.m) from T) \n   else (select max(U.m) from U) \nend;\n")),Object(o.b)("p",null,"The above could be accomplished with a simple pre-processor macro, but the fragments code generates the following:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"select (\n  select case when x >= y then x else y end \n    from select (select max(T.m) from T) x, (select max(U.m) from U) y))\n")),Object(o.b)("p",null,"Expression fragments can nest, so you could write:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"@attribute(cql:shared_fragment)\ncreate proc max3_func(x integer, y integer, z integer)\nbegin\n  select max_func(x, max_func(y, z));\nend;\n")),Object(o.b)("p",null,"Again, this particular example is a waste because regular ",Object(o.b)("inlineCode",{parentName:"p"},"max")," would already do the job."),Object(o.b)("p",null,"To give another example, common mappings from one kind of code to another using case/when can be written\nand shared this way:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"-- this sort of thing happens all the time\n@attribute(cql:shared_fragment)\ncreate proc remap(x integer not null)\nbegin\n   select case x\n     when 1 then 1001\n     when 2 then 1057\n     when 3 then 2010\n     when 4 then 2011\n     else 9999\n   end;\nend;\n")),Object(o.b)("p",null,"In the following:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"select remap(T1.c), remap(T2.d), remap(T3.e) from C, D, E;\n")),Object(o.b)("p",null,"The text for ",Object(o.b)("inlineCode",{parentName:"p"},"remap")," will appear three times in the generated SQL query but only one time in your binary."),Object(o.b)("p",null,"Restrictions:"),Object(o.b)("ul",null,Object(o.b)("li",{parentName:"ul"},"the function must consist of exactly one simple select statement",Object(o.b)("ul",{parentName:"li"},Object(o.b)("li",{parentName:"ul"},"no ",Object(o.b)("inlineCode",{parentName:"li"},"FROM"),", ",Object(o.b)("inlineCode",{parentName:"li"},"WHERE"),", ",Object(o.b)("inlineCode",{parentName:"li"},"HAVING"),", etc. -- the result is an expression"))),Object(o.b)("li",{parentName:"ul"},"the select list must have exactly one value",Object(o.b)("ul",{parentName:"li"},Object(o.b)("li",{parentName:"ul"},"Note: the expression can be a nested ",Object(o.b)("inlineCode",{parentName:"li"},"SELECT")," which could have all the usual ",Object(o.b)("inlineCode",{parentName:"li"},"SELECT")," elements"))),Object(o.b)("li",{parentName:"ul"},"the usual shared fragment rules apply, e.g. no out-parameters, exactly one statement, etc.")),Object(o.b)("p",null,"FAQ:"),Object(o.b)("p",null,"Q: Why does the expression fragment have a ",Object(o.b)("inlineCode",{parentName:"p"},"select")," in it?"),Object(o.b)("p",null,"A: Expression fragments are only interesting in SQL contexts where normal procedure and function calls are not available.\nThe ",Object(o.b)("inlineCode",{parentName:"p"},"select")," keyword makes it clear to the author and the compiler that the expression will be evaluated by\nSQLite and the rules for what is allowed to go in the expression are the SQLite rules."),Object(o.b)("p",null,"Q: Why no ",Object(o.b)("inlineCode",{parentName:"p"},"FROM")," clause?"),Object(o.b)("p",null,"A: We're trying to produce an expression, not a table-value with one column.  If you want a table-value with\none column, the original shared fragments solution already do exactly that.  This gives you a solution for\nsharing code in, say, the ",Object(o.b)("inlineCode",{parentName:"p"},"WHERE")," clause or the select list."),Object(o.b)("p",null,"Q: Isn't this just the same as doing, say, ",Object(o.b)("inlineCode",{parentName:"p"},"#define max_func(x,y) case when (x) >= (y) then x else y end;"),"?"),Object(o.b)("p",null,"A: Macros can give you a ton of flexibility, but they have many problems:"),Object(o.b)("ul",null,Object(o.b)("li",{parentName:"ul"},"if the macro has an error, you see the error in the call site with really bad diagnostic info"),Object(o.b)("li",{parentName:"ul"},"the compiler doesn't know that the sharing is going on so it won't be able to share text between call sites"),Object(o.b)("li",{parentName:"ul"},"the arguments can be evaluated many times each which could be expensive, bloaty, or wrong"),Object(o.b)("li",{parentName:"ul"},"there is no type-checking of arguments to the macro so you may or may not get compilation errors after expansion"),Object(o.b)("li",{parentName:"ul"},"you have to deal with all the usual pre-processor hazards")),Object(o.b)("p",null,"In general, macros ",Object(o.b)("em",{parentName:"p"},"can")," be used for meta-programming (as in C and C++), but that doesn't mean it's a good idea."))}u.isMDXComponent=!0},162:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return h}));var a=n(0),r=n.n(a);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var c=r.a.createContext({}),b=function(e){var t=r.a.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},u=function(e){var t=b(e.components);return r.a.createElement(c.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},p=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,l=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),u=b(n),p=a,h=u["".concat(l,".").concat(p)]||u[p]||m[p]||o;return n?r.a.createElement(h,s(s({ref:t},c),{},{components:n})):r.a.createElement(h,s({ref:t},c))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,l=new Array(o);l[0]=p;var s={};for(var i in t)hasOwnProperty.call(t,i)&&(s[i]=t[i]);s.originalType=e,s.mdxType="string"==typeof e?e:a,l[1]=s;for(var c=2;c<o;c++)l[c]=n[c];return r.a.createElement.apply(null,l)}return r.a.createElement.apply(null,n)}p.displayName="MDXCreateElement"}}]);
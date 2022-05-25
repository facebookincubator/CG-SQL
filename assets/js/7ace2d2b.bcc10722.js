"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[1740],{3905:function(e,t,n){n.d(t,{Zo:function(){return u},kt:function(){return h}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var i=a.createContext({}),c=function(e){var t=a.useContext(i),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=c(e.components);return a.createElement(i.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},p=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),p=c(n),h=r,g=p["".concat(i,".").concat(h)]||p[h]||m[h]||o;return n?a.createElement(g,l(l({ref:t},u),{},{components:n})):a.createElement(g,l({ref:t},u))}));function h(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,l=new Array(o);l[0]=p;var s={};for(var i in t)hasOwnProperty.call(t,i)&&(s[i]=t[i]);s.originalType=e,s.mdxType="string"==typeof e?e:r,l[1]=s;for(var c=2;c<o;c++)l[c]=n[c];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}p.displayName="MDXCreateElement"},4502:function(e,t,n){n.r(t),n.d(t,{assets:function(){return u},contentTitle:function(){return i},default:function(){return h},frontMatter:function(){return s},metadata:function(){return c},toc:function(){return m}});var a=n(7462),r=n(3366),o=(n(7294),n(3905)),l=["components"],s={slug:"expression-frags",title:"Introducing Expression Fragments",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},i=void 0,c={permalink:"/blog/expression-frags",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2022-02-19-expression-frags.md",source:"@site/blog/2022-02-19-expression-frags.md",title:"Introducing Expression Fragments",description:"Following on the heels of shared fragments, we're introducing the same kind of thing",date:"2022-02-19T00:00:00.000Z",formattedDate:"February 19, 2022",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],readingTime:4.42,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"expression-frags",title:"Introducing Expression Fragments",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},prevItem:{title:"Using the FROM construct in more places",permalink:"/blog/from-general"},nextItem:{title:"Using the LIKE form in the SELECT statement",permalink:"/blog/columns-sugar"}},u={authorsImageUrls:[void 0]},m=[],p={toc:m};function h(e){var t=e.components,n=(0,r.Z)(e,l);return(0,o.kt)("wrapper",(0,a.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,"Following on the heels of shared fragments, we're introducing the same kind of thing\nfor shared fragments that are expressions rather than tables.  The syntax is as follows:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"-- this isn't very exciting because regular max would do the job\n@attribute(cql:shared_fragment)\ncreate proc max_func(x integer, y integer)\nbegin\n  select case when x >= y then x else y end;\nend;\n")),(0,o.kt)("p",null,"The above can be used in the context of a SQL statement like so:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"select max_func(T1.column1, T1.column2) the_max from foo T1;\n")),(0,o.kt)("p",null,"The consequence of the above is that the body of ",(0,o.kt)("inlineCode",{parentName:"p"},"max_func")," is inlined into the generated SQL.  However, like\nthe other shared fragments, this is done in such a way that the text can be shared between instances so\nyou only pay for the cost of the text","*"," in your program one time, no matter how many time you use it."),(0,o.kt)("p",null,"*"," You still pay for the cost of a pointer to the text."),(0,o.kt)("p",null,"In particular, for the above, the compiler will generate the following SQL:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"select (\n  select case when x >= y then x else y end\n    from (select T1.column1 x, column2 y))\n")),(0,o.kt)("p",null,"But each line will be its own string literal, so, more accurately, it will concatenate the following three strings:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-c"},'"select (",                                      // string1\n" select case when x >= y then x else y end",    // string2\n" from (select T1.column1 x, column2 y))"        // string3\n')),(0,o.kt)("p",null,"Importantly, ",(0,o.kt)("inlineCode",{parentName:"p"},"string2")," is fixed for any given fragment.  The only thing that changes is ",(0,o.kt)("inlineCode",{parentName:"p"},"string3"),", i.e., the arguments.\nThe C compiler, and then the linker, will unify the ",(0,o.kt)("inlineCode",{parentName:"p"},"string2")," literal across all translation units so you only\npay for the cost of that text one time.  It also means that the text of the arguments appears exactly one time,\nno matter how complex they are.  For these benefits, we pay the cost of the select wrapper on the arguments.  This\nis cost is frequently negative.  Consider this following:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"select max_func((select max(T.m) from T), (select max(U.m) from U))\n")),(0,o.kt)("p",null,"A direct expansion of the above would result in something like this:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"case when (select max(T.m) from T) >= (select max(U.m) from U)\n   then (select max(T.m) from T)\n   else (select max(U.m) from U)\nend;\n")),(0,o.kt)("p",null,"The above could be accomplished with a simple pre-processor macro, but the fragments code generates the following:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"select (\n  select case when x >= y then x else y end\n    from select (select max(T.m) from T) x, (select max(U.m) from U) y))\n")),(0,o.kt)("p",null,"Expression fragments can nest, so you could write:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"@attribute(cql:shared_fragment)\ncreate proc max3_func(x integer, y integer, z integer)\nbegin\n  select max_func(x, max_func(y, z));\nend;\n")),(0,o.kt)("p",null,"Again, this particular example is a waste because regular ",(0,o.kt)("inlineCode",{parentName:"p"},"max")," would already do the job."),(0,o.kt)("p",null,"To give another example, common mappings from one kind of code to another using case/when can be written\nand shared this way:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"-- this sort of thing happens all the time\n@attribute(cql:shared_fragment)\ncreate proc remap(x integer not null)\nbegin\n   select case x\n     when 1 then 1001\n     when 2 then 1057\n     when 3 then 2010\n     when 4 then 2011\n     else 9999\n   end;\nend;\n")),(0,o.kt)("p",null,"In the following:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-sql"},"select remap(T1.c), remap(T2.d), remap(T3.e) from C, D, E;\n")),(0,o.kt)("p",null,"The text for ",(0,o.kt)("inlineCode",{parentName:"p"},"remap")," will appear three times in the generated SQL query but only one time in your binary."),(0,o.kt)("p",null,"Restrictions:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"the function must consist of exactly one simple select statement",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"no ",(0,o.kt)("inlineCode",{parentName:"li"},"FROM"),", ",(0,o.kt)("inlineCode",{parentName:"li"},"WHERE"),", ",(0,o.kt)("inlineCode",{parentName:"li"},"HAVING"),", etc. -- the result is an expression"))),(0,o.kt)("li",{parentName:"ul"},"the select list must have exactly one value",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Note: the expression can be a nested ",(0,o.kt)("inlineCode",{parentName:"li"},"SELECT")," which could have all the usual ",(0,o.kt)("inlineCode",{parentName:"li"},"SELECT")," elements"))),(0,o.kt)("li",{parentName:"ul"},"the usual shared fragment rules apply, e.g. no out-parameters, exactly one statement, etc.")),(0,o.kt)("p",null,"FAQ:"),(0,o.kt)("p",null,"Q: Why does the expression fragment have a ",(0,o.kt)("inlineCode",{parentName:"p"},"select")," in it?"),(0,o.kt)("p",null,"A: Expression fragments are only interesting in SQL contexts where normal procedure and function calls are not available.\nThe ",(0,o.kt)("inlineCode",{parentName:"p"},"select")," keyword makes it clear to the author and the compiler that the expression will be evaluated by\nSQLite and the rules for what is allowed to go in the expression are the SQLite rules."),(0,o.kt)("p",null,"Q: Why no ",(0,o.kt)("inlineCode",{parentName:"p"},"FROM")," clause?"),(0,o.kt)("p",null,"A: We're trying to produce an expression, not a table-value with one column.  If you want a table-value with\none column, the original shared fragments solution already do exactly that.  This gives you a solution for\nsharing code in, say, the ",(0,o.kt)("inlineCode",{parentName:"p"},"WHERE")," clause or the select list."),(0,o.kt)("p",null,"Q: Isn't this just the same as doing, say, ",(0,o.kt)("inlineCode",{parentName:"p"},"#define max_func(x,y) case when (x) >= (y) then x else y end;"),"?"),(0,o.kt)("p",null,"A: Macros can give you a ton of flexibility, but they have many problems:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"if the macro has an error, you see the error in the call site with really bad diagnostic info"),(0,o.kt)("li",{parentName:"ul"},"the compiler doesn't know that the sharing is going on so it won't be able to share text between call sites"),(0,o.kt)("li",{parentName:"ul"},"the arguments can be evaluated many times each which could be expensive, bloaty, or wrong"),(0,o.kt)("li",{parentName:"ul"},"there is no type-checking of arguments to the macro so you may or may not get compilation errors after expansion"),(0,o.kt)("li",{parentName:"ul"},"you have to deal with all the usual pre-processor hazards")),(0,o.kt)("p",null,"In general, macros ",(0,o.kt)("em",{parentName:"p"},"can")," be used for meta-programming (as in C and C++), but that doesn't mean it's a good idea."))}h.isMDXComponent=!0}}]);
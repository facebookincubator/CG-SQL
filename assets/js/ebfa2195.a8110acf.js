"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[1288],{3905:function(e,t,n){n.d(t,{Zo:function(){return s},kt:function(){return d}});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var c=r.createContext({}),u=function(e){var t=r.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},s=function(e){var t=u(e.components);return r.createElement(c.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,c=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),m=u(n),d=a,f=m["".concat(c,".").concat(d)]||m[d]||p[d]||o;return n?r.createElement(f,i(i({ref:t},s),{},{components:n})):r.createElement(f,i({ref:t},s))}));function d(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=m;var l={};for(var c in t)hasOwnProperty.call(t,c)&&(l[c]=t[c]);l.originalType=e,l.mdxType="string"==typeof e?e:a,i[1]=l;for(var u=2;u<o;u++)i[u]=n[u];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},6286:function(e,t,n){n.r(t),n.d(t,{assets:function(){return s},contentTitle:function(){return c},default:function(){return d},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return p}});var r=n(7462),a=n(3366),o=(n(7294),n(3905)),i=["components"],l={slug:"named-types-into",title:"Introducing Named Types",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},c=void 0,u={permalink:"/blog/named-types-into",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2021-01-14-named-types-intro.md",source:"@site/blog/2021-01-14-named-types-intro.md",title:"Introducing Named Types",description:"A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key",date:"2021-01-14T00:00:00.000Z",formattedDate:"January 14, 2021",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],readingTime:1.19,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"named-types-into",title:"Introducing Named Types",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},prevItem:{title:'Introducing Type "Kinds"',permalink:"/blog/type-kinds-intro"},nextItem:{title:"Introducing Virtual Tables",permalink:"/blog/virtual-table-into"}},s={authorsImageUrls:[void 0]},p=[],m={toc:p};function d(e){var t=e.components,n=(0,a.Z)(e,i);return(0,o.kt)("wrapper",(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,"A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key\nfor an entity might need to be ",(0,o.kt)("inlineCode",{parentName:"p"},"LONG")," or even always ",(0,o.kt)("inlineCode",{parentName:"p"},"LONG NOT NULL")," or ",(0,o.kt)("inlineCode",{parentName:"p"},"LONG NOT NULL @SENSITIVE")," and the only\nway to do this in the past was maybe with some ",(0,o.kt)("inlineCode",{parentName:"p"},"#define")," thing.  Otherwise you have to diligently get the type right\nin all the places, and should it ever change, again you have to visit all the places.   To help with this situation,\nand to make code a little more self-describing we add named types to the language.  This is a lot like ",(0,o.kt)("inlineCode",{parentName:"p"},"typedef")," in\nthe C language.  They do not create different incompatible types but do let you name things well."),(0,o.kt)("p",null,"You can now write these sorts of forms:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"declare foo_id type long not null;\n\ncreate table foo(\n  id foo_id primary key autoincrement,\n  name text\n);\n\ncreate proc inserter(name_ text, out id foo_id)\nbegin\n  insert into foo(id, name) values(NULL, name_);\n  set id := last_insert_rowid();\nend;\n")),(0,o.kt)("p",null,"Refer to the ",(0,o.kt)("a",{parentName:"p",href:"https://cgsql.dev/program-diagram#declare_stmt"},"railroad diagram")," for the grammar details."),(0,o.kt)("p",null,"Additionally any enumerated type can be used as a type name.  e.g."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"declare enum thing integer (\n  thing1,\n  thing2\n);\n\ndeclare x thing;\n")),(0,o.kt)("p",null,'Enumerations always get "not null" in addition to their base type.'),(0,o.kt)("p",null,"This isn't a very complex feature but we hope that it will help create clearer code that is less likely to have type-related bugs."))}d.isMDXComponent=!0}}]);
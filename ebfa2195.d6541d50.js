(window.webpackJsonp=window.webpackJsonp||[]).push([[64],{118:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return i})),n.d(t,"metadata",(function(){return c})),n.d(t,"rightToc",(function(){return l})),n.d(t,"default",(function(){return s}));var r=n(2),a=n(6),o=(n(0),n(129)),i={slug:"named-types-into",title:"Introducing Named Types",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},c={permalink:"/blog/named-types-into",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2021-01-14-named-types-intro.md",source:"@site/blog/2021-01-14-named-types-intro.md",description:"A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key",date:"2021-01-14T00:00:00.000Z",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],title:"Introducing Named Types",readingTime:1.19,truncated:!1,prevItem:{title:'Introducing Type "Kinds"',permalink:"/blog/type-kinds-intro"},nextItem:{title:"Introducing Virtual Tables",permalink:"/blog/virtual-table-into"}},l=[],p={rightToc:l};function s(e){var t=e.components,n=Object(a.a)(e,["components"]);return Object(o.b)("wrapper",Object(r.a)({},p,n,{components:t,mdxType:"MDXLayout"}),Object(o.b)("p",null,"A common source of errors in stored procedures is incorrect typing in arguments.  For instance, a particular key\nfor an entity might need to be ",Object(o.b)("inlineCode",{parentName:"p"},"LONG")," or even always ",Object(o.b)("inlineCode",{parentName:"p"},"LONG NOT NULL")," or ",Object(o.b)("inlineCode",{parentName:"p"},"LONG NOT NULL @SENSITIVE")," and the only\nway to do this in the past was maybe with some ",Object(o.b)("inlineCode",{parentName:"p"},"#define")," thing.  Otherwise you have to diligently get the type right\nin all the places, and should it ever change, again you have to visit all the places.   To help with this situation,\nand to make code a little more self-describing we add named types to the language.  This is a lot like ",Object(o.b)("inlineCode",{parentName:"p"},"typedef")," in\nthe C language.  They do not create different incompatible types but do let you name things well."),Object(o.b)("p",null,"You can now write these sorts of forms:"),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare foo_id type long not null;\n\ncreate table foo(\n  id foo_id primary key autoincrement,\n  name text\n);\n\ncreate proc inserter(name_ text, out id foo_id)\nbegin\n  insert into foo(id, name) values(NULL, name_);\n  set id := last_insert_rowid();\nend;\n")),Object(o.b)("p",null,"Refer to the ",Object(o.b)("a",Object(r.a)({parentName:"p"},{href:"https://cgsql.dev/program-diagram#declare_stmt"}),"railroad diagram")," for the grammar details."),Object(o.b)("p",null,"Additionally any enumerated type can be used as a type name.  e.g."),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare enum thing integer (\n  thing1,\n  thing2\n);\n\ndeclare x thing;\n")),Object(o.b)("p",null,'Enumerations always get "not null" in addition to their base type.'),Object(o.b)("p",null,"This isn't a very complex feature but we hope that it will help create clearer code that is less likely to have type-related bugs."))}s.isMDXComponent=!0},129:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return m}));var r=n(0),a=n.n(r);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=a.a.createContext({}),s=function(e){var t=a.a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},u=function(e){var t=s(e.components);return a.a.createElement(p.Provider,{value:t},e.children)},b={inlineCode:"code",wrapper:function(e){var t=e.children;return a.a.createElement(a.a.Fragment,{},t)}},d=a.a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),u=s(n),d=r,m=u["".concat(i,".").concat(d)]||u[d]||b[d]||o;return n?a.a.createElement(m,c(c({ref:t},p),{},{components:n})):a.a.createElement(m,c({ref:t},p))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=d;var c={};for(var l in t)hasOwnProperty.call(t,l)&&(c[l]=t[l]);c.originalType=e,c.mdxType="string"==typeof e?e:r,i[1]=c;for(var p=2;p<o;p++)i[p]=n[p];return a.a.createElement.apply(null,i)}return a.a.createElement.apply(null,n)}d.displayName="MDXCreateElement"}}]);
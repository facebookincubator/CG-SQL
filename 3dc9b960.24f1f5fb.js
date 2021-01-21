(window.webpackJsonp=window.webpackJsonp||[]).push([[20],{129:function(e,t,n){"use strict";n.d(t,"a",(function(){return b})),n.d(t,"b",(function(){return m}));var r=n(0),a=n.n(r);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var l=a.a.createContext({}),p=function(e){var t=a.a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},b=function(e){var t=p(e.components);return a.a.createElement(l.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.a.createElement(a.a.Fragment,{},t)}},d=a.a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,l=s(e,["components","mdxType","originalType","parentName"]),b=p(n),d=r,m=b["".concat(i,".").concat(d)]||b[d]||u[d]||o;return n?a.a.createElement(m,c(c({ref:t},l),{},{components:n})):a.a.createElement(m,c({ref:t},l))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=d;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c.mdxType="string"==typeof e?e:r,i[1]=c;for(var l=2;l<o;l++)i[l]=n[l];return a.a.createElement.apply(null,i)}return a.a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},75:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return i})),n.d(t,"metadata",(function(){return c})),n.d(t,"rightToc",(function(){return s})),n.d(t,"default",(function(){return p}));var r=n(2),a=n(6),o=(n(0),n(129)),i={slug:"type-kinds-intro",title:'Introducing Type "Kinds"',author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},c={permalink:"/blog/type-kinds-intro",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2021-01-20-type-kinds-intro.md",source:"@site/blog/2021-01-20-type-kinds-intro.md",description:'Further adding to the type calculus of the CQL language we introduced the ability to encode the "kind" of primtive types.  This',date:"2021-01-20T00:00:00.000Z",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],title:'Introducing Type "Kinds"',readingTime:2.255,truncated:!1,nextItem:{title:"Introducing Named Types",permalink:"/blog/named-types-into"}},s=[],l={rightToc:s};function p(e){var t=e.components,n=Object(a.a)(e,["components"]);return Object(o.b)("wrapper",Object(r.a)({},l,n,{components:t,mdxType:"MDXLayout"}),Object(o.b)("p",null,'Further adding to the type calculus of the CQL language we introduced the ability to encode the "kind" of primtive types.  This\ncan be used in a number of ways, like "units" for natural things and like a "type" for synthetic keys and other such.  It\'s easier\nto illustrate by example.'),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare job_id type long<job_id>;\ndeclare person_id type long<person_id>;\n\ndeclare j job_id;\ndecalre p person_id;\n\nset p := j;  -- this is an error\n")),Object(o.b)("p",null,"in face other expressions like  ",Object(o.b)("inlineCode",{parentName:"p"},"p == j")," would also produce errors as these ",Object(o.b)("inlineCode",{parentName:"p"},"long"),' values are no longer type compatible.  This is\na great way to add enforcement to your schema.  Likewise you can use this to add "units" to your data types.  e.g.'),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare meters type real<meters>;\ndeclare grams type real<meters>;\n")),Object(o.b)("p",null,"Variables of type ",Object(o.b)("inlineCode",{parentName:"p"},"grams")," are not compatible with variables of type ",Object(o.b)("inlineCode",{parentName:"p"},"meters")," even though both are ",Object(o.b)("inlineCode",{parentName:"p"},"real"),"."),Object(o.b)("p",null,"Likewise attemping to insert ",Object(o.b)("inlineCode",{parentName:"p"},"grams")," into a column that is typed to ",Object(o.b)("inlineCode",{parentName:"p"},"meters")," will give errors.  Of course SQLite doesn't know about any of this so all the ",Object(o.b)("inlineCode",{parentName:"p"},"<>")," stuff is\nremoved in the generated SQL.  This is just about type enforcement at compile time."),Object(o.b)("p",null,"Enumerations like:"),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare enum surface integer (paper, canvas);\ndeclare enum writer integer (pen, paper, brush);\n")),Object(o.b)("p",null,"enables this:"),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"declare s surface;                  -- s is now of type integer<surface>\ndeclare w writer;                   -- w is now of type integer<writer>\nset s := surface.paper;             -- ok\nset s := writer.pen;                -- error\nset w := writer.pencil;             -- ok\ncase when s == w then 1 else 0 end; -- error (w/s not comparable)\nset w := s;                         -- error again\n")),Object(o.b)("p",null,"additionally in the database:"),Object(o.b)("pre",null,Object(o.b)("code",Object(r.a)({parentName:"pre"},{}),"create table draw_action(\n  w writer,\n  s surface\n);\n\ninsert into draw_action values(w, s); -- ok\ninsert into draw_action values(s, w); -- error!\n")),Object(o.b)("p",null,"So the types can be quite helpful when dealing with loose variables."),Object(o.b)("p",null,"The notion of specific types was added to the language nearly two years ago to support the ",Object(o.b)("inlineCode",{parentName:"p"},"object")," type because there was a great desire\nto prevent ",Object(o.b)("inlineCode",{parentName:"p"},"object<dictionary>")," being assigned from ",Object(o.b)("inlineCode",{parentName:"p"},"object<list>"),' but this type kind, whether it\'s with units (e.g. "meters", "grams")\nor a type name (e.g. "job_id") adds a lot of high valued type checking.'),Object(o.b)("p",null,"The kind can be added, stripped, or changed with a ",Object(o.b)("inlineCode",{parentName:"p"},"cast"),' operation and the type system allows a constant or varable with no kind (e.g. "1")\nto mix and match with anything.  So you get the most value by using the specific type consistently but you won\'t go insane adding test cases\nthat use constants for instance.'),Object(o.b)("p",null,"As of this writing the expression kinds are checked for compatibility everywhere except for the ",Object(o.b)("inlineCode",{parentName:"p"},"update")," statement which should join the fun tomorrow."))}p.isMDXComponent=!0}}]);
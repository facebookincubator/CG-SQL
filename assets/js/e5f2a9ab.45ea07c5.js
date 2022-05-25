"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[3992],{3905:function(e,t,r){r.d(t,{Zo:function(){return s},kt:function(){return p}});var n=r(7294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function c(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var l=n.createContext({}),u=function(e){var t=n.useContext(l),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},s=function(e){var t=u(e.components);return n.createElement(l.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},f=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,l=e.parentName,s=c(e,["components","mdxType","originalType","parentName"]),f=u(r),p=o,b=f["".concat(l,".").concat(p)]||f[p]||m[p]||a;return r?n.createElement(b,i(i({ref:t},s),{},{components:r})):n.createElement(b,i({ref:t},s))}));function p(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,i=new Array(a);i[0]=f;var c={};for(var l in t)hasOwnProperty.call(t,l)&&(c[l]=t[l]);c.originalType=e,c.mdxType="string"==typeof e?e:o,i[1]=c;for(var u=2;u<a;u++)i[u]=r[u];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}f.displayName="MDXCreateElement"},8898:function(e,t,r){r.r(t),r.d(t,{assets:function(){return s},contentTitle:function(){return l},default:function(){return p},frontMatter:function(){return c},metadata:function(){return u},toc:function(){return m}});var n=r(7462),o=r(3366),a=(r(7294),r(3905)),i=["components"],c={slug:"result-variable",title:"Introducing @RC builtin variable",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},l=void 0,u={permalink:"/blog/result-variable",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2021-02-21-result-variable.md",source:"@site/blog/2021-02-21-result-variable.md",title:"Introducing @RC builtin variable",description:"We've long needed a way to see the most recent SQLite result code SQLite in the context",date:"2021-02-21T00:00:00.000Z",formattedDate:"February 21, 2021",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],readingTime:.72,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"result-variable",title:"Introducing @RC builtin variable",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},prevItem:{title:"Introducing Shared Fragments",permalink:"/blog/shared-fragments-intro"},nextItem:{title:"Introducing Select .. If Nothing",permalink:"/blog/select-if-nothing"}},s={authorsImageUrls:[void 0]},m=[],f={toc:m};function p(e){var t=e.components,r=(0,o.Z)(e,i);return(0,a.kt)("wrapper",(0,n.Z)({},f,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"We've long needed a way to see the most recent SQLite result code SQLite in the context\nof say a ",(0,a.kt)("inlineCode",{parentName:"p"},"catch")," block (most other times you can assume SQLITE_OK was the last\nresult code otherwise control flow would transfer elsewhere. Sometimes SQLITE_ROW\nor SQLITE_DONE might be the current result code."),(0,a.kt)("p",null,"Soon we'll provide a sample header that declares the most common error codes in an enum but\nfor now you could do something like this:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-sql"},"-- pasted from the sqlite.c\n#define SQLITE_BUSY         5   /* The database file is locked */\n\n-- this is a contrived example\ncreate proc get_first_foo(out can_retry bool not null)\nbegin\n\n  -- can_retry is set to 0 automatically, language semantics guarantee this\n\n  begin try\n    select foo from bar limit 1;\n  end try;\n  begin catch\n    set can_retry := (@rc == SQLITE_BUSY);\n    throw; -- rethrow the original error\n  end catch;\nend;\n")))}p.isMDXComponent=!0}}]);
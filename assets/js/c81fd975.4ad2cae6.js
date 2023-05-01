"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9310],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return s},MDXProvider:function(){return m},mdx:function(){return b},useMDXComponents:function(){return p},withMDXComponents:function(){return l}});var r=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(){return i=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},i.apply(this,arguments)}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function u(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=r.createContext({}),l=function(e){return function(t){var n=p(t.components);return r.createElement(e,i({},t,{components:n}))}},p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},m=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},f={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,i=e.originalType,a=e.parentName,s=u(e,["components","mdxType","originalType","parentName"]),l=p(n),m=o,d=l["".concat(a,".").concat(m)]||l[m]||f[m]||i;return n?r.createElement(d,c(c({ref:t},s),{},{components:n})):r.createElement(d,c({ref:t},s))}));function b(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var i=n.length,a=new Array(i);a[0]=d;var c={};for(var u in t)hasOwnProperty.call(t,u)&&(c[u]=t[u]);c.originalType=e,c.mdxType="string"==typeof e?e:o,a[1]=c;for(var s=2;s<i;s++)a[s]=n[s];return r.createElement.apply(null,a)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},61363:function(e,t,n){n.r(t),n.d(t,{assets:function(){return l},contentTitle:function(){return u},default:function(){return f},frontMatter:function(){return c},metadata:function(){return s},toc:function(){return p}});var r=n(83117),o=n(80102),i=(n(67294),n(3905)),a=["components"],c={id:"testing",title:"Testing CG/SQL",sidebar_label:"Testing CG/SQL"},u=void 0,s={unversionedId:"testing",id:"testing",title:"Testing CG/SQL",description:"Run this command in the /sources directory:",source:"@site/../docs/testing.md",sourceDirName:".",slug:"/testing",permalink:"/docs/testing",draft:!1,tags:[],version:"current",frontMatter:{id:"testing",title:"Testing CG/SQL",sidebar_label:"Testing CG/SQL"},sidebar:"someSidebar",previous:{title:"Code Coverage CG/SQL",permalink:"/docs/code-coverage"}},l={},p=[],m={toc:p};function f(e){var t=e.components,n=(0,o.Z)(e,a);return(0,i.mdx)("wrapper",(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,i.mdx)("p",null,"Run this command in the ",(0,i.mdx)("a",{parentName:"p",href:"https://github.com/facebookincubator/CG-SQL/tree/main/sources"},"/sources")," directory:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"./test.sh\n")),(0,i.mdx)("p",null,"This will build and run the test suite"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"./test.sh --use_amalgam\n")),(0,i.mdx)("p",null,"Does the same thing but it tests the built amalgam rather than the normal build"),(0,i.mdx)("blockquote",null,(0,i.mdx)("p",{parentName:"blockquote"},"See details in our ",(0,i.mdx)("a",{parentName:"p",href:"/cql-guide/int04#testing"},"CQL Internals documentation"),".")))}f.isMDXComponent=!0}}]);
"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5317],{3905:function(e,t,n){n.d(t,{Zo:function(){return p},kt:function(){return m}});var a=n(7294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=a.createContext({}),u=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},p=function(e){var t=u(e.components);return a.createElement(s.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,s=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),d=u(n),m=o,f=d["".concat(s,".").concat(m)]||d[m]||c[m]||r;return n?a.createElement(f,i(i({ref:t},p),{},{components:n})):a.createElement(f,i({ref:t},p))}));function m(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,i=new Array(r);i[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:o,i[1]=l;for(var u=2;u<r;u++)i[u]=n[u];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},1701:function(e,t,n){n.r(t),n.d(t,{assets:function(){return p},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return c}});var a=n(7462),o=n(3366),r=(n(7294),n(3905)),i=["components"],l={id:"dev-notes",title:"Developer Notes on CQL Development",sidebar_label:"Developer Notes"},s=void 0,u={unversionedId:"dev-notes",id:"dev-notes",title:"Developer Notes on CQL Development",description:"0. We have extensive documentation at CQL Internals.",source:"@site/../docs/dev_notes.md",sourceDirName:".",slug:"/dev-notes",permalink:"/docs/dev-notes",draft:!1,tags:[],version:"current",frontMatter:{id:"dev-notes",title:"Developer Notes on CQL Development",sidebar_label:"Developer Notes"},sidebar:"someSidebar",previous:{title:"Playground",permalink:"/docs/playground"},next:{title:"Code Coverage CG/SQL",permalink:"/docs/code-coverage"}},p={},c=[],d={toc:c};function m(e){var t=e.components,n=(0,o.Z)(e,i);return(0,r.kt)("wrapper",(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("ol",{start:0},(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"We have extensive documentation at ",(0,r.kt)("a",{parentName:"p",href:"/cql-guide/int01"},"CQL Internals"),".")),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"If you aren't good with ",(0,r.kt)("inlineCode",{parentName:"p"},"yacc"),"/",(0,r.kt)("inlineCode",{parentName:"p"},"lex")," you probably should do some homework before you start. CQL development is all about building and walking a syntax tree.  It's possible to make local changes without knowing the details but it can be hard to figure out where to make changes without context.")),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"CQL development is basically test driven, to create a new feature:"),(0,r.kt)("ol",{parentName:"li"},(0,r.kt)("li",{parentName:"ol"},"Add the language feature to ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sql")),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh"),"; it will fail due to parse error"),(0,r.kt)("li",{parentName:"ol"},"Add the syntax to ",(0,r.kt)("inlineCode",{parentName:"li"},"cql.y")," and create the necessary tree pieces in ",(0,r.kt)("inlineCode",{parentName:"li"},"ast.h")),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh"),"; accept any file differences to install this as the new reference baseline."),(0,r.kt)("li",{parentName:"ol"},"Add a test case to ",(0,r.kt)("inlineCode",{parentName:"li"},"sem_test.sql")," that uses your new feature. ",(0,r.kt)("inlineCode",{parentName:"li"},"sem_test.sql")," can contain pattern matching for the semantic output."),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh"),"; it will fail because it will find an AST node it doesn't understand"),(0,r.kt)("li",{parentName:"ol"},"edit ",(0,r.kt)("inlineCode",{parentName:"li"},"sem.c")," to do the analysis for your new node type"),(0,r.kt)("li",{parentName:"ol"},"adjust the verification in ",(0,r.kt)("inlineCode",{parentName:"li"},"sem_test.sql")," accordingly"),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh")," until it passes making fixes as needed"),(0,r.kt)("li",{parentName:"ol"},"there will be new diff output and it will be spitting out the diffs; if you are happy with the new output, accept the diffs to update the reference outputs; note the pattern matching validations will still fail if the output goes bad even if the reference comparison is good, the reference output is a double check"),(0,r.kt)("li",{parentName:"ol"},"add code that uses your new feature to ",(0,r.kt)("inlineCode",{parentName:"li"},"cg_test.sql"),", this is the code gen test, verifications using pattern matching are also allowed there"),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh")),(0,r.kt)("li",{parentName:"ol"},"it will fail because codegen doesn't know about your new feature"),(0,r.kt)("li",{parentName:"ol"},"edit ",(0,r.kt)("inlineCode",{parentName:"li"},"cg_c.c")," (or a different code gen if you're doing test helpers or some such) to support your new code"),(0,r.kt)("li",{parentName:"ol"},"cycle running ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh")," until it passes"),(0,r.kt)("li",{parentName:"ol"},"accept each diff when you're happy with the new output"),(0,r.kt)("li",{parentName:"ol"},"Add code that runs your new feature using run_test.sql"),(0,r.kt)("li",{parentName:"ol"},"Run ",(0,r.kt)("inlineCode",{parentName:"li"},"test.sh"),", if your codegen was perfect it could pass; it probably won't at first"),(0,r.kt)("li",{parentName:"ol"},"fix your code until it's done; you shouldn't need to accept any more diffs at this point"),(0,r.kt)("li",{parentName:"ol"},"run ",(0,r.kt)("inlineCode",{parentName:"li"},"cov.sh")," to confirm 100% coverage"),(0,r.kt)("li",{parentName:"ol"},"sanity check the GCC build (I use a linux box for this)"))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"Get a solid code review and land as usual."))),(0,r.kt)("p",null,"By the time you have done this you will have passed the tests dozens of times and you will know exactly what your code is doing to the entire battery of cql combinations.  Missing tests can be painful and cause downstream regressions so be ruthless about adding enough combinations and validating the essential parts.  The snapshot diffing is helpful but the real gating is done by the pattern matching logic."),(0,r.kt)("p",null,"Note: none of this works unless you are standing the main source directory"),(0,r.kt)("p",null,"Note: the test scripts make a lot of turds, at this point almost everything should be going into the ",(0,r.kt)("inlineCode",{parentName:"p"},"out"),"\ndirectory but it wasn't always so.  You can use ",(0,r.kt)("inlineCode",{parentName:"p"},"make clean")," to get rid of the build stuff wherever it may be.\nAlternatively use source control to get rid of any junk."))}m.isMDXComponent=!0}}]);
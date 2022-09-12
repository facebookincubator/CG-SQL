"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[2395],{3905:function(e,t,n){n.d(t,{Zo:function(){return u},kt:function(){return m}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),d=p(n),m=r,h=d["".concat(s,".").concat(m)]||d[m]||c[m]||i;return n?a.createElement(h,l(l({ref:t},u),{},{components:n})):a.createElement(h,l({ref:t},u))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,l=new Array(i);l[0]=d;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var p=2;p<i;p++)l[p]=n[p];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},7854:function(e,t,n){n.r(t),n.d(t,{assets:function(){return u},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return o},metadata:function(){return p},toc:function(){return c}});var a=n(7462),r=n(3366),i=(n(7294),n(3905)),l=["components"],o={id:"ch15",title:"Chapter 15: Query Plan Generation",sidebar_label:"Chapter 15: Query Plan Generation"},s=void 0,p={unversionedId:"ch15",id:"ch15",title:"Chapter 15: Query Plan Generation",description:"CQL offers a way to run SQLite's EXPLAIN QUERY PLAN command for all the SQL statements used in a CQL file using a set of special compilation steps.",source:"@site/../CQL_Guide/ch15.md",sourceDirName:".",slug:"/ch15",permalink:"/cql-guide/ch15",draft:!1,tags:[],version:"current",lastUpdatedBy:"Raoul Foaleng",lastUpdatedAt:1663006346,formattedLastUpdatedAt:"9/12/2022",frontMatter:{id:"ch15",title:"Chapter 15: Query Plan Generation",sidebar_label:"Chapter 15: Query Plan Generation"},sidebar:"someSidebar",previous:{title:"Chapter 14: CQL Query Fragments",permalink:"/cql-guide/ch14"},next:{title:"Appendix 1: Command Line Options",permalink:"/cql-guide/x1"}},u={},c=[{value:"Query Plan Generation Compilation Steps",id:"query-plan-generation-compilation-steps",level:2},{value:"Special Handling of CQL features in Query Plan Generation",id:"special-handling-of-cql-features-in-query-plan-generation",level:3},{value:"Variables",id:"variables",level:4},{value:"User Defined Functions",id:"user-defined-functions",level:4},{value:"Conditionals in Shared Fragments",id:"conditionals-in-shared-fragments",level:4}],d={toc:c};function m(e){var t=e.components,n=(0,r.Z)(e,l);return(0,i.kt)("wrapper",(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"CQL offers a way to run SQLite's ",(0,i.kt)("a",{parentName:"p",href:"https://www.sqlite.org/eqp.html"},(0,i.kt)("inlineCode",{parentName:"a"},"EXPLAIN QUERY PLAN")," command")," for all the SQL statements used in a CQL file using a set of special compilation steps."),(0,i.kt)("p",null,"Generating query plans is inherently complicated. Any given stored procedure might include many SQL statements, each of which has a plan. To get the plans, those statements must be executed in the appropriate mode. In order for them to execute, whatever tables, views, and user-defined functions they use must exist. The statements can have any number of parameters, those have to be swapped out because they might come from anywhere. When run in ",(0,i.kt)("inlineCode",{parentName:"p"},"--rt query_plan")," mode, the compiler accomplishes all of this by analyzing the original code and creating entirely new code. Running this new code creates the schema and, with the appropriate transforms, runs all the SQL statements in the original to give us the query plans. The process requires many steps as we'll see below."),(0,i.kt)("h2",{id:"query-plan-generation-compilation-steps"},"Query Plan Generation Compilation Steps"),(0,i.kt)("div",{className:"admonition admonition-tip alert alert--success"},(0,i.kt)("div",{parentName:"div",className:"admonition-heading"},(0,i.kt)("h5",{parentName:"div"},(0,i.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,i.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"},(0,i.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M6.5 0C3.48 0 1 2.19 1 5c0 .92.55 2.25 1 3 1.34 2.25 1.78 2.78 2 4v1h5v-1c.22-1.22.66-1.75 2-4 .45-.75 1-2.08 1-3 0-2.81-2.48-5-5.5-5zm3.64 7.48c-.25.44-.47.8-.67 1.11-.86 1.41-1.25 2.06-1.45 3.23-.02.05-.02.11-.02.17H5c0-.06 0-.13-.02-.17-.2-1.17-.59-1.83-1.45-3.23-.2-.31-.42-.67-.67-1.11C2.44 6.78 2 5.65 2 5c0-2.2 2.02-4 4.5-4 1.22 0 2.36.42 3.22 1.19C10.55 2.94 11 3.94 11 5c0 .66-.44 1.78-.86 2.48zM4 14h5c-.23 1.14-1.3 2-2.5 2s-2.27-.86-2.5-2z"}))),"tip")),(0,i.kt)("div",{parentName:"div",className:"admonition-content"},(0,i.kt)("p",{parentName:"div"},"The following steps are used in ",(0,i.kt)("inlineCode",{parentName:"p"},"./repl/go_query_plan.sh"),", you can ",(0,i.kt)("a",{parentName:"p",href:"/docs/playground#query-plan-playground"},"run it to get a quick demonstration of this feature in action"),". The rest of the section explains how query plan generation works and some of its quirks."))),(0,i.kt)("p",null,"To execute query plans for a given CQL file, the following commands need to be run:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-bash"},"CQL_FILE= # The CQL file to compile\nCQL_ROOT_DIR= # Path to cql directory\nCQL=$CQL_ROOT_DIR/out/cql\n\n# Generate Query Plan Script\n$CQL --in $CQL_FILE --rt query_plan --cg go-qp.sql\n\n# Generate UDF stubs\n$CQL --in $CQL_FILE --rt udf --cg go-qp-udf.h go-qp-udf.c\n\n# Compile and link CQL artifacts, with a main C file query_plan_test.c\n$CQL --in go-qp.sql --cg go-qp.h go-qp.c --dev\ncc -I$CQL_ROOT_DIR -I. -c $CQL_ROOT_DIR/query_plan_test.c go-qp.c go-qp-udf.c\ncc -I$CQL_ROOT_DIR -I. -O -o go_query_plan go-qp.o go-qp-udf.o query_plan_test.o $CQL_ROOT_DIR/cqlrt.c -lsqlite3\n\n# Run and generate query plans\n./go_query_plan\n")),(0,i.kt)("p",null,"Contrary to what one might expect, rather than providing query plans directly, CQL uses ",(0,i.kt)("inlineCode",{parentName:"p"},"--rt query_plan")," to generate a second CQL script that returns query plans for each SQL statement used."),(0,i.kt)("p",null,"A separate command, ",(0,i.kt)("inlineCode",{parentName:"p"},"--rt udf")," is required to generate any stubbed ",(0,i.kt)("a",{parentName:"p",href:"/cql-guide/ch08"},"user defined functions"),' that are used in the original script. Afterwards, the generated query plan script, udf stubs needs to compiled like any CQL file and run by a "main" function that needs to be created separately.'),(0,i.kt)("p",null,"The CQL repository provides the file ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/facebookincubator/CG-SQL/blob/main/sources/query_plan_test.c"},(0,i.kt)("inlineCode",{parentName:"a"},"query_plan_test.c")),' that can be used as the "main" function, otherwise you can make your own.'),(0,i.kt)("p",null,"::note\nWhen compiling the CQL file generated by ",(0,i.kt)("inlineCode",{parentName:"p"},"--rt query_plan"),", the ",(0,i.kt)("inlineCode",{parentName:"p"},"--dev")," flag is required.\n:::"),(0,i.kt)("h3",{id:"special-handling-of-cql-features-in-query-plan-generation"},"Special Handling of CQL features in Query Plan Generation"),(0,i.kt)("p",null,"CQL's query planner generator modifies the usage of the following features to allow SQLite run ",(0,i.kt)("inlineCode",{parentName:"p"},"EXPLAIN QUERY PLAN")," successfully:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Variables"),(0,i.kt)("li",{parentName:"ul"},"User Defined Functions"),(0,i.kt)("li",{parentName:"ul"},"Conditionals in Shared Fragments")),(0,i.kt)("div",{className:"admonition admonition-caution alert alert--warning"},(0,i.kt)("div",{parentName:"div",className:"admonition-heading"},(0,i.kt)("h5",{parentName:"div"},(0,i.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,i.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"16",height:"16",viewBox:"0 0 16 16"},(0,i.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M8.893 1.5c-.183-.31-.52-.5-.887-.5s-.703.19-.886.5L.138 13.499a.98.98 0 0 0 0 1.001c.193.31.53.501.886.501h13.964c.367 0 .704-.19.877-.5a1.03 1.03 0 0 0 .01-1.002L8.893 1.5zm.133 11.497H6.987v-2.003h2.039v2.003zm0-3.004H6.987V5.987h2.039v4.006z"}))),"caution")),(0,i.kt)("div",{parentName:"div",className:"admonition-content"},(0,i.kt)("p",{parentName:"div"},"Generating query plans of CQL files that use table valued functions, or ",(0,i.kt)("a",{parentName:"p",href:"https://sqlite.org/vtab.html#:~:text=2.-,Table%2Dvalued%20functions,columns%20of%20the%20virtual%20table."},"virtual tables")," is not supported."))),(0,i.kt)("h4",{id:"variables"},"Variables"),(0,i.kt)("p",null,"Variables used in SQL statements are stubbed into constants. The exact value varies depending on the type of the variable, but it is always equivalent to some form of ",(0,i.kt)("inlineCode",{parentName:"p"},'"1"'),"."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql",metastring:'title="original.sql"',title:'"original.sql"'},"...\nSELECT *\nFROM my_table\nWHERE id = x;\n...\n")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql",metastring:'title="query_plan.sql"',title:'"query_plan.sql"'},"...\nEXPLAIN QUERY PLAN\nSELECT *\nFROM my_table\nWHERE my_table.id = nullable(1);\n...\n")),(0,i.kt)("h4",{id:"user-defined-functions"},"User Defined Functions"),(0,i.kt)("p",null,(0,i.kt)("em",{parentName:"p"},"Read ",(0,i.kt)("a",{parentName:"em",href:"/cql-guide/ch08"},"Functions")," on details about Function Types.")),(0,i.kt)("p",null,"Since the implementation of UDFs in a CQL file do not affect SQLite query plans, CQL's query plan script expects stubs generated by ",(0,i.kt)("inlineCode",{parentName:"p"},"cql --rt udf")," to be used instead."),(0,i.kt)("h4",{id:"conditionals-in-shared-fragments"},"Conditionals in Shared Fragments"),(0,i.kt)("p",null,(0,i.kt)("em",{parentName:"p"},"Read ",(0,i.kt)("a",{parentName:"em",href:"/cql-guide/ch14"},"CQL Query Fragments")," on details about shared fragments")),(0,i.kt)("p",null,"Only one branch of a conditional is chosen for query plan analysis. By default this will be the first branch, which is the initial ",(0,i.kt)("inlineCode",{parentName:"p"},"SELECT")," statement following the ",(0,i.kt)("inlineCode",{parentName:"p"},"IF")," conditional.\nThe branch to analyze can be configured with the ",(0,i.kt)("inlineCode",{parentName:"p"},"cql:query_plan_branch")," ",(0,i.kt)("a",{parentName:"p",href:"/cql-guide/x3"},"@attribute"),"."),(0,i.kt)("p",null,"Here's an example of ",(0,i.kt)("inlineCode",{parentName:"p"},"cql:query_plan_branch")," being used:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql",metastring:'title="original.sql"',title:'"original.sql"'},"@attribute(cql:shared_fragment)\n@attribute(cql:query_plan_branch=1)\nCREATE PROC frag2(y int)\nBEGIN\n  IF y == 2 THEN\n    SELECT 10 b;\n  ELSE IF y == -1 THEN\n    SELECT 20 b;\n  ELSE\n    SELECT 30 b;\n  END IF;\nEND;\n")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-sql",metastring:'title="query_plan.sql"',title:'"query_plan.sql"'},"EXPLAIN QUERY PLAN\nSELECT 20 b;\n")),(0,i.kt)("p",null,"Setting ",(0,i.kt)("inlineCode",{parentName:"p"},"cql:query_plan_branch=1")," selects the second branch. Providing ",(0,i.kt)("inlineCode",{parentName:"p"},"cql:query_plan_branch=2")," instead would yield the ",(0,i.kt)("inlineCode",{parentName:"p"},"ELSE")," clause ",(0,i.kt)("inlineCode",{parentName:"p"},"SELECT 30 b"),". ",(0,i.kt)("inlineCode",{parentName:"p"},"cql:query_plan_branch=0")," would yield ",(0,i.kt)("inlineCode",{parentName:"p"},"SELECT 10 b"),", which is the same as the default behaviour."))}m.isMDXComponent=!0}}]);
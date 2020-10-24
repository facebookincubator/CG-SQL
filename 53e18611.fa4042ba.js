(window.webpackJsonp=window.webpackJsonp||[]).push([[16],{103:function(e,t,n){"use strict";n.d(t,"a",(function(){return d})),n.d(t,"b",(function(){return m}));var r=n(0),o=n.n(r);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var l=o.a.createContext({}),u=function(e){var t=o.a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},d=function(e){var t=u(e.components);return o.a.createElement(l.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return o.a.createElement(o.a.Fragment,{},t)}},h=o.a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,a=e.originalType,i=e.parentName,l=c(e,["components","mdxType","originalType","parentName"]),d=u(n),h=r,m=d["".concat(i,".").concat(h)]||d[h]||p[h]||a;return n?o.a.createElement(m,s(s({ref:t},l),{},{components:n})):o.a.createElement(m,s({ref:t},l))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var a=n.length,i=new Array(a);i[0]=h;var s={};for(var c in t)hasOwnProperty.call(t,c)&&(s[c]=t[c]);s.originalType=e,s.mdxType="string"==typeof e?e:r,i[1]=s;for(var l=2;l<a;l++)i[l]=n[l];return o.a.createElement.apply(null,i)}return o.a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},71:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return i})),n.d(t,"metadata",(function(){return s})),n.d(t,"rightToc",(function(){return c})),n.d(t,"default",(function(){return u}));var r=n(2),o=n(6),a=(n(0),n(103)),i={id:"introduction",title:"Introduction",sidebar_label:"Introduction"},s={unversionedId:"introduction",id:"introduction",isDocsHomePage:!1,title:"Introduction",description:"CG/SQL is a code generation system for the popular SQLite library that allows developers to write stored procedures in a variant of Transact-SQL (T-SQL) and compile them into C code that uses SQLite\u2019s C API to do the coded operations. CG/SQL enables engineers to create highly complex stored procedures with very large queries, without the manual code checking that existing methods require.",source:"@site/../docs/introduction.md",slug:"/introduction",permalink:"/CG-SQL/docs/introduction",version:"current",sidebar_label:"Introduction",sidebar:"someSidebar",next:{title:"Building CG/SQL",permalink:"/CG-SQL/docs/building"}},c=[],l={rightToc:c};function u(e){var t=e.components,n=Object(o.a)(e,["components"]);return Object(a.b)("wrapper",Object(r.a)({},l,n,{components:t,mdxType:"MDXLayout"}),Object(a.b)("p",null,"CG/SQL is a code generation system for the popular SQLite library that allows developers to write stored procedures in a variant of Transact-SQL (T-SQL) and compile them into C code that uses SQLite\u2019s C API to do the coded operations. CG/SQL enables engineers to create highly complex stored procedures with very large queries, without the manual code checking that existing methods require."),Object(a.b)("p",null,"The full system also includes features for managing and upgrading schema, creating test code for stored procedures, getting query plans for procedures, as well as interfacing with stored procedures from other languages, such as Java and Objective-C. The JSON output allows for the creation of even more analysis or interfacing code. The package includes extensive documentation on the language and system."),Object(a.b)("h1",{id:"what-it-does"},"What it does:"),Object(a.b)("p",null,"The CQL compiler does most of the heavy lifting. This compiler reads the schema and procedures, providing a strongly typed language with hundreds of compile-time errors designed to prevent runtime SQL issues. The compiler carefully tracks the data types of variables as well as schema types, and reports inconsistencies (such as trying to assign nullable columns to non-nullable output variables), and otherwise ensures that the SQLite APIs are used consistently and properly."),Object(a.b)("p",null,"The generated code always checks the various return codes and always uses the correct column ordinals and column types when binding or reading data to or from the SQLite system \u2014 areas that are notoriously difficult to get right and keep right. Additionally, annotations on schema allow the system to automatically create stored procedures that will upgrade a database from any previous schema version to the current one, with dozens of checks in place to make this possible."),Object(a.b)("p",null,"Annotations on procedures can also be used to indicate that you would like supporting test code to create schema fragments and insert data into that schema. This allows procedures to be unit tested with minimal fuss and no dependency on an as-deployed system. Likewise, these facilities can create schema to allow query plans to be checked at compile time."),Object(a.b)("h1",{id:"why-it-matters"},"Why it matters:"),Object(a.b)("p",null,"SQLite is widely used, but creating well tested and maintainable data access layers can be challenging at best. Many teams use some kind of code generation to avoid having to change dozens of ordinals every time a column is added, but these can be error prone. The CQL compiler in CG/SQL allows you to create highly complex stored procedures with very large queries and with a combination of syntax helpers and strong typing these procedures are much easier to get right and keep right. The combination of strong typing in the language plus facilities for good unit testing can provide confidence that even very complex logic is correct. Syntax helpers convert safer code into the canonical SQL, allowing engineers to write less code that is more correct \u2014 and still runs everywhere. For example:"),Object(a.b)("pre",null,Object(a.b)("code",Object(r.a)({parentName:"pre"},{}),"create procedure insert_a_row(like your_table)\nbegin\n  insert into your_table from arguments;\nend;\n")),Object(a.b)("p",null,"Creates a procedure that inserts into any table (e.g., your_table) whose arguments are exactly the columns of the table. You cannot possibly forget any columns, nor can you put the potentially dozens of arguments in the wrong order. These forms are not only brief but also highly error-resistant, making it easier for engineers to generate code without having to check every bit manually."),Object(a.b)("p",null,"Original Source: ",Object(a.b)("a",Object(r.a)({parentName:"p"},{href:"https://engineering.fb.com/open-source/cg-sql/"}),"FB Engineering blog")))}u.isMDXComponent=!0}}]);
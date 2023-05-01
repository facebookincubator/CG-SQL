"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[50],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return u},MDXProvider:function(){return p},mdx:function(){return x},useMDXComponents:function(){return m},withMDXComponents:function(){return s}});var l=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(){return r=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var l in n)Object.prototype.hasOwnProperty.call(n,l)&&(e[l]=n[l])}return e},r.apply(this,arguments)}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);t&&(l=l.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,l)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function d(e,t){if(null==e)return{};var n,l,a=function(e,t){if(null==e)return{};var n,l,a={},r=Object.keys(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var u=l.createContext({}),s=function(e){return function(t){var n=m(t.components);return l.createElement(e,r({},t,{components:n}))}},m=function(e){var t=l.useContext(u),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},p=function(e){var t=m(e.components);return l.createElement(u.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return l.createElement(l.Fragment,{},t)}},h=l.forwardRef((function(e,t){var n=e.components,a=e.mdxType,r=e.originalType,i=e.parentName,u=d(e,["components","mdxType","originalType","parentName"]),s=m(n),p=a,h=s["".concat(i,".").concat(p)]||s[p]||c[p]||r;return n?l.createElement(h,o(o({ref:t},u),{},{components:n})):l.createElement(h,o({ref:t},u))}));function x(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var r=n.length,i=new Array(r);i[0]=h;var o={};for(var d in t)hasOwnProperty.call(t,d)&&(o[d]=t[d]);o.originalType=e,o.mdxType="string"==typeof e?e:a,i[1]=o;for(var u=2;u<r;u++)i[u]=n[u];return l.createElement.apply(null,i)}return l.createElement.apply(null,n)}h.displayName="MDXCreateElement"},57695:function(e,t,n){n.r(t),n.d(t,{assets:function(){return s},contentTitle:function(){return d},default:function(){return c},frontMatter:function(){return o},metadata:function(){return u},toc:function(){return m}});var l=n(83117),a=n(80102),r=(n(67294),n(3905)),i=["components"],o={id:"x1",title:"Appendix 1: Command Line Options",sidebar_label:"Appendix 1: Command Line Options"},d=void 0,u={unversionedId:"x1",id:"x1",title:"Appendix 1: Command Line Options",description:"\x3c!---",source:"@site/../CQL_Guide/x1.md",sourceDirName:".",slug:"/x1",permalink:"/cql-guide/x1",draft:!1,tags:[],version:"current",lastUpdatedBy:"timch326",lastUpdatedAt:1682983565,formattedLastUpdatedAt:"May 1, 2023",frontMatter:{id:"x1",title:"Appendix 1: Command Line Options",sidebar_label:"Appendix 1: Command Line Options"},sidebar:"someSidebar",previous:{title:"Chapter 15: Query Plan Generation",permalink:"/cql-guide/ch15"},next:{title:"Appendix 2: CQL Grammar",permalink:"/cql-guide/x2"}},s={},m=[{value:"With No Options",id:"with-no-options",level:3},{value:"--in file",id:"--in-file",level:3},{value:"--sem",id:"--sem",level:3},{value:"--ast",id:"--ast",level:3},{value:"--echo",id:"--echo",level:3},{value:"--dot",id:"--dot",level:3},{value:"--cg output1 output2 ...",id:"--cg-output1-output2-",level:3},{value:"--nolines",id:"--nolines",level:3},{value:"--global_proc name",id:"--global_proc-name",level:3},{value:"--compress",id:"--compress",level:3},{value:"--test",id:"--test",level:3},{value:"--dev",id:"--dev",level:3},{value:"--c_include_namespace",id:"--c_include_namespace",level:3},{value:"--c_include_path",id:"--c_include_path",level:3},{value:"--objc_c_include_path",id:"--objc_c_include_path",level:3},{value:"Result Types (--rt *)",id:"result-types---rt-",level:3},{value:"--rt c",id:"--rt-c",level:4},{value:"--cqlrt foo.h",id:"--cqlrt-fooh",level:5},{value:"--generate_type_getters",id:"--generate_type_getters",level:5},{value:"--generate_exports",id:"--generate_exports",level:5},{value:"--rt objc",id:"--rt-objc",level:4},{value:"--rt schema",id:"--rt-schema",level:4},{value:"--rt schema_upgrade",id:"--rt-schema_upgrade",level:4},{value:"--include_regions a b c",id:"--include_regions-a-b-c",level:5},{value:"--exclude_regions x y z",id:"--exclude_regions-x-y-z",level:5},{value:"--min_schema_version n",id:"--min_schema_version-n",level:5},{value:"--schema_exclusive",id:"--schema_exclusive",level:5},{value:"--rt json_schema",id:"--rt-json_schema",level:4},{value:"--rt query_plan",id:"--rt-query_plan",level:4},{value:"--rt stats",id:"--rt-stats",level:4},{value:"--rt udf",id:"--rt-udf",level:4}],p={toc:m};function c(e){var t=e.components,n=(0,a.Z)(e,i);return(0,r.mdx)("wrapper",(0,l.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,r.mdx)("p",null,"CQL has a variety of command line (CLI) options but many of them are only interesting for cql development.  Nonetheless this is a comprehensive list:"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"note CQL is often used after the c pre-processor is run so this kind of invocation is typical:")),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cc -E -x c foo.sql | cql [args]\n")),(0,r.mdx)("h3",{id:"with-no-options"},"With No Options"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"emits a usage message")),(0,r.mdx)("h3",{id:"--in-file"},"--in file"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"reads the given file for the input instead of stdin"),(0,r.mdx)("li",{parentName:"ul"},"the input should probably have already been run through the C pre-processor as above"),(0,r.mdx)("li",{parentName:"ul"},"returns non-zero if the file fails to parse")),(0,r.mdx)("p",null,"Example:"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --in test.sql\n")),(0,r.mdx)("h3",{id:"--sem"},"--sem"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"performs semantic analysis on the input file ONLY"),(0,r.mdx)("li",{parentName:"ul"},"the return code is zero if there are no errors")),(0,r.mdx)("p",null,"Example:"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --in sem_test.sql --sem\n")),(0,r.mdx)("h3",{id:"--ast"},"--ast"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"walks the AST and prints it to stdout in human readable text form"),(0,r.mdx)("li",{parentName:"ul"},"may be combined with --sem (semantic info will be included)\nExample")),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --in sem_test.sql --sem --ast >sem_ast.out\n")),(0,r.mdx)("h3",{id:"--echo"},"--echo"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"walks the AST and emits the text of a program that would create it"),(0,r.mdx)("li",{parentName:"ul"},'this has the effect of "beautifying" badly formatted input or "canonicalizing" it',(0,r.mdx)("ul",{parentName:"li"},(0,r.mdx)("li",{parentName:"ul"},"some sensible indenting is added but it might not be the original indenting"),(0,r.mdx)("li",{parentName:"ul"},"extraneous whitespace, parens, etc. are removed"))),(0,r.mdx)("li",{parentName:"ul"},"may be combined with --sem (in which case you see the source after any rewrites for sugar)"),(0,r.mdx)("li",{parentName:"ul"},"this also validates that the input can be parsed")),(0,r.mdx)("p",null,"Example"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},'cql --in test.sql --echo >test.out  # test.out is "equivalent" to test.sql\n')),(0,r.mdx)("h3",{id:"--dot"},"--dot"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"prints the internal AST to stdout in DOT format for graph visualization"),(0,r.mdx)("li",{parentName:"ul"},"this is really only interesting for small graphs for discussion as it rapidly gets insane")),(0,r.mdx)("p",null,"Example:"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --dot --in dottest.sql\n")),(0,r.mdx)("h3",{id:"--cg-output1-output2-"},"--cg output1 output2 ..."),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"any number of output files may be needed for a particular result type, two is common"),(0,r.mdx)("li",{parentName:"ul"},"the return code is zero if there are no errors"),(0,r.mdx)("li",{parentName:"ul"},"any --cg option implies --sem")),(0,r.mdx)("p",null,"Example:"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --in foo.sql --cg foo.h foo.c\n")),(0,r.mdx)("h3",{id:"--nolines"},"--nolines"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"Suppress the # directives for lines.  Useful if you need to debug the C code.")),(0,r.mdx)("p",null,"Example:"),(0,r.mdx)("pre",null,(0,r.mdx)("code",{parentName:"pre"},"cql --in test.sql --nolines --cg foo.h foo.c\n")),(0,r.mdx)("h3",{id:"--global_proc-name"},"--global_proc name"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"any loose SQL statements not in a stored proc are gathered and put into a procedure of the given name"),(0,r.mdx)("li",{parentName:"ul"},"when generating a schema migrate script the global proc name is used as a prefix on all of the artifacts so that there can be several independent migrations linked into a single executable")),(0,r.mdx)("h3",{id:"--compress"},"--compress"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"for use with the C result type, (or any similar types added to the runtime array in your compiler)"),(0,r.mdx)("li",{parentName:"ul"},'string literals for the SQL are broken into "fragments" the DML is then represented by an array of fragments'),(0,r.mdx)("li",{parentName:"ul"},"since DML is often very similar there is a lot of token sharing possible"),(0,r.mdx)("li",{parentName:"ul"},"the original string is recreated at runtime from the fragments and then executed"),(0,r.mdx)("li",{parentName:"ul"},"comments show the original string inline for easier debugging and searching")),(0,r.mdx)("p",null,"NOTE: different result types require a different number of output files with different meanings"),(0,r.mdx)("h3",{id:"--test"},"--test"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"some of the output types can include extra diagnostics if ",(0,r.mdx)("inlineCode",{parentName:"li"},"--test")," is included"),(0,r.mdx)("li",{parentName:"ul"},"the test output often makes the outputs badly formed so this is generally good for humans only")),(0,r.mdx)("h3",{id:"--dev"},"--dev"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"some codegen features only make sense during development, this enables dev mode to turn those one\n** example: ",(0,r.mdx)("a",{parentName:"li",href:"/cql-guide/ch15"},"explain query plan"))),(0,r.mdx)("h3",{id:"--c_include_namespace"},"--c_include_namespace"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},'for the C codegen runtimes, it determines the header namespace (as in #include "namespace/file.h") that goes into the output C file'),(0,r.mdx)("li",{parentName:"ul"},"if this option is used, it is prefixed to the first argment to --cg to form the include path in the C file"),(0,r.mdx)("li",{parentName:"ul"},'if absent there is no "namespace/" prefix')),(0,r.mdx)("h3",{id:"--c_include_path"},"--c_include_path"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},'for the C codegen runtimes, it determines the full header path (as in #include "your_arg") that goes into the output C file'),(0,r.mdx)("li",{parentName:"ul"},"if this option is used, the first argment to --cg controls only the output path and does not appear in include path at all"),(0,r.mdx)("li",{parentName:"ul"},"this form overrides --c_include_namespace if both are specified")),(0,r.mdx)("h3",{id:"--objc_c_include_path"},"--objc_c_include_path"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"for ObjC codegen runtimes that need to refer to the generated C code, this represents the header of the C generated code that will be used during inclusion from the ObjC file")),(0,r.mdx)("h3",{id:"result-types---rt-"},"Result Types (--rt *)"),(0,r.mdx)("p",null,"These are the various outputs the compiler can produce."),(0,r.mdx)("h4",{id:"--rt-c"},"--rt c"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"requires two output files (foo.h and foo.c)"),(0,r.mdx)("li",{parentName:"ul"},"this is the standard C compilation of the sql file")),(0,r.mdx)("h5",{id:"--cqlrt-fooh"},"--cqlrt foo.h"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"emits ",(0,r.mdx)("inlineCode",{parentName:"li"},'#include "foo.h"')," into the C output instead of ",(0,r.mdx)("inlineCode",{parentName:"li"},'#include "cqlrt.h"'))),(0,r.mdx)("h5",{id:"--generate_type_getters"},"--generate_type_getters"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"changes C output for CQL result sets so that the field readers used shared functions to get fields of a certain type"),(0,r.mdx)("li",{parentName:"ul"},"this style of codegen makes result-sets more interoperable with each other if they have similar shape so it can be useful")),(0,r.mdx)("h5",{id:"--generate_exports"},"--generate_exports"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"adds an additional output file"),(0,r.mdx)("li",{parentName:"ul"},"example:  `--in foo.sql --generate_exports --rt c --cg foo.h foo.c foo_exports.sql"),(0,r.mdx)("li",{parentName:"ul"},"the output file ",(0,r.mdx)("inlineCode",{parentName:"li"},"foo_exports.sql")," includes procedure declarations for the contents of ",(0,r.mdx)("inlineCode",{parentName:"li"},"foo.sql")),(0,r.mdx)("li",{parentName:"ul"},"basically automatically generates the CQL header file you need to access the procedures in the input from some other file"),(0,r.mdx)("li",{parentName:"ul"},"if it were C it would be like auto-generating ",(0,r.mdx)("inlineCode",{parentName:"li"},"foo.h")," from ",(0,r.mdx)("inlineCode",{parentName:"li"},"foo.c"))),(0,r.mdx)("h4",{id:"--rt-objc"},"--rt objc"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"objective C wrappers for result sets produced by the stored procedures in the input"),(0,r.mdx)("li",{parentName:"ul"},"these depend on the output of a standard codegen run so this is additive"),(0,r.mdx)("li",{parentName:"ul"},"requires one output file (foo.h)")),(0,r.mdx)("h4",{id:"--rt-schema"},"--rt schema"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces the canonical schema for the given input files"),(0,r.mdx)("li",{parentName:"ul"},"stored procedures etc. are removed"),(0,r.mdx)("li",{parentName:"ul"},"whitespace etc. is removed"),(0,r.mdx)("li",{parentName:"ul"},'suitable for use to create the next or first "previous" schema for schema validation'),(0,r.mdx)("li",{parentName:"ul"},"requires one output file")),(0,r.mdx)("h4",{id:"--rt-schema_upgrade"},"--rt schema_upgrade"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces a CQL schema upgrade script which can then be compiled with CQL itself"),(0,r.mdx)("li",{parentName:"ul"},"see the chapter on schema upgrade/migration: ",(0,r.mdx)("a",{parentName:"li",href:"https://cgsql.dev/cql-guide/ch10/"},"Chapter 10")),(0,r.mdx)("li",{parentName:"ul"},"requires one output file (foo.sql)")),(0,r.mdx)("h5",{id:"--include_regions-a-b-c"},"--include_regions a b c"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"the indicated regions will be declared"),(0,r.mdx)("li",{parentName:"ul"},"used with ",(0,r.mdx)("inlineCode",{parentName:"li"},"--rt schema_upgrade")," or ",(0,r.mdx)("inlineCode",{parentName:"li"},"--rt schema")),(0,r.mdx)("li",{parentName:"ul"},"in the upgrade case excluded regions will not be themselves upgraded, but may be referred, to by things that are being upgraded")),(0,r.mdx)("h5",{id:"--exclude_regions-x-y-z"},"--exclude_regions x y z"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"the indicated regions will still be declared but the upgrade code will be suppressed, the presumption being that a different script already upgrades x y z"),(0,r.mdx)("li",{parentName:"ul"},"used with ",(0,r.mdx)("inlineCode",{parentName:"li"},"--rt schema_upgrade"))),(0,r.mdx)("h5",{id:"--min_schema_version-n"},"--min_schema_version n"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"the schema upgrade script will not include upgrade steps for schema older than the version specified")),(0,r.mdx)("h5",{id:"--schema_exclusive"},"--schema_exclusive"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"the schema upgrade script assumes it owns all the schema in the database, it aggressively removes other things")),(0,r.mdx)("h4",{id:"--rt-json_schema"},"--rt json_schema"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces JSON output suitable for consumption by downstream codegen"),(0,r.mdx)("li",{parentName:"ul"},"the JSON includes a definition of the various entities in the input"),(0,r.mdx)("li",{parentName:"ul"},"see the section on JSON output for details")),(0,r.mdx)("h4",{id:"--rt-query_plan"},"--rt query_plan"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces CQL output which can be re-compiled by CQL as normal input"),(0,r.mdx)("li",{parentName:"ul"},"the output consists of a set of procedures that will emit all query plans for the DML that was in the input"),(0,r.mdx)("li",{parentName:"ul"},"see also ",(0,r.mdx)("inlineCode",{parentName:"li"},"--rt udf")," and ",(0,r.mdx)("a",{parentName:"li",href:"/cql-guide/ch15"},"Chapter 15"))),(0,r.mdx)("h4",{id:"--rt-stats"},"--rt stats"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces  a simple .csv file with node count information for AST nodes per procedure in the input"),(0,r.mdx)("li",{parentName:"ul"},"requires one output file (foo.csv)")),(0,r.mdx)("h4",{id:"--rt-udf"},"--rt udf"),(0,r.mdx)("ul",null,(0,r.mdx)("li",{parentName:"ul"},"produces stub UDF implementations for all UDFS that were seen in the input"),(0,r.mdx)("li",{parentName:"ul"},"this output is suitable for use with ",(0,r.mdx)("inlineCode",{parentName:"li"},"--rt query_plan")," so that SQL with UDFs will run in a simple context"),(0,r.mdx)("li",{parentName:"ul"},"requires two output files (e.g. udfs.h and udfs.c)"),(0,r.mdx)("li",{parentName:"ul"},"See also ",(0,r.mdx)("a",{parentName:"li",href:"/cql-guide/ch15"},"Chapter 15"))))}c.isMDXComponent=!0}}]);
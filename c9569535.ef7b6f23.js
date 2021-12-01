(window.webpackJsonp=window.webpackJsonp||[]).push([[73],{128:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return o})),n.d(t,"metadata",(function(){return c})),n.d(t,"rightToc",(function(){return s})),n.d(t,"default",(function(){return p}));var a=n(2),r=n(6),i=(n(0),n(148)),l=["components"],o={id:"x3",title:"Appendix 3: Control Directives",sidebar_label:"Appendix 3: Control Directives"},c={unversionedId:"x3",id:"x3",isDocsHomePage:!1,title:"Appendix 3: Control Directives",description:"\x3c!---",source:"@site/../CQL_Guide/x3.md",slug:"/x3",permalink:"/cql-guide/x3",version:"current",lastUpdatedBy:"Raoul Foaleng",lastUpdatedAt:1638398501,sidebar_label:"Appendix 3: Control Directives",sidebar:"someSidebar",previous:{title:"Appendix 2: CQL Grammar",permalink:"/cql-guide/x2"},next:{title:"Appendix 4: CQL Error Codes",permalink:"/cql-guide/x4"}},s=[],b={rightToc:s};function p(e){var t=e.components,n=Object(r.a)(e,l);return Object(i.b)("wrapper",Object(a.a)({},b,n,{components:t,mdxType:"MDXLayout"}),Object(i.b)("p",null,"The control directives are those statements that begin with ",Object(i.b)("inlineCode",{parentName:"p"},"@")," and they are distinguished from other statements because they influence the compiler rather than the program logic.  Some of these are of great importance and discussed elsewhere."),Object(i.b)("p",null,"The complete list (as of this writing) is:"),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@ENFORCE_STRICT"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@ENFORCE_NORMAL")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"These enable or disable more strict semanic checking the sub options are",Object(i.b)("ul",{parentName:"li"},Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"FOREIGN KEY ON UPDATE"),": all FK's must choose some ",Object(i.b)("inlineCode",{parentName:"li"},"ON UPDATE")," strategy"),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"FOREIGN KEY ON DELETE"),": all FK's must choose some ",Object(i.b)("inlineCode",{parentName:"li"},"ON DELETE")," strategy"),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"PROCEDURE"),": all procedures must be declared before they are called (eliminating the vanilla ",Object(i.b)("inlineCode",{parentName:"li"},"C")," call option)"),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"JOIN"),": all joins must be ANSI style, the form ",Object(i.b)("inlineCode",{parentName:"li"},"FROM A,B")," is not allowed (replace with ",Object(i.b)("inlineCode",{parentName:"li"},"A INNER JOIN B")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"WINDOW FUNC"),": window functions are disallowed (useful if targeting old versions of SQLite)"),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"UPSERT STATEMENT"),": the upsert form is disallowed (useful if targeting old versions of SQLite)")))),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@SENSITIVE")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"marks a column or variable as 'sensitive' for privacy purposes, this behaves somewhat like nullability (See Chapter 3) in that it is radioactive, contaminating anything it touches"),Object(i.b)("li",{parentName:"ul"},"the intent of this annotation is to make it clear where sensitive data is being returned or consumed in your procedures"),Object(i.b)("li",{parentName:"ul"},"this information appears in the JSON output for further codegen or for analysis (See Chapter 13)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@DECLARE_SCHEMA_REGION"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@DECLARE_DEPLOYABLE_REGION"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@BEGIN_SCHEMA_REGION"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@END_SCHEMA_REGION")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"These directives control the declaration of schema regions and allow you to place things into those regions -- see ",Object(i.b)("a",{parentName:"li",href:"https://cgsql.dev/cql-guide/ch10"},"Chapter 10"))),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@SCHEMA_AD_HOC_MIGRATION")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"Allows for the creation of a ad hoc migration step at a given schema version, (See Chapter 10)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@ECHO")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"Emits text into the C output stream, useful for emiting things like function prototypes or preprocessor directives"),Object(i.b)("li",{parentName:"ul"},"e.g. `echo C, '#define foo bar'")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@RECREATE"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@CREATE"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@DELETE")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"used to mark the schema version where an object is created or deleted, or alternatively indicate the the object is always dropped and recreated when it changes (See Chapter 10)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@SCHEMA_UPGRADE_VERSION")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"used to indicate that the code that follows is part of a migration script for the indicated schema version"),Object(i.b)("li",{parentName:"ul"},"this has the effect of making the schema appear to be how it existed at the indicated version"),Object(i.b)("li",{parentName:"ul"},"the idea here is that migration procedures operate on previous versions of the schema where (e.g.) some columns/tables hadn't been deleted yet")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@PREVIOUS_SCHEMA")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"indicates the start of the previous version of the schema for comparison (See Chapter 11)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@SCHEMA_UPGRADE_SCRIPT")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"CQL emits a schema upgrade script as part of its upgrade features, this script declares tables in their final form but also creates the same tables as they existed when they were first created"),Object(i.b)("li",{parentName:"ul"},"this directive instructs CQL to ignore the incompatible creations, the first declaration controls"),Object(i.b)("li",{parentName:"ul"},"the idea here is that the upgrade script is in the business of getting you to the finish line in an orderly fashion and some of the interim steps are just not all the way there yet"),Object(i.b)("li",{parentName:"ul"},"note that the upgrade script recapitulates the version history, it does not take you directly to the finish line, this is so that all instances get to the same place the same way (and this fleshes out any bugs in migration)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@DUMMY_NULLABLES"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@DUMMY_DEFAULTS"),"\n",Object(i.b)("inlineCode",{parentName:"p"},"@DUMMY_SEED")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"these control the creation of dummy data for ",Object(i.b)("inlineCode",{parentName:"li"},"insert")," and ",Object(i.b)("inlineCode",{parentName:"li"},"fetch")," statements (See Chapters 5 and 12)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@FILE")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"a string literal that corresponds to the current file name with a prefix stripped (to remove build lab junk in the path)")),Object(i.b)("p",null,Object(i.b)("inlineCode",{parentName:"p"},"@ATTRIBUTE")),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},"the main purpose of ",Object(i.b)("inlineCode",{parentName:"p"},"@attribute")," is to appear in the JSON output so that it can control later codegen stages in whatever way you deem appropriate")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},"the nested nature of attribute values is sufficiently flexible than you could encode an arbitrary LISP program in an attribute, so really anything you might need to express is possible")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},"there are a number of attributes known to the compiler which I list below (complete as of this writing)")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:autodrop=(table1, table2, ...)")," when present the indicated tables, which must be temp tables, are dropped when the results of the procedure have been fetched into a rowset")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:identity=(column1, column2, ...)")," the indicated columns are used to create a row comparator for the rowset corresponding to the procedure, this appears in a C macro of the form ",Object(i.b)("inlineCode",{parentName:"p"},"procedure_name_row_same(rowset1, row1, rowset2, row2)"))),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:suppress_getters")," the annotated procedure should not emit its related column getter functions."),Object(i.b)("ul",{parentName:"li"},Object(i.b)("li",{parentName:"ul"},"Useful if you only indend to call the procedure from CQL."),Object(i.b)("li",{parentName:"ul"},"Saves code generation and removes the possibility of C code using the getters."))),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:suppress_result_set"),' the annotated procedure should not emit its related "fetch results" function.'),Object(i.b)("ul",{parentName:"li"},Object(i.b)("li",{parentName:"ul"},"Useful if you only indend to call the procedure from CQL."),Object(i.b)("li",{parentName:"ul"},"Saves code generation and removes the possibility of C code using the result set or getters."),Object(i.b)("li",{parentName:"ul"},"Implies ",Object(i.b)("inlineCode",{parentName:"li"},"cql:suppress_getters"),"; since there is no result set, getters would be redundant."),Object(i.b)("li",{parentName:"ul"},"Note: an ",Object(i.b)("inlineCode",{parentName:"li"},"OUT UNION")," procedure cannot have a suppressed result set since all such a procedure does is produce a result set. This attribute is ignored for out union procedures."))),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:private")," the annotated procedure will be static in the generated C"),Object(i.b)("ul",{parentName:"li"},Object(i.b)("li",{parentName:"ul"},"Because the generated function is ",Object(i.b)("inlineCode",{parentName:"li"},"static")," it cannot be called from other modules and therefore will not go in any CQL exports file (that would be moot since you couldn't call it)."),Object(i.b)("li",{parentName:"ul"},"This attribute also implies ",Object(i.b)("inlineCode",{parentName:"li"},"cql:suppress_result_set")," since only CQL code in the same translation unit could possibly call it and hence the result set procedure is useless to other C code."))),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:generate_copy")," the code generation for the annotated procedure will produce a ",Object(i.b)("inlineCode",{parentName:"p"},"[procedure_name]_copy")," function that can make complete or partial copies of its result set.")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:base_fragment=frag_name")," used for base fragments (See Chapter 14)")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:extension_fragment=frag_name")," used for extension fragments (See Chapter 14)")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:assembly_fragment=frag_name")," used for assembly fragments (See Chapter 14)")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:no_table_scan")," for query plan processing, indicates that the table in question should never be table scanned in any plan (for better diagnostics)")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("p",{parentName:"li"},Object(i.b)("inlineCode",{parentName:"p"},"cql:autotest=([many forms])")," declares various autotest features (See Chapter 12)"))))}p.isMDXComponent=!0},148:function(e,t,n){"use strict";n.d(t,"a",(function(){return p})),n.d(t,"b",(function(){return d}));var a=n(0),r=n.n(a);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=r.a.createContext({}),b=function(e){var t=r.a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},p=function(e){var t=b(e.components);return r.a.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},m=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,l=e.parentName,s=c(e,["components","mdxType","originalType","parentName"]),p=b(n),m=a,d=p["".concat(l,".").concat(m)]||p[m]||u[m]||i;return n?r.a.createElement(d,o(o({ref:t},s),{},{components:n})):r.a.createElement(d,o({ref:t},s))}));function d(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,l=new Array(i);l[0]=m;var o={};for(var c in t)hasOwnProperty.call(t,c)&&(o[c]=t[c]);o.originalType=e,o.mdxType="string"==typeof e?e:a,l[1]=o;for(var s=2;s<i;s++)l[s]=n[s];return r.a.createElement.apply(null,l)}return r.a.createElement.apply(null,n)}m.displayName="MDXCreateElement"}}]);
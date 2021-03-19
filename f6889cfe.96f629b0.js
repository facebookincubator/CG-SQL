(window.webpackJsonp=window.webpackJsonp||[]).push([[72],{126:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return s})),n.d(t,"metadata",(function(){return i})),n.d(t,"rightToc",(function(){return o})),n.d(t,"default",(function(){return b}));var a=n(2),r=n(6),l=(n(0),n(136)),s={id:"ch14",title:"Chapter 14: CQL Query Fragments",sidebar_label:"Chapter 14: CQL Query Fragments"},i={unversionedId:"ch14",id:"ch14",isDocsHomePage:!1,title:"Chapter 14: CQL Query Fragments",description:"\x3c!---",source:"@site/../CQL_Guide/ch14.md",slug:"/ch14",permalink:"/cql-guide/ch14",version:"current",lastUpdatedBy:"Raoul Foaleng",lastUpdatedAt:1616128871,sidebar_label:"Chapter 14: CQL Query Fragments",sidebar:"someSidebar",previous:{title:"Chapter 13: JSON Output",permalink:"/cql-guide/ch13"},next:{title:"Appendix 1: Command Line Options",permalink:"/cql-guide/x1"}},o=[{value:"Base Query Fragments",id:"base-query-fragments",children:[]},{value:"Extension Query Fragments",id:"extension-query-fragments",children:[]}],c={rightToc:o};function b(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(l.b)("wrapper",Object(a.a)({},c,n,{components:t,mdxType:"MDXLayout"}),Object(l.b)("p",null,'CQL Query fragments are the most sophisticated rewrite CQL offers for productivity.  The idea is that a very large query\ncan be represented in "fragments" that add columns or add rows based on the original "core" query.  The final query\nwill be an assembled rewrite of all the fragments chained together.  Specifically, the motivation for this is that you\ncan have a "core" query that fetches the essential columns for some UI design and then you can add query extension\nfragments that add new/additional columns for some new set of features.  The core and extended columns can be in their\nown fragment and they can be compiled independently.  The result of this is that any errors are in much smaller\nand easier to understand fragments rather than in some monster "fetch everything" query;  any given extension does not\nhave to know all the details of all the other extensions and can take a limited dependency on even the core query.'),Object(l.b)("p",null,"It's easiest to illustrate this with an example so let's begin there."),Object(l.b)("p",null,"Let's first start with this very simple schema."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre"},"create table my_table(\n id integer primary key,\n name text not null,\n rate real not null\n);\n\ncreate table added_rows(\n like my_table -- sugar to duplicate the columns of my_table\n);\n\ncreate table added_columns(\n id integer references my_table(id),\n data text\n);\n\n")),Object(l.b)("p",null,"Typically there would be a lot more columns but where you see ",Object(l.b)("inlineCode",{parentName:"p"},"flag1")," and ",Object(l.b)("inlineCode",{parentName:"p"},"flag2")," appear in fragments you can imagine any number\nof additional columns of any type.  So we can keep the examples simple."),Object(l.b)("h3",{id:"base-query-fragments"},"Base Query Fragments"),Object(l.b)("p",null,"The base fragment might look something like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre"},"@attribute(cql:base_fragment=base_frag)\ncreate proc base_frag_template(id_ integer not null)\nbegin\n  with\n    base_frag(*) as (select * from my_table where my_table.id = id_)\n    select * from base_frag;\nend;\n")),Object(l.b)("p",null,"Here are the essential aspects:"),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"the base fragment is given a name, it can be anything, probably something that describes the purpose of the fragments"),Object(l.b)("li",{parentName:"ul"},"the procedure name can be anything at all"),Object(l.b)("li",{parentName:"ul"},"the procedure must consiste of exactly one ",Object(l.b)("inlineCode",{parentName:"li"},"with...select")," statement"),Object(l.b)("li",{parentName:"ul"},"the fragment name must be the one and only CTE in the select statement"),Object(l.b)("li",{parentName:"ul"},"you must select all the columns from the CTE")),Object(l.b)("p",null,"Note the syntax helper ",Object(l.b)("inlineCode",{parentName:"p"},"base_frag(*)")," is just shorthand to avoid retyping all the column names of ",Object(l.b)("inlineCode",{parentName:"p"},"my_table"),"."),Object(l.b)("p",null,"The interesting part is ",Object(l.b)("inlineCode",{parentName:"p"},"(select * from my_table where my_table.id = id_)")," which could have been any select statement\nof your choice. Everything else in the procedure must follow the designated format, and the format is enforced due to\nthe presence of ",Object(l.b)("inlineCode",{parentName:"p"},"@attribute(cql:base_fragment=base_frag)"),"."),Object(l.b)("p",null,"The point of putting everything on rails like this is that all base fragments will look the same and it will be clear how to transform any base fragment into the final query when it is assembled with its extensions."),Object(l.b)("p",null,"Note: the base fragment produces no codegen at all.  There is no ",Object(l.b)("inlineCode",{parentName:"p"},"base_frag_template")," procedure in the output.  This is just a template.  Also, the name of the procedure cannot be ",Object(l.b)("inlineCode",{parentName:"p"},"base_frag")," this name will be used by the assembly fragment later.  Really any descriptive unique name will do since the name does not appear in the output at all."),Object(l.b)("h3",{id:"extension-query-fragments"},"Extension Query Fragments"),Object(l.b)("h4",{id:"adding-columns"},"Adding Columns"),Object(l.b)("p",null,"The most common thing that an extension might want to do is add columns to the result.  There can be any number of such extensions in the final assembly.  Here's a simple example that adds one column."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'@attribute(cql:extension_fragment=base_frag)\ncreate proc adds_columns(id_ integer not null)\nbegin\n  with\n    base_frag(*) as (select 1 id, "name" name, 1.0 rate),\n    col_adder_frag(*) as (\n    select base_frag.*, added_columns.data\n      from base_frag\n      left outer join added_columns on base_frag.id = added_columns.id)\n  select * from col_adder_frag;\nend;\n')),Object(l.b)("p",null,"Again there are some important features to this extension and they are largely completely constrained, i.e. you must follow the pattern."),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"the attribute indicates ",Object(l.b)("inlineCode",{parentName:"li"},"extension_fragment")," and the name (here ",Object(l.b)("inlineCode",{parentName:"li"},"base_frag"),") must have been previously declared in a ",Object(l.b)("inlineCode",{parentName:"li"},"base_fragment")),Object(l.b)("li",{parentName:"ul"},"the procedure name can be any unique name other than ",Object(l.b)("inlineCode",{parentName:"li"},"base_frag"),", it corresponds to this particular extension's purpose"),Object(l.b)("li",{parentName:"ul"},"the procedure arguments must be identical to those in the base fragment"),Object(l.b)("li",{parentName:"ul"},"the first CTE must match the ",Object(l.b)("inlineCode",{parentName:"li"},"base_fragment")," attribute value, ",Object(l.b)("inlineCode",{parentName:"li"},"base_frag")," in this case"),Object(l.b)("li",{parentName:"ul"},"you do not need to repeat the full select statement for ",Object(l.b)("inlineCode",{parentName:"li"},"base_frag"),", any surrogate with the same column names and types will do",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"the base fragment code might include a #define to make this easier",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"e.g. ",Object(l.b)("inlineCode",{parentName:"li"},'#define base_frags_core as base_frag(*) as (select 1 id, "name" name, 1.0 rate)')))),Object(l.b)("li",{parentName:"ul"},"doing so will make maintenance easier if new columns are added to the base fragment"))),Object(l.b)("li",{parentName:"ul"},"there must be exactly one additional CTE",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"it may have any unique descriptive name you like"),Object(l.b)("li",{parentName:"ul"},"it must begin with ",Object(l.b)("inlineCode",{parentName:"li"},"select base_frags.*")," with the appropriate CTE name matching the base fragment CTE"),Object(l.b)("li",{parentName:"ul"},"it must add at least one column (or it would be uninteresting)"),Object(l.b)("li",{parentName:"ul"},"it may not have any clause other than the first ",Object(l.b)("inlineCode",{parentName:"li"},"from")," (e.g. no ",Object(l.b)("inlineCode",{parentName:"li"},"where"),", ",Object(l.b)("inlineCode",{parentName:"li"},"having"),", ",Object(l.b)("inlineCode",{parentName:"li"},"limit")," etc.)",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"if any of these were allowed they would remove or re-order rows in the base query which is not allowed"),Object(l.b)("li",{parentName:"ul"},"the ",Object(l.b)("inlineCode",{parentName:"li"},"from")," clause often includes nested selects which have no restrictions"))),Object(l.b)("li",{parentName:"ul"},"it must select from the base fragment name and left outer join to whereever it likes to get optional additional columns",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"because of this the additional column(s) will certainly be a nullable type in the projection"))))),Object(l.b)("li",{parentName:"ul"},"the final select must be of the form ",Object(l.b)("inlineCode",{parentName:"li"},"select * from col_adder_frag")," with the appropriate name"),Object(l.b)("li",{parentName:"ul"},"keeping all this in mind, the interesting bit happens here:  ",Object(l.b)("inlineCode",{parentName:"li"},"left outer join added_columns on base_frag.id = added_columns.id"),Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"this is where you get the data for your additional column using values in the core columns")))),Object(l.b)("p",null,"This fragment can be (and should be) compiled in its own compiland while using ",Object(l.b)("inlineCode",{parentName:"p"},"#include")," to get the base fragment only.  This will result in code gen for the accessor functions for a piece of the overall query -- the part this extension knows about.  Importantly code that uses this extension's data does not need or want to know about any other extensions that may be present, thereby keeping\ndependencies under control."),Object(l.b)("p",null,"The C signatures generated would look like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-c"},"extern cql_int32 adds_columns_get_id(\n    base_frag_result_set_ref _Nonnull result_set,\n    cql_int32 row);\n\nextern cql_string_ref _Nonnull adds_columns_get_name(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_double adds_columns_get_rate(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_string_ref _Nullable adds_columns_get_data(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_int32 adds_columns_result_count(\n  base_frag_result_set_ref _Nonnull result_set);\n")),Object(l.b)("p",null,'Even if there were dozens of other extensions, the functions for reading those columns would not be declared in the header for\nthis extension.  Any given extension "sees" only the core columns plus any columns it added.'),Object(l.b)("h4",{id:"adding-rows"},"Adding Rows"),Object(l.b)("p",null,"Query extensions also frequently want to add additional rows to the main result set, based on the data that is already present."),Object(l.b)("p",null,"The second form of extension allows for this, it is similarly locked in form.  Here is an example:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'@attribute(cql:extension_fragment=base_frag)\ncreate proc adds_rows(id_ integer not null)\nbegin\n  with\n    base_frag(*) as (select 1 id, "name" name, 1.0 rate),\n    row_adder_frag(*) as (\n    select * from base_frag\n    union all\n    select * from added_rows)\n  select * from row_adder_frag;\nend;\n')),Object(l.b)("p",null,"Let's review the features of this second template form:"),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"there is a surrogate for the core query"),Object(l.b)("li",{parentName:"ul"},"there is a manatory second CTE"),Object(l.b)("li",{parentName:"ul"},"the second CTE is a compound query with any number of branches, all ",Object(l.b)("inlineCode",{parentName:"li"},"union all")),Object(l.b)("li",{parentName:"ul"},"the first branch must be ",Object(l.b)("inlineCode",{parentName:"li"},"select * from base_frag")," (the base fragment) to ensure that the original rows remain",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"this is also why all the branches must be ",Object(l.b)("inlineCode",{parentName:"li"},"union all")))),Object(l.b)("li",{parentName:"ul"},"this form cannot add new columns"),Object(l.b)("li",{parentName:"ul"},"the extension CTE may not include ",Object(l.b)("inlineCode",{parentName:"li"},"order by")," or ",Object(l.b)("inlineCode",{parentName:"li"},"limit")," because that might reorder or remove rows of the base"),Object(l.b)("li",{parentName:"ul"},"any extensions of this form must come before those of the ",Object(l.b)("inlineCode",{parentName:"li"},"left outer join")," form for a given base fragment",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"which ironically means ",Object(l.b)("inlineCode",{parentName:"li"},"row_adder_frag")," has to come before ",Object(l.b)("inlineCode",{parentName:"li"},"col_adder_frag")))),Object(l.b)("li",{parentName:"ul"},"the usual restrictions on compound selects (same type and number of columns) ensure a consistent result"),Object(l.b)("li",{parentName:"ul"},"the final select after the CTE section must exactly in the form ",Object(l.b)("inlineCode",{parentName:"li"},"select * from row_adder_frag")," which is the name of the one and only additional CTE with no other clauses or options",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"in practice only the CTE will be used to create the final assembly so even if you did change the final select to something else it would be moot")))),Object(l.b)("p",null,"The signatures generated for this will look something like so:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-c"},"extern cql_int32 adds_rows_get_id(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_string_ref _Nonnull adds_rows_get_name(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_double adds_rows_get_rate(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_int32 adds_rows_result_count(\n  base_frag_result_set_ref _Nonnull result_set);\n")),Object(l.b)("p",null,'Which gives you access to the core columns.  Again this fragment can and should be compiled standalone with only the declaration\nfor the base fragment in the same translation unit to get the cleanest possible output.  This is so that consumers of this\nextension do not "see" other extensions which may or may not be related and may or may not always be present.'),Object(l.b)("h4",{id:"assembling-the-fragments"},"Assembling the Fragments"),Object(l.b)("p",null,"With all the fragments independently declared they need to be unified to create one final query. This is where the\nmajor rewriting happens.  The ",Object(l.b)("inlineCode",{parentName:"p"},"assembly_fragment")," looks something like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},'@attribute(cql:assembly_fragment=base_frag)\ncreate proc base_frag(id_ integer not null)\nbegin\n  with\n    base_frag(*) as (select 1 id, "name" name, 1.0 rate)\n    select * from base_frag;\nend;\n')),Object(l.b)("p",null,"It will always be as simple as this, all the complexity is in the fragments."),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"the ",Object(l.b)("inlineCode",{parentName:"li"},"assembly_fragment")," name must match the core fragment name"),Object(l.b)("li",{parentName:"ul"},"the procedure arguments must be identical to the base fragment arguments"),Object(l.b)("li",{parentName:"ul"},"the  procedure must have the same name as the assembly fragment (",Object(l.b)("inlineCode",{parentName:"li"},"base_frag")," in this case)",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"the code that was generated for the previous fragments anticipates this and makes reference to what will be generated here"),Object(l.b)("li",{parentName:"ul"},"this is enforced"))),Object(l.b)("li",{parentName:"ul"},"the assembled query is what you run to get the result set, this has real code behind it",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"the other fragments only produce result set readers that call into the helper meethods to get columns"))),Object(l.b)("li",{parentName:"ul"},"there is a surrogate for the core fragment as usual"),Object(l.b)("li",{parentName:"ul"},"all of CTE section will ultimately be replaced with the fragments chained together"),Object(l.b)("li",{parentName:"ul"},"the final select should be of the form ",Object(l.b)("inlineCode",{parentName:"li"},"select * from your_frags")," but it can include ordering and/or filtering, this statement will be present in final codegen, the final order is usually defined here")),Object(l.b)("p",null,"When compiling the assembly fragment, you should include the base, and all the other fragments, and the assembly template.  The presence of the assembly_fragment will cause codegen for the extension fragments to be suppressed. The assembly translation unit only contains the assembly query as formed from the fragments."),Object(l.b)("p",null,"Now let's look at how the query is rewritten, the process is pretty methodical."),Object(l.b)("p",null,"After rewriting the assembly looks like this:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-sql"},"CREATE PROC base_frag (id_ INTEGER NOT NULL)\nBEGIN\n  WITH\n  base_frag (id, name, rate) AS (SELECT *\n    FROM my_table\n    WHERE my_table.id = id_),\n  row_adder_frag (id, name, rate) AS (SELECT *\n    FROM base_frag\n  UNION ALL\n  SELECT *\n    FROM added_rows),\n  col_adder_frag (id, name, rate, data) AS (SELECT row_adder_frag.*, added_columns.data\n    FROM row_adder_frag\n    LEFT OUTER JOIN added_columns ON row_adder_frag.id = added_columns.id)\n  SELECT *\n    FROM col_adder_frag;\nEND;\n")),Object(l.b)("p",null,"Let's dissect this part by part, each CTE serves a purpose."),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"the core CTE was replaced by the CTE in the base_fragment, it appears directly"),Object(l.b)("li",{parentName:"ul"},"next the first extension was added as a CTE referring to the base fragment just as before",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"recall that the first extension has to be ",Object(l.b)("inlineCode",{parentName:"li"},"row_adder_frag"),", as that type must come first"),Object(l.b)("li",{parentName:"ul"},"looking at the chain you can see why it would be hard to write a correct fragment if it came after columns were added"))),Object(l.b)("li",{parentName:"ul"},"next the second extension was added as a CTE",Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"all references to the base fragment were replaced with references to row_adder_frag"),Object(l.b)("li",{parentName:"ul"},"the extra column names in the CTE were added such that all previous column names are introduced"))),Object(l.b)("li",{parentName:"ul"},"this process continues until all extensions are exhausted"),Object(l.b)("li",{parentName:"ul"},"the final select statement reads all the columns from the last extension CTE and includes and ordering and so forth that was present in the assembly query")),Object(l.b)("p",null,"The result of all this is a single query that gets all the various columns that were requested in all the extensions\nand all the ",Object(l.b)("inlineCode",{parentName:"p"},"union all")," operations play out as written.  The extensions are emitted in the order that they appear\nin the translation unit with the assembly, which again must have the row adding extensions first."),Object(l.b)("p",null,"This facility provides considerable ability to compose a large query, but each fragment can be independently checked for errors\nso that nobody ever has to debug the (possibly monstrous) overall result.  Fragments can be removed simply by\nexcluding them from the final assembly (with e.g. #ifdefs, or build rules)"),Object(l.b)("p",null,"With the rewrite of the assembly_fragment complete, the codegen for that procedure is the normal codegen for a procedure with a single select."),Object(l.b)("p",null,"As always, Java and Objective C codegen on these pieces will produce suitable wrappers for the C."),Object(l.b)("p",null,"The output code for the assembly fragment generates these reading functions:"),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-c"},"extern cql_int32 base_frag_get_id(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_string_ref _Nonnull base_frag_get_name(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_double base_frag_get_rate(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\n// used by adds_columns_get_data() to read its data\nextern cql_string_ref _Nullable __PRIVATE__base_frag_get_data(\n  base_frag_result_set_ref _Nonnull result_set,\n  cql_int32 row);\n\nextern cql_int32 base_frag_result_count(\n  base_frag_result_set_ref _Nonnull result_set);\n")),Object(l.b)("p",null,"These are exactly what you would get for a normal query except that the pieces that came from extensions are marked ",Object(l.b)("inlineCode",{parentName:"p"},"PRIVATE"),".  Those methods should not be used directly but instead the methods generated for each extension proc should be used."),Object(l.b)("p",null,"Additionally, to create the result set, as usual."),Object(l.b)("pre",null,Object(l.b)("code",{parentName:"pre",className:"language-c"},"extern CQL_WARN_UNUSED cql_code base_frag_fetch_results(\n  sqlite3 *_Nonnull _db_,\n  base_frag_result_set_ref _Nullable *_Nonnull result_set,\n  cql_int32 id_);\n")),Object(l.b)("p",null,"With the combined set of methods you can create a variety of assembled queries from extensions in a fairly straightforward way."))}b.isMDXComponent=!0},136:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return h}));var a=n(0),r=n.n(a);function l(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function s(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?s(Object(n),!0).forEach((function(t){l(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):s(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},l=Object.keys(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var c=r.a.createContext({}),b=function(e){var t=r.a.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=b(e.components);return r.a.createElement(c.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},m=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,l=e.originalType,s=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),u=b(n),m=a,h=u["".concat(s,".").concat(m)]||u[m]||d[m]||l;return n?r.a.createElement(h,i(i({ref:t},c),{},{components:n})):r.a.createElement(h,i({ref:t},c))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=n.length,s=new Array(l);s[0]=m;var i={};for(var o in t)hasOwnProperty.call(t,o)&&(i[o]=t[o]);i.originalType=e,i.mdxType="string"==typeof e?e:a,s[1]=i;for(var c=2;c<l;c++)s[c]=n[c];return r.a.createElement.apply(null,s)}return r.a.createElement.apply(null,n)}m.displayName="MDXCreateElement"}}]);
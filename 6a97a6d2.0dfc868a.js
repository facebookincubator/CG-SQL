(window.webpackJsonp=window.webpackJsonp||[]).push([[41],{148:function(e,n,a){"use strict";a.d(n,"a",(function(){return p})),a.d(n,"b",(function(){return m}));var t=a(0),r=a.n(t);function o(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function l(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function s(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?l(Object(a),!0).forEach((function(n){o(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function i(e,n){if(null==e)return{};var a,t,r=function(e,n){if(null==e)return{};var a,t,r={},o=Object.keys(e);for(t=0;t<o.length;t++)a=o[t],n.indexOf(a)>=0||(r[a]=e[a]);return r}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(t=0;t<o.length;t++)a=o[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var b=r.a.createContext({}),c=function(e){var n=r.a.useContext(b),a=n;return e&&(a="function"==typeof e?e(n):s(s({},n),e)),a},p=function(e){var n=c(e.components);return r.a.createElement(b.Provider,{value:n},e.children)},u={inlineCode:"code",wrapper:function(e){var n=e.children;return r.a.createElement(r.a.Fragment,{},n)}},d=r.a.forwardRef((function(e,n){var a=e.components,t=e.mdxType,o=e.originalType,l=e.parentName,b=i(e,["components","mdxType","originalType","parentName"]),p=c(a),d=t,m=p["".concat(l,".").concat(d)]||p[d]||u[d]||o;return a?r.a.createElement(m,s(s({ref:n},b),{},{components:a})):r.a.createElement(m,s({ref:n},b))}));function m(e,n){var a=arguments,t=n&&n.mdxType;if("string"==typeof e||t){var o=a.length,l=new Array(o);l[0]=d;var s={};for(var i in n)hasOwnProperty.call(n,i)&&(s[i]=n[i]);s.originalType=e,s.mdxType="string"==typeof e?e:t,l[1]=s;for(var b=2;b<o;b++)l[b]=a[b];return r.a.createElement.apply(null,l)}return r.a.createElement.apply(null,a)}d.displayName="MDXCreateElement"},96:function(e,n,a){"use strict";a.r(n),a.d(n,"frontMatter",(function(){return s})),a.d(n,"metadata",(function(){return i})),a.d(n,"rightToc",(function(){return b})),a.d(n,"default",(function(){return p}));var t=a(2),r=a(6),o=(a(0),a(148)),l=["components"],s={id:"x7",title:"Appendix 7: CQL Anti-patterns",sidebar_label:"Appendix 7: CQL Anti-patterns"},i={unversionedId:"x7",id:"x7",isDocsHomePage:!1,title:"Appendix 7: CQL Anti-patterns",description:"\x3c!---",source:"@site/../CQL_Guide/x7.md",slug:"/x7",permalink:"/cql-guide/x7",version:"current",lastUpdatedBy:"Edward Pastuszenski",lastUpdatedAt:1634123848,sidebar_label:"Appendix 7: CQL Anti-patterns",sidebar:"someSidebar",previous:{title:"Appendix 6: CQL In 20 Minutes",permalink:"/cql-guide/x6"},next:{title:"Appendix 8: CQL Best Practices",permalink:"/cql-guide/x8"}},b=[{value:"Common Schema",id:"common-schema",children:[]},{value:"Declarations",id:"declarations",children:[]},{value:"Casts",id:"casts",children:[]},{value:"Boolean expressions and CASE/WHEN",id:"boolean-expressions-and-casewhen",children:[]},{value:"CASE and CAST and NULL",id:"case-and-cast-and-null",children:[]},{value:"Filtering out NULLs",id:"filtering-out-nulls",children:[]},{value:"Not null boolean expressions",id:"not-null-boolean-expressions",children:[]},{value:"Using <code>IS</code> when it makes sense to do so",id:"using-is-when-it-makes-sense-to-do-so",children:[]},{value:"Left joins that are not left joins",id:"left-joins-that-are-not-left-joins",children:[]}],c={rightToc:b};function p(e){var n=e.components,a=Object(r.a)(e,l);return Object(o.b)("wrapper",Object(t.a)({},c,a,{components:n,mdxType:"MDXLayout"}),Object(o.b)("p",null,"These are a few of the antipatterns I've seen while travelling through various CQL source files.  They are in various categories."),Object(o.b)("p",null,"Refer also to Appendix 8: CQL Best Practices."),Object(o.b)("h3",{id:"common-schema"},"Common Schema"),Object(o.b)("p",null,"For these examples let's create a couple of tables we might need for examples"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"CREATE TABLE foo (\n    id integer primary key,\n    name text\n);\n\nCREATE TABLE bar (\n    id integer primary key,\n    rate real\n);\n")),Object(o.b)("h3",{id:"declarations"},"Declarations"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"DECLARE v LONG NOT NULL;\nSET v := 1;\n")),Object(o.b)("p",null,"better"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"LET v := 1L;  -- long literals have the L suffix like in C\n")),Object(o.b)("p",null,"Similarly"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"DECLARE v REAL NOT NULL;\nSET v := 1;\n")),Object(o.b)("p",null,"better"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"LET v := 1.0; -- use scientific notation or add .0 to make a real literal\n")),Object(o.b)("h3",{id:"casts"},"Casts"),Object(o.b)("p",null,"Redundant casts fatten the code and don't really add anything to readability.  Sometimems it's necessary to cast NULL to\na  particular type so that you can be sure that generated result set has the right data type, but most of the casts\nbelow are not necessary."),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT\n    CAST(foo.id as INTEGER) as id,\n    CAST(foo.name as TEXT) as name,\n    CAST(NULL as REAL) as rate\n  FROM foo\nUNION ALL\n  SELECT\n    CAST(bar.id as INTEGER) as id,\n    CAST(NULL as TEXT) as name,\n    CAST(bar.rate as REAL) as rate\n  FROM bar\n")),Object(o.b)("p",null,"Better"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT\n    foo.id,\n    foo.name,\n    CAST(NULL as REAL) as rate\n  FROM foo\nUNION ALL\n  SELECT\n    bar.id,\n    CAST(NULL as TEXT) as name,\n    bar.rate\n  FROM bar\n")),Object(o.b)("p",null,"It's possible to do the following to make this even cleaner:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"-- somewhere central\n#define NULL_TEXT CAST(NULL as TEXT)\n#define NULL_REAL CAST(NULL as REAL)\n#define NULL_INT CAST(NULL as INTEGER)\n#define NULL_LONG CAST(NULL as LONG)\n")),Object(o.b)("p",null,"Then you can write"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT\n    foo.id,\n    foo.name,\n    NULL_REAL as rate\n  FROM foo\nUNION ALL\n  SELECT\n    bar.id,\n    NULL_TEXT as name,\n    bar.rate\n  FROM bar\n")),Object(o.b)("h4",{id:"booleans"},"Booleans"),Object(o.b)("p",null,"TRUE and FALSE can be used as boolean literals."),Object(o.b)("p",null,"SQLite doesn't care about the type but CQL will get the type information it needs to make the columns of type BOOL"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT\n    foo.id,\n    foo.name,\n    NULL_REAL as rate,\n    TRUE as has_name,  -- this is a bit artificial but you get the idea\n    FALSE as has_rate\n  FROM foo\nUNION ALL\n  SELECT\n    bar.id,\n    NULL_TEXT as name,\n    bar.rate,\n    FALSE as has_name,\n    TRUE as has_rate\n  FROM bar\n")),Object(o.b)("h3",{id:"boolean-expressions-and-casewhen"},"Boolean expressions and CASE/WHEN"),Object(o.b)("p",null,"It's easy to get carried away with the power of ",Object(o.b)("inlineCode",{parentName:"p"},"CASE")," expressions, I've seen this kind of thing:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"CAST(CASE WHEN foo.name IS NULL THEN 0 ELSE 1 END AS BOOL)\n")),Object(o.b)("p",null,"But this is simply"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"foo.name IS NOT NULL\n")),Object(o.b)("p",null,"In general, if your case alternates are booleans a direct boolean expression would have served you better."),Object(o.b)("h3",{id:"case-and-cast-and-null"},"CASE and CAST and NULL"),Object(o.b)("p",null,"Somtimes there's clamping or filtering going on in a case statement"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"CAST(CASE WHEN foo.name > 'm' THEN foo.name ELSE NULL END AS TEXT)\n")),Object(o.b)("p",null,"Here the ",Object(o.b)("inlineCode",{parentName:"p"},"CAST")," is not needed at all so we could go to"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"CASE WHEN foo.name > 'm' THEN foo.name ELSE NULL END\n")),Object(o.b)("p",null,Object(o.b)("inlineCode",{parentName:"p"},"NULL")," is already the default value for the ",Object(o.b)("inlineCode",{parentName:"p"},"ELSE")," clause so you never need ",Object(o.b)("inlineCode",{parentName:"p"},"ELSE NULL")),Object(o.b)("p",null,"So better:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"CASE WHEN foo.name > 'm' THEN foo.name END\n")),Object(o.b)("h3",{id:"filtering-out-nulls"},"Filtering out NULLs"),Object(o.b)("p",null,"Consider"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"SELECT *\n    FROM foo\n    WHERE foo.name IS NOT NULL AND foo.name > 'm';\n")),Object(o.b)("p",null,"There's no need to test for ",Object(o.b)("inlineCode",{parentName:"p"},"NOT NULL")," here, the boolean will result in ",Object(o.b)("inlineCode",{parentName:"p"},"NULL")," if ",Object(o.b)("inlineCode",{parentName:"p"},"foo.name")," is null\nwhich is not true so the ",Object(o.b)("inlineCode",{parentName:"p"},"WHERE")," test will fail."),Object(o.b)("p",null,"Better:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"SELECT *\n    FROM foo\n    WHERE foo.name > 'm';\n")),Object(o.b)("h3",{id:"not-null-boolean-expressions"},"Not null boolean expressions"),Object(o.b)("p",null,"In this statement we do not want to have a null result for the boolean expression"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"SELECT\n    id,\n    name,\n    CAST(IFNULL(name > 'm', 0) AS BOOL) AS name_bigger_than_m\n    FROM FOO;\n")),Object(o.b)("p",null,"So now we've made several mistakes.  We could have used the usual ",Object(o.b)("inlineCode",{parentName:"p"},"FALSE")," defintion to avoid the cast.\nBut even that would have left us with an IFNULL that's harder to read.  Here's a much simpler formulation:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"SELECT\n    id,\n    name,\n    name > 'm' IS TRUE AS name_bigger_than_m\n    FROM FOO;\n")),Object(o.b)("p",null,"Even without the ",Object(o.b)("inlineCode",{parentName:"p"},"TRUE")," macro you could do ",Object(o.b)("inlineCode",{parentName:"p"},"IS 1")," above and still get a result of type ",Object(o.b)("inlineCode",{parentName:"p"},"BOOL NOT NULL")),Object(o.b)("h3",{id:"using-is-when-it-makes-sense-to-do-so"},"Using ",Object(o.b)("inlineCode",{parentName:"h3"},"IS")," when it makes sense to do so"),Object(o.b)("p",null,"This kind of boolean expression is also verbose for no reason"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"    rate IS NOT NULL AND rate = 20\n")),Object(o.b)("p",null,"In a ",Object(o.b)("inlineCode",{parentName:"p"},"WHERE")," clause probably ",Object(o.b)("inlineCode",{parentName:"p"},"rate = 20")," suffices but even if you really need a ",Object(o.b)("inlineCode",{parentName:"p"},"NOT NULL BOOL"),"\nresult the expression above is exactly what the ",Object(o.b)("inlineCode",{parentName:"p"},"IS")," operator is for.  e.g."),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"    rate IS 20\n")),Object(o.b)("p",null,"The ",Object(o.b)("inlineCode",{parentName:"p"},"IS")," operator is frequently avoided except for ",Object(o.b)("inlineCode",{parentName:"p"},"IS NULL")," and ",Object(o.b)("inlineCode",{parentName:"p"},"IS NOT NULL")," but it's a general equality operator\nwith the added semantic that it never returns ",Object(o.b)("inlineCode",{parentName:"p"},"NULL"),".   ",Object(o.b)("inlineCode",{parentName:"p"},"NULL IS NULL")," is true.  ",Object(o.b)("inlineCode",{parentName:"p"},"NULL IS [anything not null]")," is false."),Object(o.b)("h3",{id:"left-joins-that-are-not-left-joins"},"Left joins that are not left joins"),Object(o.b)("p",null,"Consider"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT foo.id,\n         foo.name,\n         bar.rate\n  FROM foo\n  LEFT JOIN bar ON foo.id = bar.id\n  WHERE bar.rate > 5;\n")),Object(o.b)("p",null,"This is no longer a left join because the ",Object(o.b)("inlineCode",{parentName:"p"},"WHERE")," clause demands a value for at least one column from ",Object(o.b)("inlineCode",{parentName:"p"},"bar"),"."),Object(o.b)("p",null,"Better:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-sql"},"  SELECT foo.id,\n         foo.name,\n         bar.rate\n  FROM foo\n  INNER JOIN bar ON foo.id = bar.id\n  WHERE bar.rate > 5;\n")))}p.isMDXComponent=!0}}]);
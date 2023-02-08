"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5617],{3905:function(e,t,n){n.d(t,{Zo:function(){return u},kt:function(){return h}});var l=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);t&&(l=l.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,l)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,l,a=function(e,t){if(null==e)return{};var n,l,a={},r=Object.keys(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=l.createContext({}),c=function(e){var t=l.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=c(e.components);return l.createElement(s.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return l.createElement(l.Fragment,{},t)}},p=l.forwardRef((function(e,t){var n=e.components,a=e.mdxType,r=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),p=c(n),h=a,m=p["".concat(s,".").concat(h)]||p[h]||d[h]||r;return n?l.createElement(m,i(i({ref:t},u),{},{components:n})):l.createElement(m,i({ref:t},u))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var r=n.length,i=new Array(r);i[0]=p;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:a,i[1]=o;for(var c=2;c<r;c++)i[c]=n[c];return l.createElement.apply(null,i)}return l.createElement.apply(null,n)}p.displayName="MDXCreateElement"},9398:function(e,t,n){n.r(t),n.d(t,{assets:function(){return u},contentTitle:function(){return s},default:function(){return h},frontMatter:function(){return o},metadata:function(){return c},toc:function(){return d}});var l=n(3117),a=n(102),r=(n(7294),n(3905)),i=["components"],o={id:"int05",title:"Part 5: CQL Runtime",sidebar_label:"Part 5: CQL Runtime"},s=void 0,c={unversionedId:"int05",id:"int05",title:"Part 5: CQL Runtime",description:"\x3c!---",source:"@site/../CQL_Guide/int05.md",sourceDirName:".",slug:"/int05",permalink:"/cql-guide/int05",draft:!1,tags:[],version:"current",lastUpdatedBy:"Tim Cheung",lastUpdatedAt:1675817993,formattedLastUpdatedAt:"Feb 8, 2023",frontMatter:{id:"int05",title:"Part 5: CQL Runtime",sidebar_label:"Part 5: CQL Runtime"},sidebar:"someSidebar",previous:{title:"Part 4: Testing",permalink:"/cql-guide/int04"},next:{title:"Part 6: Schema Management",permalink:"/cql-guide/int06"}},u={},d=[{value:"Preface",id:"preface",level:3},{value:"CQL Runtime",id:"cql-runtime",level:2},{value:"Standard headers",id:"standard-headers",level:3},{value:"Contract and Error Macros",id:"contract-and-error-macros",level:3},{value:"The Value Types",id:"the-value-types",level:3},{value:"The Reference Types",id:"the-reference-types",level:3},{value:"Mocking",id:"mocking",level:3},{value:"Profiling",id:"profiling",level:3},{value:"Encoding of Sensitive Columns",id:"encoding-of-sensitive-columns",level:3},{value:"The Common Headers",id:"the-common-headers",level:3},{value:"The <code>cqlrt_cf</code> Runtime",id:"the-cqlrt_cf-runtime",level:3},{value:"Recap",id:"recap",level:3}],p={toc:d};function h(e){var t=e.components,n=(0,a.Z)(e,i);return(0,r.kt)("wrapper",(0,l.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h3",{id:"preface"},"Preface"),(0,r.kt)("p",null,"Part 5 continues with a discussion of the essentials of the CQL Runtime.\nAs in the previous sections, the goal here is not to go over every detail but rather to give\na sense of how the runtime works in general -- the core strategies and implementation choices --\nso that when reading the source you will have an idea how it all hangs together. To accomplish\nthis, we'll illustrate the key pieces that can be customized and we'll discuss some\ninteresting cases."),(0,r.kt)("h2",{id:"cql-runtime"},"CQL Runtime"),(0,r.kt)("p",null,"The parts of the runtime that you can change are in ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h"),", that file invariably ends by including\n",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.h")," which are the runtime parts that you shouldn't change.  Of course this is open source\nso you can change anything, but the common things usually don't need to change -- ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h")," should\nprovide you with everything you need to target new environments."),(0,r.kt)("p",null,"The compiler itself can be customized see ",(0,r.kt)("inlineCode",{parentName:"p"},"rt.c")," to emit different strings to work with your runtime.\nThis is pretty easy to do without creating a merge hell for yourself. Meta Platforms, for instance,  has its\nown CQL runtime customized for use on phones that is not open source (and really I don't think anyone\nwould want it anyway).  But the point is that you can make your own. In fact I know of two just within\nMeta Platforms."),(0,r.kt)("p",null,"We'll go over ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h")," bit by bit.  Keeping in mind it might change but this is\nessentially what's going on.  And the essentials don't change very often."),(0,r.kt)("h3",{id:"standard-headers"},"Standard headers"),(0,r.kt)("p",null,"The rest of the system will use these, ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h")," is responsible for bringing in what you need\nlater, or what ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.h")," needs on your system."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"#pragma once\n\n#include <assert.h>\n#include <stddef.h>\n#include <stdint.h>\n#include <math.h>\n#include <sqlite3.h>\n\n#ifndef __clang__\n#ifndef _Nonnull\n    /* Hide Clang-only nullability specifiers if not Clang */\n    #define _Nonnull\n    #define _Nullable\n#endif\n#endif\n")),(0,r.kt)("h3",{id:"contract-and-error-macros"},"Contract and Error Macros"),(0,r.kt)("p",null,"CQL has a few different macros it uses for errors.  ",(0,r.kt)("inlineCode",{parentName:"p"},"contract"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"invariant"),", and ",(0,r.kt)("inlineCode",{parentName:"p"},"tripwire"),"\nusually all map to ",(0,r.kt)("inlineCode",{parentName:"p"},"assert"),".  Note that ",(0,r.kt)("inlineCode",{parentName:"p"},"tripwire")," doesn't have to be fatal, it can log\nin production and continue.  This is a \"softer\" assertion.  Something that you're trying out\nthat you'd like to be a ",(0,r.kt)("inlineCode",{parentName:"p"},"contract")," but maybe there are lingering cases that have to be fixed\nfirst."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"#define cql_contract assert\n#define cql_invariant assert\n#define cql_tripwire assert\n#define cql_log_database_error(...)\n#define cql_error_trace()\n")),(0,r.kt)("h3",{id:"the-value-types"},"The Value Types"),(0,r.kt)("p",null,"You can define these types to be whatever is appropriate on your system.\nUsually the mapping is pretty obvious."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// value types\ntypedef unsigned char cql_bool;\n#define cql_true (cql_bool)1\n#define cql_false (cql_bool)0\n\ntypedef unsigned long cql_hash_code;\ntypedef int32_t cql_int32;\ntypedef uint32_t cql_uint32;\ntypedef uint16_t cql_uint16;\ntypedef sqlite3_int64 cql_int64;\ntypedef double cql_double;\ntypedef int cql_code;\n")),(0,r.kt)("h3",{id:"the-reference-types"},"The Reference Types"),(0,r.kt)("p",null,"The default runtime first defines 4 types of reference objects.\nThese are the only reference types that CQL creates itself. In\nfact CQL doesn't actually create ",(0,r.kt)("inlineCode",{parentName:"p"},"CQL_C_TYPE_OBJECT")," but the tests\ndo.  CQL never creates raw object things, only external functions\ncan do that."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// metatypes for the straight C implementation\n#define CQL_C_TYPE_STRING 0\n#define CQL_C_TYPE_BLOB 1\n#define CQL_C_TYPE_RESULTS 2\n#define CQL_C_TYPE_BOXED_STMT 3\n#define CQL_C_TYPE_OBJECT 4\n")),(0,r.kt)("p",null,"All the reference types are reference counted. So they\nneed a simple shape that allows them to know their own\ntype and have a count.  They also have a finalize method\nto clean up their memory when the count goes to zero."),(0,r.kt)("p",null,"You get to define ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_type_ref")," to be whatever you want."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// base ref counting struct\ntypedef struct cql_type *cql_type_ref;\ntypedef struct cql_type {\n  int type;\n  int ref_count;\n  void (*_Nullable finalize)(cql_type_ref _Nonnull ref);\n} cql_type;\n")),(0,r.kt)("p",null,"Whatever you do with the types you'll need to define\na retain and release method that uses them as the signature.\nNormal references should have a generic value comparison and a hash."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"void cql_retain(cql_type_ref _Nullable ref);\nvoid cql_release(cql_type_ref _Nullable ref);\n\ncql_hash_code cql_ref_hash(cql_type_ref _Nonnull typeref);\ncql_bool cql_ref_equal(cql_type_ref _Nullable typeref1, cql_type_ref _Nullable typeref2);\n")),(0,r.kt)("p",null,"Now each of the various kinds of reference types needs an\nobject which probably includes the base type above.  It doesn't\nhave to.  You can arrange for some other universal way to do\nthese.  On iOS these can be easily mapped to ",(0,r.kt)("inlineCode",{parentName:"p"},"CF")," types."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"retain")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"release")," macros should all map to the same thing.\nThe compiler emits different variations for readability only. It\ndoesn't really work if they don't have common retain/release\nsemantics."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// builtin object\ntypedef struct cql_object *cql_object_ref;\ntypedef struct cql_object {\n  cql_type base;\n  const void *_Nonnull ptr;\n} cql_object;\n\n#define cql_object_retain(object) cql_retain((cql_type_ref)object);\n#define cql_object_release(object) cql_release((cql_type_ref)object);\n")),(0,r.kt)("p",null,"Boxed statement gets its own implementation, same as object."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"// builtin statement box\ntypedef struct cql_boxed_stmt *cql_boxed_stmt_ref;\ntypedef struct cql_boxed_stmt {\n  cql_type base;\n  sqlite3_stmt *_Nullable stmt;\n} cql_boxed_stmt;\n")),(0,r.kt)("p",null,"Same for blob, and blob has a couple of additional helper macros\nthat are used to get information. Blobs also have hash and equality\nfunctions."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// builtin blob\ntypedef struct cql_blob *cql_blob_ref;\ntypedef struct cql_blob {\n  cql_type base;\n  const void *_Nonnull ptr;\n  cql_uint32 size;\n} cql_blob;\n#define cql_blob_retain(object) cql_retain((cql_type_ref)object);\n#define cql_blob_release(object) cql_release((cql_type_ref)object);\ncql_blob_ref _Nonnull cql_blob_ref_new(const void *_Nonnull data, cql_uint32 size);\n#define cql_get_blob_bytes(data) (data->ptr)\n#define cql_get_blob_size(data) (data->size)\ncql_hash_code cql_blob_hash(cql_blob_ref _Nullable str);\ncql_bool cql_blob_equal(cql_blob_ref _Nullable blob1, cql_blob_ref _Nullable blob2);\n")),(0,r.kt)("p",null,"Strings are the same as the others but they have many more functions\nassociated with them."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// builtin string\ntypedef struct cql_string *cql_string_ref;\ntypedef struct cql_string {\n  cql_type base;\n  const char *_Nullable ptr;\n} cql_string;\ncql_string_ref _Nonnull cql_string_ref_new(const char *_Nonnull cstr);\n#define cql_string_retain(string) cql_retain((cql_type_ref)string);\n#define cql_string_release(string) cql_release((cql_type_ref)string);\n")),(0,r.kt)("p",null,"The compiler uses this macro to create a named string literal. You decide\nhow those will be implemented right here."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"#define cql_string_literal(name, text) \\\n  cql_string name##_ = { \\\n    .base = { \\\n      .type = CQL_C_TYPE_STRING, \\\n      .ref_count = 1, \\\n      .finalize = NULL, \\\n    }, \\\n    .ptr = text, \\\n  }; \\\n  cql_string_ref name = &name##_\n")),(0,r.kt)("p",null,"Strings get assorted comparison and hashing functions. Note blob also had a hash."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"int cql_string_compare(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);\ncql_hash_code cql_string_hash(cql_string_ref _Nullable str);\ncql_bool cql_string_equal(cql_string_ref _Nullable s1, cql_string_ref _Nullable s2);\nint cql_string_like(cql_string_ref _Nonnull s1, cql_string_ref _Nonnull s2);\n")),(0,r.kt)("p",null,"Strings can be converted from their reference form to standard C form. These\nmacros define how this is done.  Note that temporary allocations are possible\nhere but the standard implementation does not actually need to do an alloc.  It\nstores UTF8 in the string pointer so it's ready to go."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"#define cql_alloc_cstr(cstr, str) const char *_Nonnull cstr = (str)->ptr\n#define cql_free_cstr(cstr, str) 0\n")),(0,r.kt)("p",null,'The macros for result sets have somewhat less flexibility.  The main thing\nthat you can do here is add additional fields to the "meta" structure.  It\nneeds those key fields because it is created by the compiler.  However the\nAPI is used to create a result set so that can be any object you like.  It\nonly has to respond to the ',(0,r.kt)("inlineCode",{parentName:"p"},"get_meta"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"get_data"),", and ",(0,r.kt)("inlineCode",{parentName:"p"},"get_count"),' apis.\nThose can be mapped as you desire.  In principle there could have been\na macro to create the "meta" as well (a PR for this is welcome) but it\'s\nreally a pain for not much benefit.  The advantage of defining your own "meta"\nis that you can use it to add additional custom APIs to your result set that\nmight need some storage.'),(0,r.kt)("p",null,"The additional API ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_result_set_note_ownership_transferred(result_set)"),"\nis used in the event that you are moving ownership of the buffers from\nout of CQL's universe.  So like maybe JNI is absorbing the result, or\nObjective C is absorbing the result.  The default implementation is a no-op."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// builtin result set\ntypedef struct cql_result_set *cql_result_set_ref;\n\ntypedef struct cql_result_set_meta {\n ...\n}\n\ntypedef struct cql_result_set {\n  cql_type base;\n  cql_result_set_meta meta;\n  cql_int32 count;\n  void *_Nonnull data;\n} cql_result_set;\n\n#define cql_result_set_type_decl(result_set_type, result_set_ref) \\\n  typedef struct _##result_set_type *result_set_ref;\n\ncql_result_set_ref _Nonnull cql_result_set_create(\n  void *_Nonnull data,\n  cql_int32 count,\n  cql_result_set_meta meta);\n\n#define cql_result_set_retain(result_set) cql_retain((cql_type_ref)result_set);\n#define cql_result_set_release(result_set) cql_release((cql_type_ref)result_set);\n#define cql_result_set_note_ownership_transferred(result_set)\n#define cql_result_set_get_meta(result_set) (&((cql_result_set_ref)result_set)->meta)\n#define cql_result_set_get_data(result_set) ((cql_result_set_ref)result_set)->data\n#define cql_result_set_get_count(result_set) ((cql_result_set_ref)result_set)->count\n")),(0,r.kt)("h3",{id:"mocking"},"Mocking"),(0,r.kt)("p",null,"The CQL run test needs to do some mocking.  This bit is here for that test.  If you\nwant to use the run test with your version of ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt")," you'll need to define a\nshim for ",(0,r.kt)("inlineCode",{parentName:"p"},"sqlite3_step")," that can be intercepted.  This probably isn't going to come up."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"#ifdef CQL_RUN_TEST\n#define sqlite3_step mockable_sqlite3_step\nSQLITE_API cql_code mockable_sqlite3_step(sqlite3_stmt *_Nonnull);\n#endif\n")),(0,r.kt)("h3",{id:"profiling"},"Profiling"),(0,r.kt)("p",null,"If you want to support profiling you can implement ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_profile_start")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_profile_stop"),"\nto do whatever you want.  The CRC uniquely identifies a procedure (you can log that).  The\n",(0,r.kt)("inlineCode",{parentName:"p"},"index")," provides you with a place to store something that you can use as a handle in\nyour logging system.  Typically an integer.  This lets you assign indices to the procedures\nyou actually saw in any given run and then log them or something like that.  No data\nabout parameters is provided, this is deliberate."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// No-op implementation of profiling\n// * Note: we emit the crc as an expression just to be sure that there are no compiler\n//   errors caused by names being incorrect.  This improves the quality of the CQL\n//   code gen tests significantly.  If these were empty macros (as they once were)\n//   you could emit any junk in the call and it would still compile.\n#define cql_profile_start(crc, index) (void)crc; (void)index;\n#define cql_profile_stop(crc, index)  (void)crc; (void)index;\n")),(0,r.kt)("p",null,"The definitions in ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.c"),' can provide codegen than either has generic\n"getters" for each column type (useful for JNI) or produces a unique getter that isn\'t\nshared.  The rowset metadata will include the values for ',(0,r.kt)("inlineCode",{parentName:"p"},"getBoolean"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"getDouble")," etc.\nif ",(0,r.kt)("inlineCode",{parentName:"p"},"CQL_NO_GETTERS")," is 0.  Getters are a little slower for C but give you a small number\nof functions that need to have JNI if you are targeting Java."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// the basic version doesn't use column getters\n#define CQL_NO_GETTERS 1\n")),(0,r.kt)("h3",{id:"encoding-of-sensitive-columns"},"Encoding of Sensitive Columns"),(0,r.kt)("p",null,"By setting an attribute on any procedure that produces a result set you can\nhave the selected sensitive values encoded.  If this happens CQL first asks\nfor the encoder and then calls the encode methods passing in the encoder.\nThese aren't meant to be cryptograhically secure but rather to provide some\nability to prevent mistakes.  If you opt in, sensitive values have to be deliberately\ndecoded and that provides an audit trail."),(0,r.kt)("p",null,"The default implementation of all this is a no-op."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},"// implementation of encoding values. All sensitive values read from sqlite db will\n// be encoded at the source. CQL never decode encoded sensitive string unless the\n// user call explicitly decode function from code.\ncql_object_ref _Nullable cql_copy_encoder(sqlite3 *_Nonnull db);\ncql_bool cql_encode_bool(...)\ncql_int32 cql_encode_int32(...)\ncql_int64 cql_encode_int64(...)\ncql_double cql_encode_double(...)\ncql_string_ref _Nonnull cql_encode_string_ref_new(...);\ncql_blob_ref _Nonnull cql_encode_blob_ref_new(..);\ncql_bool cql_decode_bool(...);\ncql_int32 cql_decode_int32(...);\ncql_int64 cql_decode_int64(...);\ncql_double cql_decode_double(...);\ncql_string_ref _Nonnull cql_decode_string_ref_new(...);\ncql_blob_ref _Nonnull cql_decode_blob_ref_new(...);\n")),(0,r.kt)("h3",{id:"the-common-headers"},"The Common Headers"),(0,r.kt)("p",null,"The standard APIs all build on the above, so they should be included last."),(0,r.kt)("p",null,"Now in some cases the signature of the things you provide in ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h")," is basically fixed,\nso it seems like it would be easier to move the prototpyes into ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_common.h"),".\nHowever, in many cases additional things are needed like ",(0,r.kt)("inlineCode",{parentName:"p"},"declspec")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"export")," or\nother system specific things.  The result is that ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.h")," is maybe a bit more\nverbose that it strictly needs to be.  Also some versions of cqlrt.h choose to\nimplement some of the APIs as macros..."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-c"},'// NOTE: This must be included *after* all of the above symbols/macros.\n#include "cqlrt_common.h"\n')),(0,r.kt)("h3",{id:"the-cqlrt_cf-runtime"},"The ",(0,r.kt)("inlineCode",{parentName:"h3"},"cqlrt_cf")," Runtime"),(0,r.kt)("p",null,"In order to use the Objective-C code-gen (",(0,r.kt)("inlineCode",{parentName:"p"},"--rt objc"),") you need a runtime that has reference\ntypes that are friendly to Objective-C.  For this purpose we created an open-source\nversion of such a runtime: it can be found in the ",(0,r.kt)("inlineCode",{parentName:"p"},"sources/cqlrt_cf")," directory.\nThis runtime is also a decent example of how much customization you can do with just\na little code. Some brief notes:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"This runtime really only makes sense on macOS, iOS, or maybe some other place that Core Foundation (",(0,r.kt)("inlineCode",{parentName:"li"},"CF"),") exists",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"As such its build process is considerably less portable than other parts of the system"))),(0,r.kt)("li",{parentName:"ul"},"The CQL reference types have been redefined so that they map to:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"CFStringRef")," (strings)"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"CFTypeRef")," (objects)"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"CFDataRef")," (blobs)"))),(0,r.kt)("li",{parentName:"ul"},"The key worker functions use ",(0,r.kt)("inlineCode",{parentName:"li"},"CF"),", e.g.",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"cql_ref_hash")," maps to ",(0,r.kt)("inlineCode",{parentName:"li"},"CFHash")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"cql_ref_equal")," maps to ",(0,r.kt)("inlineCode",{parentName:"li"},"CFEqual")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"cql_retain")," uses ",(0,r.kt)("inlineCode",{parentName:"li"},"CFRetain")," (with a null guard)"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"cql_release")," uses ",(0,r.kt)("inlineCode",{parentName:"li"},"CFRelease")," (with a null guard)"))),(0,r.kt)("li",{parentName:"ul"},"Strings use ",(0,r.kt)("inlineCode",{parentName:"li"},"CF")," idioms, e.g.",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"string literals are created with ",(0,r.kt)("inlineCode",{parentName:"li"},"CFSTR")),(0,r.kt)("li",{parentName:"ul"},"C strings are created by using ",(0,r.kt)("inlineCode",{parentName:"li"},"CFStringGetCStringPtr")," or ",(0,r.kt)("inlineCode",{parentName:"li"},"CFStringGetCString")," when needed")))),(0,r.kt)("p",null,"Of course, since the meaning of some primitive types has changed, the contract to the CQL generated\ncode has changed accordingly.  For instance:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"procedures compiled against this runtime expect string arguments to be ",(0,r.kt)("inlineCode",{parentName:"li"},"CFStringRef")),(0,r.kt)("li",{parentName:"ul"},"result sets provide ",(0,r.kt)("inlineCode",{parentName:"li"},"CFStringRef")," values for string columns")),(0,r.kt)("p",null,"The consequence of this is that the Objective-C code generation ",(0,r.kt)("inlineCode",{parentName:"p"},"--rt objc")," finds friendly\ncontracts that it can freely convert to types like ",(0,r.kt)("inlineCode",{parentName:"p"},"NSString *")," which results in\nseamless integration with the rest of an Objective-C application."),(0,r.kt)("p",null,"Of course the downside of all this is that the ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_cf")," runtime is less portable.  It can only go\nwhere ",(0,r.kt)("inlineCode",{parentName:"p"},"CF")," exists.  Still, it is an interesting demonstration of the flexablity of the system."),(0,r.kt)("p",null,"The system could be further improved by creating a custom result type (e.g. ",(0,r.kt)("inlineCode",{parentName:"p"},"--rt c_cf"),") and using\nsome of the result type options for the C code generation. For instance, the compiler could do these things:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"generate ",(0,r.kt)("inlineCode",{parentName:"li"},"CFStringRef foo;")," instead of ",(0,r.kt)("inlineCode",{parentName:"li"},"cql_string_ref foo;")," for declarations"),(0,r.kt)("li",{parentName:"ul"},"generate ",(0,r.kt)("inlineCode",{parentName:"li"},"SInt32 an_integer")," instead of ",(0,r.kt)("inlineCode",{parentName:"li"},"cql_int32 an_integer"))),(0,r.kt)("p",null,"Even though ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt_cf")," is already mapping ",(0,r.kt)("inlineCode",{parentName:"p"},"cql_int32")," to something compatible with ",(0,r.kt)("inlineCode",{parentName:"p"},"CF"),",\nmaking such changes would make the C output a little bit more ",(0,r.kt)("inlineCode",{parentName:"p"},"CF")," idiomatic. This educational\nexercise could probably be completed in just a few minutes by interested readers."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"make.sh")," file in the ",(0,r.kt)("inlineCode",{parentName:"p"},"sources/cqlrt_cf")," directory illustrates how to get CQL to use\nthis new runtime.  The demo itself is a simple port of the code in ",(0,r.kt)("a",{parentName:"p",href:"https://cgsql.dev/cql-guide/x10"},"Appendix 10"),"."),(0,r.kt)("h3",{id:"recap"},"Recap"),(0,r.kt)("p",null,"The CQL runtime, ",(0,r.kt)("inlineCode",{parentName:"p"},"cqlrt.c"),", is intended to be replaced.  The version that ships with the distribution\nis a simple, portable implementation that is single threaded. Serious users of CQL will likely\nwant to replace the default version of the runtime with something more tuned to their use case."),(0,r.kt)("p",null,"Topics covered included:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"contract, error, and tracing macros"),(0,r.kt)("li",{parentName:"ul"},"how value types are defined"),(0,r.kt)("li",{parentName:"ul"},"how reference types are defined"),(0,r.kt)("li",{parentName:"ul"},"mocking (for use in a test suite)"),(0,r.kt)("li",{parentName:"ul"},"profiling"),(0,r.kt)("li",{parentName:"ul"},"encoding of sensitive columns"),(0,r.kt)("li",{parentName:"ul"},"boxing statements"),(0,r.kt)("li",{parentName:"ul"},"the ",(0,r.kt)("inlineCode",{parentName:"li"},"cqlrt_cf")," runtime")),(0,r.kt)("p",null,"As with the other parts, no attempt was made to cover every detail.  That is\nbest done by reading the source code."))}h.isMDXComponent=!0}}]);
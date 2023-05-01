"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[3733],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return l},MDXProvider:function(){return p},mdx:function(){return _},useMDXComponents:function(){return u},withMDXComponents:function(){return c}});var r=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function s(){return s=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},s.apply(this,arguments)}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function d(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},s=Object.keys(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var l=r.createContext({}),c=function(e){return function(t){var n=u(t.components);return r.createElement(e,s({},t,{components:n}))}},u=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},p=function(e){var t=u(e.components);return r.createElement(l.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},f=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,s=e.originalType,a=e.parentName,l=d(e,["components","mdxType","originalType","parentName"]),c=u(n),p=o,f=c["".concat(a,".").concat(p)]||c[p]||m[p]||s;return n?r.createElement(f,i(i({ref:t},l),{},{components:n})):r.createElement(f,i({ref:t},l))}));function _(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var s=n.length,a=new Array(s);a[0]=f;var i={};for(var d in t)hasOwnProperty.call(t,d)&&(i[d]=t[d]);i.originalType=e,i.mdxType="string"==typeof e?e:o,a[1]=i;for(var l=2;l<s;l++)a[l]=n[l];return r.createElement.apply(null,a)}return r.createElement.apply(null,n)}f.displayName="MDXCreateElement"},16541:function(e,t,n){n.r(t),n.d(t,{assets:function(){return c},contentTitle:function(){return d},default:function(){return m},frontMatter:function(){return i},metadata:function(){return l},toc:function(){return u}});var r=n(83117),o=n(80102),s=(n(67294),n(3905)),a=["components"],i={id:"x10",title:"Appendix 10: CQL Working Example",sidebar_label:"Appendix 10: CQL Working Example"},d=void 0,l={unversionedId:"x10",id:"x10",title:"Appendix 10: CQL Working Example",description:"\x3c!---",source:"@site/../CQL_Guide/x10.md",sourceDirName:".",slug:"/x10",permalink:"/cql-guide/x10",draft:!1,tags:[],version:"current",lastUpdatedBy:"timch326",lastUpdatedAt:1682983565,formattedLastUpdatedAt:"May 1, 2023",frontMatter:{id:"x10",title:"Appendix 10: CQL Working Example",sidebar_label:"Appendix 10: CQL Working Example"},sidebar:"someSidebar",previous:{title:"Appendix 9: Using the CQL Amalgam",permalink:"/cql-guide/x9"},next:{title:"Appendix 11: Production Considerations",permalink:"/cql-guide/x11"}},c={},u=[{value:"<code>todo.sql</code>",id:"todosql",level:4},{value:"<code>main.c</code>",id:"mainc",level:4},{value:"Build Steps",id:"build-steps",level:3},{value:"Results",id:"results",level:3}],p={toc:u};function m(e){var t=e.components,n=(0,o.Z)(e,a);return(0,s.mdx)("wrapper",(0,r.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,s.mdx)("p",null,"This is a working example that shows all of the basic DML statements and the call patterns\nto access them. The code also includes the various helpers you can use to convert C types to\nCQL types."),(0,s.mdx)("h4",{id:"todosql"},(0,s.mdx)("inlineCode",{parentName:"h4"},"todo.sql")),(0,s.mdx)("pre",null,(0,s.mdx)("code",{parentName:"pre",className:"language-SQL"},"-- This is a simple schema for keeping track of tasks and whether or not they have been completed\n\n-- this serves to both declare the table and create the schema\ncreate proc todo_create_tables()\nbegin\n  create table if not exists tasks(\n    description text not null,\n    done bool default false not null\n  );\nend;\n\n-- adds a new not-done task\ncreate proc todo_add(task TEXT NOT null)\nbegin\n  insert into tasks values(task, false);\nend;\n\n-- gets the tasks in inserted order\ncreate proc todo_tasks()\nbegin\n  select rowid, description, done from tasks order by rowid;\nend;\n\n-- updates a given task by rowid\ncreate proc todo_setdone_(rowid_ integer not null, done_ bool not null)\nbegin\n  update tasks set done = done_ where rowid == rowid_;\nend;\n\n-- deletes a given task by rowid\ncreate proc todo_delete(rowid_ integer not null)\nbegin\n  delete from tasks where rowid == rowid_;\nend;\n")),(0,s.mdx)("h4",{id:"mainc"},(0,s.mdx)("inlineCode",{parentName:"h4"},"main.c")),(0,s.mdx)("pre",null,(0,s.mdx)("code",{parentName:"pre",className:"language-c"},'#include <stdlib.h>\n#include <sqlite3.h>\n\n#include "todo.h"\n\nint main(int argc, char **argv)\n{\n  /* Note: not exactly world class error handling but that isn\'t the point */\n\n  // create a db\n  sqlite3 *db;\n  int rc = sqlite3_open(":memory:", &db);\n  if (rc != SQLITE_OK) {\n    exit(1);\n  }\n\n  // make schema if needed (in memory databases always begin empty)\n  rc = todo_create_tables(db);\n   if (rc != SQLITE_OK) {\n    exit(2);\n  }\n\n  // add some tasks\n  const char * const default_tasks[] = {\n    "Buy milk",\n    "Walk dog",\n    "Write code"\n  };\n\n  for (int i = 0; i < 3; i++) {\n    // note we make a string reference from a c string here\n    cql_string_ref dtask = cql_string_ref_new(default_tasks[i]);\n    rc = todo_add(db, dtask);\n    cql_string_release(dtask); // and then dispose of the reference\n    if (rc != SQLITE_OK) {\n      exit(3);\n    }\n  }\n\n  // mark a task as done\n  rc = todo_setdone_(db, 1, true);\n  if (rc != SQLITE_OK) {\n    exit(4);\n  }\n\n  // delete a row in the middle, rowid = 2\n  rc = todo_delete(db, 2);\n  if (rc != SQLITE_OK) {\n    exit(5);\n  }\n\n  // select out some results\n  todo_tasks_result_set_ref result_set;\n  rc = todo_tasks_fetch_results(db, &result_set);\n  if (rc != SQLITE_OK) {\n    printf("error: %d\\n", rc);\n    exit(6);\n  }\n\n  // get result count\n  cql_int32 result_count = todo_tasks_result_count(result_set);\n\n  // loop to print\n  for (cql_int32 row = 0; row < result_count; row++) {\n    // note "get" semantics mean that a ref count is not added\n    // if you want to keep the string you must "retain" it\n    cql_string_ref text = todo_tasks_get_description(result_set, row);\n    cql_bool done = todo_tasks_get_done(result_set, row);\n    cql_int32 rowid = todo_tasks_get_rowid(result_set, row);\n\n    // convert to c string format\n    cql_alloc_cstr(ctext, text);\n    printf("%d: rowid:%d %s (%s)\\n",\n      row, rowid, ctext, done ? "done" : "not done");\n    cql_free_cstr(ctext, text);\n  }\n\n  // done with results, free the lot\n  cql_result_set_release(result_set);\n\n  // and close the database\n  sqlite3_close(db);\n}\n')),(0,s.mdx)("h3",{id:"build-steps"},"Build Steps"),(0,s.mdx)("pre",null,(0,s.mdx)("code",{parentName:"pre",className:"language-sh"},"# ${cgsql} refers to the root of the CG/SQL repo\n% cql --in todo.sql --cg todo.h todo.c\n% cc -o todo -I${cqsql}/sources main.c todo.c ${cgsql}/sources/cqlrt.c -lsqlite3\n")),(0,s.mdx)("h3",{id:"results"},"Results"),(0,s.mdx)("p",null,"Note that rowid 2 has been deleted, the leading number is the index in\nthe result set. The rowid is of course the database rowid."),(0,s.mdx)("pre",null,(0,s.mdx)("code",{parentName:"pre"},"% ./todo\n0: rowid:1 Buy milk (done)\n1: rowid:3 Write code (not done)\n")))}m.isMDXComponent=!0}}]);
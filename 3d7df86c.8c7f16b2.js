(window.webpackJsonp=window.webpackJsonp||[]).push([[26],{148:function(n,t,e){"use strict";e.d(t,"a",(function(){return p})),e.d(t,"b",(function(){return T}));var _=e(0),a=e.n(_);function r(n,t,e){return t in n?Object.defineProperty(n,t,{value:e,enumerable:!0,configurable:!0,writable:!0}):n[t]=e,n}function s(n,t){var e=Object.keys(n);if(Object.getOwnPropertySymbols){var _=Object.getOwnPropertySymbols(n);t&&(_=_.filter((function(t){return Object.getOwnPropertyDescriptor(n,t).enumerable}))),e.push.apply(e,_)}return e}function i(n){for(var t=1;t<arguments.length;t++){var e=null!=arguments[t]?arguments[t]:{};t%2?s(Object(e),!0).forEach((function(t){r(n,t,e[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(n,Object.getOwnPropertyDescriptors(e)):s(Object(e)).forEach((function(t){Object.defineProperty(n,t,Object.getOwnPropertyDescriptor(e,t))}))}return n}function o(n,t){if(null==n)return{};var e,_,a=function(n,t){if(null==n)return{};var e,_,a={},r=Object.keys(n);for(_=0;_<r.length;_++)e=r[_],t.indexOf(e)>=0||(a[e]=n[e]);return a}(n,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(n);for(_=0;_<r.length;_++)e=r[_],t.indexOf(e)>=0||Object.prototype.propertyIsEnumerable.call(n,e)&&(a[e]=n[e])}return a}var m=a.a.createContext({}),E=function(n){var t=a.a.useContext(m),e=t;return n&&(e="function"==typeof n?n(t):i(i({},t),n)),e},p=function(n){var t=E(n.components);return a.a.createElement(m.Provider,{value:t},n.children)},l={inlineCode:"code",wrapper:function(n){var t=n.children;return a.a.createElement(a.a.Fragment,{},t)}},c=a.a.forwardRef((function(n,t){var e=n.components,_=n.mdxType,r=n.originalType,s=n.parentName,m=o(n,["components","mdxType","originalType","parentName"]),p=E(e),c=_,T=p["".concat(s,".").concat(c)]||p[c]||l[c]||r;return e?a.a.createElement(T,i(i({ref:t},m),{},{components:e})):a.a.createElement(T,i({ref:t},m))}));function T(n,t){var e=arguments,_=t&&t.mdxType;if("string"==typeof n||_){var r=e.length,s=new Array(r);s[0]=c;var i={};for(var o in t)hasOwnProperty.call(t,o)&&(i[o]=t[o]);i.originalType=n,i.mdxType="string"==typeof n?n:_,s[1]=i;for(var m=2;m<r;m++)s[m]=e[m];return a.a.createElement.apply(null,s)}return a.a.createElement.apply(null,e)}c.displayName="MDXCreateElement"},81:function(n,t,e){"use strict";e.r(t),e.d(t,"frontMatter",(function(){return i})),e.d(t,"metadata",(function(){return o})),e.d(t,"rightToc",(function(){return m})),e.d(t,"default",(function(){return p}));var _=e(2),a=e(6),r=(e(0),e(148)),s=["components"],i={id:"x2",title:"Appendix 2: CQL Grammar",sidebar_label:"Appendix 2: CQL Grammar"},o={unversionedId:"x2",id:"x2",isDocsHomePage:!1,title:"Appendix 2: CQL Grammar",description:"\x3c!---",source:"@site/../CQL_Guide/x2.md",slug:"/x2",permalink:"/cql-guide/x2",version:"current",lastUpdatedBy:"Rico Mariani",lastUpdatedAt:1637796763,sidebar_label:"Appendix 2: CQL Grammar",sidebar:"someSidebar",previous:{title:"Appendix 1: Command Line Options",permalink:"/cql-guide/x1"},next:{title:"Appendix 3: Control Directives",permalink:"/cql-guide/x3"}},m=[{value:"Operators and Literals",id:"operators-and-literals",children:[]},{value:"Statement/Type Keywords",id:"statementtype-keywords",children:[]},{value:"Rules",id:"rules",children:[]}],E={rightToc:m};function p(n){var t=n.components,e=Object(a.a)(n,s);return Object(r.b)("wrapper",Object(_.a)({},E,e,{components:t,mdxType:"MDXLayout"}),Object(r.b)("p",null,"What follows is taken from a grammar snapshot with the tree building rules removed.\nIt should give a fair sense of the syntax of CQL (but not semantic validation)."),Object(r.b)("p",null,"Snapshot as of Fri Nov 19 15:20:18 PST 2021"),Object(r.b)("h3",{id:"operators-and-literals"},"Operators and Literals"),Object(r.b)("p",null,"These are in order of priority lowest to highest"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"UNION_ALL UNION INTERSECT EXCEPT\nASSIGN\nOR\nAND\nNOT\nBETWEEN NOT_BETWEEN '<>' '!=' '=' '==' LIKE NOT_LIKE GLOB NOT_GLOB MATCH NOT_MATCH REGEXP NOT_REGEXP IN NOT_IN IS_NOT IS IS_TRUE IS_FALSE IS_NOT_TRUE IS_NOT_FALSE\nISNULL NOTNULL\n'<' '>' '>=' '<='\n'<<' '>>' '&' '|'\n'+' '-'\n'*' '/' '%'\nCONCAT\nCOLLATE\nUMINUS '~'\n")),Object(r.b)("p",null,"NOTE: The above varies considerably from the C binding order!!!"),Object(r.b)("p",null,"Literals:"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},"ID  /* a name */\nSTRLIT /* a string literal in SQL format e.g. 'it''s sql' */\nCSTRLIT /* a string literal in C format e.g. \"hello, world\\n\" */\nBLOBLIT /* a blob literal in SQL format e.g. x'12ab' */\nINTLIT /* integer literal */\nLONGLIT /* long integer literal */\nREALLIT /* floating point literal */\n")),Object(r.b)("h3",{id:"statementtype-keywords"},"Statement/Type Keywords"),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},'"@ATTRIBUTE" "@BEGIN_SCHEMA_REGION" "@CREATE"\n"@DECLARE_DEPLOYABLE_REGION" "@DECLARE_SCHEMA_REGION"\n"@DELETE" "@DUMMY_SEED" "@ECHO" "@EMIT_CONSTANTS"\n"@EMIT_ENUMS" "@END_SCHEMA_REGION" "@ENFORCE_NORMAL"\n"@ENFORCE_POP" "@ENFORCE_PUSH" "@ENFORCE_RESET"\n"@ENFORCE_STRICT" "@FILE" "@PREVIOUS_SCHEMA" "@PROC" "@RC"\n"@RECREATE" "@SCHEMA_AD_HOC_MIGRATION"\n"@SCHEMA_UPGRADE_SCRIPT" "@SCHEMA_UPGRADE_VERSION"\n"@SENSITIVE" "ABORT" "ACTION" "ADD" "AFTER" "ALL" "ALTER"\n"ARGUMENTS" "AS" "ASC" "AUTOINCREMENT" "BEFORE" "BEGIN"\n"BLOB" "BY" "CALL" "CASCADE" "CASE" "CAST" "CATCH" "CHECK"\n"CLOSE" "COLUMN" "COMMIT" "CONST" "CONSTRAINT" "CONTEXT\nCOLUMN" "CONTEXT TYPE" "CONTINUE" "CREATE" "CROSS" "CURRENT\nROW" "CURSOR" "DECLARE" "DEFAULT" "DEFERRABLE" "DEFERRED"\n"DELETE" "DESC" "DISTINCT" "DISTINCTROW" "DO" "DROP" "ELSE\nIF" "ELSE" "ENCODE" "END" "ENUM" "EXCLUDE CURRENT ROW"\n"EXCLUDE GROUP" "EXCLUDE NO OTHERS" "EXCLUDE TIES"\n"EXCLUSIVE" "EXISTS" "EXPLAIN" "FAIL" "FETCH" "FILTER"\n"FOLLOWING" "FOR EACH ROW" "FOR" "FOREIGN" "FROM" "FUNC"\n"FUNCTION" "GROUP" "GROUPS" "HAVING" "HIDDEN" "IF" "IGNORE"\n"IMMEDIATE" "INDEX" "INITIALLY" "INNER" "INOUT" "INSERT"\n"INSTEAD" "INT" "INTEGER" "INTO" "JOIN" "KEY" "LEAVE"\n"LEFT" "LET" "LIMIT" "LONG" "LONG_INT" "LONG_INTEGER"\n"LOOP" "NO" "NOT DEFERRABLE" "NOTHING" "NULL" "OBJECT" "OF"\n"OFFSET" "ON CONFLICT" "ON" "OPEN" "ORDER" "OUT" "OUTER"\n"OVER" "PARTITION" "PRECEDING" "PRIMARY" "PRIVATE" "PROC"\n"PROCEDURE" "QUERY PLAN" "RAISE" "RANGE" "REAL" "RECURSIVE"\n"REFERENCES" "RELEASE" "RENAME" "REPLACE" "RESTRICT"\n"RETURN" "RIGHT" "ROLLBACK" "ROWID" "ROWS" "SAVEPOINT"\n"SELECT" "SET" "STATEMENT" "SWITCH" "TABLE" "TEMP" "TEXT"\n"THEN" "THROW" "TO" "TRANSACTION" "TRIGGER" "TRY" "TYPE"\n"UNBOUNDED" "UNIQUE" "UPDATE" "UPSERT" "USING" "VALUES"\n"VIEW" "VIRTUAL" "WHEN" "WHERE" "WHILE" "WINDOW" "WITH"\n"WITHOUT"\n')),Object(r.b)("h3",{id:"rules"},"Rules"),Object(r.b)("p",null,"Note that in many cases the grammar is more generous than the overall language and errors have to be checked on top of this, often this is done on purpose because even when it's possible it might be very inconvenient to do checks with syntax.  For example the grammar cannot enforce non-duplicate ids in id lists, but it could enforce non-duplicate attributes in attribute lists.  It chooses to do neither as they are easily done with semantic validation.  Thus the grammar is not the final authority on what constitutes a valid program but it's a good start."),Object(r.b)("pre",null,Object(r.b)("code",{parentName:"pre"},'\n\nprogram:\n  opt_stmt_list\n  ;\n\nopt_stmt_list:\n  /*nil*/\n  | stmt_list\n  ;\n\nstmt_list:\n  stmt \';\'\n  | stmt_list stmt \';\'\n  ;\n\nstmt:\n  misc_attrs any_stmt\n  ;\n\nany_stmt:\n    alter_table_add_column_stmt\n  | begin_schema_region_stmt\n  | begin_trans_stmt\n  | call_stmt\n  | close_stmt\n  | commit_return_stmt\n  | commit_trans_stmt\n  | continue_stmt\n  | create_index_stmt\n  | create_proc_stmt\n  | create_table_stmt\n  | create_trigger_stmt\n  | create_view_stmt\n  | create_virtual_table_stmt\n  | declare_deployable_region_stmt\n  | declare_enum_stmt\n  | declare_const_stmt\n  | declare_func_stmt\n  | declare_out_call_stmt\n  | declare_proc_no_check_stmt\n  | declare_proc_stmt\n  | declare_schema_region_stmt\n  | declare_stmt\n  | delete_stmt\n  | drop_index_stmt\n  | drop_table_stmt\n  | drop_trigger_stmt\n  | drop_view_stmt\n  | echo_stmt\n  | emit_enums_stmt\n  | emit_constants_stmt\n  | end_schema_region_stmt\n  | enforce_normal_stmt\n  | enforce_pop_stmt\n  | enforce_push_stmt\n  | enforce_reset_stmt\n  | enforce_strict_stmt\n  | explain_stmt\n  | fetch_call_stmt\n  | fetch_stmt\n  | fetch_values_stmt\n  | guard_stmt\n  | if_stmt\n  | insert_stmt\n  | leave_stmt\n  | let_stmt\n  | loop_stmt\n  | open_stmt\n  | out_stmt\n  | out_union_stmt\n  | previous_schema_stmt\n  | proc_savepoint_stmt\n  | release_savepoint_stmt\n  | return_stmt\n  | rollback_return_stmt\n  | rollback_trans_stmt\n  | savepoint_stmt\n  | select_stmt\n  | schema_ad_hoc_migration_stmt\n  | schema_upgrade_script_stmt\n  | schema_upgrade_version_stmt\n  | set_stmt\n  | switch_stmt\n  | throw_stmt\n  | trycatch_stmt\n  | update_cursor_stmt\n  | update_stmt\n  | upsert_stmt\n  | while_stmt\n  | with_delete_stmt\n  | with_insert_stmt\n  | with_update_stmt\n  | with_upsert_stmt\n  ;\n\nexplain_stmt:\n  "EXPLAIN" opt_query_plan explain_target\n  ;\n\nopt_query_plan:\n  /* nil */\n  | "QUERY PLAN"\n  ;\n\nexplain_target: select_stmt\n  | update_stmt\n  | delete_stmt\n  | with_delete_stmt\n  | with_insert_stmt\n  | insert_stmt\n  | upsert_stmt\n  | drop_table_stmt\n  | drop_view_stmt\n  | drop_index_stmt\n  | drop_trigger_stmt\n  | begin_trans_stmt\n  | commit_trans_stmt\n  ;\n\nprevious_schema_stmt:\n  "@PREVIOUS_SCHEMA"\n  ;\n\nschema_upgrade_script_stmt:\n  "@SCHEMA_UPGRADE_SCRIPT"\n  ;\n\nschema_upgrade_version_stmt:\n  "@SCHEMA_UPGRADE_VERSION" \'(\' "integer-literal" \')\'\n  ;\n\nset_stmt:\n  "SET" name ":=" expr\n  | "SET" name "FROM" "CURSOR" name\n  ;\n\nlet_stmt:\n  "LET" name ":=" expr\n  ;\n\nversion_attrs_opt_recreate:\n  /* nil */\n  | "@RECREATE"\n  | "@RECREATE" \'(\' name \')\'\n  | version_attrs\n  ;\n\nopt_version_attrs:\n  /* nil */\n  | version_attrs\n  ;\n\nversion_attrs:\n  "@CREATE" version_annotation opt_version_attrs\n  | "@DELETE" version_annotation opt_version_attrs\n  ;\n\nopt_delete_version_attr:\n  /* nil */\n  | "@DELETE" version_annotation\n  ;\n\ndrop_table_stmt:\n  "DROP" "TABLE" "IF" "EXISTS" name\n  | "DROP" "TABLE" name\n  ;\n\ndrop_view_stmt:\n  "DROP" "VIEW" "IF" "EXISTS" name\n  | "DROP" "VIEW" name\n  ;\n\ndrop_index_stmt:\n  "DROP" "INDEX" "IF" "EXISTS" name\n  | "DROP" "INDEX" name\n  ;\n\ndrop_trigger_stmt:\n  "DROP" "TRIGGER" "IF" "EXISTS" name\n  | "DROP" "TRIGGER" name\n  ;\n\ncreate_virtual_table_stmt: "CREATE" "VIRTUAL" "TABLE" opt_if_not_exists name\n                           "USING" name opt_module_args\n                           "AS" \'(\' col_key_list \')\' opt_delete_version_attr ;\n\nopt_module_args: /* nil */\n  | \'(\' misc_attr_value_list \')\'\n  | \'(\' "ARGUMENTS" "FOLLOWING" \')\'\n  ;\n\ncreate_table_stmt:\n  "CREATE" opt_temp "TABLE" opt_if_not_exists name \'(\' col_key_list \')\' opt_no_rowid version_attrs_opt_recreate\n  ;\n\nopt_temp:\n  /* nil */\n  | "TEMP"\n  ;\n\nopt_if_not_exists:\n  /* nil */\n  | "IF" "NOT" "EXISTS"\n  ;\n\nopt_no_rowid:\n  /* nil */\n  | "WITHOUT" "ROWID"\n  ;\n\ncol_key_list:\n  col_key_def\n  | col_key_def \',\' col_key_list\n  ;\n\ncol_key_def:\n  col_def\n  | pk_def\n  | fk_def\n  | unq_def\n  | check_def\n  | shape_def\n  ;\n\ncheck_def:\n  "CONSTRAINT" name "CHECK" \'(\' expr \')\'\n  | "CHECK" \'(\' expr \')\'\n  ;\n\nshape_def:\n    "LIKE" name\n  | "LIKE" name "ARGUMENTS"\n  ;\n\ncol_name:\n  name\n  ;\n\nmisc_attr_key:\n  name\n  | name \':\' name\n  ;\n\nmisc_attr_value_list:\n  misc_attr_value\n  | misc_attr_value \',\' misc_attr_value_list\n  ;\n\nmisc_attr_value:\n  name\n  | any_literal\n  | const_expr\n  | \'(\' misc_attr_value_list \')\'\n  | \'-\' num_literal\n  ;\n\nmisc_attr:\n  "@ATTRIBUTE" \'(\' misc_attr_key \')\'\n  | "@ATTRIBUTE" \'(\' misc_attr_key \'=\' misc_attr_value \')\'\n  ;\n\nmisc_attrs:\n  /* nil */\n  | misc_attr misc_attrs\n  ;\n\ncol_def:\n  misc_attrs col_name data_type_any col_attrs\n  ;\n\npk_def:\n  "CONSTRAINT" name "PRIMARY" "KEY" \'(\' indexed_columns \')\' opt_conflict_clause\n  | "PRIMARY" "KEY" \'(\' indexed_columns \')\' opt_conflict_clause\n  ;\n\nopt_conflict_clause:\n  /* nil */\n  | conflict_clause\n  ;\n\nconflict_clause:\n  "ON CONFLICT" "ROLLBACK"\n  | "ON CONFLICT" "ABORT"\n  | "ON CONFLICT" "FAIL"\n  | "ON CONFLICT" "IGNORE"\n  | "ON CONFLICT" "REPLACE"\n  ;\n\nopt_fk_options:\n  /* nil */\n  | fk_options\n  ;\n\nfk_options:\n  fk_on_options\n  | fk_deferred_options\n  | fk_on_options fk_deferred_options\n  ;\n\nfk_on_options:\n  "ON" "DELETE" fk_action\n  | "ON" "UPDATE" fk_action\n  | "ON" "UPDATE" fk_action "ON" "DELETE" fk_action\n  | "ON" "DELETE" fk_action "ON" "UPDATE" fk_action\n  ;\n\nfk_action:\n  "SET" "NULL"\n  | "SET" "DEFAULT"\n  | "CASCADE"\n  | "RESTRICT"\n  | "NO" "ACTION"\n  ;\n\nfk_deferred_options:\n  "DEFERRABLE" fk_initial_state\n  | "NOT DEFERRABLE" fk_initial_state\n  ;\n\nfk_initial_state:\n  /* nil */\n  | "INITIALLY" "DEFERRED"\n  | "INITIALLY" "IMMEDIATE"\n  ;\n\nfk_def:\n  "CONSTRAINT" name "FOREIGN" "KEY" \'(\' name_list \')\' fk_target_options\n  | "FOREIGN" "KEY" \'(\' name_list \')\' fk_target_options\n  ;\n\nfk_target_options:\n  "REFERENCES" name \'(\' name_list \')\' opt_fk_options\n  ;\n\nunq_def:\n  "CONSTRAINT" name "UNIQUE" \'(\' indexed_columns \')\' opt_conflict_clause\n  | "UNIQUE" \'(\' indexed_columns \')\' opt_conflict_clause\n  ;\n\nopt_unique:\n  /* nil */\n  | "UNIQUE"\n  ;\n\nindexed_column:\n  expr opt_asc_desc\n  ;\n\nindexed_columns:\n  indexed_column\n  | indexed_column \',\' indexed_columns\n  ;\n\ncreate_index_stmt:\n  "CREATE" opt_unique "INDEX" opt_if_not_exists name "ON" name \'(\' indexed_columns \')\' opt_where opt_delete_version_attr\n  ;\n\nname:\n  "ID"\n  | "TEXT"\n  | "TRIGGER"\n  | "ROWID"\n  | "REPLACE"\n  | "KEY"\n  | "VIRTUAL"\n  | "TYPE"\n  | "HIDDEN"\n  | "PRIVATE"\n  ;\n\nopt_name:\n  /* nil */\n  | name\n  ;\n\nname_list:\n  name\n  |  name \',\' name_list\n  ;\n\nopt_name_list:\n  /* nil */\n  | name_list\n  ;\n\ncol_attrs:\n  /* nil */\n  | "NOT" "NULL" opt_conflict_clause col_attrs\n  | "PRIMARY" "KEY" opt_conflict_clause col_attrs\n  | "PRIMARY" "KEY" opt_conflict_clause "AUTOINCREMENT" col_attrs\n  | "DEFAULT" \'-\' num_literal col_attrs\n  | "DEFAULT" num_literal col_attrs\n  | "DEFAULT" const_expr col_attrs\n  | "DEFAULT" str_literal col_attrs\n  | "COLLATE" name col_attrs\n  | "CHECK" \'(\' expr \')\' col_attrs\n  | "UNIQUE" opt_conflict_clause col_attrs\n  | "HIDDEN" col_attrs\n  | "@SENSITIVE" col_attrs\n  | "@CREATE" version_annotation col_attrs\n  | "@DELETE" version_annotation col_attrs\n  | fk_target_options col_attrs\n  ;\n\nversion_annotation:\n  \'(\' "integer-literal" \',\' name \')\'\n  | \'(\' "integer-literal" \',\' name \':\' name \')\'\n  | \'(\' "integer-literal" \')\'\n  ;\n\nopt_kind:\n  /* nil */\n  | \'<\' name \'>\'\n  ;\n\ndata_type_numeric:\n  "INT" opt_kind\n  | "INTEGER" opt_kind\n  | "REAL" opt_kind\n  | "LONG" opt_kind\n  | "BOOL" opt_kind\n  | "LONG" "INTEGER" opt_kind\n  | "LONG" "INT" opt_kind\n  | "LONG_INT" opt_kind\n  | "LONG_INTEGER" opt_kind\n  ;\n\ndata_type_any:\n  data_type_numeric\n  | "TEXT"  opt_kind\n  | "BLOB"  opt_kind\n  | "OBJECT" opt_kind\n  | "OBJECT" \'<\' name "CURSOR" \'>\'\n  | "ID"\n  ;\n\ndata_type_with_options:\n  data_type_any\n  | data_type_any "NOT" "NULL"\n  | data_type_any "@SENSITIVE"\n  | data_type_any "@SENSITIVE" "NOT" "NULL"\n  | data_type_any "NOT" "NULL" "@SENSITIVE"\n  ;\n\nstr_literal:\n  "sql-string-literal"\n  | "c-string-literal"\n  ;\n\nnum_literal:\n  "integer-literal"\n  | "long-literal"\n  | "real-literal"\n  | "TRUE"\n  | "FALSE"\n  ;\n\nconst_expr:\n  "CONST" \'(\' expr \')\'\n  ;\n\nany_literal:\n  str_literal\n  | num_literal\n  | "NULL"\n  | "@FILE" \'(\' str_literal \')\'\n  | "@PROC"\n  | "sql-blob-literal"\n  ;\n\nraise_expr:\n  "RAISE" \'(\' "IGNORE" \')\'\n  | "RAISE" \'(\' "ROLLBACK" \',\'  expr \')\'\n  | "RAISE" \'(\' "ABORT" \',\'  expr \')\'\n  | "RAISE" \'(\' "FAIL" \',\'  expr \')\'\n  ;\n\ncall:\n  name \'(\' arg_list \')\' opt_filter_clause\n  | name \'(\' "DISTINCT" arg_list \')\' opt_filter_clause\n  ;\n\nbasic_expr:\n  name\n  | "@RC"\n  | name \'.\' name\n  | any_literal\n  | const_expr\n  | \'(\' expr \')\'\n  | call\n  | window_func_inv\n  | raise_expr\n  | \'(\' select_stmt \')\'\n  | \'(\' select_stmt "IF" "NOTHING" expr \')\'\n  | \'(\' select_stmt "IF" "NOTHING" "OR" "NULL" expr \')\'\n  | \'(\' select_stmt "IF" "NOTHING" "THROW"\')\'\n  | "EXISTS" \'(\' select_stmt \')\'\n  | "CASE" expr case_list "END"\n  | "CASE" expr case_list "ELSE" expr "END"\n  | "CASE" case_list "END"\n  | "CASE" case_list "ELSE" expr "END"\n  | "CAST" \'(\' expr "AS" data_type_any \')\'\n  ;\n\nmath_expr:\n  basic_expr\n  | math_expr \'&\' math_expr\n  | math_expr \'|\' math_expr\n  | math_expr "<<" math_expr\n  | math_expr ">>"  math_expr\n  | math_expr \'+\' math_expr\n  | math_expr \'-\' math_expr\n  | math_expr \'*\' math_expr\n  | math_expr \'/\' math_expr\n  | math_expr \'%\' math_expr\n  | math_expr "IS NOT TRUE"\n  | math_expr "IS NOT FALSE"\n  | math_expr "ISNULL"\n  | math_expr "NOTNULL"\n  | math_expr "IS TRUE"\n  | math_expr "IS FALSE"\n  | \'-\' math_expr\n  | \'~\' math_expr\n  | "NOT" math_expr\n  | math_expr \'=\' math_expr\n  | math_expr "==" math_expr\n  | math_expr \'<\' math_expr\n  | math_expr \'>\' math_expr\n  | math_expr "<>" math_expr\n  | math_expr "!=" math_expr\n  | math_expr ">=" math_expr\n  | math_expr "<=" math_expr\n  | math_expr "NOT IN" \'(\' expr_list \')\'\n  | math_expr "NOT IN" \'(\' select_stmt \')\'\n  | math_expr "IN" \'(\' expr_list \')\'\n  | math_expr "IN" \'(\' select_stmt \')\'\n  | math_expr "LIKE" math_expr\n  | math_expr "NOT LIKE" math_expr\n  | math_expr "MATCH" math_expr\n  | math_expr "NOT MATCH" math_expr\n  | math_expr "REGEXP" math_expr\n  | math_expr "NOT REGEXP" math_expr\n  | math_expr "GLOB" math_expr\n  | math_expr "NOT GLOB" math_expr\n  | math_expr "BETWEEN" math_expr "AND" math_expr\n  | math_expr "NOT BETWEEN" math_expr "AND" math_expr\n  | math_expr "IS NOT" math_expr\n  | math_expr "IS" math_expr\n  | math_expr "||" math_expr\n  | math_expr "COLLATE" name\n  ;\n\nexpr:\n  math_expr\n  | expr "AND" expr\n  | expr "OR" expr\n  ;\n\ncase_list:\n  "WHEN" expr "THEN" expr\n  | "WHEN" expr "THEN" expr case_list\n  ;\n\narg_expr: \'*\'\n  | expr\n  | shape_arguments\n  ;\n\narg_list:\n  /* nil */\n  | arg_expr\n  | arg_expr \',\' arg_list\n  ;\n\nexpr_list:\n  expr\n  | expr \',\' expr_list\n  ;\n\nshape_arguments:\n  "FROM" name\n  | "FROM" name shape_def\n  | "FROM" "ARGUMENTS"\n  | "FROM" "ARGUMENTS" shape_def\n  ;\n\ncall_expr:\n  expr\n  | shape_arguments\n  ;\n\ncall_expr_list:\n  call_expr\n  | call_expr \',\' call_expr_list\n  ;\n\ncte_tables:\n  cte_table\n  | cte_table \',\' cte_tables\n  ;\n\ncte_table:\n    name \'(\' name_list \')\' "AS" \'(\' select_stmt_no_with \')\'\n  | name \'(\' \'*\' \')\' "AS" \'(\' select_stmt_no_with \')\'\n  ;\n\nwith_prefix:\n  "WITH" cte_tables\n  | "WITH" "RECURSIVE" cte_tables\n  ;\n\nwith_select_stmt:\n  with_prefix select_stmt_no_with\n  ;\n\nselect_stmt:\n  with_select_stmt\n  | select_stmt_no_with\n  ;\n\nselect_stmt_no_with:\n  select_core_list opt_orderby opt_limit opt_offset\n  ;\n\nselect_core_list:\n  select_core\n  | select_core compound_operator select_core_list\n  ;\n\nvalues:\n  \'(\' insert_list \')\'\n  | \'(\' insert_list \')\' \',\' values\n  ;\n\nselect_core:\n  "SELECT" select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window\n  | "VALUES" values\n  ;\n\ncompound_operator:\n  "UNION"\n  | "UNION ALL"\n  | "INTERSECT"\n  | "EXCEPT"\n  ;\n\nwindow_func_inv:\n  name \'(\' arg_list \')\' opt_filter_clause "OVER" window_name_or_defn\n  ;\n\nopt_filter_clause:\n  /* nil */\n  | "FILTER" \'(\' opt_where \')\'\n  ;\n\nwindow_name_or_defn: window_defn\n  | name\n  ;\n\nwindow_defn:\n  \'(\' opt_partition_by opt_orderby opt_frame_spec \')\'\n  ;\n\nopt_frame_spec:\n  /* nil */\n  | frame_type frame_boundary_opts frame_exclude\n  ;\n\nframe_type:\n  "RANGE"\n  | "ROWS"\n  | "GROUPS"\n  ;\n\nframe_exclude:\n  /* nil */\n  | "EXCLUDE NO OTHERS"\n  | "EXCLUDE CURRENT ROW"\n  | "EXCLUDE GROUP"\n  | "EXCLUDE TIES"\n  ;\n\nframe_boundary_opts:\n  frame_boundary\n  | "BETWEEN" frame_boundary_start "AND" frame_boundary_end\n  ;\n\nframe_boundary_start:\n  "UNBOUNDED" "PRECEDING"\n  | expr "PRECEDING"\n  | "CURRENT ROW"\n  | expr "FOLLOWING"\n  ;\n\nframe_boundary_end:\n  expr "PRECEDING"\n  | "CURRENT ROW"\n  | expr "FOLLOWING"\n  | "UNBOUNDED" "FOLLOWING"\n  ;\n\nframe_boundary:\n  "UNBOUNDED" "PRECEDING"\n  | expr "PRECEDING"\n  | "CURRENT ROW"\n  ;\n\nopt_partition_by:\n  /* nil */\n  | "PARTITION" "BY" expr_list\n  ;\n\nopt_select_window:\n  /* nil */\n  | window_clause\n  ;\n\nwindow_clause:\n  "WINDOW" window_name_defn_list\n  ;\n\nwindow_name_defn_list:\n  window_name_defn\n  | window_name_defn \',\' window_name_defn_list\n  ;\n\nwindow_name_defn:\n  name "AS" window_defn\n  ;\n\nregion_spec:\n    name\n  | name "PRIVATE"\n  ;\n\nregion_list:\n  region_spec \',\' region_list\n  | region_spec\n  ;\n\ndeclare_schema_region_stmt:\n  "@DECLARE_SCHEMA_REGION" name\n  | "@DECLARE_SCHEMA_REGION" name "USING" region_list\n  ;\n\ndeclare_deployable_region_stmt:\n  "@DECLARE_DEPLOYABLE_REGION"  name\n  | "@DECLARE_DEPLOYABLE_REGION" name "USING" region_list\n  ;\n\nbegin_schema_region_stmt:\n  "@BEGIN_SCHEMA_REGION" name\n  ;\n\nend_schema_region_stmt:\n  "@END_SCHEMA_REGION"\n  ;\n\nschema_ad_hoc_migration_stmt:\n  "@SCHEMA_AD_HOC_MIGRATION" version_annotation\n\n  | "@SCHEMA_AD_HOC_MIGRATION" "FOR" "@RECREATE" \'(\' name \',\' name \')\'\n\n  ;\n\nemit_enums_stmt:\n  "@EMIT_ENUMS" opt_name_list\n  ;\n\nemit_constants_stmt:\n  "@EMIT_CONSTANTS" name_list\n  ;\n\nopt_from_query_parts:\n  /* nil */\n  | "FROM" query_parts\n  ;\n\nopt_where:\n  /* nil */\n  | "WHERE" expr\n  ;\n\nopt_groupby:\n  /* nil */\n  | "GROUP" "BY" groupby_list\n  ;\n\ngroupby_list:\n  groupby_item\n  | groupby_item \',\' groupby_list\n  ;\n\ngroupby_item:\n  expr opt_asc_desc\n  ;\n\nopt_asc_desc:\n  /* nil */\n  | "ASC"\n  | "DESC"\n  ;\n\nopt_having:\n  /* nil */\n  | "HAVING" expr\n  ;\n\nopt_orderby:\n  /* nil */\n  | "ORDER" "BY" groupby_list\n  ;\n\nopt_limit:\n  /* nil */\n  | "LIMIT" expr\n  ;\n\nopt_offset:\n  /* nil */\n  | "OFFSET" expr\n  ;\n\nselect_opts:\n  /* nil */\n  | "ALL"\n  | "DISTINCT"\n  | "DISTINCTROW"\n  ;\n\nselect_expr_list:\n  select_expr\n  | select_expr \',\' select_expr_list\n  | \'*\'\n  ;\n\nselect_expr:\n  expr opt_as_alias\n  |  name \'.\' \'*\'\n  ;\n\nopt_as_alias:\n  /* nil */\n  | as_alias\n  ;\n\nas_alias:\n  "AS" name\n  | name\n  ;\n\nquery_parts:\n  table_or_subquery_list\n  | join_clause\n  ;\n\ntable_or_subquery_list:\n  table_or_subquery\n  | table_or_subquery \',\' table_or_subquery_list\n  ;\n\njoin_clause:\n  table_or_subquery join_target_list\n  ;\n\njoin_target_list:\n  join_target\n  | join_target join_target_list\n  ;\n\ntable_or_subquery:\n  name opt_as_alias\n  | \'(\' select_stmt \')\' opt_as_alias\n  | table_function opt_as_alias\n  | \'(\' query_parts \')\'\n  ;\n\njoin_type:\n  /*nil */\n  | "LEFT"\n  | "RIGHT"\n  | "LEFT" "OUTER"\n  | "RIGHT" "OUTER"\n  | "INNER"\n  | "CROSS"\n  ;\n\njoin_target: join_type "JOIN" table_or_subquery opt_join_cond\n  ;\n\nopt_join_cond:\n  /* nil */\n  | join_cond\n  ;\n\njoin_cond:\n  "ON" expr\n  | "USING" \'(\' name_list \')\'\n  ;\n\ntable_function:\n  name \'(\' arg_list \')\'\n  ;\n\ncreate_view_stmt:\n  "CREATE" opt_temp "VIEW" opt_if_not_exists name "AS" select_stmt opt_delete_version_attr\n  ;\n\nwith_delete_stmt:\n  with_prefix delete_stmt\n  ;\n\ndelete_stmt:\n  "DELETE" "FROM" name opt_where\n  ;\n\nopt_insert_dummy_spec:\n  /*nil*/\n  | "@DUMMY_SEED" \'(\' expr \')\' dummy_modifier\n  ;\n\ndummy_modifier:\n  /* nil */\n  | "@DUMMY_NULLABLES"\n  | "@DUMMY_DEFAULTS"\n  | "@DUMMY_NULLABLES" "@DUMMY_DEFAULTS"\n  | "@DUMMY_DEFAULTS" "@DUMMY_NULLABLES"\n  ;\n\ninsert_stmt_type:\n  "INSERT" "INTO"\n  | "INSERT" "OR" "REPLACE" "INTO"\n  | "INSERT" "OR" "IGNORE" "INTO"\n  | "INSERT" "OR" "ROLLBACK" "INTO"\n  | "INSERT" "OR" "ABORT" "INTO"\n  | "INSERT" "OR" "FAIL" "INTO"\n  | "REPLACE" "INTO"\n  ;\n\nwith_insert_stmt:\n  with_prefix insert_stmt\n  ;\n\nopt_column_spec:\n  /* nil */\n  | \'(\' opt_name_list \')\'\n  | \'(\' shape_def \')\'\n  ;\n\nfrom_shape:\n  "FROM" "CURSOR" name opt_column_spec\n  | "FROM" name opt_column_spec\n  | "FROM" "ARGUMENTS" opt_column_spec\n  ;\n\ninsert_stmt:\n  insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec\n  | insert_stmt_type name opt_column_spec from_shape opt_insert_dummy_spec\n  | insert_stmt_type name "DEFAULT" "VALUES"\n  | insert_stmt_type name "USING" select_stmt\n  | insert_stmt_type name "USING" expr_names opt_insert_dummy_spec\n  ;\n\ninsert_list:\n  /* nil */\n  | expr\n  | expr \',\' insert_list\n  ;\n\nbasic_update_stmt:\n  "UPDATE" opt_name "SET" update_list opt_where\n  ;\n\nwith_update_stmt:\n  with_prefix update_stmt\n  ;\n\nupdate_stmt:\n  "UPDATE" name "SET" update_list opt_where opt_orderby opt_limit\n  ;\n\nupdate_entry:\n  name \'=\' expr\n  ;\n\nupdate_list:\n  update_entry\n  | update_entry \',\' update_list\n  ;\n\nwith_upsert_stmt:\n  with_prefix upsert_stmt\n  ;\n\nupsert_stmt:\n  insert_stmt "ON CONFLICT" conflict_target "DO" "NOTHING"\n  | insert_stmt "ON CONFLICT" conflict_target "DO" basic_update_stmt\n  ;\n\nupdate_cursor_stmt:\n  "UPDATE" "CURSOR" name opt_column_spec "FROM" "VALUES" \'(\' insert_list \')\'\n  | "UPDATE" "CURSOR" name opt_column_spec from_shape\n  | "UPDATE" "CURSOR" name "USING" expr_names\n  ;\n\nconflict_target:\n  /* nil */\n  | \'(\' indexed_columns \')\' opt_where\n  ;\n\nfunction: "FUNC" | "FUNCTION"\n  ;\n\ndeclare_out_call_stmt:\n  "DECLARE" "OUT" call_stmt\n  ;\n\ndeclare_enum_stmt:\n  "DECLARE" "ENUM" name data_type_numeric \'(\' enum_values \')\'\n  ;\n\nenum_values:\n    enum_value\n  | enum_value \',\' enum_values\n  ;\n\nenum_value:\n    name\n  | name \'=\' expr\n  ;\n\ndeclare_const_stmt:\n  "DECLARE" "CONST" "GROUP" name \'(\' const_values \')\'\n  ;\n\nconst_values:\n   const_value\n  | const_value \',\' const_values\n  ;\n\nconst_value:  name \'=\' expr\n  ;\n\ndeclare_func_stmt:\n  "DECLARE" function name \'(\' params \')\' data_type_with_options\n  | "DECLARE" "SELECT" function name \'(\' params \')\' data_type_with_options\n  | "DECLARE" function name \'(\' params \')\' "CREATE" data_type_with_options\n  | "DECLARE" "SELECT" function name \'(\' params \')\' \'(\' typed_names \')\'\n  ;\n\nprocedure: "PROC" | "PROCEDURE"\n  ;\n\ndeclare_proc_no_check_stmt:\n  "DECLARE" procedure name "NO" "CHECK"\n  ;\n\ndeclare_proc_stmt:\n  "DECLARE" procedure name \'(\' params \')\'\n  | "DECLARE" procedure name \'(\' params \')\' \'(\' typed_names \')\'\n  | "DECLARE" procedure name \'(\' params \')\' "USING" "TRANSACTION"\n  | "DECLARE" procedure name \'(\' params \')\' "OUT" \'(\' typed_names \')\'\n  | "DECLARE" procedure name \'(\' params \')\' "OUT" \'(\' typed_names \')\' "USING" "TRANSACTION"\n  | "DECLARE" procedure name \'(\' params \')\' "OUT" "UNION" \'(\' typed_names \')\'\n  | "DECLARE" procedure name \'(\' params \')\' "OUT" "UNION" \'(\' typed_names \')\' "USING" "TRANSACTION"\n  ;\n\ncreate_proc_stmt:\n  "CREATE" procedure name \'(\' params \')\' "BEGIN" opt_stmt_list "END"\n  ;\n\ninout:\n  "IN"\n  | "OUT"\n  | "INOUT"\n  ;\n\ntyped_name:\n  name data_type_with_options\n  | shape_def\n  | name shape_def\n  ;\n\ntyped_names:\n  typed_name\n  | typed_name \',\' typed_names\n  ;\n\nparam:\n  name data_type_with_options\n  | inout name data_type_with_options\n  | shape_def\n  | name shape_def\n  ;\n\nparams:\n  /* nil */\n  | param\n  |  param \',\' params\n  ;\n\ndeclare_stmt:\n  "DECLARE" name_list data_type_with_options\n  | "DECLARE" name "CURSOR" "FOR" select_stmt\n  | "DECLARE" name "CURSOR" "FOR" explain_stmt\n  | "DECLARE" name "CURSOR" "FOR" call_stmt\n  | "DECLARE" name "CURSOR" "FETCH" "FROM" call_stmt\n  | "DECLARE" name "CURSOR" shape_def\n  | "DECLARE" name "CURSOR" "LIKE" select_stmt\n  | "DECLARE" name "CURSOR" "FOR" name\n  | "DECLARE" name "TYPE" data_type_with_options\n  ;\n\ncall_stmt:\n  "CALL" name \'(\' \')\'\n  | "CALL" name \'(\' call_expr_list \')\'\n  ;\n\nwhile_stmt:\n  "WHILE" expr "BEGIN" opt_stmt_list "END"\n  ;\n\nswitch_stmt:\n  "SWITCH" expr switch_case switch_cases\n  | "SWITCH" expr "ALL" "VALUES" switch_case switch_cases\n  ;\n\nswitch_case:\n  "WHEN" expr_list "THEN" stmt_list\n  | "WHEN" expr_list "THEN" "NOTHING"\n  ;\n\nswitch_cases:\n  switch_case switch_cases\n  | "ELSE" stmt_list "END"\n  | "END"\n  ;\n\nloop_stmt:\n  "LOOP" fetch_stmt "BEGIN" opt_stmt_list "END"\n  ;\n\nleave_stmt:\n  "LEAVE"\n  ;\n\nreturn_stmt:\n  "RETURN"\n  ;\n\nrollback_return_stmt:\n  "ROLLBACK" "RETURN"\n  ;\n\ncommit_return_stmt:\n  "COMMIT" "RETURN"\n  ;\n\nthrow_stmt:\n  "THROW"\n  ;\n\ntrycatch_stmt:\n  "BEGIN" "TRY" opt_stmt_list "END" "TRY" \';\' "BEGIN" "CATCH" opt_stmt_list "END" "CATCH"\n  ;\n\ncontinue_stmt:\n  "CONTINUE"\n  ;\n\nfetch_stmt:\n  "FETCH" name "INTO" name_list\n  | "FETCH" name\n  ;\n\nfetch_values_stmt:\n  "FETCH" name opt_column_spec "FROM" "VALUES" \'(\' insert_list \')\' opt_insert_dummy_spec\n  | "FETCH" name opt_column_spec from_shape opt_insert_dummy_spec\n  | "FETCH" name "USING" expr_names opt_insert_dummy_spec\n  ;\n\nexpr_names:\n  expr_name\n  |  expr_name \',\' expr_names\n  ;\n\nexpr_name: expr as_alias\n  ;\n\nfetch_call_stmt:\n  "FETCH" name opt_column_spec "FROM" call_stmt\n  ;\n\nopen_stmt:\n  "OPEN" name\n  ;\n\nclose_stmt:\n  "CLOSE" name\n  ;\n\nout_stmt:\n  "OUT" name\n  ;\n\nout_union_stmt:\n  "OUT" "UNION" name\n  ;\n\nif_stmt:\n  "IF" expr "THEN" opt_stmt_list opt_elseif_list opt_else "END" "IF"\n  ;\n\nopt_else:\n  /* nil */\n  | "ELSE" opt_stmt_list\n  ;\n\nelseif_item:\n  "ELSE IF" expr "THEN" opt_stmt_list\n  ;\n\nelseif_list:\n  elseif_item\n  | elseif_item elseif_list\n  ;\n\nopt_elseif_list:\n  /* nil */\n  | elseif_list\n  ;\n\ncontrol_stmt:\n  commit_return_stmt\n  | continue_stmt\n  | leave_stmt\n  | return_stmt\n  | rollback_return_stmt\n  | throw_stmt\n\nguard_stmt:\n  "IF" expr control_stmt\n  ;\n\ntransaction_mode:\n  /* nil */\n  | "DEFERRED"\n  | "IMMEDIATE"\n  | "EXCLUSIVE"\n  ;\n\nbegin_trans_stmt:\n  "BEGIN" transaction_mode "TRANSACTION"\n  | "BEGIN" transaction_mode\n  ;\n\nrollback_trans_stmt:\n  "ROLLBACK"\n  | "ROLLBACK" "TRANSACTION"\n  | "ROLLBACK" "TO" savepoint_name\n  | "ROLLBACK" "TRANSACTION" "TO" savepoint_name\n  | "ROLLBACK" "TO" "SAVEPOINT" savepoint_name\n  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" savepoint_name\n  ;\n\ncommit_trans_stmt:\n  "COMMIT" "TRANSACTION"\n  | "COMMIT"\n  ;\n\nproc_savepoint_stmt:  procedure "SAVEPOINT" "BEGIN" opt_stmt_list "END"\n  ;\n\nsavepoint_name:\n  "@PROC"\n  | name\n  ;\n\nsavepoint_stmt:\n  "SAVEPOINT" savepoint_name\n  ;\n\nrelease_savepoint_stmt:\n  "RELEASE" savepoint_name\n  | "RELEASE" "SAVEPOINT" savepoint_name\n  ;\n\necho_stmt:\n  "@ECHO" name \',\' str_literal\n  ;\n\nalter_table_add_column_stmt:\n  "ALTER" "TABLE" name "ADD" "COLUMN" col_def\n  ;\n\ncreate_trigger_stmt:\n  "CREATE" opt_temp "TRIGGER" opt_if_not_exists trigger_def opt_delete_version_attr\n  ;\n\ntrigger_def:\n  name trigger_condition trigger_operation "ON" name trigger_action\n  ;\n\ntrigger_condition:\n  /* nil */\n  | "BEFORE"\n  | "AFTER"\n  | "INSTEAD" "OF"\n ;\n\ntrigger_operation:\n  "DELETE"\n  | "INSERT"\n  | "UPDATE" opt_of\n  ;\n\nopt_of:\n  /* nil */\n  | "OF" name_list\n  ;\n\ntrigger_action:\n  opt_foreachrow opt_when_expr "BEGIN" trigger_stmts "END"\n  ;\n\nopt_foreachrow:\n  /* nil */\n  | "FOR EACH ROW"\n  ;\n\nopt_when_expr:\n  /* nil */\n  | "WHEN" expr\n  ;\n\ntrigger_stmts:\n  trigger_stmt\n  | trigger_stmt  trigger_stmts\n  ;\n\ntrigger_stmt:\n  trigger_update_stmt \';\'\n  | trigger_insert_stmt \';\'\n  | trigger_delete_stmt \';\'\n  | trigger_select_stmt \';\'\n  ;\n\ntrigger_select_stmt:\n  select_stmt_no_with\n  ;\n\ntrigger_insert_stmt:\n  insert_stmt\n  ;\n\ntrigger_delete_stmt:\n  delete_stmt\n  ;\n\ntrigger_update_stmt:\n  basic_update_stmt\n  ;\n\nenforcement_options:\n  "FOREIGN" "KEY" "ON" "UPDATE"\n  | "FOREIGN" "KEY" "ON" "DELETE"\n  | "JOIN"\n  | "UPSERT" "STATEMENT"\n  | "WINDOW" function\n  | "WITHOUT" "ROWID"\n  | "TRANSACTION"\n  | "SELECT" "IF" "NOTHING"\n  | "INSERT" "SELECT"\n  | "TABLE" "FUNCTION"\n  | "ENCODE" "CONTEXT COLUMN"\n  | "ENCODE" "CONTEXT TYPE" "INTEGER"\n  | "ENCODE" "CONTEXT TYPE" "LONG_INTEGER"\n  | "ENCODE" "CONTEXT TYPE" "REAL"\n  | "ENCODE" "CONTEXT TYPE" "BOOL"\n  | "ENCODE" "CONTEXT TYPE" "TEXT"\n  | "ENCODE" "CONTEXT TYPE" "BLOB"\n  | "IS TRUE"\n  | "CAST"\n  | "NULL" "CHECK" "ON" "NOT" "NULL"\n  ;\n\nenforce_strict_stmt:\n  "@ENFORCE_STRICT" enforcement_options\n  ;\n\nenforce_normal_stmt:\n  "@ENFORCE_NORMAL" enforcement_options\n  ;\n\nenforce_reset_stmt:\n  "@ENFORCE_RESET"\n  ;\n\nenforce_push_stmt:\n  "@ENFORCE_PUSH"\n  ;\n\nenforce_pop_stmt:\n  "@ENFORCE_POP"\n  ;\n\n')))}p.isMDXComponent=!0}}]);
"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[8735],{3905:function(n,e,t){t.d(e,{Zo:function(){return L},kt:function(){return T}});var r=t(7294);function _(n,e,t){return e in n?Object.defineProperty(n,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):n[e]=t,n}function s(n,e){var t=Object.keys(n);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(n);e&&(r=r.filter((function(e){return Object.getOwnPropertyDescriptor(n,e).enumerable}))),t.push.apply(t,r)}return t}function o(n){for(var e=1;e<arguments.length;e++){var t=null!=arguments[e]?arguments[e]:{};e%2?s(Object(t),!0).forEach((function(e){_(n,e,t[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(n,Object.getOwnPropertyDescriptors(t)):s(Object(t)).forEach((function(e){Object.defineProperty(n,e,Object.getOwnPropertyDescriptor(t,e))}))}return n}function i(n,e){if(null==n)return{};var t,r,_=function(n,e){if(null==n)return{};var t,r,_={},s=Object.keys(n);for(r=0;r<s.length;r++)t=s[r],e.indexOf(t)>=0||(_[t]=n[t]);return _}(n,e);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(n);for(r=0;r<s.length;r++)t=s[r],e.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(n,t)&&(_[t]=n[t])}return _}var a=r.createContext({}),p=function(n){var e=r.useContext(a),t=e;return n&&(t="function"==typeof n?n(e):o(o({},e),n)),t},L=function(n){var e=p(n.components);return r.createElement(a.Provider,{value:e},n.children)},l={inlineCode:"code",wrapper:function(n){var e=n.children;return r.createElement(r.Fragment,{},e)}},u=r.forwardRef((function(n,e){var t=n.components,_=n.mdxType,s=n.originalType,a=n.parentName,L=i(n,["components","mdxType","originalType","parentName"]),u=p(t),T=_,c=u["".concat(a,".").concat(T)]||u[T]||l[T]||s;return t?r.createElement(c,o(o({ref:e},L),{},{components:t})):r.createElement(c,o({ref:e},L))}));function T(n,e){var t=arguments,_=e&&e.mdxType;if("string"==typeof n||_){var s=t.length,o=new Array(s);o[0]=u;var i={};for(var a in e)hasOwnProperty.call(e,a)&&(i[a]=e[a]);i.originalType=n,i.mdxType="string"==typeof n?n:_,o[1]=i;for(var p=2;p<s;p++)o[p]=t[p];return r.createElement.apply(null,o)}return r.createElement.apply(null,t)}u.displayName="MDXCreateElement"},1748:function(n,e,t){t.r(e),t.d(e,{assets:function(){return L},contentTitle:function(){return a},default:function(){return T},frontMatter:function(){return i},metadata:function(){return p},toc:function(){return l}});var r=t(7462),_=t(3366),s=(t(7294),t(3905)),o=["components"],i={id:"x5",title:"Appendix 5: JSON Schema Grammar",sidebar_label:"Appendix 5: JSON Schema Grammar"},a=void 0,p={unversionedId:"x5",id:"x5",title:"Appendix 5: JSON Schema Grammar",description:"\x3c!---",source:"@site/../CQL_Guide/x5.md",sourceDirName:".",slug:"/x5",permalink:"/cql-guide/x5",draft:!1,tags:[],version:"current",lastUpdatedBy:"Rico Mariani",lastUpdatedAt:1659116559,formattedLastUpdatedAt:"7/29/2022",frontMatter:{id:"x5",title:"Appendix 5: JSON Schema Grammar",sidebar_label:"Appendix 5: JSON Schema Grammar"},sidebar:"someSidebar",previous:{title:"Appendix 4: CQL Error Codes",permalink:"/cql-guide/x4"},next:{title:"Appendix 6: CQL In 20 Minutes",permalink:"/cql-guide/x6"}},L={},l=[{value:"Rules",id:"rules",level:3}],u={toc:l};function T(n){var e=n.components,t=(0,_.Z)(n,o);return(0,s.kt)("wrapper",(0,r.Z)({},u,t,{components:e,mdxType:"MDXLayout"}),(0,s.kt)("p",null,"What follows is taken from the JSON validation grammar with the tree building rules removed."),(0,s.kt)("p",null,"Snapshot as of Sun Jul 24 11:43:35 PDT 2022"),(0,s.kt)("h3",{id:"rules"},"Rules"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre"},"\n\njson_schema: '{'\n         '\"tables\"' ':' '[' opt_tables ']' ','\n         '\"virtualTables\"' ':' '[' opt_virtual_tables ']' ','\n         '\"views\"' ':' '[' opt_views ']' ','\n         '\"indices\"' ':' '[' opt_indices ']' ','\n         '\"triggers\"' ':' '[' opt_triggers ']' ','\n         '\"attributes\"' ':' '[' opt_attribute_list ']' ','\n         '\"queries\"' ':' '[' opt_queries ']' ','\n         '\"inserts\"' ':' '[' opt_inserts ']' ','\n         '\"generalInserts\"' ':' '[' opt_inserts_general ']' ','\n         '\"updates\"' ':' '[' opt_updates ']' ','\n         '\"deletes\"' ':' '[' opt_deletes ']' ','\n         '\"general\"' ':' '[' opt_generals ']' ','\n         '\"interfaces\"' ':' '[' opt_interfaces ']' ','\n         '\"regions\"' ':' '[' opt_regions ']' ','\n         '\"adHocMigrationProcs\"' ':' '[' opt_ad_hoc_migrations ']' ','\n         '\"enums\"' ':'  '[' opt_enums ']' ','\n         '\"constantGroups\"' ':'  '[' opt_const_groups ']' ','\n         '\"subscriptions\"' ':'  '[' opt_subscriptions ']'\n         '}'\n  ;\n\nBOOL_LITERAL: '0' | '1'\n  ;\n\nopt_tables: | tables\n  ;\n\ntables: table | table ',' tables\n  ;\n\ntable: '{'\n       '\"name\"' ':' STRING_LITERAL ','\n       '\"crc\"' ':' STRING_LITERAL ','\n       '\"isTemp\"' ':' BOOL_LITERAL ','\n       '\"ifNotExists\"' ':' BOOL_LITERAL ','\n       '\"withoutRowid\"' ':' BOOL_LITERAL ','\n       '\"isAdded\"' ':' BOOL_LITERAL ','\n       opt_added_version\n       '\"isDeleted\"' ':' BOOL_LITERAL ','\n       opt_deleted_version\n       '\"isRecreated\"' ':' BOOL_LITERAL ','\n       opt_recreate_group_name\n       opt_unsub_version\n       opt_resub_version\n       opt_region_info\n       opt_table_indices\n       opt_attributes\n       '\"columns\"' ':' '[' columns ']' ','\n       '\"primaryKey\"' ':' '[' opt_column_names ']' ','\n       '\"primaryKeySortOrders\"' ':' '[' opt_sort_order_names ']' ','\n       opt_primary_key_name\n       '\"foreignKeys\"' ':' '[' opt_foreign_keys ']' ','\n       '\"uniqueKeys\"' ':' '[' opt_unique_keys ']' ','\n       '\"checkExpressions\"' ':' '[' opt_check_expressions ']'\n       '}'\n  ;\n\nopt_primary_key_name:  | '\"primaryKeyName\"' ':' STRING_LITERAL ','\n  ;\n\nopt_virtual_tables: | virtual_tables\n  ;\n\nvirtual_tables: virtual_table | virtual_table ',' virtual_tables\n  ;\n\nvirtual_table: '{'\n       '\"name\"' ':' STRING_LITERAL ','\n       '\"crc\"' ':' STRING_LITERAL ','\n       '\"isTemp\"' ':' '0' ','\n       '\"ifNotExists\"' ':' BOOL_LITERAL ','\n       '\"withoutRowid\"' ':' BOOL_LITERAL ','\n       '\"isAdded\"' ':' BOOL_LITERAL ','\n       opt_added_version\n       '\"isDeleted\"' ':' BOOL_LITERAL ','\n       opt_deleted_version\n       '\"isRecreated\"' ':' BOOL_LITERAL ','\n       opt_region_info\n       '\"isVirtual\"' ':' '1' ','\n       '\"isEponymous\"' ':' BOOL_LITERAL ','\n       '\"module\"' ':' STRING_LITERAL ','\n       opt_module_args\n       opt_attributes\n       '\"columns\"' ':' '[' columns ']' ','\n       '\"primaryKey\"' ':' '[' opt_column_names ']' ','\n       '\"primaryKeySortOrders\"' ':' '[' opt_sort_order_names ']' ','\n       '\"foreignKeys\"' ':' '[' opt_foreign_keys ']' ','\n       '\"uniqueKeys\"' ':' '[' opt_unique_keys ']' ','\n       '\"checkExpressions\"' ':' '[' opt_check_expressions ']'\n       '}'\n  ;\n\nopt_module_args: | '\"moduleArgs\"' ':'  STRING_LITERAL ','\n  ;\n\nopt_added_version: | '\"addedVersion\"' ':' any_integer ',' opt_added_migration_proc\n  ;\n\nopt_added_migration_proc: | '\"addedMigrationProc\"' ':' STRING_LITERAL ','\n  ;\n\nopt_unsub_version: | '\"unsubscribedVersion\"' ':' any_integer ','\n  ;\n\nopt_resub_version: | '\"resubscribedVersion\"' ':' any_integer ','\n  ;\n\nopt_deleted_version: | '\"deletedVersion\"' ':' any_integer ',' opt_deleted_migration_proc\n  ;\n\nopt_deleted_migration_proc: | '\"deletedMigrationProc\"' ':' STRING_LITERAL ','\n  ;\n\nopt_recreate_group_name: | '\"recreateGroupName\"' ':' STRING_LITERAL ','\n  ;\n\nopt_index_names: | index_names\n  ;\n\nindex_names: STRING_LITERAL | STRING_LITERAL ',' index_names\n  ;\n\nopt_arg_names: | arg_names\n  ;\n\narg_names: STRING_LITERAL | STRING_LITERAL ',' arg_names\n  ;\n\nopt_column_names: | column_names\n  ;\n\ncolumn_names: STRING_LITERAL | STRING_LITERAL ',' column_names\n  ;\n\nopt_table_names: | table_names\n  ;\n\ntable_names: STRING_LITERAL | STRING_LITERAL ',' table_names\n  ;\n\nopt_view_names: | view_names\n  ;\n\nview_names: STRING_LITERAL | STRING_LITERAL ',' view_names\n  ;\n\nopt_procedure_names: | procedure_names\n  ;\n\nprocedure_names: STRING_LITERAL | STRING_LITERAL ',' procedure_names\n  ;\n\nopt_sort_order_names: | sort_order_names\n  ;\n\nsort_order_names: STRING_LITERAL | STRING_LITERAL ',' sort_order_names\n  ;\n\ncolumns: column | column ',' columns\n  ;\n\ncolumn: '{'\n        '\"name\"' ':' STRING_LITERAL ','\n        opt_attributes\n        '\"type\"' ':' STRING_LITERAL ','\n        opt_kind\n        opt_is_sensitive\n        '\"isNotNull\"' ':' BOOL_LITERAL ','\n        '\"isAdded\"' ':' BOOL_LITERAL ','\n        opt_added_version\n        '\"isDeleted\"' ':' BOOL_LITERAL ','\n        opt_deleted_version\n        opt_default_value\n        opt_collate\n        opt_check_expr\n        '\"isPrimaryKey\"' ':' BOOL_LITERAL ','\n        '\"isUniqueKey\"' ':' BOOL_LITERAL ','\n        '\"isAutoIncrement\"' ':' BOOL_LITERAL\n        '}'\n  ;\n\nopt_collate : | '\"collate\"' ':' STRING_LITERAL ','\n  ;\n\nopt_check_expr: | '\"checkExpr\"' ':' STRING_LITERAL ',' '\"checkExprArgs\"' ':' '[' opt_arg_names ']' ','\n  ;\n\nopt_default_value: | '\"defaultValue\"' ':' any_literal ','\n  ;\n\nopt_foreign_keys : | foreign_keys\n  ;\n\nopt_kind: | '\"kind\"' ':' STRING_LITERAL ','\n  ;\n\nopt_is_sensitive: | '\"isSensitive\"' ':' '1' ','\n  ;\n\nforeign_keys :  foreign_key | foreign_key ',' foreign_keys\n  ;\n\nforeign_key : '{'\n               opt_name\n               '\"columns\"' ':' '[' column_names ']' ','\n               '\"referenceTable\"' ':' STRING_LITERAL ','\n               '\"referenceColumns\"' ':' '[' column_names ']' ','\n               '\"onUpdate\"' ':' STRING_LITERAL ','\n               '\"onDelete\"' ':' STRING_LITERAL ','\n               '\"isDeferred\"' ':' BOOL_LITERAL\n              '}'\n  ;\n\nopt_unique_keys :  | unique_keys\n  ;\n\nunique_keys : unique_key | unique_key ',' unique_keys\n  ;\n\nunique_key:  '{'\n              opt_name\n              '\"columns\"' ':' '[' column_names ']' ','\n              '\"sortOrders\"' ':' '[' sort_order_names ']'\n             '}'\n  ;\n\nopt_check_expressions: | check_expressions\n  ;\n\ncheck_expressions: check_expression | check_expression ',' check_expressions\n  ;\n\ncheck_expression: '{'\n                   opt_name\n                   '\"checkExpr\"' ':' STRING_LITERAL ','\n                   '\"checkExprArgs\"' ':' '[' ']'\n                  '}'\n  ;\n\nopt_name: | '\"name\"' ':' STRING_LITERAL ','\n  ;\n\nopt_table_indices: | table_indices\n  ;\n\ntable_indices: '\"indices\"' ':' '[' opt_index_names ']' ','\n  ;\n\nopt_attributes:  | attributes\n  ;\n\nattributes: '\"attributes\"' ':' '[' attribute_list ']' ','\n  ;\n\nopt_attribute_list: | attribute_list\n  ;\n\nattribute_list: attribute | attribute ',' attribute_list\n  ;\n\nattribute:  '{'\n             '\"name\"' ':' STRING_LITERAL ','\n             '\"value\"' ':' attribute_value\n            '}'\n  ;\n\nattribute_array: '[' opt_attribute_value_list ']'\n  ;\n\nopt_attribute_value_list: | attribute_value_list\n  ;\n\nattribute_value_list: attribute_value | attribute_value ',' attribute_value_list\n  ;\n\nattribute_value: any_literal | attribute_array\n  ;\n\nany_integer: BOOL_LITERAL | INT_LITERAL\n  ;\n\nany_literal:  BOOL_LITERAL |\n              INT_LITERAL | '-' INT_LITERAL |\n              LONG_LITERAL | '-' LONG_LITERAL |\n              REAL_LITERAL | '-' REAL_LITERAL |\n              STRING_LITERAL | NULL_LITERAL\n  ;\n\nnum_literal:  BOOL_LITERAL |\n              INT_LITERAL | '-' INT_LITERAL |\n              LONG_LITERAL | '-' LONG_LITERAL |\n              REAL_LITERAL | '-' REAL_LITERAL\n  ;\n\nopt_views: | views\n  ;\n\nviews: view | view ',' views\n  ;\n\nview:  '{'\n       '\"name\"' ':' STRING_LITERAL ','\n       '\"crc\"' ':' STRING_LITERAL ','\n       '\"isTemp\"' ':' BOOL_LITERAL ','\n       '\"isDeleted\"' ':' BOOL_LITERAL ','\n       opt_deleted_version\n       opt_region_info\n       opt_attributes\n       projection\n       '\"select\"' ':' STRING_LITERAL ','\n       '\"selectArgs\"' ':' '[' ']' ','\n       dependencies\n       '}'\n  ;\n\nopt_region_info: | '\"region\"' ':' STRING_LITERAL ',' |  '\"region\"' ':' STRING_LITERAL ',' '\"deployedInRegion\"' ':' STRING_LITERAL ','\n  ;\n\nopt_projection: | projection\n  ;\n\nprojection: '\"projection\"' ':' '[' projected_columns ']' ','\n  ;\n\nprojected_columns: projected_column | projected_column ',' projected_columns\n  ;\n\nprojected_column: '{'\n                   '\"name\"' ':' STRING_LITERAL ','\n                   '\"type\"' ':' STRING_LITERAL ','\n                   opt_kind\n                   opt_is_sensitive\n                   '\"isNotNull\"' ':' BOOL_LITERAL\n                  '}'\n  ;\n\nopt_indices:  | indices\n  ;\n\nindices: index  | index ',' indices\n  ;\n\nindex: '{'\n        '\"name\"' ':' STRING_LITERAL ','\n        '\"crc\"' ':' STRING_LITERAL ','\n        '\"table\"' ':' STRING_LITERAL ','\n        '\"isUnique\"' ':' BOOL_LITERAL ','\n        '\"ifNotExists\"' ':' BOOL_LITERAL ','\n        '\"isDeleted\"' ':' BOOL_LITERAL ','\n        opt_deleted_version\n        opt_region_info\n        opt_partial_index_where\n        opt_attributes\n        '\"columns\"' ':' '[' column_names ']' ','\n        '\"sortOrders\"' ':' '[' sort_order_names ']'\n       '}'\n  ;\n\nopt_partial_index_where: | '\"where\"' ':' STRING_LITERAL ','\n  ;\n\nopt_triggers: | triggers\n  ;\n\ntriggers: trigger | trigger ',' triggers\n  ;\n\ntrigger: '{'\n          '\"name\"' ':' STRING_LITERAL ','\n          '\"crc\"' ':' STRING_LITERAL ','\n          '\"target\"' ':' STRING_LITERAL ','\n          '\"isTemp\"' ':' BOOL_LITERAL ','\n          '\"ifNotExists\"' ':' BOOL_LITERAL ','\n          '\"isDeleted\"' ':' BOOL_LITERAL ','\n          opt_deleted_version\n          before_after_instead ','\n          delete_insert_update ','\n          opt_for_each_row\n          opt_when_expr\n          '\"statement\"' ':' STRING_LITERAL ','\n          '\"statementArgs\"' ':' '[' opt_arg_names ']' ','\n          opt_region_info\n          opt_attributes\n          dependencies\n         '}'\n  ;\n\nbefore_after_instead: '\"isBeforeTrigger\"' ':' '1' | '\"isAfterTrigger\"' ':' '1'  | '\"isInsteadOfTrigger\"' ':' '1'\n  ;\n\ndelete_insert_update: '\"isDeleteTrigger\"' ':' '1' | '\"isInsertTrigger\"' ':' '1' | '\"isUpdateTrigger\"' ':' '1'\n  ;\n\nopt_for_each_row: | '\"forEachRow\"' ':' BOOL_LITERAL ','\n  ;\n\nopt_when_expr: | '\"whenExpr\"' ':' STRING_LITERAL ',' '\"whenExprArgs\"' ':' '[' opt_arg_names ']' ','\n  ;\n\ndependencies: opt_insert_tables\n            opt_update_tables\n            opt_delete_tables\n            opt_from_tables\n            opt_uses_procedures\n            opt_uses_views\n            '\"usesTables\"' ':' '[' opt_table_names ']'\n  ;\n\nopt_uses_views: | '\"usesViews\"' ':' '[' opt_view_names ']' ','\n  ;\n\nopt_insert_tables: | '\"insertTables\"' ':' '[' opt_table_names ']' ','\n  ;\n\nopt_update_tables: | '\"updateTables\"' ':' '[' opt_table_names ']' ','\n  ;\n\nopt_delete_tables: | '\"deleteTables\"' ':' '[' opt_table_names ']' ','\n  ;\n\nopt_from_tables: | '\"fromTables\"' ':' '[' opt_table_names ']' ','\n  ;\n\nopt_uses_procedures : | '\"usesProcedures\"' ':' '[' opt_procedure_names ']' ','\n  ;\n\nopt_queries: | queries ;\n\nqueries: query | query ',' queries ;\n\nquery: '{'\n       '\"name\"' ':' STRING_LITERAL ','\n       '\"definedInFile\"' ':' STRING_LITERAL ','\n       '\"definedOnLine\"' ':' INT_LITERAL ','\n       '\"args\"' ':' '[' opt_args ']' ','\n       dependencies ','\n       opt_region_info\n       opt_attributes\n       projection\n       '\"statement\"' ':' STRING_LITERAL ','\n       '\"statementArgs\"' ':' '[' opt_arg_names ']'\n       '}'\n  ;\n\nopt_args: | args\n  ;\n\nargs: arg | arg ',' args\n  ;\n\narg: '{'\n      '\"name\"' ':' STRING_LITERAL ','\n      '\"argOrigin\"' ':' STRING_LITERAL ','\n      '\"type\"' ':' STRING_LITERAL ','\n      opt_kind\n      opt_is_sensitive\n      '\"isNotNull\"' ':' BOOL_LITERAL\n      '}'\n  ;\n\nopt_inserts: | inserts\n  ;\n\ninserts: insert | insert ',' inserts\n  ;\n\ninsert : '{' insert_details ',' '\"values\"' ':' '[' opt_values ']' '}'\n  ;\n\nopt_inserts_general: | inserts_general\n  ;\n\ninserts_general: insert_general | insert_general ',' inserts_general\n  ;\n\ninsert_details:\n         '\"name\"' ':' STRING_LITERAL ','\n         '\"definedInFile\"' ':' STRING_LITERAL ','\n         '\"definedOnLine\"' ':' INT_LITERAL ','\n         '\"args\"' ':' '[' opt_args ']' ','\n         dependencies ','\n         opt_region_info\n         opt_attributes\n         '\"table\"' ':' STRING_LITERAL ','\n         '\"statement\"' ':' STRING_LITERAL ','\n         '\"statementArgs\"' ':' '[' opt_arg_names ']' ','\n         '\"statementType\"' ':' STRING_LITERAL ','\n         '\"columns\"' ':' '[' column_names ']'\n\ninsert_general : '{' insert_details '}'\n  ;\n\nopt_values: | values\n  ;\n\nvalues: value | value ',' values\n  ;\n\nvalue:  '{'\n         '\"value\"' ':' STRING_LITERAL ','\n         '\"valueArgs\"' ':' '[' opt_arg_names ']'\n        '}'\n  ;\n\nopt_updates: | updates\n  ;\n\nupdates: update | update ',' updates\n  ;\n\nupdate : '{'\n         '\"name\"' ':' STRING_LITERAL ','\n         '\"definedInFile\"' ':' STRING_LITERAL ','\n         '\"definedOnLine\"' ':' INT_LITERAL ','\n         '\"args\"' ':' '[' opt_args ']' ','\n         dependencies ','\n         opt_region_info\n         opt_attributes\n         '\"table\"' ':' STRING_LITERAL ','\n         '\"statement\"' ':' STRING_LITERAL ','\n         '\"statementArgs\"' ':' '[' opt_arg_names ']'\n         '}'\n  ;\n\nopt_deletes: | deletes\n  ;\n\ndeletes: delete | delete ',' deletes\n  ;\n\ndelete : '{'\n         '\"name\"' ':' STRING_LITERAL ','\n         '\"definedInFile\"' ':' STRING_LITERAL ','\n         '\"definedOnLine\"' ':' INT_LITERAL ','\n         '\"args\"' ':' '[' opt_args ']' ','\n         dependencies ','\n         opt_region_info\n         opt_attributes\n         '\"table\"' ':' STRING_LITERAL ','\n         '\"statement\"' ':' STRING_LITERAL ','\n         '\"statementArgs\"' ':' '[' opt_arg_names ']'\n         '}'\n  ;\n\nopt_generals: | generals\n  ;\n\ngenerals: general | general ',' generals\n  ;\n\ngeneral: '{'\n          '\"name\"' ':' STRING_LITERAL ','\n          '\"definedInFile\"' ':' STRING_LITERAL ','\n          '\"definedOnLine\"' ':' INT_LITERAL ','\n          '\"args\"' ':' '[' opt_complex_args ']' ','\n          dependencies ','\n          opt_regions\n          opt_attributes\n          opt_projection\n          opt_result_contract\n          '\"usesDatabase\"' ':' BOOL_LITERAL\n         '}'\n  ;\n\nopt_result_contract: | '\"hasSelectResult\"' ':' '1' ',' | '\"hasOutResult\"' ':' '1' ',' | '\"hasOutUnionResult\"' ':''1' ','\n  ;\n\nopt_complex_args: | complex_args\n  ;\n\ncomplex_args: complex_arg | complex_arg ',' complex_args\n  ;\n\ncomplex_arg: '{'\n              binding\n              '\"name\"' ':' STRING_LITERAL ','\n              '\"argOrigin\"' ':' STRING_LITERAL ','\n              '\"type\"' ':' STRING_LITERAL ','\n              opt_kind\n              opt_is_sensitive\n              '\"isNotNull\"' ':' BOOL_LITERAL\n             '}'\n  ;\n\nbinding: | '\"binding\"' ':' '\"inout\"' ',' | '\"binding\"' ':' '\"out\"' ','\n  ;\n\nopt_enums: | enums\n  ;\n\nenums: enum | enum ',' enums\n  ;\n\nenum: '{'\n      '\"name\"' ':' STRING_LITERAL ','\n      '\"type\"' ':' STRING_LITERAL ','\n      '\"isNotNull\"' ':' '1' ','\n      '\"values\"' ':' '[' enum_values ']'\n      '}'\n  ;\n\nenum_values: enum_value | enum_value ',' enum_values\n  ;\n\nenum_value: '{'\n             '\"name\"' ':' STRING_LITERAL ','\n             '\"value\"' ':' num_literal\n            '}'\n  ;\n\nopt_interfaces: | interfaces\n  ;\n\ninterfaces: interface | interface ',' interfaces\n  ;\n\ninterface: '{'\n          '\"name\"' ':' STRING_LITERAL ','\n          '\"definedInFile\"' ':' STRING_LITERAL ','\n          '\"definedOnLine\"' ':' INT_LITERAL ','\n          '\"projection\"' ':' '[' projected_columns ']'\n         '}'\n  ;\n\nopt_subscriptions: | subscriptions\n  ;\n\nsubscriptions: subscription | subscription ',' subscriptions\n  ;\n\nsubscription: '{'\n     '\"type\"' ':' STRING_LITERAL ','\n     '\"table\"' ':' STRING_LITERAL ','\n     opt_region_info\n     '\"version\"' ':' any_integer\n     '}'\n  ;\n\nopt_const_groups: | const_groups\n  ;\n\nconst_groups: const_group | const_group ',' const_groups\n  ;\n\nconst_group: '{'\n      '\"name\"' ':' STRING_LITERAL ','\n      '\"values\"' ':' '[' const_values ']'\n      '}'\n  ;\n\nconst_values: const_value | const_value ',' const_values\n  ;\n\nconst_value: '{'\n             '\"name\"' ':' STRING_LITERAL ','\n             '\"type\"' ':' STRING_LITERAL ','\n             opt_kind\n             '\"isNotNull\"' ':' BOOL_LITERAL ','\n             '\"value\"' ':' num_literal\n            '}'\n  | '{'\n             '\"name\"' ':' STRING_LITERAL ','\n             '\"type\"' ':' STRING_LITERAL ','\n             opt_kind\n             '\"isNotNull\"' ':' BOOL_LITERAL ','\n             '\"value\"' ':' STRING_LITERAL\n            '}'\n  ;\n\nopt_regions: | regions\n  ;\n\nregions: region | region ',' regions\n  ;\n\nregion:  '{'\n          '\"name\"' ':' STRING_LITERAL ','\n          '\"isDeployableRoot\"' ':' BOOL_LITERAL ','\n          '\"deployedInRegion\"' ':' STRING_LITERAL ','\n          '\"using\"' ':' '[' opt_region_names ']' ','\n          '\"usingPrivately\"' ':' '[' opt_bool_list ']'\n         '}'\n  ;\n\nopt_region_names: | region_names\n  ;\n\nregion_names: STRING_LITERAL | STRING_LITERAL ',' region_names\n  ;\n\nopt_bool_list: | bool_list\n  ;\n\nbool_list: BOOL_LITERAL | BOOL_LITERAL ',' bool_list\n  ;\n\nopt_ad_hoc_migrations: | ad_hoc_migrations\n  ;\n\nad_hoc_migrations: ad_hoc_migration | ad_hoc_migration ',' ad_hoc_migrations\n  ;\n\nad_hoc_migration: '{'\n                  '\"name\"' ':' STRING_LITERAL ','\n                  '\"crc\"' ':' STRING_LITERAL ','\n                  opt_attributes\n                  '\"version\"' ':' any_integer\n                  '}'\n  | '{'\n                  '\"name\"' ':' STRING_LITERAL ','\n                  '\"crc\"' ':' STRING_LITERAL ','\n                  opt_attributes\n                  '\"onRecreateOf\"' ':' STRING_LITERAL\n                  '}'\n  ;\n\n")))}T.isMDXComponent=!0}}]);
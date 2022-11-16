---
id: x2
title: "Appendix 2: CQL Grammar"
sidebar_label: "Appendix 2: CQL Grammar"
---
<!---
-- Copyright (c) Meta Platforms, Inc. and affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from a grammar snapshot with the tree building rules removed.
It should give a fair sense of the syntax of CQL (but not semantic validation).

Snapshot as of Tue Nov 15 17:52:14 PST 2022

### Operators and Literals

These are in order of priority lowest to highest

```
"UNION ALL" "UNION" "INTERSECT" "EXCEPT"
":="
"OR"
"AND"
"NOT"
"BETWEEN" "NOT BETWEEN" "<>" "!=" '=' "==" "LIKE" "NOT LIKE" "GLOB" "NOT GLOB" "MATCH" "NOT MATCH" "REGEXP" "NOT REGEXP" "IN" "NOT IN" "IS NOT" "IS" "IS TRUE" "IS FALSE" "IS NOT TRUE" "IS NOT FALSE"
"ISNULL" "NOTNULL"
'<' '>' ">=" "<="
"<<" ">>" '&' '|'
'+' '-'
'*' '/' '%'
"||"
"COLLATE"
"UMINUS" '~'
```
NOTE: The above varies considerably from the C binding order!!!

Literals:
```
ID  /* a name */
STRLIT /* a string literal in SQL format e.g. 'it''s sql' */
CSTRLIT /* a string literal in C format e.g. "hello, world\n" */
BLOBLIT /* a blob literal in SQL format e.g. x'12ab' */
INTLIT /* integer literal */
LONGLIT /* long integer literal */
REALLIT /* floating point literal */
```
### Statement/Type Keywords
```
"@ATTRIBUTE" "@BEGIN_SCHEMA_REGION" "@BLOB_CREATE_KEY"
"@BLOB_CREATE_VAL" "@BLOB_GET_KEY" "@BLOB_GET_KEY_TYPE"
"@BLOB_GET_VAL" "@BLOB_GET_VAL_TYPE" "@BLOB_UPDATE_KEY"
"@BLOB_UPDATE_VAL" "@CREATE" "@DECLARE_DEPLOYABLE_REGION"
"@DECLARE_SCHEMA_REGION" "@DELETE" "@DUMMY_SEED" "@ECHO"
"@EMIT_CONSTANTS" "@EMIT_ENUMS" "@EMIT_GROUP"
"@END_SCHEMA_REGION" "@ENFORCE_NORMAL" "@ENFORCE_POP"
"@ENFORCE_PUSH" "@ENFORCE_RESET" "@ENFORCE_STRICT"
"@EPONYMOUS" "@FILE" "@PREVIOUS_SCHEMA" "@PROC" "@RC"
"@RECREATE" "@SCHEMA_AD_HOC_MIGRATION"
"@SCHEMA_UPGRADE_SCRIPT" "@SCHEMA_UPGRADE_VERSION"
"@SENSITIVE" "@UNSUB" "ABORT" "ACTION" "ADD" "AFTER" "ALL"
"ALTER" "ARGUMENTS" "AS" "ASC" "AUTOINCREMENT" "BEFORE"
"BEGIN" "BLOB" "BY" "CALL" "CASCADE" "CASE" "CAST" "CATCH"
"CHECK" "CLOSE" "COLUMN" "COLUMNS" "COMMIT" "CONST"
"CONSTRAINT" "CONTEXT COLUMN" "CONTEXT TYPE" "CONTINUE"
"CREATE" "CROSS" "CURRENT ROW" "CURSOR HAS ROW" "CURSOR"
"DECLARE" "DEFAULT" "DEFERRABLE" "DEFERRED" "DELETE" "DESC"
"DISTINCT" "DISTINCTROW" "DO" "DROP" "ELSE IF" "ELSE"
"ENCODE" "END" "ENUM" "EXCLUDE CURRENT ROW" "EXCLUDE GROUP"
"EXCLUDE NO OTHERS" "EXCLUDE TIES" "EXCLUSIVE" "EXISTS"
"EXPLAIN" "FAIL" "FETCH" "FILTER" "FIRST" "FOLLOWING" "FOR
EACH ROW" "FOR" "FOREIGN" "FROM BLOB" "FROM" "FUNC"
"FUNCTION" "GROUP" "GROUPS" "HAVING" "HIDDEN" "IF" "IGNORE"
"IMMEDIATE" "INDEX" "INITIALLY" "INNER" "INOUT" "INSERT"
"INSTEAD" "INT" "INTEGER" "INTERFACE" "INTO" "JOIN" "KEY"
"LAST" "LEAVE" "LEFT" "LET" "LIMIT" "LONG" "LONG_INT"
"LONG_INTEGER" "LOOP" "NO" "NOT DEFERRABLE" "NOTHING"
"NULL" "NULLS" "OBJECT" "OF" "OFFSET" "ON CONFLICT" "ON"
"OPEN" "ORDER" "OUT" "OUTER" "OVER" "PARTITION" "PRECEDING"
"PRIMARY" "PRIVATE" "PROC" "PROCEDURE" "QUERY PLAN" "RAISE"
"RANGE" "REAL" "RECURSIVE" "REFERENCES" "RELEASE" "RENAME"
"REPLACE" "RESTRICT" "RETURN" "RIGHT" "ROLLBACK" "ROWID"
"ROWS" "SAVEPOINT" "SELECT" "SET" "SIGN FUNCTION"
"STATEMENT" "SWITCH" "TABLE" "TEMP" "TEXT" "THEN" "THROW"
"TO" "TRANSACTION" "TRIGGER" "TRY" "TYPE" "TYPE_CHECK"
"UNBOUNDED" "UNIQUE" "UPDATE" "UPSERT" "USING" "VALUES"
"VAR" "VIEW" "VIRTUAL" "WHEN" "WHERE" "WHILE" "WINDOW"
"WITH" "WITHOUT"
```
### Rules

Note that in many cases the grammar is more generous than the overall language and errors have to be checked on top of this, often this is done on purpose because even when it's possible it might be very inconvenient to do checks with syntax.  For example the grammar cannot enforce non-duplicate ids in id lists, but it could enforce non-duplicate attributes in attribute lists.  It chooses to do neither as they are easily done with semantic validation.  Thus the grammar is not the final authority on what constitutes a valid program but it's a good start.
```


program:
  opt_stmt_list
  ;

opt_stmt_list:
  /*nil*/
  | stmt_list
  ;

stmt_list:
  stmt ';'
  | stmt_list stmt ';'
  ;

stmt:
  misc_attrs any_stmt
  ;

any_stmt:
    alter_table_add_column_stmt
  | begin_schema_region_stmt
  | begin_trans_stmt
  | blob_get_key_type_stmt
  | blob_get_val_type_stmt
  | blob_get_key_stmt
  | blob_get_val_stmt
  | blob_create_key_stmt
  | blob_create_val_stmt
  | blob_update_key_stmt
  | blob_update_val_stmt
  | call_stmt
  | close_stmt
  | commit_return_stmt
  | commit_trans_stmt
  | continue_stmt
  | create_index_stmt
  | create_proc_stmt
  | create_table_stmt
  | create_trigger_stmt
  | create_view_stmt
  | create_virtual_table_stmt
  | declare_deployable_region_stmt
  | declare_enum_stmt
  | declare_const_stmt
  | declare_group_stmt
  | declare_select_func_no_check_stmt
  | declare_func_stmt
  | declare_out_call_stmt
  | declare_proc_no_check_stmt
  | declare_proc_stmt
  | declare_interface_stmt
  | declare_schema_region_stmt
  | declare_vars_stmt
  | declare_forward_read_cursor_stmt
  | declare_fetched_value_cursor_stmt
  | declare_type_stmt
  | delete_stmt
  | drop_index_stmt
  | drop_table_stmt
  | drop_trigger_stmt
  | drop_view_stmt
  | echo_stmt
  | emit_enums_stmt
  | emit_group_stmt
  | emit_constants_stmt
  | end_schema_region_stmt
  | enforce_normal_stmt
  | enforce_pop_stmt
  | enforce_push_stmt
  | enforce_reset_stmt
  | enforce_strict_stmt
  | explain_stmt
  | select_nothing_stmt
  | fetch_call_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_cursor_from_blob_stmt
  | guard_stmt
  | if_stmt
  | insert_stmt
  | leave_stmt
  | let_stmt
  | loop_stmt
  | out_stmt
  | out_union_stmt
  | out_union_parent_child_stmt
  | previous_schema_stmt
  | proc_savepoint_stmt
  | release_savepoint_stmt
  | return_stmt
  | rollback_return_stmt
  | rollback_trans_stmt
  | savepoint_stmt
  | select_stmt
  | schema_ad_hoc_migration_stmt
  | schema_unsub_stmt
  | schema_upgrade_script_stmt
  | schema_upgrade_version_stmt
  | set_stmt
  | switch_stmt
  | throw_stmt
  | trycatch_stmt
  | update_cursor_stmt
  | update_stmt
  | upsert_stmt
  | while_stmt
  | with_delete_stmt
  | with_insert_stmt
  | with_update_stmt
  | with_upsert_stmt
  ;

explain_stmt:
  "EXPLAIN" opt_query_plan explain_target
  ;

opt_query_plan:
  /* nil */
  | "QUERY PLAN"
  ;

explain_target: select_stmt
  | update_stmt
  | delete_stmt
  | with_delete_stmt
  | with_insert_stmt
  | insert_stmt
  | upsert_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | begin_trans_stmt
  | commit_trans_stmt
  ;

previous_schema_stmt:
  "@PREVIOUS_SCHEMA"
  ;

schema_upgrade_script_stmt:
  "@SCHEMA_UPGRADE_SCRIPT"
  ;

schema_upgrade_version_stmt:
  "@SCHEMA_UPGRADE_VERSION" '(' "integer-literal" ')'
  ;

set_stmt:
  "SET" name ":=" expr
  | "SET" name "FROM" "CURSOR" name
  ;

let_stmt:
  "LET" name ":=" expr
  ;

version_attrs_opt_recreate:
  /* nil */
  | "@RECREATE"  opt_delete_plain_attr
  | "@RECREATE" '(' name ')'  opt_delete_plain_attr
  | version_attrs
  ;

opt_delete_plain_attr:
  /* nil */
  | "@DELETE"
  ;

opt_version_attrs:
  /* nil */
  | version_attrs
  ;

version_attrs:
  "@CREATE" version_annotation opt_version_attrs
  | "@DELETE" version_annotation opt_version_attrs
  ;

opt_delete_version_attr:
  /* nil */
  | "@DELETE" version_annotation
  ;

drop_table_stmt:
  "DROP" "TABLE" "IF" "EXISTS" name
  | "DROP" "TABLE" name
  ;

drop_view_stmt:
  "DROP" "VIEW" "IF" "EXISTS" name
  | "DROP" "VIEW" name
  ;

drop_index_stmt:
  "DROP" "INDEX" "IF" "EXISTS" name
  | "DROP" "INDEX" name
  ;

drop_trigger_stmt:
  "DROP" "TRIGGER" "IF" "EXISTS" name
  | "DROP" "TRIGGER" name
  ;

create_virtual_table_stmt: "CREATE" "VIRTUAL" "TABLE" opt_vtab_flags name
                           "USING" name opt_module_args
                           "AS" '(' col_key_list ')' opt_delete_version_attr ;

opt_module_args: /* nil */
  | '(' misc_attr_value_list ')'
  | '(' "ARGUMENTS" "FOLLOWING" ')'
  ;

create_table_prefix_opt_temp:
  "CREATE" opt_temp "TABLE" ;

create_table_stmt:
  create_table_prefix_opt_temp opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate
  ;

opt_temp:
  /* nil */
  | "TEMP"
  ;

opt_if_not_exists:
  /* nil */
  | "IF" "NOT" "EXISTS"
  ;

opt_no_rowid:
  /* nil */
  | "WITHOUT" "ROWID"
  ;

opt_vtab_flags:
  /* nil */
  | "IF" "NOT" "EXISTS"
  | "@EPONYMOUS"
  | "@EPONYMOUS" "IF" "NOT" "EXISTS"
  | "IF" "NOT" "EXISTS" "@EPONYMOUS"
  ;

col_key_list:
  col_key_def
  | col_key_def ',' col_key_list
  ;

col_key_def:
  col_def
  | pk_def
  | fk_def
  | unq_def
  | check_def
  | shape_def
  ;

check_def:
  "CONSTRAINT" name "CHECK" '(' expr ')'
  | "CHECK" '(' expr ')'
  ;

shape_exprs :
  shape_expr ',' shape_exprs
  | shape_expr
  ;

shape_expr:
  name
  | '-' name
  ;

shape_def:
    shape_def_base
  | shape_def_base '(' shape_exprs ')'
  ;

shape_def_base:
    "LIKE" name
  | "LIKE" name "ARGUMENTS"
  ;

col_name:
  name
  ;

misc_attr_key:
  name
  | name ':' name
  ;

misc_attr_value_list:
  misc_attr_value
  | misc_attr_value ',' misc_attr_value_list
  ;

misc_attr_value:
  name
  | any_literal
  | const_expr
  | '(' misc_attr_value_list ')'
  | '-' num_literal
  | '+' num_literal
  ;

misc_attr:
  "@ATTRIBUTE" '(' misc_attr_key ')'
  | "@ATTRIBUTE" '(' misc_attr_key '=' misc_attr_value ')'
  ;

misc_attrs:
  /* nil */
  | misc_attr misc_attrs
  ;

col_def:
  misc_attrs col_name data_type_any col_attrs
  ;

pk_def:
  "CONSTRAINT" name "PRIMARY" "KEY" '(' indexed_columns ')' opt_conflict_clause
  | "PRIMARY" "KEY" '(' indexed_columns ')' opt_conflict_clause
  ;

opt_conflict_clause:
  /* nil */
  | conflict_clause
  ;

conflict_clause:
  "ON CONFLICT" "ROLLBACK"
  | "ON CONFLICT" "ABORT"
  | "ON CONFLICT" "FAIL"
  | "ON CONFLICT" "IGNORE"
  | "ON CONFLICT" "REPLACE"
  ;

opt_fk_options:
  /* nil */
  | fk_options
  ;

fk_options:
  fk_on_options
  | fk_deferred_options
  | fk_on_options fk_deferred_options
  ;

fk_on_options:
  "ON" "DELETE" fk_action
  | "ON" "UPDATE" fk_action
  | "ON" "UPDATE" fk_action "ON" "DELETE" fk_action
  | "ON" "DELETE" fk_action "ON" "UPDATE" fk_action
  ;

fk_action:
  "SET" "NULL"
  | "SET" "DEFAULT"
  | "CASCADE"
  | "RESTRICT"
  | "NO" "ACTION"
  ;

fk_deferred_options:
  "DEFERRABLE" fk_initial_state
  | "NOT DEFERRABLE" fk_initial_state
  ;

fk_initial_state:
  /* nil */
  | "INITIALLY" "DEFERRED"
  | "INITIALLY" "IMMEDIATE"
  ;

fk_def:
  "CONSTRAINT" name "FOREIGN" "KEY" '(' name_list ')' fk_target_options
  | "FOREIGN" "KEY" '(' name_list ')' fk_target_options
  ;

fk_target_options:
  "REFERENCES" name '(' name_list ')' opt_fk_options
  ;

unq_def:
  "CONSTRAINT" name "UNIQUE" '(' indexed_columns ')' opt_conflict_clause
  | "UNIQUE" '(' indexed_columns ')' opt_conflict_clause
  ;

opt_unique:
  /* nil */
  | "UNIQUE"
  ;

indexed_column:
  expr opt_asc_desc
  ;

indexed_columns:
  indexed_column
  | indexed_column ',' indexed_columns
  ;

create_index_stmt:
  "CREATE" opt_unique "INDEX" opt_if_not_exists name "ON" name '(' indexed_columns ')' opt_where opt_delete_version_attr
  ;

name:
  "ID"
  | "TEXT"
  | "TRIGGER"
  | "ROWID"
  | "REPLACE"
  | "KEY"
  | "VIRTUAL"
  | "TYPE"
  | "HIDDEN"
  | "PRIVATE"
  | "FIRST"
  | "LAST"
  ;

opt_name:
  /* nil */
  | name
  ;

name_list:
  name
  |  name ',' name_list
  ;

opt_name_list:
  /* nil */
  | name_list
  ;

cte_binding_list:
  cte_binding
  | cte_binding ',' cte_binding_list
  ;

cte_binding: name name
  | name "AS" name
  ;

col_attrs:
  /* nil */
  | "NOT" "NULL" opt_conflict_clause col_attrs
  | "PRIMARY" "KEY" opt_conflict_clause col_attrs
  | "PRIMARY" "KEY" opt_conflict_clause "AUTOINCREMENT" col_attrs
  | "DEFAULT" '-' num_literal col_attrs
  | "DEFAULT" '+' num_literal col_attrs
  | "DEFAULT" num_literal col_attrs
  | "DEFAULT" const_expr col_attrs
  | "DEFAULT" str_literal col_attrs
  | "COLLATE" name col_attrs
  | "CHECK" '(' expr ')' col_attrs
  | "UNIQUE" opt_conflict_clause col_attrs
  | "HIDDEN" col_attrs
  | "@SENSITIVE" col_attrs
  | "@CREATE" version_annotation col_attrs
  | "@DELETE" version_annotation col_attrs
  | fk_target_options col_attrs
  ;

version_annotation:
  '(' "integer-literal" ',' name ')'
  | '(' "integer-literal" ',' name ':' name ')'
  | '(' "integer-literal" ')'
  ;

opt_kind:
  /* nil */
  | '<' name '>'
  ;

data_type_numeric:
  "INT" opt_kind
  | "INTEGER" opt_kind
  | "REAL" opt_kind
  | "LONG" opt_kind
  | "BOOL" opt_kind
  | "LONG" "INTEGER" opt_kind
  | "LONG" "INT" opt_kind
  | "LONG_INT" opt_kind
  | "LONG_INTEGER" opt_kind
  ;

data_type_any:
  data_type_numeric
  | "TEXT"  opt_kind
  | "BLOB"  opt_kind
  | "OBJECT" opt_kind
  | "OBJECT" '<' name "CURSOR" '>'
  | "OBJECT" '<' name "SET" '>'
  | "ID"
  ;

data_type_with_options:
  data_type_any
  | data_type_any "NOT" "NULL"
  | data_type_any "@SENSITIVE"
  | data_type_any "@SENSITIVE" "NOT" "NULL"
  | data_type_any "NOT" "NULL" "@SENSITIVE"
  ;

str_literal:
  str_chain
  ;

str_chain:
  str_leaf
  | str_leaf str_chain
  ;

str_leaf:
  "sql-string-literal"
  | "c-string-literal"
  ;

num_literal:
  "integer-literal"
  | "long-literal"
  | "real-literal"
  | "TRUE"
  | "FALSE"
  ;

const_expr:
  "CONST" '(' expr ')'
  ;

any_literal:
  str_literal
  | num_literal
  | "NULL"
  | "@FILE" '(' str_literal ')'
  | "@PROC"
  | "sql-blob-literal"
  ;

raise_expr:
  "RAISE" '(' "IGNORE" ')'
  | "RAISE" '(' "ROLLBACK" ','  expr ')'
  | "RAISE" '(' "ABORT" ','  expr ')'
  | "RAISE" '(' "FAIL" ','  expr ')'
  ;

call:
  name '(' arg_list ')' opt_filter_clause
  | name '(' "DISTINCT" arg_list ')' opt_filter_clause
  | basic_expr ':' name '(' arg_list ')'
  ;

basic_expr:
  name
  | "@RC"
  | name '.' name
  | any_literal
  | const_expr
  | '(' expr ')'
  | call
  | window_func_inv
  | raise_expr
  | '(' select_stmt ')'
  | '(' select_stmt "IF" "NOTHING" expr ')'
  | '(' select_stmt "IF" "NOTHING" "OR" "NULL" expr ')'
  | '(' select_stmt "IF" "NOTHING" "THROW"')'
  | "EXISTS" '(' select_stmt ')'
  | "CASE" expr case_list "END"
  | "CASE" expr case_list "ELSE" expr "END"
  | "CASE" case_list "END"
  | "CASE" case_list "ELSE" expr "END"
  | "CAST" '(' expr "AS" data_type_any ')'
  | "TYPE_CHECK" '(' expr "AS" data_type_with_options ')'

math_expr:
  basic_expr
  | math_expr '&' math_expr
  | math_expr '|' math_expr
  | math_expr "<<" math_expr
  | math_expr ">>"  math_expr
  | math_expr '+' math_expr
  | math_expr '-' math_expr
  | math_expr '*' math_expr
  | math_expr '/' math_expr
  | math_expr '%' math_expr
  | math_expr "IS NOT TRUE"
  | math_expr "IS NOT FALSE"
  | math_expr "ISNULL"
  | math_expr "NOTNULL"
  | math_expr "IS TRUE"
  | math_expr "IS FALSE"
  | '-' math_expr
  | '+' math_expr
  | '~' math_expr
  | "NOT" math_expr
  | math_expr '=' math_expr
  | math_expr "==" math_expr
  | math_expr '<' math_expr
  | math_expr '>' math_expr
  | math_expr "<>" math_expr
  | math_expr "!=" math_expr
  | math_expr ">=" math_expr
  | math_expr "<=" math_expr
  | math_expr "NOT IN" '(' expr_list ')'
  | math_expr "NOT IN" '(' select_stmt ')'
  | math_expr "IN" '(' expr_list ')'
  | math_expr "IN" '(' select_stmt ')'
  | math_expr "LIKE" math_expr
  | math_expr "NOT LIKE" math_expr
  | math_expr "MATCH" math_expr
  | math_expr "NOT MATCH" math_expr
  | math_expr "REGEXP" math_expr
  | math_expr "NOT REGEXP" math_expr
  | math_expr "GLOB" math_expr
  | math_expr "NOT GLOB" math_expr
  | math_expr "BETWEEN" math_expr "AND" math_expr
  | math_expr "NOT BETWEEN" math_expr "AND" math_expr
  | math_expr "IS NOT" math_expr
  | math_expr "IS" math_expr
  | math_expr "||" math_expr
  | math_expr "COLLATE" name
  ;

expr:
  math_expr
  | expr "AND" expr
  | expr "OR" expr
  ;

case_list:
  "WHEN" expr "THEN" expr
  | "WHEN" expr "THEN" expr case_list
  ;

arg_expr: '*'
  | expr
  | shape_arguments
  ;

arg_list:
  /* nil */
  | arg_expr
  | arg_expr ',' arg_list
  ;

expr_list:
  expr
  | expr ',' expr_list
  ;

shape_arguments:
  "FROM" name
  | "FROM" name shape_def
  | "FROM" "ARGUMENTS"
  | "FROM" "ARGUMENTS" shape_def
  ;

column_calculation:
  "COLUMNS" '(' col_calcs ')'
  | "COLUMNS" '(' "DISTINCT" col_calcs ')'
  ;

col_calcs:
  col_calc
  | col_calc ',' col_calcs
  ;

col_calc:
  name
  | shape_def
  | name shape_def
  | name '.' name
  ;

call_expr:
  expr
  | shape_arguments
  ;

call_expr_list:
  call_expr
  | call_expr ',' call_expr_list
  ;

cte_tables:
  cte_table
  | cte_table ',' cte_tables
  ;

cte_decl:
  name '(' name_list ')'
  | name '(' '*' ')'
  ;

shared_cte:
  call_stmt
  | call_stmt "USING" cte_binding_list
  ;

cte_table:
  cte_decl "AS" '(' select_stmt ')'
  | cte_decl "AS" '(' shared_cte')'
  | '(' call_stmt ')'
  | '(' call_stmt "USING" cte_binding_list ')'
  | cte_decl "LIKE" '(' select_stmt ')'
  | cte_decl "LIKE" name
  ;

with_prefix:
  "WITH" cte_tables
  | "WITH" "RECURSIVE" cte_tables
  ;

with_select_stmt:
  with_prefix select_stmt_no_with
  ;

select_nothing_stmt:
  "SELECT" "NOTHING"
  ;

select_stmt:
  with_select_stmt
  | select_stmt_no_with
  ;

select_stmt_no_with:
  select_core_list opt_orderby opt_limit opt_offset
  ;

select_core_list:
  select_core
  | select_core compound_operator select_core_list
  ;

values:
  '(' insert_list ')'
  | '(' insert_list ')' ',' values
  ;

select_core:
  "SELECT" select_opts select_expr_list opt_from_query_parts opt_where opt_groupby opt_having opt_select_window
  | "VALUES" values
  ;

compound_operator:
  "UNION"
  | "UNION ALL"
  | "INTERSECT"
  | "EXCEPT"
  ;

window_func_inv:
  name '(' arg_list ')' opt_filter_clause "OVER" window_name_or_defn
  ;

opt_filter_clause:
  /* nil */
  | "FILTER" '(' opt_where ')'
  ;

window_name_or_defn: window_defn
  | name
  ;

window_defn:
  '(' opt_partition_by opt_orderby opt_frame_spec ')'
  ;

opt_frame_spec:
  /* nil */
  | frame_type frame_boundary_opts frame_exclude
  ;

frame_type:
  "RANGE"
  | "ROWS"
  | "GROUPS"
  ;

frame_exclude:
  /* nil */
  | "EXCLUDE NO OTHERS"
  | "EXCLUDE CURRENT ROW"
  | "EXCLUDE GROUP"
  | "EXCLUDE TIES"
  ;

frame_boundary_opts:
  frame_boundary
  | "BETWEEN" frame_boundary_start "AND" frame_boundary_end
  ;

frame_boundary_start:
  "UNBOUNDED" "PRECEDING"
  | expr "PRECEDING"
  | "CURRENT ROW"
  | expr "FOLLOWING"
  ;

frame_boundary_end:
  expr "PRECEDING"
  | "CURRENT ROW"
  | expr "FOLLOWING"
  | "UNBOUNDED" "FOLLOWING"
  ;

frame_boundary:
  "UNBOUNDED" "PRECEDING"
  | expr "PRECEDING"
  | "CURRENT ROW"
  ;

opt_partition_by:
  /* nil */
  | "PARTITION" "BY" expr_list
  ;

opt_select_window:
  /* nil */
  | window_clause
  ;

window_clause:
  "WINDOW" window_name_defn_list
  ;

window_name_defn_list:
  window_name_defn
  | window_name_defn ',' window_name_defn_list
  ;

window_name_defn:
  name "AS" window_defn
  ;

region_spec:
    name
  | name "PRIVATE"
  ;

region_list:
  region_spec ',' region_list
  | region_spec
  ;

declare_schema_region_stmt:
  "@DECLARE_SCHEMA_REGION" name
  | "@DECLARE_SCHEMA_REGION" name "USING" region_list
  ;

declare_deployable_region_stmt:
  "@DECLARE_DEPLOYABLE_REGION"  name
  | "@DECLARE_DEPLOYABLE_REGION" name "USING" region_list
  ;

begin_schema_region_stmt:
  "@BEGIN_SCHEMA_REGION" name
  ;

end_schema_region_stmt:
  "@END_SCHEMA_REGION"
  ;

schema_unsub_stmt:
  "@UNSUB" '(' "integer-literal" ',' name ')'
  | "@UNSUB"  '(' name ')'
  ;

schema_ad_hoc_migration_stmt:
  "@SCHEMA_AD_HOC_MIGRATION" version_annotation
  | "@SCHEMA_AD_HOC_MIGRATION" "FOR" "@RECREATE" '(' name ',' name ')'
  ;

emit_enums_stmt:
  "@EMIT_ENUMS" opt_name_list
  ;

emit_group_stmt:
  "@EMIT_GROUP" opt_name_list
  ;

emit_constants_stmt:
  "@EMIT_CONSTANTS" name_list
  ;

opt_from_query_parts:
  /* nil */
  | "FROM" query_parts
  ;

opt_where:
  /* nil */
  | "WHERE" expr
  ;

opt_groupby:
  /* nil */
  | "GROUP" "BY" groupby_list
  ;

groupby_list:
  groupby_item
  | groupby_item ',' groupby_list
  ;

groupby_item:
  expr
  ;

opt_asc_desc:
  /* nil */
  | "ASC"  opt_nullsfirst_nullslast
  | "DESC"  opt_nullsfirst_nullslast
  ;

opt_nullsfirst_nullslast:
  /* nil */
  | "NULLS" "FIRST"
  | "NULLS" "LAST"
  ;

opt_having:
  /* nil */
  | "HAVING" expr
  ;

opt_orderby:
  /* nil */
  | "ORDER" "BY" orderby_list
  ;

orderby_list:
  orderby_item
  | orderby_item ',' orderby_list
  ;

orderby_item:
  expr opt_asc_desc
  ;

opt_limit:
  /* nil */
  | "LIMIT" expr
  ;

opt_offset:
  /* nil */
  | "OFFSET" expr
  ;

select_opts:
  /* nil */
  | "ALL"
  | "DISTINCT"
  | "DISTINCTROW"
  ;

select_expr_list:
  select_expr
  | select_expr ',' select_expr_list
  | '*'
  ;

select_expr:
  expr opt_as_alias
  | name '.' '*'
  | column_calculation
  ;

opt_as_alias:
  /* nil */
  | as_alias
  ;

as_alias:
  "AS" name
  | name
  ;

query_parts:
  table_or_subquery_list
  | join_clause
  ;

table_or_subquery_list:
  table_or_subquery
  | table_or_subquery ',' table_or_subquery_list
  ;

join_clause:
  table_or_subquery join_target_list
  ;

join_target_list:
  join_target
  | join_target join_target_list
  ;

table_or_subquery:
  name opt_as_alias
  | '(' select_stmt ')' opt_as_alias
  | '(' shared_cte ')' opt_as_alias
  | table_function opt_as_alias
  | '(' query_parts ')'
  ;

join_type:
  /*nil */
  | "LEFT"
  | "RIGHT"
  | "LEFT" "OUTER"
  | "RIGHT" "OUTER"
  | "INNER"
  | "CROSS"
  ;

join_target: join_type "JOIN" table_or_subquery opt_join_cond
  ;

opt_join_cond:
  /* nil */
  | join_cond
  ;

join_cond:
  "ON" expr
  | "USING" '(' name_list ')'
  ;

table_function:
  name '(' arg_list ')'
  ;

create_view_stmt:
  "CREATE" opt_temp "VIEW" opt_if_not_exists name "AS" select_stmt opt_delete_version_attr
  ;

with_delete_stmt:
  with_prefix delete_stmt
  ;

delete_stmt:
  "DELETE" "FROM" name opt_where
  ;

opt_insert_dummy_spec:
  /*nil*/
  | "@DUMMY_SEED" '(' expr ')' dummy_modifier
  ;

dummy_modifier:
  /* nil */
  | "@DUMMY_NULLABLES"
  | "@DUMMY_DEFAULTS"
  | "@DUMMY_NULLABLES" "@DUMMY_DEFAULTS"
  | "@DUMMY_DEFAULTS" "@DUMMY_NULLABLES"
  ;

insert_stmt_type:
  "INSERT" "INTO"
  | "INSERT" "OR" "REPLACE" "INTO"
  | "INSERT" "OR" "IGNORE" "INTO"
  | "INSERT" "OR" "ROLLBACK" "INTO"
  | "INSERT" "OR" "ABORT" "INTO"
  | "INSERT" "OR" "FAIL" "INTO"
  | "REPLACE" "INTO"
  ;

with_insert_stmt:
  with_prefix insert_stmt
  ;

opt_column_spec:
  /* nil */
  | '(' opt_name_list ')'
  | '(' shape_def ')'
  ;

from_shape:
  "FROM" "CURSOR" name opt_column_spec
  | "FROM" name opt_column_spec
  | "FROM" "ARGUMENTS" opt_column_spec
  ;

insert_stmt:
  insert_stmt_type name opt_column_spec select_stmt opt_insert_dummy_spec
  | insert_stmt_type name opt_column_spec from_shape opt_insert_dummy_spec
  | insert_stmt_type name "DEFAULT" "VALUES"
  | insert_stmt_type name "USING" select_stmt
  | insert_stmt_type name "USING" expr_names opt_insert_dummy_spec
  ;

insert_list_item:
  expr
  | shape_arguments
  ;

insert_list:
  /* nil */
  | insert_list_item
  | insert_list_item ',' insert_list
  ;

basic_update_stmt:
  "UPDATE" opt_name "SET" update_list opt_from_query_parts opt_where
  ;

with_update_stmt:
  with_prefix update_stmt
  ;

update_stmt:
  "UPDATE" name "SET" update_list opt_from_query_parts opt_where opt_orderby opt_limit
  ;

update_entry:
  name '=' expr
  ;

update_list:
  update_entry
  | update_entry ',' update_list
  ;

with_upsert_stmt:
  with_prefix upsert_stmt
  ;

upsert_stmt:
  insert_stmt "ON CONFLICT" conflict_target "DO" "NOTHING"
  | insert_stmt "ON CONFLICT" conflict_target "DO" basic_update_stmt
  ;

update_cursor_stmt:
  "UPDATE" "CURSOR" name opt_column_spec "FROM" "VALUES" '(' insert_list ')'
  | "UPDATE" "CURSOR" name opt_column_spec from_shape
  | "UPDATE" "CURSOR" name "USING" expr_names
  ;

conflict_target:
  /* nil */
  | '(' indexed_columns ')' opt_where
  ;

function: "FUNC" | "FUNCTION"
  ;

declare_out_call_stmt:
  "DECLARE" "OUT" call_stmt
  ;

declare_enum_stmt:
  "DECLARE" "ENUM" name data_type_numeric '(' enum_values ')'
  ;

enum_values:
    enum_value
  | enum_value ',' enum_values
  ;

enum_value:
    name
  | name '=' expr
  ;

declare_const_stmt:
  "DECLARE" "CONST" "GROUP" name '(' const_values ')'
  ;

declare_group_stmt:
  "DECLARE" "GROUP" name "BEGIN" simple_variable_decls "END"
  ;

simple_variable_decls:
  declare_vars_stmt ';'
  | declare_vars_stmt ';' simple_variable_decls
  ;

const_values:
   const_value
  | const_value ',' const_values
  ;

const_value:  name '=' expr
  ;

declare_select_func_no_check_stmt:
  "DECLARE" "SELECT" function name "NO" "CHECK" data_type_with_options
  | "DECLARE" "SELECT" function name "NO" "CHECK" '(' typed_names ')'
  ;

declare_func_stmt:
  "DECLARE" function name '(' func_params ')' data_type_with_options
  | "DECLARE" "SELECT" function name '(' params ')' data_type_with_options
  | "DECLARE" function name '(' func_params ')' "CREATE" data_type_with_options
  | "DECLARE" "SELECT" function name '(' params ')' '(' typed_names ')'
  ;

procedure: "PROC" | "PROCEDURE"
  ;

declare_proc_no_check_stmt:
  "DECLARE" procedure name "NO" "CHECK"
  ;

declare_proc_stmt:
  "DECLARE" procedure name '(' params ')'
  | "DECLARE" procedure name '(' params ')' '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "USING" "TRANSACTION"
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "OUT" '(' typed_names ')' "USING" "TRANSACTION"
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')'
  | "DECLARE" procedure name '(' params ')' "OUT" "UNION" '(' typed_names ')' "USING" "TRANSACTION"
  ;

declare_interface_stmt:
  "DECLARE" "INTERFACE" name '(' typed_names ')'
  | "INTERFACE" name '(' typed_names ')'
  ;

create_proc_stmt:
  "CREATE" procedure name '(' params ')' "BEGIN" opt_stmt_list "END"
  ;

inout:
  "IN"
  | "OUT"
  | "INOUT"
  ;

typed_name:
  name data_type_with_options
  | shape_def
  | name shape_def
  ;

typed_names:
  typed_name
  | typed_name ',' typed_names
  ;

func_param:
  param
  | name "CURSOR"
  ;

func_params:
  /* nil */
  | func_param
  |  func_param ',' func_params
  ;

param:
  name data_type_with_options
  | inout name data_type_with_options
  | shape_def
  | name shape_def
  ;

params:
  /* nil */
  | param
  |  param ',' params
  ;

declare_value_cursor:
  "DECLARE" name "CURSOR" shape_def
  | "CURSOR" name shape_def
  | "DECLARE" name "CURSOR" "LIKE" select_stmt
  | "CURSOR" name "LIKE" select_stmt
  | "DECLARE" name "CURSOR" "LIKE" '(' typed_names ')'
  | "CURSOR" name "LIKE" '(' typed_names ')'
  ;

declare_forward_read_cursor_stmt:
  "DECLARE" name "CURSOR" "FOR" select_stmt
  | "CURSOR" name "FOR" select_stmt
  | "DECLARE" name "CURSOR" "FOR" explain_stmt
  | "CURSOR" name "FOR" explain_stmt
  | "DECLARE" name "CURSOR" "FOR" call_stmt
  | "CURSOR" name "FOR" call_stmt
  | "DECLARE" name "CURSOR" "FOR" expr
  | "CURSOR" name "FOR" expr
  ;

declare_fetched_value_cursor_stmt:
  "DECLARE" name "CURSOR" "FETCH" "FROM" call_stmt
  | "CURSOR" name "FETCH" "FROM" call_stmt
  ;

declare_type_stmt:
  "DECLARE" name "TYPE" data_type_with_options
  | "TYPE" name data_type_with_options
  ;

declare_vars_stmt:
  "DECLARE" name_list data_type_with_options
  | "VAR" name_list data_type_with_options
  | declare_value_cursor
  ;

call_stmt:
  "CALL" name '(' ')'
  | "CALL" name '(' call_expr_list ')'
  | "CALL" name '(' '*' ')'
  ;

while_stmt:
  "WHILE" expr "BEGIN" opt_stmt_list "END"
  ;

switch_stmt:
  "SWITCH" expr switch_case switch_cases
  | "SWITCH" expr "ALL" "VALUES" switch_case switch_cases
  ;

switch_case:
  "WHEN" expr_list "THEN" stmt_list
  | "WHEN" expr_list "THEN" "NOTHING"
  ;

switch_cases:
  switch_case switch_cases
  | "ELSE" stmt_list "END"
  | "END"
  ;

loop_stmt:
  "LOOP" fetch_stmt "BEGIN" opt_stmt_list "END"
  ;

leave_stmt:
  "LEAVE"
  ;

return_stmt:
  "RETURN"
  ;

rollback_return_stmt:
  "ROLLBACK" "RETURN"
  ;

commit_return_stmt:
  "COMMIT" "RETURN"
  ;

throw_stmt:
  "THROW"
  ;

trycatch_stmt:
  "BEGIN" "TRY" opt_stmt_list "END" "TRY" ';' "BEGIN" "CATCH" opt_stmt_list "END" "CATCH"
  ;

continue_stmt:
  "CONTINUE"
  ;

fetch_stmt:
  "FETCH" name "INTO" name_list
  | "FETCH" name
  ;

fetch_cursor_from_blob_stmt:
  "FETCH" name "FROM BLOB" expr
  ;

fetch_values_stmt:
  "FETCH" name opt_column_spec "FROM" "VALUES" '(' insert_list ')' opt_insert_dummy_spec
  | "FETCH" name opt_column_spec from_shape opt_insert_dummy_spec
  | "FETCH" name "USING" expr_names opt_insert_dummy_spec
  ;

expr_names:
  expr_name
  |  expr_name ',' expr_names
  ;

expr_name: expr as_alias
  ;

fetch_call_stmt:
  "FETCH" name opt_column_spec "FROM" call_stmt
  ;

close_stmt:
  "CLOSE" name
  ;

out_stmt:
  "OUT" name
  ;

out_union_stmt:
  "OUT" "UNION" name
  ;

out_union_parent_child_stmt:
  "OUT" "UNION" call_stmt "JOIN" child_results
  ;

child_results:
   child_result
   | child_result "AND" child_results
   ;

child_result:
  call_stmt "USING" '(' name_list ')'
  | call_stmt "USING" '(' name_list ')' "AS" name
  ;

if_stmt:
  "IF" expr "THEN" opt_stmt_list opt_elseif_list opt_else "END" "IF"
  ;

opt_else:
  /* nil */
  | "ELSE" opt_stmt_list
  ;

elseif_item:
  "ELSE IF" expr "THEN" opt_stmt_list
  ;

elseif_list:
  elseif_item
  | elseif_item elseif_list
  ;

opt_elseif_list:
  /* nil */
  | elseif_list
  ;

control_stmt:
  commit_return_stmt
  | continue_stmt
  | leave_stmt
  | return_stmt
  | rollback_return_stmt
  | throw_stmt

guard_stmt:
  "IF" expr control_stmt
  ;

transaction_mode:
  /* nil */
  | "DEFERRED"
  | "IMMEDIATE"
  | "EXCLUSIVE"
  ;

begin_trans_stmt:
  "BEGIN" transaction_mode "TRANSACTION"
  | "BEGIN" transaction_mode
  ;

rollback_trans_stmt:
  "ROLLBACK"
  | "ROLLBACK" "TRANSACTION"
  | "ROLLBACK" "TO" savepoint_name
  | "ROLLBACK" "TRANSACTION" "TO" savepoint_name
  | "ROLLBACK" "TO" "SAVEPOINT" savepoint_name
  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" savepoint_name
  ;

commit_trans_stmt:
  "COMMIT" "TRANSACTION"
  | "COMMIT"
  ;

proc_savepoint_stmt:  procedure "SAVEPOINT" "BEGIN" opt_stmt_list "END"
  ;

savepoint_name:
  "@PROC"
  | name
  ;

savepoint_stmt:
  "SAVEPOINT" savepoint_name
  ;

release_savepoint_stmt:
  "RELEASE" savepoint_name
  | "RELEASE" "SAVEPOINT" savepoint_name
  ;

echo_stmt:
  "@ECHO" name ',' str_literal
  ;

alter_table_add_column_stmt:
  "ALTER" "TABLE" name "ADD" "COLUMN" col_def
  ;

create_trigger_stmt:
  "CREATE" opt_temp "TRIGGER" opt_if_not_exists trigger_def opt_delete_version_attr
  ;

trigger_def:
  name trigger_condition trigger_operation "ON" name trigger_action
  ;

trigger_condition:
  /* nil */
  | "BEFORE"
  | "AFTER"
  | "INSTEAD" "OF"
 ;

trigger_operation:
  "DELETE"
  | "INSERT"
  | "UPDATE" opt_of
  ;

opt_of:
  /* nil */
  | "OF" name_list
  ;

trigger_action:
  opt_foreachrow opt_when_expr "BEGIN" trigger_stmts "END"
  ;

opt_foreachrow:
  /* nil */
  | "FOR EACH ROW"
  ;

opt_when_expr:
  /* nil */
  | "WHEN" expr
  ;

trigger_stmts:
  trigger_stmt
  | trigger_stmt  trigger_stmts
  ;

trigger_stmt:
  trigger_update_stmt ';'
  | trigger_insert_stmt ';'
  | trigger_delete_stmt ';'
  | trigger_select_stmt ';'
  ;

trigger_select_stmt:
  select_stmt_no_with
  ;

trigger_insert_stmt:
  insert_stmt
  ;

trigger_delete_stmt:
  delete_stmt
  ;

trigger_update_stmt:
  basic_update_stmt
  ;

enforcement_options:
  "FOREIGN" "KEY" "ON" "UPDATE"
  | "FOREIGN" "KEY" "ON" "DELETE"
  | "JOIN"
  | "UPSERT" "STATEMENT"
  | "WINDOW" function
  | "WITHOUT" "ROWID"
  | "TRANSACTION"
  | "SELECT" "IF" "NOTHING"
  | "INSERT" "SELECT"
  | "TABLE" "FUNCTION"
  | "ENCODE" "CONTEXT COLUMN"
  | "ENCODE" "CONTEXT TYPE" "INTEGER"
  | "ENCODE" "CONTEXT TYPE" "LONG_INTEGER"
  | "ENCODE" "CONTEXT TYPE" "REAL"
  | "ENCODE" "CONTEXT TYPE" "BOOL"
  | "ENCODE" "CONTEXT TYPE" "TEXT"
  | "ENCODE" "CONTEXT TYPE" "BLOB"
  | "IS TRUE"
  | "CAST"
  | "SIGN FUNCTION"
  | "CURSOR HAS ROW"
  | "UPDATE" "FROM"
  ;

enforce_strict_stmt:
  "@ENFORCE_STRICT" enforcement_options
  ;

enforce_normal_stmt:
  "@ENFORCE_NORMAL" enforcement_options
  ;

enforce_reset_stmt:
  "@ENFORCE_RESET"
  ;

enforce_push_stmt:
  "@ENFORCE_PUSH"
  ;

enforce_pop_stmt:
  "@ENFORCE_POP"
  ;

opt_use_offset:
  /* nil */
  | "OFFSET"
  ;

blob_get_key_type_stmt:
  "@BLOB_GET_KEY_TYPE" name
  ;

blob_get_val_type_stmt:
  "@BLOB_GET_VAL_TYPE" name
  ;

blob_get_key_stmt:
  "@BLOB_GET_KEY" name opt_use_offset
  ;

blob_get_val_stmt:
  "@BLOB_GET_VAL" name opt_use_offset
  ;

blob_create_key_stmt:
  "@BLOB_CREATE_KEY" name opt_use_offset
  ;

blob_create_val_stmt:
  "@BLOB_CREATE_VAL" name opt_use_offset
  ;

blob_update_key_stmt:
  "@BLOB_UPDATE_KEY" name opt_use_offset
  ;

blob_update_val_stmt:
  "@BLOB_UPDATE_VAL" name opt_use_offset
  ;

```

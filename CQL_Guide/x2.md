---
id: x2
title: "Appendix 2: CQL Grammar"
sidebar_label: "Appendix 2: CQL Grammar"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from a grammar snapshot with the tree building rules removed.
It should give a fair sense of the syntax of CQL (but not semantic validation).

Snapshot as of Tue Jan  5 14:53:53 PST 2021

### Operators and Literals

These are in order of priority lowest to highest

```
UNION_ALL UNION INTERSECT EXCEPT
ASSIGN
OR
AND
BETWEEN
NOT
'<>' '!=' '=' '==' LIKE NOT_LIKE GLOB MATCH REGEXP IN IS_NOT IS
'<' '>' '>=' '<='
'<<' '>>' '&' '|'
'+' '-'
'*' '/' '%'
UMINUS '~' COLLATE
CONCAT
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
EXCLUDE_GROUP EXCLUDE_CURRENT_ROW EXCLUDE_TIES EXCLUDE_NO_OTHERS CURRENT_ROW UNBOUNDED PRECEDING FOLLOWING
CREATE DROP TABLE WITHOUT ROWID PRIMARY KEY NULL_ DEFAULT CHECK AT_DUMMY_SEED VIRTUAL AT_EMIT_ENUMS
OBJECT TEXT BLOB LONG_ INT_ INTEGER LONG_INTEGER REAL ON UPDATE CASCADE ON_CONFLICT DO NOTHING
DELETE INDEX FOREIGN REFERENCES CONSTRAINT UPSERT STATEMENT CONST
INSERT INTO VALUES VIEW SELECT QUERY_PLAN EXPLAIN OVER WINDOW FILTER PARTITION RANGE ROWS GROUPS
AS CASE WHEN FROM THEN ELSE END LEFT
OUTER JOIN WHERE GROUP BY ORDER ASC
DESC INNER FCOUNT AUTOINCREMENT DISTINCT
LIMIT OFFSET TEMP TRIGGER IF ALL CROSS USING RIGHT
UNIQUE HAVING SET TO DISTINCTROW ENUM
FUNC FUNCTION PROC PROCEDURE BEGIN_ OUT INOUT CURSOR DECLARE FETCH LOOP LEAVE CONTINUE FOR
OPEN CLOSE ELSE_IF WHILE CALL TRY CATCH THROW RETURN
SAVEPOINT ROLLBACK COMMIT TRANSACTION RELEASE ARGUMENTS
CAST WITH RECURSIVE REPLACE IGNORE ADD COLUMN RENAME ALTER
AT_ECHO AT_CREATE AT_RECREATE AT_DELETE AT_SCHEMA_UPGRADE_VERSION AT_PREVIOUS_SCHEMA AT_SCHEMA_UPGRADE_SCRIPT
AT_PROC AT_FILE AT_ATTRIBUTE AT_SENSITIVE DEFERRED NOT_DEFERRABLE DEFERRABLE IMMEDIATE RESTRICT ACTION INITIALLY NO
BEFORE AFTER INSTEAD OF FOR_EACH_ROW EXISTS RAISE FAIL ABORT AT_ENFORCE_STRICT AT_ENFORCE_NORMAL
AT_BEGIN_SCHEMA_REGION AT_END_SCHEMA_REGION
AT_DECLARE_SCHEMA_REGION AT_DECLARE_DEPLOYABLE_REGION AT_SCHEMA_AD_HOC_MIGRATION PRIVATE
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
  | stmt ';' stmt_list
  ;

stmt:
  misc_attrs any_stmt
  ;

any_stmt: select_stmt
  | explain_stmt
  | create_trigger_stmt
  | create_table_stmt
  | create_virtual_table_stmt
  | create_index_stmt
  | create_view_stmt
  | alter_table_add_column_stmt
  | drop_table_stmt
  | drop_view_stmt
  | drop_index_stmt
  | drop_trigger_stmt
  | with_delete_stmt
  | delete_stmt
  | call_stmt
  | with_insert_stmt
  | insert_stmt
  | with_update_stmt
  | update_stmt
  | update_cursor_stmt
  | upsert_stmt
  | with_upsert_stmt
  | set_stmt
  | create_proc_stmt
  | declare_proc_stmt
  | declare_func_stmt
  | declare_stmt
  | declare_enum_stmt
  | fetch_stmt
  | fetch_values_stmt
  | fetch_call_stmt
  | while_stmt
  | loop_stmt
  | leave_stmt
  | return_stmt
  | rollback_return_stmt
  | commit_return_stmt
  | continue_stmt
  | if_stmt
  | open_stmt
  | close_stmt
  | out_stmt
  | out_union_stmt
  | throw_stmt
  | trycatch_stmt
  | begin_trans_stmt
  | rollback_trans_stmt
  | commit_trans_stmt
  | proc_savepoint_stmt
  | savepoint_stmt
  | release_savepoint_stmt
  | echo_stmt
  | schema_upgrade_version_stmt
  | schema_upgrade_script_stmt
  | previous_schema_stmt
  | enforce_strict_stmt
  | enforce_normal_stmt
  | declare_schema_region_stmt
  | declare_deployable_region_stmt
  | begin_schema_region_stmt
  | end_schema_region_stmt
  | schema_ad_hoc_migration_stmt
  | emit_enums_stmt
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

version_attrs_opt_recreate:
  /* nil */
  | "@RECREATE"
  | "@RECREATE" '(' name ')'
  | version_attrs
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

create_virtual_table_stmt: "CREATE" "VIRTUAL" "TABLE" opt_if_not_exists name
                           "USING" name opt_module_args
                           "AS" '(' col_key_list ')' opt_delete_version_attr ;

opt_module_args: /* nil */
  | '(' misc_attr_value_list ')'
  | '(' "ARGUMENTS" "FOLLOWING" ')'
  ;

create_table_stmt:
  "CREATE" opt_temp "TABLE" opt_if_not_exists name '(' col_key_list ')' opt_no_rowid version_attrs_opt_recreate
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

shape_def:
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
  misc_attrs col_name data_type col_attrs
  ;

pk_def:
  "PRIMARY" "KEY" '(' name_list ')'
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
  "FOREIGN" "KEY" '(' name_list ')' fk_target_options
  ;

fk_target_options:
  "REFERENCES" name '(' name_list ')' opt_fk_options
  ;

unq_def:
  "CONSTRAINT" name "UNIQUE" '(' name_list ')'
  | "UNIQUE" '(' name_list ')'
  ;

opt_unique:
  /* nil */
  | "UNIQUE"
  ;

indexed_column:
  name opt_asc_desc
  ;

indexed_columns:
  indexed_column
  | indexed_column ',' indexed_columns
  ;

create_index_stmt:
  "CREATE" opt_unique "INDEX" opt_if_not_exists name "ON" name '(' indexed_columns ')' opt_delete_version_attr
  ;

name:
  "ID"
  | "TEXT"
  | "TRIGGER"
  | "ROWID"
  | "KEY"
  | "VIRTUAL"
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

col_attrs:
  /* nil */
  | "NOT" "NULL" col_attrs
  | "PRIMARY" "KEY" col_attrs
  | "PRIMARY" "KEY" "AUTOINCREMENT" col_attrs
  | "DEFAULT" '-' num_literal col_attrs
  | "DEFAULT" num_literal col_attrs
  | "DEFAULT" const_expr col_attrs
  | "DEFAULT" str_literal col_attrs
  | "COLLATE" name col_attrs
  | "CHECK" '(' expr ')' col_attrs
  | "UNIQUE" col_attrs
  | "@SENSITIVE" col_attrs
  | "@CREATE" version_annotation col_attrs
  | "@DELETE" version_annotation col_attrs
  | fk_target_options col_attrs
  ;

version_annotation:
  '(' "integer-literal" ',' name ')'
  | '(' "integer-literal" ')'
  ;

object_type:
  "OBJECT"
  | "OBJECT" '<' name '>'
  | "OBJECT" '<' name "CURSOR" '>'
  ;

data_type_numeric:
  "INT"
  | "INTEGER"
  | "REAL"
  | "LONG"
  | "BOOL"
  | "LONG" "INTEGER"
  | "LONG" "INT"
  | "LONG_INT" | "LONG_INTEGER"

data_type:
  data_type_numeric
  | "TEXT"
  | "BLOB"
  | object_type
  ;

data_type_opt_notnull:
  data_type
  | data_type "NOT" "NULL"
  | data_type "@SENSITIVE"
  | data_type "@SENSITIVE" "NOT" "NULL"
  | data_type "NOT" "NULL" "@SENSITIVE"
  ;

str_literal:
  "sql-string-literal"
  | "c-string-literal"
  ;

num_literal:
  "integer-literal"
  | "long-literal"
  | "real-literal"
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
  ;

basic_expr:
  name
  | name '.' name
  | any_literal
  | const_expr
  | '(' expr ')'
  | call
  | window_func_inv
  | raise_expr
  | '(' select_stmt ')'
  | "EXISTS" '(' select_stmt ')'
  ;

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
  | '-' math_expr
  | math_expr "||" math_expr
  ;

expr:
  basic_expr
  | expr '&' expr
  | expr '|' expr
  | expr "<<" expr
  | expr ">>" expr
  | expr '+' expr
  | expr '-' expr
  | expr '*' expr
  | expr '/' expr
  | expr '%' expr
  | '-' expr
  | "NOT" expr
  | '~' expr
  | expr "COLLATE" name
  | expr "AND" expr
  | expr "OR" expr
  | expr '=' expr
  | expr "==" expr
  | expr '<' expr
  | expr '>' expr
  | expr "<>" expr
  | expr "!=" expr
  | expr ">=" expr
  | expr "<=" expr
  | expr "NOT" "IN" '(' expr_list ')'
  | expr "NOT" "IN" '(' select_stmt ')'
  | expr "IN" '(' expr_list ')'
  | expr "IN" '(' select_stmt ')'
  | expr "LIKE" expr
  | expr "NOT LIKE" expr
  | expr "MATCH" expr
  | expr "REGEXP" expr
  | expr "GLOB" expr
  | expr "NOT" "BETWEEN" math_expr "AND" math_expr
  | expr "BETWEEN" math_expr "AND" math_expr
  | expr "IS NOT" expr
  | expr "IS" expr
  | expr "||" expr
  | "CASE" expr case_list "END"
  | "CASE" expr case_list "ELSE" expr "END"
  | "CASE" case_list "END"
  | "CASE" case_list "ELSE" expr "END"
  | "CAST" '(' expr "AS" data_type ')'
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

cte_table:
    name '(' name_list ')' "AS" '(' select_stmt_no_with ')'
  | name '(' '*' ')' "AS" '(' select_stmt_no_with ')'
  ;

with_prefix:
  "WITH" cte_tables
  | "WITH" "RECURSIVE" cte_tables
  ;

with_select_stmt:
  with_prefix select_stmt_no_with
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

schema_ad_hoc_migration_stmt:
  "@SCHEMA_AD_HOC_MIGRATION" version_annotation
  ;

emit_enums_stmt:
  "@EMIT_ENUMS" opt_name_list
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
  expr opt_asc_desc
  ;

opt_asc_desc:
  /* nil */
  | "ASC"
  | "DESC"
  ;

opt_having:
  /* nil */
  | "HAVING" expr
  ;

opt_orderby:
  /* nil */
  | "ORDER" "BY" groupby_list
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
  |  name '.' '*'
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
  | insert_stmt_type name "USING" expr_names opt_insert_dummy_spec
  ;

insert_list:
  /* nil */
  | expr
  | expr ',' insert_list
  ;

basic_update_stmt:
  "UPDATE" opt_name "SET" update_list opt_where
  ;

with_update_stmt:
  with_prefix update_stmt
  ;

update_stmt:
  "UPDATE" name "SET" update_list opt_where opt_orderby opt_limit
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

creation_type:
  object_type
  | object_type "NOT" "NULL"
  | "TEXT"
  | "TEXT" "NOT" "NULL"
  | "BLOB"
  | "BLOB" "NOT" "NULL"
  ;

function: "FUNC" | "FUNCTION"
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

declare_func_stmt:
  "DECLARE" function name '(' params ')' data_type_opt_notnull
  | "DECLARE" "SELECT" function name '(' params ')' data_type_opt_notnull
  | "DECLARE" function name '(' params ')' "CREATE" creation_type
  | "DECLARE" "SELECT" function name '(' params ')' '(' typed_names ')'
  ;

procedure: "PROC" | "PROCEDURE"
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

create_proc_stmt:
  "CREATE" procedure name '(' params ')' "BEGIN" opt_stmt_list "END"
  ;

inout:
  "IN"
  | "OUT"
  | "INOUT"
  ;

typed_name:
  name data_type_opt_notnull
  | shape_def
  | name shape_def
  ;

typed_names:
  typed_name
  | typed_name ',' typed_names
  ;

param:
  name data_type_opt_notnull
  | inout name data_type_opt_notnull
  | shape_def
  | name shape_def
  ;

params:
  /* nil */
  | param
  |  param ',' params
  ;

declare_stmt:
  "DECLARE" name_list data_type_opt_notnull
  | "DECLARE" name "CURSOR" "FOR" select_stmt
  | "DECLARE" name "CURSOR" "FOR" explain_stmt
  | "DECLARE" name "CURSOR" "FOR" call_stmt
  | "DECLARE" name "CURSOR" "FETCH" "FROM" call_stmt
  | "DECLARE" name "CURSOR" shape_def
  | "DECLARE" name "CURSOR" "LIKE" select_stmt
  | "DECLARE" name "CURSOR" "FOR" name
  ;

call_stmt:
  "CALL" name '(' ')'
  | "CALL" name '(' call_expr_list ')'
  ;

while_stmt:
  "WHILE" expr "BEGIN" opt_stmt_list "END"
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

open_stmt:
  "OPEN" name
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

begin_trans_stmt:
  "BEGIN" "TRANSACTION"
  ;

rollback_trans_stmt:
  "ROLLBACK" "TRANSACTION"
  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" name
  | "ROLLBACK" "TRANSACTION" "TO" "SAVEPOINT" "@PROC"
  ;

commit_trans_stmt:
  "COMMIT" "TRANSACTION"
  ;

proc_savepoint_stmt:  procedure "SAVEPOINT" "BEGIN" opt_stmt_list "END"
  ;

savepoint_stmt:
  "SAVEPOINT" name
  | "SAVEPOINT" "@PROC"
  ;

release_savepoint_stmt:
  "RELEASE" "SAVEPOINT" name
  | "RELEASE" "SAVEPOINT" "@PROC"
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
  | "FOR" "EACH" "ROW"
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
  | procedure
  | "WITHOUT" "ROWID"
  ;

enforce_strict_stmt:
  "@ENFORCE_STRICT" enforcement_options
  ;

enforce_normal_stmt:
  "@ENFORCE_NORMAL" enforcement_options
  ;

```

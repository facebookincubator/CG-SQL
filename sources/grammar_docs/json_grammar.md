---
id: x5
title: "Appendix 5: JSON Schema Grammar"
sidebar_label: "Appendix 5: JSON Schema Grammar"
---
<!---
-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
-->

What follows is taken from the JSON validation grammar with the tree building rules removed.

Snapshot as of Thu Jan 28 17:31:07 PST 2021

### Rules

```


json_schema: '{'
         '"tables"' ':' '[' opt_tables ']' ','
         '"virtualTables"' ':' '[' opt_virtual_tables ']' ','
         '"views"' ':' '[' opt_views ']' ','
         '"indices"' ':' '[' opt_indices ']' ','
         '"triggers"' ':' '[' opt_triggers ']' ','
         '"attributes"' ':' '[' opt_attribute_list ']' ','
         '"queries"' ':' '[' opt_queries ']' ','
         '"inserts"' ':' '[' opt_inserts ']' ','
         '"generalInserts"' ':' '[' opt_inserts_general ']' ','
         '"updates"' ':' '[' opt_updates ']' ','
         '"deletes"' ':' '[' opt_deletes ']' ','
         '"general"' ':' '[' opt_generals ']' ','
         '"regions"' ':' '[' opt_regions ']' ','
         '"adHocMigrationProcs"' ':' '[' opt_ad_hoc_migrations ']' ','
         '"enums"' ':'  '[' opt_enums ']'
         '}'
  ;

BOOL_LITERAL: '0' | '1'
  ;

opt_tables: | tables
  ;

tables: table | table ',' tables
  ;

table: '{'
       '"name"' ':' STRING_LITERAL ','
       '"isTemp"' ':' BOOL_LITERAL ','
       '"ifNotExists"' ':' BOOL_LITERAL ','
       '"withoutRowid"' ':' BOOL_LITERAL ','
       '"isAdded"' ':' BOOL_LITERAL ','
       opt_added_version
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       '"isRecreated"' ':' BOOL_LITERAL ','
       opt_recreate_group_name
       opt_region_info
       opt_table_indices
       opt_attributes
       '"columns"' ':' '[' columns ']' ','
       '"primaryKey"' ':' '[' opt_column_names ']' ','
       opt_primary_key_name
       '"foreignKeys"' ':' '[' opt_foreign_keys ']' ','
       '"uniqueKeys"' ':' '[' opt_unique_keys ']' ','
       '"checkExpressions"' ':' '[' opt_check_expressions ']'
       '}'
  ;

opt_primary_key_name:  | '"primaryKeyName"' ':' STRING_LITERAL ','
  ;

opt_virtual_tables: | virtual_tables
  ;

virtual_tables: virtual_table | virtual_table ',' virtual_tables
  ;

virtual_table: '{'
       '"name"' ':' STRING_LITERAL ','
       '"isTemp"' ':' '0' ','
       '"ifNotExists"' ':' BOOL_LITERAL ','
       '"withoutRowid"' ':' '0' ','
       '"isAdded"' ':' BOOL_LITERAL ','
       opt_added_version
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       '"isRecreated"' ':' BOOL_LITERAL ','
       opt_region_info
       '"isVirtual"' ':' '1' ','
       '"module"' ':' STRING_LITERAL ','
       opt_module_args
       opt_attributes
       '"columns"' ':' '[' columns ']' ','
       '"primaryKey"' ':' '[' opt_column_names ']' ','
       '"foreignKeys"' ':' '[' opt_foreign_keys ']' ','
       '"uniqueKeys"' ':' '[' opt_unique_keys ']' ','
       '"checkExpressions"' ':' '[' opt_check_expressions ']'
       '}'
  ;

opt_module_args: | '"moduleArgs"' ':'  STRING_LITERAL ','
  ;

opt_added_version: | '"addedVersion"' ':' any_integer ',' opt_added_migration_proc
  ;

opt_added_migration_proc: | '"addedMigrationProc"' ':' STRING_LITERAL ','
  ;

opt_deleted_version: | '"deletedVersion"' ':' any_integer ',' opt_deleted_migration_proc
  ;

opt_deleted_migration_proc: | '"deletedMigrationProc"' ':' STRING_LITERAL ','
  ;

opt_recreate_group_name: | '"recreateGroupName"' ':' STRING_LITERAL ','
  ;

opt_index_names: | index_names
  ;

index_names: STRING_LITERAL | STRING_LITERAL ',' index_names
  ;

opt_arg_names: | arg_names
  ;

arg_names: STRING_LITERAL | STRING_LITERAL ',' arg_names
  ;

opt_column_names: | column_names
  ;

column_names: STRING_LITERAL | STRING_LITERAL ',' column_names
  ;

opt_table_names: | table_names
  ;

table_names: STRING_LITERAL | STRING_LITERAL ',' table_names
  ;

opt_view_names: | view_names
  ;

view_names: STRING_LITERAL | STRING_LITERAL ',' view_names
  ;

opt_procedure_names: | procedure_names
  ;

procedure_names: STRING_LITERAL | STRING_LITERAL ',' procedure_names
  ;

sort_order_names: STRING_LITERAL | STRING_LITERAL ',' sort_order_names
  ;

columns: column | column ',' columns
  ;

column: '{'
        '"name"' ':' STRING_LITERAL ','
        opt_attributes
        '"type"' ':' STRING_LITERAL ','
        opt_is_sensitive
        '"isNotNull"' ':' BOOL_LITERAL ','
        '"isAdded"' ':' BOOL_LITERAL ','
        opt_added_version
        '"isDeleted"' ':' BOOL_LITERAL ','
        opt_deleted_version
        opt_default_value
        opt_collate
        opt_check_expr
        '"isPrimaryKey"' ':' BOOL_LITERAL ','
        '"isUniqueKey"' ':' BOOL_LITERAL ','
        '"isAutoIncrement"' ':' BOOL_LITERAL
        '}'
  ;

opt_collate : | '"collate"' ':' STRING_LITERAL ','
  ;

opt_check_expr: | '"checkExpr"' ':' STRING_LITERAL ',' '"checkExprArgs"' ':' '[' opt_arg_names ']' ','
  ;

opt_default_value: | '"defaultValue"' ':' any_literal ','
  ;

opt_foreign_keys : | foreign_keys
  ;

opt_is_sensitive: | '"isSensitive"' ':' '1' ','
  ;

foreign_keys :  foreign_key | foreign_key ',' foreign_keys
  ;

foreign_key : '{'
               opt_name
               '"columns"' ':' '[' column_names ']' ','
               '"referenceTable"' ':' STRING_LITERAL ','
               '"referenceColumns"' ':' '[' column_names ']' ','
               '"onUpdate"' ':' STRING_LITERAL ','
               '"onDelete"' ':' STRING_LITERAL ','
               '"isDeferred"' ':' BOOL_LITERAL
              '}'
  ;

opt_unique_keys :  | unique_keys
  ;

unique_keys : unique_key | unique_key ',' unique_keys
  ;

unique_key:  '{'
              opt_name
              '"columns"' ':' '[' column_names ']'
             '}'
  ;

opt_check_expressions: | check_expressions
  ;

check_expressions: check_expression | check_expression ',' check_expressions
  ;

check_expression: '{'
                   opt_name
                   '"checkExpr"' ':' STRING_LITERAL ','
                   '"checkExprArgs"' ':' '[' ']'
                  '}'
  ;

opt_name: | '"name"' ':' STRING_LITERAL ','
  ;

opt_table_indices: | table_indices
  ;

table_indices: '"indices"' ':' '[' opt_index_names ']' ','
  ;

opt_attributes:  | attributes
  ;

attributes: '"attributes"' ':' '[' attribute_list ']' ','
  ;

opt_attribute_list: | attribute_list
  ;

attribute_list: attribute | attribute ',' attribute_list
  ;

attribute:  '{'
             '"name"' ':' STRING_LITERAL ','
             '"value"' ':' attribute_value
            '}'
  ;

attribute_array: '[' opt_attribute_value_list ']'
  ;

opt_attribute_value_list: | attribute_value_list
  ;

attribute_value_list: attribute_value | attribute_value ',' attribute_value_list
  ;

attribute_value: any_literal | attribute_array
  ;

any_integer: BOOL_LITERAL | INT_LITERAL
  ;

any_literal:  BOOL_LITERAL |
              INT_LITERAL | '-' INT_LITERAL |
              LONG_LITERAL | '-' LONG_LITERAL |
              REAL_LITERAL | '-' REAL_LITERAL |
              STRING_LITERAL | NULL_LITERAL
  ;

num_literal:  BOOL_LITERAL |
              INT_LITERAL | '-' INT_LITERAL |
              LONG_LITERAL | '-' LONG_LITERAL |
              REAL_LITERAL | '-' REAL_LITERAL
  ;

opt_views: | views
  ;

views: view | view ',' views
  ;

view:  '{'
       '"name"' ':' STRING_LITERAL ','
       '"isTemp"' ':' BOOL_LITERAL ','
       '"isDeleted"' ':' BOOL_LITERAL ','
       opt_deleted_version
       opt_region_info
       projection
       '"select"' ':' STRING_LITERAL ','
       '"selectArgs"' ':' '[' ']'
       '}'
  ;

opt_region_info: | '"region"' ':' STRING_LITERAL ',' |  '"region"' ':' STRING_LITERAL ',' '"deployedInRegion"' ':' STRING_LITERAL ','
  ;

opt_projection: | projection
  ;

projection: '"projection"' ':' '[' projected_columns ']' ','
  ;

projected_columns: projected_column | projected_column ',' projected_columns
  ;

projected_column: '{'
                   '"name"' ':' STRING_LITERAL ','
                   '"type"' ':' STRING_LITERAL ','
                   opt_is_sensitive
                   '"isNotNull"' ':' BOOL_LITERAL
                  '}'
  ;

opt_indices:  | indices
  ;

indices: index  | index ',' indices
  ;

index: '{'
        '"name"' ':' STRING_LITERAL ','
        '"table"' ':' STRING_LITERAL ','
        '"isUnique"' ':' BOOL_LITERAL ','
        '"ifNotExists"' ':' BOOL_LITERAL ','
        '"isDeleted"' ':' BOOL_LITERAL ','
        opt_deleted_version
        opt_region_info
        '"columns"' ':' '[' column_names ']' ','
        '"sortOrders"' ':' '[' sort_order_names ']'
       '}'
  ;

opt_triggers: | triggers
  ;

triggers: trigger | trigger ',' triggers
  ;

trigger: '{'
          '"name"' ':' STRING_LITERAL ','
          '"target"' ':' STRING_LITERAL ','
          '"isTemp"' ':' BOOL_LITERAL ','
          '"ifNotExists"' ':' BOOL_LITERAL ','
          '"isDeleted"' ':' BOOL_LITERAL ','
          opt_deleted_version
          before_after_instead ','
          delete_insert_update ','
          opt_for_each_row
          opt_when_expr
          '"statement"' ':' STRING_LITERAL ','
          '"statementArgs"' ':' '[' opt_arg_names ']' ','
          opt_region_info
          dependencies
         '}'
  ;

before_after_instead: '"isBeforeTrigger"' ':' '1' | '"isAfterTrigger"' ':' '1'  | '"isInsteadOfTrigger"' ':' '1'
  ;

delete_insert_update: '"isDeleteTrigger"' ':' '1' | '"isInsertTrigger"' ':' '1' | '"isUpdateTrigger"' ':' '1'
  ;

opt_for_each_row: | '"forEachRow"' ':' BOOL_LITERAL ','
  ;

opt_when_expr: | '"whenExpr"' ':' STRING_LITERAL ',' '"whenExprArgs"' ':' '[' opt_arg_names ']' ','
  ;

dependencies: opt_insert_tables
            opt_update_tables
            opt_delete_tables
            opt_from_tables
            opt_uses_procedures
            opt_uses_views
            '"usesTables"' ':' '[' opt_table_names ']'
  ;

opt_uses_views: | '"usesViews"' ':' '[' opt_view_names ']' ','
  ;

opt_insert_tables: | '"insertTables"' ':' '[' opt_table_names ']' ','
  ;

opt_update_tables: | '"updateTables"' ':' '[' opt_table_names ']' ','
  ;

opt_delete_tables: | '"deleteTables"' ':' '[' opt_table_names ']' ','
  ;

opt_from_tables: | '"fromTables"' ':' '[' opt_table_names ']' ','
  ;

opt_uses_procedures : | '"usesProcedures"' ':' '[' opt_procedure_names ']' ','
  ;

opt_queries: | queries ;

queries: query | query ',' queries ;

query: '{'
       '"name"' ':' STRING_LITERAL ','
       '"definedInFile"' ':' STRING_LITERAL ','
       '"args"' ':' '[' opt_args ']' ','
       dependencies ','
       opt_region_info
       opt_attributes
       projection
       '"statement"' ':' STRING_LITERAL ','
       '"statementArgs"' ':' '[' opt_arg_names ']'
       '}'
  ;

opt_args: | args
  ;

args: arg | arg ',' args
  ;

arg: '{'
      '"name"' ':' STRING_LITERAL ','
      '"argOrigin"' ':' STRING_LITERAL ','
      '"type"' ':' STRING_LITERAL ','
      opt_is_sensitive
      '"isNotNull"' ':' BOOL_LITERAL
      '}'
  ;

opt_inserts: | inserts
  ;

inserts: insert | insert ',' inserts
  ;

insert : '{' insert_details ',' '"values"' ':' '[' opt_values ']' '}'
  ;

opt_inserts_general: | inserts_general
  ;

inserts_general: insert_general | insert_general ',' inserts_general
  ;

insert_details:
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']' ','
         '"statementType"' ':' STRING_LITERAL ','
         '"columns"' ':' '[' column_names ']'

insert_general : '{' insert_details '}'
  ;

opt_values: | values
  ;

values: value | value ',' values
  ;

value:  '{'
         '"value"' ':' STRING_LITERAL ','
         '"valueArgs"' ':' '[' opt_arg_names ']'
        '}'
  ;

opt_updates: | updates
  ;

updates: update | update ',' updates
  ;

update : '{'
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']'
         '}'
  ;

opt_deletes: | deletes
  ;

deletes: delete | delete ',' deletes
  ;

delete : '{'
         '"name"' ':' STRING_LITERAL ','
         '"definedInFile"' ':' STRING_LITERAL ','
         '"args"' ':' '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         '"table"' ':' STRING_LITERAL ','
         '"statement"' ':' STRING_LITERAL ','
         '"statementArgs"' ':' '[' opt_arg_names ']'
         '}'
  ;

opt_generals: | generals
  ;

generals: general | general ',' generals
  ;

general: '{'
          '"name"' ':' STRING_LITERAL ','
          '"definedInFile"' ':' STRING_LITERAL ','
          '"args"' ':' '[' opt_complex_args ']' ','
          dependencies ','
          opt_regions
          opt_attributes
          opt_projection
          opt_result_contract
          '"usesDatabase"' ':' BOOL_LITERAL
         '}'
  ;

opt_result_contract: | '"hasSelectResult"' ':' '1' ',' | '"hasOutResult"' ':' '1' ',' | '"hasOutUnionResult"' ':''1' ','
  ;

opt_complex_args: | complex_args
  ;

complex_args: complex_arg | complex_arg ',' complex_args
  ;

complex_arg: '{'
              binding
              '"name"' ':' STRING_LITERAL ','
              '"argOrigin"' ':' STRING_LITERAL ','
              '"type"' ':' STRING_LITERAL ','
              opt_is_sensitive
              '"isNotNull"' ':' BOOL_LITERAL
             '}'
  ;

binding: | '"binding"' ':' '"inout"' ',' | '"binding"' ':' '"out"' ','
  ;

opt_enums: | enums
  ;

enums: enum | enum ',' enums
  ;

enum: '{'
      '"name"' ':' STRING_LITERAL ','
      '"type"' ':' STRING_LITERAL ','
      '"isNotNull"' ':' '1' ','
      '"values"' ':' '[' enum_values ']'
      '}'
  ;

enum_values: enum_value | enum_value ',' enum_values
  ;

enum_value: '{'
             '"name"' ':' STRING_LITERAL ','
             '"value"' ':' num_literal
            '}'
  ;

opt_regions: | regions
  ;

regions: region | region ',' regions
  ;

region:  '{'
          '"name"' ':' STRING_LITERAL ','
          '"isDeployableRoot"' ':' BOOL_LITERAL ','
          '"deployedInRegion"' ':' STRING_LITERAL ','
          '"using"' ':' '[' opt_region_names ']' ','
          '"usingPrivately"' ':' '[' opt_bool_list ']'
         '}'
  ;

opt_region_names: | region_names
  ;

region_names: STRING_LITERAL | STRING_LITERAL ',' region_names
  ;

opt_bool_list: | bool_list
  ;

bool_list: BOOL_LITERAL | BOOL_LITERAL ',' bool_list
  ;

opt_ad_hoc_migrations: | ad_hoc_migrations
  ;

ad_hoc_migrations: ad_hoc_migration | ad_hoc_migration ',' ad_hoc_migrations
  ;

ad_hoc_migration: '{'
                  '"name"' ':' STRING_LITERAL ','
                  '"version"' ':' any_integer
                  '}'
  ;

```

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// In case there is any doubt, 'json_test.y' is included in the license as well as
// the code bision generates from it.

%{

#include <inttypes.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

// The stack needed is modest (32k) and this prevents leaks in error cases because
// it's just a stack alloc.
#define YYSTACK_USE_ALLOCA 1

int yylex();
void yyerror(const char *s, ...);
void yyset_in(FILE *);
void yyset_lineno(int);

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wimplicit-int-conversion"
#pragma clang diagnostic ignored "-Wconversion"

%}

%define parse.error verbose

%union {
  char *sval;
}

%token NULL_LITERAL
%token STRING_LITERAL
%token INT_LITERAL
%token LONG_LITERAL
%token REAL_LITERAL
%token TABLES CRC VIRTUAL_TABLES MODULE MODULE_ARGS
%token NAME ARG_ORIGIN IS_TEMP IS_VIRTUAL IF_NOT_EXISTS WITHOUT_ROWID IS_ADDED IS_DELETED IS_RECREATED REGION DEPLOYED_IN_REGION
%token ADDED_VERSION DELETED_VERSION ADDED_MIGRATION_PROC DELETED_MIGRATION_PROC RECREATE_GROUP_NAME
%token COLUMNS
%token TYPE KIND IS_NOT_NULL IS_PRIMARY_KEY IS_UNIQUE_KEY IS_AUTO_INCREMENT IS_SENSITIVE
%token PRIMARY_KEY PRIMARY_KEY_SORT_ORDERS PRIMARY_KEY_NAME FOREIGN_KEYS UNIQUE_KEYS
%token REFERENCE_TABLE REFERENCE_COLUMNS ON_UPDATE ON_DELETE IS_DEFERRED
%token ATTRIBUTES VALUE DEFAULT_VALUE VALUES
%token VIEWS PROJECTION SELECT SELECT_ARGS
%token INDICES SORT_ORDERS TABLE IS_UNIQUE WHERE
%token TRIGGERS DELETE_TABLES FOR_EACH_ROW FROM_TABLES INSERT_TABLES
%token IS_INSTEAD_OF_TRIGGER IS_BEFORE_TRIGGER IS_DELETE_TRIGGER IS_AFTER_TRIGGER IS_INSERT_TRIGGER IS_UPDATE_TRIGGER
%token STATEMENT STATEMENT_ARGS TARGET UPDATE_TABLES USES_PROCEDURES USES_VIEWS USES_TABLES WHEN_EXPR WHEN_EXPR_ARGS
%token QUERIES ARGS DEFINED_IN_FILE VALUE_ARGS STATEMENT_TYPE INSERTS UPDATES DELETES GENERAL_INSERTS
%token USES_DATABASE HAS_SELECT_RESULT HAS_OUT_UNION_RESULT HAS_OUT_RESULT REGIONS GENERAL
%token USING USING_PRIVATELY IS_DEPLOYABLE_ROOT AD_HOC_MIGRATION_PROCS VERSION
%token BINDING_INOUT BINDING_OUT COLLATE CHECK_EXPR CHECK_EXPR_ARGS CHECK_EXPRESSIONS
%token ENUMS

%start json_schema

%%

json_schema: '{'
         TABLES '[' opt_tables ']' ','
         VIRTUAL_TABLES '[' opt_virtual_tables ']' ','
         VIEWS '[' opt_views ']' ','
         INDICES '[' opt_indices ']' ','
         TRIGGERS '[' opt_triggers ']' ','
         ATTRIBUTES '[' opt_attribute_list ']' ','
         QUERIES '[' opt_queries ']' ','
         INSERTS '[' opt_inserts ']' ','
         GENERAL_INSERTS '[' opt_inserts_general ']' ','
         UPDATES '[' opt_updates ']' ','
         DELETES '[' opt_deletes ']' ','
         GENERAL '[' opt_generals ']' ','
         REGIONS '[' opt_regions ']' ','
         AD_HOC_MIGRATION_PROCS '[' opt_ad_hoc_migrations ']' ','
         ENUMS  '[' opt_enums ']'
         '}'
  ;

BOOL_LITERAL: '0' | '1'
  ;

opt_tables: | tables
  ;

tables: table | table ',' tables
  ;

table: '{'
       NAME STRING_LITERAL ','
       CRC STRING_LITERAL ','
       IS_TEMP BOOL_LITERAL ','
       IF_NOT_EXISTS BOOL_LITERAL ','
       WITHOUT_ROWID BOOL_LITERAL ','
       IS_ADDED BOOL_LITERAL ','
       opt_added_version
       IS_DELETED BOOL_LITERAL ','
       opt_deleted_version
       IS_RECREATED BOOL_LITERAL ','
       opt_recreate_group_name
       opt_region_info
       opt_table_indices
       opt_attributes
       COLUMNS '[' columns ']' ','
       PRIMARY_KEY '[' opt_column_names ']' ','
       PRIMARY_KEY_SORT_ORDERS '[' opt_sort_order_names ']' ','
       opt_primary_key_name
       FOREIGN_KEYS '[' opt_foreign_keys ']' ','
       UNIQUE_KEYS '[' opt_unique_keys ']' ','
       CHECK_EXPRESSIONS '[' opt_check_expressions ']'
       '}'
  ;

opt_primary_key_name:  | PRIMARY_KEY_NAME STRING_LITERAL ','
  ;

opt_virtual_tables: | virtual_tables
  ;

virtual_tables: virtual_table | virtual_table ',' virtual_tables
  ;

virtual_table: '{'
       NAME STRING_LITERAL ','
       CRC STRING_LITERAL ','
       IS_TEMP '0' ','
       IF_NOT_EXISTS BOOL_LITERAL ','
       WITHOUT_ROWID '0' ','
       IS_ADDED BOOL_LITERAL ','
       opt_added_version
       IS_DELETED BOOL_LITERAL ','
       opt_deleted_version
       IS_RECREATED BOOL_LITERAL ','
       opt_region_info
       IS_VIRTUAL '1' ','
       MODULE STRING_LITERAL ','
       opt_module_args
       opt_attributes
       COLUMNS '[' columns ']' ','
       PRIMARY_KEY '[' opt_column_names ']' ','
       PRIMARY_KEY_SORT_ORDERS '[' opt_sort_order_names ']' ','
       FOREIGN_KEYS '[' opt_foreign_keys ']' ','
       UNIQUE_KEYS '[' opt_unique_keys ']' ','
       CHECK_EXPRESSIONS '[' opt_check_expressions ']'
       '}'
  ;

opt_module_args: | MODULE_ARGS  STRING_LITERAL ','
  ;

opt_added_version: | ADDED_VERSION any_integer ',' opt_added_migration_proc
  ;

opt_added_migration_proc: | ADDED_MIGRATION_PROC STRING_LITERAL ','
  ;

opt_deleted_version: | DELETED_VERSION any_integer ',' opt_deleted_migration_proc
  ;

opt_deleted_migration_proc: | DELETED_MIGRATION_PROC STRING_LITERAL ','
  ;

opt_recreate_group_name: | RECREATE_GROUP_NAME STRING_LITERAL ','
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

opt_sort_order_names: | sort_order_names
  ;

sort_order_names: STRING_LITERAL | STRING_LITERAL ',' sort_order_names
  ;

columns: column | column ',' columns
  ;

column: '{'
        NAME STRING_LITERAL ','
        opt_attributes
        TYPE STRING_LITERAL ','
        opt_kind
        opt_is_sensitive
        IS_NOT_NULL BOOL_LITERAL ','
        IS_ADDED BOOL_LITERAL ','
        opt_added_version
        IS_DELETED BOOL_LITERAL ','
        opt_deleted_version
        opt_default_value
        opt_collate
        opt_check_expr
        IS_PRIMARY_KEY BOOL_LITERAL ','
        IS_UNIQUE_KEY BOOL_LITERAL ','
        IS_AUTO_INCREMENT BOOL_LITERAL
        '}'
  ;

opt_collate : | COLLATE STRING_LITERAL ','
  ;

opt_check_expr: | CHECK_EXPR STRING_LITERAL ',' CHECK_EXPR_ARGS '[' opt_arg_names ']' ','
  ;

opt_default_value: | DEFAULT_VALUE any_literal ','
  ;

opt_foreign_keys : | foreign_keys
  ;

opt_kind: | KIND STRING_LITERAL ','
  ;

opt_is_sensitive: | IS_SENSITIVE '1' ','
  ;

foreign_keys :  foreign_key | foreign_key ',' foreign_keys
  ;

foreign_key : '{'
               opt_name
               COLUMNS '[' column_names ']' ','
               REFERENCE_TABLE STRING_LITERAL ','
               REFERENCE_COLUMNS '[' column_names ']' ','
               ON_UPDATE STRING_LITERAL ','
               ON_DELETE STRING_LITERAL ','
               IS_DEFERRED BOOL_LITERAL
              '}'
  ;

opt_unique_keys :  | unique_keys
  ;

unique_keys : unique_key | unique_key ',' unique_keys
  ;

unique_key:  '{'
              opt_name
              COLUMNS '[' column_names ']' ','
              SORT_ORDERS '[' sort_order_names ']'
             '}'
  ;

opt_check_expressions: | check_expressions
  ;

check_expressions: check_expression | check_expression ',' check_expressions
  ;

check_expression: '{'
                   opt_name
                   CHECK_EXPR STRING_LITERAL ','
                   CHECK_EXPR_ARGS '[' ']'
                  '}'
  ;

opt_name: | NAME STRING_LITERAL ','
  ;

opt_table_indices: | table_indices
  ;

table_indices: INDICES '[' opt_index_names ']' ','
  ;

opt_attributes:  | attributes
  ;

attributes: ATTRIBUTES '[' attribute_list ']' ','
  ;

opt_attribute_list: | attribute_list
  ;

attribute_list: attribute | attribute ',' attribute_list
  ;

attribute:  '{'
             NAME STRING_LITERAL ','
             VALUE attribute_value
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
       NAME STRING_LITERAL ','
       CRC STRING_LITERAL ','
       IS_TEMP BOOL_LITERAL ','
       IS_DELETED BOOL_LITERAL ','
       opt_deleted_version
       opt_region_info
       opt_attributes
       projection
       SELECT STRING_LITERAL ','
       SELECT_ARGS '[' ']'
       '}'
  ;

opt_region_info: | REGION STRING_LITERAL ',' |  REGION STRING_LITERAL ',' DEPLOYED_IN_REGION STRING_LITERAL ','
  ;

opt_projection: | projection
  ;

projection: PROJECTION '[' projected_columns ']' ','
  ;

projected_columns: projected_column | projected_column ',' projected_columns
  ;

projected_column: '{'
                   NAME STRING_LITERAL ','
                   TYPE STRING_LITERAL ','
                   opt_kind
                   opt_is_sensitive
                   IS_NOT_NULL BOOL_LITERAL
                  '}'
  ;

opt_indices:  | indices
  ;

indices: index  | index ',' indices
  ;

index: '{'
        NAME STRING_LITERAL ','
        CRC STRING_LITERAL ','
        TABLE STRING_LITERAL ','
        IS_UNIQUE BOOL_LITERAL ','
        IF_NOT_EXISTS BOOL_LITERAL ','
        IS_DELETED BOOL_LITERAL ','
        opt_deleted_version
        opt_region_info
        opt_partial_index_where
        opt_attributes
        COLUMNS '[' column_names ']' ','
        SORT_ORDERS '[' sort_order_names ']'
       '}'
  ;

opt_partial_index_where: | WHERE STRING_LITERAL ','
  ;

opt_triggers: | triggers
  ;

triggers: trigger | trigger ',' triggers
  ;

trigger: '{'
          NAME STRING_LITERAL ','
          CRC STRING_LITERAL ','
          TARGET STRING_LITERAL ','
          IS_TEMP BOOL_LITERAL ','
          IF_NOT_EXISTS BOOL_LITERAL ','
          IS_DELETED BOOL_LITERAL ','
          opt_deleted_version
          before_after_instead ','
          delete_insert_update ','
          opt_for_each_row
          opt_when_expr
          STATEMENT STRING_LITERAL ','
          STATEMENT_ARGS '[' opt_arg_names ']' ','
          opt_region_info
          opt_attributes
          dependencies
         '}'
  ;

before_after_instead: IS_BEFORE_TRIGGER '1' | IS_AFTER_TRIGGER '1'  | IS_INSTEAD_OF_TRIGGER '1'
  ;

delete_insert_update: IS_DELETE_TRIGGER '1' | IS_INSERT_TRIGGER '1' | IS_UPDATE_TRIGGER '1'
  ;

opt_for_each_row: | FOR_EACH_ROW BOOL_LITERAL ','
  ;

opt_when_expr: | WHEN_EXPR STRING_LITERAL ',' WHEN_EXPR_ARGS '[' opt_arg_names ']' ','
  ;

dependencies: opt_insert_tables
            opt_update_tables
            opt_delete_tables
            opt_from_tables
            opt_uses_procedures
            opt_uses_views
            USES_TABLES '[' opt_table_names ']'
  ;

opt_uses_views: | USES_VIEWS '[' opt_view_names ']' ','
  ;

opt_insert_tables: | INSERT_TABLES '[' opt_table_names ']' ','
  ;

opt_update_tables: | UPDATE_TABLES '[' opt_table_names ']' ','
  ;

opt_delete_tables: | DELETE_TABLES '[' opt_table_names ']' ','
  ;

opt_from_tables: | FROM_TABLES '[' opt_table_names ']' ','
  ;

opt_uses_procedures : | USES_PROCEDURES '[' opt_procedure_names ']' ','
  ;

opt_queries: | queries ;

queries: query | query ',' queries ;

query: '{'
       NAME STRING_LITERAL ','
       DEFINED_IN_FILE STRING_LITERAL ','
       ARGS '[' opt_args ']' ','
       dependencies ','
       opt_region_info
       opt_attributes
       projection
       STATEMENT STRING_LITERAL ','
       STATEMENT_ARGS '[' opt_arg_names ']'
       '}'
  ;

opt_args: | args
  ;

args: arg | arg ',' args
  ;

arg: '{'
      NAME STRING_LITERAL ','
      ARG_ORIGIN STRING_LITERAL ','
      TYPE STRING_LITERAL ','
      opt_kind
      opt_is_sensitive
      IS_NOT_NULL BOOL_LITERAL
      '}'
  ;

opt_inserts: | inserts
  ;

inserts: insert | insert ',' inserts
  ;

insert : '{' insert_details ',' VALUES '[' opt_values ']' '}'
  ;

opt_inserts_general: | inserts_general
  ;

inserts_general: insert_general | insert_general ',' inserts_general
  ;

insert_details:
         NAME STRING_LITERAL ','
         DEFINED_IN_FILE STRING_LITERAL ','
         ARGS '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         TABLE STRING_LITERAL ','
         STATEMENT STRING_LITERAL ','
         STATEMENT_ARGS '[' opt_arg_names ']' ','
         STATEMENT_TYPE STRING_LITERAL ','
         COLUMNS '[' column_names ']'

insert_general : '{' insert_details '}'
  ;

opt_values: | values
  ;

values: value | value ',' values
  ;

value:  '{'
         VALUE STRING_LITERAL ','
         VALUE_ARGS '[' opt_arg_names ']'
        '}'
  ;

opt_updates: | updates
  ;

updates: update | update ',' updates
  ;

update : '{'
         NAME STRING_LITERAL ','
         DEFINED_IN_FILE STRING_LITERAL ','
         ARGS '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         TABLE STRING_LITERAL ','
         STATEMENT STRING_LITERAL ','
         STATEMENT_ARGS '[' opt_arg_names ']'
         '}'
  ;

opt_deletes: | deletes
  ;

deletes: delete | delete ',' deletes
  ;

delete : '{'
         NAME STRING_LITERAL ','
         DEFINED_IN_FILE STRING_LITERAL ','
         ARGS '[' opt_args ']' ','
         dependencies ','
         opt_region_info
         opt_attributes
         TABLE STRING_LITERAL ','
         STATEMENT STRING_LITERAL ','
         STATEMENT_ARGS '[' opt_arg_names ']'
         '}'
  ;

opt_generals: | generals
  ;

generals: general | general ',' generals
  ;

general: '{'
          NAME STRING_LITERAL ','
          DEFINED_IN_FILE STRING_LITERAL ','
          ARGS '[' opt_complex_args ']' ','
          dependencies ','
          opt_regions
          opt_attributes
          opt_projection
          opt_result_contract
          USES_DATABASE BOOL_LITERAL
         '}'
  ;

opt_result_contract: | HAS_SELECT_RESULT '1' ',' | HAS_OUT_RESULT '1' ',' | HAS_OUT_UNION_RESULT'1' ','
  ;

opt_complex_args: | complex_args
  ;

complex_args: complex_arg | complex_arg ',' complex_args
  ;

complex_arg: '{'
              binding
              NAME STRING_LITERAL ','
              ARG_ORIGIN STRING_LITERAL ','
              TYPE STRING_LITERAL ','
              opt_kind
              opt_is_sensitive
              IS_NOT_NULL BOOL_LITERAL
             '}'
  ;

binding: | BINDING_INOUT ',' | BINDING_OUT ','
  ;

opt_enums: | enums
  ;

enums: enum | enum ',' enums
  ;

enum: '{'
      NAME STRING_LITERAL ','
      TYPE STRING_LITERAL ','
      IS_NOT_NULL '1' ','
      VALUES '[' enum_values ']'
      '}'
  ;

enum_values: enum_value | enum_value ',' enum_values
  ;

enum_value: '{'
             NAME STRING_LITERAL ','
             VALUE num_literal
            '}'
  ;

opt_regions: | regions
  ;

regions: region | region ',' regions
  ;

region:  '{'
          NAME STRING_LITERAL ','
          IS_DEPLOYABLE_ROOT BOOL_LITERAL ','
          DEPLOYED_IN_REGION STRING_LITERAL ','
          USING '[' opt_region_names ']' ','
          USING_PRIVATELY '[' opt_bool_list ']'
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
                  NAME STRING_LITERAL ','
                  CRC STRING_LITERAL ','
                  opt_attributes
                  VERSION any_integer
                  '}'
  ;

%%

void yyerror(const char *s, ...) {
  extern int yylineno;
  va_list args;
  va_start(args, s);
  printf("Syntax error at: %s:%d\n", "stdin",yylineno);
  vprintf(s, args);
  printf("\n");
}

int main(int argc, char **argv) {
   if (yyparse()) {
     printf("Parse Error\n");
     return 1;
   }
   return 0;
}

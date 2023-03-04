%{

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>

int yywrap(void) {
  // no further input
  return 1;
}

int yylex(void);

int main(int argc, char **argv) {
  while (yylex()) ;
}

void printTag(const char *tag) {
  printf("'\"%s\"' ':'", tag);
}

%}

%%

ADDED_MIGRATION_PROC                  { printTag("addedMigrationProc"); }
ADDED_VERSION                         { printTag("addedVersion"); }
AD_HOC_MIGRATION_PROCS                { printTag("adHocMigrationProcs"); }
ARGS                                  { printTag("args"); }
ARG_ORIGIN                            { printTag("argOrigin"); }
ATTRIBUTES                            { printTag("attributes"); }
BINDING_INOUT                         { printTag("binding"); printf(" '\"inout\"'"); }
BINDING_OUT                           { printTag("binding"); printf(" '\"out\"'"); }
CHECK_EXPR                            { printTag("checkExpr"); }
CHECK_EXPRESSIONS                     { printTag("checkExpressions"); }
CHECK_EXPR_ARGS                       { printTag("checkExprArgs"); }
COLLATE                               { printTag("collate"); }
COLUMNS                               { printTag("columns"); }
CONSTANT_GROUPS                       { printTag("constantGroups"); }
CRC                                   { printTag("crc"); }
CREATES_OBJECT                        { printTag("createsObject"); }
DECLARE_FUNCS                         { printTag("declareFuncs"); }
DECLARE_PROCS                         { printTag("declareProcs"); }
DEFAULT_VALUE                         { printTag("defaultValue"); }
DEFINED_IN_FILE                       { printTag("definedInFile"); }
DEFINED_ON_LINE                       { printTag("definedOnLine"); }
DELETED_MIGRATION_PROC                { printTag("deletedMigrationProc"); }
DELETED_VERSION                       { printTag("deletedVersion"); }
DELETES                               { printTag("deletes"); }
DELETE_TABLES                         { printTag("deleteTables"); }
DEPLOYED_IN_REGION                    { printTag("deployedInRegion"); }
ENUMS                                 { printTag("enums"); }
FOREIGN_KEYS                          { printTag("foreignKeys"); }
FOR_EACH_ROW                          { printTag("forEachRow"); }
FROM_TABLES                           { printTag("fromTables"); }
GENERAL                               { printTag("general"); }
GENERAL_INSERTS                       { printTag("generalInserts"); }
HAS_OUT_RESULT                        { printTag("hasOutResult"); }
HAS_OUT_UNION_RESULT                  { printTag("hasOutUnionResult"); }
HAS_SELECT_RESULT                     { printTag("hasSelectResult"); }
IF_NOT_EXISTS                         { printTag("ifNotExists"); }
INDICES                               { printTag("indices"); }
INSERTS                               { printTag("inserts"); }
INSERT_TABLES                         { printTag("insertTables"); }
INTERFACES                            { printTag("interfaces"); }
IS_ADDED                              { printTag("isAdded"); }
IS_AFTER_TRIGGER                      { printTag("isAfterTrigger"); }
IS_AUTO_INCREMENT                     { printTag("isAutoIncrement"); }
IS_BACKED                             { printTag("isBacked"); }
IS_BACKING                            { printTag("isBacking"); }
IS_BEFORE_TRIGGER                     { printTag("isBeforeTrigger"); }
IS_DEFERRED                           { printTag("isDeferred"); }
IS_DELETED                            { printTag("isDeleted"); }
IS_DELETE_TRIGGER                     { printTag("isDeleteTrigger"); }
IS_DEPLOYABLE_ROOT                    { printTag("isDeployableRoot"); }
IS_EPONYMOUS                          { printTag("isEponymous"); }
IS_INSERT_TRIGGER                     { printTag("isInsertTrigger"); }
IS_INSTEAD_OF_TRIGGER                 { printTag("isInsteadOfTrigger"); }
IS_NOT_NULL                           { printTag("isNotNull"); }
IS_PRIMARY_KEY                        { printTag("isPrimaryKey"); }
IS_RECREATED                          { printTag("isRecreated"); }
IS_SENSITIVE                          { printTag("isSensitive"); }
IS_TEMP                               { printTag("isTemp"); }
IS_UNIQUE                             { printTag("isUnique"); }
IS_UNIQUE_KEY                         { printTag("isUniqueKey"); }
IS_UPDATE_TRIGGER                     { printTag("isUpdateTrigger"); }
IS_VIRTUAL                            { printTag("isVirtual"); }
KIND                                  { printTag("kind"); }
MODULE                                { printTag("module"); }
MODULE_ARGS                           { printTag("moduleArgs"); }
NAME                                  { printTag("name"); }
ON_DELETE                             { printTag("onDelete"); }
ON_RECREATE_OF                        { printTag("onRecreateOf"); }
ON_UPDATE                             { printTag("onUpdate"); }
PRIMARY_KEY                           { printTag("primaryKey"); }
PRIMARY_KEY_NAME                      { printTag("primaryKeyName"); }
PRIMARY_KEY_SORT_ORDER                { printTag("primaryKeySortOrder"); }
PRIMARY_KEY_SORT_ORDERS               { printTag("primaryKeySortOrders"); }
PROJECTION                            { printTag("projection"); }
QUERIES                               { printTag("queries"); }
RECREATE_GROUP_NAME                   { printTag("recreateGroupName"); }
REFERENCE_COLUMNS                     { printTag("referenceColumns"); }
REFERENCE_TABLE                       { printTag("referenceTable"); }
REGION                                { printTag("region"); }
REGIONS                               { printTag("regions"); }
RESUB_VERSION                         { printTag("resubscribedVersion"); }
RETURN_TYPE                           { printTag("returnType"); }
SCHEMA                                { printTag("schema"); }
SELECT                                { printTag("select"); }
SELECT_ARGS                           { printTag("selectArgs"); }
SORT_ORDERS                           { printTag("sortOrders"); }
STATEMENT                             { printTag("statement"); }
STATEMENT_ARGS                        { printTag("statementArgs"); }
STATEMENT_TYPE                        { printTag("statementType"); }
SUBSCRIPTIONS                         { printTag("subscriptions"); }
TABLE                                 { printTag("table"); }
TABLES                                { printTag("tables"); }
TARGET                                { printTag("target"); }
TRIGGERS                              { printTag("triggers"); }
TYPE                                  { printTag("type"); }
TYPE_HASH                             { printTag("typeHash"); }
UNIQUE_KEYS                           { printTag("uniqueKeys"); }
UNSUB_VERSION                         { printTag("unsubscribedVersion"); }
UPDATES                               { printTag("updates"); }
UPDATE_TABLES                         { printTag("updateTables"); }
USES_DATABASE                         { printTag("usesDatabase"); }
USES_PROCEDURES                       { printTag("usesProcedures"); }
USES_TABLES                           { printTag("usesTables"); }
USES_VIEWS                            { printTag("usesViews"); }
USING                                 { printTag("using"); }
USING_PRIVATELY                       { printTag("usingPrivately"); }
VALUE                                 { printTag("value"); }
VALUES                                { printTag("values"); }
VALUE_ARGS                            { printTag("valueArgs"); }
VERSION                               { printTag("version"); }
VIEWS                                 { printTag("views"); }
VIRTUAL_TABLES                        { printTag("virtualTables"); }
WHEN_EXPR                             { printTag("whenExpr"); }
WHEN_EXPR_ARGS                        { printTag("whenExprArgs"); }
WHERE                                 { printTag("where"); }
WITHOUT_ROWID                         { printTag("withoutRowid"); }

  /*  passthrough normalized grammar rule operator */

::=                                   { printf("::="); }

  /* simple single quote and passthrough */

.                                     { putc(*yytext, stdout); }
'.'                                   { printf("'%c'", yytext[1]); }

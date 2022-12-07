%{

#include <stdio.h>

int yywrap(void) {
  // no further input
  return 1;
}

int yylex(void);

int main(int argc, char **argv) {
  while (yylex()) ;
}

%}

%%

VIRTUAL_TABLES                        { printf("'\"virtualTables\"' ':'"); }
TABLES                                { printf("'\"tables\"' ':'"); }
NAME                                  { printf("'\"name\"' ':'"); }
WHERE                                 { printf("'\"where\"' ':'"); }
ARG_ORIGIN                            { printf("'\"argOrigin\"' ':'"); }
ENUMS                                 { printf("'\"enums\"' ':'"); }
CONSTANT_GROUPS                       { printf("'\"constantGroups\"' ':'"); }
IS_TEMP                               { printf("'\"isTemp\"' ':'"); }
IF_NOT_EXISTS                         { printf("'\"ifNotExists\"' ':'"); }
WITHOUT_ROWID                         { printf("'\"withoutRowid\"' ':'"); }
IS_ADDED                              { printf("'\"isAdded\"' ':'"); }
IS_BACKED                             { printf("'\"isBacked\"' ':'"); }
IS_BACKING                            { printf("'\"isBacking\"' ':'"); }
TYPE_HASH                             { printf("'\"typeHash\"' ':'"); }
ADDED_VERSION                         { printf("'\"addedVersion\"' ':'"); }
ADDED_MIGRATION_PROC                  { printf("'\"addedMigrationProc\"' ':'"); }
IS_DELETED                            { printf("'\"isDeleted\"' ':'"); }
DELETED_VERSION                       { printf("'\"deletedVersion\"' ':'"); }
DELETED_MIGRATION_PROC                { printf("'\"deletedMigrationProc\"' ':'"); }
SUBSCRIPTIONS                         { printf("'\"subscriptions\"' ':'"); }
UNSUB_VERSION                         { printf("'\"unsubscribedVersion\"' ':'"); }
RESUB_VERSION                         { printf("'\"resubscribedVersion\"' ':'"); }
IS_RECREATED                          { printf("'\"isRecreated\"' ':'"); }
ON_RECREATE_OF                        { printf("'\"onRecreateOf\"' ':'"); }
RECREATE_GROUP_NAME                   { printf("'\"recreateGroupName\"' ':'"); }
REGION                                { printf("'\"region\"' ':'"); }
DEPLOYED_IN_REGION                    { printf("'\"deployedInRegion\"' ':'"); }
COLUMNS                               { printf("'\"columns\"' ':'"); }
TYPE                                  { printf("'\"type\"' ':'"); }
KIND                                  { printf("'\"kind\"' ':'"); }
IS_NOT_NULL                           { printf("'\"isNotNull\"' ':'"); }
IS_EPONYMOUS                          { printf("'\"isEponymous\"' ':'"); }
IS_SENSITIVE                          { printf("'\"isSensitive\"' ':'"); }
IS_PRIMARY_KEY                        { printf("'\"isPrimaryKey\"' ':'"); }
PRIMARY_KEY_SORT_ORDERS               { printf("'\"primaryKeySortOrders\"' ':'"); }
IS_UNIQUE_KEY                         { printf("'\"isUniqueKey\"' ':'"); }
IS_AUTO_INCREMENT                     { printf("'\"isAutoIncrement\"' ':'"); }
IS_VIRTUAL                            { printf("'\"isVirtual\"' ':'"); }
PRIMARY_KEY                           { printf("'\"primaryKey\"' ':'"); }
PRIMARY_KEY_NAME                      { printf("'\"primaryKeyName\"' ':'"); }
PRIMARY_KEY_SORT_ORDER                { printf("'\"primaryKeySortOrder\"' ':'"); }
FOREIGN_KEYS                          { printf("'\"foreignKeys\"' ':'"); }
UNIQUE_KEYS                           { printf("'\"uniqueKeys\"' ':'"); }
REFERENCE_TABLE                       { printf("'\"referenceTable\"' ':'"); }
REFERENCE_COLUMNS                     { printf("'\"referenceColumns\"' ':'"); }
ON_UPDATE                             { printf("'\"onUpdate\"' ':'"); }
ON_DELETE                             { printf("'\"onDelete\"' ':'"); }
IS_DEFERRED                           { printf("'\"isDeferred\"' ':'"); }
INDICES                               { printf("'\"indices\"' ':'"); }
VALUE                                 { printf("'\"value\"' ':'"); }
ATTRIBUTES                            { printf("'\"attributes\"' ':'"); }
DEFAULT_VALUE                         { printf("'\"defaultValue\"' ':'"); }
VIEWS                                 { printf("'\"views\"' ':'"); }
PROJECTION                            { printf("'\"projection\"' ':'"); }
SELECT                                { printf("'\"select\"' ':'"); }
SELECT_ARGS                           { printf("'\"selectArgs\"' ':'"); }
SORT_ORDERS                           { printf("'\"sortOrders\"' ':'"); }
TABLE                                 { printf("'\"table\"' ':'"); }
IS_UNIQUE                             { printf("'\"isUnique\"' ':'"); }
DELETE_TABLES                         { printf("'\"deleteTables\"' ':'"); }
FOR_EACH_ROW                          { printf("'\"forEachRow\"' ':'"); }
FROM_TABLES                           { printf("'\"fromTables\"' ':'"); }
INSERT_TABLES                         { printf("'\"insertTables\"' ':'"); }
IS_BEFORE_TRIGGER                     { printf("'\"isBeforeTrigger\"' ':'"); }
IS_AFTER_TRIGGER                      { printf("'\"isAfterTrigger\"' ':'"); }
IS_INSTEAD_OF_TRIGGER                 { printf("'\"isInsteadOfTrigger\"' ':'"); }
IS_INSERT_TRIGGER                     { printf("'\"isInsertTrigger\"' ':'"); }
IS_DELETE_TRIGGER                     { printf("'\"isDeleteTrigger\"' ':'"); }
IS_UPDATE_TRIGGER                     { printf("'\"isUpdateTrigger\"' ':'"); }
STATEMENT                             { printf("'\"statement\"' ':'"); }
STATEMENT_ARGS                        { printf("'\"statementArgs\"' ':'"); }
TARGET                                { printf("'\"target\"' ':'"); }
UPDATE_TABLES                         { printf("'\"updateTables\"' ':'"); }
USES_PROCEDURES                       { printf("'\"usesProcedures\"' ':'"); }
USES_TABLES                           { printf("'\"usesTables\"' ':'"); }
USES_VIEWS                            { printf("'\"usesViews\"' ':'"); }
WHEN_EXPR                             { printf("'\"whenExpr\"' ':'"); }
WHEN_EXPR_ARGS                        { printf("'\"whenExprArgs\"' ':'"); }
TRIGGERS                              { printf("'\"triggers\"' ':'"); }
QUERIES                               { printf("'\"queries\"' ':'"); }
INSERTS                               { printf("'\"inserts\"' ':'"); }
GENERAL_INSERTS                       { printf("'\"generalInserts\"' ':'"); }
DELETES                               { printf("'\"deletes\"' ':'"); }
UPDATES                               { printf("'\"updates\"' ':'"); }
ARGS                                  { printf("'\"args\"' ':'"); }
DEFINED_IN_FILE                       { printf("'\"definedInFile\"' ':'"); }
DEFINED_ON_LINE                       { printf("'\"definedOnLine\"' ':'"); }
STATEMENT_TYPE                        { printf("'\"statementType\"' ':'"); }
VALUES                                { printf("'\"values\"' ':'"); }
VALUE_ARGS                            { printf("'\"valueArgs\"' ':'"); }
USES_DATABASE                         { printf("'\"usesDatabase\"' ':'"); }
BINDING_OUT                           { printf("'\"binding\"' ':' '\"out\"'"); }
BINDING_INOUT                         { printf("'\"binding\"' ':' '\"inout\"'"); }
HAS_SELECT_RESULT                     { printf("'\"hasSelectResult\"' ':'"); }
HAS_OUT_UNION_RESULT                  { printf("'\"hasOutUnionResult\"' ':'"); }
HAS_OUT_RESULT                        { printf("'\"hasOutResult\"' ':'"); }
GENERAL                               { printf("'\"general\"' ':'"); }
INTERFACES                            { printf("'\"interfaces\"' ':'"); }
REGIONS                               { printf("'\"regions\"' ':'"); }
USING                                 { printf("'\"using\"' ':'"); }
USING_PRIVATELY                       { printf("'\"usingPrivately\"' ':'"); }
IS_DEPLOYABLE_ROOT                    { printf("'\"isDeployableRoot\"' ':'"); }
AD_HOC_MIGRATION_PROCS                { printf("'\"adHocMigrationProcs\"' ':'"); }
VERSION                               { printf("'\"version\"' ':'"); }
COLLATE                               { printf("'\"collate\"' ':'"); }
CHECK_EXPR                            { printf("'\"checkExpr\"' ':'"); }
CHECK_EXPR_ARGS                       { printf("'\"checkExprArgs\"' ':'"); }
CHECK_EXPRESSIONS                     { printf("'\"checkExpressions\"' ':'"); }
MODULE                                { printf("'\"module\"' ':'"); }
MODULE_ARGS                           { printf("'\"moduleArgs\"' ':'"); }
CRC                                   { printf("'\"crc\"' ':'"); }

  /*  passthrough normalized grammar rule operator */

::=                                   { printf("::="); }

  /* simple single quote and passthrough */

.                                     { putc(*yytext, stdout); }
'.'                                   { printf("'%c'", yytext[1]); }

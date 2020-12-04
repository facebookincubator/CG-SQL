%option noyywrap nodefault yylineno case-insensitive
%{

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// In case there is any doubt, 'json_test.l' is included in the license as well as
// the code flex generates from it.

#include "json_test.y.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

void yyerror(char *s, ...);
int isatty(int);
char *Strdup(const char *);
int fileno(FILE *);

// the lexer has unused functions and implicit conversions, not easily removed
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wconversion"
%}

%%

\"tables\"[ \t]*:                { return TABLES; }
\"name\"[ \t]*:                  { return NAME; }
\"isTemp\"[ \t]*:                { return IS_TEMP; }
\"ifNotExists\"[ \t]*:           { return IF_NOT_EXISTS; }
\"withoutRowid\"[ \t]*:          { return WITHOUT_ROWID; }
\"isAdded\"[ \t]*:               { return IS_ADDED; }
\"addedVersion\"[ \t]*:          { return ADDED_VERSION; }
\"addedMigrationProc\"[ \t]*:    { return ADDED_MIGRATION_PROC; }
\"isDeleted\"[ \t]*:             { return IS_DELETED; }
\"deletedVersion\"[ \t]*:        { return DELETED_VERSION;}
\"deletedMigrationProc\"[ \t]*:  { return DELETED_MIGRATION_PROC; }
\"isRecreated\"[ \t]*:           { return IS_RECREATED; }
\"recreateGroupName\"[ \t]*:     { return RECREATE_GROUP_NAME; }
\"region\"[ \t]*:                { return REGION; }
\"deployedInRegion\"[ \t]*:      { return DEPLOYED_IN_REGION; }
\"columns\"[ \t]*:               { return COLUMNS; }
\"type\"[ \t]*:                  { return TYPE; }
\"isNotNull\"[ \t]*:             { return IS_NOT_NULL; }
\"isSensitive\"[ \t]*:           { return IS_SENSITIVE; }
\"isPrimaryKey\"[ \t]*:          { return IS_PRIMARY_KEY; }
\"isUniqueKey\"[ \t]*:           { return IS_UNIQUE_KEY; }
\"isAutoIncrement\"[ \t]*:       { return IS_AUTO_INCREMENT; }
\"primaryKey\"[ \t]*:            { return PRIMARY_KEY; }
\"foreignKeys\"[ \t]*:           { return FOREIGN_KEYS; }
\"uniqueKeys\"[ \t]*:            { return UNIQUE_KEYS; }
\"referenceTable\"[ \t]*:        { return REFERENCE_TABLE; }
\"referenceColumns\"[ \t]*:      { return REFERENCE_COLUMNS; }
\"onUpdate\"[ \t]*:              { return ON_UPDATE; }
\"onDelete\"[ \t]*:              { return ON_DELETE; }
\"isDeferred\"[ \t]*:            { return IS_DEFERRED; }
\"indices\"[ \t]*:               { return INDICES; }
\"value\"[ \t]*:                 { return VALUE; }
\"attributes\"[ \t]*:            { return ATTRIBUTES; }
\"defaultValue\"[ \t]*:          { return DEFAULT_VALUE; }
\"views\"[ \t]*:                 { return VIEWS; }
\"projection\"[ \t]*:            { return PROJECTION; }
\"select\"[ \t]*:                { return SELECT; }
\"selectArgs\"[ \t]*:            { return SELECT_ARGS; }
\"sortOrders\"[ \t]*:            { return SORT_ORDERS; }
\"table\"[ \t]*:                 { return TABLE; }
\"isUnique\"[ \t]*:              { return IS_UNIQUE; }
\"deleteTables\"[ \t]*:          { return DELETE_TABLES; }
\"forEachRow\"[ \t]*:            { return FOR_EACH_ROW; }
\"fromTables\"[ \t]*:            { return FROM_TABLES; }
\"insertTables\"[ \t]*:          { return INSERT_TABLES; }
\"isBeforeTrigger\"[ \t]*:       { return IS_BEFORE_TRIGGER; }
\"isAfterTrigger\"[ \t]*:        { return IS_AFTER_TRIGGER; }
\"isInsteadOfTrigger\"[ \t]*:    { return IS_INSTEAD_OF_TRIGGER; }
\"isInsertTrigger\"[ \t]*:       { return IS_INSERT_TRIGGER; }
\"isDeleteTrigger\"[ \t]*:       { return IS_DELETE_TRIGGER; }
\"isUpdateTrigger\"[ \t]*:       { return IS_UPDATE_TRIGGER; }
\"statement\"[ \t]*:             { return STATEMENT; }
\"statementArgs\"[ \t]*:         { return STATEMENT_ARGS; }
\"target\"[ \t]*:                { return TARGET; }
\"updateTables\"[ \t]*:          { return UPDATE_TABLES; }
\"usesProcedures\"[ \t]*:        { return USES_PROCEDURES; }
\"usesTables\"[ \t]*:            { return USES_TABLES; }
\"whenExpr\"[ \t]*:              { return WHEN_EXPR; }
\"whenExprArgs\"[ \t]*:          { return WHEN_EXPR_ARGS; }
\"triggers\"[ \t]*:              { return TRIGGERS; }
\"queries\"[ \t]*:               { return QUERIES; }
\"inserts\"[ \t]*:               { return INSERTS; }
\"generalInserts\"[ \t]*:        { return GENERAL_INSERTS; }
\"deletes\"[ \t]*:               { return DELETES; }
\"updates\"[ \t]*:               { return UPDATES; }
\"enums\"[ \t]*:                 { return ENUMS; }
\"args\"[ \t]*:                  { return ARGS; }
\"definedInFile\"[ \t]*:         { return DEFINED_IN_FILE; }
\"statementType\"[ \t]*:         { return STATEMENT_TYPE; }
\"values\"[ \t]*:                { return VALUES; }
\"valueArgs\"[ \t]*:             { return VALUE_ARGS; }
\"usesDatabase\"[ \t]*:          { return USES_DATABASE; }
\"binding\"[ \t]*:[ \t]*\"out\"   { return BINDING_OUT; }
\"binding\"[ \t]*:[ \t]*\"inout\" { return BINDING_INOUT; }
\"hasSelectResult\"[ \t]*:       { return HAS_SELECT_RESULT; }
\"hasOutUnionResult\"[ \t]*:     { return HAS_OUT_UNION_RESULT; }
\"hasOutResult\"[ \t]*:          { return HAS_OUT_RESULT; }
\"general\"[ \t]*:               { return GENERAL; }
\"regions\"[ \t]*:               { return REGIONS; }
\"using\"[ \t]*:                 { return USING; }
\"usingPrivately\"[ \t]*:        { return USING_PRIVATELY; }
\"isDeployableRoot\"[ \t]*:      { return IS_DEPLOYABLE_ROOT; }
\"adHocMigrationProcs\"[ \t]*:   { return AD_HOC_MIGRATION_PROCS; }
\"version\"[ \t]*:               { return VERSION; }
\"collate\"[ \t]*:               { return COLLATE; }
\"checkExpr\"[ \t]*:             { return CHECK_EXPR; }
\"checkExprArgs\"[ \t]*:         { return CHECK_EXPR_ARGS; }

[01]                             { return yytext[0]; }
[0-9]+L                          { return LONG_LITERAL; }
([0-9]+"."[0-9]*|"."[0-9]+)(E("+"|"-")?[0-9]+)? { return REAL_LITERAL; }
[0-9]+                           { return INT_LITERAL; }
\"(\\.|[^"\n])*\"                { return STRING_LITERAL; }
null				 { return NULL_LITERAL; }
[-\{\}\[\],]                     { return yytext[0]; }

[ \t\n]                      ;
\-\-.*                       ;

.                            { yyerror("Unexpected %s\n", yytext); }

"/*"                         { // sqlite supports C style comments... ignore those
                               int c1 = 0, c2 = input();
                               for (;;) {
                                 if (c2 == EOF) break;
                                 if (c1 == '*' && c2 == '/') break;
                                 c1 = c2;
                                 c2 = input();
                               }
                             }


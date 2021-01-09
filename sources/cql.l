%option noyywrap nodefault yylineno case-insensitive
%{

/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// In case there is any doubt, 'cql.l' is included in the license as well as
// the code flex generates from it.

#include "cql.y.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

void yyerror(const char *s, ...);
void line_directive(const char *);
int isatty(int);
char *Strdup(const char *);
int fileno(FILE *);

int a;

// the lexer has unused functions and implicit conversions, not easily removed
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wconversion"
%}

%%

CURRENT[ \t\n]+ROW                {return CURRENT_ROW; }
EXCLUDE[ \t\n]+NO[ \t\n]+OTHERS   {return EXCLUDE_NO_OTHERS; }
EXCLUDE[ \t\n]+CURRENT[ \t\n]+ROW {return EXCLUDE_CURRENT_ROW; }
EXCLUDE[ \t\n]+GROUP              {return EXCLUDE_GROUP; }
EXCLUDE[ \t\n]+TIES               {return EXCLUDE_TIES; }
UNBOUNDED                    { return UNBOUNDED; }
PRECEDING                    { return PRECEDING; }
FOLLOWING                    { return FOLLOWING; }
RANGE                        { return RANGE; }
ENUM                         { return ENUM; }
ROWS                         { return ROWS; }
GROUPS                       { return GROUPS; }
PARTITION                    { return PARTITION; }
FILTER                       { return FILTER; }
WINDOW                       { return WINDOW; }
EXPLAIN                      { return EXPLAIN; }
QUERY[ \t\n]+PLAN            { return QUERY_PLAN; }
SELECT                       { return SELECT; }
CAST                         { return CAST; }
CREATE                       { return CREATE; }
DROP                         { return DROP; }
TABLE                        { return TABLE; }
TEMP                         { return TEMP; }
COLLATE                      { return COLLATE; }
PRIMARY                      { return PRIMARY; }
KEY                          { return KEY; }
IF                           { return IF; }
WHILE                        { return WHILE; }
CALL                         { return CALL; }
EXISTS                       { return EXISTS; }
UNION                        { return UNION; }
UNION[ \t\n]+ALL             { return UNION_ALL; }
INTERSECT                    { return INTERSECT; }
EXCEPT                       { return EXCEPT; }
NOT                          { return NOT; }
NULL                         { return NULL_; }
DEFAULT                      { return DEFAULT; }
CHECK                        { return CHECK; }
LONG                         { return LONG_; }
LONG_INTEGER                 { return LONG_INTEGER; }
LONG_INT                     { return LONG_INTEGER; }
INT                          { return INT_; }
INTEGER                      { return INTEGER; }
TEXT                         { return TEXT; }
VIRTUAL                      { return VIRTUAL; }
WITH                         { return WITH; }
RECURSIVE                    { return RECURSIVE; }
WITHOUT                      { return WITHOUT; }
ROWID                        { return ROWID; }
AUTOINCREMENT                { return AUTOINCREMENT; }
BOOL                         { return BOOL_; }
REFERENCES                   { return REFERENCES; }
FOREIGN                      { return FOREIGN; }
REAL                         { return REAL; }
CASCADE                      { return CASCADE; }
ON                           { return ON; }
ON[ \t\n]+CONFLICT           { return ON_CONFLICT; }
DO                           { return DO; }
NOTHING                      { return NOTHING; }
UPDATE                       { return UPDATE; }
DELETE                       { return DELETE; }
CONST                        { return CONST; }
CONSTRAINT                   { return CONSTRAINT; }
UNIQUE                       { return UNIQUE; }
PRIVATE                      { return PRIVATE; }
INDEX                        { return INDEX; }
ALL                          { return ALL; }
AS                           { return AS; }
BY                           { return BY; }
DISTINCT                     { return DISTINCT; }
DISTINCTROW                  { return DISTINCTROW; }
INNER                        { return INNER; }
OUTER                        { return OUTER; }
CROSS                        { return CROSS; }
USING                        { return USING; }
RIGHT                        { return RIGHT; }
FROM                         { return FROM; }
WHERE                        { return WHERE; }
GROUP                        { return GROUP; }
HAVING                       { return HAVING; }
ASC                          { return ASC; }
DESC                         { return DESC; }
LEFT                         { return LEFT; }
JOIN                         { return JOIN; }
SET                          { return SET; }
OVER                         { return OVER; }
"<<"                         { return LS; }
">>"                         { return RS; }
"<>"                         { return NE; }
"!="                         { return NE_; }
">="                         { return GE; }
"<="                         { return LE; }
":="                         { return ASSIGN; }
"=="                         { return EQEQ; }
"||"                         { return CONCAT; }
IS[ \t\n]+NOT                { return IS_NOT; }
IS                           { return IS; }
AND                          { return AND; }
ORDER                        { return ORDER; }
CASE                         { return CASE; }
END                          { return END; }
WHEN                         { return WHEN; }
ELSE                         { return ELSE; }
THEN                         { return THEN; }
VIEW                         { return VIEW; }
INSERT                       { return INSERT; }
INTO                         { return INTO; }
VALUES                       { return VALUES; }
OR                           { return OR; }
LIMIT                        { return LIMIT; }
OFFSET                       { return OFFSET; }
PROC                         { return PROC; }
@PROC                        { return AT_PROC; }
PROCEDURE                    { return PROCEDURE; }
FUNCTION                     { return FUNCTION; }
FUNC                         { return FUNC; }
BEGIN                        { return BEGIN_; }
IN                           { return IN; }
TO                           { return TO; }
FOR                          { return FOR; }
THROW                        { return THROW; }
TRY                          { return TRY; }
CATCH                        { return CATCH; }
LIKE                         { return LIKE; }
NOT[ \t\n]+LIKE              { return NOT_LIKE; }
MATCH                        { return MATCH; }
REGEXP                       { return REGEXP; }
GLOB                         { return GLOB; }
BETWEEN                      { return BETWEEN; }
OUT                          { return OUT; }
INOUT                        { return INOUT; }
CURSOR                       { return CURSOR; }
DECLARE                      { return DECLARE; }
FETCH                        { return FETCH; }
LOOP                         { return LOOP; }
LEAVE                        { return LEAVE; }
CONTINUE                     { return CONTINUE; }
OPEN                         { return OPEN; }
CLOSE                        { return CLOSE; }
ELSE[ \t\n]IF                { return ELSE_IF; }
SAVEPOINT                    { return SAVEPOINT; }
ROLLBACK                     { return ROLLBACK; }
RAISE                        { return RAISE; }
FAIL                         { return FAIL; }
ABORT                        { return ABORT; }
COMMIT                       { return COMMIT; }
TRANSACTION                  { return TRANSACTION; }
RELEASE                      { return RELEASE; }
REPLACE                      { return REPLACE; }
IGNORE                       { return IGNORE; }
OBJECT                       { return OBJECT; }
BLOB                         { return BLOB; }
UPSERT                       { return UPSERT; }
STATEMENT                    { return STATEMENT; }
TYPE                         { return TYPE; }
@ENFORCE_STRICT              { return AT_ENFORCE_STRICT; }
@ENFORCE_NORMAL              { return AT_ENFORCE_NORMAL; }
@SENSITIVE                   { return AT_SENSITIVE; }
@DECLARE_SCHEMA_REGION       { return AT_DECLARE_SCHEMA_REGION; }
@DECLARE_DEPLOYABLE_REGION   { return AT_DECLARE_DEPLOYABLE_REGION; }
@BEGIN_SCHEMA_REGION         { return AT_BEGIN_SCHEMA_REGION; }
@END_SCHEMA_REGION           { return AT_END_SCHEMA_REGION; }
@SCHEMA_AD_HOC_MIGRATION     { return AT_SCHEMA_AD_HOC_MIGRATION; }
@ECHO                        { return AT_ECHO; }
@RECREATE                    { return AT_RECREATE; }
@CREATE                      { return AT_CREATE; }
@DELETE                      { return AT_DELETE; }
@SCHEMA_UPGRADE_VERSION      { return AT_SCHEMA_UPGRADE_VERSION; }
@PREVIOUS_SCHEMA             { return AT_PREVIOUS_SCHEMA; }
@SCHEMA_UPGRADE_SCRIPT       { return AT_SCHEMA_UPGRADE_SCRIPT; }
ALTER                        { return ALTER; }
RENAME                       { return RENAME; }
COLUMN                       { return COLUMN; }
ADD                          { return ADD; }
ARGUMENTS                    { return ARGUMENTS; }
RETURN                       { return RETURN; }
@DUMMY_NULLABLES             { return AT_DUMMY_NULLABLES; }
@DUMMY_DEFAULTS              { return AT_DUMMY_DEFAULTS; }
@DUMMY_SEED                  { return AT_DUMMY_SEED; }
@FILE                        { return AT_FILE; }
@ATTRIBUTE                   { return AT_ATTRIBUTE; }
@EMIT_ENUMS                  { return AT_EMIT_ENUMS; }
DEFERRED                     { return DEFERRED; }
DEFERRABLE                   { return DEFERRABLE; }
NOT[ \t\n]DEFERRABLE         { return NOT_DEFERRABLE; }
IMMEDIATE                    { return IMMEDIATE; }
EXCLUSIVE                    { return EXCLUSIVE; }
RESTRICT                     { return RESTRICT; }
ACTION                       { return ACTION; }
INITIALLY                    { return INITIALLY; }
NO                           { return NO; }
BEFORE                       { return BEFORE; }
AFTER                        { return AFTER; }
INSTEAD                      { return INSTEAD; }
OF                           { return OF; }
TRIGGER                      { return TRIGGER; }
FOR[ \t\n]EACH[ \t\n]ROW     { return FOR_EACH_ROW; }

0x[0-9a-fA-F]+               { char *s = Strdup(yytext); yylval.sval = s; return strtoll(s+2, NULL, 16) <= 0x7fffffff ? INTLIT: LONGLIT; }
0x[0-9a-fA-F]+L              { char *s = Strdup(yytext); s[strlen(s)-1] = 0; /* remove the L */ yylval.sval = s; return LONGLIT; }
[0-9]+                       { char *s = Strdup(yytext); yylval.sval = s; return atoll(s) <= 0x7fffffff ? INTLIT: LONGLIT; }
[0-9]+L                      { char *s = Strdup(yytext); s[strlen(s)-1] = 0; /* remove the L */ yylval.sval = s; return LONGLIT; }

([0-9]+"."[0-9]*|"."[0-9]+)(E("+"|"-")?[0-9]+)? { yylval.sval = Strdup(yytext); return REALLIT; }

\"(\\.|[^\\"\n])*\"          { yylval.sval = Strdup(yytext); return CSTRLIT; }
'(''|[^'\n])*'               { yylval.sval = Strdup(yytext); return STRLIT; }
X'([0-9a-fA-F][0-9a-fA-F])*' { yylval.sval = Strdup(yytext); return BLOBLIT; }
[-+&~|^/%*(),.;!<>:=]        { return yytext[0]; }
[_A-Za-z][A-Za-z0-9_]*       { yylval.sval = Strdup(yytext); return ID; }

[ \t\n]                      ;
\-\-.*                       ;

.                            { yyerror("Unexpected %s\n", yytext); }

^#\ [0-9]+\ \"[^"]*\".*      { line_directive(yytext); }

"/*"                         { // sqlite supports C style comments... ignore those
                               int c1 = 0, c2 = input();
                               for (;;) {
                                 if (c2 == EOF) break;
                                 if (c1 == '*' && c2 == '/') break;
                                 c1 = c2;
                                 c2 = input();
                               }
                             }

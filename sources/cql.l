%option noyywrap nodefault yylineno case-insensitive never-interactive
%{

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
#include <cql.h>
#include <charbuf.h>
#include <encoders.h>

void yyerror(const char *s, ...);
void line_directive(const char *);
char *Strdup(const char *);
int fileno(FILE *);
static CSTR last_doc_comment = NULL;

// the lexer has unused functions and implicit conversions, not easily removed
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wconversion"

cql_noexport CSTR get_last_doc_comment() {
  CSTR result = last_doc_comment;
  last_doc_comment = NULL;
  return result;
}

static bool cql_builtins_processing = false;
static bool cql_delete_main_buffer = false;
static YY_BUFFER_STATE cql_main_buffer;

cql_noexport void cql_setup_for_builtins() {
  // stash a buffer for the main input, note that we might already have a buffer
  // in the amalgam case, if we do then use that.
  if (YY_CURRENT_BUFFER) {
    cql_main_buffer = YY_CURRENT_BUFFER;
    cql_delete_main_buffer = false;
  }
  else {
    // if we don't already have a buffer make one using yyin or stdin
    cql_main_buffer = yy_create_buffer(yyin ? yyin : stdin, YY_BUF_SIZE);
    cql_delete_main_buffer = true;
  }

  // add the builtin declares before we process the real input
  yy_scan_string(cql_builtin_text());
  cql_builtins_processing = true;
}

static bool_t cql_finish_stream() {
  // when we read the end of the temporary buffer we switch to the main buffer
  if (cql_builtins_processing) {
    cql_builtins_processing = false;
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(cql_main_buffer);
    return false;
  }

  if (cql_delete_main_buffer) {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    cql_delete_main_buffer = false;
  }

  // nothing further to process, terminate
  return true;
}

%}

stop [^A-Z_0-9]
sp [ \t]+
hex [0-9A-F]
d [0-9]

%%

EXCLUDE{sp}NO{sp}OTHERS/{stop}   { return EXCLUDE_NO_OTHERS; }
EXCLUDE{sp}CURRENT{sp}ROW/{stop} { return EXCLUDE_CURRENT_ROW; }
EXCLUDE{sp}GROUP/{stop}          { return EXCLUDE_GROUP; }
EXCLUDE{sp}TIES/{stop}           { return EXCLUDE_TIES; }

CURRENT{sp}ROW/{stop}        { return CURRENT_ROW; }
UNBOUNDED                    { return UNBOUNDED; }
PRECEDING                    { return PRECEDING; }
FOLLOWING                    { return FOLLOWING; }
SWITCH                       { return SWITCH; }
RANGE                        { return RANGE; }
ENUM                         { return ENUM; }
ROWS                         { return ROWS; }
GROUPS                       { return GROUPS; }
PARTITION                    { return PARTITION; }
FILTER                       { return FILTER; }
WINDOW                       { return WINDOW; }
EXPLAIN                      { return EXPLAIN; }
QUERY{sp}PLAN/{stop}         { return QUERY_PLAN; }
SELECT                       { return SELECT; }
CAST                         { return CAST; }
CREATE                       { return CREATE; }
DROP                         { return DROP; }
TABLE                        { return TABLE; }
TEMP                         { return TEMP; }
COLLATE                      { return COLLATE; }
HIDDEN                       { return HIDDEN; }
PRIMARY                      { return PRIMARY; }
KEY                          { return KEY; }
IF                           { return IF; }
WHILE                        { return WHILE; }
CALL                         { return CALL; }
EXISTS                       { return EXISTS; }
UNION                        { return UNION; }
UNION{sp}ALL/{stop}          { return UNION_ALL; }
INTERSECT                    { return INTERSECT; }
EXCEPT                       { return EXCEPT; }
NOT                          { return NOT; }
NULL                         { return NULL_; }
NULLS                        { return NULLS; }
TRUE                         { return TRUE_; }
FALSE                        { return FALSE_; }
DEFAULT                      { return DEFAULT; }
CHECK                        { return CHECK; }
LET                          { return LET; }
LONG                         { return LONG_; }
LONG_INTEGER                 { return LONG_INTEGER; }
LONG_INT                     { return LONG_INT; }
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
ON{sp}CONFLICT/{stop}        { return ON_CONFLICT; }
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
FROM{sp}BLOB/{stop}          { return FROM_BLOB; }
WHERE                        { return WHERE; }
GROUP                        { return GROUP; }
HAVING                       { return HAVING; }
ASC                          { return ASC; }
DESC                         { return DESC; }
FIRST                        { return FIRST; }
LAST                         { return LAST; }
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
IS{sp}NOT{sp}FALSE/{stop}    { return IS_NOT_FALSE; }
IS{sp}NOT{sp}TRUE/{stop}     { return IS_NOT_TRUE; }
IS{sp}FALSE/{stop}           { return IS_FALSE; }
IS{sp}TRUE/{stop}            { return IS_TRUE; }
IS{sp}NOT/{stop}             { return IS_NOT; }
ISNULL                       { return ISNULL; }
NOTNULL                      { return NOTNULL; }
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
@RC                          { return AT_RC; }
PROCEDURE                    { return PROCEDURE; }
INTERFACE                    { return INTERFACE; }
FUNCTION                     { return FUNCTION; }
FUNC                         { return FUNC; }
BEGIN                        { return BEGIN_; }
IN                           { return IN; }
NOT{sp}IN/{stop}             { return NOT_IN; }
TO                           { return TO; }
FOR                          { return FOR; }
THROW                        { return THROW; }
TRY                          { return TRY; }
CATCH                        { return CATCH; }
NOT{sp}LIKE/{stop}           { return NOT_LIKE; }
LIKE                         { return LIKE; }
NOT{sp}MATCH/{stop}          { return NOT_MATCH; }
MATCH                        { return MATCH; }
NOT{sp}REGEXP/{stop}         { return NOT_REGEXP; }
REGEXP                       { return REGEXP; }
NOT{sp}GLOB/{stop}           { return NOT_GLOB; }
GLOB                         { return GLOB; }
NOT{sp}BETWEEN/{stop}        { return NOT_BETWEEN; }
BETWEEN                      { return BETWEEN; }
OUT                          { return OUT; }
INOUT                        { return INOUT; }
CURSOR                       { return CURSOR; }
DECLARE                      { return DECLARE; }
VAR                          { return VAR; }
FETCH                        { return FETCH; }
LOOP                         { return LOOP; }
LEAVE                        { return LEAVE; }
CONTINUE                     { return CONTINUE; }
CLOSE                        { return CLOSE; }
ELSE{sp}IF/{stop}            { return ELSE_IF; }
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
TYPE_CHECK                   { return TYPE_CHECK; }
@ATTRIBUTE                   { return AT_ATTRIBUTE; }
@BEGIN_SCHEMA_REGION         { return AT_BEGIN_SCHEMA_REGION; }
@BLOB_GET_KEY_TYPE           { return AT_BLOB_GET_KEY_TYPE ; }
@BLOB_GET_VAL_TYPE           { return AT_BLOB_GET_VAL_TYPE ; }
@BLOB_GET_KEY                { return AT_BLOB_GET_KEY ; }
@BLOB_GET_VAL                { return AT_BLOB_GET_VAL ; }
@BLOB_CREATE_KEY             { return AT_BLOB_CREATE_KEY ; }
@BLOB_CREATE_VAL             { return AT_BLOB_CREATE_VAL; }
@BLOB_UPDATE_KEY             { return AT_BLOB_UPDATE_KEY; }
@BLOB_UPDATE_VAL             { return AT_BLOB_UPDATE_VAL; }
@CREATE                      { return AT_CREATE; }
@DECLARE_DEPLOYABLE_REGION   { return AT_DECLARE_DEPLOYABLE_REGION; }
@DECLARE_SCHEMA_REGION       { return AT_DECLARE_SCHEMA_REGION; }
@DELETE                      { return AT_DELETE; }
@DUMMY_DEFAULTS              { return AT_DUMMY_DEFAULTS; }
@DUMMY_NULLABLES             { return AT_DUMMY_NULLABLES; }
@DUMMY_SEED                  { return AT_DUMMY_SEED; }
@ECHO                        { return AT_ECHO; }
@EMIT_CONSTANTS              { return AT_EMIT_CONSTANTS; }
@EMIT_ENUMS                  { return AT_EMIT_ENUMS; }
@EMIT_GROUP                  { return AT_EMIT_GROUP; }
@END_SCHEMA_REGION           { return AT_END_SCHEMA_REGION; }
@ENFORCE_NORMAL              { return AT_ENFORCE_NORMAL; }
@ENFORCE_POP                 { return AT_ENFORCE_POP; }
@ENFORCE_PUSH                { return AT_ENFORCE_PUSH; }
@ENFORCE_RESET               { return AT_ENFORCE_RESET; }
@ENFORCE_STRICT              { return AT_ENFORCE_STRICT; }
@EPONYMOUS                   { return AT_EPONYMOUS; }
@FILE                        { return AT_FILE; }
@PREVIOUS_SCHEMA             { return AT_PREVIOUS_SCHEMA; }
@RECREATE                    { return AT_RECREATE; }
@RESUB                       { return AT_RESUB; }
@SCHEMA_AD_HOC_MIGRATION     { return AT_SCHEMA_AD_HOC_MIGRATION; }
@SCHEMA_UPGRADE_SCRIPT       { return AT_SCHEMA_UPGRADE_SCRIPT; }
@SCHEMA_UPGRADE_VERSION      { return AT_SCHEMA_UPGRADE_VERSION; }
@SENSITIVE                   { return AT_SENSITIVE; }
@UNSUB                       { return AT_UNSUB; }
ALTER                        { return ALTER; }
RENAME                       { return RENAME; }
COLUMN                       { return COLUMN; }
COLUMNS                      { return COLUMNS; }
ADD                          { return ADD; }
ARGUMENTS                    { return ARGUMENTS; }
RETURN                       { return RETURN; }
DEFERRED                     { return DEFERRED; }
DEFERRABLE                   { return DEFERRABLE; }
NOT{sp}DEFERRABLE/{stop}     { return NOT_DEFERRABLE; }
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
FOR{sp}EACH{sp}ROW/{stop}    { return FOR_EACH_ROW; }
ENCODE                       { return ENCODE; }
CONTEXT{sp}COLUMN/{stop}     { return CONTEXT_COLUMN; }
CONTEXT{sp}TYPE/{stop}       { return CONTEXT_TYPE; }
SIGN{sp}FUNCTION/{stop}      { return SIGN_FUNCTION; }
CURSOR{sp}HAS{sp}ROW/{stop}  { return CURSOR_HAS_ROW; }

<<EOF>>                      { if (cql_finish_stream()) yyterminate(); }

0x{hex}+                     { char *s = Strdup(yytext); yylval.sval = s; return strtoll(s+2, NULL, 16) <= 0x7fffffff ? INTLIT: LONGLIT; }
0x{hex}+L                    { char *s = Strdup(yytext); s[strlen(s)-1] = 0; /* remove the L */ yylval.sval = s; return LONGLIT; }
{d}+                         { char *s = Strdup(yytext); yylval.sval = s; return atoll(s) <= 0x7fffffff ? INTLIT: LONGLIT; }
{d}+L                        { char *s = Strdup(yytext); s[strlen(s)-1] = 0; /* remove the L */ yylval.sval = s; return LONGLIT; }

({d}+"."{d}*|"."{d}+)(E("+"|"-")?{d}+)? { yylval.sval = Strdup(yytext); return REALLIT; }

\"(\\.|[^\\"\n])*\"          { yylval.sval = Strdup(yytext); return CSTRLIT; }
'(''|[^'\n])*'               { yylval.sval = Strdup(yytext); return STRLIT; }
X'({hex}{hex})*'             { yylval.sval = Strdup(yytext); return BLOBLIT; }
[-+&~|^/%*(),.;!<>:=]        { return yytext[0]; }
[_A-Z][A-Z0-9_]*             { yylval.sval = Strdup(yytext); return ID; }

[ \t\n]                      ;
\-\-.*                       ;

.                            { yyerror("Unexpected %s\n", yytext); }

^#\ {d}+\ \"[^"]*\".*        { line_directive(yytext); }
^\ *#line\ {d}+\ \"[^"]*\".* { line_directive(yytext); }

"/*"                         { // sqlite supports C style comments... ignore those
                               CHARBUF_OPEN(tmp);
                               int c1 = 0, c2 = input();
                               bool_t save_comment = false;
                               if (c2 == '!' || c2 == '*') {
                                 save_comment = true;
                                 bputc(&tmp, '/');
                                 bputc(&tmp, '*');
                               }
                               for (;;) {
                                 if (c2 == EOF) break;
                                 if (save_comment) {
                                   bputc(&tmp, c2);
                                 }
                                 if (c1 == '*' && c2 == '/') break;
                                 c1 = c2;
                                 c2 = input();
                               }
                               if (save_comment) {
                                 CHARBUF_OPEN(strlit);
                                 cg_encode_c_string_literal(tmp.ptr, &strlit);
                                 last_doc_comment = Strdup(strlit.ptr);
                                 CHARBUF_CLOSE(strlit);
                               }
                               CHARBUF_CLOSE(tmp);
                             }

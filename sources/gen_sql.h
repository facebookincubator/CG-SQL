/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "charbuf.h"
#include "ast.h"
#include "sem.h"

cql_data_decl( int32_t gen_stmt_level );

cql_noexport void gen_init(void);
cql_noexport void gen_cleanup(void);
cql_noexport void gen_set_output_buffer(struct charbuf *_Nonnull buffer);

typedef void (*_Nonnull gen_func)(ast_node *_Nonnull ast);

cql_noexport CSTR _Nonnull gen_type_hash(ast_node *_Nonnull ast);
cql_noexport CSTR _Nonnull get_field_hash(CSTR _Nonnull cname, sem_t sem_type);
cql_noexport void gen_stmt_list_to_stdout(ast_node *_Nullable ast);
cql_noexport void gen_select_core(ast_node *_Nonnull ast);
cql_noexport void gen_one_stmt_to_stdout(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attrs_to_stdout(ast_node *_Nonnull ast);
cql_noexport void gen_root_expr(ast_node *_Nonnull ast);
cql_noexport void gen_col_or_key(ast_node *_Nonnull ast);
cql_noexport void gen_params(ast_node *_Nonnull ast);
cql_noexport void gen_declare_proc_from_create_or_decl(ast_node *_Nonnull ast);
cql_noexport void gen_declare_proc_closure(ast_node *_Nonnull ast, symtab *_Nullable already_done);
cql_noexport void gen_declare_interface_stmt(ast_node *_Nonnull ast);
cql_noexport void gen_one_stmt(ast_node *_Nonnull stmt);
cql_noexport void gen_one_stmt_and_misc_attrs(ast_node *_Nonnull stmt);
cql_noexport void gen_misc_attrs(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attr_value(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attr_value_list(ast_node *_Nonnull ast);
cql_noexport void gen_fk_action(int32_t action);
cql_noexport void gen_insert_type(ast_node *_Nonnull ast);
cql_noexport void gen_col_key_list(ast_node *_Nonnull list);
cql_noexport void gen_typed_names(ast_node *_Nullable ast);
cql_noexport void gen_data_type(ast_node *_Nonnull ast);

// automatically sets the output buffer and printf's the results of the above
cql_noexport void gen_to_stdout(ast_node *_Nullable ast, gen_func fn);

// signature for a callback, you get your context plus the ast
// if you return true then the normal output is suppressed
// in any case the output you provide is emitted
typedef bool_t (*_Nullable gen_sql_callback)(struct ast_node *_Nonnull ast, void *_Nullable context, charbuf *_Nonnull output);

// These modes control the overall style of the output
enum gen_sql_mode {
  gen_mode_echo,          // Prints everything in the original, with standard whitespace and parentheses
  gen_mode_sql,           // Prints the AST formatted for SQLite consumption, omits anything CQL specific
  gen_mode_no_annotations // Equivalent to gen_mode_echo without versioning attributes or generic attributes
                          // * @create, @delete, @recreate, and @attribute are removed
                          // * statements like @echo are not affected, nor is the type specifier @sensitive
};

// Callbacks allow you to significantly alter the generated sql, see the particular flags below.
typedef struct gen_sql_callbacks {
  // Each time a local/global variable is encountered in the AST, this callback is invoked
  // this is to allow the variable reference to be noted and replaced with ? in the generated SQL
  gen_sql_callback _Nullable variables_callback;
  void *_Nullable variables_context;

  // Each time a column definition is emitted this callback is invoked, it may choose to
  // suppress that column.  This is used to remove columns that were added in later schema
  // versions from the baseline schema.
  gen_sql_callback _Nullable col_def_callback;
  void *_Nullable col_def_context;

  // This callback is used to explain the * in select * or select T.*
  gen_sql_callback _Nullable star_callback;
  void *_Nullable star_context;

  // This callback is used to force the "IF NOT EXISTS" form of DDL statements when generating
  // schema upgrade steps.  e.g. a "CREATE TABLE Foo declarations get "IF NOT EXISTS" added
  // to them in upgrade steps.
  gen_sql_callback _Nullable if_not_exists_callback;
  void *_Nullable if_not_exists_context;

  // This callback is used to allow the caller to rename some table references to other names
  // Normally this is used to make replacements in shared fragments
  gen_sql_callback _Nullable table_rename_callback;
  void *_Nullable table_rename_context;

  // This callback is used to expand CALL sequences inside of a CTE expression inline
  // the normal response will be to recursively generate the SQL for the procedure
  // and emit it to the output stream
  gen_sql_callback _Nullable cte_proc_callback;
  void *_Nullable cte_proc_context;

  // This callback is used to expand inline function call sequences inside of a SQL expression
  // the normal response will be to recursively generate the SQL for the function fragment
  // and emit it to the output stream
  gen_sql_callback _Nullable inline_func_callback;
  void *_Nullable inline_func_context;

  // This callback is used to suppress any particular CTE that we might need to omit from a select statement
  // normally this causes us to check the name of the CTE against a blocklist
  gen_sql_callback _Nullable cte_suppress_callback;
  void *_Nullable cte_suppress_context;

  // This callback is used to override entire if/else statements
  gen_sql_callback _Nullable if_stmt_callback;
  void *_Nullable if_stmt_context;

  // This callback allows named types to be resolved when comparing ASTs during
  // semantic analysis.
  gen_sql_callback _Nullable named_type_callback;
  void *_Nullable named_type_context;

  // This callback allows embedded <X SET> types to be recursively walked
  // during emission of exports
  gen_sql_callback _Nullable set_kind_callback;
  void *_Nullable set_kind_context;

  // If true, hex literals are converted to decimal.  This is for JSON which does not support hex literals.
  bool_t convert_hex;

  // If true casts like "CAST(NULL as TEXT)" are reduced to just NULL.  The type information is not needed
  // by SQLite so it just wastes space.
  bool_t minify_casts;

  // If true then unused aliases in select statements are elided to save space.  This is safe because
  // CQL always binds the top level select statement by ordinal anyway.
  bool_t minify_aliases;

  // mode to print cql statement: gen_mode_echo, gen_mode_sql, gen_mode_no_annotations.
  // gen_mode_sql mode causes the AS part of virtual table to be suppressed
  enum gen_sql_mode mode;

  // If CQL finds a column such as 'x' below'
  //
  // create table foo(
  //   x long_int primary key autoincrement
  // );
  //
  // that column must be converted to this form:
  //
  // create table foo(
  //   x integer primary key autoincrement
  // );
  //
  // This is because SQLite mandates that autoincrement must be exactly
  // in the second example above however, it is also the case that in SQLite
  // an integer can store a 64 bit value.  So sending "integer" to SQLite while
  // keeping the sense that the column is to be treated as 64 bits in CQL works
  // just fine.
  //
  // However, when we are emitting CQL (rather than SQL) we want to keep
  // the original long_int type so as not to lose fidelity when processing
  // schema for other semantic checks (such as matching FK data types).
  //
  // This flag is for that purpose: It tells us that the target isn't SQLite
  // and we don't need to do the mapping (yet). Indeed, we shouldn't, or the
  // types will be messed up.
  //
  // In short, if CQL is going to process the output again, use this flag
  // to control the autoincrement transform.  It might be possible to fold
  // this flag with the mode flag but it's sufficiently weird that this
  // extra documentation and special handling is probably worth the extra
  // boolean storage.
  bool_t long_to_int_conv;

  // Each time a table value function is encountered in the AST, this callback is invoked
  // this is to allow the table value function reference to be noted and replaced with table name in the generated SQL
  gen_sql_callback _Nullable table_function_callback;
  void *_Nullable table_function_context;
} gen_sql_callbacks;

cql_noexport void init_gen_sql_callbacks(gen_sql_callbacks *_Nullable callbacks);

// Generate helper methods

cql_noexport void gen_with_callbacks(ast_node *_Nonnull ast, gen_func fn, gen_sql_callbacks *_Nullable _callbacks);
cql_noexport void gen_col_def_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
cql_noexport void gen_statement_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
cql_noexport void gen_statement_and_attributes_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);

cql_noexport bool_t eval_star_callback(ast_node *_Nonnull ast);
cql_noexport bool_t eval_variables_callback(ast_node *_Nonnull ast);
cql_noexport bool_t eval_column_callback(ast_node *_Nonnull ast);

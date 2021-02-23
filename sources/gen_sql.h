/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "charbuf.h"
#include "ast.h"

cql_data_decl( int32_t gen_stmt_level );

cql_noexport void gen_init(void);
cql_noexport void gen_cleanup(void);
cql_noexport void gen_set_output_buffer(struct charbuf *_Nonnull buffer);

typedef void (*_Nonnull gen_func)(ast_node *_Nonnull ast);

cql_noexport void gen_stmt_list_to_stdout(ast_node *_Nullable ast);
cql_noexport void gen_select_core(ast_node *_Nonnull ast);
cql_noexport void gen_one_stmt_to_stdout(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attrs_to_stdout(ast_node *_Nonnull ast);
cql_noexport void gen_root_expr(ast_node *_Nonnull ast);
cql_noexport void gen_col_or_key(ast_node *_Nonnull ast);
cql_noexport void gen_params(ast_node *_Nonnull ast);
cql_noexport void gen_declare_proc_from_create_or_decl(ast_node *_Nonnull ast);
cql_noexport void gen_one_stmt(ast_node *_Nonnull stmt);
cql_noexport void gen_misc_attrs(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attr_value(ast_node *_Nonnull ast);
cql_noexport void gen_misc_attr_value_list(ast_node *_Nonnull ast);
cql_noexport void gen_fk_action(int32_t action);
cql_noexport void gen_insert_type(ast_node *_Nonnull ast);
cql_noexport void gen_declare_proc_from_create_proc(ast_node *_Nonnull ast);
cql_noexport void gen_col_key_list(ast_node *_Nonnull list);

// automatically sets the output buffer and printf's the results of the above
cql_noexport void gen_to_stdout(ast_node *_Nullable ast, gen_func fn);

// signature for a callback, you get your context plus the ast
// if you return true then the normal output is suppressed
// in any case the output you provide is emitted
typedef bool_t (*_Nullable gen_sql_callback)(struct ast_node *_Nonnull ast, void *_Nullable context, charbuf *_Nonnull output);

// The three mode to alter the generated cql slightly.
enum gen_sql_mode {
  gen_mode_echo,            // Print everything in the sql statement
  gen_mode_sql,             // Print only statement valid to sqlite. e.g: annotation is not valid to sqlite
  gen_mode_no_annotations   // Equivalent to gen_mode_echo without all the CQL annotations except:
                            //   - sensentive_attr node
                            //   - note: statements that start with @ are not annotations. like @ECHO @SCHEMA_UPGRADE_SCRIPT, ...
};

// Callbacks allow you to alter the generated sql slightly
// this in particular lets you suppress some columns and record the use of variables
// so that SQL can be changed to include '?' for variables and columns added
// in later versions of the schema can be suppressed in create table statements.
//  This is pretty generalizable.
typedef struct gen_sql_callbacks {
  gen_sql_callback _Nullable variables_callback;
  void *_Nullable variables_context;
  gen_sql_callback _Nullable col_def_callback;
  void *_Nullable col_def_context;
  gen_sql_callback _Nullable star_callback;
  void *_Nullable star_context;
  gen_sql_callback _Nullable if_not_exists_callback;
  void *_Nullable if_not_exists_context;
  gen_sql_callback _Nullable from_etc_callback;
  void *_Nullable from_etc_context;
  bool_t convert_hex;
  bool_t minify_casts;
  bool_t minify_aliases;

  // mode to print cql statement: gen_mode_echo, gen_mode_sql, gen_mode_no_annotations.
  // gen_mode_sql mode causes the AS part of virtual table to be suppressed
  enum gen_sql_mode mode;

  // If CQL finds a column such as this
  // create table foo(x long int primary key autoincrement)
  //
  // that column must be converted to
  // create table foo(x interger primary key autoincrement)
  //
  // SQLite mandates that autoincrement must be exactly as the above
  // however, it is also the case that in SQLite integers can store
  // 64 bit values.  So sending "integer" to sqlite while keeping
  // the sense that the column is to be treated as 64 bits in CQL works
  // just fine.
  //
  // However, when we are emitted CQL (rather than SQL) we want to keep
  // the original long int type and not do the mapping so that things
  // like FK type checking don't give bogus errors.  This flag is for that
  // purpose.  It tells us that the target isn't SQLite and we don't need
  // to do the mapping (yet). Indeed, we shouldn't, or the types will be
  // messed up.
  //
  // In short, if CQL is going to process the output again, use this flag
  // to control the autoincrement transform.
  bool_t long_to_int_conv;
} gen_sql_callbacks;

cql_noexport void init_gen_sql_callbacks(gen_sql_callbacks *_Nullable callbacks);

// Generate helper methods

cql_noexport void gen_with_callbacks(ast_node *_Nonnull ast, gen_func fn, gen_sql_callbacks *_Nullable _callbacks);
cql_noexport void gen_col_def_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);
cql_noexport void gen_statement_with_callbacks(ast_node *_Nonnull ast, gen_sql_callbacks *_Nullable _callbacks);

cql_noexport bool_t eval_star_callback(ast_node *_Nonnull ast);
cql_noexport bool_t eval_variables_callback(ast_node *_Nonnull ast);
cql_noexport bool_t eval_column_callback(ast_node *_Nonnull ast);

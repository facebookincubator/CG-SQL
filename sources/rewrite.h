/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "ast.h"
#include "bytebuf.h"
#include "symtab.h"
#include "charbuf.h"
#include "list.h"
#include "sem.h"

cql_noexport void rewrite_proclit(ast_node *ast);
cql_noexport void rewrite_insert_list_from_cursor(ast_node *ast, ast_node *from_cursor, uint32_t count);
cql_noexport void rewrite_like_column_spec_if_needed(ast_node *columns_values);
cql_noexport void rewrite_from_cursor_if_needed(ast_node *ast_stmt, ast_node *columns_values);
cql_noexport void rewrite_from_shape_args(ast_node *head);
cql_noexport bool_t rewrite_col_key_list(ast_node *ast);
cql_noexport CSTR process_proclit(ast_node *ast, CSTR name);
cql_noexport ast_node *rewrite_gen_data_type(sem_t sem_type);
cql_noexport ast_node *rewrite_gen_full_column_list(sem_struct *sptr);
cql_noexport void rewrite_expr_names_to_columns_values(ast_node* columns_values);
cql_noexport void rewrite_empty_column_list(ast_node *columns_values, sem_struct *sptr);
cql_noexport void rewrite_cql_cursor_diff(ast_node *ast, bool_t report_column_name);
cql_noexport void rewrite_cql_cursor_format(ast_node *ast);
cql_noexport void rewrite_iif(ast_node *ast);
cql_noexport bool_t rewrite_call_args_if_needed(ast_node *arg_list);
cql_noexport void rewrite_cql_cursor_format(ast_node *ast);
cql_noexport void rewrite_cql_cursor_diff(ast_node *ast, bool_t report_column_name);
cql_noexport void rewrite_iif(ast_node *ast);
cql_noexport void rewrite_cte_name_list_from_columns(ast_node *ast, ast_node *select_core);
cql_noexport void rewrite_params(ast_node *head, bytebuf *args_info);
cql_noexport void rewrite_typed_names(ast_node *head);

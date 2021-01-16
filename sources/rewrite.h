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

cql_noexport void rewrite_proclit(ast_node *_Nonnull ast);
cql_noexport void rewrite_insert_list_from_shape(ast_node *_Nonnull ast, ast_node *_Nonnull from_shape, uint32_t count);
cql_noexport void rewrite_like_column_spec_if_needed(ast_node *_Nonnull columns_values);
cql_noexport void rewrite_from_shape_if_needed(ast_node *_Nonnull ast_stmt, ast_node *_Nonnull columns_values);
cql_noexport void rewrite_from_shape_args(ast_node *_Nonnull head);
cql_noexport bool_t rewrite_col_key_list(ast_node *_Nullable ast);
cql_noexport CSTR _Nullable process_proclit(ast_node *_Nullable ast, CSTR _Nonnull name);
cql_noexport ast_node *_Nonnull rewrite_gen_data_type(sem_t sem_type, CSTR _Nullable kind);
cql_noexport ast_node *_Nonnull rewrite_gen_full_column_list(sem_struct *_Nonnull sptr);
cql_noexport void rewrite_expr_names_to_columns_values(ast_node *_Nonnull columns_values);
cql_noexport void rewrite_empty_column_list(ast_node *_Nonnull columns_values, sem_struct *_Nonnull sptr);
cql_noexport void rewrite_cql_cursor_diff(ast_node *_Nonnull ast, bool_t report_column_name);
cql_noexport void rewrite_cql_cursor_format(ast_node *_Nonnull ast);
cql_noexport void rewrite_iif(ast_node *_Nonnull ast);
cql_noexport bool_t rewrite_call_args_if_needed(ast_node *_Nullable arg_list);
cql_noexport void rewrite_cte_name_list_from_columns(ast_node *_Nonnull ast, ast_node *_Nonnull select_core);
cql_noexport void rewrite_params(ast_node *_Nullable head, bytebuf *_Nullable args_info);
cql_noexport void rewrite_typed_names(ast_node *_Nullable head);
cql_noexport void rewrite_data_type_if_needed(ast_node *_Nonnull ast);
cql_noexport void rewrite_right_col_def_type_attrs_if_needed(ast_node *_Nonnull ast);

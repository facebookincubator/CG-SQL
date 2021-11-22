/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_UDF)

// stubs to avoid link errors
cql_noexport void cg_udf_main(struct ast_node *root) {}

#else

#include <stdint.h>
#include "ast.h"
#include "cg_udf.h"
#include "charbuf.h"
#include "cg_common.h"
#include "cql.h"

cql_noexport void cg_udf_main(struct ast_node *root) {
  Contract(options.file_names_count == 2);
  cql_exit_on_semantic_errors(root);
  exit_on_validating_schema();

  CHARBUF_OPEN(header_file);
  CHARBUF_OPEN(body_file);

  bprintf(&body_file, "%s", rt->source_prefix);

  bprintf(&body_file, "#include \"%s\"\n\n", options.file_names[0]);
  bprintf(&body_file, "static void _udf_callback(sqlite3_context* context, int argc, sqlite3_value** argv) {\n");
  bprintf(&body_file, "}\n");

  bprintf(&body_file, "\nvoid create_udf(sqlite3 *_Nonnull _db_) {\n");

  for (list_item *item = all_functions_list; item; item = item->next) {
    EXTRACT_ANY_NOTNULL(any_func, item->ast);
    Contract(is_ast_declare_func_stmt(any_func) || is_ast_declare_select_func_stmt(any_func));
    if (is_ast_declare_select_func_stmt(any_func)) {
      EXTRACT_STRING(name, any_func->left);
      EXTRACT_NOTNULL(func_params_return, any_func->right);
      EXTRACT(params, func_params_return->left);
      int32_t count = 0;
      while (params) {
        count++;
        params = params->right;
      }

      bprintf(&body_file, "  sqlite3_create_function_v2(\n");
      bprintf(&body_file, "    _db_,\n");
      bprintf(&body_file, "    \"%s\",\n", name);
      bprintf(&body_file, "    %d,\n", count);
      bprintf(&body_file, "    SQLITE_UTF8 | SQLITE_DETERMINISTIC,\n");
      bprintf(&body_file, "    NULL,\n");
      bprintf(&body_file, "    &_udf_callback,\n");
      bprintf(&body_file, "    NULL,\n");
      bprintf(&body_file, "    NULL,\n");
      bprintf(&body_file, "    NULL\n");
      bprintf(&body_file, "  );\n");
    }
  }

  bprintf(&body_file, "}\n");

  bprintf(&header_file, "%s", rt->header_prefix);
  bprintf(&header_file, rt->cqlrt_template, rt->cqlrt);
  bprintf(&header_file, "void create_udf(sqlite3 *_Nonnull _db_); \n\n");

  if (options.test) {
    while (root->right) {
      root = root->right;
    }
    bprintf(&header_file, "// The statement ending at line %d\n\n", root->left->lineno);
    bprintf(&body_file, "// The statement ending at line %d\n\n", root->left->lineno);
  }

  cql_write_file(options.file_names[0], header_file.ptr);
  cql_write_file(options.file_names[1], body_file.ptr);

  CHARBUF_CLOSE(body_file);
  CHARBUF_CLOSE(header_file);
}

#endif

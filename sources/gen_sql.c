/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// (re)generate equivalent SQL to what we parsed
// validate the tree shape in painful detail as we go

#include "cql.h"
#include "ast.h"
#include "gen_sql.h"
#include "sem.h"
#include "charbuf.h"
#include "symtab.h"
#include "encoders.h"
#include <string.h>
#include <stdlib.h>

// for dispatching expression types
typedef struct gen_expr_dispatch {
  void (*func)(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new);
  CSTR str;
  int32_t pri_new;
} gen_expr_dispatch;

// for INDENT macros
#define output gen_output

static symtab *gen_stmts;
static symtab *gen_exprs;
static charbuf *gen_output;
static gen_sql_callbacks *gen_callbacks = NULL;
static symtab *used_alias_syms = NULL;

// forward references for things that appear out of order or mutually call each other
static void gen_select_core_list(ast_node *ast);
static void gen_groupby_list(ast_node *_Nonnull ast);
static void gen_stmt_list(ast_node *_Nullable ast);
static void gen_expr(ast_node *_Nonnull ast, int32_t pri);
static void gen_version_attrs(ast_node *_Nullable ast);
static void gen_col_def(ast_node *_Nonnull ast);
static void gen_query_parts(ast_node *ast);
static void gen_select_stmt(ast_node *_Nonnull ast);
static void gen_opt_where_without_new_line(ast_node *ast);
static void gen_opt_orderby(ast_node *ast);
static void gen_shape_arg(ast_node *ast);
static void gen_insert_list(ast_node *_Nullable ast);
static void gen_column_spec(ast_node *ast);
static void gen_from_shape(ast_node *ast);
static void gen_opt_filter_clause(ast_node *ast);
static void gen_if_not_exists(ast_node *ast, bool_t if_not_exist);
static void gen_shape_def(ast_node *ast);
static void gen_expr_names(ast_node *ast);

#define gen_printf(...) bprintf(output, __VA_ARGS__)

cql_noexport void gen_to_stdout(ast_node *ast, gen_func fn) {
  charbuf *gen_saved = output;
  CHARBUF_OPEN(sql_out);
  gen_set_output_buffer(&sql_out);
  (*fn)(ast);
  cql_output("%s", sql_out.ptr);
  CHARBUF_CLOSE(sql_out);
  output = gen_saved;
}

static bool_t suppress_attributes() {
  return gen_callbacks && (gen_callbacks->for_sqlite || gen_callbacks->suppress_attributes);
}

static bool_t for_sqlite() {
  return gen_callbacks && gen_callbacks->for_sqlite;
}

cql_noexport void gen_stmt_list_to_stdout(ast_node *ast) {
  gen_to_stdout(ast, gen_stmt_list);
}

cql_noexport void gen_one_stmt_to_stdout(ast_node *ast) {
  gen_to_stdout(ast, gen_one_stmt);
  cql_output(";\n");
}

cql_noexport void gen_misc_attrs_to_stdout(ast_node *ast) {
  gen_to_stdout(ast, gen_misc_attrs);
}

cql_noexport void gen_with_callbacks(ast_node *ast, gen_func fn, gen_sql_callbacks *_callbacks) {
  gen_callbacks = _callbacks;
  (*fn)(ast);
  gen_callbacks = NULL;
}

cql_noexport void gen_col_def_with_callbacks(ast_node *ast, gen_sql_callbacks *_callbacks) {
  gen_with_callbacks(ast, gen_col_def, _callbacks);
}

cql_noexport void gen_statement_with_callbacks(ast_node *ast, gen_sql_callbacks *_callbacks) {
  gen_with_callbacks(ast, gen_one_stmt, _callbacks);
}

cql_noexport void gen_set_output_buffer(struct charbuf *buffer) {
  output = buffer;
}

static void gen_name(ast_node *ast) {
  EXTRACT_STRING(name, ast);
  gen_printf("%s", name);
}

static void gen_name_list(ast_node *list) {
  Contract(is_ast_name_list(list));

  for (ast_node *item = list; item; item = item->right) {
    gen_name(item->left);
    if (item->right) {
      gen_printf(", ");
    }
  }
}

cql_noexport void gen_misc_attr_value_list(ast_node *ast) {
  Contract(is_ast_misc_attr_value_list(ast));
  for (ast_node *item = ast; item; item = item->right) {
    gen_misc_attr_value(item->left);
    if (item->right) {
      gen_printf(", ");
    }
  }
}

cql_noexport void gen_misc_attr_value(ast_node *ast) {
  if (is_ast_misc_attr_value_list(ast)) {
    gen_printf("(");
    gen_misc_attr_value_list(ast);
    gen_printf(")");
  }
  else {
    gen_root_expr(ast);
  }
}

static void gen_misc_attr(ast_node *ast) {
  Contract(is_ast_misc_attr(ast));
  gen_printf("@ATTRIBUTE(");
  if (is_ast_dot(ast->left)) {
    gen_name(ast->left->left);
    gen_printf(":");
    gen_name(ast->left->right);
  }
  else {
    gen_name(ast->left);
  }
  if (ast->right) {
    gen_printf("=");
    gen_misc_attr_value(ast->right);
  }
  gen_printf(")\n");
}

cql_noexport void gen_misc_attrs(ast_node *list) {
  Contract(is_ast_misc_attrs(list));

  // misc attributes don't go into the output if we are writing for Sqlite
  if (suppress_attributes()) {
    return;
  }

  for (ast_node *item = list; item; item = item->right) {
    gen_misc_attr(item->left);
  }
}

static void gen_data_type(ast_node *ast) {
  if (is_ast_create_data_type(ast)) {
    gen_printf("CREATE ");
    gen_data_type(ast->left);
    return;
  }
  else if (is_ast_notnull(ast)) {
    gen_data_type(ast->left);
    gen_printf(" NOT NULL");
    return;
  }
  else if (is_ast_sensitive_attr(ast)) {
    gen_data_type(ast->left);
    if (!for_sqlite()) {
      gen_printf(" @SENSITIVE");
    }
    return;
  }
  else if (is_ast_type_int(ast)) {
    gen_printf("INTEGER");
  } else if (is_ast_type_text(ast)) {
    gen_printf("TEXT");
  } else if (is_ast_type_blob(ast)) {
    gen_printf("BLOB");
  } else if (is_ast_type_object(ast)) {
    gen_printf("OBJECT");
  } else if (is_ast_type_long(ast)) {
    gen_printf("LONG_INT");
  } else if (is_ast_type_real(ast)) {
    gen_printf("REAL");
  } else if (is_ast_type_bool(ast)) {
    gen_printf("BOOL");
  } else {
    Contract(is_ast_str(ast));
    EXTRACT_STRING(name, ast);
    gen_printf("%s", name);
    return;
  }

  if (!for_sqlite()) {
    if (ast->left) {
      gen_printf("<");
      gen_name(ast->left);
      gen_printf(">");
    }
  }
}

static void gen_indexed_column(ast_node *ast) {
  Contract(is_ast_indexed_column(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("%s", name);
  if (is_ast_asc(ast->right)) {
    gen_printf(" ASC");
  }
  else if (is_ast_desc(ast->right)) {
    gen_printf(" DESC");
  }
}

static void gen_indexed_columns(ast_node *ast) {
  Contract(is_ast_indexed_columns(ast));
  for (ast_node *item = ast; item; item = item->right) {
    gen_indexed_column(item->left);
    if (item->right) {
      gen_printf(", ");
    }
  }
}

static void gen_create_index_stmt(ast_node *ast) {
  Contract(is_ast_create_index_stmt(ast));
  EXTRACT_NOTNULL(create_index_on_list, ast->left);
  EXTRACT_NOTNULL(flags_names_attrs, ast->right);
  EXTRACT_NOTNULL(index_names_and_attrs, flags_names_attrs->right);
  EXTRACT_OPTION(flags, flags_names_attrs->left);
  EXTRACT_NOTNULL(indexed_columns, index_names_and_attrs->left);
  EXTRACT_ANY(attrs, index_names_and_attrs->right);
  EXTRACT_STRING(index_name, create_index_on_list->left);
  EXTRACT_STRING(table_name, create_index_on_list->right);

  gen_printf("CREATE ");
  if (flags & INDEX_UNIQUE) {
    gen_printf("UNIQUE ");
  }
  gen_printf("INDEX ");
  gen_if_not_exists(ast, !!(flags & INDEX_IFNE));
  gen_printf("%s ON %s (", index_name, table_name);
  gen_indexed_columns(indexed_columns);
  gen_printf(")");
  gen_version_attrs(attrs);
}

static void gen_unq_def(ast_node *def) {
  Contract(is_ast_unq_def(def));
  if (def->left) {
    EXTRACT_STRING(name, def->left);
    gen_printf("CONSTRAINT %s ", name);
  }

  EXTRACT_NOTNULL(name_list, def->right);
  gen_printf("UNIQUE (");
  gen_name_list(name_list);
  gen_printf(")");
}

static void gen_check_def(ast_node *def) {
  Contract(is_ast_check_def(def));
  if (def->left) {
    EXTRACT_STRING(name, def->left);
    gen_printf("CONSTRAINT %s ", name);
  }

  EXTRACT_ANY_NOTNULL(expr, def->right);
  gen_printf("CHECK (");
  gen_root_expr(expr);
  gen_printf(")");
}

cql_noexport void gen_fk_action(int32_t action) {
  switch (action) {
    case FK_SET_NULL:
      gen_printf("SET NULL");
      break;
    case FK_SET_DEFAULT:
      gen_printf("SET DEFAULT");
      break;
    case FK_CASCADE:
      gen_printf("CASCADE");
      break;
    case FK_RESTRICT:
      gen_printf("RESTRICT");
      break;
    default:
      // this is all that's left, it better be this...
      Contract(action == FK_NO_ACTION);
      gen_printf("NO ACTION");
      break;
  }
}

static void gen_fk_flags(int32_t flags) {
  if (flags) {
    gen_printf(" ");
  }

  int32_t action = (flags & FK_ON_UPDATE) >> 4;

  if (action) {
    gen_printf("ON UPDATE ");
    gen_fk_action(action);
    if (flags & (FK_ON_DELETE|FK_DEFERRABLES)) {
      gen_printf(" ");
    }
  }

  action = (flags & FK_ON_DELETE);
  if (action) {
    gen_printf("ON DELETE ");
    gen_fk_action(action);
    if (flags & FK_DEFERRABLES) {
      gen_printf(" ");
    }
  }

  if (flags & FK_DEFERRABLES) {
    Contract(flags & (FK_DEFERRABLE|FK_NOT_DEFERRABLE));
    if (flags & FK_DEFERRABLE) {
      Contract(!(flags & FK_NOT_DEFERRABLE));
      gen_printf("DEFERRABLE");
    }
    else {
      gen_printf("NOT DEFERRABLE");
    }
    if (flags & FK_INITIALLY_IMMEDIATE) {
      Contract(!(flags & FK_INITIALLY_DEFERRED));
      gen_printf(" INITIALLY IMMEDIATE");
    }
    else if (flags & FK_INITIALLY_DEFERRED) {
      gen_printf(" INITIALLY DEFERRED");
    }
  }
}

static void gen_fk_target_options(ast_node *ast) {
  Contract(is_ast_fk_target_options(ast));
  EXTRACT_NOTNULL(fk_target, ast->left);
  EXTRACT_OPTION(flags, ast->right);
  EXTRACT_STRING(table_name, fk_target->left);
  EXTRACT_NAMED_NOTNULL(ref_list, name_list, fk_target->right);

  gen_printf("REFERENCES ");
  gen_printf("%s", table_name);
  gen_printf(" (");
  gen_name_list(ref_list);
  gen_printf(")");
  gen_fk_flags(flags);
}

static void gen_fk_def(ast_node *def) {
  Contract(is_ast_fk_def(def));
  EXTRACT(fk_info, def->right);
  EXTRACT_NAMED_NOTNULL(src_list, name_list, fk_info->left);
  EXTRACT_NOTNULL(fk_target_options, fk_info->right);

  if (def->left) {
    EXTRACT_STRING(name, def->left);
    gen_printf("CONSTRAINT %s ", name);
  }

  gen_printf("FOREIGN KEY (");
  gen_name_list(src_list);
  gen_printf(") ");
  gen_fk_target_options(fk_target_options);
}

static void gen_pk_def(ast_node *def) {
  Contract(is_ast_pk_def(def));
  EXTRACT(name_list, def->right);

  if (def->left) {
    EXTRACT_STRING(name, def->left);
    gen_printf("CONSTRAINT %s ", name);
  }

  gen_printf("PRIMARY KEY (");
  gen_name_list(name_list);
  gen_printf(")");
}

static void gen_version_and_proc(ast_node *version_annotation)
{
  Contract(is_ast_version_annotation(version_annotation));
  EXTRACT_OPTION(vers, version_annotation->left);
  gen_printf("%d", vers);
  if (version_annotation->right) {
    EXTRACT_STRING(name, version_annotation->right);
    gen_printf(", %s", name);
  }
}

static void gen_recreate_attr(ast_node *attr) {
  Contract (is_ast_recreate_attr(attr));
  if (!suppress_attributes()) {
    // attributes do not appear when writing out commands for Sqlite
    gen_printf(" @RECREATE");
    if (attr->left) {
      EXTRACT_STRING(group_name, attr->left);
      gen_printf("(%s)", group_name);
    }
  }
}

static void gen_create_attr(ast_node *attr) {
  Contract (is_ast_create_attr(attr));
  if (!suppress_attributes()) {
    // attributes do not appear when writing out commands for Sqlite
    gen_printf(" @CREATE(");
    gen_version_and_proc(attr->left);
    gen_printf(")");
  }
}

static void gen_delete_attr(ast_node *attr) {
  Contract (is_ast_delete_attr(attr));
  if (!suppress_attributes()) {
    // attributes do not appear when writing out commands for Sqlite
    gen_printf(" @DELETE(");
    gen_version_and_proc(attr->left);
    gen_printf(")");
  }
}

static void gen_sensitive_attr(ast_node *attr) {
  Contract (is_ast_sensitive_attr(attr));
  if (!for_sqlite()) {
    // attributes do not appear when writing out commands for Sqlite
    gen_printf(" @SENSITIVE");
  }
}

static void gen_col_attrs(ast_node *_Nullable attrs) {
  for (ast_node *attr = attrs; attr; attr = attr->right) {
    if (is_ast_create_attr(attr)) {
      gen_create_attr(attr);
    } else if (is_ast_sensitive_attr(attr)) {
      gen_sensitive_attr(attr);
    } else if (is_ast_delete_attr(attr)) {
      gen_delete_attr(attr);
    } else if (is_ast_col_attrs_not_null(attr)) {
      gen_printf(" NOT NULL");
    } else if (is_ast_col_attrs_pk(attr)) {
      gen_printf(" PRIMARY KEY");
      if (is_ast_col_attrs_autoinc(attr->left)) {
        gen_printf(" AUTOINCREMENT");
      }
    } else if (is_ast_col_attrs_unique(attr)) {
      gen_printf(" UNIQUE");
    } else if (is_ast_col_attrs_hidden(attr)) {
      gen_printf(" HIDDEN");
    } else if (is_ast_col_attrs_fk(attr)) {
      gen_printf(" ");
      gen_fk_target_options(attr->left);
    } else if (is_ast_col_attrs_check(attr)) {
      gen_printf(" CHECK(");
      gen_root_expr(attr->left);
      gen_printf(") ");
    } else if (is_ast_col_attrs_collate(attr)) {
      gen_printf(" COLLATE ");
      gen_root_expr(attr->left);
    } else {
      Contract(is_ast_col_attrs_default(attr));
      gen_printf(" DEFAULT ");
      gen_root_expr(attr->left);
    }
  }
}

static void gen_col_def(ast_node *def) {
  Contract(is_ast_col_def(def));
  EXTRACT_NOTNULL(col_def_type_attrs, def->left);
  EXTRACT(misc_attrs, def->right);
  EXTRACT_ANY(attrs, col_def_type_attrs->right);
  EXTRACT_NOTNULL(col_def_name_type, col_def_type_attrs->left);
  EXTRACT_STRING(name, col_def_name_type->left);
  EXTRACT_ANY_NOTNULL(data_type, col_def_name_type->right);

  if (misc_attrs) {
    gen_misc_attrs(misc_attrs);
  }

  gen_printf("%s ", name);

  if (gen_callbacks && gen_callbacks->long_to_int_conv && def->sem && (def->sem->sem_type & SEM_TYPE_AUTOINCREMENT)) {
    // semantic checking must have already validated that this is either an integer or long_integer
    sem_t core_type = core_type_of(def->sem->sem_type);
    Contract(core_type == SEM_TYPE_INTEGER || core_type == SEM_TYPE_LONG_INTEGER);
    gen_printf("INTEGER");
  }
  else {
    gen_data_type(data_type);
  }
  gen_col_attrs(attrs);
}

cql_export bool_t eval_star_callback(ast_node *ast) {
  Contract(is_ast_star(ast) || is_ast_table_star(ast));
  bool_t suppress = 0;

  if (gen_callbacks && gen_callbacks->star_callback && ast->sem) {
    CHARBUF_OPEN(buf);
    suppress = gen_callbacks->star_callback(ast, gen_callbacks->star_context, &buf);
    gen_printf("%s", buf.ptr);
    CHARBUF_CLOSE(buf);
  }

  return suppress;
}

cql_noexport bool_t eval_column_callback(ast_node *ast) {
  Contract(is_ast_col_def(ast));
  bool_t suppress = 0;

  if (gen_callbacks && gen_callbacks->col_def_callback && ast->sem) {
    CHARBUF_OPEN(buf);
    suppress = gen_callbacks->col_def_callback(ast, gen_callbacks->col_def_context, &buf);
    gen_printf("%s", buf.ptr);
    CHARBUF_CLOSE(buf);
  }

  return suppress;
}

bool_t eval_variables_callback(ast_node *ast) {
  bool_t suppress = 0;
  if (gen_callbacks && gen_callbacks->variables_callback && ast->sem && is_variable(ast->sem->sem_type)) {
    CHARBUF_OPEN(buf);
    suppress = gen_callbacks->variables_callback(ast, gen_callbacks->variables_context, &buf);
    gen_printf("%s", buf.ptr);
    CHARBUF_CLOSE(buf);
  }
  return suppress;
}

cql_noexport void gen_col_or_key(ast_node *def) {
  if (is_ast_col_def(def)) {
    gen_col_def(def);
  } else if (is_ast_pk_def(def)) {
    gen_pk_def(def);
  } else if (is_ast_fk_def(def)) {
    gen_fk_def(def);
  } else if (is_ast_like(def)) {
    gen_shape_def(def);
  } else if (is_ast_check_def(def)) {
    gen_check_def(def);
  } else {
    Contract(is_ast_unq_def(def));
    gen_unq_def(def);
  }
}

cql_noexport void gen_col_key_list(ast_node *list) {
  Contract(is_ast_col_key_list(list));
  bool_t need_comma = 0;

  BEGIN_INDENT(coldefs, 2);

  for (ast_node *item = list; item; item = item->right) {
    EXTRACT_ANY_NOTNULL(def, item->left);

    // give the callback system a chance to suppress columns that are not in this version
    if (is_ast_col_def(def) && eval_column_callback(def)) {
      continue;
    }

    if (need_comma) {
      gen_printf(",\n");
    }
    need_comma = 1;

    gen_col_or_key(def);
  }
  END_INDENT(coldefs);
}

static void gen_select_opts(ast_node *ast) {
  Contract(is_ast_select_opts(ast));
  EXTRACT_ANY_NOTNULL(opt, ast->left);

  if (is_ast_all(opt)) {
    gen_printf(" ALL");
  }
  else if (is_ast_distinct(opt)) {
    gen_printf(" DISTINCT");
  }
  else {
    Contract(is_ast_distinctrow(opt));
    gen_printf(" DISTINCTROW");
  }
}

static void gen_binary(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {

  // We add parens if our priority is less than the parent prioirty
  // meaning something like this:
  // * we're a + node, our parent is a * node
  // * we need parens because the tree specifies that the + happens before the *
  //
  // Also, grouping of equal operators is left to right
  // so for so if our right child is the same precendence as us
  // that means there were parens there in the original expression
  // e.g.  3+(4+7);
  // effectively it's like we're one binding strength higher for our right child
  // so we call it with pri_new + 1.  If it's equal to us it must emit parens

  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" %s ", op);
  gen_expr(ast->right, pri_new + 1);
  if (pri_new < pri) gen_printf(")");
}

static void gen_unary(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  if (pri_new < pri) gen_printf("(");
  gen_printf("%s", op);
  gen_expr(ast->left, pri_new);
  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_const(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  gen_printf("CONST(");
  gen_expr(ast->left, pri_new);
  gen_printf(")");
}

static void gen_uminus(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  if (pri_new < pri) gen_printf("(");
  gen_printf("%s", op);

  // we don't ever want -- in the output because that's a comment
  CHARBUF_OPEN(tmp);
  charbuf *saved = output;
  output = &tmp;
  gen_expr(ast->left, pri_new);
  output = saved;

  if (tmp.ptr[0] == '-') {
    gen_printf(" ");
  }

  gen_printf("%s", tmp.ptr);
  CHARBUF_CLOSE(tmp);

  if (pri_new < pri) gen_printf(")");
}

static void gen_concat(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_concat(ast));

  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" %s ", op);
  gen_expr(ast->right, pri_new);
  if (pri_new < pri) gen_printf(")");
}

static void gen_arg_expr(ast_node *ast) {
  if (is_ast_star(ast)) {
    gen_printf("*");
  }
  else if (is_ast_from_shape(ast)) {
    gen_shape_arg(ast);
  }
  else {
    gen_root_expr(ast);
  }
}

static void gen_expr_exists(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_exists_expr(ast));
  EXTRACT_NOTNULL(select_stmt, ast->left);

  gen_printf("EXISTS (");
  gen_select_stmt(select_stmt);
  gen_printf(")");
}

static void gen_arg_list(ast_node *ast) {
  while (ast) {
    gen_arg_expr(ast->left);
    if (ast->right) {
      gen_printf(", ");
    }
    ast = ast->right;
  }
}

static void gen_expr_list(ast_node *ast) {
  while (ast) {
    gen_root_expr(ast->left);
    if (ast->right) {
      gen_printf(", ");
    }
    ast = ast->right;
  }
}

static void gen_shape_arg(ast_node *ast) {
  Contract(is_ast_from_shape(ast));
  EXTRACT_STRING(shape, ast->left);
  gen_printf("FROM %s", shape);
  if (ast->right) {
    gen_printf(" ");
    gen_shape_def(ast->right);
  }
}

static void gen_call_expr_list(ast_node *ast) {
  while (ast) {
    ast_node *left = ast->left;
    if (is_ast_from_shape(left)) {
      gen_shape_arg(left);
    }
    else {
      gen_root_expr(left);
    }

    if (ast->right) {
      gen_printf(", ");
    }
    ast = ast->right;
  }
}

static void gen_case_list(ast_node *ast) {
  Contract(is_ast_case_list(ast));

  while (ast) {
    EXTRACT_NOTNULL(when, ast->left);
    EXTRACT_ANY_NOTNULL(case_expr, when->left);
    EXTRACT_ANY_NOTNULL(then_expr, when->right);

    // additional parens never needed because WHEN/THEN act like parens
    gen_printf("WHEN ");
    gen_root_expr(case_expr);
    gen_printf(" THEN ");
    gen_root_expr(then_expr);
    gen_printf("\n");

    ast = ast->right;
  }
}

static void gen_expr_num(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_num(ast));
  EXTRACT_NUM_VALUE(val, ast);
  Contract(val);

  if (has_hex_prefix(val) && gen_callbacks && gen_callbacks->convert_hex) {
    int64_t v = strtol(val, NULL, 16);
    gen_printf("%lld", (llint_t)v);
  }
  else {
    gen_printf("%s", val);
  }

  if (for_sqlite()) {
    return;
  }

  EXTRACT_NUM_TYPE(num_type, ast);
  if (num_type == NUM_LONG) {
    gen_printf("L");
  }
}

static void gen_expr_blob(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_blob(ast));
  EXTRACT_BLOBTEXT(str, ast);

  // blob literals are easy, we just emit them, there's no conversion or anything like that
  gen_printf("%s", str);
}

static void gen_expr_str(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_str(ast));
  EXTRACT_STRING(str, ast);

  if (is_ast_strlit(ast)) {
    str_ast_node *asts = (str_ast_node *)ast;
    if (!asts->cstr_literal || for_sqlite()) {
      // Note: str is the lexeme, so it is either still quoted and escaped
      // or if it was a c string literal it was already normalized to SQL form.
      // In both cases we can just print.
      gen_printf("%s", str);
    }
    else {
      // If was originally a c string literal re-encode it for echo output
      // so that it looks the way it was given to us.  This is so that when we
      // echo the SQL back for say test output C string literal forms come out
      // just as they were given to us.
      CHARBUF_OPEN(decoded);
      CHARBUF_OPEN(encoded);
      cg_decode_string_literal(str, &decoded);
      cg_encode_c_string_literal(decoded.ptr, &encoded);

      gen_printf("%s", encoded.ptr);
      CHARBUF_CLOSE(encoded);
      CHARBUF_CLOSE(decoded);
    }
  }
  else {
    if (!eval_variables_callback(ast)) {
      gen_printf("%s", str);  // an identifier
    }
  }
}

static void gen_expr_null(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_null(ast));
  gen_printf("NULL");
}

static void gen_expr_dot(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_dot(ast));
  if (eval_variables_callback(ast)) {
    return;
  }

  EXTRACT_STRING(left, ast->left);
  EXTRACT_STRING(right, ast->right);

  if (!strcmp("ARGUMENTS", left) && ast->sem && ast->sem->name) {
    // special case for rewritten arguments, hide the "ARGUMENTS." stuff
    gen_printf("%s", ast->sem->name);
  }
  else {
    gen_printf("%s.%s", left, right);
  }
}

static void gen_expr_in_pred(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_in_pred(ast));
  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" IN (");
  if (is_ast_expr_list(ast->right)) {
    EXTRACT_NOTNULL(expr_list, ast->right);
    gen_expr_list(expr_list);
  }
  else {
    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);
    gen_select_stmt(select_stmt);
  }
  gen_printf(")");

  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_not_in(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_not_in(ast));
  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" NOT IN (");
  if (is_ast_expr_list(ast->right)) {
    EXTRACT_NOTNULL(expr_list, ast->right);
    gen_expr_list(expr_list);
  }
  else {
    EXTRACT_ANY_NOTNULL(select_stmt, ast->right);
    gen_select_stmt(select_stmt);
  }
  gen_printf(")");

  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_call(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT_NOTNULL(call_filter_clause, call_arg_list->left);
  EXTRACT(distinct, call_filter_clause->left);
  EXTRACT(opt_filter_clause, call_filter_clause->right);
  EXTRACT(arg_list, call_arg_list->right);

  if (for_sqlite()) {
    // The nullable function has no actual sql for it, it's just type info
    // don't echo nullable if we're doing codegen (callback present)
    if (!Strcasecmp("nullable", name)) {
      gen_arg_list(arg_list);
      return;
    }

    // the ptr function has no actual sql for it, it's just type info
    // don't echo ptr if we're doing codegen (callback present)
    if (!Strcasecmp("ptr", name)) {
      gen_arg_list(arg_list);
      return;
    }
  }

  gen_printf("%s(", name);
  if (distinct) {
    gen_printf("DISTINCT ");
  }
  gen_arg_list(arg_list);
  gen_printf(")");

  if (opt_filter_clause) {
    gen_opt_filter_clause(opt_filter_clause);
  }
}

static void gen_opt_filter_clause(ast_node *ast) {
  Contract(is_ast_opt_filter_clause(ast));
  EXTRACT_NOTNULL(opt_where, ast->left);

  gen_printf(" FILTER (");
  gen_opt_where_without_new_line(opt_where);
  gen_printf(")");
}

static void gen_opt_partition_by(ast_node *ast) {
  Contract(is_ast_opt_partition_by(ast));
  EXTRACT_NOTNULL(expr_list, ast->left);

  gen_printf("PARTITION BY ");
  gen_expr_list(expr_list);
}

static void gen_frame_spec_flags(int32_t flags) {
  if (flags & FRAME_TYPE_RANGE) {
    gen_printf("RANGE");
  }
  if (flags & FRAME_TYPE_ROWS) {
    gen_printf("ROWS");
  }
  if (flags & FRAME_TYPE_GROUPS) {
    gen_printf("GROUPS");
  }
  if (flags & FRAME_BOUNDARY_UNBOUNDED || flags & FRAME_BOUNDARY_START_UNBOUNDED) {
    gen_printf("UNBOUNDED PRECEDING");
  }
  if (flags & FRAME_BOUNDARY_PRECEDING ||
      flags & FRAME_BOUNDARY_START_PRECEDING ||
      flags & FRAME_BOUNDARY_END_PRECEDING) {
    gen_printf("PRECEDING");
  }
  if (flags & FRAME_BOUNDARY_CURRENT_ROW ||
      flags & FRAME_BOUNDARY_START_CURRENT_ROW ||
      flags & FRAME_BOUNDARY_END_CURRENT_ROW) {
    gen_printf("CURRENT ROW");
  }
  if (flags & FRAME_BOUNDARY_START_FOLLOWING ||
      flags & FRAME_BOUNDARY_END_FOLLOWING) {
    gen_printf("FOLLOWING");
  }
  if (flags & FRAME_BOUNDARY_END_UNBOUNDED) {
    gen_printf("UNBOUNDED FOLLOWING");
  }
  if (flags & FRAME_EXCLUDE_NO_OTHERS) {
    gen_printf("EXCLUDE NO OTHERS");
  }
  if (flags & FRAME_EXCLUDE_CURRENT_ROW) {
    gen_printf("EXCLUDE CURRENT ROW");
  }
  if (flags & FRAME_EXCLUDE_GROUP) {
    gen_printf("EXCLUDE GROUP");
  }
  if (flags & FRAME_EXCLUDE_TIES) {
    gen_printf("EXCLUDE TIES");
  }
}

static void gen_frame_type(int32_t flags) {
  Invariant(flags == (flags & FRAME_TYPE_FLAGS));
  gen_frame_spec_flags(flags);
  gen_printf(" ");
}

static void gen_frame_exclude(int32_t flags) {
  Invariant(flags == (flags & FRAME_EXCLUDE_FLAGS));
  if (flags != FRAME_EXCLUDE_NONE) {
    gen_printf(" ");
  }
  gen_frame_spec_flags(flags);
}

static void gen_frame_boundary(ast_node *ast, int32_t flags) {
  EXTRACT_ANY(expr, ast->left);
  Invariant(flags == (flags & FRAME_BOUNDARY_FLAGS));

  if (expr) {
    gen_root_expr(expr);
    gen_printf(" ");
  }
  gen_frame_spec_flags(flags);
}

static void gen_frame_boundary_start(ast_node *ast, int32_t flags) {
  Contract(is_ast_expr_list(ast));
  EXTRACT_ANY(expr, ast->left);
  Invariant(flags == (flags & FRAME_BOUNDARY_START_FLAGS));

  gen_printf("BETWEEN ");
  if (expr) {
    gen_root_expr(expr);
    gen_printf(" ");
  }
  gen_frame_spec_flags(flags);
}

static void gen_frame_boundary_end(ast_node *ast, int32_t flags) {
  Contract(is_ast_expr_list(ast));
  EXTRACT_ANY(expr, ast->right);
  Invariant(flags == (flags & FRAME_BOUNDARY_END_FLAGS));

  gen_printf(" AND ");
  if (expr) {
    gen_root_expr(expr);
    gen_printf(" ");
  }
  gen_frame_spec_flags(flags);
}

static void gen_opt_frame_spec(ast_node *ast) {
  Contract(is_ast_opt_frame_spec(ast));
  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_NOTNULL(expr_list, ast->right);

  int32_t frame_type_flags = flags & FRAME_TYPE_FLAGS;
  int32_t frame_boundary_flags = flags & FRAME_BOUNDARY_FLAGS;
  int32_t frame_boundary_start_flags = flags & FRAME_BOUNDARY_START_FLAGS;
  int32_t frame_boundary_end_flags = flags & FRAME_BOUNDARY_END_FLAGS;
  int32_t frame_exclude_flags = flags & FRAME_EXCLUDE_FLAGS;

  if (frame_type_flags) {
    gen_frame_type(frame_type_flags);
  }
  if (frame_boundary_flags) {
    gen_frame_boundary(expr_list, frame_boundary_flags);
  }
  if (frame_boundary_start_flags) {
    gen_frame_boundary_start(expr_list, frame_boundary_start_flags);
  }
  if (frame_boundary_end_flags) {
    gen_frame_boundary_end(expr_list, frame_boundary_end_flags);
  }
  if (frame_exclude_flags) {
    gen_frame_exclude(frame_exclude_flags);
  }
}

static void gen_window_defn(ast_node *ast) {
  Contract(is_ast_window_defn(ast));
  EXTRACT(opt_partition_by, ast->left);
  EXTRACT_NOTNULL(window_defn_orderby, ast->right);
  EXTRACT(opt_orderby, window_defn_orderby->left);
  EXTRACT(opt_frame_spec, window_defn_orderby->right);

  // the first optional element never needs a space
  bool need_space = 0;

  gen_printf(" (");
  if (opt_partition_by) {
    Invariant(!need_space);
    gen_opt_partition_by(opt_partition_by);
    need_space = 1;
  }

  if (opt_orderby) {
    if (need_space) gen_printf(" ");
    gen_opt_orderby(opt_orderby);
    need_space = 1;
  }

  if (opt_frame_spec) {
    if (need_space) gen_printf(" ");
    gen_opt_frame_spec(opt_frame_spec);
  }
  gen_printf(")");
}

static void gen_name_or_window_defn(ast_node *ast) {
  if (is_ast_str(ast)) {
    EXTRACT_STRING(window_name, ast);
    gen_printf(" %s", window_name);
  }
  else {
    Contract(is_ast_window_defn(ast));
    gen_window_defn(ast);
  }
}

static void gen_expr_window_func_inv(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_window_func_inv(ast));
  EXTRACT_NOTNULL(call, ast->left);
  EXTRACT_ANY_NOTNULL(name_or_window_defn, ast->right);

  gen_printf("\n  ");
  gen_expr_call(call, op, pri, pri_new);
  gen_printf(" OVER");
  gen_name_or_window_defn(name_or_window_defn);
}

static void gen_expr_raise(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_raise(ast));
  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_ANY(expr, ast->right);

  Contract(flags >= RAISE_IGNORE && flags <= RAISE_FAIL);

  gen_printf("RAISE(");
  switch (flags) {
    case RAISE_IGNORE: gen_printf("IGNORE"); break;
    case RAISE_ROLLBACK: gen_printf("ROLLBACK"); break;
    case RAISE_ABORT: gen_printf("ABORT"); break;
    case RAISE_FAIL: gen_printf("FAIL"); break;
  }
  if (expr) {
    gen_printf(", ");
    gen_root_expr(expr);
  }
  gen_printf(")");
}

static void gen_expr_between(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_between(ast));
  EXTRACT_NOTNULL(range, ast->right);

  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" BETWEEN ");
  gen_expr(range->left, pri_new);
  gen_printf(" AND ");
  gen_expr(range->right, pri_new);
  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_not_between(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_not_between(ast));
  EXTRACT_NOTNULL(range, ast->right);

  if (pri_new < pri) gen_printf("(");
  gen_expr(ast->left, pri_new);
  gen_printf(" NOT BETWEEN ");
  gen_expr(range->left, pri_new);
  gen_printf(" AND ");
  gen_expr(range->right, pri_new);
  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_between_rewrite(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_between_rewrite(ast));
  EXTRACT_NOTNULL(range, ast->right);

  if (pri_new < pri) gen_printf("(");
  gen_printf("BETWEEN REWRITE ");
  gen_expr(range->left, pri_new);
  gen_printf(" := ");
  gen_expr(ast->left, pri_new);
  gen_printf(" CHECK ");
  gen_expr(range->right, pri_new);
  if (pri_new < pri) gen_printf(")");
}

static void gen_expr_case(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_case_expr(ast));
  EXTRACT_ANY(expr, ast->left);
  EXTRACT_NOTNULL(connector, ast->right);
  EXTRACT_NOTNULL(case_list, connector->left);
  EXTRACT_ANY(else_expr, connector->right);

  // case is like parens already, you never need more parens
  gen_printf("CASE ");
  if (expr) {
    // case can have expression or just when clauses
    gen_root_expr(expr);
    gen_printf(" ");
  }
  gen_case_list(case_list);
  if (else_expr) {
    gen_printf("ELSE ");
    gen_root_expr(else_expr);
    gen_printf("\n");
  }
  gen_printf("END");
}

static void gen_expr_select(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_select_stmt(ast));
  gen_printf("( ");
  gen_select_stmt(ast);
  gen_printf(" )");
}

static void gen_expr_cast(ast_node *ast, CSTR op, int32_t pri, int32_t pri_new) {
  Contract(is_ast_cast_expr(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, ast->right);

  if (gen_callbacks && gen_callbacks->minify_casts) {
    if (is_ast_null(expr)) {
      // when generating the actual SQL for Sqlite, we don't need to include cast expressions on NULL
      // we only need those for type checking.
      gen_printf("NULL");
      return;
    }

    if (expr->sem && ast->sem) {
      // If the expression is already of the correct type (less nullability), we don't need the cast at all.
      sem_t core_type_expr = core_type_of(expr->sem->sem_type);
      sem_t core_type_ast = core_type_of(ast->sem->sem_type);
      if (core_type_expr == core_type_ast) {
        gen_printf("(");
        gen_expr(expr, EXPR_PRI_ROOT);
        gen_printf(")");
        return;
      }
    }
  }
  gen_printf("CAST(");
  gen_expr(expr, EXPR_PRI_ROOT);
  gen_printf(" AS ");
  gen_data_type(data_type);
  gen_printf(")");
}

static void gen_expr(ast_node *ast, int32_t pri) {
  // These are all the expressions there are, we have to find it in this table
  // or else someone added a new expression type and it isn't supported yet.
  symtab_entry *entry = symtab_find(gen_exprs, ast->type);
  Invariant(entry);
  gen_expr_dispatch *disp = (gen_expr_dispatch*)entry->val;
  disp->func(ast, disp->str, pri, disp->pri_new);
}

cql_noexport void gen_root_expr(ast_node *ast) {
  gen_expr(ast, EXPR_PRI_ROOT);
}

static void gen_as_alias(ast_node *ast) {
  EXTRACT_STRING(name, ast->left);

  gen_printf(" AS %s", name);
}

static void gen_select_expr(ast_node *ast) {
  Contract(is_ast_select_expr(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(opt_as_alias, ast->right);

  gen_root_expr(expr);

  if (opt_as_alias) {
    EXTRACT_STRING(name, opt_as_alias->left);

    if (used_alias_syms && !symtab_find(used_alias_syms, name)) {
      return;
    }

    gen_as_alias(opt_as_alias);
  }
}

static void gen_select_expr_list(ast_node *ast) {

  if (is_ast_star(ast->left)) {
    Contract(ast->right == NULL);
    if (!eval_star_callback(ast->left)) {
      gen_printf("*");
    }
    return;
  }

  symtab *temp = used_alias_syms;
  used_alias_syms = NULL;

  if (ast->sem && gen_callbacks && gen_callbacks->minify_aliases) {
    used_alias_syms = ast->sem->used_symbols;
  }

  for (ast_node *item = ast; item; item = item->right) {
    ast_node *expr = item->left;
    if (is_ast_table_star(expr)) {
      if (!eval_star_callback(expr)) {
        EXTRACT_NOTNULL(table_star, expr);
        EXTRACT_STRING(name, table_star->left);
        gen_printf("%s.*", name);
      }
    }
    else {
      EXTRACT_NOTNULL(select_expr, expr);
      gen_select_expr(select_expr);
    }
    if (item->right) {
      gen_printf(", ");
    }
  }
  used_alias_syms = temp;
}

static void gen_table_or_subquery(ast_node *ast) {
  Contract(is_ast_table_or_subquery(ast));

  EXTRACT_ANY_NOTNULL(factor, ast->left);

  if (is_ast_str(factor)) {
    EXTRACT_STRING(name, factor);
    gen_printf("%s", name);
  }
  else if (is_ast_select_stmt(factor) || is_ast_with_select_stmt(factor)) {
    gen_printf("(");
    gen_select_stmt(factor);
    gen_printf(")");
  }
  else if (is_ast_table_function(factor)) {
    EXTRACT_STRING(name, factor->left);
    EXTRACT(arg_list, factor->right);
    gen_printf("%s(", name);
    gen_arg_list(arg_list);
    gen_printf(")");
  }
  else {
    // this is all that's left
    gen_printf("(");
    gen_query_parts(factor);
    gen_printf(")");
  }

  EXTRACT(opt_as_alias, ast->right);
  if (opt_as_alias) {
    gen_as_alias(opt_as_alias);
  }
}

static void gen_join_cond(ast_node *ast) {
  Contract(is_ast_join_cond(ast));
  EXTRACT_ANY_NOTNULL(cond_type, ast->left);

  if (is_ast_on(cond_type)) {
    gen_printf(" ON ");
    gen_root_expr(ast->right);
  }
  else {
    // only other ast type that is allowed
    Contract(is_ast_using(cond_type));
    gen_printf(" USING (");
    gen_name_list(ast->right);
    gen_printf(")");
  }
}

static void gen_join_target(ast_node *ast) {
  Contract(is_ast_join_target(ast));
  EXTRACT_OPTION(join_type, ast->left);

  switch (join_type) {
    case JOIN_INNER: gen_printf("\n  INNER JOIN "); break;
    case JOIN_CROSS: gen_printf("\n  CROSS JOIN "); break;
    case JOIN_LEFT_OUTER: gen_printf("\n  LEFT OUTER JOIN "); break;
    case JOIN_RIGHT_OUTER: gen_printf("\n  RIGHT OUTER JOIN "); break;
    case JOIN_LEFT: gen_printf("\n  LEFT JOIN "); break;
    case JOIN_RIGHT: gen_printf("\n  RIGHT JOIN "); break;
  }

  EXTRACT_NOTNULL(table_join, ast->right);
  EXTRACT_NOTNULL(table_or_subquery, table_join->left);
  gen_table_or_subquery(table_or_subquery);

  EXTRACT(join_cond, table_join->right);
  if (join_cond) {
    gen_join_cond(join_cond);
  }
}

static void gen_join_target_list(ast_node *ast) {
  Contract(is_ast_join_target_list(ast));

  for (ast_node *item = ast; item; item = item->right) {
    EXTRACT(join_target, item->left);
    gen_join_target(join_target);
  }
}

static void gen_join_clause(ast_node *ast) {
  Contract(is_ast_join_clause(ast));
  EXTRACT_NOTNULL(table_or_subquery, ast->left);
  EXTRACT_NOTNULL(join_target_list, ast->right);

  gen_table_or_subquery(table_or_subquery);
  gen_join_target_list(join_target_list);
}

static void gen_table_or_subquery_list(ast_node *ast) {
  Contract(is_ast_table_or_subquery_list(ast));

  for (ast_node *item = ast; item; item = item->right) {
    gen_table_or_subquery(item->left);
    if (item->right) {
      gen_printf(",\n");
    }
  }
}

static void gen_query_parts(ast_node *ast) {
  if (is_ast_table_or_subquery_list(ast)) {
    gen_table_or_subquery_list(ast);
  }
  else {
    Contract(is_ast_join_clause(ast)); // this is the only other choice
    gen_join_clause(ast);
  }
}

static void gen_asc_desc(ast_node *ast) {
  if (is_ast_asc(ast)) {
    gen_printf(" ASC");
  }
  else if (is_ast_desc(ast)) {
    gen_printf(" DESC");
  }
  else {
    Contract(!ast);
  }
}

static void gen_groupby_list(ast_node *ast) {
  Contract(is_ast_groupby_list(ast));

  for (ast_node *item = ast; item ; item = item->right) {
    Contract(is_ast_groupby_list(item));
    EXTRACT_NOTNULL(groupby_item, item->left);
    EXTRACT_ANY_NOTNULL(expr, groupby_item->left);
    EXTRACT_ANY(opt_asc_desc, groupby_item->right);

    gen_root_expr(expr);
    gen_asc_desc(opt_asc_desc);

    if (item->right) {
      gen_printf(", ");
    }
  }
}

static void gen_opt_where(ast_node *ast) {
  gen_printf("\n  ");
  gen_opt_where_without_new_line(ast);
}

static void gen_opt_where_without_new_line(ast_node *ast) {
  Contract(is_ast_opt_where(ast));

  gen_printf("WHERE ");
  gen_root_expr(ast->left);
}

static void gen_opt_groupby(ast_node *ast) {
  Contract(is_ast_opt_groupby(ast));
  EXTRACT_NOTNULL(groupby_list, ast->left);

  gen_printf("\n  GROUP BY ");
  gen_groupby_list(groupby_list);
}

static void gen_opt_orderby(ast_node *ast) {
  Contract(is_ast_opt_orderby(ast));
  EXTRACT(groupby_list, ast->left);

  gen_printf("ORDER BY ");
  gen_groupby_list(groupby_list);
}

static void gen_opt_limit(ast_node *ast) {
  Contract(is_ast_opt_limit(ast));

  gen_printf("\nLIMIT ");
  gen_root_expr(ast->left);
}

static void gen_opt_offset(ast_node *ast) {
  Contract(is_ast_opt_offset(ast));

  gen_printf("\nOFFSET ");
  gen_root_expr(ast->left);
}

static void gen_window_name_defn(ast_node *ast) {
  Contract(is_ast_window_name_defn(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(window_defn, ast->right);

  gen_printf("\n    %s AS", name);
  gen_window_defn(window_defn);
}

static void gen_window_name_defn_list(ast_node *ast) {
  Contract(is_ast_window_name_defn_list(ast));
  for (ast_node *item = ast; item; item = item->right) {
    EXTRACT_NOTNULL(window_name_defn, item->left);
    gen_window_name_defn(window_name_defn);
    if (item->right) {
      gen_printf(", ");
    }
  }
}

static void gen_window_clause(ast_node *ast) {
  Contract(is_ast_window_clause(ast));
  EXTRACT_NOTNULL(window_name_defn_list, ast->left);

  gen_window_name_defn_list(window_name_defn_list);
}

static void gen_opt_select_window(ast_node *ast) {
  Contract(is_ast_opt_select_window(ast));
  EXTRACT_NOTNULL(window_clause, ast->left);

  gen_printf("\n  WINDOW ");
  gen_window_clause(window_clause);
}

static void gen_select_from_etc(ast_node *ast) {
  Contract(is_ast_select_from_etc(ast));

  EXTRACT_ANY(query_parts, ast->left);
  EXTRACT_NOTNULL(select_where, ast->right);
  EXTRACT(opt_where, select_where->left);
  EXTRACT_NOTNULL(select_groupby, select_where->right);
  EXTRACT(opt_groupby, select_groupby->left);
  EXTRACT_NOTNULL(select_having, select_groupby->right);
  EXTRACT(opt_having, select_having->left);
  EXTRACT(opt_select_window, select_having->right);

  if (query_parts) {
    gen_printf ("\n  FROM ");
    gen_query_parts(query_parts);
  }
  if (opt_where) {
    gen_opt_where(opt_where);
  }
  if (opt_groupby) {
    gen_opt_groupby(opt_groupby);
  }
  if (opt_having) {
    gen_printf("\n  HAVING ");
    gen_root_expr(opt_having->left);
  }
  if (opt_select_window) {
    gen_opt_select_window(opt_select_window);
  }
}

static void gen_select_orderby(ast_node *ast) {
  Contract(is_ast_select_orderby(ast));
  EXTRACT(opt_orderby, ast->left);
  EXTRACT_NOTNULL(select_limit, ast->right);
  EXTRACT(opt_limit, select_limit->left);
  EXTRACT_NOTNULL(select_offset, select_limit->right);
  EXTRACT(opt_offset, select_offset->left);

  if (opt_orderby) {
    gen_printf("\n");
    gen_opt_orderby(opt_orderby);
  }
  if (opt_limit) {
    gen_opt_limit(opt_limit);
  }
  if (opt_offset) {
    gen_opt_offset(opt_offset);
  }
}

static void gen_select_expr_list_con(ast_node *ast) {
  Contract(is_ast_select_expr_list_con(ast));
  EXTRACT(select_expr_list, ast->left);
  EXTRACT(select_from_etc, ast->right);

  gen_select_expr_list(select_expr_list);
  if (select_from_etc) {
    gen_select_from_etc(select_from_etc);
  }
}

cql_noexport void init_gen_sql_callbacks(gen_sql_callbacks *cb)
{
  memset((void *)cb, 0, sizeof(*gen_callbacks));
  // with callbacks is for SQLite be default, the normal raw output
  // case is done with callbacks == NULL
  cb->for_sqlite = true;
}

static void gen_select_statement_type(ast_node *ast) {
  Contract(is_ast_select_core(ast));
  EXTRACT_ANY(select_opts, ast->left);

  if (select_opts && is_ast_select_values(select_opts)) {
    gen_printf("VALUES");
  } else {
    gen_printf("SELECT");
    if (select_opts) {
      Contract(is_ast_select_opts(select_opts));
      gen_select_opts(select_opts);
    }
  }
}

static void gen_values(ast_node *ast) {
  Contract(is_ast_values(ast));
  for (ast_node *item = ast; item; item = item->right) {
    EXTRACT(insert_list, item->left);
    gen_printf("(");
    if (insert_list) {
      gen_insert_list(insert_list);
    }
    gen_printf(")");
    if (item->right) {
      gen_printf(", ");
    }
  }
}

cql_noexport void gen_select_core(ast_node *ast) {
  Contract(is_ast_select_core(ast));
  EXTRACT_ANY(select_core_left, ast->left);

  gen_select_statement_type(ast);

  // select_core subtree can be a SELECT or VALUES statement
  if (is_ast_select_values(select_core_left)) {
    // VALUES [values]
    EXTRACT(values, ast->right);
    gen_values(values);
  } else {
    // SELECT [select_expr_list_con]
    // We're making sure that we're in the SELECT clause of the select stmt
    Contract(select_core_left == NULL || is_ast_select_opts(select_core_left));
    gen_printf(" ");
    EXTRACT_NOTNULL(select_expr_list_con, ast->right);
    gen_select_expr_list_con(select_expr_list_con);
  }
}

static void gen_select_no_with(ast_node *ast) {
  Contract(is_ast_select_stmt(ast));
  EXTRACT_NOTNULL(select_core_list, ast->left);
  EXTRACT_NOTNULL(select_orderby, ast->right);

  gen_select_core_list(select_core_list);
  gen_select_orderby(select_orderby);
}

static void gen_cte_decl(ast_node *ast)  {
  Contract(is_ast_cte_decl(ast));
  EXTRACT_STRING(name, ast->left);
  gen_printf("%s (", name);
  if (is_ast_star(ast->right)) {
    gen_printf("*", name);
  }
  else {
    gen_name_list(ast->right);
  }
  gen_printf(")", name);
}

static void gen_cte_table(ast_node *ast)  {
  Contract(is_ast_cte_table(ast));
  EXTRACT(cte_decl, ast->left);
  EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

  gen_cte_decl(cte_decl);
  gen_printf(" AS (");
  gen_select_stmt(select_stmt);
  gen_printf(")");
}

static void gen_cte_tables(ast_node *ast)  {
  Contract(is_ast_cte_tables(ast));

  while (ast) {
    gen_cte_table(ast->left);
    if (ast->right) {
      gen_printf(",\n");
    }
    ast = ast->right;
  }
}

static void gen_with_prefix(ast_node *ast) {
  EXTRACT(cte_tables, ast->left);

  // for us there is no difference between WITH and WITH RECURSIVE
  // except we have to remember which one it was so that we can
  // emit the same thing we saw.  Sqlite lets you do recursion
  // even if don't use WITH RECURSIVE
  if (is_ast_with(ast)) {
    gen_printf("WITH\n");
  }
  else {
    Contract(is_ast_with_recursive(ast));
    gen_printf("WITH RECURSIVE\n");
  }

  gen_cte_tables(cte_tables);
  gen_printf("\n");
}

static void gen_with_select_stmt(ast_node *ast) {
  Contract(is_ast_with_select_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT_ANY_NOTNULL(select_stmt, ast->right);

  gen_with_prefix(with_prefix);
  gen_select_stmt(select_stmt);
}

static void gen_select_core_list(ast_node *ast) {
  Contract(is_ast_select_core_list(ast));
  EXTRACT_NOTNULL(select_core, ast->left);

  gen_select_core(select_core);

  EXTRACT(select_core_compound, ast->right);
  if (!select_core_compound) {
    return;
  }
  EXTRACT_OPTION(compound_operator, select_core_compound->left);
  EXTRACT_NOTNULL(select_core_list, select_core_compound->right);

  gen_printf("\n%s\n", get_compound_operator_name(compound_operator));
  gen_select_core_list(select_core_list);
}

static void gen_select_stmt(ast_node *ast) {
  if (is_ast_with_select_stmt(ast)) {
    gen_with_select_stmt(ast);
  }
  else {
    Contract(is_ast_select_stmt(ast));
    gen_select_no_with(ast);
  }
}

static void gen_version_attrs(ast_node *_Nullable ast) {
  for (ast_node *attr = ast; attr; attr = attr->right) {
    if (is_ast_recreate_attr(attr)) {
      gen_recreate_attr(attr);
    }
    else if (is_ast_create_attr(attr)) {
      gen_create_attr(attr);
    } else {
      Contract(is_ast_delete_attr(attr)); // the only other kind
      gen_delete_attr(attr);
    }
  }
}

// If there is a handler, the handler will decide what to do.  If there is no handler
// or the handler returns false, then we honor the flag bit.  This lets you override
// the if_not_exists flag forcing it to be either ignored or enabled.  Both are potentially
// needed.  When emitting schema creation scripts for instance we always use IF NOT EXISTS
// even if the schema declaration didn't have it (which it usually doesn't).
static void gen_if_not_exists(ast_node *ast, bool_t if_not_exist) {
  bool_t if_not_exists_callback = gen_callbacks && gen_callbacks->if_not_exists_callback;
  bool_t handled = false;

  if (if_not_exists_callback) {
    handled = gen_callbacks->if_not_exists_callback(ast, gen_callbacks->if_not_exists_context, output);
  }

  if (if_not_exist && !handled) {
    gen_printf("IF NOT EXISTS ");
  }
}

static void gen_create_view_stmt(ast_node *ast) {
  Contract(is_ast_create_view_stmt(ast));
  EXTRACT_OPTION(flags, ast->left);
  EXTRACT(view_and_attrs, ast->right);
  EXTRACT(name_and_select, view_and_attrs->left);
  EXTRACT_ANY(attrs, view_and_attrs->right);
  EXTRACT_ANY_NOTNULL(select_stmt, name_and_select->right);
  EXTRACT_ANY_NOTNULL(name_ast, name_and_select->left);
  EXTRACT_STRING(name, name_ast);

  bool_t if_not_exist = !!(flags & VIEW_IF_NOT_EXISTS);

  gen_printf("CREATE ");
  if (flags & VIEW_IS_TEMP) {
    gen_printf("TEMP ");
  }
  gen_printf("VIEW ");
  gen_if_not_exists(ast, if_not_exist);

  gen_printf("%s AS\n", name);
  gen_select_stmt(select_stmt);
  gen_version_attrs(attrs);
}

static void gen_create_trigger_stmt(ast_node *ast) {
  Contract(is_ast_create_trigger_stmt(ast));

  EXTRACT_OPTION(flags, ast->left);
  EXTRACT_NOTNULL(trigger_body_vers, ast->right);
  EXTRACT_ANY(trigger_attrs, trigger_body_vers->right);
  EXTRACT_NOTNULL(trigger_def, trigger_body_vers->left);
  EXTRACT_STRING(trigger_name, trigger_def->left);
  EXTRACT_NOTNULL(trigger_condition, trigger_def->right);
  EXTRACT_OPTION(cond_flags, trigger_condition->left);
  flags |= cond_flags;
  EXTRACT_NOTNULL(trigger_op_target, trigger_condition->right);
  EXTRACT_NOTNULL(trigger_operation, trigger_op_target->left);
  EXTRACT_OPTION(op_flags,  trigger_operation->left);
  EXTRACT(name_list, trigger_operation->right);
  flags |= op_flags;
  EXTRACT_NOTNULL(trigger_target_action, trigger_op_target->right);
  EXTRACT_STRING(table_name, trigger_target_action->left);
  EXTRACT_NOTNULL(trigger_action, trigger_target_action->right);
  EXTRACT_OPTION(action_flags, trigger_action->left);
  flags |= action_flags;
  EXTRACT_NOTNULL(trigger_when_stmts, trigger_action->right);
  EXTRACT_ANY(when_expr, trigger_when_stmts->left);
  EXTRACT_NOTNULL(stmt_list, trigger_when_stmts->right);

  gen_printf("CREATE ");
  if (flags & TRIGGER_IS_TEMP) {
    gen_printf("TEMP ");
  }
  gen_printf("TRIGGER ");
  gen_if_not_exists(ast, !!(flags & TRIGGER_IF_NOT_EXISTS));

  gen_printf("%s\n  ", trigger_name);

  if (flags & TRIGGER_BEFORE) {
    gen_printf("BEFORE ");
  }
  else if (flags & TRIGGER_AFTER) {
    gen_printf("AFTER ");
  }
  else if (flags & TRIGGER_INSTEAD_OF) {
    gen_printf("INSTEAD OF ");
  }

  if (flags & TRIGGER_DELETE) {
    gen_printf("DELETE ");
  }
  else if (flags & TRIGGER_INSERT) {
    gen_printf("INSERT ");
  }
  else {
    gen_printf("UPDATE ");
    if (name_list) {
      gen_printf("OF ");
      gen_name_list(name_list);
      gen_printf(" ");
    }
  }
  gen_printf("ON %s", table_name);

  if (flags & TRIGGER_FOR_EACH_ROW) {
    gen_printf("\n  FOR EACH ROW");
  }

  if (when_expr) {
    gen_printf("\n  WHEN ");
    gen_root_expr(when_expr);
  }

  gen_printf("\nBEGIN\n");
  gen_stmt_list(stmt_list);
  gen_printf("END");
  gen_version_attrs(trigger_attrs);
}

static void gen_create_table_stmt(ast_node *ast) {
  Contract(is_ast_create_table_stmt(ast));
  EXTRACT(create_table_name_flags, ast->left);
  EXTRACT(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_STRING(name, create_table_name_flags->right);
  EXTRACT_NOTNULL(col_key_list, ast->right);

  bool_t temp = !!(flags & TABLE_IS_TEMP);
  bool_t if_not_exist = !!(flags & TABLE_IF_NOT_EXISTS);
  bool_t no_rowid = !!(flags & TABLE_IS_NO_ROWID);

  gen_printf("CREATE ");
  if (temp) {
    gen_printf("TEMP ");
  }

  gen_printf("TABLE ");
  gen_if_not_exists(ast, if_not_exist);

  gen_printf("%s(\n", name);
  gen_col_key_list(col_key_list);
  gen_printf("\n)");
  if (no_rowid) {
    gen_printf(" WITHOUT ROWID");
  }
  gen_version_attrs(table_attrs);
}

static void gen_create_virtual_table_stmt(ast_node *ast) {
  Contract(is_ast_create_virtual_table_stmt(ast));
  EXTRACT_NOTNULL(module_info, ast->left);
  EXTRACT_NOTNULL(create_table_stmt, ast->right);
  EXTRACT(create_table_name_flags, create_table_stmt->left);
  EXTRACT(table_flags_attrs, create_table_name_flags->left);
  EXTRACT_OPTION(flags, table_flags_attrs->left);
  EXTRACT_ANY(table_attrs, table_flags_attrs->right);
  EXTRACT_STRING(name, create_table_name_flags->right);
  EXTRACT_NOTNULL(col_key_list, create_table_stmt->right);
  EXTRACT_STRING(module_name, module_info->left);
  EXTRACT_ANY(module_args, module_info->right);

  bool_t if_not_exist = !!(flags & TABLE_IF_NOT_EXISTS);

  gen_printf("CREATE VIRTUAL TABLE ");
  gen_if_not_exists(ast, if_not_exist);
  gen_printf("%s USING %s", name, module_name);

  if (!for_sqlite()) {
    if (is_ast_following(module_args)) {
      gen_printf(" (ARGUMENTS FOLLOWING) ");
    }
    else if (module_args) {
      gen_printf(" ");
      gen_misc_attr_value(module_args);
      gen_printf(" ");
    }
    else {
      gen_printf(" ");
    }

    // When emitting to SQLite we do not include the column declaration part
    // just whatever the args were because SQLite doesn't parse that part of the CQL syntax.
    // Note that CQL does not support general args because that's not parseable with this parser
    // tech but this is pretty general.  The declaration part is present here so that
    // CQL knows the type info of the net table we are creating.
    // Note also that virtual tables are always on the recreate plan, it isn't an option
    // and this will mean that you can't make a foreign key to a virtual table which is probably
    // a wise thing.

    gen_printf("AS (\n");
    gen_col_key_list(col_key_list);
    gen_printf("\n)");

    // delete attribute is the only option (recreate by default)
    if (!is_ast_recreate_attr(table_attrs)) {
      Invariant(is_ast_delete_attr(table_attrs));
      gen_delete_attr(table_attrs);
    }
  }
  else {
    if (is_ast_following(module_args)) {
      gen_printf(" (\n");
      gen_col_key_list(col_key_list);
      gen_printf(")");
    } else if (module_args) {
      gen_printf(" ");
      gen_misc_attr_value(module_args);
    }
  }
}

static void gen_drop_view_stmt(ast_node *ast) {
  Contract(is_ast_drop_view_stmt(ast));
  EXTRACT_ANY(if_exists, ast->left);
  EXTRACT_STRING(name, ast->right);

  gen_printf("DROP VIEW ");
  if (if_exists) {
    gen_printf("IF EXISTS ");
  }
  gen_printf("%s", name);
}

static void gen_drop_table_stmt(ast_node *ast) {
  Contract(is_ast_drop_table_stmt(ast));
  EXTRACT_ANY(if_exists, ast->left);
  EXTRACT_STRING(name, ast->right);

  gen_printf("DROP TABLE ");
  if (if_exists) {
    gen_printf("IF EXISTS ");
  }
  gen_printf("%s", name);
}

static void gen_drop_index_stmt(ast_node *ast) {
  Contract(is_ast_drop_index_stmt(ast));
  EXTRACT_ANY(if_exists, ast->left);
  EXTRACT_STRING(name, ast->right);

  gen_printf("DROP INDEX ");
  if (if_exists) {
    gen_printf("IF EXISTS ");
  }
  gen_printf("%s", name);
}

static void gen_drop_trigger_stmt(ast_node *ast) {
  Contract(is_ast_drop_trigger_stmt(ast));
  EXTRACT_ANY(if_exists, ast->left);
  EXTRACT_STRING(name, ast->right);

  gen_printf("DROP TRIGGER ");
  if (if_exists) {
    gen_printf("IF EXISTS ");
  }
  gen_printf("%s", name);
}

static void gen_alter_table_add_column_stmt(ast_node *ast) {
  Contract(is_ast_alter_table_add_column_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT(col_def, ast->right);

  gen_printf("ALTER TABLE %s ADD COLUMN ", name);
  gen_col_def(col_def);
}

static void gen_cond_action(ast_node *ast) {
  Contract(is_ast_cond_action(ast));
  EXTRACT(stmt_list, ast->right);

  gen_root_expr(ast->left);
  gen_printf(" THEN\n");
  gen_stmt_list(stmt_list);
}

static void gen_elseif_list(ast_node *ast) {
  Contract(is_ast_elseif(ast));

  while (ast) {
    Contract(is_ast_elseif(ast));
    EXTRACT(cond_action, ast->left);
    gen_printf("ELSE IF ");
    gen_cond_action(cond_action);
    ast = ast->right;
  }
}

static void gen_if_stmt(ast_node *ast) {
  Contract(is_ast_if_stmt(ast));
  EXTRACT(cond_action, ast->left);
  EXTRACT(if_alt, ast->right);

  gen_printf("IF ");
  gen_cond_action(cond_action);
  if (if_alt) {
    EXTRACT(elseif, if_alt->left);
    EXTRACT_NAMED(elsenode, else, if_alt->right);

    if (elseif) {
      gen_elseif_list(elseif);
    }

    if (elsenode) {
      gen_printf("ELSE\n");
      EXTRACT(stmt_list, elsenode->left);
      gen_stmt_list(stmt_list);
    }
  }
  gen_printf("END IF");
}

static void gen_delete_stmt(ast_node *ast) {
  Contract(is_ast_delete_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT(opt_where, ast->right);

  gen_printf("DELETE FROM %s", name);
  if (opt_where) {
    gen_printf(" WHERE ");
    gen_root_expr(opt_where->left);
  }
}

static void gen_with_delete_stmt(ast_node *ast) {
  Contract(is_ast_with_delete_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT_NOTNULL(delete_stmt, ast->right);

  gen_with_prefix(with_prefix);
  gen_delete_stmt(delete_stmt);
}

static void gen_update_entry(ast_node *ast) {
  Contract(is_ast_update_entry(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->right)
  EXTRACT_STRING(name, ast->left);
  gen_printf("%s = ", name);

  gen_root_expr(expr);
}

static void gen_update_list(ast_node *ast) {
  Contract(is_ast_update_list(ast));

  for (ast_node *item = ast; item; item = item->right) {
    Contract(is_ast_update_list(item));
    EXTRACT_NOTNULL(update_entry, item->left);

    gen_update_entry(update_entry);
    if (item->right) {
      gen_printf(",\n");
    }
  }
}

static void gen_from_shape(ast_node *ast) {
  Contract(is_ast_from_shape(ast));
  EXTRACT_STRING(shape_name, ast->right);
  EXTRACT_ANY(column_spec, ast->left);
  gen_printf("FROM %s", shape_name);
  gen_column_spec(column_spec);
}

static void gen_update_cursor_stmt(ast_node *ast) {
  Contract(is_ast_update_cursor_stmt(ast));
  EXTRACT_ANY(cursor, ast->left);
  EXTRACT_STRING(name, cursor);
  EXTRACT_ANY_NOTNULL(columns_values, ast->right);

  gen_printf("UPDATE CURSOR %s", name);

  if (is_ast_expr_names(columns_values)) {
    gen_printf(" USING ");
    gen_expr_names(columns_values);
  }
  else {
    EXTRACT_ANY(column_spec, columns_values->left);
    EXTRACT_ANY(insert_list, columns_values->right);

    gen_column_spec(column_spec);
    gen_printf(" ");
    if (is_ast_from_shape(insert_list)) {
      gen_from_shape(insert_list);
    }
    else {
      gen_printf("FROM VALUES(");
      gen_insert_list(insert_list);
      gen_printf(")");
    }
  }
}

static void gen_update_stmt(ast_node *ast) {
  Contract(is_ast_update_stmt(ast));
  EXTRACT_NOTNULL(update_set, ast->right);
  EXTRACT_NOTNULL(update_list, update_set->left);
  EXTRACT_NOTNULL(update_where, update_set->right);
  EXTRACT(opt_where, update_where->left);
  EXTRACT_NOTNULL(update_orderby, update_where->right);
  EXTRACT(opt_orderby, update_orderby->left);
  EXTRACT(opt_limit, update_orderby->right);

  gen_printf("UPDATE");
  if (ast->left) {
    EXTRACT_STRING(name, ast->left);
    gen_printf(" %s", name);
  }
  gen_printf("\nSET ");
  gen_update_list(update_list);
  if (opt_where) {
    gen_opt_where(opt_where);
  }
  if (opt_orderby) {
    gen_printf("\n");
    gen_opt_orderby(opt_orderby);
  }
  if (opt_limit) {
    gen_opt_limit(opt_limit);
  }
}

static void gen_with_update_stmt(ast_node *ast) {
  Contract(is_ast_with_update_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT_NOTNULL(update_stmt, ast->right);

  gen_with_prefix(with_prefix);
  gen_update_stmt(update_stmt);
}

static void gen_insert_list(ast_node *_Nullable ast) {
  Contract(!ast || is_ast_insert_list(ast));

  while (ast) {
    Contract(is_ast_insert_list(ast));
    gen_root_expr(ast->left);

    if (ast->right) {
      gen_printf(", ");
    }
    ast = ast->right;
  }
}

cql_noexport void gen_insert_type(ast_node *ast) {
  if (is_ast_insert_or_ignore(ast)) {
    gen_printf("INSERT OR IGNORE");
  }
  else if (is_ast_insert_or_replace(ast)) {
    gen_printf("INSERT OR REPLACE");
  }
  else if (is_ast_insert_replace(ast)) {
    gen_printf("REPLACE");
  }
  else if (is_ast_insert_or_abort(ast)) {
    gen_printf("INSERT OR ABORT");
  }
  else if (is_ast_insert_or_fail(ast)) {
    gen_printf("INSERT OR FAIL");
  }
  else if (is_ast_insert_or_rollback(ast)) {
     gen_printf("INSERT OR ROLLBACK");
  }
  else {
    Contract(is_ast_insert_normal(ast));
    gen_printf("INSERT");
  }
}

static void gen_insert_dummy_spec(ast_node *ast) {
  Contract(is_ast_insert_dummy_spec(ast));
  EXTRACT_ANY_NOTNULL(seed_expr, ast->left);
  EXTRACT_OPTION(flags, ast->right);

  if (for_sqlite()) {
    return;
  }

  gen_printf(" @DUMMY_SEED(");
  gen_root_expr(seed_expr);
  gen_printf(")");

  if (flags & INSERT_DUMMY_DEFAULTS) {
    gen_printf(" @DUMMY_DEFAULTS");
  }

  if (flags & INSERT_DUMMY_NULLABLES) {
    gen_printf(" @DUMMY_NULLABLES");
  }
}

static void gen_shape_def(ast_node *ast) {
  Contract(is_ast_like(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY(from_args, ast->right);

  gen_printf("LIKE %s", name);
  if (from_args) {
    gen_printf(" ARGUMENTS");
  }
}

static void gen_column_spec(ast_node *ast) {
  // allow null column_spec here so we don't have to test it everywhere
  if (ast) {
    gen_printf("(");
    if (is_ast_like(ast->left)) {
      gen_shape_def(ast->left);
    }
    else {
      EXTRACT(name_list, ast->left);
      if (name_list) {
        gen_name_list(name_list);
      }
    }
    gen_printf(")");
  }
}

static void gen_insert_stmt(ast_node *ast) {
  Contract(is_ast_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(insert_type, ast->left);
  EXTRACT_NOTNULL(name_columns_values, ast->right);
  EXTRACT_STRING(name, name_columns_values->left);
  EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);
  EXTRACT(insert_dummy_spec, insert_type->left);

  gen_insert_type(insert_type);
  gen_printf(" INTO %s", name);

  if (is_ast_expr_names(columns_values)) {
    gen_printf(" USING ");
    gen_expr_names(columns_values);
  }
  else if (is_ast_columns_values(columns_values)) {
    EXTRACT(column_spec, columns_values->left);
    EXTRACT_ANY(insert_list, columns_values->right);
    gen_column_spec(column_spec);
    gen_printf(" ");

    if (is_select_stmt(insert_list)) {
      gen_select_stmt(insert_list);
    }
    else if (is_ast_from_shape(insert_list)) {
      gen_from_shape(insert_list);
    }
    else {
      gen_printf("VALUES(");
      gen_insert_list(insert_list);
      gen_printf(")");
    }

    if (insert_dummy_spec) {
      gen_insert_dummy_spec(insert_dummy_spec);
    }
  }
  else {
    // INSERT [conflict resolution] INTO name DEFAULT VALUES
    Contract(is_ast_default_columns_values(columns_values));
    gen_printf(" DEFAULT VALUES");
  }
}

static void gen_with_insert_stmt(ast_node *ast) {
  Contract(is_ast_with_insert_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT_NOTNULL(insert_stmt, ast->right);

  gen_with_prefix(with_prefix);
  gen_insert_stmt(insert_stmt);
}

static void gen_expr_names(ast_node *ast) {
  Contract(is_ast_expr_names(ast));

  for (ast_node *list = ast; list; list = list->right) {
    EXTRACT(expr_name, list->left);
    EXTRACT_ANY(expr, expr_name->left);
    EXTRACT_NOTNULL(opt_as_alias, expr_name->right);

    gen_expr(expr, EXPR_PRI_ROOT);
    gen_as_alias(opt_as_alias);

    if (list->right) {
      gen_printf(", ");
    }
  }
}

static void gen_fetch_values_stmt(ast_node *ast) {
  Contract(is_ast_fetch_values_stmt(ast));

  EXTRACT(insert_dummy_spec, ast->left);
  EXTRACT_NOTNULL(name_columns_values, ast->right);
  EXTRACT_STRING(name, name_columns_values->left);
  EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);

  gen_printf("FETCH %s", name);

  if (is_ast_expr_names(columns_values)) {
    gen_printf(" USING ");
    gen_expr_names(columns_values);
  } else {
    EXTRACT(column_spec, columns_values->left);
    gen_column_spec(column_spec);
    gen_printf(" ");

    if (is_ast_from_shape(columns_values->right)) {
      gen_from_shape(columns_values->right);
    }
    else {
      EXTRACT(insert_list, columns_values->right);
      gen_printf("FROM VALUES(", name);
      gen_insert_list(insert_list);
      gen_printf(")");
    }
  }

  if (insert_dummy_spec) {
    gen_insert_dummy_spec(insert_dummy_spec);
  }
}

static void gen_assign(ast_node *ast) {
  Contract(is_ast_assign(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY_NOTNULL(expr, ast->right);

  gen_printf("SET %s := ", name);
  gen_root_expr(expr);
}

static void gen_opt_inout(ast_node *ast) {
  if (is_ast_in(ast)) {
    gen_printf("IN ");
  }
  else if (is_ast_out(ast)) {
    gen_printf("OUT ");
  }
  else if (is_ast_inout(ast)) {
    gen_printf("INOUT ");
  }
  else {
    Contract(!ast);
  }
}

static void gen_normal_param(ast_node *ast) {
  Contract(is_ast_param(ast));
  EXTRACT_ANY(opt_inout, ast->left);
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_STRING(name, param_detail->left);
  EXTRACT_ANY_NOTNULL(data_type, param_detail->right);

  gen_opt_inout(opt_inout);
  gen_printf("%s ", name);
  gen_data_type(data_type);
}

static void gen_like_param(ast_node *ast) {
  Contract(is_ast_param(ast));
  EXTRACT_NOTNULL(param_detail, ast->right);
  EXTRACT_NOTNULL(like, param_detail->right);

  if (param_detail->left) {
    EXTRACT_STRING(name, param_detail->left);
    gen_printf("%s ", name);
  }

  gen_shape_def(like);
}

static void gen_param(ast_node *ast) {
  Contract(is_ast_param(ast));

  EXTRACT_NOTNULL(param_detail, ast->right);
  if (is_ast_like(param_detail->right)) {
    gen_like_param(ast);
  }
  else {
    gen_normal_param(ast);
  }
}

cql_noexport void gen_params(ast_node *ast) {
  Contract(is_ast_params(ast));

  for (ast_node *cur = ast; cur; cur = cur->right) {
    Contract(is_ast_params(cur));
    EXTRACT_NOTNULL(param, cur->left);

    gen_param(param);

    if (cur->right) {
      gen_printf(", ");
    }
  }
}

static void gen_create_proc_stmt(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(stmt_list, proc_params_stmts->right);

  gen_printf("CREATE PROC %s (", name);
  if (params) {
    gen_params(params);
  }
  gen_printf(")\nBEGIN\n");
  gen_stmt_list(stmt_list);
  gen_printf("END");
}

cql_noexport void gen_declare_proc_from_create_proc(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);

  gen_printf("DECLARE PROC %s (", name);
  if (params) {
    gen_params(params);
  }
  gen_printf(")");

  if (ast->sem) {
    if (has_out_stmt_result(ast)) {
      gen_printf(" OUT");
    }

    if (has_out_union_stmt_result(ast)) {
      gen_printf(" OUT UNION");
    }

    if (is_struct(ast->sem->sem_type)) {
      sem_struct *sptr = ast->sem->sptr;

      gen_printf(" (");
      for (int32_t i = 0; i < sptr->count; i++) {
        gen_printf("%s ", sptr->names[i]);
        sem_t sem_type = sptr->semtypes[i];

        // results come from select statements, they cannot include objects!
        Invariant(core_type_of(sem_type) != SEM_TYPE_OBJECT);

        gen_printf("%s", coretype_string(sem_type));

        if (is_not_nullable(sem_type)) {
          gen_printf(" NOT NULL");
        }

        if (sensitive_flag(sem_type)) {
          gen_printf(" @SENSITIVE");
        }

        if (i + 1 < sptr->count) {
          gen_printf(", ");
        }
      }
      gen_printf(")");

      if ((has_out_stmt_result(ast) || has_out_union_stmt_result(ast)) && is_dml_proc(ast->sem->sem_type)) {
        // out [union] can be DML or not, so we have to specify
        gen_printf(" USING TRANSACTION");
      }
    }
    else if (is_dml_proc(ast->sem->sem_type)) {
      gen_printf(" USING TRANSACTION");
    }
  }
}

static void gen_typed_name(ast_node *ast) {
  EXTRACT(typed_name, ast);
  EXTRACT_ANY(name, typed_name->left);
  EXTRACT_ANY_NOTNULL(type, typed_name->right);

  if (name) {
    EXTRACT_STRING(formal, name);
    gen_printf("%s ", formal);
  }

  if (is_ast_like(type)) {
    gen_shape_def(type);
  }
  else {
    gen_data_type(type);
  }
}

static void gen_typed_names(ast_node *ast) {
  Contract(is_ast_typed_names(ast));

  while (ast) {
    Contract(is_ast_typed_names(ast));
    gen_typed_name(ast->left);

    if (ast->right) {
      gen_printf(", ");
    }

    ast = ast->right;
  }
}

static void gen_declare_proc_stmt(ast_node *ast) {
  Contract(is_ast_declare_proc_stmt(ast));
  EXTRACT_NOTNULL(proc_name_type, ast->left);
  EXTRACT_STRING(name, proc_name_type->left);
  EXTRACT_OPTION(type, proc_name_type->right);
  EXTRACT_NOTNULL(proc_params_stmts, ast->right);
  EXTRACT(params, proc_params_stmts->left);
  EXTRACT(typed_names, proc_params_stmts->right);

  gen_printf("DECLARE PROC %s (", name);
  if (params) {
    gen_params(params);
  }
  gen_printf(")");

  if (type & PROC_FLAG_USES_OUT) {
    gen_printf(" OUT");
  }

  if (type & PROC_FLAG_USES_OUT_UNION) {
    gen_printf(" OUT UNION");
  }

  if (typed_names) {
    Contract(type & PROC_FLAG_STRUCT_TYPE);
    gen_printf(" (");
    gen_typed_names(typed_names);
    gen_printf(")");
  }

  // we don't emit USING TRANSACTION unless it's needed

  // if it doesnt use DML it's not needed
  if (!(type & PROC_FLAG_USES_DML)) {
    return;
  }

  // out can be either, so emit it if needed
  if (type & (PROC_FLAG_USES_OUT | PROC_FLAG_USES_OUT_UNION)) {
    gen_printf(" USING TRANSACTION");
    return;
  }

  // if the proc returns a struct not via out then it uses SELECT and so it's implictly DML
  if (type & PROC_FLAG_STRUCT_TYPE) {
    return;
  }

  // it's not an OUT and it doesn't have a result but it does use DML
  // the only flag combo left is a basic dml proc.
  Contract(type == PROC_FLAG_USES_DML);
  gen_printf(" USING TRANSACTION");
}

cql_noexport void gen_declare_proc_from_create_or_decl(ast_node *ast) {
  Contract(is_ast_create_proc_stmt(ast) || is_ast_declare_proc_stmt(ast));
  if (is_ast_create_proc_stmt(ast)) {
    gen_declare_proc_from_create_proc(ast);
  }
  else {
    gen_declare_proc_stmt(ast);
  }
}

static void gen_declare_select_func_stmt(ast_node *ast) {
  Contract(is_ast_declare_select_func_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(func_params_return, ast->right);
  EXTRACT(params, func_params_return->left);
  EXTRACT_ANY_NOTNULL(ret_data_type, func_params_return->right);

  gen_printf("DECLARE SELECT FUNC %s (", name);
  if (params) {
    gen_params(params);
  }
  gen_printf(") ");

  if (is_ast_typed_names(ret_data_type)) {
    // table valued function
    gen_printf("(");
    gen_typed_names(ret_data_type);
    gen_printf(")");
  }
  else {
    // standard function
    gen_data_type(ret_data_type);
  }
}

static void gen_declare_func_stmt(ast_node *ast) {
  Contract(is_ast_declare_func_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_NOTNULL(func_params_return, ast->right);
  EXTRACT(params, func_params_return->left);
  EXTRACT_ANY_NOTNULL(ret_data_type, func_params_return->right);

  gen_printf("DECLARE FUNC %s (", name);
  if (params) {
    gen_params(params);
  }
  gen_printf(") ");

  gen_data_type(ret_data_type);
}

static void gen_declare_vars_type(ast_node *ast) {
  Contract(is_ast_declare_vars_type(ast));
  EXTRACT_NOTNULL(name_list, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, ast->right);

  gen_printf("DECLARE ");
  gen_name_list(name_list);
  gen_printf(" ");
  gen_data_type(data_type);
}

static void gen_declare_cursor(ast_node *ast) {
  Contract(is_ast_declare_cursor(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY_NOTNULL(source, ast->right);

  gen_printf("DECLARE %s CURSOR FOR ", name);

  if (is_ast_str(source)) {
    // The unboxing case gives a name rather than a statement
    EXTRACT_STRING(var_name, ast->right);
    gen_printf("%s", var_name);
  }
  else {
    // The two statement cases are unified
    gen_one_stmt(source);
  }
}

static void gen_declare_cursor_like_name(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_name(ast));
  EXTRACT_STRING(new_cursor_name, ast->left);
  EXTRACT_NOTNULL(like, ast->right);

  gen_printf("DECLARE %s CURSOR ", new_cursor_name);
  gen_shape_def(like);
}

static void gen_declare_cursor_like_select(ast_node *ast) {
  Contract(is_ast_declare_cursor_like_select(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY_NOTNULL(stmt, ast->right);

  gen_printf("DECLARE %s CURSOR LIKE ", name);
  gen_one_stmt(stmt);
}

static void gen_declare_named_type(ast_node *ast) {
  Contract(is_ast_declare_named_type(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, ast->right);

  gen_printf("DECLARE %s TYPE ", name);
  gen_data_type(data_type);
}

static void gen_declare_value_cursor(ast_node *ast) {
  Contract(is_ast_declare_value_cursor(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT_ANY_NOTNULL(stmt, ast->right);

  gen_printf("DECLARE %s CURSOR FETCH FROM ", name);
  gen_one_stmt(stmt);
}

static void gen_declare_enum_stmt(ast_node *ast) {
  Contract(is_ast_declare_enum_stmt(ast));
  EXTRACT_NOTNULL(typed_name, ast->left);
  EXTRACT_NOTNULL(enum_values, ast->right);
  gen_printf("DECLARE ENUM ");
  gen_typed_name(typed_name);
  gen_printf(" (");

  while (enum_values) {
     EXTRACT_NOTNULL(enum_value, enum_values->left);
     EXTRACT_STRING(enum_name, enum_value->left);
     EXTRACT_ANY(expr, enum_value->right);

     gen_printf("\n  %s", enum_name);
     if (expr) {
       gen_printf(" = ");
       gen_root_expr(expr);
     }

     if (enum_values->right) {
       gen_printf(",");
     }

     enum_values = enum_values->right;
  }
  gen_printf("\n)");
}

static void gen_set_from_cursor(ast_node *ast) {
  Contract(is_ast_set_from_cursor(ast));
  EXTRACT_STRING(var_name, ast->left);
  EXTRACT_STRING(cursor_name, ast->right);

  gen_printf("SET %s FROM CURSOR %s", var_name, cursor_name);
}

static void gen_fetch_stmt(ast_node *ast) {
  Contract(is_ast_fetch_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT(name_list, ast->right);

  gen_printf("FETCH %s", name);
  if (name_list) {
    gen_printf(" INTO ", name);
    gen_name_list(name_list);
  }
}

static void gen_while_stmt(ast_node *ast) {
  Contract(is_ast_while_stmt(ast));
  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT(stmt_list, ast->right);

  // WHILE [expr] BEGIN [stmt_list] END

  gen_printf("WHILE ");
  gen_root_expr(expr);

  gen_printf("\nBEGIN\n");
  gen_stmt_list(stmt_list);
  gen_printf("END");
}

static void gen_loop_stmt(ast_node *ast) {
  Contract(is_ast_loop_stmt(ast));
  EXTRACT_NOTNULL(fetch_stmt, ast->left);
  EXTRACT(stmt_list, ast->right);

  // LOOP [fetch_stmt] BEGIN [stmt_list] END

  gen_printf("LOOP ");
  gen_fetch_stmt(fetch_stmt);
  gen_printf("\nBEGIN\n");
  gen_stmt_list(stmt_list);
  gen_printf("END");
}

static void gen_call_stmt(ast_node *ast) {
  Contract(is_ast_call_stmt(ast));
  EXTRACT_STRING(name, ast->left);
  EXTRACT(expr_list, ast->right);

  gen_printf("CALL %s(", name);
  if (expr_list) {
    gen_call_expr_list(expr_list);
  }

  gen_printf(")", name);
}

static void gen_fetch_call_stmt(ast_node *ast) {
  Contract(is_ast_fetch_call_stmt(ast));
  Contract(is_ast_call_stmt(ast->right));
  EXTRACT_STRING(cursor_name, ast->left);
  EXTRACT_ANY_NOTNULL(call_stmt, ast->right);

  gen_printf("FETCH %s FROM ", cursor_name);
  gen_call_stmt(call_stmt);
}

static void gen_continue_stmt(ast_node *ast) {
  Contract(is_ast_continue_stmt(ast));

  gen_printf("CONTINUE");
}

static void gen_leave_stmt(ast_node *ast) {
  Contract(is_ast_leave_stmt(ast));

  gen_printf("LEAVE");
}

static void gen_return_stmt(ast_node *ast) {
  Contract(is_ast_return_stmt(ast));

  gen_printf("RETURN");
}

static void gen_rollback_return_stmt(ast_node *ast) {
  Contract(is_ast_rollback_return_stmt(ast));

  gen_printf("ROLLBACK RETURN");
}

static void gen_commit_return_stmt(ast_node *ast) {
  Contract(is_ast_commit_return_stmt(ast));

  gen_printf("COMMIT RETURN");
}

static void gen_proc_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_proc_savepoint_stmt(ast));
  EXTRACT(stmt_list, ast->left);

  gen_printf("PROC SAVEPOINT");
  gen_printf("\nBEGIN\n");
  gen_stmt_list(stmt_list);
  gen_printf("END");
}

static void gen_throw_stmt(ast_node *ast) {
  Contract(is_ast_throw_stmt(ast));

  gen_printf("THROW");
}

static void gen_begin_trans_stmt(ast_node *ast) {
  Contract(is_ast_begin_trans_stmt(ast));
  EXTRACT_OPTION(mode, ast->left);

  gen_printf("BEGIN");

  if (mode == TRANS_IMMEDIATE) {
    gen_printf(" IMMEDIATE");
  }
  else if (mode == TRANS_EXCLUSIVE) {
    gen_printf(" EXCLUSIVE");
  }
  else {
    // this is the default, and only remaining case, no additional output needed
    Contract(mode == TRANS_DEFERRED);
  }
}

static void gen_commit_trans_stmt(ast_node *ast) {
  Contract(is_ast_commit_trans_stmt(ast));

  gen_printf("COMMIT");
}

static void gen_rollback_trans_stmt(ast_node *ast) {
  Contract(is_ast_rollback_trans_stmt(ast));

  gen_printf("ROLLBACK");

  if (ast->left) {
    EXTRACT_STRING(name, ast->left);
    gen_printf(" TO %s", name);
  }
}

static void gen_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_savepoint_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("SAVEPOINT %s", name);
}

static void gen_release_savepoint_stmt(ast_node *ast) {
  Contract(is_ast_release_savepoint_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("RELEASE %s", name);
}

static void gen_trycatch_stmt(ast_node *ast) {
  Contract(is_ast_trycatch_stmt(ast));
  EXTRACT_NAMED(try_list, stmt_list, ast->left);
  EXTRACT_NAMED(catch_list, stmt_list, ast->right);

  gen_printf("BEGIN TRY\n");
  gen_stmt_list(try_list);
  gen_printf("END TRY;\n");
  gen_printf("BEGIN CATCH\n");
  gen_stmt_list(catch_list);
  gen_printf("END CATCH");
}

static void gen_open_stmt(ast_node *ast) {
  Contract(is_ast_open_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("OPEN %s", name);
}

static void gen_close_stmt(ast_node *ast) {
  Contract(is_ast_close_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("CLOSE %s", name);
}

static void gen_out_stmt(ast_node *ast) {
  Contract(is_ast_out_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("OUT %s", name);
}

static void gen_out_union_stmt(ast_node *ast) {
  Contract(is_ast_out_union_stmt(ast));
  EXTRACT_STRING(name, ast->left);

  gen_printf("OUT UNION %s", name);
}

static void gen_echo_stmt(ast_node *ast) {
  Contract(is_ast_echo_stmt(ast));
  EXTRACT_STRING(rt_name, ast->left);

  gen_printf("@ECHO %s, ", rt_name);
  gen_root_expr(ast->right);  // emit the quoted literal
}

static void gen_schema_upgrade_script_stmt(ast_node *ast) {
  Contract(is_ast_schema_upgrade_script_stmt(ast));

  gen_printf("@SCHEMA_UPGRADE_SCRIPT");
}

static void gen_schema_upgrade_version_stmt(ast_node *ast) {
  Contract(is_ast_schema_upgrade_version_stmt(ast));
  EXTRACT_OPTION(vers, ast->left);

  gen_printf("@SCHEMA_UPGRADE (%d)", vers);
}

static void gen_previous_schema_stmt(ast_node *ast) {
  Contract(is_ast_previous_schema_stmt(ast));

  gen_printf("@PREVIOUS_SCHEMA");
}

static void gen_enforcement_options(ast_node *ast) {
  EXTRACT_OPTION(option, ast);

  switch (option) {
    case ENFORCE_STRICT_JOIN:
      gen_printf("JOIN");
      break;

    case ENFORCE_FK_ON_UPDATE:
      gen_printf("FOREIGN KEY ON UPDATE");
      break;

    case ENFORCE_UPSERT_STMT:
      gen_printf("UPSERT STATEMENT");
      break;

    case ENFORCE_WINDOW_FUNC:
      gen_printf("WINDOW FUNCTION");
      break;

    case ENFORCE_PROCEDURE:
      gen_printf("PROCEDURE");
      break;

    case ENFORCE_WITHOUT_ROWID:
      gen_printf("WITHOUT ROWID");
      break;

    case ENFORCE_TRANSACTION:
      gen_printf("TRANSACTION");
      break;

    default:
      // this is all that's left
      Contract(option == ENFORCE_FK_ON_DELETE);
      gen_printf("FOREIGN KEY ON DELETE");
      break;
  }
}

static void gen_enforce_strict_stmt(ast_node * ast) {
  Contract(is_ast_enforce_strict_stmt(ast));
  gen_printf("@ENFORCE_STRICT ");
  gen_enforcement_options(ast->left);
}

static void gen_enforce_normal_stmt(ast_node * ast) {
  Contract(is_ast_enforce_normal_stmt(ast));
  gen_printf("@ENFORCE_NORMAL ");
  gen_enforcement_options(ast->left);
}

static void gen_enforce_push_stmt(ast_node * ast) {
  Contract(is_ast_enforce_push_stmt(ast));
  gen_printf("@ENFORCE_PUSH");
}

static void gen_enforce_pop_stmt(ast_node * ast) {
  Contract(is_ast_enforce_pop_stmt(ast));
  gen_printf("@ENFORCE_POP");
}

static void gen_region_spec(ast_node *ast) {
  Contract(is_ast_region_spec(ast));
  EXTRACT_OPTION(type, ast->right);
  bool_t is_private = (type == PRIVATE_REGION);

  gen_name(ast->left);
  if (is_private) {
    gen_printf(" PRIVATE");
  }
}

static void gen_region_list(ast_node *ast) {
  Contract(is_ast_region_list(ast));
  while (ast) {
    gen_region_spec(ast->left);
    if (ast->right) {
      gen_printf(", ");
    }
    ast = ast->right;
  }
}

static void gen_declare_deployable_region_stmt(ast_node * ast) {
  Contract(is_ast_declare_deployable_region_stmt(ast));
  gen_printf("@DECLARE_DEPLOYABLE_REGION ");
  gen_name(ast->left);
  if (ast->right) {
    gen_printf(" USING ");
    gen_region_list(ast->right);
  }
}

static void gen_declare_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_declare_schema_region_stmt(ast));
  gen_printf("@DECLARE_SCHEMA_REGION ");
  gen_name(ast->left);
  if (ast->right) {
    gen_printf(" USING ");
    gen_region_list(ast->right);
  }
}

static void gen_begin_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_begin_schema_region_stmt(ast));
  gen_printf("@BEGIN_SCHEMA_REGION ");
  gen_name(ast->left);
}

static void gen_end_schema_region_stmt(ast_node * ast) {
  Contract(is_ast_end_schema_region_stmt(ast));
  gen_printf("@END_SCHEMA_REGION");
}

static void gen_schema_ad_hoc_migration_stmt(ast_node *ast) {
  Contract(is_ast_schema_ad_hoc_migration_stmt(ast));
  EXTRACT_ANY_NOTNULL(attrs, ast->left);

  gen_printf("@SCHEMA_AD_HOC_MIGRATION(");
  gen_version_and_proc(attrs);
  gen_printf(")");
}

static void gen_emit_enums_stmt(ast_node *ast) {
  Contract(is_ast_emit_enums_stmt(ast));
  EXTRACT(name_list, ast->left);

  gen_printf("@EMIT_ENUMS");
  if (name_list) {
    gen_printf(" ");
    gen_name_list(name_list);
  }
}

static void gen_conflict_target(ast_node *ast) {
  Contract(is_ast_conflict_target(ast));
  EXTRACT(indexed_columns, ast->left);
  EXTRACT(opt_where, ast->right);

  gen_printf("\nON CONFLICT ");
  if (indexed_columns) {
    gen_printf("(");
    gen_indexed_columns(indexed_columns);
    gen_printf(") ");
  }
  if (opt_where) {
    gen_opt_where(opt_where);
    gen_printf(" ");
  }
}

static void gen_upsert_update(ast_node *ast) {
  Contract(is_ast_upsert_update(ast));
  EXTRACT_NOTNULL(conflict_target, ast->left);
  EXTRACT(update_stmt, ast->right);

  gen_conflict_target(conflict_target);
  gen_printf("DO ");
  if (update_stmt) {
    gen_update_stmt(update_stmt);
  } else {
    gen_printf("NOTHING");
  }
}

static void gen_upsert_stmt(ast_node *ast) {
  Contract(is_ast_upsert_stmt(ast));

  EXTRACT_NOTNULL(insert_stmt, ast->left);
  EXTRACT_NOTNULL(upsert_update, ast->right);

  gen_insert_stmt(insert_stmt);
  gen_upsert_update(upsert_update);
}

static void gen_with_upsert_stmt(ast_node *ast) {
  Contract(is_ast_with_upsert_stmt(ast));
  EXTRACT_ANY_NOTNULL(with_prefix, ast->left)
  EXTRACT_NOTNULL(upsert_stmt, ast->right);

  gen_with_prefix(with_prefix);
  gen_upsert_stmt(upsert_stmt);
}

static void gen_explain_stmt(ast_node *ast) {
  Contract(is_ast_explain_stmt(ast));
  EXTRACT_OPTION(query_plan, ast->left);
  EXTRACT_ANY_NOTNULL(stmt_target, ast->right);

  gen_printf("EXPLAIN");
  if (query_plan == EXPLAIN_QUERY_PLAN) {
    gen_printf(" QUERY PLAN");
  }
  gen_printf("\n");
  gen_one_stmt(stmt_target);
}

cql_data_defn( int32_t gen_stmt_level );

static void gen_stmt_list(ast_node *root) {
  if (!root) {
    return;
  }

  gen_stmt_level++;

  int32_t indent_level = (gen_stmt_level > 1) ? 2 : 0;

  BEGIN_INDENT(statement, indent_level);

  for (ast_node *semi = root; semi; semi = semi->right) {
    if (gen_stmt_level == 1 && semi != root) {
      gen_printf("\n");
    }
    EXTRACT_STMT_AND_MISC_ATTRS(stmt, misc_attrs, semi);
    if (misc_attrs) {
      gen_misc_attrs(misc_attrs);
    }
    gen_one_stmt(stmt);
    gen_printf(";\n");
  }

  END_INDENT(statement);
  gen_stmt_level--;
}

cql_noexport void gen_one_stmt(ast_node *stmt)  {
  symtab_entry *entry = symtab_find(gen_stmts, stmt->type);

  // These are all the statements there are, we have to find it in this table
  // or else someone added a new statement and it isn't supported yet.
  Invariant(entry);
  ((void (*)(ast_node*))entry->val)(stmt);
}

// so the name doesn't otherwise conflict in the amalgam
#undef output

#undef STMT_INIT
#define STMT_INIT(x) symtab_add(gen_stmts, k_ast_ ## x, (void *)gen_ ## x)

#undef EXPR_INIT
#define EXPR_INIT(x, func, str, pri_new) \
  static gen_expr_dispatch expr_disp_ ## x = { func, str, pri_new }; \
  symtab_add(gen_exprs, k_ast_ ## x, (void *)&expr_disp_ ## x);

cql_noexport void gen_init() {
  gen_stmts = symtab_new();
  gen_exprs = symtab_new();

  STMT_INIT(if_stmt);
  STMT_INIT(while_stmt);
  STMT_INIT(leave_stmt);
  STMT_INIT(continue_stmt);
  STMT_INIT(return_stmt);
  STMT_INIT(rollback_return_stmt);
  STMT_INIT(commit_return_stmt);
  STMT_INIT(call_stmt);
  STMT_INIT(declare_vars_type);
  STMT_INIT(assign);
  STMT_INIT(set_from_cursor);
  STMT_INIT(create_proc_stmt);
  STMT_INIT(trycatch_stmt);
  STMT_INIT(throw_stmt);
  STMT_INIT(create_trigger_stmt);
  STMT_INIT(create_table_stmt);
  STMT_INIT(create_virtual_table_stmt);
  STMT_INIT(drop_table_stmt);
  STMT_INIT(drop_view_stmt);
  STMT_INIT(drop_index_stmt);
  STMT_INIT(drop_trigger_stmt);
  STMT_INIT(alter_table_add_column_stmt);
  STMT_INIT(create_index_stmt);
  STMT_INIT(create_view_stmt);
  STMT_INIT(select_stmt);
  STMT_INIT(with_select_stmt);
  STMT_INIT(delete_stmt);
  STMT_INIT(with_delete_stmt);
  STMT_INIT(update_stmt);
  STMT_INIT(update_cursor_stmt);
  STMT_INIT(with_update_stmt);
  STMT_INIT(insert_stmt);
  STMT_INIT(with_insert_stmt);
  STMT_INIT(upsert_stmt);
  STMT_INIT(with_upsert_stmt);
  STMT_INIT(upsert_update);
  STMT_INIT(conflict_target);
  STMT_INIT(fetch_values_stmt);
  STMT_INIT(declare_enum_stmt);
  STMT_INIT(declare_cursor);
  STMT_INIT(declare_cursor_like_name);
  STMT_INIT(declare_cursor_like_select);
  STMT_INIT(declare_named_type);
  STMT_INIT(declare_value_cursor);
  STMT_INIT(declare_proc_stmt);
  STMT_INIT(declare_func_stmt);
  STMT_INIT(declare_select_func_stmt);
  STMT_INIT(loop_stmt);
  STMT_INIT(fetch_stmt);
  STMT_INIT(fetch_call_stmt);
  STMT_INIT(open_stmt);
  STMT_INIT(begin_trans_stmt);
  STMT_INIT(commit_trans_stmt);
  STMT_INIT(rollback_trans_stmt);
  STMT_INIT(proc_savepoint_stmt);
  STMT_INIT(savepoint_stmt);
  STMT_INIT(release_savepoint_stmt);
  STMT_INIT(close_stmt);
  STMT_INIT(out_stmt);
  STMT_INIT(out_union_stmt);
  STMT_INIT(echo_stmt);
  STMT_INIT(schema_upgrade_version_stmt);
  STMT_INIT(schema_upgrade_script_stmt);
  STMT_INIT(previous_schema_stmt);
  STMT_INIT(enforce_strict_stmt);
  STMT_INIT(enforce_normal_stmt);
  STMT_INIT(enforce_push_stmt);
  STMT_INIT(enforce_pop_stmt);
  STMT_INIT(declare_schema_region_stmt);
  STMT_INIT(declare_deployable_region_stmt);
  STMT_INIT(begin_schema_region_stmt);
  STMT_INIT(end_schema_region_stmt);
  STMT_INIT(schema_ad_hoc_migration_stmt);
  STMT_INIT(explain_stmt);
  STMT_INIT(emit_enums_stmt);

  EXPR_INIT(num, gen_expr_num, "NUM", EXPR_PRI_ROOT);
  EXPR_INIT(str, gen_expr_str, "STR", EXPR_PRI_ROOT);
  EXPR_INIT(blob, gen_expr_blob, "BLB", EXPR_PRI_ROOT);
  EXPR_INIT(null, gen_expr_null, "NULL", EXPR_PRI_ROOT);
  EXPR_INIT(dot, gen_expr_dot, "DOT", EXPR_PRI_ROOT);
  EXPR_INIT(const, gen_expr_const, "CONST", EXPR_PRI_ROOT);
  EXPR_INIT(bin_and, gen_binary, "&", EXPR_PRI_BINARY);
  EXPR_INIT(bin_or, gen_binary, "|", EXPR_PRI_BINARY);
  EXPR_INIT(lshift, gen_binary, "<<", EXPR_PRI_BINARY);
  EXPR_INIT(rshift, gen_binary, ">>", EXPR_PRI_BINARY);
  EXPR_INIT(mul, gen_binary, "*", EXPR_PRI_MUL);
  EXPR_INIT(div, gen_binary, "/", EXPR_PRI_MUL);
  EXPR_INIT(mod, gen_binary, "%", EXPR_PRI_MUL);
  EXPR_INIT(add, gen_binary, "+", EXPR_PRI_ADD);
  EXPR_INIT(sub, gen_binary, "-", EXPR_PRI_ADD);
  EXPR_INIT(not, gen_unary, "NOT ", EXPR_PRI_NOT);
  EXPR_INIT(tilde, gen_unary, "~", EXPR_PRI_TILDE);
  EXPR_INIT(collate, gen_binary, "COLLATE", EXPR_PRI_TILDE);
  EXPR_INIT(uminus, gen_uminus, "-", EXPR_PRI_TILDE);
  EXPR_INIT(eq, gen_binary, "=", EXPR_PRI_EQUALITY);
  EXPR_INIT(lt, gen_binary, "<", EXPR_PRI_INEQUALITY);
  EXPR_INIT(gt, gen_binary, ">", EXPR_PRI_INEQUALITY);
  EXPR_INIT(ne, gen_binary, "<>", EXPR_PRI_INEQUALITY);
  EXPR_INIT(ge, gen_binary, ">=", EXPR_PRI_INEQUALITY);
  EXPR_INIT(le, gen_binary, "<=", EXPR_PRI_INEQUALITY);
  EXPR_INIT(call, gen_expr_call, "CALL", EXPR_PRI_ROOT);
  EXPR_INIT(window_func_inv, gen_expr_window_func_inv, "WINDOW-FUNC-INV", EXPR_PRI_ROOT);
  EXPR_INIT(raise, gen_expr_raise, "RAISE", EXPR_PRI_ROOT);
  EXPR_INIT(between, gen_expr_between, "BETWEEN", EXPR_PRI_BETWEEN);
  EXPR_INIT(not_between, gen_expr_not_between, "NOT BETWEEN", EXPR_PRI_BETWEEN);
  EXPR_INIT(and, gen_binary, "AND", EXPR_PRI_AND);
  EXPR_INIT(between_rewrite, gen_expr_between_rewrite, "BETWEEN", EXPR_PRI_BETWEEN);
  EXPR_INIT(or, gen_binary, "OR", EXPR_PRI_OR);
  EXPR_INIT(select_stmt, gen_expr_select, "SELECT", EXPR_PRI_ROOT);
  EXPR_INIT(with_select_stmt, gen_expr_select, "WITH...SELECT", EXPR_PRI_ROOT);
  EXPR_INIT(is, gen_binary, "IS", EXPR_PRI_EQUALITY);
  EXPR_INIT(is_not, gen_binary, "IS NOT", EXPR_PRI_EQUALITY);
  EXPR_INIT(like, gen_binary, "LIKE", EXPR_PRI_EQUALITY);
  EXPR_INIT(not_like, gen_binary, "NOT LIKE", EXPR_PRI_EQUALITY);
  EXPR_INIT(match, gen_binary, "MATCH", EXPR_PRI_EQUALITY);
  EXPR_INIT(regexp, gen_binary, "REGEXP", EXPR_PRI_EQUALITY);
  EXPR_INIT(glob, gen_binary, "GLOB", EXPR_PRI_EQUALITY);
  EXPR_INIT(in_pred, gen_expr_in_pred, "IN", EXPR_PRI_EQUALITY);
  EXPR_INIT(not_in, gen_expr_not_in, "NOT IN", EXPR_PRI_EQUALITY);
  EXPR_INIT(case_expr, gen_expr_case, "CASE", EXPR_PRI_ROOT);
  EXPR_INIT(exists_expr, gen_expr_exists, "EXISTS", EXPR_PRI_ROOT);
  EXPR_INIT(cast_expr, gen_expr_cast, "CAST", EXPR_PRI_ROOT);
  EXPR_INIT(concat, gen_concat, "||", EXPR_PRI_CONCAT);
}

cql_export void gen_cleanup() {
  SYMTAB_CLEANUP(gen_stmts);
  SYMTAB_CLEANUP(gen_exprs);
  gen_output = NULL;
  gen_callbacks = NULL;
  used_alias_syms = NULL;
}

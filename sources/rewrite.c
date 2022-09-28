/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if defined(CQL_AMALGAM_LEAN) && !defined(CQL_AMALGAM_SEM)

// stubs to avoid link errors (none needed)

#else

// Most of the functions that rewrite the AST have been hoisted out of sem.c and are here
// Rewrites always happen during semantic analysis so this is really part of that phase.

#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include "cg_common.h"
#include "compat.h"
#include "cql.h"
#include "ast.h"
#include "cql.y.h"
#include "sem.h"
#include "charbuf.h"
#include "bytebuf.h"
#include "list.h"
#include "gen_sql.h"
#include "symtab.h"
#include "eval.h"
#include "rewrite.h"
#include "printf.h"

static ast_node* rewrite_gen_arg_list(charbuf* format_buf, CSTR cusor_name, CSTR col_name, sem_t type);
static ast_node* rewrite_gen_printf_call(CSTR format, ast_node *arg_list);
static ast_node *rewrite_gen_iif_case_expr(ast_node *expr, ast_node *val1, ast_node *val2);
static ast_node *rewrite_gen_case_expr(ast_node *var1, ast_node *var2, bool_t report_column_name);
static bool_t rewrite_one_def(ast_node *head);
static void rewrite_one_typed_name(ast_node *typed_name, symtab *used_names);
static void rewrite_from_shape_args(ast_node *head);

// @PROC can be used in place of an ID in various places
// replace that name if appropriate
cql_noexport void rewrite_proclit(ast_node *ast) {
  Contract(is_ast_str(ast));
  EXTRACT_STRING(name, ast);
  CSTR newname = process_proclit(ast, name);
  if (newname) {
    ((str_ast_node*)ast)->value = newname;
  }
}

// To do this rewrite we only need to check a few things:
//  * is the given name really a shape
//  * does the shape have storage (i.e. SEM_TYPE_HAS_SHAPE_STORAGE is set)
//  * were enough fields specified?
//  * were any fields requested?  [e.g. FETCH C() FROM CURSOR is meaningless]
//
// If the above conditions are met then we're basically good to go. For each column specified
// e.g. FETCH C(a,b) has two; we will take the next shape columns and add it an automatically
// created values list.  At the end the AST will be transformed into
//   FETCH C(a,b, etc.) FROM VALUES(D.col1, D.col2, etc.)
// and it can then be type checked as usual.
//
cql_noexport void rewrite_insert_list_from_shape(ast_node *ast, ast_node *from_shape, uint32_t count) {
  Contract(is_ast_columns_values(ast));
  Contract(is_ast_from_shape(from_shape));
  Contract(count > 0);
  EXTRACT_ANY_NOTNULL(shape, from_shape->right);

  // from_shape must have the columns
  if (!(shape->sem->sem_type & SEM_TYPE_HAS_SHAPE_STORAGE)) {
    report_error(shape, "CQL0298: cannot read from a cursor without fields", shape->sem->name);
    record_error(shape);
    record_error(ast);
    return;
  }

  EXTRACT_ANY_NOTNULL(column_spec, from_shape->left);
  EXTRACT_ANY(name_list, column_spec->left);

  uint32_t provided_count = 0;
  for (ast_node *item = name_list; item; item = item->right) {
    provided_count++;
  }

  if (provided_count < count) {
    report_error(ast, "CQL0299: [shape] has too few fields", shape->sem->name);
    record_error(ast);
    return;
  }

  AST_REWRITE_INFO_SET(shape->lineno, shape->filename);

  ast_node *insert_list = NULL;
  ast_node *insert_list_tail = NULL;

  ast_node *item = name_list;

  for (int32_t i = 0; i < count; i++, item = item->right) {
    EXTRACT_STRING(item_name, item->left);
    ast_node *cname = new_ast_str(shape->sem->name);
    ast_node *col = new_ast_str(item_name);
    ast_node *dot = new_ast_dot(cname, col);

    // add name to the name list
    ast_node *new_tail = new_ast_insert_list(dot, NULL);

    if (insert_list) {
      ast_set_right(insert_list_tail, new_tail);
    }
    else {
      insert_list = new_tail;
    }

    insert_list_tail = new_tail;
  }

  AST_REWRITE_INFO_RESET();

  // the tree is rewritten, semantic analysis can proceed
  ast_set_right(ast, insert_list);

  // temporarily mark the ast ok, there is more checking to do
  record_ok(ast);
}

// The form "LIKE x" can appear in most name lists instead of a list of names
// the idea here is that if you want to use the columns of a shape
// for the data you don't want to specify the columns manually, you'd like
// to get them from the type information.  So for instance:
// INSERT INTO T(like C) values(C.x, C.y) is better than
// INSERT INTO T(x,y) values(C.x, C.y), but better still
// INSERT INTO T(like C) from C;
//
// This is sugar, so the code gen system never sees the like form.
// The rewrite is semantically checked as usual so you get normal errors
// if the column types are not compatible.
//
// There are good helpers for creating the name list and for finding
// the likeable object.  So we just use those for all the heavy lifting.
cql_noexport void rewrite_like_column_spec_if_needed(ast_node *columns_values) {
  Contract(is_ast_columns_values(columns_values) || is_ast_from_shape(columns_values));
  EXTRACT_NOTNULL(column_spec, columns_values->left);
  EXTRACT_ANY(shape_def, column_spec->left);

  if (is_ast_shape_def(shape_def)) {
     ast_node *found_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
     if (!found_shape) {
       record_error(columns_values);
       return;
     }

     AST_REWRITE_INFO_SET(shape_def->lineno, shape_def->filename);

     sem_struct *sptr = found_shape->sem->sptr;
     ast_node *name_list = rewrite_gen_full_column_list(sptr);
     ast_set_left(column_spec, name_list);

     AST_REWRITE_INFO_RESET();
  }

  record_ok(columns_values);
}

// FROM [shape] is a sugar feature, this is the place where we trigger rewriting of the AST
// to replace FROM [shape] with normal values from the shape
//  * Note: By this point column_spec has already  been rewritten so that it is for sure not
//    null if it was absent.  It will be an empty name list.
// All we're doing here is setting up the call to the worker using the appropriate AST args
cql_noexport void rewrite_from_shape_if_needed(ast_node *ast_stmt, ast_node *columns_values)
{
  Contract(ast_stmt); // we can record the error on any statement
  Contract(is_ast_columns_values(columns_values));
  EXTRACT_NOTNULL(column_spec, columns_values->left);

  if (!is_ast_from_shape(columns_values->right)) {
    record_ok(ast_stmt);
    return;
  }

  uint32_t count = 0;
  for (ast_node *item = column_spec->left; item; item = item->right) {
    count++;
  }

  if (count == 0) {
    report_error(columns_values->right, "CQL0297: FROM [shape] is redundant if column list is empty", NULL);
    record_error(ast_stmt);
    return;
  }

  EXTRACT_NOTNULL(from_shape, columns_values->right);
  EXTRACT_ANY_NOTNULL(shape, from_shape->right);

  sem_any_shape(shape);
  if (is_error(shape)) {
    record_error(ast_stmt);
    return;
  }

  // Now we're going to go a bit meta, the from [shape] clause itself has a column
  // list we might need to rewrite THAT column list before we can proceed.
  // The from [shape] column list could be empty
  sem_struct *sptr = shape->sem->sptr;
  rewrite_empty_column_list(from_shape, sptr);

  rewrite_like_column_spec_if_needed(from_shape);
  if (is_error(from_shape)) {
    record_error(ast_stmt);
    return;
  }

  rewrite_insert_list_from_shape(columns_values, from_shape, count);
  if (is_error(columns_values)) {
    record_error(ast_stmt);
    return;
  }

  // temporarily mark the ast ok, there is more checking to do
  // record_ok(ast_stmt);
  record_ok(ast_stmt);
}

// Here we will rewrite the arguments in a call statement expanding any
// FROM [shape] [LIKE type ] entries we encounter.  We don't validate
// the types here.  That happens after expansion.  It's possible that the
// types don't match at all, but we don't care yet.
static void rewrite_from_shape_args(ast_node *head) {
  Contract(is_ast_expr_list(head) || is_ast_arg_list(head) || is_ast_insert_list(head));

  // We might need to make arg_list nodes, insert_list nodes, or expr_list nodes, they are the
  // same really so we'll change the node type to what we need.  We just stash what
  // the first item was and make any that we create the same as this one.
  CSTR node_type = head->type;

  for (ast_node *item = head ; item ; item = item->right) {
    EXTRACT_ANY_NOTNULL(arg, item->left);
    if (is_ast_from_shape(arg)) {
      EXTRACT_ANY_NOTNULL(shape, arg->left);

      // Note if this shape has no storage (e.g. non automatic cursor) then we will fail later
      // when we try to resolve the '.' expression.  That error message tells the story well enough
      // so we don't need an extra check here.
      sem_any_shape(shape);
      if (is_error(shape)) {
        record_error(head);
        return;
      }

      ast_node *shape_def = arg->right;
      ast_node *likeable_shape = NULL;

      if (shape_def) {
          likeable_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
          if (!likeable_shape) {
            record_error(head);
            return;
          }
      }

      AST_REWRITE_INFO_SET(shape->lineno, shape->filename);

      // use the names from the LIKE clause if there is one, otherwise use
      // all the names in the shape.
      sem_struct *sptr = likeable_shape ? likeable_shape->sem->sptr : shape->sem->sptr;
      uint32_t count = sptr->count;

      for (uint32_t i = 0; i < count; i++) {
        ast_node *cname = new_ast_str(shape->sem->name);
        ast_node *col = new_ast_str(sptr->names[i]);
        ast_node *dot = new_ast_dot(cname, col);

        if (i == 0) {
          // the first item just replaces the FROM cursor node
          ast_set_left(item, dot);
        }
        else {
          // subsequent items are threaded after our current position
          // we leave arg_list pointed to the end of what we inserted
          ast_node *right = item->right;
          ast_node *new_item = new_ast_expr_list(dot, right);
          new_item->type = node_type;
          ast_set_right(item, new_item);
          item = new_item;
        }
      }

      AST_REWRITE_INFO_RESET();
    }
  }

  // at least provisionally ok
  record_ok(head);
}

// Walk the list of column definitions looking for any of the
// "LIKE table/proc/view". If any are found, replace that parameter with
// the table/prov/view columns
cql_noexport bool_t rewrite_col_key_list(ast_node *head) {
  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_col_key_list(ast));

    if (is_ast_shape_def(ast->left)) {
      bool_t success = rewrite_one_def(ast);
      if (!success) {
        return false;
      }
    }
  }

  return true;
}

// There is a LIKE [table/view/proc] used to create a table so we
// - Look up the parameters to the table/view/proc
// - Create a col_def node for each field of the table/view/proc
// - Reconstruct the ast
cql_noexport bool_t rewrite_one_def(ast_node *head) {
  Contract(is_ast_col_key_list(head));
  EXTRACT_NOTNULL(shape_def, head->left);

  // it's ok to use the LIKE construct on old tables
  ast_node *likeable_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
  if (!likeable_shape) {
    record_error(head);
    return false;
  }

  AST_REWRITE_INFO_SET(shape_def->lineno, shape_def->filename);

  // Store the remaining nodes while we reconstruct the AST
  EXTRACT_ANY(right_ast, head->right);

  sem_struct *sptr = likeable_shape->sem->sptr;
  uint32_t count = sptr->count;

  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR col_name = sptr->names[i];

    // Construct a col_def using name and core semantic type
    ast_node *data_type = rewrite_gen_data_type(core_type_of(sem_type), NULL);
    ast_node *name_ast = new_ast_str(col_name);
    ast_node *name_type = new_ast_col_def_name_type(name_ast, data_type);

    // In the case of columns the ast has col attributes to represent
    // not null and sensitive so we add those after we've already
    // added the basic data type above
    ast_node *attrs = NULL;
    if (is_not_nullable(sem_type)) {
      attrs = new_ast_col_attrs_not_null(NULL, NULL);
    }

    if (sensitive_flag(sem_type)) {
      // link it in, in case not null was also in play
      attrs = new_ast_sensitive_attr(NULL, attrs);
    }

    ast_node *col_def_type_attrs = new_ast_col_def_type_attrs(name_type, attrs);
    ast_node *col_def = new_ast_col_def(col_def_type_attrs, NULL);

    if (i) {
      ast_node *new_head = new_ast_col_key_list(col_def, NULL);
      ast_set_right(head, new_head);
      head = new_head;
    } else {
      Invariant(is_ast_col_key_list(head));
      Invariant(is_ast_shape_def(head->left));

      // replace the shape def entry with a col_def
      // on the next iteration, we will insert to the right of ast
      ast_set_right(head, NULL);
      ast_set_left(head, col_def);
    }
  }

  AST_REWRITE_INFO_RESET();

  // Put the stored columns at the 'tail' of the linked list
  ast_set_right(head, right_ast);
  return true;
}

// Give the best name for the shape type given then AST
// there are many casese, the best data is on the struct type unless
// it's anonymous, in which case the item name is the best choice.
CSTR static best_shape_type_name(ast_node *shape) {
  Contract(shape->sem);
  Contract(shape->sem->sptr);

  CSTR struct_name = shape->sem->sptr->struct_name;
  CSTR obj_name = shape->sem->name;

  // "select" is the generic name used for structs that are otherwise unnamed.
  // e.g.  "declare C cursor like select 1 x, 2 y"
  if (struct_name && strcmp("select", struct_name)) {
    return struct_name;
  }
  else {
    // use "select" only as a last recourse, it means some anonymous shape
    return obj_name ? obj_name : "select";
  }
}

// Here we have found a "like T" name that needs to be rewritten with
// the various columns of T.  We do this by:
// * looking up "T" (this is the only thing that can go wrong)
// * replace the "like T" slug with a param node for the first column of T
// * for each additional column create a param node and link it in.
// * emit any given name only once, (so you can do like T1, like T1 even if both have the same pk)
// * arg names get a _ suffix so they don't conflict with column names
static ast_node *rewrite_one_param(ast_node *param, symtab *param_names, bytebuf *args_info) {
  Contract(is_ast_param(param));
  EXTRACT_NOTNULL(param_detail, param->right);
  EXTRACT_ANY(shape_name_ast, param_detail->left);
  EXTRACT_NOTNULL(shape_def, param_detail->right);

  ast_node *likeable_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_ARGS);
  if (!likeable_shape) {
    record_error(param);
    return param;
  }

  AST_REWRITE_INFO_SET(shape_def->lineno, shape_def->filename);

  // Nothing can go wrong from here on
  record_ok(param);

  sem_struct *sptr = likeable_shape->sem->sptr;
  uint32_t count = sptr->count;
  bool_t first_rewrite = true;
  CSTR shape_name = "";
  CSTR shape_type = best_shape_type_name(likeable_shape);

  if (shape_name_ast) {
    EXTRACT_STRING(sname, shape_name_ast);
    shape_name = sname;
    ast_node *shape_ast = new_ast_str(shape_name);
    shape_ast->sem = likeable_shape->sem;
    sem_add_flags(shape_ast, SEM_TYPE_HAS_SHAPE_STORAGE); // the arg bundle has storage!
    shape_ast->sem->name = shape_name;
    add_arg_bundle(shape_ast, shape_name);
  }

  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR param_name = sptr->names[i];
    CSTR param_kind = sptr->kinds[i];
    CSTR original_name = param_name;

    if (shape_name[0]) {
      // the orignal name in this form has to be compound to disambiguate
      param_name = dup_printf("%s_%s", shape_name, param_name);

      // note we skip none of these, if the names conflict that is an error:
      // e.g. if you make an arg like x_y and you then have a shape named x
      //      with a field y you'll get an error
      symtab_add(param_names, param_name, NULL);
    }
    else {
      // If the shape came from a procedure we keep the args unchanged
      // If the shape came from a data type or cursor then we add _
      // The idea here is that if it came from a procedure we want to keep the same signature
      // exactly and if any _ needed to be added to avoid conflict with a column name then it already was.

      if (!(sem_type & (SEM_TYPE_IN_PARAMETER | SEM_TYPE_OUT_PARAMETER))) {
        param_name = dup_printf("%s_", param_name);
      }

      // skip any that we have already added or that are manually present
      if (!symtab_add(param_names, param_name, NULL)) {
        continue;
      }
    }

    if (args_info) {
      // args info uses the cleanest version of the name, no trailing _
      bytebuf_append_var(args_info, original_name);
      bytebuf_append_var(args_info, shape_name);
      bytebuf_append_var(args_info, shape_type);
    }

    ast_node *type = rewrite_gen_data_type(sem_type, param_kind);
    ast_node *name_ast = new_ast_str(param_name);
    ast_node *param_detail_new = new_ast_param_detail(name_ast, type);

    ast_node *inout = NULL; // IN by default
    if (sem_type & SEM_TYPE_OUT_PARAMETER) {
      if (sem_type & SEM_TYPE_IN_PARAMETER) {
        inout = new_ast_inout();
      }
      else {
        inout = new_ast_out();
      }
    }

    if (!first_rewrite) {
      // for the 2nd and subsequent args make a new node
      ast_node *params = param->parent;
      ast_node *new_param = new_ast_param(inout, param_detail_new);
      ast_set_right(params, new_ast_params(new_param, params->right));
      param = new_param;
    }
    else {
      // for the first arg, just replace the param details
      // recall that we are on a param node and it is the like entry
      Invariant(is_ast_param(param));

      // replace the like entry with a real param detail
      // on the next iteration, we will insert to the right of ast
      ast_set_right(param, param_detail_new);
      ast_set_left(param, inout);
      first_rewrite = false;
    }
    record_ok(param);
  }

  // There's a chance we did nothing.  If that happens we still have to remove the like node.
  // If we did anything the like node is already gone.
  if (first_rewrite) {
    // since this can only happen if there is 100% duplication, that means there is always a previous parameter
    // if this were the first node we would have expanded ... something
    EXTRACT_NOTNULL(params, param->parent);
    EXTRACT_NAMED_NOTNULL(tail, params, params->parent);
    ast_set_right(tail, params->right);
  }

  AST_REWRITE_INFO_RESET();

  // this is the last param that we modified
  return param;
}

// The name @proc refers to the current procedure name, this can appear in various
// contexts either as a literal string or a valid id.  If it matches replace it here
cql_noexport CSTR process_proclit(ast_node *ast, CSTR name) {
  if (!Strcasecmp(name, "@proc")) {
    if (!current_proc) {
       report_error(ast, "CQL0252: @PROC literal can only appear inside of procedures", NULL);
       record_error(ast);
       return NULL;
    }

    ast_node *name_ast = get_proc_name(current_proc);
    EXTRACT_STRING(proc_name, name_ast);
    name = proc_name;
  }

  record_ok(ast);
  return name;
}

cql_noexport ast_node *rewrite_gen_data_type(sem_t sem_type, CSTR kind) {
  ast_node *ast = NULL;
  ast_node *kind_ast = kind ? new_ast_str(kind) : NULL;

  switch (core_type_of(sem_type)) {
    case SEM_TYPE_INTEGER:      ast = new_ast_type_int(kind_ast); break;
    case SEM_TYPE_TEXT:         ast = new_ast_type_text(kind_ast); break;
    case SEM_TYPE_LONG_INTEGER: ast = new_ast_type_long(kind_ast); break;
    case SEM_TYPE_REAL:         ast = new_ast_type_real(kind_ast); break;
    case SEM_TYPE_BOOL:         ast = new_ast_type_bool(kind_ast); break;
    case SEM_TYPE_BLOB:         ast = new_ast_type_blob(kind_ast); break;
    case SEM_TYPE_OBJECT:       ast = new_ast_type_object(kind_ast); break;
  }

  Invariant(ast);

  if (is_not_nullable(sem_type)) {
    ast = new_ast_notnull(ast);
  }

  if (sensitive_flag(sem_type)) {
    ast = new_ast_sensitive_attr(ast, NULL);
  }

  return ast;
}

// If no name list then fake a name list so that both paths are the same
// no name list is the same as all the names
cql_noexport ast_node *rewrite_gen_full_column_list(sem_struct *sptr) {
  Contract(sptr);
  ast_node *name_list = NULL;
  ast_node *name_list_tail = NULL;

  for (int32_t i = 0; i < sptr->count; i++) {
    if (sptr->semtypes[i] & SEM_TYPE_HIDDEN_COL) {
      continue;
    }

    ast_node *ast_col = new_ast_str(sptr->names[i]);

    // add name to the name list
    ast_node *new_tail = new_ast_name_list(ast_col, NULL);
    if (name_list) {
      ast_set_right(name_list_tail, new_tail);
    }
    else {
      name_list = new_tail;
    }

    name_list_tail = new_tail;
  }

  return  name_list;
}

// This helper function rewrites the expr_names ast to the columns_values ast.
// e.g: fetch C using 1 a, 2 b, 3 c; ==> fetch C (a,b,c) values (1, 2, 3);
cql_noexport void rewrite_expr_names_to_columns_values(ast_node* columns_values) {
  Contract(is_ast_expr_names(columns_values));

  AST_REWRITE_INFO_SET(columns_values->lineno, columns_values->filename);

  EXTRACT(expr_names, columns_values);
  ast_node *name_list = NULL;
  ast_node *insert_list = NULL;

  for ( ; expr_names->right ; expr_names = expr_names->right) ;

  do {
    EXTRACT(expr_name, expr_names->left);
    EXTRACT_ANY(expr, expr_name->left);
    EXTRACT_ANY(as_alias, expr_name->right);
    EXTRACT_ANY_NOTNULL(name, as_alias->left);

    name_list = new_ast_name_list(name, name_list);
    insert_list = new_ast_insert_list(expr, insert_list);

    expr_names = expr_names->parent;
  } while (is_ast_expr_names(expr_names));

  ast_node *opt_column_spec = new_ast_column_spec(name_list);
  ast_node *new_columns_values = new_ast_columns_values(opt_column_spec, insert_list);

  columns_values->type = new_columns_values->type;
  ast_set_left(columns_values, new_columns_values->left);
  ast_set_right(columns_values, new_columns_values->right);

  AST_REWRITE_INFO_RESET();
}

// This helper function rewrites the select statement ast to the columns_values ast.
// e.g: insert into X using select 1 a, 2 b, 3 c; ==> insert into X (a,b,c) values (1, 2, 3);
cql_noexport void rewrite_select_stmt_to_columns_values(ast_node* columns_values) {
  EXTRACT_ANY_NOTNULL(select_stmt, columns_values);
  Contract(is_select_stmt(select_stmt));

  AST_REWRITE_INFO_SET(columns_values->lineno, columns_values->filename);

  ast_node *name_list = NULL;

  Invariant(select_stmt->sem);
  Invariant(select_stmt->sem->sptr);

  sem_struct *sptr = select_stmt->sem->sptr;

  // doing the names in reverse order is easier to build up the list
  int32_t i = (int32_t)sptr->count;

  while (--i >= 0) {
    CSTR name = sptr->names[i];
    ast_node *name_ast = new_ast_str(name);

    name_list = new_ast_name_list(name_ast, name_list);
  }

  // we need a new select statement to push down the tree because we're mutating the current one
  ast_node *new_select_stmt = new_ast_select_stmt(select_stmt->left, select_stmt->right);
  new_select_stmt->type = select_stmt->type;

  // now make the columns values we need that holds the names we computed plus the new select node
  ast_node *opt_column_spec = new_ast_column_spec(name_list);
  ast_node *new_columns_values = new_ast_columns_values(opt_column_spec, new_select_stmt);

  // The current columns_values becomes a true columns values node taking over the content
  // of the fresh one we just made.  This used to be the select node, hence we copied it.
  columns_values->type = new_columns_values->type;
  ast_set_left(columns_values, new_columns_values->left);
  ast_set_right(columns_values, new_columns_values->right);

  AST_REWRITE_INFO_RESET();
}

// There are two reasons the columns might be missing. A form like this:
//    INSERT C FROM VALUES(...);
// or
//    INSERT C() FROM VALUES() @dummy_seed(...)
//
// The first form is shorthand for specifying that all of the columns are present.
// It will be expanded into something like FETCH C(x,y,z) FROM VALUES(....)
//
// The second form indicates that there are NO values specified at all.  This might
// be ok if all the columns have some default value.  Or if dummy data is used.
// When dummy data is present, any necessary but missing columns are provided
// using the seed variable.  The same rules apply to the FETCH statement.
//
// So these kinds of cases:
//   FETCH C FROM VALUES(...)  // all values are specified
//   FETCH C() FROM VALUES() @dummy_seed(...) -- NO values are specified, all dummy
//
// If you add FROM ARGUMENTS to this situation, the arguments take the place of the
// values. Each specified column will cause an argument to be used as a value, in
// the declared order.  The usual type checking will be done.
//
// So we have these kinds of cases:
//  FETCH C FROM ARGUMENTS  -- args are covering everything (dummy data not applicable as usual)
//  FETCH C() FROM ARGUMENTS @dummy_seed(...)  -- error, args can't possibly be used, no columns specified
//  FETCH C() FROM VALUES() @dummy_seed(...)  -- all values are dummy
//  FETCH C(x,y) FROM VALUES(1,2) @dummy_seed(...)  -- x, y from values, the rest are dummy
//  FETCH C(x,y) FROM ARGUMENTS @dummy_seed(...) -- x,y from args, the rest are dummy
//
// This is harder to explain than it is to code.
cql_noexport void rewrite_empty_column_list(ast_node *columns_values, sem_struct *sptr)
{
  Invariant(is_ast_columns_values(columns_values) || is_ast_from_shape(columns_values));
  EXTRACT(column_spec, columns_values->left);

  AST_REWRITE_INFO_SET(columns_values->lineno, columns_values->filename);

  if (!column_spec) {
    // no list was specified, always make the full list
    ast_node *name_list = rewrite_gen_full_column_list(sptr);
    column_spec = new_ast_column_spec(name_list);
    ast_set_left(columns_values, column_spec);
  }

  AST_REWRITE_INFO_RESET();
}

// We can't just return the error in the tree like we usually do because
// arg_list might be null and we're trying to do all the helper logic here.
cql_noexport bool_t rewrite_shape_forms_in_list_if_needed(ast_node *arg_list) {
  if (arg_list) {
    // if there are any cursor forms in the arg list that need to be expanded, do that here.
    rewrite_from_shape_args(arg_list);
    if (is_error(arg_list)) {
      return false;
    }
  }
  return true;
}

// rewrite call node with cql_cursor_diff_xxx(X,Y) to a case_expr statement
// e.g: C1 and C2 are two cursor variable with the same shape
// cql_cursor_diff_xxx(C1, C2); ===> CASE WHEN C1.x IS NOT C2.x THEN 'x' WHEN C1.y IS NOT C2.y THEN 'y'
cql_noexport void rewrite_cql_cursor_diff(ast_node *ast, bool_t report_column_name) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  Contract(
      !Strcasecmp("cql_cursor_diff_col", name) ||
      !Strcasecmp("cql_cursor_diff_val", name));

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);

  AST_REWRITE_INFO_SET(name_ast->lineno, name_ast->filename);

  ast_node *case_expr = rewrite_gen_case_expr(arg1, arg2, report_column_name);

  AST_REWRITE_INFO_RESET();

  // Reset the cql_cursor_diff_col function call node to a case_expr
  // node.
  ast->type = case_expr->type;
  ast_set_left(ast, case_expr->left);
  ast_set_right(ast, case_expr->right);

  // do semantic analysis of the rewrite AST to validate the rewrite
  sem_expr(ast);

  // the rewrite is not expected to have any semantic error
  Invariant(!is_error(ast));
}

// This helper function rewrites an iif ast to a case_expr ast, e.g.:
//
//   iif(X, Y, Z) => CASE WHEN X THEN Y ELSE Z END;
//
// The caller is responsible for validating that we have the three arguments
// required. In fact, we don't do any form of semantic analysis here at all:
// Unlike in other rewrite functions that call `sem_expr` to validate the
// rewrite, it's very much the case that the rewritten expression may not be
// semantically valid due to an error in the input program, so we simply let the
// caller deal with it.
cql_noexport void rewrite_iif(ast_node *ast) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);

  ast_node *arg1 = first_arg(arg_list);
  ast_node *arg2 = second_arg(arg_list);
  ast_node *arg3 = third_arg(arg_list);

  AST_REWRITE_INFO_SET(name_ast->lineno, name_ast->filename);

  ast_node *case_expr = rewrite_gen_iif_case_expr(arg1, arg2, arg3);

  AST_REWRITE_INFO_RESET();

  // Reset the call node to a case_expr node.
  ast->type = case_expr->type;
  ast_set_left(ast, case_expr->left);
  ast_set_right(ast, case_expr->right);
}

// The form we're trying to rewrite here is
// with cte(*) as (select 1 a, 2 b) select * from cte;
// The idea is that if you named all the columns in the projection of the select
// in this case "a, b" you don't want to rename all again in the cte definiton.
// That is with cte(a,b) as (select 1 a, 2 b) is redundant.
// There are many cases with dozens of names and it becomes a real problem to make sure
// the names all match and are in the right order.  This avoids all that.  Even if you
// select the columns you need in the wrong order it won't matter because you get them
// by name from the CTE anyway.  If you're using a union, the additional enforcement
// that the names match on each branch locks you in to correct columns.
// All we have to do is:
//   * make sure all the columns have a name and a reasonable type
//   * make a name list for the column names
//   * swap it in
cql_noexport void rewrite_cte_name_list_from_columns(ast_node *ast, ast_node *select_core) {
  Contract(is_ast_cte_decl(ast));
  EXTRACT_NOTNULL(star, ast->right)

  sem_verify_no_anon_no_null_columns(select_core);
  if (is_error(select_core)) {
    record_error(ast);
    return;
  }

  AST_REWRITE_INFO_SET(star->lineno, star->filename);

  sem_struct *sptr = select_core->sem->sptr;
  ast_node *name_list = rewrite_gen_full_column_list(sptr);
  ast_set_right(ast, name_list);

  AST_REWRITE_INFO_RESET();

  record_ok(ast);
}

// Here we have found a "like T" name that needs to be rewritten with
// the various columns of T.  We do this by:
// * looking up "T" (this is the only thing that can go wrong)
// * replace the "like T" slug with the first column of T
// * for each additional column create a typed name node and link it in.
// * emit any given name only once, (so you can do like T1, like T1 even if both have the same pk)
static void rewrite_one_typed_name(ast_node *typed_name, symtab *used_names) {
  Contract(is_ast_typed_name(typed_name));
  EXTRACT_ANY(shape_name_ast, typed_name->left);
  EXTRACT_NOTNULL(shape_def, typed_name->right);

  ast_node *found_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
  if (!found_shape) {
    record_error(typed_name);
    return;
  }

  AST_REWRITE_INFO_SET(shape_def->lineno, shape_def->filename);

  // Nothing can go wrong from here on
  record_ok(typed_name);

  sem_struct *sptr = found_shape->sem->sptr;
  uint32_t count = sptr->count;
  bool_t first_rewrite = true;
  CSTR shape_name = "";

  ast_node *insertion = typed_name;

  if (shape_name_ast) {
    EXTRACT_STRING(sname, shape_name_ast);
    shape_name = sname;

    // note that typed names are part of a procedure return type in a declaration
    // they don't create a proc or a proc body and so we don't add to arg_bundles,
    // indeed arg_bundles is null at this point
  }

  for (int32_t i = 0; i < count; i++) {
    sem_t sem_type = sptr->semtypes[i];
    CSTR name = sptr->names[i];
    CSTR kind = sptr->kinds[i];
    CSTR combined_name = name;

    if (shape_name[0]) {
      combined_name = dup_printf("%s_%s", shape_name, name);
    }

    // skip any that we have already added or that are manually present
    if (!symtab_add(used_names, combined_name, NULL)) {
      continue;
    }

    ast_node *name_ast = new_ast_str(combined_name);
    ast_node *type = rewrite_gen_data_type(sem_type, kind);
    ast_node *new_typed_name = new_ast_typed_name(name_ast, type);
    ast_node *typed_names = insertion->parent;

    if (!first_rewrite) {
      ast_set_right(typed_names, new_ast_typed_names(new_typed_name, typed_names->right));
    }
    else {
      ast_set_left(typed_names, new_typed_name);
      first_rewrite = false;
    }

    insertion = new_typed_name;
  }

  // There's a chance we did nothing.  If that happens we still have to remove the like node.
  // If we did anything the like node is already gone.
  if (first_rewrite) {
    // since this can only happen if there is 100% duplication, that means there is always a previous typed name
    // if this were the first node we would have expanded ... something
    EXTRACT_NOTNULL(typed_names, typed_name->parent);
    EXTRACT_NAMED_NOTNULL(tail, typed_names, typed_names->parent);
    ast_set_right(tail, typed_names->right);
  }

  AST_REWRITE_INFO_RESET();
}

// Walk the typed name list looking for any of the "like T" forms
// if any is found, replace that entry  with the table/shape columns
cql_noexport void rewrite_typed_names(ast_node *head) {
  symtab *used_names = symtab_new();

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_typed_names(ast));
    EXTRACT_NOTNULL(typed_name, ast->left);

    if (is_ast_shape_def(typed_name->right)) {
      rewrite_one_typed_name(typed_name, used_names);
      if (is_error(typed_name)) {
        record_error(head);
        goto cleanup;
      }
    }
    else {
      // Just extract the name and record that we used it -- no rewrite needed.
      EXTRACT_STRING(name, typed_name->left);
      symtab_add(used_names, name, NULL);
    }
  }
  record_ok(head);

cleanup:
  symtab_delete(used_names);
}

// Walk through the ast and grab the arg list as well as the function name.
// Create a new call node using these two and the argument passed in
// prior to the ':' symbol.
cql_noexport void rewrite_reverse_apply(ast_node *_Nonnull head) {
  Contract(is_ast_reverse_apply(head));
  EXTRACT_ANY_NOTNULL(argument, head->left);
  EXTRACT_NOTNULL(call, head->right);
  EXTRACT_ANY_NOTNULL(function_name, call->left);
  EXTRACT_NOTNULL(call_arg_list, call->right);
  // This may be NULL if the function only has one argument
  EXTRACT(arg_list, call_arg_list->right);

  AST_REWRITE_INFO_SET(head->lineno, head->filename);

  ast_node* new_arg_list = new_ast_call_arg_list(new_ast_call_filter_clause(NULL, NULL),
                                new_ast_arg_list(argument, arg_list));
  ast_node* new_call = new_ast_call(function_name, new_arg_list);

  AST_REWRITE_INFO_RESET();

  ast_set_right(head, new_call->right);
  ast_set_left(head, new_call->left);
  head->type = new_call->type;

}

// Walk the param list looking for any of the "like T" forms
// if any is found, replace that parameter with the table/shape columns
cql_noexport void rewrite_params(ast_node *head, bytebuf *args_info) {
  symtab *param_names = symtab_new();

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left)
    EXTRACT_NOTNULL(param_detail, param->right)

    if (is_ast_shape_def(param_detail->right)) {
      param = rewrite_one_param(param, param_names, args_info);
      if (is_error(param)) {
        record_error(head);
        goto cleanup;
      }
      ast = param->parent;
      Invariant(is_ast_params(ast));
    }
    else {
      // Just extract the name and record that we used it -- no rewrite needed.
      EXTRACT_STRING(param_name, param_detail->left);
      CSTR shape_type = "";
      CSTR shape_name = "";
      if (args_info) {
        bytebuf_append_var(args_info, param_name);
        bytebuf_append_var(args_info, shape_name);
        bytebuf_append_var(args_info, shape_type);
      }

      symtab_add(param_names, param_name, NULL);
    }
  }

  record_ok(head);

cleanup:
  symtab_delete(param_names);
}

static CSTR coretype_format(sem_t sem_type) {
  CSTR result = NULL;
  switch (core_type_of(sem_type)) {
    case SEM_TYPE_INTEGER:
    case SEM_TYPE_BOOL: result = "%d"; break;
    case SEM_TYPE_BLOB:
    case SEM_TYPE_LONG_INTEGER: result = "%lld"; break;
    case SEM_TYPE_REAL: result = "%f"; break;
    case SEM_TYPE_TEXT: result = "%s"; break;
  }
  Invariant(result);
  return result;
}

// Generate arg_list nodes and formatting values for a printf(...) ast
static ast_node* rewrite_gen_arg_list(charbuf* format_buf, CSTR cusor_name, CSTR col_name, sem_t type) {
  // left to arg_list node
  ast_node* dot = new_ast_dot(new_ast_str(cusor_name), new_ast_str(col_name));
  // If the argument is blob type we need to print just its size therefore we rewrite
  // ast to call cql_get_blob_size(<blob>) which return the size of the argument
  if (is_blob(type)) {
    // right to call_arg_list node
    ast_node* arg_list = new_ast_arg_list(dot, NULL);
    ast_node* call_arg_list =
        new_ast_call_arg_list(new_ast_call_filter_clause(NULL, NULL), arg_list);
    dot = new_ast_call(new_ast_str("cql_get_blob_size"), call_arg_list);
  }

  bprintf(format_buf, is_blob(type) ? "length %s blob" : "%s", coretype_format(type));
  return new_ast_arg_list(dot, NULL);
}

// Generate printf(...) function node. This is used by
// rewrite_gen_cursor_printf() to generate the rewrite for cql_cursor_format
// function.
// e.g: cusor_name = C, dot_name = x, type = text PRINTF("%s", C.x);
// e.g: cusor_name = C, dot_name = x, type = blob PRINTF("length %d blob", cql_get_blob_size(C.x));
static ast_node* rewrite_gen_printf_call(CSTR format, ast_node *arg_list) {
  CSTR copy_format = dup_printf("'%s'", format);
  // right to call_arg_list node
  ast_node* first_arg_list = new_ast_arg_list(new_ast_str(copy_format), arg_list);
  // right to call node
  ast_node* call_arg_list = new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL), first_arg_list);
  ast_node* call = new_ast_call(new_ast_str("printf"), call_arg_list);
  return call;
}

// Generates a call to nullable with `ast` as the argument.
static ast_node *rewrite_gen_nullable(ast_node *ast) {
  Contract(ast);

  return new_ast_call(
    new_ast_str("nullable"),
    new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL),
      new_ast_arg_list(ast, NULL)));
}

// This helper generates a case_expr node that check if an expression to return value or
// otherwise another value
// e.g: (expr, val1, val2) => CASE WHEN expr THEN val2 ELSE val1;
static ast_node *rewrite_gen_iif_case_expr(ast_node *expr, ast_node *val1, ast_node *val2) {
  // left case_list node
  ast_node* when = new_ast_when(expr, val1);
  // left connector node
  ast_node* case_list = new_ast_case_list(when, NULL);
  // case list with no ELSE (we get ELSE NULL by default)
  ast_node* connector = new_ast_connector(case_list, val2);
  // CASE WHEN expr THEN result form; not CASE expr WHEN val THEN result
  ast_node* case_expr = new_ast_case_expr(NULL, connector);
  return case_expr;
}

// This helper generates a 'case_expr' node from two cursors variables. This is used to rewrite
// cql_cursor_diff_col(X,Y) and cql_cursor_diff_val(X,Y) function to a case expr.
// e.g:
// cql_cursor_diff_col(C1, C2); ===> CASE WHEN C1.x IS NOT C2.x THEN 'x' WHEN C1.y IS NOT C2.y THEN 'y'
// cql_cursor_diff_val(C1, C2); ===> CASE WHEN C1.x IS NOT C2.x THEN printf('column:%s left:%s right:%s', 'y', printf('%s', C1.x), printf('%s', C2.x))
//                                        WHEN C1.y IS NOT C2.y THEN printf('column:%s left:%s right:%s', 'y', printf('%s', C1.y), printf('%s', C2.y))
static ast_node *rewrite_gen_case_expr(ast_node *var1, ast_node *var2, bool_t report_column_name) {
  Contract(is_variable(var1->sem->sem_type));
  Contract(is_variable(var2->sem->sem_type));

  CSTR c1_name = var1->sem->name;
  CSTR c2_name = var2->sem->name;
  sem_struct *sptr1 = var1->sem->sptr;
  sem_struct *sptr2 = var2->sem->sptr;

  Invariant(sptr1->count == sptr2->count);

  // We don't need to make sure both cursors have the same shape because it's has been done
  // already. Therefore we just assume both cursors have identical shape
  int32_t count = (int32_t) sptr1->count;
  ast_node *case_list = NULL;
  for (int32_t i = count - 1; i >= 0; i--) {
    Invariant(sptr1->names[i]);
    // left side of IS NOT
    ast_node *dot1 = new_ast_dot(new_ast_str(c1_name), new_ast_str(sptr1->names[i]));
    // right side of IS NOT
    ast_node *dot2 = new_ast_dot(new_ast_str(c2_name), new_ast_str(sptr2->names[i]));
    ast_node *is_not = new_ast_is_not(dot1, dot2);
    // the THEN part of WHEN THEN
    ast_node *val = NULL;
    if (report_column_name) {
      CSTR name_lit = dup_printf("'%s'", sptr1->names[i]);  // this turns into literal name
      val = new_ast_str(name_lit);
    } else {
      ast_node *arg_list = NULL;
      CHARBUF_OPEN(format_output);

      // fourth argument to call printf node: call printf(...) node
      ast_node* printf_arg_list3 = rewrite_gen_arg_list(
          &format_output, c2_name, sptr2->names[i], sptr2->semtypes[i]);
      // CALL PRINTF ast on fourth argument
      ast_node *call_printf3 = rewrite_gen_printf_call(format_output.ptr, printf_arg_list3);
      // left of is node
      ast_node *dot = new_ast_dot(new_ast_str(c2_name), new_ast_str(sptr2->names[i]));
      // We wrap the dot in a call to nullable if it is of a nonnull type so
      // that the IS NULL check will not result in a type error. Eliding the
      // check is not possible in the nonnull case because even a dot of a
      // nonnull type could, unfortunately, be null if a row was not fetched.
      if (is_not_nullable(sptr2->semtypes[i])) {
        dot = rewrite_gen_nullable(dot);
      }
      // left of WHEN expr
      ast_node* is_node = new_ast_is(dot, new_ast_null());
      // case_expr node: CASE WHEN C.x IS NULL THEN 'null' ELSE printf("%s", C.x)
      ast_node *check_call_printf3 = rewrite_gen_iif_case_expr(
        is_node,
        new_ast_str("'null'"),
        call_printf3);
      arg_list = new_ast_arg_list(check_call_printf3, NULL);
      bclear(&format_output);

      // third argument to call printf node: call print(...) node
      ast_node* printf_arg_list2 = rewrite_gen_arg_list(
          &format_output, c1_name, sptr1->names[i], sptr1->semtypes[i]);
      // CALL PRINTF ast on third argument
      ast_node *call_printf2 = rewrite_gen_printf_call(format_output.ptr, printf_arg_list2);
      // left of IS node
      dot = new_ast_dot(new_ast_str(c1_name), new_ast_str(sptr1->names[i]));
      if (is_not_nullable(sptr1->semtypes[i])) {
        dot = rewrite_gen_nullable(dot);
      }
      // left of WHEN expr
      is_node = new_ast_is(dot, new_ast_null());
      // case_expr node: CASE WHEN C.x IS NULL THEN 'null' ELSE printf("%s", C.x)
      ast_node *check_call_printf2 = rewrite_gen_iif_case_expr(
        is_node,
        new_ast_str("'null'"),
        call_printf2);
      arg_list = new_ast_arg_list(check_call_printf2, arg_list);
      bclear(&format_output);

      // second argument too call printf node: name node
      ast_node * printf_arg_list1 = new_ast_str(dup_printf("'%s'", sptr1->names[i]));
      arg_list = new_ast_arg_list(printf_arg_list1, arg_list);

      // printf call node
      CHARBUF_OPEN(tmp);
        bprintf(&tmp, "column:%%s %s:%%s %s:%%s", c1_name, c2_name);
        val = rewrite_gen_printf_call(tmp.ptr, arg_list);
      CHARBUF_CLOSE(tmp);

      CHARBUF_CLOSE(format_output);
    }
    // The WHEN node and the CASE LIST that holds it
    ast_node *when = new_ast_when(is_not, val);
    ast_node *new_case_list = new_ast_case_list(when, case_list);
    case_list = new_case_list;
  }

  // case list with no ELSE (we get ELSE NULL by default)
  ast_node *connector = new_ast_connector(case_list, NULL);
  // CASE WHEN expr THEN result form; not CASE expr WHEN val THEN result
  ast_node *case_expr = new_ast_case_expr(NULL, connector);

  return case_expr;
}

// This helper rewrites col_def_type_attrs->right nodes to include notnull and sensitive
// flag from the data type of a column in create table statement. This is only applicable
// if column data type of the column is the name of an emum type or a declared named type.
cql_noexport void rewrite_right_col_def_type_attrs_if_needed(ast_node *ast) {
  Contract(is_ast_col_def_type_attrs(ast));
  EXTRACT_NOTNULL(col_def_name_type, ast->left);
  EXTRACT_ANY_NOTNULL(data_type, col_def_name_type->right);
  EXTRACT_ANY(col_attrs, ast->right);

  if (is_ast_str(data_type)) {
    EXTRACT_STRING(name, data_type);
    ast_node *named_type = find_named_type(name);
    if (!named_type) {
      report_error(ast, "CQL0360: unknown type", name);
      record_error(ast);
      return;
    }

    AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

    sem_t found_sem_type = named_type->sem->sem_type;
    if (!is_nullable(found_sem_type)) {
      col_attrs = new_ast_col_attrs_not_null(NULL, col_attrs);
    }
    if (sensitive_flag(found_sem_type)) {
      col_attrs = new_ast_sensitive_attr(NULL, col_attrs);
    }

    ast_set_right(ast, col_attrs);

    AST_REWRITE_INFO_RESET();
  }

  record_ok(ast);
}

// Rewrite a data type represented as a string node to the
// actual type if the string name is a declared type.
cql_noexport void rewrite_data_type_if_needed(ast_node *ast) {
  ast_node *data_type = NULL;
  if (is_ast_create_data_type(ast)) {
    data_type = ast->left;
  } else {
    data_type = ast;
  }

  if (is_ast_str(data_type)) {
    EXTRACT_STRING(name, data_type);
    ast_node *named_type = find_named_type(name);
    if (!named_type) {
      report_error(ast, "CQL0360: unknown type", name);
      record_error(ast);
      return;
    }

    sem_t sem_type = named_type->sem->sem_type;

    // * The cast_expr node doesn't need attributes, it only casts to the
    //   target type.  When casting, both nullability and sensitivity are
    //   perserved. So in that case we remove the extra attributes.  They
    //   are not expected/required in the rewrite.
    //
    // * Columns are a little different; nullability and sensitivity are
    //   encoded differently in columns than in variables.
    //   So in that case we again only produce the base type here.
    //   The caller will do the rest. This work is done in
    //   rewrite_right_col_def_type_attrs_if_needed(ast_node
    bool_t only_core_type = ast->parent &&
        (is_ast_col_def_name_type(ast->parent) || is_ast_cast_expr(ast->parent));

    if (only_core_type) {
      sem_type = core_type_of(sem_type);
    }

    AST_REWRITE_INFO_SET(data_type->lineno, data_type->filename);
    ast_node *node = rewrite_gen_data_type(sem_type, named_type->sem->kind);
    AST_REWRITE_INFO_RESET();

    ast_set_left(data_type, node->left);
    ast_set_right(data_type, node->right);
    data_type->sem = node->sem;
    data_type->type = node->type;  // note this is ast type, not semantic type
  }

  record_ok(ast);
}

// Wraps an id or dot in a call to cql_inferred_notnull.
cql_noexport void rewrite_nullable_to_notnull(ast_node *_Nonnull ast) {
  Contract(is_id_or_dot(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  ast_node *id_or_dot;
  if (is_id(ast)) {
    EXTRACT_STRING(name, ast);
    id_or_dot = new_ast_str(name);
  } else {
    Invariant(is_ast_dot(ast));
    EXTRACT_NAME_AND_SCOPE(ast);
    id_or_dot = new_ast_dot(new_ast_str(scope), new_ast_str(name));
  }
  ast_node *cql_inferred_notnull = new_ast_str("cql_inferred_notnull");
  ast_node* call_arg_list = new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL),
      new_ast_arg_list(id_or_dot, NULL));
  ast->type = k_ast_call;
  ast_set_left(ast, cql_inferred_notnull);
  ast_set_right(ast, call_arg_list);

  AST_REWRITE_INFO_RESET();

  // Analyze the AST to validate the rewrite.
  sem_expr(ast);

  // The rewrite is not expected to have any semantic error.
  Invariant(!is_error(ast));
}

// Rewrites a guard statement of the form `IF expr stmt` to a regular if
// statement of the form `IF expr THEN stmt END IF`.
cql_noexport void rewrite_guard_stmt_to_if_stmt(ast_node *_Nonnull ast) {
  Contract(is_ast_guard_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  EXTRACT_ANY_NOTNULL(expr, ast->left);
  EXTRACT_ANY_NOTNULL(stmt, ast->right);

  ast->type = k_ast_if_stmt;
  ast_set_left(ast, new_ast_cond_action(expr, new_ast_stmt_list(stmt, NULL)));
  ast_set_right(ast, new_ast_if_alt(NULL, NULL));

  AST_REWRITE_INFO_RESET();

  sem_one_stmt(ast);
}

// Rewrites an already analyzed printf call such that all arguments whose core
// types do not match the format string exactly have casts inserted to make them
// do so. This allows programmers to enjoy the usual subtyping semantics of
// `sem_verify_assignment` while making sure that all types match up exactly for
// calls to `sqlite3_mprintf` in the C output.
cql_noexport void rewrite_printf_inserting_casts_as_needed(ast_node *ast, CSTR format_string) {
  Contract(is_ast_call(ast));
  Contract(!is_error(ast));
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT_NOTNULL(arg_list, call_arg_list->right);

  printf_iterator *iterator = minipool_alloc(ast_pool, (uint32_t)sizeof_printf_iterator);
  printf_iterator_init(iterator, NULL, format_string);

  ast_node *args_for_format = arg_list->right;
  for (ast_node *arg_item = args_for_format; arg_item; arg_item = arg_item->right) {
    sem_t sem_type = printf_iterator_next(iterator);
    // We know the format string cannot have an error.
    Contract(sem_type != SEM_TYPE_ERROR);
    // We know that we do not have too many arguments.
    Contract(sem_type != SEM_TYPE_OK);
    ast_node *arg = arg_item->left;
    AST_REWRITE_INFO_SET(arg->lineno, arg->filename);
    if (core_type_of(arg->sem->sem_type) == SEM_TYPE_NULL) {
      // We cannot cast NULL outside of an SQL context, so we just insert the
      // correct zero-valued literal instead, if needed.
      switch (sem_type) {
        case SEM_TYPE_INTEGER:
          ast_set_left(arg_item, new_ast_num(NUM_INT, "0"));
          break;
        case SEM_TYPE_LONG_INTEGER:
          ast_set_left(arg_item, new_ast_num(NUM_LONG, "0"));
          break;
        case SEM_TYPE_REAL:
          ast_set_left(arg_item, new_ast_num(NUM_REAL, "0.0"));
          break;
        default:
          // Reference types do not need to be casted.
          break;
      }
    }
    else if (core_type_of(arg->sem->sem_type) != sem_type) {
      Invariant(is_numeric(sem_type));
      // The format string specifies a larger type than what was provided, so
      // we must insert a cast to make the types match exactly.
      ast_node *type_ast;
      switch (sem_type) {
        case SEM_TYPE_INTEGER:
          type_ast = new_ast_type_int(NULL);
          break;
        case SEM_TYPE_LONG_INTEGER:
          type_ast = new_ast_type_long(NULL);
          break;
        default:
          Invariant(sem_type == SEM_TYPE_REAL);
          type_ast = new_ast_type_real(NULL);
          break;
      }
      ast_set_left(arg_item, new_ast_cast_expr(arg, type_ast));
    }
    AST_REWRITE_INFO_RESET();
  }

  // We know that we do not have too few arguments.
  Contract(printf_iterator_next(iterator) == SEM_TYPE_OK);

  // Validate the rewrite.
  sem_expr(ast);
}

// Just maintain head and tail whilst adding a node at the tail.
// This uses the usual convention that ->right is the "next" pointer.
static void add_tail(ast_node **head, ast_node **tail, ast_node *node) {
  if (*head) {
    (*tail)->right = node;
  }
  else {
    *head = node;
  }
  *tail = node;
}

static void append_scoped_name(ast_node **head, ast_node **tail, CSTR scope, CSTR name) {
  ast_node *expr = NULL;
  if (scope) {
    expr = new_ast_dot(new_ast_str(scope), new_ast_str(name));
  }
  else {
    expr = new_ast_str(name);
  }
  ast_node *select_expr = new_ast_select_expr(expr, NULL);
  ast_node *select_expr_list = new_ast_select_expr_list(select_expr, NULL);
  add_tail(head, tail, select_expr_list);
}

// This is our helper struct with the computed symbol tables for disambiguation
// we flow this around when we need to do the searches.
typedef struct jfind_t {
  sem_join *jptr;
  symtab *location;
  symtab *dups;
  symtab *tables;
} jfind_t;

// This just gives us easy access to the sem_struct or NULL
static sem_struct *jfind_table(jfind_t *jfind, CSTR name) {
  symtab_entry *entry = symtab_find(jfind->tables, name);
  return entry ? (sem_struct *)(entry->val) : NULL;
}

// If we need them we make these fast disambiguation tables so that
// we don't have to do a cubic algorithm re-searching every column we need
// These will tell us the disambiguated location of any given column name
// and its duplicate status as well fast access to the sem_struct for
// any scope within the jptr -- this will be the jptr of a FROM clause.
static void jfind_init(jfind_t *jfind, sem_join *jptr) {
  jfind->jptr = jptr;

  // this will map from column name to the first table that has that column
  jfind->location = symtab_new();

  // this will tell us if any given column requires disambiguation
  jfind->dups = symtab_new();

  // this will tell us the sptr index for a particular table name
  jfind->tables = symtab_new();

  // here we make the lookup maps by walking the jptr for the from clause
  // this will save us a lot of searching later...
  for (int32_t i = 0; i < jptr->count; i++) {
    CSTR name = jptr->names[i];
    sem_struct *sptr = jptr->tables[i];
    symtab_add(jfind->tables, name, (void *)sptr);

    for (int32_t j = 0; j < sptr->count; j++) {
      CSTR col = sptr->names[j];

      if (!symtab_add(jfind->location, col, (void*)name)) {
        symtab_add(jfind->dups, col, NULL);
      }
    }
  }
}

// cleanup the helper tables so we don't leak in the amalgam
static void jfind_cleanup(jfind_t *jfind) {
  if (jfind->location) {
    symtab_delete(jfind->location);
  }
  if (jfind->dups) {
    symtab_delete(jfind->dups);
  }
  if (jfind->tables) {
    symtab_delete(jfind->tables);
  }
}

// This will check if the indicated column of the required sptr is a type match
// for the same column name (maybe different index) in the actual column.  We
// have to do this because we want to make sure that when you say COLUMNS(X like foo)
// that the foo columns of X are the same type as those in foo.
static bool_t verify_matched_column(
  ast_node *ast,
  sem_struct *sptr_reqd,
  int32_t i_reqd,
  sem_struct *sptr_actual,
  CSTR scope)
{
  CHARBUF_OPEN(err);
  bool_t ok = false;
  CSTR col = sptr_reqd->names[i_reqd];

  // if we're emitting from the same structure there's nothing to check
  // this is not the LIKE case
  if (sptr_reqd == sptr_actual) {
    ok = true;
    goto cleanup;
  }

  // for better diagnostics, we can give the scoped name
  bprintf(&err, "%s.%s", scope, col);

  int32_t i_actual = find_col_in_sptr(sptr_actual, col);
  if (i_actual < 0) {
    report_error(ast, "CQL0069: name not found", err.ptr);
    goto cleanup;
  }

  // here the ast is only where we charge the error, but as it happens that will be the node we just added
  // which by an amazing coincidence has exactly the right file/line number for the columns node
  if (!sem_verify_assignment(ast, sptr_reqd->semtypes[i_reqd], sptr_actual->semtypes[i_actual], err.ptr)) {
    goto cleanup;
  }

  ok = true;

cleanup:
  CHARBUF_CLOSE(err);
  return ok;
}

// Here we've found one column_calculation node, this corresponds to a single
// instance of COLUMNS(...) in the select list.  When we process this, we
// will replace it with its expansion.  Note that each one is independent
// so often you really only need one (distinct is less powerful if you have two or more).
static void rewrite_column_calculation(ast_node *column_calculation, jfind_t *jfind) {
  Contract(is_ast_column_calculation(column_calculation));

  bool_t distinct = !!column_calculation->right;

  symtab *used_names = distinct ? symtab_new() : NULL;

  ast_node *tail = NULL;
  ast_node *head = NULL;

  for (ast_node *item = column_calculation->left; item; item = item->right) {
    Contract(is_ast_col_calcs(item));
    EXTRACT(col_calc, item->left);

    if (is_ast_dot(col_calc->left)) {
      // If a column is explicitly mentioned, we simply emit it
      // we won't duplicate the column later but neither will we
      // filter it out if distinct is mentioned, this is to prevent
      // bogus manual columns from staying in select lists.  If it's
      // not distinct, either hoist it to the front or else remove it.

      EXTRACT_NOTNULL(dot, col_calc->left);
      EXTRACT_STRING(left, dot->left);
      EXTRACT_STRING(right, dot->right);

      // no type check is needed here, we just emit the name whatever it is
      append_scoped_name(&head, &tail, left, right);
      if (used_names) {
        symtab_add(used_names, right, NULL);
      }
    }
    else if (col_calc->left) {
      EXTRACT_STRING(scope, col_calc->left);

      sem_struct *sptr_table = jfind_table(jfind, scope);

      if (!sptr_table) {
        report_error(col_calc->left, "CQL0069: name not found", scope);
        record_error(column_calculation);
        goto cleanup;
      }

      EXTRACT(shape_def, col_calc->right);

      sem_struct *sptr;

      if (shape_def) {
        ast_node *found_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
        if (!found_shape) {
          record_error(column_calculation);
          goto cleanup;
        }
        // get just the shape columns (or try anyway)
        sptr = found_shape->sem->sptr;
      }
      else {
        // get all the columns from this table
        sptr = sptr_table;
      }

      for (int32_t j = 0; j < sptr->count; j++) {
        CSTR col = sptr->names[j];

        if (used_names && !symtab_add(used_names, col, NULL)) {
          continue;
        }

        append_scoped_name(&head, &tail, scope, col);

        if (!verify_matched_column(tail, sptr, j, sptr_table, scope)) {
          record_error(column_calculation);
          goto cleanup;
        }
      }
    }
    else {
      // the other case has just a like expression
      EXTRACT_NOTNULL(shape_def, col_calc->right);

      ast_node *found_shape = sem_find_shape_def(shape_def, LIKEABLE_FOR_VALUES);
      if (!found_shape) {
        record_error(column_calculation);
        goto cleanup;
      }

      // get just the shape columns (or try anyway)
      sem_struct *sptr = found_shape->sem->sptr;

      // now we can use our found structure from the like
      // we will find the table that has the given column
      // we generate a disambiguation scope if it is needed
      for (int32_t i = 0; i < sptr->count; i++) {
        CSTR col = sptr->names[i];

        if (!used_names || symtab_add(used_names, col, NULL)) {
          // if the name has duplicates then qualify it

          symtab_entry *entry = symtab_find(jfind->location, col);

          if (!entry) {
            report_error(shape_def, "CQL0069: name not found", col);
            record_error(column_calculation);
            goto cleanup;
          }

          CSTR scope = (CSTR)entry->val;

          sem_struct *sptr_table = jfind_table(jfind, scope);
          Invariant(sptr_table); // this is our lookup of a scope that is known, it cant fail

          // We only use the scope in the output if it's needed and if distinct was specified
          // if distinct wasn't specified then ambiguity is an error and it will be.  The later
          // stages will check for an unambiguous name.
          CSTR used_scope = (used_names && symtab_find(jfind->dups, col)) ? scope : NULL;

          append_scoped_name(&head, &tail, used_scope, col);

          // We check the type of the first match of the name, this is the only column that
          // can match legally.  If there are other columns ambiguity errors will be emitted.
          if (!verify_matched_column(tail, sptr, i, sptr_table, scope)) {
            record_error(column_calculation);
            goto cleanup;
          }
        }
      }
    }
  }

  // replace the calc node with the head payload
  ast_node *splice = column_calculation->parent;

  ast_set_left(splice, head->left);
  ast_set_right(tail, splice->right); // this could be mutating the head
  ast_set_right(splice, head->right); // works even if head is an alias for tail

  record_ok(column_calculation);

cleanup:
  if (used_names) {
    symtab_delete(used_names);
  }
}

// At this point we're going to walk the select expression list looking for
// the construct COLUMNS(...) with its various forms.  This is a generalization
// of the T.* syntax that allows you to pull slices of the tables and to
// get distinct columns where there are duplicates due to joins.  Ultimately
// this is just sugar but the point is that there could be dozens of such columns
// and if you have to type it all yourself it is very easy to get it wrong. So
// here we're going to expand out the COLUMNS(...) operator into the actual
// tables/columns you requested.  SQLite, has no support for this sort of thing
// so it, and indeed the rest of the compilation chain, will just see the result
// of the expansion.
cql_noexport void rewrite_select_expr_list(ast_node *ast, sem_join *jptr_from) {
  Contract(is_ast_select_expr_list_con(ast));
  EXTRACT_NOTNULL(select_expr_list, ast->left);

  jfind_t jfind = {0};

  for (ast_node *item = select_expr_list; item; item = item->right) {
    Contract(is_ast_select_expr_list(item));
    if (is_ast_column_calculation(item->left)) {
      EXTRACT_NOTNULL(column_calculation, item->left);

      if (!jptr_from) {
        report_error(ast, "CQL0053: select columns(...) cannot be used with no FROM clause", NULL);
        record_error(ast);
        return;
      }

      if (!jfind.jptr) {
        jfind_init(&jfind, jptr_from);
      }

      AST_REWRITE_INFO_SET(column_calculation->lineno, column_calculation->filename);

      rewrite_column_calculation(column_calculation, &jfind);

      AST_REWRITE_INFO_RESET();

      if (is_error(column_calculation)) {
        record_error(ast);
        goto cleanup;
      }
    }
  }
  record_ok(ast);

cleanup:
  jfind_cleanup(&jfind);
}


// Here we convert one of the normal fetch_values forms
//
//   FETCH C from B       -- C is a cursor B is a blob
//   SET B from CURSOR C  -- load the blob from the cursor
//
// into the blob deserializing or deserializing forms.
// We rewrite the AST here to the blob form so that it's easier
// for the later passes to see the difference.  The parser
// can't do this because you have to know the type of the arguments
// to know that this is the blob case.
//
// The relevant statments are fetch_cursor_from_blob_stmt and
// set_blob_from_cursor_stmt.  These only have very simple forms,
// the idea is that if you need any slicing, extraction,
// or whatever, you do it with the cursors not blobs.
cql_noexport bool_t try_rewrite_blob_fetch_forms(ast_node *ast) {
  Contract(is_ast_fetch_values_stmt(ast) || is_ast_set_from_cursor(ast));

  ast_node *cursor = NULL;
  ast_node *blob = NULL;
  ast_node *dest = NULL;
  ast_node *src = NULL;

  if (is_ast_fetch_values_stmt(ast)) {
    EXTRACT(insert_dummy_spec, ast->left);
    EXTRACT_NOTNULL(name_columns_values, ast->right);
    EXTRACT_ANY_NOTNULL(target, name_columns_values->left)
    EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);

    // check for the simple case of fetching to or from a blob

    if (insert_dummy_spec) {
      return false;
    }

    if (!is_ast_from_shape(columns_values->right)) {
      return false;
    }

    EXTRACT_NOTNULL(from_shape, columns_values->right);

    // any shape with a like clause is not the blob case
    if (from_shape->left) {
      return false;
    }

    EXTRACT_ANY_NOTNULL(source, from_shape->right);

    EXTRACT_STRING(source_name, source);
    EXTRACT_STRING(target_name, target);
    ast_node *variable = find_local_or_global_variable(source_name);
    if (variable && is_blob(variable->sem->sem_type)) {
      // we're committed now, there's a blob on the right
      // one way or another we are resolving the type
      dest = cursor = target;
      src = blob = source;
      goto rewrite_or_fail;
    }
  }
  else {
    EXTRACT_ANY_NOTNULL(source, ast->right);
    EXTRACT_ANY_NOTNULL(target, ast->left);
    EXTRACT_STRING(target_name, target);

    ast_node *variable = find_local_or_global_variable(target_name);
    if (variable && is_blob(variable->sem->sem_type)) {
      // we're committed now, there's a blob on the right
      // one way or another we are resolving the type
      src = cursor = source;
      dest = blob = target;
      goto rewrite_or_fail;
    }
  }

  // the blob position is not a blob, proceed as usual
  return false;

rewrite_or_fail:
  Invariant(cursor);

  sem_validate_cursor_blob_compat(ast, cursor, blob, dest, src);
  if (is_error(ast)) {
    return true;
  }

  if (dest == blob) {
    ast->type = k_ast_set_blob_from_cursor_stmt;
  }
  else {
    ast->type = k_ast_fetch_cursor_from_blob_stmt;
  }

  ast_set_left(ast, dest);
  ast_set_right(ast, src);

  return true;
}

static int32_t cursor_base;

// This creates the statements for each child partition creation
static ast_node *rewrite_child_partition_creation(ast_node *child_results, int32_t cursor_num, ast_node *tail) {
  if (!child_results) {
    return tail;
  }

  // note that I have not included the numbers that get appended to the names
  //
  // let __partition__ := cql_partition_create();
  // declare __child_cursor__ cursor for call child_proc();  -- args as needed
  // loop fetch __child_cursor__
  // begin
  //   fetch __key__ from __child_cursor__(like __key__);
  //   set result_ := cql_partition_cursor(__partition__, __key__, __child_cursor__));
  // end;
  //

  EXTRACT_NOTNULL(child_result, child_results->left);
  EXTRACT_NOTNULL(call_stmt, child_result->left);
  EXTRACT_NOTNULL(named_result, child_result->right);
  EXTRACT_NOTNULL(name_list, named_result->right);
  EXTRACT_STRING(proc_name, call_stmt->left);

  CSTR key_name = dup_printf("__key__%d", cursor_num);
  CSTR cursor_name = dup_printf("__child_cursor__%d", cursor_num);
  CSTR partition_name = dup_printf("__partition__%d", cursor_num);
  CSTR result_name = dup_printf("__result__%d", cursor_base);

  return new_ast_stmt_list(
      new_ast_declare_cursor_like_name(
        new_ast_str(key_name),
        new_ast_shape_def(
          new_ast_like(
            new_ast_str(proc_name),
            NULL
          ),
          name_list
        )
      ),
    new_ast_stmt_list(
      new_ast_let_stmt(
        new_ast_str(partition_name),
        new_ast_call(
          new_ast_str("cql_partition_create"),
          new_ast_call_arg_list(
            new_ast_call_filter_clause(NULL, NULL),
            NULL
          )
        )
      ),
    new_ast_stmt_list(
      new_ast_declare_cursor(
        new_ast_str(cursor_name),
        call_stmt
      ),
    // loop fetch __child_cursor__
    new_ast_stmt_list(
      new_ast_loop_stmt(
        new_ast_fetch_stmt(
          new_ast_str(cursor_name),
          NULL
        ),
        // FETCH __key_cursor FROM _child_cursor_(LIKE __key_cursor_);
        new_ast_stmt_list(
          new_ast_fetch_values_stmt(
            NULL,  // no dummy values
            new_ast_name_columns_values(
              new_ast_str(key_name),
              new_ast_columns_values(
                NULL,
                new_ast_from_shape(
                  new_ast_column_spec(
                    new_ast_shape_def(
                      new_ast_like(
                        new_ast_str(key_name),
                        NULL
                      ),
                      NULL
                    )
                  ),
                  new_ast_str(cursor_name)
                )
              )
            )
          ),
          //  SET _add_result_ := cql_partition_cursor(__partition___, __key_cursor_, __child_cursor__);
          new_ast_stmt_list(
            new_ast_assign(
              new_ast_str(result_name),
              new_ast_call(
                new_ast_str("cql_partition_cursor"),
                new_ast_call_arg_list(
                  new_ast_call_filter_clause(NULL, NULL),
                  new_ast_arg_list(
                    new_ast_str(partition_name),
                    new_ast_arg_list(
                      new_ast_str(key_name),
                      new_ast_arg_list(
                        new_ast_str(cursor_name),
                        NULL
                      )
                    )
                  )
                )
              )
            ),
            NULL
          )
        )
      ),
      rewrite_child_partition_creation(child_results->right, cursor_num + 1, tail)
  ))));
}

static ast_node *build_child_typed_names(ast_node *child_results, int32_t child_index) {
  if (!child_results) {
    return NULL;
  }

  // named_type  child[n] object<child_proc set>, ...

  Contract(is_ast_child_results(child_results));
  EXTRACT_NOTNULL(child_result, child_results->left);
  EXTRACT_NOTNULL(call_stmt, child_result->left);
  EXTRACT_NOTNULL(named_result, child_result->right);
  EXTRACT_NOTNULL(name_list, named_result->right);
  EXTRACT_STRING(proc_name, call_stmt->left);

  // optional child result name
  CSTR child_column_name = NULL;
  if (named_result->left) {
    EXTRACT_STRING(name, named_result->left);
    child_column_name = name;
  }

  if (!child_column_name) {
    child_column_name = dup_printf("child%d", child_index);
  }

  return new_ast_typed_names(
    new_ast_typed_name(
      new_ast_str(child_column_name),
      new_ast_notnull(
        new_ast_type_object(
          new_ast_str(dup_printf("%s SET", proc_name))
        )
      )
    ),
    build_child_typed_names(child_results->right, child_index + 1)
  );
}

static ast_node *rewrite_out_cursor_declare(
  CSTR parent_proc_name,
  CSTR out_cursor_name,
  ast_node *child_results,
  ast_node *tail)
{
  // DECLARE __out_cursor__ CURSOR LIKE (LIKE __parent__,  .. child .. list)
  return new_ast_stmt_list(
    new_ast_declare_cursor_like_typed_names(
      new_ast_str(out_cursor_name),
      new_ast_typed_names(
        new_ast_typed_name(
          NULL,
          new_ast_shape_def(
            new_ast_like(
              new_ast_str(parent_proc_name),
              NULL
            ),
            NULL
          )
        ),
        build_child_typed_names(child_results, 1)
      )
    ),
    tail
  );
}

ast_node *rewrite_load_child_keys_from_parent(
  ast_node *child_results,
  CSTR parent_cursor_name,
  int32_t cursor_num,
  ast_node *tail)
{
  if (!child_results) {
    return tail;
  }

  // generates this pattern for each child
  // fetch __key__ from __parent__0(like __key__);

  CSTR key_name = dup_printf("__key__%d", cursor_num);

  return new_ast_stmt_list(
    new_ast_fetch_values_stmt(
      NULL,  // no dummy values
      new_ast_name_columns_values(
        new_ast_str(key_name),
        new_ast_columns_values(
          NULL,
          new_ast_from_shape(
            new_ast_column_spec(
              new_ast_shape_def(
                new_ast_like(
                  new_ast_str(key_name),
                  NULL
                ),
                NULL
              )
            ),
            new_ast_str(parent_cursor_name)
          )
        )
      )
    ),
    rewrite_load_child_keys_from_parent(child_results->right, parent_cursor_name, cursor_num + 1, tail)
  );
}

static ast_node *rewrite_insert_children_partitions(
  ast_node *child_results,
  int32_t cursor_num)
{
  if (!child_results) {
    return NULL;
  }

  //  cql_partition_extract(__partition__, __key__)
  CSTR partition_name = dup_printf("__partition__%d", cursor_num);
  CSTR key_name = dup_printf("__key__%d", cursor_num);

  return new_ast_insert_list(
    new_ast_call(
      new_ast_str("cql_extract_partition"),
      new_ast_call_arg_list(
        new_ast_call_filter_clause(NULL, NULL),
        new_ast_arg_list(
          new_ast_str(partition_name),
          new_ast_arg_list(
            new_ast_str(key_name),
            NULL
          )
        )
      )
    ),
    rewrite_insert_children_partitions(child_results->right, cursor_num + 1)
  );
}

static ast_node *rewrite_declare_parent_cursor(
  CSTR parent_cursor_name,
  ast_node *parent_call_stmt,
  ast_node *tail)
{
  Contract(is_ast_call_stmt(parent_call_stmt));

  // DECLARE CP CURSOR FOR CALL parent();

  return new_ast_stmt_list(
    new_ast_declare_cursor(
      new_ast_str(parent_cursor_name),
      parent_call_stmt
    ),
    tail
  );
}

static ast_node *rewrite_fetch_results(
  CSTR out_cursor_name,
  CSTR parent_cursor_name,
  ast_node *child_results)
{
  Contract(is_ast_child_results(child_results));

  //   -- load up the wider cursor
  //   fetch __out_cursor__ from values(
  //     from __parent__,
  //     cql_partition_extract(__partition__1, __key__1),
  //     cql_partition_extract(__partition__2, __key__2)
  //   );

  return new_ast_stmt_list(
    new_ast_fetch_values_stmt(
      NULL,  // no dummy values
      new_ast_name_columns_values(
        new_ast_str(out_cursor_name),
        new_ast_columns_values(
          NULL,
          new_ast_insert_list(
            new_ast_from_shape(
              new_ast_str(parent_cursor_name),
              NULL
            ),
            rewrite_insert_children_partitions(child_results, cursor_base)
          )
        )
      )
    ),
    new_ast_stmt_list(
      new_ast_out_union_stmt(
        new_ast_str(out_cursor_name)
      ),
      NULL
    )
  );
}

static ast_node *rewrite_loop_fetch_parent_cursor(
  CSTR parent_cursor_name,
  CSTR out_cursor_name,
  ast_node *child_results)
{
  // generate code to read rows from parent and attach the matching rows from each partition above via hash lookup
  //
  // declare __parent__ cursor for call parent();  -- args as needed
  // loop fetch __parent__
  // begin
  //   -- look up key columns using the matching column names
  //   fetch __key__1 from __parent__(like __key__1);
  //   fetch __key__2 from __parent__(like __key__2);
  //
  //   -- load up the wider cursor
  //   fetch __out_cursor__ from values(
  //     from __parent__,
  //     cql_partition_extract(__partition__1, __key__1),
  //     cql_partition_extract(__partition__2, __key__2)
  //   );
  //   out union __out_cursor__;
  // end;

  return new_ast_stmt_list(
    new_ast_loop_stmt(
      new_ast_fetch_stmt(
        new_ast_str(parent_cursor_name),
        NULL
      ),
      rewrite_load_child_keys_from_parent(child_results, parent_cursor_name, cursor_base,
        rewrite_fetch_results(out_cursor_name, parent_cursor_name, child_results)
      )
    ),
    NULL
  );
}

// The general rewrite looks like this:
//
// create proc test_parent_child()
// begin
//   out union call parent(2) join call child(1) using (x);
// end;
//
// becomes:
//
// CREATE PROC test_parent_child ()
// BEGIN
//   DECLARE __result__0 BOOL NOT NULL;
//   DECLARE __key__0 CURSOR LIKE test_child(x);
//   LET __partition__0 := cql_partition_create();
//   DECLARE __child_cursor__0 CURSOR FOR CALL test_child(1);
//   LOOP FETCH __child_cursor__0
//   BEGIN
//     FETCH __key__0(x) FROM VALUES(__child_cursor__0.x);
//     SET __result__0 := cql_partition_cursor(__partition__0, __key__0, __child_cursor__0);
//   END;
//
//   The above repeats once for each child result set
//
//   DECLARE __out_cursor__0 CURSOR LIKE (x INTEGER, child1 OBJECT<test_child SET> NOT NULL);
//   DECLARE __parent__0 CURSOR FOR CALL test_parent(2);
//   LOOP FETCH __parent__0
//   BEGIN
//     FETCH __key__0(x) FROM VALUES(__parent__0.x);
//     FETCH __out_cursor__0(x, child1) FROM VALUES(__parent__0.x, cql_extract_partition(__partition__0, __key__0));
//     OUT UNION __out_cursor__0;
//   END;
// END;
//
cql_noexport void rewrite_out_union_parent_child_stmt(ast_node *ast) {
  Contract(is_ast_out_union_parent_child_stmt(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  CSTR result_name = dup_printf("__result__%d", cursor_base);
  CSTR out_cursor_name = dup_printf("__out_cursor__%d", cursor_base);
  CSTR parent_cursor_name = dup_printf("__parent__%d", cursor_base);

  // DECLARE __result_ BOOL NOT NULL;
  ast_node *result_var =
    new_ast_declare_vars_type(
      new_ast_name_list(
        new_ast_str(result_name), NULL),
      new_ast_notnull(new_ast_type_bool(NULL))
    );

  EXTRACT_NOTNULL(child_results, ast->right);
  EXTRACT_NOTNULL(call_stmt, ast->left);
  EXTRACT_STRING(parent_proc_name, call_stmt->left);

  // we have to go up the ast to find the statement list, we need to insert ourselves here.
  ast_node *stmt_tail = ast;
  while (!is_ast_stmt_list(stmt_tail)) {
    stmt_tail = stmt_tail->parent;
  }

  ast_node *result = rewrite_child_partition_creation(child_results, cursor_base,
    rewrite_out_cursor_declare(parent_proc_name, out_cursor_name, child_results,
      rewrite_declare_parent_cursor(parent_cursor_name, call_stmt,
        rewrite_loop_fetch_parent_cursor(parent_cursor_name, out_cursor_name, child_results)
      )
    )
  );

  // now we find the last statement in the chain of statements we just generated
  ast_node *end = result;
  Invariant(is_ast_stmt_list(end));

  while (end->right) {
    end = end->right;
    Invariant(is_ast_stmt_list(end));
  }

  Invariant(is_ast_stmt_list(end));

  // we link our new stuff in and we're good to go
  ast_set_right(end, stmt_tail->right);
  ast_set_right(stmt_tail, result);

  AST_REWRITE_INFO_RESET();

  // the last thing we do is clobber the original statement node with the result variable assignment
  // leaving no trace of the original ast
  ast_set_left(ast, result_var->left);
  ast_set_right(ast, result_var->right);
  ast->type = result_var->type;

  int32_t child_count = 0;
  while (child_results) {
    child_count++;
    child_results = child_results->right;
  }
  cursor_base += child_count;
}

// Each column in the backed table needs an entry in the select list for the generated
// create proc.  It will fetch from either the key blob or the value blob.  This can be
// generalized in the future but for now we support only the "two blob" backing store
// shape.  We peel off the first item and then recurse to add the nested item.  Not
// especially economical but fine for any normal sized table.  This can be made non-recursive
// if it ever matters.
static ast_node *rewrite_backed_expr_list(ast_node *backed_table, CSTR key, CSTR val, int32_t index) {
  sem_struct *sptr = backed_table->sem->sptr;
  if (index >= sptr->count) {
    return NULL;
  }

  sem_t sem_type = sptr->semtypes[index];
  bool_t is_key_column = is_primary_key(sem_type) || is_partial_pk(sem_type);

  ast_node *result = new_ast_select_expr_list(
    new_ast_select_expr(
      new_ast_call(
        new_ast_str("cql_blob_get"),
        new_ast_call_arg_list(
          new_ast_call_filter_clause(NULL, NULL),
          new_ast_arg_list(
            new_ast_dot(
              new_ast_str("T"),
              new_ast_str(is_key_column ? key : val)
            ),
            new_ast_arg_list(
              new_ast_dot(
                new_ast_str(sptr->struct_name),
                new_ast_str(sptr->names[index])
              ),
              NULL
            )
          )
        )
      ),
      new_ast_opt_as_alias(
        new_ast_str(sptr->names[index])
      )
    ),
    rewrite_backed_expr_list(backed_table, key, val, index + 1)
  );

  return result;
}

// Once we have a valid backed table we need to create a shared fragment that access it
// so that we can use it when rewriting select statements.  To do this we make the create
// proc statement out of thin air and then parse it, adding it to the set of shared fragments.
// The generated procedure looks like this:
//
// @ATTRIBUTE(cql:shared_fragment)
// CREATE PROC _backed ()
// BEGIN
//   SELECT
//     cql_blob_get(T.k, backed.id) AS id,
//     cql_blob_get(T.v, backed.t) AS t,
//     cql_blob_get(T.v, backed.v) AS v
//   FROM backing AS T;
// END;
//
// This is a fixed shape with just names plugged in except for the expression list
// which is generated by a helper above.
// Here the table "backing" has two blob columns "k" and "v" for the key and value storage.
cql_noexport void rewrite_shared_fragment_from_backed_table(ast_node *_Nonnull backed_table) {
  EXTRACT_MISC_ATTRS(backed_table, misc_attrs);

  AST_REWRITE_INFO_SET(backed_table->lineno, backed_table->filename);

  Contract(is_ast_create_table_stmt(backed_table));
  EXTRACT_NOTNULL(create_table_name_flags, backed_table->left);
  EXTRACT_STRING(backed_table_name, create_table_name_flags->right);
  CSTR proc_name = dup_printf("_%s", backed_table_name);

  CSTR backing_table_name = get_named_string_attribute_value(misc_attrs, "backed_by");
  Invariant(backing_table_name);  // already validated
  ast_node *backing_table = find_table_or_view_even_deleted(backing_table_name);
  Invariant(backing_table);  // already validated
  sem_struct *sptr_backing = backing_table->sem->sptr;
  Invariant(sptr_backing);  // table must have a sem_struct

  // figure out the column order of the key and value columns in the backing store
  // the options are "key, value" or "value, key"
  sem_t sem_type = sptr_backing->semtypes[0];
  bool_t is_key_first = is_primary_key(sem_type) || is_partial_pk(sem_type);

  CSTR backing_key = sptr_backing->names[!is_key_first]; // if the order is kv then the key is column 0, else 1
  CSTR backing_val = sptr_backing->names[is_key_first];  // if the oder is kv then the value is colume 1, else 0

  ast_node *select_expr_list = rewrite_backed_expr_list(backed_table, backing_key, backing_val, 0);

  ast_node *select_stmt =
    new_ast_select_stmt(
      new_ast_select_core_list(
        new_ast_select_core(
          NULL,
          new_ast_select_expr_list_con(
            // computed select list (see above)
            new_ast_select_expr_list(
              new_ast_select_expr(
                new_ast_str("rowid"),
                NULL
              ),
              select_expr_list
            ),
            // from backing store, with short alias "T"
            new_ast_select_from_etc(
              new_ast_table_or_subquery_list(
                new_ast_table_or_subquery(
                  new_ast_str(backing_table_name),
                  new_ast_opt_as_alias(new_ast_str("T"))
                ),
                NULL
              ),
              // use where to constraint the row type
              new_ast_select_where(
                new_ast_opt_where(
                  new_ast_eq(
                    new_ast_call(
                      new_ast_str("cql_blob_get_type"),
                      new_ast_call_arg_list(
                        new_ast_call_filter_clause(NULL, NULL),
                        new_ast_arg_list(
                          new_ast_dot(
                            new_ast_str("T"),
                            new_ast_str(backing_key)
                          ),
                          NULL
                        )
                      )
                    ),
                    new_ast_num(NUM_LONG, gen_type_hash(backed_table))
                  )
                ),
                new_ast_select_groupby(
                  NULL,
                  new_ast_select_having(
                    NULL,
                    NULL
                  )
                )
              )
            )
          )
        ),
        NULL
      ),
      // empty orderby, limit, offset
      new_ast_select_orderby(
        NULL,
        new_ast_select_limit(
          NULL,
          new_ast_select_offset(
            NULL,
            NULL
          )
        )
      )
    );

  // create the proc wrapper and add @attribute(cql:shared_fragment)
  ast_node *stmt_and_attr =
    new_ast_stmt_and_attr(
      new_ast_misc_attrs(
        new_ast_misc_attr(
          new_ast_dot(
            new_ast_str("cql"),
            new_ast_str("shared_fragment")
          ),
          NULL
        ),
        NULL
      ),
      new_ast_create_proc_stmt(
        new_ast_str(proc_name),
        new_ast_proc_params_stmts(
          NULL,
          new_ast_stmt_list(
            select_stmt,
            NULL
          )
        )
      )
    );

  // stdout the rewrite for debugging if needed
  // gen_stmt_list_to_stdout(new_ast_stmt_list(stmt_and_attr, NULL));

  AST_REWRITE_INFO_RESET();

  // analysis can't fail, there's nothing to go wrong, all names already checked
  // note that semantic analysis expects to start at the statement not the attributes
  // so we skip into the statement.
  sem_one_stmt(stmt_and_attr->right);
  Invariant(!is_error(stmt_and_attr->right));
}

// Here we find all of the backed tables that have been mentioned in this statement
// from the backed tables list and produce a chain of CTEs that define them.  This
// can then be linked into some other statement (see below)
static void rewrite_backed_table_ctes(
  list_item *backed_tables_list,
  ast_node **pcte_tables,
  ast_node **pcte_tail)
{
  symtab *backed = symtab_new();

  ast_node *backed_cte_tables = NULL;
  ast_node *cte_tail = NULL;

  for (list_item *item = backed_tables_list; item; item = item->next) {
    EXTRACT_NOTNULL(table_or_subquery, item->ast);
    EXTRACT_STRING(backed_table_name, table_or_subquery->left);
    CSTR backed_proc_name = dup_printf("_%s", backed_table_name);

    if (symtab_add(backed, backed_table_name, NULL)) {
      // need a new backed table CTE for this one
      backed_cte_tables = new_ast_cte_tables(
        new_ast_cte_table(
          new_ast_cte_decl(
            new_ast_str(backed_table_name),
            new_ast_star()
          ),
          new_ast_shared_cte(
            new_ast_call_stmt(
              new_ast_str(backed_proc_name),
              NULL
            ),
            NULL
          )
        ),
        backed_cte_tables
      );

      if (cte_tail == NULL) {
        cte_tail = backed_cte_tables;
      }
    }
  }

  *pcte_tail = cte_tail;
  *pcte_tables = backed_cte_tables;

  symtab_delete(backed);
}

// This is the magic, we have tracked the backed tables so now we can insert calls to
// the generated shared fragments (see above) for each such table.  Once we've done that,
// the select will "just work." because the backed table has been aliased by a correct CTE.
cql_noexport void rewrite_statement_backed_table_ctes(
  ast_node *_Nonnull stmt,
  list_item *_Nonnull backed_tables_list)
{
  Contract(stmt);
  Contract(backed_tables_list);

  ast_node *backed_cte_tables = NULL;
  ast_node *cte_tail = NULL;

  rewrite_backed_table_ctes(backed_tables_list, &backed_cte_tables, &cte_tail);
  
  Invariant(cte_tail);
  Invariant(backed_cte_tables);

  if (is_ast_with(stmt->left)) {
    // add the backed table CTEs to the front of the list
    EXTRACT(with, stmt->left);
    EXTRACT(cte_tables, with->left);
    ast_set_right(cte_tail, cte_tables);
    ast_set_left(with, cte_tail);
  }
  else {
    // preserve the old (left, right) in a nested node and swap in the "with" node
    ast_set_right(stmt, new_ast(stmt->type, stmt->left, stmt->right));
    ast_set_left(stmt, new_ast_with(backed_cte_tables));

    // map the node type to the with form
    if (stmt->type == k_ast_select_stmt) {
      stmt->type = k_ast_with_select_stmt;
    }
    else if (stmt->type == k_ast_update_stmt) {
      stmt->type = k_ast_with_update_stmt;
    }
    else if (stmt->type == k_ast_delete_stmt) {
      stmt->type = k_ast_with_delete_stmt;
    }
    else { 
      // this is all that's left
      Invariant(stmt->type == k_ast_insert_stmt);
      stmt->type = k_ast_with_insert_stmt;
    }
  }

  // stdout the rewrite for debugging if needed
  // gen_stmt_list_to_stdout(new_ast_stmt_list(stmt, NULL));
}

// Select is the simplest case, all we have to do is add the references
// to the backed tables to the select statement converting it into
// a with select in the process rewrite_statement_backed_table_ctes
// does exactly this job.  All the statement types use that helper
// to get the CTE structure correct.
cql_noexport void rewrite_select_for_backed_tables(
  ast_node *_Nonnull stmt,
  list_item *_Nonnull backed_tables_list)
{
  Contract(is_ast_select_stmt(stmt) || is_ast_with_select_stmt(stmt));
  Contract(backed_tables_list);

  AST_REWRITE_INFO_SET(stmt->lineno, stmt->filename);

  rewrite_statement_backed_table_ctes(stmt, backed_tables_list);

  AST_REWRITE_INFO_RESET();

  sem_select(stmt);
}

// This walks the name list and generates either the args for the key or the args for the value
// both are just going to be V.col_name from the _vals alias and the backed table.column
static ast_node *rewrite_create_blob_args(bool_t for_key, ast_node *backed_table, ast_node *name_list)
{
  if (!name_list) {
    return NULL;
  }

  EXTRACT_STRING(name, name_list->left);
  sem_struct *sptr = backed_table->sem->sptr;
  int32_t icol = sem_column_index(sptr, name);
  Invariant(icol >= 0);  // must be valid name, already checked!
  sem_t sem_type = sptr->semtypes[icol];
  bool_t is_key = is_primary_key(sem_type) || is_partial_pk(sem_type);
  CSTR backed_table_name = sptr->struct_name;

  if (is_key == for_key) {
    return new_ast_arg_list(
      new_ast_dot(new_ast_str("V"), new_ast_str(name)),
      new_ast_arg_list(
        new_ast_dot(new_ast_str(backed_table_name), new_ast_str(name)),
        rewrite_create_blob_args(for_key, backed_table, name_list->right)
      )
    );
  }
  else {
    return rewrite_create_blob_args(for_key, backed_table, name_list->right);
  }
}

// This walks the name list and generates either the key create call or the value create call
// This is the fixed part of the call.
static ast_node *rewrite_blob_create(bool_t for_key, ast_node *backed_table, ast_node *name_list) {
  return new_ast_call(
    new_ast_str("cql_blob_create"),
    new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL),
      new_ast_arg_list(
        new_ast_str(backed_table->sem->sptr->struct_name),
        rewrite_create_blob_args(for_key, backed_table, name_list)
      )
    )
  );
}


static ast_node *cql_blob_get_call (
  CSTR key_val,
  CSTR backed_table,
  CSTR col)
{
  return new_ast_call(
    new_ast_str("cql_blob_get"),
    new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL),
      new_ast_arg_list(
        new_ast_str(key_val),
        new_ast_arg_list(
          new_ast_dot(
            new_ast_str(backed_table),
            new_ast_str(col)
          ),
          NULL
        )
      )
    )
  );
}

// several args need to flow during the recursion so we bundle them into a struct
// so that we can flow the pointer instead of all these arguments
typedef struct update_rewrite_info {
  CSTR backing_key;
  CSTR backing_val;
  bool_t for_key;
  ast_node *backed_table;
} update_rewrite_info;

// if we found any references to backed columns extract from the blob
static void rewrite_blob_column_references(update_rewrite_info *info, ast_node *ast) {
  // the name nodes have all we need in the semantic payload
  if (is_ast_str(ast) || is_ast_dot(ast)) {
    if (ast->sem && ast->sem->backed_table) {
       // we know the name but to get the PK info we need to get to the semantic type of the column
       // pk info doesn't flow through the normal exression tree. No problem, we'll just look
       // up the name in the backed table and get the type from there.
       sem_struct *sptr_backed = info->backed_table->sem->sptr;
       int32_t i = find_col_in_sptr(sptr_backed, ast->sem->name);
       Invariant(i >= 0);  // the column for sure exists, it's already been checked
       sem_t sem_type = sptr_backed->semtypes[i];

       // now we can easily decide which backing column to use
       bool_t is_key_column = is_primary_key(sem_type) || is_partial_pk(sem_type);
       CSTR blob_field = is_key_column ? info->backing_key : info->backing_val;
       ast_node *new = cql_blob_get_call(blob_field, ast->sem->backed_table, ast->sem->name);
       ast->type = new->type;
       ast_set_left(ast, new->left);
       ast_set_right(ast, new->right);
    }
    return;
  }

  if (ast_has_left(ast)) {
    rewrite_blob_column_references(info, ast->left);
  }
  if (ast_has_right(ast)) {
    rewrite_blob_column_references(info, ast->right);
  }
}

// This walks the update list and generates either the args for the key or the args for the value
// the values come from the assignment in the update entry list
static ast_node *rewrite_update_blob_args(update_rewrite_info *info, ast_node *update_list) {
  if (!update_list) {
    return NULL;
  }
  Contract(is_ast_update_list(update_list));

  EXTRACT_NOTNULL(update_entry, update_list->left);
  EXTRACT_STRING(name, update_entry->left);
  EXTRACT_ANY_NOTNULL(expr, update_entry->right);

  sem_struct *sptr = info->backed_table->sem->sptr;
  int32_t icol = sem_column_index(sptr, name);
  Invariant(icol >= 0);  // must be valid name, already checked!
  sem_t sem_type = sptr->semtypes[icol];
  bool_t is_key = is_primary_key(sem_type) || is_partial_pk(sem_type);
  CSTR backed_table_name = sptr->struct_name;

  if (is_key == info->for_key) {
    rewrite_blob_column_references(info, expr);
    return new_ast_arg_list(
      expr,
      new_ast_arg_list(
        new_ast_dot(new_ast_str(backed_table_name), new_ast_str(name)),
        rewrite_update_blob_args(info, update_list->right)
      )
    );
  }
  else {
    return rewrite_update_blob_args(info, update_list->right);
  }
}

// This walks the name list and generates either the key update call or the value update call
// This is the fixed part of the call.
static ast_node *rewrite_blob_update(
  bool_t for_key,
  sem_struct *sptr_backing,
  ast_node *backed_table,
  ast_node *update_list)
{

  sem_t sem_type = sptr_backing->semtypes[0];
  bool_t is_key_first = is_primary_key(sem_type) || is_partial_pk(sem_type);

  update_rewrite_info info;

  info.backing_key = sptr_backing->names[!is_key_first]; // if the order is kv then the key is column 0, else 1
  info.backing_val = sptr_backing->names[is_key_first];  // if the oder is kv then the value is colume 1, else 0
  info.for_key = for_key;
  info.backed_table = backed_table;

  // if there are no args for this blob type then do not make the blob update call at all.
  ast_node *arg_list = rewrite_update_blob_args(&info, update_list);
  if (!arg_list) {
    return NULL;
  }

  CSTR blob_name = for_key ? info.backing_key : info.backing_val;
  ast_node *blob_val = new_ast_str(blob_name);

  Contract(is_ast_update_list(update_list));

  return new_ast_call(
    new_ast_str("cql_blob_update"),
    new_ast_call_arg_list(
      new_ast_call_filter_clause(NULL, NULL),
      new_ast_arg_list(
        blob_val,
        arg_list
      )
    )
  );
}

// This helper creates the select list we will need to get the values out
// from the statement that was the insert list (it could be values or a select statement)
static ast_node *rewrite_insert_list_as_select_values(ast_node *insert_list) {
  return new_ast_select_stmt(
    new_ast_select_core_list(
      new_ast_select_core(
        new_ast_select_values(),
        new_ast_values(
            insert_list,
            NULL
        )
      ),
      NULL
    ),
    new_ast_select_orderby(
      NULL,
      new_ast_select_limit(
        NULL,
        new_ast_select_offset(
          NULL,
          NULL
        )
      )
    )
  );
}

// The general insert pattern converts something like this:
//
// insert into backed values(1,2,3), (4,5,6), (7,8,9);
//
// into:
//
// WITH
// _vals (pk, x, y) AS (VALUES(1, "2", 3.14), (4, "5", 6), (7, "8", 9.7))
// INSERT INTO backing(k, v)
//   SELECT bcreatekey(9032558069325805135L, V.pk, 1),
//          bcreateval(9032558069325805135L, V.x, 7953209610392031882L, 4, V.y, 4501343740738089802L, 3)
//   FROM _vals V;
//
// To do this we need to:
//  * make the _vals CTE out of the values clause
//  * add a select clause that maps the values
//
// This code uses cql_blob_create(...) to which ultimately expands into whatever the blob create
// functions will be when sql code gen happens.
//
// cql_blob_create calls look like
//
// cql_blob_create(backed_type, val1, backed_type.col1, val2, backed_type.col2, ...)
//
// Those calls expand to include the hash codes if needed and field types.
//
cql_noexport void rewrite_insert_statement_for_backed_table(
  ast_node *ast,
  list_item *backed_tables_list)
{
  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  ast_node *with_node = NULL;
  ast_node *stmt = NULL;

  if (is_ast_with_insert_stmt(ast)) {
    EXTRACT_NOTNULL(with, ast->left)
    EXTRACT_NOTNULL(insert_stmt, ast->right);
    stmt = insert_stmt;
    with_node = with;
  }
  else {
    Contract(is_ast_insert_stmt(ast));
    stmt = ast;
  }

  Invariant(is_ast_insert_stmt(stmt));
  EXTRACT_NOTNULL(name_columns_values, stmt->right);
  EXTRACT_STRING(backed_table_name, name_columns_values->left);
  EXTRACT_ANY_NOTNULL(columns_values, name_columns_values->right);

  // table has already been checked, it exists, it's legal
  // but it might not be backed, in which case we have less work to do
  ast_node *backed_table = find_table_or_view_even_deleted(backed_table_name);
  Contract(is_ast_create_table_stmt(backed_table));
  if (!is_backed(backed_table->sem->sem_type)) {
    goto replace_backed_tables_only;
  }

  EXTRACT_MISC_ATTRS(backed_table, misc_attrs);

  CSTR backing_table_name = get_named_string_attribute_value(misc_attrs, "backed_by");
  Invariant(backing_table_name);  // already validated
  ast_node *backing_table = find_table_or_view_even_deleted(backing_table_name);
  Invariant(backing_table);  // already validated
  sem_struct *sptr_backing = backing_table->sem->sptr;
  Invariant(sptr_backing);  // table must have a sem_struct

  // Some explicit contract to clarify which error you have made...

  // the INSERT... USING form must already be resolved by the time we get here
  Contract(!is_ast_expr_names(columns_values));

  // DEFAULT VALUES is not allowed for backed tables, this should have already errored out
  Contract(!is_ast_default_columns_values(columns_values));

  // Standard columns_values node is the only option
  Contract(is_ast_columns_values(columns_values));

  EXTRACT(column_spec, columns_values->left);
  EXTRACT_ANY(insert_list, columns_values->right);

  // Most insert types are rewritten into select form including the standard values clause
  // but the insert forms that came from a cursor, args, or some other shape are still written
  // using an insert list, these are just vanilla values.  Dummy default and all that sort
  // of business likewise applies to simple insert lists and all of that processing is done.
  // If we find an insert list form the first step is to normalize the insert list into a
  // select...values. We do this so thatwe have just one rewrite path after this point, and
  // because it's stupid simple

  if (is_ast_insert_list(insert_list)) {
    ast_node *select_stmt = rewrite_insert_list_as_select_values(insert_list);
    // debug output if needed
    // gen_stmt_list_to_stdout(new_ast_stmt_list(select_stmt, NULL));
    insert_list = select_stmt;
  }

  // Now either the incoming list came in before it was transformed, in which case contract
  // is broken, or we fixed the one legal case above.  We have an select statement or
  // a broken caller.
  Contract(is_select_stmt(insert_list));

  EXTRACT_NOTNULL(name_list, column_spec->left);

  // make a CTE table _vals that will hold the selected data using the user-provided name list
  ast_node *cte_table_vals = new_ast_cte_table(
    new_ast_cte_decl(
      new_ast_str("_vals"),
      name_list
    ),
    insert_list
  );

  ast_node *key_expr = rewrite_blob_create(true, backed_table, name_list);
  ast_node *val_expr = rewrite_blob_create(false, backed_table, name_list);

  // now we need expressions for the key and value
  // this is a stub for now
  ast_node *select_expr_list = new_ast_select_expr_list(
    new_ast_select_expr(key_expr, NULL),
    new_ast_select_expr_list(
      new_ast_select_expr(val_expr, NULL),
      NULL
    )
  );

  ast_node *select_stmt =
    new_ast_select_stmt(
      new_ast_select_core_list(
        new_ast_select_core(
          NULL,
          new_ast_select_expr_list_con(
            // computed select list (see above)
            select_expr_list,
            // from insert values, with short alias "V"
            new_ast_select_from_etc(
              new_ast_table_or_subquery_list(
                new_ast_table_or_subquery(
                  new_ast_str("_vals"),
                  new_ast_opt_as_alias(new_ast_str("V"))
                ),
                NULL
              ),
              // use where to constraint the row type
              new_ast_select_where(
                NULL,
                new_ast_select_groupby(
                  NULL,
                  new_ast_select_having(
                    NULL,
                    NULL
                  )
                )
              )
            )
          )
        ),
        NULL
      ),
      // empty orderby, limit, offset
      new_ast_select_orderby(
        NULL,
        new_ast_select_limit(
          NULL,
          new_ast_select_offset(
            NULL,
            NULL
          )
        )
      )
    );

  // for debugging dump the generated select statement
  // gen_stmt_list_to_stdout(new_ast_stmt_list(select_stmt, NULL));

  // figure out the column order of the key and value columns in the backing store
  // the options are "key, value" or "value, key"
  sem_t sem_type = sptr_backing->semtypes[0];
  bool_t is_key_first = is_primary_key(sem_type) || is_partial_pk(sem_type);

  CSTR backing_key = sptr_backing->names[!is_key_first]; // if the order is kv then the key is column 0, else 1
  CSTR backing_val = sptr_backing->names[is_key_first];  // if the oder is kv then the value is colume 1, else 0

  ast_node *new_name_columns_values = new_ast_name_columns_values(
    new_ast_str(backing_table_name),
    new_ast_columns_values(
      new_ast_column_spec(
        new_ast_name_list(
          new_ast_str(backing_key),
          new_ast_name_list(
            new_ast_str(backing_val),
            NULL
          )
        )
      ),
      select_stmt
    )
  );

  ast_set_right(stmt, new_name_columns_values);
  // for debugging dump the generated insert statement
  // gen_stmt_list_to_stdout(new_ast_stmt_list(ast, NULL));

  if (with_node) {
    EXTRACT_NOTNULL(cte_tables, with_node->left);

    while (cte_tables->right) {
      Contract(is_ast_cte_tables(cte_tables));
      cte_tables = cte_tables->right;
    }

    Contract(is_ast_cte_tables(cte_tables));
    ast_set_right(cte_tables, new_ast_cte_tables(cte_table_vals, NULL));
  }
  else {
    ast_node *with_insert_stmt = new_ast_with_insert_stmt(
      with_node = new_ast_with(
        new_ast_cte_tables(cte_table_vals, NULL)
      ),
      new_ast_insert_stmt(
        stmt->left,
        stmt->right
      )
    );
    ast->type = with_insert_stmt->type;
    ast_set_left(ast, with_insert_stmt->left);
    ast_set_right(ast, with_insert_stmt->right);
  }

replace_backed_tables_only:

  if (backed_tables_list) {
    rewrite_statement_backed_table_ctes(stmt, backed_tables_list);
  }

  // for debugging, dump the generated ast without trying to validate it at all
  // print_root_ast(ast);
  //
  // for debugging dump the generated insert statement
  // gen_stmt_list_to_stdout(new_ast_stmt_list(ast, NULL));

  AST_REWRITE_INFO_RESET();

  // the insert statement is top level, when it re-enters it expects the cte state to be nil
  cte_state *saved = cte_cur;
  cte_cur = NULL;
    sem_one_stmt(ast);
  cte_cur = saved;
}

static ast_node *rewrite_select_rowid(
  CSTR backed_table_name,
  ast_node *opt_where,
  ast_node *opt_orderby,
  ast_node *opt_limit)
{
  return
    new_ast_select_stmt(
      new_ast_select_core_list(
        new_ast_select_core(
          NULL,
          new_ast_select_expr_list_con(
            // select list is just "rowid"
            new_ast_select_expr_list(
              new_ast_select_expr(
                new_ast_str("rowid"),
                NULL
              ),
              NULL
            ),
            // from clause is just the backed table which will will alias a CTE as usual
            new_ast_select_from_etc(
              new_ast_table_or_subquery_list(
                new_ast_table_or_subquery(
                  new_ast_str(backed_table_name),
                  NULL
                ),
                NULL
              ),
              // use where to hold the previous where clause if any
              new_ast_select_where(
                opt_where,
                new_ast_select_groupby(
                  NULL,
                  new_ast_select_having(
                    NULL,
                    NULL
                  )
                )
              )
            )
          )
        ),
        NULL
      ),
      // empty orderby, limit, offset
      new_ast_select_orderby(
        opt_orderby,
        new_ast_select_limit(
          opt_limit,
          new_ast_select_offset(
            NULL,
            NULL
          )
        )
      )
    );
}

cql_noexport void rewrite_delete_statement_for_backed_table(
  ast_node *ast,
  list_item *backed_tables_list)
{
  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  ast_node *stmt = NULL;

  if (is_ast_with_delete_stmt(ast)) {
    EXTRACT_NOTNULL(with, ast->left)
    EXTRACT_NOTNULL(delete_stmt, ast->right);
    stmt = delete_stmt;
  }
  else {
    Contract(is_ast_delete_stmt(ast));
    stmt = ast;
  }

  Invariant(is_ast_delete_stmt(stmt));
  EXTRACT_STRING(backed_table_name, stmt->left);
  EXTRACT(opt_where, stmt->right);

  // table has already been checked, it exists, it's legal
  // but it might not be backed, in which case we have less work to do
  ast_node *backed_table = find_table_or_view_even_deleted(backed_table_name);
  Contract(is_ast_create_table_stmt(backed_table));
  if (!is_backed(backed_table->sem->sem_type)) {
    goto replace_backed_tables_only;
  }

  // the deleted table needs to be added to the referenced backed tables
  add_item_to_list(
    &backed_tables_list,
    new_ast_table_or_subquery(new_ast_str(backed_table_name), NULL)
  );

  // we are going to need the name of the backing table

  EXTRACT_MISC_ATTRS(backed_table, misc_attrs);

  CSTR backing_table_name = get_named_string_attribute_value(misc_attrs, "backed_by");
  Invariant(backing_table_name);  // already validated

  // the new where clause has at its core a select statement that generates the
  // rowids of the rows to be deleted.  This is using the existing where clause
  // against a from clause that is just the backed table.

  ast_node *select_stmt = rewrite_select_rowid(backed_table_name, opt_where, NULL, NULL);

  // for debugging print just the select statement
  // gen_stmt_list_to_stdout(new_ast_stmt_list(select_stmt, NULL));

  // the new where clause for the delete statement is going to be something like
  // rowid IN (select rowid from selected rows)

  ast_node *new_opt_where =
    new_ast_opt_where(
     new_ast_in_pred(
       new_ast_str("rowid"),
       select_stmt
     )
  );

  // replace the target table and where clause of the backed table
  // with the backing table and adjusted where clause

  ast_set_left(stmt, new_ast_str(backing_table_name));
  ast_set_right(stmt, new_opt_where);

replace_backed_tables_only:

  // now add the backed tables and convert the node to a with delete if necessary
  rewrite_statement_backed_table_ctes(ast, backed_tables_list);

  AST_REWRITE_INFO_RESET();

  // the delete statement is top level, when it re-enters it expects the cte state to be nil
  cte_state *saved = cte_cur;
  cte_cur = NULL;
    sem_one_stmt(ast);
  cte_cur = saved;
}

cql_noexport void rewrite_update_statement_for_backed_table(
  ast_node *ast,
  list_item *backed_tables_list)
{
  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  ast_node *stmt = NULL;

  if (is_ast_with_update_stmt(ast)) {
    EXTRACT_NOTNULL(with, ast->left)
    EXTRACT_NOTNULL(update_stmt, ast->right);
    stmt = update_stmt;
  }
  else {
    Contract(is_ast_update_stmt(ast));
    stmt = ast;
  }

  Invariant(is_ast_update_stmt(stmt));
  EXTRACT_NOTNULL(update_set, stmt->right);
  EXTRACT_NOTNULL(update_list, update_set->left);
  EXTRACT_NOTNULL(update_where, update_set->right);
  EXTRACT(opt_where, update_where->left);
  EXTRACT_NOTNULL(update_orderby, update_where->right);
  EXTRACT(opt_orderby, update_orderby->left);
  EXTRACT(opt_limit, update_orderby->right);

  EXTRACT_STRING(backed_table_name, stmt->left);

  // table has already been checked, it exists, it's legal
  // but it might not be backed, in which case we have less work to do
  ast_node *backed_table = find_table_or_view_even_deleted(backed_table_name);
  Contract(is_ast_create_table_stmt(backed_table));
  if (!is_backed(backed_table->sem->sem_type)) {
    goto replace_backed_tables_only;
  }

  // the updated table needs to be added to the referenced backed tables
  add_item_to_list(
    &backed_tables_list,
    new_ast_table_or_subquery(new_ast_str(backed_table_name), NULL)
  );

  // we are going to need the name of the backing table

  EXTRACT_MISC_ATTRS(backed_table, misc_attrs);

  CSTR backing_table_name = get_named_string_attribute_value(misc_attrs, "backed_by");
  Invariant(backing_table_name);  // already validated
  ast_node *backing_table = find_table_or_view_even_deleted(backing_table_name);
  Invariant(backing_table);  // already validated
  sem_struct *sptr_backing = backing_table->sem->sptr;
  Invariant(sptr_backing);  // table must have a sem_struct

  // figure out the column order of the key and value columns in the backing store
  // the options are "key, value" or "value, key"
  sem_t sem_type = sptr_backing->semtypes[0];
  bool_t is_key_first = is_primary_key(sem_type) || is_partial_pk(sem_type);

  CSTR backing_key = sptr_backing->names[!is_key_first]; // if the order is kv then the key is column 0, else 1
  CSTR backing_val = sptr_backing->names[is_key_first];  // if the oder is kv then the value is colume 1, else 0

  // the new where clause has at its core a select statement that generates the
  // rowids of the rows to be updated.  This is using the existing where clause
  // against a from clause that is just the backed table.

  ast_node *select_stmt = rewrite_select_rowid(
     backed_table_name,
     opt_where,
     opt_orderby,
     opt_limit);

  // for debugging print just the select statement
  // gen_stmt_list_to_stdout(new_ast_stmt_list(select_stmt, NULL));

  // the new where clause for the update statement is going to be something like
  // rowid IN (select rowid from selected rows)

  ast_node *key_expr = rewrite_blob_update(true, sptr_backing, backed_table, update_list);
  ast_node *val_expr = rewrite_blob_update(false, sptr_backing, backed_table, update_list);

  ast_node *new_update_list = new_ast_update_list(NULL, NULL);  // fake list head
  ast_node *up_tail = new_update_list;
  if (key_expr) {
    ast_node *new = new_ast_update_list(
      new_ast_update_entry(new_ast_str(backing_key), key_expr),
      NULL
    );
    ast_set_right(up_tail, new);
    up_tail =  new;
  }

  if (val_expr) {
    ast_node *new = new_ast_update_list(
      new_ast_update_entry(new_ast_str(backing_val), val_expr),
      NULL
    );
    ast_set_right(up_tail, new);
  }

  ast_node *new_opt_where =
    new_ast_opt_where(
     new_ast_in_pred(
       new_ast_str("rowid"),
       select_stmt
     )
  );

  // replace the target table and where clause of the backed table
  // with the backing table and adjusted where clause

  ast_set_left(stmt, new_ast_str(backing_table_name));
  ast_set_left(update_set, new_update_list->right);
  ast_set_left(update_where, new_opt_where);
  ast_set_left(update_orderby, NULL); // opt orderby handled in the select
  ast_set_right(update_orderby, NULL); // opt limit handled in the select

replace_backed_tables_only:

  // now add the backed tables and convert the node to a with delete if necessary
  rewrite_statement_backed_table_ctes(ast, backed_tables_list);

  AST_REWRITE_INFO_RESET();

  // the update statement is top level, when it re-enters it expects the cte state to be nil
  cte_state *saved = cte_cur;
  cte_cur = NULL;
    sem_one_stmt(ast);
  cte_cur = saved;
}

#endif

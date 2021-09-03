/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

static ast_node* rewrite_gen_arg_list(charbuf* format_buf, CSTR cusor_name, CSTR col_name, sem_t type);
static ast_node* rewrite_gen_printf_call(CSTR format, ast_node *arg_list);
static ast_node *rewrite_gen_cursor_printf(ast_node *variable);
static ast_node *rewrite_gen_iif_case_expr(ast_node *expr, ast_node *val1, ast_node *val2);
static ast_node *rewrite_gen_case_expr(ast_node *var1, ast_node *var2, bool_t report_column_name);
static bool_t rewrite_one_def(ast_node *head);
static void rewrite_one_typed_name(ast_node *typed_name, symtab *used_names);

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
  EXTRACT_ANY(like, column_spec->left);

  if (is_ast_like(like)) {
     ast_node *found_shape = sem_find_likeable_ast(like, LIKEABLE_FOR_VALUES);
     if (!found_shape) {
       record_error(columns_values);
       return;
     }

     AST_REWRITE_INFO_SET(like->lineno, like->filename);

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
cql_noexport void rewrite_from_shape_args(ast_node *head) {
  Contract(is_ast_expr_list(head) || is_ast_arg_list(head));

  // We might need to make arg_list nodes or expr_list nodes, they are the same really
  // so we'll change the node type to what we need
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

      ast_node *like_ast = arg->right;
      ast_node *likeable_shape = NULL;

      if (like_ast) {
          likeable_shape = sem_find_likeable_ast(like_ast, LIKEABLE_FOR_VALUES);
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

    if (is_ast_like(ast->left)) {
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
  Contract(is_ast_like(head->left));
  EXTRACT_NOTNULL(like, head->left);
  EXTRACT_STRING(like_name, like->left);

  // it's ok to use the LIKE construct on old tables
  ast_node *likeable_shape = sem_find_likeable_ast(like, LIKEABLE_FOR_VALUES);
  if (!likeable_shape) {
    record_error(head);
    return false;
  }

  AST_REWRITE_INFO_SET(like->lineno, like->filename);

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
      Invariant(is_ast_like(head->left));

      // replace the like entry with a col_def
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
  EXTRACT_NOTNULL(like, param_detail->right);
  EXTRACT_STRING(like_name, like->left);

  ast_node *likeable_shape = sem_find_likeable_ast(like, LIKEABLE_FOR_ARGS);
  if (!likeable_shape) {
    record_error(param);
    return param;
  }

  AST_REWRITE_INFO_SET(like->lineno, like->filename);

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
    EXTRACT_NAMED_NOTNULL(prev, params, params->parent);
    ast_set_right(prev, params->right);
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
cql_noexport bool_t rewrite_call_args_if_needed(ast_node *arg_list) {
  if (arg_list) {
    // if there are any cursor forms in the arg list that need to be expanded, do that here.
    rewrite_from_shape_args(arg_list);
    if (is_error(arg_list)) {
      return false;
    }
  }
  return true;
}

// rewrite call node with cql_cursor_format(X) to a printf(format, arg ...)
// statement. e.g: C cursor has column text x, y
// cql_cursor_format(C); ===> printf("x:%s|y:%s", C.x, C.y);
cql_noexport void rewrite_cql_cursor_format(ast_node *ast) {
  Contract(is_ast_call(ast));
  EXTRACT_ANY_NOTNULL(name_ast, ast->left);
  EXTRACT_STRING(name, name_ast);
  EXTRACT_NOTNULL(call_arg_list, ast->right);
  EXTRACT(arg_list, call_arg_list->right);
  Contract(!Strcasecmp("cql_cursor_format", name));

  ast_node *arg = first_arg(arg_list);

  AST_REWRITE_INFO_SET(name_ast->lineno, name_ast->filename);
  ast_node *printf_node = rewrite_gen_cursor_printf(arg);
  AST_REWRITE_INFO_RESET();

  // Reset the cql_cursor_format function call node to a case_expr
  // node.
  ast->type = printf_node->type;
  ast_set_left(ast, printf_node->left);
  ast_set_right(ast, printf_node->right);

  // do semantic analysis of the rewritten AST to validate it
  sem_expr(ast);

  // the rewrite is not expected to have any semantic error
  Invariant(!is_error(ast));
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
  EXTRACT_NOTNULL(like, typed_name->right);
  EXTRACT_STRING(like_name, like->left);

  ast_node *found_shape = sem_find_likeable_ast(like, LIKEABLE_FOR_VALUES);
  if (!found_shape) {
    record_error(typed_name);
    return;
  }

  AST_REWRITE_INFO_SET(like->lineno, like->filename);

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
    EXTRACT_NAMED_NOTNULL(prev, typed_names, typed_names->parent);
    ast_set_right(prev, typed_names->right);
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

    if (is_ast_like(typed_name->right)) {
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

// Walk the param list looking for any of the "like T" forms
// if any is found, replace that parameter with the table/shape columns
cql_noexport void rewrite_params(ast_node *head, bytebuf *args_info) {
  symtab *param_names = symtab_new();

  for (ast_node *ast = head; ast; ast = ast->right) {
    Contract(is_ast_params(ast));
    EXTRACT_NOTNULL(param, ast->left)
    EXTRACT_NOTNULL(param_detail, param->right)

    if (is_ast_like(param_detail->right)) {
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

// Generate a 'call' node for printf function from a cursor variable.
// This is used to rewrite cql_cursor_format(X) when called from a
// sql context.
// e.g:
// select cql_cursor_format(C) as p; ===> select printf("x:%d|y:%s", C.x, C.y) as p;
static ast_node *rewrite_gen_cursor_printf(ast_node *variable) {
  Contract(is_variable(variable->sem->sem_type));

  CHARBUF_OPEN(format);
  sem_struct *sptr = variable->sem->sptr;
  int32_t count = (int32_t) sptr->count;
  Invariant(count > 0);
  ast_node *arg_list = NULL;

  for (int32_t i = count - 1; i >= 0; i--) {
    Invariant(sptr->names[i]);
    // left side of IS
    ast_node* dot = new_ast_dot(
        new_ast_str(variable->sem->name), new_ast_str(sptr->names[i]));
    // right side of IS
    ast_node* null_node = new_ast_null();
    // left side of WHEN
    ast_node* is_node = new_ast_is(dot, null_node);
    // the THEN part of WHEN THEN
    ast_node* val = new_ast_str("'null'");
    // left case_list node
    ast_node* when = new_ast_when(is_node, val);
    // left connector node
    ast_node* case_list = new_ast_case_list(when, NULL);
    // right connector node: printf(...)
    CHARBUF_OPEN(format_output);
    // arg_list node for the printf call
    ast_node* printf_arg_list = rewrite_gen_arg_list(
        &format_output, variable->sem->name, sptr->names[i], sptr->semtypes[i]);
    ast_node* call_printf = rewrite_gen_printf_call(format_output.ptr, printf_arg_list);
    CHARBUF_CLOSE(format_output);
    // case list with no ELSE (we get ELSE NULL by default)
    ast_node* connector = new_ast_connector(case_list, call_printf);
    // CASE WHEN expr THEN result form; not CASE expr WHEN val THEN result
    ast_node* case_expr = new_ast_case_expr(NULL, connector);
    // new arg_list node
    ast_node* new_arg_list = new_ast_arg_list(case_expr, arg_list);
    arg_list = new_arg_list;
  }

  for (int32_t i = 0; i < count; i++) {
    if (i > 0) {
      bprintf(&format, "|");
    }
    bprintf(&format, "%s:%s", sptr->names[i], "%s");
  }

  CSTR format_lit = dup_printf("'%s'", format.ptr); // this turns into literal name
  ast_node *first_arg_list = new_ast_arg_list(new_ast_str(format_lit), arg_list);
  // call_arg_list node
  ast_node *call_arg_list = new_ast_call_arg_list(new_ast_call_filter_clause(NULL, NULL), first_arg_list);
  ast_node *call = new_ast_call(new_ast_str("printf"), call_arg_list);

  CHARBUF_CLOSE(format);

  return call;
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
      ast_node *expr = new_ast_dot(new_ast_str(c2_name), new_ast_str(sptr2->names[i]));
      // left of WHEN expr
      ast_node* is_node = new_ast_is(expr, new_ast_null());
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
      expr = new_ast_dot(new_ast_str(c1_name), new_ast_str(sptr1->names[i]));
      // left of WHEN expr
      is_node = new_ast_is(expr, new_ast_null());
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
cql_noexport void rewrite_nullable_to_unsafe_notnull(ast_node *_Nonnull ast) {
  Contract(is_id_or_dot(ast));

  AST_REWRITE_INFO_SET(ast->lineno, ast->filename);

  ast_node *id_or_dot;
  if (is_id(ast)) {
    EXTRACT_STRING(name, ast);
    id_or_dot = new_ast_str(name);
  } else {
    Invariant(is_ast_dot(ast));
    EXTRACT_STRING(name, ast->right);
    EXTRACT_STRING(scope, ast->left);
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

#endif

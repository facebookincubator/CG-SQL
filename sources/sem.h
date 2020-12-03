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

//
// The key semantic type information
//
// The rules:
//   * if sem_type is STRUCT then sptr is not null
//   * if sem_type is JOIN then jptr is not null
//   * if node is on a table or a view element, which is a STRUCT
//     then jptr is also not null as an optimization
//   * in all other cases neither is populated
//
// Tables and Views have both their sptr and jptr filled out
// because the first thing you're going to do with a table/view is
// join it to something and so the base case of a 1 table join happens
// all the time.  To make this easier that jptr is pre-populated as
// an optimization.
//

typedef uint64_t sem_t;
#define sem_not(x) u64_not(x)

typedef struct sem_node {
  sem_t sem_type;                   // core type plus flags
  CSTR name;                        // for named expressions in select columns etc.
  CSTR object_type;                 // the Foo in object<Foo>, not a variable or column name
  CSTR error;                       // error text for test output, not used otherwise
  struct sem_struct *sptr;          // encoded struct if any
  struct sem_join *jptr;            // encoded join if any
  int32_t create_version;           // create version if any (really only for tables and columns)
  int32_t delete_version;           // create version if any (really only for tables and columns)
  bool_t  recreate;                 // for tables only, true if marked @create
  CSTR recreate_group_name;         // for tables only, the name of the recreate gruop if they are in one
  CSTR region;                      // the schema region, if applicable, null means unscoped (default)
  symtab *used_symbols;             // for select statements, we need to know which of the ids in the select list was used if any
  list_item *index_list;            // for tables we need the list of indices that use this table (so we can recreate them together if needed)
  struct eval_node *value;          // for enum values we have to store the evaluated constant value of each member of the enum
} sem_node;

// for tables and views and the result of a select

typedef struct sem_struct {
  CSTR struct_name;               // struct name
  uint32_t count;                 // count of fields
  CSTR *names;                    // field names
  sem_t *semtypes;                // typecode for each field
} sem_struct;

// for the data type of (parts of) the FROM clause
// sometimes I refer to as a "joinscope"

typedef struct sem_join {
  uint32_t count;                 // count of table/views in the join
  CSTR *names;                    // names of the table/view
  struct sem_struct **tables;     // struct type of each table/view
} sem_join;

typedef struct recreate_annotation {
  CSTR target_name;               // the name of the target
  CSTR group_name;                // group name or "" if no group (not null, safe to sort)
  ast_node *target_ast;           // top level target (table, view, or index)
  ast_node *annotation_ast;       // the actual annotation
  int32_t ordinal;                // when sorting we want to use the original order (reversed actually) within a group
} recreate_annotation;

typedef struct schema_annotation {
  int32_t version;                // the version number (always > 0)
  ast_node *target_ast;           // top level target (table, view, or index)
  CSTR target_name;               // the name of the target
  uint32_t annotation_type;       // one of the codes below for the type of annotation
  ast_node *annotation_ast;       // the actual annotation
  int32_t column_ordinal;         // -1 if not a column
  ast_node *column_ast;           // a particular column if column annotation
} schema_annotation;

// Note: schema annoations are processed in the indicated order: the numbers matter
#define SCHEMA_ANNOTATION_INVALID 0
#define SCHEMA_ANNOTATION_FIRST 1
#define SCHEMA_ANNOTATION_CREATE_TABLE 1
#define SCHEMA_ANNOTATION_CREATE_COLUMN 2
#define SCHEMA_ANNOTATION_DELETE_TRIGGER 3
#define SCHEMA_ANNOTATION_DELETE_VIEW 4
#define SCHEMA_ANNOTATION_DELETE_INDEX 5
#define SCHEMA_ANNOTATION_DELETE_COLUMN 6
#define SCHEMA_ANNOTATION_DELETE_TABLE 7
#define SCHEMA_ANNOTATION_AD_HOC 8
#define SCHEMA_ANNOTATION_LAST 8

cql_data_decl( bytebuf *schema_annotations );
cql_data_decl( bytebuf *recreate_annotations );

#define SEM_TYPE_NULL 0         // the subtree is a null literal (not just nullable)
#define SEM_TYPE_BOOL 1         // the subtree is a bool
#define SEM_TYPE_INTEGER 2      // the subtree is an integer
#define SEM_TYPE_LONG_INTEGER 3 // the subtree is a long integer
#define SEM_TYPE_REAL 4         // the subtree is a real
#define SEM_TYPE_TEXT 5         // the subtree is a text type
#define SEM_TYPE_BLOB 6         // the subtree is a blob type
#define SEM_TYPE_OBJECT 7       // the subtree is any object type
#define SEM_TYPE_STRUCT 8       // the subtree is a table/view
#define SEM_TYPE_JOIN 9         // the subtree is a join
#define SEM_TYPE_ERROR 10       // marks the subtree as having a problem
#define SEM_TYPE_OK 11          // sentinel for ok but no type info
#define SEM_TYPE_PENDING 12     // sentinel for type calculation in flight
#define SEM_TYPE_REGION 13      // the ast is a schema region
#define SEM_TYPE_CORE 0xff      // bit mask for the core types

#define SEM_TYPE_MAX_UNITARY (SEM_TYPE_OBJECT+1) // the last unitary type

#define SEM_TYPE_NOTNULL          0x0100   // set if and only if null is not possible
#define SEM_TYPE_HAS_DEFAULT      0x0200   // set for table columns with a default
#define SEM_TYPE_AUTOINCREMENT    0x0400   // set for table columns with autoinc
#define SEM_TYPE_VARIABLE         0x0800   // set for variables and parameters
#define SEM_TYPE_IN_PARAMETER     0x1000   // set for in parameters (can mix with below)
#define SEM_TYPE_OUT_PARAMETER    0x2000   // set for out paramters (can mix with above)
#define SEM_TYPE_DML_PROC         0x4000   // set for stored procs that have DML/DDL
#define SEM_TYPE_AUTO_CURSOR      0x8000   // set for a cursor with simplified fetch syntax
#define SEM_TYPE_CREATE_FUNC     0x10000   // set for a function that returns a created object +1 ref
#define SEM_TYPE_SELECT_FUNC     0x20000   // set for a sqlite UDF function declaration
#define SEM_TYPE_HIDDEN          0x40000   // set for columns that are not visible in the current schema version
#define SEM_TYPE_VALIDATED       0x80000   // set if item has already been validated against previous schema
#define SEM_TYPE_USES_OUT       0x100000   // set if proc has a one rowresult using the OUT statement
#define SEM_TYPE_USES_OUT_UNION 0x200000   // set if proc uses the OUT UNION form for multi row result
#define SEM_TYPE_PK             0x400000   // set if column is a primary key
#define SEM_TYPE_FK             0x800000   // set if column is a foreign key
#define SEM_TYPE_UK            0x1000000   // set if column is a unique key
#define SEM_TYPE_VALUE_CURSOR  0x2000000   // set only if SEM_TYPE_AUTO_CURSOR is set and the cursor has no statement
#define SEM_TYPE_SENSITIVE     0x4000000   // set if the object is privacy sensitive
#define SEM_TYPE_DEPLOYABLE    0x8000000   // set if the object is a deployable region
#define SEM_TYPE_BOXED        0x10000000   // set if a cursor's lifetime is managed by a box object
#define SEM_TYPE_HAS_CHECK    0x20000000   // set for table column with a "check" clause
#define SEM_TYPE_HAS_COLLATE  0x40000000   // set for table column with a "collate" clause
#define SEM_TYPE_USES_THROW   0x80000000   // set for a procedure that has a throw in it
#define SEM_TYPE_FLAGS        0xFFFFFF00   // all the flag bits we have so far

#define SEM_EXPR_CONTEXT_NONE           0x001
#define SEM_EXPR_CONTEXT_SELECT_LIST    0x002
#define SEM_EXPR_CONTEXT_WHERE          0x004
#define SEM_EXPR_CONTEXT_ON             0x008
#define SEM_EXPR_CONTEXT_HAVING         0x010
#define SEM_EXPR_CONTEXT_ORDER_BY       0x020
#define SEM_EXPR_CONTEXT_GROUP_BY       0x040
#define SEM_EXPR_CONTEXT_LIMIT          0x080
#define SEM_EXPR_CONTEXT_OFFSET         0x100
#define SEM_EXPR_CONTEXT_TABLE_FUNC     0x200
#define SEM_EXPR_CONTEXT_WINDOW         0x400
#define SEM_EXPR_CONTEXT_WINDOW_FILTER  0x800

#define CURRENT_EXPR_CONTEXT_IS(x)  (!!(current_expr_context & (x)))
#define CURRENT_EXPR_CONTEXT_IS_NOT(x)  (!(current_expr_context & (x)))

#define has_hex_prefix(s) (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))

cql_noexport sem_t core_type_of(sem_t sem_type);
cql_noexport sem_t sensitive_flag(sem_t sem_type);
cql_noexport CSTR coretype_string(sem_t sem_type);

cql_noexport bool_t is_bool(sem_t sem_type);
cql_noexport bool_t is_string_compat(sem_t sem_type);
cql_noexport bool_t is_blob_compat(sem_t sem_type);
cql_noexport bool_t is_object_compat(sem_t sem_type);
cql_noexport bool_t is_create_func(sem_t sem_type);
cql_noexport bool_t is_numeric(sem_t sem_type);
cql_noexport bool_t is_numeric_compat(sem_t sem_type);
cql_noexport bool_t is_numeric_expr(ast_node *expr);
cql_noexport bool_t is_unitary(sem_t sem_type);
cql_noexport bool_t is_struct(sem_t sem_type);
cql_noexport bool_t is_cursor(sem_t sem_type);
cql_noexport bool_t is_primary_key(sem_t sem_type);
cql_noexport bool_t is_foreign_key(sem_t sem_type);
cql_noexport bool_t is_sem_error(sem_node *sem);
cql_noexport bool_t is_error(ast_node *ast);
cql_noexport bool_t is_not_nullable(sem_t sem_type);
cql_noexport bool_t is_variable(sem_t sem_type);
cql_noexport bool_t is_in_parameter(sem_t sem_type);
cql_noexport bool_t is_out_parameter(sem_t sem_type);
cql_noexport bool_t is_in_only(sem_t sem_type);
cql_noexport bool_t is_dml_proc(sem_t sem_type);
cql_noexport bool_t is_text(sem_t sem_type);
cql_noexport bool_t is_blob(sem_t sem_type);
cql_noexport bool_t is_object(sem_t sem_type);
cql_noexport bool_t is_ref_type(sem_t sem_type);
cql_noexport bool_t is_nullable(sem_t sem_type);
cql_noexport bool_t is_null_type(sem_t sem_type);
cql_noexport bool_t is_select_stmt(ast_node *ast);
cql_noexport bool_t is_delete_stmt(ast_node *ast);
cql_noexport bool_t is_insert_stmt(ast_node *ast);
cql_noexport bool_t is_update_stmt(ast_node *ast);
cql_noexport bool_t has_result_set(ast_node *ast);
cql_noexport bool_t has_out_stmt_result(ast_node *ast);
cql_noexport bool_t has_out_union_stmt_result(ast_node *ast);
cql_noexport bool_t is_autotest_dummy_table(CSTR name);
cql_noexport bool_t is_autotest_dummy_insert(CSTR name);
cql_noexport bool_t is_autotest_dummy_select(CSTR name);
cql_noexport bool_t is_autotest_dummy_result_set(CSTR name);
cql_noexport bool_t is_autotest_dummy_test(CSTR name);
cql_noexport bool_t is_referenceable_by_foreign_key(ast_node *ref_table, CSTR column_name);

// Exit if schema validation directive was seen
cql_noexport void exit_on_validating_schema(void);

cql_noexport void sem_main(ast_node *node);
cql_noexport void sem_cleanup(void);
cql_noexport void print_sem_type(struct sem_node *sem);
cql_noexport int32_t sem_column_index(sem_struct *sptr, CSTR name);

cql_noexport ast_node *find_proc(CSTR name);
cql_noexport ast_node *find_region(CSTR name);
cql_noexport ast_node *find_func(CSTR name);
cql_noexport ast_node *find_table_or_view_even_hidden(CSTR name);
cql_noexport ast_node *find_base_fragment(CSTR name);
cql_noexport ast_node *sem_get_col_default_value(ast_node *attrs);
cql_noexport void sem_accumulate_full_region_image(symtab *regions, CSTR name);
cql_noexport void sem_accumulate_public_region_image(symtab *regions, CSTR name);
cql_noexport sem_t find_column_type(CSTR table_name, CSTR column_name);

cql_data_decl( struct list_item *all_tables_list );
cql_data_decl( struct list_item *all_functions_list );
cql_data_decl( struct list_item *all_views_list );
cql_data_decl( struct list_item *all_indices_list );
cql_data_decl( struct list_item *all_triggers_list );
cql_data_decl( struct list_item *all_regions_list );
cql_data_decl( struct list_item *all_ad_hoc_list );
cql_data_decl( struct list_item *all_select_functions_list );
cql_data_decl( struct list_item *all_enums_list );
cql_data_decl( symtab *schema_regions );
cql_data_decl( ast_node *current_proc );
cql_data_decl( charbuf *error_capture );

// These are the symbol tables with the accumulated included/excluded regions
cql_data_decl( symtab *included_regions );
cql_data_decl( symtab *excluded_regions );
cql_data_decl( sem_t global_proc_flags );

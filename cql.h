/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// cql - prounounced "see-queue-el" is a basic tool for enabling stored
//       procedures for SQLite. The tool does this by parsing a language
//       not unlike typical SQL stored procedure forms available in
//       MySql and SQL Server.
//
//       Broadly speaaking compilation is as follows:
//         * SQL statements such as SELECT/INSERT/UPDATE/DELETE
//           are converted into calls to SQLite to do the work.
//           Any variables in those statements are converted into
//           the appropriate binding and and results are read out
//           with the usual SQLite column reading.
//         * Stored procedure control flow is converted into the equivalent
//           C directly.  So for instance an 'IF' in the SQL becomes
//           a correlated 'if' in the generated code.
//
//       The result of this is that CQL produces, "The C you could have
//       written yourself using the SQLite API to do that database operation."
//       CQL does this in a less brittle and type-safe way that is far
//       more maintainable.
//
// Design principles:
//
//  1. Keep each pass in one file (simple, focused, and easy refactor)
//  2. Use simple printable AST parse nodes (no separate #define per AST node type)
//  3. 100% coverage of all logic, no exceptions.

#pragma once

#include "diags.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef CQL_AMALGAM

// as well as the integration points.
#define cql_noexport extern
#define cql_export extern
#define cql_data_decl(x) extern x
#define cql_data_defn(x) x

#endif

#include "compat.h"

typedef uint8_t bool_t;
typedef long long int llint_t;

#define u32_not(x) ((uint32_t)(~(x)))
#define u64_not(x) ((uint64_t)(~(x)))

// patternlint-disable-next-line prefer-sized-ints-in-msys
int main(int argc, char **argv);

typedef struct cmd_options {
  bool_t test;
  bool_t print_ast;
  bool_t print_dot;
  bool_t semantic;
  bool_t codegen;
  bool_t compress;
  bool_t generate_exports;
  bool_t generate_copy;
  bool_t run_unit_tests;
  char *rt;
  char **file_names;
  int32_t file_names_count;
  char **include_regions;
  int32_t include_regions_count;
  char **exclude_regions;
  int32_t exclude_regions_count;
  char *objc_c_include_path;
  char *objc_assembly_query_namespace;
  char *c_include_namespace;
  char *java_class_under_test;
  char *java_package_name;
  char *java_assembly_query_classname;
  char *cqlrt;
  bool_t dev;                           // option use to activate features in development or dev features
} cmd_options;

cql_data_decl( cmd_options options );

#define Invariant assert
#define Contract assert

#define _new(x) ((x*)malloc(sizeof(x)))
#define _new_array(x,c) ((x*)malloc(c*sizeof(x)))

#define CQL_NICE_LITERAL_NAME_LIMIT 32

// note this is not easily changed, storage for used strach variables is in an unsigned long long
#define CQL_MAX_STACK 64

typedef const char *CSTR;

typedef enum cg_symbol_case {
  cg_symbol_case_snake,
  cg_symbol_case_pascal,
  cg_symbol_case_camel,
} cg_symbol_case;

cql_data_decl( const char *global_proc_name );

typedef struct ast_node *ast_ptr;

typedef struct rtdata {
  // the command line name of this result type
  const char *name;

  // The main code generator function that will be executed.
  void (*code_generator)(ast_ptr root);

  // The number of file names required by the rt. Use -1 for a variable number
  // of file names that will be verified by the code generator itself based on
  // the arguments passed t it
  int32_t required_file_names_count;

  // A string to add before any header contents (include copyright, autogen comments, runtime include, etc).
  const char *header_prefix;

  // The default "cqlrt.h" for this code type
  const char *cqlrt;

  // the formatting string into which the filename above is placed
  const char *cqlrt_template;

  // A begin string to wrap the contents of the header file.
  const char *header_wrapper_begin;

  // A end string to wrap the contents of the header file.
  const char *header_wrapper_end;

  // A string to add before any source contents (include copyright, autogen comments, etc).
  const char *source_prefix;

  // A begin string to wrap the contents of the source file.
  const char *source_wrapper_begin;

  // A end string to wrap the contents of the source file.
  const char *source_wrapper_end;

  // A string to add before any import file contents (include copyright, autgen comments, etc).
  const char *exports_prefix;

  // The case to use for symbole.
  cg_symbol_case symbol_case;

  // If enabled, generic type-based getters are used by the generated code, registering the callback function
  // pointers when creating the result set objects.
  bool_t generate_type_getters;

  // If enabled, macros will be generated to test equality between 2 list/index pairs.
  bool_t generate_equality_macros;

  // Called for each proc name that is processed.
  void (*register_proc_name)(const char *proc_name);

  // Test function to determine whether to implicitly generate the copy function for a result set.  The generate_copy
  // command line argument overrides the value, if specified.
  bool_t (*test_proc_generate_copy)(const char *proc_name);

  // Prefix for public symbol.
  const char *symbol_prefix;

  // Prefix for private implementation symbol.
  const char *impl_symbol_prefix;

  // Visibility attribute for generated functions.
  const char *symbol_visibility;

  // Assertion macro for API contract violations.
  const char *cql_contract;

  // Logging database error;
  const char *cql_log_database_error;

  // The type for a hash code.
  const char *cql_hash_code;

  // The type for a boolean value.
  const char *cql_bool;

  // The type for a 32-bit integer value.
  const char *cql_int32;

  // The type for a 64-bit integer value.
  const char *cql_int64;

  // The type for a double value.
  const char *cql_double;

  // The type for a sqlite3 result code.
  const char *cql_code;

  // The type for an object ref.
  const char *cql_object_ref;

  // Adds a reference count to the object.
  // @param obj The  object to be retained.
  // void cql_object_retain(cql_object_ref _Nullable obj);
  const char *cql_object_retain;

  // Subtracts a reference count from the object.  When it reaches 0, the object SHOULD be freed.
  // @param str The object to be released.
  // void cql_object_release(cql_object_ref _Nullable obj);
  const char *cql_object_release;

  // The type for a blob ref.
  const char *cql_blob_ref;

  // Adds a reference count to the blob.
  // @param blob The blob to be retained.
  // void cql_blob_retain(cql_blob_ref _Nullable blob);
  const char *cql_blob_retain;

  // Subtracts a reference count from the blob.  When it reaches 0, the blob SHOULD be freed.
  // @param str The blob to be released.
  // void cql_blob_release(cql_blob_ref _Nullable blob);
  const char *cql_blob_release;

  // The type for a string object.
  const char *cql_string_ref;

  // Construct a new string object.
  // @param cstr The C string to be stored.
  // @return A string object of the type defined by cql_string_ref.
  // cql_string_ref cql_string_ref_new(const char *cstr);
  const char *cql_string_ref_new;

  // Declare a const string literal object.  This must be a global object and will be executed in the global context.
  // NOTE: This MUST be implemented as a macro, as it both declares and assigns the value.
  // @param name The name of the object.
  // @param text The text to be stored in the object.
  // cql_string_literal(cql_string_ref name, const char *text);
  const char *cql_string_literal;

  // Adds a reference count to the string object.
  // @param str The string object to be retained.
  // void cql_string_retain(cql_string_ref _Nullable str);
  const char *cql_string_retain;

  // Subtracts a reference count from the string object.  When it reaches 0, the string SHOULD be freed.
  // @param str The string object to be released.
  // void cql_string_release(cql_string_ref _Nullable str);
  const char *cql_string_release;

  // Creates a hash code for the string object.
  // @param str The string object to be hashed.
  // cql_hash_code cql_string_hash(cql_string_ref _Nullable str);
  const char *cql_string_hash;

  // Creates a hash code for the blob object.
  // @param blob The blob object to be hashed.
  // cql_hash_code cql_blob_hash(cql_string_ref _Nullable str);
  const char *cql_blob_hash;

  // Compares two string objects.
  // @param str1 The first string to compare.
  // @param str2 The second string to compare.
  // @return < 0 if str1 is less than str2, > 0 if str2 is less than str1, = 0 if str1 is equal to str2.
  // int cql_string_compare(cql_string_ref str1, cql_string_ref str2);
  const char *cql_string_compare;

  // Checks if two string objects are equal.
  // NOTE: If both objects are NULL, they are equal; if only 1 is NULL, they are not equal.
  // @param str1 The first string to compare.
  // @param str2 The second string to compare.
  // @return cql_true if they are equal, otherwise cql_false.
  // cql_bool cql_string_equal(cql_string_ref _Nullable str1, cql_string_ref _Nullable str2);
  const char *cql_string_equal;

  // Compares two string objects with SQL LIKE semantics.
  // NOTE: If either object is NULL, the result should be 1.
  // @param str1 The first string to compare.
  // @param str2 The second string to compare.
  // @return 0 if the str1 is LIKE str2, else != 0.
  // int cql_string_like(cql_string_ref str1, cql_string_ref str2);
  const char *cql_string_like;

  // Declare and allocate a C string from a string object.
  // NOTE: This MUST be implemented as a macro, as it both declares and assigns the value.
  // @param cstr The C string var to be declared and assigned.
  // @param str The string object that contains the string value.
  // cql_alloc_cstr(const char *cstr, cql_string_ref str);
  const char *cql_alloc_cstr;

  // Free a C string that was allocated by cql_alloc_cstr
  // @param cstr The C string to be freed.
  // @param str The string object that the C string was allocated from.
  // cql_free_cstr(const char *cstr, cql_string_ref str);
  const char *cql_free_cstr;

  // The type for a generic cql result set.
  // NOTE: Result sets are cast to this type before being passed to the cql_result_set_get_count/_data functions.
  const char *cql_result_set_ref;

  // Construct a new result set object.
  // @param data The data to be stored in the result set.
  // @param count The count of records represented by the data in the result_set.
  // @param columns The number of columns for this result type.
  // @param dataTypes The data types for the columns.
  // @param callbacks The callbacks that are used for the data access.
  // @return A result_set object of the type.
  // cql_result_set_ref _Nonnull cql_result_set_ref_new(
  //     void *_Nonnull data,
  //     cql_int32 count,
  //     void (*_Nonnull teardown)(cql_result_set_ref _Nonnull result_set),
  //     uint8_t *_Nonnull dataTypes,
  //     cql_result_set_meta_struct meta);
  const char *cql_result_set_ref_new;

  // The name of the struct for all of the metadata passed to cql_result_set_ref_new.  The struct must have the
  // following fields, by name.  Any additional fields may be added for internal support of the runtime.
  //
  //   teardown:    void (*_Nullable)(cql_result_set_ref _Nonnull result_set)
  //   copy:        void (*_Nullable)(
  //                    cql_result_set_ref _Nonnull result_set,
  //                    cql_result_set_ref _Nullable *_Nonnull to_result_set,
  //                    cql_int32 from,
  //                    cql_int32 count)
  //   refOffsets:  unsigned short *refOffsets (offsets to all of the references in a row)
  //   rowsize:     size_t rowsize  (the size of each row in bytes)
  //
  // The following getter callbacks are only used when generate_type_getters is true.
  //   getBoolean:  Boolean (*_Nullable)(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col)
  //   getDouble:   double (*_Nullable)(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col)
  //   getInt32:    int32_t (*_Nullable)(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col)
  //   getInt64:    int64_t (*_Nullable)(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col)
  //   getString:   MCFStringRef _Nonnull (*_Nullable)(
  //                    cql_result_set_ref _Nonnull result_set,
  //                    cql_int32 row,
  //                    cql_int32 col)
  //   getIsNull:   Boolean (*_Nullable)(cql_result_set_ref _Nonnull result_set, cql_int32 row, cql_int32 col)
  const char *cql_result_set_meta_struct;

  // The name of the method that will give the metadata struct back as provided to the construction above
  const char *cql_result_set_get_meta;

  // Adds a reference count to the result_set object.
  // NOTE: This MUST be implemented as a macro, as it takes a result set as a param, which has an undefined type.
  // @param result_set The result set object to be retained.
  // void cql_result_set_retain(** _Nullable result_set);
  const char *cql_result_set_retain;

  // Subtracts a reference count from the result_set object.  When it reaches 0, the result_set SHOULD be freed.
  // NOTE: This MUST be implemented as a macro, as it takes a result set as a param, which has an undefined type.
  // @param result_set The result set object to be released.
  // void cql_result_set_release(** _Nullable result_set);
  const char *cql_result_set_release;

  // Get the count of the query data.
  // NOTE: This MUST be implemented as a macro, as it takes a result set as a param, which has an undefined type.
  // @param result_set The cql result set object.
  // @return The count that was previous stored on the result set.
  // cql_int32 cql_result_set_get_count(** result_set);
  const char *cql_result_set_get_count;

  // Retrieve the storage of the query data.
  // NOTE: This MUST be implemented as a macro, as it takes a result set as a param, which has an undefined type.
  // @param result_set The cql result_set object.
  // @return The data that was previous stored on the result set.
  // void *cql_result_set_get_data(** result_set)
  const char *cql_result_set_get_data;

  // Generic bool value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The bool value.
  // cql_bool cql_result_set_get_bool(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_bool;

  // Generic double value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The double value.
  // cql_double cql_result_set_get_double(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_double;

  // Generic int32 value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The int32 value.
  // cql_int32 cql_result_set_get_int32(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_int32;

  // Generic int64 value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The int64 value.ali
  // cql_int64 cql_result_set_get_int64(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_int64;

  // Generic string value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The string value.
  // cql_string_ref _Nullable cql_result_set_get_string(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_string;

  // Generic blob value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return The string value.
  // cql_blob_ref _Nullable cql_result_set_get_blob(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_blob;

  // Generic is_null value getter on base result set object.
  // NOTE: This is only used when generate_type_getters is true.  This function should call through to the
  // inline type getters that are passed into the ctor for the result set.
  // @param result_set The cql result_set object.
  // @param row The row number to fetch the value for.
  // @param col The column to fetch the value for.
  // @return cql_true if the value is null, otherwise cql_false.
  // cql_bool cql_result_set_get_is_null(cql_result_set_ref result_set, int32_t row, int32_t col)
  const char *cql_result_set_get_is_null;

  // The java type for a nullable boolean value.
  const char *cql_bool_nullable;
  // The java type for a nullable int value.
  const char *cql_int32_nullable;
  // The java type for a nullable long value.
  const char *cql_int64_nullable;
  // The java type for a nullable double value.
  const char *cql_double_nullable;
  // Template for the java class definition.
  const char *cql_java_tmp_class_def;
  // Template for the java method hasIdentityColumns.
  const char *cql_result_set_has_identity_columns;
  // Template for the java method copy.
  const char *cql_result_set_copy;
  // Template for the java class contructor.
  const char *cql_java_tmp_class_constructor;
  // Template for the java getter function return a nullable value.
  const char *cql_java_tmp_getter_nullable;
} rtdata;

cql_data_decl( rtdata *rt );

cql_noexport void cql_cleanup_and_exit(int32_t code);

// output to "stderr"
cql_noexport void cql_error(const char *format, ...);

// output to "stdout"
cql_noexport void cql_output(const char *format, ...);

// Creates a file in write mode. Aborts if there's any error.
cql_export FILE *cql_open_file_for_write(CSTR file_name);

// Create file, write the data to it, and close the file
cql_export void cql_write_file(const char *file_name, const char *data);

cql_noexport void line_directive(const char *directive);

cql_export void cql_emit_error(const char *err);

cql_export void cql_emit_output(const char *out);

cql_data_decl( char *current_file );

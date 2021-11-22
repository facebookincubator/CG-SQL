/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "TestResult.h"
#include "cqlrt.h"
#include "Sample.h"

static sqlite3 * _db;

/*
 * Class:     TestResult
 * Method:    open
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_TestResult_open(JNIEnv *env, jclass thiz) {
  return sqlite3_open(":memory:", &_db);
}

/*
 * Class:     TestResult
 * Method:    getTestResult
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_TestResult_getTestResult(JNIEnv *env, jclass thiz) {
  Sample_result_set_ref result_set;
  cql_code rc = Sample_fetch_results(_db, &result_set);
  if (rc) return 0;
  return (jlong)result_set;
}

/*
 * Class:     TestResult
 * Method:    close
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_TestResult_close(JNIEnv *env, jclass thiz) {
  sqlite3_close(_db);
  _db = NULL;
}

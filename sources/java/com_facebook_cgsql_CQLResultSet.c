/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "com_facebook_cgsql_CQLResultSet.h"
#include "cqlrt.h"

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    close
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_com_facebook_cgsql_CQLResultSet_close
  (JNIEnv *env, jobject thiz, jlong rs) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);
  cql_result_set_release(ref);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getBoolean
 * Signature: (JII)Z
 */
JNIEXPORT jboolean JNICALL Java_com_facebook_cgsql_CQLResultSet_getBoolean
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_bool_col(ref, row, col);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getInteger
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL Java_com_facebook_cgsql_CQLResultSet_getInteger
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_int32_col(ref, row, col);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getLong
 * Signature: (JII)J
 */
JNIEXPORT jlong JNICALL Java_com_facebook_cgsql_CQLResultSet_getLong
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_int64_col(ref, row, col);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getString
 * Signature: (JII)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_facebook_cgsql_CQLResultSet_getString
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  cql_string_ref str = cql_result_set_get_string_col(ref, row, col);
  cql_alloc_cstr(c_str, str);
  jstring result =  (*env)->NewStringUTF(env, c_str);
  cql_free_cstr(c_str, str);
  return result;
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getDouble
 * Signature: (JII)D
 */
JNIEXPORT jdouble JNICALL Java_com_facebook_cgsql_CQLResultSet_getDouble
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_double_col(ref, row, col);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getBlob
 * Signature: (JII)[B
 */
JNIEXPORT jbyteArray JNICALL Java_com_facebook_cgsql_CQLResultSet_getBlob
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  cql_blob_ref blob = cql_result_set_get_blob_col(ref, row, col);
  cql_uint32 size = cql_get_blob_size(blob);
  const void *bytes = cql_get_blob_bytes(blob);

  jbyteArray ret = (*env)->NewByteArray(env, size);
  (*env)->SetByteArrayRegion (env, ret, 0, size, bytes);
  return ret;
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    isNull
 * Signature: (JII)Z
 */
JNIEXPORT jboolean JNICALL Java_com_facebook_cgsql_CQLResultSet_isNull
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_is_null_col(ref, row, col);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getCount
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_com_facebook_cgsql_CQLResultSet_getCount
  (JNIEnv *env, jobject thiz, jlong rs) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_result_set_get_count(ref);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    rowHashCode
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL Java_com_facebook_cgsql_CQLResultSet_rowHashCode
  (JNIEnv *env, jobject thiz, jlong rs, jint row) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);

  return cql_row_hash(ref, row);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    rowsEqual
 * Signature: (JIJI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_facebook_cgsql_CQLResultSet_rowsEqual
  (JNIEnv *env, jobject thiz, jlong rs1, jint row1, jlong rs2, jint row2) {

  cql_result_set_ref ref1 = (cql_result_set_ref)(rs1);
  cql_result_set_ref ref2 = (cql_result_set_ref)(rs2);

  return cql_rows_equal(ref1, row1, ref2, row2);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    rowsSame
 * Signature: (JIJI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_facebook_cgsql_CQLResultSet_rowsSame
  (JNIEnv *env, jobject thiz, jlong rs1, jint row1, jlong rs2, jint row2) {

  cql_result_set_ref ref1 = (cql_result_set_ref)(rs1);
  cql_result_set_ref ref2 = (cql_result_set_ref)(rs2);

  return cql_rows_same(ref1, row1, ref2, row2);
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    copy
 * Signature: (JII)J
 */
JNIEXPORT jlong JNICALL Java_com_facebook_cgsql_CQLResultSet_copy
  (JNIEnv *env, jobject thiz, jlong rs, jint row, jint count) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);
  cql_result_set_ref refNew = NULL;
  cql_rowset_copy(ref, &refNew, row, count);
  return (jlong)refNew;
}

/*
 * Class:     com_facebook_cgsql_CQLResultSet
 * Method:    getIsEncoded
 * Signature: (JI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_facebook_cgsql_CQLResultSet_getIsEncoded
  (JNIEnv *env, jobject thiz, jlong rs, jint col) {
  cql_result_set_ref ref = (cql_result_set_ref)(rs);
  return cql_result_set_get_is_encoded_col(ref, col);
}

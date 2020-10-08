/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.cgsql;

/**
 * CQLResultSet is a simple utility class that holds a native cql_result_set_ref
 *
 * <p><b>YOU CANNOT USE THIS CLASS DIRECTLY</b>
 *
 * <p>This class is only meant to be used directly by generated code, so any other code that depends
 * on this class directly is considered invalid and could break anytime without further notice.
 */
public final class CQLResultSet {
  static {
    System.loadLibrary("CQLResultSet");
  }

  private long result_set_ref;

  public CQLResultSet(long result_set_ref_) {
    result_set_ref = result_set_ref_;
  }

  public Boolean getNullableBoolean(int row, int column) {
    if (isNull(row, column)) {
      return null;
    }
    return getBoolean(row, column);
  }

  public Integer getNullableInteger(int row, int column) {
    if (isNull(row, column)) {
      return null;
    }
    return getInteger(row, column);
  }

  public Long getNullableLong(int row, int column) {
    if (isNull(row, column)) {
      return null;
    }
    return getLong(row, column);
  }

  public Double getNullableDouble(int row, int column) {
    if (isNull(row, column)) {
      return null;
    }
    return getDouble(row, column);
  }

  public void close() {
    if (result_set_ref != 0) {
      close(result_set_ref);
      result_set_ref = 0;
    }
  }

  public boolean getBoolean(int row, int column) {
    return getBoolean(result_set_ref, row, column);
  }

  public int getInteger(int row, int column) {
    return getInteger(result_set_ref, row, column);
  }

  public long getLong(int row, int column) {
    return getLong(result_set_ref, row, column);
  }

  public String getString(int row, int column) {
    return getString(result_set_ref, row, column);
  }

  public double getDouble(int row, int column) {
    return getDouble(result_set_ref, row, column);
  }

  public byte[] getBlob(int row, int column) {
    return getBlob(result_set_ref, row, column);
  }

  public boolean isNull(int row, int column) {
    return isNull(result_set_ref, row, column);
  }

  public int getCount() {
    return getCount(result_set_ref);
  }

  public long rowHashCode(int row) {
    return rowHashCode(result_set_ref, row);
  }

  public boolean rowsEqual(int row1, CQLResultSet rs2, int row2) {
    return rowsEqual(result_set_ref, row1, rs2.result_set_ref, row2);
  }

  public boolean rowsSame(int row1, CQLResultSet rs2, int row2) {
    return rowsSame(result_set_ref, row1, rs2.result_set_ref, row2);
  }

  public CQLResultSet copy(int row, int count) {
    return new CQLResultSet(copy(result_set_ref, row, count));
  }

  // native calls
  public native void close(long result_set_ref);

  public native boolean getBoolean(long result_set_ref, int row, int column);

  public native int getInteger(long result_set_ref, int row, int column);

  public native long getLong(long result_set_ref, int row, int column);

  public native String getString(long result_set_ref, int row, int column);

  public native double getDouble(long result_set_ref, int row, int column);

  public native byte[] getBlob(long result_set_ref, int row, int column);

  public native boolean isNull(long result_set_ref, int row, int column);

  public native int getCount(long result_set_ref);

  public native long rowHashCode(long result_set_ref, int row);

  public native boolean rowsEqual(long result_set_ref, int row1, long rs2, int row2);

  public native boolean rowsSame(long result_set_ref, int row1, long rs2, int row2);

  public native long copy(long result_set_ref, int row, int count);
}

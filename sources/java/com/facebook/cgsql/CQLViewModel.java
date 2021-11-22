/*
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.cgsql;

/**
 * Super class extended by all CQL based view models.
 *
 * <p><b>YOU CANNOT USE THIS CLASS DIRECTLY</b>
 *
 * <p>This class is only meant to be used directly by generated code, so any other code that depends
 * on this class directly is considered invalid and could break anytime without further notice.
 */
public abstract class CQLViewModel {
  protected CQLResultSet mResultSet;

  public CQLViewModel(CQLResultSet resultSet) {
    mResultSet = resultSet;
  }

  public long rowHashCode(int row) {
    return mResultSet.rowHashCode(row);
  }

  public boolean rowsEqual(int row1, CQLViewModel rs2, int row2) {
    return mResultSet.rowsEqual(row1, rs2.mResultSet, row2);
  }

  public boolean rowsSame(int row1, CQLViewModel rs2, int row2) {
    if (!hasIdentityColumns()) {
      return false;
    }
    return mResultSet.rowsSame(row1, rs2.mResultSet, row2);
  }

  protected abstract boolean hasIdentityColumns();
}

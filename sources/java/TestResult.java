/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public final class TestResult {
  static {
    System.loadLibrary("TestResult");
  }

  public static native int open();

  public static native long getTestResult();

  public static native void close();
}

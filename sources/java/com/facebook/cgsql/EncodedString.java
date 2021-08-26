/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.cgsql;

/**
 * EncodedString is a simple class that incapsulate the encoded string from a ResultSet to restrict
 * and control its access
 *
 * <p>It has no getter for the purpose to hide the encoded string.
 *
 * <p>Use case: You could add a method to this class that takes as parameter a TextView and write a
 * decoded value of the encoded string directly to the TextView. Like that only the TextView get to
 * have the decode value of the encoded string.
 */
public class EncodedString {
  public String mValue;

  public EncodedString(String value) {
    mValue = value;
  }
}

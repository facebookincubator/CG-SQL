/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import com.facebook.cgsql.CQLResultSet;
import java.nio.charset.StandardCharsets;
import sample.*;

public class CGSQLMain {
  public static void main(String[] args) {
    TestResult.open();

    // get result set handle
    long handle = TestResult.getTestResult();

    // make the sample result set
    Sample data = new Sample(new CQLResultSet(handle));

    // use the results
    dumpResults(data);

    // release the connection
    TestResult.close();
  }

  public static void dumpResults(Sample data) {
    System.out.println("Dumping Results");
    int count = data.getCount();
    System.out.println(String.format("count = %d", count));

    for (int i = 0; i < count; i++) {
      byte[] bytes = data.getBytes(i);
      String s = new String(bytes, StandardCharsets.UTF_8);
      System.out.println(
          String.format(
              "Row %d: %s %s %d %f", i, data.getName(i), s, data.getAge(i), data.getThing(i)));
    }
  }
}

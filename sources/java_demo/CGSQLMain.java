/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
    Sample.SampleViewModel data = new Sample.SampleViewModel(new CQLResultSet(handle));

    // use the results
    dumpResults(data);

    // release the connection
    TestResult.close();
  }

  public static void dumpResults(Sample.SampleViewModel data) {
    System.out.println("Dumping Results");
    int count = data.getCount();
    System.out.println(String.format("count = %d", count));

    for (int i = 0; i < count; i++) {
      byte[] bytes = data.get_bytes(i);
      String s = new String(bytes, StandardCharsets.UTF_8);
      System.out.println(
          String.format(
              "Row %d: name:%s blob:%s age:%d(encoded = %s) thing:%f key1:%s key2:%s(encoded = %s)",
              i,
              data.get_name(i),
              s,
              data.get_age(i),
              Boolean.toString(data.get_age_IsEncoded()),
              data.get_thing(i),
              data.get_key1(i),
              data.get_key2(i),
              Boolean.toString(data.get_key2_IsEncoded())));

      Sample.ChildViewModel child = new Sample.ChildViewModel(data.get_my_child_result(i));
      for (int j = 0; j < child.getCount(); j++) {
        System.out.println(
            String.format("--> Child Row %d: x:%d y:%s", j, child.get_x(j), child.get_y(j)));
      }
    }
  }
}

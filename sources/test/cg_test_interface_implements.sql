/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@attribute(cql:java_package="com.facebook.cqlviewmodels")
declare interface user_messages (
  user INT,
  message TEXT
);

@attribute(cql:implements=user_messages)
create proc facebook_user_messages(
  user_ INT,
  message_ TEXT
)
BEGIN
  SELECT user_ user, message_ message;
END;

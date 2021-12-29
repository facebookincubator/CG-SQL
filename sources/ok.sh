#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OUT_DIR="out"
TEST_DIR="test"

copy_extras() {
  echo "no extra files to copy at this time"
}

# shellcheck disable=SC1091
source common/ok_common.sh || exit 1

#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC1091
source common/cov_common.sh || exit 1

OUT_DIR="out"

if ! coverage gcovr
then
  echo "A coverage step failed, aborting"
  exit 1
fi

cat "${OUT_DIR}/report.txt"
exit 0

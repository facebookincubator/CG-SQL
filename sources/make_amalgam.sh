#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

process_license() {

cat <<EOF
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

EOF
}

process_extra_h_files() {
  echo // no extra .h files
}

process_extra_c_files() {
  echo // no extra .c files
}

# shellcheck disable=SC1091
source common/amalgam_common.sh || exit 1

create_amalgam

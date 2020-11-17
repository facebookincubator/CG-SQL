#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OUT_DIR="out"
TEST_DIR="test"
CQL="./${OUT_DIR}/cql"

# shellcheck disable=SC2034
ERROR_DOC="../CQL_Guide/x4.md"

# shellcheck disable=SC1091
source common/test_helpers.sh || exit 1

while [ "$1" != "" ]
do
  if [ "$1" == "--coverage" ]
  then
     MAKE_COVERAGE_ARGS="COVERAGE=1"
     TEST_COVERAGE_ARGS="--coverage"
     shift 1
  elif [ "$1" == "--use_amalgam" ]
  then
     CQL=${OUT_DIR}/cql_amalgam
     shift 1
     USE_AMALGAM=1
  else
     echo "Usage: test.sh [--coverage] [--use_amalgam]"
     exit 1
  fi
done

# no extra tests
extra_tests() {
  echo "no extra tests at this time"
}

# shellcheck disable=SC1091
source common/test_common.sh || exit 1

#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail

CQL_SOURCES=./sources
GRAMMAR_DOCS=${CQL_SOURCES}/grammar_docs
CQL_GUIDE=./CQL_Guide
DIAGRAMS=./diagrams

# shellcheck disable=SC1091
source common/update_docs.sh || exit 1

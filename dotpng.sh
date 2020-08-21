#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

tmpfile=$(mktemp /tmp/cql.dot.output)
./out/cql --dot < $1 >  tmpfile
dot tmpfile -Tpng -o /out/$1.png
open /out/$1.png

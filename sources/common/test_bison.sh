#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Checking for the super common case of the XCode version of Bison
# This could be generalized obviously... but this one deserves special treatment

echo "Testing bison version of " "$1"
$1 -V
if [ -n "$($1 -V | grep ' 2[.]3')" ] ; then
 echo Bison 2.3 detected, this is too old.  Use brew install bison to update. See README.md
 exit 91
fi

exit 0

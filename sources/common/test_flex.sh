#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Checking for the super common case of the XCode version of Flex
# This could be generalized obviously... but this one deserves special treatment

echo "Testing flex version of " "$1"
$1 -V
if [ -n "$($1 -V | grep ' 2[.]5')" ] ; then
 echo "Flex 2.5 detected, this is too old.  Use brew install flex to update. See README.md"
 exit 90
fi

exit 0

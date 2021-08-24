#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Checking for the super common case of the XCode version of Flex
# This could be generalized obviously... but this one deserves special treatment

if [ -n "$($1 -V | grep ' 2[.]5')" ] ; then
 echo Flex 2.5 detected, this is too old.  Use brew install flex to update. See README.md
 exit 1
fi

exit 0

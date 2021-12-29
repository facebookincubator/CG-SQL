#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import sys

data = json.load(sys.stdin)
json_data = json.dumps(data, indent=2)
print(json_data)

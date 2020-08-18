#!/usr/bin/env python3
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import json
import sys

data = json.load(sys.stdin)
json_data = json.dumps(data, indent=2)
print(json_data)

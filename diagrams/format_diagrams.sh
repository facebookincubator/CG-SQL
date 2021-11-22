#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

function move_font {
    file_name=$1
    sed -e '/font: normal 12px Verdana, sans-serif;/d' -e 's/a.palette/p,li { font: normal 12px Verdana, sans-serif; } a.palette/g' < "$file_name".html > "$file_name".thtml
    mv "$file_name".thtml "$file_name".html
}


move_font 'railroad_diagram'
move_font 'json_output_railroad_diagram'

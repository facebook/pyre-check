#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Source this file to run it:
# . ./setup.sh

pip install -r requirements.txt
rm ../../.pyre_configuration
echo '{
    "source_directories": ["."],
    "search_path": "../../stubs",
    "taint_models_path": "../../stubs"
}' > ./.pyre_configuration

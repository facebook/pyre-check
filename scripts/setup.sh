#!/bin/bash

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Switch to pyre directory.
cd "$(dirname "$0")/.."

./scripts/setup.py "${@}"

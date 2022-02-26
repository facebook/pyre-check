#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Switch to pyre directory.
cd "$(dirname "$0")/.."

./scripts/setup.py "${@}"

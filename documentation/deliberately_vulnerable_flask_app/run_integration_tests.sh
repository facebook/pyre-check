#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

cd "$(dirname "$0")" || exit
exec python3 ../../tools/pysa_integration_tests/run.py \
    --skip-model-verification \
    --run-from-source \
    --ignore-positions \
    --write-actual-results-on-failure \
    "$@"

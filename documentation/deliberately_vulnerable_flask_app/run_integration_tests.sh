#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set +e
python3 ../../tools/pysa_integration_tests/run.py \
    --skip-model-verification \
    --run-from-source

exit_code=$?

if [[ "$exit_code" != "0" ]]; then
    echo "--- raw_results.json --"
    cat raw_results.json
    echo "---"
fi

exit $exit_code

#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

SCRIPTS_DIRECTORY="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "${SCRIPTS_DIRECTORY}/../.."

echo '  Enumerating test files:'
files=$(find 'pyre-check/client' -name '*_test.py' ! -name 'watchman_test.py')
echo "${files}"
if [[ -z "${files}" ]]; then
  echo 'No test files found, exiting.'
  exit 2
fi

echo '  Running all tests:'
echo "${files}" | sed 's/.py$//' | sed 's:/:.:g' | xargs python -m unittest -v

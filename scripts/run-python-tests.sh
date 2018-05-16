#!/bin/bash

# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

if ! which python3 &>/dev/null; then
   echo 'Could not find a python3 interpreter to run tests'
   exit 2
fi

echo "  Using interpreter at $(which python3) with version: $(python3 --version)"

echo '  Enumerating test files:'
files=$(find client -name '*_test.py')
echo "${files}"
if [[ -z "${files}" ]]; then
  echo 'No test files found, exiting.'
  exit 2
fi

echo '  Running all tests:'
echo "${files}" | sed 's/.py$//' | sed 's:/:.:g' | xargs python3 -m unittest -v

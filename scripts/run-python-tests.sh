#!/bin/bash

# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

# Prefer a python3.5 interpreter to maintain full backward compatibility.
if command -v python3.5 &>/dev/null; then
  PYTHON_INTERPRETER=$(command -v python3.5)
elif command -v python3 &>/dev/null; then
  PYTHON_INTERPRETER=$(command -v python3)
else
  echo 'Could not find a suitable python interpreter to run tests.'
  exit 2
fi

echo "  Using interpreter at ${PYTHON_INTERPRETER} with version: $(${PYTHON_INTERPRETER} --version)"

echo '  Enumerating test files:'
files=$(find client -name '*_test.py')
echo "${files}"
if [[ -z "${files}" ]]; then
  echo 'No test files found, exiting.'
  exit 2
fi

echo '  Running all tests:'
echo "${files}" | sed 's/.py$//' | sed 's:/:.:g' | xargs "${PYTHON_INTERPRETER}" -m unittest -v

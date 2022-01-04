#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

SCRIPTS_DIRECTORY="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# For open-source tests to work, we have to run them from one directory
# higher than the project root, so that the project root is treated as a
# valid package for relative imports.
#
# We also want to be sure the tests work regardless of the project directory name
# (particularly because both `pyre-check` and `pyre` are sometimes used).
cd "${SCRIPTS_DIRECTORY}/.."
ROOT_DIRECTORY="$(pwd)"
ROOT_DIRECTORY_BASE="$(basename "${ROOT_DIRECTORY}")"
cd ..

files=$(find "${ROOT_DIRECTORY_BASE}/client" -name '*_test.py' ! -name 'watchman_test.py')
echo "Found these test files:
${files}
---"
if [[ -z "${files}" ]]; then
  echo 'No test files found, exiting.'
  exit 2
fi

echo ' Running all tests:'
echo "${files}" | xargs testslide

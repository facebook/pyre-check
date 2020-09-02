#!/usr/bin/env bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -x
set -e

ROOT="$(mktemp -d)"
# Only the "client" subdirectory includes files we want to check.
cp -a client "${ROOT}"
pushd "${ROOT}"
TERM=dumb pyre --source-directory . --search-path "${VIRTUAL_ENV}/lib/"python*/site-packages --strict check
popd

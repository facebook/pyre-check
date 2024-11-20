#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -ex

# clone upstream so we can copy code in
rm -rf /tmp/oss-python-typing
git clone https://github.com/python/typing.git /tmp/oss-python-typing

# set up the source tree for (internal) pyre
find third_party -type f -name "*.py" -exec rm {} +
find third_party -type f -name "*.pyi" -exec rm {} +
mkdir -p third_party

# copy over all the python files
cp /tmp/oss-python-typing/conformance/tests/* third_party/

# clean up
rm -rf /tmp/oss-python-typing

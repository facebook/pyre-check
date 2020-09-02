#!/usr/bin/env bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -x
set -e

pip install pyre-check black==19.3b0 pywatchman psutil ipython libcst
pyre --version
black --version

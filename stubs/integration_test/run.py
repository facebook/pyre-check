#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import sys

directory = os.path.dirname(sys.argv[0])
script_name = os.path.basename(sys.argv[0])

if directory != ".":
    os.chdir(directory)

os.execv(
    sys.executable,
    [
        script_name,
        "../../tools/pysa_integration_tests/run.py",
        "--require-pyre-env",
        "--check-invariants",
        "--inline-decorators",
        "--typeshed", "../typeshed/typeshed",
    ]
    + sys.argv[1:],
)

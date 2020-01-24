#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import tempfile
import unittest
from pathlib import Path

from ..build_pypi_package import add_init_files, valid_version


class TestArgumentValidationMethods(unittest.TestCase):
    def test_validate_version(self) -> None:
        self.assertEqual(valid_version("0.0.01"), "0.0.01")
        with self.assertRaises(ValueError):
            valid_version("x0.0.01")


class TestCreatingWheel(unittest.TestCase):
    def test_setup_skeleton(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            add_init_files(build_root)
            # Assert the expected __init__ files are present
            path = Path(build_root)
            self.assertEqual(
                [str(path) for path in path.glob("**/*.py")],
                [
                    build_root + "/pyre_check/__init__.py",
                    build_root + "/pyre_check/tools/__init__.py",
                    build_root + "/pyre_check/tools/upgrade/__init__.py",
                ],
            )

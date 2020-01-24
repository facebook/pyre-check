#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..build_pypi_package import valid_version


class TestArgumentValidationMethods(unittest.TestCase):
    def test_validate_version(self) -> None:
        self.assertEqual(valid_version("0.0.01"), "0.0.01")
        with self.assertRaises(ValueError):
            valid_version("x0.0.01")

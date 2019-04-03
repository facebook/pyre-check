# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import mock_open, patch

from .. import Parser
from ..build_target import PythonBinary


TARGETS_FILE_1 = """
load("@fbcode_macros//build_defs:python_binary.bzl", "python_binary")
load("@fbcode_macros//build_defs:python_library.bzl", "python_library")
load("@fbcode_macros//build_defs:python_unittest.bzl", "python_unittest")

python_binary(
    name = "binary_target",
    main_module = "my.module.main",
    deps = [
        ":other_target",
        "//another/project:target",
    ],
)

python_binary(
    name = "other_target",
    main_module = "my.module.other",
    deps = [],
)

"""


class ParserTest(unittest.TestCase):
    def test_parse_file(self):
        parser = Parser("/buck_root")

        with patch("builtins.open", mock_open(read_data=TARGETS_FILE_1)):
            result = parser.parse_file("my/module")

            self.assertEqual(result.path, "my/module")
            self.assertEqual(len(result.targets), 2)

            target = result.targets["binary_target"]
            self.assertIsInstance(target, PythonBinary)
            self.assertEqual(target.target, "//my/module:binary_target")
            self.assertListEqual(
                target.dependencies,
                ["//my/module:other_target", "//another/project:target"],
            )

            target = result.targets["other_target"]
            self.assertIsInstance(target, PythonBinary)
            self.assertEqual(target.target, "//my/module:other_target")
            self.assertListEqual(target.dependencies, [])

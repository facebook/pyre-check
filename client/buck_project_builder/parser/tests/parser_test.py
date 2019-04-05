# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import mock_open, patch

from .. import Parser, ParserException
from ..build_target import (
    BuildTarget,
    Glob,
    PythonBinary,
    PythonLibrary,
    PythonUnitTest,
)


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

python_library(
    name = "library_target",
    base_module = "foo.bar",
    srcs = ["a.py", "b.py"],
)

python_unittest(
    name = "test_target",
    srcs = glob(["tests/*.py"]),
)

cpp_python_extension(
    name = "cpp_python_target",
    deps = [":other_target"],
)
"""

TARGETS_FILE_2 = """
python_binary(
    name = 1234,
)
"""


class ParserTest(unittest.TestCase):
    def test_parse_file(self):
        parser = Parser("/buck_root")

        with patch("builtins.open", mock_open(read_data=TARGETS_FILE_1)) as mocked_open:
            result = parser.parse_file("my/module")
            mocked_open.assert_called_once_with("/buck_root/my/module/TARGETS", "r")
            mocked_open.reset_mock()

            self.assertEqual(result.path, "my/module")
            self.assertEqual(len(result.targets), 5)

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

            target = result.targets["library_target"]
            self.assertIsInstance(target, PythonLibrary)
            self.assertEqual(target.target, "//my/module:library_target")
            self.assertListEqual(target.sources.files, ["a.py", "b.py"])
            self.assertListEqual(target.sources.globs, [])
            self.assertListEqual(target.dependencies, [])

            target = result.targets["test_target"]
            self.assertIsInstance(target, PythonUnitTest)
            self.assertEqual(target.target, "//my/module:test_target")
            self.assertListEqual(target.sources.files, [])
            self.assertListEqual(target.sources.globs, [Glob(["tests/*.py"], [])])
            self.assertListEqual(target.dependencies, [])

            target = result.targets["cpp_python_target"]
            self.assertEqual(target.target, "//my/module:cpp_python_target")
            self.assertEqual(target.dependencies, [])

            # The parser should cache files it has already parsed.
            parser.parse_file("my/module")
            mocked_open.assert_not_called()

        with patch("builtins.open", mock_open(read_data=TARGETS_FILE_2)) as mocked_open:
            self.assertRaises(ParserException, parser.parse_file, "my/other_module")
            mocked_open.assert_called_once_with(
                "/buck_root/my/other_module/TARGETS", "r"
            )

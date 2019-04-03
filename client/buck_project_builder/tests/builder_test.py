# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from typing import List
from unittest.mock import MagicMock, patch

from .. import Builder, parser
from ..parser.build_target import BuildTarget, PythonBinary, PythonLibrary


class BuilderTest(unittest.TestCase):
    def assert_targets_equal(
        self, targets: List[BuildTarget], target_names: List[str]
    ) -> None:
        computed_target_names = sorted(target.target for target in targets)
        expected_target_names = sorted(target_names)
        self.assertListEqual(computed_target_names, expected_target_names)

    def test_compute_targets_to_build_simple(self):
        # Dependency graph:
        #    a
        #  /  \
        # b <- c
        # |  /
        # d      e
        build_file = MagicMock()
        build_file.targets = {
            "a": PythonBinary("project", "a", ["//project:b", "//project:c"]),
            "b": PythonLibrary("project", "b", ["//project:d"], []),
            "c": PythonLibrary("project", "c", ["//project:b", "//project:d"], []),
            "d": PythonLibrary("project", "d", [], []),
            "e": PythonLibrary("project", "e", [], []),
        }

        with patch.object(parser.Parser, "parse_file", return_value=build_file):
            builder = Builder("/ROOT")

            targets = builder.compute_targets_to_build("//project:a")
            self.assert_targets_equal(
                targets, ["//project:a", "//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build("//project:b")
            self.assert_targets_equal(targets, ["//project:b", "//project:d"])

            targets = builder.compute_targets_to_build("//project:c")
            self.assert_targets_equal(
                targets, ["//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build("//project:d")
            self.assert_targets_equal(targets, ["//project:d"])

            targets = builder.compute_targets_to_build("//project:e")
            self.assert_targets_equal(targets, ["//project:e"])

            self.assertRaises(
                ValueError, builder.compute_targets_to_build, "//project:f"
            )

    def test_compute_targets_to_build_complex(self):
        # Dependency graph:
        #    a
        #  /  \
        # b    c
        # |    |
        # d <- e
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary("project1", "a", ["//project1:b", "//project2:c"]),
            "b": PythonLibrary("project1", "b", ["//project2:d"], []),
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("project2", "c", ["//project2:e"], []),
            "d": PythonLibrary("project2", "d", [], []),
            "e": PythonLibrary("project2", "e", ["//project2:d"], []),
        }
        build_file_mapping = {"project1": build_file_1, "project2": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = Builder("/ROOT")

            targets = builder.compute_targets_to_build("//project1:a")
            self.assert_targets_equal(
                targets,
                [
                    "//project1:a",
                    "//project1:b",
                    "//project2:c",
                    "//project2:d",
                    "//project2:e",
                ],
            )

            targets = builder.compute_targets_to_build("//project1:b")
            self.assert_targets_equal(targets, ["//project1:b", "//project2:d"])

            targets = builder.compute_targets_to_build("//project2:c")
            self.assert_targets_equal(
                targets, ["//project2:c", "//project2:e", "//project2:d"]
            )

            targets = builder.compute_targets_to_build("//project2:d")
            self.assert_targets_equal(targets, ["//project2:d"])

            targets = builder.compute_targets_to_build("//project2:e")
            self.assert_targets_equal(targets, ["//project2:e", "//project2:d"])

            self.assertRaises(
                ValueError, builder.compute_targets_to_build, "//project1:f"
            )

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import unittest
from textwrap import dedent
from typing import List

from ..filesystem import Target, TargetCollector


class FilesystemTest(unittest.TestCase):
    def assert_collector(
        self, source: str, expected_targets: List[Target], pyre_only: bool
    ) -> None:
        target_collector = TargetCollector(pyre_only)
        tree = ast.parse(dedent(source))
        target_collector.visit(tree)
        targets = target_collector.result()
        self.assertEqual(expected_targets, targets)

    def test_target_collector(self) -> None:
        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_targets = []
        self.assert_collector(source, expected_targets, False)

        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = False,
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_targets = [Target("target_name", strict=False, pyre=True)]
        self.assert_collector(source, expected_targets, False)

        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_targets = [
            Target("target_name", strict=False, pyre=True),
            Target("test_target_name", strict=False, pyre=True),
        ]
        self.assert_collector(source, expected_targets, False)

        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_targets = [Target("target_name", strict=False, pyre=True)]
        self.assert_collector(source, expected_targets, True)

        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "strict, mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_targets = [Target("target_name", strict=False, pyre=True)]
        self.assert_collector(source, expected_targets, True)

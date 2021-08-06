# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap
import unittest
from typing import List

import libcst as cst

from ..commands.coverage import _collect_coverage


class CoverageTest(unittest.TestCase):
    def assert_coverage_equal(
        self,
        file_content: str,
        expected_covered: List[int],
        expected_uncovered: List[int],
    ) -> None:
        module = cst.parse_module(textwrap.dedent(file_content).strip())
        actual_coverage = _collect_coverage({"test.py": module})[0]
        self.assertEqual(expected_covered, actual_coverage.covered_lines)
        self.assertEqual(expected_uncovered, actual_coverage.uncovered_lines)

    def test_coverage_covered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo() -> int:
                return 5
            """,
            expected_covered=[1, 2],
            expected_uncovered=[],
        )

    def test_coverage_uncovered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo():
                return 5
            """,
            expected_covered=[],
            expected_uncovered=[1, 2],
        )

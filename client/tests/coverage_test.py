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
        self.assertEqual(
            expected_covered, actual_coverage.covered_lines, "Covered mismatch"
        )
        self.assertEqual(
            expected_uncovered, actual_coverage.uncovered_lines, "Not covered mismatch"
        )

    def assert_coverage_matches_comments(self, file_content: str) -> None:
        file_content = textwrap.dedent(file_content).strip()
        lines = file_content.split("\n")
        expected_covered = []
        expected_uncovered = []
        for line_number, line in enumerate(lines):
            if line.lower().endswith("# covered"):
                expected_covered.append(line_number)
            elif line.lower().endswith("# not covered"):
                expected_uncovered.append(line_number)
        self.assert_coverage_equal(file_content, expected_covered, expected_uncovered)

    def test_coverage_covered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo() -> int:
                return 5
            """,
            expected_covered=[0],
            expected_uncovered=[],
        )

    def test_coverage_uncovered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo():
                return 5
            """,
            expected_covered=[],
            expected_uncovered=[0],
        )

    def test_coverage_details(self) -> None:
        self.assert_coverage_matches_comments(
            """
            def foo(x) -> int:     # Covered
                  pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            def bar(x: int,        # Covered
                    y):            # Not Covered
                pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            a = foo()              # Covered
            b: int = bar()         # Covered
            """
        )
        self.assert_coverage_matches_comments(
            """
            class A:
                a: int = 100       # Covered
                b = ""             # Covered
            """
        )
        # For now, don't count annotations inside of functions
        self.assert_coverage_matches_comments(
            """
            def foo() -> None:     # Covered
                a: int = 100
            """
        )
        self.assert_coverage_matches_comments(
            """
            def foo():             # Not covered
                a: int = 100
            """
        )
        self.assert_coverage_matches_comments(
            """
            def foo():                       # Not covered
                def bar(x: int) -> int:      # Covered
                    pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            class A:
                def bar(self, x: int):       # Covered
                    pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            class A:
                def bar(this, x: int) -> None:   # Covered
                    pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            class A:
                @classmethod
                def bar(cls, x: int):            # Covered
                    pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            def bar(self, x: int):               # Covered
                pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            class A:
                @staticmethod
                def bar(self, x: int) -> None:   # Covered
                    pass
            """
        )
        self.assert_coverage_matches_comments(
            """
            def foo(x):              # Not covered
                def bar(x):          # Not covered
                    return x
                return bar

            class A:
                @foo(42)
                def baz(self): ...   # Covered
            """
        )
        self.assert_coverage_matches_comments(
            """
            def foo(x: str) -> str:     # Covered
                return x
            """
        )
        self.assert_coverage_matches_comments(
            """
            class Test:
                def foo(self, input: str) -> None:       # Covered
                    class Foo:
                        pass

                    pass

                def bar(self, input: str) -> None:       # Covered
                    pass
            """
        )
        # Ensure globals and attributes with literal values are considered annotated.
        self.assert_coverage_matches_comments(
            """
            x: int = 1        # Covered
            y = 2             # Covered
            z = foo           # Covered

            class Foo:
                x = 1         # Covered
                y = foo       # Covered
            """
        )
        self.assert_coverage_matches_comments(
            """
            def a_very_long_function_name(
                    parameter_1: int,        # Covered
                    parameter_2,             # Not covered
                    parameter_3: str,        # Covered
            ):                               # Not covered
                pass
            """
        )

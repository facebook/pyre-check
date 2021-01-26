# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap
import unittest
from typing import Dict

import libcst as cst

from ..commands.statistics import _path_wise_counts
from ..statistics_collectors import FixmeCountCollector, StrictCountCollector


class StatisticsCollectorTest(unittest.TestCase):
    def assert_fixme_count_equal(
        self, file_content: str, expected_fixmes: Dict[str, int]
    ) -> None:
        module = cst.parse_module(
            textwrap.dedent(file_content).strip().replace("FIXME", "pyre-fixme")
        )
        fixme_counts = _path_wise_counts({"test.py": module}, FixmeCountCollector)
        actual_fixmes = fixme_counts["test.py"].build_json()
        self.assertEqual(expected_fixmes, actual_fixmes)

    def test_fixme_count(self) -> None:
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {"7": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                # FIXME[7]: comments
                return x
            """,
            {"7": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x # unrelated # FIXME[7]
            """,
            {"7": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x # unrelated   #  FIXME[7] comments
            """,
            {"7": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME
            """,
            {"No Code": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME: comments
            """,
            {"No Code": 1},
        )
        self.assert_fixme_count_equal(
            """
            def foo(x: str) -> int:
                return x # FIXME[7, 8]
            """,
            {"7": 1, "8": 1},
        )
        self.assert_fixme_count_equal(
            """
            # FIXME[8]
            def foo(x: str) -> int:
                return x # FIXME[7, 8]
            """,
            {"7": 1, "8": 2},
        )

    def assert_strict_count_equal(
        self,
        file_content: str,
        expected_strict: Dict[str, int],
        strict_default: bool = False,
    ) -> None:
        module = cst.parse_module(textwrap.dedent(file_content).strip())
        strict_counts = _path_wise_counts(
            {"test.py": module}, StrictCountCollector, strict_default
        )
        actual_strict = strict_counts["test.py"].build_json()
        self.assertEqual(expected_strict, actual_strict)

    def test_strict_count(self) -> None:
        self.assert_strict_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {"strict_count": 0, "unsafe_count": 1},
        )
        self.assert_strict_count_equal(
            """
            #  pyre-strict
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {"strict_count": 1, "unsafe_count": 0},
        )

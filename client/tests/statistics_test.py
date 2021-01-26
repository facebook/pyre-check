# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap
import unittest
from typing import Dict

import libcst as cst

from ..commands.statistics import _path_wise_counts
from ..statistics_collectors import FixmeCountCollector


class StatisticsCollectorTest(unittest.TestCase):
    def assert_count_equal(
        self, file_content: str, expected_fixmes: Dict[str, int]
    ) -> None:
        module = cst.parse_module(
            textwrap.dedent(file_content).strip().replace("FIXME", "pyre-fixme")
        )
        fixme_counts = _path_wise_counts({"test.py": module}, FixmeCountCollector)
        actual_fixmes = fixme_counts["test.py"].build_json()
        self.assertEqual(expected_fixmes, actual_fixmes)

    def test_fixme_count(self) -> None:
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {"7": 1},
        )
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                # FIXME[7]: comments
                return x
            """,
            {"7": 1},
        )
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                return x # unrelated # FIXME[7]
            """,
            {"7": 1},
        )
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                return x # unrelated   #  FIXME[7] comments
            """,
            {"7": 1},
        )
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME
            """,
            {"No Code": 1},
        )
        self.assert_count_equal(
            """
            def foo(x: str) -> int:
                return x  # FIXME: comments
            """,
            {"No Code": 1},
        )

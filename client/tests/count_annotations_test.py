# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import textwrap
import unittest
from typing import Dict

from libcst import Module, parse_module

from ..count_annotations import TypeCollector


class ApplyAnnotationsTest(unittest.TestCase):
    @staticmethod
    def format_files(source: str) -> Module:
        return parse_module(textwrap.dedent(source.rstrip()))

    def assert_counts(self, source: str, expected: Dict[str, int]) -> None:
        source_module = self.format_files(source)
        collector = TypeCollector()
        source_module.visit(collector)
        self.assertEqual(collector.build_json(), expected)

    def test_annotate_functions(self) -> None:
        self.assert_counts(
            """
            def foo(x) -> int:
                pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
            },
        )

        self.assert_counts(
            """
            def bar(x: int, y):
                pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
            },
        )

        self.assert_counts(
            """
            a = foo()
            b: int = bar()
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 1,
                "annotated_parameter_count": 0,
                "return_count": 0,
                "globals_count": 2,
                "parameter_count": 0,
            },
        )

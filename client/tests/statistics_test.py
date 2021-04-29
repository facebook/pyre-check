# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap
import unittest
from typing import Dict

import libcst as cst
from libcst.metadata import MetadataWrapper

from ..commands.statistics import _path_wise_counts
from ..statistics_collectors import (
    AnnotationCountCollector,
    FixmeCountCollector,
    StrictCountCollector,
)


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
        self.assert_strict_count_equal(
            """
            #  pyre-ignore-all-errors[56]
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {"strict_count": 1, "unsafe_count": 0},
            True,
        )

    def assert_annotation_count_equal(
        self,
        file_content: str,
        expected_counts: Dict[str, int],
    ) -> None:
        module = cst.parse_module(textwrap.dedent(file_content).strip())
        annotation_counts = _path_wise_counts(
            {"test.py": MetadataWrapper(module)}, AnnotationCountCollector
        )
        actual_counts = annotation_counts["test.py"].build_json()
        self.assertEqual(expected_counts, actual_counts)

    def test_annotation_count(self) -> None:
        self.assert_annotation_count_equal(
            """
            def foo(x: str) -> str:
                return x
            """,
            {
                "return_count": 1,
                "annotated_return_count": 1,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 1,
                "annotated_parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
                "line_count": 3,
            },
        )
        self.assert_annotation_count_equal(
            """
            class Test:
                def foo(self, input: str) -> None:
                    class Foo:
                        pass

                    pass

                def bar(self, input: str) -> None:
                    pass
            """,
            {
                "return_count": 2,
                "annotated_return_count": 2,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 4,
                "annotated_parameter_count": 4,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 2,
                "line_count": 10,
            },
        )

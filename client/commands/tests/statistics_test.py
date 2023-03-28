# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
import textwrap
from pathlib import Path

from typing import Dict, List

import libcst

import testslide

from ... import coverage_data

from ...tests import setup
from ..statistics import (
    aggregate_statistics,
    AggregatedStatisticsData,
    collect_statistics,
    FixmeCountCollector,
    IgnoreCountCollector,
)


def parse_code(code: str) -> libcst.MetadataWrapper:
    module = coverage_data.module_from_code(textwrap.dedent(code.rstrip()))
    if module is None:
        raise RuntimeError(f"Failed to parse code {code}")
    return module


class FixmeCountCollectorTest(testslide.TestCase):
    def assert_counts(
        self,
        source: str,
        expected_codes: Dict[int, List[int]],
        expected_no_codes: List[int],
    ) -> None:
        source_module = parse_code(source.replace("FIXME", "pyre-fixme"))
        result = FixmeCountCollector().collect(source_module)
        self.assertEqual(expected_codes, result.code)
        self.assertEqual(expected_no_codes, result.no_code)

    def test_count_fixmes(self) -> None:
        # no error codes (none in first example, unparseable in second)
        self.assert_counts(
            """
            # FIXME
            # FIXME[8,]
            """,
            {},
            [2, 3],
        )
        self.assert_counts(
            """
            # FIXME[3]: Example Error Message
            # FIXME[3, 4]: Another Message

            # FIXME[34]: Example
            """,
            {3: [2, 3], 4: [3], 34: [5]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                # FIXME[7]: comments
                return x
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME
            """,
            {},
            [3],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME: comments
            """,
            {},
            [3],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # unrelated # FIXME[7]
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # unrelated   #  FIXME[7] comments
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # FIXME[7, 8]
            """,
            {7: [3], 8: [3]},
            [],
        )


class IgnoreCountCollectorTest(testslide.TestCase):
    maxDiff = 2000

    def assert_counts(
        self,
        source: str,
        expected_codes: Dict[int, List[int]],
        expected_no_codes: List[int],
    ) -> None:
        source_module = parse_code(source.replace("IGNORE", "pyre-ignore"))
        result = IgnoreCountCollector().collect(source_module)
        self.assertEqual(expected_codes, result.code)
        self.assertEqual(expected_no_codes, result.no_code)

    def test_count_ignores(self) -> None:
        self.assert_counts("# IGNORE[2]: Example Error Message", {2: [1]}, [])
        self.assert_counts(
            """
            # IGNORE[3]: Example Error Message

            # IGNORE[34]: Example
            """,
            {3: [2], 34: [4]},
            [],
        )
        self.assert_counts(
            """
            # IGNORE[2]: Example Error Message

            # IGNORE[2]: message
            """,
            {2: [2, 4]},
            [],
        )


class StatisticsTest(testslide.TestCase):
    def test_collect_statistics(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(root_path, ["foo.py", "bar.py"])
            foo_path = root_path / "foo.py"
            bar_path = root_path / "bar.py"

            data = collect_statistics([foo_path, bar_path], strict_default=False)
            self.assertIn(str(foo_path), data)
            self.assertIn(str(bar_path), data)

    def test_aggregate_statistics__single_file(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            a_path = root_path / "a.py"
            a_path.write_text(
                textwrap.dedent(
                    """
                    # pyre-unsafe

                    def foo():
                        return 1
                    """.rstrip()
                )
            )

            self.assertEqual(
                aggregate_statistics(
                    collect_statistics([a_path], strict_default=False)
                ),
                AggregatedStatisticsData(
                    annotations={
                        "return_count": 1,
                        "annotated_return_count": 0,
                        "globals_count": 0,
                        "annotated_globals_count": 0,
                        "parameter_count": 0,
                        "annotated_parameter_count": 0,
                        "attribute_count": 0,
                        "annotated_attribute_count": 0,
                        "function_count": 1,
                        "partially_annotated_function_count": 0,
                        "fully_annotated_function_count": 0,
                        "line_count": 5,
                    },
                    fixmes=0,
                    ignores=0,
                    strict=0,
                    unsafe=1,
                ),
            )

    def test_aggregate_statistics__multiple_files(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            a_path = root_path / "a.py"
            b_path = root_path / "b.py"
            a_path.write_text(
                textwrap.dedent(
                    """
                    # pyre-unsafe

                    def foo():
                        return 1
                    """.rstrip()
                )
            )
            b_path.write_text(
                textwrap.dedent(
                    """
                    # pyre-strict

                    def foo(x: int) -> int:
                        return 1
                    """.rstrip()
                )
            )

            self.assertEqual(
                aggregate_statistics(
                    collect_statistics([a_path, b_path], strict_default=False)
                ),
                AggregatedStatisticsData(
                    annotations={
                        "return_count": 2,
                        "annotated_return_count": 1,
                        "globals_count": 0,
                        "annotated_globals_count": 0,
                        "parameter_count": 1,
                        "annotated_parameter_count": 1,
                        "attribute_count": 0,
                        "annotated_attribute_count": 0,
                        "function_count": 2,
                        "partially_annotated_function_count": 0,
                        "fully_annotated_function_count": 1,
                        "line_count": 10,
                    },
                    fixmes=0,
                    ignores=0,
                    strict=1,
                    unsafe=1,
                ),
            )

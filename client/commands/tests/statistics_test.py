# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
import textwrap
from pathlib import Path

import testslide

from ...tests import setup
from ..statistics import (
    aggregate_statistics,
    AggregatedStatisticsData,
    collect_statistics,
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

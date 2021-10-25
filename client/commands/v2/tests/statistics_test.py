# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
import textwrap
from pathlib import Path

import testslide

from .... import configuration, command_arguments
from ....tests import setup
from ..statistics import (
    find_roots,
    find_paths_to_parse,
    parse_text_to_module,
    parse_path_to_module,
    collect_statistics,
    aggregate_statistics,
    AggregatedStatisticsData,
)


class StatisticsTest(testslide.TestCase):
    def test_find_roots__filter_path_duplicate(self) -> None:
        self.assertCountEqual(
            find_roots(
                configuration.Configuration(
                    project_root="/root", dot_pyre_directory=Path("/irrelevant")
                ),
                command_arguments.StatisticsArguments(
                    filter_paths=["/root/foo.py", "/root/bar.py", "/root/foo.py"]
                ),
            ),
            [Path("/root/foo.py"), Path("/root/bar.py")],
        )

    def test_find_roots__filter_path_expand(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        configuration.Configuration(
                            project_root="/root", dot_pyre_directory=Path("/irrelevant")
                        ),
                        command_arguments.StatisticsArguments(
                            filter_paths=["foo.py", "bar.py"]
                        ),
                    ),
                    [root_path / "foo.py", root_path / "bar.py"],
                )

    def test_find_roots__local_root(self) -> None:
        self.assertCountEqual(
            find_roots(
                configuration.Configuration(
                    project_root="/root",
                    dot_pyre_directory=Path("/irrelevant"),
                    relative_local_root="local",
                ),
                command_arguments.StatisticsArguments(filter_paths=[]),
            ),
            [Path("/root/local")],
        )

    def test_find_roots__current_working_directory(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        configuration.Configuration(
                            project_root="/root", dot_pyre_directory=Path("/irrelevant")
                        ),
                        command_arguments.StatisticsArguments(filter_paths=[]),
                    ),
                    [root_path],
                )

    def test_find_paths_to_parse(self) -> None:
        pyre_configuration = configuration.Configuration(
            project_root="/root", dot_pyre_directory=Path("/irrelevant")
        )
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_paths_to_parse(
                    pyre_configuration,
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                ),
                [
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                ],
            )
            self.assertCountEqual(
                find_paths_to_parse(pyre_configuration, [root_path]),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                    root_path / "b/c/s3.py",
                ],
            )

        pyre_configuration = configuration.Configuration(
            project_root="/root",
            dot_pyre_directory=Path("/irrelevant"),
            excludes=[r".*2\.py"],
        )
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_paths_to_parse(
                    pyre_configuration,
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                ),
                [
                    root_path / "a/s1.py",
                ],
            )
            self.assertCountEqual(
                find_paths_to_parse(pyre_configuration, [root_path]),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/c/s3.py",
                ],
            )

    def test_parse_text_to_module(self) -> None:
        self.assertIsNotNone(
            parse_text_to_module(
                textwrap.dedent(
                    """
                    def foo() -> int:
                        pass
                    """
                )
            )
        )
        self.assertIsNone(
            parse_text_to_module(
                textwrap.dedent(
                    """
                    def foo() ->
                    """
                )
            )
        )

    def test_parse_path_to_module(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            source_path = root_path / "source.py"
            source_path.write_text("reveal_type(42)")

            self.assertIsNotNone(parse_path_to_module(source_path))
            self.assertIsNone(parse_path_to_module(root_path / "nonexistent.py"))

    def test_collect_statistics(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(root_path, ["foo.py", "bar.py"])
            foo_path = root_path / "foo.py"
            bar_path = root_path / "bar.py"

            data = collect_statistics([foo_path, bar_path], strict_default=False)
            self.assertIn(str(foo_path), data.annotations)
            self.assertIn(str(foo_path), data.fixmes)
            self.assertIn(str(foo_path), data.ignores)
            self.assertIn(str(foo_path), data.strict)
            self.assertIn(str(bar_path), data.annotations)
            self.assertIn(str(bar_path), data.fixmes)
            self.assertIn(str(bar_path), data.ignores)
            self.assertIn(str(bar_path), data.strict)

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
                        "partially_annotated_function_count": 0,
                        "fully_annotated_function_count": 0,
                        "line_count": 6,
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
                        "partially_annotated_function_count": 0,
                        "fully_annotated_function_count": 1,
                        "line_count": 12,
                    },
                    fixmes=0,
                    ignores=0,
                    strict=1,
                    unsafe=1,
                ),
            )

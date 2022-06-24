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
    find_paths_to_parse,
    find_roots,
    parse_path_to_module,
    parse_text_to_module,
)


class StatisticsTest(testslide.TestCase):
    def test_find_roots__duplicate_directories(self) -> None:
        self.assertCountEqual(
            find_roots(
                ["/root/foo.py", "/root/bar.py", "/root/foo.py"],
                local_root=None,
                global_root=Path("/root"),
            ),
            [Path("/root/foo.py"), Path("/root/bar.py")],
        )

        self.assertCountEqual(
            find_roots(
                ["/root/foo", "/root/bar", "/root/foo"],
                local_root=None,
                global_root=Path("/root"),
            ),
            [Path("/root/foo"), Path("/root/bar")],
        )

    def test_find_roots__expand_directories(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        ["foo.py", "bar.py"],
                        local_root=None,
                        global_root=root_path,
                    ),
                    [root_path / "foo.py", root_path / "bar.py"],
                )

    def test_find_roots__invalid_given_subdirectory(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        ["subdirectory"],
                        local_root=None,
                        global_root=root_path / "project_root",
                    ),
                    [root_path / "project_root"],
                )

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        ["subdirectory"],
                        local_root=root_path / "local_root",
                        global_root=root_path,
                    ),
                    [root_path / "local_root"],
                )

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        ["project_root/subdirectory"],
                        local_root=None,
                        global_root=root_path / "project_root",
                    ),
                    [root_path / "project_root/subdirectory"],
                )

    def test_find_roots__local_root(self) -> None:
        self.assertCountEqual(
            find_roots(
                [],
                local_root=Path("/root/local"),
                global_root=Path("/root"),
            ),
            [Path("/root/local")],
        )

    def test_find_roots__global_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    find_roots(
                        [],
                        local_root=None,
                        global_root=Path("/root"),
                    ),
                    [Path("/root")],
                )

    def test_find_paths_to_parse(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_paths_to_parse(
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                    excludes=[],
                ),
                [
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                ],
            )
            self.assertCountEqual(
                find_paths_to_parse([root_path], excludes=[]),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                    root_path / "b/c/s3.py",
                ],
            )

    def test_find_paths_to_parse_with_exclude(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_paths_to_parse(
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                    excludes=[r".*2\.py"],
                ),
                [
                    root_path / "a/s1.py",
                ],
            )
            self.assertCountEqual(
                find_paths_to_parse(
                    [root_path],
                    excludes=[r".*2\.py"],
                ),
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

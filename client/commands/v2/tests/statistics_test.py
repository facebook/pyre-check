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
            root_path = Path(root)
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
            root_path = Path(root)
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
                    ]
                ),
                [
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                ],
            )
            self.assertCountEqual(
                find_paths_to_parse([root_path]),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
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

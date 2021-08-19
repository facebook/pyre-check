# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from pathlib import Path

import testslide

from .... import configuration, command_arguments
from ....tests import setup
from ..statistics import find_roots


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

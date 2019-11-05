# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from typing import Dict
from unittest.mock import MagicMock, call, patch

from .. import buck, commands, filesystem
from ..analysis_directory import (
    AnalysisDirectory,
    SharedAnalysisDirectory,
    resolve_analysis_directory,
)


class AnalysisDirectoryTest(unittest.TestCase):
    @patch.object(
        buck,
        "generate_source_directories",
        side_effect=lambda targets, build, prompt: targets,
    )
    def test_resolve_analysis_directory(self, buck) -> None:  # pyre-fixme[2]
        arguments = MagicMock()
        arguments.build = None
        arguments.original_directory = "/project"
        arguments.current_directory = "/project"

        def assert_analysis_directory(
            expected: AnalysisDirectory, actual: AnalysisDirectory
        ) -> None:
            self.assertEqual(expected.get_root(), actual.get_root())
            self.assertEqual(expected.get_filter_root(), actual.get_filter_root())

        configuration = MagicMock()
        configuration.source_directories = []
        configuration.targets = []
        configuration.local_configuration_root = None

        arguments.source_directories = ["a/b"]
        arguments.targets = []
        arguments.filter_directory = None
        expected_analysis_directory = AnalysisDirectory("a/b")
        analysis_directory = resolve_analysis_directory(
            arguments, commands, configuration
        )
        assert_analysis_directory(expected_analysis_directory, analysis_directory)

        arguments.source_directories = ["/symlinked/directory"]
        arguments.targets = []
        arguments.filter_directory = "/real/directory"
        expected_analysis_directory = AnalysisDirectory(
            "/symlinked/directory", filter_paths=["/real/directory"]
        )
        analysis_directory = resolve_analysis_directory(
            arguments, commands, configuration
        )
        assert_analysis_directory(expected_analysis_directory, analysis_directory)

        arguments.source_directories = []
        arguments.targets = ["//x:y"]
        arguments.filter_directory = "/real/directory"
        expected_analysis_directory = SharedAnalysisDirectory(
            [],
            ["//x:y"],
            original_directory="/project",
            filter_paths=["/real/directory"],
        )
        analysis_directory = resolve_analysis_directory(
            arguments, commands, configuration
        )
        assert_analysis_directory(expected_analysis_directory, analysis_directory)

        arguments.source_directories = ["a/b"]
        arguments.targets = ["//x:y", "//y/..."]
        arguments.filter_directory = "/filter"
        configuration.targets = ["//overridden/..."]
        expected_analysis_directory = SharedAnalysisDirectory(
            ["a/b"],
            ["//x:y", "//y:/..."],
            original_directory="/project",
            filter_paths=["/filter"],
        )
        analysis_directory = resolve_analysis_directory(
            arguments, commands, configuration
        )
        assert_analysis_directory(expected_analysis_directory, analysis_directory)

        arguments.source_directories = []
        arguments.targets = []
        arguments.filter_directory = "/filter"
        configuration.source_directories = []
        configuration.targets = ["//not:overridden/..."]
        expected_analysis_directory = SharedAnalysisDirectory(
            [],
            ["//not:overridden/..."],
            original_directory="/project",
            filter_paths=["/filter"],
        )
        analysis_directory = resolve_analysis_directory(
            arguments, commands, configuration
        )
        assert_analysis_directory(expected_analysis_directory, analysis_directory)

    def test_merge_into_paths(self) -> None:
        directory = tempfile.mkdtemp()  # type: str
        root = os.path.realpath(directory)

        Path(root, "a.py").touch()
        Path(root, "b.pyi").touch()
        Path(root, "c.cpp").touch()
        Path(root, "link1.py").symlink_to(Path(root, "a.py"))
        Path(root, "link2.py").symlink_to(Path(root, "dangling.py"))
        Path(root, "link3.py").symlink_to(Path(root, "c.cpp"))
        Path(root, "link4.cpp").symlink_to(Path(root, "a.py"))
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        Path(root, "directory_symlink.py").symlink_to(Path(root, "spy.py"))
        Path(root, "mypy/my.py").touch()
        Path(root, "scipyi/sci.pyi").touch()
        Path(root, "mypy/another.pyi").symlink_to(Path(root, "mypy/my.py"))
        Path(root, "scipyi/another.py").symlink_to(Path(root, "scipyi/sci.pyi"))
        shared_analysis_directory = SharedAnalysisDirectory([root], [])
        all_paths = {}  # type: Dict[str, str]
        shared_analysis_directory._merge_into_paths(root, all_paths)
        self.assertEqual(
            all_paths,
            {
                "a.py": os.path.join(root, "a.py"),
                "b.pyi": os.path.join(root, "b.pyi"),
                "link1.py": os.path.join(root, "a.py"),
                "link3.py": os.path.join(root, "c.cpp"),
                "mypy/another.pyi": os.path.join(root, "mypy/my.py"),
                "mypy/my.py": os.path.join(root, "mypy/my.py"),
                "scipyi/another.py": os.path.join(root, "scipyi/sci.pyi"),
                "scipyi/sci.pyi": os.path.join(root, "scipyi/sci.pyi"),
            },
        )

    @patch.object(filesystem, "is_empty", return_value=False)
    @patch.object(os, "symlink")
    @patch.object(subprocess, "check_output")
    @patch.object(os, "makedirs")
    @patch.object(os.path, "exists")
    @patch.object(os.path, "realpath")
    def test_merge(
        self,
        os_path_realpath: MagicMock,
        os_path_exists: MagicMock,
        os_makedirs: MagicMock,
        check_output: MagicMock,
        os_symlink: MagicMock,
        is_empty: MagicMock,
    ) -> None:
        os_path_exists.return_value = False
        root = tempfile.mkdtemp()
        os.mkdir(os.path.join(root, "first"))
        os.mkdir(os.path.join(root, "first", "b"))
        os.mkdir(os.path.join(root, "second"))

        Path(root, "first", "x.py").touch()
        Path(root, "first", "y.py").touch()
        Path(root, "first", "b", "z.py").touch()
        Path(root, "second", "a.py").touch()

        # pyre-fixme[2]: Parameter `stderr` has type `None` but no type is specified.
        def side_effect(path: str, stderr=None) -> bytes:
            if path[1].endswith("first"):
                serialized = "\n".join(
                    [
                        os.path.join(root, path)
                        for path in ["first/x.py", "first/y.py", "first/b/z.py"]
                    ]
                )
            else:
                serialized = os.path.join(root, "second/a.py")
            return bytes(serialized, "utf-8")

        check_output.side_effect = side_effect
        os_path_realpath.side_effect = lambda x: x
        shared_analysis_directory = SharedAnalysisDirectory(
            [os.path.join(root, "first"), os.path.join(root, "second")], []
        )
        shared_analysis_directory._merge()
        shared_root = shared_analysis_directory.get_root()
        os_makedirs.assert_has_calls(
            [call(shared_root), call(shared_root + "/b")], any_order=True
        )
        os_symlink.assert_has_calls(
            [
                call(root + "/first/x.py", shared_root + "/x.py"),
                call(root + "/first/y.py", shared_root + "/y.py"),
                call(root + "/first/b/z.py", shared_root + "/b/z.py"),
                call(root + "/second/a.py", shared_root + "/a.py"),
            ],
            any_order=True,
        )

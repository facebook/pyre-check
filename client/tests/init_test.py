# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from typing import NamedTuple, Optional
from unittest.mock import MagicMock, patch

from .. import (
    __name__ as client_name,
    _resolve_filter_paths,
    find_local_root,
    find_log_directory,
    find_project_root,
    get_binary_version,
)
from ..filesystem import __name__ as filesystem_name


class MockCompletedProcess(NamedTuple):
    returncode: int
    stdout: str


class InitTest(unittest.TestCase):
    def test_find_project_root(self) -> None:
        original_directory = "/a/b/c"
        with patch("os.path.realpath", return_value="realpath"), patch(
            "os.path.isfile", return_value=False
        ) as isfile, patch("os.getcwd", return_value="/a/b/c"):
            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration"
            )
            directory = find_project_root(original_directory)
            self.assertEqual(directory, "/a/b")

        with patch("{}.find_root".format(filesystem_name)) as mock_find_root:
            original_directory = "/a/b"
            mock_find_root.side_effect = ["/a", "/a/b"]
            directory = find_project_root(original_directory)
            self.assertEqual(directory, "/a/b")

    @patch("{}.LOG.warning".format(client_name))
    def test_find_local_root(self, warning) -> None:
        original_directory = "/a/b/c"
        with patch("os.path.realpath", return_value="realpath"), patch(
            "os.path.isfile", return_value=False
        ) as isfile:
            local_root = find_local_root(original_directory)
            self.assertEqual(local_root, None)

            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration.local"
            )
            local_root = find_local_root(original_directory)
            self.assertEqual(local_root, "/a/b")

            isfile.side_effect = (
                lambda directory: directory == "/a/b/.pyre_configuration.local"
                or directory == "/a/.pyre_configuration.local"
            )
            find_local_root(original_directory)
            warning.assert_called_once()

    @patch("{}.Path".format(client_name))
    @patch("{}.Path.mkdir".format(client_name))
    @patch("os.makedirs")
    def test_find_log_directory(self, mkdirs, path_mkdir, path) -> None:
        local_configuration = None
        current_directory = "project"
        log_directory = find_log_directory(
            current_directory, local_configuration, dot_pyre_directory="project/.pyre"
        )
        self.assertEqual(log_directory, "project/.pyre")

        local_configuration = "/project/subdirectory"
        current_directory = "/project"
        log_directory = find_log_directory(
            current_directory, local_configuration, "/project/.pyre"
        )
        self.assertEqual(log_directory, "/project/.pyre/subdirectory")

    def test_resolve_filter_paths(self) -> None:
        arguments = MagicMock()
        configuration = MagicMock()
        original_directory = "/project"
        arguments.source_directories = []
        arguments.targets = []
        configuration.local_configuration_root = None

        filter_paths = _resolve_filter_paths(
            arguments, configuration, original_directory
        )
        self.assertEqual(filter_paths, set())

        arguments.source_directories = ["/project/a"]
        filter_paths = _resolve_filter_paths(
            arguments, configuration, original_directory
        )
        self.assertEqual(filter_paths, {"/project/a"})

        arguments.source_directories = ["/project/a"]
        arguments.targets = ["//x/y/..."]
        filter_paths = _resolve_filter_paths(
            arguments, configuration, original_directory
        )
        self.assertEqual(filter_paths, {"/project/a", "x/y"})

        arguments.source_directories = ["/project/local/a"]
        arguments.targets = ["//x/y:z"]
        configuration.local_configuration_root = "project/local"
        filter_paths = _resolve_filter_paths(
            arguments, configuration, original_directory
        )
        self.assertEqual(filter_paths, {"/project/local/a", "x/y"})

        arguments.source_directories = []
        arguments.targets = []
        configuration.local_configuration_root = "/project/local"
        filter_paths = _resolve_filter_paths(
            arguments, configuration, original_directory
        )
        self.assertEqual(filter_paths, {"/project/local"})

    def test_get_binary_version(self) -> None:
        configuration: MagicMock = MagicMock()
        configuration.binary = ""

        def assert_version(
            returncode: int, stdout: str, expected: Optional[str]
        ) -> None:
            with patch(
                "subprocess.run",
                return_value=MockCompletedProcess(returncode, stdout=stdout),
            ):
                self.assertEqual(expected, get_binary_version(configuration))

        assert_version(
            returncode=0, stdout="facefacefaceb00", expected="facefacefaceb00"
        )
        assert_version(
            returncode=0, stdout=" facefacefaceb00\n", expected="facefacefaceb00"
        )
        assert_version(returncode=1, stdout="facefacefaceb00", expected=None)

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import unittest
from unittest.mock import MagicMock, patch

from .. import (
    EnvironmentException,
    __name__ as client_name,
    _resolve_filter_paths,
    find_local_root,
    find_log_directory,
    find_project_root,
    get_binary_version_from_file,
)
from ..filesystem import __name__ as filesystem_name


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

    def test_find_local_root(self) -> None:
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

            with self.assertRaises(EnvironmentException):
                isfile.side_effect = (
                    lambda directory: directory == "/a/b/.pyre_configuration.local"
                    or directory == "/a/.pyre_configuration.local"
                )
                find_local_root(original_directory)

    @patch("{}.Path".format(client_name))
    @patch("{}.Path.mkdir".format(client_name))
    @patch("os.makedirs")
    def test_find_log_directory(self, mkdirs, path_mkdir, path) -> None:
        local_configuration = None
        current_directory = "project"
        log_directory = find_log_directory(None, current_directory, local_configuration)
        self.assertEqual(log_directory, "project/.pyre")

        local_configuration = "/project/subdirectory"
        current_directory = "/project"
        log_directory = find_log_directory(None, current_directory, local_configuration)
        self.assertEqual(log_directory, "/project/.pyre/subdirectory")

        log_directory = find_log_directory(
            "something", current_directory, local_configuration
        )
        self.assertEqual(log_directory, "something")

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

    @patch.object(os, "getenv", return_value=None)
    @patch("builtins.open")
    @patch("json.loads")
    def test_get_binary_version_from_file(
        self, json_load, open, os_environment
    ) -> None:
        # No local configuration
        json_load.side_effect = [
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
                "version": "VERSION",
            },
            {},
        ]
        self.assertEqual("VERSION", get_binary_version_from_file(None))

        json_load.side_effect = [
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
            },
            {},
        ]
        self.assertEqual("No version set", get_binary_version_from_file(None))

        # With local configuration
        json_load.side_effect = [
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
                "version": "LOCAL_VERSION",
            },
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
                "version": "MASTER_VERSION",
            },
        ]
        self.assertEqual("LOCAL_VERSION", get_binary_version_from_file("local"))

        json_load.side_effect = [
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
            },
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
                "version": "MASTER_VERSION",
            },
        ]
        self.assertEqual("MASTER_VERSION", get_binary_version_from_file("local"))

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from .. import (
    EnvironmentException,
    __name__ as client_name,
    _resolve_filter_paths,
    _resolve_source_directories,
    buck,
    find_configuration_root,
    switch_root,
)


class InitTest(unittest.TestCase):
    @patch("os.path.isfile")
    def test_find_configuration(self, os_mock_isfile) -> None:
        os_mock_isfile.side_effect = [False, False, False, True]
        self.assertEqual(find_configuration_root("/a/b/c/d", "configuration"), "/a")
        os_mock_isfile.side_effect = [True]
        self.assertEqual(find_configuration_root("/a", "configuration"), "/a")
        os_mock_isfile.side_effect = [False, False]
        self.assertEqual(find_configuration_root("/a/b", "configuration"), None)

    @patch("os.chdir")
    def test_switch_root(self, chdir) -> None:
        arguments = MagicMock()
        with patch("os.path.realpath", return_value="realpath"), patch(
            "os.path.isfile", return_value=False
        ) as isfile, patch("os.chdir"):
            arguments.local_configuration = None
            switch_root(arguments)
            self.assertEqual(arguments.local_configuration, None)

            arguments.local_configuration = None
            with patch("os.getcwd", return_value="/a/b/c"):
                isfile.side_effect = (
                    lambda directory: directory == "/a/b/.pyre_configuration.local"
                )
                switch_root(arguments)
                self.assertEqual(arguments.original_directory, "/a/b/c")
                self.assertEqual(arguments.local_configuration, "/a/b")

        with patch(
            "{}.find_configuration_root".format(client_name)
        ) as mock_find_configuation_root:
            with patch("os.getcwd", return_value="/a/b"):
                arguments.original_directory = "/a/b"
                arguments.current_directory = "/a/b"
                arguments.local_configuration = None
                mock_find_configuation_root.side_effect = ["/a", "/a/b"]
                switch_root(arguments)
                self.assertEqual(arguments.original_directory, "/a/b")
                self.assertEqual(arguments.current_directory, "/a/b")
                self.assertEqual(arguments.local_configuration, None)

    @patch("os.path.realpath", side_effect=lambda path: "realpath({})".format(path))
    @patch("os.getcwd", return_value="/")
    @patch("os.path.exists", return_value=True)
    def test_resolve_source_directories(self, realpath, cwd, exists) -> None:
        arguments = MagicMock()
        arguments.source_directories = []
        arguments.original_directory = "/root"
        arguments.build = False
        configuration = MagicMock()
        configuration.source_directories = []

        with self.assertRaises(EnvironmentException):
            _resolve_source_directories(arguments, configuration, prompt=True)

        # Arguments override configuration.
        with patch.object(
            buck, "generate_source_directories", return_value=[]
        ) as buck_source_directories:
            arguments.source_directories = ["arguments_source_directory"]
            configuration.source_directories = ["configuration_source_directory"]

            source_directories = _resolve_source_directories(
                arguments, configuration, prompt=True
            )
            buck_source_directories.assert_called_with(set(), build=False, prompt=True)
            self.assertEqual(
                source_directories, {"realpath(root/arguments_source_directory)"}
            )

        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.target = ["arguments_target"]
            configuration.source_directories = ["configuration_source_directory"]

            source_directories = _resolve_source_directories(
                arguments, configuration, prompt=True
            )
            buck_source_directories.assert_called_with(
                {"arguments_target"}, build=False, prompt=True
            )
            self.assertEqual(source_directories, {"realpath(root/arguments_target)"})

        # Configuration is picked up when no arguments provided.
        with patch.object(
            buck, "generate_source_directories", return_value=[]
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.target = []
            arguments.build = True
            configuration.targets = ["configuration_target"]
            configuration.source_directories = ["configuration_source_directory"]

            source_directories = _resolve_source_directories(
                arguments, configuration, prompt=True
            )
            buck_source_directories.assert_called_with(
                {"configuration_target"}, build=True, prompt=True
            )
            self.assertEqual(
                source_directories, {"realpath(root/configuration_source_directory)"}
            )

        # Files are translated relative to project root
        with patch.object(
            buck, "generate_source_directories", return_value=[]
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.target = []
            configuration.source_directories = ["."]
            source_directories = _resolve_source_directories(
                arguments, configuration, prompt=True
            )
            self.assertEqual(source_directories, {"realpath(root/.)"})

    def test_resolve_filter_paths(self) -> None:
        arguments = MagicMock()
        configuration = MagicMock()
        arguments.source_directories = []
        arguments.target = []
        arguments.original_directory = "/project"
        configuration.local_configuration_root = None

        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, [])

        arguments.source_directories = ["/project/a"]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a"])

        arguments.source_directories = ["/project/a"]
        arguments.target = ["//x/y/..."]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a", "x/y"])

        arguments.source_directories = ["/project/local/a"]
        arguments.target = ["//x/y:z"]
        configuration.local_configuration_root = "project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local/a", "x/y"])

        arguments.source_directories = []
        arguments.target = []
        configuration.local_configuration_root = "/project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local"])

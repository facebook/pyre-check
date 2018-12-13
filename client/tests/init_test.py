# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from .. import (
    EnvironmentException,
    __name__ as client_name,
    _resolve_analysis_paths,
    _resolve_filter_paths,
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

            arguments.local_configuration = "fakepath"
            switch_root(arguments)
            self.assertEqual(arguments.local_configuration, "realpath")

            arguments.local_configuration = None
            with patch("os.getcwd", return_value="/a/b/c"):
                isfile.side_effect = (
                    lambda directory: directory == "/a/b/.pyre_configuration.local"
                )
                switch_root(arguments)
                self.assertEqual(arguments.original_directory, "/a/b/c")
                self.assertEqual(arguments.local_configuration_directory, "/a/b")

        with patch(
            "{}.find_configuration_root".format(client_name)
        ) as mock_find_configuation_root:
            with patch("os.getcwd", return_value="/a/b"):
                arguments.original_directory = "/a/b"
                arguments.current_directory = "/a/b"
                arguments.local_configuration_directory = "/a"
                mock_find_configuation_root.side_effect = ["/a", "/a/b"]
                switch_root(arguments)
                self.assertEqual(arguments.original_directory, "/a/b")
                self.assertEqual(arguments.current_directory, "/a/b")
                self.assertEqual(arguments.local_configuration_directory, None)

    @patch("os.path.realpath", side_effect=lambda path: "realpath({})".format(path))
    @patch("os.getcwd", return_value="/")
    @patch("os.path.exists", return_value=True)
    def test_resolve_analysis_paths(self, realpath, cwd, exists) -> None:
        arguments = MagicMock()
        arguments.analysis_directory = []
        arguments.original_directory = "/root"
        arguments.build = False
        configuration = MagicMock()
        configuration.analysis_directories = []

        with self.assertRaises(EnvironmentException):
            _resolve_analysis_paths(arguments, configuration, prompt=True)

        # Arguments override configuration.
        with patch.object(
            buck, "generate_analysis_directories", return_value=[]
        ) as buck_analysis_directories:
            arguments.analysis_directory = ["arguments_analysis_directory"]
            configuration.analysis_directories = ["configuration_analysis_directory"]

            analysis_directories = _resolve_analysis_paths(
                arguments, configuration, prompt=True
            )
            buck_analysis_directories.assert_called_with(
                set(), build=False, prompt=True
            )
            self.assertEqual(
                analysis_directories, {"realpath(root/arguments_analysis_directory)"}
            )

        with patch.object(
            buck, "generate_analysis_directories", return_value=["arguments_target"]
        ) as buck_analysis_directories:
            arguments.analysis_directory = []
            arguments.target = ["arguments_target"]
            configuration.analysis_directories = ["configuration_analysis_directory"]

            analysis_directories = _resolve_analysis_paths(
                arguments, configuration, prompt=True
            )
            buck_analysis_directories.assert_called_with(
                {"arguments_target"}, build=False, prompt=True
            )
            self.assertEqual(analysis_directories, {"realpath(root/arguments_target)"})

        # Configuration is picked up when no arguments provided.
        with patch.object(
            buck, "generate_analysis_directories", return_value=[]
        ) as buck_analysis_directories:
            arguments.analysis_directory = []
            arguments.target = []
            arguments.build = True
            configuration.targets = ["configuration_target"]
            configuration.analysis_directories = ["configuration_analysis_directory"]

            analysis_directories = _resolve_analysis_paths(
                arguments, configuration, prompt=True
            )
            buck_analysis_directories.assert_called_with(
                {"configuration_target"}, build=True, prompt=True
            )
            self.assertEqual(
                analysis_directories,
                {"realpath(root/configuration_analysis_directory)"},
            )

        # Files are translated relative to project root
        with patch.object(
            buck, "generate_analysis_directories", return_value=[]
        ) as buck_analysis_directories:
            arguments.analysis_directory = []
            arguments.target = []
            configuration.analysis_directories = ["."]
            analysis_directories = _resolve_analysis_paths(
                arguments, configuration, prompt=True
            )
            self.assertEqual(analysis_directories, {"realpath(root/.)"})

    def test_resolve_filter_paths(self) -> None:
        arguments = MagicMock()
        configuration = MagicMock()
        arguments.analysis_directory = []
        arguments.target = []
        arguments.original_directory = "/project"
        configuration.local_configuration_root = None

        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, [])

        arguments.analysis_directory = ["/project/a"]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a"])

        arguments.analysis_directory = ["/project/a"]
        arguments.target = ["//x/y/..."]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a", "x/y"])

        arguments.analysis_directory = ["/project/local/a"]
        arguments.target = ["//x/y:z"]
        configuration.local_configuration_root = "project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local/a", "x/y"])

        arguments.analysis_directory = []
        arguments.target = []
        configuration.local_configuration_root = "/project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local"])

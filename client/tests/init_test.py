# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import unittest
from unittest.mock import MagicMock, patch

from .. import (
    EnvironmentException,
    _resolve_filter_paths,
    buck,
    commands,
    get_binary_version_from_file,
    resolve_analysis_directory,
    switch_root,
    translate_paths,
)
from ..filesystem import (
    AnalysisDirectory,
    SharedAnalysisDirectory,
    __name__ as filesystem_name,
)


class InitTest(unittest.TestCase):
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

        with patch("{}.find_root".format(filesystem_name)) as mock_find_root:
            with patch("os.getcwd", return_value="/a/b"):
                arguments.original_directory = "/a/b"
                arguments.current_directory = "/a/b"
                arguments.local_configuration = None
                mock_find_root.side_effect = ["/a", "/a/b"]
                switch_root(arguments)
                self.assertEqual(arguments.original_directory, "/a/b")
                self.assertEqual(arguments.current_directory, "/a/b")
                self.assertEqual(arguments.local_configuration, None)

    def test_resolve_filter_paths(self) -> None:
        arguments = MagicMock()
        configuration = MagicMock()
        arguments.source_directories = []
        arguments.targets = []
        arguments.original_directory = "/project"
        configuration.local_configuration_root = None

        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, [])

        arguments.source_directories = ["/project/a"]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a"])

        arguments.source_directories = ["/project/a"]
        arguments.targets = ["//x/y/..."]
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/a", "x/y"])

        arguments.source_directories = ["/project/local/a"]
        arguments.targets = ["//x/y:z"]
        configuration.local_configuration_root = "project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local/a", "x/y"])

        arguments.source_directories = []
        arguments.targets = []
        configuration.local_configuration_root = "/project/local"
        filter_paths = _resolve_filter_paths(arguments, configuration)
        self.assertEqual(filter_paths, ["/project/local"])

    @patch.object(
        buck,
        "generate_source_directories",
        side_effect=lambda targets, build, prompt: targets,
    )
    def test_resolve_analysis_directory(self, buck) -> None:
        arguments = MagicMock()
        arguments.build = None
        arguments.original_directory = "/project"
        arguments.current_directory = "/project"

        def assert_analysis_directory(expected, actual) -> None:
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

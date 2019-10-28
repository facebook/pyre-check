# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from .. import buck, commands
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

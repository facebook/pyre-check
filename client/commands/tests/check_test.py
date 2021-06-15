# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import commands, find_directories, configuration as configuration_module
from ...analysis_directory import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


NO_ERROR_JSON_OUTPUT = {"errors": []}


class CheckTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check(
        self,
        get_directories_to_analyze,
        realpath,
        check_output,
        find_global_and_local_root,
    ) -> None:
        realpath.side_effect = lambda x: x

        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ):
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

        shared_analysis_directory = MagicMock()
        shared_analysis_directory.get_root = lambda: "."
        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ), patch.object(
            shared_analysis_directory, "prepare"
        ) as prepare:
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=shared_analysis_directory,
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)
            prepare.assert_called_once_with()

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_sequential_check(
        self, directories_to_analyze, realpath, check_output, find_global_and_local_root
    ) -> None:
        realpath.side_effect = lambda x: x

        original_directory = "/original/directory"
        arguments = mock_arguments(sequential=True)
        configuration = mock_configuration()

        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ):
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.Reporting, "_get_directories_to_analyze", return_value={"a", "b"}
    )
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    def test_filter_directories(
        self, find_global_and_local_root, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x

        original_directory = "/original/directory"
        arguments = mock_arguments(sequential=True)
        configuration = mock_configuration()

        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ):
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-filter-directories",
                    "a;b",
                    "-workers",
                    "5",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check_dumb_terminal(
        self, directories_to_analyze, realpath, check_output, find_global_and_local_root
    ) -> None:
        realpath.side_effect = lambda x: x

        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ):
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                ],
            )
            exit_code = command.run().exit_code()
            self.assertEqual(exit_code, 0)
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check_strict(
        self, directories_to_analyze, realpath, check_output, find_global_and_local_root
    ) -> None:
        realpath.side_effect = lambda x: x

        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        configuration.strict = True

        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(
            json, "loads", return_value=NO_ERROR_JSON_OUTPUT
        ):
            command = commands.Check(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-strict",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...commands import check
from ...find_directories import FoundRoot
from ..command import __name__ as client_name
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(check.__name__)


NO_ERROR_JSON_OUTPUT = {"errors": []}


class CheckTest(unittest.TestCase):
    @patch(
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
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
                analysis_directory=AnalysisDirectory("."),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
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
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
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
                analysis_directory=AnalysisDirectory("."),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(
        commands.Reporting, "_get_directories_to_analyze", return_value={"a", "b"}
    )
    @patch(
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
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
                analysis_directory=AnalysisDirectory("."),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-filter-directories",
                    "a;b",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch(
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
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
                analysis_directory=AnalysisDirectory("."),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            exit_code = command.run().exit_code()
            self.assertEqual(exit_code, 0)
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch(
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
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
                analysis_directory=AnalysisDirectory("."),
            )
            self.assertEqual(
                command._flags(),
                [
                    "-strict",
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

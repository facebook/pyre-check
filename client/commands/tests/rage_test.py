# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import sys
import unittest
from pathlib import Path
from typing import IO, List, Optional
from unittest.mock import MagicMock, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...commands.command import Result
from ...commands.rage import Rage
from ...commands.servers import ServerDetails
from .command_test import mock_arguments, mock_configuration


def _call_client_side_effect(
    command: str, capture_output: bool = True, stdout: Optional[IO[str]] = None
) -> Result:
    if stdout:
        stdout.write("<SERVER RAGE>")
    return Result(output="", code=0)


class RageTest(unittest.TestCase):
    def test_flags(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")
        self.assertEqual(
            Rage(
                arguments,
                original_directory=original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                output_path=None,
            )._flags(),
            ["-log-directory", ".pyre"],
        )

    def assert_output(self, output: io.StringIO) -> None:
        lines = output.getvalue().split("\n")
        self.assertEqual(len(lines), 4)
        self.assertTrue(lines[0].startswith("Client version:"))
        self.assertTrue(lines[1].startswith("Binary path:"))
        self.assertTrue(lines[2].startswith("Configured binary version:"))
        self.assertEqual(lines[3], "<SERVER RAGE>")

    @patch("sys.stdout", new_callable=io.StringIO)
    @patch("subprocess.run")
    @patch.object(
        commands.Command, "_call_client", side_effect=_call_client_side_effect
    )
    def test_terminal_output(
        self, call_client: MagicMock, run: MagicMock, stdout: MagicMock
    ) -> None:
        arguments = mock_arguments(local_configuration="foo/bar")
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")

        Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        ).run()
        call_client.assert_called_once_with(
            command=Rage.NAME, capture_output=False, stdout=sys.stdout
        )
        self.assert_output(stdout)

    @patch("subprocess.run")
    @patch.object(
        commands.Command, "_call_client", side_effect=_call_client_side_effect
    )
    def test_file_output(self, call_client: MagicMock, run: MagicMock) -> None:
        arguments = mock_arguments(local_configuration="foo/bar")
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")

        # We cannot use StringIO directly because .getvalue() fails after the
        # file is closed
        output_content = io.StringIO()
        output_file = MagicMock()
        output_file.__enter__ = lambda x: x
        output_file.write = output_content.write
        output_file.flush = output_content.flush

        with patch("builtins.open", return_value=output_file) as open:
            Rage(
                arguments,
                original_directory=original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                output_path="/output",
            ).run()
            open.assert_called_once_with("/output", "w")
            call_client.assert_called_once_with(
                command=Rage.NAME, capture_output=False, stdout=output_file
            )
            self.assert_output(output_content)

    @patch.object(commands.Servers, "_all_server_details")
    @patch.object(commands.Command, "_call_client")
    def test_call_client_for_root_project__server_exists(
        self, call_client: MagicMock, all_server_details: MagicMock
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")
        rage_command = Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        )
        rage_command._log_directory_for_binary = "/some-log-directory"
        all_server_details.return_value = [
            ServerDetails(pid=1, local_root=".", server_pid_path=Path("something")),
            ServerDetails(pid=1, local_root="bar", server_pid_path=Path("something")),
        ]
        rage_command._call_client_for_root_project(sys.stdout)
        call_client.assert_called_once()

    @patch.object(commands.Servers, "_all_server_details")
    @patch.object(commands.Command, "_call_client", autospec=True)
    def test_call_client_for_root_project__server_does_not_exist(
        self, call_client: MagicMock, all_server_details: MagicMock
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")
        rage_command = Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        )

        all_server_details.return_value = [
            ServerDetails(pid=1, local_root="foo", server_pid_path=Path("something")),
            ServerDetails(pid=1, local_root="bar", server_pid_path=Path("something")),
        ]

        log_directories: List[str] = []

        def mock_call_client(_object: Rage, **kwargs: object) -> MagicMock:
            """Capture the log directory that will be passed to the binary."""
            log_directories.append(_object._log_directory_for_binary)
            return MagicMock()

        call_client.side_effect = mock_call_client
        rage_command._call_client_for_root_project(sys.stdout)

        self.assertEqual(call_client.call_count, 2)
        self.assertEqual(log_directories, [".pyre/foo", ".pyre/bar"])

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest
from unittest.mock import MagicMock, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...commands import command
from .command_test import mock_arguments, mock_configuration


class RageTest(unittest.TestCase):
    def test_flags(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")
        self.assertEqual(
            commands.Rage(
                arguments, original_directory, configuration, analysis_directory
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
    @patch.object(
        commands.Command,
        "_call_client",
        return_value=command.Result(output="<SERVER RAGE>", code=0),
    )
    def test_terminal_output(self, call_client: MagicMock, stdout: MagicMock) -> None:
        arguments = mock_arguments()
        arguments.output_path = None
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")

        commands.Rage(
            arguments, original_directory, configuration, analysis_directory
        ).run()
        call_client.assert_called_once_with(
            command=commands.Rage.NAME, capture_output=True
        )
        self.assert_output(stdout)

    @patch.object(
        commands.Command,
        "_call_client",
        return_value=command.Result(output="<SERVER RAGE>", code=0),
    )
    def test_file_output(self, call_client: MagicMock) -> None:
        arguments = mock_arguments()
        arguments.output_path = "/output"
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
            commands.Rage(
                arguments, original_directory, configuration, analysis_directory
            ).run()
            open.assert_called_once_with("/output", "w")
            call_client.assert_called_once_with(
                command=commands.Rage.NAME, capture_output=True
            )
            self.assert_output(output_content)

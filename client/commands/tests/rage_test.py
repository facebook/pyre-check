# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import sys
import unittest
from typing import IO, List, Optional
from unittest.mock import MagicMock, patch

from ... import (
    commands,
    filesystem,
    recently_used_configurations,
    configuration as configuration_module,
)
from ...analysis_directory import AnalysisDirectory
from ...commands.command import Result
from ...commands.rage import Rage
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
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
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
        self.assertTrue(lines[0].startswith("Client version:"))
        self.assertTrue(lines[1].startswith("Binary path:"))
        self.assertTrue(lines[2].startswith("Configured binary version:"))
        self.assertEqual(lines[3], "<SERVER RAGE>")

    @patch.object(recently_used_configurations.Cache, "get_all_items", return_value=[])
    @patch("sys.stdout", new_callable=io.StringIO)
    @patch("subprocess.run")
    @patch.object(
        commands.Command, "_call_client", side_effect=_call_client_side_effect
    )
    def test_terminal_output(
        self,
        call_client: MagicMock,
        run: MagicMock,
        stdout: MagicMock,
        get_recently_used_configurations: MagicMock,
    ) -> None:
        arguments = mock_arguments(local_configuration="foo/bar")
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )

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

    @patch.object(recently_used_configurations.Cache, "get_all_items", return_value=[])
    # pyre-fixme[56]: Argument `tools.pyre.client.filesystem` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(filesystem, "acquire_lock")
    @patch("subprocess.run")
    @patch.object(
        commands.Command, "_call_client", side_effect=_call_client_side_effect
    )
    def test_file_output(
        self,
        call_client: MagicMock,
        run: MagicMock,
        acquire_lock: MagicMock,
        get_recently_used_configurations: MagicMock,
    ) -> None:
        arguments = mock_arguments(local_configuration="foo/bar")
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )

        # We cannot use StringIO directly because .getvalue() fails after the
        # file is closed
        output_content = io.StringIO()
        output_file = MagicMock()
        output_file.__enter__ = lambda x: x
        output_file.write = output_content.write
        output_file.flush = output_content.flush

        outer_output_file: IO[str] = output_file

        output_path: str = "/output"

        def _open(file: str, mode: str) -> IO[str]:
            if file == output_path:
                return outer_output_file
            return MagicMock()

        with patch("builtins.open", side_effect=_open) as open:
            Rage(
                arguments,
                original_directory=original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                output_path=output_path,
            ).run()
            open.assert_called_once_with(output_path, "w")
            call_client.assert_called_once_with(
                command=Rage.NAME, capture_output=False, stdout=output_file
            )
            self.assert_output(output_content)

    @patch.object(recently_used_configurations.Cache, "get_all_items")
    @patch.object(commands.Servers, "is_root_server_running", return_value=True)
    @patch.object(commands.Command, "_call_client")
    def test_call_client_for_root_project__root_server_running(
        self,
        call_client: MagicMock,
        is_root_server_running: MagicMock,
        get_recently_used_configurations: MagicMock,
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        rage_command = Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        )
        rage_command._log_directory_for_binary = "/some-log-directory"
        rage_command._call_client_for_root_project(sys.stdout)
        call_client.assert_called_once()

    @patch.object(recently_used_configurations.Cache, "get_all_items", return_value=[])
    @patch.object(commands.Servers, "is_root_server_running", return_value=False)
    @patch.object(commands.Command, "_call_client")
    def test_call_client_for_root_project__no_root_server_or_recent_local_roots(
        self,
        call_client: MagicMock,
        is_root_server_running: MagicMock,
        get_recently_used_configurations: MagicMock,
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        rage_command = Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        )
        rage_command._log_directory_for_binary = "/some-log-directory"
        rage_command._call_client_for_root_project(sys.stdout)
        call_client.assert_called_once()

    @patch.object(
        recently_used_configurations.Cache,
        "get_all_items",
        return_value=["foo", "bar/baz"],
    )
    @patch.object(commands.Servers, "is_root_server_running", return_value=False)
    @patch.object(commands.Command, "_call_client", autospec=True)
    def test_call_client_for_root_project__recent_local_roots(
        self,
        call_client: MagicMock,
        is_root_server_running: MagicMock,
        get_recently_used_configurations: MagicMock,
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        rage_command = Rage(
            arguments,
            original_directory=original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            output_path=None,
        )

        log_directories: List[str] = []

        def mock_call_client(_object: Rage, **kwargs: object) -> MagicMock:
            """Capture the log directory that will be passed to the binary."""
            log_directories.append(_object._log_directory_for_binary)
            return MagicMock()

        call_client.side_effect = mock_call_client
        rage_command._call_client_for_root_project(sys.stdout)

        self.assertEqual(call_client.call_count, 2)
        self.assertEqual(log_directories, [".pyre/foo", ".pyre/bar/baz"])

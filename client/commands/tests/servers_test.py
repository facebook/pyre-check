# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from ... import command_arguments, commands, configuration as configuration_module, log
from ...analysis_directory import AnalysisDirectory
from .. import servers
from ..servers import ServerDetails, Servers
from .command_test import mock_arguments, mock_configuration


class ServersCommandTest(unittest.TestCase):
    @patch.object(log.stdout, "write")
    def test_print_server_details(self, log_stdout: MagicMock) -> None:
        commands.Servers._print_server_details(
            [
                ServerDetails(
                    pid=789,
                    local_root=".",
                    server_pid_path=Path("/root/.pyre/server/server.pid"),
                ),
                ServerDetails(
                    pid=456,
                    local_root="bar/baz",
                    server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
                ),
                ServerDetails(
                    pid=123,
                    local_root="foo",
                    server_pid_path=Path("/root/.pyre/foo/server/server.pid"),
                ),
            ],
            output_format=command_arguments.TEXT,
        )
        log_stdout.assert_has_calls(
            [
                call("PID       project\n"),
                call("789       <root>\n"),
                call("456       bar/baz\n"),
                call("123       foo\n"),
            ]
        )

        log_stdout.reset_mock()
        commands.Servers._print_server_details(
            [
                ServerDetails(
                    pid=789,
                    local_root=".",
                    server_pid_path=Path("/root/.pyre/server/server.pid"),
                ),
                ServerDetails(
                    pid=456,
                    local_root="bar/baz",
                    server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
                ),
                ServerDetails(
                    pid=123,
                    local_root="foo",
                    server_pid_path=Path("/root/.pyre/foo/server/server.pid"),
                ),
            ],
            output_format=command_arguments.JSON,
        )
        log_stdout.assert_called_once_with(
            json.dumps(
                [
                    {"pid": 789, "relative_local_root": None},
                    {"pid": 456, "relative_local_root": "bar/baz"},
                    {"pid": 123, "relative_local_root": "foo"},
                ]
            )
        )

    @patch.object(
        configuration_module, "create_configuration", return_value=mock_configuration()
    )
    @patch.object(Path, "mkdir")
    # pyre-fixme[56]: Argument `tools.pyre.client.commands.servers` to decorator
    #  factory `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(servers, "Stop")
    def test_stop_servers(
        self,
        stop_class: MagicMock,
        make_directory: MagicMock,
        create_configuration: MagicMock,
    ) -> None:
        servers = Servers(
            command_arguments=mock_arguments(
                dot_pyre_directory=Path("/dot-pyre-directory/.pyre")
            ),
            original_directory="/root",
            configuration=mock_configuration(),
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            subcommand="stop",
        )
        servers._stop_servers(
            [
                ServerDetails(
                    pid=789,
                    local_root=".",
                    server_pid_path=Path("/dot-pyre-directory/.pyre/server/server.pid"),
                ),
                ServerDetails(
                    pid=456,
                    local_root="bar/baz",
                    server_pid_path=Path(
                        "/dot-pyre-directory/.pyre/bar/baz/server/server.pid"
                    ),
                ),
                ServerDetails(
                    pid=123,
                    local_root="foo",
                    server_pid_path=Path(
                        "/dot-pyre-directory/.pyre/foo/server/server.pid"
                    ),
                ),
            ]
        )
        self.assertEqual(servers._configuration.project_root, "/root")
        stop_class.assert_called()
        create_configuration.assert_has_calls(
            [
                call(
                    dataclasses.replace(
                        servers._command_arguments, local_configuration="."
                    ),
                    Path("/root"),
                ),
                call(
                    dataclasses.replace(
                        servers._command_arguments, local_configuration="bar/baz"
                    ),
                    Path("/root"),
                ),
                call(
                    dataclasses.replace(
                        servers._command_arguments, local_configuration="foo"
                    ),
                    Path("/root"),
                ),
            ],
            any_order=True,
        )

    @patch.object(Servers, "_find_servers")
    @patch.object(ServerDetails, "_from_server_path")
    @patch.object(Servers, "_print_server_details")
    @patch.object(Servers, "_stop_servers")
    def test_run(
        self,
        stop_servers: MagicMock,
        print_server_details: MagicMock,
        find_servers: MagicMock,
        fetch_server_details: MagicMock,
    ) -> None:
        arguments = mock_arguments()
        servers = Servers(
            arguments,
            original_directory="/",
            configuration=mock_configuration(),
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            subcommand=None,
        )
        servers._run()
        print_server_details.assert_called_once()
        print_server_details.reset_mock()

        arguments = mock_arguments()
        servers = Servers(
            arguments,
            original_directory="/",
            configuration=mock_configuration(),
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            subcommand="list",
        )
        servers._run()
        print_server_details.assert_called_once()
        print_server_details.reset_mock()

        arguments = mock_arguments()
        servers = Servers(
            arguments,
            original_directory="/",
            configuration=mock_configuration(),
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement(".")
            ),
            subcommand="stop",
        )
        servers._run()
        stop_servers.assert_called_once()
        print_server_details.assert_not_called()


class ServerDetailsTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_from_server_path(self, read_text: MagicMock) -> None:
        read_text.side_effect = ["123", "789"]
        self.assertEqual(
            ServerDetails._from_server_path(
                Path("/root/.pyre/bar/baz/server/server.pid"), Path("/root/.pyre")
            ),
            ServerDetails(
                pid=123,
                local_root="bar/baz",
                server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
            ),
        )
        self.assertEqual(
            ServerDetails._from_server_path(
                Path("/root/.pyre/server/server.pid"), Path("/root/.pyre")
            ),
            ServerDetails(
                pid=789,
                local_root=".",
                server_pid_path=Path("/root/.pyre/server/server.pid"),
            ),
        )

    def test_is_root(self) -> None:
        self.assertTrue(
            ServerDetails(
                pid=1, local_root=".", server_pid_path=Path("something")
            ).is_root()
        )
        self.assertFalse(
            ServerDetails(
                pid=1, local_root="foo/bar", server_pid_path=Path("something")
            ).is_root()
        )

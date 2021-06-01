# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import socket
import tempfile
import threading
import unittest
from unittest.mock import MagicMock, patch

from .. import json_rpc, project_files_monitor
from ..analysis_directory import UpdatedPaths
from ..json_rpc import Request, read_lsp_request
from ..project_files_monitor import MonitorException, ProjectFilesMonitor
from ..socket_connection import SocketConnection, SocketException
from ..tests.mocks import mock_configuration


class MonitorTest(unittest.TestCase):
    @patch.object(SocketConnection, "connect")
    @patch.object(json_rpc, "perform_handshake")
    @patch.object(project_files_monitor, "find_parent_directory_containing_file")
    def test_subscriptions(
        self,
        find_parent_directory_containing_file,
        perform_handshake,
        _socket_connection,
    ) -> None:
        find_parent_directory_containing_file.return_value = "/ROOT"
        configuration = mock_configuration()
        analysis_directory = MagicMock()
        analysis_directory.get_root.return_value = "/ROOT"

        # no additional extensions
        configuration.extensions = []
        monitor = ProjectFilesMonitor(configuration, ".", analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:2], ["allof", ["type", "f"]]
        )
        self.assertCountEqual(
            subscription.subscription["expression"][2],
            [
                "anyof",
                ["suffix", "py"],
                ["suffix", "pyi"],
                ["suffix", "thrift"],
                ["match", "TARGETS"],
            ],
        )

        # additional extensions
        configuration.get_valid_extension_suffixes = lambda: [".thrift", ".whl"]
        monitor = ProjectFilesMonitor(configuration, ".", analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:2], ["allof", ["type", "f"]]
        )
        self.assertCountEqual(
            subscription.subscription["expression"][2],
            [
                "anyof",
                ["suffix", "py"],
                ["suffix", "pyi"],
                ["suffix", "thrift"],
                ["suffix", "whl"],
                ["match", "TARGETS"],
            ],
        )

        # no watchman root -> terminate
        find_parent_directory_containing_file.return_value = None
        self.assertRaises(
            MonitorException,
            ProjectFilesMonitor,
            configuration,
            ".",
            analysis_directory,
        )

    def test_bad_socket(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            bad_socket_path = os.path.join(root, "bad.sock")
            socket_connection = SocketConnection(bad_socket_path)
            self.assertRaises(SocketException, socket_connection.connect)

    @patch.object(ProjectFilesMonitor, "_find_watchman_path")
    def test_socket_communication(self, _find_watchman_path) -> None:
        # Create a "server" thread to complete the handshake
        server_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        errors = []

        with tempfile.TemporaryDirectory() as root:
            socket_path = os.path.join(root, ".pyre", "server", "json_server.sock")
            os.makedirs(os.path.dirname(socket_path))

            socket_created_lock = threading.Lock()
            socket_created_lock.acquire()  # hold lock until server creates socket

            def server():
                server_socket.bind(socket_path)
                server_socket.listen(1)
                socket_created_lock.release()
                connection, _ = server_socket.accept()

                outfile = connection.makefile(mode="wb")
                infile = connection.makefile(mode="rb")
                request = Request(
                    method="handshake/server",
                    parameters=json_rpc.ByNameParameters({"version": "123"}),
                )
                json_rpc.write_lsp_request(outfile, request)

                response = read_lsp_request(infile)
                if response.method != "handshake/client":
                    errors.append("Client handshake malformed")
                    return

                request = Request(method="handshake/socket_added")
                json_rpc.write_lsp_request(outfile, request)

                updated_message = read_lsp_request(infile)
                if (
                    updated_message.method != "updateFiles"
                    or not updated_message.parameters
                    or updated_message.parameters.get("files")
                    != ["/ANALYSIS/a.py", "/ANALYSIS/subdir/b.py"]
                ):
                    errors.append("Update message malformed")

            server_thread = threading.Thread(target=server)
            server_thread.start()

            configuration = mock_configuration(version_hash="123")
            configuration.log_directory = root + "/.pyre"
            configuration.extensions = []
            analysis_directory = MagicMock()
            analysis_directory.process_updated_files.side_effect = (
                lambda files: UpdatedPaths(
                    updated_paths=[file.replace("ROOT", "ANALYSIS") for file in files],
                    deleted_paths=[],
                )
            )

            # only create the monitor once the socket is open
            with socket_created_lock:
                monitor = ProjectFilesMonitor(configuration, ".", analysis_directory)
                monitor._handle_response(
                    {"root": "/ROOT", "files": ["a.py", "subdir/b.py"]}
                )
                analysis_directory.process_updated_files.assert_called_once_with(
                    ["/ROOT/a.py", "/ROOT/subdir/b.py"]
                )

            server_thread.join()

        self.assertEqual(errors, [])

    @patch.object(os.path, "realpath")
    def test_socket_connection(self, realpath) -> None:
        server_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

        with tempfile.TemporaryDirectory() as root:
            realpath.side_effect = lambda path: path.replace(
                os.path.dirname(path), root  # replace parent directories with tempdir
            )

            # Unix sockets have a limited length of ~100 characters, so the server uses
            # symbolic links as a workaround. We need to properly translate these.
            socket_link = os.path.join(
                ".pyre", "long_name" * 15, "server", "json_server.sock"
            )

            socket_path = os.path.join(root, "json_server.sock")

            socket_created_lock = threading.Lock()
            socket_created_lock.acquire()  # hold lock until server creates socket

            def server():
                server_socket.bind(socket_path)
                server_socket.listen(1)
                socket_created_lock.release()
                connection, _ = server_socket.accept()

            server_thread = threading.Thread(target=server)
            server_thread.start()

            with socket_created_lock:
                SocketConnection(socket_link).connect()
            server_thread.join()

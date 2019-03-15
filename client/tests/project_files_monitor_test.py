# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import socket
import tempfile
import threading
import unittest
from unittest.mock import MagicMock, patch

from .. import language_server_protocol, project_files_monitor
from ..language_server_protocol import (
    LanguageServerProtocolMessage,
    read_message,
    write_message,
)
from ..project_files_monitor import (
    ProjectFilesMonitor,
    ProjectFilesMonitorException,
    SocketConnection,
)


class ProjectFilesMonitorTest(unittest.TestCase):
    @patch.object(language_server_protocol, "perform_handshake")
    @patch.object(ProjectFilesMonitor, "_connect_to_socket")
    @patch.object(project_files_monitor, "find_root")
    def test_subscriptions(self, find_root, _connect_to_socket, perform_handshake):
        find_root.return_value = "/ROOT"

        arguments = MagicMock()
        configuration = MagicMock()
        analysis_directory = MagicMock()
        analysis_directory.get_root.return_value = "/ROOT"

        # no additional extensions
        configuration.extensions = []
        monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:3],
            ["allof", ["type", "f"], ["not", "empty"]],
        )
        self.assertCountEqual(
            subscription.subscription["expression"][3],
            ["anyof", ["suffix", "py"], ["suffix", "pyi"]],
        )

        # additional extensions
        configuration.extensions = ["thrift", "whl"]
        monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
        self.assertEqual(len(monitor._subscriptions), 1)
        subscription = monitor._subscriptions[0]
        self.assertEqual(subscription.root, "/ROOT")
        self.assertEqual(subscription.name, "pyre_file_change_subscription")
        self.assertEqual(subscription.subscription["fields"], ["name"])
        self.assertEqual(
            subscription.subscription["expression"][0:3],
            ["allof", ["type", "f"], ["not", "empty"]],
        )
        self.assertCountEqual(
            subscription.subscription["expression"][3],
            [
                "anyof",
                ["suffix", "py"],
                ["suffix", "pyi"],
                ["suffix", "thrift"],
                ["suffix", "whl"],
            ],
        )

        # no watchman root -> terminate
        find_root.return_value = None
        self.assertRaises(
            ProjectFilesMonitorException,
            ProjectFilesMonitor,
            arguments,
            configuration,
            analysis_directory,
        )

    @patch.object(project_files_monitor, "find_paths_with_extensions")
    @patch.object(
        os.path,
        "realpath",
        side_effect=lambda path: path.replace("ANALYSIS_ROOT", "LOCAL_ROOT"),
    )
    def test_calculate_symbolic_links(self, realpath, find_paths_with_extensions):
        find_paths_with_extensions.return_value = [
            "ANALYSIS_ROOT/a.py",
            "ANALYSIS_ROOT/b.thrift",
            "ANALYSIS_ROOT/subX/d.pyi",
            "ANALYSIS_ROOT/subX/e.py",
            "ANALYSIS_ROOT/subY/subZ/g.pyi",
        ]

        self.assertDictEqual(
            ProjectFilesMonitor._calculate_symbolic_links(
                "ANALYSIS_ROOT", ["py", "pyi", "thrift"]
            ),
            {
                "LOCAL_ROOT/a.py": "ANALYSIS_ROOT/a.py",
                "LOCAL_ROOT/b.thrift": "ANALYSIS_ROOT/b.thrift",
                "LOCAL_ROOT/subX/d.pyi": "ANALYSIS_ROOT/subX/d.pyi",
                "LOCAL_ROOT/subX/e.py": "ANALYSIS_ROOT/subX/e.py",
                "LOCAL_ROOT/subY/subZ/g.pyi": "ANALYSIS_ROOT/subY/subZ/g.pyi",
            },
        )

    def test_bad_socket(self):
        with tempfile.TemporaryDirectory() as root:
            bad_socket_path = os.path.join(root, "bad.sock")
            self.assertRaises(
                ProjectFilesMonitorException,
                ProjectFilesMonitor._connect_to_socket,
                bad_socket_path,
            )

    @patch.object(ProjectFilesMonitor, "_find_watchman_path")
    def test_socket_communication(self, _find_watchman_path):
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
                write_message(
                    outfile,
                    LanguageServerProtocolMessage(
                        method="handshake/server", parameters={"version": "123"}
                    ),
                )

                response = read_message(infile)
                if not response or response.method != "handshake/client":
                    errors.append("Client handshake malformed")
                    return

                updated_message = read_message(infile)
                if (
                    not updated_message
                    or updated_message.method != "updateFiles"
                    or not updated_message.parameters
                    or updated_message.parameters.get("files")
                    != ["/ANALYSIS/a.py", "/ANALYSIS/subdir/b.py"]
                ):
                    errors.append("Update message malformed")

            server_thread = threading.Thread(target=server)
            server_thread.start()

            arguments = MagicMock()
            configuration = MagicMock()
            configuration.extensions = []
            configuration.version_hash = "123"
            analysis_directory = MagicMock()
            analysis_directory.get_root.return_value = root

            # only create the monitor once the socket is open
            with socket_created_lock:
                monitor = ProjectFilesMonitor(
                    arguments, configuration, analysis_directory
                )
                monitor._symbolic_links["/ROOT/a.py"] = "/ANALYSIS/a.py"
                monitor._symbolic_links["/ROOT/subdir/b.py"] = "/ANALYSIS/subdir/b.py"

                monitor._handle_response(
                    {"root": "/ROOT", "files": ["a.py", "subdir/b.py", "untracked.py"]}
                )

            server_thread.join()

        self.assertEqual(errors, [])

    @patch.object(language_server_protocol, "perform_handshake")
    @patch.object(ProjectFilesMonitor, "_watchman_client")
    @patch.object(ProjectFilesMonitor, "_connect_to_socket")
    @patch.object(ProjectFilesMonitor, "_find_watchman_path")
    def test_files_cleaned_up(
        self,
        _find_watchman_path,
        _connect_to_socket,
        _watchman_client,
        perform_handshake,
    ):
        with tempfile.TemporaryDirectory() as root:
            arguments = MagicMock()
            configuration = MagicMock()
            configuration.extensions = []
            analysis_directory = MagicMock()
            analysis_directory.get_root.return_value = root

            monitor = ProjectFilesMonitor(arguments, configuration, analysis_directory)
            monitor._alive = False  # never enter watchman loop
            monitor._run()

            monitor_folder = os.path.join(root, ".pyre", "file_monitor")
            self.assertFalse(
                os.path.exists(os.path.join(monitor_folder, "file_monitor.lock"))
            )
            self.assertFalse(
                os.path.exists(os.path.join(monitor_folder, "file_monitor.pid"))
            )

    def test_socket_connection(self):
        server_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

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

            server_thread = threading.Thread(target=server)
            server_thread.start()

            with socket_created_lock:
                SocketConnection(socket_path)
            server_thread.join()

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
from ..project_files_monitor import ProjectFilesMonitor, ProjectFilesMonitorException


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
                ProjectFilesMonitor(arguments, configuration, analysis_directory)

            server_thread.join()

        self.assertEqual(errors, [])

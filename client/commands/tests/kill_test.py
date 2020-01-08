# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import shutil
import signal
import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, call, patch

import psutil

from ... import commands
from .. import kill
from ..kill import Kill, _get_process_name


class KillTest(unittest.TestCase):
    @patch.object(os, "getenv")
    def test_get_process_name(self, get_environment: MagicMock) -> None:
        get_environment.return_value = None
        self.assertEqual(_get_process_name("PYRE_BINARY", "foo"), "foo")
        get_environment.return_value = "/tmp/pyre_directory/main.exe"
        self.assertEqual(_get_process_name("PYRE_BINARY", "foo"), "main.exe")

    @patch.object(commands.stop.WatchmanSubscriber, "stop_subscriber")
    @patch.object(psutil, "process_iter")
    @patch.object(os, "getpgid", side_effect=lambda id: id)
    @patch.object(os, "getpid", return_value=1234)
    @patch.object(os, "kill")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill_client_processes(
        self,
        kill_init: MagicMock,
        os_kill: MagicMock,
        get_process_id: MagicMock,
        get_process_group_id: MagicMock,
        process_iterator: MagicMock,
        stop_subscriber: MagicMock,
    ) -> None:
        process_iterator.return_value = [
            Mock(info={"name": "pyre-client"}, pid=1234),
            Mock(info={"name": "pyre-client"}, pid=5678),
            Mock(info={"name": "not-pyre-client"}, pid=4321),
            Mock(info={"name": "pyre-client"}, pid=9101),
        ]
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._configuration = MagicMock(log_directory=".pyre")
        kill_command._kill_client_processes()
        os_kill.assert_has_calls(
            [call(5678, signal.SIGKILL), call(9101, signal.SIGKILL)]
        )
        stop_subscriber.assert_has_calls(
            [
                call(".pyre/file_monitor", "file_monitor"),
                call(".pyre/configuration_monitor", "configuration_monitor"),
            ]
        )

        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._configuration = MagicMock(log_directory=".pyre")
        os_kill.side_effect = ProcessLookupError
        # Ensure that we don't crash even if os.kill fails to find a process.
        kill_command._kill_client_processes()

    @patch.object(shutil, "rmtree")
    @patch.object(Kill, "__init__", return_value=None)
    def test_delete_caches(self, kill_init: MagicMock, remove_tree: MagicMock) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._current_directory = "/root"
        kill_command._log_directory = "/root/.pyre/foo"
        kill_command._delete_caches()
        remove_tree.assert_has_calls(
            [call("/root/.pyre/resource_cache"), call("/tmp/pyre/buck_builder_cache")]
        )

    @patch.object(os, "remove")
    @patch.object(os, "unlink")
    @patch.object(os, "readlink", return_value="/tmp/actual_socket")
    def test_delete_linked_paths(
        self, readlink: MagicMock, unlink: MagicMock, remove: MagicMock
    ) -> None:
        socket_path = Path("foo.sock")
        Kill._delete_linked_path(socket_path)
        remove.assert_called_once_with("/tmp/actual_socket")
        unlink.assert_called_once_with(socket_path)

    @patch.object(subprocess, "run")
    @patch.object(kill, "_get_process_name", return_value="foo.exe")
    def test_kill_binary_processes(
        self, get_process_name: MagicMock, run: MagicMock
    ) -> None:
        Kill._kill_binary_processes()
        run.assert_called_once_with(["pkill", "foo.exe"])

    @patch.object(Kill, "_delete_linked_path")
    @patch.object(kill, "Path")
    @patch.object(Kill, "__init__", return_value=None)
    def test_delete_server_files(
        self, kill_init: MagicMock, path_class: MagicMock, delete_linked_path: MagicMock
    ) -> None:
        def path_constructor(*subpaths: str) -> Mock:
            if "/".join(subpaths).endswith(".pyre"):
                # The project root has two local server sockets under it.
                return Mock(glob=Mock(return_value=["a.sock", "b.sock"]))
            else:
                # The local server has just one socket under it.
                return Mock(glob=Mock(return_value=["a.sock"]))

        path_class.side_effect = path_constructor
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._current_directory = "/root/.pyre"
        kill_command._delete_server_files()
        self.assertEqual(delete_linked_path.call_count, 6)

    @patch.object(Kill, "_delete_server_files")
    @patch.object(Kill, "_delete_caches")
    @patch.object(Kill, "_kill_client_processes")
    @patch.object(Kill, "_kill_binary_processes")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill(
        self,
        kill_init: MagicMock,
        kill_binary_processes: MagicMock,
        kill_client_processes: MagicMock,
        delete_caches: MagicMock,
        delete_server_files: MagicMock,
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._log_directory = ".pyre"
        kill_command._arguments = Mock(with_fire=False)
        kill_command._configuration = Mock()
        kill_command._run()

        delete_caches.assert_not_called()

        kill_binary_processes.assert_called_once()
        kill_client_processes.assert_called_once()
        delete_server_files.assert_called_once()

        kill_command._arguments = Mock(with_fire=True)
        kill_command._run()

        delete_caches.assert_called_once()
        self.assertEqual(kill_binary_processes.call_count, 2)
        self.assertEqual(kill_client_processes.call_count, 2)
        self.assertEqual(delete_server_files.call_count, 2)

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import shutil
import signal
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, call, patch

import psutil

from ... import commands
from ...analysis_directory import AnalysisDirectory
from .. import kill
from ..kill import Kill, _get_process_name
from .command_test import mock_arguments, mock_configuration


class KillTest(unittest.TestCase):
    @patch.object(os, "getenv")
    def test_get_process_name(self, get_environment: MagicMock) -> None:
        get_environment.return_value = None
        self.assertEqual(_get_process_name("PYRE_BINARY", "foo"), "foo")
        get_environment.return_value = "/tmp/pyre_directory/main.exe"
        self.assertEqual(_get_process_name("PYRE_BINARY", "foo"), "main.exe")

    @patch.object(kill, "Rage")
    @patch.object(
        tempfile, "NamedTemporaryFile", return_value=MagicMock(name="/tmp/file")
    )
    def test_rage(self, named_temporary_file: MagicMock, rage: MagicMock) -> None:
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")

        kill_command = Kill(
            arguments, original_directory, configuration, analysis_directory
        )
        kill_command._rage()

        named_temporary_file.assert_called_once_with(
            prefix="pyre-rage-", suffix=".log", delete=False
        )
        rage.assert_called_with(
            arguments, original_directory, configuration, analysis_directory
        )

    @patch.object(psutil, "process_iter")
    @patch.object(os, "getpgid", side_effect=lambda id: id)
    @patch.object(os, "getpid", return_value=1234)
    @patch.object(os, "kill")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill_processes_by_name(
        self,
        kill_init: MagicMock,
        os_kill: MagicMock,
        get_process_id: MagicMock,
        get_process_group_id: MagicMock,
        process_iterator: MagicMock,
    ) -> None:
        process_iterator.return_value = [
            Mock(info={"name": "pyre-client"}, pid=1234),
            Mock(info={"name": "pyre-client"}, pid=5678),
            Mock(info={"name": "not-pyre-client"}, pid=4321),
            Mock(info={"name": "pyre-client"}, pid=9101),
        ]
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._kill_processes_by_name("pyre-client")
        os_kill.assert_has_calls(
            [call(5678, signal.SIGKILL), call(9101, signal.SIGKILL)]
        )

        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        os_kill.side_effect = ProcessLookupError
        # Ensure that we don't crash even if os.kill fails to find a process.
        kill_command._kill_processes_by_name("pyre-client")

        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        os_kill.side_effect = PermissionError
        # Ensure that we don't crash even if os.kill fails due to permissions.
        kill_command._kill_processes_by_name("pyre-client")

    @patch.object(commands.stop.WatchmanSubscriber, "stop_subscriber")
    @patch.object(Kill, "_kill_processes_by_name")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill_client_processes(
        self,
        kill_init: MagicMock,
        kill_processes_by_name: MagicMock,
        stop_subscriber: MagicMock,
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._configuration = MagicMock(log_directory=".pyre")
        kill_command._kill_client_processes()
        kill_processes_by_name.assert_called_with("pyre-client")
        stop_subscriber.assert_has_calls(
            [
                call(".pyre/file_monitor", "file_monitor"),
                call(".pyre/configuration_monitor", "configuration_monitor"),
            ]
        )

    @patch.object(Path, "glob")
    @patch.object(
        subprocess, "check_output", return_value=b"/some/scratch/directory/pyre\n"
    )
    @patch.object(shutil, "rmtree")
    @patch.object(Kill, "__init__", return_value=None)
    def test_delete_caches(
        self,
        kill_init: MagicMock,
        remove_tree: MagicMock,
        check_output: MagicMock,
        path_glob: MagicMock,
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._current_directory = "/root"
        kill_command._dot_pyre_directory = Path("/some/log/directory/.pyre")
        kill_command._log_directory = "/some/log/directory/.pyre/foo"
        path_glob.return_value = [
            "/some/scratch/directory/pyre/.buck_builder_cache",
            "/some/scratch/directory/pyre/.buck_builder_cache_isolated_1234",
        ]
        kill_command._delete_caches()
        remove_tree.assert_has_calls(
            [
                call("/some/log/directory/.pyre/resource_cache"),
                call("/some/scratch/directory/pyre/.buck_builder_cache"),
                call("/some/scratch/directory/pyre/.buck_builder_cache_isolated_1234"),
            ]
        )

    @patch.object(subprocess, "check_output")
    @patch.object(shutil, "rmtree")
    @patch.object(Kill, "__init__", return_value=None)
    def test_delete_caches_scratch_path_exception(
        self, kill_init: MagicMock, remove_tree: MagicMock, check_output: MagicMock
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._current_directory = "/root"
        kill_command._dot_pyre_directory = Path("/some/log/directory/.pyre")
        kill_command._log_directory = "/some/log/directory/.pyre/foo"
        check_output.side_effect = Exception
        kill_command._delete_caches()
        remove_tree.assert_has_calls([call("/some/log/directory/.pyre/resource_cache")])

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

    @patch.object(kill, "_get_process_name", return_value="foo.exe")
    @patch.object(Kill, "_kill_processes_by_name")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill_binary_processes(
        self,
        kill_init: MagicMock,
        kill_processes_by_name: MagicMock,
        get_process_name: MagicMock,
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._kill_binary_processes()
        kill_processes_by_name.assert_called_with("foo.exe")

    @patch.object(Kill, "_delete_linked_path")
    @patch.object(Path, "glob", return_value=["a.sock", "b.sock"])
    @patch.object(Kill, "__init__", return_value=None)
    def test_delete_server_files(
        self, kill_init: MagicMock, glob: MagicMock, delete_linked_path: MagicMock
    ) -> None:
        kill_command = Kill(MagicMock(), MagicMock(), MagicMock(), MagicMock())
        kill_command._dot_pyre_directory = Path("/root/.pyre")
        kill_command._delete_server_files()
        self.assertEqual(delete_linked_path.call_count, 6)

    @patch.object(Kill, "_delete_server_files")
    @patch.object(Kill, "_delete_caches")
    @patch.object(Kill, "_kill_client_processes")
    @patch.object(Kill, "_kill_binary_processes")
    @patch.object(Kill, "_rage")
    @patch.object(Kill, "__init__", return_value=None)
    def test_kill(
        self,
        kill_init: MagicMock,
        rage: MagicMock,
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

        rage.assert_called_once()
        kill_binary_processes.assert_called_once()
        kill_client_processes.assert_called_once()
        delete_caches.assert_called_once()
        delete_server_files.assert_called_once()

        kill_command._arguments = Mock(with_fire=True)
        kill_command._run()

        self.assertEqual(rage.call_count, 2)
        self.assertEqual(kill_binary_processes.call_count, 2)
        self.assertEqual(kill_client_processes.call_count, 2)
        self.assertEqual(delete_caches.call_count, 2)
        self.assertEqual(delete_server_files.call_count, 2)

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import shutil  # noqa
import signal
import subprocess
import unittest
from unittest.mock import MagicMock, call, patch

import psutil  # noqa

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class KillTest(unittest.TestCase):
    @patch("psutil.process_iter")
    @patch("os.getpgid", side_effect=lambda id: id)
    @patch("os.kill")
    @patch("os.remove")
    @patch("os.unlink")
    @patch(
        "os.readlink",
        side_effect=[
            "/tmp/actual_socket",
            "/tmp/json_socket",
            "/tmp/actual_socket",
            "/tmp/json_socket",
            "/tmp/actual_socket",
            "/tmp/json_socket",
            "/tmp/actual_socket",
            "/tmp/json_socket",
        ],
    )
    @patch("os.path.realpath")
    @patch("subprocess.run")
    def test_kill(
        self,
        run,
        realpath,
        readlink,
        unlink,
        remove,
        kill,
        _get_process_group_id,
        process_iter,
    ) -> None:
        processA = MagicMock()
        processA.info = {"name": "pyre-client"}
        processA.pid = 1234
        processB = MagicMock()
        processB.info = {"name": "pyre-client"}
        processB.pid = 5678
        processC = MagicMock()
        processC.info = {"name": "not-pyre-client"}
        processC.pid = 4321
        process_iter.return_value = [processA, processB, processC]
        with patch("os.getenv", return_value=None), patch(
            "os.getpid", return_value=1234
        ):
            realpath.return_value = "/test-binary"
            arguments = mock_arguments()
            configuration = mock_configuration()
            analysis_directory = MagicMock()
            commands.Kill(arguments, configuration, analysis_directory).run()
            run.assert_has_calls([call(["pkill", "pyre.bin"])])
            kill.assert_has_calls([call(5678, signal.SIGKILL)])
            remove.assert_has_calls(
                [call("/tmp/actual_socket"), call("/tmp/json_socket")]
            )
            unlink.assert_has_calls(
                [
                    call("/test-binary/.pyre/server/server.sock"),
                    call("/test-binary/.pyre/server/json_server.sock"),
                ]
            )
        with patch(
            "os.getenv",
            side_effect=["/tmp/pyre_directory/main.exe", "/tmp/pyre/my_client"],
        ):
            kill.reset_mock()
            realpath.return_value = "/test-binary"
            arguments = mock_arguments()
            configuration = mock_configuration()
            analysis_directory = MagicMock()
            commands.Kill(arguments, configuration, analysis_directory).run()
            run.assert_has_calls([call(["pkill", "main.exe"])])
            kill.assert_has_calls([call(5678, signal.SIGKILL)])

        with patch("os.getcwd", return_value="/root"), patch(
            "shutil.rmtree"
        ) as remove_tree:
            arguments = mock_arguments()
            configuration = mock_configuration()
            analysis_directory = MagicMock()
            arguments.with_fire = True
            commands.Kill(arguments, configuration, analysis_directory).run()
            remove_tree.assert_called_once_with("/root/.pyre/resource_cache")
        # Ensure that we don't crash even if os.kill fails to find a process.
        with patch("os.getenv", return_value=None), patch(
            "os.getpid", return_value=1234
        ), patch("os.kill", side_effect=ProcessLookupError):
            realpath.return_value = "/test-binary"
            arguments = mock_arguments()
            configuration = mock_configuration()
            analysis_directory = MagicMock()
            commands.Kill(arguments, configuration, analysis_directory).run()

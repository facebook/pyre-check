# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import signal
import unittest
from unittest.mock import call, mock_open, patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class KillTest(unittest.TestCase):

    @patch("os.kill")
    @patch("os.path.exists")
    def test_kill(self, os_path_exists, os_kill) -> None:
        os_path_exists.result = True

        arguments = mock_arguments()
        arguments.with_fire = False
        configuration = mock_configuration()

        with patch("builtins.open", mock_open(read_data="11")) as open:
            commands.Kill(
                arguments, configuration, source_directory="/some/link/tree/"
            ).run()
            open.assert_called_with("/some/link/tree/.pyre/watchman/watchman.pid", "r")
            os_kill.assert_has_calls(
                [call(11, signal.SIGTERM), call(11, signal.SIGTERM)]
            )

    @patch("os.path.realpath")
    @patch("subprocess.run")
    def test_kill_all(self, run, realpath) -> None:
        realpath.side_effect = ["/test-binary"]
        arguments = mock_arguments()
        arguments.with_fire = True
        configuration = mock_configuration()
        commands.Kill(arguments, configuration, source_directory=None).run()
        run.assert_called_with(["pkill", "-f", "pyre.bin"])

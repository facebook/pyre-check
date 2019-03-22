# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, call, patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class KillTest(unittest.TestCase):
    @patch("os.remove")
    @patch("os.unlink")
    @patch("os.readlink", side_effect=["/tmp/actual_socket", "/tmp/json_socket"])
    @patch("os.path.realpath")
    @patch("subprocess.run")
    def test_kill(self, run, realpath, readlink, unlink, remove) -> None:
        realpath.return_value = "/test-binary"
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = MagicMock()
        commands.Kill(arguments, configuration, analysis_directory).run()
        run.assert_called_with(["pkill", "pyre.bin"])
        remove.assert_has_calls([call("/tmp/actual_socket"), call("/tmp/json_socket")])
        unlink.assert_has_calls(
            [
                call("/test-binary/.pyre/server/server.sock"),
                call("/test-binary/.pyre/server/json_server.sock"),
            ]
        )

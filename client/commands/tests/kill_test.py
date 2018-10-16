# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class KillTest(unittest.TestCase):
    @patch("os.path.realpath")
    @patch("subprocess.run")
    def test_kill(self, run, realpath) -> None:
        realpath.side_effect = ["/test-binary"]
        arguments = mock_arguments()
        configuration = mock_configuration()
        commands.Kill(arguments, configuration, analysis_directory=None).run()
        run.assert_called_with(["pkill", "pyre.bin"])

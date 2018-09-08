# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import unittest
from unittest.mock import call, patch

from ... import EnvironmentException, commands, log
from ...commands import initialize
from ...filesystem import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class InitializeTest(unittest.TestCase):
    @patch.object(log, "get_yes_no_input", return_value=True)
    @patch.object(log, "get_input", return_value="")
    @patch("shutil.which")
    @patch("os.path.isfile")
    @patch("subprocess.call")
    @patch("builtins.open")
    def test_initialize(
        self, mock_open, subprocess_call, isfile, which, _get_input, get_yes_no_input
    ):
        get_yes_no_input.return_value = True
        arguments = mock_arguments()
        # pyre.py does not provide a Configuration instance to
        # Initialize - this test should do the same
        configuration = None

        def exists(path):
            if path.endswith(".watchmanconfig"):
                return False
            elif path.endswith(".pyre_configuration"):
                return False
            elif path.endswith(".pyre_configuration.local"):
                return False
            else:
                return True

        isfile.side_effect = exists
        # One for shutil.which("watchman"), another for shutil.which(BINARY_NAME).
        which.side_effect = [True, True]
        with patch.object(commands.Command, "_call_client"):
            initialize.Initialize(
                arguments, configuration, AnalysisDirectory(".")
            ).run()
            subprocess_call.assert_has_calls([call(["watchman", "watch-project", "."])])
            mock_open.assert_any_call(os.path.abspath(".watchmanconfig"), "w+")

        arguments.local = True

        def exists(path):
            return False

        isfile.side_effect = exists
        with patch.object(commands.Command, "_call_client"):
            initialize.Initialize(
                arguments, configuration, AnalysisDirectory(".")
            ).run()

        def exists(path):
            if path.endswith(".pyre_configuration"):
                return True
            return False

        isfile.side_effect = exists
        with patch.object(commands.Command, "_call_client"):
            with self.assertRaises(EnvironmentException):
                initialize.Initialize(
                    arguments, configuration, AnalysisDirectory(".")
                ).run()

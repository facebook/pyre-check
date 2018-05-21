# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import unittest
from unittest.mock import patch

from .. import buck, commands, configuration, pyre


class PyreTest(unittest.TestCase):

    @patch.object(configuration.Configuration, "validate")
    @patch.object(configuration.Configuration, "disabled", return_value=True)
    def test_disabled(self, disabled, validate) -> None:
        with patch.object(sys, "argv", ["pyre", "check"]):
            self.assertEqual(pyre.main(), 0)
            validate.assert_not_called()

    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "validate")
    @patch.object(commands.Persistent, "_run_null_server")
    def test_persistent_integration(self, run_null_server, validate, read) -> None:
        validate.side_effect = commands.ClientException
        with patch.object(sys, "argv", ["pyre", "persistent"]):
            self.assertEqual(pyre.main(), 1)
            run_null_server.assert_called_once()

    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "validate")
    @patch.object(buck, "generate_source_directories", return_value=["."])
    def test_buck_build_prompting(
        self, generate_source_directories, validate, read
    ) -> None:
        with patch.object(commands.Check, "run"):
            with patch.object(sys, "argv", ["pyre", "check"]):
                self.assertEqual(pyre.main(), 0)
                generate_source_directories.assert_called_with(
                    set(), build=False, prompt=False
                )
        with patch.object(commands.Incremental, "run"):
            with patch.object(sys, "argv", ["pyre"]):
                self.assertEqual(pyre.main(), 0)
                generate_source_directories.assert_called_with(
                    set(), build=False, prompt=False
                )
        with patch.object(commands.Persistent, "run"):
            with patch.object(sys, "argv", ["pyre", "persistent"]):
                self.assertEqual(pyre.main(), 0)
                generate_source_directories.assert_called_with(
                    set(), build=False, prompt=True
                )

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import shutil
import sys
import unittest
from unittest.mock import call, patch

from .. import EnvironmentException, buck, commands, configuration, pyre


class PyreTest(unittest.TestCase):
    @patch.object(configuration.Configuration, "validate")
    @patch.object(configuration.Configuration, "disabled", return_value=True)
    def test_disabled(self, disabled, validate) -> None:
        with patch.object(sys, "argv", ["pyre", "check"]):
            self.assertEqual(pyre.main(), 0)
            validate.assert_not_called()

    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "validate")
    @patch.object(commands.Persistent, "run_null_server")
    def test_persistent_integration(self, run_null_server, validate, read) -> None:
        validate.side_effect = commands.ClientException
        with patch.object(sys, "argv", ["pyre", "persistent"]):
            self.assertEqual(pyre.main(), 2)
            run_null_server.assert_not_called()

        validate.side_effect = EnvironmentException
        with patch.object(sys, "argv", ["pyre", "persistent"]):
            self.assertEqual(pyre.main(), 2)
            run_null_server.assert_has_calls([call(timeout=3600)])

    @patch.object(json, "dump")
    @patch.object(json, "load")
    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "validate")
    @patch.object(buck, "generate_source_directories", return_value=["."])
    def test_buck_build_prompting(
        self, generate_source_directories, validate, read, _json_load, _json_dump
    ) -> None:
        with patch.object(commands.Check, "run", return_value=0):
            with patch.object(sys, "argv", ["pyre", "check"]):
                self.assertEqual(pyre.main(), 0)
                generate_source_directories.assert_called_with(
                    set(), build=False, prompt=False, use_cache=False
                )
        with patch.object(commands.Incremental, "run", return_value=0):
            with patch.object(sys, "argv", ["pyre"]):
                # One for shutil.which("watchman"), another for shutil.which(BINARY_NAME).
                with patch.object(shutil, "which", side_effect=[True, True]):
                    self.assertEqual(pyre.main(), 0)
                    generate_source_directories.assert_called_with(
                        set(), build=False, prompt=False, use_cache=True
                    )
        with patch.object(commands.Persistent, "run", return_value=0):
            with patch.object(sys, "argv", ["pyre", "persistent"]):
                self.assertEqual(pyre.main(), 0)
                generate_source_directories.assert_called_with(
                    set(), build=False, prompt=True, use_cache=True
                )

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import os
import shutil
import unittest
from unittest.mock import MagicMock, call, patch

from .. import analysis_directory, buck, commands, configuration, pyre
from ..exceptions import EnvironmentException


class PyreTest(unittest.TestCase):
    @patch.object(configuration.Configuration, "_validate")
    @patch.object(configuration.Configuration, "disabled", return_value=True)
    def test_disabled(self, disabled, validate) -> None:
        self.assertEqual(pyre.main(["check"]), 0)

    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "_validate")
    @patch.object(commands.Persistent, "run_null_server")
    def test_persistent_integration(self, run_null_server, validate, read) -> None:
        validate.side_effect = commands.ClientException
        self.assertEqual(pyre.main(["persistent"]), 2)
        run_null_server.assert_not_called()

        validate.side_effect = EnvironmentException
        self.assertEqual(pyre.main(["persistent"]), 0)
        run_null_server.assert_has_calls([call(timeout=3600 * 12)])

    @patch.object(os, "getenv")
    @patch.object(os, "isatty")
    @patch.object(json, "dump")
    @patch.object(json, "load")
    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "_validate")
    @patch.object(buck, "generate_source_directories", return_value=["."])
    def test_buck_build_prompting(
        self,
        generate_source_directories,
        validate,
        read,
        _json_load,
        _json_dump,
        _os_isatty,
        _os_getenv,
    ) -> None:
        mock_success = MagicMock()
        mock_success.exit_code = lambda: 0

        # Pretend that all CI jobs are running attached to a terminal, in a tty.
        _os_isatty.return_value = True
        _os_getenv.return_value = "term"

        with patch.object(
            commands.Check, "run", return_value=mock_success
        ), patch.object(
            analysis_directory.SharedAnalysisDirectory, "cleanup"
        ) as cleanup:
            self.assertEqual(pyre.main(["check"]), 0)
            generate_source_directories.assert_not_called()
            cleanup.assert_has_calls([call()])

            # The generation of source directories is handled within _run, which is
            # mocked here (via the mock of run()), so verify that we don't
            # call generate_source_directories outside of run. Tests in the
            # subcommands verify that prepare() is called, which calls
            # generate_source_directories.
            self.assertEqual(pyre.main(["--target", "//a/b", "check"]), 0)
            generate_source_directories.assert_not_called()

        with patch.object(commands.Incremental, "run", return_value=mock_success):
            # One for shutil.which("watchman"),
            # another for shutil.which(BINARY_NAME).
            with patch.object(shutil, "which", side_effect=[True, True]):
                self.assertEqual(pyre.main([]), 0)
                generate_source_directories.assert_not_called()
        with patch.object(commands.Persistent, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["persistent"]), 0)
            generate_source_directories.assert_not_called()
        with patch.object(commands.Start, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["start"]), 0)
            generate_source_directories.assert_not_called()
        with patch.object(commands.Start, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["--noninteractive", "start"]), 0)
            generate_source_directories.assert_not_called()

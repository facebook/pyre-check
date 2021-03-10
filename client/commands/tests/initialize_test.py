# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import subprocess
import sys
import unittest
from pathlib import Path
from unittest.mock import call, mock_open, patch

from ...commands import initialize
from ...commands.initialize import log


class InitializeTest(unittest.TestCase):
    @patch.object(os, "getcwd", return_value="/original/directory")
    @patch.object(log, "get_yes_no_input", return_value=True)
    @patch.object(log, "get_optional_input", return_value="")
    # pyre-fixme[56]: Argument `tools.pyre.client.commands.initialize.log` to
    #  decorator factory `unittest.mock.patch.object` could not be resolved in a global
    #  scope.
    @patch.object(log, "get_input", return_value="")
    @patch("shutil.which")
    @patch("os.path.isfile")
    @patch("subprocess.run")
    @patch("builtins.open")
    def test_initialize(
        self,
        open,
        subprocess_run,
        isfile,
        which,
        _get_input,
        _get_optional_input,
        get_yes_no_input,
        getcwd,
    ) -> None:
        get_yes_no_input.return_value = True

        def exists(path):
            if str(path).endswith(".watchmanconfig"):
                return False
            elif str(path).endswith(".pyre_configuration"):
                return False
            elif str(path).endswith(".pyre_configuration.local"):
                return False
            else:
                return True

        isfile.side_effect = exists
        # One for shutil.which("watchman"), another for shutil.which(BINARY_NAME).
        which.side_effect = ["watchman", "binary"]
        with patch.object(initialize, "find_typeshed", return_value=Path("/tmp")):
            initialize.Initialize().run()
            subprocess_run.assert_has_calls(
                [
                    call(
                        ["watchman", "watch-project", "."],
                        check=True,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        universal_newlines=True,
                    )
                ]
            )
            open.assert_any_call(os.path.abspath(".watchmanconfig"), "w+")

        def exists(path):
            return False

        isfile.side_effect = exists
        file = mock_open()
        with patch("builtins.open", file), patch.object(
            initialize.Initialize, "_get_local_configuration", return_value={}
        ), patch.object(initialize, "find_global_root", return_value=Path("/")):
            initialize.Initialize().run()
            file().write.assert_has_calls([call("{}"), call("\n")])

        with patch.object(sys, "argv", ["/tmp/pyre/bin/pyre"]), patch.object(
            initialize, "find_typeshed", return_value=Path("/tmp")
        ):
            which.reset_mock()
            which.side_effect = [True, None, "/tmp/pyre/bin/pyre.bin"]
            initialize.Initialize()._get_configuration()
            which.assert_has_calls(
                [call("watchman"), call("pyre.bin"), call("/tmp/pyre/bin/pyre.bin")]
            )

    def test_get_local_configuration(self) -> None:
        command = initialize.Initialize()

        with patch.object(log, "get_yes_no_input") as yes_no_input, patch.object(
            log, "get_input", return_value="//target/..."
        ):
            yes_no_input.side_effect = [True]
            self.assertEqual(
                command._get_local_configuration(Path("/"), Path("/")),
                {"targets": ["//target/..."]},
            )

        with patch.object(log, "get_yes_no_input") as yes_no_input, patch.object(
            log, "get_input", return_value="project/a, project/b"
        ):
            yes_no_input.side_effect = [False]
            self.assertEqual(
                command._get_local_configuration(Path("/"), Path("/")),
                {"source_directories": ["project/a", "project/b"]},
            )

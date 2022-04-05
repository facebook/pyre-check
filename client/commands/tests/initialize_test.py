# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import call, mock_open, patch

from ...tests.setup import (
    ensure_directories_exists,
    ensure_files_exist,
    switch_working_directory,
)
from .. import initialize
from ..initialize import _create_source_directory_element, log


class InitializeTest(unittest.TestCase):
    @patch.object(os, "getcwd", return_value="/original/directory")
    @patch.object(log, "get_yes_no_input", return_value=True)
    @patch.object(log, "get_optional_input", return_value="")
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
        with patch.object(
            initialize, "find_typeshed", return_value=Path("/tmp")
        ), patch.object(initialize, "find_global_root", return_value=None):
            initialize.run()
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
            initialize, "_get_local_configuration", return_value={}
        ), patch.object(initialize, "find_global_root", return_value=Path("/")):
            initialize.run()
            file().write.assert_has_calls([call("{}"), call("\n")])

        with patch.object(sys, "argv", ["/tmp/pyre/bin/pyre"]), patch.object(
            initialize, "find_typeshed", return_value=Path("/tmp")
        ):
            which.reset_mock()
            which.side_effect = [True, None, "/tmp/pyre/bin/pyre.bin"]
            initialize._get_configuration()
            which.assert_has_calls(
                [call("watchman"), call("pyre.bin"), call("/tmp/pyre/bin/pyre.bin")]
            )

    def test_create_source_directory_element(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            with switch_working_directory(root_path):
                ensure_directories_exists(root_path, "a")
                ensure_files_exist(root_path, ["a/__init__.py"])
                self.assertEqual(
                    _create_source_directory_element("a"),
                    {"import_root": ".", "source": "a"},
                )

        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            with switch_working_directory(root_path):
                ensure_directories_exists(root_path, "a")
                self.assertEqual(
                    _create_source_directory_element("a"),
                    "a",
                )

    def test_get_local_configuration(self) -> None:
        with patch.object(log, "get_yes_no_input") as yes_no_input, patch.object(
            log, "get_input", return_value="//target/..."
        ):
            yes_no_input.side_effect = [True]
            self.assertEqual(
                initialize._get_local_configuration(Path("/"), Path("/")),
                {"targets": ["//target/..."]},
            )

        with patch.object(log, "get_yes_no_input") as yes_no_input, patch.object(
            log, "get_input", return_value=""
        ):
            yes_no_input.side_effect = [True]
            self.assertEqual(
                initialize._get_local_configuration(Path("/project"), Path("/")),
                {"targets": ["//project/..."]},
            )

        with patch.object(log, "get_yes_no_input") as yes_no_input, patch.object(
            log, "get_input", return_value="project/a, project/b"
        ):
            yes_no_input.side_effect = [False]
            self.assertEqual(
                initialize._get_local_configuration(Path("/"), Path("/")),
                {"source_directories": ["project/a", "project/b"]},
            )

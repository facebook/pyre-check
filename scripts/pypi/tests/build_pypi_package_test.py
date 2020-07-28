#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch

from ..build_pypi_package import (
    MODULE_NAME,
    add_init_files,
    patch_version,
    sync_pysa_stubs,
    sync_python_files,
    valid_version,
)


class TestArgumentValidationMethods(unittest.TestCase):
    def test_validate_version(self) -> None:
        self.assertEqual(valid_version("0.0.01"), "0.0.01")
        with self.assertRaises(ValueError):
            valid_version("x0.0.01")


class TestCreatingWheel(unittest.TestCase):
    def test_create_init_files(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            path = Path(build_root)
            add_init_files(path)
            # Assert the expected __init__ files are present
            init_files = [str(path) for path in path.glob("**/*.py")]
            self.assertTrue(build_root + "/pyre_check/__init__.py" in init_files)
            self.assertTrue(build_root + "/pyre_check/client/__init__.py" in init_files)
            self.assertTrue(build_root + "/pyre_check/tools/__init__.py" in init_files)
            self.assertTrue(
                build_root + "/pyre_check/tools/upgrade/__init__.py" in init_files
            )

    def test_sync_files(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            build_path = Path(build_root)
            add_init_files(build_path)
            sync_python_files(build_path)
            command_directory = build_path / "pyre_check/client/commands"
            self.assertTrue(command_directory.is_dir())

    @patch("subprocess.run")
    @patch("shutil.copy")
    def test_rsync(self, copy: Mock, subprocess_run: Mock) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            build_path = Path(build_root)
            add_init_files(build_path)
            sync_pysa_stubs(build_path)
            args, _ = subprocess_run.call_args
            expected_args = [
                "rsync",
                "-avm",
                "--filter=+ */",
                "--filter=-! *.pysa",
                build_root,
            ]
            self.assertTrue(all(x in args[0] for x in expected_args))
            subprocess_run.assert_called()
            copy.assert_called()

    def test_patch_version(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            build_path = Path(build_root)
            add_init_files(build_path)
            patch_version("0.0.21", build_path)
            path = build_path / MODULE_NAME / "client/version.py"
            self.assertTrue(path.is_file())

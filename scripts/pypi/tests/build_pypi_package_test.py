#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch

from ..build_pypi_package import (
    _add_init_files,
    _patch_version,
    _sync_pysa_stubs,
    _sync_python_files,
    _validate_version,
    MODULE_NAME,
)


class TestArgumentValidationMethods(unittest.TestCase):
    def test_validate_version(self) -> None:
        _validate_version("0.0.01")
        with self.assertRaises(ValueError):
            _validate_version("x0.0.01")


class TestCreatingWheel(unittest.TestCase):
    def setUp(self) -> None:
        self.pyre_directory: Path = Path(__file__).resolve().parent.parent.parent.parent

    def test_create_init_files(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            path = Path(build_root)
            _add_init_files(path, "version")
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
            _add_init_files(build_path, "version")
            _sync_python_files(self.pyre_directory, build_path)
            command_directory = build_path / "pyre_check/client/commands"
            self.assertTrue(command_directory.is_dir())

    @patch("subprocess.run")
    def test_rsync(self, subprocess_run: Mock) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            build_path = Path(build_root)
            _add_init_files(build_path, "version")
            _sync_pysa_stubs(self.pyre_directory, build_path)
            args, _ = subprocess_run.call_args
            expected_args = [
                "rsync",
                "-avm",
                "--filter=+ */",
                build_root,
            ]
            self.assertTrue(all(x in args[0] for x in expected_args))
            subprocess_run.assert_called()

    def test_patch_version(self) -> None:
        with tempfile.TemporaryDirectory() as build_root:
            build_path = Path(build_root)
            _add_init_files(build_path, "version")
            _patch_version("0.0.21", build_path)
            path = build_path / MODULE_NAME / "client/version.py"
            self.assertTrue(path.is_file())

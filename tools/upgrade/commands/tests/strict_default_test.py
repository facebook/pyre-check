# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import tempfile
import unittest
from pathlib import Path
from typing import Iterable, Optional
from unittest.mock import MagicMock, mock_open, patch

from ...repository import Repository
from .. import strict_default
from ..strict_default import (
    _get_configuration_path,
    Configuration,
    ErrorSuppressingCommand,
    StrictDefault,
)


repository = Repository()


class StrictDefaultTest(unittest.TestCase):
    @patch.object(strict_default, "_get_configuration_path", return_value=Path("."))
    @patch.object(Configuration, "get_directory")
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "add_strict")
    @patch.object(ErrorSuppressingCommand, "_get_and_suppress_errors")
    def test_run_strict_default(
        self,
        get_and_suppress_errors,
        add_strict,
        configuration_write,
        get_directory,
        get_configuration_path,
    ) -> None:
        arguments = MagicMock()
        arguments.local_configuration = Path("local")
        arguments.fixme_threshold = 1
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            get_and_suppress_errors.assert_called_once()

        get_and_suppress_errors.reset_mock()
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            get_and_suppress_errors.assert_called_once()

        # Exceeding error threshold
        get_and_suppress_errors.reset_mock()
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            get_and_suppress_errors.assert_called_once()


def _ensure_files_exist(root: Path, relatives: Iterable[str]) -> None:
    for relative in relatives:
        full_path = root / relative
        full_path.parent.mkdir(parents=True, exist_ok=True)
        full_path.touch(exist_ok=True)


class GetConfigurationPathTest(unittest.TestCase):
    def assert_configuration_path(
        self, files: Iterable[str], local_root: Optional[str], expected: Optional[str]
    ) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            _ensure_files_exist(root_path, files)
            self.assertEqual(
                _get_configuration_path(
                    root_path / local_root if local_root is not None else root_path
                ),
                root_path / expected if expected is not None else None,
            )

    def test_get_configuration_path(self) -> None:
        self.assert_configuration_path(files=[], local_root=None, expected=None)
        self.assert_configuration_path(
            files=[".pyre_configuration"],
            local_root=None,
            expected=".pyre_configuration",
        )
        self.assert_configuration_path(
            files=["a/.pyre_configuration"],
            local_root="a",
            expected="a/.pyre_configuration",
        )
        self.assert_configuration_path(
            files=["a/.pyre_configuration", "b/c"], local_root="b", expected=None
        )
        self.assert_configuration_path(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local"],
            local_root="a/b",
            expected="a/b/.pyre_configuration.local",
        )
        self.assert_configuration_path(
            files=["a/.pyre_configuration", "a/b/.pyre_configuration.local"],
            local_root="a/b/.pyre_configuration.local",
            expected="a/b/.pyre_configuration.local",
        )

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

from ... import errors
from ...repository import Repository
from .. import strict_default
from ..strict_default import (
    Configuration,
    ErrorSuppressingCommand,
    StrictDefault,
    _get_configuration_path,
)


repository = Repository()


class StrictDefaultTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_add_local_mode(self, read_text) -> None:
        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with("# pyre-unsafe\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# comment\n1"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with(
                "# comment\n# comment\n\n# pyre-unsafe\n1"
            )

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-strict\n1"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-ignore-all-errors\n1"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.STRICT)
            path_write_text.assert_called_once_with("# pyre-strict\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            strict_default.add_local_mode("local.py", strict_default.LocalMode.IGNORE)
            path_write_text.assert_called_once_with("# pyre-ignore-all-errors\n1\n2")

    @patch.object(Configuration, "find_project_configuration", return_value=Path("."))
    @patch.object(Configuration, "get_directory")
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "add_strict")
    @patch.object(Configuration, "get_errors")
    @patch(f"{strict_default.__name__}.add_local_mode")
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    def test_run_strict_default(
        self,
        apply_suppressions,
        add_local_mode,
        get_errors,
        add_strict,
        configuration_write,
        get_directory,
        find_configuration,
    ) -> None:
        arguments = MagicMock()
        arguments.local_configuration = Path("local")
        arguments.fixme_threshold = 1
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            add_local_mode.assert_not_called()
            apply_suppressions.assert_not_called()

        add_local_mode.reset_mock()
        get_errors.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            add_local_mode.assert_not_called()
            apply_suppressions.assert_called_once_with(errors.Errors(pyre_errors))

        # Exceeding error threshold
        get_errors.return_value = []
        add_local_mode.reset_mock()
        apply_suppressions.reset_mock()
        get_errors.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "ignore_error": False,
                "external_to_global_root": False,
            },
            {
                "line": 3,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "ignore_error": False,
                "external_to_global_root": False,
            },
        ]
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            add_local_mode.assert_called_once()
            apply_suppressions.assert_not_called()


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

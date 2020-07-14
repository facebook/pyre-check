# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
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
    @patch.object(ErrorSuppressingCommand, "_suppress_errors")
    def test_run_strict_default(
        self,
        suppress_errors,
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
            suppress_errors.assert_not_called()

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
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            add_local_mode.assert_not_called()
            suppress_errors.assert_called_once_with(errors.Errors(pyre_errors))

        # Exceeding error threshold
        get_errors.return_value = []
        add_local_mode.reset_mock()
        suppress_errors.reset_mock()
        get_errors.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
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
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            },
        ]
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault.from_arguments(arguments, repository).run()
            add_local_mode.assert_called_once()
            suppress_errors.assert_not_called()

    def test_get_configuration_path(self):
        project_path = Path("project/path")
        configuration = _get_configuration_path(
            local_configuration=Path("local/config/example"),
            project_configuration=Path("project/example"),
        )
        self.assertEqual(
            configuration, Path("local/config/example/.pyre_configuration.local")
        )

        with patch("os.getcwd", returns="fake/path"), patch(
            f"{strict_default.__name__}.find_local_root", return_value=Path("cwd/path")
        ):
            configuration = _get_configuration_path(
                local_configuration=None, project_configuration=project_path
            )
            self.assertEqual(configuration, Path("cwd/path/.pyre_configuration.local"))

        with patch("os.getcwd", returns="fake/path"), patch(
            f"{strict_default.__name__}.find_local_root", return_value=None
        ):
            configuration = _get_configuration_path(
                local_configuration=None, project_configuration=project_path
            )
            self.assertEqual(configuration, project_path)

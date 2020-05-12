# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

from ... import errors, upgrade
from ...repository import Repository
from .. import strict_default
from ..strict_default import StrictDefault


repository = Repository()


class StrictDefaultTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_add_local_mode(self, read_text) -> None:
        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with("# pyre-unsafe\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# comment\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with(
                "# comment\n# comment\n\n# pyre-unsafe\n1"
            )

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-strict\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-ignore-all-errors\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.STRICT)
            path_write_text.assert_called_once_with("# pyre-strict\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.IGNORE)
            path_write_text.assert_called_once_with("# pyre-ignore-all-errors\n1\n2")

    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "get_directory")
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "add_strict")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch(f"{strict_default.__name__}.add_local_mode")
    def test_run_strict_default(
        self,
        add_local_mode,
        get_errors,
        add_strict,
        configuration_write,
        get_directory,
        find_configuration,
    ) -> None:
        arguments = MagicMock()
        arguments.local_configuration = Path("local")
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            StrictDefault(arguments, repository).run()
            add_local_mode.assert_not_called()

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
            StrictDefault(arguments, repository).run()
            add_local_mode.assert_called_once()

        arguments.reset_mock()
        get_errors.return_value = []
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
            StrictDefault(arguments, repository).run()
            add_local_mode.assert_called_once()

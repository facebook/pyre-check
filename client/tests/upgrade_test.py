# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools
import pathlib
import unittest
from unittest.mock import MagicMock, call, patch

from .. import upgrade


def _result(errors):
    def error_path(error):
        return error["path"]

    return itertools.groupby(sorted(errors, key=error_path), error_path)


class FixmeTest(unittest.TestCase):
    @patch.object(pathlib.Path, "read_text")
    def test_fixme(self, path_read_text) -> None:
        arguments = MagicMock()
        arguments.comment = None

        upgrade.run_fixme(arguments, {})

        # Test single error.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [1]: description",
                    }
                ]
            )
            path_read_text.return_value = "  1\n2"
            upgrade.run_fixme(arguments, result)
            path_write_text.assert_called_once_with(
                "  # pyre-fixme[1]: description\n  1\n2"
            )

        # Test error with comment.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [1]: description",
                    }
                ]
            )
            path_read_text.return_value = "  1\n2"
            arguments.comment = "T1234"
            upgrade.run_fixme(arguments, result)
            arguments.comment = None
            path_write_text.assert_called_once_with("  # pyre-fixme[1]: T1234\n  1\n2")

        # Test multiple errors and multiple lines.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [1]: description",
                    },
                    {
                        "path": "path.py",
                        "line": 2,
                        "description": "Error [1]: description",
                    },
                    {
                        "path": "path.py",
                        "line": 2,
                        "description": "Error [2]: description",
                    },
                ]
            )
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, result)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n# pyre-fixme[1, 2]: description\n2"
            )

        # Test errors in multiple files.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [1]: description",
                    },
                    {
                        "path": "other.py",
                        "line": 2,
                        "description": "Error [2]: description",
                    },
                ]
            )
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, result)
            path_write_text.has_calls(
                [
                    call("# pyre-fixme[1]: description\n1\n2"),
                    call("1\n#pyre-fixme[2]: description\n2"),
                ]
            )

        # Test removal of extraneous ignore.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [0]: extraneous ignore",
                    }
                ]
            )
            path_read_text.return_value = "  # pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments, result)
            arguments.comment = None
            path_write_text.assert_called_once_with("2")

        # Test removal of extraneous ignore (trailing comment).
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            result = _result(
                [
                    {
                        "path": "path.py",
                        "line": 1,
                        "description": "Error [0]: extraneous ignore",
                    }
                ]
            )
            path_read_text.return_value = "1# pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments, result)
            arguments.comment = None
            path_write_text.assert_called_once_with("1\n2")

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from ... import errors
from ...repository import Repository
from .. import command, fixme
from ..fixme import Fixme


repository = Repository()


class FixmeTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    @patch(f"{fixme.__name__}._errors_from_run")
    @patch(f"{fixme.__name__}.Errors.from_stdin")
    @patch(f"{command.__name__}.Repository.submit_changes")
    def test_run_fixme(
        self, submit_changes, stdin_errors, run_errors, path_read_text
    ) -> None:
        arguments = MagicMock()
        arguments.comment = None
        arguments.max_line_length = 88
        arguments.error_source = "stdin"
        arguments.truncate = True

        stdin_errors.return_value = errors.Errors.empty()
        run_errors.return_value = errors.Errors.empty()
        Fixme(arguments, repository).run()

        # Test single error.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Generated files.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# @" "generated\n1\n2\n"
            Fixme(arguments, repository).run()
            path_write_text.assert_not_called()

        arguments.error_source = "stdin"
        arguments.lint = False

        # Test error with custom message.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.return_value = errors.Errors(pyre_errors)
            path_read_text.return_value = "1\n2"
            arguments.comment = "T1234"
            Fixme(arguments, repository).run()
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[1]: T1234\n1\n2")

        # Test error with existing comment.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                }
            ]
            arguments.comment = None
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# existing comment\n2"
            Fixme(arguments, repository).run()
            arguments.comment = None
            path_write_text.assert_called_once_with(
                "# existing comment\n# pyre-fixme[1]: description\n2"
            )

        # Test multiple errors and multiple lines.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: description",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n"
                "1\n"
                "# pyre-fixme[1]: description\n"
                "# pyre-fixme[2]: description\n"
                "2"
            )
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [10]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [11]: Description two.",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[10]: Description one.\n"
                "# pyre-fixme[11]: Description two.\n"
                "2"
            )
        arguments.max_line_length = 40
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Very long description two.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [3]: Very long description three.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [4]: Description four.",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[1]: Description one.\n"
                "# pyre-fixme[2]: Very long descriptio...\n"
                "# pyre-fixme[3]: Very long descriptio...\n"
                "# pyre-fixme[4]: Description four.\n"
                "2"
            )
        arguments.max_line_length = 36
        arguments.truncate = False
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [2]: Maximum characters.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Too many characters.",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "# pyre-fixme[2]: Maximum characters.\n"
                "1\n"
                "# pyre-fixme[2]: Too many\n"
                "#  characters.\n"
                "2"
            )

        arguments.max_line_length = 40
        arguments.truncate = False
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Very long description two.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [3]: Very long description three.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [4]: Description four.",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[1]: Description one.\n"
                "# pyre-fixme[2]: Very long\n"
                "#  description two.\n"
                "# pyre-fixme[3]: Very long\n"
                "#  description three.\n"
                "# pyre-fixme[4]: Description four.\n"
                "2"
            )
        arguments.truncate = True

        # Test errors in multiple files.
        arguments.max_line_length = 88
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "other.py",
                    "line": 2,
                    "concise_description": "Error [2]: description",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.has_calls(
                [
                    call("# pyre-fixme[1]: description\n1\n2"),
                    call("1\n#pyre-fixme[2]: description\n2"),
                ]
            )

        # Test removal of extraneous ignore.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "  # pyre-ignore[0]: [1, 2, 3]\n2"
            Fixme(arguments, repository).run()
            arguments.comment = None
            path_write_text.assert_called_once_with("2")

        # Test removal of extraneous ignore.
        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n  #  continuation comment\n2"
            )
            Fixme(arguments, repository).run()
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("2")

        # We don't remove legitimate comments.
        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n  # assumed continuation\n2"
            )
            Fixme(arguments, repository).run()
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("2")

        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore that's "
                    "quite long",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]:\n  #  comment that doesn't fit on one line\n"
                "# pyre-ignore[1]:\n2"
            )
            Fixme(arguments, repository).run()
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("# pyre-ignore[1]:\n2")

        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# pyre-fixme[1]\n# pyre-fixme[2]\n2"
            Fixme(arguments, repository).run()
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[2]\n2")

        # Test removal of extraneous ignore (trailing comment).
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1# pyre-ignore[0]: [1, 2, 3]\n2"
            Fixme(arguments, repository).run()
            arguments.comment = None
            path_write_text.assert_called_once_with("1\n2")

        # Test long lines.
        with patch.object(Path, "write_text") as path_write_text:
            arguments_short = MagicMock()
            arguments_short.comment = None
            arguments_short.max_line_length = 35
            arguments_short.run = False

            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description one, "
                    "that has a pretty verbose text",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: description-that-will-not-break-"
                    "even-when-facing-adversities",
                },
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "line = 1\nline = 2\nline = 3"
            Fixme(arguments_short, repository).run()
            path_write_text.assert_called_once_with(
                """# FIXME[1]: description one...
                line = 1
                # FIXME[2]: description-tha...
                line = 2
                line = 3""".replace(
                    "                ", ""
                ).replace(
                    "FIXME", "pyre-fixme"
                )
            )

        # Fall back to normal description for backwards compatibility.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Ensure that we prefer concise descriptions.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "Error[1]: Concise.",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            Fixme(arguments, repository).run()
            path_write_text.assert_called_once_with("# pyre-fixme[1]: Concise.\n1\n2")

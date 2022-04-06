# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import tempfile
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, patch

from ...errors import Errors, PartialErrorSuppression
from ...filesystem import LocalMode
from .. import command
from ..command import ErrorSource, ErrorSuppressingCommand
from ..fixme_single import Configuration, FixmeSingle


class CommandTest(unittest.TestCase):
    def test_apply_suppressions(self) -> None:
        arguments = MagicMock()
        arguments.force_format_unsuppressed = False

        force_format = MagicMock()
        repository = MagicMock()
        repository.force_format = force_format

        successful_suppression = MagicMock()
        errors = MagicMock()
        errors.suppress = successful_suppression

        ErrorSuppressingCommand(arguments, repository)._apply_suppressions(errors)
        successful_suppression.assert_called_once()

        errors.reset_mock()

        repeaded_unsuccessful_suppression = MagicMock(
            side_effect=PartialErrorSuppression("Message", [])
        )
        errors.suppress = repeaded_unsuccessful_suppression
        with self.assertRaises(PartialErrorSuppression):
            ErrorSuppressingCommand(arguments, repository)._apply_suppressions(errors)
        repeaded_unsuccessful_suppression.assert_called_once()

        errors.reset_mock()
        repeaded_unsuccessful_suppression.reset_mock()

        arguments.force_format_unsuppressed = True
        with self.assertRaises(PartialErrorSuppression):
            ErrorSuppressingCommand(arguments, repository)._apply_suppressions(errors)
        force_format.assert_called_once()
        repeaded_unsuccessful_suppression.assert_has_calls(
            [
                call(
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                    arguments.unsafe,
                ),
                call(
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                    arguments.unsafe,
                ),
            ]
        )

        errors.reset_mock()
        force_format.reset_mock()

        class MixedSuppression:
            def __init__(self):
                self._fail = True

            def __call__(self, *args):
                if self._fail:
                    self._fail = False
                    raise PartialErrorSuppression("Message", [])

        mixed_suppression = MagicMock(side_effect=MixedSuppression())
        errors.suppress = mixed_suppression

        ErrorSuppressingCommand(arguments, repository)._apply_suppressions(errors)
        force_format.assert_called_once()
        mixed_suppression.assert_has_calls(
            [
                call(
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                    arguments.unsafe,
                ),
                call(
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                    arguments.unsafe,
                ),
            ]
        )

    @patch(f"{command.__name__}.add_local_mode")
    def test_get_and_suppress_errors(self, add_local_mode) -> None:
        arguments = MagicMock()
        repository = MagicMock()
        configuration = Configuration(Path("test"), {})

        errors_list = [
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
                "path": "other.py",
                "line": 3,
                "concise_description": "Error [2]: description",
            },
        ]
        errors = Errors(errors_list)
        get_errors = MagicMock()
        get_errors.return_value = errors
        configuration.get_errors = get_errors
        configuration.is_local = True

        command = ErrorSuppressingCommand(arguments, repository)
        apply_suppressions = MagicMock()
        command._apply_suppressions = apply_suppressions

        # No fixme threshold set.
        command._get_and_suppress_errors(configuration)
        apply_suppressions.assert_has_calls(
            [
                call(errors),
                call(errors),
            ]
        )

        # Fixme threshold specified.
        apply_suppressions.reset_mock()
        command._get_and_suppress_errors(configuration, fixme_threshold=2)
        add_local_mode.assert_not_called()
        self.assertEqual(apply_suppressions.call_count, 3)

        apply_suppressions.reset_mock()
        command._get_and_suppress_errors(configuration, fixme_threshold=1)
        add_local_mode.assert_called_once_with("path.py", LocalMode.IGNORE)
        self.assertEqual(apply_suppressions.call_count, 2)

    def test_argument_parsing(self) -> None:
        parser = argparse.ArgumentParser()
        FixmeSingle.add_arguments(parser)
        with tempfile.TemporaryDirectory() as directory:
            self.assertEqual(
                parser.parse_args([directory, "--error-source", "stdin"]).error_source,
                ErrorSource.STDIN,
            )
            self.assertEqual(
                parser.parse_args([directory]).error_source, ErrorSource.GENERATE
            )
            with self.assertRaises(SystemExit):
                parser.parse_args([directory, "--error-source", "foo"]).error_source,

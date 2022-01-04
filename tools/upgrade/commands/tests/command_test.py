# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import tempfile
import unittest
from unittest.mock import MagicMock, call

from ...errors import PartialErrorSuppression
from ..command import ErrorSource, ErrorSuppressingCommand
from ..fixme_single import FixmeSingle


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

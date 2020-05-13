# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock, call

from ...errors import PartialErrorSuppression
from ..command import ErrorSuppressingCommand


class CommandTest(unittest.TestCase):
    def test_suppress_errors(self) -> None:
        arguments = MagicMock()
        arguments.force_format_unsuppressed = False

        force_format = MagicMock()
        repository = MagicMock()
        repository.force_format = force_format

        successful_suppression = MagicMock()
        errors = MagicMock()
        errors.suppress = successful_suppression

        ErrorSuppressingCommand(arguments, repository)._suppress_errors(errors)
        successful_suppression.assert_called_once()

        errors.reset_mock()

        repeaded_unsuccessful_suppression = MagicMock(
            side_effect=PartialErrorSuppression("Message", [])
        )
        errors.suppress = repeaded_unsuccessful_suppression
        with self.assertRaises(PartialErrorSuppression):
            ErrorSuppressingCommand(arguments, repository)._suppress_errors(errors)
        repeaded_unsuccessful_suppression.assert_called_once()

        errors.reset_mock()
        repeaded_unsuccessful_suppression.reset_mock()

        arguments.force_format_unsuppressed = True
        with self.assertRaises(PartialErrorSuppression):
            ErrorSuppressingCommand(arguments, repository)._suppress_errors(errors)
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

        ErrorSuppressingCommand(arguments, repository)._suppress_errors(errors)
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

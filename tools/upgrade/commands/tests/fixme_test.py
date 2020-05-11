# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock, patch

from ... import errors
from ...repository import Repository
from .. import fixme
from ..command import ErrorSuppressingCommand
from ..fixme import Fixme


repository = Repository()


class FixmeTest(unittest.TestCase):
    def test_run(self) -> None:
        arguments = MagicMock()

        arguments.error_source = "stdin"
        mock_errors = MagicMock()
        with patch.object(
            errors.Errors, "from_stdin", return_value=mock_errors
        ) as errors_from_stdin, patch.object(
            ErrorSuppressingCommand, "_suppress_errors"
        ) as suppress_errors:
            Fixme(arguments, repository).run()
            errors_from_stdin.assert_called_once()
            suppress_errors.assert_called_once_with(mock_errors)

        arguments.error_source = "generate"
        arguments.lint = False

        with patch(
            f"{fixme.__name__}._errors_from_run", return_value=mock_errors
        ) as errors_from_run, patch.object(
            ErrorSuppressingCommand, "_suppress_errors"
        ) as suppress_errors:
            Fixme(arguments, repository).run()
            errors_from_run.assert_called_once()
            suppress_errors.assert_called_once_with(mock_errors)

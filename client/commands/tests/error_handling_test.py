# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import os
import unittest
from unittest.mock import MagicMock, call, patch

from ... import commands  # noqa
from ...error import Error  # noqa
from .command_test import mock_arguments, mock_configuration


class ErrorHandlingTest(unittest.TestCase):
    @patch.object(Error, "__init__", return_value=None)
    @patch.object(Error, "__hash__", return_value=0)
    @patch.object(os.path, "realpath", side_effect=lambda path: path)
    def test_get_errors(self, realpath, error_hash, create_error) -> None:
        arguments = mock_arguments()
        arguments.original_directory = "/test"  # called from
        arguments.current_directory = "/"  # project root
        arguments.local_configuration = None
        configuration = mock_configuration()
        result = MagicMock()
        error = MagicMock()
        error_dictionary = {"path": "target"}
        error.__getitem__.side_effect = error_dictionary.__getitem__

        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/f/g"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            handler._get_errors(result, True)
            create_error.assert_has_calls([call(False, False), call(False, False)])
            create_error.reset_mock()

        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/h/i"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            handler._get_errors(result, True)
            create_error.assert_has_calls([call(False, False), call(False, False)])
            create_error.reset_mock()

        arguments.target = ["//f/g:target"]
        configuration.targets = []
        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/f/g"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            handler._get_errors(result, True)
            create_error.assert_has_calls([call(False, False), call(False, False)])
            create_error.reset_mock()

        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/h/i"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            handler._get_errors(result, True)
            create_error.assert_has_calls([call(False, False), call(False, True)])
            create_error.reset_mock()

        arguments.target = []
        configuration.do_not_check = ["/test/auto/gen"]
        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/auto/gen"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            create_error.assert_has_calls([call(True, False)])
            create_error.reset_mock()

        arguments.original_directory = "/f/g/target"
        arguments.target = ["//f/g:target"]
        configuration.targets = []
        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/test/h/i"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            handler._get_errors(result, True)
            create_error.assert_has_calls([call(False, False), call(False, True)])
            create_error.reset_mock()

        arguments.original_directory = "/"  # called from
        arguments.current_directory = "/"  # project root
        error_dictionary = {"path": "b/c"}
        error.__getitem__.side_effect = error_dictionary.__getitem__
        configuration.do_not_check = ["*/b"]
        handler = commands.ErrorHandling(
            arguments, configuration, source_directory="/a"
        )
        with patch.object(json, "loads", return_value=[error]):
            handler._get_errors(result)
            create_error.assert_has_calls([call(True, False)])
            create_error.reset_mock()

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, mock_open, patch

from ... import commands  # noqa
from ...filesystem import AnalysisDirectory
from ..color import CoverageLevel, TypeAnnotation
from .command_test import mock_arguments, mock_configuration


class ColorTest(unittest.TestCase):
    @patch("builtins.open")
    @patch("sys.exit")
    def test_query(self, sys, open_mock) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        open_mock.return_value = mock_open(
            read_data="""
            def foo() -> int:
                pass
            """
        ).return_value
        with patch.object(commands.Command, "_call_client") as call_client:
            result = MagicMock()
            result.output = '{"response": {"types": []}}'
            call_client.return_value = result
            arguments.file = ""
            commands.Color(arguments, configuration, AnalysisDirectory(".")).run()
            call_client.assert_called_once_with(command=commands.Query.NAME)

    def test_type_annotations(self) -> None:
        json_example = {
            "location": {
                "path": "a/b.py",
                "start": {"line": 6, "column": 8},
                "stop": {"line": 6, "column": 9},
            },
            "coverage": ["Partial"],
        }
        annotation = TypeAnnotation.create_from_json(json_example)
        self.assertEqual(annotation.start_line, 6)
        self.assertEqual(annotation.stop_line, 6)
        self.assertEqual(annotation.start_column, 8)
        self.assertEqual(annotation.stop_column, 9)
        self.assertEqual(annotation.coverage, CoverageLevel.PARTIAL)

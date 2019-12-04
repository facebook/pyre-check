# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ... import commands
from ...analysis_directory import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class RageTest(unittest.TestCase):
    def test_flags(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"
        analysis_directory = AnalysisDirectory(".")
        self.assertEqual(
            commands.Rage(
                arguments, original_directory, configuration, analysis_directory
            )._flags(),
            ["-log-directory", ".pyre"],
        )

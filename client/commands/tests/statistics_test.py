# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import unittest
from pathlib import Path

from ... import commands  # noqa
from ..statistics import _find_paths
from .command_test import mock_arguments


class StatisticsTest(unittest.TestCase):
    def test_find_targets(self) -> None:
        arguments = mock_arguments()
        arguments.filter_paths = []
        arguments.local_configuration = "example/path/client"
        self.assertEqual(
            _find_paths(arguments.local_configuration, arguments.filter_paths),
            [Path("example/path/client")],
        )

        arguments.local_configuration = "example/path/client/.pyre_configuration.local"
        self.assertEqual(
            _find_paths(arguments.local_configuration, arguments.filter_paths),
            [Path("example/path/client")],
        )

        arguments.filter_paths = ["a.py", "b.py"]
        self.assertEqual(
            _find_paths(arguments.local_configuration, arguments.filter_paths),
            [Path("example/path/client/a.py"), Path("example/path/client/b.py")],
        )

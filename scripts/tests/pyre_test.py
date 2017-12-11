# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
import sys

from unittest.mock import patch

from .. import (
    commands,
    configuration,
    pyre,
)


class PyreTest(unittest.TestCase):
    @patch.object(configuration.Configuration, '_read')
    @patch.object(configuration.Configuration, 'validate')
    @patch.object(commands.Persistent, '_run_null_server')
    def test_persistent_integration(
            self,
            run_null_server,
            validate,
            read):
        validate.side_effect = commands.ClientException
        with patch.object(sys, 'argv', ['pyre', 'persistent']):
            self.assertEqual(pyre.main(), 1)
            run_null_server.assert_called_once()

# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from .. import platform


class PlatformTest(unittest.TestCase):
    def test_get_platform(self):
        self.assertEqual(platform.get_platform(), "platform007")

    def test_get_python_version(self):
        self.assertEqual(platform.get_python_version(), (3, 0))

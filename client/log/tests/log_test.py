# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from .. import log


class LogTest(unittest.TestCase):
    def test_truncate(self) -> None:
        self.assertEqual(log.truncate("a", 10), "a")
        self.assertEqual(log.truncate("a" * 10, 10), "a" * 10)
        self.assertEqual(log.truncate("123456789", 4), "1234..[truncated 5 characters]")

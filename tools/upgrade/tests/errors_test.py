# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from ..errors import Errors


class ErrorsTest(unittest.TestCase):
    def test_from_json(self) -> None:
        self.assertEqual(
            Errors.from_json('[{ "path": "test.py", "key": "value" }]'),
            Errors([{"path": "test.py", "key": "value"}]),
        )
        self.assertEqual(Errors.from_json(None), Errors([]))
        self.assertEqual(
            Errors.from_json('[{ "path": "test.py", "key": "value" }'), Errors([])
        )

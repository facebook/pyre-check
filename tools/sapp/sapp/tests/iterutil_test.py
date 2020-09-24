# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from unittest import TestCase

from ..iterutil import split_every


class UtilsTest(TestCase):
    # pyre-fixme[3]: Return type must be annotated.
    def test_split_every(self):
        self.assertEqual(
            list(split_every(2, range(10))), [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]]
        )

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Tuple, TypeVar

import testslide

from ..interval_tree import IntervalTree

T = TypeVar("T")


# Intervals where each point is a pair, i.e., (1, 2) to (1, 10).
PairIntervalTree = IntervalTree[Tuple[int, int], str]


class IntervalTreeTest(testslide.TestCase):
    def test_getitem(self) -> None:
        interval_tree: PairIntervalTree = PairIntervalTree(
            [((1, 4), (1, 8), "(1,4) to (1,8)"), ((8, 10), (9, 20), "(8,10) to (9,20)")]
        )
        self.assertEqual(interval_tree[(1, 7)], "(1,4) to (1,8)")
        # Left-inclusive.
        self.assertEqual(
            interval_tree[(8, 10)],
            "(8,10) to (9,20)",
        )
        self.assertEqual(
            interval_tree[(8, 99)],
            "(8,10) to (9,20)",
        )
        # Not right-inclusive.
        self.assertEqual(interval_tree[(9, 20)], None)
        self.assertEqual(interval_tree[(99, 99)], None)

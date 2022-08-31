# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..language_server_protocol import PyrePosition
from ..location_lookup import LocationLookup


class IntervalTreeTest(testslide.TestCase):
    def test_getitem(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                (PyrePosition(1, 4), PyrePosition(1, 8), "(1,4) to (1,8)"),
                (PyrePosition(8, 10), PyrePosition(9, 20), "(8,10) to (9,20)"),
            ]
        )
        self.assertEqual(location_lookup[PyrePosition(1, 7)], "(1,4) to (1,8)")
        # Left-inclusive.
        self.assertEqual(
            location_lookup[PyrePosition(8, 10)],
            "(8,10) to (9,20)",
        )
        self.assertEqual(
            location_lookup[PyrePosition(8, 99)],
            "(8,10) to (9,20)",
        )
        # Not right-inclusive.
        self.assertEqual(location_lookup[PyrePosition(9, 20)], None)
        self.assertEqual(location_lookup[PyrePosition(99, 99)], None)

    def test_picks_smallest_containing_interval(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                # Overall expression.
                (PyrePosition(1, 4), PyrePosition(4, 13), "(1, 4), (4, 13)"),
                # Expression 1.
                (PyrePosition(1, 4), PyrePosition(1, 8), "(1, 4), (1, 8)"),
                (PyrePosition(1, 4), PyrePosition(1, 5), "(1, 4), (1, 5)"),
                (PyrePosition(1, 6), PyrePosition(1, 8), "(1, 6), (1, 8)"),
                # Expression 2.
                (PyrePosition(2, 4), PyrePosition(4, 13), "(2, 4), (4, 13)"),
            ]
        )
        self.assertEqual(location_lookup[PyrePosition(1, 4)], "(1, 4), (1, 5)")
        self.assertEqual(location_lookup[PyrePosition(1, 7)], "(1, 6), (1, 8)")
        self.assertEqual(location_lookup[PyrePosition(2, 11)], "(2, 4), (4, 13)")
        self.assertEqual(location_lookup[PyrePosition(3, 9)], "(2, 4), (4, 13)")
        self.assertEqual(location_lookup[PyrePosition(1, 5)], "(1, 4), (1, 8)")
        self.assertEqual(location_lookup[PyrePosition(1, 9)], "(1, 4), (4, 13)")
        self.assertEqual(location_lookup[PyrePosition(1, 99)], "(1, 4), (4, 13)")

    def test_picks_smallest_containing_interval__different_lines(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                (PyrePosition(1, 4), PyrePosition(4, 4), "(1, 4), (4, 4)"),
                (PyrePosition(3, 1), PyrePosition(3, 10000), "(3, 1), (3, 10000)"),
            ]
        )
        # This interval is "smaller" than the other interval because it spans only
        # one line.
        self.assertEqual(location_lookup[PyrePosition(3, 1)], "(3, 1), (3, 10000)")

    def test_ignores_null_intervals(self) -> None:
        interval_tree: LocationLookup[str] = LocationLookup[str](
            [
                (PyrePosition(1, 11), PyrePosition(1, 11), "(1, 11), (1, 11)"),
                # End is before the beginning.
                (PyrePosition(1, 11), PyrePosition(1, 0), "(1, 11), (1, 0)"),
            ]
        )
        self.assertEqual(interval_tree[PyrePosition(1, 11)], None)
        self.assertEqual(interval_tree[PyrePosition(1, 0)], None)

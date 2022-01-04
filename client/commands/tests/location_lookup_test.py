# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..language_server_protocol import Position
from ..location_lookup import LocationLookup


class IntervalTreeTest(testslide.TestCase):
    def test_getitem(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                (Position(1, 4), Position(1, 8), "(1,4) to (1,8)"),
                (Position(8, 10), Position(9, 20), "(8,10) to (9,20)"),
            ]
        )
        self.assertEqual(location_lookup[Position(1, 7)], "(1,4) to (1,8)")
        # Left-inclusive.
        self.assertEqual(
            location_lookup[Position(8, 10)],
            "(8,10) to (9,20)",
        )
        self.assertEqual(
            location_lookup[Position(8, 99)],
            "(8,10) to (9,20)",
        )
        # Not right-inclusive.
        self.assertEqual(location_lookup[Position(9, 20)], None)
        self.assertEqual(location_lookup[Position(99, 99)], None)

    def test_picks_smallest_containing_interval(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                # Overall expression.
                (Position(1, 4), Position(4, 13), "(1, 4), (4, 13)"),
                # Expression 1.
                (Position(1, 4), Position(1, 8), "(1, 4), (1, 8)"),
                (Position(1, 4), Position(1, 5), "(1, 4), (1, 5)"),
                (Position(1, 6), Position(1, 8), "(1, 6), (1, 8)"),
                # Expression 2.
                (Position(2, 4), Position(4, 13), "(2, 4), (4, 13)"),
            ]
        )
        self.assertEqual(location_lookup[Position(1, 4)], "(1, 4), (1, 5)")
        self.assertEqual(location_lookup[Position(1, 7)], "(1, 6), (1, 8)")
        self.assertEqual(location_lookup[Position(2, 11)], "(2, 4), (4, 13)")
        self.assertEqual(location_lookup[Position(3, 9)], "(2, 4), (4, 13)")
        self.assertEqual(location_lookup[Position(1, 5)], "(1, 4), (1, 8)")
        self.assertEqual(location_lookup[Position(1, 9)], "(1, 4), (4, 13)")
        self.assertEqual(location_lookup[Position(1, 99)], "(1, 4), (4, 13)")

    def test_picks_smallest_containing_interval__different_lines(self) -> None:
        location_lookup: LocationLookup[str] = LocationLookup[str](
            [
                (Position(1, 4), Position(4, 4), "(1, 4), (4, 4)"),
                (Position(3, 1), Position(3, 10000), "(3, 1), (3, 10000)"),
            ]
        )
        # This interval is "smaller" than the other interval because it spans only
        # one line.
        self.assertEqual(location_lookup[Position(3, 1)], "(3, 1), (3, 10000)")

    def test_ignores_null_intervals(self) -> None:
        interval_tree: LocationLookup[str] = LocationLookup[str](
            [
                (Position(1, 11), Position(1, 11), "(1, 11), (1, 11)"),
                # End is before the beginning.
                (Position(1, 11), Position(1, 0), "(1, 11), (1, 0)"),
            ]
        )
        self.assertEqual(interval_tree[Position(1, 11)], None)
        self.assertEqual(interval_tree[Position(1, 0)], None)

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from typing import (
    Generic,
    Iterable,
    Optional,
    Tuple,
    TypeVar,
)

import intervaltree

from .language_server_protocol import Position


LOG: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")
Value = TypeVar("Value")


LOG: logging.Logger = logging.getLogger(__name__)


class LocationLookup(Generic[Value]):
    """Interval tree to store a `Value` for each interval of `(Position, Position)`.

    The intervals are left-inclusive, right-exclusive.

    This class is implemented using `intervaltree.IntervalTree`."""

    def __init__(self, intervals: Iterable[Tuple[Position, Position, Value]]) -> None:
        non_null_intervals = [
            interval for interval in intervals if interval[1] > interval[0]
        ]
        self._interval_tree: object = intervaltree.IntervalTree.from_tuples(
            non_null_intervals
        )

    def __getitem__(self, position: Position) -> Optional[Value]:
        """Returns the value from the first matching interval, if any.

        Pyre query returns overlapping intervals for compound expressions, so
        pick the narrowest containing interval.

        We do this by subtracting the two endpoints (as tuples) and sorting in
        lexicographic order.

        For example, if we have Position = (line, character), then we look at the
        difference in lines before looking at the difference in characters. We use
        this instead of Euclidean distance because an interval that spans just 1
        lines is 'narrower' than an interval that spans 2 lines even if the
        former spans 1000 characters."""

        # pyre-ignore[16]: No type stubs for intervaltree.
        intervals = self._interval_tree.__getitem__(position)
        if len(intervals) == 0:
            return None

        def difference_between_endpoints(interval: object) -> Tuple[float, float]:
            return (
                # pyre-ignore[16]: No type stubs for intervaltree.
                interval.end.line - interval.begin.line,
                interval.end.character - interval.begin.character,
            )

        _, _, value = sorted(intervals, key=difference_between_endpoints)[0]
        return value

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, LocationLookup):
            return False

        # pyre-ignore[16]: No type stubs for intervaltree.
        return self._interval_tree.items() == other._interval_tree.items()

    def __str__(self) -> str:
        items = ", ".join(
            [
                f"({point1}, {point2}): {value}"
                # pyre-ignore[16]: No type stubs for intervaltree.
                for (point1, point2, value) in self._interval_tree.items()
            ]
        )
        return f"{{{items}}}"

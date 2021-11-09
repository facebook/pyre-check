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

LOG: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")
Point = TypeVar("Point")
Value = TypeVar("Value")


LOG: logging.Logger = logging.getLogger(__name__)


class IntervalTree(Generic[Point, Value]):
    """Interval tree to store a `Value` for each interval of `(Point, Point)`.

    The intervals are left-inclusive, right-exclusive.

    This class is a typed wrapper around `intervaltree.IntervalTree`."""

    def __init__(self, intervals: Iterable[Tuple[Point, Point, Value]]) -> None:
        self._interval_tree: object = intervaltree.IntervalTree.from_tuples(intervals)

    def __getitem__(self, point: Point) -> Optional[Value]:
        """Returns the value from the first matching interval, if any.

        Pyre query returns disjoint intervals, so picking the first interval
        will lead to correct results."""

        # pyre-ignore[16]: No type stubs for intervaltree.
        intervals = self._interval_tree.__getitem__(point)
        if len(intervals) == 0:
            return None

        _, _, value = list(intervals)[0]
        return value

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, IntervalTree):
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

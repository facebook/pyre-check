# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors[16]: Pyre does not understand `dataclass_merge`.

from dataclasses import dataclass, field
from typing import List, Optional

import testslide

from ..dataclasses_merge import dataclass_merge, DataclassMergeError, Policy


@dataclass_merge
@dataclass(frozen=True)
class Basic:
    x: Optional[int] = None
    y: Optional[str] = None


@dataclass_merge
@dataclass(frozen=True)
class Nesting:
    a: Optional[bool] = None
    b: Basic = field(default_factory=Basic)


@dataclass_merge
@dataclass(frozen=True)
class Prepend:
    x: List[int] = field(
        default_factory=list, metadata={"merge_policy": Policy.PREPEND}
    )


@dataclass_merge
@dataclass(frozen=True)
class RaiseWhenOverwritten:
    x: Optional[int] = field(
        default=None, metadata={"merge_policy": Policy.RAISE_WHEN_OVERWRITTEN}
    )


def _always_prefer_base(base: Optional[int], override: Optional[int]) -> Optional[int]:
    return base


@dataclass_merge
@dataclass(frozen=True)
class Custom:
    x: Optional[int] = field(
        default=None, metadata={"merge_policy": _always_prefer_base}
    )


class DataclassMergeTest(testslide.TestCase):
    def test_basic(self) -> None:
        self.assertEqual(
            Basic.merge(Basic(x=1, y="base"), Basic(x=2, y="override")),
            Basic(x=2, y="override"),
        )
        self.assertEqual(
            Basic.merge(Basic(x=1, y="base"), Basic(x=2, y=None)),
            Basic(x=2, y="base"),
        )
        self.assertEqual(
            Basic.merge(Basic(x=1, y="base"), Basic(x=None, y="override")),
            Basic(x=1, y="override"),
        )
        self.assertEqual(
            Basic.merge(Basic(x=1, y="base"), Basic(x=None, y=None)),
            Basic(x=1, y="base"),
        )

    def test_nesting(self) -> None:
        self.assertEqual(
            Nesting.merge(
                Nesting(a=True, b=Basic(x=1, y="base")),
                Nesting(a=False, b=Basic(x=2, y="override")),
            ),
            Nesting(a=False, b=Basic(x=2, y="override")),
        )
        self.assertEqual(
            Nesting.merge(
                Nesting(a=True, b=Basic(x=1, y="base")),
                Nesting(a=False, b=Basic(x=2, y=None)),
            ),
            Nesting(a=False, b=Basic(x=2, y="base")),
        )
        self.assertEqual(
            Nesting.merge(
                Nesting(a=True, b=Basic(x=1, y="base")),
                Nesting(a=None, b=Basic(x=None, y="override")),
            ),
            Nesting(a=True, b=Basic(x=1, y="override")),
        )
        self.assertEqual(
            Nesting.merge(
                Nesting(a=True, b=Basic(x=1, y="base")),
                Nesting(a=None, b=Basic(x=None, y=None)),
            ),
            Nesting(a=True, b=Basic(x=1, y="base")),
        )

    def test_prepend(self) -> None:
        self.assertEqual(
            Prepend.merge(Prepend(x=[]), Prepend(x=[2])),
            Prepend(x=[2]),
        )
        self.assertEqual(
            Prepend.merge(Prepend(x=[1]), Prepend(x=[])),
            Prepend(x=[1]),
        )
        self.assertEqual(
            Prepend.merge(Prepend(x=[1, 2]), Prepend(x=[3, 4])),
            Prepend(x=[3, 4, 1, 2]),
        )

    def test_raise_when_overwritten(self) -> None:
        self.assertEqual(
            RaiseWhenOverwritten.merge(
                RaiseWhenOverwritten(x=1), RaiseWhenOverwritten(x=None)
            ),
            RaiseWhenOverwritten(x=1),
        )
        self.assertEqual(
            RaiseWhenOverwritten.merge(
                RaiseWhenOverwritten(x=None), RaiseWhenOverwritten(x=2)
            ),
            RaiseWhenOverwritten(x=2),
        )
        self.assertEqual(
            RaiseWhenOverwritten.merge(
                RaiseWhenOverwritten(x=None), RaiseWhenOverwritten(x=None)
            ),
            RaiseWhenOverwritten(x=None),
        )
        with self.assertRaises(DataclassMergeError):
            RaiseWhenOverwritten.merge(
                RaiseWhenOverwritten(x=1), RaiseWhenOverwritten(x=2)
            )

    def test_custom(self) -> None:
        self.assertEqual(Custom.merge(Custom(x=1), Custom(x=2)), Custom(x=1))
        self.assertEqual(Custom.merge(Custom(x=1), Custom(x=None)), Custom(x=1))
        self.assertEqual(Custom.merge(Custom(x=None), Custom(x=2)), Custom(x=None))
        self.assertEqual(Custom.merge(Custom(x=None), Custom(x=None)), Custom(x=None))

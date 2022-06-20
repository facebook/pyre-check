# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Any, Optional, Tuple


def len(o: Any) -> int:
    return 0


def min(x: int, y: str):
    return 0


def named(*, named_parameter: int, **kw):
    pass


def tito_via_len(o: Any):
    return len(o)


def tito_via_min_left(o: Any):
    return min(o, "")


def tito_via_min_right(o: Any):
    return min(5, o)


def tito_via_named(o: Any):
    return named(named_parameter=o)


def tito_via_min_or_not(o: Any, b: bool):
    if b:
        return min(o, "abc")
    else:
        return o


def tito_via_constructor(o: Any):
    return int(o)


def optional_scalar(parameter) -> Optional[int]:
    return 0


def tito_via_optional(o: Any):
    return optional_scalar(o)


def issue_via_bool():
    o = _test_source()
    x = bool(o)
    _test_sink(x)


def returns_tainted_object() -> object:
    return _test_source()


def issue_via_equality():
    o = returns_tainted_object()
    matches_tainted = o == "tainted"
    _test_sink(matches_tainted)


def return_tuple_of_bools() -> Tuple[bool, bool]:
    return _test_source(), _test_source()


def issue_from_tuple():
    a, b = return_tuple_of_bools()
    _test_sink(a)


def tito_with_tuple(o: Any, b: bool) -> Tuple[bool, bool]:
    if b:
        return min(o, "abc"), min(o, "abc")
    else:
        return o, o


async def async_tuple_of_bools() -> Tuple[bool, bool]:
    return _test_source(), _test_source()


async def async_issue_bools() -> None:
    x, y = await async_tuple_of_bools()
    _test_sink(x)


def dynamic_feature_tito(x):
    return x


def issue_via_in():
    o = _test_source()
    _test_sink("a" in o)
    _test_sink(o in "a")


def always_feature_for_duplicate_models(arg):
    # Should see "always-via" for feature string_concat_rhs
    return 0


class SetItemClass:
    def __setitem__(self, key, value):
        pass


def no_always_when_calling_setitem():
    o = _test_source()
    a = SetItemClass()
    a["test_key"] = o
    # This test ensures the problem in T62465444 does not appear again.
    # The model of this method should have feature "always-via": "copy",
    # instead of "via": "copy"
    return a

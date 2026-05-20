# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools
from typing import Callable, TypeVar

T = TypeVar("T")
TSelf = TypeVar("TSelf")


def _test_sink(arg):
    ...


def _test_source():
    ...


def cachedmethod(f: Callable[[TSelf], T]) -> Callable[[TSelf], T]:
    cache: str = f"_cache_{f.__name__}"

    @functools.wraps(f)
    def g(self: TSelf) -> T:
        if hasattr(self, cache):
            return getattr(self, cache)
        setattr(self, cache, f(self))
        return getattr(self, cache)

    return g


class MyClass:
    def __init__(self, value: str) -> None:
        self.value = value

    @cachedmethod
    def __repr__(self) -> str:
        return self.value


def test_str_of_decorated_repr(x: MyClass) -> None:
    _test_sink(str(x))

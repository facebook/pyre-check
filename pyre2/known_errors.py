# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This a list of known errors, in some cases that people are working on.

from pathlib import Path
from typing import (
    Callable,
    Collection,
    Generic,
    ParamSpec,
    SupportsIndex,
    TypeAlias,
    TypeVar,
    Unpack,
)

# Steven: Generic, Protocol
_T = TypeVar("_T")


class BaseClass(Generic[_T]):
    pass


protocol_subtype_1: SupportsIndex = -1

protocol_subtype_2: Collection[int] = (1, 2, 3)


# Sam: Callable, ParamSpec, Unpack
_P = ParamSpec("_P")

def complex_callable(func: Callable[_P, bool], *args: Unpack[tuple[str, str]]):
    pass

# Up for grabs: try
def try_statement():
    try:
        pass
    except Exception as e:
        pass
    else:
        pass
    finally:
        pass


# Up for grabs: overload
def overload():
    print("test")


# Up for grabs: yield
def yielding():
    yield 1


# Up for grabs: __new__
def dunder_new():
    return Path("test")

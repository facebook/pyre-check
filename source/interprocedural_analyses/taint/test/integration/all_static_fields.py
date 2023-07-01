# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass
from typing import Optional, Union, List, Type


"""
The .pysa file adds `TaintSink[Test, ParameterPath[_.all_static_fields()]]` on
all parameters of functions. See `.models` file for the results.
"""


class RegularClass:
    def __init__(self, a: int, b: str) -> None:
        self.a = a
        self.b = b


def regular_class(parameter: RegularClass) -> None:
    pass


def optional_regular_class(parameter: Optional[RegularClass]) -> None:
    pass


@dataclass
class Dataclass:
    c: int
    d: str


def dataclass(parameter: Dataclass) -> None:
    pass


def optional_dataclass(parameter: Optional[Dataclass]) -> None:
    pass


def union_dataclass_regular(parameter: Union[Dataclass, RegularClass]) -> None:
    pass


# Builtins do not have attributes, so we just add a sink on the whole parameter.
def builtin_parameters(x: int, y: str, z: List[int], t: bool) -> None:
    pass


# If a parameter is not annotated, we just add a sink on the whole parameter.
def unnannotated(x) -> None:
    pass


class A:
    """Test doc string"""

    def __init__(self, a: int) -> None:
        self.a = a


class B(A):
    pass


class C(B):
    def __init__(self, a: int, c: int) -> None:
        super().__init__(a)
        self.c = c


class D(A):
    d: int

    def __init__(self, a: int, d: int) -> None:
        super().__init__(a)
        self.d = d


def test_b(parameter: B) -> None:
    pass


def test_c(parameter: C) -> None:
    pass


def test_d(parameter: D) -> None:
    pass


def test_union_c_d(parameter: Union[C, D]) -> None:
    pass


def test_type(parameter: Type[A]) -> None:
    pass

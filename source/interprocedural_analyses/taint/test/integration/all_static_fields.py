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


def parameter_sink_regular_class(parameter: RegularClass) -> None:
    pass


def return_source_regular_class() -> RegularClass:
    return RegularClass(a=0, b="")


def parameter_sink_optional_regular_class(parameter: Optional[RegularClass]) -> None:
    pass


def return_source_optional_regular_class() -> Optional[RegularClass]:
    return None


@dataclass
class Dataclass:
    c: int
    d: str


def parameter_sink_dataclass(parameter: Dataclass) -> None:
    pass


def return_source_dataclass() -> Dataclass:
    return Dataclass(c=0, d="")


def parameter_sink_optional_dataclass(parameter: Optional[Dataclass]) -> None:
    pass


def return_source_optional_dataclass() -> Optional[Dataclass]:
    return None


def parameter_sink_union_dataclass_regular(
    parameter: Union[Dataclass, RegularClass]
) -> None:
    pass


def return_source_union_dataclass_regular() -> Union[Dataclass, RegularClass]:
    return RegularClass(a=0, b="")


# Builtins do not have attributes, so we just add a sink on the whole parameter.
def parameter_sink_builtin_parameters(x: int, y: str, z: List[int], t: bool) -> None:
    pass


# Builtins do not have attributes, so we just add a source on the whole return value.
def return_source_builtin_int() -> int:
    return 0


def return_source_builtin_list() -> List[int]:
    return []


# If a parameter is not annotated, we just add a sink on the whole parameter.
def parameter_sink_unnannotated(x) -> None:
    pass


# If the return value is not annotated, we just add a source on value.
def return_source_unnannotated():
    return 0


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


def parameter_sink_b(parameter: B) -> None:
    pass


def return_source_b() -> B:
    return C(a=0, c=0)


def parameter_sink_c(parameter: C) -> None:
    pass


def return_source_c() -> C:
    return C(a=0, c=0)


def parameter_sink_d(parameter: D) -> None:
    pass


def return_source_d() -> D:
    return D(a=0, d=0)


def parameter_sink_union_c_d(parameter: Union[C, D]) -> None:
    pass


def return_source_union_c_d() -> Union[C, D]:
    return D(a=0, d=0)


def parameter_sink_type(parameter: Type[A]) -> None:
    pass


def return_source_type() -> Type[A]:
    return A


class Empty:
    def __init__(self) -> None:
        pass


def parameter_sink_empty(parameter: Empty) -> None:
    pass


def return_source_empty() -> Empty:
    return Empty()


async def return_source_async() -> bool:
    return True

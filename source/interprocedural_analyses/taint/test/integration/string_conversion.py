# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source
from typing import Any, Type, Union

from django.http import Request


request: Request = ...


class StrIsTainted:
    def __str__(self):
        return request.GET["tainted"]


class ReprIsTainted:
    def __repr__(self):
        return request.GET["tainted"]


def str_is_tainted():
    s = StrIsTainted()
    eval(str(s))


def repr_is_tainted():
    r = ReprIsTainted()
    eval(repr(r))


def str_falls_back_to_repr():
    r = ReprIsTainted()
    eval(str(r))


def implicit_str():
    s = StrIsTainted()
    eval(f"prefix{s}suffix")  # noqa: P204


def implicit_repr():
    r = ReprIsTainted()
    eval(f"prefix{r}suffix")  # noqa: P204


def explicit_str():
    s = StrIsTainted()
    eval(f"prefix{s.__str__()}suffix")  # noqa: P204


class A:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value


def propagate_taint():
    eval(f"{A(request.GET['tainted'])}")  # noqa: P204


def not_propagate_taint():
    eval(f"{A('not tainted')}")  # noqa: P204


def multiple_targets_for_single_expression(x: Union[A, StrIsTainted]):
    # two targets: [A.__str__, StrIsTainted.__str__]
    eval(f"{x}")  # noqa: P204


class B:
    f: str = ""

    def __str__(self):
        return self.f


class C:
    g: str = ""

    def __str__(self):
        return self.g


def join_source_and_attribute_source(i: int):
    if i > 0:
        a: str = request.GET["tainted"]
    else:
        a: C = C()
    eval(f"{a}")  # noqa: P204


def multiple_targets_for_single_expression_2(a: Union[int, B, C]):
    eval(f"{a}")  # noqa: P204


def joined_base():
    a = request.GET["tainted"]
    b = "benign"
    eval(f"{a}{b}")  # noqa: P204


def analyze_implicit_call():
    b = B()
    b.f = request.GET["tainted"]
    # Require inferring a tito model for `B.__str__`
    eval(f"{str(b)}")  # noqa: P204.
    # Require analyzing an implicit call to `str(b)`
    eval(f"{b}")  # noqa: P204.


def multiple_targets_for_single_expression_3(b_or_c: Union[B, C], d: int):
    a = 1
    # Require the proper accumulation of tito information under implicit `str`
    return f"{a}{b_or_c}{d}"


def tito_f(x):
    return x


def tito_g(y):
    return y


def compute_tito(x, y):
    # Require the proper accumulation of tito information
    return f"{tito_g(y)}{tito_f(x)}"


class D:
    def __str__(self):
        return "benign"


def forward_unioned_callees():
    x: Union[str, D] = _test_source()
    return f"{x}"


def forward_unioned_callees_2():
    x: Union[Any, D] = _test_source()
    return f"{x}"


def backward_unioned_callees(x: Union[str, D]):
    return f"{x}"


def backward_unioned_callees_2(x: Union[Any, D]):
    return f"{x}"


def any_type(x: Any):
    return f"{x}"


def object_type(x: object):
    return f"{x}"


class OverrideStr(float):
    def __str__(self):
        x = _test_source()
        return f"{x}"  # __str__ method may introduce sources


def base_exception(e: Exception):
    return f"{type(e)}"


def function_call_target_1(error_type: Union[str, Type[Exception]]):
    f"{error_type}"  # Resolved as an implicit call to a function


def function_call_target_2(x: Union[B, C]):
    f"{x.__class__}"  # Resolved as an implicit call to a function


def multiple_callees_same_location():
    s = StrIsTainted()
    # The call to str() and the attribute access to str.__add__ have the same location in the AST.
    return str(s) + "hello"

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Union

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

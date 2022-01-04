# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.http import HttpRequest, HttpResponse


def example_sanitizer():
    ...


def get_operator_safe(request: HttpRequest) -> str:
    operator = request.POST["operator"]

    assert operator in {"+", "-", "*", "/"}

    return operator


def operate_on_twos(request: HttpRequest) -> HttpResponse:
    operator = get_operator_safe(request)

    result = eval(f"2 {operator} 2")  # noqa: P204

    return result


def operate_on_threes(request: HttpRequest) -> HttpResponse:
    operator = request.GET["operator"]

    assert operator in {"+", "-", "*", "/"}

    exec(f"result = 3 {operator} 3")

    return result  # noqa: F821

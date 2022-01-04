# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.http import HttpRequest, HttpResponse


def example_feature(argument: str) -> None:
    ...


def assert_numeric(operand: str) -> None:
    assert operand.isnumeric()


def do_and(request: HttpRequest) -> HttpResponse:
    left = bool(request.GET["left"])
    right = bool(request.GET["right"])

    result = eval(f"{left} and {right}")  # noqa: P204

    return result


def do_or(request: HttpRequest) -> HttpResponse:
    left = request.GET["left"]
    right = request.GET["right"]

    assert_numeric(left)
    assert_numeric(right)

    result = eval(f"{left} or {right}")  # noqa: P204

    return result

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import Callable

from django.http import HttpRequest

from .logging_decorator import with_logging_with_helper, with_logging_without_helper


def with_logging(f: Callable[[str], None]) -> Callable[[str], None]:
    def inner(x: str) -> None:
        eval(x)
        f(x)

    return inner


@with_logging
def foo(x: str) -> None:
    print(x)


@with_logging_with_helper
@with_logging_without_helper
def foo2(x: int) -> None:
    eval(x)


def bar(request: HttpRequest) -> None:
    foo(request.GET["bad"])
    foo2(request.GET["bad"])

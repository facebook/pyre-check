# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import Callable

from django.http import HttpRequest, HttpResponse


def with_logging(f: Callable[[str], None]) -> Callable[[str], None]:
    def inner(x: str) -> None:
        eval(x)
        f(x)

    return inner


@with_logging
def foo(x: str) -> None:
    print(x)


def bar(request: HttpRequest) -> None:
    foo(request.GET["bad"])

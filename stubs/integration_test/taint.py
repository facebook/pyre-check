# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.http import HttpRequest


def source() -> str:
    request = HttpRequest()
    return request.GET["bad"]


def sink(argument: str) -> None:
    eval(argument)  # noqa

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.http import HttpRequest


def concatenate_lhs(request: HttpRequest):
    source = request.GET["bad"]
    return source + "A"


def concatenate_rhs(request: HttpRequest):
    source = request.GET["bad"]
    return "A" + source


def bad_1(request: HttpRequest):
    a = concatenate_lhs(request)
    eval(a)  # noqa


def bad_2(request: HttpRequest):
    a = concatenate_rhs(request)
    eval(a)  # noqa

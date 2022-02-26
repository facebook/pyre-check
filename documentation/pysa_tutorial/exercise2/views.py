# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess

from django.http import HttpRequest, HttpResponse


def operate_on_twos(request: HttpRequest) -> HttpResponse:
    operator = request.POST["operator"]

    result = eval(f"2 {operator} 2")  # noqa: P204

    return result


def operate_on_threes(request: HttpRequest) -> HttpResponse:
    operator = request.GET["operator"]

    exec(f"result = 3 {operator} 3")

    return result  # noqa: F821


def operate_on_fours(request: HttpRequest) -> HttpResponse:
    operator = request.GET["operator"]

    result = subprocess.getoutput(f"expr 4 {operator} 4")

    return result

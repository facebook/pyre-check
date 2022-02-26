# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from typing import Dict, Optional

from django.http import HttpRequest, HttpResponse


# Integration test illustrating flows from request sources.


def test_index(request: HttpRequest):
    eval(request.GET["bad"])


def test_get(request: HttpRequest):
    eval(request.GET.get("bad"))


def test_getlist(request: HttpRequest):
    eval(request.GET.getlist("bad"))


def test_optional(request: Optional[HttpRequest]):
    eval(request.GET["bad"])


def test_assigning_to_request_fields(request: HttpRequest):
    request.GET = request.GET.copy()
    eval(request.GET["bad"])
    request.POST = request.POST.copy()
    eval(request.POST["bad"])

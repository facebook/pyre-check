# flake8: noqa

from typing import Optional

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

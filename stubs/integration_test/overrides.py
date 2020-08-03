# flake8: noqa

from typing import Iterable, TypeVar

from django.http import HttpRequest


T = TypeVar("T")


class MyIterable(Iterable[T]):
    def __iter__(self):
        request = HttpRequest()
        return request.GET["bad"]


def issue_with_direct_call_of_subclass(mi: MyIterable[int]):
    eval(mi.__iter__())


def no_issue_with_iterable_call(mi: Iterable[int]):
    eval(mi.__iter__())

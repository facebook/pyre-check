# flake8: noqa
from django.http import Request


request: Request


class StrIsTainted:
    def __str__(self):
        return request.GET["tainted"]


class ReprIsTainted:
    def __repr__(self):
        return request.GET["tainted"]


def str_is_tainted():
    s = StrIsTainted()
    eval(str(s))


def repr_is_tainted():
    r = ReprIsTainted()
    eval(repr(r))


def str_falls_back_to_repr():
    r = ReprIsTainted()
    eval(str(r))

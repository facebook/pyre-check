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

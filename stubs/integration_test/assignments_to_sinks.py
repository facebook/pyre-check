# @nolint

import pyre
from django.http import HttpRequest


# Integration test illustrating how we deal with assignments to sinks.


def indirect(into_global_sink):
    pyre.__global_sink = into_global_sink


def test_indirect(request: HttpRequest):
    source = request.GET["bad"]
    indirect(source)


def test_direct(request: HttpRequest):
    source = request.GET["bad"]
    pyre.__global_sink = source

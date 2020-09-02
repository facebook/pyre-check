# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import __test_sink, __test_source

from django.http.request import HttpRequest


def test_untainted_assign(request: HttpRequest):
    request.GET = {}
    __test_sink(request.GET)


def test_trace_has_no_tito(request: HttpRequest):
    request.GET = __test_source()
    __test_sink(request.GET)


def request_get_flows_to_sink(request: HttpRequest):
    __test_sink(request.GET)


def test_hop_is_cut_off(request: HttpRequest):
    request.GET = __test_source()
    request_get_flows_to_sink(request)

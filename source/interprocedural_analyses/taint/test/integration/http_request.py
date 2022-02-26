# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source

from django.http.request import HttpRequest


def test_untainted_assign(request: HttpRequest):
    request.GET = {}
    _test_sink(request.GET)


def test_trace_has_no_tito(request: HttpRequest):
    request.GET = _test_source()
    _test_sink(request.GET)


def request_get_flows_to_sink(request: HttpRequest):
    _test_sink(request.GET)


def test_hop_is_cut_off(request: HttpRequest):
    request.GET = _test_source()
    request_get_flows_to_sink(request)

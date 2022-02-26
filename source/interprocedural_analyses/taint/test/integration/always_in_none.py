# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import _test_sink, _test_source

from django.http import HttpRequest, HttpResponse


class ComplicatedService:
    def serve_tainted_request(self):
        return "Valid"


def test(complicated_service: ComplicatedService):
    exception = False
    result = None
    try:
        result = complicated_service.serve_tainted_request()

    except:
        exception = True

    # Only try reactivation if all other checks passed
    if exception:
        try:
            result = complicated_service.serve_tainted_request()
        except:
            raise

    _test_sink(result)


def test_none_clears_taint():
    x = _test_source()
    x = None
    _test_sink(x)

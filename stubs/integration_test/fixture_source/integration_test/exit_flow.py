# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from django.http import HttpRequest, HttpResponse


# Integration test illustrating flows to exit nodes.


def test_to_response(request: HttpRequest):
    source = request.GET["bad"]
    return HttpResponse(content=source)

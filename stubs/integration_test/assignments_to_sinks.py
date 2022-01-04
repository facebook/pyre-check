# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

import pyre
from django.http import HttpRequest


# Integration test illustrating how we deal with assignments to sinks.


def indirect(into_global_sink):
    pyre._global_sink = into_global_sink


def test_indirect(request: HttpRequest):
    source = request.GET["bad"]
    indirect(source)


def test_direct(request: HttpRequest):
    source = request.GET["bad"]
    pyre._global_sink = source

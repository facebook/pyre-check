# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from functools import lru_cache

from django.http import HttpRequest


@lru_cache
def cached_sanitizer(x):
    return x


def test_cached_sanitizer(request: HttpRequest) -> None:
    sanitized = cached_sanitizer(request.GET["bad"])
    eval(sanitized)

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from functools import lru_cache, wraps

from django.http import HttpRequest


def invoker(f, *args, **kwargs):
    f(*args, **kwargs)


@lru_cache
def cached_eval(s: str) -> None:
    eval(s)


def test_cached_eval(request: HttpRequest):
    cached_eval(request.GET["bad"])  # This is an issue.


def test_cached_eval_inner(request: HttpRequest):
    @lru_cache
    def inner(s: str) -> None:
        eval(s)

    inner(request.GET["bad"])  # This is an issue.


def test_cached_eval_higher_order_function(request: HttpRequest):
    @lru_cache
    def inner(s: str) -> None:
        eval(s)

    invoker(inner, request.GET["bad"])  # This is an issue.


@lru_cache(maxsize=10)
def cached_eval_with_maxsize(s: str) -> None:
    eval(s)


def test_cached_eval_with_maxsize(request: HttpRequest):
    cached_eval_with_maxsize(request.GET["bad"])  # This is an issue.


@lru_cache
def cached_sanitizer(x):
    return x


def test_cached_sanitizer(request: HttpRequest) -> None:
    sanitized = cached_sanitizer(request.GET["bad"])
    eval(sanitized)  # This is NOT an issue.


def test_wraps(f, request: HttpRequest):
    @wraps(f)
    def wrapper():
        return f(request.GET["bad"])

    eval(wrapper())  # This is an issue.

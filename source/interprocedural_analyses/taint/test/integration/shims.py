# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
import functools
import multiprocessing
import typing
from pysa import _test_sink, _test_source, to_callable_target


@to_callable_target
def async_callable(x, y):
    _test_sink(x)


def test_async_delay():
    async_callable.async_delay(0, 0)
    async_callable.async_delay(0, _test_source())
    async_callable.async_delay(_test_source(), 0)


def test_async_schedule():
    async_callable.async_schedule(0, 0)
    async_callable.async_schedule(0, _test_source())
    async_callable.async_schedule(_test_source(), 0)

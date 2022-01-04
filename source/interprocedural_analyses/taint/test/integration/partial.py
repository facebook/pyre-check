# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
import multiprocessing
from builtins import _test_sink, _test_source
from functools import partial


def a_flows_to_sink(a, b):
    _test_sink(a)


def partial_application_with_tainted():
    x = _test_source()
    partial(a_flows_to_sink, x)


def partial_application_with_benign():
    x = 1
    partial(a_flows_to_sink, x)


def partial_application_with_named_a():
    x = _test_source()
    partial(a_flows_to_sink, a=x)


def partial_application_with_named_b():
    x = _test_source()
    partial(a_flows_to_sink, b=x)


def multiprocessing_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(_test_source(), 1))


def multiprocessing_not_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(1, _test_source()))

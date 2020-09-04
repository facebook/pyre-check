# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source
from typing import Sequence, TypeVar


T = TypeVar("T")


def return_taint_sanitize(arg: T) -> T:
    """Identity function that returns the argument unmodified, but is marked as
    'Sanitize' in the accompanying .pysa file
    """
    return arg


def test1():
    tainted = object()
    tainted.id = __test_source()
    test2(tainted)
    test3(tainted)


def test2(tainted_other):
    tainted = return_taint_sanitize(tainted_other)
    __test_sink(tainted.id)  # This shouldn't be a flow, but it is


def test3(colliding_name):
    # TODO(T59731231): There shouldn't be a false positive here either.
    colliding_name = return_taint_sanitize(colliding_name)
    __test_sink(colliding_name.id)


def source_with_tito(x):
    return x


def sanitize_sources(x):
    __test_sink(x)
    return source_with_tito(x)


def sanitize_sinks(x):
    __test_sink(x)
    return source_with_tito(x)


def sanitize_tito(x):
    __test_sink(x)
    return source_with_tito(x)

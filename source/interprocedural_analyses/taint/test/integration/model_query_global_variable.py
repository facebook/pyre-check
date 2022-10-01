# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink
from os import environ

foo = []


def f():
    _test_sink(environ)


# TODO (T132423781): classes are not correctly selected as sources
class Baz:
    ...


def g():
    foo.append(1)


def h():
    _test_sink(Baz)

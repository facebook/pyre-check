# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import TypedDict


class Foo(TypedDict):
    a: int
    b: int


class Bar(TypedDict):
    other: int
    foo: Foo


def test1():
    bar: Bar = _test_source()
    _test_sink(bar["other"])


def test2():
    bar: Bar = _test_source()
    # TODO(T81192268): this should not trigger an issue.
    _test_sink(bar["foo"]["a"])


def test3():
    bar: Bar = _test_source()
    _test_sink(bar["foo"]["b"])

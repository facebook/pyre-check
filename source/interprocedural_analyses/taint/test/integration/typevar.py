# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import TypeVar

from pysa import _test_sink

T = TypeVar("T", bound="Foo")


class Foo:
    def __init__(self, tainted: str) -> None:
        self.tainted: str = tainted


def issue(foo: T) -> T:
    _test_sink(foo.tainted)
    return foo

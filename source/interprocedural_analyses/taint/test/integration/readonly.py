# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_source, _test_sink
from typing import Any, MutableMapping
from typing_extensions import Self
from pyre_extensions import PyreReadOnly


class A:
    def __init__(self) -> None:
        self.B: str = ""
        self.mapping: MutableMapping[str, Any] = {}

    def self_readonly_str(
        self: PyreReadOnly[Self]
    ) -> None:
        _test_sink(self.B)

    def self_untyped_str(
        self
    ) -> None:
        _test_sink(self.B)

    def self_readonly_map(
        self: PyreReadOnly[Self]
    ) -> None:
        # pyre-ignore[3005]: Ignore ReadOnly Violation
        _test_sink(self.mapping.get(""))

    def self_untyped_map(
        self
    ) -> None:
        _test_sink(self.mapping.get(""))

    def readonly_tito(self, x: PyreReadOnly[str]):
        return x


def readonly_tito():
    a = A()
    x = a.readonly_tito(_test_source())
    _test_sink(x)


class Foo:
    tainted: str = ""
    not_tainted: str = ""


def readonly_foo_tainted(foo: PyreReadOnly[Foo]) -> None:
    _test_sink(foo.tainted)


def readonly_foo_not_tainted(foo: PyreReadOnly[Foo]) -> None:
    _test_sink(foo.not_tainted)


def regular_foo_tainted(foo: Foo) -> None:
    _test_sink(foo.tainted)


def regular_foo_not_tainted(foo: Foo) -> None:
    _test_sink(foo.not_tainted)

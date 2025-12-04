# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source
from typing import TypeVar
from typing_extensions import Self

MySelf = TypeVar("MySelf")


class Base:
    def __init__(self) -> None:
        self.foo: str = ""
        self.bar: str = ""

    def foo_sink(self) -> None:
        _test_sink(self.foo)

    def bar_sink(self) -> None:
        _test_sink(self.bar)

    def mutates_foo(self) -> None:
        self.foo = _test_source()

    def mutates_foo_with_hop(self) -> None:
        self.mutates_foo()

    def isssue_mutates_foo(self) -> None:
        self.mutates_foo()
        self.foo_sink()  # Issue.
        self.bar_sink()  # Not an issue.

    def issue_mutates_foo_with_hop(self) -> None:
        self.mutates_foo_with_hop()
        self.foo_sink()  # Issue.
        self.bar_sink()  # Not an issue.

    def mutates_foo_and_returns(self) -> "Base":
        self.foo = _test_source()
        return self

    def issue_mutates_and_returns(self) -> None:
        self.mutates_foo_and_returns().foo_sink()  # Issue.

    # pyre-ignore[47]: Self is a valid type
    def mutates_foo_self_annotation(self: Self) -> None:
        self.foo = _test_source()

    def issue_mutates_foo_self_annotation(self) -> None:
        self.mutates_foo_self_annotation()
        self.foo_sink()  # Issue.
        self.bar_sink()  # Not an issue.

    def mutates_foo_self_typevar(self: MySelf) -> None:
        self.foo = _test_source()

    def issue_mutates_foo_self_typevar(self) -> None:
        self.mutates_foo_self_typevar()
        self.foo_sink()  # Issue.
        self.bar_sink()  # Not an issue.


def issue_mutates_foo_instance() -> None:
    b = Base()
    b.mutates_foo()
    b.foo_sink()  # Issue.
    b.bar_sink()  # Not an issue.


def issue_mutates_foo_and_returns_instance() -> None:
    Base().mutates_foo_and_returns().foo_sink()  # Issue.


def free_function_mutates_self(self: Base) -> None:
    self.foo = _test_source()


def issue_free_function() -> None:
    b = Base()
    free_function_mutates_self(b)
    b.foo_sink()  # TODO(T171333442): Issue, currently a false negative.
    b.bar_sink()  # Not an issue.

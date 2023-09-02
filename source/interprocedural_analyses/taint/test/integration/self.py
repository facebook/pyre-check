# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source, _test_sink
from typing import TypeVar
from typing_extensions import Self

TFoo = TypeVar("TFoo", bound="Foo")


class Foo:
    tainted_class: str = ""
    not_tainted_class: str = ""

    def __init__(self, tainted_instance: str, not_tainted_instance: str) -> None:
        self.tainted_instance: str = tainted_instance
        self.not_tainted_instance: str = not_tainted_instance
        self.tainted_extra_instance: str = _test_source()
        self.not_tainted_extra_instance: str = ""

    def untyped_self_class_direct(self) -> None:
        _test_sink(self.__class__.tainted_class)

    def untyped_self_class(self) -> None:
        _test_sink(self.tainted_class)  # TODO(T162455102): False Negative class attribute access through instance

    def untyped_self_instance(self) -> None:
        _test_sink(self.tainted_instance)  # TODO(T162455745): False Negative instance attribute access

    def untyped_self_extra_instance(self) -> None:
        _test_sink(self.tainted_extra_instance)  # TODO(T162455745): False Negative instance attribute access

    def untyped_self_not_tainted(self) -> None:
        _test_sink(self.not_tainted_class)
        _test_sink(self.not_tainted_instance)
        _test_sink(self.not_tainted_extra_instance)

    def untyped_access_self(self) -> "Foo":
        return self

    def typevar_self_class_direct(self: TFoo) -> TFoo:
        _test_sink(self.__class__.tainted_class)
        return self

    def typevar_self_class(self: TFoo) -> TFoo:
        _test_sink(self.tainted_class)  # TODO(T162456424): False Negative attribute access through typevar
        return self

    def typevar_self_instance(self: TFoo) -> TFoo:
        _test_sink(self.tainted_instance)  # TODO(T162456424): False Negative attribute access through typevar
        return self

    def typevar_self_extra_instance(self: TFoo) -> TFoo:
        _test_sink(self.tainted_extra_instance)  # TODO(T162456424): False Negative attribute access through typevar
        return self

    def typevar_self_not_tainted(self: TFoo) -> TFoo:
        _test_sink(self.not_tainted_class)
        _test_sink(self.not_tainted_instance)
        _test_sink(self.not_tainted_extra_instance)
        return self

    def typevar_access_self(self: TFoo, other: TFoo) -> TFoo:
        return self

    def typevar_access_other(self: TFoo, other: TFoo) -> TFoo:
        return self

    # pyre-ignore[11]: Self is valid type
    def selftype_self_class_direct(self: Self) -> Self:
        _test_sink(self.__class__.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
        return self

    def selftype_self_class(self: Self) -> Self:
        _test_sink(self.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
        return self

    def selftype_self_instance(self: Self) -> Self:
        _test_sink(self.tainted_instance)  # TODO(T162456612): False Negative attribute access through selftype
        return self

    def selftype_self_extra_instance(self: Self) -> Self:
        _test_sink(self.tainted_extra_instance)  # TODO(T162456612): False Negative attribute access through selftype
        return self

    def selftype_self_not_tainted(self: Self) -> Self:
        _test_sink(self.not_tainted_class)
        _test_sink(self.not_tainted_instance)
        _test_sink(self.not_tainted_extra_instance)
        return self

    def selftype_access_self(self: Self, other: Self) -> Self:
        return self

    def selftype_access_other(self: Self, other: Self) -> Self:
        return self

    def selftype_access_untyped_self(self, other: Self) -> Self:
        return self


def foo_class_attributes() -> None:
    _test_sink(Foo.tainted_class)
    _test_sink(Foo.not_tainted_class)


def untyped_access_self() -> None:
    f = Foo("", "")
    _test_sink(f.__class__.tainted_class)
    _test_sink(f.tainted_class)  # TODO(T162455102): False Negative class attribute access through instance
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)  # TODO(T162457000): False Positive class attribute access through instance __class__
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)


def typevar_access_self() -> None:
    f1, f2 = Foo("", ""), Foo("", "")
    f = f1.typevar_access_self(f2)
    _test_sink(f.__class__.tainted_class)
    _test_sink(f.tainted_class)  # TODO(T162456424): False Negative attribute access through typevar
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)  # TODO(T162457164): False Positive class attribute access through typevar instance __class__
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)


def typevar_access_other() -> None:
    f1, f2 = Foo("", ""), Foo("", "")
    f = f1.typevar_access_other(f2)
    _test_sink(f.__class__.tainted_class)
    _test_sink(f.tainted_class)  # TODO(T162456424): False Negative attribute access through typevar
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)  # TODO(T162457164): False Positive class attribute access through typevar instance __class__
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)


def selftype_access_self() -> None:
    f1, f2 = Foo("", ""), Foo("", "")
    f = f1.selftype_access_self(f2)
    _test_sink(f.__class__.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
    _test_sink(f.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)


def selftype_access_other() -> None:
    f1, f2 = Foo("", ""), Foo("", "")
    f = f1.selftype_access_other(f2)
    _test_sink(f.__class__.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
    _test_sink(f.tainted_class)  # TODO(T162456612): False Negative attribute access through selftype
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)


def selftype_access_untyped_self() -> None:
    f1, f2 = Foo("", ""), Foo("", "")
    f = f1.selftype_access_untyped_self(f2)
    _test_sink(f.__class__.tainted_class)
    _test_sink(f.tainted_class)  # TODO(T162455102): False Negative class attribute access through instance
    _test_sink(f.tainted_instance)
    _test_sink(f.tainted_extra_instance)

    _test_sink(f.__class__.not_tainted_class)  # TODO(T162457000): False Positive class attribute access through instance __class__
    _test_sink(f.not_tainted_class)
    _test_sink(f.not_tainted_instance)
    _test_sink(f.not_tainted_extra_instance)

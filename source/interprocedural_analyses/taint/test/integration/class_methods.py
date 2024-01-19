# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from abc import abstractclassmethod
from typing import TypeVar, Generic


class Test:
    @classmethod
    def foo(cls, x) -> None:
        return _test_sink(x)


def bar():
    Test.foo(_test_source())


TInput = TypeVar("TInput")


class C(Generic[TInput]):
    @abstractclassmethod
    def abstract_class_method(cls, arg):
        _test_sink(arg)

    @classmethod
    def one_hop_abstract_class_method(cls, arg):
        # Expect sink on `arg`
        cls.abstract_class_method(arg)

    @classmethod
    def class_method(cls, arg):
        _test_sink(arg)

    @classmethod
    def one_hop_class_method(cls, arg):
        # Expect sink on `arg`
        cls.class_method(arg)


def issue_with_abstract_class_method():
    C.one_hop_abstract_class_method(_test_source())  # Expect an issue
    C.one_hop_class_method(_test_source())  # Expect an issue

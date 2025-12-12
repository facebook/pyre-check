# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source
from abc import abstractclassmethod, abstractmethod  # pyrefly: ignore[deprecated]
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
    def abstract_class_method(cls, arg: TInput):
        pass

    @classmethod
    def one_hop_abstract_class_method(cls, arg: TInput):
        cls.abstract_class_method(arg)  # Expect sink on `arg`

    @classmethod
    def class_method(cls, arg: TInput):
        _test_sink(arg)

    @classmethod
    def one_hop_class_method(cls, arg: TInput):
        cls.class_method(arg)  # Expect sink on `arg`

    @abstractmethod
    def abstract_method(self, arg):
        pass

    @classmethod
    def one_hop_abstract_method(cls, arg):
        # pyrefly: ignore[missing-argument]
        cls.abstract_method(arg)  # Expect sink on `arg`

    @classmethod
    @abstractmethod
    def class_abstractmethod(cls, arg):
        pass

    @classmethod
    def one_hop_class_abstractmethod(cls, arg):
        cls.class_abstractmethod(arg)  # Expect sink on `arg`

    @staticmethod
    @abstractmethod
    def static_abstractmethod(arg):
        _test_sink(arg)

    @classmethod
    def one_hop_static_abstractmethod(cls, arg):
        # Currently false negative
        cls.static_abstractmethod(arg)  # Expect sink on `arg`

    @property
    @abstractmethod
    def my_abstract_property(self):
        pass

    @my_abstract_property.setter
    @abstractmethod
    def my_abstract_property(self, my_abstract_property):
        pass


class D(C[str]):
    @classmethod
    def abstract_class_method(cls, arg: str):
        _test_sink(arg)

    def abstract_method(self, arg):
        _test_sink(arg)

    @classmethod
    def class_abstractmethod(cls, arg):
        _test_sink(arg)

    @property
    def my_abstract_property(self):
        return self._my_abstract_property

    @my_abstract_property.setter
    def my_abstract_property(self, my_abstract_property):
        self._my_abstract_property = my_abstract_property


def issue_with_abstract_class_method():
    D.one_hop_abstract_class_method(_test_source())  # Expect an issue
    D.one_hop_class_method(_test_source())  # Expect an issue
    D.one_hop_abstract_method(
        _test_source()
    )  # Expect an issue. Currently false negative.
    D.one_hop_class_abstractmethod(_test_source())  # Expect an issue
    D.one_hop_static_abstractmethod(_test_source())  # Expect an issue.


def issue_with_abstract_property():
    d = D()
    d.my_abstract_property = _test_source()
    _test_sink(d.my_abstract_property)  # Expect an issue


class ObscureClassMethodTito:
    def __init__(self, value: str) -> None:
        self.value = value

    @classmethod
    def method(cls): ...


def test_obscure_class_method_tito():
    o = ObscureClassMethodTito(_test_source())
    # TODO(T113911314): False positive
    _test_sink(o.method())

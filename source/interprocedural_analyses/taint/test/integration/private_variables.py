# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import List


class Simple:
    def __init__(self, private: str = "", public: str = "") -> None:
        self.__value: str = private
        self.value: str = public

    def private_into_sink(self) -> None:
        _test_sink(self.__value)

    def public_into_sink(self) -> None:
        _test_sink(self.value)

    @staticmethod
    def expand_subexpression(values: List[Simple]) -> None:
        # Private variables are expended in all expressions.
        _test_sink(values[0].__value)

    def getattr_public(self) -> str:
        return getattr(self, "value")

    def getattr_private(self) -> str:
        return getattr(self, "_Simple__value")

    def getattr_invalid(self) -> str:
        # This should not work according to the documentation.
        # pyre-ignore
        return getattr(self, "__value")


def test_simple() -> None:
    Simple(private=_test_source()).private_into_sink()


def test_private_public_different() -> None:
    Simple(private=_test_source()).private_into_sink()  # Error.
    Simple(private=_test_source()).public_into_sink()  # No error.
    Simple(public=_test_source()).private_into_sink()  # No error.
    Simple(public=_test_source()).public_into_sink()  # Error.


def test_expand_subexpression() -> None:
    Simple.expand_subexpression([Simple(private=_test_source())])  # Error
    Simple.expand_subexpression([Simple(), Simple(private=_test_source())])  # No error.


def test_getattr() -> None:
    # Expect no error, currently a false positive.
    _test_sink(Simple(private=_test_source()).getattr_public())
    # Expect no error, currently a false positive.
    _test_sink(Simple(private=_test_source()).getattr_private())
    # Expect no error, currently a false positive.
    _test_sink(Simple(private=_test_source()).getattr_invalid())
    # Expect no error, currently a false positive.
    _test_sink(Simple(public=_test_source()).getattr_public())
    # Expect no error, currently a false positive.
    _test_sink(Simple(public=_test_source()).getattr_private())
    # Expect no error, currently a false positive.
    _test_sink(Simple(public=_test_source()).getattr_invalid())


def test_bypass_private() -> None:
    _test_sink(Simple(private=_test_source())._Simple__value)  # Error.
    _test_sink(Simple(public=_test_source())._Simple__value)  # No error.
    # pyre-ignore
    _test_sink(Simple(private=_test_source()).__value)  # No error.
    _test_sink(Simple(public=_test_source()).__value)  # No error.


class Other:
    @staticmethod
    def private_into_sink(s: Simple) -> None:
        # Should produce a sink on _Other__value, not _Simple__value.
        # pyre-ignore
        _test_sink(s.__value)


def test_access_from_other_class() -> None:
    Other.private_into_sink(Simple(private=_test_source()))  # No error.


class PrivateAttributeSourceModels:
    def __init__(self):
        # See private_variables.py.pysa
        self.__model_mangled: str = ""
        self.__model_unmangled: str = ""
        self.__model_query: str = ""

    def get_model_mangled(self) -> str:
        return self.__model_mangled

    def get_model_unmangled(self) -> str:
        return self.__model_unmangled

    def get_model_query(self) -> str:
        return self.__model_query


def test_private_attribute_source_models() -> None:
    # Error.
    _test_sink(PrivateAttributeSourceModels().get_model_mangled())
    # TODO(T169448194): Support models on private attributes
    _test_sink(PrivateAttributeSourceModels().get_model_unmangled())
    # Error
    _test_sink(PrivateAttributeSourceModels().get_model_query())


class PrivateAttributeSinkModels:
    def __init__(self):
        # See private_variables.py.pysa
        self.__model_mangled: str = ""
        self.__model_unmangled: str = ""
        self.__model_query: str = ""

    def set_model_mangled(self, value: str) -> None:
        self.__model_mangled = value

    def set_model_unmangled(self, value: str) -> None:
        self.__model_unmangled = value

    def set_model_query(self, value: str) -> None:
        self.__model_query = value


def test_private_attribute_sink_models() -> None:
    # Error.
    PrivateAttributeSinkModels().set_model_mangled(_test_source())
    # TODO(T169448194): Support models on private attributes
    PrivateAttributeSinkModels().set_model_unmangled(_test_source())
    # Error
    PrivateAttributeSinkModels().set_model_query(_test_source())

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source
from typing import Optional, TypeVar

from pyre_extensions import classproperty


class Class:
    def __init__(self):
        self.tainted = ""
        self.untainted = ""

    @property
    def my_property(self) -> str:
        return self.tainted

    def uses_property(self):
        self.tainted = __test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property

    def uses_other(self, other: Optional[Class]):
        if other:
            return other.my_property
        else:
            return None


class Derived(Class):
    def uses_property(self):
        self.tainted = __test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property


class OtherDerived(Class):
    @property
    def my_property(self) -> str:
        return __test_source()

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property


class TaintedGetterAndSetter:
    # This will be the model.
    @property
    def my_property(self) -> str:
        # Ensure that setter taint doesn't pollute the getter, there shouldn't
        # be an issue here.
        __test_sink(self)
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        pass

    def uses_property(self):
        return self.my_property

    # TODO(T52657355): Handle the property write here.
    def writes_to_property(self):
        self.my_property = __test_source()


class DerivedTaintedSetter(TaintedGetterAndSetter):
    @property
    def my_property(self) -> str:
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        __test_sink(value)


class GrandDerived(DerivedTaintedSetter):
    @property
    def my_property(self) -> str:
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        return None


def sets_tainted_value(t: TaintedGetterAndSetter) -> None:
    t.my_property = __test_source()


class SetterMutatesValue:
    def __init__(self) -> None:
        self._p = ""

    @property
    def p(self) -> str:
        return self._p

    @p.setter
    def p(self, value) -> None:
        self._p = value


def setters_are_simulated() -> None:
    x = SetterMutatesValue()
    # Expect no issue
    __test_sink(x.p)
    x.p = __test_source()
    # x.p should now have an issue
    __test_sink(x.p)


class ClassProperty:
    @classproperty
    def my_class_property(cls) -> str:
        return ""


def test_issue_in_class_property():
    __test_sink(ClassProperty.my_class_property)


class Class2:
    @property
    def my_property(self) -> str:
        return ""


T = TypeVar("T", Class, Class2)


def test_type_variable_properties_are_resolved(x: T):
    return x.my_property


class PropertySetterInConstructor:
    def __init__(self, y):
        self.underlying = 0
        self.x = y

    @property
    def x(self) -> int:
        return self.underlying

    @x.setter
    def x(self, x_value) -> None:
        self.underlying = x_value


def property_setter_in_constructor():
    obj = PropertySetterInConstructor(__test_source())
    __test_sink(obj.x)
    __test_sink(obj.underlying)

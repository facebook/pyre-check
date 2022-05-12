# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Optional, TypeVar, Union

from pyre_extensions import classproperty


class Class:
    def __init__(self):
        self.tainted = ""
        self.untainted = ""

    @property
    def my_property(self) -> str:
        return self.tainted

    def uses_property(self):
        self.tainted = _test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = _test_source()
        return self.my_property

    def uses_other(self, other: Optional[Class]):
        if other:
            return other.my_property
        else:
            return None


class Derived(Class):
    def uses_property(self):
        self.tainted = _test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = _test_source()
        return self.my_property


class OtherDerived(Class):
    @property
    def my_property(self) -> str:
        return _test_source()

    def uses_property_but_no_tito_taint(self):
        self.untainted = _test_source()
        return self.my_property


class TaintedGetterAndSetter:
    # This will be the model.
    @property
    def my_property(self) -> str:
        # Ensure that setter taint doesn't pollute the getter, there shouldn't
        # be an issue here.
        _test_sink(self)
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        pass

    def uses_property(self):
        return self.my_property

    def writes_to_property(self):
        self.my_property = _test_source()


class DerivedTaintedSetter(TaintedGetterAndSetter):
    @property
    def my_property(self) -> str:
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        _test_sink(value)


class GrandDerived(DerivedTaintedSetter):
    @property
    def my_property(self) -> str:
        return ""

    @my_property.setter
    def my_property(self, value) -> None:
        return None


def sets_tainted_value(t: TaintedGetterAndSetter) -> None:
    t.my_property = _test_source()


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
    _test_sink(x.p)
    x.p = _test_source()
    # x.p should now have an issue
    _test_sink(x.p)


class ClassProperty:
    @classproperty
    def my_class_property(cls) -> str:
        return ""


def test_issue_in_class_property():
    _test_sink(ClassProperty.my_class_property)


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
    obj = PropertySetterInConstructor(_test_source())
    _test_sink(obj.x)
    _test_sink(obj.underlying)


class PropertyCallableReturn:
    def __init__(self, x: str) -> None:
        self.x = x

    def __call__(self, y):
        _test_sink(self.x)
        _test_sink(y)
        return x


class PropertyCallable:
    def __init__(self, z: str) -> None:
        self.z = z

    @property
    def attribute(self) -> PropertyCallableReturn:
        _test_sink(self.z)
        return PropertyCallableReturn(_test_source())


def test_property_callable():
    obj = PropertyCallable(_test_source())
    return obj.attribute(_test_source())


class RegularAttribute:
    def __init__(self, my_property: str) -> None:
        self.my_property = my_property


def test_union_property_attribute_source():
    obj: Union[TaintedGetterAndSetter, RegularAttribute]
    if 1 > 2:
        obj = TaintedGetterAndSetter()
    else:
        obj = RegularAttribute(_test_source())
    return obj.my_property


def test_union_property_attribute_sink(x):
    obj: Union[TaintedGetterAndSetter, RegularAttribute]
    if 1 > 2:
        obj = TaintedGetterAndSetter()
    else:
        obj = RegularAttribute(x)
    _test_sink(obj.my_property)


def test_getattr_on_property(x: TaintedGetterAndSetter):
    _test_sink(getattr(x, "my_property", ""))


def foo():
    pass


def bar():
    pass


def function_with_nested_properties():
    # Property setters within a function, not a class
    @property
    def my_property(self) -> int:
        foo()
        return 0

    @my_property.setter
    def my_property(self, value) -> None:
        _test_sink(_test_source())
        bar()


class CollidePropertySetterName:
    def foo(self):
        pass

    def bar(self):
        pass

    @property
    def collided_property(self):
        # Please ensure the target name of the inner function does not
        # collide with the property setter of the outer function.
        def setter(value):
            self.foo()

        setter(1)

    @collided_property.setter
    def collided_property(self, value):
        self.bar()

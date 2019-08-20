# flake8: noqa

from typing import List, Optional, Type


class C:
    tainted_attribute: List[int] = []
    tainted_class_attribute: List[int] = []
    not_tainted = 2


class D(C):
    pass


def tainted_attribute_flow_issue(c: C) -> None:
    c.tainted_attribute = __test_source()


def untainted_flow_not_issue(c: C) -> None:
    c.not_tainted = __test_source()


def tainted_attribute_for_class_not_issue(c: Type[C]) -> None:
    c.tainted_attribute = __test_source()


def tainted_attribute_through_inheritance_not_issue(d: D) -> None:
    # TODO(T47337940): Support this.
    d.tainted_attribute = __test_source()


def tainted_class_attribute_through_instance_not_issue(c: C) -> None:
    c.tainted_class_attribute = __test_source()


def tainted_class_attribute_through_class_issue(class_object: Type[C]) -> None:
    class_object.tainted_class_attribute = __test_source()


def tainted_class_attribute_through_double_underscore_class_issue(c: C) -> None:
    c.__class__.tainted_class_attribute = __test_source()


def tainted_class_attribute_through_optional_class_issue(
    class_object: Optional[Type[C]]
) -> None:
    if class_object is not None:
        class_object.tainted_class_attribute = __test_source()


def global_class_attribute_issue() -> None:
    C.tainted_class_attribute = __test_source()

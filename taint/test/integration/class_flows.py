# @nolint

from typing import Type


class C:
    tainted_attribute: List[int] = []
    not_tainted = 2


class D(C):
    pass


def tainted_attribute_flow(c: C) -> None:
    c.tainted_attribute = __test_source()


def untainted_flow(c: C) -> None:
    c.not_tainted = __test_source()


def tainted_attribute_for_class(c: Type[C]) -> None:
    c.tainted_attribute = __test_source()


def tainted_attribute_through_inheritance(d: D) -> None:
    # TODO(T47337940): Support this.
    d.tainted_attribute = __test_source()

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Helper functions for type refinement or asserts.

from typing import Any, Optional, Type, TypeVar

_T = TypeVar("_T")
_TClass = TypeVar("_TClass")


def none_throws(optional: Optional[_T], message: str = "Unexpected `None`") -> _T:
    """Convert an optional to its value. Raises an `AssertionError` if the
    value is `None`"""
    if optional is None:
        raise AssertionError(message)
    return optional


def assert_is_instance(obj: object, cls: Type[_TClass]) -> _TClass:
    """Assert that the given object is an instance of the given class. Raises a
    `TypeError` if not."""
    if not isinstance(obj, cls):
        raise TypeError(f"obj is not an instance of cls: obj={obj} cls={cls}")
    return obj


# pyre-ignore[2]: Intentional `Any` input type.
def safe_cast(new_type: Type[_T], value: Any) -> _T:
    """safe_cast will change the type checker's inference of x if it was
    already a subtype of what we are casting to, and error otherwise."""
    return value

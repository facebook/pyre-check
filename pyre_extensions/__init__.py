# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors
from typing import Any, Optional, Type, TypeVar

# Surfacing `tests` is important for importers to be able to run our tests
# in their own environment
from . import safe_json, tests, type_variable_operators
from .generic import Generic


__all__ = [
    "safe_json",
    "type_variable_operators",
    "tests",
    "Generic",
    "none_throws",
    "assert_is_instance",
    "safe_cast",
    "ParameterSpecification",
    "ListVariadic",
    "TypeVarTuple",
    "Unpack",
]


_T = TypeVar("_T")


def none_throws(optional: Optional[_T], message: str = "Unexpected `None`") -> _T:
    """Convert an optional to its value. Raises an `AssertionError` if the
    value is `None`"""
    if optional is None:
        raise AssertionError(message)
    return optional


TClass = TypeVar("TClass")


def assert_is_instance(obj: object, cls: Type[TClass]) -> TClass:
    """Assert that the given object is an instance of the given class. Raises a
    `TypeError` if not."""
    if not isinstance(obj, cls):
        raise TypeError(f"obj is not an instance of cls: obj={obj} cls={cls}")
    return obj


def safe_cast(new_type: Type[_T], value: Any) -> _T:
    """safe_cast will change the type checker's inference of x if it was
    already a subtype of what we are casting to, and error otherwise."""
    return value


class ParameterSpecification(list):
    """This kind of type variable captures callable parameter specifications
    (known as argspecs in the runtime and inspect library) instead of types,
    allowing the typing of decorators which transform the return type of the
    given callable.
    For example:
        from typing import TypeVar, Callable, List
        from pyre_extensions import ParameterSpecification
        Tparams = ParameterSpecification("Tparams")
        Treturn = TypeVar("Treturn")
        def unwrap(
            f: Callable[Tparams, List[Treturn],
        ) -> Callable[Tparams, Treturn]: ...
        @unwrap
        def foo(x: int, y: str, z: bool = False) -> List[int]:
            return [1, 2, 3]
    decorates foo into a callable that returns int, but still has the same
    parameters, including their names and whether they are required.

    The list inheritance is required for backwards compatibility with the runtime
    implementation for callables, which requires the first argument to be
    a list.

    The args and kwargs properties are used for specifying that a literal definition
    has the same signature as a ParameterSpecification, like:
    def listify(
        f: Callable[TParams, TReturn]
    ) -> Callable[TParams, List[TReturn]]:
        def wrapped( *args: TParams.args, **kwargs: TParams.kwargs):
            return [f(*args, **kwargs)]
    """

    args = object()
    kwargs = object()

    def __init__(self, *args: object, **kwargs: object) -> None:
        pass


def ListVariadic(name) -> object:
    return Any


_A = TypeVar("_A", bound=int)
_B = TypeVar("_B", bound=int)
_Ts = ListVariadic("_Ts")


class Add(Generic[_A, _B], int):
    pass


class Multiply(Generic[_A, _B], int):
    pass


class Divide(Generic[_A, _B], int):
    pass


class Length(Generic[_Ts], int):
    pass


class Product(Generic[_Ts], int):
    pass


class TypeVarTuple:
    def __init__(
        self,
        name: str,
    ) -> None:
        pass


class Unpack(Generic[_T]):
    pass

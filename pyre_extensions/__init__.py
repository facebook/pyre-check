# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors
from typing import Any, Callable, Dict, List, TypeVar, Union

from typing_extensions import Protocol

# Surfacing `tests` is important for importers to be able to run our tests
# in their own environment
from . import safe_json, tests, type_variable_operators
from .generic import Generic
from .refinement import assert_is_instance, none_throws, safe_cast


__all__ = [
    "assert_is_instance",
    "DecoratorFactory",
    "Generic",
    "JSON",
    "none_throws",
    "override",
    "ParameterSpecification",
    "ReadOnly",
    "safe_cast",
    "safe_json",
    "tests",
    "type_variable_operators",
    "TypeVarTuple",
    "Unpack",
]


_T = TypeVar("_T")
_R = TypeVar("_R")
TClass = TypeVar("TClass")

JSON = Union[bool, float, int, str, Dict[str, "JSON"], List["JSON"]]


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


def TypeVarTuple(name: str) -> object:
    return Any


_A = TypeVar("_A", bound=int)
_B = TypeVar("_B", bound=int)
_T1 = TypeVar("_T1")
_T2 = TypeVar("_T2")
_Ts = TypeVarTuple("_Ts")
_P = ParameterSpecification("_P")

T = TypeVar("T", bound=Callable[..., object])


def override(__f: T) -> T:
    """Indicate that a method is intended to override a method in a base class.
    Usage:
        class Base:
            def method(self) -> None: ...
                pass
        class Child(Base):
            @override
            def method(self) -> None:
                super().method()

    When this decorator is applied to a method, the type checker will
    validate that it overrides a method with the same name on a base class.
    This helps prevent bugs that may occur when a base class is changed
    without an equivalent change to a child class.
    There is no runtime checking of these properties. The decorator

    sets the ``__override__`` attribute to ``True`` on the decorated object
    to allow runtime introspection.

    See PEP 698 for details.
    """
    try:
        __f.__override__ = True
    except (AttributeError, TypeError):
        # Skip the attribute silently if it is not writable.
        # AttributeError happens if the object has __slots__ or a
        # read-only property, TypeError if it's a builtin class.
        pass
    return __f


class DecoratorFactory(Protocol):
    """Usable as a return type for simple decorator factories"""

    def __call__(self, __f: Callable[_P, _R]) -> Callable[_P, _R]:
        ...


class Add(Generic[_A, _B], int):
    pass


class Multiply(Generic[_A, _B], int):
    pass


class Subtract(Generic[_A, _B], int):
    pass


class Divide(Generic[_A, _B], int):
    pass


class Length(Generic[_Ts], int):
    pass


class Product(Generic[_Ts], int):
    pass


class Unpack(Generic[_T]):
    pass


class Broadcast(Generic[_T1, _T2]):
    pass


class BroadcastError(Generic[_T1, _T2]):
    pass


class Compose(Generic[_Ts]):
    pass


class ReadOnly(Generic[_T]):
    pass

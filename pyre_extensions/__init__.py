# pyre-ignore-all-errors
from typing import Any, Type, TypeVar

from . import tests, type_variable_operators  # noqa F401


_T = TypeVar("_T")


def none_throws(optional, message: str = "Unexpected `None`"):
    # type: (Optional[_T], str) -> _T
    """Convert an optional to its value. Raises an `AssertionError` if the
    value is `None`"""
    if optional is None:
        raise AssertionError(message)
    return optional


def safe_cast(new_type: Type[_T], value: Any) -> _T:
    """safe_cast will change the type checker's inference of x if it was
    already a subtype of what we are casting to, and error otherwise."""
    return new_type(value)


def ParameterSpecification(name):
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

    The empty list is required for backwards compatibility with the runtime
    implementation for callables, which requires the first argument to be
    a list of types
    """
    return []


def ListVariadic(name):
    return Any

# Pyre Extensions
This module defines extensions to the standard “typing” module that are supported by the [Pyre typechecker](https://pypi.org/project/pyre-check/).

## `none_throws`
Function to make assumptions about `Optional`s explicit. The function will raise an
assertion error if passed `None` and return the value otherwise.

## ParameterSpecification
`ParameterSpecification`s are a special kind of type variable that captures callable parameter
specifications (known as argspecs in the runtime and inspect library) instead of types, allowing
the typing of decorators which transform the return type of the given callable.  
For example:
```
from typing import TypeVar, Callable, List
from pyre_extensions import ParameterSpecification
TParams = ParameterSpecification("TParams")
TReturn = TypeVar("TReturn")
def unwrap(f: Callable[TParams, List[TReturn]]) -> Callable[TParams, TReturn]:
    def inner(*args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
        return f(*args, **kwargs)[0]

    return inner
@unwrap
def foo(x: int, y: str, z: bool = False) -> List[int]:
    return [1, 2, 3]
```
decorates foo into a callable that returns int, but still has the same parameters, including their
names and whether they are required.

These `ParameterSpecification` variables also have two special properties, `args` and `kwargs`,
which correspond to the positional and keyword arguments for a specific call to the
`ParameterSpecification` function.  Because the division of parameters into these two argument
collections can be different each invocation, these special annotations can only be used in one
manner: together, in a function definition, as `*args` and `**kwargs` with no other parameters
listed.

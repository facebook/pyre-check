# Pyre Extensions
This module defines extensions to the standard “typing” module that are supported by the [Pyre typechecker](https://pypi.org/project/pyre-check/).

## ArgSpec 
ArgSpecs are a special kind of type variable that captures callable parameter specifications
(known as argspecs in the runtime) instead of types, allowing the typing of decorators which 
transform the return type of the given callable.  
For example:
```
from typing import TypeVar, Callable, List
from pyre_extensions import ArgSpec
Tparams = ArgSpec("Tparams")
Treturn = TypeVar("Treturn")
def unwrap(f: Callable[Tparams, List[Treturn]) -> Callable[Tparams, Treturn]: ...
@unwrap
def foo(x: int, y: str, z: bool = False) -> List[int]:
    return [1, 2, 3]
```
decorates foo into a callable that returns int, but still has the same parameters, including their 
names and whether they are required.

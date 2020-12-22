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

## Safe JSON
The `safe_json` module provides a type-safe way to parse JSON. It is meant as a drop-in replacement
for the builtin `json` module but instead of returning an object of undefined shape (i.e. `Any`)
allows you to specify the shape of the JSON you're expecting. The parser will validate whether the
input matches the expected type and raise an exception if it does not.

### Examples
For trivial JSON structures you can use builtin types:

```python
>>> from pyre_extensions import safe_json
>>> from typing import List, Dict
>>> safe_json.loads("[1, 2, 3]", List[int])
[1, 2, 3]
>>> safe_json.loads("[1, 2, 3]", List[str])
# Raises `pyre_extensions.safe_json.InvalidJson`
>>> safe_json.loads('{"key": "value"}', Dict[str, str])
{'key': 'value'}
>>> safe_json.loads('{"key": "value"}', Dict[str, int])
# Raises `pyre_extensions.safe_json.InvalidJson`
```

For more complicated, nested structures, typed dictionaries are the way to go:
```python
>>> from typing import TypedDict
>>> class Movie(TypedDict):
...     name: str
...     year: int
...
>>> safe_json.loads('{"name": "Blade Runner", "year": 1982 }', Movie)
{'name': 'Blade Runner', 'year': 1982}
>>> safe_json.loads('{"name": "Blade Runner", "year": "1982" }', Movie)
# Raises `pyre_extensions.safe_json.InvalidJson`
```

Validate if data is expected type:
```python
>>> from pyre_extensions import safe_json
>>> from typing import List, Dict
>>> data = {"foo": 23}
>>> safe_json.validate(data, Dict[str, str])
# Raises `pyre_extensions.safe_json.InvalidJson`
>>> safe_json.validate(data, Dict[str, int])
{"foo": 23}
```

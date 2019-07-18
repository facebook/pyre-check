---
id: querying-pyre
title: Querying Pyre
sidebar_label: Querying Pyre
---

# Overview

Pyre has a subcommand called `query` allows you to hook into a Pyre server and get type-related
information without having to run a full type check. This allows you, for instance, to get the type of an expression at a certain line and column, check whether a type is a subtype or the other or get the list of methods for a class. The rest of this page goes through the various query options with examples.

Note: The responses in the examples are prettified using the `pyre query <query> | python -m json.tool` pattern.

## Attributes

This command gives you the list of attributes for a class.

```
# a.py
class C:
    a: int = 2
    def foo(self) -> str:
        return ""
```
```
$ pyre query "attributes(a.C)"
{
    "response": {
        "attributes": [
            {
                "annotation": "int",
                "name": "a"
            },
            {
                "annotation": "typing.Callable(a.C.foo)[[], str]",
                "name": "foo"
            }
        ]
    }
}
```

## Dump dependency graph

The command `dump_dependencies('filename.py')` dumps the dependency graph rooted at `filename.py` to a file called `dependencies.dot` in the `.pyre` directory.
This file can be processed with `graphviz` to produce an image version of the dependency graph.

## Join

This command uses Pyre's type engine to find a common superclass for two types.

```
$ pyre query "join(typing.Iterable[int], typing.Iterable[str])"
{
    "response": {
        "type": "typing.Iterable[typing.Union[int, str]]"
    }
}
```

## Less or equal

This command returns whether the type on the left can be used when the type on the right is expected.

```
# a.py
class C:
  pass

class D(C):
  pass
```

```
$ pyre query "less_or_equal(a.D, a.C)"
{"response":{"boolean":true}}

$ pyre query "less_or_equal(a.C, a.D)"
{"response":{"boolean":true}}
```

## Meet

This command uses Pyre's type engine to find a common subclass for two types.

```
$ pyre query "meet(typing.Iterable[int], typing.Iterable[typing.Union[int, str]])"
{
    "response": {
        "type": "typing.Iterable[int]"
    }
}
```

## Methods

This command returns the list of methods for a type, excluding inherited ones.

```
# a.py
class C:
  def f(self, x: int) -> str:
    return ""
```

```
$ pyre query "methods(a.C)"
{
    "response": {
        "methods": [
            {
                "name": "foo",
                "parameters": [
                    "self",
                    "int"
                ],
                "return_annotation": "str"
            }
        ]
    }
}
```

## Normalize type

This command resolves type aliases for a given type.

```
# a.py
A = typing.Union[int, str]
B = typing.Union[A, typing.List[str]]
```

```
$ pyre query "normalize_type(a.B)"
{
    "response": {
        "type": "typing.Union[typing.List[int], int, str]"
    }
}
```

## Signature

Returns the type signature of a function.


```
# a.py
def foo(x: int) -> str:
  ...
```

```
$ pyre query "signature(a.foo)"
{
    "response": {
        "signature": [
            {
                "parameters": [
                    {
                        "annotation": "int",
                        "parameter_name": "x"
                    }
                ],
                "return_type": "str"
            }
        ]
    }
}
```

## Superclasses

Displays the superclasses of a given class name.

```
$ pyre query "superclasses(int)"
{
    "response": {
        "superclasses": [
            "float",
            "complex",
            "numbers.Integral",
            "numbers.Rational",
            "numbers.Real",
            "numbers.Complex",
            "numbers.Number",
            "typing.SupportsFloat",
            "typing.Any"
        ]
    }
}
```

## Type

Evaluates the type of the given expression.

```
$ pyre query "type([1 + 2, ''])"
{
    "response": {
        "type": "typing.List[typing.Union[int, str]]"
    }
}
```


## Type at position

This command returns the type of the symbol at the provided position.

```
# a.py
variable = 2
```

```
$ pyre query "type_at_position('a.py', 1, 2)"
{
    "response": {
        "annotation": "int",
        "location": {
            "path": "a.py",
            "start": {
                "column": 0,
                "line": 1
            },
            "stop": {
                "column": 8,
                "line": 1
            }
        }
    }
}
```

## Types in file

This command returns all the types for a file that Pyre has been able to resolve.

```
# a.py
class C:
  attribute = ""
```

```
$ pyre query "types(path='a.py')"
{
    "response": [
        {
            "path": "a.py",
            "types": [
                {
                    "annotation": "str",
                    "location": {
                        "path": "a.py",
                        "start": {
                            "column": 16,
                            "line": 2
                        },
                        "stop": {
                            "column": 18,
                            "line": 2
                        }
                    }
                },
                {
                    "annotation": "str",
                    "location": {
                        "path": "a.py",
                        "start": {
                            "column": 4,
                            "line": 2
                        },
                        "stop": {
                            "column": 13,
                            "line": 2
                        }
                    }
                },
                {
                    "annotation": "typing.Type[a.C]",
                    "location": {
                        "path": "a.py",
                        "start": {
                            "column": 4,
                            "line": 2
                        },
                        "stop": {
                            "column": 13,
                            "line": 2
                        }
                    }
                }
            ]
        }
    ]
}
```

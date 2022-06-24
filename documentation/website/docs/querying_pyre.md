---
id: querying-pyre
title: Querying Pyre
sidebar_label: Querying Pyre
---

Pyre has a subcommand called `query` allows you to hook into a Pyre server and get type-related
information without having to run a full type check.

This allows you, for instance, to get the type of an expression at a certain line and column, check whether a type is a subtype of the other, or get the list of methods for a class.

To get started, set up a server with `pyre` or `pyre start`. The rest of this page goes through the various query options with examples. You can also run `pyre query help` to see a full list of available queries to the Pyre server.

**Note:** The responses in the examples are prettified using the `pyre query <query> | python -m json.tool` pattern.

**IMPORTANT**:
These interfaces are considered legacy code by our team. They are far from production-ready, and will receive minimal maintenance effort in the short to medium term (for Pysa only) and will eventually be removed in the long term. It is ok if you want to rely on them for debugging or manual triaging purpose. But we would **strongly discourage** relying on them to build any automation or product on top.

## Supported Queries
### Attributes

The command `attributes` gives you the list of attributes for a class.

```python
# a.py
class C:
    a: int = 2
    def foo(self) -> str:
        return ""
```
```bash
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

### Callees

The command `callees` returns a list of all calls from a given function, including locations if using `callees_from_location`.

```python
# a.py
def foo() -> None: pass
def bar() -> None:
    foo()
```
```bash
$ pyre query "callees(a.bar)"
{
    "response": {
        "callees": [
            {
                "kind": "function",
                "target": "a.foo"
            }
        ]
    }
}
```
```bash
$ pyre query "callees_with_location(a.bar)"
{
    "response": {
        "callees": [
            {
                "locations": [
                    {
                        "path": "a.py",
                        "start": {
                            "line": 6,
                            "column": 5
                        },
                        "stop": {
                            "line": 6,
                            "column": 8
                        }
                    }
                ],
                "kind": "function",
                "target": "a.foo"
            }
        ]
    }
}
```

### Defines

The command `defines` returns all function and method definitions for a given module or class.

```python
# a.py
class C:
    a: int = 2
    def foo(self) -> str:
        return ""

def bar() -> None: pass
```
```bash
$ pyre query "defines(a.C)"
{
    "response": [
        {
            "name": "a.C.foo",
            "parameters": [
                {
                    "name": "self",
                    "annotation": null
                }
            ],
            "return_annotation": "str"
        }
    ]
}
```
```bash
$ pyre query "defines(a)"
{
    "response": [
        {
            "name": "a.C.foo",
            "parameters": [
                {
                    "name": "self",
                    "annotation": null
                }
            ],
            "return_annotation": "str"
        },
        {
            "name": "a.bar",
            "parameters": [],
            "return_annotation": "None"
        }
    ]
}
```

### Dump call graph

The command `dump_call_graph()` returns a comprehensive JSON mapping each call to a list of callees.

### Dump class hierarchy

The command `dump_class_hierarchy()` returns the entire class hierarchy as Pyre understands it; elides type variables.

### Less or equal

The command `less_or_equal` returns whether the type on the left can be used when the type on the right is expected.

```python
# a.py
class C:
  pass

class D(C):
  pass
```

```bash
$ pyre query "less_or_equal(a.D, a.C)"
{"response":{"boolean":true}}

$ pyre query "less_or_equal(a.C, a.D)"
{"response":{"boolean":true}}
```

### Path of module

The command `path_of_module` returns the full absolute path for a given module.

```bash
$ pyre query "path_of_module(module_name)"
{
    "response": {
        "path": "/Users/user/my_project/module_name.py"
    }
}
```

### Save server state

The command `save_server_state` saves the server's serialized state into the given `path`, which can the be used to start up the identical server without re-analyzing all project files.

```bash
$ pyre query "save_server_state('my_saved_state')"
{
    "response": {
        "message": "Saved state."
    }
}
$ pyre stop
$ pyre --load-initial-state-from my_saved_state start
```

### Superclasses

The command `superclasses` returns the superclasses of given class names.

```bash
$ pyre query "superclasses(int, str)"
{
    "response": [
        {
            "int": [
                "complex",
                "float",
                "numbers.Complex",
                "numbers.Integral",
                "numbers.Number",
                "numbers.Rational",
                "numbers.Real",
                "object",
                "typing.Generic",
                "typing.Protocol",
                "typing.SupportsFloat"
            ]
        },
        {
            "str": [
                "object",
                "typing.Collection",
                "typing.Container",
                "typing.Generic",
                "typing.Iterable",
                "typing.Protocol",
                "typing.Reversible",
                "typing.Sequence"
            ]
        }
    ]
}
```

### Type

The command `type` evaluates the type of the given expression.

```bash
$ pyre query "type([1 + 2, ''])"
{
    "response": {
        "type": "typing.List[typing.Union[int, str]]"
    }
}
```


### Types in file

The command `types` returns all the types for a file that Pyre has been able to resolve. It can be called on multiple files at once with
`types('path1', 'path2', ...)`.

```python
# a.py
class C:
  attribute = ""
```

```bash
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

## API Details

### Location Guidelines
We determine locations for expressions using the following guidelines:
- Ignore leading and trailing whitespace, commas, comments, and wrapping parenthesis.
- Include whitespace, parenthesis or other noop tokens in the locations of compound expressions they are nested inside.
    - Ex. `(a).b` will register two expressions, a at columns 1-2 (still following the guideline above), and `a.b` at columns 0-5
- Similarly, compound expression locations must encompass the locations of all of its components.
    - Ex. `a = b = 1` will register the assignment `a = 1` at columns 0-9, with `a` at columns 0-1 and `1` at columns 8-9
    - The only exception are classes, which do not encompass their decorators
- All semantically meaningful tokens and reserved words are included in the node they define.
    - Ex. `await a` will register the awaitable node at columns 0-7, and the included identifier `a` at columns 6-7
    - Ex. `async def foo(): ...` will register the define node at columns 0-20
    - Ex. `foo(*args, **kwargs)` will register args at columns 4-9 and kwargs at columns 11-19
    - Ex. `"""string"""` will register the string string at columns 0-12
- All implicit values in the AST contribute a length of 0 and point to the closest location to where an equivalent explicit value would live.
    - Ex. `a: int` would register an Ellipsis object at columns 6-6
    - Ex. `a[0]` would register a at columns 0-1 and `a.__getitem__` at columns 0-1
    - Ex. `a[:1]` would register the first argument of slice to be `None` at columns 2-2, the second argument to be `1` at columns 3-4, and the third argument to be `None` at columns 4-4.

### Batching Queries
The `batch` command can be used to run several queries at once and return a map of responses. The list of queries to batch may include any combination of other valid queries except for `batch` itself.

The response for a `batch` command will be a list of responses the same length as the number of queries getting batched, and the order of the responses will match the order of the queries.

```bash
$ pyre query "batch(less_or_equal(int, str), join(int, str))"
{
    "response": [
        {
            "response": {
                "boolean": false
            }
        },
        {
            "response": {
                "type": "typing.Union[int, str]"
            }
        }
    ]
}
```

### Caching
Pyre rechecks each file when queried to generate the location-type mapping, caching results for re-queries of the same file. If you anticipate a large codemod where significant portions of the codebase will be queried, you may increase incremental performance by starting a temporary server with the flag: `pyre --store-type-check-resolution start`.

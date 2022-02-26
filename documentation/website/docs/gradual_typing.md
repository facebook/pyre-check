---
id: types-in-python
title: Types in Python
sidebar_label: Types in Python
---

Python's type system was specified in [PEP 484](https://www.python.org/dev/peps/pep-0484/). If you are new to Python's type system and want to learn the basics, we highly recommend you take a look at [mypy's cheatsheet](https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html) as well as their [type system reference](https://mypy.readthedocs.io/en/stable/builtin_types.html). The following discussion focuses on Pyre's approach to "gradual typing" and how you can get from an untyped codebase to a fully typed codebase.

## Gradual Typing
Most Python code does not (yet) start out typed. PEP 484 specifies a [gradual type system](https://en.wikipedia.org/wiki/Gradual_typing), which is built to allow you to *gradually* add annotations over time. It does so by

- only reporting errors on functions that have an explicit return or parameter type annotation,
- introducing an escape hatch: a special type `Any` that has all possible attributes and is both sub- and super-type of any other type,
- and assuming that all untyped fuctions implicitly return `Any`.

For example,
```python
from typing import List

def unannotated():        # implictly returns `Any`
    return b"" + ""       # function body is not checked

def annotated() -> List:  # explicit return annotation means we type check `annotated`
    any = unannotated()
    any.attribute         # `Any` has all possible attributes
    return 1              # Error: returning `int` but expecting `List`
```

In combination, these rules allow you to slowly annotate code without getting overwhelmed by type errors in one sitting. Incrementally adding more annotations will give you stronger safety and consistency guarantees in your codebase.

In the example above, if you changed `unannotated` to return `str`, you would get a type error when accessing the attribute `any.attribute` in `annotated`.

## Strict Mode
While `Any` is a necessary escape hatch when annotating large codebases over time, it can hide legitimate type errors. We've introduced *strict mode* in Pyre to address this problem. Strict mode can be toggled at a module level by introducing a `# pyre-strict` comment to the file. In strict mode, Pyre will

- run on all functions, whether they are annotated or not,
- error on functions, globals, or attributes that are missing annotations,
- and error on annotations containing `Any` (with some exceptions to accommodate for common patterns).

In our previous example,
```python
# pyre-strict
from typing import List

def unannotated():        # Error: missing return annotation
    return b"" + ""       # Error: function body *is* checked

def annotated() -> List:  # Error: implicit `Any` for generic parameter to `List`
    any = unannotated()
    any.attribute         # Note: the type of `any` is still any.
    return 1              # Error: returning `int` but expecting `List`
```

As you can see in the example, `Any` can still sneak into modules that are strict, but increasing strict coverage and fixing the surfaced errors will gradually eliminate them.

### Strict-By-Default
Strict mode can also be set as the default in a [project configuration](configuration.md). To opt individual files out of strict mode, use `# pyre-unsafe` in place of `# pyre-strict`.

## When Source Code is not Available
We do not always have access to all the source code that contributes type information to our project: e.g. `builtins` is compiled native code, and other libraries may be using *cython*. Other times, we may be working with Python code that is just too dynamic to be reasonably typed.

To address these cases, Pyre will give precedence to type *stub files* with a `*.pyi` extension over source files when these are specified in the search path in the [project configuration](configuration.md) or if they are located next to the implementation file.
Stub files have the same structure as implementation files but only contain class and function signatures:

```python
# my_dynamic_module.pyi
def dynamic_function() -> int: ...   # Function body is omitted
```

If a `__getattr__` function is defined in the stub file as follows, Pyre will take it as a signal that the stub file is partially complete: accessing attributes whose name is not defined in the stub file will result in `Any` instead of a type error.

```python
# my_stub.pyi
from typing import Any
foo: int = 42
# Parameter needs to be typed as `str` and return type needs to be `Any`
def __getattr__(name: str) -> Any: ...

# my_source.py
import my_stub
reveal_type(my_stub.foo)        # Reveals `int`
reveal_type(my_stub.undefined)  # Reveals `Any`
```

## Strategies for Increasing Coverage
Pyre comes with tooling to make it easy to increase type coverage in your project.

### Upgrade
When upgrading the type checker, new errors inevitably get surfaced. In order to keep a codebase clean through upgrades we've built `pyre-upgrade`, which automatically [suppresses](errors.md#suppression) newly surfaced type errors. It takes Pyre's output and adds supression comments to the code explaining what's wrong so that developers can easily address the issues individually.

You can run `pyre-upgrade` with
```bash
(venv) $ pyre --output=json | pyre-upgrade fixme
```
or if you are using a local configuration
```bash
(venv) $ pyre --output=json -l <project> | pyre-upgrade fixme
```

### Automatic Type Inference
We have found tools that automatically add type annotations to code useful to get started with a project. There are two general approaches to automatic type inference: static inference and dynamic inference from runtime information. Both approaches come with their own trade-offs and we have found a combination of the two to be useful.

Pyre can do static type inference. You can run
```bash
(venv) $ cd <path to project>; pyre infer -i
```
to automatically apply annotations.

For dynamic inference we recommend you give [MonkeyType](https://github.com/Instagram/MonkeyType) a try.

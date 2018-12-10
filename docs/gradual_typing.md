---
id: gradual-typing
title: Gradual Typing
sidebar_label: Gradual Typing
---

The term gradual typing describes a type system where not every expression is typed.
Pyre allows users to explicitly specify how strict the type checking should be on a per-file basis.

By default, untyped expressions are assumed to be type Any, which is defined as having
every attribute and being a sub- and super-type of every other known type.
This is useful for annotating functions relying on untyped library code:

```
  def foo() -> int:
    return unannotated_library_function() # has type `Any`, which is a subtype of `int`
```

There will be no type error in the above code, so we can annotate our function without
having to edit the library function we're using.
This can be desirable or necessary, but also gives us very weak guarantees about our code.

## Strict Mode

Pyre supports a strict mode where the above code will no longer type check.
Strict mode can be enabled by annotating a file with `# pyre-strict`.

```
  # pyre-strict
  def foo() -> int:
    return unannotated_library_function()  # type error: expected `int` but got `Any`
```
In case the callee is not under our control we can either suppress the error
(`# pyre-ignore`) or create stubs for the library function.
Pyre uses [typeshed](https://github.com/python/typeshed), a collection of stubs
for the standard library originally built to support MyPy.
Pyre also supports custom, repository local stubs.

## Declarative Mode

In case we're in control of the library function we can now go in and annotate that function too

```
  def unannotated_library_function() -> int:
    return sometimes_returns_a_string(return_string=False)  # uh oh...
```
This example is contrived but not unrealistic. Especially when transitioning from a highly dynamic
codebase to a statically typed one this happens quite frequently.
We could go in, refactor and clean up sometimes_returns_a_string to behave more consistently
but more likely than not this will just put us in the same situation over and over again.

Pyre solves this problem with a per-file declarative mode:
```
  # pyre-ignore-all-errors
  def unannotated_library_function() -> int:
    return sometimes_returns_a_string(return_string=False)  # this is fine
```
This tells Pyre to take all annotations at face value (so dependencies type check),
but allows us to deal with internal type correctness later.

## Suppressing Specific Errors

In some cases we may have a file that will almost meet type-checking save for a few specific kinds
of dynamic behavior.
If we have a file with several of these above kind of incompatible return type errors, we could
suppress all of them, but no other kinds of errors with:

```
  # pyre-ignore-all-errors[7]

  def unannotated_library_function_A() -> int:
    return sometimes_returns_a_string(return_string=False)  # this is fine

  def unannotated_library_function_B() -> str:
    return sometimes_returns_a_string(return_string=True)  # this is also fine (in the same file)
```

This gives you access to as much static typing as you can handle at a given point.

However, this should be a stopgap solution, and not a permanent fix.  Suppressing an entire class
of errors is still pretty dangerous, and over time you should try to converge to full compliance.

## Stubs

You might have some highly dynamic code that Pyre is having trouble analyzing and
getting accurate annotations for - say, `file.py`.
In this case, you can add a stub `file.pyi` in the same directory that contains
the annotations for classes & functions, and omit the implementations.

```
  # file.py
  from memoizer import memoize

  class C:
   cached_fetch = None

  def fetch(self, id):
    return get_from_network(id)

  C.cached_fetch = memoize(fetch)

  # file.pyi
  class C:
    def cached_fetch(self, id: str) -> HttpResponse: ...
```
Here, Pyre will ignore the contents of `file.py`, and won't know about the existence
of `fetch` and a function that takes an unexpected self argument,
instead only seeing the intended signatures.

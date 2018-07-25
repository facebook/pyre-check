---
id: error-types
title: Pyre Errors
sidebar_label: Error Types
---

Elaboration on some categories of errors thrown by Pyre.

## Pyre Error [9]: Incompatible Variable Type

Pyre will error when assigning incompatible types to local variables and parameters that were explicitly annotated.

That is, the following will error:

```
def f(x: int) -> None:
  x = "" # Incompatible variable type error
  y: int = 1
  y = "" # Incompatible variable type error
```

The rationale here is that it's surprising for an explicitly annotated variable to have an
incompatible type later on in the same function.

If you intended to change the type of the variable, you can explicitly annotate it with the new type:

```
def f(x: int) -> None:
  x: str = "" # No errors!
  y: int = 1
  y: str = "" # No errors!
```


## Pyre Errors [14/15]: Behavioral Subtyping

Method overrides should follow
[Liskov's substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle).
In short, parameter types can't be more restrictive and return types
can't be more permissive in overridden methods. To see why, consider the following example:

```
  def width(image: Image) -> float:
    return image.width()
```

Say we now have different implementations of our `Image` class, one of which
violates the substitution principle:

```
  class Image:
    @abstractmethod:
    def width() -> float: pass

  class JpegImage(Image):
    @override
    def width() -> int: return 10  # this is fine

  class ComplexImage(Image):
    @override
    def width() -> complex: return 1j
```

Clearly our `width` function above breaks when used with a `ComplexImage` instance.
The case for parameters follows analogously.

## Pyre Errors [16]: Missing Attributes

Your code is most likely trying to access an attribute that Pyre does not know about.
Pyre has various ways of inferring what is an attribute of an object:

### Explicitly Declared
```
  class Derp:
    attribute: int = 1

    @property
    def property(self) -> int: ...
```

### Implicitly Declared
```
  class Derp:
    def __init__(self):
       self.attribute: int = 1
```
Pyre does one level of inlining to infer implicit parameters
We suggest you do not heavily rely on this feature as it is not sound and makes our code brittle.
Support for this is temporary.

## Pyre Errors [18/21]: Undefined Name, Undefined Import

Error 18 ("Undefined name") is raised when your code tries to access a variable or function that Pyre could not resolve.
This is usually caused by failing to import the proper module.

```
  # 'import some_module' is missing
  some_module.some_func()
```

Pyre will raise error 21 instead ("Undefined import") when the import statement is present, but the module to be imported could not be found in the search path.
If the module provides stub files, please provide their location via the `--search-path` commandline parameter.

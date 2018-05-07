---
id: error-types
title: Pyre Errors
sidebar_label: Error Types
---

Elaboration on some categories of errors thrown by Pyre.

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

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


## List and Dictionary Mismatches With Subclassing

Pyre will error when, for instance, a `List[int]` is passed in when a `List[float]` is expected, as in the following example:

```
def to_seconds(milliseconds: List[float]) -> List[int]:
  return [int(x/1000.0) for x in milliseconds]

my_list: List[int] = [1]
my_list = to_seconds(my_list) # Pyre errors here!
```

This code is fine at runtime, so it might be surprising that Pyre errors here. However, consider the following code:

```
def halve_first_element(list: List[float]) -> None:
  list[0] /= 2

my_list: List[int] = [1]
halve_first_element(my_list)
function_taking_int(my_list[0]) # Oh no, my_list[0] is 0.5!
```

If we allowed passing in `my_list` to the `halve_first_element` function here, the above code would type check. It's perfectly valid from the perspective of the callee to modify the list's element to be a float, as it was annotated as taking a list of floats, but because this list escapes the scope of the callee, we can't allow this in the type checker.

To work around this, we can signal to the type checker that the parameter can't be modified. Here's how you can tell the type checker that you won't change the container in your function:

```
# I can't modify milliseconds here, so it's safe to pass a Iterable[int].
def to_seconds(milliseconds: Iterable[float]) -> List[int]:
  return [int(x/1000.0) for x in milliseconds]

my_list: List[int] = [1]
my_list = to_seconds(my_list) # Type checks!
```

`typing.Iterable` is an immutable variant for lists that allows accessing the list without modifying it. Most commonly used generic containers have immutable variants, and I would encourage you to use them for function parameters whenever you don't need to modify a container in your function.
Here are some immutable variants for commonly used containers:

```
typing.List → typing.Sequence (if you need random access via my_list[id])
typing.List → typing.Iterable (if you're just iterating over the list in a loop and want to support sets as well)
typing.Dict → typing.Mapping
typing.Set → typing.AbstractSet
```

Invariance, combined with type inference, comes with a few gotchas. When you write an expression, Pyre infers the most precise type possible. For instance, Pyre infers the `List[int]` type for `[1, 2]`, even though `List[float]` would be a perfectly valid type here. This can cause issues, as in the following example:

```
def zeroes(number_of_elements: int) -> List[float]:
  a = [0] * number_of_elements
  return a # Pyre errors here!
```

What happened above is that Pyre inferred a type of `List[int]` for a, and invariance kicked in. You can work around this by adding an explicit annotation when declaring a:

```
def zeroes(number_of_elements: int) -> List[float]:
  a: List[float] = [0] * number_of_elements
  return a # Type checks!
```

## [35]: Invalid type variance

In brief, read-only data types can be covariant, write-only data types can be contravariant, and data types that support both reads and writes must be invariant.
If a data type implements any functions accepting parameters of that type, we cannot guarantee that writes are not happening. If a data type implements any functions returning values of that type, we cannot guarantee that reads are not happening.
For example (note: int is a subclass of float in the type system and in these examples):
Writes taking covariants:

```
_T_co = typing.TypeVar("_T_co", covariant=True)

class MyList(typing.Generic[_T_co]):
    def write(self, element: _T_co) -> None:
        ... # adds element to list

def takes_float_list(float_list: MyList[float]) -> None:
    float_list.write(1.0)

int_list: MyList[int] = ...
takes_float_list(int_list)  # this call is OK because MyList is covariant: MyList[int] < MyList[float]
# int_list contains floats
```

Reads returning contravariants:

```
_T_cont = typing.TypeVar("_T_cont", contravariant=True)

class MyList(typing.Generic[_T_cont]):
    def read(self) -> _T_cont:
        ... # returns first element from list

def takes_int_list(int_list: MyList[int]) -> int:
   return int_list.read()

float_list: MyList[float] = ...
takes_int_list(float_list)  # this call is OK because MyList is contravariant: MyList[float] < MyList[int]
# problem with return above is clear
```

## Pyre Error \[56\]: Invalid decoration

This error code is a catch-all for a variety of problems that can arise in the course of resolving the type of a decorated function.
In all of these cases, these decoration failures will lead to the function being registered with type `Any` to avoid any spurious downstream errors.

### "Pyre was not able to infer the type of the decorator ..."

This should only happen when the decorator access itself is invalid, e.g. when you use a decorator which isn't declared in the stubs for a third-party library.

### "Pyre was not able to infer the type of argument ..."

When using the "decorator factory" pattern, we need to resolve the type of both the decorator factory itself as well as the arguments passed to the decorator factory.
This is because the types of these arguments can alter the behavior of the returned decorator via overloads or type variables.
However, this resolution has to happen early in the environment-building pipeline, when we don't yet have all of the context we need in order to resolve the types of arbitrary expressions.
We support resolving literals and simple globals as arguments, but using anything else will result in this error.

To work around this, you can statically type your arguments to the decorator factory as separate globals, which can be validated later in the type-checking pipeline.

```python
T = TypeVar("T")
def decorator_factory(x: T) -> Callable[[Callable[[int], str]], Callable[[str], T]]:
  ...
# pyre-fixme[56]: Pyre was not able to infer the type of argument
#  `complex_expression()` to decorator factory `decorator_factory`.
@decorator_factory(complex_expression())
def foo(x: int) -> str:
  ...


argument: float = complex_expression()

@decorator_factory(argument) # Accepted!  bar resolves to Callable[[str], float]
def bar(x: int) -> str:
  ...
```

### "Decorator factory \`X\` could not be called"

This corresponds to when the decorator factory access resolves to a type that is not callable (i.e. has no `__call__` method).

```python
not_a_factory: int = 5

# pyre-fixme[56]: Decorator factory `not_a_factory` could not be called, because its 
# type `int` is not callable
@not_a_factory(1)
def bar() -> None:
  pass
```

### "Decorator \`X\` could not be called"

Similarly, these errors correspond to when the entire decorator expression (potentially including arguments to a decorator factory), resolves to a non-callable type.


```python
def foo() -> int:
  return 42

# pyre-fixme[56]: Decorator `foo()` could not be called, because its 
# type `int` is not callable
@foo()
def bar() -> None:
  pass
```

### "While applying decorator factory ..." 

These errors are emitted from attempting to pass the resolved factory arguments to the factory, as with any other function call.

```python
def factory(x: str) -> Callable[[object], object]: 
  ...

# pyre-fixme[56]: While applying decorator factory `factory`: 
# Expected `str` for 1st param but got `int`.
@factory(1)
def foo() -> None:
  pass
```

### "While applying decorator ..."

Correspondingly, these errors are emitted from trying to pass the decorated function as an argument to the resolved decorator type.

```python
def decorator(f: Callable[[int], str]) -> int:
  ...

# pyre-fixme[56]: While applying decorator `decorator`:
# Expected `Callable[[int], str]` for 1st param but got `Callable[[str], int]`.
@decorator
def foo(x: str) -> int:
  return 5
```


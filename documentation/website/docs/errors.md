---
id: errors
title: Errors
sidebar_label: Errors
---

import Internal from './fb/errors.md';

## Common Issues
### Covariance and Contravariance
[Variance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)) is tricky and a common source of confusion for people new to Python's type system.

Pyre will error when, for instance, a `List[int]` is passed in when a `List[float]` is expected, as in the following example:

```python
def to_seconds(milliseconds: List[float]) -> List[int]:
  return [int(x/1000.0) for x in milliseconds]

my_list: List[int] = [1]
my_list = to_seconds(my_list) # Pyre errors here!
```

This code is works perfectly fine at runtime, and we may think that since `int` is a subtype of `float` this should not be a problem for the type checker either. However, consider the following code:

```python
def halve_first_element(list: List[float]) -> None:
  list[0] /= 2

my_list: List[int] = [1]
halve_first_element(my_list)
function_taking_int(my_list[0]) # Oh no, my_list[0] is 0.5!
```

If we allowed passing in `my_list` to the `halve_first_element` function here, the above code would type check. It's perfectly valid from the perspective of the callee to modify the list's element to be a float, as it was annotated as taking a list of floats, but because this list escapes the scope of the callee, we can't allow this in the type checker.

To work around this, we can signal to the type checker that the parameter can't be modified. Here's how you can tell the type checker that you won't change the container in your function:

```python
# I can't modify milliseconds here, so it's safe to pass a Iterable[int].
def to_seconds(milliseconds: Iterable[float]) -> List[int]:
  return [int(x/1000.0) for x in milliseconds]

my_list: List[int] = [1]
my_list = to_seconds(my_list) # Type checks!
```

`typing.Iterable` is an immutable variant for lists that allows accessing the list without modifying it. Most commonly used generic containers have immutable variants, and I would encourage you to use them for function parameters whenever you don't need to modify a container in your function.
Here are some immutable variants for commonly used containers:

```python
typing.List → typing.Sequence (if you need random access via my_list[id])
typing.List → typing.Iterable (if you're just iterating over the list in a loop and want to support sets as well)
typing.Dict → typing.Mapping
typing.Set → typing.AbstractSet
```

Invariance, combined with type inference, comes with a few gotchas. When you write an expression, Pyre infers the most precise type possible. For instance, Pyre infers the `List[int]` type for `[1, 2]`, even though `List[float]` would be a perfectly valid type here. This can cause issues, as in the following example:

```python
def zeroes(number_of_elements: int) -> List[float]:
  a = [0] * number_of_elements
  return a # Pyre errors here!
```

What happened above is that Pyre inferred a type of `List[int]` for a, and invariance kicked in. You can work around this by adding an explicit annotation when declaring a:

```python
def zeroes(number_of_elements: int) -> List[float]:
  a: List[float] = [0] * number_of_elements
  return a # Type checks!
```

### Optional Attributes
A common pattern in Python is to check whether an attribute is `None` before accessing its value. E.g.

```python
from typing import Optional

class Data:
  field: Optional[int]

def process_field(input: int) -> None:
  ...

def process_data(data: Data) -> None:
  if data.field:
    # ...
    process_field(data.field)  # Error: expected `int` but got `Optional[int]`
```

The above fails to type-check because Pyre cannot guarantee that `data.field` is not `None` even after checking explicitly in the line before: `field` could be set to `None` by another thread or it could be a property that returns something different the next time we access it.

The preferred way to make this code type-check is to mark the attribute `Final`, i.e. to specify that it can't be reassigned.

```python
from typing import Final, Optional

class Data:
  # Needs to be assigned in the constructor and cannot be changed afterwards.
  field: Final[Optional[int]] = 1
```

It is always safe to refine attributes when their types are `Final`.

Alternatively, it is also safe to assign the attribute to a local variable before accessing its value.

```python
def process_data(data: Data) -> None:
  field = data.field
  if field:
    process_field(field)
```

### Third-Party Libraries
Not all third-party libraries come with Python code that Pyre can analyze (e.g. `cython` modules), and some libraries contain source code without annotations. This will often show up in the form of undefined attribute errors:

```
Undefined attribute [16]: Module <library> has no attribute <some attribute>.
```

Since it is not always possible to annotate code, PEP 484 specifies a format for [stub files](https://www.python.org/dev/peps/pep-0484/#stub-files) with a `.pyi` extension. Pyre will look for stub files in [typeshed](https://github.com/python/typeshed), or next to your source code. You can also provide additional paths to Pyre to look for stubs (see [Configuration](configuration.md)).

<Internal />


## Error Codes
Different errors raised by Pyre have different error codes. E.g. in

```bash
(venv) $ pyre
 ƛ Found 1 type error!
test.py:1:0 Incompatible variable type [9]: a is declared to have type `int` but is used as type `str`.
```

The `9` in the brackets indicates that we raised an error with code 9.

### 3: Missing Return Annotation

If strict mode is turned on, Pyre will error when a function is either annotated with a return type that contains `typing.Any`, or is not annotated with any return type at all (in which case Pyre will treat it as returning `typing.Any` by default).

This is bad because a return type of `typing.Any` may potentially hiding legitimate type errors that may happen at runtime:

```python
from typing import Any

def f():
  return 42

# This line will raise at runtime, but no type error here since `f()` has type `Any`.
print("a" + f())
```

The best way to silence this error is to add non-`Any` return annotation to every function.

### 6: Incompatible Parameter Type
Pyre will error if an argument passed into a function call does not match the expected parameter type of that function.

```python
def takes_int(x: int) -> None:
  pass
def f(x: Optional[int]) -> None:
  takes_int(x) # Incompatible parameter type error
```

If you are seeing errors with invariant containers where some `Container[T]` is expected but you are passing `Container[S]` where `S < T`, please see [Covariance and Contravariance](errors#covariance-and-contravariance).

### 9: Incompatible Variable Type
Pyre will error when assigning incompatible types to local variables and parameters that were explicitly annotated.

That is, the following will error:

```python
def f(x: int) -> None:
  x = "" # Incompatible variable type error
  y: int = 1
  y = "" # Incompatible variable type error
```

The rationale here is that it's surprising for an explicitly annotated variable to have an
incompatible type later on in the same function.

If you are constructing an object that is generic over an invariant type, you may run into an error:

```python
_T = TypeVar('_T')

class Foo(Generic[_T]):
    def __init__(self, x: _T) -> None: ...

def f() -> None:
    foo: Foo[Optional[int]] = Foo(x=1) # Incompatible variable type error
```

This is due to the fact that `Foo[X]` is not less than `Foo[Y]` even if `X < Y` when the type variable is invariant.
You can declare your intention to initialize the object with a wider type than is given to fix this error:

```python
def f() -> None:
    foo: Foo[Optional[int]] = Foo[Optional[int]](x=1)
```

### 14,15: Behavioral Subtyping
Method overrides should follow
[Liskov's substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle).
In short, parameter types can't be more restrictive and return types
can't be more permissive in overridden methods. To see why, consider the following example:

```python
  def width(image: Image) -> float:
    return image.width()
```

Say we now have different implementations of our `Image` class, one of which
violates the substitution principle:

```python
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

### 16: Missing Attributes

Your code is most likely trying to access an attribute that Pyre does not know about.
Pyre has various ways of inferring what is an attribute of an object:

#### Explicitly Declared
```python
  class Derp:
    attribute: int = 1

    @property
    def property(self) -> int: ...
```

#### Implicitly Declared
```python
  class Derp:
    def __init__(self):
       self.attribute: int = 1
```
Pyre does one level of inlining to infer implicit parameters
We suggest you do not heavily rely on this feature as it is not sound and makes our code brittle.
Support for this is temporary.

### 18,21: Undefined Name, Undefined Import
Error 18 ("Undefined name") is raised when your code tries to access a variable or function that Pyre could not resolve.
This is usually caused by failing to import the proper module.

```python
  # 'import some_module' is missing
  some_module.some_func()
```

Pyre will raise error 21 instead ("Undefined import") when the import statement is present, but the module to be imported could not be found in the search path.
If the module provides stub files, please provide their location via the `--search-path` commandline parameter.

### 34: Invalid type variable

Type variables can only be used as types when they have already been placed "in scope".
A type variable can be placed into scope via:

* Generic class declarations
  * for example, `class C(Generic[T]):` puts `T` into scope for the body of the class
* The **parameter** types of a generic function
  * for example, `def foo(x: T)` puts `T` into scope for the body and return type annotation of the function

Something notably absent from this list is "inside of a `typing.Callable` type".
This means that `Callable[[T], T]` does not spell the type of a generic function, but rather a specific identity function, with the `T` defined by an outer scope.
Therefore, if you want to spell the signature of a function that takes/returns a generic function, you will need to declare it separately via a callback protocol:

```python
T = TypeVar("T")

def returns_identity() -> Callable[[T], T]: ... # Rejected

class IdentityFunction(Protocol):
  def __call__(self, x: T) -> T: ...

def returns_identity() -> IdentityFunction: # Accepted
  def inner(x: T) -> T:
    return x
  return inner
```


### 35: Invalid type variance
In brief, read-only data types can be covariant, write-only data types can be contravariant, and data types that support both reads and writes must be invariant.
If a data type implements any functions accepting parameters of that type, we cannot guarantee that writes are not happening. If a data type implements any functions returning values of that type, we cannot guarantee that reads are not happening.
For example (note: int is a subclass of float in the type system and in these examples):
Writes taking covariants:

```python
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

```python
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

### 53: Missing annotation for captured variables
Pyre makes no attempt at trying to infer the types across function boundaries. The statement holds for nested functions as well.
From a nested function's perspective, a variable defined in an nesting function behaves not too differently from a global variable. Therefore, Pyre treats such variables in the same way as it treats global variable: an explicit annotation is required if strict mode is turned on.

```python
def outer_function0() -> int:
    x = foo()
    def inner_function() -> int:
        return x  # Due to the lack of explicit annotation, Pyre will treat this variable as having type `Any`.
    return inner_function()

def outer_function1() -> int:
    x: int = foo()
    def inner_function() -> int:
        return x  # This is ok: the type of `x` is known to be `int`.
    return inner_function()

def outer_function2() -> int:
    x = foo()
    def inner_function(x: int) -> int:
        return x  # This is also ok: even though the outer `x` is not annotated, the `x` parameter of the inner function is.
    return inner_function(x)
```

### 56: Invalid decoration

This error code is a catch-all for a variety of problems that can arise in the course of resolving the type of a decorated function.
In all of these cases, these decoration failures will lead to the function being registered with type `Any` to avoid any spurious downstream errors.

#### "Pyre was not able to infer the type of the decorator ..."

This should only happen when the decorator access itself is invalid, e.g. when you use a decorator which isn't declared in the stubs for a third-party library.

#### "Pyre was not able to infer the type of argument ..."

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

#### "Decorator factory \`X\` could not be called"

This corresponds to when the decorator factory access resolves to a type that is not callable (i.e. has no `__call__` method).

```python
not_a_factory: int = 5

# pyre-fixme[56]: Decorator factory `not_a_factory` could not be called, because its
# type `int` is not callable
@not_a_factory(1)
def bar() -> None:
  pass
```

#### "Decorator \`X\` could not be called"

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

#### "While applying decorator factory ..."

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

#### "While applying decorator ..."

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

## Suppression
It is not always possible to address all errors immediately – some code is too dynamic and should be refactored, other times it's *just not the right time* to deal with a type error. We do encourage people to keep their type check results clean at all times and provide mechanisms to suppress errors that cannot be immediately fixed.

### Suppressing Individual Errors
Pyre supports error suppression of individual errors with comments that can be placed on the line of the error or on the line preceeding the error.

- `# pyre-fixme` indicates there is an issue in the code that will be revisited later.
- `# pyre-ignore` indicates there's an issue with the type checker or the code is too dynamic and we have decided to not fix this. If this is a Pyre bug, make sure you [open an issue](https://github.com/facebook/pyre/issues) on our tracker.

Both comment styles allow you to suppress individual error codes as well as adding additional context.

```python
def foo() -> int:
    # pyre-fixme[7]: only suppresses return mismatches
    return ""
```

Pyre also supports `# type: ignore` comments for backwards-compatibility with *mypy*.

### Suppressing All Errors
You can use the [Pyre upgrade tool](types-in-python#upgrade) to add inline error suppressions for all errors in your project.

### Suppressing Errors Across Files
You can suppress all errors in entire sections of your code by adding the path to the [`ignore_all_errors` section of your configuration](configuration#global).

Furthermore Pyre supports suppressing all errors in an individual file if you add a `# pyre-ignore-all-errors` to your file. Like the other suppression comments, you can use square brackets to chose to only ignore one or more particular error types.

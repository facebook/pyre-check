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


### 0: Unused Ignore
Pyre fixmes and ignores allow you to ignore specific type errors by their code until you are able to fix them. In order to avoid outdated fixme comments in your project, Pyre will also error when a fixme is no longer needed. Removing the fixme comment will resolve the error.

```python
# pyre-fixme[7] # unused ignore
def foo() -> int:
  return 1
```

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

### 4: Missing Attribute Annotation
In strict mode, Pyre will error when an attribute does not have an annotation.

```python
class A:
    b = foo() # Missing attribute annotation
```
Adding a type annotation will resolve this error.

```python
class A:
  b: int = foo()
```

### 5: Missing Global Annotation

If strict mode is turned on, Pyre will error when a globally accessible variable is not annotated. If pyre was able to infer a type for the
variable, it will emit this type in the error message. The fix is usually to add an annotation to the variable.

Note: This error has also arisen when there is some ambiguity of whether a declaration is a global expression or a type alias, in these cases pyre assumes it is an expression. Adding a `: TypeAlias` annotation lets pyre know that it is a type alias and solves the problem.

```python
from typing_extensions import TypeAlias

# This declaration would result in an error
MyTypeAlias = Dict[str, "AnotherTypeAlias"]

# This declaration ensures that pyre knows MyTypeAlias is a type alias
MyTypeAlias: TypeAlias = Dict[str, "AnotherTypeAlias"]
```

### 6: Incompatible Parameter Type
Pyre will error if an argument passed into a function call does not match the expected parameter type of that function.

```python
def takes_int(x: int) -> None:
  pass
def f(x: Optional[int]) -> None:
  takes_int(x) # Incompatible parameter type error
```

If you are seeing errors with invariant containers where some `Container[T]` is expected but you are passing `Container[S]` where `S < T`, please see [Covariance and Contravariance](errors#covariance-and-contravariance).

### 7: Incompatible Return Type
Pyre will error when the value returned from a function does not match the annotation.

```python
def foo() -> int:
  return "" # incompatible return type
```
Updating the return annotation, or the value returned from the function will resolve this error.

### 8: Incompatible Attribute Type
Pyre will error if a value is assigned to an attribute that does not match the annotated type of that attribute.

```python
class Foo:
  x: int = 0

def f(foo: Foo) -> None:
  foo.x = "abc" # Incompatible attribute type error
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

### 11, 31: Undefined or Invalid Type
Pyre recognizes class names as valid annotations. Most basic types are imported from the `typing` module or are already available from builtins like `str`, `int`, `bool`, etc. You can also define your own type alias on the global scope, which can be used as annotations:

```python
from typing_extensions import TypeAlias

INT_OR_STR: TypeAlias = Union[int, str]
```

If you use a name as an annotation that is not a valid type or valid alias, you will see this error:

```python
GLOBAL_VALUE = "string"

def f0() -> GLOBAL_VALUE: ... # Error! `GLOBAL_VALUE` is a value, not a type.

def f1() -> type(GLOBAL_VALUE): ...   # Error! Static type annotations cannot be dynamically computed.

def f2() -> [int]: ...  # Error! `[int]` is not a valid type. If you mean a list of int, use `typing.List[int]`.

def f3() -> (int, str): ...  # Error! `(int, str)` is not a valid type. If you mean a pair of int and str, use `typing.Tuple[int, str]`.

from typing import Callable
def f4() -> Callable[[int]]: ...  # Error! `Callable[[int]]` is not a valid type because the return type of the callable is missing. Good example: `Callable[[int], int]`.

def f5() -> Callable[int, int]: ...  # Error! `Callable[int, int]` is not a valid type. The parameter types of the callable must be enclosed in square brackets. Good example: `Callable[[int], int]`.

from typing_extensions import Final
def f6() -> List[Final[int]]: ...  # Error! `Final` may only be used as the outermost type in annotations. See PEP 591.

from typing_extensions import Literal
def f7() -> Literal[GLOBAL_VALUE]: ...  # Error! Only literals are allowed as parameters for `Literal`. See PEP586. Good example: `Literal[42]` or `Literal["string"]`.
```

You can fix this error by verifying that your annotation is

1. statically determined.
2. properly imported from `typing` if applicable.
3. properly defined in the module you are importing from. If the module you are importing from has a [stub file](#third-party-libraries), you should check the definition there.
4. properly adhere to the additional rules of special types (e.g. `Callable`, `Final`, and `Literal`).

For type aliases, check that your type alias is defined

1. with a valid type on the RHS. If you provide an annotation for the TypeAlias assignment, it must be `typing_extensions.TypeAlias`.
2. on the global scope, not nested inside a function or class.


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

def foo() -> None:
    image: Image = ComplexImage()
    print(int(image.width()))
```

The above code fails at runtime with `TypeError: can't convert complex to int`. The case for parameters follows analogously.

#### Common Reasons

+ `Could not find parameter y in overriding signature.`: Check if the overriding function can accept all arguments that the overridden function can.

  ```python
  class Base:
      def foo(self, x: int, y: str) -> None:
          pass

  class Child(Base):
      def foo(self, x: int) -> None:
          pass
  ```

+ `Type Foo is not a subtype of the overridden attribute type Bar`:

  ```python
  class Base:
      a: int = 0
  class Child(Base):
      a: str = ""

  def foo() -> None:
      base: Base = Child()
      base.a + 1
  ```

  This would fail at runtime with `TypeError: can only concatenate str (not "int") to str`.

+ `Returned type Foo is not a subtype of the overridden return Bar.`: Check for reasons like [invariance](#covariance-and-contravariance).

### 16: Missing Attributes

Your code is most likely trying to access an attribute that Pyre does not know about.
Pyre has various ways of inferring what is an attribute of an object:

#### Explicitly Declare the Attribute

```python
class Derp:
    my_attribute: int = 1

    @property
    def my_property(self) -> int: ...
```

#### Implicitly Declare the Attribute

```python
class Derp:
    def __init__(self, foo: str) -> None:
        self.my_attribute: int = 1

        # The `foo` attribute is inferred to have type `str` because the
        # parameter `foo` has type `str`.
        self.foo = foo
```
Pyre does one level of inlining to infer implicit parameters
We suggest you do not heavily rely on this feature as it is not sound and makes our code brittle.
Support for this is temporary.

#### Common Reasons

+ `Optional type has no attribute foo.`: See [Optional attributes](#optional-attributes).

+ `Foo has no attribute bar.`: Check if you have explicitly provided the type for `bar` either in the constructor or as a class attribute.

+ `Module foo has no attribute bar`: Check if the library has [stubs](#third-party-libraries). If so, you may need to add the function, class, or global variable to the stub.

+ A library class has an attribute but it is not recognized by Pyre: Check if the library has [stubs](#third-party-libraries). If so, you may need to add the attribute to the class in the stub.

+ Your class has dynamic attributes: Consider using `__getattr__` in a [stub](gradual_typing.md#when-source-code-is-not-available) so that Pyre doesn't complain about those attributes.

### 18,21: Undefined Name, Undefined Import
Error 18 ("Undefined name") is raised when your code tries to access a variable or function that Pyre could not resolve.
This is usually caused by failing to import the proper module.

```python
  # 'import some_module' is missing
  some_module.some_func()
```

Pyre will raise error 21 instead ("Undefined import") when the import statement is present, but the module to be imported could not be found in the search path.
If the module provides stub files, please provide their location via the `--search-path` commandline parameter.

### 24: Invalid Type Parameters
Pyre will error if a generic type annotation is given with unexpected type parameters.

#### "Generic type expects X type parameters ..."
Either too few or too many type parameters were provided for the container type. For example,
```python
x: List[int, str] = [] # Invalid type parameters error
```
In this case, `typing.List` is a generic type taking exactly one type parameter. If we pass a single parameter, this resolves the issue.
```python
x: List[Union[int, str]] = [] # No error
```

If you do not know or do not want to specify the type parameters, use `typing.Any` but still ensure the arity is correct.
```python
x: List = [] # Invalid type parameters error
x: List[Any] = [] # No error
```

Note: You may see a suggestion to use `typing.List` instead of builtins `list` as the type annotation when providing type parameters. This is to avoid runtime errors, because the builtin `list` does not support subscripting and `list[int]` is therefore not runtime-friendly.


#### "Non-generic type cannot take type parameters ..."
Type parameters are only meaningful if the container type is generic. Passing in the type parameter binds the provided parameter type to the generic in the container class. For example,

```python
class Container:
  def add(element: int) -> None: ...
  def get_element() -> int: ...

x: Container[int] = Container() # Invalid type parameter error
```

```python
T = TypeVar('T')

class Container(Generic[T]):
  def add(element: T) -> None: ...
  def get_element() -> T: ...

x: Container[int] = Container()
x.get_element() # returns int

y: Container[str] = Container()
y.get_element() # returns str
```


#### "Type parameter violates constraints ..."
If a container class is generic over a type variable with given type bounds, any type parameter used must comply with those type bounds. For example,

```python
T = TypeVar('T', bound=Union[int, bool])

class Container(Generic[T]):
  def add(element: T) -> None: ...
  def get_element() -> T: ...

x: Container[int] = Container() # No error

y: Container[str] = Container() # Invalid type parameter error
```

### 29: Call Error

Pyre will emit an error on seeing a call of one of the following types:

1. The called object is not a function. This means that its inferred type is not Callable and it is not an instance of a class which implements a `__call__` method. This could happen due to user error (the object is indeed not a function) or due to an incorrect or incomplete type stub for the object's class causing pyre to infer the wrong type.

2. The call cannot be safely typed since the types and kinds of its parameters depend on a type variable. This is seen when the callable is typed using a ParameterSpecification type variable and the `*args` and `**kwargs` are not passed into the call correctly, i.e. together and in order. (For more details see [PEP 612](https://www.python.org/dev/peps/pep-0612/#the-components-of-a-paramspec))

```python
from pyre_extensions import ParameterSpecification

P = ParameterSpecification("P")

def decorator(f: Callable[P, int]) -> Callable[P, None]:

  def foo(*args: P.args, **kwargs: P.kwargs) -> None:

    f(*args, **kwargs)    # Accepted, should resolve to int

    f(*args)              # Rejected

    f(*kwargs, **args)    # Rejected

    f(1, *args, **kwargs) # Rejected

  return foo
```

### 33: Prohibited Any
Pyre will warn on any usage of `typing.Any` when run in [strict mode](types-in-python#strict-mode). `Any` is an escape hatch that hides type errors and introduces potential type inconsistencies which Pyre strict is designed to make explicit. To resolve this error, replace `Any` with any other annotation. Using builtins `object` is acceptable if you are looking for a supertype of all classes.


### 30: Terminating Analysis

This indicates a bug in Pyre. Please open an issue on [Github](https://github.com/facebook/pyre/issues).

### 34: Invalid type variable

Example:

```python
class Base:
    foo: List[T] = []

$ pyre
Invalid type variable [34]: The current class isn't generic with respect to the type variable `Variable[T]`.
```


```python
def foo(x: int) -> List[T]:
    return [x, x]

$ pyre
Invalid type variable [34]: The type variable `Variable[T]` isn't present in the function's parameters.
```

Type variables can only be used as types when they have already been placed "in scope".
A type variable can be placed into scope via:

* Generic class declarations
  * for example, `class C(Generic[T]):` puts `T` into scope for the body of the class
* The **parameter** types of a generic function
  * for example, `def foo(x: T)` puts `T` into scope for the body and return type annotation of the function

Suggested fix:

```python
class Base(Generic[T]):
    foo: List[T] = []

base: Base[int]

def foo(x: T) -> List[T]:
    return [x, x]
```

#### Decorator Factories

One common error is when defining a generic decorator factory. The Python type system doesn't currently place `T` into scope within a `Callable` type. So, it considers `T` to be a type variable from the outer scope. This can lead to errors for apparently valid code:

```python
from typing import *

T = TypeVar("T")
R = TypeVar("R")

def my_decorator_factory(message: str) -> Callable[[Callable[[T], R]], Callable[[T], R]]:

    def _decorator(f: Callable[[T], R]) -> Callable[[T], R]:

        def _inner(x: T) -> R:
            print(message)
            return f(x)

        return _inner

    return _decorator

$ pyre
Invalid type variable [34]: The type variable `Variable[R]` isn't present in the function's parameters.
Invalid type variable [34]: The type variable `Variable[T]` isn't present in the function's parameters.
```

Suggested fix: Use a callback protocol to define the return type.

```python
from typing import *

T = TypeVar("T")
R = TypeVar("R")

class MyCallableProtocol(Protocol):
    def __call__(self, __f: Callable[[T], R]) -> Callable[[T], R]: ...

def my_decorator_factory(message: str) -> MyCallableProtocol:

    def _decorator(f: Callable[[T], R]) -> Callable[[T], R]:

        def _inner(x: T) -> R:
            print(message)
            return f(x)

        return _inner

    return _decorator
```

### 35: Illegal Annotation Target
Pyre will error when a type annotation is applied to something that can't be annotated. This could happen when:

1. A variable is re-annotated after first declaration or an explicity annotated function parameter is re-annotated within the function body. This is not allowed as re-annotating variables reduces readability and causes the annotation of a variable to depend on the position in control flow.

```python
def transformation(p: int) -> str:
  return str(p + 1)

def foo(x: int) -> None:
  y: int = x + 2
  z = x + 3

  # Each of the following will produce an error
  x: str = transformation(x)
  y: str = transformation(y)
  z: int = 4
```
An easy fix for the first two errors is to use a new variable rather than re-annotating the old variable so it can hold a new type. For the third error, `z` should have been annotated at first declaration.

2. Trying to annotate non-self attributes, i.e annotating the attributes of a different class than the one whose scope you are in:

```python
class Foo:
  attribute: int = 1

class Bar:
  def __init__(self):
    Foo.attribute: str = "hello"

def some_method() -> None:
  Foo.attribute: int = 5
```

This is not allowed as Pyre needs to be able to statically determine the type of globally accessible values, including class attributes. Even if Pyre followed control flow across functions to determine class attribute annotations, such re-annotations imply very dynamic behavior that makes the code difficult to work with.

The fix for this situation, similar to the case above, is to annotate the class attribute at its definition in the class that owns it and remove any annotations elsewhere. If this attribute is from a third party library, then you can add a [stub](errors#third-party-libraries) for the class and annotate the attribute there.

### 39: Invalid Inheritance
When defining a new class, Pyre will error if the base class given is not a valid parent class. This may be caused by various conditions:

1. The parent class is marked as final which means it explicitly is annotated as not supporting child classes.

```python
@final
class Base:
  ...

class Derived(Base): # Invalid inheritance error
  ...
```

2. The expression given in the base class field is not a class at all.

```python
MY_GLOBAL: str = "string"
class Foo(MY_GLOBAL): # Invalid inheritance error
  ...
```

Pyre does not support dynamic expressions as base classes, even if they may evaluate to a valid class at runtime. This is because the type checker relies on building up a valid class hierarchy before it can resolve types in the Python it is analyzing. On the other hand, type aliases are equivalent to types and are acceptable as base classes.


3. You are defining a typed dictionary that does not inherit from another typed dictionary.

```python
class NonTypedDict:
  ...

class Movie(TypedDict):
  name: str
  year: int

class BookBasedMovie(Movie): # No error
  based_on: str

class BookBasedMovie(NonTypedDict): # Invalid inheritance error
  based_on: str
```

If inheriting from another typed dictionary, fields need to have a consistent type between child and parent, in order for subclassing to be sound. Similarly, a required field in the child must also be required for the parent.

### 41: Invalid Assignment
Pyre will error on assignments to final attributes, read-only properties, and class variables from a class instance. For example,

```python
class Foo:
  field: Final[Optional[int]] = 1

  def foo() -> None:
    self.field = 2 # Invalid assignment

class Bar:
    _x = 1
    @property
    def x(self) -> int:
        return self._x

def bar(b: Bar) -> None:
    b.x = 1 # Invalid assignment
```

To fix this error, change the definition of this attribute to something that is mutable, if it is not intended to be read-only.

### 46: Invalid type variance
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

### 49: Unsafe Cast
Pyre supports `typing.cast` to force the type checker to accept a given type for your expression, no matter what it would otherwise infer that type to be. This is a good escape hatch but can also hide type inconsistencies and introduce unsoundness. For example:

```python
def foo(x: int) -> str:
    y = cast(str, x)
    return y # No type error, even though this is unsound.
```

It is safe to broaden the inferred type of a variable. In other words, casting an expression to a more general type than the type checker thinks it has is sound. If you wish to broaden the inferred type without running the risk of introducing type inconsistencies, you can use `pyre_extensions.safe_cast`. This will warn if the type you are casting to is not greater than or equal to the inferred type of the expression.

```python
def foo(x: int) -> str:
    y = safe_cast(str, x) # Unsafe cast error
    z = safe_cast(Union[int, str], x) # No error
    return z # Invalid return type error
```


### 51: Unused Local Mode
Pyre only supports two modes of type checking, [unsafe](types-in-python#gradual-typing) and [strict](types-in-python#strict-mode). By default, every file runs in unsafe mode, but you can change this default to strict in your [configuration file](configuration#configuration-files).

You can also change the type checking mode of a single file by adding a local mode in the form of a `# pyre-strict` or `# pyre-unsafe` comment on its own line to the file header. This will ensure that file checks under the specified mode regardless of the default.

If you specify more than one local mode, Pyre will error and ask you to remove all but one.

### 40: Invalid Override
Pyre will error when methods in a child class override those in a parent class inconsistently.
Static methods cannot be overwritten by non-static methods, and final methods cannot be overwritten.

```python
class A:
    @staticmethod
    def foo(self) -> int:
        pass

class B(A):
    @classmethod # Non-static method `B.foo` cannot override a static method defined in `A`.
    def foo(cls) -> int:
        pass
```


```python
  from typing import final
  class Foo:
    @final
    def bar(self) -> None:
      pass

  class Bar(Foo):
    def bar(self) -> None: # Invalid override [40]: `Bar.bar` cannot override final method defined in `Foo`.
      pass
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

### 57: Incompatible async generator return type

An async generator function is an `async` function that contains at least one `yield` statement. The Python runtime ensures that all async generator would return an async generator object. Therefore, the return type of async generator functions should always be `typing.AsyncGenerator` or one of its supertypes.

```python
async def f() -> int:  # Error
  yield 0

from typing import AsyncGenerator
async def g() -> AsyncGenerator[int, None]:  # OK
  if False:
    yield 1
```

## Suppression
It is not always possible to address all errors immediately – some code is too dynamic and should be refactored, other times it's *just not the right time* to deal with a type error. We do encourage people to keep their type check results clean at all times and provide mechanisms to suppress errors that cannot be immediately fixed.

### Suppressing Individual Errors
Pyre supports error suppression of individual errors with comments that can be placed on the line of the error or on the line preceding the error.

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

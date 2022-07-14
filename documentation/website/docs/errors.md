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

Most commonly used generic containers have immutable variants, and I would encourage you to use them for function parameters whenever you don't need to modify a container in your function.
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

#### Contravariance

`Callable`, on the other hand, is [contravariant](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)#Contravariant_method_parameter_type) in its parameter types. This means that, when checking if `Callable[[A], None]` is compatible with `Callable[[B], None]`, we check if `B` is compatible with `A`, not the other way around. This is because the former should be capable of accepting any arguments accepted by the latter.

For example, a function of type `Callable[[Base], int]` may be given an argument of type `Child2`. But if we passed in a function of type `Callable[[Child1], int]`, this could fail at runtime:

```python
class Base: pass

class Child1(Base):
    size: int = 42

# No size field.
class Child2(Base): pass

def print_child2_size(get_size: Callable[[Base], int]) -> None:
    child2 = Child2()
    size = get_size(child2)
    print(size)

def size_of_child1(child1: Child1) -> int:
    return child1.size

print_child2_size(size_of_child1) # BAD!

# At runtime:
# AttributeError: 'Child2' object has no attribute 'size'
```

To prevent such errors, Pyre raises a type error when violating contravariance:

```
$ pyre
Incompatible parameter type [6]: Expected `typing.Callable[[Base], int]` for 1st positional only parameter to call `print_child2_size` but got `typing.Callable(size_of_child1)[[Named(child1, Child1)], int]`.
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
    # ... interleaving logic
    process_field(data.field)  # Error: expected `int` but got `Optional[int]`
```

The above fails to type-check because Pyre cannot guarantee that `data.field` remains not `None` if the interleaving logic between the explicit check and the later reference contains anything that may have side effects, like function calls.

An interleaving call could set `field` back to `None`, since it's a non local variable and is mutable. Therefore any calls between the None check and the access will invalidate the "not `None`" refinement. If `data.field` is defined as a class property or if the parent class has overridden `__getattr__`, then all bets are off even if there are no interleaving calls.

The preferred way to make this code type-check is to either move the check closer to the access, or to mark the attribute `Final` if it is not meant to be reassigned to, and you can guarantee to the type checker that no interleaving side effects can modify this attribute.

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
    # ... interleaving logic
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


### 2: Missing Parameter Annotation

If strict mode is turned on, Pyre will error when a function argument is either annotated with a return type that contains `typing.Any`, or is not annotated with any type at all (in which case Pyre will treat it as `typing.Any` by default). It will also error when a method argument is not annotated, unless that argument is the first argument of a bound or static method (i.e. `self`, whose type pyre can infer).

We enforce typed argument because `typing.Any` can hide type errors that will happen at runtime:
```python
from typing import Any

def say_hello(name) -> None:
    print("Hello " + name)


# This line will raise at runtime, but no type error since `say_hello`s `name` has type `Any`.
say_hello(42)
```

You can silence this by adding a non-`Any` annotation to all parameters of functions and methods (other than `self` and `cls` for bound and class methods, which you may omit).

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

Adding a type annotation will resolve this error:
```python
class A:
    b: int = foo()
```


This error can also occur when pyre is inferring attribute types from constructors.
For example, here we know that `b` is an int based on the parameter annotation in `__init__`:
```python
class A:
    def __init__(self, b: int) -> None:
        self.b = b
```

But here we need a annotations because we can't just propagate an argument annotation:
```python
class A:
    def __init__(self, arg: int) -> None:
        self.a = arg + 5
        self.b = arg + 5
```

We can fix this by making the annotation explicit, either in the class body or
in `__init__`:
```python
class A:
    a: int
    def __init__(self, arg: int) -> None:
        self.a = arg + 5
        self.b: int = arg + 5
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

If you are seeing errors with invariant containers where some `Container[T]` is expected but you are passing `Container[S]` where `S < T`, please see [Covariance and Contravariance](errors.md#covariance-and-contravariance).

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

If you are seeing errors with invariant containers where some `Container[T]` is expected but you are passing `Container[S]` where `S < T`, please see [Covariance and Contravariance](errors.md#covariance-and-contravariance).

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
from typing import TypeVar

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


### 10: Unbound Name

Pyre produces an Unbound Name error when your code access a variable (local or global) that pyre believes is not defined.

In most cases code that does this is invalid and will always fail. For example this code will always fail at runtime:
```python
def f() -> int:
    return x  # use of an unbound name x
```

There are some cases where python code that works fine at runtime could produce this error, for example if a function implicitly sets a module-level global variable that is not declared in the toplevel. Pyre will not accept this because module-level globals require type annotations, and if they have no declaration there is nowhere to put the annotation:
```python
def set_x() -> None:
    global x
    x = 42

def use_x() -> None:
    print(x)


# this code will run fine, but pyre cannot analyze the type or use of the
# implicitly-defined global x and will complain about an unbound name.
set_x()
use_x()
```

You can fix this by explicitly adding a declaration of the top-level variable `x`, for example:
```python
x : Optional[int] = None
```

### 11, 31: Undefined or Invalid Type
Pyre recognizes class names as valid annotations. Most basic types are imported from the `typing` module or are already available from builtins like `str`, `int`, `bool`, etc. You can also define your own type alias on the global scope, which can be used as annotations:

```python
from typing_extensions import TypeAlias

INT_OR_STR: TypeAlias = Union[int, str]
```

If you use a name as an annotation that is not a valid type or valid alias, you will see this error:

```python
from typing import Callable, List
from typing_extensions import Final, Literal

GLOBAL_VALUE = "string"

def f0() -> GLOBAL_VALUE: ... # Error! `GLOBAL_VALUE` is a value, not a type.

def f1() -> type(GLOBAL_VALUE): ...   # Error! Static type annotations cannot be dynamically computed.

def f2() -> [int]: ...  # Error! `[int]` is not a valid type. If you mean a list of int, use `typing.List[int]`.

def f3() -> (int, str): ...  # Error! `(int, str)` is not a valid type. If you mean a pair of int and str, use `typing.Tuple[int, str]`.

def f4() -> Callable[[int]]: ...  # Error! `Callable[[int]]` is not a valid type because the return type of the callable is missing. Good example: `Callable[[int], int]`.

def f5() -> Callable[int, int]: ...  # Error! `Callable[int, int]` is not a valid type. The parameter types of the callable must be enclosed in square brackets. Good example: `Callable[[int], int]`.

def f6() -> List[Final[int]]: ...  # Error! `Final` may only be used as the outermost type in annotations. See PEP 591.

def f7() -> Literal[GLOBAL_VALUE]: ...  # Error! Only literals are allowed as parameters for `Literal`. See PEP586. Good example: `Literal[42]` or `Literal["string"]`.
```

You can fix this error by verifying that your annotation is

1. statically determined.
2. properly imported from `typing` if applicable.
3. properly defined in the module you are importing from. If the module you are importing from has a [stub file](errors.md#third-party-libraries), you should check the definition there.
4. properly adhere to the additional rules of special types (e.g. `Callable`, `Final`, and `Literal`).

#### Type Aliases

For type aliases, check that your type alias is defined

1. with a valid type on the RHS. If you provide an annotation for the TypeAlias assignment, it must be `typing_extensions.TypeAlias`.
2. on the global scope, not nested inside a function or class.

#### ParamSpec

For `ParamSpec`, check that you have used both `*args: P.args` and `**kwargs: P.kwargs` in your function's parameters:

```python
from typing import Callable

from pyre_extensions import ParameterSpecification

P = ParameterSpecification("P")

# OK
def good(f: Callable[P, int], *args: P.args, **kwargs: P.kwargs) -> int:
    return f(*args, **kwargs)

# Error because `**kwargs: P.kwargs` is missing.
def bad1(f: Callable[P, int], *args: P.args) -> int:
    return f(*args)

# Error because `*args: P.args` is missing.
def bad2(f: Callable[P, int], **kwargs: P.kwargs) -> int:
    return f(**kwargs)

$ pyre
Undefined or invalid type [11]: Annotation `P.args` is not defined as a type.
Call error [29]: `typing.Callable[P, int]` cannot be safely called because the types and kinds of its parameters depend on a type variable.
Undefined or invalid type [11]: Annotation `P.kwargs` is not defined as a type.

```


### 12: Incompatible Awaitable Type

In strict mode, pyre will verify that all calls to `await` are on awaitable values, to ensure that you cannot get a runtime error awaiting an object that is not a coroutine.

A common situation where working code will produce this error is when pyre cannot statically verify that an awaitable is non-Null, for example:
```python
import asyncio

async def f(flag: bool) -> None:
    if flag:
        task = asyncio.create_task(asyncio.sleep(1))
    else:
        task = None
    await task  # would throw a ValueError if flag were false

asyncio.run(f(True))
```

In this example, `task` will always be a valid awaitable unless some other module overwrites the global `flag`, but pyre cannot prove that this does not happen. The error we get has the message
```
Expected an awaitable but got `typing.Optional[asyncio.tasks.Task[None]]`
```

You can fix this error by ensuring that the awaited object has an awaitable type. In the case of optional values, you can use refinement to rule out `None`. The example above can be fixed by tweaking the definition of `main`:
```python
async def f(flag: bool) -> None:
    if flag:
        task = asyncio.create_task(asyncio.sleep(1))
    else:
        task = None
    if task is not None:
      await task
```


### 13: Uninitialized Attribute

In strict mode, pyre will throw an error for class attributes which are declared without default values if they are not initialized in a constructor, for example:
```python
class A:
    x : int

    def __init__(self) -> None:
        pass
```
For a case like this, you can fix the error either by setting a default value like `x : int = 0` at the class level, or by setting `x` in the constructor e.g. `self.x = 0`.

#### Dataclass-like classes

One case where this can occur is when using a library providing a "dataclass-like" decorator that, for example, autogenerates a constructor setting attributes.

Pyre currently knows that that uninitialized attributes of classes wrapped in `dataclass` and `attrs` decorators will generate constructors that set the attributes. But it does not understand many custom libraries that do similar things, for example test frameworks, or new decorators that wrap the dataclass decorator and add more logic.

There is not currently a way to fix this other than via `pyre-ignore` or `pyre-fixme` directives. The python typing community is aware of this problem but has not yet settled on a solution, you can see discussion [here](https://mail.python.org/archives/list/typing-sig@python.org/thread/TXL5LEHYX5ZJAZPZ7YHZU7MVFXMVUVWL/).


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
    def width() -> float: pass

class JpegImage(Image):
    def width() -> int: return 10  # this is fine

class ComplexImage(Image):
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

+ `Returned type Foo is not a subtype of the overridden return Bar.`: Check for reasons like [invariance](errors.md#covariance-and-contravariance).

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

+ `Optional type has no attribute foo.`: See [Optional attributes](errors.md#optional-attributes).

+ `Foo has no attribute bar.`: Check if you have explicitly provided the type for `bar` either in the constructor or as a class attribute.

+ `Module foo has no attribute bar`: Check if the library has [stubs](errors.md#third-party-libraries). If so, you may need to add the function, class, or global variable to the stub.

+ A library class has an attribute but it is not recognized by Pyre: Check if the library has [stubs](errors.md#third-party-libraries). If so, you may need to add the attribute to the class in the stub.

+ Your class has dynamic attributes: Consider using `__getattr__` in a [stub](gradual_typing.md#when-source-code-is-not-available) so that Pyre doesn't complain about those attributes.


### 17: Incompatible Constructor Annotation

[PEP 484](https://www.python.org/dev/peps/pep-0484/#the-meaning-of-annotations) specifies that `__init__` method of any class must be annotated to return `None`. Pyre will emit an error if the user's annotation does not conform to the specification.

```python
# pyre-strict

class A:
    def __init__(self) -> "A":  # Error 17: Invalid return annotation of `__init__`.
        ...

class B:
    def __init__(self) -> None:  # OK
        ...
```

### 18,21: Undefined Name, Undefined Import
Error 18 ("Undefined name") is raised when your code tries to access a variable or function that Pyre could not resolve.
This is usually caused by failing to import the proper module.

```python
  # 'import some_module' is missing
  some_module.some_func()
```

Pyre will raise error 21 instead ("Undefined import") when the import statement is present, but the module to be imported could not be found in the search path.
If the module provides stub files, please provide their location via the `--search-path` commandline parameter.

#### Namespace Package Modules

One case where you may run into undefined imports on code that works at runtime is when importing namespace modules.
The CPython runtime allows you to import a directory that is on your `PYTHONPATH`, even if it contains no `__init__.py`; this behavior is defined in [PEP 420](https://peps.python.org/pep-0420/) and the module is called a namespace package.
In order to make Pyre both fast and consistent on incremental updates, in Pyre we only allow importing namespace packages that have at least one python file as a direct child.

So, for example, if I have a directory tree with just `a/b/c.py` then Pyre will allow `import a.b.c` and `import a.b` but not `import a`.
A namespace package module can never contain useful types or code so it is rare to directly import it, but in special cases it might be useful (for example to access the `__name__` attribute).
In these cases, you'll need to suppress Pyre errors.

### 19: Too Many Argument

Pyre verifies that you pass a legal number of arguments to functions.

The most obvious way to encounter this error is to just pass too many arguments to a function:
```python
def f(x: int) -> int:
    return x

f(5, 6)  # this would throw a TypeError at runtime, and pyre complains
```
To fix this, make sure you pass the correct number of parameters. In some cases you may encounter this error if you intended to use a variadic argument (`*args`) or to set a default value.

Pyre will also throw this error if you pass too many positional arguments to
a function that uses python's ability restrict arguments to be keyword-only
specified by [PEP 3102](https://www.python.org/dev/peps/pep-3102/):
```python
def f(*, x: int) -> int:
    return x

f(5)  # As before, this throws a TypeError because x is positional-only
f(x=5)  # this line will typecheck and run without error
```

### 20: Missing Argument

Pyre verifies that function calls provide all the expected arguments, so it will complain about code like this:
```python
def f(x: int) -> ing:
    return x

f()
```

To fix this, make sure all required arguments are provided.

### 22: Redundant Cast
Pyre will warn when you attempt to use `typing.cast` to cast a variable to a type that the type checker already knows that variable has. This is because `typing.cast` is purely a tool for communicating with the static type checker, and will not provide any runtime guarantees. Therefore a redundant cast provides no value and is likely a mistake.

If you are trying to document the type of the variable, you can provide an explicit annotation where it is declared. If you are trying to add a sanity check at runtime that the type of a variable is what you already believe it must be, use `isinstance`.


### 23: Unable to Unpack

Pyre will warn you when trying to assign a value to a tuple with the wrong number of items.

```python
def foo() -> None:
    a, b = (1, 2, 3)
    x, y = 42

$ pyre
Unable to unpack [23]: Unable to unpack 3 values, 2 were expected.
Unable to unpack [23]: Unable to unpack `int` into 2 values.
```

Common reasons:

+ Trying to assign an `Optional` value to a tuple:

  ```python
  def bar() -> None:
      x = None
      if 2 + 2 == 4:
          x = ("a", "b")

      a, b = x

  $ pyre
  Unable to unpack [23]: Unable to unpack `typing.Optional[typing.Tuple[str, str]]` into 2 values.
  ```

+ Unpacking an incorrect number of elements when looping over a list:

  ```python
  for a, b in [1, 2, 3]:
      print(a, b)

  $ pyre
  Unable to unpack [23]: Unable to unpack `int` into 2 values.
  ```

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
from typing import TypeVar

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
from typing import TypeVar

T = TypeVar('T', bound=Union[int, bool])

class Container(Generic[T]):
    def add(element: T) -> None: ...
    def get_element() -> T: ...

x: Container[int] = Container() # No error

y: Container[str] = Container() # Invalid type parameter error
```

### 26: Typed Dictionary Access With Non-Literal

In python, [typed dictionaries can only be accessed using literal strings](https://www.python.org/dev/peps/pep-0589/#id21) that can be statically verified as valid. As a result, code like this will not typecheck even though it works at runtime, because we cannot statically verify that `key` in `print_value` is a valid `Shape` key:
```python
from typing import TypedDict

class Shape(TypedDict):
    sides: int
    color: str

shape: Shape = {"sides": 4, "color": "blue"}

print(shape["sides"])  # this is fine because "sides" is a literal

for key in ["sides", "color"]:
    print(key, shapes[key])  # pyre will complain here because it can't prove `key` is valid
```

The example above shows a situation where you might hit this error: when you want to iterate over the fields of a typed dict. A suggested fix is to use type-safe operations like `dictionary.items` instead. For example the following code produces the same results but typechecks:
```python
for key, value  in shapes.items():
    print(key, value)
```

In other cases where you need to access a `TypedDict` using a variable as a key, you can use `dictionary.get(key)`.

### 27: Typed Dictionary Key Not Found

If you try to access a typed dictionary with a string literal that pyre knows is not a valid key, pyre will emit an error:
```python
from typing import TypedDict

class Shape(TypedDict):
    sides: int
    color: str

def f(shape: Shape) -> None:
    print(shape["location"])  # error here
```

A possible fix: pyre considers instances of any `TypedDict` with additional fields to be a subtype of `Shape`, so in many cases you could handle the need for a `location` field in some `Shape` dicts by creating a new type as follows:
```python
from typing import TypedDict

class Shape(TypedDict):
    sides: int
    color: str

class ShapeWithLocation(TypedDict):
    sides: int
    color: str
    location: str

def f(shape: ShapeWithLocation) -> None:
    print(shape["location"])  # okay
    g(shape)  # also okay: ShapeWithLocation is a subtype of Shape

def g(shape: Shape) -> None:
    print(shape)
```

### 28: Unexpected Keyword

Pyre will error if attempting to pass an argument by name and there are no parameters with a matching name. For example,

```python
def foo(integer: int, string: str) -> None: ...

foo(1, "one")  # no error
foo(string="one", integer=1)  # no error
foo(integer=1, undefined="one")  # type error
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

### 30, 36: Terminating Analysis, Mutually Recursive Type Variables

#### Overly-complex Functions

In very rare cases where a function has a lot of `if` branches or `for`-loops, Pyre may raise an error saying that is unable to analyze the function fully. Analyzing extremely complex functions in depth can be costly, so Pyre only does so up to a limit. This means that it won't infer precise types for some variables and won't catch errors related to their usage. For example:

```python
def my_function() -> None:
    u = 42

    if foo():
        x1 = bar()
        if x1:
            x2 = baz()
            if x2:
                # <more branches of code>
        else:
            # <more branches>

        if foo2():
            # <code>

        if foo3():
            # <even more branches>
        # <and even more branches>

$ pyre
Analysis failure [30]: Pyre gave up inferring types for some variables because function `foo` was too complex.
Please simplify the function by factoring out some if-statements or for-loops.
```

To remedy this, factor out some of the branching code into separate functions out that each function has a limited amount of branching logic:

```python
def do_stuff() -> None:
    if foo():
        # <code>

    if foo():
        # <even more branches>

    # <and even more branches>

def bar() -> None:
    u = 42

    if foo():
        x1 = bar()
        if x1:
            x2 = baz()
            if x2:
                # <more branches of code>
        else:
            # <more branches>

        do_stuff()
```
#### Other Analysis Failures

These errors usually indicates a bug in Pyre. Please open an issue on [Github](https://github.com/facebook/pyre/issues).

### 31: Invalid Type

This indicates that you are using some expression that pyre does not understand as a type.

Some situations where you might run into this:

+ Using a list of types rather than `List[type]`:
 ```python
 x: [str] = ["a string"]
 ```
 You can fix this by using the `List` type:
 ```python
 x: List[str] = ["a string"]
 ```

+ Using a constructor call rather than a bare class name:
  ```python
  class A:
      ...

  a: A() = A()
  ```
  You can fix this by using a bare type name:
  ```python
  a: A = A()
  ```


### 32: Invalid Argument

This error usually means you are using a variable in a way that is incompatible with its structure, either as an argument to a function call or as part of a data structure.

This could be from using an invalid variadic parameter (informally a "splat"):
```python
x: int = 5

print(*x)   # invalid use of x, which is not iterable
```
or using an invalid keyword parameter (informally a "double-splat"):
```python
from typing import Dict

x: int = 5

d: Dict[int, int] = {**x}  # invalid use of x, which is not a mapping

dict(**d)  # invalid use of d; function kwargs must be a mapping with string keys
```

It's also possible to hit this error code on constraint mismatches when using tuple variadic variables as specified in [PEP 646](https://www.python.org/dev/peps/pep-0646/), which are an advanced feature of pyre.

### 33: Prohibited Any
Pyre will warn on any usage of `typing.Any` when run in [strict mode](gradual_typing.md#strict-mode). `Any` is an escape hatch that hides type errors and introduces potential type inconsistencies which Pyre strict is designed to make explicit. To resolve this error, replace `Any` with any other annotation. Using builtins `object` is acceptable if you are looking for a supertype of all classes.

### 34: Invalid Type Variable


Type variables can only be used as types when they have already been placed "in scope".
A type variable can be placed into scope via:

* Generic class declarations
  * for example, `class C(Generic[T]):` puts `T` into scope for the body of the class
* The **parameter** types of a generic function
  * for example, `def foo(x: T)` puts `T` into scope for the body and return type annotation of the function

For example:

```python
from typing import List

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
Suggested fix:

```python
from typing import Generic, List

class Base(Generic[T]):
    foo: List[T] = []

base: Base[int]

def foo(x: T) -> List[T]:
    return [x, x]
```

#### Decorator Factories

One common error is when defining a generic decorator factory. The Python type system doesn't currently place `T` into scope within a `Callable` type. So, it considers `T` to be a type variable from the outer scope. This can lead to errors for apparently valid code:

```python
from typing import Callable, TypeVar

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
from typing import Callable, Protocol, TypeVar

T = TypeVar("T")
R = TypeVar("R")

class MyCallableProtocol(Protocol):
    def __call__(self, f: Callable[[T], R]) -> Callable[[T], R]: ...

def my_decorator_factory(message: str) -> MyCallableProtocol:

    def _decorator(f: Callable[[T], R]) -> Callable[[T], R]:

        def _inner(x: T) -> R:
            print(message)
            return f(x)

        return _inner

    return _decorator
```

If you are using a `ParamSpec` in your decorator, use the following:

```python
from typing import Awaitable, Callable, Protocol, TypeVar
from pyre_extensions import ParameterSpecification

R = TypeVar("R")
P = ParameterSpecification("P")

class MyCallableProtocol(Protocol):
    def __call__(self, f: Callable[P, Awaitable[R]]) -> Callable[P, Awaitable[R]]: ...

def my_decorator_factory(message: str) -> MyCallableProtocol:

    def _decorator(f: Callable[P, Awaitable[R]]) -> Callable[P, Awaitable[R]]:

        async def _inner(*args: P.args, **kwargs: P.kwargs) -> R:
            print(message)
            return await f(*args, **kwargs)

        return _inner

    return _decorator
```

Note: Support for such callables is currently **experimental** and varies from one typechecker to another. This behavior may change in the future.

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

  The fix for this situation, similar to the case above, is to annotate the class attribute at its definition in the class that owns it and remove any annotations elsewhere. If this attribute is from a third party library, then you can add a [stub](errors.md#third-party-libraries) for the class and annotate the attribute there.

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
  from typing import TypedDict

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

### 40: Invalid Override
Pyre will error when methods in a child class override those in a parent class inconsistently.
Static methods cannot be overwritten by non-static methods, and final methods cannot be overwritten.

```python
class A:
    @staticmethod
    def foo() -> int:
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
    def bar(self) -> None: # Invalid override, because Foo.bar is final
        pass
```

### 41: Invalid Assignment
Pyre will error on assignments to final attributes, read-only properties, and class variables from a class instance. For example,

```python
from typing import Final, Optional

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

### 42: Missing Overload Implementation

Pyre will throw this error if a source module specifies one or more overloads via [`typing.overload`](https://fburl.com/d7b8cd2h) but fails to provide an implementation, for example:
```python
from typing import overload

@overload
def f(x: int) -> float:
    ...

@overload
def f(x: str) -> str:
    ...
```
Missing implementations are allowed in `.pyi` stub files.

To fix it, provide exactly one implementation (a function of the same name without the `typing.overload` decorator). For example above we could implement `f` as follows:
```python
from typing import overload, Union

@overload
def f(x: int) -> float:
    ...

@overload
def f(x: str) -> str:
    ...

@overload
def f(x: str) -> str:
    ...

def f(x: Union[int, str]) -> Union[float, str]:
    if isinstance(x, int):
        return float(x)
    else:
        return x
```

### 43: Incompatible Overload Implementation

Pyre will error if you define one or more overloads using [`typing.overload`](https://fburl.com/d7b8cd2h), and your concrete implementation has an incompatible type signature.

For example, this code will produce an incompatible overload implementation error
```python
# pyre-strict

from typing import overload, Union

@overload
def f(x: int) -> float:
    ...

@overload
def f(x: float) -> int:
    ...

@overload
def f(x: str) -> str:
    ...

def f(x: Union[int, float, str]) -> Union[int, str]:
    if isinstance(x, float):
        return int(x)
    elif isinstance(x, int):
        return float(x)
    else:
        return x
```
The problem here is that the return type `Union[int, str]` is too narrow to
permit `f` to return `float` when called on an `int` argument.

You can fix this by either removing incorrect overload delcarations or making
sure all parameters and return type annotations on the concrete
implementation are general enough to be consistent with the overloads:
```python
def f(x: Union[int, float, str]) -> Union[int, float, str]:
    <same implementation>
```

### 45: Invalid Class Instantiation

In typed Python, some classes that represent abstract interfaces may not be directly instantiated. Pyre considers a class `C` abstract, and will error on invalid instantiation if you try to construct an instance directly in either of the following cases:

1. `C` contains one or more abstract methods that are left not overridden. Abstract methods are defined as methods that are decorated with [`@abc.abstractmethod`](https://docs.python.org/3/library/abc.html#abc.abstractmethod).

  For example, here `Derived0` is abstract because it does not override `bar`, but `Derived1` may be instantiated:
  ```python
  import abc
  from typing import Protocol

  class Base(abc.ABC):
      @abc.abstractmethod
      def foo(self) -> None:
          raise NotImplementedError
      @abc.abstractmethod
      def bar(self) -> str:
          raise NotImplementedError

  class Derived0(Base):
      def foo(self) -> None:
          print(self.bar())

  class Derived1(Derived0):
      def bar(self) -> str:
          return "bar"

  def test0() -> None:
      base = Base()  # Error! Class `Base` contains 2 abstract methods and therefore cannot be instantiated.
      derived0 = Derived0()  # Error! Class `Derived0` contains 1 abstract method `bar` and therefore cannot be instantiated.
      derived1 = Derived1()  # OK
  ```

2. `C` directly inherits from [`typing.Protocol`](https://docs.python.org/3/library/typing.html#typing.Protocol).

  For example, here `MyProtocol` is abstract because it inherits directly from `typing.Protocol`, but `MyClass` (which implements the protocol interface) may be instantiated:
  ```python
  class MyProtocol(Protocol):
      def baz(self, x: int) -> int:
          ...

  class MyClass:
      def baz(self, x: int) -> int:
          return x

  def test1() -> None:
      object0 = MyProtocol()  # Error! Class `MyProtocol` cannot be instantiated.
      object1 = MyClass()  # OK
  ```

### 46: Invalid Type Variance

In brief, read-only data types can be covariant, write-only data types can be contravariant, and data types that support both reads and writes must be invariant.
If a data type implements any functions accepting parameters of that type, we cannot guarantee that writes are not happening. If a data type implements any functions returning values of that type, we cannot guarantee that reads are not happening.
For example (note: int is a subclass of float in the type system and in these examples):
Writes taking covariants:

```python
from typing import TypeVar, Generic

_T_co = TypeVar("_T_co", covariant=True)

class MyList(Generic[_T_co]):
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
from typing import TypeVar, Generic

_T_cont = TypeVar("_T_cont", contravariant=True)

class MyList(Generic[_T_cont]):
    def read(self) -> _T_cont:
        ... # returns first element from list

def takes_int_list(int_list: MyList[int]) -> int:
    return int_list.read()

float_list: MyList[float] = ...
takes_int_list(float_list)  # this call is OK because MyList is contravariant: MyList[float] < MyList[int]
# problem with return above is clear
```

### 47: Invalid Method Signature

Pyre will error if a non-static method fails to specify an expected implicit parameter like `self` for an instance method or `cls` for a class method, as this argument is always implicitly passed in a call and will cause a runtime crash if not specified. Additionally, Pyre will warn if this parameter is specified but typed as something incompatible with the type of the parent class.

Often times, the method may not need a `self` or `cls` and should be decorated with `@staticmethod` to resolve this error. For example,

```python
class Foo:
    def foo() -> None: ...  # type error

class Foo:
    @staticmethod
    def foo() -> None: ... # no type error

class Foo:
    def foo(self) -> None: ... # no type error
```

Only type variables with compatible bounds can be used to annotate the `self` or `cls` parameter. For example,

```python
from typing import TypeVar

P = TypeVar("T", bound="Parent")
A = TypeVar("S", bound="ChildA")
B = TypeVar("S", bound="ChildB")

class Parent: ...

class ChildA(Parent):
    @classmethod
    def foo(cls: Type[A]) -> A: ...  # no type error

class ChildB(Parent):
    def foo(self: A) -> A: ...  # type error
    def bar(self: B) -> B: ...  # no type error
    def baz(self: P) -> P: ...  # no type error
```


### 48: Invalid Exception

In python, you can only raise objects that derive from `BaseException` (it's more common to subtype `Exception` or one of the standard library-defined errors like `ValueError`). Attempting to raise another object such as a bare string will result in a `TypeError`. As a result, pyre will flag code like this:
```python
def f(x: int) -> None:
    if x > 1:
        raise "x is too big"
```

To fix this, wrap the information you are trying to raise (usually an error message) in some exception type, for example:
```python
def f(x: int) -> None:
    if x > 1:
        raise ValueError("x is too big")
```

### 49: Unsafe Cast

To allow "safe" casts that preserve type soundness, you can use `pyre_extensions.safe_cast`. This will verify that the type you are casting to is broader than the type of the expression. In cases where this is not the case, pyre will produce an Unsafe Cast error. For example:
```python
from pyre_extensions import safe_cast

def foo(x: int) -> str:
    y = safe_cast(str, x) # Unsafe cast error
    z = safe_cast(Union[int, str], x) # No error
    return z # Invalid return type error
```

Some context on this: `pyre_extensions.safe_cast` is a type-safe alternative to `typing.cast`. The `typing.cast` function forces type checkers to accept a type for an expression that otherwise would not be valid, which is sometimes useful but also can hide clear type errors, for example:
```python
from typing import cast

def foo(x: int) -> str:
    y = cast(str, x)
    return y # No type error, even though this is unsound.
```

### 51: Unused Local Mode

This error will be thrown if you specify more than one local mode, by having multiple line comments of the form `# pyre-strict` or `# pyre-unsafe` in the header. Pyre will ask you to remove all but one local mode declaration if you have more than one because the mode needs to be unambiguous.

Context: Pyre  supports two modes of type checking, [unsafe](gradual_typing.md#gradual-typing) and [strict](gradual_typing.md#strict-mode).
- By default, every file runs in unsafe mode, but you can change this default to strict in your [configuration file](configuration.md#configuration-files).
- In addition, you can set the type checking mode of a module to differ from the default for the project by adding a comment in the form `# pyre-strict` or `# pyre-unsafe` comment on its own line to the file header.

### 52: Private Protocol Property

Python [Protocols](https://www.python.org/dev/peps/pep-0544/) provide a way to statically check "duck typing", what many languages would refer to as `interfaces`.

Because protocols specify only an interface, they should not include [private fields and methods](https://docs.python.org/3/tutorial/classes.html#private-variables), which cannot not be accessed outside of the class where they are defined (including in subclasses). Pyre will complain about the following:
```python
from typing import Protocol

class Duck(Protocol):

    def __quack(self) -> str:
        ...

class SomeDuck:

    def __quack(self) -> str:
        return "quack"
```

To signal a non-public part of an interface, use a protected field or method (single leading underscore), which is accessible by classes implementing the interface:
```python
from typing import Protocol

class Duck(Protocol):

    def _quack(self) -> str:
        ...

class SomeDuck:

    def _quack(self) -> str:
        return "quack"
```

### 53: Missing Annotation For Captured Variables

Pyre makes no attempt at trying to infer the types across function boundaries. The statement holds for nested functions as well.
From a nested function's perspective, a variable defined in an nesting function behaves similarly to a global variable. As with global variables, an explicit annotation is required if strict mode is turned on:

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

### 54: Invalid TypedDict Operation

In accordance with [PEP 598](https://www.python.org/dev/peps/pep-0589/), code that tries to assign a value of the wrong type to a field of a `TypedDict` will not typecheck:
```python
from typing import TypedDict

class MyDict(TypedDict):
    value: str

d: MyDict = {"value": "hello"}
d["value"] = 5  # Invalid TypedDict operation
```

To fix this you may need to change your field type to a `Union`, if variable types are actually needed for a field.

### 55: TypedDict Initialization Error

Pyre will warn you when initializing a TypedDict with:

+ Missing required fields

  ```python
  from typing import TypedDict

  class Movie(TypedDict):
      name: str
      year: int

  movie: Movie = {"name": "The Matrix"}

  $ pyre
  TypedDict initialization error [55]: Missing required field `year` for TypedDict `Movie`.
  ```

+ Incorrect field type

  ```python
  movie: Movie = {"name": "The Matrix", "year": "1999"}

  $ pyre
  TypedDict initialization error [55]: Expected type `int` for `Movie` field `year` but got `str`.
  ```

+ Undefined fields

  ```python
  movie: Movie = {"name": "The Matrix", "year": 1999, "extra_field": "hello"}

  $ pyre
  TypedDict initialization error [55]: TypedDict `Movie` has no field `extra_field`.
  ```

### 56: Invalid Decoration

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
from typing import TypeVar

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
from typing import Callable

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
from typing import Callable

def decorator(f: Callable[[int], str]) -> int:
    ...

# pyre-fixme[56]: While applying decorator `decorator`:
# Expected `Callable[[int], str]` for 1st param but got `Callable[[str], int]`.
@decorator
def foo(x: str) -> int:
    return 5
```

### 57: Incompatible Async Generator Return Type

An async generator function is an `async` function that contains at least one `yield` statement. The Python runtime ensures that all async generator would return an async generator object. Therefore, the return type of async generator functions should always be `typing.AsyncGenerator` or one of its supertypes.

```python
from typing import AsyncGenerator

async def f() -> int:  # Error
    yield 0

async def g() -> AsyncGenerator[int, None]:  # OK
    if False:
        yield 1
```

### 58: Unsupported Operand

Pyre will warn if an infix operator is not supported for the right or left operands provided.

In Python, an infix operator is converted to a method call on either of the operands - for example, `a < b` is equivalent to `a.__lt__(b)`. Therefore, this type error can also be considered sugar for an error that method `a.__lt__` does not accept the type of `b` as an argument.

For example,

```python
from typing import Optional

def foo(x: Optional[int]) -> bool:
    return x < 0  # type error: Optional[int] is not a supported operand

def bar(x: Optional[int]) -> bool:
    if x:
        return x < 0  # no type error
    return False
```


### 59: Duplicate Type Variables

This occurs when the same type variable is provided more than once to a `Generic` or `Protocol`. A type variable needs to be bound to a single value. Thus, if one wants two independent type variables with perhaps the same bounds or same properties, they have to be different variables.

```python
from typing import TypeVar, Generic

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

class A(Generic[T0, T1, T0]):  # Error
    pass

class B(Generic[T0, T1, T2]):  # OK
    pass
```

### 60: Unable to Concatenate Tuple

#### "Expected to unpack an iterable ..."

This can occur if during concatenation of a tuple one tries to unpack a non-iterable since non-iterables can't be unpacked. Either try to unpack an iterable, or concatenate without unpacking.

```python
def foo(x: int, not_iterable: int, iterable: list[int]) -> None:
    y = (x, *not_iterable)  # Error
    z = (x, not_iterable) # OK
    w = (x, *iterable)  # OK
```

#### "Concatenation not yet supported for multiple variadic tuples ..."

This can occur if during concatenation one tries to use multiple variadic tuples. This is due the limitations of the current type system and there is no workaround currently. One may use `# pyre-ignore[60]` to suppress.

```python
from typing import Tuple
from pyre_extensions import TypeVarTuple

Ts = TypeVarTuple("Ts")

def foo(xs: Tuple[*Ts]) -> None:
    y = (*xs, *xs)  # Error
```

### 61: Uninitialized Local

This indicates that there are code paths along which a local variable may not be initialized. Below are some common code patterns that may cause this error:

#### Not initialized in all branches of condition

```python
def f(x: int) -> None:
    z = None
    if x > 5:
        y = 2
        z = 2
    print(y)  # Error
    print(z)  # OK
```
`y` is not defined when the `if` condition is not met. For instance, `f(4)` will result in a runtime error. Possible ways to address this:

- initialize `y` to a default value, outside the conditional or in the `else` branch
- refactor so that initialization and access are in the same conditional

Pyre static analysis does not reason about runtime values or potential side effects of interleaving calls, so for instance, in the example below we cannot guarantee that the two if statements will always be consistent and, hence, throw the same error:
```python
def f(x: int) -> None:
    if x > 5:
        y = 2
    # ...some operations...
    if x > 5:
        print(y)    # Error
```

#### Initialized only inside a `for` loop
```python
def f(xs: List[int]) -> None
    for x in xs:
        y = "yes"
    print("Last element is: ", x)  # Error
    print("Did we enter the loop?", y)  # Error
```
Here, if one calls `f([])`, it will result in errors.

One way to remediate is to initialize outside the loop. For instance,
```python
def f(xs: List[int]) -> None:
    x = None
    y = "no"
    for x in xs:
        y = "yes"
    print("Last element is: ", x)  # OK
    print("Did we enter the loop?", y)  # OK
```

#### Initialized in `try` block
```python
def f(divisor: int) -> None:
    answer_good = None
    try:
        answer_bad = 5 / divisor
        answer_good = 5 / divisor
        answer_also_good = 5 / divisor
        print(f"5 divided by {divisor} is {answer_also_good}")   # OK
    except ZeroDivisionError:
        pass
    print(f"5 divided by {divisor} is {answer_bad}")  # Error
    print(f"5 divided by {divisor} is {answer_good}")  # OK
```
Here, `f(0)` leads to an error on access of `answer_bad`. Suggested approaches to address this:

- Initialize any variables needed after the `try` block to a default value before entering the `try` block.
- Keep the access to variables initialized inside the `try` block within the `try` block.
- Consider if pulling the initialization as-is before the `try` block is possible. It is generally considered a good practice to minimize the code inside a try block, and keep it to exception throwing code. This also helps with Pyre, as it does not reason about which operations might throw exceptions.

```python
def bad(divisor: int) -> Optional[int]:
    try:
        dividend = 5
        return dividend // divisor
    except ZeroDivisionError:
        print(f"Cannot divide {dividend} by 0")  # Error (according to Pyre)

def good(divisor: int) -> Optional[int]:
    dividend = 5
    try:
        return dividend // divisor
    except ZeroDivisionError:
        print(f"Cannot divide {dividend} by 0")  # OK
```
## Suppression
It is not always possible to address all errors immediately – some code is too dynamic and should be refactored, other times it's *just not the right time* to deal with a type error. We do encourage people to keep their type check results clean at all times and provide mechanisms to suppress errors that cannot be immediately fixed.

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

### Suppressing Errors within Format Strings

If you want to suppress an error within an f-string, you can add a fixme comment on the line before the string. This will suppress all errors within the f-string matching that fixme:

```python
def print_profile(name: str, age: Optional[int]) -> None:
    # pyre-fixme[58]: `-` is not supported for operand types `Optional[int]` and `int`.
    s = f"""
    Your personal details!!

    Name: {name}
    Age: {age - 3}
    """
    print(s)
```

### Suppressing All Errors
You can use the [Pyre upgrade tool](gradual_typing.md#upgrade) to add inline error suppressions for all errors in your project.

### Suppressing Errors Across Files
You can suppress all errors in entire sections of your code by adding the path to the [`ignore_all_errors` section of your configuration](configuration.md#the-global-configuration).

Furthermore Pyre supports suppressing all errors in an individual file if you add a `# pyre-ignore-all-errors` to your file. Like the other suppression comments, you can use square brackets to chose to only ignore one or more particular error types. For example, you can suppress all incompatible return type errors by adding:

```python
# pyre-ignore-all-errors[7]

def foo(x: int) -> str:
    return x  # pyre will not error here
```

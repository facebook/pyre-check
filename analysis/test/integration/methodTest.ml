(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Test
open OUnit2
open IntegrationTest

let test_check_method_returns context =
  assert_type_errors
    ~context
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    ~context
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    ~context
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."]


let test_check_method_parameters context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  (* Calls to methods *)
  assert_type_errors {|
      def foo(input: str) -> None:
        input.substr(1)
    |} [];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`." ];
  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st anonymous parameter to call `foo` but got `int`." ];
  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`." ];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`." ];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`." ];
  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      def foo(b: str, a: str) -> None: ...
      b: int = 1
      a: int = 1
      foo(b, a)
    |}
    [ "Incompatible parameter type [6]: Expected `str` for 1st anonymous parameter "
      ^ "to call `foo` but got `int`." ];

  (* Special Methods *)
  assert_strict_type_errors
    {|
      def foo(x: typing.Type[int]) -> str:
        return str(x)
    |}
    [];
  assert_strict_type_errors
    {|
      def foo(x: typing.Iterable[int]) -> int:
        return x[0]
    |}
    ["Undefined attribute [16]: `typing.Iterable` has no attribute `__getitem__`."];
  assert_strict_type_errors
    {|
      def foo(x: typing.Type[int], y: object) -> bool:
        return x == y
    |}
    [];
  assert_strict_type_errors
    {|
      # adding `or` to avoid triggering type alias validation
      x = typing.Mapping[int] or None
    |}
    [ "Missing global annotation [5]: Globally accessible variable `x` has no type specified.";
      "Incompatible parameter type [6]: Expected `typing.Tuple[typing.Type[Variable[typing._KT]], \
       typing.Type[Variable[typing._VT_co](covariant)]]` for 1st anonymous parameter to call \
       `typing.GenericMeta.__getitem__` but got `typing.Type[int]`." ];
  assert_strict_type_errors
    {|
      x = typing.Mapping[int, str]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Type[typing.Mapping[int, str]]`."];
  assert_strict_type_errors
    {|
      class Meta:
          def foo(self) -> None: ...

      class Foo(metaclass=Meta):
          def foo(self) -> None: ...

      reveal_type(Foo.foo)
      reveal_type(Foo().foo)
    |}
    [ "Revealed type [-1]: Revealed type for `Foo.foo` is "
      ^ "`typing.Callable(Foo.foo)[[Named(self, unknown)], None]`.";
      "Revealed type [-1]: Revealed type for `Foo().foo` is "
      ^ "`typing.Callable(Foo.foo)[[], None]`." ];
  assert_strict_type_errors
    {|
      class Meta:
          def __getitem__(self, item: int) -> int: ...

      class Foo(metaclass=Meta):
          def __getitem__(self, item: int) -> str: ...

      reveal_type(Foo[1])
      reveal_type(Foo()[1])
    |}
    [ "Revealed type [-1]: Revealed type for `Foo.__getitem__(1)` is `int`.";
      "Revealed type [-1]: Revealed type for `Foo().__getitem__(1)` is `str`." ];
  assert_strict_type_errors
    {|
      _T = typing.TypeVar('_T')

      class EnumMeta:
          def __getitem__(self: typing.Type[_T], name: str) -> _T: ...

      class Enum(metaclass=EnumMeta): ...

      # Definition in class str: def __getitem__(self, i: Union[int, slice]) -> str: ...

      class StringEnum(Enum, str): ...

      reveal_type(StringEnum["key"])

      class StringEnumTwo(str, Enum): ...

      reveal_type(StringEnumTwo["key"])
    |}
    [ "Invalid method signature [47]: `typing.Type[Variable[_T]]` cannot be the type of `self`.";
      "Revealed type [-1]: Revealed type for `StringEnum.__getitem__(\"key\")` is `StringEnum`.";
      "Revealed type [-1]: Revealed type for `StringEnumTwo.__getitem__(\"key\")` is \
       `StringEnumTwo`." ];

  (* Defining methods *)
  assert_type_errors
    {|
      class Foo:
        def bar(self) -> None:
          def baz(x: int) -> int:
            return x
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def bar(x: int) -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `self` parameter."];
  assert_type_errors
    {|
      class Foo:
        def bar(x: Foo) -> None:
          return
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(x: int) -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `cls` parameter."];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(x: typing.Type[Foo]) -> None:
          return
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def bar() -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `self` parameter."];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar() -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `cls` parameter."];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar() -> None:
          return
    |}
    [];
  assert_type_errors
    {|
    class Foo:
      @staticmethod
      def __new__(cls) -> typing.Type[Foo]: ...
    Foo()
  |}
    [];

  (* TODO(T45029821): Eliminate special casing so that calls to Foo() error here. *)
  assert_type_errors
    {|
    class Foo:
      @staticmethod
      def __new__() -> typing.Type[Foo]: ...
    Foo()
  |}
    [];
  assert_type_errors
    {|
      class Foo:
        def foo(self, x: typing.Optional[typing.Set[int]] = ...) -> None:
          self.x = x
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `x`."]


let test_check_abstract_methods context =
  let update_environment_with =
    [ {
        handle = "abc.pyi";
        (* This is just a mock stub of abc and is not meant to be accurate or complete *)
        source =
          {|
          from typing import Any
          def abstractmethod(funcobj: Any) -> Any: ...
          def abstractproperty(property: Any) -> Any: ...
        |};
      } ]
  in
  assert_type_errors
    ~context
    ~update_environment_with
    {|
      @abc.abstractmethod
      def abstract()->int:
        pass
    |}
    [];
  assert_type_errors
    ~context
    ~update_environment_with
    {|
      @abc.abstractproperty
      def abstract()->int:
        pass
    |}
    []


let test_check_behavioral_subtyping context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* Strengthened postcondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> int: ...
      class Bar(Foo):
        def foo(self) -> float: return 1.0
    |}
    [ "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `float` is not a subtype of the overridden return `int`." ];
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> float: ...
      class Bar(Foo):
        def foo(self) -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> int: ...
      class Bar(Foo):
        def foo(self) -> None: pass
    |}
    [ "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `None` is not a subtype of the overridden return `int`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Bar(Foo[float]):
        def foo(self) -> str: return ""
    |}
    [ "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `str` is not a subtype of the overridden return `float`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Bar(Foo[float]):
        def foo(self) -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo(self) -> str: return ""
    |}
    [ "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `str` is not a subtype of the overridden return `float`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo(self) -> int: return 1
    |}
    [];

  (* Missing annotations. *)
  assert_default_type_errors
    {|
      class Foo():
        def foo(self) -> int: ...
      class Bar(Foo):
        def foo(self): pass
    |}
    [ "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "The overriding method is not annotated but should return a subtype of `int`." ];

  (* Starred arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, *args: int) -> None: ...
      class D(C):
        def f(self, *args: int) -> None: ...
    |}
    [];

  (* Keyword arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, **kwargs: str) -> None: ...
      class D(C):
        def f(self, **kwargs: str) -> None: ...
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo(self, input: int) -> int: ...
      class Bar(Foo):
        def foo(self, input) -> int: ...
    |}
    ["Missing parameter annotation [2]: Parameter `input` has no type specified."];
  assert_type_errors
    {|
      T = typing.TypeVar("T", bound=int)
      class Foo():
        def foo(self, x: T) -> str:
          return ""
      class Bar(Foo[str]):
        def foo(self, x: str) -> str:
          return x
    |}
    [ "Invalid type parameters [24]: Non-generic type `Foo` cannot take parameters.";
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `str` is not a supertype of the overridden parameter "
      ^ "`Variable[T (bound to int)]`." ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class Foo(typing.Generic[T]):
        def foo(self) -> T: ...
      class Bar(Foo[int]):
        def foo(self) -> int:
          return 1
      class BarTwo(Foo[None]):
        def foo(self) -> None:
          pass
    |}
    [];
  assert_type_errors
    ~show_error_traces:true
    {|
      class Foo():
        def bar(self, x: int) -> int:
          return 1
      class Bar(Foo):
        def bar(self, x: int) -> typing.Union[str, int]:
          return 1
    |}
    [ "Inconsistent override [15]: `Bar.bar` overrides method defined in `Foo` "
      ^ "inconsistently. Returned type `typing.Union[int, str]` is not a subtype "
      ^ "of the overridden return `int`." ];

  (* Decorators are applied. *)
  assert_type_errors
    {|
      class Foo():
        @contextlib.contextmanager
        def foo(self) -> typing.Generator[int, None, None]: ...
      class Bar():
        @contextlib.contextmanager
        def foo(self) -> typing.Generator[int, None, None]: ...
    |}
    [];

  (* Weakened precondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: float) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `int` is not a supertype of the overridden parameter `float`." ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: ...
      class Bar(Foo):
        def foo(self) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature." ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, ) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: pass
      class Bar(Foo):
        def foo(self, b: int) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature." ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: pass
      class Bar(Foo):
        def foo(self, _a: int) -> None: pass
    |}
    [];
  assert_type_errors
    ~show_error_traces:true
    {|
      class Foo():
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
      class Bar(Foo):
        def bar(self, x: int) -> None:
          pass
    |}
    [ "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` "
      ^ "inconsistently. Parameter of type `int` is not a "
      ^ "supertype of the overridden parameter `typing.Union[int, str]`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [ "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `typing.Union[int, str]` is not a supertype "
      ^ "of the overridden parameter `typing.Union[float, str]`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [ "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `typing.Union[int, str]` is not a supertype "
      ^ "of the overridden parameter `typing.Union[float, str]`." ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];

  (* A leading underscore indicates parameters are unused; they should still be recognized *)
  assert_type_errors
    {|
      class Foo:
          def bar(self, _x: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, _x: int) -> str:
              return ""
      class Baz(Foo):
          def bar(self, _x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, x: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, _x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, _y: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, x: int) -> str:
              return ""
    |}
    [ "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` "
      ^ "inconsistently. Could not find parameter `_y` in overriding signature." ];

  (* Don't warn on constructors or class methods. *)
  assert_type_errors
    {|
      class Foo():
        def __init__(self, a: float) -> None: ...
      class Bar(Foo):
        def __init__(self, a: int) -> None: pass
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        @classmethod
        def foo(cls, a: float) -> None: ...
      class Bar(Foo):
        @classmethod
        def foo(cls, a: int) -> None: pass
    |}
    [];

  (* Don't warn on dunder methods. *)
  assert_type_errors
    {|
      class Foo():
        def __dunder__(self, a: float) -> None: ...
      class Bar(Foo):
        def __dunder__(self, a: int) -> None: pass
    |}
    [];

  (* Dunder methods must end with dunder. *)
  assert_type_errors
    {|
      class Foo():
        def __f(self, a: float) -> None: ...
      class Bar(Foo):
        def __f(self, a: int) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.__f` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `int` is not a supertype of the overridden parameter `float`." ];

  (* Weakening of object precondition is not possible. *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, o: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other: int) -> bool: ...
    |}
    [];

  (* Ensure that our preprocessing doesn't clobber starred argument names. *)
  assert_type_errors
    {|
      class Foo():
        def foo( **kwargs) -> int: ...
      class Bar(Foo):
        def foo( **kwargs) -> int: ...
    |}
    [];

  (* Ignore anything involving `Any`. *)
  assert_default_type_errors
    {|
      class Foo():
        def __eq__(self, o: typing.Any) -> typing.Any: ...
      class Bar(Foo):
        def __eq__(self, o: int) -> int: pass
    |}
    [];

  (* Overrides when both *args and **kwargs exist are not inconsistent. *)
  assert_default_type_errors
    {|
      class Foo():
        def f(self, a: float) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.f` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature." ];
  assert_default_type_errors
    {|
      class Foo():
        def f(self, b: int) -> None: ...
      class Bar(Foo):
        def f(self, **kwargs: typing.Any) -> None: pass
    |}
    [ "Inconsistent override [14]: `Bar.f` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `b` in overriding signature." ];
  assert_default_type_errors
    {|
      class Foo():
        def f(self, c: str) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any, **kwargs: typing.Any) -> None: pass
    |}
    []


let test_check_nested_class_inheritance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      def foo() -> Y.Q:
          return Y.Q()
    |}
    [];
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      def foo() -> Y.Q:
          return X.Q()
    |}
    [];
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      class Z():
          class Q():
              pass

      def foo() -> Y.Q:
          return Z.Q()
    |}
    ["Incompatible return type [7]: Expected `X.Q` but got `Z.Q`."];
  assert_type_errors
    {|
      class X:
        class N:
          class NN:
            class NNN:
              pass
      class Y(X):
        pass
      def foo() -> Y.N.NN.NNN:
          return Y.N.NN.NNN()
    |}
    [];
  assert_type_errors
    {|
      class B1:
        class N:
          pass
      class B2:
        class N:
          pass
      class C(B1, B2):
        pass
      def foo() -> C.N:
        return C.N()
    |}
    []


let test_check_method_resolution context =
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        bar().baz()
    |}
    [ "Undefined name [18]: Global name `bar` is not defined, or there is at least one control \
       flow path that doesn't define `bar`." ];
  assert_type_errors ~context {|
      def foo(input: str) -> None:
        input.lower()
    |} [];
  assert_type_errors
    ~context
    {|
      class Foo:
        def __getattr__(self, name: str) -> typing.Any: ...
      def foo(x: Foo) -> None:
        reveal_type(x.attribute)
        x.attribute + 1
    |}
    ["Revealed type [-1]: Revealed type for `x.attribute` is `typing.Any`."]


let test_check_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      x: int = 1
      x()
    |}
    ["Call error [29]: `int` is not a function."];
  assert_type_errors
    {|
      def foo() -> None: pass
      def bar() -> None: pass
      x = foo
      x()
      x = bar
      x()
    |}
    [ "Missing global annotation [5]: Globally accessible variable `x` "
      ^ "has type `typing.Callable[[], None]` but no type is specified." ];
  assert_type_errors
    {|
      def foo() -> None: pass
      def bar() -> None: pass
      test: bool
      if test:
        x = foo
      else:
        x = bar
      reveal_type(x)
      x()
    |}
    [ "Missing global annotation [5]: Globally accessible variable `x` "
      ^ "has type `typing.Callable[[], None]` but no type is specified.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[], None]`." ];
  assert_type_errors
    {|
      class A:
        def __init__(self) -> None: pass
      class B:
        def __init__(self) -> None: pass
      def foo(
        x: typing.Union[typing.Type[A], typing.Type[B]],
        y: typing.Type[A],
        z: typing.Type[B],
      ) -> None:
        x()
        y()
        z()
    |}
    [];
  assert_type_errors
    {|
      class A:
        def __init__(self) -> None: pass
      def foo() -> None: pass
      def bar(x: typing.Union[typing.Type[A], typing.Callable[[], None]]) -> None:
        x()
    |}
    []


let test_check_callable_protocols context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* Objects with a `__call__` method are callables. *)
  assert_type_errors
    {|
      class Call:
        def __call__(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class Call(typing.Generic[T]):
        def __call__(self) -> T: ...
      def foo(call: Call[int]) -> int:
        return call()
    |}
    [];

  (* We handle subclassing. *)
  assert_type_errors
    {|
      class BaseClass:
        def __call__(self, val: typing.Optional[str] = None) -> "BaseClass":
          ...
      class SubClass(BaseClass):
        pass
      def f(sc: SubClass) -> None:
        sc('foo')
    |}
    [];
  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [ "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Call error [29]: `Call` is not a function." ];
  assert_default_type_errors {|
      def foo(call) -> int:
        return call()
    |} [];

  (* Test for terminating fixpoint *)
  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(x: int, call: Call) -> int:
        for x in range(0, 7):
          call()
        return 7
    |}
    ["Call error [29]: `Call` is not a function."];
  assert_type_errors
    {|
      class patch:
        def __call__(self) -> int: ...

      unittest.mock.patch: patch = ...

      def foo() -> None:
        unittest.mock.patch()
        unittest.mock.patch()  # subequent calls should not modify annotation map
    |}
    ["Illegal annotation target [35]: Target `unittest.mock.patch` cannot be annotated."];
  assert_type_errors
    {|
      class Foo:
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [ "Incompatible return type [7]: Expected `None` but got "
      ^ "`typing.Callable(Foo.bar)[[Named(self, unknown), Named(x, int)], str]`." ];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [ "Incompatible return type [7]: Expected `None` but got "
      ^ "`typing.Callable(Foo.bar)[[Named(x, int)], str]`." ];
  assert_type_errors
    {|
      class Call:
        def __call__(self, x: int) -> int: ...
      def foo(call: Call) -> int:
        return call("")
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Call.__call__` but got `str`." ]


let test_check_explicit_method_call context =
  assert_type_errors
    ~context
    {|
      class Class:
        def method(self, i: int) -> None:
          pass
      Class.method(object(), 1)
    |}
    []


let test_check_self context =
  let assert_type_errors = assert_type_errors ~context in
  (* Self parameter is typed. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return 1
        def bar(self) -> str:
          return self.foo()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Other:
          pass

      class Some:
          def one(self) -> None:
              self.two()

          def two(self: Other) -> None:
              pass
    |}
    ["Invalid method signature [47]: `Other` cannot be the type of `self`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T, x: int) -> T:
          return self
      class Subclass(C):
        pass
      def f() -> C:
        a = Subclass()
        b = a.f
        return b(1)
      def f() -> Subclass:
        a = Subclass()
        b = a.f
        return b(1)
    |}
    [];

  (* Make sure the SelfType pattern works *)
  assert_type_errors
    {|
      TSelf = typing.TypeVar('TSelf', bound="C")
      class C:
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      SubTSelf = typing.TypeVar('SubTSelf', bound="Subclass")
      class Subclass(C):
        def subinner(self, x:str) -> None:
          pass
        def interface(self: SubTSelf, x: str) -> SubTSelf:
          self.inner(7)
          self.subinner(x)
          return self
      class SubSubclass(Subclass): pass
      def f() -> SubSubclass:
        return SubSubclass().verbose(7).interface("A")
      def g() -> SubSubclass:
        return SubSubclass().interface("A").verbose(7)
    |}
    [];

  (* Make sure the SelfType pattern works for generics *)
  assert_type_errors
    {|
      TSelf = typing.TypeVar('TSelf', bound="C")
      TG = typing.TypeVar('TG')
      class C:
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      class G(C, typing.Generic[TG]): pass
      def foo(x: G[int], y: G[str]) -> typing.Tuple[G[int], G[str]]:
        return (x.verbose(1), y.verbose(1))
    |}
    [];
  assert_type_errors
    {|
      TG = typing.TypeVar('TG')
      TSelf = typing.TypeVar('TSelf', bound="G")
      class G(typing.Generic[TG]):
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      class C(G[TG]):
        def outer(self) -> TG: ...
      def foo(x: C[int]) -> None:
        reveal_type(x.verbose(1).outer())
    |}
    [ "Invalid type parameters [24]: Generic type `G` expects 1 type parameter.";
      "Revealed type [-1]: Revealed type for `x.verbose(1).outer()` is `int`." ];
  ()


let test_check_meta_self context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_default_type_errors
    {|
      T = typing.TypeVar('T')
      S = typing.TypeVar('S')
      class C(typing.Generic[T]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(C[int], input)
      class D(typing.Generic[T, S]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(D[int, float], input)
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> C:
        return C.__construct__()
      def boo() -> Subclass:
        return Subclass.__construct__()
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo() -> C:
        return Subclass.__construct__()
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> Subclass:
        return C.__construct__()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(s: Subclass) -> Subclass:
        to_call = s.f
        return to_call()
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(c: C)-> Subclass:
        to_call = c.f
        return to_call()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> typing.Type[Foo]:
          return type(self)
        def bar(self) -> typing.Type[int]:
          return type(1)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        ATTRIBUTE: typing.ClassVar[int] = 1
        def foo(self) -> int:
          return type(self).ATTRIBUTE
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(t: T) -> str:
        return type(t).__name__
    |}
    [];
  assert_type_errors {|
      def foo(x: int) -> str:
        return type(x).__name__
    |} [];
  assert_type_errors
    {|
      class C:
        pass
      R = C
      def foo() -> C:
        return R()
    |}
    []


let test_check_static context =
  let assert_type_errors = assert_type_errors ~context in
  (* No self parameter in static method. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar(input: str) -> str:
          return input.lower()

      class Bar:
        @classmethod
        def bar(cls, input: str) -> str:
          return input.lower()

        def baz(self) -> None:
          self.bar("")
    |}
    [];

  (* Static method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`." ];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

        def bar(self) -> None:
          self.foo('asdf')

    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`." ];

  (* Class method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls, input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Foo.foo` but got `str`." ];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls) -> typing.Type[Foo]:
          return cls
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.classmethod('1234')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.classmethod` but got `str`." ];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def staticmethod(i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.staticmethod('1234')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.staticmethod` but got `str`." ];
  assert_type_errors
    {|
      class Foo:
        def instancemethod(self, i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.instancemethod(Foo(), '1234')
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.instancemethod` but got `str`." ];

  (* Special classmethods are treated properly without a decorator. *)
  assert_type_errors
    {|
      class Foo:
        def __init_subclass__(cls) -> typing.Type[Foo]:
          return cls
        def __new__(cls) -> typing.Type[Foo]:
          return cls
        def __class_getitem__(cls, key: int) -> typing.Type[Foo]:
          return cls
    |}
    []


let test_check_setitem context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x["foo"] = "bar"
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`." ];
  assert_type_errors
    {|
      class A:
        pass
      def foo(x: typing.Dict[str, int], y: A) -> None:
        x["foo"] = y["bar"] = "baz"
    |}
    [ "Undefined attribute [16]: `A` has no attribute `__setitem__`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`." ];
  assert_type_errors
    {|
      def foo(x: typing.Dict[str, typing.Dict[str, int]]) -> None:
        x["foo"]["bar"] = "baz"
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`." ];
  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x[7] = 7
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st anonymous parameter to call `dict.__setitem__` but got `int`." ]


let test_check_in context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class WeirdContains:
        def __contains__(self, x: int) -> int:
          ...
      reveal_type(1 in WeirdContains())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdContains()` is `int`."];
  assert_type_errors
    {|
      class WeirdIterator:
        def __eq__(self, other: object) -> str:
          ...
        def __iter__(self) -> typing.Iterator[WeirdIterator]:
          ...
      reveal_type(1 in WeirdIterator())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdIterator()` is `str`."];
  assert_type_errors
    {|
      class WeirdEqual:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class WeirdGetItem:
        def __getitem__(self, x: int) -> WeirdEqual:
          ...
      reveal_type(1 in WeirdGetItem())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdGetItem()` is `typing.List[int]`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __iter__(self, x: int) -> typing.Iterator[Equal]:
          ...
        def __contains__(self, a: object) -> bool:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in Multiple()` is `bool`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __getitem__(self, x: int) -> Equal:
          ...
        def __contains__(self, a: object) -> int:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in Multiple()` is `int`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItemA:
        def __getitem__(self, x: int) -> Equal:
          ...
      class GetItemB:
        def __getitem__(self, x: int) -> Equal:
          ...
      def foo(a: typing.Union[GetItemA, GetItemB]) -> None:
        5 in a
    |}
    []


let test_check_enter context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> str:
          return ''

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)
    |}
    [];
  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> int:
          return 5

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)

    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st anonymous parameter to call `expect_string` but got `int`." ]


let test_enforce_dunder_params context =
  assert_type_errors
    ~context
    {|
      def foo(__f: str) -> int:
        return 1

      def bar() -> None:
        foo("A")
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(__f: str) -> int:
        return 1

      def bar() -> None:
        foo(__f="A")
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `__f` to call `foo`."];
  assert_type_errors
    ~context
    {|
      def foo(__f__: str) -> int:
        return 1

      def bar() -> None:
        foo(__f__="A")
    |}
    [];
  ()


let () =
  "method"
  >::: [ "check_method_returns" >:: test_check_method_returns;
         "check_method_parameters" >:: test_check_method_parameters;
         "check_abstract_methods" >:: test_check_abstract_methods;
         "check_behavioral_subtyping" >:: test_check_behavioral_subtyping;
         "check_nested_class_inheritance" >:: test_check_nested_class_inheritance;
         "check_method_resolution" >:: test_check_method_resolution;
         "check_callables" >:: test_check_callables;
         "check_callable_protocols" >:: test_check_callable_protocols;
         "check_explicit_method_call" >:: test_check_explicit_method_call;
         "check_self" >:: test_check_self;
         "check_meta_self" >:: test_check_meta_self;
         "check_setitem" >:: test_check_setitem;
         "check_static" >:: test_check_static;
         "check_in" >:: test_check_in;
         "check_enter" >:: test_check_enter;
         "enforce_dunder_params" >:: test_enforce_dunder_params ]
  |> Test.run

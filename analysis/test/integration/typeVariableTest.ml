(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_unbounded_variables _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def expects_any(input) -> None: ...
      def expects_string(inut: str) -> None: ...
      def foo(input: T) -> None:
        expects_any(input)
        expects_string(input)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expects_string` but got `Variable[T]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> typing.Any:
        return input
    |}
    ["Missing return annotation [3]: Returning `Variable[T]` but type `Any` is specified."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> int:
        return input
    |}
    ["Incompatible return type [7]: Expected `int` but got `Variable[T]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def mapping_get(k: str, default: typing.Union[int, T]) -> typing.Union[int, T]: ...
      def foo() -> None:
        reveal_type(mapping_get("A", "A"))
        reveal_type(mapping_get("A", 7))
    |}
    [
      "Revealed type [-1]: Revealed type for `mapping_get.(...)` is " ^
      "`typing.Union[typing_extensions.Literal['A'], int]`.";
      "Revealed type [-1]: Revealed type for `mapping_get.(...)` is `int`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> None:
        input.impossible()
    |}
    ["Undefined attribute [16]: `Variable[T]` has no attribute `impossible`."];
  ()


let test_check_variable_bindings _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        str_to_int(t)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `str_to_int` but got " ^
     "`Variable[T (bound to int)]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo() -> T:
        return 1.0
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T (bound to int)]` isn't present \
       in the function's parameters.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        int_to_str(t)
      def bar(x: str) -> None:
        foo(x)
    |}
    ["Incompatible parameter type [6]: Expected `Variable[T (bound to int)]` for 1st anonymous " ^
     "parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      class C():
        def baz(self) -> int:
          return 7
      T = typing.TypeVar('T', bound=C)
      def foo(t: T) -> int:
        return t.baz()
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar

      T = TypeVar("T", bound=int)

      def f(x: T, y: int) -> T:
        return x

      def buggy(n: None) -> None:
        return f(2, n)
    |}
    [
      "Incompatible return type [7]: Expected `None` but got `int`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `f` but got `None`.";
    ];
  assert_type_errors
    {|
      class C: pass
      T = typing.TypeVar('T', bound=C)
      def foo(input: typing.Type[T]) -> T:
        v = input()
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `Variable[T (bound to C)]`."];
  assert_type_errors
    {|
      _T = typing.TypeVar("T", bound=int)
      class Foo:
        def foo(self, x: int) -> int:
          return x
      class Bar(Foo):
        def foo(self, x: _T) -> _T:
          return x
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar("T", bound=float)
      class Foo:
        def foo(self, x: int) -> int:
          return x
      class Bar(Foo):
        def foo(self, x: _T) -> _T:
          return x
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Returned type `Variable[_T (bound to float)]` is not a subtype of the overridden return " ^
      "`int`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar("T", bound=float)
      class Foo:
        def foo(self, x: _T) -> _T:
          return x
      class Bar(Foo):
        def foo(self, x: int) -> int:
          return x
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `int` is not a supertype of the overridden parameter " ^
      "`Variable[_T (bound to float)]`."
    ];
  assert_type_errors
    {|
      from typing import TypeVar

      _SelfT = TypeVar("SelfT", bound=C)
      class C():
          def clone(self: _SelfT) -> _SelfT: ...
          def foo(self: _SelfT) -> _SelfT:
              x = self.clone()
              reveal_type(x)
              return x

    |}
    ["Revealed type [-1]: Revealed type for `x` is `Variable[_SelfT (bound to C)]`."];
  assert_type_errors
    {|
      from typing import TypeVar, Type

      _SelfT = TypeVar("SelfT", bound=C)
      class C():
          @classmethod
          def clone(cls: Type[_SelfT]) -> _SelfT: ...
          @classmethod
          def foop(cls: Type[_SelfT]) -> _SelfT:
              x = cls.clone()
              reveal_type(x)
              return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Variable[_SelfT (bound to C)]`."];
  ()


let test_bottom_unbound_variables _ =
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
        def eat(self, x: T_Explicit) -> None:
          pass
      def bar() -> G[int]:
        g = G()
        reveal_type(g)
        g.eat(7)
        reveal_type(g)
        return g
    |}
    [
      "Revealed type [-1]: Revealed type for `g` is `G[]`.";
      "Revealed type [-1]: Revealed type for `g` is `G[int]`.";
    ]


let test_distinguish _ =
  assert_type_errors
    {|
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      class C(typing.Generic[_T1]):
        def pair(self, a: _T1, b: _T2) -> typing.Tuple[_T1, _T2]:
          return (a, b)
      def foo(q: C[_T2], x: _T2, y:_T1) -> typing.Tuple[_T2, _T1]:
        A = q.pair(x, y)
        reveal_type(A)
        return A
    |}
    [
      "Revealed type [-1]: Revealed type for `A` is `typing.Tuple[Variable[_T2], Variable[_T1]]`.";
    ];
  assert_type_errors
    {|
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      def foo(f: typing.Callable[[_T1], _T2], p: _T1) -> _T2:
        v = f(p)
        reveal_type(v)
        return v
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `Variable[_T2]`.";
    ];
  assert_type_errors
    {|
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      def foo(f: typing.Callable[[_T1], _T2], p: _T1) -> _T2:
        return f(1)
    |}
    [
      "Incompatible parameter type [6]: Expected `Variable[_T1]` for 1st anonymous parameter to " ^
      "anoynmous call but got `int`.";
    ];
  assert_type_errors
    {|
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      class B: pass
      class C(B): pass
      def foo(f: typing.Callable[[typing.List[typing.Tuple[_T1, B]]], _T2], p: _T1) -> _T2:
        v = f([(p, C())])
        reveal_type(v)
        return v
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `Variable[_T2]`.";
    ];
  assert_type_errors
    {|
      class C():
        def __init__(self, x: int) -> None:
          pass
      def foo() -> typing.Iterator[C]:
        v = map(C, [1, 2, 3])
        reveal_type(v)
        return v
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo() -> typing.Iterator[C[int]]:
        v = map(C, [1, 2, 3])
        reveal_type(v)
        return v
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C[int]]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo(x: typing.List[T]) -> typing.Iterator[C[T]]:
        v = map(C, x)
        reveal_type(v)
        return v
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C[Variable[T]]]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def foo(x: T) -> typing.List[T]:
        return [x]
      T1 = typing.TypeVar("T1")
      def bar(x: typing.Callable[[T1], T1]) -> None:
        pass
      def baz() -> None:
         bar(foo)
    |}
    [
      "Mutually recursive type variables [36]: Solving type variables for call `bar` " ^
      "led to infinite recursion"
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def foo(x: T) -> T:
        return x
      T1 = typing.TypeVar("T1")
      T2 = typing.TypeVar("T2")
      def bar(x: typing.Callable[[T1], T2], y: typing.Callable[[T2], T1]) -> None:
        pass
      def baz() -> None:
         bar(foo, foo)
    |}
    [
      "Mutually recursive type variables [36]: Solving type variables for call `bar` " ^
      "led to infinite recursion"
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x = collections.defaultdict(dict)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is " ^
      "`typing.DefaultDict[undefined, typing.Dict[]]`."
    ];
  ()



let () =
  "typeVariable">:::[
    "check_unbounded_variables">::test_check_unbounded_variables;
    "check_variable_bindings">::test_check_variable_bindings;
    "bottom_unbound_variables">::test_bottom_unbound_variables;
    "distinguish">::test_distinguish;
  ]
  |> Test.run

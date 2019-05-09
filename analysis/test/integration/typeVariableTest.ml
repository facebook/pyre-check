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
  assert_type_errors
    {|
      X = typing.TypeVar("X")
      class Foo(typing.Generic[X]): pass

      reveal_type(Foo[float])
      reveal_type(Foo[float]())
      reveal_type(Foo[str]())
      Foo["str"]()
    |}
    [
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...)` is `typing.Type[Foo[float]]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[float]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[str]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X]]` for 1st anonymous " ^
      "parameter to call `typing.Generic.__getitem__` but got `str`.";
    ];
  assert_type_errors
    {|
      X = typing.TypeVar("X")
      class Foo(typing.Generic[X]):
        def __init__(self, x: X) -> None: ...

      def one() -> Foo[int]:
        return Foo[int](1)
      def two() -> Foo[int]:
        return Foo[int](1.2)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call " ^
      "`Foo.__init__` but got `float`.";
    ];
  assert_type_errors
    {|
      from typing import overload, TypeVar, List, Callable, Tuple
      @overload
      def overloaded(x: int) -> str: ...
      @overload
      def overloaded(x: bool) -> float: ...
      @overload
      def overloaded(x: float) -> bool: ...
      @overload
      def overloaded(x: str) -> int: ...

      T1 = typing.TypeVar("T1")
      T2 = typing.TypeVar("T2")
      def generic(x: Callable[[T1], T2], y: List[T1], z: List[T2]) -> Tuple[T1, T2]: ...

      def foo() -> None:
        reveal_type(generic(overloaded, [1], ["1"]))
        reveal_type(generic(overloaded, [True], [1.0]))
        reveal_type(generic(overloaded, [1.0], [False]))
        reveal_type(generic(overloaded, ["1"], [7]))

        generic(overloaded, [1], [7])
    |}
    [
      "Revealed type [-1]: Revealed type for `generic.(...)` is `typing.Tuple[int, str]`.";
      "Revealed type [-1]: Revealed type for `generic.(...)` is `typing.Tuple[bool, float]`.";
      "Revealed type [-1]: Revealed type for `generic.(...)` is `typing.Tuple[float, bool]`.";
      "Revealed type [-1]: Revealed type for `generic.(...)` is `typing.Tuple[str, int]`.";
      "Incompatible parameter type [6]: Expected `List[Variable[T2]]` for 3rd anonymous " ^
      "parameter to call `generic` but got `List[int]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T, b: bool) -> typing.Optional[T]:
        x = None
        if b:
          x = input
        reveal_type(x)
        return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[Variable[T]]`."];
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
  assert_type_errors
    {|
      X = typing.TypeVar("X", bound=C)
      class Foo(typing.Generic[X]): pass
      class C(): pass
      class D(C): pass

      reveal_type(Foo[C])
      reveal_type(Foo[C]())
      reveal_type(Foo[D]())
      Foo[int]()
    |}
    [
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...)` is `typing.Type[Foo[C]]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[C]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[D]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X (bound to C)]]` for " ^
      "1st anonymous parameter to call `typing.Generic.__getitem__` but got `typing.Type[int]`.";
    ];
  assert_type_errors
    {|
      X = typing.TypeVar("X", Mineral, Animal)
      class Foo(typing.Generic[X]): pass
      class Mineral(): pass
      class Animal(): pass
      class Fish(Animal): pass

      reveal_type(Foo[Animal])
      reveal_type(Foo[Animal]())
      reveal_type(Foo[Mineral]())
      reveal_type(Foo[Fish]())
      Foo[int]()
    |}
    [
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...)` is " ^
      "`typing.Type[Foo[Animal]]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[Animal]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[Mineral]`.";
      "Revealed type [-1]: Revealed type for `Foo.__getitem__.(...).(...)` is `Foo[Animal]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X <: [Mineral, Animal]]]` " ^
      "for 1st anonymous parameter to call `typing.Generic.__getitem__` but got " ^
      "`typing.Type[int]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      class ConstrainedBase(typing.Generic[T]): pass
      class BadChild(ConstrainedBase[str]): pass
    |}
    [
      "Invalid type parameters [24]: Type parameter `str` violates constraints on " ^
      "`Variable[T (bound to int)]` in generic type `ConstrainedBase`";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      class ConstrainedBase(typing.Generic[T]): pass
      class AnyChild(ConstrainedBase[typing.Any]): pass
    |}
    [];
  ()


let test_unbound_variables _ =
  assert_type_errors
    {|
      def foo() -> None:
        x = []
    |}
    [
      "Incomplete Type [37]: Type `typing.List[Variable[_T]]` inferred for `x` is incomplete, " ^
      "add an explicit annotation.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.List[int] = []
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Sequence[int] = []
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: int = []
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as " ^
      "type `typing.List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Optional[typing.List[int]]
        x = []
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.List[int]`."];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[str, typing.List[int]] = { "A" : [] }
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[int, typing.List[int]] = { "A" : [] }
    |}
    [
      "Incompatible variable type [9]: x is declared to have type " ^
      "`typing.Dict[int, typing.List[int]]` but is used as type " ^
      "`typing.Dict[str, typing.List[Variable[_T]]]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.List[int]:
        return []
    |}
    [];
  assert_type_errors
    {|
      def bar(x: typing.List[int]) -> None:
        pass
      def foo() -> None:
        bar([])
    |}
    [];
  (* TODO(T42360946): Probably want a better error here *)
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def bar(x: typing.List[T]) -> T:
        return x[0]
      def foo() -> None:
        x = bar([])
    |}
    ["Incomplete Type [37]: Type inferred for `x` is incomplete, add an explicit annotation."];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[int]:
        return G()
    |}
    [];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[int]:
        g = G()
        reveal_type(g)
        return g
    |}
    [
      "Incomplete Type [37]: Type `G[Variable[T_Explicit <: [int, str]]]` inferred for `g` is " ^
      "incomplete, add an explicit annotation.";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
      "Incompatible return type [7]: Expected `G[int]` but got `G[typing.Any]`.";
    ];
  assert_type_errors
    ~debug:false
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[int]:
        g = G()
        reveal_type(g)
        return g
    |}
    ["Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`."];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[int]:
        g: G[int] = G()
        reveal_type(g)
        return g
    |}
    [
      "Revealed type [-1]: Revealed type for `g` is `G[int]`.";
    ];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[bool]:
        g: G[bool] = G()
        reveal_type(g)
        return g
    |}
    [
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on " ^
      "`Variable[T_Explicit <: [int, str]]` in generic type `G`";
      "Incompatible variable type [9]: g is declared to have type `G[typing.Any]` but is used " ^
      "as type `G[Variable[T_Explicit <: [int, str]]]`.";
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on " ^
      "`Variable[T_Explicit <: [int, str]]` in generic type `G`";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
    ];
  assert_type_errors
    ~debug:false
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[bool]:
        g: G[bool] = G()
        reveal_type(g)
        return g
    |}
    [
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on " ^
      "`Variable[T_Explicit <: [int, str]]` in generic type `G`";
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on " ^
      "`Variable[T_Explicit <: [int, str]]` in generic type `G`";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
    ];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      T = typing.TypeVar("T")
      class G(typing.Generic[T_Explicit, T]):
        def __init__(self) -> None:
          pass
      def bar(g: G[bool, bool]) -> None:
        reveal_type(g)
    |}
    [
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on " ^
      "`Variable[T_Explicit <: [int, str]]` in generic type `G`";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any, bool]`.";
    ];
  assert_type_errors
    {|
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
        def foo(self) -> int:
          return 7
      def bar() -> int:
        return G().foo()
    |}
    [
      "Incomplete Type [37]: Type `G[Variable[T_Explicit <: [int, str]]]` inferred for `G.(...)` " ^
      "is incomplete, so attribute `foo` cannot be accessed. Separate the expression into an " ^
      "assignment and give it an explicit annotation.";
    ];
  assert_type_errors
    {|
      def bar() -> None:
        for x in []:
          pass
    |}
    [
      "Incomplete Type [37]: Type `typing.List[Variable[_T]]` inferred for `[]` is incomplete, " ^
      "so attribute `__iter__` cannot be accessed. Separate the expression into an assignment " ^
      "and give it an explicit annotation.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[int, typing.Dict[int, str]] = collections.defaultdict(dict)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[int, str] = collections.defaultdict(dict)
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Dict[int, str]` " ^
      "but is used as type `typing.DefaultDict[Variable[collections._KT], " ^
      "typing.Dict[Variable[_T], Variable[_S]]]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[typing.List[int], typing.List[str]]:
        return [], []
    |}
    [];
  (* This could cause an infinite loop due to mismatching errors if we didn't make the error set
     namespace insensitive *)
  assert_type_errors
    {|
      def foo(x: int) -> None: pass
      def bar() -> None:
        for x in [1, 2, 3]:
          foo([])
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call `foo` " ^
      "but got `typing.List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      def bar(
          a: typing.Optional[typing.List[int]], b: typing.Optional[typing.List[str]]
      ) -> typing.Tuple[typing.List[int], typing.List[str]]:
         return a or [], b or []
    |}
    [];
  ()


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
      def bar(x: typing.Callable[[T1], T2], y: typing.Callable[[T2], T1]) -> typing.Tuple[T1, T2]:
         ...
      def baz() -> None:
         x = bar(foo, foo)
    |}
    [
      "Incomplete Type [37]: Type `typing.Tuple[Variable[T1], Variable[T1]]` inferred for `x" ^
      "` is incomplete, add an explicit annotation.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar("T")
      def identity(x: T) -> T:
        return x
      def f() -> None:
        reveal_type(map(identity, [1, 2, 3]))
    |}
    ["Revealed type [-1]: Revealed type for `map.(...)` is `typing.Iterator[int]`."];
  ()


let test_integer_variables _ =
  assert_type_errors
    {|
      T = pyre_check.extensions.IntVar("T")
      X = pyre_check.extensions.IntVar("X")
      def baz(x: X) -> X:
        return x
      def bop(x: int) -> None:
        pass
      def foo(x: T) -> T:
        y = x.__add__(5)
        z = baz(x)
        bop(x)
        return z
      def bar() -> None:
        x = foo(1)
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[1]`."];
  assert_type_errors
    {|
      X = pyre_check.extensions.IntVar("X")
      def baz(x: X) -> X:
        return x
      def bar(y: int) -> None:
        baz(y)
    |}
    [
      "Incompatible parameter type [6]: Expected `IntegerVariable[X]` for 1st anonymous " ^
      "parameter to call `baz` but got `int`.";
    ];
  ()



let () =
  "typeVariable">:::[
    "check_unbounded_variables">::test_check_unbounded_variables;
    "check_variable_bindings">::test_check_variable_bindings;
    "unbound_variables">::test_unbound_variables;
    "distinguish">::test_distinguish;
    "integer_variables">::test_integer_variables;
  ]
  |> Test.run

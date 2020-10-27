(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_bounded_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import TypeVar, Callable
      TFun = TypeVar("TFun", bound=Callable[[int], None])
      def foo(x: TFun) -> None:
        x(7)
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Callable
      TFun = TypeVar("TFun", bound=Callable[[int], None])
      def foo(x: TFun) -> None:
        x("7")
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to anonymous call but got `str`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Callable, Union
      T1 = TypeVar("T1", bound=Union[Callable[[], str], Callable[[], int]])
      def foo(x: T1) -> None:
        y = x()
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `Union[int, str]`."];
  assert_type_errors
    {|
      from typing import TypeVar, Callable, Union
      T1 = TypeVar("T1", bound=Union[Callable[[], str], Callable[[], str]])
      def foo(x: T1) -> None:
        y = x()
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `str`."];
  assert_type_errors
    {|
      from typing import TypeVar
      class CallableClass:
        def __call__(self, x:int) -> str:
          return "A"
      T2 = TypeVar("T2", bound=CallableClass)
      def foo(x: T2) -> None:
        y = x(5)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `str`."];
  assert_type_errors
    {|
      from typing import TypeVar
      class CallableClass:
        def __call__(self, x:int) -> str:
          return "A"
      T2 = TypeVar("T2", bound=CallableClass)
      def foo(x: T2) -> None:
        y = x(2)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `str`."];
  assert_type_errors
    {|
      from typing import Type, TypeVar
      class Constructable:
        def __init__(self, x:int) -> None:
          return
      T3 = TypeVar("T3", bound=Type[Constructable])
      def foo(x: T3) -> None:
        x(5)
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Generic, List
      S = TypeVar('S', bound=List[float])
      def bar(x: List[float]) -> None:
        pass
      def foo(x: S) -> S:
        bar(x)
        return x
    |}
    [];
  assert_type_errors
    {|
    from typing import TypeVar, Generic
    T = TypeVar('T', covariant=True)
    S = TypeVar('S', bound="Foo[float]")

    class Foo(Generic[T]):
        def a(self, x: S) -> S:
            return x

        def b(self, x: S) -> None:
            self.a(x)

    def foo(a: Foo[int]) -> Foo[float]:
        return a
    |}
    [];

  assert_type_errors
    {|
      from typing import TypeVar, List, Tuple, Optional, Callable
      T = TypeVar("T", int, str)
      def f(x: Callable[[T], None]) -> None:
        y = g(x)
      def g(x: Callable[[T], None]) -> None:
        ...
      |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, List, Tuple, Optional, Callable
      T = TypeVar("T", int, str)
      def f(x: Optional[Callable[[Optional[T]], None]]) -> None:
        y = g(x)
      def g(x: Optional[Callable[[Optional[T]], None]]) -> None:
        ...
      |}
    [];
  ()


let test_check_unbounded_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def expects_any(input: object) -> None: ...
      def expects_string(inut: str) -> None: ...
      def foo(input: T) -> None:
        expects_any(input)
        expects_string(input)
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st positional only parameter to call `expects_string` but got \
         `Variable[T]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(input: T) -> typing.Any:
        return input
    |}
    ["Missing return annotation [3]: Returning `Variable[T]` but type `Any` is specified."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(input: T) -> int:
        return input
    |}
    ["Incompatible return type [7]: Expected `int` but got `Variable[T]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def mapping_get(k: str, default: typing.Union[int, T]) -> typing.Union[int, T]: ...
      def foo() -> None:
        reveal_type(mapping_get("A", "A"))
        reveal_type(mapping_get("A", 7))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.mapping_get(\"A\", \"A\")` is "
      ^ "`typing.Union[typing_extensions.Literal['A'], int]`.";
      "Revealed type [-1]: Revealed type for `test.mapping_get(\"A\", 7)` is `int`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(input: T) -> None:
        input.impossible()
    |}
    ["Undefined attribute [16]: `Variable[T]` has no attribute `impossible`."];
  assert_type_errors
    {|
      import typing
      X = typing.TypeVar("X")
      class Foo(typing.Generic[X]): pass

      reveal_type(Foo[float])
      reveal_type(Foo[float]())
      reveal_type(Foo[str]())
      Foo["str"]()
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo[float]` is `typing.Type[Foo[float]]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[float]()` is `Foo[float]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[str]()` is `Foo[str]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X]]` for 1st positional \
       only "
      ^ "parameter to call `typing.GenericMeta.__getitem__` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      X = typing.TypeVar("X")
      class Foo(typing.Generic[X]):
        def __init__(self, x: X) -> None: ...

      def one() -> Foo[int]:
        return Foo[int](1)
      def two() -> Foo[int]:
        return Foo[int](1.2)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call "
      ^ "`Foo.__init__` but got `float`.";
    ];
  assert_type_errors
    {|
      from typing import overload, TypeVar, List, Callable, Tuple, Union
      @overload
      def overloaded(x: int) -> str: ...
      @overload
      def overloaded(x: bool) -> float: ...
      @overload
      def overloaded(x: float) -> bool: ...
      @overload
      def overloaded(x: str) -> int: ...
      def overloaded(x: Union[int, bool, float, str]) -> Union[int, bool, float, str]: ...

      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      def generic(x: Callable[[T1], T2], y: List[T1], z: List[T2]) -> Tuple[T1, T2]: ...

      def foo() -> None:
        reveal_type(generic(overloaded, [1], ["1"]))
        reveal_type(generic(overloaded, [True], [1.0]))
        reveal_type(generic(overloaded, [1.0], [False]))
        reveal_type(generic(overloaded, ["1"], [7]))

        generic(overloaded, [1], [7])
    |}
    [
      "Revealed type [-1]: Revealed type for `test.generic(test.overloaded, [1], [\"1\"])` is \
       `Tuple[int, str]`.";
      "Revealed type [-1]: Revealed type for `test.generic(test.overloaded, [True], [1.000000])` \
       is `Tuple[bool, float]`.";
      "Revealed type [-1]: Revealed type for `test.generic(test.overloaded, [1.000000], [False])` \
       is `Tuple[float, bool]`.";
      "Revealed type [-1]: Revealed type for `test.generic(test.overloaded, [\"1\"], [7])` is \
       `Tuple[str, int]`.";
      "Incompatible parameter type [6]: Expected `List[Variable[T2]]` for 3rd positional only "
      ^ "parameter to call `generic` but got `List[int]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(input: T, b: bool) -> typing.Optional[T]:
        x = None
        if b:
          x = input
        reveal_type(x)
        return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[Variable[T]]`."];
  assert_type_errors
    {|
      from typing import TypeVar, Generic, Optional
      T1 = TypeVar("T1")
      class Lol(Generic[T1]):
          def bar(self, x: Optional[T1]) -> None:
              if x is not None and self.bop(x):
                  return
          def bop(self, x: T1) -> bool:
              return True
    |}
    [];
  assert_type_errors
    {|
    from typing import TypeVar, Union, List
    T = TypeVar("T")
    def foo(x: Union[T, List[T]]) -> None: ...
    def bar(x: Union[T, List[T]]) -> None:
      foo(x)
    |}
    [];
  assert_type_errors
    {|
    from builtins import identity
    from typing import Union, Tuple
    SeparatedUnion = Union[
        Tuple[int, bool],
        Tuple[str, None],
    ]
    def foo(x: SeparatedUnion) -> SeparatedUnion:
      i = identity(x)
      reveal_type(i)
      return i
    |}
    ["Revealed type [-1]: Revealed type for `i` is `Union[Tuple[int, bool], Tuple[str, None]]`."];
  assert_type_errors
    {|
    from typing import Callable, TypeVar
    T = TypeVar("T")
    class CallMe:
      def __call__(self, x: int) -> str:
        return "A"
    def foo(f: Callable[[int], T]) -> T:
      return f(1)
    def bar() -> None:
      x = foo(CallMe())
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `str`."];

  (* Type variables in the nesting function is correctly captured *)
  assert_type_errors
    {|
     from typing import TypeVar, Callable
     T = TypeVar('T')
     def foo(x: T) -> Callable[[], T]:
         def bar() -> T:
           return x
         return bar
  |}
    [];
  (* Type variables in the parent class is correctly captured *)
  assert_type_errors
    {|
     from typing import TypeVar, Generic, Callable
     T = TypeVar('T')
     class A(Generic[T]):
       def foo(self, x: T) -> T:
         return x
  |}
    [];
  (* Type variables in the parent class of nesting function is correctly captured *)
  assert_type_errors
    {|
     from typing import TypeVar, Generic, Callable
     T = TypeVar('T')
     class A(Generic[T]):
       def foo(self, x: T) -> Callable[[T], int]:
         def bar(x: T) -> int:
           return 42
         return bar
  |}
    [];

  (* Correctly mark the boundness of nested function type variables when there're recursive calls *)
  assert_type_errors
    {|
    from typing import TypeVar, Dict, Any, Union
    def loads(obj: object) -> Dict[str, Any]: ...
    T = TypeVar('T')
    def foo() -> None:
      def bar(obj: T, *, top_level: bool = True) -> Union[str, T]:
        if isinstance(obj, dict):
          return "dict"
        else:
          loaded = loads(obj)
          modified = bar(loaded, top_level = False)
          return str(modified)
  |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, List, Generic
      T_bound_int = TypeVar('T_bound_int', bound=int)
      class G(Generic[T_bound_int]):
        pass
      T = TypeVar('T')
      def foo(a: G[List[T]]) -> T: ...
    |}
    [
      "Invalid type parameters [24]: Type parameter `List[Variable[T]]` violates constraints on \
       `Variable[T_bound_int (bound to int)]` in generic type `G`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, List, Generic
      T_Con = TypeVar('T_Con', contravariant=True)
      class G(Generic[T_Con]):
        pass
      def foo(a: G[str], b: G[int]) -> None:
        l: List[G[object]] = [a, b]
    |}
    [
      "Incompatible variable type [9]: l is declared to have type `List[G[object]]` but is used as \
       type `List[typing.Union[G[int], G[str]]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, Optional, TypeVar

      _T = TypeVar('_T')

      class ContextVar(Generic[_T]):
        def __init__(self, name: str, *, default: _T = ...) -> None: ...

      def foo() -> None:
        x: ContextVar[Optional[int]] = ContextVar[Optional[int]]("var1", default=None)
    |}
    [];
  ()


let test_check_variable_bindings context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from builtins import str_to_int
      import typing
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        str_to_int(t)
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st positional only parameter to call `str_to_int` but got "
      ^ "`Variable[T (bound to int)]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', bound=int)
      def foo() -> T:
        return 1.0
    |}
    [
      "Invalid type variable [34]: The type variable `Variable[T (bound to int)]` isn't present in \
       the function's parameters.";
      "Incompatible return type [7]: Expected `Variable[T (bound to int)]` but got `float`.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_str
      import typing
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        int_to_str(t)
      def bar(x: str) -> None:
        foo(x)
    |}
    [
      "Incompatible parameter type [6]: Expected `Variable[T (bound to int)]` for 1st positional \
       only "
      ^ "parameter to call `foo` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
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
      "Incompatible parameter type [6]: Expected `int` for 2nd positional only parameter to call \
       `f` but got `None`.";
    ];
  assert_type_errors
    {|
      import typing
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
      import typing
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
      import typing
      _T = typing.TypeVar("T", bound=float)
      class Foo:
        def foo(self, x: int) -> int:
          return x
      class Bar(Foo):
        def foo(self, x: _T) -> _T:
          return x
    |}
    [
      "Inconsistent override [15]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `Variable[_T (bound to float)]` is not a subtype of the overridden return "
      ^ "`int`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar("T", bound=float)
      class Foo:
        def foo(self, x: _T) -> _T:
          return x
      class Bar(Foo):
        def foo(self, x: int) -> int:
          return x
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `int` is not a supertype of the overridden parameter "
      ^ "`Variable[_T (bound to float)]`.";
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
      import typing
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
      "Revealed type [-1]: Revealed type for `test.Foo[test.C]` is `typing.Type[Foo[C]]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[test.C]()` is `Foo[C]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[test.D]()` is `Foo[D]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X (bound to C)]]` for "
      ^ "1st positional only parameter to call `typing.GenericMeta.__getitem__` but got \
         `typing.Type[int]`.";
    ];
  assert_type_errors
    {|
      import typing
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
      "Revealed type [-1]: Revealed type for `test.Foo[test.Animal]` is "
      ^ "`typing.Type[Foo[Animal]]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[test.Animal]()` is `Foo[Animal]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[test.Mineral]()` is `Foo[Mineral]`.";
      "Revealed type [-1]: Revealed type for `test.Foo[test.Fish]()` is `Foo[Animal]`.";
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[X <: [Mineral, Animal]]]` "
      ^ "for 1st positional only parameter to call `typing.GenericMeta.__getitem__` but got "
      ^ "`typing.Type[int]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', bound=int)
      class ConstrainedBase(typing.Generic[T]): pass
      class BadChild(ConstrainedBase[str]): pass
    |}
    [
      "Invalid type parameters [24]: Type parameter `str` violates constraints on "
      ^ "`Variable[T (bound to int)]` in generic type `ConstrainedBase`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', bound=int)
      class ConstrainedBase(typing.Generic[T]): pass
      class AnyChild(ConstrainedBase[typing.Any]): pass
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      T = TypeVar('T', bound="G")
      class G(Generic[T]):
        pass
    |}
    ["Invalid type parameters [24]: Generic type `G` expects 1 type parameter."];
  ()


let test_unbound_variables context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        x = []
    |}
    [
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `x` is incomplete, "
      ^ "add an explicit annotation.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.List[int] = []
    |}
    [];
  assert_type_errors
    {|
      import typing
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
      "Incompatible variable type [9]: x is declared to have type `int` but is used as "
      ^ "type `typing.List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Optional[typing.List[int]]
        x = []
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Optional[typing.List[int]]` (inferred: \
       `typing.List[int]`).";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[str, typing.List[int]] = { "A" : [] }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.List[int] = {}
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.List[int]` but is used \
       as type `typing.Dict[Variable[_KT], Variable[_VT]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[int, str] = []
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Dict[int, str]` but is \
       used as type `typing.List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[int, typing.List[int]] = { "A" : [] }
    |}
    [
      "Incompatible variable type [9]: x is declared to have type "
      ^ "`typing.Dict[int, typing.List[int]]` but is used as type "
      ^ "`typing.Dict[str, typing.List[int]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[int]:
        return []
    |}
    [];
  assert_type_errors
    {|
      import typing
      def bar(x: typing.List[int]) -> None:
        pass
      def foo() -> None:
        bar([])
    |}
    [];

  (* TODO(T42360946): Probably want a better error here *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def bar(x: typing.List[T]) -> T:
        return x[0]
      def foo() -> None:
        x = bar([])
    |}
    ["Incomplete type [37]: Type inferred for `x` is incomplete, add an explicit annotation."];
  assert_type_errors
    {|
      import typing
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
      import typing
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
      "Incomplete type [37]: Type `G[Variable[T_Explicit <: [int, str]]]` inferred for `g` is "
      ^ "incomplete, add an explicit annotation.";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
    ];
  assert_default_type_errors
    {|
      import typing
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
      import typing
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      class G(typing.Generic[T_Explicit]):
        def __init__(self) -> None:
          pass
      def bar() -> G[int]:
        g: G[int] = G()
        reveal_type(g)
        return g
    |}
    ["Revealed type [-1]: Revealed type for `g` is `G[int]`."];
  assert_type_errors
    {|
      import typing
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
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on "
      ^ "`Variable[T_Explicit <: [int, str]]` in generic type `G`.";
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on "
      ^ "`Variable[T_Explicit <: [int, str]]` in generic type `G`.";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
    ];
  assert_default_type_errors
    {|
      import typing
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
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on "
      ^ "`Variable[T_Explicit <: [int, str]]` in generic type `G`.";
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on "
      ^ "`Variable[T_Explicit <: [int, str]]` in generic type `G`.";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any]`.";
    ];
  assert_type_errors
    {|
      import typing
      T_Explicit = typing.TypeVar("T_Explicit", int, str)
      T = typing.TypeVar("T")
      class G(typing.Generic[T_Explicit, T]):
        def __init__(self) -> None:
          pass
      def bar(g: G[bool, bool]) -> None:
        reveal_type(g)
    |}
    [
      "Invalid type parameters [24]: Type parameter `bool` violates constraints on "
      ^ "`Variable[T_Explicit <: [int, str]]` in generic type `G`.";
      "Revealed type [-1]: Revealed type for `g` is `G[typing.Any, bool]`.";
    ];
  assert_type_errors
    {|
      import typing
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
      "Incomplete type [37]: Type `G[Variable[T_Explicit <: [int, str]]]` inferred for `test.G()` "
      ^ "is incomplete, so attribute `foo` cannot be accessed. Separate the expression into an "
      ^ "assignment and give it an explicit annotation.";
    ];
  assert_type_errors
    {|
      def bar() -> None:
        for x in []:
          pass
    |}
    [
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `[]` is incomplete, so \
       attribute `__iter__` cannot be accessed. Separate the expression into an assignment and \
       give it an explicit annotation.";
    ];
  assert_type_errors
    {|
      import typing
      import collections
      def foo() -> None:
        x: typing.Dict[int, typing.Dict[int, str]] = collections.defaultdict(dict)
    |}
    [];
  assert_type_errors
    {|
      import typing
      import collections
      def foo() -> None:
        x: typing.Dict[int, str] = collections.defaultdict(dict)
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Dict[int, str]` "
      ^ "but is used as type `typing.DefaultDict[Variable[collections._KT], "
      ^ "typing.Dict[Variable[_T], Variable[_S]]]`.";
    ];
  assert_type_errors
    {|
      import typing
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
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `foo` "
      ^ "but got `typing.List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def bar(
          a: typing.Optional[typing.List[int]], b: typing.Optional[typing.List[str]]
      ) -> typing.Tuple[typing.List[int], typing.List[str]]:
         return a or [], b or []
    |}
    [];
  assert_type_errors
    {|
      from typing import Generic, TypeVar, Any
      T = TypeVar('T')
      class G(Generic[T]):
        prop: T
        def __init__(self, prop: T) -> None:
          self.prop = prop
      class C(G[int]):
        def foo(self) -> None:
          reveal_type(self.prop)
    |}
    ["Revealed type [-1]: Revealed type for `self.prop` is `int`."];
  ()


let test_distinguish context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
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
    ["Revealed type [-1]: Revealed type for `A` is `typing.Tuple[Variable[_T2], Variable[_T1]]`."];
  assert_type_errors
    {|
      import typing
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      def foo(f: typing.Callable[[_T1], _T2], p: _T1) -> _T2:
        v = f(p)
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `Variable[_T2]`."];
  assert_type_errors
    {|
      import typing
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      def foo(f: typing.Callable[[_T1], _T2], p: _T1) -> _T2:
        return f(1)
    |}
    [
      "Incompatible parameter type [6]: Expected `Variable[_T1]` for 1st positional only parameter \
       to "
      ^ "anonymous call but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      _T1 = typing.TypeVar("_T1")
      _T2 = typing.TypeVar("_T2")
      class B: pass
      class C(B): pass
      def foo(f: typing.Callable[[typing.List[typing.Tuple[_T1, B]]], _T2], p: _T1) -> _T2:
        v = f([(p, C())])
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `Variable[_T2]`."];
  assert_type_errors
    {|
      import typing
      class C():
        def __init__(self, x: int) -> None:
          pass
      def foo() -> typing.Iterator[C]:
        v = map(C, [1, 2, 3])
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo() -> typing.Iterator[C[int]]:
        v = map(C, [1, 2, 3])
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C[int]]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class C(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo(x: typing.List[T]) -> typing.Iterator[C[T]]:
        v = map(C, x)
        reveal_type(v)
        return v
    |}
    ["Revealed type [-1]: Revealed type for `v` is `typing.Iterator[C[Variable[T]]]`."];
  assert_type_errors
    {|
      import typing
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
      "Mutually recursive type variables [36]: Solving type variables for call `bar` "
      ^ "led to infinite recursion.";
    ];
  assert_type_errors
    {|
      import typing
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
      "Incomplete type [37]: Type `typing.Tuple[Variable[T1], Variable[T1]]` inferred for `x"
      ^ "` is incomplete, add an explicit annotation.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def identity(x: T) -> T:
        return x
      def f() -> None:
        reveal_type(map(identity, [1, 2, 3]))
    |}
    [
      "Revealed type [-1]: Revealed type for `map(test.identity, [1, 2, 3])` is \
       `typing.Iterator[int]`.";
    ];
  ()


let test_integer_variables context =
  assert_type_errors
    ~context
    {|
      import typing_extensions
      T = typing_extensions.IntVar("T")
      X = typing_extensions.IntVar("X")
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
    ~context
    {|
      import typing_extensions
      X = typing_extensions.IntVar("X")
      def baz(x: X) -> X:
        return x
      def bar(y: int) -> None:
        baz(y)
    |}
    [
      "Incompatible parameter type [6]: Expected `IntegerVariable[X]` for 1st positional only "
      ^ "parameter to call `baz` but got `int`.";
    ];
  ()


let test_nested_variable_error context =
  assert_type_errors
    ~context
    {|
      import typing
      T1 = typing.TypeVar("T1")
      T2 = typing.TypeVar("T2", typing.List[T1], typing.Dict[str, T1])
    |}
    [
      "Invalid type [31]: Expression `Variable[T2 <: [typing.List[Variable[test.T1]], "
      ^ "typing.Dict[str, Variable[test.T1]]]]` is not a valid type. Type variables cannot contain "
      ^ "other type variables in their constraints.";
    ];
  ()


let test_single_explicit_error context =
  assert_type_errors
    ~context
    {|
      import typing
      T1 = typing.TypeVar("T1", int)
    |}
    [
      "Invalid type [31]: TypeVar can't have a single explicit constraint. Did you mean `bound=int`?";
    ];
  ()


let test_callable_parameter_variadics context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Callable, List
      import pyre_extensions
      V = pyre_extensions.ParameterSpecification("V")
      def f(x: Callable[V, int]) -> Callable[V, List[int]]: ...
      def foo(x: int) -> int:
        return 7
      def bar(x: int, y: str) -> int:
        return 7
      def g() -> None:
         reveal_type(f(foo))
         reveal_type(f(bar))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.f(test.foo)` is `typing.Callable[[Named(x, \
       int)], "
      ^ "List[int]]`.";
      "Revealed type [-1]: Revealed type for `test.f(test.bar)` is `typing.Callable[[Named(x, \
       int), "
      ^ "Named(y, str)], List[int]]`.";
    ];
  assert_type_errors
    {|
      import typing
      import pyre_extensions
      V = pyre_extensions.ParameterSpecification("V")
      class Propagating(typing.List[typing.Callable[V, int]]):
         def foo(self) -> int: ...
    |}
    [];
  assert_type_errors
    ~handle:"qualifier.py"
    {|
      from typing import Callable, List
      from pyre_extensions import ParameterSpecification
      from pyre_extensions.type_variable_operators import PositionalArgumentsOf, KeywordArgumentsOf
      V = ParameterSpecification("V")
      def f(x: Callable[V, int]) -> Callable[V, List[int]]:
        def decorated( *args: V.args, **kwargs: V.kwargs) -> List[int]:
          return [x( *args, **kwargs)]
        return decorated
    |}
    [];
  assert_type_errors
    {|
     from typing import Callable
     from pyre_extensions import ParameterSpecification

     TParams = ParameterSpecification("TParams")
     def eek(x: Callable[TParams, int]) -> Callable[TParams, float]:
         return x
    |}
    [];
  assert_type_errors
    {|
      from typing import Protocol, Callable, TypeVar
      import pyre_extensions
      TParams = pyre_extensions.ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def call_this_function(__f: Callable[TParams, TReturn], *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
        return __f( *args, **kwargs)
      def int_to_string(i: int) -> str:
        return "A"
      def foo() -> None:
        x = call_this_function(int_to_string, 1)
        reveal_type(x)
        y = call_this_function(int_to_string, i=1)
        reveal_type(y)
        call_this_function(int_to_string, "A")
        call_this_function(int_to_string, i="A")
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd positional only parameter to call \
       `call_this_function` but got `str`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd parameter `i` to call \
       `call_this_function` but got `str`.";
    ];
  (* Interaction with overloads *)
  assert_type_errors
    {|
      from typing import Protocol, Callable, TypeVar, overload, Union
      import pyre_extensions
      TParams = pyre_extensions.ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def call_this_function(__f: Callable[TParams, TReturn], *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
        return __f( *args, **kwargs)

      @overload
      def overloaded(x: int) -> str:...
      @overload
      def overloaded(x: str) -> int:...
      def overloaded(x: Union[int, str]) -> Union[int, str]:
        if isinstance(x, int):
          return "A"
        else:
          return 1

      def foo() -> None:
        x = call_this_function(overloaded, 1)
        reveal_type(x)
        y = call_this_function(overloaded, "A")
        reveal_type(y)
        call_this_function(overloaded, 1.0)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Incompatible parameter type [6]: Expected `int` for 2nd positional only parameter to call \
       `call_this_function` but got `float`.";
    ];
  (* Example from PEP *)
  assert_type_errors
    {|
      from typing import Protocol, Callable, TypeVar
      import pyre_extensions
      TParams = pyre_extensions.ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def call_n_times(
          __f: Callable[TParams, None],
          __n: int,
          *args: TParams.args,
          **kwargs: TParams.kwargs,
      ) -> None:
          for x in range(__n):
              __f( *args, **kwargs)
      def valid(x: int, y: str) -> None: ...
      def invalid(x: int, y: str) -> int: ...
      def foo() -> None:
        call_n_times(valid, 75, 1, "A")
        call_n_times(valid, 75, y="A", 1)
        # invalid first argument
        call_n_times(invalid, 75, 1, "A")
        # missing second argument
        call_n_times(valid, y="A", x=1)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing.Callable[test.TParams, None]` for 1st \
       positional only parameter to call `call_n_times` but got \
       `typing.Callable(invalid)[[Named(x, int), Named(y, str)], int]`.";
      "Missing argument [20]: Call `call_n_times` expects argument in position 1.";
    ];
  (* Decorator to supply an argument to a method. *)
  assert_type_errors
    {|
      from typing import *
      from pyre_extensions import ParameterSpecification
      from pyre_extensions.type_variable_operators import Concatenate

      P = ParameterSpecification("P")
      R = TypeVar("R")

      class Client: ...

      def with_client(
        f: Callable[Concatenate["Foo", Client, P], R]
      ) -> Callable[Concatenate["Foo", P], R]:
        def inner(__self: "Foo", *args: P.args, **kwargs: P.kwargs) -> R:
          return f(__self, Client(), *args, **kwargs)
        return inner

      class Foo:
        @with_client
        def takes_int_str(self, client: Client, x: int, y: str) -> int:
          # Use `client` here.

          return x + 7

      reveal_type(with_client)
      x: Foo
      reveal_type(x.takes_int_str)

      x.takes_int_str(1, "A") # Accepted
      x.takes_int_str("B", 2) # Correctly rejected by the type checker
    |}
    [
      "Revealed type [-1]: Revealed type for `test.with_client` is \
       `typing.Callable(with_client)[[Named(f, \
       typing.Callable[pyre_extensions.type_variable_operators.Concatenate[Foo, Client, test.P], \
       Variable[R]])], typing.Callable[pyre_extensions.type_variable_operators.Concatenate[Foo, \
       test.P], Variable[R]]]`.";
      "Revealed type [-1]: Revealed type for `x.takes_int_str` is \
       `BoundMethod[typing.Callable[[Foo, Named(x, int), Named(y, str)], int], Foo]`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to \
       anonymous call but got `str`.";
    ];
  (* PyTorch style delegation pattern *)
  assert_type_errors
    {|
      from abc import ABCMeta
      from typing import Protocol, Callable, TypeVar
      import pyre_extensions
      TParams = pyre_extensions.ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      class HasForward(Protocol[TParams, TReturn]):
        forward: Callable[TParams, TReturn]

      class Model(metaclass=ABCMeta):
        forward: Callable[..., object]

        def __call__(__self: HasForward[TParams, TReturn], *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
          # do some common stuff
          return_value = __self.forward( *args, **kwargs)
          # do some more stuff
          return return_value

      class AModel(Model):
         def forward(self, x: int, y: str) -> bool:
           ...

      class BModel(Model):
         def forward(self, x: bool, *args: int) -> str:
           ...

      def foo() -> None:
        # Correct usages
        x = AModel()(1, "A")
        reveal_type(x)
        y = AModel()(y="A", x=5)
        reveal_type(y)
        # Incorrect second argument
        AModel()(1, 1)

        # Different model
        z = BModel()(True, 1, 4, 5)
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `bool`.";
      "Revealed type [-1]: Revealed type for `y` is `bool`.";
      "Incompatible parameter type [6]: Expected `str` for 2nd positional only parameter to call \
       `Model.__call__` but got `int`.";
      "Revealed type [-1]: Revealed type for `z` is `str`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import Generic, TypeVar
      T = TypeVar("T")
      P = ParameterSpecification("P")
      class H(Generic[T, P]):
        def f(self, /, *args: P.args, **kwargs: P.kwargs) -> T: ...

      def foo(x: H[bool, [int, str]]) -> None:
        reveal_type(x.f.__call__)

        # incorrect
        x.f()
        x.f("A", 1)

        # correct
        z = x.f(1, "A")
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x.f.__call__` is `typing.Callable[[int, str], bool]`.";
      "Missing argument [20]: Call `H.f` expects argument in position 1.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `H.f` but got `str`.";
      "Revealed type [-1]: Revealed type for `z` is `bool`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import Generic
      P = ParameterSpecification("P")
      class H(Generic[P]):
        def f(self, /, *args: P.args, **kwargs: P.kwargs) -> int:
          return 5

      def foo(x: H[int, str]) -> None:
        reveal_type(x.f.__call__)

        # incorrect
        x.f()
        x.f("A", 1)

        # correct
        x.f(1, "A")
    |}
    [
      "Revealed type [-1]: Revealed type for `x.f.__call__` is `typing.Callable[[int, str], int]`.";
      "Missing argument [20]: Call `H.f` expects argument in position 1.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `H.f` but got `str`.";
    ];

  assert_type_errors
    {|
      from typing import Callable
      import pyre_extensions

      TParams = pyre_extensions.ParameterSpecification("TParams")
      def outer(f: Callable[TParams, int]) -> None:
        def foo(x: int, *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          pass
        def bar(__x: int, *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          pass
        def baz(x: int, /, *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          pass
        reveal_type(foo)
        reveal_type(bar)
        reveal_type(baz)
    |}
    [
      "Revealed type [-1]: Revealed type for `foo` is \
       `typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, test.TParams], \
       None]`.";
      "Revealed type [-1]: Revealed type for `bar` is \
       `typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, test.TParams], \
       None]`.";
      "Revealed type [-1]: Revealed type for `baz` is \
       `typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, test.TParams], \
       None]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable
      import pyre_extensions

      TParams = pyre_extensions.ParameterSpecification("TParams")
      def outer(f: Callable[TParams, int]) -> Callable[TParams, None]:
        def foo(x: int, *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          f( *args, **kwargs)
        def bar( *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          foo(1, *args, **kwargs) # Accepted
          foo(x=1, *args, **kwargs) # Rejected
        return bar
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `x` to anonymous call."];
  assert_type_errors
    {|
      from typing import Protocol, Callable, TypeVar, overload, Union
      import pyre_extensions
      TParams = pyre_extensions.ParameterSpecification("TParams")

      def doesnt_care_positional( *args: object) -> None:
        pass

      def doesnt_care_keywords( **kwargs: object) -> None:
        pass

      def does_care_positional( *args: int) -> None:
        pass

      def does_care_keywords( **kwargs: int) -> None:
        pass

      def outer(f: Callable[TParams, int]) -> Callable[TParams, None]:
        def foo( *args: TParams.args, **kwargs: TParams.kwargs) -> None:
          doesnt_care_positional( *args)
          doesnt_care_keywords( **kwargs)

          does_care_positional( *args)
          does_care_keywords( **kwargs)


          f( *args, **kwargs)
        return foo
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `does_care_positional` but got `object`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `does_care_keywords` but got `object`.";
    ];
  ()


let test_list_variadics context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
    from typing import Tuple
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def duple(x: Tuple[Ts]) -> Tuple[Tuple[Ts], Tuple[Ts]]:
      return x, x
    def foo(x: int, y: str) -> None:
      reveal_type(duple((x, y)))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.duple((x, y))` is `Tuple[Tuple[int, str], \
       Tuple[int, str]]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def duple(x: Optional[Tuple[Ts]] = None) -> Tuple[Ts]: ...
    def foo() -> Tuple[int, str, bool]:
      x = duple()
      reveal_type(x)
      return x
    |}
    [
      "Incomplete type [37]: Type `typing.Tuple[test.Ts]` inferred for `x` is incomplete, add an \
       explicit annotation.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Tuple[...]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def duple(x: Optional[Tuple[Ts]] = None) -> Tuple[Ts]: ...
    def foo() -> Tuple[int, str, bool]:
      x: Tuple[int, str, bool] = duple()
      reveal_type(x)
      return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
    from typing import List
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def bad(x: List[Ts]) -> None:
      pass
    |}
    [
      "Invalid type parameters [24]: Single type parameter `Variable[_T]` expected, but a type \
       parameter group `[test.Ts]` was given for generic type list.";
    ];
  assert_type_errors
    {|
     from typing import Dict
     import pyre_extensions
     Ts = pyre_extensions.ListVariadic("Ts")
     def bad(x: Dict[Ts]) -> None:
       pass
     |}
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, received 1, \
       use `typing.Dict` to avoid runtime subscripting errors.";
    ];

  assert_type_errors
    {|
    from typing import Tuple, Optional
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def strip_first(x: Tuple[object, Ts]) -> Tuple[Ts]: ...
    def foo() -> None:
      x = strip_first((1,2,3))
      reveal_type(x)
    |}
    [
      "Invalid type [31]: Expression `typing.Tuple[(object, $local_test$Ts)]` is not a valid type.";
      "Invalid type variable [34]: The type variable `Ts` isn't present in the function's \
       parameters.";
      "Incomplete type [37]: Type `typing.Tuple[test.Ts]` inferred for `x` is incomplete, add an \
       explicit annotation.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Tuple[...]`.";
    ];
  assert_type_errors
    {|
    from typing import Callable, Tuple
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def tuple_to_callable(x: Tuple[Ts]) -> Callable[Ts, int]: ...
    def foo(x: int, y: str, z: bool) -> None:
      reveal_type(tuple_to_callable((x, y, z)))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.tuple_to_callable((x, y, z))` is \
       `typing.Callable[[int, str, bool], int]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def tuple_to_callable(x: Optional[Tuple[Ts]] = None) -> Callable[Ts, int]: ...
    def foo() -> Callable[[int, str, bool], int]:
      f = tuple_to_callable()
      reveal_type(f)
      return f
    |}
    [
      "Incomplete type [37]: Type `typing.Callable[[Variable(test.Ts)], int]` inferred for `f` is \
       incomplete, add an explicit annotation.";
      "Revealed type [-1]: Revealed type for `f` is `typing.Callable[[Variable(typing.Any)], int]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def tuple_to_callable(x: Optional[Tuple[Ts]] = None) -> Callable[Ts, int]: ...
    def foo() -> Callable[[int, str, bool], int]:
      f: Callable[[int, str, bool], int] = tuple_to_callable()
      reveal_type(f)
      return f
    |}
    ["Revealed type [-1]: Revealed type for `f` is `typing.Callable[[int, str, bool], int]`."];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def callable_to_tuple(f: Callable[Ts, int]) -> Tuple[Ts]: ...
    def bar(x: int, y: str, z: bool) -> int:
      return 7
    def foo() -> Tuple[int, str, bool]:
      t = callable_to_tuple(bar)
      reveal_type(t)
      return t
    |}
    ["Revealed type [-1]: Revealed type for `t` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def two_callables_to_tuple(f1: Callable[Ts, int], f2: Callable[Ts, int]) -> Tuple[Ts]: ...
    def bar(x: int, y: str, z: bool) -> int:
      return 7
    def barrel(a: float, b: str, c: bool) -> int:
      return 7
    def foo() -> Tuple[int, str, bool]:
      t = two_callables_to_tuple(barrel, bar)
      reveal_type(t)
      return t
    |}
    ["Revealed type [-1]: Revealed type for `t` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable, TypeVar
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def call_with_tuple(f: Callable[Ts, TReturn], tupleargs: Tuple[Ts]) -> TReturn:
      return f( *tupleargs)
    def foo(x: int, y: str, z: bool) -> str: ...
    def bar(x: bool, y: int, z: float) -> int: ...
    def use() -> None:
      reveal_type(call_with_tuple(foo, (1, "A", False)))
      reveal_type(call_with_tuple(bar, (True, 19, 37)))
      call_with_tuple(bar, (True, 19.5, 37))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.call_with_tuple(test.foo, (1, \"A\", False))` \
       is `str`.";
      "Revealed type [-1]: Revealed type for `test.call_with_tuple(test.bar, (True, 19, 37))` is \
       `int`.";
      "Incompatible parameter type [6]: Expected `typing.Tuple[test.Ts]` for 2nd positional only \
       parameter to call `call_with_tuple` but got `Tuple[bool, float, int]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable, Protocol
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TParams = pyre_extensions.ParameterSpecification("TParams")
    def callable_to_callable(f: Callable[Ts, int]) -> Callable[Ts, int]:
      return f
    def rich_callable_to_callable(f: Callable[TParams, int]) -> Callable[TParams, int]:
      return f
    def bar(x: int, y: str, z: bool) -> int:
      return 7
    class Barable(Protocol):
      def __call__(self, x: int, y: str, z: bool) -> int: ...
    def foo() -> None:
      # bar directly does implement Barable
      f: Barable = bar
      # but putting it through a list variadic loses the name of the parameters
      f2: Barable = callable_to_callable(bar)
      # ParameterSpecifications capture the names, so we don't lose information
      f3: Barable = rich_callable_to_callable(bar)
    |}
    [
      "Incompatible variable type [9]: f2 is declared to have type `Barable` but is used as type \
       `typing.Callable[[int, str, bool], int]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def loop( *args: Ts) -> Tuple[Ts]:
      return args
    def foo(x: int, y: str, z: bool) -> None:
      reveal_type(loop(x, y, z))
    |}
    ["Revealed type [-1]: Revealed type for `test.loop(x, y, z)` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    def loop( *args: Ts) -> Tuple[Ts]:
      return args
    def foo(x: int, y: str, z: bool, t: Tuple[Ts]) -> None:
      l = loop(x, y, *t, z)
      reveal_type(l)
    |}
    [
      "Revealed type [-1]: Revealed type for `l` is `typing.Tuple[Concatenate[int, str, test.Ts, \
       bool]]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TsB = pyre_extensions.ListVariadic("TsB")
    def loop( *args: Ts) -> Tuple[Ts]:
      return args
    def foo(tA: Tuple[Ts], tB: Tuple[TsB]) -> None:
      loop( *tA, *tB)
    |}
    [
      "Invalid argument [32]: Variadic type variable `test.Ts` cannot be made to contain `test.Ts, \
       test.TsB`, concatenation of multiple variadic type variables is not yet implemented.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable, TypeVar
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def call_with_args(f: Callable[Ts, TReturn], *args: Ts) -> TReturn:
      return f( *args)
    def foo(x: int, y: str, z: bool) -> str: ...
    def bar(x: bool, y: int, z: float) -> int: ...
    def use(x: int, y: str, z: bool) -> None:
      reveal_type(call_with_args(foo, x, y, z))
      reveal_type(call_with_args(bar, z, x, x))
      call_with_args(bar, x, y, z)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.call_with_args(test.foo, x, y, z)` is `str`.";
      "Revealed type [-1]: Revealed type for `test.call_with_args(test.bar, z, x, x)` is `int`.";
      "Invalid argument [32]: Types `int, str, bool` conflict with existing constraints on \
       `test.Ts`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable, TypeVar
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def call_with_args(f: Callable[Ts, TReturn], *args: Ts) -> TReturn:
      return f( *args)
    def foo(x: int, y: str, z: bool) -> str: ...
    def bar(x: int, y: int, z: int) -> int: ...
    def use(x: Tuple[int, str, bool], y: Tuple[int, ...]) -> None:
      reveal_type(call_with_args(foo, *x))
      call_with_args(bar, *y)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.call_with_args(test.foo, *x)` is `str`.";
      "Invalid argument [32]: Variable argument `y` has type `typing.Tuple[int, ...]` but must be \
       a definite tuple to be included in variadic type variable `test.Ts`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, Optional, Callable, TypeVar
    import pyre_extensions
    Ts = pyre_extensions.ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def call_with_args(f: Callable[Ts, TReturn], *args: Ts) -> TReturn:
      return f( *args)
    def foo(x: int, y: str, z: bool) -> str: ...
    def bar(x: bool, y: int, z: float) -> int: ...
    def use(x: Tuple[int, str], y: Tuple[Ts]) -> None:
      reveal_type(call_with_args(foo, *x, True))
      call_with_args(bar, *x, *y)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.call_with_args(test.foo, *x, True)` is `str`.";
      "Invalid argument [32]: Types `Concatenate[int, str, test.Ts]` conflict with existing \
       constraints on `test.Ts`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    def foo(x: Tuple[Map[List, Ts]], y: Tuple[Ts]) -> None:
      reveal_type(x)
      reveal_type(y)
      for i in x:
        reveal_type(i)
      for i in y:
        reveal_type(i)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Tuple[Map[list, test.Ts]]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[test.Ts]`.";
      "Revealed type [-1]: Revealed type for `i` is `object`.";
      "Revealed type [-1]: Revealed type for `i` is `object`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    def foo(x: Tuple[Map[List, Ts]], y: Tuple[Ts]) -> None:
      reveal_type(x)
      reveal_type(y)
      for i in x:
        reveal_type(i)
      for i in y:
        reveal_type(i)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Tuple[Map[list, test.Ts]]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[test.Ts]`.";
      "Revealed type [-1]: Revealed type for `i` is `object`.";
      "Revealed type [-1]: Revealed type for `i` is `object`.";
    ];
  ()


let test_map context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    def wrap(x: Tuple[Ts]) -> Tuple[Map[List, Ts]]: ...
    def unwrap(x: Tuple[Map[List, Ts]]) -> Tuple[Ts]: ...
    def foo(x: int, y: str, lx: List[int], ly: List[str]) -> None:
      reveal_type(wrap((x, y)))
      reveal_type(unwrap((lx, ly)))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.wrap((x, y))` is `Tuple[List[int], List[str]]`.";
      "Revealed type [-1]: Revealed type for `test.unwrap((lx, ly))` is `Tuple[int, str]`.";
    ];
  assert_type_errors
    ~handle:"qualifier.py"
    {|
    from typing import Tuple, List, Generic, TypeVar
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    def unwrap( *args: Map[List, Ts]) -> Tuple[Ts]: ...
    def foo(lx: List[int], ly: List[str]) -> None:
      reveal_type(unwrap(lx, ly))
    |}
    ["Revealed type [-1]: Revealed type for `qualifier.unwrap(lx, ly)` is `Tuple[int, str]`."];
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar, Callable, TypeVar
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def unwrap_with(c: Callable[Map[List, Ts], TReturn], t: Tuple[Map[List, Ts]]) -> TReturn:
      return c( *t)
    def foo(lx: List[int], ly: List[str]) -> bool:
      return False
    def bar() -> None:
      reveal_type(unwrap_with(foo, ([2,3], ["A", "B"])))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.unwrap_with(test.foo, ([2, 3], [\"A\", \
       \"B\"]))` is `bool`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar, Callable, Iterable
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    def better_map(func: Callable[Ts, TReturn], *args: Map[Iterable, Ts]) -> TReturn: ...
    def takes_int(x: int) -> str: ...
    def takes_int_str(x: int, y: str) -> str: ...
    def foo() -> None:
      reveal_type(better_map(takes_int, [1,2]))
      reveal_type(better_map(takes_int_str, [1,2], ["A", "B"]))
      better_map(takes_int_str, ["A", "B"], [1, 2])
    |}
    [
      "Revealed type [-1]: Revealed type for `test.better_map(test.takes_int, [1, 2])` is `str`.";
      "Revealed type [-1]: Revealed type for `test.better_map(test.takes_int_str, [1, 2], [\"A\", \
       \"B\"])` is `str`.";
      "Invalid argument [32]: Types `typing.List[str], typing.List[int]` conflict with existing \
       constraints on `Map[typing.Iterable, test.Ts]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, List, Generic, TypeVar, Callable, Iterable, Awaitable
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    TReturn = TypeVar("TReturn")
    class AbstractEventLoop: pass
    def better_gather( *args: Map[Awaitable, Ts],
         loop: AbstractEventLoop = ..., return_exceptions: bool = ...) -> Awaitable[Tuple[Ts]]: ...
    def foo(i: Awaitable[int], s: Awaitable[str]) -> None:
      reveal_type(await better_gather(i))
      reveal_type(await better_gather(i, s))
      many = (i, s, i, s, i, s, i, s, i)
      reveal_type(await better_gather( *many))
    |}
    [
      "Revealed type [-1]: Revealed type for `await test.better_gather(i)` is `Tuple[int]`.";
      "Revealed type [-1]: Revealed type for `await test.better_gather(i, s)` is `Tuple[int, str]`.";
      "Revealed type [-1]: Revealed type for `await test.better_gather(*many)` is `Tuple[int, str, \
       int, str, int, str, int, str, int]`.";
    ];
  assert_type_errors
    {|
     from typing import List
     from pyre_extensions import ListVariadic
     from pyre_extensions.type_variable_operators import Map
     Ts = ListVariadic("Ts")
     def bad(x: List[Map[List, Ts]]) -> None:
       reveal_type(x)
       pass
     |}
    [
      "Invalid type parameters [24]: Single type parameter `Variable[_T]` expected, but a type \
       parameter group `[Map[list, test.Ts]]` was given for generic type list.";
      "Revealed type [-1]: Revealed type for `x` is `List[typing.Any]`.";
    ];

  ()


let test_user_defined_variadics context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
    from typing import Generic, Tuple, List
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    class Foo(Generic[Ts]):
      x: Tuple[Ts]
      y: Tuple[Map[List, Ts]]
      def __init__(self, x: Tuple[Ts], y: Tuple[Map[List, Ts]]) -> None:
        self.x = x
        self.y = y
      def meth(self, x: int, *args: Ts) -> bool:
        return True
    def fun(f: Foo[int, str, bool]) -> None:
      reveal_type(f.x)
      reveal_type(f.y)
      reveal_type(f.meth)
    def gun(f: Foo[bool, int, float]) -> None:
      reveal_type(f.x)
      reveal_type(f.y)
      reveal_type(f.meth)
    |}
    [
      "Revealed type [-1]: Revealed type for `f.x` is `Tuple[int, str, bool]`.";
      "Revealed type [-1]: Revealed type for `f.y` is `Tuple[List[int], List[str], List[bool]]`.";
      "Revealed type [-1]: Revealed type for `f.meth` is \
       `BoundMethod[typing.Callable(Foo.meth)[[Named(self, Foo[int, str, bool]), Named(x, int), \
       int, str, bool], bool], Foo[int, str, bool]]`.";
      "Revealed type [-1]: Revealed type for `f.x` is `Tuple[bool, int, float]`.";
      "Revealed type [-1]: Revealed type for `f.y` is `Tuple[List[bool], List[int], List[float]]`.";
      "Revealed type [-1]: Revealed type for `f.meth` is \
       `BoundMethod[typing.Callable(Foo.meth)[[Named(self, Foo[bool, int, float]), Named(x, int), \
       bool, int, float], bool], Foo[bool, int, float]]`.";
    ];

  assert_type_errors
    ~handle:"test.py"
    {|
    from typing import Generic, Tuple, List, Protocol
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    class Foo(Protocol[Ts]):
      def m(self, *args: Ts) -> bool: ...
    class I:
      def m(self, x: int, y: str, z: bool) -> bool:
        return True
    def fun(f: Foo[Ts]) -> Tuple[Ts]: ...
    def gun(x: I) -> None:
      reveal_type(fun(x))
    |}
    ["Revealed type [-1]: Revealed type for `test.fun(x)` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
    from typing import Generic, Tuple, List, Protocol
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    class Foo(Generic[Ts]):
      pass
    def f_in( *args: Ts) -> Foo[Ts]: ...
    def f_out(f: Foo[Ts]) -> Tuple[Ts]: ...
    def fun(i: int, s: str, b: bool) -> None:
      x = f_in(i, s, b)
      reveal_type(x)
      y = f_out(x)
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Foo[int, str, bool]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, str, bool]`.";
    ];
  assert_type_errors
    {|
    from typing import Tuple, List, Protocol
    from pyre_extensions import ListVariadic, Generic
    from pyre_extensions.type_variable_operators import Map
    Ts = ListVariadic("Ts")
    class Foo(Generic[Ts]):
      pass
    def f_in( *args: Ts) -> Foo[Ts]: ...
    def f_out(f: Foo[Ts]) -> Tuple[Ts]: ...
    def fun(i: int, s: str, b: bool) -> None:
      x = f_in(i, s, b)
      reveal_type(x)
      y = f_out(x)
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Foo[int, str, bool]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, str, bool]`.";
    ];
  (* Distinguishing between these two cases is the main reason we introduce the intermediate type
     Type.Single *)
  assert_type_errors
    {|
    from typing import Generic, Tuple, List, TypeVar
    from typing_extensions import Literal
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Concatenate
    Ts = ListVariadic("Ts")
    T = TypeVar("T")
    One = Literal[1]
    Two = Literal[2]
    Three = Literal[3]
    class Tensor(Generic[T, Ts]):
      def el(self) -> T: ...
      def dims(self) -> Tuple[Ts]: ...
    def foo(
      valid: Tensor[int, [One, Two, Three]],
      invalid: Tensor[[int], [One, Two, Three]]
    ) -> None:
      reveal_type(valid.el())
      reveal_type(invalid.el())
    |}
    [
      "Invalid type parameters [24]: Single type parameter `Variable[T]` expected, but a type \
       parameter group `[int]` was given for generic type Tensor.";
      "Revealed type [-1]: Revealed type for `valid.el()` is `int`.";
      "Revealed type [-1]: Revealed type for `invalid.el()` is `typing.Any`.";
    ];
  ()


let test_concatenation_operator context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
    from typing import Generic, Tuple, List
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Concatenate
    Ts = ListVariadic("Ts")
    def add_on(t: Tuple[Ts]) -> Tuple[Concatenate[int, Ts, float]]:
      ...
    def strip_off(t: Tuple[Concatenate[int, Ts, bool]]) -> Tuple[Ts]:
      ...
    def bar(t: Tuple[int, str, bool]) -> None:
      added = add_on(t)
      reveal_type(added)
      removed = strip_off(t)
      reveal_type(removed)
    |}
    [
      "Revealed type [-1]: Revealed type for `added` is `Tuple[int, int, str, bool, float]`.";
      "Revealed type [-1]: Revealed type for `removed` is `Tuple[str]`.";
    ];
  assert_type_errors
    {|
    from typing import Generic, Tuple, List
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Concatenate, Map
    Ts = ListVariadic("Ts")
    def map_tuple(t: Tuple[Ts]) -> Tuple[Map[List, Ts]]:
      ...
    def unmap_tuple(t: Tuple[Map[List, Ts]]) -> Tuple[Ts]:
      ...
    def foo(t: Tuple[Concatenate[int, Ts, bool]]) -> None:
      x = map_tuple(t)
      reveal_type(x)
      # this is not implemented yet (T48180915)
      unmap_tuple(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Tuple[Concatenate[List[int], Map[list, \
       test.Ts], List[bool]]]`.";
      "Incompatible parameter type [6]: Expected `typing.Tuple[Map[list, test.Ts]]` for 1st \
       positional only parameter to call `unmap_tuple` but got \
       `typing.Tuple[Concatenate[List[int], Map[list, test.Ts], List[bool]]]`.";
    ];
  assert_type_errors
    {|
    from typing import Generic, Tuple, List, TypeVar
    from typing_extensions import Literal
    from pyre_extensions import ListVariadic
    from pyre_extensions.type_variable_operators import Concatenate
    Ts = ListVariadic("Ts")
    T = TypeVar("T")
    class Tensor(Generic[T, Ts]):
      def el(self) -> T: ...
      def dims(self) -> Tuple[Ts]: ...
    One = Literal[1]
    Two = Literal[2]
    Three = Literal[3]
    def bar(t: Tensor[int, [One, Two, Three]]) -> None:
      el = t.el()
      reveal_type(el)
      dims = t.dims()
      reveal_type(dims)
    |}
    [
      "Revealed type [-1]: Revealed type for `el` is `int`.";
      "Revealed type [-1]: Revealed type for `dims` is `Tuple[typing_extensions.Literal[1], \
       typing_extensions.Literal[2], typing_extensions.Literal[3]]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, TypeVar
      from pyre_extensions import ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate
      Ts = ListVariadic("Ts")

      def prepend_addition_argument(f: Callable[Ts, int]) -> Callable[Concatenate[int, Ts], str]:
           def inner(x: int, *args: Ts) -> str:
               return str(x + f( *args))
           return inner

      @prepend_addition_argument
      def foo(x: int, y: int) -> int:
          return x + y

      reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable[[int, int, int], str]`."];
  assert_type_errors
    {|
      from typing import Callable, TypeVar, List
      from pyre_extensions import ListVariadic
      from pyre_extensions.type_variable_operators import Concatenate
      Ts = ListVariadic("Ts")
      TReturn = TypeVar("TReturn")

      def simple_partial_application(
        f: Callable[Concatenate[float, Ts], TReturn]
      ) -> Callable[Ts, TReturn]:
          def inner( *args: Ts) -> TReturn:
              return f(42.0, *args)
          return inner
      @simple_partial_application
      def foo(x: float, y: str, z: bool) -> int:
          return 3

      reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable[[str, bool], int]`."];
  assert_type_errors
    {|
      from typing import Generic, Tuple, List, TypeVar
      from typing_extensions import Literal
      import pyre_extensions
      Ts = pyre_extensions.ListVariadic("Ts")
      T = TypeVar("T")
      class Tensor(Generic[T, Ts]):
        pass
      def bar(t: Tensor[int, str]) -> None:
        pass
    |}
    [
      "Invalid type parameters [24]: Type parameter group expected for variadic parameter `Ts`, \
       but a single type `str` was given for generic type Tensor.";
    ];
  assert_type_errors
    {|
      from typing import Generic, Tuple, List, TypeVar
      from typing_extensions import Literal
      import pyre_extensions
      T = TypeVar("T")
      Ts1 = pyre_extensions.ListVariadic("Ts1")
      Ts2 = pyre_extensions.ListVariadic("Ts2")
      class Multi(Generic[Ts1, T, Ts2]):
        def first(self) -> Tuple[Ts1]: ...
        def second(self) -> Tuple[Ts2]: ...
      def bar(m: Multi[[int, str], float, [bool, int, bool]]) -> None:
        reveal_type(m.first())
        reveal_type(m.second())
    |}
    [
      "Revealed type [-1]: Revealed type for `m.first()` is `Tuple[int, str]`.";
      "Revealed type [-1]: Revealed type for `m.second()` is `Tuple[bool, int, bool]`.";
    ];
  ()


let test_user_defined_parameter_specification_classes context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import TypeVar, Generic, Callable

      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def function(param: str) -> str:
        ...
      class MyClass(Generic[TParams, TReturn]):
        f: Callable[TParams, TReturn]

        def __init__(self, f: Callable[TParams, TReturn]) -> None:
         self.f = f

        def call(__self, *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
          f = __self.f
          # do some logging or something
          return f( *args, **kwargs)

      def client(f: Callable[TParams, TReturn]) -> MyClass[TParams, TReturn]:
        return MyClass(f)

      def foo() -> None:
        x = client(function).call(param="")
        reveal_type(x)
        client(function).call(parm="")
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Unexpected keyword [28]: Unexpected keyword argument `parm` to call `MyClass.call`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import TypeVar, Generic, Callable
      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def client(f: Callable[TParams, TReturn]) -> None:
        def inner(__x: int, *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn:
          return f( *args, **kwargs)
        reveal_type(inner)
    |}
    [
      "Revealed type [-1]: Revealed type for `inner` is \
       `typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, test.TParams], \
       Variable[TReturn]]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import TypeVar, Generic, Callable, Protocol
      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      class CallableReturningInt(Protocol[TParams]):
        def __call__(__self, __f: int, *args: TParams.args, **kwargs: TParams.kwargs) -> int:
          ...
      def remove_int_argument(f: CallableReturningInt[TParams]) -> Callable[TParams, int]: ...
      def goof(x: int, y: str) -> int:
        return x
      def foo() -> None:
        f = remove_int_argument(goof)
        reveal_type(f)
    |}
    ["Revealed type [-1]: Revealed type for `f` is `typing.Callable[[Named(y, str)], int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from pyre_extensions.type_variable_operators import Concatenate
      from typing import TypeVar, Generic, Callable, Protocol
      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def remove_int_argument(f: Callable[Concatenate[int, TParams], str]) -> Callable[TParams, int]:
        def inner( *args: TParams.args, **kwargs: TParams.kwargs) -> int:
          s = f(75, *args, **kwargs)
          return int(s)
        return inner
      def goof(x: int, y: str) -> str:
        return str(x)
      def foo() -> None:
        f = remove_int_argument(goof)
        reveal_type(f)
    |}
    ["Revealed type [-1]: Revealed type for `f` is `typing.Callable[[Named(y, str)], int]`."];
  assert_type_errors
    {|
    from typing import Protocol
    from pyre_extensions import ParameterSpecification
    from typing import TypeVar, Generic, Callable
    TParams = ParameterSpecification("TParams")
    TReturn = TypeVar("TReturn")
    TSelf = TypeVar("TSelf")
    class ObjectMethod(Protocol[TSelf, TParams, TReturn]):
        def __call__(__self, __other_self: TSelf, *args: TParams.args, **kwargs: TParams.kwargs) -> TReturn: ...
    def track_assertion(
      assertion: ObjectMethod["TestCommand", TParams, None]
    ) -> ObjectMethod["TestCommand", TParams, int]:
         def assert_test(
           __self: "TestCommand",
           *args: TParams.args,
           **kwargs: TParams.kwargs
         ) -> int:
           assertion(__self, *args, **kwargs)
           return 7
         return assert_test
    class TestCommand:
      @track_assertion
      def method(self: "TestCommand", x: int) -> None:
        pass

    def foo() -> None:
      m = TestCommand().method
      reveal_type(m)
    |}
    [
      "Revealed type [-1]: Revealed type for `m` is `ObjectMethod[TestCommand, [Named(x, int)], \
       int]`.";
    ];
  assert_type_errors
    {|
    from typing import Protocol
    from pyre_extensions import ParameterSpecification
    from pyre_extensions.type_variable_operators import Concatenate
    from typing import TypeVar, Generic, Callable
    TParams = ParameterSpecification("TParams")
    TReturn = TypeVar("TReturn")
    TSelf = TypeVar("TSelf")
    def track_assertion(
      assertion: Callable[Concatenate["TestCommand", TParams], None]
    ) -> Callable[Concatenate["TestCommand", TParams], int]:
         def assert_test(
           __self: "TestCommand",
           *args: TParams.args,
           **kwargs: TParams.kwargs
         ) -> int:
           assertion(__self, *args, **kwargs)
           return 7
         return assert_test
    class TestCommand:
      @track_assertion
      def method(self: "TestCommand", x: int) -> None:
        pass

    def foo() -> None:
      m = TestCommand().method
      reveal_type(m)

    |}
    [
      "Revealed type [-1]: Revealed type for `m` is `BoundMethod[typing.Callable[[TestCommand, \
       Named(x, int)], int], TestCommand]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from pyre_extensions.type_variable_operators import Concatenate
      from typing import TypeVar, Generic, Callable, Protocol
      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      def add_on_argument(f: Callable[TParams, str]) -> Callable[Concatenate[str, TParams], int]:
        def inner(first: str, /, *args: TParams.args, **kwargs: TParams.kwargs) -> int:
          s = f( *args, **kwargs)
          return int(s)
        return inner
      def goof(x: int) -> str:
        return str(x)
      def foo() -> None:
        f = add_on_argument(goof)
        reveal_type(f)
    |}
    ["Revealed type [-1]: Revealed type for `f` is `typing.Callable[[str, Named(x, int)], int]`."];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification, ListVariadic
      from typing import TypeVar, Generic, Callable

      Ts = ListVariadic("Ts")
      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")
      class MyClass(Generic[Ts, TReturn]):
        pass

      def bad(x: MyClass[[TParams, int], str]) -> None:
        pass
    |}
    ["Undefined or invalid type [11]: Annotation `TParams` is not defined as a type."];
  ()


let test_duplicate_type_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
    from typing import TypeVar, Generic

    T = TypeVar("T")
    S = TypeVar("S")
    class A(Generic[T, S, T]):
        pass
  |}
    ["Duplicate type variables [59]: Duplicate type variable `T` in Generic[...]."];
  assert_type_errors
    {|
    from typing import TypeVar, Protocol

    T = TypeVar("T")
    class A(Protocol[T, T, T]):
        pass
  |}
    ["Duplicate type variables [59]: Duplicate type variable `T` in Protocol[...]."];
  assert_type_errors
    {|
    from typing import Generic
    from pyre_extensions import ListVariadic

    Ts = ListVariadic("Ts")
    class A(Generic[Ts, Ts]):
        pass
  |}
    ["Duplicate type variables [59]: Duplicate type variable `Ts` in Generic[...]."];
  assert_type_errors
    {|
    from typing import Generic
    from pyre_extensions import ParameterSpecification

    P = ParameterSpecification("P")
    class A(Generic[P, P]):
        pass
  |}
    ["Duplicate type variables [59]: Duplicate type variable `P` in Generic[...]."];
  ()


let () =
  "typeVariable"
  >::: [
         "check_bounded_variables" >:: test_check_bounded_variables;
         "check_unbounded_variables" >:: test_check_unbounded_variables;
         "check_variable_bindings" >:: test_check_variable_bindings;
         "unbound_variables" >:: test_unbound_variables;
         "distinguish" >:: test_distinguish;
         "integer_variables" >:: test_integer_variables;
         "nested_variable_error" >:: test_nested_variable_error;
         "single_explicit_error" >:: test_single_explicit_error;
         "callable_parameter_variadics" >:: test_callable_parameter_variadics;
         "list_variadics" >:: test_list_variadics;
         "map" >:: test_map;
         "user_defined_variadics" >:: test_user_defined_variadics;
         "concatenation" >:: test_concatenation_operator;
         "user_defined_parameter_variadics" >:: test_user_defined_parameter_specification_classes;
         "duplicate_type_variables" >:: test_duplicate_type_variables;
       ]
  |> Test.run

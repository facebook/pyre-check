(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `int` but got `str`.";
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
      "Incompatible parameter type [6]: In call `expects_string`, for 1st positional only \
       parameter expected `str` but got `Variable[T]`.";
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
      "Incompatible parameter type [6]: In call `typing.GenericMeta.__getitem__`, for 1st \
       positional only parameter expected `Type[Variable[X]]` but got `str`.";
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
      "Incompatible parameter type [6]: In call `Foo.__init__`, for 1st positional only parameter \
       expected `int` but got `float`.";
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
      "Incompatible parameter type [6]: In call `generic`, for 3rd positional only parameter \
       expected `List[Variable[T2]]` but got `List[int]`.";
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
       type `List[Union[G[int], G[str]]]`.";
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
      "Incompatible parameter type [6]: In call `str_to_int`, for 1st positional only parameter \
       expected `str` but got `Variable[T (bound to int)]`.";
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
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Variable[T (bound to int)]` but got `str`.";
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
      "Incompatible parameter type [6]: In call `f`, for 2nd positional only parameter expected \
       `int` but got `None`.";
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
      "Incompatible parameter type [6]: In call `typing.GenericMeta.__getitem__`, for 1st \
       positional only parameter expected `Type[Variable[X (bound to C)]]` but got `Type[int]`.";
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
      "Incompatible parameter type [6]: In call `typing.GenericMeta.__getitem__`, for 1st \
       positional only parameter expected `Type[Variable[X <: [Mineral, Animal]]]` but got \
       `Type[int]`.";
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
  (* Test for a common misuse of variable bounds. *)
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      TSelf = TypeVar("TSelf", bound="G")
      T = TypeVar("T")

      class G(Generic[T]):
        # This method restricts the inputs to be less than `G[Any]` but does
        # not enforce that the two inputs are of the same type.
        def expect_self(self: TSelf, other: TSelf) -> TSelf: ...

      x: G[int]

      y: G[str]
      x.expect_self(y)
      reveal_type(x.expect_self(y))

      z: bool
      x.expect_self(z)
    |}
    [
      "Invalid type parameters [24]: Generic type `G` expects 1 type parameter.";
      "Revealed type [-1]: Revealed type for `x.expect_self(y)` is `typing.Union[G[int], G[str]]`.";
      "Incompatible parameter type [6]: In call `G.expect_self`, for 1st positional only parameter \
       expected `Variable[TSelf (bound to G[typing.Any])]` but got `bool`.";
    ];
  (* Same test as above but without an explicit type for `self`. *)
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      TSelf = TypeVar("TSelf", bound="G")
      T = TypeVar("T")
      class G(Generic[T]):
        # This method restricts the inputs to be less than `G[Any]` but does
        # not enforce that the two inputs are of the same type.
        def expect_self(self, other: TSelf) -> TSelf: ...

      x: G[int]

      y: G[str]
      x.expect_self(y)
      reveal_type(x.expect_self(y))

      z: bool
      x.expect_self(z)
    |}
    [
      "Invalid type parameters [24]: Generic type `G` expects 1 type parameter.";
      "Revealed type [-1]: Revealed type for `x.expect_self(y)` is `G[str]`.";
      "Incompatible parameter type [6]: In call `G.expect_self`, for 1st positional only parameter \
       expected `Variable[TSelf (bound to G[typing.Any])]` but got `bool`.";
    ];
  (* This actually requires the input to be of the same type as `self`. *)
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      TSelf = TypeVar("TSelf", bound="G")
      T = TypeVar("T")
      class G(Generic[T]):
        def expect_same_type(self: G[T], other: G[T]) -> G[T]: ...

      x: G[int]

      y: G[str]
      x.expect_same_type(y)
      reveal_type(x.expect_same_type(y))

      z: bool
      x.expect_same_type(z)
    |}
    [
      "Invalid type parameters [24]: Generic type `G` expects 1 type parameter.";
      "Incompatible parameter type [6]: In call `G.expect_same_type`, for 1st positional only \
       parameter expected `G[int]` but got `G[str]`.";
      "Revealed type [-1]: Revealed type for `x.expect_same_type(y)` is `G[int]`.";
      "Incompatible parameter type [6]: In call `G.expect_same_type`, for 1st positional only \
       parameter expected `G[int]` but got `bool`.";
    ];
  (* Setting the bound as a parameter-less generic class `INode` replaces the parameters with Any.
     This is equivalent to writing `bound=INode[Any]`. *)
  assert_type_errors
    {|
      from typing import Generic, Tuple, TypeVar

      T = TypeVar("T")

      class INode(Generic[T]): ...

      TBoundToINode = TypeVar("TNodeGetResult", bound=INode)

      TResult = TypeVar("TResult")

      class Query(Generic[TResult]):
        def get_result(self) -> TResult: ...

      class NodeGetQuery(Query[TBoundToINode]): ...

      y: NodeGetQuery[int]
      z: NodeGetQuery[INode[str]]
      z3: NodeGetQuery[INode[int]]
    |}
    [
      "Invalid type parameters [24]: Generic type `INode` expects 1 type parameter.";
      "Invalid type parameters [24]: Type parameter `int` violates constraints on \
       `Variable[TBoundToINode (bound to test.INode)]` in generic type `NodeGetQuery`.";
    ];
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
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type \
       `List[Variable[_T]]`.";
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
      "Incompatible variable type [9]: x is declared to have type `List[int]` but is used as type \
       `Dict[Variable[_KT], Variable[_VT]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[int, str] = []
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `Dict[int, str]` but is used as \
       type `List[Variable[_T]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Dict[int, typing.List[int]] = { "A" : [] }
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `Dict[int, List[int]]` but is \
       used as type `Dict[str, List[int]]`.";
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
      "Incompatible variable type [9]: x is declared to have type `Dict[int, str]` but is used as \
       type `DefaultDict[Variable[collections._KT], Dict[Variable[_KT], Variable[_VT]]]`.";
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
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `List[Variable[_T]]`.";
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
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `Variable[_T1]` but got `int`.";
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
      "Incompatible parameter type [6]: In call `baz`, for 1st positional only parameter expected \
       `IntegerVariable[X]` but got `int`.";
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
      "Incompatible parameter type [6]: In call `call_this_function`, for 2nd positional only \
       parameter expected `int` but got `str`.";
      "Incompatible parameter type [6]: In call `call_this_function`, for 2nd parameter `i` \
       expected `int` but got `str`.";
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
      "Incompatible parameter type [6]: In call `call_this_function`, for 2nd positional only \
       parameter expected `int` but got `float`.";
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
        # invalid first argument
        call_n_times(invalid, 75, 1, "A")
        # missing second argument
        call_n_times(valid, y="A", x=1)
    |}
    [
      "Incompatible parameter type [6]: In call `call_n_times`, for 1st positional only parameter \
       expected `typing.Callable[test.TParams, None]` but got `typing.Callable(invalid)[[Named(x, \
       int), Named(y, str)], int]`.";
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
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `int` but got `str`.";
      "Incompatible parameter type [6]: In anonymous call, for 2nd positional only parameter \
       expected `str` but got `int`.";
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
      "Incompatible parameter type [6]: In call `Model.__call__`, for 2nd positional only \
       parameter expected `str` but got `int`.";
      "Revealed type [-1]: Revealed type for `z` is `str`.";
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
      "Incompatible parameter type [6]: In call `H.f`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "Incompatible parameter type [6]: In call `H.f`, for 2nd positional only parameter expected \
       `str` but got `int`.";
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
      "Incompatible parameter type [6]: In call `does_care_positional`, for 1st positional only \
       parameter expected `int` but got `object`.";
      "Incompatible parameter type [6]: In call `does_care_keywords`, for 1st positional only \
       parameter expected `int` but got `object`.";
    ];
  ()


let test_user_defined_parameter_specification_classes context =
  let assert_type_errors = assert_type_errors ~context in

  (* Make sure `typing.ParamSpec` works. *)
  assert_type_errors
    {|
      from typing import Callable, ParamSpec
      TParams = ParamSpec("TParams")
      def client(f: Callable[TParams, int]) -> None:
        def inner( *args: TParams.args, **kwargs: TParams.kwargs) -> int:
          return f( *args, **kwargs)
    |}
    [];
  (* Make sure `typing_extensions.ParamSpec` works. *)
  assert_type_errors
    {|
      from typing import Callable
      from typing_extensions import ParamSpec
      TParams = ParamSpec("TParams")
      def client(f: Callable[TParams, int]) -> None:
        def inner( *args: TParams.args, **kwargs: TParams.kwargs) -> int:
          return f( *args, **kwargs)
    |}
    [];
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
      from pyre_extensions import ParameterSpecification
      from typing import TypeVar, Generic, Callable

      TParams = ParameterSpecification("TParams")

      class MyClass(Generic[TParams]):
        def __call__(__self, *args: TParams.args, **kwargs: TParams.kwargs) -> bool: ...

      IntStrParamSpec = MyClass[int, str]

      def foo() -> None:
        f: IntStrParamSpec
        reveal_type(f)

        f(1, "hello")

        f("invalid")
    |}
    [
      "Revealed type [-1]: Revealed type for `f` is `MyClass[[int, str]]`.";
      "Missing argument [20]: Call `MyClass.__call__` expects argument in position 2.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification
      from typing import TypeVar, Generic, Callable, Protocol

      TParams = ParameterSpecification("TParams")

      class PrependIntProtocol(Protocol[TParams]):
        def __call__(__self, __f: int, *args: TParams.args, **kwargs: TParams.kwargs) -> int: ...

      IntBoolStrParamSpec = PrependIntProtocol[bool, str]

      def foo() -> None:
        f: IntBoolStrParamSpec
        reveal_type(f)

        f(1, True, "hello")

        f("invalid")
    |}
    [
      "Revealed type [-1]: Revealed type for `f` is `PrependIntProtocol[[bool, str]]`.";
      "Missing argument [20]: Call `PrependIntProtocol.__call__` expects argument in position 2.";
    ];
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
    from pyre_extensions import ParameterSpecification

    P = ParameterSpecification("P")
    class A(Generic[P, P]):
        pass
  |}
    ["Duplicate type variables [59]: Duplicate type variable `P` in Generic[...]."];
  ()


let test_generic_aliases context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import List

      MyList = List[int]

      x: MyList
      reveal_type(x)
      reveal_type(x[0])
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `x[0]` is `int`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple, TypeVar
      T = TypeVar("T")

      Pair = Tuple[T, T]

      x: Pair[str]
      reveal_type(x)
      reveal_type(x[0])
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Tuple[str, str]`.";
      "Revealed type [-1]: Revealed type for `x[0]` is `str`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Union
      T = TypeVar("T")

      UnionWithInt = Union[T, int]

      x: UnionWithInt[str]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Union[int, str]`."];
  assert_type_errors
    {|
      from typing import List, Tuple, TypeVar, Union
      T = TypeVar("T")

      Alias1 = Union[T, int]
      Alias2 = Tuple[T, Alias1[T]]
      Alias3 = List[Alias2[T]]

      x: Alias3[str]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `List[Tuple[str, Union[int, str]]]`."];
  (* `MyList3` resolves to `List[int]`. So, it ignores the extra `str` argument. *)
  assert_type_errors
    {|
      from typing import *

      T = TypeVar("T")
      MyList1 = List[T]
      MyList2 = MyList1[int]
      MyList3 = MyList2

      xs: MyList3[str]
      reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `typing.List[int]`."];
  let sources_exporting_generic_classes =
    [
      {
        Test.handle = "foo.py";
        source =
          {|
            from typing import Generic, TypeVar
            T= TypeVar("T")
            class SomeGenericClass(Generic[T]): ...
          |};
      };
      {
        handle = "baz.py";
        source =
          {|
            from typing import Dict, Generic, Iterable, Optional, Sequence, Union, TypeVar
            from foo import SomeGenericClass
          |};
      };
    ]
  in
  (* `Optional` is imported as `foo.bar.baz.Optional`, which is an alias we resolve to
     `typing.Optional`. *)
  assert_type_errors
    ~update_environment_with:sources_exporting_generic_classes
    {|
      from baz import *
      from typing import List as MyList

      reveal_type(Optional)
      reveal_type(Union)
      reveal_type(MyList)
      reveal_type(Iterable)
      reveal_type(SomeGenericClass)
    |}
    [
      "Revealed type [-1]: Revealed type for `baz.Optional` is `typing.Type[typing.Optional]`.";
      "Revealed type [-1]: Revealed type for `baz.Union` is `typing.Type[typing.Union]`.";
      "Revealed type [-1]: Revealed type for `typing.List` is `typing.Type[list]`.";
      "Revealed type [-1]: Revealed type for `baz.Iterable` is `typing.Type[typing.Iterable]`.";
      "Revealed type [-1]: Revealed type for `baz.SomeGenericClass` is \
       `typing.Type[foo.SomeGenericClass]`.";
    ];
  assert_type_errors
    ~update_environment_with:sources_exporting_generic_classes
    {|
      from baz import *
      from typing import List as MyList, TypeVar

      z: MyList[int] = ["hello"]
      z2: Iterable[int] = ["hello"]
      z3: SomeGenericClass[int] = ["hello"]
    |}
    [
      "Incompatible variable type [9]: z is declared to have type `MyList[int]` but is used as \
       type `MyList[str]`.";
      "Incompatible variable type [9]: z2 is declared to have type `Iterable[int]` but is used as \
       type `Iterable[str]`.";
      "Incompatible variable type [9]: z3 is declared to have type `SomeGenericClass[int]` but is \
       used as type `MyList[str]`.";
    ];
  (* We should correctly resolve nested generic aliases like `baz.Dict`. *)
  assert_type_errors
    ~update_environment_with:sources_exporting_generic_classes
    {|
      from baz import *

      x: Optional[Dict[str, int]]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[typing.Dict[str, int]]`."];
  let sources_exporting_generic_classes =
    [
      {
        Test.handle = "bar/baz.py";
        source = {|
            from typing import Callable
          |};
      };
    ]
  in
  assert_type_errors
    ~update_environment_with:sources_exporting_generic_classes
    {|
      from bar.baz import Callable

      def foo() -> None:
        reveal_type(Callable)
        f: Callable[[int], str]
        y = f(1)
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `bar.baz.Callable` is `typing.Type[typing.Callable]`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
    ];
  assert_type_errors
    {|
      from typing import Callable

      C = Callable

      def foo() -> None:
        f: C[[int], str]
        reveal_type(f)
    |}
    [
      (* TODO(T78935633): Probably shouldn't error here. *)
      "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
      "Revealed type [-1]: Revealed type for `f` is `typing.Callable[[int], str]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Iterable, Iterator, TypeVar

      T = TypeVar("T")

      Predicate = Callable[[T], int]
      def dropwhile(predicate: Predicate[T], iterable: Iterable[T]) -> Iterator[T]: ...

      def foo() -> None:
        reveal_type(dropwhile)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.dropwhile` is \
       `typing.Callable(dropwhile)[[Named(predicate, typing.Callable[[Variable[T]], int]), \
       Named(iterable, Iterable[Variable[T]])], Iterator[Variable[T]]]`.";
    ];
  (* Generic alias for a class respects variance. *)
  assert_type_errors
    {|
      from typing import TypeVar, Iterable as MyIterable, List as MyList
      T = TypeVar("T")

      class Base: ...
      class Child(Base): ...

      xs: MyIterable[Child]
      # No error, since Iterable is covariant.
      ys: MyIterable[Base] = xs

      xs: MyList[Child]
      # Error because List is invariant.
      ys: MyList[Base] = xs
    |}
    [
      "Incompatible variable type [9]: ys is declared to have type `MyList[Base]` but is used as \
       type `MyList[Child]`.";
    ];
  (* Error messages. *)
  (* Zero type parameters provided. *)
  assert_type_errors
    {|
      from typing import Tuple, TypeVar
      T = TypeVar("T")

      Pair = Tuple[T, T]

      y: Pair
      reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `Tuple[typing.Any, typing.Any]`."];
  (* Extra type parameters provided. *)
  assert_type_errors
    {|
      from typing import Tuple, TypeVar
      T = TypeVar("T")

      Pair = Tuple[T, T]

      y: Pair[int, str]
      reveal_type(y)
    |}
    [
      (* TODO(T78935633): Raise clearer error. *)
      "Invalid type variable [34]: The type variable `Variable[T]` can only be used to annotate \
       generic classes or functions.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
    ];
  (* More than one free variable in the alias body. *)
  assert_type_errors
    {|
      from typing import Tuple, TypeVar
      T1 = TypeVar("T1")
      T2 = TypeVar("T2")

      Pair = Tuple[T1, T2]

      y: Pair[int]
      reveal_type(y)
      y: Pair[int, str]
      reveal_type(y)
    |}
    [
      (* TODO(T78935633): Raise clearer error. *)
      "Invalid type variable [34]: The type variable `Variable[T1]` can only be used to annotate \
       generic classes or functions.";
      "Invalid type variable [34]: The type variable `Variable[T2]` can only be used to annotate \
       generic classes or functions.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, str]`.";
    ];
  (* No free variables in the alias body. *)
  assert_type_errors
    {|
      from typing import Any, Tuple, TypeVar
      T = TypeVar("T")

      Pair = Tuple[str, int]

      y: Pair
      reveal_type(y)
      y: Pair[str]
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[str, int]`.";
      (* TODO(T78935633): Raise error about extra parameter. *)
      "Revealed type [-1]: Revealed type for `y` is `Tuple[str, int]`.";
    ];
  (* TODO(T78935633): We should error on the naked Foo and treat it as Foo[Any]. *)
  assert_type_errors
    {|
      from typing import *

      T = TypeVar("T")

      MyList = List[T]

      def foo(x: T, y: MyList) -> MyList:
        return y

      foo(1, ['hello'])
      foo('some', ['hello'])
      reveal_type(foo(1, ['hello']))
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo(1, [\"hello\"])` is \
       `typing.List[typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import *

      MyList = List

      def foo(x: MyList) -> MyList: ...
      reveal_type(foo)
      reveal_type(foo(['hello']))
    |}
    [
      "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors.";
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[Named(x, \
       typing.List[typing.Any])], typing.List[typing.Any]]`.";
      "Revealed type [-1]: Revealed type for `test.foo([\"hello\"])` is `typing.List[typing.Any]`.";
    ];
  (* This confusing behavior is the downside of allowing multiple type variables. *)
  assert_type_errors
    {|
      from typing import List, Tuple, TypeVar, Union

      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      T3 = TypeVar("T3")

      Alias2Before3 = Tuple[T1, Union[T2, T3], T2]
      Alias3Before2 = Tuple[T1, Union[T3, T2], T2]

      x: Alias2Before3[int, str, bool]
      reveal_type(x)
      y: Alias3Before2[int, str, bool]
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Tuple[int, Union[bool, str], str]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, Union[bool, str], str]`.";
    ];
  ()


let test_recursive_aliases context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
      x: Tree
      reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `test.Tree (resolves to Union[Tuple[Tree, \
       Tree], int])`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]

      x: Tree

      some_int: int
      x = some_int

      tuple_int: Tuple[int, int]
      x = tuple_int

      tuple_tuple_int: Tuple[Tuple[int, int], int]
      x = tuple_tuple_int
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]

      x: Tree

      x = 1
      x = (2, 3)
      x = ((4, 5), (6, 7))
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]

      x: Tree

      some_str: str
      x = some_str

      tuple_int_str: Tuple[int, str]
      x = tuple_int_str
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `test.Tree (resolves to \
       Union[Tuple[Tree, Tree], int])` but is used as type `str`.";
      "Incompatible variable type [9]: x is declared to have type `test.Tree (resolves to \
       Union[Tuple[Tree, Tree], int])` but is used as type `Tuple[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]

      x: Tree
      x = "hello"
      x = (1, "hello")
      x = ((2, 3), (4, "hello"))
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `test.Tree (resolves to \
       Union[Tuple[Tree, Tree], int])` but is used as type `str`.";
      "Incompatible variable type [9]: x is declared to have type `test.Tree (resolves to \
       Union[Tuple[Tree, Tree], int])` but is used as type `Tuple[int, str]`.";
      "Incompatible variable type [9]: x is declared to have type `test.Tree (resolves to \
       Union[Tuple[Tree, Tree], int])` but is used as type `Tuple[Tuple[int, int], Tuple[int, \
       str]]`.";
    ];
  assert_type_errors
    {|
      from typing import Mapping, Union

      StringDict = Union[str, Mapping[str, "StringDict"]]

      valid: StringDict = {"hello": {"world": "from here"}}
      contains_int: StringDict = {"hello": {"world": 1}}
    |}
    [
      "Incompatible variable type [9]: contains_int is declared to have type `test.StringDict \
       (resolves to Union[Mapping[str, StringDict], str])` but is used as type `Dict[str, \
       Dict[str, int]]`.";
    ];
  assert_type_errors
    {|
      from typing import List, Tuple

      Tree = Tuple[str, List["Tree"]]

      tree: Tree = ("foo", [])
      tree2: Tree = ("foo", [("branch1", [("leaf1", [])]), ("leaf2", [])])
    |}
    [];
  (* Useless but valid recursive alias. *)
  assert_type_errors
    {|
      from typing import List, Union
      X = List["X"]

      def foo() -> None:
        x: X = [[], [[], []], []]
     |}
    [];
  assert_type_errors
    {|
      from typing import Mapping, Union

      StringMapping = Union[str, Mapping[str, "StringMapping"]]

      d: Mapping[str, str]
      d2: StringMapping = d
    |}
    [];
  (* Incompatible because Dict is invariant. *)
  assert_type_errors
    {|
      from typing import Dict, Union

      StringDict = Union[str, Dict[str, "StringDict"]]

      d: Dict[str, str]
      d2: StringDict = d
    |}
    [
      "Incompatible variable type [9]: d2 is declared to have type `test.StringDict (resolves to \
       Union[Dict[str, StringDict], str])` but is used as type `Dict[str, str]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]
      Y = Union[int, Tuple[int, "Y"]]

      x: X
      y: Y = x

      y2: Y
      x2: X = y2
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]
      NotQuiteIsomorphicToX = Union[int, Tuple[str, "NotQuiteIsomorphicToX"]]

      x: X
      not_quite_isomorphic: NotQuiteIsomorphicToX = x

      not_quite_isomorphic2: NotQuiteIsomorphicToX
      x2: X = not_quite_isomorphic2
    |}
    [
      "Incompatible variable type [9]: not_quite_isomorphic is declared to have type \
       `test.NotQuiteIsomorphicToX (resolves to Union[Tuple[str, NotQuiteIsomorphicToX], int])` \
       but is used as type `test.X (resolves to Union[Tuple[int, X], int])`.";
      "Incompatible variable type [9]: x2 is declared to have type `test.X (resolves to \
       Union[Tuple[int, X], int])` but is used as type `test.NotQuiteIsomorphicToX (resolves to \
       Union[Tuple[str, NotQuiteIsomorphicToX], int])`.";
    ];
  (* Unrolling an equirecursive type still makes it equivalent to the original recursive type. *)
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      unrolled: Tuple[int, X]
      x: X = unrolled
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      unrolled: Tuple[int, X]
      unrolled2: Tuple[int, X] = unrolled
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      unrolled_union: Union[int, Tuple[int, X]]
      x: X = unrolled_union

      x2: X
      unrolled_union2: Union[int, Tuple[int, X]] = x2
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      x: X
      unrolled_multiple_times: Union[int, Tuple[int, Union[int, Tuple[int, X]]]] = x

      unrolled_multiple_times2: Union[int, Tuple[int, Union[int, Tuple[int, X]]]]
      x2: X = unrolled_multiple_times2
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      unrolled_once: Union[int, Tuple[int, X]]
      unrolled_multiple_times: Union[int, Tuple[int, Union[int, Tuple[int, X]]]]
      unrolled_once = unrolled_multiple_times

      unrolled_once2: Union[int, Tuple[int, X]]
      unrolled_multiple_times2: Union[int, Tuple[int, Union[int, Tuple[int, X]]]]
      unrolled_multiple_times2 = unrolled_once2
    |}
    [];
  (* Cannot assign a recursive type to a concrete type *)
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      x: X
      y: Union[int, Tuple[int, int]] = x
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `Union[Tuple[int, int], int]` \
       but is used as type `test.X (resolves to Union[Tuple[int, X], int])`.";
    ];
  (* Fixpoint should not blow up on a loop that constructs a recursive type. *)
  assert_type_errors
    {|
      from typing import Tuple, Union

      X = Union[int, Tuple[int, "X"]]

      def foo(x: X, n: int) -> X:
        result = x

        for i in range(n):
          result = (i, result)
          reveal_type(result)

        reveal_type(result)
        return result
    |}
    [
      "Revealed type [-1]: Revealed type for `result` is `Tuple[int, test.X (resolves to \
       Union[Tuple[int, X], int])]`.";
      "Revealed type [-1]: Revealed type for `result` is `test.X (resolves to Union[Tuple[int, X], \
       int])`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]

      def foo(tree: Tree, some_bool: bool) -> Tree:
        if some_bool:
          x = 42
        else:
          x = (1, (2, tree))
        return x
    |}
    [];
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
      Unrolled = Union[int, Tuple[Union[int, Tuple["Unrolled", "Unrolled"]], "Unrolled"]]

      def foo(some_bool: bool) -> Tree:
        tree: Tree
        unrolled_tree: Unrolled
        if some_bool:
          x = tree
        else:
          x = unrolled_tree
        return x
    |}
    [];
  Type.RecursiveType.Namespace.reset ();
  assert_type_errors
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
      # str instead of int.
      Wrong = Union[int, Tuple[Union[str, Tuple["Wrong", "Wrong"]], "Wrong"]]

      def foo(some_bool: bool) -> Tree:
        tree: Tree
        wrong_unrolled_tree: Wrong
        if some_bool:
          x = tree
        else:
          x = wrong_unrolled_tree
        return x
    |}
    [
      "Incompatible return type [7]: Expected `test.Tree (resolves to Union[Tuple[Tree, Tree], \
       int])` but got `$RecursiveType1 (resolves to Union[Tuple[Union[Tuple[$RecursiveType1, \
       $RecursiveType1], str], $RecursiveType1], Tuple[$RecursiveType1, $RecursiveType1], int])`.";
    ];
  assert_type_errors
    {|
      from typing import Final
      from typing_extensions import Literal
      x: Final[str] = "x"
      y: Literal["y"] = "y"
      reveal_type(x)
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str` (inferred: \
       `typing_extensions.Literal['x']`, final).";
      "Revealed type [-1]: Revealed type for `y` is `typing_extensions.Literal['y']`.";
    ];
  assert_type_errors
    {|
      x: str = "x"
      reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str` (inferred: \
       `typing_extensions.Literal['x']`).";
    ];
  assert_type_errors
    {|
      from typing_extensions import TypeAlias
      MyInt = int
      X: TypeAlias = "MyInt"
      y: X
      reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `int`."];
  assert_type_errors
    {|
      from typing import List, Union
      X = List[Union[int, "X"]]

      def foo() -> None:
        x: X
        y = x[0]
        reveal_type(y)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Union[int, test.X (resolves to List[Union[X, \
       int]])]`.";
    ];
  assert_type_errors
    {|
      from typing import Dict, Union
      D = Dict[str, Union[str, "D"]]

      def foo(d: D) -> None:
        y = d["hello"]
        reveal_type(y)
        if isinstance(y, str):
          reveal_type(y)
        else:
          z = y["world"]
          reveal_type(z)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Union[str, test.D (resolves to Dict[str, \
       Union[D, str]])]`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Revealed type [-1]: Revealed type for `z` is `Union[str, test.D (resolves to Dict[str, \
       Union[D, str]])]`.";
    ];
  (* Forbid directly-recursive aliases. *)
  assert_type_errors
    {|
      from typing import Union
      D = Union[int, "D"]
      D2 = Union[int, "D2"]

      def foo() -> None:
        d: D
        reveal_type(d)
        d2: D2
        d = d2
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `D` has no type specified.";
      "Missing global annotation [5]: Globally accessible variable `D2` has no type specified.";
      "Undefined or invalid type [11]: Annotation `D` is not defined as a type.";
      "Revealed type [-1]: Revealed type for `d` is `typing.Any`.";
      "Undefined or invalid type [11]: Annotation `D2` is not defined as a type.";
    ];
  assert_type_errors
    {|
      from typing import List, Union

      NestedList = List[Union[int, "NestedList"]]

      def pass_spurious_parameter(x: NestedList[int]) -> None:
        reveal_type(x)
    |}
    (* TODO(T78935633): We should raise an error on parameters to non-generic recursive alias. *)
    [
      "Revealed type [-1]: Revealed type for `x` is `test.NestedList (resolves to \
       List[Union[NestedList, int]])`.";
    ];
  (* TODO(T82613757): Generic recursive aliases are unsupported as of now. *)
  assert_type_errors
    {|
      from typing import Tuple, TypeVar, Union

      T = TypeVar("T")
      GenericTree = Union[T, Tuple["GenericTree[T]", "GenericTree[T]"]]

      def foo(x: GenericTree[int]) -> None:
        reveal_type(x)
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `GenericTree` has no type \
       specified.";
      "Undefined or invalid type [11]: Annotation `GenericTree` is not defined as a type.";
      "Revealed type [-1]: Revealed type for `x` is `unknown`.";
    ];
  (* Aliases that refer to recursive aliases. *)
  assert_type_errors
    {|
      from typing import List, Union

      X = List["X"]

      Y = Union[int, X]

      def foo() -> None:
        y: Y
        y == y
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `Union[int, test.X (resolves to List[X])]`."];
  assert_type_errors
    {|
      from typing import List, Sequence, Union

      class Foo: ...

      X = Union[
          Sequence["X"],
          List["X"]
      ]

      Y = Union[Foo, X]

      def foo() -> None:
        y: Y
        y == y
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Union[Foo, test.X (resolves to Union[List[X], \
       Sequence[X]])]`.";
    ];
  assert_type_errors
    {|
      from typing import List, Sequence, Union

      class Foo: ...

      X = Union[
          Sequence["X"],
          List["X"]
      ]

      Y = Union[Foo, X]
      Z = List[Y]

      def foo() -> None:
        z: Z
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `z` is `List[Union[Foo, test.X (resolves to \
       Union[List[X], Sequence[X]])]]`.";
    ];
  ()


let test_variadic_tuples context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: Tuple[int, Unpack[Ts]]) -> Tuple[bool, Unpack[Ts]]: ...

      def bar() -> None:
        x: Tuple[int, str, bool]
        y = foo(x)
        reveal_type(y)

        x2: Tuple[int]
        y2 = foo(x2)
        reveal_type(y2)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[bool, str, bool]`.";
      "Revealed type [-1]: Revealed type for `y2` is `Tuple[bool]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: Tuple[int, Unpack[Ts], str]) -> Tuple[bool, Unpack[Ts]]: ...

      def bar() -> None:
        x: Tuple[int]
        foo(x)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `typing.Tuple[int, *test.Ts, str]` but got `Tuple[int]`.";
    ];
  (* We should be able to typecheck the body of a generic variadic function. *)
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def add_int(xs: Tuple[Unpack[Ts]]) -> Tuple[int, Unpack[Ts]]: ...
      def remove_int(xs: Tuple[int, Unpack[Ts]]) -> Tuple[Unpack[Ts]]: ...

      def generic_function(xs: Tuple[Unpack[Ts]]) -> None:
        y = remove_int(add_int(xs))
        reveal_type(y)

        add_int(remove_int(xs))
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[*test.Ts]`.";
      "Incompatible parameter type [6]: In call `remove_int`, for 1st positional only parameter \
       expected `typing.Tuple[int, *test.Ts]` but got `typing.Tuple[*test.Ts]`.";
    ];
  (* We should not infer Tuple[int|bool, str|bool] for Ts. That would surprise most users who would
     expect that the Ts was bound to at least one of the concrete types they specified. *)
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def expects_same_tuples(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]]) -> Tuple[Unpack[Ts]]: ...

      def bar() -> None:
        tuple1: Tuple[int, str]
        tuple2: Tuple[bool, bool]
        expects_same_tuples(tuple1, tuple2)
     |}
    [
      "Incompatible parameter type [6]: In call `expects_same_tuples`, for 2nd positional only \
       parameter expected `typing.Tuple[*test.Ts]` but got `Tuple[bool, bool]`.";
    ];
  (* Length mismatch. *)
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def expects_same_tuples(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]]) -> Tuple[Unpack[Ts]]: ...

      def bar() -> None:
        tuple1: Tuple[int, str]
        shorter_tuple: Tuple[bool]
        expects_same_tuples(tuple1, shorter_tuple)
     |}
    [
      "Incompatible parameter type [6]: In call `expects_same_tuples`, for 2nd positional only \
       parameter expected `typing.Tuple[*test.Ts]` but got `Tuple[bool]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def expects_same_tuples(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]]) -> Tuple[Unpack[Ts]]: ...

      def bar() -> None:
        tuple1: Tuple[int, str]
        shorter_tuple: Tuple[bool]
        expects_same_tuples(tuple1, shorter_tuple)
     |}
    [
      "Incompatible parameter type [6]: In call `expects_same_tuples`, for 2nd positional only \
       parameter expected `typing.Tuple[*test.Ts]` but got `Tuple[bool]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def add_int(xs: Tuple[Unpack[Tuple[str, ...]]]) -> Tuple[int, Unpack[Tuple[str, ...]]]: ...

      def foo() -> None:
        xs: Tuple[str, str]
        y = add_int(xs)
        reveal_type(y)

        invalid: Tuple[int, str]
        add_int(invalid)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[str, ...]]`.";
      "Incompatible parameter type [6]: In call `add_int`, for 1st positional only parameter \
       expected `typing.Tuple[str, ...]` but got `Tuple[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(xs: Tuple[Unpack[Ts]]) -> Tuple[Unpack[Ts]]: ...

      def baz() -> None:
       	unbounded_tuple: Tuple[int, ...]
       	y = foo(unbounded_tuple)
       	reveal_type(y)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, ...]`."];
  assert_type_errors
    {|
      from typing import Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      def foo(xs: Tuple[T, Unpack[Tuple[str, ...]]]) -> T: ...

      def baz() -> None:
       	some_tuple: Tuple[int, str, str]
       	y = foo(some_tuple)
       	reveal_type(y)

       	invalid_tuple: Tuple[int, str, int]
       	foo(invalid_tuple)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `typing.Tuple[Variable[T], *Tuple[str, ...]]` but got `Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      from typing import Any, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      N = TypeVar("N", bound=int)

      def foo(x: Tuple[N, Unpack[Ts]]) -> Tuple[Unpack[Ts], N]: ...

      def bar() -> None:
        x: Tuple[Any, ...]
        y = foo(x)
        reveal_type(y)

        x2: Tuple[int, ...]
        y2 = foo(x2)
        reveal_type(y2)
    |}
    [
      "Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[*Tuple[typing.Any, ...], \
       typing.Any]`.";
      "Revealed type [-1]: Revealed type for `y2` is `typing.Tuple[*Tuple[int, ...], int]`.";
    ];
  assert_type_errors
    {|
      from typing import Any, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      N = TypeVar("N", bound=int)

      def foo(x: Tuple[N, Unpack[Ts]]) -> Tuple[Unpack[Ts], N]: ...

      def bar() -> None:
        x_error: Tuple[str, ...]
        y_error = foo(x_error)
        reveal_type(y_error)
    |}
    [
      "Incomplete type [37]: Type `typing.Tuple[*test.Ts, Variable[N (bound to int)]]` inferred \
       for `y_error` is incomplete, add an explicit annotation.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `typing.Tuple[Variable[N (bound to int)], *test.Ts]` but got `typing.Tuple[str, ...]`.";
      "Revealed type [-1]: Revealed type for `y_error` is `typing.Tuple[*Tuple[typing.Any, ...], \
       typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import Any, Tuple, TypeVar

      N = TypeVar("N", bound=int)

      def foo(x: Tuple[N]) -> Tuple[N]: ...

      def bar() -> None:
        x: Tuple[int, ...]
        y = foo(x)
        reveal_type(y)

        x_error: Tuple[str, ...]
        foo(x_error)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int]`.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Tuple[Variable[N (bound to int)]]` but got `typing.Tuple[str, ...]`.";
    ];
  ()


let test_variadic_classes context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[Unpack[Ts]]): ...

      def add_bool(x: Tensor[int, Unpack[Ts], str]) -> Tensor[bool, Unpack[Ts]]: ...

      def foo() -> None:
        x: Tensor[int, bool, str]
        y = add_bool(x)
        reveal_type(y)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `Tensor[bool, bool]`."];
  (* Expect the same Tensor type for both parameters. We don't infer `Ts = Tuple[int | bool, str |
     bool]` even though it is sound, because it is unintuitive. *)
  assert_type_errors
    {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[Unpack[Ts]]): ...

      def expects_same_tensors(x: Tensor[Unpack[Ts]], y: Tensor[Unpack[Ts]]) -> Tensor[Unpack[Ts]]: ...

      def bar() -> None:
        tensor: Tensor[int, str]
        tensor2: Tensor[bool, bool]
        y = expects_same_tensors(tensor, tensor2)
        reveal_type(y)
     |}
    [
      "Incompatible parameter type [6]: In call `expects_same_tensors`, for 2nd positional only \
       parameter expected `Tensor[*test.Ts]` but got `Tensor[bool, bool]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tensor[int, str]`.";
    ];
  (* Length mismatch. *)
  assert_type_errors
    {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[Unpack[Ts]]): ...

      def expects_same_length(xs: Tensor[Unpack[Ts]], ys: Tensor[Unpack[Ts]]) -> Tensor[Unpack[Ts]]: ...

      def bar() -> None:
        xs: Tensor[int, str]
        ys: Tensor[bool]
        expects_same_length(xs, ys)
     |}
    [
      "Incompatible parameter type [6]: In call `expects_same_length`, for 2nd positional only \
       parameter expected `Tensor[*test.Ts]` but got `Tensor[bool]`.";
    ];
  (* Tensor is covariant in its shape, since the shape is immutable. However, it is invariant in the
     unary datatype. *)
  assert_type_errors
    {|
      from typing import Generic, List, Protocol, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      class Base: ...
      class Child(Base): ...

      def foo(x: Tensor[float, Base, Base]) -> None: ...

      def bar() -> None:
        child: Tensor[float, Child, Child]
        foo(child)

        int_tensor: Tensor[int, Base, Base]
        foo(int_tensor)
     |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Tensor[float, Base, Base]` but got `Tensor[int, Base, Base]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      Ts = TypeVarTuple("Ts")

      Tin = TypeVar("Tin")
      Tout = TypeVar("Tout")

      class Tensor(Generic[Unpack[Ts]]): ...

      class Linear(Generic[Tin, Tout]):
        """Transform the last dimension from Tin to Tout."""

        def __init__(self, in_dimension: Tin, out_dimension: Tout) -> None:
          self.in_dimension = in_dimension
          self.out_dimension = out_dimension

        def __call__(self, x: Tensor[Unpack[Ts], Tin]) -> Tensor[Unpack[Ts], Tout]: ...

      def bar() -> None:
        x: Tensor[L[10], L[20]]
        layer1 = Linear(20, 30)
        layer2 = Linear(30, 40)
        layer3 = Linear(40, 50)
        y = layer3(layer2(layer1(x)))
        reveal_type(y)

        shape_mismatch = (10, 21)
        layer1(shape_mismatch)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tensor[typing_extensions.Literal[10], \
       typing_extensions.Literal[50]]`.";
      "Incompatible parameter type [6]: In call `Linear.__call__`, for 1st positional only \
       parameter expected `Tensor[*test.Ts, typing_extensions.Literal[20]]` but got \
       `Tuple[typing_extensions.Literal[10], typing_extensions.Literal[21]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[Unpack[Ts]]):
        def some_method(self, x: Tensor[Unpack[Ts]]) -> None: ...

      def bar() -> None:
        xs: Tensor[int, str]
        xs.some_method(xs)
     |}
    [];
  assert_type_errors
    {|
      from typing import Generic, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      def bar() -> None:
        x = Tensor.__getitem__
        reveal_type(x)
     |}
    [
      "Revealed type [-1]: Revealed type for `x` is \
       `BoundMethod[typing.Callable(typing.GenericMeta.__getitem__)[[Named(self, unknown), \
       typing.Tuple[typing.Type[Variable[T]], typing.Any]], typing.Type[Tensor[Variable[T], \
       typing.Any]]], typing.Type[Tensor]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, List, Protocol, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class VariadicProtocol(Protocol[T, Unpack[Ts]]):
        def foo(self, x: Tuple[T, Unpack[Ts]]) -> None: ...

      class Tensor(Generic[Unpack[Ts]]):
        """This implements VariadicProtocol with T = List[int] and Ts = Tuple[Unpack[Ts]]."""

        def foo(self, x: Tuple[List[int], Unpack[Ts]]) -> None:...


      def accepts_variadic_protocol(x: VariadicProtocol[T, Unpack[Ts]]) -> VariadicProtocol[T, Unpack[Ts]]: ...

      def bar() -> None:
        x: Tensor[int, str]
        y = accepts_variadic_protocol(x)
        reveal_type(y)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `VariadicProtocol[List[int], int, str]`."];
  (* TODO(T84553937): While Tensor is indeed invariant, we should have inferred `Tensor[int, Base,
     Base]` below. *)
  assert_type_errors
    {|
      from typing import Generic, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]):
        def __init__(self, default: T, shape: Tuple[Unpack[Ts]]) -> None: ...

      class Base: ...
      class Child(Base): ...

      def expects_base(t: Tensor[int, Base, Base]) -> None: ...

      def bar() -> None:
        expects_base(Tensor(1, (Child(), Child())))
     |}
    [
      "Incompatible parameter type [6]: In call `expects_base`, for 1st positional only parameter \
       expected `Tensor[int, Base, Base]` but got `Tensor[int, Child, Child]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      FloatTensor = Tensor[float, Unpack[Ts]]

      def bar() -> None:
        x: FloatTensor[L[10], L[20]]
        reveal_type(x)

        y: FloatTensor
        reveal_type(y)
     |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Tensor[float, typing_extensions.Literal[10], \
       typing_extensions.Literal[20]]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tensor[float, *Tuple[typing.Any, ...]]`.";
    ];
  assert_type_errors
    {|
      from typing import Generic, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      def get_last_type(t: Tensor[float, Unpack[Tuple[int, ...]], T]) -> T: ...

      def bar() -> None:
        x: Tensor[float, L[10], L[20]]
        y = get_last_type(x)
        reveal_type(y)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `typing_extensions.Literal[20]`."];
  assert_type_errors
    {|
      from typing import Generic, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      # pyre-ignore[24]: Generic type `Tensor` expects at least 1 type parameter.
      def accept_arbitrary_tensor(t: Tensor) -> Tensor: ...

      def bar() -> None:
        x: Tensor[float, L[10], L[20]]
        y = accept_arbitrary_tensor(x)
        reveal_type(y)

        # pyre-ignore[24]: Generic type `Tensor` expects at least 1 type parameter.
        no_parameters: Tensor
        accept_arbitrary_tensor(no_parameters)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `Tensor[typing.Any, *Tuple[typing.Any, ...]]`."];
  assert_type_errors
    {|
      from typing import Generic, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[T, Unpack[Ts]]): ...

      def strip_last(x: Tensor[int, Unpack[Ts], int]) -> Tensor[int, Unpack[Ts]]: ...

      def bar() -> None:
        invalid: Tensor[int, L[10], str]
        y = strip_last(invalid)
        reveal_type(y)
     |}
    [
      "Incomplete type [37]: Type `Tensor[int, *test.Ts]` inferred for `y` is incomplete, add an \
       explicit annotation.";
      "Incompatible parameter type [6]: In call `strip_last`, for 1st positional only parameter \
       expected `Tensor[int, *test.Ts, int]` but got `Tensor[int, int, str]`.";
      "Revealed type [-1]: Revealed type for `y` is `Tensor[int, *Tuple[typing.Any, ...]]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Generic, Tuple, TypeVar
      from pyre_extensions import ParameterSpecification, TypeVarTuple, Unpack
      from typing_extensions import Literal as L

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")
      TParams = ParameterSpecification("TParams")

      class Tensor(Generic[T, TParams, Unpack[Ts]]):
        def __init__(self, f: Callable[TParams, T], shape: Tuple[Unpack[Ts]]) -> None:
          self.f = f
          self.shape = shape


      def bar() -> None:
        tensor: Tensor[float, [int, str], int, str]
        y = tensor.f( *tensor.shape)
        reveal_type(y)

        tensor.f("wrong argument")
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `float`.";
      "Missing argument [20]: PositionalOnly call expects argument in position 1.";
    ];
  ()


let test_variadic_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Callable, Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def make_tuple(leave_this_out: int, *args: Unpack[Ts], message: str) -> Tuple[Unpack[Ts], bool]: ...

      def foo() -> None:
        y = make_tuple(1, 2, 3, message="hello")
        reveal_type(y)

        y2 = make_tuple(1, message="hello")
        reveal_type(y2)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[typing_extensions.Literal[2], \
       typing_extensions.Literal[3], bool]`.";
      "Revealed type [-1]: Revealed type for `y2` is `Tuple[bool]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def make_tuple(leave_this_out: int, *args: Unpack[Tuple[int, Unpack[Ts], str]], message: str) -> Tuple[int, Unpack[Ts], str]:
        return args

      def foo() -> None:
        y = make_tuple(1, 2, 3, "has to end with a string", message="hello")
        reveal_type(y)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, typing_extensions.Literal[3], str]`.";
    ];
  (* Unpack an unbounded tuple. *)
  assert_type_errors
    {|
      from typing import Callable, Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def make_tuple( *args: Unpack[Tuple[int, Unpack[Ts], str]]) -> None: ...

      def foo(x: Tuple[Unpack[Ts]]) -> None:
        unbounded_tuple: Tuple[int, ...]
        make_tuple(1, *unbounded_tuple, "foo")

        make_tuple( *unbounded_tuple, "foo")

        unbounded_str_tuple: Tuple[str, ...]
        make_tuple( *unbounded_str_tuple, "foo")
     |}
    [
      "Invalid argument [32]: Argument types `*Tuple[str, ...], typing_extensions.Literal['foo']` \
       are not compatible with expected variadic elements `int, *test.Ts, str`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def make_tuple( *args: Unpack[Tuple[int, Unpack[Ts], str]]) -> None: ...

      def foo(x: Tuple[Unpack[Ts]]) -> None:
        make_tuple(1, 2)
        make_tuple(1, *x, *x, "foo")
     |}
    [
      "Invalid argument [32]: Argument types `typing_extensions.Literal[1], \
       typing_extensions.Literal[2]` are not compatible with expected variadic elements `int, \
       *test.Ts, str`.";
      "Invalid argument [32]: Variadic type variable `int, *test.Ts, str` cannot be made to \
       contain `typing_extensions.Literal[1], *test.Ts, *test.Ts, \
       typing_extensions.Literal['foo']`; concatenation of multiple variadic type variables is not \
       yet implemented.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def strip_int_parameter(f: Callable[[int, Unpack[Ts]], None]) -> Callable[[Unpack[Ts]], None]: ...

      def foo(x: int, y: str, z: bool) -> None: ...

      def baz() -> None:
       	f = strip_int_parameter(foo)
       	reveal_type(f)

       	# Valid
       	f("hello", True)

       	# Error
       	f("hello")
     |}
    [
      "Revealed type [-1]: Revealed type for `f` is `typing.Callable[[str, bool], None]`.";
      "Missing argument [20]: PositionalOnly call expects argument in position 1.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def strip_int_parameter(f: Callable[[int, Unpack[Ts]], None]) -> Callable[[Unpack[Ts]], None]: ...

      def no_leading_int(y: str, z: bool) -> None: ...

      def foo() -> None:
       	strip_int_parameter(no_leading_int)
     |}
    [
      "Incompatible parameter type [6]: In call `strip_int_parameter`, for 1st positional only \
       parameter expected `typing.Callable[[Variable(int, *test.Ts)], None]` but got \
       `typing.Callable(no_leading_int)[[Named(y, str), Named(z, bool)], None]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Generic, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      T = TypeVar("T")
      Ts = TypeVarTuple("Ts")

      class Tensor(Generic[Unpack[Ts]]):
        def some_method(self, *args: Unpack[Ts]) -> Tuple[Unpack[Ts]]: ...

      def bar() -> None:
        x: Tensor[int, str]
        y = x.some_method(1, "hello")
        reveal_type(y)

        x.some_method("invalid")
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[int, str]`.";
      "Missing argument [20]: Call `Tensor.some_method` expects argument in position 2.";
    ];
  assert_type_errors
    {|
      from typing import Callable, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      T = TypeVar("T")

      def apply(f: Callable[[Unpack[Ts]], T], *args: Unpack[Ts]) -> T: ...

      def foo(x: int, y: str, z: bool) -> str: ...

      def bar(a: int, b: str, c: bool) -> None:
        y = apply(foo, a, b, c)
        reveal_type(y)

        apply(foo, a, b)
     |}
    [
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Invalid argument [32]: Argument types `int, str` are not compatible with expected variadic \
       elements `*test.Ts`.";
    ];
  (* It should be fine to pass a subclass to a function expecting the base class. *)
  assert_type_errors
    {|
      from typing import Callable, Tuple, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      T = TypeVar("T")

      def apply(f: Callable[[Unpack[Ts]], T], *args: Unpack[Ts]) -> T: ...

      class Base: ...
      class Child(Base): ...

      def expects_base(x: int, y: str, z: Base) -> str: ...
      def expects_child(x: int, y: str, z: Child) -> str: ...

      def bar() -> None:
        child: Child
        apply(expects_base, 1, "hello", child)

        base: Base
        apply(expects_child, 1, "hello", base)
     |}
    [
      "Invalid argument [32]: Argument types `typing_extensions.Literal[1], \
       typing_extensions.Literal['hello'], test.Base` are not compatible with expected variadic \
       elements `*test.Ts`.";
    ];
  ()


let test_self_type context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: float = 0.0) -> None:
          self.scale = scale

        def set_scale(self, scale: float) -> Self:
          reveal_type(self)
          self.scale = scale
          return self

      class Circle(Shape):
        def __init__(self, scale: float = 0.0, radius: float = 0.0) -> None:
          super(Circle, self).__init__(scale)
          self.radius = radius

        def set_radius(self, radius: float) -> Self:
          self.radius = radius
          return self

      def foo() -> None:
        circle: Circle
        y = circle.set_scale(0.5).set_radius(2.7)
        reveal_type(y)
     |}
    [
      "Revealed type [-1]: Revealed type for `self` is `Variable[_Self_test_Shape__ (bound to \
       Shape)]`.";
      "Revealed type [-1]: Revealed type for `y` is `Circle`.";
    ];
  (* Same example but with protocols. *)
  assert_type_errors
    {|
      from typing_extensions import Self
      from typing import Protocol

      class ShapeProtocol(Protocol):
        def __init__(self, scale: float = 0.0) -> None:
          self.scale = scale

        def set_scale(self, scale: float) -> Self:
          reveal_type(self)
          self.scale = scale
          return self

      class CircleProtocol(ShapeProtocol, Protocol):
        def __init__(self, scale: float = 0.0, radius: float = 0.0) -> None:
          super(CircleProtocol, self).__init__(scale)
          self.radius = radius

        def set_radius(self, radius: float) -> Self:
          self.radius = radius
          return self

      def foo() -> None:
        circle: CircleProtocol
        y = circle.set_scale(0.5).set_radius(2.7)
        reveal_type(y)
     |}
    [
      "Revealed type [-1]: Revealed type for `self` is `Variable[_Self_test_ShapeProtocol__ (bound \
       to ShapeProtocol)]`.";
      "Revealed type [-1]: Revealed type for `y` is `CircleProtocol`.";
    ];
  assert_type_errors
    {|
      from typing_extensions import Self
      from typing import Protocol

      class ShapeProtocol(Protocol):
        def set_scale(self, scale: float) -> Self: ...

      class ReturnSelf:
        scale: float = 1.0

        def set_scale(self, scale: float) -> Self:
          self.scale = scale
          return self

      class ReturnConcreteShape:
        scale: float = 1.0

        def set_scale(self, scale: float) -> ReturnConcreteShape:
          self.scale = scale
          return self

      class BadReturnType:
        scale: float = 1.0

        def set_scale(self, scale: float) -> int:
          self.scale = scale
          return 42

      def foo(shape: ShapeProtocol) -> None:
        y = shape.set_scale(0.5)
        reveal_type(y)

      def main() -> None:
        return_self_shape: ReturnSelf
        return_concrete_shape: ReturnConcreteShape
        bad_return_type: BadReturnType

        foo(return_self_shape)
        foo(return_concrete_shape)
        foo(bad_return_type)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `ShapeProtocol`.";
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `ShapeProtocol` but got `BadReturnType`.";
    ];
  (* TODO(T103914175): Allow overriding a method that uses Self. *)
  assert_type_errors
    {|
      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: float = 0.0) -> None:
          self.scale = scale

        def set_scale(self, scale: float) -> Self:
          self.scale = scale
          return self

      class Circle(Shape):
        def set_scale(self, scale: float) -> Self:
          self.scale = scale + 1.0
          return self
     |}
    [
      "Inconsistent override [14]: `test.Circle.set_scale` overrides method defined in `Shape` \
       inconsistently. Parameter of type `Variable[_Self_test_Circle__ (bound to Circle)]` is not \
       a supertype of the overridden parameter `Variable[_Self_test_Shape__ (bound to Shape)]`.";
    ];
  (* Generic class. *)
  assert_type_errors
    {|
      from typing_extensions import Self
      from typing import Generic, TypeVar

      T = TypeVar("T")

      class Container(Generic[T]):
        def __init__(self, value: T) -> None:
          self.value = value

        def set_value(self, value: T) -> Self:
          reveal_type(self)
          self.value = value
          return self

      class ChildContainer(Container[T]): ...

      class ConcreteContainer(ChildContainer[int]): ...

      def foo() -> None:
        child: ChildContainer[str]
        y = child.set_value("hello")
        reveal_type(y)
        child.set_value(42)

        concrete: ConcreteContainer
        y2 = concrete.set_value(42)
        reveal_type(y2)
        concrete.set_value("bad")
     |}
    [
      (* TODO(T103918696): Don't complain about the synthetic TypeVar bound. *)
      "Invalid type parameters [24]: Generic type `Container` expects 1 type parameter.";
      "Revealed type [-1]: Revealed type for `self` is `Variable[_Self_test_Container__ (bound to \
       Container[typing.Any])]`.";
      "Revealed type [-1]: Revealed type for `y` is `ChildContainer[str]`.";
      "Incompatible parameter type [6]: In call `Container.set_value`, for 1st positional only \
       parameter expected `str` but got `int`.";
      "Revealed type [-1]: Revealed type for `y2` is `ConcreteContainer`.";
      "Incompatible parameter type [6]: In call `Container.set_value`, for 1st positional only \
       parameter expected `int` but got `str`.";
    ];
  (* Nested class using Self. *)
  assert_type_errors
    {|
      from typing_extensions import Self

      class Outer:
        class Shape:
          def __init__(self, scale: float = 0.0) -> None:
            self.scale = scale

          def set_scale(self, scale: float) -> Self:
            self.scale = scale
            return self

        class Circle(Shape): ...

      def foo() -> None:
        circle: Outer.Circle
        y = circle.set_scale(0.5)
        reveal_type(y)
     |}
    ["Revealed type [-1]: Revealed type for `y` is `Outer.Circle`."];
  assert_type_errors
    {|
      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: float) -> None: ...

        @classmethod
        def from_config(cls, config: dict[str, float]) -> Self:
          reveal_type(cls)
          return cls(config["scale"])

      class Circle(Shape): ...

      def foo() -> None:
        circle = Circle.from_config({"scale": 7.0})
        reveal_type(circle)
     |}
    [
      "Revealed type [-1]: Revealed type for `cls` is `typing.Type[Variable[_Self_test_Shape__ \
       (bound to Shape)]]`.";
      "Revealed type [-1]: Revealed type for `circle` is `Circle`.";
    ];
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
         "user_defined_parameter_variadics" >:: test_user_defined_parameter_specification_classes;
         "duplicate_type_variables" >:: test_duplicate_type_variables;
         "generic_aliases" >:: test_generic_aliases;
         "recursive_aliases" >:: test_recursive_aliases;
         "variadic_tuples" >:: test_variadic_tuples;
         "variadic_classes" >:: test_variadic_classes;
         "variadic_callables" >:: test_variadic_callables;
         "self_type" >:: test_self_type;
       ]
  |> Test.run

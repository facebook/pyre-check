(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_assign context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x = 'string'  # Reassignment is okay.
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: str = 1
        reveal_type(x)
        x = 1
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `str` "
      ^ "but is used as type `int`.";
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Incompatible variable type [9]: x is declared to have type `str` "
      ^ "but is used as type `int`.";
    ];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.Any) -> None:
        y: int = x
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x += 'asdf'
    |}
    [
      "Incomplete type [37]: Type `pyre_extensions.IntExpression[1 + N2]` inferred for `x` is \
       incomplete, add an explicit annotation.";
      "Unsupported operand [58]: `+` is not supported for operand types `int` and `str`.";
    ];

  (* Prune `undeclared` from assignments. *)
  assert_type_errors
    {|
      def foo() -> None:
        y = x
        z = y
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."];
  assert_type_errors
    {|
      def foo() -> None:
        y = [x]
        z = y
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."];
  assert_type_errors
    {|
      import typing
      from typing import Final
      x: Final[int] = 3
      x = 200
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
  assert_type_errors
    {|
      import typing
      from typing import Final
      def foo() -> None:
        x: Final[int] = 3
        x = 200
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
  assert_type_errors
    {|
      from typing import Final
      i = 0
      while i < 10:
        i += 1
        x: Final[int] = i
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
  assert_type_errors
    {|
      from typing import Final
      class A:
        x: Final[int] = 10
      A.x = 20
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `A.x`."];
  assert_type_errors
    {|
      from typing import ClassVar
      class Base:
          y: ClassVar[int] = 0
      Base.y = 100 # ok
      b = Base()
      b.y = 12 # error
    |}
    [
      "Invalid assignment [41]: Assigning to class variable through instance, did you mean to \
       assign to `Base.y` instead?";
    ];
  assert_type_errors
    {|
      from typing import ClassVar
      class Base:
          y: ClassVar[int] = 0

      x = Base
      x.y = 100
    |}
    [];
  assert_type_errors
    {|
      from dataclasses import dataclass
      @dataclass(frozen=True)
      class A:
          foo:int = 1
      a = A()
      a.foo = 2
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `a.foo`."];
  assert_type_errors
    {|
      class A:
          @property
          def foo(self) -> int:
            ...
      a = A()
      a.foo = 1
    |}
    ["Invalid assignment [41]: `a.foo` cannot be reassigned. It is a read-only property."];
  assert_type_errors
    {|
      class A:
          @property
          def foo(self) -> int:
            ...
          @foo.setter
          def foo(self, value: int) -> None:
            ...
      a = A()
      a.foo = 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      x = typing.List[int]
      reveal_type(x)

      y = typing.Mapping[int, str]
      reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Type[typing.List[int]]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Type[typing.Mapping[int, str]]`.";
    ];
  assert_type_errors
    {|
      import typing
      x = typing.List["foo"]
      reveal_type(x)
      class foo: pass
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Type[typing.List[foo]]`."];
  assert_type_errors
    {|
      import typing
      x = typing.List["foo"]
      reveal_type(x)
    |}
    [
      "Incomplete type [37]: Type `typing.Type[typing.List[Variable[_T]]]` inferred for `x` is \
       incomplete, add an explicit annotation.";
      "Missing global annotation [5]: Globally accessible variable `x` has no type specified.";
      "Incompatible parameter type [6]: In call `typing.GenericMeta.__getitem__`, for 1st \
       positional only parameter expected `Type[Variable[_T]]` but got `str`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Type[typing.List[typing.Any]]`.";
    ];
  assert_type_errors
    {|
      from typing import Type
      def foo(x: Type[Exception]) -> Exception:
         y: Type[Exception] = x
         return y()
    |}
    [];
  assert_type_errors {|
      def foo( *args: str) -> None:
       args = args[1:]
    |} [];
  assert_type_errors
    {|
      def foo( *args: str) -> None:
       args = [1, 2, 3]
    |}
    ["Incompatible variable type [9]: Unable to unpack `List[int]`, expected a tuple."];
  assert_type_errors
    {|
      def foo( *args: str) -> None:
       args = (1, 2, 3)
    |}
    [
      "Incompatible variable type [9]: args is declared to have type `typing.Tuple[str, ...]` but \
       is used as type `Tuple[int, int, int]`.";
    ];

  assert_type_errors
    {|
      def foo( **kwargs: str) -> None:
       kwargs = {"foo": "bar"}
    |}
    [];
  assert_type_errors
    {|
      def foo( **kwargs: str) -> None:
       kwargs = {"foo": 1}
    |}
    [
      "Incompatible variable type [9]: kwargs is declared to have type `Dict[str, str]` but is \
       used as type `Dict[str, int]`.";
    ];
  assert_type_errors
    {|
      from typing import Dict, Union
      D = Dict[str, Union[str, int]]
      def foo(x: D) -> None:
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Dict[str, Union[int, str]]`."];
  assert_default_type_errors
    {|
      from typing import Any

      def i() -> str:
        x: Any
        reveal_type(x)
        x = 42
        reveal_type(x)
        return x
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any

      def h() -> str:
        x: Any = 42
        reveal_type(x)
        return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_type_errors
    {|
      def f() -> None:
        x: int = 25
        x: str = "hi"
    |}
    ["Illegal annotation target [35]: Target `x` cannot be annotated after it is first declared."];
  assert_type_errors
    {|
      def f() -> None:
        x: int = 42
        def g(x: str) -> None:
          print(x)
        g("hello")
    |}
    [];
  assert_type_errors
    {|
      def f() -> None:
        x = 1
        x: int = 2
    |}
    ["Illegal annotation target [35]: Target `x` cannot be annotated after it is first declared."];
  assert_type_errors {|
      def f() -> None:
        x: int = 1
        x: int = 2
    |} [];
  assert_type_errors
    {|
      def f() -> None:
        y = 0
        while (y < 10):
          y += 1
          x: int = 2
    |}
    [];
  assert_default_type_errors
    {|
       from typing import Any, Callable
       class A:
         pass
       class B:
         def __getattr__(self, name: str) -> Any: ...
       class C:
         def __setattr__(self, name: str, value: Any) -> None: ...
       class D(C):
         pass
       class E:
         def __setattr__(self, name: str, value: int) -> None: ...
       class F:
         # pyre-ignore: this isnt consistent with object.__setattr__
         __setattr__: BoundMethod[Callable[[E, str, Any], None], E]
       class G:
         # pyre-ignore: this isnt consistent with object.__setattr__
         __setattr__: BoundMethod[Callable[[E, str, int], None], E]
       class H(C):
          def __getattr__(self, name: str) -> Any: ...

       def derp(a: A, b: B, c: C, d: D, e: E, f: F, g: G) -> None:
         a.foo = 42
         b.foo = 43
         c.foo = 44
         d.foo = 45
         e.foo = 45
         f.foo = 45
         g.foo = 45
    |}
    [
      "Undefined attribute [16]: `A` has no attribute `foo`.";
      "Undefined attribute [16]: `C` has no attribute `foo`.";
      "Undefined attribute [16]: `D` has no attribute `foo`.";
      "Undefined attribute [16]: `E` has no attribute `foo`.";
      "Undefined attribute [16]: `F` has no attribute `foo`.";
      "Undefined attribute [16]: `G` has no attribute `foo`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any
      class Foo:
          def __setattr__(self, name: str, value: Any) -> None: ...
          def __getattr__(self, name: str) -> Any: ...
      class Bar(Foo):
          pass
      class Baz(object):
          pass
      def test(foo: Foo, bar: Bar, baz: Baz) -> None:
          foo.attr = 1
          bar.attr = 1
          baz.attr = 1
    |}
    ["Undefined attribute [16]: `Baz` has no attribute `attr`."];
  assert_default_type_errors
    {|
      from typing import Any
      class Foo:
          def __getattr__(self, name: str) -> Any: ...
      class Bar(Foo):
          pass
      class Baz(object):
          def __getattr__(self, name: str) -> int: ...
      def test(foo: Foo, bar: Bar, baz: Baz) -> None:
          foo.attr = 1
          bar.attr = 1
          baz.attr = 1
    |}
    ["Undefined attribute [16]: `Baz` has no attribute `attr`."];
  assert_default_type_errors
    {|
      from typing import Any
      class Foo:
          def __getattr__(self, x: Any) -> Any:
              return x


      def test(f: Foo) -> None:
          access = f.some_attribute
          f.another_attribute = 1
    |}
    [];
  assert_type_errors
    {|
    from typing import Union

    def f() -> int:
      x: Union[int, str] = 42
      reveal_type(x)
      return x

  |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]` (inferred: \
       `typing_extensions.Literal[42]`).";
    ];
  assert_type_errors
    {|
    from typing import Union

    def f() -> int:
      x: Union[int, str]
      reveal_type(x)
      x = 42
      reveal_type(x)
      return x

  |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]` (inferred: \
       `typing_extensions.Literal[42]`).";
    ];
  assert_type_errors
    {|
    from typing import Union

    def f() -> int:
      x: Union[int, str] = "hello"
      reveal_type(x)
      x = 42
      reveal_type(x)
      return x

  |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]` (inferred: \
       `typing_extensions.Literal['hello']`).";
      "Revealed type [-1]: Revealed type for `x` is `Union[int, str]` (inferred: \
       `typing_extensions.Literal[42]`).";
    ];
  assert_default_type_errors
    {|
    from typing import Dict, Any

    def foo(p: Dict[str, bool]) -> None:
      x: Dict[str, Any] = {"field": "value"}
      reveal_type(x)
      x.update(p)
      reveal_type(x)
  |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`.";
      "Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`.";
    ];
  assert_default_type_errors
    {|
    from typing import Dict, Any

    def foo(p: Dict[str, bool]) -> None:
      x: Dict[str, Any]
      x = {"field": "value"}
      reveal_type(x)
      x.update(p)
      reveal_type(x)
      bar(x)

    def bar(f: Dict[str,str]) -> None:
      return
  |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`.";
      "Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`.";
    ];
  assert_default_type_errors
    {|
    from typing import List, Any

    def foo(p: bool) -> None:
      x: List[Any] = ["str"]
      x.append(p)
      reveal_type(x)

  |}
    ["Revealed type [-1]: Revealed type for `x` is `List[typing.Any]`."];
  assert_default_type_errors
    {|
  from typing import List

  def foo() -> None:
    x = []
    x.append("hello")
    reveal_type(x)
    x.append(True)
  |}
    ["Revealed type [-1]: Revealed type for `x` is `List[typing.Any]`."];
  assert_default_type_errors
    {|
    from typing import List, Any

    def foo() -> None:
      x: List[int] = [1]
      reveal_type(x)
  |}
    ["Revealed type [-1]: Revealed type for `x` is `List[int]`."];
  ()


let () = "assign" >::: ["check_assign" >:: test_check_assign] |> Test.run

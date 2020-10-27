(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `int.__add__` but got `str`.";
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
      "Incompatible parameter type [6]: Expected `typing.Type[Variable[_T]]` for 1st positional \
       only parameter to call `typing.GenericMeta.__getitem__` but got `str`.";
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
    ["Incompatible variable type [9]: Unable to unpack `typing.List[int]`, expected a tuple."];
  assert_type_errors
    {|
      def foo( *args: str) -> None:
       args = (1, 2, 3)
    |}
    [
      "Incompatible variable type [9]: args is declared to have type `typing.Tuple[str, ...]` but \
       is used as type `typing.Tuple[int, int, int]`.";
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
      "Incompatible variable type [9]: kwargs is declared to have type `typing.Dict[str, str]` but \
       is used as type `typing.Dict[str, int]`.";
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
  assert_default_type_errors
    {|
       from typing import Any, Callable
       class A:
         pass
       class B:
         def __getattr_(self, name: str) -> Any: ...
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
      "Undefined attribute [16]: `B` has no attribute `foo`.";
      "Undefined attribute [16]: `D` has no attribute `foo`.";
      (* TODO(T64156088): This should be accepted *)
      "Undefined attribute [16]: `E` has no attribute `foo`.";
      (* TODO(T64156088): This should be accepted *)
      "Undefined attribute [16]: `G` has no attribute `foo`.";
    ];
  ()


let () = "assign" >::: ["check_assign" >:: test_check_assign] |> Test.run

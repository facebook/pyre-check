(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_union context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: bool = True
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = False
    |}
    [
      "Incompatible attribute type [8]: Attribute `attr` declared in class `B` has type `str` but \
       is used as type `bool`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: bool = True
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = 2
    |}
    [
      "Incompatible attribute type [8]: Attribute `attr` declared in class `A` has type `bool` but \
       is used as type `int`.";
      "Incompatible attribute type [8]: Attribute `attr` declared in class `B` has type `str` but \
       is used as type `int`.";
      "Incompatible variable type [9]: y.attr is declared to have type `Union[bool, str]` but is \
       used as type `int`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr1: int = 2
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr1 = 3
    |}
    ["Undefined attribute [16]: `B` has no attribute `attr1`."];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: Union[bool, int] = True
      class B:
        attr: Union[str, int] = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = 3
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `Union[int, str]` but got `float`."];
  assert_type_errors
    {|
      from builtins import condition
      import typing
      def foo() -> typing.Union[str, int]:
        if condition():
          return 1
        else:
          return 'foo'
    |}
    [];
  assert_type_errors
    {|
      import typing
      def takes_int(a: int) -> None: ...
      def takes_str(a: str) -> None: ...

      def foo(a: typing.Union[str, int]) -> None:
        if isinstance(a, str):
          takes_str(a)
        else:
          takes_int(a)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Union[str, int, float]) -> int:
        if isinstance(a, int):
          return a
        else:
          return a
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[float, str]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    ["Incompatible return type [7]: Expected `float` but got `Variable[T <: [int, str]]`."];
  assert_type_errors
    {|
      import typing
      variable: typing.Union[typing.Optional[int], typing.Optional[str]] = None
      def ret_opt_int() -> typing.Optional[int]:
          return None
      variable = ret_opt_int()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, Undefined]) -> None:
        pass
      foo(1)
    |}
    ["Unbound name [10]: Name `Undefined` is used but not defined in the current scope."];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.int_attribute
    |}
    [];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Item `Attributes` of `typing.Union[Attributes, OtherAttributes]` \
       has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[OtherAttributes, Attributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Item `Attributes` of `typing.Union[Attributes, OtherAttributes]` \
       has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def derp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [];
  assert_type_errors
    {|
      import typing
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def herp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    ["Undefined attribute [16]: Item `Bar` of `typing.Union[Bar, Foo]` has no attribute `derp`."];

  (* We require that all elements in a union have the same method for `in`. *)
  assert_type_errors
    {|
      import typing
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItem:
        def __getitem__(self, x: int) -> Equal:
          ...
      class Contains:
        def __contains__(self, a: object) -> bool:
          ...
      def foo(a: typing.Union[GetItem, Contains]) -> None:
        5 in a
    |}
    [
      "Unsupported operand [58]: `in` is not supported for right operand type \
       `typing.Union[Contains, GetItem]`.";
    ];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Optional[typing.Union[int, str]]) -> None:
        return g(x)

      def g(x: typing.Union[typing.Optional[int], typing.Optional[str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Tuple[int, int], typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[typing.Optional[int], typing.Tuple[int, int], typing.Optional[str]]) \
          -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[typing.Optional[int]]) -> None:
        return g(x)

      def g(x: typing.Optional[int]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, str, None]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Union[str, typing.Optional[typing.Tuple[int, int]]]]) -> \
          None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Union[str, typing.Optional[int]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, str, typing.Tuple[int, int]]) -> None:
        pass

      x: typing.Union[int, typing.Optional[str]] = ...
      f(x)
    |}
    [
      "Incompatible parameter type [6]: In call `f`, for 1st positional only parameter expected \
       `Union[Tuple[int, int], int, str]` but got `Union[None, int, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      class A:
        def __call__(self, x: int) -> bool:
          return True
      class B:
        def __call__(self, x: int) -> str:
          return "True"
      def f(x: typing.Union[A, B]) -> None:
        return x(8)

    |}
    ["Incompatible return type [7]: Expected `None` but got `Union[bool, str]`."];
  ()


let () = "union" >::: ["check_union" >:: test_check_union] |> Test.run

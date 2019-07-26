(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_union context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `typing.Union[int, str]` but got `float`."];
  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        if condition():
          return 1
        else:
          return 'foo'
    |}
    [];
  assert_type_errors
    {|
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
      def foo(a: typing.Union[str, int, float]) -> int:
        if isinstance(a, int):
          return a
        else:
          return a
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Union[float, str]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    ["Incompatible return type [7]: Expected `float` but got `Variable[T <: [int, str]]`."];
  assert_type_errors
    {|
      variable: typing.Union[typing.Optional[int], typing.Optional[str]] = None
      def ret_opt_int() -> typing.Optional[int]:
          return None
      variable = ret_opt_int()
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, Undefined]) -> None:
        pass
      foo(1)
    |}
    ["Undefined type [11]: Type `Undefined` is not defined."];
  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.int_attribute
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.str_attribute
    |}
    [ "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`." ];
  assert_type_errors
    {|
      def foo(x: typing.Union[OtherAttributes, Attributes]) -> int:
        return x.str_attribute
    |}
    [ "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`." ];
  assert_type_errors
    {|
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
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def herp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [ "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Bar` has no attribute `derp`." ];

  (* We require that all elements in a union have the same method for `in`. *)
  assert_type_errors
    {|
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
    ["Undefined attribute [16]: `Contains` has no attribute `__getitem__`."];
  assert_type_errors
    {|
      def f(x: typing.Optional[typing.Union[int, str]]) -> None:
        return g(x)

      def g(x: typing.Union[typing.Optional[int], typing.Optional[str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[int, typing.Tuple[int, int], typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[typing.Optional[int], typing.Tuple[int, int], typing.Optional[str]]) \
          -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[typing.Optional[int]]) -> None:
        return g(x)

      def g(x: typing.Optional[int]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[int, str, None]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[int, typing.Union[str, typing.Optional[typing.Tuple[int, int]]]]) -> \
          None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[int, typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Union[str, typing.Optional[int]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      def f(x: typing.Union[int, str, typing.Tuple[int, int]]) -> None:
        pass

      x: typing.Union[int, typing.Optional[str]] = ...
      f(x)
    |}
    [ "Incompatible parameter type [6]: Expected `typing.Union[int, str, typing.Tuple[int, int]]` \
       for 1st anonymous parameter to call `f` but got `typing.Optional[typing.Union[int, str]]`."
    ];
  assert_type_errors
    {|
      class A:
        def __call__(self, x: int) -> bool:
          return True
      class B:
        def __call__(self, x: int) -> str:
          return "True"
      def f(x: typing.Union[A, B]) -> None:
        return x(8)

    |}
    ["Incompatible return type [7]: Expected `None` but got `typing.Union[bool, str]`."];
  ()


let () = "union" >::: ["check_union" >:: test_check_union] |> Test.run

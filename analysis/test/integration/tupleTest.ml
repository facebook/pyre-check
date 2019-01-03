(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_tuple _ =
  assert_type_errors
    {|
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
    [];

  assert_type_errors
    {|
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `tuple.tuple_method` but got `float`."];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, 2, 3)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, str]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo()-> typing.Tuple[int, ...]:
        return tuple([1,2,3])
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return tuple([""])
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[str, ...]`.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return tuple([1])
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1, 2)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2.0)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2)
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Tuple[int, int, str]) -> typing.Tuple[str, int]:
        a, *b = x
        return b
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[str, int]` but got " ^
      "`typing.List[typing.Union[int, str]]`.";
    ];

  assert_type_errors
    {|
      def derp() -> int:
        a, b = [1,2,3]
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo() -> int:
        (x, y), z = 0
        return x + y + z
    |}
    [
      "Unable to unpack [23]: Unable to unpack `unknown` into 2 values.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self, coord: typing.Tuple[int, int]) -> None:
            self.xxx, self.yyy = coord
    |}
    [
      "Missing attribute annotation [4]: Attribute `xxx` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Missing attribute annotation [4]: Attribute `yyy` of class `Foo` " ^
      "has type `int` but no type is specified.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1,)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1, "")
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Tuple:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[()]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, str]:
        return ()
    |}
    ["Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got `typing.Tuple[]`."];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Iterable[int]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def bar(z: typing.Optional[int]) -> typing.Tuple[int, typing.Optional[int]]:
          return 1, int_to_int(z) if z is not None else None
    |}
    [];

  assert_type_errors
    {|
      def foo( *args: int) -> typing.Iterable[str]:
        return args
    |}
    [
      "Incompatible return type [7]: Expected `typing.Iterable[str]` but " ^
      "got `typing.Tuple[int, ...]`.";
    ];

  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.d
    |}
    [
      "Undefined attribute [16]: `typing.Any` has no attribute `__add__`.";
      "Undefined attribute [16]: `T` has no attribute `d`.";
    ];

  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y = t
        x, y, z = t
        x, y, z, other = t
    |}
    [];
  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
    []


let () =
  "tuple">:::[
    "check_tuple">::test_check_tuple;
  ]
  |> Test.run

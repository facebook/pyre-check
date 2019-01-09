(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest
open Ast.Expression


let test_check_assert _ =
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional or len(optional) > 0:
          pass
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Sized` for 1st anonymous parameter to call `len` but got " ^
     "`typing.Optional[str]`."];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional is None or len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional and len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False, "unreachable, surely"
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert not True
        return int_to_int(x)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter " ^
      "to call `int_to_int` but got `typing.Undeclared`.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];
  assert_type_errors
    {|
      def foo() -> int:
        if True:
          return 0
        else:
          return int_to_int("monkey news")
    |}
    []


let test_check_assert_functions _ =
  assert_type_errors
    ~debug:false
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    ~debug:false
    ~qualifier:(Access.create "foo")
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    {|
      class One:
          a: int = 1

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    []


let () =
  "assert">:::[
    "check_assert">::test_check_assert;
    "check_assert_functions">::test_check_assert_functions;
  ]
  |> Test.run

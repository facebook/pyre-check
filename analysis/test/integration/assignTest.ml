(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_assign _ =
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
      "Incompatible variable type [9]: x is declared to have type `str` " ^
      "but is used as type `int`.";
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Incompatible variable type [9]: x is declared to have type `str` " ^
      "but is used as type `int`.";
    ];

  assert_type_errors
    ~debug:false
    {|
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."];

  (* Prune `undeclared` from assignments. *)
  assert_type_errors
    {|
      def foo() -> None:
        y = x
        z = y
    |}
    ["Undefined name [18]: Global name `x` is undefined."];
  assert_type_errors
    {|
      def foo(a: bool) -> None:
        if a:
          x = 12
        y = x
        z = y
    |}
    ["Undefined name [18]: Global name `x` is undefined."];
  assert_type_errors
    {|
      def foo() -> None:
        y = [x]
        z = y
    |}
    ["Undefined name [18]: Global name `x` is undefined."];
  assert_type_errors
    {|
      def foo(a: bool) -> None:
        if a:
          x = 12
        y = [x]
        z = y
    |}
    ["Undefined name [18]: Global name `x` is undefined."]



let () =
  "assign">:::[
    "check_assign">::test_check_assign;
  ]
  |> Test.run

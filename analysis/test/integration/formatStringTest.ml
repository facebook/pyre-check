(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_format_string context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      def foo() -> None:
        f'foo{1}'
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1 + "x"}'
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`." ];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + "x"}'
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`." ];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + 2}'
    |}
    [];
  assert_type_errors
    {|
      def boo() -> int:
        return 1

      def foo() -> None:
        f'{boo() + "x"}'
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`." ];
  assert_type_errors {|
      def foo() -> None:
        f'{{x}}'
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        f'{{{x}}}'
    |}
    [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ]


let () = "format_string" >::: ["format_string" >:: test_format_string] |> Test.run

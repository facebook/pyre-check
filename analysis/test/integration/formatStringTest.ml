(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    ["Incompatible parameter type [6]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + "x"}'
    |}
    ["Incompatible parameter type [6]: `+` is not supported for operand types `int` and `str`."];
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
    ["Incompatible parameter type [6]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors {|
      def foo() -> None:
        f'{{x}}'
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        f'{{{x}}}'
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."]


let () = "format_string" >::: ["format_string" >:: test_format_string] |> Test.run

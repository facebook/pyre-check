(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_ignore_readonly context =
  let assert_type_errors = assert_type_errors ~context ~enable_readonly_analysis:false in
  (* The type checking analysis ignores `ReadOnly`. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> int:
        y: int = x
        z: ReadOnly[int] = y
        return z
    |}
    [];
  (* The type checking analysis will check compatibility for the type wrapped by `ReadOnly`. *)
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int]) -> None:
        y: str = x
    |}
    [
      (* TODO(T130377746): Avoid mixing `ReadOnly` in type errors. *)
      "Incompatible variable type [9]: y is declared to have type `str` but is used as type \
       `pyre_extensions.ReadOnly[int]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly
      from typing_extensions import Literal

      def foo(
        always_true: ReadOnly[Literal[True]],
        always_false: ReadOnly[Literal[False]]
      ) -> None:
        x: Literal[True] = always_true or False
        y: Literal[False] = always_false and True
    |}
    [];
  assert_type_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(s: ReadOnly[str]) -> None:
        y: str = s.capitalize()
    |}
    [];
  ()


let test_readonly_configuration_flag context =
  let assert_type_errors_including_readonly =
    assert_type_errors ~context ~enable_readonly_analysis:true
  in
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  (* Test readonly violations at the top level. *)
  assert_type_errors_including_readonly
    {|
      from pyre_extensions import ReadOnly

      x: ReadOnly[int] = 42
      y: int = x
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  ()


let () =
  "readOnly"
  >::: [
         "ignore" >:: test_ignore_readonly;
         "readonly_configuration_flag" >:: test_readonly_configuration_flag;
       ]
  |> Test.run

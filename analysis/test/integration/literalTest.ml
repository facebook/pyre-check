(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_boolean_literal context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar() -> None:
        foo(True)
    |}
    [];
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar() -> None:
        foo(False)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing_extensions.Literal[True]` for 1st \
       positional only parameter to call `foo` but got `typing_extensions.Literal[False]`.";
    ];
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar(b: bool) -> None:
        foo(b)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing_extensions.Literal[True]` for 1st \
       positional only parameter to call `foo` but got `bool`.";
    ]


let () = "literal" >::: ["boolean_literal" >:: test_boolean_literal] |> Test.run

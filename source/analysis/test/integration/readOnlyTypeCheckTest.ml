(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_ignore_readonly context =
  (* The type checking analysis ignores `ReadOnly`. *)
  assert_type_errors
    ~context
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
    ~context
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
    ~context
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
    ~context
    {|
      from pyre_extensions import ReadOnly

      def foo(s: ReadOnly[str]) -> None:
        y: str = s.capitalize()
    |}
    [];
  ()


let () = "readOnly" >::: ["ignore" >:: test_ignore_readonly] |> Test.run

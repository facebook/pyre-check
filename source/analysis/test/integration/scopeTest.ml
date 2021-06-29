(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_scoping context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      def foo(foo: str) -> str:
        return foo
    |} [];

  assert_type_errors
    {|
      class C:
        def method(self, foo: str) -> str:
          return foo
    |}
    [];

  assert_type_errors
    {|
      def f() -> None:
        def g() -> None:
          x = y
          y = 5
    |}
    ["Uninitialized local [61]: Local variable `y` may not be initialized here."];

  assert_type_errors
    {|
      def f() -> None:
        x: int = 0
        y: int = 0
        def g() -> None:
          nonlocal y, z
          _ = x    # Refers to local `x`, error as not yet initialized
          x = 1
          _ = y    # Refers to nonlocal `y`, hence OK
          y = 1
          _ = z    # Refers to nonlocal `z`, error as not defined in outer scope
    |}
    [
      "Uninitialized local [61]: Local variable `x` may not be initialized here.";
      "Unbound name [10]: Name `z` is used but not defined in the current scope.";
    ];

  (* TODO (T78261323): should be no error *)
  assert_type_errors
    {|
    f'{"".join(f"{}{metric}" for metric in [])}'
    |}
    ["Unbound name [10]: Name `metric` is used but not defined in the current scope."];

  ()


let () = "scope" >::: ["check_scoping" >:: test_check_scoping] |> Test.run

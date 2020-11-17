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
    []


let () = "scope" >::: ["check_scoping" >:: test_check_scoping] |> Test.run

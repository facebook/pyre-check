(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let assert_uninitialized_errors ~context =
  let check ~configuration ~environment ~source =
    UninitializedLocalCheck.run
      ~configuration
      ~environment:(TypeEnvironment.read_only environment)
      ~source
  in
  assert_errors ~context ~check


let test_simple context =
  let assert_uninitialized_errors = assert_uninitialized_errors ~context in
  assert_uninitialized_errors {|
      def foo():
        pass
    |} []


let () = "uninitializedCheck" >::: ["simple" >:: test_simple] |> Test.run

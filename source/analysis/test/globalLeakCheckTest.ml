(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let assert_global_leak_errors ~context =
  let check ~environment ~source =
    GlobalLeakCheck.check_module_TESTING_ONLY
      ~type_environment:(TypeEnvironment.read_only environment)
      source
  in
  assert_errors ~context ~check


let test_forward context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    {|
      def foo():
         x = y
    |}
    ["Global leak [3100]: Data is leaked to global `x`."];

  ()


let () = "global_leaks" >::: ["forward" >:: test_forward] |> Test.run

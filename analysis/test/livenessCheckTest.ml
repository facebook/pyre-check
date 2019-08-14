(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test

let assert_liveness_errors ~context =
  let check ~configuration ~global_resolution ~source =
    TypeCheck.run ~configuration ~global_resolution ~source |> ignore;
    LivenessCheck.run ~configuration ~global_resolution ~source
  in
  assert_errors ~context ~check


let test_forward context =
  let assert_liveness_errors = assert_liveness_errors ~context in
  assert_liveness_errors
    {|
      x = 1
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      x = 1
      y = x
    |}
    ["Dead store [1003]: Value assigned to `y` is never used."]


let () = "livenessCheck" >::: ["forward" >:: test_forward] |> Test.run

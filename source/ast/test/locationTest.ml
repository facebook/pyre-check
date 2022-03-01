(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Test

let test_contains _ =
  let contains ~location position =
    Location.contains ~location:(parse_location location) (parse_position position)
  in
  let assert_contains ~location position = assert_true (contains ~location position) in
  let assert_not_contains ~location position = assert_false (contains ~location position) in
  assert_not_contains ~location:"1:3-2:14" "1:2";
  assert_contains ~location:"1:3-2:14" "1:3";
  assert_contains ~location:"1:3-2:14" "1:99";
  assert_contains ~location:"1:3-2:14" "2:0";
  assert_contains ~location:"1:3-2:14" "2:13";
  assert_not_contains ~location:"1:3-2:14" "2:14";
  ()


let () = "location" >::: ["contains" >:: test_contains] |> Test.run

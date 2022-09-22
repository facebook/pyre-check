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
open ReadOnlyCheck

let test_less_or_equal _ =
  let open ReadOnlyness in
  let assert_less_or_equal ~expected ~left ~right =
    assert_bool_equals ~expected (less_or_equal ~left ~right)
  in
  assert_less_or_equal ~left:Mutable ~right:ReadOnly ~expected:true;
  assert_less_or_equal ~left:ReadOnly ~right:Mutable ~expected:false;
  assert_less_or_equal ~left:Mutable ~right:Mutable ~expected:true;
  assert_less_or_equal ~left:ReadOnly ~right:ReadOnly ~expected:true;
  ()


let () = "readOnly" >::: ["less_or_equal" >:: test_less_or_equal] |> Test.run

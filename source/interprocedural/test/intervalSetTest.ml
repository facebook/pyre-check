(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural

let of_list list = List.map list ~f:(fun (lower, upper) -> ClassInterval.create lower upper)

let test_of_list _ =
  let assert_of_list ~input ~expected =
    let actual = of_list input |> IntervalSet.of_list |> IntervalSet.to_list in
    let expected = of_list expected in
    assert_equal
      expected
      actual
      ~cmp:(List.equal ClassInterval.equal)
      ~printer:IntervalSet.show_list
  in
  assert_of_list ~input:[2, 1; 2, 0] ~expected:[];
  assert_of_list ~input:[1, 2; 2, 0] ~expected:[1, 2];
  assert_of_list ~input:[2, 0; 1, 2] ~expected:[1, 2];
  assert_of_list ~input:[1, 2] ~expected:[1, 2];
  assert_of_list ~input:[1, 0] ~expected:[];
  assert_of_list ~input:[1, 2; 1, 3] ~expected:[1, 3];
  assert_of_list ~input:[1, 3; 1, 2] ~expected:[1, 3];
  assert_of_list ~input:[1, 2; 2, 4] ~expected:[1, 4];
  assert_of_list ~input:[2, 4; 1, 2] ~expected:[1, 4];
  assert_of_list ~input:[1, 2; 3, 4] ~expected:[1, 4];
  assert_of_list ~input:[3, 4; 1, 2] ~expected:[1, 4];
  assert_of_list ~input:[1, 2; 1, 3; 2, 3] ~expected:[1, 3];
  assert_of_list ~input:[1, 2; 1, 3; 5, 10; 4, 7; 6, 0] ~expected:[1, 10];
  assert_of_list ~input:[1, 2; 3, 4; 5, 6] ~expected:[1, 6];
  assert_of_list ~input:[5, 6; 10, 11; 1, 2] ~expected:[1, 2; 5, 6; 10, 11]


let () = "interval_set" >::: ["of_list" >:: test_of_list] |> Test.run

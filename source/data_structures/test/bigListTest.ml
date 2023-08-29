(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module BigList = Data_structures.BigList

let test_add _ =
  let list = BigList.empty in
  assert_equal (BigList.to_list list) [];
  let list = BigList.empty |> BigList.cons 5 |> BigList.cons 7 in
  assert_equal (BigList.to_list list) [7; 5]


let test_merge _ =
  let list_1 = BigList.of_list [1; 2; 3] in
  let list_2 = BigList.of_list [3; 5] in
  assert_equal (BigList.merge list_1 list_2 |> BigList.to_list) [5; 3; 1; 2; 3];
  assert_equal (BigList.merge list_2 list_1 |> BigList.to_list) [5; 3; 1; 2; 3];
  assert_equal (BigList.merge list_1 BigList.empty |> BigList.to_list) [1; 2; 3];
  assert_equal (BigList.merge BigList.empty list_1 |> BigList.to_list) [1; 2; 3]


let test_length _ =
  let list = BigList.of_list [1; 2; 3] in
  assert_equal (BigList.length list) 3;
  assert_equal (BigList.length BigList.empty) 0


let () =
  "sizedListTest"
  >::: ["add" >:: test_add; "merge" >:: test_merge; "length" >:: test_length]
  |> run_test_tt_main

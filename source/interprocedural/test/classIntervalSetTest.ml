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

let assert_equal_interval_set ~actual ~expected =
  assert_equal
    expected
    actual
    ~cmp:(List.equal ClassInterval.equal)
    ~printer:ClassIntervalSet.show_list


let test_of_list _ =
  let assert_of_list ~input ~expected =
    let actual = of_list input |> ClassIntervalSet.of_list |> ClassIntervalSet.to_list in
    let expected = of_list expected in
    assert_equal_interval_set ~expected ~actual
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


let test_meet _ =
  let assert_meet ~left ~right ~expected =
    let left = of_list left |> ClassIntervalSet.of_list in
    let right = of_list right |> ClassIntervalSet.of_list in
    let actual = ClassIntervalSet.meet left right |> ClassIntervalSet.to_list in
    let expected = of_list expected in
    assert_equal_interval_set ~expected ~actual
  in
  assert_meet ~left:[1, 2] ~right:[2, 3] ~expected:[2, 2];
  assert_meet ~left:[2, 3] ~right:[1, 2] ~expected:[2, 2];
  assert_meet ~left:[3, 7] ~right:[3, 6] ~expected:[3, 6];
  assert_meet ~left:[3, 7] ~right:[4, 7] ~expected:[4, 7];
  assert_meet ~left:[3, 7] ~right:[4, 8] ~expected:[4, 7];
  assert_meet ~left:[3, 7] ~right:[7, 8] ~expected:[7, 7];
  assert_meet ~left:[3, 7] ~right:[3, 3] ~expected:[3, 3];
  assert_meet ~left:[3, 7] ~right:[2, 3] ~expected:[3, 3];
  assert_meet ~left:[3, 7] ~right:[2, 7] ~expected:[3, 7];
  assert_meet ~left:[3, 7] ~right:[2, 8] ~expected:[3, 7];
  assert_meet ~left:[3, 7] ~right:[2, 2] ~expected:[];
  assert_meet
    ~left:[17, 100; 1, 10; 14, 16; 12, 11]
    ~right:[17, 20; 5, 6; 17, 16; 7, 12]
    ~expected:[5, 10; 17, 20];
  assert_meet ~left:[14, 16; 1, 15; 12, 11] ~right:[5, 6; 7, 12] ~expected:[5, 12];
  assert_meet ~left:[1, 2; 3, 4] ~right:[1, 2; 3, 4] ~expected:[1, 4]


let test_join _ =
  let assert_join ~left ~right ~expected =
    let left = of_list left |> ClassIntervalSet.of_list in
    let right = of_list right |> ClassIntervalSet.of_list in
    let actual = ClassIntervalSet.join left right |> ClassIntervalSet.to_list in
    let expected = of_list expected in
    assert_equal_interval_set ~expected ~actual
  in
  assert_join ~left:[1, 2] ~right:[2, 3] ~expected:[1, 3];
  assert_join ~left:[2, 3] ~right:[1, 3] ~expected:[1, 3];
  assert_join ~left:[1, 7] ~right:[2, 7] ~expected:[1, 7];
  assert_join ~left:[2, 7] ~right:[1, 7] ~expected:[1, 7];
  assert_join ~left:[1, 7] ~right:[2, 8] ~expected:[1, 8];
  assert_join ~left:[2, 8] ~right:[1, 7] ~expected:[1, 8];
  assert_join ~left:[1, 10; 20, 30] ~right:[2, 3; 4, 5] ~expected:[1, 10; 20, 30];
  assert_join ~left:[1, 10; 20, 30] ~right:[2, 3; 4, 12] ~expected:[1, 12; 20, 30];
  assert_join ~left:[2, 3; 4, 12] ~right:[1, 10; 20, 30] ~expected:[1, 12; 20, 30];
  assert_join ~left:[1, 10; 20, 30] ~right:[2, 3; 12, 25] ~expected:[1, 10; 12, 30];
  assert_join ~left:[1, 10; 12, 14] ~right:[2, 15; 20, 30] ~expected:[1, 15; 20, 30];
  assert_join ~left:[1, 10; 12, 20] ~right:[2, 15; 20, 30] ~expected:[1, 30];
  assert_join ~left:[1, 10; 12, 14] ~right:[20, 30] ~expected:[1, 10; 12, 14; 20, 30];
  assert_join ~left:[1, 10; 12, 22] ~right:[20, 30] ~expected:[1, 10; 12, 30];
  assert_join ~left:[1, 10; 25, 27] ~right:[20, 30] ~expected:[1, 10; 20, 30];
  assert_join ~left:[1, 10] ~right:[10, 20; 2, 15; 30, 40] ~expected:[1, 20; 30, 40];
  assert_join ~left:[30, 40; 30, 50; 3, 0; 2, 21; 10, 20] ~right:[1, 10] ~expected:[1, 21; 30, 50];
  assert_join ~left:[1, 7] ~right:[1, 2; 8, 9] ~expected:[1, 9];
  assert_join ~left:[1, 2] ~right:[3, 4] ~expected:[1, 4]


let test_less_or_equal _ =
  let assert_less_or_equal ~left ~right ~expected =
    let left = of_list left |> ClassIntervalSet.of_list in
    let right = of_list right |> ClassIntervalSet.of_list in
    assert_equal expected (ClassIntervalSet.less_or_equal ~left ~right)
  in
  assert_less_or_equal ~left:[1, 2] ~right:[2, 3] ~expected:false;
  assert_less_or_equal ~left:[2, 3] ~right:[1, 2] ~expected:false;
  assert_less_or_equal ~left:[1, 2] ~right:[1, 3] ~expected:true;
  assert_less_or_equal ~left:[1, 2] ~right:[3, 4] ~expected:false;
  assert_less_or_equal ~left:[1, 2; 4, 5] ~right:[1, 7] ~expected:true;
  assert_less_or_equal ~left:[1, 7] ~right:[1, 2; 4, 5] ~expected:false;
  assert_less_or_equal ~left:[1, 2; 4, 5; 7, 10] ~right:[1, 11] ~expected:true;
  assert_less_or_equal ~left:[1, 11] ~right:[1, 2; 4, 5; 7, 10] ~expected:false


let () =
  "interval_set"
  >::: [
         "of_list" >:: test_of_list;
         "meet" >:: test_meet;
         "join" >:: test_join;
         "less_or_equal" >:: test_less_or_equal;
       ]
  |> Test.run

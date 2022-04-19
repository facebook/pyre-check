(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural
open ClassHierarchyGraph

let of_list list = List.map list ~f:(fun (lower, upper) -> ClassInterval.create lower upper)

let assert_equal_interval_set ~actual ~expected =
  assert_equal expected actual ~cmp:(List.equal ClassInterval.equal) ~printer:IntervalSet.show_list


let test_of_list _ =
  let assert_of_list ~input ~expected =
    let actual = of_list input |> IntervalSet.of_list |> IntervalSet.to_list in
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
    let left = of_list left |> IntervalSet.of_list in
    let right = of_list right |> IntervalSet.of_list in
    let actual = IntervalSet.meet left right |> IntervalSet.to_list in
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
    let left = of_list left |> IntervalSet.of_list in
    let right = of_list right |> IntervalSet.of_list in
    let actual = IntervalSet.join left right |> IntervalSet.to_list in
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
    let left = of_list left |> IntervalSet.of_list in
    let right = of_list right |> IntervalSet.of_list in
    assert_equal expected (IntervalSet.less_or_equal ~left ~right)
  in
  assert_less_or_equal ~left:[1, 2] ~right:[2, 3] ~expected:false;
  assert_less_or_equal ~left:[2, 3] ~right:[1, 2] ~expected:false;
  assert_less_or_equal ~left:[1, 2] ~right:[1, 3] ~expected:true;
  assert_less_or_equal ~left:[1, 2] ~right:[3, 4] ~expected:false;
  assert_less_or_equal ~left:[1, 2; 4, 5] ~right:[1, 7] ~expected:true;
  assert_less_or_equal ~left:[1, 7] ~right:[1, 2; 4, 5] ~expected:false;
  assert_less_or_equal ~left:[1, 2; 4, 5; 7, 10] ~right:[1, 11] ~expected:true;
  assert_less_or_equal ~left:[1, 11] ~right:[1, 2; 4, 5; 7, 10] ~expected:false


let test_class_intervals _ =
  let create_map list =
    List.fold list ~init:ClassNameMap.empty ~f:(fun accumulator (class_name, intervals) ->
        let intervals =
          List.map intervals ~f:(function lower, upper -> ClassInterval.create lower upper)
        in
        ClassNameMap.add class_name (IntervalSet.of_list intervals) accumulator)
  in
  let assert_class_intervals ~class_hierarchy_graph ~expected =
    let intervals = IntervalSet.compute_intervals class_hierarchy_graph in
    assert_equal
      expected
      intervals
      ~printer:(ClassNameMap.show ~pp_value:IntervalSet.pp)
      ~cmp:(ClassNameMap.equal IntervalSet.equal)
  in
  assert_class_intervals
    ~class_hierarchy_graph:
      (ClassHierarchyGraph.create
         ~roots:["object"]
         ~edges:
           [
             "object", ["test.A"];
             "test.A", ["test.B"; "test.C"];
             "test.B", ["test.D"];
             "test.C", ["test.D"];
             "test.D", [];
           ])
    ~expected:
      (create_map
         [
           "object", [1, 10];
           "test.A", [2, 9];
           "test.B", [3, 6];
           "test.C", [4, 5; 7, 8];
           "test.D", [4, 5];
         ]);
  assert_class_intervals
    ~class_hierarchy_graph:
      (ClassHierarchyGraph.create
         ~roots:["object"; "type"]
         ~edges:
           [
             "object", ["test.A"];
             "type", ["test.Meta"];
             "test.Meta", ["test.B"];
             "test.A", [];
             "test.B", [];
           ])
    ~expected:
      (create_map
         [
           "object", [1, 4]; "test.A", [2, 3]; "test.B", [7, 8]; "test.Meta", [6, 9]; "type", [5, 10];
         ]);
  assert_class_intervals
    ~class_hierarchy_graph:
      (ClassHierarchyGraph.create
         ~roots:["object"]
         ~edges:
           [
             "object", ["test.A"; "test.B"];
             "test.A", ["test.C"; "test.D"; "test.F"];
             "test.B", ["test.D"];
             "test.C", [];
             "test.D", ["test.E"; "test.F"];
             "test.E", [];
             "test.F", [];
           ])
    ~expected:
      (create_map
         [
           "object", [1, 14];
           "test.A", [2, 11];
           "test.B", [5, 10; 12, 13];
           "test.C", [3, 4];
           "test.D", [5, 10];
           "test.E", [6, 7];
           "test.F", [8, 9];
         ]);
  assert_class_intervals
    ~class_hierarchy_graph:
      (ClassHierarchyGraph.create
         ~roots:["test.A"]
         ~edges:
           [
             "test.A", ["test.B"; "test.C"; "test.D"];
             "test.B", ["test.E"];
             "test.C", ["test.E"];
             "test.D", ["test.C"];
             "test.E", [];
           ])
    ~expected:
      (create_map
         [
           "test.A", [1, 10];
           "test.B", [2, 5];
           "test.C", [3, 4; 6, 7];
           "test.D", [3, 4; 6, 9];
           "test.E", [3, 4];
         ]);
  assert_class_intervals
    ~class_hierarchy_graph:
      (ClassHierarchyGraph.create
         ~roots:["test.A"; "test.E"; "test.G"]
         ~edges:
           [
             "test.A", ["test.B"; "test.C"];
             "test.B", ["test.D"];
             "test.C", ["test.D"];
             "test.D", [];
             "test.E", ["test.F"];
             "test.F", ["test.C"];
             "test.G", ["test.B"];
           ])
    ~expected:
      (create_map
         [
           "test.A", [1, 8];
           "test.B", [2, 5];
           "test.C", [3, 4; 6, 7];
           "test.D", [3, 4];
           "test.E", [3, 4; 6, 7; 9, 12];
           "test.F", [3, 4; 6, 7; 10, 11];
           "test.G", [2, 5; 13, 14];
         ])


let () =
  "interval_set"
  >::: [
         "of_list" >:: test_of_list;
         "meet" >:: test_meet;
         "join" >:: test_join;
         "less_or_equal" >:: test_less_or_equal;
         "class_intervals" >:: test_class_intervals;
       ]
  |> Test.run

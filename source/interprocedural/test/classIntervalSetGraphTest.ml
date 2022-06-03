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

let test_class_intervals _ =
  let create_map list =
    List.fold list ~init:ClassNameMap.empty ~f:(fun accumulator (class_name, intervals) ->
        let intervals =
          List.map intervals ~f:(function lower, upper -> ClassInterval.create lower upper)
        in
        ClassNameMap.add class_name (ClassIntervalSet.of_list intervals) accumulator)
  in
  let assert_class_intervals ~class_hierarchy_graph ~expected =
    let intervals = ClassIntervalSetGraph.Heap.from_class_hierarchy class_hierarchy_graph in
    assert_equal
      expected
      intervals
      ~printer:(ClassNameMap.show ~pp_value:ClassIntervalSet.pp)
      ~cmp:(ClassNameMap.equal ClassIntervalSet.equal)
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


let () = "interval_set_graph" >::: ["class_intervals" >:: test_class_intervals] |> Test.run

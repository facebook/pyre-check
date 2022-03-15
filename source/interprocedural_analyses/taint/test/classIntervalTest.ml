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
open Taint

let test_meet _ =
  let assert_meet ~left ~right ~result = assert_equal result (ClassInterval.meet left right) in
  assert_meet
    ~left:ClassInterval.empty
    ~right:(ClassInterval.create 3 6)
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:ClassInterval.empty
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 2)
    ~result:ClassInterval.empty;
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 4)
    ~result:(ClassInterval.create 3 4);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 2 7)
    ~result:(ClassInterval.create 3 6);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 4 5)
    ~result:(ClassInterval.create 4 5);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 4 7)
    ~result:(ClassInterval.create 4 6);
  assert_meet
    ~left:(ClassInterval.create 3 6)
    ~right:(ClassInterval.create 7 8)
    ~result:ClassInterval.empty


let test_join _ =
  let assert_join ~left ~right ~result = assert_equal result (ClassInterval.join left right) in
  assert_join
    ~left:ClassInterval.empty
    ~right:(ClassInterval.create 3 6)
    ~result:(ClassInterval.create 3 6);
  assert_join
    ~left:(ClassInterval.create 1 2)
    ~right:(ClassInterval.create 3 4)
    ~result:(ClassInterval.create 1 4);
  assert_join
    ~left:(ClassInterval.create 1 2)
    ~right:(ClassInterval.create 2 3)
    ~result:(ClassInterval.create 1 3)


let test_class_interval _ =
  let create_map list =
    List.fold list ~init:ClassNameMap.empty ~f:(fun accumulator (k, v) ->
        ClassNameMap.add k v accumulator)
  in
  let assert_class_interval ~class_hierarchy_graph ~expected =
    let intervals = ClassInterval.compute_intervals class_hierarchy_graph in
    assert_equal
      expected
      intervals
      ~printer:(ClassNameMap.show ~pp_value:ClassInterval.pp)
      ~cmp:(ClassNameMap.equal ClassInterval.equal)
  in
  assert_class_interval
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
           "object", ClassInterval.create 1 10;
           "test.A", ClassInterval.create 2 9;
           "test.B", ClassInterval.create 3 6;
           "test.C", ClassInterval.create 4 8;
           "test.D", ClassInterval.create 4 5;
         ]);
  assert_class_interval
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
           "object", ClassInterval.create 1 4;
           "test.A", ClassInterval.create 2 3;
           "test.B", ClassInterval.create 7 8;
           "test.Meta", ClassInterval.create 6 9;
           "type", ClassInterval.create 5 10;
         ]);
  assert_class_interval
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
           "object", ClassInterval.create 1 14;
           "test.A", ClassInterval.create 2 11;
           "test.B", ClassInterval.create 5 13;
           "test.C", ClassInterval.create 3 4;
           "test.D", ClassInterval.create 5 10;
           "test.E", ClassInterval.create 6 7;
           "test.F", ClassInterval.create 8 9;
         ]);
  assert_class_interval
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
           "test.A", ClassInterval.create 1 10;
           "test.B", ClassInterval.create 2 5;
           "test.C", ClassInterval.create 3 7;
           "test.D", ClassInterval.create 3 9;
           "test.E", ClassInterval.create 3 4;
         ]);
  assert_class_interval
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
           "test.A", ClassInterval.create 1 8;
           "test.B", ClassInterval.create 2 5;
           "test.C", ClassInterval.create 3 7;
           "test.D", ClassInterval.create 3 4;
           "test.E", ClassInterval.create 3 12;
           "test.F", ClassInterval.create 3 11;
           "test.G", ClassInterval.create 2 14;
         ])


let () =
  "class_interval"
  >::: ["meet" >:: test_meet; "join" >:: test_join; "class_interval" >:: test_class_interval]
  |> Test.run

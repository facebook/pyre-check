(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Pyre

module StringKey = struct
  type t = string

  type key = string [@@deriving sexp, compare]

  module KeySet = Caml.Set.Make (struct
    type t = key [@@deriving compare, sexp]
  end)

  type registered = string [@@deriving sexp, compare]

  module RegisteredSet = KeySet

  let to_string x = x

  let compare = String.compare

  let from_string x = x

  module Registry = struct
    let table = DependencyTrackedMemory.EncodedDependency.Table.create ()

    let encode key =
      let add = function
        | None -> String.Set.singleton key
        | Some existing -> String.Set.add existing key
      in
      let encoded = DependencyTrackedMemory.EncodedDependency.make key ~hash:String.hash in
      DependencyTrackedMemory.EncodedDependency.Table.update table encoded ~f:add;
      encoded


    let decode hash =
      DependencyTrackedMemory.EncodedDependency.Table.find table hash >>| Set.to_list
  end
end

module StringDependencyKey = DependencyTrackedMemory.DependencyKey.Make (StringKey)

module StringValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Test1"

  let equal = String.equal
end

module OtherStringValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Test2"

  let equal = String.equal
end

module TableA =
  DependencyTrackedMemory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey)
    (StringValue)
module TableB =
  DependencyTrackedMemory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey)
    (OtherStringValue)

let assert_dependency ~expected actual =
  let expected_set = StringDependencyKey.RegisteredSet.of_list expected in
  assert_bool
    "Check if the actual dependency overapproximate the expected one"
    (StringDependencyKey.RegisteredSet.subset expected_set actual)


let table_a = TableA.create ()

let table_b = TableB.create ()

let test_dependency_table _ =
  let function_1 = "function_1" in
  let function_2 = "function_2" in
  let function_3 = "function_3" in
  let function_4 = "function_4" in
  let _value : string option = TableA.get table_a "Foo" ~dependency:function_1 in
  let _value : string option = TableA.get table_a "Bar" ~dependency:function_1 in
  let _value : string option = TableA.get table_a "Foo" ~dependency:function_2 in
  let _value : string option = TableA.get table_a "Foo" ~dependency:function_3 in

  assert_dependency
    ~expected:[function_3; function_2; function_1]
    (TableA.get_dependents ~kind:Get "Foo");
  assert_dependency ~expected:[function_1] (TableA.get_dependents ~kind:Get "Bar");
  let _value : string option = TableB.get table_b "Foo" ~dependency:function_4 in

  (* Ensure that different tables' same keys are encoded differently *)
  assert_dependency ~expected:[function_4] (TableB.get_dependents ~kind:Get "Foo");

  (* Ensure that `reset_shared_memory` correctly resets all dependency-related info *)
  Memory.reset_shared_memory ();
  assert_dependency ~expected:[] (TableA.get_dependents ~kind:Get "Foo");
  assert_dependency ~expected:[] (TableB.get_dependents ~kind:Get "Foo");
  let _value : string option = TableB.get table_b "Foo" ~dependency:function_4 in
  assert_dependency ~expected:[function_4] (TableB.get_dependents ~kind:Get "Foo");

  (* Ensure that the `get` interface also adds the corresponding dependencies *)
  TableA.get table_a "Foo" ~dependency:function_1 |> ignore;
  TableA.get table_a "Bar" ~dependency:function_1 |> ignore;
  TableA.get table_a "Foo" ~dependency:function_2 |> ignore;
  TableA.get table_a "Foo" ~dependency:function_3 |> ignore;

  assert_dependency
    ~expected:[function_3; function_2; function_1]
    (TableA.get_dependents ~kind:Get "Foo");
  assert_dependency ~expected:[function_1] (TableA.get_dependents ~kind:Get "Bar");

  (* Final cleanup *)
  Memory.reset_shared_memory ();
  ()


module UpdateDependencyTest = struct
  type t = {
    key: string;
    old_value: string option;
    new_value: string option;
    get_dependencies: string list;
    mem_dependencies: string list;
  }

  let assert_dependencies ~expected specification =
    let open Core in
    let setup_old_state { key; old_value; get_dependencies; mem_dependencies; _ } =
      Option.iter old_value ~f:(TableA.add table_a key);
      let _values : string option list =
        List.map get_dependencies ~f:(fun dependency -> TableA.get table_a key ~dependency)
      in
      let _values : bool list =
        List.map mem_dependencies ~f:(fun dependency -> TableA.mem table_a key ~dependency)
      in
      ()
    in
    List.iter specification ~f:setup_old_state;
    let setup_new_state { key; new_value; _ } = Option.iter new_value ~f:(TableA.add table_a key) in
    let update _ = List.iter specification ~f:setup_new_state in
    let keys = List.map specification ~f:(fun { key; _ } -> key) |> TableB.KeySet.of_list in
    let _, actual =
      StringDependencyKey.Transaction.empty ~scheduler:(Test.mock_scheduler ())
      |> TableA.add_to_transaction table_a ~keys
      |> StringDependencyKey.Transaction.execute ~update
    in
    assert_dependency ~expected actual;
    Memory.reset_shared_memory ()
end

let test_update_dependency_table _ =
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = Some "NewAVal";
        get_dependencies = [];
        mem_dependencies = ["dep_a"];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = None;
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = None;
        get_dependencies = [];
        mem_dependencies = ["dep_a"];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = [];
        mem_dependencies = ["dep_a"];
      };
    ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = ["dep_a"];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = None;
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = None;
        get_dependencies = [];
        mem_dependencies = ["dep_a"];
      };
    ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "AVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"; "dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"; "dep_b"];

  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = None;
        new_value = Some "NewBVal";
        get_dependencies = ["dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = None;
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = None;
        get_dependencies = ["dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = None;
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = None;
        get_dependencies = ["dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A1";
        old_value = Some "A1Val";
        new_value = Some "NewA1Val";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "A2";
        old_value = Some "A2Val";
        new_value = Some "A2Val";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        get_dependencies = ["dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "BVal";
        get_dependencies = ["dep_b"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "AVal";
        get_dependencies = ["dep_a"; "dep_b"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        get_dependencies = ["dep_b"; "dep_c"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_b"; "dep_c"];

  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "AVal";
        get_dependencies = ["dep_a_b"; "dep_a_c"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "BVal";
        get_dependencies = ["dep_a_b"; "dep_b_c"];
        mem_dependencies = [];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        get_dependencies = ["dep_a_c"; "dep_b_c"];
        mem_dependencies = [];
      };
    ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a_b"; "dep_a_c"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "BVal";
        get_dependencies = ["dep_a_b"; "dep_b_c"];
        mem_dependencies = [];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        get_dependencies = ["dep_a_c"; "dep_b_c"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a_b"; "dep_a_c"];
  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a_b"; "dep_a_c"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        get_dependencies = ["dep_a_b"; "dep_b_c"];
        mem_dependencies = [];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        get_dependencies = ["dep_a_c"; "dep_b_c"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a_b"; "dep_a_c"; "dep_b_c"];

  UpdateDependencyTest.assert_dependencies
    [
      {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        get_dependencies = ["dep_a_b"; "dep_a_c"];
        mem_dependencies = [];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        get_dependencies = ["dep_a_b"; "dep_b_c"];
        mem_dependencies = [];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "NewCVal";
        get_dependencies = ["dep_a_c"; "dep_b_c"];
        mem_dependencies = [];
      };
    ]
    ~expected:["dep_a_b"; "dep_a_c"; "dep_b_c"];
  ()


let () =
  "memory"
  >::: [
         "dependencies" >:: test_dependency_table;
         "update_dependencies" >:: test_update_dependency_table;
       ]
  |> Test.run

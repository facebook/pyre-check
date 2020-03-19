(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2

module StringKey = struct
  type t = string

  let to_string x = x

  let compare = String.compare

  type out = string

  let from_string x = x
end

module StringDependencyKey = DependencyTrackedMemory.DependencyKey.Make (StringKey)

module StringValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Test1"

  let compare = String.compare

  (* Strings are not marshalled by shared memory *)
  let unmarshall value = value
end

module OtherStringValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Test2"

  let compare = String.compare

  (* Strings are not marshalled by shared memory *)
  let unmarshall value = value
end

module TableA =
  DependencyTrackedMemory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey)
    (StringValue)
module TableB =
  DependencyTrackedMemory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey)
    (OtherStringValue)

let assert_dependency ~expected actual =
  let expected_set = StringDependencyKey.KeySet.of_list expected in
  assert_bool
    "Check if the actual dependency overapproximate the expected one"
    (StringDependencyKey.KeySet.subset expected_set actual)


let test_dependency_table _ =
  let function_1 = "function_1" in
  let function_2 = "function_2" in
  let function_3 = "function_3" in
  let function_4 = "function_4" in
  TableA.add_dependency ~kind:Get "Foo" function_1;
  TableA.add_dependency ~kind:Get "Bar" function_1;
  TableA.add_dependency ~kind:Get "Foo" function_2;
  TableA.add_dependency ~kind:Get "Foo" function_3;

  assert_dependency
    ~expected:[function_3; function_2; function_1]
    (TableA.get_dependents ~kind:Get "Foo");
  assert_dependency ~expected:[function_1] (TableA.get_dependents ~kind:Get "Bar");
  TableB.add_dependency "Foo" ~kind:Get function_4;

  (* Ensure that different tables' same keys are encoded differently *)
  assert_dependency ~expected:[function_4] (TableB.get_dependents ~kind:Get "Foo");

  (* Ensure that `reset_shared_memory` correctly resets all dependency-related info *)
  Memory.reset_shared_memory ();
  assert_dependency ~expected:[] (TableA.get_dependents ~kind:Get "Foo");
  assert_dependency ~expected:[] (TableB.get_dependents ~kind:Get "Foo");
  TableB.add_dependency ~kind:Get "Foo" function_4;
  assert_dependency ~expected:[function_4] (TableB.get_dependents ~kind:Get "Foo");

  (* Ensure that the `get` interface also adds the corresponding dependencies *)
  TableA.get "Foo" ~dependency:function_1 |> ignore;
  TableA.get "Bar" ~dependency:function_1 |> ignore;
  TableA.get "Foo" ~dependency:function_2 |> ignore;
  TableA.get "Foo" ~dependency:function_3 |> ignore;

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
      Option.iter old_value ~f:(TableA.add key);
      List.iter get_dependencies ~f:(TableA.add_dependency ~kind:Get key);
      List.iter mem_dependencies ~f:(TableA.add_dependency ~kind:Mem key)
    in
    List.iter specification ~f:setup_old_state;
    let setup_new_state { key; new_value; _ } = Option.iter new_value ~f:(TableA.add key) in
    let update _ = List.iter specification ~f:setup_new_state in
    let keys = List.map specification ~f:(fun { key; _ } -> key) |> TableB.KeySet.of_list in
    let _, actual =
      StringDependencyKey.Transaction.empty
        ~scheduler:(Test.mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
      |> TableA.add_to_transaction ~keys
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

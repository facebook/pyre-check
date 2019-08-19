(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2

let test_little_endian_representation _ =
  let assert_correctly_represented ~expected ~key =
    let digest = Digest.from_hex key in
    let little_endian_representation = Memory.unsafe_little_endian_representation ~key:digest in
    assert_equal ~printer:Int64.to_string (Int64.of_string expected) little_endian_representation
  in
  assert_correctly_represented
    ~expected:"0x0706050403020100"
    ~key:"000102030405060708090a0b0c0d0e0f";
  assert_correctly_represented
    ~expected:"0x0102030405060708"
    ~key:"0807060504030201090a0b0c0d0e0fff";
  assert_raises (Invalid_argument "Digest.to_hex") (fun () ->
      Memory.unsafe_little_endian_representation ~key:"short");
  assert_raises (Invalid_argument "Digest.to_hex") (fun () ->
      Memory.unsafe_little_endian_representation ~key:"Too_long_of_a_message")


module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Core.Int.of_string
end

module MockEdgeValue = struct
  type t = Analysis.ClassHierarchy.Target.t list

  let prefix = Prefix.make ()

  let description = "Edges"

  let unmarshall value = Marshal.from_string value 0
end

module MockEdges = Memory.WithCache.Make (IntKey) (MockEdgeValue)

module MockBackedgeValue = struct
  type t = Analysis.ClassHierarchy.Target.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Backedges"

  let unmarshall value = Marshal.from_string value 0
end

module MockBackedges = Memory.WithCache.Make (IntKey) (MockBackedgeValue)

module MockAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Annotations"

  (* Shared memory does not marshall strings *)
  let unmarshall value = value
end

module MockAnnotations = Memory.WithCache.Make (IntKey) (MockAnnotationValue)

let test_decodable _ =
  let assert_decode prefix key value expected =
    let key = Prefix.make_key prefix key in
    assert_equal (Memory.decode ~key ~value) (Ok expected)
  in
  assert_decode
    MockEdgeValue.prefix
    (IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures])
    (MockEdges.Decoded (1234, Some []));
  assert_decode
    MockBackedgeValue.prefix
    (IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures])
    (MockBackedges.Decoded (1234, Some Analysis.ClassHierarchy.Target.Set.Tree.empty));
  assert_decode
    MockEdgeValue.prefix
    (IntKey.to_string 1234)
    "can't decode this"
    (MockEdges.Decoded (1234, None));
  assert_decode
    MockAnnotationValue.prefix
    (IntKey.to_string 1234)
    "can decode this"
    (MockAnnotations.Decoded (1234, Some "can decode this"));
  assert_equal
    (Error `Malformed_key)
    (Memory.decode ~key:"" ~value:(Marshal.to_string [] [Marshal.Closures]));
  let unregistered_prefix = Prefix.make () in
  assert_equal
    (Error `Unknown_type)
    (Memory.decode
       ~key:(Prefix.make_key unregistered_prefix "")
       ~value:(Marshal.to_string [] [Marshal.Closures]))


let test_serialize_key _ =
  assert_equal
    (MockEdges.serialize_key 1234)
    (Prefix.make_key MockEdgeValue.prefix "1234" |> Base64.encode_exn);
  assert_equal
    (MockBackedges.serialize_key 1234)
    (Prefix.make_key MockBackedgeValue.prefix "1234" |> Base64.encode_exn)


let test_hash_of_key _ =
  assert_equal
    (MockEdges.hash_of_key 1234)
    ( MockEdges.string_of_key 1234
    |> (fun key -> Memory.unsafe_little_endian_representation ~key)
    |> Int64.to_string );
  assert_equal
    (MockBackedges.hash_of_key 1234)
    ( MockBackedges.string_of_key 1234
    |> (fun key -> Memory.unsafe_little_endian_representation ~key)
    |> Int64.to_string );

  (* Make sure different modules have different caches. *)
  assert_equal
    false
    (String.equal
       (MockBackedges.hash_of_key 1234)
       ( MockEdges.string_of_key 1234
       |> (fun key -> Memory.unsafe_little_endian_representation ~key)
       |> Int64.to_string ))


let test_reset _ =
  MockAnnotations.add 42 "derp";

  Memory.reset_shared_memory ();

  assert_bool "reset_shared_memory should clear all cached results" (not (MockAnnotations.mem 42));

  let heap_size = SharedMem.heap_size () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 heap_size;

  let { SharedMem.nonempty_slots; used_slots; _ } = SharedMem.dep_stats () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 nonempty_slots;
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 used_slots;

  let { SharedMem.nonempty_slots; used_slots; _ } = SharedMem.hash_stats () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 nonempty_slots;
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 used_slots;
  ()


module StringKey = struct
  type t = string

  let to_string x = x

  let compare = String.compare

  type out = string

  let from_string x = x
end

module StringDependencyKey = Memory.DependencyKey.Make (StringKey)

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
  Memory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey) (StringValue)
module TableB =
  Memory.DependencyTrackedTableWithCache (StringKey) (StringDependencyKey) (OtherStringValue)

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
  TableA.add_dependency "Foo" function_1;
  TableA.add_dependency "Bar" function_1;
  TableA.add_dependency "Foo" function_2;
  TableA.add_dependency "Foo" function_3;

  assert_dependency ~expected:[function_3; function_2; function_1] (TableA.get_dependents "Foo");
  assert_dependency ~expected:[function_1] (TableA.get_dependents "Bar");
  TableB.add_dependency "Foo" function_4;

  (* Ensure that different tables' same keys are encoded differently *)
  assert_dependency ~expected:[function_4] (TableB.get_dependents "Foo");

  (* Ensure that `reset_shared_memory` correctly resets all dependency-related info *)
  Memory.reset_shared_memory ();
  assert_dependency ~expected:[] (TableA.get_dependents "Foo");
  assert_dependency ~expected:[] (TableB.get_dependents "Foo");
  TableB.add_dependency "Foo" function_4;
  assert_dependency ~expected:[function_4] (TableB.get_dependents "Foo");

  (* Ensure that the `get` interface also adds the corresponding dependencies *)
  TableA.get "Foo" ~dependency:function_1 |> ignore;
  TableA.get "Bar" ~dependency:function_1 |> ignore;
  TableA.get "Foo" ~dependency:function_2 |> ignore;
  TableA.get "Foo" ~dependency:function_3 |> ignore;

  assert_dependency ~expected:[function_3; function_2; function_1] (TableA.get_dependents "Foo");
  assert_dependency ~expected:[function_1] (TableA.get_dependents "Bar");

  (* Final cleanup *)
  Memory.reset_shared_memory ();
  ()


module UpdateDependencyTest = struct
  type t = {
    key: string;
    old_value: string option;
    new_value: string option;
    dependencies: string list;
  }

  let assert_dependencies ~expected specification =
    let open Core in
    let setup_old_state { key; old_value; dependencies; _ } =
      Option.iter old_value ~f:(TableA.add key);
      List.iter dependencies ~f:(TableA.add_dependency key)
    in
    List.iter specification ~f:setup_old_state;
    let setup_new_state { key; new_value; _ } = Option.iter new_value ~f:(TableA.add key) in
    let update _ = List.iter specification ~f:setup_new_state in
    let keys = List.map specification ~f:(fun { key; _ } -> key) |> TableB.KeySet.of_list in
    let _, actual =
      StringDependencyKey.Transaction.empty
      |> TableA.add_to_transaction ~keys
      |> StringDependencyKey.Transaction.execute ~update
    in
    assert_dependency ~expected actual;
    Memory.reset_shared_memory ()
end

let test_update_dependency_table _ =
  UpdateDependencyTest.assert_dependencies
    [{ key = "A"; old_value = None; new_value = Some "NewAVal"; dependencies = ["dep_a"] }]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [{ key = "A"; old_value = Some "AVal"; new_value = None; dependencies = ["dep_a"] }]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [{ key = "A"; old_value = Some "AVal"; new_value = Some "NewAVal"; dependencies = ["dep_a"] }]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [{ key = "A"; old_value = None; new_value = None; dependencies = ["dep_a"] }]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [{ key = "A"; old_value = Some "AVal"; new_value = Some "AVal"; dependencies = ["dep_a"] }]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        dependencies = ["dep_a"; "dep_b"];
      } ]
    ~expected:["dep_a"; "dep_b"];

  UpdateDependencyTest.assert_dependencies
    [ { key = "A"; old_value = None; new_value = Some "NewAVal"; dependencies = ["dep_a"] };
      { key = "B"; old_value = None; new_value = Some "NewBVal"; dependencies = ["dep_b"] } ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [ { key = "A"; old_value = Some "AVal"; new_value = None; dependencies = ["dep_a"] };
      { key = "B"; old_value = Some "BVal"; new_value = None; dependencies = ["dep_b"] } ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [ { key = "A"; old_value = None; new_value = Some "NewAVal"; dependencies = ["dep_a"] };
      { key = "B"; old_value = Some "BVal"; new_value = None; dependencies = ["dep_b"] } ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A1";
        old_value = Some "A1Val";
        new_value = Some "NewA1Val";
        dependencies = ["dep_a"];
      };
      { key = "A2"; old_value = Some "A2Val"; new_value = Some "A2Val"; dependencies = ["dep_a"] }
    ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [ { key = "A"; old_value = Some "AVal"; new_value = Some "NewAVal"; dependencies = ["dep_a"] };
      { key = "B"; old_value = Some "BVal"; new_value = Some "NewBVal"; dependencies = ["dep_b"] }
    ]
    ~expected:["dep_a"; "dep_b"];
  UpdateDependencyTest.assert_dependencies
    [ { key = "A"; old_value = Some "AVal"; new_value = Some "NewAVal"; dependencies = ["dep_a"] };
      { key = "B"; old_value = Some "BVal"; new_value = Some "BVal"; dependencies = ["dep_b"] } ]
    ~expected:["dep_a"];
  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "AVal";
        dependencies = ["dep_a"; "dep_b"];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        dependencies = ["dep_b"; "dep_c"];
      } ]
    ~expected:["dep_b"; "dep_c"];

  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "AVal";
        dependencies = ["dep_a_b"; "dep_a_c"];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "BVal";
        dependencies = ["dep_a_b"; "dep_b_c"];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        dependencies = ["dep_a_c"; "dep_b_c"];
      } ]
    ~expected:[];
  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        dependencies = ["dep_a_b"; "dep_a_c"];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "BVal";
        dependencies = ["dep_a_b"; "dep_b_c"];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        dependencies = ["dep_a_c"; "dep_b_c"];
      } ]
    ~expected:["dep_a_b"; "dep_a_c"];
  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        dependencies = ["dep_a_b"; "dep_a_c"];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        dependencies = ["dep_a_b"; "dep_b_c"];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "CVal";
        dependencies = ["dep_a_c"; "dep_b_c"];
      } ]
    ~expected:["dep_a_b"; "dep_a_c"; "dep_b_c"];

  UpdateDependencyTest.assert_dependencies
    [ {
        key = "A";
        old_value = Some "AVal";
        new_value = Some "NewAVal";
        dependencies = ["dep_a_b"; "dep_a_c"];
      };
      {
        key = "B";
        old_value = Some "BVal";
        new_value = Some "NewBVal";
        dependencies = ["dep_a_b"; "dep_b_c"];
      };
      {
        key = "C";
        old_value = Some "CVal";
        new_value = Some "NewCVal";
        dependencies = ["dep_a_c"; "dep_b_c"];
      } ]
    ~expected:["dep_a_b"; "dep_a_c"; "dep_b_c"];
  ()


let () =
  "memory"
  >::: [ "little_endian_representation" >:: test_little_endian_representation;
         "decodable" >:: test_decodable;
         "serialize_key" >:: test_serialize_key;
         "hash_of_key" >:: test_hash_of_key;
         "test_reset" >:: test_reset;
         "dependencies" >:: test_dependency_table;
         "update_dependencies" >:: test_update_dependency_table ]
  |> Test.run

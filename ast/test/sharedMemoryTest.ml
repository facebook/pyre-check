(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core

open Test
open OUnit2

let test_normalize_handle_keys context =
  (* Ensure shared memory gets cleaned up afterwards. *)
  let tear_down _ _ = Ast.SharedMemory.HandleKeys.clear () in
  bracket (fun _ -> ()) tear_down context;
  (* Ensure that the structural representation of the keys is identical for all possible
     permutations. *)
  let assert_normalized keys =
    let keys = List.map keys ~f:File.Handle.create in
    let open Ast.SharedMemory in
    HandleKeys.clear ();
    HandleKeys.add ~handles:(File.Handle.Set.Tree.of_list keys);
    HandleKeys.normalize ();
    let canonical_representation =
      List.sort keys ~compare:File.Handle.compare
      |> File.Handle.Set.Tree.of_list
    in
    assert_equal canonical_representation (HandleKeys.get ())
  in
  assert_normalized [];
  assert_normalized ["a"];
  assert_normalized ["a"; "b"; "c"];
  assert_normalized ["d"; "c"; "b"; "a"];
  assert_normalized ["a"; "b"; "c"; "d"];
  assert_normalized ["d"; "b"; "a"; "c"];
  assert_normalized ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];
  assert_normalized ["h"; "g"; "f"; "e"; "d"; "c"; "b"; "a"]


let test_compute_hashes_to_keys _ =
  let open Ast.SharedMemory in
  let assert_mapping_equal expected actual =
    assert_equal
      ~printer:(fun map -> Sexp.to_string (String.Map.sexp_of_t String.sexp_of_t map))
      ~cmp:(String.Map.equal String.equal)
      (String.Map.of_alist_exn expected)
      actual
  in
  assert_mapping_equal
    [
      SymlinksToPaths.hash_of_key "first", SymlinksToPaths.serialize_key "first";
      SymlinksToPaths.hash_of_key "second", SymlinksToPaths.serialize_key "second";
    ]
    (SymlinksToPaths.compute_hashes_to_keys ~keys:["first"; "second"]);
  assert_mapping_equal
    [
      Sources.hash_of_handle (File.Handle.create "first.py"),
      Sources.serialize_handle (File.Handle.create "first.py");
      Sources.hash_of_qualifier (!&"first"),
      Sources.serialize_qualifier (!&"first");
      Sources.hash_of_handle (File.Handle.create "second/__init__.py"),
      Sources.serialize_handle (File.Handle.create "second/__init__.py");
      Sources.hash_of_qualifier (!&"second"),
      Sources.serialize_qualifier (!&"second");

    ]
    (Sources.compute_hashes_to_keys
       ~keys:[File.Handle.create "first.py"; File.Handle.create "second/__init__.py"]
    );
  assert_mapping_equal
    [
      HandleKeys.HandleKeys.hash_of_key Memory.SingletonKey.key,
      HandleKeys.HandleKeys.serialize_key Memory.SingletonKey.key;
    ]
    (HandleKeys.compute_hashes_to_keys ());
  assert_mapping_equal
    [
      Modules.hash_of_key (!&"foo"), Modules.serialize_key (!&"foo");
      Modules.hash_of_key (!&"bar"), Modules.serialize_key (!&"bar");
      Modules.hash_of_key (!&"foo.b"),
      Modules.serialize_key (!&"foo.b");
    ]
    (Modules.compute_hashes_to_keys
       ~keys:[
         !&"foo";
         !&"bar";
         !&"foo.b";
       ]);
  assert_mapping_equal
    [
      Handles.hash_of_key (String.hash "a.py"), Handles.serialize_key (String.hash "a.py");
      Handles.hash_of_key (String.hash "b/c.py"), Handles.serialize_key (String.hash "b/c.py");
    ]
    (Handles.compute_hashes_to_keys ~keys:["a.py"; "b/c.py"])


let test_remove_handle_keys _ =
  let open Ast.SharedMemory in
  HandleKeys.clear ();
  let handles paths =
    paths
    |> List.map  ~f:File.Handle.create
    |> File.Handle.Set.Tree.of_list
  in
  HandleKeys.add ~handles:(handles ["a.py"; "b.py"; "c.py"]);
  assert_equal
    ~cmp:File.Handle.Set.Tree.equal
    (HandleKeys.get ())
    (handles ["a.py"; "b.py"; "c.py"]);
  HandleKeys.remove ~handles:(List.map ~f:File.Handle.create ["b.py"; "nonexistent"]);
  assert_equal
    ~cmp:File.Handle.Set.Tree.equal
    (HandleKeys.get ())
    (handles ["a.py"; "c.py"])


let () =
  "ast_shared_memory">:::[
    "normalize_handle_keys">::test_normalize_handle_keys;
    "compute_hashes_to_keys">::test_compute_hashes_to_keys;
    "remove_handle_keys">::test_remove_handle_keys;
  ]
  |> Test.run

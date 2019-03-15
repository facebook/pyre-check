(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core

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
  let open Ast.Expression in
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
      Sources.hash_of_qualifier (Access.create "first"),
      Sources.serialize_qualifier (Access.create "first");
      Sources.hash_of_handle (File.Handle.create "second/__init__.py"),
      Sources.serialize_handle (File.Handle.create "second/__init__.py");
      Sources.hash_of_qualifier (Access.create "second"),
      Sources.serialize_qualifier (Access.create "second");

    ]
    (Sources.compute_hashes_to_keys
       ~keys:[File.Handle.create "first.py"; File.Handle.create "second/__init__.py"]
    );
  assert_mapping_equal
    [HandleKeys.hash_of_key 0, HandleKeys.serialize_key 0]
    (HandleKeys.compute_hashes_to_keys ());
  assert_mapping_equal
    [
      Modules.hash_of_key (Access.create "foo"), Modules.serialize_key (Access.create "foo");
      Modules.hash_of_key (Access.create "bar"), Modules.serialize_key (Access.create "bar");
      Modules.hash_of_key (Access.create "foo.b"), Modules.serialize_key (Access.create "foo.b");
    ]
    (Modules.compute_hashes_to_keys
       ~keys:[
         Access.create "foo";
         Access.create "bar";
         Access.create "foo.b";
       ]);
  assert_mapping_equal
    [
      Handles.hash_of_key (String.hash "a.py"), Handles.serialize_key (String.hash "a.py");
      Handles.hash_of_key (String.hash "b/c.py"), Handles.serialize_key (String.hash "b/c.py");
    ]
    (Handles.compute_hashes_to_keys ~keys:["a.py"; "b/c.py"])


let () =
  "ast_shared_memory">:::[
    "normalize_handle_keys">::test_normalize_handle_keys;
    "compute_hashes_to_keys">::test_compute_hashes_to_keys;
  ]
  |> Test.run

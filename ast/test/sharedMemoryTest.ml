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


let () =
  "ast_shared_memory">:::[
    "normalize_handle_keys">::test_normalize_handle_keys;
  ]
  |> Test.run

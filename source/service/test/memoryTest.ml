(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Core.Int.of_string
end

module MockAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Annotations"

  (* Shared memory does not marshall strings *)
  let unmarshall value = value
end

module MockAnnotations = Memory.WithCache.Make (IntKey) (MockAnnotationValue)

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


let () = "memory" >::: ["test_reset" >:: test_reset] |> Test.run

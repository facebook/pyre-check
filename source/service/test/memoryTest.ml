(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  let from_string = Core.Int.of_string
end

module MockAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Annotations"
end

module MockAnnotations = Memory.WithCache.Make (IntKey) (MockAnnotationValue)

let test_reset _ =
  MockAnnotations.add 42 "derp";

  Memory.reset_shared_memory ();

  assert_bool "reset_shared_memory should clear all cached results" (not (MockAnnotations.mem 42));

  let heap_size = SharedMemory.heap_size () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 heap_size;

  let { SharedMemory.nonempty_slots; used_slots; _ } = SharedMemory.dep_stats () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 nonempty_slots;
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 used_slots;

  let { SharedMemory.nonempty_slots; used_slots; _ } = SharedMemory.hash_stats () in
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 nonempty_slots;
  assert_equal ~cmp:Int.equal ~printer:Int.to_string 0 used_slots;
  ()


let test_interner _ =
  let module StringInterner = Memory.Interner (struct
    include String

    let to_string x = x

    let prefix = Prefix.make ()

    let description = "string"
  end)
  in
  let foo = StringInterner.intern "foo" in
  let bar = StringInterner.intern "bar" in
  assert_equal (StringInterner.unintern foo) "foo";
  assert_equal (StringInterner.unintern bar) "bar";
  let second_foo = StringInterner.intern "foo" in
  let second_bar = StringInterner.intern "bar" in
  assert_equal foo second_foo;
  assert_equal bar second_bar;

  let words = List.init 1000 string_of_int in
  let interned_words = List.map (fun word -> word, StringInterner.intern word) words in
  List.iter (fun (word, id) -> assert_equal word (StringInterner.unintern id)) interned_words;
  let module IntSet = Set.Make (Int) in
  let size = interned_words |> List.map snd |> IntSet.of_list |> IntSet.cardinal in
  assert_equal size 1000;
  ()


let () = "memory" >::: ["test_reset" >:: test_reset; "test_interner" >:: test_interner] |> Test.run

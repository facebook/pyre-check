(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open LatticeOfMaps
open Test
module StringMap = IdentifierMap

type t =
  | Bottom
  | Foo
  | Top
[@@deriving compare, sexp, show]

let less_or_equal ~left ~right = [%compare: t] left right <= 0

let join left right =
  match left, right with
  | Bottom, other
  | other, Bottom ->
      other
  | _, Top
  | Top, _ ->
      Top
  | Foo, Foo -> Foo


let meet left right =
  match left, right with
  | Top, other
  | other, Top ->
      other
  | _, Bottom
  | Bottom, _ ->
      Bottom
  | Foo, Foo -> Foo


let _ = show

let test_less_or_equal _ =
  let assert_less_or_equal ~left ~right =
    assert_bool_equals (StringMap.less_or_equal ~less_or_equal_one:less_or_equal ~left ~right)
  in
  assert_less_or_equal ~expected:true ~left:StringMap.empty ~right:StringMap.empty;
  assert_less_or_equal
    ~expected:true
    ~left:(StringMap.of_alist_exn ["a", Bottom])
    ~right:StringMap.empty;
  assert_less_or_equal
    ~expected:false
    ~left:StringMap.empty
    ~right:(StringMap.of_alist_exn ["a", Bottom]);
  assert_less_or_equal
    ~expected:true
    ~left:(StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    ~right:(StringMap.of_alist_exn ["a", Top; "b", Top]);
  assert_less_or_equal
    ~expected:false
    ~left:(StringMap.of_alist_exn ["a", Top; "b", Bottom])
    ~right:(StringMap.of_alist_exn ["a", Bottom; "b", Bottom]);
  assert_less_or_equal
    ~expected:false
    ~left:(StringMap.of_alist_exn ["a", Top; "b", Top])
    ~right:(StringMap.of_alist_exn ["a", Top; "b", Bottom]);
  ()


let test_join _ =
  let assert_join ~expected left right =
    let assert_equal =
      assert_equal
        ~cmp:(StringMap.equal [%compare.equal: t])
        ~printer:(fun map -> StringMap.sexp_of_t [%sexp_of: t] map |> Sexp.to_string_hum)
    in
    assert_equal expected (StringMap.join ~join_one:join left right);
    assert_equal expected (StringMap.join ~join_one:join right left)
  in
  assert_join ~expected:StringMap.empty StringMap.empty StringMap.empty;
  assert_join ~expected:StringMap.empty (StringMap.of_alist_exn ["a", Bottom]) StringMap.empty;
  assert_join
    ~expected:(StringMap.of_alist_exn ["a", Top])
    (StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    (StringMap.of_alist_exn ["a", Top; "c", Bottom]);
  ()


let test_meet _ =
  let assert_meet ~expected left right =
    let assert_equal =
      assert_equal
        ~cmp:(StringMap.equal [%compare.equal: t])
        ~printer:(fun map -> StringMap.sexp_of_t [%sexp_of: t] map |> Sexp.to_string_hum)
    in
    assert_equal expected (StringMap.meet ~meet_one:meet left right);
    assert_equal expected (StringMap.meet ~meet_one:meet right left)
  in
  assert_meet ~expected:StringMap.empty StringMap.empty StringMap.empty;
  assert_meet
    ~expected:(StringMap.of_alist_exn ["a", Bottom])
    (StringMap.of_alist_exn ["a", Bottom])
    StringMap.empty;
  assert_meet
    ~expected:(StringMap.of_alist_exn ["a", Bottom; "b", Foo; "c", Bottom])
    (StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    (StringMap.of_alist_exn ["a", Top; "c", Bottom]);
  ()


let test_merge _ =
  let assert_merge ~expected left right =
    let assert_equal =
      assert_equal
        ~cmp:(StringMap.equal [%compare.equal: t])
        ~printer:(fun map -> StringMap.sexp_of_t [%sexp_of: t] map |> Sexp.to_string_hum)
    in
    let merge_with_left_bias left _ = left in
    assert_equal expected (StringMap.merge_with ~merge_one:merge_with_left_bias left right)
  in
  assert_merge ~expected:StringMap.empty StringMap.empty StringMap.empty;
  assert_merge
    ~expected:(StringMap.of_alist_exn ["a", Bottom])
    (StringMap.of_alist_exn ["a", Bottom])
    StringMap.empty;
  assert_merge
    ~expected:(StringMap.of_alist_exn ["a", Bottom; "b", Foo; "c", Bottom])
    (StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    (StringMap.of_alist_exn ["a", Top; "c", Bottom]);
  ()


let test_update_existing_entries _ =
  let assert_update ~expected ~old_map ~new_map =
    let assert_equal =
      assert_equal
        ~cmp:(StringMap.equal [%compare.equal: t])
        ~printer:(fun map -> StringMap.sexp_of_t [%sexp_of: t] map |> Sexp.to_string_hum)
    in
    assert_equal expected (StringMap.update_existing_entries ~map_to_update:old_map ~new_map)
  in
  assert_update
    ~expected:(StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    ~old_map:(StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    ~new_map:StringMap.empty;
  assert_update
    ~expected:(StringMap.of_alist_exn ["a", Top; "b", Foo])
    ~old_map:(StringMap.of_alist_exn ["a", Bottom; "b", Foo])
    ~new_map:(StringMap.of_alist_exn ["a", Top; "c", Bottom]);
  ()


let test_mem _ =
  assert_bool_equals
    ~expected:true
    (StringMap.mem (StringMap.of_alist_exn ["a", Bottom; "b", Foo]) "a");
  assert_bool_equals
    ~expected:false
    (StringMap.mem (StringMap.of_alist_exn ["a", Bottom; "b", Foo]) "z");
  ()


(* Test that we preserve all functions from the `Map` data structure from which we generated lattice
   functions. We are not limited to the functions specified in `MapSignature`. *)
let test_function_not_in_map_signature _ =
  (* `singleton` is not present in `MapSignature`. *)
  assert_bool_equals ~expected:true (StringMap.mem (StringMap.singleton "foo" 42) "foo");
  ()


let () =
  "latticeOfMaps"
  >::: [
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "meet" >:: test_meet;
         "merge" >:: test_merge;
         "update_existing_entries" >:: test_update_existing_entries;
         "mem" >:: test_mem;
         "function_not_in_map_signature" >:: test_function_not_in_map_signature;
       ]
  |> Test.run

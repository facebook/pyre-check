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

module T = struct
  type t = string [@@deriving compare, sexp]
end

module StringMapWithoutLatticeFunctions = Map.Make (T)

module StringMap = Make (struct
  include StringMapWithoutLatticeFunctions.Tree

  type key = T.t
end)

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


let () =
  "latticeOfMaps" >::: ["less_or_equal" >:: test_less_or_equal; "join" >:: test_join] |> Test.run

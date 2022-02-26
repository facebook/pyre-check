(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Core

type test_node = string Ast.Node.t [@@deriving compare, sexp, hash]

let test_equality _ =
  let compare_two_locations left right equal compare_equal hashes_equal =
    let value = "some_string" in
    let node_left = Node.create ~location:left value in
    let node_right = Node.create ~location:right value in
    let assert_bool_equal = assert_equal ~cmp:Bool.equal ~printer:Bool.to_string in
    assert_bool_equal ([%compare.equal: test_node] node_left node_right) equal;
    assert_bool_equal (compare_test_node node_left node_right = 0) compare_equal;
    assert_bool_equal (hash_test_node node_left = hash_test_node node_right) hashes_equal
  in
  let location_1 =
    {
      Location.start = { Location.line = 1; column = 1 };
      Location.stop = { Location.line = 2; column = 5 };
    }
  in
  let location_2 =
    {
      Location.start = { Location.line = 12; column = 3 };
      Location.stop = { Location.line = 12; column = 7 };
    }
  in
  compare_two_locations location_1 location_1 true true true;
  compare_two_locations Location.any location_1 false false false;
  compare_two_locations Location.any location_2 false false false;
  compare_two_locations location_1 location_2 false false false


let () = "node" >::: ["equality" >:: test_equality] |> Test.run

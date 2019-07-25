(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Ast
open Core
open Test

type test_node = string Ast.Node.t [@@deriving compare, eq, sexp, show, hash]

let test_equality _ =
  let compare_two_locations left right =
    let full_printer { Node.value; location } =
      Format.asprintf "%s/%s" (Location.Reference.show location) value
    in
    let value = "some_string" in
    let node_left = Node.create ~location:left value in
    let node_right = Node.create ~location:right value in
    assert_equal ~cmp:equal_test_node ~printer:full_printer node_left node_right;
    assert_equal ~printer:Int.to_string (hash_test_node node_left) (hash_test_node node_right)
  in
  let location_1 =
    {
      Location.path = !&"some_path";
      Location.start = { Location.line = 1; column = 1 };
      Location.stop = { Location.line = 2; column = 5 };
    }
  in
  let location_2 =
    {
      Location.path = !&"some_other_path";
      Location.start = { Location.line = 12; column = 3 };
      Location.stop = { Location.line = 12; column = 7 };
    }
  in
  compare_two_locations Location.Reference.any location_1;
  compare_two_locations Location.Reference.any location_2;
  compare_two_locations location_1 location_2


let () = "node" >::: ["equality" >:: test_equality] |> Test.run

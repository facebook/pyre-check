(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Core
open Taint


let test_common_prefix _ =
  let open AccessPathTree.Label in
  let path1 = [create_name_field "foo"; create_name_field "bar"] in
  let path2 = [create_name_field "foo"; create_name_field "baz"] in
  let common = [create_name_field "foo"] in
  let path3 = common_prefix path1 path2 in
  let path4 = common_prefix path1 path3 in
  let path5 = common_prefix path3 path2 in
  assert_equal common path3;
  assert_equal path3 path4;
  assert_equal path4 path5;
  ()

let () =
  "taintaccesspathtree">:::[
    "common_prefix">::test_common_prefix
  ]
  |> Test.run

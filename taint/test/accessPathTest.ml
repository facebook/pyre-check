(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core
open Analysis
open Taint
open Test

let test_of_expression context =
  let ( !+ ) expression = Test.parse_single_expression expression in
  let assert_of_expression ~resolution expression expected =
    assert_equal
      ~cmp:(Option.equal AccessPath.equal)
      ~printer:(function
        | None -> "None"
        | Some access_path -> AccessPath.show access_path)
      expected
      (AccessPath.of_expression ~resolution expression)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  assert_of_expression
    ~resolution
    !+"a"
    (Some { AccessPath.root = AccessPath.Root.Variable "a"; path = [] });
  assert_of_expression
    ~resolution
    !+"a.b"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "a";
         path = [AbstractTreeDomain.Label.Field "b"];
       });
  assert_of_expression
    ~resolution
    !+"a.b.c"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "a";
         path = [AbstractTreeDomain.Label.Field "b"; AbstractTreeDomain.Label.Field "c"];
       });
  assert_of_expression ~resolution !+"a.b.call()" None;

  let resolution =
    ScratchProject.setup ~context ["qualifier.py", "unannotated = unknown_value()"]
    |> ScratchProject.build_resolution
  in
  assert_of_expression
    ~resolution
    !"$local_qualifier$unannotated"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "qualifier";
         path = [AbstractTreeDomain.Label.Field "unannotated"];
       });
  assert_of_expression
    ~resolution
    !"$local_qualifier$missing"
    (Some { AccessPath.root = AccessPath.Root.Variable "$local_qualifier$missing"; path = [] })


let () = "taintaccesspath" >::: ["of_expression" >:: test_of_expression] |> Test.run

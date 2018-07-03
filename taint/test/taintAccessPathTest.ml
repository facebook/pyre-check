(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Taint
open Core
open Ast
open Expression
open Test


let test_normalize_access _ =
  let assert_normalized expression expected =
    let { Node.value = Access access; _ } = parse_single_expression expression in
    let normalized = AccessPath.normalize_access access in
    assert_equal ~printer:(AccessPath.show_normalized_expression) normalized expected
  in
  assert_normalized
    "a.b.c"
    (AccessPath.Access {
        member = Identifier.create "c";
        expression = AccessPath.Access {
            member = Identifier.create "b";
            expression = (AccessPath.Identifier (Identifier.create "a"))
          }
      })


let () =
  "taintaccesspath">:::[
    "normalize">::test_normalize_access;
  ]
  |> run_test_tt_main

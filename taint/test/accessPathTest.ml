(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Taint
open Core
open Ast
open Expression


let test_normalize_access _ =
  let assert_normalized expression expected =
    let access = Access.create expression in
    let normalized = AccessPath.normalize_access access in
    assert_equal ~printer:(AccessPath.show_normalized_expression) normalized expected
  in

  assert_normalized
    "a.b.c"
    (AccessPath.Global [
        Identifier.create "a";
        Identifier.create "b";
        Identifier.create "c";
      ]);

  assert_normalized
    "$a"
    (AccessPath.Local (
        Identifier.create "$a"
      ));

  assert_normalized
    "$a.b"
    (AccessPath.Access {
        expression = (AccessPath.Local (
            Identifier.create "$a"
          ));
        member = Identifier.create "b";
      })


let () =
  "taintaccesspath">:::[
    "normalize">::test_normalize_access;
  ]
  |> run_test_tt_main

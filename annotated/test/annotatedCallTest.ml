(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Pyre

open Test


let test_backup _ =
  let assert_backup call expected =
    let actual = Annotated.Call.create ~kind:Annotated.Call.Function call in
    let expected = expected >>| Annotated.Call.create ~kind:Annotated.Call.Function in
    assert_equal (Annotated.Call.backup actual) expected
  in
  assert_backup
    { Call.name = !"name"; arguments = [] }
    None;
  assert_backup
    { Call.name = !"__add__"; arguments = [] }
    (Some { Call.name = !"__radd__"; arguments = [] });
  assert_backup
    { Call.name = !"__sub__"; arguments = [] }
    (Some { Call.name = !"__rsub__"; arguments = [] })


let () =
  "call">:::[
    "backup">::test_backup;
  ]
  |> run_test_tt_main;

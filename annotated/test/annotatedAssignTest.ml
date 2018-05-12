(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement

open Test
open AnnotatedTest

module Assign = Annotated.Assign


let test_fold _ =
  let resolution =
    populate {|
      i = 1
      s = 'asdf'
      t = 1, 1.0
    |}
    |> resolution
  in
  let assert_fold source expected =
    let actual =
      let assign =
        match parse_single_statement source with
        | { Node.value = Statement.Assign assign; _ } -> assign
        | _ -> failwith "No Assign to parse"
      in
      let single_assignments ~target ~value_annotation assignments =
        (Expression.show target, value_annotation) :: assignments
      in
      Assign.create assign
      |> Assign.fold
        ~resolution
        ~f:single_assignments
        ~initial:[]
      |> List.rev
    in
    assert_equal actual expected
  in

  assert_fold "a = i" ["a", Type.integer];
  assert_fold "a, b = i, s" ["a", Type.integer; "b", Type.string];
  assert_fold "a, b = t" ["a", Type.integer; "b", Type.float];
  assert_fold "a, b = unknown" ["(a, b)", Type.Top]


let () =
  "assign">:::[
    "fold">::test_fold;
  ]
  |> run_test_tt_main;

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


let test_resolve_literal _ =
  let resolution =
    populate {|
      def foo()->int:
        ...
      i = 1
      j = foo()
      s = 'asdf'
      t = 1, 1.0
      awaitable: typing.Awaitable[int]
    |}
    |> resolution
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal ~printer:Type.show (Annotated.resolve_literal ~resolution expression) expected
  in
  assert_resolve_literal "i" Type.integer;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.integer;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Object;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Object


let () =
  "resolve">:::[
    "resolve_literal">::test_resolve_literal;
  ]
  |> run_test_tt_main;

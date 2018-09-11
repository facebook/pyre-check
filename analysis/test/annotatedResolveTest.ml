(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis

open Test
open AnnotatedTest


let test_resolve_literal _ =
  let resolution =
    populate {|
      class C:
        def __init__(self) -> None:
          pass
      def foo()->int:
        ...
      i = 1
      j = foo()
      s = 'asdf'
      t = 1, 1.0
      awaitable: typing.Awaitable[int]
    |}
    |> fun environment -> Environment.resolution environment ()
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal ~printer:Type.show (Annotated.resolve_literal ~resolution expression) expected
  in
  assert_resolve_literal "i" Type.Top;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.Top;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Top;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Top;
  assert_resolve_literal "C()" (Type.primitive "C")


let () =
  "resolve">:::[
    "resolve_literal">::test_resolve_literal;
  ]
  |> run_test_tt_main;

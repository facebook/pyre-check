(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Core

open Ast.Expression
open Analysis
open PyreCommand
open Protocol
open Pyre

open Test

let fake_root = Path.create_absolute "/tmp"


let assert_parses serialized query =
  assert_equal
    ~cmp:(fun left right -> Option.equal Request.equal left right)
    (Some (Request.TypeQueryRequest query))
    (Query.parse_query ~root:fake_root serialized)


let assert_fails_to_parse serialized =
  assert_equal
    None
    (Query.parse_query ~root:fake_root serialized)


let test_parse_query _ =
  assert_parses
    "less_or_equal(int, bool)"
    (LessOrEqual (Type.integer, Type.bool));
  assert_parses
    "less_or_equal (int, bool)"
    (LessOrEqual (Type.integer, Type.bool));
  assert_parses
    "less_or_equal(  int, int)"
    (LessOrEqual (Type.integer, Type.integer));

  assert_parses
    "meet(int, bool)"
    (Meet (Type.integer, Type.bool));

  assert_parses
    "join(int, bool)"
    (Join (Type.integer, Type.bool));

  assert_fails_to_parse "less_or_equal()";
  assert_fails_to_parse "less_or_equal(int, int, int)";
  assert_fails_to_parse "less_or_eq(int, bool)";

  assert_fails_to_parse "meet(int, int, int)";
  assert_fails_to_parse "meet(int)";

  assert_fails_to_parse "join(int)";
  assert_parses "superclasses(int)" (Superclasses (Type.integer));
  assert_fails_to_parse "superclasses()";
  assert_fails_to_parse "superclasses(int, bool)";

  assert_parses "normalizeType(int)" (NormalizeType (+Access (Access.create "int")));
  assert_fails_to_parse "normalizeType(int, str)";

  assert_equal
    (Query.parse_query ~root:fake_root "typecheckPath(fiddle.py)")
    (Some (Request.TypeCheckRequest
             (TypeCheckRequest.create
                ~check:[
                  File.create (Path.create_relative ~root:fake_root ~relative:"fiddle.py");
                ]
                ()
             )));

  assert_fails_to_parse "typecheck(1+2)"


let () =
  "query">:::[
    "parse_query">::test_parse_query;
  ]
  |> run_test_tt_main

(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Analysis
open ServerProtocol
open Pyre

let fake_root = Path.create_absolute "/tmp"


let assert_parses serialized query =
  assert_equal
    (CommandQuery.parse_query ~root:fake_root serialized)
    query


let test_parse_query _ =
  assert_parses
    "less_or_equal(int, bool)"
    (Some (Request.TypeQueryRequest (LessOrEqual (Type.integer, Type.bool))));
  assert_parses
    "less_or_equal (int, bool)"
    (Some (Request.TypeQueryRequest (LessOrEqual (Type.integer, Type.bool))));
  assert_parses
    "less_or_equal(  int, int)"
    (Some (Request.TypeQueryRequest (LessOrEqual (Type.integer, Type.integer))));

  assert_parses
    "meet(int, bool)"
    (Some (Request.TypeQueryRequest (Meet (Type.integer, Type.bool))));

  assert_parses
    "join(int, bool)"
    (Some (Request.TypeQueryRequest (Join (Type.integer, Type.bool))));

  assert_parses "less_or_equal()" None;
  assert_parses "less_or_equal(int, int, int)" None;
  assert_parses "less_or_eq(int, bool)" None;

  assert_parses "meet(int, int, int)" None;
  assert_parses "meet(int)" None;

  assert_parses "join(int)" None;
  assert_parses "superclasses(int)" (Some (Request.TypeQueryRequest (Superclasses (Type.integer))));
  assert_parses "superclasses()" None;
  assert_parses "superclasses(int, bool)" None;

  assert_parses
    "typecheckPath(fiddle.py)"
    (Some
       (Request.TypeCheckRequest
          (TypeCheckRequest.create
             ~check:[
               File.create (Path.create_relative ~root:fake_root ~relative:"fiddle.py");
             ]
             ()
          )));

  assert_parses "typecheck(1+2)" None


let () =
  "query">:::[
    "parse_query">::test_parse_query;
  ]
  |> run_test_tt_main

(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Analysis

let assert_parses serialized query =
  assert_equal
    (CommandQuery.parse_query serialized)
    query


let test_parse_query _ =
  assert_parses
    "less_or_equal(int, bool)"
    (Some (ServerProtocol.LessOrEqual (Type.integer, Type.bool)));
  assert_parses
    "less_or_equal (int, bool)"
    (Some (ServerProtocol.LessOrEqual (Type.integer, Type.bool)));
  assert_parses
    "less_or_equal(  int, int)"
    (Some (ServerProtocol.LessOrEqual (Type.integer, Type.integer)));

  assert_parses
    "meet(int, bool)"
    (Some (ServerProtocol.Meet (Type.integer, Type.bool)));

  assert_parses
    "join(int, bool)"
    (Some (ServerProtocol.Join (Type.integer, Type.bool)));

  assert_parses "less_or_equal()" None;
  assert_parses "less_or_equal(int, int, int)" None;
  assert_parses "less_or_eq(int, bool)" None;

  assert_parses "meet(int, int, int)" None;
  assert_parses "meet(int)" None;

  assert_parses "join(int)" None


let () =
  "query">:::[
    "parse_query">::test_parse_query;
  ]
  |> run_test_tt_main

(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open OUnit2
open Ast

let test_remove_leading_underscores _ =
  let assert_removed identifier expected =
    let removed =
      Identifier.create identifier
      |> Identifier.remove_leading_underscores
      |> Identifier.show
    in
    assert_equal ~printer:ident removed expected
  in

  assert_removed "$renamed_x" "$renamed_x";
  assert_removed "$renamed__x" "$renamed_x";
  assert_removed "$head_____x" "$head_x";
  assert_removed "$hx" "$hx";
  assert_removed "__a" "__a"


let () =
  "identifier">:::[
    "remove_leading_underscores">::test_remove_leading_underscores;
  ]
  |> run_test_tt_main

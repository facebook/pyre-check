(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast

let test_remove_leading_underscores _ =
  let assert_removed identifier expected =
    let removed = identifier |> Identifier.remove_leading_underscores in
    assert_equal ~printer:ident removed expected
  in
  assert_removed "$local_0$x" "$local_0$x";
  assert_removed "$local_0$_x" "$local_0$x";
  assert_removed "$local_0$_____x" "$local_0$x";
  assert_removed "$return" "$return";
  assert_removed "__name__" "__name__"


let () =
  "identifier" >::: ["remove_leading_underscores" >:: test_remove_leading_underscores] |> Test.run

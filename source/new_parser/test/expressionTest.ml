(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Test
open Ast.Expression

let expression_location_insensitive_equal left right =
  Expression.location_insensitive_compare left right |> Int.equal 0


let expression_print_to_sexp expression = Sexp.to_string_hum (Expression.sexp_of_t expression)

let assert_parsed ~context ~expected text =
  match PyreNewParser.parse_expression ~context text with
  | Result.Error { PyreNewParser.Error.message; _ } ->
      let message = Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual ->
      assert_equal
        ~cmp:expression_location_insensitive_equal
        ~printer:expression_print_to_sexp
        expected
        actual


let assert_not_parsed ~context text =
  match PyreNewParser.parse_expression ~context text with
  | Result.Ok actual ->
      let message =
        Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (Expression.sexp_of_t actual)
      in
      assert_failure message
  | Result.Error _ -> ()


let test_name _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "x" ~expected:(+Expression.Name (Name.Identifier "x"));
    assert_parsed "x1" ~expected:(+Expression.Name (Name.Identifier "x1"));
    assert_parsed "x_y" ~expected:(+Expression.Name (Name.Identifier "x_y"));
    assert_not_parsed "$x";
    assert_not_parsed "1x";
    ()
  in
  PyreNewParser.with_context do_test


let () = "parse_expression" >::: ["name" >:: test_name] |> Test.run

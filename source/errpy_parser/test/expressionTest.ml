(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let assert_parsed ~expected text =
  let open Ast.Statement in
  match PyreErrpyParser.parse_module text with
  | Result.Error message ->
      let message = Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual -> (
      match List.hd actual with
      | Some first_statement -> (
          match first_statement.value with
          | Statement.Expression as_first_expression ->
              assert_equal
                ~cmp:expression_location_insensitive_equal
                ~printer:expression_print_to_sexp
                expected
                as_first_expression
          | _ ->
              let message =
                Format.asprintf
                  "expected expression to be parsed but got: %a"
                  Sexp.pp_hum
                  (Statement.sexp_of_t first_statement)
              in
              assert_failure message)
      | None -> assert_failure "expected parse result")


let assert_not_parsed text =
  match PyreErrpyParser.parse_module text with
  | Result.Ok _ ->
      let message = Format.asprintf "Unexpected parsing success of input: %s" text in
      assert_failure message
  | Result.Error _ -> ()


let test_name _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed "x" ~expected:(+Expression.Name (Name.Identifier "x"));
  assert_parsed "x1" ~expected:(+Expression.Name (Name.Identifier "x1"));
  assert_parsed "x_y" ~expected:(+Expression.Name (Name.Identifier "x_y"));
  assert_not_parsed "$x";
  assert_not_parsed "1x";
  ()


let () =
  "parse_expression"
  >::: [
         "name" >:: test_name;
         (*"attribute" >:: test_attribute;*)
         (*"constant" >:: test_constant;*)
         (*"fstring" >:: test_fstring;*)
         (*"await_yield" >:: test_await_yield;*)
         (*"ternary_walrus" >:: test_ternary_walrus;*)
         (*"container_literals" >:: test_container_literals;*)
         (*"comprehensions" >:: test_comprehensions;*)
         (*"starred" >:: test_starred;*)
         (*"boolean_operators" >:: test_boolean_operators;*)
         (*"unary_operators" >:: test_unary_operators;*)
         (*"comparison_operators" >:: test_comparison_operators;*)
         (*"binary_operators" >:: test_binary_operators;*)
         (*"test_call" >:: test_call;*)
         (*"test_subscript" >:: test_subscript;*)
         (*"test_lambda" >:: test_lambda;*)
       ]
  |> Test.run

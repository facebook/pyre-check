(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Statement
open Expression
open Test

let parse_single_pattern pattern_string =
  match parse_single_statement ("match subject:\n  case " ^ pattern_string ^ ":\n    pass") with
  | { Node.value = Statement.Match { cases = [{ Match.Case.pattern; _ }]; _ }; _ } -> pattern
  | _ -> failwith "Could not parse single pattern"


let assert_pattern_to_condition ~pattern ~expected =
  assert_equal
    ~cmp:(fun x y -> Int.equal (Expression.location_insensitive_compare x y) 0)
    ~printer:Expression.show
    (MatchTranslate.pattern_to_condition ~subject:!"subject" (parse_single_pattern pattern))
    (parse_single_expression expected)


let test_as _ =
  assert_pattern_to_condition ~pattern:"x" ~expected:"(x := subject) == x";
  assert_pattern_to_condition ~pattern:"42 as x" ~expected:"(x := subject) == x and x == 42";
  ()


let test_value _ =
  assert_pattern_to_condition ~pattern:"42" ~expected:"subject == 42";
  ()


let test_wildcard _ =
  assert_pattern_to_condition ~pattern:"_" ~expected:"True";
  ()


let () =
  "match_translate"
  >::: ["as" >:: test_as; "value" >:: test_value; "wildcard" >:: test_wildcard]
  |> Test.run

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
    (parse_single_expression expected)
    (MatchTranslate.pattern_to_condition ~subject:!"subject" (parse_single_pattern pattern))


let test_as _ =
  assert_pattern_to_condition ~pattern:"x" ~expected:"(x := subject) == x";
  assert_pattern_to_condition ~pattern:"42 as x" ~expected:"(x := subject) == x and x == 42";
  ()


let test_class _ =
  assert_pattern_to_condition ~pattern:"Foo()" ~expected:"isinstance(subject, Foo)";
  assert_pattern_to_condition
    ~pattern:"Foo(1)"
    ~expected:"isinstance(subject, Foo) and getattr(subject, subject.__match_args__[0]) == 1";
  assert_pattern_to_condition
    ~pattern:"Foo(x)"
    ~expected:"isinstance(subject, Foo) and (x := getattr(subject, subject.__match_args__[0])) == x";
  assert_pattern_to_condition
    ~pattern:"Foo(field=1)"
    ~expected:"isinstance(subject, Foo) and subject.field == 1";
  assert_pattern_to_condition
    ~pattern:"Foo(field=x)"
    ~expected:"isinstance(subject, Foo) and (x := subject.field) == x";
  assert_pattern_to_condition
    ~pattern:"Foo(1, x, field_alpha=1, field_beta=y)"
    ~expected:
      {|
      (isinstance(subject, Foo) and
       getattr(subject, subject.__match_args__[0]) == 1 and
       (x := getattr(subject, subject.__match_args__[1])) == x and
       subject.field_alpha == 1 and
       (y := subject.field_beta) == y)
    |};
  ()


let test_guard _ =
  let parse_single_case case_string =
    match parse_single_statement ("match subject:\n  case " ^ case_string ^ ":\n    pass") with
    | { Node.value = Statement.Match { cases = [case]; _ }; _ } -> case
    | _ -> failwith "Could not parse single case"
  in
  let assert_to_condition ~case ~expected =
    assert_equal
      ~cmp:(fun x y -> Int.equal (Expression.location_insensitive_compare x y) 0)
      ~printer:Expression.show
      (parse_single_expression expected)
      (MatchTranslate.to_condition ~subject:!"subject" ~case:(parse_single_case case))
  in
  assert_to_condition ~case:"_ if guard" ~expected:"guard";
  assert_to_condition ~case:"42" ~expected:"subject == 42";
  assert_to_condition ~case:"x if x > 0" ~expected:"(x := subject) == x and x > 0";
  ()


let test_mapping _ =
  assert_pattern_to_condition ~pattern:"{1:2}" ~expected:"subject[1] == 2";
  assert_pattern_to_condition ~pattern:"{1:2, 3:4}" ~expected:"subject[1] == 2 and subject[3] == 4";
  assert_pattern_to_condition ~pattern:"{1:x}" ~expected:"(x := subject[1]) == x";
  assert_pattern_to_condition
    ~pattern:"{1:str() as x}"
    ~expected:"(x := subject[1]) == x and isinstance(x, str)";
  ()


let test_nested _ =
  assert_pattern_to_condition
    ~pattern:
      {|(
        {"text": (["hello", x] as y) | (["world", x] as y) , "foo": bar} |
        {"text2": (["hello", x] as y) | (["world", x] as y) , "foo": bar}
      )|}
    ~expected:
      {|(
          ( ( ((y := subject["text"]) == y and (y[0] == "hello" and (x := y[1]) == x))
              or
              ((y := subject["text"]) == y and (y[0] == "world" and (x := y[1]) == x))
            )
            and
            (bar := subject["foo"]) == bar
          )
          or
          ( ( ((y := subject["text2"]) == y and (y[0] == "hello" and (x := y[1]) == x))
              or
              ((y := subject["text2"]) == y and (y[0] == "world" and (x := y[1]) == x))
            )
            and
            (bar := subject["foo"]) == bar
          )
      )|};
  ()


let test_or _ =
  assert_pattern_to_condition
    ~pattern:"1|2|3"
    ~expected:"subject == 1 or subject == 2 or subject == 3";
  ()


let test_sequence _ =
  assert_pattern_to_condition
    ~pattern:"[7, 8, 9]"
    ~expected:"subject[0] == 7 and subject[1] == 8 and subject[2] == 9";
  assert_pattern_to_condition
    ~pattern:"[x, y]"
    ~expected:"(x := subject[0]) == x and (y := subject[1]) == y";
  assert_pattern_to_condition
    ~pattern:"[7, 8, *_, 99, 100]"
    ~expected:"subject[0] == 7 and subject[1] == 8 and subject[-2] == 99 and subject[-1] == 100";
  assert_pattern_to_condition
    ~pattern:"[first, *_, last]"
    ~expected:"(first := subject[0]) == first and (last := subject[-1]) == last";
  ()


let test_value _ =
  assert_pattern_to_condition ~pattern:"42" ~expected:"subject == 42";
  ()


let test_wildcard _ =
  assert_pattern_to_condition ~pattern:"_" ~expected:"True";
  ()


let () =
  "match_translate"
  >::: [
         "as" >:: test_as;
         "class" >:: test_class;
         "guard" >:: test_guard;
         "mapping" >:: test_mapping;
         "nested" >:: test_nested;
         "or" >:: test_or;
         "sequence" >:: test_sequence;
         "value" >:: test_value;
         "wildcard" >:: test_wildcard;
       ]
  |> Test.run

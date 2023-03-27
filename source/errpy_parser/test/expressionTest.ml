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


let test_attribute _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "a.b"
    ~expected:(+Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false }));
  assert_parsed
    "a  .  b"
    ~expected:(+Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false }));
  assert_parsed
    "a.b.c"
    ~expected:
      (+Expression.Name
          (Name.Attribute
             {
               base =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               attribute = "c";
               special = false;
             }));
  assert_parsed
    "1.0.b"
    ~expected:
      (+Expression.Name
          (Name.Attribute
             { base = +Expression.Constant (Constant.Float 1.0); attribute = "b"; special = false }));

  (*TODO (T148669698): assert_not_parsed "a.async"; *)
  assert_not_parsed "a.1";
  assert_not_parsed "1 .0 .a";
  ()


let test_constant _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in

  assert_parsed "None" ~expected:(+Expression.Constant Constant.NoneLiteral);
  assert_parsed "False" ~expected:(+Expression.Constant Constant.False);
  assert_parsed "True" ~expected:(+Expression.Constant Constant.True);
  assert_parsed "..." ~expected:(+Expression.Constant Constant.Ellipsis);

  assert_parsed "1" ~expected:(+Expression.Constant (Constant.Integer 1));
  assert_parsed "-1" ~expected:(+Expression.Constant (Constant.Integer (-1)));
  assert_parsed "+1" ~expected:(+Expression.Constant (Constant.Integer 1));
  assert_parsed "0" ~expected:(+Expression.Constant (Constant.Integer 0));
  assert_parsed "00" ~expected:(+Expression.Constant (Constant.Integer 0));
  assert_parsed "00_0" ~expected:(+Expression.Constant (Constant.Integer 0));
  assert_parsed "0x1" ~expected:(+Expression.Constant (Constant.Integer 0x1));
  assert_parsed "1_01" ~expected:(+Expression.Constant (Constant.Integer 101));
  assert_parsed "(1)" ~expected:(+Expression.Constant (Constant.Integer 1));
  assert_parsed "((1))" ~expected:(+Expression.Constant (Constant.Integer 1));
  assert_parsed "0XaBc" ~expected:(+Expression.Constant (Constant.Integer 0xABC));
  assert_parsed "0o13" ~expected:(+Expression.Constant (Constant.Integer 0o13));
  assert_parsed "0b01" ~expected:(+Expression.Constant (Constant.Integer 0b01));
  assert_parsed "0b0_1" ~expected:(+Expression.Constant (Constant.Integer 0b01));
  assert_parsed "0b_0_1" ~expected:(+Expression.Constant (Constant.Integer 0b01));
  assert_not_parsed "1L";

  (*TODO (T148669698): assert_not_parsed "01";*)
  assert_parsed "1.0" ~expected:(+Expression.Constant (Constant.Float 1.0));
  assert_parsed "1_0.1_01" ~expected:(+Expression.Constant (Constant.Float 10.101));
  assert_parsed ".1" ~expected:(+Expression.Constant (Constant.Float 0.1));
  assert_parsed "1." ~expected:(+Expression.Constant (Constant.Float 1.0));
  assert_parsed "1e10" ~expected:(+Expression.Constant (Constant.Float 1e10));
  assert_parsed "0.1j" ~expected:(+Expression.Constant (Constant.Complex 0.1));
  assert_parsed "1e10j" ~expected:(+Expression.Constant (Constant.Complex 1e10));
  assert_parsed "1e1_0j" ~expected:(+Expression.Constant (Constant.Complex 1e10));
  assert_parsed "2j" ~expected:(+Expression.(Constant (Constant.Complex 2.0)));
  assert_parsed "2J" ~expected:(+Expression.(Constant (Constant.Complex 2.0)));
  assert_not_parsed "0xZ";
  (*TODO (T148669698): assert_not_parsed "0_1";*)
  assert_not_parsed "0o9";
  assert_not_parsed "1a3";

  assert_parsed
    "'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "\"foo\""
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "'''foo'''"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "\"\"\"foo\"\"\""
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "r'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "R'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "b'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
  assert_parsed
    "u'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
  assert_parsed
    "bR'foo'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
  assert_parsed
    "'foo' 'bar'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foobar")));
  (*TODO (T148669698): assert_parsed "\"'\"" ~expected:(+Expression.Constant (Constant.String
    (StringLiteral.create "'")));*)
  assert_parsed
    "'\"'"
    ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "\"")));

  (*TODO (T148669698): assert_parsed "\"\\\"\"" ~expected:(+Expression.Constant (Constant.String
    (StringLiteral.create "\"")));*)
  (*TODO (T148669698): assert_parsed "\"\\'\"" ~expected:(+Expression.Constant (Constant.String
    (StringLiteral.create "'")));*)
  (*TODO (T148669698): assert_parsed "\"\"\"\nfoo\"\"\"" ~expected:(+Expression.Constant
    (Constant.String (StringLiteral.create "\nfoo")));*)
  (*TODO (T148669698): assert_parsed "\"f.o\\no\"" ~expected:(+Expression.Constant (Constant.String
    (StringLiteral.create "f.o\no")));*)
  (*TODO (T148669698): assert_not_parsed "ub'foo'";*)
  (*TODO (T148669698): assert_not_parsed "ur'foo'";*)
  ()


let test_unary_operators _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "not x"
    ~expected:
      (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" });
  assert_parsed
    "~x"
    ~expected:
      (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Invert; operand = !"x" });
  assert_parsed
    "+x"
    ~expected:
      (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Positive; operand = !"x" });
  assert_parsed
    "-x"
    ~expected:
      (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Negative; operand = !"x" });
  assert_parsed
    "+-x"
    ~expected:
      (+Expression.UnaryOperator
          {
            UnaryOperator.operator = UnaryOperator.Positive;
            operand =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Negative; operand = !"x" };
          });
  assert_parsed
    "not ~x"
    ~expected:
      (+Expression.UnaryOperator
          {
            UnaryOperator.operator = UnaryOperator.Not;
            operand =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Invert; operand = !"x" };
          });
  assert_parsed
    "-(not x)"
    ~expected:
      (+Expression.UnaryOperator
          {
            UnaryOperator.operator = UnaryOperator.Negative;
            operand =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" };
          });

  (* `not` has higher precedence than `and`/`or` *)
  assert_parsed
    "not x and y"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" };
            operator = BooleanOperator.And;
            right = !"y";
          });
  assert_parsed
    "not x or y"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" };
            operator = BooleanOperator.Or;
            right = !"y";
          });
  assert_not_parsed "not";
  assert_not_parsed "x not";
  assert_not_parsed "x~";
  assert_not_parsed "x+";
  assert_not_parsed "x-";
  assert_not_parsed "+ not x";
  ()


let test_boolean_operators _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "True and False"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left = +Expression.Constant Constant.True;
            operator = BooleanOperator.And;
            right = +Expression.Constant Constant.False;
          });
  assert_parsed
    "True or False"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left = +Expression.Constant Constant.True;
            operator = BooleanOperator.Or;
            right = +Expression.Constant Constant.False;
          });
  assert_parsed
    "1 and 2 and 3"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.BooleanOperator
                 {
                   BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
                   operator = BooleanOperator.And;
                   right = +Expression.Constant (Constant.Integer 2);
                 };
            operator = BooleanOperator.And;
            right = +Expression.Constant (Constant.Integer 3);
          });
  assert_parsed
    "1 or 2 or 3"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.BooleanOperator
                 {
                   BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
                   operator = BooleanOperator.Or;
                   right = +Expression.Constant (Constant.Integer 2);
                 };
            operator = BooleanOperator.Or;
            right = +Expression.Constant (Constant.Integer 3);
          });
  assert_parsed
    "1 and 2 or 3"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.BooleanOperator
                 {
                   BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
                   operator = BooleanOperator.And;
                   right = +Expression.Constant (Constant.Integer 2);
                 };
            operator = BooleanOperator.Or;
            right = +Expression.Constant (Constant.Integer 3);
          });
  (* `and` has higher precedence than `or` *)
  assert_parsed
    "1 or 2 and 3"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
            operator = BooleanOperator.Or;
            right =
              +Expression.BooleanOperator
                 {
                   BooleanOperator.left = +Expression.Constant (Constant.Integer 2);
                   operator = BooleanOperator.And;
                   right = +Expression.Constant (Constant.Integer 3);
                 };
          });

  assert_not_parsed "and";
  assert_not_parsed "or";
  assert_not_parsed "and or";
  assert_not_parsed "True and";
  assert_not_parsed "and True";
  assert_not_parsed "True or";
  assert_not_parsed "or True";
  ()


let test_await_yield _ =
  let assert_parsed = assert_parsed in
  (*let assert_not_parsed = assert_not_parsed in*)
  assert_parsed "await x" ~expected:(+Expression.Await !"x");
  assert_parsed "(yield)" ~expected:(+Expression.Yield None);
  assert_parsed
    "(yield 1)"
    ~expected:(+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))));
  assert_parsed "(yield from x)" ~expected:(+Expression.YieldFrom !"x");
  assert_parsed "(yield (await x))" ~expected:(+Expression.Yield (Some (+Expression.Await !"x")));
  assert_parsed "await (yield from x)" ~expected:(+Expression.Await (+Expression.YieldFrom !"x"));
  assert_parsed "await (yield x)" ~expected:(+Expression.Await (+Expression.Yield (Some !"x")));

  (*TODO: FIX In ERRPY: assert_not_parsed "await"; *)
  (* Standalone yield/yield from expressions are required to be protected with parenthesis. *)
  (*TODO: FIX In ERRPY: assert_not_parsed "yield"; *)
  (*TODO: FIX In ERRPY: assert_not_parsed "yield from"; *)
  ()


let test_container_literals _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed "()" ~expected:(+Expression.Tuple []);
  (* The outer parens will *NOT* be interpreted as another layer of tuple. *)
  assert_parsed "(())" ~expected:(+Expression.Tuple []);
  assert_parsed "((),)" ~expected:(+Expression.Tuple [+Expression.Tuple []]);
  assert_parsed "1," ~expected:(+Expression.Tuple [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed "(1,)" ~expected:(+Expression.Tuple [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed
    "1,2"
    ~expected:
      (+Expression.Tuple
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "1,2,"
    ~expected:
      (+Expression.Tuple
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "(1,2)"
    ~expected:
      (+Expression.Tuple
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "(\n\t1,\n\t2\n)"
    ~expected:
      (+Expression.Tuple
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

  assert_parsed "[]" ~expected:(+Expression.List []);
  assert_parsed "[[]]" ~expected:(+Expression.List [+Expression.List []]);
  assert_parsed "[1]" ~expected:(+Expression.List [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed "[1,]" ~expected:(+Expression.List [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed
    "[1,2]"
    ~expected:
      (+Expression.List
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "[1,2,]"
    ~expected:
      (+Expression.List
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "[\n\t1,\n\t2\n]"
    ~expected:
      (+Expression.List
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

  assert_parsed "{1}" ~expected:(+Expression.Set [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed "{1,}" ~expected:(+Expression.Set [+Expression.Constant (Constant.Integer 1)]);
  assert_parsed
    "{1,2}"
    ~expected:
      (+Expression.Set
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "{1,2,}"
    ~expected:
      (+Expression.Set
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
  assert_parsed
    "{\n\t1,\n\t2\n}"
    ~expected:
      (+Expression.Set
          [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

  assert_parsed "{}" ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [] });
  assert_parsed
    "{{}}"
    ~expected:(+Expression.Set [+Expression.Dictionary { Dictionary.entries = []; keywords = [] }]);
  assert_parsed
    "{1:2}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
              ];
            keywords = [];
          });
  assert_parsed
    "{1:2,}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
              ];
            keywords = [];
          });
  assert_parsed
    "{1:2,3:4}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                  value = +Expression.Constant (Constant.Integer 4);
                };
              ];
            keywords = [];
          });
  assert_parsed
    "{1:2,3:4,}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                  value = +Expression.Constant (Constant.Integer 4);
                };
              ];
            keywords = [];
          });
  assert_parsed
    "{\n\t1:2,\n\t3:4\n}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                  value = +Expression.Constant (Constant.Integer 4);
                };
              ];
            keywords = [];
          });
  assert_parsed
    "{**foo}"
    ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [!"foo"] });
  assert_parsed
    "{**foo, **bar}"
    ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [!"foo"; !"bar"] });
  assert_parsed
    "{**foo, 1:2}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
              ];
            keywords = [!"foo"];
          });
  assert_parsed
    "{1:2,**foo}"
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries =
              [
                {
                  Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                  value = +Expression.Constant (Constant.Integer 2);
                };
              ];
            keywords = [!"foo"];
          });

  assert_not_parsed "(1,2";
  assert_not_parsed "[1,2";
  assert_not_parsed "{1,2";
  ()


let () =
  "parse_expression"
  >::: [
         "name" >:: test_name;
         "attribute" >:: test_attribute;
         "constant" >:: test_constant;
         "unary_operators" >:: test_unary_operators;
         "boolean_operators" >:: test_boolean_operators;
         "await_yield" >:: test_await_yield;
         "container_literals" >:: test_container_literals;
         (*"fstring" >:: test_fstring;*)
         (*"ternary_walrus" >:: test_ternary_walrus;*)
         (*"comprehensions" >:: test_comprehensions;*)
         (*"starred" >:: test_starred;*)
         (*"comparison_operators" >:: test_comparison_operators;*)
         (*"binary_operators" >:: test_binary_operators;*)
         (*"test_call" >:: test_call;*)
         (*"test_subscript" >:: test_subscript;*)
         (*"test_lambda" >:: test_lambda;*)
       ]
  |> Test.run

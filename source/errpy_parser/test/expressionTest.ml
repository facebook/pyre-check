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
  let check_ast (actual_ast : Ast.Statement.t list) =
    match List.hd actual_ast with
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
    | None -> assert_failure "expected parse result"
  in
  match PyreErrpyParser.parse_module text with
  | Result.Error error -> (
      match error with
      | Recoverable recoverable -> check_ast recoverable.recovered_ast
      | Unrecoverable message ->
          let message = Format.sprintf "Unexpected parsing failure: %s" message in
          assert_failure message)
  | Result.Ok actual_ast -> check_ast actual_ast


let assert_not_parsed text =
  match PyreErrpyParser.parse_module text with
  | Result.Ok _ ->
      let message = Format.asprintf "Unexpected parsing success of input: %s" text in
      assert_failure message
  | Result.Error error -> (
      match error with
      | Recoverable _ -> ()
      | Unrecoverable message ->
          let message = Format.sprintf "Unexpected errpy stacktrace thrown: %s" message in
          assert_failure message)


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

  (*TODO (T148669698): assert_not_parsed "await"; *)
  (* Standalone yield/yield from expressions are required to be protected with parenthesis. *)
  (*TODO (T148669698): assert_not_parsed "yield"; *)
  (*TODO (T148669698): assert_not_parsed "yield from"; *)
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


let test_comprehensions _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "(a for a in b)"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = false;
                };
              ];
          });
  assert_parsed
    "(a for a in (b for b in c))"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator =
                    +Expression.Generator
                       {
                         Comprehension.element = !"b";
                         generators =
                           [
                             {
                               Comprehension.Generator.target = !"b";
                               iterator = !"c";
                               conditions = [];
                               async = false;
                             };
                           ];
                       };
                  conditions = [];
                  async = false;
                };
              ];
          });
  assert_parsed
    "(c for a in b for c in a)"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"c";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = false;
                };
                {
                  Comprehension.Generator.target = !"c";
                  iterator = !"a";
                  conditions = [];
                  async = false;
                };
              ];
          });
  assert_parsed
    "(a for a in b if True)"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [+Expression.Constant Constant.True];
                  async = false;
                };
              ];
          });
  assert_parsed
    "(a for a in b if True if False)"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions =
                    [+Expression.Constant Constant.True; +Expression.Constant Constant.False];
                  async = false;
                };
              ];
          });
  assert_parsed
    "(a async for a in b)"
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = true;
                };
              ];
          });
  assert_parsed
    "[a for a in b]"
    ~expected:
      (+Expression.ListComprehension
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = false;
                };
              ];
          });
  assert_parsed
    "{a for a in b}"
    ~expected:
      (+Expression.SetComprehension
          {
            Comprehension.element = !"a";
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = false;
                };
              ];
          });
  assert_parsed
    "{a:0 for a in b}"
    ~expected:
      (+Expression.DictionaryComprehension
          {
            Comprehension.element =
              { Dictionary.Entry.key = !"a"; value = +Expression.Constant (Constant.Integer 0) };
            generators =
              [
                {
                  Comprehension.Generator.target = !"a";
                  iterator = !"b";
                  conditions = [];
                  async = false;
                };
              ];
          });

  assert_not_parsed "(*a for a in b)";
  assert_not_parsed "[*a for a in b]";
  assert_not_parsed "{*a for a in b}";
  assert_not_parsed "{**a for a in b}";
  (*TODO (T148669698): assert_not_parsed "[i+1 for i in (j := stuff)]";*)
  (*TODO (T148669698): assert_not_parsed "[i+1 for i in range(2) for j in (k := stuff)]";*)
  (*TODO (T148669698): assert_not_parsed "[i+1 for i in [j for j in (k := stuff)]]";*)
  (*TODO (T148669698): assert_not_parsed "[i+1 for i in (lambda: (j := stuff))()]";*)
  ()


let test_starred _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in

  assert_parsed "( *a, )" ~expected:(+Expression.Tuple [+Expression.Starred (Starred.Once !"a")]);
  assert_parsed
    "(a, *b)"
    ~expected:(+Expression.Tuple [!"a"; +Expression.Starred (Starred.Once !"b")]);
  assert_parsed
    "( *a, b )"
    ~expected:(+Expression.Tuple [+Expression.Starred (Starred.Once !"a"); !"b"]);
  assert_parsed
    "( *a, *b )"
    ~expected:
      (+Expression.Tuple
          [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

  assert_parsed "[*a]" ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a")]);
  assert_parsed "[*a,]" ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a")]);
  assert_parsed
    "[*a, b]"
    ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a"); !"b"]);
  assert_parsed
    "[a, *b]"
    ~expected:(+Expression.List [!"a"; +Expression.Starred (Starred.Once !"b")]);
  assert_parsed
    "[*a, *b]"
    ~expected:
      (+Expression.List
          [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

  assert_parsed "{*a}" ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a")]);
  assert_parsed "{*a,}" ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a")]);
  assert_parsed
    "{*a, b}"
    ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a"); !"b"]);
  assert_parsed
    "{a, *b}"
    ~expected:(+Expression.Set [!"a"; +Expression.Starred (Starred.Once !"b")]);
  assert_parsed
    "{*a, *b}"
    ~expected:
      (+Expression.Set
          [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

  (* Star expressions cannot appear out-of-context. *)
  assert_not_parsed "*x";
  assert_not_parsed "**x";
  assert_not_parsed "*(x)";
  assert_not_parsed "*[x]";
  (*TODO (T148669698): assert_not_parsed "( *x )";*)
  assert_not_parsed "( **x )";
  assert_not_parsed "[**x]";
  ()


let test_binary_operators _ =
  let assert_parsed = assert_parsed in
  assert_parsed
    "x + y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__add__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x - y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__sub__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x * y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__mul__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x @ y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__matmul__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x / y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__truediv__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x % y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__mod__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x ** y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__pow__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x >> y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__rshift__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x << y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__lshift__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x | y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__or__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x ^ y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__xor__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x & y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__and__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x // y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__floordiv__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "x + y + z"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute
                    {
                      base =
                        +Expression.Call
                           {
                             callee =
                               +Expression.Name
                                  (Name.Attribute
                                     { base = !"x"; attribute = "__add__"; special = true });
                             arguments = [{ Call.Argument.name = None; value = !"y" }];
                           };
                      attribute = "__add__";
                      special = true;
                    });
            arguments = [{ Call.Argument.name = None; value = !"z" }];
          });
  assert_parsed
    "x + (y + z)"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__add__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee =
                           +Expression.Name
                              (Name.Attribute { base = !"y"; attribute = "__add__"; special = true });
                         arguments = [{ Call.Argument.name = None; value = !"z" }];
                       };
                };
              ];
          });
  assert_parsed
    "x + y * z"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__add__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee =
                           +Expression.Name
                              (Name.Attribute { base = !"y"; attribute = "__mul__"; special = true });
                         arguments = [{ Call.Argument.name = None; value = !"z" }];
                       };
                };
              ];
          });
  assert_parsed
    "x * -y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__mul__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.UnaryOperator
                       { UnaryOperator.operator = UnaryOperator.Negative; operand = !"y" };
                };
              ];
          });
  assert_parsed
    "x + y.z"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"x"; attribute = "__add__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Name
                       (Name.Attribute { base = !"y"; attribute = "z"; special = false });
                };
              ];
          });
  assert_parsed
    "x < y + z"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left = !"x";
            operator = ComparisonOperator.LessThan;
            right =
              +Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"y"; attribute = "__add__"; special = true });
                   arguments = [{ Call.Argument.name = None; value = !"z" }];
                 };
          });
  ()


let test_comparison_operators _ =
  let assert_parsed = assert_parsed in
  assert_parsed
    "a == b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.Equals; right = !"b" });
  assert_parsed
    "a != b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.NotEquals; right = !"b" });
  assert_parsed
    "a < b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.LessThan; right = !"b" });
  assert_parsed
    "a <= b"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.LessThanOrEquals;
            right = !"b";
          });
  assert_parsed
    "a > b"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.GreaterThan;
            right = !"b";
          });
  assert_parsed
    "a >= b"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.GreaterThanOrEquals;
            right = !"b";
          });
  assert_parsed
    "a is b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.Is; right = !"b" });
  assert_parsed
    "a is not b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.IsNot; right = !"b" });
  assert_parsed
    "a in b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.In; right = !"b" });
  assert_parsed
    "a not in b"
    ~expected:
      (+Expression.ComparisonOperator
          { ComparisonOperator.left = !"a"; operator = ComparisonOperator.NotIn; right = !"b" });
  assert_parsed
    "a < b < c"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"a";
                   operator = ComparisonOperator.LessThan;
                   right = !"b";
                 };
            operator = BooleanOperator.And;
            right =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"b";
                   operator = ComparisonOperator.LessThan;
                   right = !"c";
                 };
          });
  (* Comparisions bind tighter than `not` *)
  assert_parsed
    "not a in b"
    ~expected:
      (+Expression.UnaryOperator
          {
            UnaryOperator.operator = UnaryOperator.Not;
            operand =
              +Expression.ComparisonOperator
                 { ComparisonOperator.left = !"a"; operator = ComparisonOperator.In; right = !"b" };
          });
  (* Comparisions bind tighter than `and`/`or` *)
  assert_parsed
    "a < b and b < c"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"a";
                   operator = ComparisonOperator.LessThan;
                   right = !"b";
                 };
            operator = BooleanOperator.And;
            right =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"b";
                   operator = ComparisonOperator.LessThan;
                   right = !"c";
                 };
          });
  assert_parsed
    "a < b or b < c"
    ~expected:
      (+Expression.BooleanOperator
          {
            BooleanOperator.left =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"a";
                   operator = ComparisonOperator.LessThan;
                   right = !"b";
                 };
            operator = BooleanOperator.Or;
            right =
              +Expression.ComparisonOperator
                 {
                   ComparisonOperator.left = !"b";
                   operator = ComparisonOperator.LessThan;
                   right = !"c";
                 };
          });
  (* Comparisions bind looser than `+`/`-` *)
  assert_parsed
    "+a >= -b"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Positive; operand = !"a" };
            operator = ComparisonOperator.GreaterThanOrEquals;
            right =
              +Expression.UnaryOperator
                 { UnaryOperator.operator = UnaryOperator.Negative; operand = !"b" };
          });
  ()


let test_fstring _ =
  let assert_parsed ~expected text =
    assert_parsed ~expected:(+Expression.FormatString expected) text
  in
  let literal value = Substring.Literal (+value) in
  let format value = Substring.Format (+value) in
  assert_parsed "f'foo'" ~expected:[literal "foo"];
  (*TODO (T148669698): assert_parsed "F'foo'" ~expected:[literal "foo"];*)
  assert_parsed "f'foo' f'bar'" ~expected:[literal "foobar"];
  assert_parsed "f'foo' 'bar'" ~expected:[literal "foobar"];
  assert_parsed "f'{  3.14  }'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'{3.14:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'{3.14:.10f}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'{3.14!s:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'{3.14!r:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'{3.14!a:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed "f'a{{'" ~expected:[literal "a{"];
  assert_parsed "f'a{{b}}c'" ~expected:[literal "a{b}c"];
  assert_parsed
    "f'{\"{{}}\"}'"
    ~expected:[format (Expression.Constant (Constant.String (StringLiteral.create "{{}}")))];
  assert_parsed
    "f\"{'''x'''}\""
    ~expected:[format (Expression.Constant (Constant.String (StringLiteral.create "x")))];
  (* The `=` syntax was added in Python 3.8. *)
  assert_parsed "f'{x=}'" ~expected:[literal "x="; format (Expression.Name (Name.Identifier "x"))];
  assert_parsed
    "f'{x=!r:^20}'"
    ~expected:[literal "x="; format (Expression.Name (Name.Identifier "x"))];
  assert_parsed "f'{3.14!a:{w}.{p}}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
  assert_parsed
    "f'a{b}c'"
    ~expected:[literal "a"; format (Expression.Name (Name.Identifier "b")); literal "c"];
  assert_parsed
    "f'a{\"b\"}c'"
    ~expected:
      [
        literal "a";
        format (Expression.Constant (Constant.String (StringLiteral.create "b")));
        literal "c";
      ];
  assert_parsed
    "f'{f\"a{b}c\"}'"
    ~expected:
      [
        format
          (Expression.FormatString
             [literal "a"; format (Expression.Name (Name.Identifier "b")); literal "c"]);
      ];

  (*TODO (T148669698):assert_parsed "f'''\n{b}\nc'''" ~expected:[literal "\n"; format
    (Expression.Name (Name.Identifier "b")); literal "\nc"];*)
  (*TODO (T148669698):assert_not_parsed "f'' b''";*)
  (*TODO (T148669698):assert_not_parsed "f'{\"x'";*)
  (*TODO (T148669698):assert_not_parsed "f'{\"x}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{(\"x'";*)
  (*TODO (T148669698):assert_not_parsed "f'{(\"x)'";*)
  (*TODO (T148669698):assert_not_parsed "f'{((}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{a[4)}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{a(4]}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{a[4}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{a(4}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{1#}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{#}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{,}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{\n}'";*)
  (*TODO (T148669698):assert_not_parsed "f'{\\'a\\'}'";*)
  ()


let test_ternary_walrus _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "1 if 2 else 3"
    ~expected:
      (+Expression.Ternary
          {
            Ternary.target = +Expression.Constant (Constant.Integer 1);
            test = +Expression.Constant (Constant.Integer 2);
            alternative = +Expression.Constant (Constant.Integer 3);
          });
  assert_parsed
    "1 if 2 else 3 if 4 else 5"
    ~expected:
      (+Expression.Ternary
          {
            Ternary.target = +Expression.Constant (Constant.Integer 1);
            test = +Expression.Constant (Constant.Integer 2);
            alternative =
              +Expression.Ternary
                 {
                   Ternary.target = +Expression.Constant (Constant.Integer 3);
                   test = +Expression.Constant (Constant.Integer 4);
                   alternative = +Expression.Constant (Constant.Integer 5);
                 };
          });
  assert_parsed
    "(a := 1)"
    ~expected:
      (+Expression.WalrusOperator
          { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
  assert_parsed
    "(a := (b := 1))"
    ~expected:
      (+Expression.WalrusOperator
          {
            target = !"a";
            value =
              +Expression.WalrusOperator
                 { target = !"b"; value = +Expression.Constant (Constant.Integer 1) };
          });

  (* Standalone assignment expressions are required to be protected with parenthesis. *)
  (*TODO (T148669698): assert_not_parsed "a := 1";*)
  (*TODO (T148669698): assert_not_parsed "(a := 1) := 2"; *)
  assert_not_parsed "(a.b := 1)";
  assert_not_parsed "(a[b] := 1)";
  assert_not_parsed "((a, b) := 1)";
  assert_not_parsed "([a, b] := 1)";
  ()


let test_call _ =
  let assert_parsed = assert_parsed in
  assert_parsed "foo()" ~expected:(+Expression.Call { callee = !"foo"; arguments = [] });
  assert_parsed
    "foo(x)"
    ~expected:
      (+Expression.Call
          { callee = !"foo"; arguments = [{ Call.Argument.name = None; value = !"x" }] });
  assert_parsed
    "foo(x,)"
    ~expected:
      (+Expression.Call
          { callee = !"foo"; arguments = [{ Call.Argument.name = None; value = !"x" }] });
  assert_parsed
    "foo(x=y)"
    ~expected:
      (+Expression.Call
          { callee = !"foo"; arguments = [{ Call.Argument.name = Some (+"x"); value = !"y" }] });
  assert_parsed
    "foo(x=y,)"
    ~expected:
      (+Expression.Call
          { callee = !"foo"; arguments = [{ Call.Argument.name = Some (+"x"); value = !"y" }] });
  assert_parsed
    "foo(x, y)"
    ~expected:
      (+Expression.Call
          {
            callee = !"foo";
            arguments =
              [
                { Call.Argument.name = None; value = !"x" };
                { Call.Argument.name = None; value = !"y" };
              ];
          });
  assert_parsed
    "foo((x, y))"
    ~expected:
      (+Expression.Call
          {
            callee = !"foo";
            arguments = [{ Call.Argument.name = None; value = +Expression.Tuple [!"x"; !"y"] }];
          });
  assert_parsed
    "foo(x,y=z)"
    ~expected:
      (+Expression.Call
          {
            callee = !"foo";
            arguments =
              [
                { Call.Argument.name = None; value = !"x" };
                { Call.Argument.name = Some (+"y"); value = !"z" };
              ];
          });
  assert_parsed
    "a.foo(x)"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name (Name.Attribute { base = !"a"; attribute = "foo"; special = false });
            arguments = [{ Call.Argument.name = None; value = !"x" }];
          });
  assert_parsed
    "foo(1, a = 2, *args, **kwargs)"
    ~expected:
      (+Expression.Call
          {
            callee = !"foo";
            arguments =
              [
                { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                { Call.Argument.name = None; value = +Expression.Starred (Starred.Once !"args") };
                {
                  Call.Argument.name = Some (+"a");
                  value = +Expression.Constant (Constant.Integer 2);
                };
                { Call.Argument.name = None; value = +Expression.Starred (Starred.Twice !"kwargs") };
              ];
          });
  assert_parsed
    "foo(x) + y"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute
                    {
                      base =
                        +Expression.Call
                           {
                             callee = !"foo";
                             arguments = [{ Call.Argument.name = None; value = !"x" }];
                           };
                      attribute = "__add__";
                      special = true;
                    });
            arguments = [{ Call.Argument.name = None; value = !"y" }];
          });
  assert_parsed
    "foo(x) > y"
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left =
              +Expression.Call
                 { callee = !"foo"; arguments = [{ Call.Argument.name = None; value = !"x" }] };
            operator = ComparisonOperator.GreaterThan;
            right = !"y";
          });
  assert_parsed
    "not foo(x)"
    ~expected:
      (+Expression.UnaryOperator
          {
            UnaryOperator.operator = UnaryOperator.Not;
            UnaryOperator.operand =
              +Expression.Call
                 { callee = !"foo"; arguments = [{ Call.Argument.name = None; value = !"x" }] };
          });
  ()


let test_subscript _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "a[b]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"b" }];
          });
  assert_parsed
    "a.__getitem__(b)"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = false });
            arguments = [{ Call.Argument.name = None; value = !"b" }];
          });
  assert_parsed
    "a[(b)]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments = [{ Call.Argument.name = None; value = !"b" }];
          });
  assert_parsed
    "a[1 < 2]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                         operator = ComparisonOperator.LessThan;
                         right = +Expression.Constant (Constant.Integer 2);
                       };
                };
              ];
          });
  assert_parsed
    "a[b,c]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments = [{ Call.Argument.name = None; value = +Expression.Tuple [!"b"; !"c"] }];
          });
  assert_parsed
    "a[b].c"
    ~expected:
      (+Expression.Name
          (Name.Attribute
             {
               base =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute
                              { base = !"a"; attribute = "__getitem__"; special = true });
                      arguments = [{ Call.Argument.name = None; value = !"b" }];
                    };
               attribute = "c";
               special = false;
             }));
  assert_parsed
    "a[:]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[::]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[1:]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[:1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[::1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[1:1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[1::1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[:1:1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant Constant.NoneLiteral;
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[1:1:1]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Call
                       {
                         callee = !"slice";
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                           ];
                       };
                };
              ];
          });
  assert_parsed
    "a[:1,2]"
    ~expected:
      (+Expression.Call
          {
            callee =
              +Expression.Name
                 (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
            arguments =
              [
                {
                  Call.Argument.name = None;
                  value =
                    +Expression.Tuple
                       [
                         +Expression.Call
                            {
                              callee = !"slice";
                              arguments =
                                [
                                  {
                                    Call.Argument.name = None;
                                    value = +Expression.Constant Constant.NoneLiteral;
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value = +Expression.Constant (Constant.Integer 1);
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value = +Expression.Constant Constant.NoneLiteral;
                                  };
                                ];
                            };
                         +Expression.Constant (Constant.Integer 2);
                       ];
                };
              ];
          });

  assert_not_parsed "a[]";
  assert_not_parsed "a[:::]";
  ()


let test_lambda _ =
  let assert_parsed = assert_parsed in
  assert_parsed
    "lambda: 1"
    ~expected:
      (+Expression.Lambda
          { Lambda.parameters = []; body = +Expression.Constant (Constant.Integer 1) });
  assert_parsed
    "lambda x: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
            body = !"x";
          });
  assert_parsed
    "lambda x,: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
            body = !"x";
          });
  assert_parsed
    "lambda x: x is y"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
            body =
              +Expression.ComparisonOperator
                 { ComparisonOperator.left = !"x"; operator = ComparisonOperator.Is; right = !"y" };
          });

  assert_parsed
    "lambda x,y=1: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "x"; value = None; annotation = None };
                +{
                   Parameter.name = "y";
                   value = Some (+Expression.Constant (Constant.Integer 1));
                   annotation = None;
                 };
              ];
            body = !"x";
          });
  assert_parsed
    "lambda *args, **kwargs: args"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "*args"; value = None; annotation = None };
                +{ Parameter.name = "**kwargs"; value = None; annotation = None };
              ];
            body = !"args";
          });
  assert_parsed
    "lambda x, /: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "x"; value = None; annotation = None };
                +{ Parameter.name = "/"; value = None; annotation = None };
              ];
            body = !"x";
          });
  assert_parsed
    "lambda *, x: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "*"; value = None; annotation = None };
                +{ Parameter.name = "x"; value = None; annotation = None };
              ];
            body = !"x";
          });
  assert_parsed
    "lambda *args, x: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "*args"; value = None; annotation = None };
                +{ Parameter.name = "x"; value = None; annotation = None };
              ];
            body = !"x";
          });
  assert_parsed
    "lambda x, /, y, z=1, *, w=2: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "x"; value = None; annotation = None };
                +{ Parameter.name = "/"; value = None; annotation = None };
                +{ Parameter.name = "y"; value = None; annotation = None };
                +{
                   Parameter.name = "z";
                   value = Some (+Expression.Constant (Constant.Integer 1));
                   annotation = None;
                 };
                +{ Parameter.name = "*"; value = None; annotation = None };
                +{
                   Parameter.name = "w";
                   value = Some (+Expression.Constant (Constant.Integer 2));
                   annotation = None;
                 };
              ];
            body = !"x";
          });
  assert_parsed
    "lambda x, y=1, /, z=2, *, u, v=3, **w: x"
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [
                +{ Parameter.name = "x"; value = None; annotation = None };
                +{
                   Parameter.name = "y";
                   value = Some (+Expression.Constant (Constant.Integer 1));
                   annotation = None;
                 };
                +{ Parameter.name = "/"; value = None; annotation = None };
                +{
                   Parameter.name = "z";
                   value = Some (+Expression.Constant (Constant.Integer 2));
                   annotation = None;
                 };
                +{ Parameter.name = "*"; value = None; annotation = None };
                +{ Parameter.name = "u"; value = None; annotation = None };
                +{
                   Parameter.name = "v";
                   value = Some (+Expression.Constant (Constant.Integer 3));
                   annotation = None;
                 };
                +{ Parameter.name = "**w"; value = None; annotation = None };
              ];
            body = !"x";
          });
  (*TODO (T148669698): assert_not_parsed "lambda x=1, y: x"; assert_not_parsed "lambda *x, *, y:
    x";*)
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
         "comprehensions" >:: test_comprehensions;
         "starred" >:: test_starred;
         "binary_operators" >:: test_binary_operators;
         "comparison_operators" >:: test_comparison_operators;
         "fstring" >:: test_fstring;
         "ternary_walrus" >:: test_ternary_walrus;
         "call" >:: test_call;
         "subscript" >:: test_subscript;
         "lambda" >:: test_lambda;
       ]
  |> Test.run

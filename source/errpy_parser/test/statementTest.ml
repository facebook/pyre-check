(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Test
open Ast
open Ast.Expression
open Ast.Statement

let statements_location_insensitive_equal left right =
  List.compare Statement.location_insensitive_compare left right |> Int.equal 0


let statements_print_to_sexp statements =
  Sexp.to_string_hum ((List.sexp_of_t Statement.sexp_of_t) statements)


let assert_parsed ~expected text =
  match PyreErrpyParser.parse_module text with
  | Result.Error message ->
      let message = Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual ->
      assert_equal
        ~cmp:statements_location_insensitive_equal
        ~printer:statements_print_to_sexp
        expected
        actual


let assert_not_parsed text =
  match PyreErrpyParser.parse_module text with
  | Result.Ok _ ->
      let message = Format.asprintf "Unexpected parsing success of input: %s" text in
      assert_failure message
  | Result.Error _ -> ()


let test_pass_break_continue _ =
  let assert_parsed = assert_parsed in
  (*TODO: FIX In ERRPY: let assert_not_parsed = assert_not_parsed in*)
  assert_parsed "pass" ~expected:[+Statement.Pass];
  assert_parsed "break" ~expected:[+Statement.Break];
  assert_parsed "continue" ~expected:[+Statement.Continue];
  assert_parsed "pass\npass" ~expected:[+Statement.Pass; +Statement.Pass];
  (*TODO: FIX In ERRPY: assert_not_parsed "pass\n pass";*)
  assert_parsed
    "break\ncontinue\npass\n"
    ~expected:[+Statement.Break; +Statement.Continue; +Statement.Pass];
  ()


let test_global_nonlocal _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed "global a" ~expected:[+Statement.Global ["a"]];
  assert_parsed "global a, b" ~expected:[+Statement.Global ["a"; "b"]];
  assert_parsed "nonlocal a" ~expected:[+Statement.Nonlocal ["a"]];
  assert_parsed "nonlocal a, b" ~expected:[+Statement.Nonlocal ["a"; "b"]];

  assert_not_parsed "global";
  assert_not_parsed "nonlocal";
  ()


let test_expression_return_raise _ =
  let assert_parsed = assert_parsed in
  assert_parsed "a" ~expected:[+Statement.Expression !"a"];
  assert_parsed
    "a is b"
    ~expected:
      [
        +Statement.Expression
           (+Expression.ComparisonOperator
               { ComparisonOperator.left = !"a"; operator = ComparisonOperator.Is; right = !"b" });
      ];
  assert_parsed
    "foo(x)"
    ~expected:
      [
        +Statement.Expression
           (+Expression.Call
               { callee = !"foo"; arguments = [{ Call.Argument.name = None; value = !"x" }] });
      ];
  assert_parsed
    "return"
    ~expected:[+Statement.Return { Return.expression = None; is_implicit = false }];
  assert_parsed
    "return a"
    ~expected:[+Statement.Return { Return.expression = Some !"a"; is_implicit = false }];
  assert_parsed "raise" ~expected:[+Statement.Raise { Raise.expression = None; from = None }];
  assert_parsed "raise a" ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = None }];
  assert_parsed
    "raise a from b"
    ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = Some !"b" }];
  ()


let test_assert_delete _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "assert a"
    ~expected:
      [+Statement.Assert { Assert.test = !"a"; message = None; origin = Assert.Origin.Assertion }];
  assert_parsed
    "assert a, b"
    ~expected:
      [
        +Statement.Assert
           { Assert.test = !"a"; message = Some !"b"; origin = Assert.Origin.Assertion };
      ];
  assert_parsed
    "assert (a, b)"
    ~expected:
      [
        +Statement.Assert
           {
             Assert.test = +Expression.Tuple [!"a"; !"b"];
             message = None;
             origin = Assert.Origin.Assertion;
           };
      ];
  assert_parsed
    "assert a is not None, 'b or c'"
    ~expected:
      [
        +Statement.Assert
           {
             Assert.test =
               +Expression.ComparisonOperator
                  {
                    ComparisonOperator.left = !"a";
                    operator = ComparisonOperator.IsNot;
                    right = +Expression.Constant Constant.NoneLiteral;
                  };
             message = Some (+Expression.Constant (Constant.String (StringLiteral.create "b or c")));
             origin = Assert.Origin.Assertion;
           };
      ];
  assert_parsed "del ()" ~expected:[+Statement.Delete [+Expression.Tuple []]];
  assert_parsed "del a" ~expected:[+Statement.Delete [!"a"]];
  assert_parsed "del (a)" ~expected:[+Statement.Delete [!"a"]];
  assert_parsed "del a, b" ~expected:[+Statement.Delete [!"a"; !"b"]];
  assert_parsed "del (a, b)" ~expected:[+Statement.Delete [+Expression.Tuple [!"a"; !"b"]]];

  assert_not_parsed "assert";
  assert_not_parsed "del";
  ()


let test_import _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "import a"
    ~expected:
      [+Statement.Import { Import.from = None; imports = [+{ Import.name = !&"a"; alias = None }] }];
  assert_parsed
    "import a.b"
    ~expected:
      [
        +Statement.Import
           { Import.from = None; imports = [+{ Import.name = !&"a.b"; alias = None }] };
      ];
  assert_parsed
    "import a as b"
    ~expected:
      [
        +Statement.Import
           { Import.from = None; imports = [+{ Import.name = !&"a"; alias = Some "b" }] };
      ];
  assert_parsed
    "import a as b, c, d as e"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = None;
             imports =
               [
                 +{ Import.name = !&"a"; alias = Some "b" };
                 +{ Import.name = !&"c"; alias = None };
                 +{ Import.name = !&"d"; alias = Some "e" };
               ];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from a import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"a");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from a import *"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"a");
             imports = [+{ Import.name = !&"*"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from . import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&".");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from ...foo import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"...foo");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from .....foo import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&".....foo");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from .a import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&".a");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from ..a import b"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"..a");
             imports = [+{ Import.name = !&"b"; alias = None }];
           };
      ];
  assert_parsed
    "from a import (b, c)"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"a");
             imports =
               [+{ Import.name = !&"b"; alias = None }; +{ Import.name = !&"c"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from a.b import c"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"a.b");
             imports = [+{ Import.name = !&"c"; alias = None }];
           };
      ];
  (*FIXME (T148694587): For tests of the form: `from ... import ...` test for actual locations,
    don't just use create_with_default_location *)
  assert_parsed
    "from f import a as b, c, d as e"
    ~expected:
      [
        +Statement.Import
           {
             Import.from = Some (Node.create_with_default_location !&"f");
             imports =
               [
                 +{ Import.name = !&"a"; alias = Some "b" };
                 +{ Import.name = !&"c"; alias = None };
                 +{ Import.name = !&"d"; alias = Some "e" };
               ];
           };
      ];

  assert_not_parsed "import";
  assert_not_parsed "import .";
  assert_not_parsed "import a.async";
  assert_not_parsed "from import foo";
  ()


let test_for_while_if _ =
  let assert_parsed = assert_parsed in
  let assert_not_parsed = assert_not_parsed in
  assert_parsed
    "for a in b: c\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Statement.Expression !"c"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for a in b:\n  c\n  d"
    ~expected:
      [
        +Statement.For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Statement.Expression !"c"; +Statement.Expression !"d"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for a, b in c: d\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = +Expression.Tuple [!"a"; !"b"];
             iterator = !"c";
             body = [+Statement.Expression !"d"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for (a, b) in c: d\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = +Expression.Tuple [!"a"; !"b"];
             iterator = !"c";
             body = [+Statement.Expression !"d"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for [a, b] in c: d\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = +Expression.List [!"a"; !"b"];
             iterator = !"c";
             body = [+Statement.Expression !"d"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for a.b in c: d\n"
    ~expected:
      [
        +Statement.For
           {
             For.target =
               +Expression.Name
                  (Name.Attribute { Name.Attribute.base = !"a"; attribute = "b"; special = false });
             iterator = !"c";
             body = [+Statement.Expression !"d"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "async for a in b: c\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Statement.Expression !"c"];
             orelse = [];
             async = true;
           };
      ];
  assert_parsed
    "for a in b:\n\tc\n"
    ~expected:
      [
        +Statement.For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Statement.Expression !"c"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "for a in b:\n\tc\nelse:\n\td\n\te"
    ~expected:
      [
        +Statement.For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Statement.Expression !"c"];
             orelse = [+Statement.Expression !"d"; +Statement.Expression !"e"];
             async = false;
           };
      ];
  assert_parsed
    "for a, *b in c: \n\ta"
    ~expected:
      [
        +Statement.For
           {
             For.target = +Expression.Tuple [!"a"; +Expression.Starred (Starred.Once !"b")];
             iterator = !"c";
             body = [+Statement.Expression !"a"];
             orelse = [];
             async = false;
           };
      ];
  assert_parsed
    "while a: b\n"
    ~expected:
      [+Statement.While { While.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed
    "while a:\n  b\n  c\n"
    ~expected:
      [
        +Statement.While
           {
             While.test = !"a";
             body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
             orelse = [];
           };
      ];
  assert_parsed
    "while a:\n\tb\nelse:\n\tc\n\td\n"
    ~expected:
      [
        +Statement.While
           {
             While.test = !"a";
             body = [+Statement.Expression !"b"];
             orelse = [+Statement.Expression !"c"; +Statement.Expression !"d"];
           };
      ];
  assert_parsed
    "if a: b\n"
    ~expected:[+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed
    "if a:\n  b\n  c\n"
    ~expected:
      [
        +Statement.If
           {
             If.test = !"a";
             body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
             orelse = [];
           };
      ];
  assert_parsed
    "if a: b\nelif c: d"
    ~expected:
      [
        +Statement.If
           {
             If.test = !"a";
             body = [+Statement.Expression !"b"];
             orelse =
               [+Statement.If { If.test = !"c"; body = [+Statement.Expression !"d"]; orelse = [] }];
           };
      ];
  assert_parsed
    "if a:\n\n\tb\n"
    ~expected:[+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed
    "if a:\n\tb\n\n\tc"
    ~expected:
      [
        +Statement.If
           {
             If.test = !"a";
             body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
             orelse = [];
           };
      ];
  assert_parsed
    "if a:\n\tb\nelse:\n\tc\n\td\n"
    ~expected:
      [
        +Statement.If
           {
             If.test = !"a";
             body = [+Statement.Expression !"b"];
             orelse = [+Statement.Expression !"c"; +Statement.Expression !"d"];
           };
      ];

  assert_not_parsed "for";
  assert_not_parsed "for a in b";
  assert_not_parsed "while a";
  (*TODO: FIX In ERRPY: assert_not_parsed "while a:";*)
  assert_not_parsed "if a";
  (*TODO: FIX In ERRPY: assert_not_parsed "if a:";*)
  ()


let () =
  "parse_statements"
  >::: [
         "pass_break_continue" >:: test_pass_break_continue;
         "global_nonlocal" >:: test_global_nonlocal;
         "expression_return_raise" >:: test_expression_return_raise;
         "assert_delete" >:: test_assert_delete;
         "import" >:: test_import;
         "for_while_if" >:: test_for_while_if;
         (*"try" >:: test_try;*)
         (*"with" >:: test_with;*)
         (*"assign" >:: test_assign;*)
         (*"define" >:: test_define;*)
         (*"class" >:: test_class;*)
         (*TODO: FIX In ERRPY: "match" >:: test_match;*)
       ]
  |> Test.run

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
open Ast.Statement

let statements_location_insensitive_equal left right =
  List.compare Statement.location_insensitive_compare left right |> Int.equal 0


let statements_print_to_sexp statements =
  Sexp.to_string_hum ((List.sexp_of_t Statement.sexp_of_t) statements)


let assert_parsed ~context ~expected text =
  match PyreNewParser.parse_module ~context ~enable_type_comment:true text with
  | Result.Error { PyreNewParser.Error.message; _ } ->
      let message = Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual ->
      assert_equal
        ~cmp:statements_location_insensitive_equal
        ~printer:statements_print_to_sexp
        expected
        actual


let assert_not_parsed = ExpressionTest.assert_not_parsed

let test_pass_break_continue _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "pass" ~expected:[+Statement.Pass];
    assert_parsed "break" ~expected:[+Statement.Break];
    assert_parsed "continue" ~expected:[+Statement.Continue];
    assert_parsed "pass\npass" ~expected:[+Statement.Pass; +Statement.Pass];
    assert_not_parsed "pass\n  pass";
    assert_parsed
      "break\ncontinue\npass\n"
      ~expected:[+Statement.Break; +Statement.Continue; +Statement.Pass];
    ()
  in
  PyreNewParser.with_context do_test


let test_global_nonlocal _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "global a" ~expected:[+Statement.Global ["a"]];
    assert_parsed "global a, b" ~expected:[+Statement.Global ["a"; "b"]];
    assert_parsed "nonlocal a" ~expected:[+Statement.Nonlocal ["a"]];
    assert_parsed "nonlocal a, b" ~expected:[+Statement.Nonlocal ["a"; "b"]];

    assert_not_parsed "global";
    assert_not_parsed "nonlocal";
    ()
  in
  PyreNewParser.with_context do_test


let test_expression_return_raise _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
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
    assert_parsed
      "raise a"
      ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = None }];
    assert_parsed
      "raise a from b"
      ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = Some !"b" }];
    ()
  in
  PyreNewParser.with_context do_test


let test_assert_delete _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
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
               message =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "b or c")));
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
  in
  PyreNewParser.with_context do_test


let test_import _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "import a"
      ~expected:
        [
          +Statement.Import
             { Import.from = None; imports = [+{ Import.name = !&"a"; alias = None }] };
        ];
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
    assert_parsed
      "from a import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"a"; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from a import *"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"a"; imports = [+{ Import.name = !&"*"; alias = None }] };
        ];
    assert_parsed
      "from . import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"."; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from ...foo import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"...foo"; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from .....foo import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&".....foo"; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from .a import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&".a"; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from ..a import b"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"..a"; imports = [+{ Import.name = !&"b"; alias = None }] };
        ];
    assert_parsed
      "from a import (b, c)"
      ~expected:
        [
          +Statement.Import
             {
               Import.from = Some !&"a";
               imports =
                 [+{ Import.name = !&"b"; alias = None }; +{ Import.name = !&"c"; alias = None }];
             };
        ];
    assert_parsed
      "from a.b import c"
      ~expected:
        [
          +Statement.Import
             { Import.from = Some !&"a.b"; imports = [+{ Import.name = !&"c"; alias = None }] };
        ];
    assert_parsed
      "from f import a as b, c, d as e"
      ~expected:
        [
          +Statement.Import
             {
               Import.from = Some !&"f";
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
  in
  PyreNewParser.with_context do_test


let test_for_while_if _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
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
                 [
                   +Statement.If { If.test = !"c"; body = [+Statement.Expression !"d"]; orelse = [] };
                 ];
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
    assert_not_parsed "while a:";
    assert_not_parsed "if a";
    assert_not_parsed "if a:";
    ()
  in
  PyreNewParser.with_context do_test


let test_try _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "try:\n\ta\nfinally:\n\tb"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers = [];
               orelse = [];
               finally = [+Statement.Expression !"b"];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept:\n\tb"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [{ Try.Handler.kind = None; name = None; body = [+Statement.Expression !"b"] }];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept a:\n\tb"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   { Try.Handler.kind = Some !"a"; name = None; body = [+Statement.Expression !"b"] };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept a as b:\n\tb"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   {
                     Try.Handler.kind = Some !"a";
                     name = Some "b";
                     body = [+Statement.Expression !"b"];
                   };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept a or b:\n\tc"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   {
                     Try.Handler.kind =
                       Some
                         (+Expression.BooleanOperator
                             {
                               BooleanOperator.left = !"a";
                               operator = BooleanOperator.Or;
                               right = !"b";
                             });
                     name = None;
                     body = [+Statement.Expression !"c"];
                   };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept a or b as e:\n\tc"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   {
                     Try.Handler.kind =
                       Some
                         (+Expression.BooleanOperator
                             {
                               BooleanOperator.left = !"a";
                               operator = BooleanOperator.Or;
                               right = !"b";
                             });
                     name = Some "e";
                     body = [+Statement.Expression !"c"];
                   };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept (a, b) as c:\n\tb"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   {
                     Try.Handler.kind = Some (+Expression.Tuple [!"a"; !"b"]);
                     name = Some "c";
                     body = [+Statement.Expression !"b"];
                   };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [
                   {
                     Try.Handler.kind = Some !"a";
                     name = Some "b";
                     body = [+Statement.Expression !"b"];
                   };
                   {
                     Try.Handler.kind = Some !"d";
                     name = None;
                     body = [+Statement.Expression !"e"];
                   };
                 ];
               orelse = [];
               finally = [];
             };
        ];
    assert_parsed
      "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"];
               handlers =
                 [{ Try.Handler.kind = None; name = None; body = [+Statement.Expression !"b"] }];
               orelse = [+Statement.Expression !"c"];
               finally = [+Statement.Expression !"d"];
             };
        ];
    assert_parsed
      "try:\n\ta\n\tb\nexcept:\n\tc\n\td\nelse:\n\te\n\tf\nfinally:\n\tg\n\th"
      ~expected:
        [
          +Statement.Try
             {
               Try.body = [+Statement.Expression !"a"; +Statement.Expression !"b"];
               handlers =
                 [
                   {
                     Try.Handler.kind = None;
                     name = None;
                     body = [+Statement.Expression !"c"; +Statement.Expression !"d"];
                   };
                 ];
               orelse = [+Statement.Expression !"e"; +Statement.Expression !"f"];
               finally = [+Statement.Expression !"g"; +Statement.Expression !"h"];
             };
        ];

    assert_not_parsed "try: a";
    assert_not_parsed "try:\n\ta\nelse:\n\tb";
    assert_not_parsed "try:\n\ta\nexcept a, b:\n\tb";
    ()
  in
  PyreNewParser.with_context do_test


let test_with _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    assert_parsed
      "with a: b\n"
      ~expected:
        [
          +Statement.With
             { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = false };
        ];
    assert_parsed
      "with a:\n  b\n  c"
      ~expected:
        [
          +Statement.With
             {
               With.items = [!"a", None];
               body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
               async = false;
             };
        ];
    assert_parsed
      "with (yield from a): b\n"
      ~expected:
        [
          +Statement.With
             {
               With.items = [+Expression.YieldFrom !"a", None];
               body = [+Statement.Expression !"b"];
               async = false;
             };
        ];
    assert_parsed
      "async with a: b\n"
      ~expected:
        [
          +Statement.With
             { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = true };
        ];
    assert_parsed
      "with a as b: b\n"
      ~expected:
        [
          +Statement.With
             { With.items = [!"a", Some !"b"]; body = [+Statement.Expression !"b"]; async = false };
        ];
    assert_parsed
      "with a as b, c as d: b\n"
      ~expected:
        [
          +Statement.With
             {
               With.items = [!"a", Some !"b"; !"c", Some !"d"];
               body = [+Statement.Expression !"b"];
               async = false;
             };
        ];
    assert_parsed
      "with a, c as d: b\n"
      ~expected:
        [
          +Statement.With
             {
               With.items = [!"a", None; !"c", Some !"d"];
               body = [+Statement.Expression !"b"];
               async = false;
             };
        ];
    assert_parsed
      "with (a as b, c as d,): b\n"
      ~expected:
        [
          +Statement.With
             {
               With.items = [!"a", Some !"b"; !"c", Some !"d"];
               body = [+Statement.Expression !"b"];
               async = false;
             };
        ];
    assert_parsed
      "with (\n  a as b,\n  c as d\n):\n  b\n"
      ~expected:
        [
          +Statement.With
             {
               With.items = [!"a", Some !"b"; !"c", Some !"d"];
               body = [+Statement.Expression !"b"];
               async = false;
             };
        ];
    ()
  in
  PyreNewParser.with_context do_test


let test_define _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "def foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(*, a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "*"; value = None; annotation = None };
                       +{ Parameter.name = "a"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a, /, b):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "a"; value = None; annotation = None };
                       +{ Parameter.name = "/"; value = None; annotation = None };
                       +{ Parameter.name = "b"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(**a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "**a"; value = None; annotation = None }];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a, b,) -> c:\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "a"; value = None; annotation = None };
                       +{ Parameter.name = "b"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = Some !"c";
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "async def foo():\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = true;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "async def foo():\n  ..."
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = true;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
             };
        ];
    assert_parsed
      "@foo\nasync def foo():\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [!"foo"];
                   return_annotation = None;
                   async = true;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "@decorator\ndef foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators = [!"decorator"];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "@decorator(a=b, c=d)\ndef foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators =
                     [
                       +Expression.Call
                          {
                            Call.callee = !"decorator";
                            arguments =
                              [
                                { Call.Argument.name = Some ~+"a"; value = !"b" };
                                { Call.Argument.name = Some ~+"c"; value = !"d" };
                              ];
                          };
                     ];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "@foo\n\n@bar\ndef foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators = [!"foo"; !"bar"];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "@x[0].y\ndef foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators =
                     [
                       +Expression.Name
                          (Name.Attribute
                             {
                               Name.Attribute.base =
                                 +Expression.Call
                                    {
                                      Call.callee =
                                        +Expression.Name
                                           (Name.Attribute
                                              {
                                                Name.Attribute.base = !"x";
                                                attribute = "__getitem__";
                                                special = true;
                                              });
                                      arguments =
                                        [
                                          {
                                            Call.Argument.name = None;
                                            value = +Expression.Constant (Constant.Integer 0);
                                          };
                                        ];
                                    };
                               attribute = "y";
                               special = false;
                             });
                     ];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "@(x<y)\ndef foo(a):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                   decorators =
                     [
                       +Expression.ComparisonOperator
                          {
                            ComparisonOperator.left = !"x";
                            operator = ComparisonOperator.LessThan;
                            right = !"y";
                          };
                     ];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a, b):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   Define.Signature.name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "a"; value = None; annotation = None };
                       +{ Parameter.name = "b"; value = None; annotation = None };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a, b = 1):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "a"; value = None; annotation = None };
                       +{
                          Parameter.name = "b";
                          value = Some (+Expression.Constant (Constant.Integer 1));
                          annotation = None;
                        };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a=()):\n  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{
                          Parameter.name = "a";
                          value = Some (+Expression.Tuple []);
                          annotation = None;
                        };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(): 1; 2"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Expression (+Expression.Constant (Constant.Integer 1));
                   +Statement.Expression (+Expression.Constant (Constant.Integer 2));
                 ];
             };
        ];
    assert_parsed
      "def foo():\n  1\n  2\n3"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Expression (+Expression.Constant (Constant.Integer 1));
                   +Statement.Expression (+Expression.Constant (Constant.Integer 2));
                 ];
             };
          +Statement.Expression (+Expression.Constant (Constant.Integer 3));
        ];
    assert_parsed
      "def foo():\n  def bar():\n    1\n    2\n3"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            return_annotation = None;
                            async = false;
                            generator = false;
                            parent = None;
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body =
                          [
                            +Statement.Expression (+Expression.Constant (Constant.Integer 1));
                            +Statement.Expression (+Expression.Constant (Constant.Integer 2));
                          ];
                      };
                 ];
             };
          +Statement.Expression (+Expression.Constant (Constant.Integer 3));
        ];
    assert_parsed
      "def foo(a: int):  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = Some !"int" }];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a: int = 1):  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{
                          Parameter.name = "a";
                          value = Some (+Expression.Constant (Constant.Integer 1));
                          annotation = Some !"int";
                        };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      "def foo(a: int, b: str):  1"
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "a"; value = None; annotation = Some !"int" };
                       +{ Parameter.name = "b"; value = None; annotation = Some !"str" };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
        def foo():
          # type: () -> str
          return 4
      |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = Some !"str";
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Return
                      {
                        Return.expression = Some (+Expression.Constant (Constant.Integer 4));
                        is_implicit = false;
                      };
                 ];
             };
        ];
    assert_parsed
      (trim_extra_indentation {|
        def foo():  # type: () -> str
          return 4
      |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [];
                   decorators = [];
                   return_annotation = Some !"str";
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Return
                      {
                        Return.expression = Some (+Expression.Constant (Constant.Integer 4));
                        is_implicit = false;
                      };
                 ];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
        def foo(a):
          # type: (str) -> str
          return 4
      |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters = [+{ Parameter.name = "a"; value = None; annotation = Some !"str" }];
                   decorators = [];
                   return_annotation = Some !"str";
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Return
                      {
                        Return.expression = Some (+Expression.Constant (Constant.Integer 4));
                        is_implicit = false;
                      };
                 ];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
        class A:
          def foo(self, a):
            # type: (str) -> str
            return 4
      |})
      ~expected:
        [
          +Statement.Class
             {
               name = !&"A";
               base_arguments = [];
               decorators = [];
               top_level_unbound_names = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"foo";
                            parameters =
                              [
                                +{ Parameter.name = "self"; value = None; annotation = None };
                                +{ Parameter.name = "a"; value = None; annotation = Some !"str" };
                              ];
                            decorators = [];
                            return_annotation = Some !"str";
                            async = false;
                            generator = false;
                            parent = Some !&"A";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body =
                          [
                            +Statement.Return
                               {
                                 Return.expression =
                                   Some (+Expression.Constant (Constant.Integer 4));
                                 is_implicit = false;
                               };
                          ];
                      };
                 ];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
        class A:
          def foo(self, a):
            # type: (A, str) -> str
            return 4
      |})
      ~expected:
        [
          +Statement.Class
             {
               name = !&"A";
               base_arguments = [];
               decorators = [];
               top_level_unbound_names = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"foo";
                            parameters =
                              [
                                +{ Parameter.name = "self"; value = None; annotation = Some !"A" };
                                +{ Parameter.name = "a"; value = None; annotation = Some !"str" };
                              ];
                            decorators = [];
                            return_annotation = Some !"str";
                            async = false;
                            generator = false;
                            parent = Some !&"A";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body =
                          [
                            +Statement.Return
                               {
                                 Return.expression =
                                   Some (+Expression.Constant (Constant.Integer 4));
                                 is_implicit = false;
                               };
                          ];
                      };
                 ];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
      def foo(
        a,  # type: bool
        b  # type: bool
      ):
        pass
    |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{
                          Parameter.name = "a";
                          value = None;
                          annotation =
                            Some
                              (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                        };
                       +{
                          Parameter.name = "b";
                          value = None;
                          annotation =
                            Some
                              (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                        };
                     ];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Pass];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
      async def foo(
        a,  # type: bool
        b  # type: bool
      ):  # type: (...) -> int
        pass
    |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{
                          Parameter.name = "a";
                          value = None;
                          annotation =
                            Some
                              (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                        };
                       +{
                          Parameter.name = "b";
                          value = None;
                          annotation =
                            Some
                              (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                        };
                     ];
                   decorators = [];
                   return_annotation = Some !"int";
                   async = true;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Pass];
             };
        ];
    assert_parsed
      (trim_extra_indentation
         {|
         def foo( *args, **kwargs): # type: ( *str, **str) -> str
           return 4
       |})
      ~expected:
        [
          +Statement.Define
             {
               signature =
                 {
                   name = !&"foo";
                   parameters =
                     [
                       +{ Parameter.name = "*args"; value = None; annotation = Some !"str" };
                       +{ Parameter.name = "**kwargs"; value = None; annotation = Some !"str" };
                     ];
                   decorators = [];
                   return_annotation = Some !"str";
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body =
                 [
                   +Statement.Return
                      {
                        Return.expression = Some (+Expression.Constant (Constant.Integer 4));
                        is_implicit = false;
                      };
                 ];
             };
        ];

    assert_not_parsed
      (trim_extra_indentation
         {|
        def foo(x):
          # type: (str, str) -> str
          return 4
      |});
    ()
  in
  PyreNewParser.with_context do_test


let test_class _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    assert_parsed
      "class foo: pass"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments = [];
               body = [+Statement.Pass];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "@bar\nclass foo():\n\tpass"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments = [];
               body = [+Statement.Pass];
               decorators = [!"bar"];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo():\n\tdef bar(): pass"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            return_annotation = None;
                            async = false;
                            generator = false;
                            parent = Some !&"foo";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body = [+Statement.Pass];
                      };
                 ];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo():\n\tdef bar():\n\t\tdef baz(): pass"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            return_annotation = None;
                            async = false;
                            generator = false;
                            parent = Some !&"foo";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body =
                          [
                            +Statement.Define
                               {
                                 signature =
                                   {
                                     name = !&"baz";
                                     parameters = [];
                                     decorators = [];
                                     return_annotation = None;
                                     async = false;
                                     generator = false;
                                     parent = None;
                                     nesting_define = None;
                                   };
                                 captures = [];
                                 unbound_names = [];
                                 body = [+Statement.Pass];
                               };
                          ];
                      };
                 ];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo(1, 2):\n\t1"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) };
                 ];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo(init_subclass_arg=\"literal_string\"):\n\t1"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments =
                 [
                   {
                     Call.Argument.name = Some ~+"init_subclass_arg";
                     value =
                       +Expression.Constant
                          (Constant.String (StringLiteral.create "literal_string"));
                   };
                 ];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo(1, **kwargs):\n\t1"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   {
                     Call.Argument.name = None;
                     value = +Expression.Starred (Starred.Twice !"kwargs");
                   };
                 ];
               body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class foo(superfoo):\n\tdef bar(): pass"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"foo";
               base_arguments = [{ Call.Argument.name = None; value = !"superfoo" }];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            return_annotation = None;
                            async = false;
                            generator = false;
                            parent = Some !&"foo";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body = [+Statement.Pass];
                      };
                 ];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    assert_parsed
      "class A:\n\tdef foo(): pass\n\tclass B:\n\t\tdef bar(): pass\n"
      ~expected:
        [
          +Statement.Class
             {
               Class.name = !&"A";
               base_arguments = [];
               body =
                 [
                   +Statement.Define
                      {
                        signature =
                          {
                            name = !&"foo";
                            parameters = [];
                            decorators = [];
                            return_annotation = None;
                            async = false;
                            generator = false;
                            parent = Some !&"A";
                            nesting_define = None;
                          };
                        captures = [];
                        unbound_names = [];
                        body = [+Statement.Pass];
                      };
                   +Statement.Class
                      {
                        Class.name = !&"B";
                        base_arguments = [];
                        body =
                          [
                            +Statement.Define
                               {
                                 signature =
                                   {
                                     name = !&"bar";
                                     parameters = [];
                                     decorators = [];
                                     return_annotation = None;
                                     async = false;
                                     generator = false;
                                     parent = Some !&"B";
                                     nesting_define = None;
                                   };
                                 captures = [];
                                 unbound_names = [];
                                 body = [+Statement.Pass];
                               };
                          ];
                        decorators = [];
                        top_level_unbound_names = [];
                      };
                 ];
               decorators = [];
               top_level_unbound_names = [];
             };
        ];
    ()
  in
  PyreNewParser.with_context do_test


let test_assign _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    assert_parsed
      "a = b"
      ~expected:[+Statement.Assign { Assign.target = !"a"; annotation = None; value = !"b" }];
    assert_parsed
      "a = b  # type: int"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
               value = !"b";
             };
        ];
    assert_parsed
      "a = b or c"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = None;
               value =
                 +Expression.BooleanOperator
                    { BooleanOperator.left = !"b"; operator = BooleanOperator.Or; right = !"c" };
             };
        ];
    assert_parsed
      "a: int"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = Some !"int";
               value = +Expression.Constant Constant.Ellipsis;
             };
        ];
    assert_parsed
      "a: int = 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = Some !"int";
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a.b = 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               annotation = None;
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a.b: int = 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               annotation = Some !"int";
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a.b = 1  # type: int"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a, b = 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = +Expression.Tuple [!"a"; !"b"];
               annotation = None;
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a = a().foo()"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = None;
               value =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute
                              {
                                base = +Expression.Call { callee = !"a"; arguments = [] };
                                attribute = "foo";
                                special = false;
                              });
                      arguments = [];
                    };
             };
        ];
    assert_parsed
      "a = b = 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = None;
               value = +Expression.Constant (Constant.Integer 1);
             };
          +Statement.Assign
             {
               Assign.target = !"b";
               annotation = None;
               value = +Expression.Constant (Constant.Integer 1);
             };
        ];
    assert_parsed
      "a += 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target = !"a";
               annotation = None;
               value =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute { base = !"a"; attribute = "__iadd__"; special = true });
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                        ];
                    };
             };
        ];
    assert_parsed
      "a.b += 1"
      ~expected:
        [
          +Statement.Assign
             {
               Assign.target =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               annotation = None;
               value =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute
                              {
                                base =
                                  +Expression.Name
                                     (Name.Attribute
                                        { base = !"a"; attribute = "b"; special = false });
                                attribute = "__iadd__";
                                special = true;
                              });
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                        ];
                    };
             };
        ];
    assert_parsed
      "i[j] = 3"
      ~expected:
        [
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
                   arguments =
                     [
                       { Call.Argument.name = None; value = !"j" };
                       {
                         Call.Argument.name = None;
                         value = +Expression.Constant (Constant.Integer 3);
                       };
                     ];
                 });
        ];
    assert_parsed
      "i[j]: int = 3"
      ~expected:
        [
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
                   arguments =
                     [
                       { Call.Argument.name = None; value = !"j" };
                       {
                         Call.Argument.name = None;
                         value = +Expression.Constant (Constant.Integer 3);
                       };
                     ];
                 });
        ];
    assert_parsed
      "i[j] = 3  # type: int"
      ~expected:
        [
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
                   arguments =
                     [
                       { Call.Argument.name = None; value = !"j" };
                       {
                         Call.Argument.name = None;
                         value = +Expression.Constant (Constant.Integer 3);
                       };
                     ];
                 });
        ];
    assert_parsed
      "i[j] += 3"
      ~expected:
        [
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
                   arguments =
                     [
                       { Call.Argument.name = None; value = !"j" };
                       {
                         Call.Argument.name = None;
                         value =
                           +Expression.Call
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
                                                         {
                                                           base = !"i";
                                                           attribute = "__getitem__";
                                                           special = true;
                                                         });
                                                 arguments =
                                                   [{ Call.Argument.name = None; value = !"j" }];
                                               };
                                          attribute = "__iadd__";
                                          special = true;
                                        });
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value = +Expression.Constant (Constant.Integer 3);
                                    };
                                  ];
                              };
                       };
                     ];
                 });
        ];
    assert_parsed
      "i[j][7] = 8"
      ~expected:
        [
          +Statement.Expression
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
                                            {
                                              base = !"i";
                                              attribute = "__getitem__";
                                              special = true;
                                            });
                                    arguments = [{ Call.Argument.name = None; value = !"j" }];
                                  };
                             attribute = "__setitem__";
                             special = true;
                           });
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value = +Expression.Constant (Constant.Integer 7);
                       };
                       {
                         Call.Argument.name = None;
                         value = +Expression.Constant (Constant.Integer 8);
                       };
                     ];
                 });
        ];
    assert_parsed
      "i[j::1] = i[:j]"
      ~expected:
        [
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
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
                                    { Call.Argument.name = None; value = !"j" };
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
                       {
                         Call.Argument.name = None;
                         value =
                           +Expression.Call
                              {
                                callee =
                                  +Expression.Name
                                     (Name.Attribute
                                        { base = !"i"; attribute = "__getitem__"; special = true });
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
                                                 { Call.Argument.name = None; value = !"j" };
                                                 {
                                                   Call.Argument.name = None;
                                                   value = +Expression.Constant Constant.NoneLiteral;
                                                 };
                                               ];
                                           };
                                    };
                                  ];
                              };
                       };
                     ];
                 });
        ];
    assert_parsed
      "x = i[j] = y"
      ~expected:
        [
          +Statement.Assign { target = !"x"; annotation = None; value = !"y" };
          +Statement.Expression
             (+Expression.Call
                 {
                   callee =
                     +Expression.Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true });
                   arguments =
                     [
                       { Call.Argument.name = None; value = !"j" };
                       { Call.Argument.name = None; value = !"y" };
                     ];
                 });
        ];
    (* This is to demonstrate that we get a wrong result. *)
    assert_parsed
      "x, i[j] = y"
      ~expected:
        [
          +Statement.Assign
             {
               target =
                 +Expression.Tuple
                    [
                      !"x";
                      +Expression.Call
                         {
                           callee =
                             +Expression.Name
                                (Name.Attribute
                                   { base = !"i"; attribute = "__getitem__"; special = true });
                           arguments = [{ Call.Argument.name = None; value = !"j" }];
                         };
                    ];
               annotation = None;
               value = !"y";
             };
        ];
    ()
  in
  PyreNewParser.with_context do_test


let test_match _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    let assert_case_parsed case_source ~expected_pattern ~expected_guard =
      assert_parsed
        ("match x:\n  " ^ case_source ^ ":\n    pass")
        ~expected:
          [
            +Statement.Match
               {
                 Match.subject = !"x";
                 cases =
                   [
                     {
                       Match.Case.pattern = expected_pattern;
                       guard = expected_guard;
                       body = [+Statement.Pass];
                     };
                   ];
               };
          ]
    in
    assert_case_parsed
      "case 1"
      ~expected_pattern:(+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 1)))
      ~expected_guard:None;
    assert_case_parsed
      "case 2 as y"
      ~expected_pattern:
        (+Match.Pattern.MatchAs
            {
              pattern = Some (+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 2)));
              name = "y";
            })
      ~expected_guard:None;
    assert_case_parsed
      "case 3 | 4"
      ~expected_pattern:
        (+Match.Pattern.MatchOr
            [
              +Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 3));
              +Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 4));
            ])
      ~expected_guard:None;
    assert_case_parsed
      "case None"
      ~expected_pattern:(+Match.Pattern.MatchSingleton Constant.NoneLiteral)
      ~expected_guard:None;
    assert_case_parsed
      "case [y, z, *rest]"
      ~expected_pattern:
        (+Match.Pattern.MatchSequence
            [
              +Match.Pattern.MatchAs { pattern = None; name = "y" };
              +Match.Pattern.MatchAs { pattern = None; name = "z" };
              +Match.Pattern.MatchStar (Some "rest");
            ])
      ~expected_guard:None;
    assert_case_parsed
      "case Foo(5, y=6)"
      ~expected_pattern:
        (+Match.Pattern.MatchClass
            {
              class_name = +Name.Identifier "Foo";
              patterns = [+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 5))];
              keyword_attributes = ["y"];
              keyword_patterns =
                [+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 6))];
            })
      ~expected_guard:None;
    assert_case_parsed
      "case {7: y, 8: z, **rest}"
      ~expected_pattern:
        (+Match.Pattern.MatchMapping
            {
              keys =
                [
                  +Expression.Constant (Constant.Integer 7);
                  +Expression.Constant (Constant.Integer 8);
                ];
              patterns =
                [
                  +Match.Pattern.MatchAs { pattern = None; name = "y" };
                  +Match.Pattern.MatchAs { pattern = None; name = "z" };
                ];
              rest = Some "rest";
            })
      ~expected_guard:None;
    assert_case_parsed
      "case _ if True"
      ~expected_pattern:(+Match.Pattern.MatchWildcard)
      ~expected_guard:(Some (+Expression.Constant Constant.True));
    assert_not_parsed "match x:\n  case 1 as _:\n    pass";
    assert_not_parsed "match x:\n  case y | z:\n    pass";
    assert_not_parsed "match x:\n  case (1 as y) | (2 as z):\n    pass";
    assert_not_parsed "match x:\n  case [1, *_, 5, *_, 10]:\n    pass";
    assert_not_parsed "match x:\n  case x:\n    pass\n  case x:\n    pass";
    assert_not_parsed "match x:\n  case _:\n    pass\n  case 42:\n    pass";
    ()
  in
  PyreNewParser.with_context do_test


let () =
  "parse_statements"
  >::: [
         "pass_break_continue" >:: test_pass_break_continue;
         "global_nonlocal" >:: test_global_nonlocal;
         "expression_return_raise" >:: test_expression_return_raise;
         "assert_delete" >:: test_assert_delete;
         "import" >:: test_import;
         "for_while_if" >:: test_for_while_if;
         "try" >:: test_try;
         "with" >:: test_with;
         "assign" >:: test_assign;
         "define" >:: test_define;
         "class" >:: test_class;
         "match" >:: test_match;
       ]
  |> Test.run

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
open Ast.Statement

let statements_location_insensitive_equal left right =
  List.compare Statement.location_insensitive_compare left right |> Int.equal 0


let statements_print_to_sexp statements =
  Sexp.to_string_hum ((List.sexp_of_t Statement.sexp_of_t) statements)


let assert_parsed ~context ~expected text =
  match PyreNewParser.parse_module ~context text with
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
      "for a in b:\n\tc\nelse:\n\td\n"
      ~expected:
        [
          +Statement.For
             {
               For.target = !"a";
               iterator = !"b";
               body = [+Statement.Expression !"c"];
               orelse = [+Statement.Expression !"d"];
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
      "while a:\n\tb\nelse:\n\tc\n"
      ~expected:
        [
          +Statement.While
             {
               While.test = !"a";
               body = [+Statement.Expression !"b"];
               orelse = [+Statement.Expression !"c"];
             };
        ];
    assert_parsed
      "if a: b\n"
      ~expected:[+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
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
      "if a:\n\tb\nelse:\n\tc\n"
      ~expected:
        [
          +Statement.If
             {
               If.test = !"a";
               body = [+Statement.Expression !"b"];
               orelse = [+Statement.Expression !"c"];
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

    assert_not_parsed "try: a";
    assert_not_parsed "try:\n\ta\nelse:\n\tb";
    assert_not_parsed "try:\n\ta\nexcept a, b:\n\tb";
    ()
  in
  PyreNewParser.with_context do_test


let test_with _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    (* let assert_not_parsed = assert_not_parsed ~context in *)
    assert_parsed
      "with a: b\n"
      ~expected:
        [
          +Statement.With
             { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = false };
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
                            parent = None;
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
                            parent = None;
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
         "define" >:: test_define;
         "class" >:: test_class;
       ]
  |> Test.run

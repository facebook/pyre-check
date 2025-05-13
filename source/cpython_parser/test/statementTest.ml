(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Test
open Ast
open Ast.Expression
open Ast.Statement

let statements_location_insensitive_equal left right =
  List.compare Statement.location_insensitive_compare left right |> Int.equal 0


let statements_print_to_sexp statements =
  Base.Sexp.to_string_hum ((Base.List.sexp_of_t Statement.sexp_of_t) statements)


let assert_parsed ~context ~expected text _ounit_context =
  match PyreCPythonParser.parse_module ~context ~enable_type_comment:true text with
  | Result.Error { PyreCPythonParser.Error.message; _ } ->
      let message = Stdlib.Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual ->
      assert_equal
        ~cmp:statements_location_insensitive_equal
        ~printer:statements_print_to_sexp
        expected
        actual


let assert_not_parsed ~context text _ounit_context =
  match PyreCPythonParser.parse_expression ~context text with
  | Result.Ok actual ->
      let message =
        Stdlib.Format.asprintf
          "Unexpected parsing success: %a"
          Base.Sexp.pp_hum
          (Expression.sexp_of_t actual)
      in
      assert_failure message
  | Result.Error _ -> ()


let test_pass_break_continue =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_parsed "pass" ~expected:[+Statement.Pass];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "break" ~expected:[+Statement.Break];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "continue" ~expected:[+Statement.Continue];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "pass\npass" ~expected:[+Statement.Pass; +Statement.Pass];
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "pass\n  pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "break\ncontinue\npass\n"
             ~expected:[+Statement.Break; +Statement.Continue; +Statement.Pass];
      ]
  in
  PyreCPythonParser.with_context do_test


let test_global_nonlocal =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "global a" ~expected:[+Statement.Global ["a"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "global a, b" ~expected:[+Statement.Global ["a"; "b"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "nonlocal a" ~expected:[+Statement.Nonlocal ["a"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "nonlocal a, b" ~expected:[+Statement.Nonlocal ["a"; "b"]];
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "global";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "nonlocal";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_expression_return_raise =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "a" ~expected:[+Statement.Expression !"a"];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a is b"
             ~expected:
               [
                 +Statement.Expression
                    (+Expression.ComparisonOperator
                        {
                          ComparisonOperator.left = !"a";
                          operator = ComparisonOperator.Is;
                          right = !"b";
                          origin = None;
                        });
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "foo(x)"
             ~expected:
               [
                 +Statement.Expression
                    (+Expression.Call
                        {
                          Call.callee = !"foo";
                          arguments = [{ Call.Argument.name = None; value = !"x" }];
                          origin = None;
                        });
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "return"
             ~expected:[+Statement.Return { Return.expression = None; is_implicit = false }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "return a"
             ~expected:[+Statement.Return { Return.expression = Some !"a"; is_implicit = false }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "raise"
             ~expected:[+Statement.Raise { Raise.expression = None; from = None }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "raise a"
             ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = None }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "raise a from b"
             ~expected:[+Statement.Raise { Raise.expression = Some !"a"; from = Some !"b" }];
      ]
  in
  PyreCPythonParser.with_context do_test


let test_assert_delete =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "assert a"
             ~expected:[+Statement.Assert { Assert.test = !"a"; message = None; origin = None }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "assert a, b"
             ~expected:
               [+Statement.Assert { Assert.test = !"a"; message = Some !"b"; origin = None }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "assert (a, b)"
             ~expected:
               [
                 +Statement.Assert
                    { Assert.test = +Expression.Tuple [!"a"; !"b"]; message = None; origin = None };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                             origin = None;
                           };
                      message =
                        Some
                          (+Expression.Constant (Constant.String (StringLiteral.create "b or c")));
                      origin = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "del ()" ~expected:[+Statement.Delete [+Expression.Tuple []]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "del a" ~expected:[+Statement.Delete [!"a"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "del (a)" ~expected:[+Statement.Delete [!"a"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "del a, b" ~expected:[+Statement.Delete [!"a"; !"b"]];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed "del (a, b)" ~expected:[+Statement.Delete [+Expression.Tuple [!"a"; !"b"]]];
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "assert";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "del";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_import =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "import a"
             ~expected:
               [
                 +Statement.Import
                    { Import.from = None; imports = [+{ Import.name = !&"a"; alias = None }] };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "import a.b"
             ~expected:
               [
                 +Statement.Import
                    { Import.from = None; imports = [+{ Import.name = !&"a.b"; alias = None }] };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "import a as b"
             ~expected:
               [
                 +Statement.Import
                    { Import.from = None; imports = [+{ Import.name = !&"a"; alias = Some "b" }] };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from a import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"a");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from a import *"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"a");
                      imports = [+{ Import.name = !&"*"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from . import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&".");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from ...foo import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"...foo");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from .....foo import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&".....foo");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from .a import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&".a");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from ..a import b"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"..a");
                      imports = [+{ Import.name = !&"b"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from a import (b, c)"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"a");
                      imports =
                        [
                          +{ Import.name = !&"b"; alias = None };
                          +{ Import.name = !&"c"; alias = None };
                        ];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "from a.b import c"
             ~expected:
               [
                 +Statement.Import
                    {
                      Import.from = Some (Node.create_with_default_location !&"a.b");
                      imports = [+{ Import.name = !&"c"; alias = None }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "import";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "import .";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "import a.async";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "from import foo";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_for_while_if =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "for a.b in c: d\n"
             ~expected:
               [
                 +Statement.For
                    {
                      For.target =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      iterator = !"c";
                      body = [+Statement.Expression !"d"];
                      orelse = [];
                      async = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "for a in *b: \n\tc"
             ~expected:
               [
                 +Statement.For
                    {
                      For.target = !"a";
                      iterator = +Expression.Starred (Starred.Once !"b");
                      body = [+Statement.Expression !"c"];
                      orelse = [];
                      async = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "for a in *b, *c: \n\td"
             ~expected:
               [
                 +Statement.For
                    {
                      For.target = !"a";
                      iterator =
                        +Expression.Tuple
                           [
                             +Expression.Starred (Starred.Once !"b");
                             +Expression.Starred (Starred.Once !"c");
                           ];
                      body = [+Statement.Expression !"d"];
                      orelse = [];
                      async = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "while a: b\n"
             ~expected:
               [
                 +Statement.While
                    { While.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "if a: b\n"
             ~expected:
               [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "if a: b\nelif c: d"
             ~expected:
               [
                 +Statement.If
                    {
                      If.test = !"a";
                      body = [+Statement.Expression !"b"];
                      orelse =
                        [
                          +Statement.If
                             { If.test = !"c"; body = [+Statement.Expression !"d"]; orelse = [] };
                        ];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "if a:\n\n\tb\n"
             ~expected:
               [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "for";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "for a in b";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "while a";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "while a:";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "if a";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "if a:";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_try =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nfinally:\n\tb"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers = [];
                      orelse = [];
                      finally = [+Statement.Expression !"b"];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nexcept:\n\tb"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers =
                        [
                          {
                            Try.Handler.kind = None;
                            name = None;
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nexcept a:\n\tb"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers =
                        [
                          {
                            Try.Handler.kind = Some !"a";
                            name = None;
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nexcept* a:\n\tb"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers =
                        [
                          {
                            Try.Handler.kind = Some !"a";
                            name = None;
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = true;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                            name = Some (+"b");
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                                      origin = None;
                                    });
                            name = None;
                            body = [+Statement.Expression !"c"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                                      origin = None;
                                    });
                            name = Some (+"e");
                            body = [+Statement.Expression !"c"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                            name = Some (+"c");
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [];
                      finally = [];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                            name = Some (+"b");
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
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nexcept* a as b:\n\tb\nexcept* d:\n\te"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers =
                        [
                          {
                            Try.Handler.kind = Some !"a";
                            name = Some (+"b");
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
                      handles_exception_group = true;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
             ~expected:
               [
                 +Statement.Try
                    {
                      Try.body = [+Statement.Expression !"a"];
                      handlers =
                        [
                          {
                            Try.Handler.kind = None;
                            name = None;
                            body = [+Statement.Expression !"b"];
                          };
                        ];
                      orelse = [+Statement.Expression !"c"];
                      finally = [+Statement.Expression !"d"];
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      handles_exception_group = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "try: a";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "try:\n\ta\nelse:\n\tb";
        labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "try:\n\ta\nexcept a, b:\n\tb";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "try:\n\ta\nexcept a:\n\tb\nexcept* c:\n\td";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "try:\n\ta\nexcept* a:\n\tb\nexcept c:\n\td";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_with =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "with a: b\n"
             ~expected:
               [
                 +Statement.With
                    {
                      With.items = [!"a", None];
                      body = [+Statement.Expression !"b"];
                      async = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "async with a: b\n"
             ~expected:
               [
                 +Statement.With
                    { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = true };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "with a as b: b\n"
             ~expected:
               [
                 +Statement.With
                    {
                      With.items = [!"a", Some !"b"];
                      body = [+Statement.Expression !"b"];
                      async = false;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
      ]
  in
  PyreCPythonParser.with_context do_test


let test_define =
  let parent = NestingContext.create_toplevel () in
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo[T, T2: str, *Ts, **P](a: T) -> T:\n  return a"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{
                                 Parameter.name = "a";
                                 value = None;
                                 annotation = Some (+Expression.Name (Name.Identifier "T"));
                               };
                            ];
                          decorators = [];
                          return_annotation = Some (+Expression.Name (Name.Identifier "T"));
                          async = false;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params =
                            [
                              +TypeParam.TypeVar { name = "T"; bound = None };
                              +TypeParam.TypeVar
                                 {
                                   name = "T2";
                                   bound = Some (+Expression.Name (Name.Identifier "str"));
                                 };
                              +TypeParam.TypeVarTuple "Ts";
                              +TypeParam.ParamSpec "P";
                            ];
                        };
                      captures = [];
                      unbound_names = [];
                      body =
                        [+Statement.Return { Return.is_implicit = false; expression = Some !"a" }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "async def foo[T, T2: str, *Ts, **P](a: T) -> T:\n  return a"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{
                                 Parameter.name = "a";
                                 value = None;
                                 annotation = Some (+Expression.Name (Name.Identifier "T"));
                               };
                            ];
                          decorators = [];
                          return_annotation = Some (+Expression.Name (Name.Identifier "T"));
                          async = true;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params =
                            [
                              +TypeParam.TypeVar { name = "T"; bound = None };
                              +TypeParam.TypeVar
                                 {
                                   name = "T2";
                                   bound = Some (+Expression.Name (Name.Identifier "str"));
                                 };
                              +TypeParam.TypeVarTuple "Ts";
                              +TypeParam.ParamSpec "P";
                            ];
                        };
                      captures = [];
                      unbound_names = [];
                      body =
                        [+Statement.Return { Return.is_implicit = false; expression = Some !"a" }];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(*, a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{ Parameter.name = "*"; value = None; annotation = None };
                              +{ Parameter.name = "a"; value = None; annotation = None };
                            ];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a, /, b):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
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
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(**a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [+{ Parameter.name = "**a"; value = None; annotation = None }];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a, b,) -> c:\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{ Parameter.name = "a"; value = None; annotation = None };
                              +{ Parameter.name = "b"; value = None; annotation = None };
                            ];
                          decorators = [];
                          return_annotation = Some !"c";
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "async def foo():\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = None;
                          async = true;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "async def foo():\n  ..."
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = None;
                          async = true;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@foo\nasync def foo():\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [!"foo"];
                          return_annotation = None;
                          async = true;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@decorator\ndef foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                          decorators = [!"decorator"];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@decorator(a=b, c=d)\ndef foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
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
                                   origin = None;
                                 };
                            ];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@foo\n\n@bar\ndef foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                          decorators = [!"foo"; !"bar"];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@x[0].y\ndef foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                          decorators =
                            [
                              +Expression.Name
                                 (Name.Attribute
                                    {
                                      Name.Attribute.base =
                                        +Expression.Subscript
                                           {
                                             Subscript.base = !"x";
                                             index = +Expression.Constant (Constant.Integer 0);
                                             origin = None;
                                           };
                                      attribute = "y";
                                      origin = None;
                                    });
                            ];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@(x<y)\ndef foo(a):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                          decorators =
                            [
                              +Expression.ComparisonOperator
                                 {
                                   ComparisonOperator.left = !"x";
                                   operator = ComparisonOperator.LessThan;
                                   right = !"y";
                                   origin = None;
                                 };
                            ];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a, b):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
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
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a, b = 1):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
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
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a=()):\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
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
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(): 1; 2"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo():\n  1\n  2\n3"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo():\n  def bar():\n    1\n    2\n3"
             ~expected:
               (let parent = NestingContext.create_toplevel () in
                [
                  +Statement.Define
                     {
                       Define.signature =
                         {
                           Define.Signature.name = !&"foo";
                           parameters = [];
                           decorators = [];
                           return_annotation = None;
                           async = false;
                           generator = false;
                           parent;
                           legacy_parent = None;
                           type_params = [];
                         };
                       captures = [];
                       unbound_names = [];
                       body =
                         (let parent = NestingContext.create_function ~parent "foo" in
                          [
                            +Statement.Define
                               {
                                 Define.signature =
                                   {
                                     Define.Signature.name = !&"bar";
                                     parameters = [];
                                     decorators = [];
                                     return_annotation = None;
                                     async = false;
                                     generator = false;
                                     parent;
                                     legacy_parent = None;
                                     type_params = [];
                                   };
                                 captures = [];
                                 unbound_names = [];
                                 body =
                                   [
                                     +Statement.Expression
                                        (+Expression.Constant (Constant.Integer 1));
                                     +Statement.Expression
                                        (+Expression.Constant (Constant.Integer 2));
                                   ];
                               };
                          ]);
                     };
                  +Statement.Expression (+Expression.Constant (Constant.Integer 3));
                ]);
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a: int):  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [+{ Parameter.name = "a"; value = None; annotation = Some !"int" }];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a: int = 1):  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
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
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo(a: int, b: str):  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{ Parameter.name = "a"; value = None; annotation = Some !"int" };
                              +{ Parameter.name = "b"; value = None; annotation = Some !"str" };
                            ];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = Some !"str";
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             (trim_extra_indentation
                {|
        def foo():  # type: () -> str
          return 4
      |})
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = Some !"str";
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [+{ Parameter.name = "a"; value = None; annotation = Some !"str" }];
                          decorators = [];
                          return_annotation = Some !"str";
                          async = false;
                          generator = false;
                          parent = NestingContext.create_toplevel ();
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             (trim_extra_indentation
                {|
        class A:
          def foo(self, a):
            # type: (str) -> str
            return 4
      |})
             ~expected:
               [
                 (let parent = NestingContext.create_toplevel () in
                  +Statement.Class
                     {
                       Class.name = !&"A";
                       base_arguments = [];
                       decorators = [];
                       top_level_unbound_names = [];
                       type_params = [];
                       parent;
                       body =
                         (let parent = NestingContext.create_class ~parent "A" in
                          [
                            +Statement.Define
                               {
                                 Define.signature =
                                   {
                                     Define.Signature.name = !&"foo";
                                     parameters =
                                       [
                                         +{
                                            Parameter.name = "self";
                                            value = None;
                                            annotation = None;
                                          };
                                         +{
                                            Parameter.name = "a";
                                            value = None;
                                            annotation = Some !"str";
                                          };
                                       ];
                                     decorators = [];
                                     return_annotation = Some !"str";
                                     async = false;
                                     generator = false;
                                     parent;
                                     legacy_parent = Some !&"A";
                                     type_params = [];
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
                          ]);
                     });
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             (trim_extra_indentation
                {|
        class A:
          def foo(self, a):
            # type: (A, str) -> str
            return 4
      |})
             ~expected:
               [
                 (let parent = NestingContext.create_toplevel () in
                  +Statement.Class
                     {
                       Class.name = !&"A";
                       base_arguments = [];
                       decorators = [];
                       top_level_unbound_names = [];
                       type_params = [];
                       parent;
                       body =
                         (let parent = NestingContext.create_class ~parent "A" in
                          [
                            +Statement.Define
                               {
                                 Define.signature =
                                   {
                                     Define.Signature.name = !&"foo";
                                     parameters =
                                       [
                                         +{
                                            Parameter.name = "self";
                                            value = None;
                                            annotation = Some !"A";
                                          };
                                         +{
                                            Parameter.name = "a";
                                            value = None;
                                            annotation = Some !"str";
                                          };
                                       ];
                                     decorators = [];
                                     return_annotation = Some !"str";
                                     async = false;
                                     generator = false;
                                     parent;
                                     legacy_parent = Some !&"A";
                                     type_params = [];
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
                          ]);
                     });
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{
                                 Parameter.name = "a";
                                 value = None;
                                 annotation =
                                   Some
                                     (+Expression.Constant
                                         (Constant.String (StringLiteral.create "bool")));
                               };
                              +{
                                 Parameter.name = "b";
                                 value = None;
                                 annotation =
                                   Some
                                     (+Expression.Constant
                                         (Constant.String (StringLiteral.create "bool")));
                               };
                            ];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Pass];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{
                                 Parameter.name = "a";
                                 value = None;
                                 annotation =
                                   Some
                                     (+Expression.Constant
                                         (Constant.String (StringLiteral.create "bool")));
                               };
                              +{
                                 Parameter.name = "b";
                                 value = None;
                                 annotation =
                                   Some
                                     (+Expression.Constant
                                         (Constant.String (StringLiteral.create "bool")));
                               };
                            ];
                          decorators = [];
                          return_annotation = Some !"int";
                          async = true;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Pass];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             (trim_extra_indentation
                {|
         def foo( *args, **kwargs): # type: ( *str, **str) -> str
           return 4
       |})
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters =
                            [
                              +{ Parameter.name = "*args"; value = None; annotation = Some !"str" };
                              +{
                                 Parameter.name = "**kwargs";
                                 value = None;
                                 annotation = Some !"str";
                               };
                            ];
                          decorators = [];
                          return_annotation = Some !"str";
                          async = false;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params = [];
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo() -> Tuple[*Ts]:\n  1"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation =
                            Some
                              (+Expression.Subscript
                                  {
                                    Subscript.base = !"Tuple";
                                    index =
                                      +Expression.Tuple [+Expression.Starred (Starred.Once !"Ts")];
                                    origin = None;
                                  });
                          async = false;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed
             (trim_extra_indentation
                {|
        def foo(x):
          # type: (str, str) -> str
          return 4
      |});
      ]
  in
  PyreCPythonParser.with_context do_test


let test_class =
  let parent = NestingContext.create_toplevel () in
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo[T, T2: str, *Ts, **P]:\n\tdef bar(x: T) -> T: return x"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [];
                      parent;
                      body =
                        (let parent = NestingContext.create_class ~parent "foo" in
                         [
                           +Statement.Define
                              {
                                Define.signature =
                                  {
                                    Define.Signature.name = !&"bar";
                                    parameters =
                                      [
                                        +{
                                           Parameter.name = "x";
                                           value = None;
                                           annotation =
                                             Some (+Expression.Name (Name.Identifier "T"));
                                         };
                                      ];
                                    decorators = [];
                                    return_annotation =
                                      Some (+Expression.Name (Name.Identifier "T"));
                                    async = false;
                                    generator = false;
                                    parent;
                                    legacy_parent = Some !&"foo";
                                    type_params = [];
                                  };
                                captures = [];
                                unbound_names = [];
                                body =
                                  [
                                    +Statement.Return
                                       { Return.is_implicit = false; expression = Some !"x" };
                                  ];
                              };
                         ]);
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params =
                        [
                          +TypeParam.TypeVar { name = "T"; bound = None };
                          +TypeParam.TypeVar
                             {
                               name = "T2";
                               bound = Some (+Expression.Name (Name.Identifier "str"));
                             };
                          +TypeParam.TypeVarTuple "Ts";
                          +TypeParam.ParamSpec "P";
                        ];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo: pass"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [];
                      parent;
                      body = [+Statement.Pass];
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "@bar\nclass foo():\n\tpass"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [];
                      parent;
                      body = [+Statement.Pass];
                      decorators = [!"bar"];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo():\n\tdef bar(): pass"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [];
                      parent;
                      body =
                        (let parent = NestingContext.create_class ~parent "foo" in
                         [
                           +Statement.Define
                              {
                                Define.signature =
                                  {
                                    Define.Signature.name = !&"bar";
                                    parameters = [];
                                    decorators = [];
                                    return_annotation = None;
                                    async = false;
                                    generator = false;
                                    parent;
                                    legacy_parent = Some !&"foo";
                                    type_params = [];
                                  };
                                captures = [];
                                unbound_names = [];
                                body = [+Statement.Pass];
                              };
                         ]);
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo():\n\tdef bar():\n\t\tdef baz(): pass"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [];
                      parent;
                      body =
                        (let parent = NestingContext.create_class ~parent "foo" in
                         [
                           +Statement.Define
                              {
                                Define.signature =
                                  {
                                    Define.Signature.name = !&"bar";
                                    parameters = [];
                                    decorators = [];
                                    return_annotation = None;
                                    async = false;
                                    generator = false;
                                    parent;
                                    legacy_parent = Some !&"foo";
                                    type_params = [];
                                  };
                                captures = [];
                                unbound_names = [];
                                body =
                                  (let parent = NestingContext.create_function ~parent "bar" in
                                   [
                                     +Statement.Define
                                        {
                                          Define.signature =
                                            {
                                              Define.Signature.name = !&"baz";
                                              parameters = [];
                                              decorators = [];
                                              return_annotation = None;
                                              async = false;
                                              generator = false;
                                              parent;
                                              legacy_parent = None;
                                              type_params = [];
                                            };
                                          captures = [];
                                          unbound_names = [];
                                          body = [+Statement.Pass];
                                        };
                                   ]);
                              };
                         ]);
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo(1, 2):\n\t1"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 2);
                          };
                        ];
                      parent;
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
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
                      parent;
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo(1, **kwargs):\n\t1"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          {
                            Call.Argument.name = None;
                            value = +Expression.Starred (Starred.Twice !"kwargs");
                          };
                        ];
                      parent;
                      body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class foo(superfoo):\n\tdef bar(): pass"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"foo";
                      base_arguments = [{ Call.Argument.name = None; value = !"superfoo" }];
                      parent;
                      body =
                        [
                          +Statement.Define
                             {
                               Define.signature =
                                 {
                                   Define.Signature.name = !&"bar";
                                   parameters = [];
                                   decorators = [];
                                   return_annotation = None;
                                   async = false;
                                   generator = false;
                                   parent = NestingContext.create_class ~parent "foo";
                                   legacy_parent = Some !&"foo";
                                   type_params = [];
                                 };
                               captures = [];
                               unbound_names = [];
                               body = [+Statement.Pass];
                             };
                        ];
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "class A:\n\tdef foo(): pass\n\tclass B:\n\t\tdef bar(): pass\n"
             ~expected:
               [
                 +Statement.Class
                    {
                      Class.name = !&"A";
                      base_arguments = [];
                      parent;
                      body =
                        (let parent = NestingContext.create_class ~parent "A" in
                         [
                           +Statement.Define
                              {
                                Define.signature =
                                  {
                                    Define.Signature.name = !&"foo";
                                    parameters = [];
                                    decorators = [];
                                    return_annotation = None;
                                    async = false;
                                    generator = false;
                                    parent;
                                    legacy_parent = Some !&"A";
                                    type_params = [];
                                  };
                                captures = [];
                                unbound_names = [];
                                body = [+Statement.Pass];
                              };
                           +Statement.Class
                              {
                                Class.name = !&"B";
                                base_arguments = [];
                                parent;
                                body =
                                  [
                                    (let parent = NestingContext.create_class ~parent "B" in
                                     +Statement.Define
                                        {
                                          Define.signature =
                                            {
                                              Define.Signature.name = !&"bar";
                                              parameters = [];
                                              decorators = [];
                                              return_annotation = None;
                                              async = false;
                                              generator = false;
                                              parent;
                                              legacy_parent = Some !&"B";
                                              type_params = [];
                                            };
                                          captures = [];
                                          unbound_names = [];
                                          body = [+Statement.Pass];
                                        });
                                  ];
                                decorators = [];
                                top_level_unbound_names = [];
                                type_params = [];
                              };
                         ]);
                      decorators = [];
                      top_level_unbound_names = [];
                      type_params = [];
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "def foo():\n\tclass bar:\n\t\tpass"
             ~expected:
               [
                 +Statement.Define
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"foo";
                          parameters = [];
                          decorators = [];
                          return_annotation = None;
                          async = false;
                          generator = false;
                          parent;
                          legacy_parent = None;
                          type_params = [];
                        };
                      captures = [];
                      unbound_names = [];
                      body =
                        [
                          +Statement.Class
                             {
                               Class.name = !&"bar";
                               base_arguments = [];
                               parent = NestingContext.create_function ~parent "foo";
                               body = [+Statement.Pass];
                               decorators = [];
                               top_level_unbound_names = [];
                               type_params = [];
                             };
                        ];
                    };
               ];
      ]
  in
  PyreCPythonParser.with_context do_test


let test_assign =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a = b"
             ~expected:
               [+Statement.Assign { Assign.target = !"a"; annotation = None; value = Some !"b" }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a = b  # type: int"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = !"a";
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                      value = Some !"b";
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a = b or c"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = !"a";
                      annotation = None;
                      value =
                        Some
                          (+Expression.BooleanOperator
                              {
                                BooleanOperator.left = !"b";
                                operator = BooleanOperator.Or;
                                right = !"c";
                                origin = None;
                              });
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a: int"
             ~expected:
               [+Statement.Assign { Assign.target = !"a"; annotation = Some !"int"; value = None }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a: int = 1"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = !"a";
                      annotation = Some !"int";
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a.b = 1"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      annotation = None;
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a.b: int = 1"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      annotation = Some !"int";
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a.b = 1  # type: int"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a, b = 1"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = +Expression.Tuple [!"a"; !"b"];
                      annotation = None;
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a = a().foo()"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = !"a";
                      annotation = None;
                      value =
                        Some
                          (+Expression.Call
                              {
                                Call.callee =
                                  +Expression.Name
                                     (Name.Attribute
                                        {
                                          Name.Attribute.base =
                                            +Expression.Call
                                               { Call.callee = !"a"; arguments = []; origin = None };
                                          attribute = "foo";
                                          origin = None;
                                        });
                                arguments = [];
                                origin = None;
                              });
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a = b = 1"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target = !"a";
                      annotation = None;
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
                 +Statement.Assign
                    {
                      Assign.target = !"b";
                      annotation = None;
                      value = Some (+Expression.Constant (Constant.Integer 1));
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a += 1"
             ~expected:
               [
                 +Statement.AugmentedAssign
                    {
                      AugmentedAssign.target = !"a";
                      operator = BinaryOperator.Add;
                      value = +Expression.Constant (Constant.Integer 1);
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "a.b += 1"
             ~expected:
               [
                 +Statement.AugmentedAssign
                    {
                      AugmentedAssign.target =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      operator = BinaryOperator.Add;
                      value = +Expression.Constant (Constant.Integer 1);
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j] = 3"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                      value = Some (+Expression.Constant (Constant.Integer 3));
                      annotation = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j]: int = 3"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                      value = Some (+Expression.Constant (Constant.Integer 3));
                      annotation = Some !"int";
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j] = 3  # type: int"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                      value = Some (+Expression.Constant (Constant.Integer 3));
                      annotation = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j] += 3"
             ~expected:
               [
                 +Statement.AugmentedAssign
                    {
                      AugmentedAssign.target =
                        +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                      operator = BinaryOperator.Add;
                      value = +Expression.Constant (Constant.Integer 3);
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j][7] = 8"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript
                           {
                             Subscript.base =
                               +Expression.Subscript
                                  { Subscript.base = !"i"; index = !"j"; origin = None };
                             index = +Expression.Constant (Constant.Integer 7);
                             origin = None;
                           };
                      value = Some (+Expression.Constant (Constant.Integer 8));
                      annotation = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "i[j::1] = i[:j]"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript
                           {
                             Subscript.base = !"i";
                             index =
                               +Expression.Slice
                                  {
                                    Slice.start = Some !"j";
                                    stop = None;
                                    step = Some (+Expression.Constant (Constant.Integer 1));
                                    origin = None;
                                  };
                             origin = None;
                           };
                      value =
                        Some
                          (+Expression.Subscript
                              {
                                Subscript.base = !"i";
                                index =
                                  +Expression.Slice
                                     {
                                       Slice.start = None;
                                       stop = Some !"j";
                                       step = None;
                                       origin = None;
                                     };
                                origin = None;
                              });
                      annotation = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "x = i[j] = y"
             ~expected:
               [
                 +Statement.Assign { Assign.target = !"x"; annotation = None; value = Some !"y" };
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                      value = Some !"y";
                      annotation = None;
                    };
               ];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "x, i[j] = y"
             ~expected:
               [
                 +Statement.Assign
                    {
                      Assign.target =
                        +Expression.Tuple
                           [
                             !"x";
                             +Expression.Subscript
                                { Subscript.base = !"i"; index = !"j"; origin = None };
                           ];
                      annotation = None;
                      value = Some !"y";
                    };
               ];
      ]
  in
  PyreCPythonParser.with_context do_test


let test_match =
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
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case 1"
             ~expected_pattern:
               (+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 1)))
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case 2 as y"
             ~expected_pattern:
               (+Match.Pattern.MatchAs
                   {
                     pattern =
                       Some (+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 2)));
                     name = "y";
                   })
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case 3 | 4"
             ~expected_pattern:
               (+Match.Pattern.MatchOr
                   [
                     +Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 3));
                     +Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 4));
                   ])
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case None"
             ~expected_pattern:(+Match.Pattern.MatchSingleton Constant.NoneLiteral)
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case [y, z, *rest]"
             ~expected_pattern:
               (+Match.Pattern.MatchSequence
                   [
                     +Match.Pattern.MatchAs { pattern = None; name = "y" };
                     +Match.Pattern.MatchAs { pattern = None; name = "z" };
                     +Match.Pattern.MatchStar (Some "rest");
                   ])
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case [y, z, *_]"
             ~expected_pattern:
               (+Match.Pattern.MatchSequence
                   [
                     +Match.Pattern.MatchAs { pattern = None; name = "y" };
                     +Match.Pattern.MatchAs { pattern = None; name = "z" };
                     +Match.Pattern.MatchStar None;
                   ])
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case Foo(5, y=6)"
             ~expected_pattern:
               (+Match.Pattern.MatchClass
                   {
                     class_name = +Name.Identifier "Foo";
                     patterns =
                       [+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 5))];
                     keyword_attributes = ["y"];
                     keyword_patterns =
                       [+Match.Pattern.MatchValue (+Expression.Constant (Constant.Integer 6))];
                   })
             ~expected_guard:None;
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
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
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_case_parsed
             "case _ if True"
             ~expected_pattern:(+Match.Pattern.MatchWildcard)
             ~expected_guard:(Some (+Expression.Constant Constant.True));
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case 1 as _:\n    pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case y | z:\n    pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case (1 as y) | (2 as z):\n    pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case [1, *_, 5, *_, 10]:\n    pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case x:\n    pass\n  case x:\n    pass";
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_not_parsed "match x:\n  case _:\n    pass\n  case 42:\n    pass";
      ]
  in
  PyreCPythonParser.with_context do_test


let test_type_alias =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    test_list
      [
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "type A = B"
             ~expected:
               [+Statement.TypeAlias { TypeAlias.name = !"A"; type_params = []; value = !"B" }];
        labeled_test_case __FUNCTION__ __LINE__
        @@ assert_parsed
             "type A[T, T2: str, *Ts, **P] = B"
             ~expected:
               [
                 +Statement.TypeAlias
                    {
                      TypeAlias.name = !"A";
                      type_params =
                        [
                          +TypeParam.TypeVar { name = "T"; bound = None };
                          +TypeParam.TypeVar
                             {
                               name = "T2";
                               bound = Some (+Expression.Name (Name.Identifier "str"));
                             };
                          +TypeParam.TypeVarTuple "Ts";
                          +TypeParam.ParamSpec "P";
                        ];
                      value = !"B";
                    };
               ];
      ]
  in
  PyreCPythonParser.with_context do_test


let () =
  "parse_statements"
  >::: [
         test_pass_break_continue;
         test_global_nonlocal;
         test_expression_return_raise;
         test_assert_delete;
         test_import;
         test_for_while_if;
         test_try;
         test_with;
         test_assign;
         test_define;
         test_class;
         test_match;
         test_type_alias;
       ]
  |> Test.run

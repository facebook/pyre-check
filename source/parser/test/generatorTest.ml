(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Expression
open Statement
open Test

let parse_untrimmed source =
  let open PyreParser in
  match Parser.parse (String.split source ~on:'\n') with
  | Result.Ok statements -> Source.create statements
  | Result.Error { Parser.Error.location = { Location.start = { Location.line; column }; _ }; _ } ->
      let error =
        Format.asprintf
          "Could not parse test source at line %d, column %d. Test input:\n%s"
          line
          column
          source
      in
      failwith error


let assert_parsed_equal source statements =
  let parsed_source = parse_untrimmed source in
  let found_any =
    Visit.collect_locations parsed_source |> List.for_all ~f:(Location.equal Location.any)
  in
  if found_any then
    Printf.printf "\nLocation.any\n  found in parse of %s\n" source;
  assert_false found_any;
  assert_source_equal ~location_insensitive:true (Source.create statements) parsed_source


let assert_not_parsed source =
  let open PyreParser in
  match Parser.parse (String.split source ~on:'\n') with
  | Result.Error _ -> ()
  | Result.Ok statements ->
      let error =
        Format.asprintf
          "Unexpected parsing success for `%s`:\n%a"
          source
          Sexp.pp_hum
          (List.sexp_of_t Statement.sexp_of_t statements)
      in
      assert_failure error


let test_lexer _ =
  assert_parsed_equal
    "1 # comment"
    [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal
    "# comment\n1"
    [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal
    "if a:\n\tb # comment\n"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a:\n\tb\n\n#comment\nelse:\n\tc\n"
    [
      +Statement.If
         {
           If.test = !"a";
           body = [+Statement.Expression !"b"];
           orelse = [+Statement.Expression !"c"];
         };
    ];
  assert_parsed_equal
    "if a:\n\tb\n# comment"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a: #comment\n\tb"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a:\n#comment\n\tb"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a:\n\t\t#comment\n\tb"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a:\n\tb\n\n #comment\n #comment\n\n\tc"
    [
      +Statement.If
         {
           If.test = !"a";
           body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
           orelse = [];
         };
    ];
  assert_parsed_equal "print a" [+Statement.Expression !"a"];
  assert_parsed_equal
    "a.print == 1"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "print"; special = false });
               operator = ComparisonOperator.Equals;
               right = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "print (a, file=b)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"print";
               arguments =
                 [
                   { Call.Argument.name = None; value = !"a" };
                   { Call.Argument.name = Some ~+"file"; value = !"b" };
                 ];
             });
    ];
  assert_parsed_equal "print >> a, b" [+Statement.Expression (+Expression.Tuple [!"a"; !"b"])];
  assert_parsed_equal
    "def foo():\n\tprint >> a"
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
           body = [+Statement.Expression !"a"];
         };
    ];
  assert_parsed_equal
    "1 +\\\n 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "1 + \\\n 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "(1 +\n 2)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "(1 +\n 2)\n3"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
      +Statement.Expression (+Expression.Constant (Constant.Integer 3));
    ]


let test_number _ =
  assert_parsed_equal "1" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "0" [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
  assert_parsed_equal "00" [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
  assert_parsed_equal "00_0" [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
  assert_parsed_equal "01" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "1_01" [+Statement.Expression (+Expression.Constant (Constant.Integer 101))];
  assert_parsed_equal "(1)" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "((1))" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "1;" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "1.0" [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
  assert_parsed_equal
    "1_0.1_01"
    [+Statement.Expression (+Expression.Constant (Constant.Float 10.101))];
  assert_parsed_equal ".1" [+Statement.Expression (+Expression.Constant (Constant.Float 0.1))];
  assert_parsed_equal "1." [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
  assert_parsed_equal "1e10" [+Statement.Expression (+Expression.Constant (Constant.Float 1e10))];
  assert_parsed_equal "0x1" [+Statement.Expression (+Expression.Constant (Constant.Integer 0x1))];
  assert_parsed_equal
    "0XaBc"
    [+Statement.Expression (+Expression.Constant (Constant.Integer 0xABC))];
  assert_parsed_equal "0o13" [+Statement.Expression (+Expression.Constant (Constant.Integer 0o13))];
  assert_parsed_equal "0b01" [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
  assert_parsed_equal "0b0_1" [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
  assert_parsed_equal
    "0b_0_1"
    [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
  assert_parsed_equal "0.1j" [+Statement.Expression (+Expression.Constant (Constant.Complex 0.1))];
  assert_parsed_equal "1e10j" [+Statement.Expression (+Expression.Constant (Constant.Complex 1e10))];
  assert_parsed_equal
    "1e1_0j"
    [+Statement.Expression (+Expression.Constant (Constant.Complex 1e10))];
  assert_parsed_equal "2j" [+Statement.Expression (+Expression.(Constant (Constant.Complex 2.0)))];
  assert_parsed_equal "2J" [+Statement.Expression (+Expression.(Constant (Constant.Complex 2.0)))];
  assert_parsed_equal "1L" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];

  assert_not_parsed "0xZ";
  assert_not_parsed "0_1";
  assert_not_parsed "0o9";
  assert_not_parsed "1a3";

  (* Overflow. *)
  assert_parsed_equal
    "0xffffffffff0000000000000000000000"
    [+Statement.Expression (+Expression.Constant (Constant.Integer Int.max_value))]


let test_await _ =
  assert_parsed_equal
    "await 1"
    [+Statement.Expression (+Expression.Await (+Expression.Constant (Constant.Integer 1)))];
  assert_parsed_equal
    "await foo() + 1"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base =
                           +Expression.Await (+Expression.Call { callee = !"foo"; arguments = [] });
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) }];
             });
    ];
  assert_parsed_equal
    "await foo() * 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base =
                           +Expression.Await (+Expression.Call { callee = !"foo"; arguments = [] });
                         attribute = "__mul__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ]


let test_name _ =
  assert_parsed_equal "a" [+Statement.Expression !"a"];
  assert_parsed_equal "$a" [+Statement.Expression !"$a"];
  assert_parsed_equal "_a" [+Statement.Expression !"_a"];
  assert_parsed_equal "_a0" [+Statement.Expression !"_a0"];
  assert_parsed_equal
    "a.b"
    [
      +Statement.Expression
         (+Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false }));
    ];
  assert_parsed_equal
    "a.async"
    [
      +Statement.Expression
         (+Expression.Name (Name.Attribute { base = !"a"; attribute = "async"; special = false }));
    ];
  assert_parsed_equal
    "1.0.b"
    [
      +Statement.Expression
         (+Expression.Name
             (Name.Attribute
                {
                  base = +Expression.Constant (Constant.Float 1.0);
                  attribute = "b";
                  special = false;
                }));
    ];
  assert_parsed_equal
    "a.b.c"
    [
      +Statement.Expression
         (+Expression.Name
             (Name.Attribute
                {
                  base =
                    +Expression.Name
                       (Name.Attribute { base = !"a"; attribute = "b"; special = false });
                  attribute = "c";
                  special = false;
                }));
    ];
  assert_parsed_equal
    "a[1]"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) }];
             });
    ];
  assert_parsed_equal
    "a.__getitem__(1)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = false });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) }];
             });
    ];
  assert_parsed_equal
    "a[1 < 2]"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "a[1].b"
    [
      +Statement.Expression
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
                         arguments =
                           [
                             {
                               Call.Argument.name = None;
                               value = +Expression.Constant (Constant.Integer 1);
                             };
                           ];
                       };
                  attribute = "b";
                  special = false;
                }));
    ];
  assert_parsed_equal
    "a[b]"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
               arguments = [{ Call.Argument.name = None; value = !"b" }];
             });
    ];
  assert_parsed_equal
    "a[:]"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "a[1:]"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "a[::2]"
    [
      +Statement.Expression
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
                                  value = +Expression.Constant (Constant.Integer 2);
                                };
                              ];
                          };
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "a[:1]"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "a[:1 if True else 2]"
    [
      +Statement.Expression
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
                                  value =
                                    +Expression.Ternary
                                       {
                                         Ternary.target = +Expression.Constant (Constant.Integer 1);
                                         test = +Expression.Constant Constant.True;
                                         alternative = +Expression.Constant (Constant.Integer 2);
                                       };
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
    ];
  assert_parsed_equal
    "a[1:1]"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "a[1,2]"
    [
      +Statement.Expression
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
                            +Expression.Constant (Constant.Integer 1);
                            +Expression.Constant (Constant.Integer 2);
                          ];
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "a[:1,2]"
    [
      +Statement.Expression
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
    ];
  assert_not_parsed "a.((2, 3))"


let test_starred _ =
  assert_parsed_equal "*a" [+Statement.Expression (+Expression.Starred (Starred.Once !"a"))];
  assert_parsed_equal "*(a)" [+Statement.Expression (+Expression.Starred (Starred.Once !"a"))];
  assert_parsed_equal "**a" [+Statement.Expression (+Expression.Starred (Starred.Twice !"a"))]


let test_compound _ =
  assert_parsed_equal
    "1.0\n2"
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
    ];
  assert_parsed_equal
    "1.0;2"
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
    ];
  assert_parsed_equal
    "\n1.0\n2\n3"
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
      +Statement.Expression (+Expression.Constant (Constant.Integer 3));
    ]


let decorator ?arguments name =
  Decorator.to_expression { Decorator.name = Node.create_with_default_location !&name; arguments }


let test_define _ =
  assert_parsed_equal
    "def foo(a):\n  1"
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
  assert_parsed_equal
    "def foo(*, a):\n  1"
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
  assert_parsed_equal
    "def foo(a, /, b):\n  1"
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
  assert_parsed_equal
    "def foo(**a):\n  1"
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
  assert_parsed_equal
    "async def foo():\n  1"
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
  assert_parsed_equal
    "async def foo():\n  ..."
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
  assert_parsed_equal
    "@foo\nasync def foo():\n  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [decorator "foo"];
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
  assert_parsed_equal
    "@decorator\ndef foo(a):\n  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [decorator "decorator"];
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
  assert_parsed_equal
    "@decorator(a=b, c=d)\ndef foo(a):\n  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators =
                 [
                   decorator
                     "decorator"
                     ~arguments:
                       [
                         { Call.Argument.name = Some ~+"a"; value = !"b" };
                         { Call.Argument.name = Some ~+"c"; value = !"d" };
                       ];
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
  assert_parsed_equal
    "@foo\n\n@bar\ndef foo(a):\n  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [decorator "foo"; decorator "bar"];
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
  assert_parsed_equal
    "@x[0].y\ndef foo(a):\n  1"
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
  assert_parsed_equal
    "@(x<y)\ndef foo(a):\n  1"
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
  assert_parsed_equal
    "def foo(a, b):\n  1"
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
  assert_parsed_equal
    "def foo(a = 1, b):\n  1"
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
                      annotation = None;
                    };
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
  assert_parsed_equal
    "def foo(a=()):\n  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [+{ Parameter.name = "a"; value = Some (+Expression.Tuple []); annotation = None }];
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
  assert_parsed_equal
    "def foo(): 1; 2"
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
  assert_parsed_equal
    "def foo():\n  1\n  2\n3"
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
  assert_parsed_equal
    "def foo():\n  def bar():\n    1\n    2\n3"
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
  assert_parsed_equal
    "def foo(a: int):  1"
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
  assert_parsed_equal
    "def foo(a: int = 1):  1"
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
  assert_parsed_equal
    "def foo(a: int, b: string):  1"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{ Parameter.name = "a"; value = None; annotation = Some !"int" };
                   +{ Parameter.name = "b"; value = None; annotation = Some !"string" };
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
  assert_parsed_equal
    "def foo(a: Tuple[int, str]):\n  1"
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
                          (+Expression.Call
                              {
                                callee =
                                  +Expression.Name
                                     (Name.Attribute
                                        {
                                          base = !"Tuple";
                                          attribute = "__getitem__";
                                          special = true;
                                        });
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value = +Expression.Tuple [!"int"; !"str"];
                                    };
                                  ];
                              });
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
  assert_parsed_equal
    "def foo(a, b,) -> c:\n  1"
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
  assert_parsed_equal
    "def foo() -> str:\n  1\n  2"
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
               +Statement.Expression (+Expression.Constant (Constant.Integer 1));
               +Statement.Expression (+Expression.Constant (Constant.Integer 2));
             ];
         };
    ];
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo():  # pyre-ignore
        # type: (...) -> int
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo():
        # type: (...) -> int
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo():
        # type: (...) -> 'int'
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo():
        # type: () -> str
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation {|
        def foo(): # type: ()-> str
          return 4
      |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation {|
        def foo(): # type:   ()-> str
          return 4
      |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
         def foo( *args): # type: ( *str) -> str
           return 4
       |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{
                      Parameter.name = "*args";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
         def foo( **kwargs): # type: ( **str) -> str
           return 4
       |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{
                      Parameter.name = "**kwargs";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
         def foo( *args, **kwargs): # type: ( *str, **str) -> str
           return 4
       |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{
                      Parameter.name = "*args";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                   +{
                      Parameter.name = "**kwargs";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(a):
        # type: (str) -> str
        return 4
    |})
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
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a): # type: (str) -> str
        return 4
    |})
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
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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

  (* Don't use string annotations if list length does not match signature *)
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a): # type: (str, str) -> str
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], str) -> str
        return 4
    |})
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
                          (+Expression.Constant
                              (Constant.String
                                 (StringLiteral.create "typing.Union[typing.List[int], str]")));
                    };
                   +{
                      Parameter.name = "b";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], typing.List[str]) -> str
        return 4
    |})
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
                          (+Expression.Constant
                              (Constant.String
                                 (StringLiteral.create "typing.Union[typing.List[int], str]")));
                    };
                   +{
                      Parameter.name = "b";
                      value = None;
                      annotation =
                        Some
                          (+Expression.Constant
                              (Constant.String (StringLiteral.create "typing.List[str]")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(self, a, b): # type: (typing.Union[typing.List[int], str], typing.List[str]) -> str
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{ Parameter.name = "self"; value = None; annotation = None };
                   +{
                      Parameter.name = "a";
                      value = None;
                      annotation =
                        Some
                          (+Expression.Constant
                              (Constant.String
                                 (StringLiteral.create "typing.Union[typing.List[int], str]")));
                    };
                   +{
                      Parameter.name = "b";
                      value = None;
                      annotation =
                        Some
                          (+Expression.Constant
                              (Constant.String (StringLiteral.create "typing.List[str]")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(self, a, b): # type: (int) -> str
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{ Parameter.name = "self"; value = None; annotation = None };
                   +{ Parameter.name = "a"; value = None; annotation = None };
                   +{ Parameter.name = "b"; value = None; annotation = None };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo():
        # type: (...) ->List[str]
        return 4
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "List[str]")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(
        self,
        a,  # type: bool
        b,  # type: bool
      ):  # type: (...) -> int
        pass
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [
                   +{ Parameter.name = "self"; value = None; annotation = None };
                   +{
                      Parameter.name = "a";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                    };
                   +{
                      Parameter.name = "b";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(
        a,  # type: bool
        b  # type: bool
      ):
        pass
    |})
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
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                    };
                   +{
                      Parameter.name = "b";
                      value = None;
                      annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
      def foo(
        a,  # type: bool
        **kwargs
      ):
        pass
    |})
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
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "bool")));
                    };
                   +{ Parameter.name = "**kwargs"; value = None; annotation = None };
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
  assert_parsed_equal
    (trim_extra_indentation
       {|
        def foo(a): # type: (A_b.C) -> str
          return "hi"
    |})
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
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "A_b.C")));
                    };
                 ];
               decorators = [];
               return_annotation =
                 Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
                    Return.expression =
                      Some (+Expression.Constant (Constant.String (StringLiteral.create "hi")));
                    is_implicit = false;
                  };
             ];
         };
    ];
  assert_parsed_equal
    (trim_extra_indentation {|
        async def foo(a):
          yield "A"
    |})
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation = None;
               async = true;
               generator = true;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Expression
                  (+Expression.Yield
                      (Some (+Expression.Constant (Constant.String (StringLiteral.create "A")))));
             ];
         };
    ];
  ()


let test_boolean_operator _ =
  assert_parsed_equal "True" [+Statement.Expression (+Expression.Constant Constant.True)];
  assert_parsed_equal "False" [+Statement.Expression (+Expression.Constant Constant.False)];
  assert_parsed_equal
    "True and False"
    [
      +Statement.Expression
         (+Expression.BooleanOperator
             {
               BooleanOperator.left = +Expression.Constant Constant.True;
               operator = BooleanOperator.And;
               right = +Expression.Constant Constant.False;
             });
    ];
  assert_parsed_equal
    "1 and False"
    [
      +Statement.Expression
         (+Expression.BooleanOperator
             {
               BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
               operator = BooleanOperator.And;
               right = +Expression.Constant Constant.False;
             });
    ];
  assert_parsed_equal
    "True or 1"
    [
      +Statement.Expression
         (+Expression.BooleanOperator
             {
               BooleanOperator.left = +Expression.Constant Constant.True;
               operator = BooleanOperator.Or;
               right = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "1 and 2 or 3"
    [
      +Statement.Expression
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
    ]


let test_binary_operator _ =
  assert_parsed_equal
    "1 + 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "1 ^ 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__xor__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "1 // 2"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__floordiv__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) }];
             });
    ];
  assert_parsed_equal
    "1 >> 2 >> 3"
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
                                          base = +Expression.Constant (Constant.Integer 1);
                                          attribute = "__rshift__";
                                          special = true;
                                        });
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value = +Expression.Constant (Constant.Integer 2);
                                    };
                                  ];
                              };
                         attribute = "__rshift__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 3) }];
             });
    ];
  assert_parsed_equal
    "1 >> a.b"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.Integer 1);
                         attribute = "__rshift__";
                         special = true;
                       });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value =
                       +Expression.Name
                          (Name.Attribute { base = !"a"; attribute = "b"; special = false });
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "1 - 2 + 3"
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
                                          base = +Expression.Constant (Constant.Integer 1);
                                          attribute = "__sub__";
                                          special = true;
                                        });
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value = +Expression.Constant (Constant.Integer 2);
                                    };
                                  ];
                              };
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 3) }];
             });
    ];
  assert_parsed_equal
    "a + b.c"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "__add__"; special = true });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value =
                       +Expression.Name
                          (Name.Attribute { base = !"b"; attribute = "c"; special = false });
                   };
                 ];
             });
    ]


let test_unary_operator _ =
  assert_parsed_equal
    "not 1"
    [
      +Statement.Expression
         (+Expression.UnaryOperator
             {
               UnaryOperator.operator = UnaryOperator.Not;
               operand = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "~1"
    [
      +Statement.Expression
         (+Expression.UnaryOperator
             {
               UnaryOperator.operator = UnaryOperator.Invert;
               operand = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "+1"
    [
      +Statement.Expression
         (+Expression.UnaryOperator
             {
               UnaryOperator.operator = UnaryOperator.Positive;
               operand = +Expression.Constant (Constant.Integer 1);
             });
    ]


let test_lambda _ =
  assert_parsed_equal
    "lambda: 1"
    [
      +Statement.Expression
         (+Expression.Lambda
             { Lambda.parameters = []; body = +Expression.Constant (Constant.Integer 1) });
    ];
  assert_parsed_equal
    "lambda x,: x"
    [
      +Statement.Expression
         (+Expression.Lambda
             {
               Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
               body = !"x";
             });
    ];
  assert_parsed_equal
    "lambda x: x is y"
    [
      +Statement.Expression
         (+Expression.Lambda
             {
               Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
               body =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = !"x";
                      operator = ComparisonOperator.Is;
                      right = !"y";
                    };
             });
    ];
  assert_parsed_equal
    "lambda x: x"
    [
      +Statement.Expression
         (+Expression.Lambda
             {
               Lambda.parameters = [+{ Parameter.name = "x"; value = None; annotation = None }];
               body = !"x";
             });
    ];
  assert_parsed_equal
    "lambda x = 1, y: x + 1"
    [
      +Statement.Expression
         (+Expression.Lambda
             {
               Lambda.parameters =
                 [
                   +{
                      Parameter.name = "x";
                      value = Some (+Expression.Constant (Constant.Integer 1));
                      annotation = None;
                    };
                   +{ Parameter.name = "y"; value = None; annotation = None };
                 ];
               body =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute
                              {
                                base = +Expression.Name (Name.Identifier "x");
                                attribute = "__add__";
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
             });
    ];
  assert_parsed_equal
    "lambda *, x: x"
    [
      +Statement.Expression
         (+Expression.Lambda
             {
               Lambda.parameters =
                 [
                   +{ Parameter.name = "*"; value = None; annotation = None };
                   +{ Parameter.name = "x"; value = None; annotation = None };
                 ];
               body = !"x";
             });
    ]


let test_ternary _ =
  assert_parsed_equal
    "5 if 1 else 1"
    [
      +Statement.Expression
         (+Expression.Ternary
             {
               Ternary.target = +Expression.Constant (Constant.Integer 5);
               test = +Expression.Constant (Constant.Integer 1);
               alternative = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "a in b if 1 else 1"
    [
      +Statement.Expression
         (+Expression.Ternary
             {
               Ternary.target =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = !"a";
                      operator = ComparisonOperator.In;
                      right = !"b";
                    };
               test = +Expression.Constant (Constant.Integer 1);
               alternative = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "1 if 2 else 3 if 4 else 5"
    [
      +Statement.Expression
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
    ]


let test_dictionary _ =
  assert_parsed_equal
    "{}"
    [+Statement.Expression (+Expression.Dictionary { Dictionary.entries = []; keywords = [] })];
  assert_parsed_equal
    "{1: 2}"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "{1: 2,}"
    [
      +Statement.Expression
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
    ];
  assert_parsed_equal
    "{1: 2, **durp}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
               keywords = [!"durp"];
             });
    ];
  assert_parsed_equal
    "{1: 2, **durp, **hurp}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
               keywords = [!"durp"; !"hurp"];
             });
    ];
  assert_parsed_equal
    "{**[1]}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries = [];
               keywords = [+Expression.List [+Expression.Constant (Constant.Integer 1)]];
             });
    ];
  assert_parsed_equal
    "{**durp, 1: 2}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
               keywords = [!"durp"];
             });
    ];
  assert_parsed_equal
    "{1: 1 < 2,}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value =
                       +Expression.ComparisonOperator
                          {
                            ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                            operator = ComparisonOperator.LessThan;
                            right = +Expression.Constant (Constant.Integer 2);
                          };
                   };
                 ];
               keywords = [];
             });
    ];
  assert_parsed_equal
    "{1: 2, 2: 3}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 2);
                     value = +Expression.Constant (Constant.Integer 3);
                   };
                 ];
               keywords = [];
             });
    ];
  assert_parsed_equal
    "{\n\t1: 2,\n\t2: 3}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                   {
                     Dictionary.Entry.key = +Expression.Constant (Constant.Integer 2);
                     value = +Expression.Constant (Constant.Integer 3);
                   };
                 ];
               keywords = [];
             });
    ];
  assert_parsed_equal
    "{a: b for a in []}"
    [
      +Statement.Expression
         (+Expression.DictionaryComprehension
             {
               Comprehension.element = { Dictionary.Entry.key = !"a"; value = !"b" };
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a if a else a: b for a in []}"
    [
      +Statement.Expression
         (+Expression.DictionaryComprehension
             {
               Comprehension.element =
                 {
                   Dictionary.Entry.key =
                     +Expression.Ternary { Ternary.target = !"a"; test = !"a"; alternative = !"a" };
                   value = !"b";
                 };
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a if a else a: b if b else b for a in []}"
    [
      +Statement.Expression
         (+Expression.DictionaryComprehension
             {
               Comprehension.element =
                 {
                   Dictionary.Entry.key =
                     +Expression.Ternary { Ternary.target = !"a"; test = !"a"; alternative = !"a" };
                   value =
                     +Expression.Ternary { Ternary.target = !"b"; test = !"b"; alternative = !"b" };
                 };
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a.b or c: a for a in b}"
    [
      +Statement.Expression
         (+Expression.DictionaryComprehension
             {
               Comprehension.element =
                 {
                   Dictionary.Entry.key =
                     +Expression.BooleanOperator
                        {
                          BooleanOperator.left =
                            +Expression.Name
                               (Name.Attribute { base = !"a"; attribute = "b"; special = false });
                          operator = BooleanOperator.Or;
                          right = !"c";
                        };
                   value = !"a";
                 };
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
    ];
  assert_parsed_equal
    "{a: b for c, d in []}"
    [
      +Statement.Expression
         (+Expression.DictionaryComprehension
             {
               Comprehension.element = { Dictionary.Entry.key = !"a"; value = !"b" };
               generators =
                 [
                   {
                     Comprehension.Generator.target = +Expression.Tuple [!"c"; !"d"];
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a or b: 2}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key =
                       +Expression.BooleanOperator
                          {
                            BooleanOperator.left = !"a";
                            operator = BooleanOperator.Or;
                            right = !"b";
                          };
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
               keywords = [];
             });
    ];
  assert_parsed_equal
    "{a and b: 2}"
    [
      +Statement.Expression
         (+Expression.Dictionary
             {
               Dictionary.entries =
                 [
                   {
                     Dictionary.Entry.key =
                       +Expression.BooleanOperator
                          {
                            BooleanOperator.left = !"a";
                            operator = BooleanOperator.And;
                            right = !"b";
                          };
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
               keywords = [];
             });
    ];
  assert_not_parsed "{ a or lambda b: b + 1: c }"


let test_list _ =
  assert_parsed_equal "[]" [+Statement.Expression (+Expression.List [])];
  assert_parsed_equal "[[]]" [+Statement.Expression (+Expression.List [+Expression.List []])];
  assert_parsed_equal
    "[1,]"
    [+Statement.Expression (+Expression.List [+Expression.Constant (Constant.Integer 1)])];
  assert_parsed_equal
    "[1, 2]"
    [
      +Statement.Expression
         (+Expression.List
             [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    ];
  assert_parsed_equal
    "[1 if 2 else 3]"
    [
      +Statement.Expression
         (+Expression.List
             [
               +Expression.Ternary
                  {
                    Ternary.target = +Expression.Constant (Constant.Integer 1);
                    test = +Expression.Constant (Constant.Integer 2);
                    alternative = +Expression.Constant (Constant.Integer 3);
                  };
             ]);
    ];
  assert_parsed_equal
    "[\n\t1,\n\t2\n]"
    [
      +Statement.Expression
         (+Expression.List
             [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    ];
  assert_parsed_equal
    "[a for a in []]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a in b for a in []]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = !"a";
                      operator = ComparisonOperator.In;
                      right = !"b";
                    };
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a for a in a for b in []]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = !"a";
                     conditions = [];
                     async = false;
                   };
                   {
                     Comprehension.Generator.target = !"b";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a for a in [] if b]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [!"b"];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a for a in (c for c in []) if b]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator =
                       +Expression.Generator
                          {
                            Comprehension.element = !"c";
                            generators =
                              [
                                {
                                  Comprehension.Generator.target = !"c";
                                  iterator = +Expression.List [];
                                  conditions = [];
                                  async = false;
                                };
                              ];
                          };
                     conditions = [!"b"];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a for a in [] if 1 < 2]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions =
                       [
                         +Expression.ComparisonOperator
                            {
                              ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                              operator = ComparisonOperator.LessThan;
                              right = +Expression.Constant (Constant.Integer 2);
                            };
                       ];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a for a in [] if a is 1 or True]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions =
                       [
                         +Expression.BooleanOperator
                            {
                              BooleanOperator.left =
                                +Expression.ComparisonOperator
                                   {
                                     ComparisonOperator.left = !"a";
                                     operator = ComparisonOperator.Is;
                                     right = +Expression.Constant (Constant.Integer 1);
                                   };
                              operator = BooleanOperator.Or;
                              right = +Expression.Constant Constant.True;
                            };
                       ];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "[a async for a in []]"
    [
      +Statement.Expression
         (+Expression.ListComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = true;
                   };
                 ];
             });
    ]


let test_set _ =
  assert_parsed_equal
    "{1}"
    [+Statement.Expression (+Expression.Set [+Expression.Constant (Constant.Integer 1)])];
  assert_parsed_equal
    "{*[1]}"
    [
      +Statement.Expression
         (+Expression.Set
             [
               +Expression.Starred
                  (Starred.Once (+Expression.List [+Expression.Constant (Constant.Integer 1)]));
             ]);
    ];
  assert_parsed_equal
    "{1,}"
    [+Statement.Expression (+Expression.Set [+Expression.Constant (Constant.Integer 1)])];
  assert_parsed_equal
    "{1, 2}"
    [
      +Statement.Expression
         (+Expression.Set
             [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    ];
  assert_parsed_equal
    "{1, 1 if 2 else 3}"
    [
      +Statement.Expression
         (+Expression.Set
             [
               +Expression.Constant (Constant.Integer 1);
               +Expression.Ternary
                  {
                    Ternary.target = +Expression.Constant (Constant.Integer 1);
                    test = +Expression.Constant (Constant.Integer 2);
                    alternative = +Expression.Constant (Constant.Integer 3);
                  };
             ]);
    ];
  assert_parsed_equal
    "{a for a in []}"
    [
      +Statement.Expression
         (+Expression.SetComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a for a in [] if b}"
    [
      +Statement.Expression
         (+Expression.SetComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [!"b"];
                     async = false;
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "{a for a in [] if b if c}"
    [
      +Statement.Expression
         (+Expression.SetComprehension
             {
               Comprehension.element = !"a";
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [!"b"; !"c"];
                     async = false;
                   };
                 ];
             });
    ]


let test_generator _ =
  assert_parsed_equal
    "(a in b for a in [] if b)"
    [
      +Statement.Expression
         (+Expression.Generator
             {
               Comprehension.element =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = !"a";
                      operator = ComparisonOperator.In;
                      right = !"b";
                    };
               generators =
                 [
                   {
                     Comprehension.Generator.target = !"a";
                     iterator = +Expression.List [];
                     conditions = [!"b"];
                     async = false;
                   };
                 ];
             });
    ]


let test_yield _ =
  assert_parsed_equal "yield" [+Statement.Expression (+Expression.Yield None)];
  assert_parsed_equal
    "yield 1"
    [+Statement.Expression (+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))))];
  assert_parsed_equal
    "yield from a"
    [+Statement.Expression (+Expression.YieldFrom (+Expression.Name (Name.Identifier "a")))];
  assert_parsed_equal
    "yield 1, 2"
    [
      +Statement.Expression
         (+Expression.Yield
             (Some
                (+Expression.Tuple
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ])));
    ];
  assert_parsed_equal
    "x = yield 1"
    [
      +Statement.Assign
         {
           Assign.target = !"x";
           annotation = None;
           value = +Expression.Yield (Some (+Expression.Constant (Constant.Integer 1)));
         };
    ];
  assert_parsed_equal
    "x: str = yield 1"
    [
      +Statement.Assign
         {
           Assign.target = !"x";
           annotation = Some !"str";
           value = +Expression.Yield (Some (+Expression.Constant (Constant.Integer 1)));
         };
    ];
  assert_parsed_equal
    "x += yield 1"
    [
      +Statement.Assign
         {
           Assign.target = !"x";
           annotation = None;
           value =
             +Expression.Call
                {
                  callee =
                    +Expression.Name
                       (Name.Attribute { base = !"x"; attribute = "__iadd__"; special = true });
                  arguments =
                    [
                      {
                        Call.Argument.name = None;
                        value = +Expression.Yield (Some (+Expression.Constant (Constant.Integer 1)));
                      };
                    ];
                };
         };
    ]


let test_comparison _ =
  assert_parsed_equal
    "a.b < 2"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left =
                 +Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false });
               operator = ComparisonOperator.LessThan;
               right = +Expression.Constant (Constant.Integer 2);
             });
    ];
  assert_parsed_equal
    "1 in []"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
               operator = ComparisonOperator.In;
               right = +Expression.List [];
             });
    ];
  assert_parsed_equal
    "1 is 1"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
               operator = ComparisonOperator.Is;
               right = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "1 is not 1"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
               operator = ComparisonOperator.IsNot;
               right = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "foo is not_bar"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left = +Expression.Name (Name.Identifier "foo");
               operator = ComparisonOperator.Is;
               right = +Expression.Name (Name.Identifier "not_bar");
             });
    ];
  assert_parsed_equal
    "1 == 1"
    [
      +Statement.Expression
         (+Expression.ComparisonOperator
             {
               ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
               operator = ComparisonOperator.Equals;
               right = +Expression.Constant (Constant.Integer 1);
             });
    ];
  assert_parsed_equal
    "1 < 1 < 2"
    [
      +Statement.Expression
         (+Expression.BooleanOperator
             {
               BooleanOperator.left =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.LessThan;
                      right = +Expression.Constant (Constant.Integer 1);
                    };
               operator = BooleanOperator.And;
               right =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.LessThan;
                      right = +Expression.Constant (Constant.Integer 2);
                    };
             });
    ];
  assert_parsed_equal
    "1 < 1 is 2"
    [
      +Statement.Expression
         (+Expression.BooleanOperator
             {
               BooleanOperator.left =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.LessThan;
                      right = +Expression.Constant (Constant.Integer 1);
                    };
               operator = BooleanOperator.And;
               right =
                 +Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.Is;
                      right = +Expression.Constant (Constant.Integer 2);
                    };
             });
    ]


let test_call _ =
  assert_parsed_equal
    "foo()"
    [+Statement.Expression (+Expression.Call { callee = !"foo"; arguments = [] })];
  assert_parsed_equal
    "foo(a for a in [])"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value =
                       +Expression.Generator
                          {
                            Comprehension.element = !"a";
                            generators =
                              [
                                {
                                  Comprehension.Generator.target = !"a";
                                  iterator = +Expression.List [];
                                  conditions = [];
                                  async = false;
                                };
                              ];
                          };
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "foo(a for a in [],)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value =
                       +Expression.Generator
                          {
                            Comprehension.element = !"a";
                            generators =
                              [
                                {
                                  Comprehension.Generator.target = !"a";
                                  iterator = +Expression.List [];
                                  conditions = [];
                                  async = false;
                                };
                              ];
                          };
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "foo(1, 2,)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 2) };
                 ];
             });
    ];
  assert_parsed_equal
    "foo((1, 2))"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value =
                       +Expression.Tuple
                          [
                            +Expression.Constant (Constant.Integer 1);
                            +Expression.Constant (Constant.Integer 2);
                          ];
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "foo(x, 1, (a, b))"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   { Call.Argument.name = None; value = !"x" };
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   { Call.Argument.name = None; value = +Expression.Tuple [!"a"; !"b"] };
                 ];
             });
    ];
  assert_parsed_equal
    "a.foo(x)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"a"; attribute = "foo"; special = false });
               arguments = [{ Call.Argument.name = None; value = !"x" }];
             });
    ];
  assert_parsed_equal
    "foo(1, a = 1, b = 2)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   {
                     Call.Argument.name = Some ~+"a";
                     value = +Expression.Constant (Constant.Integer 1);
                   };
                   {
                     Call.Argument.name = Some ~+"b";
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "foo(1, a = 2, *args, **kwargs)"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee = !"foo";
               arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
                   {
                     Call.Argument.name = Some ~+"a";
                     value = +Expression.Constant (Constant.Integer 2);
                   };
                   { Call.Argument.name = None; value = +Expression.Starred (Starred.Once !"args") };
                   {
                     Call.Argument.name = None;
                     value = +Expression.Starred (Starred.Twice !"kwargs");
                   };
                 ];
             });
    ]


let test_string _ =
  let create_literal value = Substring.Literal (+value) in
  assert_parsed_equal
    "'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "\"foo\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "'''foo'''"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "\"\"\"foo\"\"\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "r'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "R'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "b'foo'"
    [
      +Statement.Expression
         (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
    ];
  assert_parsed_equal
    "u'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "ub'foo'"
    [
      +Statement.Expression
         (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
    ];
  assert_parsed_equal
    "bR'foo'"
    [
      +Statement.Expression
         (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
    ];
  assert_parsed_equal
    "'foo' 'bar'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foobar")))];
  assert_parsed_equal
    "ur'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "uR'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "Ur'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "UR'foo'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "foo")))];
  assert_parsed_equal
    "f'foo'"
    [+Statement.Expression (+Expression.FormatString [create_literal "foo"])];
  assert_parsed_equal
    "F'foo'"
    [+Statement.Expression (+Expression.FormatString [create_literal "foo"])];
  assert_parsed_equal
    "f'foo' f'bar'"
    [+Statement.Expression (+Expression.FormatString [create_literal "foo"; create_literal "bar"])];
  assert_parsed_equal
    "f'foo' 'bar'"
    [+Statement.Expression (+Expression.FormatString [create_literal "foo"; create_literal "bar"])];
  assert_parsed_equal
    "'foo' f'bar'"
    [+Statement.Expression (+Expression.FormatString [create_literal "foo"; create_literal "bar"])];
  assert_parsed_equal
    "\"'\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "'")))];
  assert_parsed_equal
    "'\"'"
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "\"")))];
  assert_parsed_equal
    "\"\\\"\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "\\\"")))];
  assert_parsed_equal
    "\"\\'\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "\\\'")))];
  assert_parsed_equal
    "\"\"\"\nfoo\"\"\""
    [+Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "\nfoo")))];
  assert_parsed_equal
    "\"f.o\\no\""
    [
      +Statement.Expression (+Expression.Constant (Constant.String (StringLiteral.create "f.o\\no")));
    ];
  assert_parsed_equal
    "'a' + 'b'"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "\"a\" + \"b\""
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "'''a''' + '''b'''"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "\"\"\"a\"\"\" + \"\"\"b\"\"\""
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute
                       {
                         base = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                         attribute = "__add__";
                         special = true;
                       });
               arguments =
                 [
                   {
                     Call.Argument.name = None;
                     value = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                   };
                 ];
             });
    ]


let test_class _ =
  assert_parsed_equal
    "@bar\nclass foo():\n\tpass"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body = [+Statement.Pass];
           decorators = [decorator "bar"];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo: pass"
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
  assert_parsed_equal
    "class foo():\n\tdef bar(): pass"
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
  assert_parsed_equal
    "class foo():\n\tdef bar():\n\t\tdef baz(): pass"
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
  assert_parsed_equal
    "class foo.bar: pass"
    [
      +Statement.Class
         {
           Class.name = !&"foo.bar";
           base_arguments = [];
           body = [+Statement.Pass];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo(1, 2):\n\t1"
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
  assert_parsed_equal
    "class foo(init_subclass_arg=\"literal_string\"):\n\t1"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments =
             [
               {
                 Call.Argument.name = Some ~+"init_subclass_arg";
                 value =
                   +Expression.Constant (Constant.String (StringLiteral.create "literal_string"));
               };
             ];
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo(1, **kwargs):\n\t1"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments =
             [
               { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) };
               { Call.Argument.name = None; value = +Expression.Starred (Starred.Twice !"kwargs") };
             ];
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo:\n\tattribute: int = 1"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"attribute";
                    annotation = Some !"int";
                    value = +Expression.Constant (Constant.Integer 1);
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo:\n\tattribute = 1 # type: int"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"attribute";
                    annotation =
                      Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                    value = +Expression.Constant (Constant.Integer 1);
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo:\n\tattribute: int"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"attribute";
                    annotation = Some !"int";
                    value = +Expression.Constant Constant.Ellipsis;
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo(superfoo):\n\tdef bar(): pass"
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
  assert_parsed_equal
    "class foo():\n\tdef __init__(self): self.bar = 0"
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
                        name = !&"__init__";
                        parameters = [+{ Parameter.name = "self"; value = None; annotation = None }];
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
                        +Statement.Assign
                           {
                             Assign.target =
                               +Expression.Name
                                  (Name.Attribute
                                     { base = !"self"; attribute = "bar"; special = false });
                             annotation = None;
                             value = +Expression.Constant (Constant.Integer 0);
                           };
                      ];
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];

  (* We mark parents for functions under if statements. *)
  assert_parsed_equal
    (trim_extra_indentation
       {|
      class foo():
        if True:
          def bar():
            pass
    |})
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant Constant.True;
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
                    orelse = [];
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ]


let test_return _ =
  assert_parsed_equal "return" [+Statement.Return { Return.expression = None; is_implicit = false }];
  assert_parsed_equal
    "return 1"
    [
      +Statement.Return
         {
           Return.expression = Some (+Expression.Constant (Constant.Integer 1));
           is_implicit = false;
         };
    ]


let test_delete _ =
  assert_parsed_equal "del a" [+Statement.Delete [!"a"]];
  assert_parsed_equal "del a, b" [+Statement.Delete [!"a"; !"b"]];
  assert_parsed_equal "del (a, b)" [+Statement.Delete [+Expression.Tuple [!"a"; !"b"]]]


let test_assign _ =
  assert_parsed_equal
    "a = b"
    [+Statement.Assign { Assign.target = !"a"; annotation = None; value = !"b" }];
  assert_parsed_equal
    "a = 1"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = None;
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a: int = 1"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some !"int";
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a = 1  # type: int"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a = 1  # type: 'int'"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a: int"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some !"int";
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a # type: int"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a = 1  # type: ignore"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = None;
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a, b = 1"
    [
      +Statement.Assign
         {
           Assign.target = +Expression.Tuple [!"a"; !"b"];
           annotation = None;
           value = +Expression.Constant (Constant.Integer 1);
         };
    ];
  assert_parsed_equal
    "a = a().foo()"
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
  assert_parsed_equal
    "a = b = 1"
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
  assert_parsed_equal
    "a = yield from b"
    [
      +Statement.Assign
         { Assign.target = !"a"; annotation = None; value = +Expression.YieldFrom !"b" };
    ];
  assert_parsed_equal
    "a += 1"
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
  assert_parsed_equal
    "a.b += 1"
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
                                 (Name.Attribute { base = !"a"; attribute = "b"; special = false });
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
  assert_parsed_equal
    "a = b if b else c"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = None;
           value = +Expression.Ternary { Ternary.target = !"b"; test = !"b"; alternative = !"c" };
         };
    ];
  assert_parsed_equal
    "a = b or c"
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
  assert_parsed_equal
    "a = b or c or d"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = None;
           value =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left = !"b";
                  operator = BooleanOperator.Or;
                  right =
                    +Expression.BooleanOperator
                       { BooleanOperator.left = !"c"; operator = BooleanOperator.Or; right = !"d" };
                };
         };
    ]


let test_for _ =
  assert_parsed_equal
    "for a in b: c\n"
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
  assert_parsed_equal
    "for a, b in c: d\n"
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
  assert_parsed_equal
    "for a in b: break\n"
    [
      +Statement.For
         {
           For.target = !"a";
           iterator = !"b";
           body = [+Statement.Break];
           orelse = [];
           async = false;
         };
    ];
  assert_parsed_equal
    "for a in b: continue\n"
    [
      +Statement.For
         {
           For.target = !"a";
           iterator = !"b";
           body = [+Statement.Continue];
           orelse = [];
           async = false;
         };
    ];
  assert_parsed_equal
    "async for a in b: c\n"
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
  assert_parsed_equal
    "for a in b:\n\tc\n"
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
  assert_parsed_equal
    "for a in b:\n\tc\nelse:\n\td\n"
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
  assert_parsed_equal
    "for a in b: # type: int\n\ta"
    [
      +Statement.For
         {
           For.target = !"a";
           iterator = !"b";
           body = [+Statement.Expression !"a"];
           orelse = [];
           async = false;
         };
    ];
  assert_parsed_equal
    "for a, *b in c: \n\ta"
    [
      +Statement.For
         {
           For.target = +Expression.Tuple [!"a"; +Expression.Starred (Starred.Once !"b")];
           iterator = !"c";
           body = [+Statement.Expression !"a"];
           orelse = [];
           async = false;
         };
    ]


let test_while _ =
  assert_parsed_equal
    "while a: b\n"
    [+Statement.While { While.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "while a:\n\tb\nelse:\n\tc\n"
    [
      +Statement.While
         {
           While.test = !"a";
           body = [+Statement.Expression !"b"];
           orelse = [+Statement.Expression !"c"];
         };
    ]


let test_if _ =
  assert_parsed_equal
    "if a: b\n"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a: b\nelif c: d"
    [
      +Statement.If
         {
           If.test = !"a";
           body = [+Statement.Expression !"b"];
           orelse =
             [+Statement.If { If.test = !"c"; body = [+Statement.Expression !"d"]; orelse = [] }];
         };
    ];
  assert_parsed_equal
    "if a:\n\n\tb\n"
    [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
  assert_parsed_equal
    "if a:\n\tb\n\n\tc"
    [
      +Statement.If
         {
           If.test = !"a";
           body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if a:\n\tb\nelse:\n\tc\n"
    [
      +Statement.If
         {
           If.test = !"a";
           body = [+Statement.Expression !"b"];
           orelse = [+Statement.Expression !"c"];
         };
    ];
  assert_parsed_equal
    "if isinstance(x, int) and x > 0:\n\tb"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left =
                    +Expression.Call
                       {
                         callee = !"isinstance";
                         arguments =
                           [
                             { Call.Argument.name = None; value = !"x" };
                             { Call.Argument.name = None; value = !"int" };
                           ];
                       };
                  operator = BooleanOperator.And;
                  right =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"x";
                         operator = ComparisonOperator.GreaterThan;
                         right = +Expression.Constant (Constant.Integer 0);
                       };
                };
           body = [+Statement.Expression !"b"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if x and foo(x) > 0:\n\tb"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left = !"x";
                  operator = BooleanOperator.And;
                  right =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left =
                           +Expression.Call
                              {
                                callee = !"foo";
                                arguments = [{ Call.Argument.name = None; value = !"x" }];
                              };
                         operator = ComparisonOperator.GreaterThan;
                         right = +Expression.Constant (Constant.Integer 0);
                       };
                };
           body = [+Statement.Expression !"b"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if a is 1 or True:\n\tb"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"a";
                         operator = ComparisonOperator.Is;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                  operator = BooleanOperator.Or;
                  right = +Expression.Constant Constant.True;
                };
           body = [+Statement.Expression !"b"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if a is 1 or b is 1:\n\tc"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"a";
                         operator = ComparisonOperator.Is;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                  operator = BooleanOperator.Or;
                  right =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"b";
                         operator = ComparisonOperator.Is;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                };
           body = [+Statement.Expression !"c"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if a is 1 or b == 1:\n\tc"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"a";
                         operator = ComparisonOperator.Is;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                  operator = BooleanOperator.Or;
                  right =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"b";
                         operator = ComparisonOperator.Equals;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                };
           body = [+Statement.Expression !"c"];
           orelse = [];
         };
    ];
  assert_parsed_equal
    "if a is 1 or b is 1 or c is 1:\n\td"
    [
      +Statement.If
         {
           If.test =
             +Expression.BooleanOperator
                {
                  BooleanOperator.left =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"a";
                         operator = ComparisonOperator.Is;
                         right = +Expression.Constant (Constant.Integer 1);
                       };
                  operator = BooleanOperator.Or;
                  right =
                    +Expression.BooleanOperator
                       {
                         BooleanOperator.left =
                           +Expression.ComparisonOperator
                              {
                                ComparisonOperator.left = !"b";
                                operator = ComparisonOperator.Is;
                                right = +Expression.Constant (Constant.Integer 1);
                              };
                         operator = BooleanOperator.Or;
                         right =
                           +Expression.ComparisonOperator
                              {
                                ComparisonOperator.left = !"c";
                                operator = ComparisonOperator.Is;
                                right = +Expression.Constant (Constant.Integer 1);
                              };
                       };
                };
           body = [+Statement.Expression !"d"];
           orelse = [];
         };
    ]


let test_with _ =
  assert_parsed_equal
    "with a: b\n"
    [
      +Statement.With
         { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = false };
    ];
  assert_parsed_equal
    "with (yield from a): b\n"
    [
      +Statement.With
         {
           With.items = [+Expression.YieldFrom !"a", None];
           body = [+Statement.Expression !"b"];
           async = false;
         };
    ];
  assert_parsed_equal
    "async with a: b\n"
    [
      +Statement.With
         { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = true };
    ];
  assert_parsed_equal
    "with a as b: b\n"
    [
      +Statement.With
         { With.items = [!"a", Some !"b"]; body = [+Statement.Expression !"b"]; async = false };
    ];
  assert_parsed_equal
    "with a as b, c as d: b\n"
    [
      +Statement.With
         {
           With.items = [!"a", Some !"b"; !"c", Some !"d"];
           body = [+Statement.Expression !"b"];
           async = false;
         };
    ];
  assert_parsed_equal
    "with a, c as d: b\n"
    [
      +Statement.With
         {
           With.items = [!"a", None; !"c", Some !"d"];
           body = [+Statement.Expression !"b"];
           async = false;
         };
    ];
  assert_parsed_equal
    "with a as b: # type: int\n\tb\n"
    [
      +Statement.With
         { With.items = [!"a", Some !"b"]; body = [+Statement.Expression !"b"]; async = false };
    ]


let test_raise _ =
  assert_parsed_equal "raise" [+Statement.Raise { Raise.expression = None; from = None }];
  assert_parsed_equal "raise a" [+Statement.Raise { Raise.expression = Some !"a"; from = None }];
  assert_parsed_equal
    "raise a from b"
    [+Statement.Raise { Raise.expression = Some !"a"; from = Some !"b" }]


let test_try _ =
  assert_parsed_equal
    "try: a"
    [
      +Statement.Try
         { Try.body = [+Statement.Expression !"a"]; handlers = []; orelse = []; finally = [] };
    ];
  assert_parsed_equal
    "try:\n\ta\nelse:\n\tb"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers = [];
           orelse = [+Statement.Expression !"b"];
           finally = [];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nfinally:\n\tb"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers = [];
           orelse = [];
           finally = [+Statement.Expression !"b"];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept:\n\tb"
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
  assert_parsed_equal
    "try:\n\ta\nexcept a:\n\tb"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers =
             [{ Try.Handler.kind = Some !"a"; name = None; body = [+Statement.Expression !"b"] }];
           orelse = [];
           finally = [];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a as b:\n\tb"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers =
             [
               { Try.Handler.kind = Some !"a"; name = Some "b"; body = [+Statement.Expression !"b"] };
             ];
           orelse = [];
           finally = [];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a or b:\n\tc"
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
  assert_parsed_equal
    "try:\n\ta\nexcept a or b as e:\n\tc"
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
  assert_parsed_equal
    "try:\n\ta\nexcept a, b:\n\tb"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers =
             [
               { Try.Handler.kind = Some !"a"; name = Some "b"; body = [+Statement.Expression !"b"] };
             ];
           orelse = [];
           finally = [];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept (a, b) as c:\n\tb"
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
  assert_parsed_equal
    "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers =
             [
               { Try.Handler.kind = Some !"a"; name = Some "b"; body = [+Statement.Expression !"b"] };
               { Try.Handler.kind = Some !"d"; name = None; body = [+Statement.Expression !"e"] };
             ];
           orelse = [];
           finally = [];
         };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
    [
      +Statement.Try
         {
           Try.body = [+Statement.Expression !"a"];
           handlers =
             [{ Try.Handler.kind = None; name = None; body = [+Statement.Expression !"b"] }];
           orelse = [+Statement.Expression !"c"];
           finally = [+Statement.Expression !"d"];
         };
    ]


let test_assert _ =
  assert_parsed_equal
    "assert a"
    [+Statement.Assert { Assert.test = !"a"; message = None; origin = Assert.Origin.Assertion }];
  assert_parsed_equal
    "assert a is b"
    [
      +Statement.Assert
         {
           Assert.test =
             +Expression.ComparisonOperator
                { ComparisonOperator.left = !"a"; operator = ComparisonOperator.Is; right = !"b" };
           message = None;
           origin = Assert.Origin.Assertion;
         };
    ];
  assert_parsed_equal
    "assert a, b"
    [
      +Statement.Assert { Assert.test = !"a"; message = Some !"b"; origin = Assert.Origin.Assertion };
    ];
  assert_parsed_equal
    "assert a is not None, 'b or c'"
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
    ]


let test_import _ =
  assert_parsed_equal
    "import a"
    [+Statement.Import { Import.from = None; imports = [+{ Import.name = !&"a"; alias = None }] }];
  assert_parsed_equal
    "import async"
    [
      +Statement.Import
         { Import.from = None; imports = [+{ Import.name = !&"async"; alias = None }] };
    ];
  assert_parsed_equal
    "import a.async"
    [
      +Statement.Import
         { Import.from = None; imports = [+{ Import.name = !&"a.async"; alias = None }] };
    ];
  assert_parsed_equal
    "import a.b"
    [+Statement.Import { Import.from = None; imports = [+{ Import.name = !&"a.b"; alias = None }] }];
  assert_parsed_equal
    "import a as b"
    [
      +Statement.Import
         { Import.from = None; imports = [+{ Import.name = !&"a"; alias = Some "b" }] };
    ];
  assert_parsed_equal
    "import a as b, c, d as e"
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
  assert_parsed_equal
    "from a import b"
    [
      +Statement.Import
         { Import.from = Some !&"a"; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from a import *"
    [
      +Statement.Import
         { Import.from = Some !&"a"; imports = [+{ Import.name = !&"*"; alias = None }] };
    ];
  assert_parsed_equal
    "from . import b"
    [
      +Statement.Import
         { Import.from = Some !&"."; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from ...foo import b"
    [
      +Statement.Import
         { Import.from = Some !&"...foo"; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from .....foo import b"
    [
      +Statement.Import
         { Import.from = Some !&".....foo"; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from .a import b"
    [
      +Statement.Import
         { Import.from = Some !&".a"; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from ..a import b"
    [
      +Statement.Import
         { Import.from = Some !&"..a"; imports = [+{ Import.name = !&"b"; alias = None }] };
    ];
  assert_parsed_equal
    "from a import (b, c)"
    [
      +Statement.Import
         {
           Import.from = Some !&"a";
           imports =
             [+{ Import.name = !&"b"; alias = None }; +{ Import.name = !&"c"; alias = None }];
         };
    ];
  assert_parsed_equal
    "from a.b import c"
    [
      +Statement.Import
         { Import.from = Some !&"a.b"; imports = [+{ Import.name = !&"c"; alias = None }] };
    ];
  assert_parsed_equal
    "from f import a as b, c, d as e"
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
  assert_not_parsed "from import x"


let test_global _ =
  assert_parsed_equal "global a" [+Statement.Global ["a"]];
  assert_parsed_equal "global a, b" [+Statement.Global ["a"; "b"]]


let test_tuple _ =
  assert_parsed_equal "1" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
  assert_parsed_equal "()" [+Statement.Expression (+Expression.Tuple [])];
  assert_parsed_equal
    "(1,)"
    [+Statement.Expression (+Expression.Tuple [+Expression.Constant (Constant.Integer 1)])];
  assert_parsed_equal
    "1, 2"
    [
      +Statement.Expression
         (+Expression.Tuple
             [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    ];
  assert_parsed_equal
    "1, 1 + 1"
    [
      +Statement.Expression
         (+Expression.Tuple
             [
               +Expression.Constant (Constant.Integer 1);
               +Expression.Call
                  {
                    callee =
                      +Expression.Name
                         (Name.Attribute
                            {
                              base = +Expression.Constant (Constant.Integer 1);
                              attribute = "__add__";
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
             ]);
    ];
  assert_parsed_equal
    "1, 2 if 3 else 4"
    [
      +Statement.Expression
         (+Expression.Tuple
             [
               +Expression.Constant (Constant.Integer 1);
               +Expression.Ternary
                  {
                    Ternary.target = +Expression.Constant (Constant.Integer 2);
                    test = +Expression.Constant (Constant.Integer 3);
                    alternative = +Expression.Constant (Constant.Integer 4);
                  };
             ]);
    ];
  assert_parsed_equal
    "1 + 1, 1"
    [
      +Statement.Expression
         (+Expression.Tuple
             [
               +Expression.Call
                  {
                    callee =
                      +Expression.Name
                         (Name.Attribute
                            {
                              base = +Expression.Constant (Constant.Integer 1);
                              attribute = "__add__";
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
               +Expression.Constant (Constant.Integer 1);
             ]);
    ];
  assert_parsed_equal
    "(1, 2, 3)"
    [
      +Statement.Expression
         (+Expression.Tuple
             [
               +Expression.Constant (Constant.Integer 1);
               +Expression.Constant (Constant.Integer 2);
               +Expression.Constant (Constant.Integer 3);
             ]);
    ]


let test_stubs _ =
  assert_parsed_equal
    "a = ..."
    [
      +Statement.Assign
         { Assign.target = !"a"; annotation = None; value = +Expression.Constant Constant.Ellipsis };
    ];
  assert_parsed_equal
    "a: int = ..."
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some !"int";
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a = ... # type: int"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation = Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a = ... # type: Tuple[str]"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation =
             Some (+Expression.Constant (Constant.String (StringLiteral.create "Tuple[str]")));
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a = ... # type: Tuple[str, ...]"
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation =
             Some (+Expression.Constant (Constant.String (StringLiteral.create "Tuple[str, ...]")));
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "a: Optional[int] = ..."
    [
      +Statement.Assign
         {
           Assign.target = !"a";
           annotation =
             Some
               (+Expression.Call
                   {
                     callee =
                       +Expression.Name
                          (Name.Attribute
                             { base = !"Optional"; attribute = "__getitem__"; special = true });
                     arguments = [{ Call.Argument.name = None; value = !"int" }];
                   });
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "class A:\n\ta = ... # type: int"
    [
      +Statement.Class
         {
           Class.name = !&"A";
           base_arguments = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"a";
                    annotation =
                      Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                    value = +Expression.Constant Constant.Ellipsis;
                  };
             ];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "def foo(a): ..."
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
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
         };
    ];
  assert_parsed_equal
    "def foo(a): ... # type: ignore"
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
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
         };
    ];
  assert_parsed_equal
    "def foo(a):\n\t..."
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
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
         };
    ];
  assert_parsed_equal
    "@overload\ndef foo(a: int = ...):  ..."
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
                      value = Some (+Expression.Constant Constant.Ellipsis);
                      annotation = Some !"int";
                    };
                 ];
               decorators = [decorator "overload"];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
         };
    ];
  assert_parsed_equal
    "class foo(): ..."
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo():\n\t..."
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
           decorators = [];
           top_level_unbound_names = [];
         };
    ];
  assert_parsed_equal
    "class foo(): ... # type: ignore"
    [
      +Statement.Class
         {
           Class.name = !&"foo";
           base_arguments = [];
           body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
           decorators = [];
           top_level_unbound_names = [];
         };
    ]


let test_nonlocal _ =
  assert_parsed_equal "nonlocal a" [+Statement.Nonlocal ["a"]];
  assert_parsed_equal "nonlocal a, b" [+Statement.Nonlocal ["a"; "b"]]


let test_ellipsis _ =
  assert_parsed_equal
    "def __init__(debug = ...):\n\tpass"
    [
      +Statement.Define
         {
           signature =
             {
               name = !&"__init__";
               parameters =
                 [
                   +{
                      Parameter.name = "debug";
                      value = Some (+Expression.Constant Constant.Ellipsis);
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
           body = [+Statement.Pass];
         };
    ];
  assert_parsed_equal
    "if x is ...:\n\tpass"
    [
      +Statement.If
         {
           If.test =
             +Expression.ComparisonOperator
                {
                  ComparisonOperator.left = !"x";
                  operator = ComparisonOperator.Is;
                  right = +Expression.Constant Constant.Ellipsis;
                };
           body = [+Statement.Pass];
           orelse = [];
         };
    ]


let test_setitem _ =
  assert_parsed_equal
    "i[j] = 3"
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
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 3) };
                 ];
             });
    ];
  assert_parsed_equal
    "i[j] += 3"
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
  assert_parsed_equal
    "i[j][7] = 8"
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
                                        { base = !"i"; attribute = "__getitem__"; special = true });
                                arguments = [{ Call.Argument.name = None; value = !"j" }];
                              };
                         attribute = "__setitem__";
                         special = true;
                       });
               arguments =
                 [
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 7) };
                   { Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 8) };
                 ];
             });
    ];
  assert_parsed_equal
    "i[j::1] = i[:j]"
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
  assert_parsed_equal
    "i[j] = 5 if 1 else 1"
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
                       +Expression.Ternary
                          {
                            target = +Expression.Constant (Constant.Integer 5);
                            test = +Expression.Constant (Constant.Integer 1);
                            alternative = +Expression.Constant (Constant.Integer 1);
                          };
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "x = i[j] = y"
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
  assert_parsed_equal
    "j[i] = x = i[j] = y"
    [
      +Statement.Expression
         (+Expression.Call
             {
               callee =
                 +Expression.Name
                    (Name.Attribute { base = !"j"; attribute = "__setitem__"; special = true });
               arguments =
                 [
                   { Call.Argument.name = None; value = !"i" };
                   { Call.Argument.name = None; value = !"y" };
                 ];
             });
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
  assert_parsed_equal
    "x, i[j] = y"
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
  assert_parsed_equal
    "i[j] = x =  ... # type: Something"
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
                   { Call.Argument.name = None; value = +Expression.Constant Constant.Ellipsis };
                 ];
             });
      +Statement.Assign
         {
           target = !"x";
           annotation =
             Some (+Expression.Constant (Constant.String (StringLiteral.create "Something")));
           value = +Expression.Constant Constant.Ellipsis;
         };
    ];
  assert_parsed_equal
    "i[j] += 3,"
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
                                  value =
                                    +Expression.Tuple [+Expression.Constant (Constant.Integer 3)];
                                };
                              ];
                          };
                   };
                 ];
             });
    ];
  assert_parsed_equal
    "i[j] += yield 3"
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
                                  value =
                                    +Expression.Yield
                                       (Some (+Expression.Constant (Constant.Integer 3)));
                                };
                              ];
                          };
                   };
                 ];
             });
    ];
  ()


let test_byte_order_mark _ =
  let parse lines = PyreParser.Parser.parse_exn lines |> ignore in
  let byte_order_mark = [0xEF; 0xBB; 0xBF] |> List.map ~f:Char.of_int_exn |> String.of_char_list in
  (* Ensure that we can parse UTF-8 files with byte order marks properly. *)
  parse [byte_order_mark ^ "1"];
  assert_raises
    (PyreParser.Parser.Error "Could not parse file at $invalid_path:2:0-2:0\n  \239\187\1912\n  ^")
    (fun () -> parse ["1"; byte_order_mark ^ "2"])


let test_walrus_operator _ =
  assert_parsed_equal
    "(a := 1)"
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
    ];
  assert_parsed_equal
    (* Binds more tightly than a comma. *)
    "(a := 1, 2)"
    [
      +Statement.Expression
         (+Expression.Tuple
             [
               +Expression.WalrusOperator
                  { target = !"a"; value = +Expression.Constant (Constant.Integer 1) };
               +Expression.Constant (Constant.Integer 2);
             ]);
    ];
  assert_parsed_equal
    (* Binds less tightly than a binary operator. *)
    "(a := 1 + 2)"
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             {
               target = !"a";
               value =
                 +Expression.Call
                    {
                      callee =
                        +Expression.Name
                           (Name.Attribute
                              {
                                base = +Expression.Constant (Constant.Integer 1);
                                attribute = "__add__";
                                special = true;
                              });
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 2);
                          };
                        ];
                    };
             });
    ];
  assert_parsed_equal
    (* Binds less tightly than `and`. *)
    "(a := True and False)"
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             {
               target = !"a";
               value =
                 +Expression.BooleanOperator
                    {
                      BooleanOperator.left = +Expression.Constant Constant.True;
                      operator = BooleanOperator.And;
                      right = +Expression.Constant Constant.False;
                    };
             });
    ];
  assert_parsed_equal
    (* Binds less tightly than a conditional expression. *)
    "(a := 1 if True else 2)"
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             {
               target = !"a";
               value =
                 +Expression.Ternary
                    {
                      Ternary.target = +Expression.Constant (Constant.Integer 1);
                      test = +Expression.Constant Constant.True;
                      alternative = +Expression.Constant (Constant.Integer 2);
                    };
             });
    ];
  assert_parsed_equal
    "(a := (b := 1))"
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             {
               target = !"a";
               value =
                 +Expression.WalrusOperator
                    { target = !"b"; value = +Expression.Constant (Constant.Integer 1) };
             });
    ];
  assert_not_parsed "(a := 1) := 2"


let () =
  "parsing"
  >::: [
         "lexer" >:: test_lexer;
         "number" >:: test_number;
         "await" >:: test_await;
         "name" >:: test_name;
         "starred" >:: test_starred;
         "compound" >:: test_compound;
         "define" >:: test_define;
         "boolean_operator" >:: test_boolean_operator;
         "binary_operator" >:: test_binary_operator;
         "unary_operator" >:: test_unary_operator;
         "lambda" >:: test_lambda;
         "ternary" >:: test_ternary;
         "dictionary" >:: test_dictionary;
         "list" >:: test_list;
         "set" >:: test_set;
         "generator" >:: test_generator;
         "yield" >:: test_yield;
         "comparison" >:: test_comparison;
         "call" >:: test_call;
         "string" >:: test_string;
         "class" >:: test_class;
         "return" >:: test_return;
         "delete" >:: test_delete;
         "assign" >:: test_assign;
         "for" >:: test_for;
         "while" >:: test_while;
         "if" >:: test_if;
         "with" >:: test_with;
         "raise" >:: test_raise;
         "try" >:: test_try;
         "assert" >:: test_assert;
         "import" >:: test_import;
         "global" >:: test_global;
         "tuple" >:: test_tuple;
         "stubs" >:: test_stubs;
         "nonlocal" >:: test_nonlocal;
         "ellipsis" >:: test_ellipsis;
         "setitem" >:: test_setitem;
         "byte_order_mark" >:: test_byte_order_mark;
         "walrus_operator" >:: test_walrus_operator;
       ]
  |> Test.run

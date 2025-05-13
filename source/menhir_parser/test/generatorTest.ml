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
  match PyreMenhirParser.Parser.parse (String.split source ~on:'\n') with
  | Result.Ok statements -> Source.create statements
  | Result.Error
      {
        PyreMenhirParser.Parser.Error.location = { Location.start = { Location.line; column }; _ };
        _;
      } ->
      let error =
        Format.asprintf
          "Could not parse test source at line %d, column %d. Test input:\n%s"
          line
          column
          source
      in
      failwith error


let assert_parsed_equal source statements _context =
  let parsed_source = parse_untrimmed source in
  let found_any =
    Visit.collect_locations parsed_source |> List.for_all ~f:(Location.equal Location.any)
  in
  if found_any then
    Printf.printf "\nLocation.any\n  found in parse of %s\n" source;
  assert_false found_any;
  assert_source_equal ~location_insensitive:true (Source.create statements) parsed_source


let assert_not_parsed source _context =
  match PyreMenhirParser.Parser.parse (String.split source ~on:'\n') with
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


let test_lexer =
  let parent = NestingContext.create_toplevel () in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 # comment"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "# comment\n1"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb # comment\n"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb\n\n#comment\nelse:\n\tc\n"
           [
             +Statement.If
                {
                  If.test = !"a";
                  body = [+Statement.Expression !"b"];
                  orelse = [+Statement.Expression !"c"];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb\n# comment"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a: #comment\n\tb"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n#comment\n\tb"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\t\t#comment\n\tb"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb\n\n #comment\n #comment\n\n\tc"
           [
             +Statement.If
                {
                  If.test = !"a";
                  body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.print == 1"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "print"; origin = None });
                      operator = ComparisonOperator.Equals;
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "print (a, file=b)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"print";
                      arguments =
                        [
                          { Call.Argument.name = None; value = !"a" };
                          { Call.Argument.name = Some ~+"file"; value = !"b" };
                        ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo():\n\tprint >> a"
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
                  body = [+Statement.Expression !"a"];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 +\\\n 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 + \\\n 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(1 +\n 2)"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(1 +\n 2)\n3"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
             +Statement.Expression (+Expression.Constant (Constant.Integer 3));
           ];
    ]


let test_number =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "1" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "0" [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "00"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "00_0"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "01"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1_01"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 101))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(1)"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "((1))"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1;"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1.0"
           [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1_0.1_01"
           [+Statement.Expression (+Expression.Constant (Constant.Float 10.101))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           ".1"
           [+Statement.Expression (+Expression.Constant (Constant.Float 0.1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1."
           [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1e10"
           [+Statement.Expression (+Expression.Constant (Constant.Float 1e10))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0x1"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0x1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0XaBc"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0xABC))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0o13"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0o13))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0b01"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0b0_1"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0b_0_1"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 0b01))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0.1j"
           [+Statement.Expression (+Expression.Constant (Constant.Complex 0.1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1e10j"
           [+Statement.Expression (+Expression.Constant (Constant.Complex 1e10))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1e1_0j"
           [+Statement.Expression (+Expression.Constant (Constant.Complex 1e10))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "2j"
           [+Statement.Expression (+Expression.(Constant (Constant.Complex 2.0)))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "2J"
           [+Statement.Expression (+Expression.(Constant (Constant.Complex 2.0)))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1L"
           [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "0xZ";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "0_1";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "0o9";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "1a3";
      (* Overflow. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "0xffffffffff0000000000000000000000"
           [+Statement.Expression (+Expression.Constant (Constant.Integer Int.max_value))];
    ]


let test_await =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "await 1"
           [+Statement.Expression (+Expression.Await (+Expression.Constant (Constant.Integer 1)))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "await foo() + 1"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left =
                        +Expression.Await
                           (+Expression.Call { Call.callee = !"foo"; arguments = []; origin = None });
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "await foo() * 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Mult;
                      left =
                        +Expression.Await
                           (+Expression.Call { Call.callee = !"foo"; arguments = []; origin = None });
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
    ]


let test_name =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_parsed_equal "a" [+Statement.Expression !"a"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "$a" [+Statement.Expression !"$a"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "_a" [+Statement.Expression !"_a"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "_a0" [+Statement.Expression !"_a0"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.b"
           [
             +Statement.Expression
                (+Expression.Name
                    (Name.Attribute { Name.Attribute.base = !"a"; attribute = "b"; origin = None }));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.async"
           [
             +Statement.Expression
                (+Expression.Name
                    (Name.Attribute
                       { Name.Attribute.base = !"a"; attribute = "async"; origin = None }));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1.0.b"
           [
             +Statement.Expression
                (+Expression.Name
                    (Name.Attribute
                       {
                         Name.Attribute.base = +Expression.Constant (Constant.Float 1.0);
                         attribute = "b";
                         origin = None;
                       }));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.b.c"
           [
             +Statement.Expression
                (+Expression.Name
                    (Name.Attribute
                       {
                         Name.Attribute.base =
                           +Expression.Name
                              (Name.Attribute
                                 { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                         attribute = "c";
                         origin = None;
                       }));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.__getitem__(1)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee =
                        +Expression.Name
                           (Name.Attribute
                              {
                                Name.Attribute.base = !"a";
                                attribute = "__getitem__";
                                origin = None;
                              });
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                        ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1 < 2]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.ComparisonOperator
                           {
                             ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                             operator = ComparisonOperator.LessThan;
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1].b"
           [
             +Statement.Expression
                (+Expression.Name
                    (Name.Attribute
                       {
                         Name.Attribute.base =
                           +Expression.Subscript
                              {
                                Subscript.base = !"a";
                                index = +Expression.Constant (Constant.Integer 1);
                                origin = None;
                              };
                         attribute = "b";
                         origin = None;
                       }));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[b]"
           [
             +Statement.Expression
                (+Expression.Subscript { Subscript.base = !"a"; index = !"b"; origin = None });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[:]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           { Slice.start = None; stop = None; step = None; origin = None };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1:]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           {
                             Slice.start = Some (+Expression.Constant (Constant.Integer 1));
                             stop = None;
                             step = None;
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[::2]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           {
                             Slice.start = None;
                             stop = None;
                             step = Some (+Expression.Constant (Constant.Integer 2));
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[:1]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           {
                             Slice.start = None;
                             stop = Some (+Expression.Constant (Constant.Integer 1));
                             step = None;
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[:1 if True else 2]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           {
                             Slice.start = None;
                             stop =
                               Some
                                 (+Expression.Ternary
                                     {
                                       Ternary.target = +Expression.Constant (Constant.Integer 1);
                                       test = +Expression.Constant Constant.True;
                                       alternative = +Expression.Constant (Constant.Integer 2);
                                     });
                             step = None;
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1:1]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Slice
                           {
                             Slice.start = Some (+Expression.Constant (Constant.Integer 1));
                             stop = Some (+Expression.Constant (Constant.Integer 1));
                             step = None;
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[1,2]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Tuple
                           [
                             +Expression.Constant (Constant.Integer 1);
                             +Expression.Constant (Constant.Integer 2);
                           ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a[:1,2]"
           [
             +Statement.Expression
                (+Expression.Subscript
                    {
                      Subscript.base = !"a";
                      index =
                        +Expression.Tuple
                           [
                             +Expression.Slice
                                {
                                  Slice.start = None;
                                  stop = Some (+Expression.Constant (Constant.Integer 1));
                                  step = None;
                                  origin = None;
                                };
                             +Expression.Constant (Constant.Integer 2);
                           ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "a.((2, 3))";
    ]


let test_starred =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "*a" [+Statement.Expression (+Expression.Starred (Starred.Once !"a"))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "*(a)"
           [+Statement.Expression (+Expression.Starred (Starred.Once !"a"))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "**a"
           [+Statement.Expression (+Expression.Starred (Starred.Twice !"a"))];
    ]


let test_compound =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1.0\n2"
           [
             +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
             +Statement.Expression (+Expression.Constant (Constant.Integer 2));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1.0;2"
           [
             +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
             +Statement.Expression (+Expression.Constant (Constant.Integer 2));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\n1.0\n2\n3"
           [
             +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
             +Statement.Expression (+Expression.Constant (Constant.Integer 2));
             +Statement.Expression (+Expression.Constant (Constant.Integer 3));
           ];
    ]


let decorator ?arguments name =
  Decorator.create_original_expression
    ~create_origin_for_reference:(fun _ -> None)
    ~call_origin:None
    ~name:(Node.create_with_default_location !&name)
    ~arguments


let test_define =
  let parent = NestingContext.create_toplevel () in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a):\n  1"
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
      @@ assert_parsed_equal
           "def foo(*, a):\n  1"
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
      @@ assert_parsed_equal
           "def foo(a, /, b):\n  1"
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
      @@ assert_parsed_equal
           "def foo(**a):\n  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [+{ Parameter.name = "**a"; value = None; annotation = None }];
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
                  body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "async def foo():\n  1"
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
      @@ assert_parsed_equal
           "async def foo():\n  ..."
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
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@foo\nasync def foo():\n  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [decorator "foo"];
                      return_annotation = None;
                      async = true;
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
      @@ assert_parsed_equal
           "@decorator\ndef foo(a):\n  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                      decorators = [decorator "decorator"];
                      return_annotation = None;
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
      @@ assert_parsed_equal
           "@decorator(a=b, c=d)\ndef foo(a):\n  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
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
      @@ assert_parsed_equal
           "@foo\n\n@bar\ndef foo(a):\n  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                      decorators = [decorator "foo"; decorator "bar"];
                      return_annotation = None;
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
      @@ assert_parsed_equal
           "@x[0].y\ndef foo(a):\n  1"
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
      @@ assert_parsed_equal
           "@(x<y)\ndef foo(a):\n  1"
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
      @@ assert_parsed_equal
           "def foo(a, b):\n  1"
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
      @@ assert_parsed_equal
           "def foo(a = 1, b):\n  1"
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
                             annotation = None;
                           };
                          +{ Parameter.name = "b"; value = None; annotation = None };
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
                  body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a=()):\n  1"
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
      @@ assert_parsed_equal
           "def foo(): 1; 2"
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
                      +Statement.Expression (+Expression.Constant (Constant.Integer 1));
                      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
                    ];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo():\n  1\n  2\n3"
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
                      +Statement.Expression (+Expression.Constant (Constant.Integer 1));
                      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
                    ];
                };
             +Statement.Expression (+Expression.Constant (Constant.Integer 3));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo():\n  def bar():\n    1\n    2\n3"
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
                               parent = NestingContext.create_function ~parent "foo";
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
                };
             +Statement.Expression (+Expression.Constant (Constant.Integer 3));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: int):  1"
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
      @@ assert_parsed_equal
           "def foo(a: int = 1):  1"
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
      @@ assert_parsed_equal
           "def foo(a: int, b: string):  1"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters =
                        [
                          +{ Parameter.name = "a"; value = None; annotation = Some !"int" };
                          +{ Parameter.name = "b"; value = None; annotation = Some !"string" };
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
                  body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: Tuple[int, str]):\n  1"
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
                                 (+Expression.Subscript
                                     {
                                       Subscript.base = !"Tuple";
                                       index = +Expression.Tuple [!"int"; !"str"];
                                       origin = None;
                                     });
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
                  body = [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a, b,) -> c:\n  1"
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
      @@ assert_parsed_equal
           "def foo() -> str:\n  1\n  2"
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
                      parent;
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo():  # pyre-ignore
        # type: (...) -> int
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo():
        # type: (...) -> int
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo():
        # type: (...) -> 'int'
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo():
        # type: () -> str
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
        def foo(): # type: ()-> str
          return 4
      |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
        def foo(): # type:   ()-> str
          return 4
      |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
         def foo( *args): # type: ( *str) -> str
           return 4
       |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters =
                        [
                          +{
                             Parameter.name = "*args";
                             value = None;
                             annotation =
                               Some
                                 (+Expression.Constant
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
         def foo( **kwargs): # type: ( **str) -> str
           return 4
       |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters =
                        [
                          +{
                             Parameter.name = "**kwargs";
                             value = None;
                             annotation =
                               Some
                                 (+Expression.Constant
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
         def foo( *args, **kwargs): # type: ( *str, **str) -> str
           return 4
       |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters =
                        [
                          +{
                             Parameter.name = "*args";
                             value = None;
                             annotation =
                               Some
                                 (+Expression.Constant
                                     (Constant.String (StringLiteral.create "str")));
                           };
                          +{
                             Parameter.name = "**kwargs";
                             value = None;
                             annotation =
                               Some
                                 (+Expression.Constant
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(a):
        # type: (str) -> str
        return 4
    |})
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
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(a): # type: (str) -> str
        return 4
    |})
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
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      (* Don't use string annotations if list length does not match signature *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(a): # type: (str, str) -> str
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], str) -> str
        return 4
    |})
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
                                     (Constant.String
                                        (StringLiteral.create "typing.Union[typing.List[int], str]")));
                           };
                          +{
                             Parameter.name = "b";
                             value = None;
                             annotation =
                               Some
                                 (+Expression.Constant
                                     (Constant.String (StringLiteral.create "str")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], typing.List[str]) -> str
        return 4
    |})
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(self, a, b): # type: (typing.Union[typing.List[int], str], typing.List[str]) -> str
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo(self, a, b): # type: (int) -> str
        return 4
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
      def foo():
        # type: (...) ->List[str]
        return 4
    |})
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
                          (+Expression.Constant (Constant.String (StringLiteral.create "List[str]")));
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
      @@ assert_parsed_equal
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
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters =
                        [
                          +{ Parameter.name = "self"; value = None; annotation = None };
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
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
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
      @@ assert_parsed_equal
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
      @@ assert_parsed_equal
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
                          +{ Parameter.name = "**kwargs"; value = None; annotation = None };
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
      @@ assert_parsed_equal
           (trim_extra_indentation
              {|
        def foo(a): # type: (A_b.C) -> str
          return "hi"
    |})
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
                                     (Constant.String (StringLiteral.create "A_b.C")));
                           };
                        ];
                      decorators = [];
                      return_annotation =
                        Some (+Expression.Constant (Constant.String (StringLiteral.create "str")));
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
                           Return.expression =
                             Some
                               (+Expression.Constant (Constant.String (StringLiteral.create "hi")));
                           is_implicit = false;
                         };
                    ];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (trim_extra_indentation {|
        async def foo(a):
          yield "A"
    |})
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"foo";
                      parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
                      decorators = [];
                      return_annotation = None;
                      async = true;
                      generator = true;
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body =
                    [
                      +Statement.Expression
                         (+Expression.Yield
                             (Some
                                (+Expression.Constant (Constant.String (StringLiteral.create "A")))));
                    ];
                };
           ];
    ]


let test_boolean_operator =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "True" [+Statement.Expression (+Expression.Constant Constant.True)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "False" [+Statement.Expression (+Expression.Constant Constant.False)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "True and False"
           [
             +Statement.Expression
                (+Expression.BooleanOperator
                    {
                      BooleanOperator.left = +Expression.Constant Constant.True;
                      operator = BooleanOperator.And;
                      right = +Expression.Constant Constant.False;
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 and False"
           [
             +Statement.Expression
                (+Expression.BooleanOperator
                    {
                      BooleanOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = BooleanOperator.And;
                      right = +Expression.Constant Constant.False;
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "True or 1"
           [
             +Statement.Expression
                (+Expression.BooleanOperator
                    {
                      BooleanOperator.left = +Expression.Constant Constant.True;
                      operator = BooleanOperator.Or;
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
                           };
                      operator = BooleanOperator.Or;
                      right = +Expression.Constant (Constant.Integer 3);
                      origin = None;
                    });
           ];
    ]


let test_binary_operator =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 + 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 ^ 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.BitXor;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 // 2"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.FloorDiv;
                      left = +Expression.Constant (Constant.Integer 1);
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 >> 2 >> 3"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.RShift;
                      left =
                        +Expression.BinaryOperator
                           {
                             operator = BinaryOperator.RShift;
                             left = +Expression.Constant (Constant.Integer 1);
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      right = +Expression.Constant (Constant.Integer 3);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 >> a.b"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.RShift;
                      left = +Expression.Constant (Constant.Integer 1);
                      right =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 - 2 + 3"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left =
                        +Expression.BinaryOperator
                           {
                             operator = BinaryOperator.Sub;
                             left = +Expression.Constant (Constant.Integer 1);
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      right = +Expression.Constant (Constant.Integer 3);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a + b.c"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = !"a";
                      right =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"b"; attribute = "c"; origin = None });
                      origin = None;
                    });
           ];
    ]


let test_unary_operator =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "not 1"
           [
             +Statement.Expression
                (+Expression.UnaryOperator
                    {
                      UnaryOperator.operator = UnaryOperator.Not;
                      operand = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "~1"
           [
             +Statement.Expression
                (+Expression.UnaryOperator
                    {
                      UnaryOperator.operator = UnaryOperator.Invert;
                      operand = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "+1"
           [
             +Statement.Expression
                (+Expression.UnaryOperator
                    {
                      UnaryOperator.operator = UnaryOperator.Positive;
                      operand = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
    ]


let test_lambda =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "lambda: 1"
           [
             +Statement.Expression
                (+Expression.Lambda
                    { Lambda.parameters = []; body = +Expression.Constant (Constant.Integer 1) });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "lambda x,: x"
           [
             +Statement.Expression
                (+Expression.Lambda
                    {
                      Lambda.parameters =
                        [+{ Parameter.name = "x"; value = None; annotation = None }];
                      body = !"x";
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "lambda x: x is y"
           [
             +Statement.Expression
                (+Expression.Lambda
                    {
                      Lambda.parameters =
                        [+{ Parameter.name = "x"; value = None; annotation = None }];
                      body =
                        +Expression.ComparisonOperator
                           {
                             ComparisonOperator.left = !"x";
                             operator = ComparisonOperator.Is;
                             right = !"y";
                             origin = None;
                           };
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "lambda x: x"
           [
             +Statement.Expression
                (+Expression.Lambda
                    {
                      Lambda.parameters =
                        [+{ Parameter.name = "x"; value = None; annotation = None }];
                      body = !"x";
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                        +Expression.BinaryOperator
                           {
                             operator = BinaryOperator.Add;
                             left = !"x";
                             right = +Expression.Constant (Constant.Integer 1);
                             origin = None;
                           };
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
           ];
    ]


let test_ternary =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
                           };
                      test = +Expression.Constant (Constant.Integer 1);
                      alternative = +Expression.Constant (Constant.Integer 1);
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
           ];
    ]


let test_dictionary =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "{}" [+Statement.Expression (+Expression.Dictionary [])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 2}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 2,}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 2, **durp}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                      Splat !"durp";
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 2, **durp, **hurp}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                      Splat !"durp";
                      Splat !"hurp";
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{**[1]}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [Splat (+Expression.List [+Expression.Constant (Constant.Integer 1)])]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{**durp, 1: 2}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      Splat !"durp";
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 1 < 2,}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value =
                            +Expression.ComparisonOperator
                               {
                                 ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                                 operator = ComparisonOperator.LessThan;
                                 right = +Expression.Constant (Constant.Integer 2);
                                 origin = None;
                               };
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1: 2, 2: 3}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 2);
                          value = +Expression.Constant (Constant.Integer 3);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{\n\t1: 2,\n\t2: 3}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 1);
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                      KeyValue
                        {
                          key = +Expression.Constant (Constant.Integer 2);
                          value = +Expression.Constant (Constant.Integer 3);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a: b for a in []}"
           [
             +Statement.Expression
                (+Expression.DictionaryComprehension
                    {
                      Comprehension.element = { key = !"a"; value = !"b" };
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a if a else a: b for a in []}"
           [
             +Statement.Expression
                (+Expression.DictionaryComprehension
                    {
                      Comprehension.element =
                        {
                          key =
                            +Expression.Ternary
                               { Ternary.target = !"a"; test = !"a"; alternative = !"a" };
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a if a else a: b if b else b for a in []}"
           [
             +Statement.Expression
                (+Expression.DictionaryComprehension
                    {
                      Comprehension.element =
                        {
                          key =
                            +Expression.Ternary
                               { Ternary.target = !"a"; test = !"a"; alternative = !"a" };
                          value =
                            +Expression.Ternary
                               { Ternary.target = !"b"; test = !"b"; alternative = !"b" };
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a.b or c: a for a in b}"
           [
             +Statement.Expression
                (+Expression.DictionaryComprehension
                    {
                      Comprehension.element =
                        {
                          key =
                            +Expression.BooleanOperator
                               {
                                 BooleanOperator.left =
                                   +Expression.Name
                                      (Name.Attribute
                                         {
                                           Name.Attribute.base = !"a";
                                           attribute = "b";
                                           origin = None;
                                         });
                                 operator = BooleanOperator.Or;
                                 right = !"c";
                                 origin = None;
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a: b for c, d in []}"
           [
             +Statement.Expression
                (+Expression.DictionaryComprehension
                    {
                      Comprehension.element = { key = !"a"; value = !"b" };
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a or b: 2}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key =
                            +Expression.BooleanOperator
                               {
                                 BooleanOperator.left = !"a";
                                 operator = BooleanOperator.Or;
                                 right = !"b";
                                 origin = None;
                               };
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{a and b: 2}"
           [
             +Statement.Expression
                (+Expression.Dictionary
                    [
                      KeyValue
                        {
                          key =
                            +Expression.BooleanOperator
                               {
                                 BooleanOperator.left = !"a";
                                 operator = BooleanOperator.And;
                                 right = !"b";
                                 origin = None;
                               };
                          value = +Expression.Constant (Constant.Integer 2);
                        };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "{ a or lambda b: b + 1: c }";
    ]


let test_list_parsing =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "[]" [+Statement.Expression (+Expression.List [])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "[[]]" [+Statement.Expression (+Expression.List [+Expression.List []])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "[1,]"
           [+Statement.Expression (+Expression.List [+Expression.Constant (Constant.Integer 1)])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "[1, 2]"
           [
             +Statement.Expression
                (+Expression.List
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "[\n\t1,\n\t2\n]"
           [
             +Statement.Expression
                (+Expression.List
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                     ComparisonOperator.left =
                                       +Expression.Constant (Constant.Integer 1);
                                     operator = ComparisonOperator.LessThan;
                                     right = +Expression.Constant (Constant.Integer 2);
                                     origin = None;
                                   };
                              ];
                            async = false;
                          };
                        ];
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                            origin = None;
                                          };
                                     operator = BooleanOperator.Or;
                                     right = +Expression.Constant Constant.True;
                                     origin = None;
                                   };
                              ];
                            async = false;
                          };
                        ];
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
           ];
    ]


let test_set =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1}"
           [+Statement.Expression (+Expression.Set [+Expression.Constant (Constant.Integer 1)])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{*[1]}"
           [
             +Statement.Expression
                (+Expression.Set
                    [
                      +Expression.Starred
                         (Starred.Once
                            (+Expression.List [+Expression.Constant (Constant.Integer 1)]));
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1,}"
           [+Statement.Expression (+Expression.Set [+Expression.Constant (Constant.Integer 1)])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "{1, 2}"
           [
             +Statement.Expression
                (+Expression.Set
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
           ];
    ]


let test_generator =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
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
           ];
    ]


let test_yield =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "yield" [+Statement.Expression (+Expression.Yield None)];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "yield 1"
           [
             +Statement.Expression
                (+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "yield from a"
           [+Statement.Expression (+Expression.YieldFrom (+Expression.Name (Name.Identifier "a")))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "x = yield 1"
           [
             +Statement.Assign
                {
                  Assign.target = !"x";
                  annotation = None;
                  value =
                    Some (+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "x: str = yield 1"
           [
             +Statement.Assign
                {
                  Assign.target = !"x";
                  annotation = Some !"str";
                  value =
                    Some (+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "x += yield 1"
           [
             +Statement.AugmentedAssign
                {
                  AugmentedAssign.target = !"x";
                  operator = BinaryOperator.Add;
                  value = +Expression.Yield (Some (+Expression.Constant (Constant.Integer 1)));
                };
           ];
    ]


let test_comparison =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.b < 2"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "b"; origin = None });
                      operator = ComparisonOperator.LessThan;
                      right = +Expression.Constant (Constant.Integer 2);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 in []"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.In;
                      right = +Expression.List [];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 is 1"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.Is;
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 is not 1"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.IsNot;
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo is not_bar"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Name (Name.Identifier "foo");
                      operator = ComparisonOperator.Is;
                      right = +Expression.Name (Name.Identifier "not_bar");
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 == 1"
           [
             +Statement.Expression
                (+Expression.ComparisonOperator
                    {
                      ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                      operator = ComparisonOperator.Equals;
                      right = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
                           };
                      operator = BooleanOperator.And;
                      right =
                        +Expression.ComparisonOperator
                           {
                             ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                             operator = ComparisonOperator.LessThan;
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                             origin = None;
                           };
                      operator = BooleanOperator.And;
                      right =
                        +Expression.ComparisonOperator
                           {
                             ComparisonOperator.left = +Expression.Constant (Constant.Integer 1);
                             operator = ComparisonOperator.Is;
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      origin = None;
                    });
           ];
    ]


let test_call =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo()"
           [
             +Statement.Expression
                (+Expression.Call { Call.callee = !"foo"; arguments = []; origin = None });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(a for a in [])"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
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
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(a for a in [],)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
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
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(1, 2,)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
                      arguments =
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
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo((1, 2))"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
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
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(x, 1, (a, b))"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
                      arguments =
                        [
                          { Call.Argument.name = None; value = !"x" };
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          { Call.Argument.name = None; value = +Expression.Tuple [!"a"; !"b"] };
                        ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.foo(x)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee =
                        +Expression.Name
                           (Name.Attribute
                              { Name.Attribute.base = !"a"; attribute = "foo"; origin = None });
                      arguments = [{ Call.Argument.name = None; value = !"x" }];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(1, a = 1, b = 2)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          {
                            Call.Argument.name = Some ~+"a";
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          {
                            Call.Argument.name = Some ~+"b";
                            value = +Expression.Constant (Constant.Integer 2);
                          };
                        ];
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "foo(1, a = 2, *args, **kwargs)"
           [
             +Statement.Expression
                (+Expression.Call
                    {
                      Call.callee = !"foo";
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value = +Expression.Constant (Constant.Integer 1);
                          };
                          {
                            Call.Argument.name = Some ~+"a";
                            value = +Expression.Constant (Constant.Integer 2);
                          };
                          {
                            Call.Argument.name = None;
                            value = +Expression.Starred (Starred.Once !"args");
                          };
                          {
                            Call.Argument.name = None;
                            value = +Expression.Starred (Starred.Twice !"kwargs");
                          };
                        ];
                      origin = None;
                    });
           ];
    ]


let test_string =
  let create_literal value = Substring.Literal (+value) in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"foo\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'''foo'''"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"\"\"foo\"\"\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "r'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "R'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "b'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "u'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "ub'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "bR'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'foo' 'bar'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foobar")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "ur'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "uR'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "Ur'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "UR'foo'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "foo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "f'foo'"
           [+Statement.Expression (+Expression.FormatString [create_literal "foo"])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "F'foo'"
           [+Statement.Expression (+Expression.FormatString [create_literal "foo"])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "f'foo' f'bar'"
           [
             +Statement.Expression
                (+Expression.FormatString [create_literal "foo"; create_literal "bar"]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "f'foo' 'bar'"
           [
             +Statement.Expression
                (+Expression.FormatString [create_literal "foo"; create_literal "bar"]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'foo' f'bar'"
           [
             +Statement.Expression
                (+Expression.FormatString [create_literal "foo"; create_literal "bar"]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"'\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "'")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'\"'"
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "\"")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"\\\"\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "\\\"")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"\\'\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "\\\'")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"\"\"\nfoo\"\"\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "\nfoo")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"f.o\\no\""
           [
             +Statement.Expression
                (+Expression.Constant (Constant.String (StringLiteral.create "f.o\\no")));
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'a' + 'b'"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                      right = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"a\" + \"b\""
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                      right = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "'''a''' + '''b'''"
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                      right = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "\"\"\"a\"\"\" + \"\"\"b\"\"\""
           [
             +Statement.Expression
                (+Expression.BinaryOperator
                    {
                      operator = BinaryOperator.Add;
                      left = +Expression.Constant (Constant.String (StringLiteral.create "a"));
                      right = +Expression.Constant (Constant.String (StringLiteral.create "b"));
                      origin = None;
                    });
           ];
    ]


let test_class =
  let parent = NestingContext.create_toplevel () in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@bar\nclass foo():\n\tpass"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Pass];
                  decorators = [decorator "bar"];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo: pass"
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
      @@ assert_parsed_equal
           "class foo:\n\tclass bar:\n\t\tpass"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Class
                         {
                           Class.name = !&"bar";
                           base_arguments = [];
                           parent = NestingContext.create_class ~parent "foo";
                           body = [+Statement.Pass];
                           decorators = [];
                           top_level_unbound_names = [];
                           type_params = [];
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo():\n\tclass bar:\n\t\tpass"
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo():\n\tdef bar(): pass"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      (let parent = NestingContext.create_class ~parent "foo" in
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
                          });
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo():\n\tdef bar():\n\t\tdef baz(): pass"
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
                              [
                                (let parent = NestingContext.create_function ~parent "bar" in
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
                                    });
                              ];
                          };
                     ]);
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo.bar: pass"
           [
             +Statement.Class
                {
                  Class.name = !&"foo.bar";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Pass];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(1, 2):\n\t1"
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
      @@ assert_parsed_equal
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
      @@ assert_parsed_equal
           "class foo(1, **kwargs):\n\t1"
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
      @@ assert_parsed_equal
           "class foo:\n\tattribute: int = 1"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Assign
                         {
                           Assign.target = !"attribute";
                           annotation = Some !"int";
                           value = Some (+Expression.Constant (Constant.Integer 1));
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo:\n\tattribute = 1 # type: int"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Assign
                         {
                           Assign.target = !"attribute";
                           annotation =
                             Some
                               (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                           value = Some (+Expression.Constant (Constant.Integer 1));
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo:\n\tattribute: int"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Assign
                         { Assign.target = !"attribute"; annotation = Some !"int"; value = None };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(superfoo):\n\tdef bar(): pass"
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
      @@ assert_parsed_equal
           "class foo():\n\tdef __init__(self): self.bar = 0"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Define
                         {
                           Define.signature =
                             {
                               Define.Signature.name = !&"__init__";
                               parameters =
                                 [+{ Parameter.name = "self"; value = None; annotation = None }];
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
                           body =
                             [
                               +Statement.Assign
                                  {
                                    Assign.target =
                                      +Expression.Name
                                         (Name.Attribute
                                            {
                                              Name.Attribute.base = !"self";
                                              attribute = "bar";
                                              origin = None;
                                            });
                                    annotation = None;
                                    value = Some (+Expression.Constant (Constant.Integer 0));
                                  };
                             ];
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      (* We mark parents for functions under if statements. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                  parent;
                  body =
                    [
                      +Statement.If
                         {
                           If.test = +Expression.Constant Constant.True;
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
                           orelse = [];
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
    ]


let test_return =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "return"
           [+Statement.Return { Return.expression = None; is_implicit = false }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "return 1"
           [
             +Statement.Return
                {
                  Return.expression = Some (+Expression.Constant (Constant.Integer 1));
                  is_implicit = false;
                };
           ];
    ]


let test_delete =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "del a" [+Statement.Delete [!"a"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "del a, b" [+Statement.Delete [!"a"; !"b"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "del (a, b)" [+Statement.Delete [+Expression.Tuple [!"a"; !"b"]]];
    ]


let test_assign =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = b"
           [+Statement.Assign { Assign.target = !"a"; annotation = None; value = Some !"b" }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = 1"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = None;
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a: int = 1"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = Some !"int";
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = 1  # type: int"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = 1  # type: 'int'"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a: int"
           [+Statement.Assign { Assign.target = !"a"; annotation = Some !"int"; value = None }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a # type: int"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                  value = None;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = 1  # type: ignore"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = None;
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a, b = 1"
           [
             +Statement.Assign
                {
                  Assign.target = +Expression.Tuple [!"a"; !"b"];
                  annotation = None;
                  value = Some (+Expression.Constant (Constant.Integer 1));
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = a().foo()"
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
      @@ assert_parsed_equal
           "a = b = 1"
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
      @@ assert_parsed_equal
           "a = yield from b"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = None;
                  value = Some (+Expression.YieldFrom !"b");
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a += 1"
           [
             +Statement.AugmentedAssign
                {
                  AugmentedAssign.target = !"a";
                  value = +Expression.Constant (Constant.Integer 1);
                  operator = BinaryOperator.Add;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a.b += 1"
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
      @@ assert_parsed_equal
           "a = b if b else c"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = None;
                  value =
                    Some
                      (+Expression.Ternary
                          { Ternary.target = !"b"; test = !"b"; alternative = !"c" });
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = b or c"
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
      @@ assert_parsed_equal
           "a = b or c or d"
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
                            right =
                              +Expression.BooleanOperator
                                 {
                                   BooleanOperator.left = !"c";
                                   operator = BooleanOperator.Or;
                                   right = !"d";
                                   origin = None;
                                 };
                            origin = None;
                          });
                };
           ];
    ]


let test_for =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
           ];
    ]


let test_while =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "while a: b\n"
           [
             +Statement.While { While.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "while a:\n\tb\nelse:\n\tc\n"
           [
             +Statement.While
                {
                  While.test = !"a";
                  body = [+Statement.Expression !"b"];
                  orelse = [+Statement.Expression !"c"];
                };
           ];
    ]


let test_if =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a: b\n"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a: b\nelif c: d"
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
      @@ assert_parsed_equal
           "if a:\n\n\tb\n"
           [+Statement.If { If.test = !"a"; body = [+Statement.Expression !"b"]; orelse = [] }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb\n\n\tc"
           [
             +Statement.If
                {
                  If.test = !"a";
                  body = [+Statement.Expression !"b"; +Statement.Expression !"c"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if a:\n\tb\nelse:\n\tc\n"
           [
             +Statement.If
                {
                  If.test = !"a";
                  body = [+Statement.Expression !"b"];
                  orelse = [+Statement.Expression !"c"];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                Call.callee = !"isinstance";
                                arguments =
                                  [
                                    { Call.Argument.name = None; value = !"x" };
                                    { Call.Argument.name = None; value = !"int" };
                                  ];
                                origin = None;
                              };
                         operator = BooleanOperator.And;
                         right =
                           +Expression.ComparisonOperator
                              {
                                ComparisonOperator.left = !"x";
                                operator = ComparisonOperator.GreaterThan;
                                right = +Expression.Constant (Constant.Integer 0);
                                origin = None;
                              };
                         origin = None;
                       };
                  body = [+Statement.Expression !"b"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                       Call.callee = !"foo";
                                       arguments = [{ Call.Argument.name = None; value = !"x" }];
                                       origin = None;
                                     };
                                operator = ComparisonOperator.GreaterThan;
                                right = +Expression.Constant (Constant.Integer 0);
                                origin = None;
                              };
                         origin = None;
                       };
                  body = [+Statement.Expression !"b"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                origin = None;
                              };
                         operator = BooleanOperator.Or;
                         right = +Expression.Constant Constant.True;
                         origin = None;
                       };
                  body = [+Statement.Expression !"b"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                origin = None;
                              };
                         operator = BooleanOperator.Or;
                         right =
                           +Expression.ComparisonOperator
                              {
                                ComparisonOperator.left = !"b";
                                operator = ComparisonOperator.Is;
                                right = +Expression.Constant (Constant.Integer 1);
                                origin = None;
                              };
                         origin = None;
                       };
                  body = [+Statement.Expression !"c"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                origin = None;
                              };
                         operator = BooleanOperator.Or;
                         right =
                           +Expression.ComparisonOperator
                              {
                                ComparisonOperator.left = !"b";
                                operator = ComparisonOperator.Equals;
                                right = +Expression.Constant (Constant.Integer 1);
                                origin = None;
                              };
                         origin = None;
                       };
                  body = [+Statement.Expression !"c"];
                  orelse = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                                origin = None;
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
                                       origin = None;
                                     };
                                operator = BooleanOperator.Or;
                                right =
                                  +Expression.ComparisonOperator
                                     {
                                       ComparisonOperator.left = !"c";
                                       operator = ComparisonOperator.Is;
                                       right = +Expression.Constant (Constant.Integer 1);
                                       origin = None;
                                     };
                                origin = None;
                              };
                         origin = None;
                       };
                  body = [+Statement.Expression !"d"];
                  orelse = [];
                };
           ];
    ]


let test_with =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with a: b\n"
           [
             +Statement.With
                { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = false };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with (yield from a): b\n"
           [
             +Statement.With
                {
                  With.items = [+Expression.YieldFrom !"a", None];
                  body = [+Statement.Expression !"b"];
                  async = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "async with a: b\n"
           [
             +Statement.With
                { With.items = [!"a", None]; body = [+Statement.Expression !"b"]; async = true };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with a as b: b\n"
           [
             +Statement.With
                {
                  With.items = [!"a", Some !"b"];
                  body = [+Statement.Expression !"b"];
                  async = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with a as b, c as d: b\n"
           [
             +Statement.With
                {
                  With.items = [!"a", Some !"b"; !"c", Some !"d"];
                  body = [+Statement.Expression !"b"];
                  async = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with a, c as d: b\n"
           [
             +Statement.With
                {
                  With.items = [!"a", None; !"c", Some !"d"];
                  body = [+Statement.Expression !"b"];
                  async = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "with a as b: # type: int\n\tb\n"
           [
             +Statement.With
                {
                  With.items = [!"a", Some !"b"];
                  body = [+Statement.Expression !"b"];
                  async = false;
                };
           ];
    ]


let test_raise =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "raise" [+Statement.Raise { Raise.expression = None; from = None }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "raise a"
           [+Statement.Raise { Raise.expression = Some !"a"; from = None }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "raise a from b"
           [+Statement.Raise { Raise.expression = Some !"a"; from = Some !"b" }];
    ]


let test_try =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "try: a"
           [
             +Statement.Try
                {
                  Try.body = [+Statement.Expression !"a"];
                  handlers = [];
                  orelse = [];
                  finally = [];
                  handles_exception_group = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "try:\n\ta\nelse:\n\tb"
           [
             +Statement.Try
                {
                  Try.body = [+Statement.Expression !"a"];
                  handlers = [];
                  orelse = [+Statement.Expression !"b"];
                  finally = [];
                  handles_exception_group = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "try:\n\ta\nfinally:\n\tb"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept:\n\tb"
           [
             +Statement.Try
                {
                  Try.body = [+Statement.Expression !"a"];
                  handlers =
                    [{ Try.Handler.kind = None; name = None; body = [+Statement.Expression !"b"] }];
                  orelse = [];
                  finally = [];
                  handles_exception_group = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "try:\n\ta\nexcept a:\n\tb"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept a as b:\n\tb"
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
      @@ assert_parsed_equal
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
      @@ assert_parsed_equal
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept a, b:\n\tb"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept (a, b) as c:\n\tb"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
           [
             +Statement.Try
                {
                  Try.body = [+Statement.Expression !"a"];
                  handlers =
                    [{ Try.Handler.kind = None; name = None; body = [+Statement.Expression !"b"] }];
                  orelse = [+Statement.Expression !"c"];
                  finally = [+Statement.Expression !"d"];
                  handles_exception_group = false;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "try:\n\ta\nexcept* a:\n\tb"
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
      @@ assert_parsed_equal
           "try:\n\ta\nexcept* a as b:\n\tb"
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
                  handles_exception_group = true;
                };
           ];
    ]


let test_assert =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "assert a"
           [+Statement.Assert { Assert.test = !"a"; message = None; origin = None }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "assert a is b"
           [
             +Statement.Assert
                {
                  Assert.test =
                    +Expression.ComparisonOperator
                       {
                         ComparisonOperator.left = !"a";
                         operator = ComparisonOperator.Is;
                         right = !"b";
                         origin = None;
                       };
                  message = None;
                  origin = None;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "assert a, b"
           [+Statement.Assert { Assert.test = !"a"; message = Some !"b"; origin = None }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                         origin = None;
                       };
                  message =
                    Some (+Expression.Constant (Constant.String (StringLiteral.create "b or c")));
                  origin = None;
                };
           ];
    ]


(* TODO(T145739378) Add location information on from *)
let test_import =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "import a"
           [
             +Statement.Import
                { Import.from = None; imports = [+{ Import.name = !&"a"; alias = None }] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "import async"
           [
             +Statement.Import
                { Import.from = None; imports = [+{ Import.name = !&"async"; alias = None }] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "import a.async"
           [
             +Statement.Import
                { Import.from = None; imports = [+{ Import.name = !&"a.async"; alias = None }] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "import a.b"
           [
             +Statement.Import
                { Import.from = None; imports = [+{ Import.name = !&"a.b"; alias = None }] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "import a as b"
           [
             +Statement.Import
                { Import.from = None; imports = [+{ Import.name = !&"a"; alias = Some "b" }] };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from a import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"a");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from a import *"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"a");
                  imports = [+{ Import.name = !&"*"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from . import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&".");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from ...foo import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 11) !&"...foo");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from .....foo import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 13) !&".....foo");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from .a import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 7) !&".a");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from ..a import b"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"..a");
                  imports = [+{ Import.name = !&"b"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from a import (b, c)"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"a");
                  imports =
                    [+{ Import.name = !&"b"; alias = None }; +{ Import.name = !&"c"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from a.b import c"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"a.b");
                  imports = [+{ Import.name = !&"c"; alias = None }];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "from f import a as b, c, d as e"
           [
             +Statement.Import
                {
                  Import.from = Some (node ~start:(1, 5) ~stop:(1, 6) !&"f");
                  imports =
                    [
                      +{ Import.name = !&"a"; alias = Some "b" };
                      +{ Import.name = !&"c"; alias = None };
                      +{ Import.name = !&"d"; alias = Some "e" };
                    ];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "from import x";
    ]


let test_global =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "global a" [+Statement.Global ["a"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "global a, b" [+Statement.Global ["a"; "b"]];
    ]


let test_tuple =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "1" [+Statement.Expression (+Expression.Constant (Constant.Integer 1))];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "()" [+Statement.Expression (+Expression.Tuple [])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(1,)"
           [+Statement.Expression (+Expression.Tuple [+Expression.Constant (Constant.Integer 1)])];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1, 2"
           [
             +Statement.Expression
                (+Expression.Tuple
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1, 1 + 1"
           [
             +Statement.Expression
                (+Expression.Tuple
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.BinaryOperator
                         {
                           operator = BinaryOperator.Add;
                           left = +Expression.Constant (Constant.Integer 1);
                           right = +Expression.Constant (Constant.Integer 1);
                           origin = None;
                         };
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "1 + 1, 1"
           [
             +Statement.Expression
                (+Expression.Tuple
                    [
                      +Expression.BinaryOperator
                         {
                           operator = BinaryOperator.Add;
                           left = +Expression.Constant (Constant.Integer 1);
                           right = +Expression.Constant (Constant.Integer 1);
                           origin = None;
                         };
                      +Expression.Constant (Constant.Integer 1);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(1, 2, 3)"
           [
             +Statement.Expression
                (+Expression.Tuple
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                      +Expression.Constant (Constant.Integer 3);
                    ]);
           ];
    ]


let test_stubs =
  let parent = NestingContext.create_toplevel () in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = ..."
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = None;
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a: int = ..."
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = Some !"int";
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = ... # type: int"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = ... # type: Tuple[str]"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some
                      (+Expression.Constant (Constant.String (StringLiteral.create "Tuple[str]")));
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a = ... # type: Tuple[str, ...]"
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some
                      (+Expression.Constant
                          (Constant.String (StringLiteral.create "Tuple[str, ...]")));
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a: Optional[int] = ..."
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation =
                    Some
                      (+Expression.Subscript
                          { Subscript.base = !"Optional"; index = !"int"; origin = None });
                  value = Some (+Expression.Constant Constant.Ellipsis);
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class A:\n\ta = ... # type: int"
           [
             +Statement.Class
                {
                  Class.name = !&"A";
                  base_arguments = [];
                  parent;
                  body =
                    [
                      +Statement.Assign
                         {
                           Assign.target = !"a";
                           annotation =
                             Some
                               (+Expression.Constant (Constant.String (StringLiteral.create "int")));
                           value = Some (+Expression.Constant Constant.Ellipsis);
                         };
                    ];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a): ..."
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
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a): ... # type: ignore"
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
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a):\n\t..."
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
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@overload\ndef foo(a: int = ...):  ..."
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
                             value = Some (+Expression.Constant Constant.Ellipsis);
                             annotation = Some !"int";
                           };
                        ];
                      decorators = [decorator "overload"];
                      return_annotation = None;
                      async = false;
                      generator = false;
                      parent;
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(): ..."
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo():\n\t..."
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(): ... # type: ignore"
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
    ]


let test_nonlocal =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "nonlocal a" [+Statement.Nonlocal ["a"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal "nonlocal a, b" [+Statement.Nonlocal ["a"; "b"]];
    ]


let test_ellipsis =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def __init__(debug = ...):\n\tpass"
           [
             +Statement.Define
                {
                  Define.signature =
                    {
                      Define.Signature.name = !&"__init__";
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
                      parent = NestingContext.create_toplevel ();
                      legacy_parent = None;
                      type_params = [];
                    };
                  captures = [];
                  unbound_names = [];
                  body = [+Statement.Pass];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
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
                         origin = None;
                       };
                  body = [+Statement.Pass];
                  orelse = [];
                };
           ];
    ]


let test_setitem =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "i[j] = 3"
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
      @@ assert_parsed_equal
           "i[j] += 3"
           [
             +Statement.AugmentedAssign
                {
                  AugmentedAssign.target =
                    +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                  value = +Expression.Constant (Constant.Integer 3);
                  operator = BinaryOperator.Add;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "i[j][7] = 8"
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
      @@ assert_parsed_equal
           "i[j::1] = i[:j]"
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
      @@ assert_parsed_equal
           "i[j] = 5 if 1 else 1"
           [
             +Statement.Assign
                {
                  Assign.target =
                    +Expression.Subscript { Subscript.base = !"i"; index = !"j"; origin = None };
                  value =
                    Some
                      (+Expression.Ternary
                          {
                            Ternary.target = +Expression.Constant (Constant.Integer 5);
                            test = +Expression.Constant (Constant.Integer 1);
                            alternative = +Expression.Constant (Constant.Integer 1);
                          });
                  annotation = None;
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "x = i[j] = y"
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
      @@ assert_parsed_equal
           "x, i[j] = y"
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


let test_byte_order_mark =
  "test_byte_order_mark"
  >:: fun _context ->
  let parse lines = PyreMenhirParser.Parser.parse_exn lines |> ignore in
  let byte_order_mark = [0xEF; 0xBB; 0xBF] |> List.map ~f:Char.of_int_exn |> String.of_char_list in
  (* Ensure that we can parse UTF-8 files with byte order marks properly. *)
  parse [byte_order_mark ^ "1"];
  assert_raises
    (PyreMenhirParser.Parser.Error
       {
         PyreMenhirParser.Parser.Error.location =
           {
             Location.start = { Location.line = 2; column = 0 };
             stop = { Location.line = 2; column = 0 };
           };
         file_name = "$invalid_path";
         content = Some (byte_order_mark ^ "2");
       })
    (fun () -> parse ["1"; byte_order_mark ^ "2"])


let test_walrus_operator =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(a := 1)"
           [
             +Statement.Expression
                (+Expression.WalrusOperator
                    {
                      WalrusOperator.target = !"a";
                      value = +Expression.Constant (Constant.Integer 1);
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (* Binds more tightly than a comma. *)
           "(a := 1, 2)"
           [
             +Statement.Expression
                (+Expression.Tuple
                    [
                      +Expression.WalrusOperator
                         {
                           WalrusOperator.target = !"a";
                           value = +Expression.Constant (Constant.Integer 1);
                           origin = None;
                         };
                      +Expression.Constant (Constant.Integer 2);
                    ]);
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (* Binds less tightly than a binary operator. *)
           "(a := 1 + 2)"
           [
             +Statement.Expression
                (+Expression.WalrusOperator
                    {
                      WalrusOperator.target = !"a";
                      value =
                        +Expression.BinaryOperator
                           {
                             operator = BinaryOperator.Add;
                             left = +Expression.Constant (Constant.Integer 1);
                             right = +Expression.Constant (Constant.Integer 2);
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (* Binds less tightly than `and`. *)
           "(a := True and False)"
           [
             +Statement.Expression
                (+Expression.WalrusOperator
                    {
                      WalrusOperator.target = !"a";
                      value =
                        +Expression.BooleanOperator
                           {
                             BooleanOperator.left = +Expression.Constant Constant.True;
                             operator = BooleanOperator.And;
                             right = +Expression.Constant Constant.False;
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           (* Binds less tightly than a conditional expression. *)
           "(a := 1 if True else 2)"
           [
             +Statement.Expression
                (+Expression.WalrusOperator
                    {
                      WalrusOperator.target = !"a";
                      value =
                        +Expression.Ternary
                           {
                             Ternary.target = +Expression.Constant (Constant.Integer 1);
                             test = +Expression.Constant Constant.True;
                             alternative = +Expression.Constant (Constant.Integer 2);
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "(a := (b := 1))"
           [
             +Statement.Expression
                (+Expression.WalrusOperator
                    {
                      WalrusOperator.target = !"a";
                      value =
                        +Expression.WalrusOperator
                           {
                             WalrusOperator.target = !"b";
                             value = +Expression.Constant (Constant.Integer 1);
                             origin = None;
                           };
                      origin = None;
                    });
           ];
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_not_parsed "(a := 1) := 2";
    ]


let test_format_string =
  let assert_format_string source value _context =
    let parsed_source = parse_untrimmed source in
    let found_any =
      Visit.collect_locations parsed_source |> List.for_all ~f:(Location.equal Location.any)
    in
    if found_any then
      Printf.printf "\nLocation.any\n  found in parse of %s\n" source;
    assert_false found_any;
    assert_source_equal
      ~location_insensitive:true
      (Source.create [+Statement.Expression (+Expression.FormatString value)])
      parsed_source
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string "f'foo'" [Substring.Literal (+"foo")];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'{1}'"
           [
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{1}'"
           [
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{1}' 'foo{2}'"
           [
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+"foo{2}");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "'foo{1}' f'foo{2}'"
           [
             Substring.Literal (+"foo{1}");
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 2) };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{1}' f'foo{2}'"
           [
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 2) };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{1}{2}foo'"
           [
             Substring.Literal (+"foo");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 2) };
             Substring.Literal (+"foo");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{{1}}'"
           [Substring.Literal (+"foo"); Substring.Literal (+"{{1}}")];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{{ {1} }}'"
           [
             Substring.Literal (+"foo");
             Substring.Literal (+"{{ ");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+" }}");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{{{1} }}'"
           [
             Substring.Literal (+"foo");
             Substring.Literal (+"{{");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+" }}");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{{ {1}}}'"
           [
             Substring.Literal (+"foo");
             Substring.Literal (+"{{ ");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+"}}");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{{{1}}}'"
           [
             Substring.Literal (+"foo");
             Substring.Literal (+"{{");
             Substring.Format
               { format_spec = None; value = +Expression.Constant (Constant.Integer 1) };
             Substring.Literal (+"}}");
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string "f'foo{{'" [Substring.Literal (+"foo"); Substring.Literal (+"{{")];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string "f'foo}}'" [Substring.Literal (+"foo}}")];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'foo{1+2}'"
           [
             Substring.Literal (+"foo");
             Substring.Format
               {
                 format_spec = None;
                 value =
                   +Expression.BinaryOperator
                      {
                        operator = BinaryOperator.Add;
                        left = +Expression.Constant (Constant.Integer 1);
                        right = +Expression.Constant (Constant.Integer 2);
                        origin = None;
                      };
               };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_format_string
           "f'{x for x in []}'"
           [
             Substring.Format
               {
                 format_spec = None;
                 value =
                   +Expression.Generator
                      {
                        Comprehension.element = +Expression.Name (Name.Identifier "x");
                        generators =
                          [
                            {
                              Comprehension.Generator.target = +Expression.Name (Name.Identifier "x");
                              iterator = +Expression.List [];
                              conditions = [];
                              async = false;
                            };
                          ];
                      };
               };
           ];
    ]


let () =
  "parsing"
  >::: [
         test_lexer;
         test_number;
         test_await;
         test_name;
         test_starred;
         test_compound;
         test_define;
         test_boolean_operator;
         test_binary_operator;
         test_unary_operator;
         test_lambda;
         test_ternary;
         test_dictionary;
         test_list_parsing;
         test_set;
         test_generator;
         test_yield;
         test_comparison;
         test_call;
         test_string;
         test_class;
         test_return;
         test_delete;
         test_assign;
         test_for;
         test_while;
         test_if;
         test_with;
         test_raise;
         test_try;
         test_assert;
         test_import;
         test_global;
         test_tuple;
         test_stubs;
         test_nonlocal;
         test_ellipsis;
         test_setitem;
         test_byte_order_mark;
         test_walrus_operator;
         test_format_string;
       ]
  |> Test.run

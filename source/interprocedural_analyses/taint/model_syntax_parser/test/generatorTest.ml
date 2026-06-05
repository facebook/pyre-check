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
  match PysaModelSyntaxParser.Parser.parse (String.split source ~on:'\n') with
  | Result.Ok statements -> Source.create statements
  | Result.Error
      {
        PysaModelSyntaxParser.Parser.Error.location =
          { Location.start = { Location.line; column }; _ };
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
  match PysaModelSyntaxParser.Parser.parse (String.split source ~on:'\n') with
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
           "def foo(): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
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
           "def foo(*, a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a, /, b): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(**a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
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
           "@foo\nasync def foo(): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@decorator\ndef foo(a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@decorator(a=b, c=d)\ndef foo(a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@foo\n\n@bar\ndef foo(a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@x[0].y\ndef foo(a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@(x<y)\ndef foo(a): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a, b): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a = 1, b): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a=()): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: int):  ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: int = 1):  ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: int, b: string):  ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a: Tuple[int, str]): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo(a, b,) -> c: ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def foo() -> str: ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
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
           "[\n\t1,\n\t2\n]"
           [
             +Statement.Expression
                (+Expression.List
                    [
                      +Expression.Constant (Constant.Integer 1);
                      +Expression.Constant (Constant.Integer 2);
                    ]);
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
    ]


let test_class =
  let parent = NestingContext.create_toplevel () in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "@bar\nclass foo():\n\t..."
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [];
                  parent;
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [decorator "bar"];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo: ..."
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
           "class foo.bar: ..."
           [
             +Statement.Class
                {
                  Class.name = !&"foo.bar";
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
           "class foo(1, 2): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(init_subclass_arg=\"literal_string\"): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(1, **kwargs): ..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "class foo(superfoo): ..."
           [
             +Statement.Class
                {
                  Class.name = !&"foo";
                  base_arguments = [{ Call.Argument.name = None; value = !"superfoo" }];
                  parent;
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  decorators = [];
                  top_level_unbound_names = [];
                  type_params = [];
                };
           ];
    ]


let test_assign =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "a: int"
           [
             +Statement.Assign
                { Assign.target = !"a"; annotation = Some !"int"; value = None; origin = None };
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
           "a: int = ..."
           [
             +Statement.Assign
                {
                  Assign.target = !"a";
                  annotation = Some !"int";
                  value = Some (+Expression.Constant Constant.Ellipsis);
                  origin = None;
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
                  origin = None;
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
    ]


let test_ellipsis =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "def __init__(debug = ...):\n\t..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_parsed_equal
           "if x is ...:\n\t..."
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
                  body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
                  orelse = [];
                };
           ];
    ]


let test_byte_order_mark =
  "test_byte_order_mark"
  >:: fun _context ->
  let parse lines = PysaModelSyntaxParser.Parser.parse_exn lines |> ignore in
  let byte_order_mark = [0xEF; 0xBB; 0xBF] |> List.map ~f:Char.of_int_exn |> String.of_char_list in
  (* Ensure that we can parse UTF-8 files with byte order marks properly. *)
  parse [byte_order_mark ^ "1"];
  assert_raises
    (PysaModelSyntaxParser.Parser.Error
       {
         PysaModelSyntaxParser.Parser.Error.location =
           {
             Location.start = { Location.line = 2; column = 0 };
             stop = { Location.line = 2; column = 0 };
           };
         file_name = "$invalid_path";
         content = Some (byte_order_mark ^ "2");
       })
    (fun () -> parse ["1"; byte_order_mark ^ "2"])


let () =
  "parsing"
  >::: [
         test_lexer;
         test_number;
         test_name;
         test_starred;
         test_compound;
         test_define;
         test_boolean_operator;
         test_unary_operator;
         test_list_parsing;
         test_comparison;
         test_call;
         test_string;
         test_class;
         test_assign;
         test_if;
         test_tuple;
         test_stubs;
         test_ellipsis;
         test_byte_order_mark;
       ]
  |> Test.run

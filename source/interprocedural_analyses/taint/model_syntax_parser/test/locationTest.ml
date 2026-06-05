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


let parse source = Test.trim_extra_indentation source |> parse_untrimmed

let assert_source_locations source statements =
  let parsed_source = parse source in
  let expected_source = { parsed_source with Source.statements } in
  assert_source_equal_with_locations expected_source parsed_source


let test_assign_locations _ =
  assert_source_locations
    "a: int"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation =
               Some (node ~start:(1, 3) ~stop:(1, 6) (Expression.Name (Name.Identifier "int")));
             value = None;
             origin = None;
           });
    ]


let test_call_locations _ =
  assert_source_locations
    "a[1 < 2]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 8)
              (Expression.Subscript
                 {
                   Subscript.base =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   index =
                     node
                       ~start:(1, 2)
                       ~stop:(1, 7)
                       (Expression.ComparisonOperator
                          {
                            ComparisonOperator.left =
                              node
                                ~start:(1, 2)
                                ~stop:(1, 3)
                                (Expression.Constant (Constant.Integer 1));
                            operator = ComparisonOperator.LessThan;
                            right =
                              node
                                ~start:(1, 6)
                                ~stop:(1, 7)
                                (Expression.Constant (Constant.Integer 2));
                            origin = None;
                          });
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a[1, 2]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Subscript
                 {
                   Subscript.base =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   index =
                     node
                       ~start:(1, 2)
                       ~stop:(1, 6)
                       (Expression.Tuple
                          [
                            node
                              ~start:(1, 2)
                              ~stop:(1, 3)
                              (Expression.Constant (Constant.Integer 1));
                            node
                              ~start:(1, 5)
                              ~stop:(1, 6)
                              (Expression.Constant (Constant.Integer 2));
                          ]);
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a.__getitem__(argument)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 23)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 23)
              (Expression.Call
                 {
                   Call.callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 13)
                       (Expression.Name
                          (Name.Attribute
                             {
                               Name.Attribute.base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               origin = None;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 14)
                             ~stop:(1, 22)
                             (Expression.Name (Name.Identifier "argument"));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a((1, 2))"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 9)
              (Expression.Call
                 {
                   Call.callee =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 3)
                             ~stop:(1, 7)
                             (Expression.Tuple
                                [
                                  node
                                    ~start:(1, 3)
                                    ~stop:(1, 4)
                                    (Expression.Constant (Constant.Integer 1));
                                  node
                                    ~start:(1, 6)
                                    ~stop:(1, 7)
                                    (Expression.Constant (Constant.Integer 2));
                                ]);
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a(arg1,  arg2,)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 15)
              (Expression.Call
                 {
                   Call.callee =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 2)
                             ~stop:(1, 6)
                             (Expression.Name (Name.Identifier "arg1"));
                       };
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 9)
                             ~stop:(1, 13)
                             (Expression.Name (Name.Identifier "arg2"));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a(arg1)(arg2)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 13)
              (Expression.Call
                 {
                   Call.callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 7)
                       (Expression.Call
                          {
                            Call.callee =
                              node
                                ~start:(1, 0)
                                ~stop:(1, 1)
                                (Expression.Name (Name.Identifier "a"));
                            arguments =
                              [
                                {
                                  Call.Argument.name = None;
                                  value =
                                    node
                                      ~start:(1, 2)
                                      ~stop:(1, 6)
                                      (Expression.Name (Name.Identifier "arg1"));
                                };
                              ];
                            origin = None;
                          });
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 8)
                             ~stop:(1, 12)
                             (Expression.Name (Name.Identifier "arg2"));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a(  arg1)((arg2)  )"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 19)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 19)
              (Expression.Call
                 {
                   Call.callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 9)
                       (Expression.Call
                          {
                            Call.callee =
                              node
                                ~start:(1, 0)
                                ~stop:(1, 1)
                                (Expression.Name (Name.Identifier "a"));
                            arguments =
                              [
                                {
                                  Call.Argument.name = None;
                                  value =
                                    node
                                      ~start:(1, 4)
                                      ~stop:(1, 8)
                                      (Expression.Name (Name.Identifier "arg1"));
                                };
                              ];
                            origin = None;
                          });
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 11)
                             ~stop:(1, 15)
                             (Expression.Name (Name.Identifier "arg2"));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "foo(1, a = 2, *args, **kwargs)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 30)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 30)
              (Expression.Call
                 {
                   Call.callee =
                     node ~start:(1, 0) ~stop:(1, 3) (Expression.Name (Name.Identifier "foo"));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 4)
                             ~stop:(1, 5)
                             (Expression.Constant (Constant.Integer 1));
                       };
                       {
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 8) "a");
                         value =
                           node
                             ~start:(1, 11)
                             ~stop:(1, 12)
                             (Expression.Constant (Constant.Integer 2));
                       };
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 14)
                             ~stop:(1, 19)
                             (Expression.Starred
                                (Starred.Once
                                   (node
                                      ~start:(1, 15)
                                      ~stop:(1, 19)
                                      (Expression.Name (Name.Identifier "args")))));
                       };
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 21)
                             ~stop:(1, 29)
                             (Expression.Starred
                                (Starred.Twice
                                   (node
                                      ~start:(1, 23)
                                      ~stop:(1, 29)
                                      (Expression.Name (Name.Identifier "kwargs")))));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "foo(1, second = 2)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 18)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 18)
              (Expression.Call
                 {
                   Call.callee =
                     node ~start:(1, 0) ~stop:(1, 3) (Expression.Name (Name.Identifier "foo"));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 4)
                             ~stop:(1, 5)
                             (Expression.Constant (Constant.Integer 1));
                       };
                       {
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                         value =
                           node
                             ~start:(1, 16)
                             ~stop:(1, 17)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "foo(1, second = \n2)"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 2)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(2, 2)
              (Expression.Call
                 {
                   Call.callee =
                     node ~start:(1, 0) ~stop:(1, 3) (Expression.Name (Name.Identifier "foo"));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 4)
                             ~stop:(1, 5)
                             (Expression.Constant (Constant.Integer 1));
                       };
                       {
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                         value =
                           node
                             ~start:(2, 0)
                             ~stop:(2, 1)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a[:1]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 5)
              (Expression.Subscript
                 {
                   Subscript.base =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   index =
                     node
                       ~start:(1, 2)
                       ~stop:(1, 4)
                       (Expression.Slice
                          {
                            Slice.start = None;
                            stop =
                              Some
                                (node
                                   ~start:(1, 3)
                                   ~stop:(1, 4)
                                   (Expression.Constant (Constant.Integer 1)));
                            step = None;
                            origin = None;
                          });
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a[::2]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.Subscript
                 {
                   Subscript.base =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   index =
                     node
                       ~start:(1, 2)
                       ~stop:(1, 5)
                       (Expression.Slice
                          {
                            Slice.start = None;
                            stop = None;
                            step =
                              Some
                                (node
                                   ~start:(1, 4)
                                   ~stop:(1, 5)
                                   (Expression.Constant (Constant.Integer 2)));
                            origin = None;
                          });
                   origin = None;
                 })));
    ];
  assert_source_locations
    "a[:]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 4)
              (Expression.Subscript
                 {
                   Subscript.base =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   index =
                     node
                       ~start:(1, 2)
                       ~stop:(1, 3)
                       (Expression.Slice
                          { Slice.start = None; stop = None; step = None; origin = None });
                   origin = None;
                 })));
    ]


let test_if_locations _ =
  assert_source_locations
    "if a : b\nelif c: d"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 9)
        (Statement.If
           {
             If.test = node ~start:(1, 3) ~stop:(1, 4) (Expression.Name (Name.Identifier "a"));
             body =
               [
                 node
                   ~start:(1, 7)
                   ~stop:(1, 8)
                   (Statement.Expression
                      (node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "b"))));
               ];
             orelse =
               [
                 node
                   ~start:(2, 0)
                   ~stop:(2, 9)
                   (Statement.If
                      {
                        If.test =
                          node ~start:(2, 5) ~stop:(2, 6) (Expression.Name (Name.Identifier "c"));
                        body =
                          [
                            node
                              ~start:(2, 8)
                              ~stop:(2, 9)
                              (Statement.Expression
                                 (node
                                    ~start:(2, 8)
                                    ~stop:(2, 9)
                                    (Expression.Name (Name.Identifier "d"))));
                          ];
                        orelse = [];
                      });
               ];
           });
    ];
  assert_source_locations
    "if a:\n\n\tb\n"
    [
      node
        ~start:(1, 0)
        ~stop:(3, 2)
        (Statement.If
           {
             If.test = node ~start:(1, 3) ~stop:(1, 4) (Expression.Name (Name.Identifier "a"));
             body =
               [
                 node
                   ~start:(3, 1)
                   ~stop:(3, 2)
                   (Statement.Expression
                      (node ~start:(3, 1) ~stop:(3, 2) (Expression.Name (Name.Identifier "b"))));
               ];
             orelse = [];
           });
    ];
  assert_source_locations
    "if a:\n\tb\nelse:\n\tc\n"
    [
      node
        ~start:(1, 0)
        ~stop:(4, 2)
        (Statement.If
           {
             If.test = node ~start:(1, 3) ~stop:(1, 4) (Expression.Name (Name.Identifier "a"));
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "b"))));
               ];
             orelse =
               [
                 node
                   ~start:(4, 1)
                   ~stop:(4, 2)
                   (Statement.Expression
                      (node ~start:(4, 1) ~stop:(4, 2) (Expression.Name (Name.Identifier "c"))));
               ];
           });
    ];
  assert_source_locations
    "if a is 1 or b == 1:\n\tc"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 2)
        (Statement.If
           {
             If.test =
               node
                 ~start:(1, 3)
                 ~stop:(1, 19)
                 (Expression.BooleanOperator
                    {
                      BooleanOperator.left =
                        node
                          ~start:(1, 3)
                          ~stop:(1, 9)
                          (Expression.ComparisonOperator
                             {
                               ComparisonOperator.left =
                                 node
                                   ~start:(1, 3)
                                   ~stop:(1, 4)
                                   (Expression.Name (Name.Identifier "a"));
                               operator = ComparisonOperator.Is;
                               right =
                                 node
                                   ~start:(1, 8)
                                   ~stop:(1, 9)
                                   (Expression.Constant (Constant.Integer 1));
                               origin = None;
                             });
                      operator = BooleanOperator.Or;
                      right =
                        node
                          ~start:(1, 13)
                          ~stop:(1, 19)
                          (Expression.ComparisonOperator
                             {
                               ComparisonOperator.left =
                                 node
                                   ~start:(1, 13)
                                   ~stop:(1, 14)
                                   (Expression.Name (Name.Identifier "b"));
                               operator = ComparisonOperator.Equals;
                               right =
                                 node
                                   ~start:(1, 18)
                                   ~stop:(1, 19)
                                   (Expression.Constant (Constant.Integer 1));
                               origin = None;
                             });
                      origin = None;
                    });
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "c"))));
               ];
             orelse = [];
           });
    ]


let test_list_locations _ =
  assert_source_locations
    "[[ ] ]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.List [node ~start:(1, 1) ~stop:(1, 4) (Expression.List [])])));
    ];
  assert_source_locations
    "[1, 2,]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.List
                 [
                   node ~start:(1, 1) ~stop:(1, 2) (Expression.Constant (Constant.Integer 1));
                   node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 2));
                 ])));
    ]


let test_name_locations _ =
  assert_source_locations
    "a.b.c"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 5)
              (Expression.Name
                 (Name.Attribute
                    {
                      Name.Attribute.base =
                        node
                          ~start:(1, 0)
                          ~stop:(1, 3)
                          (Expression.Name
                             (Name.Attribute
                                {
                                  Name.Attribute.base =
                                    node
                                      ~start:(1, 0)
                                      ~stop:(1, 1)
                                      (Expression.Name (Name.Identifier "a"));
                                  attribute = "b";
                                  origin = None;
                                }));
                      attribute = "c";
                      origin = None;
                    }))));
    ];
  assert_source_locations
    "((a)).b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Name
                 (Name.Attribute
                    {
                      Name.Attribute.base =
                        node ~start:(1, 2) ~stop:(1, 3) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      origin = None;
                    }))));
    ];
  assert_source_locations
    "(a  \n).b"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 3)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(2, 3)
              (Expression.Name
                 (Name.Attribute
                    {
                      Name.Attribute.base =
                        node ~start:(1, 1) ~stop:(1, 2) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      origin = None;
                    }))));
    ];
  assert_source_locations
    {|
      a. \
      b
    |}
    [
      node
        ~start:(2, 0)
        ~stop:(3, 1)
        (Statement.Expression
           (node
              ~start:(2, 0)
              ~stop:(3, 1)
              (Expression.Name
                 (Name.Attribute
                    {
                      Name.Attribute.base =
                        node ~start:(2, 0) ~stop:(2, 1) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      origin = None;
                    }))));
    ];
  assert_source_locations
    "a(arg).b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 8)
              (Expression.Name
                 (Name.Attribute
                    {
                      Name.Attribute.base =
                        node
                          ~start:(1, 0)
                          ~stop:(1, 6)
                          (Expression.Call
                             {
                               Call.callee =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               arguments =
                                 [
                                   {
                                     Call.Argument.name = None;
                                     value =
                                       node
                                         ~start:(1, 2)
                                         ~stop:(1, 5)
                                         (Expression.Name (Name.Identifier "arg"));
                                   };
                                 ];
                               origin = None;
                             });
                      attribute = "b";
                      origin = None;
                    }))));
    ]


let test_number_locations _ =
  assert_source_locations
    "((1))"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression
           (node ~start:(1, 2) ~stop:(1, 3) (Expression.Constant (Constant.Integer 1))));
    ];
  assert_source_locations
    ".1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 2)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 2) (Expression.Constant (Constant.Float 0.1))));
    ];
  assert_source_locations
    "1."
    [
      node
        ~start:(1, 0)
        ~stop:(1, 2)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 2) (Expression.Constant (Constant.Float 1.0))));
    ];
  assert_source_locations
    "1e10"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 4) (Expression.Constant (Constant.Float 1e10))));
    ];
  assert_source_locations
    "0.1j"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 4) (Expression.Constant (Constant.Complex 0.1))));
    ];
  assert_source_locations
    "1L"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 2)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 2) (Expression.Constant (Constant.Integer 1))));
    ];
  assert_source_locations
    "-(1)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 4)
              (Expression.UnaryOperator
                 {
                   UnaryOperator.operator = UnaryOperator.Negative;
                   operand =
                     node ~start:(1, 2) ~stop:(1, 3) (Expression.Constant (Constant.Integer 1));
                   origin = None;
                 })));
    ]


let test_operator_locations _ =
  assert_source_locations
    "1 and 2 or 3"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 12)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 12)
              (Expression.BooleanOperator
                 {
                   BooleanOperator.left =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 7)
                       (Expression.BooleanOperator
                          {
                            BooleanOperator.left =
                              node
                                ~start:(1, 0)
                                ~stop:(1, 1)
                                (Expression.Constant (Constant.Integer 1));
                            operator = BooleanOperator.And;
                            right =
                              node
                                ~start:(1, 6)
                                ~stop:(1, 7)
                                (Expression.Constant (Constant.Integer 2));
                            origin = None;
                          });
                   operator = BooleanOperator.Or;
                   right =
                     node ~start:(1, 11) ~stop:(1, 12) (Expression.Constant (Constant.Integer 3));
                   origin = None;
                 })));
    ];
  assert_source_locations
    "1 is not 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 10)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 10)
              (Expression.ComparisonOperator
                 {
                   ComparisonOperator.left =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 1));
                   operator = ComparisonOperator.IsNot;
                   right =
                     node ~start:(1, 9) ~stop:(1, 10) (Expression.Constant (Constant.Integer 1));
                   origin = None;
                 })));
    ];
  assert_source_locations
    "not 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 5)
              (Expression.UnaryOperator
                 {
                   UnaryOperator.operator = UnaryOperator.Not;
                   operand =
                     node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 1));
                   origin = None;
                 })));
    ]


let test_string_locations _ =
  assert_source_locations
    "'foo'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 5)
              (Expression.Constant (Constant.String (StringLiteral.create "foo")))));
    ];
  assert_source_locations
    "'''foo'''"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 9)
              (Expression.Constant (Constant.String (StringLiteral.create "foo")))));
    ];
  assert_source_locations
    "'foo' 'bar'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 11)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 11)
              (Expression.Constant (Constant.String (StringLiteral.create "foobar")))));
    ];
  assert_source_locations
    "ur'foo'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Constant (Constant.String (StringLiteral.create "foo")))));
    ];
  assert_source_locations
    "'''multiline\nliteral'''\n"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 10)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(2, 10)
              (Expression.Constant (Constant.String (StringLiteral.create "multiline\nliteral")))));
    ];
  assert_source_locations
    "\"\"\"multiline\nliteral\"\"\"\n"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 10)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(2, 10)
              (Expression.Constant (Constant.String (StringLiteral.create "multiline\nliteral")))));
    ]


let test_tuple_locations _ =
  assert_source_locations
    "()"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 2)
        (Statement.Expression (node ~start:(1, 0) ~stop:(1, 2) (Expression.Tuple [])));
    ];
  assert_source_locations
    "(1,)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node
              ~start:(1, 1)
              ~stop:(1, 2)
              (Expression.Tuple
                 [node ~start:(1, 1) ~stop:(1, 2) (Expression.Constant (Constant.Integer 1))])));
    ]


let () =
  "locations"
  >::: [
         "test_assign_locations" >:: test_assign_locations;
         "test_call_locations" >:: test_call_locations;
         "test_if_locations" >:: test_if_locations;
         "test_list_locations" >:: test_list_locations;
         "test_name_locations" >:: test_name_locations;
         "test_number_locations" >:: test_number_locations;
         "test_operator_locations" >:: test_operator_locations;
         "test_string_locations" >:: test_string_locations;
         "test_tuple_locations" >:: test_tuple_locations;
       ]
  |> Test.run

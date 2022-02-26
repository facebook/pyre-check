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

let parse source = Test.trim_extra_indentation source |> GeneratorTest.parse_untrimmed

let assert_source_locations source statements =
  let parsed_source = parse source in
  let expected_source = { parsed_source with Source.statements } in
  assert_source_equal_with_locations expected_source parsed_source


let test_assert_locations _ =
  assert_source_locations
    "assert a"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Assert
           {
             Assert.test = node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
             message = None;
             origin = Assert.Origin.Assertion;
           });
    ];
  assert_source_locations
    "assert a is b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Statement.Assert
           {
             Assert.test =
               node
                 ~start:(1, 7)
                 ~stop:(1, 13)
                 (Expression.ComparisonOperator
                    {
                      ComparisonOperator.left =
                        node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
                      operator = ComparisonOperator.Is;
                      right =
                        node ~start:(1, 12) ~stop:(1, 13) (Expression.Name (Name.Identifier "b"));
                    });
             message = None;
             origin = Assert.Origin.Assertion;
           });
    ];
  assert_source_locations
    "assert a, b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Assert
           {
             Assert.test = node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
             message =
               Some (node ~start:(1, 10) ~stop:(1, 11) (Expression.Name (Name.Identifier "b")));
             origin = Assert.Origin.Assertion;
           });
    ];
  assert_source_locations
    "assert a is not None, 'b or c'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 20)
        (Statement.Assert
           {
             Assert.test =
               node
                 ~start:(1, 7)
                 ~stop:(1, 20)
                 (Expression.ComparisonOperator
                    {
                      ComparisonOperator.left =
                        node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
                      operator = ComparisonOperator.IsNot;
                      right =
                        node ~start:(1, 16) ~stop:(1, 20) (Expression.Constant Constant.NoneLiteral);
                    });
             message =
               Some
                 (node
                    ~start:(1, 22)
                    ~stop:(1, 30)
                    (Expression.Constant (Constant.String (StringLiteral.create "b or c"))));
             origin = Assert.Origin.Assertion;
           });
    ]


let test_assign_locations _ =
  assert_source_locations
    "a = 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation = None;
             value = node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 1));
           });
    ];
  assert_source_locations
    "a: int = 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 10)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation =
               Some (node ~start:(1, 3) ~stop:(1, 6) (Expression.Name (Name.Identifier "int")));
             value = node ~start:(1, 9) ~stop:(1, 10) (Expression.Constant (Constant.Integer 1));
           });
    ];
  assert_source_locations
    "a = 1     # type:  int"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation =
               Some
                 (node
                    ~start:(1, 19)
                    ~stop:(1, 22)
                    (Expression.Constant (Constant.String (StringLiteral.create "int"))));
             value = node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 1));
           });
    ];
  assert_source_locations
    "a = 1  # type: 'int'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation =
               Some
                 (node
                    ~start:(1, 15)
                    ~stop:(1, 20)
                    (Expression.Constant (Constant.String (StringLiteral.create "int"))));
             value = node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 1));
           });
    ];
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
             value = node ~start:(1, 6) ~stop:(1, 6) (Expression.Constant Constant.Ellipsis);
           });
    ];
  assert_source_locations
    "a = b = 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation = None;
             value = node ~start:(1, 8) ~stop:(1, 9) (Expression.Constant (Constant.Integer 1));
           });
      node
        ~start:(1, 4)
        ~stop:(1, 9)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "b"));
             annotation = None;
             value = node ~start:(1, 8) ~stop:(1, 9) (Expression.Constant (Constant.Integer 1));
           });
    ];
  assert_source_locations
    "a = yield from b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 16)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation = None;
             value =
               node
                 ~start:(1, 4)
                 ~stop:(1, 16)
                 (Expression.YieldFrom
                    (node ~start:(1, 15) ~stop:(1, 16) (Expression.Name (Name.Identifier "b"))));
           });
    ];
  assert_source_locations
    "a += 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation = None;
             value =
               node
                 ~start:(1, 0)
                 ~stop:(1, 6)
                 (Expression.Call
                    {
                      callee =
                        node
                          ~start:(1, 0)
                          ~stop:(1, 1)
                          (Expression.Name
                             (Name.Attribute
                                {
                                  base =
                                    node
                                      ~start:(1, 0)
                                      ~stop:(1, 1)
                                      (Expression.Name (Name.Identifier "a"));
                                  attribute = "__iadd__";
                                  special = true;
                                }));
                      arguments =
                        [
                          {
                            Call.Argument.name = None;
                            value =
                              node
                                ~start:(1, 5)
                                ~stop:(1, 6)
                                (Expression.Constant (Constant.Integer 1));
                          };
                        ];
                    });
           });
    ]


let test_await_locations _ =
  assert_source_locations
    "await 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Await
                 (node ~start:(1, 6) ~stop:(1, 7) (Expression.Constant (Constant.Integer 1))))));
    ];
  assert_source_locations
    "await   1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 9)
              (Expression.Await
                 (node ~start:(1, 8) ~stop:(1, 9) (Expression.Constant (Constant.Integer 1))))));
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
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
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
                                });
                       };
                     ];
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
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
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
                       };
                     ];
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
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 13)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = false;
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
                   callee = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
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
                   callee = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
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
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 7)
                       (Expression.Call
                          {
                            callee =
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
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 9)
                       (Expression.Call
                          {
                            callee =
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
                   callee =
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
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 12) "a");
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
                   callee =
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
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 17) "second");
                         value =
                           node
                             ~start:(1, 16)
                             ~stop:(1, 17)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
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
                   callee =
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
                         Call.Argument.name = Some (node ~start:(1, 7) ~stop:(2, 1) "second");
                         value =
                           node
                             ~start:(2, 0)
                             ~stop:(2, 1)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
                 })));
    ];
  assert_source_locations
    "x = i[j] = y"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 12)
        (Statement.Assign
           {
             target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "x"));
             annotation = None;
             value = node ~start:(1, 11) ~stop:(1, 12) (Expression.Name (Name.Identifier "y"));
           });
      node
        ~start:(1, 4)
        ~stop:(1, 12)
        (Statement.Expression
           (node
              ~start:(1, 4)
              ~stop:(1, 12)
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 4)
                       ~stop:(1, 8)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 4)
                                   ~stop:(1, 5)
                                   (Expression.Name (Name.Identifier "i"));
                               attribute = "__setitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node ~start:(1, 6) ~stop:(1, 7) (Expression.Name (Name.Identifier "j"));
                       };
                       {
                         Call.Argument.name = None;
                         value =
                           node ~start:(1, 11) ~stop:(1, 12) (Expression.Name (Name.Identifier "y"));
                       };
                     ];
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
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 2)
                             ~stop:(1, 4)
                             (Expression.Call
                                {
                                  callee =
                                    node
                                      ~start:(1, 2)
                                      ~stop:(1, 4)
                                      (Expression.Name (Name.Identifier "slice"));
                                  arguments =
                                    [
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 2)
                                            ~stop:(1, 2)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 3)
                                            ~stop:(1, 4)
                                            (Expression.Constant (Constant.Integer 1));
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 4)
                                            ~stop:(1, 4)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                    ];
                                });
                       };
                     ];
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
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 2)
                             ~stop:(1, 5)
                             (Expression.Call
                                {
                                  callee =
                                    node
                                      ~start:(1, 2)
                                      ~stop:(1, 5)
                                      (Expression.Name (Name.Identifier "slice"));
                                  arguments =
                                    [
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 2)
                                            ~stop:(1, 2)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 3)
                                            ~stop:(1, 3)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 4)
                                            ~stop:(1, 5)
                                            (Expression.Constant (Constant.Integer 2));
                                      };
                                    ];
                                });
                       };
                     ];
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
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Name (Name.Identifier "a"));
                               attribute = "__getitem__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 2)
                             ~stop:(1, 3)
                             (Expression.Call
                                {
                                  callee =
                                    node
                                      ~start:(1, 2)
                                      ~stop:(1, 3)
                                      (Expression.Name (Name.Identifier "slice"));
                                  arguments =
                                    [
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 2)
                                            ~stop:(1, 2)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 3)
                                            ~stop:(1, 3)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                      {
                                        Call.Argument.name = None;
                                        value =
                                          node
                                            ~start:(1, 3)
                                            ~stop:(1, 3)
                                            (Expression.Constant Constant.NoneLiteral);
                                      };
                                    ];
                                });
                       };
                     ];
                 })));
    ]


let test_class_locations _ =
  assert_source_locations
    "@bar\nclass foo():\n\tpass"
    [
      node
        ~start:(2, 0)
        ~stop:(3, 5)
        (Statement.Class
           {
             Class.name = !&"foo";
             base_arguments = [];
             body = [node ~start:(3, 1) ~stop:(3, 5) Statement.Pass];
             decorators =
               [node ~start:(1, 1) ~stop:(1, 4) (Expression.Name (Name.Identifier "bar"))];
             top_level_unbound_names = [];
           });
    ];
  assert_source_locations
    "class foo():\n\tdef bar(): pass"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 16)
        (Statement.Class
           {
             Class.name = !&"foo";
             base_arguments = [];
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 16)
                   (Statement.Define
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
                        body = [node ~start:(2, 12) ~stop:(2, 16) Statement.Pass];
                      });
               ];
             decorators = [];
             top_level_unbound_names = [];
           });
    ];
  assert_source_locations
    "class foo(1, 2):\n\t1"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 2)
        (Statement.Class
           {
             Class.name = !&"foo";
             base_arguments =
               [
                 {
                   Call.Argument.name = None;
                   value =
                     node ~start:(1, 10) ~stop:(1, 11) (Expression.Constant (Constant.Integer 1));
                 };
                 {
                   Call.Argument.name = None;
                   value =
                     node ~start:(1, 13) ~stop:(1, 14) (Expression.Constant (Constant.Integer 2));
                 };
               ];
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Constant (Constant.Integer 1))));
               ];
             decorators = [];
             top_level_unbound_names = [];
           });
    ];
  assert_source_locations
    (trim_extra_indentation
       {|
         class foo():
           if True:
             def bar():
               pass
       |})
    [
      node
        ~start:(2, 0)
        ~stop:(5, 10)
        (Statement.Class
           {
             Class.name = !&"foo";
             base_arguments = [];
             body =
               [
                 node
                   ~start:(3, 2)
                   ~stop:(5, 10)
                   (Statement.If
                      {
                        If.test =
                          node ~start:(3, 5) ~stop:(3, 9) (Expression.Constant Constant.True);
                        body =
                          [
                            node
                              ~start:(4, 4)
                              ~stop:(5, 10)
                              (Statement.Define
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
                                   body = [node ~start:(5, 6) ~stop:(5, 10) Statement.Pass];
                                 });
                          ];
                        orelse = [];
                      });
               ];
             decorators = [];
             top_level_unbound_names = [];
           });
    ]


let test_define_locations _ =
  assert_source_locations
    "async def foo():\n  1"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 3)
        (Statement.Define
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
             body =
               [
                 node
                   ~start:(2, 2)
                   ~stop:(2, 3)
                   (Statement.Expression
                      (node ~start:(2, 2) ~stop:(2, 3) (Expression.Constant (Constant.Integer 1))));
               ];
           });
    ];
  assert_source_locations
    {|
      def foo():
        def bar():
          1
          2
      3
    |}
    [
      node
        ~start:(2, 0)
        ~stop:(5, 5)
        (Statement.Define
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
                 node
                   ~start:(3, 2)
                   ~stop:(5, 5)
                   (Statement.Define
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
                            node
                              ~start:(4, 4)
                              ~stop:(4, 5)
                              (Statement.Expression
                                 (node
                                    ~start:(4, 4)
                                    ~stop:(4, 5)
                                    (Expression.Constant (Constant.Integer 1))));
                            node
                              ~start:(5, 4)
                              ~stop:(5, 5)
                              (Statement.Expression
                                 (node
                                    ~start:(5, 4)
                                    ~stop:(5, 5)
                                    (Expression.Constant (Constant.Integer 2))));
                          ];
                      });
               ];
           });
      node
        ~start:(6, 0)
        ~stop:(6, 1)
        (Statement.Expression
           (node ~start:(6, 0) ~stop:(6, 1) (Expression.Constant (Constant.Integer 3))));
    ];
  assert_source_locations
    {|
      def foo(
        a,  # type: bool
        **kwargs
      ):
        pass
    |}
    [
      node
        ~start:(2, 0)
        ~stop:(6, 6)
        (Statement.Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [
                     node
                       ~start:(3, 2)
                       ~stop:(3, 3)
                       {
                         Parameter.name = "a";
                         value = None;
                         annotation =
                           Some
                             (node
                                ~start:(3, 14)
                                ~stop:(3, 18)
                                (Expression.Constant (Constant.String (StringLiteral.create "bool"))));
                       };
                     node
                       ~start:(4, 2)
                       ~stop:(4, 10)
                       { Parameter.name = "**kwargs"; value = None; annotation = None };
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
             body = [node ~start:(6, 2) ~stop:(6, 6) Statement.Pass];
           });
    ];
  assert_source_locations
    {|
     def foo(self, a, b): # type: (int) -> str
       return 4
    |}
    [
      node
        ~start:(2, 0)
        ~stop:(3, 10)
        (Statement.Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [
                     node
                       ~start:(2, 8)
                       ~stop:(2, 12)
                       { Parameter.name = "self"; value = None; annotation = None };
                     node
                       ~start:(2, 14)
                       ~stop:(2, 15)
                       { Parameter.name = "a"; value = None; annotation = None };
                     node
                       ~start:(2, 17)
                       ~stop:(2, 18)
                       { Parameter.name = "b"; value = None; annotation = None };
                   ];
                 decorators = [];
                 return_annotation =
                   Some
                     (node
                        ~start:(2, 20)
                        ~stop:(2, 41)
                        (Expression.Constant (Constant.String (StringLiteral.create "str"))));
                 async = false;
                 generator = false;
                 parent = None;
                 nesting_define = None;
               };
             captures = [];
             unbound_names = [];
             body =
               [
                 node
                   ~start:(3, 2)
                   ~stop:(3, 10)
                   (Statement.Return
                      {
                        Return.expression =
                          Some
                            (node
                               ~start:(3, 9)
                               ~stop:(3, 10)
                               (Expression.Constant (Constant.Integer 4)));
                        is_implicit = false;
                      });
               ];
           });
    ]


let test_delete_locations _ =
  assert_source_locations
    "del a, b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Delete
           [
             node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "a"));
             node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "b"));
           ]);
    ]


let test_dictionary_locations _ =
  assert_source_locations
    "{}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 2)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 2)
              (Expression.Dictionary { Dictionary.entries = []; keywords = [] })));
    ];
  assert_source_locations
    "{1: 2,}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Dictionary
                 {
                   Dictionary.entries =
                     [
                       {
                         Dictionary.Entry.key =
                           node
                             ~start:(1, 1)
                             ~stop:(1, 2)
                             (Expression.Constant (Constant.Integer 1));
                         value =
                           node
                             ~start:(1, 4)
                             ~stop:(1, 5)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
                   keywords = [];
                 })));
    ];
  assert_source_locations
    "{1: 2, **durp, **hurp}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 22)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 22)
              (Expression.Dictionary
                 {
                   Dictionary.entries =
                     [
                       {
                         Dictionary.Entry.key =
                           node
                             ~start:(1, 1)
                             ~stop:(1, 2)
                             (Expression.Constant (Constant.Integer 1));
                         value =
                           node
                             ~start:(1, 4)
                             ~stop:(1, 5)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
                   keywords =
                     [
                       node ~start:(1, 9) ~stop:(1, 13) (Expression.Name (Name.Identifier "durp"));
                       node ~start:(1, 17) ~stop:(1, 21) (Expression.Name (Name.Identifier "hurp"));
                     ];
                 })));
    ];
  assert_source_locations
    "{\n\t1: 2,\n\t2: 3}"
    [
      node
        ~start:(1, 0)
        ~stop:(3, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(3, 6)
              (Expression.Dictionary
                 {
                   Dictionary.entries =
                     [
                       {
                         Dictionary.Entry.key =
                           node
                             ~start:(2, 1)
                             ~stop:(2, 2)
                             (Expression.Constant (Constant.Integer 1));
                         value =
                           node
                             ~start:(2, 4)
                             ~stop:(2, 5)
                             (Expression.Constant (Constant.Integer 2));
                       };
                       {
                         Dictionary.Entry.key =
                           node
                             ~start:(3, 1)
                             ~stop:(3, 2)
                             (Expression.Constant (Constant.Integer 2));
                         value =
                           node
                             ~start:(3, 4)
                             ~stop:(3, 5)
                             (Expression.Constant (Constant.Integer 3));
                       };
                     ];
                   keywords = [];
                 })));
    ];
  assert_source_locations
    "{a if a else a: b for a in []}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 30)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 30)
              (Expression.DictionaryComprehension
                 {
                   Comprehension.element =
                     {
                       Dictionary.Entry.key =
                         node
                           ~start:(1, 1)
                           ~stop:(1, 14)
                           (Expression.Ternary
                              {
                                Ternary.target =
                                  node
                                    ~start:(1, 1)
                                    ~stop:(1, 2)
                                    (Expression.Name (Name.Identifier "a"));
                                test =
                                  node
                                    ~start:(1, 6)
                                    ~stop:(1, 7)
                                    (Expression.Name (Name.Identifier "a"));
                                alternative =
                                  node
                                    ~start:(1, 13)
                                    ~stop:(1, 14)
                                    (Expression.Name (Name.Identifier "a"));
                              });
                       value =
                         node ~start:(1, 16) ~stop:(1, 17) (Expression.Name (Name.Identifier "b"));
                     };
                   generators =
                     [
                       {
                         Comprehension.Generator.target =
                           node ~start:(1, 22) ~stop:(1, 23) (Expression.Name (Name.Identifier "a"));
                         iterator = node ~start:(1, 27) ~stop:(1, 29) (Expression.List []);
                         conditions = [];
                         async = false;
                       };
                     ];
                 })));
    ]


let test_for_locations _ =
  assert_source_locations
    "for a, b in c: d\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 16)
        (Statement.For
           {
             For.target =
               node
                 ~start:(1, 4)
                 ~stop:(1, 8)
                 (Expression.Tuple
                    [
                      node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "a"));
                      node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "b"));
                    ]);
             iterator = node ~start:(1, 12) ~stop:(1, 13) (Expression.Name (Name.Identifier "c"));
             body =
               [
                 node
                   ~start:(1, 15)
                   ~stop:(1, 16)
                   (Statement.Expression
                      (node ~start:(1, 15) ~stop:(1, 16) (Expression.Name (Name.Identifier "d"))));
               ];
             orelse = [];
             async = false;
           });
    ];
  assert_source_locations
    "for a in b: break\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 17)
        (Statement.For
           {
             For.target = node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "a"));
             iterator = node ~start:(1, 9) ~stop:(1, 10) (Expression.Name (Name.Identifier "b"));
             body = [node ~start:(1, 12) ~stop:(1, 17) Statement.Break];
             orelse = [];
             async = false;
           });
    ];
  assert_source_locations
    "for a in b: continue\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 20)
        (Statement.For
           {
             For.target = node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "a"));
             iterator = node ~start:(1, 9) ~stop:(1, 10) (Expression.Name (Name.Identifier "b"));
             body = [node ~start:(1, 12) ~stop:(1, 20) Statement.Continue];
             orelse = [];
             async = false;
           });
    ];
  assert_source_locations
    "async for a in b: c\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 19)
        (Statement.For
           {
             For.target = node ~start:(1, 10) ~stop:(1, 11) (Expression.Name (Name.Identifier "a"));
             iterator = node ~start:(1, 15) ~stop:(1, 16) (Expression.Name (Name.Identifier "b"));
             body =
               [
                 node
                   ~start:(1, 18)
                   ~stop:(1, 19)
                   (Statement.Expression
                      (node ~start:(1, 18) ~stop:(1, 19) (Expression.Name (Name.Identifier "c"))));
               ];
             orelse = [];
             async = true;
           });
    ];
  assert_source_locations
    "for a in  b :\n\tc\nelse:\n\td\n"
    [
      node
        ~start:(1, 0)
        ~stop:(4, 2)
        (Statement.For
           {
             For.target = node ~start:(1, 4) ~stop:(1, 5) (Expression.Name (Name.Identifier "a"));
             iterator = node ~start:(1, 10) ~stop:(1, 11) (Expression.Name (Name.Identifier "b"));
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "c"))));
               ];
             orelse =
               [
                 node
                   ~start:(4, 1)
                   ~stop:(4, 2)
                   (Statement.Expression
                      (node ~start:(4, 1) ~stop:(4, 2) (Expression.Name (Name.Identifier "d"))));
               ];
             async = false;
           });
    ]


let test_generator_locations _ =
  assert_source_locations
    "(a in b for a in [] if b)"
    [
      node
        ~start:(1, 1)
        ~stop:(1, 24)
        (Statement.Expression
           (node
              ~start:(1, 1)
              ~stop:(1, 24)
              (Expression.Generator
                 {
                   Comprehension.element =
                     node
                       ~start:(1, 1)
                       ~stop:(1, 7)
                       (Expression.ComparisonOperator
                          {
                            ComparisonOperator.left =
                              node
                                ~start:(1, 1)
                                ~stop:(1, 2)
                                (Expression.Name (Name.Identifier "a"));
                            operator = ComparisonOperator.In;
                            right =
                              node
                                ~start:(1, 6)
                                ~stop:(1, 7)
                                (Expression.Name (Name.Identifier "b"));
                          });
                   generators =
                     [
                       {
                         Comprehension.Generator.target =
                           node ~start:(1, 12) ~stop:(1, 13) (Expression.Name (Name.Identifier "a"));
                         iterator = node ~start:(1, 17) ~stop:(1, 19) (Expression.List []);
                         conditions =
                           [
                             node
                               ~start:(1, 23)
                               ~stop:(1, 24)
                               (Expression.Name (Name.Identifier "b"));
                           ];
                         async = false;
                       };
                     ];
                 })));
    ]


let test_global_locations _ =
  assert_source_locations "global a" [node ~start:(1, 0) ~stop:(1, 8) (Statement.Global ["a"])];
  assert_source_locations
    "global a, b"
    [node ~start:(1, 0) ~stop:(1, 11) (Statement.Global ["a"; "b"])]


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
                             });
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


let test_import_locations _ =
  assert_source_locations
    "from a import *"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Statement.Import
           {
             Import.from = Some !&"a";
             imports = [node ~start:(1, 14) ~stop:(1, 15) { Import.name = !&"*"; alias = None }];
           });
    ];
  assert_source_locations
    "from .....foo import b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 22)
        (Statement.Import
           {
             Import.from = Some !&".....foo";
             imports = [node ~start:(1, 21) ~stop:(1, 22) { Import.name = !&"b"; alias = None }];
           });
    ];
  assert_source_locations
    "from a import (b, c)"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 20)
        (Statement.Import
           {
             Import.from = Some !&"a";
             imports =
               [
                 node ~start:(1, 15) ~stop:(1, 16) { Import.name = !&"b"; alias = None };
                 node ~start:(1, 18) ~stop:(1, 19) { Import.name = !&"c"; alias = None };
               ];
           });
    ];
  assert_source_locations
    "from f import a as b, c, d as e"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 31)
        (Statement.Import
           {
             Import.from = Some !&"f";
             imports =
               [
                 node ~start:(1, 14) ~stop:(1, 20) { Import.name = !&"a"; alias = Some "b" };
                 node ~start:(1, 22) ~stop:(1, 23) { Import.name = !&"c"; alias = None };
                 node ~start:(1, 25) ~stop:(1, 31) { Import.name = !&"d"; alias = Some "e" };
               ];
           });
    ];
  assert_source_locations
    "import a as b, c, d as e"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 24)
        (Statement.Import
           {
             Import.from = None;
             imports =
               [
                 node ~start:(1, 7) ~stop:(1, 13) { Import.name = !&"a"; alias = Some "b" };
                 node ~start:(1, 15) ~stop:(1, 16) { Import.name = !&"c"; alias = None };
                 node ~start:(1, 18) ~stop:(1, 24) { Import.name = !&"d"; alias = Some "e" };
               ];
           });
    ]


let test_lambda_locations _ =
  assert_source_locations
    "lambda x = 1, y: x + 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 22)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 22)
              (Expression.Lambda
                 {
                   Lambda.parameters =
                     [
                       node
                         ~start:(1, 7)
                         ~stop:(1, 12)
                         {
                           Parameter.name = "x";
                           value =
                             Some
                               (node
                                  ~start:(1, 11)
                                  ~stop:(1, 12)
                                  (Expression.Constant (Constant.Integer 1)));
                           annotation = None;
                         };
                       node
                         ~start:(1, 14)
                         ~stop:(1, 15)
                         { Parameter.name = "y"; value = None; annotation = None };
                     ];
                   body =
                     node
                       ~start:(1, 17)
                       ~stop:(1, 22)
                       (Expression.Call
                          {
                            callee =
                              node
                                ~start:(1, 17)
                                ~stop:(1, 18)
                                (Expression.Name
                                   (Name.Attribute
                                      {
                                        base =
                                          node
                                            ~start:(1, 17)
                                            ~stop:(1, 18)
                                            (Expression.Name (Name.Identifier "x"));
                                        attribute = "__add__";
                                        special = true;
                                      }));
                            arguments =
                              [
                                {
                                  Call.Argument.name = None;
                                  value =
                                    node
                                      ~start:(1, 21)
                                      ~stop:(1, 22)
                                      (Expression.Constant (Constant.Integer 1));
                                };
                              ];
                          });
                 })));
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
    ];
  assert_source_locations
    "[a for a in a \nfor b in []]"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 12)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(2, 12)
              (Expression.ListComprehension
                 {
                   Comprehension.element =
                     node ~start:(1, 1) ~stop:(1, 2) (Expression.Name (Name.Identifier "a"));
                   generators =
                     [
                       {
                         Comprehension.Generator.target =
                           node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
                         iterator =
                           node ~start:(1, 12) ~stop:(1, 13) (Expression.Name (Name.Identifier "a"));
                         conditions = [];
                         async = false;
                       };
                       {
                         Comprehension.Generator.target =
                           node ~start:(2, 4) ~stop:(2, 5) (Expression.Name (Name.Identifier "b"));
                         iterator = node ~start:(2, 9) ~stop:(2, 11) (Expression.List []);
                         conditions = [];
                         async = false;
                       };
                     ];
                 })));
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
                      base =
                        node
                          ~start:(1, 0)
                          ~stop:(1, 3)
                          (Expression.Name
                             (Name.Attribute
                                {
                                  base =
                                    node
                                      ~start:(1, 0)
                                      ~stop:(1, 1)
                                      (Expression.Name (Name.Identifier "a"));
                                  attribute = "b";
                                  special = false;
                                }));
                      attribute = "c";
                      special = false;
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
                      base = node ~start:(1, 2) ~stop:(1, 3) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      special = false;
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
                      base = node ~start:(1, 1) ~stop:(1, 2) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      special = false;
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
                      base = node ~start:(2, 0) ~stop:(2, 1) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      special = false;
                    }))));
    ];
  assert_source_locations
    "a.b;"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 3)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 3)
              (Expression.Name
                 (Name.Attribute
                    {
                      base = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                      attribute = "b";
                      special = false;
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
                      base =
                        node
                          ~start:(1, 0)
                          ~stop:(1, 6)
                          (Expression.Call
                             {
                               callee =
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
                             });
                      attribute = "b";
                      special = false;
                    }))));
    ]


let test_nonlocal_locations _ =
  assert_source_locations "nonlocal a" [node ~start:(1, 0) ~stop:(1, 10) (Statement.Nonlocal ["a"])];
  assert_source_locations
    "nonlocal a, b"
    [node ~start:(1, 0) ~stop:(1, 13) (Statement.Nonlocal ["a"; "b"])]


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
    "1;"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 1)
        (Statement.Expression
           (node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 1))));
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
                          });
                   operator = BooleanOperator.Or;
                   right =
                     node ~start:(1, 11) ~stop:(1, 12) (Expression.Constant (Constant.Integer 3));
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
                 })));
    ];
  assert_source_locations
    "1 // 2"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 1)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 1)
                                   (Expression.Constant (Constant.Integer 1));
                               attribute = "__floordiv__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 5)
                             ~stop:(1, 6)
                             (Expression.Constant (Constant.Integer 2));
                       };
                     ];
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
                 })));
    ]


let test_raise_locations _ =
  assert_source_locations
    "raise"
    [node ~start:(1, 0) ~stop:(1, 5) (Statement.Raise { Raise.expression = None; from = None })];
  assert_source_locations
    "raise a"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Raise
           {
             Raise.expression =
               Some (node ~start:(1, 6) ~stop:(1, 7) (Expression.Name (Name.Identifier "a")));
             from = None;
           });
    ];
  assert_source_locations
    "raise a from b"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 14)
        (Statement.Raise
           {
             Raise.expression =
               Some (node ~start:(1, 6) ~stop:(1, 7) (Expression.Name (Name.Identifier "a")));
             from = Some (node ~start:(1, 13) ~stop:(1, 14) (Expression.Name (Name.Identifier "b")));
           });
    ]


let test_return_locations _ =
  assert_source_locations
    "return"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Return { Return.expression = None; is_implicit = false });
    ];
  assert_source_locations
    "return 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Return
           {
             Return.expression =
               Some (node ~start:(1, 7) ~stop:(1, 8) (Expression.Constant (Constant.Integer 1)));
             is_implicit = false;
           });
    ]


let test_set_locations _ =
  assert_source_locations
    "{*[1]}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.Set
                 [
                   node
                     ~start:(1, 1)
                     ~stop:(1, 5)
                     (Expression.Starred
                        (Starred.Once
                           (node
                              ~start:(1, 2)
                              ~stop:(1, 5)
                              (Expression.List
                                 [
                                   node
                                     ~start:(1, 3)
                                     ~stop:(1, 4)
                                     (Expression.Constant (Constant.Integer 1));
                                 ]))));
                 ])));
    ];
  assert_source_locations
    "{1, 2,}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Set
                 [
                   node ~start:(1, 1) ~stop:(1, 2) (Expression.Constant (Constant.Integer 1));
                   node ~start:(1, 4) ~stop:(1, 5) (Expression.Constant (Constant.Integer 2));
                 ])));
    ];
  assert_source_locations
    "{1, 1 if 2 else 3}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 18)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 18)
              (Expression.Set
                 [
                   node ~start:(1, 1) ~stop:(1, 2) (Expression.Constant (Constant.Integer 1));
                   node
                     ~start:(1, 4)
                     ~stop:(1, 17)
                     (Expression.Ternary
                        {
                          Ternary.target =
                            node
                              ~start:(1, 4)
                              ~stop:(1, 5)
                              (Expression.Constant (Constant.Integer 1));
                          test =
                            node
                              ~start:(1, 9)
                              ~stop:(1, 10)
                              (Expression.Constant (Constant.Integer 2));
                          alternative =
                            node
                              ~start:(1, 16)
                              ~stop:(1, 17)
                              (Expression.Constant (Constant.Integer 3));
                        });
                 ])));
    ];
  assert_source_locations
    "{a for a in [] if b if c}"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 25)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 25)
              (Expression.SetComprehension
                 {
                   Comprehension.element =
                     node ~start:(1, 1) ~stop:(1, 2) (Expression.Name (Name.Identifier "a"));
                   generators =
                     [
                       {
                         Comprehension.Generator.target =
                           node ~start:(1, 7) ~stop:(1, 8) (Expression.Name (Name.Identifier "a"));
                         iterator = node ~start:(1, 12) ~stop:(1, 14) (Expression.List []);
                         conditions =
                           [
                             node
                               ~start:(1, 18)
                               ~stop:(1, 19)
                               (Expression.Name (Name.Identifier "b"));
                             node
                               ~start:(1, 23)
                               ~stop:(1, 24)
                               (Expression.Name (Name.Identifier "c"));
                           ];
                         async = false;
                       };
                     ];
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
    "b'foo'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")))));
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
    "f'foo'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.FormatString [Substring.Literal (node ~start:(1, 2) ~stop:(1, 5) "foo")])));
    ];
  assert_source_locations
    (* Format string expressions are further parsed in preprocessing. *)
    "f'foo {x}'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 10)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 10)
              (Expression.FormatString
                 [Substring.Literal (node ~start:(1, 2) ~stop:(1, 9) "foo {x}")])));
    ];
  assert_source_locations
    "f'foo' f'bar'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 13)
              (Expression.FormatString
                 [
                   Substring.Literal (node ~start:(1, 2) ~stop:(1, 5) "foo");
                   Substring.Literal (node ~start:(1, 9) ~stop:(1, 12) "bar");
                 ])));
    ];
  assert_source_locations
    "'''a''' + '''b'''"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 17)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 17)
              (Expression.Call
                 {
                   callee =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 7)
                       (Expression.Name
                          (Name.Attribute
                             {
                               base =
                                 node
                                   ~start:(1, 0)
                                   ~stop:(1, 7)
                                   (Expression.Constant (Constant.String (StringLiteral.create "a")));
                               attribute = "__add__";
                               special = true;
                             }));
                   arguments =
                     [
                       {
                         Call.Argument.name = None;
                         value =
                           node
                             ~start:(1, 10)
                             ~stop:(1, 17)
                             (Expression.Constant (Constant.String (StringLiteral.create "b")));
                       };
                     ];
                 })));
    ];

  (* Multiline strings. *)
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
    ];
  assert_source_locations
    "'''\nAAA\nBBB\n'''\npass"
    [
      node
        ~start:(1, 0)
        ~stop:(4, 3)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(4, 3)
              (Expression.Constant (Constant.String (StringLiteral.create "\nAAA\nBBB\n")))));
      node ~start:(5, 0) ~stop:(5, 4) Statement.Pass;
    ]


let test_stub_locations _ =
  assert_source_locations
    "a = ..."
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation = None;
             value = node ~start:(1, 4) ~stop:(1, 7) (Expression.Constant Constant.Ellipsis);
           });
    ];
  assert_source_locations
    "a = ... # type: Tuple[str]"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
             annotation =
               Some
                 (node
                    ~start:(1, 16)
                    ~stop:(1, 26)
                    (Expression.Constant (Constant.String (StringLiteral.create "Tuple[str]"))));
             value = node ~start:(1, 4) ~stop:(1, 7) (Expression.Constant Constant.Ellipsis);
           });
    ];
  assert_source_locations
    "def foo(a): ... # type: ignore"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Statement.Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [
                     node
                       ~start:(1, 8)
                       ~stop:(1, 9)
                       { Parameter.name = "a"; value = None; annotation = None };
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
             body =
               [
                 node
                   ~start:(1, 12)
                   ~stop:(1, 15)
                   (Statement.Expression
                      (node ~start:(1, 12) ~stop:(1, 15) (Expression.Constant Constant.Ellipsis)));
               ];
           });
    ];
  assert_source_locations
    "@overload\ndef foo(a: int = ...):\n\t..."
    [
      node
        ~start:(2, 0)
        ~stop:(3, 4)
        (Statement.Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [
                     node
                       ~start:(2, 8)
                       ~stop:(2, 14)
                       {
                         Parameter.name = "a";
                         value =
                           Some
                             (node
                                ~start:(2, 17)
                                ~stop:(2, 20)
                                (Expression.Constant Constant.Ellipsis));
                         annotation =
                           Some
                             (node
                                ~start:(2, 11)
                                ~stop:(2, 14)
                                (Expression.Name (Name.Identifier "int")));
                       };
                   ];
                 decorators =
                   [node ~start:(1, 1) ~stop:(1, 9) (Expression.Name (Name.Identifier "overload"))];
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
                 node
                   ~start:(3, 1)
                   ~stop:(3, 4)
                   (Statement.Expression
                      (node ~start:(3, 1) ~stop:(3, 4) (Expression.Constant Constant.Ellipsis)));
               ];
           });
    ];
  assert_source_locations
    "class A:\n\ta = ... # type: int"
    [
      node
        ~start:(1, 0)
        ~stop:(2, 8)
        (Statement.Class
           {
             Class.name = !&"A";
             base_arguments = [];
             body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 8)
                   (Statement.Assign
                      {
                        Assign.target =
                          node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "a"));
                        annotation =
                          Some
                            (node
                               ~start:(2, 17)
                               ~stop:(2, 20)
                               (Expression.Constant (Constant.String (StringLiteral.create "int"))));
                        value =
                          node ~start:(2, 5) ~stop:(2, 8) (Expression.Constant Constant.Ellipsis);
                      });
               ];
             decorators = [];
             top_level_unbound_names = [];
           });
    ];
  assert_source_locations
    "class foo(): ... # type: ignore"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 16)
        (Statement.Class
           {
             Class.name = !&"foo";
             base_arguments = [];
             body =
               [
                 node
                   ~start:(1, 13)
                   ~stop:(1, 16)
                   (Statement.Expression
                      (node ~start:(1, 13) ~stop:(1, 16) (Expression.Constant Constant.Ellipsis)));
               ];
             decorators = [];
             top_level_unbound_names = [];
           });
    ]


let test_ternary_locations _ =
  assert_source_locations
    "5 if 1 else 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 13)
              (Expression.Ternary
                 {
                   Ternary.target =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 5));
                   test = node ~start:(1, 5) ~stop:(1, 6) (Expression.Constant (Constant.Integer 1));
                   alternative =
                     node ~start:(1, 12) ~stop:(1, 13) (Expression.Constant (Constant.Integer 1));
                 })));
    ];
  assert_source_locations
    "1 if 2 else 3 if 4 else 5"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 25)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 25)
              (Expression.Ternary
                 {
                   Ternary.target =
                     node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 1));
                   test = node ~start:(1, 5) ~stop:(1, 6) (Expression.Constant (Constant.Integer 2));
                   alternative =
                     node
                       ~start:(1, 12)
                       ~stop:(1, 25)
                       (Expression.Ternary
                          {
                            Ternary.target =
                              node
                                ~start:(1, 12)
                                ~stop:(1, 13)
                                (Expression.Constant (Constant.Integer 3));
                            test =
                              node
                                ~start:(1, 17)
                                ~stop:(1, 18)
                                (Expression.Constant (Constant.Integer 4));
                            alternative =
                              node
                                ~start:(1, 24)
                                ~stop:(1, 25)
                                (Expression.Constant (Constant.Integer 5));
                          });
                 })));
    ]


let test_try_locations _ =
  assert_source_locations
    "try: a"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Try
           {
             Try.body =
               [
                 node
                   ~start:(1, 5)
                   ~stop:(1, 6)
                   (Statement.Expression
                      (node ~start:(1, 5) ~stop:(1, 6) (Expression.Name (Name.Identifier "a"))));
               ];
             handlers = [];
             orelse = [];
             finally = [];
           });
    ];
  assert_source_locations
    "try:\n\ta\nelse:\n\tb"
    [
      node
        ~start:(1, 0)
        ~stop:(4, 2)
        (Statement.Try
           {
             Try.body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "a"))));
               ];
             handlers = [];
             orelse =
               [
                 node
                   ~start:(4, 1)
                   ~stop:(4, 2)
                   (Statement.Expression
                      (node ~start:(4, 1) ~stop:(4, 2) (Expression.Name (Name.Identifier "b"))));
               ];
             finally = [];
           });
    ];
  assert_source_locations
    "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
    [
      node
        ~start:(1, 0)
        ~stop:(6, 2)
        (Statement.Try
           {
             Try.body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "a"))));
               ];
             handlers =
               [
                 {
                   Try.Handler.kind =
                     Some (node ~start:(3, 7) ~stop:(3, 8) (Expression.Name (Name.Identifier "a")));
                   name = Some "b";
                   body =
                     [
                       node
                         ~start:(4, 1)
                         ~stop:(4, 2)
                         (Statement.Expression
                            (node
                               ~start:(4, 1)
                               ~stop:(4, 2)
                               (Expression.Name (Name.Identifier "b"))));
                     ];
                 };
                 {
                   Try.Handler.kind =
                     Some (node ~start:(5, 7) ~stop:(5, 8) (Expression.Name (Name.Identifier "d")));
                   name = None;
                   body =
                     [
                       node
                         ~start:(6, 1)
                         ~stop:(6, 2)
                         (Statement.Expression
                            (node
                               ~start:(6, 1)
                               ~stop:(6, 2)
                               (Expression.Name (Name.Identifier "e"))));
                     ];
                 };
               ];
             orelse = [];
             finally = [];
           });
    ];
  assert_source_locations
    "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
    [
      node
        ~start:(1, 0)
        ~stop:(8, 2)
        (Statement.Try
           {
             Try.body =
               [
                 node
                   ~start:(2, 1)
                   ~stop:(2, 2)
                   (Statement.Expression
                      (node ~start:(2, 1) ~stop:(2, 2) (Expression.Name (Name.Identifier "a"))));
               ];
             handlers =
               [
                 {
                   Try.Handler.kind = None;
                   name = None;
                   body =
                     [
                       node
                         ~start:(4, 1)
                         ~stop:(4, 2)
                         (Statement.Expression
                            (node
                               ~start:(4, 1)
                               ~stop:(4, 2)
                               (Expression.Name (Name.Identifier "b"))));
                     ];
                 };
               ];
             orelse =
               [
                 node
                   ~start:(6, 1)
                   ~stop:(6, 2)
                   (Statement.Expression
                      (node ~start:(6, 1) ~stop:(6, 2) (Expression.Name (Name.Identifier "c"))));
               ];
             finally =
               [
                 node
                   ~start:(8, 1)
                   ~stop:(8, 2)
                   (Statement.Expression
                      (node ~start:(8, 1) ~stop:(8, 2) (Expression.Name (Name.Identifier "d"))));
               ];
           });
    ]


let test_tuple_locations _ =
  assert_source_locations
    {|
      (1, 2) = a
    |}
    [
      node
        ~start:(2, 0)
        ~stop:(2, 10)
        (Statement.Assign
           {
             Assign.target =
               node
                 ~start:(2, 1)
                 ~stop:(2, 5)
                 (Expression.Tuple
                    [
                      node ~start:(2, 1) ~stop:(2, 2) (Expression.Constant (Constant.Integer 1));
                      node ~start:(2, 4) ~stop:(2, 5) (Expression.Constant (Constant.Integer 2));
                    ]);
             annotation = None;
             value = node ~start:(2, 9) ~stop:(2, 10) (Expression.Name (Name.Identifier "a"));
           });
    ];
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
    ];
  assert_source_locations
    "1, 2"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 4)
              (Expression.Tuple
                 [
                   node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 1));
                   node ~start:(1, 3) ~stop:(1, 4) (Expression.Constant (Constant.Integer 2));
                 ])));
    ];
  assert_source_locations
    "1, 1 + 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 8)
              (Expression.Tuple
                 [
                   node ~start:(1, 0) ~stop:(1, 1) (Expression.Constant (Constant.Integer 1));
                   node
                     ~start:(1, 3)
                     ~stop:(1, 8)
                     (Expression.Call
                        {
                          callee =
                            node
                              ~start:(1, 3)
                              ~stop:(1, 4)
                              (Expression.Name
                                 (Name.Attribute
                                    {
                                      base =
                                        node
                                          ~start:(1, 3)
                                          ~stop:(1, 4)
                                          (Expression.Constant (Constant.Integer 1));
                                      attribute = "__add__";
                                      special = true;
                                    }));
                          arguments =
                            [
                              {
                                Call.Argument.name = None;
                                value =
                                  node
                                    ~start:(1, 7)
                                    ~stop:(1, 8)
                                    (Expression.Constant (Constant.Integer 1));
                              };
                            ];
                        });
                 ])));
    ]


let test_while_locations _ =
  assert_source_locations
    "while a:\n\tb\nelse:\n\tc\n"
    [
      node
        ~start:(1, 0)
        ~stop:(4, 2)
        (Statement.While
           {
             While.test = node ~start:(1, 6) ~stop:(1, 7) (Expression.Name (Name.Identifier "a"));
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
    ]


let test_with_locations _ =
  assert_source_locations
    "with (yield from a): b\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 22)
        (Statement.With
           {
             With.items =
               [
                 ( node
                     ~start:(1, 6)
                     ~stop:(1, 18)
                     (Expression.YieldFrom
                        (node ~start:(1, 17) ~stop:(1, 18) (Expression.Name (Name.Identifier "a")))),
                   None );
               ];
             body =
               [
                 node
                   ~start:(1, 21)
                   ~stop:(1, 22)
                   (Statement.Expression
                      (node ~start:(1, 21) ~stop:(1, 22) (Expression.Name (Name.Identifier "b"))));
               ];
             async = false;
           });
    ];
  assert_source_locations
    "async with a: b\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Statement.With
           {
             With.items =
               [node ~start:(1, 11) ~stop:(1, 12) (Expression.Name (Name.Identifier "a")), None];
             body =
               [
                 node
                   ~start:(1, 14)
                   ~stop:(1, 15)
                   (Statement.Expression
                      (node ~start:(1, 14) ~stop:(1, 15) (Expression.Name (Name.Identifier "b"))));
               ];
             async = true;
           });
    ];
  assert_source_locations
    "with a, c as d: b\n"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 17)
        (Statement.With
           {
             With.items =
               [
                 node ~start:(1, 5) ~stop:(1, 6) (Expression.Name (Name.Identifier "a")), None;
                 ( node ~start:(1, 8) ~stop:(1, 9) (Expression.Name (Name.Identifier "c")),
                   Some (node ~start:(1, 13) ~stop:(1, 14) (Expression.Name (Name.Identifier "d")))
                 );
               ];
             body =
               [
                 node
                   ~start:(1, 16)
                   ~stop:(1, 17)
                   (Statement.Expression
                      (node ~start:(1, 16) ~stop:(1, 17) (Expression.Name (Name.Identifier "b"))));
               ];
             async = false;
           });
    ]


let test_walrus_locations _ =
  assert_source_locations
    "a := 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Expression.WalrusOperator
                 {
                   target = node ~start:(1, 0) ~stop:(1, 1) (Expression.Name (Name.Identifier "a"));
                   value =
                     node ~start:(1, 5) ~stop:(1, 6) (Expression.Constant (Constant.Integer 1));
                 })));
    ]


let test_yield_locations _ =
  assert_source_locations
    "yield"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Expression (node ~start:(1, 0) ~stop:(1, 5) (Expression.Yield None)));
    ];
  assert_source_locations
    "yield 1"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 7)
              (Expression.Yield
                 (Some (node ~start:(1, 6) ~stop:(1, 7) (Expression.Constant (Constant.Integer 1)))))));
    ];
  assert_source_locations
    "yield from a"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 12)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 12)
              (Expression.YieldFrom
                 (node ~start:(1, 11) ~stop:(1, 12) (Expression.Name (Name.Identifier "a"))))));
    ]


let () =
  "parsed_locations"
  >::: [
         "assert_locations" >:: test_assert_locations;
         "assign_locations" >:: test_assign_locations;
         "await_locations" >:: test_await_locations;
         "call_locations" >:: test_call_locations;
         "class_locations" >:: test_class_locations;
         "define_locations" >:: test_define_locations;
         "delete_locations" >:: test_delete_locations;
         "dictionary_locations" >:: test_dictionary_locations;
         "for_locations" >:: test_for_locations;
         "generator_locations" >:: test_generator_locations;
         "global_locations" >:: test_global_locations;
         "if_locations" >:: test_if_locations;
         "import_locations" >:: test_import_locations;
         "lambda_locations" >:: test_lambda_locations;
         "list_locations" >:: test_list_locations;
         "name_locations" >:: test_name_locations;
         "nonlocal_locations" >:: test_nonlocal_locations;
         "number_locations" >:: test_number_locations;
         "operator_locations" >:: test_operator_locations;
         "raise_locations" >:: test_raise_locations;
         "return_locations" >:: test_return_locations;
         "set_locations" >:: test_set_locations;
         "string_locations" >:: test_string_locations;
         "stub_locations" >:: test_stub_locations;
         "ternary_locations" >:: test_ternary_locations;
         "try_locations" >:: test_try_locations;
         "tuple_locations" >:: test_tuple_locations;
         "while_locations" >:: test_while_locations;
         "with_locations" >:: test_with_locations;
         "walrus_locations" >:: test_walrus_locations;
         "yield_locations" >:: test_yield_locations;
       ]
  |> Test.run

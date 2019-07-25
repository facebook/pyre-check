(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Expression
open Statement
open Test

let assert_source_locations source statements =
  let parsed_source = parse source in
  let expected_source = { parsed_source with Source.statements } in
  assert_source_equal_with_locations expected_source parsed_source


let test_assert_locations _ =
  assert_source_locations
    "assert a"
    [ node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Assert
           {
             Assert.test = node ~start:(1, 7) ~stop:(1, 8) (Name (Name.Identifier "a"));
             message = None;
             origin = Assert.Assertion;
           }) ];
  assert_source_locations
    "assert a is b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Assert
           {
             Assert.test =
               +ComparisonOperator
                  {
                    ComparisonOperator.left = !"a";
                    operator = ComparisonOperator.Is;
                    right = !"b";
                  };
             message = None;
             origin = Assert.Assertion;
           }) ];
  assert_source_locations
    "assert a, b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Assert
           {
             Assert.test = !"a";
             message = Some (node ~start:(1, 10) ~stop:(1, 11) (Name (Name.Identifier "b")));
             origin = Assert.Assertion;
           }) ];
  assert_source_locations
    "assert a is not None, 'b or c'"
    [ node
        ~start:(1, 0)
        ~stop:(1, 20)
        (Assert
           {
             Assert.test =
               node
                 ~start:(1, 7)
                 ~stop:(1, 20)
                 (ComparisonOperator
                    {
                      ComparisonOperator.left = !"a";
                      operator = ComparisonOperator.IsNot;
                      right = !"None";
                    });
             message =
               Some (node ~start:(1, 22) ~stop:(1, 30) (String (StringLiteral.create "b or c")));
             origin = Assert.Assertion;
           }) ]


let test_assign_locations _ =
  assert_source_locations
    "a = 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Assign
           {
             Assign.target = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
             annotation = None;
             value = node ~start:(1, 4) ~stop:(1, 5) (Integer 1);
             parent = None;
           }) ];
  assert_source_locations
    "a: int = 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 10)
        (Assign
           {
             Assign.target = !"a";
             annotation = Some (node ~start:(1, 3) ~stop:(1, 6) (Name (Name.Identifier "int")));
             value = +Integer 1;
             parent = None;
           }) ];
  assert_source_locations
    "a = 1     # type:  int"
    [ node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Assign
           {
             Assign.target = !"a";
             annotation =
               Some (node ~start:(1, 19) ~stop:(1, 22) (String (StringLiteral.create "int")));
             value = +Integer 1;
             parent = None;
           }) ];
  assert_source_locations
    "a = 1  # type: 'int'"
    [ node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Assign
           {
             Assign.target = !"a";
             annotation =
               Some (node ~start:(1, 15) ~stop:(1, 20) (String (StringLiteral.create "int")));
             value = +Integer 1;
             parent = None;
           }) ];
  assert_source_locations
    "a: int"
    [ node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Assign
           {
             Assign.target = !"a";
             annotation = Some !"int";
             value = node ~start:(1, 6) ~stop:(1, 6) Ellipsis;
             parent = None;
           }) ];
  assert_source_locations
    "a = b = 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Assign { Assign.target = !"a"; annotation = None; value = +Integer 1; parent = None });
      node
        ~start:(1, 4)
        ~stop:(1, 9)
        (Assign { Assign.target = !"b"; annotation = None; value = +Integer 1; parent = None }) ];
  assert_source_locations
    "a = yield from b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 16)
        (Assign
           {
             Assign.target = !"a";
             annotation = None;
             value = +Expression.Yield (Some !"b");
             parent = None;
           }) ];
  assert_source_locations
    "a += 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Assign
           {
             Assign.target = !"a";
             annotation = None;
             value =
               node
                 ~start:(1, 0)
                 ~stop:(1, 6)
                 (Call
                    {
                      callee =
                        +Name
                           (Name.Attribute { base = !"a"; attribute = "__iadd__"; special = true });
                      arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
                    });
             parent = None;
           }) ]


let test_await_locations _ =
  assert_source_locations
    "await 1"
    [+Expression (node ~start:(1, 0) ~stop:(1, 7) (Await (+Integer 1)))];
  assert_source_locations
    "await   1"
    [+Expression (node ~start:(1, 0) ~stop:(1, 9) (Await (+Integer 1)))]


let test_call_locations _ =
  assert_source_locations
    "a[1 < 2]"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 8)
            (Call
               {
                 callee =
                   node
                     ~start:(1, 0)
                     ~stop:(1, 1)
                     (Name
                        (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true }));
                 arguments =
                   [ {
                       Call.Argument.name = None;
                       value =
                         node
                           ~start:(1, 2)
                           ~stop:(1, 7)
                           (ComparisonOperator
                              {
                                ComparisonOperator.left = +Integer 1;
                                operator = ComparisonOperator.LessThan;
                                right = +Integer 2;
                              });
                     } ];
               })) ];
  assert_source_locations
    "a.__getitem__(argument)"
    [ +Expression
         (+Call
             {
               callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 13)
                   (Name
                      (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = false }));
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 14) ~stop:(1, 22) (Name (Name.Identifier "argument"));
                   } ];
             }) ];
  assert_source_locations
    "a((1, 2))"
    [ +Expression
         (+Call
             {
               callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 3) ~stop:(1, 7) (Tuple [+Integer 1; +Integer 2]);
                   } ];
             }) ];
  assert_source_locations
    "a(arg1,  arg2,)"
    [ +Expression
         (+Call
             {
               callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 2) ~stop:(1, 6) (Name (Name.Identifier "arg1"));
                   };
                   {
                     Call.Argument.name = None;
                     value = node ~start:(1, 9) ~stop:(1, 13) (Name (Name.Identifier "arg2"));
                   } ];
             }) ];
  assert_source_locations
    "a(arg1)(arg2)"
    [ +Expression
         (+Call
             {
               callee =
                 +Call
                    {
                      callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                      arguments =
                        [ {
                            Call.Argument.name = None;
                            value = node ~start:(1, 2) ~stop:(1, 6) (Name (Name.Identifier "arg1"));
                          } ];
                    };
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 8) ~stop:(1, 12) (Name (Name.Identifier "arg2"));
                   } ];
             }) ];
  assert_source_locations
    "a(  arg1)((arg2)  )"
    [ +Expression
         (+Call
             {
               callee =
                 +Call
                    {
                      callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                      arguments =
                        [ {
                            Call.Argument.name = None;
                            value = node ~start:(1, 4) ~stop:(1, 8) (Name (Name.Identifier "arg1"));
                          } ];
                    };
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 11) ~stop:(1, 15) (Name (Name.Identifier "arg2"));
                   } ];
             }) ];
  assert_source_locations
    "foo(1, a = 2, *args, **kwargs)"
    [ +Expression
         (+Call
             {
               callee = !"foo";
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   { Call.Argument.name = Some ~+"a"; value = +Integer 2 };
                   {
                     Call.Argument.name = None;
                     value = node ~start:(1, 14) ~stop:(1, 19) (Starred (Starred.Once !"args"));
                   };
                   {
                     Call.Argument.name = None;
                     value = node ~start:(1, 21) ~stop:(1, 29) (Starred (Starred.Twice !"kwargs"));
                   } ];
             }) ];
  assert_source_locations
    "foo(1, second = 2)"
    [ +Expression
         (+Call
             {
               callee = +Name (Name.Identifier "foo");
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   {
                     Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                     value = node ~start:(1, 16) ~stop:(1, 17) (Integer 2);
                   } ];
             }) ];
  assert_source_locations
    "foo(1, second = \n2)"
    [ +Expression
         (+Call
             {
               callee = +Name (Name.Identifier "foo");
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   {
                     Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                     value = node ~start:(2, 0) ~stop:(2, 1) (Integer 2);
                   } ];
             }) ];
  assert_source_locations
    "x = i[j] = y"
    [ node
        ~start:(1, 0)
        ~stop:(1, 12)
        (Assign { target = !"x"; annotation = None; value = !"y"; parent = None });
      +Expression
         (node
            ~start:(1, 4)
            ~stop:(1, 12)
            (Call
               {
                 callee =
                   node
                     ~start:(1, 4)
                     ~stop:(1, 8)
                     (Name
                        (Name.Attribute { base = !"i"; attribute = "__setitem__"; special = true }));
                 arguments =
                   [ {
                       Call.Argument.name = None;
                       value = node ~start:(1, 6) ~stop:(1, 7) (Name (Name.Identifier "j"));
                     };
                     {
                       Call.Argument.name = None;
                       value = node ~start:(1, 11) ~stop:(1, 12) (Name (Name.Identifier "y"));
                     } ];
               })) ];
  assert_source_locations
    "a[:1]"
    [ +Expression
         (+Call
             {
               callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 1)
                   (Name
                      (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true }));
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value =
                       node
                         ~start:(1, 2)
                         ~stop:(1, 4)
                         (Call
                            {
                              callee = !"slice";
                              arguments =
                                [ {
                                    Call.Argument.name = None;
                                    value =
                                      node
                                        ~start:(1, 2)
                                        ~stop:(1, 2)
                                        (Name (Name.Identifier "None"));
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value = node ~start:(1, 3) ~stop:(1, 4) (Integer 1);
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value =
                                      node
                                        ~start:(1, 4)
                                        ~stop:(1, 4)
                                        (Name (Name.Identifier "None"));
                                  } ];
                            });
                   } ];
             }) ];
  assert_source_locations
    "a[::2]"
    [ +Expression
         (+Call
             {
               callee =
                 +Name (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true });
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value =
                       node
                         ~start:(1, 2)
                         ~stop:(1, 5)
                         (Call
                            {
                              callee = !"slice";
                              arguments =
                                [ {
                                    Call.Argument.name = None;
                                    value =
                                      node
                                        ~start:(1, 2)
                                        ~stop:(1, 2)
                                        (Name (Name.Identifier "None"));
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value =
                                      node
                                        ~start:(1, 3)
                                        ~stop:(1, 3)
                                        (Name (Name.Identifier "None"));
                                  };
                                  {
                                    Call.Argument.name = None;
                                    value = node ~start:(1, 4) ~stop:(1, 5) (Integer 2);
                                  } ];
                            });
                   } ];
             }) ]


let test_class_locations _ =
  assert_source_locations
    "@bar\nclass foo():\n\tpass"
    [ node
        ~start:(2, 0)
        ~stop:(3, 5)
        (Class
           {
             Class.name = !&"foo";
             bases = [];
             body = [+Pass];
             decorators = [node ~start:(1, 1) ~stop:(1, 4) (Name (Name.Identifier "bar"))];
             docstring = None;
           }) ];
  assert_source_locations
    "class foo():\n\tdef bar(): pass"
    [ node
        ~start:(1, 0)
        ~stop:(2, 16)
        (Class
           {
             Class.name = !&"foo";
             bases = [];
             body =
               [ node
                   ~start:(2, 1)
                   ~stop:(2, 16)
                   (Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            docstring = None;
                            return_annotation = None;
                            async = false;
                            parent = Some !&"foo";
                          };
                        body = [+Pass];
                      }) ];
             decorators = [];
             docstring = None;
           }) ];
  assert_source_locations
    "class foo(1, 2):\n\t1"
    [ +Class
         {
           Class.name = !&"foo";
           bases =
             [ {
                 Expression.Call.Argument.name = None;
                 value = node ~start:(1, 10) ~stop:(1, 11) (Integer 1);
               };
               {
                 Expression.Call.Argument.name = None;
                 value = node ~start:(1, 13) ~stop:(1, 14) (Integer 2);
               } ];
           body = [+Expression (+Integer 1)];
           decorators = [];
           docstring = None;
         } ];
  assert_source_locations
    (trim_extra_indentation
       {|
         class foo():
           if True:
             def bar():
               pass
       |})
    [ +Class
         {
           Class.name = !&"foo";
           bases = [];
           body =
             [ node
                 ~start:(3, 2)
                 ~stop:(5, 10)
                 (If
                    {
                      If.test = +Expression.True;
                      body =
                        [ node
                            ~start:(4, 4)
                            ~stop:(5, 10)
                            (Define
                               {
                                 signature =
                                   {
                                     name = !&"bar";
                                     parameters = [];
                                     decorators = [];
                                     docstring = None;
                                     return_annotation = None;
                                     async = false;
                                     parent = Some !&"foo";
                                   };
                                 body = [+Pass];
                               }) ];
                      orelse = [];
                    }) ];
           decorators = [];
           docstring = None;
         } ]


let test_define_locations _ =
  assert_source_locations
    "async def foo():\n  1"
    [ node
        ~start:(1, 0)
        ~stop:(2, 3)
        (Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters = [];
                 decorators = [];
                 docstring = None;
                 return_annotation = None;
                 async = true;
                 parent = None;
               };
             body = [node ~start:(2, 2) ~stop:(2, 3) (Expression (+Integer 1))];
           }) ];
  assert_source_locations
    {|
      def foo():
        def bar():
          1
          2
      3
    |}
    [ node
        ~start:(2, 0)
        ~stop:(5, 5)
        (Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters = [];
                 decorators = [];
                 docstring = None;
                 return_annotation = None;
                 async = false;
                 parent = None;
               };
             body =
               [ node
                   ~start:(3, 2)
                   ~stop:(5, 5)
                   (Define
                      {
                        signature =
                          {
                            name = !&"bar";
                            parameters = [];
                            decorators = [];
                            docstring = None;
                            return_annotation = None;
                            async = false;
                            parent = None;
                          };
                        body = [+Expression (+Integer 1); +Expression (+Integer 2)];
                      }) ];
           });
      node ~start:(6, 0) ~stop:(6, 1) (Expression (+Integer 3)) ];
  assert_source_locations
    {|
      def foo(
        a,  # type: bool
        **kwargs
      ):
        pass
    |}
    [ node
        ~start:(2, 0)
        ~stop:(6, 6)
        (Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [ +{
                        Parameter.name = "a";
                        value = None;
                        annotation = Some (+String (StringLiteral.create "bool"));
                      };
                     +{ Parameter.name = "**kwargs"; value = None; annotation = None } ];
                 decorators = [];
                 docstring = None;
                 return_annotation = None;
                 async = false;
                 parent = None;
               };
             body = [+Pass];
           }) ];
  assert_source_locations
    {|
     def foo(self, a, b): # type: (int) -> str
       return 4
    |}
    [ +Define
         {
           signature =
             {
               name = !&"foo";
               parameters =
                 [ +{ Parameter.name = "self"; value = None; annotation = None };
                   +{ Parameter.name = "a"; value = None; annotation = None };
                   +{ Parameter.name = "b"; value = None; annotation = None } ];
               decorators = [];
               docstring = None;
               return_annotation =
                 Some (node ~start:(2, 20) ~stop:(2, 41) (String (StringLiteral.create "str")));
               async = false;
               parent = None;
             };
           body = [+Return { Return.expression = Some (+Integer 4); is_implicit = false }];
         } ]


let test_delete_locations _ =
  assert_source_locations
    "del a, b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Delete (node ~start:(1, 4) ~stop:(1, 8) (Tuple [!"a"; !"b"]))) ]


let test_dictionary_locations _ =
  assert_source_locations
    "{}"
    [ +Expression
         (node ~start:(1, 0) ~stop:(1, 2) (Dictionary { Dictionary.entries = []; keywords = [] }))
    ];
  assert_source_locations
    "{1: 2,}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 7)
            (Dictionary
               {
                 Dictionary.entries = [{ Dictionary.key = +Integer 1; value = +Integer 2 }];
                 keywords = [];
               })) ];
  assert_source_locations
    "{1: 2, **durp, **hurp}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 22)
            (Dictionary
               {
                 Dictionary.entries = [{ Dictionary.key = +Integer 1; value = +Integer 2 }];
                 keywords =
                   [ node ~start:(1, 9) ~stop:(1, 13) (Name (Name.Identifier "durp"));
                     node ~start:(1, 17) ~stop:(1, 21) (Name (Name.Identifier "hurp")) ];
               })) ];
  assert_source_locations
    "{\n\t1: 2,\n\t2: 3}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(3, 6)
            (Dictionary
               {
                 Dictionary.entries =
                   [ { Dictionary.key = +Integer 1; value = +Integer 2 };
                     { Dictionary.key = +Integer 2; value = +Integer 3 } ];
                 keywords = [];
               })) ];
  assert_source_locations
    "{a if a else a: b for a in []}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 30)
            (DictionaryComprehension
               {
                 Comprehension.element =
                   {
                     Dictionary.key =
                       node
                         ~start:(1, 1)
                         ~stop:(1, 14)
                         (Ternary { Ternary.target = !"a"; test = !"a"; alternative = !"a" });
                     value = !"b";
                   };
                 generators =
                   [ {
                       Comprehension.target = !"a";
                       iterator = +List [];
                       conditions = [];
                       async = false;
                     } ];
               })) ]


let test_for_locations _ =
  assert_source_locations
    "for a, b in c: d\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 16)
        (For
           {
             For.target = node ~start:(1, 4) ~stop:(1, 8) (Tuple [!"a"; !"b"]);
             iterator = node ~start:(1, 12) ~stop:(1, 13) (Name (Name.Identifier "c"));
             body = [node ~start:(1, 15) ~stop:(1, 16) (Expression !"d")];
             orelse = [];
             async = false;
           }) ];
  assert_source_locations
    "for a in b: break\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 17)
        (For
           {
             For.target = !"a";
             iterator = !"b";
             body = [node ~start:(1, 12) ~stop:(1, 17) Break];
             orelse = [];
             async = false;
           }) ];
  assert_source_locations
    "for a in b: continue\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 20)
        (For
           {
             For.target = !"a";
             iterator = !"b";
             body = [node ~start:(1, 12) ~stop:(1, 20) Continue];
             orelse = [];
             async = false;
           }) ];
  assert_source_locations
    "async for a in b: c\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 19)
        (For
           {
             For.target = !"a";
             iterator = !"b";
             body = [+Expression !"c"];
             orelse = [];
             async = true;
           }) ];
  assert_source_locations
    "for a in  b :\n\tc\nelse:\n\td\n"
    [ node
        ~start:(1, 0)
        ~stop:(4, 2)
        (For
           {
             For.target = !"a";
             iterator = node ~start:(1, 10) ~stop:(1, 11) (Name (Name.Identifier "b"));
             body = [+Expression !"c"];
             orelse = [node ~start:(4, 1) ~stop:(4, 2) (Expression !"d")];
             async = false;
           }) ]


let test_generator_locations _ =
  assert_source_locations
    "(a in b for a in [] if b)"
    [ +Expression
         (node
            ~start:(1, 1)
            ~stop:(1, 24)
            (Generator
               {
                 Comprehension.element =
                   node
                     ~start:(1, 1)
                     ~stop:(1, 7)
                     (ComparisonOperator
                        {
                          ComparisonOperator.left = !"a";
                          operator = ComparisonOperator.In;
                          right = !"b";
                        });
                 generators =
                   [ {
                       Comprehension.target = !"a";
                       iterator = +List [];
                       conditions = [!"b"];
                       async = false;
                     } ];
               })) ]


let test_global_locations _ =
  assert_source_locations "global a" [node ~start:(1, 0) ~stop:(1, 8) (Global ["a"])];
  assert_source_locations "global a, b" [node ~start:(1, 0) ~stop:(1, 11) (Global ["a"; "b"])]


let test_if_locations _ =
  assert_source_locations
    "if a : b\nelif c: d"
    [ node
        ~start:(1, 0)
        ~stop:(2, 9)
        (If
           {
             If.test = node ~start:(1, 3) ~stop:(1, 4) (Name (Name.Identifier "a"));
             body = [node ~start:(1, 7) ~stop:(1, 8) (Expression !"b")];
             orelse =
               [ +If
                    {
                      If.test = node ~start:(2, 5) ~stop:(2, 6) (Name (Name.Identifier "c"));
                      body = [node ~start:(2, 8) ~stop:(2, 9) (Expression !"d")];
                      orelse = [];
                    } ];
           }) ];
  assert_source_locations
    "if a:\n\n\tb\n"
    [ node
        ~start:(1, 0)
        ~stop:(3, 2)
        (If
           {
             If.test = !"a";
             body = [node ~start:(3, 1) ~stop:(3, 2) (Expression !"b")];
             orelse = [];
           }) ];
  assert_source_locations
    "if a:\n\tb\nelse:\n\tc\n"
    [ node
        ~start:(1, 0)
        ~stop:(4, 2)
        (If
           {
             If.test = !"a";
             body = [+Expression !"b"];
             orelse = [node ~start:(4, 1) ~stop:(4, 2) (Expression !"c")];
           }) ];
  assert_source_locations
    "if a is 1 or b == 1:\n\tc"
    [ node
        ~start:(1, 0)
        ~stop:(2, 2)
        (If
           {
             If.test =
               node
                 ~start:(1, 3)
                 ~stop:(1, 19)
                 (BooleanOperator
                    {
                      BooleanOperator.left =
                        node
                          ~start:(1, 3)
                          ~stop:(1, 9)
                          (ComparisonOperator
                             {
                               ComparisonOperator.left = !"a";
                               operator = ComparisonOperator.Is;
                               right = +Integer 1;
                             });
                      operator = BooleanOperator.Or;
                      right =
                        node
                          ~start:(1, 13)
                          ~stop:(1, 19)
                          (ComparisonOperator
                             {
                               ComparisonOperator.left = !"b";
                               operator = ComparisonOperator.Equals;
                               right = +Integer 1;
                             });
                    });
             body = [+Expression !"c"];
             orelse = [];
           }) ]


let test_import_locations _ =
  assert_source_locations
    "from a import *"
    [ node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Import { Import.from = Some !&"a"; imports = [{ Import.name = !&"*"; alias = None }] }) ];
  assert_source_locations
    "from .....foo import b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 22)
        (Import
           { Import.from = Some !&".....foo"; imports = [{ Import.name = !&"b"; alias = None }] })
    ];
  assert_source_locations
    "from a import (b, c)"
    [ node
        ~start:(1, 0)
        ~stop:(1, 20)
        (Import
           {
             Import.from = Some !&"a";
             imports =
               [{ Import.name = !&"b"; alias = None }; { Import.name = !&"c"; alias = None }];
           }) ];
  assert_source_locations
    "from f import a as b, c, d as e"
    [ node
        ~start:(1, 0)
        ~stop:(1, 31)
        (Import
           {
             Import.from = Some !&"f";
             imports =
               [ { Import.name = !&"a"; alias = Some !&"b" };
                 { Import.name = !&"c"; alias = None };
                 { Import.name = !&"d"; alias = Some !&"e" } ];
           }) ];
  assert_source_locations
    "import a as b, c, d as e"
    [ node
        ~start:(1, 0)
        ~stop:(1, 24)
        (Import
           {
             Import.from = None;
             imports =
               [ { Import.name = !&"a"; alias = Some !&"b" };
                 { Import.name = !&"c"; alias = None };
                 { Import.name = !&"d"; alias = Some !&"e" } ];
           }) ]


let test_lambda_locations _ =
  assert_source_locations
    "lambda x = 1, y: x + 1"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 22)
            (Lambda
               {
                 Lambda.parameters =
                   [ node
                       ~start:(1, 7)
                       ~stop:(1, 12)
                       { Parameter.name = "x"; value = Some (+Integer 1); annotation = None };
                     node
                       ~start:(1, 14)
                       ~stop:(1, 15)
                       { Parameter.name = "y"; value = None; annotation = None } ];
                 body =
                   +Call
                      {
                        callee =
                          +Name
                             (Name.Attribute
                                {
                                  base = +Name (Name.Identifier "x");
                                  attribute = "__add__";
                                  special = true;
                                });
                        arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
                      };
               })) ]


let test_list_locations _ =
  assert_source_locations
    "[[ ] ]"
    [ node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Expression (+List [node ~start:(1, 1) ~stop:(1, 4) (List [])])) ];
  assert_source_locations
    "[1, 2,]"
    [+Expression (node ~start:(1, 0) ~stop:(1, 7) (List [+Integer 1; +Integer 2]))];
  assert_source_locations
    "[a for a in a \nfor b in []]"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(2, 12)
            (ListComprehension
               {
                 Comprehension.element =
                   node ~start:(1, 1) ~stop:(1, 2) (Name (Name.Identifier "a"));
                 generators =
                   [ {
                       Comprehension.target =
                         node ~start:(1, 7) ~stop:(1, 8) (Name (Name.Identifier "a"));
                       iterator = !"a";
                       conditions = [];
                       async = false;
                     };
                     {
                       Comprehension.target =
                         node ~start:(2, 4) ~stop:(2, 5) (Name (Name.Identifier "b"));
                       iterator = node ~start:(2, 9) ~stop:(2, 11) (List []);
                       conditions = [];
                       async = false;
                     } ];
               })) ]


let test_name_locations _ =
  assert_source_locations
    "a.b.c"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 5)
            (Name
               (Name.Attribute
                  {
                    base =
                      node
                        ~start:(1, 0)
                        ~stop:(1, 3)
                        (Name
                           (Name.Attribute
                              {
                                base = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                                attribute = "b";
                                special = false;
                              }));
                    attribute = "c";
                    special = false;
                  }))) ];
  assert_source_locations
    "((a)).b"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 7)
            (Name
               (Name.Attribute
                  {
                    base = node ~start:(1, 2) ~stop:(1, 3) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false;
                  }))) ];
  assert_source_locations
    "(a  \n).b"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(2, 3)
            (Name
               (Name.Attribute
                  {
                    base = node ~start:(1, 1) ~stop:(1, 2) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false;
                  }))) ];
  assert_source_locations
    {|
      a. \
      b
    |}
    [ +Expression
         (node
            ~start:(2, 0)
            ~stop:(3, 1)
            (Name
               (Name.Attribute
                  {
                    base = node ~start:(2, 0) ~stop:(2, 1) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false;
                  }))) ];
  assert_source_locations
    "a.b;"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 3)
            (Name
               (Name.Attribute
                  {
                    base = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false;
                  }))) ];
  assert_source_locations
    "a(arg).b"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 8)
            (Name
               (Name.Attribute
                  {
                    base =
                      node
                        ~start:(1, 0)
                        ~stop:(1, 6)
                        (Call
                           {
                             callee = !"a";
                             arguments = [{ Call.Argument.name = None; value = !"arg" }];
                           });
                    attribute = "b";
                    special = false;
                  }))) ]


let test_nonlocal_locations _ =
  assert_source_locations "nonlocal a" [node ~start:(1, 0) ~stop:(1, 10) (Nonlocal ["a"])];
  assert_source_locations "nonlocal a, b" [node ~start:(1, 0) ~stop:(1, 13) (Nonlocal ["a"; "b"])]


let test_number_locations _ =
  assert_source_locations
    "((1))"
    [node ~start:(1, 0) ~stop:(1, 5) (Expression (node ~start:(1, 2) ~stop:(1, 3) (Integer 1)))];
  assert_source_locations "((1))" [+Expression (node ~start:(1, 2) ~stop:(1, 3) (Integer 1))];
  assert_source_locations "1;" [+Expression (node ~start:(1, 0) ~stop:(1, 1) (Integer 1))];
  assert_source_locations ".1" [+Expression (node ~start:(1, 0) ~stop:(1, 2) (Float 0.1))];
  assert_source_locations "1." [+Expression (node ~start:(1, 0) ~stop:(1, 2) (Float 1.0))];
  assert_source_locations "1e10" [+Expression (node ~start:(1, 0) ~stop:(1, 4) (Float 1e10))];
  assert_source_locations "0.1j" [+Expression (node ~start:(1, 0) ~stop:(1, 4) (Complex 0.1))];
  assert_source_locations "1L" [+Expression (node ~start:(1, 0) ~stop:(1, 2) (Integer 1))];
  assert_source_locations
    "-(1)"
    [ node
        ~start:(1, 0)
        ~stop:(1, 4)
        (Expression
           (+UnaryOperator
               { UnaryOperator.operator = UnaryOperator.Negative; operand = +Integer 1 })) ]


let test_operator_locations _ =
  assert_source_locations
    "1 and 2 or 3"
    [ node
        ~start:(1, 0)
        ~stop:(1, 12)
        (Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 12)
              (BooleanOperator
                 {
                   BooleanOperator.left =
                     node
                       ~start:(1, 0)
                       ~stop:(1, 7)
                       (BooleanOperator
                          {
                            BooleanOperator.left = +Integer 1;
                            operator = BooleanOperator.And;
                            right = +Integer 2;
                          });
                   operator = BooleanOperator.Or;
                   right = node ~start:(1, 11) ~stop:(1, 12) (Integer 3);
                 }))) ];
  assert_source_locations
    "1 is not 1"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 10)
            (ComparisonOperator
               {
                 ComparisonOperator.left = node ~start:(1, 0) ~stop:(1, 1) (Integer 1);
                 operator = ComparisonOperator.IsNot;
                 right = node ~start:(1, 9) ~stop:(1, 10) (Integer 1);
               })) ];
  assert_source_locations
    "1 // 2"
    [ +Expression
         (+Call
             {
               callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 1)
                   (Name
                      (Name.Attribute
                         { base = +Integer 1; attribute = "__floordiv__"; special = true }));
               arguments = [{ Call.Argument.name = None; value = +Integer 2 }];
             }) ];
  assert_source_locations
    "not 1"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 5)
            (UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +Integer 1 }))
    ]


let test_raise_locations _ =
  assert_source_locations
    "raise"
    [node ~start:(1, 0) ~stop:(1, 5) (Raise { Raise.expression = None; from = None })];
  assert_source_locations
    "raise a"
    [ node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Raise
           {
             Raise.expression = Some (node ~start:(1, 6) ~stop:(1, 7) (Name (Name.Identifier "a")));
             from = None;
           }) ];
  assert_source_locations
    "raise a from b"
    [ node
        ~start:(1, 0)
        ~stop:(1, 14)
        (Raise
           {
             Raise.expression = Some (node ~start:(1, 6) ~stop:(1, 7) (Name (Name.Identifier "a")));
             from = Some (node ~start:(1, 13) ~stop:(1, 14) (Name (Name.Identifier "b")));
           }) ]


let test_return_locations _ =
  assert_source_locations
    "return"
    [node ~start:(1, 0) ~stop:(1, 6) (Return { Return.expression = None; is_implicit = false })];
  assert_source_locations
    "return 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Return
           {
             Return.expression = Some (node ~start:(1, 7) ~stop:(1, 8) (Integer 1));
             is_implicit = false;
           }) ]


let test_set_locations _ =
  assert_source_locations
    "{*[1]}"
    [ node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 6)
              (Set [node ~start:(1, 1) ~stop:(1, 5) (Starred (Starred.Once (+List [+Integer 1])))])))
    ];
  assert_source_locations "{1, 2,}" [+Expression (+Set [+Integer 1; +Integer 2])];
  assert_source_locations
    "{1, 1 if 2 else 3}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 18)
            (Set
               [ node ~start:(1, 1) ~stop:(1, 2) (Integer 1);
                 node
                   ~start:(1, 4)
                   ~stop:(1, 17)
                   (Ternary
                      { Ternary.target = +Integer 1; test = +Integer 2; alternative = +Integer 3 })
               ])) ];
  assert_source_locations
    "{a for a in [] if b if c}"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 25)
            (SetComprehension
               {
                 Comprehension.element = !"a";
                 generators =
                   [ {
                       Comprehension.target = !"a";
                       iterator = +List [];
                       conditions = [!"b"; !"c"];
                       async = false;
                     } ];
               })) ]


let test_string_locations _ =
  assert_source_locations
    "'foo'"
    [ node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Expression (node ~start:(1, 0) ~stop:(1, 5) (String (StringLiteral.create "foo")))) ];
  assert_source_locations
    "'''foo'''"
    [+Expression (node ~start:(1, 0) ~stop:(1, 9) (String (StringLiteral.create "foo")))];
  assert_source_locations
    "b'foo'"
    [ +Expression
         (node ~start:(1, 0) ~stop:(1, 6) (String (StringLiteral.create ~bytes:true "foo"))) ];
  assert_source_locations
    "'foo' 'bar'"
    [+Expression (node ~start:(1, 0) ~stop:(1, 11) (String (StringLiteral.create "foobar")))];
  assert_source_locations
    "ur'foo'"
    [+Expression (node ~start:(1, 0) ~stop:(1, 7) (String (StringLiteral.create "foo")))];
  assert_source_locations
    "f'foo'"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 6)
            (String
               (StringLiteral.create_mixed
                  [ node
                      ~start:(1, 2)
                      ~stop:(1, 5)
                      { StringLiteral.Substring.kind = Format; value = "foo" } ]))) ];
  assert_source_locations
    (* Format string expressions are further parsed in preprocessing. *)
    "f'foo {x}'"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 10)
            (String
               (StringLiteral.create_mixed
                  [ node
                      ~start:(1, 2)
                      ~stop:(1, 9)
                      { StringLiteral.Substring.kind = Format; value = "foo {x}" } ]))) ];
  assert_source_locations
    "f'foo' f'bar'"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 13)
            (String
               (StringLiteral.create_mixed
                  [ node
                      ~start:(1, 2)
                      ~stop:(1, 5)
                      { StringLiteral.Substring.kind = Format; value = "foo" };
                    node
                      ~start:(1, 9)
                      ~stop:(1, 12)
                      { StringLiteral.Substring.kind = Format; value = "bar" } ]))) ];
  assert_source_locations
    "'''a''' + '''b'''"
    [ +Expression
         (+Call
             {
               callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 7)
                   (Name
                      (Name.Attribute
                         {
                           base =
                             node ~start:(1, 0) ~stop:(1, 7) (String (StringLiteral.create "a"));
                           attribute = "__add__";
                           special = true;
                         }));
               arguments =
                 [ {
                     Call.Argument.name = None;
                     value = node ~start:(1, 10) ~stop:(1, 17) (String (StringLiteral.create "b"));
                   } ];
             }) ];

  (* Multiline strings. *)
  assert_source_locations
    "'''multiline\nliteral'''\n"
    [ node
        ~start:(1, 0)
        ~stop:(2, 10)
        (Expression
           (node ~start:(1, 0) ~stop:(2, 10) (String (StringLiteral.create "multiline\nliteral"))))
    ];
  assert_source_locations
    "\"\"\"multiline\nliteral\"\"\"\n"
    [ node
        ~start:(1, 0)
        ~stop:(2, 10)
        (Expression
           (node ~start:(1, 0) ~stop:(2, 10) (String (StringLiteral.create "multiline\nliteral"))))
    ];
  assert_source_locations
    "'''\nAAA\nBBB\n'''\npass"
    [ node
        ~start:(1, 0)
        ~stop:(4, 3)
        (Expression
           (node ~start:(1, 0) ~stop:(4, 3) (String (StringLiteral.create "\nAAA\nBBB\n"))));
      node ~start:(5, 0) ~stop:(5, 4) Pass ]


let test_stub_locations _ =
  assert_source_locations
    "a = ..."
    [ node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Assign
           {
             Assign.target = !"a";
             annotation = None;
             value = node ~start:(1, 4) ~stop:(1, 7) Ellipsis;
             parent = None;
           }) ];
  assert_source_locations
    "a = ... # type: Tuple[str]"
    [ node
        ~start:(1, 0)
        ~stop:(1, 7)
        (Assign
           {
             Assign.target = !"a";
             annotation =
               Some
                 (node ~start:(1, 16) ~stop:(1, 26) (String (StringLiteral.create "Tuple[str]")));
             value = node ~start:(1, 4) ~stop:(1, 7) Ellipsis;
             parent = None;
           }) ];
  assert_source_locations
    "def foo(a): ... # type: ignore"
    [ node
        ~start:(1, 0)
        ~stop:(1, 15)
        (Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [ node
                       ~start:(1, 8)
                       ~stop:(1, 9)
                       { Parameter.name = "a"; value = None; annotation = None } ];
                 decorators = [];
                 docstring = None;
                 return_annotation = None;
                 async = false;
                 parent = None;
               };
             body = [node ~start:(1, 12) ~stop:(1, 15) (Expression (+Ellipsis))];
           }) ];
  assert_source_locations
    "@overload\ndef foo(a: int = ...):\n\t..."
    [ node
        ~start:(2, 0)
        ~stop:(3, 4)
        (Define
           {
             signature =
               {
                 name = !&"foo";
                 parameters =
                   [+{ Parameter.name = "a"; value = Some (+Ellipsis); annotation = Some !"int" }];
                 decorators = [node ~start:(1, 1) ~stop:(1, 9) (Name (Name.Identifier "overload"))];
                 docstring = None;
                 return_annotation = None;
                 async = false;
                 parent = None;
               };
             body = [node ~start:(3, 1) ~stop:(3, 4) (Expression (+Ellipsis))];
           }) ];
  assert_source_locations
    "class A:\n\ta = ... # type: int"
    [ node
        ~start:(1, 0)
        ~stop:(2, 8)
        (Class
           {
             Class.name = !&"A";
             bases = [];
             body =
               [ +Assign
                    {
                      Assign.target = !"a";
                      annotation =
                        Some
                          (node ~start:(2, 17) ~stop:(2, 20) (String (StringLiteral.create "int")));
                      value = node ~start:(2, 5) ~stop:(2, 8) Ellipsis;
                      parent = Some !&"A";
                    } ];
             decorators = [];
             docstring = None;
           }) ];
  assert_source_locations
    "class foo(): ... # type: ignore"
    [ node
        ~start:(1, 0)
        ~stop:(1, 16)
        (Class
           {
             Class.name = !&"foo";
             bases = [];
             body = [+Expression (+Ellipsis)];
             decorators = [];
             docstring = None;
           }) ]


let test_ternary_locations _ =
  assert_source_locations
    "5 if 1 else 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 13)
        (Expression
           (+Ternary { Ternary.target = +Integer 5; test = +Integer 1; alternative = +Integer 1 }))
    ];
  assert_source_locations
    "1 if 2 else 3 if 4 else 5"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 25)
            (Ternary
               {
                 Ternary.target = +Integer 1;
                 test = +Integer 2;
                 alternative =
                   node
                     ~start:(1, 12)
                     ~stop:(1, 25)
                     (Ternary
                        {
                          Ternary.target = +Integer 3;
                          test = +Integer 4;
                          alternative = +Integer 5;
                        });
               })) ]


let test_try_locations _ =
  assert_source_locations
    "try: a"
    [ node
        ~start:(1, 0)
        ~stop:(1, 6)
        (Try
           {
             Try.body = [node ~start:(1, 5) ~stop:(1, 6) (Expression !"a")];
             handlers = [];
             orelse = [];
             finally = [];
           }) ];
  assert_source_locations
    "try:\n\ta\nelse:\n\tb"
    [ node
        ~start:(1, 0)
        ~stop:(4, 2)
        (Try
           {
             Try.body = [node ~start:(2, 1) ~stop:(2, 2) (Expression !"a")];
             handlers = [];
             orelse = [node ~start:(4, 1) ~stop:(4, 2) (Expression !"b")];
             finally = [];
           }) ];
  assert_source_locations
    "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
    [ node
        ~start:(1, 0)
        ~stop:(6, 2)
        (Try
           {
             Try.body = [node ~start:(2, 1) ~stop:(2, 2) (Expression !"a")];
             handlers =
               [ {
                   Try.kind = Some (node ~start:(3, 7) ~stop:(3, 8) (Name (Name.Identifier "a")));
                   name = Some "b";
                   handler_body = [node ~start:(4, 1) ~stop:(4, 2) (Expression !"b")];
                 };
                 {
                   Try.kind = Some (node ~start:(5, 7) ~stop:(5, 8) (Name (Name.Identifier "d")));
                   name = None;
                   handler_body = [node ~start:(6, 1) ~stop:(6, 2) (Expression !"e")];
                 } ];
             orelse = [];
             finally = [];
           }) ];
  assert_source_locations
    "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
    [ node
        ~start:(1, 0)
        ~stop:(8, 2)
        (Try
           {
             Try.body = [+Expression !"a"];
             handlers = [{ Try.kind = None; name = None; handler_body = [+Expression !"b"] }];
             orelse = [+Expression !"c"];
             finally = [node ~start:(8, 1) ~stop:(8, 2) (Expression !"d")];
           }) ]


let test_tuple_locations _ =
  assert_source_locations
    {|
      (1, 2) = a
    |}
    [ +Assign
         {
           Assign.target = node ~start:(2, 1) ~stop:(2, 5) (Tuple [+Integer 1; +Integer 2]);
           annotation = None;
           value = !"a";
           parent = None;
         } ];
  assert_source_locations "()" [node ~start:(1, 0) ~stop:(1, 2) (Expression (+Tuple []))];
  assert_source_locations
    "(1,)"
    [node ~start:(1, 0) ~stop:(1, 4) (Expression (+Tuple [+Integer 1]))];
  assert_source_locations
    "1, 2"
    [node ~start:(1, 0) ~stop:(1, 4) (Expression (+Tuple [+Integer 1; +Integer 2]))];
  assert_source_locations
    "1, 1 + 1"
    [ node
        ~start:(1, 0)
        ~stop:(1, 8)
        (Expression
           (+Tuple
               [ +Integer 1;
                 node
                   ~start:(1, 3)
                   ~stop:(1, 8)
                   (Call
                      {
                        callee =
                          +Name
                             (Name.Attribute
                                { base = +Integer 1; attribute = "__add__"; special = true });
                        arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
                      }) ])) ]


let test_while_locations _ =
  assert_source_locations
    "while a:\n\tb\nelse:\n\tc\n"
    [ node
        ~start:(1, 0)
        ~stop:(4, 2)
        (While
           {
             While.test = node ~start:(1, 6) ~stop:(1, 7) (Name (Name.Identifier "a"));
             body = [node ~start:(2, 1) ~stop:(2, 2) (Expression !"b")];
             orelse = [node ~start:(4, 1) ~stop:(4, 2) (Expression !"c")];
           }) ]


let test_with_locations _ =
  assert_source_locations
    "with (yield from a): b\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 22)
        (With
           {
             With.items = [node ~start:(1, 6) ~stop:(1, 18) (Expression.Yield (Some !"a")), None];
             body = [node ~start:(1, 21) ~stop:(1, 22) (Expression !"b")];
             async = false;
           }) ];
  assert_source_locations
    "async with a: b\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 15)
        (With { With.items = [!"a", None]; body = [+Expression !"b"]; async = true }) ];
  assert_source_locations
    "with a, c as d: b\n"
    [ node
        ~start:(1, 0)
        ~stop:(1, 17)
        (With
           {
             With.items =
               [ node ~start:(1, 5) ~stop:(1, 6) (Name (Name.Identifier "a")), None;
                 ( node ~start:(1, 8) ~stop:(1, 9) (Name (Name.Identifier "c")),
                   Some (node ~start:(1, 13) ~stop:(1, 14) (Name (Name.Identifier "d"))) ) ];
             body = [+Expression !"b"];
             async = false;
           }) ]


let test_yield_locations _ =
  assert_source_locations
    "yield"
    [ node
        ~start:(1, 0)
        ~stop:(1, 5)
        (Statement.Yield (node ~start:(1, 0) ~stop:(1, 5) (Expression.Yield None))) ];
  assert_source_locations
    "yield 1"
    [ +Statement.Yield
         (node
            ~start:(1, 0)
            ~stop:(1, 7)
            (Expression.Yield (Some (node ~start:(1, 6) ~stop:(1, 7) (Integer 1))))) ];
  assert_source_locations
    "yield from a"
    [ +Statement.YieldFrom
         (node
            ~start:(1, 0)
            ~stop:(1, 12)
            (Expression.Yield
               (Some
                  (node
                     ~start:(1, 0)
                     ~stop:(1, 12)
                     (Call
                        {
                          callee =
                            node
                              ~start:(1, 0)
                              ~stop:(1, 12)
                              (Name
                                 (Name.Attribute
                                    {
                                      base =
                                        node
                                          ~start:(1, 11)
                                          ~stop:(1, 12)
                                          (Name (Name.Identifier "a"));
                                      attribute = "__iter__";
                                      special = true;
                                    }));
                          arguments = [];
                        }))))) ]


let () =
  "parsed_locations"
  >::: [ "assert_locations" >:: test_assert_locations;
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
         "yield_locations" >:: test_yield_locations ]
  |> Test.run

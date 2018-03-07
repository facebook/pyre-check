(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Expression

open Test


let test_is_explicit_constructor_call _ =
  let call name =
    {
      Call.name = Node.create_with_default_location (Expression.Access (Access.create name));
      arguments = [];
    }
  in
  assert_true (Call.is_explicit_constructor_call (call "Foo.__init__"));
  assert_false (Call.is_explicit_constructor_call (call "Foo.foo"));
  assert_false (Call.is_explicit_constructor_call (call "foo"))


let assert_expression_equal =
  assert_equal
    ~printer:(Expression.show)
    ~pp_diff:(diff ~print:Expression.pp)

let test_negate _ =
  assert_expression_equal
    (negate (+True))
    (+UnaryOperator {
       UnaryOperator.operator = UnaryOperator.Not;
       operand = +True;
     })

let test_normalize _ =
  assert_expression_equal
    (normalize (+True))
    (+True);

  assert_expression_equal
    (normalize
       (+BooleanOperator {
          BooleanOperator.operator = BooleanOperator.And;
          left = +False;
          right = +UnaryOperator {
            UnaryOperator.operator = UnaryOperator.Not;
            operand = +True;
          };
        }))
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.And;
       left = +False;
       right = +False;
     });

  assert_expression_equal
    (normalize
       (+BooleanOperator {
          BooleanOperator.operator = BooleanOperator.Or;
          left = +False;
          right = +False;
        }))
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.And;
       left = +True;
       right = +True;
     });

  assert_expression_equal
    (normalize
       (+BooleanOperator {
          BooleanOperator.operator = BooleanOperator.Or;
          left = +False;
          right = !"right";
        }))
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.And;
       left = +True;
       right = +UnaryOperator {
         UnaryOperator.operator = UnaryOperator.Not;
         operand = !"right";
       };
     });

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +True;
        }))
    (+False);

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +False;
        }))
    (+True);

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +UnaryOperator {
            UnaryOperator.operator = UnaryOperator.Not;
            operand = +True;
          };
        }))
    (+True);

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +ComparisonOperator {
            ComparisonOperator.left = !"a";
            right = [ComparisonOperator.LessThan, !"b"];
          };
        }))
    (+ComparisonOperator {
       ComparisonOperator.left = !"a";
       right = [ComparisonOperator.GreaterThanOrEquals, !"b"];
     });

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +ComparisonOperator {
            ComparisonOperator.left = !"x";
            right = [ ComparisonOperator.IsNot, !"y" ];
          };
        }))
    (+ComparisonOperator {
       ComparisonOperator.left = !"x";
       right = [ ComparisonOperator.Is, !"y" ];
     });

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +UnaryOperator {
            UnaryOperator.operator = UnaryOperator.Not;
            operand = !"x";
          };
        }))
    (!"x")


let test_pp _ =
  let simple_expression =
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.And;
       left = +False;
       right = +UnaryOperator {
         UnaryOperator.operator = UnaryOperator.Not;
         operand = +True;
       };
     }) in
  assert_equal
    (Expression.show simple_expression)
    "False and not True";

  let expression =
    (+List [simple_expression;simple_expression]) in
  assert_equal
    (Expression.show expression)
    "[False and not True, False and not True]";

  let expression =
    (+ListComprehension {
       Comprehension.element = !"element";
       Comprehension.generators = [
         {
           Comprehension.target = !"x";
           Comprehension.iterator = !"x";
           Comprehension.conditions = [simple_expression; simple_expression];
           async = false;
         }
       ]
     }) in
  assert_equal
    (Expression.show expression)
    "comprehension(element for generators(generator(x in x if False and not \
     True, False and not True)))";

  let expression =
    (+Lambda {
       Lambda.parameters = [
         +{
           Parameter.name = ~~"x";
           Parameter.value = Some (+Integer 1);
           annotation = None
         };
         +{
           Parameter.name = ~~"y";
           Parameter.value = Some (+Integer 2);
           annotation = None
         }
       ];
       Lambda.body = +Tuple [!"x"; +String "y"]
     }) in
  assert_equal
    (Expression.show expression)
    {|lambda (x=1, y=2) ((x, "y"))|}

let () =
  "call">:::[
    "is_explicit_constructor_call">::test_is_explicit_constructor_call;
  ]
  |> run_test_tt_main;
  "expression">:::[
    "negate">::test_negate;
    "normalize">::test_normalize;
    "pp">::test_pp;
  ]
  |> run_test_tt_main

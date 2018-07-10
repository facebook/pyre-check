(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Core
open Expression

open Test


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
       BooleanOperator.operator = BooleanOperator.Or;
       left = +False;
       right = +False;
     });

  assert_expression_equal
    (normalize
       (+BooleanOperator {
          BooleanOperator.operator = BooleanOperator.Or;
          left = +False;
          right = !"right";
        }))
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.Or;
       left = +False;
       right = !"right";
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
    (!"x");

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = !"x";
        }))
    (+UnaryOperator {
       UnaryOperator.operator = UnaryOperator.Not;
       operand = !"x";
     })


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


let test_drop_prefix _ =
  let assert_access_equal = assert_equal ~printer:(Access.show) ~pp_diff:(diff ~print:Access.pp) in

  assert_access_equal
    (Access.drop_prefix ~prefix:(Access.create "a.b") (Access.create "a.b.c"))
    (Access.create "c");

  assert_access_equal
    (Access.drop_prefix ~prefix:(Access.create "b.c") (Access.create "a.b.c"))
    (Access.create "a.b.c")


let test_equality _ =
  let compare_two_locations left right =
    let full_printer ({ Node.location; _ } as expression) =
      Format.asprintf "%s/%a" (Location.to_string_reference location) Expression.pp expression
    in
    let value = String "some_string" in
    let expression_left = Node.create ~location:left value in
    let expression_right = Node.create ~location:right value in
    assert_equal
      ~cmp:Expression.equal
      ~printer:full_printer
      expression_left
      expression_right;
    assert_equal
      ~printer:Int.to_string
      (Expression.hash expression_left)
      (Expression.hash expression_right);

    let access_left = Expression.access expression_left in
    let access_right = Expression.access expression_right in
    assert_equal
      ~cmp:Access.equal
      access_left
      access_right;
    assert_equal
      ~printer:Int.to_string
      (Access.hash access_left)
      (Access.hash access_right);

    let set = Access.Set.add Access.Set.empty access_left in
    assert_bool
      "Element should appear in the set"
      (Access.Set.mem set access_right);

    let map = Access.Map.add_exn Access.Map.empty ~key:access_left ~data:1 in
    assert_bool
      "Element should appear in the map"
      (Access.Map.mem map access_right);

    let table = Access.Table.create () in
    Hashtbl.set table ~key:access_left ~data:1;
    assert_bool
      "Element should appear in the table"
      (Hashtbl.mem table access_right)
  in
  let location_1 =
    {
      Location.path = String.hash "some_path";
      Location.start = { Location.line = 1; column = 1 };
      Location.stop = { Location.line = 2; column = 5 };
    }
  in
  let location_2 =
    {
      Location.path = String.hash "some_other_path";
      Location.start = { Location.line = 12; column = 3 };
      Location.stop = { Location.line = 12; column = 7 };
    }
  in
  compare_two_locations Location.any location_1;
  compare_two_locations Location.any location_2;
  compare_two_locations location_1 location_2


let test_delocalize _ =
  let assert_delocalized source expected =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (parse_single_expression expected)
      (parse_single_expression source |> Expression.delocalize)
  in

  assert_delocalized "constant" "constant";
  assert_delocalized "$local_qualifier$variable" "qualifier.variable";
  assert_delocalized "$local_base64$b64encode" "base64.b64encode";
  assert_delocalized "$local_module?qualifier$variable" "module.qualifier.variable";

  (* Don't attempt to delocalize qualified expressions. *)
  assert_delocalized "qualifier.$local_qualifier$variable" "qualifier.$local_qualifier$variable"



let () =
  "expression">:::[
    "negate">::test_negate;
    "normalize">::test_normalize;
    "pp">::test_pp;
    "drop_prefix">::test_drop_prefix;
    "equality">::test_equality;
    "delocalize">::test_delocalize;
  ]
  |> run_test_tt_main

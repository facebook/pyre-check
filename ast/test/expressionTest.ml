(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Core
open Expression
open Pyre

open Test


let assert_expression_equal =
  assert_equal
    ~printer:(Expression.show)
    ~pp_diff:(diff ~print:Expression.pp)


let test_negate _ =
  let assert_negate ~expected ~negated =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (parse_single_expression expected)
      (Expression.negate (parse_single_expression negated))
  in
  assert_negate ~expected:"True" ~negated:"not True";
  assert_negate ~expected:"not True" ~negated:"True";
  assert_negate ~expected:"True is False" ~negated:"True is not False";
  assert_negate ~expected:"True is not False" ~negated:"True is False"


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
            operator = ComparisonOperator.LessThan;
            right = !"b";
          };
        }))
    (+ComparisonOperator {
       ComparisonOperator.left = !"a";
       operator = ComparisonOperator.GreaterThanOrEquals;
       right = !"b";
     });

  assert_expression_equal
    (normalize
       (+UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = +ComparisonOperator {
            ComparisonOperator.left = !"x";
            operator = ComparisonOperator.IsNot;
            right = !"y";
          };
        }))
    (+ComparisonOperator {
       ComparisonOperator.left = !"x";
       operator = ComparisonOperator.Is;
       right = !"y";
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
  let assert_pp_equal source expected =
    assert_equal ~printer:ident (Expression.show source) expected
  in
  let simple_expression =
    (+BooleanOperator {
       BooleanOperator.operator = BooleanOperator.And;
       left = +False;
       right = +UnaryOperator {
         UnaryOperator.operator = UnaryOperator.Not;
         operand = +True;
       };
     }) in
  assert_pp_equal
    simple_expression
    "False and not True";

  assert_pp_equal
    (+List [simple_expression;simple_expression])
    "[False and not True, False and not True]";

  assert_pp_equal
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
     })
    "comprehension(element for generators(generator(x in x if False and not \
     True, False and not True)))";

  assert_pp_equal
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
       Lambda.body = +Tuple [!"x"; +String (StringLiteral.create "y")]
     })
    {|lambda (x=1, y=2) ((x, "y"))|};

  assert_pp_equal
    (+Access [
       Access.Identifier ~~"a";
       Access.Identifier ~~"b";
       Access.Identifier ~~"__getitem__";
       Access.Call
         (+[
            {
              Argument.name = None;
              value =
                (+Access [
                   Access.Identifier ~~"c";
                   Access.Identifier ~~"__getitem__";
                   Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
                 ]);
            }
          ]);
     ])
    "a.b[c[1]]";

  assert_pp_equal
    (+Access [
       Access.Identifier ~~"a";
       Access.Identifier ~~"b";
       Access.Identifier ~~"__getitem__";
       Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
       Access.Identifier ~~"c";
     ])
    "a.b[1].c"


let test_drop_prefix _ =
  let assert_access_equal = assert_equal ~printer:(Access.show) ~pp_diff:(diff ~print:Access.pp) in

  assert_access_equal
    (Access.drop_prefix ~prefix:(Access.create "a.b") (Access.create "a.b.c"))
    (Access.create "c");

  assert_access_equal
    (Access.drop_prefix ~prefix:(Access.create "b.c") (Access.create "a.b.c"))
    (Access.create "a.b.c")


let test_prefix _ =
  let assert_access_equal = assert_equal ~printer:(Access.show) ~pp_diff:(diff ~print:Access.pp) in

  assert_access_equal
    (Access.prefix (Access.create "a.b.c"))
    (Access.create "a.b");

  assert_access_equal
    (Access.prefix [])
    []


let test_equality _ =
  let compare_two_locations left right =
    let full_printer ({ Node.location; _ } as expression) =
      Format.asprintf "%s/%a" (Location.Reference.show location) Expression.pp expression
    in
    let value = String (StringLiteral.create "some_string") in
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
  compare_two_locations Location.Reference.any location_1;
  compare_two_locations Location.Reference.any location_2;
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


let test_comparison_operator_override _ =
  let assert_override source expected =
    let operator =
      match parse_single_expression source with
      | { Node.value = ComparisonOperator operator; _ } -> operator
      | _ -> failwith "Could not parse comparison operator."
    in
    assert_equal
      ~printer:(function | Some expression -> Expression.show expression | _ -> "None")
      ~cmp:(Option.equal Expression.equal)
      (expected >>| parse_single_expression)
      (ComparisonOperator.override operator)
  in
  assert_override "a < b" (Some "a.__lt__(b)");
  assert_override "a == b" (Some "a.__eq__(b)");
  assert_override "a >= b" (Some "a.__ge__(b)");
  assert_override "a in b" None;
  assert_override "a not in b" None;
  assert_override "a is not b" None


let test_name_and_arguments _ =
  let assert_call ~call ?(expected_arguments = []) expected_name =
    let { Expression.Access.callee = name; arguments } =
      match parse_single_expression call with
      | { Node.value = Access call; _ } ->
          Option.value_exn (Expression.Access.name_and_arguments ~call)
      | _ -> failwith "Unable to parse access"
    in
    assert_equal ~printer:ident expected_name name;
    assert_equal
      ~cmp:(List.equal ~equal:Argument.equal)
      ~printer:(List.to_string ~f:Argument.show)
      expected_arguments
      arguments
  in
  let assert_not_call ~call =
    match parse_single_expression call with
    | { Node.value = Access call; _ } ->
        assert_equal (Expression.Access.name_and_arguments ~call) None
    | _ ->
        failwith "Unable to parse access"
  in
  let argument ?name value =
    let name =
      name
      >>| Identifier.create
      >>| Ast.Node.create_with_default_location
    in
    { Argument.name; value }
  in
  assert_call ~call:"foo.bar()" "foo.bar";
  assert_call ~call:"f(1)" ~expected_arguments:[argument (+Integer 1)] "f";

  assert_not_call ~call:"foo.bar";
  assert_not_call ~call:"foo.bar().baz";
  assert_not_call ~call:"foo.bar().baz()";

  assert_call
    ~call:"foo.bar(1, x=2)"
    ~expected_arguments:[argument (+Integer 1); argument ~name:"x" (+Integer 2)]
    "foo.bar"


let test_is_assert_function _ =
  let is_assert name =
    Access.create name
    |> Access.is_assert_function
  in
  assert_true (is_assert "pyretestassert");
  assert_false (is_assert "pyretestassert()");
  assert_false (is_assert "notAssert")


let () =
  "expression">:::[
    "negate">::test_negate;
    "normalize">::test_normalize;
    "pp">::test_pp;
    "drop_prefix">::test_drop_prefix;
    "equality">::test_equality;
    "delocalize">::test_delocalize;
    "comparison_operator_override">::test_comparison_operator_override;
    "name_and_arguments">::test_name_and_arguments;
    "is_assert_function">::test_is_assert_function;
  ]
  |> Test.run

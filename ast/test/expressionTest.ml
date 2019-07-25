(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Ast
open Core
open Expression
open Pyre
open Test

let assert_expression_equal =
  assert_equal ~printer:Expression.show ~pp_diff:(diff ~print:Expression.pp)


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
  assert_expression_equal (normalize (+True)) (+True);
  assert_expression_equal
    (normalize
       (+BooleanOperator
           {
             BooleanOperator.operator = BooleanOperator.And;
             left = +False;
             right = +UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +True };
           }))
    (+BooleanOperator
        { BooleanOperator.operator = BooleanOperator.And; left = +False; right = +False });
  assert_expression_equal
    (normalize
       (+BooleanOperator
           { BooleanOperator.operator = BooleanOperator.Or; left = +False; right = +False }))
    (+BooleanOperator
        { BooleanOperator.operator = BooleanOperator.Or; left = +False; right = +False });
  assert_expression_equal
    (normalize
       (+BooleanOperator
           { BooleanOperator.operator = BooleanOperator.Or; left = +False; right = !"right" }))
    (+BooleanOperator
        { BooleanOperator.operator = BooleanOperator.Or; left = +False; right = !"right" });
  assert_expression_equal
    (normalize (+UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +True }))
    (+False);
  assert_expression_equal
    (normalize (+UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +False }))
    (+True);
  assert_expression_equal
    (normalize
       (+UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +True };
           }))
    (+True);
  assert_expression_equal
    (normalize
       (+UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +ComparisonOperator
                  {
                    ComparisonOperator.left = !"a";
                    operator = ComparisonOperator.LessThan;
                    right = !"b";
                  };
           }))
    (+ComparisonOperator
        {
          ComparisonOperator.left = !"a";
          operator = ComparisonOperator.GreaterThanOrEquals;
          right = !"b";
        });
  assert_expression_equal
    (normalize
       (+UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +ComparisonOperator
                  {
                    ComparisonOperator.left = !"x";
                    operator = ComparisonOperator.IsNot;
                    right = !"y";
                  };
           }))
    (+ComparisonOperator
        { ComparisonOperator.left = !"x"; operator = ComparisonOperator.Is; right = !"y" });
  assert_expression_equal
    (normalize
       (+UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" };
           }))
    !"x";
  assert_expression_equal
    (normalize (+UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" }))
    (+UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" })


let test_pp _ =
  let assert_pp_equal source expected =
    assert_equal ~printer:ident (Expression.show source) expected
  in
  let simple_expression =
    +BooleanOperator
       {
         BooleanOperator.operator = BooleanOperator.And;
         left = +False;
         right = +UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = +True };
       }
  in
  assert_pp_equal simple_expression "False and not True";
  assert_pp_equal
    (+List [simple_expression; simple_expression])
    "[False and not True, False and not True]";
  assert_pp_equal
    (+ListComprehension
        {
          Comprehension.element = !"element";
          Comprehension.generators =
            [ {
                Comprehension.target = !"x";
                Comprehension.iterator = !"x";
                Comprehension.conditions = [simple_expression; simple_expression];
                async = false;
              } ];
        })
    "comprehension(element for generators(generator(x in x if False and not True, False and not \
     True)))";
  assert_pp_equal
    (+Lambda
        {
          Lambda.parameters =
            [ +{ Parameter.name = "x"; Parameter.value = Some (+Integer 1); annotation = None };
              +{ Parameter.name = "y"; Parameter.value = Some (+Integer 2); annotation = None } ];
          Lambda.body = +Tuple [!"x"; +String (StringLiteral.create "y")];
        })
    {|lambda (x=1, y=2) ((x, "y"))|};
  assert_pp_equal
    (+Call
        {
          callee =
            +Name
               (Name.Attribute
                  {
                    base = +Name (Name.Identifier "a");
                    attribute = "__getitem__";
                    special = false;
                  });
          arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
        })
    "a.__getitem__(1)";
  assert_pp_equal
    (+Call
        {
          callee =
            +Name
               (Name.Attribute
                  {
                    base =
                      +Name
                         (Name.Attribute
                            {
                              base = +Name (Name.Identifier "a");
                              attribute = "b";
                              special = false;
                            });
                    attribute = "__getitem__";
                    special = true;
                  });
          arguments =
            [ {
                Call.Argument.name = None;
                value =
                  +Call
                     {
                       callee =
                         +Name
                            (Name.Attribute
                               {
                                 base = +Name (Name.Identifier "c");
                                 attribute = "__getitem__";
                                 special = true;
                               });
                       arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
                     };
              } ];
        })
    "a.b[c[1]]";
  assert_pp_equal
    (+Name
        (Name.Attribute
           {
             base =
               +Call
                  {
                    callee =
                      +Name
                         (Name.Attribute
                            {
                              base =
                                +Name
                                   (Name.Attribute
                                      {
                                        base = +Name (Name.Identifier "a");
                                        attribute = "b";
                                        special = false;
                                      });
                              attribute = "__getitem__";
                              special = true;
                            });
                    arguments = [{ Call.Argument.name = None; value = +Integer 1 }];
                  };
             attribute = "c";
             special = false;
           }))
    "a.b[1].c";
  assert_pp_equal (parse_single_expression "'string {}'.format(1)") "\"string {}\".format(1)"


let test_equality _ =
  let compare_two_locations left right =
    let full_printer ({ Node.location; _ } as expression) =
      Format.asprintf "%s/%a" (Location.Reference.show location) Expression.pp expression
    in
    let value = Name (Name.Identifier "some_string") in
    let expression_left = Node.create ~location:left value in
    let expression_right = Node.create ~location:right value in
    assert_equal ~cmp:Expression.equal ~printer:full_printer expression_left expression_right;
    assert_equal
      ~printer:Int.to_string
      (Expression.hash expression_left)
      (Expression.hash expression_right)
  in
  let location_1 =
    {
      Location.path = !&"some_path";
      Location.start = { Location.line = 1; column = 1 };
      Location.stop = { Location.line = 2; column = 5 };
    }
  in
  let location_2 =
    {
      Location.path = !&"some_other_path";
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
  assert_delocalized "qualifier.$local_qualifier$variable" "qualifier.$local_qualifier$variable";
  let assert_delocalize_qualified source expected =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (parse_single_expression expected)
      (parse_single_expression source |> Expression.delocalize_qualified)
  in
  assert_delocalize_qualified "qualifier.$local_qualifier$variable" "qualifier.variable"


let test_comparison_operator_override _ =
  let assert_override source expected =
    let operator =
      match parse_single_expression source with
      | { Node.value = ComparisonOperator operator; _ } -> operator
      | _ -> failwith "Could not parse comparison operator."
    in
    assert_equal
      ~printer:(function
        | Some expression -> Expression.show expression
        | _ -> "None")
      ~cmp:(Option.equal Expression.equal)
      (expected >>| parse_single_expression ~coerce_special_methods:true)
      (ComparisonOperator.override operator)
  in
  assert_override "a < b" (Some "a.__lt__(b)");
  assert_override "a == b" (Some "a.__eq__(b)");
  assert_override "a >= b" (Some "a.__ge__(b)");
  assert_override "a in b" None;
  assert_override "a not in b" None;
  assert_override "a is not b" None


let test_exists_in_list _ =
  let assert_exists ~match_prefix expression_list target_string =
    exists_in_list ~match_prefix ~expression_list target_string |> assert_true
  in
  let assert_not_exists ~match_prefix expression_list target_string =
    exists_in_list ~match_prefix ~expression_list target_string |> assert_false
  in
  let simple = parse_single_expression "a.b.c" in
  let call_at_end = parse_single_expression "a.b.c()" in
  let call_in_the_middle = parse_single_expression "a.b().c" in
  let call_everywhere = parse_single_expression "a().b().c()" in
  assert_exists [simple] ~match_prefix:false "a.b.c";
  assert_exists [simple] ~match_prefix:true "a.b.c";
  assert_not_exists [simple] "a.b" ~match_prefix:false;
  assert_exists [simple] "a.b" ~match_prefix:true;
  assert_not_exists [simple] "a.c" ~match_prefix:false;
  assert_not_exists [simple] "a.c" ~match_prefix:true;
  assert_not_exists [simple] "a.b.c.d" ~match_prefix:false;
  assert_not_exists [simple] "a.b.c.d" ~match_prefix:true;
  assert_not_exists [call_at_end] "a.b.c" ~match_prefix:false;
  assert_exists [call_at_end] "a.b.c" ~match_prefix:true;
  assert_not_exists [call_at_end] "a.b" ~match_prefix:false;
  assert_exists [call_at_end] "a.b" ~match_prefix:true;
  assert_not_exists [call_at_end] "a.c" ~match_prefix:false;
  assert_not_exists [call_at_end] "a.c" ~match_prefix:true;
  assert_not_exists [call_at_end] "a.b.c.d" ~match_prefix:false;
  assert_not_exists [call_at_end] "a.b.c.d" ~match_prefix:true;
  assert_not_exists [call_in_the_middle] "a.b.c" ~match_prefix:false;
  assert_not_exists [call_in_the_middle] "a.b.c" ~match_prefix:true;
  assert_not_exists [call_in_the_middle] "a.b" ~match_prefix:false;
  assert_exists [call_in_the_middle] "a.b" ~match_prefix:true;
  assert_not_exists [call_in_the_middle] "a.c" ~match_prefix:false;
  assert_not_exists [call_in_the_middle] "a.c" ~match_prefix:true;
  assert_not_exists [call_in_the_middle] "a.b.c.d" ~match_prefix:false;
  assert_not_exists [call_in_the_middle] "a.b.c.d" ~match_prefix:true;
  assert_not_exists [call_everywhere] "a.b.c" ~match_prefix:false;
  assert_not_exists [call_everywhere] "a.b.c" ~match_prefix:true;
  assert_not_exists [call_everywhere] "a.b" ~match_prefix:false;
  assert_not_exists [call_everywhere] "a.b" ~match_prefix:true;
  assert_not_exists [call_everywhere] "a.c" ~match_prefix:false;
  assert_not_exists [call_everywhere] "a.c" ~match_prefix:true;
  assert_not_exists [call_everywhere] "a.b.c.d" ~match_prefix:false;
  assert_not_exists [call_everywhere] "a.b.c.d" ~match_prefix:true;

  (* Qualified *)
  assert_exists
    [parse_single_expression "qualifier.$local_qualifier$property"]
    ~match_prefix:false
    "qualifier.property"


let test_create_name _ =
  let assert_create_from_identifiers identifiers expected =
    let identifier_nodes = List.map ~f:Node.create_with_default_location identifiers in
    assert_equal expected (create_name_from_identifiers identifier_nodes)
  in
  assert_create_from_identifiers ["a"] (Name.Identifier "a");
  assert_create_from_identifiers
    ["a"; "b"; "c"]
    (Name.Attribute
       {
         base =
           ~+(Name
                (Name.Attribute
                   { base = ~+(Name (Name.Identifier "a")); attribute = "b"; special = false }));
         attribute = "c";
         special = false;
       });
  let assert_create raw_string expected =
    assert_equal expected (create_name ~location:Location.Reference.any raw_string)
  in
  assert_create "a" (Name.Identifier "a");
  assert_create
    "a.b.c"
    (Name.Attribute
       {
         base =
           ~+(Name
                (Name.Attribute
                   { base = ~+(Name (Name.Identifier "a")); attribute = "b"; special = false }));
         attribute = "c";
         special = false;
       })


let test_name_to_identifiers _ =
  let assert_name_to_identifiers name identifiers =
    assert_equal
      ~cmp:(Option.equal (List.equal String.equal))
      identifiers
      (Expression.name_to_identifiers name)
  in
  assert_name_to_identifiers (Name.Identifier "a") (Some ["a"]);
  assert_name_to_identifiers
    (Name.Attribute
       {
         base =
           ~+(Name
                (Name.Attribute
                   { base = ~+(Name (Name.Identifier "a")); attribute = "b"; special = false }));
         attribute = "c";
         special = false;
       })
    (Some ["a"; "b"; "c"]);
  assert_name_to_identifiers
    (Name.Attribute
       {
         base =
           ~+(Name (Name.Attribute { base = ~+(Integer 1); attribute = "b"; special = false }));
         attribute = "c";
         special = false;
       })
    None


let test_name_equals _ =
  let create_base name = Node.create_with_default_location (Name name) in
  let assert_name_equals name expression = assert_true (name_is ~name (create_base expression)) in
  let assert_name_not_equals name expression =
    assert_false (name_is ~name (create_base expression))
  in
  assert_name_equals "a" (Name.Identifier "a");
  assert_name_equals
    "a.b"
    (Name.Attribute { base = create_base (Name.Identifier "a"); attribute = "b"; special = false });
  assert_name_not_equals
    "a.b.c"
    (Name.Attribute { base = create_base (Name.Identifier "a"); attribute = "b"; special = false });
  assert_name_not_equals "a" (Name.Identifier "b");
  assert_name_not_equals "a.b" (Name.Identifier "a");
  assert_name_not_equals "" (Name.Identifier "a")


let test_arguments_location _ =
  let assert_arguments_location expression expected_start expected_stop =
    let expression = parse_single_expression expression in
    let expected_location =
      {
        expression.Node.location with
        start = { Location.line = 1; column = expected_start };
        stop = { Location.line = 1; column = expected_stop };
      }
    in
    let actual_location =
      match expression with
      | { Node.value = Call call; _ } -> arguments_location call
      | _ -> failwith "Expected a call expression."
    in
    assert_equal ~printer:Location.show expected_location actual_location
  in
  assert_arguments_location "call(1, 2)" 4 10;
  assert_arguments_location "call(1, 2, 3)" 4 13;
  assert_arguments_location "long_call()" 9 11


let () =
  "expression"
  >::: [ "negate" >:: test_negate;
         "normalize" >:: test_normalize;
         "pp" >:: test_pp;
         "equality" >:: test_equality;
         "delocalize" >:: test_delocalize;
         "comparison_operator_override" >:: test_comparison_operator_override;
         "exists_in_list" >:: test_exists_in_list;
         "create_name" >:: test_create_name;
         "name_to_identifiers" >:: test_name_to_identifiers;
         "name_equals" >:: test_name_equals;
         "arguments_location" >:: test_arguments_location ]
  |> Test.run

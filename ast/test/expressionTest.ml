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


let simple_access access =
  +Access (SimpleAccess access)


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
           Parameter.name = "x";
           Parameter.value = Some (+Integer 1);
           annotation = None
         };
         +{
           Parameter.name = "y";
           Parameter.value = Some (+Integer 2);
           annotation = None
         }
       ];
       Lambda.body = +Tuple [!"x"; +String (StringLiteral.create "y")]
     })
    {|lambda (x=1, y=2) ((x, "y"))|};

  assert_pp_equal
    (simple_access [
        Access.Identifier "a";
        Access.Identifier "b";
        Access.Identifier "__getitem__";
        Access.Call
          (+[
             {
               Argument.name = None;
               value =
                 (simple_access [
                     Access.Identifier "c";
                     Access.Identifier "__getitem__";
                     Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
                   ]);
             }
           ]);
      ])
    "a.b[c[1]]";

  assert_pp_equal
    (simple_access [
        Access.Identifier "a";
        Access.Identifier "b";
        Access.Identifier "__getitem__";
        Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
        Access.Identifier "c";
      ])
    "a.b[1].c";

  assert_pp_equal
    (parse_single_expression ~convert:true "'string {}'.format(1)")
    "\"string {}\".format(1)"


let test_drop_prefix _ =
  let assert_access_equal = assert_equal ~printer:(Access.show) ~pp_diff:(diff ~print:Access.pp) in

  assert_access_equal
    (Access.drop_prefix ~prefix:(!+"a.b") (!+"a.b.c"))
    (!+"c");

  assert_access_equal
    (Access.drop_prefix ~prefix:(!+"b.c") (!+"a.b.c"))
    (!+"a.b.c")


let test_prefix _ =
  let assert_access_equal = assert_equal ~printer:(Access.show) ~pp_diff:(diff ~print:Access.pp) in

  assert_access_equal
    (Access.prefix (!+"a.b.c"))
    (!+"a.b");

  assert_access_equal
    (Access.prefix [])
    []


let test_equality _ =
  let compare_two_locations left right =
    let full_printer ({ Node.location; _ } as expression) =
      Format.asprintf "%s/%a" (Location.Reference.show location) Expression.pp expression
    in
    let value = Access (SimpleAccess [Identifier "some_string"]) in
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

    let extract_access = function
      | { Node.value = Access (SimpleAccess access); _ } -> access
      | _ -> failwith "Expected access."
    in
    let access_left = extract_access expression_left in
    let access_right = extract_access expression_right in
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

    let map = Access.Map.set Access.Map.empty ~key:access_left ~data:1 in
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
      (parse_single_expression ~convert:true expected)
      (parse_single_expression ~convert:true source |> Expression.delocalize);
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
      match parse_single_expression ~convert:true call with
      | { Node.value = Access (SimpleAccess call); _ } ->
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
    match parse_single_expression ~convert:true call with
    | { Node.value = Access (SimpleAccess call); _ } ->
        assert_equal (Expression.Access.name_and_arguments ~call) None
    | _ ->
        failwith "Unable to parse access"
  in
  let argument ?name value =
    let name =
      name
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
    !+name
    |> Access.is_assert_function
  in
  assert_true (is_assert "pyretestassert");
  assert_false (is_assert "pyretestassert()");
  assert_false (is_assert "notAssert")


let test_exists_in_list _ =
  let make_expression target_string =
    let make_access = function
      | "()" -> Access.Call { Node.location = Location.Reference.any; value = [] }
      | name -> Access.Identifier name
    in
    let elements = String.split ~on:'.' target_string in
    let access = List.map elements ~f:make_access in
    +Access (SimpleAccess access)
  in
  let assert_exists ~match_prefix expression_list target_string =
    exists_in_list ~match_prefix ~expression_list target_string
    |> assert_true
  in
  let assert_not_exists ~match_prefix expression_list target_string =
    exists_in_list ~match_prefix ~expression_list target_string
    |> assert_false
  in

  let simple = make_expression "a.b.c" in
  let call_at_end = make_expression "a.b.c.()" in
  let call_in_the_middle = make_expression "a.b.().c" in
  let call_everywhere = make_expression "a.().b.().c.()" in
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
  assert_not_exists [call_everywhere] "a.b.c.d" ~match_prefix:true


let test_convert_accesses _ =
  let assert_convert_new_to_old new_access expected =
    let converted =
      new_access
      |> Node.create_with_default_location
      |> convert
    in
    assert_equal
      ~printer:(Expression.show)
      ~+expected
      converted
  in
  assert_convert_new_to_old
    (Name (Name.Identifier "a"))
    (Access (SimpleAccess [Identifier "a"]));
  assert_convert_new_to_old
    (Name (
      Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b" }
    ))
    (Access (SimpleAccess [Identifier "a"; Identifier "b"]));
  assert_convert_new_to_old
    (Name (Name.Attribute {
      base = ~+(Name (
        Name.Attribute {
          base = ~+(Name (Name.Identifier "a"));
          attribute = "b";
      }));
      attribute = "c";
    }))
    (Access (SimpleAccess [Identifier"a"; Identifier "b"; Identifier "c"]));
  assert_convert_new_to_old
    (Call {
      callee = ~+(Name (Name.Identifier "a"));
      arguments = [{ Call.Argument.name = None; value = +Name (Name.Identifier "x") }];
    })
    (Access (SimpleAccess [Identifier "a"; Call ~+[{ Argument.name = None; value = !"x" }]]));
  assert_convert_new_to_old
    (Call {
      callee = ~+(Name (
        Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b" }
      ));
      arguments = [{ Call.Argument.name = None; value = +Name (Name.Identifier "x") }];
    })
    (Access
      (SimpleAccess [
        Identifier "a";
        Identifier "b";
        Call ~+[{ Argument.name = None; value = !"x" }];
      ])
    );

  let assert_convert_old_to_new old_access expected =
    let converted =
      (Access old_access)
      |> Node.create_with_default_location
      |> convert_to_new
    in
    assert_equal
      ~printer:(Expression.show)
      ~+expected
      converted
  in
  assert_convert_old_to_new
    (SimpleAccess [Identifier "a"])
    (Name (Name.Identifier "a"));
  assert_convert_old_to_new
    (SimpleAccess [Identifier "a"; Identifier "b"])
    (Name (
      Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b" }
    ));
  assert_convert_old_to_new
    (SimpleAccess [Identifier"a"; Identifier "b"; Identifier "c"])
    (Name (Name.Attribute {
      base = ~+(Name (
        Name.Attribute {
          base = ~+(Name (Name.Identifier "a"));
          attribute = "b";
      }));
      attribute = "c";
    }));
  assert_convert_old_to_new
    (SimpleAccess [Identifier "a"; Call ~+[{ Argument.name = None; value = !"x" }]])
    (Call {
      callee = ~+(Name (Name.Identifier "a"));
      arguments = [{ Call.Argument.name = None; value = +Name (Name.Identifier "x") }];
    });
  assert_convert_old_to_new
    (SimpleAccess [
      Identifier "a";
      Identifier "b";
      Call ~+[{ Argument.name = None; value = !"x" }];
    ])
    (Call {
      callee = ~+(Name (
        Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b" }
      ));
      arguments = [{ Call.Argument.name = None; value = +Name (Name.Identifier "x") }];
    });
  assert_convert_old_to_new
    (ExpressionAccess { expression = ~+(List []); access = [Identifier "a"; Identifier "b"]})
    (Name (Name.Attribute {
      base = ~+(Name (Name.Attribute { base = ~+(List []); attribute = "a" }));
      attribute = "b";
    }))


let test_create_name _ =
  let assert_create_from_identifiers identifiers expected =
    let identifier_nodes = List.map ~f:Node.create_with_default_location identifiers in
    assert_equal
      expected
      (create_name_from_identifiers identifier_nodes)
  in
  assert_create_from_identifiers
    ["a"]
    (Name.Identifier "a");
  assert_create_from_identifiers
    ["a"; "b"; "c"]
    (Name.Attribute {
      base = ~+(Name (Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b"}));
      attribute = "c";
    });

  let assert_create raw_string expected =
    assert_equal expected (create_name ~location:Location.Reference.any raw_string)
  in
  assert_create "a" (Name.Identifier "a");
  assert_create
    "a.b.c"
    (Name.Attribute {
      base = ~+(Name (Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b"}));
      attribute = "c";
    })


let test_name_to_identifiers _ =
  let assert_name_to_identifiers name identifiers =
    assert_equal
      ~cmp:(Option.equal (List.equal ~equal:String.equal))
      identifiers
      (Expression.name_to_identifiers name)
  in
  assert_name_to_identifiers (Name.Identifier "a") (Some ["a"]);
  assert_name_to_identifiers
    (Name.Attribute {
      base = ~+(Name (Name.Attribute { base = ~+(Name (Name.Identifier "a")); attribute = "b"}));
      attribute = "c";
    })
    (Some ["a"; "b"; "c"]);
  assert_name_to_identifiers
    (Name.Attribute {
      base = ~+(Name (Name.Attribute { base = ~+(Integer 1); attribute = "b"}));
      attribute = "c";
    })
    None


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
    "exists_in_list">::test_exists_in_list;
    "convert_accesses">::test_convert_accesses;
    "create_name">::test_create_name;
    "name_to_identifiers">::test_name_to_identifiers;
  ]
  |> Test.run

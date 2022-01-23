(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Core
open Expression
open Pyre
open Test

let assert_expression_equal =
  assert_equal ~printer:Expression.show ~pp_diff:(diff ~print:Expression.pp)


let location_insensitive_equal left right = Expression.location_insensitive_compare left right = 0

let test_negate _ =
  let assert_negate ~expected ~negated =
    assert_equal
      ~printer:Expression.show
      ~cmp:location_insensitive_equal
      (parse_single_expression expected)
      (negate (parse_single_expression negated))
  in
  assert_negate ~expected:"True" ~negated:"not True";
  assert_negate ~expected:"not True" ~negated:"True";
  assert_negate ~expected:"True is False" ~negated:"True is not False";
  assert_negate ~expected:"True is not False" ~negated:"True is False"


let test_normalize _ =
  assert_expression_equal
    (normalize (+Expression.Constant Constant.True))
    (+Expression.Constant Constant.True);
  assert_expression_equal
    (normalize
       (+Expression.BooleanOperator
           {
             BooleanOperator.operator = BooleanOperator.And;
             left = +Expression.Constant Constant.False;
             right =
               +Expression.UnaryOperator
                  {
                    UnaryOperator.operator = UnaryOperator.Not;
                    operand = +Expression.Constant Constant.True;
                  };
           }))
    (+Expression.BooleanOperator
        {
          BooleanOperator.operator = BooleanOperator.And;
          left = +Expression.Constant Constant.False;
          right = +Expression.Constant Constant.False;
        });
  assert_expression_equal
    (normalize
       (+Expression.BooleanOperator
           {
             BooleanOperator.operator = BooleanOperator.Or;
             left = +Expression.Constant Constant.False;
             right = +Expression.Constant Constant.False;
           }))
    (+Expression.BooleanOperator
        {
          BooleanOperator.operator = BooleanOperator.Or;
          left = +Expression.Constant Constant.False;
          right = +Expression.Constant Constant.False;
        });
  assert_expression_equal
    (normalize
       (+Expression.BooleanOperator
           {
             BooleanOperator.operator = BooleanOperator.Or;
             left = +Expression.Constant Constant.False;
             right = !"right";
           }))
    (+Expression.BooleanOperator
        {
          BooleanOperator.operator = BooleanOperator.Or;
          left = +Expression.Constant Constant.False;
          right = !"right";
        });
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand = +Expression.Constant Constant.True;
           }))
    (+Expression.Constant Constant.False);
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand = +Expression.Constant Constant.False;
           }))
    (+Expression.Constant Constant.True);
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +Expression.UnaryOperator
                  {
                    UnaryOperator.operator = UnaryOperator.Not;
                    operand = +Expression.Constant Constant.True;
                  };
           }))
    (+Expression.Constant Constant.True);
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +Expression.ComparisonOperator
                  {
                    ComparisonOperator.left = !"a";
                    operator = ComparisonOperator.LessThan;
                    right = !"b";
                  };
           }))
    (+Expression.ComparisonOperator
        {
          ComparisonOperator.left = !"a";
          operator = ComparisonOperator.GreaterThanOrEquals;
          right = !"b";
        });
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +Expression.ComparisonOperator
                  {
                    ComparisonOperator.left = !"x";
                    operator = ComparisonOperator.IsNot;
                    right = !"y";
                  };
           }))
    (+Expression.ComparisonOperator
        { ComparisonOperator.left = !"x"; operator = ComparisonOperator.Is; right = !"y" });
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator
           {
             UnaryOperator.operator = UnaryOperator.Not;
             operand =
               +Expression.UnaryOperator
                  { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" };
           }))
    !"x";
  assert_expression_equal
    (normalize
       (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" }))
    (+Expression.UnaryOperator { UnaryOperator.operator = UnaryOperator.Not; operand = !"x" })


let test_pp _ =
  let assert_pp_equal source expected =
    assert_equal ~printer:ident (Expression.show source) expected
  in
  let simple_expression =
    +Expression.BooleanOperator
       {
         BooleanOperator.operator = BooleanOperator.And;
         left = +Expression.Constant Constant.False;
         right =
           +Expression.UnaryOperator
              {
                UnaryOperator.operator = UnaryOperator.Not;
                operand = +Expression.Constant Constant.True;
              };
       }
  in
  assert_pp_equal simple_expression "False and not True";
  assert_pp_equal
    (+Expression.List [simple_expression; simple_expression])
    "[False and not True, False and not True]";
  assert_pp_equal
    (+Expression.ListComprehension
        {
          Comprehension.element = !"element";
          Comprehension.generators =
            [
              {
                Comprehension.Generator.target = !"x";
                iterator = !"x";
                conditions = [simple_expression; simple_expression];
                async = false;
              };
            ];
        })
    "comprehension(element for generators(generator(x in x if False and not True, False and not \
     True)))";
  assert_pp_equal
    (+Expression.Lambda
        {
          Lambda.parameters =
            [
              +{
                 Parameter.name = "x";
                 Parameter.value = Some (+Expression.Constant (Constant.Integer 1));
                 annotation = None;
               };
              +{
                 Parameter.name = "y";
                 Parameter.value = Some (+Expression.Constant (Constant.Integer 2));
                 annotation = None;
               };
            ];
          Lambda.body =
            +Expression.Tuple
               [!"x"; +Expression.Constant (Constant.String (StringLiteral.create "y"))];
        })
    {|lambda (x=1, y=2) ((x, "y"))|};
  assert_pp_equal
    (+Expression.Call
        {
          callee =
            +Expression.Name
               (Name.Attribute
                  {
                    base = +Expression.Name (Name.Identifier "a");
                    attribute = "__getitem__";
                    special = false;
                  });
          arguments =
            [{ Call.Argument.name = None; value = +Expression.Constant (Constant.Integer 1) }];
        })
    "a.__getitem__(1)";
  assert_pp_equal
    (+Expression.Call
        {
          callee =
            +Expression.Name
               (Name.Attribute
                  {
                    base =
                      +Expression.Name
                         (Name.Attribute
                            {
                              base = +Expression.Name (Name.Identifier "a");
                              attribute = "b";
                              special = false;
                            });
                    attribute = "__getitem__";
                    special = true;
                  });
          arguments =
            [
              {
                Call.Argument.name = None;
                value =
                  +Expression.Call
                     {
                       callee =
                         +Expression.Name
                            (Name.Attribute
                               {
                                 base = +Expression.Name (Name.Identifier "c");
                                 attribute = "__getitem__";
                                 special = true;
                               });
                       arguments =
                         [
                           {
                             Call.Argument.name = None;
                             value = +Expression.Constant (Constant.Integer 1);
                           };
                         ];
                     };
              };
            ];
        })
    "a.b[c[1]]";
  assert_pp_equal
    (+Expression.Name
        (Name.Attribute
           {
             base =
               +Expression.Call
                  {
                    callee =
                      +Expression.Name
                         (Name.Attribute
                            {
                              base =
                                +Expression.Name
                                   (Name.Attribute
                                      {
                                        base = +Expression.Name (Name.Identifier "a");
                                        attribute = "b";
                                        special = false;
                                      });
                              attribute = "__getitem__";
                              special = true;
                            });
                    arguments =
                      [
                        {
                          Call.Argument.name = None;
                          value = +Expression.Constant (Constant.Integer 1);
                        };
                      ];
                  };
             attribute = "c";
             special = false;
           }))
    "a.b[1].c";
  assert_pp_equal
    (+Expression.WalrusOperator { target = !"a"; value = +Expression.Constant (Constant.Integer 1) })
    "a := 1";
  assert_pp_equal (parse_single_expression "'string {}'.format(1)") "\"string {}\".format(1)";
  assert_pp_equal
    (+Expression.Dictionary
        {
          Dictionary.entries =
            [
              {
                Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                value = +Expression.Constant (Constant.Integer 2);
              };
            ];
          keywords = [];
        })
    "{ 1:2 }";
  assert_pp_equal (+Expression.Yield None) "(yield)";
  assert_pp_equal (+Expression.Yield (Some (+Expression.Constant (Constant.Integer 5)))) "(yield 5)";
  assert_pp_equal
    (+Expression.YieldFrom (+Expression.List [+Expression.Constant (Constant.Integer 5)]))
    "(yield from [5])";
  ()


let test_equality _ =
  let compare_two_locations left right equal compare_equal hash_equal =
    let value = Expression.Name (Name.Identifier "some_string") in
    let expression_left = Node.create ~location:left value in
    let expression_right = Node.create ~location:right value in
    let assert_bool_equal = assert_equal ~cmp:Bool.equal ~printer:Bool.to_string in
    assert_bool_equal ([%compare.equal: Expression.t] expression_left expression_right) equal;
    assert_bool_equal (Expression.compare expression_left expression_right = 0) compare_equal;
    assert_bool_equal
      (Expression.hash expression_left = Expression.hash expression_right)
      hash_equal
  in
  let location_1 =
    {
      Location.start = { Location.line = 1; column = 1 };
      Location.stop = { Location.line = 2; column = 5 };
    }
  in
  let location_2 =
    {
      Location.start = { Location.line = 12; column = 3 };
      Location.stop = { Location.line = 12; column = 7 };
    }
  in
  compare_two_locations location_1 location_1 true true true;
  compare_two_locations Location.any location_1 false false false;
  compare_two_locations Location.any location_2 false false false;
  compare_two_locations location_1 location_2 false false false


let test_delocalize _ =
  let assert_delocalized source expected =
    assert_equal
      ~printer:Expression.show
      ~cmp:location_insensitive_equal
      expected
      (delocalize source)
  in
  assert_delocalized !"constant" !"constant";
  assert_delocalized !"$local_qualifier$variable" !"qualifier.variable";
  assert_delocalized !"$local_base64$b64encode" !"base64.b64encode";
  assert_delocalized !"$local_module?qualifier$variable" !"module.qualifier.variable";
  assert_delocalized
    !"$local_module_hyphenated?qualifier$variable"
    !"module_hyphenated.qualifier.variable";

  (* Don't attempt to delocalize qualified expressions. *)
  assert_delocalized
    (+Expression.Name
        (Name.Attribute
           { base = !"qualifier"; attribute = "$local_qualifier$variable"; special = true }))
    (+Expression.Name
        (Name.Attribute
           { base = !"qualifier"; attribute = "$local_qualifier$variable"; special = true }));

  let assert_delocalize_qualified source expected =
    assert_equal
      ~printer:Expression.show
      ~cmp:location_insensitive_equal
      expected
      (delocalize_qualified source)
  in
  assert_delocalize_qualified
    (+Expression.Name
        (Name.Attribute
           { base = !"qualifier"; attribute = "$local_qualifier$variable"; special = true }))
    (+Expression.Name
        (Name.Attribute { base = !"qualifier"; attribute = "variable"; special = true }))


let test_comparison_operator_override _ =
  let assert_override source expected =
    let operator, location =
      match parse_single_expression source with
      | { Node.value = ComparisonOperator operator; location } -> operator, location
      | _ -> failwith "Could not parse comparison operator."
    in
    assert_equal
      ~printer:(function
        | Some expression ->
            Node.sexp_of_t Expression.sexp_of_expression expression |> Sexp.to_string_hum
        | _ -> "None")
      ~cmp:(Option.equal (fun left right -> Expression.location_insensitive_compare left right = 0))
      (expected >>| parse_single_expression ~coerce_special_methods:true)
      (ComparisonOperator.override ~location operator)
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
    [
      +Expression.Name
         (Name.Attribute
            {
              Name.Attribute.base = !"qualifier";
              attribute = "$local_qualifier$property";
              special = false;
            });
    ]
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
           ~+(Expression.Name
                (Name.Attribute
                   {
                     base = ~+(Expression.Name (Name.Identifier "a"));
                     attribute = "b";
                     special = false;
                   }));
         attribute = "c";
         special = false;
       });
  let assert_create raw_string expected =
    assert_equal expected (create_name ~location:Location.any raw_string)
  in
  assert_create "a" (Name.Identifier "a");
  assert_create
    "a.b.c"
    (Name.Attribute
       {
         base =
           ~+(Expression.Name
                (Name.Attribute
                   {
                     base = ~+(Expression.Name (Name.Identifier "a"));
                     attribute = "b";
                     special = false;
                   }));
         attribute = "c";
         special = false;
       })


let test_name_to_identifiers _ =
  let assert_name_to_identifiers name identifiers =
    assert_equal
      ~cmp:(Option.equal (List.equal String.equal))
      identifiers
      (name_to_identifiers name)
  in
  assert_name_to_identifiers (Name.Identifier "a") (Some ["a"]);
  assert_name_to_identifiers
    (Name.Attribute
       {
         base =
           ~+(Expression.Name
                (Name.Attribute
                   {
                     base = ~+(Expression.Name (Name.Identifier "a"));
                     attribute = "b";
                     special = false;
                   }));
         attribute = "c";
         special = false;
       })
    (Some ["a"; "b"; "c"]);
  assert_name_to_identifiers
    (Name.Attribute
       {
         base =
           ~+(Expression.Name
                (Name.Attribute
                   {
                     base = ~+(Expression.Constant (Constant.Integer 1));
                     attribute = "b";
                     special = false;
                   }));
         attribute = "c";
         special = false;
       })
    None


let test_name_equals _ =
  let create_base name = Node.create_with_default_location (Expression.Name name) in
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


let test_default_folder context =
  (* Define a folder that computes the sum of all integer constants in the expression *)
  let folder =
    let fold_constant ~folder:_ ~state = function
      | Constant.Integer i -> state + i
      | _ -> state
    in
    Folder.create_with_uniform_location_fold ~fold_constant ()
  in
  let assert_count ~expected expression =
    let actual = Folder.fold ~folder ~state:0 expression in
    assert_equal
      ~ctxt:context
      ~cmp:Int.equal
      ~printer:Int.to_string
      ~msg:"Unexpected integer sum"
      expected
      actual
  in
  let integer i = +Expression.Constant (Constant.Integer i) in
  assert_count (+Expression.Await (integer 1)) ~expected:1;
  assert_count
    (+Expression.BooleanOperator
        { BooleanOperator.left = integer 1; operator = BooleanOperator.And; right = integer 2 })
    ~expected:3;
  assert_count
    (+Expression.Call
        {
          Call.callee = integer 1;
          arguments =
            [
              { Call.Argument.name = None; value = integer 2 };
              { Call.Argument.name = None; value = integer 3 };
            ];
        })
    ~expected:6;
  assert_count
    (+Expression.ComparisonOperator
        {
          ComparisonOperator.left = integer 1;
          operator = ComparisonOperator.Equals;
          right = integer 2;
        })
    ~expected:3;
  assert_count (+Expression.Constant Constant.NoneLiteral) ~expected:0;
  assert_count
    (+Expression.Dictionary
        {
          Dictionary.entries = [{ Dictionary.Entry.key = integer 1; value = integer 2 }];
          keywords = [integer 3];
        })
    ~expected:6;
  assert_count
    (+Expression.DictionaryComprehension
        {
          Comprehension.element = { Dictionary.Entry.key = !"x"; value = integer 1 };
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:15;
  assert_count
    (+Expression.Generator
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:15;
  assert_count
    (+Expression.FormatString [Substring.Literal (+"abc"); Substring.Format (integer 1)])
    ~expected:1;
  assert_count
    (+Expression.Lambda
        {
          Lambda.parameters =
            [Parameter.create ~location:Location.any ~value:(integer 1) ~name:"x" ()];
          body = integer 2;
        })
    ~expected:3;
  assert_count (+Expression.List [integer 1; integer 2]) ~expected:3;
  assert_count
    (+Expression.ListComprehension
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:15;
  assert_count !"x" ~expected:0;
  assert_count
    (+Expression.Name
        (Name.Attribute { Name.Attribute.base = integer 1; attribute = "x"; special = false }))
    ~expected:1;
  assert_count (+Expression.Set [integer 1; integer 2]) ~expected:3;
  assert_count
    (+Expression.SetComprehension
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:15;
  assert_count
    (+Expression.Starred (Starred.Once (+Expression.List [integer 1; integer 2])))
    ~expected:3;
  assert_count
    (+Expression.Ternary { Ternary.target = integer 1; test = integer 2; alternative = integer 3 })
    ~expected:6;
  assert_count (+Expression.Tuple [integer 1; integer 2]) ~expected:3;
  assert_count
    (+Expression.UnaryOperator
        { UnaryOperator.operator = UnaryOperator.Negative; operand = integer 1 })
    ~expected:1;
  assert_count
    (+Expression.WalrusOperator { WalrusOperator.target = !"x"; value = integer 1 })
    ~expected:1;
  assert_count (+Expression.Yield (Some (integer 1))) ~expected:1;
  assert_count (+Expression.YieldFrom (integer 1)) ~expected:1;
  ()


let test_default_mapper context =
  (* Define a mapper that increase all integer constants by 1 *)
  let mapper =
    let map_constant ~mapper:_ = function
      | Constant.Integer i -> Constant.Integer (i + 1)
      | _ as constant -> constant
    in
    Mapper.create_transformer ~map_constant ()
  in
  let assert_transformed ~expected expression =
    let actual = Mapper.map ~mapper expression in
    assert_equal
      ~ctxt:context
      ~cmp:(fun left right -> Int.equal 0 (Expression.location_insensitive_compare left right))
      ~printer:(fun expression -> Sexp.to_string_hum ([%sexp_of: Expression.t] expression))
      ~msg:"Unexpected transformed expression"
      expected
      actual
  in
  let integer i = +Expression.Constant (Constant.Integer i) in
  assert_transformed (+Expression.Await (integer 1)) ~expected:(+Expression.Await (integer 2));
  assert_transformed
    (+Expression.BooleanOperator
        { BooleanOperator.left = integer 1; operator = BooleanOperator.And; right = integer 2 })
    ~expected:
      (+Expression.BooleanOperator
          { BooleanOperator.left = integer 2; operator = BooleanOperator.And; right = integer 3 });
  assert_transformed
    (+Expression.Call
        {
          Call.callee = integer 1;
          arguments =
            [
              { Call.Argument.name = None; value = integer 2 };
              { Call.Argument.name = None; value = integer 3 };
            ];
        })
    ~expected:
      (+Expression.Call
          {
            Call.callee = integer 2;
            arguments =
              [
                { Call.Argument.name = None; value = integer 3 };
                { Call.Argument.name = None; value = integer 4 };
              ];
          });
  assert_transformed
    (+Expression.ComparisonOperator
        {
          ComparisonOperator.left = integer 1;
          operator = ComparisonOperator.Equals;
          right = integer 2;
        })
    ~expected:
      (+Expression.ComparisonOperator
          {
            ComparisonOperator.left = integer 2;
            operator = ComparisonOperator.Equals;
            right = integer 3;
          });
  assert_transformed
    (+Expression.Constant Constant.NoneLiteral)
    ~expected:(+Expression.Constant Constant.NoneLiteral);
  assert_transformed
    (+Expression.Dictionary
        {
          Dictionary.entries = [{ Dictionary.Entry.key = integer 1; value = integer 2 }];
          keywords = [integer 3];
        })
    ~expected:
      (+Expression.Dictionary
          {
            Dictionary.entries = [{ Dictionary.Entry.key = integer 2; value = integer 3 }];
            keywords = [integer 4];
          });
  assert_transformed
    (+Expression.DictionaryComprehension
        {
          Comprehension.element = { Dictionary.Entry.key = !"x"; value = integer 1 };
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:
      (+Expression.DictionaryComprehension
          {
            Comprehension.element = { Dictionary.Entry.key = !"x"; value = integer 2 };
            generators =
              [
                {
                  Comprehension.Generator.target = integer 3;
                  iterator = integer 4;
                  conditions = [integer 5; integer 6];
                  async = true;
                };
              ];
          });
  assert_transformed
    (+Expression.Generator
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:
      (+Expression.Generator
          {
            Comprehension.element = integer 2;
            generators =
              [
                {
                  Comprehension.Generator.target = integer 3;
                  iterator = integer 4;
                  conditions = [integer 5; integer 6];
                  async = true;
                };
              ];
          });
  assert_transformed
    (+Expression.FormatString [Substring.Literal (+"abc"); Substring.Format (integer 1)])
    ~expected:(+Expression.FormatString [Substring.Literal (+"abc"); Substring.Format (integer 2)]);
  assert_transformed
    (+Expression.Lambda
        {
          Lambda.parameters =
            [Parameter.create ~location:Location.any ~value:(integer 1) ~name:"x" ()];
          body = integer 2;
        })
    ~expected:
      (+Expression.Lambda
          {
            Lambda.parameters =
              [Parameter.create ~location:Location.any ~value:(integer 2) ~name:"x" ()];
            body = integer 3;
          });
  assert_transformed
    (+Expression.List [integer 1; integer 2])
    ~expected:(+Expression.List [integer 2; integer 3]);
  assert_transformed
    (+Expression.ListComprehension
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:
      (+Expression.ListComprehension
          {
            Comprehension.element = integer 2;
            generators =
              [
                {
                  Comprehension.Generator.target = integer 3;
                  iterator = integer 4;
                  conditions = [integer 5; integer 6];
                  async = true;
                };
              ];
          });
  assert_transformed !"x" ~expected:!"x";
  assert_transformed
    (+Expression.Name
        (Name.Attribute { Name.Attribute.base = integer 1; attribute = "x"; special = false }))
    ~expected:
      (+Expression.Name
          (Name.Attribute { Name.Attribute.base = integer 2; attribute = "x"; special = false }));
  assert_transformed
    (+Expression.Set [integer 1; integer 2])
    ~expected:(+Expression.Set [integer 2; integer 3]);
  assert_transformed
    (+Expression.SetComprehension
        {
          Comprehension.element = integer 1;
          generators =
            [
              {
                Comprehension.Generator.target = integer 2;
                iterator = integer 3;
                conditions = [integer 4; integer 5];
                async = true;
              };
            ];
        })
    ~expected:
      (+Expression.SetComprehension
          {
            Comprehension.element = integer 2;
            generators =
              [
                {
                  Comprehension.Generator.target = integer 3;
                  iterator = integer 4;
                  conditions = [integer 5; integer 6];
                  async = true;
                };
              ];
          });
  assert_transformed
    (+Expression.Starred (Starred.Once (+Expression.List [integer 1; integer 2])))
    ~expected:(+Expression.Starred (Starred.Once (+Expression.List [integer 2; integer 3])));
  assert_transformed
    (+Expression.Ternary { Ternary.target = integer 1; test = integer 2; alternative = integer 3 })
    ~expected:
      (+Expression.Ternary { Ternary.target = integer 2; test = integer 3; alternative = integer 4 });
  assert_transformed
    (+Expression.Tuple [integer 1; integer 2])
    ~expected:(+Expression.Tuple [integer 2; integer 3]);
  assert_transformed
    (+Expression.UnaryOperator
        { UnaryOperator.operator = UnaryOperator.Negative; operand = integer 1 })
    ~expected:
      (+Expression.UnaryOperator
          { UnaryOperator.operator = UnaryOperator.Negative; operand = integer 2 });
  assert_transformed
    (+Expression.WalrusOperator { WalrusOperator.target = !"x"; value = integer 1 })
    ~expected:(+Expression.WalrusOperator { WalrusOperator.target = !"x"; value = integer 2 });
  assert_transformed
    (+Expression.Yield (Some (integer 1)))
    ~expected:(+Expression.Yield (Some (integer 2)));
  assert_transformed
    (+Expression.YieldFrom (integer 1))
    ~expected:(+Expression.YieldFrom (integer 2));
  ()


let () =
  "expression"
  >::: [
         "negate" >:: test_negate;
         "normalize" >:: test_normalize;
         "pp" >:: test_pp;
         "equality" >:: test_equality;
         "delocalize" >:: test_delocalize;
         "comparison_operator_override" >:: test_comparison_operator_override;
         "exists_in_list" >:: test_exists_in_list;
         "create_name" >:: test_create_name;
         "name_to_identifiers" >:: test_name_to_identifiers;
         "name_equals" >:: test_name_equals;
         "arguments_location" >:: test_arguments_location;
         "default_folder" >:: test_default_folder;
         "default_mapper" >:: test_default_mapper;
       ]
  |> Test.run

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

module ModifyingTransformer : sig
  type t = int

  include Transform.Transformer with type t := t

  val final : t -> int
end = struct
  include Transform.Identity

  type t = int

  let final count = count

  let expression _ = function
    | { Node.location; value = Expression.Constant (Constant.Integer number) } ->
        { Node.location; value = Expression.Constant (Constant.Integer (number + 1)) }
    | expression -> expression
end

module ShallowModifyingTransformer : sig
  type t = int

  include Transform.Transformer with type t := t
end = struct
  include Transform.Identity
  include ModifyingTransformer

  let transform_children state _ = state, false
end

module ModifyingTransform = Transform.Make (ModifyingTransformer)
module ShallowModifyingTransform = Transform.Make (ShallowModifyingTransformer)

let assert_modifying_source ?(shallow = false) statements expected_statements expected_sum =
  let state, modified =
    if shallow then
      let { ShallowModifyingTransform.state; source } =
        ShallowModifyingTransform.transform 0 (Source.create statements)
      in
      state, source
    else
      let { ModifyingTransform.state; source } =
        ModifyingTransform.transform 0 (Source.create statements)
      in
      state, source
  in
  assert_source_equal (Source.create expected_statements) modified;
  assert_equal expected_sum (ModifyingTransformer.final state) ~printer:string_of_int


let test_transform _ =
  assert_modifying_source
    [
      +Statement.Expression (+Expression.Constant (Constant.Integer 1));
      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
    ]
    [
      +Statement.Expression (+Expression.Constant (Constant.Integer 2));
      +Statement.Expression (+Expression.Constant (Constant.Integer 3));
    ]
    0;
  assert_modifying_source
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
    ]
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             { target = !"a"; value = +Expression.Constant (Constant.Integer 2) });
    ]
    0;
  assert_modifying_source
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant (Constant.Integer 2);
                    body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
                    orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 4))];
                  };
             ];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 2);
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant (Constant.Integer 3);
                    body = [+Statement.Expression (+Expression.Constant (Constant.Integer 4))];
                    orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
                  };
             ];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 6))];
         };
    ]
    0;
  assert_modifying_source
    ~shallow:true
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant (Constant.Integer 2);
                    body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
                    orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 4))];
                  };
             ];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant (Constant.Integer 2);
                    body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
                    orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 4))];
                  };
             ];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]
    0;
  assert_modifying_source
    [
      +Statement.Match
         {
           Match.subject = +Expression.Constant (Constant.Integer 0);
           cases =
             [
               {
                 Match.Case.pattern = +Match.Pattern.MatchSingleton (Constant.Integer 2);
                 guard = Some (+Expression.Constant (Constant.Integer 4));
                 body = [];
               };
             ];
         };
    ]
    [
      +Statement.Match
         {
           Match.subject = +Expression.Constant (Constant.Integer 1);
           cases =
             [
               {
                 Match.Case.pattern = +Match.Pattern.MatchSingleton (Constant.Integer 3);
                 guard = Some (+Expression.Constant (Constant.Integer 5));
                 body = [];
               };
             ];
         };
    ]
    0


module ExpandingTransformer : sig
  type t = unit

  include Transform.Transformer with type t := t
end = struct
  include Transform.Identity

  type t = unit

  let statement state statement = state, [statement; statement]
end

module ShallowExpandingTransformer : sig
  type t = unit

  include Transform.Transformer with type t := t
end = struct
  include Transform.Identity
  include ExpandingTransformer

  let transform_children state _ = state, false
end

module ExpandingTransform = Transform.Make (ExpandingTransformer)
module ShallowExpandingTransform = Transform.Make (ShallowExpandingTransformer)

let assert_expanded_source ?(shallow = false) statements expected_statements =
  let modified =
    if shallow then
      ShallowExpandingTransform.transform () (Source.create statements)
      |> ShallowExpandingTransform.source
    else
      ExpandingTransform.transform () (Source.create statements) |> ExpandingTransform.source
  in
  assert_source_equal (Source.create expected_statements) modified


let test_expansion _ =
  assert_expanded_source
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
    ]
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
    ];
  assert_expanded_source
    ~shallow:true
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
    ]
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
    ];
  assert_expanded_source
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body =
             [
               +Statement.Expression (+Expression.Constant (Constant.Integer 3));
               +Statement.Expression (+Expression.Constant (Constant.Integer 3));
             ];
           orelse =
             [
               +Statement.Expression (+Expression.Constant (Constant.Integer 5));
               +Statement.Expression (+Expression.Constant (Constant.Integer 5));
             ];
         };
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body =
             [
               +Statement.Expression (+Expression.Constant (Constant.Integer 3));
               +Statement.Expression (+Expression.Constant (Constant.Integer 3));
             ];
           orelse =
             [
               +Statement.Expression (+Expression.Constant (Constant.Integer 5));
               +Statement.Expression (+Expression.Constant (Constant.Integer 5));
             ];
         };
    ];
  assert_expanded_source
    ~shallow:true
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Integer 1);
           body = [+Statement.Expression (+Expression.Constant (Constant.Integer 3))];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Integer 5))];
         };
    ]


let test_expansion_with_stop _ =
  let module StoppingExpandingTransformer : sig
    type t = unit

    include Transform.Transformer with type t := t
  end = struct
    include ExpandingTransformer

    let transform_children state _ = state, false
  end
  in
  let module StoppingExpandingTransform = Transform.Make (StoppingExpandingTransformer) in
  let assert_expanded_source_with_stop source expected_source =
    let modified =
      StoppingExpandingTransform.transform () (parse source) |> StoppingExpandingTransform.source
    in
    assert_source_equal ~location_insensitive:true (parse expected_source) modified
  in
  assert_expanded_source_with_stop
    {|
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
    |}
    {|
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
       if (1):
         if (2):
           3
         else:
           4
       else:
         if (5):
           6
         else:
           7
    |}


let test_double_count _ =
  let module DoubleCounterTransformer : sig
    type t = int

    include Transform.Transformer with type t := t
  end = struct
    include Transform.Identity

    type t = int

    let statement count statement = count + 1, [statement]
  end
  in
  let module ShallowDoubleCounterTransformer : sig
    type t = int

    include Transform.Transformer with type t := t
  end = struct
    include Transform.Identity
    include DoubleCounterTransformer

    let transform_children state _ = state, false
  end
  in
  let module DoubleCounterTransform = Transform.Make (DoubleCounterTransformer) in
  let module ShallowDoubleCounterTransform = Transform.Make (ShallowDoubleCounterTransformer) in
  let assert_double_count ?(shallow = false) source expected_sum =
    let state, modified =
      if shallow then
        let { ShallowDoubleCounterTransform.state; source } =
          ShallowDoubleCounterTransform.transform 0 (parse source)
        in
        state, source
      else
        let { DoubleCounterTransform.state; source } =
          DoubleCounterTransform.transform 0 (parse source)
        in
        state, source
    in
    (* expect no change in the source *)
    assert_source_equal (parse source) modified;
    assert_equal expected_sum (ModifyingTransformer.final state) ~printer:string_of_int
  in
  assert_double_count {|
      1.0
      2.0
    |} 2;
  assert_double_count ~shallow:true {|
      1.0
      2.0
    |} 2;
  assert_double_count {|
      if (1):
        3
      else:
        5
    |} 3;
  assert_double_count ~shallow:true {|
      if (1):
        3
      else:
        5
    |} 1;
  assert_double_count
    {|
      if (1):
        if (2):
          3
        else:
          4
      else:
        if (5):
          6
        else:
          7
    |}
    7;
  assert_double_count
    ~shallow:true
    {|
      if (1):
        if (2):
          3
        else:
          4
      else:
        if (5):
          6
        else:
          7
    |}
    1


let test_conditional_count _ =
  let module NestedCounterTransformer : sig
    type t = {
      is_nested: bool;
      statement_count: int;
    }

    include Transform.Transformer with type t := t
  end = struct
    type t = {
      is_nested: bool;
      statement_count: int;
    }

    let transform_expression_children _ _ = false

    let transform_children ({ is_nested; _ } as state) statement =
      match Node.value statement with
      | Statement.Class _ -> { state with is_nested = true }, true
      | _ -> state, is_nested


    let expression _ expression = expression

    let statement { statement_count; is_nested } statement =
      let is_nested, statement_count =
        (* Reset `is_nested` to false after leaving a Class *)
        match Node.value statement with
        | Statement.Class _ -> false, statement_count
        | _ -> is_nested, if is_nested then statement_count + 1 else statement_count
      in
      { is_nested; statement_count }, [statement]
  end
  in
  let module NestedCounterTransform = Transform.Make (NestedCounterTransformer) in
  let assert_conditional_count source expected_statement_count =
    let statement_count { NestedCounterTransformer.statement_count; _ } = statement_count in
    let state, modified =
      let { NestedCounterTransform.state; source } =
        NestedCounterTransform.transform { statement_count = 0; is_nested = false } (parse source)
      in
      state, source
    in
    assert_source_equal (parse source) modified;
    assert_equal expected_statement_count (statement_count state)
  in
  (* Test ability to count only statements nested in classes. *)
  assert_conditional_count {|
      1.0
      2.0
    |} 0;
  assert_conditional_count {|
      1.0
      class Foo:
        2.0
    |} 1;
  assert_conditional_count
    {|
      1.0
      class Foo:
        2.0
        def method(self) -> None:
          3.0
      4.0
    |}
    3;
  ()


let test_statement_transformer _ =
  let module ModifyingStatementTransformer : sig
    type t = int

    include Transform.StatementTransformer with type t := t

    val final : t -> int
  end = struct
    type t = int

    let final count = count

    let statement count { Node.location; value } =
      let count, value =
        match value with
        | Statement.Assign
            ({ Assign.value = { Node.value = Constant (Constant.Integer number); _ } as value; _ }
            as assign) ->
            ( count + number,
              Statement.Assign
                {
                  assign with
                  Assign.value =
                    { value with Node.value = Constant (Constant.Integer (number + 1)) };
                } )
        | _ -> count, value
      in
      count, [{ Node.location; value }]
  end
  in
  let module Transform = Transform.MakeStatementTransformer (ModifyingStatementTransformer) in
  let assert_transform source expected expected_sum =
    let { Transform.state; source = modified } = Transform.transform 0 (parse source) in
    assert_source_equal (parse expected) modified;
    assert_equal expected_sum (ModifyingStatementTransformer.final state) ~printer:string_of_int
  in
  assert_transform
    {|
      def foo():
        x = 1
        y = 2
      2
      3 + 4
      x = 3
      y = 4
      if 1 == 3:
        x = 5
      else:
        if a > b:
          y = 6
      class C:
        z = 7
      match x:
        case 1:
          w = 0
    |}
    {|
      def foo():
        x = 2
        y = 3
      2
      3 + 4
      x = 4
      y = 5
      if 1 == 3:
        x = 6
      else:
        if a > b:
          y = 7
      class C:
        z = 8
      match x:
        case 1:
          w = 1
    |}
    28


let test_transform_in_statement _ =
  let keep_first_argument = function
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Identifier given_callee_name); location };
          arguments = argument :: _;
        } ->
        Expression.Call
          {
            Call.callee = { Node.value = Name (Identifier given_callee_name); location };
            arguments = [argument];
          }
    | expression -> expression
  in
  let assert_transform given expected =
    match parse given |> Source.statements, parse expected |> Source.statements with
    | [{ Node.value = given; _ }], [{ Node.value = expected; _ }] ->
        let actual = Transform.transform_in_statement ~transform:keep_first_argument given in
        let printer x = [%sexp_of: Statement.statement] x |> Sexp.to_string_hum in
        assert_equal
          ~cmp:(fun left right ->
            Statement.location_insensitive_compare
              (Node.create_with_default_location left)
              (Node.create_with_default_location right)
            = 0)
          ~printer
          ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
          expected
          actual
    | _ -> failwith "expected one statement each"
  in
  assert_transform
    {|
      def foo():
        bar(1, 2)
        x = bar(bar(1, bar(2, 3)), bar(4, 5))
        some_other_function()
    |}
    {|
      def foo():
        bar(1)
        x = bar(bar(1))
        some_other_function()
    |};
  ()


let test_transform_in_expression _ =
  let keep_first_argument = function
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Identifier given_callee_name); location };
          arguments = argument :: _;
        } ->
        Expression.Call
          {
            Call.callee = { Node.value = Name (Identifier given_callee_name); location };
            arguments = [argument];
          }
    | expression -> expression
  in
  let assert_transform given expected =
    let actual =
      parse_single_expression given
      |> Transform.transform_in_expression ~transform:keep_first_argument
    in
    let printer x = [%sexp_of: Expression.t] x |> Sexp.to_string_hum in
    assert_equal
      ~cmp:(fun left right -> Expression.location_insensitive_compare left right = 0)
      ~printer
      ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
      (parse_single_expression expected)
      actual
  in
  assert_transform {|
      bar(bar(1, bar(2, 3)), bar(4, 5))
    |} {|
      bar(bar(1))
    |};
  assert_transform {|
      bar()
    |} {|
      bar()
    |};
  ()


let test_sanitize_statement _ =
  let assert_sanitized statements expected =
    let given_statement, expected_statement =
      match statements, parse expected |> Source.statements with
      | [{ Node.value = given_statement; _ }], [{ Node.value = expected_statement; _ }] ->
          given_statement, expected_statement
      | _ -> failwith "Expected defines"
    in
    assert_equal
      ~cmp:(fun left right -> Statement.location_insensitive_compare left right = 0)
      ~printer:[%show: Statement.t]
      (expected_statement |> Node.create_with_default_location)
      (Transform.sanitize_statement given_statement |> Node.create_with_default_location)
  in
  assert_sanitized
    [
      +Statement.Define
         {
           Define.signature =
             {
               Define.Signature.name = !&"$local_test?foo$bar";
               parameters =
                 [+{ Parameter.name = "$parameter$a"; value = None; annotation = Some !"int" }];
               decorators = [];
               return_annotation = Some !"int";
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"$local_test?foo?bar$my_kwargs";
                    annotation = None;
                    value =
                      +Expression.Dictionary
                         {
                           Dictionary.entries =
                             [
                               {
                                 Dictionary.Entry.key =
                                   +Expression.Constant (Constant.String (StringLiteral.create "a"));
                                 value = !"$parameter$a";
                               };
                             ];
                           keywords = [];
                         };
                  };
               +Statement.Expression
                  (+Expression.Call
                      {
                        Call.callee = !"print";
                        arguments =
                          [{ Call.Argument.name = None; value = !"$local_test?foo?bar$my_kwargs" }];
                      });
               +Statement.Return
                  {
                    Return.is_implicit = false;
                    expression =
                      Some
                        (+Expression.Call
                            {
                              Call.callee = !"$local_test?foo$baz";
                              arguments = [{ Call.Argument.name = None; value = !"$parameter$a" }];
                            });
                  };
             ];
         };
    ]
    {|
    def bar(a: int) -> int:
      my_kwargs = { "a":a }
      print(my_kwargs)
      return baz(a)
    |};
  assert_sanitized
    [
      +Statement.Define
         {
           Define.signature =
             {
               Define.Signature.name = !&"bar";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = Some !"int" }];
               decorators = [];
               return_annotation = Some !"int";
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Assign
                  {
                    Assign.target = !"my_kwargs";
                    annotation = None;
                    value =
                      +Expression.Dictionary
                         {
                           Dictionary.entries =
                             [
                               {
                                 Dictionary.Entry.key =
                                   +Expression.Constant (Constant.String (StringLiteral.create "a"));
                                 value = !"a";
                               };
                             ];
                           keywords = [];
                         };
                  };
               +Statement.Expression
                  (+Expression.Call
                      {
                        Call.callee = !"print";
                        arguments = [{ Call.Argument.name = None; value = !"my_kwargs" }];
                      });
               +Statement.Return
                  {
                    Return.is_implicit = false;
                    expression =
                      Some
                        (+Expression.Call
                            {
                              Call.callee = !"baz";
                              arguments = [{ Call.Argument.name = None; value = !"a" }];
                            });
                  };
             ];
         };
    ]
    {|
    def bar(a: int) -> int:
      my_kwargs = { "a":a }
      print(my_kwargs)
      return baz(a)
    |};
  ()


let () =
  "transform"
  >::: [
         "transform" >:: test_transform;
         "expansion" >:: test_expansion;
         "expansion_with_stop" >:: test_expansion_with_stop;
         "statement_double_counter" >:: test_double_count;
         "statement_conditional_counter" >:: test_conditional_count;
         "statement_transformer" >:: test_statement_transformer;
         "transform_in_statement" >:: test_transform_in_statement;
         "transform_in_expression" >:: test_transform_in_expression;
         "sanitize_statement" >:: test_sanitize_statement;
       ]
  |> Test.run

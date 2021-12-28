(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open OUnit2
open Analysis
open Test

let test_prepare_arguments_for_signature_selection _ =
  let open AttributeResolution in
  let assert_prepared_arguments ~self_argument arguments expected =
    let actual =
      SignatureSelection.prepare_arguments_for_signature_selection ~self_argument arguments
    in
    assert_equal
      ~printer:[%show: Argument.WithPosition.t list]
      ~cmp:[%compare.equal: Argument.WithPosition.t list]
      expected
      actual
  in
  assert_prepared_arguments ~self_argument:None [] [];
  assert_prepared_arguments
    ~self_argument:(Some (Type.parametric "Foo" []))
    [
      {
        Argument.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
      };
      {
        Argument.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
      };
    ]
    [
      {
        Argument.WithPosition.resolved = Type.parametric "Foo" [];
        kind = Positional;
        expression = None;
        position = 0;
      };
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
    ];
  assert_prepared_arguments
    ~self_argument:None
    [
      {
        Argument.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
      };
      {
        Argument.resolved = Type.tuple [Type.integer; Type.string; Type.bool];
        kind = SingleStar;
        expression = None;
      };
      {
        Argument.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
      };
    ]
    [
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
        position = 5;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = None;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Positional;
        expression = None;
        position = 3;
      };
      {
        Argument.WithPosition.resolved = Type.bool;
        kind = Positional;
        expression = None;
        position = 4;
      };
    ];
  ()


let test_get_parameter_argument_mapping _ =
  let open AttributeResolution in
  let open Type.Callable in
  let assert_parameter_argument_mapping ~callable ~self_argument arguments expected =
    let parameters =
      match parse_callable callable with
      | Type.Callable { implementation = { parameters = Defined parameters; _ }; _ } -> parameters
      | _ -> failwith "expected defined parameters"
    in
    let actual =
      SignatureSelection.get_parameter_argument_mapping
        ~all_parameters:(Defined parameters)
        ~parameters
        ~self_argument
        arguments
    in
    assert_equal
      ~pp_diff:(diff ~print:ParameterArgumentMapping.pp)
      ~printer:[%show: ParameterArgumentMapping.t]
      ~cmp:[%compare.equal: ParameterArgumentMapping.t]
      expected
      actual
  in
  assert_parameter_argument_mapping
    ~callable:
      "typing.Callable[[PositionalOnly(int), Named(some_argument, str), Variable(bool)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.string;
        kind = Named (Node.create_with_default_location "some_argument");
        expression = parse_single_expression "'hello'" |> Option.some;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "some_argument"; annotation = Type.string; default = false },
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.string;
                    kind = Named (Node.create_with_default_location "some_argument");
                    expression = parse_single_expression "'hello'" |> Option.some;
                    position = 2;
                  };
              ] );
            ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.integer;
                    kind = Positional;
                    expression = parse_single_expression "42" |> Option.some;
                    position = 1;
                  };
              ] );
            Variable (Concrete Type.bool), [];
          ];
      reasons = { arity = []; annotation = [] };
    };
  (* TODO(T107236583): Handle `foo(x, *args)` correctly. *)
  assert_parameter_argument_mapping
    ~callable:
      "typing.Callable[[PositionalOnly(int), Named(some_argument, str), Variable(bool)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "some_argument"; annotation = Type.string; default = false },
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 2;
                  };
              ] );
            ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 2;
                  };
              ] );
            ( Variable (Concrete Type.bool),
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.integer;
                    kind = Positional;
                    expression = parse_single_expression "42" |> Option.some;
                    position = 1;
                  };
                Argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 2;
                  };
              ] );
          ];
      reasons = { arity = []; annotation = [] };
    };
  (* TODO(T107236583): We should raise an error about the extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    { parameter_argument_mapping = Parameter.Map.empty; reasons = { arity = []; annotation = [] } };
  (* TODO(T107236583): We should raise an error about the extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 2;
      };
    ]
    {
      parameter_argument_mapping = Parameter.Map.empty;
      reasons =
        {
          arity = [SignatureSelectionTypes.TooManyArguments { expected = 0; provided = 1 }];
          annotation = [];
        };
    };
  (* We collect multiple starred arguments under the same `*args` parameter. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Variable(int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 2;
      };
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 3;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Variable (Concrete Type.integer),
              [
                Argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 3;
                  };
                Argument
                  {
                    Argument.WithPosition.resolved = Type.integer;
                    kind = Positional;
                    expression = parse_single_expression "42" |> Option.some;
                    position = 2;
                  };
                Argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
          ];
      reasons = { arity = []; annotation = [] };
    };
  (* TODO(T107236583): We mistakenly count `provided` arguments as 3. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Keywords(int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 2;
      };
    ]
    {
      parameter_argument_mapping = Parameter.Map.empty;
      reasons =
        {
          arity = [SignatureSelectionTypes.TooManyArguments { expected = 1; provided = 3 }];
          annotation = [];
        };
    };
  ()


let () =
  "attributeResolution"
  >::: [
         "prepare_arguments" >:: test_prepare_arguments_for_signature_selection;
         "parameter_argument_mapping" >:: test_get_parameter_argument_mapping;
       ]
  |> Test.run

(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
      ~printer:[%show: Type.t Argument.WithPosition.t list]
      ~cmp:[%compare.equal: Type.t Argument.WithPosition.t list]
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
        position = 3;
      };
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.string; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 2;
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
      ~pp_diff:(diff ~print:ParameterArgumentMapping.pp_with_resolved_type)
      ~printer:(Format.asprintf "%a" ParameterArgumentMapping.pp_with_resolved_type)
      ~cmp:ParameterArgumentMapping.equal_mapping_with_resolved_type
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
                make_matched_argument
                  {
                    Argument.WithPosition.resolved = Type.string;
                    kind = Named (Node.create_with_default_location "some_argument");
                    expression = parse_single_expression "'hello'" |> Option.some;
                    position = 2;
                  };
              ] );
            ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
              [
                make_matched_argument
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
  assert_parameter_argument_mapping
    ~callable:
      "typing.Callable[[PositionalOnly(int), Named(some_argument, str), Variable(bool)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.integer;
        kind = Positional;
        expression = parse_single_expression "42" |> Option.some;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 2;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  {
                    Argument.WithPosition.resolved = Type.integer;
                    kind = Positional;
                    expression = parse_single_expression "42" |> Option.some;
                    position = 1;
                  };
              ] );
            ( Named { name = "some_argument"; annotation = Type.string; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string; Type.bool; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 2;
                  };
              ] );
            ( Variable (Concrete Type.bool),
              [
                make_matched_argument
                  ~index_into_starred_tuple:1
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
                make_matched_argument
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 3;
                  };
                make_matched_argument
                  {
                    Argument.WithPosition.resolved = Type.integer;
                    kind = Positional;
                    expression = parse_single_expression "42" |> Option.some;
                    position = 2;
                  };
                make_matched_argument
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
  (* TODO(T107236583): We fail to complain about extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Keywords(int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    { parameter_argument_mapping = Parameter.Map.empty; reasons = { arity = []; annotation = [] } };
  (* TODO(T107236583): We fail to complain about extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Keywords(int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.dictionary ~key:Type.string ~value:Type.integer;
        kind = DoubleStar;
        expression = None;
        position = 2;
      };
    ]
    { parameter_argument_mapping = Parameter.Map.empty; reasons = { arity = []; annotation = [] } };
  (* TODO(T107236583): We fail to complain about extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[KeywordOnly(x, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [KeywordOnly { name = "x"; annotation = Type.integer; default = false }, []];
      reasons = { arity = []; annotation = [] };
    };
  (* TODO(T107236583): We fail to complain about extra `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[KeywordOnly(x, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer; Type.integer];
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
  (* TODO(T107236583): We currently store the argument for a named parameter as the entire `*args`. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Named(x, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.string; Type.bool];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "x"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved =
                      Type.tuple [Type.integer; Type.string; Type.bool];
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
    ~callable:"typing.Callable[[Named(x, int), Named(y, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.string; Type.bool];
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
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "x"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved =
                      Type.tuple [Type.integer; Type.string; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Named { name = "y"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:1
                  {
                    Argument.WithPosition.resolved =
                      Type.tuple [Type.integer; Type.string; Type.bool];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
          ];
      reasons =
        {
          arity = [SignatureSelectionTypes.TooManyArguments { expected = 2; provided = 3 }];
          annotation = [];
        };
    };
  let tuple_str_unbounded_int =
    Type.OrderedTypes.Concatenation.create_from_unbounded_element ~prefix:[Type.string] Type.integer
  in
  (* We slice the `*xs` argument to assign it across `x: str` and `*args: *Tuple[<...>]`. *)
  assert_parameter_argument_mapping
    ~callable:
      "typing.Callable[[Named(x, str), Variable(str, pyre_extensions.Unpack[typing.Tuple[int, \
       ...]])], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved =
          Type.tuple [Type.string; Type.string; Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "x"; annotation = Type.string; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved =
                      Type.tuple [Type.string; Type.string; Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Variable (Concatenation tuple_str_unbounded_int),
              [
                make_matched_argument
                  ~index_into_starred_tuple:1
                  {
                    Argument.WithPosition.resolved =
                      Type.tuple [Type.string; Type.string; Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
          ];
      reasons = { arity = []; annotation = [] };
    };
  (* TODO(T107236583): Need to handle the case where there are multiple `*xs` that get used up by
     positional parameters. *)
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Named(x, str), Named(y, str), Named(z, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
      {
        Argument.WithPosition.resolved = Type.tuple [Type.string; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 2;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "x"; annotation = Type.string; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Named { name = "y"; annotation = Type.string; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:1
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Named { name = "z"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:2
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.string];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
          ];
      reasons = { arity = []; annotation = [] };
    };
  assert_parameter_argument_mapping
    ~callable:"typing.Callable[[Named(x, int), Named(y, int), Named(z, int)], None]"
    ~self_argument:None
    [
      {
        Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
        kind = SingleStar;
        expression = None;
        position = 1;
      };
    ]
    {
      parameter_argument_mapping =
        Parameter.Map.of_alist_exn
          [
            ( Named { name = "x"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:0
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Named { name = "y"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:1
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
            ( Named { name = "z"; annotation = Type.integer; default = false },
              [
                make_matched_argument
                  ~index_into_starred_tuple:2
                  {
                    Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                    kind = SingleStar;
                    expression = None;
                    position = 1;
                  };
              ] );
          ];
      reasons = { arity = []; annotation = [] };
    };
  ()


let test_check_arguments_against_parameters context =
  let open AttributeResolution in
  let open Type.Callable in
  let open Type.OrderedTypes in
  let assert_arguments_against_parameters
      ~callable
      ~parameter_argument_mapping_with_reasons
      ?(expected_reasons = empty_reasons)
      expected_constraints_set
    =
    let order =
      ScratchProject.setup ~context ["test.py", ""]
      |> ScratchProject.build_global_environment
      |> (fun { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } -> global_environment)
      |> GlobalResolution.create
      |> GlobalResolution.full_order
    in
    let parse_callable_record callable =
      match parse_callable callable with
      | Type.Callable ({ implementation = { parameters = Defined _; _ }; _ } as callable_record) ->
          callable_record
      | _ -> failwith "expected defined parameters"
    in
    let { constraints_set = actual_constraints_set; reasons = actual_reasons; _ } =
      SignatureSelection.check_arguments_against_parameters
        ~order
        ~resolve_mutable_literals:(fun ~resolve:_ ~expression:_ ~resolved ~expected:_ ->
          WeakenMutableLiterals.make_weakened_type resolved)
        ~resolve_with_locals:(fun ~locals:_ _ -> failwith "don't care")
        ~callable:(parse_callable_record callable)
        parameter_argument_mapping_with_reasons
    in
    let print_reasons format reasons = Format.fprintf format "%s" ([%show: reasons] reasons) in
    assert_equal
      ~pp_diff:
        (diff ~print:(fun format x ->
             Format.fprintf format "%s" ([%show: TypeConstraints.t list] x)))
      ~printer:[%show: TypeConstraints.t list]
      ~cmp:[%compare.equal: TypeConstraints.t list]
      expected_constraints_set
      actual_constraints_set;
    assert_equal
      ~pp_diff:(diff ~print:print_reasons)
      ~printer:(Format.asprintf "%a" print_reasons)
      ~cmp:(fun left right -> location_insensitive_compare_reasons left right = 0)
      expected_reasons
      actual_reasons
  in
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Variable(int)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concrete Type.integer),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  let ordered_type_str_int_unbounded_int =
    Type.OrderedTypes.Concatenation.create_from_unbounded_element
      ~prefix:[Type.string; Type.integer]
      Type.integer
  in
  (* Check `*Tuple[str, int, *Tuple[int, ...]]` after dropping a prefix of one type. This leaves
     `*Tuple[int, *Tuple[int, ...]]`, which is compatible against the expected type `*Tuple[int,
     ...]`. *)
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[Named(x, str), Variable(pyre_extensions.Unpack[typing.Tuple[int, ...]])], \
       None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Named { name = "x"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Concatenation ordered_type_str_int_unbounded_int);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Variable
                  (Concatenation
                     (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer)),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Concatenation ordered_type_str_int_unbounded_int);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  let tuple_int_str_int_unbounded_int =
    Type.Tuple
      (Concatenation
         (Type.OrderedTypes.Concatenation.create_from_unbounded_element
            ~prefix:[Type.integer; Type.string; Type.integer]
            Type.integer))
  in
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[PositionalOnly(int), Named(some_argument, str), Variable(int)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "some_argument"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Variable (Concrete Type.integer),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[PositionalOnly(int), Named(argument, str), Named(argument2, int), \
       Named(argument3, int)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "argument"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "argument2"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "argument3"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:3
                    {
                      Argument.WithPosition.resolved = tuple_int_str_int_unbounded_int;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    ~expected_reasons:{ arity = []; annotation = [] }
    [TypeConstraints.empty];
  let tuple_int_unbounded_int =
    Type.OrderedTypes.Concatenation.create_from_unbounded_element
      ~prefix:[Type.integer]
      Type.integer
  in
  (* Pass an unpacked list to `*args: *Tuple[int, *Tuple[int, ...]]`. *)
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[Variable(int, pyre_extensions.Unpack[typing.Tuple[int, ...]])], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concatenation tuple_int_unbounded_int),
                [
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  (* Pass multiple unpacked lists to `*args: *Tuple[int, *Tuple[int, ...]]`. *)
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[Variable(int, pyre_extensions.Unpack[typing.Tuple[int, ...]])], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concatenation tuple_int_unbounded_int),
                [
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 2;
                    };
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.integer;
                      kind = Positional;
                      expression = None;
                      position = 3;
                    };
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 4;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  (* Pass multiple, heterogeneous, unpacked lists to `*args: *Tuple[int, *Tuple[int, ...]]`. *)
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[Variable(int, pyre_extensions.Unpack[typing.Tuple[int, ...]])], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concatenation tuple_int_unbounded_int),
                [
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.string;
                      kind = SingleStar;
                      expression = None;
                      position = 2;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    ~expected_reasons:
      {
        arity =
          [
            SignatureSelectionTypes.Mismatches
              [
                MismatchWithUnpackableType
                  {
                    variable =
                      Concatenation
                        (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                           ~prefix:[Type.integer]
                           Type.integer);
                    mismatch =
                      ConstraintFailure
                        (Concatenation
                           (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                              (Type.union [Type.integer; Type.string])));
                  };
              ];
          ];
        annotation = [];
      }
    [TypeConstraints.empty];
  let tuple_unbounded_int =
    Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer
  in
  (* Pass an unpacked list of strings to `*args: *Tuple[int, ...]`. *)
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Variable(pyre_extensions.Unpack[typing.Tuple[int, ...]])], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concatenation tuple_unbounded_int),
                [
                  make_matched_argument
                    {
                      Argument.WithPosition.resolved = Type.list Type.string;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    ~expected_reasons:
      {
        arity =
          [
            SignatureSelectionTypes.Mismatches
              [
                Mismatch
                  ({
                     SignatureSelectionTypes.actual = Type.string;
                     expected = Type.integer;
                     name = None;
                     position = 1;
                   }
                  |> Node.create_with_default_location);
              ];
          ];
        annotation = [];
      }
    [TypeConstraints.empty];
  (* Pass part of an unpacked tuple to `*args: *Tuple[int, ...]`. *)
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Variable(pyre_extensions.Unpack[typing.Tuple[int, ...]])], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concatenation tuple_unbounded_int),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.string];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    ~expected_reasons:
      {
        arity =
          [
            SignatureSelectionTypes.Mismatches
              [
                Mismatch
                  ({
                     SignatureSelectionTypes.actual = Type.string;
                     expected = Type.integer;
                     name = None;
                     position = 1;
                   }
                  |> Node.create_with_default_location);
              ];
          ];
        annotation = [];
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Variable(int)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concrete Type.integer),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = Type.list Type.integer;
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Variable(typing.object)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Variable (Concrete Type.object_primitive),
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved =
                        (Type.Variable.Variadic.Parameters.create "P"
                        |> Type.Variable.Variadic.Parameters.decompose
                        |> fun { positional_component; _ } -> positional_component);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = empty_reasons;
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Named(x, int), Named(y, int), Named(z, str)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Named { name = "x"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "y"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "z"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    ~expected_reasons:
      { arity = [SignatureSelectionTypes.MissingArgument (Named "z")]; annotation = [] }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[PositionalOnly(int), PositionalOnly(str), PositionalOnly(bool)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( PositionalOnly { index = 1; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( PositionalOnly { index = 2; annotation = Type.bool; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    ~expected_reasons:
      { arity = [SignatureSelectionTypes.MissingArgument (PositionalOnly 2)]; annotation = [] }
    [TypeConstraints.empty];
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let ordered_type_int_variadic =
    Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] variadic
  in
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[PositionalOnly(int), PositionalOnly(str), PositionalOnly(bool)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( PositionalOnly { index = 0; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Concatenation ordered_type_int_variadic);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( PositionalOnly { index = 1; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Concatenation ordered_type_int_variadic);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( PositionalOnly { index = 2; annotation = Type.bool; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved =
                        Type.Tuple (Concatenation ordered_type_int_variadic);
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    ~expected_reasons:
      {
        arity = [];
        annotation =
          [
            SignatureSelectionTypes.Mismatches
              [
                Mismatch
                  ({
                     SignatureSelectionTypes.actual = Type.object_primitive;
                     expected = Type.bool;
                     name = None;
                     position = 3;
                   }
                  |> Node.create_with_default_location);
              ];
            SignatureSelectionTypes.Mismatches
              [
                Mismatch
                  ({
                     SignatureSelectionTypes.actual = Type.object_primitive;
                     expected = Type.string;
                     name = None;
                     position = 2;
                   }
                  |> Node.create_with_default_location);
              ];
          ];
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[Named(x, int), Named(y, str)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Named { name = "x"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "y"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved = Type.tuple [Type.integer; Type.integer];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    ~expected_reasons:
      {
        arity = [];
        annotation =
          [
            SignatureSelectionTypes.Mismatches
              [
                Mismatch
                  ({
                     SignatureSelectionTypes.actual = Type.integer;
                     expected = Type.string;
                     name = None;
                     position = 2;
                   }
                  |> Node.create_with_default_location);
              ];
          ];
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:
      "typing.Callable[[Named(x, int), Named(y, str), Named(z, bool), Named(a, int, default)], \
       None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( Named { name = "x"; annotation = Type.integer; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved =
                        Type.tuple [Type.integer; Type.string; Type.bool];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "y"; annotation = Type.string; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:1
                    {
                      Argument.WithPosition.resolved =
                        Type.tuple [Type.integer; Type.string; Type.bool];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "z"; annotation = Type.bool; default = false },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:2
                    {
                      Argument.WithPosition.resolved =
                        Type.tuple [Type.integer; Type.string; Type.bool];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
              ( Named { name = "a"; annotation = Type.bool; default = true },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:3
                    {
                      Argument.WithPosition.resolved =
                        Type.tuple [Type.integer; Type.string; Type.bool];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    [TypeConstraints.empty];
  assert_arguments_against_parameters
    ~callable:"typing.Callable[[PositionalOnly(int, default)], None]"
    ~parameter_argument_mapping_with_reasons:
      {
        parameter_argument_mapping =
          Parameter.Map.of_alist_exn
            [
              ( PositionalOnly { index = 0; annotation = Type.integer; default = true },
                [
                  make_matched_argument
                    ~index_into_starred_tuple:0
                    {
                      Argument.WithPosition.resolved = Type.tuple [];
                      kind = SingleStar;
                      expression = None;
                      position = 1;
                    };
                ] );
            ];
        reasons = { arity = []; annotation = [] };
      }
    [TypeConstraints.empty];
  ()


let test_most_important_error_reason _ =
  let open AttributeResolution in
  let open SignatureSelectionTypes in
  let assert_most_important_error_reason
      ~arity_mismatch_reasons
      ~annotation_mismatch_reasons
      expected
    =
    assert_equal
      ~printer:[%show: reason option]
      ~cmp:[%compare.equal: reason option]
      expected
      (SignatureSelection.most_important_error_reason
         ~arity_mismatch_reasons
         annotation_mismatch_reasons)
  in
  assert_most_important_error_reason ~arity_mismatch_reasons:[] ~annotation_mismatch_reasons:[] None;
  assert_most_important_error_reason
    ~arity_mismatch_reasons:[]
    ~annotation_mismatch_reasons:[UnexpectedKeyword "foo"]
    (Some (UnexpectedKeyword "foo"));
  (* `Mismatch` is less important than `UnexpectedKeyword`, but because it comes under `arity` it
     takes precedence. *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "foo"; position = 1 });
          ];
      ]
    ~annotation_mismatch_reasons:[UnexpectedKeyword "foo"]
    (Some
       (Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "foo"; position = 1 });
          ]));
  (* `UnexpectedKeyword` beats `Mismatch`. *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "foo"; position = 1 });
          ];
        UnexpectedKeyword "foo";
      ]
    ~annotation_mismatch_reasons:[]
    (Some (UnexpectedKeyword "foo"));
  (* We merge and sort `Mismatch`-es. *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "foo"; position = 2 });
          ];
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "bar"; position = 1 });
          ];
      ]
    ~annotation_mismatch_reasons:[]
    (Some
       (Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "bar"; position = 1 });
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "foo"; position = 2 });
          ]));
  (* We filter out `TooManyArguments` when the number of expected arguments is -1 (i.e., the method
     has no `self` parameter.). *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [TooManyArguments { expected = -1; provided = 1 }; UnexpectedKeyword "foo"]
    ~annotation_mismatch_reasons:[]
    (Some (UnexpectedKeyword "foo"));
  (* We filter out mismatches on the 0th position, i.e., the argument for the `self` parameter. *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "bar"; position = 1 });
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "self"; position = 0 });
          ];
      ]
    ~annotation_mismatch_reasons:[]
    (Some
       (Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 { actual = Type.integer; expected = Type.string; name = Some "bar"; position = 1 });
          ]));
  (* We preserve mismatches for the `self` parameter if it the given argument is readonly, since
     that means we have called a mutating method on a readonly object. *)
  assert_most_important_error_reason
    ~arity_mismatch_reasons:
      [
        Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 {
                   actual = Type.ReadOnly.create (Type.Primitive "Foo");
                   expected = Type.Primitive "Foo";
                   name = Some "self";
                   position = 0;
                 });
          ];
      ]
    ~annotation_mismatch_reasons:[]
    (Some
       (Mismatches
          [
            Mismatch
              (Node.create_with_default_location
                 {
                   actual = Type.ReadOnly.create (Type.Primitive "Foo");
                   expected = Type.Primitive "Foo";
                   name = Some "self";
                   position = 0;
                 });
          ]));
  ()


let () =
  "attributeResolution"
  >::: [
         "prepare_arguments" >:: test_prepare_arguments_for_signature_selection;
         "parameter_argument_mapping" >:: test_get_parameter_argument_mapping;
         "check_arguments_against_parameters" >:: test_check_arguments_against_parameters;
         "most_important_error_reason" >:: test_most_important_error_reason;
       ]
  |> Test.run

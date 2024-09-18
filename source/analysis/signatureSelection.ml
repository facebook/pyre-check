(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast

module Argument = struct
  type 'argument_type t = {
    expression: Expression.t option;
    kind: Ast.Expression.Call.Argument.kind;
    resolved: 'argument_type;
  }

  module WithPosition = struct
    type 'argument_type t = {
      position: int;
      expression: Expression.t option;
      kind: Ast.Expression.Call.Argument.kind;
      resolved: 'argument_type;
    }
    [@@deriving compare, show]
  end
end

type 'argument_type matched_argument =
  | MatchedArgument of {
      argument: 'argument_type Argument.WithPosition.t;
      index_into_starred_tuple: int option;
    }
  | Default
[@@deriving compare, show]

let make_matched_argument ?index_into_starred_tuple argument =
  MatchedArgument { argument; index_into_starred_tuple }


type ranks = {
  arity: int;
  annotation: int;
  position: int;
}
[@@deriving compare, show]

type reasons = {
  arity: SignatureSelectionTypes.reason list;
  annotation: SignatureSelectionTypes.reason list;
}
[@@deriving compare, show]

type extracted_ordered_type = {
  ordered_type: Type.OrderedTypes.t;
  argument: Type.t Argument.WithPosition.t;
  item_type_for_error: Type.t;
}

let location_insensitive_compare_reasons
    { arity = left_arity; annotation = left_annotation }
    { arity = right_arity; annotation = right_annotation }
  =
  match
    List.compare SignatureSelectionTypes.location_insensitive_compare_reason left_arity right_arity
  with
  | x when not (Int.equal x 0) -> x
  | _ ->
      List.compare
        SignatureSelectionTypes.location_insensitive_compare_reason
        left_annotation
        right_annotation


let empty_reasons = { arity = []; annotation = [] }

module ParameterArgumentMapping = struct
  type 'argument_type t = {
    parameter_argument_mapping:
      'argument_type matched_argument list Type.Callable.CallableParamType.Map.t;
    reasons: reasons;
  }

  let empty =
    {
      parameter_argument_mapping = Type.Callable.CallableParamType.Map.empty;
      reasons = empty_reasons;
    }


  let equal_mapping_with_resolved_type
      ({ parameter_argument_mapping = left_mapping; reasons = left_reasons } : Type.t t)
      { parameter_argument_mapping = right_mapping; reasons = right_reasons }
    =
    [%compare.equal: Type.t matched_argument list Type.Callable.CallableParamType.Map.t]
      left_mapping
      right_mapping
    && [%compare.equal: reasons] left_reasons right_reasons


  let pp_with_resolved_type format { parameter_argument_mapping; reasons } =
    Format.fprintf
      format
      "ParameterArgumentMapping { parameter_argument_mapping: %s; reasons: %a }"
      ([%show: (Type.Callable.CallableParamType.parameter * Type.t matched_argument list) list]
         (Map.to_alist parameter_argument_mapping))
      pp_reasons
      reasons
end

type signature_match = {
  callable: Type.Callable.t;
  parameter_argument_mapping: Type.t matched_argument list Type.Callable.CallableParamType.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}
[@@deriving compare]

let pp_signature_match
    format
    { callable; parameter_argument_mapping; constraints_set; ranks; reasons }
  =
  Format.fprintf
    format
    "{ callable = %a; parameter_argument_mapping = %s; constraints_set = %s; ranks = %a; reasons = \
     %a }"
    Type.Callable.pp
    callable
    ([%show: (Type.Callable.CallableParamType.parameter * Type.t matched_argument list) list]
       (Map.to_alist parameter_argument_mapping))
    ([%show: TypeConstraints.t list] constraints_set)
    pp_ranks
    ranks
    pp_reasons
    reasons


let show_signature_match = Format.asprintf "%a" pp_signature_match

let reserved_position_for_self_argument = 0

let get_location ~expression ~default = expression >>| Node.location |> Option.value ~default

let prepare_arguments_for_signature_selection ~self_argument arguments =
  let add_positions arguments =
    let add_index index { Argument.expression; kind; resolved } =
      { Argument.WithPosition.position = index + 1; expression; kind; resolved }
    in
    List.mapi ~f:add_index arguments
  in
  let separate_labeled_unlabeled_arguments arguments =
    let classify_argument argument =
      match argument with
      | { Argument.WithPosition.kind = DoubleStar; _ } -> false
      | _ -> true
    in
    let positional_and_named_args, kwargs = List.partition_tf arguments ~f:classify_argument in
    let self_argument =
      self_argument
      >>| (fun resolved ->
            {
              Argument.WithPosition.position = reserved_position_for_self_argument;
              expression = None;
              kind = Positional;
              resolved;
            })
      |> Option.to_list
    in
    self_argument @ positional_and_named_args @ kwargs
  in
  arguments |> add_positions |> separate_labeled_unlabeled_arguments


(** Return a mapping from each parameter to the arguments that may be assigned to it. Also include
    any error reasons when there are too many arguments, too few arguments, or badly typed starred
    arguments.

    Parameters such as `*args: int` and `**kwargs: str` may have any number of arguments assigned to
    them.

    Other parameters such as named parameters (`x: int`), positional-only, or keyword-only
    parameters will have zero or one argument mapped to them.

    If a starred argument, such as `*xs`, is being distributed across multiple parameters, each
    parameter will receive `*xs` with its index into the starred tuple. That way, later stages of
    the signature selection pipeline can find the precise type of the tuple element that will be
    assigned to each parameter. *)
let get_parameter_argument_mapping
    ~all_parameters
    ~parameters
    ~self_argument
    ~order
    ~location
    ~resolve
    ~get_typed_dictionary
    arguments
  =
  let open Type.Callable in
  let all_arguments = arguments in
  let all_parameters_list = parameters in
  let rec consume
      ?index_into_starred_tuple
      ~arguments
      ~parameters
      ({
         ParameterArgumentMapping.parameter_argument_mapping;
         reasons = { arity; annotation } as reasons;
       } as parameter_argument_mapping_with_reasons)
    =
    let consume_with_new_index ?index_into_starred_tuple = consume ?index_into_starred_tuple in
    let consume = consume ?index_into_starred_tuple in
    let update_mapping parameter argument =
      Map.add_multi parameter_argument_mapping ~key:parameter ~data:argument
    in
    let arity_mismatch ?(unreachable_parameters = []) ~arguments reasons =
      match all_parameters with
      | Defined all_parameters ->
          let matched_keyword_arguments =
            let is_keyword_argument = function
              | { Argument.WithPosition.kind = Named _; _ } -> true
              | _ -> false
            in
            let matched_arguments =
              List.take all_arguments (List.length all_arguments - List.length arguments)
            in
            List.filter ~f:is_keyword_argument matched_arguments
          in
          let keywords_parameter_adjustment =
            let has_keywords_parameter =
              List.exists
                ~f:(fun parameter ->
                  match parameter with
                  | CallableParamType.Keywords _ -> true
                  | _ -> false)
                all_parameters
            in
            if has_keywords_parameter then 1 else 0
          in
          let positional_parameter_count =
            List.length all_parameters
            - List.length unreachable_parameters
            - List.length matched_keyword_arguments
            - keywords_parameter_adjustment
          in
          let self_argument_adjustment =
            if Option.is_some self_argument then
              1
            else
              0
          in
          let error =
            SignatureSelectionTypes.TooManyArguments
              {
                expected = positional_parameter_count - self_argument_adjustment;
                provided =
                  positional_parameter_count + List.length arguments - self_argument_adjustment;
              }
          in
          { reasons with arity = error :: arity }
      | _ -> reasons
    in
    let extract_matching_parameter_name argument_name parameters =
      let rec search_parameters searched to_search =
        match to_search with
        | [] -> None, List.rev searched
        | (CallableParamType.KeywordOnly { name = parameter_name; _ } as head) :: tail
        | (CallableParamType.Named { name = parameter_name; _ } as head) :: tail
          when Identifier.equal_sanitized parameter_name argument_name ->
            Some head, List.rev searched @ tail
        | (CallableParamType.Keywords _ as head) :: tail ->
            let matching, parameters = search_parameters (head :: searched) tail in
            let matching = Some (Option.value matching ~default:head) in
            matching, parameters
        | head :: tail -> search_parameters (head :: searched) tail
      in
      search_parameters [] parameters
    in
    (* Check whether left_type is assignable to right_type. *)
    let is_less_or_equal left_type right_type =
      let constraints =
        if Type.is_unbound left_type then
          ConstraintsSet.impossible
        else
          TypeOrder.OrderedConstraintsSet.add_and_simplify
            ConstraintsSet.empty
            ~new_constraint:(LessOrEqual { left = left_type; right = right_type })
            ~order
      in
      Option.is_some (TypeOrder.OrderedConstraintsSet.solve constraints ~order)
    in
    (* Get a copy of parameter_argument_mapping_with_reasons with an annotation error added. *)
    let add_annotation_error expression resolved error_factory =
      let argument_location = get_location ~expression ~default:location in
      let invalid_argument : SignatureSelectionTypes.invalid_argument =
        { expression; annotation = resolved }
      in
      let error = error_factory (Node.create ~location:argument_location invalid_argument) in
      {
        parameter_argument_mapping_with_reasons with
        reasons = { reasons with annotation = error :: annotation };
      }
    in
    match arguments, parameters with
    | [], [] ->
        (* Both empty *)
        parameter_argument_mapping_with_reasons
    | ( ({ Argument.WithPosition.kind = SingleStar; resolved; expression; _ } as argument)
        :: arguments_tail,
        _ ) -> (
        (* Starred argument. Check if its type is assignable to Iterable[Any] *)
        let iterable_type = Type.iterable Type.Any in
        let parameter_argument_mapping_with_reasons =
          if is_less_or_equal resolved iterable_type then
            parameter_argument_mapping_with_reasons
          else
            let error_factory invalid_argument =
              SignatureSelectionTypes.InvalidVariableArgument invalid_argument
            in
            add_annotation_error expression resolved error_factory
        in
        (* If we can determine from the expression or type that the starred argument expands into a
           fixed number of arguments, do the expansion now. *)
        let maybe_fixed_items =
          match expression, resolved with
          | Some { Node.value = List items | Tuple items; _ }, _ ->
              Some (List.map ~f:(fun item -> Some item, resolve item) items)
          | _, Type.Tuple (Concrete item_types) ->
              Some (List.map ~f:(fun item_type -> None, item_type) item_types)
          | _ -> None
        in
        match maybe_fixed_items with
        | Some fixed_items ->
            let expanded_arguments =
              List.mapi
                ~f:(fun i (expression, resolved) ->
                  {
                    Argument.WithPosition.kind = Positional;
                    expression;
                    position = argument.position + i;
                    resolved;
                  })
                fixed_items
            in
            let position_delta = List.length expanded_arguments - 1 in
            let updated_arguments_tail =
              List.map
                ~f:(fun argument -> { argument with position = argument.position + position_delta })
                arguments_tail
            in
            consume
              ~arguments:(expanded_arguments @ updated_arguments_tail)
              ~parameters
              parameter_argument_mapping_with_reasons
        | None -> (
            (* Starred argument that cannot be expanded to a fixed number of arguments. Match it
               against the first parameter (if any). *)
            match parameters with
            | [] ->
                (* Starred argument; parameters empty *)
                consume
                  ~arguments:arguments_tail
                  ~parameters
                  parameter_argument_mapping_with_reasons
            | (CallableParamType.Variable _ as parameter) :: _ ->
                (* Starred argument; starred parameter *)
                let parameter_argument_mapping =
                  update_mapping
                    parameter
                    (make_matched_argument ?index_into_starred_tuple argument)
                in
                (* We don't need to slice any further `*xs` arguments since they are consumed fully
                   by the expected `Variable` parameter. *)
                consume_with_new_index
                  ?index_into_starred_tuple:None
                  ~arguments:arguments_tail
                  ~parameters
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
            | CallableParamType.Keywords _ :: parameters_tail ->
                (* Starred argument, double starred parameter *)
                consume
                  ~arguments
                  ~parameters:parameters_tail
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
            | Type.Callable.CallableParamType.KeywordOnly _ :: _ ->
                (* Starred argument, keyword only parameter *)
                consume
                  ~arguments:arguments_tail
                  ~parameters
                  parameter_argument_mapping_with_reasons
            | parameter :: parameters_tail ->
                (* Starred argument, parameter *)
                let index_into_starred_tuple = Option.value index_into_starred_tuple ~default:0 in
                let parameter_argument_mapping =
                  update_mapping
                    parameter
                    (make_matched_argument ~index_into_starred_tuple argument)
                in
                consume_with_new_index
                  ~index_into_starred_tuple:(index_into_starred_tuple + 1)
                  ~arguments
                  ~parameters:parameters_tail
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }))
    | ({ kind = DoubleStar; resolved; expression; _ } as argument) :: arguments_tail, parameters
      -> (
        (* Double starred argument. Check if its type is assignable to Mapping[str, Any] *)
        let mapping_type = Type.parametric "typing.Mapping" [Single Type.string; Single Type.Any] in
        let parameter_argument_mapping_with_reasons =
          if is_less_or_equal resolved mapping_type then
            parameter_argument_mapping_with_reasons
          else
            let error_factory invalid_argument =
              SignatureSelectionTypes.InvalidKeywordArgument invalid_argument
            in
            add_annotation_error expression resolved error_factory
        in
        (* Expand the double starred argument if it is a constant, or if a typed dictionary is being
           matched against a non-kwargs parameter *)
        let maybe_expanded_arguments =
          let should_expand_typed_dictionary =
            match parameters with
            | CallableParamType.Keywords _ :: _ -> false
            | _ -> true
          in
          match expression with
          | Some { Node.value = Dictionary items; _ } ->
              let filtered_arguments =
                let accumulate_argument item so_far =
                  let open Expression in
                  match item with
                  | Dictionary.Entry.KeyValue
                      {
                        key =
                          {
                            Node.value =
                              Constant (Constant.String { StringLiteral.value = name; _ });
                            _;
                          } as key;
                        value;
                      } ->
                      let argument =
                        {
                          Argument.WithPosition.kind =
                            Named
                              {
                                name =
                                  {
                                    Node.value = "$parameter$" ^ name;
                                    location = Node.location key;
                                  };
                                requires_default = false;
                              };
                          expression = Some value;
                          position = argument.position;
                          resolved = resolve value;
                        }
                      in
                      argument :: so_far
                  | _ -> so_far
                in
                List.fold_right ~init:[] ~f:accumulate_argument items
              in
              if List.length filtered_arguments = List.length items then
                Some filtered_arguments
              else
                None
          | Some expression when should_expand_typed_dictionary -> (
              (* Expand typed dictionaries *)
              match get_typed_dictionary resolved with
              | Some { Type.TypedDictionary.fields; _ } ->
                  let accumulate_argument
                      { Type.TypedDictionary.name; annotation; required; _ }
                      so_far
                    =
                    {
                      Argument.WithPosition.kind =
                        Named
                          {
                            name =
                              {
                                Node.value = "$parameter$" ^ name;
                                location = Node.location expression;
                              };
                            requires_default = not required;
                          };
                      expression = None;
                      position = argument.position;
                      resolved = annotation;
                    }
                    :: so_far
                  in
                  Some (List.fold_right ~init:[] ~f:accumulate_argument fields)
              | _ -> None)
          | _ -> None
        in
        match maybe_expanded_arguments with
        | Some expanded_arguments ->
            consume
              ~arguments:(expanded_arguments @ arguments_tail)
              ~parameters
              parameter_argument_mapping_with_reasons
        | None -> (
            (* Non-constant double starred argument. Match it against the first parameter (if
               any). *)
            match parameters with
            | [] ->
                (* Double starred argument; parameters empty *)
                consume
                  ~arguments:arguments_tail
                  ~parameters
                  parameter_argument_mapping_with_reasons
            | (CallableParamType.Keywords _ as parameter) :: _ ->
                (* Double starred argument; double starred parameter *)
                let parameter_argument_mapping =
                  update_mapping parameter (make_matched_argument argument)
                in
                consume
                  ~arguments:arguments_tail
                  ~parameters
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
            | CallableParamType.Variable _ :: parameters_tail ->
                (* Double starred argument, starred parameter *)
                consume
                  ~arguments
                  ~parameters:parameters_tail
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
            | parameter :: parameters_tail ->
                (* Double starred argument, parameter *)
                let index_into_starred_tuple = Option.value index_into_starred_tuple ~default:0 in
                let parameter_argument_mapping =
                  update_mapping
                    parameter
                    (make_matched_argument ~index_into_starred_tuple argument)
                in
                consume_with_new_index
                  ~index_into_starred_tuple:(index_into_starred_tuple + 1)
                  ~arguments
                  ~parameters:parameters_tail
                  { parameter_argument_mapping_with_reasons with parameter_argument_mapping }))
    | ({ kind = Named { name; _ }; _ } as argument) :: _, [] -> (
        (* Named argument; parameters empty *)
        let matching_parameter, _ =
          extract_matching_parameter_name name.value all_parameters_list
        in
        match matching_parameter with
        | Some matching_parameter -> (
            let named_parameter_already_matched = function
              | MatchedArgument { index_into_starred_tuple = None; _ } -> true
              | _ -> false
            in
            match Map.find parameter_argument_mapping matching_parameter with
            | Some matched_arguments
              when List.exists matched_arguments ~f:named_parameter_already_matched ->
                (* Another named or positional argument has already matched the parameter *)
                {
                  parameter_argument_mapping_with_reasons with
                  reasons = { reasons with arity = UnexpectedKeyword name.value :: arity };
                }
            | _ ->
                (* Possible matched arguments are all unpacked tuples or dicts *)
                {
                  parameter_argument_mapping =
                    update_mapping matching_parameter (make_matched_argument argument);
                  reasons;
                })
        | None ->
            (* No parameter has that name *)
            {
              parameter_argument_mapping_with_reasons with
              reasons = { reasons with arity = UnexpectedKeyword name.value :: arity };
            })
    | _, [] ->
        (* Positional argument; parameters empty *)
        { parameter_argument_mapping_with_reasons with reasons = arity_mismatch ~arguments reasons }
    | [], (CallableParamType.KeywordOnly { default = true; _ } as parameter) :: parameters_tail
    | [], (CallableParamType.PositionalOnly { default = true; _ } as parameter) :: parameters_tail
    | [], (CallableParamType.Named { default = true; _ } as parameter) :: parameters_tail ->
        (* Arguments empty, default parameter *)
        let parameter_argument_mapping = update_mapping parameter Default in
        consume
          ~arguments
          ~parameters:parameters_tail
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
    | [], parameter :: parameters_tail ->
        (* Arguments empty, parameter *)
        let parameter_argument_mapping =
          match Map.find parameter_argument_mapping parameter with
          | Some _ -> parameter_argument_mapping
          | None -> Map.set ~key:parameter ~data:[] parameter_argument_mapping
        in
        consume
          ~arguments
          ~parameters:parameters_tail
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
    | ( ({ kind = Named _; _ } as argument) :: arguments_tail,
        (CallableParamType.Keywords _ as parameter) :: _ ) ->
        (* Labeled argument, keywords parameter *)
        let parameter_argument_mapping =
          update_mapping parameter (make_matched_argument argument)
        in
        consume
          ~arguments:arguments_tail
          ~parameters
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
    | ({ kind = Named { name; _ }; _ } as argument) :: arguments_tail, parameters ->
        (* Labeled argument *)
        let matching_parameter, remaining_parameters =
          extract_matching_parameter_name name.value parameters
        in
        let parameter_argument_mapping, reasons =
          match matching_parameter with
          | Some matching_parameter ->
              update_mapping matching_parameter (make_matched_argument argument), reasons
          | None ->
              ( parameter_argument_mapping,
                { reasons with arity = UnexpectedKeyword name.value :: arity } )
        in
        consume
          ~arguments:arguments_tail
          ~parameters:remaining_parameters
          { parameter_argument_mapping; reasons }
    | { kind = Positional; _ } :: _, CallableParamType.Keywords _ :: parameters_tail ->
        (* Unlabeled argument, double starred parameter *)
        consume
          ~arguments
          ~parameters:parameters_tail
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
    | ( ({ kind = Positional; _ } as argument) :: arguments_tail,
        (CallableParamType.Variable _ as parameter) :: _ ) ->
        (* Unlabeled argument, starred parameter *)
        let parameter_argument_mapping_with_reasons =
          let parameter_argument_mapping =
            update_mapping parameter (make_matched_argument argument)
          in
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
        in
        consume ~arguments:arguments_tail ~parameters parameter_argument_mapping_with_reasons
    | ( { kind = Positional; _ } :: _,
        (CallableParamType.KeywordOnly _ as parameter) :: parameters_tail ) ->
        (* Unlabeled argument, keyword only parameter *)
        let reasons =
          arity_mismatch reasons ~unreachable_parameters:(parameter :: parameters_tail) ~arguments
        in
        { parameter_argument_mapping_with_reasons with reasons }
    | ({ kind = Positional; _ } as argument) :: arguments_tail, parameter :: parameters_tail ->
        (* Unlabeled argument, parameter *)
        let parameter_argument_mapping =
          update_mapping parameter (make_matched_argument argument)
        in
        consume
          ~arguments:arguments_tail
          ~parameters:parameters_tail
          { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
  in
  {
    ParameterArgumentMapping.parameter_argument_mapping = CallableParamType.Map.empty;
    reasons = empty_reasons;
  }
  |> consume ?index_into_starred_tuple:None ~arguments ~parameters


(** Check all arguments against the respective parameter types. Return a signature match containing
    constraints from the above compatibility checks and any mismatch errors. *)
let check_arguments_against_parameters
    ~order
    ~resolve_mutable_literals
    ~resolve_with_locals
    ~get_typed_dictionary
    ~location
    ~callable
    { ParameterArgumentMapping.parameter_argument_mapping; reasons }
  =
  let open SignatureSelectionTypes in
  let open Type.Callable in
  (* Check whether the parameter annotation is `Callable[[ParamVar], ReturnVar]` * and the argument
     is `lambda parameter: body` *)
  let is_generic_lambda parameter arguments =
    match parameter, arguments with
    | ( CallableParamType.PositionalOnly
          {
            annotation =
              Type.Callable
                {
                  kind = Anonymous;
                  implementation =
                    {
                      annotation = Type.Variable return_variable;
                      parameters =
                        Defined
                          [
                            CallableParamType.PositionalOnly
                              {
                                index = 0;
                                annotation = Type.Variable parameter_variable;
                                default = false;
                              };
                          ];
                    };
                  overloads = [];
                } as annotation;
            _;
          },
        [
          MatchedArgument
            {
              argument =
                {
                  expression =
                    Some
                      {
                        value =
                          Lambda
                            {
                              body = lambda_body;
                              parameters =
                                [
                                  {
                                    value =
                                      { name = lambda_parameter; value = None; annotation = None };
                                    _;
                                  };
                                ];
                            };
                        _;
                      };
                  _;
                };
              _;
            };
        ] )
      when Type.Variable.TypeVar.is_free parameter_variable
           && Type.Variable.TypeVar.is_free return_variable ->
        Some (annotation, parameter_variable, return_variable, lambda_parameter, lambda_body)
    | _ -> None
  in
  let check_arguments_and_update_signature_match
      ~parameter
      ~arguments
      ({ reasons = { arity; _ } as reasons; _ } as signature_match)
    =
    let check_argument_and_set_constraints_and_reasons
        ~position
        ~argument_location
        ~name
        ~argument_annotation
        ~parameter_annotation
        ({ constraints_set; reasons = { annotation; _ } as reasons; _ } as signature_match)
      =
      let reasons_with_mismatch =
        let mismatch =
          let location = name >>| Node.location |> Option.value ~default:argument_location in
          {
            actual = argument_annotation;
            expected = parameter_annotation;
            name = Option.map name ~f:Node.value;
            position;
          }
          |> Node.create ~location
          |> fun mismatch -> Mismatches [Mismatch mismatch]
        in
        { reasons with annotation = mismatch :: annotation }
      in
      let updated_constraints_set =
        TypeOrder.OrderedConstraintsSet.add_and_simplify
          constraints_set
          ~new_constraint:(LessOrEqual { left = argument_annotation; right = parameter_annotation })
          ~order
      in
      if ConstraintsSet.potentially_satisfiable updated_constraints_set then
        { signature_match with constraints_set = updated_constraints_set }
      else
        { signature_match with constraints_set; reasons = reasons_with_mismatch }
    in
    let extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved =
      let iterable_constraints =
        if Type.is_unbound resolved then
          ConstraintsSet.impossible
        else
          TypeOrder.OrderedConstraintsSet.add_and_simplify
            ConstraintsSet.empty
            ~new_constraint:(LessOrEqual { left = resolved; right = generic_iterable_type })
            ~order
      in
      TypeOrder.OrderedConstraintsSet.solve iterable_constraints ~order
      >>| fun solution ->
      TypeConstraints.Solution.instantiate_single_type_var solution synthetic_variable
      |> Option.value ~default:Type.Any
    in
    let bind_arguments_to_variadic ~expected ~arguments =
      let extract_ordered_types arguments =
        let extracted, errors =
          let extract
              ( ({ Argument.WithPosition.kind; resolved; expression; _ } as argument),
                index_into_starred_tuple )
            =
            match kind with
            | SingleStar -> (
                match resolved, index_into_starred_tuple with
                | Type.Tuple ordered_type, Some index_into_starred_tuple ->
                    Type.OrderedTypes.drop_prefix ~length:index_into_starred_tuple ordered_type
                    >>| (fun ordered_type ->
                          Either.First
                            {
                              ordered_type;
                              argument;
                              item_type_for_error = Type.OrderedTypes.union_upper_bound ordered_type;
                            })
                    |> Option.value ~default:(Either.Second { expression; annotation = resolved })
                | Type.Tuple ordered_type, None ->
                    Either.First
                      {
                        ordered_type;
                        argument;
                        item_type_for_error = Type.OrderedTypes.union_upper_bound ordered_type;
                      }
                | _, _ -> (
                    let synthetic_variable = Type.Variable.TypeVar.create "$_T" in
                    let generic_iterable_type = Type.iterable (Type.Variable synthetic_variable) in
                    match
                      extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved
                    with
                    | Some item_type ->
                        Either.First
                          {
                            ordered_type =
                              Type.OrderedTypes.create_unbounded_concatenation item_type;
                            argument;
                            item_type_for_error = item_type;
                          }
                    | _ -> Either.Second { expression; annotation = resolved }))
            | _ ->
                Either.First
                  {
                    ordered_type = Type.OrderedTypes.Concrete [resolved];
                    argument;
                    item_type_for_error = resolved;
                  }
          in
          List.rev arguments |> List.partition_map ~f:extract
        in
        match errors with
        | [] -> Ok extracted
        | not_bounded_tuple :: _ ->
            Error
              (Mismatches
                 [
                   MismatchWithUnpackableType
                     { variable = expected; mismatch = NotUnpackableType not_bounded_tuple };
                 ])
      in
      let concatenate extracted =
        let ordered_types = List.map extracted ~f:(fun { ordered_type; _ } -> ordered_type) in
        match Type.OrderedTypes.coalesce_ordered_types ordered_types with
        | Some concatenated -> Ok (concatenated, extracted)
        | None ->
            Error
              (Mismatches
                 [
                   MismatchWithUnpackableType
                     { variable = expected; mismatch = CannotConcatenate ordered_types };
                 ])
      in
      let solve (concatenated, extracted_ordered_types) =
        let updated_constraints_set =
          TypeOrder.OrderedConstraintsSet.add_and_simplify
            signature_match.constraints_set
            ~new_constraint:(OrderedTypesLessOrEqual { left = concatenated; right = expected })
            ~order
        in
        if ConstraintsSet.potentially_satisfiable updated_constraints_set then
          Ok updated_constraints_set
        else
          let expected_concatenation_type =
            match expected with
            | Concatenation concatenation -> Some concatenation
            | _ -> None
          in
          match
            expected_concatenation_type
            >>= Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
          with
          | Some expected_item_type ->
              (* The expected type is `*args: *Tuple[X, ...]`. Raise an individual error for each
                 argument that was passed. *)
              let make_mismatch
                  {
                    argument = { Argument.WithPosition.position; expression; kind; _ };
                    item_type_for_error;
                    _;
                  }
                =
                let name =
                  match kind with
                  | Named { name = name_node; _ } -> Some name_node
                  | _ -> None
                in
                let location =
                  Option.first_some (name >>| Node.location) (expression >>| Node.location)
                  |> Option.value ~default:location
                in
                let is_mismatch =
                  TypeOrder.OrderedConstraintsSet.add_and_simplify
                    signature_match.constraints_set
                    ~new_constraint:
                      (LessOrEqual { left = item_type_for_error; right = expected_item_type })
                    ~order
                  |> ConstraintsSet.potentially_satisfiable
                  |> not
                in
                {
                  actual = item_type_for_error;
                  expected = expected_item_type;
                  name = name >>| Node.value;
                  position;
                }
                |> Node.create ~location
                |> fun mismatch -> Mismatch mismatch |> Option.some_if is_mismatch
              in
              Error (Mismatches (List.filter_map extracted_ordered_types ~f:make_mismatch))
          | None ->
              (* The expected type is different from `*args: *Tuple[X, ...]`, such as `*Ts` or more
                 complicated unbounded tuples. It may require a prefix or suffix of arguments. Since
                 we cannot express that clearly by raising individual errors, we raise a combined
                 error about the arguments. *)
              Error
                (Mismatches
                   [
                     MismatchWithUnpackableType
                       { variable = expected; mismatch = ConstraintFailure concatenated };
                   ])
      in
      let make_signature_match = function
        | Ok constraints_set -> { signature_match with constraints_set }
        | Error error -> { signature_match with reasons = { reasons with arity = error :: arity } }
      in
      let arguments =
        List.map arguments ~f:(function
            | MatchedArgument { argument; index_into_starred_tuple } ->
                argument, index_into_starred_tuple
            | Default -> failwith "Variable parameters do not have defaults")
      in
      let open Result in
      extract_ordered_types arguments >>= concatenate >>= solve |> make_signature_match
    in
    match parameter, arguments with
    | CallableParamType.Variable (Concatenation concatenation), arguments ->
        bind_arguments_to_variadic
          ~expected:(Type.OrderedTypes.Concatenation concatenation)
          ~arguments
    | CallableParamType.Variable (Concrete parameter_annotation), arguments ->
        bind_arguments_to_variadic
          ~expected:(Type.OrderedTypes.create_unbounded_concatenation parameter_annotation)
          ~arguments
    | CallableParamType.Keywords parameter_annotation, [] when Type.is_unpack parameter_annotation
      -> (
        (* Check that unmatched typed dictionary kwargs has no required fields *)
        match Type.unpack_value parameter_annotation with
        | None -> signature_match
        | Some parameter_annotation -> (
            (* We will emit an error in typechecking if the unpacked kwargs is not a typed
               dictionary *)
            match get_typed_dictionary parameter_annotation with
            | None -> signature_match
            | Some { Type.TypedDictionary.fields; _ } -> (
                let missing_typed_dictionary_arguments =
                  List.filter_map fields ~f:(fun { Type.TypedDictionary.name; required; _ } ->
                      if required then
                        Some (MissingArgument (Named name))
                      else
                        None)
                in
                match missing_typed_dictionary_arguments with
                | [] -> signature_match
                | _ ->
                    {
                      signature_match with
                      reasons = { reasons with arity = missing_typed_dictionary_arguments @ arity };
                    })))
    | CallableParamType.Keywords _, [] ->
        (* Parameter was not matched, but empty is acceptable for variable arguments and keyword
           arguments. *)
        signature_match
    | CallableParamType.KeywordOnly { name; _ }, []
    | CallableParamType.Named { name; _ }, [] ->
        (* Parameter was not matched *)
        let reasons = { reasons with arity = MissingArgument (Named name) :: arity } in
        { signature_match with reasons }
    | CallableParamType.PositionalOnly { index; _ }, [] ->
        (* Parameter was not matched *)
        let reasons = { reasons with arity = MissingArgument (PositionalOnly index) :: arity } in
        { signature_match with reasons }
    | Keywords parameter_annotation, arguments when Type.is_unpack parameter_annotation -> (
        match Type.unpack_value parameter_annotation with
        | None -> signature_match
        | Some parameter_annotation -> (
            (* We will emit an error in typechecking if the unpacked kwargs is not a typed
               dictionary *)
            match get_typed_dictionary parameter_annotation with
            | None -> signature_match
            | Some { Type.TypedDictionary.fields; _ } -> (
                let rec check ~arguments ~unmatched_fields signature_match =
                  match arguments with
                  | [] -> unmatched_fields, signature_match
                  | Default :: tail -> check signature_match ~unmatched_fields ~arguments:tail
                  | MatchedArgument { argument = { expression; position; kind; resolved }; _ }
                    :: tail -> (
                      let argument_location = get_location ~expression ~default:location in
                      let name =
                        match kind with
                        | Named { name = name_node; _ } -> Some name_node
                        | _ -> None
                      in
                      let check_argument
                          ~signature_match
                          ~position
                          ~parameter_annotation
                          argument_annotation
                        =
                        check_argument_and_set_constraints_and_reasons
                          ~position
                          ~argument_location
                          ~argument_annotation
                          ~parameter_annotation
                          ~name
                          signature_match
                      in
                      let add_annotation_error
                          ({ reasons = { annotation; _ }; _ } as signature_match)
                          error
                        =
                        {
                          signature_match with
                          reasons = { reasons with annotation = error :: annotation };
                        }
                      in
                      match kind with
                      | DoubleStar -> (
                          let argument_annotation, weakening_error =
                            if Type.Variable.all_variables_are_resolved parameter_annotation then
                              let { WeakenMutableLiterals.resolved; typed_dictionary_errors } =
                                resolve_mutable_literals
                                  ~resolve:(resolve_with_locals ~locals:[])
                                  ~expression
                                  ~resolved
                                  ~expected:parameter_annotation
                              in
                              let weakening_error =
                                if List.is_empty typed_dictionary_errors then
                                  None
                                else
                                  Some (TypedDictionaryInitializationError typed_dictionary_errors)
                              in
                              resolved, weakening_error
                            else
                              resolved, None
                          in
                          (* Check that unpacked typed dictionary does not collide with already
                             bound kwargs fields *)
                          let signature_match =
                            match get_typed_dictionary argument_annotation with
                            | None -> signature_match
                            | Some { Type.TypedDictionary.fields; _ } -> (
                                let extra_field_errors =
                                  List.filter_map fields ~f:(fun { Type.TypedDictionary.name; _ } ->
                                      if Map.find unmatched_fields name |> Option.is_some then
                                        None
                                      else
                                        Some (UnexpectedKeyword name))
                                in
                                match extra_field_errors with
                                | [] -> signature_match
                                | _ ->
                                    {
                                      signature_match with
                                      reasons = { reasons with arity = extra_field_errors @ arity };
                                    })
                          in
                          let unmatched_fields = Identifier.Map.empty in
                          match weakening_error with
                          | Some weakening_error ->
                              unmatched_fields, add_annotation_error signature_match weakening_error
                          | None ->
                              argument_annotation
                              |> check_argument ~signature_match ~position ~parameter_annotation
                              |> check ~unmatched_fields ~arguments:tail)
                      | Named { name = { Node.value = argument_name; _ }; requires_default } -> (
                          let argument_name = Identifier.sanitized argument_name in
                          match Map.find unmatched_fields argument_name with
                          | Some { Type.TypedDictionary.annotation; required; _ }
                            when requires_default && required ->
                              check_argument
                                ~signature_match
                                ~position
                                ~parameter_annotation:annotation
                                resolved
                              |> check ~unmatched_fields ~arguments:tail
                          | Some { Type.TypedDictionary.annotation; _ } ->
                              check_argument
                                ~signature_match
                                ~position
                                ~parameter_annotation:annotation
                                resolved
                              |> check
                                   ~unmatched_fields:(Map.remove unmatched_fields argument_name)
                                   ~arguments:tail
                          | None ->
                              check
                                ~unmatched_fields
                                ~arguments:tail
                                {
                                  signature_match with
                                  reasons =
                                    {
                                      reasons with
                                      arity = UnexpectedKeyword argument_name :: arity;
                                    };
                                })
                      | SingleStar
                      | Positional ->
                          (* These should not be matched with kwargs in the first place *)
                          check signature_match ~unmatched_fields ~arguments:tail)
                in
                let unmatched_fields, signature_match =
                  let fields_map =
                    List.map fields ~f:(fun ({ Type.TypedDictionary.name; _ } as field) ->
                        Identifier.sanitized name, field)
                    |> Identifier.Map.of_alist_reduce ~f:(fun first _ -> first)
                  in
                  check ~unmatched_fields:fields_map ~arguments:(List.rev arguments) signature_match
                in
                let missing_typed_dictionary_arguments =
                  Map.data unmatched_fields
                  |> List.filter_map ~f:(fun { Type.TypedDictionary.name; required; _ } ->
                         if required then
                           Some (MissingArgument (Named name))
                         else
                           None)
                in
                match missing_typed_dictionary_arguments with
                | [] -> signature_match
                | _ ->
                    {
                      signature_match with
                      reasons = { reasons with arity = missing_typed_dictionary_arguments @ arity };
                    })))
    | PositionalOnly { annotation = parameter_annotation; _ }, arguments
    | KeywordOnly { annotation = parameter_annotation; _ }, arguments
    | Named { annotation = parameter_annotation; _ }, arguments
    | Keywords parameter_annotation, arguments -> (
        let has_default =
          match parameter with
          | Named { default; _ } -> default
          | _ -> false
        in
        let rec check ~arguments signature_match =
          match arguments with
          | [] -> signature_match
          | Default :: tail ->
              (* Parameter default value was used. Assume it is correct. *)
              check signature_match ~arguments:tail
          | MatchedArgument
              { argument = { expression; position; kind; resolved }; index_into_starred_tuple }
            :: tail -> (
              let argument_location = get_location ~expression ~default:location in
              let name =
                match kind with
                | Named { name = name_node; _ } -> Some name_node
                | _ -> None
              in
              let check_argument ~position argument_annotation =
                check_argument_and_set_constraints_and_reasons
                  ~position
                  ~argument_location
                  ~argument_annotation
                  ~parameter_annotation
                  ~name
                  signature_match
              in
              let add_annotation_error ({ reasons = { annotation; _ }; _ } as signature_match) error
                =
                { signature_match with reasons = { reasons with annotation = error :: annotation } }
              in
              let update_signature_match_for_iterable ~position iterable_item_type =
                let argument_location = get_location ~expression ~default:location in
                let iterable_item_type = Option.value iterable_item_type ~default:Type.Any in
                check_argument_and_set_constraints_and_reasons
                  ~position
                  ~argument_location
                  ~argument_annotation:iterable_item_type
                  ~parameter_annotation
                  ~name
                  signature_match
                |> check ~arguments:tail
              in
              match kind with
              | DoubleStar ->
                  let synthetic_variable = Type.Variable.TypeVar.create "$_T" in
                  let generic_iterable_type =
                    Type.parametric
                      "typing.Mapping"
                      [Single Type.string; Single (Type.Variable synthetic_variable)]
                  in
                  extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved
                  |> update_signature_match_for_iterable ~position
              | SingleStar -> (
                  let signature_match_for_single_element =
                    match parameter, index_into_starred_tuple, resolved with
                    | ( (PositionalOnly _ | Named _),
                        Some index_into_starred_tuple,
                        Type.Tuple ordered_type ) -> (
                        match
                          Type.OrderedTypes.index
                            ~python_index:index_into_starred_tuple
                            ordered_type
                        with
                        | Some type_ ->
                            check_argument ~position:(position + index_into_starred_tuple) type_
                            |> check ~arguments:tail
                            |> Option.some
                        | None ->
                            (* We could not index into the tuple type to find the element for the
                               current parameter. This will be handled later in the function, so
                               return None. *)
                            None)
                    | _ -> None
                  in
                  match signature_match_for_single_element with
                  | Some signature_match_for_single_element -> signature_match_for_single_element
                  | None ->
                      let synthetic_variable = Type.Variable.TypeVar.create "$_T" in
                      let generic_iterable_type =
                        Type.iterable (Type.Variable synthetic_variable)
                      in
                      extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved
                      |> update_signature_match_for_iterable
                           ~position:(position + Option.value ~default:0 index_into_starred_tuple))
              | Named _
              | Positional -> (
                  match kind with
                  | Named { requires_default = true; name = { Node.value = name; _ } }
                    when not has_default ->
                      let reasons =
                        {
                          reasons with
                          arity = MissingArgument (NotRequiredTypedDict name) :: arity;
                        }
                      in
                      { signature_match with reasons }
                  | _ -> (
                      let argument_annotation, weakening_error =
                        if Type.Variable.all_variables_are_resolved parameter_annotation then
                          let { WeakenMutableLiterals.resolved; typed_dictionary_errors } =
                            resolve_mutable_literals
                              ~resolve:(resolve_with_locals ~locals:[])
                              ~expression
                              ~resolved
                              ~expected:parameter_annotation
                          in
                          let weakening_error =
                            if List.is_empty typed_dictionary_errors then
                              None
                            else
                              Some (TypedDictionaryInitializationError typed_dictionary_errors)
                          in
                          resolved, weakening_error
                        else
                          resolved, None
                      in
                      match weakening_error with
                      | Some weakening_error -> add_annotation_error signature_match weakening_error
                      | None ->
                          argument_annotation |> check_argument ~position |> check ~arguments:tail))
              )
        in
        match is_generic_lambda parameter arguments with
        | Some _ -> signature_match (* Handle this later in `special_case_lambda_parameter` *)
        | None -> check ~arguments:(List.rev arguments) signature_match)
  in
  let check_if_solution_exists
      ({ constraints_set; reasons = { annotation; _ } as reasons; callable; _ } as signature_match)
    =
    let solution =
      TypeOrder.OrderedConstraintsSet.solve
        constraints_set
        ~order
        ~only_solve_for:(Type.Variable.all_free_variables (Type.Callable callable))
    in
    if Option.is_some solution then
      signature_match
    else
      (* All other cases should have been able to been blamed on a specific argument, this is the
         only global failure. *)
      {
        signature_match with
        reasons = { reasons with annotation = MutuallyRecursiveTypeVariables :: annotation };
      }
  in
  let special_case_dictionary_constructor
      ({ parameter_argument_mapping; callable; constraints_set; _ } as signature_match)
    =
    let open Type.Record.Callable in
    let has_matched_keyword_parameter parameters =
      List.find parameters ~f:(function
          | CallableParamType.Keywords _ -> true
          | _ -> false)
      >>= Map.find parameter_argument_mapping
      >>| List.is_empty
      >>| not
      |> Option.value ~default:false
    in
    match callable with
    | {
     kind = Named name;
     implementation =
       {
         parameters = Defined parameters;
         annotation = Type.Parametric { arguments = [Single key_type; _]; _ };
         _;
       };
     _;
    }
      when String.equal (Reference.show name) "dict.__init__"
           && has_matched_keyword_parameter parameters ->
        let updated_constraints =
          TypeOrder.OrderedConstraintsSet.add_and_simplify
            constraints_set
            ~new_constraint:(LessOrEqual { left = Type.string; right = key_type })
            ~order
        in
        if ConstraintsSet.potentially_satisfiable updated_constraints then
          { signature_match with constraints_set = updated_constraints }
        else (* TODO(T41074174): Error here *)
          signature_match
    | _ -> signature_match
  in
  let special_case_lambda_parameter ({ parameter_argument_mapping; _ } as signature_match) =
    (* Special case: `Callable[[ParamVar], ReturnVar]` with `lambda parameter: body` *)
    let check_lambda_argument_and_update_signature_match
        ~parameter
        ~arguments
        ({ constraints_set; _ } as signature_match)
      =
      match is_generic_lambda parameter arguments with
      | None -> signature_match
      | Some (annotation, parameter_variable, _, lambda_parameter, lambda_body) -> (
          (* Infer the parameter type using existing constraints. *)
          let solution =
            TypeOrder.OrderedConstraintsSet.solve
              constraints_set
              ~order
              ~only_solve_for:[Type.Record.Variable.TypeVarVariable parameter_variable]
            >>= fun solution ->
            TypeConstraints.Solution.instantiate_single_type_var solution parameter_variable
          in
          match solution with
          | None -> signature_match
          | Some parameter_type ->
              (* Infer the return type by resolving the lambda body with the parameter type *)
              let updated_constraints =
                let resolved =
                  let return_type =
                    resolve_with_locals
                      ~locals:
                        [
                          ( Reference.create lambda_parameter,
                            TypeInfo.Unit.create_mutable parameter_type );
                        ]
                      lambda_body
                    |> Type.weaken_literals
                  in
                  let parameters =
                    Type.Callable.CallableParamType.create
                      [
                        {
                          Type.Callable.CallableParamType.name = lambda_parameter;
                          annotation = parameter_type;
                          default = false;
                        };
                      ]
                  in
                  Type.Callable.create ~parameters:(Defined parameters) ~annotation:return_type ()
                in
                TypeOrder.OrderedConstraintsSet.add_and_simplify
                  constraints_set
                  ~new_constraint:(LessOrEqual { left = resolved; right = annotation })
                  ~order
                (* Once we've used this solution, we have to commit to it *)
                |> TypeOrder.OrderedConstraintsSet.add_and_simplify
                     ~new_constraint:
                       (VariableIsExactly (TypeVarPair (parameter_variable, parameter_type)))
                     ~order
              in
              { signature_match with constraints_set = updated_constraints })
    in
    Map.fold
      ~init:signature_match
      ~f:(fun ~key ~data ->
        check_lambda_argument_and_update_signature_match ~parameter:key ~arguments:data)
      parameter_argument_mapping
  in
  let signature_match =
    {
      callable;
      parameter_argument_mapping;
      constraints_set = [TypeConstraints.empty];
      ranks = { arity = 0; annotation = 0; position = 0 };
      reasons;
    }
  in
  Map.fold
    ~init:signature_match
    ~f:(fun ~key ~data -> check_arguments_and_update_signature_match ~parameter:key ~arguments:data)
    parameter_argument_mapping
  |> special_case_dictionary_constructor
  |> special_case_lambda_parameter
  |> check_if_solution_exists


(** Check arguments against the given callable signature and returning possible signature matches. *)
let rec check_arguments_against_signature
    ~order
    ~resolve_mutable_literals
    ~resolve_with_locals
    ~get_typed_dictionary
    ~location
    ~callable
    ~self_argument
    ~(arguments : Type.t Argument.WithPosition.t list)
    implementation
  =
  let open SignatureSelectionTypes in
  let open Type.Callable in
  let callable = { callable with Type.Callable.implementation; overloads = [] } in
  let base_signature_match =
    {
      callable;
      parameter_argument_mapping = CallableParamType.Map.empty;
      constraints_set = [TypeConstraints.empty];
      ranks = { arity = 0; annotation = 0; position = 0 };
      reasons = empty_reasons;
    }
  in
  let { parameters = all_parameters; _ } = implementation in
  let check_arguments_against_parameters =
    check_arguments_against_parameters
      ~location
      ~order
      ~resolve_mutable_literals
      ~resolve_with_locals
      ~get_typed_dictionary
      ~callable
  in
  match all_parameters with
  | Defined parameters ->
      get_parameter_argument_mapping
        ~parameters
        ~all_parameters
        ~self_argument
        ~order
        ~location
        ~resolve:(resolve_with_locals ~locals:[])
        ~get_typed_dictionary
        arguments
      |> check_arguments_against_parameters
      |> fun signature_match -> [signature_match]
  | Undefined -> [base_signature_match]
  | FromParamSpec { head; variable } when Type.Variable.ParamSpec.is_free variable -> (
      (* Handle callables where an early parameter binds a ParamSpec and later parameters expect the
         corresponding arguments.

         For example, when a function like `def foo(f: Callable[P, R], *args: P.args, **kwargs:
         P.kwargs) -> None` is called as `foo(add, 1, 2)`, first solve for the free variable `P`
         using the callable argument `add` and then use the solution to get concrete types for
         `P.args` and `P.kwargs`. *)
      let front, back =
        let is_labeled = function
          | { Argument.WithPosition.kind = Named _; _ } -> true
          | _ -> false
        in
        (* extract the first N unlabeled arguments, keeping the remaining arguments in order *)
        let rec partition_first_unlabeled left right n args =
          match n, args with
          | 0, _
          | _, [] ->
              List.rev left, List.rev right @ args
          | _, hd :: tl when is_labeled hd -> partition_first_unlabeled left (hd :: right) n tl
          | _, hd :: tl -> partition_first_unlabeled (hd :: left) right (n - 1) tl
        in
        partition_first_unlabeled [] [] (List.length head) arguments
      in
      let ({ constraints_set; reasons = { arity = head_arity; annotation = head_annotation }; _ } as
          head_signature)
        =
        get_parameter_argument_mapping
          ~all_parameters
          ~parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
          ~self_argument
          ~order
          ~location
          ~resolve:(resolve_with_locals ~locals:[])
          ~get_typed_dictionary
          front
        |> check_arguments_against_parameters
      in
      let solve_back parameters =
        let constraints_set =
          (* If we use this option, we have to commit to it as to not move away from it later *)
          TypeOrder.OrderedConstraintsSet.add_and_simplify
            constraints_set
            ~new_constraint:(VariableIsExactly (ParamSpecPair (variable, parameters)))
            ~order
        in
        check_arguments_against_signature
          ~order
          ~resolve_mutable_literals
          ~resolve_with_locals
          ~get_typed_dictionary
          ~location
          ~callable
          ~self_argument
          ~arguments:back
          { implementation with parameters }
        |> List.map ~f:(fun { reasons = { arity = tail_arity; annotation = tail_annotation }; _ } ->
               {
                 base_signature_match with
                 constraints_set;
                 reasons =
                   {
                     arity = head_arity @ tail_arity;
                     annotation = head_annotation @ tail_annotation;
                   };
               })
      in
      TypeOrder.OrderedConstraintsSet.get_parameter_specification_possibilities
        constraints_set
        ~parameter_specification:variable
        ~order
      |> List.concat_map ~f:solve_back
      |> function
      | [] -> [head_signature]
      | nonempty -> nonempty)
  | FromParamSpec { head; variable } -> (
      (* The ParamSpec variable `P` is in scope, so the only valid arguments are `*args` and
         `**kwargs` that have "type" `P.args` and `P.kwargs` respectively. If the ParamSpec has a
         `head` prefix of parameters, check for any prefix arguments. *)
      let combines_into_variable ~positional_component ~keyword_component =
        Type.Variable.ParamSpec.Components.combine { positional_component; keyword_component }
        >>| Type.Variable.ParamSpec.equal variable
        |> Option.value ~default:false
      in
      match List.rev arguments with
      | { kind = DoubleStar; resolved = keyword_component; _ }
        :: { kind = SingleStar; resolved = positional_component; _ }
        :: reversed_arguments_head
        when combines_into_variable ~positional_component ~keyword_component ->
          let arguments = List.rev reversed_arguments_head in
          get_parameter_argument_mapping
            ~parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
            ~all_parameters
            ~self_argument
            ~order
            ~location
            ~resolve:(resolve_with_locals ~locals:[])
            ~get_typed_dictionary
            arguments
          |> check_arguments_against_parameters
          |> fun signature_match -> [signature_match]
      | _ ->
          [
            {
              base_signature_match with
              reasons = { arity = [CallingFromParamSpec]; annotation = [] };
            };
          ])


let most_important_error_reason ~arity_mismatch_reasons annotation_mismatch_reasons =
  let open SignatureSelectionTypes in
  let remove_self_argument_errors reasons =
    let remove_self_related_errors = function
      | TooManyArguments { expected; _ } when Int.equal expected (-1) ->
          (* This arises when calling a method that lacks a `self` parameter. We already error about
             that on the method definition, so don't repeat it for every call of that method. *)
          None
      | Mismatches mismatches ->
          let mismatches =
            List.filter mismatches ~f:(function
                | Mismatch { Node.value = { position = 0; actual; _ }; _ } ->
                    (* A mismatch on the 0th parameter, i.e., the `self` parameter, is a sign that
                       the explicit `self` annotation was wrong, since you wouldn't be able to look
                       up that method otherwise. We already error about invalid `self` annotations,
                       so don't emit an error for every call of that method.

                       However, we preserve mismatches when the `self` argument is `ReadOnly`. This
                       indicates that a mutating method was called on a readonly object, which
                       should be surfaced at the method call site. *)
                    Type.PyreReadOnly.is_readonly actual
                | _ -> true)
          in
          Mismatches mismatches |> Option.some
      | reason -> Some reason
    in
    List.rev_filter_map ~f:remove_self_related_errors reasons
  in
  match
    ( remove_self_argument_errors arity_mismatch_reasons,
      remove_self_argument_errors annotation_mismatch_reasons )
  with
  | [], [] -> None
  | reason :: reasons, _
  | [], reason :: reasons ->
      let importance = function
        | AbstractClassInstantiation _ -> 1
        | CallingFromParamSpec -> 1
        | NonInstantiableSpecialForm _ -> 1
        | InvalidKeywordArgument _ -> 0
        | InvalidVariableArgument _ -> 0
        | Mismatches _ -> -1
        | MissingArgument _ -> 1
        | MutuallyRecursiveTypeVariables -> 1
        | ProtocolInstantiation _ -> 1
        | TooManyArguments _ -> 1
        | TypedDictionaryInitializationError _ -> 1
        | UnexpectedKeyword _ -> 1
      in
      let get_most_important best_reason reason =
        if importance reason > importance best_reason then
          reason
        else
          match best_reason, reason with
          | Mismatches mismatches, Mismatches other_mismatches ->
              Mismatches (List.append mismatches other_mismatches)
          | _, _ -> best_reason
      in
      let sort_mismatches reason =
        match reason with
        | Mismatches mismatches ->
            let compare_mismatches mismatch other_mismatch =
              match mismatch, other_mismatch with
              | ( Mismatch { Node.value = { position; _ }; _ },
                  Mismatch { Node.value = { position = other_position; _ }; _ } ) ->
                  position - other_position
              | _, _ -> 0
            in
            Mismatches (List.sort mismatches ~compare:compare_mismatches)
        | _ -> reason
      in
      Some (List.fold ~init:reason ~f:get_most_important reasons |> sort_mismatches)


(** Given a signature match for a callable, solve for any type variables and instantiate the return
    annotation. *)
let instantiate_return_annotation
    ?(skip_marking_escapees = false)
    ~order
    {
      callable =
        { implementation = { annotation = uninstantiated_return_annotation; _ }; _ } as callable;
      constraints_set;
      reasons = { arity = arity_mismatch_reasons; annotation = annotation_mismatch_reasons; _ };
      _;
    }
  =
  let local_free_variables = Type.Variable.all_free_variables (Type.Callable callable) in
  let solution =
    TypeOrder.OrderedConstraintsSet.solve
      constraints_set
      ~only_solve_for:local_free_variables
      ~order
    |> Option.value ~default:TypeConstraints.Solution.empty
  in
  let instantiated =
    TypeConstraints.Solution.instantiate solution uninstantiated_return_annotation
  in
  let instantiated_return_annotation =
    if skip_marking_escapees then
      instantiated
    else
      Type.Variable.mark_all_free_variables_as_escaped ~specific:local_free_variables instantiated
      (* We need to do transformations of the form Union[T_escaped, int] => int in order to properly
         handle some typeshed stubs which only sometimes bind type variables and expect them to fall
         out in this way (see Mapping.get) *)
      |> Type.Variable.collapse_all_escaped_variable_unions
  in
  match most_important_error_reason ~arity_mismatch_reasons annotation_mismatch_reasons with
  | None ->
      SignatureSelectionTypes.Found { selected_return_annotation = instantiated_return_annotation }
  | Some reason ->
      NotFound { closest_return_annotation = instantiated_return_annotation; reason = Some reason }


let default_instantiated_return_annotation
    { Type.Callable.implementation = { annotation = default_return_annotation; _ }; _ }
  =
  let open SignatureSelectionTypes in
  NotFound { closest_return_annotation = default_return_annotation; reason = None }


let calculate_rank ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
  let open SignatureSelectionTypes in
  let arity_rank = List.length arity in
  let positions, annotation_rank =
    let count_unique (positions, count) = function
      | Mismatches mismatches ->
          let count_unique_mismatches (positions, count) mismatch =
            match mismatch with
            | Mismatch { Node.value = { position; _ }; _ } when not (Set.mem positions position) ->
                Set.add positions position, count + 1
            | Mismatch _ -> positions, count
            | _ -> positions, count + 1
          in
          List.fold ~init:(positions, count) mismatches ~f:count_unique_mismatches
      | _ -> positions, count + 1
    in
    List.fold ~init:(Int.Set.empty, 0) ~f:count_unique annotation
  in
  let position_rank = Set.min_elt positions >>| Int.neg |> Option.value ~default:Int.min_value in
  {
    signature_match with
    ranks = { arity = arity_rank; annotation = annotation_rank; position = position_rank };
  }


(** Find the signature that is "closest" to what the user intended. Essentially, sort signatures on
    the number of arity mismatches, number of annotation mismatches, and the earliest mismatch
    position.

    TODO(T109092235): Clean up the rank calculation to more clearly reflect that we want to do
    `maximum_by (arity, annotation, position)`. *)
let find_closest_signature signature_matches =
  let get_arity_rank { ranks = { arity; _ }; _ } = arity in
  let get_annotation_rank { ranks = { annotation; _ }; _ } = annotation in
  let get_position_rank { ranks = { position; _ }; _ } = position in
  let rec get_best_rank ~best_matches ~best_rank ~getter = function
    | [] -> best_matches
    | head :: tail ->
        let rank = getter head in
        if rank < best_rank then
          get_best_rank ~best_matches:[head] ~best_rank:rank ~getter tail
        else if rank = best_rank then
          get_best_rank ~best_matches:(head :: best_matches) ~best_rank ~getter tail
        else
          get_best_rank ~best_matches ~best_rank ~getter tail
  in
  signature_matches
  |> List.map ~f:calculate_rank
  |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_arity_rank
  |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_annotation_rank
  |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_position_rank
  (* Each get_best_rank reverses the list, because we have an odd number, we need an extra reverse
     in order to prefer the first defined overload *)
  |> List.rev
  |> List.hd


(** Select the closest overload for [callable] when it is called with [self_argument] and
    [arguments]. If there are no overloads, just return results for the base implementation.

    Return a [signature_match] containing the selected signature along with errors and constraints
    for any type variables. *)
let select_closest_signature_for_function_call
    ~order
    ~resolve_with_locals
    ~resolve_mutable_literals
    ~get_typed_dictionary
    ~arguments
    ~location
    ~callable:({ Type.Callable.implementation; overloads; _ } as callable)
    ~self_argument
  =
  let get_match signatures =
    let check_arguments_against_signature =
      check_arguments_against_signature
        ~order
        ~resolve_with_locals
        ~resolve_mutable_literals
        ~get_typed_dictionary
        ~location
        ~callable
        ~self_argument
        ~arguments:(prepare_arguments_for_signature_selection ~self_argument arguments)
    in
    signatures |> List.concat_map ~f:check_arguments_against_signature |> find_closest_signature
  in
  if List.is_empty overloads then
    get_match [implementation]
  else
    get_match overloads

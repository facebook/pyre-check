(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Expression

type mismatch = {
  actual: Type.t;
  actual_expression: Expression.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show, compare]

type invalid_argument = {
  expression: Expression.t;
  annotation: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]

type missing_argument =
  | Named of Identifier.t
  | Anonymous of int
[@@deriving eq, show, compare, sexp, hash]

type mismatch_with_list_variadic_type_variable =
  | NotDefiniteTuple of invalid_argument
  | CantConcatenate of Type.OrderedTypes.t list
  | ConstraintFailure of Type.OrderedTypes.t
[@@deriving compare, eq, show, sexp, hash]

type reason =
  | AbstractClassInstantiation of Reference.t
  | CallingParameterVariadicTypeVariable
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatch of mismatch Node.t
  | MismatchWithListVariadicTypeVariable of
      Type.OrderedTypes.t * mismatch_with_list_variadic_type_variable
  | MissingArgument of missing_argument
  | MutuallyRecursiveTypeVariables
  | ProtocolInstantiation of Reference.t
  | TooManyArguments of {
      expected: int;
      provided: int;
    }
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]

type closest = {
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving show]

let equal_closest (left : closest) (right : closest) =
  (* Ignore rank. *)
  Type.Callable.equal left.callable right.callable
  && Option.equal equal_reason left.reason right.reason


type t =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]

module Argument = struct
  type kind =
    | SingleStar
    | DoubleStar
    | Named of string Node.t
    | Positional

  type t = {
    expression: Expression.t;
    full_expression: Expression.t;
    position: int;
    kind: kind;
    resolved: Type.t;
  }
end

type argument =
  | Argument of Argument.t
  | Default

type ranks = {
  arity: int;
  annotation: int;
  position: int;
}

type reasons = {
  arity: reason list;
  annotation: reason list;
}

type signature_match = {
  callable: Type.Callable.t;
  argument_mapping: argument list Type.Callable.Parameter.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}

let select
    ~resolution
    ~arguments
    ~callable:({ Type.Callable.implementation; overloads; _ } as callable)
  =
  let open Type.Callable in
  let match_arity ({ parameters = all_parameters; _ } as implementation) =
    let all_arguments = arguments in
    let base_signature_match =
      {
        callable = { callable with Type.Callable.implementation; overloads = [] };
        argument_mapping = Parameter.Map.empty;
        constraints_set = [TypeConstraints.empty];
        ranks = { arity = 0; annotation = 0; position = 0 };
        reasons = { arity = []; annotation = [] };
      }
    in
    let rec consume
        ({ argument_mapping; reasons = { arity; _ } as reasons; _ } as signature_match)
        ~arguments
        ~parameters
      =
      let update_mapping parameter argument =
        Map.add_multi argument_mapping ~key:parameter ~data:argument
      in
      let arity_mismatch ?(unreachable_parameters = []) ~arguments reasons =
        match all_parameters with
        | Defined all_parameters ->
            let matched_keyword_arguments =
              let is_keyword_argument = function
                | { Call.Argument.name = Some _; _ } -> true
                | _ -> false
              in
              List.filter ~f:is_keyword_argument all_arguments
            in
            let positional_parameter_count =
              List.length all_parameters
              - List.length unreachable_parameters
              - List.length matched_keyword_arguments
            in
            let error =
              TooManyArguments
                {
                  expected = positional_parameter_count;
                  provided = positional_parameter_count + List.length arguments;
                }
            in
            { reasons with arity = error :: arity }
        | _ -> reasons
      in
      match arguments, parameters with
      | [], [] ->
          (* Both empty *)
          signature_match
      | { Argument.kind = Argument.SingleStar; _ } :: arguments_tail, []
      | { kind = DoubleStar; _ } :: arguments_tail, [] ->
          (* Starred or double starred arguments; parameters empty *)
          consume ~arguments:arguments_tail ~parameters signature_match
      | { kind = Named name; _ } :: _, [] ->
          (* Named argument; parameters empty *)
          let reasons = { reasons with arity = UnexpectedKeyword name.value :: arity } in
          { signature_match with reasons }
      | _, [] ->
          (* Positional argument; parameters empty *)
          { signature_match with reasons = arity_mismatch ~arguments reasons }
      | [], (Parameter.KeywordOnly { default = true; _ } as parameter) :: parameters_tail
      | [], (Parameter.Anonymous { default = true; _ } as parameter) :: parameters_tail
      | [], (Parameter.Named { default = true; _ } as parameter) :: parameters_tail ->
          (* Arguments empty, default parameter *)
          let argument_mapping = update_mapping parameter Default in
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | [], parameter :: parameters_tail ->
          (* Arguments empty, parameter *)
          let argument_mapping =
            match Map.find argument_mapping parameter with
            | Some _ -> argument_mapping
            | None -> Map.set ~key:parameter ~data:[] argument_mapping
          in
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | ( ({ kind = Named _; _ } as argument) :: arguments_tail,
          (Parameter.Keywords _ as parameter) :: _ ) ->
          (* Labeled argument, keywords parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume ~arguments:arguments_tail ~parameters { signature_match with argument_mapping }
      | ({ kind = Named name; _ } as argument) :: arguments_tail, parameters ->
          (* Labeled argument *)
          let rec extract_matching_name searched to_search =
            match to_search with
            | [] -> None, List.rev searched
            | (Parameter.KeywordOnly { name = parameter_name; _ } as head) :: tail
            | (Parameter.Named { name = parameter_name; _ } as head) :: tail
              when Identifier.equal_sanitized parameter_name name.value ->
                Some head, List.rev searched @ tail
            | (Parameter.Keywords _ as head) :: tail ->
                let matching, parameters = extract_matching_name (head :: searched) tail in
                let matching = Some (Option.value matching ~default:head) in
                matching, parameters
            | head :: tail -> extract_matching_name (head :: searched) tail
          in
          let matching_parameter, remaining_parameters = extract_matching_name [] parameters in
          let argument_mapping, reasons =
            match matching_parameter with
            | Some matching_parameter ->
                update_mapping matching_parameter (Argument argument), reasons
            | None ->
                argument_mapping, { reasons with arity = UnexpectedKeyword name.value :: arity }
          in
          consume
            ~arguments:arguments_tail
            ~parameters:remaining_parameters
            { signature_match with argument_mapping; reasons }
      | ( ({ kind = DoubleStar; _ } as argument) :: arguments_tail,
          (Parameter.Keywords _ as parameter) :: _ )
      | ( ({ kind = SingleStar; _ } as argument) :: arguments_tail,
          (Parameter.Variable _ as parameter) :: _ ) ->
          (* (Double) starred argument, (double) starred parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume ~arguments:arguments_tail ~parameters { signature_match with argument_mapping }
      | { kind = SingleStar; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
          (* Starred argument, double starred parameter *)
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | { kind = Positional; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
          (* Unlabeled argument, double starred parameter *)
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | { kind = DoubleStar; _ } :: _, Parameter.Variable _ :: parameters_tail ->
          (* Double starred argument, starred parameter *)
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | ( ({ kind = Positional; _ } as argument) :: arguments_tail,
          (Parameter.Variable _ as parameter) :: _ ) ->
          (* Unlabeled argument, starred parameter *)
          let signature_match =
            let argument_mapping = update_mapping parameter (Argument argument) in
            { signature_match with argument_mapping }
          in
          consume ~arguments:arguments_tail ~parameters signature_match
      | { kind = SingleStar; _ } :: arguments_tail, Type.Callable.Parameter.KeywordOnly _ :: _ ->
          (* Starred argument, keyword only parameter *)
          consume ~arguments:arguments_tail ~parameters signature_match
      | ({ kind = DoubleStar; _ } as argument) :: _, parameter :: parameters_tail
      | ({ kind = SingleStar; _ } as argument) :: _, parameter :: parameters_tail ->
          (* Double starred or starred argument, parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
      | { kind = Positional; _ } :: _, (Parameter.KeywordOnly _ as parameter) :: parameters_tail ->
          (* Unlabeled argument, keyword only parameter *)
          let reasons =
            arity_mismatch
              reasons
              ~unreachable_parameters:(parameter :: parameters_tail)
              ~arguments
          in
          { signature_match with reasons }
      | ({ kind = Positional; _ } as argument) :: arguments_tail, parameter :: parameters_tail ->
          (* Unlabeled argument, parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume
            ~arguments:arguments_tail
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
    in
    let ordered_arguments () =
      let create_argument index { Call.Argument.name; value } =
        let expression, kind =
          match value, name with
          | { Node.value = Starred (Starred.Once expression); _ }, _ ->
              expression, Argument.SingleStar
          | { Node.value = Starred (Starred.Twice expression); _ }, _ -> expression, DoubleStar
          | expression, Some name -> expression, Named name
          | expression, None -> expression, Positional
        in
        let resolved = Resolution.resolve resolution expression in
        { Argument.position = index + 1; expression; full_expression = value; kind; resolved }
      in
      let is_labeled = function
        | { Argument.kind = Named _; _ } -> true
        | _ -> false
      in
      let labeled_arguments, unlabeled_arguments =
        arguments |> List.mapi ~f:create_argument |> List.partition_tf ~f:is_labeled
      in
      labeled_arguments @ unlabeled_arguments
    in
    match all_parameters with
    | Defined parameters ->
        consume base_signature_match ~arguments:(ordered_arguments ()) ~parameters
    | Undefined -> base_signature_match
    | ParameterVariadicTypeVariable variable -> (
        let combines_into_variable ~positional_component ~keyword_component =
          Type.Variable.Variadic.Parameters.Components.combine
            { positional_component; keyword_component }
          >>| Type.Variable.Variadic.Parameters.equal variable
          |> Option.value ~default:false
        in
        match ordered_arguments () with
        | [ { kind = SingleStar; resolved = positional_component; _ };
            { kind = DoubleStar; resolved = keyword_component; _ } ]
          when combines_into_variable ~positional_component ~keyword_component ->
            base_signature_match
        | _ ->
            {
              base_signature_match with
              reasons = { arity = [CallingParameterVariadicTypeVariable]; annotation = [] };
            } )
  in
  let check_annotations ({ argument_mapping; _ } as signature_match) =
    let update ~key ~data ({ reasons = { arity; _ } as reasons; _ } as signature_match) =
      let bind_arguments_to_variadic ~expected ~arguments =
        let extract arguments =
          let extracted, errors =
            let arguments =
              List.map arguments ~f:(function
                  | Argument argument -> argument
                  | Default -> failwith "Variable parameters do not have defaults")
            in
            let extract { Argument.kind; resolved; expression; _ } =
              match kind with
              | SingleStar -> (
                match resolved with
                | Type.Tuple (Bounded ordered_types) -> `Fst ordered_types
                (* We don't support expanding indefinite containers into ListVariadics *)
                | annotation -> `Snd { expression; annotation } )
              | _ -> `Fst (Type.OrderedTypes.Concrete [resolved])
            in
            List.rev arguments |> List.partition_map ~f:extract
          in
          match errors with
          | [] -> Ok extracted
          | not_definite_tuple :: _ ->
              Error
                (MismatchWithListVariadicTypeVariable
                   (expected, NotDefiniteTuple not_definite_tuple))
        in
        let concatenate extracted =
          let concatenated =
            match extracted with
            | [] -> Some (Type.OrderedTypes.Concrete [])
            | head :: tail ->
                let concatenate sofar next =
                  sofar >>= fun left -> Type.OrderedTypes.concatenate ~left ~right:next
                in
                List.fold tail ~f:concatenate ~init:(Some head)
          in
          match concatenated with
          | Some concatenated -> Ok concatenated
          | None ->
              Error (MismatchWithListVariadicTypeVariable (expected, CantConcatenate extracted))
        in
        let solve concatenated =
          match
            List.concat_map signature_match.constraints_set ~f:(fun constraints ->
                GlobalResolution.solve_ordered_types_less_or_equal
                  (Resolution.global_resolution resolution)
                  ~left:concatenated
                  ~right:expected
                  ~constraints)
          with
          | [] ->
              Error
                (MismatchWithListVariadicTypeVariable (expected, ConstraintFailure concatenated))
          | updated_constraints_set -> Ok updated_constraints_set
        in
        let make_signature_match = function
          | Ok constraints_set -> { signature_match with constraints_set }
          | Error error ->
              { signature_match with reasons = { reasons with arity = error :: arity } }
        in
        let open Result in
        extract arguments >>= concatenate >>= solve |> make_signature_match
      in
      match key, data with
      | Parameter.Variable (Concatenation concatenation), arguments ->
          bind_arguments_to_variadic
            ~expected:(Type.OrderedTypes.Concatenation concatenation)
            ~arguments
      | Parameter.Variable _, []
      | Parameter.Keywords _, [] ->
          (* Parameter was not matched, but empty is acceptable for variable arguments and keyword
             arguments. *)
          signature_match
      | Parameter.KeywordOnly { name; _ }, []
      | Parameter.Named { name; _ }, [] ->
          (* Parameter was not matched *)
          let reasons = { reasons with arity = MissingArgument (Named name) :: arity } in
          { signature_match with reasons }
      | Parameter.Anonymous { index; _ }, [] ->
          (* Parameter was not matched *)
          let reasons = { reasons with arity = MissingArgument (Anonymous index) :: arity } in
          { signature_match with reasons }
      | Anonymous { annotation = parameter_annotation; _ }, arguments
      | KeywordOnly { annotation = parameter_annotation; _ }, arguments
      | Named { annotation = parameter_annotation; _ }, arguments
      | Variable (Concrete parameter_annotation), arguments
      | Keywords parameter_annotation, arguments ->
          let rec set_constraints_and_reasons
              ~resolution
              ~position
              ~argument
              ~name
              ~argument_annotation
              ({ constraints_set; reasons = { annotation; _ }; _ } as signature_match)
            =
            let reasons_with_mismatch =
              let mismatch =
                let location =
                  name >>| Node.location |> Option.value ~default:argument.Node.location
                in
                {
                  actual = argument_annotation;
                  actual_expression = argument;
                  expected = parameter_annotation;
                  name = Option.map name ~f:Node.value;
                  position;
                }
                |> Node.create ~location
                |> fun mismatch -> Mismatch mismatch
              in
              { reasons with annotation = mismatch :: annotation }
            in
            match
              List.concat_map constraints_set ~f:(fun constraints ->
                  GlobalResolution.solve_less_or_equal
                    (Resolution.global_resolution resolution)
                    ~constraints
                    ~left:argument_annotation
                    ~right:parameter_annotation)
            with
            | [] -> { signature_match with constraints_set; reasons = reasons_with_mismatch }
            | updated_constraints_set ->
                { signature_match with constraints_set = updated_constraints_set }
          in
          let rec check signature_match = function
            | [] -> signature_match
            | Default :: tail ->
                (* Parameter default value was used. Assume it is correct. *)
                check signature_match tail
            | Argument { expression; full_expression; position; kind; resolved } :: tail -> (
                let set_constraints_and_reasons argument_annotation =
                  let name =
                    match kind with
                    | Named name -> Some name
                    | _ -> None
                  in
                  set_constraints_and_reasons
                    ~resolution
                    ~position
                    ~argument:full_expression
                    ~argument_annotation
                    ~name
                    signature_match
                  |> fun signature_match -> check signature_match tail
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
                let solution_based_extraction ~create_error ~synthetic_variable ~solve_against =
                  let signature_with_error =
                    { expression; annotation = resolved }
                    |> Node.create ~location:expression.location
                    |> create_error
                    |> add_annotation_error signature_match
                  in
                  let iterable_constraints =
                    if Type.is_unbound resolved then
                      []
                    else
                      GlobalResolution.solve_less_or_equal
                        (Resolution.global_resolution resolution)
                        ~constraints:TypeConstraints.empty
                        ~left:resolved
                        ~right:solve_against
                  in
                  match iterable_constraints with
                  | [] -> signature_with_error
                  | iterable_constraint :: _ ->
                      GlobalResolution.solve_constraints
                        (Resolution.global_resolution resolution)
                        iterable_constraint
                      >>= (fun solution ->
                            TypeConstraints.Solution.instantiate_single_variable
                              solution
                              synthetic_variable)
                      >>| set_constraints_and_reasons
                      |> Option.value ~default:signature_with_error
                in
                match kind with
                | DoubleStar ->
                    let create_error error = InvalidKeywordArgument error in
                    let synthetic_variable = Type.Variable.Unary.create "$_T" in
                    let solve_against =
                      Type.parametric
                        "typing.Mapping"
                        (Concrete [Type.string; Type.Variable synthetic_variable])
                    in
                    solution_based_extraction ~create_error ~synthetic_variable ~solve_against
                | SingleStar ->
                    let create_error error = InvalidVariableArgument error in
                    let synthetic_variable = Type.Variable.Unary.create "$_T" in
                    let solve_against = Type.iterable (Type.Variable synthetic_variable) in
                    solution_based_extraction ~create_error ~synthetic_variable ~solve_against
                | Named _
                | Positional ->
                    let argument_annotation =
                      if Type.Variable.all_variables_are_resolved parameter_annotation then
                        Resolution.resolve_mutable_literals
                          resolution
                          ~expression:(Some expression)
                          ~resolved
                          ~expected:parameter_annotation
                      else
                        resolved
                    in
                    if Type.is_meta parameter_annotation && Type.is_top argument_annotation then
                      GlobalResolution.parse_annotation
                        (Resolution.global_resolution resolution)
                        expression
                      |> Type.meta
                      |> set_constraints_and_reasons
                    else
                      argument_annotation |> set_constraints_and_reasons )
          in
          List.rev arguments |> check signature_match
    in
    let check_if_solution_exists
        ( { constraints_set; reasons = { annotation; _ } as reasons; callable; _ } as
        signature_match )
      =
      let solutions =
        let variables = Type.Variable.all_free_variables (Type.Callable callable) in
        List.filter_map
          constraints_set
          ~f:
            (GlobalResolution.partial_solve_constraints
               ~variables
               (Resolution.global_resolution resolution))
      in
      if not (List.is_empty solutions) then
        signature_match
      else
        (* All other cases should have been able to been blamed on a specefic argument, this is the
           only global failure. *)
        {
          signature_match with
          reasons = { reasons with annotation = MutuallyRecursiveTypeVariables :: annotation };
        }
    in
    let special_case_dictionary_constructor
        ({ argument_mapping; callable; constraints_set; _ } as signature_match)
      =
      let open Type.Record.Callable in
      let has_matched_keyword_parameter parameters =
        List.find parameters ~f:(function
            | RecordParameter.Keywords _ -> true
            | _ -> false)
        >>= Type.Callable.Parameter.Map.find argument_mapping
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
           annotation = Type.Parametric { parameters = Concrete [key_type; _]; _ };
           _;
         };
       _;
      }
        when String.equal (Reference.show name) "dict.__init__"
             && has_matched_keyword_parameter parameters ->
          let updated_constraints =
            List.concat_map constraints_set ~f:(fun constraints ->
                GlobalResolution.solve_less_or_equal
                  (Resolution.global_resolution resolution)
                  ~constraints
                  ~left:Type.string
                  ~right:key_type)
          in
          if List.is_empty updated_constraints then (* TODO(T41074174): Error here *)
            signature_match
          else
            { signature_match with constraints_set = updated_constraints }
      | _ -> signature_match
    in
    Map.fold ~init:signature_match ~f:update argument_mapping
    |> special_case_dictionary_constructor
    |> check_if_solution_exists
  in
  let calculate_rank ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
    let arity_rank = List.length arity in
    let positions, annotation_rank =
      let count_unique (positions, count) = function
        | Mismatch { Node.value = { position; _ }; _ } when not (Set.mem positions position) ->
            Set.add positions position, count + 1
        | Mismatch _ -> positions, count
        | _ -> positions, count + 1
      in
      List.fold ~init:(Int.Set.empty, 0) ~f:count_unique annotation
    in
    let position_rank =
      Int.Set.min_elt positions >>| Int.neg |> Option.value ~default:Int.min_value
    in
    {
      signature_match with
      ranks = { arity = arity_rank; annotation = annotation_rank; position = position_rank };
    }
  in
  let find_closest signature_matches =
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
    let determine_reason { callable; constraints_set; reasons = { arity; annotation; _ }; _ } =
      let callable =
        let instantiate annotation =
          let solution =
            let variables = Type.Variable.all_free_variables (Type.Callable callable) in
            List.filter_map
              constraints_set
              ~f:
                (GlobalResolution.partial_solve_constraints
                   ~variables
                   (Resolution.global_resolution resolution))
            |> List.map ~f:snd
            |> List.hd
            |> Option.value ~default:TypeConstraints.Solution.empty
          in
          TypeConstraints.Solution.instantiate solution annotation
          |> Type.Variable.mark_all_free_variables_as_escaped
          (* We need to do transformations of the form Union[T_escaped, int] => int in order to
             properly handle some typeshed stubs which only sometimes bind type variables and
             expect them to fall out in this way (see Mapping.get) *)
          |> Type.Variable.collapse_all_escaped_variable_unions
        in
        Type.Callable.map ~f:instantiate callable
        |> function
        | Some callable -> callable
        | _ -> failwith "Instantiate did not return a callable"
      in
      match List.rev arity, List.rev annotation with
      | [], [] -> Found callable
      | reason :: reasons, _
      | [], reason :: reasons ->
          let importance = function
            | AbstractClassInstantiation _ -> 1
            | CallingParameterVariadicTypeVariable -> 1
            | InvalidKeywordArgument _ -> 0
            | InvalidVariableArgument _ -> 0
            | Mismatch { Node.value = { position; _ }; _ } -> 0 - position
            | MissingArgument _ -> 1
            | MismatchWithListVariadicTypeVariable _ -> 1
            | MutuallyRecursiveTypeVariables -> 1
            | ProtocolInstantiation _ -> 1
            | TooManyArguments _ -> 1
            | UnexpectedKeyword _ -> 1
          in
          let get_most_important best_reason reason =
            if importance reason > importance best_reason then
              reason
            else
              best_reason
          in
          let reason = Some (List.fold ~init:reason ~f:get_most_important reasons) in
          NotFound { callable; reason }
    in
    signature_matches
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_arity_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_annotation_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_position_rank
    (* Each get_best_rank reverses the list, because we have an odd number, we need an extra
       reverse in order to prefer the first defined overload *)
    |> List.rev
    |> List.hd
    >>| determine_reason
    |> Option.value ~default:(NotFound { callable; reason = None })
  in
  let get_match signatures =
    signatures
    |> List.map ~f:match_arity
    |> List.map ~f:check_annotations
    |> List.map ~f:calculate_rank
    |> find_closest
  in
  if List.is_empty overloads then
    get_match [implementation]
  else if Type.Callable.Overload.is_undefined implementation then
    get_match overloads
  else
    (* TODO(T41195241) always ignore implementation when has overloads. Currently put
       implementation as last resort *)
    match get_match overloads with
    | Found signature_match -> Found signature_match
    | NotFound _ -> get_match [implementation]

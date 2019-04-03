(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Expression

module Class = AnnotatedClass


type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show, compare]


type invalid_argument = {
  expression: Expression.t;
  annotation: Type.t;
}
[@@deriving eq, show, compare]


type reason =
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatch of mismatch Node.t
  | MissingArgument of Identifier.t
  | MutuallyRecursiveTypeVariables
  | TooManyArguments of { expected: int; provided: int }
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]


type found = {
  callable: Type.Callable.t;
  constraints: Type.t Type.Map.t;
}
[@@deriving eq]


let pp_found format { callable; _ } =
  Format.fprintf format "%a" Type.Callable.pp callable


let show_found =
  Format.asprintf "%a" pp_found


let equal_found (left: found) (right: found) =
  (* Ignore constraints. *)
  Type.Callable.equal left.callable right.callable


type closest = {
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]


let equal_closest (left: closest) (right: closest) =
  (* Ignore rank. *)
  Type.Callable.equal left.callable right.callable &&
  Option.equal equal_reason left.reason right.reason


type t =
  | Found of found
  | NotFound of closest
[@@deriving eq, show]


type argument =
  | Argument of { argument: Argument.t; position: int }
  | Default
[@@deriving eq, show]


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
  argument_mapping: (argument list) Type.Callable.Parameter.Map.t;
  constraints: TypeConstraints.t;
  ranks: ranks;
  reasons: reasons;
}


let select
    ~resolution
    ~arguments
    ~callable:({ Type.Callable.implementation; overloads; _ } as callable) =
  let open Type.Callable in
  let match_arity ({ parameters = all_parameters; _ } as implementation) =
    let all_arguments = arguments in
    let base_signature_match =
      {
        callable = { callable with Type.Callable.implementation; overloads = [] };
        argument_mapping = Parameter.Map.empty;
        constraints = TypeConstraints.empty;
        ranks = {
          arity = 0;
          annotation = 0;
          position = 0;
        };
        reasons = {
          arity = [];
          annotation = [];
        };
      }
    in
    let rec consume
        ({ argument_mapping; reasons = ({ arity; _ } as reasons); _; } as signature_match)
        ~arguments
        ~parameters =
      let update_mapping parameter argument =
        Map.add_multi argument_mapping ~key:parameter ~data:argument
      in
      let arity_mismatch ?(unreachable_parameters = []) ~arguments reasons =
        match all_parameters with
        | Defined all_parameters ->
            let matched_keyword_arguments =
              let is_keyword_argument = function
                | { Argument.name = Some _; _ } -> true
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
              TooManyArguments {
                expected = positional_parameter_count;
                provided = positional_parameter_count + List.length arguments;
              }
            in
            {
              reasons with
              arity = error :: arity
            }
        | _ ->
            reasons
      in
      match arguments, parameters with
      | [], [] ->
          (* Both empty *)
          signature_match
      | Argument {
          argument = { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ };
          _;
        } :: arguments_tail,
        []
      | Argument {
          argument = { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ };
          _;
        } :: arguments_tail,
        [] ->
          (* Starred or double starred arguments; parameters empty *)
          consume ~arguments:arguments_tail ~parameters signature_match
      | Argument { argument = { Argument.name = Some { Node.value = name; _ }; _ }; _ } :: _, [] ->
          (* Named argument; parameters empty *)
          let reasons = { reasons with arity = (UnexpectedKeyword name) :: arity } in
          { signature_match with reasons }
      | _, [] ->
          (* Positional argument; parameters empty *)
          { signature_match with reasons = arity_mismatch ~arguments reasons }
      | [], (Parameter.Named { Parameter.default = true; _ } as parameter) :: parameters_tail ->
          (* Arguments empty, default parameter *)
          let argument_mapping = update_mapping parameter Default in
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | [], parameter :: parameters_tail ->
          (* Arguments empty, parameter *)
          let argument_mapping =
            match Map.find argument_mapping parameter with
            | Some _ -> argument_mapping
            | None -> Map.set ~key:parameter ~data:[] argument_mapping
          in
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Argument ({ argument = { Argument.name = Some _; _ }; _ } as argument) :: arguments_tail,
        ((Parameter.Keywords _) as parameter) :: _ ->
          (* Labeled argument, keywords parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume
            ~arguments:arguments_tail
            ~parameters
            { signature_match with argument_mapping }
      | Argument ({
          argument = { Argument.name = Some { Node.value = name; _ }; _ };
          _;
        } as argument) :: arguments_tail,
        parameters ->
          (* Labeled argument *)
          let rec extract_matching_name searched to_search =
            match to_search with
            | [] ->
                None, (List.rev searched)
            | (Parameter.Named { Parameter.name = parameter_name; _ } as head) :: tail
              when Identifier.equal parameter_name name ->
                Some head, (List.rev searched) @ tail
            | (Parameter.Keywords _ as head) :: tail ->
                let matching, parameters = extract_matching_name (head :: searched) tail in
                let matching = Some (Option.value matching ~default:head) in
                matching, parameters
            | head :: tail ->
                extract_matching_name (head :: searched) tail
          in
          let matching_parameter, remaining_parameters = extract_matching_name [] parameters in
          let argument_mapping, reasons =
            match matching_parameter with
            | Some matching_parameter ->
                update_mapping matching_parameter (Argument argument), reasons
            | None ->
                argument_mapping, { reasons with arity = (UnexpectedKeyword name) :: arity }
          in
          consume
            ~arguments:arguments_tail
            ~parameters:remaining_parameters
            { signature_match with argument_mapping; reasons }
      | Argument ({
          argument = { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ };
          _;
        } as argument)
        :: arguments_tail,
        (Parameter.Keywords _ as parameter) :: _
      | Argument ({
            argument = { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ };
            _;
          } as argument)
        :: arguments_tail,
        (Parameter.Variable _ as parameter) :: _ ->
          (* (Double) starred argument, (double) starred parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume
            ~arguments:arguments_tail
            ~parameters
            { signature_match with argument_mapping }
      | Argument {
          argument = { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ };
          _;
        } :: _,
        Parameter.Keywords _ :: parameters_tail ->
          (* Starred argument, double starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Argument { argument = { Argument.name = None; _ }; _ } :: _,
        Parameter.Keywords _ :: parameters_tail ->
          (* Unlabeled argument, double starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Argument {
          argument = { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ };
          _;
        } :: _,
        Parameter.Variable _ :: parameters_tail ->
          (* Double starred argument, starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Argument ({ argument = { Argument.name = None; _ }; _ } as argument) :: arguments_tail,
        (Parameter.Variable _ as parameter) :: parameters_tail ->
          (* Unlabeled argument, starred parameter *)
          let signature_match =
            if Identifier.sanitized (Parameter.name parameter) = "*" then
              let reasons =
                arity_mismatch
                  reasons
                  ~unreachable_parameters:(parameter :: parameters_tail)
                  ~arguments
              in
              {
                signature_match with
                reasons;
              }
            else
              let argument_mapping = update_mapping parameter (Argument argument) in
              { signature_match with argument_mapping }
          in
          consume
            ~arguments:arguments_tail
            ~parameters
            signature_match
      | Argument ({
          argument = { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ };
          _;
        } as argument) :: _,
        parameter :: parameters_tail
      | Argument ({
            argument = { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ };
            _;
          } as argument) :: _,
        parameter :: parameters_tail ->
          (* Double starred or starred argument, parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume
            ~arguments
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Argument ({ argument = { Argument.name = None; _ }; _ } as argument) :: arguments_tail,
        parameter :: parameters_tail ->
          (* Unlabeled argument, parameter *)
          let argument_mapping = update_mapping parameter (Argument argument) in
          consume
            ~arguments:arguments_tail
            ~parameters:parameters_tail
            { signature_match with argument_mapping }
      | Default :: _, _ ->
          failwith "Unpossible"
    in
    match all_parameters with
    | Defined parameters ->
        let ordered_arguments =
          let add_original_positions index argument =
            Argument { argument; position = index + 1 }
          in
          let is_labeled = function
            | Argument { argument = { Argument.name = Some _; _ }; _ } -> true
            | _ -> false
          in
          let labeled_arguments, unlabeled_arguments =
            arguments
            |> List.mapi ~f:add_original_positions
            |> List.partition_tf ~f:is_labeled
          in
          labeled_arguments @ unlabeled_arguments
        in
        consume base_signature_match ~arguments:ordered_arguments ~parameters
    | Undefined ->
        base_signature_match
  in
  let check_annotations ({ argument_mapping; _ } as signature_match) =
    let update ~key ~data ({ reasons = { arity; _ } as reasons; _; } as signature_match) =
      let (parameter_name, parameter_annotation) =
        match key with
        | Parameter.Named { Parameter.name; annotation; _ }
        | Parameter.Variable { Parameter.name; annotation; _ }
        | Parameter.Keywords { Parameter.name; annotation; _ } ->
            name, annotation
      in
      match key, data with
      | Parameter.Variable _, []
      | Parameter.Keywords _, [] ->
          (* Parameter was not matched, but empty is acceptable for variable arguments and
             keyword arguments. *)
          signature_match
      | _, [] ->
          (* Parameter was not matched *)
          let reasons = { reasons with arity = (MissingArgument parameter_name) :: arity } in
          { signature_match with reasons }
      | _, arguments ->
          let rec set_constraints_and_reasons
              ~resolution
              ~position
              ~argument:{ Argument.name; value = { Node.location; _ } }
              ~argument_annotation
              ({ constraints; reasons = { annotation; _ }; _; } as signature_match) =
            let reasons_with_mismatch =
              let mismatch =
                let location =
                  name
                  >>| Node.location
                  |> Option.value ~default:location
                in
                {
                  actual = argument_annotation;
                  expected = parameter_annotation;
                  name = Option.map name ~f:Node.value;
                  position;
                }
                |> Node.create ~location
                |> fun mismatch -> Mismatch mismatch
              in
              { reasons with annotation = mismatch :: annotation }
            in
            Resolution.solve_less_or_equal
              resolution
              ~constraints
              ~left:argument_annotation
              ~right:parameter_annotation
            >>| (fun updated_constraints ->
                { signature_match with constraints = updated_constraints }
              )
            |> Option.value
              ~default:{ signature_match with constraints; reasons = reasons_with_mismatch }
          in
          let rec check signature_match = function
            | [] ->
                signature_match
            | Default :: tail ->
                (* Parameter default value was used. Assume it is correct. *)
                check signature_match tail
            | Argument { argument; position } :: tail ->
                let set_constraints_and_reasons argument_annotation =
                  set_constraints_and_reasons
                    ~resolution
                    ~position
                    ~argument
                    ~argument_annotation
                    signature_match
                  |> (fun signature_match -> check signature_match tail)
                in
                let add_annotation_error
                    ({ reasons = { annotation; _ }; _ } as signature_match)
                    error =
                  {
                    signature_match with
                    reasons = { reasons with annotation = error :: annotation }
                  }
                in
                begin
                  match argument with
                  | {
                    Argument.value = { Node.value = Starred (Starred.Twice expression); location };
                    _;
                  } ->
                      let annotation = Resolution.resolve resolution expression in
                      let mapping = Type.parametric "typing.Mapping" [Type.string; Type.Top] in
                      if Resolution.less_or_equal resolution ~left:annotation ~right:mapping then
                        (* Try to extract second parameter. *)
                        Type.parameters annotation
                        |> (fun parameters -> List.nth parameters 1)
                        |> Option.value ~default:Type.Top
                        |> set_constraints_and_reasons
                      else
                        { expression; annotation }
                        |> Node.create ~location
                        |> (fun error -> InvalidKeywordArgument error)
                        |> add_annotation_error signature_match
                  | {
                    Argument.value = { Node.value = Starred (Starred.Once expression); location };
                    _;
                  } ->
                      let annotation = Resolution.resolve resolution expression in
                      let iterable =
                        (* Unannotated parameters are assigned a type of Bottom for inference,
                           in which case we should avoid joining with an iterable, as doing so
                           would suppress errors. *)
                        if Type.equal annotation Type.Bottom then
                          Type.Top
                        else
                          Resolution.join resolution annotation (Type.iterable Type.Bottom)
                      in
                      if Type.is_iterable iterable then
                        Type.single_parameter iterable
                        |> set_constraints_and_reasons
                      else
                        { expression; annotation }
                        |> Node.create ~location
                        |> (fun error -> InvalidVariableArgument error)
                        |> add_annotation_error signature_match
                  | { Argument.value = expression; _ } ->
                      let resolved = Resolution.resolve resolution expression in
                      let argument_annotation =
                        if Type.is_resolved parameter_annotation then
                          Resolution.resolve_mutable_literals
                            resolution
                            ~expression:(Some expression)
                            ~resolved
                            ~expected:parameter_annotation
                        else
                          resolved
                      in
                      if Type.is_meta parameter_annotation &&
                         Type.equal argument_annotation Type.Top then
                        Resolution.parse_annotation resolution expression
                        |> Type.meta
                        |> set_constraints_and_reasons
                      else
                        argument_annotation
                        |> set_constraints_and_reasons
                end
          in
          List.rev arguments
          |> check signature_match
    in
    let check_if_solution_exists
        ({ constraints; reasons = ({ annotation; _ } as reasons); _ } as signature_match) =
      if Option.is_some (Resolution.solve_constraints resolution constraints) then
        signature_match
      else
        (* All other cases should have been able to been blamed on a specefic argument, this is the
           only global failure. *)
        {
          signature_match with
          reasons = { reasons with annotation = MutuallyRecursiveTypeVariables :: annotation }
        }
    in
    let special_case_dictionary_constructor
        ({ argument_mapping; callable; constraints; _ } as signature_match) =
      let open Type.Record.Callable in
      let has_matched_keyword_parameter parameters =
        List.find parameters ~f:(function RecordParameter.Keywords _ -> true | _ -> false)
        >>= Type.Callable.Parameter.Map.find argument_mapping
        >>| List.is_empty
        >>| not
        |> Option.value ~default:false
      in
      match callable with
      | {
        kind = Named name;
        implementation = {
          parameters = Defined parameters;
          annotation = Type.Parametric { parameters = [ key_type; _ ]; _ }
        };
        _;
      } when Access.show name = "dict.__init__" && has_matched_keyword_parameter parameters ->
          Resolution.solve_less_or_equal
            resolution
            ~constraints
            ~left:Type.string
            ~right:key_type
          >>| (fun updated_constraints ->
              { signature_match with constraints = updated_constraints }
            )
          (* TODO(T41074174): Error here *)
          |> Option.value ~default:signature_match
      | _ ->
          signature_match
    in
    Map.fold ~init:signature_match ~f:update argument_mapping
    |> special_case_dictionary_constructor
    |> check_if_solution_exists
  in
  let calculate_rank
      ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
    let arity_rank = List.length arity in
    let (positions, annotation_rank) =
      let count_unique (positions, count) = function
        | Mismatch { Node.value = { position; _ }; _ } when not (Set.mem positions position) ->
            (Set.add positions position, count + 1)
        | Mismatch _ ->
            (positions, count)
        | _ ->
            (positions, count + 1)
      in
      List.fold ~init:(Int.Set.empty, 0) ~f:(count_unique) annotation
    in
    let position_rank =
      Int.Set.min_elt positions
      >>| Int.neg
      |> Option.value ~default:Int.min_value
    in
    {
      signature_match with
      ranks = { arity = arity_rank; annotation = annotation_rank; position = position_rank }
    }
  in
  let find_closest signature_matches =
    let get_arity_rank { ranks = { arity; _ }; _ } =
      arity
    in
    let get_annotation_rank { ranks = { annotation; _ }; _ } =
      annotation
    in
    let get_position_rank { ranks = { position; _ }; _ } =
      position
    in
    let rec get_best_rank ~best_matches ~best_rank ~getter = function
      | [] ->
          best_matches
      | head :: tail ->
          let rank = getter head in
          if rank < best_rank then
            get_best_rank ~best_matches:[head] ~best_rank:rank ~getter tail
          else if rank = best_rank then
            get_best_rank ~best_matches:(head :: best_matches) ~best_rank ~getter tail
          else
            get_best_rank ~best_matches ~best_rank ~getter tail
    in
    let determine_reason
        { callable; constraints; reasons = { arity; annotation; _ }; _ } =
      let solution =
        let solution =
          Resolution.solve_constraints resolution constraints
          |> Option.value ~default:Type.Map.empty
        in
        let to_bottom constraints variable =
          Map.update constraints variable ~f:(function | None -> Type.Bottom | Some value -> value)
        in
        Type.Callable callable
        |> Type.free_variables
        |> List.fold ~f:to_bottom ~init:solution
      in
      let callable =
        Type.Callable.map
          ~f:(Type.instantiate ~widen:false ~constraints:(Map.find solution))
          callable
        |> (function
            | Some callable -> callable
            | _ -> failwith "Instantiate did not return a callable")
      in
      match List.rev arity, List.rev annotation with
      | [], [] ->
          Found { callable; constraints = solution }
      | reason :: reasons, _
      | [], reason :: reasons ->
          let importance = function
            | InvalidKeywordArgument _ -> 0
            | InvalidVariableArgument _ -> 0
            | Mismatch { Node.value = { position; _ }; _ } -> 0 - position
            | MissingArgument _ -> 1
            | MutuallyRecursiveTypeVariables -> 1
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
    (* TODO(T41195241) always ignore implementation when has overloads.  Currently put
       implementation as last resort *)
    match get_match overloads with
    | Found signature_match -> Found signature_match
    | NotFound _ -> get_match [implementation]


let determine signature ~resolution ~annotation =
  match annotation, signature with
  | Type.Parametric { name; parameters } as annotation,
    Found { constraints; _ } ->
      Resolution.class_definition resolution annotation
      >>| Class.create
      >>| Class.generics ~resolution
      >>= (fun generics ->
          if List.length generics = List.length parameters then
            let instantiated =
              let uninstantiated =
                let uninstantiated generic parameter =
                  match parameter with
                  | Type.Bottom -> generic
                  | _ -> parameter
                in
                let parameters = List.map2_exn ~f:uninstantiated generics parameters in
                Type.Parametric { name; parameters }
              in
              let constraints annotation =
                Map.find constraints annotation
                >>| Type.weaken_literals
              in
              Type.instantiate ~constraints  uninstantiated
              |> Type.instantiate_free_variables ~replacement:Type.Bottom
            in
            Some instantiated
          else
            None)
  | _ ->
      None

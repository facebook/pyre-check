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


type reason =
  | Mismatch of mismatch Node.t
  | MissingArgument of Access.t
  | TooManyArguments of { expected: int; provided: int }
  | TypedDictionaryAccessWithNonLiteral of string list
  | TypedDictionaryMissingKey of { typed_dictionary_name: Identifier.t; missing_key: string }
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
}


type reasons = {
  arity: reason list;
  annotation: reason list;
}


type signature_match = {
  callable: Type.Callable.t;
  argument_mapping: (argument list) Type.Callable.Parameter.Map.t;
  constraints: Type.t Type.Map.t;
  ranks: ranks;
  reasons: reasons;
}


let select
    ~resolution
    ~arguments
    ~callable:({ Type.Callable.implementation; overloads; _ } as callable) =
  let open Type.Callable in
  let match_arity ({ parameters = all_parameters; _ } as implementation) =
    let base_signature_match =
      {
        callable = { callable with Type.Callable.implementation; overloads = [] };
        argument_mapping = Parameter.Map.empty;
        constraints = Type.Map.empty;
        ranks = {
          arity = 0;
          annotation = 0;
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
      let arity_mismatch reasons ~parameters ~arguments =
        match parameters with
        | Defined parameters ->
            let expected = List.length parameters in
            let provided = expected + List.length arguments in
            {
              reasons with
              arity = arity @ [TooManyArguments { expected; provided }]
            }
        | _ ->
            reasons
      in
      match arguments, parameters with
      | [], [] ->
          (* Both empty *)
          Some signature_match
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
      | _, [] ->
          (* Arguments; parameters empty *)
          let reasons = arity_mismatch reasons ~parameters:all_parameters ~arguments in
          Some { signature_match with reasons }
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
            | (Parameter.Named { Parameter.name = [Access.Identifier parameter_name]; _ } as head)
              :: tail
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
                argument_mapping, { reasons with arity = arity @ [UnexpectedKeyword name] }
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
        (Parameter.Variable _ as parameter) :: _ ->
          (* Unlabeled argument, starred parameter *)
          let signature_match =
            if Identifier.show_sanitized (Parameter.name parameter) = "*" then
              {
                signature_match with
                reasons = arity_mismatch reasons ~parameters:all_parameters ~arguments;
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
        Some base_signature_match
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
          let reasons =
            {
              reasons with
              arity =  arity @ [(MissingArgument parameter_name)]
            }
          in
          { signature_match with reasons }
      | _, arguments ->
          let rec set_constraints_and_reasons
              ~resolution
              ~position
              ~argument:({ Argument.name; value = { Node.location; _ } } as argument)
              ~argument_annotation
              ({ constraints; reasons = { annotation; _ }; _; } as signature_match) =
            let reasons =
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
            let less_or_equal =
              try
                (Type.equal argument_annotation Type.Top &&
                 Type.equal parameter_annotation Type.Object) ||
                Resolution.less_or_equal
                  resolution
                  ~left:argument_annotation
                  ~right:parameter_annotation
              with TypeOrder.Untracked _ ->
                false
            in
            match argument_annotation with
            | Type.Union elements when not less_or_equal ->
                let rec check_elements ~signature_match = function
                  | element :: elements ->
                      let access = Access.create "$argument" in
                      let annotation = Annotation.create element in
                      let resolution = Resolution.set_local resolution ~access ~annotation in
                      set_constraints_and_reasons
                        ~resolution
                        ~position
                        ~argument
                        ~argument_annotation:(Annotation.annotation annotation)
                        signature_match
                      |> (fun signature_match -> check_elements ~signature_match elements)
                  | _ ->
                      signature_match
                in
                check_elements ~signature_match elements
            | _ ->
                let parameters_to_infer = Type.variables parameter_annotation |> List.length in
                if parameters_to_infer > 0 then
                  let updated_constraints =
                    let rec update argument_annotation parameter_annotation constraints =
                      let update_constraints ~constraints ~variable ~resolved =
                        let resolved =
                          Map.find constraints variable
                          >>| (fun existing -> Resolution.join resolution existing resolved)
                          |> Option.value ~default:resolved
                        in
                        let in_constraints =
                          match variable with
                          | Type.Variable { constraints = Type.Explicit constraints; _ } ->
                              let in_constraint bound =
                                Resolution.less_or_equal resolution ~left:resolved ~right:bound
                              in
                              List.exists ~f:in_constraint constraints
                          | Type.Variable { constraints = Type.Bound bound; _ } ->
                              Resolution.less_or_equal resolution ~left:resolved ~right:bound
                          | _ ->
                              true
                        in
                        if in_constraints then
                          Some (Map.set ~key:variable ~data:resolved constraints)
                        else if less_or_equal then
                          Some constraints
                        else
                          None
                      in
                      match argument_annotation, parameter_annotation with
                      | Type.Bottom, _ ->
                          Some constraints
                      | _, (Type.Variable _ as variable) ->
                          update_constraints ~constraints ~variable ~resolved:argument_annotation
                      | _, Type.Parametric _ ->
                          let primitive, parameters = Type.split parameter_annotation in
                          Resolution.class_definition resolution primitive
                          >>| Class.create
                          >>= fun target ->
                          let primitive, _ = Type.split argument_annotation in
                          Resolution.class_definition resolution primitive
                          >>| Class.create
                          >>| Class.constraints
                            ~target
                            ~parameters
                            ~instantiated:argument_annotation
                            ~resolution
                          >>= fun inferred ->
                          if Map.length inferred < parameters_to_infer && not less_or_equal then
                            None
                          else
                            let update_constraints ~key ~data constraints =
                              constraints
                              >>= fun constraints ->
                              update_constraints ~constraints ~variable:key ~resolved:data
                            in
                            Map.fold ~init:(Some constraints) ~f:update_constraints inferred
                      | _, Type.Union annotations ->
                          List.fold
                            ~init:(Some constraints)
                            ~f:(fun constraints annotation ->
                                constraints >>= update argument_annotation annotation)
                            annotations
                      | Type.Callable {
                          Type.Callable.implementation = {
                            Type.Callable.annotation = argument_annotation;
                            parameters = argument_parameters;
                          };
                          _;
                        },
                        Type.Callable {
                          Type.Callable.implementation = {
                            Type.Callable.annotation = parameter_annotation;
                            parameters = parameters;
                          };
                          _;
                        } ->
                          let constraints =
                            update
                              argument_annotation
                              parameter_annotation
                              constraints
                          in
                          let argument_parameters =
                            match argument_parameters with
                            | Type.Callable.Defined parameters ->
                                List.map parameters ~f:Type.Callable.Parameter.annotation
                            | _ ->
                                []
                          in
                          let parameters =
                            match parameters with
                            | Type.Callable.Defined parameters ->
                                List.map parameters ~f:Type.Callable.Parameter.annotation
                            | _ ->
                                []
                          in
                          List.fold2
                            argument_parameters
                            parameters
                            ~init:constraints
                            ~f:(fun constraints argument parameter ->
                                constraints >>= update argument parameter)
                          |> (function
                              | List.Or_unequal_lengths.Ok constraints -> constraints
                              | _ -> None)
                      | _ ->
                          Some constraints
                    in
                    update argument_annotation parameter_annotation constraints
                  in
                  updated_constraints
                  >>| (fun updated_constraints ->
                      { signature_match with constraints = updated_constraints })
                  |> Option.value ~default:{ signature_match with constraints; reasons }
                else if less_or_equal then
                  signature_match
                else
                  { signature_match with reasons }
          in
          let rec check signature_match = function
            | [] ->
                signature_match
            | Default :: tail ->
                (* Parameter default value was used. Assume it is correct. *)
                check signature_match tail
            | Argument { argument; position } :: tail ->
                let get_argument_annotation = function
                  | {
                    Argument.value = { Node.value = Starred (Starred.Twice expression); _ };
                    _;
                  } ->
                      let mapping_value_parameter annotation =
                        let mapping = Type.parametric "typing.Mapping" [Type.string; Type.Object] in
                        if Resolution.less_or_equal resolution ~left:annotation ~right:mapping then
                          (* Try to extract second parameter. *)
                          Type.parameters annotation
                          |> (fun parameters -> List.nth parameters 1)
                          |> Option.value ~default:Type.Top
                        else
                          annotation
                      in
                      Resolution.resolve resolution expression
                      |> mapping_value_parameter
                  | { Argument.value = { Node.value = Starred (Starred.Once expression); _ }; _ } ->
                      let sequence_parameter annotation =
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
                        else
                          Type.Top
                      in
                      Resolution.resolve resolution expression
                      |> sequence_parameter
                  | { Argument.value = expression; _ } ->
                      let argument_annotation = Resolution.resolve resolution expression in
                      if Type.is_meta parameter_annotation &&
                         Type.equal argument_annotation Type.Top then
                        Resolution.parse_annotation resolution expression
                        |> Type.meta
                      else
                        argument_annotation
                in
                set_constraints_and_reasons
                  ~resolution
                  ~position
                  ~argument
                  ~argument_annotation:(get_argument_annotation argument)
                  signature_match
                |> (fun signature_match -> check signature_match tail)
          in
          let instantiate_unbound_constraints
              ({
                callable = { Type.Callable.implementation; _ };
                constraints;
                _;
              } as signature_match) =
            (* Map unresolved and unbound constraints to `Bottom`. *)
            let unbound_variables =
              let is_unbound = function
                | Type.Variable { constraints = Type.Explicit _; _ } -> false
                | _ -> true
              in
              Type.Callable {
                Type.Callable.kind = Anonymous;
                implementation;
                overloads = [];
                implicit = None;
              }
              |> Type.variables
              |> List.filter ~f:is_unbound
            in
            let remaining_to_bottom constraints variable =
              let update = function
                | None -> Some Type.Bottom
                | value -> value
              in
              Map.change constraints variable ~f:update
            in
            List.fold unbound_variables ~f:remaining_to_bottom ~init:constraints
            |> (fun constraints -> { signature_match with constraints })
          in
          check signature_match arguments
          |> instantiate_unbound_constraints
    in
    Map.fold ~init:signature_match ~f:update argument_mapping
  in
  let calculate_rank
      ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
    let arity_rank = List.length arity in
    let (_, annotation_rank) =
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
    { signature_match with ranks = { arity = arity_rank; annotation = annotation_rank }}
  in
  let find_closest signature_matches =
    let get_arity_rank { ranks = { arity; _ }; _ } =
      arity
    in
    let get_annotation_rank { ranks = { annotation; _ }; _ } =
      annotation
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
      match arity, annotation with
      | [], [] ->
          Type.Callable.map
            ~f:(Type.instantiate ~widen:false ~constraints:(Map.find constraints))
            callable
          |> (function
              | Some callable ->
                  Found { callable; constraints }
              | _ ->
                  failwith "Instantiate did not return a callable")
      | reason :: _, _
      | [], reason :: _ ->
          NotFound { callable; reason = Some reason }
    in
    signature_matches
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_arity_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_annotation_rank
    |> List.hd
    >>| determine_reason
    |> Option.value ~default:(NotFound { callable; reason = None })
  in
  let get_match signatures =
    signatures
    |> List.filter_map ~f:match_arity
    |> List.map ~f:check_annotations
    |> List.map ~f:calculate_rank
    |> find_closest
  in
  if List.is_empty overloads then
    get_match [implementation]
  else if Type.Callable.Overload.is_undefined implementation then
    get_match overloads
  else
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
              Type.instantiate ~constraints:(Map.find constraints) uninstantiated
              |> Type.instantiate
                ~constraints:(function | Type.Variable _ -> Some Type.Bottom | _ -> None)
            in
            Some instantiated
          else
            None)
  | _ ->
      None

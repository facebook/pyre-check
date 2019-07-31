(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement
module Error = AnalysisError

module type Context = sig
  val configuration : Configuration.Analysis.t

  val define : Define.t Node.t
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Resolution.t -> unit -> t

  val initial : resolution:Resolution.t -> t

  val initial_forward : resolution:Resolution.t -> t

  val initial_backward : forward:t -> t

  include Fixpoint.State with type t := t
end

module State (Context : Context) = struct
  module TypeCheckContext = struct
    let configuration = Context.configuration

    let define = Context.define

    let calls = Location.Reference.Table.create ()
  end

  module TypeCheckState = TypeCheck.State (TypeCheckContext)

  type t = {
    resolution: Resolution.t;
    errors: Error.t TypeCheck.ErrorMap.Map.t;
    bottom: bool;
  }

  let pp format { resolution; errors; bottom; _ } =
    let expected =
      Annotated.Callable.return_annotation
        ~define:(Node.value Context.define)
        ~resolution:(Resolution.global_resolution resolution)
    in
    let annotations =
      let annotation_to_string (name, annotation) =
        Format.asprintf "    %a -> %a" Reference.pp name Annotation.pp annotation
      in
      Resolution.annotations resolution
      |> Map.to_alist
      |> List.map ~f:annotation_to_string
      |> String.concat ~sep:"\n"
    in
    let errors =
      let error_to_string error =
        Format.asprintf
          "    %a -> %s"
          Location.Instantiated.pp
          (Error.location error)
          (Error.description error ~show_error_traces:true)
      in
      List.map (Map.data errors) ~f:error_to_string |> String.concat ~sep:"\n"
    in
    Format.fprintf
      format
      "  Bottom: %b\n  Expected return: %a\n  Types:\n%s\n  Errors:\n%s\n"
      bottom
      Type.pp
      expected
      annotations
      errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal
      Annotation.equal
      (Resolution.annotations left.resolution)
      (Resolution.annotations right.resolution)
    && left.bottom = right.bottom


  let create ?(bottom = false) ~resolution () =
    { resolution; errors = TypeCheck.ErrorMap.Map.empty; bottom }


  let errors { resolution; errors; bottom } =
    TypeCheckState.errors (TypeCheckState.create ~bottom ~errors ~resolution ())


  let coverage { resolution; _ } =
    Resolution.annotations resolution |> Map.data |> Coverage.aggregate


  let less_or_equal ~left:({ resolution; _ } as left) ~right =
    if left.bottom then
      true
    else if right.bottom then
      false
    else
      let entry_less_or_equal other less_or_equal ~key ~data sofar =
        sofar
        &&
        match Map.find other key with
        | Some other -> less_or_equal data other
        | _ -> false
      in
      let left_errors = Map.data left.errors |> Error.Set.of_list in
      let right_errors = Map.data right.errors |> Error.Set.of_list in
      Set.is_subset left_errors ~of_:right_errors
      && Map.fold
           ~init:true
           ~f:
             (entry_less_or_equal
                (Resolution.annotations right.resolution)
                (Refinement.less_or_equal ~resolution:(Resolution.global_resolution resolution)))
           (Resolution.annotations left.resolution)


  let join left right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let join_resolutions left_resolution right_resolution =
        let merge_annotations ~key:_ = function
          | `Both (left, right) ->
              Some
                (Refinement.join
                   ~resolution:(Resolution.global_resolution left_resolution)
                   left
                   right)
          | `Left _
          | `Right _ ->
              Some (Annotation.create Type.Top)
        in
        let annotations =
          Map.merge
            ~f:merge_annotations
            (Resolution.annotations left_resolution)
            (Resolution.annotations right_resolution)
        in
        Resolution.with_annotations left_resolution ~annotations
      in
      let combine_errors ~key:_ left_error right_error =
        Error.join
          ~resolution:(Resolution.global_resolution left.resolution)
          left_error
          right_error
      in
      {
        left with
        errors = Map.merge_skewed left.errors right.errors ~combine:combine_errors;
        resolution = join_resolutions left.resolution right.resolution;
      }


  let widening_threshold = 10

  let widen ~previous:({ resolution; _ } as previous) ~next ~iteration =
    if previous.bottom then
      next
    else if next.bottom then
      previous
    else
      let widen_annotations ~key annotation =
        match annotation with
        | `Both (previous, next) ->
            Some
              (Refinement.widen
                 ~resolution:(Resolution.global_resolution resolution)
                 ~widening_threshold
                 ~previous
                 ~next
                 ~iteration)
        | `Left previous
        | `Right previous
          when Reference.length key = 1 ->
            let widened =
              Refinement.widen
                ~resolution:(Resolution.global_resolution resolution)
                ~widening_threshold
                ~previous
                ~next:(Annotation.create Type.undeclared)
                ~iteration
            in
            Some widened
        | `Left previous
        | `Right previous ->
            Some previous
        | _ -> None
      in
      let annotations =
        Map.merge
          ~f:widen_annotations
          (Resolution.annotations previous.resolution)
          (Resolution.annotations next.resolution)
      in
      let combine_errors ~key:_ left_error right_error =
        if iteration > widening_threshold then
          { left_error with Error.kind = Error.Top }
        else
          Error.join
            ~resolution:(Resolution.global_resolution previous.resolution)
            left_error
            right_error
      in
      {
        previous with
        errors = Map.merge_skewed previous.errors next.errors ~combine:combine_errors;
        resolution = Resolution.with_annotations resolution ~annotations;
      }


  let rec initial ~resolution =
    let initial = TypeCheckState.initial ~resolution in
    { resolution; errors = TypeCheckState.error_map initial; bottom = false }


  let forward ?key:_ ({ resolution; bottom; errors; _ } as state) ~statement =
    if bottom then
      state
    else
      let initial_type_check_state = TypeCheckState.create ~errors ~resolution () in
      let final_type_check_state =
        TypeCheckState.forward_statement ~state:initial_type_check_state ~statement
      in
      {
        state with
        resolution = TypeCheckState.resolution final_type_check_state;
        errors = TypeCheckState.error_map final_type_check_state;
      }


  let return_reference = Reference.create "$return"

  let initial_forward ~resolution =
    let { Node.value = { Define.signature = { parameters; parent; _ }; _ } as define; _ } =
      Context.define
    in
    let state = initial ~resolution in
    let annotations =
      let reset_parameter
          index
          annotations
          { Node.value = { Parameter.name; value; annotation }; _ }
        =
        match index, parent with
        | 0, Some _ when Define.is_method define && not (Define.is_static_method define) ->
            annotations
        | _ -> (
          match annotation, value with
          | None, None ->
              let reference =
                name |> String.filter ~f:(fun character -> character <> '*') |> Reference.create
              in
              Map.set annotations ~key:reference ~data:(Annotation.create Type.Bottom)
          | _ -> annotations )
      in
      List.foldi ~init:(Resolution.annotations resolution) ~f:reset_parameter parameters
    in
    { state with resolution = Resolution.with_annotations resolution ~annotations }


  let initial_backward ~forward:{ resolution; errors; _ } =
    let expected_return =
      Annotated.Callable.return_annotation
        ~define:(Node.value Context.define)
        ~resolution:(Resolution.global_resolution resolution)
      |> Annotation.create
    in
    let backward_initial_state =
      let resolution =
        Resolution.with_annotations
          resolution
          ~annotations:(Reference.Map.of_alist_exn [return_reference, expected_return])
      in
      create ~resolution ()
    in
    let combine_annotations left right =
      let add_annotation ~key ~data map =
        if
          Type.is_unknown data.Annotation.annotation
          || Type.is_not_instantiated data.Annotation.annotation
          || Reference.equal key return_reference
        then
          map
        else
          Map.set ~key ~data map
      in
      Map.fold ~init:left ~f:add_annotation right
    in
    let resolution =
      let annotations =
        combine_annotations
          (Resolution.annotations backward_initial_state.resolution)
          (Resolution.annotations resolution)
      in
      Resolution.with_annotations resolution ~annotations
    in
    { backward_initial_state with resolution; errors }


  let update_only_existing_annotations ({ resolution; _ } as initial_state) new_state =
    let update ~key ~data map =
      if Map.mem map key then
        Map.set ~key ~data map
      else
        map
    in
    let resolution =
      let annotations =
        Map.fold
          ~init:(Resolution.annotations initial_state.resolution)
          ~f:update
          (Resolution.annotations new_state.resolution)
      in
      Resolution.with_annotations resolution ~annotations
    in
    { initial_state with resolution }


  let check_entry ({ resolution; errors; _ } as state) =
    let { Node.value = { Define.signature = { parameters; _ }; _ } as define; _ } =
      Context.define
    in
    let add_parameter_errors errors { Node.value = { Parameter.name; annotation; _ }; location } =
      let add_missing_parameter_error ~given_annotation =
        let reference = Reference.create name in
        Resolution.get_local resolution ~reference
        >>| (fun { Annotation.annotation; _ } ->
              let annotation = Type.remove_undeclared annotation in
              let error =
                Error.create
                  ~location
                  ~kind:
                    (Error.MissingParameterAnnotation
                       {
                         name = reference;
                         annotation = Some annotation;
                         given_annotation;
                         evidence_locations = [];
                         thrown_at_source = true;
                       })
                  ~define:Context.define
              in
              TypeCheck.ErrorMap.add ~errors error)
        |> Option.value ~default:errors
      in
      match annotation with
      | None -> add_missing_parameter_error ~given_annotation:None
      | Some annotation
        when Type.is_any
               (GlobalResolution.parse_annotation
                  (Resolution.global_resolution resolution)
                  annotation) ->
          add_missing_parameter_error ~given_annotation:(Some Type.Any)
      | _ -> errors
    in
    let parameters =
      if Define.is_method define && not (Define.is_static_method define) then
        List.tl parameters |> Option.value ~default:[]
      else
        parameters
    in
    { state with errors = List.fold parameters ~init:errors ~f:add_parameter_errors }


  let backward ?key:_ ({ resolution; _ } as state) ~statement =
    Type.Variable.Namespace.reset ();
    let resolve_assign annotation target_annotation =
      match annotation, target_annotation with
      | Type.Top, Type.Top -> None
      | Type.Top, target_annotation -> Some target_annotation
      | _ -> Some annotation
    in
    let forward_expression ~state:{ errors; resolution; _ } ~expression =
      let initial_type_check_state = TypeCheckState.create ~errors ~resolution () in
      let { TypeCheckState.resolved; _ } =
        TypeCheckState.forward_expression ~state:initial_type_check_state ~expression
      in
      resolved
    in
    let annotate_call_accesses statement resolution =
      let propagate resolution { Call.callee; arguments } =
        let resolved = forward_expression ~state ~expression:callee in
        match resolved with
        | Type.Callable
            {
              Type.Callable.implementation =
                { Type.Callable.parameters = Type.Callable.Defined parameters; _ };
              _;
            } ->
            let rec infer_annotations_list parameters arguments resolution =
              let rec infer_annotation resolution parameter_annotation argument =
                let state = { state with resolution } in
                match Node.value argument with
                | Name name when Expression.is_simple_name name ->
                    let reference = Expression.name_to_reference_exn name in
                    let resolved = forward_expression ~state ~expression:argument in
                    resolve_assign parameter_annotation resolved
                    >>| (fun refined ->
                          Resolution.set_local
                            resolution
                            ~reference
                            ~annotation:(Annotation.create refined))
                    |> Option.value ~default:resolution
                | Tuple arguments -> (
                  match parameter_annotation with
                  | Type.Tuple (Type.Bounded (Concrete parameter_annotations))
                    when List.length arguments = List.length parameter_annotations ->
                      List.fold2_exn
                        ~init:resolution
                        ~f:infer_annotation
                        parameter_annotations
                        arguments
                  | _ -> resolution )
                | _ -> resolution
              in
              match parameters, arguments with
              | ( Type.Callable.Parameter.Named { annotation; _ } :: parameters,
                  { Call.Argument.value = argument; _ } :: arguments ) ->
                  infer_annotation resolution annotation argument
                  |> infer_annotations_list parameters arguments
              | _ -> resolution
            in
            infer_annotations_list parameters arguments resolution
        | _ -> resolution
      in
      Visit.collect_calls statement
      |> List.map ~f:Node.value
      |> List.fold ~init:resolution ~f:propagate
    in
    let resolution =
      match Node.value statement with
      | Assign { Assign.target; value; _ } -> (
          (* Get the annotations of the targets and set the 'value' to be the meet *)
          let rec propagate_assign resolution target_annotation value =
            let state = { state with resolution } in
            match Node.value value with
            | Name (Name.Identifier identifier) ->
                let resolution =
                  let resolved = forward_expression ~state ~expression:value in
                  resolve_assign target_annotation resolved
                  >>| (fun refined ->
                        Resolution.set_local
                          resolution
                          ~reference:(Reference.create identifier)
                          ~annotation:(Annotation.create refined))
                  |> Option.value ~default:resolution
                in
                annotate_call_accesses statement resolution
            | Call _
            | Name _ ->
                annotate_call_accesses statement resolution
            (* Recursively break down tuples such as x : Tuple[int, string] = y, z *)
            | Tuple values ->
                let parameters =
                  match target_annotation with
                  | Type.Tuple (Type.Bounded (Concrete parameters)) -> parameters
                  | Type.Tuple (Type.Unbounded parameter) ->
                      List.map values ~f:(fun _ -> parameter)
                  | _ -> []
                in
                if List.length values = List.length parameters then
                  List.fold2_exn ~init:resolution ~f:propagate_assign parameters values
                else
                  resolution
            | _ -> resolution
          in
          match Node.value target, Node.value value with
          | Tuple targets, Tuple values when List.length targets = List.length values ->
              let target_annotations =
                let resolve expression =
                  let resolved = forward_expression ~state:{ state with resolution } ~expression in
                  resolved
                in
                List.map targets ~f:resolve
              in
              List.fold2_exn ~init:resolution ~f:propagate_assign target_annotations values
          | _, _ ->
              let resolved =
                forward_expression ~state:{ state with resolution } ~expression:target
              in
              propagate_assign resolution resolved value )
      | Return { Return.expression = Some { Node.value = Name name; _ }; _ }
        when Expression.is_simple_name name ->
          let return_annotation =
            Option.value_exn (Resolution.get_local resolution ~reference:return_reference)
            |> Annotation.annotation
          in
          Resolution.set_local
            resolution
            ~reference:(Expression.name_to_reference_exn name)
            ~annotation:(Annotation.create return_annotation)
      | _ -> annotate_call_accesses statement resolution
    in
    { state with resolution }
end

module SingleSourceResult = struct
  type t = {
    errors: Error.t list;
    coverage: Coverage.t;
  }

  let errors { errors; _ } = errors

  let coverage { coverage; _ } = coverage
end

let name = "Inference"

let run
    ~configuration
    ~global_resolution
    ~source:({ Source.relative; is_stub; metadata = { local_mode; _ }; _ } as source)
  =
  Log.debug "Checking %s..." relative;
  let resolution = TypeCheck.resolution global_resolution () in
  let dequalify_map = Preprocessing.dequalify_map source in
  let check
      ({ Node.location; value = { Define.signature = { name; _ }; _ } as define } as define_node)
    =
    let module State = State (struct
      let configuration = configuration

      let define = Node.create ~location define
    end)
    in
    let module Fixpoint = Fixpoint.Make (State) in
    Log.log ~section:`Check "Checking %a" Reference.pp name;
    let dump = Define.dump define in
    if dump then (
      Log.dump "Checking `%s`..." (Log.Color.yellow (Reference.show name));
      Log.dump "AST:\n%s" (Annotated.Define.create define |> Annotated.Define.show) );
    let print_state name state =
      if dump then
        Log.dump "%s state:\n%a" name State.pp state;
      state
    in
    try
      let cfg = Cfg.create define in
      let rec backward_fixpoint ~initial_forward ~initialize_backward =
        let rec fixpoint iteration ~initial_forward ~initialize_backward =
          let invariants =
            Fixpoint.forward ~cfg ~initial:initial_forward
            |> Fixpoint.exit
            >>| (fun forward_state -> initialize_backward ~forward:forward_state)
            |> Option.value ~default:initial_forward
            |> fun initial -> Fixpoint.backward ~cfg ~initial
          in
          let entry =
            invariants
            |> Fixpoint.entry
            >>| State.update_only_existing_annotations initial_forward
            >>| (fun post -> State.widen ~previous:initial_forward ~next:post ~iteration)
            |> Option.value ~default:initial_forward
          in
          if State.less_or_equal ~left:entry ~right:initial_forward then
            invariants
          else
            fixpoint (iteration + 1) ~initial_forward:entry ~initialize_backward
        in
        fixpoint 0 ~initial_forward ~initialize_backward
      in
      let exit =
        backward_fixpoint
          ~initial_forward:(State.initial_forward ~resolution)
          ~initialize_backward:State.initial_backward
        |> Fixpoint.entry
        >>| print_state "Entry"
        >>| State.check_entry
      in
      let errors =
        let errors = exit >>| State.errors |> Option.value ~default:[] in
        if configuration.debug then
          errors
        else
          let keep_error error =
            let mode = Ast.Source.mode ~configuration ~local_mode:(Some local_mode) in
            not (Error.suppress ~mode ~resolution error)
          in
          List.filter ~f:keep_error errors
      in
      let coverage = exit >>| State.coverage |> Option.value ~default:(Coverage.create ()) in
      { SingleSourceResult.errors; coverage }
    with
    | ClassHierarchy.Untracked annotation ->
        Statistics.event
          ~name:"undefined type"
          ~integers:[]
          ~normals:["handle", relative; "define", Reference.show name; "type", Type.show annotation]
          ();
        {
          SingleSourceResult.errors =
            ( if configuration.debug then
                [ Error.create
                    ~location
                    ~kind:(Error.AnalysisFailure annotation)
                    ~define:define_node ]
            else
              [] );
          coverage = Coverage.create ~crashes:1 ();
        }
  in
  let format_errors errors =
    let contains_unknown error =
      match Error.kind error with
      | MissingReturnAnnotation { annotation = Some annotation; _ }
      | MissingParameterAnnotation { annotation = Some annotation; _ }
      | MissingAttributeAnnotation { missing_annotation = { annotation = Some annotation; _ }; _ }
      | MissingGlobalAnnotation { annotation = Some annotation; _ } ->
          Type.contains_unknown annotation || Type.is_undeclared annotation
      | _ -> false
    in
    errors
    |> List.map
         ~f:(Error.dequalify dequalify_map ~resolution:(Resolution.global_resolution resolution))
    |> List.map ~f:(fun ({ Error.kind; _ } as error) ->
           { error with kind = Error.weaken_literals kind })
    |> List.sort ~compare:Error.compare
    |> List.filter ~f:(fun error -> not (contains_unknown error))
  in
  if is_stub then
    []
  else
    let results = source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check in
    let errors =
      List.map results ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution:(Resolution.global_resolution resolution)
      |> format_errors
    in
    let coverage =
      List.map results ~f:SingleSourceResult.coverage |> Coverage.aggregate_over_source ~source
    in
    Coverage.log coverage ~total_errors:(List.length errors) ~path:relative;
    errors

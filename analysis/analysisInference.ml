(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Configuration
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Error = AnalysisError
module Preprocessing = AnalysisPreprocessing
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder
module Coverage = AnalysisCoverage
module TypeCheck = AnalysisTypeCheck

open TypeCheck


module State = struct
  include TypeCheck.State


  let initial_backward
      ?(configuration = Configuration.create ())
      define
      ~forward:{ resolution; errors; _ } =
    let expected_return =
      Annotated.Callable.return_annotation ~define:(Node.value define) ~resolution
      |> Annotation.create
    in
    let backward_initial_state =
      let resolution =
        Resolution.with_annotations
          resolution
          ~annotations:(Access.Map.of_alist_exn [Preprocessing.return_access, expected_return])
      in
      create ~configuration ~resolution ~define ()
    in
    let combine_annotations left right =
      let add_annotation ~key ~data map =
        if Type.is_unknown data.Annotation.annotation ||
           Type.is_not_instantiated data.Annotation.annotation ||
           Access.equal key Preprocessing.return_access then
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


  let check_entry
      ({
        resolution;
        define = ({ Node.value = { Define.parameters; _ } as define; _ } as define_node);
        errors;
        _;
      } as state) =
    let add_parameter_errors
        errors
        { Node.value = { Parameter.name; annotation; _ }; location } =
      let access = [Access.Identifier name] in
      let add_missing_parameter_error ~due_to_any =
        Resolution.get_local resolution ~access
        >>| (fun { Annotation.annotation; _ } ->
            let name = Access.create_from_identifiers [name] in
            let error = {
              Error.location;
              kind = Error.MissingParameterAnnotation { Error.name; annotation; due_to_any };
              define = define_node;
            }
            in
            Map.set ~key:error.Error.location ~data:error errors)
        |> Option.value ~default:errors
      in
      match annotation with
      | None ->
          add_missing_parameter_error ~due_to_any:false
      | Some annotation
        when Type.equal (Resolution.parse_annotation resolution annotation) Type.Object ->
          add_missing_parameter_error ~due_to_any:true
      | _ -> errors
    in
    let parameters =
      if Define.is_method define &&
         not (Define.is_static_method define) then
        List.tl parameters
        |> Option.value ~default:[]
      else
        parameters
    in
    { state with errors = List.fold parameters ~init:errors ~f:add_parameter_errors}


  let backward
      ({ resolution; errors; _ } as state)
      ~statement =
    let resolve_assign annotation target_annotation =
      match annotation, target_annotation with
      | Type.Top, Type.Top -> None
      | Type.Top, target_annotation -> Some target_annotation
      | _ -> Some annotation
    in


    let annotate_call_accesses statement resolution =
      let propagate resolution access =
        let infer_annotations resolution arguments { Type.Callable.overloads; implicit; _ } =
          let rec infer_annotations_list parameters arguments resolution =
            let rec infer_annotation resolution parameter_annotation argument =
              match Node.value argument with
              | Access value ->
                  resolve_assign parameter_annotation (Annotated.resolve ~resolution argument)
                  >>| (fun refined ->
                      Resolution.set_local
                        resolution
                        ~access:value
                        ~annotation:(Annotation.create refined))
                  |> Option.value ~default:resolution
              | Tuple arguments ->
                  begin
                    match parameter_annotation with
                    | Type.Tuple (Type.Bounded parameter_annotations)
                      when List.length arguments = List.length parameter_annotations ->
                        List.fold2_exn
                          ~init:resolution
                          ~f:infer_annotation
                          parameter_annotations
                          arguments
                    | _ ->
                        resolution
                  end
              | _ ->
                  resolution
            in
            match parameters, arguments with
            | (Type.Callable.Parameter.Named { Type.Callable.Parameter.annotation; _ }) ::
              parameters,
              { Argument.value = argument; _ } :: arguments ->
                infer_annotation resolution annotation argument
                |> infer_annotations_list parameters arguments
            | _ ->
                resolution
          in
          match implicit, overloads with
          | Type.Callable.Instance,
            [ { Type.Callable.parameters = Type.Callable.Defined (_ :: parameters); _ } ] ->
              infer_annotations_list parameters arguments resolution
          | _, [ { Type.Callable.parameters = Type.Callable.Defined parameters; _ } ] ->
              infer_annotations_list parameters arguments resolution
          | _ ->
              resolution
        in
        let propagate_access type_accumulator ~resolution:_ ~resolved:_ ~element =
          let open Annotated.Access.Element in
          match element with
          | Signature {
              signature =
                Annotated.Signature.Found { Annotated.Signature.callable; _ };
              arguments;
            }
          | Signature {
              signature =
                Annotated.Signature.NotFound {
                  Annotated.Signature.callable;
                  reason = Some (Annotated.Signature.Mismatch _);
                  _;
                };
              arguments;
            } ->
              infer_annotations type_accumulator arguments callable
          | _ ->
              type_accumulator
        in
        Annotated.Access.fold
          ~resolution
          ~initial:resolution
          ~f:propagate_access
          (Annotated.Access.create access)
      in
      Visit.collect_accesses statement
      |> List.fold ~init:resolution ~f:propagate
    in

    let resolution =
      match Node.value statement with
      | Assign { Assign.target; value = Some value; _ } -> (
          (* Get the annotations of the targets and set the 'value' to be the meet *)
          let rec propagate_assign resolution target_annotation value =
            match Node.value value with
            | Access value_access ->
                let resolution =
                  match value_access with
                  | [Access.Identifier _] ->
                      resolve_assign target_annotation (Annotated.resolve ~resolution value)
                      >>| (fun refined ->
                          Resolution.set_local
                            resolution
                            ~access:value_access
                            ~annotation:(Annotation.create refined))
                      |> Option.value ~default:resolution
                  | _ ->
                      resolution
                in
                (* Optimistic assumption: after seeing x = y, we optimistically retain type of x *)
                annotate_call_accesses statement resolution

            (* Recursively break down tuples such as x : Tuple[int, string] = y, z *)
            | Tuple values  ->
                let parameters =
                  match target_annotation with
                  | Type.Tuple (Type.Bounded parameters) ->
                      parameters
                  | Type.Tuple (Type.Unbounded parameter) ->
                      List.map ~f:(fun _ -> parameter) values
                  | _ ->
                      []
                in
                if List.length values = List.length parameters then
                  List.fold2_exn
                    ~init:resolution
                    ~f:propagate_assign
                    parameters
                    values
                else
                  resolution
            | _ ->
                resolution
          in
          match (Node.value target), (Node.value value) with
          | Tuple targets, Tuple values
            when List.length targets = List.length values ->
              let target_annotations =
                List.map
                  ~f:(Annotated.resolve ~resolution)
                  targets
              in
              List.fold2_exn
                ~init:resolution
                ~f:propagate_assign
                target_annotations
                values
          | _, _ ->
              let target_annotation = Annotated.resolve ~resolution target in
              propagate_assign resolution target_annotation value)
      | _ ->
          annotate_call_accesses statement resolution
    in

    { state with errors; resolution }
end


module Fixpoint = AnalysisFixpoint.Make(State)


let rec backward_fixpoint cfg ~initial_forward ~initialize_backward =
  let rec fixpoint cfg iteration ~initial_forward ~initialize_backward =
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
      fixpoint cfg (iteration + 1) ~initial_forward:entry ~initialize_backward
  in
  fixpoint cfg 0 ~initial_forward ~initialize_backward


module SingleSourceResult = struct
  type t = {
    errors: Error.t list;
    coverage: Coverage.t;
  }


  let errors { errors; _ } =
    errors


  let coverage { coverage; _ } =
    coverage
end


let infer configuration environment ?mode_override ({ Source.path; _ } as source) =
  Log.debug "Checking %s..." path;
  let resolution = Environment.resolution environment () in

  let dequalify_map = Preprocessing.dequalify_map source in

  let check ({ Node.location; value = { Define.name; _ } as define } as define_node) =
    Log.log ~section:`Check "Checking %a" Access.pp name;
    let dump = Define.dump define in

    if dump then
      begin
        Log.dump
          "Checking `%s`..."
          (Log.Color.yellow (Access.show name));
        Log.dump "AST:\n%s" (Annotated.Define.create define |> Annotated.Define.show);
      end;

    let print_state name state =
      if dump then
        Log.dump "%s state:\n%a" name State.pp state;
      state
    in

    try
      let cfg = Cfg.create define in
      let initial_forward =
        State.initial
          ~configuration
          ~resolution
          { Node.location; value = define }
      in
      let exit =
        backward_fixpoint
          cfg
          ~initial_forward
          ~initialize_backward:(State.initial_backward ~configuration define_node)
        |> Fixpoint.entry
        >>| print_state "Entry"
        >>| State.check_entry
      in
      let (module Handler: Environment.Handler) = environment in
      let errors =
        let errors =
          exit
          >>| State.errors
          |> Option.value ~default:[]
        in
        if configuration.debug then
          errors
        else
          let keep_error ({ Error.location = { Location.path; _ }; _ } as error) =
            let mode =
              match mode_override with
              | Some mode ->
                  mode
              | None ->
                  Handler.mode path
                  |> Option.value ~default:Source.Default
            in
            not (Error.suppress ~mode error)
          in
          List.filter ~f:keep_error errors
      in
      let coverage =
        exit
        >>| State.coverage
        |> Option.value ~default:(Coverage.create ())
      in
      { SingleSourceResult.errors; coverage }
    with
    | TypeOrder.Untracked annotation ->
        Statistics.event
          ~name:"undefined type"
          ~configuration
          ~integers:[]
          ~normals:[
            "path", path;
            "define", Access.show name;
            "type", Type.show annotation;
          ]
          ();
        {
          SingleSourceResult.errors =
            if configuration.debug then
              [{
                Error.location;
                kind = Error.UndefinedType annotation;
                define = define_node;
              }]
            else
              [];
          coverage = Coverage.create ~crashes:1 ();
        }
  in

  let rec recursive_infer_source added_global_errors iterations =
    let add_errors_to_environment errors =
      let add_error (changed, globals_added_sofar) error =
        let module Handler = (val environment : Environment.Handler) in
        let add_missing_annotation_error ~access ~name ~location ~annotation =
          match Handler.globals name with
          | Some { Node.value; _ }
            when not (Type.is_unknown (Annotation.annotation value)) ->
              changed, globals_added_sofar
          | _ ->
              let global =
                Annotation.create_immutable ~global:true ~original:(Some Type.Top) annotation
                |> Node.create ~location
              in
              Handler.register_global ~path ~access ~global;
              true, error :: globals_added_sofar
        in
        match error with
        | {
          Error.kind = Error.MissingReturnAnnotation { Error.annotation; _ };
          define = ({ Node.value = define; location } as define_node);
          _;
        } ->
            let is_redundant
                ({ Node.value = { Define.return_annotation; _ }; _ } as define_node) =
              define_node.Node.location = location &&
              return_annotation = Some (Type.expression annotation)
            in
            begin
              match Handler.function_definitions define.Define.name with
              | Some define_node_list when List.exists ~f:is_redundant define_node_list ->
                  changed, globals_added_sofar
              | _ ->
                  let define =
                    {
                      define with
                      Define.return_annotation = Some (Type.expression annotation)
                    }
                  in
                  Handler.register_definition
                    ~path
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | {
          Error.kind = Error.MissingParameterAnnotation { Error.name; annotation; _ };
          define = ({ Node.value = define; location } as define_node);
          _;
        } ->
            let is_redundant
                ({ Node.value = { Define.parameters; _ }; _ } as define_node) =
              let find_parameter { Node.value = parameter; _ } =
                Access.equal (Access.create_from_identifiers [parameter.Parameter.name]) name &&
                parameter.Parameter.annotation = Some (Type.expression annotation)
              in
              define_node.Node.location = location &&
              List.exists ~f:find_parameter parameters
            in
            begin
              match Handler.function_definitions define.Define.name with
              | Some define_node_list when List.exists ~f:is_redundant define_node_list ->
                  changed, globals_added_sofar
              | _ ->
                  let update_parameter parameters name annotation =
                    let update updated ({ Node.value = parameter; _ } as parameter_node) =
                      if Access.create_from_identifiers [Parameter.name parameter_node] = name then
                        let updated_parameter =
                          {
                            parameter_node with
                            Node.value = ({
                                parameter with
                                Parameter.annotation = Some annotation
                              })
                          }
                        in
                        updated_parameter :: updated
                      else
                        parameter_node :: updated
                    in
                    List.fold ~init:[] ~f:update parameters
                  in
                  let define =
                    let annotation = Type.expression annotation in
                    {
                      define with
                      Define.parameters =
                        update_parameter define.Define.parameters name annotation
                    }
                  in
                  Handler.register_definition
                    ~path
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | {
          Error.kind = Error.MissingAttributeAnnotation {
              Error.parent;
              missing_annotation = { Error.name; annotation; _ };
            };
          location;
          _;
        } ->
            add_missing_annotation_error
              ~access:((Annotated.Class.name parent) @ name)
              ~name
              ~location
              ~annotation
        | {
          Error.kind = Error.MissingGlobalAnnotation { Error.name; annotation; _ };
          location;
          _;
        } ->
            add_missing_annotation_error ~access:name ~name ~location ~annotation
        | _ ->
            changed, globals_added_sofar
      in
      List.fold ~init:(false, []) ~f:add_error errors
    in
    let errors =
      Preprocessing.defines source
      |> List.map ~f:check
      |> List.map ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution
    in
    let (changed, newly_added_global_errors) = add_errors_to_environment errors in
    if changed && iterations <= State.widening_threshold then
      recursive_infer_source (newly_added_global_errors @ added_global_errors) (iterations + 1)
    else
      errors @ added_global_errors
      |> List.map ~f:(Error.dequalify dequalify_map environment)
      |> List.sort ~compare:Error.compare
      |> fun errors -> {
        TypeCheck.Result.errors;
        coverage = Coverage.create ();
      }
  in

  if configuration.recursive_infer then
    recursive_infer_source [] 0
  else
    let results = Preprocessing.defines source |> List.map ~f:check in

    let errors =
      List.map ~f:SingleSourceResult.errors results
      |> List.concat
      |> Error.join_at_source ~resolution
      |> List.map ~f:(Error.dequalify dequalify_map environment)
      |> List.sort ~compare:Error.compare
    in

    let coverage =
      List.map ~f:SingleSourceResult.coverage results
      |> Coverage.aggregate_over_source ~source
    in
    Coverage.log coverage ~configuration ~total_errors:(List.length errors) ~path;

    {
      TypeCheck.Result.errors;
      coverage;
    }

(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Error = AnalysisError


module State = struct
  open TypeCheck
  include TypeCheck.State


  let return_access = Access.create "$return"


  let initial_backward
      ?(configuration = Configuration.Analysis.create ())
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
          ~annotations:(Access.Map.of_alist_exn [return_access, expected_return])
      in
      create ~configuration ~resolution ~define ()
    in
    let combine_annotations left right =
      let add_annotation ~key ~data map =
        if Type.is_unknown data.Annotation.annotation ||
           Type.is_not_instantiated data.Annotation.annotation ||
           Access.equal key return_access then
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
            let error =
              Error.create
                ~location
                ~kind:(Error.MissingParameterAnnotation { name; annotation; due_to_any })
                ~define:define_node
            in
            Map.set errors ~key:location ~data:error)
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
      ?key:_
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
        let infer_annotations resolution arguments { Type.Callable.implementation; _ } =
          let rec infer_annotations_list parameters arguments resolution =
            let rec infer_annotation resolution parameter_annotation argument =
              let state = { state with resolution } in
              match Node.value argument with
              | Access value ->
                  let { resolved; _ } =
                    TypeCheck.State.forward_expression ~state ~expression:argument
                  in
                  resolve_assign parameter_annotation resolved
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
          match implementation with
          | { Type.Callable.parameters = Type.Callable.Defined parameters; _ } ->
              infer_annotations_list parameters arguments resolution
          | _ ->
              resolution
        in
        let propagate_access type_accumulator ~resolution:_ ~resolved:_ ~element ~lead:_ =
          let open Annotated.Access in
          match element with
          | Signature {
              signature = Annotated.Signature.Found { callable; _ };
              arguments;
              _;
            }
          | Signature {
              signature =
                Annotated.Signature.NotFound {
                  callable;
                  reason = Some (Annotated.Signature.Mismatch _);
                  _;
                };
              arguments;
              _;
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
      | Assign { Assign.target; value; _ } -> (
          (* Get the annotations of the targets and set the 'value' to be the meet *)
          let rec propagate_assign resolution target_annotation value =
            let state = { state with resolution } in
            match Node.value value with
            | Access value_access ->
                let resolution =
                  match value_access with
                  | [Access.Identifier _] ->
                      let { resolved; _ } =
                        TypeCheck.State.forward_expression ~state ~expression:value
                      in
                      resolve_assign target_annotation resolved
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
                      List.map values ~f:(fun _ -> parameter)
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
                let resolve expression =
                  let { resolved; _ } =
                    TypeCheck.State.forward_expression ~state:{ state with resolution } ~expression
                  in
                  resolved
                in
                List.map targets ~f:resolve
              in
              List.fold2_exn
                ~init:resolution
                ~f:propagate_assign
                target_annotations
                values
          | _, _ ->
              let { resolved; _ } =
                TypeCheck.State.forward_expression
                  ~state:{ state with resolution }
                  ~expression:target
              in
              propagate_assign resolution resolved value)

      | Return { Return.expression = Some { Node.value = Access access; _ }; _ } ->
          let return_annotation =
            Option.value_exn (Resolution.get_local resolution ~access:return_access)
            |> Annotation.annotation
          in
          Resolution.set_local
            resolution
            ~access:access
            ~annotation:(Annotation.create return_annotation)

      | _ ->
          annotate_call_accesses statement resolution
    in

    { state with errors; resolution }
end


module Fixpoint = Fixpoint.Make(State)


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


let infer
    ~configuration
    ~environment
    ~source:({ Source.handle; _ } as source) =
  Log.debug "Checking %s..." (File.Handle.show handle);
  let resolution = TypeCheck.resolution environment () in

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
          let keep_error error =
            let mode =
              Handler.local_mode (Error.path error |> File.Handle.create)
              |> (fun local_mode -> Ast.Source.mode ~configuration ~local_mode)
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
          ~integers:[]
          ~normals:[
            "handle", (File.Handle.show handle);
            "define", Access.show name;
            "type", Type.show annotation;
          ]
          ();
        {
          SingleSourceResult.errors =
            if configuration.debug then
              [
                Error.create
                  ~location
                  ~kind:(Error.AnalysisFailure annotation)
                  ~define:define_node
              ]
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
              Handler.register_global ~handle ~access ~global;
              true, error :: globals_added_sofar
        in
        match error with
        | {
          Error.kind = Error.MissingReturnAnnotation { annotation; _ };
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
                    ~handle
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | {
          Error.kind = Error.MissingParameterAnnotation { name; annotation; _ };
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
                    ~handle
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | ({
            Error.kind = Error.MissingAttributeAnnotation {
                parent;
                missing_annotation = { Error.name; annotation = Some annotation; _ };
              };
            _;
          } as error) ->
            add_missing_annotation_error
              ~access:((Type.show parent |> Access.create) @ name)
              ~name
              ~location:(Error.location error |> Location.reference)
              ~annotation
        | ({
            Error.kind = Error.MissingGlobalAnnotation {
                Error.name;
                annotation = Some annotation;
                _;
              };
            _;
          } as error) ->
            add_missing_annotation_error
              ~access:name
              ~name
              ~location:(Error.location error |> Location.reference)
              ~annotation
        | _ ->
            changed, globals_added_sofar
      in
      List.fold ~init:(false, []) ~f:add_error errors
    in
    let errors =
      (* TODO(T31738631): remove extract_into_toplevel *)
      Preprocessing.defines ~extract_into_toplevel:true source
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
      |> List.map ~f:(Error.dequalify dequalify_map ~resolution)
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
      List.map results ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution
      |> List.map ~f:(Error.dequalify dequalify_map ~resolution)
      |> List.sort ~compare:Error.compare
    in

    let coverage =
      List.map results ~f:SingleSourceResult.coverage
      |> Coverage.aggregate_over_source ~source
    in
    Coverage.log coverage ~total_errors:(List.length errors) ~path:(File.Handle.show handle);

    {
      TypeCheck.Result.errors;
      coverage;
    }

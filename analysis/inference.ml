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

module State = struct
  type t = {
    configuration: Configuration.Analysis.t;
    resolution: Resolution.t;
    errors: Error.t TypeCheck.ErrorKey.Map.t;
    define: Define.t Node.t;
    bottom: bool
  }

  let pp format
         { resolution; errors; define = { Node.value = define; _ }; bottom; _ } =
    let expected = Annotated.Callable.return_annotation ~define ~resolution in
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


  let create
      ?(configuration = Configuration.Analysis.create ())
      ?(bottom = false)
      ~resolution
      ~define
      ()
    =
    { configuration; resolution; errors = TypeCheck.ErrorKey.Map.empty; define; bottom }


  let errors
      { configuration;
        resolution;
        errors;
        define =
          { Node.value = { Define.signature = { name; _ }; _ } as define; location } as define_node
      ; _
      }
    =
    let class_initialization_errors errors =
      (* Ensure non-nullable typed attributes are instantiated in init. This must happen after
         typechecking is finished to access the annotations added to resolution. *)
      let check_attributes_initialized define =
        let open Annotated in
        Define.parent_definition ~resolution (Define.create define)
        >>| (fun definition ->
              let propagate_initialization_errors errors attribute =
                let expected = Annotation.annotation (Attribute.annotation attribute) in
                let location = Attribute.location attribute in
                match Attribute.name attribute with
                | name when not (Type.is_top expected || Attribute.initialized attribute) ->
                    let reference =
                      Reference.create_from_list [Statement.Define.self_identifier define; name]
                    in
                    if
                      Map.mem (Resolution.annotations resolution) reference
                      && not (Statement.Define.is_class_toplevel define)
                    then
                      errors
                    else
                      let error =
                        Error.create
                          ~location
                          ~kind:
                            (Error.UninitializedAttribute
                               { name;
                                 parent = Annotated.Class.annotation definition;
                                 mismatch =
                                   { Error.expected;
                                     actual = Type.optional expected;
                                     actual_expressions = [];
                                     due_to_invariance = false
                                   }
                               })
                          ~define:define_node
                      in
                      error :: errors
                | name ->
                    let actual = expected in
                    let check_override
                        ( { Node.value = { Attribute.annotation; name; final; _ }; _ } as
                        overridden_attribute )
                      =
                      let expected = Annotation.annotation annotation in
                      if
                        ( Resolution.less_or_equal resolution ~left:actual ~right:expected
                        || Type.is_top actual )
                        && not final
                      then
                        errors
                      else
                        let kind =
                          if final then
                            Error.InvalidAssignment (Final (Reference.create name))
                          else
                            Error.InconsistentOverride
                              { overridden_method = name;
                                parent =
                                  Attribute.parent overridden_attribute
                                  |> Type.show
                                  |> Reference.create;
                                override_kind = Attribute;
                                override =
                                  Error.WeakenedPostcondition
                                    (Error.create_mismatch
                                       ~resolution
                                       ~actual
                                       ~actual_expression:None
                                       ~expected
                                       ~covariant:false)
                              }
                        in
                        Error.create ~location ~kind ~define:define_node :: errors
                    in
                    Class.overrides ~resolution ~name definition
                    >>| check_override
                    |> Option.value ~default:errors
              in
              Class.attribute_fold
                ~include_generated_attributes:false
                ~initial:errors
                ~resolution
                ~f:propagate_initialization_errors
                definition)
        |> Option.value ~default:errors
      in
      let check_bases define =
        let open Annotated in
        let is_final { Call.Argument.name; value } =
          let add_error { Resolution.is_final; _ } =
            if is_final then
              let error =
                Error.create
                  ~location
                  ~kind:(Error.InvalidInheritance (Class (Expression.show value)))
                  ~define:define_node
              in
              error :: errors
            else
              errors
          in
          match name, value with
          | None, { Node.value = Name name; _ } when Expression.is_simple_name name ->
              let reference = Reference.from_name_exn name in
              Resolution.class_metadata resolution (Type.Primitive (Reference.show reference))
              >>| add_error
              |> Option.value ~default:errors
          | _ -> errors
        in
        Define.parent_definition ~resolution (Define.create define)
        >>| Class.bases
        >>| List.map ~f:is_final
        >>| List.concat
        |> Option.value ~default:errors
      in
      if Define.is_constructor define && not (Define.is_stub define) then
        let base_errors = check_bases define in
        List.append base_errors (check_attributes_initialized define)
      else if Define.is_class_toplevel define then
        let no_explicit_class_constructor =
          let name = Reference.prefix name >>| Reference.show |> Option.value ~default:"" in
          Resolution.class_definition resolution (Type.Primitive name)
          >>| Annotated.Class.create
          >>| Annotated.Class.constructors ~resolution
          >>| List.is_empty
          |> Option.value ~default:false
        in
        if no_explicit_class_constructor then
          let base_errors = check_bases define in
          List.append base_errors (check_attributes_initialized define)
        else
          errors
      else
        errors
    in
    let overload_errors errors =
      let annotation = Resolution.get_local resolution ~reference:name in
      let check_implementation errors =
        match annotation with
        | Some { annotation = Type.Callable { implementation; _ }; _ }
          when Statement.Define.is_overloaded_method define
               && Type.Callable.Overload.is_undefined implementation ->
            let error =
              Error.create
                ~location
                ~kind:(Error.MissingOverloadImplementation name)
                ~define:define_node
            in
            error :: errors
        | _ -> errors
      in
      let check_compatible_return_types errors =
        match annotation with
        | Some
            { annotation =
                Type.Callable
                  { overloads; implementation = { annotation = implementation_annotation; _ }; _ }
            ; _
            }
          when not (Statement.Define.is_overloaded_method define) ->
            List.fold
              ~init:errors
              ~f:(fun sofar { annotation; _ } ->
                if
                  Resolution.is_consistent_with
                    resolution
                    annotation
                    implementation_annotation
                    ~expression:None
                then
                  sofar
                else
                  let error =
                    Error.create
                      ~location
                      ~kind:
                        (Error.IncompatibleOverload
                           { implementation_annotation; overload_annotation = annotation; name })
                      ~define:define_node
                  in
                  error :: sofar)
              overloads
        | _ -> errors
      in
      check_implementation errors |> check_compatible_return_types
    in
    Map.data errors
    |> Error.join_at_define ~resolution
    |> Error.deduplicate
    |> class_initialization_errors
    |> overload_errors
    |> Error.filter ~configuration ~resolution


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
                (Refinement.less_or_equal ~resolution))
           (Resolution.annotations left.resolution)


  let join left right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let join_resolutions left_resolution right_resolution =
        let merge_annotations ~key:_ = function
          | `Both (left, right) -> Some (Refinement.join ~resolution:left_resolution left right)
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
        Error.join ~resolution:left.resolution left_error right_error
      in
      { left with
        errors = Map.merge_skewed left.errors right.errors ~combine:combine_errors;
        resolution = join_resolutions left.resolution right.resolution
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
            Some (Refinement.widen ~resolution ~widening_threshold ~previous ~next ~iteration)
        | `Left previous
        | `Right previous
          when Reference.length key = 1 ->
            let widened =
              Refinement.widen
                ~resolution
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
          Error.join ~resolution:previous.resolution left_error right_error
      in
      { previous with
        errors = Map.merge_skewed previous.errors next.errors ~combine:combine_errors;
        resolution = Resolution.with_annotations resolution ~annotations
      }


  let rec initial
      ?(configuration = Configuration.Analysis.create ())
      ~resolution
      ({ Node.location; value = define } as define_node)
    =
    let module Context = struct
      let configuration = configuration

      let define = { Node.location; value = define }
    end
    in
    let module TypeCheckState = TypeCheck.State (Context) in
    let initial = TypeCheckState.initial ~resolution () in
    { configuration;
      resolution;
      errors = TypeCheckState.error_map initial;
      define = define_node;
      bottom = false
    }


  let forward ?key:_ ({ define; resolution; configuration; bottom; errors; _ } as state) ~statement
    =
    if bottom then
      state
    else
      let module Context = struct
        let configuration = configuration

        let define = define
      end
      in
      let module TypeCheckState = TypeCheck.State (Context) in
      let initial_type_check_state = TypeCheckState.create ~errors ~resolution () in
      let final_type_check_state =
        TypeCheckState.forward_statement ~state:initial_type_check_state ~statement
      in
      { state with
        resolution = TypeCheckState.resolution final_type_check_state;
        errors = TypeCheckState.error_map final_type_check_state
      }


  let return_reference = Reference.create "$return"

  let initial_forward
      ~configuration
      ~resolution
      ( { Node.value = { Define.signature = { parameters; parent; _ }; _ } as define; _ } as
      define_node )
    =
    let state = initial ~configuration ~resolution define_node in
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


  let initial_backward
      ?(configuration = Configuration.Analysis.create ())
      define
      ~forward:{ resolution; errors; _ }
    =
    let expected_return =
      Annotated.Callable.return_annotation ~define:(Node.value define) ~resolution
      |> Annotation.create
    in
    let backward_initial_state =
      let resolution =
        Resolution.with_annotations
          resolution
          ~annotations:(Reference.Map.of_alist_exn [return_reference, expected_return])
      in
      create ~configuration ~resolution ~define ()
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


  let check_entry
      ( { resolution;
          define =
            { Node.value = { Define.signature = { parameters; _ }; _ } as define; _ } as
            define_node;
          errors
        ; _
        } as state )
    =
    let add_parameter_errors errors
                             { Node.value = { Parameter.name; annotation; _ }; location } =
      let add_missing_parameter_error ~given_annotation =
        let reference = Reference.create name in
        Resolution.get_local resolution ~reference
        >>| (fun { Annotation.annotation; _ } ->
              let error =
                Error.create
                  ~location
                  ~kind:
                    (Error.MissingParameterAnnotation
                       { name = reference;
                         annotation = Some annotation;
                         given_annotation;
                         evidence_locations = [];
                         thrown_at_source = true
                       })
                  ~define:define_node
              in
              TypeCheck.ErrorKey.add_error ~errors error)
        |> Option.value ~default:errors
      in
      match annotation with
      | None -> add_missing_parameter_error ~given_annotation:None
      | Some annotation when Type.is_any (Resolution.parse_annotation resolution annotation) ->
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


  let backward ?key:_
               ({ resolution; configuration; define; _ } as state)
               ~statement =
    Type.Variable.Namespace.reset ();
    let resolve_assign annotation target_annotation =
      match annotation, target_annotation with
      | Type.Top, Type.Top -> None
      | Type.Top, target_annotation -> Some target_annotation
      | _ -> Some annotation
    in
    let forward_expression ~state:{ errors; resolution; _ } ~expression =
      let module Context = struct
        let configuration = configuration

        let define = define
      end
      in
      let module TypeCheckState = TypeCheck.State (Context) in
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
            { Type.Callable.implementation =
                { Type.Callable.parameters = Type.Callable.Defined parameters; _ }
            ; _
            } ->
            let rec infer_annotations_list parameters arguments resolution =
              let rec infer_annotation resolution parameter_annotation argument =
                let state = { state with resolution } in
                match Node.value argument with
                | Name name when Expression.is_simple_name name ->
                    let reference = Reference.from_name_exn name in
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
            ~reference:(Reference.from_name_exn name)
            ~annotation:(Annotation.create return_annotation)
      | _ -> annotate_call_accesses statement resolution
    in
    { state with resolution }
end

module Fixpoint = Fixpoint.Make (State)

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
    coverage: Coverage.t
  }

  let errors { errors; _ } = errors

  let coverage { coverage; _ } = coverage
end

let name = "Inference"

let run ~configuration
        ~environment
        ~source:({ Source.handle; _ } as source) =
  Log.debug "Checking %s..." (File.Handle.show handle);
  let resolution = TypeCheck.resolution environment () in
  let dequalify_map = Preprocessing.dequalify_map source in
  let check
      ({ Node.location; value = { Define.signature = { name; _ }; _ } as define } as define_node)
    =
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
      let exit =
        backward_fixpoint
          cfg
          ~initial_forward:(State.initial_forward ~configuration ~resolution define_node)
          ~initialize_backward:(State.initial_backward ~configuration define_node)
        |> Fixpoint.entry
        >>| print_state "Entry"
        >>| State.check_entry
      in
      let (module Handler : Environment.Handler) = environment in
      let errors =
        let errors = exit >>| State.errors |> Option.value ~default:[] in
        if configuration.debug then
          errors
        else
          let keep_error error =
            let mode =
              Handler.local_mode (Error.path error |> File.Handle.create)
              |> fun local_mode -> Ast.Source.mode ~configuration ~local_mode
            in
            not (Error.suppress ~mode ~resolution error)
          in
          List.filter ~f:keep_error errors
      in
      let coverage = exit >>| State.coverage |> Option.value ~default:(Coverage.create ()) in
      { SingleSourceResult.errors; coverage }
    with
    | TypeOrder.Untracked annotation ->
        Statistics.event
          ~name:"undefined type"
          ~integers:[]
          ~normals:
            [ "handle", File.Handle.show handle;
              "define", Reference.show name;
              "type", Type.show annotation ]
          ();
        { SingleSourceResult.errors =
            ( if configuration.debug then
                [ Error.create
                    ~location
                    ~kind:(Error.AnalysisFailure annotation)
                    ~define:define_node ]
            else
              [] );
          coverage = Coverage.create ~crashes:1 ()
        }
  in
  let rec recursive_infer_source added_global_errors iterations =
    let add_errors_to_environment errors =
      let add_error (changed, globals_added_sofar) error =
        let module Handler = (val environment : Environment.Handler) in
        let add_missing_annotation_error ~reference ~name ~location ~annotation =
          match Handler.globals name with
          | Some { Node.value; _ } when not (Type.is_unknown (Annotation.annotation value)) ->
              changed, globals_added_sofar
          | _ ->
              let global =
                Annotation.create_immutable ~global:true ~original:(Some Type.Top) annotation
                |> Node.create ~location
              in
              Handler.register_global ~handle ~reference ~global;
              true, error :: globals_added_sofar
        in
        (* TODO(T31680236): use inferred annotations in global fixpoint. *)
        match error with
        | { Error.kind =
              Error.MissingAttributeAnnotation
                { parent; missing_annotation = { Error.name; annotation = Some annotation; _ } }
          ; _
          } as error ->
            add_missing_annotation_error
              ~reference:(Reference.combine (Type.show parent |> Reference.create) name)
              ~name
              ~location:(Error.location error |> Location.reference)
              ~annotation
        | { Error.kind =
              Error.MissingGlobalAnnotation { Error.name; annotation = Some annotation; _ }
          ; _
          } as error ->
            add_missing_annotation_error
              ~reference:name
              ~name
              ~location:(Error.location error |> Location.reference)
              ~annotation
        | _ -> changed, globals_added_sofar
      in
      List.fold ~init:(false, []) ~f:add_error errors
    in
    let errors =
      (* TODO(T31738631): remove include_toplevels *)
      Preprocessing.defines ~include_toplevels:true source
      |> List.map ~f:check
      |> List.map ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution
    in
    let changed, newly_added_global_errors = add_errors_to_environment errors in
    if changed && iterations <= State.widening_threshold then
      recursive_infer_source (newly_added_global_errors @ added_global_errors) (iterations + 1)
    else
      errors @ added_global_errors
      |> List.map ~f:(Error.dequalify dequalify_map ~resolution)
      |> List.sort ~compare:Error.compare
  in
  if configuration.recursive_infer then
    recursive_infer_source [] 0
  else
    let results = source |> Preprocessing.defines |> List.map ~f:check in
    let errors =
      List.map results ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution
      |> List.map ~f:(Error.dequalify dequalify_map ~resolution)
      |> List.map ~f:(fun ({ Error.kind; _ } as error) ->
             { error with kind = Error.weaken_literals kind })
      |> List.sort ~compare:Error.compare
    in
    let coverage =
      List.map results ~f:SingleSourceResult.coverage |> Coverage.aggregate_over_source ~source
    in
    Coverage.log coverage ~total_errors:(List.length errors) ~path:(File.Handle.show handle);
    errors

(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The RequestHandler module provides event handling to power the code navigation backend server.
   Requests coming from either the LSP or the file watcher are dispatched to the build system and/or
   analyzer from this module *)

open Base
open Analysis
open Pyre

module ServerInternal = struct
  type t = {
    properties: Server.ServerProperties.t;
    (* Subscriptions is stored outside of [state]. The reason is that [state] needs to be
       Lwt-locked, to prevent it from getting concurrently modified during buck rebuild.
       Subscriptions need no such protection. *)
    subscriptions: Subscriptions.t;
    state: State.t Server.ExclusiveLock.t;
  }
end

(* Get overlay ID without checking if the file is added to the working set. *)
let get_overlay_id_unsafe ~path ~client_id client_states =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  match State.Client.WorkingSet.lookup client_states ~client_id ~source_path with
  | `ClientNotRegistered -> Result.Error (Response.ErrorKind.ClientNotRegistered { client_id })
  | _ -> Result.Ok { State.Client.OverlayId.client_id; source_path }


let get_overlay_id ~path ~client_id client_states =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  match State.Client.WorkingSet.lookup client_states ~client_id ~source_path with
  | `ClientNotRegistered -> Result.Error (Response.ErrorKind.ClientNotRegistered { client_id })
  | `FileNotAdded -> Result.Error (Response.ErrorKind.FileNotOpened { path })
  | `Ok overlay_id -> Result.Ok overlay_id


let get_or_create_overlay ~environment overlay_id =
  let overlay_id = State.Client.OverlayId.to_string overlay_id in
  OverlaidEnvironment.get_or_create_overlay environment overlay_id
  |> ErrorsEnvironment.Overlay.read_only


let get_overlay ~environment overlay_id =
  let overlay_id = State.Client.OverlayId.to_string overlay_id in
  OverlaidEnvironment.overlay environment overlay_id
  |> Option.value_exn
       ~message:(Stdlib.Format.sprintf "Unexpected overlay lookup failure with id `%s`" overlay_id)


let get_modules ~source_code_api ~build_system path =
  let modules =
    let source_path = PyrePath.create_absolute path |> SourcePath.create in
    match
      Analysis.SourcePaths.qualifiers_of_source_path
        source_path
        ~source_code_api
        ~lookup_artifact:(BuildSystem.lookup_artifact build_system)
    with
    (* In case there's no build system artifacts for this source, lookup the module as if it's built
       by a no-op build system (using normal source path mapping) *)
    | [] ->
        Analysis.SourcePaths.qualifiers_of_source_path
          source_path
          ~source_code_api
          ~lookup_artifact:BuildSystem.default_lookup_artifact
    | _ as modules -> modules
  in
  match modules with
  | [] -> Result.Error (Response.ErrorKind.ModuleNotTracked { path })
  | _ -> Result.Ok modules


let get_type_errors_in_overlay ~overlay ~build_system path =
  let open Result in
  let source_code_api = ErrorsEnvironment.ReadOnly.get_untracked_source_code_api overlay in
  get_modules ~source_code_api ~build_system path
  >>| fun modules ->
  ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers overlay modules
  |> List.sort ~compare:AnalysisError.compare
  |> List.map
       ~f:
         (Server.RequestHandler.instantiate_error
            ~lookup_source:(BuildSystem.lookup_source build_system)
            ~show_error_traces:false
            ~source_code_api)


let handle_get_type_errors ~paths ~client_id { State.environment; build_system; client_states; _ } =
  let open Result in
  let get_type_errors_for_path type_error_result path =
    get_overlay_id ~path ~client_id client_states
    >>= fun overlay_id ->
    let overlay = get_overlay ~environment overlay_id in
    get_type_errors_in_overlay ~overlay ~build_system path
    >>| fun type_errors -> type_errors @ type_error_result
  in
  List.fold_result ~init:[] ~f:get_type_errors_for_path paths
  >>| fun type_errors -> Response.TypeErrors { errors = type_errors }


let get_document_symbol_content_for_module source =
  LocationBasedLookup.SingleSymbolQueries.document_symbol_info ~source


let get_hover_content_for_module ~overlay ~position module_reference =
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  LocationBasedLookup.SingleSymbolQueries.hover_info_for_position
    ~type_environment
    ~module_reference
    position


let get_hover_in_overlay ~overlay ~build_system ~position module_ =
  let open Result in
  let source_code_api = ErrorsEnvironment.ReadOnly.get_untracked_source_code_api overlay in
  get_modules ~source_code_api ~build_system module_
  >>| List.map ~f:(get_hover_content_for_module ~overlay ~position)


let handle_hover ~path ~position ~client_id { State.environment; build_system; client_states; _ } =
  let open Result in
  get_overlay_id_unsafe ~path ~client_id client_states
  >>= fun overlay_id ->
  let overlay = get_or_create_overlay ~environment overlay_id in
  get_hover_in_overlay ~overlay ~build_system ~position path
  >>| fun contents ->
  Response.(
    Hover
      {
        contents =
          List.map contents ~f:(fun { value; docstring } -> HoverContent.{ value; docstring });
      })


let get_document_symbol_in_overlay ~overlay ~build_system module_ =
  let open Result in
  let source_code_api = ErrorsEnvironment.ReadOnly.get_untracked_source_code_api overlay in
  get_modules ~source_code_api ~build_system module_
  >>| fun x ->
  match x with
  | [] -> failwith "invalid module"
  | first :: _ -> (
      match SourceCodeApi.source_of_qualifier source_code_api first with
      | Some source -> get_document_symbol_content_for_module source
      | _ -> failwith "invalid source file")


let transform_symbol_kind symbol_kind =
  match symbol_kind with
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.File ->
      Response.DocumentSymbolItem.SymbolKind.File
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Module ->
      Response.DocumentSymbolItem.SymbolKind.Module
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Namespace ->
      Response.DocumentSymbolItem.SymbolKind.Namespace
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Package ->
      Response.DocumentSymbolItem.SymbolKind.Package
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Class ->
      Response.DocumentSymbolItem.SymbolKind.Class
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Method ->
      Response.DocumentSymbolItem.SymbolKind.Method
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Property ->
      Response.DocumentSymbolItem.SymbolKind.Property
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Field ->
      Response.DocumentSymbolItem.SymbolKind.Field
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Constructor ->
      Response.DocumentSymbolItem.SymbolKind.Constructor
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Enum ->
      Response.DocumentSymbolItem.SymbolKind.Enum
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Interface ->
      Response.DocumentSymbolItem.SymbolKind.Interface
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Function ->
      Response.DocumentSymbolItem.SymbolKind.Function
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Variable ->
      Response.DocumentSymbolItem.SymbolKind.Variable
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Constant ->
      Response.DocumentSymbolItem.SymbolKind.Constant
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.String ->
      Response.DocumentSymbolItem.SymbolKind.String
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Number ->
      Response.DocumentSymbolItem.SymbolKind.Number
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Boolean ->
      Response.DocumentSymbolItem.SymbolKind.Boolean
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Array ->
      Response.DocumentSymbolItem.SymbolKind.Array
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Object ->
      Response.DocumentSymbolItem.SymbolKind.Object
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Key ->
      Response.DocumentSymbolItem.SymbolKind.Key
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Null ->
      Response.DocumentSymbolItem.SymbolKind.Null
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.EnumMember ->
      Response.DocumentSymbolItem.SymbolKind.EnumMember
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Struct ->
      Response.DocumentSymbolItem.SymbolKind.Struct
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Event ->
      Response.DocumentSymbolItem.SymbolKind.Event
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.Operator ->
      Response.DocumentSymbolItem.SymbolKind.Operator
  | LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.SymbolKind.TypeParameter ->
      Response.DocumentSymbolItem.SymbolKind.TypeParameter


let rec transform_symbol symbol =
  {
    Response.DocumentSymbolItem.name =
      symbol.LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.name;
    detail = symbol.LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.detail;
    kind =
      transform_symbol_kind symbol.LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.kind;
    range = symbol.LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.range;
    selectionRange =
      symbol.LocationBasedLookup.SingleSymbolQueries.DocumentSymbolItem.selectionRange;
    children = List.map symbol.children ~f:(fun symbol -> transform_symbol symbol);
  }


let handle_document_symbol ~path ~client_id { State.environment; build_system; client_states; _ } =
  let open Result in
  get_overlay_id_unsafe ~path ~client_id client_states
  >>= fun overlay_id ->
  let overlay = get_or_create_overlay ~environment overlay_id in
  get_document_symbol_in_overlay ~overlay ~build_system path
  >>| fun symbols ->
  Response.(
    DocumentSymbol { symbols = List.map symbols ~f:(fun symbol -> transform_symbol symbol) })


let get_location_of_definition_for_module ~overlay ~build_system ~position module_reference =
  let open Result in
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  let source_code_api = TypeEnvironment.ReadOnly.get_untracked_source_code_api type_environment in
  match
    LocationBasedLookup.SingleSymbolQueries.location_of_definition
      ~type_environment
      ~module_reference
      position
  with
  | Error error -> Error (Response.LocationBasedLookupError error)
  | Ok { Ast.Location.WithModule.module_reference; start; stop } -> (
      match
        Analysis.SourcePaths.absolute_source_path_of_qualifier
          ~lookup_source:(BuildSystem.lookup_source build_system)
          ~source_code_api
          module_reference
      with
      | Some path -> Ok { Response.DefinitionLocation.path; range = { Ast.Location.start; stop } }
      | None -> Error (Response.SourcePathNotFound { module_reference }))


let get_location_of_definition_in_overlay ~overlay ~build_system ~position path =
  let open Result in
  let source_code_api = ErrorsEnvironment.ReadOnly.get_untracked_source_code_api overlay in
  get_modules ~source_code_api ~build_system path
  >>| List.map ~f:(get_location_of_definition_for_module ~overlay ~build_system ~position)


let handle_location_of_definition
    ~timer
    ~path
    ~position
    ~client_id
    { State.environment; build_system; client_states; _ }
  =
  let open Result in
  get_overlay_id_unsafe ~path ~client_id client_states
  >>= fun overlay_id ->
  let overlay = get_or_create_overlay ~environment overlay_id in
  get_location_of_definition_in_overlay ~overlay ~build_system ~position path
  >>| fun definitions ->
  let definitions, empty_reason =
    match List.partition_map ~f:Result.to_either definitions with
    | [], errors -> [], List.hd errors
    | definitions, _ -> definitions, None
  in
  let duration_us = Timer.stop_in_us timer in
  let duration = Float.of_int duration_us /. 1000. in
  Response.(LocationOfDefinition { definitions; empty_reason; duration })


let get_completion_for_module ~overlay ~position module_reference =
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  LocationBasedLookup.SingleSymbolQueries.completion_info_for_position
    ~type_environment
    ~module_reference
    position
  |> List.map
       ~f:(fun { LocationBasedLookup.SingleSymbolQueries.AttributeDetail.kind; name; detail } ->
         let attribute_kind =
           match kind with
           | Simple -> Response.CompletionItem.CompletionItemKind.Simple
           | Variable -> Response.CompletionItem.CompletionItemKind.Variable
           | Method -> Response.CompletionItem.CompletionItemKind.Method
           | Property -> Response.CompletionItem.CompletionItemKind.Property
         in
         { Response.CompletionItem.label = name; kind = attribute_kind; detail })


let get_completion_in_overlay ~overlay ~build_system ~position path =
  let open Result in
  let source_code_api = ErrorsEnvironment.ReadOnly.get_untracked_source_code_api overlay in
  get_modules ~source_code_api ~build_system path
  >>| List.map ~f:(get_completion_for_module ~overlay ~position)


let handle_completion
    ~path
    ~position
    ~client_id
    { State.environment; build_system; client_states; _ }
  =
  let open Result in
  get_overlay_id_unsafe ~path ~client_id client_states
  >>= fun overlay_id ->
  let overlay = get_or_create_overlay ~environment overlay_id in
  get_completion_in_overlay ~overlay ~build_system ~position path
  >>| List.concat
  >>| fun completions -> Response.(Completion { completions })


let handle_superclasses
    ~class_:{ Request.ClassExpression.module_; qualified_name }
    { State.environment; _ }
  =
  let root_environment = OverlaidEnvironment.root environment in
  let global_resolution =
    ErrorsEnvironment.ReadOnly.type_environment root_environment
    |> TypeEnvironment.ReadOnly.global_resolution
  in
  let get_module_by_name name =
    let module_name = Ast.Reference.create name in
    Option.some_if (GlobalResolution.module_exists global_resolution module_name) module_name
  in
  let to_class_expression superclass =
    (* TODO(T139769506): Instead of this hack where we assume `a.b.c` is a module when looking at
       `a.b.c.D`, we need to either fix qualification or use the DefineNames shared memory table to
       support nested classes etc. *)
    let superclass_reference = Ast.Reference.create superclass in
    match Ast.Reference.prefix superclass_reference with
    | Some module_ ->
        Some
          {
            Request.ClassExpression.module_ = Ast.Reference.show module_;
            qualified_name = Ast.Reference.last superclass_reference;
          }
    | None -> None
  in
  let as_exported_class_reference resolved =
    match resolved with
    | ResolvedReference.ModuleAttribute
        { from; name; remaining = []; export = Exported Module.Export.Name.Class; _ } ->
        Some (Ast.Reference.create ~prefix:from name)
    | _ -> None
  in
  match get_module_by_name module_ with
  | None ->
      Response.ErrorKind.InvalidRequest
        (Stdlib.Format.sprintf "Cannot find module with name `%s`" module_)
      |> Result.fail
  | Some module_name -> (
      let class_reference =
        Ast.Reference.combine module_name (Ast.Reference.create qualified_name)
      in
      let resolved_class_name =
        GlobalResolution.resolve_exports global_resolution class_reference
        >>| as_exported_class_reference
        |> Option.join
        >>| Ast.Reference.show
      in
      let invalid_request =
        Response.ErrorKind.InvalidRequest
          (Stdlib.Format.asprintf
             "Cannot find class `%s` in module `%a`."
             qualified_name
             Ast.Reference.pp
             module_name)
        |> Result.fail
      in
      match resolved_class_name with
      | None -> invalid_request
      | Some resolved_class_name ->
          let superclasses =
            resolved_class_name
            |> GlobalResolution.successors global_resolution
            |> List.filter_map ~f:to_class_expression
          in
          Response.Superclasses { superclasses } |> Result.return)


let with_broadcast ~subscriptions ~status f =
  let%lwt () =
    Subscriptions.broadcast subscriptions ~response:(lazy Response.(ServerStatus status))
  in
  let%lwt result = f () in
  let%lwt () =
    Subscriptions.broadcast subscriptions ~response:(lazy Response.(ServerStatus Status.Idle))
  in
  Lwt.return result


let with_broadcast_busy_checking ~subscriptions ~client_id f =
  with_broadcast ~subscriptions ~status:(Response.Status.BusyChecking { client_id }) f


let with_broadcast_busy_building ~subscriptions f =
  with_broadcast ~subscriptions ~status:Response.Status.BusyBuilding f


let handle_register_client ~client_id { State.client_states; _ } =
  match State.Client.register client_states client_id with
  | false -> Response.(Error (ErrorKind.ClientAlreadyRegistered { client_id }))
  | true -> Response.Ok_


let handle_dispose_client ~client_id { State.client_states; _ } =
  match State.Client.dispose client_states client_id with
  | false -> Response.(Error (ErrorKind.ClientNotRegistered { client_id }))
  | true -> Response.Ok_


let get_raw_path { Request.FileUpdateEvent.path; _ } = PyrePath.create_absolute path

let get_source_path_event_kind = function
  | Request.FileUpdateEvent.Kind.CreatedOrChanged -> SourcePath.Event.Kind.CreatedOrChanged
  | Request.FileUpdateEvent.Kind.Deleted -> SourcePath.Event.Kind.Deleted


let get_source_path_event { Request.FileUpdateEvent.kind; path } =
  let kind = get_source_path_event_kind kind in
  PyrePath.create_absolute path |> SourcePath.create |> SourcePath.Event.create ~kind


let get_artifact_path_event_kind = function
  | SourcePath.Event.Kind.CreatedOrChanged -> ArtifactPath.Event.Kind.CreatedOrChanged
  | SourcePath.Event.Kind.Deleted -> ArtifactPath.Event.Kind.Deleted


let get_artifact_path_event ~build_system { SourcePath.Event.kind; path } =
  let kind = get_artifact_path_event_kind kind in
  (match BuildSystem.lookup_artifact build_system path with
  (* In case there's no build system artifacts for this source, lookup the module as if it's built
     by a no-op build system (using normal source path mapping) *)
  | [] -> BuildSystem.default_lookup_artifact path
  | artifacts -> artifacts)
  |> List.map ~f:(ArtifactPath.Event.create ~kind)


let handle_non_critical_file_update ~scheduler ~subscriptions ~environment artifact_path_events =
  match artifact_path_events with
  | [] -> Lwt.return_unit
  | _ ->
      let run_file_update () =
        let _ = OverlaidEnvironment.run_update_root environment ~scheduler artifact_path_events in
        () |> Lwt.return
      in
      with_broadcast_busy_checking ~subscriptions ~client_id:None run_file_update


let update_build_system_sources ~build_system ~working_set source_path_events =
  let open Lwt.Infix in
  Lwt.catch
    (fun () ->
      BuildSystem.update_sources ~working_set build_system source_path_events >>= Lwt.return_ok)
    (function
      | _ as exn -> Lwt.return_error exn)


let handle_build_system_working_set_update ~build_system source_paths =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> BuildSystem.update_working_set build_system source_paths >>= Lwt.return_ok)
    (function
      | _ as exn -> Lwt.return_error exn)


let get_buck_error_message ~description ~additional_logs () =
  let header = Printf.sprintf "Cannot build the project: %s." description in
  if List.is_empty additional_logs then
    header
  else
    Printf.sprintf
      "%s Here are the last few lines of Buck log:\n  ...\n  %s"
      header
      (String.concat additional_logs ~sep:"\n  ")


let handle_file_update
    ~events
    ~subscriptions
    ~properties:{ Server.ServerProperties.critical_files; _ }
    { State.environment; scheduler; build_system; client_states; build_failure }
  =
  match CriticalFile.find critical_files ~within:(List.map events ~f:get_raw_path) with
  | Some path -> Lwt.return_error (Server.Stop.Reason.CriticalFileUpdate path)
  | None ->
      let current_source_path_events = List.map events ~f:get_source_path_event in
      let current_and_deferred_source_path_events =
        match Server.ServerState.BuildFailure.get_deferred_events build_failure with
        | [] -> current_source_path_events
        | deferred_events ->
            Log.log
              ~section:`Server
              "%d pre-existing deferred update events detected. Processing them now..."
              (List.length deferred_events);
            List.append deferred_events current_source_path_events
      in
      let working_set = State.Client.WorkingSet.to_list client_states in
      let handle_building_file_update () =
        match%lwt
          update_build_system_sources
            ~build_system
            ~working_set
            current_and_deferred_source_path_events
        with
        | Result.Ok sources ->
            (* The build has succeeded and deferred events are all processed. *)
            Server.ServerState.BuildFailure.clear build_failure;
            let changed_artifacts_from_source =
              List.concat_map
                current_and_deferred_source_path_events
                ~f:(get_artifact_path_event ~build_system)
            in
            let artifact_path_events = List.append sources changed_artifacts_from_source in
            let%lwt () =
              handle_non_critical_file_update
                ~scheduler
                ~subscriptions
                ~environment
                artifact_path_events
            in
            Lwt.return_ok Response.Ok_
        | Result.Error (Buck.Raw.BuckError { description; additional_logs; _ }) ->
            (* On build errors, stash away the current update and defer their processing until next
               update, hoping that the user could fix the error by then. This prevents the Pyre
               server from crashing on build failures. *)
            Log.log
              ~section:`Server
              "Build failure detected. Deferring %d events..."
              (List.length current_source_path_events);
            let error_message = get_buck_error_message ~description ~additional_logs () in
            Server.ServerState.BuildFailure.update
              ~events:current_source_path_events
              ~error_message
              build_failure;
            Lwt.return_ok Response.Ok_
        | Result.Error error ->
            (* We do not currently know how to recover from these exceptions *)
            Lwt.fail error
      in
      with_broadcast_busy_building ~subscriptions handle_building_file_update


let handle_working_set_update
    ~subscriptions
    { State.environment; scheduler; build_system; client_states; build_failure }
  =
  let paths = State.Client.WorkingSet.to_list client_states in
  match%lwt
    with_broadcast_busy_building ~subscriptions (fun () ->
        handle_build_system_working_set_update ~build_system paths)
  with
  | Result.Ok artifact_path_events ->
      handle_non_critical_file_update ~scheduler ~subscriptions ~environment artifact_path_events
  | Result.Error (Buck.Raw.BuckError { description; additional_logs; _ }) ->
      (* On working set errors, ignore the build error. On the next successful working set update,
         we will be consistent since we get the status for every open file. *)
      Log.log ~section:`Server "Build failure detected. Ignoring...";
      let error_message = get_buck_error_message ~description ~additional_logs () in
      Server.ServerState.BuildFailure.update
        ~events:
          (List.map paths ~f:(SourcePath.Event.create ~kind:SourcePath.Event.Kind.CreatedOrChanged))
        ~error_message
        build_failure;
      Lwt.return_unit
  | Result.Error error ->
      (* We do not currently know how to recover from these exceptions *)
      Lwt.fail error


let handle_local_update_in_overlay ~path ~content ~subscriptions ~build_system ~client_id overlay =
  let source_code_api =
    ErrorsEnvironment.Overlay.read_only overlay
    |> ErrorsEnvironment.ReadOnly.get_untracked_source_code_api
  in
  match get_modules ~source_code_api ~build_system path with
  | Result.Error kind -> Lwt.return (Response.Error kind)
  | Result.Ok modules ->
      let code_updates =
        let code_update =
          match content with
          | Some content -> SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode content
          | None -> SourceCodeIncrementalApi.Overlay.CodeUpdate.ResetCode
        in
        let to_update module_name =
          ArtifactPaths.artifact_path_of_qualifier ~source_code_api module_name
          |> Option.map ~f:(fun artifact_path -> artifact_path, code_update)
        in
        List.filter_map modules ~f:to_update
      in

      let%lwt () =
        match code_updates with
        | [] -> Lwt.return_unit
        | _ ->
            let run_local_update () =
              ErrorsEnvironment.Overlay.update_overlaid_code overlay ~code_updates |> Lwt.return
            in
            let%lwt _ =
              with_broadcast_busy_checking
                ~subscriptions
                ~client_id:(Some client_id)
                run_local_update
            in
            Lwt.return_unit
      in
      Lwt.return Response.Ok_


let handle_local_update
    ~path
    ~content
    ~client_id
    ~subscriptions
    { State.environment; build_system; client_states; _ }
    : Response.t Lwt.t
  =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  match State.Client.WorkingSet.lookup client_states ~client_id ~source_path with
  | `ClientNotRegistered ->
      Lwt.return Response.(Error (ErrorKind.ClientNotRegistered { client_id }))
  | `FileNotAdded -> Lwt.return Response.(Error (ErrorKind.FileNotOpened { path }))
  | `Ok overlay_id ->
      State.Client.OverlayId.to_string overlay_id
      |> OverlaidEnvironment.get_or_create_overlay environment
      |> handle_local_update_in_overlay ~path ~content ~subscriptions ~build_system ~client_id


let handle_file_opened
    ~path
    ~content
    ~client_id
    ~subscriptions
    ({ State.environment; client_states; build_system; _ } as state)
  =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  match State.Client.WorkingSet.add client_states ~client_id ~source_path with
  | `ClientNotRegistered ->
      Lwt.return Response.(Error (ErrorKind.ClientNotRegistered { client_id }))
  | `Ok overlay_id ->
      let%lwt () = handle_working_set_update ~subscriptions state in
      State.Client.OverlayId.to_string overlay_id
      |> OverlaidEnvironment.get_or_create_overlay environment
      |> handle_local_update_in_overlay ~path ~content ~subscriptions ~build_system ~client_id


let handle_file_closed
    ~path
    ~client_id
    ~subscriptions
    ({ State.environment; client_states; build_system; _ } as state)
  =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  match State.Client.WorkingSet.remove client_states ~client_id ~source_path with
  | `ClientNotRegistered ->
      Lwt.return Response.(Error (ErrorKind.ClientNotRegistered { client_id }))
  | `FileNotAdded -> Lwt.return Response.(Error (ErrorKind.FileNotOpened { path }))
  | `Ok overlay_id ->
      let overlay_id = State.Client.OverlayId.to_string overlay_id in
      let%lwt _ =
        (* This should already be a "get" instead of "create" since the creation should already have
           happend when the file is opened. *)
        OverlaidEnvironment.get_or_create_overlay environment overlay_id
        (* Attempt to reset the overlay state.

           Note that [path] may get removed from the filesystem after it's opened, in which case
           [handle_local_update_in_overlay] will return us an error. In theory, we should fix the
           module tracker such that it won't fail the [handle_local_update_in_overlay] on removed
           files. But that is a big TODO (T147166738) at the moment.

           But either way, we should never surface the error returned from
           [handle_local_update_in_overlay] to the client, given that all the issues are only
           related to server internals which the client should not see. *)
        |> handle_local_update_in_overlay
             ~path
             ~content:None
             ~subscriptions
             ~build_system
             ~client_id
      in
      let () = OverlaidEnvironment.remove_overlay environment overlay_id in
      let%lwt () = handle_working_set_update ~subscriptions state in
      Lwt.return Response.Ok_


let response_from_result = function
  | Result.Ok response -> response
  | Result.Error kind -> Response.Error kind


let handle_query
    ~server:
      {
        ServerInternal.state;
        properties =
          {
            Server.ServerProperties.socket_path;
            configuration = { Configuration.Analysis.project_root; local_root; _ };
            _;
          };
        _;
      }
    ~timer
  = function
  | Request.Query.GetTypeErrors { paths; client_id } ->
      let f state =
        let response = handle_get_type_errors ~paths ~client_id state |> response_from_result in
        Lwt.return response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Query.Hover { path; position; client_id } ->
      let state = Server.ExclusiveLock.unsafe_read state in
      let response = handle_hover ~path ~position ~client_id state |> response_from_result in
      Lwt.return response
  | Request.Query.LocationOfDefinition { path; position; client_id } ->
      let state = Server.ExclusiveLock.unsafe_read state in
      let response =
        handle_location_of_definition ~timer ~path ~position ~client_id state
        |> response_from_result
      in
      Lwt.return response
  | Request.Query.DocumentSymbol { path; client_id } ->
      let state = Server.ExclusiveLock.unsafe_read state in
      let response = handle_document_symbol ~path ~client_id state |> response_from_result in
      Lwt.return response
  | Request.Query.Completion { path; position; client_id } ->
      let state = Server.ExclusiveLock.unsafe_read state in
      let response = handle_completion ~path ~position ~client_id state |> response_from_result in
      Lwt.return response
  | Request.Query.GetInfo ->
      Response.Info
        {
          version = Version.version ();
          pid = Unix.getpid ();
          socket = PyrePath.absolute socket_path;
          global_root = PyrePath.absolute project_root;
          relative_local_root = PyrePath.get_relative_to_root ~root:project_root ~path:local_root;
        }
      |> Lwt.return
  | Request.Query.Superclasses { class_ } ->
      let f state =
        let response = handle_superclasses ~class_ state |> response_from_result in
        Lwt.return response
      in
      Server.ExclusiveLock.read state ~f


let handle_command ~server:{ ServerInternal.state; subscriptions; properties } = function
  | Request.Command.Stop -> Lwt.return_error Server.Stop.Reason.ExplicitRequest
  | Request.Command.RegisterClient { client_id } ->
      let f state =
        let response = handle_register_client ~client_id state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.DisposeClient { client_id } ->
      let f state =
        let response = handle_dispose_client ~client_id state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.FileOpened { path; content; client_id } ->
      let f state =
        let%lwt response = handle_file_opened ~path ~content ~client_id ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.FileClosed { path; client_id } ->
      let f state =
        let%lwt response = handle_file_closed ~path ~client_id ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.LocalUpdate { path; content; client_id } ->
      let f state =
        let%lwt response = handle_local_update ~path ~content ~client_id ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.FileUpdate events ->
      Server.ExclusiveLock.read state ~f:(handle_file_update ~events ~subscriptions ~properties)

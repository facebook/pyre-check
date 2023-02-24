(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base
open Analysis

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

let get_overlay ~environment overlay_id =
  match overlay_id with
  | None -> Result.Ok (OverlaidEnvironment.root environment)
  | Some overlay_id -> (
      match OverlaidEnvironment.overlay environment overlay_id with
      | Some overlay -> Result.Ok overlay
      | None -> Result.Error (Response.ErrorKind.OverlayNotFound { overlay_id }))


let get_modules ~module_tracker ~build_system path =
  let modules =
    let source_path = PyrePath.create_absolute path |> SourcePath.create in
    Server.PathLookup.modules_of_source_path
      source_path
      ~module_tracker
      ~lookup_artifact:(BuildSystem.lookup_artifact build_system)
  in
  match modules with
  | [] -> Result.Error (Response.ErrorKind.ModuleNotTracked { path })
  | _ -> Result.Ok modules


let get_type_errors_in_overlay ~overlay ~build_system path =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker ~build_system path
  >>| fun modules ->
  ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers overlay modules
  |> List.sort ~compare:AnalysisError.compare
  |> List.map
       ~f:
         (Server.RequestHandler.instantiate_error
            ~lookup_source:(BuildSystem.lookup_source build_system)
            ~show_error_traces:false
            ~module_tracker)


let handle_get_type_errors ~path ~overlay_id { State.environment; build_system; _ } =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_type_errors_in_overlay ~overlay ~build_system path
  >>| fun type_errors -> Response.TypeErrors type_errors


let get_hover_content_for_module ~overlay ~position module_reference =
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  LocationBasedLookup.hover_info_for_position ~type_environment ~module_reference position


let get_hover_in_overlay ~overlay ~build_system ~position module_ =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker ~build_system module_
  >>| List.map ~f:(get_hover_content_for_module ~overlay ~position)


let handle_hover ~path ~position ~overlay_id { State.environment; build_system; _ } =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_hover_in_overlay ~overlay ~build_system ~position path
  >>| fun contents ->
  Response.(
    Hover
      {
        contents =
          List.map contents ~f:(fun { value; docstring } -> HoverContent.{ value; docstring });
      })


let get_location_of_definition_for_module ~overlay ~build_system ~position module_reference =
  let open Option in
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  let module_tracker = TypeEnvironment.ReadOnly.module_tracker type_environment in
  LocationBasedLookup.location_of_definition ~type_environment ~module_reference position
  >>= fun { Ast.Location.WithModule.module_reference; start; stop } ->
  Server.PathLookup.instantiate_path
    ~lookup_source:(BuildSystem.lookup_source build_system)
    ~module_tracker
    module_reference
  >>| fun path -> { Response.DefinitionLocation.path; range = { Ast.Location.start; stop } }


let get_location_of_definition_in_overlay ~overlay ~build_system ~position path =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker ~build_system path
  >>| List.filter_map ~f:(get_location_of_definition_for_module ~overlay ~build_system ~position)


let handle_location_of_definition ~path ~position ~overlay_id { State.environment; build_system; _ }
  =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_location_of_definition_in_overlay ~overlay ~build_system ~position path
  >>| fun definitions -> Response.(LocationOfDefinition { definitions })


let handle_superclasses
    ~class_:{ Request.ClassExpression.module_; qualified_name }
    { State.environment; _ }
  =
  let root_environment = OverlaidEnvironment.root environment in
  let global_resolution =
    ErrorsEnvironment.ReadOnly.type_environment root_environment
    |> TypeEnvironment.ReadOnly.global_resolution
  in
  let get_module_by_name ~module_tracker name =
    let module_name = Ast.Reference.create name in
    Option.some_if (ModuleTracker.ReadOnly.is_module_tracked module_tracker module_name) module_name
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

  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker root_environment in
  match get_module_by_name ~module_tracker module_ with
  | None ->
      Response.ErrorKind.InvalidRequest (Format.sprintf "Cannot find module with name `%s`" module_)
      |> Result.fail
  | Some module_name ->
      let class_name =
        Ast.Reference.combine module_name (Ast.Reference.create qualified_name)
        |> Ast.Reference.show
      in
      if not (GlobalResolution.class_exists global_resolution class_name) then
        Response.ErrorKind.InvalidRequest
          (Format.asprintf
             "Cannot find class `%s` in module `%a`."
             qualified_name
             Ast.Reference.pp
             module_name)
        |> Result.fail
      else
        let superclasses =
          class_name
          |> GlobalResolution.successors ~resolution:global_resolution
          |> List.filter_map ~f:to_class_expression
        in
        Response.Superclasses { superclasses } |> Result.return


let with_broadcast ~subscriptions ~status f =
  let%lwt () =
    Subscriptions.broadcast subscriptions ~response:(lazy Response.(ServerStatus status))
  in
  let%lwt result = f () in
  let%lwt () =
    Subscriptions.broadcast subscriptions ~response:(lazy Response.(ServerStatus Status.Idle))
  in
  Lwt.return result


let with_broadcast_busy_checking ~subscriptions ~overlay_id f =
  with_broadcast ~subscriptions ~status:(Response.Status.BusyChecking { overlay_id }) f


let with_broadcast_busy_building ~subscriptions f =
  with_broadcast ~subscriptions ~status:Response.Status.BusyBuilding f


let handle_local_update
    ~path
    ~content
    ~overlay_id
    ~subscriptions
    { State.environment; build_system; _ }
    : Response.t Lwt.t
  =
  let module_tracker =
    OverlaidEnvironment.root environment |> ErrorsEnvironment.ReadOnly.module_tracker
  in
  match get_modules ~module_tracker ~build_system path with
  | Result.Error kind -> Lwt.return (Response.Error kind)
  | Result.Ok modules ->
      let code_updates =
        let code_update =
          match content with
          | Some content -> ModuleTracker.Overlay.CodeUpdate.NewCode content
          | None -> ModuleTracker.Overlay.CodeUpdate.ResetCode
        in
        let to_update module_name =
          ModuleTracker.ReadOnly.lookup_full_path module_tracker module_name
          |> Option.map ~f:(fun artifact_path -> artifact_path, code_update)
        in
        List.filter_map modules ~f:to_update
      in

      let%lwt () =
        match code_updates with
        | [] -> Lwt.return_unit
        | _ ->
            let run_local_update () =
              OverlaidEnvironment.update_overlay_with_code environment ~code_updates overlay_id
              |> Lwt.return
            in
            let%lwt _ =
              with_broadcast_busy_checking
                ~subscriptions
                ~overlay_id:(Some overlay_id)
                run_local_update
            in
            Lwt.return_unit
      in
      Lwt.return Response.Ok


let handle_file_opened ~path ~content ~overlay_id ~subscriptions state : Response.t Lwt.t =
  let%lwt response =
    match overlay_id with
    | Some overlay_id -> handle_local_update ~path ~content ~overlay_id ~subscriptions state
    | None -> Lwt.return Response.Ok
  in
  let () =
    match response with
    | Response.Error _ -> ()
    | _ ->
        let { State.open_files; _ } = state in
        let source_path = PyrePath.create_absolute path |> SourcePath.create in
        OpenFiles.open_file open_files ~source_path ~overlay_id
  in
  Lwt.return response


let handle_file_closed ~path ~overlay_id ~subscriptions ({ State.open_files; _ } as state)
    : Response.t Lwt.t
  =
  let source_path = PyrePath.create_absolute path |> SourcePath.create in
  if OpenFiles.contains open_files ~source_path ~overlay_id then
    let%lwt response =
      match overlay_id with
      | Some overlay_id -> handle_local_update ~path ~content:None ~overlay_id ~subscriptions state
      | None -> Lwt.return Response.Ok
    in
    match OpenFiles.close_file open_files ~source_path ~overlay_id with
    | Result.Ok () -> Lwt.return response
    | Result.Error error_kind -> Lwt.return (Response.Error error_kind)
  else
    Lwt.return (Response.Error (Response.ErrorKind.UntrackedFileClosed { path }))


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
  BuildSystem.lookup_artifact build_system path |> List.map ~f:(ArtifactPath.Event.create ~kind)


let handle_non_critical_file_update ~subscriptions ~environment artifact_path_events =
  match artifact_path_events with
  | [] -> Lwt.return_unit
  | _ ->
      let run_file_update () =
        let configuration =
          OverlaidEnvironment.root environment
          |> ErrorsEnvironment.ReadOnly.controls
          |> EnvironmentControls.configuration
        in
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            OverlaidEnvironment.run_update_root environment ~scheduler artifact_path_events)
        |> Lwt.return
      in
      with_broadcast_busy_checking ~subscriptions ~overlay_id:None run_file_update


let handle_file_update
    ~events
    ~subscriptions
    ~properties:{ Server.ServerProperties.critical_files; _ }
    { State.environment; build_system; open_files }
  =
  match Server.CriticalFile.find critical_files ~within:(List.map events ~f:get_raw_path) with
  | Some path -> Lwt.return_error (Server.Stop.Reason.CriticalFileUpdate path)
  | None ->
      let source_path_events = List.map events ~f:get_source_path_event in
      let%lwt () =
        Subscriptions.broadcast
          subscriptions
          ~response:(lazy Response.(ServerStatus Status.BusyBuilding))
      in
      let%lwt changed_artifacts_from_rebuild =
        let working_set = OpenFiles.open_files open_files in
        BuildSystem.update_sources build_system source_path_events ~working_set
      in
      let changed_artifacts_from_source =
        List.concat_map source_path_events ~f:(get_artifact_path_event ~build_system)
      in
      let artifact_path_events =
        List.append changed_artifacts_from_rebuild changed_artifacts_from_source
      in
      let%lwt () =
        handle_non_critical_file_update ~subscriptions ~environment artifact_path_events
      in
      Lwt.return_ok Response.Ok


let handle_working_set_update ~subscriptions { State.environment; build_system; open_files } =
  let%lwt artifact_path_events =
    with_broadcast_busy_building ~subscriptions (fun () ->
        OpenFiles.open_files open_files |> BuildSystem.update_working_set build_system)
  in
  handle_non_critical_file_update ~subscriptions ~environment artifact_path_events


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
  = function
  | Request.Query.GetTypeErrors { path; overlay_id } ->
      let f state =
        let response = handle_get_type_errors ~path ~overlay_id state |> response_from_result in
        Lwt.return response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Query.Hover { path; position; overlay_id } ->
      let f state =
        let response = handle_hover ~path ~position ~overlay_id state |> response_from_result in
        Lwt.return response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Query.LocationOfDefinition { path; position; overlay_id } ->
      let f state =
        let response =
          handle_location_of_definition ~path ~position ~overlay_id state |> response_from_result
        in
        Lwt.return response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Query.GetInfo ->
      Response.Info
        {
          version = Version.version ();
          pid = Core_unix.getpid () |> Core.Pid.to_int;
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
  | Request.Command.FileOpened { path; content; overlay_id } ->
      let f state =
        let%lwt response = handle_file_opened ~path ~content ~overlay_id ~subscriptions state in
        let%lwt () = handle_working_set_update ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.FileClosed { path; overlay_id } ->
      let f state =
        let%lwt response = handle_file_closed ~path ~overlay_id ~subscriptions state in
        let%lwt () = handle_working_set_update ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.LocalUpdate { path; content; overlay_id } ->
      let f state =
        let%lwt response = handle_local_update ~path ~content ~overlay_id ~subscriptions state in
        Lwt.return_ok response
      in
      Server.ExclusiveLock.read state ~f
  | Request.Command.FileUpdate events ->
      Server.ExclusiveLock.read state ~f:(handle_file_update ~events ~subscriptions ~properties)

(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre
open Ast
open Analysis

let instantiate_error ~lookup_source ~show_error_traces ~module_tracker error =
  AnalysisError.instantiate
    ~show_error_traces
    ~lookup:(PathLookup.instantiate_path ~lookup_source ~module_tracker)
    error


let instantiate_error_with_build_system
    ~build_system
    ~configuration:{ Configuration.Analysis.show_error_traces; _ }
    ~module_tracker
    error
  =
  instantiate_error
    ~lookup_source:(BuildSystem.lookup_source build_system)
    ~show_error_traces
    ~module_tracker
    error


let instantiate_errors_with_build_system ~build_system ~configuration ~module_tracker errors =
  List.map
    errors
    ~f:(instantiate_error_with_build_system ~build_system ~configuration ~module_tracker)


let process_display_type_error_request
    ~configuration
    ~state:{ ServerState.overlaid_environment; build_system; _ }
    ?overlay_id
    paths
  =
  let errors_environment =
    overlay_id
    >>= OverlaidEnvironment.overlay overlaid_environment
    |> Option.value ~default:(OverlaidEnvironment.root overlaid_environment)
  in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker errors_environment in
  let modules =
    match paths with
    | [] -> ModuleTracker.ReadOnly.project_qualifiers module_tracker
    | _ ->
        let get_module_for_source_path path =
          PyrePath.create_absolute path
          |> SourcePath.create
          |> PathLookup.modules_of_source_path_with_build_system ~build_system ~module_tracker
        in
        List.concat_map paths ~f:get_module_for_source_path
  in
  let errors =
    ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers errors_environment modules
    |> List.sort ~compare:AnalysisError.compare
  in
  Response.TypeErrors
    (instantiate_errors_with_build_system errors ~build_system ~configuration ~module_tracker)


let create_info_response
    {
      ServerProperties.socket_path;
      configuration = { Configuration.Analysis.project_root; local_root; _ };
      _;
    }
  =
  Response.Info
    {
      version = Version.version ();
      pid = Core_unix.getpid () |> Pid.to_int;
      socket = PyrePath.absolute socket_path;
      global_root = PyrePath.show project_root;
      relative_local_root = PyrePath.get_relative_to_root ~root:project_root ~path:local_root;
    }


let create_telemetry_response overall_timer =
  lazy (Response.IncrementalTelemetry { overall_duration_ms = Timer.stop_in_ms overall_timer })


let create_source_path_event path =
  let kind =
    if PyrePath.file_exists path then
      SourcePath.Event.Kind.CreatedOrChanged
    else
      SourcePath.Event.Kind.Deleted
  in
  SourcePath.create path |> SourcePath.Event.create ~kind


let create_artifact_path_event ~build_system { SourcePath.Event.kind; path } =
  let kind =
    match kind with
    | SourcePath.Event.Kind.CreatedOrChanged -> ArtifactPath.Event.Kind.CreatedOrChanged
    | SourcePath.Event.Kind.Deleted -> ArtifactPath.Event.Kind.Deleted
  in
  BuildSystem.lookup_artifact build_system path |> List.map ~f:(ArtifactPath.Event.create ~kind)


let process_incremental_update_request
    ~properties:{ ServerProperties.configuration; critical_files; _ }
    ~state:({ ServerState.overlaid_environment; subscriptions; build_system; _ } as state)
    paths
  =
  let open Lwt.Infix in
  let paths = List.map paths ~f:PyrePath.create_absolute in
  let subscriptions = ServerState.Subscriptions.all subscriptions in
  let errors_environment = OverlaidEnvironment.root overlaid_environment in
  match CriticalFile.find critical_files ~within:paths with
  | Some path ->
      let reason = Stop.Reason.CriticalFileUpdate path in
      let message = Stop.Reason.message_of reason in
      StartupNotification.produce ~log_path:configuration.log_directory message;
      Subscription.batch_send subscriptions ~response:(lazy (Response.Error message))
      >>= fun () -> Stop.stop_waiting_server reason
  | None ->
      let overall_timer = Timer.start () in
      let source_path_events = List.map paths ~f:create_source_path_event in
      let create_status_update_response status = lazy (Response.StatusUpdate status) in
      let create_type_errors_response =
        lazy
          (let errors =
             ErrorsEnvironment.ReadOnly.get_all_errors errors_environment
             |> List.sort ~compare:AnalysisError.compare
           in
           Response.TypeErrors
             (instantiate_errors_with_build_system
                errors
                ~build_system
                ~configuration
                ~module_tracker:(ErrorsEnvironment.ReadOnly.module_tracker errors_environment)))
      in
      Subscription.batch_send
        ~response:(create_status_update_response Response.ServerStatus.Rebuilding)
        subscriptions
      >>= fun () ->
      BuildSystem.update build_system source_path_events
      >>= fun changed_paths_from_rebuild ->
      Subscription.batch_send
        ~response:(create_status_update_response Response.ServerStatus.Rechecking)
        subscriptions
      >>= fun () ->
      let changed_paths =
        List.concat_map source_path_events ~f:(create_artifact_path_event ~build_system)
        |> List.append changed_paths_from_rebuild
        |> List.dedup_and_sort ~compare:ArtifactPath.Event.compare
      in
      let () =
        Scheduler.with_scheduler
          ~configuration
          ~should_log_exception:(fun _ -> true)
          ~f:(fun scheduler ->
            OverlaidEnvironment.run_update_root overlaid_environment ~scheduler changed_paths)
      in
      List.filter subscriptions ~f:Subscription.wants_type_errors
      |> Subscription.batch_send ~response:create_type_errors_response
      >>= fun () ->
      List.filter subscriptions ~f:(fun x -> not (Subscription.wants_type_errors x))
      |> Subscription.batch_send
           ~response:(create_status_update_response Response.ServerStatus.Ready)
      >>= fun () ->
      Subscription.batch_send ~response:(create_telemetry_response overall_timer) subscriptions
      >>= fun () -> Lwt.return state


let process_overlay_update ~build_system ~overlaid_environment ~overlay_id ~source_path ~code_update
  =
  let artifact_paths = BuildSystem.lookup_artifact build_system source_path in
  let code_updates = List.map artifact_paths ~f:(fun artifact_path -> artifact_path, code_update) in
  let _ =
    OverlaidEnvironment.update_overlay_with_code overlaid_environment ~code_updates overlay_id
  in
  let type_errors_for_module errors_environment =
    let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker errors_environment in
    let qualifier_for_artifact_path artifact_path =
      (* TODO(T126093907) Handle shadowed files correctly; this is not possible without a refactor
         of ModuleTracker and isn't necessary to get early feedback, but what we actually want to do
         is overlay the artifact path even though it is shadowed; this will be a no-op but it is
         needed to behave correctly if the user then deletes the shadowing file. *)
      ModuleTracker.ReadOnly.lookup_path module_tracker artifact_path
      |> function
      | ModuleTracker.PathLookup.Found module_path -> Some (ModulePath.qualifier module_path)
      | ModuleTracker.PathLookup.ShadowedBy _
      | ModuleTracker.PathLookup.NotFound ->
          None
    in
    List.filter_map artifact_paths ~f:qualifier_for_artifact_path
    |> List.concat_map ~f:(ErrorsEnvironment.ReadOnly.get_errors_for_qualifier errors_environment)
    |> instantiate_errors_with_build_system
         ~build_system
         ~configuration:
           (ModuleTracker.ReadOnly.controls module_tracker |> EnvironmentControls.configuration)
         ~module_tracker
  in
  OverlaidEnvironment.overlay overlaid_environment overlay_id
  >>| type_errors_for_module
  |> function
  | Some errors -> Response.TypeErrors errors
  | None -> Response.Error ("Unable to update overlay " ^ overlay_id)


let process_request
    ~properties:({ ServerProperties.configuration; _ } as properties)
    ~state:({ ServerState.overlaid_environment; build_system; _ } as state)
    request
  =
  match request with
  | Request.DisplayTypeError paths ->
      let response = process_display_type_error_request ~configuration ~state paths in
      Lwt.return (state, response)
  | Request.GetOverlayTypeErrors { overlay_id; path } ->
      let response = process_display_type_error_request ~configuration ~state ~overlay_id [path] in
      Lwt.return (state, response)
  | Request.IncrementalUpdate paths ->
      let open Lwt.Infix in
      process_incremental_update_request ~properties ~state paths
      >>= fun new_state -> Lwt.return (new_state, Response.Ok)
  | Request.Query query_text ->
      let response =
        Response.Query
          (Query.parse_and_process_request ~build_system ~overlaid_environment query_text None)
      in
      Lwt.return (state, response)
  | Request.QueryWithOverlay { query_text; overlay_id } ->
      let response =
        Response.Query
          (Query.parse_and_process_request
             ~build_system
             ~overlaid_environment
             query_text
             overlay_id)
      in
      Lwt.return (state, response)
  | Request.OverlayUpdate { overlay_id; source_path; code_update } ->
      let response =
        process_overlay_update
          ~build_system
          ~overlaid_environment
          ~overlay_id
          ~source_path:(PyrePath.create_absolute source_path |> SourcePath.create)
          ~code_update:
            (match code_update with
            | Request.OverlayCodeUpdate.NewCode code ->
                ModuleTracker.Overlay.CodeUpdate.NewCode code
            | Request.OverlayCodeUpdate.ResetCode -> ModuleTracker.Overlay.CodeUpdate.ResetCode)
      in
      Lwt.return (state, response)

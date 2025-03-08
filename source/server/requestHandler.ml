(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module provides the request handler for the "classic" Pyre daemon that powers `pyre
 * incremental`, `pyre persistent`, and `pyre query`.
 *
 * There are a few types of requests that can arrive:
 * - File updates from watchman (`IncrementalUpdate`) which trigger us to do an
 *   incremental update and propagate to overlays. - Overlay actions, which
 *   cause us to add / update / remove overlays from the overlaid environment in
 *   the server state.
 * - Requests to display type errors (in an overlay or not), to which we respond
 *   with type check results.
 * - Requests from `pyre query`, which are dispatched to the handler in
 *   `Query.ml`
 *)

module CamlUnix = Unix
open Core
open Pyre
open Analysis

let instantiate_error ~lookup_source ~show_error_traces ~source_code_api error =
  AnalysisError.instantiate
    ~show_error_traces
    ~lookup:(Analysis.SourcePaths.absolute_source_path_of_qualifier ~lookup_source ~source_code_api)
    error


let instantiate_error_with_build_system ~build_system ~source_code_api error =
  let { Configuration.Analysis.show_error_traces; _ } =
    SourceCodeApi.controls source_code_api |> EnvironmentControls.configuration
  in
  instantiate_error
    ~lookup_source:(BuildSystem.lookup_source build_system)
    ~show_error_traces
    ~source_code_api
    error


let instantiate_errors_with_build_system ~build_system ~source_code_api errors =
  List.map errors ~f:(instantiate_error_with_build_system ~build_system ~source_code_api)


let create_type_errors_response ?build_failure instantiated_errors =
  Response.TypeErrors { errors = instantiated_errors; build_failure }


let instantiate_and_create_type_errors_response
    ?build_failure
    ~build_system
    ~source_code_api
    raw_errors
  =
  instantiate_errors_with_build_system ~build_system ~source_code_api raw_errors
  |> create_type_errors_response ?build_failure


let instantiate_and_create_type_errors_response_for_modules
    ?build_failure
    ~build_system
    ~modules
    errors_environment
  =
  let source_code_api =
    ErrorsEnvironment.ReadOnly.get_untracked_source_code_api errors_environment
  in
  ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers errors_environment modules
  |> List.sort ~compare:AnalysisError.compare
  |> instantiate_and_create_type_errors_response ~build_system ~source_code_api ?build_failure


let instantiate_and_create_type_errors_response_for_all
    ?build_failure
    ~build_system
    overlaid_environment
  =
  let errors_environment = OverlaidEnvironment.root overlaid_environment in
  let type_check_qualifiers =
    OverlaidEnvironment.AssumeGlobalModuleListing.global_module_paths_api overlaid_environment
    |> GlobalModulePathsApi.type_check_qualifiers
  in
  instantiate_and_create_type_errors_response_for_modules
    errors_environment
    ~build_system
    ?build_failure
    ~modules:type_check_qualifiers


let process_display_type_error_request
    ~state:{ ServerState.overlaid_environment; build_system; build_failure; _ }
    ?overlay_id
    paths
  =
  let errors_environment =
    overlay_id
    >>= OverlaidEnvironment.overlay overlaid_environment
    |> Option.value ~default:(OverlaidEnvironment.root overlaid_environment)
  in
  let modules =
    match paths with
    | [] ->
        let global_module_paths_api =
          OverlaidEnvironment.AssumeGlobalModuleListing.global_module_paths_api overlaid_environment
        in
        GlobalModulePathsApi.type_check_qualifiers global_module_paths_api
    | _ ->
        let get_module_for_source_path path =
          let source_code_api =
            ErrorsEnvironment.ReadOnly.get_untracked_source_code_api errors_environment
          in
          PyrePath.create_absolute path
          |> SourcePath.create
          |> PathLookup.qualifiers_of_source_path_with_build_system ~build_system ~source_code_api
        in
        List.concat_map paths ~f:get_module_for_source_path
  in
  instantiate_and_create_type_errors_response_for_modules
    ?build_failure:(ServerState.BuildFailure.get_last_error_message build_failure)
    ~build_system
    ~modules
    errors_environment


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
      pid = CamlUnix.getpid ();
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


let create_status_update_response status = lazy (Response.StatusUpdate status)

let update_build_system ~build_system source_path_events =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> BuildSystem.update build_system source_path_events >>= Lwt.return_ok)
    (function
      | _ as exn -> Lwt.return_error exn)


let process_successful_rebuild
    ~scheduler
    ~subscriptions
    ~build_system
    ~overlaid_environment
    ~query_cache
    changed_paths_from_filesystem
    changed_paths_from_rebuild
  =
  let open Lwt.Infix in
  Subscription.batch_send
    ~response:(create_status_update_response Response.ServerStatus.Rechecking)
    subscriptions
  >>= fun () ->
  let changed_paths =
    List.concat_map changed_paths_from_filesystem ~f:(create_artifact_path_event ~build_system)
    |> List.append changed_paths_from_rebuild
    |> List.dedup_and_sort ~compare:ArtifactPath.Event.compare
  in
  (* TODO(stroxler): once we add a cache for `types` queries, invalidate it here. *)
  let update_result =
    OverlaidEnvironment.run_update_root overlaid_environment ~scheduler changed_paths
  in
  Query.Cache.invalidate query_cache update_result;
  let type_error_subscriptions, status_change_subscriptions =
    List.partition_tf subscriptions ~f:Subscription.wants_type_errors
  in
  Subscription.batch_send
    type_error_subscriptions
    ~response:
      (lazy
        (instantiate_and_create_type_errors_response_for_all ~build_system overlaid_environment))
  >>= fun () ->
  Subscription.batch_send
    status_change_subscriptions
    ~response:(create_status_update_response Response.ServerStatus.Ready)


let get_buck_error_message ~description ~additional_logs () =
  let header = Format.sprintf "Cannot build the project: %s." description in
  if List.is_empty additional_logs then
    header
  else
    Format.sprintf
      "%s Here are the last few lines of Buck log:\n  ...\n  %s"
      header
      (String.concat additional_logs ~sep:"\n  ")


let process_incremental_update_request
    ~properties:{ ServerProperties.configuration; critical_files; _ }
    ~state:
      ({
         ServerState.overlaid_environment;
         subscriptions;
         scheduler;
         build_system;
         build_failure;
         query_cache;
         _;
       } as state)
    paths
  =
  let open Lwt.Infix in
  let paths = List.map paths ~f:PyrePath.create_absolute in
  let subscriptions = ServerState.Subscriptions.all subscriptions in
  match CriticalFile.find critical_files ~within:paths with
  | Some path ->
      let reason = Stop.Reason.CriticalFileUpdate path in
      let message = Stop.Reason.message_of reason in
      StartupNotification.produce ~log_path:configuration.log_directory message;
      Subscription.batch_send subscriptions ~response:(lazy (Response.Error message))
      >>= fun () -> Stop.stop_waiting_server reason
  | None ->
      let overall_timer = Timer.start () in
      let current_source_path_events = List.map paths ~f:create_source_path_event in
      let current_and_deferred_source_path_events =
        match ServerState.BuildFailure.get_deferred_events build_failure with
        | [] -> current_source_path_events
        | deferred_events ->
            Log.log
              ~section:`Server
              "%d pre-existing deferred update events detected. Processing them now..."
              (List.length deferred_events);
            List.append deferred_events current_source_path_events
      in
      Subscription.batch_send
        ~response:(create_status_update_response Response.ServerStatus.Rebuilding)
        subscriptions
      >>= fun () ->
      update_build_system ~build_system current_and_deferred_source_path_events
      >>= (function
            | Result.Ok changed_paths_from_rebuild ->
                (* The build has succeeded and deferred events are all processed. *)
                ServerState.BuildFailure.clear build_failure;
                process_successful_rebuild
                  ~scheduler
                  ~subscriptions
                  ~build_system
                  ~overlaid_environment
                  ~query_cache
                  current_and_deferred_source_path_events
                  changed_paths_from_rebuild
            | Result.Error (Buck.Raw.BuckError { description; additional_logs; _ }) ->
                (* On build errors, stash away the current update and defer their processing until
                   next update, hoping that the user could fix the error by then. This prevents the
                   Pyre server from crashing on build failures. *)
                Log.log
                  ~section:`Server
                  "Build failure detected. Deferring %d events..."
                  (List.length current_source_path_events);
                let error_message = get_buck_error_message ~description ~additional_logs () in
                ServerState.BuildFailure.update
                  ~events:current_source_path_events
                  ~error_message
                  build_failure;
                Subscription.batch_send
                  subscriptions
                  ~response:(create_status_update_response Response.ServerStatus.Ready)
            | Result.Error exn ->
                (* We do not currently know how to recover from these exceptions *)
                Lwt.fail exn)
      >>= fun () ->
      Subscription.batch_send ~response:(create_telemetry_response overall_timer) subscriptions
      >>= fun () -> Lwt.return state


let process_overlay_update
    ~build_system
    ~overlaid_environment
    ~overlay_id
    ~build_failure
    ~source_path
    ~code_update
  =
  let artifact_paths = BuildSystem.lookup_artifact build_system source_path in
  let code_updates = List.map artifact_paths ~f:(fun artifact_path -> artifact_path, code_update) in
  let modules =
    OverlaidEnvironment.update_overlay_with_code overlaid_environment ~code_updates overlay_id
    |> ErrorsEnvironment.UpdateResult.invalidated_modules
  in
  match OverlaidEnvironment.overlay overlaid_environment overlay_id with
  | None -> Response.Error ("Unable to update overlay " ^ overlay_id)
  | Some errors_environment ->
      (* TODO(T126093907) Handle shadowed files correctly; this is not possible without a refactor
         of ModuleTracker and isn't necessary to get early feedback, but what we actually want to do
         is overlay the artifact path even though it is shadowed; this will be a no-op but it is
         needed to behave correctly if the user then deletes the shadowing file. *)
      instantiate_and_create_type_errors_response_for_modules
        ?build_failure:(ServerState.BuildFailure.get_last_error_message build_failure)
        ~build_system
        ~modules
        errors_environment


let process_request
    ~properties
    ~state:
      ({ ServerState.overlaid_environment; scheduler; build_system; build_failure; query_cache; _ }
      as state)
    request
  =
  match request with
  | Request.DisplayTypeError paths ->
      let response = process_display_type_error_request ~state paths in
      Lwt.return (state, response)
  | Request.GetOverlayTypeErrors { overlay_id; path } ->
      let response = process_display_type_error_request ~state ~overlay_id [path] in
      Lwt.return (state, response)
  | Request.IncrementalUpdate paths ->
      let open Lwt.Infix in
      process_incremental_update_request ~properties ~state paths
      >>= fun new_state -> Lwt.return (new_state, Response.Ok)
  | Request.Query query_text ->
      let response =
        Response.Query
          (Query.parse_and_process_request
             ~scheduler
             ~build_system
             ~overlaid_environment
             ~query_cache
             query_text
             None)
      in
      Lwt.return (state, response)
  | Request.QueryWithOverlay { query_text; overlay_id } ->
      let response =
        Response.Query
          (Query.parse_and_process_request
             ~scheduler
             ~build_system
             ~overlaid_environment
             ~query_cache
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
          ~build_failure
          ~source_path:(PyrePath.create_absolute source_path |> SourcePath.create)
          ~code_update:
            (match code_update with
            | Request.OverlayCodeUpdate.NewCode code ->
                SourceCodeIncrementalApi.Overlay.CodeUpdate.NewCode code
            | Request.OverlayCodeUpdate.ResetCode ->
                SourceCodeIncrementalApi.Overlay.CodeUpdate.ResetCode)
      in
      Lwt.return (state, response)

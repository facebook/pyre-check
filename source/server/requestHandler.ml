(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis

let module_of_path ~configuration ~module_tracker path =
  match ModuleTracker.lookup_path ~configuration module_tracker path with
  | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } -> Some qualifier
  | _ -> None


let instantiate_path ~build_system ~configuration ~ast_environment qualifier =
  match AstEnvironment.ReadOnly.get_real_path ~configuration ast_environment qualifier with
  | None -> None
  | Some artifact_path ->
      let path =
        match BuildSystem.lookup_source build_system artifact_path with
        | Some source_path -> source_path
        | None ->
            (* NOTE (grievejia): This means the path is under the search roots but is not tracked by
               Buck. Showing the original path here is a compromise: ideally we should instead look
               into configuring Buck-built project in such a way that all source files are tracked
               by Buck. *)
            artifact_path
      in
      Some (PyrePath.absolute path)


let instantiate_error
    ~build_system
    ~configuration:({ Configuration.Analysis.show_error_traces; _ } as configuration)
    ~ast_environment
    error
  =
  AnalysisError.instantiate
    ~show_error_traces
    ~lookup:(instantiate_path ~build_system ~configuration ~ast_environment)
    error


let instantiate_errors ~build_system ~configuration ~ast_environment errors =
  List.map errors ~f:(instantiate_error ~build_system ~configuration ~ast_environment)


let process_display_type_error_request
    ~configuration
    ~state:{ ServerState.type_environment; error_table; build_system; _ }
    paths
  =
  let modules =
    match paths with
    | [] -> Hashtbl.keys error_table
    | _ ->
        let get_module_for_source_path path =
          let path = PyrePath.create_absolute path in
          match BuildSystem.lookup_artifact build_system path with
          | [] -> None
          | artifact_path :: _ ->
              (* If the same source file is mapped to more than one artifact paths, all of the
                 artifact paths will likely contain the same set of type errors. It does not matter
                 which artifact path is picked.

                 NOTE (grievejia): It is possible for the type errors to differ. We may need to
                 reconsider how this is handled in the future. *)
              module_of_path
                ~configuration
                ~module_tracker:(TypeEnvironment.module_tracker type_environment)
                artifact_path
        in
        List.filter_map paths ~f:get_module_for_source_path
  in
  let errors =
    let get_type_error qualifier = Hashtbl.find error_table qualifier |> Option.value ~default:[] in
    List.concat_map modules ~f:get_type_error |> List.sort ~compare:AnalysisError.compare
  in
  Response.TypeErrors
    (instantiate_errors
       errors
       ~build_system
       ~configuration
       ~ast_environment:
         (TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only))


let notify_all_subscriptions ~with_response = function
  | [] -> Lwt.return_unit
  | _ as subscriptions ->
      let response = with_response () in
      List.map subscriptions ~f:(fun subscription -> Subscription.send ~response subscription)
      |> Lwt.join


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
      pid = Unix.getpid () |> Pid.to_int;
      socket = PyrePath.absolute socket_path;
      global_root = PyrePath.show project_root;
      relative_local_root = PyrePath.get_relative_to_root ~root:project_root ~path:local_root;
    }


let process_incremental_update_request
    ~properties:({ ServerProperties.configuration; critical_files; _ } as properties)
    ~state:({ ServerState.type_environment; error_table; subscriptions; build_system; _ } as state)
    paths
  =
  let open Lwt.Infix in
  let paths = List.map paths ~f:PyrePath.create_absolute in
  match CriticalFile.find critical_files ~within:paths with
  | Some path ->
      Format.asprintf
        "Pyre needs to restart as it is notified on potential changes in `%a`"
        PyrePath.pp
        path
      |> StartupNotification.produce ~log_path:configuration.log_directory;
      Stop.log_and_stop_waiting_server ~reason:"critical file update" ~properties ()
  | None ->
      let create_status_update_response status () = Response.StatusUpdate status in
      let create_type_errors_response () =
        let errors =
          Hashtbl.data error_table
          |> List.concat_no_order
          |> List.sort ~compare:AnalysisError.compare
        in
        Response.TypeErrors
          (instantiate_errors
             errors
             ~build_system
             ~configuration
             ~ast_environment:
               (TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only))
      in
      let subscriptions = ServerState.Subscriptions.all subscriptions in
      notify_all_subscriptions
        ~with_response:(create_status_update_response Response.ServerStatus.Rebuilding)
        subscriptions
      >>= fun () ->
      BuildSystem.update build_system paths
      >>= fun changed_paths_from_rebuild ->
      notify_all_subscriptions
        ~with_response:(create_status_update_response Response.ServerStatus.Rechecking)
        subscriptions
      >>= fun () ->
      let changed_paths =
        List.concat_map paths ~f:(BuildSystem.lookup_artifact build_system)
        |> List.append changed_paths_from_rebuild
        |> List.dedup_and_sort ~compare:PyrePath.compare
      in
      let _ =
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            Service.IncrementalCheck.recheck
              ~configuration
              ~scheduler
              ~environment:type_environment
              ~errors:error_table
              changed_paths)
      in
      notify_all_subscriptions ~with_response:create_type_errors_response subscriptions
      >>= fun () -> Lwt.return state


let process_request
    ~properties:({ ServerProperties.configuration; _ } as properties)
    ~state:({ ServerState.type_environment; build_system; _ } as state)
    request
  =
  match request with
  | Request.DisplayTypeError paths ->
      let response = process_display_type_error_request ~configuration ~state paths in
      Lwt.return (state, response)
  | Request.IncrementalUpdate paths ->
      let open Lwt.Infix in
      process_incremental_update_request ~properties ~state paths
      >>= fun new_state -> Lwt.return (new_state, Response.Ok)
  | Request.Query query_text ->
      let response =
        Response.Query
          (Query.parse_and_process_request
             ~build_system
             ~environment:type_environment
             ~configuration
             query_text)
      in
      Lwt.return (state, response)

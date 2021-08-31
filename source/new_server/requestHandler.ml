(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
module Path = Pyre.Path

let module_of_path ~configuration ~module_tracker path =
  match ModuleTracker.lookup_path ~configuration module_tracker path with
  | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } -> Some qualifier
  | _ -> None


let instantiate_path ~build_system ~configuration ~ast_environment qualifier =
  AstEnvironment.ReadOnly.get_real_path ~configuration ast_environment qualifier
  |> Option.bind ~f:(BuildSystem.lookup_source build_system)
  |> Option.map ~f:Path.absolute


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
    ~state:{ ServerState.configuration; type_environment; error_table; build_system; _ }
    paths
  =
  let modules =
    match paths with
    | [] -> Hashtbl.keys error_table
    | _ ->
        let get_module_for_source_path path =
          let path = Path.create_absolute path in
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


let process_incremental_update_request
    ~state:
      ({
         ServerState.critical_files;
         configuration;
         type_environment;
         error_table;
         subscriptions;
         build_system;
         _;
       } as state)
    paths
  =
  let open Lwt.Infix in
  let paths = List.map paths ~f:Path.create_absolute in
  match CriticalFile.find critical_files ~within:paths with
  | Some path ->
      Format.asprintf
        "Pyre needs to restart as it is notified on potential changes in `%a`"
        Path.pp
        path
      |> StartupNotification.produce ~log_path:configuration.log_directory;
      Stop.log_and_stop_waiting_server ~reason:"critical file update" ~state ()
  | None -> (
      BuildSystem.update build_system paths
      >>= fun changed_paths_from_rebuild ->
      let changed_paths =
        List.concat_map paths ~f:(BuildSystem.lookup_artifact build_system)
        |> List.append changed_paths_from_rebuild
        |> List.dedup_and_sort ~compare:Path.compare
      in
      let _ =
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            Server.IncrementalCheck.recheck
              ~configuration
              ~scheduler
              ~environment:type_environment
              ~errors:error_table
              changed_paths)
      in
      match ServerState.Subscriptions.all subscriptions with
      | [] -> Lwt.return state
      | _ as subscriptions ->
          let response =
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
          List.map subscriptions ~f:(fun subscription -> Subscription.send ~response subscription)
          |> Lwt.join
          >>= fun () -> Lwt.return state)


let process_request
    ~state:({ ServerState.socket_path; configuration; type_environment; build_system; _ } as state)
    request
  =
  match request with
  | Request.GetInfo ->
      let { Configuration.Analysis.project_root; local_root; _ } = configuration in
      let response =
        Response.Info
          {
            version = Version.version ();
            pid = Unix.getpid () |> Pid.to_int;
            socket = Pyre.Path.absolute socket_path;
            global_root = Path.show project_root;
            relative_local_root = Path.get_relative_to_root ~root:project_root ~path:local_root;
          }
      in
      Lwt.return (state, response)
  | Request.DisplayTypeError paths ->
      let response = process_display_type_error_request ~state paths in
      Lwt.return (state, response)
  | Request.IncrementalUpdate paths ->
      let open Lwt.Infix in
      process_incremental_update_request ~state paths
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
  | Request.Stop -> Stop.log_and_stop_waiting_server ~reason:"explicit request" ~state ()

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


let instantiate_error
    ~build_system
    ~configuration:({ Configuration.Analysis.show_error_traces; _ } as configuration)
    ~ast_environment
    error
  =
  let lookup qualifier =
    match AstEnvironment.ReadOnly.get_real_path ~configuration ast_environment qualifier with
    | None -> Lwt.return_none
    | Some path ->
        let open Lwt.Infix in
        BuildSystem.lookup_source build_system path
        >>= fun path -> Lwt.return (Option.map path ~f:Path.absolute)
  in
  AnalysisError.async_instantiate ~show_error_traces ~lookup error


let instantiate_errors ~build_system ~configuration ~ast_environment errors =
  List.map errors ~f:(instantiate_error ~build_system ~configuration ~ast_environment) |> Lwt.all


let find_critical_file ~server_configuration:{ ServerConfiguration.critical_files; _ } paths =
  ServerConfiguration.CriticalFile.find critical_files ~within:paths


let process_display_type_error_request
    ~state:{ ServerState.configuration; type_environment; error_table; build_system; _ }
    paths
  =
  let open Lwt.Infix in
  let get_changed_modules paths =
    match paths with
    | [] -> Lwt.return (Hashtbl.keys error_table)
    | _ ->
        let get_module_for_source_path path =
          let path = Path.create_absolute path in
          BuildSystem.lookup_artifact build_system path
          >>= function
          | [] -> Lwt.return_none
          | artifact_path :: _ ->
              (* If the same source file is mapped to more than one artifact paths, all of the
                 artifact paths will likely contain the same set of type errors. It does not matter
                 which artifact path is picked.

                 NOTE (grievejia): It is possible for the type errors to differ. We may need to
                 reconsider how this is handled in the future. *)
              Lwt.return
                (module_of_path
                   ~configuration
                   ~module_tracker:(TypeEnvironment.module_tracker type_environment)
                   artifact_path)
        in
        List.map paths ~f:get_module_for_source_path
        |> Lwt.all
        >>= fun modules -> Lwt.return (List.filter_opt modules)
  in
  get_changed_modules paths
  >>= fun modules ->
  let errors =
    let get_type_error qualifier = Hashtbl.find error_table qualifier |> Option.value ~default:[] in
    List.concat_map modules ~f:get_type_error |> List.sort ~compare:AnalysisError.compare
  in
  instantiate_errors
    errors
    ~build_system
    ~configuration
    ~ast_environment:(TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only)
  >>= fun errors -> Lwt.return (Response.TypeErrors errors)


let translate_changed_paths ~build_system paths =
  let open Lwt.Infix in
  List.map paths ~f:(BuildSystem.lookup_artifact build_system)
  |> Lwt.all
  >>= fun changed_paths -> Lwt.return (List.concat changed_paths)


let process_incremental_update_request
    ~state:
      ( {
          ServerState.server_configuration;
          configuration;
          type_environment;
          error_table;
          subscriptions;
          build_system;
          _;
        } as state )
    paths
  =
  let open Lwt.Infix in
  let paths = List.map paths ~f:Path.create_absolute in
  match find_critical_file ~server_configuration paths with
  | Some path ->
      Format.asprintf
        "Pyre needs to restart as it is notified on potential changes in `%a`"
        Path.pp
        path
      |> StartupNotification.produce_for_configuration ~server_configuration;
      Stop.log_and_stop_waiting_server ~reason:"critical file update" ~state ()
  | None -> (
      BuildSystem.update build_system paths
      >>= fun changed_paths_from_rebuild ->
      translate_changed_paths ~build_system paths
      >>= fun changed_paths_from_filesystem ->
      let changed_paths =
        List.append changed_paths_from_rebuild changed_paths_from_filesystem
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
          let errors =
            Hashtbl.data error_table
            |> List.concat_no_order
            |> List.sort ~compare:AnalysisError.compare
          in
          instantiate_errors
            errors
            ~build_system
            ~configuration
            ~ast_environment:
              (TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only)
          >>= fun errors ->
          let response = Response.TypeErrors errors in
          List.map subscriptions ~f:(fun subscription -> Subscription.send ~response subscription)
          |> Lwt.join
          >>= fun () -> Lwt.return state )


let process_request
    ~state:
      ( { ServerState.socket_path; server_configuration; configuration; type_environment; _ } as
      state )
    request
  =
  let open Lwt.Infix in
  match request with
  | Request.GetInfo ->
      let response =
        Response.Info
          {
            version = Version.version ();
            pid = Unix.getpid () |> Pid.to_int;
            socket = Pyre.Path.absolute socket_path;
            configuration = server_configuration;
          }
      in
      Lwt.return (state, response)
  | Request.DisplayTypeError paths ->
      process_display_type_error_request ~state paths
      >>= fun response -> Lwt.return (state, response)
  | Request.IncrementalUpdate paths ->
      process_incremental_update_request ~state paths
      >>= fun new_state -> Lwt.return (new_state, Response.Ok)
  | Request.Query query_text ->
      let response =
        Response.Query
          (Server.Query.parse_and_process_request
             ~environment:type_environment
             ~configuration
             query_text)
      in
      Lwt.return (state, response)
  | Request.Stop -> Stop.log_and_stop_waiting_server ~reason:"explicit request" ~state ()

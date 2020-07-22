(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
module Path = Pyre.Path

let module_of_path ~configuration ~module_tracker path =
  match ModuleTracker.lookup_path ~configuration module_tracker path with
  | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } -> Some qualifier
  | _ -> None


let instantiate_error
    ~configuration:({ Configuration.Analysis.show_error_traces; _ } as configuration)
    ~ast_environment
    error
  =
  AnalysisError.instantiate
    ~show_error_traces
    ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
    error


let process_request
    ~state:
      ( {
          ServerState.socket_path;
          server_configuration;
          configuration;
          type_environment;
          error_table;
          _;
        } as state )
    request
  =
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
      let modules =
        match paths with
        | [] -> Hashtbl.keys error_table
        | _ ->
            List.filter_map paths ~f:(fun path ->
                module_of_path
                  ~configuration
                  ~module_tracker:(TypeEnvironment.module_tracker type_environment)
                  (Path.create_absolute ~follow_symbolic_links:false path))
      in
      let errors =
        let get_type_error qualifier =
          Hashtbl.find error_table qualifier |> Option.value ~default:[]
        in
        List.concat_map modules ~f:get_type_error |> List.sort ~compare:AnalysisError.compare
      in
      let instantiated_errors =
        List.map
          errors
          ~f:
            (instantiate_error
               ~configuration
               ~ast_environment:
                 (TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only))
      in
      let response = Response.TypeErrors instantiated_errors in
      Lwt.return (state, response)
  | Request.Stop -> Stop.stop_waiting_server ()

(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Network
open Server

let run
    _
    transitive
    verbose
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    additional_checks
    sequential
    filter_directories
    ignore_all_errors
    number_of_workers
    log_identifier
    logger
    profiling_output
    project_root
    search_path
    _taint_models_directory
    excludes
    extensions
    local_root
    ()
  =
  try
    let filter_directories =
      filter_directories
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:Path.create_absolute
    in
    let ignore_all_errors =
      ignore_all_errors
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:Path.create_absolute
    in
    let configuration =
      Configuration.Analysis.create
        ~verbose
        ?expected_version
        ~sections
        ~debug
        ~strict
        ~declare
        ~show_error_traces
        ~log_identifier
        ?logger
        ?profiling_output
        ~infer
        ~additional_checks
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create)
        ~project_root:(Path.create_absolute project_root)
        ~excludes
        ~extensions
        ~local_root:(Path.create_absolute local_root)
        ~incremental_transitive_dependencies:transitive
        ()
    in
    (fun () ->
      let socket =
        try Operations.connect ~retries:3 ~configuration with
        | Operations.ConnectionFailure -> raise Operations.ServerNotRunning
      in
      Socket.write socket (Protocol.Request.DisplayTypeErrors []);
      let response_json =
        match Socket.read socket with
        | Protocol.TypeCheckResponse errors ->
            `Assoc
              [ ( "errors",
                  `List
                    (List.map
                       ~f:(fun error -> Analysis.Error.to_json ~show_error_traces error)
                       errors) ) ]
        | _ -> failwith "Unexpected response in incremental check."
      in
      Log.print "%s" (Yojson.Safe.to_string response_json))
    |> Scheduler.run_process ~configuration
  with
  | Operations.ServerNotRunning ->
      Log.print "Server is not running.\n";
      exit 1
  | Operations.VersionMismatch _ ->
      Log.print "The running server has an incompatible version with the current version.\n";
      exit 1


let command =
  Command.basic_spec
    ~summary:
      "Shows current errors by asking the server. Starts a daemon server in the current directory \
       if it does not exist."
    Command.Spec.(
      empty
      +> flag
           "-nonblocking"
           no_arg
           ~doc:
             ( "Ask the server to return partial results immediately, "
             ^ "even if analysis is still in progress." )
      +> flag "-transitive" no_arg ~doc:"Calculate dependencies of changed files transitively."
      ++ Specification.base_command_line_arguments)
    run

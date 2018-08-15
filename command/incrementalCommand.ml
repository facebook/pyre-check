(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Network
open Server

module Scheduler = Service.Scheduler


let run
    verbose
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    filter_directories
    filter_directories_semicolon
    number_of_workers
    log_identifier
    logger
    project_root
    search_path
    typeshed
    local_root
    () =
  try
    let filter_directories =
      let deprecated_directories =
        filter_directories
        >>| List.map ~f:Path.create_absolute
      in
      filter_directories_semicolon
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:Path.create_absolute
      |> (fun directories ->
          if Option.is_some directories then directories else deprecated_directories)
    in
    let configuration =
      Configuration.create
        ~verbose
        ?expected_version
        ~sections
        ~debug
        ~strict
        ~declare
        ~show_error_traces
        ~log_identifier
        ?logger
        ~infer
        ~recursive_infer
        ~parallel:(not sequential)
        ?filter_directories
        ~number_of_workers
        ~search_path:(List.map ~f:Path.create_absolute search_path)
        ?typeshed:(typeshed >>| Path.create_absolute)
        ~project_root:(Path.create_absolute project_root)
        ~local_root:(Path.create_absolute local_root)
        ()
    in
    Scheduler.initialize_process ~configuration;

    let socket =
      try
        Server.Operations.connect ~retries:3 ~configuration
      with Server.Operations.ConnectionFailure ->
        raise ServerConfiguration.ServerNotRunning
    in

    Socket.write
      socket
      Server.Protocol.Request.FlushTypeErrorsRequest;

    let response_json =
      match Socket.read socket with
      | Server.Protocol.TypeCheckResponse errors ->
          errors
          |> List.map ~f:snd
          |> List.concat
          |> (fun errors ->
              `List
                (List.map
                   ~f:(fun error -> Analysis.Error.to_json ~detailed:show_error_traces error)
                   errors))
      | _ -> failwith "Unexpected response in incremental check."
    in
    Log.print "%s" (Yojson.Safe.to_string response_json);
    Statistics.flush ()
  with
  | ServerConfiguration.ServerNotRunning ->
      Log.print "Server is not running.\n";
      exit 1
  | Server.Operations.VersionMismatch _ ->
      Log.print "The running server has an incompatible version with the current version.\n";
      exit 1


let command =
  Command.basic_spec
    ~summary:"Shows current errors by asking the server. \
              Starts a daemon server in the current directory if it does not exist."
    Command.Spec.(
      empty
      ++ Specification.base_command_line_arguments)
    run

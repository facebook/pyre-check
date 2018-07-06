(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Socket = CommandSocket
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
    analyze
    sequential
    number_of_workers
    log_identifier
    project_root
    search_path
    typeshed
    source_root
    () =
  try
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
        ~infer
        ~recursive_infer
        ~analyze
        ~parallel:(not sequential)
        ~number_of_workers
        ~search_path:(List.map ~f:Path.create_absolute search_path)
        ?typeshed:(typeshed >>| Path.create_absolute)
        ~project_root:(Path.create_absolute project_root)
        ~source_root:(Path.create_absolute source_root)
        ()
    in
    Scheduler.initialize_process ~configuration;

    let socket =
      try
        ServerOperations.connect ~retries:3 ~configuration
      with ServerOperations.ConnectionFailure ->
        raise ServerConfiguration.ServerNotRunning
    in

    Socket.write
      socket
      ServerProtocol.Request.FlushTypeErrorsRequest;

    let response_json =
      match Socket.read socket with
      | ServerProtocol.TypeCheckResponse errors ->
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
  | ServerOperations.VersionMismatch _ ->
      Log.print "The running server has an incompatible version with the current version.\n";
      exit 1


let command =
  Command.basic_spec
    ~summary:"Shows current errors by asking the server. \
              Starts a daemon server in the current directory if it does not exist."
    Command.Spec.(
      empty
      ++ CommandSpec.base_command_line_arguments)
    run

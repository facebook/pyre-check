(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Network
open Scheduler
open Server
open State
open Socket
open Protocol
module Time = Core_kernel.Time_ns.Span
module Request = Server.Request
module Connections = Server.Connections.Unix

exception AlreadyRunning

(* In theory the exception that's supposed to be emitted because of a RST packet is supposed to be
   `Unix.Unix_error (Unix.ECONNRESET, _, _)`. However for reasons unknown, sometimes the OCaml
   networking stack emits a `Sys_error "Connection reset by peer"` instead. The hacky fix herein is
   just to match for both of these. *)
let is_connection_reset_error = function
  | Unix.Unix_error (Unix.ECONNRESET, _, _) -> true
  (* We can't match on Sys_error "Connection reset by peer" directly since Sys_error is marked as
     @ocaml.warn_on_literal_pattern, so we'd get a `warning 52` since relying on the exact string
     here is fragile to version upgrades. Unfortunately we're just going to have to live with this
     fragility, and hope they fix this behavior before they modify the message *)
  | Sys_error error when String.equal error "Connection reset by peer" -> true
  | _ -> false


let computation_thread
    request_queue
    ({ Configuration.Server.pid_path; configuration = analysis_configuration; _ } as configuration)
    ({ Server.State.environment; scheduler; _ } as state)
  =
  let errors_to_lsp_responses { Server.State.open_documents; _ } errors =
    let build_file_to_error_map error_list =
      let table = String.Table.create () in
      let add_to_table error =
        let update = function
          | None -> [error]
          | Some errors -> error :: errors
        in
        let key = Analysis.AnalysisError.Instantiated.path error in
        let key_qualifier = Ast.SourcePath.qualifier_of_relative key in
        let open_document_keys = Hashtbl.keys open_documents in
        if List.mem ~equal:Ast.Reference.equal open_document_keys key_qualifier then
          Hashtbl.update table key ~f:update
      in
      let add_empty_error_array reference =
        let update = function
          | None -> []
          | Some errors -> errors
        in
        let open Analysis in
        reference
        |> Analysis.AstEnvironment.ReadOnly.get_real_path_relative
             (TypeEnvironment.ast_environment environment |> AstEnvironment.read_only)
             ~configuration:analysis_configuration
        >>| Hashtbl.update table ~f:update
        |> ignore
      in
      Ast.Reference.Table.iter_keys open_documents ~f:add_empty_error_array;
      List.iter error_list ~f:add_to_table;
      Hashtbl.to_alist table
    in
    let { Configuration.Analysis.local_root; _ } = analysis_configuration in
    let file_diagnostic_response (handle, errors) =
      let path =
        try
          Path.create_relative ~root:local_root ~relative:handle
          |> Path.real_path
          |> function
          | Path.Absolute path -> Some path
          | Path.Relative _ -> None
        with
        | Unix.Unix_error (name, kind, parameters) ->
            Log.log_unix_error (name, kind, parameters);
            None
      in
      path
      >>| (fun path -> LanguageServer.Protocol.PublishDiagnostics.of_errors path errors)
      >>| LanguageServer.Protocol.PublishDiagnostics.to_yojson
      >>| Yojson.Safe.to_string
      >>| fun serialized_diagnostic -> LanguageServerProtocolResponse serialized_diagnostic
    in
    build_file_to_error_map errors |> List.filter_map ~f:file_diagnostic_response
  in
  (* Decides what to broadcast to persistent clients after a request is processed. *)
  let broadcast_response state response =
    let responses =
      match response with
      | TypeCheckResponse errors -> errors_to_lsp_responses state errors
      | LanguageServerProtocolResponse _ -> [response]
      | ClientExitResponse Persistent -> [response]
      | ServerUuidResponse _ -> [response]
      | _ -> []
    in
    List.iter responses ~f:(fun response ->
        Connections.broadcast_response ~connections:state.connections ~response)
  in
  let rec loop state =
    let rec handle_request ?(retries = 2) state ~origin ~request =
      try
        let process_request ~state ~request =
          Log.log ~section:`Server "Processing request %a" Protocol.Request.pp request;
          Request.process ~state ~configuration ~request
        in
        match origin with
        | Protocol.Request.PersistentSocket socket ->
            let { Request.state; response } = process_request ~state ~request in
            (match response with
            | Some (LanguageServerProtocolResponse _)
            | Some (ServerUuidResponse _)
            | Some (ClientExitResponse Persistent) ->
                response
                >>| (fun response ->
                      Connections.write_to_persistent_client
                        ~connections:state.connections
                        ~socket
                        ~response)
                |> ignore
            | Some (TypeCheckResponse error_map) ->
                StatusUpdate.information ~message:"Done recheck." ~state;
                broadcast_response state (TypeCheckResponse error_map)
            | Some _ -> Log.error "Unexpected response for persistent client request"
            | None -> ());
            state
        | Protocol.Request.JSONSocket socket ->
            let write_to_json_socket response =
              try
                Connections.write_to_json_socket ~socket response;
                Connections.remove_json_socket ~connections:state.connections ~socket |> ignore
              with
              | Unix.Unix_error (name, kind, parameters) ->
                  Connections.remove_json_socket ~connections:state.connections ~socket |> ignore;
                  Log.log_unix_error (name, kind, parameters)
              | _ ->
                  Connections.remove_json_socket ~connections:state.connections ~socket |> ignore;
                  Log.error "Socket error"
            in
            (match request with
            | Server.Protocol.Request.StopRequest ->
                write_to_json_socket (Jsonrpc.Response.Stop.to_json ());
                Operations.stop ~reason:"explicit request" ~configuration ~scheduler
            | _ -> ());
            let { Request.state; response } = process_request ~state ~request in
            (match response with
            | Some (TypeCheckResponse errors) ->
                List.iter (errors_to_lsp_responses state errors) ~f:(fun response ->
                    Connections.broadcast_to_adapter_sockets
                      ~response
                      ~connections:state.connections);
                write_to_json_socket (Jsonrpc.Response.TypeErrors.to_json errors)
            | Some (TypeQueryResponse response) ->
                write_to_json_socket (TypeQuery.json_socket_response response)
            | Some (LanguageServerProtocolResponse response) ->
                Connections.write_lsp_response_to_json_socket ~socket response;
                Connections.remove_json_socket ~connections:state.connections ~socket |> ignore
            | _ -> ());
            state
        | Protocol.Request.FileNotifier ->
            let { Request.state; response } = process_request ~state ~request in
            (match response with
            | Some response -> broadcast_response state response
            | None -> ());
            state
        | Protocol.Request.NewConnectionSocket socket ->
            (* Stop requests are special - they require communicating back to the socket, but never
               return, so we need to respond to the request before processing it. *)
            (* TODO: Remove this special handling when we've switched over to json sockets
               completely. *)
            (match request with
            | Server.Protocol.Request.StopRequest -> Socket.write socket StopResponse
            | _ -> ());
            let { Request.state; response } = process_request ~state ~request in
            (match response with
            | Some response ->
                Socket.write_ignoring_epipe socket response;
                broadcast_response state response
            | None -> ());
            state
      with
      | uncaught_exception ->
          if retries > 0 then
            handle_request ~retries:(retries - 1) state ~origin ~request
          else
            raise uncaught_exception
    in
    let state =
      match Squeue.length request_queue with
      | 0 ->
          (* Stop if the server is idle. *)
          let current_time = Unix.time () in
          let stop_after_idle_for = 24.0 *. 60.0 *. 60.0 (* 1 day *) in
          if Float.(current_time -. state.last_request_time > stop_after_idle_for) then
            Error_checking_mutex.critical_section state.connections.lock ~f:(fun () ->
                Operations.stop ~reason:"idle" ~configuration ~scheduler);

          (* Stop if there's any inconsistencies in the .pyre directory. *)
          let last_integrity_check =
            let integrity_check_every = 60.0 (* 1 minute *) in
            if Float.(current_time -. state.last_integrity_check > integrity_check_every) then
              try
                let pid =
                  let pid_file = Path.absolute pid_path |> In_channel.create in
                  protect
                    ~f:(fun () -> In_channel.input_all pid_file)
                    ~finally:(fun () -> In_channel.close pid_file)
                in
                if not (String.equal (Pid.to_string (Unix.getpid ())) pid) then
                  raise (Failure "pid mismatch");
                current_time
              with
              | _ ->
                  Error_checking_mutex.critical_section state.connections.lock ~f:(fun () ->
                      Operations.stop ~reason:"failed integrity check" ~configuration ~scheduler)
            else
              state.last_integrity_check
          in
          (* This sleep is necessary because OCaml threads aren't pre-emptively scheduled. *)
          Unix.nanosleep 0.1 |> ignore;
          { state with last_integrity_check }
      | _ ->
          let state = { state with last_request_time = Unix.time () } in
          let origin, request = Squeue.pop request_queue in
          handle_request state ~origin ~request
    in
    loop state
  in
  loop state


let request_handler_thread
    ( ({ Configuration.Server.configuration = { expected_version; local_root; _ }; _ } as
      server_configuration),
      ({ Server.State.lock; connections = raw_connections } as connections),
      scheduler,
      request_queue )
  =
  let queue_request ~origin request =
    match request, origin with
    | Protocol.Request.StopRequest, Protocol.Request.NewConnectionSocket socket ->
        Socket.write socket StopResponse;
        Operations.stop ~reason:"explicit request" ~configuration:server_configuration ~scheduler
    | Protocol.Request.StopRequest, Protocol.Request.JSONSocket _ ->
        Squeue.push_or_drop request_queue (origin, request) |> ignore
    | Protocol.Request.StopRequest, _ ->
        Operations.stop ~reason:"explicit request" ~configuration:server_configuration ~scheduler
    | Protocol.Request.ClientConnectionRequest client, Protocol.Request.NewConnectionSocket socket
      ->
        Log.log ~section:`Server "Adding %s client" (show_client client);
        (match client with
        | Persistent -> Connections.add_persistent_client ~connections ~socket
        | _ -> ());
        Socket.write socket (ClientConnectionResponse client)
    | Protocol.Request.ClientConnectionRequest _, _ ->
        Log.error
          "Unexpected request origin %s for connection request"
          (Protocol.Request.origin_name origin)
    | _ -> Squeue.push_or_drop request_queue (origin, request) |> ignore
  in
  let handle_readable_persistent socket =
    let handle_disconnect () =
      Log.log ~section:`Server "Persistent client disconnected";
      Connections.remove_persistent_client ~connections ~socket
    in
    try
      Log.log ~section:`Server "A persistent client socket is readable.";
      let request = Socket.read socket in
      queue_request ~origin:(Protocol.Request.PersistentSocket socket) request
    with
    | End_of_file -> handle_disconnect ()
    | error when is_connection_reset_error error -> handle_disconnect ()
  in
  let handle_readable_json_request ~remove_socket socket =
    let handle_disconnect () =
      Log.log ~section:`Server "JSON socket notifier disconnected";
      remove_socket ~connections ~socket
    in
    try
      Log.log ~section:`Server "A file notifier is readable.";
      let request = socket |> Unix.in_channel_of_descr |> LanguageServer.Protocol.read_message in
      let origin = request >>| Jsonrpc.Request.origin ~socket |> Option.value ~default:None in
      request
      >>| Jsonrpc.Request.format_request
      |> function
      | request -> (
          match request, origin with
          | Some request, Some origin -> queue_request ~origin request
          | _, _ -> Log.log ~section:`Server "Failed to parse LSP message from JSON socket.")
    with
    | End_of_file
    | Yojson.Json_error _ ->
        handle_disconnect ()
    | error when is_connection_reset_error error -> handle_disconnect ()
  in
  let rec loop () =
    Connections.close_json_sockets ~connections;
    let {
      socket = server_socket;
      json_socket;
      adapter_socket;
      persistent_clients;
      json_sockets;
      adapter_sockets;
      _;
    }
      =
      Error_checking_mutex.critical_section lock ~f:(fun () -> !raw_connections)
    in
    if not (PyrePath.is_directory local_root) then (
      Log.error
        "Stopping server due to missing source root, %s is not a directory."
        (Path.show local_root);
      Operations.stop ~reason:"missing source root" ~configuration:server_configuration ~scheduler);
    let readable =
      Unix.select
        ~restart:true
        ~read:
          (server_socket :: json_socket :: adapter_socket :: Map.keys persistent_clients
          @ json_sockets
          @ adapter_sockets)
        ~write:[]
        ~except:[]
        ~timeout:(`After (Time.of_sec 5.0))
        ()
      |> fun { Unix.Select_fds.read; _ } -> read
    in
    let handle_socket socket =
      let add_json_socket ~add_socket ~socket =
        let new_socket, _ =
          Log.log ~section:`Server "New json client connection";
          Unix.accept socket
        in
        Jsonrpc.handshake_message (Option.value ~default:"-1" expected_version)
        |> LanguageServer.Types.HandshakeServer.to_yojson
        |> Connections.write_to_json_socket ~socket:new_socket;
        new_socket
        |> Unix.in_channel_of_descr
        |> LanguageServer.Protocol.read_message
        >>| LanguageServer.Types.HandshakeClient.of_yojson
        |> function
        | Some (Ok _) ->
            add_socket ~connections ~socket:new_socket;
            Jsonrpc.socket_added_message |> Connections.write_to_json_socket ~socket:new_socket
        | Some (Error error) -> Log.warning "Failed to parse handshake: %s" error
        | None -> Log.warning "Failed to parse handshake as LSP."
      in
      if Unix.File_descr.equal socket server_socket then
        let new_socket, _ =
          Log.log ~section:`Server "New client connection";
          Unix.accept server_socket
        in
        try
          Socket.write
            new_socket
            (Handshake.ServerConnected (Option.value ~default:"-1" expected_version));
          Socket.read new_socket
          |> fun Handshake.ClientConnected ->
          let request = Socket.read new_socket in
          queue_request ~origin:(Protocol.Request.NewConnectionSocket new_socket) request
        with
        | Unix.Unix_error (Unix.EPIPE, _, _) -> Log.warning "EPIPE while writing to socket."
        | error when is_connection_reset_error error ->
            Log.warning "ECONNRESET while reading from socket."
        | End_of_file -> Log.warning "New client socket unreadable"
      else if Unix.File_descr.equal socket json_socket then
        try add_json_socket ~add_socket:Connections.add_json_socket ~socket:json_socket with
        | End_of_file -> Log.warning "Got end of file while waiting for handshake."
        | Sys_error error
        | Yojson.Json_error error ->
            Log.warning "Failed to complete handshake: %s" error
      else if Unix.File_descr.equal socket adapter_socket then
        try add_json_socket ~add_socket:Connections.add_adapter_socket ~socket:adapter_socket with
        | End_of_file -> Log.warning "Got end of file while waiting for handshake."
        | Sys_error error
        | Yojson.Json_error error ->
            Log.warning "Failed to complete handshake: %s" error
      else if
        Error_checking_mutex.critical_section lock ~f:(fun () -> Map.mem persistent_clients socket)
      then
        handle_readable_persistent socket
      else if
        Error_checking_mutex.critical_section lock ~f:(fun () ->
            List.mem ~equal:Unix.File_descr.equal adapter_sockets socket)
      then
        handle_readable_json_request ~remove_socket:Connections.remove_adapter_socket socket
      else
        handle_readable_json_request ~remove_socket:Connections.remove_json_socket socket
    in
    List.iter ~f:handle_socket readable;

    (* We need to introduce this nanosleep to avoid burning CPU. *)
    if List.is_empty readable then
      Unix.nanosleep 0.1 |> ignore;
    loop ()
  in
  try loop () with
  | uncaught_exception ->
      Statistics.log_exception uncaught_exception ~fatal:true ~origin:"server";
      Operations.stop ~reason:"exception" ~configuration:server_configuration ~scheduler


(** Main server either as a daemon or in terminal *)
let serve ~socket ~json_socket ~adapter_socket ~server_configuration =
  Version.log_version_banner ();
  (fun () ->
    Log.log ~section:`Server "Starting daemon server loop...";
    Configuration.Server.set_global server_configuration;
    let request_queue = Squeue.create 25 in
    let connections =
      {
        lock = Error_checking_mutex.create ();
        connections =
          ref
            {
              socket;
              json_socket;
              adapter_socket;
              persistent_clients = Socket.Map.empty;
              json_sockets = [];
              sockets_to_close = [];
              adapter_sockets = [];
            };
      }
    in
    let state = Operations.start ~connections ~configuration:server_configuration () in
    (* Register signal handlers. *)
    Signal.Expert.handle Signal.int (fun _ ->
        Operations.stop
          ~reason:"interrupt"
          ~configuration:server_configuration
          ~scheduler:state.scheduler);
    Signal.Expert.handle Signal.pipe (fun _ -> ());
    Thread.create
      ~on_uncaught_exn:`Kill_whole_process
      request_handler_thread
      (server_configuration, connections, state.scheduler, request_queue)
    |> ignore;
    try computation_thread request_queue server_configuration state with
    | uncaught_exception ->
        Statistics.log_exception uncaught_exception ~fatal:true ~origin:"server";
        Operations.stop
          ~reason:"exception"
          ~configuration:server_configuration
          ~scheduler:state.scheduler)
  |> Scheduler.run_process


(* Create lock file and pid file. Used for both daemon mode and in-terminal *)
let acquire_lock ~server_configuration:{ Configuration.Server.lock_path; pid_path; _ } =
  let pid = Unix.getpid () |> Pid.to_int in
  if not (Lock.grab (Path.absolute lock_path)) then
    raise AlreadyRunning;
  Out_channel.with_file (Path.absolute pid_path) ~f:(fun out_channel ->
      Format.fprintf (Format.formatter_of_out_channel out_channel) "%d%!" pid)


type run_server_daemon_entry =
  ( Socket.t
    * Socket.t
    * Socket.t
    * Configuration.Server.t
    * Log.GlobalState.t
    * Profiling.GlobalState.t
    * Statistics.GlobalState.t,
    unit Daemon.in_channel,
    unit Daemon.out_channel )
  Daemon.entry
(** Daemon forking code *)

(** When spawned, a child is passed input/output channels that can communicate with the parent
    process. We don't currently use these, so close them after spawning. *)
let run_server_daemon_entry : run_server_daemon_entry =
  Daemon.register_entry_point
    "server_daemon"
    (fun
      ( socket,
        json_socket,
        adapter_socket,
        server_configuration,
        log_state,
        profiling_state,
        statistics_state )
      (parent_in_channel, parent_out_channel)
    ->
      Daemon.close_in parent_in_channel;
      Daemon.close_out parent_out_channel;

      (* Restore global states *)
      Log.GlobalState.restore log_state;
      Profiling.GlobalState.restore profiling_state;
      Statistics.GlobalState.restore statistics_state;

      (* Detach the from a controlling terminal *)
      Unix.Terminal_io.setsid () |> ignore;
      acquire_lock ~server_configuration;
      let _ =
        match Sys.getenv "PYRE_DISABLE_TELEMETRY" with
        | Some _ -> ()
        | None -> Telemetry.reset_budget ()
      in
      serve ~socket ~json_socket ~adapter_socket ~server_configuration)


let run
    ({
       Configuration.Server.lock_path;
       socket = { path = socket_path; _ };
       json_socket = { path = json_socket_path; _ };
       adapter_socket = { path = adapter_socket_path; _ };
       log_path;
       daemonize;
       configuration = { incremental_style; _ };
       _;
     } as server_configuration)
  =
  try
    (fun () ->
      try
        let () =
          match incremental_style with
          | Configuration.Analysis.FineGrained -> Log.info "Starting up server ..."
          | Configuration.Analysis.Shallow ->
              Log.warning
                "Starting server in legacy incremental mode. Incremental Pyre check will only get \
                 triggered on changed files but not on any of their dependencies."
        in
        if daemonize then
          Version.log_version_banner ();
        if not (Lock.check (Path.absolute lock_path)) then
          raise AlreadyRunning;
        Log.log ~section:`Server "Creating server socket at `%a`" Path.pp socket_path;
        let socket = Socket.initialize_unix_socket socket_path in
        let json_socket = Socket.initialize_unix_socket json_socket_path in
        let adapter_socket = Socket.initialize_unix_socket adapter_socket_path in
        if daemonize then (
          let stdin = Daemon.null_fd () in
          let log_path = Log.rotate (Path.absolute log_path) in
          let stdout = Daemon.fd_of_path log_path in
          Log.log ~section:`Server "Spawning the daemon now.";
          let ({ Daemon.pid; _ } as handle) =
            Daemon.spawn
              (stdin, stdout, stdout)
              run_server_daemon_entry
              ( socket,
                json_socket,
                adapter_socket,
                server_configuration,
                Log.GlobalState.get (),
                Profiling.GlobalState.get (),
                Statistics.GlobalState.get () )
          in
          Daemon.close handle;
          Log.log ~section:`Server "Forked off daemon with pid %d" pid;
          Log.info "Starting the server. Please wait...";
          pid)
        else (
          acquire_lock ~server_configuration;
          serve ~socket ~json_socket ~adapter_socket ~server_configuration)
      with
      | AlreadyRunning ->
          Log.info "Server is already running";
          0)
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


(** Default configuration when run from command line *)
let run_start_command
    log_path
    terminal
    load_state_from
    save_state_to
    changed_files_path
    saved_state_project
    saved_state_metadata
    configuration_file_hash
    store_type_check_resolution
    new_incremental_check
    perform_autocompletion
    features
    _verbose
    expected_version
    sections
    debug
    strict
    show_error_traces
    sequential
    filter_directories
    ignore_all_errors
    number_of_workers
    log_identifier
    logger
    profiling_output
    memory_profiling_output
    project_root
    source_path
    search_path
    taint_model_paths
    excludes
    extensions
    log_directory
    python_major_version
    python_minor_version
    python_micro_version
    shared_memory_heap_size
    shared_memory_dependency_table_power
    shared_memory_hash_table_power
    local_root
    ()
  =
  let source_path = Option.value source_path ~default:[local_root] in
  let local_root = SearchPath.create local_root |> SearchPath.get_root in
  Log.GlobalState.initialize ~debug ~sections;
  Statistics.GlobalState.initialize
    ~log_identifier
    ?logger
    ~project_name:(Path.last local_root)
    ~project_root
    ();
  Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
  let filter_directories =
    filter_directories
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)
  in
  let ignore_all_errors =
    ignore_all_errors
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)
  in
  let configuration =
    let incremental_style =
      if new_incremental_check then
        Configuration.Analysis.FineGrained
      else
        Shallow
    in
    Configuration.Analysis.create
      ?expected_version
      ~debug
      ?configuration_file_hash
      ~strict
      ~show_error_traces
      ~parallel:(not sequential)
      ?filter_directories
      ?ignore_all_errors
      ~number_of_workers
      ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
      ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
      ~taint_model_paths:
        (List.map taint_model_paths ~f:(Path.create_absolute ~follow_symbolic_links:true))
      ~excludes
      ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
      ~local_root
      ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
      ~store_type_check_resolution
      ~incremental_style
      ~perform_autocompletion
      ~features:(Configuration.Features.create features)
      ?log_directory
      ?python_major_version
      ?python_minor_version
      ?python_micro_version
      ?shared_memory_heap_size
      ?shared_memory_dependency_table_power
      ?shared_memory_hash_table_power
      ()
  in
  let log_path = log_path >>| Path.create_absolute ~follow_symbolic_links:true in
  let saved_state_action =
    match save_state_to, saved_state_project with
    | Some path, _ -> Some (Configuration.Server.Save path)
    | None, Some project_name ->
        Some
          (Configuration.Server.Load
             (Configuration.Server.LoadFromProject { project_name; metadata = saved_state_metadata }))
    | None, None -> (
        match load_state_from, changed_files_path with
        | Some shared_memory_path, _ ->
            Some
              (Load
                 (Configuration.Server.LoadFromFiles
                    {
                      Configuration.Server.shared_memory_path =
                        Path.create_absolute ~follow_symbolic_links:true shared_memory_path;
                      changed_files_path =
                        changed_files_path >>| Path.create_absolute ~follow_symbolic_links:true;
                    }))
        | None, Some _ ->
            Log.error "-load-state-from must be set when -changed-files-path is passed in.";
            exit 1
        | _ -> None)
  in
  run
    (Operations.create_configuration
       ~daemonize:(not terminal)
       ?log_path
       ?saved_state_action
       configuration)
  |> ignore


let command =
  Command.basic_spec
    ~summary:"Starts a server in the foreground by default. See help for daemon options."
    Command.Spec.(
      empty
      +> flag
           "-log-file"
           (optional string)
           ~doc:(Format.sprintf "filename Log file (Default is ./pyre/server/server.stdout)")
      +> flag
           "-terminal"
           no_arg
           ~doc:"Run the server from the terminal instead of running as a daemon."
      +> flag
           "-load-state-from"
           (optional string)
           ~doc:"The Pyre server will start from the specified path if one is passed in."
      +> flag
           "-save-initial-state-to"
           (optional string)
           ~doc:
             "The Pyre server will save its initial state to the path passed in by this argument."
      +> flag
           "-changed-files-path"
           (optional string)
           ~doc:"Pyre will reanalyze the paths listed in path if started from a saved state."
      +> flag
           "-saved-state-project"
           (optional string)
           ~doc:"Pyre will attempt to fetch the project's saved state from the project name."
      +> flag
           "-saved-state-metadata"
           (optional string)
           ~doc:"The metadata to search for the project with, if any."
      +> flag
           "-configuration-file-hash"
           (optional string)
           ~doc:"SHA1 of the .pyre_configuration used to initialize this server."
      +> flag
           "-store-type-check-resolution"
           no_arg
           ~doc:"Store extra information, needed for `types_at_position` and `types` queries."
      +> flag "-new-incremental-check" no_arg ~doc:"Use the new fine grain dependency incremental"
      +> flag "-autocomplete" no_arg ~doc:"Process autocomplete requests."
      +> flag
           "-features"
           (optional string)
           ~doc:"Features gated by permissions sent from the client."
      ++ Specification.base_command_line_arguments)
    run_start_command
